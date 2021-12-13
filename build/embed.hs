#!/usr/bin/env runhaskell
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Main where

import Text.Pandoc (def, nullMeta, pandocExtensions, queryWith, readerExtensions, readHtml, readMarkdown, writeHtml5String, Block(BulletList, Para), Inline(Link, Str), runPure, Pandoc(..))
import Text.Pandoc.Walk (walk)
import qualified Data.Text as T  (append, drop, head, init, intercalate, last, length, pack, replace, strip, take, unlines, unpack, Text)
import Data.List ((\\), intercalate, sort, nub)
import Data.List.Utils (replace)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M (filter, keys, lookup)
import System.Directory (doesFileExist, renameFile)
import Data.Containers.ListUtils (nubOrd)
import System.IO.Temp (emptySystemTempFile)
import Text.Read (readMaybe)
import qualified Control.Monad.Parallel as Par (mapM, mapM_)
import Control.Parallel.Strategies (parMap, rseq)
import Text.Show.Pretty (ppShow)
import Data.FileStore.Utils (runShellCommand)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import System.Exit (ExitCode(ExitFailure))
import Data.Binary (decodeFile, encodeFile)
import qualified Data.Text.IO as TIO (writeFile)
import Network.HTTP (urlEncode)
import System.IO (stderr, hPutStrLn)

import LinkMetadata (readLinkMetadata, authorsTruncate, Metadata, MetadataItem, safeHtmlWriterOptions)

import Columns (simplifiedDoc)

import Interwiki (convertInterwikiLinks, inlinesToString)

import qualified Data.Vector as V (toList, Vector)
import Control.Monad.Identity (runIdentity, Identity)
import Data.RPTree (knn, forest, metricL2, rpTreeCfg, fpMaxTreeDepth, fpDataChunkSize, fpProjNzDensity, fromListDv, DVector, Embed(..), RPForest, recallWith)
import Data.Conduit (ConduitT)
import Data.Conduit.List (sourceList)

main :: IO ()
main = do md  <- readLinkMetadata
          edb <- readEmbeddings

          -- update for any missing embeddings, and return updated DB for computing distances & writing out fragments:
          let todo = take 200 $ sort $ missingEmbeddings md edb
          edb'' <- do if (length todo) == 0 then hPutStrLn stderr "(Read databases; all updated.)" >> return edb else do
                       newEmbeddings <- Par.mapM embed todo
                       let edb' = nubOrd (edb ++ newEmbeddings)
                       writeEmbeddings edb'
                       hPutStrLn stderr "(Wrote embeddings.)"
                       return edb'

          let hyperparams = hyperparameterSweep edb''
          print $ take 20 hyperparams
          print hyperparams

          -- rp-tree supports serializing the tree to disk, but unclear how to update it, and it's fast enough to construct that it's not a bottleneck, so we recompute it from the embeddings every time.
          let ddb  = embeddings2Forest edb''
          Par.mapM_ (writeOutMatch md . findN ddb bestNEmbeddings) edb''

-- how many results do we want?
bestNEmbeddings :: Int
bestNEmbeddings = 10

type Embedding  = (String, String, [Double]) -- NOTE: 'Float' in Haskell is 32-bit single-precision float (FP32); OA API apparently returns 64-bit double-precision (FP64), so we use 'Double' instead. (Given the very small magnitude of the Doubles, it is probably a bad idea to try to save space/compute by casting to Float.)
type Embeddings = [Embedding]

embeddingsPath :: String
embeddingsPath = "metadata/embeddings.bin"

readEmbeddings :: IO Embeddings
readEmbeddings = do exists <- doesFileExist embeddingsPath
                    if exists then decodeFile embeddingsPath else return []

writeEmbeddings :: Embeddings -> IO ()
writeEmbeddings es = do tempf <- emptySystemTempFile "hakyll-embeddings"
                        encodeFile tempf es
                        renameFile tempf embeddingsPath

missingEmbeddings :: Metadata -> Embeddings -> [(String, MetadataItem)]
missingEmbeddings md edb = let urlsToCheck = M.keys $ M.filter (\(t, aut, _, _, tags, abst) -> length (t++aut++show tags++abst) > minimumLength) md
                               urlsEmbedded = map (\(u,_,_) -> u) edb :: [String]
                               missing      = urlsToCheck \\ urlsEmbedded
                               in map (\u -> (u, fromJust $ M.lookup u md)) missing

  where minimumLength :: Int
        minimumLength = 400 -- how many characters long should metadata be before it is worth trying to embed?

-- convert an annotated item into a single text string: concatenate the useful metadata
formatDoc :: (String,MetadataItem) -> T.Text
formatDoc (path,mi@(t,aut,dt,_,tags,abst)) =
    let document = T.pack $ replace "\n" "\n\n" $ unlines ["'"++t++"' " ++ "("++path++")" ++ ", by "++authorsTruncate aut++(if dt==""then"."else" ("++dt++")."), "Subject: "++(intercalate ", " tags) ++ ".", replace "<hr />" "" abst]
        parsedEither = let parsed = runPure $ readHtml def{readerExtensions = pandocExtensions } document
                       in case parsed of
                          Left e -> error $ "Failed to parse HTML document into Pandoc AST: error: " ++ show e ++ " : " ++ show mi ++ " : " ++ T.unpack document
                          Right p -> p
        -- create a numbered list of URL references inside each document to expose it to the embedding model, as 'simplifiedDoc' necessarily strips URLs:
        documentURLs = extractURLs parsedEither
        documentURLsText = if null documentURLs then "" else "References:\n" `T.append` T.unlines (map (\(n, (url,titles)) -> T.pack n `T.append` ". " `T.append` url `T.append` " " `T.append` (T.intercalate ", " $ tail titles)) $ zip (map show [(1::Int)..]) documentURLs)
        -- simple plaintext ASCII-ish version, which hopefully is more comprehensible to NN models than full-blown HTML
        plainText = simplifiedDoc parsedEither `T.append` documentURLsText
        -- post-processing: 'We suggest replacing newlines (\n) in your input with a single space, as we have observed inferior results when newlines are present.' https://beta.openai.com/docs/api-reference/embeddings/create
        -- GPT-3 apparently doesn't do well with Unicode punctuation either (they get a bad BPE expansion factor too), so smart quotes are right out.
        gptPlainText = T.take maxLength $ T.strip $ T.replace "\n" " " $ T.replace "  " " " $ T.replace "  " " " $ T.replace "…" "..." $ T.replace "“" "'" $ T.replace "”" "'" $ T.replace "‘" "'" $ T.replace "’" "'" $ T.replace "\\" "" $ T.replace "\"" "'" $ T.replace "\"" "'" plainText
    in
      gptPlainText
  where
    maxLength :: Int
    maxLength = 8100 -- how long is too long? OA guesstimates 1 BPE = 4 characters on average (https://beta.openai.com/tokenizer), so 2047 BPEs ~ 8192 characters. If a call fails, the shell script will truncate the input and retry until it works so we don't need to set the upper limit too low.

    -- | Read 1 Pandoc AST and return its URLs/anchor-text pairs;
    -- if a URL has both a title and an anchor text, we return 2 pairs because both might be valid (eg '[GPT-3](https://arxiv.org/foo "Language Models are Few-Shot Learners")' - we would like to do similar-links on both the short noun 'GPT-3' and the paper title, but we can't if we arbitrarily return one but not the other).
    extractURLs :: Pandoc -> [(T.Text,[T.Text])]
    extractURLs = queryWith extractURL . walk convertInterwikiLinks
     where
       extractURL :: Inline -> [(T.Text,[T.Text])]
       extractURL (Link _ il (u,""))     = [(u, [cleanURL $ inlinesToString il])]
       extractURL (Link _ il (u,target)) = [(u, [cleanURL $ inlinesToString il]), (u, [target])]
       extractURL _ = []

       -- NOTE: apparently due to nested Spans (from the smallcaps) and the RawInline issue (yet again), some link suggestions look like ">ADHD<". Very undesirable replacement targets. So we special-case clean those:
       cleanURL :: T.Text -> T.Text
       cleanURL "" = ""
       cleanURL u = if T.head u == '>' && T.last u == '<' then T.init $ T.drop 1 u else u

embed :: (String,MetadataItem) -> IO Embedding
embed i@(p,_) = do
  let doc = formatDoc i
  (modelType,embedding) <- oaAPIEmbed doc
  return (p,modelType,embedding)

-- we shell out to a Bash script `similar.sh` to do the actual curl + JSON processing; see it for details.
oaAPIEmbed :: T.Text -> IO (String,[Double])
oaAPIEmbed doc = do (status,_,mb) <- runShellCommand "./" Nothing "bash" ["static/build/embed.sh", replace "\n" "\\n" $ -- JSON escaping of newlines
                                                                                                   T.unpack doc]
                    case status of
                      ExitFailure err -> error $ "Exit Failure: " ++ (intercalate " : " [show (T.length doc), T.unpack doc, ppShow status, ppShow err, ppShow mb])
                      _ -> do let results = lines $ U.toString mb
                              case results of
                                [] -> error $ "Failed to read embed.sh output? " ++ "\n" ++ show (T.length doc) ++ "\n" ++ T.unpack doc ++ "\n" ++ U.toString mb
                                (modelType:latents) -> let embeddingM = readMaybe (unlines latents) :: Maybe [Double] in
                                                         case embeddingM of
                                                           Nothing -> error $ "Failed to read embed.sh output? " ++ "\n" ++ show (T.length doc) ++ "\n" ++ T.unpack doc ++ "\n" ++ U.toString mb
                                                           Just embedding -> return (modelType, embedding)

type Distances = [(String, [String])]

type Forest = RPForest Double (V.Vector (Embed DVector Double String))

-- magic hyperparameters must be chosen based on embedding type. For 'ada-similarity', '21 5 12' works reasonably well.
embeddings2Forest :: Embeddings -> Forest
embeddings2Forest = embeddings2ForestConfigurable 21 5 12

embeddings2ForestConfigurable :: Int -> Int -> Int -> Embeddings -> Forest
embeddings2ForestConfigurable ls nt pvd es =
  let minLeafSize = ls -- ???
      cfg = rpTreeCfg minLeafSize
              (length es) -- data N
              (length $ (\(_,_,embedding) -> embedding) $ head es) -- dimension of each datapoint (eg 1024 for ada-similarity embeddings, 12288 for davinci)
      nTrees = nt -- ???
      projectionVectorDimension = pvd -- ???
  in
    runIdentity $
    forest 4 (fpMaxTreeDepth cfg) minLeafSize nTrees (fpDataChunkSize cfg) (fpProjNzDensity cfg) projectionVectorDimension $
    embeddings2Conduit es
 where
   embeddings2Conduit :: Embeddings -> ConduitT () (Embed DVector Double String) Identity ()
   embeddings2Conduit = sourceList . map (\(p,_,embedding) -> Embed (fromListDv embedding) p)


knnEmbedding :: Forest -> Int -> Embedding -> [(Double, Embed DVector Double String)]
knnEmbedding f k (_,_,embd) = V.toList $
                               -- NOTE: 'metricL2' *seems* to be the L2-normalized Euclidean distance? which is *proportional* to cosine similarity/distance: not identical, but produces the same ranking, and so just as good for my purpose here? or so claims https://stats.stackexchange.com/a/146279/16897
                               -- 'inner' also works, type-wise, but produces terrible results on the GPT-3-ada embeddings; this is apparently due to the extremely similar magnitude of the embeddings, and dot-product not working as well as cosine similarity on language model embeddings is apparently common and expected.
                              -- 'knn', 'knnH', 'knnPQ': knnH/knnPQ always perform way worse for me.
                               knn (metricL2) k f ((fromListDv embd)::DVector Double)

findNearest :: Forest -> Int -> Embedding -> [(String,Double)]
findNearest f k e = map (\(dist,Embed _ p) -> (p,dist)) $ knnEmbedding f k e

-- we'll keep the distance to insert into the metadata for debugging purposes.
findN :: Forest -> Int -> Embedding -> (String,[(String,Double)])
findN f k e@(p1,_,_) = let results = take bestNEmbeddings $ nub $ filter (\(p2,_) -> p1 /= p2) $ findNearest f k e in
                 -- NOTE: 'knn' is the fastest (and most accurate?), but seems to return duplicate results, so requesting 10 doesn't return 10 unique hits.
                 -- (I'm not sure why, the rp-tree docs don't mention or warn about this that I noticed...)
                 -- If that happens, back off and request more k up to a max of 150.
                 if k>150 then (p1, [])
                 else if length results < bestNEmbeddings then findN f (k*2) e else (p1,results)

hyperparameterSweep :: Embeddings -> [(Double, (Int,Int,Int))]
hyperparameterSweep edb =
  -- sweep over `R> expand.grid(seq(30,100,by=10), seq(1,10,by=2), seq(2,200,by=10))` (n=800), ranges chosen by trying some hand optimization below
  let gridsearchParameters = [(30,1,2), (40,1,2), (50,1,2), (60,1,2), (70,1,2), (80,1,2), (90,1,2), (100,1,2), (30,3,2), (40,3,2), (50,3,2), (60,3,2), (70,3,2), (80,3,2), (90,3,2), (100,3,2), (30,5,2), (40,5,2), (50,5,2), (60,5,2), (70,5,2), (80,5,2), (90,5,2), (100,5,2), (30,7,2), (40,7,2), (50,7,2), (60,7,2), (70,7,2), (80,7,2), (90,7,2), (100,7,2), (30,9,2), (40,9,2), (50,9,2), (60,9,2), (70,9,2), (80,9,2), (90,9,2), (100,9,2), (30,1,12), (40,1,12), (50,1,12), (60,1,12), (70,1,12), (80,1,12), (90,1,12), (100,1,12), (30,3,12), (40,3,12), (50,3,12), (60,3,12), (70,3,12), (80,3,12), (90,3,12), (100,3,12), (30,5,12), (40,5,12), (50,5,12), (60,5,12), (70,5,12), (80,5,12), (90,5,12), (100,5,12), (30,7,12), (40,7,12), (50,7,12), (60,7,12), (70,7,12), (80,7,12), (90,7,12), (100,7,12), (30,9,12), (40,9,12), (50,9,12), (60,9,12), (70,9,12), (80,9,12), (90,9,12), (100,9,12), (30,1,22), (40,1,22), (50,1,22), (60,1,22), (70,1,22), (80,1,22), (90,1,22), (100,1,22), (30,3,22), (40,3,22), (50,3,22), (60,3,22), (70,3,22), (80,3,22), (90,3,22), (100,3,22), (30,5,22), (40,5,22), (50,5,22), (60,5,22), (70,5,22), (80,5,22), (90,5,22), (100,5,22), (30,7,22), (40,7,22), (50,7,22), (60,7,22), (70,7,22), (80,7,22), (90,7,22), (100,7,22), (30,9,22), (40,9,22), (50,9,22), (60,9,22), (70,9,22), (80,9,22), (90,9,22), (100,9,22), (30,1,32), (40,1,32), (50,1,32), (60,1,32), (70,1,32), (80,1,32), (90,1,32), (100,1,32), (30,3,32), (40,3,32), (50,3,32), (60,3,32), (70,3,32), (80,3,32), (90,3,32), (100,3,32), (30,5,32), (40,5,32), (50,5,32), (60,5,32), (70,5,32), (80,5,32), (90,5,32), (100,5,32), (30,7,32), (40,7,32), (50,7,32), (60,7,32), (70,7,32), (80,7,32), (90,7,32), (100,7,32), (30,9,32), (40,9,32), (50,9,32), (60,9,32), (70,9,32), (80,9,32), (90,9,32), (100,9,32), (30,1,42), (40,1,42), (50,1,42), (60,1,42), (70,1,42), (80,1,42), (90,1,42), (100,1,42), (30,3,42), (40,3,42), (50,3,42), (60,3,42), (70,3,42), (80,3,42), (90,3,42), (100,3,42), (30,5,42), (40,5,42), (50,5,42), (60,5,42), (70,5,42), (80,5,42), (90,5,42), (100,5,42), (30,7,42), (40,7,42), (50,7,42), (60,7,42), (70,7,42), (80,7,42), (90,7,42), (100,7,42), (30,9,42), (40,9,42), (50,9,42), (60,9,42), (70,9,42), (80,9,42), (90,9,42), (100,9,42), (30,1,52), (40,1,52), (50,1,52), (60,1,52), (70,1,52), (80,1,52), (90,1,52), (100,1,52), (30,3,52), (40,3,52), (50,3,52), (60,3,52), (70,3,52), (80,3,52), (90,3,52), (100,3,52), (30,5,52), (40,5,52), (50,5,52), (60,5,52), (70,5,52), (80,5,52), (90,5,52), (100,5,52), (30,7,52), (40,7,52), (50,7,52), (60,7,52), (70,7,52), (80,7,52), (90,7,52), (100,7,52), (30,9,52), (40,9,52), (50,9,52), (60,9,52), (70,9,52), (80,9,52), (90,9,52), (100,9,52), (30,1,62), (40,1,62), (50,1,62), (60,1,62), (70,1,62), (80,1,62), (90,1,62), (100,1,62), (30,3,62), (40,3,62), (50,3,62), (60,3,62), (70,3,62), (80,3,62), (90,3,62), (100,3,62), (30,5,62), (40,5,62), (50,5,62), (60,5,62), (70,5,62), (80,5,62), (90,5,62), (100,5,62), (30,7,62), (40,7,62), (50,7,62), (60,7,62), (70,7,62), (80,7,62), (90,7,62), (100,7,62), (30,9,62), (40,9,62), (50,9,62), (60,9,62), (70,9,62), (80,9,62), (90,9,62), (100,9,62), (30,1,72), (40,1,72), (50,1,72), (60,1,72), (70,1,72), (80,1,72), (90,1,72), (100,1,72), (30,3,72), (40,3,72), (50,3,72), (60,3,72), (70,3,72), (80,3,72), (90,3,72), (100,3,72), (30,5,72), (40,5,72), (50,5,72), (60,5,72), (70,5,72), (80,5,72), (90,5,72), (100,5,72), (30,7,72), (40,7,72), (50,7,72), (60,7,72), (70,7,72), (80,7,72), (90,7,72), (100,7,72), (30,9,72), (40,9,72), (50,9,72), (60,9,72), (70,9,72), (80,9,72), (90,9,72), (100,9,72), (30,1,82), (40,1,82), (50,1,82), (60,1,82), (70,1,82), (80,1,82), (90,1,82), (100,1,82), (30,3,82), (40,3,82), (50,3,82), (60,3,82), (70,3,82), (80,3,82), (90,3,82), (100,3,82), (30,5,82), (40,5,82), (50,5,82), (60,5,82), (70,5,82), (80,5,82), (90,5,82), (100,5,82), (30,7,82), (40,7,82), (50,7,82), (60,7,82), (70,7,82), (80,7,82), (90,7,82), (100,7,82), (30,9,82), (40,9,82), (50,9,82), (60,9,82), (70,9,82), (80,9,82), (90,9,82), (100,9,82), (30,1,92), (40,1,92), (50,1,92), (60,1,92), (70,1,92), (80,1,92), (90,1,92), (100,1,92), (30,3,92), (40,3,92), (50,3,92), (60,3,92), (70,3,92), (80,3,92), (90,3,92), (100,3,92), (30,5,92), (40,5,92), (50,5,92), (60,5,92), (70,5,92), (80,5,92), (90,5,92), (100,5,92), (30,7,92), (40,7,92), (50,7,92), (60,7,92), (70,7,92), (80,7,92), (90,7,92), (100,7,92), (30,9,92), (40,9,92), (50,9,92), (60,9,92), (70,9,92), (80,9,92), (90,9,92), (100,9,92), (30,1,102), (40,1,102), (50,1,102), (60,1,102), (70,1,102), (80,1,102), (90,1,102), (100,1,102), (30,3,102), (40,3,102), (50,3,102), (60,3,102), (70,3,102), (80,3,102), (90,3,102), (100,3,102), (30,5,102), (40,5,102), (50,5,102), (60,5,102), (70,5,102), (80,5,102), (90,5,102), (100,5,102), (30,7,102), (40,7,102), (50,7,102), (60,7,102), (70,7,102), (80,7,102), (90,7,102), (100,7,102), (30,9,102), (40,9,102), (50,9,102), (60,9,102), (70,9,102), (80,9,102), (90,9,102), (100,9,102), (30,1,112), (40,1,112), (50,1,112), (60,1,112), (70,1,112), (80,1,112), (90,1,112), (100,1,112), (30,3,112), (40,3,112), (50,3,112), (60,3,112), (70,3,112), (80,3,112), (90,3,112), (100,3,112), (30,5,112), (40,5,112), (50,5,112), (60,5,112), (70,5,112), (80,5,112), (90,5,112), (100,5,112), (30,7,112), (40,7,112), (50,7,112), (60,7,112), (70,7,112), (80,7,112), (90,7,112), (100,7,112), (30,9,112), (40,9,112), (50,9,112), (60,9,112), (70,9,112), (80,9,112), (90,9,112), (100,9,112), (30,1,122), (40,1,122), (50,1,122), (60,1,122), (70,1,122), (80,1,122), (90,1,122), (100,1,122), (30,3,122), (40,3,122), (50,3,122), (60,3,122), (70,3,122), (80,3,122), (90,3,122), (100,3,122), (30,5,122), (40,5,122), (50,5,122), (60,5,122), (70,5,122), (80,5,122), (90,5,122), (100,5,122), (30,7,122), (40,7,122), (50,7,122), (60,7,122), (70,7,122), (80,7,122), (90,7,122), (100,7,122), (30,9,122), (40,9,122), (50,9,122), (60,9,122), (70,9,122), (80,9,122), (90,9,122), (100,9,122), (30,1,132), (40,1,132), (50,1,132), (60,1,132), (70,1,132), (80,1,132), (90,1,132), (100,1,132), (30,3,132), (40,3,132), (50,3,132), (60,3,132), (70,3,132), (80,3,132), (90,3,132), (100,3,132), (30,5,132), (40,5,132), (50,5,132), (60,5,132), (70,5,132), (80,5,132), (90,5,132), (100,5,132), (30,7,132), (40,7,132), (50,7,132), (60,7,132), (70,7,132), (80,7,132), (90,7,132), (100,7,132), (30,9,132), (40,9,132), (50,9,132), (60,9,132), (70,9,132), (80,9,132), (90,9,132), (100,9,132), (30,1,142), (40,1,142), (50,1,142), (60,1,142), (70,1,142), (80,1,142), (90,1,142), (100,1,142), (30,3,142), (40,3,142), (50,3,142), (60,3,142), (70,3,142), (80,3,142), (90,3,142), (100,3,142), (30,5,142), (40,5,142), (50,5,142), (60,5,142), (70,5,142), (80,5,142), (90,5,142), (100,5,142), (30,7,142), (40,7,142), (50,7,142), (60,7,142), (70,7,142), (80,7,142), (90,7,142), (100,7,142), (30,9,142), (40,9,142), (50,9,142), (60,9,142), (70,9,142), (80,9,142), (90,9,142), (100,9,142), (30,1,152), (40,1,152), (50,1,152), (60,1,152), (70,1,152), (80,1,152), (90,1,152), (100,1,152), (30,3,152), (40,3,152), (50,3,152), (60,3,152), (70,3,152), (80,3,152), (90,3,152), (100,3,152), (30,5,152), (40,5,152), (50,5,152), (60,5,152), (70,5,152), (80,5,152), (90,5,152), (100,5,152), (30,7,152), (40,7,152), (50,7,152), (60,7,152), (70,7,152), (80,7,152), (90,7,152), (100,7,152), (30,9,152), (40,9,152), (50,9,152), (60,9,152), (70,9,152), (80,9,152), (90,9,152), (100,9,152), (30,1,162), (40,1,162), (50,1,162), (60,1,162), (70,1,162), (80,1,162), (90,1,162), (100,1,162), (30,3,162), (40,3,162), (50,3,162), (60,3,162), (70,3,162), (80,3,162), (90,3,162), (100,3,162), (30,5,162), (40,5,162), (50,5,162), (60,5,162), (70,5,162), (80,5,162), (90,5,162), (100,5,162), (30,7,162), (40,7,162), (50,7,162), (60,7,162), (70,7,162), (80,7,162), (90,7,162), (100,7,162), (30,9,162), (40,9,162), (50,9,162), (60,9,162), (70,9,162), (80,9,162), (90,9,162), (100,9,162), (30,1,172), (40,1,172), (50,1,172), (60,1,172), (70,1,172), (80,1,172), (90,1,172), (100,1,172), (30,3,172), (40,3,172), (50,3,172), (60,3,172), (70,3,172), (80,3,172), (90,3,172), (100,3,172), (30,5,172), (40,5,172), (50,5,172), (60,5,172), (70,5,172), (80,5,172), (90,5,172), (100,5,172), (30,7,172), (40,7,172), (50,7,172), (60,7,172), (70,7,172), (80,7,172), (90,7,172), (100,7,172), (30,9,172), (40,9,172), (50,9,172), (60,9,172), (70,9,172), (80,9,172), (90,9,172), (100,9,172), (30,1,182), (40,1,182), (50,1,182), (60,1,182), (70,1,182), (80,1,182), (90,1,182), (100,1,182), (30,3,182), (40,3,182), (50,3,182), (60,3,182), (70,3,182), (80,3,182), (90,3,182), (100,3,182), (30,5,182), (40,5,182), (50,5,182), (60,5,182), (70,5,182), (80,5,182), (90,5,182), (100,5,182), (30,7,182), (40,7,182), (50,7,182), (60,7,182), (70,7,182), (80,7,182), (90,7,182), (100,7,182), (30,9,182), (40,9,182), (50,9,182), (60,9,182), (70,9,182), (80,9,182), (90,9,182), (100,9,182), (30,1,192), (40,1,192), (50,1,192), (60,1,192), (70,1,192), (80,1,192), (90,1,192), (100,1,192), (30,3,192), (40,3,192), (50,3,192), (60,3,192), (70,3,192), (80,3,192), (90,3,192), (100,3,192), (30,5,192), (40,5,192), (50,5,192), (60,5,192), (70,5,192), (80,5,192), (90,5,192), (100,5,192), (30,7,192), (40,7,192), (50,7,192), (60,7,192), (70,7,192), (80,7,192), (90,7,192), (100,7,192), (30,9,192), (40,9,192), (50,9,192), (60,9,192), (70,9,192), (80,9,192), (90,9,192), (100,9,192)] :: [(Int,Int,Int)]
      evalOne f'' i = recallWith metricL2 f'' 10 $ (\(_,_,embd) -> ((fromListDv embd)::DVector Double)) $ edb!!i
      eval f' = sum (parMap rseq (evalOne f') [4585, 5768, 421, 6103, 1000, 2000, 4000, 5000, 6000]) / 9 -- spot check a few picked at random
  in
   reverse $ sort $
     parMap rseq (\z@(a,b,c) -> let f = embeddings2ForestConfigurable a b c edb in (eval f, z) ) gridsearchParameters

{-
davinci:
20  5 50: 0.39
30 10 55: 0.39
40  5 55: 0.42
21  5 20: 0.46
21  5 12: 0.47
21 10 20: 0.48
21  1 20: 0.48
30 10 60: 0.49
30  1 70: 0.50
80 10 20: 0.54
30  5 80: 0.54
40 10 20: 0.55
40  1 55: 0.55
30 10 40: 0.58
30  5 50: 0.58
70 5 200: 0.59
40 10 40: 0.60
40 5 100: 0.60
30 10 70: 0.62
30 10 80: 0.63
60 5 200: 0.64
60 20 100: 0.66
30 10 50: 0.67
60 10 150: 0.73

edb <- readEmbeddings
let f = embeddings2Forest edb -- embeddings2ForestConfigurable 21 5 12 edb -- embeddings2Forest edb -- embeddings2ForestConfigurable 60 10 150 edb

let f = embeddings2ForestConfigurable 45 4 50 edb
Data.RPTree.recallWith metricL2 f 20 $ (\(_,_,embd) -> ((fromListDv embd)::DVector Double)) $ head edb

findNearest f 20 $ head edb
findN f 20 $ head edb
-}

writeOutMatch :: Metadata -> (String, [(String,Double)]) -> IO ()
writeOutMatch md (p,matches) =
  do case M.lookup p md of
       Nothing -> return ()
       Just (_,_,_,_,_,abst) -> do
             -- we don't want to provide as a 'see also' a link already in the annotation, of course, so we need to pull them out & filter by:
             let alreadyLinked = extractLinks False $ T.pack abst
             let matchesPruned = filter (\(p2,_) -> not ((T.pack p2) `elem` alreadyLinked)) matches

             let similar = BulletList $ map (generateItem md) matchesPruned

             let pandoc = Pandoc nullMeta [similar]
             let html = let htmlEither = runPure $ writeHtml5String safeHtmlWriterOptions pandoc
                        in case htmlEither of
                                    Left e -> error $ show e ++ ":" ++ show p ++ ":" ++ show matches ++ ":" ++ show similar
                                    Right output -> output
             let similarLinksHtmlFragment = "<div class=\"columns\">\n" `T.append` html `T.append` "\n</div>"

             let f = take 274 $ "metadata/annotations/similar/" ++ urlEncode p ++ ".html"
             TIO.writeFile f similarLinksHtmlFragment
             -- HACK: write out a duplicate 'metadata/annotations/similar/foo.html.html' file to provide a 'syntax-highlighted' version that the popups fallback will render as proper HTML
             -- We overload the syntax-highlighting feature to make similar-links popup *partially* work (doesn't enable full suite of features like recursive popups); right now, when popups.js tries to load the similar-links `$PAGE.html`, it treats it as a raw source code file, and tries to fetch the *syntax-highlighted* version, `$PAGE.html.html` (which doesn't exist & thus errors out). But what if... we claimed the original HTML *was* the 'syntax-highlighted (HTML) version'? Then wouldn't popups.js then render it as HTML, and accidentally Just Work?
             TIO.writeFile (f++".html") similarLinksHtmlFragment

generateItem :: Metadata -> (String,Double) -> [Block]
generateItem md (p2,distance) = case M.lookup p2 md of
                                  Nothing -> []
                                  Just (t,_,_,_,_,_) ->
                                      [Para
                                        [Link ("", ["docMetadata"], [("embeddingDistance", T.pack $ show distance) ]) [Str $ T.pack $ "“"++t++"”"] (T.pack p2,"")]
                                        ]

-- | Read one Text string and return its URLs (as Strings)
extractLinks :: Bool -> T.Text -> [T.Text]
extractLinks md txt = let parsedEither = if md then runPure $ readMarkdown def{readerExtensions = pandocExtensions } txt
                                         else runPure $ readHtml def{readerExtensions = pandocExtensions } txt
                   in case parsedEither of
                              Left _ -> []
                              Right links -> extractURLs links
    where -- | Read 1 Pandoc AST and return its URLs as Strings
        extractURLs :: Pandoc -> [T.Text]
        extractURLs = queryWith extractURL
         where
           extractURL :: Inline -> [T.Text]
           extractURL (Link _ _ (u,_)) = [u]
           extractURL _ = []
