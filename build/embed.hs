#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Pandoc (def, pandocExtensions, queryWith, readerExtensions, readHtml, Inline(Link), runPure, Pandoc)
import Text.Pandoc.Walk (walk)
import qualified Data.Text as T  (append, drop, head, init, intercalate, last, length, pack, replace, take, unlines, unpack, Text)
import Data.List ((\\), intercalate, sort, nub)
import Data.List.Utils (replace)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M (filter, keys, lookup)
import System.Directory (doesFileExist, renameFile)
import Data.Containers.ListUtils (nubOrd)
import System.IO.Temp (emptySystemTempFile)
import Text.Read (readMaybe)
import qualified Control.Monad.Parallel as Par (mapM) -- mapM_
import Control.Parallel.Strategies (parMap, rseq)
import Text.Show.Pretty (ppShow)
import Data.FileStore.Utils (runShellCommand)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import System.Exit (ExitCode(ExitFailure))
import Data.Binary (decodeFile, encodeFile)

import LinkMetadata (readLinkMetadata, authorsTruncate, Metadata, MetadataItem)

import Columns (simplifiedDoc)

import Interwiki (convertInterwikiLinks, inlinesToString)

import qualified Data.Vector as V (toList, Vector)
import Control.Monad.Identity (runIdentity, Identity)
import Data.RPTree (knn, forest, metricL2, rpTreeCfg, fpMaxTreeDepth, fpDataChunkSize, fpProjNzDensity, fromListDv, DVector, Embed(..), RPForest)
import Data.Conduit (ConduitT)
import Data.Conduit.List (sourceList)

main :: IO ()
main = do md  <- readLinkMetadata
          edb <- readEmbeddings

          -- update for any missing embeddings, and return updated DB for computing distances & writing out fragments:
          let todo = sort $ missingEmbeddings md edb
          edb'' <- do if (length todo) == 0 then putStrLn "(Read databases; all updated.)" >> return edb else do
                       newEmbeddings <- Par.mapM embed todo
                       let edb' = nubOrd (edb ++ newEmbeddings)
                       writeEmbeddings edb'
                       return edb'

          let ddb = embeddings2Forest edb''
          let pairs  = parMap rseq (findN ddb bestNEmbeddings) edb''
          print $ head pairs
          -- writeOutMatches matches
          return ()

-- how many results do we want?
bestNEmbeddings :: Int
bestNEmbeddings = 10

embeddingsPath :: String
embeddingsPath = "metadata/embeddings.bin"

type Embedding  = (String, String, [Double]) -- NOTE: 'Float' in Haskell is 32-bit single-precision float (FP32); OA API apparently returns 64-bit double-precision (FP64), so we use 'Double' instead
type Embeddings = [Embedding]

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
    let document = T.pack $ replace "\n" "\n\n" $ unlines ["'"++t++"' " ++ "("++path++")" ++ ", by "++authorsTruncate aut++" ("++dt++").", "Keywords: "++(intercalate ", " tags) ++ ".", "Abstract: "++abst]
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
        gptPlainText = T.take maxLength $ T.replace "  " " " $ T.replace "  " " " $ T.replace "\n" "" $ T.replace "…" "..." $ T.replace "“" "'" $ T.replace "”" "'" $ T.replace "‘" "''" $ T.replace "’" "'" $ T.replace "\\" "" $ T.replace "\"" "'" $ T.replace "\"" "'" plainText
    in
      gptPlainText
  where
    maxLength :: Int
    maxLength = 8100 -- how long is too long? OA guesstimates 1 BPE = 4 characters on average (https://beta.openai.com/tokenizer), so 2047 BPEs ~ 8192 characters. If a call fails, the shell script will truncate the input and retry until it works so we don't need to set the upper limit too low.

    -- | Read 1 Pandoc AST and return its URLs/anchor-text pairs;
    -- if a URL has both a title and an anchor text, we return 2 pairs because both might be valid (eg '[GPT-3](https://arxiv.org/foo "Language Models are Few-Shot Learners")' - we would like to do link-suggestions on both the short noun 'GPT-3' and the paper title, but we can't if we arbitrarily return one but not the other).
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
oaAPIEmbed doc = do (status,_,mb) <- runShellCommand "./" Nothing "bash" ["static/build/embed.sh", T.unpack doc]
                    case status of
                      ExitFailure err -> error $ "Exit Failure: " ++ (intercalate " : " [show (T.length doc), T.unpack doc, ppShow status, ppShow err, ppShow mb])
                      _ -> do let results = lines $ U.toString mb
                              case results of
                                [] -> error $ "Failed to read embed.sh output? " ++ "\n" ++ show (T.length doc) ++ "\n" ++ T.unpack doc ++ "\n" ++ U.toString mb
                                (modelType:latents) -> let embeddingM = readMaybe (unlines latents) :: Maybe [Double] in
                                                         case embeddingM of
                                                           Nothing -> error $ "Failed to read embed.sh output? " ++ "\n" ++ show (T.length doc) ++ "\n" ++ T.unpack doc ++ "\n" ++ U.toString mb
                                                           Just embedding -> return (modelType, embedding)

-- type Distances = M.Map String [(String, Double)]
type Distances = [(String, [String])]

writeOutMatches :: [(String, [String])] -> IO ()
writeOutMatches = Prelude.mapM_ writeOutMatch -- Par.mapM_ writeOutMatch

writeOutMatch :: (String, [String]) -> IO ()
writeOutMatch = putStrLn . show

type Forest = RPForest Double (V.Vector (Embed DVector Double String))

embeddings2Forest :: Embeddings -> Forest
embeddings2Forest = embeddings2ForestConfigurable 50 10 15

embeddings2ForestConfigurable :: Int -> Int -> Int -> Embeddings -> Forest
embeddings2ForestConfigurable ls nt pvd es =
  let minLeafSize = ls -- ???
      cfg = rpTreeCfg minLeafSize
              (length es) -- data N
              (length $ (\(_,_,embedding) -> embedding) $ head es) -- dimension of each datapoint (eg 512 for ada-similarity embeddings)
      nTrees = nt -- ???
      projectionVectorDimension = pvd -- ???
  in
    runIdentity $
    forest 0 (fpMaxTreeDepth cfg) minLeafSize nTrees (fpDataChunkSize cfg) (fpProjNzDensity cfg) projectionVectorDimension $
    embeddings2Conduit es
 where
   embeddings2Conduit :: Embeddings -> ConduitT () (Embed DVector Double String) Identity ()
   embeddings2Conduit = sourceList . map (\(p,_,embedding) -> Embed (fromListDv embedding) p)


knnEmbedding :: Forest -> Int -> Embedding -> [(Double, Embed DVector Double String)]
knnEmbedding f k (_,_,embd) = V.toList $
                               -- NOTE: 'metricL2' *seems* to be the L2-normalized Euclidean distance? which is *proportional* to cosine similarity/distance: not identical, but produces the same ranking, and so just as good for my purpose here? or so claims https://stats.stackexchange.com/a/146279/16897
                               knn (metricL2) k f ((fromListDv embd)::DVector Double)

findNearest :: Forest -> Int -> Embedding -> [(String,Double)]
findNearest f k e = map (\(dist,Embed _ p) -> (p,dist)) $ knnEmbedding f k e

-- we'll keep the distance to insert into the metadata for debugging purposes.
findN :: Forest -> Int -> Embedding -> (String,[(String,Double)])
findN f k e@(p1,_,_) = let results = nub $ filter (\(p2,_) -> p1 /= p2) $ findNearest f k e in
                 -- NOTE: 'knn' is the fastest, but seems to return duplicate results, so requesting 10 doesn't return 10 unique hits.
                 -- (I'm not sure why, the rp-tree docs don't mention or warn about this that I noticed...)
                 -- If that happens, back off and request more k.
                 if length results < bestNEmbeddings then findN f (k*2) e else (p1,results)

-- hyperparameterSweep :: Embeddings -> [(Double, (Int,Int,Int))]
-- hyperparameterSweep edb =
--   let gridsearchParameters = [(1,1,2), (11,1,2), (21,1,2), (31,1,2), (41,1,2), (51,1,2), (61,1,2), (71,1,2), (81,1,2), (91,1,2), (1,3,2), (11,3,2), (21,3,2), (31,3,2), (41,3,2), (51,3,2), (61,3,2), (71,3,2), (81,3,2), (91,3,2), (1,5,2), (11,5,2), (21,5,2), (31,5,2), (41,5,2), (51,5,2), (61,5,2), (71,5,2), (81,5,2), (91,5,2), (1,7,2), (11,7,2), (21,7,2), (31,7,2), (41,7,2), (51,7,2), (61,7,2), (71,7,2), (81,7,2), (91,7,2), (1,9,2), (11,9,2), (21,9,2), (31,9,2), (41,9,2), (51,9,2), (61,9,2), (71,9,2), (81,9,2), (91,9,2), (1,11,2), (11,11,2), (21,11,2), (31,11,2), (41,11,2), (51,11,2), (61,11,2), (71,11,2), (81,11,2), (91,11,2), (1,13,2), (11,13,2), (21,13,2), (31,13,2), (41,13,2), (51,13,2), (61,13,2), (71,13,2), (81,13,2), (91,13,2), (1,15,2), (11,15,2), (21,15,2), (31,15,2), (41,15,2), (51,15,2), (61,15,2), (71,15,2), (81,15,2), (91,15,2), (1,17,2), (11,17,2), (21,17,2), (31,17,2), (41,17,2), (51,17,2), (61,17,2), (71,17,2), (81,17,2), (91,17,2), (1,19,2), (11,19,2), (21,19,2), (31,19,2), (41,19,2), (51,19,2), (61,19,2), (71,19,2), (81,19,2), (91,19,2), (1,1,4), (11,1,4), (21,1,4), (31,1,4), (41,1,4), (51,1,4), (61,1,4), (71,1,4), (81,1,4), (91,1,4), (1,3,4), (11,3,4), (21,3,4), (31,3,4), (41,3,4), (51,3,4), (61,3,4), (71,3,4), (81,3,4), (91,3,4), (1,5,4), (11,5,4), (21,5,4), (31,5,4), (41,5,4), (51,5,4), (61,5,4), (71,5,4), (81,5,4), (91,5,4), (1,7,4), (11,7,4), (21,7,4), (31,7,4), (41,7,4), (51,7,4), (61,7,4), (71,7,4), (81,7,4), (91,7,4), (1,9,4), (11,9,4), (21,9,4), (31,9,4), (41,9,4), (51,9,4), (61,9,4), (71,9,4), (81,9,4), (91,9,4), (1,11,4), (11,11,4), (21,11,4), (31,11,4), (41,11,4), (51,11,4), (61,11,4), (71,11,4), (81,11,4), (91,11,4), (1,13,4), (11,13,4), (21,13,4), (31,13,4), (41,13,4), (51,13,4), (61,13,4), (71,13,4), (81,13,4), (91,13,4), (1,15,4), (11,15,4), (21,15,4), (31,15,4), (41,15,4), (51,15,4), (61,15,4), (71,15,4), (81,15,4), (91,15,4), (1,17,4), (11,17,4), (21,17,4), (31,17,4), (41,17,4), (51,17,4), (61,17,4), (71,17,4), (81,17,4), (91,17,4), (1,19,4), (11,19,4), (21,19,4), (31,19,4), (41,19,4), (51,19,4), (61,19,4), (71,19,4), (81,19,4), (91,19,4), (1,1,6), (11,1,6), (21,1,6), (31,1,6), (41,1,6), (51,1,6), (61,1,6), (71,1,6), (81,1,6), (91,1,6), (1,3,6), (11,3,6), (21,3,6), (31,3,6), (41,3,6), (51,3,6), (61,3,6), (71,3,6), (81,3,6), (91,3,6), (1,5,6), (11,5,6), (21,5,6), (31,5,6), (41,5,6), (51,5,6), (61,5,6), (71,5,6), (81,5,6), (91,5,6), (1,7,6), (11,7,6), (21,7,6), (31,7,6), (41,7,6), (51,7,6), (61,7,6), (71,7,6), (81,7,6), (91,7,6), (1,9,6), (11,9,6), (21,9,6), (31,9,6), (41,9,6), (51,9,6), (61,9,6), (71,9,6), (81,9,6), (91,9,6), (1,11,6), (11,11,6), (21,11,6), (31,11,6), (41,11,6), (51,11,6), (61,11,6), (71,11,6), (81,11,6), (91,11,6), (1,13,6), (11,13,6), (21,13,6), (31,13,6), (41,13,6), (51,13,6), (61,13,6), (71,13,6), (81,13,6), (91,13,6), (1,15,6), (11,15,6), (21,15,6), (31,15,6), (41,15,6), (51,15,6), (61,15,6), (71,15,6), (81,15,6), (91,15,6), (1,17,6), (11,17,6), (21,17,6), (31,17,6), (41,17,6), (51,17,6), (61,17,6), (71,17,6), (81,17,6), (91,17,6), (1,19,6), (11,19,6), (21,19,6), (31,19,6), (41,19,6), (51,19,6), (61,19,6), (71,19,6), (81,19,6), (91,19,6), (1,1,8), (11,1,8), (21,1,8), (31,1,8), (41,1,8), (51,1,8), (61,1,8), (71,1,8), (81,1,8), (91,1,8), (1,3,8), (11,3,8), (21,3,8), (31,3,8), (41,3,8), (51,3,8), (61,3,8), (71,3,8), (81,3,8), (91,3,8), (1,5,8), (11,5,8), (21,5,8), (31,5,8), (41,5,8), (51,5,8), (61,5,8), (71,5,8), (81,5,8), (91,5,8), (1,7,8), (11,7,8), (21,7,8), (31,7,8), (41,7,8), (51,7,8), (61,7,8), (71,7,8), (81,7,8), (91,7,8), (1,9,8), (11,9,8), (21,9,8), (31,9,8), (41,9,8), (51,9,8), (61,9,8), (71,9,8), (81,9,8), (91,9,8), (1,11,8), (11,11,8), (21,11,8), (31,11,8), (41,11,8), (51,11,8), (61,11,8), (71,11,8), (81,11,8), (91,11,8), (1,13,8), (11,13,8), (21,13,8), (31,13,8), (41,13,8), (51,13,8), (61,13,8), (71,13,8), (81,13,8), (91,13,8), (1,15,8), (11,15,8), (21,15,8), (31,15,8), (41,15,8), (51,15,8), (61,15,8), (71,15,8), (81,15,8), (91,15,8), (1,17,8), (11,17,8), (21,17,8), (31,17,8), (41,17,8), (51,17,8), (61,17,8), (71,17,8), (81,17,8), (91,17,8), (1,19,8), (11,19,8), (21,19,8), (31,19,8), (41,19,8), (51,19,8), (61,19,8), (71,19,8), (81,19,8), (91,19,8), (1,1,10), (11,1,10), (21,1,10), (31,1,10), (41,1,10), (51,1,10), (61,1,10), (71,1,10), (81,1,10), (91,1,10), (1,3,10), (11,3,10), (21,3,10), (31,3,10), (41,3,10), (51,3,10), (61,3,10), (71,3,10), (81,3,10), (91,3,10), (1,5,10), (11,5,10), (21,5,10), (31,5,10), (41,5,10), (51,5,10), (61,5,10), (71,5,10), (81,5,10), (91,5,10), (1,7,10), (11,7,10), (21,7,10), (31,7,10), (41,7,10), (51,7,10), (61,7,10), (71,7,10), (81,7,10), (91,7,10), (1,9,10), (11,9,10), (21,9,10), (31,9,10), (41,9,10), (51,9,10), (61,9,10), (71,9,10), (81,9,10), (91,9,10), (1,11,10), (11,11,10), (21,11,10), (31,11,10), (41,11,10), (51,11,10), (61,11,10), (71,11,10), (81,11,10), (91,11,10), (1,13,10), (11,13,10), (21,13,10), (31,13,10), (41,13,10), (51,13,10), (61,13,10), (71,13,10), (81,13,10), (91,13,10), (1,15,10), (11,15,10), (21,15,10), (31,15,10), (41,15,10), (51,15,10), (61,15,10), (71,15,10), (81,15,10), (91,15,10), (1,17,10), (11,17,10), (21,17,10), (31,17,10), (41,17,10), (51,17,10), (61,17,10), (71,17,10), (81,17,10), (91,17,10), (1,19,10), (11,19,10), (21,19,10), (31,19,10), (41,19,10), (51,19,10), (61,19,10), (71,19,10), (81,19,10), (91,19,10), (1,1,12), (11,1,12), (21,1,12), (31,1,12), (41,1,12), (51,1,12), (61,1,12), (71,1,12), (81,1,12), (91,1,12), (1,3,12), (11,3,12), (21,3,12), (31,3,12), (41,3,12), (51,3,12), (61,3,12), (71,3,12), (81,3,12), (91,3,12), (1,5,12), (11,5,12), (21,5,12), (31,5,12), (41,5,12), (51,5,12), (61,5,12), (71,5,12), (81,5,12), (91,5,12), (1,7,12), (11,7,12), (21,7,12), (31,7,12), (41,7,12), (51,7,12), (61,7,12), (71,7,12), (81,7,12), (91,7,12), (1,9,12), (11,9,12), (21,9,12), (31,9,12), (41,9,12), (51,9,12), (61,9,12), (71,9,12), (81,9,12), (91,9,12), (1,11,12), (11,11,12), (21,11,12), (31,11,12), (41,11,12), (51,11,12), (61,11,12), (71,11,12), (81,11,12), (91,11,12), (1,13,12), (11,13,12), (21,13,12), (31,13,12), (41,13,12), (51,13,12), (61,13,12), (71,13,12), (81,13,12), (91,13,12), (1,15,12), (11,15,12), (21,15,12), (31,15,12), (41,15,12), (51,15,12), (61,15,12), (71,15,12), (81,15,12), (91,15,12), (1,17,12), (11,17,12), (21,17,12), (31,17,12), (41,17,12), (51,17,12), (61,17,12), (71,17,12), (81,17,12), (91,17,12), (1,19,12), (11,19,12), (21,19,12), (31,19,12), (41,19,12), (51,19,12), (61,19,12), (71,19,12), (81,19,12), (91,19,12), (1,1,14), (11,1,14), (21,1,14), (31,1,14), (41,1,14), (51,1,14), (61,1,14), (71,1,14), (81,1,14), (91,1,14), (1,3,14), (11,3,14), (21,3,14), (31,3,14), (41,3,14), (51,3,14), (61,3,14), (71,3,14), (81,3,14), (91,3,14), (1,5,14), (11,5,14), (21,5,14), (31,5,14), (41,5,14), (51,5,14), (61,5,14), (71,5,14), (81,5,14), (91,5,14), (1,7,14), (11,7,14), (21,7,14), (31,7,14), (41,7,14), (51,7,14), (61,7,14), (71,7,14), (81,7,14), (91,7,14), (1,9,14), (11,9,14), (21,9,14), (31,9,14), (41,9,14), (51,9,14), (61,9,14), (71,9,14), (81,9,14), (91,9,14), (1,11,14), (11,11,14), (21,11,14), (31,11,14), (41,11,14), (51,11,14), (61,11,14), (71,11,14), (81,11,14), (91,11,14), (1,13,14), (11,13,14), (21,13,14), (31,13,14), (41,13,14), (51,13,14), (61,13,14), (71,13,14), (81,13,14), (91,13,14), (1,15,14), (11,15,14), (21,15,14), (31,15,14), (41,15,14), (51,15,14), (61,15,14), (71,15,14), (81,15,14), (91,15,14), (1,17,14), (11,17,14), (21,17,14), (31,17,14), (41,17,14), (51,17,14), (61,17,14), (71,17,14), (81,17,14), (91,17,14), (1,19,14), (11,19,14), (21,19,14), (31,19,14), (41,19,14), (51,19,14), (61,19,14), (71,19,14), (81,19,14), (91,19,14), (1,1,16), (11,1,16), (21,1,16), (31,1,16), (41,1,16), (51,1,16), (61,1,16), (71,1,16), (81,1,16), (91,1,16), (1,3,16), (11,3,16), (21,3,16), (31,3,16), (41,3,16), (51,3,16), (61,3,16), (71,3,16), (81,3,16), (91,3,16), (1,5,16), (11,5,16), (21,5,16), (31,5,16), (41,5,16), (51,5,16), (61,5,16), (71,5,16), (81,5,16), (91,5,16), (1,7,16), (11,7,16), (21,7,16), (31,7,16), (41,7,16), (51,7,16), (61,7,16), (71,7,16), (81,7,16), (91,7,16), (1,9,16), (11,9,16), (21,9,16), (31,9,16), (41,9,16), (51,9,16), (61,9,16), (71,9,16), (81,9,16), (91,9,16), (1,11,16), (11,11,16), (21,11,16), (31,11,16), (41,11,16), (51,11,16), (61,11,16), (71,11,16), (81,11,16), (91,11,16), (1,13,16), (11,13,16), (21,13,16), (31,13,16), (41,13,16), (51,13,16), (61,13,16), (71,13,16), (81,13,16), (91,13,16), (1,15,16), (11,15,16), (21,15,16), (31,15,16), (41,15,16), (51,15,16), (61,15,16), (71,15,16), (81,15,16), (91,15,16), (1,17,16), (11,17,16), (21,17,16), (31,17,16), (41,17,16), (51,17,16), (61,17,16), (71,17,16), (81,17,16), (91,17,16), (1,19,16), (11,19,16), (21,19,16), (31,19,16), (41,19,16), (51,19,16), (61,19,16), (71,19,16), (81,19,16), (91,19,16), (1,1,18), (11,1,18), (21,1,18), (31,1,18), (41,1,18), (51,1,18), (61,1,18), (71,1,18), (81,1,18), (91,1,18), (1,3,18), (11,3,18), (21,3,18), (31,3,18), (41,3,18), (51,3,18), (61,3,18), (71,3,18), (81,3,18), (91,3,18), (1,5,18), (11,5,18), (21,5,18), (31,5,18), (41,5,18), (51,5,18), (61,5,18), (71,5,18), (81,5,18), (91,5,18), (1,7,18), (11,7,18), (21,7,18), (31,7,18), (41,7,18), (51,7,18), (61,7,18), (71,7,18), (81,7,18), (91,7,18), (1,9,18), (11,9,18), (21,9,18), (31,9,18), (41,9,18), (51,9,18), (61,9,18), (71,9,18), (81,9,18), (91,9,18), (1,11,18), (11,11,18), (21,11,18), (31,11,18), (41,11,18), (51,11,18), (61,11,18), (71,11,18), (81,11,18), (91,11,18), (1,13,18), (11,13,18), (21,13,18), (31,13,18), (41,13,18), (51,13,18), (61,13,18), (71,13,18), (81,13,18), (91,13,18), (1,15,18), (11,15,18), (21,15,18), (31,15,18), (41,15,18), (51,15,18), (61,15,18), (71,15,18), (81,15,18), (91,15,18), (1,17,18), (11,17,18), (21,17,18), (31,17,18), (41,17,18), (51,17,18), (61,17,18), (71,17,18), (81,17,18), (91,17,18), (1,19,18), (11,19,18), (21,19,18), (31,19,18), (41,19,18), (51,19,18), (61,19,18), (71,19,18), (81,19,18), (91,19,18), (1,1,20), (11,1,20), (21,1,20), (31,1,20), (41,1,20), (51,1,20), (61,1,20), (71,1,20), (81,1,20), (91,1,20), (1,3,20), (11,3,20), (21,3,20), (31,3,20), (41,3,20), (51,3,20), (61,3,20), (71,3,20), (81,3,20), (91,3,20), (1,5,20), (11,5,20), (21,5,20), (31,5,20), (41,5,20), (51,5,20), (61,5,20), (71,5,20), (81,5,20), (91,5,20), (1,7,20), (11,7,20), (21,7,20), (31,7,20), (41,7,20), (51,7,20), (61,7,20), (71,7,20), (81,7,20), (91,7,20), (1,9,20), (11,9,20), (21,9,20), (31,9,20), (41,9,20), (51,9,20), (61,9,20), (71,9,20), (81,9,20), (91,9,20), (1,11,20), (11,11,20), (21,11,20), (31,11,20), (41,11,20), (51,11,20), (61,11,20), (71,11,20), (81,11,20), (91,11,20), (1,13,20), (11,13,20), (21,13,20), (31,13,20), (41,13,20), (51,13,20), (61,13,20), (71,13,20), (81,13,20), (91,13,20), (1,15,20), (11,15,20), (21,15,20), (31,15,20), (41,15,20), (51,15,20), (61,15,20), (71,15,20), (81,15,20), (91,15,20), (1,17,20), (11,17,20), (21,17,20), (31,17,20), (41,17,20), (51,17,20), (61,17,20), (71,17,20), (81,17,20), (91,17,20), (1,19,20), (11,19,20), (21,19,20), (31,19,20), (41,19,20), (51,19,20), (61,19,20), (71,19,20), (81,19,20), (91,19,20)] :: [(Int,Int,Int)] in
--   reverse $ sort $ map (\z@(a,b,c) -> let f = embeddings2ForestConfigurable a b c edb in (recallWith inner f 10 $ (\(_,_,embd) -> ((fromListDv embd)::DVector Double)) $ head edb, z)) gridsearchParameters

{-
edb <- readEmbeddings
let f = embeddings2Forest edb
recallWith inner f 10 $ (\(_,_,embd) -> ((fromListDv embd)::DVector Double)) $ head edb

findNearest f 10 $ head edb

-}
