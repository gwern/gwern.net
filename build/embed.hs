#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Pandoc (def, pandocExtensions, queryWith, readerExtensions, readHtml, Inline(Link), runPure, Pandoc)
import Text.Pandoc.Walk (walk)
import qualified Data.Text as T  (append, drop, head, init, intercalate, last, length, pack, replace, take, unlines, unpack, Text)
import Data.List ((\\), intercalate, sort, sortOn, nub)
import Data.List.Utils (replace)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M (filter, fromListWith, keys, lookup, map, Map)
import System.Directory (doesFileExist, renameFile)
import Data.Containers.ListUtils (nubOrd)
import System.IO.Temp (emptySystemTempFile)
import Text.Read (readMaybe)
import qualified Control.Monad.Parallel as Par (mapM) -- mapM_
import Text.Show.Pretty (ppShow)
import Data.FileStore.Utils (runShellCommand)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import System.Exit (ExitCode(ExitFailure))
import Data.Binary (decodeFile, encodeFile)

import LinkMetadata (readLinkMetadata, authorsTruncate, Metadata, MetadataItem)

import Columns (simplifiedDoc)

import Interwiki (convertInterwikiLinks, inlinesToString)

main :: IO ()
main = do md  <- readLinkMetadata
          edb <- readEmbeddings

          -- update for any missing embeddings, and return updated DB for computing distances & writing out fragments:
          let todo = sort $ missingEmbeddings md edb
          edb'' <- do if (length todo) == 0 then putStrLn "done" >> return edb else do
                       newEmbeddings <- Par.mapM embed todo
                       let edb' = nubOrd (edb ++ newEmbeddings)
                       writeEmbeddings edb'
                       return edb'

          let distances = embeddingDistances $ take 500 edb''
          let targets   = M.keys distances
          let matches   = (map (\target -> (target, map fst $ take topNEmbeddings $
                                                    fromJust $ M.lookup target distances) )
                            targets) :: [(String, [String])]
          writeOutMatches matches
          return ()

-- how many results do we want?
topNEmbeddings :: Int
topNEmbeddings = 10

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

uniquePairs :: (Ord a) => [a] -> [(a,a)]
uniquePairs l = [(x,y) | x <- l, y <- l, x < y]

-- copied from https://ardoris.wordpress.com/2014/08/14/cosine-similarity-pearson-correlation-inner-products/
cosineSimilarity :: [Double] -> [Double] -> Double
cosineSimilarity = similarity dot
 where similarity ip xs ys = (ip xs ys) / ( (len xs) * (len ys) )
         where len xs' = sqrt (ip xs' xs')
       -- the inner products
       dot xs ys = sum $ zipWith (*) xs ys

type Distances = M.Map String [(String, Double)]

-- create a dictionary to compute all pairwise distances (𝒪(n²)); store both versions so we can look up by either of the pair:
-- λ putStrLn $ ppShow $ embeddingDistances [("foo", "bar", [1,0]), ("foo2", "bar", [0.9,0.5]), ("foo3", "bar", [0.2,0.2])]
-- → fromList
--   [ ( "foo"
--     , [ ( "foo2" , 0.8741572761215378 )
--       , ( "foo3" , 0.7071067811865475 )
--       ]
--     )
--   , ( "foo2"
--     , [ ( "foo3" , 0.961523947640823 )
--       , ( "foo" , 0.8741572761215378 )
--       ]
--     )
--   , ( "foo3"
--     , [ ( "foo2" , 0.961523947640823 )
--       , ( "foo" , 0.7071067811865475 )
--       ]
--     )
--   ]
embeddingDistances :: Embeddings -> Distances
embeddingDistances es = let allPairs = uniquePairs es in
  M.map (take topNEmbeddings . reverse . nub . sortOn snd) $! M.fromListWith (\a b -> a++b) $!
          concat $! Par.mapM (\((p1,md1,embed1),(p2,md2,embed2)) -> if md1/=md2 then [] else
                              let distance = cosineSimilarity embed1 embed2 in
                                [(p1, [(p2, distance)]), (p2, [(p1, distance)])]
                                                                   ) allPairs


writeOutMatches :: [(String, [String])] -> IO ()
writeOutMatches = Prelude.mapM_ writeOutMatch -- Par.mapM_ writeOutMatch

writeOutMatch :: (String, [String]) -> IO ()
writeOutMatch = putStrLn . show
