#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Pandoc (def, nullMeta, pandocExtensions, queryWith, readerExtensions,
                     readHtml, readMarkdown, runPure, writeHtml5String,
                     Pandoc(Pandoc), Block(BulletList,Para), Inline(Link,Str))
import Text.Pandoc.Walk (walk)
import qualified Data.Text as T -- (append, isPrefixOf, isInfixOf, isSuffixOf, head, pack, unpack, tail, takeWhile, Text, replace)
import qualified Data.Text.IO as TIO (readFile)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, (\\), intercalate, sort)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M -- (filter, lookup, keys, elems, traverseWithKey, fromListWith, union)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import Network.HTTP (urlDecode, urlEncode)
import Data.List.Utils (replace)
import Data.Containers.ListUtils (nubOrd)
import System.IO.Temp (writeSystemTempFile)
import Control.Monad (forM_, unless)
import Text.Read (readMaybe)
import Control.Monad.Parallel as Par (mapM)
import Text.Show.Pretty (ppShow)
import Data.FileStore.Utils (runShellCommand)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import System.Exit (ExitCode(ExitFailure))

import System.Environment (getEnv)

import LinkMetadata (sed, hasAnnotation, readLinkMetadata, generateID, Metadata, MetadataItem, readBacklinksDB, writeBacklinksDB, safeHtmlWriterOptions, authorsTruncate)

import Columns (simplifiedDoc)

import Interwiki (convertInterwikiLinks, inlinesToString)

main :: IO ()
main = do md  <- readLinkMetadata
          edb <- readEmbeddings
          let todo = take 500 $ sort $ missingEmbeddings md edb
          newEmbeddings <- Par.mapM embed todo
          let edb' = nubOrd (edb ++ newEmbeddings)
          writeEmbeddings edb'
          return ()

embeddingsPath :: String
embeddingsPath = "metadata/embeddings.hs"

type Embedding  = (String, String, [Double]) -- NOTE: 'Float' in Haskell is 32-bit single-precision float (FP32); OA API apparently returns 64-bit double-precision (FP64), so we use 'Double' instead
type Embeddings = [Embedding]

readEmbeddings :: IO Embeddings
readEmbeddings = do embeddingF <- readFile embeddingsPath
                    let embeddings = readMaybe embeddingF :: Maybe Embeddings
                    case embeddings of
                      Nothing -> error $ "Malformed embeddings database didn't parse?"
                      Just embeddingDB -> return embeddingDB

writeEmbeddings :: Embeddings -> IO ()
writeEmbeddings edb = updateFile embeddingsPath (ppShow edb)
 where
  updateFile :: FilePath -> String -> IO ()
  updateFile f contentsNew = do t <- writeSystemTempFile "hakyll-embeddings" contentsNew
                                existsOld <- doesFileExist f
                                if not existsOld then
                                  renameFile t f
                                  else
                                    do contentsOld <- Prelude.readFile f
                                       if contentsNew /= contentsOld then renameFile t f else removeFile t

missingEmbeddings :: Metadata -> Embeddings -> [(String, MetadataItem)]
missingEmbeddings md edb = let urlsToCheck = M.keys $ M.filter (\(t, aut, _, _, tags, abst) -> length (t++aut++show tags++abst) > minimumLength) md
                               urlsEmbedded = map (\(u,_,_) -> u) edb :: [String]
                               missing      = urlsToCheck \\ urlsEmbedded
                               in map (\u -> (u, fromJust $ M.lookup u md)) missing

  where minimumLength :: Int
        minimumLength = 400 -- how many characters long should metadata be before it is worth trying to embed?

-- convert an annotated item into a single text string: concatenate the useful metadata
formatDoc :: MetadataItem -> T.Text
formatDoc mi@(t,aut,dt,_,tags,abst) =
    let document = T.pack $ unlines ["'"++t++"', by "++authorsTruncate aut++" ("++dt++").", "Keywords: "++(replace "\"" "" $ ppShow tags) ++ ".", "Abstract: "++abst]
        parsedEither = let parsed = runPure $ readHtml def{readerExtensions = pandocExtensions } document
                       in case parsed of
                          Left e -> error $ "Failed to parse HTML document into Pandoc AST: error: " ++ show e ++ " : " ++ show mi ++ " : " ++ T.unpack document
                          Right p -> p
        -- create a numbered list of URL references inside each document to expose it to the embedding model, as 'simplifiedDoc' necessarily strips URLs:
        documentURLs = "References:\n" `T.append` T.unlines (map (\(n, (url,titles)) -> T.pack n `T.append` ". " `T.append` url `T.append` " " `T.append` (T.intercalate ", " $ tail titles)) $ zip (map show [(1::Int)..]) (extractURLs parsedEither))
        -- simple plaintext ASCII-ish version, which hopefully is more comprehensible to NN models than full-blown HTML
        plainText = simplifiedDoc parsedEither `T.append` documentURLs
        -- post-processing: 'We suggest replacing newlines (\n) in your input with a single space, as we have observed inferior results when newlines are present.' https://beta.openai.com/docs/api-reference/embeddings/create
        -- GPT-3 apparently doesn't do well with Unicode punctuation either (they get a bad BPE expansion factor too), so smart quotes are right out.
        gptPlainText = T.take maxLength $ T.replace "\n" "" $ T.replace "“" "'" $ T.replace "”" "'" $ T.replace "‘" "''" $ T.replace "’" "'" $ T.replace "\\" "" $ T.replace "\"" "'" $ T.replace "\"" "'" plainText
    in
      gptPlainText
  where
    maxLength :: Int
    maxLength = 7900 -- how long is too long? OA guesstimates 1 BPE = 4 characters on average (https://beta.openai.com/tokenizer), so 2047 BPEs ~ 8192 characters.

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
embed (p,mi) = do
  let doc = formatDoc mi
  (modelType,embedding) <- oaAPIEmbed doc
  return (p,modelType,embedding)

-- we shell out to a Bash script `similar.sh`
oaAPIEmbed :: T.Text -> IO (String,[Double])
oaAPIEmbed doc = do (status,_,mb) <- runShellCommand "./" Nothing "bash" ["static/build/embed.sh", T.unpack doc]
                    case status of
                      ExitFailure err -> error $ "Exit Failure: " ++ (intercalate " : " [show (T.length doc), T.unpack doc, ppShow status, ppShow err, ppShow mb])
                      _ -> do let (modelType:latents) = lines $ U.toString mb
                              let embeddingM = readMaybe (unlines latents) :: Maybe [Double]
                              case embeddingM of
                                Nothing -> error $ "Failed to read embed.sh output? " ++ "\n" ++ show (T.length doc) ++ "\n" ++ T.unpack doc
                                Just embedding -> return (modelType, embedding)
