#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
-- dependencies: libghc-pandoc-dev

module Main where

import Text.Pandoc (def, extensionsFromList, queryWith, readerExtensions, readMarkdown, runIOorExplode, unMeta,
                    pandocExtensions, Inline(Str), Pandoc(Pandoc), Block(Para), MetaValue(MetaInlines))
import qualified Data.Text as T (pack, take, intercalate, unpack, Text)
import qualified Data.Text.IO as TIO (readFile)
import System.Environment (getArgs)
import qualified Data.Map as M (lookup)

-- | Map over the filenames
main :: IO ()
main = do
  fs <- getArgs
  mapM_ printDropcap fs

-- | Read 1 file and print out its URLs
printDropcap :: FilePath -> IO ()
printDropcap file = do
  input <- TIO.readFile file -- we drop the first 100 characters to skip the YAML metadata
  [dropCap, letter] <- dropcap input
  print $ T.unpack $  T.intercalate ":" [dropCap, letter, T.pack file]

-- | Read one Text string, parse as YAML+Markdown, extract the CSS extension (ie dropcap used), take the first letter of the first paragraph after the abstract, and return the dropcap + dropped letter.
dropcap :: T.Text -> IO [T.Text]
dropcap txt = runIOorExplode $
  -- if we don't explicitly enable footnotes, Pandoc interprets the footnotes as broken links, which throws many spurious warnings to stdout
  do (Pandoc m bs) <- readMarkdown def{readerExtensions = pandocExtensions} txt
     let dropCapType = M.lookup "cssExtension" $ unMeta m
     case dropCapType of
       Nothing -> let contents = extractParagraphs [bs !! 2] in
                                                  return  ["drop-caps-de-zs", T.take 1 $ head contents]
       Just (MetaInlines [Str d]) -> let contents = extractParagraphs [bs !! 2] in
                                                  return  [d, T.take 1 $ head contents]
       Just _ -> return []

extractParagraphs :: [Block] -> [T.Text]
extractParagraphs = queryWith extractParagraph
  where
    extractParagraph :: Block -> [T.Text]
    extractParagraph x@(Para _) = [queryWith extractStr x]
    extractParagraph _ = []
    extractStr :: Inline -> T.Text
    extractStr (Str s) = s
    extractStr _ = ""
