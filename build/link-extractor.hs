#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
-- dependencies: libghc-pandoc-dev

-- usage: 'link-extract.hs [file]'; prints out a newline-delimited list of hyperlinks found in targeted Pandoc Markdown files when parsed.
-- Hyperlinks are not necessarily to the WWW but can be internal or interwiki hyperlinks (eg '/local/file.pdf' or '!Wikipedia').

module Main where

import Text.Pandoc (def, extensionsFromList, queryWith, readerExtensions, readMarkdown, runIOorExplode,
                     Extension(Ext_footnotes), Inline(Link), Pandoc)
import Data.Text as T (append, drop, pack, unlines, Text)
import qualified Data.Text.IO as TIO (readFile, putStr)
import System.Environment (getArgs)

-- | Map over the filenames
main :: IO ()
main = do
  fs <- getArgs
  let printfilename = (fs !! 0) == "--print-filenames"
  let fs' = if printfilename then Prelude.drop 1 fs else fs
  mapM_ (printURLs printfilename) fs'

-- | Read 1 file and print out its URLs
printURLs :: Bool -> FilePath -> IO ()
printURLs printfilename file = do
  input <- fmap (T.drop 100) $ TIO.readFile file -- we drop the first 100 characters to skip the YAML metadata
  converted <- extractLinks input
  if printfilename then TIO.putStr $ T.unlines $ Prelude.map (\url -> (T.pack file) `T.append` ":" `T.append` url) converted else
     TIO.putStr $ T.unlines $ converted

-- | Read one Text string and return its URLs (as Strings)
extractLinks :: Text -> IO [T.Text]
extractLinks txt = runIOorExplode $
  -- if we don't explicitly enable footnotes, Pandoc interprets the footnotes as broken links, which throws many spurious warnings to stdout
  do parsed <- readMarkdown def{readerExtensions = extensionsFromList [Ext_footnotes]} txt
     let links = extractURLs parsed
     return links

-- | Read 1 Pandoc AST and return its URLs as Strings
extractURLs :: Pandoc -> [T.Text]
extractURLs = queryWith extractURL
 where
   extractURL :: Inline -> [T.Text]
   extractURL (Link _ _ (u,_)) = [u]
   extractURL _ = []
