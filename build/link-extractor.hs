#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
-- dependencies: libghc-pandoc-dev

-- usage: 'link-extract.hs [file]'; prints out a newline-delimited list of hyperlinks found in targeted Pandoc Markdown files when parsed.
-- Hyperlinks are not necessarily to the WWW but can be internal or interwiki hyperlinks (eg. '/local/file.pdf' or '!W').

module Main where

import qualified Data.Text as T (append,  head, pack, unlines)
import qualified Data.Text.IO as TIO (readFile, putStr)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)

import Query (extractLinks)

-- | Map over the filenames
main :: IO ()
main = do
  fs <- getArgs
  let printfilename = (head fs) == "--print-filenames"
  let fs' = if printfilename then Prelude.drop 1 fs else fs
  mapM_ (printURLs printfilename) fs'

-- | Read 1 file and print out its URLs
printURLs :: Bool -> FilePath -> IO ()
printURLs printfilename file = do
  input <- TIO.readFile file
  let converted = extractLinks True input
  -- rewrite self-links like "#discriminator-ranking" → "/Faces#discriminator-ranking" by prefixing the original Markdown filename's absolute-ized basename;
  -- this makes frequency counts more informative, eg. for deciding what sections to refactor out into standalone pages (because heavy cross-referencing
  -- *inside* a page is an important indicator of a section being 'too big', just like cross-page references are).
  let converted' = map (\u -> if T.head u /= '#' then u else "/" `T.append` (T.pack $ takeBaseName file) `T.append` u) converted

  if printfilename then TIO.putStr $ T.unlines $ Prelude.map (\url -> (T.pack file) `T.append` ":" `T.append` url) converted' else
     TIO.putStr $ T.unlines converted'
