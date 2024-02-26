#!/usr/bin/env runghc
{-# LANGUAGE OverloadedStrings #-}
-- dependencies: libghc-pandoc-dev

-- usage: 'link-extractor.hs [--print-filenames] [file]'; prints out a newline-delimited list of hyperlinks found in
-- targeted Pandoc Markdown .md files (or simple Pandoc-readable HTML .html files) when parsed.
-- Local anchor links are rewritten assuming Gwern.net-style paths of Markdown .md files (ie. a link like `[discriminator ranking](#discriminator-ranking)` in ~/wiki/face.md will be parsed to `/face#discriminator-ranking`). Interwiki links are rewritten to their full URLs.
--
-- If no filename arguments, link-extractor will instead read stdin as Markdown and attempt to parse that instead (falling back to HTML if no URLs are parsed).
-- This makes it easy to pipe in arbitrary sections of pages or annotations, such as `$ xclip -o | runghc -i/home/gwern/wiki/static/build/ /home/gwern/wiki/static/build/link-extractor.hs`.
--
-- Hyperlinks are not necessarily to the WWW but can be internal or interwiki hyperlinks (eg.
-- '/local/file.pdf' or '!W').
-- This reads multiple files and processes them one by one, so is not parallelized, but you can parallelize it at the process level with eg. `parallel --max-args=500 --jobs 30` to use 30 cores, more or less.

module Main where

import Control.Monad (unless)
import Data.List (isSuffixOf)
import qualified Data.Text as T (append,  head, pack, unlines)
import qualified Data.Text.IO as TIO (getContents, readFile, putStr, putStrLn)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)

import Query (extractLinks)

-- | Map over the filenames
main :: IO ()
main = do
  fs <- getArgs
  let printfilename = take 1 fs == ["--print-filenames"]
  let fs' = if printfilename then Prelude.drop 1 fs else fs
  if null fs then do stdin <- TIO.getContents
                     let links = extractLinks True stdin
                     let links' = if links /= [] then links else extractLinks False stdin
                     mapM_ TIO.putStrLn links'
    else mapM_ (printURLs printfilename) fs'

-- | Read 1 file and print out its URLs
printURLs :: Bool -> FilePath -> IO ()
printURLs printfilename file = do
  exists <- doesFileExist file
  unless exists $ error ("A specified file argument is invalid/does not exist? Arguments: " ++ show printfilename ++ " : " ++ file)
  input <- TIO.readFile file
  let converted = extractLinks (".md"`isSuffixOf`file) input
  -- rewrite self-links like "#discriminator-ranking" → "/face#discriminator-ranking" by prefixing the original Markdown filename's absolute-ized base-name;
  -- this makes frequency counts more informative, eg. for deciding what sections to refactor out into standalone pages (because heavy cross-referencing
  -- *inside* a page is an important indicator of a section being 'too big', just like cross-page references are).
  let converted' = map (\u -> if T.head u /= '#' then u else "/" `T.append` T.pack (takeBaseName file) `T.append` u) converted

  if printfilename then TIO.putStr $ T.unlines $ Prelude.map (\url -> T.pack file `T.append` ":" `T.append` url) converted' else
     TIO.putStr $ T.unlines converted'
