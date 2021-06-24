#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
-- dependencies: libghc-pandoc-dev

-- usage: 'link-extract.hs [file]'; prints out a newline-delimited list of hyperlinks found in targeted Pandoc Markdown files when parsed.
-- Hyperlinks are not necessarily to the WWW but can be internal or interwiki hyperlinks (eg '/local/file.pdf' or '!Wikipedia').

module Main where

import Text.Pandoc (def, queryWith, readerExtensions, readMarkdown, runPure,
                     pandocExtensions, Inline(Link), Pandoc, Block(Para))
import qualified Data.Text as T (append,  head, replace, pack, unlines, Text)
import qualified Data.Text.IO as TIO (readFile, putStr)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)

import Columns (simplified)

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
  let converted = extractLinks input
  -- rewrite self-links like "#discriminator-ranking" → "/Faces#discriminator-ranking" by prefixing the original Markdown filename's absolute-ized basename;
  -- this makes frequency counts more informative, eg for deciding what sections to refactor out into standalone pages (because heavy cross-referencing
  -- *inside* a page is an important indicator of a section being 'too big', just like cross-page references are).
  let converted' = map (\u -> if T.head u /= '#' then u else "/" `T.append` (T.pack $ takeBaseName file) `T.append` u) converted

  if printfilename then TIO.putStr $ T.unlines $ Prelude.map (\url -> (T.pack file) `T.append` ":" `T.append` url) converted' else
     TIO.putStr $ T.unlines converted'

-- | Read one Text string and return its URLs (as Strings)
extractLinks :: T.Text -> [T.Text]
extractLinks txt = let parsedEither = runPure $ readMarkdown def{readerExtensions = pandocExtensions } txt
                        -- if we don't explicitly enable footnotes, Pandoc interprets the footnotes as broken links, which throws many spurious warnings to stdout
                   in case parsedEither of
                              Left _ -> []
                              Right links -> extractURLs links

-- | Read 1 Pandoc AST and return its URLs as Strings
extractURLs :: Pandoc -> [T.Text]
extractURLs = queryWith extractURL
 where
   extractURL :: Inline -> [T.Text]
   extractURL (Link _ il (u,_)) = [u`T.append`" "`T.append` (T.replace "\n" " " $ simplified $ Para il)]
   extractURL _ = []
