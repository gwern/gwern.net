#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
-- dependencies: libghc-pandoc-dev

-- usage: 'find . -name "*.page" -or -name "*.html" | link-suggester.hs'; like 'link-extractor.hs', this extracts URLs from Markdown/HTML files
-- but with a focus on compiling `(anchor/target, URL)` pairs to be used for semi-automated link rewrites in Emacs.

module Main where

import qualified Data.Text as T (isPrefixOf, Text)
import qualified Data.Text.IO as TIO (readFile)
import Data.List(nub,sort)

import Text.Pandoc (def, queryWith, readerExtensions, readMarkdown, runPure,
                     pandocExtensions, Inline(Link), Pandoc)
import Text.Pandoc.Walk (walk)

import Control.Monad.Parallel as Par (mapM)

import Interwiki (convertInterwikiLinks, inlinesToString)

import qualified Data.Map.Strict as M

hitsMinimum :: Int
hitsMinimum = 3

-- | Map over the filenames
main :: IO ()
main = do
  fs <- fmap lines $ getContents
  pairs <- fmap concat $ Par.mapM parseURLs fs
  let db = M.filterWithKey (\k _ -> filterURLs k) $ (M.fromListWith (++) pairs) :: M.Map T.Text [T.Text]
  -- we de-duplicate *after* checking for minimum. Particularly for citations, each use counts, but we don't need each instance of 'Foo et al 2021' in the DB
  let db' = M.map (nub . sort) $ M.filter (\texts -> length texts >= hitsMinimum) db
  -- TODO: we want to filter out any anchor text which is associated with more than 1 URL: those are too ambiguous to be useful
  print db'

filterURLs :: T.Text -> Bool
filterURLs u = not ("$"`T.isPrefixOf`u || "\8383"`T.isPrefixOf`u|| "#"`T.isPrefixOf`u)

-- | Read 1 file and return its URL-pairs
parseURLs :: FilePath -> IO [(T.Text, [T.Text])]
parseURLs file = do
  input <- TIO.readFile file
  let converted = extractLinks input
  return converted

-- | Read one Text string and return its URLs (as Strings)
extractLinks :: T.Text -> [(T.Text,[T.Text])]
extractLinks txt = let parsedEither = runPure $ readMarkdown def{readerExtensions = pandocExtensions } txt
                        -- if we don't explicitly enable footnotes, Pandoc interprets the footnotes as broken links, which throws many spurious warnings to stdout
                   in case parsedEither of
                              Left _ -> []
                              Right links -> extractURLs links

-- | Read 1 Pandoc AST and return its URLs as Strings
extractURLs :: Pandoc -> [(T.Text,[T.Text])]
extractURLs = queryWith extractURL . walk convertInterwikiLinks
 where
   extractURL :: Inline -> [(T.Text,[T.Text])]
   extractURL (Link _ il (u,""))     = [(u, [inlinesToString il])]
   extractURL (Link _ il (u,target)) = [(u, [inlinesToString il]), (u, [target])]
   extractURL _ = []
