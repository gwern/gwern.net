#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
-- dependencies: libghc-pandoc-dev, Unique

-- usage: 'find . -name "*.page" -or -name "*.html" | link-suggester.hs'; like 'link-extractor.hs', this extracts URLs from Markdown/HTML files
-- but with a focus on compiling `(anchor/target, URL)` pairs to be used for semi-automated link rewrites in Emacs.

module Main where

import Data.List (nub, sort, sortBy)
import qualified Data.Map.Strict as M (elems, filter, filterWithKey, fromListWith, toList, map, Map)
import qualified Data.Set as S (fromList, member, Set)
import qualified Data.Text as T (append, length, lines, intercalate, pack, isInfixOf, isPrefixOf, Text)
import qualified Data.Text.IO as TIO (readFile, writeFile)
import System.Environment (getArgs)

import Text.Pandoc (def, queryWith, readerExtensions, readMarkdown, runPure,
                     pandocExtensions, Inline(Link), Pandoc)
import Text.Pandoc.Walk (walk)

import Control.Monad.Parallel as Par (mapM)

import Data.List.Unique as U (repeated)

import Interwiki (convertInterwikiLinks, inlinesToString)

hitsMinimum :: Int
hitsMinimum = 2

-- | Map over the filenames
main :: IO ()
main = do
  (output:_) <- getArgs
  fs         <- fmap lines getContents
  pairs <- fmap concat $ Par.mapM parseURLs fs

  -- blacklist bad URLs, which don't count
  let db = M.filterWithKey (\k _ -> filterURLs k) $ M.fromListWith (++) pairs :: M.Map T.Text [T.Text]

  -- we de-duplicate *after* checking for minimum. Particularly for citations, each use counts, but we don't need each instance of 'Foo et al 2021' in the DB (`/usr/share/dict/words`), so we unique the list of anchors
  let db' = M.map (nub . sort) $ M.filter (\texts -> length texts >= hitsMinimum) db

  -- We want to filter out any anchor text which is associated with more than 1 URL (those are too ambiguous to be useful), any text which is in the system dictionary, and anything in the blacklist patterns or list.
  let anchorTextsDupes = U.repeated $ concat $ M.elems db'
  dict <- dictionary
  let db'' = M.filter (not . null) $ M.map (filter (\t -> not (t `elem` anchorTextsDupes || filterAnchors dict t))) db'

  -- swap [(URL,[Anchor])] to [(Anchor,URL)] (which we need for doing 'search-replace before after'), by expanding/flattening the list:
  let reversedDB = concatMap (\(url,ts) -> zip ts (repeat url)) $ M.toList db''
  -- sort by length of anchor text in descending length: longer matches should come first, for greater specificity in doing rewrites.
  let reversedDBSorted = sortBy (\(t1,_) (t2,_) -> if T.length t1 > T.length t2 then LT else GT) reversedDB

  TIO.writeFile output (haskellListToElispList reversedDBSorted)

-- format the pairs in Elisp `(setq rewrites '((foo bar)) )` style so it can be read in & executed directly by Emacs's `load-file`.
haskellListToElispList :: [(T.Text, T.Text)] -> T.Text
haskellListToElispList xs = "(setq rewrites '("
                            `T.append`
                            T.intercalate "\n                 " (map (T.pack . (\(t,u) -> "(" ++ show t ++ " " ++ show u ++ ")")) xs)
                            `T.append`
                            "\n                 )\n      )\n"

filterURLs :: T.Text -> Bool
filterURLs    u = not ("$"`T.isPrefixOf`u || "\8383"`T.isPrefixOf`u || "#"`T.isPrefixOf`u)
filterAnchors :: S.Set T.Text -> T.Text -> Bool
filterAnchors d t = not $
                    ("$"`T.isInfixOf`t || "%"`T.isInfixOf`t || "["`T.isInfixOf`t || "]"`T.isInfixOf`t) ||
                    S.member t d
                    -- || (any (t==) [] )

dictionary :: IO (S.Set T.Text)
dictionary = fmap (S.fromList . T.lines) $ TIO.readFile "/usr/share/dict/words"

-- | Read 1 file and return its URL-pairs
parseURLs :: FilePath -> IO [(T.Text, [T.Text])]
parseURLs file = do
  input <- TIO.readFile file
  let converted = extractLinks input
  return converted

-- | Read one Text string and pass to 'extractURLs'
extractLinks :: T.Text -> [(T.Text,[T.Text])]
extractLinks txt = let parsedEither = runPure $ readMarkdown def{readerExtensions = pandocExtensions } txt
                        -- if we don't explicitly enable footnotes, Pandoc interprets the footnotes as broken links, which throws many spurious warnings to stdout
                   in case parsedEither of
                              Left _ -> []
                              Right links -> extractURLs links

-- | Read 1 Pandoc AST and return its URLs/anchor-text pairs;
-- if a URL has both a title and an anchor text, we return 2 pairs because both might be valid (eg '[GPT-3](https://arxiv.org/foo "Language Models are Few-Shot Learners")' - we would like to do link-suggestions on both the short noun 'GPT-3' and the paper title, but we can't if we arbitrarily return one but not the other).
extractURLs :: Pandoc -> [(T.Text,[T.Text])]
extractURLs = queryWith extractURL . walk convertInterwikiLinks
 where
   extractURL :: Inline -> [(T.Text,[T.Text])]
   extractURL (Link _ il (u,""))     = [(u, [inlinesToString il])]
   extractURL (Link _ il (u,target)) = [(u, [inlinesToString il]), (u, [target])]
   extractURL _ = []
