#!/usr/bin/env runghc
{-# LANGUAGE OverloadedStrings #-}
-- dependencies: libghc-pandoc-dev, Unique, monad-parallel, Interwiki.hs

-- usage: 'find . -name "*.md" -or -name "*.html" | link-suggester.hs'; like 'link-extractor.hs'
--
-- This extracts URLs from Markdown/HTML files, but with a focus on compiling `(anchor/target, URL)`
-- pairs to be used for semi-automated link rewrites in Emacs.

module Main where

import Data.List (sort, sortBy)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map.Strict as M (difference, elems, filter, filterWithKey, fromListWith, toList, map, union, Map)
import qualified Data.Set as S (fromList, member, Set)
import qualified Data.Text as T (append, dropWhile, dropWhileEnd, head, length, lines, intercalate, pack, unpack, toLower, Text, replace)
import Data.Char (isSpace, isPunctuation)
import qualified Data.Text.IO as TIO (readFile)
import Network.URI (isURI)
import System.Environment (getArgs)

import Text.Show.Pretty (ppShow)

import Control.Monad.Parallel as Par (mapM) -- 'monad-parallel' package

import Query (extractURLsAndAnchorTooltips, parseMarkdownOrHTML)
import Utils (writeUpdatedFile, printGreen, repeated)
import qualified Config.LinkSuggester as C

-- | Map over the filenames
main :: IO ()
main = do
  (outputTarget:_) <- getArgs
  fs         <- fmap lines getContents
  printGreen "Parsing all files for links…"
  pairs <- fmap concat $ Par.mapM parseURLs fs
  printGreen "Parsed all files for links."

  -- blacklist bad URLs, which don't count
  let db = M.filterWithKey (\k _ -> (k /= "") && (T.head k == '/' || isURI (T.unpack k)) && not (C.filterURLs k)) $ M.fromListWith (++) pairs :: M.Map T.Text [T.Text]

  -- we de-duplicate *after* checking for minimum. Particularly for citations, each use counts, but we don't need each instance of 'Foo et al 2021' in the DB (`/usr/share/dict/words`), so we unique the list of anchors
  let dbMinimumLess = M.union C.whiteListDB $ M.map (nubOrd . sort . cleanAnchors) $ M.filter (\texts -> length texts >= C.hitsMinimum) db
  let dbFailedMinimum = ("Did not pass hitsMinimum filter", db `M.difference` dbMinimumLess) -- NOTE: difference is not symmetrical: "Return elements of the first map not existing in the second map." so need to do OLD `M.difference` NEW

  -- We want to filter out any anchor text which is associated with more than 1 URL (those are too ambiguous to be useful), any text which is in the system dictionary, and anything in the blacklist patterns or list.
  let anchorTextsDupes = Utils.repeated $ concat $ M.elems dbMinimumLess
  let dbTextDupeLess = M.union C.whiteListDB $ M.map (filter (`notElem` anchorTextsDupes)) dbMinimumLess
  let dbFailedDupe = ("Did not pass anchorTextDupes filter"::T.Text,                   dbMinimumLess `M.difference` dbTextDupeLess)

  dict <- dictionarySystem
  let dbDictLess = M.union C.whiteListDB $ M.map (filter (\t -> not (S.member (T.toLower t) dict))) dbTextDupeLess
  let dbFailedDict = ("Did not pass system dictionary filter"::T.Text,                          dbTextDupeLess `M.difference` dbDictLess)

  let dbAnchorLess = M.union C.whiteListDB $ M.map (filter (not . C.filterAnchors)) dbDictLess
  let dbFailedAnchor = ("Did not pass anchor filter"::T.Text,                 dbDictLess `M.difference` dbAnchorLess)

  let dbClean = M.union C.whiteListDB $ M.filter (not . null) dbAnchorLess

  -- swap [(URL,[Anchor])] to [(Anchor,URL)] (which we need for doing 'search-replace before after'), by expanding/flattening the list:
  let reversedDB = concatMap (\(url,ts) -> zip ts (repeat url)) $ M.toList dbClean
  -- sort by length of anchor text in descending length: longer matches should come first, for greater specificity in doing rewrites.
  let reversedDBSorted = sortBy (\(t1,_) (t2,_) -> if T.length t1 > T.length t2 then LT else if T.length t1 == T.length t2 then if t1 > t2 then GT else LT else GT) reversedDB
  let elispDB = haskellListToElispList reversedDBSorted

  writeUpdatedFile "linkSuggestions.el.tmp" outputTarget elispDB
  writeUpdatedFile "linkSuggestions-deleted.hs.tmp" "metadata/linkSuggestions-deleted.hs" $ T.pack (ppShow [dbFailedAnchor, dbFailedDupe, dbFailedDict, dbFailedMinimum])
  printGreen "Wrote out link suggestion database."

-- format the pairs in Elisp `(setq rewrites '((foo bar)) )` style so it can be read in & executed directly by Emacs's `load-file`.
haskellListToElispList :: [(T.Text, T.Text)] -> T.Text
haskellListToElispList xs = "(setq markdown-rewrites '(\n                 "
                            `T.append`
                            T.intercalate "\n                 " (map (T.pack . (\(t,u) -> "(" ++ show t ++ " " ++ show u ++ ")")) xs)
                            `T.append`
                            "\n                 )\n      )\n"

-- clean/rewrite anchors to deal with whitespace/punctuation in links, eg. "RahXephon:" or "theanine," or "Bitcoin ".
cleanAnchors :: [T.Text] -> [T.Text]
cleanAnchors = map cleanAnchor
   where cleanAnchor :: T.Text -> T.Text
         cleanAnchor = T.dropWhileEnd trimText . T.dropWhile trimText
         trimText c = isSpace c || isPunctuation c || c == '=' || c == '+'

-- We do case-insensitive matching to pick up dictionary words at beginning of sentence. We only look up entire phrases; if we split into words by whitespace and look up each word, then even when all words are in the dictionary, too many reasonable anchor texts would be filtered out.
dictionarySystem :: IO (S.Set T.Text)
dictionarySystem = fmap (S.fromList . T.lines . T.toLower) $ TIO.readFile "/usr/share/dict/words"

-- | Read 1 file and return its URL-pairs
parseURLs :: FilePath -> IO [(T.Text, [T.Text])]
parseURLs file = do
  input <- TIO.readFile file
  let converted = extractURLsAndAnchorTooltips $ parseMarkdownOrHTML True input
  return $ map (\(url, text) -> (T.replace "https://gwern.net/" "/" url, text)) converted
