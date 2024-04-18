#!/usr/bin/env runghc
{- LinkPrioritize.hs: simple CLI utility for taking a list of URLs, comparing to automatic & manual
                      link annotation databases, and ranking poorly-annotated links by frequency to help prioritize
                      creation of manual link annotations.
Author: Gwern Branwen
Date: 2019-11-22
When:  Time-stamp: "2024-04-17 09:21:42 gwern"
License: CC-0
Dependencies: Gwern.net Hakyll libraries

For writing manual link annotations, a simple way to go is count un-annotated links by frequency.
Links can be extracted from Markdown documents with `link-extractor.hs` but counting is harder: not
appearing in the manual annotation database `/metadata/full.gtx` doesn't mean a link needs to be
annotated, because it could be one of the good auto-generated links in `/metadata/auto.hs` so we
can't simply `grep $URL full.gtx` for each URL & `sort --unique | sort --numeric-sort`; the list
of good auto-generated links also may change over time, because a query failed or new domains become
supported.
So instead we take a more heavyweight approach of explicitly parsing both and checking for no or
short entries.

Simple use: (because it uses the LinkMetadata module, invoking it from a different directory than
inside static/build/ is a bit tricky)

$ ~/wiki/static/build/link-extractor.hs dnb-faq.md | runghc -istatic/build/ ./static/build/link-prioritize.hs
175 : !W
4 : https://groups.google.com/g/brain-training/browse_thread/thread/3008683d4b314f6/5e833c4c0df9fb9b
4 : #jaeggi-2010
4 : #jaeggi-2008
3 : https://www.newsweek.com/2010/06/18/this-is-your-brain-aging.print.html
3 : https://www.mindsparke.com/
3 : https://www.klingberglab.se/pub/McNab2008.pdf
...

More complex use:

$ find ~/wiki/ -name "*.md" -type f -print0 | parallel --null ~/wiki/static/build/link-extractor.hs | grep -E '^http' | ./static/build/link-prioritize.hs
65 https://www.patreon.com/gwern
15 https://fis.fda.gov/sense/app/d10be6bb-494e-4cd2-82e4-0135608ddc13/sheet/45beeb74-30ab-46be-8267-5756582633b4/state/analysis
12 http://forum.evageeks.org/viewtopic.php?p=366709#366709
9 http://forum.evageeks.org/viewtopic.php?p=366717#366717
8 http://forum.evageeks.org/viewtopic.php?p=366731#366731
7 https://www.lesswrong.com/tag/inside-outside-view
7 http://predictionbook.com/users/gwern
6 https://www.reddit.com/r/TOUHOUMUSIC/search/?q=author%3Agwern&sort=new&restrict_sr=on&t=all
...
1 https://17th-angel.tumblr.com/post/11409371268/anno-a-transfer-student-opens-the-door-with-a
1 http://1000enpark.com/park/tokyo/oota/photo_heiwajima/p_main.jpg
1 http://10000yearclock.net/
1 https://0xeb.net/wp-content/uploads/2018/02/StarCraft_EUD_Emulator.pdf
1 http://0b4af6cdc2f0c5998459-c0245c5c937c5dedcca3f1764ecc9b2f.r43.cf2.rackcdn.com/12061-woot13-bangert.pdf
-}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import qualified Data.Map.Strict as M (elems, fromListWith, lookup, map, size, toList)
import Utils (replace)
import Data.List (sort)
import qualified Data.Text as T (isInfixOf, unpack)
import System.Environment (getArgs)

import LinkBacklink (readBacklinksDB)
import LinkMetadata (readLinkMetadata)
import LinkMetadataTypes (Metadata)

main :: IO ()
main = do printN:_ <- getArgs
          let printN' = if null printN then maxBound else read printN :: Int
          db <- readLinkMetadata
          when (M.size db < 1000) $ error $ "Database too small? " ++ show db
          bdb <- readBacklinksDB
          let urls = M.toList $ M.map length $ M.fromListWith (++) $ concat $ M.elems bdb
          let urls' = filter (\(url,_) -> not (isAnnotated db (T.unpack url)) && "." `T.isInfixOf` url && not ("wikipedia.org/wiki/" `T.isInfixOf` url)) urls
          let uses = reverse $ sort $ map (\(a,b) -> (b,a)) urls'
          putStrLn $ unlines $ take printN' $ map (\(n,url) -> show n ++ " " ++ T.unpack url) uses

isAnnotated :: Metadata -> String -> Bool
isAnnotated md target =
  let target' = replace "https://gwern.net/" "/" target in
    let annotated = M.lookup target' md in
      case annotated of
       -- the link has a valid annotation already defined (>100 chars, no meaningful abstract can be written in <100), so build & return;
       Just (_,_,_,_,_,_,abstrct)  -> length abstrct > 100
       Nothing -> False
