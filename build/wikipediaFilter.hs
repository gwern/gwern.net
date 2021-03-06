#!/usr/bin/env runhaskell
{- wikipediaFilter.hs: simple CLI utility for taking in HTML and deleting specified parts by their class/ID.
Author: Gwern Branwen
Date: 2021-03-05
When:  Time-stamp: "2021-03-05 21:42:29 gwern"
License: CC-0
Dependencies: dom-selector
-}

{-# LANGUAGE OverloadedStrings #-} -- , QuasiQuotes

module Main (main) where

import Text.XML.Cursor (fromDocument, node)
import Text.HTML.DOM (parseLT)
import qualified Data.Text.Lazy.IO as TI (putStrLn, getContents)

import Text.XML.Scraping (innerHtml, removeQueries)
-- import Text.XML.Selector.TH (queryT, jq)

-- import Network.HTTP.Conduit (simpleHttp) -- simpleHttp "https://en.wikipedia.org/api/rest_v1/page/mobile-html/Dog"

main :: IO ()
main = do
   root <- fmap (fromDocument . parseLT) TI.getContents
   -- let cs =  queryT [jq| section |] root
   let first = node root
   let cleaned = removeQueries [ -- visual clutter:
         ".mw-ref", ".pcs-edit-section-link-container", ".ambox",
         -- I link to the article I intend:
         ".hatnote",
         -- sidebars aren't even displayed in mobile mode! but often contain 100+ links, contributing to wild exponential explosions in the crawl
         ".sidebar"] [first]
   TI.putStrLn $ innerHtml cleaned
