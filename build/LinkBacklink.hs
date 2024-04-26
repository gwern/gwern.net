{- LinkBacklink.hs: utility functions for working with the backlinks database.
Author: Gwern Branwen
Date: 2022-02-26
When:  Time-stamp: "2024-04-26 18:52:32 gwern"
License: CC-0

This is the inverse to Query: Query extracts hyperlinks within a Pandoc document which point 'out' or 'forward',
which is the usual simple unidirectional form of a hyperlink. Backlinks are the reverse: all documents which
link *to* the current one. As a global property over all documents, they cannot be computed locally or easily.
We generate them inside the `generateBacklinks.hs` executable and store them in a database.
(Most of the complexity is inside `generateBacklinks.hs` in dealing with what files to parse, what to filter out,
how to write out the HTML snippets used to provide 'backlinks' links as Gwern.net popups, etc.)

This module provides helper functions for reading & writing the backlinks database.
Because every used link necessarily has a backlink (the document in which it is used), the backlinks database
is also a convenient way to get a list of all URLs. -}

{-# LANGUAGE OverloadedStrings #-}
module LinkBacklink (getBackLinkCount, getSimilarLinkCount, Backlinks, readBacklinksDB, writeBacklinksDB, getForwardLinks,
                    getAnnotationLink, getBackLink, getLinkBibLink, getSimilarLink,
                    getAnnotationLinkCheck, getBackLinkCheck, getLinkBibLinkCheck, getSimilarLinkCheck, suggestAnchorsToSplitOut) where

import Data.List (sort)
import qualified Data.Map.Strict as M (empty, filter, fromList, fromListWith, mapWithKey, keys, map, toList, Map) -- fromListWith,
import qualified Data.Text as T (count, isInfixOf, pack, unpack, Text)
import Data.Text.IO as TIO (readFile)
import Text.Read (readMaybe)
import Network.HTTP (urlEncode)
import Text.Show.Pretty (ppShow)
import System.Directory (doesFileExist)

import LinkMetadataTypes (isPagePath)
import Utils (writeUpdatedFile)
import Config.Misc as C (sectionizeWhiteList, sectionizeMinN)

-- base URL, then fragment+links. eg. "/improvement" has links from "/note/note" etc, but those links may target anchors like "#microsoft", and those are conceptually distinct from the page as a whole - they are sub-pages. So to preserve that, we nest.
-- eg. ("/Improvements",
--                     [("/improvement#microsoft", ["/note/note", "/review/book"])
--                      , ("/improvement", ["/index"])]
--     )
type Backlinks = M.Map T.Text [(T.Text, [T.Text])]

readBacklinksDB :: IO Backlinks
readBacklinksDB = do exists <- doesFileExist "metadata/backlinks.hs"
                     bll <- if exists then TIO.readFile "metadata/backlinks.hs" else return ""
                     if bll=="" then return M.empty else
                       let bllM = readMaybe (T.unpack bll) :: Maybe [(T.Text,[(T.Text,[T.Text])])]
                       in case bllM of
                         Nothing   -> error ("Failed to parse backlinks.hs; read string: " ++ show bll)
                         Just bldb -> return $ M.mapWithKey (\_ -> M.toList . M.fromListWith (++)) $ M.fromList bldb
writeBacklinksDB :: Backlinks -> IO ()
writeBacklinksDB bldb = do let bll = M.toList bldb :: [(T.Text,[(T.Text, [T.Text])])]
                           let bll' = sort $ map (\(a,b) -> (T.unpack a, map (\(c,d) -> (T.unpack c, sort $ map T.unpack d)) b)) bll
                           writeUpdatedFile "hakyll-backlinks" "metadata/backlinks.hs" (T.pack $ ppShow bll')

-- return the raw FilePath of an x-link, and also the URL-encoded version safe to substitute into HTML:
getXLink :: String -> FilePath -> (FilePath, -- raw on-disk relative link like 'metatata/.../foo.html'
                                   FilePath) -- URL-encoded absolute like '/metadata'.../%...foo.html'
getXLink linkType p = let linkType' = "/metadata/annotation/" ++ linkType
                          linkBase = if linkType=="" then linkType' else linkType'++"/"
                          linkRaw = linkBase ++ take 246 (urlEncode p) ++ ".html"
                          -- create the doubly-URL-escaped version which decodes to the singly-escaped on-disk version (eg. `/metadata/annotation/$LINKTYPE/%252Fdocs%252Frl%252Findex.html` is how it should be in the final HTML href, but on disk it's only `metadata/annotation/$LINKTYPE/%2Fdocs%2Frl%2Findex.html`)
                          -- NOTE: we append '.html' to remove the ambiguity over whether `/metadata/annotation/%2Ffoo.pdf` is a PDF file, or a HTML snippet about a PDF file. This ensures that the MIME type is set to HTML etc. We could probably get away without it and ignore MIME types but meh.
                          link' = linkBase ++ urlEncode (concatMap (\t -> if t=='/' || t==':' || t=='=' || t=='?' || t=='%' || t=='&' || t=='#' || t=='(' || t==')' || t=='+' then urlEncode [t] else [t]) (p++".html"))
                      in (tail linkRaw,link')
getXLinkExists :: String -> FilePath -> IO (FilePath,FilePath)
getXLinkExists linkType p = do let x@(linkRaw,_) = getXLink linkType p
                               linkExists <- doesFileExist linkRaw
                               if not linkExists then return ("","")
                                 else return x

-- convert a URL to the local path of its annotation (which may not exist because it hasn't been written yet so no need to do IO to check disk), eg. 'https://www.biology.ualberta.ca/locke.hp/dougandbill.htm' → 'metadata/annotation/https%3A%2F%2Fwww2.biology.ualberta.ca%2Flocke.hp%2Fdougandbill.htm.html'
getAnnotationLink, getBackLink, getLinkBibLink, getSimilarLink :: FilePath -> (FilePath, -- on-disk
                                                                               FilePath) -- URL-escaped
getAnnotationLink = getXLink ""
getBackLink       = getXLink "backlink"
getLinkBibLink    = getXLink "link-bibliography"
getSimilarLink    = getXLink "similar"

-- IO versions which check for existence on-disk:
getAnnotationLinkCheck, getBackLinkCheck, getLinkBibLinkCheck, getSimilarLinkCheck :: FilePath -> IO (FilePath,FilePath)
getAnnotationLinkCheck = getXLinkExists ""
getBackLinkCheck       = getXLinkExists "backlink"
getLinkBibLinkCheck    = getXLinkExists "link-bibliography"
getSimilarLinkCheck    = getXLinkExists "similar"

-- avoid use of backlinks/similar-links database for convenience and just quickly grep the on-disk snippet:
getBackLinkCount :: FilePath -> IO Int
getBackLinkCount "" = return 0
getBackLinkCount p = do (file,_) <- getBackLinkCheck p
                        if null file then return 0 else do
                          fileContents <- TIO.readFile file
                          return $ T.count "[backlink context]" fileContents -- WARNING: cannot count '.backlink-not' because not necessarily 1:1 with the bl count
getSimilarLinkCount :: FilePath -> IO Int
getSimilarLinkCount "" = return 0
getSimilarLinkCount p = do (file,_) <- getSimilarLinkCheck p
                           if null file then return 0 else do
                             fileContents <- TIO.readFile file
                             return $ T.count "class=\"link-annotated backlink-not id-not\"" fileContents

-- a backlinks database implicitly defines all the forward links as well. It's not efficient compared to converting it to a 'forwardlinks database', but we can support one-off searches easily:
getForwardLinks :: Backlinks -> T.Text -> [T.Text]
getForwardLinks bdb p = M.keys $  M.filter (p `elem`) $ M.map (concatMap snd) bdb

-- look for candidates to refactor into standalone pages, by reading the backlinks database to get internal frequency use, and ranking.
suggestAnchorsToSplitOut :: IO [(Int,T.Text)]
suggestAnchorsToSplitOut = do bdb <- readBacklinksDB
                              let anchors = filter (\(a,_) -> isPagePath a && "#" `T.isInfixOf` a && a `notElem` C.sectionizeWhiteList) $ concatMap snd $ M.toList bdb
                              let anchorsPopular = reverse $ sort $ filter (\(b,_) -> b > C.sectionizeMinN) $ map (\(a,b) -> (length b, a)) anchors
                              return anchorsPopular

----------------------

-- type Forwardlinks = M.Map T.Text [T.Text]
-- convertBacklinksToForwardlinks :: Backlinks -> Forwardlinks
-- convertBacklinksToForwardlinks = M.fromListWith (++) . convertBacklinks

-- convertBacklinks :: Backlinks -> [(T.Text,[T.Text])]
-- convertBacklinks = reverseList . M.toList
--   where reverseList :: [(a,[b])] -> [(b,[a])]
--         reverseList = concatMap (\(a,bs) -> zip bs [[a]])
