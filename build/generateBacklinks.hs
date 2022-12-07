#!/usr/bin/env runghc
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Pandoc (nullMeta,
                     runPure, writeHtml5String,
                     Pandoc(Pandoc), Block(BlockQuote, BulletList, Para), Inline(Link, RawInline, Strong, Str), Format(..), nullAttr)
import Text.Pandoc.Walk (walk)
import qualified Data.Text as T (append, isInfixOf, head, pack, replace, unpack, tail, takeWhile, Text)
import qualified Data.Text.IO as TIO (readFile)
import Data.List (isPrefixOf, isSuffixOf, sort)
import qualified Data.Map.Strict as M (lookup, keys, elems, mapWithKey, traverseWithKey, fromListWith, union, filter)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import Network.HTTP (urlEncode)
import Data.Containers.ListUtils (nubOrd)
import Control.Monad (forM_, unless)

import Control.Monad.Parallel as Par (mapM)

-- import Columns as C (listLength)
import LinkAuto (linkAutoFiltered)
import LinkID (generateID)
import LinkMetadata (hasAnnotation, isPagePath, readLinkMetadata, parseRawInline)
import LinkMetadataTypes (Metadata, MetadataItem)
import LinkBacklink (readBacklinksDB, writeBacklinksDB, getAnnotationLink)
import Query (extractLinksWith)
import Typography (typographyTransform)
import Utils (writeUpdatedFile, sed, anyInfixT, anyPrefixT, anySuffixT, anyInfix, anyPrefix, printRed, replace, safeHtmlWriterOptions)

main :: IO ()
main = do
  createDirectoryIfMissing False "metadata/annotations/backlinks/"
  priorBacklinksN <- fmap length $ listDirectory "metadata/annotations/backlinks/"
  -- for uninteresting reasons probably due to a bad architecture, when the existing set of backlinks is deleted for a clean start, apparently you have to run generateBacklinks.hs twice...? So if we appear to be at a clean start, we run twice:
  if priorBacklinksN > 0 then main' else main' >> main'

main' :: IO ()
main' = do
  bldb <- readBacklinksDB
  md <- readLinkMetadata
  -- check that all backlink targets/callers are valid:
  let dotPageFy f = if '.' `elem` f then f else f++".page" -- all files have at least 1 period in them (for file extensions); a file missing periods must be a `.page` Markdown file, with the exception of tag pages which are auto-generated
  let filesCheck = map (dotPageFy . takeWhile (/='#') . tail) $ nubOrd $
        filter (\f -> not (anyInfix f ["/index","/link-bibliography/"])) $
        filter ("/"`isPrefixOf`) $ map T.unpack $
        M.keys bldb ++ concat (M.elems bldb)
  forM_ filesCheck (\f -> do exist <- doesFileExist f
                             unless exist $ printRed ("Backlinks: files annotation error: file does not exist? " ++ f))

  -- if all are valid, write out:
  _ <- M.traverseWithKey (writeOutCallers md) bldb
  fs <- fmap (filter (\f -> not (anyPrefix f ["/backlinks/","#",".#"])) .  map (sed "^\\.\\/" "") . lines) getContents

  let markdown = filter (".page" `isSuffixOf`) fs
  links1 <- Par.mapM (parseFileForLinks True) markdown
  let html     = filter (".html" `isSuffixOf` ) fs
  links2 <- Par.mapM (parseFileForLinks False) html

  let links3 = M.elems $ M.filter (not . null) $ M.mapWithKey (parseAnnotationForLinks . T.pack) md

  let linksdb = M.fromListWith (++) $ map (\(a,b) -> (a,[b])) $ nubOrd $ concat $ links1++links2++links3
  let bldb' = linksdb `M.union` bldb
  writeBacklinksDB bldb'

writeOutCallers :: Metadata -> T.Text -> [T.Text] -> IO ()
writeOutCallers md target callers = do let f = take 274 $ "metadata/annotations/backlinks/" ++ urlEncode (T.unpack target) ++ ".html"
                                       -- guess at the anchor ID in the calling page, so the cross-page popup will pop up at the calling site,
                                       -- rather than merely popping up the entire page (and who knows *where* in it the reverse citation is).
                                       -- (NOTE: This will fail if the default generated link ID has been overridden to disambiguate, unfortunately, and
                                       -- it'll just pop up the page as a whole. It would be difficult to rewrite the schema and preserve all
                                       -- variant overrides...)
                                       let selfIdent = case M.lookup (T.unpack target) md of
                                                             Nothing -> ""
                                                             -- if we link to a top-level essay, then we want to insert the anchor to jump to the link use.
                                                             -- if the backlink caller is actually another annotation (and so has a '.' in it), we want to add no anchor because that will break the annotation lookup:
                                                             -- it'll look at '/metadata/annotations/$FOO.html#$ID' instead2 of the actual '/metadata/annotations/$FOO.html'.
                                                             -- (eg. for Boehm et al 1993's "backlinks", there will be a 'Hierarchy in the Library' backlink which would point at 'https://www.gwern.net/docs/culture/2008-johnson.pdf#boehm-et-al-1993' , which has no annotation, because it's annotated as '/docs/culture/2008-johnson.pdf').
                                                             Just (_,aut,dt,_,_,_) -> let i = generateID (T.unpack target) aut dt in
                                                                                        if i=="" then "" else "#" `T.append` i
                                       let callerDatesTitles = map (\u -> case M.lookup (T.unpack u) md of
                                                                      Nothing -> if T.head u == '/' then ("",T.tail u,u) else ("",u,u)
                                                                      Just ("",_,"",_,_,_) -> if T.head u == '/' then ("",T.tail u,u) else ("",u,u)
                                                                      Just ("",_,_,_,_,_) -> if T.head u == '/' then ("",T.tail u,u) else ("",u,u)
                                                                      Just (t,_,dt,_,_,_) -> (dt,T.pack t,u))
                                                          callers
                                       -- sort backlinks in descending order (most-recent first) as a simple way to prioritize:
                                       let callerTitles  = map (\(_,b,c) -> (b,c)) $ reverse $ sort callerDatesTitles
                                       let callerClasses = map (\(_,u) -> if T.head u == '/' && not ("." `T.isInfixOf` u) then ["link-page"] else ["link-annotated"]) callerTitles
                                       let callers' = zipWith (\a (b,c) -> (c,a,b)) callerClasses callerTitles

                                       let preface = [Para [Strong [Str (if length callers' > 1 then "Backlinks" else "Backlink")], Str ":"]]
                                       let content = BulletList $ -- WARNING: critical to insert '.backlink-not' or we might get weird recursive blowup!
                                            map (\(u,c,t) ->
                                                   -- do we transclude the context on page load? If it's an annotation, annotations are very cheap (~4kb average), so we can.
                                                   -- we can't if it's an entire page like /GPT-3, however!
                                                   let includeStrict = if isPagePath u then [] else ["include-strict"] in
                                                                   [Para ([Link ("", "id-not":"backlink-not":c, [])
                                                                          [parseRawInline nullAttr $ RawInline (Format "html") t]
                                                                          (u, "")] ++
                                                                          -- for top-level pages, we need a second link, like 'Foo (context)', because 'Foo' will popup the scraped abstract/annotation, but it will not pop up the reverse citation context displayed right below; this leads to a UI trap: the reader might be interested in navigating to the context, but they can't! The transclusion has replaced itself, so it doesn't provide any way to navigate to the actual page, and the provided annotation link doesn't know anything about the reverse citation because it is about the entire page. So we provide a backup non-transcluding link to the actual context.
                                                                          (if isPagePath u then [Str " (", Link nullAttr [Str "context"] (if isPagePath u then u`T.append`selfIdent else u,""), Str ")"] else []) ++
                                                                          [Str ":"]),
                                                               -- use transclusion to default to display inline the context of the reverse citation, akin to how it would display if the reader popped the link up as a live cross-page transclusion, but without needing to hover over each one:
                                                               BlockQuote [Para [Link ("",(["id-not", "backlink-not", "include-replace-container", "include-block-context"]++includeStrict),[]) -- TODO: need '.include-strict' for better reader experience?
                                                                                      [Str "[backlink context]"]
                                                                                      ((if isPagePath u then u else T.pack (snd $ getAnnotationLink $ T.unpack u)) `T.append` selfIdent, "")
                                                                                ]
                                                                          ]
                                                             ]
                                                ) callers'

                                       -- NOTE: auto-links are a good source of backlinks, catching cases where an abstract mentions something but I haven't actually hand-annotated the link yet (which would make it show up as a normal backlink). But auto-linking is extremely slow, and we don't care about the WP links which make up the bulk of auto-links. So we can do just the subset of non-WP auto-links.
                                       let pandoc = linkAutoFiltered (filter (\(_,url) -> not ("wikipedia.org/"`T.isInfixOf`url))) $
                                                    walk typographyTransform $ walk (hasAnnotation md) $ Pandoc nullMeta $ preface++[content]
                                       let html = let htmlEither = runPure $ writeHtml5String safeHtmlWriterOptions pandoc
                                                  in case htmlEither of
                                                              Left e -> error $ show target ++ show callers ++ show e
                                                              Right output -> output

                                       let backLinksHtmlFragment = html -- if C.listLength content > 60 || length callers' < 4 then html else "<div class=\"columns\">\n" `T.append` html `T.append` "\n</div>" -- FIXME: temporarily removed while the context transclusion is worked out

                                       writeUpdatedFile "backlink" f backLinksHtmlFragment

parseAnnotationForLinks :: T.Text -> MetadataItem -> [(T.Text,T.Text)]
parseAnnotationForLinks caller (_,_,_,_,_,abstract) =
                            let links = map truncateAnchorsForPages $ filter blackList $ filter (\l -> let l' = T.head l in l' == '/' || l' == 'h') $ -- filter out non-URLs
                                         extractLinksWith backLinksNot False (T.pack abstract)

                                in if not (blackList caller) then [] else
                                     let called =  filter (\u -> truncateAnchors u /= truncateAnchors caller) -- avoid self-links
                                           links
                                     in zip called (repeat caller)

parseFileForLinks :: Bool -> FilePath -> IO [(T.Text,T.Text)]
parseFileForLinks mdp m = do text <- TIO.readFile m

                             let links = map truncateAnchorsForPages $ filter blackList $ filter (\l -> let l' = T.head l in l' == '/' || l' == 'h') $ -- filter out non-URLs
                                          extractLinksWith backLinksNot mdp text

                             let caller = T.pack $ (\u -> if head u /= '/' && take 4 u /= "http" then "/"++u else u) $ replace "https://www.gwern.net/" "/" $ replace ".page" "" m
                             if not (blackList caller) then return [] else
                              do
                                let called = filter (/= caller) (map (T.replace "https://www.gwern.net/" "/") links)
                                return $ zip called (repeat caller)

-- filter out links with the 'backlink-not' class. This is for when we want to insert a link, but not have it 'count' as a backlink for the purpose of linking the reader. eg. the 'similar links' which are put into a 'See Also' in annotations - they're not really 'backlinks' even if they are semi-automatically approved as relevant.
backLinksNot :: Inline -> Bool
backLinksNot (Link (_, classes, _) _ _) = "backlink-not" `notElem` classes
backLinksNot _ = True

-- for URLs like 'arxiv.org/123#google' or 'docs/reinforcement-learning/2021-foo.pdf#deepmind', we want to preserve anchors; for on-site pages like '/GPT-3#prompt-programming' we want to merge all such anchor links into just callers of '/GPT-3'
truncateAnchors, truncateAnchorsForPages :: T.Text -> T.Text
truncateAnchorsForPages str = if "." `T.isInfixOf` str then str else T.takeWhile (/='#') str
truncateAnchors = T.takeWhile (/='#')

blackList :: T.Text -> Bool
blackList f
  | anyInfixT f ["/backlinks/", "/link-bibliography/", "/similars/", "wikipedia.org/wiki/"] = False
  | anyPrefixT f ["/images", "/docs/www/", "/newsletter/", "/Changelog", "/Mistakes", "/Traffic", "/Links", "/Lorem",
                   -- WARNING: do not filter out 'metadata/annotations' because that leads to empty databases & infinite loops
                   "/static/404", "https://www.dropbox.com/", "https://dl.dropboxusercontent.com/"] = False
  | anySuffixT f ["/index", "/index-long"] = False
  | otherwise = True
