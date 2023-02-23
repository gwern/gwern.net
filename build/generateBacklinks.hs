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
import LinkBacklink (readBacklinksDB, writeBacklinksDB)
import Query (extractLinkIDsWith)
import Typography (typographyTransform)
import Utils (writeUpdatedFile, sed, anyInfixT, anyPrefixT, anySuffixT, anyInfix, anyPrefix, printRed, safeHtmlWriterOptions)

main :: IO ()
main = do
  createDirectoryIfMissing False "metadata/annotation/backlink/"
  priorBacklinksN <- fmap length $ listDirectory "metadata/annotation/backlink/"
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
        M.keys bldb ++ concatMap snd (concat $ M.elems bldb)
  forM_ filesCheck (\f -> do exist <- doesFileExist f
                             unless exist $ printRed ("Backlinks: files annotation error: file does not exist? " ++ f))

  -- if all are valid, write out:
  _ <- M.traverseWithKey (writeOutCallers md) bldb
  fs <- fmap (filter (\f -> not (anyPrefix f ["/backlink/","#",".#"])) .  map (sed "^\\.\\/" "") . lines) getContents

  let markdown = filter (".page" `isSuffixOf`) fs
  links1 <- Par.mapM (parseFileForLinks True) markdown
  let html     = filter (".html" `isSuffixOf` ) fs
  links2 <- Par.mapM (parseFileForLinks False) html

  let links3 = M.elems $ M.filter (not . null) $ M.mapWithKey (parseAnnotationForLinks . T.pack) md

  let linksdb = M.fromListWith (++) $ map (\(a,b) -> (truncateAnchors a,[(a,[b])])) $ nubOrd $ concat $ links1++links2++links3
  let bldb' = linksdb `M.union` bldb
  writeBacklinksDB bldb'

writeOutCallers :: Metadata -> T.Text -> [(T.Text, [T.Text])] -> IO ()
writeOutCallers md target callerPairs
                                  = do let f = take 274 $ "metadata/annotation/backlink/" ++ urlEncode (T.unpack target) ++ ".html"
                                       -- guess at the anchor ID in the calling page, so the cross-page popup will pop up at the calling site,
                                       -- rather than merely popping up the entire page (and who knows *where* in it the reverse citation is).
                                       -- (NOTE: This will fail if the default generated link ID has been overridden to disambiguate, unfortunately, and

                                       -- it'll just pop up the page as a whole. It would be difficult to rewrite the schema and preserve all
                                       -- variant overrides...)
                                       let preface = [Para [Strong [Str (if length (concatMap snd callerPairs) > 1 then "Backlinks" else "Backlink")], Str ":"]]
                                       let content = BulletList $ concatMap (generateCaller md target) callerPairs

                                       -- NOTE: auto-links are a good source of backlinks, catching cases where an abstract mentions something but I haven't actually hand-annotated the link yet (which would make it show up as a normal backlink). But auto-linking is extremely slow, and we don't care about the WP links which make up the bulk of auto-links. So we can do just the subset of non-WP auto-links.
                                       let pandoc = linkAutoFiltered (filter (\(_,url) -> not ("wikipedia.org/"`T.isInfixOf`url))) $
                                                    walk typographyTransform $ walk (hasAnnotation md) $ Pandoc nullMeta $ preface++[content]
                                       let html = let htmlEither = runPure $ writeHtml5String safeHtmlWriterOptions pandoc
                                                  in case htmlEither of
                                                              Left e -> error $ show target ++ show callerPairs ++ show e
                                                              Right output -> output `T.append` "\n"

                                       let backLinksHtmlFragment = html -- if C.listLength content > 60 || length callers' < 4 then html else "<div class=\"columns\">\n" `T.append` html `T.append` "\n</div>" -- FIXME: temporarily removed while the context transclusion is worked out

                                       writeUpdatedFile "backlink" f backLinksHtmlFragment

generateCaller :: Metadata -> T.Text -> (T.Text, [T.Text]) -> [[Block]]
generateCaller md target (caller, callers) =
                                       let selfIdent = case M.lookup (T.unpack target) md of
                                                             Nothing -> ""
                                                             -- if we link to a top-level essay, then we want to insert the anchor to jump to the link use.
                                                             -- if the backlink caller is actually another annotation (and so has a '.' in it), we want to add no anchor because that will break the annotation lookup:
                                                             -- it'll look at '/metadata/annotation/$FOO.html#$ID' instead2 of the actual '/metadata/annotation/$FOO.html'.
                                                             -- (eg. for Boehm et al 1993's "backlinks", there will be a 'Hierarchy in the Library' backlink which would point at 'https://gwern.net/doc/culture/2008-johnson.pdf#boehm-et-al-1993' , which has no annotation, because it's annotated as '/doc/culture/2008-johnson.pdf').
                                                             Just (_,aut,dt,_,_,_) -> generateID (T.unpack caller) aut dt
                                           callerDatesTitles = map (\url -> let u = if isPagePath url then T.takeWhile (/='#') url else url in
                                                                     case M.lookup (T.unpack u) md of
                                                                      Nothing -> if T.head u == '/' then ("",T.tail u,u) else ("",u,u)
                                                                      Just ("",_,"",_,_,_) -> if T.head u == '/' then ("",T.tail u,u) else ("",u,u)
                                                                      Just ("",_,_,_,_,_) -> if T.head u == '/' then ("",T.tail u,u) else ("",u,u)
                                                                      Just (t,_,dt,_,_,_) -> (dt,T.pack t,u))
                                                          callers
                                       -- sort backlinks in descending order (most-recent first) as a simple way to prioritize:
                                           callerTitles  = map (\(_,b,c) -> (b,c)) $ reverse $ sort callerDatesTitles
                                           callerClasses = map (\(_,u) -> if T.head u == '/' && not ("." `T.isInfixOf` u) then ["link-page"] else ["link-annotated"]) callerTitles
                                           callers' = zipWith (\a (b,c) -> (c,a,b)) callerClasses callerTitles

                                           content =  -- WARNING: critical to insert '.backlink-not' or we might get weird recursive blowup!
                                             map (\(u,c,t) -> [Para ([Link ("", "backlink-not":c, [])
                                                                     [parseRawInline nullAttr $ RawInline (Format "html") t]
                                                                     (u, "")] ++
                                                                     -- for top-level pages, we need a second link, like 'Foo (full context)', because 'Foo' will popup the scraped abstract/annotation, but it will not pop up the reverse citation context displayed right below; this leads to a UI trap: the reader might be interested in navigating to the context, but they can't! The transclusion has replaced itself, so it doesn't provide any way to navigate to the actual page, and the provided annotation link doesn't know anything about the reverse citation because it is about the entire page. So we provide a backup non-transcluding link to the actual context.
                                                                     (if isPagePath u then [Str " (", Link ("",["link-annotated-not"],[]) [Str "full context"] (if isPagePath u && selfIdent/="" && not ("#" `T.isInfixOf` u) then u`T.append`"#"`T.append`selfIdent else u,""), Str ")"] else []) ++
                                                                     [Str ":"]),
                                                               -- use transclusion to default to display inline the context of the reverse citation, akin to how it would display if the reader popped the link up as a live cross-page transclusion, but without needing to hover over each one:
                                                               BlockQuote [Para [Link ("",
                                                                                        ["backlink-not", "include-replace-container", "include-block-context"]++(if isPagePath u then ["link-annotated-not"] else ["link-annotated"]),
                                                                                        if selfIdent=="" then [] else [("target-id",selfIdent)]
                                                                                      )
                                                                                      [Str "[backlink context]"]
                                                                                      (u, "")
                                                                                ]
                                                                          ]
                                                             ]
                                                ) callers'
                                             in content

parseAnnotationForLinks :: T.Text -> MetadataItem -> [(T.Text,T.Text)]
parseAnnotationForLinks caller (_,_,_,_,_,abstract) =
                            let linkPairs = map (\(a,b) -> (localize a, localize b)) $ extractLinkIDsWith backLinksNot path False (T.pack abstract)
                                linkPairs' = filter (\(a,b) -> not (blackList a || blackList b  || truncateAnchors a == truncateAnchors b)) linkPairs
                            in
                            linkPairs'
                               where m' = localize caller
                                     path = if not (anyPrefixT m' ["/", "https://", "http://"]) then "/" `T.append` m' else m'

parseFileForLinks :: Bool -> FilePath -> IO [(T.Text,T.Text)]
parseFileForLinks mdp m = do text <- TIO.readFile m

                             let linkPairs = map (\(a,b) -> (localize a, localize b)) $ extractLinkIDsWith backLinksNot path mdp text
                             let linkPairs' = filter (\(a,b) -> not (blackList a || blackList b || truncateAnchors a == truncateAnchors b)) linkPairs
                             return linkPairs'

                               where m' = localize $ T.pack m
                                     path = if not (anyPrefixT m' ["/", "https://", "http://"]) then "/" `T.append` m' else m'

localize :: T.Text -> T.Text
localize = T.replace "https://gwern.net/" "/" . T.replace ".page" ""

-- filter out links with the 'backlink-not' class. This is for when we want to insert a link, but not have it 'count' as a backlink for the purpose of linking the reader. eg. the 'similar links' which are put into a 'See Also' in annotations - they're not really 'backlinks' even if they are semi-automatically approved as relevant.
backLinksNot :: Inline -> Bool
backLinksNot (Link (_, classes, _) _ _) = "backlink-not" `notElem` classes
backLinksNot _ = True

-- -- for URLs like 'arxiv.org/123#google' or 'doc/reinforcement-learning/2021-foo.pdf#deepmind', we want to preserve anchors; for on-site pages like '/gpt-3#prompt-programming' we want to merge all such anchor links into just callers of '/gpt-3'
truncateAnchors :: T.Text -> T.Text
truncateAnchors = T.takeWhile (/='#')

blackList :: T.Text -> Bool
blackList f
  | anyInfixT f ["/backlink/", "/link-bibliography/", "/similar/", "wikipedia.org/wiki/"] = True
  | anyPrefixT f ["$", "#", "!", "mailto:", "irc://", "\8383", "/images", "/doc/www/", "/newsletter/", "/changelog", "/mistakes", "/traffic", "/me", "/lorem",
                   -- WARNING: do not filter out 'metadata/annotation' because that leads to empty databases & infinite loops
                   "/static/404", "https://www.dropbox.com/", "https://dl.dropboxusercontent.com/"] = True
  | anySuffixT f ["/index", "/index-long"] = True
  | otherwise = False
