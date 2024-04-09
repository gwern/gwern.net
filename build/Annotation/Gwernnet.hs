module Annotation.Gwernnet where

import Control.Monad (when)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.ByteString.Lazy.UTF8 as U (toString) -- TODO: why doesn't using U.toString fix the Unicode problems?
-- import Text.HTML.TagSoup (isTagOpenName, parseTags, renderOptions, renderTags, renderTagsOptions, Tag(TagClose, TagOpen, TagText))
import qualified Data.Text as T (pack, unpack)
import System.Exit (ExitCode(ExitFailure))
-- import Text.Pandoc (runPure, nullMeta, writeHtml5String, Pandoc(..))
import Data.FileStore.Utils (runShellCommand)
import System.FilePath (takeDirectory)
import Text.Regex.TDFA ((=~))

import Annotation.PDF (pdf)
import Image (invertImage)
import LinkMetadataTypes (MetadataItem, Failure(..), Path)
import MetadataFormat (checkURL, cleanAbstractsHTML, dateRegex, sectionAnonymousRegex, footnoteRegex, cleanAuthors)
import Utils (anyInfix, anySuffix, anyPrefix, printGreen, printRed, replace, replaceMany, sed, split, trim) -- safeHtmlWriterOptions, sedMany
import Tags (listTagDirectoriesAll, abbreviateTag)
import LinkAuto (linkAutoHtml5String)
-- import Query (truncateTOCHTML)

import Text.HTML.TagSoup (renderTags, parseTags, isTagOpenName, Tag(TagClose,TagOpen, TagText))

-- | Extracts the content of the first div with the specified id and class.
findDivContent :: String -> Maybe String
findDivContent html = renderTags <$> extractDivContents 0 (parseTags html)
  where
    targetOpenTag = TagOpen "div" [("id", "TOC"), ("class", "TOC")]

    -- Recursively search for the target div, tracking nesting level to find the matching closing tag.
    extractDivContents :: Int -> [Tag String] -> Maybe [Tag String]
    extractDivContents _ [] = Nothing  -- End of tags, target not found.
    extractDivContents level (tag:tags)
      | tag == targetOpenTag && level == 0 = Just $ takeContent 1 tags
      | otherwise = extractDivContents level tags

    -- Take tags until the matching closing tag for the target div is found, considering nested divs.
    takeContent :: Int -> [Tag String] -> [Tag String]
    takeContent 0 _ = []  -- Closed target div.
    takeContent level (tag:tags)
      | tag == TagOpen "div" [] = tag : takeContent (level + 1) tags  -- Found a nested open div.
      | tag == TagClose "div" && level == 1 = []  -- Found the closing tag of target div.
      | tag == TagClose "div" = tag : takeContent (level - 1) tags  -- Found a closing tag of a nested div.
      | otherwise = tag : takeContent level tags  -- Continue with the next tag.
    takeContent x y = error $ "Gwernnet.hs: findDivContent: takeContent: pattern-amtch failed, which should be impossible: " ++ show x ++ " : " ++ show y

gwern :: Path -> IO (Either Failure (Path, MetadataItem))
gwern "/doc/index" = gwerntoplevelDocAbstract -- special-case ToC generation of all subdirectories for a one-stop shop
gwern "doc/index"  = gwerntoplevelDocAbstract
gwern p | p == "/" || p == "" = return (Left Permanent)
        | ".pdf" `isInfixOf` p = pdf p
        | anyInfix p [".avi", ".bmp", ".conf", ".css", ".csv", ".doc", ".docx", ".ebt", ".epub", ".gif", ".GIF", ".hi", ".hs", ".htm", ".html", ".ico", ".idx", ".img", ".jpeg", ".jpg", ".JPG", ".js", ".json", ".jsonl", ".maff", ".mdb", ".mht", ".mp3", ".mp4", ".mkv", ".o", ".ods", ".opml", ".pack", ".md", ".patch", ".php", ".png", ".R", ".rm", ".sh", ".svg", ".swf", ".tar", ".ttf", ".txt", ".wav", ".webm", ".xcf", ".xls", ".xlsx", ".xml", ".xz", ".zip"] = return (Left Permanent) -- skip potentially very large archives
        | anyPrefix p ["metadata", "/metadata"] ||
          anySuffix p ["#external-links", "#see-also", "#see-also", "#see-alsos", "#see-also-1", "#see-also-2", "#footnotes", "#links", "#misc", "#miscellaneous", "#appendix", "#appendices", "#conclusion", "#conclusion-1", "#conclusion-2", "#media", "#writings", "#filmtv", "#music", "#books"] ||
          anyInfix p ["index.html", "/index#"] ||
          ("/index#" `isInfixOf` p && "-section" `isSuffixOf` p)  = return (Left Permanent)
        | "/newsletter/" `isPrefixOf` p && '#' `elem` p = return (Left Permanent) -- newsletter sections like '/newsletter/2022/01#fiction' do not have abstracts
        | p =~ sectionAnonymousRegex = return (Left Permanent) -- unnamed sections are unstable, and also will never have abstracts because they would've gotten a name as part of writing it.
        | p =~ footnoteRegex= return (Left Permanent) -- shortcut optimization: footnotes will never have abstracts (right? that would just be crazy hahaha ・・；)
        | otherwise =
            do let p' = sed "^/" "" $ replace "https://gwern.net/" "" p
               -- let indexP = "doc/" `isPrefixOf` p' && "/index" `isInfixOf` p'
               printGreen p'
               checkURL p
               (status,_,bs) <- runShellCommand "./" Nothing "curl" ["--silent", "https://gwern.net/"++p', "--user-agent", "gwern+gwernscraping@gwern.net"] -- we strip `--location` because we do *not* want to follow redirects. Redirects creating duplicate annotations is a problem.
               case status of
                 ExitFailure _ -> printRed ("Gwern.net download failed: " ++ p) >> return (Left Permanent)
                 _ -> do
                        let b = U.toString bs
                        let f = parseTags b
                        let metas = filter (isTagOpenName "meta") f
                        let title = cleanAbstractsHTML $ concatMap (\(TagOpen _ (t:u)) -> if snd t == "title" then snd $ head u else "") metas
                        let date = let dateTmp = concatMap (\(TagOpen _ (v:w)) -> if snd v == "dc.date.issued" then snd $ head w else "") metas
                                       in if dateTmp=="N/A" || dateTmp=="2009-01-01" || not (dateTmp =~ dateRegex) then "" else dateTmp
                        let dateModified = let dateTmp = concatMap (\(TagOpen _ (v:w)) -> if snd v == "dcterms.modified" then snd $ head w else "") metas
                                       in if dateTmp=="N/A" || dateTmp=="2009-01-01" || not (dateTmp =~ dateRegex) then date else dateTmp
                        let description = concatMap (\(TagOpen _ (cc:dd)) -> if snd cc == "description" then snd $ head dd else "") metas
                        let keywordTags = if "#" `isInfixOf` p then [] else
                                            concatMap (\(TagOpen _ (x:y)) -> if snd x == "keywords" then Utils.split ", " $ snd $ head y else []) metas
                        let author = cleanAuthors $ concatMap (\(TagOpen _ (aa:bb)) -> if snd aa == "author" then snd $ head bb else "") metas
                        let author' = if author == "Gwern Branwen" then "Gwern" else author
                        let thumbnail = if not (any filterThumbnail metas) then "" else
                                          (\(TagOpen _ [_, ("content", thumb)]) -> thumb) $ head $ filter filterThumbnail metas
                        let thumbnail' = if "https://gwern.net/static/img/logo/logo-whitebg-large-border.png" `isPrefixOf` thumbnail then "" else replace "https://gwern.net/" "" thumbnail
                        let thumbnailText = if not (any filterThumbnailText metas) then "" else -- WARNING: if there is no thumbnail-text, then bad things will happen downstream as the thumbnail gets rendered as solely an <img> rather than a <figure><img>. We will assume the author will always have a thumbnail-text set.
                                          (\(TagOpen _ [_, ("content", thumbt)]) -> thumbt) $ head $ filter filterThumbnailText metas
                        when (null thumbnailText) $ printRed ("Warning: no thumbnail-text alt text defined for URL " ++ p)
                        let thumbnailCSS = words $ (\(TagOpen _ [_, ("content", css)]) -> css) $ head $ filter filterThumbnailCSS metas

                        (color,h,w) <- invertImage thumbnail'
                        let color' = if "invert" `elem` thumbnailCSS || "invert-not" `elem` thumbnailCSS then ""
                                         else if color then "invert-auto" else "invert-not"
                        let thumbnailFigure = if thumbnail'=="" then "" else
                              let imgClass =  "class=\"float-right page-thumbnail " ++ color' ++ " " ++ unwords thumbnailCSS ++ "\""
                              in ("<figure><img " ++ imgClass ++ " height=\"" ++ h ++ "\" width=\"" ++ w ++ "\" src=\"/" ++ thumbnail' ++ "\" title=\"" ++ thumbnailText ++ "\" alt=\"\" /></figure>")

                        let doi = [] -- I explored the idea but DOIs are too expensive & ultimately do little useful
                        -- let footnotesP = "<section class=\"footnotes\"" `isInfixOf` b

                        let toc = findDivContent b
                        let toc' = case toc of
                              Nothing -> ""
                              Just t -> let t' = trim t in if t' == "" then "" else "<div class=\"columns TOC\">" ++ t ++ "</div>"

                        let (sectTitle,gabstract) = gwernAbstract ("/index" `isSuffixOf` p' || "newsletter/" `isPrefixOf` p') p' description toc' f

                        let title' = if null sectTitle then title else title ++ " § " ++ sectTitle

                        let combinedAnnotation = (if "</figure>" `isInfixOf` gabstract || "<img>" `isInfixOf` gabstract || null gabstract then "" else thumbnailFigure) ++ -- some pages like /question have an image inside the abstract; preserve that if it's there
                                                 gabstract

                        if gabstract == "404 Not Found Error: no page by this name!" || title' == "404 Not Found" || (null keywordTags && null gabstract) then
                          return (Left Permanent) -- NOTE: special-case: if a new essay or a tag hasn't been uploaded yet, make a stub entry; the stub entry will eventually be updated via a `updateGwernEntries` scrape. (A Temporary error has the drawback that it throws changeTag.hs into an infinite loop as it keeps trying to fix the temporary error.)
                          else return $ Right (p, (title', author', date, dateModified, doi, keywordTags, combinedAnnotation))
        where
          filterThumbnail (TagOpen "meta" [("property", "og:image"), _]) = True
          filterThumbnail _ = False
          filterThumbnailText (TagOpen "meta" [("property", "og:image:alt"), _]) = True
          filterThumbnailText _ = False
          filterThumbnailCSS (TagOpen "meta" [("property", "gwern:thumbnail:css-classes"), _]) = True
          filterThumbnailCSS _ = False

-- skip the complex gwernAbstract logic: /doc/index is special because it has only subdirectories, is not tagged, and is the entry point. We just generate the ToC directly from a recursive tree of subdirectories with 'index.md' entries:
gwerntoplevelDocAbstract :: IO (Either Failure (Path, MetadataItem))
gwerntoplevelDocAbstract = do allDirs <- listTagDirectoriesAll ["doc/"]
                              let allDirLinks = unlines $ map (\d -> "<li><a class='link-page link-tag directory-indexes-downwards link-annotated' data-link-icon='arrow-down' data-link-icon-type='svg' rel='tag' href=\"" ++ d ++ "\">" ++ (T.unpack $ abbreviateTag (T.pack (replace "/doc/" "" $ takeDirectory d))) ++ "</a></li>") allDirs
                              return $ Right ("/doc/index", ("doc tag","N/A","","",[],[],"<p>Bibliography for tag <em>doc</em>, most recent first: " ++ show (length allDirs) ++ " tags (<a href='/index' class='link-page link-tag directory-indexes-upwards link-annotated' data-link-icon='arrow-up-left' data-link-icon-type='svg' rel='tag' title='Link to parent directory'>parent</a>).</p> <div class=\"columns TOC\"> <ul>" ++ allDirLinks ++ "</ul> </div>"))

gwernAbstract :: Bool -> String -> String -> String -> [Tag String] -> (String,String)
gwernAbstract _ p' description toc f =
  let anchor  = sed ".*#" "" p'
      baseURL = sed "#.*" "" p'
      (t,abstrctRw, abstrct) = if not ("#" `isInfixOf` p') then ("", takeWhile takeToAbstract $ dropWhile dropToAbstract $ dropWhile dropToBody f, trim $ renderTags $ filter filterAbstract $ takeWhile takeToAbstract $ dropWhile dropToAbstract $ dropWhile dropToBody f)
        -- if there is an anchor, then there may be an abstract after it which is a better annotation than the first abstract on the page.
        -- Examples of this are appendices like /Timing#reverse-salients, which have not been split out to a standalone page, but also have their own abstract which is more relevant than the top-level abstract of /Timing.
                else let
                         beginning = dropWhile (dropToID anchor) $ dropWhile dropToBody f
                         -- complicated titles like `## Loehlin & Nichols 1976: _A Study of 850 Sets of Twins_` won't be just a single TagText, so grab everything inside the <a></a>:
                         title = renderTags $ takeWhile dropToLinkEnd $ dropWhile dropToText $ drop 1 $ dropWhile dropToLink beginning
                         titleClean = trim $ sed "<span>(.*)</span>" "\\1" $ replaceMany [("\n", " "), ("<span class=\"smallcaps\">",""), ("<span class=\"link-auto-first\">","")] title
                         abstractRaw = takeWhile takeToAbstract $ dropWhile dropToAbstract $ takeWhile dropToSectionEnd $ drop 1 beginning
                         restofpageAbstract = replace "<p><span class=\"reader-mode-note\"><strong>Note</strong>: to hide apparatus like the links, you can use reader-mode (<span class=\"reader-mode-selector-inline\"><!-- non-empty span placeholder --></span>).</span></p>" "" $
                                              trim $ renderTags $ filter filterAbstract abstractRaw
                         in (titleClean, abstractRaw, restofpageAbstract)
      abstrct'  = (if anyPrefix abstrct ["<p>", "<p>", "<figure>"] then abstrct
                    else if null abstrct then "" else "<p>"++abstrct++"</p>") ++ " " ++ toc
      -- combine description + abstract; if there's no abstract, settle for the description:
      abstrct'' = if description /= "" && abstrct' /= "" then "<div class=\"page-description-annotation\"><p>"++description++"</p></div>"++
                                                              abstrct'
                                      else if description == "" && abstrct' /= "" then abstrct'
                                           else if description /= "" && abstrct' == "" then "<p>"++description++"</p>"
                                                else ""
      abstrct''' = replace "<p><span class=\"reader-mode-note\"><strong>Note</strong>: to hide apparatus like the links, you can use reader-mode (<span class=\"reader-mode-selector-inline\"><!-- non-empty span placeholder --></span>).</span></p>" "" $
                   trim $ replace "href=\"#" ("href=\"/"++baseURL++"#") abstrct'' -- turn relative anchor paths into absolute paths
      abstrct'''' = linkAutoHtml5String $ sed " id=\"fnref[0-9]+\"" "" abstrct''' -- rm footnote IDs - cause problems when transcluded
  in if (("#" `isInfixOf` p') && null abstrct) then (t,"") else
       if "scrape-abstract-not" `isInfixOf` renderTags abstrctRw then (t,"") else
         (t,abstrct'''') -- if shortAllowed then (t,abstrct'''') else (t,abstrct'''')

dropToAbstract, takeToAbstract, filterAbstract, dropToBody, dropToSectionEnd, dropToLink, dropToLinkEnd, dropToText :: Tag String -> Bool
dropToClass, dropToID :: String -> Tag String -> Bool
dropToClass    i (TagOpen "div" attrs) = case lookup "class" attrs of
                                             Nothing -> True
                                             Just classes -> not (i `isInfixOf` classes)
dropToClass _ _                               = True
dropToAbstract = dropToClass "abstract"
dropToID    i (TagOpen _ attrs) = case lookup "id" attrs of
                                             Nothing -> True
                                             Just id' -> i /= id'
dropToID _ _                               = True
takeToAbstract (TagClose "div") = False
takeToAbstract _                = True
filterAbstract (TagOpen  "div" _)        = False
filterAbstract (TagClose "div")          = False
filterAbstract (TagOpen  "blockquote" _) = False
filterAbstract (TagClose "blockquote")   = False
filterAbstract _                         = True
dropToBody (TagOpen "body" _) = False
dropToBody _ = True
dropToSectionEnd (TagClose "section") = False
dropToSectionEnd (TagOpen "section" _) = False -- sections are recursively nested, and we want just the *current* section, not all the nested subsections as well!
dropToSectionEnd _ = True
dropToLink (TagOpen "a" _) = False
dropToLink _ = True
dropToLinkEnd (TagClose "a") = False
dropToLinkEnd _ = True
dropToText (TagText _) = False
dropToText (TagOpen "em" _) = False
dropToText (TagClose "em") = False
dropToText _ = True

-- gwernTOC :: Bool -> Bool -> String -> [Tag String] -> String
-- gwernTOC footnotesP indexP p' f =
--    -- Pandoc declines to add an ID to footnotes section; on Gwern.net, we override this by at compile-time rewriting the <section> to have `#footnotes`:
--   (\tc'' -> if not footnotesP then tc'' else sed "</ul>\n? *</div>$" "<li><a href=\"#footnotes\">Footnotes</a></li></ul></div>" tc'') $
--  -- for tags, condense the ToC by removing the See Also & Miscellaneous <h1>s, and the Links wrapper around the individual entries:
--  (\tc' -> if not indexP then tc'
--    else sedMany [("</li>\n          \n        </ul>",""),
--                  ("<li>\n            <a class=\"id-not\" href=\"#miscellaneous\"><span>Miscellaneous</span></a>\n          </li>", ""),
--                  ("<li>\n            <a class=\"id-not\" href=\"#miscellaneous\">Miscellaneous</a>\n          </li>", ""),
--                  ("<li>\n            <a class=\"id-not\" href=\"#links\"><span>Links</span></a>\n            <ul>", ""),
--                  ("<li>\n            <a class=\"id-not\" href=\"#links\">Links</a>\n            <ul>", ""),
--                  ("<li>\n            <a class=\"id-not\" href=\"#see-also\"><span>See Also</span></a>\n          </li>", ""),
--                  ("<li>\n            <a class=\"id-not\" href=\"#see-also\">See Also</a>\n          </li>", ""),
--                  ("<li>\n            <a class=\"id-not\" href=\"#link-bibliography\"><span>Link Bibliography</span></a>\n          </li>", ""),
--                  ("<li>\n            <a class=\"id-not\" href=\"#link-bibliography\">Link Bibliography</a>\n          </li>", ""),
--                  ("class=\"footnote-ref\" role=\"doc-noteref\"", "")
--                 ] tc') $
--  (\tc -> -- add columns class to condense it in popups/tags
--         (if indexP then id else replace "<div class=\"columns\"><div class=\"TOC\">" "<div class=\"columns TOC\">") $
--         -- WARNING: Pandoc generates redundant <span></span> wrappers by abusing the span wrapper trick while removing header self-links <https://github.com/jgm/pandoc/issues/8020>; so since those are the only <span>s which should be in ToCs (...right? [EDIT: no, the subscript citations are]), we'll remove them. TODO: should be fixed in Pandoc HEAD, so remove this after a full upgrade.
--         sed "<span>(.*)</span>" "\\1" $
--         (if '#'`elem`p' then (\t -> let toc = truncateTOC p' t in if toc /= "" then "<div class=\"columns TOC\">" ++ toc ++ "</div>" else "") else replace "<a href=" "<a class=\"id-not\" href=") $
--         -- NOTE: we strip the `id="TOC"`, and all other link IDs on TOC subentries, deliberately because the ID will cause HTML validation problems when abstracts get transcluded into tags/link-bibliographies/backlinks.
--         sed " id=\"[a-z0-9-]+\">" ">" $ replace " id=\"markdownBody\"" "" $ replace " id=\"TOC\"" "" tc) index
--         where
--           index = if length indexType1 > length indexType2 then indexType1 else indexType2
--           indexType1 = replace "markdownBody" "" $ replace "directory-indexes" "" $ replace "columns" "columns TOC" $ renderTagsOptions renderOptions $
--             takeWhile (\e' -> e' /= TagClose "div") $ dropWhile (\e -> e /=  TagOpen "div" [("id","markdownBody"),("class","markdownBody directory-indexes columns")]) f
--           indexType2 = renderTagsOptions renderOptions $
--                        [TagOpen "div" [("class","columns")]] ++
--                        (takeWhile (\e' -> e' /= TagClose "div")  $ dropWhile (\e -> e /= TagOpen "div" [("id","TOC"), ("class","TOC")]) f) ++
--                        [TagClose "div"]

-- truncateTOC :: String -> String -> String
-- truncateTOC p' toc = let pndc = truncateTOCHTML (T.pack (sed ".*#" "" p')) (T.pack toc) in
--                        if null pndc then "" else
--                            case runPure $ writeHtml5String safeHtmlWriterOptions (Pandoc nullMeta (init pndc)) of
--                              Left e -> error ("Failed to compile truncated ToC: " ++ show p' ++ show toc ++ show e)
--                              Right text -> T.unpack text
