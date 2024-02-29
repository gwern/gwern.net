{- LinkMetadata.hs: module for generating Pandoc links which are annotated with metadata, which can
                    then be displayed to the user as 'popups' by /static/js/popups.js. These popups can be excerpts,
                    abstracts, article introductions etc, and make life much more pleasant for the reader—hover over
                    link, popup, read, decide whether to go to link.
Author: Gwern Branwen
Date: 2019-08-20
When:  Time-stamp: "2024-02-28 20:02:30 gwern"
License: CC-0
-}

-- TODO:
-- 1. bugs in packages: rxvist doesn't appear to support all bioRxiv/medRxiv schemas, including the
-- '/early/' links, forcing me to use curl+Tagsoup; the R library 'fulltext' crashes on examples
-- like `ft_abstract(x = c("10.1038/s41588-018-0183-z"))`

{-# LANGUAGE OverloadedStrings #-}
module LinkMetadata (addPageLinkWalk, isPagePath, readLinkMetadata, readLinkMetadataAndCheck, readLinkMetadataNewest, walkAndUpdateLinkMetadata, updateGwernEntries, writeAnnotationFragments, Metadata, MetadataItem, MetadataList, readGtxFast, writeGtx, annotateLink, createAnnotations, hasAnnotation, hasAnnotationOrIDInline, generateAnnotationBlock, generateAnnotationTransclusionBlock, authorsToCite, authorsTruncate, cleanAbstractsHTML, sortItemDate, sortItemPathDate, warnParagraphizeGTX, dateTruncateBad, typesetHtmlField, lookupFallback) where

import Control.Monad (unless, void, when, foldM_, (<=<))

import Data.Char (isPunctuation, toLower, isNumber)
import qualified Data.Map.Strict as M (elems, filter, filterWithKey, fromList, fromListWith, keys, toList, lookup, map, union, size) -- traverseWithKey, union, Map
import qualified Data.Text as T (append, isPrefixOf, pack, unpack, Text)
import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import Data.List (intersect, isInfixOf, isPrefixOf, isSuffixOf, sort, sortBy, (\\))
import Data.List.HT (search)
import Network.HTTP (urlEncode)
import Network.URI (isURIReference)
import System.Directory (doesFileExist, doesDirectoryExist, getFileSize)
import System.FilePath (takeDirectory, takeFileName, takeExtension)
import Text.Pandoc (readerExtensions, Inline(Link, Span),
                    def, writeHtml5String, runPure, pandocExtensions,
                    readHtml, nullAttr, nullMeta,
                    Inline(Image, Str, RawInline, Space, Strong), Pandoc(..), Format(..), Block(RawBlock, Para, BlockQuote, Div))
import Text.Pandoc.Walk (walk, walkM)
import Text.Regex.TDFA ((=~))
import Text.Show.Pretty (ppShow)

import qualified Control.Monad.Parallel as Par (mapM_, mapM)

import System.IO.Unsafe (unsafePerformIO)

import Config.LinkID (affiliationAnchors)
import Inflation (nominalToRealInflationAdjuster, nominalToRealInflationAdjusterHTML)
import Interwiki (convertInterwikiLinks)
import Typography (typographyTransform, titlecase')
import Image (invertImageInline, addImgDimensions, imageLinkHeightWidthSet, isImageFilename, isVideoFilename)
import LinkArchive (localizeLink, ArchiveMetadata)
import LinkBacklink (getSimilarLinkCheck, getSimilarLinkCount, getBackLinkCount, getBackLinkCheck, getLinkBibLinkCheck, getAnnotationLink)
import LinkID (authorsToCite, generateID)
import LinkLive (linkLive, alreadyLive)
import LinkMetadataTypes (Metadata, MetadataItem, Path, MetadataList, Failure(Temporary, Permanent), isPagePath, hasHTMLSubstitute)
import Paragraph (paragraphized)
import Query (extractLinksInlines)
import Tags (listTagsAll, tagsToLinksSpan)
import MetadataFormat (processDOI, cleanAbstractsHTML, dateRegex, linkCanonicalize, authorsInitialize, balanced, authorsTruncate, dateTruncateBad)
import Utils (writeUpdatedFile, printGreen, printRed, anyInfix, anyPrefix, anySuffix, replace, anyPrefixT, hasAny, safeHtmlWriterOptions, addClass, hasClass, parseRawAllClean, hasExtensionS, isLocal)
import Annotation (linkDispatcher)
import Annotation.Gwernnet (gwern)
import LinkIcon (linkIcon)
import Gtx (appendLinkMetadata, readGtxFast, rewriteLinkMetadata, writeGtx)

-- Should the current link get a 'G' icon because it's an essay or regular page of some sort?
-- we exclude several directories (doc/, static/) entirely; a Gwern.net page is then any
-- link without a file extension (ie. a '.' in the URL - we guarantee that no Markdown essay has a
-- period inside its URL).
-- Essay/page links get the 'link-page' class.
addPageLinkWalk :: Pandoc -> Pandoc
addPageLinkWalk = walk addPageLink

addPageLink :: Inline -> Inline
addPageLink y@(Link (a,b,c) e (f,g)) = if "link-page" `elem` b || "link-page-not" `elem` b || not (isPagePath f) then y
                                        else Link (a, "link-page" : b, c) e (f, g)
addPageLink x = x

-------------------------------------------------------------------------------------------------------------------------------

-- Run an arbitrary function on the 3 databases to update individual items.
-- For example, to use `processDOIArxiv` to add inferred-DOIs to all Arxiv annotations prior to Arxiv adding official DOIs, one could run a command like:
--
-- > walkAndUpdateLinkMetadata True (\x@(path,(title,author,date,doi,tags,abstrct)) -> if not ("https://arxiv.org" `isPrefixOf` path) || (doi /= "") then return x else return (path,(title,author,date,processDOIArxiv path,tags,abstrct)))
--
-- To rewrite a tag, eg. 'conscientiousness' → 'psychology/personality/conscientiousness':
--
-- > walkAndUpdateLinkMetadata True (\(path,(title,author,date,doi,tags,abst)) -> return (path,(title,author,date,doi,
--      map (\t -> if t/="conscientiousness" then t else "psychology/personality/conscientiousness") tags,  abst)) )
--
-- To rerun LinkAuto.hs (perhaps because some rules were added):
--
-- > walkAndUpdateLinkMetadata True (\(a,(b,c,d,e,f,abst)) -> return (a,(b,c,d,e,f, linkAutoHtml5String abst)))
walkAndUpdateLinkMetadata :: Bool -> ((Path, MetadataItem) -> IO (Path, MetadataItem)) -> IO ()
walkAndUpdateLinkMetadata check f = do walkAndUpdateLinkMetadataGtx f "metadata/full.gtx"
                                       walkAndUpdateLinkMetadataGtx f "metadata/half.gtx"
                                       walkAndUpdateLinkMetadataGtx f "metadata/auto.gtx"
                                       when check (printGreen "Checking…" >> readLinkMetadataAndCheck >> printGreen "Validated all GTX post-update; exiting.")

walkAndUpdateLinkMetadataGtx :: ((Path, MetadataItem) -> IO (Path, MetadataItem)) -> Path -> IO ()
walkAndUpdateLinkMetadataGtx f file = do db <- readGtxFast file -- TODO: refactor this to take a list of URLs to update, then I can do it incrementally & avoid the mysterious space leaks
                                         db' <-  mapM f db
                                         writeGtx file db'
                                         printGreen $ "Updated " ++ file

-- This can be run every few months to update abstracts (they generally don't change much).
updateGwernEntries :: IO ()
updateGwernEntries = do rescrapeGTX gwernEntries "metadata/full.gtx"
                        rescrapeGTX gwernEntries "metadata/half.gtx"
                        rescrapeGTX gwernEntries "metadata/auto.gtx"
                        readLinkMetadataAndCheck >> printGreen "Validated all GTX post-update; exiting…"
  where gwernEntries path = ("/" `isPrefixOf` path || "https://gwern.net" `isPrefixOf` path) && not ("." `isInfixOf` path)

-- eg. to rescrape a specific abstract: `rescrapeGTX (\p -> p == "/notes/Attention") "metadata/half.gtx"`
rescrapeGTX :: (Path -> Bool) -> Path -> IO ()
rescrapeGTX filterF gtxpath = do dbl <- readGtxFast gtxpath
                                 let paths = filter filterF $ map fst dbl
                                 foldM_ (rescrapeItem gtxpath) dbl paths

rescrapeItem :: Path -> [(Path, MetadataItem)] -> Path -> IO MetadataList
rescrapeItem gtx dblist path =
  case lookup path dblist of
   Just old -> do new <- updateGwernEntry (path,old)
                  if (path,old) /= new then do let dblist' = new : filter ((/=) path . fst) dblist
                                               writeGtx gtx dblist'
                                               readGtxFast gtx
                   else return dblist
   Nothing -> return dblist

updateGwernEntry :: (Path, MetadataItem) -> IO (Path, MetadataItem)
updateGwernEntry x@(path,(title,author,date,doi,tags,_)) = if False then return x -- || not ("index"`isInfixOf` path)
    else do printGreen path
            newEntry <- gwern path
            case newEntry of
              Left Temporary -> return x
              Left Permanent -> return (path,(title,author,date,doi,tags,"")) -- zero out the abstract but preserve the other metadata; if we mistakenly scraped a page before and generated a pseudo-abstract, and have fixed that mistake so now it returns an error rather than pseudo-abstract, we want to erase that pseudo-abstract until such time as it returns a 'Right' (a successful real-abstract)
              Right (path', (title',author',date',doi',_,abstract')) -> return (path', (title',author',date',doi',tags,abstract'))

-- read the annotation base (no checks, >8× faster)
readLinkMetadata :: IO Metadata
readLinkMetadata = do
             full <- readGtxFast "metadata/full.gtx"  -- for hand created definitions, to be saved; since it's handwritten and we need line errors, we use GTX:
             half <- readGtxFast "metadata/half.gtx" -- tagged but not handwritten/cleaned-up
             auto <- readGtxFast "metadata/auto.gtx"    -- auto-generated cached definitions; can be deleted if gone stale
             -- merge the hand-written & auto-generated link annotations, and return:
             let final = M.union (M.fromList full) $ M.union (M.fromList half) (M.fromList auto) -- left-biased, so 'full' overrides 'half' overrides 'half' overrides 'auto'
             return final

-- read the annotation database, and do extensive semantic & syntactic checks for errors/duplicates:
-- TODO: split out into 3 functions at different levels of intensity: 1 full, 1 half, 1 auto and the composition; many of these functions would be better off in MetadataFormat or somewhere
readLinkMetadataAndCheck :: IO Metadata
readLinkMetadataAndCheck = do
             -- for hand created definitions, to be saved; since it's handwritten and we need line errors, we use GTX:
             full <- readGtxFast "metadata/full.gtx"

             -- Quality checks:
             -- requirements:
             -- - URLs/keys must exist, be unique, and either be a remote URL (starting with 'h') or a local filepath (starting with '/') which exists on disk (auto.gtx may have stale entries, but full.gtx should never! This indicates a stale annotation, possibly due to a renamed or accidentally-missing file, which means the annotation can never be used and the true URL/filepath will be missing the hard-earned annotation). We strip http/https because so many websites now redirect and that's an easy way for duplicate annotations to exist.
             -- - titles must exist & be unique (overlapping annotations to pages are disambiguated by adding the section title or some other description)
             -- - authors must exist (if only as 'Anonymous' or 'N/A'), but are non-unique
             -- - dates are non-unique & optional/NA for always-updated things like Wikipedia. If they exist, they should be of the format 'YYYY[-MM[-DD]]'.
             -- - DOIs are optional since they usually don't exist, and non-unique (there might be annotations for separate pages/anchors for the same PDF and thus same DOI; DOIs don't have any equivalent of `#page=n` I am aware of unless the DOI creator chose to mint such DOIs, which they never (?) do). DOIs sometimes use hyphens and so are subject to the usual problems of em/en-dashes sneaking in by 'smart' systems screwing up.
             -- - tags are optional, but all tags should exist on-disk as a directory of the form "doc/$TAG/"
             -- - annotations must exist and be unique inside full.gtx (overlap in auto.gtx can be caused by the hacky appending); their HTML should pass some simple syntactic validity checks
             let urlsC = map fst full
             let normalizedUrlsC = map (replace "https://" "" . replace "http://" "") urlsC
             when (length (nubOrd (sort normalizedUrlsC)) /=  length normalizedUrlsC) $ error $ "full.gtx: Duplicate URLs! " ++ unlines (normalizedUrlsC \\ nubOrd normalizedUrlsC)

             let tagsAllC = nubOrd $ concatMap (\(_,(_,_,_,_,ts,_)) -> ts) full

             let badDoisDash = filter (\(_,(_,_,_,doi,_,_)) -> anyInfix doi ["–", "—", " ", ",", "{", "}", "!", "@", "#", "$", "\"", "'", "arxiv", ".org", "http"]) full in
                 unless (null badDoisDash) $ error $ "full.gtx: Bad DOIs (invalid punctuation in DOI): " ++ show badDoisDash
             -- about the only requirement for DOIs, aside from being made of graphical Unicode characters (which includes spaces <https://www.compart.com/en/unicode/category/Zs>!), is that they contain one '/': https://www.doi.org/doi_handbook/2_Numbering.html#2.2.3 "The DOI syntax shall be made up of a DOI prefix and a DOI suffix separated by a forward slash. There is no defined limit on the length of the DOI name, or of the DOI prefix or DOI suffix. The DOI name is case-insensitive and can incorporate any printable characters from the legal graphic characters of Unicode." https://www.doi.org/doi_handbook/2_Numbering.html#2.2.1
             -- Thus far, I have not run into any real DOIs which omit numbers, so we'll include that as a check for accidental tags inserted into the DOI field.
             let badDois = filter (\(_,(_,_,_,doi,_,_)) -> if (doi == "") then False else doi `elem` tagsAllC || head doi `elem` ['a'..'z'] || '/' `notElem` doi || null ("0123456789" `intersect` doi) || "https" `isPrefixOf` doi) full in
               unless (null badDois) $ error $ "full.gtx: Invalid DOI (missing mandatory forward slash or a number): " ++ show badDois

             let emptyCheck = filter (\(u,(t,a,_,_,_,s)) ->  "" `elem` [u,t,a,s]) full
             unless (null emptyCheck) $ error $ "full.gtx: Link Annotation Error: empty mandatory fields! [URL/title/author/abstract] This should never happen: " ++ show emptyCheck

             let annotations = map (\(_,(_,_,_,_,_,s)) -> s) full in
               when (length (nubOrd (sort annotations)) /= length annotations) $ error $
               "full.gtx:  Duplicate annotations: " ++ unlines (annotations \\ nubOrd annotations)

             -- intermediate link annotations: not finished, like 'full.gtx' entries, but also not fully auto-generated.
             -- This is currently intended for storing entries for links which I give tags (probably as part of creating a new tag & rounding up all hits), but which are not fully-annotated; I don't want to delete the tag metadata, because it can't be rebuilt, but such half annotations can't be put into 'full.gtx' without destroying all of the checks' validity.
             half <- readGtxFast "metadata/half.gtx"
             let (fullPaths,halfPaths) = (map fst full, map fst half)
             let redundantHalfs = fullPaths `intersect` halfPaths
             unless (null redundantHalfs) (printRed "Redundant entries in half.gtx & full.gtx: " >> printGreen (show redundantHalfs))

             let urlsCP = map fst (full ++ half)
             let files = map (takeWhile (/='#') . tail) $ filter (\u -> head u == '/') urlsCP

             let ensureExtension f = if '.' `elem` f then f else f ++ ".md"
             let checkFile f = fmap not $ doesFileExist $ ensureExtension f
             fileChecks <- Par.mapM checkFile files
             let missingFiles = map fst $ filter snd $ zip files fileChecks
             let printError f = let f' = ensureExtension f in
                                printRed ("Full+half annotation error: file does not exist? " ++ f ++ " (checked file name: " ++ f' ++ ")")
             mapM_ printError missingFiles

             -- auto-generated cached definitions; can be deleted if gone stale
             rewriteLinkMetadata half full "metadata/auto.gtx" -- do auto-cleanup  first
             auto <- readGtxFast "metadata/auto.gtx"
             -- merge the hand-written & auto-generated link annotations, and return:
             let final = M.union (M.fromList full) $ M.union (M.fromList half) (M.fromList auto) -- left-biased, so 'full' overrides 'half' overrides 'auto'
             let finalL = M.toList final

             let urlsFinal = M.keys final
             let brokenUrlsFinal = filter (\u -> null u ||
                                            not (head u == 'h' || head u == '/' || anyPrefix u ["mailto:", "irc://", "rsync://"]) ||
                                            (head u == '/' && "//" `isInfixOf` u) ||
                                            ' ' `elem` u ||
                                            ('—' `elem` u) -- EM DASH
                                          )
                                   urlsFinal
             unless (null brokenUrlsFinal) $ error $ "GTX: Broken URLs: " ++ show brokenUrlsFinal

             let balancedQuotes = filter (\(_,(_,_,_,_,_,abst)) -> let count = length $ filter (=='"') abst in
                                             count > 0 && (count `mod` 2 == 1) ) finalL
             unless (null balancedQuotes) $ error $ "GTX: Link Annotation Error: unbalanced double quotes! " ++ show balancedQuotes

             let balancedBrackets = map (\(p,(title',_,_,_,_,abst) ) -> (p, balanced title', balanced abst)) $
                                     filter (\(_,(title,_,_,_,_,abst)) -> not $ null (balanced title ++ balanced abst)) finalL
             unless (null balancedBrackets) $ do printRed "GTX: Link Annotation Error: unbalanced brackets!"
                                                 printGreen $ ppShow balancedBrackets

             -- check validity of all external links:
             let urlsAll = filter (\(x@(u:_),_) -> if u `elem` ['/', '!', '$', '\8383'] ||
                                                      "wikipedia.org" `isInfixOf` x || "hoogle.haskell.org" `isInfixOf` x || not (anyPrefix x ["ttps://", "ttp://", "/wiki", "wiki/", "/http"]) then False
                                                 else not (isURIReference x)) finalL
             unless (null urlsAll) $ printRed "Invalid URIs?" >> printGreen (ppShow urlsAll)

             -- look for duplicates due to missing affiliation:
             let urlsDuplicateAffiliation = findDuplicatesURLsByAffiliation final
             unless (null urlsDuplicateAffiliation) $ printRed "Duplicated URLs by affiliation:" >> printGreen (show urlsDuplicateAffiliation)

             let titlesSimilar = sort $ map (\(u,(t,_,_,_,_,_)) -> (u, map toLower t)) $ filter (\(u,_) -> '.' `elem` u && not ("wikipedia.org" `isInfixOf` u)) $ M.toList final
             let titles = filter (\title -> length title > 10) $ map snd titlesSimilar
             unless (length (nubOrd titles) == length titles) $ printRed  "Duplicate titles in GTXs!: " >> printGreen (show (titles \\ nubOrd titles))

             let authors = map (\(_,(_,aut,_,_,_,_)) -> aut) finalL
             mapM_ (\a -> unless (null a) $ when (a =~ dateRegex || isNumber (head a) || isPunctuation (head a)) (printRed "Mixed up author & date?: " >> printGreen a) ) authors
             let authorsBadChars = filter (\a -> anyInfix a [";", "&", "?", "!"] || isPunctuation (last a)) $ filter (not . null) authors
             unless (null authorsBadChars) (printRed "Mangled author list?" >> printGreen (ppShow authorsBadChars))

             let datesBad = filter (\(_,(_,_,dt,_,_,_)) -> not (dt =~ dateRegex || null dt)) finalL
             unless (null datesBad) (printRed "Malformed date (not 'YYYY[-MM[-DD]]'): ") >> printGreen (show datesBad)

             -- 'filterMeta' may delete some titles which are good; if any annotation has a long abstract, all data sources *should* have provided a valid title. Enforce that.
             let titlesEmpty = M.filter (\(t,_,_,_,_,abst) -> t=="" && length abst > 100) final
             unless (null titlesEmpty) $ error ("Link Annotation Error: missing title despite abstract!" ++ show titlesEmpty)

             let tagIsNarrowerThanFilename = M.map (\(title,_,_,_,tags,_) -> (title,tags)) $ M.filterWithKey (\f (_,_,_,_,tags,_) -> if not ("/doc/" `isPrefixOf` f) then False else
                                                        let fileTag = replace "/doc/" "" $ takeDirectory f
                                                         in any ((fileTag++"/") `isPrefixOf`) tags) final
             unless (null tagIsNarrowerThanFilename) $ printRed "Files whose tags are more specific than their path: " >> printGreen (unlines $ map (\(f',(t',tag')) -> t' ++ " : " ++ f' ++ " " ++ unwords tag') $ M.toList tagIsNarrowerThanFilename)

             -- check tags (not just full but all of them, including half.gtx)
             let tagsSet = sort $ nubOrd $ concat $ M.elems $ M.map (\(_,_,_,_,tags,_) -> tags) $ M.filter (\(t,_,_,_,_,_) -> t /= "") final
             tagsAll <- listTagsAll
             let tagsBad = tagsSet \\ tagsAll
             let annotationsWithBadTags = M.filter (\(_,_,_,_,ts,_) -> hasAny ts tagsBad) final
             unless (null annotationsWithBadTags) $ error $ "Link Annotation Error: tag does not match a directory! Bad annotations: " ++ show annotationsWithBadTags

             -- these are good ideas but will have to wait for embedding-based refactoring to be usable warnings.
             -- let tagsOverused = filter (\(c,_) -> c > tagMax) $ tagCount final
             -- unless (null tagsOverused) $ printRed "Overused tags: " >> printGreen (show tagsOverused)

             -- let tagPairsOverused = filter (\(c,_) -> c > tagPairMax) $ tagPairsCount final
             -- unless (null tagPairsOverused) $ printRed "Overused pairs of tags: " >> printGreen (show tagPairsOverused)

             -- 'See Also' links in annotations get put in multi-columns due to their typical length, but if I cut them down to 1–2 items, the default columns will look bad. `preprocess-markdown.hs` can't do a length check because it has no idea how I will edit the list of similar-links down, so I can't remove the .columns class *there*; only way to do it is check finished annotations for having .columns set but also too few similar-links:
             let badSeeAlsoColumnsUse = M.keys $ M.filterWithKey (\_ (_,_,_,_,_,abst) -> let count = length (Data.List.HT.search "data-embeddingdistance" abst) in (count == 1 || count == 2) && "<div class=\"columns\">" `isInfixOf` abst ) final
             unless (null badSeeAlsoColumnsUse) $ printRed "Remove columns from skimpy See-Also annotations: " >> printGreen (show badSeeAlsoColumnsUse)

             return final

-- return the n most recent/newest annotations, in terms of created, not publication date.
-- HACK: Because we do not (yet) track annotation creation date, we guess at it. *Usually* a new annotation is appended to the end of full.gtx/half.gtx, and so the newest n are the last n from full+half. (auto.gtx is automatically sorted to deduplicate, erasing the temporal order of additions; however, this is not a big loss, as most auto.gtx entries which have a generated annotation worth reading would've been created by `gwtag`ing a link, which would put them into half.gtx instead.)
readLinkMetadataNewest :: Int -> IO Metadata
readLinkMetadataNewest n = do full  <- fmap (reverse . filter (\(_,(_,_,_,_,_,abstrct)) -> not (null abstrct))) $ readGtxFast "metadata/full.gtx"
                              half    <- fmap (reverse . filter (\(_,(_,_,_,_,_,abstrct)) -> not (null abstrct))) $ readGtxFast "metadata/half.gtx"
                              let ratio = fromIntegral (length full) / fromIntegral (length (full++half)) :: Double
                              let n1 = round (fromIntegral n * ratio)
                              let full' = take n1 full
                              let n2 = round (fromIntegral n * (1-ratio))
                              let half' = take n2 half
                              let final = M.fromList $ interleave full' half' -- TODO: we'd like to preserve the ordering, but the Map erases it, and generateDirectory insists on sorting by the publication-date. Hm...
                              return final
  where
    interleave :: [a] -> [a] -> [a]
    interleave (a1:a1s) (a2:a2s) = a1:a2:interleave a1s a2s
    interleave _        _        = []

-- read a GTX database and look for annotations that need to be paragraphized.
warnParagraphizeGTX :: FilePath -> IO ()
warnParagraphizeGTX path = do gtx <- readGtxFast path
                              let unparagraphized = filter (\(f,(_,_,_,_,_,abst)) -> not (paragraphized f abst)) gtx
                              unless (null unparagraphized) $ printGreen $ ppShow (map fst unparagraphized)

minimumAnnotationLength :: Int
minimumAnnotationLength = 250

writeAnnotationFragments :: ArchiveMetadata -> Metadata  -> Bool -> IO ()
writeAnnotationFragments am md writeOnlyMissing = mapM_ (\(p, mi) -> writeAnnotationFragment am md writeOnlyMissing p mi) $ M.toList md
writeAnnotationFragment :: ArchiveMetadata -> Metadata -> Bool -> Path -> MetadataItem -> IO ()
writeAnnotationFragment _ _ _ _ ("","","","",[],"") = return ()
writeAnnotationFragment am md onlyMissing u i@(a,b,c,doi,ts,abst) =
      if ("/index#" `isInfixOf` u && ("#section" `isInfixOf` u || "-section" `isSuffixOf` u)) ||
         anyInfix u ["/index#see-also", "/index#links", "/index#miscellaneous"] then return ()
      else do let u' = linkCanonicalize u
              let (filepath',_) = getAnnotationLink u'
              annotationExisted <- doesFileExist filepath'
              when (not onlyMissing || (onlyMissing && not annotationExisted)) $ do

                  (_,bl) <- getBackLinkCheck u'
                  blN    <- getBackLinkCount u'
                  (_,sl) <- getSimilarLinkCheck u'
                  slN    <- getSimilarLinkCount u'
                  (_,lb) <- getLinkBibLinkCheck u'
                  -- we prefer annotations which have a fully-written abstract, but we will settle for 'partial' annotations,
                  -- which serve as a sort of souped-up tooltip: partials don't get the dotted-underline indicating a full annotation, but it will still pop-up on hover.
                  -- Now, tooltips already handle title/author/date, so we only need partials in the case of things with tags, abstracts, backlinks, or similar-links, which cannot be handled by tooltips (since HTML tooltips only let you pop up some raw unstyled Unicode text, not clickable links).
                  when (any (not . null) [concat (drop 1 ts), abst, (if blN > 1 then bl else ""), (if slN > 5 then sl else "")]) $ do
                      let titleHtml    = nominalToRealInflationAdjusterHTML c $ typesetHtmlField $ titlecase' a
                      let authorHtml   = typesetHtmlField b
                      -- obviously no point in trying to reformatting date/DOI, so skip those
                      let abstractHtml = typesetHtmlField abst
                      -- TODO: this is fairly redundant with 'pandocTransform' in hakyll.hs; but how to fix without circular dependencies...
                      let pandoc = Pandoc nullMeta $ generateAnnotationBlock False True (u', Just (titleHtml,authorHtml,c,doi,ts,abstractHtml)) bl sl lb
                      unless (null abst) $ void $ createAnnotations md pandoc
                      pandoc' <- do let p = walk (linkLive . nominalToRealInflationAdjuster) $
                                                  convertInterwikiLinks $
                                                  walk (hasAnnotation md) $
                                                  walk addPageLinkWalk $
                                                  parseRawAllClean pandoc
                                    walkM (invertImageInline <=< imageLinkHeightWidthSet <=< localizeLink am) p
                      let finalHTMLEither = runPure $ writeHtml5String safeHtmlWriterOptions pandoc'

                      when (length (urlEncode u') > 273) (printRed "Warning, annotation fragment path → URL truncated!" >>
                                                          putStrLn ("Was: " ++ urlEncode u' ++ " but truncated to: " ++ take 247 u' ++ "; (check that the truncated file name is still unique, otherwise some popups will be wrong)"))

                      case finalHTMLEither of
                        Left er -> error ("Writing annotation fragment failed! " ++ show u ++ " : " ++ show i ++ " : " ++ show er)
                        Right finalHTML -> do finalHTML' <- fmap T.pack $ addImgDimensions $ T.unpack finalHTML -- try to add image height=/width= attributes to `<img>` elements for faster rendering for annotations
                                              writeUpdatedFile "annotation" filepath' finalHTML'
             -- HACK: the current hakyll.hs assumes that all annotations already exist before compilation begins, although we actually dynamically write as we go.
             -- This leads to an annoying behavior where a new annotation will not get synced in its first build, because Hakyll doesn't "know" about it and won't copy it into the _site/ compiled version, and it won't get rsynced up. This causes unnecessary errors.
             -- There is presumably some way for Hakyll to do the metadata file listing *after* compilation is finished, but it's easier to hack around here by forcing 'new' annotation writes to be manually inserted into _site/.
                                              unless annotationExisted $ writeUpdatedFile "annotation" ("./_site/"++filepath') finalHTML

typesetHtmlField :: String -> String
typesetHtmlField "" = ""
typesetHtmlField  t = let fieldPandocMaybe = runPure $ readHtml def{readerExtensions = pandocExtensions} (T.pack t) in
                        case fieldPandocMaybe of
                          Left errr -> error $ " : " ++ t ++ show errr
                          Right fieldPandoc -> let (Pandoc _ fieldPandoc') = typographyTransform fieldPandoc in
                                                 let (Right fieldHtml) = runPure $ writeHtml5String safeHtmlWriterOptions (Pandoc nullMeta fieldPandoc') in
                            T.unpack fieldHtml

-- walk each page, extract the links, and create annotations as necessary for new links
createAnnotations :: Metadata -> Pandoc -> IO ()
createAnnotations md (Pandoc _ markdown) = Par.mapM_ (annotateLink md) $ extractLinksInlines (Pandoc nullMeta markdown)

annotateLink :: Metadata -> Inline -> IO (Either Failure (Path, MetadataItem))
annotateLink md x@(Link (_,_,_) _ (targetT,_))
  | anyPrefixT targetT ["/metadata/", "#", "!", "\8383", "$"] = return (Left Permanent) -- annotation intermediate files, self-links, interwiki links, and inflation-adjusted currencies *never* have annotations.
  | otherwise =
  do let target = T.unpack targetT
     when (null target) $ error (show x)
     when ((reverse $ take 3 $ reverse target) == "%20" || last target == ' ') $ error $ "URL ends in space? " ++ target ++ " (" ++ show x ++ ")"
     -- normalize: convert 'https://gwern.net/doc/foo.pdf' to '/doc/foo.pdf' and './doc/foo.pdf' to '/doc/foo.pdf'
     -- the leading '/' indicates this is a local Gwern.net file
     let target' = replace "https://gwern.net/" "/" target
     let target'' = if head target' == '.' then drop 1 target' else target'

     -- check local link validity: every local link except tags should exist on-disk:
     when (head target'' == '/' && not ("/metadata/annotation/" `isPrefixOf` target'')) $
       do isDirectory <- doesDirectoryExist (tail target'')
          when isDirectory $ error ("Attempted to annotate a directory, which is not allowed (links must be to files or $DIRECTORY/index): " ++ target' ++ " : " ++ target ++ " (" ++ show x ++ ")")
          let target''' = (\f -> if '.' `notElem` f then f ++ ".md" else f) $ takeWhile (/='#') $ tail target''

          unless (takeFileName target''' == "index" || takeFileName target''' == "index.md") $
             do exist <- doesFileExist target'''
                unless exist $ printRed ("Link error in 'annotateLink': file does not exist? " ++ target''' ++ " (" ++target++")" ++ " (" ++ show x ++ ")")

     let annotated = M.lookup target'' md
     case annotated of
       -- the link has a valid annotation already defined, so we're done: nothing changed.
       Just i  -> return (Right (target'', i))
       Nothing -> do new <- linkDispatcher x
                     case new of
                       -- some failures we don't want to cache because they may succeed when checked differently or later on or should be fixed:
                       Left Temporary -> return (Left Temporary)
                       -- cache the failures too, so we don't waste time rechecking the PDFs every build; return False because we didn't come up with any new useful annotations:
                       Left Permanent -> appendLinkMetadata target'' ("", "", "", "", [], "") >> return (Left Permanent)
                       Right y@(f,m@(_,_,_,_,_,e)) -> do
                                       when (e=="") $ printGreen (f ++ " : " ++ show target ++ " : " ++ show y)
                                       -- return true because we *did* change the database & need to rebuild:
                                       appendLinkMetadata target'' m >> return (Right y)
annotateLink _ x = error ("annotateLink was passed an Inline which was not a Link: " ++ show x)

-- walk the page, and modify each URL to specify if it has an annotation available or not:
hasAnnotation :: Metadata -> Block -> Block
hasAnnotation md = walk (hasAnnotationOrIDInline md)

hasAnnotationOrIDInline :: Metadata -> Inline -> Inline
hasAnnotationOrIDInline metadata inline = case inline of
    link@(Link (_, classes, _) _ (url, _)) ->
        if hasAnyAnnotatedClass classes
            then link
            else processLink metadata url link
    _ -> inline
 where
        hasAnyAnnotatedClass :: [T.Text] -> Bool
        hasAnyAnnotatedClass = hasAny ["link-annotated-not", "link-annotated", "link-annotated-partial"]

        processLink :: Metadata -> T.Text -> Inline -> Inline
        processLink metadatadb url link =
            let canonicalUrl = linkCanonicalize $ T.unpack url
            in case M.lookup canonicalUrl metadatadb of
                Nothing                  -> addID Nothing link
                Just ("","","","",[],"") -> addID Nothing link
                Just metadataItem        -> addID (Just metadataItem) (addHasAnnotation metadataItem link)

addID :: Maybe MetadataItem -> Inline -> Inline
addID maybeMetadataItem inline = case inline of
    (Link x@(anchor, classes, _) e (url, title)) ->
        if anchor == "" && "id-not" `notElem` classes
            then Link (generateLinkID x maybeMetadataItem url) e (url, title)
            else inline
    _ -> handleInvalidAddIDCall maybeMetadataItem inline
 where
        generateLinkID :: (T.Text, [T.Text], [(T.Text, T.Text)]) -> Maybe MetadataItem -> T.Text -> (T.Text, [T.Text], [(T.Text, T.Text)])
        generateLinkID ("", classs, kvs) maybeMetadataItem' url = case maybeMetadataItem' of
            Nothing                         -> (generateID (T.unpack url) "" "",       classs, kvs)
            Just (_, author, date, _, _, _) -> (generateID (T.unpack url) author date, classs, kvs)
        -- if it has an ID already, avoid overriding?
        generateLinkID a _ _ = a

        handleInvalidAddIDCall :: Maybe MetadataItem -> Inline -> a
        handleInvalidAddIDCall maybeMetadataItemBad inlineBad = error $
            "LinkMetadata.hs: addID: called with " ++
            show maybeMetadataItemBad ++
            " annotation and a non-Link Inline element:" ++
            show inlineBad ++
            "; This should never happen."

addHasAnnotation :: MetadataItem -> Inline -> Inline
addHasAnnotation (title,aut,dt,_,_,abstrct) (Link (a,b,c) e (f,g))
  | "https://en.wikipedia.org" `T.isPrefixOf` f = x'
  -- WARNING: Twitter is currently handled in Config.LinkArchive, because whether a Twitter/Nitter URL is a valid 'annotation' depends on whether there is a Nitter snapshot hosted locally the JS can query. Many Nitter snapshots, sadly, fail, so it is *not* guaranteed that a Twitter URL will have a usable snapshot. TODO: when Twitter is merged into the backend, parsing the Nitter mirrors to create proper annotations, rather than using JS to parse them at runtime, this should be removed.
  | length abstrct > minimumAnnotationLength    = addClass "link-annotated" x' -- full annotation, no problem.
   -- may be a partial...
  | not $ unsafePerformIO $ doesFileExist $ fst $ getAnnotationLink $ T.unpack f = x' -- no, a viable partial would have a (short) fragment written out, see `writeAnnotationFragment` logic
  | otherwise = addClass "link-annotated-partial" x'
  where
    g'
      | g/="" = g
      | title=="" && aut=="" = g
      | title/="" && aut=="" = T.pack title
      | title=="" && aut/="" = T.pack $ authorsToCite (T.unpack f) aut dt
      | otherwise = T.pack $ "'" ++ title ++ "', " ++ authorsToCite (T.unpack f) aut dt
    x' = Link (a,b,c) e (f,g')
addHasAnnotation _ z = z

-- was this link given either a partial or full annotation?
wasAnnotated :: Inline -> Bool
wasAnnotated x@Link{}  = isAnnotatedInline x
wasAnnotated x@Image{} = isAnnotatedInline x
wasAnnotated x = error $ "LinkMetadata.wasAnnotated: tried to get annotation status of a non-Link/Image element, which makes no sense? " ++ show x
isAnnotatedInline :: Inline -> Bool
isAnnotatedInline x = -- let f = inline2Path x in
                            hasClass "link-annotated" x ||
                            hasClass "link-annotated-partial" x

generateAnnotationBlock :: Bool -> Bool -> (FilePath, Maybe MetadataItem) -> FilePath -> FilePath -> FilePath -> [Block]
generateAnnotationBlock truncAuthorsp annotationP (f, ann) blp slp lb =
  case ann of
     Nothing                 -> nonAnnotatedLink
     -- Just ("",   _,_,_,_,_)  -> nonAnnotatedLink
     -- Just (_,    _,_,_,_,"") -> nonAnnotatedLink
     Just x@(tle,aut,dt,doi,ts,abst) ->
       let tle' = if null tle then "<code>"++f++"</code>" else if "<em>"`isPrefixOf`tle && "</em>"`isSuffixOf`tle then tle else "“"++tle++"”"
           lid = let tmpID = generateID f aut dt in
                   if tmpID=="" then "" else T.pack "link-bibliography-" `T.append` tmpID
           -- NOTE: we cannot link to an anchor fragment in ourselves, like just link in the annotation header to `#backlink-transclusion`, because it would severely complicate all the anchor-rewriting logic (how would it know if `#backlink-transclusion` refers to something *in* the annotation, or is a section or anchor inside the annotated URL?). But fortunately, by the logic of caching, it doesn't much matter if we link the same URL twice and pop it up the first time vs transclude it inside the popup/popin the second time.
           lidBacklinkFragment    = if lid=="" then "" else "backlink-transclusion-"    `T.append` lid
           lidSimilarLinkFragment = if lid=="" then "" else "similarlink-transclusion-" `T.append` lid
           lidLinkBibLinkFragment = if lid=="" then "" else "link-bibliography-transclusion-" `T.append` lid
           authorShort = authorsTruncate aut
           authorsInitialized = if truncAuthorsp then [Str (T.pack authorShort)] else authorsInitialize aut
           authorSpan = if aut/=authorShort then Span ("", ["author", "cite-author-plural"], [("title",T.pack aut)]) authorsInitialized
                        else Span ("", ["author", "cite-author"], []) authorsInitialized
           author = if aut=="" || aut=="N/A" || aut=="N/\8203A" then [Space] else [Space, authorSpan]
           date = if dt=="" then [] else [Span ("", ["date", "cite-date"],
                                                 if dateTruncateBad dt /= dt then [("title",T.pack dt)] else []) -- don't set a redundant title
                                           [Str (T.pack $ dateTruncateBad dt)]]
           tags = if ts==[] then [] else [tagsToLinksSpan $ map T.pack ts]
           backlink = if blp=="" then [] else (if tags==[] then [] else [Str ";", Space]) ++  [Span ("", ["backlinks"], []) [Link ("",["aux-links", "link-page", "backlinks", "icon-not", "link-annotated-not"],[]) [Str "backlinks"] (T.pack blp, "Reverse citations for this page.")]]
           similarlink = if slp=="" then [] else (if blp=="" && tags==[] then [] else [Str ";", Space]) ++ [Span ("", ["similars"], []) [Link ("",["aux-links", "link-page", "similars", "icon-not", "link-annotated-not"],[]) [Str "similar"] (T.pack slp, "Similar links for this link (by text embedding).")]]
           linkBibliography = if lb=="" then [] else (if blp=="" && slp=="" && tags==[] then []
                                                       else [Str ";", Space]) ++ [Span ("", ["link-bibliography"], [])
                                                                                   [Link ("",["aux-links", "link-page", "link-bibliography", "icon-not", "link-annotated-not"],[]) [Str "bibliography"] (T.pack lb, "Link-bibliography for this annotation (list of references/sources/links it cites).")]]
           values = if doi=="" then [] else [("doi",T.pack $ processDOI doi)]
           -- on directory indexes/link bibliography pages, we don't want to set 'link-annotated' class because the annotation is already being presented inline. It makes more sense to go all the way popping the link/document itself, as if the popup had already opened. So 'annotationP' makes that configurable:
           link = Link (lid, if annotationP then ["link-annotated"] else ["link-annotated-not"], values) [RawInline (Format "html") (T.pack $ tle')] (T.pack f,"")
           -- make sure every abstract is wrapped in paragraph tags for proper rendering:
           abst' = if null abst || anyPrefix abst ["<p>", "<ul", "<ol", "<h2", "<h3", "<bl", "<figure"] then abst else "<p>" ++ abst ++ "</p>"
       in
         [Para
              ([link] ++
                (if null aut && null date then [] else [Str ","]) ++
                author ++
                date ++
                (if (tags++backlink++similarlink++linkBibliography)==[] then []
                  else [Str " ("] ++
                       tags ++
                       backlink ++
                       similarlink ++
                       linkBibliography ++
                       [Str ")"] ++
                       (if null abst then [] else [Str "\8288:"])
                ))] ++
                (if null abst then []
                  else [BlockQuote [RawBlock (Format "html") (rewriteAnchors f (T.pack abst') `T.append`
                                                   if (blp++slp++lb)=="" then ""
                                                   else
                                                        ((if blp=="" then "" else ("<div class=\"backlinks-append aux-links-append collapse\"" `T.append` " id=\"" `T.append` lidBacklinkFragment `T.append` "\" " `T.append` ">\n<p><a class=\"include-even-when-collapsed\" href=\"" `T.append` T.pack blp `T.append` "\"><strong>Backlinks</strong></a>:</p>\n</div>")) `T.append`
                                                         (if slp=="" then "" else ("<div class=\"similars-append aux-links-append collapse\"" `T.append` " id=\"" `T.append` lidSimilarLinkFragment `T.append` "\" " `T.append` ">\n<p><a class=\"include-even-when-collapsed\" href=\"" `T.append` T.pack slp `T.append` "\"><strong>Similar Links</strong></a>.</p>\n</div>")) `T.append`
                                                          (if lb=="" then "" else ("<div class=\"link-bibliography-append aux-links-append collapse\"" `T.append` " id=\"" `T.append` lidLinkBibLinkFragment `T.append` "\" " `T.append` ">\n<p><a class=\"include-even-when-collapsed\" href=\"" `T.append` T.pack lb `T.append` "\"><strong>Link Bibliography</strong></a>:</p>\n</div>")))
                                                              )]
                       ]) ++
                generateFileTransclusionBlock True False (f, x)
    where
      nonAnnotatedLink :: [Block]
      nonAnnotatedLink = [Para [Link nullAttr [Str (T.pack f)] (T.pack f, "")]] ++
                         generateFileTransclusionBlock True False (f, ("",undefined,undefined,undefined,undefined,undefined))
-- generate an 'annotation block' except we leave the actual heavy-lifting of 'generating the annotation' to transclude.js, which will pull the popups annotation instead dynamically/lazily at runtime. As such, this is a simplified version of `generateAnnotationBlock`.
generateAnnotationTransclusionBlock :: (FilePath, MetadataItem) -> [Block]
generateAnnotationTransclusionBlock (f, x@(tle,_,_,_,_,_)) =
                                let tle' = if null tle then "<code>"++f++"</code>" else "“" ++ tle ++ "”"
                                    -- NOTE: we set this on special-case links like Twitter links anyway, even if they technically do not have 'an annotation'; the JS will handle `.include-annotation` correctly anyway
                                    link = linkIcon $ linkLive $ addHasAnnotation x $ Link ("", ["id-not", "include-annotation"], [])
                                      [RawInline (Format "html") (T.pack tle')] (T.pack f,"")
                                    livep = alreadyLive link -- for web pages which are link-live capable, we wish to file-transclude them; this is handled by annotations as usual, but for annotation-less URLs we have the same problem as we do for annotation-less local-file media - #Miscellaneous tag-directories get shafted. So we check for link-live here and force a fallback for links which are live but annotation-less.
                                    fileTransclude = if wasAnnotated link then [] else generateFileTransclusionBlock livep livep (f, ("",undefined,undefined,undefined,undefined,undefined))
                                    linkColon = if wasAnnotated link || null fileTransclude then [] else [Str "\8288:"]
                                in [Para [Strong (link:linkColon)]] ++ fileTransclude
                           --  isVideoFilename (T.unpack f) ||
                           --  "https://www.youtube.com/watch?v=" `T.isPrefixOf` f ||
                           -- ("https://twitter.com/" `T.isPrefixOf` f && "/status/" `T.isInfixOf` f)

-- transclude a *file* (or possibly a URL) directly, if possible. For example, an image will be displayed by `generateAnnotationTransclusionBlock` as a normal list item with its name & metadata as text, but then the image itself will be displayed immediately following it. `generateFileTransclusionBlock` handles the logic of transcluding each supported file type, as each file will require a different approach. (Image files are supported directly by Pandoc, but video files require raw HTML to be generated, while CSV files must be rendered to HTML etc.)
--
-- Collapse behavior: media types are displayed by default everywhere (the user wants to see them immediately because it's easy to see an image etc, and performance-wise they are cheap, because they are either small like images or set to their equivalents of 'lazy loading' like video/audio); document types are collapsed by default everywhere (many users will have no interest and documents like PDFs or HTML can be almost arbitrarily large, like a HTML mirror of "The Forgotten Pixel Art Masterpieces of the PlayStation 1 Era" which due to the animations is fully 183MB!).
-- We want to display media (particularly images) by default, so tag-directories can serve as informal 'galleries'; many images will never be seen in pages/annotations, nor do I want to constantly update a 'gallery' page with every single minimally-interesting image, and images are highly suitable for browsing very rapidly through, so it is fine to display all images for scrolling through.
--
-- For a list of legal Gwern.net filetypes, see </lorem-link#file-type>
-- Supported: documents/code (most, see `isDocumentViewable`/`isCodeViewable`); images (all except PSD); audio (MP3); video (MP4, WebM, YouTube, except SWF); archive/binary (none)
generateFileTransclusionBlock :: Bool -> Bool -> (FilePath, MetadataItem) -> [Block]
generateFileTransclusionBlock fallbackP liveP (f, (tle,_,_,_,_,_)) = if null generateFileTransclusionBlock' then [] else [Div ("", ["aux-links-transclude-file"], []) generateFileTransclusionBlock']
 where
   localP = isLocal (T.pack f)
   titleCaption = if null tle then [] else [Strong [Str "[Expand to view"], Str " “", RawInline (Format "HTML") $ T.pack tle, Str "”]"]
   minFileSizeWarning = 20 -- at what megabyte size should we warn readers about a file before uncollapsing & loading it? We want to avoid those silly warnings like 'PDF (warning: 0.11MB)', since no one is ever going to decide to *not* read an interesting paper if it's only a few MBs. And many webpages today think nothing of loading 10MB+ of assets, and no one demands warnings for those. So the pain point these days seems >10MB. We'll try >20MB for now.
   fileSizeMB = if not localP then 0 else round (fromIntegral (unsafePerformIO $ getFileSize $ takeWhile (/='#') $ tail f) / (1000000::Double)) :: Int
   fileSizeMBString = if fileSizeMB < minFileSizeWarning then "" else show fileSizeMB++"MB"
   fileTypeDescription = fileExtensionToEnglish $ takeExtension f
   fileTypeDescriptionString = if fileTypeDescription/="" then fileTypeDescription else if liveP && not localP then "external link" else "HTML"
   fileDescription = Str $ T.pack $
                            "(" ++ fileTypeDescriptionString
                              ++ (if fileTypeDescriptionString/="" && fileSizeMBString/="" then "; " else "")
                              ++ fileSizeMBString ++ ") "
   generateFileTransclusionBlock'
    | isPagePath (T.pack f) = [] -- for essays, we skip the transclude block: transcluding an entire essay is just a plain bad idea.
    | "wikipedia.org/wiki/" `isInfixOf` f = [] -- TODO: there must be some more principled way to do this, but we don't seem to have an `Interwiki.isWikipedia` or any equivalent...?
    | isDocumentViewable f || isCodeViewable f = let titleDocCode | titleCaption/=[]     = titleCaption
                                                                  | isDocumentViewable f = [Str "[", fileDescription, Strong [Str "Expand to view document"], Str "]"]
                                                                  | otherwise            = [Str "[", fileDescription, Strong [Str "Expand to view code/data"], Str "]"]
                                                 in [Div ("",["collapse"],[])
                                                      [Para [linkIcon $ Link ("", ["id-not", "include-content", "include-lazy"], []) titleDocCode (T.pack f, "")]]]
    -- image/video/audio:
    | Image.isImageFilename f || Image.isVideoFilename f || hasExtensionS ".mp3" f = [Para [Link ("",["include-content", "width-full"],[]) [Str "[", fileDescription, Str "view multimedia in-browser]"] (T.pack f, "")]]
    | otherwise = if not fallbackP then [] else
                   [Para [linkIcon $ Link ("",["id-not", "include-content", "include-lazy", "collapse"],[])
                          (if titleCaption/=[] then titleCaption else [Str "[", fileDescription, Strong [Str "Expand to view web page"], Str "]"])
                          (T.pack f, "")]]

-- document types excluded: ebt, epub, mdb, mht, ttf, docs.google.com; cannot be viewed easily in-browser (yet?)
isDocumentViewable, isCodeViewable :: FilePath -> Bool
isDocumentViewable f = -- (isLocal (T.pack f) && hasExtensionS ".html" f) ||
                       anyInfix f [".json", ".jsonl", ".opml", ".md", ".pdf", ".txt", ".xml"] || -- Pandoc syntax-highlighted or native-browser
                       hasHTMLSubstitute f -- these are converted by LibreOffice to clean HTML versions for preview
-- local source files have syntax-highlighted versions we can load. (NOTE: we cannot transclude remote files which match these, because many URLs are not 'cool URIs' and casually include extensions like '.php' or '.js' while being HTML outputs thereof.)
isCodeViewable     f = isLocal (T.pack f) && anySuffix f [".R", ".css", ".hs", ".js", ".patch", ".sh", ".php", ".conf"] -- we exclude `/static/*/.html` since that's not possible

-- convert a file extension like 'webm' to a human-readable name like 'WebM' (not always simply an upcase)
fileExtensionToEnglish :: String -> String
fileExtensionToEnglish ext = case lookup (takeWhile (/= '#') ext) extensionMapping of
                               Just name -> name
                               Nothing   -> ""
  where extensionMapping = map (\(a,b) -> ("."++a,b)) $ [("json", "JSON"), ("jsonl", "JSON Lines"), ("opml", "OPML"), ("md", "Markdown")
                           , ("pdf", "PDF"), ("txt", "text"), ("xml", "XML"), ("R", "R code"), ("css", "CSS")
                           , ("hs", "Haskell"), ("js", "Javascript"), ("patch", "patch"), ("sh", "Bash")
                           , ("php", "PHP"), ("conf", "configuration"), ("mp3", "MP3"), ("webm", "WebM")
                           , ("mp4", "MP4"), ("bmp", "bitmap"), ("gif", "GIF"), ("ico", "icon"), ("jpg", "JPG")
                           , ("png", "PNG"), ("svg", "SVG"), ("xcf", "XCF (GIMP)"), ("html", "HTML")]

-- annotations, like </face>, often link to specific sections or anchors, like 'I clean the data with [Discriminator Ranking](#discriminator-ranking)'; when transcluded into other pages, these links are broken. But we don't want to rewrite the original abstract as `[Discriminator Ranking](/face#discriminator-ranking)` to make it absolute, because that screws with section-popups/link-icons! So instead, when we write out the body of each annotation inside the link bibliography, while we still know what the original URL was, we traverse it looking for any links starting with '#' and rewrite them to be absolute:
-- WARNING: because of the usual RawBlock/Inline(HTML) issues, reading with Pandoc doesn't help - it just results in RawInline elements which still need to be parsed somehow. I settled for a braindead string-rewrite; in annotations, there shouldn't be *too* many cases where the href=# pattern shows up without being a div link...
rewriteAnchors :: FilePath -> T.Text -> T.Text
rewriteAnchors f = T.pack . replace "href=\"#" ("href=\""++f++"#") . T.unpack

-- find all instances where I link "https://arxiv.org/abs/1410.5401" when it should be "https://arxiv.org/abs/1410.5401#deepmind", where they are inconsistent and the hash matches a whitelist of orgs.
findDuplicatesURLsByAffiliation :: Metadata -> [(String, [String])]
findDuplicatesURLsByAffiliation md = let urls  = nubOrd . filter ('.' `elem`) $ map (\(u,_) -> u) $ M.toList md
                                         urlDB = M.fromListWith (++) $ map (\u -> (takeWhile (/= '#') u, [u])) urls
                                         affiliationURLPatterns = (map (\org -> "#"++org) Config.LinkID.affiliationAnchors) ++
                                                                   (map (\org -> "org="++org) Config.LinkID.affiliationAnchors)
                                         affiliationWhitelist = ["page=", "lilianweng.github.io"]
                                         affiliationURLs = M.filter (\vs -> any (\v -> anyInfix v affiliationURLPatterns) vs) urlDB
                                     in M.toList $ M.filter (\v -> length (filter (\v' -> not (anyInfix v' affiliationWhitelist)) v) > 1) affiliationURLs

-- how do we handle files with appended data, which are linked like '/doc/reinforcement-learning/model-free/2020-bellemare.pdf#google' but exist as files as '/doc/reinforcement-learning/model-free/2020-bellemare.pdf'? We can't just look up the *filename* because it's missing the # fragment, and the annotation is usually for the full path including the fragment. If a lookup fails, we fallback to looking for any annotation with the file as a *prefix*, and accept the first match.
lookupFallback :: Metadata -> String -> (FilePath, MetadataItem)
lookupFallback m u = case M.lookup u m of
                       Nothing -> tryPrefix
                       Just ("","","","",[],"") -> tryPrefix
                       Just mi -> (u,mi)
                       where tryPrefix = let possibles =  M.filterWithKey (\url _ -> u `isPrefixOf` url && url /= u) m
                                             u' = if M.size possibles > 0 then fst $ head $ M.toList possibles else u
                                         in
                                               (if (".md" `isInfixOf` u') || (u == u') then (u, ("", "", "", "", [], "")) else
                                                  -- sometimes the fallback is useless eg, a link to a section will trigger a 'longer' hit, like
                                                  -- '/review/cat.md' will trigger a fallback to /review/cat#fuzz-testing'; the
                                                  -- longer hit will also be empty, usually, and so not better. We check for that case and return
                                                  -- the original path and not the longer path.
                                                  let possibleFallback = lookupFallback m u' in
                                                    if snd possibleFallback == ("", "", "", "", [], "") then (u, ("", "", "", "", [], "")) else
                                                      (u',snd possibleFallback))


-------------------------------------------------------------------------------------------------------------------------------

sortItemDate :: [MetadataItem] -> [MetadataItem]
sortItemDate = sortBy (flip compare `on` third)

sortItemPathDate :: [(Path,(MetadataItem,String))] -> [(Path,(MetadataItem,String))]
sortItemPathDate = sortBy (flip compare `on` (third . fst . snd))

third :: (a,b,c,d,e,f) -> c
third (_,_,rd,_,_,_) = rd
