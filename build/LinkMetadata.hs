{- LinkMetadata.hs: module for generating Pandoc links which are annotated with metadata, which can
                    then be displayed to the user as 'popups' by /static/js/popups.js. These popups can be excerpts,
                    abstracts, article introductions etc, and make life much more pleasant for the reader—hover over
                    link, popup, read, decide whether to go to link.
Author: Gwern Branwen
Date: 2019-08-20
When:  Time-stamp: "2023-04-20 09:14:54 gwern"
License: CC-0
-}

-- TODO:
-- 1. bugs in packages: rxvist doesn't appear to support all bioRxiv/medRxiv schemas, including the
-- '/early/' links, forcing me to use curl+Tagsoup; the R library 'fulltext' crashes on examples
-- like `ft_abstract(x = c("10.1038/s41588-018-0183-z"))`

{-# LANGUAGE OverloadedStrings #-}
module LinkMetadata (addPageLinkWalk, isPagePath, readLinkMetadata, readLinkMetadataAndCheck, readLinkMetadataNewest, walkAndUpdateLinkMetadata, updateGwernEntries, writeAnnotationFragments, Metadata, MetadataItem, MetadataList, readYaml, readYamlFast, writeYaml, annotateLink, createAnnotations, hasAnnotation, hasAnnotationInline, parseRawBlock, parseRawInline, generateAnnotationBlock, generateAnnotationTransclusionBlock, authorsToCite, authorsTruncate, cleanAbstractsHTML, sortItemDate, sortItemPathDate, warnParagraphizeYAML, simplifiedHTMLString, dateTruncateBad, typesetHtmlField) where

import Control.Monad (unless, void, when, foldM_, (<=<))

import qualified Data.ByteString as B (appendFile, readFile)
import Data.Char (isPunctuation, toLower, isSpace, isNumber)
import qualified Data.Map.Strict as M (elems, filter, filterWithKey, fromList, fromListWith, keys, toList, lookup, map, union) -- traverseWithKey, union, Map
import qualified Data.Text as T (append, isInfixOf, isPrefixOf, pack, unpack, Text)
import Data.Containers.ListUtils (nubOrd)
import Data.IORef (IORef)
import Data.Function (on)
import Data.List (intercalate, intersect, isInfixOf, isPrefixOf, isSuffixOf, nub, sort, sortBy, (\\))
import Data.List.HT (search)
import Data.Text.Encoding (decodeUtf8) -- ByteString -> T.Text
import Data.Yaml as Y (decodeFileEither, decodeEither', encode, ParseException) -- NOTE: from 'yaml' package, *not* 'HsYaml'
import Network.HTTP (urlEncode)
import Network.URI (isURIReference)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (takeDirectory, takeExtension, takeFileName, takeBaseName)
import System.GlobalLock (lock)
import Text.Pandoc (readerExtensions, Inline(Link, Span),
                    def, writeHtml5String, runPure, pandocExtensions,
                    readHtml, nullAttr, nullMeta,
                    Inline(Str, RawInline, Space), Pandoc(..), Format(..), Block(RawBlock, Para, BlockQuote, Div, Plain), Attr)
import Text.Pandoc.Walk (walk, walkM)
import Text.Regex.TDFA ((=~)) -- WARNING: avoid the native Posix 'Text.Regex' due to bugs and segfaults/strange-closure GHC errors
import Text.Show.Pretty (ppShow)

import qualified Control.Monad.Parallel as Par (mapM_)

import System.IO.Unsafe (unsafePerformIO)

import Inflation (nominalToRealInflationAdjuster)
import Interwiki (convertInterwikiLinks)
import Typography (typographyTransform, titlecase')
import Image (invertImageInline, addImgDimensions, imageLinkHeightWidthSet)
import LinkArchive (localizeLink, ArchiveMetadata)
import LinkBacklink (getSimilarLinkCheck, getSimilarLinkCount, getBackLinkCount, getBackLinkCheck, getLinkBibLinkCheck, getAnnotationLink)
import LinkID (authorsToCite, generateID)
import LinkLive (linkLive)
import LinkMetadataTypes (Metadata, MetadataItem, Path, MetadataList, Failure(..))
import Paragraph (paragraphized)
import Query (extractLinksInlines)
import Tags (uniqTags, guessTagFromShort, tag2TagsWithDefault, guessTagFromShort, tag2Default, pages2Tags, listTagsAll, tagsToLinksSpan)
import Utils (writeUpdatedFile, printGreen, printRed, sed, trim, simplified, anyInfix, anyPrefix, anySuffix, replace, split, anyPrefixT, hasAny, safeHtmlWriterOptions, addClass, processDOI, cleanAbstractsHTML, dateRegex, linkCanonicalize)
import Annotation (linkDispatcher)
import Annotation.Gwernnet (gwern)

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

isPagePath :: T.Text -> Bool
isPagePath f = let f' = replace "https://gwern.net" "" $ T.unpack f in
    (if
        not ("/" `isPrefixOf` f') ||
      ("/static/" `isPrefixOf` f')
     then False else
       takeExtension f' == "")

-------------------------------------------------------------------------------------------------------------------------------

-- Run an arbitrary function on the 3 databases to update individual items.
-- For example, to use `processDOIArxiv` to add inferred-DOIs to all Arxiv annotations prior to Arxiv adding official DOIs, one could run a command like:
--
-- > walkAndUpdateLinkMetadata (\x@(path,(title,author,date,doi,tags,abstrct)) -> if not ("https://arxiv.org" `isPrefixOf` path) || (doi /= "") then return x else return (path,(title,author,date,processDOIArxiv path,tags,abstrct)))
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
walkAndUpdateLinkMetadata check f = do walkAndUpdateLinkMetadataYaml f "metadata/full.yaml"
                                       walkAndUpdateLinkMetadataYaml f "metadata/half.yaml"
                                       walkAndUpdateLinkMetadataYaml f "metadata/auto.yaml"
                                       when check (printGreen "Checking…" >> readLinkMetadataAndCheck >> printGreen "Validated all YAML post-update; exiting.")

walkAndUpdateLinkMetadataYaml :: ((Path, MetadataItem) -> IO (Path, MetadataItem)) -> Path -> IO ()
walkAndUpdateLinkMetadataYaml f file = do db <- readYaml file -- TODO: refactor this to take a list of URLs to update, then I can do it incrementally & avoid the mysterious space leaks
                                          db' <-  mapM f db
                                          writeYaml file db'
                                          printGreen $ "Updated " ++ file

-- This can be run every few months to update abstracts (they generally don't change much).
updateGwernEntries :: IO ()
updateGwernEntries = do rescrapeYAML gwernEntries "metadata/full.yaml"
                        rescrapeYAML gwernEntries "metadata/half.yaml"
                        rescrapeYAML gwernEntries "metadata/auto.yaml"
                        readLinkMetadataAndCheck >> printGreen "Validated all YAML post-update; exiting…"
  where gwernEntries path = ("/" `isPrefixOf` path || "https://gwern.net" `isPrefixOf` path) && not ("." `isInfixOf` path || "#manual-annotation" `isInfixOf` path)

-- eg. to rescrape a specific abstract: `rescrapeYAML (\p -> p == "/notes/Attention") "metadata/half.yaml"`
rescrapeYAML :: (Path -> Bool) -> Path -> IO ()
rescrapeYAML filterF yamlpath = do dbl <- readYaml yamlpath
                                   let paths = filter filterF $ map fst dbl
                                   foldM_ (rescrapeItem yamlpath) dbl paths

rescrapeItem :: Path -> [(Path, MetadataItem)] -> Path -> IO MetadataList
rescrapeItem yaml dblist path =
  case lookup path dblist of
   Just old -> do new <- updateGwernEntry (path,old)
                  if (path,old) /= new then do let dblist' = new : filter ((/=) path . fst) dblist
                                               writeYaml yaml dblist'
                                               readYaml yaml
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
             full  <- readYaml "metadata/full.yaml"  -- for hand created definitions, to be saved; since it's handwritten and we need line errors, we use YAML:
             half <- readYaml "metadata/half.yaml" -- tagged but not handwritten/cleaned-up
             auto    <- readYaml "metadata/auto.yaml"    -- auto-generated cached definitions; can be deleted if gone stale
             -- merge the hand-written & auto-generated link annotations, and return:
             let final = M.union (M.fromList full) $ M.union (M.fromList half) (M.fromList auto) -- left-biased, so 'full' overrides 'half' overrides 'half' overrides 'auto'
             return final

-- read the annotation database, and do extensive semantic & syntactic checks for errors/duplicates:
readLinkMetadataAndCheck :: IO Metadata
readLinkMetadataAndCheck = do
             -- for hand created definitions, to be saved; since it's handwritten and we need line errors, we use YAML:
             full <- readYaml "metadata/full.yaml"

             -- Quality checks:
             -- requirements:
             -- - URLs/keys must exist, be unique, and either be a remote URL (starting with 'h') or a local filepath (starting with '/') which exists on disk (auto.yaml may have stale entries, but full.yaml should never! This indicates a stale annotation, possibly due to a renamed or accidentally-missing file, which means the annotation can never be used and the true URL/filepath will be missing the hard-earned annotation). We strip http/https because so many websites now redirect and that's an easy way for duplicate annotations to exist.
             -- - titles must exist & be unique (overlapping annotations to pages are disambiguated by adding the section title or some other description)
             -- - authors must exist (if only as 'Anonymous' or 'N/A'), but are non-unique
             -- - dates are non-unique & optional/NA for always-updated things like Wikipedia. If they exist, they should be of the format 'YYYY[-MM[-DD]]'.
             -- - DOIs are optional since they usually don't exist, and non-unique (there might be annotations for separate pages/anchors for the same PDF and thus same DOI; DOIs don't have any equivalent of `#page=n` I am aware of unless the DOI creator chose to mint such DOIs, which they never (?) do). DOIs sometimes use hyphens and so are subject to the usual problems of em/en-dashes sneaking in by 'smart' systems screwing up.
             -- - tags are optional, but all tags should exist on-disk as a directory of the form "doc/$TAG/"
             -- - annotations must exist and be unique inside full.yaml (overlap in auto.yaml can be caused by the hacky appending); their HTML should pass some simple syntactic validity checks
             let urlsC = map fst full
             let normalizedUrlsC = map (replace "https://" "" . replace "http://" "") urlsC
             when (length (nub (sort normalizedUrlsC)) /=  length normalizedUrlsC) $ error $ "full.yaml: Duplicate URLs! " ++ unlines (normalizedUrlsC \\ nubOrd normalizedUrlsC)

             let tagsAllC = nubOrd $ concatMap (\(_,(_,_,_,_,ts,_)) -> ts) full

             let badDoisDash = filter (\(_,(_,_,_,doi,_,_)) -> anyInfix doi ["–", "—", " ", ",", "{", "}", "!", "@", "#", "$", "\"", "'"] || "http" `isInfixOf` doi) full in
                 unless (null badDoisDash) $ error $ "full.yaml: Bad DOIs (invalid punctuation in DOI): " ++ show badDoisDash
             -- about the only requirement for DOIs, aside from being made of graphical Unicode characters (which includes spaces <https://www.compart.com/en/unicode/category/Zs>!), is that they contain one '/': https://www.doi.org/doi_handbook/2_Numbering.html#2.2.3 "The DOI syntax shall be made up of a DOI prefix and a DOI suffix separated by a forward slash. There is no defined limit on the length of the DOI name, or of the DOI prefix or DOI suffix. The DOI name is case-insensitive and can incorporate any printable characters from the legal graphic characters of Unicode." https://www.doi.org/doi_handbook/2_Numbering.html#2.2.1
             -- Thus far, I have not run into any real DOIs which omit numbers, so we'll include that as a check for accidental tags inserted into the DOI field.
             let badDois = filter (\(_,(_,_,_,doi,_,_)) -> if (doi == "") then False else doi `elem` tagsAllC || head doi `elem` ['a'..'z'] || '/' `notElem` doi || null ("0123456789" `intersect` doi)) full in
               unless (null badDois) $ error $ "full.yaml: Invalid DOI (missing mandatory forward slash or a number): " ++ show badDois

             let emptyCheck = filter (\(u,(t,a,_,_,_,s)) ->  "" `elem` [u,t,a,s]) full
             unless (null emptyCheck) $ error $ "full.yaml: Link Annotation Error: empty mandatory fields! [URL/title/author/abstract] This should never happen: " ++ show emptyCheck

             let annotations = map (\(_,(_,_,_,_,_,s)) -> s) full in
               when (length (nub (sort annotations)) /= length annotations) $ error $
               "full.yaml:  Duplicate annotations: " ++ unlines (annotations \\ nubOrd annotations)

             -- intermediate link annotations: not finished, like 'full.yaml' entries, but also not fully auto-generated.
             -- This is currently intended for storing entries for links which I give tags (probably as part of creating a new tag & rounding up all hits), but which are not fully-annotated; I don't want to delete the tag metadata, because it can't be rebuilt, but such half annotations can't be put into 'full.yaml' without destroying all of the checks' validity.
             half <- readYaml "metadata/half.yaml"
             let (fullPaths,halfPaths) = (map fst full, map fst half)
             let redundantHalfs = fullPaths `intersect` halfPaths
             unless (null redundantHalfs) (printRed "Redundant entries in half.yaml & full.yaml: " >> printGreen (show redundantHalfs))

             let urlsCP = map fst (full ++ half)
             let files = map (takeWhile (/='#') . tail) $ filter (\u -> head u == '/') urlsCP
             Par.mapM_ (\f -> let f' = if '.' `elem` f then f else f ++ ".page" in
                                    do exist <- doesFileExist f'
                                       unless exist $ printRed ("Full annotation error: file does not exist? " ++ f ++ " (checked file name: " ++ f' ++ ")")) files

             -- auto-generated cached definitions; can be deleted if gone stale
             rewriteLinkMetadata half full "metadata/auto.yaml" -- do auto-cleanup  first
             auto <- readYaml "metadata/auto.yaml"
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
             unless (null brokenUrlsFinal) $ error $ "YAML: Broken URLs: " ++ show brokenUrlsFinal

             let balancedQuotes = filter (\(_,(_,_,_,_,_,abst)) -> let count = length $ filter (=='"') abst in
                                             count > 0 && (count `mod` 2 == 1) ) finalL
             unless (null balancedQuotes) $ error $ "YAML: Link Annotation Error: unbalanced double quotes! " ++ show balancedQuotes

             let balancedBracketsCurly = filter (\(_,(_,_,_,_,_,abst)) -> let count = length $ filter (\c -> c == '{' || c == '}') abst in
                                                                     count > 0 && (count `mod` 2 == 1) ) finalL
             unless (null balancedBracketsCurly) $ error $ "YAML: Link Annotation Error: unbalanced curly brackets! " ++ show balancedBracketsCurly

             let balancedBracketsSquare = filter (\(_,(_,_,_,_,_,abst)) -> let count = length $ filter (\c -> c == '[' || c == ']') abst in
                                                                     count > 0 && (count `mod` 2 == 1) ) finalL
             unless (null balancedBracketsSquare) $ error $ "YAML: Link Annotation Error: unbalanced square brackets! " ++ show balancedBracketsSquare

             let balancedParens = filter (\(_,(_,_,_,_,_,abst)) -> let count = length $ filter (\c -> c == '(' || c == ')') abst in
                                                                     count > 0 && (count `mod` 2 == 1) ) finalL
             unless (null balancedParens) $ error $ "YAML: Link Annotation Error: unbalanced parentheses! " ++ show (map fst balancedParens)
             let balancedParensTitle = filter (\(_,(t,_,_,_,_,_)) -> let count = length $ filter (\c -> c == '(' || c == ')') t in
                                                                       count > 0 && (count `mod` 2 == 1) ) finalL
             unless (null balancedParensTitle) $ error $ "YAML: Link Title Error: unbalanced parentheses! " ++ show (map fst balancedParensTitle)

             -- check validity of all external links:
             let urlsAll = filter (\(x@(u:_),_) -> if u `elem` ['/', '!', '$', '\8383'] ||
                                                      "wikipedia.org" `isInfixOf` x || "hoogle.haskell.org" `isInfixOf` x || not ("ttps://" `isPrefixOf` x || "ttp://" `isPrefixOf` x || "/wiki" `isPrefixOf` x || "wiki/" `isPrefixOf` x) then False
                                                 else not (isURIReference x)) finalL
             unless (null urlsAll) $ printRed "Invalid URIs?" >> printGreen (ppShow urlsAll)

             -- look for duplicates due to missing affiliation:
             let urlsDuplicateAffiliation = findDuplicatesURLsByAffiliation final
             unless (null urlsDuplicateAffiliation) $ printRed "Duplicated URLs by affiliation:" >> printGreen (show urlsDuplicateAffiliation)

             let titlesSimilar = sort $ map (\(u,(t,_,_,_,_,_)) -> (u, map toLower t)) $ filter (\(u,_) -> '.' `elem` u && not ("wikipedia.org" `isInfixOf` u)) $ M.toList final
             let titles = filter (\title -> length title > 10) $ map snd titlesSimilar
             unless (length (nubOrd titles) == length titles) $ printRed  "Duplicate titles in YAMLs!: " >> printGreen (show (titles \\ nubOrd titles))

             let authors = map (\(_,(_,aut,_,_,_,_)) -> aut) finalL
             Par.mapM_ (\a -> unless (null a) $ when (a =~ dateRegex || isNumber (head a)) (printRed "Mixed up author & date?: " >> printGreen a) ) authors
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

             -- check tags (not just full but all of them, including halfs)
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
-- HACK: Because we do not (yet) track annotation creation date, we guess at it. *Usually* a new annotation is appended to the end of full.yaml/half.yaml, and so the newest n are the last n from full+half. (Auto.yaml is automatically sorted to deduplicate, erasing the temporal order of additions; however, this is not a big loss, as most auto.yaml entries which have a generated annotation worth reading would've been created by `gwtag`ing a link, which would put them into half.yaml instead.)
readLinkMetadataNewest :: Int -> IO Metadata
readLinkMetadataNewest n = do full  <- fmap (reverse . filter (\(_,(_,_,_,_,_,abstrct)) -> not (null abstrct))) $ readYaml "metadata/full.yaml"
                              half    <- fmap (reverse . filter (\(_,(_,_,_,_,_,abstrct)) -> not (null abstrct))) $ readYaml "metadata/half.yaml"
                              let ratio = fromIntegral (length full) / fromIntegral (length (full++half)) :: Double
                              let n1 = round (fromIntegral n * ratio)
                              let full' = take n1 full
                              let n2 = round (fromIntegral n * (1-ratio))
                              let half' = take n2 half
                              let final = M.fromList $ filter (\(path,(_,_,_,_,_,_)) -> not (anySuffix path ["#manual-annotation"])) $ interleave full' half' -- TODO: we'd like to preserve the ordering, but the Map erases it, and generateDirectory insists on sorting by the publication-date. Hm...
                              return final
  where
    interleave :: [a] -> [a] -> [a]
    interleave (a1:a1s) (a2:a2s) = a1:a2:interleave a1s a2s
    interleave _        _        = []

-- read a YAML database and look for annotations that need to be paragraphized.
warnParagraphizeYAML :: FilePath -> IO ()
warnParagraphizeYAML path = do yaml <- readYaml path
                               let unparagraphized = filter (\(f,(_,_,_,_,_,abst)) -> not (paragraphized f abst)) yaml
                               unless (null unparagraphized) $ printGreen $ ppShow (map fst unparagraphized)

minimumAnnotationLength :: Int
minimumAnnotationLength = 250

writeAnnotationFragments :: ArchiveMetadata -> Metadata -> IORef Integer -> Bool -> IO ()
writeAnnotationFragments am md archived writeOnlyMissing = mapM_ (\(p, mi) -> writeAnnotationFragment am md archived writeOnlyMissing p mi) $ M.toList md
writeAnnotationFragment :: ArchiveMetadata -> Metadata -> IORef Integer -> Bool -> Path -> MetadataItem -> IO ()
writeAnnotationFragment _ _ _ _ _ ("","","","",[],"") = return ()
writeAnnotationFragment am md archived onlyMissing u i@(a,b,c,d,ts,abst) =
      if ("/index#" `isInfixOf` u && ("#section" `isInfixOf` u || "-section" `isSuffixOf` u)) ||
         anyInfix u ["/index#see-also", "/index#links", "/index#miscellaneous", "/index#manual-annotation"] then return ()
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
                      let titleHtml    = typesetHtmlField $ titlecase' a
                      let authorHtml   = typesetHtmlField b
                      -- obviously no point in trying to reformatting date/DOI, so skip those
                      let abstractHtml = typesetHtmlField abst
                      -- TODO: this is fairly redundant with 'pandocTransform' in hakyll.hs; but how to fix without circular dependencies...
                      let pandoc = Pandoc nullMeta $ generateAnnotationBlock False True (u', Just (titleHtml,authorHtml,c,d,ts,abstractHtml)) bl sl lb
                      -- for partials, we skip the heavyweight processing:
                      unless (null abst) $ void $ createAnnotations md pandoc
                      pandoc' <- if null abst then return pandoc
                                    else do
                                          let p = walk (convertInterwikiLinks . nominalToRealInflationAdjuster) $
                                                  walk (hasAnnotation md) $
                                                  walk addPageLinkWalk $
                                                  walk (parseRawBlock nullAttr) pandoc
                                          walkM (invertImageInline <=< imageLinkHeightWidthSet <=< localizeLink am archived) p
                      let finalHTMLEither = runPure $ writeHtml5String safeHtmlWriterOptions pandoc'
                      when (length (urlEncode u') > 273) (printRed "Warning, annotation fragment path → URL truncated!" >>
                                                          putStrLn ("Was: " ++ urlEncode u' ++ " but truncated to: " ++ take 247 u' ++ "; (check that the truncated file name is still unique, otherwise some popups will be wrong)"))

                      case finalHTMLEither of
                        Left er -> error ("Writing annotation fragment failed! " ++ show u ++ " : " ++ show i ++ " : " ++ show er)
                        Right finalHTML -> do finalHTML' <- if not ("<img " `T.isInfixOf` finalHTML) then return finalHTML else fmap T.pack $ addImgDimensions $ T.unpack finalHTML -- try to add image height=/width= attributes to `<img>` elements for faster rendering for annotations
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
createAnnotations md (Pandoc _ markdown) = mapM_ (annotateLink md) $ extractLinksInlines (Pandoc nullMeta markdown)

annotateLink :: Metadata -> Inline -> IO Bool
annotateLink md x@(Link (_,_,_) _ (targetT,_))
  | anyPrefixT targetT ["/metadata/", "#", "$", "!", "\8383"] = return False
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
          let target''' = (\f -> if '.' `notElem` f then f ++ ".page" else f) $ takeWhile (/='#') $ tail target''

          unless (takeFileName target''' == "index" || takeFileName target''' == "index.page") $
             do exist <- doesFileExist target'''
                unless exist $ printRed ("Link error in 'annotateLink': file does not exist? " ++ target''' ++ " (" ++target++")" ++ " (" ++ show x ++ ")")

     let annotated = M.lookup target'' md
     case annotated of
       -- the link has a valid annotation already defined, so we're done: nothing changed.
       Just _  -> return False
       Nothing -> do new <- linkDispatcher x
                     case new of
                       -- some failures we don't want to cache because they may succeed when checked differently or later on or should be fixed:
                       Left Temporary -> return False
                       -- cache the failures too, so we don't waste time rechecking the PDFs every build; return False because we didn't come up with any new useful annotations:
                       Left Permanent -> appendLinkMetadata target'' ("", "", "", "", [], "") >> return False
                       Right y@(f,m@(_,_,_,_,_,e)) -> do
                                       when (e=="") $ printGreen (f ++ " : " ++ show target ++ " : " ++ show y)
                                       -- return true because we *did* change the database & need to rebuild:
                                       appendLinkMetadata target'' m >> return True
annotateLink _ x = error ("annotateLink was passed an Inline which was not a Link: " ++ show x)

-- walk the page, and modify each URL to specify if it has an annotation available or not:
hasAnnotation :: Metadata -> Block -> Block
hasAnnotation md = walk (hasAnnotationInline md)

hasAnnotationInline :: Metadata -> Inline -> Inline
hasAnnotationInline mdb y@(Link (a,classes,c) d (f,g)) =
  if hasAny ["link-annotated-not", "link-annotated", "link-annotated-partial"] classes then y
  else
    let f' = linkCanonicalize $ T.unpack f in
      case M.lookup f' mdb of
        Nothing                  -> if a=="" then Link (generateID f' "" "",classes,c) d (f,g) else y
        Just ("","","","",[],"") -> if a=="" then Link (generateID f' "" "",classes,c) d (f,g) else y -- zilch
        Just                 mi  -> addHasAnnotation mi y -- possible partial
hasAnnotationInline _ y = y

addHasAnnotation :: MetadataItem -> Inline -> Inline
addHasAnnotation (title,aut,dt,_,_,abstrct) (Link (a,b,c) e (f,g))  =
 let a'
       | a == ""    = generateID (T.unpack f) aut dt
       | otherwise  = a
     -- f' = linkCanonicalize $ T.unpack f
     -- we would like to set the tooltip title too if omitted, for the non-JS users and non-human users who may not read the data-* attributes:
     g'
       | g/="" = g
       |       title=="" && aut=="" = g
       |       title/="" && aut=="" = T.pack title
       |       title=="" && aut/="" = T.pack $ authorsToCite (T.unpack f) aut dt
       |                  otherwise = T.pack $ "'" ++ title ++ "', " ++ authorsToCite (T.unpack f) aut dt
     x' = Link (a',b,c) e (f,g') -- remember to set the ID!
 in -- erase link ID?
   if "https://en.wikipedia.org" `T.isPrefixOf` f then x' else
    if length abstrct > minimumAnnotationLength then -- full annotation, no problem:
      addClass "link-annotated" x'
      else -- may be a partial...
           -- no, a viable partial would have a (short) fragment written out, see `writeAnnotationFragment` logic
           if not $ unsafePerformIO $ doesFileExist $ fst $ getAnnotationLink $ T.unpack f then x'
           else -- so it's not a local link, doesn't have a full annotation, doesn't have an on-demand annotation like a Wikipedia article, but does have *some* partial annotation since it exists on disk, so it gets `.link-annotated-partial`
             addClass "link-annotated-partial" x'
addHasAnnotation _ z = z

parseRawBlock :: Attr -> Block -> Block
parseRawBlock attr x@(RawBlock (Format "html") h) = let pandoc = runPure $ readHtml def{readerExtensions = pandocExtensions} h in
                                          case pandoc of
                                            Left e -> error (show x ++ " : " ++ show e)
                                            Right (Pandoc _ blocks) -> Div attr blocks
parseRawBlock _ x = x
parseRawInline :: Attr -> Inline -> Inline
parseRawInline attr x@(RawInline (Format "html") h) = let pandoc = runPure $ readHtml def{readerExtensions = pandocExtensions} h in
                                          case pandoc of
                                            Left e -> error (show x ++ " : " ++ show e)
                                            Right (Pandoc _ [Para inlines]) -> Span attr inlines
                                            Right (Pandoc _ [Plain inlines]) -> Span attr inlines
                                            Right (Pandoc _ inlines) -> Span attr (extractAndFlattenInlines inlines)
parseRawInline _ x = x
extractAndFlattenInlines :: [Block] -> [Inline]
extractAndFlattenInlines x = error (show x)

simplifiedHTMLString :: String -> String
simplifiedHTMLString arg = trim $ T.unpack $ simplified $ parseRawBlock nullAttr (RawBlock (Text.Pandoc.Format "html") (T.pack arg))

generateAnnotationBlock :: Bool -> Bool -> (FilePath, Maybe MetadataItem) -> FilePath -> FilePath -> FilePath -> [Block]
generateAnnotationBlock truncAuthorsp annotationP (f, ann) blp slp lb = case ann of
                              Nothing                 -> nonAnnotatedLink
                              -- Just ("",   _,_,_,_,_)  -> nonAnnotatedLink
                              -- Just (_,    _,_,_,_,"") -> nonAnnotatedLink
                              Just (tle,aut,dt,doi,ts,abst) ->
                                let tle' = if null tle then "<code>"++f++"</code>" else tle
                                    lid = let tmpID = (generateID f aut dt) in if tmpID=="" then "" else (T.pack "link-bibliography-") `T.append` tmpID
                                    -- NOTE: we cannot link to an anchor fragment in ourselves, like just link in the annotation header to `#backlink-transclusion`, because it would severely complicate all the anchor-rewriting logic (how would it know if `#backlink-transclusion` refers to something *in* the annotation, or is a section or anchor inside the annotated URL?). But fortunately, by the logic of caching, it doesn't much matter if we link the same URL twice and pop it up the first time vs transclude it inside the popup/popin the second time.
                                    lidBacklinkFragment    = if lid=="" then "" else "backlink-transclusion-"    `T.append` lid
                                    lidSimilarLinkFragment = if lid=="" then "" else "similarlink-transclusion-" `T.append` lid
                                    lidLinkBibLinkFragment = if lid=="" then "" else "link-bibliography-transclusion-" `T.append` lid
                                    authorShort = authorsTruncate aut
                                    authorSpan = if aut/=authorShort then Span ("", ["author", "cite-author-plural"], [("title",T.pack aut)]) [Str (T.pack $ if truncAuthorsp then authorShort else aut)]
                                                 else Span ("", ["author", "cite-author"], []) [Str (T.pack $ if truncAuthorsp then authorShort else aut)]
                                    author = if aut=="" || aut=="N/A" || aut=="N/\8203A" then [Space] else [Space, authorSpan]
                                    date = if dt=="" then [] else [Span ("", ["date", "cite-date"],
                                                                          if dateTruncateBad dt /= dt then [("title",T.pack dt)] else []) -- don't set a redundant title
                                                                    [Str (T.pack $ dateTruncateBad dt)]]
                                    tags = if ts==[] then [] else [tagsToLinksSpan $ map T.pack ts]
                                    backlink = if blp=="" then [] else (if tags==[] then [] else [Str ";", Space]) ++  [Span ("", ["backlinks"], []) [Link ("",["aux-links", "link-page", "backlinks", "icon-not", "link-annotated-not"],[]) [Str "backlinks"] (T.pack blp, "Reverse citations for this page.")]]
                                    similarlink = if slp=="" then [] else (if blp=="" && tags==[] then [] else [Str ";", Space]) ++ [Span ("", ["similars"], []) [Link ("",["aux-links", "link-page", "similars", "icon-not", "link-annotated-not"],[]) [Str "similar"] (T.pack slp, "Similar links for this link (by text embedding).")]]
                                    linkBibliography = if lb=="" then [] else (if blp=="" && slp=="" && tags==[] then [] else [Str ";", Space]) ++ [Span ("", ["link-bibliography"], []) [Link ("",["aux-links", "link-page", "link-bibliography", "icon-not", "link-annotated-not"],[]) [Str "bibliography"] (T.pack lb, "Link-bibliography for this annotation (list of links it cites).")]]
                                    values = if doi=="" then [] else [("doi",T.pack $ processDOI doi)]
                                    -- on directory indexes/link bibliography pages, we don't want to set 'link-annotated' class because the annotation is already being presented inline. It makes more sense to go all the way popping the link/document itself, as if the popup had already opened. So 'annotationP' makes that configurable:
                                    link = linkLive $ Link (lid, if annotationP then ["link-annotated"] else ["link-annotated-not"], values) [RawInline (Format "html") (T.pack $ "“"++tle'++"”")] (T.pack f,"")
                                    -- make sure every abstract is wrapped in paragraph tags for proper rendering:
                                    abst' = if null abst || anyPrefix abst ["<p>", "<ul", "<ol", "<h2", "<h3", "<bl", "<figure"] then abst else "<p>" ++ abst ++ "</p>"
                                in
                                  [Para
                                       ([link,Str ","] ++
                                         author ++
                                         date ++
                                         (if (tags++backlink++similarlink++linkBibliography)==[] then []
                                           else [Str " ("] ++
                                                tags ++
                                                backlink ++
                                                similarlink ++
                                                linkBibliography ++
                                                [Str ")"] ++
                                                (if null abst then [] else [Str ":"])
                                         ))] ++
                                         (if null abst then []
                                           else [BlockQuote [RawBlock (Format "html") (rewriteAnchors f (T.pack abst') `T.append`
                                                                            if (blp++slp++lb)=="" then ""
                                                                            else "<div class=\"collapse aux-links-container\">" `T.append`
                                                                                 ((if blp=="" then "" else ("<div class=\"backlinks-append aux-links-append\"" `T.append` " id=\"" `T.append` lidBacklinkFragment `T.append` "\" " `T.append` ">\n<p>[<a class=\"include-even-when-collapsed include-replace-container\" href=\"" `T.append` T.pack blp `T.append` "\">Backlinks for this annotation</a>.]</p>\n</div>")) `T.append`
                                                                                  (if slp=="" then "" else ("<div class=\"similars-append aux-links-append\"" `T.append` " id=\"" `T.append` lidSimilarLinkFragment `T.append` "\" " `T.append` ">\n<p>[<a class=\"include-even-when-collapsed include-replace-container\" href=\"" `T.append` T.pack slp `T.append` "\">Similar links for this annotation</a>.]</p>\n</div>")) `T.append`
                                                                                   (if lb=="" then "" else ("<div class=\"link-bibliography-append aux-links-append\"" `T.append` " id=\"" `T.append` lidLinkBibLinkFragment `T.append` "\" " `T.append` ">\n<p>[<a class=\"include include-replace-container\" href=\"" `T.append` T.pack lb `T.append` "\">Link bibliography for this annotation</a>.]</p>\n</div>"))) `T.append`
                                                                            "</div>"
                                                                                       )]
                                                ])
                             where
                               nonAnnotatedLink :: [Block]
                               nonAnnotatedLink = [Para [Link nullAttr [Str (T.pack f)] (T.pack f, "")]]

-- generate an 'annotation block' except we leave the actual heavy-lifting of 'generating the annotation' to transclude.js, which will pull the popups annotation instead dynamically/lazily at runtime; but for backwards compatibility with non-JS readers (such as NoScript users, bots, tools etc), we still provide the title/author/date/backlinks/similar-links along with the transcluded-URL. For JS users, that all will be replaced by the proper popups annotation, and is mostly irrelevant to them. As such, this is a simplified version of `generateAnnotationBlock`.
generateAnnotationTransclusionBlock :: (FilePath, MetadataItem) -> FilePath -> FilePath -> FilePath -> [Block]
generateAnnotationTransclusionBlock (f, (tle,aut,dt,doi,ts,_)) blp slp lb =
                                let tle' = if null tle then "<code>"++f++"</code>" else tle
                                    lid = let tmpID = (generateID f aut dt) in if tmpID=="" then "" else (T.pack "link-bibliography-") `T.append` tmpID
                                    authorShort = authorsTruncate aut
                                    authorSpan = if aut/=authorShort then Span ("", ["author", "cite-author-plural"], [("title",T.pack aut)]) [Str (T.pack authorShort)]
                                                 else Span ("", ["author", "cite-author"], []) [Str (T.pack authorShort)]
                                    author = if aut=="" || aut=="N/A" || aut=="N/\8203A" then [Space] else [Space, authorSpan]
                                    date = if dt=="" then [] else [Span ("", ["date", "cite-date"],
                                                                          if dateTruncateBad dt /= dt then [("title",T.pack dt)] else []) -- don't set a redundant title
                                                                    [Str (T.pack $ dateTruncateBad dt)]]
                                    tags = if ts==[] then [] else [tagsToLinksSpan $ map T.pack ts]
                                    backlink = if blp=="" then [] else (if tags==[] then [] else [Str ";", Space]) ++  [Span ("", ["backlinks"], []) [Link ("",["aux-links", "link-page", "backlinks", "icon-not"],[]) [Str "backlinks"] (T.pack blp,"Reverse citations for this page.")]]
                                    similarlink = if slp=="" then [] else (if blp=="" && tags==[] then [] else [Str ";", Space]) ++ [Span ("", ["similars"], []) [Link ("",["aux-links", "link-page", "similars", "icon-not"],[]) [Str "similar"] (T.pack slp,"Similar links for this link (by text embedding).")]]
                                    linkBibliography = if lb=="" then [] else (if blp=="" && slp=="" && tags==[] then [] else [Str ";", Space]) ++ [Span ("", ["link-bibliography"], []) [Link ("",["aux-links", "link-page", "icon-not"],[]) [Str "bibliography"] (T.pack lb, "Link-bibliography for this annotation (list of links it cites).")]]
                                    values = if doi=="" then [] else [("doi",T.pack $ processDOI doi)]
                                    link = linkLive $ Link (lid, ["link-annotated", "include-annotation", "include-replace-container"], values) [RawInline (Format "html") (T.pack $ "“"++tle'++"”")] (T.pack f,"")
                                in
                                  [Para
                                       ([link,Str ","] ++
                                         author ++
                                         date ++
                                         (if (tags++backlink++similarlink)==[] then []
                                           else [Str " ("] ++
                                                tags ++
                                                backlink ++
                                                similarlink ++
                                                linkBibliography ++
                                                [Str ")"]
                                         ))
                                  ]

-- annotations, like /face, often link to specific sections or anchors, like 'I clean the data with [Discriminator Ranking](#discriminator-ranking)'; when transcluded into other pages, these links are broken. But we don't want to rewrite the original abstract as `[Discriminator Ranking](/face#discriminator-ranking)` to make it absolute, because that screws with section-popups/link-icons! So instead, when we write out the body of each annotation inside the link bibliography, while we still know what the original URL was, we traverse it looking for any links starting with '#' and rewrite them to be absolute:
-- WARNING: because of the usual RawHtml issues, reading with Pandoc doesn't help - it just results in RawInlines which still need to be parsed somehow. I settled for a braindead string-rewrite; in annotations, there shouldn't be *too* many cases where the href=# pattern shows up without being a div link...
rewriteAnchors :: FilePath -> T.Text -> T.Text
rewriteAnchors f = T.pack . replace "href=\"#" ("href=\""++f++"#") . T.unpack

-- WARNING: update the list in /static/js/annotations.js L354 if you change this list!
affiliationAnchors :: [String]
affiliationAnchors = ["adobe", "alibaba", "allen", "amazon", "anthropic", "apple", "baai", "baidu", "bair", "bytedance", "cerebras", "cohere", "deepmind", "eleutherai", "elementai", "facebook", "flickr", "github", "google", "google-graphcore", "googledeepmind", "graphcore", "huawei", "huggingface", "ibm", "intel", "jd", "kakao", "laion", "lighton", "microsoft", "microsoftnvidia", "miri", "naver", "nvidia", "openai", "pinterest", "pdf", "salesforce", "samsung", "sberbank", "schmidhuber", "sensetime", "snapchat", "sony", "spotify", "tencent", "tensorfork", "twitter", "uber", "yandex"]

-- find all instances where I link "https://arxiv.org/abs/1410.5401" when it should be "https://arxiv.org/abs/1410.5401#deepmind", where they are inconsistent and the hash matches a whitelist of orgs.
findDuplicatesURLsByAffiliation :: Metadata -> [(String, [String])]
findDuplicatesURLsByAffiliation md = let urls  = nubOrd . filter ('.' `elem`) $ map (\(u,_) -> u) $ M.toList md
                                         urlDB = M.fromListWith (++) $ map (\u -> (takeWhile (/= '#') u, [u])) urls
                                         affiliationURLPatterns = (map (\org -> "#"++org) affiliationAnchors) ++
                                                                   (map (\org -> "org="++org) affiliationAnchors)
                                         affiliationWhitelist = ["page=", "lilianweng.github.io"]
                                         affiliationURLs = M.filter (\vs -> any (\v -> anyInfix v affiliationURLPatterns) vs) urlDB
                                     in M.toList $ M.filter (\v -> length (filter (\v' -> not (anyInfix v' affiliationWhitelist)) v) > 1) affiliationURLs

-------------------------------------------------------------------------------------------------------------------------------

sortItemDate :: [MetadataItem] -> [MetadataItem]
sortItemDate = sortBy (flip compare `on` third)

sortItemPathDate :: [(Path,(MetadataItem,String))] -> [(Path,(MetadataItem,String))]
sortItemPathDate = sortBy (flip compare `on` (third . fst . snd))

third :: MetadataItem -> String
third (_,_,rd,_,_,_) = rd

writeYaml :: Path -> MetadataList -> IO ()
writeYaml path yaml = lock $ do
  let newYaml = decodeUtf8 $ Y.encode $ map (\(a,(b,c,d,e,ts,f)) -> let defTag = tag2Default a in (a,b,c,d,e, intercalate " " (filter (/=defTag) ts),f)) $ yaml
  writeUpdatedFile "hakyll-yaml" path newYaml

-- skip all of the checks, validations, tag creation etc
readYamlFast :: Path -> IO MetadataList
readYamlFast yamlp = do file <- B.readFile yamlp
                        let yaml = Y.decodeEither' file :: Either ParseException [[String]]
                        case yaml of
                           Left  e -> error (show e)
                           Right y -> (return $ concatMap convertListToMetadataFast y) :: IO MetadataList
                where
                 convertListToMetadataFast :: [String] -> MetadataList
                 convertListToMetadataFast [u, t, a, d, di,     s] = [(u, (t,a,d,di,tag2TagsWithDefault u "", s))]
                 convertListToMetadataFast [u, t, a, d, di, ts, s] = [(u, (t,a,d,di,tag2TagsWithDefault u ts, s))]
                 convertListToMetadataFast                        e = error $ "Pattern-match failed (too few fields?): " ++ ppShow e

readYaml :: Path -> IO MetadataList
readYaml yaml = do yaml' <- do filep <- doesFileExist yaml
                               if filep then return yaml
                               else do fileAbsoluteP <- doesFileExist ("/home/gwern/wiki/" ++ yaml)
                                       if not fileAbsoluteP then printRed ("YAML path does not exist: " ++ yaml ++ "; refusing to continue. Create an empty or otherwise initialize the file to retry.") >> return yaml
                                       else return ("/home/gwern/wiki/" ++ yaml)
                   file <- Y.decodeFileEither yaml' :: IO (Either ParseException [[String]])
                   allTags <- listTagsAll
                   case file of
                     Left  e -> error $ "File: "++ yaml ++ "; parse error: " ++ ppShow e
                     Right y -> (return $ concatMap (convertListToMetadata allTags) y) :: IO MetadataList
                where
                 convertListToMetadata :: [String] -> [String] -> MetadataList
                 convertListToMetadata allTags' [u, t, a, d, di,     s] = [(stripUnicodeWhitespace u, (t,a,guessDateFromLocalSchema u d,di,map (guessTagFromShort allTags') $ uniqTags $ pages2Tags u $ tag2TagsWithDefault u "", s))]
                 convertListToMetadata allTags' [u, t, a, d, di, ts, s] = [(stripUnicodeWhitespace u, (t,a,guessDateFromLocalSchema u d,di,map (guessTagFromShort allTags') $ uniqTags $ pages2Tags u $ tag2TagsWithDefault u ts, s))]
                 convertListToMetadata _                     e = error $ "Pattern-match failed (too few fields?): " ++ ppShow e
                 stripUnicodeWhitespace :: String -> String
                 stripUnicodeWhitespace = replace "⁄" "/" . filter (not . isSpace)

-- If no accurate date is available, attempt to guess date from the local file schema of 'YYYY-surname-[title, disambiguation, etc].ext' or 'YYYY-MM-DD-...'
-- This is useful for PDFs with bad metadata, or data files with no easy way to extract metadata (like HTML files with hopelessly inconsistent dirty metadata fields like `<meta>` tags) or where it's not yet supported (image files usually have a reliable creation date).
--  > guessDateFromLocalSchema "/doc/ai/2020-10-10-barr.png" ""
-- → "2020-10-10"
-- > guessDateFromLocalSchema "/doc/ai/2020-barr.pdf" ""
-- → "2020"
-- > guessDateFromLocalSchema "http://cnn.com" ""
-- → ""
guessDateFromLocalSchema :: String -> String -> String
guessDateFromLocalSchema url date = if head url /= '/' || date /= "" then date
                                    else let f = takeBaseName url in
                                           if not (head f == '1' || head f == '2') -- I don't have any documents from the future or from <1000 AD, so all viable matches start with '1' or '2', I think...
                                           then date else sed "^([12][0-9][0-9][0-9])(-[0-9][0-9])?(-[0-9][0-9])?-.*" "\\1\\2\\3" f

-- clean a YAML metadata file by sorting & unique-ing it (this cleans up the various appends or duplicates):
rewriteLinkMetadata :: MetadataList -> MetadataList -> Path -> IO ()
rewriteLinkMetadata half full yaml
  = do old <- readYaml yaml
       -- de-duplicate by removing anything in auto.yaml which has been promoted to full/half:
       let (halfURLs,fullURLs) = (map fst half, map fst full)
       let betterURLs = nubOrd (halfURLs ++ fullURLs) -- these *should* not have any duplicates, but...
       let old' = filter (\(p,_) -> p `notElem` betterURLs) old
       let new = M.fromList old' :: Metadata -- NOTE: constructing a Map data structure automatically sorts/dedupes
       let newYaml = decodeUtf8 $ Y.encode $ map (\(a,(b,c,d,e,ts,f)) -> let defTag = tag2Default a in (a,b,c,d,e, intercalate " " (filter (/=defTag) ts),f)) $ -- flatten [(Path, (String, String, String, String, String))]
                     M.toList new
       writeUpdatedFile "yaml" yaml newYaml

-- append (rather than rewrite entirely) a new automatic annotation if its Path is not already in the auto-annotation database:
appendLinkMetadata :: Path -> MetadataItem -> IO ()
appendLinkMetadata l i@(t,a,d,di,ts,abst) = lock $ do printGreen (l ++ " : " ++ ppShow i)
                                                      let newYaml = Y.encode [(l,t,a,d,di, intercalate " " ts,abst)]
                                                      B.appendFile "metadata/auto.yaml" newYaml

--------------------------------------------
-- String munging and processing
--------------------------------------------

-- for link bibliographies / tag pages, better truncate author lists at a reasonable length.
-- (We can make it relatively short because the full author list will be preserved as part of it.)
authorsTruncate :: String -> String
authorsTruncate a = let (before,after) = splitAt 100 a in before ++ (if null after then "" else (head $ split ", " after))

dateTruncateBad :: String -> String
 -- we assume that dates are guaranteed to be 'YYYY[-MM[-DD]]' format because of the validation in readLinkMetadataAndCheck enforcing this
-- dates of the form 'YYYY-01-01' are invariably lies, and mean just 'YYYY'.
dateTruncateBad d = if "-01-01" `isSuffixOf` d then take 4 d else d
