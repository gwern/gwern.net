{- LinkMetadata.hs: module for generating Pandoc links which are annotated with metadata, which can
                    then be displayed to the user as 'popups' by /static/js/popups.js. These popups can be excerpts,
                    abstracts, article introductions etc, and make life much more pleasant for the reader—hover over
                    link, popup, read, decide whether to go to link.
Author: Gwern Branwen
Date: 2019-08-20
When:  Time-stamp: "2022-11-30 12:59:17 gwern"
License: CC-0
-}

-- TODO:
-- 1. bugs in packages: rxvist doesn't appear to support all bioRxiv/medRxiv schemas, including the
-- '/early/' links, forcing me to use curl+Tagsoup; the R library 'fulltext' crashes on examples
-- like `ft_abstract(x = c("10.1038/s41588-018-0183-z"))`

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module LinkMetadata (addPageLinkWalk, isPagePath, readLinkMetadata, readLinkMetadataAndCheck, walkAndUpdateLinkMetadata, updateGwernEntries, writeAnnotationFragments, Metadata, MetadataItem, MetadataList, readYaml, readYamlFast, writeYaml, annotateLink, createAnnotations, hasAnnotation, parseRawBlock, parseRawInline, generateAnnotationBlock, generateAnnotationTransclusionBlock, authorsToCite, authorsTruncate, cleanAbstractsHTML, sortItemDate, sortItemPathDate, warnParagraphizeYAML, simplifiedHTMLString, tooltipToMetadata, dateTruncateBad) where

import Control.Monad (unless, void, when, foldM_)
import Data.Aeson (eitherDecode, FromJSON)
import qualified Data.ByteString as B (appendFile, readFile)
import qualified Data.ByteString.Lazy as BL (length, concat)
import qualified Data.ByteString.Lazy.UTF8 as U (toString) -- TODO: why doesn't using U.toString fix the Unicode problems?
import Data.Char (isPunctuation)
import qualified Data.Map.Strict as M (elems, filter, filterWithKey, fromList, fromListWith, keys, toList, lookup, map, union) -- traverseWithKey, union, Map
import qualified Data.Text as T (append, breakOnAll, head, isInfixOf, isPrefixOf, pack, unpack, Text)
import Data.Containers.ListUtils (nubOrd)
import Data.IORef (IORef)
import Data.FileStore.Utils (runShellCommand)
import Data.Function (on)
import Data.List (intercalate, intersect, isInfixOf, isPrefixOf, isSuffixOf, nub, sort, sortBy, (\\))
import Data.List.HT (search)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8) -- ByteString -> T.Text
import Data.Yaml as Y (decodeFileEither, decodeEither', encode, ParseException) -- NOTE: from 'yaml' package, *not* 'HsYaml'
import GHC.Generics (Generic)
import Network.HTTP (urlDecode, urlEncode)
import Network.URI (isURIReference)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.Exit (ExitCode(ExitFailure))
import System.FilePath (takeDirectory, takeExtension, takeFileName, takeBaseName)
import System.GlobalLock (lock)
import Text.HTML.TagSoup (isTagCloseName, isTagOpenName, parseTags, renderOptions, renderTags, renderTagsOptions, Tag(TagClose, TagOpen, TagText))
import Text.Pandoc (readerExtensions, writerWrapText, writerHTMLMathMethod, Inline(Link, Span), HTMLMathMethod(MathJax),
                    defaultMathJaxURL, def, readLaTeX, readMarkdown, writeHtml5String, WrapOption(WrapNone), runPure, pandocExtensions,
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
import Image (invertImage, imageSrcset, addImgDimensions)
import LinkArchive (localizeLink, ArchiveMetadata)
import LinkAuto (linkAutoHtml5String)
import LinkBacklink (getSimilarLinkCheck, getSimilarLinkCount, getBackLinkCount, getBackLinkCheck, getLinkBibLinkCheck, getAnnotationLink)
import LinkID (authorsToCite, generateID)
import LinkLive (linkLive)
import LinkMetadataTypes (Metadata, MetadataItem, Path, MetadataList)
import Query (truncateTOCHTML, extractLinksInlines)
import Tags (abbreviateTag, uniqTags, guessTagFromShort, tag2TagsWithDefault, guessTagFromShort, tag2Default, pages2Tags, listTagsAll, tagsToLinksSpan, tagPairsCount, tagCount, tagMax, tagPairMax, listTagDirectories)
import Utils (writeUpdatedFile, printGreen, printRed, fixedPoint, sed, sedMany, replaceMany, toMarkdown, trim, simplified, anyInfix, anyPrefix, anySuffix, replace, split, anyPrefixT, hasAny, safeHtmlWriterOptions, addClass)

-- Should the current link get a 'G' icon because it's an essay or regular page of some sort?
-- we exclude several directories (docs/, static/, images/) entirely; a Gwern.net page is then any
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
isPagePath f = let f' = replace "https://www.gwern.net" "" $ T.unpack f in
    (if
        not ("/" `isPrefixOf` f') ||
      ("/images/" `isPrefixOf` f' || "/static/" `isPrefixOf` f')
     then False else
       takeExtension f' == "")

-------------------------------------------------------------------------------------------------------------------------------

-- Run an arbitrary function on the 3 databases to update individual items.
-- For example, to use `processDOIArxiv` to add inferred-DOIs to all Arxiv annotations prior to Arxiv adding official DOIs, one could run a command like:
--
-- > walkAndUpdateLinkMetadata (\x@(path,(title,author,date,doi,tags,abstrct)) -> if not ("https://arxiv.org" `isPrefixOf` path) || (doi /= "") then return x else return (path,(title,author,date,processDOIArxiv path,tags,abstrct)))
--
-- To rewrite a tag, eg 'conscientiousness' → 'psychology/personality/conscientiousness':
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
  where gwernEntries path = ("/" `isPrefixOf` path || "https://www.gwern.net" `isPrefixOf` path) && not ("." `isInfixOf` path || "#manual-annotation" `isInfixOf` path)

-- eg to rescrape a specific abstract: `rescrapeYAML (\p -> p == "/notes/Attention") "metadata/half.yaml"`
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
             custom  <- readYaml "metadata/full.yaml"  -- for hand created definitions, to be saved; since it's handwritten and we need line errors, we use YAML:
             half <- readYaml "metadata/half.yaml" -- tagged but not handwritten/cleaned-up
             auto    <- readYaml "metadata/auto.yaml"    -- auto-generated cached definitions; can be deleted if gone stale
             -- merge the hand-written & auto-generated link annotations, and return:
             let final = M.union (M.fromList custom) $ M.union (M.fromList half) (M.fromList auto) -- left-biased, so 'custom' overrides 'half' overrides 'half' overrides 'auto'
             return final

-- read the annotation database, and do extensive semantic & syntactic checks for errors/duplicates:
readLinkMetadataAndCheck :: IO Metadata
readLinkMetadataAndCheck = do
             -- for hand created definitions, to be saved; since it's handwritten and we need line errors, we use YAML:
             custom <- readYaml "metadata/full.yaml"

             -- Quality checks:
             -- requirements:
             -- - URLs/keys must exist, be unique, and either be a remote URL (starting with 'h') or a local filepath (starting with '/') which exists on disk (auto.yaml may have stale entries, but full.yaml should never! This indicates a stale annotation, possibly due to a renamed or accidentally-missing file, which means the annotation can never be used and the true URL/filepath will be missing the hard-earned annotation). We strip http/https because so many websites now redirect and that's an easy way for duplicate annotations to exist.
             -- - titles must exist & be unique (overlapping annotations to pages are disambiguated by adding the section title or some other description)
             -- - authors must exist (if only as 'Anonymous' or 'N/A'), but are non-unique
             -- - dates are non-unique & optional/NA for always-updated things like Wikipedia. If they exist, they should be of the format 'YYYY[-MM[-DD]]'.
             -- - DOIs are optional since they usually don't exist, and non-unique (there might be annotations for separate pages/anchors for the same PDF and thus same DOI; DOIs don't have any equivalent of `#page=n` I am aware of unless the DOI creator chose to mint such DOIs, which they never (?) do). DOIs sometimes use hyphens and so are subject to the usual problems of em/en-dashes sneaking in by 'smart' systems screwing up.
             -- - tags are optional, but all tags should exist on-disk as a directory of the form "docs/$TAG/"
             -- - annotations must exist and be unique inside full.yaml (overlap in auto.yaml can be caused by the hacky appending); their HTML should pass some simple syntactic validity checks
             let urlsC = map fst custom
             let normalizedUrlsC = map (replace "https://" "" . replace "http://" "") urlsC
             when (length (nub (sort normalizedUrlsC)) /=  length normalizedUrlsC) $ error $ "full.yaml: Duplicate URLs! " ++ unlines (normalizedUrlsC \\ nubOrd normalizedUrlsC)

             let tagsAllC = nubOrd $ concatMap (\(_,(_,_,_,_,ts,_)) -> ts) custom

             let badDoisDash = filter (\(_,(_,_,_,doi,_,_)) -> anyInfix doi ["–", "—", " ", ",", "{", "}", "!", "@", "#", "$", "\"", "'"] || "http" `isInfixOf` doi) custom in
                 unless (null badDoisDash) $ error $ "full.yaml: Bad DOIs (invalid punctuation in DOI): " ++ show badDoisDash
             -- about the only requirement for DOIs, aside from being made of graphical Unicode characters (which includes spaces <https://www.compart.com/en/unicode/category/Zs>!), is that they contain one '/': https://www.doi.org/doi_handbook/2_Numbering.html#2.2.3 "The DOI syntax shall be made up of a DOI prefix and a DOI suffix separated by a forward slash. There is no defined limit on the length of the DOI name, or of the DOI prefix or DOI suffix. The DOI name is case-insensitive and can incorporate any printable characters from the legal graphic characters of Unicode." https://www.doi.org/doi_handbook/2_Numbering.html#2.2.1
             -- Thus far, I have not run into any real DOIs which omit numbers, so we'll include that as a check for accidental tags inserted into the DOI field.
             let badDois = filter (\(_,(_,_,_,doi,_,_)) -> if (doi == "") then False else doi `elem` tagsAllC || head doi `elem` ['a'..'z'] || '/' `notElem` doi || null ("0123456789" `intersect` doi)) custom in
               unless (null badDois) $ error $ "full.yaml: Invalid DOI (missing mandatory forward slash or a number): " ++ show badDois

             let emptyCheck = filter (\(u,(t,a,_,_,_,s)) ->  "" `elem` [u,t,a,s]) custom
             unless (null emptyCheck) $ error $ "full.yaml: Link Annotation Error: empty mandatory fields! [URL/title/author/abstract] This should never happen: " ++ show emptyCheck

             let annotations = map (\(_,(_,_,_,_,_,s)) -> s) custom in
               when (length (nub (sort annotations)) /= length annotations) $ error $
               "full.yaml:  Duplicate annotations: " ++ unlines (annotations \\ nubOrd annotations)

             -- intermediate link annotations: not finished, like 'full.yaml' entries, but also not fully auto-generated.
             -- This is currently intended for storing entries for links which I give tags (probably as part of creating a new tag & rounding up all hits), but which are not fully-annotated; I don't want to delete the tag metadata, because it can't be rebuilt, but such half annotations can't be put into 'full.yaml' without destroying all of the checks' validity.
             half <- readYaml "metadata/half.yaml"
             let (customPaths,halfPaths) = (map fst custom, map fst half)
             let redundantHalfs = customPaths `intersect` halfPaths
             unless (null redundantHalfs) (printRed "Redundant entries in half.yaml & full.yaml: " >> printGreen (show redundantHalfs))

             let urlsCP = map fst (custom ++ half)
             let files = map (takeWhile (/='#') . tail) $ filter (\u -> head u == '/') urlsCP
             Par.mapM_ (\f -> let f' = if '.' `elem` f then f else f ++ ".page" in
                                    do exist <- doesFileExist f'
                                       unless exist $ printRed ("Custom annotation error: file does not exist? " ++ f ++ " (checked file name: " ++ f' ++ ")")) files

             -- auto-generated cached definitions; can be deleted if gone stale
             rewriteLinkMetadata half custom "metadata/auto.yaml" -- do auto-cleanup  first
             auto <- readYaml "metadata/auto.yaml"
             -- merge the hand-written & auto-generated link annotations, and return:
             let final = M.union (M.fromList custom) $ M.union (M.fromList half) (M.fromList auto) -- left-biased, so 'custom' overrides 'half' overrides 'auto'
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

             -- check validity of all external links:
             let urlsAll = filter (\(x@(u:_),_) -> if u `elem` ['/', '!', '$', '\8383'] ||
                                                      "wikipedia.org" `isInfixOf` x || "hoogle.haskell.org" `isInfixOf` x || not ("ttps://" `isPrefixOf` x || "ttp://" `isPrefixOf` x || "/wiki" `isPrefixOf` x || "wiki/" `isPrefixOf` x) then False
                                                 else not (isURIReference x)) finalL
             unless (null urlsAll) $ printRed "Invalid URIs?" >> printGreen (ppShow urlsAll)

             -- look for duplicates due to missing affiliation:
             let urlsDuplicateAffiliation = findDuplicatesURLsByAffiliation final
             unless (null urlsDuplicateAffiliation) $ printRed "Duplicated URLs by affiliation:" >> printGreen (show urlsDuplicateAffiliation)

             let titlesSimilar = sort $ map (\(u,(t,_,_,_,_,_)) -> (u, t)) $ filter (\(u,_) -> '.' `elem` u && not ("wikipedia.org" `isInfixOf` u)) $ M.toList final
             let titles = filter (\title -> length title > 10) $ map snd titlesSimilar
             unless (length (nubOrd titles) == length titles) $ printRed  "Duplicate titles in YAMLs!: " >> printGreen (show (titles \\ nubOrd titles))

             let authors = map (\(_,(_,aut,_,_,_,_)) -> aut) finalL
             Par.mapM_ (\a -> unless (null a) $ when (a =~ dateRegex) (printRed $ "Mixed up author & date?: " ++ a) ) authors
             let authorsBadChars = filter (\a -> anyInfix a [";", "&", "?", "!"] || isPunctuation (last a)) authors
             unless (null authorsBadChars) (printRed "Mangled author list?" >> printGreen (ppShow authorsBadChars))

             let datesBad = filter (\(_,(_,_,dt,_,_,_)) -> not (dt =~ dateRegex || null dt)) finalL
             unless (null datesBad) (printRed "Malformed date (not 'YYYY[-MM[-DD]]'): ") >> printGreen (show datesBad)

             -- 'filterMeta' may delete some titles which are good; if any annotation has a long abstract, all data sources *should* have provided a valid title. Enforce that.
             let titlesEmpty = M.filter (\(t,_,_,_,_,abst) -> t=="" && length abst > 100) final
             unless (null titlesEmpty) $ error ("Link Annotation Error: missing title despite abstract!" ++ show titlesEmpty)

             let tagIsNarrowerThanFilename = M.map (\(title,_,_,_,tags,_) -> (title,tags)) $ M.filterWithKey (\f (_,_,_,_,tags,_) -> if not ("/docs/" `isPrefixOf` f) then False else
                                                        let fileTag = replace "/docs/" "" $ takeDirectory f
                                                         in any ((fileTag++"/") `isPrefixOf`) tags) final
             unless (null tagIsNarrowerThanFilename) $ printRed "Files whose tags are more specific than their path: " >> printGreen (unlines $ map (\(f',(t',tag')) -> t' ++ " : " ++ f' ++ " " ++ unwords tag') $ M.toList tagIsNarrowerThanFilename)

             -- check tags (not just custom but all of them, including halfs)
             let tagsSet = sort $ nubOrd $ concat $ M.elems $ M.map (\(_,_,_,_,tags,_) -> tags) $ M.filter (\(t,_,_,_,_,_) -> t /= "") final
             tagsAll <- listTagsAll
             let tagsBad = tagsSet \\ tagsAll
             let annotationsWithBadTags = M.filter (\(_,_,_,_,ts,_) -> hasAny ts tagsBad) final
             unless (null annotationsWithBadTags) $ error $ "Link Annotation Error: tag does not match a directory! Bad annotations: " ++ show annotationsWithBadTags

             let tagsOverused = filter (\(c,_) -> c > tagMax) $ tagCount final
             unless (null tagsOverused) $ printRed "Overused tags: " >> printGreen (show tagsOverused)

             let tagPairsOverused = filter (\(c,_) -> c > tagPairMax) $ tagPairsCount final
             unless (null tagPairsOverused) $ printRed "Overused pairs of tags: " >> printGreen (show tagPairsOverused)

             -- See-Alsos in annotations get put in multi-columns due to their typical length, but if I cut them down to 1–2 items, the default columns will look bad. `preprocess-markdown.hs` can't do a length check because it has no idea how I will edit the list of similar-links down, so I can't remove the .columns class *there*; only way to do it is check finished annotations for having .columns set but also too few similar-links:
             let badSeeAlsoColumnsUse = M.keys $ M.filterWithKey (\_ (_,_,_,_,_,abst) -> let count = length (Data.List.HT.search "data-embeddingdistance" abst) in (count == 1 || count == 2) && "<div class=\"columns\">" `isInfixOf` abst ) final
             unless (null badSeeAlsoColumnsUse) $ printRed "Remove columns from skimpy See-Also annotations: " >> printGreen (show badSeeAlsoColumnsUse)

             return final

dateRegex, footnoteRegex, sectionAnonymousRegex, badUrlRegex :: String
dateRegex             = "^[1-2][0-9][0-9][0-9](-[0-2][0-9](-[0-3][0-9])?)?$"
footnoteRegex         = "^/?[[:alnum:]-]+#fn[1-9][0-9]*$" -- '/Foo#fn3', 'Foo#fn1', 'Foo-Bar-2020#fn999' etc
sectionAnonymousRegex = "^#section-[0-9]+$" -- unnamed sections which receive Pandoc positional auto-names like "#section-1", "#section-15"; unstable, should be named if ever to be linked to, etc.
badUrlRegex           = "http.*http|docs/.*docs/"::String

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
                                          p' <- walkM (localizeLink am archived) p
                                          walkM imageSrcset p' -- add 'srcset' HTML <img> property - helps toggle between the small/large image versions for mobile vs desktop
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
     -- normalize: convert 'https://www.gwern.net/docs/foo.pdf' to '/docs/foo.pdf' and './docs/foo.pdf' to '/docs/foo.pdf'
     -- the leading '/' indicates this is a local Gwern.net file
     let target' = replace "https://www.gwern.net/" "/" target
     let target'' = if head target' == '.' then drop 1 target' else target'

     -- check local link validity: every local link except tags should exist on-disk:
     when (head target'' == '/' && not ("/metadata/annotations/" `isPrefixOf` target'')) $
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
  if hasAny ["link-annotated-not", "id-not", "link-annotated", "link-annotated-partial"] classes then y
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
      -- TEMPORARY: we do not set '.link-annotated-partial' on local links, even when they are a normal partial link, as a compromise due to the weakness of 'partial' popups right now. They only popup the partial itself (so, maybe a title & backlinks, a tag, perhaps an author or date, and that's about it); for a local PDF, that's inferior to popping up the PDF itself, because you generally wouldn't learn much from the partial popup compared to popping up the full PDF, and adds an annoying extra mouse-search to every popup. Bad. So, instead, we just ignore partial status for local links. Ideally, the partial popup would pop up the full PDF, with a wrapper frame containing just the partial metadata. Then the partial popup is no worse than the non-partial popup, and usually better. (Once that is added, the link-page boolean can be removed.)
      else if "link-page" `elem` b || T.head f == '/' then
             x' else -- may be a partial...
             -- no, a viable partial would have a (short) fragment written out, see `writeAnnotationFragment` logic
             if not $ unsafePerformIO $ doesFileExist $ fst $ getAnnotationLink $ T.unpack f then x'
             else -- so it's not a local link, doesn't have a full annotation, doesn't have an on-demand annotation like a Wikipedia article, but does have *some* partial annotation since it exists on disk, so it gets `.link-annotated-partial`
               addClass "link-annotated" $ addClass "link-annotated-partial" x'
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
                                                                            else "<div class=\"collapse\">" `T.append`
                                                                                 ((if blp=="" then "" else ("<div class=\"backlinks-append aux-links-append\"" `T.append` " id=\"" `T.append` lidBacklinkFragment `T.append` "\" " `T.append` ">\n<p>[<a class=\"include-strict include-replace-container\" href=\"" `T.append` T.pack blp `T.append` "\">Backlinks for this annotation</a>.]</p>\n</div>")) `T.append`
                                                                                  (if slp=="" then "" else ("<div class=\"similars-append aux-links-append\"" `T.append` " id=\"" `T.append` lidSimilarLinkFragment `T.append` "\" " `T.append` ">\n<p>[<a class=\"include-strict include-replace-container\" href=\"" `T.append` T.pack slp `T.append` "\">Similar links for this annotation</a>.]</p>\n</div>")) `T.append`
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

-- annotations, like /Faces, often link to specific sections or anchors, like 'I clean the data with [Discriminator Ranking](#discriminator-ranking)'; when transcluded into other pages, these links are broken. But we don't want to rewrite the original abstract as `[Discriminator Ranking](/Faces#discriminator-ranking)` to make it absolute, because that screws with section-popups/link-icons! So instead, when we write out the body of each annotation inside the link bibliography, while we still know what the original URL was, we traverse it looking for any links starting with '#' and rewrite them to be absolute:
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
  let newYaml = decodeUtf8 $ Y.encode $ map (\(a,(b,c,d,e,ts,f)) -> let defTag = tag2Default a in (a,b,c,d,e, intercalate ", " (filter (/=defTag) ts),f)) $ yaml
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
                                       if not fileAbsoluteP then printRed ("YAML path does not exist: " ++ yaml) >> return yaml
                                       else return ("/home/gwern/wiki/" ++ yaml)
                   file <- Y.decodeFileEither yaml' :: IO (Either ParseException [[String]])
                   allTags <- listTagsAll
                   case file of
                     Left  e -> error $ "File: "++ yaml ++ "; parse error: " ++ ppShow e
                     Right y -> (return $ concatMap (convertListToMetadata allTags) y) :: IO MetadataList
                where
                 convertListToMetadata :: [String] -> [String] -> MetadataList
                 convertListToMetadata allTags' [u, t, a, d, di,     s] = [(u, (t,a,guessDateFromLocalSchema u d,di,map (guessTagFromShort allTags') $ uniqTags $ pages2Tags u $ tag2TagsWithDefault u "", s))]
                 convertListToMetadata allTags' [u, t, a, d, di, ts, s] = [(u, (t,a,guessDateFromLocalSchema u d,di,map (guessTagFromShort allTags') $ uniqTags $ pages2Tags u $ tag2TagsWithDefault u ts, s))]
                 convertListToMetadata _                     e = error $ "Pattern-match failed (too few fields?): " ++ ppShow e

-- If no accurate date is available, attempt to guess date from the local file schema of 'YYYY-surname-[title, disambiguation, etc].ext' or 'YYYY-MM-DD-...'
-- This is useful for PDFs with bad metadata, or data files with no easy way to extract metadata (like HTML files with hopelessly inconsistent dirty metadata fields like `<meta>` tags) or where it's not yet supported (image files usually have a reliable creation date).
--  > guessDateFromLocalSchema "/docs/ai/2020-10-10-barr.png" ""
-- → "2020-10-10"
-- > guessDateFromLocalSchema "/docs/ai/2020-barr.pdf" ""
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
rewriteLinkMetadata half custom yaml
  = do old <- readYaml yaml
       -- de-duplicate by removing anything in auto.yaml which has been promoted to custom/half:
       let (halfURLs,customURLs) = (map fst half, map fst custom)
       let betterURLs = nubOrd (halfURLs ++ customURLs) -- these *should* not have any duplicates, but...
       let old' = filter (\(p,_) -> p `notElem` betterURLs) old
       let new = M.fromList old' :: Metadata -- NOTE: constructing a Map data structure automatically sorts/dedupes
       let newYaml = decodeUtf8 $ Y.encode $ map (\(a,(b,c,d,e,ts,f)) -> let defTag = tag2Default a in (a,b,c,d,e, intercalate ", " (filter (/=defTag) ts),f)) $ -- flatten [(Path, (String, String, String, String, String))]
                     M.toList new
       writeUpdatedFile "yaml" yaml newYaml

-- append (rather than rewrite entirely) a new automatic annotation if its Path is not already in the auto-annotation database:
appendLinkMetadata :: Path -> MetadataItem -> IO ()
appendLinkMetadata l i@(t,a,d,di,ts,abst) = lock $ do printGreen (l ++ " : " ++ ppShow i)
                                                      let newYaml = Y.encode [(l,t,a,d,di, intercalate ", " ts,abst)]
                                                      B.appendFile "metadata/auto.yaml" newYaml

data Failure = Temporary | Permanent deriving Show

linkDispatcher :: Inline -> IO (Either Failure (Path, MetadataItem))
linkDispatcher (Link _ _ (l, tooltip)) = do l' <- linkDispatcherURL (T.unpack l)
                                            case l' of
                                              Right _ -> return l'
                                              Left Permanent -> let (title,author,date) = tooltipToMetadata (T.unpack l) (T.unpack tooltip) in
                                                                  if title/="" then return (Right ((T.unpack l),(title,author,date,"",[],""))) else return l'
                                              Left Temporary -> return l'
linkDispatcher x = error ("linkDispatcher passed a non-Link Inline element: " ++ show x)
linkDispatcherURL :: Path -> IO (Either Failure (Path, MetadataItem))
linkDispatcherURL l | anyPrefix l ["/metadata/annotations/backlinks/", "/metadata/annotations/similars/", "/docs/www/"] = return (Left Permanent)
                 -- WP is now handled by annotations.js calling the Mobile WP API; we pretty up the title for tags.
                 | "https://en.wikipedia.org/wiki/" `isPrefixOf` l = return $ Right (l, (wikipediaURLToTitle l, "", "", "", [], ""))
                 | "https://arxiv.org/abs/" `isPrefixOf` l = arxiv l
                 | "https://openreview.net/forum?id=" `isPrefixOf` l || "https://openreview.net/pdf?id=" `isPrefixOf` l = openreview l
                 | anyPrefix l ["https://www.biorxiv.org/content/", "https://www.medrxiv.org/content/"] = biorxiv l
                 | "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC" `isPrefixOf` l = pubmed l
                     -- WARNING: this is not a complete list of PLOS domains, just the ones currently used on Gwern.net; didn't see a complete list anywhere...
                 | anyInfix l ["journals.plos.org", "plosbiology.org", "ploscompbiology.org", "plosgenetics.org", "plosmedicine.org", "plosone.org"] = pubmed l
                 | null l = return (Left Permanent)
                 -- locally-hosted PDF?
                 | ".pdf" `isInfixOf` l = let l' = linkCanonicalize l in if head l' == '/' then pdf $ tail l' else return (Left Permanent)
                 | otherwise = let l' = linkCanonicalize l in if head l' == '/' then gwern $ tail l
                 -- And everything else is unhandled:
                    else return (Left Permanent)

-- Attempt to parse tooltips back into citation metadata:
tooltipToMetadata :: String -> String -> (String,String,String)
tooltipToMetadata _ "" = ("","","")
tooltipToMetadata path s | (head s) `elem` ['/', '!', '$', '\8383'] || anyInfix s ["Original URL:"] = ("","","")
                    | otherwise =
                        let title  = filterMeta $ sed "['‘“](.*)['’”], .*" "\\1" s
                            author = filterMeta $  sed "^Branwen$" "Gwern Branwen" $ replace " et al" ", et al" $ replace " & " ", " $
                                     sed "^([A-Z].*) [0-9][0-9][0-9][0-9][0-9-]*[a-z]?$" "\\1" $
                                     sed "['‘“].*['’”], ([A-Z].+) [0-9][0-9][0-9][0-9][0-9-]*[a-z]?" "\\1" s
                            date   = filterMeta $ sed "^[A-Za-z].+ ([0-9][0-9][0-9][0-9][0-9-]*)[a-z]?$" "\\1" $ sed "['‘“].+['’”], [A-Za-z].+ ([0-9][0-9][0-9][0-9][0-9-]*)[a-z]?$" "\\1" s
                            pageNumber = let n = pageNumberParse path in if null n then "" else " § pg" ++ n
                            (a,b,c) = ((minLength 6 $ changed title)++pageNumber, changed author, minLength 4 $ changed date) in
                          if a==b && b==c then (s,"","") else (a,b,c)
                    where changed x = if s==x then "" else x
                          minLength n x = if length x < n then "" else x
-- tooltipToMetadataTest :: [(String,(String,String,String))]
-- tooltipToMetadataTest = filter (\(t1, t2) -> tooltipToMetadata "" t1 /= t2)
--     [("‘Title1 Title2's First Word Title3’, Foo et al 2020a",    ("Title1 Title2's First Word Title3","Foo, et al","2020"))
--       , ("“Title1 Title2's First Word Title3”, Foo et al 2020a", ("Title1 Title2's First Word Title3","Foo, et al","2020"))
--       , ("'Title1 Title2's First Word Title3', Foo & Bar 2020a", ("Title1 Title2's First Word Title3","Foo, Bar","2020"))
--       , ("'Title1 Title2's First Word Title3', Foo 2020a",       ("Title1 Title2's First Word Title3","Foo","2020"))
--       , ("'Title1 Title2's First Word Title3', John Smith 2020", ("Title1 Title2's First Word Title3","John Smith","2020"))
--       , ("'Montaillou: The Promised Land of Error: chapter 2, the <em>domus</em>', Le Roy Ladurie 1978", ("Montaillou: The Promised Land of Error: chapter 2, the <em>domus</em>", "Le Roy Ladurie", "1978"))
--       , ("'Meta-meta-blinker', Adam P. Goucher 2016-12-15", ("Meta-meta-blinker", "Adam P. Goucher", "2016-12-15"))
--       , ("'Formal Theory of Creativity & Fun & Intrinsic Motivation (1990-2010)', Jurgen Schmidhuber 2010", ("Formal Theory of Creativity & Fun & Intrinsic Motivation (1990-2010)", "Jurgen Schmidhuber", "2010"))
--       , ( "$5",      ("","",""))
--       , ( "$20, 2g", ("","",""))
--       , ("!W",       ("","",""))
--       , ("₿20",      ("","",""))
--       , ("'LaMDA: Language Models for Dialog Applications', Thoppilan?et?al?2022 (Original URL: https://arxiv.org/abs/2201.08239#google )", ("","",""))
--       , ("'A', John Smith 2020", ("","John Smith","2020"))
--       , ("klynch 2011",     ("","","2011"))
--       , ("Foo 2020",        ("", "Foo", "2020"))
--       , ("Foo 2020-06-12",  ("", "Foo", "2020-06-12"))
--       , ("John Smith 2020", ("", "John Smith", "2020"))
--       ]

gwern, arxiv, biorxiv, pubmed, openreview :: Path -> IO (Either Failure (Path, MetadataItem))
-- handles both PM & PLOS right now:
pubmed l = do checkURL l
              (status,_,mb) <- runShellCommand "./" Nothing "Rscript" ["static/build/linkAbstract.R", l]
              case status of
                ExitFailure err -> printGreen (intercalate " : " [l, ppShow status, ppShow err, ppShow mb]) >> return (Left Permanent)
                _ -> do
                        let parsed = lines $ replace " \n" "\n" $ trim $ U.toString mb
                        if length parsed < 5 then return (Left Permanent) else
                          do let (title:author:date:doi:abstrct) = parsed
                             let ts = [] -- TODO: replace with ML call to infer tags
                             abstract' <- fmap linkAutoHtml5String $ processParagraphizer l $ processPubMedAbstract $ unlines abstrct
                             return $ Right (l, (cleanAbstractsHTML $ trimTitle title, initializeAuthors $ trim author, trim date, trim $ processDOI doi, ts, abstract'))

 -- eg "foo.pdf#page=50&org=openai" → "50"; "foo.pdf" → ""
pageNumberParse :: String -> String
pageNumberParse u = let pg = sed ".*\\.pdf#page=([0-9]+).*" "\\1" u
                    in if u == pg then "" else pg

pdf :: Path -> IO (Either Failure (Path, MetadataItem))
pdf "" = error "Fatal error: LinkMetadata.pdf called on empty string argument; this should never happen."
pdf p = do let p' = takeWhile (/='#') $ if head p == '/' then tail p else p
           existsp <- doesFileExist p'
           unless existsp $ error $ "PDF file doesn't exist? Tried to query " ++ p
           let pageNumber = pageNumberParse p
           let pageNumber' = if pageNumber == p then "" else pageNumber

           (_,_,mbTitle)  <- runShellCommand "./" Nothing "exiftool" ["-printFormat", "$Title",  "-Title",  p']
           (_,_,mbAuthor) <- runShellCommand "./" Nothing "exiftool" ["-printFormat", "$Author", "-Author", p']
           (_,_,mbCreator)<- runShellCommand "./" Nothing "exiftool" ["-printFormat", "$Creator", "-Creator", p']
           (_,_,mbDate)   <- runShellCommand "./" Nothing "exiftool" ["-printFormat", "$Date",    "-dateFormat", "%F", "-Date", p']
           (_,_,mbDoi)    <- runShellCommand "./" Nothing "exiftool" ["-printFormat", "$DOI",     "-DOI", p']
           if BL.length (BL.concat [mbTitle,mbAuthor,mbDate,mbDoi]) > 0 then
             do printGreen (show [mbTitle,mbCreator,mbAuthor,mbDate,mbDoi])
                let title = (filterMeta $ trimTitle $ cleanAbstractsHTML $ U.toString mbTitle) ++ (if null pageNumber' then "" else " § pg" ++ pageNumber')
                let edoi = trim $ U.toString mbDoi
                let edoi' = if null edoi then "" else processDOI edoi
                -- PDFs have both a 'Creator' and 'Author' metadata field sometimes. Usually Creator refers to the (single) person who created the specific PDF file in question, and Author refers to the (often many) authors of the content; however, sometimes PDFs will reverse it: 'Author' means the PDF-maker and 'Creators' the writers. If the 'Creator' field is longer than the 'Author' field, then it's a reversed PDF and we want to use that field instead of omitting possibly scores of authors from our annotation.
                let ecreator = filterMeta $ U.toString mbCreator
                let eauthor' = filterMeta  $ U.toString mbAuthor
                let author = linkAutoHtml5String $ cleanAbstractsHTML $ initializeAuthors $ trim $ if length eauthor' > length ecreator then eauthor' else ecreator
                let ts = [] -- TODO: replace with ML call to infer tags
                printGreen $ "PDF: " ++ p ++" DOI: " ++ edoi'
                at <- fmap (fromMaybe "") $ doi2Abstract edoi'
                if length (title ++ author ++ U.toString mbDate ++ edoi') > 0 then
                  return $ Right (p, (title, author, trim $ replace ":" "-" (U.toString mbDate), edoi', ts, at))
                  else
                  return (Left Permanent)
                -- if there is no abstract, there's no point in displaying title/author/date since that's already done by tooltip+URL:
                -- case aMaybe of
                --   Nothing -> return (Left Permanent)
                --   Just a -> return $ Right (p, (trimTitle etitle, author, trim $ replace ":" "-" edate, edoi', a))
           else printRed "PDF annotation failed, insufficient data or unreadable file; exiftool returned: " >> putStrLn ("title/author/date: " ++ show mbTitle ++ " ; DOI: " ++ show mbDoi) >> return (Left Permanent)

filterMeta :: String -> String
filterMeta ea = if anyInfix ea badSubstrings || elem ea badWholes then "" else ea
 where badSubstrings, badWholes :: [String]
       badSubstrings = ["ABBYY", "Adobe", "InDesign", "Arbortext", "Unicode", "Total Publishing", "pdftk", "aBBYY", "FineReader", "LaTeX", "hyperref", "Microsoft", "Office Word", "Acrobat", "Plug-in", "Capture", "ocrmypdf", "tesseract", "Windows", "JstorPdfGenerator", "Linux", "Mozilla", "Chromium", "Gecko", "QuarkXPress", "LaserWriter", "AppleWorks", "PDF", "Apache", ".tex", ".tif", "2001", "2014", "3628", "4713", "AR PPG", "ActivePDF", "Administrator", "Administratör", "American Association for the Advancement of Science", "Appligent", "BAMAC6", "CDPUBLICATIONS", "CDPublications", "Chennai India", "Copyright", "DesktopOperator", "Emacs", "G42", "GmbH", "IEEE", "Image2PDF", "J-00", "JN-00", "LSA User", "LaserWriter", "Org-mode", "PDF Generator", "PScript5.dll", "PageMaker", "PdfCompressor", "Penta", "Preview", "PrimoPDF", "PrincetonImaging.com", "Print Plant", "QuarkXPress", "Radical Eye", "RealPage", "SDK", "SYSTEM400", "Sci Publ Svcs", "Scientific American", "Springer", "TIF", "Unknown", "Utilities", "XPP", "apark", "bhanson", "cairo 1", "cairographics.org", "dvips", "easyPDF", "eguise", "epfeifer", "fdz", "ftfy", "gscan2pdf", "jsalvatier", "jwh1975", "kdx", "pdf", " OVID ", "imogenes", "firefox", "Firefox", "Mac1", "EBSCO", "faculty.vp", ".book", "PII", "Typeset", ".pmd", "affiliations", "list of authors", ".doc", "untitled", "Untitled", "FrameMaker", "PSPrinter", "qxd", "INTEGRA", "Xyvision", "CAJUN", "PPT Extended", "Secure Data Services", "MGS V", "mgs;", "COPSING", "- AAAS", "Science Journals", "Serif Affinity", "Google Analytics", "rnvb085", ".indd", "hred_", "penta@", "WorkStation", "ORDINATO+", ":Gold:", "XeTeX", "Aspose", "Abbyy", "Archetype Publishing Inc.", "AmornrutS", "OVID-DS", "PAPER Template", "IATED", "TECHBOOKS", "Word 6.01", "TID Print Plant", "8.indd", "pdftk-java", "OP-ESRJ", "FUJIT S. U.", "JRC5", "klynch", "pruich", "Micron", "Anonymous Submission", "Asterisk", "KBarry2", ",-0", "fi-5530C2dj", "FUJIT S. U.", "LEVET_Layout", "Digitized by the ", "shaniahl", ".orig.pdf", ".dvi", ".tif", ".qxd", ".ps", "doi:10", "DOI", ".tmp", ".pdf", ".eps", ".PDF", ".indd", "APA template", "Abbyy", "Author Guidelines", "156x234mm", "C:\\", "D:\\", "CUP_JBS", ".vp", ".wpd", "EBSCOhost", ".doc", ".docx", ".qxp", "PDF_Banner", ".pmd", "MergedFile", "No Job Name", "PII: ", "ProQuest Dissertation", "ScanGate", "Science Journals", "Science Magazine", "Untitled", ".CHP", ".chp", ".tex", ".fm", "http://content.nejm.org/cgi/content/", "stdin", "Corel PHOTO-PAINT"]
       badWholes = ["P", "b", "cretu", "user", "yeh", "Canon", "times", "is2020", "downes", "American Medical Association", "om", "lhf", "comp", "khan", "Science Magazine", "Josh Lerner, Scott Stern (Editors)", "arsalan", "rssa_a0157 469..482", "Schniederjans_lo", "mcdonaldm", "ET35-4G.vp", "spco_037.fm", "mchahino", "LaTeX2e", "Paperless", "fulvio", "Winter", "yeh", "markj", "Vahrenhorst", "vahrenhorst", "Vahrenhorst 2004", "Vahrenhorst 2008", "pilc2501", "yeh 2008", "markj 2009", "021186U129", "02_ASQ523602 1..33", "03_gbb155 240..248", "1)", "1.0b", "110s(6) Science. 555-", "1247", "2913 | 01 Chorney", "301246", "378787 1100..1105", "4559", "459119", "4AD004/Prov9 FMV/4P", "52457938", "7.0 psym 7#1 2002", "72508-danigelis.q41", "9757 van Stuijvenberg", "99202", "BBS1200319 661..726", "BBS1300119 297..350", "Backtalk", "Backups", "BatesTalk", "Brookings draft v", "CAM200", "CDP370136 177..182", "CMMI10", "COLLINCH01", "COMM34$U44", "COPSINGOLEVET", "DO00010384 643..650", "DP 14-009", "Digestion", "Final_EXIT_text", "Gerontotherapeutics", "Harrison J", "II", "IMD JATS", "ISO/IEC 9899:yyyy", "Information", "JC162160 1642..1651", "JEOBP_14_2_2011", "JMCB16U208", "Journal06-04b.cdr", "Latvala", "Layout 1", "MASTER THESIS 5", "MIT-LCS:MIT/LCS/TR-164", "MSS21283 927..934", "Masters' Thesis", "Nowicka, R", "OP-ESRJ170071 1..13", "OP-QJEC150001 571..614 ++", "P:TEXSICOMP 9-1 8849 8849", "PEDS20142707_pap_peds 1..9", "PEDS_20173872 1..11", "S0747-5632(00)00043-1", "PME00042", "PSCI13124", "Print", "RAAG_A_1310021_O", "RULES-2014-P-CON", "Review", "SIA", "Schniederjans_lo", "Slide 1", "Standards 05-06", "TF-TPDR120086 1..8 ++", "Title", "Title:", "Tobler_CameraReady", "US020180204116A120180719", "Unknown", "VJMB_A_1327416_O", "Ventura - 12JBR8", "Vol 9#5", "WR02015.fm", "Wildcats", "ZP577", "ajps_461_HR", "anp060-79 407..506", "bhm211 1737..1747", "btn127 1381..1385", "c011.tex", "cns_317_LR", "ddl206 2721..2731", "default", "desc_745.fm", "e08:9", "ejn_5217 3532..3540", "emon-84-04-01 3..28", "es-2013-03688w 1..9", "foo", "hcrj901068 151..157", "hred_91_110.42_66", "inhy_60_205.213_218", "ipg1200217a", "jasar08282 841..854", "jcp20186 373..378", "jcp25202 175..179", "jcpp_1798 1192..1199", "jn169326 872..877", "khan", "mbev_16_1218.1791_1798", "mgs;01jan95", "osteo-1203 257..264", "oup_cercor_bhy157 1..11 ++", "pnas201201895 1..8", "pnp06457 1125..1128", "rssa_a0157 469..482", "s00221-005-2334-6ca-web 23..30", "stdin", "template", "title", "vsp0092 187..211", "ÿþ1", "ÿþ14-226", "“Alt"]

-- nested JSON object: eg. 'jq .message.abstract'
newtype Crossref = Crossref { message :: Message } deriving (Show,Generic)
instance FromJSON Crossref
newtype Message = Message { abstract :: Maybe String } deriving (Show,Generic)
instance FromJSON Message
doi2Abstract :: String -> IO (Maybe String)
doi2Abstract doi = if length doi < 7 then return Nothing
                   else do (_,_,bs) <- runShellCommand "./" Nothing "curl" ["--location", "--silent", "https://api.crossref.org/works/"++doi, "--user-agent", "gwern+crossrefscraping@gwern.net"]
                           if bs=="Resource not found." then return Nothing
                           else let j = eitherDecode bs :: Either String Crossref
                                in case j of -- start unwrapping...
                                    Left e -> printRed ("Error: Crossref request failed: "++doi++" "++e) >> return Nothing
                                    Right j' -> let j'' = abstract $ message j' in
                                      case j'' of
                                       Nothing -> return Nothing
                                       Just a -> do trimmedAbstract <- fmap cleanAbstractsHTML $ processParagraphizer doi $ cleanAbstractsHTML a
                                                    return $ Just trimmedAbstract

-- handles medRxiv too (same codebase)
biorxiv p = do checkURL p
               (status,_,bs) <- runShellCommand "./" Nothing "curl" ["--location", "--silent", p, "--user-agent", "gwern+biorxivscraping@gwern.net"]
               case status of
                 ExitFailure _ -> printRed ("BioRxiv download failed: " ++ p) >> return (Left Permanent)
                 _ -> do
                        let b = U.toString bs
                        let f = parseTags b
                        let metas = filter (isTagOpenName "meta") f

                        let title = concat $ parseMetadataTagsoup "DC.Title" metas
                        if title=="" then printRed ("BioRxiv parsing failed: " ++ p ++ " : parsed metadata: " ++ ppShow metas ++ "\nParsed tags: " ++ show f) >> return (Left Permanent)
                          else do
                                 let date    = concat $ parseMetadataTagsoup "DC.Date" metas
                                 let doi     = processDOI $ concat $ parseMetadataTagsoup "citation_doi" metas
                                 let author  = initializeAuthors $ intercalate ", " $ filter (/="") $ parseMetadataTagsoup "DC.Contributor" metas
                                 let abstractRaw = concat $ parseMetadataTagsoupSecond "citation_abstract" metas
                                 let abstractRaw' = if not (null abstractRaw) then abstractRaw else concat $ parseMetadataTagsoup "DC.Description" metas
                                 abstrct <- fmap (replace "9s" "s") $ -- BUG: BioRxiv abstracts have broken quote encoding. I reported this to them 2 years ago and they still have not fixed it.
                                   fmap (linkAutoHtml5String . cleanAbstractsHTML) $ processParagraphizer p $ cleanAbstractsHTML abstractRaw'
                                 let ts = [] -- TODO: replace with ML call to infer tags
                                 if abstrct == "" then do printRed ("BioRxiv parsing failed")
                                                          print ("Metas: " ++ show metas)
                                                          print abstractRaw
                                                          print abstractRaw'
                                                          return (Left Temporary)
                                   else
                                                   return $ Right (p, (title, author, date, doi, ts, abstrct))
  where
    parseMetadataTagsoup, parseMetadataTagsoupSecond :: String -> [Tag String] -> [String]
    parseMetadataTagsoup key metas = map (\(TagOpen _ (a:b)) ->  if snd a == key then snd $ head b else "") metas
      -- 'TagOpen "meta" [("name","citation_abstract"),("lang","en"),("content","<h3>ABSTRACT</h3>\n<p>The vast majority of human mutations have minor allele frequencies (MAF) under 1%, with the plurality observed only once (ie. \8220singletons\8221). While Mendelian diseases are predominantly caused by rare alleles, their role in complex phenotypes remains largely unknown. We develop and rigorously validate an approach to jointly estimate the contribution of alleles with different frequencies, including singletons, to phenotypic variation. We apply our approach to transcriptional regulation, an intermediate between genetic variation and complex disease. Using whole genome DNA and RNA sequencing data from 360 European individuals, we find that singletons alone contribute ~23% of all <i>cis</i>-heritability across genes (dwarfing the contributions of other frequencies). We then integrate external estimates of global MAF from worldwide samples to improve our inference, and find that average <i>cis</i>-heritability is 15.3%. Strikingly, 50.9% of <i>cis</i>-heritability is contributed by globally rare variants (MAF&lt;0.1%), implicating purifying selection as a pervasive force shaping the regulatory architecture of most human genes.</p><h3>One Sentence Summary</h3>\n<p>The vast majority of variants so far discovered in humans are rare, and together they have a substantial impact on gene regulation.</p>")]'
    parseMetadataTagsoupSecond key metas = map (\(TagOpen _ (a:b)) ->  if snd a == key then snd $ b!!1 else "") metas

-- Heuristic checks for specific link sources:
checkURL :: String -> IO ()
checkURL u = do let doubleURL = u =~ badUrlRegex -- I keep accidentally concatenating Arxiv URLs when tagging.
                if not doubleURL then return () else error u

arxiv url = do -- Arxiv direct PDF links are deprecated but sometimes sneak through or are deliberate section/page links
               checkURL url
               let arxivid = takeWhile (/='#') $ if "/pdf/" `isInfixOf` url && ".pdf" `isSuffixOf` url
                                 then replaceMany [("https://arxiv.org/pdf/", ""), (".pdf", "")] url
                                 else replace "https://arxiv.org/abs/" "" url
               (status,_,bs) <- runShellCommand "./" Nothing "curl" ["--location","--silent","https://export.arxiv.org/api/query?search_query=id:"++arxivid++"&start=0&max_results=1", "--user-agent", "gwern+arxivscraping@gwern.net"]
               case status of
                 ExitFailure _ -> printRed ("Error: curl API call failed on Arxiv ID: " ++ arxivid ++ "; Result: " ++ show bs) >> return (Left Temporary)
                 _ -> do let (tags,_) = element "entry" $ parseTags $ U.toString bs
                         -- compile the title string because it may include math (usually a superscript, like "S$^2$-MLP: Spatial-Shift MLP Architecture for Vision" or "RL$^2$" etc)
                         let title = replace "<p>" "" $ replace "</p>" "" $ cleanAbstractsHTML $ processArxivAbstract $ trimTitle $ findTxt $ fst $ element "title" tags
                         let authors = initializeAuthors $ intercalate ", " $ getAuthorNames tags
                         let published = take 10 $ findTxt $ fst $ element "published" tags -- "2017-12-01T17:13:14Z" → "2017-12-01"
                         -- NOTE: Arxiv used to not provide its own DOIs; that changed in 2022: <https://blog.arxiv.org/2022/02/17/new-arxiv-articles-are-now-automatically-assigned-dois/>; so look for DOI and if not set, try to construct it automatically using their schema `10.48550/arXiv.2202.01037`
                         let doiTmp = processDOI $ findTxt $ fst $ element "arxiv:doi" tags
                         let doi = if null doiTmp then processDOIArxiv url else doiTmp
                         abst <- fmap (linkAutoHtml5String . cleanAbstractsHTML) $ processParagraphizer url $ cleanAbstractsHTML $ processArxivAbstract $ findTxt $ fst $ element "summary" tags
                         let ts = [] :: [String] -- TODO: replace with ML call to infer tags
                         -- the API sometimes lags the website, and a valid Arxiv URL may not yet have obtainable abstracts, so it's a temporary failure:
                         if abst=="" then do printRed "Error: Arxiv parsing failed!"
                                             printGreen ("Error details: failure on Arxiv URL "++url ++"; Arxiv ID: " ++ arxivid ++ "; raw response: " ++ show bs ++ "; parsed data: " ++ show [show tags, title, authors, published, doiTmp, doi, abst, show ts])
                                             return (Left Temporary)
                                      else return $ Right (url, (title,authors,published,doi,ts,abst))
-- NOTE: we inline Tagsoup convenience code from Network.Api.Arxiv (https://hackage.haskell.org/package/arxiv-0.0.1/docs/src/Network-Api-Arxiv.html); because that library is unmaintained & silently corrupts data (https://github.com/toschoo/Haskell-Libs/issues/1), we keep the necessary code close at hand so at least we can easily patch it when errors come up
-- Get the content of a 'TagText'
findTxt :: [Tag String] -> String
findTxt [] = ""
findTxt (t:ts) = case t of
                   TagText x -> x
                   _         -> findTxt ts
getAuthorNames :: [Tag String] -> [String]
getAuthorNames = go
  where go s = case element "author" s of
                 ([],[]) -> []
                 (a,[])  -> [getString "name" a]
                 (a,r)   ->  getString "name" a : go r
        getString :: String -> [Tag String] -> String
        getString n soup = let (i,_) = element n soup
                      in if null i then "" else findTxt i
element :: String -> [Tag String] -> ([Tag String], [Tag String])
element _  []     = ([],[])
element nm (t:ts) | isTagOpenName nm t = let (r,rs) = closeEl 0 ts
                                          in (t:r,rs)
                  | otherwise          = element nm ts
  where closeEl :: Int -> [Tag String] -> ([Tag String], [Tag String])
        closeEl _ [] = ([],[])
        closeEl i (x:xs) = go i (isTagCloseName nm x) x xs
        go i b x xs | b && i == 0        = ([x],xs)
                    | b && i >  0        = let (r,rs) = closeEl (i-1) xs
                                            in (x:r,rs)
                    | isTagOpenName nm x = let (r,rs) = closeEl (i+1) xs
                                            in (x:r,rs)
                    | otherwise          = let (r,rs) = closeEl i     xs
                                            in (x:r,rs)
openreview p = do checkURL p
                  let p' = replace "/pdf?id=" "/forum?id=" p
                  (status,_,bs) <- runShellCommand "./" Nothing "openReviewAbstract.sh" [p']
                  case status of
                      ExitFailure _ -> printRed ("OpenReview download failed: " ++ p) >> return (Left Permanent)
                      _ -> do
                             let (title:author:date:tldr:desc:keywords) = lines $ U.toString bs
                             let keywords' = if null keywords || keywords == [""] then "" else
                                               if length keywords > 1 then (unlines $ init keywords) ++ "\n[Keywords: " ++ last keywords ++ "]"
                                               else "[Keywords: " ++ concat keywords ++ "]"
                             let tldr' = cleanAbstractsHTML $ processArxivAbstract tldr
                             let desc' = cleanAbstractsHTML $ processArxivAbstract desc
                             let abstractCombined = trim $ intercalate "\n" [tldr', desc', linkAutoHtml5String $ cleanAbstractsHTML $ processArxivAbstract keywords']
                             return $ Right (p, (trimTitle title, initializeAuthors $ trim author, date, "", [],
                                                 -- due to pseudo-LaTeX
                                                   abstractCombined))

processDOI, processDOIArxiv :: String -> String
processDOI = replace "–" "-" . replace "—" "-"
 -- Arxiv has some weird URLs and edge-cases like <https://arxiv.org/abs/hep-ph/0204295> (note double-subdirectory & lack of period-separation).
processDOIArxiv url = "10.48550/arXiv." ++
                               sed "https://arxiv.org/[a-z-]+/([0-9]+\\.[0-9]+).*" "\\1" -- regular current Arxiv URL pattern
                               (sed "https://arxiv.org/abs/[a-z-]+/([0-9]+).*" "\\1" url) -- old-style like 'hep-ph'

processPubMedAbstract :: String -> String
processPubMedAbstract abst = let clean = runPure $ do
                                   -- strip overly-indented PLOS formatting like:
                                   -- <abstract>
                                   --   <sec>
                                   --     <title>Background</title>
                                   --     <p>Systematic reviews (SRs) of TCM have become increasingly popular in China and have been published in large numbers. This review provides the first examination of epidemiological characteristics of these SRs as well as compliance with the PRISMA and AMSTAR guidelines.</p>
                                   --   </sec>
                                   --   <sec>
                                   --     <title>Objectives</title>...
                                   pandoc <- readMarkdown def{readerExtensions=pandocExtensions} (T.pack $ sed "^ +" "" abst)
                                   html <- writeHtml5String safeHtmlWriterOptions pandoc
                                   return $ T.unpack html
                             in case clean of
                                  Left e -> error $ ppShow e ++ " : " ++ abst
                                  Right output -> linkAutoHtml5String $ cleanAbstractsHTML $ trim $ replace "<br/>" "" $ cleanAbstractsHTML output

-- Arxiv makes multi-paragraph abstracts hard because the 'HTML' is actually LaTeX, so we need to special Pandoc preprocessing (for paragraph breaks, among other issues):
processArxivAbstract :: String -> String
processArxivAbstract a = let cleaned = runPure $ do
                                     -- if we don't escape dollar signs, it breaks abstracts with dollar amounts like "a $700 GPU"; crude heuristic, if only 1 '$', then it's not being used for LaTeX math (eg. https://arxiv.org/abs/2108.05818#tencent )
                                    let dollarSignsN = length $ filter (=='$') a
                                    let tex = sedMany [("\\\\citep?\\{([[:graph:]]*)\\}", "(\\texttt{\\1})"),
                                                       ("\\\\citep?\\{([[:graph:]]*, ?[[:graph:]]*)\\}", "(\\texttt{\\1})"),
                                                       ("\\\\citep?\\{([[:graph:]]*, ?[[:graph:]]*, ?[[:graph:]]*)\\}", "(\\texttt{\\1})"),
                                                       ("\\\\citep?\\{([[:graph:]]*, ?[[:graph:]]*, ?[[:graph:]]*, ?[[:graph:]]*)\\}", "(\\texttt{\\1})"),
                                                       ("({\\lambda})", "(λ)")] $
                                              replaceMany [("%", "\\%"), ("\\%", "%"), ("$\\%$", "%"), ("\n  ", "\n\n"), (",\n", ", "), ("~", " \\sim")

                                                          ] $ (if dollarSignsN == 1 then replaceMany [("$", "\\$")] else id) a

                                    pandoc <- readLaTeX def{ readerExtensions = pandocExtensions } $ T.pack tex
                                      -- NOTE: an Arxiv API abstract can have any of '%', '\%', or '$\%$' in it. All of these are dangerous and potentially breaking downstream LaTeX parsers.

                                    writeHtml5String safeHtmlWriterOptions{writerWrapText=WrapNone, writerHTMLMathMethod = MathJax defaultMathJaxURL} pandoc
              in case cleaned of
                 Left e -> error $ " : " ++ ppShow e ++ " : " ++ a
                 Right output -> cleanAbstractsHTML $ cleanArxivAbstracts $ T.unpack output

-- 'significan✱' in Arxiv abstracts typically doesn't mean statistically-significant, but 'important' or 'large'; unfortunately,
-- this is puffery applied to every single advance, and in an Arxiv abstract, is meaningless
cleanArxivAbstracts :: String -> String
cleanArxivAbstracts = replaceMany [(" significant", ""), (" significantly", ""), (" significance", "")] .
                            replaceMany [("more significant", "important"), ("significant margin", "large margin"), ("significant capital", "large capital"), ("significant amount", "large amount"), ("significant cost", "large cost"), ("hugely significant", "important"), ("without significant overhead", "without much overhead"), ("significant risk", "large risk"), ("significant semantic complexity", "high semantic complexity"), ("more significantly correlate", "more correlate"), ("significantly yet smoothly", "substantially get smoothly"), ("significance metric", "statistical-significance metric")]

-- Is an annotation (HTML or Markdown) already If the input has more than one <p>, or if there is one or more double-newlines, that means this input is already multiple-paragraphs
-- and we will skip trying to break it up further.
paragraphized :: FilePath -> String -> Bool
paragraphized f a = f `elem` whitelist ||
                  paragraphsMarkdown a || blockElements a || length (paragraphsHtml a) > 1
 where
   -- double newlines are only in Markdown strings, and split paragraphs:
   paragraphsMarkdown :: String -> Bool
   paragraphsMarkdown b = "\n\n" `isInfixOf` b
   blockElements :: String -> Bool
   -- full-blown lists or blockquotes also imply it's fully-formatted
   blockElements b = anyInfix b ["<ul>", "<ol>", "<ul type=", "<ol type=", "<blockquote>", "<figure>"]
   -- annotations are wrapped in a '<p>...</p>' pair, unless they start with another block element; if there are two or more '<p>', then, there are at least two paragraphs (because it must be '<p>...</p> ... <p>...</p>') and it counts as being paragraphized.
   paragraphsHtml :: String -> [(T.Text,T.Text)]
   paragraphsHtml b = T.breakOnAll "<p>" (T.pack b)
   whitelist :: [String]
   whitelist = ["/docs/cs/1980-rytter.pdf", "/docs/economics/1998-delong.pdf", "/docs/cs/algorithm/1980-rytter.pdf"]

-- If a String (which is not HTML!) is a single long paragraph (has no double-linebreaks), call out to paragraphizer.py, which will use GPT-3 to try to break it up into multiple more-readable paragraphs.
-- This is quite tricky to use: it wants non-HTML plain text (any HTML will break GPT-3), but everything else wants HTML
processParagraphizer :: FilePath -> String -> IO String
processParagraphizer _ "" = return ""
processParagraphizer p a =
      if length a < 1024 || paragraphized p a then return a
      else do let a' = replace "<p>" "" $ replace "</p>" "" a
              let a'' = trim $ replace "\160" " " $ toMarkdown a'
              (status,_,mb) <- runShellCommand "./" Nothing "python" ["static/build/paragraphizer.py", a'']
              case status of
                ExitFailure err -> printGreen (intercalate " : " [a, a', ppShow status, ppShow err, ppShow mb]) >> printRed "Paragraphizer failed!" >> return a
                _ -> do let clean = runPure $ do
                              pandoc <- readMarkdown def{readerExtensions=pandocExtensions} (T.pack $ trim $ U.toString mb)
                              html <- writeHtml5String safeHtmlWriterOptions pandoc
                              return $ T.unpack html
                        case clean of
                              Left e -> error $ ppShow e ++ " : " ++ a
                              Right output -> return $ cleanAbstractsHTML output

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

linkCanonicalize :: String -> String
linkCanonicalize l | "https://www.gwern.net/" `isPrefixOf` l = replace "https://www.gwern.net/" "/" l
                   -- | head l == '#' = l
                   | otherwise = l

-- gwern :: Path -> IO (Either Failure (Path, MetadataItem))
gwern "/docs/index" = gwerntoplevelDocAbstract -- special-case ToC generation of all subdirectories for a one-stop shop
gwern "docs/index"  = gwerntoplevelDocAbstract
gwern p | p == "/" || p == "" = return (Left Permanent)
        | ".pdf" `isInfixOf` p = pdf p
        | anyInfix p [".avi", ".bmp", ".conf", ".css", ".csv", ".doc", ".docx", ".ebt", ".epub", ".gif", ".GIF", ".hi", ".hs", ".htm", ".html", ".ico", ".idx", ".img", ".jpeg", ".jpg", ".JPG", ".js", ".json", ".jsonl", ".maff", ".mdb", ".mht", ".mp3", ".mp4", ".mkv", ".o", ".ods", ".opml", ".pack", ".page", ".patch", ".php", ".png", ".R", ".rm", ".sh", ".svg", ".swf", ".tar", ".ttf", ".txt", ".wav", ".webm", ".xcf", ".xls", ".xlsx", ".xml", ".xz", ".yaml", ".zip"] = return (Left Permanent) -- skip potentially very large archives
        | anyPrefix p ["metadata", "/metadata"] ||
          anySuffix p ["#external-links", "#see-also", "#see-also", "#see-alsos", "#see-also-1", "#see-also-2", "#footnotes", "#links", "#misc", "#miscellaneous", "#appendix", "#appendices", "#conclusion", "#conclusion-1", "#conclusion-2", "#media", "#writings", "#filmtv", "#music", "#books"] ||
          anyInfix p ["index.html", "/index#"] ||
          ("/index#" `isInfixOf` p && "-section" `isSuffixOf` p)  = return (Left Permanent)
        | "/newsletter/" `isPrefixOf` p && '#' `elem` p = return (Left Permanent) -- newsletter sections like '/newsletter/2022/01#fiction' do not have abstracts
        | p =~ sectionAnonymousRegex = return (Left Permanent) -- unnamed sections are unstable, and also will never have abstracts because they would've gotten a name as part of writing it.
        | p =~ footnoteRegex= return (Left Permanent) -- shortcut optimization: footnotes will never have abstracts (right? that would just be crazy hahaha ・・；)
        | otherwise =
            do let p' = sed "^/" "" $ replace "https://www.gwern.net/" "" p
               let indexP = "docs/" `isPrefixOf` p' && "/index" `isInfixOf` p'
               printGreen p'
               checkURL p
               (status,_,bs) <- runShellCommand "./" Nothing "curl" ["--silent", "https://www.gwern.net/"++p', "--user-agent", "gwern+gwernscraping@gwern.net"] -- we strip `--location` because we do *not* want to follow redirects. Redirects creating duplicate annotations is a problem.
               case status of
                 ExitFailure _ -> printRed ("Gwern.net download failed: " ++ p) >> return (Left Permanent)
                 _ -> do
                        let b = U.toString bs
                        let f = parseTags b
                        let metas = filter (isTagOpenName "meta") f
                        let title = cleanAbstractsHTML $ concatMap (\(TagOpen _ (t:u)) -> if snd t == "title" then snd $ head u else "") metas
                        let date = let dateTmp = concatMap (\(TagOpen _ (v:w)) -> if snd v == "dc.date.issued" then snd $ head w else "") metas
                                       in if dateTmp=="N/A" || dateTmp=="2009-01-01" || not (dateTmp =~ dateRegex) then "" else dateTmp
                        let description = concatMap (\(TagOpen _ (cc:dd)) -> if snd cc == "description" then snd $ head dd else "") metas
                        let keywordTags = if "#" `isInfixOf` p then [] else
                                            concatMap (\(TagOpen _ (x:y)) -> if snd x == "keywords" then Utils.split ", " $ snd $ head y else []) metas
                        let author = initializeAuthors $ concatMap (\(TagOpen _ (aa:bb)) -> if snd aa == "author" then snd $ head bb else "") metas
                        let thumbnail = if not (any filterThumbnail metas) then "" else
                                          (\(TagOpen _ [_, ("content", thumb)]) -> thumb) $ head $ filter filterThumbnail metas
                        let thumbnail' = if ("https://www.gwern.net/static/img/logo/logo-whitebg-large-border.png" `isPrefixOf` thumbnail) then "" else replace "https://www.gwern.net/" "" thumbnail
                        let thumbnailText = if not (any filterThumbnailText metas) then "" else -- WARNING: if there is no thumbnailText, then bad things will happen downstream as the thumbnail gets rendered as solely an <img> rather than a <figure><img>. We will assume the author will always have a thumbnailText set.
                                          (\(TagOpen _ [_, ("content", thumbt)]) -> thumbt) $ head $ filter filterThumbnailText metas
                        when (null thumbnailText) $ printRed ("Warning: no thumbnailText alt text defined for URL " ++ p)
                        thumbnailFigure <- if thumbnail'=="" then return "" else do
                              (color,h,w) <- invertImage thumbnail'
                              let imgClass = if color then "class=\"invert-auto float-right page-thumbnail\"" else "class=\"float-right page-thumbnail\""
                              return ("<figure><img " ++ imgClass ++ " height=\"" ++ h ++ "\" width=\"" ++ w ++ "\" src=\"/" ++ thumbnail' ++ "\" title=\"" ++ thumbnailText ++ "\" alt=\"\" /></figure>")

                        let doi = "" -- I explored the idea but DOIs are too expensive & ultimately do little useful
                        let footnotesP = "<section class=\"footnotes\"" `isInfixOf` b

                        let toc = gwernTOC footnotesP indexP p' f
                        let toc' = if toc == "<div class=\"columns TOC\"></div>" then "" else toc

                        let (sectTitle,gabstract) = gwernAbstract ("/index" `isSuffixOf` p' || "newsletter/" `isPrefixOf` p') p' description toc' f

                        let title' = if null sectTitle then title else title ++ " § " ++ sectTitle

                        let combinedAnnotation = (if "</figure>" `isInfixOf` gabstract || "<img>" `isInfixOf` gabstract || null gabstract then "" else thumbnailFigure) ++ -- some pages like /Questions have an image inside the abstract; preserve that if it's there
                                                 gabstract

                        if gabstract == "404 Not Found Error: no page by this name!" || title' == "404 Not Found" || (null keywordTags && null gabstract) then
                          return (Left Permanent) -- NOTE: special-case: if a new essay or a tag hasn't been uploaded yet, make a stub entry; the stub entry will eventually be updated via a `updateGwernEntries` scrape. (A Temporary error has the drawback that it throws changeTag.hs into an infinite loop as it keeps trying to fix the temporary error.)
                          else return $ Right (p, (title', author, date, doi, keywordTags, combinedAnnotation))
        where
          filterThumbnail (TagOpen "meta" [("property", "og:image"), _]) = True
          filterThumbnail _ = False
          filterThumbnailText (TagOpen "meta" [("property", "og:image:alt"), _]) = True
          filterThumbnailText _ = False

-- skip the complex gwernAbstract logic: /docs/index is special because it has only subdirectories, is not tagged, and is the entry point. We just generate the ToC directly from a recursive tree of subdirectories with 'index.page' entries:
gwerntoplevelDocAbstract :: IO (Either Failure (Path, MetadataItem))
gwerntoplevelDocAbstract = do allDirs <- listTagDirectories ["docs/"]
                              let allDirLinks = unlines $ map (\d -> "<li><a class='link-page link-tag directory-indexes-downwards link-annotated link-annotated-partial' data-link-icon='arrow-down' data-link-icon-type='svg' rel='tag' href=\"" ++ d ++ "\">" ++ (T.unpack $ abbreviateTag (T.pack (replace "/docs/" "" $ takeDirectory d))) ++ "</a></li>") allDirs
                              return $ Right ("/docs/index", ("docs tag","N/A","","",[],"<p>Bibliography for tag <em>docs</em>, most recent first: " ++ show (length allDirs) ++ " tags (<a href='/index' class='link-page link-tag directory-indexes-upwards link-annotated link-annotated-partial' data-link-icon='arrow-up-left' data-link-icon-type='svg' rel='tag' title='Link to parent directory'>parent</a>).</p> <div class=\"columns TOC\"> <ul>" ++ allDirLinks ++ "</ul> </div>"))

gwernAbstract :: Bool -> String -> String -> String -> [Tag String] -> (String,String)
gwernAbstract shortAllowed p' description toc f =
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
                         restofpageAbstract = trim $ renderTags $ filter filterAbstract abstractRaw
                         in (titleClean, abstractRaw, restofpageAbstract)
      abstrct'  = (if anyPrefix abstrct ["<p>", "<p>", "<figure>"] then abstrct
                    else if null abstrct then "" else "<p>"++abstrct++"</p>") ++ " " ++ toc
      -- combine description + abstract; if there's no abstract, settle for the description:
      abstrct'' = if description /= "" && abstrct' /= "" then "<div class=\"page-description-annotation\"><p>"++description++"</p></div>"++
                                                              abstrct'
                                      else if description == "" && abstrct' /= "" then abstrct'
                                           else if description /= "" && abstrct' == "" then "<p>"++description++"</p>"
                                                else ""
      abstrct''' = trim $ replace "href=\"#" ("href=\"/"++baseURL++"#") abstrct'' -- turn relative anchor paths into absolute paths
      abstrct'''' = linkAutoHtml5String $ sed " id=\"fnref[0-9]+\"" "" abstrct''' -- rm footnote IDs - cause problems when transcluded
  in if (("#" `isInfixOf` p') && null abstrct) then (t,"") else
       if "scrape-abstract-not" `isInfixOf` (renderTags abstrctRw) then (t,"") else
         if shortAllowed then (t,abstrct'''') else (t,abstrct'''')
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

gwernTOC :: Bool -> Bool -> String -> [Tag String] -> String
gwernTOC footnotesP indexP p' f =
 -- for tags, condense the ToC by removing the See Also & Miscellaneous <h1>s, and the Links wrapper around the individual entries:
 (\tc' -> if not indexP then tc'
   else sedMany [("</li>\n          \n        </ul>",""),
                 ("<li>\n            <a class=\"id-not\" href=\"#miscellaneous\"><span>Miscellaneous</span></a>\n          </li>", ""),
                 ("<li>\n            <a class=\"id-not\" href=\"#miscellaneous\">Miscellaneous</a>\n          </li>", ""),
                 ("<li>\n            <a class=\"id-not\" href=\"#links\"><span>Links</span></a>\n            <ul>", ""),
                 ("<li>\n            <a class=\"id-not\" href=\"#links\">Links</a>\n            <ul>", ""),
                 ("<li>\n            <a class=\"id-not\" href=\"#see-also\"><span>See Also</span></a>\n          </li>", ""),
                 ("<li>\n            <a class=\"id-not\" href=\"#see-also\">See Also</a>\n          </li>", ""),
                 ("<li>\n            <a class=\"id-not\" href=\"#link-bibliography\"><span>Link Bibliography</span></a>\n          </li>", ""),
                 ("<li>\n            <a class=\"id-not\" href=\"#link-bibliography\">Link Bibliography</a>\n          </li>", "")
                ] tc') $
 -- Pandoc declines to add an ID to footnotes section; on Gwern.net, we override this by at compile-time rewriting the <section> to have `#footnotes`:
 (\tc -> if not footnotesP then tc else replace "</ul>\n</div>" "<li><a href=\"#footnotes\">Footnotes</a></li></ul></div>" tc) $
        -- add columns class to condense it in popups/tags
        replace "<div class=\"columns\"><div class=\"TOC\">" "<div class=\"columns TOC\">" $
        -- WARNING: Pandoc generates redundant <span></span> wrappers by abusing the span wrapper trick while removing header self-links <https://github.com/jgm/pandoc/issues/8020>; so since those are the only <span>s which should be in ToCs (...right? [EDIT: no, the subscript citations are]), we'll remove them.
        -- sed "<span>(.*)</span>" "" $
        (if '#'`elem`p' then (\t -> let toc = truncateTOC p' t in if toc /= "" then "<div class=\"columns TOC\">" ++ toc ++ "</div>" else "") else replace "<a href=" "<a class=\"id-not\" href=") $
        -- NOTE: we strip the `id="TOC"`, and all other link IDs on TOC subentries, deliberately because the ID will cause HTML validation problems when abstracts get transcluded into tags/link-bibliographies
        sed " id=\"[a-z0-9-]+\">" ">" $ replace " id=\"markdownBody\"" "" $ replace " id=\"TOC\"" "" index
        where
          index = if length indexType1 > length indexType2 then indexType1 else indexType2
          indexType1 = replace "markdownBody" "" $ replace "directory-indexes" "" $ replace "columns" "columns TOC" $ renderTagsOptions renderOptions $
            takeWhile (\e' -> e' /= TagClose "div") $ dropWhile (\e -> e /=  (TagOpen "div" [("id","markdownBody"),("class","markdownBody directory-indexes columns")])) f
          indexType2 = renderTagsOptions renderOptions $
                       [TagOpen "div" [("class","columns")]] ++
                       (takeWhile (\e' -> e' /= TagClose "div")  $ dropWhile (\e -> e /=  (TagOpen "div" [("id","TOC"), ("class","TOC")])) f) ++
                       [TagClose "div"]

truncateTOC :: String -> String -> String
truncateTOC p' toc = let pndc = truncateTOCHTML (T.pack (sed ".*#" "" p')) (T.pack toc) in
                       if null pndc then "" else
                           case (runPure $ writeHtml5String safeHtmlWriterOptions (Pandoc nullMeta (init pndc))) of
                             Left e -> error ("Failed to compile truncated ToC: " ++ show p' ++ show toc ++ show e)
                             Right text -> T.unpack text

-- handle initials consistently as space-separated; delete the occasional final Oxford 'and' cluttering up author lists
initializeAuthors :: String -> String
initializeAuthors = trim . replaceMany [(". . ", ". "), ("?",""), (",,", ","), (",,", ","), (", ,", ", "), (" ", " "), (" MA,", ","), (", MA,", ","), (" MS,", ","), ("Dr ", ""), (" PhD", ""), (" MRCGP", ""), (" OTR/L", ""), (" OTS", ""), (" FMedSci", ""), ("Prof ", ""), (" FRCPE", ""), (" FRCP", ""), (" FRS", ""), (" MD", ""), (",, ,", ", "), ("; ", ", "), (" ; ", ", "), (" , ", ", "), (" and ", ", "), (", & ", ", "), (", and ", ", "), (" MD,", " ,"), (" M. D.,", " ,"), (" MSc,", " ,"), (" PhD,", " ,"), (" Ph.D.,", " ,"), (" BSc,", ","), (" BSc(Hons)", ""), (" MHSc,", ","), (" BScMSc,", ","), (" ,,", ","), (" PhD1", ""), (" , BSc", ","), (" BA(Hons),1", ""), (" , BSc(Hons),1", ","), (" , MHSc,", ","), ("PhD,1,2 ", ""), ("PhD,1", ""), (" , BSc", ", "), (",1 ", ","), (" & ", ", "), (",,", ","), ("BA(Hons),", ","), (", (Hons),", ","), (", ,2 ", ","), (",2", ","), (" MSc", ","), (" , PhD,", ","), (" JD,", ","), ("MS,", ","), (" BS,", ","), (" MB,", ","), (" ChB", ""), ("Meena", "M."), ("and ", ", "), (", PhD1", ","), ("  DMSc", ""), (", (Hons),", ","), (",, ", ", "), (", ,,", ", "), (",,", ", "), ("\"", ""), ("'", "’"), ("OpenAI, :, ", ""), (" et al", ""), (" et al.", ""), (", et al.", ""), ("Jr.", "Junior"), (", Jr.", " Junior"), (", Junior", " Junior")] .
                       sedMany [
                         ("([a-zA-Z]+),([A-Z][a-z]+)", "\\1, \\2"), -- "Foo Bar,Quuz Baz" → "Foo Bar, Quuz Baz"
                         (",$", ""),
                         (", +", ", "),
                         ("^([A-Z][a-z]+), ([A-Z]\\.)$", "\\2 \\1"), -- "Smith, J." → "J. Smith"; for single words followed by a single letter, we can assume that it is a 'surname, initial' rather than 2 authors, 'surname1, surname2'
                         ("^([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.)$", "\\2 \\1, \\4 \\3"), -- likewise, but for the 2-author case: 'Smith, J.; Doe, J.'
                         ("^([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.)$", "\\2 \\1, \\4 \\3, \\6 \\5"), -- 3-author
                         ("^([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.)$", "\\2 \\1, \\4 \\3, \\6 \\5, \\8 \\7"), -- 4-author, and I won't try for more
                         ("([A-Z]\\.)([A-Za-z]+)", "\\1 \\2"),                              -- "A.Smith"      → "A. Smith"
                         ("[ ^]?([A-Z])([A-Z]) ([A-Za-z]+)", " \\1. \\2. \\3"),             -- " LK Barnes"   → " L. K. Barnes"
                         ("([A-Z]\\.)([A-Z]\\.) ([A-Za-z]+)", "\\1 \\2 \\3"),               -- "A.B. Smith"   → "A. B. Smith"
                         ("([A-Z]\\.)([A-Z]\\.)([A-Z]\\.) ([A-Za-z]+)", "\\1 \\2 \\3 \\4"), -- "C.A.B. Smith" → "C. A. B. Smith"
                         (" ([A-Z])([A-Z])([A-Z]) ", " \\1. \\2. \\3. "),                   -- "John HAB Smith" → "John H. A. B. Smith"
                         (" ([A-Z])([A-Z]) ", " \\1. \\2. "),                               -- "John HA Smith"  → "John H. A. Smith"
                         (" ([A-Z]\\.) ([A-Z]) ", " \\1 \\2. "),                            -- "John H. A Smith"  → "John H. A. Smith"
                         (" ([A-Z]) ([A-Z]\\.) ", " \\1. \\2 "),                            -- "John H A. Smith"  → "John H. A. Smith"
                         (" ([A-Z]) ", " \\1. ")                                             -- "John H Smith"   → "John H. Smith"
                         ]

wikipediaURLToTitle :: String -> String
wikipediaURLToTitle u = trimTitle $ cleanAbstractsHTML $ replace "#" " § " $ urlDecode $ replace "% " "%25 " $ replace "_" " " $ replace "https://en.wikipedia.org/wiki/" "" u

-- title clean up: delete the period at the end of many titles, extraneous colon spacing, remove Arxiv's newline+double-space, and general whitespace cleaning
trimTitle :: String -> String
trimTitle [] = ""
trimTitle t = let t' = reverse $ sedMany [("†.*", ""), -- eg "Relation of Serum 25-Hydroxyvitamin D to Heart Rate and Cardiac Work (from the National Health and Nutrition Examination Surveys)†\n†Conflict of interest: Dr. Simpson receives support from Abbott Laboratories, Chicago, Illinois"
                                          ("([a-z])_ ", "\\1: ")] $ -- a lot of Elsevier papers replace colons with underscores (‽) in PDF metadata eg. "Compensatory conspicuous communication_ Low status increases jargon use"
                       replaceMany [(" : ", ": "), ("\n ", " ")] $ trim t in
                if not (null t') then reverse (if head t' == '.' then tail t' else t') else ""

-- run all necessary rewrites on a string to clean up malformation, inconsistent formatting, errors, convert to house style, etc
cleanAbstractsHTML :: String -> String
cleanAbstractsHTML = fixedPoint cleanAbstractsHTML'
 where cleanAbstractsHTML' :: String -> String
       cleanAbstractsHTML' = trim . sedMany [
         -- add newlines for readability
         ("<p><strong>\nAuthor summary\n</strong>:</p>\n<p>\n", "<p><strong>\nAuthor summary\n</strong>: ")
         , ("<strong>Abstract</strong>: ", "")
         , ("<p><strong>Abstract</strong>: <strong>Objective</strong>: ", "<p><strong>Objective</strong>: ")
         , ("<strong>Abstract</strong>\n<p>", "<p>")
         , ("<strong>Abstract</strong>\n \n ", "")
         , ("<p>Abstract. ", "<p>")
         , ("<strong>ABSTRACT</strong><br/>", "")
         , ("<strong>Abstract</strong><br/>", "")
         , ("<strong>One Sentence Summary</strong></p>\n<p>", "<strong>One Sentence Summary</strong>: ")
         , ("<strong>One Sentence Summary</strong></p> <p>", "<strong>One Sentence Summary</strong>: ")
         , ("R<sup>2</sup>D2", "R2D2")
         , ("</p> ?<p>", "</p>\n<p>")
         , ("</p>\n<p>", "</p> <p>")
         , ("</p>\n \n<p>", "</p>\n<p>")
         , ("  *", " ") -- squeeze whitespace
         , (" \\( ", " (")
         , (" \\) ", " )")
         , (" </p>", "</p>")
         , ("</a></p>", "</a>.</p>")
         , ("<br/> *</p>", "</p>")
         , ("<p> *", "<p>")
         , (" *</p>", "</p>")
         , ("<em>R</em>  *<sup>2</sup>", "<em>R</em><sup>2</sup>")

         -- regexp substitutions:
         -- rewrite *Markdown italics* to <em>HTML italics</em>, and strong/bold:
         , ("(.*)\\*(.+)\\*(.*)", "\\1<em>\\2</em>\\3")
         , ("(.*)\\*\\*(.+)\\*\\*(.*)", "\\1<strong>\\2</strong>\\3")
         , ("<p>This paper was accepted by [A-Z][a-z]+ [A-Z][a-z]+, .*\\.</p>", "")
         , (" ### Author Declarations .*$", "")
         , (" ### Competing Interest Statement .*$", "")
         , (" Study ([0-9][a-z]?)", " <strong>Study \\1</strong>")
         , (" Experiment ([0-9][a-z]?)", " <strong>Experiment \\1</strong>")
         -- <https://en.wikipedia.org/wiki/ClinicalTrials.gov>
         , (" (NCT[0-9]+)", "<a href=\"https://clinicaltrials.gov/show/\1\">\1</a>")
         , (" (NCT[0-9]+)</p>", "<a href=\"https://clinicaltrials.gov/show/\1\">\1</a>.</p>")
         -- cleanup bare URLs (particularly common in Arxiv abstracts when linking to Github):
         , (" (https?://[a-zA-Z0-9_\\.\\?/-]+)$", " <a href=\"\\1\">\\1</a>$")
         , (" (https?://[a-zA-Z0-9_\\.\\?/-]+)</p>", " <a href=\"\\1\">\\1</a></p>")
         , (" (https?://[a-zA-Z0-9_\\.\\?/-]+)\\)", " <a href=\"\\1\">\\1</a> )")
         , (" (https?://[a-zA-Z0-9_\\.\\?/-]+) \\.", " <a href=\"\\1\">\\1</a>.")
         , (" (https?://[a-zA-Z0-9_\\.\\?/-]+) ?\\.</p>", " <a href=\"\\1\">\\1</a>.</p>")
         , ("at:? (github.com/.*).</p>", "at <a href=\"https://\\1\">\\1</a>.</p>") -- "Code is available at github.com/microsoft/SPACH.</p>" / "Code will be released at: github.com/NVlabs/SegFormer.</p>"
         , (" (https://github.com/[a-zA-Z0-9_\\.\\?/-]+) ?\\.</p>", " <a href=\"\\1\">Github</a>.</p>")
         -- try to rewrite half-parenthesis lists like '(( 1) foo; 2) bar' into '(1) foo; (2) bar' for consistency & parentheses-checking:
         , ("\\(10\\) (.*) 11\\)", " (10) \\1 (11)")
         , (" 10\\) (.*) 11\\)", " (10) \\1 (11)")
         , ("\\(9\\) (.*) 10\\)", " (9) \\1 (10)")
         , (" 9\\) (.*) 10\\)", " (9) \\1 (10)")
         , ("\\(8\\) (.*) 9\\)", " (8) \\1 (9)")
         , (" 8\\) (.*) 9\\)", " (8) \\1 (9)")
         , ("\\(7\\) (.*) 8\\)", " (7) \\1 (8)")
         , (" 7\\) (.*) 8\\)", " (7) \\1 (8)")
         , ("\\(6\\) (.*) 7\\)", " (6) \\1 (7)")
         , (" 6\\) (.*) 7\\)", " (6) \\1 (7)")
         , ("\\(5\\) (.*) 6\\)", " (5) \\1 (6)")
         , (" 5\\) (.*) 6\\)", " (5) \\1 (6)")
         , ("\\(4\\) (.*) 5\\)", " (4) \\1 (5)")
         , (" 4\\) (.*) 5\\)", " (4) \\1 (5)")
         , ("\\(3\\) (.*) 4\\)", " (3) \\1 (4)")
         , (" 3\\) (.*) 4\\)", " (3) \\1 (4)")
         , ("\\(2\\) (.*) 3\\)", " (2) \\1 (3)")
         , (" 2\\) (.*) 3\\)", " (2) \\1 (3)")
         , ("\\(1\\) (.*) 2\\)", " (1) \\1 (2)")
         , (" 1\\) (.*) 2\\)", " (1) \\1 (2)")
         -- citations: eg '...biochemical programs (preconditioning)2,3,4. Under...'; we require 2 because 1 number is ambiguous & collides with chemistry/genetics.
         , ("([a-z[:punct:]])([0-9]+,[0-9]+)\\.", "\\1<sup>\\2</sup>.")
         , ("([a-z[:punct:]])([0-9]+,[0-9]+,[0-9]+)\\.", "\\1<sup>\\2</sup>.") -- '2,3,4.'
         , ("([a-z[:punct:]])([0-9]+,[0-9]+,[0-9]+,[0-9]+)\\.", "\\1<sup>\\2</sup>.")
         , ("([a-z[:punct:]])([0-9]+,[0-9]+,[0-9]+,[0-9]+,[0-9]+)\\.", "\\1<sup>\\2</sup>.")
         , ("([a-z[:punct:]])([0-9]+,[0-9]+,[0-9]+,[0-9]+,[0-9]+,[0-9]+)\\.", "\\1<sup>\\2</sup>.")
         , ("([a-z[:punct:]])([0-9]+,[0-9]+,[0-9]+,[0-9]+,[0-9]+,[0-9]+,[0-9]+)\\.", "\\1<sup>\\2</sup>.")
         , ("([0-9]) x ([0-9]+)\\(([0-9−-]+)\\)", "\\1 × \\2<sup>\\3</sup>") -- '~2.5 x 10(13)' → '~2.5 × 10<sup>13</sup>'
         -- common spelling error, 'a' → 'an':
         , (" a ([aeio][a-z]+)", " an \\1")
         -- - comma-separate at thousands for consistency:
         -- skip thousands, since clobbers citations like 'Herring 2009' (which turns into 'Herring 2,009')
         , (" ([0-9]+)([0-9][0-9][0-9])([0-9][0-9][0-9])",                                   " \\1,\\2,\\3")         -- millions
         , (" ([0-9]+)([0-9][0-9][0-9])([0-9][0-9][0-9])([0-9][0-9][0-9])",                  " \\1,\\2,\\3,\\4")     -- billions
         , (" ([0-9]+)([0-9][0-9][0-9])([0-9][0-9][0-9])([0-9][0-9][0-9])([0-9][0-9][0-9])", " \\1,\\2,\\3,\\4,\\5") -- trillions
         , ("([0-9]+) percent([ [:punct:]])", "\\1%\\2") -- eg '$22,000 (46 percent) higher annual early-career wages than they would'
         , ("([0-9][0-9]+) [xX] ([0-9][0-9]+) ", "\\1×\\2") -- "high fidelity generation of 1024 x 1024 images" / "0.85 X 30 mEq/kg"
         , ("([0-9][0-9]+) ?[xX] ?([0-9][0-9]+) ?px", "\\1×\\2px") --  "Alexnet performance for 16 x16 px features)."
         , ("([0-9]+)[ -]fold", "\\1×")
         , ("([0-9]+)[ -]times", "\\1×")
         , ("<br/> <strong>([A-Z][a-z]+)<\\/strong><p>", "<p><strong>\\1</strong>: ") --         <br/> <strong>Background</strong><p>
         , ("</p><strong>([A-Z][a-z]+)<\\/strong><p>", "</p> <p><strong>\\1</strong>: ")
         , ("<p><strong>([A-Z][a-z]+)<\\/strong>:</p> <p>", "<p><strong>\\1</strong>: ")
         , ("<p><strong>([A-Z][a-z]+ [A-Za-z]+)<\\/strong>:</p> <p>", "<p><strong>\\1</strong>: ")
         , ("<p><strong>([A-Z][a-z]+ [A-Za-z]+ [A-Za-z]+)<\\/strong>:</p> <p>", "<p><strong>\\1</strong>: ")
         , ("<p><strong>([A-Z][a-z]+ [A-Za-z]+ [A-Za-z]+ [A-Za-z]+)<\\/strong>:</p> <p>", "<p><strong>\\1</strong>: ")
         , ("<xref rid=\"sec[0-9]+\" ref-type=\"sec\">([A-Za-z]+ [0-9]+)</xref>", "<strong>\\1</strong>") -- PLOS: '<xref rid="sec022" ref-type="sec">Experiment 3</xref>' etc.
         , ("^en$", "")
         , (" ([0-9]) h ", " \\1h ") -- hour abbreviation
         , ("([0-9%]) – ([0-9])", "\\1–\\2") -- space-separated en-dash ranges eg. "with a range of ~0.60 – 0.71 for height"
         , ("([0-9%]) – ([a-z])", "\\1—\\2") -- a number-alphabet en-dash is usually an em-dash eg. "a Fréchet Inception Distance (FID) of 10.59 – beating the baseline BigGAN model—at"
         , ("([a-zA-Z]) – ([[:punct:]])", "\\1—\\2") -- en dash errors in WP abstracts: usually meant em-dash. eg. 'disc format – <a href="https://en.wikipedia.org/wiki/Universal_Media_Disc">Universal'
         , ("([[:punct:]]) – ([a-zA-Z])", "\\1—\\2")
         , ("([a-zA-Z’]) – ([a-zA-Z])", "\\1—\\2") -- eg: "Aspects of General Intelligence – a Deep Phenotyping Approach"
         , ("([a-zA-Z’]) - ([a-zA-Z])", "\\1—\\2") -- spaced hyphens: also usually em dashes: "Towards personalized human AI interaction - adapting the behavior of AI agents"
         , (" = -([0-9])", " = −\\1") -- eg. 'β = -0.08', HYPHEN to MINUS SIGN
         , ("× ?10[-–—]([0-9]+)", "× 10<sup>−\\1</sup>") -- the Unicode '×' seems to never match when used inside a range...?
         , ("([0-9]) [x×] 10[-–—]([0-9]+)", "\\1 × 10<sup>−\\2</sup>")
         , ("([0-9]) [x×] 10\\([-–—]([0-9]+)\\)", "\\1 × 10<sup>−\\2</sup>")
         , ("<sup>-([0-9]+)</sup>", "<sup>−\\1</sup>") -- eg. '10<sup>-7</sup>', HYPHEN to MINUS SIGN
         , ("([0-9]+%?)-([0-9]+)", "\\1–\\2")
         , ("([0-9]) %", "\\1%")
         , ("([.0-9]+)[xX]", "\\1×")
         , ("=-\\.([.0-9]+)", " = -0.\\1")
         , ("([0-9]*[02456789])th ", "\\1<sup>th</sup> ")
         , ("([0-9]*[1])st ",        "\\1<sup>st</sup> ")
         , ("([0-9]*[3])rd ",        "\\1<sup>rd</sup> ")
         , ("\\(JEL [A-Z][0-9][0-9]+\\)\\.?", "")
         , (" \\(JEL [A-Z][0-9][0-9], .* [A-Z][0-9][0-9]\\)", "") -- rm AERA classification tags they stick into the Crossref abstracts
         , ("CI=([.0-9])", "CI = \\1") -- 'CI=0.90' → 'CI = 0.90'
         , ("RR=([.0-9])", "RR = \\1") -- 'RR=2.9' → 'RR = 2.09'
         , ("OR=([.0-9])", "OR = \\1") -- 'OR=2.9' → 'OR = 2.09'
         , ("AOR=([.0-9])", "AOR = \\1") -- 'AOR=2.9' → 'AOR = 2.09'
         -- math regexes
         , ("<span class=\"math inline\">\\\\\\(([a-zA-Z])\\\\\\)</span>", "<em>\\1</em>") -- '<span class="math inline">\(d\)</span>', 'the state matrix <span class="math inline">\(A\)</span>'
         , ("<span class=\"math inline\">\\\\\\(([0-9.]+)\\\\\\)</span>", "\\1") -- '<span class="math inline">\(30\)</span>'
         , ("\\$([.0-9]+) \\\\cdot ([.0-9]+)\\^([.0-9]+)\\$",             "\\1 × \\2^\\3^")
         , ("\\$([.0-9]+) \\\\cdot ([.0-9]+)\\^\\{([.0-9]+)\\}\\$",       "\\1 × \\2^\\3^")
         , ("<span class=\"math inline\">\\\\\\(([0-9.]+) ?\\\\times\\\\\\)</span>", "\\1×") -- '<span class="math inline">\(1.5\times\)</span>'
         , ("<span class=\"math inline\">\\\\\\(([0-9.]+) ?\\\\times ([0-9.]+)\\\\\\)</span>", "\\1×\\2") -- '<span class="math inline">\(224\times\ 224)</span>'
         , ("<span class=\"math inline\">\\\\\\(([0-9.]+)\\\\\\%\\\\\\)</span>", "\\1%") -- '<span class=\"math inline\">\\(83.6\\%\\)</span>'
         , ("<span class=\"math inline\">\\\\\\(\\\\texttt\\{([A-Za-z]+)\\}\\\\\\)</span>", "<code>\\1</code>") -- 'we present the <span class=\"math inline\">\\(\\texttt{GamePhysics}\\)</span> dataset'
         , ("<span class=\"math inline\">\\\\\\(\\\\textbf\\{([A-Za-z]+)\\}\\\\\\)</span>", "<strong>\\1</strong>") -- 'We dub ρ the <span class="math inline">\(\textbf{polarity}\)</span> parameter'
         , ("<span class=\"math inline\">\\\\\\(\\\\times\\\\\\)</span>", "×") -- '<span class="math inline">\(\times\)</span>'
         , ("<span class=\"math inline\">\\\\\\(([0-9]*)\\^([0-9]*)\\\\\\)</span>", "\\1<sup>\\2</sup>") -- '<span class="math inline">\(10^4\)</span>'
         , ("<span class=\"math inline\">\\\\\\(([0-9]*)\\^{([0-9]*)}\\\\\\)</span>", "\\1<sup>\\2</sup>") -- '<span class="math inline">\(10^{40}\)</span>'
         , ("([A-z][a-z]+) ?et ?al ?\\(([0-9][0-9][0-9][0-9])\\)", "\\1 et al \\2") -- 'Dette et al (2013)'
         , ("([A-Z][a-z]+) and ([A-Z][a-z]+),? ([0-9]+)", "\\1 & \\2 \\3") -- 'Foo and Bar 1999', 'Foo and Bar, 1999' → 'Foo & Bar 1999'; 'et al' is handled by Pandoc already
         , ("([A-Z][a-z]+) &amp; ([A-Z][a-z]+), ([12][0-9][0-9][0-9])", "\\1 & \\2 \\3")
         , ("([A-Z][a-z]+) & ([A-Z][a-z]+), ([12][0-9][0-9][0-9])",     "\\1 & \\2 \\3")
         , ("<br/>    <strong>([a-zA-Z]+)</strong><br/><p>", "<p><strong>\\1</strong>: ")
         , ("<strong>([a-zA-Z0-9_]+)</strong>:<p>", "<p><strong>\\1</strong>: ")
         , ("<jats:title>([a-zA-Z0-9_]+):</jats:title><jats:p>", "<p><strong>\\1</strong>: ")
         , ("<jats:title>([a-zA-Z0-9_]+)\\.</jats:title><jats:p>", "<p><strong>\\1</strong>: ")
         , ("<jats:sec id=\"[a-zA-Z0-9_]+\">", "")
         , ("<jats:sec id=\"[a-zA-Z0-9_]+\" sec-type=\"[a-z]+\">", "")
         , (" © [0-9]+ European Association of Personality Psychology", "")
         ] .
         -- simple string substitutions:
         replaceMany [
         ("<span style=\"font-weight:normal\"> </span>", "")
         , ("href=\"github.com", "href=\"https://github.com")
         , ("https://github.com/deepmind/ deepmind-research/", "https://github.com/deepmind/deepmind-research/")
         , ("i . e .,", "ie.")
         , ("<p><strong>Motivation</strong></p>\n<p>", "<p><strong>Motivation</strong>: ")
         , ("<p><strong>Availability</strong></p>\n<p>", "<p><strong>Availability</strong>: ")
         , ("<p>[<strong>Keywords</strong>: ]</p>", "")
         , ("<strong>Null</strong>: ", "")
         , ("<em>p</em>=", "<em>p</em> = ")
         , ("β=", "β = ")
         , ("\8217=", "\8217 = ")
         , (" the the ", " the ")
         , (" a imbalance", " an imbalance")
         , (",”", "”,")
         , ("<span style=\"display:inline-block;vertical-align:-0.4em;font-size:80%;text-align:left\"><sup></sup><br /><sub>", "")
         , ("<sup>+</sup>", "⁺")
         , ("</sub>=", "</sub> = ") -- eg. '<em>r</em><sub>A</sub>=0.28'
         , ("<sup></sup>", "")
         , ("<sub></sub>", "")
         , ("<i>", "<em>")
         , ("</i>", "</em>")
           -- math substitutions:
         , ("<span class=\"math inline\">\\(2^{\\Omega(k)}\\)</span>", "2<sup>Ω(<em>k</em>)</sup>")
         , ("<span class=\"math inline\">\\(k = \\log n\\)</span>", "<em>k</em> = log <em>n</em>")
         , ("<span class=\"math inline\">\\(\\perp\\)</span>", "⟂")
         , ("<span class=\"math inline\">\\(^{\\perp}\\)</span>", "<sup>⟂</sup>")
         , ("<span class=\"math inline\">\\(^\\circ\\)</span>", "°")
         , ("<span class=\"math inline\">\\(\\pi_1\\)</span>", "π<sub>1</sub>")
         , ("<span class=\"math inline\">\\(7.5\\sim9.5\\times\\)</span>", "7.5–9.5×")
         , ("<span class=\"math inline\">\times</span>", "×")
         , ("$\\mu$", "μ")
         , ("<span class=\"math inline\">\\(\\mu\\)</span>", "μ")
         , ("<span class=\"math inline\">\\mu</span>", "μ")
         , ("<span class=\"math inline\">\\(S&#39;\\)</span>", "<em>S</em>′")
         , ("<span class=\"math inline\">\\(S&#39; \\subset S\\)</span>", "<em>S</em>′ ⊂ <em>S</em>")
         , ("<span class=\"math inline\">\\(2^S \\to \\mathbb{R}\\)</span>", "2<sup><em>S</em></sup> ⟶ ℝ")
         , ("<span class=\"math inline\">\\(μ\\)</span>", "𝜇")
         , ("<span class=\"math inline\">\\(e^{-kq^2}.\\)</span>", "<em>e</em><sup>−<em>kq</em><sup>2</sup></sup>")
         , ("<span class=\"math inline\">\\(\\approx\\)</span>", "~")
         , ("<span class=\"math inline\">\\(1.644934\\approx \\pi^2/6\\)</span>", "1.644934 ≈ π<sup>2</sup>⁄6")
         , ("<span class=\"math inline\">\\(\\operatorname{bessel0}(10)\\approx \\frac{\\sin(10)+\\cos(10)}{\\sqrt{\\pi x}}\\)</span>", "<code>bessel0(<em>x</em>) ≈ sin(<em>x</em>)+cos(<em>x</em>) / √π<em>x</em>")
         , ("<span class=\"math inline\">\\(P_B(f\\mid S)\\)</span>", "<em>P</em><sub><em>b</em></sub>(<em>f</em>|<em>S</em>)")
         , ("<span class=\"math inline\">\\(P_{SGD}(f\\mid S)\\)</span>", "<em>P</em><sub><em>SGD</em></sub>(<em>f</em>|<em>S</em>)")
         , ("<span class=\"math inline\">\\(6\\times 10^{-6}\\)</span>", "6×10<sup>−6</sup>")
         , ("<span class=\"math inline\">\\(\\sqrt{n/k}\\)</span>", "√<em>n</em>⁄<em>k</em>")
         , ("<span class=\"math inline\">\\(O\\sqrt{n})\\)</span>", "𝒪(√<em>n</em>)")
         , ("<span class=\\\"math inline\\\">\\\\(O(\\\\sqrt{n})\\\\)</span>", "𝒪(√<em>n</em>)")
         , ("<span class=\"math inline\">\\(128/255\\)</span>", "128⁄255")
         , ("<span class=\"math inline\">\\(8/255\\)</span>", "8⁄255")
         , (" <span class=\"math inline\">\\(-\\)</span> ", "—")
         , ("<span class=\"math inline\">\\(\\nu\\)</span>", "ν")
         , ("<span class=\"math inline\">\\(O(n^2)\\)</span>", "𝒪(<em>n</em><sup>2</sup>)")
         , ("<span class=\"math inline\">\\(O(N^2)\\)</span>", "𝒪(<em>n</em><sup>2</sup>)")
         , ("<span class=\"math inline\">\\(\\alpha &gt; 0\\)</span>", "α > 0")
         , ("<span class=\"math inline\">\\(n^{-\\alpha}\\)</span>", "<em>n</em><sup>−α</sup>")
         , ("<span class=\"math inline\">\\(L_2\\)</span>", "<em>L</em><sub>2</sub>")
         , ("<span class=\"math inline\">\\(L_1\\)</span>", "<em>L</em><sub>1</sub>")
         , ("<span class=\"math inline\">\\(L_0\\)</span>", "<em>L</em><sub>0</sub>")
         , ("<span class=\"math inline\">\\(\\sim5\\%\\)</span>", "~5%")
         , ("<span class=\"math inline\">\\(\\text{RL}^2\\)</span>", "RL<sup>2</sup>")
         , ("<span class=\"math inline\">\\(\\dot{x} = Ax + Bu, y = Cx + Du\\)</span>", "<em>͘x</em> = <em>Ax</em> + <em>Bu</em>, <em>y</em> = <em>Cx</em> + <em>Du</em>")
         , ("<span class=\"math inline\">\\(u \\mapsto y\\)</span>", "<em>u</em> ↦ <em>y</em>")
         , ("<span class=\"math inline\">\\(\\sqrt{T}\\)</span>", "√<em>T</em>")
         , ("<span class=\"math inline\">\\(\\sqrt{H}\\)</span>", "√<em>H</em>")
         , ("<span class=\"math inline\">\\(\\tilde{O}(\\sqrt{H^3 SAT})\\)</span>", "𝒪(√<em>H</em><sup>3</sup><em>SAT</em>)")
         , ("<span class=\"math inline\">\\(\\gamma = 0.99\\)</span>", "γ = 0.99")
         , ("<span class=\"math inline\">\\(\\gamma = 0.999\\)</span>", "γ = 0.999")
         , ("<span class=\"math inline\">\\(d^{5/4}\\)</span>", "<em>d</em><sup>5⁄5</sup>")
         , ("<span class=\"math inline\">\\(d^{3/2}\\)</span>", "<em>d</em><sup>3⁄2</sup>")
         , ("<span class=\"math inline\">\\(x&#39;(t) = Ax(t) + Bu(t), y(t) = Cx(t) + Du(t)\\)</span>", "<em>x&#39;(t)</em> = <em>Ax(t)</em> + <em>Bu(t)</em>, <em>y(t)</em> = <em>Cx(t)</em> + <em>Du(t)</em>")
         , ("<span class=\"math inline\">\\(_{50}\\)</span>", "<sub>50</sub>")
         , ("<span class=\"math inline\">\\(_r\\)</span>", "<sub><em>r</em></sub>")
         , ("<span class=\"math inline\">\\(tanh\\)</span>", "<em>tanh</em>")
         , ("<span class=\"texhtml \">O(log <i>n</i>)</span>", "𝒪(log <em>n</em>)")
         , ("<span class=\"math inline\">\\(\\mathcal{O}(100)\\)</span>", "𝒪(100)")
         , ("<span class=\"math inline\">\\(O(1)\\)</span>", "𝒪(1)")
         , ("<span class=\"math inline\">\\(\\mathcal{O}(1)\\)</span>", "𝒪(1)")
         , ("<span class=\"texhtml \">\\mathcal{O}(log <i>n</i>)</span>", "𝒪(log <em>n</em>)")
         , ("$O(log n)$", "𝒪(log <em>n</em>)")
         , ("<span class=\"math inline\">\\(\\log n\\)</span>", "log <em>n</em>")
         , ("$\\mathcal{O}(log n)$", "𝒪(log <em>n</em>)")
         , ("<span class=\"math inline\">\\(\\mathcal{O}(L^2)\\)</span>", "𝒪(<em>L</em><sup>2</sup>)")
         , ("<span class=\"math inline\">\\(\\mathcal{O}(L\\log(L))\\)</span>", "𝒪(log <em>L</em>)")
         , ("<span class=\"math inline\">\\(\\mathcal{O}(L\\sqrt{L})\\)</span>", "𝒪(√<em>L</em>)")
         , ("<span class=\"math inline\">\\(\\mathcal{O}(1/\\sqrt{d})\\)</span>", "𝒪(1⁄√<em>d</em>)")
         , ("<span class=\"math inline\">\\(\\mathcal{O}(n\\log n)\\)</span>", "𝒪(<em>n</em> log <em>n</em>)")
         , ("<span class=\"math inline\">\\(O(\\log n)\\)</span>", "𝒪(log <em>n</em>)")
         , ("<span class=\"math inline\">\\(n \\approx d\\)</span>", "<em>n</em> ≈ <em>d</em>")
         , ("<span class=\"math inline\">\\(k \\leq d\\)</span>", "<em>k</em> ≤ <em>d</em>")
         , ("<span class=\"math inline\">\\(n \\approx d^p\\)</span>", "<em>n</em> ≈ <em>d<sup>p</sup></em>")
         , ("$\\mathrm{sinc}(ax)$", "sinc(<em>ax</em>)")
         , ("<span class=\"texhtml \">\\mathrm{sinc}(ax)</span>", "sinc(<em>ax</em>)")
         , ("$\\mathrm{sinc}(x)=\\sin(x)/x$", "sinc(<em>x</em>) = sin(<em>x</em>)⁄<em>x</em>")
         , ("$x$", "<em>x</em>")
         , ("$\\mathrm{sinc}(0)=1$", "sinc(0) = 1")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">n</span>", "<em>n</em>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">x</span>", "<em>x</em>")
         , ("<span><span class=\"texhtml mvar\" style=\"font-style:italic\">π</span></span>", "<em>π</em>")
         , ("<span class=\"texhtml \"><i>a</i> + <i>b i</i></span>", "<em>a</em> + <em>b i</em>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">a</span>", "<em>a</em>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">b</span>", "<em>b</em>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">i</span>", "<em>i</em>")
         , ("<span class=\"texhtml \">ℂ</span>", "ℂ")
         , ("<span class=\"texhtml \"><strong>C</strong></span>", "<strong>C</strong>")
         , ("<span class=\"texhtml \"><i>x</i><sup>2</sup> + 1 = 0</span>", "<em>x</em><sup>2</sup> + 1 = 0")
         , ("<span class=\"texhtml \">2 + 3<i>i</i></span>", "2 + 3<em>i</em>")
         , ("<span class=\"texhtml \"><i>Y</i> = ln(<i>X</i>)</span>", "<em>Y</em> = ln(<em>X</em>)")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">Y</span>", "<em>Y</em>")
         , ("<span class=\"texhtml \"><i>X</i> = exp(<i>Y</i>)</span>", "<em>X</em> = exp(<em>Y</em>)")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">p</span>", "<em>p</em>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">n</span>", "<em>n</em>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">π</span>", "<em>π</em>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\"><strong>c</strong></span>", "<strong><em>c</em></strong>")
         , ("<span class=\"texhtml \"><strong>C</strong></span>", "<strong>C</strong>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">c</span>", "<em>c</em>")
         , ("<span class=\"math inline\">\\(\\times\\)</span>", "×")
         , ("<span class=\"math inline\">\\(n\\)</span>", "<em>n</em>")
         , ("<span class=\"math inline\">\\(\\pi\\)</span>", "π")
         , ("<span class=\"math inline\">\\(1,...,n\\)</span>", "1,...,<em>n</em>")
         , ("<span class=\"math inline\">\\(\\pi^*\\)</span>", "π<sup>*</sup>")
         , ("<span class=\"math inline\">\\(c\\)</span>", "<em>c</em>")
         , ("<span class=\"math inline\">\\(G\\)</span>", "<em>G</em>")
         , ("<span class=\"math inline\">\\(\\hbar\\)</span>", "ℏ")
         , ("<span class=\"math inline\">\\(n\\)</span>", "<em>n</em>")
         , ("<span class=\"math inline\">\\(\\alpha\\)</span>", "α")
         , ("<span class=\"math inline\">\\(n^{1/4}\\)</span>", "<em>n</em><sup>1⁄4</sup>")
         , ("<span class=\"math inline\">\\(n^{1/3}\\)</span>", "<em>n</em><sup>1⁄3</sup>")
         , ("<span class=\"math inline\">\\(c n^{1/3}\\)</span>", "<em>cn</em><sup>1⁄3</sup>")
         , ("<span class=\"math inline\">\\(c&gt;0\\)</span>", "<em>c</em> &gt; 0")
         , ("<span class=\"math inline\">\\(n^{1/2}\\)</span>", "<em>n</em><sup>1⁄2</sup>")
         , ("<span class=\"math inline\">\\(n^{-1/2}\\)</span>", "<em>n</em><sup>−1⁄2</sup>")
         , ("<span class=\"math inline\">\\(n^{-1}\\)</span>", "<em>n</em><sup>−1</sup>")
         , ("<span class=\"math inline\">\\(n^{-\beta}\\)</span>", "<em>n<sup>−β</sup></em>")
         , ("<span class=\"math inline\">\\(\\beta&gt;0\\)</span>", "<em>β</em> > 0")
         , ("<span class=\"math inline\">\\(1,\\!000\\times\\)</span>", "1,000×")
         , ("<span class=\"math inline\">\\(3,\\!000\\times\\)</span>", "3,000×")
         , ("<span class=\"math inline\">\\(100,\\!000\\)</span>", "100,000")
         , ("<span class=\"math inline\">\\(k\\)</span>", "<em>k</em>")
         , ("<span class=\"math inline\">\\(k \\rightarrow \\infty\\)</span>", "<em>k</em> → ∞")
         , ("<span class=\"math inline\">\\(\\infty\\)</span>", "∞")
         , ("<span class=\"math inline\">\\(1/2 H_n\\)</span>", "1⁄2<em>H<sub>n</sub></em>")
         , ("<span class=\"math inline\">\\(H_n \\sim \\ln n\\)</span>", "<em>H<sub>n</sub></em> ln <em>n</em>")
         , ("<span class=\"math inline\">\\(f_\\theta\\)</span>", "<em>f</em><sub>θ</sub>")
         , ("<math>A</math>", "<em>A</em>")
         , ("<math>B</math>", "<em>B</em>")
         , ("<math>C</math>", "<em>C</em>")
         , ("<math>S</math>", "<em>S</em>")
         , (" O(sqrt(n)) ", " 𝒪(√<em>n</em>) ")
         , ("O(log n) ", "𝒪(log <em>n</em>) ")
         , (" O(log n)", " 𝒪(log <em>n</em>)")
         , (" O(n log n) ", " 𝒪(<em>n</em> log <em>n</em>) ")
         , ("<span class=\"math inline\">\\(O(K^2 \\log T)\\)</span>", "𝒪(<em>K</em><sup>2</sup> log <em>T</em>)")
         , ("<span class=\"math inline\">\\(O(K \\log T + K^2 \\log \\log T)\\)</span>", "𝒪(<em>K</em> log <em>T</em> + <em>K</em><sup>2</sup> log log <em>T</em>)")
         , ("<span class=\"math inline\">\\(Q\\)</span>", "<em>Q</em>")
         , ("<span></span>-greedy", "ε-greedy")
         , ("{\\epsilon}-greedy", "ε-greedy")
         , ("<span class=\"math inline\">\\(\\epsilon\\)</span>", "ε")
         , ("<span class=\"math inline\">\\(\\rightarrow\\)</span>", "→")
         , ("<span class=\"math inline\">\\(\\leftarrow\\)</span>", "←")
         , ("<span class=\"math inline\">\\(D\\)</span>", "<em>D</em>")
         , ("<span class=\"math inline\">\\(G\\)</span>", "<em>G</em>")
         , ("<span class=\"math inline\">\\(K\\)</span>", "<em>K</em>")
         , ("<span class=\"math inline\">\\(\\sin\\Theta\\)</span>", "sinΘ")
         , ("<span class=\"math inline\">\\(\\ell_\\infty\\)</span>", "𝓁<sub>∞</sub>")
         , ("<span class=\"math inline\">\\(\\ell_2\\)</span>", "𝓁<sub>2</sub>")
         , ("<span class=\"math inline\">\\(\\ell_1\\)</span>", "𝓁<sub>1</sub>")
         , ("<span class=\"math inline\">\\(l_1\\)</span>", "𝓁<sub>1</sub>")
         , ("<span class=\"math inline\">\\(2.4\\)</span>", "2.4")
         , ("<span class=\"math inline\">\\(47.1\\)</span>", "47.1")
         , ("<span class=\"math inline\">\\(42.5\\)</span>", "42.5")
         , ("<span class=\"math inline\">\\(f(x) = x \\cdot \\text{sigmoid}(\\beta x)\\)</span>", "<em>f(x)</em> = <em>x</em> × sigmoid(β <em>x</em>)")
         , ("<span class=\"math inline\">\\(<em>r</em> = 0.99\\)</span>", "<em>r</em> = 0.99")
         , ("<span class=\"math inline\">\\(0.96\\)</span>", "0.96")
         , ("<span class=\"math inline\">\\(\\it<br/>performance\\)</span>", "<em>performance</em>")
         , ("<span class=\"math inline\">\\(\\it and\\)</span>", "<span class=\"math inline\">\\(\\it also\\)</span> <em>and also</em>")
         , ("<span class=\"math inline\">\\(\\sim 6\\)</span>", "~6")
         , ("<span class=\"math inline\">\\(\\sim\\)</span>", "~")
         , ("<span class=\"math inline\">\\({\\sim}\\)</span>", "~")
         , ("<span class=\"math inline\">\\(\\sim 10^3\\)</span>", "~10<sup>3</sup>")
         , ("<span class=\"math inline\">\\(5\\%-35\\%\\)</span>", "5%–35%")
         , ("<span class=\"math inline\">\\(124/144\\)</span>", "124⁄144")
         , ("<span class=\"math inline\">\\(86\\%\\)</span>", "86%")
         , ("<span class=\"math inline\">\\(9.3\\%\\)</span>", "9.3%")
         , ("<span class=\"math inline\">\\(4.5\\%\\)</span>", "4.5%")
         , ("<span class=\"math inline\">\\(\\textit{Magic: The Gathering}\\)</span>", "<em>Magic: The Gathering</em>")
         , ("<span class=\"math inline\">\\(\\textit{Magic}\\)</span>", "<em>Magic</em>")
         , ("<span class=\"math inline\">\\(O(n \\sqrt{n})\\)</span>", "𝒪(<em>n</em> √<em>n</em>)")
         , ("<span class=\"math inline\">\\(O(\\sqrt{T})\\)</span>", "𝒪(√<em>T</em>)")
         , ("<span class=\"math inline\">\\(\\textit{Embedded agents}\\)</span>", "<em>Embedded agents</em>")
         , ("<span class=\"math inline\">\\(\\textit{wirehead}\\)</span>", "<em>wirehead</em>")
         , ("<span class=\"math inline\">\\(L\\)</span>", "<em>L</em>")
         , ("<span class=\"math inline\">\\(O(L(\\log L)^{2})\\)</span>", "𝒪(<em>L</em>(log <em>L</em>)<sup>2</sup>)")
         , ("<span class=\"math inline\">\\(_{16}\\)</span>", "<sub>16</sub>")
         , ("<span class=\"math inline\">\\(&gt;\\)</span>", "&gt;")
         , ("<span class=\"math inline\">\\(O(k\\cdot n\\log (n/k))\\)</span>", " 𝒪(<em>k</em> × log(<em>n</em>⁄<em>k</em>))")
         , ("<span class=\"math inline\">\\(O(D^3)\\)</span>", "𝒪(<em>D</em><sup>3</sup>)")
         , ("<span class=\"math inline\">\\(O(C)\\)</span>", "𝒪(<em>C</em>)")
         , ("O(<span class=\"math inline\">\\(L^2\\)</span>", "𝒪(<em>L</em><sup>2</sup>)")
         , ("<span class=\"math inline\">\\(L\\)</span>", "<em>L</em>")
         , ("<span class=\"math inline\">\\(rho\\)</span>", "ρ")
         , ("<span class=\"math inline\">\\(rho &gt; 0\\)</span>", "ρ > 0")
         , ("<span class=\"math inline\">\\(rho &lt; 0\\)</span>", "ρ < 0")
         , ("<span class=\"math inline\">\\(N\\)</span>", "<em>N</em>")
         , ("O(<span class=\"math inline\">\\(L\\log L\\)</span>", "𝒪(<em>L</em> log <em>L</em>)")
         , ("<span class=\"math inline\">\\(\\Delta^0_n\\)</span>", "Δ<span class=\"subsup\"><sup>0</sup><sub><em>n</em></sub></span>")
         , ("<span class=\"math inline\">\\(\\tt KRISS\\)</span>", "<code>KRISS</code>")
         , ("<span class=\"math inline\">\\(\\tt KRISSBERT\\)</span>", "<code>KRISSBERT</code>")
         , ("<span class=\"math inline\">\\(X_1,\\ldots,X_p\\)</span>", "<em>X</em><sub>1</sub>,...,<em>X</em><sub><em>p</em></sub>")
         , ("<span class=\"math inline\">\\([0,1]\\)</span>", "[0,1]")
         , ("<span class=\"math inline\">\\(R^2\\)</span>", "<em>R</em><sup>2</sup>")
         , ("<span class=\"math inline\">\\(m^{1+o(1)}\\)</span>", "<em>m</em><sup>1+<em>o</em>(1)</sup>")
         , ("<span class=\"math inline\">\\(1,000\\times\\)</span>", "1,000×")
         , ("<span class=\"math inline\">\\(10^5\\times\\)</span>", "10<sup>5</sup>×")
         , ("<span class=\"math inline\">\\(4\\sim 16\\times\\)</span>", "4–16×")
         , ("<span class=\"math inline\">\\(\\exp({\\Omega}(d))\\)</span>", "exp(Ω(<em>d</em>))")
         , ("<span class=\"math inline\">\\(\\exp({\\mathcal{O}}(k))\\)</span>", "exp(𝒪(<em>k</em>))")
         , ("<span class=\"math inline\">\\(\\mathcal{M}\\)</span>", "𝓜")
         , ("<span class=\"math inline\">\\(p_T\\)</span>", "<em>p<sub>T</sub></em>")
         , ("<span class=\"math inline\">\\(\\nabla \\log p_t\\)</span>", "∇ log <em>p<sub>t</sub></em>")
         , ("<span class=\"math inline\">\\(k \\ll d\\)</span>", "<em>k</em> ≪ <em>d</em>")
         , ("<span class=\"math inline\">\\(a^nb^mc^{n+m}\\)</span>", "<em>a<sup>n</sup>b<sup>m</sup>c<sup>n+m</sup>")
         , ("<span class=\"math inline\">\\(a^nb^{2n}\\)</span>", "<em>a<sup>n</sup>b<sup>2n</sup>")
         , ("<span class=\"math inline\">\\(a^nb^nc^n\\)</span>", "<em>a<sup>n</sup>b<sup>n</sup>c<sup>n</sup>")
         , ("<span class=\"math inline\">\\(a^nb^n\\)</span>", "<em>a<sup>n</sup>b<sup>n</sup>")
         , ("<span class=\"math inline\">\\(G: X \\rightarrow Y\\)</span>", "<em>G : X → Y</em>")
         , ("<span class=\"math inline\">\\(G(X)\\)</span>", "<em>G(X)</em>")
         , ("<span class=\"math inline\">\\(F: Y \\rightarrow X\\)</span>", "<em>F : Y → X</em>")
         , ("<span class=\"math inline\">\\(F(G(X)) \\approx X\\)</span>", "<em>F(G(X)) ≈ X</em>")
         , ("<span class=\"math inline\">\\(\\boldsymbol{sponge} \\sim\\boldsymbol{examples}\\)</span>", "<strong>sponge examples</strong>")
         , ("<span class=\"math inline\">\\(\\kappa\\)</span>", "𝜅")
         , (" <span class=\"math inline\">\\(\\unicode{x2014}\\)</span> ", "—")
         , ("<span class=\"math inline\">\\(x_i \\in {\\mathbb R}^p\\)</span>", "<em>x<sub>i</sub></em> ∈ ℝ<sup><em>p</em></sup>")
         , ("<span class=\"math inline\">\\(x_i = \\Sigma^{1/2} z_i\\)</span>", "<em>x<sub>i</sub></em> = ∑<sup>1⁄2</sup><em>z<sub>i</sub></em>")
         , ("<span class=\"math inline\">\\(z_i \\in {\\mathbb R}^p\\)</span>", "<em>z<sub>i</sub></em> ∈ ℝ<sup><em>p</em></sup>")
         , ("<span class=\"math inline\">\\(x_i = \\varphi(W z_i)\\)</span>", "<em>x<sub>i</sub></em> = ϕ(<em>Wz<sub>i</sub></em>")
         , ("<span class=\"math inline\">\\(z_i \\in {\\mathbb R}^d\\)</span>", "<em>z<sub>i</sub></em> ∈ ℝ<sup><em>d</em></sup>")
         , ("<span class=\"math inline\">\\(W \\in {\\mathbb R}^{p \\times d}\\)</span>", "<em>W</em> ∈ ℝ<sup><em>p</em>×<em>d</em></sup>")
         , ("<span class=\"math inline\">\\(\\varphi\\)</span>", "ϕ")
         , ("<span class=\"math inline\">\\(x\\mathbf{1}_{x&gt;0}\\)</span>", "<em>x</em><strong>1</strong><sub><em>x</em>&gt;0</sub>")
         , ("<span class=\"math inline\">\\(\\Phi(10)\\)</span>", "Φ(<em>x</em>)")
         , ("<span class=\"math inline\">\\(x\\Phi(10)\\)</span>", "<em>x</em>Φ(<em>x</em>)")
         , ("<span class=\"math inline\">\\(W z_i\\)</span>", "<em>Wz<sub>i</sub></em>")
         , ("<span class=\"math inline\">\\({\\raise.17ex\\hbox{<span class=\"math inline\">~</span>}}\\)</span>", "~")
         , ("<span class=\"math inline\">\\(z=0\\)</span>", "<em>z</em> = 0")
         , ("<span class=\"math inline\">\\(r \\to\\infty\\)</span>", "<em>r</em> → ∞")
         , ("<span class=\"math inline\">\\(N_A/(N_A+N_B)\\)</span>", "<em>N<sub>a</sub></em> / (<em>N<sub>A</sub></em> + <em>N<sub>B</sub></em>)")
         , ("<span class=\"math inline\">\\(N_A\\)</span>", "<em>N<sub>A</sub></em>")
         , ("<span class=\"math inline\">\\(N_B\\)</span>", "<em>N<sub>B</sub></em>")
         , ("<span class=\"math inline\">\\(N_A \\ll N_B\\)</span>", "<em>N<sub>A</sub></em> ≪ <em>N<sub>B</sub></em>")
         , ("<span class=\"math inline\">\\(\\dot{M}\\)</span>", "<em>̇M</em>")
         , ("<span class=\"math inline\">\\(0.15{-}1.3 {\\rm M}_{\\odot}\\)</span>", "0.15–1.3M<sub>☉</sub>")
         , ("<span class=\"math inline\">\\({\\sim} 0.3 {\\rm M}_{\\odot}\\)</span>", "~0.3M<sub>☉</sub>")
         , ("<span class=\"math inline\">\\({\\rm M}_{\\odot}\\)</span>", "M<sub>☉</sub>")
         , ("<span class=\"math inline\">\\({\\sim}0.02 {\\rm M}_{\\mathrm{Ceres}}\\)</span>", "~0.02M<sub><a href=\"https://en.wikipedia.org/wiki/Ceres_(dwarf_planet)\">Ceres</a></sub>")
         , ("<span class=\"math inline\">\\(ii\\)</span>", "1")
         , ("<span class=\"math inline\">\\(ii\\)</span>", "2")
         , ("<span class=\"math inline\">\\(iii\\)</span>", "3")
         , ("<span class=\"math inline\">\\(iv\\)</span>", "4")
         , (" TD()", " TD(λ)")
         , ("({\\lambda})", "(λ)")
         , ("O((log n log log n)^2)", "𝒪(log<sup>2</sup> <em>n</em> log log <em>n</em>)")
         , ("O(m log^2 n)", "𝒪(<em>m</em> log <em>n</em> + <em>n</em> log<sup>2</sup> <em>n</em>)")
         , ("O(n^2)", "𝒪(<em>n</em><sup>2</sup>)")
         , ("O(nm)", "𝒪(<em>n</em>×<em>m</em>)")
         , ("O(n)", "𝒪(<em>n</em>)")
         , ("O(N) ", "𝒪(<em>N</em>) ")
         , (" O(N)", " 𝒪(<em>N</em>)")
         , (" n ", " <em>n</em> ")
         , ("(n ", "<em>n</em> ") -- it's safe to replace 'n', but not 'N' because 'N' is used fairly often in chemistry for nitrogen
         , ("<span class=\"math inline\">\\(n^{O(k)}\\)</span>", "<em>n</em><sup>𝒪(<em>k</em>)</sup>")
         , (" N pixels", " <em>N</em> pixels")
         , ("a n layer", "a <em>n</em> layer")
         , (" n-step", " <em>n</em>-step")
         , ("$f(x; x_0,\\gamma)$", "<em>f(x; x<sub>0</sub>,γ")
         , ("$(x_0,\\gamma)$", "<em>(x<sub>0</sub>, γ)</em>")
         , ("$e=mc^2$", "<em>e</em> = <em>mc</em><sup>2</sup>")
         , ("$\frac{4}{3} \\cdot \\pi \\cdot r^3$", "4⁄3 × π × _r_^3^")
         , (" Lp", " 𝓁<sub><em>p</em></sub>")
         , (" L2", " 𝓁<sub>2</sub>")
         , ("l2-distance", "𝓁<sub>2</sub>-distance")
         , ("l1-distance", "𝓁<sub>1</sub>-distance")
         , (" L1", " 𝓁<sub>1</sub>")
         , ("L1-penalized", "𝓁<sub>1</sub>-penalized")
         , ("L1-regularized", "𝓁<sub>1</sub>-regularized")
         , ("L1 loss", "𝓁<sub>1</sub> loss")
         , (" L0", " 𝓁<sub>0</sub>")
         , (" L-infinity", " 𝓁<sub>∞</sub>")
         , ("<span class=\"math inline\">\\(L_\\infty\\)</span>", "𝓁<sub>∞</sub>")
         , (" L-∞", " 𝓁<sub>∞</sub>")
         , (" L∞", " 𝓁<sub>∞</sub>")
           -- rest:
         , ("(PsycINFO Database Record", "")
         , ("</p> <p>", "</p>\n<p>")
         , ("</p><p>", "</p>\n<p>")
         , ("</li> <li>", "</li>\n<li>")
         , ("</p> <figure>", "</p>\n<figure>")
         , ("</figure> <p>", "</figure>\n<p>")
         , ("/><figcaption", "/>\n    <figcaption")
         , ("</p> <table>", "</p>\n<table>")
         , ("</table> <p>", "</table>\n<p>")
         , ("</p> <div", "</p>\n<div")
         , ("<p><strong>Abstract</strong>:T", "<p>T")
         , ("<strong>ABSTRACT</strong>: ", "")
         , ("<strong>Abstract</strong>: <p>", "<p>")
         , ("<p><strong>ABSTRACT</strong><p>", "")
         , ("<p><strong>Abstract</strong><p>", "")
         , (":</strong>", "</strong>:")
         , ("</p><p>", "</p> <p>")
         , (":</strong></p> <p>", "</strong>: ")
         , (" :</strong>", "</strong>:")
         , (" </sec>", "")
         , ("<title>", "")
         , ("</title>", "")
         , ("   <title/>    <p>", "<p>")
         , ("  <p>", "<p>")
         , ("I(2)", "I<sup>2</sup>")
         , (" h2",     " <em>h</em><sup>2</sup>")
         , ("h(2)",    "<em>h</em><sup>2</sup>")
         , ("</h2>", "</strong></p>")
         , ("<h2>", "<p><strong>")
         , ("</h3>", "</strong></p>")
         , ("<h3>", "<p><strong>")
         , ("</p>\\n<p>", "</p> <p>")
         , ("<br/></p>", "</p>")
         , ("<br/><br/>", "</p> <p>")
         , ("<br/><h3>", "<h3>")
         , ("</p><p>", "</p> <p>")
         , ("Alzheimer9", "Alzheimer'")
         , ("<br/> <br/>", "<br/>")
         , ("1.<p>", "<p>")
         , ("<list list-type=\"bullet\">", "<ul>")
         , ("</list>", "</ul>")
         , ("</list-item>", "</li>")
         , ("<list-item>", "<li>")
         , ("<p> ", "<p>")
         , (" <p>", "<p>")
         , ("</p> ", "</p>")
         , (" </p>", "</p>")
         , ("</p><br/>", "</p>")
         , ("</p> <br/>", "</p>")
         , ("<p><br/>", "<p>")
         , ("<p><br />", "<p>")
         , ("<p></p>", "")
         , ("<p></li> </ul> </p>", "</li> </ul>")
         , ("</li><br/>", "</li>")
         , ("<jats:sec>", "")
         , ("<jats:sec>\n ", "")
         , ("</p>\n\n<jats:sec>\n<strong>", "</p> <p><strong>")
         , ("</p>\n \n <jats:sec><p>", "</p> <p>")
         , ("</p>\n \n <jats:sec>\n<p>", "</p> <p>")
         , ("<strong>Abstract</strong>\n <jats:sec>\n<p>", "<p>")
         , ("<sec>", "")
         , ("</sec>", "")
         , ("  </sec><br/>  ", "")
         , ("<sec><br/>    ", "")
         , ("</jats:sec>", "")
         , ("<jats:sec><br/>", "")
         , ("</jats:sec><br/>", "")
         , ("  </sec> <br/>", "")
         , ("<sec id=\"sec001\">", "")
         , ("<sec id=\"sec002\">", "")
         , ("<sec id=\"sec003\">", "")
         , ("<sec id=\"sec004\">", "")
         , ("<sec id=\"sec005\">", "")
         , ("<sec id=\"sec006\">", "")
         , ("<sec id=\"sec007\">", "")
         , ("<sec id=\"sec008\">", "")
         , ("<sec id=\"sec009\">", "")
         , ("<sec id=\"sec010\">", "")
         , ("<sec id=\"sec001\">", "")
         , ("<sec id=\"sec002\">", "")
         , ("<sec id=\"sec003\">", "")
         , ("<sec id=\"sec004\">", "")
         , ("<sec id=\"sec005\">", "")
         , ("<sec id=\"sec006\">", "")
         , ("<sec id=\"sec007\">", "")
         , ("<sec id=\"sec008\">", "")
         , ("<sec id=\"sec009\">", "")
         , ("<sec id=\"sec010\">", "")
         , ("<sec id=\"english\">", "")
         , ("<sec id=\"st1\">", "")
         , ("<sec id=\"st2\">", "")
         , ("<sec id=\"st3\">", "")
         , ("<sec id=\"sb1a\">", "")
         , ("<sec id=\"sb1b\">", "")
         , ("<sec id=\"sb1c\">", "")
         , ("<sec id=\"sb1d\">", "")
         , ("<sec id=\"sb1e\">", "")
         , ("<sec id=\"english\">", "")
         , ("<sec sec-type=\"headed\">", "")
         , ("<p><sec sec-type=\"headed\"></p>", "")
         , ("<strong>Abstract</strong>\n<jats:sec>\n<strong>", "<p><strong>")
         , ("</strong></p>    <p>", "</strong> ")
         , ("</title>", "</strong>:</p>")
         , ("<title/>", "")
         , ("<title>", "<p><strong>")
         , ("</title><br/>", "</title>")
         , ("<p>\n\n", "<p>")
         , ("<br></p>", "</p>")
         , ("\n<br />\n", "")
         , ("<br/><p>", "<p>")
         , ("non–", "non-")
         , (" -- ", "&mdash;")
         , ("---", "&mdash;")
         , (" - ", "—")
         , (" — ", "—")
         , (" -- ", "—") -- eg. 'Darknet Data Mining -- A Canadian Cyber-crime Perspective'
         , ("was significantly diminished", "was statistically-significantly diminished")
         , ("decreased significantly", "decreased statistically-significantly")
         , ("is significantly better than", "is statistically-significantly better than")
         , (" significant increase", " statistically-significant increase")
         , (" significantly less", " statistically-significantly less")
         , (" significantly more", " statistically-significantly more")
         , ("boundary of significance", "boundary of statistical-significance")
         , ("robustly significant", "robustly statistically-significant")
         , (" significant trend", " statistically-significant trend")
         , (" non-significant trend", " non-statistically-significant trend")
         , (" significant difference", " statistically-significant difference")
         , (" significant genetic correlation", " statistically-significant genetic correlation")
         , (" significant allele-phenotype associations", " statistically-significant allele-phenotype associations")
         , (" significant association", " statistically-significant association")
         , (" significant correlation", " statistically-significant correlation")
         , ("the significant SNPs", "the statistically-significant SNPs")
         , (" significantly associated", " statistically-significantly associated")
         , (" significantly correlated", " statistically-significantly correlated")
         , (" significantly higher (", " statistically-significantly higher (")
         , (" significant interaction effect", "  statistically-significant interaction effect")
         , (" significant effect", " statistically-significant effect")
         , (" significance testing", " statistical-significance testing")
         , ("nominally significant", "nominally statistically-significant")
         , (" nonsignificant result", " non-statistically-significant result")
         , (" significant excess", " statistically-significant excess")
         , (" significantly enriched", " statistically-significantly enriched")
         , ("levels of significance", "levels of statistical-significance")
         , (" significant at the ", " statistically-significant at the ")
         , ("statistical significance", "statistical-significance")
         , ("statistically significant", "statistically-significant")
         , ("genome-wide significance", "genome-wide statistical-significance")
         , ("genome-wide significant", "genome-wide statistically-significant")
         , ("statistical significance", "statistical-significance")
         , ("statistically significant", "statistically-significant")
         , ("clinical significance", "clinical-significance")
         , ("clinically significant", "clinically-significant")
         , ("<p><strong>Significance Statement</strong></p>\n<p>", "<p><strong>Significance Statement</strong>: ")
         , (". <strong>Conclusion</strong>: ", ".</p> <p><strong>Conclusio</strong>: ")
         , (". <strong>Conclusions</strong>: ", ".</p> <p><strong>Conclusion</strong>: ")
         , ("<strong>Conclusions & Significance</strong>", "<strong>Conclusion</strong>")
         , ("<strong>Conclusions and Significance</strong>", "<strong>Conclusion</strong>")
         , ("<strong>Conclusions</strong>\n<p>", "<p><strong>Conclusion</strong>: ")
         , ("<p>Conclusions: ", "<p><strong>Conclusion</strong>: ")
         , ("\n <strong>Conclusion</strong>\n<p>", "<p><strong>Conclusion</strong>: ")
         , (". <strong>Results</strong>: ", ".</p> <p><strong>Results</strong>: ")
         , ("\n <strong>Results</strong>\n<p>", "<p><strong>Results</strong>: ")
         , ("<p>Results: ", "<p><strong>Results</strong>: ")
         , ("<p>Aims: ", "<p><strong>Aims</strong>: ")
         , ("<p>Background. ", "<p><strong>Background</strong>: ")
         , ("<strong>Background</strong>\n<p>", "<p><strong>Background</strong>: ")
         , ("<p>Background: ", "<p><strong>Background</strong>: ")
         , (" Interpretation. ", "</p> <p><strong>Interpretation</strong>: ")
         , (" Findings. ", "</p> <p><strong>Results</strong>: ")
         , ("<strong>Methods</strong>\n<p>", "<p><strong>Methods</strong>: ")
         , (" Methods. ", "</p> <p><strong>Methods</strong>: ")
         , (". <strong>Methods</strong>: ", ".</p> <p><strong>Methods</strong>: ")
         , (" \n <strong>Methods</strong>\n<p>", "<p><strong>Methods</strong>: ")
         , ("<p>Methods: ", "<p><strong>Methods</strong>: ")
         , ("<p>Outcomes: ", "<p><strong>Outcomes</strong>: ")
         , ("<p>Interpretation: ", "<p><strong>Interpretation</strong>: ")
         , ("<p>Funding: ", "<p><strong>Funding</strong>: ")
         , (" N1 =", " <em>n</em><sub>1</sub> =")
         , (" N2 =", " <em>n</em><sub>2</sub> =")
         , ("(N1 =", "(<em>n</em><sub>1</sub> =")
         , ("(N2 =", "(<em>n</em><sub>2</sub> =")
         , ("( <em>n</em> =", "(<em>n</em> =")
         , ("<em>N</em> =", "<em>n</em> =")
         , ("N = ", "<em>N</em> = ")
         , ("n = ", "<em>n</em> = ")
         , (" (x) ",   " (10) ")
         , (" (ix) ",  " (9) ")
         , (" (viii) "," (8) ")
         , (" (vii) ", " (7) ")
         , (" (vi) ",  " (6) ")
         , (" (v) ",   " (5) ")
         , (" (iv) ",  " (4) ")
         , (" (iii) ", " (3) ")
         , (" (ii) ",  " (2) ")
         , (" (i) ",   " (1) ")
         , ("(i)",     "(1)")
         , (" i)",     " (1)")
         , ("(ii)",    "(2)")
         , (" ii)",    " (2)")
         , ("(iii)",   "(3)")
         , (" iii)",   " (3)")
         , ("(iv)",    "(4)")
         , (" iv)",    " (4)")
         , ("(v)",     "(5)")
         , (" v)",     " (5)")
         , ("(vi)",    "(6)")
         , (" vi)",    " (6)")
         , ("(vii)",   "(7)")
         , (" vii)",   " (7)")
         , ("(viii)",  "(8)")
         , (" viii)",  " (8)")
         , ("(ix)",    "(9)")
         , (" ix)",    " (9)")
         , ("(x)",     "(10)")
         , (" x)",     " (10)")
         , (" a) ", " (1) ")
         , (" b) ", " (2) ")
         , (" c) ", " (3) ")
         , (" d) ", " (4) ")
         , (" e) ", " (5) ")
         , (" f) ", " (6) ")
         , (" h) ", " (7) ")
         -- i excluded due to ambiguity with Roman numeral i/ii/iii etc numbering
         , (" j) ", " (9) ")
         , (" k) ", " (10) ")
         , (" =  ", " = ")
         , ("</strong>\n<p>", "</strong>: ")
         , ("<strong><strong>", "<strong>")
         , ("</strong></strong>", "</strong>")
         , ("< /b>", "</strong>")
         , ("<b>", "<strong>")
         , ("</b>", "</strong>")
         , ("<jats:sec><strong>", "<strong>")
         , ("<jats:title>Abstract</jats:title><br/>               ", "")
         , ("</jats:p>", "</p>")
         , ("< sub>", "<sub>")
         , ("<jats:sub>", "<sub>")
         , ("</jats:sub>", "</sub>")
         , ("<jats:sup>", "<sup>")
         , ("</jats:sup>", "</sup>")
         , ("<jats:title content-type=\"abstract-subheading\">", "<strong>")
         , ("<jats:title>", "<strong>")
         , ("</jats:title>", "</strong>")
         , (".</jats:title>", "</strong>")
         , ("<jats:p xml:lang=\"en\">", "<p>")
         , ("<jats:p>", "<p>")
         , ("</Emphasis>", "</em>")
         , ("<Emphasis Type=\"Italic\">", "<em>")
         , (" <i> </i>", " ") -- Wikipedia {{ety}} weirdness, but just in Ancient Greek instances?
         , ("<jats:italics>", "<em>")
         , ("</jats:italics>", "</em>")
         , ("<jats:italic>", "<em>")
         , ("</jats:italic>", "</em>")
         , ("<italic>", "<em>")
         , ("</ italic>", "</em>")
         , ("< /italic>", "</em>")
         , ("</italic>", "</em>")
         , ("< /i>", "</i>")
         , ("<jats:title>Abstract</jats:title>\n\t  <jats:p>", "<p>")
         , ("<jats:title>Abstract</jats:title><jats:p>The</jats:p>", "")
         , ("<p><strong>Author Summary</strong></p> <p>", "<p><strong>Author Summary</strong>: ")
         , ("<p><strong>Author Summary</strong></p>\n<p>", "<p><strong>Author Summary</strong>: ")
         , ("<strong>Author Summary</strong> :", "<strong>Author Summary</strong>:")
         , ("strong>Author summary</strong>", "strong>Author Summary</strong>")
         , ("<jats:title>SUMMARY</jats:title>", "")
         , ("<jats:title>Summary</jats:title>", "")
         , ("<strong>ABSTRACT</strong><br/>              <p>", "<p>")
         , ("</strong><p>", "</strong>: <p>")
         , ("<p></abstract></p>", "")
         , ("<strong>Abstract</strong>:        ", "")
         , ("<abstract abstract-type=\"summary\"><br/>", "")
         , ("<abstract abstract-type=\"toc\">", "")
         , ("<abstract abstract-type=\"editor\">", "")
         , ("<abstract abstract-type=\"synopsis\">", "")
         , ("<strong>SUMMARY</jats:title>", "")
         , ("<strong>Abstract</jats:title>", "")
         , ("<strong>Abstract</strong><br/>", "")
         , ("<h3>Abstract:</h3>", "")
         , ("<h3>Summary/Abstract</h3>", "")
         , ("<h3>ABSTRACT</h3>", "")
         , ("<h3>Abstract</h3>", "")
         , ("<h3>SUMMARY</h3>", "")
         , ("<h3>Summary</h3>", "")
         , ("<abstract abstract-type=\"summary\">", "")
         , ("<p><abstract abstract-type=\"short\"></p>", "")
         , ("</abstract>", "")
         , ("<abstract>", "")
         , ("<abstract>\n  ", "")
         , ("\n</abstract>", "")
         , ("<strong>Abstract</strong>:<p>", "<p>")
         , ("<p><strong>Abstract</strong>: ", "<p>")
         , ("<strong>AIM:</strong>", "<strong>Aim</strong>:")
         , ("<strong>METHODS:</strong>", "<strong>Methods</strong>:")
         , ("<strong>RESULTS:</strong>", "<strong>Results</strong>:")
         , ("<strong>CONCLUSION:</strong>", "<strong>Conclusion</strong>:")
         , ("<strong>AIM</strong>:", "<strong>Aim</strong>:")
         , ("<strong>METHODS</strong>:", "<strong>Methods</strong>:")
         , ("<strong>RESULTS</strong>:", "<strong>Results</strong>:")
         , ("<strong>CONCLUSION</strong>:", "<strong>Conclusion</strong>:")
         , ("\nHighlights: ", "\n<strong>Highlights</strong>: ")
         , ("\nBackground: ", "\n<strong>Background</strong>: ")
         , ("\nAbstract: ", "\n<strong>Abstract</strong>: ")
         , ("\nContext: ", "\n<strong>Context</strong>: ")
         , ("<strong>Purpose</strong>\n<p>", "<p><strong>Purpose</strong>: ")
         , ("\nPurpose: ", "\n<strong>Purpose</strong>: ")
         , ("<p>Purpose. ", "\n<strong>Purpose</strong>: ")
         , ("\nRationale: ", "\n<strong>Rationale</strong>: ")

         , ("<strong>ANIMALS</strong>: ", "<strong>Animals</strong>: ")
         , ("<strong>OBJECTIVE</strong>: ", "<strong>Objective</strong>: ")
         , ("<strong>METHOD</strong>: ", "<strong>Method</strong>: ")
         , ("<strong>RESULTS</strong>: ", "<strong>Results</strong>: ")
         , ("<strong>CONCLUSIONS</strong>: ", "<strong>Conclusion</strong>: ")
         , ("<strong>CLINICAL RELEVANCE</strong>: ", "<strong>Clinical Relevance</strong>: ")
         , ("<strong>PROCEDURES</strong>: ", "<strong>Procedures</strong>: ")

         , ("</p><strong>Setting & Participants</strong>:<p>", "</p><p><strong>Setting & Participants</strong>: ")
         , ("<strong>OBJECTIVE</strong></p>\n", "<strong>Objective</strong>: ")
         , ("<strong>METHOD</strong></p>\n", "<strong>Method</strong>: ")
         , ("<strong>RESULTS</strong></p>\n", "<strong>Results</strong>: ")
         , ("<strong>CONCLUSIONS</strong></p>\n         ", "<strong>Conclusion</strong>: ")
         , ("<strong>CLINICAL RELEVANCE</strong></p>\n         ", "<strong>Clinical Relevance</strong>: ")
         , ("<p><strong>OBJECTIVE</strong></p>\n<p>", "<p><strong>Objective</strong>: ")
         , ("<p><strong>METHOD</strong></p>\n<p>", "<p><strong>Method</strong>: ")
         , ("<p><strong>RESULTS</strong></p>\n<p>", "<p><strong>Results</strong>: ")
         , ("<p><strong>CONCLUSIONS</strong></p>\n<p>         ", "<p><strong>Conclusion</strong>: ")
         , ("<p><strong>CLINICAL RELEVANCE</strong></p>\n<p>         ", "<p><strong>Clinical Relevance</strong>: ")
         , ("\nObjective: ", "\n<strong>Objective</strong>: ")
         , ("\nObjectives: ", "\n<strong>Objectives</strong>: ")
         , ("\nQuestion: ", "\n<strong>Question</strong>: ")
         , ("\nDescription: ", "\n<strong>Description</strong>: ")
         , ("Design and</p>\n<p><strong>Methods</strong>: ", "</p> <p><strong>Design & Methods</strong>: ")
         , ("\nDesign: ", "\n<strong>Design</strong>: ")
         , ("\nMethods: ", "\n<strong>Methods</strong>: ")
         , ("\nDesign and Methods: ", "\n<strong>Design & Methods</strong>: ")
         , ("\nSetting: ", "\n<strong>Setting</strong>: ")
         , ("\nParticipants: ", "\n<strong>Participants</strong>: ")
         , ("\nMeaning: ", "\n<strong>Meaning</strong>: ")
         , ("Setting and Participants", "Setting & Participants")
         , ("\nDesign, Setting, and Participants: ", "\n<strong>Design, Setting, & Participants</strong>: ")
         , ("\nIntervention: ", "\n<strong>Intervention</strong>: ")
         , ("\nData Sources: ", "\n<strong>Data Sources</strong>: ")
         , ("\nMain Outcomes & Measures: ", "\n<strong>Main Outcomes & Measures</strong>: ")
         , ("\nMeasurements: ", "\n<strong>Measurements</strong>: ")
         , (". Results. ", ".</p> <p><strong>Results</strong>: ")
         , ("\nResults: ", "\n<strong>Results</strong>: ")
         , ("\nSignificance: ", "\n<strong>Significance</strong>: ")
         , (". Conclusions. ", ".</p> <p><strong>Conclusion</strong>: ")
         , ("\nConclusion: ", "\n<strong>Conclusion</strong>: ")
         , ("\nConclusions: ", "\n<strong>Conclusion</strong>: ")
         , ("\nConclusions & Relevance: ", "\n<strong>Conclusion & Relevance</strong>: ")
         , ("\nIntroduction: ", "\n<strong>Background</strong>: ")
         , ("\nTrial Registration: ", "\n<strong>Trial Registration</strong>: ")
         , ("<h3>Highlights</h3>\n<p>", "<p><strong>Highlights</strong>: ")
         , ("<h3>Background</h3>\n<p>", "<p><strong>Background</strong>: ")
         , ("<h3>Abstract</h3>\n<p>", "<p><strong>Abstract</strong>: ")
         , ("<h3>Context</h3>\n<p>", "<p><strong>Context</strong>: ")
         , ("<h3>Purpose</h3>\n<p>", "<p><strong>Purpose</strong>: ")
         , ("<h3>Rationale</h3>\n<p>", "<p><strong>Rationale</strong>: ")
         , ("<h3>Objective</h3>\n<p>", "<p><strong>Objective</strong>: ")
         , ("<h3>Objectives</h3>\n<p>", "<p><strong>Objectives</strong>: ")
         , ("<h3>Question</h3>\n<p>", "<p><strong>Question</strong>: ")
         , ("<h3>Description</h3>\n<p>", "<p><strong>Description</strong>: ")
         , ("<h3>Design</h3>\n<p>", "<p><strong>Design</strong>: ")
         , ("<h3>Methods</h3>\n<p>", "<p><strong>Methods</strong>: ")
         , ("<h3>Setting</h3>\n<p>", "<p><strong>Setting</strong>: ")
         , ("<h3>Participants</h3>\n<p>", "<p><strong>Participants</strong>: ")
         , ("<h3>Meaning</h3>\n<p>", "<p><strong>Meaning</strong>: ")
         , ("<h3>Design, Setting, and Participants</h3>\n<p>", "<p><strong>Design, Setting, & Participants</strong>: ")
         , ("<h3>Intervention</h3>\n<p>", "<p><strong>Intervention</strong>: ")
         , ("<h3>Data Sources</h3>\n<p>", "<p><strong>Data Sources</strong>: ")
         , ("<h3>Main Outcomes & Measures</h3>\n<p>", "<p><strong>Main Outcomes & Measures</strong>: ")
         , ("<h3>Measurements</h3>\n<p>", "<p><strong>Measurements</strong>: ")
         , ("<h3>Results</h3>\n<p>", "<p><strong>Results</strong>: ")
         , ("<h3>Significance</h3>\n<p>", "<p><strong>Significance</strong>: ")
         , ("<h3>Conclusion</h3>\n<p>", "<p><strong>Conclusion</strong>: ")
         , ("<h3>Conclusions</h3>\n<p>", "<p><strong>Conclusion</strong>: ")
         , ("<h3>Conclusions & relevance</h3>\n<p>", "<p><strong>Conclusion & Relevance</strong>: ")
         , ("<h3>Conclusions & Relevance</h3>\n<p>", "<p><strong>Conclusion & Relevance</strong>: ")
         , ("<h3>Trial Registration</h3>\n<p>", "<p><strong>Trial Registration</strong>: ")
         , ("</h3><br/>", "</h3>")
         , ("<br/><h3>", "<h3>")
         , ("<em>Background:</em>", "<strong>Background</strong>:")
         , ("<em>Objective:</em> ", "<strong>Objective</strong>:")
         , ("<em>Results:</em> ", "<strong>Results</strong>:")
         , ("<em>Conclusions:</em>", "<strong>Conclusion</strong>:")
         , ("<strong>Originality/value</strong>", "<strong>Conclusion</strong>:")
         , ("\91Keywords: ", "\91<strong>Keywords</strong>: ")
         , ("&lt;/i&gt;&lt;/b&gt;", "</em>")
         , ("&lt;b&gt;&lt;i&gt;", "<em>")
         , ("acc.:", "accuracy:")
         , (" m/s", " m⁄s")
         , (" 1/100 ", " 1⁄100 ")
         , ("~1/250", "~1⁄250")
         , (" 2/3 ", " 2⁄3 ")
         , (" 3/3 ", " 3⁄3 ")
         , (" 1/10 ", " 1⁄10 ")
         , (" (4/8 ", " (4⁄8 ")
         , (" (4/8 ", " (4⁄8 ")
         , (" (5/8 ", " (5⁄8 ")
         , (" (5/8 ", " (5⁄8 ")
         , (" (5/8 ", " (5⁄8 ")
         , (" 1/2 ", " 1⁄2 ")
         , (" 1/3 ", " 1⁄3 ")
         , (" 1/4 ", " 1⁄4 ")
         , (" 2/3 ", " 2⁄3 ")
         , (" 4/5 ", " 4⁄5 ")
         , (" 5/8 ", " 5⁄8 ")
         , (" 5/9 ", " 5⁄9 ")
         , (" 6/13 ", " 6⁄13 ")
         , (" 7/13 ", " 7⁄13 ")
         , (" 8/13 ", " 8⁄13 ")
         , (" 9/13 ", " 9⁄13 ")
         , (" 15/16 ", " 15⁄16 ")
         , (" 5/16 ", " 5⁄16 ")
         , (" 5/8 ", " 5⁄8 ")
         , (" 15/20 ", " 15⁄20 ")
         , (" (23/96) ", " (23⁄96) ")
         , (" (24/50) ", " (24⁄50) ")
         , (" (30/96) ", " (30⁄96) ")
         , (" (35/96) ", " (35⁄96) ")
         , (" (39/50) ", " (39⁄50) ")
         , (" (41/50) ", " (41⁄50) ")
         , (" (43/50) ", " (43⁄50) ")
         , (" (48/96) ", " (48⁄96) ")
         , (" (50/96) ", " (50⁄96) ")
         , (" (6/96), ", " (6⁄96), ")
         , (" (68/96) ", " (68⁄96) ")
         , (" (90/96) ", " (90⁄96) ")
         , (" 11/90 ", " 11⁄90 ")
         , (" 33/96 ", " 33⁄96 ")
         , (" 42/50 ", " 42⁄50 ")
         , ("(11/31)", "(11⁄31)")
         , ("(9/11)", "(9⁄11)")
         , ("(2/7)", "(2⁄7)")
         , ("(28/31)", "(28⁄31)")
         , ("(9/10)", "(9⁄10)")
         , ("10(-10)", "10<sup>−10</sup>")
         , ("10(-11)", "10<sup>−11</sup>")
         , ("10(-13)", "10<sup>−13</sup>")
         , ("10(-15)", "10<sup>−15</sup>")
         , ("10(-19)", "10<sup>−19</sup>")
         , ("10(-26)", "10<sup>−26</sup>")
         , ("10(-3)", "10<sup>−3</sup>")
         , ("10(-4)", "10<sup>−4</sup>")
         , ("10(-5)", "10<sup>−5</sup>")
         , ("10(-6)", "10<sup>−6</sup>")
         , ("10(-7)", "10<sup>−7</sup>")
         , ("10(-8)", "10<sup>−8</sup>")
         , ("10(-9)", "10<sup>−9</sup>")
         , ("10(-)(3)", "10<sup>−3</sup>")
         , ("10(-)(4)", "10<sup>−4</sup>")
         , ("10(-)(5)", "10<sup>−5</sup>")
         , ("10(-)(6)", "10<sup>−6</sup>")
         , ("10(-)(7)", "10<sup>−7</sup>")
         , ("10(-)(8)", "10<sup>−8</sup>")
         , ("10(-)(9)", "10<sup>−9</sup>")
         , ("10(-)(10)", "10<sup>−10</sup>")
         , ("R (2) ", "R<sup>2</sup> ")
         , ("CO(2)", "CO<sub>2</sub>")
         , (" = .",    " = 0.")
         , (" gf ", " <em>gf</em> ")
         , (" gc ", " <em>gc</em> ")
         , ("( g = ", "(<em>g</em> = ")
         , ("<i><em>h</em><sup>2</sup></i>", "<em>h</em><sup>2</sup>")
         , ("<i><em>h</em><sup>2</sup><sub>SNP</sub></i>", "<em>h</em><span class=\"subsup\"><sub>SNP</sub><sup>2</sup></span>")
         , ("h2SNP", "<em>h</em><span class=\"subsup\"><sub>SNP</sub><sup>2</sup></span>")
         , ("h<sup>2</sup>", "<em>h</em><sup>2</sup>")
         , ("|rA|", "|r<sub>A</sub>|")
         , ("|rE|", "|r<sub>E</sub>|")
         , ("<em>r</em> <sub>g</sub>", "<em>r</em><sub>g</sub>")
         , ("r(g)",    "<em>r</em><sub><em>g</em></sub>")
         , (" rg:", " <em>r</em><sub><em>g</em></sub>:")
         , (" rg ", " <em>r</em><sub><em>g</em></sub> ")
         , (" rg=", " <em>r</em><sub><em>g</em></sub> = ")
         , (" rg = ", " <em>r</em><sub><em>g</em></sub> = ")
         , ("(rg)", "(<em>r</em><sub><em>g</em></sub>)")
         , ("(rg", "(<em>r</em><sub><em>g</em></sub>")
         , ("|rg|=", "|<em>r</em><sub><em>g</em></sub>| = ")
         , ("-&gt;", "→")
         , (" r = ", " <em>r</em> = ")
         , (" r=", "<em>r</em> = ")
         , (" r>", "<em>r</em> > ")
         , (" r<", " <em>r</em> < ")
         , ("r≥", "<em>r</em> ≥ ")
         , ("r≤", "<em>r</em> ≤ ")
         , ("≤n≤", " ≤ <em>n</em> ≤ ")
         , ("<var>", "<em>")
         , ("</var>", "</em>")
         , ("</monospace>", "</code>")
         , ("<monospace>", "<code>")
         , ("<code class=\"mw-highlight mw-highlight-lang-text mw-content-ltr\" dir=\"ltr\"", "<code")
         , ("<wbr />", "")
         , ("<wbr/>", "")
         , ("<wbr>", "")
         , ("<wbr />&#8203;", "")
         , ("<wbr></wbr>", "")
         , ("<wbr></wbr>\8203", "")
         , ("<abbr>", "<span>")
         , ("</abbr>", "</span>")
         , ("</a> .", "</a>.")
         , ("<ext-link ext-link-type=\"uri\"", "<a")
         , ("<ext-link ext-link-type=\"uri\" xlink:href=", "<a href=")
         , ("xlink:type=\"simple\"", "")
         , ("</ext-link>", "</a>")
         , ("beta=", "β = ")
         , (" = 0", " = 0")
         , ("cm3", "cm<sup>3</sup>")
         , (" cm(3)", " cm<sup>3</sup>")
         , (" R2", " R<sup>2</sup>")
         , ("R2 = ", "R<sup>2</sup> = ")
         , (" R(2)", " R<sup>2</sup>")
         , ("top-k", "top-<em>k</em>")
         , ("Top-k", "Top-<em>k</em>")
         , ("Top-K", "Top-<em>k</em>")
         , (" z = ", " <em>z</em> = ")
         , ("z-score", "<em>z</em>-score")
         , ("Z-score", "<em>z</em>-score")
         , ("z-scores", "<em>z</em>-scores")
         , (" z-latent", "<em>z</em>-latent")
         , (" w-latent", "<em>w</em>-latent")
         , (" &lt; .0", " &lt; 0.0")
         , (" p &amp;gt; ", " <em>p</em> &gt; ")
         , (" p &amp;lt; ", " <em>p</em> &lt; ")
         , ("<em>p<\\/em>=", "<em>p</em> = ")
         , ("P = ", "<em>p</em> = ")
         , ("P values", "<em>p</em>-values")
         , (" p value", " <em>p</em>-value")
         , (" P &lt; .", " <em>p</em> &lt; 0.")
         , (" P &lt;", " <em>p</em> &lt;")
         , (" P &lt;", " <em>p</em> &lt;")
         , ("≤p≤",     " ≤ <em>p</em> ≤ ")
         , (" d = ", " <em>d</em> = ")
         , ("( d = ", "(<em>d</em> = ")
         , ("(d = ", "(<em>d</em> = ")
         , ("(d < ", "(<em>d</em> < ")
         , ("(d > ", "(<em>d</em> > ")
         , ("(rs)", "(<em>r</em>s)")
         , ("\40r=",     "\40<em>r</em> = ")
         , ("\40R=",     "\40<em>r</em> = ")
         , ("\40R = ",   "\40<em>r</em> = ")
         , ("\40r = ",   "\40<em>r</em> = ")
         , ("\40N = ",   "\40<em>N</em> = ")
         , ("\40n = ",   "\40<em>n</em> = ")
         , ("\40n=",     "\40<em>n</em> = ")
         , ("\40N=",     "\40<em>N</em> = ")
         , ("(r&gt;", "9<em>r</em> &gt; ")
         , (" N ~ ",     " <em>n</em> ~ ")
         , ("( N = ", "(<em>n</em> = ")
         , ("( n = ", "(<em>n</em> = ")
         , ("( ns = ", "(<em>ns</em> = ")
         , ("( n = ", "(<em>n</em> = ")
         , ("n = ", "<em>n</em> = ")
         , ("(minimum p ", "(minimum <em>p</em> ")
         , ("<em>P</em> = 0", "<em>p</em> = 0")
         , ("(p = ", "(<em>p</em> = ")
         , (" p&lt;", " <em>p</em> < ")
         , (" p&gt;", " <em>p</em> > ")
         , (" p&gte;", " <em>p</em> ≥ ")
         , (" p&lte;", " <em>p</em> ≤ ")
         , (" P&lt;", " <em>p</em> < ")
         , (" P&gt;", " <em>p</em> > ")
         , (" P&gte;", " <em>p</em> ≥ ")
         , (" P&lte;", " <em>p</em> ≤ ")
         , ("<em>p</em> = .", "<em>p</em> = 0.")
         , ("<em>p</em> < .", "<em>p</em> < 0.")
         , (" N=",     " <em>N</em> = ")
         , (" N = ",     " <em>N</em> = ")
         , (" n=",     " <em>n</em> = ")
         , (" n = ",     " <em>n</em> = ")
         , ("\40p=",     "\40<em>p</em> = ")
         , (" n=",     " <em>n</em> = ")
         , ("( n=", "( <em>n</em> = ")
         , ("Neffective", "<em>n</em><sub>effective</sub>")
         , ("Neffective=", "<em>n</em><sub>effective</sub> = ")
         , (" n-gram", " <em>n</em>-gram")
         , (" k &gt; ", " <em>nk</em> &gt; ")
         , (" k > ", " <em>k</em> > ")
         , (" N &gt; ", " <em>n</em> &gt; ")
         , (" N > ", " <em>n</em> > ")
         , (" N<sub>effective</sub>", " <em>N<sub>effective</sub>")
         , (" N-weighted", " <em>N</em>-weighted")
         , (" n-back", " <em>n</em>-back")
         , ("N-back", "<em>N</em>-back")
         , ("<em>p</em> &lt; .05", "<em>p</em> &lt; 0.05")
         , ("( <em>p</em>", "(<em>p</em>")
         , (" p&lt;", " <em>p</em> &lt; ")
         , ("p = 0",   "<em>p</em> = 0")
         , (" P=",     " <em>p</em> = ")
         , (" P = ",   " <em>p</em> = ")
         , ("(P = ",   "(<em>p</em> = ")
         , ("(P=",     "(<em>p</em> = ")
         , (" p = ",   " <em>p</em> = ")
         , (" p=",     " <em>p</em> = ")
         , (" P<",     " <em>p</em> < ")
         , ("P ≤ ", "<em>p</em> ≤ ")
         , ("\40P<",     "\40<em>p</em> < ")
         , ("(<em>P</em> &lt;", "(<em>p</em> &lt;")
         , ("<span>0,1</span>^n", "{0,1}<sup><em>n</em></sup>")
         , (" n&lt;", " <em>n</em> &lt; ")
         , (" P(t) ", " <em>P(t)</em> ")
         , (" t ", " <em>t</em> ")
         , (" t test", " <em>t</em>-test")
         , ("(P≤", "(<em>p</em> ≤ ")
         , ("(P&lt;", "(<em>p</em> &lt; ")
         , ("(P&gt;", "(<em>p</em> &gt; ")
         , ("(P &lt;", "(<em>p</em> &lt;")
         , ("(P &gt;", "(<em>p</em> &gt;")
         , ("(p≤", "(<em>p</em> ≤ ")
         , ("&gt; &gt;0", "≫0")
         , ("(p&lt;", "(<em>p</em> &lt; ")
         , ("(p&gt;", "(<em>p</em> &gt; ")
         , ("(p &lt;", "(<em>p</em> &lt;")
         , ("(p &gt;", "(<em>p</em> &gt;")
         , (" p &lt;", " <em>p</em> &lt;")
         , (" p &gt;", " <em>p</em> &gt;")
         , (" P < ",   " <em>p</em> < ")
         , (" p < ",   " <em>p</em> < ")
         , (" p<",     " <em>p</em> < ")
         , (" p<.",    " <em>p</em> < 0.")
         , ("(P < 0.", "(<em>p</em> < 0.")
         , ("(P < .", "(<em>p</em> < 0.")
         , ("\40P=",     "\40<em>p</em> = ")
         , ("P-value", "<em>p</em>-value")
         , ("p-value", "<em>p</em>-value")
         , (" p-level ", " <em>p</em>-level ")
         , ("ﬁ", "fi")
         , (" ", " ")
         , ("DALL-E", "DALL·E")
           -- many uses of 'approximately' are weasel-wordy which add nothing whatsoever semantically, so we can drop the '~':
         , ("~linearly", "linearly")
         , ("~every", "every")
         , ("~isometrically", "isometrically")
         , ("~solving", "solving")
         , ("~simulate", "simulate")
         , ("~this", "Approximately this")
         , ("~equivalent", "equivalent")
         , ("~the", "the")
         , ("~one ", "~1")
         , (" one tenth ", " 1⁄10<sup>th</sup>")
         , (" two thirds ", " 2⁄4<sup>rds</sup>")
         , ("~zero ", "~0")
         , ("~identical", "near-identical")
         , ("approximately ", "~")
         , ("Approximately ", "~")
         , ("∼", "~")
         , ("GxE", "G×E")
         , (" 10x", " 10×")
         , (" 100x", " 100×")
         , (" 2x", " 2×")
         , (" 3x", " 3×")
         , ("~twice", "~2×")
         , ("five times", "5×")
         , ("fifty-five years", "55 years")
         , ("Fifty-five years", "55 years")
         , ("<p> ", "<p>")
         , ("+/-", "±")
         , (" +- ", "±")
         , ("<sup>~</sup>", "~")
         , ("one-third", "1⁄3<sup>rd</sup>")
         , ("one-quarter", "1⁄4<sup>th</sup>")
         , ("three and a half", "3.5")
         , (" 11th", " 11<sup>th</sup>")
         , (" 12th", " 12<sup>th</sup>")
         , (" 13th", " 13<sup>th</sup>")
         , (" 14th", " 14<sup>th</sup>")
         , (" 15th", " 15<sup>th</sup>")
         , (" 16th", " 16<sup>th</sup>")
         , (" 17th", " 17<sup>th</sup>")
         , (" 18th", " 18<sup>th</sup>")
         , (" 19th", " 19<sup>th</sup>")
         , (" 20th", " 20<sup>th</sup>")
         , (" 21st", " 21<sup>st</sup>")
         , ("\"21st", "\"21<sup>st</sup>")
         , ("early-12th", "early-12<sup>th</sup>")
         , ("mid-21st", "mid-21<sup>st</sup>")
         , ("early-20th-century", "early-20<sup>th</sup>-century")
         , ("<i>25th", "<i>25<sup>th</sup>")
         , (">15th", ">15<sup>th</sup>")
         , ("mid-17th", "mid-17<sup>th</sup>")
         , ("mid-16th", "mid-16<sup>th</sup>")
         , (">21st", ">21<sup>st</sup>")
         , ("–19th", "–19<sup>th</sup>")
         , ("late-20th", "late-20<sup>th</sup>")
         , ("64,000th", "64,000<sup>th</sup>")
         , ("(5th", "(5<sup>th</sup>")
         , ("(12th", "(12<sup>th</sup>")
         , (" 14th ", " 14<sup>th</sup> ")
         , ("<code class=\"mw-highlight mw-highlight-lang-bash mw-content-ltr\" dir=\"ltr\">", "<code>")
         , ("ml-1", "ml<sup>−1</sup>")
         , ("10(9)", "10<sup>9</sup>")
         , ("(10(9))", "(10<sup>9</sup>)")
         , ("kg/m(2)", "kg⁄m<sup>2</sup>")
         , ("Cmax", "C<sub>max</sub>")
         , ("<small></small>", "")
         , (" et al ", " et al ") -- et al: try to ensure no linebreaking of citations
         , (" et al. ", " et al ")
         , (" et al., ", " et al ")
         , ("<em>i</em>.<em>e</em>.,", "ie.")
         , ("(ie, ", "(ie. ")
         , ("(ie ", "(ie. ")
         , ("(i.e.,", "(ie.")
         , ("(i.e.", "(ie.")
         , ("<em>e</em>.<em>g</em>.,", "eg.")
         , (" e.g.", " eg.")
         , ("(e.g.", "(eg.")
         , ("(eg ", "(eg.")
         , (" eg ", " eg. ")
         , ("eg., ", "eg. ")
         , ("e.g., ", "eg. ")
         , ("e.g. ", "eg. ")
         , ("labell", "label")
         , ( "optimise", "optimize")
         , ("organise", "organize")
         , ("totall ", "total ")
         , ("minimis", "minimiz")
         , ("maximis", "maximiz")
         , (" Escherichia coli", " <em>Escherichia coli</em>")
         , (" Saccharomyces cerevisiae", " <em>Saccharomyces cerevisiae</em>")
         , ("(Calcarius lapponicus) ", "(<em>Calcarius lapponicus)</em> ")
         , ("(Corvus brachyrhynchos) ", "(<em>Corvus brachyrhynchos)</em> ")
         , ("(Felis catus)", "(<em>Felis catus</em>)")
         , ("(Felis silvestris catus)", "(<em>Felis silvestris catus</em>)")
         , ("(Panthera leo)", "(<em>Panthera leo</em>)")
         , ("(Macaca fascicularis) ", "(<em>Macaca fascicularis)</em> ")
         , ("(Orcinus orca) ", "(<em>Orcinus orca)</em> ")
         , ("(Plectrophenax nivalis)", "(<em>Plectrophenax nivalis</em>)")
         , ("(Poecilia reticulata)", "(<em>Poecilia reticulata</em>)")
         , (" Pan Troglodytes", " <em>Pan Troglodytes</em>")
         , ("(Bacopa monniera)", "(<em>Bacopa monniera</em>)")
         , ("(Canis latrans)", "(<em>Canis latrans</em>)")
         , ("(Herpestes ichneumon)", "(<em>Herpestes ichneumon</em>)")
         , ("(Lynx canadensis)", "(<em>Lynx canadensis</em>)")
         , ("(Mammuthus primigenius)", "(<em>Mammuthus primigenius</em>)")
         , ("(Pan Troglodytes)", "(<em>Pan Troglodytes</em>)")
         , ("(Psilocybe cubensis)", "(<em>Psilocybe cubensis</em>)")
         , ("(Rattus norvegicus) ", "(<em>Rattus norvegicus)</em> ")
         , ("(Mus musculus)", "(<em>Mus musculus</em>)")
         , ("(Taxidea taxus)", "(<em>Taxidea taxus</em>)")
         , ("(Peromyscus leucopus)", "(<em>Peromyscus leucopus</em>)")
         , ("(Globicephala melas)", "(<em>Globicephala melas</em>)")
         , (" Arabidopsis thaliana", " <em>Arabidopsis thaliana</em>")
         , ("(Heterocephalus glaber)", "(<em>Heterocephalus glaber</em>)")
         , ("(Drosophila melanogaster", "(<em>Drosophila melanogaster</em>")
         , (" Drosophila melanogaster", " <em>Drosophila melanogaster</em>")
         , (" Arabidopsis Thaliana", " <em>Arabidopsis Thaliana</em>")
         , ("<em>C</em>. <em>elegans</em>", "<em>C. elegans</em>")
         , (" Caenorhabditis elegans ", " <em>Caenorhabditis elegans</em> ")
         , (" C. elegans", " <em>C. elegans</em>")
         , (" T. gondii", " <em>T. gondii</em>")
         , (" Equus ", " <em>Equus</em> ")
         , ("Lempel–Ziv–Markov", "Lempel-Ziv-Markov")
         , ("learn-ing", "learning")
         , ("Per- formance", "Performance")
         , ("per- formance", "performance")
         , (" itis ", " it is ")
         , (" k ", " <em>k</em> ")
         , (" k-shot", " <em>k</em>-shot")
         , ("one- or five-shot", "one-shot or five-shot")
         , ("lan- guage", "language")
         , ("pro-posed", "proposed")
         , ("case- control", "case-control")
         , ("high- g", "high-<em>g</em>")
         , ("ap-proach", "approach")
         , ("AsRL", "As RL")
         , ("spaceusing", "space using")
         , ("withits", "with its")
         , ("languagemodel", "language model")
         , ("questiongeneration", "question generation")
         , ("genomewide", "genome-wide")
         , ("regularise", "regularize")
         , (" memoise", " memoize")
         , (" hypothesise" ," hypothesize")
         , ("factorise" ,"factorize")
         , ("initialis", "initializ")
         , ("wethen", "we then")
         , ("successfullylearns", "successfully learns")
         , ("n-of-1", "<em>n</em>-of-1")
         , ("“ ", "“")
         , ("\t", "")
         , ("\t\t", "")
         , ("\t\t\t\t\t", "")
         , ("longstanding", "long-standing")
         , ("dis -ambiguate", "disambiguate")
         , ("co- occurring", "co-occurring")
         , ("CLIP- based", "CLIP-based")
         , ("self- and psychiatrist-rated", "self-rated & psychiatrist-rated")
         , (" GPT2", " GPT-2")
         , (" GPT3", " GPT-3")
         , ("</sup><br/>", "</sup>")
         , (" < jats:sub>", "<sub>")
         , ("<italic toggle=\"yes\">", "<em>")
         , ("\n            <jats:italic>k</jats:italic>\n            ", "<em>k</em>")
         , ("\n            <jats:sup>–6</jats:sup>\n            ", "<sup>–6</sup>")
         , ("\n            <jats:italic>in vitro</jats:italic>\n", " <em>in vitro</em>")
         , ("\n            <jats:italic>R</jats:italic>\n", "<em>R</em>")
         , ("_X_s", "<em>X</em>s")
         , ("Yann Le Cun", "Yann LeCun")
         , ("Oliver Bryne", "Oliver Byrne")
         , ("UCF-101", "UCF101")
         , ("WikiText103", "WikiText-103")
         , (" an universe", " a universe")
         , ("eyetracking", "eye tracking")
         , ("PsychINFO", "PsycINFO")
         , (" an universe", " a universe")
         , ("Norbert Weiner",  "Norbert Wiener")
         , ("mulitple",  "multiple")
         , (" a improvement",  " an improvement")
         , (" a enrichment", " an enrichment")
         , (" a insertional", " an insertional")
         , (" a overlap", " an overlap")
         , (" a audio", " an audio")
         , (" a interaction", " an interaction")
         , ("non-significant", "non-statistically-significant")
         , ("non-significance", "non-statistical-significance")
         , ("statistically statistically-significant", "statistically-significant")
         , ("-wide significance", "-wide statistical-significance")
         , ("GW significance", "genome-wide statistical-significance")
         , ("Most of the significance for", "Most of the statistical-significance for")
         , ("a significance test", "a statistical-significance test")
         , ("significance tests", "statistical-significance tests")
         , ("The significance of melatonergic", "The importance of melatonergic")
         , (", with significance for the ", ", with implications for the")
         , ("variants of uncertain significance", "variants of uncertain importance")
         , ("philosophical significance", "philosophical importance")
         , ("study-wide significance", "study-wide statistical-significance")
         , (" significance threshold", " statistical-significance threshold")
         , (" significance measures", " statistical-significance measures")
         , ("but not significance criteria", "but not statistical-significance criteria")
         , ("unique biological significance of", "unique biological importance of")
         , ("epidemiological significance", "epidemiological importance")
         , ("assess its significance", "assess its importance")
         , ("nominal significance level", "nominal statistical-significance level")
         , ("strict significance level", "strict statistical-significance level")
         , ("levels of significance", "levels of statistical-significance")
         , ("Excess significance", "Excess statistical-significance")
         , ("their scientific significance", "their scientific importance")
         , ("behavioral significance", "behavioral importance")
         , ("parametris", "parameteriz")
         , ("normalis", "normaliz")
         , ("generalizt", "generalist")
         , ("generalise", "generalize")
         , ("generalisi", "generalizi")
         , ("generalisa", "generaliza")
         , (" disincentivis", " disincentiviz")
         , (" incentivis", " incentiviz")
         , ("benefitt", "benefit")
         , ("noninsight", "non-insight")
         , (" UNet", " U-Net")
         , ("PsycArticles", "PsycARTICLES")
         , ("behaviour", "behavior")
         , ("de Novo", "De Novo")
         , ("small saple", "small sample")
         , ("ofsmoking", "of smoking")
         , (" u.s. ", " U.S. ")
         , ("ofthe", "of the")
         , ("ofdata", "of data")
         , ("\8201", " ")
         , ("ADE20k", "ADE20K")
         , (" XSUM", " XSum")
         , (" Xsum", " XSum")
         , (" xsum", " XSum")
         , ("CityScapes", "Cityscapes")
         , ("Fr’echet", "Fréchet")
         , ("Frechet", "Fréchet")
         , ("h20ttps://", "https://")
         , ("mm(2)", "mm<sup>2</sup>")
         , ("²", "<sup>2</sup>")
         , ("₂", "<sub>2</sub>")
         , ("\173", "") -- all web browsers now do hyphenation so strip soft-hyphens
         , ("‐", "-")
         , ("‰", "%") -- PER MILLE SIGN https://en.wikipedia.org/wiki/Per_mille - only example I've ever seen was erroneous
         , ("FROH", "<em>F<sub>ROH</sub></em>")
         , (" Ne ", " <em>N<sub>e</sub></em> ")
         , ("Oamp#x02019;", "O’")
         , ("Camp#x000ED;", "Cí")
         , ("amp#x000E9", "é")
         , ("amp#x000E9,", "é")
         , ("\\aka", "a.k.a.")
         , (" three ", " 3 ")
         , (" Three ", " 3 ")
         , (" four ", " 4 ")
         , (" Four ", " 4 ")
         , (" five ", " 5 ")
         , (" Five ", " 5 ")
         , (" six ", " 6 ")
         , (" Six ", " 6 ")
         , (" seven ", " 7 ")
         , (" Seven ", " 7 ")
         , (" eight ", " 8 ")
         , (" Eight ", " 8 ")
         , (" nine ", " 9 ")
         , (" Nine ", " 9 ")
         , (" ten ", " 10 ")
         , (" Ten ", " 10 ")
         , (" eleven", " 11")
         , (" Eleven", " 11")
         , (" twelve", " 12")
         , (" Twelve", " 12")
         , (" thirteen", " 13")
         , (" Thirteen", " 13")
         , (" fourteen", " 14")
         , (" Fourteen", " 14")
         , (" fifteen", " 15")
         , (" Fifteen", " 15")
         , (" sixteen", " 16")
         , (" Sixteen", " 16")
         , (" seventeen", " 17")
         , (" Seventeen", " 17")
         , (" eighteen", " 18")
         , (" Eighteen", " 18")
         , (" nineteen", " 19")
         , (" Nineteen", " 19")
         , (" twenty", " 20")
         , (" Twenty", " 20")
         , ("twenty six", "26")
         , ("\160", " ") -- NO BREAK SPACE
         ]
