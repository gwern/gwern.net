#!/usr/bin/env runghc
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Read directories like "doc/iodine/" for its files, as well as any files/links with annotations &
-- the tag 'iodine'; generate a list item with the abstract in a blockquote where available; the
-- full list is then turned into a directory listing, which gets compiled with Hakyll and gets the
-- usual popup annotations. We can then treat directories as 'tags', unifying two
-- apparently-separate systems of organization.
--
-- Very nifty. Much nicer than simply browsing a list of filenames or even the Google search of a
-- directory (mostly showing random snippets).

import Control.Monad (filterM, void, unless)
import Control.Monad.Parallel as Par (mapM_)
import Data.List (elemIndex, isPrefixOf, isInfixOf, isSuffixOf, sort, sortBy, (\\))
import Data.Containers.ListUtils (nubOrd)
import Data.List.Split (chunksOf)
import qualified Data.Map as M (keys, lookup, filter, filterWithKey, fromList, toList)
import Data.Maybe (fromJust)
import qualified Data.Text as T (append, isInfixOf, pack, unpack, Text)
import System.Directory (listDirectory, doesFileExist)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, takeFileName, splitPath)

import Text.Pandoc (def, nullAttr, nullMeta, pandocExtensions, runPure, writeMarkdown, writerExtensions,
                    Block(BulletList, Div, Header, Para, OrderedList), ListNumberDelim(DefaultDelim), ListNumberStyle(DefaultStyle), Format(Format), Inline(Code, Emph, Image, Link, Space, Span, Str, RawInline), Pandoc(Pandoc))
import Text.Pandoc.Walk (walk)

import LinkArchive (readArchiveMetadata, ArchiveMetadata)
import LinkID (generateID, authorsToCite)
import LinkMetadata as LM (readLinkMetadata, generateAnnotationTransclusionBlock, hasAnnotation, annotateLink, lookupFallback, sortItemPathDateCreated, sortItemDateModified)
import LinkMetadataTypes (Metadata, MetadataItem)
import Tags (listTagDirectories, listTagDirectoriesAll, abbreviateTag)
import LinkBacklink (getLinkBibLinkCheck)
import Query (extractImages)
import Typography (identUniquefy, titlecase')
import MetadataFormat (extractTwitterUsername)
import MetadataAuthor (authorCollapse)
import Utils (inlinesToText, replace, sed, writeUpdatedFile, printRed, toPandoc, anySuffix)
import Config.Misc as C (cd)
import GenerateSimilar (sortSimilarsStartingWithNewestWithTag, readListName, readListSortedMagic, ListName, ListSortedMagic)
import Config.GenerateSimilar as CGS (minTagAuto)
-- import Text.Show.Pretty (ppShow)

main :: IO ()
main = do C.cd

          dirs <- getArgs
          -- result: '["doc/","doc/ai/","doc/ai/anime/","doc/ai/anime/danbooru/","doc/ai/dataset/", ..., "newsletter/2022/","nootropic/","note/","review/","zeo/"]'
          let dirs' = sort $ map (\dir -> sed "/index$" "" $ replace "/index.md" "" $ replace "//" "/" ((if "./" `isPrefixOf` dir then drop 2 dir else dir) ++ "/")) dirs

          meta <- readLinkMetadata
          am <- readArchiveMetadata
          ldb <- readListName
          sortDB <- readListSortedMagic

          let chunkSize = 6 -- can't be >20 or else it'll OOM due to trying to force all the 100s of tag-directories in parallel
          let dirChunks = chunksOf chunkSize dirs'

          -- because of the expense of searching the annotation database for each tag, it's worth parallelizing as much as possible. (We could invert and do a single joint search, but at the cost of ruining our clear top-down parallel workflow.)
          Prelude.mapM_ (Par.mapM_ (generateDirectory False am meta ldb sortDB dirs')) dirChunks

          -- Special-case directories:
          -- 'newest': the _n_ newest link annotations created
          -- Optimization: if there are only a few arguments, that means we are doing tag-directory development/debugging, and we should skip doing `/doc/newest` since we aren't going to look at it & it would increase runtime.
          unless (length dirs < 5) $
            generateDirectory True am meta ldb sortDB ["doc/", "doc/newest/", "/"] "doc/newest/"

generateDirectory :: Bool -> ArchiveMetadata -> Metadata -> ListName -> ListSortedMagic -> [FilePath] -> FilePath -> IO ()
generateDirectory newestp am md ldb sortDB dirs dir'' = do

  -- remove the tag for *this* directory; it is redundant to display 'cat/psychology/drug/catnip' on every doc/link inside '/doc/cat/psychology/drug/catnip/index.md', after all.
  let tagSelf = if dir'' == "doc/" then "" else init $ replace "doc/" "" dir'' -- "doc/cat/psychology/drug/catnip/" → 'cat/psychology/drug/catnip'

  -- for the arabesque navbar 'previous'/'next', we want to fill more useful than the default values, but also not be too redundant with the up/sideways/downwards tag-directory links; so we pass in the (lexicographically) sorted list of all tag-directories being created this run, and try to provide previous/next links to the 'previous' and the 'next' directory, which may be a parent, sibling, or nothing at all.
  -- so eg. /doc/cryonics/index will point to `previous: /doc/crime/terrorism/index \n next: /doc/cs/index`
  let i = fromJust $ elemIndex dir'' dirs
  let (before,after) = splitAt i dirs
  let (previous,next) = if length dirs < 2 || before==after then ("","") else
                            (if null before || last before == dir'' then "" else "previous: /"++last before++"index",
                              if length after < 2 || (after !! 1) == dir'' then "" else "next: /"++(after !! 1)++"index")

  -- actual subdirectories:
  let parentDirectory = takeDirectory $ takeDirectory dir''
  let parentDirectory' = if parentDirectory == "." then "/index" else "/" ++ parentDirectory ++ "/index"
  direntries <- fmap (filter (/="index.md")) $ -- filter out self
                listDirectory dir''
  let direntries' = sort $ map (\entry -> "/"++dir''++entry) direntries

  -- subset the Metadata database down to just the relevant entries for this particular tag-directory
  -- handle the special-case of 'newest', which is a meta-tag-directory, so we need to process it down before passing it to `listTagged` (which needs to do a ton of I/O work)
  let md' = if not newestp then md else filterDbNewest 10 50 50 md
  tagged <- listTagged newestp md' (init dir'')

  -- We allow tags to be cross-listed, not just children.
  -- So '/doc/exercise/index' can be tagged with 'longevity', and it'll show up in the Directory section (not the Files/me section!) of '/doc/longevity/index'.
  -- This allows cross-references without requiring deep nesting—'longevity/exercise' might seem OK enough (although it runs roughshod over a lot of the links in there...), but what about if there was a third? Or fourth?
  let taggedDirs = sort $ map (\(f,_,_) -> f) $ filter (\(f,_,_) -> "/doc/"`isPrefixOf`f && "/index"`isSuffixOf`f && f `notElem` direntries') tagged

  -- we suppress what would be duplicate entries in the File/me section
  let taggedAll  = filter (\(f,_,_) -> not ("/doc/"`isPrefixOf`f && "/index"`isSuffixOf`f)) tagged
  let taggedSelf = filter (\(_,(_,aut,_,_,_,_,_),_) -> aut `elem` ["Gwern", "gwern", "Gwern Branwen"]) taggedAll
  let tagged'    = taggedAll \\ taggedSelf

  dirsChildren   <- listTagDirectoriesAll [dir'']
  dirsSeeAlsos   <- listTagDirectories False taggedDirs

  triplets  <- listFiles md' direntries'

  let linksSelf = sortByDateModified taggedSelf  -- newest first, to show recent additions
  let sorter = if newestp then sortByDateModified else sortByDatePublished
  let linksAll  = sorter $ triplets++tagged'
  -- split into WP vs non-WP:
  let links = filter (\(f,_,_) -> not ("wikipedia.org/wiki/" `isInfixOf` f)) linksAll -- TODO: isWikipedia?
  let linksWP = linksAll \\ links

  -- walk the list of observed links and if they do not have an entry in the annotation database, try to create one now before doing any more work:
  Prelude.mapM_ (\(l,_,_) -> case M.lookup l md' of
                         Nothing -> void $ annotateLink md' (Link nullAttr [] (T.pack l, ""))
                         _ -> return ()
        ) links

  -- a very long List can be hard to browse, and doesn't provide a useful ToC. If we have titles, we can use those as section headers.
  -- (Entries without even a title must be squashed into a list and chucked at the end.)
  let selfTitledLinks = map (\(f,a,_) -> (f,a)) $ filter (\(_,(t,_,_,_,_,_,_),_) -> t /= "") linksSelf
  let titledLinks     = map (\(f,a,_) -> (f,a)) $ filter (\(_,(t,_,_,_,_,_,_),_) -> t /= "") links
  let untitledLinks   = map (\(f,a,_) -> (f,a)) $ filter (\(_,(t,_,_,_,_,_,_),_) -> t == "") links
  titledLinksSorted <- if newestp then return []
                        -- sort-by-magic: NOTE: we skip clustering on the /doc/newest virtual-tag because by being so heterogeneous, the clusters are garbage compared to clustering within a regular tag, and can't be handled heuristically reasonably.
                       else sortSimilarsStartingWithNewestWithTag ldb sortDB md' tagSelf titledLinks

  let selfLinksSection = generateSections' am 2 selfTitledLinks
  let titledLinksSections   = generateSections am titledLinks titledLinksSorted (map (\(f,a,_) -> (f,a)) linksWP)
  let untitledLinksSection  = generateListItems am untitledLinks

  -- take the first image as the 'thumbnail', and preserve any caption/alt text and use as 'thumbnail-text'
  let imageFirst = take 1 $ filter (\(Image _ _ (f,_)) -> not (".svg" `T.isInfixOf` f)) $ -- SVGs break as page thumbnails in many previews, so we exclude them
        concatMap (\(_,(_,_,_,_,_,_,abstract),_) -> extractImages $ toPandoc abstract) links

  let thumbnail = if null imageFirst then "" else "thumbnail: " ++ T.unpack ((\(Image _ _ (imagelink,_)) -> imagelink) (head imageFirst))
  let thumbnailText = replace "fig:" "" $ if null imageFirst then "" else "thumbnail-text: '" ++ replace "'" "''" (T.unpack ((\(Image _ caption (_,altText)) -> let captionText = inlinesToText caption in if captionText /= "" then captionText else if altText /= "" then altText else "") (head imageFirst))) ++ "'"

  let header = generateYAMLHeader parentDirectory' previous next tagSelf (getNewestDate links) (length (dirsChildren++dirsSeeAlsos), length titledLinks, length untitledLinks) thumbnail thumbnailText
  let sectionDirectoryChildren = generateDirectoryItems (Just parentDirectory') dir'' dirsChildren
  let sectionDirectorySeeAlsos = generateDirectoryItems Nothing dir'' dirsSeeAlsos
  let sectionDirectory = Div ("see-alsos", ["directory-indexes", "columns"], []) [BulletList $ sectionDirectoryChildren ++ sectionDirectorySeeAlsos]

  -- A tag index may have an optional Markdown essay/page explaining it; if it does, that is located at `/note/basename($TAG)`, and we transclude it at runtime. If there is no such `/note/`, then we fall back to checking for an entire top-level essay of the same name, `/basename($TAG)`, but that would be far too long to transclude as a whole, so we transclude the summary in that case.
  -- (If we desire to transclude an annotation but the top-level essay file does not exactly coincide with the tag-name, then the workaround is to simply create a `/note/basename($TAG)` which contains only a .include-annotation link in it.)
  abstract <- if not ("doc/" `isPrefixOf` dir'') then return [] else
                do let tagBase = takeDirectory $ last $ splitPath  dir'' -- 'doc/cat/psychology/catnip/' -> 'catnip'
                   let abstractf = "/note/" ++ tagBase --- construct absolute path in the final website, '/note/catnip'
                   abstractp <- doesFileExist (tail abstractf ++ ".md") -- check existence of (relative) file, 'note/catnip.md'
                   essayp    <- doesFileExist (tagBase ++ ".md")
                   return $ if abstractp then [Div ("manual-annotation", ["abstract", "abstract-tag-directory"], []) [Para [Link ("", ["include-content-core", "include-strict", "link-page"], [])
                                                                                                                             [Str "[page summary]"] (T.pack abstractf, T.pack ("Transclude link for " ++ dir'' ++ " notes page."))]]]
                            else if essayp then [Div ("manual-annotation", ["abstract", "abstract-tag-directory"], []) [Para [Link ("", ["include-annotation", "include-strict"], []) [Str "[essay on this tag topic]"] (T.pack ("/" ++ tagBase), T.pack ("Transclude link for " ++ dir'' ++ " annotation of essay on this topic."))]]]
                                 else []

  let linkBibList = generateLinkBibliographyItems $ filter (\(_,(_,_,_,_,_,_,_),lb) -> not (null lb)) links

  let body = abstract ++

             [Header 1 nullAttr [Str "See Also"]] ++ [sectionDirectory] ++

             (if null selfLinksSection then [] else
                [Para []] ++
                 [Header 1 ("", ["display-pop-not"], []) [Str "Gwern"]] ++
                 selfLinksSection) ++

             (if null titledLinks then [] else
                 -- NOTE: we need a <h1> for proper hierarchical tree, but that <h1> uses up a lot of visual space in popups/popovers, and we can't just suppress *all* first-<h1>s, we only want to suppress the ones on directory/tag pages. So we define a new class 'display-pop-not', and the CSS (in default.css's popups section) will suppress that in popups/popovers.
                 [Para []] ++
                 [Header 1 ("", ["display-pop-not"], []) [Str "Links"]] ++
                 titledLinksSections) ++

             (if null untitledLinks then [] else
                 Header 1 nullAttr [Str "Miscellaneous"] :
                 [untitledLinksSection]) ++

               (if null linkBibList then [] else
                 Para [] : Header 1 ("link-bibliography-section", [], []) [Str "Link Bibliography"] :
                 linkBibList)

  let document = Pandoc nullMeta body
  let p = runPure $ writeMarkdown def{writerExtensions = pandocExtensions} $
           walk identUniquefy $ walk (hasAnnotation md') document  -- global rewrite to de-duplicate all of the inserted URLs

  case p of
    Left e   -> printRed (show e)
    -- compare with the old version, and update if there are any differences:
    Right p' -> do let contentsNew = T.pack header `T.append` p'
                   writeUpdatedFile "directory" (dir'' ++ "index.md") contentsNew
  -- putStrLn $ "dir'' done: " ++ dir''

-- we have 3 kinds of entries for a tag-directory: entries written by me, annotations, and partials/links; for /doc/newest, we want to take the 'newest' but set different limits: I don't write new essays nearly as often as I add new annotations or links, so we can't just take _n_ of each.
-- And for new essays, but not annotations/links, we want to use a broader definition of 'new' to include *modified*: as modified >= created, we just use modification time instead
filterDbNewest :: Int -> Int -> Int -> Metadata -> Metadata
filterDbNewest selfN annotationN linkN md = let -- ml = M.toList md
                                                mdAnnotated = M.filterWithKey (\p (_,_,_,_,_,_,abst) -> abst /= "" && not ("/index" `isSuffixOf` p || "/newsletter/" `isPrefixOf` p || "/lorem" `isPrefixOf` p || "/changelog" `isPrefixOf` p)) md
                                                selfs      = take selfN $ sortItemDateModified $ M.toList $ M.filterWithKey (\p (_,aut,_,_,_,_,_) -> aut `elem` ["Gwern", "gwern", "Gwern Branwen"] && not ('#' `elem` p)) mdAnnotated

                                                annotations = take annotationN $ sortItemPathDateCreated $ M.toList $
                                                              M.filter (\(_,aut,_,_,_,_,_) -> aut `notElem` ["Gwern", "gwern", "Gwern Branwen"]) mdAnnotated
                                                links       = take linkN $ sortItemPathDateCreated $ M.toList $
                                                              M.filter (\(_,_,_,_,_,_,abst) -> abst == "") md
                                             in M.fromList $ selfs ++ annotations ++ links

generateLinkBibliographyItems :: [(String,MetadataItem,FilePath)] -> [Block]
generateLinkBibliographyItems [] = []
generateLinkBibliographyItems items = [OrderedList (1, DefaultStyle, DefaultDelim) $ map generateLinkBibliographyItem items]
generateLinkBibliographyItem  :: (String,MetadataItem,FilePath) -> [Block]
generateLinkBibliographyItem x@(_,(_,_,_,_,_,_,_),"") = error $ "generateDirectory.hs.generateLinkBibliographyItem asked to generate a link-bib entry for an item passed to it with no link-bib file defined! This should never happen. Data: " ++ show x
generateLinkBibliographyItem (f,(t,aut,_,_,_,_,_),lb)  =
  let f'
        | "http" `isPrefixOf` f = f
        | "index" `isSuffixOf` f = takeDirectory f
        | otherwise = takeFileName f
      authorSpan  = authorCollapse aut
      author = if aut=="" || aut=="N/A" then []
               else
                 [Str ",", Space] ++ authorSpan -- NOTE: no ':' as usual after the author in an annotation transclusion, because the link-bibliography will be its own section header with a ':' in it so would be redundant here.
      -- I skip date because files don't usually have anything better than year, and that's already encoded in the filename which is shown
  in
    let
        link = if t=="" then Link nullAttr [Code nullAttr (T.pack f')] (T.pack f, "") : author
               else Code nullAttr (T.pack f') : Str ":" : Space : Link nullAttr [Str "“", RawInline (Format "html") (T.pack $ titlecase' t), Str "”"] (T.pack f, "") : author
    in [Para link, Para [Span ("", ["collapse", "tag-index-link-bibliography-block"], []) [Link ("",["include-even-when-collapsed"],[]) [Str "link-bibliography"] (T.pack lb,"Directory-tag link-bibliography for link " `T.append` T.pack f)]]]

generateYAMLHeader :: FilePath -> FilePath -> FilePath -> FilePath -> String -> (Int,Int,Int) -> String -> String -> String
generateYAMLHeader parent previous next d date (directoryN,annotationN,linkN) thumbnail thumbnailText
  = unlines $ filter (not . null) [ "---",
             "title: " ++ (if d=="" then "docs" else T.unpack (abbreviateTag (T.pack (replace "doc/" "" d)))) ++ " tag",
             "description: \"Bibliography for tag <code>" ++ (if d=="" then "docs" else d) ++ "</code>, most recent first: " ++
              (if directoryN == 0 then ""  else "" ++ show directoryN ++ " <a class='icon-not' href='/doc/" ++ (if d=="" then "" else d++"/") ++ "index#see-alsos'>related tag" ++ pl directoryN ++ "</a>") ++
              (if annotationN == 0 then "" else (if directoryN==0 then "" else ", ") ++ show annotationN ++ " <a class='icon-not' href='/doc/" ++ d ++ "/index#links'>annotation" ++ pl annotationN ++ "</a>") ++
              (if linkN == 0 then ""       else (if (directoryN/=0 && annotationN/=0 && linkN/=0) then ", & " else " & ") ++ show linkN ++ " <a class='icon-not' href='/doc/" ++ d ++ "/index#miscellaneous'>link" ++ pl linkN ++ "</a>") ++
              " (<a href='" ++ parent ++ "' class='link-page link-tag directory-indexes-upwards link-annotated' data-link-icon='arrow-up-left' data-link-icon-type='svg' rel='tag' title='Link to parent directory'>parent</a>)" ++
               ".\"",
             thumbnail,
             thumbnailText,
             "thumbnail-css: 'outline'", -- the thumbnails of tag-directories are usually screenshots of graphs/figures/software, so we will default to `.outline` for them
             "created: 'N/A'",
             if date=="" then "" else "modified: \'" ++ date++"\'",
             "status: 'in progress'",
             previous,
             next,
             "confidence: log",
             "importance: 0",
             "css-extension: dropcaps-de-zs",
             "index: true",
             "backlink: False",
             "...\n"]
  where pl n = if n > 1 || n == 0 then "s" else "" -- pluralize helper: "2 links", "1 link", "0 links".

listFiles :: Metadata -> [FilePath] -> IO [(FilePath,MetadataItem,FilePath)]
listFiles m direntries' = do
                   files <- filterM (doesFileExist . tail) direntries'
                   let files'          = (sort . filter (\f -> not $ anySuffix f ["index", ".tar", ".webm-poster.jpg", ".mp4-poster.jpg"]) . map (replace ".md" "") . filter ('#' `notElem`)) files
                   let fileAnnotationsMi = map (lookupFallback m) files'
                   -- NOTE: files may be annotated only under a hash, eg. '/doc/ai/scaling/hardware/2021-norrie.pdf#google'; so we can't look for their backlinks/similar-links under '/doc/ai/scaling/hardware/2021-norrie.pdf', but we ask 'lookupFallback' for the best reference; 'lookupFallback' will tell us that '/doc/ai/scaling/hardware/2021-norrie.pdf' → `('/doc/ai/scaling/hardware/2021-norrie.pdf#google',_)`. This is also true of PDF page-anchors.
                   linkbiblios  <- mapM (fmap snd . getLinkBibLinkCheck . fst) fileAnnotationsMi

                   return $ zipWith (\(a,b) c -> (a,b,c)) fileAnnotationsMi linkbiblios

-- Fetch URLs/file 'tagged' with the current directory but not residing in it.
--
-- tags are only in "doc/*", so "haskell/" etc is out. Tags drop the doc/ prefix, and we want to avoid
-- the actual files inside the current directory, because they'll be covered by the `listFiles` version, of course.
listTagged :: Bool -> Metadata -> FilePath -> IO [(FilePath,MetadataItem,FilePath)]
listTagged newestp m dir = if not ("doc/" `isPrefixOf` dir) then return [] else
                   let dirTag = replace "doc/" "" dir in
                     let tagged = if newestp then m else M.filterWithKey (\u (_,_,_,_,_,tgs,_) -> not (dir `isInfixOf` u) && dirTag `elem` tgs) m in
                       do let files = nubOrd $ map truncateAnchors $ M.keys tagged
                          linkbiblios  <- mapM (fmap snd . getLinkBibLinkCheck) files
                          let fileAnnotationsMi = map (lookupFallback m) files
                          return $ zipWith (\(a,b) c -> (a,b,c)) fileAnnotationsMi linkbiblios
  where
    -- for essays, not files/links, drop section anchors to look up/link:
    truncateAnchors :: String -> String
    truncateAnchors str = if '.' `elem` str then str else takeWhile (/='#') str

-- sort a list of entries in ascending order using the annotation last-modified date when available (as 'YYYY[-MM[-DD]]', which string-sorts correctly), and falling back to sorting on the filenames ('YYYY-author.pdf').
-- We generally prefer to reverse this to descending order, to show newest-first.
-- For cases where only alphabetic sorting is available, we fall back to alphabetical order on the URL.
sortByDateModified :: [(FilePath, MetadataItem, FilePath)] -> [(FilePath, MetadataItem, FilePath)]
sortByDateModified = sortBy compareEntries
  where
    compareEntries (f, (_, _, _, d, _, _, _), _) (f', (_, _, _, d', _, _, _), _)
      | not (null d) || not (null d') = compare d' d -- Reverse order for dates, to show newest first
      | head f == '/' && head f' == '/' = compare f' f -- Reverse order for file paths when both start with '/'
      | head f == '/' = LT -- '/' paths come after non '/' paths
      | head f' == '/' = GT -- non '/' paths come before '/' paths
      | otherwise = compare f f' -- Alphabetical order for the rest

sortByDatePublished :: [(FilePath, MetadataItem, FilePath)] -> [(FilePath, MetadataItem, FilePath)]
sortByDatePublished = sortBy compareEntries
  where
    compareEntries (f, (_, _, d, _, _, _, _), _) (f', (_, _, d', _, _, _, _), _)
      | not (null d) || not (null d') = compare d' d -- Reverse order for dates, to show newest first
      | head f == '/' && head f' == '/' = compare f' f -- Reverse order for file paths when both start with '/'
      | head f == '/' = LT -- '/' paths come after non '/' paths
      | head f' == '/' = GT -- non '/' paths come before '/' paths
      | otherwise = compare f f' -- Alphabetical order for the rest


-- assuming already-descending-sorted input from `sortByDateBoth`, output the date of the first (ie. newest) item:
getNewestDate :: [(FilePath,MetadataItem,FilePath)] -> String
getNewestDate [] = ""
getNewestDate ((_,(_,_,date,date',_,_,_),_):_) = max date date'

generateDirectoryItems :: Maybe FilePath -> FilePath -> [FilePath] -> [[Block]]
generateDirectoryItems parent current ds =
  -- all directories have a parent directory with an index (eg. /doc/index has the parent /index), so we always link it.
  -- (We pass in the parent path to write an absolute link instead of the easier '../' relative link, because relative links break inside popups.)
      -- for directories like ./doc/statistics/ where there are 9+ subdirectories, we'd like to multi-column the directory section to make it more compact (we can't for annotated files/links because there are so many annotations & they are too long to work all that nicely):
     parent'' ++ filter (not . null) (map generateDirectoryItem ds)
 where
       parent'' = case parent of
                     Nothing -> []
                     Just p -> [[Para [Span nullAttr [Link ("",
                                                               ["link-tag", "directory-indexes-upwards"],
                                                               [("rel","tag")]
                                                             )
                                                               [Str "Parent"] (T.pack p, "Link to parent directory '" `T.append` (T.pack $ tail $ takeDirectory p) `T.append` "/' (ascending)")]]]]

       generateDirectoryItem :: FilePath -> [Block]
       -- arrow symbolism: subdirectories are 'down' (prefix because it's 'inside'), while the parent directory is 'up' (handled above); cross-linked directories (due to tags) are then 'out and to the right' (suffix because it's 'across')
       generateDirectoryItem d = let downP = directoryPrefixDown current d
                                     nameShort = T.pack $ replace "/doc/" "" $ takeDirectory d
                                     (nameDisplayed,parenthetical) = if parent == Just "/index" then abbreviateTagLongForm nameShort else (abbreviateTag nameShort, [])
                                 in
                                   [Para ([Link ("",
                                               ["link-tag", if downP then "directory-indexes-downwards" else "directory-indexes-sideways"],
                                               [("rel","tag")]
                                             )
                                               [Emph [RawInline (Format "html") nameDisplayed]] (T.pack d, "")]
                                           ++ parenthetical)
                                 ]
       directoryPrefixDown :: FilePath -> FilePath -> Bool
       directoryPrefixDown currentd d' = ("/"++currentd) `isPrefixOf` d'

       abbreviateTagLongForm :: T.Text -> (T.Text, [Inline])
       abbreviateTagLongForm dir = ("<code>" `T.append`   dir `T.append` "</code>",
                                    [Space, RawInline (Format "html") $ "<span class=\"doc-index-tag-short\">" `T.append` abbreviateTag dir `T.append` "</span>"])

-- for links without a full annotation:
generateListItems :: ArchiveMetadata -> [(FilePath, MetadataItem)] -> Block
generateListItems am p = BulletList (map (\(f,a) -> LM.generateAnnotationTransclusionBlock am (f,a)) p)

generateSections :: ArchiveMetadata -> [(FilePath, MetadataItem)] -> [(String,[(FilePath, MetadataItem)])] -> [(FilePath, MetadataItem)] -> [Block]
generateSections am links linksSorted linkswp = (if null links then [] else annotated) ++
                                                (if length linksSorted < CGS.minTagAuto then [] else sorted) ++
                                                (if null linkswp then [] else wp)
    where annotated = generateSections' am 2 links
          sorted
            = [Header 2 nullAttr
                 [Str "Sort By Magic"]] ++
                 [Div ("",[],[("demo-type", "sort-by-magic-preface")])
                   [Para [Str "Annotations sorted by machine learning into ", Link nullAttr [Str "inferred 'tags'"] ("/design#future-tag-features",""), Str ". This provides an alternative way to browse: instead of by ", Emph [Str "date"], Str " order, one can browse in ", Emph [Str "topic"], Str " order. The 'sorted' list has been automatically clustered into multiple sections & auto-labeled for easier browsing."],
                    Para [Str "Beginning with the newest annotation, it uses the embedding of each annotation to attempt to create a list of nearest-neighbor annotations, creating a progression of topics. For more details, see the link."]
                   ]
                 ] ++
                 concatMap generateReferenceToPreviousSection linksSorted
          wp
            = [Header 2 ("titled-links-wikipedia", [], [])
                 [Str "Wikipedia"],
               OrderedList (1, DefaultStyle, DefaultDelim)
                 (map (LM.generateAnnotationTransclusionBlock am) linkswp)]

-- for the sorted-by-magic links, they all are by definition already generated as a section; so instead of bloating the page & ToC with even more sections, let's just generate a transclude of the original section!
generateReferenceToPreviousSection :: (String, [(FilePath, MetadataItem)]) -> [Block]
generateReferenceToPreviousSection (tag,items) = [Header 3 ("", ["collapse"], [("title","Machine-generated tag name for the following cluster of links.")]) [Code nullAttr (T.pack $ if tag == "" then "N/A" else tag)]] ++
                                             concatMap (\(f,(_,aut,dt,_,_,_,_)) ->
                                                  let linkId = generateID f aut dt in
                                                    if linkId=="" then [] else
                                                      let sectionID = "#" `T.append` linkId `T.append` "-section"
                                                      in [Para [Link ("", ["include", "include-even-when-collapsed"], []) [Str "[see previous entry]"] (sectionID, "")]]
                                                       ) items
generateSections' :: ArchiveMetadata -> Int -> [(FilePath, MetadataItem)] -> [Block]
generateSections' am headerLevel = concatMap (\(f,a@(t,aut,dt,_,_,_,_)) ->
                                let sectionID = if aut=="" then "" else let linkId = generateID f aut dt in
                                                                          if linkId=="" then "" else linkId `T.append` "-section"
                                    authorShort = authorsToCite f aut dt
                                    -- for tag-directory purposes (but nowhere else), we simplify tweet titles to just 'USER @ DATE' if possible.
                                    sectionTitle = if "https://twitter.com/" `isPrefixOf` f then T.pack $ twitterTitle f dt else
                                                     T.pack $ "“"++titlecase' t++"”" ++
                                                     (if authorShort=="" then "" else ", " ++ authorsToCite f aut dt)
                                in
                                 Header headerLevel (sectionID, [], []) [RawInline (Format "html") sectionTitle]
                                 : LM.generateAnnotationTransclusionBlock am (f,a))
   where twitterTitle u dte = let username = extractTwitterUsername u in username ++ (if null dte then "" else " @ " ++ show dte)
