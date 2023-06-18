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

import Control.Monad (filterM, void)
import Data.List (elemIndex, isPrefixOf, isInfixOf, isSuffixOf, nub, sort, sortBy, (\\))
import Data.Maybe (fromJust)
import Data.Text.Titlecase (titlecase)
import System.Directory (listDirectory, doesFileExist)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, takeFileName)
import Text.Pandoc (def, nullAttr, nullMeta, pandocExtensions, runPure, writeMarkdown, writerExtensions,
                    Block(BulletList, Div, Header, Para, RawBlock, OrderedList), ListNumberDelim(DefaultDelim), ListNumberStyle(DefaultStyle, UpperAlpha), Format(Format), Inline(Code, Emph, Image, Link, Space, Span, Str, RawInline), Pandoc(Pandoc))
import qualified Data.Map as M (keys, lookup, filterWithKey)
import qualified Data.Text as T (append, pack, unpack, Text)
import Control.Parallel.Strategies (parListChunk, rseq, using)
import Text.Pandoc.Walk (walk)

import Interwiki (inlinesToText)
import LinkID (generateID, authorsToCite)
import LinkMetadata as LM (readLinkMetadata, readLinkMetadataNewest, generateAnnotationTransclusionBlock, authorsTruncate, parseRawBlock, hasAnnotation, parseRawInline, annotateLink, lookupFallback)
import LinkMetadataTypes (Metadata, MetadataItem)
import Tags (listTagDirectories, abbreviateTag)
import LinkBacklink (getLinkBibLinkCheck)
import Query (extractImages)
import Typography (identUniquefy)
import Utils (replace, writeUpdatedFile, printRed, toPandoc, anySuffix)
import Config.Misc as C (miscellaneousLinksCollapseLimit)
import GenerateSimilar (sortSimilarsStartingWithNewestWithTag)
-- import Text.Show.Pretty (ppShow)

main :: IO ()
main = do dirs <- getArgs
          -- result: '["doc/","doc/ai/","doc/ai/anime/","doc/ai/anime/danbooru/","doc/ai/dataset/", ..., "newsletter/2022/","nootropic/","note/","review/","zeo/"]'
          let dirs' = map (\dir -> replace "//" "/" ((if "./" `isPrefixOf` dir then drop 2 dir else dir) ++ "/")) dirs

          meta <- readLinkMetadata

          -- Prelude.mapM_ (generateDirectory True meta dirs') dirs'
          Prelude.mapM_ (generateDirectory True meta dirs') (dirs' `using` parListChunk chunkSize rseq) -- because of the expense of searching the annotation database for each tag, it's worth parallelizing as much as possible. (We could invert and do a single joint search, but at the cost of ruining our clear top-down parallel workflow.)

          -- Special-case directories:
          -- 'newest': the _n_ newest link annotations created (currently, 'newest' is not properly tracked, and is inferred from being at the bottom/end of full.yaml/partial.yaml TODO: actually track annotation creation dates...)
          metaNewest <- readLinkMetadataNewest 100
          generateDirectory False metaNewest ["doc/", "doc/newest/", "/"] "doc/newest/"
  where chunkSize :: Int
        chunkSize = 10

generateDirectory :: Bool -> Metadata -> [FilePath] -> FilePath -> IO ()
generateDirectory filterp md dirs dir'' = do

  -- print dirs >> print dir''

  -- for the arabesque navbar 'previous'/'next', we want to fill more useful than the default values, but also not be too redundant with the up/sideways/downwards tag-directory links; so we pass in the (lexicographically) sorted list of all tag-directories being created this run, and try to provide previous/next links to the 'previous' and the 'next' directory, which may be a parent, sibling, or nothing at all.
  -- so eg. /doc/cryonics/index will point to `previous: /doc/crime/terrorism/index \n next: /doc/cs/index`
  let i = fromJust $ elemIndex dir'' dirs
  let (before,after) = splitAt i dirs
  let (previous,next) = if length dirs < 2 || before==after then ("","") else
                            (if null before || last before == dir'' then "" else "previous: /"++last before++"index",
                              if length after < 2 || (after !! 1) == dir'' then "" else "next: /"++(after !! 1)++"index")

  tagged <- listTagged filterp md (init dir'')

  -- actual subdirectories:
  let parentDirectory = takeDirectory $ takeDirectory dir''
  let parentDirectory' = if parentDirectory == "." then "/index" else "/" ++ parentDirectory ++ "/index"
  direntries <- fmap (filter (/="index.page")) $ -- filter out self
                listDirectory dir''
  let direntries' = sort $ map (\entry -> "/"++dir''++entry) direntries

  -- We allow tags to be cross-listed, not just children.
  -- So '/doc/exercise/index' can be tagged with 'longevity', and it'll show up in the Directory section (not the Files/me section!) of '/doc/longevity/index'.
  -- This allows cross-references without requiring deep nesting—'longevity/exercise' might seem OK enough (although it runs roughshod over a lot of the links in there...), but what about if there was a third? Or fourth?
  let taggedDirs = sort $ map (\(f,_,_) -> f) $ filter (\(f,_,_) -> "/doc/"`isPrefixOf`f && "/index"`isSuffixOf`f && f `notElem` direntries') tagged

  -- we suppress what would be duplicate entries in the File/me section
  let tagged' = filter (\(f,_,_) -> not ("/doc/"`isPrefixOf`f && "/index"`isSuffixOf`f)) tagged

  dirsChildren   <- listTagDirectories [dir'']
  dirsSeeAlsos   <- listTagDirectories taggedDirs

  triplets  <- listFiles md direntries'

  let linksAll = nub $ reverse $ sortByDate $ triplets++tagged' -- newest first, to show recent additions
  -- split into WP vs non-WP:
  let links = filter (\(f,_,_) -> not ("https://en.wikipedia.org/wiki/" `isPrefixOf` f)) linksAll
  let linksWP = linksAll \\ links

  -- walk the list of observed links and if they do not have an entry in the annotation database, try to create one now before doing any more work:
  Prelude.mapM_ (\(l,_,_) -> case M.lookup l md of
                         Nothing -> void $ annotateLink md (Link nullAttr [] (T.pack l, ""))
                         _ -> return ()
        ) links

  -- a very long List can be hard to browse, and doesn't provide a useful ToC. If we have titles, we can use those as section headers.
  -- (Entries without even a title must be squashed into a list and chucked at the end.)
  let titledLinks   = map (\(f,a,_) -> (f,a)) $ filter (\(_,(t,_,_,_,_,_),_) -> t /= "") links
  let untitledLinks = map (\(f,a,_) -> (f,a)) $ filter (\(_,(t,_,_,_,_,_),_) -> t == "") links
  let allUnannotatedUntitledP = (length untitledLinks >= 3) && all (=="") (map (\(_,(_,_,_,_,_,annotation)) -> annotation) untitledLinks) -- whether to be compact columns
  -- print ("titledLinks:"::String) >> putStrLn (ppShow $ sort titledLinks)
  titledLinksSorted <- sortSimilarsStartingWithNewestWithTag md titledLinks
  -- print ("-------------------------------------------------------"::String) >> print ("titledLinksSorted:"::String) >> print titledLinksSorted

  let titledLinksSections   = generateSections  titledLinks titledLinksSorted (map (\(f,a,_) -> (f,a)) linksWP)
  let untitledLinksSection  = generateListItems untitledLinks

  -- take the first image as the 'thumbnail', and preserve any caption/alt text and use as 'thumbnailText'
  let imageFirst = take 1 $ concatMap (\(_,(_,_,_,_,_,abstract),_) -> extractImages (toPandoc abstract)) links

  let thumbnail = if null imageFirst then "" else "thumbnail: " ++ T.unpack ((\(Image _ _ (imagelink,_)) -> imagelink) (head imageFirst)) ++ "\n"
  let thumbnailText = replace "fig:" "" $ if null imageFirst then "" else "thumbnailText: '" ++ replace "'" "''" (T.unpack ((\(Image _ caption (_,altText)) -> let captionText = inlinesToText caption in if captionText /= "" then captionText else if altText /= "" then altText else "") (head imageFirst))) ++ "'\n"

  -- remove the tag for *this* directory; it is redundant to display 'cat/catnip' on every doc/link inside '/doc/cat/catnip/index.page', after all.
  let tagSelf = if dir'' == "doc/" then "" else init $ replace "doc/" "" dir'' -- "doc/cat/catnip/" ? 'cat/catnip'

  let header = generateYAMLHeader parentDirectory' previous next tagSelf (getNewestDate links) (length (dirsChildren++dirsSeeAlsos), length titledLinks, length untitledLinks) (thumbnail++thumbnailText)
  let sectionDirectoryChildren = generateDirectoryItems (Just parentDirectory') dir'' dirsChildren
  let sectionDirectorySeeAlsos = generateDirectoryItems Nothing dir'' dirsSeeAlsos
  let sectionDirectory = Div ("see-alsos", ["directory-indexes", "columns"], []) [BulletList $ sectionDirectoryChildren ++ sectionDirectorySeeAlsos]

  -- A tag index may have an optional header explaining or commenting on it. If it does, it is defined as a link annotation at the ID '/doc/foo/index#manual-annotation'
  let abstract = case M.lookup ("/"++dir''++"index#manual-annotation") md of
                   Nothing -> []
                   Just (_,_,_,_,_,"") -> []
                   Just (_,_,_,_,_,dirAbstract) -> [parseRawBlock ("",["abstract", "abstract-tag-directory"],[]) $ RawBlock (Format "html") (T.pack $ "<blockquote>"++dirAbstract++"</blockquote>")]

  let linkBibList = generateLinkBibliographyItems $ filter (\(_,(_,_,_,_,_,_),lb) -> not (null lb)) links

  let body = abstract ++

             [Header 1 nullAttr [Str "See Also"]] ++ [sectionDirectory] ++

             (if null titledLinks then [] else
                 -- NOTE: we need a <h1> for proper hierarchical tree, but that <h1> uses up a lot of visual space in popups/popins, and we can't just suppress *all* first-<h1>s, we only want to suppress the ones on directory/tag pages. So we define a new class 'display-pop-not', and the CSS (in default.css's popups section) will suppress that in popups/popins.
                 [Para []] ++
                 [Header 1 ("", ["display-pop-not", "link-annotated-not"], []) [Str "Links"]] ++
                 titledLinksSections) ++

             (if null untitledLinks then [] else
                 Header 1 ("", ["link-annotated-not"] ++ (if length untitledLinks > C.miscellaneousLinksCollapseLimit then ["collapse"] else []), []) [Str "Miscellaneous"] :
                 if not allUnannotatedUntitledP then [untitledLinksSection] else
                   [RawBlock (Format "html") "<div id=\"miscellaneous-links-list\">\n\n",
                    untitledLinksSection,
                    RawBlock (Format "html") "</div>"]) ++

               (if null linkBibList then [] else
                 Para [] : Header 1 ("link-bibliography-section", ["link-annotated-not"], []) [Str "Link Bibliography"] :
                 linkBibList)

  let document = Pandoc nullMeta body
  let p = runPure $ writeMarkdown def{writerExtensions = pandocExtensions} $
           walk identUniquefy $ walk (hasAnnotation md) document  -- global rewrite to de-duplicate all of the inserted URLs

  case p of
    Left e   -> printRed (show e)
    -- compare with the old version, and update if there are any differences:
    Right p' -> do let contentsNew = T.pack header `T.append` p'
                   writeUpdatedFile "directory" (dir'' ++ "index.page") contentsNew
  -- putStrLn $ "dir'' done: " ++ dir''

generateLinkBibliographyItems :: [(String,MetadataItem,FilePath)] -> [Block]
generateLinkBibliographyItems [] = []
generateLinkBibliographyItems items = [OrderedList (1, DefaultStyle, DefaultDelim) $ map generateLinkBibliographyItem items]
generateLinkBibliographyItem  :: (String,MetadataItem,FilePath) -> [Block]
generateLinkBibliographyItem x@(_,(_,_,_,_,_,_),"") = error $ "generateDirectory.hs.generateLinkBibliographyItem asked to generate a link-bib entry for an item passed to it with no link-bib file defined! This should never happen. Data: " ++ show x
generateLinkBibliographyItem (f,(t,aut,_,_,_,_),lb)  =
  let f'
        | "http" `isPrefixOf` f = f
        | "index" `isSuffixOf` f = takeDirectory f
        | otherwise = takeFileName f
      authorShort = authorsTruncate aut
      authorSpan  = if authorShort/=aut then Span ("",["full-authors-list"],[("title", T.pack aut)]) [Str (T.pack $ authorsTruncate aut)]
                    else Str (T.pack authorShort)
      author = if aut=="" || aut=="N/A" then []
               else
                 [Str ",", Space, authorSpan] -- NOTE: no ':' as usual after the author in an annotation transclusion, because the link-bibliography will be its own section header with a ':' in it so would be redundant here.
      -- I skip date because files don't usually have anything better than year, and that's already encoded in the filename which is shown
  in
    let linkAttr = if "https://en.wikipedia.org/wiki/" `isPrefixOf` f then ("",["include-annotation"],[]) else nullAttr
        link = if t=="" then Link linkAttr [Code nullAttr (T.pack f')] (T.pack f, "") : author
               else Code nullAttr (T.pack f') : Str ":" : Space : Link linkAttr [Str "“", Str (T.pack $ titlecase t), Str "”"] (T.pack f, "") : author
    in [Para link, Para [Span ("", ["collapse", "tag-index-link-bibliography-block"], []) [Link ("",["include-even-when-collapsed"],[]) [Str "link-bibliography"] (T.pack lb,"Directory-tag link-bibliography for link " `T.append` (T.pack f))]]]

generateYAMLHeader :: FilePath -> FilePath -> FilePath -> FilePath -> String -> (Int,Int,Int) -> String -> String
generateYAMLHeader parent previous next d date (directoryN,annotationN,linkN) thumbnail
  = concat [ "---\n",
             "title: " ++ (if d=="" then "docs" else T.unpack (abbreviateTag (T.pack (replace "doc/" "" d)))) ++ " tag\n",
             "author: 'N/A'\n",
             "description: \"Bibliography for tag <code>" ++ (if d=="" then "docs" else d) ++ "</code>, most recent first: " ++
              (if directoryN == 0 then ""  else "" ++ show directoryN ++ " <a class='icon-not link-annotated-not' href='/doc/" ++ (if d=="" then "" else d++"/") ++ "index#see-alsos'>related tag" ++ pl directoryN ++ "</a>") ++
              (if annotationN == 0 then "" else (if directoryN==0 then "" else ", ") ++ show annotationN ++ " <a class='icon-not link-annotated-not' href='/doc/" ++ d ++ "/index#links'>annotation" ++ pl annotationN ++ "</a>") ++
              (if linkN == 0 then ""       else (if (directoryN+annotationN) > 0 then ", & " else "") ++ show linkN ++ " <a class='icon-not link-annotated-not' href='/doc/" ++ d ++ "/index#miscellaneous'>link" ++ pl linkN ++ "</a>") ++
              " (<a href='" ++ parent ++ "' class='link-page link-tag directory-indexes-upwards link-annotated' data-link-icon='arrow-up-left' data-link-icon-type='svg' rel='tag' title='Link to parent directory'>parent</a>)" ++
               ".\"\n",
             thumbnail,
             "created: 'N/A'\n",
             if date=="" then "" else "modified: " ++ date ++ "\n",
             "status: in progress\n",
             previous++"\n",
             next++"\n",
             "confidence: log\n",
             "importance: 0\n",
             "cssExtension: drop-caps-de-zs\n",
             "index: true\n",
             "...\n",
             "\n"]
  where pl n = if n > 1 || n == 0 then "s" else "" -- pluralize helper: "2 links", "1 link", "0 links".

listFiles :: Metadata -> [FilePath] -> IO [(FilePath,MetadataItem,FilePath)]
listFiles m direntries' = do
                   files <- filterM (doesFileExist . tail) direntries'
                   let files'          = (sort . filter (\f -> not $ anySuffix f ["index", ".tar", ".webm-poster.jpg", ".mp4-poster.jpg"]) . map (replace ".page" "") . filter ('#' `notElem`)) files
                   let fileAnnotationsMi = map (lookupFallback m) files'
                   -- NOTE: files may be annotated only under a hash, eg. '/doc/ai/scaling/hardware/2021-norrie.pdf#google'; so we can't look for their backlinks/similar-links under '/doc/ai/scaling/hardware/2021-norrie.pdf', but we ask 'lookupFallback' for the best reference; 'lookupFallback' will tell us that '/doc/ai/scaling/hardware/2021-norrie.pdf' → `('/doc/ai/scaling/hardware/2021-norrie.pdf#google',_)`
                   -- backlinks    <- mapM (fmap snd . getBackLinkCheck . fst)    fileAnnotationsMi
                   -- similarlinks <- mapM (fmap snd . getSimilarLinkCheck . fst) fileAnnotationsMi
                   linkbiblios  <- mapM (fmap snd . getLinkBibLinkCheck . fst) fileAnnotationsMi

                   return $ zipWith (\(a,b) c -> (a,b,c)) fileAnnotationsMi linkbiblios

-- Fetch URLs/file 'tagged' with the current directory but not residing in it.
--
-- tags are only in "doc/*", so "haskell/" etc is out. Tags drop the doc/ prefix, and we want to avoid
-- the actual files inside the current directory, because they'll be covered by the `listFiles` version, of course.
listTagged :: Bool -> Metadata -> FilePath -> IO [(FilePath,MetadataItem,FilePath)]
listTagged filterp m dir = if not ("doc/" `isPrefixOf` dir) then return [] else
                   let dirTag = replace "doc/" "" dir in
                     let tagged = if not filterp then m else M.filterWithKey (\u (_,_,_,_,tgs,_) -> not (dir `isInfixOf` u) && dirTag `elem` tgs) m in
                       do let files = nub $ map truncateAnchors $ M.keys tagged
                          -- backlinks    <- mapM (fmap snd . getBackLinkCheck)    files
                          -- similarlinks <- mapM (fmap snd . getSimilarLinkCheck) files
                          linkbiblios  <- mapM (fmap snd . getLinkBibLinkCheck) files
                          let fileAnnotationsMi = map (lookupFallback m) files
                          return $ zipWith (\(a,b) c -> (a,b,c)) fileAnnotationsMi linkbiblios
  where
    -- for essays, not files/links, drop section anchors to look up/link:
    truncateAnchors :: String -> String
    truncateAnchors str = if '.' `elem` str then str else takeWhile (/='#') str

-- sort a list of entries in ascending order using the annotation date when available (as 'YYYY[-MM[-DD]]', which string-sorts correctly), and falling back to sorting on the filenames ('YYYY-author.pdf').
-- We generally prefer to reverse this to descending order, to show newest-first.
-- For cases where only alphabetic sorting is available, we fall back to alphabetical order on the URL.
sortByDate :: [(FilePath,MetadataItem,FilePath)] -> [(FilePath,MetadataItem,FilePath)]
sortByDate = sortBy (\(f,(_,_,d,_,_,_),_) (f',(_,_,d',_,_,_),_) ->
                       if not (null d && null d') then (if d > d' then GT else LT)
                       else (if head f == '/' && head f' == '/' then (if f > f' then GT else LT) else
                                if head f == '/' && head f' /= '/' then GT else
                                  if head f /= '/' && head f' == '/' then LT else
                                    (if f > f' then LT else GT)))

-- assuming already-descending-sorted input from `sortByDate`, output the date of the first (ie. newest) item:
getNewestDate :: [(FilePath,MetadataItem,FilePath)] -> String
getNewestDate [] = ""
getNewestDate ((_,(_,_,date,_,_,_),_):_) = date

generateDirectoryItems :: Maybe FilePath -> FilePath -> [FilePath] -> [[Block]]
generateDirectoryItems parent current ds =
  -- all directories have a parent directory with an index (eg. /doc/index has the parent /index), so we always link it.
  -- (We pass in the parent path to write an absolute link instead of the easier '../' relative link, because relative links break inside popups.)
      -- for directories like ./doc/statistics/ where there are 9+ subdirectories, we'd like to multi-column the directory section to make it more compact (we can't for annotated files/links because there are so many annotations & they are too long to work all that nicely):
     parent'' ++ filter (not . null) (map generateDirectoryItem ds)
 where
       parent'' = case parent of
                     Nothing -> []
                     Just p -> [[Para [Span ("",[],[]) [Link ("",
                                                               ["link-tag", "directory-indexes-upwards"],
                                                               [("rel","tag")]
                                                             )
                                                               [Str "Parent"] (T.pack p, "Link to parent directory '" `T.append`  (T.pack $ init $ takeDirectory p) `T.append` "/' (ascending)")]]]]

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
                                    [Space, RawInline (Format "html") $ "(" `T.append` abbreviateTag dir `T.append` ")"])

generateListItems :: [(FilePath, MetadataItem)] -> Block
generateListItems p = BulletList (map (\(f,a) -> LM.generateAnnotationTransclusionBlock (f,a)) p)

generateSections :: [(FilePath, MetadataItem)] -> [(String,[(FilePath, MetadataItem)])] -> [(FilePath, MetadataItem)] -> [Block]
generateSections links linksSorted linkswp
    | null linkswp && null links = []
    | null linkswp && not (null links) = annotated ++ sorted
    | not (null linkswp) && null links = wp
    | otherwise                        = annotated ++ sorted ++ wp
    where annotated = generateSections' 2 links
          sorted
            = [Header 2 ("", ["link-annotated-not", "collapse"], [])
                 [Str "Sort By Magic"],
               OrderedList (1, DefaultStyle, DefaultDelim)
                 (map generateReferenceToPreviousSection linksSorted)]
          wp
            = [Header 2 ("titled-links-wikipedia", ["link-annotated-not"], [])
                 [Str "Wikipedia"],
               OrderedList (1, DefaultStyle, DefaultDelim)
                 (map LM.generateAnnotationTransclusionBlock linkswp)]

-- for the sorted-by-magic links, they all are by definition already generated as a section; so instead of bloating the page & ToC with even more sections, let's just generate a transclude of the original section!
generateReferenceToPreviousSection :: (String, [(FilePath, MetadataItem)]) -> [Block]
generateReferenceToPreviousSection (tag,items) = [Para [Code nullAttr (T.pack tag), Str ":"],
                                                  OrderedList (1, UpperAlpha, DefaultDelim) $
                                             concatMap (\(f,(_,aut,dt,_,_,_)) ->
                                                  let linkId = generateID f aut dt in
                                                    if linkId=="" then [] else
                                                      let sectionID = "#" `T.append` linkId `T.append` "-section"
                                                      in [[Para [Link ("", ["include"], []) [Str "[previous entry]"] (sectionID, "")]]]
                                                       ) items
                                           ]
generateSections' :: Int -> [(FilePath, MetadataItem)] -> [Block]
generateSections' headerLevel = concatMap (\(f,a@(t,aut,dt,_,_,_)) ->
                                let sectionID = if aut=="" then "" else let linkId = generateID f aut dt in
                                                                          if linkId=="" then "" else linkId `T.append` "-section"
                                    authorShort = authorsToCite f aut dt
                                    sectionTitle = T.pack $ "“"++titlecase t++"”" ++
                                                     (if authorShort=="" then "" else ", " ++ authorsToCite f aut dt)
                                in
                                 Header headerLevel (sectionID, ["link-annotated-not"], []) [parseRawInline nullAttr $ RawInline (Format "html") sectionTitle]
                                 : LM.generateAnnotationTransclusionBlock (f,a))
