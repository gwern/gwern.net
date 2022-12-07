#!/usr/bin/env runghc
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Read directories like "docs/iodine/" for its files, as well as any files/links with annotations &
-- the tag 'iodine'; generate a list item with the abstract in a blockquote where available; the
-- full list is then turned into a directory listing, which gets compiled with Hakyll and gets the
-- usual popup annotations. We can then treat directories as 'tags', unifying two
-- apparently-separate systems of organization.
--
-- Very nifty. Much nicer than simply browsing a list of filenames or even the Google search of a
-- directory (mostly showing random snippets).

import Control.Monad (filterM, void)
import Data.List (elemIndex, isPrefixOf, isInfixOf, isSuffixOf, nub, sort, sortBy, zipWith4, (\\))
import Data.List.Utils (countElem)
import Data.Maybe (fromJust)
import Data.Text.Titlecase (titlecase)
import System.Directory (listDirectory, doesFileExist)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, takeFileName)
import Text.Pandoc (def, nullAttr, nullMeta, pandocExtensions, runPure, writeMarkdown, writerExtensions,
                    Block(BulletList, Div, Header, Para, RawBlock, OrderedList), ListNumberDelim(DefaultDelim), ListNumberStyle(DefaultStyle, UpperAlpha), Format(Format), Inline(Code, Emph, Image, Link, Space, Span, Str, RawInline), Pandoc(Pandoc))
import qualified Data.Map as M (keys, lookup, size, toList, filterWithKey)
import qualified Data.Text as T (append, pack, unpack)
import Control.Monad.Parallel as Par (mapM_)
import Text.Pandoc.Walk (walk)

import Interwiki (inlinesToText)
import LinkID (generateID, authorsToCite)
import LinkMetadata (readLinkMetadata, generateAnnotationTransclusionBlock, authorsTruncate, parseRawBlock, hasAnnotation, dateTruncateBad, parseRawInline, annotateLink)
import LinkMetadataTypes (Metadata, MetadataItem)
import Tags (tagsToLinksSpan, listTagDirectories, abbreviateTag)
import LinkBacklink (getBackLinkCheck, getSimilarLinkCheck, getLinkBibLinkCheck)
import Query (extractImages)
import Typography (identUniquefy)
import Utils (replace, writeUpdatedFile, printRed)

main :: IO ()
main = do dirs <- getArgs
          let dirs' = map (\dir -> replace "//" "/" ((if "./" `isPrefixOf` dir then drop 2 dir else dir) ++ "/")) $ sort dirs

          meta <- readLinkMetadata

          Par.mapM_ (generateDirectory meta dirs') dirs' -- because of the expense of searching the annotation database for each tag, it's worth parallelizing as much as possible. (We could invert and do a single joint search, but at the cost of ruining our clear top-down parallel workflow.)

generateDirectory :: Metadata -> [FilePath] -> FilePath -> IO ()
generateDirectory md dirs dir'' = do

  -- for the arabesque navbar 'previous'/'next', we want to fill more useful than the default values, but also not be too redundant with the up/sideways/downwards tag-directory links; so we pass in the (lexicographically) sorted list of all tag-directories being created this run, and try to provide previous/next links to the 'previous' and the 'next' directory, which may be a parent, sibling, or nothing at all.
  -- so eg. /docs/cryonics/index will point to `previous: /docs/crime/terrorism/index \n next: /docs/cs/index`
  let i = fromJust $ elemIndex dir'' dirs
  let (before,after) = splitAt i dirs
  let (previous,next) = if length dirs < 2 || before==after then ("","") else
                            (if null before || last before == dir'' then "" else "previous: /"++last before++"index",
                              if length after < 2 || head (drop 1 after) == dir'' then "" else "next: /"++head (drop 1 after)++"index")

  tagged <- listTagged md (init dir'')

  -- actual subdirectories:
  let parentDirectory = takeDirectory $ takeDirectory dir''
  let parentDirectory' = if parentDirectory == "." then "/index" else "/" ++ parentDirectory ++ "/index"
  direntries <- fmap (filter (/="index.page")) $ -- filter out self
                listDirectory dir''
  let direntries' = sort $ map (\entry -> "/"++dir''++entry) direntries

  -- We allow tags to be cross-listed, not just children.
  -- So '/docs/exercise/index' can be tagged with 'longevity', and it'll show up in the Directory section (not the Files/Links section!) of '/docs/longevity/index'.
  -- This allows cross-references without requiring deep nesting—'longevity/exercise' might seem OK enough (although it runs roughshod over a lot of the links in there...), but what about if there was a third? Or fourth?
  let taggedDirs = sort $ map (\(f,_,_,_,_) -> f) $ filter (\(f,_,_,_,_) -> "/docs/"`isPrefixOf`f && "/index"`isSuffixOf`f && f `notElem` direntries') tagged

  -- we suppress what would be duplicate entries in the File/Links section
  let tagged' = filter (\(f,_,_,_,_) -> not ("/docs/"`isPrefixOf`f && "/index"`isSuffixOf`f)) tagged

  dirsChildren   <- listTagDirectories [dir'']
  dirsSeeAlsos   <- listTagDirectories taggedDirs

  triplets  <- listFiles md direntries'

  let linksAll = nub $ reverse $ sortByDate $ triplets++tagged' -- newest first, to show recent additions
  -- split into WP vs non-WP:
  let links = filter (\(f,_,_,_,_) -> not ("https://en.wikipedia.org/wiki/" `isPrefixOf` f)) linksAll
  let linksWP = linksAll \\ links

  -- walk the list of observed links and if they do not have an entry in the annotation database, try to create one now before doing any more work:
  Prelude.mapM_ (\(l,_,_,_,_) -> case M.lookup l md of
                         Nothing -> void $ annotateLink md (Link nullAttr [] (T.pack l, ""))
                         _ -> return ()
        ) links

  -- remove the tag for *this* directory; it is redundant to display 'cat/catnip' on every doc/link inside '/docs/cat/catnip/index.page', after all.
  let tagSelf = if dir'' == "docs/" then "" else init $ replace "docs/" "" dir'' -- "docs/cat/catnip/" → 'cat/catnip'
  let links' = map (\(y,(a,b,c,d,tags,f),z,zz,zzz) -> (y,(a,b,c,d, filter (/= tagSelf) tags,f),z,zz,zzz)) links

  -- a very long List can be hard to browse, and doesn't provide a useful ToC. If we have titles, we can use those as section headers.
  -- (Entries without even a title must be squashed into a list and chucked at the end.)
  let titledLinks   = filter (\(_,(t,_,_,_,_,_),_,_,_) -> t /= "") links'
  let untitledLinks = filter (\(_,(t,_,_,_,_,_),_,_,_) -> t == "") links'
  let allUnannotatedUntitledP = (length untitledLinks >= 3) && (all (=="") $ map (\(_,(_,_,_,_,_,annotation),_,_,_) -> annotation) untitledLinks) -- whether to be compact columns

  let titledLinksSections   = generateSections titledLinks linksWP
  let untitledLinksSection  = generateListItems untitledLinks

  -- take the first image as the 'thumbnail', and preserve any caption/alt text and use as 'thumbnailText'
  let imageFirst = take 1 $ extractImages (Pandoc nullMeta titledLinksSections)
  let thumbnail = if null imageFirst then "" else "thumbnail: " ++ T.unpack ((\(Image _ _ (imagelink,_)) -> imagelink) (head imageFirst)) ++ "\n"
  let thumbnailText = replace "fig:" "" $ if null imageFirst then "" else "thumbnailText: '" ++ replace "'" "''" (T.unpack ((\(Image _ caption (_,altText)) -> let captionText = inlinesToText caption in if captionText /= "" then captionText else if altText /= "" then altText else "") (head imageFirst))) ++ "'\n"

  let header = generateYAMLHeader parentDirectory' previous next tagSelf (getNewestDate links) (length (dirsChildren++dirsSeeAlsos), length titledLinks, length untitledLinks) (thumbnail++thumbnailText)
  let sectionDirectoryChildren = generateDirectoryItems (Just parentDirectory') dir'' dirsChildren
  let sectionDirectorySeeAlsos = generateDirectoryItems Nothing dir'' dirsSeeAlsos
  let sectionDirectory = Div ("see-alsos", ["directory-indexes", "columns"], []) [BulletList $ sectionDirectoryChildren ++ sectionDirectorySeeAlsos]

  -- A tag index may have an optional header explaining or commenting on it. If it does, it is defined as a link annotation at the ID '/docs/foo/index#manual-annotation'
  let abstract = case M.lookup ("/"++dir''++"index#manual-annotation") md of
                   Nothing -> []
                   Just (_,_,_,_,_,"") -> []
                   Just (_,_,_,_,_,dirAbstract) -> [parseRawBlock ("",["abstract", "abstract-tag-directory"],[]) $ RawBlock (Format "html") (T.pack $ "<blockquote>"++dirAbstract++"</blockquote>")]

  let linkBibList = generateLinkBibliographyItems $ filter (\(_,(_,_,_,_,_,_),_,_,lb) -> not (null lb)) links'

  let body = abstract ++

             [Header 1 nullAttr [Str "See Also"]] ++ [sectionDirectory] ++

             (if null titledLinks then [] else
                 -- NOTE: we need a <h1> for proper hierarchical tree, but that <h1> uses up a lot of visual space in popups/popins, and we can't just suppress *all* first-<h1>s, we only want to suppress the ones on directory/tag pages. So we define a new class 'display-pop-not', and the CSS (in default.css's popups section) will suppress that in popups/popins.
                 [Para []] ++
                 [Header 1 ("", ["display-pop-not", "link-annotated-not"], []) [Str "Links"]] ++
                 titledLinksSections) ++

             (if null untitledLinks then [] else
                 Header 1 ("", ["link-annotated-not"], []) [Str "Miscellaneous"] :
                 if not allUnannotatedUntitledP then [untitledLinksSection] else
                   [RawBlock (Format "html") "<div id=\"miscellaneous-links-list\">\n\n",
                    untitledLinksSection,
                    RawBlock (Format "html") "</div>"]) ++

               (if null linkBibList then [] else
                 Header 1 ("", ["link-annotated-not"], []) [Str "Link Bibliography"] :
                 linkBibList)

  let document = Pandoc nullMeta body
  let p = runPure $ writeMarkdown def{writerExtensions = pandocExtensions} $
           walk identUniquefy $ walk (hasAnnotation md) document  -- global rewrite to de-duplicate all of the inserted URLs

  case p of
    Left e   -> printRed (show e)
    -- compare with the old version, and update if there are any differences:
    Right p' -> do let contentsNew = T.pack header `T.append` p'
                   writeUpdatedFile "directory" (dir'' ++ "index.page") contentsNew

generateLinkBibliographyItems :: [(String,MetadataItem,FilePath,FilePath,FilePath)] -> [Block]
generateLinkBibliographyItems [] = []
generateLinkBibliographyItems items = [OrderedList (1, DefaultStyle, DefaultDelim) $ map generateLinkBibliographyItem items]
generateLinkBibliographyItem  :: (String,MetadataItem,FilePath,FilePath,FilePath) -> [Block]
generateLinkBibliographyItem x@(_,(_,_,_,_,_,_),_,_,"") = error $ "generateDirectory.hs.generateLinkBibliographyItem asked to generate a link-bib entry for an item passed to it with no link-bib file defined! This should never happen. Data: " ++ show x
generateLinkBibliographyItem (f,(t,aut,_,_,_,_),_,_,lb)  =
  let f'
        | "http" `isPrefixOf` f = f
        | "index" `isSuffixOf` f = takeDirectory f
        | otherwise = takeFileName f
      authorShort = authorsTruncate aut
      authorSpan  = if authorShort/=aut then Span ("",["full-authors-list"],[("title", T.pack aut)]) [Str (T.pack $ authorsTruncate aut)]
                    else Str (T.pack authorShort)
      author = if aut=="" || aut=="N/A" then []
               else
                 [Str ",", Space, authorSpan, Str ":"]
      -- I skip date because files don't usually have anything better than year, and that's already encoded in the filename which is shown
  in
    let linkAttr = if "https://en.wikipedia.org/wiki/" `isPrefixOf` f then ("",["include-annotation"],[]) else nullAttr
        link = if t=="" then Link linkAttr [Code nullAttr (T.pack f')] (T.pack f, "") : author
               else Code nullAttr (T.pack f') : Str ":" : Space : Link linkAttr [Str "“", Str (T.pack $ titlecase t), Str "”"] (T.pack f, "") : author
    in [Para link, Para [Link ("",["include", "include-replace-container"],[]) [Str "link-bibliography"] (T.pack lb,"Directory-tag link-bibliography for link " `T.append` (T.pack f))]]

generateYAMLHeader :: FilePath -> FilePath -> FilePath -> FilePath -> String -> (Int,Int,Int) -> String -> String
generateYAMLHeader parent previous next d date (directoryN,annotationN,linkN) thumbnail
  = concat [ "---\n",
             "title: " ++ (if d=="" then "docs" else T.unpack (abbreviateTag (T.pack (replace "docs/" "" d)))) ++ " tag\n",
             "author: 'N/A'\n",
             "description: \"Bibliography for tag <code>" ++ (if d=="" then "docs" else d) ++ "</code>, most recent first: " ++
              (if directoryN == 0 then ""  else "" ++ show directoryN ++ " <a class='icon-not link-annotated-not' href='/docs/" ++ (if d=="" then "" else d++"/") ++ "index#see-alsos'>related tag" ++ pl directoryN ++ "</a>") ++
              (if annotationN == 0 then "" else (if directoryN==0 then "" else ", ") ++ show annotationN ++ " <a class='icon-not link-annotated-not' href='/docs/" ++ d ++ "/index#links'>annotation" ++ pl annotationN ++ "</a>") ++
              (if linkN == 0 then ""       else (if (directoryN+annotationN) > 0 then ", & " else "") ++ show linkN ++ " <a class='icon-not link-annotated-not' href='/docs/" ++ d ++ "/index#miscellaneous'>link" ++ pl linkN ++ "</a>") ++
              " (<a href='" ++ parent ++ "' class='link-page link-tag directory-indexes-upwards link-annotated link-annotated-partial' data-link-icon='arrow-up-left' data-link-icon-type='svg' rel='tag' title='Link to parent directory'>parent</a>)" ++
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

listFiles :: Metadata -> [FilePath] -> IO [(FilePath,MetadataItem,FilePath,FilePath,FilePath)]
listFiles m direntries' = do
                   files <- filterM (doesFileExist . tail) direntries'
                   let files'          = (sort . filter (not . ("index"`isSuffixOf`)) . map (replace ".page" "") . filter ('#' `notElem`) . filter (not . isSuffixOf ".tar") ) files
                   let fileAnnotationsMi = map (lookupFallback m) files'
                   -- NOTE: files may be annotated only under a hash, eg. '/docs/ai/scaling/hardware/2021-norrie.pdf#google'; so we can't look for their backlinks/similar-links under '/docs/ai/scaling/hardware/2021-norrie.pdf', but we ask 'lookupFallback' for the best reference; 'lookupFallback' will tell us that '/docs/ai/scaling/hardware/2021-norrie.pdf' → `('/docs/ai/scaling/hardware/2021-norrie.pdf#google',_)`
                   backlinks    <- mapM (fmap snd . getBackLinkCheck . fst)    fileAnnotationsMi
                   similarlinks <- mapM (fmap snd . getSimilarLinkCheck . fst) fileAnnotationsMi
                   linkbiblios  <- mapM (fmap snd . getLinkBibLinkCheck . fst) fileAnnotationsMi

                   return $ zipWith4 (\(a,b) c d e -> (a,b,c,d,e)) fileAnnotationsMi backlinks similarlinks linkbiblios

-- Fetch URLs/file 'tagged' with the current directory but not residing in it.
--
-- tags are only in "docs/*", so "haskell/" etc is out. Tags drop the docs/ prefix, and we want to avoid
-- the actual files inside the current directory, because they'll be covered by the `listFiles` version, of course.
listTagged :: Metadata -> FilePath -> IO [(FilePath,MetadataItem,FilePath,FilePath,FilePath)]
listTagged m dir = if not ("docs/" `isPrefixOf` dir) then return [] else
                   let dirTag = replace "docs/" "" dir in
                     let tagged = M.filterWithKey (\u (_,_,_,_,tgs,_) -> not (dir `isInfixOf` u) && dirTag `elem` tgs) m in
                       do let files = nub $ map truncateAnchors $ M.keys tagged
                          backlinks    <- mapM (fmap snd . getBackLinkCheck)    files
                          similarlinks <- mapM (fmap snd . getSimilarLinkCheck) files
                          linkbiblios  <- mapM (fmap snd . getLinkBibLinkCheck) files
                          let fileAnnotationsMi = map (lookupFallback m) files
                          return $ zipWith4 (\(a,b) c d e -> (a,b,c,d,e)) fileAnnotationsMi backlinks similarlinks linkbiblios
  where
    -- for essays, not files/links, drop section anchors to look up/link:
    truncateAnchors :: String -> String
    truncateAnchors str = if '.' `elem` str then str else takeWhile (/='#') str

-- sort a list of entries in ascending order using the annotation date when available (as 'YYYY[-MM[-DD]]', which string-sorts correctly), and falling back to sorting on the filenames ('YYYY-author.pdf').
-- We generally prefer to reverse this to descending order, to show newest-first.
-- For cases where only alphabetic sorting is available, we fall back to alphabetical order on the URL.
sortByDate :: [(FilePath,MetadataItem,FilePath,FilePath,FilePath)] -> [(FilePath,MetadataItem,FilePath,FilePath,FilePath)]
sortByDate = sortBy (\(f,(_,_,d,_,_,_),_,_,_) (f',(_,_,d',_,_,_),_,_,_) ->
                       if not (null d && null d') then (if d > d' then GT else LT)
                       else (if head f == '/' && head f' == '/' then (if f > f' then GT else LT) else
                                if head f == '/' && head f' /= '/' then GT else
                                  if head f /= '/' && head f' == '/' then LT else
                                    (if f > f' then LT else GT)))

-- assuming already-descending-sorted input from `sortByDate`, output the date of the first (ie. newest) item:
getNewestDate :: [(FilePath,MetadataItem,FilePath,FilePath,FilePath)] -> String
getNewestDate [] = ""
getNewestDate ((_,(_,_,date,_,_,_),_,_,_):_) = date

-- how do we handle files with appended data, which are linked like '/docs/reinforcement-learning/model-free/2020-bellemare.pdf#google' but exist as files as '/docs/reinforcement-learning/model-free/2020-bellemare.pdf'? We can't just look up the *filename* because it's missing the # fragment, and the annotation is usually for the full path including the fragment. If a lookup fails, we fallback to looking for any annotation with the file as a *prefix*, and accept the first match.
lookupFallback :: Metadata -> String -> (FilePath, MetadataItem)
lookupFallback m u = case M.lookup u m of
                       Nothing -> tryPrefix
                       Just ("","","","",[],"") -> tryPrefix
                       Just mi -> (u,mi)
                       where tryPrefix = let possibles =  M.filterWithKey (\url _ -> u `isPrefixOf` url && url /= u) m
                                             u' = if M.size possibles > 0 then fst $ head $ M.toList possibles else u
                                         in
                                               (if (".page" `isInfixOf` u') || (u == u') then (u, ("", "", "", "", [], "")) else
                                                  -- sometimes the fallback is useless eg, a link to a section will trigger a 'longer' hit, like
                                                  -- '/reviews/Cat-Sense.page' will trigger a fallback to /reviews/Cat-Sense#fuzz-testing'; the
                                                  -- longer hit will also be empty, usually, and so not better. We check for that case and return
                                                  -- the original path and not the longer path.
                                                  let possibleFallback = lookupFallback m u' in
                                                    if snd possibleFallback == ("", "", "", "", [], "") then (u, ("", "", "", "", [], "")) else
                                                      (u',snd possibleFallback))

generateDirectoryItems :: Maybe FilePath -> FilePath -> [FilePath] -> [[Block]]
generateDirectoryItems parent current ds =
  -- all directories have a parent directory with an index (eg. /docs/index has the parent /index), so we always link it.
  -- (We pass in the parent path to write an absolute link instead of the easier '../' relative link, because relative links break inside popups.)
      -- for directories like ./docs/statistics/ where there are 9+ subdirectories, we'd like to multi-column the directory section to make it more compact (we can't for annotated files/links because there are so many annotations & they are too long to work all that nicely):
     parent'' ++ (filter (not . null) (map generateDirectoryItem ds))
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
       generateDirectoryItem d = let downP = directoryPrefixDown current d in
                                   [Para [Link ("",
                                               ["link-tag", if downP then "directory-indexes-downwards" else "directory-indexes-sideways"],
                                               [("rel","tag")]
                                             )
                                               [Emph [RawInline (Format "html") $ abbreviateTag $ T.pack $ replace "/docs/" "" $ takeDirectory d]] (T.pack d, "")]
                                 ]
       directoryPrefixDown :: FilePath -> FilePath -> Bool
       directoryPrefixDown currentd d' = ("/"++currentd) `isPrefixOf` d'


generateListItems :: [(FilePath, MetadataItem,FilePath,FilePath,FilePath)] -> Block
generateListItems p = BulletList (map generateItem p)

generateSections :: [(FilePath, MetadataItem,FilePath,FilePath,FilePath)] -> [(FilePath, MetadataItem,FilePath,FilePath,FilePath)] -> [Block]
generateSections links [] = generateSections' links
generateSections links linkswp = generateSections' links ++ [Header 2 ("titled-links-wikipedia", ["link-annotated-not"], []) [Str "Wikipedia"],
                                                               OrderedList (1, UpperAlpha, DefaultDelim) (map generateItem linkswp)]

generateSections' :: [(FilePath, MetadataItem,FilePath,FilePath,FilePath)] -> [Block]
generateSections' = concatMap (\p@(f,(t,aut,dt,_,_,_),_,_,_) ->
                                let sectionID = if aut=="" then "" else let linkId = (generateID f aut dt) in
                                                                          if linkId=="" then "" else linkId `T.append` "-section"
                                    authorShort = authorsToCite f aut dt
                                    sectionTitle = T.pack $ "“"++titlecase t++"”" ++
                                                     (if authorShort=="" then "" else ", " ++ authorsToCite f aut dt)
                                in
                                 [Header 2 (sectionID, ["link-annotated-not"], []) [parseRawInline nullAttr $ RawInline (Format "html") sectionTitle]]
                                 ++ generateItem p)

generateItem :: (FilePath,MetadataItem,FilePath,FilePath,FilePath) -> [Block]
generateItem (f,(t,aut,dt,_,tgs,""),bl,sl,lb) = -- no abstracts:
 if ("https://en.wikipedia.org/wiki/"`isPrefixOf`f) then [Para [Link ("",["include-annotation"],[]) [Str (T.pack t)] (T.pack f, if t=="" then "" else T.pack $ "Wikipedia link about " ++ t)]]
 else
  let
       f'       = if "http"`isPrefixOf`f then f else if "index" `isSuffixOf` f then takeDirectory f else takeFileName f
       title    = if t=="" then [Code nullAttr (T.pack f')] else [RawInline (Format "html") (T.pack $ "“"++t++"”")]
       -- prefix   = if t=="" then [] else [Code nullAttr (T.pack f'), Str ": "]
       -- we display short authors by default, but we keep a tooltip of the full author list for on-hover should the reader need it.
       authorShort = authorsTruncate aut
       authorSpan  = if authorShort/=aut || ", et al" `isSuffixOf` aut then Span ("",["full-authors-list", "cite-author-plural"],[("title", T.pack aut)]) [Str (T.pack $ replace ", et al" "" authorShort)]
                     else Span ("", ["author", "cite-author"], []) [Str (T.pack $ if countElem ',' aut == 1 then replace ", " " & " authorShort else  authorShort)]
       author   = if aut=="" || aut=="N/A" then [] else [Str ",", Space, authorSpan]
       date     = if dt=="" then [] else [Span ("", ["cite-date"], []) [Str (T.pack (dateTruncateBad dt))]]
       tags     = if tgs==[] then [] else [tagsToLinksSpan $ map T.pack tgs]
       backlink = if bl=="" then [] else (if null tgs then [] else [Str ";", Space]) ++ [Span ("", ["backlinks"], []) [Link ("",["aux-links", "link-page", "backlinks", "icon-not"],[]) [Str "backlinks"] (T.pack bl,"Reverse citations/backlinks for this page (the list of other pages which link to this URL).")]]
       similar  = if sl=="" then [] else [Str ";", Space, Span ("", ["similars"], []) [Link ("",["aux-links", "link-page", "similar", "icon-not"],[]) [Str "similar"] (T.pack sl,"Similar links (by text embedding).")]]
       linkBibliography = if lb=="" then [] else (if bl=="" && sl=="" && tags==[] then [] else [Str ";", Space]) ++ [Span ("", ["link-bibliography"], []) [Link ("",["aux-links", "link-page", "icon-not"],[]) [Str "bibliography"] (T.pack lb, "Link-bibliography for this annotation (list of links it cites).")]]
  in
  if (tgs==[] && bl=="" && dt=="") then [Para (Link nullAttr title (T.pack f, "") : author)]
  else [Div ("", ["annotation-partial"], [])
         [Para (Link nullAttr title (T.pack f, "") : (author ++ date ++ (if null (tags ++ backlink ++ similar)
                                                                        then []
                                                                        else [Space, Str "("] ++ tags ++ backlink ++ similar ++ linkBibliography ++ [Str ")"]))
                                                   )]]
-- long abstracts:
generateItem (f,a,bl,sl,lb) = generateAnnotationTransclusionBlock (f,a) bl sl lb
