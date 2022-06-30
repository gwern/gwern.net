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

import Control.Monad (filterM)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, sort, sortBy)
import Utils (replace)
import Data.Text.Titlecase (titlecase)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, takeFileName)
import Text.Pandoc (def, nullAttr, nullMeta, pandocExtensions, runPure, writeMarkdown, writerExtensions,
                    Block(BulletList, Div, Header, Para, RawBlock), Format(Format), Inline(Code, Emph, Image, Link, Space, Span, Str, RawInline), Pandoc(Pandoc))
import qualified Data.Map as M (keys, lookup, size, toList, filterWithKey)
import qualified Data.Text as T (append, pack, unpack)
import System.IO (stderr, hPrint)
import Control.Monad.Parallel as Par (mapM_)
import Text.Pandoc.Walk (walk)

import Interwiki (inlinesToText)
import LinkAuto (cleanUpDivsEmpty)
import LinkMetadata (readLinkMetadata, generateAnnotationBlock, generateID, authorsToCite, authorsTruncate, tagsToLinksSpan, Metadata, MetadataItem, parseRawBlock, abbreviateTag, hasAnnotation)
import LinkBacklink (getBackLink, getSimilarLink)
import Query (extractImages)
import Typography (identUniquefy)
import Utils (writeUpdatedFile, sed)

main :: IO ()
main = do dirs <- getArgs
          let dirs' = map (\dir -> replace "//" "/" ((if "./" `isPrefixOf` dir then drop 2 dir else dir) ++ "/")) dirs

          meta <- readLinkMetadata

          Par.mapM_ (generateDirectory meta) dirs' -- because of the expense of searching the annotation database for each tag, it's worth parallelizing as much as possible. (We could invert and do a single joint search, but at the cost of ruining our clear top-down parallel workflow.)

generateDirectory :: Metadata -> FilePath -> IO ()
generateDirectory md dir'' = do

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
  let taggedDirs = sort $ map (\(f,_,_,_) -> f) $ filter (\(f,_,_,_) -> "/docs/"`isPrefixOf`f && "/index"`isSuffixOf`f && f `notElem` direntries') tagged

  -- we suppress what would be duplicate entries in the File/Links section
  let tagged' = filter (\(f,_,_,_) -> not ("/docs/"`isPrefixOf`f && "/index"`isSuffixOf`f)) tagged

  dirsChildren   <- listDirectories direntries'
  dirsSeeAlsos   <- listDirectories taggedDirs

  triplets  <- listFiles md direntries'

  let links = nub $ reverse $ sortByDate $ triplets++tagged' -- newest first, to show recent additions

  -- remove the tag for *this* directory; it is redundant to display 'cat/catnip' on every doc/link inside '/docs/cat/catnip/index.page', after all.
  let tagSelf = init $ replace "docs/" "" dir'' -- "docs/cat/catnip/" → 'cat/catnip'
  let links' = map (\(y,(a,b,c,d,tags,f),z,zz) -> (y,(a,b,c,d, filter (/= tagSelf) tags,f),z,zz)) links

  -- a very long List can be hard to browse, and doesn't provide a useful ToC. If we have titles, we can use those as section headers.
  -- (Entries without even a title must be squashed into a list and chucked at the end.)
  let titledLinks   = filter (\(_,(t,_,_,_,_,_),_,_) -> t /= "") links'
  let untitledLinks = filter (\(_,(t,_,_,_,_,_),_,_) -> t == "") links'
  let allUnannotatedUntitledP = all (=="") $ map (\(_,(_,_,_,_,_,annotation),_,_) -> annotation) untitledLinks -- whether to be compact columns

  let titledLinksSections   = generateSections titledLinks
  let untitledLinksSection  = generateListItems untitledLinks

  -- take the first image as the 'thumbnail', and preserve any caption/alt text and use as 'thumbnailText'
  let imageFirst = take 1 $ extractImages (Pandoc nullMeta titledLinksSections)
  let thumbnail = if null imageFirst then "" else "thumbnail: " ++ T.unpack ((\(Image _ _ (imagelink,_)) -> imagelink) (head imageFirst)) ++ "\n"
  let thumbnailText = replace "fig:" "" $ if null imageFirst then "" else "thumbnailText: '" ++ replace "'" "''" (T.unpack ((\(Image _ caption (_,altText)) -> let captionText = inlinesToText caption in if not (captionText == "") then captionText else if not (altText == "") then altText else "") (head imageFirst))) ++ "'\n"

  let header = generateYAMLHeader tagSelf (getNewestDate links) (length (dirsChildren++dirsSeeAlsos), length titledLinks, length untitledLinks) (thumbnail++thumbnailText)
  let directorySectionChildren = generateDirectoryItems (Just parentDirectory') dir'' dirsChildren
  let directorySectionSeeAlsos = generateDirectoryItems Nothing dir'' dirsSeeAlsos
  let directorySection = Div ("see-alsos", ["directory-indexes", "columns", "directorySectionChildren"], []) [BulletList $ directorySectionChildren ++ directorySectionSeeAlsos]

  -- A tag index may have an optional header explaining or commenting on it. If it does, it is defined as a link annotation at the ID '/docs/foo/index#manual-annotation'
  let abstract = case M.lookup ("/"++dir''++"index#manual-annotation") md of
                   Nothing -> []
                   Just (_,_,_,_,_,"") -> []
                   Just (_,_,_,_,_,dirAbstract) -> [parseRawBlock ("",["abstract"],[]) $ RawBlock (Format "html") (T.pack $ "<blockquote>"++dirAbstract++"</blockquote>")]

  let body = abstract ++

             [Header 1 ("", ["display-pop-not", "link-annotated-not"], []) [Str "See Also"]] ++ [directorySection] ++

             (if null titledLinks then [] else
                 -- NOTE: we need a <h1> for proper hierarchical tree, but that <h1> uses up a lot of visual space in popups/popins, and we can't just suppress *all* first-<h1>s, we only want to suppress the ones on directory/tag pages. So we define a new class 'display-pop-not', and the CSS (in default.css's popups section) will suppress that in popups/popins.
                 [Para []] ++
                 [Header 1 ("", ["display-pop-not", "link-annotated-not"], []) [Str "Links"]] ++
                 titledLinksSections) ++

             (if null untitledLinks then [] else
                 Header 1 ("", ["link-annotated-not"], []) [Str "Miscellaneous"] :
                 -- for lists, they *may* all be devoid of annotations and short
                 if not allUnannotatedUntitledP then [untitledLinksSection] else
                   [RawBlock (Format "html") "<div id=\"miscellaneous-links-list\" class=\"columns\">\n\n",
                    untitledLinksSection,
                    RawBlock (Format "html") "</div>"])

  let document = Pandoc nullMeta body
  let p = runPure $ writeMarkdown def{writerExtensions = pandocExtensions} $
           walk identUniquefy $ walk (hasAnnotation md True) document  -- global rewrite to de-duplicate all of the inserted URLs

  case p of
    Left e   -> hPrint stderr e
    -- compare with the old version, and update if there are any differences:
    Right p' -> do let contentsNew = T.pack header `T.append` p'
                   writeUpdatedFile "directory" (dir'' ++ "index.page") contentsNew

generateYAMLHeader :: FilePath -> String -> (Int,Int,Int) -> String -> String
generateYAMLHeader d date (directoryN,annotationN,linkN) thumbnail
  = concat [ "---\n",
             "title: " ++ T.unpack (abbreviateTag (T.pack (replace "docs/" "" d))) ++ " directory\n",
             "author: 'N/A'\n",
             "description: \"Bibliography for tag <em>" ++ d ++ "</em>, most recent first: " ++
              (if directoryN == 0 then ""  else "" ++ show directoryN ++ " <a class='no-icon link-annotated-not' href='/docs/" ++ d ++ "/index#see-alsos'>related tag" ++ pl directoryN ++ "</a>") ++
              (if annotationN == 0 then "" else (if directoryN==0 then "" else ", ") ++ show annotationN ++ " <a class='no-icon link-annotated-not' href='/docs/" ++ d ++ "/index#links'>annotation" ++ pl annotationN ++ "</a>") ++
              (if linkN == 0 then ""       else (if (directoryN+annotationN) > 0 then ", & " else ", ") ++ show linkN ++ " <a class='no-icon link-annotated-not' href='/docs/" ++ d ++ "/index#miscellaneous'>link" ++ pl linkN ++ "</a>") ++
               ".\"\n",
             thumbnail,
             "created: 'N/A'\n",
             if date=="" then "" else "modified: " ++ date ++ "\n",
             "status: in progress\n",
             "confidence: log\n",
             "importance: 0\n",
             "cssExtension: drop-caps-de-zs\n",
             "index: true\n",
             "...\n",
             "\n"]
  where pl n = if n > 1 || n == 0 then "s" else "" -- pluralize helper: "2 links", "1 link", "0 links".

-- given a list of ["docs/foo/index.page"] directories, convert them to what will be the final absolute path ("/docs/foo/index"), while checking they exist (typos are easy, eg. dropping 'docs/' is common).
listDirectories :: [FilePath] -> IO [FilePath]
listDirectories direntries' = do
                       directories <- filterM (doesDirectoryExist . tail) $ map (sed "/index$" "/" . replace "/index.page" "/")  direntries'
                       let directoriesMi = map (replace "//" "/" . (++"/index")) directories
                       filterM (\f -> doesFileExist $ tail (f++".page")) directoriesMi

listFiles :: Metadata -> [FilePath] -> IO [(FilePath,MetadataItem,FilePath,FilePath)]
listFiles m direntries' = do
                   files <- filterM (doesFileExist . tail) direntries'
                   let files'          = (sort . filter (not . ("index"`isSuffixOf`)) . map (replace ".page" "") . filter ('#' `notElem`) . filter (not . isSuffixOf ".tar") ) files
                   let fileAnnotationsMi = map (lookupFallback m) files'
                   -- NOTE: files may be annotated only under a hash, eg. '/docs/ai/scaling/hardware/2021-norrie.pdf#google'; so we can't look for their backlinks/similar-links under '/docs/ai/scaling/hardware/2021-norrie.pdf', but we ask 'lookupFallback' for the best reference; 'lookupFallback' will tell us that '/docs/ai/scaling/hardware/2021-norrie.pdf' → `('/docs/ai/scaling/hardware/2021-norrie.pdf#google',_)`
                   backlinks    <- mapM (getBackLink . fst) fileAnnotationsMi
                   similarlinks <- mapM (getSimilarLink . fst) fileAnnotationsMi

                   return $
                     zipWith3 (\(a,b) c d -> (a,b,c,d)) fileAnnotationsMi backlinks similarlinks

-- Fetch URLs/file 'tagged' with the current directory but not residing in it.
--
-- tags are only in "docs/*", so "haskell/" etc is out. Tags drop the docs/ prefix, and we want to avoid
-- the actual files inside the current directory, because they'll be covered by the `listFiles` version, of course.
listTagged :: Metadata -> FilePath -> IO [(FilePath,MetadataItem,FilePath,FilePath)]
listTagged m dir = if not ("docs/" `isPrefixOf` dir) then return [] else
                   let dirTag = replace "docs/" "" dir in
                     let tagged = M.filterWithKey (\u (_,_,_,_,tgs,_) -> not (dir `isInfixOf` u) && dirTag `elem` tgs) m in
                       do let files = nub $ map truncateAnchors $ M.keys tagged
                          backlinks    <- mapM getBackLink files
                          similarlinks <- mapM getSimilarLink files
                          let fileAnnotationsMi = map (lookupFallback m) files
                          return $
                            zipWith3 (\(a,b) c d -> (a,b,c,d)) fileAnnotationsMi backlinks similarlinks
  where
    -- for essays, not files/links, drop section anchors to look up/link:
    truncateAnchors :: String -> String
    truncateAnchors str = if '.' `elem` str then str else takeWhile (/='#') str

-- sort a list of entries in ascending order using the annotation date when available (as 'YYYY[-MM[-DD]]', which string-sorts correctly), and falling back to sorting on the filenames ('YYYY-author.pdf').
-- We generally prefer to reverse this to descending order, to show newest-first.
sortByDate :: [(FilePath,MetadataItem,FilePath,FilePath)] -> [(FilePath,MetadataItem,FilePath,FilePath)]
sortByDate = sortBy (\(f,(_,_,d,_,_,_),_,_) (f',(_,_,d',_,_,_),_,_) -> if not (null d && null d') then (if d > d' then GT else LT) else (if f > f' then GT else LT))

-- assuming already-descending-sorted input from `sortByDate`, output the date of the first (ie. newest) item:
getNewestDate :: [(FilePath,MetadataItem,FilePath,FilePath)] -> String
getNewestDate [] = ""
getNewestDate ((_,(_,_,date,_,_,_),_,_):_) = date

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
                                                               [Str "Parent"] (T.pack p, "Link to parent directory '" `T.append`  (T.pack $ takeDirectory p) `T.append` "/' (ascending)")]]]]

       generateDirectoryItem :: FilePath -> [Block]
       -- arrow symbolism: subdirectories are 'down' (prefix because it's 'inside'), while the parent directory is 'up' (handled above); cross-linked directories (due to tags) are then 'out and to the right' (suffix because it's 'across')
       generateDirectoryItem d = let downP = directoryPrefixDown current d in
                                   [Para [Link ("",
                                               ["link-tag", if downP then "directory-indexes-downwards" else "directory-indexes-sideways"],
                                               [("rel","tag")]
                                             )
                                               [Emph [Str $ abbreviateTag $ T.pack $ takeDirectory d]] (T.pack d, "")]
                                 ]
       directoryPrefixDown :: FilePath -> FilePath -> Bool
       directoryPrefixDown currentd d' = ("/"++currentd) `isPrefixOf` d'


generateListItems :: [(FilePath, MetadataItem,FilePath,FilePath)] -> Block
generateListItems p = BulletList (map generateItem p)

generateSections :: [(FilePath, MetadataItem,FilePath,FilePath)] -> [Block]
generateSections = concatMap (\p@(f,(t,aut,dt,_,_,_),_,_) ->
                                let sectionID = if aut=="" then "" else let linkId = (generateID f aut dt) in
                                                                          if linkId=="" then "" else linkId `T.append` "-section"
                                    authorShort = authorsToCite f aut dt
                                    sectionTitle = T.pack $ if "wikipedia"`isInfixOf`f then t else "“"++titlecase t++"”" ++
                                                     (if authorShort=="" then "" else ", " ++ authorsToCite f aut dt)
                                in
                                 [Header 2 (sectionID, ["link-annotated-not"], []) [RawInline (Format "html") sectionTitle]]
                                 ++ generateItem p)

generateItem :: (FilePath,MetadataItem,FilePath,FilePath) -> [Block]
generateItem (f,(t,aut,dt,_,tgs,""),bl,sl) = -- no abstracts:
 if ("https://en.wikipedia.org/wiki/"`isInfixOf`f) then [Para [Link nullAttr [Str "Wikipedia"] (T.pack f, if t=="" then "" else T.pack $ "Wikipedia link about " ++ t)]]
 else
  let
       f'       = if "http"`isPrefixOf`f then f else if "index" `isSuffixOf` f then takeDirectory f else takeFileName f
       title    = if t=="" then [Code nullAttr (T.pack f')] else [Str (T.pack $ "“"++t++"”")]
       prefix   = if t=="" then [] else [Code nullAttr (T.pack f'), Str ": "]
       -- we display short authors by default, but we keep a tooltip of the full author list for on-hover should the reader need it.
       authorShort = authorsTruncate aut
       authorSpan  = if authorShort/=aut then Span ("",["full-authors-list"],[("title", T.pack aut)]) [Str (T.pack $ authorsTruncate aut)]
                     else Str (T.pack $ authorShort)
       author   = if aut=="" || aut=="N/A" then [] else [Str ",", Space, authorSpan]
       date     = if dt=="" then [] else [Span ("", ["date"], []) [Str (T.pack dt)]]
       tags     = if tgs==[] then [] else (if dt/="" then [Str "; "] else []) ++ [tagsToLinksSpan $ map T.pack tgs]
       backlink = if bl=="" then [] else (if dt=="" && tgs==[] then [] else [Str ";", Space]) ++ [Span ("", ["backlinks"], []) [Link ("",["aux-links", "link-local", "backlinks"],[]) [Str "backlinks"] (T.pack bl,"Reverse citations/backlinks for this page (the list of other pages which link to this URL).")]]
       similar  = if sl=="" then [] else [Str ";", Space, Span ("", ["similars"], []) [Link ("",["aux-links", "link-local", "similar"],[]) [Str "similar"] (T.pack sl,"Similar links (by text embedding).")]]
  in
  if (tgs==[] && bl=="" && dt=="") then [Para (prefix ++ Link nullAttr title (T.pack f, "") : (author))]
  else [Para (Link nullAttr title (T.pack f, "") : (author ++ [Space, Str "("] ++ date ++ tags ++ backlink ++ similar ++ [Str ")"]))]
-- long abstracts:
generateItem (f,a,bl,sl) =
  -- render annotation as: (skipping DOIs)
  --
  -- > [`2010-lucretius-dererumnatura.pdf`: "On The Nature of Things"](/docs/philosophy/2010-lucretius-dererumnatura.pdf), Lucretius (55BC-01-01):
  -- >
  -- > > A poem on the Epicurean model of the world...
  walk cleanUpDivsEmpty $ walk (parseRawBlock nullAttr) $ generateAnnotationBlock ("/"`isPrefixOf`f) True False (f,Just a) bl sl
