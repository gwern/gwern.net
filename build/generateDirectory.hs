#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Read directories like "docs/iodine/" for its files, as well as any files/links with annotations & the tag 'iodine'; generate a list item with the abstract in a blockquote where available; the full list is then turned into a directory listing, which gets compiled with Hakyll and gets the usual popup annotations.
-- Very nifty. Much nicer than simply browsing a list of filenames or even the Google search of a directory (mostly showing random snippets).

import Control.Monad (filterM)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, sort, sortBy)
import Data.List.Utils (replace)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist, renameFile, removeFile)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, takeFileName)
import Text.Pandoc (def, nullAttr, nullMeta, pandocExtensions, runPure, writeMarkdown, writerExtensions,
                    Block(BulletList, Header, Para, RawBlock), Format(Format), Inline(Code, Link, Space, Span, Str, RawInline),  Pandoc(Pandoc))
import qualified Data.Map as M (keys, lookup, size, toList, filterWithKey)
import qualified Data.Text as T (unpack, pack, append)
import System.IO (stderr, hPrint)
import System.IO.Temp (writeSystemTempFile)
import Control.Monad.Parallel as Par (mapM_)

import LinkMetadata (readLinkMetadata, generateAnnotationBlock, getBackLink, generateID, authorsToCite, Metadata, MetadataItem)

main :: IO ()
main = do dirs <- getArgs
          let dirs' = map (\dir -> replace "//" "/" ((if "./" `isPrefixOf` dir then drop 2 dir else dir) ++ "/")) dirs

          meta <- readLinkMetadata

          Par.mapM_ (generateDirectory meta) dirs' -- because of the expense of searching the annotation database for each directory-tag, it's worth parallelizing as much as possible. (We could invert and do a single joint search, but at the cost of ruining our clear top-down parallel workflow.)

generateDirectory :: Metadata -> FilePath -> IO ()
generateDirectory mta dir'' = do
  let parentDirectory = takeDirectory $ takeDirectory dir''
  let parentDirectory' = if parentDirectory == "." then "/index" else "/" ++ parentDirectory ++ "/index"
  direntries <- listDirectory dir''
  let direntries' = map (\entry -> "/"++dir''++entry) direntries

  dirs   <- listDirectories direntries'

  pairs  <- listFiles  mta  direntries'
  tagged <- listTagged mta  (init dir'')
  let links = nub $ reverse $ sortByDate $ pairs++tagged -- newest first, to show recent additions

  -- remove the tag for *this* directory; it is redundant to display 'cat/catnip' on every doc/link inside '/docs/cat/catnip/index.page', after all.
  let tagSelf = init $ replace "docs/" "" dir'' -- "docs/cat/catnip/" → 'cat/catnip'
  let links' = map (\(y,(a,b,c,d,tags,f),z) -> (y,(a,b,c,d, filter (/= tagSelf) tags,f),z)) links

  -- a very long List can be hard to browse, and doesn't provide a useful ToC. If we have titles, we can use those as section headers.
  -- (Entries without even a title must be squashed into a list and chucked at the end.)
  let titledLinks   = filter (\(_,(t,_,_,_,_,_),_) -> t /= "") links'
  let untitledLinks = filter (\(_,(t,_,_,_,_,_),_) -> t == "") links'
  let allUnannotatedUntitledP = all (=="") $ map (\(_,(_,_,_,_,_,annotation),_) -> annotation) untitledLinks -- whether to be compact columns

  let titledLinksSections   = generateSections titledLinks
  let untitledLinksSection  = generateListItems untitledLinks

  let header = generateYAMLHeader dir''
  let directorySection = generateDirectoryItems parentDirectory' dirs

  let body = [Header 1 nullAttr [Str "Directories"]] ++
               -- for pages like ./docs/statistics/index.page where there are 9+ subdirectories, we'd like to multi-column the directory section (we can't for files/links because there are so many annotations):
               (if length dirs < 8 then [directorySection] else
                 [RawBlock (Format "html") "<div class=\"columns\">\n\n",
                   directorySection,
                   RawBlock (Format "html") "</div>"]) ++

               (if null titledLinks then [] else
                   [Header 1 nullAttr [Str "Links"]] ++
                   titledLinksSections) ++

               (if null untitledLinks then [] else
                   [Header 1 nullAttr [Str "Miscellaneous"]] ++
                   -- for lists, they *may* all be devoid of annotations and short
                   if not allUnannotatedUntitledP then [untitledLinksSection] else
                     [RawBlock (Format "html") "<div class=\"columns\">\n\n",
                      untitledLinksSection,
                      RawBlock (Format "html") "</div>"])

  let document = Pandoc nullMeta body
  let p = runPure $ writeMarkdown def{writerExtensions = pandocExtensions} document

  case p of
    Left e   -> hPrint stderr e
    -- compare with the old version, and update if there are any differences:
    Right p' -> do let contentsNew = header ++ T.unpack p'
                   updateFile (dir'' ++ "index.page") contentsNew

updateFile :: FilePath -> String -> IO ()
updateFile f contentsNew = do t <- writeSystemTempFile "hakyll-directories" contentsNew
                              existsOld <- doesFileExist f
                              if not existsOld then
                                renameFile t f
                                else
                                  do contentsOld <- readFile f
                                     if contentsNew /= contentsOld then renameFile t f else removeFile t

generateYAMLHeader :: FilePath -> String
generateYAMLHeader d = "---\n" ++
                       "title: /" ++ d ++ " Directory Listing\n" ++
                       "description: Annotated bibliography of files in the directory <code>/" ++ d ++ "</code>, most recent first.\n" ++
                       "tags: index\n" ++
                       "created: 2009-01-01\n" ++
                       "status: in progress\n" ++
                       "confidence: log\n" ++
                       "importance: 0\n" ++
                       "cssExtension: drop-caps-de-zs\n" ++
                       "index: true\n" ++
                       "...\n" ++
                       "\n"

listDirectories :: [FilePath] -> IO [FilePath]
listDirectories direntries' = do
                       directories <- filterM (doesDirectoryExist . tail) direntries'
                       let directoriesMi = sort $ map (++"/index") directories
                       filterM (\f -> doesFileExist $ tail (f++".page")) directoriesMi

listFiles :: Metadata -> [FilePath] -> IO [(FilePath,MetadataItem,FilePath)]
listFiles m direntries' = do
                   files <- filterM (doesFileExist . tail) direntries'
                   let files'          = (sort . filter (not . ("index"`isSuffixOf`)) . map (replace ".page" "") . filter (not . isSuffixOf ".tar") ) files
                   backlinks <- mapM getBackLink files'
                   let fileAnnotationsMi = map (lookupFallback m) files'

                   return $
                     zipWith (\(a,b) c -> (a,b,c)) fileAnnotationsMi backlinks

-- Fetch URLs/file 'tagged' with the current directory but not residing in it.
--
-- tag-dirs are only in "docs/*", so "haskell/" etc is out. Tags drop the docs/ prefix, and we want to avoid
-- the actual files inside the current directory, because they'll be covered by the `listFiles` version, of course.
listTagged :: Metadata -> FilePath -> IO [(FilePath,MetadataItem,FilePath)]
-- listTags :: Metadata -> FilePath -> Metadata
listTagged m dir = if not ("docs/" `isPrefixOf` dir) then return [] else -- M.empty else --
                   let dirTag = replace "docs/" "" dir in
                     let tagged = M.filterWithKey (\u (_,_,_,_,ts,_) -> not (dir `isInfixOf` u) && dirTag `elem` ts) m in
                       do let files = M.keys tagged
                          backlinks <- mapM getBackLink files
                          let fileAnnotationsMi = map (lookupFallback m) files
                          return $
                            zipWith (\(a,b) c -> (a,b,c)) fileAnnotationsMi backlinks

-- sort a list of entries in ascending order using the annotation date when available (as 'YYYY[-MM[-DD]]', which string-sorts correctly), and falling back to sorting on the filenames ('YYYY-author.pdf').
-- We generally prefer to reverse this to descending order, to show newest-first.
sortByDate :: [(FilePath,MetadataItem,FilePath)] -> [(FilePath,MetadataItem,FilePath)]
sortByDate = sortBy (\(f,(_,_,d,_,_,_),_) (f',(_,_,d',_,_,_),_) -> if not (null d && null d') then (if d > d' then GT else LT) else (if f > f' then GT else LT))

-- how do we handle files with appended data, which are linked like '/docs/reinforcement-learning/2020-bellemare.pdf#google' but exist as files as '/docs/reinforcement-learning/2020-bellemare.pdf'? We can't just look up the *filename* because it's missing the # fragment, and the annotation is usually for the full path including the fragment. If a lookup fails, we fallback to looking for any annotation with the file as a *prefix*, and accept the first match.
lookupFallback :: Metadata -> String -> (FilePath, MetadataItem)
lookupFallback m u = case M.lookup u m of
                       Nothing -> tryPrefix
                       Just ("",_,_,_,_,_) -> tryPrefix
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

generateDirectoryItems :: FilePath -> [FilePath] -> Block
generateDirectoryItems parent ds = let parent' = T.pack $ takeDirectory parent in
                             BulletList
                              $ filter (not . null) $
                              [Para [Link nullAttr [Str "↑ Parent directory"] (T.pack parent, "Link to parent directory '" `T.append` parent' `T.append` "' (ascending)")]] :
                              map generateDirectoryItem ds
 where generateDirectoryItem :: FilePath -> [Block]
       generateDirectoryItem d = [Para [Link nullAttr [Code nullAttr (T.pack $ "↓ " ++ takeDirectory d)] (T.pack d, "")]]

generateListItems :: [(FilePath, MetadataItem,FilePath)] -> Block
generateListItems p = BulletList (map generateItem p)

generateSections :: [(FilePath, MetadataItem,FilePath)] -> [Block]
generateSections = concatMap (\p@(f,(t,aut,dt,_,_,_),_) ->
                                let sectionID = if aut=="" then "" else (generateID f aut dt) `T.append` "-section"
                                    authorShort = authorsToCite aut dt
                                in
                                 [Header 2 (sectionID, [], []) [RawInline (Format "html") (T.pack $ "“"++t++"”" ++ (if authorShort=="" then "" else ", " ++ authorsToCite aut dt))]]
                                 ++ generateItem p)


generateItem :: (FilePath,MetadataItem,FilePath) -> [Block]
generateItem (f,(t,aut,_,_,_,""),bl)  = let f' = if "http"`isPrefixOf`f then f else if "index" `isSuffixOf` f then takeDirectory f else takeFileName f
                                            author = if aut=="" then [] else [Str ",", Space, Str (T.pack aut)]
                                            -- I skip date because files don't usually have anything better than year, and that's already encoded in the filename which is shown
                                            backlink = if bl=="" then [] else [Space, Str "(",  Span ("", ["backlinks"], []) [Link ("",["backlink"],[]) [Str "backlinks"] (T.pack bl,"Reverse citations/backlinks for this page (the list of other pages which link to this URL).")], Str ")"]
                                        in
                                          if t=="" then
                                            [Para (Link nullAttr [Code nullAttr (T.pack f')] (T.pack f, "") : (author ++ backlink))]
                                          else
                                            [Para (Code nullAttr (T.pack f') : (Link nullAttr [Str ":", Space, Str "“", Str (T.pack t), Str "”"] (T.pack f, "")) : (author ++ backlink))]

generateItem (f,a,bl) =
  -- render annotation as: (skipping DOIs)
  --
  -- > [`2010-lucretius-dererumnatura.pdf`: "On The Nature of Things"](/docs/philosophy/2010-lucretius-dererumnatura.pdf), Lucretius (55BC-01-01):
  -- >
  -- > > A poem on the Epicurean model of the world...
  generateAnnotationBlock ("/"`isPrefixOf`f) (f,Just a) bl
