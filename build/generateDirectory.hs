{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Read directories like "docs/iodine/" for its files; generate a list item with the abstract in a blockquote where available; the full list is then turned into a directory listing, which gets compiled with Hakyll and gets the usual popup annotations. Very nifty. Much nicer than simply browsing a list of filenames or even the Google search of a directory (mostly showing random snippets).

import Control.Monad (filterM)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.List.Utils (replace)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist, renameFile, removeFile)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, takeFileName)
import Text.Pandoc (def, nullAttr, nullMeta, pandocExtensions, runPure, writeMarkdown, writerExtensions,
                    Block(BlockQuote, BulletList, Header, RawBlock, Para), Inline(Code, Link, Str, Space, RawInline), Format(..), Pandoc(Pandoc))
import qualified Data.Map as M (lookup, size, toList, filterWithKey)
import qualified Data.Text as T (unpack, pack)
import System.IO (stderr, hPrint)
import System.IO.Temp (writeSystemTempFile)

import LinkMetadata (parseRawBlock, readLinkMetadata, Metadata, MetadataItem)

main :: IO ()
main = do dirs <- getArgs
          let dirs' = map (\dir -> if "./" `isPrefixOf` dir then drop 2 dir else dir) dirs

          meta <- readLinkMetadata

          mapM_ (generateDirectory meta) dirs'  -- NOTE: while embarrassingly-parallel & trivial to switch to `Control.Monad.Parallel.mapM-`, because of the immensely slow Haskell compilation (due to Pandoc), 2021-04-23 benchmarking suggests that compile+runtime is ~1min slower than `runhaskell` interpretation

generateDirectory :: Metadata -> FilePath -> IO ()
generateDirectory mta dir'' = do
  direntries <- listDirectory dir''
  let direntries' = map (\entry -> "/"++dir''++entry) direntries

  dirs  <- listDirectories direntries'
  pairs <- listFiles mta   direntries'

  let header = generateYAMLHeader dir''
  let directorySection = generateDirectoryItems dirs

  let fileSection = generateListItems pairs
  let body = [Header 2 nullAttr [Str "Directories"], directorySection, Header 2 nullAttr [Str "Files"], fileSection]

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
                       "description: Annotated bibliography of files in the directory <code>/" ++ d ++ "</code>.\n" ++
                       "tags: index\n" ++
                       "created: 2009-01-01\n" ++
                       "status: in progress\n" ++
                       "confidence: log\n" ++
                       "importance: 0\n" ++
                       "cssExtension: drop-caps-de-zs\n" ++
                       "...\n" ++
                       "\n"

listDirectories :: [FilePath] -> IO [FilePath]
listDirectories direntries' = do
                       directories <- filterM (doesDirectoryExist . tail) direntries'
                       let directoriesMi = sort $ map (++"/index") directories
                       filterM (\f -> doesFileExist $ tail (f++".page")) directoriesMi

listFiles :: Metadata -> [FilePath] -> IO [(FilePath,MetadataItem)]
listFiles m direntries' = do
                   files <- filterM (doesFileExist . tail) direntries'
                   let files'          = (sort . filter (not . ("index"`isSuffixOf`)) . map (replace ".page" "") . filter (not . isSuffixOf ".tar") ) files
                   let fileAnnotationsMi = map (lookupFallback m) files'
                   return fileAnnotationsMi

-- how do we handle files with appended data, like '/docs/rl/2020-bellemare.pdf#google'? We can't just look up the *filename* because it's missing the # fragment, and the annotation is usually for the full path including the fragment. If a lookup fails, we fallback to looking for any annotation with the file as a *prefix*, and accept the first match.
lookupFallback :: Metadata -> String -> (FilePath, MetadataItem)
lookupFallback m u = case M.lookup u m of
                       Nothing -> tryPrefix
                       Just ("","","","","") -> tryPrefix
                       Just mi -> (u,mi)
                       where tryPrefix = let possibles =  M.filterWithKey (\url _ -> u `isPrefixOf` url && url /= u) m in
                                           let u' = if M.size possibles > 0 then fst $ head $ M.toList possibles else u in
                                               (if (".page" `isSuffixOf` u') || (u == u') then
                                                  (u, ("", "", "", "", "")) else lookupFallback m u')

generateDirectoryItems :: [FilePath] -> Block
generateDirectoryItems ds = BulletList
                              $ filter (not . null) $
                              [Para [Link nullAttr [Str "↑ Parent directory"] ("../index", "Link to parent directory (ascending)")]] :
                              map generateDirectoryItem ds
 where generateDirectoryItem :: FilePath -> [Block]
       generateDirectoryItem d = [Para [Link nullAttr [Code nullAttr (T.pack $ "↓ " ++ takeDirectory d)] (T.pack d, "")]]

generateListItems :: [(FilePath, MetadataItem)] -> Block
generateListItems p = BulletList (map generateListItem p)
generateListItem :: (FilePath,MetadataItem) -> [Block]
generateListItem (f,("",_,_,_,_))  = let f' = if "index" `isSuffixOf` f then takeDirectory f else takeFileName f in
                         [Para [Link nullAttr [Code nullAttr (T.pack f')] (T.pack f, "")]]
generateListItem (f,(tle,aut,dt,_,abst)) =
  -- render annotation as: (skipping DOIs)
  --
  -- > [`2010-lucretius-dererumnatura.pdf`: "On The Nature of Things"](/docs/philo/2010-lucretius-dererumnatura.pdf), Lucretius (55BC-01-01):
  -- >
  -- > > A poem on the Epicurean model of the world...
  [Para [Link nullAttr [
            Code nullAttr (T.pack $ takeFileName f), Str ":", Space,
            RawInline (Format "html") (T.pack $ "“"++tle++"”")] (T.pack f,""),  Str ",", Space,
         Str (T.pack aut), Space,
         Str (T.pack $ "("++dt++")"), if null abst then Str "" else Str ":"]] ++
  if null abst then [] else
    [BlockQuote [parseRawBlock $ RawBlock (Format "html") (T.pack abst)]
  ]
