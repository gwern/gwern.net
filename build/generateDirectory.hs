{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Read directories like "docs/iodine/" for its files; generate a list item with the abstract in a blockquote where available; the full list is then turned into a directory listing, but, because of the flattened-annotation pass in hakyll.hs, it has a 'Link Bibliography' which provides an automatically-annotated directory interface! Very nifty. Much nicer than simply browsing a list of filenames or even the Google search of a directory (mostly showing random snippets).

import Control.Monad (filterM)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.List.Utils (replace)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist, renameFile, removeFile)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, takeFileName)
import Text.Pandoc (def, nullAttr, nullMeta, pandocExtensions, runPure, writeMarkdown, writerExtensions,
                    Block(BlockQuote, BulletList, RawBlock, Para), Inline(Code, Link, Str, Space, RawInline), Format(..), Pandoc(Pandoc))
import qualified Data.Map as M (lookup, size, toList, filterWithKey)
import qualified Data.Text as T (unpack, pack)
import System.IO (stderr, hPrint)
import System.IO.Temp (writeSystemTempFile)

import LinkMetadata (parseRawBlock, readLinkMetadata, Metadata, MetadataItem)

main :: IO ()
main = do dirs <- getArgs
          let dirs' = map (\dir -> if "./" `isPrefixOf` dir then drop 2 dir else dir) dirs

          meta <- readLinkMetadata

          mapM_ (generateDirectory meta) dirs'

generateDirectory :: Metadata -> FilePath -> IO ()
generateDirectory mta dir'' = do
  pairs <- listFiles mta dir''

  let header = generateYAMLHeader dir''
  let body = [BulletList (map generateListItems pairs)]
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
                                     if (contentsNew /= contentsOld) then renameFile t f else removeFile t


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
                       "\n" ++
                       "# Files\n" ++
                       "\n"

listFiles :: Metadata -> FilePath -> IO [(FilePath,MetadataItem)]
listFiles m d = do direntries <- listDirectory d
                   let direntries' = map (\entry -> "/"++d++entry) direntries

                   directories <- filterM (doesDirectoryExist . tail) direntries'
                   let directoriesMi = zip (sort $ map (++"/index") directories) (repeat ("","","","",""))

                   files <- filterM (doesFileExist . tail) direntries'
                   let files'          = (sort . map (replace ".page" "") . filter (not . isSuffixOf ".tar") .  filter (/=("/"++d++"index.page"))) files
                   let fileAnnotationsMi = map (lookupFallback m) files'

                   return $ directoriesMi ++ fileAnnotationsMi

-- how do we handle files with appended data, like '/docs/rl/2020-bellemare.pdf#google'? We can't just look up the *filename* because it's missing the # fragment, and the annotation is usually for the full path including the fragment. If a lookup fails, we fallback to looking for any annotation with the file as a *prefix*, and accept the first match.
lookupFallback :: Metadata -> String -> (FilePath, MetadataItem)
lookupFallback m u = case M.lookup u m of
                       Nothing -> tryPrefix
                       Just ("","","","","") -> tryPrefix
                       Just mi -> (u,mi)
                       where tryPrefix = let possibles =  M.filterWithKey (\url _ -> u `isPrefixOf` url && url /= u) m in
                                           let u' = if M.size possibles > 0 then fst $ head $ M.toList possibles else u in
                                             if ".page" `isSuffixOf` u' then (u, ("","","","","")) else if u==u' then (u, ("","","","","")) else lookupFallback m u'

generateListItems :: (FilePath,MetadataItem) -> [Block]
generateListItems (f,("",_,_,_,_))  = let f' = if "index" `isSuffixOf` f then takeDirectory f else takeFileName f in
                         [Para [Link nullAttr [Code nullAttr (T.pack f')] (T.pack f, "")]]
generateListItems (f,(tle,aut,dt,_,abst)) =
  -- render annotation as: (skipping DOIs)
  --
  -- > [`2010-lucretius-dererumnatura.pdf`: "On The Nature of Things"](/docs/philo/2010-lucretius-dererumnatura.pdf), Lucretius (55BC-01-01):
  -- >
  -- > > A poem on the Epicurean model of the world...
  [Para [Link nullAttr [
            Code nullAttr (T.pack $ takeFileName f), Str ":", Space,
            RawInline (Format "html") (T.pack $ "“"++tle++"”")] (T.pack f,""),  Str ",", Space,
         Str (T.pack aut), Space,
         Str (T.pack $ "("++dt++")"), Str ":"],
   BlockQuote [parseRawBlock $ RawBlock (Format "html") (T.pack abst)]
  ]
