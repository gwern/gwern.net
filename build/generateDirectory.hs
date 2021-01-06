{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Read directories like "docs/iodine/" for its files; generate a list item with the abstract in a blockquote where available; the full list is then turned into a directory listing, but, because of the flattened-annotation pass in hakyll.hs, it has a 'Link Bibliography' which provides an automatically-annotated directory interface! Very nifty. Much nicer than simply browsing a list of filenames or even the Google search of a directory (mostly showing random snippets).

import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Time (getCurrentTime)
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.FilePath (takeFileName)
import Text.Pandoc (def, nullAttr, nullMeta, pandocExtensions, runPure, writeMarkdown, writerExtensions,
                    Block(BlockQuote, BulletList, RawBlock, Para), Format(..), Inline(Space, Str, Code, Link, RawInline), Pandoc(Pandoc))
import qualified Data.Map as M (lookup, size, toList, filterWithKey)
import qualified Data.Text as T (unpack, pack)
import System.IO (stderr, hPrint)

import LinkMetadata (readLinkMetadata, Metadata, MetadataItem)

main :: IO ()
main = do dirs <- getArgs
          let dirs' = map (\dir -> if "./" `isPrefixOf` dir then drop 2 dir else dir) dirs

          today <- fmap (take 10 . show) Data.Time.getCurrentTime
          meta <- readLinkMetadata

          mapM_ (generateDirectory meta today) dirs'

generateDirectory :: Metadata -> String -> FilePath -> IO ()
generateDirectory mta tdy dir'' = do
  pairs <- listFiles mta dir''

  let header = generateYAMLHeader dir'' tdy
  let body = [BulletList (map (generateListItems) pairs)]
  let document = Pandoc nullMeta body
  let p = runPure $ writeMarkdown def{writerExtensions = pandocExtensions} document

  case p of
    Left e   -> hPrint stderr e
    Right p' -> writeFile (dir'' ++ "index.page") $ header ++ (T.unpack p') ++ generateYAMLFooter

generateYAMLHeader :: FilePath -> String -> String
generateYAMLHeader d tdy = "---\n" ++
                           "title: /" ++ d ++ " Directory Listing\n" ++
                           "description: Annotated bibliography of files in the directory <code>/" ++ d ++ "</code>.\n" ++
                           "tags: meta\n" ++
                           "created: 2009-01-01\n" ++
                           "modified: " ++ tdy ++ "\n" ++
                           "status: in progress\n" ++
                           "confidence: log\n" ++
                           "importance: 0\n" ++
                           "cssExtension: directory-index\n" ++
                           "...\n" ++
                           "\n" ++
                           "<div class=\"abstract\">\n" ++
                           "> <code>/" ++ d ++ "</code> directory contents (<a href=\"#link-bibliography\">with annotations</a> where available):\n" ++
                           "</div>\n" ++
                           "\n" ++
                           "# Files {.collapse}\n" ++
                           "\n" ++
                           "<div class=\"columns\">" ++
                           "\n"
generateYAMLFooter :: String
generateYAMLFooter = "</div>"

listFiles :: Metadata -> FilePath -> IO [FilePath]
listFiles m d = do files <- listDirectory d
                   let files'          = map (\f -> "/"++d++f) $ (sort . filter (not . isSuffixOf ".tar") .  filter (/="index.page")) files
                   let fileAnnotations = map (lookupFallback m) files'
                   return fileAnnotations

-- how do we handle files with appended data, like '/docs/rl/2020-bellemare.pdf#google'? We can't just look up the *filename* because it's missing the # fragment, and the annotation is usually for the full path including the fragment. If a lookup fails, we fallback to looking for any annotation with the file as a *prefix*, and accept the first match.
lookupFallback :: Metadata -> String -> FilePath
lookupFallback m u = case M.lookup u m of
                       Nothing -> tryPrefix
                       Just ("","","","","") -> tryPrefix
                       Just i -> u
                       where tryPrefix = let possibles =  M.filterWithKey (\url _ -> u `isPrefixOf` url && url /= u) m in
                                           if M.size possibles > 0 then fst $ head $ M.toList possibles else u

generateListItems :: FilePath -> [Block]
generateListItems f  = [Para [Link nullAttr [Code nullAttr (T.pack $ takeFileName f)] (T.pack f, "")]]
