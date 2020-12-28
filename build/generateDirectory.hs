{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Read directories like "docs/iodine/" for its files, and look up each file as a link in the link annotation database of `metadata/*.yaml`; generate a list item with the abstract in a blockquote where available; the full list is then turned into a directory listing, but an automatically-annotated one! Very nifty. Much nicer than simply browsing a list of filenames or even the Google search of a directory (mostly showing random snippets).

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

import LinkMetadata (readLinkMetadataOnce, Metadata, MetadataItem)

main :: IO ()
main = do dirs <- getArgs
          let dirs' = map (\dir -> if "./" `isPrefixOf` dir then drop 2 dir else dir) dirs

          today <- fmap (take 10 . show) Data.Time.getCurrentTime
          meta <- readLinkMetadataOnce

          mapM_ (generateDirectory meta today) dirs'

generateDirectory :: Metadata -> String -> FilePath -> IO ()
generateDirectory mta tdy dir'' = do
  pairs <- listFiles mta dir''

  let header = generateYAMLHeader dir'' tdy
  let body = [BulletList (map generateListItems pairs)]
  let document = Pandoc nullMeta body
  let p = runPure $ writeMarkdown def{writerExtensions = pandocExtensions} document

  case p of
    Left e   -> hPrint stderr e
    Right p' -> writeFile (dir'' ++ "index.page") $ header ++ (T.unpack p')

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
                           "> <code>/" ++ d ++ "</code> directory contents (with annotations where available):\n" ++
                           "</div>\n" ++
                           "\n" ++
                           "# Files\n" ++
                           "\n"

listFiles :: Metadata -> FilePath -> IO [(FilePath, Maybe LinkMetadata.MetadataItem)]
listFiles m d = do files <- listDirectory d
                   let files'          = map (\f -> "/"++d++f) $ (sort . filter (not . isSuffixOf ".tar") .  filter (/="index.page")) files
                   let fileAnnotations = map (lookupFallback m) files'
                   return fileAnnotations

-- how do we handle files with appended data, like '/docs/rl/2020-bellemare.pdf#google'? We can't just look up the *filename* because it's missing the # fragment, and the annotation is usually for the full path including the fragment. If a lookup fails, we fallback to looking for any annotation with the file as a *prefix*, and accept the first match.
lookupFallback :: Metadata -> String -> (FilePath, Maybe LinkMetadata.MetadataItem)
lookupFallback m u = case M.lookup u m of
                       Nothing -> tryPrefix
                       Just ("","","","","") -> tryPrefix
                       Just i -> (u, Just i)
                       where tryPrefix = let possibles =  M.filterWithKey (\url _ -> u `isPrefixOf` url && url /= u) m in
                                           if M.size possibles > 0 then (fst $ head $ M.toList possibles, Just $ snd $ head $ M.toList possibles) else
                                             (u, Nothing)
generateListItems :: (FilePath, Maybe LinkMetadata.MetadataItem) -> [Block]
generateListItems (f, ann) = case ann of
                              Nothing -> nonAnnotatedLink
                              Just ("",   _, _,_ ,_) -> nonAnnotatedLink
                              Just (tle,aut,dt,_,abst) -> [Para [Link nullAttr [RawInline (Format "html") (T.pack $ "“"++tle++"”")] (T.pack f,""),
                                                                  Str ",", Space, Str (T.pack aut), Space, Str (T.pack $ "("++dt++")"), Str ":"],
                                                           BlockQuote [RawBlock (Format "html") (T.pack abst)]
                                                           ]
                             where
                               nonAnnotatedLink :: [Block]
                               nonAnnotatedLink = [Para [Link nullAttr [Code nullAttr (T.pack $ takeFileName f)] (T.pack f, "")]]
