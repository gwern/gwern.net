{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Read a directory like "docs/iodine/" for its files, and look up each file as a link in the link annotation database of `metadata/*.yaml`; generate a list item with the abstract in a blockquote where available; the full list is then turned into a directory listing, but an automatically-annotated one! Very nifty. Much nicer than simply browsing a list of filenames or even the Google search of a directory (mostly showing random snippets).

import Data.List (isPrefixOf)
import Data.Time (getCurrentTime)
import LinkMetadata (readLinkMetadata, Metadata, MetadataItem)
import Text.Pandoc (def, nullAttr, nullMeta, pandocExtensions, runPure, writeMarkdown, writerExtensions,
                     Block(BlockQuote, BulletList, RawBlock, Para), Format(..), Inline(Space, Str, Code, Link), Pandoc(Pandoc))
import System.Directory (listDirectory)
import System.Environment (getArgs)
import qualified Data.Text as T (unpack, pack)
import qualified Data.Map as M (lookup)
import System.FilePath (takeFileName)

main :: IO ()
main = do dir <- fmap head getArgs
          let dir' = if "./" `isPrefixOf` dir then drop 2 dir else dir

          today <- fmap (take 10 . show) Data.Time.getCurrentTime
          let header = generateYAMLHeader dir' today

          meta <- readLinkMetadata
          pairs <- listFiles meta dir'

          let body = [BulletList (map generateListItems pairs)]
          let document = Pandoc nullMeta body
          let p = runPure $ writeMarkdown def{writerExtensions = pandocExtensions} document
          case p of
            Left e   -> print e
            Right p' -> putStrLn $ header ++ (T.unpack p')

generateYAMLHeader :: FilePath -> String -> String
generateYAMLHeader d tdy = "---\n" ++
                           "title: '<code>" ++ d ++ "</code> Directory Listing'\n" ++
                           "description: Annotated bibliography of files in the directory " ++ d ++ ".\n" ++
                           "tags: meta\n" ++
                           "created: 2009-01-01\n" ++
                           "modified: " ++ tdy ++ "\n" ++
                           "status: in progress\n" ++
                           "confidence: log\n" ++
                           "importance: 0\n" ++
                           "...\n" ++
                           "\n" ++
                           "List of directory contents (with annotations where available):\n" ++
                           "\n"

listFiles :: Metadata -> FilePath -> IO [(FilePath, Maybe LinkMetadata.MetadataItem)]
listFiles m d = do files <- listDirectory d
                   let files'          = map (\f -> "/"++d++f) files
                   let fileAnnotations = map (`M.lookup` m) files'
                   return $ zip files' fileAnnotations

generateListItems :: (FilePath, Maybe LinkMetadata.MetadataItem) -> [Block]
generateListItems (f, ann) = case ann of
                              Nothing -> nonAnnotatedLink
                              Just ("",   _, _,_ ,_) -> nonAnnotatedLink
                              Just (tle,aut,dt,_,abst) -> [Para [Link nullAttr [Str (T.pack $ "“"++tle++"”")] (T.pack f,""),
                                                                  Str ",", Space, Str (T.pack $ aut++", "), Str (T.pack $ "("++dt++")"), Str ":"],
                                                          BlockQuote [RawBlock (Format "html") (T.pack abst)]
                                                    ]
                             where
                               nonAnnotatedLink :: [Block]
                               nonAnnotatedLink = [Para [Link nullAttr [Code nullAttr (T.pack $ takeFileName f)] (T.pack f, "")]]
