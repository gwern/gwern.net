#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Pandoc -- (def, queryWith, readerExtensions, readMarkdown, runPure,
                     -- pandocExtensions, Inline(Link), Pandoc)
import qualified Data.Text as T -- (append,  pack, unlines, Text)
import qualified Data.Text.IO as TIO -- (readFile, putStr)
import System.Environment -- (getArgs)
import Data.List
import qualified Data.HashMap.Strict as HM
import System.Directory
import Network.HTTP (urlEncode)
import Data.List.Utils (replace)

-- | Map over the filenames
main :: IO ()
main = do
  bldb <- readBacklinksDB
  createDirectoryIfMissing False "metadata/annotations/backlinks/"

  _ <- HM.traverseWithKey writeOutCallers bldb

  fs <- fmap lines getContents

  let markdown = filter (".page" `isSuffixOf`) fs
  links1 <- mapM (parseFileForLinks True) markdown
  let html     = filter (".html" `isSuffixOf` ) fs
  links2 <- mapM (parseFileForLinks False) html

  let linksdb = HM.fromListWith (++) $ map (\(a,b) -> (a,[b])) $ concat $ links1++links2
  let bldb' = bldb `HM.union` linksdb
  writeBacklinksDB bldb'

writeOutCallers :: T.Text -> [T.Text] -> IO ()
writeOutCallers target callers = do let f = take 274 $ "metadata/annotations/backlinks/" ++ urlEncode (T.unpack target) ++ ".html"
                                    let content = BulletList $
                                          map (\c -> [Para [Link nullAttr [Str c] (c, "")]]) callers
                                    let html = let htmlEither = runPure $  writeHtml5String def $ Pandoc nullMeta [content]
                                                  in case htmlEither of
                                                              Left e -> error $ (show target) ++ (show callers) ++ (show e)
                                                              Right output -> output
                                    TIO.writeFile f html

-- -- | read 1 file and print out its URLs
-- printURLs :: Bool -> FilePath -> IO ()
-- printURLs printfilename file = do
--   input <- TIO.readFile file
--   let converted = extractLinks input
--   if printfilename then TIO.putStr $ T.unlines $ Prelude.map (\url -> (T.pack file) `T.append` ":" `T.append` url) converted else
--      TIO.putStr $ T.unlines converted

parseFileForLinks :: Bool -> FilePath -> IO [(T.Text,T.Text)]
parseFileForLinks md m = do text <- TIO.readFile m
                            let links = extractLinks md text
                            return $ zip links (repeat $ T.pack $ (replace ".page" "" $ replace ".html" "" ("/"++m)))

type Backlinks = HM.HashMap T.Text [T.Text]

readBacklinksDB :: IO Backlinks
readBacklinksDB = do bll <- readFile "metadata/backlinks.hs"
                     let bldb = HM.fromList $ map (\(a,b) -> (T.pack a, map T.pack b)) (read bll :: [(String,[String])])
                     return bldb
writeBacklinksDB :: Backlinks -> IO ()
writeBacklinksDB bldb = do let bll = HM.toList bldb :: [(T.Text,[T.Text])]
                           let bll' = map (\(a,b) -> (T.unpack a, map T.unpack b)) bll
                           writeFile "metadata/backlinks.hs" $ show bll'

-- | Read one Text string and return its URLs (as Strings)
extractLinks :: Bool -> T.Text -> [T.Text]
extractLinks md txt = let parsedEither = if md then runPure $ readMarkdown def{readerExtensions = pandocExtensions } txt
                                         else runPure $ readHtml def{readerExtensions = pandocExtensions } txt
                   in case parsedEither of
                              Left _ -> []
                              Right links -> extractURLs links

-- | Read 1 Pandoc AST and return its URLs as Strings
extractURLs :: Pandoc -> [T.Text]
extractURLs = queryWith extractURL
 where
   extractURL :: Inline -> [T.Text]
   extractURL (Link _ _ (u,_)) = [u]
   extractURL _ = []
