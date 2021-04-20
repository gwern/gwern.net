#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Pandoc (def, nullAttr, nullMeta, pandocExtensions, queryWith, readerExtensions,
                     readHtml, readMarkdown, runPure, writeHtml5, writerExtensions, writerTemplate, compileTemplate, runWithDefaultPartials,
                     Pandoc(Pandoc), Block(BulletList,Para), Inline(Link,Str), Template)
import qualified Data.Text as T (head, pack, unpack, Text)
import qualified Data.Text.IO as TIO (readFile)
import Data.List (isSuffixOf, sort)
import qualified Data.HashMap.Strict as HM (toList, fromList, traverseWithKey, fromListWith, union, HashMap)
import System.Directory (createDirectoryIfMissing)
import Network.HTTP (urlEncode)
import Data.List.Utils (replace)
import Data.Containers.ListUtils (nubOrd)
import Text.Show.Pretty (ppShow)
import Text.Blaze.Html.Renderer.String (renderHtml)
import System.IO.Unsafe (unsafePerformIO)

-- | Map over the filenames
main :: IO ()
main = do
  bldb <- readBacklinksDB
  createDirectoryIfMissing False "metadata/annotations/backlinks/"

  let t = backTemplate
  _ <- HM.traverseWithKey (writeOutCallers t) bldb

  fs <- fmap lines getContents

  let markdown = filter (".page" `isSuffixOf`) fs
  links1 <- mapM (parseFileForLinks True) markdown
  let html     = filter (".html" `isSuffixOf` ) fs
  links2 <- mapM (parseFileForLinks False) html

  let linksdb = HM.fromListWith (++) $ map (\(a,b) -> (a,[b])) $ nubOrd $ concat $ links1++links2
  let bldb' = bldb `HM.union` linksdb
  writeBacklinksDB bldb'

writeOutCallers :: Template T.Text -> T.Text -> [T.Text] -> IO ()
writeOutCallers tp target callers = do let f = take 274 $ "metadata/annotations/backlinks/" ++ urlEncode (T.unpack target) ++ ".html"
                                       let content = BulletList $
                                            map (\c -> [Para [Link nullAttr [Str c] (c, "")]]) callers

                                       let html = let htmlEither = runPure $ writeHtml5 def{writerExtensions = pandocExtensions, writerTemplate=Just tp} $ Pandoc nullMeta [content]
                                                  in case htmlEither of
                                                              Left e -> error $ show target ++ show callers ++ show e
                                                              Right output -> output
                                       writeFile f $ renderHtml html

backTemplate :: Template T.Text
backTemplate =
          either error id $ either (error . show) id $
        runPure $ runWithDefaultPartials $
        compileTemplate "" (unsafePerformIO $ TIO.readFile "/home/gwern/bin/bin/pandoc-template-html5-articleedit.html5") -- &%&^%!

parseFileForLinks :: Bool -> FilePath -> IO [(T.Text,T.Text)]
parseFileForLinks md m = do text <- TIO.readFile m
                            let links = filter (\l -> let l' = T.head l in l' == '/' || l' == 'h') $ -- filter out non-URLs
                                  extractLinks md text
                            return $ zip links
                                         (repeat $ T.pack $ replace "https://www.gwern.net/" "/" $ replace ".page" "" $ replace ".html" "" ("/"++m))

type Backlinks = HM.HashMap T.Text [T.Text]

readBacklinksDB :: IO Backlinks
readBacklinksDB = do bll <- readFile "metadata/backlinks.hs"
                     let bldb = HM.fromList $ map (\(a,b) -> (T.pack a, map T.pack b)) (read bll :: [(String,[String])])
                     return bldb
writeBacklinksDB :: Backlinks -> IO ()
writeBacklinksDB bldb = do let bll = HM.toList bldb :: [(T.Text,[T.Text])]
                           let bll' = sort $ map (\(a,b) -> (T.unpack a, map T.unpack b)) bll
                           writeFile "metadata/backlinks.hs" $ ppShow bll'

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
