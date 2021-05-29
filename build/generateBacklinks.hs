#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Pandoc (def, nullAttr, nullMeta, pandocExtensions, queryWith, readerExtensions,
                     readHtml, readMarkdown, runPure, writeHtml5String, writerExtensions,
                     Pandoc(Pandoc), Block(BulletList,Para), Inline(Link,Str))
import Text.Pandoc.Walk (walk)
import qualified Data.Text as T (append, isPrefixOf, isInfixOf, isSuffixOf, head, pack, unpack, tail, Text)
import qualified Data.Text.IO as TIO (readFile)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort)
import qualified Data.HashMap.Strict as HM (empty, keys, elems, toList, fromList, traverseWithKey, fromListWith, union, HashMap)
import qualified Data.Map.Strict as M (lookup)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import Network.HTTP (urlDecode, urlEncode)
import Data.List.Utils (replace)
import Data.Containers.ListUtils (nubOrd)
import Text.Show.Pretty (ppShow)
import System.IO.Temp (writeSystemTempFile)
import Control.Monad (forM_, unless)

import LinkMetadata (sed, hasAnnotation, readLinkMetadata, generateID, Metadata)

main :: IO ()
main = do
  bldb <- readBacklinksDB
  mdb <- readLinkMetadata
  createDirectoryIfMissing False "metadata/annotations/backlinks/"

  -- check that all backlink targets/callers are valid:
  let dotPageFy f = if '.' `elem` f then f else f++".page" -- all files have at least 1 period in them (for file extensions); a file missing periods must be a `.page` Markdown file, with the exception of tag pages which are auto-generated
  let files = map (dotPageFy . takeWhile (/='#') . tail) $ nubOrd $ filter (not . ("tags/"`isInfixOf`)) $ filter ("/"`isPrefixOf`) $ map T.unpack $ HM.keys bldb ++ concat (HM.elems bldb)
  forM_ files (\f -> do exist <- doesFileExist f
                        unless exist $ error ("Custom annotation error: file does not exist? " ++ f))

  -- if all are valid, write out:
  _ <- HM.traverseWithKey (writeOutCallers mdb) bldb

  fs <- fmap (filter (\f -> not $ ("/backlinks/"`isPrefixOf`f || "#"`isPrefixOf`f || ".#"`isPrefixOf`f)) . map (sed "^\\.\\/" "")) $ fmap lines getContents

  let markdown = filter (".page" `isSuffixOf`) fs
  links1 <- mapM (parseFileForLinks True) markdown -- NOTE: while embarrassingly-parallel & trivial to switch to `Control.Monad.Parallel.mapM`, because of the immensely slow Haskell compilation (due to Pandoc), 2021-04-23 benchmarking suggests that compile+runtime is ~2min slower than `runhaskell` interpretation
  let html     = filter (".html" `isSuffixOf` ) fs
  links2 <- mapM (parseFileForLinks False) html

  let linksdb = HM.fromListWith (++) $ map (\(a,b) -> (a,[b])) $ nubOrd $ concat $ links1++links2
  let bldb' = bldb `HM.union` linksdb
  writeBacklinksDB bldb'

writeOutCallers :: Metadata -> T.Text -> [T.Text] -> IO ()
writeOutCallers md target callers = do let f = take 274 $ "metadata/annotations/backlinks/" ++ urlEncode (T.unpack target) ++ ".html"
                                       -- guess at the anchor ID in the calling page, so the cross-page popup will pop up at the calling site,
                                       -- rather than merely popping up the entire page (and who knows *where* in it the reverse citation is).
                                       -- (NOTE: This will fail if the default generated link ID has been overriden to disambiguate, unfortunately, and
                                       -- it'll just pop up the page as a whole. It would be difficult to rewrite the schema and preserve all
                                       -- variant overrides...)
                                       let ident = case M.lookup (T.unpack target) md of
                                                             Nothing -> ""
                                                             Just (_,aut,dt,_,_) -> let i = generateID (T.unpack target) aut dt in
                                                                                      if i=="" then "" else "#" `T.append` i
                                       let content = BulletList $
                                            map (\c -> [Para [Link nullAttr
                                                                  [Str (if T.head c == '/' then T.tail c else c)]
                                                                  (c`T.append`ident, "")]
                                                   ]
                                                ) callers

                                       let pandoc = walk (hasAnnotation md True) $ Pandoc nullMeta [content]
                                       let html = let htmlEither = runPure $ writeHtml5String def{writerExtensions = pandocExtensions} pandoc
                                                  in case htmlEither of
                                                              Left e -> error $ show target ++ show callers ++ show e
                                                              Right output -> output
                                       updateFile f $ T.unpack html

updateFile :: FilePath -> String -> IO ()
updateFile f contentsNew = do t <- writeSystemTempFile "hakyll-backlinks" contentsNew
                              existsOld <- doesFileExist f
                              if not existsOld then
                                renameFile t f
                                else
                                  do contentsOld <- readFile f
                                     if contentsNew /= contentsOld then renameFile t f else removeFile t


parseFileForLinks :: Bool -> FilePath -> IO [(T.Text,T.Text)]
parseFileForLinks md m = do text <- TIO.readFile m
                            let links = filter blackList $ filter (\l -> let l' = T.head l in l' == '/' || l' == 'h') $ -- filter out non-URLs
                                  extractLinks md text

                            let caller = filter blackList $ repeat $ T.pack $ (\u -> if head u /= '/' && take 4 u /= "http" then "/"++u else u) $ replace "metadata/annotations/" "" $ replace "https://www.gwern.net/" "/" $ replace ".page" "" $ sed "^metadata/annotations/(.*)\\.html$" "\\1" $ urlDecode m
                            let called = filter (/= head caller) (map (T.pack . takeWhile (/='#') . replace "/metadata/annotations/" "" . replace "https://www.gwern.net/" "/"  . (\l -> if "/metadata/annotations"`isPrefixOf`l then urlDecode $ replace "/metadata/annotations" "" l else l) . T.unpack) links)
                            return $ zip called caller

type Backlinks = HM.HashMap T.Text [T.Text]

readBacklinksDB :: IO Backlinks
readBacklinksDB = do bll <- readFile "metadata/backlinks.hs"
                     if bll=="" then return HM.empty else
                       let bldb = HM.fromList $ map (\(a,b) -> (T.pack a, map T.pack b)) (read bll :: [(String,[String])]) in
                         return bldb
writeBacklinksDB :: Backlinks -> IO ()
writeBacklinksDB bldb = do let bll = HM.toList bldb :: [(T.Text,[T.Text])]
                           let bll' = sort $ map (\(a,b) -> (T.unpack a, sort $ map T.unpack b)) bll
                           t <- writeSystemTempFile "hakyll-backlinks" $ ppShow bll'
                           renameFile t "metadata/backlinks.hs"

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

blackList :: T.Text -> Bool
blackList f
  | any (`T.isInfixOf` f) ["/backlinks/"] = False
  | any (`T.isPrefixOf` f) ["/images", "/tags/", "/docs/www/",
                            -- WARNING: do not filter out 'metadata/annotations' because that leads to empty databases & infinite loops
                            "https://wwwyoutube.com/", "https://en.wikipedia.org/wiki/",
                            "https://www.dropbox.com/", "https://dl.dropboxusercontent.com/"] = False
  | any (`T.isSuffixOf` f) ["/index"] = False
  | otherwise = True
