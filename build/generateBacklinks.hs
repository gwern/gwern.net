#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Pandoc (def, nullMeta, pandocExtensions, queryWith, readerExtensions,
                     readHtml, readMarkdown, runPure, writeHtml5String,
                     Pandoc(Pandoc), Block(BulletList,Para), Inline(Link,Str))
import Text.Pandoc.Walk (walk)
import qualified Data.Text as T (append, isPrefixOf, isInfixOf, isSuffixOf, head, pack, unpack, tail, takeWhile, Text)
import qualified Data.Text.IO as TIO (readFile)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Map.Strict as M (lookup, keys, elems, traverseWithKey, fromListWith, union)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import Network.HTTP (urlDecode, urlEncode)
import Data.List.Utils (replace)
import Data.Containers.ListUtils (nubOrd)
import System.IO.Temp (writeSystemTempFile)
import Control.Monad (forM_, unless)

import Control.Monad.Parallel as Par (mapM)

import LinkMetadata (sed, hasAnnotation, readLinkMetadata, generateID, Metadata, readBacklinksDB, writeBacklinksDB, safeHtmlWriterOptions)

main :: IO ()
main = do
  bldb <- readBacklinksDB
  mdb <- readLinkMetadata
  createDirectoryIfMissing False "metadata/annotations/backlinks/"

  -- check that all backlink targets/callers are valid:
  let dotPageFy f = if '.' `elem` f then f else f++".page" -- all files have at least 1 period in them (for file extensions); a file missing periods must be a `.page` Markdown file, with the exception of tag pages which are auto-generated
  let filesCheck = map (dotPageFy . takeWhile (/='#') . tail) $ nubOrd $
        filter (\f -> not ("tags/"`isInfixOf` f || "/index"`isInfixOf` f || "/docs/link-bibliography/" `isPrefixOf` f)) $
        filter ("/"`isPrefixOf`) $ map T.unpack $
        M.keys bldb ++ concat (M.elems bldb)
  forM_ filesCheck (\f -> do exist <- doesFileExist f
                             unless exist $ error ("Backlinks: files annotation error: file does not exist? " ++ f))

  -- if all are valid, write out:
  _ <- M.traverseWithKey (writeOutCallers mdb) bldb

  fs <- fmap (filter (\f -> not $ ("/backlinks/"`isPrefixOf`f || "/docs/link-bibliography/"`isPrefixOf`f || "#"`isPrefixOf`f || ".#"`isPrefixOf`f)) .  map (sed "^\\.\\/" "")) $
         fmap lines getContents

  let markdown = filter (".page" `isSuffixOf`) fs
  links1 <- Par.mapM (parseFileForLinks True) markdown
  let html     = filter (".html" `isSuffixOf` ) fs
  links2 <- Par.mapM (parseFileForLinks False) html

  let linksdb = M.fromListWith (++) $ map (\(a,b) -> (a,[b])) $ nubOrd $ concat $ links1++links2
  let bldb' = linksdb `M.union` bldb
  writeBacklinksDB bldb'

writeOutCallers :: Metadata -> T.Text -> [T.Text] -> IO ()
writeOutCallers md target callers = do let f = take 274 $ "metadata/annotations/backlinks/" ++ urlEncode (T.unpack target) ++ ".html"
                                       -- guess at the anchor ID in the calling page, so the cross-page popup will pop up at the calling site,
                                       -- rather than merely popping up the entire page (and who knows *where* in it the reverse citation is).
                                       -- (NOTE: This will fail if the default generated link ID has been overridden to disambiguate, unfortunately, and
                                       -- it'll just pop up the page as a whole. It would be difficult to rewrite the schema and preserve all
                                       -- variant overrides...)
                                       let ident = case M.lookup (T.unpack target) md of
                                                             Nothing -> ""
                                                             Just (_,aut,dt,_,_,_) -> let i = generateID (T.unpack target) aut dt in
                                                                                      if i=="" then "" else "#" `T.append` i
                                       let callerTitles = map (\u -> case M.lookup (T.unpack u) md of
                                                                      Nothing -> if T.head u == '/' then T.tail u else u
                                                                      Just ("",_,_,_,_,_) -> if T.head u == '/' then T.tail u else u
                                                                      Just (t,_,_,_,_,_) -> T.pack $ "“"++t++"”")
                                                          callers
                                       let callerClasses = map (\u -> if T.head u == '/' && not ("." `T.isInfixOf` u) then ["link-local"] else ["docMetadata"]) callers
                                       let callers' = zip3 callers callerClasses callerTitles
                                       let content = BulletList $
                                            map (\(u,c,t) -> [Para [Link ("", c, [])
                                                                  [Str t]
                                                                  (u`T.append`ident, "")]
                                                   ]
                                                ) callers'

                                       let pandoc = walk (hasAnnotation md True) $ Pandoc nullMeta [content]
                                       let html = let htmlEither = runPure $ writeHtml5String safeHtmlWriterOptions pandoc
                                                  in case htmlEither of
                                                              Left e -> error $ show target ++ show callers ++ show e
                                                              Right output -> output
                                       let backLinksHtmlFragment = "<div class=\"columns\">\n" ++ T.unpack html ++ "\n</div>"
                                       updateFile f backLinksHtmlFragment
                                       -- HACK: write out a duplicate 'metadata/annotations/backlinks/foo.html.html' file to provide a 'syntax-highlighted' version that the popups fallback will render as proper HTML
                                       -- We overload the syntax-highlighting feature to make backlinks *partially* work (doesn't enable full suite of features like recursive popups); right now, when popups.js tries to load the backlinks `$PAGE.html`, it treats it as a raw source code file, and tries to fetch the *syntax-highlighted* version, `$PAGE.html.html` (which doesn't exist & thus errors out). But what if... we claimed the original HTML *was* the 'syntax-highlighted (HTML) version'? Then wouldn't popups.js then render it as HTML, and accidentally Just Work?
                                       updateFile (f++".html") $ T.unpack html

updateFile :: FilePath -> String -> IO ()
updateFile f contentsNew = do t <- writeSystemTempFile "hakyll-backlinks" contentsNew
                              existsOld <- doesFileExist f
                              if not existsOld then
                                renameFile t f
                                else
                                  do contentsOld <- Prelude.readFile f
                                     if contentsNew /= contentsOld then renameFile t f else removeFile t

parseFileForLinks :: Bool -> FilePath -> IO [(T.Text,T.Text)]
parseFileForLinks md m = do text <- TIO.readFile m

                            let links = map truncateAnchors $ filter blackList $ filter (\l -> let l' = T.head l in l' == '/' || l' == 'h') $ -- filter out non-URLs
                                         extractLinks md text

                            let caller = T.pack $ (\u -> if head u /= '/' && take 4 u /= "http" then "/"++u else u) $ replace "metadata/annotations/" "" $ replace "https://www.gwern.net/" "/" $ replace ".page" "" $ sed "^metadata/annotations/(.*)\\.html$" "\\1" $ urlDecode m
                            if not (blackList caller) then return [] else
                             do
                                let called = filter (/= caller) (map (T.pack . replace "/metadata/annotations/" "" . replace "https://www.gwern.net/" "/"  . (\l -> if "/metadata/annotations"`isPrefixOf`l then urlDecode $ replace "/metadata/annotations" "" l else l) . T.unpack) links)
                                return $ zip called (repeat caller)

-- for URLs like 'arxiv.org/123#google' or 'docs/reinforcement-learning/2021-foo.pdf#deepmind', we want to preserve anchors; for on-site pages like '/GPT-3#prompt-programming' we want to merge all such anchor links into just callers of '/GPT-3'
truncateAnchors :: T.Text -> T.Text
truncateAnchors str = if "." `T.isInfixOf` str then str else T.takeWhile (/='#') str

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
  | any (`T.isInfixOf` f) ["/link-bibliography/"] = False
  | any (`T.isPrefixOf` f) ["/images", "/tags/", "/docs/www/", "/newsletter/", "/Changelog", "/Mistakes", "/Traffic", "/Links", "/Lorem",
                            -- WARNING: do not filter out 'metadata/annotations' because that leads to empty databases & infinite loops
                            "https://wwwyoutube.com/", "https://en.wikipedia.org/wiki/",
                            "https://www.dropbox.com/", "https://dl.dropboxusercontent.com/"] = False
  | any (`T.isSuffixOf` f) ["/index"] = False
  | otherwise = True
