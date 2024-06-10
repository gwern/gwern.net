{-# LANGUAGE OverloadedStrings #-}
module Annotation (linkDispatcher, tooltipToMetadata) where

import Data.List (isPrefixOf, isInfixOf)
import Text.Pandoc (Inline(Link))
import Network.HTTP (urlDecode)

import Annotation.Biorxiv (biorxiv)
import Annotation.Gwernnet (gwern)
import Annotation.PDF (pdf)
import Annotation.OpenReview (openreview)
import Annotation.Arxiv (arxiv)
import LinkMetadataTypes (Failure(..), MetadataItem, Path)
import MetadataFormat (trimTitle, cleanAbstractsHTML, pageNumberParse, filterMeta, linkCanonicalize, extractTwitterUsername)
import Utils (replace, deleteMany, sed, anyInfix, anyPrefix, trim)

import Network.HTTP.Conduit (simpleHttp, HttpException)
import Text.HTML.TagSoup (innerText, sections, parseTags, (~/=), (~==))
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Data.Text.Lazy.Encoding as TLE (decodeUtf8)
import qualified Data.Text as T (strip, unpack, Text)
import Control.Exception (catch)

-- 'new link' handler: if we have never seen a URL before (because it's not in the metadata database), we attempt to parse it or call out to external sources to get metadata on it, and hopefully a complete annotation.
linkDispatcher :: Inline -> IO (Either Failure (Path, MetadataItem))
linkDispatcher (Link _ _ (l, tooltip)) = do l' <- linkDispatcherURL (T.unpack l)
                                            case l' of
                                              -- apply global per-field rewrites here
                                              Right (l'',(title,author,date,dc,kvs,tags,abstract)) -> return $ Right (l'',(reformatTitle title,author,date,dc,kvs,tags,abstract))
                                              Left Permanent -> let (title,author,date) = tooltipToMetadata (T.unpack l) (T.unpack tooltip) in
                                                                  if title/="" then return (Right (T.unpack l,(reformatTitle title,author,date,"",[],[],""))) else return l'
                                              Left Temporary -> return l'
  where reformatTitle = replace " - " "—" -- NOTE: we cannot simply put this in `typesetHtmlField`/`cleanAbstractsHTML` because while a space-separated hyphen in a *title* is almost always an em-dash, in an *abstract*, it often is meant to be an en-dash or a minus sign instead. So if we want to clean those up across all titles, we have to confine it to title fields only.
linkDispatcher x = error ("Annotation.linkDispatcher passed a non-Link Inline element: " ++ show x)

linkDispatcherURL :: Path -> IO (Either Failure (Path, MetadataItem))
linkDispatcherURL l | anyPrefix l ["/metadata/annotation/backlink/", "/metadata/annotation/similar/", "/doc/www/"] = return (Left Permanent)
                 -- WP is now handled by annotations.js calling the Mobile WP API; we pretty up the title for tags.
                 | "https://en.wikipedia.org/wiki/" `isPrefixOf` l = return $ Right (l, (wikipediaURLToTitle l, "", "", "", [], [], ""))
                 | "arxiv.org/abs/" `isInfixOf` l || "browse.arxiv.org/html/" `isInfixOf` l = arxiv l
                 | "https://openreview.net/forum?id=" `isPrefixOf` l || "https://openreview.net/pdf?id=" `isPrefixOf` l = openreview l
                 | anyPrefix l ["https://www.biorxiv.org/content/", "https://www.medrxiv.org/content/"] = biorxiv l
                 | "https://x.com" `isPrefixOf` l = twitter l
                 | null l = return (Left Permanent)
                 -- locally-hosted PDF?
                 | ".pdf" `isInfixOf` l = let l' = linkCanonicalize l in if head l' == '/' then pdf $ tail l' else return (Left Permanent)
                 | otherwise = let l' = linkCanonicalize l in if head l' == '/' then gwern $ tail l
                 -- And everything else is unhandled:
                    else do title <- htmlDownloadAndParseTitleClean l
                            if title /= "" then return (Right (l, (title, "", "", "", [], [], "")))
                              else return (Left Permanent)

---

-- Attempt to parse tooltips back into citation metadata:
tooltipToMetadata :: String -> String -> (String,String,String)
tooltipToMetadata _ "" = ("","","")
tooltipToMetadata path s
                    | head s `elem` ['/', '!', '$', '\8383'] || anyInfix s ["Original URL:"] = ("","","")
                    | otherwise =
                        let title  = filterMeta $ sed "['‘“](.*)['’”], .*" "\\1" s
                            author = filterMeta $  sed "^Branwen$" "Gwern Branwen" $ replace " et al" ", et al" $ replace " & " ", " $
                                     sed "^([A-Z].*) [0-9][0-9][0-9][0-9][0-9-]*[a-z]?$" "\\1" $
                                     sed "['‘“].*['’”], ([A-Z].+) [0-9][0-9][0-9][0-9][0-9-]*[a-z]?" "\\1" s
                            date   = filterMeta $ sed "^[A-Za-z].+ ([0-9][0-9][0-9][0-9][0-9-]*)[a-z]?$" "\\1" $ sed "['‘“].+['’”], [A-Za-z].+ ([0-9][0-9][0-9][0-9][0-9-]*)[a-z]?$" "\\1" s
                            pageNumber = let n = pageNumberParse path in if null n then "" else " § pg" ++ n
                            (t,a,d) = (minLength 6 (changed title) ++ pageNumber, changed author, minLength 4 $ changed date) in
                          if a==d then (s,"","") else (t,a,d)
                    where changed x = if s==x then "" else x
                          minLength n x = if length x < n then "" else x

wikipediaURLToTitle :: String -> String
wikipediaURLToTitle u = trimTitle $ cleanAbstractsHTML $ replace "#" " § " $ urlDecode $ replace "% " "%25 " $ replace "_" " " $ replace "https://en.wikipedia.org/wiki/" "" u

twitter :: Path -> IO (Either Failure (Path, MetadataItem))
twitter u = return $ Right (u, ("", extractTwitterUsername u, "", "", [], [], ""))


-- Fetch HTML content from a URL
fetchHTML :: String -> IO (Either HttpException T.Text)
fetchHTML url = do
  result <- (Right <$> simpleHttp url) `catch` (return . Left)
  return $ case result of
    Left ex -> Left ex
    Right response -> Right $ TL.toStrict $ TLE.decodeUtf8 response

-- Extract the title from the HTML content
extractTitle :: T.Text -> T.Text
extractTitle html =
  let tags = parseTags html
      titleTags = sections (~== ("<title>" :: String)) tags
  in if null titleTags
     then ""
     else T.strip $ innerText $ takeWhile (~/= ("</title>" :: String)) $ drop 1 $ head titleTags

htmlDownloadAndParseTitle :: String -> IO String
htmlDownloadAndParseTitle url = do
  htmlResult <- fetchHTML url
  return $ case htmlResult of
             Left _ -> ""
             Right html -> T.unpack $ extractTitle html

htmlDownloadAndParseTitleClean :: String -> IO String
htmlDownloadAndParseTitleClean u = do
  title <- htmlDownloadAndParseTitle u
  let title' = trim $ clean $ if any (`elem` separators) title
                              then reverse $ tail $ dropWhile (`notElem` separators) $ reverse title
                              else title
  if title' `elem` badStrings
  then return ""
  else return title'
  where
    separators = "—·|" :: String
    badStrings = ["Quanta Magazine", "OSF", "CAIDA Resource Catalog"] :: [String]
    clean = deleteMany [" - The Public Domain Review"
            , "Github - "
            , " - The New York Times"
            , " |"
            , " - YouTube"
            , "—Bulletin of the Atomic Scientists"
            , " MIT News"
            , " \\\\ Anthropic"
            , " – Brendan Eich"
            , "—ACM Queue"
            , " American Political Science Review"
            , " @ Things Of Interest"
            , " - LessWrong"]
