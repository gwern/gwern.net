{-# LANGUAGE OverloadedStrings #-}
module Annotation (linkDispatcher, tooltipToMetadata, htmlDownloadAndParseTitleClean) where

import Data.List (isPrefixOf, isInfixOf, intercalate)
import Text.Pandoc (Inline(Link))
import Network.HTTP (urlDecode)

import Annotation.Biorxiv (biorxiv)
import Annotation.Gwernnet (gwern)
import Annotation.PDF (pdf)
import Annotation.OpenReview (openreview)
import Annotation.Arxiv (arxiv)
import LinkMetadataTypes (Failure(..), MetadataItem, Path)
import MetadataFormat (trimTitle, cleanAbstractsHTML, pageNumberParse, filterMeta, linkCanonicalize, extractTwitterUsername)
import Utils (delete, replace, deleteMany, sed, anyInfix, anyPrefix, trim, printRed)
import Config.Misc as C (cd)

import Data.FileStore.Utils (runShellCommand)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import qualified Data.Text as T (unpack)
import System.Exit (ExitCode(ExitFailure))

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
wikipediaURLToTitle u = trimTitle $ cleanAbstractsHTML $ replace "#" " § " $ urlDecode $ replace "% " "%25 " $ replace "_" " " $ delete "https://en.wikipedia.org/wiki/" u

twitter :: Path -> IO (Either Failure (Path, MetadataItem))
twitter u = return $ Right (u, ("", extractTwitterUsername u, "", "", [], [], ""))

htmlDownloadAndParseTitle :: String -> IO String
htmlDownloadAndParseTitle url = do
 C.cd
 let args = ["static/build/download-title.sh", url]
 (status,stderr,mb) <- runShellCommand "./" Nothing "bash" args
 case status of
     ExitFailure err -> printRed ("Exit Failure: " ++ Data.List.intercalate " ::: " [url, show status, show err, show mb, show stderr]) >> return ""
     _ -> return $ U.toString mb

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
    badStrings = ["Quanta Magazine", "OSF", "CAIDA Resource Catalog", "Blogger", "Log in", "404 Not Found", "301 Moved Permanently", "Object moved", "302 Found", "WordPress \8250 Error", "Login \187 Qstream", "Kaggle Blog - Medium", "403 Forbidden", "500 Internal Server Error", "BBC NEWS Science &amp; Environment", "Welcome!", "Research", "niplav", "SL4: By Thread", "Moved Temporarily", "Redirecting\8230", "Torch", "crazymeds.us", "Site Not Found - Carnegie Mellon University School of Computer Science", "Documented Moved", "Notion \8211 The all-in-one workspace for your notes, tasks, wikis, and databases."] :: [String]
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
            , " - LessWrong", "A Neighborhood of Infinity: ", " by Jason Scott", " - <antirez>", " - Nick's Blog", " - &lt;antirez&gt;", " - Dwarf Fortress Wiki", " \8211 Armed and Dangerous", "Details Magazine: ", "(urth) ", " - Wishful Coding", "The Codeless Code:", "The Virtuosi: ", "Tom Waits - ", " \9733 The Scintillating But Ultimately Untrue Thought", " (Aaron Swartz: The Weblog)", " (Aaron Swartz's Raw Thought)", " - Inquiries Journal", "Locus Online: ", "Loper OS \187 ", "Mike On Ads  \187 Blog Archive   \187 ", " // Hayao Miyazaki Web", " - GhibliWiki", "\8212The Intercept", " American Political Science Review", "zlkj.in - ", " : zhanpei fang", "zenpundit.com  \187 Blog Archive   \187 ", "American Express\nApple Pay\nDiners Club\nDiscover\nMeta Pay\nGoogle Pay\nMastercard\nPayPal\nShop Pay\nVenmo\nVisa", " \8211 You Are Not So Smart", " - XTools", "xkcd: ", " \8211 xcorr: AI &amp; neuro"]
