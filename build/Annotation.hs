module Annotation (linkDispatcher, tooltipToMetadata) where

import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T (unpack)
import Text.Pandoc (Inline(Link))
import Network.HTTP (urlDecode)

import Annotation.Biorxiv (biorxiv)
import Annotation.Gwernnet (gwern)
import Annotation.PDF (pdf)
import Annotation.OpenReview (openreview)
import Annotation.Arxiv (arxiv)
import LinkMetadataTypes (Failure(..), MetadataItem, Path)
import MetadataFormat (trimTitle, cleanAbstractsHTML, pageNumberParse, filterMeta, linkCanonicalize, extractTwitterUsername)
import Utils (replace, sed, anyInfix, anyPrefix)

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
linkDispatcher x = error ("linkDispatcher passed a non-Link Inline element: " ++ show x)

linkDispatcherURL :: Path -> IO (Either Failure (Path, MetadataItem))
linkDispatcherURL l | anyPrefix l ["/metadata/annotation/backlink/", "/metadata/annotation/similar/", "/doc/www/"] = return (Left Permanent)
                 -- WP is now handled by annotations.js calling the Mobile WP API; we pretty up the title for tags.
                 | "https://en.wikipedia.org/wiki/" `isPrefixOf` l = return $ Right (l, (wikipediaURLToTitle l, "", "", "", [], [], ""))
                 | "arxiv.org/abs/" `isInfixOf` l || "browse.arxiv.org/html/" `isInfixOf` l = arxiv l
                 | "https://openreview.net/forum?id=" `isPrefixOf` l || "https://openreview.net/pdf?id=" `isPrefixOf` l = openreview l
                 | anyPrefix l ["https://www.biorxiv.org/content/", "https://www.medrxiv.org/content/"] = biorxiv l
                 | "https://x.com" `isPrefixOf` l || "https://twitter.com/" `isPrefixOf` l = twitter l
                 | null l = return (Left Permanent)
                 -- locally-hosted PDF?
                 | ".pdf" `isInfixOf` l = let l' = linkCanonicalize l in if head l' == '/' then pdf $ tail l' else return (Left Permanent)
                 | otherwise = let l' = linkCanonicalize l in if head l' == '/' then gwern $ tail l
                 -- And everything else is unhandled:
                    else return (Left Permanent)


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
