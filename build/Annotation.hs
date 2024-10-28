{-# LANGUAGE OverloadedStrings #-}
module Annotation (linkDispatcher, tooltipToMetadata, htmlDownloadAndParseTitleClean) where

import Data.List (isPrefixOf, isInfixOf)
import Text.Pandoc (Inline(Link))

import Annotation.Biorxiv (biorxiv)
import Annotation.Gwernnet (gwern)
import Annotation.PDF (pdf)
import Annotation.OpenReview (openreview)
import Annotation.Arxiv (arxiv)
import LinkMetadataTypes (Failure(..), MetadataItem, Path, Metadata)
import Metadata.Format (linkCanonicalize)
import Metadata.Date (guessDateFromString)
import Metadata.Author (extractTwitterUsername)
import Metadata.Title (tooltipToMetadata, wikipediaURLToTitle, htmlDownloadAndParseTitleClean)
import Typography (typesetHtmlFieldPermanent)
import Utils (replace, anyPrefix)
import Config.Misc as C (todayDayString)
import qualified Data.Text as T (unpack)

-- 'new link' handler: if we have never seen a URL before (because it's not in the metadata database), we attempt to parse it or call out to external sources to get metadata on it, and hopefully a complete annotation.
linkDispatcher :: Metadata -> Inline -> IO (Either Failure (Path, MetadataItem))
linkDispatcher md (Link _ _ (l, tooltip)) =
 do let l' = linkCanonicalize $ T.unpack l
    mi <- linkDispatcherURL md l'

    today <- todayDayString
    let defaultCreatedToToday d = if null d then today else d

    case mi of
      -- apply global per-field rewrites here
      Right (l'',(title,author,dateRaw,dc,kvs,tags,abstract)) ->
        do date <- if dateRaw /= "" then return dateRaw else guessDateFromString (title ++ " : " ++ l'')
           return $ Right (l'',(reformatTitle title,author,date,defaultCreatedToToday dc,kvs,tags,abstract))
      Left Permanent -> do let (title,author,date') = tooltipToMetadata l' (T.unpack tooltip)
                           if title/="" then return (Right (l',(reformatTitle title,author,date',defaultCreatedToToday "",[],[],""))) else return mi
      Left Temporary -> return mi
  where reformatTitle = typesetHtmlFieldPermanent False . replace " – " "—" . replace " - " "—" -- NOTE: we cannot simply put this in `typesetHtmlField`/`cleanAbstractsHTML` because while a space-separated hyphen in a *title* is almost always an em-dash, in an *abstract*, it often is meant to be an en-dash or a minus sign instead. So if we want to clean those up across all titles, we have to confine it to title fields only.
linkDispatcher _ x = error ("Annotation.linkDispatcher passed a non-Link Inline element: " ++ show x)

linkDispatcherURL :: Metadata -> Path -> IO (Either Failure (Path, MetadataItem))
linkDispatcherURL md l
  | anyPrefix l ["/metadata/annotation/backlink/", "/metadata/annotation/similar/", "/doc/www/"] =
      return (Left Permanent)
  -- WP is now handled by annotations.js calling the Mobile WP API; we pretty up the title for tags.
  | "https://en.wikipedia.org/wiki/" `isPrefixOf` l = return $ Right (l, (wikipediaURLToTitle l, "", "", "", [], [], ""))
  | "arxiv.org/abs/" `isInfixOf` l || "browse.arxiv.org/html/" `isInfixOf` l = arxiv md l
  | "https://openreview.net/forum?id=" `isPrefixOf` l || "https://openreview.net/pdf?id=" `isPrefixOf` l =
      openreview md l
  | anyPrefix l ["https://www.biorxiv.org/content/", "https://www.medrxiv.org/content/"] = biorxiv md l
  | "https://x.com/" `isPrefixOf` l = twitter l
  | null l = return (Left Permanent)
  -- locally-hosted PDF?
  | ".pdf" `isInfixOf` l = let l' = linkCanonicalize l in if head l' == '/' then pdf md l' else return (Left Permanent)
  | otherwise = let l' = linkCanonicalize l in if head l' == '/' then gwern md l
  -- And everything else is unhandled:
     else do title <- htmlDownloadAndParseTitleClean l
             if title /= "" then return (Right (l, (title, "", "", "", [], [], "")))
               else return (Left Permanent)

--- currently can only extract Twitter username due to difficulty scraping/parsing Twitter
twitter :: Path -> IO (Either Failure (Path, MetadataItem))
twitter u = return $ Right (u, ("", extractTwitterUsername u, "", "", [], [], ""))
