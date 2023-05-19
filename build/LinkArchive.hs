{- LinkArchive.hs: module for generating Pandoc external links which are rewritten to a local static
                   mirror which cannot break or linkrot—if something's worth linking, it's worth hosting!
Author: Gwern Branwen
Date: 2019-11-20
When:  Time-stamp: "2023-05-18 09:47:10 gwern"
License: CC-0
Dependencies: pandoc, filestore, tld, pretty; runtime: SingleFile CLI extension, Chromium, wget, etc (see `linkArchive.sh`)
-}

{- Local Mirror design: <https://gwern.net/archiving#preemptive-local-archiving>

Because link rot has proven difficult to keep up with on `gwern.net` using [ordinary reactive link
archiving methods](https://gwern.net/archiving), I am switching to *pre-emptive archiving*:
for most external links on Gwern.net, they will now point to a local (stored on Gwern.net) mirror of
the original external link as the default. As the cost of disk/bandwidth falls while the value of
human attention increases, if something is worth linking, it is worth hosting—assuming good tooling.
The local mirror will be a self-contained static HTML copy which cannot linkrot (unless Gwern.net
itself goes down in which case the issue is largely moot). The local mirrors will, aside from being
infinitely more reliable, also be faster for the reader to load & browse, as they will be loaded
from the current domain's CDN and are the final DOMs saved using adblock etc (benefiting the large
fraction of Gwern.net readers who do not have ad blockers installed).

The implementation strategy is, similar to the [link
popups](https://gwern.net/static/build/LinkMetadata.hs), a Pandoc Hakyll plugin which at
Markdown → HTML compile-time traverses the Markdown AST for non-whitelisted external links, looks
for a local mirror of each one, create a local mirror via SingleFile
(https://github.com/gildas-lormeau/SingleFile/) if necessary, and rewrites the link to point to the
local mirror.

Details:

- at compile-time, [`hakyll.hs`](https://gwern.net/static/build/hakyll.hs) reads the local
  database, and feeds it into a `localizeLink` function which walks the Pandoc AST and processes all
  external links
- mirror metadata is stored in a local database (a Haskell association list read in as a
  [`Data.Map`](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html) for now,
  like the popup previews) with the schema: `(URL, MIRROR_FILE, FIRST_SEEN, MIRRORED_SUCCESSFULLY)`
- links are checked against a whitelist of domains to exclude, where mirroring is either unnecessary
  or undesirable: arxiv.org, Wikipedia, Gwern.net, lesswrong.com, HN, Nature, Youtube etc. (Primary
  exclusion reasons: services, interactive pages, pages which are too hard to snapshot, pages which
  are too large due to inline media, domains which are stable & not at linkrot risk, pages which are
  inherently updated frequently & archiving is not helpful.)
- links are only mirrored _n_ days after first being seen, giving them time to reach a finalized
  form (eg. blog post discussions); if there is no entry in the database, one is made with the
  current date as `FIRST_SEEN` and the link is skipped; if an entry with `FIRST_SEEN` exists and the
  current date is ≥ _n_ days later and `MIRROR_FILE` does not exist, then unless `Failed` is false
  (indicating the previous mirror attempt failed and it's probably a permanently broken link which
  must be updated manually), it is mirrored and rewritten, otherwise, just rewritten

    - only one mirror is made per Hakyll run; because links are processed independently in parallel,
      this is enforced by passing around an IORef. Simply downloading *all* links turns out to have
      a lot of undesired consequences: bulk-adds of PDFs will trigger anti-bot defenses & corrupt PDFs
      requiring extremely labor-intensive checks & repairs; and manual review of a dozen simultaneous
      HTML snapshots can come at unwelcome, stressed-out times (which is _muri_).
- mirrors are made using SingleFile, which runs Chromium to serialize a DOM

    - PDFs are just downloaded unconditionally because they are so easy to archive
    - Why SingleFile instead of Chromium's screenshot mode, which is built-in & easy to use? Because
      Chromium screenshot mode is deliberately crippled and will never support extensions or
      cookies. Why do I want mirroring to be done with extensions & cookies? I want extensions
      because if I don't use uBlock, the mirrored versions will be stuffed full of malware, tracking
      code, and ads; and if I don't use a 'kill sticky' extension, thousands of mirrored pages will
      have giant "Please click here to accept cookies / GDPR / sign up for our newsletter /
      ill-thought-through floating-headers" obscuring the entire page. (I won't use NoScript because
      in whitelist mode, NoScript breaks a large fraction of all websites currently, and going
      through and adding the necessary whitelists is infeasible; I have to rely on uBlock to get rid
      of bad JS.) And I want cookies because this allows cookies for logins (eg. subreddits marked
      NSFW like the darknet market ones) rather than archiving useless login wall pages.
- mirrors are saved to `wiki/doc/www/$DOMAIN($URL)/SHA1($URL).html`; the additional nesting by
  domain name is necessary to allow CSS link annotations to match rewritten links

    - `/doc/www/` is excluded from the `sitemap.xml` & `sync-gwern.net` compile-time checks,
      blocked in `robots.txt`, `rel="canonical"` is set in the mirrors by SingleFile,
      additional`rel="archive nofollow"` attributes are set on each link when used in Gwern.net, and
      noarchive/noindex/nofollow/nocache are set as HTTP headers by nginx for `/doc/www/`; exposing
      thousands of mirrors to the rest of the Internet risks causing SEO issues, high bandwidth
      consumption, and legal or social problems
- links are rewritten to point the `href` to `wiki/doc/www/$DOMAIN($URL)/SHA1($URL).html`; like
  inflation-adjustments or link popups, the original `href` is stored as a span (eg. `<a
  href="https://foo.com">foo</a>` → `<a rel="archive nofollow" data-url-original="https://foo.com"
  href="/doc/www/foo.com/cf934d97a8012ba1c2d354d6cd39e77535fd0fb9.html">foo</a></span>`)
- the `data-url-original` metadata is used by `popups.js` to add to link popups a '[original]'
  hyperlink (using the JS templating, something like `<p>…"Title" [<a
  href="${target.dataset.urlOriginal}" title="Original (live) Internet version of
  ${target.dataset.popupTitle}">original</a>]…</p>`)

There are approximately 30k external links on Gwern.net as of October 2019, of which perhaps 5k need
to be mirrored; I estimate this will take up somewhere on the order of ~50GB and add a few dollars
to S3 hosting costs. (After exclusions, my archive was 5,300 links (excluding optional PDFs) / 20GB
in February 2020. It has grown to 14,352 / 47G as of 2023-03-09.) But it'll be worth it to forestall
thousands of dying links, regular reader frustration, and a considerable waste of my time every month
dealing with the latest broken links. -}

{-# LANGUAGE OverloadedStrings #-}
module LinkArchive (C.archivePerRunN, localizeLink, readArchiveMetadata, ArchiveMetadata) where

import Control.Monad (filterM, unless)
import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M (fromList, insert, lookup, toAscList, Map)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isNothing, fromMaybe)
import Text.Read (readMaybe)
import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Text as T (pack, unpack)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Posix.Files (getFileStatus, fileSize)
import Data.FileStore.Utils (runShellCommand)
import Network.URI.TLD (parseTLD)
import Text.Pandoc (Inline(Link))
import Text.Show.Pretty (ppShow)
import Data.ByteString.Base16 (encode) -- base16-bytestring
import Crypto.Hash.SHA1 (hash) -- cryptohash
import Data.ByteString.Char8 (pack, unpack)
import System.FilePath (takeFileName)
import System.Directory (doesFileExist)

import Utils (writeUpdatedFile, printGreen, printRed, addClass, currentDay)
import qualified Config.LinkArchive as C (whiteList, transformURLsForArchiving, transformURLsForLinking, archivePerRunN, archiveDelay, isCheapArchive)

type ArchiveMetadataItem = Either
  Integer -- Age: first seen date -- ModifiedJulianDay, eg. 2019-11-22 = 58810
  (Maybe FilePath) -- Our archive of the URL: local archive path (if successful, otherwise, should be skipped - already dead?)
type ArchiveMetadataList= [(Path, -- URL: original raw URL of an external link
                            ArchiveMetadataItem)] -- date/path
type ArchiveMetadata = M.Map Path ArchiveMetadataItem
type Path = String

-- Pandoc types: Link = Link Attr [Inline] Target; Attr = (String, [String], [(String, String)]); Target = (String, String)
localizeLink :: ArchiveMetadata -> IORef Integer -> Inline -> IO Inline
localizeLink adb archivedN x@(Link (identifier, classes, pairs) b (targetURL, targetDescription)) =
  -- skip local archiving if matches the whitelist, or it has a manual annotation '.archive-not' class on it, like
  -- `[Foo](!W "Bar"){.archive-not}` in which case we don't do any sort of 'archiving' such as rewriting to point to a
  -- local link (or possibly, in the future, rewriting WP links to point to the historical revision ID when first
  -- linked, to avoid deletionist content rot)
  if C.whiteList (T.unpack targetURL) || "archive-not" `elem` classes then return x else
    do targetURL' <- rewriteLink adb archivedN $ T.unpack targetURL
       if targetURL' == T.unpack targetURL then return x -- no archiving has been done yet, return original
       else do -- rewrite & annotate link with local archive:
         let padding = if targetDescription == "" then "" else " "
         let targetDescription' = T.unpack targetDescription ++ padding ++ "(Original URL: " ++ T.unpack targetURL ++ " )"
         -- specify that the rewritten links are mirrors & to be ignored:
         let archiveAttributes = [("rel", "archived alternate nofollow"), ("data-url-original", T.pack (C.transformURLsForLinking (T.unpack targetURL)))]
         let archivedLink = addClass "archive-local" $ Link (identifier, classes, pairs++archiveAttributes) b (T.pack ('/':targetURL'), T.pack targetDescription')
         return archivedLink
localizeLink _ _ x = return x

readArchiveMetadata :: IO ArchiveMetadata
readArchiveMetadata = do pdlString <- (fmap T.unpack $ TIO.readFile "metadata/archive.hs") :: IO String
                         case (readMaybe pdlString :: Maybe ArchiveMetadataList) of
                           Nothing -> error $ "Failed to read metadata/archive.hs. Contents of string: " ++ pdlString
                           Just pdl -> do
                            -- check for failed archives:
                            pdl' <- filterM (\(p,ami) -> case ami of
                                     Right (Just "") -> printRed "Error! Invalid empty archive link: " >> print (show p ++ show ami) >> return False
                                     Right u@(Just ('/':'/':_)) -> printRed "Error! Invalid double-slash archive link: " >> print (show p ++ show ami ++ show u) >> return False
                                     Right (Just u)  -> if not ("http" `isPrefixOf` p || "\n" `isInfixOf` p) then
                                                          printRed "Warning: Did a local link slip in somehow? (this will be removed automatically) " >> print (show p ++ show u ++ show ami) >> return False
                                                        else
                                                          if isNothing (parseTLD p) then
                                                           printRed "Error! Invalid URI link in archive? " >> print (show p ++ show u ++ show ami) >> return False
                                                          else do let filepath = takeWhile (/='#') u
                                                                  exists <- doesFileExist filepath
                                                                  unless exists $ error ("Archive file not found: " ++ filepath ++ " (original path in archive.hs: " ++ u ++ "; original tuple: " ++ show (p,ami) ++ ")")
                                                                  size <- getFileStatus filepath >>= \s -> return $ fileSize s
                                                                  if size == 0 then
                                                                    printRed "Error! Empty archive file. Not using: " >> print (show p ++ show u ++ show ami) >> return False
                                                                    else if size > 1024 then return True else return False
                                     Right Nothing   -> return True
                                     Left  _         -> return True)
                                 pdl
                            let pdl'' = filter (\(p,_) -> "http" `isPrefixOf` p && not (C.whiteList p)) pdl'
                            -- for mismatches, we know they were archived before, so we should archive them ASAP:
                            let pdl''' = map (\(p,ami) ->  if checksumIsValid p ami then (p,ami) else (p, Left 0)) pdl''
                            return $ M.fromList pdl'''

-- When we rewrite links to fix link rot, archive.hs can become stale: it records a failed archive of the old URL, and doesn't know there's a new URL because archive.hs was rewritten with the rest of Gwern.net. But since the checksum is deterministically derived from the URL, the checksum of the URL will no longer match the checksum encoded in the file name. So when there is a mismatch, we can drop that entry, deleting it, and now the new URL will get picked up as a fresh URL entered into the archive queue.
checksumIsValid :: Path -> ArchiveMetadataItem -> Bool
checksumIsValid _ (Left _) = True
checksumIsValid _ (Right Nothing) = True
checksumIsValid url (Right (Just file)) = let derivedChecksum = Data.ByteString.Char8.unpack $ Data.ByteString.Base16.encode $ Crypto.Hash.SHA1.hash $ Data.ByteString.Char8.pack (C.transformURLsForArchiving (takeWhile (/='#') url) ++ "\n")
                                              storedChecksum = takeWhile (/= '.') $ takeFileName file
                                          in derivedChecksum == storedChecksum

-- rewriteLink:
-- 1. Exit on whitelisted URLs.
-- 2. Access what we know about the URL, defaulting to "First I've heard of it.".
-- 3. If we've never attempted to archive it and have known the url _n_ days, do so. (With the exception of PDFs, which we
--    locally archive immediately.) Do only 1 archive per run by checking the IORef to see
-- 4. Return archive contents.
rewriteLink :: ArchiveMetadata -> IORef Integer -> String -> IO String
rewriteLink adb archivedN url = do
  today <- currentDay
  fromMaybe url <$> if C.whiteList url then return Nothing else
    case M.lookup url adb of
      Nothing               -> Nothing <$ insertLinkIntoDB (Left today) url
      Just (Left firstSeen) -> let cheapArchive = C.isCheapArchive url

       in if ((today - firstSeen) < C.archiveDelay) && not cheapArchive
          then return Nothing
          else do
                   let url' = C.transformURLsForArchiving url
                   archivedP <- archiveURLCheck url'
                   if archivedP then do archive <- archiveURL url'
                                        insertLinkIntoDB (Right archive) url
                                        return archive
                   else do archivedNAlreadyP <- readIORef archivedN
                           -- have we already used up our link archive 'budget' this run? If so, skip all additional link archives
                           if archivedNAlreadyP < 1 then return Nothing
                           else  do archive <- archiveURL url'
                                    insertLinkIntoDB (Right archive) url
                                    unless cheapArchive $ writeIORef archivedN (archivedNAlreadyP - 1)
                                    return archive
      Just (Right archive) -> if archive == Just "" then printRed "Error! Tried to return a link to a non-existent archive! " >> print url >> return Nothing else return archive

insertLinkIntoDB :: ArchiveMetadataItem -> String -> IO ()
insertLinkIntoDB a url = do adb <- readArchiveMetadata
                            let adb' = M.insert url a adb
                            writeUpdatedFile "archive-metadata-auto.db.hs" "metadata/archive.hs" (T.pack $ ppShow $ M.toAscList adb')

-- has a URL been archived already (ie. does a hashed file exist?)
archiveURLCheck :: String -> IO Bool
archiveURLCheck l = do (exit,stderr',stdout) <- runShellCommand "./" Nothing "linkArchive.sh" [l, "--check"]
                       case exit of
                         ExitSuccess -> return $ stdout /= ""
                         ExitFailure _ -> printRed (l ++ " : archiving script existence-check failed to run correctly: ") >> print (U.toString stderr') >> return False

-- take a URL, archive it, and if successful return the hashed path
archiveURL :: String -> IO (Maybe Path)
archiveURL l = do let args = if C.isCheapArchive l then [l] else [l, "--no-preview"]
                  (exit,stderr',stdout) <- runShellCommand "./" Nothing "linkArchive.sh" args
                  case exit of
                     ExitSuccess -> do let result = U.toString stdout
                                       printGreen ( "Archiving (LinkArchive.hs): " ++ l ++ " returned: " ++ result)
                                       if result == "" then return Nothing else return $ Just result
                     ExitFailure _ -> printRed (l ++ " : archiving script failed to run correctly: ") >> print (U.toString stderr') >> return Nothing
