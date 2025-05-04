{- LinkArchive.hs: module for generating Pandoc external links which are rewritten to a local static
                   mirror which cannot break or linkrot—if something's worth linking, it's worth hosting!
Author: Gwern Branwen
Date: 2019-11-20
When:  Time-stamp: "2025-05-04 17:01:23 gwern"
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

    - Only a few URLs should be mirrored each time. Simply downloading *all* links turns out to have
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

  WARNING: the details here are in flux, as we try to eliminate linkrot/external links by linking
  local archives where they exist, but we also want to avoid users copy-pasting those URLs rather
  than the original, canonical URLs. (The local archive URLs are fragile and cause other problems;
  for example, they make my writing a lot harder as I can no longer search using a URL I right-click
  because all of the references in Markdown or annotations will be to the original URL, not the current
  local archive in `/doc/www/`.) Our current attempt as of April 2025 is to link in the HTML source to the
  local archive, `href=archive`, but then on page load, the links will be rewritten by JS to `href=original`.
  This should in theory give the best of both worlds.
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
module LinkArchive (localizeLink, manualArchive, readArchiveMetadata, readArchiveMetadataAndCheck, testLinkRewrites, localizeLinkURL, getTotalSizeArchive, getTotalSizeArchiveURL, calculateArchiveSizePercentiles, ArchiveMetadata) where

import Control.Monad (filterM, unless)
import Data.Either (isLeft)
import qualified Data.Map.Strict as M (toList, fromList, insert, lookup, toAscList, union, filter, size, keys, Map)
import Data.List (isInfixOf, isPrefixOf, sortOn, sort)
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (isNothing, fromMaybe, mapMaybe)
import Text.Read (readMaybe)
import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Text as T (pack, unpack, append, Text)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Posix.Files (getFileStatus, fileSize)
import Data.FileStore.Utils (runShellCommand)
import Network.URI.TLD (parseTLD) -- tld
import Text.Pandoc (Inline(Link), nullAttr)
import Text.Show.Pretty (ppShow)
import Data.ByteString.Base16 (encode) -- base16-bytestring
import Crypto.Hash.SHA1 (hash) -- cryptohash
import Data.ByteString.Char8 (pack, unpack)
import System.FilePath (takeFileName, takeExtension, dropExtension)
import System.Directory (doesFileExist, makeAbsolute, doesDirectoryExist)
import Control.Concurrent.Async (mapConcurrently)
import qualified Control.Monad.Parallel as Par (mapM)

import LinkMetadataTypes (ArchiveMetadataItem, ArchiveMetadataList, ArchiveMetadata, Path, SizeDB)

import qualified Config.Misc as CM (cd, todayDay)
import Utils (writeUpdatedFile, putStrStdErr, green, printRed', printGreen, safeGetFileSize, getDirectoryContentsSizeRecursive, calculateSizeToPercentileMap)
import qualified Config.LinkArchive as C (whiteList, transformURLsForArchiving, transformURLsForLiveLinking, transformURLsForMobile, archiveDelay, isCheapArchive, localizeLinkTestDB, localizeLinktestCases)

-- | Calculate file size and percentile rank for each entry in the archive database.
--   Percentiles are calculated relative to all entries with positive file sizes.
--   Returns a map from URL to (Size, Percentile).
calculateArchiveSizePercentiles :: ArchiveMetadata -> IO SizeDB
calculateArchiveSizePercentiles am = do
    let keys :: [FilePath]
        keys = M.keys am

    -- Get (Maybe Int) size for each key
    sizes <- Par.mapM (getTotalSizeArchiveURL am . T.pack) keys

    -- Pair keys with their maybe sizes
    let keyedSizes :: [(FilePath, Int)]
        keyedSizes = zip keys sizes

    -- Filter out non-positive sizes, keeping valid (FilePath, Size) pairs
    let validPairs :: [(FilePath, Int)]
        validPairs = [ (fp, size) | (fp, size) <- keyedSizes, size > 0 ]

    -- Extract just the positive sizes to calculate the percentile map
    let positiveSizes :: [Int]
        positiveSizes = map snd validPairs

    -- Create the lookup map from Size -> Percentile using the helper
    let sizeToPercentileMap :: M.Map Int Int
        sizeToPercentileMap = calculateSizeToPercentileMap positiveSizes

    -- Build the final list of (FilePath, (Size, Percentile)) tuples
    -- Use mapMaybe to handle potential lookup failures gracefully (though unlikely here)
    let finalData :: [(FilePath, (Int, Int))]
        finalData = mapMaybe (\x@(fp, size) ->
                                -- Look up the percentile for this size
                                let percentMaybe = M.lookup size sizeToPercentileMap in
                                case percentMaybe of
                                  Just percentile -> Just (fp, (size, percentile))
                                  Nothing         -> error $ "LinkArchive.calculateArchiveSizePercentiles.finalData: yielded a Nothing! Should never happen? case was: " ++ show x ++ " : " ++ show percentMaybe
                             ) validPairs

    -- Create the final Map and return it (we are already in IO)
    return $ M.fromList finalData

getTotalSizeArchiveURL :: ArchiveMetadata -> T.Text -> IO Int
getTotalSizeArchiveURL _ ""   = error "LinkArchive.getTotalSizeArchiveURL: passed empty URL."
getTotalSizeArchiveURL am url = do archive <- tail <$> localizeLinkURL am (T.unpack url)
                                   if archive == T.unpack url
                                     then return (-1)
                                     else do size <- getTotalSizeArchive archive
                                             case size of
                                               Nothing -> return 0
                                               Just i -> return i

-- | Calculates the total size of a Gwern.net local-archive file path.
--   If the file ends in '.html', it *adds* the size of all files
--   in a potential subdirectory named without the '.html' extension,
--   accounting for the splitting.
--
--   Returns Nothing if the initial file path does not exist or is not a file.
--   Returns Just Int representing the total size in bytes otherwise.
getTotalSizeArchive :: FilePath -> IO (Maybe Int)
getTotalSizeArchive ""   = error "LinkArchive.getTotalSizeArchive: passed empty URL."
getTotalSizeArchive relativeFilePath = do
    -- *** Convert to absolute path immediately ***
    absFilePath <- makeAbsolute relativeFilePath

    fileExists <- doesFileExist absFilePath -- Use absolute path
    if not fileExists then
        return Nothing
    else do
        -- Get size of the primary file (using absolute path)
        initialFileSize <- safeGetFileSize absFilePath

        -- Check if it's an HTML file
        let extension = takeExtension absFilePath -- Ext from abs path (no functional diff)
        if extension == ".html" then do
            -- *** Derive potential directory from the absolute path ***
            let potentialDirPath = dropExtension absFilePath
            -- putStrLn $ "DEBUG: Checking for resource directory: " ++ potentialDirPath -- Keep if needed

            -- *** Check existence using the derived absolute path ***
            isResourceDir <- doesDirectoryExist potentialDirPath
            -- putStrLn $ "DEBUG: Does resource directory exist? " ++ show isResourceDir -- Keep if needed

            if isResourceDir then do
                -- Get the size of the resource directory contents (pass absolute path)
                resourceDirSize <- getDirectoryContentsSizeRecursive potentialDirPath
                let totalSize = initialFileSize + resourceDirSize
                return $ Just (fromIntegral totalSize)
            else do
                -- HTML file exists, but corresponding directory doesn't. Return only HTML size.
                return $ Just (fromIntegral initialFileSize)
        else do
            -- Not an HTML file, just return its size
            return $ Just (fromIntegral initialFileSize)

localizeLink :: ArchiveMetadata -> Inline -> IO Inline
localizeLink adb (Link (identifier, classes, pairs) b (targetURL, targetDescription)) = do
  targetURL' <- fmap T.pack $ rewriteLink adb $ T.unpack targetURL
  let mobileURL = T.pack $ C.transformURLsForMobile  $ T.unpack targetURL
      liveURL  = T.pack $ C.transformURLsForLiveLinking $ T.unpack targetURL
       -- NOTE: because the archive database is checked before the whitelist or `.archive-not` class, the archive database now overrides the whitelist or transforms
      archiveAttributes = if C.whiteList (T.unpack targetURL) || "archive-not" `elem` classes || targetURL == targetURL' then []
                              else [("data-url-archive", "/" `T.append` targetURL'),
                                    ("data-url-original", targetURL)]
      allAttributes = archiveAttributes ++
                          (if mobileURL == targetURL then [] else [("data-href-mobile", mobileURL)]) ++
                          (if liveURL  == targetURL then [] else [("data-url-iframe",    liveURL)])
  let archiveAnnotatedLink = Link (identifier, nubOrd classes, nubOrd (sort (pairs++allAttributes)))
                                  b
                                  (if null archiveAttributes then targetURL else ("/"`T.append`targetURL'),
                                    targetDescription)
  return archiveAnnotatedLink
localizeLink _ x = return x

-- simplified `localizeLink`: simply return the value of `data-url-archive` if it would exist when a Link with a given URL is rewritten by `localizeLink`.
localizeLinkURL :: ArchiveMetadata -> FilePath -> IO FilePath
localizeLinkURL _   ""  = error "LinkArchive.localizeLinkURL: called on an empty URL! This should never happen."
localizeLinkURL adb url = do (Link (_,_,kvs) _ _) <- localizeLink adb (Link nullAttr [] (T.pack url,""))
                             case lookup "data-url-archive" kvs of
                               Nothing   -> return url
                               Just ""   -> error $ "LinkArchive.localizeLinkURL: data-url-archive lookup returned an empty string; this should never happen! Tried to look up URL: " ++ url ++ "; key-values were: " ++ show kvs
                               Just url' -> return $ T.unpack url'

testLinkRewrites :: IO [(Inline, Inline)]
testLinkRewrites = filterNotEqual $ mapM (\(u, results) -> do
                            linkActual <- localizeLink C.localizeLinkTestDB (l u)
                            return (l' u results, linkActual)
    ) C.localizeLinktestCases

  where filterNotEqual :: IO [(Inline, Inline)] -> IO [(Inline, Inline)]
        filterNotEqual values = filterM (\(x, y) -> return (x /= y)) =<< values
        l :: T.Text -> Inline
        l url = Link nullAttr [] (url, "")
        l' :: T.Text -> (T.Text, T.Text, T.Text, [T.Text]) -> Inline
        l' url ("", mobile, html, classes) = Link ("", classes,
                                                        filter (\(_,b) -> b/="") (sort [("data-href-mobile", mobile), ("data-url-iframe", html)]))
                                                  [] (url, "")
        l' url (archive, mobile, html, classes) = Link ("", classes,
                                                        filter (\(_,b) -> b/="") (sort [("data-url-archive", archive), ("data-href-mobile", mobile), ("data-url-iframe", html), ("data-url-original", url)]))
                                                  [] (archive, "")

-- archive the first _n_ links which are due, and all pending 'cheap' archives.
-- Can be scripted like `$ cd ~/wiki/ && ghci -istatic/build/ ./static/build/LinkArchive.hs -e 'manualArchive 10'`
manualArchive :: Int -> IO ()
manualArchive n | n < 1 = error $ "manualArchive called with no work to do (𝑛 = " ++ show n ++ "); this was probably a mistake?"
                | otherwise =
 do
  adb <- readArchiveMetadataAndCheck
  today <- CM.todayDay

  let adbPendingAll = M.filter isLeft adb
  print $ "All pending URLs: " ++ show (M.size adbPendingAll)
  let adbPending = M.filter (archiveItemDue today) adbPendingAll
  let itemsWithDates = [(url, date) | (url, Left date) <- M.toList adbPending]
  let cheapItems = filter (\(u,_) -> C.isCheapArchive u) itemsWithDates

  unless (null cheapItems) $ putStrLn ("Cheap: " ++ show cheapItems)
  let sortedItems = take n $ Data.List.sortOn snd itemsWithDates
  unless (null sortedItems) $ putStrLn ("n due: " ++ show sortedItems)

  let urlsToArchive = nubOrd $ map fst $ cheapItems ++ sortedItems
  adbExecuted <- mapConcurrently archiveItem urlsToArchive

  let adb' = M.union (M.fromList $ zip urlsToArchive adbExecuted) adb
  writeArchiveMetadata adb'

archiveItemDue :: Integer -> ArchiveMetadataItem -> Bool
archiveItemDue _    (Right _)        = False -- already been tried
archiveItemDue date (Left firstSeen) = (date - firstSeen) < C.archiveDelay

archiveItem :: String -> IO ArchiveMetadataItem
archiveItem url =
 do let url' = C.transformURLsForArchiving url
    archive <- archiveURL url'
    return (Right archive)

insertLinkIntoDB :: ArchiveMetadataItem -> String -> IO ()
insertLinkIntoDB a                "" = error $ "LinkArchive.hs: called with empty URL! Other input was: " ++ show a
insertLinkIntoDB (Right Nothing) url = error $ "LinkArchive.hs: called with meaningless ArchiveMetadataItem (Right Nothing)! Other input was: " ++ show url
insertLinkIntoDB a url = do adb <- readArchiveMetadata
                            let adb' = M.insert url a adb
                            writeArchiveMetadata adb'

writeArchiveMetadata :: ArchiveMetadata -> IO ()
writeArchiveMetadata adb = writeUpdatedFile "archive-metadata-auto.db.hs" "metadata/archive.hs" (T.pack $ ppShow $ M.toAscList adb)

-- fast path:
readArchiveMetadata :: IO ArchiveMetadata
readArchiveMetadata = do CM.cd
                         pdlString <- (fmap T.unpack $ TIO.readFile "metadata/archive.hs") :: IO String
                         case (readMaybe pdlString :: Maybe ArchiveMetadataList) of
                           Nothing -> error $ "Failed to read metadata/archive.hs. First 10k characters of read string: " ++ take 10000 pdlString
                           Just pdl -> return $ M.fromList pdl
-- slow path with error-checking:
-- TODO: refactor into a pure check, which just checks URL/URI, and an IO check, which checks file existences.
readArchiveMetadataAndCheck :: IO ArchiveMetadata
readArchiveMetadataAndCheck =
 do pdl <- readArchiveMetadata
    -- check for failed archives:
    pdl' <- filterM (\(p,ami) -> case ami of
             Right (Just "") -> printRed' "Error! Invalid empty archive link: " (show p ++ " : " ++ show ami) >> return False
             Right u@(Just ('/':'/':_)) -> printRed' "Error! Invalid double-slash archive link: " (show p ++ show ami ++ show u) >> return False
             Right (Just u)  -> if not ("http" `isPrefixOf` p || "\n" `isInfixOf` p) then
                                  printGreen "Warning: Did a local link slip in somehow? (this will be removed automatically): " >> print (show p ++ show u ++ show ami) >> return False
                                else
                                  if isNothing (parseTLD p) then
                                   printRed' "Error! Invalid URI link in archive? " (show p ++ show u ++ show ami) >> return False
                                  else do let filepath = takeWhile (/='#') u
                                          exists <- doesFileExist filepath
                                          unless exists $ error ("Archive file not found: " ++ filepath ++ " (original path in archive.hs: " ++ u ++ "; original tuple: " ++ show (p,ami) ++ ")")
                                          size <- getFileStatus filepath >>= \s -> return $ fileSize s
                                          if size == 0 then
                                            printRed' "Error! Empty archive file. Not using: " (show p ++ show u ++ show ami) >> return False
                                            else if size > 1024 then return True else return False
             Right Nothing   -> return True
             Left  _         -> return True)
         $ M.toList pdl
    let pdl'' = filter (\(p,_) -> "http" `isPrefixOf` p && not (C.whiteList p)) pdl'
    -- for mismatches, we know they were archived before, so we should archive them ASAP:
    let pdl''' = map (\(p,ami) ->  if checksumIsValid p ami then (p,ami) else (p, Left 0)) pdl''
    return $ M.fromList pdl'''

-- When we rewrite links to fix link rot, archive.hs can become stale: it records a failed archive of the old URL, and doesn't know there's a new URL because archive.hs was rewritten with the rest of Gwern.net.
-- But since the checksum is deterministically derived from the URL, the checksum of the URL will no longer match the checksum encoded in the file name.
-- So when there is a mismatch, we can drop that entry, deleting it, and now the new URL will get picked up as a fresh URL entered into the archive queue.
checksumIsValid :: Path -> ArchiveMetadataItem -> Bool
checksumIsValid _ (Left _) = True
checksumIsValid _ (Right Nothing) = True
checksumIsValid url (Right (Just file)) = let derivedChecksum = Data.ByteString.Char8.unpack $
                                                Data.ByteString.Base16.encode $ Crypto.Hash.SHA1.hash $
                                                Data.ByteString.Char8.pack (C.transformURLsForArchiving (takeWhile (/='#') url) ++ "\n")
                                              storedChecksum = takeWhile (/= '.') $ takeFileName file
                                          in derivedChecksum == storedChecksum

-- rewriteLink:
-- 1. Skip whitelisted URLs.
-- 2. Access what we know about the URL, defaulting to "First I've heard of it.".
-- 3. If not seen before, insert into the archive DB with the current date as 'first-seen'
-- 4. Else, return archive path.
rewriteLink :: ArchiveMetadata -> String -> IO String
rewriteLink adb url = fromMaybe url <$> if C.whiteList url then return Nothing else
 case M.lookup url adb of
      Nothing                -> do today <- CM.todayDay
                                   Nothing <$ insertLinkIntoDB (Left today) url
      Just (Left  _)         -> return Nothing
      Just (Right (Just "")) -> printRed' "Error! Tried to return a link to a non-existent archive! " url >> return Nothing
      Just (Right archive)   -> return archive

-- take a URL, archive it, and if successful return the hashed path
archiveURL :: String -> IO (Maybe Path)
archiveURL l = do let args = if C.isCheapArchive l then ["--no-preview", l] else [l]
                  (exit,stderr',stdout) <- runShellCommand "./" Nothing "linkArchive.sh" args
                  case exit of
                     ExitSuccess -> do let result = U.toString stdout
                                       putStrStdErr (green "Archiving (LinkArchive.hs): " ++ l ++ green  " returned: "  ++ result ++ "\n")
                                       if result == "" then return Nothing else
                                         do let filepath = takeWhile (/='#') result
                                            exists <- doesFileExist filepath
                                            unless exists $ error ("LinkArchive.hs:archiveURL: Archived file not found: " ++ filepath ++ " (original path returned by linkArchive.sh: " ++ result ++ " for URL: " ++ l ++ "); this should never happen.")
                                            return $ Just result
                     ExitFailure _ -> printRed' "LA: archiving script failed to run correctly: " (l ++ " result: " ++ U.toString stderr') >> return Nothing
