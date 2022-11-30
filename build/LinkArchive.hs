{- LinkArchive.hs: module for generating Pandoc external links which are rewritten to a local static
                   mirror which cannot break or linkrot—if something's worth linking, it's worth hosting!
Author: Gwern Branwen
Date: 2019-11-20
When:  Time-stamp: "2022-11-28 22:21:51 gwern"
License: CC-0
Dependencies: pandoc, filestore, tld, pretty; runtime: SingleFile CLI extension, Chromium, wget, etc (see `linkArchive.sh`)
-}

{- Local Mirror design:

Because link rot has proven difficult to keep up with on `gwern.net` using [ordinary reactive link
archiving methods](https://www.gwern.net/Archiving-URLs), I am switching to *pre-emptive archiving*:
for most external links on Gwern.net, they will now point to a local (stored on Gwern.net) mirror of
the original external link as the default. As the cost of disk/bandwidth falls while the value of
human attention increases, if something is worth linking, it is worth hosting—assuming good tooling.
The local mirror will be a self-contained static HTML copy which cannot linkrot (unless Gwern.net
itself goes down in which case the issue is largely moot). The local mirrors will, aside from being
infinitely more reliable, also be faster for the reader to load & browse, as they will be loaded
from the current domain's CDN and are the final DOMs saved using adblock etc (benefiting the large
fraction of Gwern.net readers who do not have ad blockers installed).

The implementation strategy is, similar to the [link
popups](https://www.gwern.net/static/build/LinkMetadata.hs), a Pandoc Hakyll plugin which at
Markdown → HTML compile-time traverses the Markdown AST for non-whitelisted external links, looks
for a local mirror of each one, create a local mirror via SingleFile
(https://github.com/gildas-lormeau/SingleFile/) if necessary, and rewrites the link to point to the
local mirror.

Details:

- at compile-time, [`hakyll.hs`](https://www.gwern.net/static/build/hakyll.hs) reads the local
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
- mirrors are saved to `wiki/docs/www/$DOMAIN($URL)/SHA1($URL).html`; the additional nesting by
  domain name is necessary to allow CSS link annotations to match rewritten links

    - `/docs/www/` is excluded from the `sitemap.xml` & `sync-gwern.net` compile-time checks,
      blocked in `robots.txt`, `rel="canonical"` is set in the mirrors by SingleFile,
      additional`rel="archive nofollow"` attributes are set on each link when used in Gwern.net, and
      noarchive/noindex/nofollow/nocache are set as HTTP headers by nginx for `/docs/www/`; exposing
      thousands of mirrors to the rest of the Internet risks causing SEO issues, high bandwidth
      consumption, and legal or social problems
- links are rewritten to point the `href` to `wiki/docs/www/$DOMAIN($URL)/SHA1($URL).html`; like
  inflation-adjustments or link popups, the original `href` is stored as a span (eg. `<a
  href="https://foo.com">foo</a>` → `<a rel="archive nofollow" data-url-original="https://foo.com"
  href="/docs/www/foo.com/cf934d97a8012ba1c2d354d6cd39e77535fd0fb9.html">foo</a></span>`)
- the `data-url-original` metadata is used by `popups.js` to add to link popups a '[original]'
  hyperlink (using the JS templating, something like `<p>..."Title" [<a
  href="${target.dataset.urlOriginal}" title="Original (live) Internet version of
  ${target.dataset.popupTitle}">original</a>]...</p>`)

There are approximately 30k external links on Gwern.net as of October 2019, of which perhaps 5k need
to be mirrored; I estimate this will take up somewhere on the order of ~50GB and add a few dollars
to S3 hosting costs. (After exclusions, my archive was 5300 links (excluding optional PDFs) / 20GB
in February 2020.) But it'll be worth it to forestall thousands of dying links, regular reader
frustration, and a considerable waste of my time every month dealing with the latest broken links. -}

{-# LANGUAGE OverloadedStrings #-}
module LinkArchive (archivePerRunN, localizeLink, readArchiveMetadata, ArchiveMetadata) where

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

import Utils (writeUpdatedFile, printGreen, printRed, sed, addClass, anyInfix, anyPrefix, anySuffix, currentDay, replace)

archiveDelay, archivePerRunN :: Integer
archiveDelay = 60
archivePerRunN = 12

type ArchiveMetadataItem = Either
  Integer -- Age: first seen date -- ModifiedJulianDay, eg. 2019-11-22 = 58810
  (Maybe FilePath) -- Our archive of the URL: local archive path (if successful, otherwise, should be skipped - already dead?)
type ArchiveMetadataList= [(Path, -- URL: original raw URL of an external link
                            ArchiveMetadataItem)] -- date/path
type ArchiveMetadata = M.Map Path ArchiveMetadataItem
type Path = String

-- Pandoc types: Link = Link Attr [Inline] Target; Attr = (String, [String], [(String, String)]); Target = (String, String)
localizeLink :: ArchiveMetadata -> IORef Integer -> Inline -> IO Inline
localizeLink adb archived x@(Link (identifier, classes, pairs) b (targetURL, targetDescription)) =
  -- skip local archiving if matches the whitelist, or it has a manual annotation '.archive-not' class on it, like
  -- `[Foo](!W "Bar"){.archive-not}` in which case we don't do any sort of 'archiving' such as rewriting to point to a
  -- local link (or possibly, in the future, rewriting WP links to point to the historical revision ID when first
  -- linked, to avoid deletionist content rot)
  if whiteList (T.unpack targetURL) || "archive-not" `elem` classes then return x else
    do targetURL' <- rewriteLink adb archived $ T.unpack targetURL
       if targetURL' == T.unpack targetURL then return x -- no archiving has been done yet, return original
       else do -- rewrite & annotate link with local archive:
         let padding = if targetDescription == "" then "" else " "
         let targetDescription' = T.unpack targetDescription ++ padding ++ "(Original URL: " ++ T.unpack targetURL ++ " )"
         -- specify that the rewritten links are mirrors & to be ignored:
         let archiveAttributes = [("rel", "archived alternate nofollow"), ("data-url-original", T.pack (transformURLsForLinking (T.unpack targetURL)))]
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
                                     Right (Just "") -> printRed ("Error! Invalid empty archive link: ") >> print (show p ++ show ami) >> return False
                                     Right u@(Just ('/':'/':_)) -> printRed "Error! Invalid double-slash archive link: " >> print (show p ++ show ami ++ show u) >> return False
                                     Right (Just u)  -> if not ("http" `isPrefixOf` p || "\n" `isInfixOf` p) then
                                                          printRed "Error! Did a local link slip in somehow? " >> print (show p ++ show u ++ show ami) >> return False
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
                            let pdl'' = filter (\(p,_) -> "http" `isPrefixOf` p && not (whiteList p)) pdl'
                            -- for mismatches, we know they were archived before, so we should archive them ASAP:
                            let pdl''' = map (\(p,ami) ->  if checksumIsValid p ami then (p,ami) else (p, Left 0)) pdl''
                            return $ M.fromList pdl'''

-- When we rewrite links to fix link rot, archive.hs can become stale: it records a failed archive of the old URL, and doesn't know there's a new URL because archive.hs was rewritten with the rest of gwern.net. But since the checksum is deterministically derived from the URL, the checksum of the URL will no longer match the checksum encoded in the file name. So when there is a mismatch, we can drop that entry, deleting it, and now the new URL will get picked up as a fresh URL entered into the archive queue.
checksumIsValid :: Path -> ArchiveMetadataItem -> Bool
checksumIsValid _ (Left _) = True
checksumIsValid _ (Right Nothing) = True
checksumIsValid url (Right (Just file)) = let derivedChecksum = Data.ByteString.Char8.unpack $ Data.ByteString.Base16.encode $ Crypto.Hash.SHA1.hash $ Data.ByteString.Char8.pack (transformURLsForArchiving (takeWhile (/='#') url) ++ "\n")
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
  fromMaybe url <$> if whiteList url then return Nothing else
    case M.lookup url adb of
      Nothing               -> Nothing <$ insertLinkIntoDB (Left today) url
      Just (Left firstSeen) -> if ((today - firstSeen) < archiveDelay) && not ("pdf" `isInfixOf` url && not ("twitter" `isInfixOf` url))
        then return Nothing
        else do
                 let url' = transformURLsForArchiving url
                 archivedP <- archiveURLCheck url'
                 if archivedP then do archive <- archiveURL url'
                                      insertLinkIntoDB (Right archive) url
                                      return archive
                 else do archivedNAlreadyP <- readIORef archivedN
                         -- have we already used up our link archive 'budget' this run? If so, skip all additional link archives
                         if archivedNAlreadyP > 0 then do archive <- archiveURL url'
                                                          insertLinkIntoDB (Right archive) url
                                                          writeIORef archivedN (archivedNAlreadyP - 1)
                                                          return archive
                         else return Nothing
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
archiveURL l = do (exit,stderr',stdout) <- runShellCommand "./" Nothing "linkArchive.sh" [l]
                  case exit of
                     ExitSuccess -> do let result = U.toString stdout
                                       printGreen ( "Archiving (LinkArchive.hs): " ++ l ++ " returned: " ++ result)
                                       if result == "" then return Nothing else return $ Just result
                     ExitFailure _ -> printRed (l ++ " : archiving script failed to run correctly: ") >> print (U.toString stderr') >> return Nothing

-- sometimes we may want to do automated transformations of a URL *before* we check any whitelists. In the case of
-- Arxiv, we want to generate the PDF equivalent of the HTML abstract landing page, so the PDF gets archived, but then
-- we also want to rewrite it to use the Ar5iv (HTML5 version) service, and provide *both*.
--
-- In the case of OpenReview, the `forum?id=` is the peer reviews, which are worth reading, but we'd like to provide the
-- PDF link too. We don't need a third version, just to provide the two, so this is easier than the Ar5iv rewrite.
-- (Hypothetically, we could do Twitter→Nitter, Reddit.com→Old.Reddit.com, or LW→GW rewrites this way too.)
transformURLsForArchiving :: String -> String
transformURLsForArchiving = sed "https://arxiv.org/abs/([0-9]+\\.[0-9]+)(#.*)?" "https://arxiv.org/pdf/\\1.pdf\\2" . sed "https://arxiv.org/abs/([a-z-]+)/([0-9]+).*(#.*)?" "https://arxiv.org/pdf/\\1/\\2.pdf\\3"
                            . replace "https://openreview.net/forum" "https://openreview.net/pdf"
                            -- Mobile Reddit snapshots for live popups much better than Old Reddit (although Old Reddit is a much better browsing/user experience)
                            . replace "https://old.reddit.com" "https://i.reddit.com"
transformURLsForLinking   :: String -> String
transformURLsForLinking   = sed "https://arxiv.org/abs/([0-9]+\\.[0-9]+)(#.*)?" "https://ar5iv.labs.arxiv.org/html/\\1?fallback=original\\2" .
  sed "https://arxiv.org/abs/([a-z-]+)/([0-9]+).*(#.*)?" "https://ar5iv.labs.arxiv.org/html/\\1/\\2?fallback=original\\3" -- handle oddities like hep-ph

{- re URL transforms: Why?

The status quo of Arxiv links is unsatisfactory. Linking to the abstract page is the worst of all worlds. The
annotations for your standard Arxiv /abs/ landing page link provide almost all of the information that the abstract page
does, and so the abstract page is redundant; the abstract page can't be popped-up either, as Arxiv is one of the many
websites which sets headers blocking cross-site loads so your web browser will refuse to pop it up. So an Arxiv link
requires at least 2 clicks to do anything useful: click on the title link, and then click on the `PDF` link. What to do?

We could use only PDF links. Arxiv /abs/ links *could* be rewritten to go straight to /pdf/. The upside is that this
would make clicking on the link meaningful (saving 1 click), and it would integrate into my local archiving system
cleanly, so the link would be to a local mirror of the Arxiv PDF, which would be both faster & popup-able (saving 2
clicks while being nicer).
But the downside is that then mobile users will have a bad experience (it might need to download and be viewed in an
entirely separate app!) and people who don't want to deal with PDFs at that moment (eg. no night mode) would also prefer
to not be shunted into a PDF when they could have been linked to a HTML landing page. Thus, if you hang around Reddit or
Twitter or HN, you will see people or even outright bots responding to Arxiv PDF links with the /abs/ instead. This
strikes me as fussy (I don't mind PDF links) but I can't deny that these people exist and seem to care.

Could we use non-abstract HTML links? Unlike BioRxiv/MedRxiv, where you can simply append `.full` and get a nice usable
HTML version, Arxiv provides only the PDF, so it's unclear what other HTML page you could send people to. The good news
is that there turns out there are projects to create HTML versions of Arxiv PDFs:
[Arxiv-vanity](https://www.arxiv-vanity.com/) and a new one, [Ar5iv](https://ar5iv.labs.arxiv.org/). Both use the same
trick: a LaTeX→HTML compiler <https://github.com/brucemiller/LaTeXML>. (As they use the same compiler, they are fairly
similar, but Ar5iv appears to be much more ambitious & actively maintained and may be merged into Arxiv proper at some
point, so I will consider just Ar5iv.) Compiling LaTeX to anything else is... hard. And many of the papers have
rendering problems, major or minor, or are not present at all. (Ar5iv is about a month out of date. They served an error
page before, but at my request changed it to redirect to Arxiv proper with the query parameter `fallback=original`
(redirecting without an option apparently confuses non-gwern.net readers), so you can just rewrite your Arxiv links and
not need to care about whether Ar5iv has it or not.) But they provide responsive reflowable HTML,
justification/hyphenation, and dark mode, so for a mobile smartphone user, this is, for many users and many papers and
many circumstances, better than your average 1-column paper, or constantly dragging on a 2-column paper. Still, it's not
so much better that all the PDF readers will want to see the HTML version instead of the PDF version.

So linking to the /abs/ makes no one happy; linking the PDF makes all mobile and some desktop users unhappy; and linking
to the Ar5iv HTML version is the opposite. What to do? Well, why not link *both*? Popups already have a system for
linking a local PDF or HTML archive of a URL, and also the URL: the PDF is the main link, and then a small `[LIVE]` link
is provided to the original live un-archived URL. So we could rewrite every /abs/ link to /pdf/, which will then get
archived & rewritten to the local archive, and then the 'original' URL gets quietly rewritten Arxiv → Ar5iv. To make it
even more transparent, we swap 'LIVE' for 'HTML' (it's not really the 'live' link anymore, and 'HTML' tells the mobile
user it may serve them better.) Mobile users see the PDF icon, avoid it, and go to `[HTML]`, desktop or PDF-enjoyers
hover on it and without a click get their PDF, and after a bit of learning & adjustment (hopefully near-instant due to
the icons & text labels), everyone gets their preferred medium to read the paper.

The implementation is a little uglier than that because the popups JS code does not expect the original-URL data to be
fiddled with, but it works now and is live on Gwern.net. (On some Arxiv links. Trying to download all the PDFs got my IP
banned temporarily. I'll get the rest eventually.)

While the logic is a little opaque to readers, I think this handles Arxiv much more cleanly than before. -}

-- whitelist of strings/domains which are safe to link to directly, either because they have a long history of stability
-- & reader-friendly design, or attempting to archive them is pointless (eg. interactive services); and blacklist of
-- URLs we always archive even if otherwise on a safe domain:
-- 1. some matches we always want to skip
-- 2. after that, we want to mirror PDFs everywhere (except Gwern.net because that's already 'mirrored')
-- 3. after that, we may want to skip various filetypes and domains
whiteList :: String -> Bool
whiteList url
  | anyInfix url ["citeseerx.ist.psu.edu"] = False -- TODO: after fixing all existing Citeseer links, set this rule to False
  | anyPrefix url ["/", "./", "../", "https://www.gwern.net", "#", "!", "$", "mailto", "irc", "/metadata/", "/docs/"] = True
  | anySuffix url [".pdf", "/pdf", ".pdf#"] = False
  | anyInfix url [".txt" -- TODO: generalize the PDF download to handle all non-HTML filetypes
      , ".xlsx"
      , ".xz"
      , ".csv"
      , ".ps"
      , ".mp3"
      , ".png"
      , ".ogg"
      , ".jpg"
      , ".ogg"
      , "halshs.archives-ouvertes.fr/"
      , "apenwarr.ca"
      , "distill.pub"
      , "thegradient.pub"
      , "perma.cc"
      , "nips.cc"
      , "youtu.be"
      , "nomeata.de"
      , "ccc.de"
      , "your-server.de"
      , "philosopher.life"
      , "blog.google"
      , "leme.me/"
      , "bakabt.me"
      , "fullfrontal.moe"
      , "girls.moe"
      , "google.com/cse"
      , "aleph.se"
      , "grok.se"
      , "w3.org"
      , "wikimedia.org"
      , "wikipedia.org"
      , "scholarpedia.org"
      , "mozilla.org"
      , "r-inla.org"
      , "coursera.org"
      , "ourworldindata.org"
      , "isfdb.org"
      , "tor2web.org"
      , "mementoweb.org"
      , "cdlib.org"
      , "econlib.org"
      , "ssgac.org"
      , "stlouisfed.org"
      , "davidsongifted.org"
      , "projecteuclid.org"
      , "erowid.org"
      , "rand.org"
      , "cato-unbound.org"
      , "bellard.org"
      , "straighttalkonevidence.org"
      , "intelligence.org"
      , "wikisource.org"
      , "edge.org"
      , "gmane.org"
      , "sciencebasedmedicine.org"
      , "plosmedicine.org"
      , "waybackmachine.org"
      , "psychiatryonline.org"
      , "plosone.org"
      , "antipope.org"
      , "nixnote.org"
      , "wikiquote.org"
      , "archive.org"
      , "web.archive.org"
      , "philarchive.org"
      , "httparchive.org"
      , "ietf.org"
      , "sciencemag.org"
      , "racket-lang.org"
      , "fightaging.org"
      , "royalsocietypublishing.org"
      , "gutenberg.org"
      , "familysearch.org"
      , "rcpsych.org"
      , "jneurosci.org"
      , "mediawiki.org"
      , "emacswiki.org"
      , "dwarffortresswiki.org"
      , "ledger-cli.org"
      , "doi.org"
      , "mnemosyne-proj.org"
      , "imagemagick.org"
      , "onegeek.org"
      , "kk.org"
      , "econtalk.org"
      , "deeplearningbook.org"
      , "openscienceframework.org"
      , "equator-network.org"
      , "haskell.org"
      , "realworldhaskell.org"
      , "tryhaskell.org"
      , "givewell.org"
      , "archiveteam.org"
      , "nejm.org"
      , "longform.org"
      , "qntm.org"
      , "4chan.org"
      , "debian.org"
      , "mc-stan.org"
      , "ajcn.org"
      , "libgen.org"
      , "frontiersin.org"
      , "journalofvision.org"
      , "wikimediafoundation.org"
      , "poetryfoundation.org"
      , "psychiatryinvestigation.org"
      , "scitation.org"
      , "rdocumentation.org"
      , "ama-assn.org"
      , "datascienceassn.org"
      , "wikichip.org"
      , "escholarship.org"
      , "winehq.org"
      , "semanticscholar.org"
      , "iacr.org"
      , "nber.org"
      , "jstor.org"
      , "npr.org"
      , "pnas.org"
      , "tools.wmflabs.org"
      , "sciencepubs.org"
      , "genetics.org"
      , "plosgenetics.org"
      , "elifesciences.org"
      , "tvtropes.org"
      , "mayoclinicproceedings.org"
      , "evageeks.org"
      , "wikibooks.org"
      , "annals.org"
      , "esajournals.org"
      , "oxfordjournals.org"
      , "yourmorals.org"
      , "creativecommons.org"
      , "tasvideos.org"
      , "plos.org"
      , "philpapers.org"
      , "80000hours.org"
      , "rootsofprogress.org"
      , "humanprogress.org"
      , "longbets.org"
      , "cogprints.org"
      , "annualreviews.org"
      , "sciencenews.org"
      , "r-project.org"
      , "metafor-project.org"
      , "torproject.org"
      , "personality-project.org"
      , "ieet.org"
      , "jstatsoft.org"
      , "archive-it.org"
      , "slashdot.org"
      , "hathitrust.org"
      , "nongnu.org"
      , "safebooru.org"
      , "derpibooru.org"
      , "biorxiv.org" -- stable
      , "medrxiv.org" -- stable
      , "publicdomainreview.org" -- stable
      , "biomedicalcomputationreview.org"
      , "theparisreview.org"
      , "usenix.org"
      , "archlinux.org"
      , "html-tidy.org"
      , "plosbiology.org"
      , "scipy.org"
      , "wiktionary.org"
      , "wellcomelibrary.org"
      , "ecologyandsociety.org"
      , "ieee-security.org"
      , "jwz.org"
      , "google.com/search"
      , "vast.ai"
      , "iqtest.dk"
      , "ac.uk"
      , "goo.gl"
      , "dtic.mil"
      , "fastmail.fm"
      , "c2.com"
      , "everything2.com"
      , "doc88.com"
      , "arstechnica.com" -- stable / poor-quality archiving - articles are usually split over multiple pages, so archiving just the link is no good
      , "uptontea.com"
      , "wolframalpha.com"
      , "nvidia.com"
      , "wikia.com"
      , "algolia.com"
      , "atlasobscura.com"
      , "gitlab.com"
      , "flashcarddb.com"
      , "imdb.com"
      , "sagepub.com"
      , "amsciepub.com"
      , "liebertpub.com"
      , "bbc.com"
      , "theatlantic.com"
      , "xkcd.com"
      , "buzzfeed.com"
      , "cogmed.com"
      , "haveibeenpwned.com"
      , "ted.com"
      , "deepmind.com"
      , "wunderground.com"
      , "soundcloud.com"
      , "youtube.com"
      , "sendspace.com"
      , "vice.com"
      , "googlecode.com"
      , "paperswithcode.com"
      , "flashcardexchange.com"
      , "stackexchange.com"
      , "google-melange.com"
      , "theverge.com"
      , "kaggle.com"
      , "bigquery.cloud.google.com"
      , "cse.google.com"
      , "translate.google.com"
      , "drive.google.com"
      , "colab.research.google.com"
      , "powersearchingwithgoogle.com"
      , "scholar.google.com"
      , "docs.google.com"
      , "spreadsheets.google.com"
      , "plus.google.com"
      , "tandfonline.com"
      , "sciencenewsline.com"
      , "examine.com"
      , "discovermagazine.com"
      , "tomshardware.com"
      , "joelonsoftware.com"
      , "mediafire.com"
      , "nature.com"
      , "springernature.com"
      , "theuncertainfuture.com"
      , "timeanddate.com"
      , "slate.com"
      , "iqcomparisonsite.com"
      , "mail-archive.com"
      , "darknetlive.com"
      , "docsdrive.com"
      , "tineye.com"
      , "smithsonianmag.com"
      , "nymag.com"
      , "norvig.com"
      , "librarything.com"
      , "greaterwrong.com"
      , "lesswrong.com"
      , "farnamstreetblog.com"
      , "bloomberg.com"
      , "f1000research.com"
      , "patch.com"
      , "openai.com"
      , "mdpi.com"
      , "bmj.com"
      , "peerj.com"
      , "wsj.com"
      , "dual-n-back.com"
      , "equilibriabook.com"
      , "unsongbook.com"
      , "predictionbook.com"
      , "johndcook.com"
      , "jamanetwork.com"
      , "animenewsnetwork.com"
      , "whichfaceisreal.com"
      , "bioethicsjournal.com"
      , "biologicalpsychiatryjournal.com"
      , "biomedcentral.com"
      , "psychcentral.com"
      , "nintil.com"
      , "cell.com"
      , "learnyouahaskell.com"
      , "codeschool.com"
      , "eepurl.com"
      , "paulgraham.com"
      , "fandom.com"
      , "nickbostrom.com"
      , "ribbonfarm.com"
      , "scientificamerican.com"
      , "theguardian.com"
      , "andrewgelman.com"
      , "statmodeling.stat.columbia.edu"
      , "marginalrevolution.com"
      , "clickotron.com"
      , "patrickcollison.com"
      , "amazon.com" -- service
      , "amzn.to" -- service redirect
      , "amzn.com"
      , "sparkfun.com"
      , "greenspun.com"
      , "shawwn.com"
      , "gizmodo.com"
      , "vimeo.com"
      , "duckduckgo.com"
      , "duolingo.com"
      , "projectrho.com"
      , "rocketpunk-manifesto.com"
      , "tarsnap.com"
      , "bandcamp.com"
      , "oup.com"
      , "gnxp.com"
      , "artbreeder.com" -- interactive/service
      , "beeminder.com" -- interactive/service
      , "springer.com"
      , "schneier.com"
      , "pcpartpicker.com" -- interactive
      , "newyorker.com" -- stable
      , "talktotransformer.com" -- interactive
      , "feedburner.com" -- service
      , "metafilter.com" -- stable
      , "kickstarter.com" -- service
      , "kill-the-newsletter.com" -- service
      , "tinyletter.com" -- service
      , "vanityfair.com" -- stable
      , "flickr.com" -- stable
      , "hpmor.com" -- stable
      , "cryonicscalculator.com" -- interactive/service
      , "wikifur.com" -- stable
      , "overcomingbias.com" -- stable
      , "waifulabs.com" -- interactive/service
      , "rpubs.com"
      , "smbc-comics.com"
      , "goodreads.com"
      , "longreads.com"
      , "idlewords.com"
      , "cambridgebrainsciences.com"
      , "nytimes.com"
      , "motherjones.com"
      , "snopes.com"
      , "scienceblogs.com"
      , "googleapis.com"
      , "goproblems.com"
      , "r-bloggers.com"
      , "rifters.com" -- stable
      , "thepharmacyexpress.com" -- interactive/service
      , "inklingmarkets.com" -- interactive/service
      , "academictorrents.com" -- interactive/service
      , "kalzumeus.com"
      , "sciencedirect.com" -- stable or needs manual archiving
      , "meltingasphalt.com"
      , "dropboxusercontent.com" -- needs manual archiving
      , "dafont.com" -- stable
      , "millionshort.com" -- service
      , "cogtest.com" -- service
      , "iqtest.com" -- service
      , "proquest.com" -- needs manual archiving
      , "economist.com" -- stable - I think? TODO: does The Economist paywall?
      , "thesecatsdonotexist.com" -- interactive/service
      , "thiseyedoesnotexist.com" -- interactive/service
      , "thismarketingblogdoesnotexist.com" -- interactive/service
      , "thisrentaldoesnotexist.com" -- interactive/service
      , "thisvesseldoesnotexist.com" -- interactive/service
      , "thispersondoesnotexist.com" -- interactive/service
      , "thiscatdoesnotexist.com" -- interactive/service
      , "thisstorydoesnotexist.com" -- interactive/service
      , "thiswaifudoesnotexist.net" -- interactive/service
      , "thisanimedoesnotexist.ai" -- interactive/service
      , "washingtonpost.com" -- stable
      , "iqout.com"  -- interactive/service
      , "ubuntu.com" -- stable
      , "psyarxiv.com" -- stable
      , "stackroboflow.com"
      , "stackoverflow.com" -- stable
      , "slatestarcodex.com" -- stable
      , "wikiwix.com" -- archive/mirror
      , "dropbox.com" -- needs manual archiving
      , "dl.dropbox.com" -- needs manual archiving
      , "vox.com" -- stable
      , "psychologytoday.com" -- stable
      , "gratipay.com" -- dead
      , "berkshirehathaway.com" -- stable
      , "evamonkey.com" -- stable
      , "wiley.com" -- stable or needs manual archiving
      , "guzey.com" -- stable
      , "europeanneuropsychopharmacology.com" -- stable
      , "genomebiology.com" -- stable
      , "sciencedaily.com" -- stable
      , "philanthropy.com" -- stable
      , "urbandictionary.com" -- stable
      , "qwantz.com" -- stable
      , "pinboard.in" -- stable
      , "otokei-dou.in" -- dead
      , "bl3j73taluhwidx5.onion" -- dead
      , "thehub7dnl5nmcz5.onion" -- dead
      , "silkroad5v7dywlc.onion" -- dead
      , "thehub7gqe43miyc.onion" -- dead
      , "hmxutiksabsj4ozi.onion" -- dead
      , "5onwnspjvuk7cwvk.onion" -- dead
      , "grams7enufi7jmdl.onion" -- dead
      , "assmkedzgorodn7o.onion" -- dead
      , "silkroadvb5piz3r.onion" -- dead
      , "lacbzxobeprssrfx.onion" -- dead
      , "alpaca727o3c75xx.onion" -- dead
      , "kth2mwuwlkezwziy.onion" -- dead
      , "i25c62nvu4cgeqyz.onion" -- dead
      , "gitcoin.co"
      , "gitlab.io" -- stable
      , "osf.io" -- stable
      , "libgen.io" -- archive
      , "websitedownloader.io" -- service/source code
      , "webrecorder.io" -- service/source code
      , "keras.io" -- stable
      , "shinyapps.io" -- interactive
      , "archivebox.io" -- archive
      , "toranoana.jp" -- click-walled
      , "intrade.com/jsp" -- dead
      , "isciii.es"
      , "archive.is" -- archive
      , "books.google.com/books"
      , "buzzricksons" -- stable
      , "reddit.com/r/AgMarketplace/" -- dead (banned)
      , "reddit.com/r/BlackBank/" -- dead (banned)
      , "reddit.com/r/DarkNetDeals/" -- dead (banned)
      , "reddit.com/r/DarkNetMarkets/" -- dead (banned)
      , "reddit.com/r/DarknetMarketsNZ/" -- dead (banned)
      , "reddit.com/r/MEMarketplace/" -- dead (banned)
      , "reddit.com/r/QuantikXanax/" -- dead (banned)
      , "reddit.com/r/blackmarketreloaded/" -- dead (banned)
      , "reddit.com/r/fakeid/" -- dead (banned)
      , "reddit.com/r/grams/" -- dead (banned)
      , "reddit.com/r/havanamarket/" -- dead (banned)
      , "reddit.com/r/medsforbitcoins/" -- dead (banned)
      , "reddit.com/r/modup/" -- dead (banned)
      , "reddit.com/r/sanitariummarket/" -- dead (banned)
      , "reddit.com/r/themarketplace/" -- dead (banned)
      , "donmai.us" -- stable
      , "hutter1.net" -- stable
      , "anidb.net" -- stable
      , "ankiweb.net"
      , "johnmacfarlane.net" -- stable
      , "filfre.net" -- stable
      , "researchgate.net" -- needs to be replaced by hand
      , "boingboing.net" -- stable
      , "nearlyfreespeech.net" -- service
      , "urth.net" -- stable
      , "scp-wiki.net" -- stable
      , "touhouwiki.net" -- stable
      , "epjournal.net"
      , "gwern.net" -- redundant
      , "www.gwern.net" -- redundant
      , "lwn.net" -- stable
      , "incompleteideas.net" -- stable
      , "videolectures.net" -- service/interactive
      , "torservers.net" -- stable
      , "ankisrs.net" -- stable
      , "sethroberts.net" -- stable
      , "cloudfront.net" -- service
      , "obormot.net" -- stable
      , "personalitytest.net" -- interactive
      , "myanimelist.net" -- stable
      , "mathoverflow.net" -- stable
      , "bitcoin.it" -- stable
      , "http://get.tt/" -- archive/dead
      , "academia.edu" -- need to be replaced by hand
      , "nih.gov" -- stable
      , "clinicaltrials.gov" -- stable
      , "bls.gov" -- stable
      , "ndcourts.gov" -- interactive/service
      , "uscourts.gov" -- interactive/service
      , "census.gov" -- stable
      , "treasurydirect.gov" -- service
      , "twitch.tv"  -- service
      , "archive.today" -- archive
      , "archive.ph" -- archive
      , "quantum.country" -- interactive
      , "www.michaelnielsen.org" -- stable
      , "numinous.productions" -- stable/interactive
      , "neuralnetworksanddeeplearning.com" -- stable
      , "mega.nz"  -- service
      , "stylegan.xyz" -- dead
      , "beepb00p.xyz" -- stable
      , "equestriadaily.com/search" -- service
      , "libgen.is" -- service
      , "libgen.rs" -- service
      , "gen.lib.rus.ec" -- service
      , "nyaa.se" -- service
      , "nyaa.eu" -- service
      , "nyaa.si" -- service
      , "modup.net" -- service
      , "flutterguy.org" -- service
      , "abandonedfootnotes.blogspot.com" -- stable
      , "jacurutu.com" -- stable
      , "fanfiction.net" -- stable
      , "patreon.com/" -- service
      , "practicaltypography.com" -- stable
      , "oglaf.com" -- stable
      , "catb.org" -- stable
      , "ascii.textfiles.com" -- stable
      , "ricon.dev" -- service
      , "kanotype.iptime.org:8003" -- service (DeepDanbooru)
      , "sourceforge.net" -- service/source code
      , "waifu2x.udp.jp" -- service
      , "danwang.co" -- stable
      , "cognitivefun.net" -- interactive
      , "groups.google.com" -- low quality
      , "twitter.com" -- low quality: breaks ArchiveBox by straight up lying & claiming all tweets not found; with SingleFile, prompts to click & then 403 error
      , "www.impactcybertrust.org" -- stable/service
      , "old.reddit.com/r/DarkNetMarkets/" -- deleted by Reddit
      , "old.reddit.com/r/afinil/" -- over-18 so logged out SingleFile doesn't work
      , "old.reddit.com/r/SilkRoad/" -- over-18 so logged out SingleFile doesn't work
      , "reddit.com/r/gwern/.rss" -- news/portal
      , "antilop.cc" -- stable
      , "www.ncbi.nlm.nih.gov" -- stable
      , "eric.ed.gov" -- stable
      , "scholars-stage.org" -- stable
      , "toolbox.google.com/datasetsearch/" -- interactive
      , "trends.google.com" -- interactive
      , "landing.google.com/sre" -- stable
      , "onlinelibrary.wiley.com" -- low-quality archives
      , "old.reddit.com/r/MLPtunes/" -- news/portal
      , "old.reddit.com/r/TOUHOUMUSIC/" -- news/portal
      , "old.reddit.com/r/Vocaloid/" -- news/portal
      , "thepiratebay." -- service
      , "reddit.com/user/gwern/" -- news/portal
      , "old.reddit.com/r/gwern/" -- news/portal
      , "snpedia.com" -- stable
      , "recapthelaw.org" -- service
      , "dspace.mit.edu" -- stable
      , "www.find-more-books.com/" -- service
      , "www.abebooks.com/" -- service
      , "www.thriftbooks.com/" -- service
      , "www.betterworldbooks.com/" -- service
      , "www.barnesandnoble.com/" -- service
      , "www.discoverbooks.com" -- service
      , "camelcamelcamel.com/" -- service
      , "1dollarscan.com/" -- service
      , "old.reddit.com/r/DataHoarder/" -- news/portal
      , "old.reddit.com/r/Piracy/" -- news/portal
      , "old.reddit.com/r/PrequelMemes/" -- news/portal
      , "old.reddit.com/r/emojipasta/" -- news/portal
      , "old.reddit.com/r/explorables/" -- news/portal
      , "cryptome.org" -- stable
      , "danluu.com" -- stable
      , "getlamp.com" -- stable
      , "vizhub.healthdata.org" -- interactive
      , "ai.googleblog.com" -- way too heavy
      , "blog.otoro.net" -- low quality: can't guarantee animations/interactions work via SingleFile
      , "quora.com" -- low quality: so heavily paywalled/blocked no point
      , "casual-effects.com" -- service/source code
      , "pcdb.santafe.edu" -- updated database
      , "unesdoc.unesco.org" -- low quality
      , "fifteen.ai/" -- service
      , "cran.r-project.org" -- stable
      , "mattlakeman.org" -- stable? we'll give him a chance
      , "gallica.bnf.fr" -- low quality/interactive
      , "find-more-books.com" -- service
      , "nicovideo.jp" -- service/videos
      , "darwin-online.org.uk" -- interactive documents
      , "ncase.me" -- interactive
      , "facebook.com" -- low quality (too JS-heavy, requires log-in etc)
      , "bellroy.com"
      , "mruniversity.com" -- videos
      , "imgur.com"
      , "proofofexistence.com" -- service
      , "sifter.org" -- service
      , "drmaciver.com" -- stable
      , "newegg.com" -- service
      , "worrydream.com" -- interactive/stable
      , "thiscardoesnotexist.glitch.me" -- service
      , "windowsphone.com" -- service/low quality
      , "accidentallyquadratic.tumblr.com" -- sometimes updated
      , "ohyouprettythings.com" -- service
      , "aurellem.org" -- low quality (YouTube links don't inline properly)
      , "www.iarpa.gov" -- stable?
      , "stlouisfed.org" -- service/stable (FRED economic statistics)
      , "apps.allenai.org" -- service/interactive
      , "bam-dataset.org" -- service/dataset
      , "streamable.com" -- service/video
      , "blockchain.info" -- service
      , "polecat.mascherari.press" -- dataset
      , "explorabl.es" -- interactive
      , "papers.ssrn.com" -- already blacklisted for manual archiving
      , "thisfursonadoesnotexist.com" -- interactive
      , "plato.stanford.edu" -- stable/bad archiving
      , "harney.com" -- service
      , "press.etc.cmu.edu" -- downloads/service
      , "tom7.org/mario/" -- stable
      , "orbis.stanford.edu" -- interactive
      , "www-cs-faculty.stanford.edu/~knuth/" -- stable
      , "www.cylab.cmu.edu" -- stable
      , "bibliophilly.library.upenn.edu" -- interactive
      , "fis.fda.gov" -- interactive
      , "www.genome.gov" -- stable
      , "dantelabs.com" -- service
      , "www.cdc.gov/nchs" -- service/data
      , "www.cdc.gov/ncbddd/" -- service/data
      , "folding.stanford.edu" -- stable
      , "www.writingroutines.com" -- stable/updating
      , "wolfewiki.com" -- stable
      , "www.vocativ.com/interactive/" -- interactive
      , "row1.ca" -- bad archiving?
      , "folkrnn.org" -- stable
      , "flatisjustice.moe/TADNE" -- service
      , "thecleverest.com" -- service
      , "inventingabstraction.tumblr.com" -- bad archiving
      , "www.moma.org" -- bad archiving
      , "www.e-codices.unifr.ch" -- bad archiving
      , "darkdata.bc.edu" -- interactive
      , "thisponydoesnotexist.net" -- interactive
      , "eurekamag.com" -- service
      , "rationality.org" -- stable
      , "www.thisworddoesnotexist.com" -- service
      , "substack.com" -- service
      , "gwern.substack.com" -- service
      , "jasoncrawford.org" -- stable
      , "progressstudies.school" -- service
      , "readonlymemory.vg" -- service
      , "hivemind-repo.s3-us-west-2.amazonaws.com/twdne3/" -- large binaries
      , "fursona.app" -- redirect to Google Colab
      , "old.reddit.com/r/reinforcementlearning/search" -- service
      , "https://old.reddit.com/r/reinforcementlearning/" -- stable
      , "usesthis.com" -- stable
      , "modafinil-store.com" -- dead
      , "https://huggingface.co/calculator/" -- interactive
      , "sevensecularsermons.org" -- stable
      , "old.reddit.com/r/NavySealCopypasta/" -- updated/service
      , "listudy.org" -- service
      , "playground.tensorflow.org" -- interactive
      , "ciechanow.ski" -- interactive, JS widgets didn't preserve right
      , "serendipityrecs.com" -- service
      , "discord.gg" -- service
      , "gptprompts.wikidot.com" -- updated/stable
      , "leaderboard.allenai.org" -- updated/low-quality archiving
      , "cdm16630.contentdm.oclc.org" -- low-quality archiving
      , "ergodox-ez.com" -- service/sales
      , "archive.foolz.us" -- blocks crawlers
      , "arxiv-vanity.com" -- mirror of stable site
      , "yalelawjournal.org" -- low-quality archiving (sidenotes don't work)
      , "www.theindiaforum.in" -- low-quality archiving (sidenotes don't work)
      , "knightcolumbia.org" -- low-quality archiving (sidenotes don't work)
      , "thefirstaibook.com" -- low-quality archiving
      , "tl.net/blogs/" -- low-quality archiving (screenshots/images break)
      , "metaculus.com" -- updated/service/stable
      , "simplify.so" -- service
      , "old.reddit.com/r/IncreasinglyVerbose" -- service/updated
      , "old.reddit.com/r/mlscaling/" -- service/updated
      , "old.reddit.com/r/Parahumans/" -- service/updated
      , "old.reddit.com/domain/gameprogrammingpatterns.com/" -- service/updated
      , "ctan.org/pkg/" -- stable
      , "magicemail.io" -- service
      , "parametric.press/issue-01/unraveling" -- doesn't archive the interactive right
      , "vision-explorer.allenai.org/" -- interactive service
      , "aidungeon.io" -- updated/interactive service
      , "www.tensorflow.org" -- stable/updated (technical documentation, better not point at potentially-outdated archives)
      , "bit-player.org" -- low-quality (interactive JS widgets didn't preserve right)
      , "www.themoneyillusion.com" -- low-quality
      , "cedar.wwu.edu/cedarbooks/4/" -- stable
      , "krebsonsecurity.com" -- stable
      , "buttercupfestival.com" -- stable
      , "www.cna.org" -- landing/homepage, not useful to archive
      , "www.caniuse.com" -- updated service
      , "archiveofourown.org" -- blocks archiving
      -- Nitter mirrors seem to be setting X headers to block live popups, but linkrot & uptime remains a problem, so time to start mirroring them:
      -- , "nitter.net/search" -- example/updated
      -- , "nitter.hu/search" -- example/updated
      -- , "nitter.hu" -- service/mirror, low quality (videos don't save)
      , "flashgamehistory.com" -- low quality
      , "thebrowser.com" -- paywall
      , "git.sr.ht" -- service/updated
      , "b-ok.cc/" -- service
      , "www.nap.edu/catalog" -- low quality
      , "store.steampowered.com/app/" -- service
      , "www.traditionalmedicinals.com/" -- homepage
      , "hiyo.jp" -- service/defunct
      , "wkhtmltopdf.org" -- stable
      , "www.sumatrapdfreader.org" -- stable
      , "tradejournalcooperative.com" -- service
      , "www.openphilanthropy.org" -- stable
      , "alignmentforum.org" -- stable
      , "forum.effectivealtruism.org" -- stable
      , "www.effectivealtruism.org" -- stable
      , "www.preclinicaltrials.eu" -- service
      , "disease-connect.org" -- service
      , "chrome.google.com/webstore/" -- service
      , "microsoftedge.microsoft.com/addons/" -- service
      , "magenta.tensorflow.org/music-transformer" -- low quality
      , "magenta.tensorflow.org/maestro-wave2midi2wave" -- low quality
      , "magenta.tensorflow.org/piano-transformer" -- low quality
      , "e621.net" -- service/porn
      , "www.ctan.org" -- stable
      , "ideas.repec.org/p/uea/" -- stable (preprint server)
      , "freakonomicsexperiments.com" -- service
      , "apps.chiragjpgroup.org/catch/" -- service (interactive stats)
      , "fdaaa.trialstracker.net/" -- service (interactive stats)
      , "rottenlibrary.net" -- redundant (mirror of my rotten.com mirror)
      , "www.justinpinkney.com" -- stable
      , "ssb.fit" -- service
      , "course.fast.ai/videos/" -- low quality
      , "vk.com/alexeyguzey" -- stable
      , "paulfchristiano.com" -- stable
      , "ought.org" -- homepage
      , "theory.cs.berkeley.edu/" -- homepage
      , "ai-alignment.com" -- stable?
      , "sideways-view.com" -- stable
      , "jakewestfall.org/ivy/" -- interactive
      , "findclone.ru" -- service
      , "reddit.com/user/shawwwn" -- updated/profile
      , "www.projectvesta.org" -- homepage
      , "pygments.org" -- homepage
      , "test.mensa.no" -- service
      , "www.queendom.com/tests/" -- service
      , "brainturk.com" -- service
      , "www.courtlistener.com/" -- stable
      , "console.cloud.google.com" -- cloud service requiring login
      , "imsdb.com" -- homepage
      , "www.reg.ru/blog/anime-generation-with-stylegan/" -- unnecessary (mirror of /Faces)
      , "catscan2.toolforge.org" -- interactive/service
      , "www.mangafox.com/manga" -- service/pirated
      , "highlightcam.co.nz" -- homepage
      , "michaelnielsen.org" -- stable/homepage
      , "juliagalef.com" -- stable/homepage
      , "opensnp.org" -- service/database
      , "tzstamp.io" -- service
      , "ggplot2.tidyverse.org" -- homepage
      , "fantasticanachronism.com" -- stable
      , "coderelay.io/fontemon.html" -- interactive
      , "www.speedtest.net" -- service
      , "okmij.org" -- stable
      , "packdeps.haskellers.com" -- service
      , "meyerweb.com/eric/tools/" -- service
      , "bifunctor.homelinux.net" -- service
      , "paste.hskll.org" -- service
      , "tuftandneedle.com" -- homepage
      , "www.microcovid.org" -- service
      , "grantland.com/features/" -- stable
      , "instant.page" -- stable
      , "support.google.com" -- updated
      , "competicionmental.appspot.com" -- app/game/interactive
      , "lair.lighton.ai/akronomicon/" -- updated
      , "public.tableau.com" -- interactive visualizations
      , "andyljones.com" -- stable? give a chance
      , "mlcommons.org" -- organization/homepage
      , "eleuther.ai" -- stable
      , "ieeexplore.ieee.org/abstract/" -- bad quality
      , "arankomatsuzaki.wordpress.com" -- stable
      , "www.fimfiction.net" -- stable/homepage
      , "gaussianbp.github.io" -- interactive
      , "bitcoinbook.cs.princeton.edu" -- homepage
      , "theme.typora.io" -- stable/updated source code/docs
      , "sigbovik.org/" -- stable/homepage
      , "sociologicalscience.com" -- stable (preprint host)
      , "eugenewei.com" -- stable
      , "h01-dot-neuroglancer-demo.appspot.com" -- interactive
      , "dawn.cs.stanford.edu/benchmark/" -- updated database/benchmark
      , "super.gluebenchmark.com" -- updated database/benchmark
      , "lab.aminer.cn" -- interactive/demo
      , "www.rte.ie/archives/" -- video
      , "geroprotectors.org" -- updated database
      , "www.satcompetition.org" -- updated database
      , "endlessvn.io/" -- homepage
      , "readthedocs.io" -- generally stable, updated docs
      , "blog.codinghorror.com" -- stable
      , "teanobi.com" -- homepage
      , "scp-wiki.wikidot.com" -- stable/updated
      , "programmablesearchengine.google.com" -- service
      , "library.bz/" -- Libgen
      , "wellcomecollection.org/works/" -- low-quality
      , "www.reddit.com/r/mlscaling/search" -- search/updated
      , "thechineseteashop.com/" -- homepage
      , "teahabitat.com/" -- homepage
      , "raganwald.com" -- stable
      , "aclanthology.org" -- stable
      , "ml.berkeley.edu/blog/" -- low-quality - redirects?
      , "advertising-effects.chicagobooth.edu/" -- interactive
      , "carbonplan.org/research/forest-offsets" -- interactive
      , "www.avclub.com/the-100-best-worst-and-weirdest-things-we-saw-on-the-1839566367" -- too much media
      , "https://ww2.arb.ca.gov/our-work/programs/cap-and-trade-program" -- homepage
      , "https://egamebook.com/lochness/" -- interactive
      , "https://sites.research.google/trc/about/" -- homepage
      , "https://realbotix.com/" -- homepage
      , "https://www.grillitype.com/" -- homepage
      , "https://www.royalroad.com/fiction/" -- stable
      , "https://www.lipreading.org/" -- homepage
      , "https://research.google/pubs/" -- stable
      , "https://www.tabnine.com/" -- homepage
      , "https://www.alphagomovie.com/" -- stable
      , "https://pone.dev/" -- interactive
      , "https://www.luckyscent.com/" -- homepage
      , "https://pioneer.app/" -- homepage
      , "dev.kanotype.net:8003/deepdanbooru/" -- interactive/service
      , "https://app.inferkit.com/demo"  -- interactive/service
      , "https://animetudes.com" -- low quality (media)
      , "https://icar-project.com/" -- homepage
      , "https://discord.com/invite/" -- service
      , "https://same.energy/" -- interactive/service
      , "https://old.reddit.com/r/MediaSynthesis/" -- low-quality due to Imgur/image embeds
      , "proceedings.mlr.press/" -- stable
      , "https://absa.org/" -- homepage
      , "https://www.agriapet.co.uk/" -- homepage
      , "www.word.golf" -- interactive game
      , "https://demo.allennlp.org" -- interactive demos
      , "https://www.cancerresearchuk.org/about-us" -- homepage
      , "mymodafinil.net" -- dead
      , "https://free.law/recap" -- homepage
      , "https://dominicfrisby.com/" -- homepage
      , "https://forum.effectivealtruism.org" -- stable
      , "https://old.reddit.com/r/hangovereffect/" -- homepage
      , "https://bwc.thelab.dc.gov/" -- low quality
      , "https://www.trubrain.com/" -- homepage
      , "https://energycontrol.org/quienes-somos/proyectos/" -- homepage
      , "https://www.teaandabsinthe.com/" -- homepage
      , "https://www.limeadery.com/" -- homepage
      , "https://taooftea.com/" -- homepage
      , "https://ooo.ghostbows.ooo/" -- interactive/sound
      , "https://www.jetbrains.com/lp/mono/" -- low quality
      , "linusakesson.net" -- low quality (YT embed breaks)
      , "https://willcrichton.net/nota/" -- interactive
      , "https://trixter.oldskool.org/" -- low quality (YT embed breaks)
      , "http://www.michaelburge.us/2019/05/21/marai-agent.html" -- low quality (YT embed breaks)
      , "https://pol.is/home" -- homepage
      , "http://images.google.com/images" -- interactive (search engine query)
      , "https://danijar.com/project/apd/" -- video embed
      , "http://lispm.de/symbolics-lisp-machine-ergonomics" -- video embed
      , "https://boardgamegeek.com" -- stable
      , "https://dl.acm.org/doi/10.1145/3266037.3266090" -- Open Access
      , "https://laion.ai/laion-400-open-dataset/" -- video embed
      , "https://wy-lang.org/" -- homepage
      , "https://ali-design.github.io/gan_steerability/" -- video embed
      , "https://github.com/" -- stable
      , "https://xbpeng.github.io/projects/VDB/index.html" -- video embed
      , "https://news.ycombinator.com/newest" -- updated
      , "https://news.ycombinator.com/news" -- updated
      , "http://www.packomania.com/" -- low quality/updated
      , "https://metarationality.com/rational-pcr" -- video embed
      , "https://ashish-kmr.github.io/rma-legged-robots/" -- video embed
      , "https://www.clinicaltrialsregister.eu/ctr-search/search" -- interactive
      , "https://imagelibrary.bgu.ac.il/pf.tlx/O6ORSOx-nut" -- already mirrored locally
      , "https://wudao.aminer.cn/CogView/index.html" -- interactive
      , "https://tfhub.dev/google/collections/gtr/1" -- source code/docs/updated
      , "https://universome.github.io/stylegan-v" -- video embed
      , "https://openaipublic.blob.core.windows.net/webgpt-answer-viewer/index.html" -- interactive
      , "http://recur-env.eba-rm3fchmn.us-east-2.elasticbeanstalk.com/" -- interactive
      , "http://recur-env.eba-rm3fchmn.us-east-2.elasticbeanstalk.com/" -- interactive
      , "https://pandoc.org/" -- homepage
      , "https://caniuse.com/" -- updated
      , "https://www.vesta.earth/" -- homepage
      , "https://www.janelia.org/project-team/flyem/hemibrain" -- video embeds
      , "https://seegrid.com/" -- homepage
      , "https://www.sixdegreesofwikipedia.com/" -- interactive
      , "https://rdiff-backup.net/" -- homepage
      , "https://conifer.rhizome.org/" -- homepage
      , "https://manifold.markets/" -- homepage
      , "https://highnoongmt.wordpress.com/" -- homepage
      , "https://fanfox.net/manga/oyasumi_punpun/v08/c084/15.html" -- blocks mirroring
      , "https://mru.org/development-economics" -- video embeds
      , "https://www.pluralsight.com/" -- interactive
      , "https://www.sapa-project.org/" -- homepage
      , "https://www.cram.com/topics/popular" -- updated
      , "https://www.cram.com" -- homepage
      , "https://knowyourmeme.com/memes/" -- stable/updated
      , "https://wellcomecollection.org/" -- low quality
      , "https://cognitivemedium.com/" -- stable
      , "https://arxiv.org/abs/quant-ph/" -- these really old pre-2007 (https://arxiv.org/help/arxiv_identifier) Arxiv papers don't work with Ar5iv, so we'll exempt them from the archive+rewrite process
      , "https://arxiv.org/abs/math/"
      , "https://arxiv.org/abs/physics/"
      , "https://arxiv.org/abs/cond-mat/"
      , "https://arxiv.org/abs/cs/"
      , "https://sebastianrisi.com/self_assembling_ai/" -- video embeds
      , "https://brainwindows.wordpress.com/2009/10/14/playing-quake-with-a-real-mouse/" -- video embed
      , "https://medium.com/mindsoft/rats-in-doom-eb6c52c73aca" -- video embed
      , "https://podcasts.google.com/feed/aHR0cHM6Ly9yc3MuYWNhc3QuY29tL2Rhbm55aW50aGV2YWxsZXk/episode/MDI4NDI4ODMtZmE3YS00MzA2LTk1ZGItZjgzZDdlMzAwZThk" -- audio embed
      , "https://jdlm.info/articles/2018/03/18/markov-decision-process-2048.html" -- interactive solver
      , "https://www.dailydot.com/crime/hive-silk-road-online-drug-culture-history/" -- video embed
      , "ar5iv.labs.arxiv.org" -- HTML mirror of Arxiv PDFs, regularly updated to improve compilation quality, and allows live popups so no need to mirror locally
      , "digital.library.unt.edu/explore/partners/" -- search
      , "worldcat.org" -- updated
      , "tinyurl.com" -- redirects
      , "www.character.ai" -- homepage
      , "ageofem.com" -- low quality (video embeds)
      , "https://mujoco.org/" -- homepage
      , "https://sites.google.com/view/mbmf" -- low quality (video embeds)
      , "https://pathak22.github.io/large-scale-curiosity/" -- low quality (video embeds)
      , "https://adversarialpolicies.github.io/" -- low quality (video embeds)
      , "https://svito-zar.github.io/gesticulator/" -- low quality (video embeds)
      , "https://pearl-insertion.github.io/" -- low quality (video embeds)
      , "https://ramanans1.github.io/plan2explore/" -- low quality (video embeds)
      , "https://wilson1yan.github.io/videogpt/index.html" -- low quality (video embeds)
      , "https://repurposegans.github.io/" -- low quality (video embeds)
      , "https://adapterhub.ml/" -- homepage
      , "https://www.megaverse.info/" -- low quality (video embeds)
      , "https://attentionneuron.github.io/" -- interactive
      , "https://es-clip.github.io/" -- low quality (video/animated embeds)
      , "https://sites.google.com/view/mbrl-amortization/home" -- low quality (video embeds)
      , "https://sites.google.com/view/mend-editing" -- low quality (animated embeds)
      , "https://energy-based-model.github.io/comet/" -- low quality (video embeds)
      , "https://huggingface.co/spaces/" -- interactive
      , "https://www.arknights.global/" -- homepage
      , "http://arborjs.org/" -- homepage
      , "https://nitter.hu/OpenAI" -- PR account
      , "https://songweige.github.io/projects/tats/index.html" -- video
      , "https://self-distilled-stylegan.github.io/" -- low quality (video embeds)
      , "https://www.flavorwire.com/415737/5-of-the-most-scandalous-affairs-in-literary-history" -- fails to archive, as does https://story.californiasunday.com/cosmic-crisp-apple-launch/ https://www.outsideonline.com/health/training-performance/inside-look-surprisingly-violent-quidditch-world-cup/ https://www.technologyreview.com/2020/02/17/844721/ai-openai-moonshot-elon-musk-sam-altman-greg-brockman-messy-secretive-reality/
      , "https://semantle.com/" -- interactive game
      , "https://agc.platform.baai.ac.cn/CogView/index.html" -- interactive demo
      , "https://unminify.com/" -- interactive service
      , "https://www.bulletproof.com/" -- homepage
      , "https://www.google.com/alerts" -- interactive service
      , "http://darwintunes.org/" -- SoundCloud music sound embed doesn't preserve
      , "https://danlark.org/2022/04/20/changing-stdsort-at-googles-scale-and-beyond/" -- low quality: video embeds
      , "http://relative-attention-music-nips-2018.s3-website-us-west-1.amazonaws.com/" -- low quality: sound embeds
      , "https://sites.google.com/view/nips2017-one-shot-imitation/home" -- low quality (video embeds)
      , "https://www.flickchart.com/" -- interactive service/homepage
      , "https://sites.google.com/view/hybridmrca" -- low quality (video embeds)
      , "https://roboturk.stanford.edu/realrobotdataset" -- low quality (video embeds)
      , "https://wenlong.page/modular-rl/" -- low quality (video embeds)
      , "https://retinagan.github.io/" -- low quality (entire page broken?)
      , "https://speechresearch.github.io/deepsinger/" -- low quality (sound embeds)
      , "https://sites.google.com/view/efficient-robotic-manipulation" -- low quality (video embeds)
      , "https://next-week-tonight.github.io/NWT/" -- low quality (video embeds)
      , "https://magenta.tensorflow.org/perceiver-ar" -- low quality (audio embeds)
      , "https://hazyresearch.stanford.edu/sashimi-examples/" -- low quality (audio embeds)
      , "https://speechresearch.github.io/naturalspeech/" -- low quality (audio embeds)
      , "https://keithito.com/LJ-Speech-Dataset/" -- low quality (audio embeds)
      , "https://carolineec.github.io/informative_drawings/" -- low quality (video embeds)
      , "https://parti.research.google/" -- low-quality (initial image carousel doesn't work)
      , "https://unconventionality.github.io/" -- low-quality (interactive app breaks)
      , "https://elicit.org/" -- homepage
      , "https://yzqin.github.io/dexmv/" -- low-quality (video, GIF embeds break)
      , "https://nitter.hu/hausman_k/status/1511152160695730181" -- low quality (video embeds)
      , "https://say-can.github.io/" -- low quality (video embeds)
      , "https://wenlong.page/language-planner/" -- low quality (video embeds)
      , "https://patents.google.com/?inventor" -- interactive
      , "https://sites.google.com/view/ving-robot" -- low quality (video embeds)
      , "https://agility.csail.mit.edu/" -- low quality (video embeds)
      , "https://plai.cs.ubc.ca/2022/05/20/flexible-diffusion-modeling-of-long-videos/" -- low quality (video embeds)
      , "https://models.aminer.cn/cogvideo/" -- interactive service
      , "https://nitter.hu/JeanRemiKing/status/1533720262344073218" -- low quality (video embeds)
      , "https://danijar.com/project/director/" -- low quality (video embeds)
      , "https://bigvgan-demo.github.io/" -- low quality (audio embeds)
      , "https://gist.github.com/brockmanmatt/7265297f21634693868c2aad9d2c5919" -- Github iPython notebook - always fail to render for me
      , "https://gist.github.com/brockmanmatt/deafb4dba7e4399327e44f2c8fd97b2b" -- Github iPython notebook - always fail to render for me
      , "https://sites.google.com/berkeley.edu/fleet-dagger/home" -- low quality (video embeds)
      , "https://danijar.com/project/daydreamer/" -- low quality (video embeds)
      , "https://sites.google.com/view/lmnav" -- low quality (video embeds)
      , "https://innermonologue.github.io/" -- low quality (video embeds)
      , "https://salu133445.github.io/mtmt/" -- low quality (audio embeds)
      , "https://paddlehelix.baidu.com/app/drug/protein" -- interactive
      , "https://semantic-abstraction.cs.columbia.edu/" -- low quality (video embeds)
      , "https://nuwa-infinity.microsoft.com/" -- low quality (video embeds)
      , "https://celebv-hq.github.io/" -- low quality (video embeds)
      , "https://baghunter.com/" -- homepage
      , "https://www.talkrl.com/episodes/" -- low quality (audio embeds)
      , "https://www.ultimagenomics.com/" -- homepage
      , "http://tweet.onerandom.com/" -- randomized
      , "https://google.github.io/deepmac/" -- low quality (video embeds)
      , "https://semantle.pimanrul.es/" -- interactive
      , "https://labelerrors.com/" -- interactive
      , "https://novelai.net/" -- homepage
      , "https://well-typed.com/" -- homepage
      , "https://everynoise.com/engenremap.html" -- interactive
      , "https://jamiepinheiro.com/cyclic_tv_reference_paradox_finder/" -- interactive
      , "https://practiceoftheory.weebly.com/a-causal-models-probability-of-being-true.html" -- interactive
      , "https://wenxin.baidu.com/moduleApi/ernieVilg" -- interactive
      , "https://ai-muzic.github.io/meloform/" -- low quality (audio embeds)
      , "https://www.webdesignmuseum.org/" -- homepage
      , "https://minerva-demo.github.io/#category=Algebra&index=1" -- interactive
      , "https://yashkant.github.io/housekeep/" -- low quality (video embeds)
      , "https://google-research.github.io/seanet/audiolm/examples/" -- low quality (audio embeds)
      , "https://www.painreprocessingtherapy.com/" -- homepage / low quality (video embeds)
      , "https://namuol.github.io/banal-duck/" -- interactive
      , "https://psych.hanover.edu/JavaTest/CLE/Cognition/Cognition/MentalRotation.html" -- interactive (Java applet!)
      , "https://sites.google.com/view/multi-agent-competition" -- low quality (video embeds)
      , "https://webdiplomacy.net/" -- homepage
      , "https://www.mmlab-ntu.com/project/vtoonify/" -- low quality (video embeds)
      , "https://makeavideo.studio/#facebook" -- low quality (video embeds)
      , "https://www.storylive.com/main.htm" -- interactive
      , "https://sites.google.com/view/projected-gan/" -- low quality (video embeds)
      , "https://diffusion-planning.github.io/"  -- low quality (video embeds)
      , "https://emalmi.kapsi.fi/edit5_code.html" -- low quality (placeholder for future updates)
      ] = True
    | otherwise = False
