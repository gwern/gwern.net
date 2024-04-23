 {- LinkLive.hs: Specify domains which can be popped-up "live" in a frame by adding a link class.
Author: Gwern Branwen
Date: 2022-02-26
When:  Time-stamp: "2024-04-23 10:47:20 gwern"
License: CC-0

Based on LinkIcon.hs. At compile-time, set the HTML class `link-live` on URLs from domains verified
to work (reasonably) well as cross-site popups inside a frame.
`extracts-contents.js` at runtime reads the class to decide which links will be live-popup-able.

Live popups are an alternative to, or a further step from, annotations. They let the reader preview
a link instantly. This is useful when an annotation is not available, or when the reader has read
the annotation and wants to go further.

However, due to the March of Web Progress™, many websites set X headers <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options>
or just plain don't work in a frame (often due to JS, and then often due to extremely reader-unfriendly design
like giant headers or stickies), or are 'mixed content' - that is, the HTTP (or HTTPS) version would work perfectly
*except browsers block it* in the name of security, so a reader who visits HTTPS Gwern.net can't load the HTTP version
and I have to test the HTTPS version instead, which even when it exists, will often be broken by things like
self-signed certificates etc.
Perhaps only a quarter of external links work as live popups.
So we can't just offer it as an option on all links, that will waste reader time & trust, and they will
learn to avoid the feature entirely and resent the visual clutter and trap of this 'feature'.

We instead whitelist domains based on manual testing using the list of links in /lorem#live-link-popups.
Since there are so many domains, we need a testsuite to keep track of what domains have been tested & found good,
tested & found bad, and testing is not done or ambiguous (due to practical issues like a test link having become
a local archive or 404 or changed domain entirely).

Finally, to keep up to date with new domains, each sync we rank domains by # of uses, and above a threshold,
automatically generate a live-link test-case appended to /lorem for manual review.

For an independent JS NPM library implementation, see <https://github.com/Stvad/link-summoner>.
-}

{-# LANGUAGE OverloadedStrings #-}
module LinkLive (linkLive, linkLiveString, alreadyLive, linkLiveTest, linkLiveTestHeaders, urlLive, linkLivePrioritize) where

import Control.Monad (forM_, when, unless)
import Data.Char (toLower)
import Data.List (intersect, isInfixOf, sort)
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as M (fromListWith, toList, map, keys)
import qualified Data.Text as T (append, isInfixOf, isPrefixOf, pack, unpack, Text)
import Data.Text.IO as TIO (appendFile)
import Text.Pandoc (Inline(Link), nullAttr)
import Data.FileStore.Utils (runShellCommand)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import System.Exit (ExitCode(ExitFailure))

import Interwiki (wpPopupClasses)
import LinkBacklink (readBacklinksDB, Backlinks)
import Utils (addClass, hasClass, host, anySuffixT, printRed, anyInfixT, ensure, isURLT)
import qualified Config.LinkLive as C
import qualified Config.Misc as CM (userAgent)

linkLive :: Inline -> Inline
linkLive x@(Link (_,cl,kvs) _ (u, _))
 | "link-live-not" `elem` cl = x
 | u `elem` overrideLinkLive = aL x
 | "data-url-archive" `elem` map fst kvs = aL x -- if a link has a local-archive, we can always pop up the local mirror instead
 | "http://" `T.isPrefixOf` u   = x -- WARNING: no HTTP page can be live-link loaded by a browser visiting HTTP*S*-only Gwern.net due to 'mixed security context'
 | "/"    `T.isPrefixOf` u   = x -- local links shouldn't match anything, but to be safe, we'll check anyway.
 | otherwise = case urlLive u of
                 Just True -> aL x
                 _         -> x
 where aL :: Inline -> Inline
       aL = addClass "link-live"
linkLive x = x

alreadyLive :: Inline -> Bool
alreadyLive = hasClass "link-live"

linkLiveString :: String -> Inline
linkLiveString u = linkLive (Link nullAttr [] (T.pack u,""))

-- hardwire URLs which should be live
overrideLinkLive :: [T.Text]
overrideLinkLive = []

-- Nothing = unknown/untested; Just True = known good; Just False = known bad
-- precedence for overrides: bad {simple, sub} > good {simple, sub} > WP > misc
urlLive :: T.Text -> Maybe Bool
urlLive u | u'     `elem`   C.badDomainsSimple  = Just False
          | anySuffixT u'   C.badDomainsSub     = Just False
          | u'      `elem`  C.goodDomainsSimple = Just True
          | anySuffixT u'   C.goodDomainsSub    = Just True
          | anyInfixT u C.wikipediaURLs  = wikipedia u
          | otherwise = C.miscUrlRules u
   where u' = host u

linkLivePrioritize :: IO [(Int, T.Text)]
linkLivePrioritize = do b <- readBacklinksDB
                        let b' = M.toList $ M.map length b
                        let b'' = map (\(c,d) -> (host c,d)) $ filter (\(url',count) -> count >= C.linkLivePrioritizeMinimum &&
                                                                                       not ("pdf" `T.isInfixOf` url' || "PDF" `T.isInfixOf` url' || ".ps" `T.isInfixOf` url') &&
                                                                                       host url' `notElem` C.linkLivePrioritizeBlacklist &&
                                                                                       (isNothing . urlLive) url' &&
                                                                                       ("." `T.isInfixOf` url')) b'
                        let b''' =  M.fromListWith (+) b''
                        let hits = reverse $ sort $ Prelude.filter ((/="") . snd) $ map (\(e,f) -> (f,e)) $ M.toList b'''
                        unless (null hits) $ mapM_ (\(_,l) -> writeLinkLiveTestcase b l) hits
                        return hits
  where
        -- Append an example of a prioritized link to /lorem#link-testcases for manual review, to skip copy-paste hassle
        writeLinkLiveTestcase :: Backlinks -> T.Text -> IO ()
        writeLinkLiveTestcase b l = let link = head $ filter (l `T.isInfixOf`) $ M.keys b in -- take the first URL which matches the domain:
                                      TIO.appendFile C.testPage $ "\n- <" `T.append` link `T.append` ">{.archive-not .link-annotated-not .link-live}" -- NOTE: we explicitly disable any annotation with `.link-annotated-not` to ensure it pops up as a live link the first time, to save us a little effort when reviewing

-- Wikipedia link-live capabilities are page-dependent: anything in the Special namespace is blocked by headers (which makes sense given how many queries/capabilities are inside it). But it looks like pretty much all other namespaces (see Interwiki.hs's nonArticleNamespace for a list) should be live?
wikipedia :: T.Text -> Maybe Bool
wikipedia u = Just $ "link-live" `elem` wpPopupClasses u

url :: T.Text -> Inline
url u = linkLive (Link nullAttr [] (u,""))

-- URLs which fail their rule test:
linkLiveTest :: [(T.Text,Bool)]
linkLiveTest = filter (\(u, bool) -> bool /=
                                       (url u == Link ("",["link-live"], []) [] (u,""))
                      )
               linkLiveTestUnits

  ++ overlapping
  where overlap xs ys = zip (xs `intersect` ys) (repeat False)
        overlapping = concat [
          overlap C.goodDomainsSub C.badDomainsSub,
          overlap C.goodDomainsSimple C.badDomainsSimple,
          overlap C.goodLinks C.badLinks
          ]

-- check the live test-cases with curl for `X-Frame` HTTPS headers; the presence of these guarantees liveness no longer works and they need to be updated.
linkLiveTestHeaders :: IO ()
linkLiveTestHeaders = forM_ C.goodLinks
  (\u -> do (status,_,bs) <- runShellCommand "./" Nothing "curl" ["--compressed", "--insecure", "--user-agent", CM.userAgent, "--location", "--silent", "--head", T.unpack u]
            case status of
                ExitFailure _ -> printRed "Error: curl download failed on URL " >> print (T.unpack u ++ " : " ++ show status ++ " : " ++ show bs)
                _ -> do let s = map toLower $ U.toString bs
                        when ("x-frame" `isInfixOf` s && not ("x-archive-orig-x-frame-options" `isInfixOf` s)) $ -- IA preserves the original FRAME for completeness, but it doesn't count for breaking live links, so we need to avoid false positives from string-matching on it
                          printRed (T.unpack u) >> print (" : X-FRAME option detected on URL : " ++ show bs)
  )

linkLiveTestUnits :: [(T.Text,Bool)]
linkLiveTestUnits = map (\u -> (u,True)) good ++
                    map (\u -> (u,False)) bad
  where good = map (\u -> if "https://" `T.isPrefixOf` u then u else error "LinkLive.linkLiveTestUnits.good.goodLinks: 'http'-only link detected; due to cross-site browser requirements, it is impossible to pop up a HTTP-only web page from the HTTPS Gwern.net pages; therefore this cannot be a 'good link'.")
          $ ensure "goodLinks" "isURL" isURLT C.goodLinks
        bad  = ensure "badLinks"  "isURL" isURLT C.badLinks
