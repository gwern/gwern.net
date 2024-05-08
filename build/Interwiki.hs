{-# LANGUAGE OverloadedStrings #-}
module Interwiki (convertInterwikiLinks, convertInterwikiLinksInline, wpPopupClasses, interwikiTestSuite, interwikiCycleTestSuite, isWPDisambig, escapeWikiArticleTitle, toWikipediaEnURL) where

import Data.List (isInfixOf, intersect)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map as M (fromList, lookup, Map)
import qualified Data.Text as T (append, head, isInfixOf, null, tail, take, toUpper, pack, unpack, Text, isPrefixOf, isSuffixOf, takeWhile, init, replace)
import Network.URI (parseURIReference, uriPath, uriAuthority, uriRegName)
import qualified Network.URI.Encode as E (encodeTextWith, isAllowed)

import Text.Pandoc (Inline(..), Pandoc)
import Text.Pandoc.Walk (walk)

import Cycle (isCycleLess, findCycles)
import Utils (replaceManyT, anyPrefixT, fixedPoint, inlinesToText)
import qualified Config.Interwiki as C (redirectDB, quoteOverrides, testCases)

import Network.HTTP.Simple (parseRequest, httpLBS, getResponseBody, Response) -- http-conduit
import qualified Data.ByteString.Lazy.UTF8 as U (toString, ByteString)
import Control.Exception (catch, SomeException)
-- import Network.HTTP.Client (Response)

-- if there is an English WP article, is it a disambiguation page? (we generally want to avoid linking to those)
-- use curl to call the WP API and (to avoid complicated JSON processing overhead) simply look for the fixed string '"type":"disambiguation"', and return Just True/False.
-- While if there is apparently no article at all, return `Nothing` (as callers may need to treat non-existent WP articles differently from disambig WP articles).
-- Bash shell equivalent: `API_RESPONSE=$(curl --silent "https://en.wikipedia.org/api/rest_v1/page/summary/$(basename "$URL")"); if [[ $API_RESPONSE == *'"type":"disambiguation"'* ]]; then echo "Warning: $URL is a disambiguation page."`
isWPDisambig :: T.Text -> IO (Maybe Bool)
isWPDisambig articleName = do
  let encodedArticleName = escapeWikiArticleTitle articleName
  let url = "https://en.wikipedia.org/api/rest_v1/page/summary/" `T.append` encodedArticleName
  request <- parseRequest (T.unpack url)
  result <- catch (Right <$> httpLBS request) handleException :: IO (Either String (Response U.ByteString))
  case result of
    Left _ -> return Nothing  -- On any exception, ignore error message & return Nothing
    Right response -> return $
      let responseBody = U.toString $ getResponseBody response
      in if "Not found" `isInfixOf` responseBody
         then Nothing
         else Just ("\"type\":\"disambiguation\"" `isInfixOf` responseBody)

-- Exception handler for all exceptions
handleException :: SomeException -> IO (Either String (Response U.ByteString))
handleException _ = return $ Left "An exception occurred"

toWikipediaEnURL :: T.Text -> T.Text
toWikipediaEnURL title = "https://en.wikipedia.org/wiki/" `T.append` escapeWikiArticleTitle title

escapeWikiArticleTitle :: T.Text -> T.Text
escapeWikiArticleTitle title = E.encodeTextWith (\c -> (E.isAllowed c || c `elem` [':','/', '(', ')', ',', '#', '+'])) $
                               replaceManyT [("–", "%E2%80%93"), ("\"", "%22"), ("[", "%5B"), ("]", "%5D"), ("%", "%25"), (" ", "_")] $
                               deunicode title
    where deunicode :: T.Text -> T.Text
          deunicode = replaceManyT [("‘", "\'"), ("’", "\'"), (" ", " "), (" ", " "), (" ", "")]

-- INTERWIKI PLUGIN
-- This is a simplification of the original interwiki plugin I wrote for Gitit: <https://github.com/jgm/gitit/blob/master/plugins/Interwiki.hs>
-- It's more or less the same thing, but the interwiki mapping is cut down to only the ones I use, and it avoids a dependency on Gitit.

-- wrap `convertInterwikiLinksInline` with its document-level context for error-reporting purposes. It is too difficult to debug errors like empty links (eg. a `<a href="!W"></a>`) when you have no idea where they are located, and the empty link, almost by definition, has no information about itself & can be quite hard to search for (especially if it's generated or in an intermediate).
convertInterwikiLinks :: Pandoc -> Pandoc
convertInterwikiLinks doc = walk (convertInterwikiLinksInline doc) doc

-- BUG: Escaping bugs with Unicode: eg. [Pāli Canon](!W) / <https://en.wikipedia.org/wiki/P%C4%81li_Canon>
-- but if I simply Network.HTTP.urlEncode the article, that breaks a lot of other stuff (like colons in namespaces)...? What *is* the right way to escape/encode WP article names?
convertInterwikiLinksInline :: Pandoc -> Inline -> Inline
convertInterwikiLinksInline doc x@(Link _ []           _) = error $ "Link error (convertInterwikiLinksInline): no anchor text‽ " ++ show x ++ " : " ++ show doc
convertInterwikiLinksInline _ x@(Link _ _ ("", _))        = x
convertInterwikiLinksInline _ x@(Link (ident, classes, kvs) ref (interwiki, article)) =
  if not (T.null article) && T.head article == ' ' then error $ "Link error (convertInterwikiLinksInline): tooltip malformed with excess whitespace? " ++ show x else
  if T.head interwiki == '!' then if article/="" && (T.head article == '$' || T.head article == '₿') then error $ "Interwiki.convertInterwikiLinksInline called with accidental inflation-adjustment amount instead? " ++ show x else
        case M.lookup (T.tail interwiki) interwikiMap of
                Just url  -> let attr' = (ident,
                                            nubOrd (wpPopupClasses (url `interwikiurl` (if article=="" then inlinesToText ref else article)) ++
                                            classes),
                                           kvs) in
                             case article of
                                  -- NOTE: only in cases of displayed text do we want to run the transformations like deleting possessives.
                                  -- So eg. `[George Washington's](!W)` is automatically transformed to 'George Washington' but if we explicitly write `[George Washington](!W "George Washington's")`, then we respect the user override because there must be a reason for it.
                                  "" -> Link attr' ref (url `interwikiurl` wpURLRewrites (inlinesToText ref), "") -- tooltip is now handled by LinkMetadata.hs
                                  _  -> Link attr' ref (url `interwikiurl` article, "")
                Nothing -> error $ "Attempted to use an interwiki link with no defined interwiki: " ++ show x
  else let classes' = nubOrd (wpPopupClasses interwiki ++ classes) in
         if ".wikipedia.org/wiki/" `T.isInfixOf` interwiki || ".wikipedia.org/w/index.php" `T.isInfixOf` interwiki then
           Link (ident, classes', kvs) ref (wpURLRedirectRewrites interwiki, article)
              else x
  where
    interwikiurl :: T.Text -> T.Text -> T.Text
    -- normalize links; MediaWiki requires first letter to be capitalized, and prefers '_' to ' '/'%20' for whitespace
    interwikiurl "" _ = error (show x)
    interwikiurl _ "" = error (show x)
    interwikiurl u a = let a' = if ".wikipedia.org/wiki/" `T.isInfixOf` u then T.toUpper (T.take 1 a) `T.append` T.tail a else a
                       in
                         fixedPoint wpURLRedirectRewrites $ u `T.append` escapeWikiArticleTitle a'
convertInterwikiLinksInline _ x = x

-- special case rewrites: for example, automatically rewrite anchor texts ending in "'s" to delete it (eg. "George Washington's" to "George Washington") if it is not a special-case where that is part of the official name (eg. "Antoine's"). This makes writing much easier because you can simply write '[George Washington's](!W) first act as president was' instead of ''[George Washington's](!W "George Washington") first act...'. This sort of possessive rewriting gets especially annoying in long runs of "$CREATOR's $MEDIA" like in reviews.
wpURLRewrites, wpURLRedirectRewrites :: T.Text -> T.Text
wpURLRewrites ref
  | ref == ""                                          = error "Interwiki.wpURLRewrites called with empty string; this should never happen."
  | "**" `T.isPrefixOf` ref                            = error "Interwiki.wpURLRewrites called with what looks like mistaken bolding/<strong> (a prefixed '**'); this should never happen and needs to be fixed in-place (it cannot be repaired by ignoring the '**' because the original formatting will still be broken)."
  | ref `elem` overrides'                              = ref
  | "'s" `T.isSuffixOf` ref || "’s" `T.isSuffixOf` ref = T.init $ T.init ref
  -- WP seems to not permit double or single quotation marks at the beginning of article titles, so we don't have to worry about whitelisting; any leading quotation mark means to strip the surrounding pair
  | anyPrefixT ref ["\"", "'", "‘", "’", "“", "”"]     = T.tail $ T.init ref
  | otherwise                                          = ref
    where overrides' = C.quoteOverrides ++ map (T.replace "'" "‘") C.quoteOverrides ++ map (T.replace "'" "’") C.quoteOverrides

-- bypass WP redirects to make links slightly faster, more consistent (important for link-suggester), and avoid noise in linkchecker runs warning about redirects:
-- NOTE: we match by prefix due to hash-anchors.
wpURLRedirectRewrites url = let baseURL = T.takeWhile (/='#') url
                                hits = take 1 $ filter (\(t,_) -> (T.takeWhile (/='#') t) == baseURL) C.redirectDB in
                              if null hits then url else T.replace baseURL (snd $ head hits) url -- TODO: T.replace could be checked further, with a hypothetical `replaceCheckedT`

interwikiTestSuite :: [(Inline, Inline, Inline)]
interwikiTestSuite = let redirectsCircular = map fst C.redirectDB `intersect` map snd C.redirectDB
                         redirectsDuplicate = nubOrd (map fst C.redirectDB) /= map fst C.redirectDB
  in if not (null redirectsCircular) then error ("Interwiki.hs: circular redirects detected: " ++ show redirectsCircular)
     else if redirectsDuplicate then error "Interwiki.hs: duplicate redirects detected (in either original or destination)"
  else
            map (\(a,b) -> (a, (convertInterwikiLinksInline undefined) a, b)) $ filter (\(link1, link2) -> (convertInterwikiLinksInline undefined) link1 /= link2) C.testCases

interwikiCycleTestSuite :: [(T.Text, T.Text)]
interwikiCycleTestSuite = if null (isCycleLess C.redirectDB) then [] else findCycles C.redirectDB

-- Set link-live/link-live-not classes on a WP link depending on its namespace. As the quality of WP API annotations, and the possibility of iframe popups, varies across WP namespaces, we can't simply set them universally.
--
-- A WP link may be to non-article sets of pages, or namespaces (https://en.wikipedia.org/wiki/Wikipedia:Namespace): `Talk`, `User`, `File`, `Wikipedia` etc. eg. 'https://en.wikipedia.org/wiki/File:Energy_density.svg' . Note that we need to match on the colon separator, we can't just match the namespace prefix, because the prefixes are not unique without it, eg. 'https://en.wikipedia.org/wiki/Image_segmentation' is *not* in the `Image` namespace—because images have a colon, and so they would be `Image:...`. It may also be to another language's Wikipedia, eg. <https://it.wikipedia.org/wiki/Liber_Figurarum>.
-- So just checking for 'en.wikipedia.org/wiki/' prefix is not enough.
--
-- This is important because we can request Articles through the API and display them as a WP popup, but for other namespaces it would be meaningless (what is the contents of [[Special:Random]]? Or [[Special:BookSources/0-123-456-7]]?). These can only be done as live link popups (if at all, we can't for Special:).
wpPopupClasses :: T.Text -> [T.Text]
wpPopupClasses u = ["id-not"] ++ case parseURIReference (T.unpack u) of
                        Nothing -> []
                        Just uri -> case uriAuthority uri of
                          Nothing -> []
                          Just authority -> let article = T.pack $ uriPath uri
                                                domain = T.pack $ uriRegName authority
                                            in
                                             if not ("wikipedia.org" `T.isSuffixOf` domain) && "http" `T.isPrefixOf` u then [] else
                                                        let u' = T.takeWhile (/= ':') $ replaceManyT [("/wiki/", "")] article in
                                                          (if u' `elem` apiNamespacesNo || "Signpost" `T.isInfixOf` article -- specialcase: Signpost articles apparently break popups with very strange HTML
                                                            then ["content-transform-not"] else []) ++
                                                           (if u' `elem` linkliveNamespacesNo then ["link-live-not"] else ["link-live"])

-- WP namespaces which are known to not return a useful annotation from the API; Special: does not (eg. Special:Random, or, common in article popups, Special:BookSources for ISBNs) and returns nothing while Category: returns something which is useless (just the category title!), but surprisingly, most others return something useful (eg. even Talk pages like <https:/en.wikipedia.org/api/rest_v1/page/mobile-sections/Talk:Small_caps> do).
-- I have not checked the full list of namespaces carefully so some of the odder namespaces may be bad.
apiNamespacesNo :: [T.Text]
apiNamespacesNo = ["Category", "File", "Special", "/w/index.php"]

-- A separate question from API annotations is whether a namespace permits live popups, or if it sets X-FRAME headers. Thus far, only Special: appears to block embeddings (probably for security reasons, as there is a lot of MediaWiki functionality gatewayed behind Special: URLs, while the other namespaces should be harder to abuse).
linkliveNamespacesNo :: [T.Text]
linkliveNamespacesNo = ["Special", "/w/index.php"]

-- nonArticleNamespace :: [T.Text]
-- nonArticleNamespace = ["Talk", "User", "User_talk", "Wikipedia", "Wikipedia_talk", "File", "File_talk", "MediaWiki", "MediaWiki_talk", "Template", "Template_talk", "Help", "Help_talk", "Category", "Category_talk", "Portal", "Portal_talk", "Draft", "Draft_talk", "TimedText", "TimedText_talk", "Module", "Module_talk", "Gadget", "Gadget_talk", "Gadget definition", "Gadget definition_talk", "Special", "Media"]

-- | Large table of constants; this is a mapping from shortcuts to a URL. The URL can be used by
--   appending to it the article name (suitably URL-escaped, of course).
interwikiMap :: M.Map T.Text T.Text
interwikiMap = M.fromList $ wpInterwikiMap ++ customInterwikiMap
wpInterwikiMap, customInterwikiMap :: [(T.Text, T.Text)]
customInterwikiMap = [("Hackage", "https://hackage.haskell.org/package/"),
                      ("Hawiki", "https://wiki.haskell.org/"),
                      ("Hoogle", "https://hoogle.haskell.org/?hoogle="),
                      -- shortcuts
                      ("W", "https://en.wikipedia.org/wiki/"),
                      ("WP", "https://en.wikipedia.org/wiki/")]
wpInterwikiMap = [("Wikipedia", "https://en.wikipedia.org/wiki/"),
                  ("Wikiquote", "https://en.wikiquote.org/wiki/"),
                  ("Wiktionary", "https://en.wiktionary.org/wiki/")]
