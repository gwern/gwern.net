{-# LANGUAGE OverloadedStrings #-}
module Interwiki (convertInterwikiLinks, inlinesToString, wpPopupClasses) where

import qualified Data.Map as M (fromList, lookup, Map)
import Text.Pandoc (Inline(..))
import qualified Data.Text as T (append, concat, head, isPrefixOf, null, tail, take, toUpper, pack, unpack, Text)
import Data.List.Utils (replace)
import Data.List (isPrefixOf)
-- import Network.HTTP (urlEncode)

-- INTERWIKI PLUGIN
-- This is a simplification of the original interwiki plugin I wrote for Gitit: <https://github.com/jgm/gitit/blob/master/plugins/Interwiki.hs>
-- It's more or less the same thing, but the interwiki mapping is cut down to only the ones I use, and it avoids a dependency on Gitit.
-- | Convert a list of inlines into a string.
inlinesToString :: [Inline] -> T.Text
inlinesToString = T.concat . map go
  where go x = case x of
               -- reached the literal T.Text:
               Str s    -> s
               -- strip & recurse on the [Inline]:
               Emph        x' -> inlinesToString x'
               Underline   x' -> inlinesToString x'
               Strong      x' -> inlinesToString x'
               Strikeout   x' -> inlinesToString x'
               Superscript x' -> inlinesToString x'
               Subscript   x' -> inlinesToString x'
               SmallCaps   x' -> inlinesToString x'
               -- throw away attributes and recurse on the [Inline]:
               Span _      x' -> inlinesToString x' -- eg. [foo]{.smallcaps} -> foo
               Quoted _    x' -> inlinesToString x'
               Cite _      x' -> inlinesToString x'
               Link _   x' _  -> inlinesToString x'
               Image _  x' _  -> inlinesToString x'
               -- throw away attributes, return the literal T.Text:
               Math _      x' -> x'
               RawInline _ x' -> x'
               Code _      x' -> x'
               -- fall through with a blank:
               _        -> " "::T.Text
convertInterwikiLinks :: Inline -> Inline
convertInterwikiLinks x@(Link _ []           _) = error $ "Link error: no anchor text‽ " ++ show x
convertInterwikiLinks x@(Link _ _ ("", _))      = x
convertInterwikiLinks x@(Link (ident, classes, kvs) ref (interwiki, article)) =
  if not (T.null article) && T.head article == ' ' then error $ "Link error: tooltip malformed with excess whitespace? " ++ show x else
  if T.head interwiki == '!' then
        case M.lookup (T.tail interwiki) interwikiMap of
                Just url  -> let attr' = (ident,
                                           ["id-not"] ++
                                           wpPopupClasses (if article=="" then T.unpack $ inlinesToString ref else T.unpack article) ++
                                           classes,
                                           kvs) in
                             case article of
                                  "" -> Link attr' ref (url `interwikiurl` inlinesToString ref, "") -- tooltip is now handled by LinkMetadata.hs
                                  _  -> Link attr' ref (url `interwikiurl` article, "")
                Nothing -> error $ "Attempted to use an interwiki link with no defined interwiki: " ++ show x
  else let classes' = wpPopupClasses (T.unpack interwiki) ++ classes in
         if "https://en.wikipedia.org/wiki/" `T.isPrefixOf` interwiki then
           Link (ident, "link-annotated":"backlinks-not":classes', kvs) ref (interwiki, article)
         else if "https://en.wikipedia.org/wiki/" `T.isPrefixOf` interwiki then
                Link (ident, "link-annotated-not":"backlinks-not":classes', kvs) ref (interwiki, article)
              else x
  where
    interwikiurl :: T.Text -> T.Text -> T.Text
    -- normalize links; MediaWiki requires first letter to be capitalized, and prefers '_' to ' '/'%20' for whitespace
    interwikiurl u a = let a' = if u=="https://en.wikipedia.org/wiki/" then T.toUpper (T.take 1 a) `T.append` T.tail a else a in
                         u `T.append` T.pack (replace "%" "%25" $ replace " " "_" $ deunicode (T.unpack a'))
    deunicode :: String -> String
    deunicode = replace "‘" "\'" . replace "’" "\'" . replace " " " " . replace " " " "
convertInterwikiLinks x = x

-- Set link-live/link-live-not and link-annotation/link-annotation-not classes on a WP link depending on its namespace. As the quality of WP API annotations, and the possibility of iframe popups, varies across WP namespaces, we can't simply set them universally.
--
-- A WP link may be to non-article sets of pages, or namespaces (https://en.wikipedia.org/wiki/Wikipedia:Namespace): `Talk`, `User`, `File`, `Wikipedia` etc. eg. 'https://en.wikipedia.org/wiki/File:Energy_density.svg' . Note that we need to match on the colon separator, we can't just match the namespace prefix, because the prefixes are not unique without it, eg. 'https://en.wikipedia.org/wiki/Image_segmentation' is *not* in the `Image` namespace—because images have a colon, and so they would be `Image:...`.
-- So just checking for 'en.wikipedia.org/wiki/' prefix is not enough.
--
-- This is important because we can request Articles through the API and display them as a WP popup, but for other namespaces it would be meaningless (what is the contents of [[Special:Random]]? Or [[Special:BookSources/0-123-456-7]]?). These can only be done as live link popups (if at all, we can't for Special:).
wpPopupClasses :: String -> [T.Text]
wpPopupClasses u = if not ("https://en.wikipedia.org/wiki/" `isPrefixOf` u) && "http" `isPrefixOf` u then [] else
                                let u' = takeWhile (\c -> c/=':' && c/='%' ) $ replace "https://en.wikipedia.org/wiki/" "" u in
                                  [if u' `elem` apiNamespacesNo then "link-annotation-not" else "link-annotation",
                                   if u' `elem` linkliveNamespacesNo then "link-live-not" else "link-live"]

-- WP namespaces which are known to not return a useful annotation from the API; Special: does not (eg. Special:Random, or, common in article popups, Special:BookSources for ISBNs) and returns nothing while Category: returns something which is useless (just the category title!), but surprisingly, most others return something useful (eg. even Talk pages like <https:/en.wikipedia.org/api/rest_v1/page/mobile-sections/Talk:Small_caps> do).
-- I have not checked the full list of namespaces carefully so some of the odder namespaces may be bad.
apiNamespacesNo :: [String]
apiNamespacesNo = ["Category", "Special"]

-- A separate question from API annotations is whether a namespace permits live popups, or if it sets X-FRAME headers. Thus far, only Special: appears to block embeddings (probably for security reasons, as there is a lot of MediaWiki functionality gatewayed behind Special: URLs, while the other namespaces should be harder to abuse).
linkliveNamespacesNo :: [String]
linkliveNamespacesNo = ["Special"]

-- nonArticleNamespace :: [String]
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
