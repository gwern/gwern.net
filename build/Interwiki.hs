{-# LANGUAGE OverloadedStrings #-}
module Interwiki (convertInterwikiLinks, inlinesToString, wpPopupClasses, interwikiTestSuite) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map as M (fromList, lookup, Map)
import qualified Data.Text as T (append, concat, head, isInfixOf, null, tail, take, toUpper, pack, unpack, Text)
import Network.URI (parseURIReference, uriPath, uriAuthority, uriRegName)

import Text.Pandoc (Inline(..), nullAttr)

import Utils (replaceMany)

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
                                            wpPopupClasses (T.unpack (url `interwikiurl` (if article=="" then inlinesToString ref else article))) ++
                                            classes,
                                           kvs) in
                             case article of
                                  "" -> Link attr' ref (url `interwikiurl` inlinesToString ref, "") -- tooltip is now handled by LinkMetadata.hs
                                  _  -> Link attr' ref (url `interwikiurl` article, "")
                Nothing -> error $ "Attempted to use an interwiki link with no defined interwiki: " ++ show x
  else let classes' = wpPopupClasses (T.unpack interwiki) ++ classes in
         if ".wikipedia.org/wiki/" `T.isInfixOf` interwiki then
           Link (ident, classes', kvs) ref (interwiki, article)
              else x
  where
    interwikiurl :: T.Text -> T.Text -> T.Text
    -- normalize links; MediaWiki requires first letter to be capitalized, and prefers '_' to ' '/'%20' for whitespace
    interwikiurl u a = let a' = if ".wikipedia.org/wiki/" `T.isInfixOf` u then T.toUpper (T.take 1 a) `T.append` T.tail a else a in
                         u `T.append` T.pack (replaceMany [("\"", "%22"), ("[", "%5B"), ("]", "%5D"), ("%", "%25"), (" ", "_")] $ deunicode (T.unpack a'))
    deunicode :: String -> String
    deunicode = replaceMany [("‘", "\'"), ("’", "\'"), (" ", " "), (" ", " ")]
convertInterwikiLinks x = x

interwikiTestSuite :: [(Inline, Inline, Inline)]
interwikiTestSuite = map (\(a,b) -> (a, convertInterwikiLinks a, b)) $ filter (\(link1, link2) -> convertInterwikiLinks link1 /= link2) [
  -- !Wikipedia
  (Link nullAttr [Str "Pondicherry"] ("!Wikipedia",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "Pondicherry"] ("https://en.wikipedia.org/wiki/Pondicherry", ""))
  , (Link nullAttr [Str "Special:Pondicherry"] ("!Wikipedia",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated-not", "link-live-not"], []) [Str "Special:Pondicherry"] ("https://en.wikipedia.org/wiki/Special:Pondicherry", ""))
  , (Link nullAttr [Str "SpecialPondicherry"] ("!Wikipedia",""),
     Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "SpecialPondicherry"] ("https://en.wikipedia.org/wiki/SpecialPondicherry", ""))
  , (Link nullAttr [Str "Category:Pondicherry"] ("!Wikipedia",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated-not", "link-live"], []) [Str "Category:Pondicherry"] ("https://en.wikipedia.org/wiki/Category:Pondicherry", ""))

  -- !W
  , (Link nullAttr [Str "Pondicherry"] ("!W",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "Pondicherry"] ("https://en.wikipedia.org/wiki/Pondicherry", ""))
  , (Link nullAttr [Str "Special:Pondicherry"] ("!W",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated-not", "link-live-not"], []) [Str "Special:Pondicherry"] ("https://en.wikipedia.org/wiki/Special:Pondicherry", ""))
  , (Link nullAttr [Str "SpecialPondicherry"] ("!W",""),
     Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "SpecialPondicherry"] ("https://en.wikipedia.org/wiki/SpecialPondicherry", ""))
  , (Link nullAttr [Str "Category:Pondicherry"] ("!W",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated-not", "link-live"], []) [Str "Category:Pondicherry"] ("https://en.wikipedia.org/wiki/Category:Pondicherry", ""))

  -- !W + title
  , (Link nullAttr [Str "foo"] ("!W","Pondicherry"),
    Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "foo"] ("https://en.wikipedia.org/wiki/Pondicherry", ""))
  , (Link nullAttr [Str "foo"] ("!W","Special:Pondicherry"),
    Link ("", ["backlinks-not", "id-not", "link-annotated-not", "link-live-not"], []) [Str "foo"] ("https://en.wikipedia.org/wiki/Special:Pondicherry", ""))
  , (Link nullAttr [Str "foo"] ("!W","SpecialPondicherry"),
     Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "foo"] ("https://en.wikipedia.org/wiki/SpecialPondicherry", ""))
  , (Link nullAttr [Str "foo"] ("!W","Category:Pondicherry"),
    Link ("", ["backlinks-not", "id-not", "link-annotated-not", "link-live"], []) [Str "foo"] ("https://en.wikipedia.org/wiki/Category:Pondicherry", ""))

   -- <https://en.wikipedia.org/wiki/$ARTICLE>
  , (Link nullAttr [Str "Pondicherry"] ("https://en.wikipedia.org/wiki/Pondicherry",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "Pondicherry"] ("https://en.wikipedia.org/wiki/Pondicherry", ""))
  , (Link nullAttr [Str "Special:Pondicherry"] ("https://en.wikipedia.org/wiki/Special:Pondicherry",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated-not", "link-live-not"], []) [Str "Special:Pondicherry"] ("https://en.wikipedia.org/wiki/Special:Pondicherry", ""))
  , (Link nullAttr [Str "SpecialPondicherry"] ("https://en.wikipedia.org/wiki/SpecialPondicherry",""),
     Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "SpecialPondicherry"] ("https://en.wikipedia.org/wiki/SpecialPondicherry", ""))
  , (Link nullAttr [Str "Category:Pondicherry"] ("https://en.wikipedia.org/wiki/Category:Pondicherry",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated-not", "link-live"], []) [Str "Category:Pondicherry"] ("https://en.wikipedia.org/wiki/Category:Pondicherry", ""))

  -- /Lorem testcases: Should popup (as an **annotation**):
  , (Link nullAttr [Emph [Str "Liber Figurarum"]] ("https://it.wikipedia.org/wiki/Liber_Figurarum",""),
     Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Emph [Str "Liber Figurarum"]] ("https://it.wikipedia.org/wiki/Liber_Figurarum", ""))
  , (Link nullAttr [Str "Small caps"] ("!W",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "Small caps"] ("https://en.wikipedia.org/wiki/Small_caps", ""))
  , (Link nullAttr [Str "Talk:Small caps"] ("!W",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "Talk:Small caps"] ("https://en.wikipedia.org/wiki/Talk:Small_caps", ""))
  , (Link nullAttr [Str "User:Gwern"] ("!W",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "User:Gwern"] ("https://en.wikipedia.org/wiki/User:Gwern", ""))
  , (Link nullAttr [Str "User talk:Gwern"] ("!W",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "User talk:Gwern"] ("https://en.wikipedia.org/wiki/User_talk:Gwern", ""))
  , (Link nullAttr [Str "Help:Authority control"] ("!W",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "Help:Authority control"] ("https://en.wikipedia.org/wiki/Help:Authority_control", ""))
  , (Link nullAttr [Str "Help talk:Authority control"] ("!W",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "Help talk:Authority control"] ("https://en.wikipedia.org/wiki/Help_talk:Authority_control", ""))
  , (Link nullAttr [Str "Wikipedia:Wikipedia Signpost"] ("!W",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "Wikipedia:Wikipedia Signpost"] ("https://en.wikipedia.org/wiki/Wikipedia:Wikipedia_Signpost", ""))
  , (Link nullAttr [Str "Wikipedia talk:Wikipedia Signpost"] ("!W",""),
    Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "Wikipedia talk:Wikipedia Signpost"] ("https://en.wikipedia.org/wiki/Wikipedia_talk:Wikipedia_Signpost", ""))
  , (Link nullAttr [Str "Wikipedia talk:Wikipedia Signpost"] ("!W",""),
     Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "Wikipedia talk:Wikipedia Signpost"] ("https://en.wikipedia.org/wiki/Wikipedia_talk:Wikipedia_Signpost", ""))
  , (Link nullAttr [Str "File:NASA Worm logo.svg"] ("!W",""),
      Link ("", ["backlinks-not", "id-not", "link-annotated-not", "link-live"], []) [Str "File:NASA Worm logo.svg"] ("https://en.wikipedia.org/wiki/File:NASA_Worm_logo.svg", ""))
  , (Link nullAttr [Str "MediaWiki:Citethispage-content"] ("!W",""),
      Link ("", ["backlinks-not", "id-not", "link-annotated", "link-live"], []) [Str "MediaWiki:Citethispage-content"] ("https://en.wikipedia.org/wiki/MediaWiki:Citethispage-content", ""))

    -- Should popup (as a **live link** but not annotation): [Category:Buddhism and sports](!W)
  , (Link nullAttr [Str "Category:Buddhism and sports"] ("https://en.wikipedia.org/wiki/Category:Buddhism_and_sports",""),
     Link ("", ["backlinks-not", "id-not", "link-annotated-not", "link-live"], []) [Str "Category:Buddhism and sports"] ("https://en.wikipedia.org/wiki/Category:Buddhism_and_sports", ""))
    , (Link nullAttr [Str "Category:Buddhism and sports"] ("!W",""),
     Link ("", ["backlinks-not", "id-not", "link-annotated-not", "link-live"], []) [Str "Category:Buddhism and sports"] ("https://en.wikipedia.org/wiki/Category:Buddhism_and_sports", ""))
    , (Link nullAttr [Str "Category:Buddhism and sports"] ("!W",""),
     Link ("", ["backlinks-not", "id-not", "link-annotated-not", "link-live"], []) [Str "Category:Buddhism and sports"] ("https://en.wikipedia.org/wiki/Category:Buddhism_and_sports", ""))
    , (Link nullAttr [Str "Buddhism category"] ("!W","Category:Buddhism and sports"),
     Link ("", ["backlinks-not", "id-not", "link-annotated-not", "link-live"], []) [Str "Buddhism category"] ("https://en.wikipedia.org/wiki/Category:Buddhism_and_sports", ""))

    -- Should **not** popup at all: [Special:Random](!W)
  , (Link nullAttr [Str "Special:Random"] ("!W",""),
      Link ("", ["backlinks-not", "id-not", "link-annotated-not", "link-live-not"], []) [Str "Special:Random"] ("https://en.wikipedia.org/wiki/Special:Random", ""))
  , (Link nullAttr [Str "Special:BookSources/0-8054-2836-4"] ("!W",""),
     Link ("", ["backlinks-not", "id-not", "link-annotated-not", "link-live-not"], []) [Str "Special:BookSources/0-8054-2836-4"] ("https://en.wikipedia.org/wiki/Special:BookSources/0-8054-2836-4", ""))
  ]

-- Set link-live/link-live-not and link-annotated/link-annotated-not classes on a WP link depending on its namespace. As the quality of WP API annotations, and the possibility of iframe popups, varies across WP namespaces, we can't simply set them universally.
--
-- A WP link may be to non-article sets of pages, or namespaces (https://en.wikipedia.org/wiki/Wikipedia:Namespace): `Talk`, `User`, `File`, `Wikipedia` etc. eg. 'https://en.wikipedia.org/wiki/File:Energy_density.svg' . Note that we need to match on the colon separator, we can't just match the namespace prefix, because the prefixes are not unique without it, eg. 'https://en.wikipedia.org/wiki/Image_segmentation' is *not* in the `Image` namespace—because images have a colon, and so they would be `Image:...`.
-- So just checking for 'en.wikipedia.org/wiki/' prefix is not enough.
--
-- This is important because we can request Articles through the API and display them as a WP popup, but for other namespaces it would be meaningless (what is the contents of [[Special:Random]]? Or [[Special:BookSources/0-123-456-7]]?). These can only be done as live link popups (if at all, we can't for Special:).
wpPopupClasses :: String -> [T.Text]
wpPopupClasses u = nubOrd $ ["backlinks-not", "id-not"] ++ case parseURIReference u of
                        Nothing -> []
                        Just uri -> case uriAuthority uri of
                          Nothing -> []
                          Just authority -> let article = uriPath uri
                                                domain = uriRegName authority
                                            in
                                             if not ("wikipedia.org" `isSuffixOf` domain) && "http" `isPrefixOf` u then [] else
                                                        let u' = takeWhile (/= ':') $ replaceMany [("/wiki/", "")] article in
                                                          [if u' `elem` apiNamespacesNo then "link-annotated-not" else "link-annotated",
                                                           if u' `elem` linkliveNamespacesNo then "link-live-not" else "link-live"]

-- WP namespaces which are known to not return a useful annotation from the API; Special: does not (eg. Special:Random, or, common in article popups, Special:BookSources for ISBNs) and returns nothing while Category: returns something which is useless (just the category title!), but surprisingly, most others return something useful (eg. even Talk pages like <https:/en.wikipedia.org/api/rest_v1/page/mobile-sections/Talk:Small_caps> do).
-- I have not checked the full list of namespaces carefully so some of the odder namespaces may be bad.
apiNamespacesNo :: [String]
apiNamespacesNo = ["Category", "File", "Special"]

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
