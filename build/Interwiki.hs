{-# LANGUAGE OverloadedStrings #-}
module Interwiki (convertInterwikiLinks, inlinesToString) where

import qualified Data.Map as M (fromList, lookup, Map)
import Text.Pandoc (Inline(..))
import qualified Data.Text as T (append, concat, head, isPrefixOf, null, tail, take, toUpper, pack, unpack, Text)
import Data.List.Utils (replace)
import Data.List (isPrefixOf)
import Network.HTTP (urlEncode)

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
                                           "idNot" : (if enWikipediaArticleNamespace (T.unpack url) then "docMetadata" else "docMetadataNot") : classes,
                                           kvs) in
                             case article of
                                  "" -> Link attr' ref (url `interwikiurl` inlinesToString ref, "") -- tooltip is now handled by LinkMetadata.hs
                                  _  -> Link attr' ref (url `interwikiurl` article, "")
                Nothing -> error $ "Attempted to use an interwiki link with no defined interwiki: " ++ show x
  else if "https://en.wikipedia.org/wiki/" `T.isPrefixOf` interwiki && enWikipediaArticleNamespace (T.unpack interwiki) then
            Link (ident, "docMetadata":classes, kvs) ref (interwiki, article)
       else x
            where
                  interwikiurl :: T.Text -> T.Text -> T.Text
                  -- normalize links; MediaWiki requires first letter to be capitalized
                  interwikiurl u a = let a' = if u=="https://en.wikipedia.org/wiki/" then T.toUpper (T.take 1 a) `T.append` T.tail a else a in
                                       u `T.append` T.pack (replace "%20" "_" $ replace "%23" "#" $ urlEncode (deunicode (T.unpack a')))
                  deunicode :: String -> String
                  deunicode = replace "’" "\'" . replace " " " " . replace " " " "
convertInterwikiLinks x = x

-- If True, a URL is a regular English Wikipedia article. If False, it's something else, like a talk page or history page etc.
--
-- a WP link may be to non-article sets of pages, or namespaces (https://en.wikipedia.org/wiki/Wikipedia:Namespace): `Talk`, `User`, `File`, `Wikipedia` etc. eg. 'https://en.wikipedia.org/wiki/File:Energy_density.svg' . Note that we need the colon separator because the prefixes are not unique without it, eg. 'https://en.wikipedia.org/wiki/Image_segmentation' is not in the `Image` namespace because images have a colon, and so they would be `Image:...`.
-- so just checking for 'en.wikipedia.org/wiki/' prefix is not enough; we can only popup on articles, the other pages need raw URL previews.
enWikipediaArticleNamespace :: String -> Bool
enWikipediaArticleNamespace u = if not ("https://en.wikipedia.org/wiki/" `isPrefixOf` u) then False else
                                let u' = takeWhile (/=':') $ replace "https://en.wikipedia.org/wiki/" "" u in
                                  not $ u' `elem` (map (++":") ["Talk", "User", "User talk", "Wikipedia", "Wikipedia talk", "File", "File talk", "MediaWiki", "MediaWiki talk", "Template", "Template talk", "Help", "Help talk", "Category", "Category talk", "Portal", "Portal talk", "Draft", "Draft talk", "TimedText", "TimedText talk", "Module", "Module talk", "Gadget", "Gadget talk", "Gadget definition", "Gadget definition talk", "Special", "Media"])

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
