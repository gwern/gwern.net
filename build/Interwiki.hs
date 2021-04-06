{-# LANGUAGE OverloadedStrings #-}
module Interwiki (convertInterwikiLinks, inlinesToString) where

import qualified Data.Map as M (fromList, lookup, Map)
import Text.Pandoc (Inline(..))
import qualified Data.Text as T (append, concat, head, tail, take, toUpper, pack, unpack, Text)
import Data.List.Utils (replace)
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
               Span _      x' -> inlinesToString x' -- eg [foo]{.smallcaps} -> foo
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
convertInterwikiLinks x@(Link attr@(ident, classes, kvs) ref (interwiki, article)) =
  if T.head interwiki == '!' then
        case M.lookup (T.tail interwiki) interwikiMap of
                Just url  -> case article of
                                  "" -> Link attr' ref (url `interwikiurl` inlinesToString ref, "") -- tooltip is now handled by LinkMetadata.hs
                                  _  -> Link attr' ref (url `interwikiurl` article, "")
                Nothing -> error $ "Attempted to use an interwiki link with no defined interwiki: " ++ show x
  else x
            where -- 'https://starwars.wikia.com/wiki/Emperor_Palpatine'
              --
                  interwikiurl :: T.Text -> T.Text -> T.Text
                  -- normalize links; MediaWiki requires first letter to be capitalized
                  interwikiurl u a = let a' = if u=="https://en.wikipedia.org/wiki/" then T.toUpper (T.take 1 a) `T.append` T.tail a else a in
                                       u `T.append` T.pack (replace "%20" "_" $ replace "%23" "#" $ urlEncode (deunicode (T.unpack a')))
                  deunicode :: String -> String
                  deunicode = map (\c -> if c == '’' then '\'' else c)
                  attr' = if "docMetadata" `elem` classes then attr else (ident, "docMetadata":classes, kvs)
convertInterwikiLinks x = x
-- | Large table of constants; this is a mapping from shortcuts to a URL. The URL can be used by
--   appending to it the article name (suitably URL-escaped, of course).
interwikiMap :: M.Map T.Text T.Text
interwikiMap = M.fromList $ wpInterwikiMap ++ customInterwikiMap
wpInterwikiMap, customInterwikiMap :: [(T.Text, T.Text)]
customInterwikiMap = [("Hackage", "https://hackage.haskell.org/package/"),
                      ("Hawiki", "https://haskell.org/haskellwiki/"),
                      ("Hoogle", "https://www.haskell.org/hoogle/?hoogle="),
                      -- shortcuts
                      ("W", "https://en.wikipedia.org/wiki/"),
                      ("WP", "https://en.wikipedia.org/wiki/")]
wpInterwikiMap = [("Wikipedia", "https://en.wikipedia.org/wiki/"),
                  ("Wikiquote", "https://en.wikiquote.org/wiki/"),
                  ("Wiktionary", "https://en.wiktionary.org/wiki/")]
