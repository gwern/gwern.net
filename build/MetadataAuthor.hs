{-# LANGUAGE OverloadedStrings #-}

{- MetadataAuthor.hs: module for managing 'author' metadata & hyperlinking author names in annotations
Author: Gwern Branwen
Date: 2024-04-14
When:  Time-stamp: "2024-06-10 22:16:31 gwern"
License: CC-0

Authors are useful to hyperlink in annotations, but pose some problems: author names are often ambiguous in both colliding and having many non-canonical versions, are sometimes extremely high frequency & infeasible to link one by one, and there can be a large number of authors (sometimes hundreds or even thousands in some scientific fields).
Inserting an author link in the annotation body is highly unsatisfactory, because it doesn't generalize & is clumsy & confusing (a reader naturally expects the regular author name to be a hyperlink, not an odd editorial insertion in the abstract or summary), so we want to hyperlink author names themselves.

This requires multiple logically-separate phases:

1. the annotation format GTX backend allow the 'author' field to contain HTML, as long as it is still comma-separated; this is permitted by the frontend, so we can hyperlink author names arbitrarily in the original annotation as the first level.

    As simple HTML, we can use `<span>`-level collapsing to tame long lists of authors for the reader. And as simple HTML links, they benefit from all existing HTML processing; for example, the author link's URL may be an annotated URL, thereby creating a popup of its own. And such a link is also a standard backlink.

    So we get 'author bibliographies' for free in the form of backlinks: if author X is defined as URL Y, then the backlinks for URL Y are now a list of every URL with author X in the author list.
2. author names are unambiguously comma-separated, so we can parse author fields into lists of authors, and automatically link each one individually by a string match (using a database of name → URL) & rewrite. The author name is truncated at the '#' character to allow disambiguation.
3. further, we can canonicalize author names, by rewriting the original annotation and updating the GTX
4. the canonicalization & link rewrite can be overridden by the original annotation, and can themselves include black/whitelists as necessary.

So let's take a case like "Eliezer Yudkowsky".
This is sometimes written as "E. Yudkowsky", "Eliezer S. Yudkowsky", "E. S. Yudkowsky", or "esyudkowsky" (Twitter username); the canonicalizer rewrites them all to "Eliezer Yudkowsky".
"Eliezer Yudkowsky" gets an auto-link to "https://yudkowsky.net", his homepage.
However, for an annotation of a LessWrong blog post, eg. it might be manually written inline as `<a href=http://lesswrong.com/user/Eliezer_Yudkowsky/>Eliezer Yudkowsky</a>`, or if it is a tweet, to `<a href=https://x.com/esyudkowsky>` etc.
If this is common for an author, it would be possible to define 'disambiguated' author names, using the familiar HTML anchor syntax trick we already use for disambiguating multiple annotations of the same URL: the annotation specifies an author name like "Eliezer Yudkowsky#Twitter", and the author link database specifies `("Eliezer Yudkowsky#Twitter", "https://x.com/esyudkowsky")`.
This can also be applied to multiple people of the same name, giving them a mnemonic disambiguation: "John Smith#genetics" vs "John Smith#venture-capital" etc.

The initial set of author links was created by pinging Wikipedia for whether a non-disambiguation article existed for the exact author name.
(This resulted in many false positives, but so it goes.)
Subsequent author links can be prioritized by the usual approach of setting up a blacklist of author names, and then regularly reviewing the 𝑛 top author names by site-wide frequency.
This could further come with some browser automation like searching Wikipedia + Google + Google Scholar profile.
-}

module MetadataAuthor where

import Control.Monad (void)
import Data.List (intersperse, intercalate)
import qualified Data.Map.Strict as M (lookup, toList)
import qualified Data.Text as T (find, pack, splitOn, takeWhile, Text, append, unpack)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Text.Pandoc (Inline(Link, Span, Space, Str), nullAttr)
import Network.HTTP (urlEncode)

import Data.FileStore.Utils (runShellCommand)
import Interwiki (toWikipediaEnURL, toWikipediaEnURLSearch)
import LinkMetadataTypes (Metadata)
import Utils (split, frequency)

import qualified Config.MetadataAuthor as C

-- for link bibliographies / tag pages, better truncate author lists at a reasonable length.
-- (We can make it relatively short because the full author list will be preserved as part of it.)
-- simple string-based author-list truncation, with no attempt to do inline-collapse: take the first 100 characters + whatever is necessary to finish the next author (as defined by the space-comma separation)
authorsTruncateString :: String -> String
authorsTruncateString a = let (before,after) = splitAt 100 a in before ++ (if null after then "" else head $ split ", " after)

authorCollapseTest :: [(String, [Inline])]
authorCollapseTest = filter (\(i,o) -> authorCollapse i /= o) C.authorCollapseTestCases

authorCollapse :: String -> [Inline]
authorCollapse aut
  | aut `elem` ["", "N/A", "N/\8203A"] = []
  | otherwise =
  let authors = intersperse (Str ", ") $ map (linkify . T.pack) $ split ", " aut
      authorSpan = if length authors <= 2 then Span ("", ["author", "cite-author"], []) authors
                                               else if length authors < 8 then
                                                      Span ("", ["author"], []) authors
                                                    else let authorsInitial = take 5 authors  -- at >4, we give up trying to display them all & show just the first 3 by default (so we 'snap back' to default 3 authors max, after allowing a bit of elasticity of up to 4, to avoid the situation where we have an inline-collapse with just 1 author tucked away in it - which is annoying because it means cognitive / visual overhead & effort which is then disappointed to see just 1 author hidden - make it worth the reader's while to bother to uncollapse it!)
                                                             authorsRest = drop 5 authors
                                                         in Span ("", ["author", "collapse"], [])
                                                            [Span ("", ["abstract-collapse"], []) authorsInitial
                                                            , Span ("", ["abstract-collapse-only"], []) [Span ("", ["cite-author-plural"], []) []]
                                                            , Span nullAttr authorsRest]
  in [Space, authorSpan]

-- authorsCanonicalizeT :: T.Text -> T.Text
-- authorsCanonicalizeT = T.intercalate ", " . replaceExact (map (\(a,b) -> (T.pack a, T.pack b)) C.canonicals) . T.splitOn ", "
authorsCanonicalize :: String -> String
authorsCanonicalize = intercalate ", " . map (\a -> fromMaybe a (M.lookup a C.canonicals)) . split ", "

authorsLinkify :: T.Text -> [Inline]
authorsLinkify = intersperse (Str ", ") . map linkify . T.splitOn ", "

linkify :: T.Text -> Inline
linkify ""  = Space
linkify " " = Space
linkify aut -- skip anything which might be HTML:
  | isJust (T.find (== '<') aut) || isJust (T.find (== '>') aut) = Str aut -- TODO: my installation of text-1.2.4.1 doesn't provide `T.elem`, so we use a more convoluted `T.find` call until an upgrade
  | otherwise = case M.lookup aut C.authorLinkDB of
                  Nothing -> Str aut
                  Just u -> let aut' = T.takeWhile (/= '#') aut in -- author disambiguation is done by appending an anchor-style disambig like '#foo'; once we have done the lookup, we no longer need it and delete it for display
                                Link nullAttr [Str aut'] (u, "") -- TODO: authorsInitialize -- must be done inside the link-ification step, so skip for now; do we really want to initialize authors at all?

authorPrioritize :: [T.Text] -> [(Int,T.Text)]
authorPrioritize auts = reverse $ frequency $ map fst $
  filter (\(_,b) -> isNothing b) $ map (\a -> (a, M.lookup a C.authorLinkDB)) $
  filter (`notElem` C.authorLinkBlacklist) auts

authorBrowseTopN :: Metadata -> Int -> IO ()
authorBrowseTopN md n = do let mdl = M.toList md
                           let authorList = map T.pack $ concatMap (split ", ") $
                                 map (\(_,(_,aut,_,_,_,_,_)) -> aut) mdl
                           let authorsTop = take n $ map snd $ authorPrioritize authorList
                           authorBrowseSearchEngines authorsTop

-- search for an author in various search engines to conveniently define a URL for an author:
-- generally, we want the WP article first, then a GS profile is often second-best, and if we can't find anything there, something in the wild west of the general Internet may be the only option.
authorBrowseSearchEngines :: [T.Text] -> IO ()
authorBrowseSearchEngines [] = error "MetadataAuthor.authorBrowseSearchEngines: passed an empty list; this should never happen!"
authorBrowseSearchEngines [""] = error "MetadataAuthor.authorBrowseSearchEngines: passed an empty string; this should never happen!"
authorBrowseSearchEngines authors = let urls = concatMap authorURLs authors
                                       in void $ runShellCommand "./" (Just [("DISPLAY", ":0")]) "chromium"
                                          (map T.unpack urls)
 where authorURLs :: T.Text -> [T.Text]
       authorURLs "" = error "MetadataAuthor.authorURLs: passed an empty string for an author; this should never happen!"
       authorURLs author = let escapedAuthor = T.pack $ urlEncode $ T.unpack author
                               wpURL       = toWikipediaEnURL       author
                               wpSearchURL = toWikipediaEnURLSearch author
                               gsURL = "https://scholar.google.com/scholar?q=" `T.append` escapedAuthor
                               gURL  = "https://www.google.com/search?q="      `T.append` escapedAuthor
                            in [wpURL, wpSearchURL, gsURL, gURL]
