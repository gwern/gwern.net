{-# LANGUAGE OverloadedStrings #-}

{- Metadata.Author.hs: module for managing 'author' metadata & hyperlinking author names in annotations
Author: Gwern Branwen
Date: 2024-04-14
When:  Time-stamp: "2025-03-23 21:11:27 gwern"
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
The canonical name "Eliezer Yudkowsky" gets an auto-link to "https://yudkowsky.net", his homepage.
However, for an annotation of a LessWrong blog post, eg. it might be manually written inline as `<a href=http://lesswrong.com/user/Eliezer_Yudkowsky/>Eliezer Yudkowsky</a>`, or if it is a tweet, to `<a href=https://x.com/esyudkowsky>` etc.
If this is common for an author, it would be possible to define 'disambiguated' author names, using the familiar HTML anchor syntax trick we already use for disambiguating multiple annotations of the same URL: the annotation specifies an author name like "Eliezer Yudkowsky#Twitter", and the author link database specifies `("Eliezer Yudkowsky#Twitter", "https://x.com/esyudkowsky")`.
This can also be applied to multiple people of the same name, giving them a mnemonic disambiguation: "John Smith#genetics" vs "John Smith#venture-capital" etc.

The first set of author links was created by pinging Wikipedia for whether a non-disambiguation article existed for the exact author name.
(This resulted in many false positives, but so it goes.)
Subsequent author links can be prioritized by the usual approach of setting up a blacklist of author names, and then regularly reviewing the 𝑛 top author names by site-wide frequency.
This could further come with some browser automation like searching Wikipedia + Google + Google Scholar profile.
-}

module Metadata.Author where

import Control.Monad (void)
import Data.Char (isLetter, toUpper)
import Data.List (intersperse, intercalate)
import Data.List.Split (splitOn)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map.Strict as M (lookup, map, fromList, toList, unionWithKey, Map)
import qualified Data.Text as T (find, pack, splitOn, takeWhile, Text, append, unpack)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Text.Pandoc (Inline(Link, Span, Space, Str), nullAttr, Pandoc(Pandoc), Block(Para), nullMeta)
import Network.HTTP (urlEncode)

import Data.FileStore.Utils (runShellCommand)
import Interwiki (toWikipediaEnURLSearch)
import LinkMetadataTypes (Metadata)
import Utils (split, frequency, trim, replaceMany, sedMany, printRed, printGreen)
import qualified LinkBacklink as BL
import Query (extractURLs)
import Cycle (testInfixRewriteLoops)

import qualified Config.Metadata.Author as CA

-- Convenience function for defining author aliases to map various ways of spelling or abbreviating an author's name, as papers & databases can be very inconsistent about how they handle first names & middle names. This avoids the manual toil of trying to catch every variant by hand.
--
-- The input must be a full name with 2–3 components like "Foo Bar" or "Foo Baz Bar" or "Foo B. Bar", "Foo Baz Quux Bar", <30 characters (very long names suggest errors like swapped metadata fields).
--
-- > name2Abbreviations "Ken Ong"
-- → [("K. Ong","Ken Ong"),("K Ong","Ken Ong")]
-- > name2Abbreviations "Ken J. Ong"
-- → [("K J. Ong","Ken J. Ong"),("K J Ong","Ken J. Ong"),("K. J. Ong","Ken J. Ong"),("K. J Ong","Ken J. Ong"),("K. Ong","Ken J. Ong"),("Ken Ong","Ken J. Ong"),("Ken J Ong","Ken J. Ong")]
-- > name2Abbreviations "Ingrid Sigfrid Melle"
-- → [("I S Melle","Ingrid Sigfrid Melle"),("I Sigfrid Melle","Ingrid Sigfrid Melle"),("I. Melle","Ingrid Sigfrid Melle"),("I. S. Melle","Ingrid Sigfrid Melle"),("I. Sigfrid Melle","Ingrid Sigfrid Melle"),("I.S. Melle","Ingrid Sigfrid Melle"),("IS Melle","Ingrid Sigfrid Melle"),("Ingrid Melle","Ingrid Sigfrid Melle"),("Ingrid S Melle","Ingrid Sigfrid Melle"),("Ingrid S. Melle","Ingrid Sigfrid Melle")]
name2Abbreviations :: String -> [(String, String)]
name2Abbreviations fullName
  | null fullName                                = error $ "Author.name2Abbreviations: Name cannot be empty; input was: '" ++ fullName ++ "'"
  | length nameParts < 2 || length nameParts > 3 = error $ "Author.name2Abbreviations: Name must have 2 or 3 parts; input was: '" ++ fullName ++ "'"
  | any null nameParts                           = error $ "Author.name2Abbreviations: Name parts cannot be empty; input was: '" ++ fullName ++ "'"
  | (not . all (isLetter . head)) nameParts      = error $ "Author.name2Abbreviations: Each name part must start with a letter; input was: '" ++ fullName ++ "'"
  | any ((> 50) . length) nameParts              = error $ "Author.name2Abbreviations: Each name part must be 50 characters or less; input was: '" ++ fullName ++ "'"
  | otherwise = case nameParts of
      [first, lastName] ->
        let iFirst = i first
        in [ (iFirst ++ ". " ++ lastName, fullName)
           , (iFirst ++ " " ++ lastName, fullName)
           ]
      [first, middle, lastName] ->
        let iFirst = i first
            iMiddle = if length middle == 2 && last middle == '.' then middle else i middle
            middleAbbreviations = if length middle == 2 && last middle == '.'
                                  then [middle, init middle]
                                  else [iMiddle ++ ".", iMiddle, middle]
        in nubOrd $ -- remove potential duplicates due to abbreviation punctuation of either first or middle name
           [ (iFirst ++ " " ++ m ++ " " ++ lastName, fullName) | m <- middleAbbreviations ] ++
           [ (iFirst ++ ". " ++ m ++ " " ++ lastName, fullName) | m <- middleAbbreviations ] ++
           [ (iFirst ++ "." ++ iMiddle ++ ". " ++ lastName, fullName) | length middle > 2 ] ++
           [ (iFirst ++ ". " ++ lastName, fullName)
           , (first ++ " " ++ lastName, fullName)
           ] ++
           [ (first ++ " " ++ m ++ " " ++ lastName, fullName) | m <- middleAbbreviations, m /= middle ]
      _ -> error $ "Author.name2Abbreviations: Unexpected number of name parts; input was: '" ++ fullName ++ "'" -- This should never be reached due to earlier checks
  where
    nameParts :: [String]
    nameParts = words fullName
    i :: String -> String
    i = (:[]) . toUpper . head

-- handle initials consistently as period+space-separated; delete titles; delete the occasional final Oxford 'and' cluttering up author lists
cleanAuthors :: String -> String
cleanAuthors = trim . replaceMany CA.cleanAuthorsFixedRewrites . sedMany CA.cleanAuthorsRegexps

cleanAuthorsTest :: [(String,String,String)]
cleanAuthorsTest = testInfixRewriteLoops CA.cleanAuthorsFixedRewrites cleanAuthors

-- A Twitter username is a 1–15 character alphanumeric/underscore string:
-- must handle both "https://x.com/grantslatton/status/1703913578036904431" and "https://x.com/grantslatton":
-- Unit-tests in `Config.Metadata.Format.extractTwitterUsernameTestSuite`.
extractTwitterUsername :: String -> String
extractTwitterUsername url = (\u -> if length u > 15 then error ("Metadata.Format.extractTwitterUsername: extracted username >15 characters in length, which is illegal; extracted: " ++ u ++ "; URL:" ++ url) else u) $
   sedMany [("^https:\\/\\/x\\.com\\/([a-zA-Z0-9_]+)(/.*)?(\\?lang=[a-z]+)?$", "\\1")] url

-- Compact lists of authors to abbreviate personal names, but preserve the full name in a span tooltip for on-hover like usual.
--
-- eg. > authorsInitialize "J. Smith, Foo Bar"
-- → [Str "J. Smith",Str ", ",Span ("",[],[("title","Foo Bar")]) [Str "F. Bar"]]
--
-- > authorsInitialize "John Jacob Jingleheimer Schmidt"
-- → [Str "John Jacob Jingleheimer Schmidt"]
-- vs:
-- > authorsInitialize "John Jacob Jingleheimer Schmidt, John Jacob Jingleheimer Schmidt, John Jacob Jingleheimer Schmidt"
-- → [Span ("",[],[("title","John Jacob Jingleheimer Schmidt")]) [Str "J. Schmidt"],Str ", ",
--    Span ("",[],[("title","John Jacob Jingleheimer Schmidt")]) [Str "J. Schmidt"],Str ", ",
--    Span ("",[],[("title","John Jacob Jingleheimer Schmidt")]) [Str "J. Schmidt"]]
--
-- BUG: does not initialize names with Unicode. This is because the regex library with search-and-replace does not support Unicode, and the regex libraries which support Unicode do not provide search-and-replace. Presumably I can hack up a search-and-replace on the latter.
-- Example: > authorsInitialize "Robert Geirhos, Jörn-Henrik Jacobsen, Claudio Michaelis, ..."
-- → [Span ("",[],[("title","Robert Geirhos")]) [Str "R. Geirhos"],Str ", ",Str "J\246rn-Henrik Jacobsen", ...]
authorsInitialize :: String -> [Inline]
authorsInitialize aut = let authors = split ", " aut in
                          if length authors == 1 then [Str $ T.pack aut]
                          else
                              intersperse (Str ", ") $
                              map (\a -> let short = sedMany (reverse [
                                               -- three middle-names:
                                                 ("^([A-Z.-])[A-za-z.-]+ [A-za-z.-]+ [A-za-z.-]+ [A-za-z.-]+ (.*)", "\\1. \\2")
                                               -- two middle-names:
                                               , ("^([A-Z.-])[A-za-z.-]+ [A-za-z.-]+ [A-za-z.-]+ (.*)", "\\1. \\2")
                                               -- one middle-name:
                                               , ("^([A-Z.-])[A-za-z.-]+ [A-za-z.-]+ (.*)", "\\1. \\2")
                                               -- no middle-name:
                                               , ("^([A-Z.-])[A-za-z.-]+ (.*)", "\\1. \\2")]) a in
                                           if a==short then Str (T.pack a) else Span ("", [], [("title", T.pack a)]) [Str (T.pack short)]) authors

-- For link bibliographies / tag pages, better truncate author lists at a reasonable length.
--
-- (We can make it relatively short because the full author list will be preserved as part of it.)
-- This is a simple string-based author-list truncation, with no attempt to do inline-collapse:
-- take the first 100 characters + whatever is necessary to finish the next author
-- (as defined by the space-comma separation).
authorsTruncateString :: String -> String
authorsTruncateString a = let (before,after) = splitAt 100 a in before ++ (if null after then "" else head $ split ", " after)

authorCollapseTest :: [(String, [Inline])]
authorCollapseTest = filter (\(i,o) -> authorCollapse i /= o) CA.authorCollapseTestCases

-- NOTE: we deliberately keep the displayed authors outside the span collapse abstract to avoid triggering the ordinal size indicator; this is because, unlike 'normal' span collapses, the length of an author list has little to do with the 'quality' or 'interest' of the author list and so it is not useful to know what percentage of the author list has been collapsed. (In fact, the longer the author list, the less interesting any name in it is, and the less you want to look at it. In a physics or GWAS paper with 1,000+ authors, any name but the first few, possibly is near-meaningless for anyone except a tenure committee.) All the reader needs to know is that the rest of the author list exists, and where to find it in the rare event they need it, but not its size.
authorCollapse :: String -> [Inline]
authorCollapse aut
  | aut `elem` ["", "N/A", "N/\8203A"] = []
  | otherwise =
  let authors = intersperse (Str ", ") $ map (linkify . -- removes '#' disambiguation as well
                                               T.pack) $ split ", " aut
      authorSpan = if length authors <= 2 then Span ("", ["author", "cite-author"], []) authors
                                               else if length authors < 8 then
                                                      Span ("", ["author"], []) authors
        -- at >4, we give up trying to display them all & show just the first 3 by default
        -- (so we 'snap back' to default 3 authors max, after allowing a bit of elasticity
        -- of up to 4, to avoid the situation where we have an inline-collapse with
        -- just 1 author tucked away in it—which is annoying because
        -- it means cognitive / visual overhead & effort, who is then disappointed to see
        -- just 1 author hidden—make it worth the reader's while to bother to uncollapse it!)
                                                    else let authorsInitial = take 5 authors
                                                             authorsRest = drop 5 authors
                                                         in Span ("", ["author"], [])
                                                            (authorsInitial ++
                                                            -- ellipsis to indicate the hidden authors will be handled by the span collapse JS code:
                                                             [Span ("",["collapse"],[]) authorsRest])
  in [Space, authorSpan]

-- authorsCanonicalizeT :: T.Text -> T.Text
-- authorsCanonicalizeT = T.intercalate ", " . replaceExact (map (\(a,b) -> (T.pack a, T.pack b)) CA.canonicals) . T.splitOn ", "
authorsCanonicalize :: String -> String
authorsCanonicalize = intercalate ", " . map authorCanonicalize . split ", "

authorCanonicalize :: String -> String
authorCanonicalize a = fromMaybe a $ M.lookup a authorDB

isAuthor :: String -> Bool
isAuthor "" = False
isAuthor a = case M.lookup a authorDB of
               Nothing -> isJust $ M.lookup (T.pack a) CA.authorLinkDB
               Just _ -> True

-- convenience function for working with metadata to quickly see which authors are currently unknown; some of the authors might obviously have a Wikipedia or other easy URL to define.
authorsUnknown :: [String] -> [String]
authorsUnknown [] = error "Author.authorsUnknown: called with an empty list, but this should only ever be called on some specific author or authors, and so that should be impossible!"
authorsUnknown [""] = []
authorsUnknown auts = filter (not . isAuthor) auts

authorsUnknownPrint :: String -> IO ()
authorsUnknownPrint auts = let missing = authorsUnknown $ splitOn ", " auts in
                             if null missing then return () else printRed "Authors unknown: " >> mapM_ printGreen missing

-- final database of alias→author rewrites: combine the handwritten with the generated.
-- WARNING: the two databases are required to be unique and non-overlapping; we could override generated with manual, but that kind of conflict indicates a semantic issue and must be dealt with by the user.
authorDB :: M.Map String String
authorDB = M.unionWithKey e
             CA.canonicals
            (M.fromList (concatMap name2Abbreviations CA.canonicalsWithInitials))
  where e zero one _ = error $ "Author.authorDB: there were two overlapping canonicalized results (both '" ++ one ++ "') for the input author name '" ++ zero ++ "'"

-- we allow empty strings for convenience in processing annotations
authorsLinkify :: T.Text -> [Inline]
authorsLinkify "" = []
authorsLinkify a = intersperse (Str ", ") $ map linkify $ T.splitOn ", " a

-- process an author text field and just extract the generated URLs:
authorsLinkifyAndExtractURLs :: T.Text -> [T.Text]
authorsLinkifyAndExtractURLs "" = []
authorsLinkifyAndExtractURLs aut =  extractURLs $ Pandoc nullMeta [Para $ authorsLinkify aut]

linkify :: T.Text -> Inline
linkify ""  = Space
linkify " " = Space
linkify aut -- skip anything which might be HTML:
  | isJust (T.find (== '<') aut) || isJust (T.find (== '>') aut) = Str aut' -- TODO: my installation of text-1.2.4.1 doesn't provide `T.elem`, so we use a more convoluted `T.find` call until an upgrade
  | otherwise = case M.lookup aut CA.authorLinkDB of
                  Nothing -> Str aut'
                  Just u ->  Link nullAttr [Str aut'] (u, "") -- TODO: authorsInitialize -- must be done inside the link-ification step, so skip for now; do we really want to initialize authors at all?
  where aut' :: T.Text
        aut' = T.takeWhile (/= '#') aut -- author disambiguation is done by appending an anchor-style disambig like '#foo'; once we have done the lookup, we no longer need it and delete it for display

authorPrioritize :: [T.Text] -> [(Int,T.Text)]
authorPrioritize auts = reverse $ frequency $ map fst $
  filter (\(_,b) -> isNothing b) $ map (\a -> (a, M.lookup a CA.authorLinkDB)) $
  filter (`notElem` CA.authorLinkBlacklist) auts

-- Compile a list of all authors, and calculate a weighted frequency list for them.
-- We weight authors by: # of annotations × # of backlinks (of each annotation).
-- This avoids the problem where some industrious geneticist or physicist winds up
-- in dozens of papers which have 100+ authors and gets prioritized at the top,
-- while some highly-influential author writes 2 URLs I link everywhere but
-- would languish forever at the bottom (despite a bio being highly useful
-- so it exposes the full set of backlinks).
-- Then open a set of search queries in a web browser to help define
-- the bio link of the ones without bio links.
authorBrowseTopN :: Metadata -> Int -> IO ()
authorBrowseTopN md n = do let mdl = M.toList md
                           bldb <- BL.readBacklinksDB
                           let bldbN = M.map (length . concatMap snd) bldb
                           let urlAuthorList = concatMap (\(u,(_,aut,_,_,_,_,_)) -> zip (repeat u) (split ", " aut)) mdl
                           let weightedList = map (\(u,a) -> (u, a, 1 + fromMaybe 0 (M.lookup (T.pack u) bldbN))) urlAuthorList
                           let unweightedList = map T.pack $ concatMap (\(_,a,n') -> replicate n' a) weightedList
                           let authorsTop = take n $ map snd $ authorPrioritize unweightedList
                           authorBrowseSearchEngines authorsTop

-- search for an author in various search engines to conveniently define a URL for an author:
-- generally, we want the WP article first, then a GS profile is often second-best, and if we can't find anything there, something in the wild west of the general Internet may be the only option.
authorBrowseSearchEngines :: [T.Text] -> IO ()
authorBrowseSearchEngines [] = error "Metadata.Author.authorBrowseSearchEngines: passed an empty list; this should never happen!"
authorBrowseSearchEngines [""] = error "Metadata.Author.authorBrowseSearchEngines: passed an empty string; this should never happen!"
authorBrowseSearchEngines authors = let urls = concatMap authorURLs authors
                                       in void $ runShellCommand "./" (Just [("DISPLAY", ":0")]) "chromium"
                                          (map T.unpack urls)
 where authorURLs :: T.Text -> [T.Text]
       authorURLs "" = error "Metadata.Author.authorURLs: passed an empty string for an author; this should never happen!"
       authorURLs author = let escapedAuthor = T.pack $ urlEncode $ T.unpack author
                               wpSearchURL = toWikipediaEnURLSearch author
                               gsURL = "https://scholar.google.com/scholar?q=" `T.append` escapedAuthor
                               gURL  = "https://www.google.com/search?q="      `T.append` escapedAuthor
                            in [wpSearchURL, gsURL, gURL]
