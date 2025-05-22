{-# LANGUAGE OverloadedStrings #-}

module LinkID (authorsToCite, generateID, generateURL, getDisambiguatedPairs, metadataItem2ID, url2ID, id2URLdb, writeOutID2URLdb, isValidID) where

import Control.Monad (replicateM)
import Data.Char (isAlphaNum, isPunctuation, toLower, isDigit, isLetter, isAsciiLower, isAsciiUpper, ord)
import Data.List (isInfixOf, isPrefixOf, sortOn, elemIndex, sort, isSuffixOf)
import Data.Maybe (fromJust, mapMaybe)
import Network.URI (uriFragment, parseURIReference)
import qualified Data.Text as T (append, null, pack, unpack, take, Text)
import qualified Data.Map.Strict as M (toList, fromListWith, (!), mapWithKey, lookup)
import Text.Printf (printf)

import Data.Array (accumArray, assocs, Array)

-- hash IDs:
import qualified Crypto.Hash.SHA1 as SHA1 (hash)
import qualified Data.ByteString.Base64.URL as B64URL (encode)
import qualified Data.ByteString.Char8 as BS (take)
import qualified Data.Text.Encoding as TE (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LBS (toStrict)

-- serializing the JSON maps for client-side JS browser use:
import Data.Aeson (object, (.=), encode)
import qualified Data.Aeson.Key as Key (fromText)

import LinkMetadataTypes (Metadata, MetadataItem, Path)
import Utils (replace, replaceMany, deleteMany, sedMany, split, trim, delete, simplifiedHtmlToString, writeUpdatedFile)
import qualified Config.Misc as CM (currentYear, cd, authorL)
import qualified Config.LinkID as C (linkIDOverrides)

-- Convert a URL/path to a 9-character URL-safe Base64 (the 64-character range [a-zA-Z0-9_-] or "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-") ID, using SHA-1.
-- This ID must start with an underscore, to uniquely identify hash-IDs from manually-written or citation-inferred cites (which themselves must not start with an underscore).
-- This is the 'universal' fallback ID for all URLs/paths where there isn't enough metadata to create a human-readable citation-style ID like "foo-2020". (Note that the two kinds of IDs are not mutually exclusive: "foo-2020" is also a possible hash value.)
-- It ensures we can always define backlinks for URLs (eg. in link-bibliographies) as the targets of `<a>` links, as the IDs will always be safe to use as a hash like '#ID'.
--
-- Implementation: We use a Web Crypto browser-available hash (SHA-1, like LinkArchive), encoded into URL-safe Base64 (eg. 'https://example.com' → '_Mnw_2ofO'), which we truncate to a short length (9 characters) which is readable & will not bloat the HTML *too* much, but which is long enough that it should have near-zero collision probability over the expected scale of Gwern.net for the foreseeable future (<1m metadata-less URL paths).
-- Collisions are not necessarily *too* harmful, but if they happen, the author is expected to resolve them by either adding metadata to offending links or manually overriding link IDs in `Config.LinkID`.
url2ID :: T.Text -> T.Text
url2ID "" = error "LinkID.url2ID: passed empty string as a URL/path to hash into an ID, which should never happen."
url2ID url = T.append "_" $ T.take 8 $ TE.decodeUtf8 $ B64URL.encode $ BS.take 6 hash -- 6 bytes / 48 bits
  where
    hash = SHA1.hash (TE.encodeUtf8 url)

-- | Validates whether a string is a valid ID
-- Valid IDs are either:
-- 1. Exactly 8 URL-safe Base64 characters [a-zA-Z0-9_-]
-- 2. Structured IDs with letters (including Unicode), digits, and hyphens; double-hyphens are permitted to encode page-sections.
isValidID :: String -> Bool
isValidID ""        = False
isValidID s
    | length s == 9 = (head s == '_' && all isBase64Char s) || isStructuredID s
    | otherwise     = isStructuredID s
  where
    -- URL-safe Base64 character set [a-zA-Z0-9_-]
    isBase64Char c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '-'

    -- Structured ID validation
    isStructuredID str =
        let nonEmpty = not (null str)
            noLeadingHyphen = not (isPrefixOf "-" str)
            noTrailingHyphen = not (isSuffixOf "-" str)
            validChars = all isValidStructuredChar str
            nonHashPrefixed = head str /= '_'
        in nonEmpty && validChars && noLeadingHyphen && noTrailingHyphen && nonHashPrefixed

    -- Allow any letter (including Unicode), digits, and hyphens; we accept all non-ASCII Unicode characters because it's quite difficult to define invalid Unicode characters given all the surnames floating around; we also have to permit '_' because too many usernames include it as a space separator...
    isValidStructuredChar c = isLetter c || isDigit c || c == '-' || c == '_' || ord c >= 128

-- convenience wrapper around `generateID`:
metadataItem2ID :: Metadata -> Path -> MetadataItem -> T.Text
metadataItem2ID _ "" mi = error $ "LinkID.metadataItem2ID: passed an empty URL for an ID; metadata item was: " ++ show mi
-- we choose to not require non-empty author/dates, to allow convenient application to the entire metadata database, like to look for colliding IDs:
metadataItem2ID md u (_,author,date,_,_,_,_) = generateID md u author date

-- To ensure unique-ish links (see /design#backlink on why this is important), duplicate annotation links should be handled:
--
-- 1. all citations like 'Foo & Bar 1990' or 'Quux et al 2020' should be hyperlinked (either as a internal anchor or fulltext link);
-- 2. annotated links get a predictable anchor ID generated from the metadata, like '#foo-et-al-2020' (ie. grab the first 4 characters of the date, check
--    the number of commas in the author field to decide if 'foo 1990' or 'foo & bar 1990' or 'foo et al 1990' etc);
-- 3. duplicate links will, then, generate invalid HTML as two Foo et al 2020s (which must be links per #1) will both define id='#foo-et-al-2020', and this will trigger htmltidy errors/warnings on sync; so, one of them will be manually edited to either point to another instance which
--    is part of a larger discussion/context, or be given a manual ID like id='#foo-et-al-2020-2'. (since the annotation is based on the URL not the
--    ID, this doesn't affect the annotations.)
--
-- so, all citations have a hyperlink, supporting hypertextual reading or readers who didn't happen to
-- memorize the previous use in the page, independent instances of links remain independent while back/forward
-- references pop up the relevant section with the annotated link in context, htmltidy automatically detects links that need to be updated, and a
-- regexp can warn about citation-text which needs to be linkified.
generateID :: Metadata -> String -> String -> String -> T.Text
generateID md url author date
  -- hardwire tricky cases where unique IDs can't easily be derived from the URL/metadata:
  | any (\(u,_) -> u == url) C.linkIDOverrides = blacklistIDs $ fromJust $ lookup url C.linkIDOverrides
  -- otherwise, the annotation may include an ID key-value which overrides the hash or surname-date
  | otherwise = case M.lookup url md of
                 Just (_,_,_,_,kvs,_,_) -> case lookup "id" kvs of
                                             Nothing    -> blacklistIDs $ generateID'
                                             Just ident -> blacklistIDs $ T.pack ident
                 _ -> blacklistIDs $ generateID'
 where
  url' :: String
  url' = delete "https://gwern.net" url
  blacklistIDs i = if i `notElem` (["9jvwHKDX","CgaXg9w7","CrWYuUGd","J4QE_Ni0","NOJxosKS","Nt-HgfVu","TdyKOIAM","YffqVj7A","abojh9Co","dF10QFII","jJtPk-Ln","kM7gdAh1","krywxZcd","vpFpfPxe","zVk76M9i"]::[T.Text]) then i else error ("LinkID.generateID.blacklistIDs: forbidden ID generated! Inputs were: " ++ show [url, author, date]) -- indicate interwiki or inflation gone wrong
  generateID' :: T.Text
  generateID'
    | head url' `elem` ['!', '$', '₿'] = error $ "LinkID.generateID.generateID': invalid pseudo-URL passed in. Inputs were: "  ++ show [url, author, date]
    -- is it a /blog/ self-post? If so, we can infer the ID immediately from the slug, avoiding needing to add a manual ID to its annotation, reducing friction; eg. '/blog/2025/large-files' → 'gwern-2025-large-files'
    | "/blog/2" `isPrefixOf` url' = T.pack (CM.authorL ++ "-" ++ replace "/" "-" (delete "/blog/" url'))
  -- indexes or tag-directories shouldn't be cited as they would be often linked many times on a page due to transcludes:
  -- | ("https://gwern.net" `isPrefixOf` url || "/" `isPrefixOf` url) && ("/index" `isSuffixOf` url) = ""
  -- eg. '/face' = '#gwern-face'; `generateID "https://gwern.net/font" "Gwern Branwen" "2021-01-01"` → "gwern-font" (since we are using the short URL/slug, we don't need a year/date to disambiguate, and those are often meaningless on Gwern.net anyway).
  -- NOTE: we have hitherto not set IDs on *section* or *anchor* links like '/improvement#microsoft'. Those got no ID, because no authorship metadata is available (unless metadata had been manually added via an annotation for that URL specifically). If we *assume*, no contrary metadata being available, that they were written by me, then they would get an ID like 'gwern-improvement-microsoft'. (Tacking on the hash to the baseline ID of '/improvement' → 'gwern-improvement'.)
    | ("Gwern Branwen" == author || "gwern" == author || "Gwern" == author || "" == author) &&
      (("/" `isPrefixOf` url') && notElem '.' url' && not ("/index"`isInfixOf`url'))
    = T.pack (trim $ replaceMany [(".", "-"), ("/", "-"), ("#", "--"), ("'", ""), ("https://", "")] $ map toLower $ "gwern-"++tail url')
    -- skip the ubiquitous WP links: I don't repeat WP refs, and the identical author/dates impedes easy cites/links anyway (forcing use of hashes).
    -- | "https://en.wikipedia.org/wiki/" `isPrefixOf` url = ""
    | "#" `isPrefixOf` url = "" -- HACK/TODO: skip self-links: often there's a bunch of them because we may repeatedly refer to a particular section on a page (is this safe? do we actually need self-links to have unique IDs per use?)
    -- _shikata ga nai_, not enough metadata; we use the hash ID fallback:
    | author == "" || date == "" = url2ID (T.pack url)
    -- 'Foo 2020' → '#foo-2020'; 'Foo & Bar 2020' → '#foo-bar-2020'; 'foo et al 2020' → 'foo-et-al-2020'
    | otherwise = T.pack $ citeToID $ authorsToCite url author date

authorsToCite :: String -> String -> String -> String
authorsToCite url author date =
  let year = if date=="" then show CM.currentYear else take 4 date -- YYYY-MM-DD
      authors = map (takeWhile (/= '#')) $ split ", " $ sedMany [(" \\([A-Za-z ]+\\)", ""), (" \\[[A-Za-z ]+\\]", "")] author -- affiliations like "Schizophrenia Working Group of the Psychiatric Genomics Consortium (PGC), Stephan Foo" or "Foo Bar (Atlas Obscura)" or /doc/math/humor/lion-hunting/1976-barrington.pdf's "John Barrington [Ian Stewart]" (the former is a pseudonym) would break the later string-munging & eventually the HTML
      authorCount = length authors
      firstAuthorSurname = if authorCount==0 then "" else filter (\c -> isAlphaNum c || isPunctuation c) $ reverse $ takeWhile (/=' ') $ reverse $ deleteMany [" Senior", " Junior"] $ simplifiedHtmlToString $ head authors -- 'John Smith Junior 2020' is a weird cite if it turns into 'Junior 2020'! easiest fix is to just delete it, so as to get the expected 'Smith 2020'.
  in
       if authorCount == 0 then "" else
           let
             -- hashes: "https://www.google.com/foo.pdf#page=14" → "-page-14"; this is most useful for cases where we link a PDF but also specific pages in it, which produces colliding ids (eg. '/doc/psychology/2013-kurzban.pdf#page=14' vs '/doc/psychology/2013-kurzban.pdf')
             -- We skip this for annotations like '#deepmind', however. There's no need to have to write IDs like 'silver-et-al-2016-deepmind', when 'silver-et-al-2016' is entirely adequate for a unique short ID.
             extension = if not ("#page=" `isInfixOf` url) then "" else
                           " (" ++ drop 1 (map (\c -> if isAlphaNum c then c else ' ') $ uriFragment $ fromJust $ parseURIReference url) ++ ")"
             -- handle cases like '/doc/statistics/peer-review/1975-johnson.pdf' vs '/doc/statistics/peer-review/1975-johnson-2.pdf'
             suffix' = (let suffix = sedMany [("^/doc/.*-([0-9][0-9]?)\\.[a-z]+$", "\\1")] url in
                          -- eg. "/doc/economics/2019-brynjolfsson-3.pdf" → "Brynjolfsson et al 2019c"
                           if suffix == url then "" else acronymGenerator !! (((read suffix :: Int) - 1)`max`0) ) ++ extension

           in
           if authorCount >= 3 then
                           firstAuthorSurname ++ " et al " ++ year ++ suffix' else
                             if authorCount == 2 then
                               let secondAuthorSurname = filter (\c -> isAlphaNum c || isPunctuation c) $ reverse $ takeWhile (/=' ') $ reverse (authors !! 1) in
                                 firstAuthorSurname ++ " & " ++ secondAuthorSurname ++ " " ++ year ++ suffix'
                             else
                               firstAuthorSurname ++ " " ++ year ++ suffix'
  where -- lazily generate all alphabetical concatenations in order: 'a'...'aa'...'zz'...'aaa' etc; this ensures we never run out of suffixes
     acronymGenerator :: [String]
     acronymGenerator = (concatMap (flip replicateM ['a'..'z']) [1..])
citeToID :: String -> String
citeToID = filter (\c -> c/='.' && c/='\'' && c/='’'&& c/='('&&c/=')') . map toLower . replace " " "-" . replace " & " "-"

-- find all ambiguous link IDs in the current metadata database, and print out along with a '-n' disambiguation for adding to the link ID overrides in `Config.LinkID`:
getDisambiguatedPairs :: Metadata -> [(Path, String)]
getDisambiguatedPairs md = sortOn snd $ -- sort by the new IDs, to make it easier to see what URLs are disambiguated from each other
    concatMap processDuplicates $
    filter (\(_, urls) -> length urls > 1) $
    M.toList $
    M.fromListWith (++) $
    mapMaybe (\(url, item) ->
        let ident = metadataItem2ID md url item
        in if T.null ident then Nothing else Just (ident, [url])
    ) $
    M.toList md
  where
    processDuplicates :: (T.Text, [Path]) -> [(Path, String)]
    processDuplicates (ident, urls) =
        let padding = length (show (length urls))
            sortedUrls = sortOn (metadataItem2ID md (T.unpack ident) . (md M.!)) urls
        in zipWith (\url (n :: Int) -> (url, T.unpack ident ++ "-" ++ printf ("%0" ++ show padding ++ "d") n)) sortedUrls [1..]

-- create a mapping of ID → URL for easier search.
--
-- Useful for creating the JSON maps to power the client-side /ref/ annotation queries. We split them by the first character of the ID (minus the common underscore for hashes), so that we only need to query 1 small JSON file instead of the entire DB. If necessary, we can split them further, recursively, until the download size is tolerable.
-- (We also provide an `all.json` which contains all of them in reversed order, (URL,ID), for the occasional rare query-by-URL rather than query-by-ID.)
-- Then the JS can look at the current URL `/ref/$ID`, take the first character of $ID, download the relevant JSON dictionary (<100kb on the wire), look up the corresponding URL, and display its annotation the usual way. (The prefixes are limited to the URL-safe Base-64 subset; any characters not inside that, like Unicode from foreign surnames, is put into the final entry, for '-'.)
-- This enables stable easy links to arbitrary annotations, which currently can only be awkwardly linked as unstable section anchor-links in tag-directories.
-- The /ref/ URLs are directly exposed to readers in the popup/popover title-bar, where they replace the original URL (which makes sense conceptually: all the other buttons in the title-bar refer to the popup/annotation itself, rather than to the contents or the URL).
--
-- If this isn't efficient enough, perhaps because the list of IDs is *extremely* large (for a much larger site, or perhaps due to assigning IDs to every paragraph or block element), a more scalable approach might ultimately be to adopt the HTTP range-query trick: create a single large sorted line-delimited list, and then the client does binary search through it using <https://en.wikipedia.org/wiki/Byte_serving> queries to download only a few kilobytes at a time to check indices and compute the next offset to query. This allows efficient querying of even gigabyte-sized databases, using only log queries + a few kilobytes total payload. (We can even support complicated queries through running SQLite3 in JavaScript and doing range queries onto a server-side database! <https://github.com/psanford/sqlite3vfshttp> <https://phiresky.github.io/blog/2021/hosting-sqlite-databases-on-github-pages/> <https://ansiwave.net/blog/sqlite-over-http.html>)
id2URLdb :: Metadata -> [(String, Path)]
id2URLdb md = map (\(url,ident) -> (T.unpack ident,url)) $
              sort $ -- URLs are much more compressible than random IDs, so we'll sort by the value (URL) instead of key (ID), to let URLs compress better with each other & save some bytes on the wire
              M.toList $ M.mapWithKey (metadataItem2ID md) md

shardByCharPrefix :: [(String, Path)] -> [(Char, [(String, Path)])]
shardByCharPrefix xs = [ (alphabet !! i, group) | (i, group) <- assocs arr ]
  where
    alphabet :: String
    alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-"

    arr :: Array Int [(String,Path)]
    arr = accumArray (flip (:)) [] (0, 63)
          [ (index s, (s, p)) | (s, p) <- xs ]

    index :: String -> Int
    index []     = error "Key is empty"
    -- hash-IDs are always prefixed with underscore, so detect those and strip '_' when looking it up:
    index ('_':c:_) = case elemIndex c alphabet of
                        Nothing -> 63  -- dump Unicode-prefix IDs into final fallback entry, '-'
                        Just i  -> i
    -- otherwise just look it up normally:
    index (c:_) = case elemIndex c alphabet of
                      Nothing -> 63
                      Just i  -> i

tupleList2JSONString :: [(String, Path)] -> T.Text
tupleList2JSONString xs =
  TE.decodeUtf8 . LBS.toStrict . encode $
    -- we'll pretty-print them later with `jq` in the post-site-compilation reformatting passes
    object [ Key.fromText (T.pack key) .= T.pack path | (key, path) <- xs ]

writeOutID2URLdb :: Metadata -> IO ()
writeOutID2URLdb md = do let dbl = id2URLdb md
                         let sharded = shardByCharPrefix dbl
                         let allReversed = sort $ map (\(a,b)->(b,a)) $ dbl
                         CM.cd
                         writeUpdatedFile "id-all" "metadata/annotation/id/all.json" (tupleList2JSONString allReversed)
                         mapM_ (\(char,shard) -> writeUpdatedFile "id-shard" ("metadata/annotation/id/" ++ [char] ++ ".json") (tupleList2JSONString shard)) sharded

-- return the /ref/ URL for a specific annotation somewhere for easier linking (used in `gwa` dumps)
generateURL :: Metadata -> Path -> MetadataItem -> String
generateURL _ _ (_,_,_,_,_,[],_) = ""
generateURL md url x@(_,a,d,_,_,_,_) =
  let ident = T.unpack $ generateID md url a d in
    if null ident then "" else
      if not (isValidID ident) then
        error $ "LinkID.generateURL: invalid ID generated? ID was: " ++ show ident ++ "; " ++ show url ++ show x
      else "https://gwern.net/ref/" ++ ident
