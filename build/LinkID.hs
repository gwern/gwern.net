{-# LANGUAGE OverloadedStrings #-}

module LinkID (authorsToCite, generateID, generateURL, getDisambiguatedPairs, metadataItem2ID, url2ID) where

import Data.Char (isAlphaNum, isPunctuation, toLower)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sortOn)
import Data.Maybe (fromJust, mapMaybe)
import Network.URI (uriFragment, parseURIReference)
import qualified Data.Text as T (null, pack, unpack, take, Text)
import qualified Data.Map.Strict as M (toList, fromListWith, (!))
import Text.Printf (printf)

-- hash IDs:
import qualified Crypto.Hash.SHA1 as SHA1 (hash)
import qualified Data.ByteString.Base64.URL as B64URL (encode)
import qualified Data.ByteString.Char8 as BS (take)
import qualified Data.Text.Encoding as TE (decodeUtf8, encodeUtf8)

import LinkMetadataTypes (Metadata, MetadataItem, Path)
import Utils (replace, replaceMany, deleteMany, sedMany, split, trim, delete, simplifiedHtmlToString)
import Config.Misc (currentYear)
import qualified Config.LinkID as C (linkIDOverrides)

-- Convert a URL/path to a 9-character URL-safe Base64 ID, using SHA-1.
-- This is the 'universal' fallback ID for all URLs/paths where there isn't enough metadata to create a human-readable citation-style ID like "foo-2020".
-- It ensures we can always define backlinks for URLs (eg. in link-bibliographies) as the targets of `<a>` links, as the IDs will always be safe to use as a hash like '#ID'.
--
-- Implementation: We use a Web Crypto browser-available hash (SHA-1, like LinkArchive), encoded into URL-safe Base64 (eg. 'https://example.com' → 'Mnw_2ofO'), which we truncate to a short length (9 characters) which is readable & will not bloat the HTML *too* much, but which is long enough that it should have near-zero collision probability over the expected scale of Gwern.net for the foreseeable future (<1m metadata-less URL paths).
-- Collisions are not necessarily *too* harmful, but if they happen, the author is expected to resolve them by either adding metadata to offending links or manually overriding link IDs in `Config.LinkID`.
url2ID :: T.Text -> T.Text
url2ID "" = error "LinkID.url2ID: passed empty string as a URL/path to hash into an ID, which should never happen."
url2ID url = T.take 9 $ TE.decodeUtf8 $ B64URL.encode $ BS.take 6 hash -- 6 bytes / 48 bits
  where
    hash = SHA1.hash (TE.encodeUtf8 url)

-- convenience wrapper around `generateID`:
metadataItem2ID :: Path -> MetadataItem -> T.Text
metadataItem2ID "" mi = error $ "LinkID.metadataItem2ID: passed an empty URL for an ID; metadata item was: " ++ show mi
-- we choose to not require non-empty author/dates, to allow convenient application to the entire metadata database, like to look for colliding IDs:
metadataItem2ID u (_,author,date,_,_,_,_) = generateID u author date

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
generateID :: String -> String -> String -> T.Text
generateID url author date
  -- hardwire tricky cases where unique IDs can't easily be derived from the URL/metadata:
  | any (\(u,_) -> u == url) C.linkIDOverrides = fromJust $ lookup url C.linkIDOverrides
  -- indexes or tag-directories shouldn't be cited/have IDs (often linked many times on a page)
  | ("https://gwern.net" `isPrefixOf` url || "/" `isPrefixOf` url) && ("/index" `isSuffixOf` url) = ""
  -- eg. '/face' = '#gwern-face'; `generateID "https://gwern.net/font" "Gwern Branwen" "2021-01-01"` → "gwern-font" (since we are using the short URL/slug, we don't need a year/date to disambiguate, and those are often meaningless on Gwern.net anyway).
  -- NOTE: we have hitherto not set IDs on *section* or *anchor* links like '/improvement#microsoft'. Those got no ID, because no authorship metadata is available (unless metadata had been manually added via an annotation for that URL specifically). If we *assume*, no contrary metadata being available, that they were written by me, then they would get an ID like 'gwern-improvement-microsoft'. (Tacking on the hash to the baseline ID of '/improvement' → 'gwern-improvement'.)
  | ("Gwern Branwen" == author || "gwern" == author || "Gwern" == author || "" == author) &&
    (("/" `isPrefixOf` url') && notElem '.' url' && not ("/index"`isInfixOf`url'))
  = T.pack (trim $ replaceMany [(".", "-"), ("/", "-"), ("#", "--"), ("'", ""), ("https://", "")] $ map toLower $ "gwern-"++tail url')
  -- skip the ubiquitous WP links: I don't repeat WP refs, and the identical author/dates impedes easy cites/links anyway.
  | "https://en.wikipedia.org/wiki/" `isPrefixOf` url = ""
  -- _shikata ga nai_, not enough metadata; we use the hash ID fallback:
  | author == "" || date == "" = url2ID (T.pack url)
  -- 'Foo 2020' → '#foo-2020'; 'Foo & Bar 2020' → '#foo-bar-2020'; 'foo et al 2020' → 'foo-et-al-2020'
  | otherwise = T.pack $ citeToID $ authorsToCite url author date
  where
    url' = delete "https://gwern.net" url

-- attempt to guess the URL for a specific annotation somewhere in the tag-directories for easier reference (used in `gwa` dumps)
generateURL :: Path -> MetadataItem -> String
generateURL _ (_,_,_,_,_,[],_) = ""
generateURL url (_,a,d,_,_,ts,_) = let ident = T.unpack $ generateID url a d in
                                     if null ident then "" else
                                       "https://gwern.net/doc/" ++ head ts ++ "/index#" ++ ident ++ "-section"

authorsToCite :: String -> String -> String -> String
authorsToCite url author date =
  let year = if date=="" then show currentYear else take 4 date -- YYYY-MM-DD
      authors = map (takeWhile (/= '#')) $ split ", " $ sedMany [(" \\([A-Za-z ]+\\)", ""), (" \\[[A-Za-z ]+\\]", "")] author -- affiliations like "Schizophrenia Working Group of the Psychiatric Genomics Consortium (PGC), Stephan Foo" or "Foo Bar (Atlas Obscura)" or /doc/math/humor/1976-barrington.pdf's "John Barrington [Ian Stewart]" (the former is a pseudonym) would break the later string-munging & eventually the HTML
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
                           if suffix == url then "" else [['a'..'z'] !! ((read suffix :: Int) - 1)]  ) ++ extension

           in
           if authorCount >= 3 then
                           firstAuthorSurname ++ " et al " ++ year ++ suffix' else
                             if authorCount == 2 then
                               let secondAuthorSurname = filter (\c -> isAlphaNum c || isPunctuation c) $ reverse $ takeWhile (/=' ') $ reverse (authors !! 1) in
                                 firstAuthorSurname ++ " & " ++ secondAuthorSurname ++ " " ++ year ++ suffix'
                             else
                               firstAuthorSurname ++ " " ++ year ++ suffix'
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
        let ident = metadataItem2ID url item
        in if T.null ident then Nothing else Just (ident, [url])
    ) $
    M.toList md
  where
    processDuplicates :: (T.Text, [Path]) -> [(Path, String)]
    processDuplicates (ident, urls) =
        let padding = length (show (length urls))
            sortedUrls = sortOn (metadataItem2ID (T.unpack ident) . (md M.!)) urls
        in zipWith (\url (n :: Int) -> (url, T.unpack ident ++ "-" ++ printf ("%0" ++ show padding ++ "d") n)) sortedUrls [1..]
