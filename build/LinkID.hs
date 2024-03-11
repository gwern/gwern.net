{-# LANGUAGE OverloadedStrings #-}

module LinkID (authorsToCite, generateID, generateURL) where

import Data.Char (isAlphaNum, isPunctuation, toLower)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (fromJust)
import Network.URI (uriFragment, parseURIReference)
import qualified Data.Text as T (pack, unpack, Text)

import LinkMetadataTypes (MetadataItem, Path)
import Utils (replace, replaceMany, sedMany, split, trim)
import Config.Misc (currentYear)
import qualified Config.LinkID as C (linkIDOverrides)

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
  = T.pack (trim $ replaceMany [(".", "-"), ("--", "-"), ("/", "-"), ("#", "-"), ("'", ""), ("https://", "")] $ map toLower $ "gwern-"++tail url')
  -- skip tag links:
  -- skip the ubiquitous WP links: I don't repeat WP refs, and the identical author/dates impedes easy cites/links anyway.
  | "https://en.wikipedia.org/wiki/" `isPrefixOf` url = ""
  -- shikata ga nai:
  | author == "" = ""
  | date   == "" = ""
  -- 'Foo 2020' → '#foo-2020'; 'Foo & Bar 2020' → '#foo-bar-2020'; 'foo et al 2020' → 'foo-et-al-2020'
  | otherwise = T.pack $ citeToID $ authorsToCite url author date
  where
    url' = replace "https://gwern.net" "" url

-- attempt to guess the URL for a specific annotation somewhere in the tag-directories for easier reference (used in `gwa` dumps)
generateURL :: Path -> MetadataItem -> String
generateURL _ (_,_,_,_,_,[],_) = ""
generateURL url (_,a,d,_,_,ts,_) = let ident = T.unpack $ generateID url a d in
                                     if null ident then "" else
                                       "https://gwern.net/doc/" ++ head ts ++ "/index#" ++ ident ++ "-section"

authorsToCite :: String -> String -> String -> String
authorsToCite url author date =
  let year = if date=="" then show currentYear else take 4 date -- YYYY-MM-DD
      authors = split ", " $ sedMany [(" \\([A-Za-z ]+\\)", ""), (" \\[[A-Za-z ]+\\]", "")] author -- affiliations like "Schizophrenia Working Group of the Psychiatric Genomics Consortium (PGC), Stephan Foo" or "Foo Bar (Atlas Obscura)" or /doc/math/humor/1976-barrington.pdf's "John Barrington [Ian Stewart]" (the former is a pseudonym) would break the later string-munging & eventually the HTML
      authorCount = length authors
      firstAuthorSurname = if authorCount==0 then "" else filter (\c -> isAlphaNum c || isPunctuation c) $ reverse $ takeWhile (/=' ') $ reverse $ replaceMany [(" Senior",""), (" Junior" ,"")] $ head authors -- 'John Smith Junior 2020' is a weird cite if it turns into 'Junior 2020'! easiest fix is to just delete it, so as to get the expected 'Smith 2020'.
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
