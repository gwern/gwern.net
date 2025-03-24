{-# LANGUAGE OverloadedStrings #-}
module Annotation (linkDispatcher, tooltipToMetadata, htmlDownloadAndParseTitleClean, processItalicizer, testGuessAuthorDate) where

import Data.List (isPrefixOf, isInfixOf, intercalate)
import Text.Pandoc (Inline(Link))

import Annotation.Biorxiv (biorxiv)
import Annotation.Gwernnet (gwern)
import Annotation.PDF (pdf)
import Annotation.OpenReview (openreview)
import Annotation.Arxiv (arxiv)
import LinkMetadataTypes (Failure(..), MetadataItem, Path, Metadata)
import Metadata.Format as MF (linkCanonicalize, cleanAbstractsHTML)
import Metadata.Date (guessDateFromString, isDate)
import Metadata.Author (extractTwitterUsername, isAuthor, authorsUnknownPrint)
import Metadata.Title (tooltipToMetadata, wikipediaURLToTitle, htmlDownloadAndParseTitleClean)
import Typography (typesetHtmlFieldPermanent)
import Utils (replace, anyPrefix, printGreen, printRed, trim, delete)
import qualified Config.Misc as C (todayDayString, cd, todayDayStringUnsafe)
import qualified Data.Text as T (unpack)

import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import System.Exit (ExitCode(ExitFailure))
import Data.FileStore.Utils (runShellCommand)
import Text.Show.Pretty (ppShow)

import Data.List.Split  (splitOn)
import Data.Char (isDigit)
import System.FilePath (takeBaseName)

-- 'new link' handler: if we have never seen a URL before (because it's not in the metadata database), we attempt to parse it or call out to external sources to get metadata on it, and hopefully a complete annotation.
linkDispatcher :: Metadata -> Inline -> IO (Either Failure (Path, MetadataItem))
linkDispatcher md (Link _ _ (l, tooltip)) =
 do let l' = MF.linkCanonicalize $ T.unpack l
    mi <- linkDispatcherURL md l'

    today <- C.todayDayString
    let defaultCreatedToToday d = if null d then today else d

    case mi of
      -- apply global per-field rewrites here
      Right (l'',(title,author,dateRaw,dc,kvs,tags,abstract)) ->
        do date <- if dateRaw /= "" then return dateRaw else guessDateFromString (title ++ " : " ++ l'')
           title' <- reformatTitle title
           print $ "guessAuthorDateFromPath : " ++ show (l'',(title',author,date,defaultCreatedToToday dc,kvs,tags,abstract))
           authorsUnknownPrint author
           return $ Right $ guessAuthorDateFromPath (l'',(title',author,date,defaultCreatedToToday dc,kvs,tags,abstract))
      Left Permanent -> do let (title,author,date') = tooltipToMetadata l' (T.unpack tooltip)
                           title' <- reformatTitle title
                           print ("Left Permanent"::String)
                           let guess = guessAuthorDateFromPath (l',(title',author,date',defaultCreatedToToday "",[],[],""))
                           authorsUnknownPrint author
                           return (Right guess)
      Left Temporary -> return mi
linkDispatcher _ x = error ("Annotation.linkDispatcher passed a non-Link Inline element: " ++ show x)

-- NOTE: we cannot simply put this in `typesetHtmlField`/`MF.cleanAbstractsHTML` because while a space-separated hyphen in a *title* is almost always an em-dash, in an *abstract*, it often is meant to be an en-dash or a minus sign instead. So if we want to clean those up across all titles, we have to confine it to title fields only.
reformatTitle :: String -> IO String
reformatTitle t = do
  t' <- processItalicizer t
  return $ typesetHtmlFieldPermanent True . MF.cleanAbstractsHTML . replace " – " "—" . replace " - " "—" $ t'

linkDispatcherURL :: Metadata -> Path -> IO (Either Failure (Path, MetadataItem))
linkDispatcherURL md l
  | anyPrefix l ["/metadata/annotation/backlink/", "/metadata/annotation/similar/", "/doc/www/", "/ref/", "/blog/", "irc://", "mailto:"] =
      return (Left Permanent)
  -- WP is now handled by annotations.js calling the Mobile WP API; we pretty up the title for tags.
  | "https://en.wikipedia.org/wiki/" `isPrefixOf` l = return $ Right (l, (wikipediaURLToTitle l, "", "", "", [], [], ""))
  | "arxiv.org/abs/" `isInfixOf` l = arxiv md l
  | "https://openreview.net/forum?id=" `isPrefixOf` l || "https://openreview.net/pdf?id=" `isPrefixOf` l =
      openreview md l
  | anyPrefix l ["https://www.biorxiv.org/content/", "https://www.medrxiv.org/content/"] = biorxiv md l
  | "https://x.com/" `isPrefixOf` l = twitter l
  | null l = return (Left Permanent)
  -- locally-hosted PDF?
  | ".pdf" `isInfixOf` l = let l' = MF.linkCanonicalize l in if head l' == '/' then pdf md l' else return (Left Permanent)
  | otherwise = let l' = MF.linkCanonicalize l in if head l' == '/' then gwern md l
  -- And everything else is unhandled:
     else do title <- htmlDownloadAndParseTitleClean l
             if title /= "" then return (Right (l, (title, "", "", "", [], [], "")))
               else return (Left Permanent)

--- currently can only extract Twitter username due to difficulty scraping/parsing Twitter
twitter :: Path -> IO (Either Failure (Path, MetadataItem))
twitter u = return $ Right (u, ("", extractTwitterUsername u, "", "", [], [], ""))

processItalicizer :: String -> IO String
processItalicizer "" = return ""
processItalicizer t =
      if "<em>" `isInfixOf` t || t `elem` whitelist then return t
      else do
              C.cd
              (status,_,mb) <- runShellCommand "./" Nothing "python3" ["static/build/italicizer.py", t]
              case status of
                ExitFailure err -> printGreen (intercalate " : " [t, ppShow status, ppShow err, ppShow mb]) >> printRed "Italicizer failed!" >> return t
                _ -> let t' = trim $ U.toString mb in
                  -- verify that no change happened other than adding italics:
                  if t' == "\"\"" || t' == ""
                  || delete "<em>" (delete "</em>" t') /= t then return t
                  else return t'
  where whitelist = ["Guys and Dolls", "A critique of pure reason"]

guessAuthorDateFromPath :: (Path, MetadataItem) -> (Path, MetadataItem)
guessAuthorDateFromPath x@(l, (title,author,dateRaw,dc,kvs,tags,abstract))
 | null title || null author = let (author',date') = guessAuthorDateFromString l
                                     in (l, (title
                                            , if null author  then author' else author
                                            , if null dateRaw then date'   else dateRaw
                                            , dc,kvs,tags,abstract))
 | otherwise = x

-- Best-effort attempt to conservatively guess the date & author from a file path. (Authors must be whitelisted in `Config.Metadata.Author`, and dates must be valid dates between 1900 & today.)
--
-- This tries to save manual toil in annotating files with the 'obvious' metadata (date & author), but does so conservatively, to avoid creating fake metadata which may never be detected, and may be hard to fix once it is.
--
-- Gwern.net file-paths follow a rough convention of 'YYYY-MM-DD-surname-description.ext'.
-- Variations include: 'YYYY-MM-surname.ext', 'YYYY-surname.ext', 'YYYY-MM-DD-YYYY-MM-DD-surname-description.ext' (in the case of two valid dates, the later should be selected, as a file cannot be created earlier than the end of the date-range it contains).
-- Examples of extracting the raw date & author:
--
-- - /doc/ai/scaling/hardware/2020-08-05-hippke-measuringhardwareoverhang-chessscaling19902020.png : ("2020-08-05", "hippke")
-- - /doc/darknet-market/2012-05-02-tony76postmortem.mht : ("2012-05-02", "")
-- - /doc/dual-n-back/2012-05-30-kundu-dnbrapm.xls : ("2012-05-30", "kundu")
-- - /doc/traffic/2012-01-2012-07-gwern-traffic-history.png : ("2012-07", "gwern")
-- - /doc/traffic/2015-07-03-2016-01-03-gwern-gwern.net-analytics.pdf : ("2016-01-03", "gwern")
-- - /doc/wikipedia/2023-10-01-gwern-tla-lettervsunusedtlaswiththatletterpercentageoverthealphabet.png : ("2023-10-01", "gwern")
-- - /doc/zeo/2011-01-18-gwern.png : ("2011-01-18", "gwern")
-- - /doc/ai/music/2019-11-08-gpt2-irish-spaceless-100medley.mp3 : ("2019-11-08", "gpt-2")
-- - /doc/biology/2010-wrangham.pdf : ("2010", "wrangham")
-- - /doc/2012-01-2012-77-gwern-file.ext : ("", "gwern")
--
-- The raw date & author are then sanity-checked to drop suspicious guesses and return an empty string instead for that:
--
-- - Author names are typically ASCII alphanumerical characters, with occasional punctuation mixed in (the main exception is '_'). Author surnames are checked using `Metadata.Author.isAuthor :: String -> Bool`; this handles transformations as well, so lower-case names like 'gwern' will be valid (because it is canonicalized to 'Gwern', which is a known author). All exceptional cases should be handled by `Metadata.Author`, so if it is not a known author, we drop it and return an empty string.
-- - Dates should be sensible, defined as: not set in the future (ie. no later than `Config.Misc.todayDayStringUnsafe :: String`, which returns a string like "2025-03-13"), and not too far in the past (let's say, before "1900-01-01" is too old). Spurious precision is not added: a filename of '2010' is stored as '2010', to avoid a spuriously precise '2010-01-01', and '2010-01' as '2010-01', and so on. Finally, we check that dates are in fact valid with `Metadata.Date.isDate :: String -> Bool` (as a simple number regex can match invalid dates); all dates must be valid, because if they are not, that suggests we have misparsed a strange filename and are risking confabulating bogus dates (perhaps there's a typo, perhaps it's not a date at all), so file prefixes like '2012-13' or '2012-00' result in no date at all.
--
-- TODO: once this has been used for a while and shaken out, run it on all existing annotations to look at what annotations they change, as a final check, and if it's good, commit the changes.
guessAuthorDateFromString :: FilePath -> (String, String)
guessAuthorDateFromString "" = error "Annotation.guessAuthorDateFromString: called on empty string!"
guessAuthorDateFromString fp =
  let base           = takeBaseName fp
      tokens         = splitOn "-" base
      dateCandidates = allDateCandidates tokens
      validDates     = filter (\d -> isDate d && d >= "1900" && d <= C.todayDayStringUnsafe) dateCandidates
      chosenDate     = if null validDates || length validDates < length dateCandidates
                          then ""
                          else safeLast validDates
      chosenAuthor   = pickAuthor chosenDate tokens
  in if head fp /= '/' then ("","") else (chosenAuthor, chosenDate)

-------------------------------------------------------------------------------
-- Build date candidates by scanning tokens in slices of 1-3,
-- ensuring each piece is purely numeric & in valid ranges.
-- For example, "2020", "2020-08", "2020-08-05".
-------------------------------------------------------------------------------
allDateCandidates :: [String] -> [String]
allDateCandidates [] = []
allDateCandidates tokens = go tokens []
  where
    go [] acc = acc
    go (t:ts) acc =
      let possibleDates = buildDateStrings (t:ts)
      in go ts (acc ++ possibleDates)

buildDateStrings :: [String] -> [String]
buildDateStrings (y:rest) =
  let d1 = if isYear y then [y] else []
      d2 = case rest of
             (m:_) | not (null d1) && isMonth m ->
               [ head d1 ++ "-" ++ m ]
             _ -> []
      d3 = case rest of
             (_:d:_) | not (null d2) && isDay d ->
               [ head d2 ++ "-" ++ d ]
             _ -> []
  in d1 ++ d2 ++ d3
buildDateStrings [] = []

isYear :: String -> Bool
isYear s = length s == 4 && all isDigit s

isMonth :: String -> Bool
isMonth s =
  length s == 2 && all isDigit s &&
  let n = read s :: Int
  in n >= 1 && n <= 12

isDay :: String -> Bool
isDay s =
  length s == 2 && all isDigit s &&
  let d = read s :: Int
  in d >= 1 && d <= 31

safeLast :: [a] -> a
safeLast [] = error "Annotation.safeLast: called on empty list!"
safeLast xs = last xs

--------------------------------------------------------------------------------
-- If we have a chosen date like "2020-08-05" (3 tokens), we skip 3 tokens in the
-- original list, then pick the next as author. If "YYYY-MM", skip 2. If "YYYY", skip 1.
--------------------------------------------------------------------------------
pickAuthor :: String -> [String] -> String
pickAuthor dateCandidate tokens =
  let idx = case locateDateInTokens dateCandidate tokens of
              Nothing -> 0
              Just idx' -> idx'
   in
      case drop (idx + numTokens dateCandidate) tokens of
        (t:_) ->
          let candidate = takeBaseName t
          in if isAuthor candidate then candidate else ""
        [] -> ""

numTokens :: String -> Int
numTokens "" = 0
numTokens dateStr = length (splitOn "-" dateStr)

locateDateInTokens :: String -> [String] -> Maybe Int
locateDateInTokens "" _ = Nothing
locateDateInTokens _ [] = Nothing
locateDateInTokens dateStr tokens =
  go 0
  where
    needed = splitOn "-" dateStr  -- eg. ["2020","08","05"]
    lenNeeded = length needed
    maxI   = length tokens - lenNeeded
    go i
      | i > maxI = Nothing
      | take lenNeeded (drop i tokens) == needed = Just i
      | otherwise = go (i+1)

testGuessAuthorDate :: [(FilePath, (String,String), (String,String))]
testGuessAuthorDate =
  let examples =
        [ ("/doc/ai/scaling/hardware/2020-08-05-hippke-measuringhardwareoverhang-chessscaling19902020.png", ("hippke", "2020-08-05"))
        , ("/doc/darknet-market/2012-05-02-tony76postmortem.mht", ("", "2012-05-02"))
        , ("/doc/dual-n-back/2012-05-30-kundu-dnbrapm.xls", ("kundu", "2012-05-30"))
        , ("/doc/traffic/2012-01-2012-07-gwern-traffic-history.png", ("gwern", "2012-07"))
        , ("/doc/traffic/2015-07-03-2016-01-03-gwern-gwern.net-analytics.pdf", ("gwern", "2016-01-03"))
        , ("/doc/wikipedia/2023-10-01-gwern-tla-lettervsunusedtlaswiththatletterpercentageoverthealphabet.png", ("gwern", "2023-10-01"))
        , ("/doc/zeo/2011-01-18-gwern.png", ("gwern", "2011-01-18"))
        , ("/doc/ai/music/2019-11-08-gpt2-irish-spaceless-100medley.mp3", ("gpt2", "2019-11-08"))
        , ("/doc/biology/2010-wrangham.pdf", ("wrangham", "2010"))
        --  Single date exactly the lower bound: "1900-01-01" accepted, "john" unknown
        , ("/doc/1900-01-02-john.pdf", ("", "1900-01-02"))
          -- Single year exactly 1900 is valid, recognized author
        , ("/doc/1900-gwern.pdf", ("gwern", "1900"))
          -- Single year below lower bound => discard
        , ("/doc/1899-whoever.pdf", ("", ""))
          -- Year above the future bound => discard
        , ("/doc/2030-hippke.mht", ("", ""))
          -- Month out of range => discard
        , ("/doc/2023-13-huh.jpg", ("", "2023"))
          -- Day out of range => discard
        , ("/doc/2023-02-30-hippke.pdf", ("", ""))
          -- Partial date "YYYY-MM"
        , ("/doc/2012-02-hippke.txt", ("hippke", "2012-02"))
          -- Two partial dates => pick the later
        , ("/doc/traffic/2012-01-2012-07-gwern.png", ("gwern", "2012-07"))
          -- Two full dates => pick the later
        , ("/doc/2011-01-15-2011-02-02-gwern.txt", ("gwern", "2011-02-02"))
        , ("/doc/2011-01-15-2011-99-hippke.txt", ("", "2011"))
          -- No date, unknown author => empty
        , ("/doc/justaguy.png", ("", ""))
          -- No date, known author => author only
        , ("/doc/hippke.txt", ("hippke", ""))
        , ("/doc/rl/2022-12-25-gpt2-composition.mp3", ("gpt2", "2022-12-25"))
          -- Underscore in author => presumably fails isAuthor
        , ("/doc/genetics/2021-11-03-gpt_2-data.csv", ("", "2021-11-03"))
          -- Year only, near upper bound => keep if you allow 2025
        , ("/doc/psych/2025-wrangham.pdf", ("wrangham", "2025"))
          --  Exact future boundary => keep if inclusive
        , ("/doc/dnm/dnm2/2025-03-13-gwern.mht", ("gwern", "2025-03-13"))
          -- Leading/trailing hyphens => still find "2022-05-01" and "hippke"
        , ("/doc/silk-road/-2022-05-01-hippke-", ("hippke", "2022-05-01"))
          -- Extension-laden token => "hippke.final.draft" not recognized => keep date only
        , ("/doc/rl/chess/model-free/2018-09-23-hippke.final.draft.pdf", ("", "2018-09-23"))
        , ("https://example.com/doc/traffic/2012-01-2012-07-gwern.png", ("", ""))
        , ("/doc/ai/dataset/2019-02-24-skylion-myanimelist-animescrape.json.xz", ( "skylion", "2019-02-24"))
        ]
  in [ (fp, expected, actual)
     | (fp, expected) <- examples
     , let actual = guessAuthorDateFromString fp
     , actual /= expected
     ]
