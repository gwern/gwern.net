{-# LANGUAGE OverloadedStrings #-}

module Metadata.Date where

import Data.List (isSuffixOf, intercalate)
import System.FilePath (takeBaseName)
import Data.FileStore.Utils (runShellCommand)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import qualified Data.Text as T (append, concat, null, pack, unpack, length, Text, elem)
import Text.Pandoc (Inline(..), nullAttr)
import System.Exit (ExitCode(ExitFailure))
import Text.Regex.TDFA (Regex, makeRegex, match)
import Data.Time (parseTimeM, defaultTimeLocale, Day)

import Utils (sed, split, trim, printRed, delete, formatIntWithCommas, calculateDateSpan, formatDaysInLargestUnit)
import Config.Misc as CD (cd)
import Config.Typography as C (dateRangeDurationTestCases)

dateTruncateBad :: String -> String
 -- we assume that dates are guaranteed to be 'YYYY[-MM[-DD]]' format because of the validation in readLinkMetadataAndCheck enforcing this
-- dates of the form 'YYYY-01-01' (or 'YYYY-01') are invariably lies, and mean just 'YYYY'.
dateTruncateBad d = if "-01-01" `isSuffixOf` d || (length d == 7 && "-01" `isSuffixOf` d) then take 4 d else d

isDate :: String -> Bool
isDate "" = True -- this makes checks/lints easier
isDate d = case length (split "-" d) of
    1 -> isValidDate "%Y" d
    2 -> isValidDate "%Y-%m" d
    3 -> isValidDate "%Y-%m-%d" d
    _ -> False
 where
  isValidDate :: String -> String -> Bool
  isValidDate format str = case parseTimeM True defaultTimeLocale format str :: Maybe Day of
      Just _ -> True
      Nothing -> False

-- If no accurate date is available, attempt to guess date from the local file schema of 'YYYY-surname-[title, disambiguation, etc].ext' or 'YYYY-MM-DD-...'
-- This is useful for PDFs with bad metadata, or data files with no easy way to extract metadata (like HTML files with hopelessly inconsistent dirty metadata fields like `<meta>` tags) or where it's not yet supported (image files usually have a reliable creation date).
--  > guessDateFromLocalSchema "/doc/ai/2020-10-10-barr.png" ""
-- → "2020-10-10"
-- > guessDateFromLocalSchema "/doc/ai/2020-barr.pdf" ""
-- → "2020"
-- > guessDateFromLocalSchema "http://cnn.com" ""
-- → ""
guessDateFromLocalSchema :: String -> String -> String
guessDateFromLocalSchema url date = if head url /= '/' || date /= "" then date
                                    else let f = takeBaseName url in
                                           if not (head f == '1' || head f == '2') -- I don't have any documents from the future or from <1000 AD, so all viable matches start with '1' or '2', I think...
                                           then date else let datePossible = sed "^([12][0-9][0-9][0-9])(-[0-9][0-9])?(-[0-9][0-9])?-.*" "\\1\\2\\3" f
                                                              in if datePossible == f then "" else datePossible

-- TODO: in a few months, after more counter-examples have been added, run this on all outstanding date-less metadata items. But first check it against all known dates.
-- LinkMetadata.walkAndUpdateLinkMetadata True (\x@(a,(_,_,d,_,_,_,_)) -> if d == "" && (not (null (intersect "0123456789" a))) then let date = System.IO.Unsafe.unsafePerformIO (guessDateFromString a) in if null date then return x else putStrLn (a ++ " : " ++ System.IO.Unsafe.unsafePerformIO (guessDateFromString a)) >> return x else return x)
-- `guessDateFromString` is called by `Annotation.linkDispatcher` & `GTX.fixDate` for slow reads.
guessDateFromString :: String -> IO String
guessDateFromString "" = error "Metadata.Format.guessDateFromString: passed an empty string argument, which should never happen!"
guessDateFromString u  =
        do CD.cd
           (status,stderr,mb) <- runShellCommand "./" Nothing "static/build/date-guesser.py" [u]
           case status of
               ExitFailure err -> printRed ("Exit Failure: " ++ intercalate " ::: " [u, show status, show err, show mb, show stderr]) >> return ""
               _ -> let dateNew = delete "\"\"" $ trim $ U.toString mb in
                      if isDate dateNew then return dateNew else error $ "Metadata.Format.guessDateFromString: date-guesser.py returned an invalid date: " ++ dateNew ++ "; input: " ++ u


-- annotate 'YYYY--YYYY'/'YYYY-MM-DD--YYYY-MM-DD' date ranges with their range & duration since then; they are detected automatically, or can be constructed/manually written as span wrappers with the `date-range` class: eg. `<span class="date-range">1939–1945</span>`.
-- See </lorem-inline#date-subscripts>, </subscript#date-ranges>.
-- TODO: handle archaeological/geological/anthropologically-sized dates using 'kya'/'mya'/'gya'?
dateRangeDuration :: Int -> Inline -> Inline
dateRangeDuration todayYear x@(Str s)                                 = dateRangeDurationRaw todayYear x s
dateRangeDuration todayYear x@(Span ("", ["date-range"], []) [Str s]) = dateRangeDurationRaw todayYear x s
dateRangeDuration _ x = x

dateRangeDurationRaw :: Int -> Inline -> T.Text -> Inline
dateRangeDurationRaw todayYear x s =
 let yearMatch     = match dateRangeRegex     s :: [[T.Text]]
     fullDateMatch = match dateFullRangeRegex s :: [[T.Text]]
     singleYearMatch = match singleYearRegex s :: [[T.Text]]
     dateMatch = if yearMatch /= [] then yearMatch else fullDateMatch
 in if '$' `T.elem` s then x else -- HACK: patch over the regexps not doing a full job of excluding comma-less currencies
  case singleYearMatch of
   [[_original,before,year,after]] -> Span nullAttr [dateRangeDurationRaw todayYear (Str before) before,
                                                     dateDurationSingle todayYear year,
                                                     dateRangeDurationRaw todayYear (Str after) after]
   _ -> case dateMatch of
         [] -> x
         [[_original, before,dateFirst,_separator,dateSecond,after]] ->
           let dateFirstS  = take 4 $ T.unpack dateFirst -- 'YYYY-MM-DD' → 'YYYY'
               dateSecondS = take 4 $ T.unpack dateSecond
               dateLongP     = T.length dateFirst > 4 && T.length dateSecond > 4 -- is full date-pair?
               dateRangeDays = formatIntWithCommas $ calculateDateSpan (T.unpack dateFirst) (T.unpack dateSecond) -- eg. '170' days
               dateRangeDaysRounded = T.pack $ formatDaysInLargestUnit $ calculateDateSpan (T.unpack dateFirst) (T.unpack dateSecond) -- eg. '9' → "9d" '170' -> "6m" (6 months)
               dateFirstInt  = read dateFirstS :: Int
               dateSecondInt = read dateSecondS :: Int
               dateRangeInt  = dateSecondInt - dateFirstInt
               dateRangeT    = T.pack $ formatIntWithCommas dateRangeInt
               dateDuration  = todayYear - dateSecondInt
               dateDurationT = T.pack $ show dateDuration
               description   = T.concat ["The date range ", dateFirst, "–", dateSecond, " lasted",
                                          if dateRangeInt == 0 then "" else " "`T.append`dateRangeT `T.append` if dateRangeInt == 1 then " year" else " years",
                                          T.pack (if not dateLongP then "" else " (" ++ dateRangeDays ++ " days)"),
                                         if dateDuration < 2 then "." else T.concat [", ending ", dateDurationT, " years ago."]
                                        ]
               rangeP    = not dateLongP && (dateFirst == dateSecond || dateRangeInt < minRange)
               durationP = todayYear < dateSecondInt || dateDuration < minDuration || dateSecondInt > maxDateSecond
           in if rangeP && durationP || dateFirstInt > dateSecondInt || dateSecondInt > maxDateSecond then x
              else Span nullAttr $ [ -- usual anonymous Span trick for Inline type-safety; the redundant Spans are cleaned up in later passes
                    dateRangeDuration todayYear $ Str before, -- workaround Text.Regex.TDFA lack of lazy/non-greedy matches like `(.*?)`, which means it always matches the *last* date-range
                      Span ("", ["date-range"], [("title", description)]) -- overall wrapper
                      ([Str dateFirst,
                       if rangeP then Str "–" else
                         Span ("", ["subsup"], []) [Superscript [Str "–"],
                                                    Subscript   [Str $ if dateLongP then dateRangeDaysRounded else dateRangeT]],
                       Str dateSecond] ++
                       if durationP then [] else [Subscript [Str (dateDurationT`T.append`"ya")]])] ++
                   if T.null after then [] else [dateRangeDuration todayYear $ Str after]
         z -> error $ "Typography:dateRangeDuration: dateRangeRegex matched an unexpected number of results: " ++ show z

minRange, minDuration, maxDateSecond :: Int
minRange = 2
minDuration = 11
maxDateSecond = 2562 -- the latest serious AD year I see on Gwern.net currently seems to be '2561 AD', from Charles Stross’s "USENIX 2011 Keynote: Network Security in the Medium Term, 2061–2561 AD" talk.

dateDurationSingle :: Int -> T.Text -> Inline
dateDurationSingle todayYear "" = error $ "Typography.dateDurationSingle: passed an empty string year to update, with current year " ++ show todayYear
dateDurationSingle todayYear oldYear
  | todayYear < 1000            = error $ "Typography.dateDurationSingle: passed an absurdly old 'current' date: " ++ show todayYear ++ "; intended to update old year " ++ show todayYear
  | otherwise = let oldYearInt = read (T.unpack oldYear) :: Int
                    yearsSince = todayYear - oldYearInt in
                  if yearsSince < minDuration then Str oldYear else
                    Span ("", ["date-range"], []) [Str oldYear, Subscript [Str $ T.pack (formatIntWithCommas yearsSince)`T.append`"ya"]]

-- match hyphen/EN-DASH-separated comma-less years from 1000--2999, or full dates 1000-01-01--2999-12-31:
-- attempt to exclude any currency amounts (not guaranteed to work)
-- Tested in `Config.Typography.dateRangeDurationTestCases`
dateRangeRegex, dateFullRangeRegex, singleYearRegex :: Regex
dateRangeRegex     = makeRegex ("(.*)([12][0-9][0-9][0-9])(--?|–)([12][0-9][0-9][0-9])(.*)" :: T.Text)
dateFullRangeRegex = makeRegex ("([^#×€¢¥£\\$]*)([12][0-9][0-9][0-9]-[0-9][0-9][-]?[0-9]?[0-9]?)(--?|–)([12][0-9][0-9][0-9]-[0-9][0-9][-]?[0-9]?[0-9]?)([^p×€¢¥£\\$]*)" :: T.Text)
singleYearRegex    = makeRegex ("(.*[^#0-9-–×€¢¥£\\$])([12][0-9][0-9][0-9])([^0-9-s–’p][^#×€¢¥£\\$]*)" :: T.Text)

dateRangeDurationTestCasesTestsuite :: [(Int, Inline, Inline, Inline)]
dateRangeDurationTestCasesTestsuite = filter (\(_,_,expected',actual) -> expected' /= actual) $
                                      map (\(y,s,expected) -> (y, s, expected, dateRangeDuration y s)) C.dateRangeDurationTestCases
