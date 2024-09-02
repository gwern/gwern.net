{-# LANGUAGE OverloadedStrings #-}
-- Various string-munging utilities aimed at metadata; authors are handled in MetadataAuthor, titles in MetadataTitle

module MetadataFormat where

import Data.List (isSuffixOf, isPrefixOf, intercalate, unfoldr)
import Numeric (showFFloat)
import System.FilePath (takeBaseName)
import Data.FileStore.Utils (runShellCommand)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import System.Exit (ExitCode(ExitFailure))

import Data.Time (parseTimeM, defaultTimeLocale, Day)

import Text.Regex.TDFA ((=~))

import Cycle (testInfixRewriteLoops)
import Utils (anyInfix, fixedPoint, replace, replaceMany, sed, sedMany, split, trim, printRed, delete)
import Config.MetadataFormat as C
import Config.Misc as CD (cd)

-- | Check if brackets & quotes in a string are balanced.
--
-- Checks for balance of '()', '[]', & '{}' brackets, and double-quotes in the input string.
-- Returns an empty string if balanced, or the substring from the first unbalanced
-- bracket otherwise. It uses a helper function with a stack to track open brackets.
--
-- Examples:
--  > balanced "(abc[de]{fg})" → "" (balanced)
--  > balanced "(abc]de)"     → "c]de)" (unbalanced)
--  > balanced  "foo bar bar \" test test" → "\" test test"
--
-- Returns: Empty string if balanced, substring from first unbalanced bracket otherwise.
balanced :: String -> String
balanced str = helper str "" 0 0
  where
    helper [] stack _ idx = if null stack then "" else drop idx str
    helper (s:ss) stack n idx
      | s == '"' =
          if not (null stack) && head stack == '"'
            then helper ss (tail stack) (n+1) (if null stack then n else idx)
            else helper ss ('"':stack)  (n+1) (if null stack then n else idx)
      | s `elem` openBrackets = helper ss (s:stack) (n+1) (if null stack then n else idx)
      | s `elem` closeBrackets =
          if not (null stack) && head stack == matchingBracket s
            then helper ss (tail stack) (n+1) (if null stack then n else idx)
            else drop n str
      | otherwise = helper ss stack (n+1) idx
    openBrackets = "([{" :: String
    closeBrackets = ")]}" :: String
    matchingBracket ')' = '('
    matchingBracket ']' = '['
    matchingBracket '}' = '{'
    matchingBracket _ = error "Invalid bracket"

-- print out Doubles long-form, not in scientific notation. By default, Haskell will print values like '10e8', which is bad for downstream users like the inflation-adjuster JavaScript. But it turns out to be surprisingly hard to print out the literal of a Double/Float without rounding, scientific notation, fractions, precision limitations, or other issues. This tries to do so using Numeric.showFFloat, and includes a test-suite of examples to ensure the String is as expected. For very small amounts like '1.0000000000000002', they will be rounded (to '1').
-- Precision '0' means nothing after the decimal point, like '0'; '1' means 1 digit after the decimal like '0.1', etc.

printDouble :: Int -> Double -> String
printDouble precision x
    | x > 1.7976931348623157e308 || x < -1.7976931348623157e308 =
        error $ "printDouble: Extreme unsupported value past what showFFloat supports; you'll have to handle this differently: " ++ show x
    | otherwise = addCommas $ removeTrailingZeros $ showFFloat (Just precision) x ""
  where
    removeTrailingZeros, drop1IfDot :: String -> String
    removeTrailingZeros "-0"   = "0"
    removeTrailingZeros "-0."  = "0"
    removeTrailingZeros "-0.0" = "0"
    removeTrailingZeros "0.0"  = "0"
    removeTrailingZeros "0"    = "0"
    removeTrailingZeros y = drop1IfDot $ reverse $ dropWhile (== '0') $ reverse y
    drop1IfDot xs = if last xs == '.' then init xs else xs

    addCommas :: String -> String
    addCommas str
        | '-' `elem` str = '-' : addCommas (tail str)
        | '.' `elem` str = let (intPart, fracPart) = break (== '.') str
                           in addCommasToIntPart intPart ++ fracPart
        | otherwise = addCommasToIntPart str

    addCommasToIntPart :: String -> String
    addCommasToIntPart = reverse . intercalate "," . chunksOf 3 . reverse

    chunksOf :: Int -> [a] -> [[a]]
    chunksOf n = takeWhile (not . null) . unfoldr (Just . splitAt n)

printDoubleTestSuite :: [(Double, Int, String, String)]
printDoubleTestSuite = filter (\(_,_,expected,actual) -> expected /= actual) $ map (\(n,prec,s) -> (n,prec,s, printDouble prec n )) C.printDoubleTests

footnoteRegex, sectionAnonymousRegex, badUrlRegex :: String
-- 'YYYY[-MM[-DD]?]?': '2020', '2020-01', '2020-01-01' etc
-- '/Foo#fn3', 'Foo#fn1', 'Foo-Bar-2020#fn999' etc
footnoteRegex         = "^/?[[:alnum:]-]+#fn[1-9][0-9]*$"
-- unnamed sections which receive Pandoc positional auto-names like "#section-1", "#section-15"; unstable, should be named if ever to be linked to, etc.
sectionAnonymousRegex = "^#section-[0-9]+$"
badUrlRegex           = "http.*http|doc/.*doc/"::String

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

-- Heuristic checks for specific link sources:
checkURL :: String -> IO ()
checkURL u = do let doubleURL = u =~ badUrlRegex -- I keep accidentally concatenating Arxiv URLs when tagging.
                if not doubleURL then return () else error ("MetadataFormat.checkURL: double URL check failed, was the URL mangled or accidentally duplicated or 2 URLs concatenated? Retry with fixed URL. Input: " ++ u)

processDOI, processDOIArxiv :: String -> String
processDOI = replaceMany [("–", "-"), ("—", "-"), ("https://doi.org/", "")] . sed "^doi:" ""
 -- Arxiv has some weird URLs and edge-cases like <https://arxiv.org/abs/hep-ph/0204295> (note double-subdirectory & lack of period-separation).
processDOIArxiv url = "10.48550/arXiv." ++
                               sed "https://arxiv.org/[a-z-]+/([0-9]+\\.[0-9]+).*" "\\1" -- regular current Arxiv URL pattern
                               (sed "https://arxiv.org/abs/[a-z-]+/([0-9]+).*" "\\1" url) -- old-style like 'hep-ph'

-- test for possible infinite-loops in the rewrite suite; our initial source of cases is just the fixed-string rewrites.
-- The cycle detector is not enough because the rewrites operate infix, not by replacing the *whole* string, so it's possible to have expansion/contraction which produces loops.
cleanAbstractsHTMLTest :: [(String,String,String)]
cleanAbstractsHTMLTest = testInfixRewriteLoops C.htmlRewriteFixed cleanAbstractsHTML

-- run all necessary rewrites on a string to clean up malformation, inconsistent formatting, errors, convert to house style, etc
cleanAbstractsHTML :: String -> String
cleanAbstractsHTML = fixedPoint cleanAbstractsHTML'
 where cleanAbstractsHTML' :: String -> String
       cleanAbstractsHTML' = trim . sedMany C.htmlRewriteRegexpAfter . replaceMany C.htmlRewriteFixed . sedMany C.htmlRewriteRegexpBefore

linkCanonicalize :: String -> String
linkCanonicalize l | "https://gwern.net/" `isPrefixOf` l = replace "https://gwern.net/" "/" l
                   -- so we can more carelessly tag PDFs,
                     -- like `gwtag adversarial https://arxiv.org/pdf/2406.20053`, and create the annotation for the abstract page instead:
                   -- eg. "https://arxiv.org/pdf/2406.20053#org=foo"
                   -- → "https://arxiv.org/abs/2406.20053#org=foo"
                   | "https://arxiv.org/" `isPrefixOf` l = replace "https://arxiv.org/abs//" "https://arxiv.org/abs/" $ sedMany [("https://arxiv.org/pdf/([0-9.]+)([&#]org=[a-z]+)?$", "https://arxiv.org/abs/\\1\\2")] l
                   -- | head l == '#' = l
                   | otherwise = l

filterMeta :: String -> String
filterMeta ea = if anyInfix ea C.filterMetaBadSubstrings || elem ea C.filterMetaBadWholes then "" else ea

-- title clean up: delete the period at the end of many titles, extraneous colon spacing, remove Arxiv's newline+double-space, and general whitespace cleaning
trimTitle :: String -> String
trimTitle [] = ""
trimTitle t = let t' = reverse $ sedMany [("†.*", ""), -- eg. "Relation of Serum 25-Hydroxyvitamin D to Heart Rate and Cardiac Work (from the National Health and Nutrition Examination Surveys)†\n†Conflict of interest: Dr. Simpson receives support from Abbott Laboratories, Chicago, Illinois"
                                          ("([a-z])_ ", "\\1: ")] $ -- a lot of Elsevier papers replace colons with underscores (‽) in PDF metadata eg. "Compensatory conspicuous communication_ Low status increases jargon use"
                       replaceMany [(" : ", ": "), ("\n ", " ")] $ trim t in
                if not (null t') then reverse (if head t' == '.' then tail t' else t') else ""

 -- eg. "foo.pdf#page=50&org=openai" → "50"; "foo.pdf" → ""
pageNumberParse :: String -> String
pageNumberParse u = let pg = sed ".*\\.pdf#page=([0-9]+).*" "\\1" u
                    in if u == pg then "" else pg


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

dateTruncateBad :: String -> String
 -- we assume that dates are guaranteed to be 'YYYY[-MM[-DD]]' format because of the validation in readLinkMetadataAndCheck enforcing this
-- dates of the form 'YYYY-01-01' (or 'YYYY-01') are invariably lies, and mean just 'YYYY'.
dateTruncateBad d = if "-01-01" `isSuffixOf` d || (length d == 7 && "-01" `isSuffixOf` d) then take 4 d else d

-- TODO: in a few months, after more counter-examples have been added, run this on all outstanding date-less metadata items. But first check it against all known dates.
-- LinkMetadata.walkAndUpdateLinkMetadata True (\x@(a,(_,_,d,_,_,_,_)) -> if d == "" && (not (null (intersect "0123456789" a))) then let date = System.IO.Unsafe.unsafePerformIO (guessDateFromString a) in if null date then return x else putStrLn (a ++ " : " ++ System.IO.Unsafe.unsafePerformIO (guessDateFromString a)) >> return x else return x)
guessDateFromString :: String -> IO String
guessDateFromString "" = error "MetadataFormat.guessDateFromString: passed an empty string argument, which should never happen!"
guessDateFromString u  =
        do CD.cd
           (status,stderr,mb) <- runShellCommand "./" Nothing "static/build/date-guesser.py" [u]
           case status of
               ExitFailure err -> printRed ("Exit Failure: " ++ intercalate " ::: " [u, show status, show err, show mb, show stderr]) >> return ""
               _ -> let dateNew = delete "\"\"" $ trim $ U.toString mb in
                      if isDate dateNew then return dateNew else error $ "MetadataFormat.guessDateFromString: date-guesser.py returned an invalid date: " ++ dateNew ++ "; input: " ++ u
