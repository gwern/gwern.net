{-# LANGUAGE OverloadedStrings #-}
-- Various string-munging utilities aimed at metadata, like cleaning titles, authors, dates etc.

module MetadataFormat where

import Data.List (intersperse, isSuffixOf, isPrefixOf)
import Numeric (showFFloat)
import System.FilePath (takeBaseName)
import qualified Data.Text as T (pack)

import Text.Regex.TDFA ((=~))
import Text.Pandoc (Inline(Span, Str))

import Cycle (testInfixRewriteLoops)
import Utils (anyInfix, fixedPoint, replace, replaceMany, sed, sedMany, split, trim)
import Config.MetadataFormat as C

-- | Check if brackets & quotes in a string are balanced.
--
-- Checks for balance of '()', '[]', & '{}' brackets, and double-quotes in the input string.
-- Returns an empty string if balanced, or the substring from the first unbalanced
-- bracket otherwise. It uses a helper function with a stack to track open brackets.
--
-- Examples:
--  > balanced "(abc[de]{fg})" → "" (balanced)
--  > balanced "(abc]de)"      → "c]de)" (unbalanced)
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
            else helper ss ('"':stack) (n+1) (if null stack then n else idx)
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

-- must handle both "https://twitter.com/grantslatton/status/1703913578036904431" and "https://twitter.com/grantslatton":
extractTwitterUsername :: String -> String
extractTwitterUsername = sedMany [("^https:\\/\\/x\\.com\\/([a-z0-9]+)$", "\\1")
                                 , ("^https:\\/\\/twitter\\.com\\/([a-z0-9]+)$", "\\1")
                                 , ("^https:\\/\\/twitter\\.com\\/([^\\/]+)/status/[0-9]+$", "\\1")
                                 ]

-- print out Doubles long-form, not in scientific notation. By default, Haskell will print values like '10e8', which is bad for downstream users like the inflation-adjuster JavaScript. But it turns out to be surprisingly hard to print out the literal of a Double/Float without rounding, scientific notation, fractions, precision limitations, or other issues. This tries to do so using Numeric.showFFloat, and includes a test-suite of examples to ensure the String is as expected. For very small amounts like '1.0000000000000002', they will be rounded (to '1').
-- Precision '0' means nothing after the decimal point, like '0'; '1' means 1 digit after the decimal like '0.1', etc.
printDouble :: Int -> Double -> String
printDouble precision x = if x > 1.7976931348623157e308 || x < -1.7976931348623157e308
               then error $ "printDouble: Extreme unsupported value past what showFFloat supports; you'll have to handle this differently: " ++ show x
               else removeTrailingZeros $ showFFloat (Just precision) x ""
    where removeTrailingZeros, drop1IfDot :: String -> String
          removeTrailingZeros "-0"   = "0"
          removeTrailingZeros "-0."  = "0"
          removeTrailingZeros "-0.0" = "0"
          removeTrailingZeros "0.0"  = "0"
          removeTrailingZeros "0"  = "0"
          removeTrailingZeros y = drop1IfDot $ reverse $ dropWhile (== '0') $ reverse y
          drop1IfDot xs = if last xs == '.' then init xs else xs

printDoubleTestSuite :: [(Double, Int, String, String)]
printDoubleTestSuite = filter (\(_,_,expected,actual) -> expected /= actual) $ map (\(n,prec,s) -> (n,prec,s, printDouble prec n )) C.printDoubleTests

dateRegex, footnoteRegex, sectionAnonymousRegex, badUrlRegex :: String
-- 'YYYY[-MM[-DD]?]?': '2020', '2020-01', '2020-01-01' etc
dateRegex             = "^[1-2][0-9][0-9][0-9](-[0-2][0-9](-[0-3][0-9])?)?$"
-- '/Foo#fn3', 'Foo#fn1', 'Foo-Bar-2020#fn999' etc
footnoteRegex         = "^/?[[:alnum:]-]+#fn[1-9][0-9]*$"
-- unnamed sections which receive Pandoc positional auto-names like "#section-1", "#section-15"; unstable, should be named if ever to be linked to, etc.
sectionAnonymousRegex = "^#section-[0-9]+$"
badUrlRegex           = "http.*http|doc/.*doc/"::String

isDate :: String -> Bool
isDate d = d =~ dateRegex

-- Heuristic checks for specific link sources:
checkURL :: String -> IO ()
checkURL u = do let doubleURL = u =~ badUrlRegex -- I keep accidentally concatenating Arxiv URLs when tagging.
                if not doubleURL then return () else error u

processDOI, processDOIArxiv :: String -> String
processDOI = replaceMany [("–", "-"), ("—", "-"), ("https://doi.org/", "")] . sed "^doi:" ""
 -- Arxiv has some weird URLs and edge-cases like <https://arxiv.org/abs/hep-ph/0204295> (note double-subdirectory & lack of period-separation).
processDOIArxiv url = "10.48550/arXiv." ++
                               sed "https://arxiv.org/[a-z-]+/([0-9]+\\.[0-9]+).*" "\\1" -- regular current Arxiv URL pattern
                               (sed "https://arxiv.org/abs/[a-z-]+/([0-9]+).*" "\\1" url) -- old-style like 'hep-ph'

-- handle initials consistently as period+space-separated; delete titles; delete the occasional final Oxford 'and' cluttering up author lists
cleanAuthors :: String -> String
cleanAuthors = trim . replaceMany C.cleanAuthorsFixedRewrites . sedMany C.cleanAuthorsRegexps

cleanAuthorsTest :: [(String,String,String)]
cleanAuthorsTest = testInfixRewriteLoops C.cleanAuthorsFixedRewrites cleanAuthors

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
                                           then date else sed "^([12][0-9][0-9][0-9])(-[0-9][0-9])?(-[0-9][0-9])?-.*" "\\1\\2\\3" f

dateTruncateBad :: String -> String
 -- we assume that dates are guaranteed to be 'YYYY[-MM[-DD]]' format because of the validation in readLinkMetadataAndCheck enforcing this
-- dates of the form 'YYYY-01-01' (or 'YYYY-01') are invariably lies, and mean just 'YYYY'.
dateTruncateBad d = if "-01-01" `isSuffixOf` d || (length d == 7 && "-01" `isSuffixOf` d) then take 4 d else d
