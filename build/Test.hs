{-# LANGUAGE OverloadedStrings #-}
module Test where

import Control.Monad (unless)
import Data.Either (lefts)
import Data.List (intersect, foldl')
import qualified Data.Map.Strict as M (keys, toList)
import qualified Data.Set as Set (empty, insert, member)
import Data.Char (isAlpha, isLower)
import qualified Data.Text as T (unpack, elem, head, pack)

import Text.Pandoc (Inline(Link))

import Cycle (isCycleLess)
import Metadata.Format (printDoubleTestSuite, cleanAbstractsHTMLTest, balanced, cleanAbstractsHTML,
                      footnoteRegex, sectionAnonymousRegex, badUrlRegex)
import Metadata.Date (isDate, dateRangeDurationTestCasesTestsuite)
import Utils (printGreen, printRed, isDomainT, isURL, isURLT, isURLAny, isURLAnyT, ensure)
import LinkID (url2ID)

-- module self-tests:
import Annotation (tooltipToMetadata)
import qualified Cycle (testCycleDetection)
import Inflation (inflationDollarTestSuite)
import Interwiki (interwikiTestSuite, interwikiCycleTestSuite)
import LinkArchive (readArchiveMetadataAndCheck, testLinkRewrites)
import LinkAuto (linkAutoTest)
import LinkIcon (linkIconTest)
import LinkLive (linkLiveTest, linkLivePrioritize)
import Tags (testTags)
import Typography (titleCaseTest)
import LinkMetadata (readLinkMetadata, fileTranscludesTest)
import Metadata.Author (authorCollapseTest, cleanAuthorsTest, extractTwitterUsername, authorDB)

-- test the tests as configuration files for duplicates etc:
import qualified Config.GenerateSimilar (blackListURLs)
import qualified Config.Interwiki (testCases, quoteOverrides, redirectDB)
import qualified Config.LinkArchive (whiteListMatchesFixed, localizeLinktestCases)
import qualified Config.LinkIcon (prioritizeLinkIconBlackList, linkIconTestUnitsText)
import qualified Config.LinkLive (goodDomainsSub, goodDomainsSimple, badDomainsSub, badDomainsSimple, goodLinks, badLinks)
import qualified Config.LinkSuggester (badAnchorStrings, whiteList)
import qualified Config.Tags (shortTagBlacklist, tagsLong2Short, wholeTagRewritesRegexes, tagsShort2LongRewrites, shortTagTestSuite)
import qualified Config.Typography (surnameFalsePositivesWhiteList, titleCaseTestCases, dateRangeDurationTestCases)
import qualified Config.XOfTheDay (siteBlackList, quoteDBPath, siteDBPath)
import qualified XOfTheDay as XOTD (readTTDB)
import qualified Config.Inflation (bitcoinUSDExchangeRateHistory, inflationDollarLinkTestCases)
import qualified Config.LinkAuto (custom)
import qualified Config.LinkID (linkIDOverrides, affiliationAnchors)
import qualified Config.Metadata.Format (htmlRewriteRegexpBefore, htmlRewriteRegexpAfter, htmlRewriteFixed, filterMetaBadSubstrings, filterMetaBadWholes, balancedBracketTestCases, htmlRewriteTestCases)
import qualified Config.Misc (cd, tooltipToMetadataTestcases, cycleTestCases, cleanArxivAbstracts, arxivAbstractFixedRewrites, arxivAbstractRegexps)
import qualified Config.Paragraph (whitelist)
import qualified Config.Metadata.Author (authorCollapseTestCases, canonicals, canonicalsWithInitials, authorLinkDB, authorLinkBlacklist, cleanAuthorsFixedRewrites, cleanAuthorsRegexps, extractTwitterUsernameTestSuite)
import qualified Config.Metadata.Title (badStrings, stringReplace, stringDelete)

import Text.Regex.Base.RegexLike (makeRegexM)
import Text.Regex (Regex)
import Control.Exception (try, SomeException)

-- test function to validate lists of regex patterns
testRegexPatterns :: [String] -> IO ()
testRegexPatterns patterns = do
    results <- mapM validateRegex patterns
    let failures = lefts results
    unless (null failures) $ mapM_ putStrLn failures
 where -- Function to validate a regex pattern
  validateRegex :: String -> IO (Either String ())
  validateRegex pttrn = do
      result <- try (makeRegexM pttrn :: IO Regex)
      case result of
          Left e -> return . Left $ "Regex compilation failed for pattern '" ++ pttrn ++
                                    "': " ++ show (e :: SomeException)
          Right _ -> return $ Right ()

-- Config checking: checking for various kinds of uniqueness/duplications.
-- Enable additional runtime checks to very long config lists which risk error from overlap or redundancy. Prints out the duplicates.
-- (Since the config lists are static, they can in theory be checked at compile-time, but my attempt to do that with Template Haskell
-- for XMonad keymap configs many years ago ran into a lot of pain, so I won't bother even trying again.)
--
-- Helper function to check uniqueness & report the offending list:
-- Optimized helper function to get duplicates
getDuplicates :: Ord a => [a] -> [a]
getDuplicates = snd . foldl' go (Set.empty, [])
  where
    go (seen, duplicates) x
      | x `Set.member` seen = (seen, duplicates)
      | otherwise = (Set.insert x seen, duplicates)
throwError :: Show a => String -> [a] -> b
throwError msg xs = error $ "Error: " ++ msg ++ " " ++ show xs
checkUniqueOrThrow :: (Eq a, Ord a, Show a) => String -> [a] -> [a]
checkUniqueOrThrow msg xs
  | null duplicates = xs
  | otherwise = throwError msg duplicates
  where duplicates = getDuplicates xs

-- 0. check a simple list for uniqueness in the only way possible:
isUniqueList :: (Eq a, Ord a, Show a) => [a] -> [a]
isUniqueList = checkUniqueOrThrow "Simple list contains duplicates:"

-- Association-list checks:
-- 1. isUnique: all key-value pairs are unique and there are no duplicates
isUnique :: (Eq a, Show a, Eq b, Ord a, Ord b, Show b) => [(a,b)] -> [(a,b)]
isUnique = checkUniqueOrThrow "Test.isUnique: Association List contains duplicate key-value pairs:"

-- 2. isUniqueKeys: all keys are unique and there are no duplicates
isUniqueKeys :: (Eq a, Ord a, Show a, Show b) => [(a,b)] -> [(a,b)]
isUniqueKeys xs
  | null duplicates = xs
  | otherwise = throwError "Test.isUniqueKeys: Association List contains duplicate keys:" duplicates
  where duplicates = getDuplicates (map fst xs)
-- special-case:
isUniqueKeys3 :: (Eq a, Ord a, Show a) => [(a,b,c)] -> [(a,b,c)]
isUniqueKeys3 xs
  | null duplicates = xs
  | otherwise = throwError "Test.isUniqueKeys3: Association List contains duplicate keys (ie. 'a' of '(a,b,c)'):" duplicates
  where duplicates = getDuplicates (map (\(a,_,_) -> a) xs)
isUniqueKeys4 :: (Eq a, Ord a, Show a) => [(a,b,c,d)] -> [(a,b,c,d)]
isUniqueKeys4 xs
  | null duplicates = xs
  | otherwise = throwError "Test.isUniqueKeys4: Association List contains duplicate keys (ie. 'a' of '(a,b,c,d)'):" duplicates
  where duplicates = getDuplicates (map (\(a,_,_,_) -> a) xs)

isUniqueMiddle3 :: (Eq a, Ord a, Ord b, Show b, Show a) => [(a,b,c)] -> [(a,b,c)]
isUniqueMiddle3 xs
  | null duplicates = xs
  | otherwise = throwError "Test.isUniqueMiddle3: Association List contains duplicate middle-keys (ie. 'b' of '(a,b,c)'):" duplicates
  where duplicates = getDuplicates (map (\(_,b,_) -> b) xs)

-- 3. isUniqueValues: all values are unique and there are no duplicates
isUniqueValues :: (Show a, Ord a, Eq b, Ord b, Show b) => [(a,b)] -> [(a,b)]
isUniqueValues xs
  | null duplicates = xs
  | otherwise = throwError "Test.isUniqueValues: Association List contains duplicate values:" duplicates
  where duplicates = getDuplicates (map snd xs)

-- 4. isUniqueAll: all keys, values, and key-value pairs are unique
isUniqueAll :: (Eq a, Ord a, Show a, Eq b, Ord b, Show b) => [(a,b)] -> [(a,b)]
isUniqueAll xs = isUniqueValues $ isUniqueKeys $ isUnique xs

testXOTD :: IO Int
testXOTD = do s <- XOTD.readTTDB Config.XOfTheDay.siteDBPath
              q <- XOTD.readTTDB Config.XOfTheDay.quoteDBPath
              -- check that the non-URL fields are *not* valid URLs, which implies a copy-paste error, duplication, or something.
              return $ sum [length $ ensure "Test.testXOTD.sites" "isURLAny/not-isURLAny" (\(u,title,_) -> isURLAny u && not (null title || isURL title)) s
                          , length $ isUniqueKeys3 s
                          , length $ ensure "Test.testXOTD.quotes" "not-isURLAny" (\(qt,a,_) -> not (null qt || isURLAny qt) && (null a || not (isURLAny a))) q
                          , length $ isUniqueKeys3 q
                          ]

-- we prefer to test configs in a single centralized place, as inconvenient as that is, because if we simply test inside the function itself on every call, we incur overhead and we risk accidentally-quadratic behavior (like when a filter or cleaning function is applied to every entry in list or database, and has to test every entry in the config for uniqueness each time).
testConfigs :: Int
testConfigs = sum $ map length [isUniqueList Config.Metadata.Format.filterMetaBadSubstrings, isUniqueList Config.Metadata.Format.filterMetaBadWholes
                               , ensure "Test.GenerateSimilar.blackListURLs" "isURLAny (URL & file)" isURLAny $
                                 isUniqueList Config.GenerateSimilar.blackListURLs
                               , isUniqueList Config.LinkArchive.whiteListMatchesFixed
                               , isUniqueList Config.LinkID.affiliationAnchors
                               , isUniqueList Config.Tags.shortTagBlacklist
                               , isUniqueList Config.Typography.surnameFalsePositivesWhiteList
                               ] ++ -- String
                               map length [isUniqueList Config.LinkIcon.prioritizeLinkIconBlackList
                                           , isUniqueList Config.LinkLive.goodDomainsSub, isUniqueList Config.LinkLive.goodDomainsSimple, isUniqueList Config.LinkLive.badDomainsSub, isUniqueList Config.LinkLive.badDomainsSimple, isUniqueList Config.LinkLive.goodLinks, isUniqueList Config.LinkLive.badLinks
                                           , isUniqueList Config.LinkSuggester.badAnchorStrings
                                           , isUniqueList Config.XOfTheDay.siteBlackList
                                           , ensure "Test.XOfTheDay.siteBlackList" "isDomainT" isDomainT Config.XOfTheDay.siteBlackList] ++ -- T.Text
              [length $ isUniqueKeys4 Config.LinkIcon.linkIconTestUnitsText,
               length $ ensure "Test.linkIconTestUnitsText" "isURLAnyT" (\(u,_,_,_) -> T.head u == '#' || isURLAnyT u) Config.LinkIcon.linkIconTestUnitsText] ++
              [length $ isUniqueKeys Config.Interwiki.testCases, length (isUniqueKeys Config.Interwiki.redirectDB), length $ isUniqueList Config.Interwiki.quoteOverrides
              , length (ensure "Test.testConfigs.testCases" "isURLT (URL of second)" safeLink Config.Interwiki.testCases)
              , length (ensure "Test.testConfigs.redirectDB" "isURLT (URL of second)" (\(_,u2) -> isURLT u2) Config.Interwiki.redirectDB)
              , length (ensure "Test.testConfigs.extracTwitterUsernameTestSuite" "isURL (URL of first)" (\(u1,_) -> isURL u1) Config.Metadata.Author.extractTwitterUsernameTestSuite)
              , length $ isUniqueAll Config.LinkSuggester.whiteList
              , length $ ensure "Test.LinkSuggester.whiteList" "isURLAnyT" (isURLAnyT . fst) Config.LinkSuggester.whiteList
              , length $ ensure "Test.LinkSuggester.whiteList" "not isURLT" (not . any isURLT . snd) Config.LinkSuggester.whiteList
              , length $ isUniqueAll Config.Tags.tagsLong2Short, length $ isUniqueKeys Config.Tags.wholeTagRewritesRegexes, length $ isUniqueKeys Config.Tags.tagsShort2LongRewrites, length $ isUniqueKeys Config.Tags.shortTagTestSuite
              , length $ ensure "Test.Config.Tags.tagsLong2Short" "isLower" (all Data.Char.isLower . filter Data.Char.isAlpha . fst) Config.Tags.tagsLong2Short
              , length $ isUniqueKeys Config.Typography.titleCaseTestCases
              , length $ isUniqueKeys Config.Misc.tooltipToMetadataTestcases
              , length $ isUniqueKeys Config.Misc.cleanArxivAbstracts
              , length $ isUniqueKeys Config.Misc.arxivAbstractRegexps
              , length $ isUniqueKeys Config.Misc.arxivAbstractFixedRewrites
              , length $ isUniqueKeys Config.Inflation.bitcoinUSDExchangeRateHistory, length $ isUniqueAll Config.Inflation.inflationDollarLinkTestCases
              , length $ ensure "Test.Inflation.dates" "isDate" (isDate . fst) $ Config.Inflation.bitcoinUSDExchangeRateHistory
              , length $ isUniqueAll Config.LinkAuto.custom
              , length $ ensure "Test.LinkAuto.custom" "isURLAnyT" (isURLAnyT . snd) Config.LinkAuto.custom
              , length $ isUniqueAll Config.LinkID.linkIDOverrides
              , length $ ensure "Test.linkIDOverrides" "HTML identifier lambda" (\(_,ident) -> -- NOTE: HTML identifiers *must* start with `[a-zA-Z]`, and not numbers or periods etc; they must not contain periods for CSS/JS compatibility
                                                                                        let ident' = T.unpack ident in '.' `notElem` ident' && isAlpha (head ident'))
                Config.LinkID.linkIDOverrides
               , length $ ensure "Test.linkIDOverrides" "URI (first), not URL (second)" (\(u,ident) -> isURLAny u && not (isURLT ident)) Config.LinkID.linkIDOverrides
               , length $ Metadata.Author.authorDB
              , length $ isUniqueKeys Config.Metadata.Author.cleanAuthorsFixedRewrites, length $ isUniqueKeys Config.Misc.cycleTestCases, length $ isUniqueKeys Config.Metadata.Author.cleanAuthorsRegexps, length $ isUniqueKeys Config.Metadata.Format.htmlRewriteRegexpBefore, length $ isUniqueKeys Config.Metadata.Format.htmlRewriteRegexpAfter, length $ isUniqueKeys Config.Metadata.Format.htmlRewriteFixed, length $ isUniqueKeys Config.Metadata.Author.extractTwitterUsernameTestSuite
              , length $ filter (\(input,output) -> Metadata.Format.balanced input /= output) $ isUniqueKeys Config.Metadata.Format.balancedBracketTestCases
              , length $ isUniqueAll Config.Metadata.Author.authorCollapseTestCases, length $ isUniqueAll (M.toList Config.Metadata.Author.authorLinkDB)
              , length $ isUniqueValues (M.toList Config.Metadata.Author.canonicals), length $ isUniqueList Config.Metadata.Author.canonicalsWithInitials, length $ isUniqueList Config.Metadata.Author.authorLinkBlacklist
              , length $ isUniqueAll Config.Metadata.Format.htmlRewriteTestCases
              , length $ isUniqueList Config.Typography.dateRangeDurationTestCases
              , length $ ensure "Test.authorLinkDB" "isURLAny (URL of second)" (all isURLAnyT) (M.toList Config.Metadata.Author.authorLinkDB)
              , length $ ensure "Test.authorLinkDB" "no broken HTML entities indicated by a '&'" (\name -> not ('&' `T.elem` name)) (M.keys Config.Metadata.Author.authorLinkDB)
              , length $ isCycleLess (M.toList Config.Metadata.Author.canonicals), length $ isCycleLess (M.toList Config.Metadata.Author.authorLinkDB)
              , length $ (map T.unpack $ M.keys Config.Metadata.Author.authorLinkDB) `intersect` (M.keys Config.Metadata.Author.canonicals)
              , length $ isUniqueList Config.Metadata.Title.badStrings, length $ isUniqueList Config.Metadata.Title.stringDelete, length $ isUniqueKeys Config.Metadata.Title.stringReplace
              , length $ isUniqueList Config.Paragraph.whitelist, length $ ensure "Test.Paragraph.whitelist" "isURLAny" isURLAny Config.Paragraph.whitelist] ++
              [sum $ map length [ ensure "goodDomainsSimple" "isDomainT" isDomainT Config.LinkLive.goodDomainsSimple
                                , ensure "goodDomainsSub"    "isDomainT" isDomainT Config.LinkLive.goodDomainsSub
                                , ensure "badDomainsSimple"  "isDomainT" isDomainT Config.LinkLive.badDomainsSimple
                                , ensure "badDomainsSub"     "isDomainT" isDomainT Config.LinkLive.badDomainsSub
                                , ensure "Test.prioritizeLinkIconBlackList" "isDomainT" isDomainT Config.LinkIcon.prioritizeLinkIconBlackList]
              ] ++
              [length (ensure "Test.localizeLinktestCases" "URL/URI" (\(u, (af, mobileversion, html, _)) -> isURLT u && (af=="" || isURLAnyT af) && (mobileversion=="" || isURLT mobileversion) && (html=="" || isURLT html)) Config.LinkArchive.localizeLinktestCases)]
  where safeLink :: (Show a) => (a,Inline) -> Bool
        safeLink (_, (Link _ _ (u,_))) = isURLT u
        safeLink x = error $ "Test.isURLT (URL of second).safeLink: passed an Inline which was not a 'Link' (with a valid URL)? erroring out. Original: " ++ show x

--------------------------------------------------------------------------------------------------------

testAll :: IO ()
testAll = do Config.Misc.cd

             printGreen ("Testing link icon matches…" :: String)
             unless (null linkIconTest) $ printRed ("Link icon rules have errors in: " ++ show linkIconTest)

             printGreen ("Testing interwiki rewrite rules…" :: String)
             unless (null interwikiTestSuite) $ printRed ("Interwiki rules have errors in: " ++ show interwikiTestSuite)
             unless (null interwikiCycleTestSuite) $ printRed ("Interwiki redirect rewrite rules have errors in: " ++ show interwikiCycleTestSuite)

             printGreen ("Tested config rules for uniqueness requirements, verified: " ++ show testConfigs)

             archives <- testLinkRewrites
             unless (null archives) $ printRed ("Link-archive rewrite test suite has errors in: " ++ show archives)
             printGreen ("Testing regexps for regex validity…" :: String)
             testRegexPatterns $
               [footnoteRegex, sectionAnonymousRegex, badUrlRegex] ++
               (map fst $ Config.Tags.wholeTagRewritesRegexes ++ Config.Metadata.Author.cleanAuthorsRegexps ++ Config.Metadata.Format.htmlRewriteRegexpBefore ++ Config.Metadata.Format.htmlRewriteRegexpAfter ++ Config.Misc.arxivAbstractRegexps ++ map (\(a,b) -> (T.unpack a, T.unpack b)) Config.LinkAuto.custom)
             let regexUnitTests = filter (\(before,after) -> Metadata.Format.cleanAbstractsHTML before /= after) Config.Metadata.Format.htmlRewriteTestCases
             unless (null regexUnitTests) $ printRed ("Regex rewrite unit test suite has errors in: " ++ show regexUnitTests)
             let twitterUsernameTests = filter (\(u,username) -> extractTwitterUsername u /= username) Config.Metadata.Author.extractTwitterUsernameTestSuite
             unless (null twitterUsernameTests) $ printRed ("Twitter username parsing unit test suite has errors in: " ++ show twitterUsernameTests)

             printGreen ("Reading in metadata databases…" :: String)
             md <- readLinkMetadata
             am <- readArchiveMetadataAndCheck

             let linkids = map (url2ID . T.pack) $ M.keys md
             let linkidLength = length $ isUniqueList linkids
             printGreen ("Checked URL hash uniqueness for: " ++ show linkidLength)

             printGreen ("Testing file-transclusions…" :: String)
             let fileTranscludes = isUniqueKeys $ fileTranscludesTest md am
             let fileTranscludesResults = filter (uncurry (/=)) fileTranscludes
             unless (null fileTranscludesResults) $ printRed ("File-transclude unit test suite has errors in: " ++ show fileTranscludesResults)

             unless (null authorCollapseTest) $ printRed ("Author-collapse test suite has errors in: " ++ show authorCollapseTest)

             xn <- testXOTD
             printGreen ("Testing X-of-the-day data… verified: " ++ show xn :: String)

             unless (null printDoubleTestSuite) $ printRed ("Double-printing function test suite has errors in: " ++ show printDoubleTestSuite)

             unless (null Cycle.testCycleDetection) $ printRed ("Cycle-detection test suite has errors in: " ++ show Cycle.testCycleDetection)

             unless (null titleCaseTest) $ printRed ("Title-case typography test suite has errors in: " ++ show titleCaseTest)

             unless (null dateRangeDurationTestCasesTestsuite) $ printRed ("Date-range-duration subscript typography test suite has errors in: " ++ show dateRangeDurationTestCasesTestsuite)

             printGreen ("Tested HTML/author cleanup rules for infinite loops, verified: " ++ show (length (cleanAbstractsHTMLTest ++ cleanAuthorsTest)))

             printGreen ("Testing tag rewrites…" :: String)
             testTags

             printGreen ("Testing live-link-popup rules…" :: String)
             unless (null linkLiveTest) $ printRed ("Live link popup rules have errors in: " ++ show linkLiveTest)
             _ <- linkLivePrioritize -- generate testcases for new live-link targets
             -- NOTE: we skip `linkLiveTestHeaders` due to requiring too much time & IO & bandwidth, and instead do it once in a while post-sync

             unless (null inflationDollarTestSuite) $ printRed ("Inflation-adjustment rules have errors in: " ++ show inflationDollarTestSuite)

             let tooltipResults = filter (\((t1, t2), goodResult) -> Annotation.tooltipToMetadata t1 t2 /= goodResult) Config.Misc.tooltipToMetadataTestcases
             unless (null tooltipResults) $ printRed ("Tooltip-parsing rules have errors in: " ++ show tooltipResults)

             printGreen ("Testing LinkAuto rewrites…" :: String)
             unless (null linkAutoTest) $ printRed ("LinkAuto test-cases have errors in: " ++ show linkAutoTest)

             printGreen ("Testing finished." :: String)
