module Test where

import Control.Monad (unless)
import Data.List (foldl')
import qualified Data.Set as Set (empty, insert, member)

import Annotation (tooltipToMetadataTest)
import Arrow (testUpDownArrows)
import Inflation (inflationDollarTestSuite)
import Interwiki (interwikiTestSuite, interwikiCycleTestSuite)
import LinkArchive (testLinkRewrites)
import LinkAuto (linkAutoTest)
import LinkIcon (linkIconTest)
import LinkLive (linkLiveTest, linkLivePrioritize)
import Tags (testTags)
import Typography (titleCaseTest)
import Utils (printGreen, printRed, printDoubleTestSuite, testCycleDetection, cleanAbstractsHTMLTest, cleanAuthorsTest)
import qualified Config.GenerateSimilar (blackListURLs)
import qualified Config.Interwiki (testCases, quoteOverrides, redirectDB)
import qualified Config.LinkArchive (whiteListMatchesFixed)
import qualified Config.LinkIcon (prioritizeLinkIconBlackList, linkIconTestUnitsText)
import qualified Config.LinkLive (goodDomainsSub, goodDomainsSimple, badDomainsSub, badDomainsSimple, goodLinks, badLinks)
import qualified Config.LinkSuggester (badAnchorStrings, whiteList)
import qualified Config.Tags (shortTagBlacklist, tagsLong2Short, wholeTagRewritesRegexes, tagsShort2LongRewrites, shortTagTestSuite)
import qualified Typography (titleCaseTestCases)
import qualified Config.Typography (surnameFalsePositivesWhiteList)
import qualified Config.XOfTheDay (siteBlackList)
import qualified Arrow (arrowTestCases)
import qualified Config.Inflation (bitcoinUSDExchangeRateHistory, inflationDollarLinkTestCases)
import qualified Config.LinkAuto (custom)
import qualified Config.LinkID (linkIDOverrides)
import qualified Utils (cleanAuthorsFixedRewrites, cycleTestCases, cleanAuthorsRegexps, htmlRewriteRegexp, htmlRewriteFixed, filterMetaBadSubstrings, filterMetaBadWholes)

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
isUnique = checkUniqueOrThrow "Association List contains duplicate key-value pairs:"

-- 2. isUniqueKeys: all keys are unique and there are no duplicates
isUniqueKeys :: (Eq a, Ord a, Show a, Show b) => [(a,b)] -> [(a,b)]
isUniqueKeys xs
  | null duplicates = xs
  | otherwise = throwError "Association List contains duplicate keys:" duplicates
  where duplicates = getDuplicates (map fst xs)
-- special-case:
isUniqueKeys3 :: (Eq a, Ord a, Show a) => [(a,b,c)] -> [(a,b,c)]
isUniqueKeys3 xs
  | null duplicates = xs
  | otherwise = throwError "Association List contains duplicate keys:" duplicates
  where duplicates = getDuplicates (map (\(a,_,_) -> a) xs)

-- 3. isUniqueValues: all values are unique and there are no duplicates
isUniqueValues :: (Show a, Ord a, Eq b, Ord b, Show b) => [(a,b)] -> [(a,b)]
isUniqueValues xs
  | null duplicates = xs
  | otherwise = throwError "Association List contains duplicate values:" duplicates
  where duplicates = getDuplicates (map snd xs)

-- 4. isUniqueAll: all keys, values, and key-value pairs are unique
isUniqueAll :: (Eq a, Ord a, Show a, Eq b, Ord b, Show b) => [(a,b)] -> [(a,b)]
isUniqueAll xs = isUniqueValues $ isUniqueKeys $ isUnique xs

-- we prefer to test configs in a single centralized place, as inconvenient as that is, because if we simply test inside the function itself on every call, we incur overhead and we risk accidentally-quadratic behavior (like when a filter or cleaning function is applied to every entry in list or database, and has to test every entry in the config for uniqueness each time).
testConfigs :: Int
testConfigs = sum $ map length [isUniqueList Utils.filterMetaBadSubstrings, isUniqueList Utils.filterMetaBadWholes
                               , isUniqueList Config.GenerateSimilar.blackListURLs
                               , isUniqueList Config.LinkArchive.whiteListMatchesFixed
                               , isUniqueList Config.Tags.shortTagBlacklist
                               , isUniqueList Config.Typography.surnameFalsePositivesWhiteList
                               ] ++ -- String
                               (map length [isUniqueList Config.LinkIcon.prioritizeLinkIconBlackList
                                           , isUniqueList Config.LinkLive.goodDomainsSub, isUniqueList Config.LinkLive.goodDomainsSimple, isUniqueList Config.LinkLive.badDomainsSub, isUniqueList Config.LinkLive.badDomainsSimple, isUniqueList Config.LinkLive.goodLinks, isUniqueList Config.LinkLive.badLinks
                                           , isUniqueList Config.LinkSuggester.badAnchorStrings
                                           , isUniqueList Config.XOfTheDay.siteBlackList]) ++ -- T.Text
              [length $ isUniqueKeys3 Config.LinkIcon.linkIconTestUnitsText] ++
              [length $ isUniqueKeys Config.Interwiki.testCases, length (isUniqueKeys Config.Interwiki.redirectDB), length $ isUniqueList Config.Interwiki.quoteOverrides
              , length $ isUniqueAll Config.LinkSuggester.whiteList
              , length $ isUniqueAll Config.Tags.tagsLong2Short, length $ isUniqueKeys Config.Tags.wholeTagRewritesRegexes, length $ isUniqueKeys Config.Tags.tagsShort2LongRewrites, length $ isUniqueKeys Config.Tags.shortTagTestSuite
              , length $ isUniqueKeys Typography.titleCaseTestCases
              , length $ isUniqueAll Arrow.arrowTestCases
              , length $ isUniqueKeys Config.Inflation.bitcoinUSDExchangeRateHistory, length $ isUniqueAll Config.Inflation.inflationDollarLinkTestCases
              , length $ isUniqueAll Config.LinkAuto.custom
              , length $ isUniqueAll Config.LinkID.linkIDOverrides
              , length $ isUniqueKeys Utils.cleanAuthorsFixedRewrites, length $ isUniqueKeys Utils.cycleTestCases, length $ isUniqueKeys Utils.cleanAuthorsRegexps, length $ isUniqueKeys Utils.htmlRewriteRegexp, length $ isUniqueKeys Utils.htmlRewriteFixed]

-------------------------------------------------------------------------------------------------------------------------------

testAll :: IO ()
testAll = do printGreen ("Testing link icon matches…" :: String)
             unless (null linkIconTest) $ printRed ("Link icon rules have errors in: " ++ show linkIconTest)

             unless (null testUpDownArrows) $ printRed ("Self-link arrow up/down AST test suite has errors in: " ++ show testUpDownArrows)

             unless (null printDoubleTestSuite) $ printRed ("Double-printing function test suite has errors in: " ++ show printDoubleTestSuite)

             unless (null testCycleDetection) $ printRed ("Cycle-detection test suite has errors in: " ++ show testCycleDetection)

             unless (null titleCaseTest) $ printRed ("Title-case typography test suite has errors in: " ++ show titleCaseTest)

             printGreen ("Tested HTML/author cleanup rules for infinite loops, verified: " ++ show (length (cleanAbstractsHTMLTest ++ cleanAuthorsTest)))

             printGreen ("Tested config rules for uniqueness requirements, verified: " ++ show testConfigs)

             archives <- testLinkRewrites
             unless (null archives) $ printRed ("Link-archive rewrite test suite has errors in: " ++ show archives)

             printGreen ("Testing tag rewrites…" :: String)
             testTags

             printGreen ("Testing live-link-popup rules…" :: String)
             unless (null linkLiveTest) $ printRed ("Live link pop rules have errors in: " ++ show linkLiveTest)
             _ <- linkLivePrioritize -- generate testcases for new live-link targets
             -- NOTE: we skip `linkLiveTestHeaders` due to requiring too much time & IO & bandwidth, and instead do it once in a while post-sync

             printGreen ("Testing interwiki rewrite rules…" :: String)
             unless (null interwikiTestSuite) $ printRed ("Interwiki rules have errors in: " ++ show interwikiTestSuite)
             unless (null interwikiCycleTestSuite) $ printRed ("Interwiki redirect rewrite rules have errors in: " ++ show interwikiCycleTestSuite)

             unless (null inflationDollarTestSuite) $ printRed ("Inflation-adjustment rules have errors in: " ++ show inflationDollarTestSuite)

             unless (null tooltipToMetadataTest) $ printRed ("Tooltip-parsing rules have errors in: " ++ show tooltipToMetadataTest)

             printGreen ("Testing LinkAuto rewrites…" :: String)
             unless (null linkAutoTest) $ printRed ("LinkAuto test-cases have errors in: " ++ show linkAutoTest)

             printGreen ("Testing finished." :: String)
