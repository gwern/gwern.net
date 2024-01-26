module Test where

import Control.Monad (unless)

import Annotation (tooltipToMetadataTest)
import Inflation (inflationDollarTestSuite)
import Interwiki (interwikiTestSuite, interwikiCycleTestSuite)
import LinkArchive (testLinkRewrites)
import LinkIcon (linkIconTest)
import LinkLive (linkLiveTest, linkLivePrioritize)
import Tags (testTags)
import Typography (titleCaseTest)
import Utils (printGreen, printRed, printDoubleTestSuite, testCycleDetection, cleanAbstractsHTMLTest, cleanAuthorsTest)
import Arrow (testUpDownArrows)

testAll :: IO ()
testAll = do printGreen ("Testing link icon matches…" :: String)
             let linkIcons = linkIconTest
             unless (null linkIcons) $ printRed ("Link icon rules have errors in: " ++ show linkIcons)

             let arrows = testUpDownArrows
             unless (null arrows) $ printRed ("Self-link arrow up/down AST test suite has errors in: " ++ show arrows)

             let doubles = printDoubleTestSuite
             unless (null doubles) $ printRed ("Double-printing function test suite has errors in: " ++ show doubles)

             let cycles = testCycleDetection
             unless (null cycles) $ printRed ("Cycle-detection test suite has errors in: " ++ show cycles)

             let cases = titleCaseTest
             unless (null cases) $ printRed ("Title-case typography test suite has errors in: " ++ show cases)

             let infixRewrites = cleanAbstractsHTMLTest ++ cleanAuthorsTest
             printGreen ("Tested HTML/author cleanup rules for infinite loops, verified: " ++ show (length infixRewrites))

             archives <- testLinkRewrites
             unless (null archives) $ printRed ("Link-archive rewrite test suite has errors in: " ++ show archives)

             printGreen ("Testing tag rewrites…" :: String)
             testTags

             printGreen ("Testing live-link-popup rules…" :: String)
             let livelinks = linkLiveTest
             unless (null livelinks) $ printRed ("Live link pop rules have errors in: " ++ show livelinks)
             _ <- linkLivePrioritize -- generate testcases for new live-link targets
             -- NOTE: we skip `linkLiveTestHeaders` due to requiring too much time & IO & bandwidth, and instead do it once in a while post-sync

             printGreen ("Testing interwiki rewrite rules…" :: String)
             let interwikiPopupTestCases = interwikiTestSuite
             unless (null interwikiPopupTestCases) $ printRed ("Interwiki rules have errors in: " ++ show interwikiPopupTestCases)
             let interwikiCycleTestCases = interwikiCycleTestSuite
             unless (null interwikiCycleTestCases) $ printRed ("Interwiki redirect rewrite rules have errors in: " ++ show interwikiCycleTestCases)

             let inflationTestCases = inflationDollarTestSuite
             unless (null inflationTestCases) $ printRed ("Inflation-adjustment rules have errors in: " ++ show inflationTestCases)

             unless (null tooltipToMetadataTest) $ printRed ("Tooltip-parsing rules have errors in: " ++ show tooltipToMetadataTest)
             printGreen ("Testing finished." :: String)
