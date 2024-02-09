#!/usr/bin/env runghc

-- Parse the website & quote databases for the X-of-the-day, and use edit-distance to check for duplicates.
import Data.List (sortOn)
import Text.Read (readMaybe)
import Text.EditDistance (levenshteinDistance, defaultEditCosts)

import Config.XOfTheDay as C (quoteDBPath, siteDBPath)

-- type Quote = (String, String, Bool)
type Site = (String, String, Bool)

readData :: Read a => FilePath -> IO [a]
readData path = do
    content <- readFile path
    case readMaybe content of
        Just dta -> return dta
        Nothing -> return []

sortData :: [Site] -> [Site]
sortData = sortOn (\(site, _, _) -> site)

rankData :: Int -> [Site] -> [(Site, [(Site, Int)])]
rankData maxDist dta = [(datum, filter ((<= maxDist) . snd) . sortOn snd $ [(other, levenshteinDistance defaultEditCosts d o) | other@(o, _, _) <- dta, datum /= other]) | datum@(d, _, _) <- dta]

main :: IO ()
main = do
    sites <- readData C.siteDBPath
    let sortedSites = sortData sites
    let rankedSites = rankData maxDistanceSite sortedSites
    let nonEmptyMatchesSites = filter (not . null . snd) rankedSites
    mapM_ print nonEmptyMatchesSites

    quotes <- readData C.quoteDBPath
    let sortedQuotes = sortData quotes
    let filteredQuotes = filter (\(q,_,_) -> length q > minQuoteLength) sortedQuotes
    let rankedQuotes = rankData maxDistanceQuote filteredQuotes
    let nonEmptyMatchesQuotes = filter (not . null . snd) rankedQuotes
    mapM_ print nonEmptyMatchesQuotes

-- heuristically chosen:
maxDistanceQuote, minQuoteLength, maxDistanceSite :: Int
maxDistanceQuote = 18
minQuoteLength = 26

maxDistanceSite = 3
