#!/usr/bin/env runghc

-- Parse the website & quote databases for the X-of-the-day, and use edit-distance to check for duplicates.
import Control.Monad (forM)
import Data.List (sortOn)
import System.IO (readFile)
import Text.Read (readMaybe)
import Text.EditDistance (levenshteinDistance, defaultEditCosts)

type Quote = (String, String, Bool)
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
    sites <- readData dbSites
    let sortedSites = sortData sites
    let rankedSites = rankData maxDistanceSite sortedSites
    let nonEmptyMatchesSites = filter (not . null . snd) rankedSites
    mapM_ print nonEmptyMatchesSites

    quotes <- readData dbQuotes
    let sortedQuotes = sortData quotes
    let filteredQuotes = filter (\(q,_,_) -> length q > minQuoteLength) sortedQuotes
    let rankedQuotes = rankData maxDistanceQuote filteredQuotes
    let nonEmptyMatchesQuotes = filter (not . null . snd) rankedQuotes
    mapM_ print nonEmptyMatchesQuotes

-- config:
dbQuotes :: String
dbQuotes = "/home/gwern/wiki/metadata/quotes.hs"

dbSites :: String
dbSites = "/home/gwern/wiki/metadata/sites.hs"

-- heuristically chosen:
maxDistanceQuote, minQuoteLength, maxDistanceSite :: Int
maxDistanceQuote = 19
minQuoteLength = 27

maxDistanceSite = 3
