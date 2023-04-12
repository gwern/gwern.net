-- Semi-automate writing nginx redirect rules to fix incoming broken URL requests.
-- Because the set of redirects is so large already, there will often be a close but not exact match, of the sort hard to have written a regexp in advance for (and my past attempts to write very clever regexps have often backfired on me).
-- We take the corpus quite literally. Instead of attempting fancy Transformer stuff (possible but I'm still chary), we simply ad hoc parse the set of rules as pairs, run a fuzzy <https://en.wikipedia.org/wiki/Levenshtein_distance> match between match & current error, and guess that the destination is what we need to fix the error.
-- The emitted rules will, of course, need to be reviewed & fixed by hand, but about half of them (the more tedious half) will already be written correctly.
-- example use: $ cd ~/wiki/ && xclip -o | sort -u | runghc -istatic/build/ static/build/nginxredirectguesser.hs

import Text.EditDistance (levenshteinDistance, defaultEditCosts) -- <https://hackage.haskell.org/package/edit-distance>
import Data.List.Split (splitOn)
import Data.List (sort, sortBy, sortOn)
import Data.Ord (comparing)
import Utils (replaceMany)

main :: IO ()
main = do errors <- fmap lines getContents
          one <- readFile "static/redirect/nginx.conf"
          two <- readFile "static/redirect/nginx-broken.conf"
          let redirects = filter (\(_,b) -> b /= "/static/404\";") $ map (\(a:b:_) -> (a,b)) $ filter (\p -> length p == 2) $ map (splitOn "$\" \"") $ lines $ one ++ two
          let redirectsCleaned = map (\(a,b) -> (filter (`notElem` ("~^.*?+[]\""::String)) a, b)) redirects
          let errorDistances = zip errors $ map (filter (\(d,_,_) -> d <= minDistance) . diffAndRank redirectsCleaned) errors -- :: [(String, [(Int,String,String)])]
          let redirectPairs = (sortBySecondField $ map (\(err,candidates) -> (replaceMany [(".pdf$", "\\.pdf.*$")] err, if null candidates then "" else (\(_,_,target) -> target) (head candidates))) errorDistances) :: [(String,String)]
          let redirectsGenerated = map (\(er, candidate) -> if null candidate then "\"~^" ++ escape er ++ "$\" \"\";"
                                                             else "\"~^" ++ escape er ++ "$\" \"" ++ candidate) redirectPairs
          mapM_ putStrLn redirectsGenerated

escape :: String -> String
escape = replaceMany [("?","\\?"), ("[", "\\["), ("]", "\\]"), ("(", "\\("), (")", "\\)")]

sortBySecondField :: [(String, String)] -> [(String, String)]
sortBySecondField = sortOn snd

minDistance :: Int
minDistance = 4

diffAndRank :: [(String,String)] -> String -> [(Int,String,String)]
diffAndRank redirects err = sort $ map (\(a,b) -> (levenshteinDistance defaultEditCosts err a, a, b)) redirects
