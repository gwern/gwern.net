-- Semi-automate writing nginx redirect rules to fix incoming broken URL requests.
-- Because the set of redirects is so large already, there will often be a close but not exact match, of the sort hard to have written a regexp in advance for (and my past attempts to write very clever regexps have often backfired on me).
-- We take the corpus quite literally. Instead of attempting fancy Transformer stuff (possible but I'm still chary), we simply ad hoc parse the set of rules as pairs, look for any exact filename matches else run a fuzzy <https://en.wikipedia.org/wiki/Levenshtein_distance> match between match & current error, and guess that the destination is what we need to fix the error.
-- The emitted rules will, of course, need to be reviewed & fixed by hand, but about half of them (the more tedious half) will already be written correctly.
-- example use: $ cd ~/wiki/ && xclip -o | sort -u | runghc -istatic/build/ static/build/nginxredirectguesser.hs

import Control.Monad (forM)
import Data.List (sort, sortOn)
import Data.List.Split (splitOn) -- split
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeFileName)

import Text.EditDistance (levenshteinDistance, defaultEditCosts) -- <https://hackage.haskell.org/package/edit-distance>

import Config.Misc as C (root)
import Utils (replaceChecked, replaceMany)

listFilesRecursivelyWithBasename :: FilePath -> IO [(FilePath, FilePath)]
listFilesRecursivelyWithBasename dir = do
  contents <- getDirectoryContents dir
  let files = filter (`notElem` [".", "..", ".git", "_cache", "_site"]) contents
  paths <- forM files $ \file -> do
    let path = dir </> file
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then listFilesRecursivelyWithBasename path
      else return [(takeFileName path, replaceChecked C.root "/" path)]
  return $ concat paths

main :: IO ()
main = do errors <- fmap lines getContents
          files <- listFilesRecursivelyWithBasename C.root
          one <- readFile "static/redirect/nginx.conf"
          two <- readFile "static/redirect/nginx-broken.conf"
          let redirects = filter (\(_,b) -> b /= "/static/404\";") $ map (\(a:b:_) -> (a,b)) $ filter (\p -> length p == 2) $ map (splitOn "$\" \"") $ lines $ one ++ two
          let redirectsCleaned = map (\(a,b) -> (filter (`notElem` ("~^.*?+[]\""::String)) a, b)) redirects
          let errorDistances = zip errors $ map (filter (\(d,_,_) -> d <= minDistance) . diffAndRank files redirectsCleaned) errors -- :: [(String, [(Int,String,String)])]
          let redirectPairs = (sortBySecondField $ map (\(err,candidates) -> (err, if null candidates then "" else (\(_,_,target) -> target) (head candidates))) errorDistances) :: [(String,String)]
          let redirectsGenerated = map (\(er, candidate) -> if null candidate then "\"~^" ++ replaceMany [(".pdf$", "\\.pdf.*$")] (escape er ++ "$") ++ "\" \"\";"
                                                             else "\"~^" ++ replaceMany [(".pdf$", "\\.pdf.*$")] (escape er ++ "$") ++ "\" \"" ++ candidate) redirectPairs
          mapM_ putStrLn redirectsGenerated

escape :: String -> String
escape = replaceMany [("?","\\?"), ("[", "\\["), ("]", "\\]"), ("(", "\\("), (")", "\\)"), ("^", "\\^"), ("$", "\\$")]

sortBySecondField :: [(String, String)] -> [(String, String)]
sortBySecondField = sortOn snd

minDistance :: Int
minDistance = 4

diffAndRank :: [(FilePath,FilePath)] -> [(String,String)] -> String -> [(Int,String,String)]
diffAndRank filedb redirects err = let fuzzy = sort $ map (\(a,b) -> (levenshteinDistance defaultEditCosts err a, a, b)) redirects
                                       -- shortcut exact filename matches: they should always win.
                                       exact = case lookup (takeFileName err) filedb of
                                                 Nothing -> []
                                                 Just b -> [(0,err,b++"\";")]
                                   in exact ++ fuzzy
