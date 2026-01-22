-- nginxredirectguesser.hs: Semi-automate writing nginx redirect rules to fix incoming broken URL requests on Gwern.net.
-- For existing rules, see <https://gwern.net/static/nginx/redirect/move.conf> <https://gwern.net/static/nginx/redirect/broken.conf>
--
-- Because the set of redirects is so large already (>30k), there will often be a close but not exact match, of the sort hard to have written a regexp in advance for (and my past attempts to write very clever regexps have often backfired on me).
-- We take the corpus quite literally. Instead of attempting fancy Transformer stuff (possible but I'm still chary), we simply ad hoc parse the set of rules as pairs, look for any exact filename matches else run a fuzzy <https://en.wikipedia.org/wiki/Levenshtein_distance> match between match & current error, and guess that the destination is what we need to fix the error.
-- The emitted rules will, of course, need to be reviewed & fixed by hand, but about half of them (the more tedious half) will already be written correctly.
--
-- example use: $ cd ~/wiki/ && xclip -o | sort -u | runghc -istatic/build/ static/build/nginxredirectguesser.hs
-- ...
-- "~^/doc/history/medici/1963-deroover-theriseanddeclineofthemedici-bank\.pdf.*$" "";
-- "~^/doc/iq/1994-herrnstein-murray-bellcurve\.pdf.*$" "";
-- "~^/doc/iq/1994-herrnsteinbellcurve\.pdf.*$" "";
-- "~^/metadata/annotation/%252Fw%252Fapi.php.html$" "";
-- ...
-- "~^//doc/ai//about$" "/about";
-- "~^/0.0_phpinfo.php$" "";
-- "~^/0_info.php$" "";
-- "~^/Changelog.xml$" "/changelog";
-- "~^/ID3/about.php$" "/about";
-- ...
-- "~^/.cymru$" "404";
-- "~^/note//rtx$" "/note/note";
-- "~^/static/font/dropcap/ninit/dark/C-28-scifi.png$" "/static/font/dropcap/ninit/dark/index";
-- "~^/static/font/dropcap/ninit/light/C-3-abstract.png$" "/static/font/dropcap/ninit/light/C-abstract-light-6.png";

import Control.Monad (forM)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isSuffixOf, sort, sortOn)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeFileName)

import Text.EditDistance (levenshteinDistance, defaultEditCosts)

import qualified Config.Misc as C (root)
import Utils (replaceChecked, replaceMany)

-- Trim leading/trailing whitespace (kills CRLF issues too).
strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

-- Stable de-dupe preserving first occurrence order.
nubOrd :: Ord a => [a] -> [a]
nubOrd = go S.empty
  where
    go _    [] = []
    go seen (x:xs)
      | x `S.member` seen = go seen xs
      | otherwise         = x : go (S.insert x seen) xs

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
main =
  do raw <- getContents
     let errors = S.toAscList . S.fromList . filter (not . null) . map strip . lines $ raw

     files <- listFilesRecursivelyWithBasename C.root

     one <- readFile "static/nginx/redirect/move.conf"
     two <- readFile "static/nginx/redirect/broken.conf"

     -- Optional: suppress exact-line duplicates already present in existing files.
     let existingLines = S.fromList . map strip . lines $ one ++ two

     let redirects = filter (\(_,b) -> b `notElem` ["/404\";","404\";"]) . map safeTuplize . filter ((== 2) . length) . map (splitOn "$\" \"") . lines $ one ++ two

     let redirectsCleaned = map (\(a,b) -> (filter (`notElem` ("~^.*?+[]\"" :: String)) a, b)) redirects

     let bestMatch err =
           case filter (\(d,_,_) -> d <= minDistance) (diffAndRank files redirectsCleaned err) of
             []    -> Nothing
             x : _ -> Just x

     let results :: [(String, Maybe (Int,String,String))]
         results = [ (err, bestMatch err) | err <- errors ]

     -- failures first; then closer matches; then lexicographic by err
     let resultsSorted =
           sortOn (\(err, m) ->
                     case m of
                       Nothing      -> (0 :: Int, maxBound :: Int, err)
                       Just (d,_,_) -> (1 :: Int, d, err))
                  results

     let mkRule err m =
           case m of
             Nothing ->
               "\"~^" ++ mkPattern err ++ "\" \"\";"
             Just (_,_,tgt) ->
               "\"~^" ++ mkPattern err ++ "\" \"" ++ tgt

     let generated = map (uncurry mkRule) resultsSorted

     -- De-dupe output, and drop rules already present verbatim in existing config.
     let generated' = nubOrd . filter (`S.notMember` existingLines) $ generated

     mapM_ putStrLn generated'

     where
       safeTuplize []      = error "nginxredirectguesser: main: safeTuplize: empty list!"
       safeTuplize [a]     = error $ "nginxredirectguesser: main: safeTuplize: only 1 item in list! original: " ++ show a
       safeTuplize (a:b:_) = (a,b)

-- Build the regex body after the leading "~^".
-- Special-case: if the *path part* ends in .pdf, emit \.pdf.*$ so queries/fragments/etc still match.
mkPattern :: String -> String
mkPattern err =
  let (path, _rest) = break (\c -> c == '?' || c == '#') err
  in if ".pdf" `isSuffixOf` path
        then escapeRegex (take ((length path :: Int) - (length (".pdf"::String) :: Int)) path) ++ "\\.pdf.*$"
        else escapeRegex err ++ "$"

-- Escape a string so it is a literal PCRE regex fragment.
-- (We also escape "~" just in case; it’s not a regex metacharacter but harmless.)
escapeRegex :: String -> String
escapeRegex = replaceMany
  [ ("\\", "\\\\")
--  , (".",  "\.")
  , ("?",  "\\?")
  , ("*",  "\\*")
  , ("+",  "\\+")
  , ("|",  "\\|")
  , ("(",  "\\(")
  , (")",  "\\)")
  , ("[",  "\\[")
  , ("]",  "\\]")
  , ("{",  "\\{")
  , ("}",  "\\}")
  , ("^",  "\\^")
  , ("$",  "\\$")
  , ("~",  "\\~")
  , ("\"", "\\\"")
  ]

minDistance :: Int
minDistance = 2

diffAndRank :: [(FilePath,FilePath)] -> [(String,String)] -> String -> [(Int,String,String)]
diffAndRank filedb redirects err =
  let fuzzy = sort $ map (\(a,b) -> (levenshteinDistance defaultEditCosts err a, a, b)) redirects

      -- Shortcut exact basename matches: they should always win.
      exact =
        case lookup (takeFileName err) filedb of
          Nothing -> []
          Just b  -> [(0, err, b ++ "\";")]

  in exact ++ fuzzy

