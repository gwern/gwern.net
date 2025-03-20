module Metadata.Title where

import Data.List (isInfixOf, intercalate)
import Network.HTTP (urlDecode)
import Data.FileStore.Utils (runShellCommand)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import System.Exit (ExitCode(ExitFailure))

import Metadata.Format (filterMeta, pageNumberParse, trimTitle, cleanAbstractsHTML)
import Utils (delete, replace, sed, anyInfix, trim, printRed, isURL, replaceMany, deleteMixedMany)
import qualified Config.Misc as CM (cd)
import qualified Config.Metadata.Title as C (separators, badStrings, badStringPatterns, stringReplace, stringDelete)

-- Attempt to parse tooltips back into citation metadata:
tooltipToMetadata :: String -> String -> (String,String,String)
tooltipToMetadata _ "" = ("","","")
tooltipToMetadata path s
                    | head s `elem` ['/', '!', '$', '\8383'] || anyInfix s ["Original URL:"] = ("","","")
                    | otherwise =
                        let title  = filterMeta $ sed "['‘“](.*)['’”], .*" "\\1" s
                            author = filterMeta $  sed "^Branwen$" "Gwern Branwen" $ replace " et al" ", et al" $ replace " & " ", " $
                                     sed "^([A-Z].*) [0-9][0-9][0-9][0-9][0-9-]*[a-z]?$" "\\1" $
                                     sed "['‘“].*['’”], ([A-Z].+) [0-9][0-9][0-9][0-9][0-9-]*[a-z]?" "\\1" s
                            date   = filterMeta $ sed "^[A-Za-z].+ ([0-9][0-9][0-9][0-9][0-9-]*)[a-z]?$" "\\1" $ sed "['‘“].+['’”], [A-Za-z].+ ([0-9][0-9][0-9][0-9][0-9-]*)[a-z]?$" "\\1" s
                            pageNumber = let n = pageNumberParse path in if null n then "" else " § pg" ++ n
                            (t,a,d) = (minLength 6 (changed title) ++ pageNumber, changed author, minLength 4 $ changed date) in
                          if a==d then (s,"","") else (t,a,d)
                    where changed x = if s==x then "" else x
                          minLength n x = if length x < n then "" else x

wikipediaURLToTitle :: String -> String
wikipediaURLToTitle "" = error "Metadata.Title.wikipediaURLToTitle: passed an empty string argument, which should never happen!"
wikipediaURLToTitle u  = trimTitle $ cleanAbstractsHTML $ replace "#" " § " $ urlDecode $ replace "% " "%25 " $ replace "_" " " $
                          delete "https://en.wikipedia.org/wiki/" u

htmlDownloadAndParseTitle :: String -> IO String
htmlDownloadAndParseTitle ""  = error "Metadata.Title.htmlDownloadAndParseTitle: passed an empty string argument, which should never happen!"
htmlDownloadAndParseTitle url =
 if not (isURL url) then error $ "Metadata.Title.htmlDownloadAndParseTitle: passed a non-URL argument, which should never happen: " ++ show url else
 do CM.cd
    let args = ["static/build/download-title.sh", url]
    (status,stderr,mb) <- runShellCommand "./" Nothing "bash" args
    case status of
        ExitFailure err -> printRed ("Exit Failure: " ++ intercalate " ::: " [url, show status, show err, show mb, show stderr]) >> return ""
        _ -> return $ U.toString mb

htmlDownloadAndParseTitleClean :: String -> IO String
htmlDownloadAndParseTitleClean "" = error "Metadata.Title.htmlDownloadAndParseTitleClean: passed an empty string argument, which should never happen!"
htmlDownloadAndParseTitleClean u  = if not (isURL u) then error $ "Metadata.Title.htmlDownloadAndParseTitleClean: passed a non-URL argument, which should never happen: " ++ show u else
 do
  title <- htmlDownloadAndParseTitle u
  let title' = trim $ unlines $ take 1 $ lines $ replaceMany C.stringReplace $ deleteMixedMany C.stringDelete $
                 if any (`elem` C.separators) title
                 then reverse $ tail $ dropWhile (`notElem` C.separators) $ reverse title
                 else title
  if title' `elem` C.badStrings || anyInfix title' C.badStringPatterns || length title' < 5 || length title' > 500
  then return "" -- no need to shell out to a LLM for cleaning if it is a known-bad title
  else
        do titleCleaned <- fmap cleanAbstractsHTML $ cleanTitleWithAI title'
           return $ if titleCleaned == "" then "" else
                     if titleCleaned /= title' && titleCleaned `isInfixOf` title' then titleCleaned
                       else title'

-- shell out to `title-cleaner.py` to clean a title using LLMs:
cleanTitleWithAI :: String -> IO String
cleanTitleWithAI title =
  do CM.cd
     (status,stderr,mb) <- runShellCommand "./" Nothing "static/build/title-cleaner.py" [title]
     case status of
       ExitFailure err -> printRed ("Exit Failure: " ++ intercalate " ::: " [show status, show err, show mb, show stderr]) >> return ""
       -- NOTE: to avoid confabulations or mangled rewrites, we impose a further requirement:
       -- that any changed title must be a strict substring of the original title,
       -- to reflect that the title-cleaning script should only be returning " ",
       -- the original title, or the title with ranges deleted but neither changed nor added to.
       _ -> return $ trim $ U.toString mb
