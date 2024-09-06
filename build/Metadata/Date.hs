{-# LANGUAGE OverloadedStrings #-}

module Metadata.Date where

import Data.List (isSuffixOf, intercalate)
import System.FilePath (takeBaseName)
import Data.FileStore.Utils (runShellCommand)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import System.Exit (ExitCode(ExitFailure))

import Data.Time (parseTimeM, defaultTimeLocale, Day)

import Utils (sed, split, trim, printRed, delete)
import Config.Misc as CD (cd)

dateTruncateBad :: String -> String
 -- we assume that dates are guaranteed to be 'YYYY[-MM[-DD]]' format because of the validation in readLinkMetadataAndCheck enforcing this
-- dates of the form 'YYYY-01-01' (or 'YYYY-01') are invariably lies, and mean just 'YYYY'.
dateTruncateBad d = if "-01-01" `isSuffixOf` d || (length d == 7 && "-01" `isSuffixOf` d) then take 4 d else d

isDate :: String -> Bool
isDate "" = True -- this makes checks/lints easier
isDate d = case length (split "-" d) of
    1 -> isValidDate "%Y" d
    2 -> isValidDate "%Y-%m" d
    3 -> isValidDate "%Y-%m-%d" d
    _ -> False
 where
  isValidDate :: String -> String -> Bool
  isValidDate format str = case parseTimeM True defaultTimeLocale format str :: Maybe Day of
      Just _ -> True
      Nothing -> False

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
                                           then date else let datePossible = sed "^([12][0-9][0-9][0-9])(-[0-9][0-9])?(-[0-9][0-9])?-.*" "\\1\\2\\3" f
                                                              in if datePossible == f then "" else datePossible

-- TODO: in a few months, after more counter-examples have been added, run this on all outstanding date-less metadata items. But first check it against all known dates.
-- LinkMetadata.walkAndUpdateLinkMetadata True (\x@(a,(_,_,d,_,_,_,_)) -> if d == "" && (not (null (intersect "0123456789" a))) then let date = System.IO.Unsafe.unsafePerformIO (guessDateFromString a) in if null date then return x else putStrLn (a ++ " : " ++ System.IO.Unsafe.unsafePerformIO (guessDateFromString a)) >> return x else return x)
-- `guessDateFromString` is called by `Annotation.linkDispatcher` & `GTX.fixDate` for slow reads.
guessDateFromString :: String -> IO String
guessDateFromString "" = error "Metadata.Format.guessDateFromString: passed an empty string argument, which should never happen!"
guessDateFromString u  =
        do CD.cd
           (status,stderr,mb) <- runShellCommand "./" Nothing "static/build/date-guesser.py" [u]
           case status of
               ExitFailure err -> printRed ("Exit Failure: " ++ intercalate " ::: " [u, show status, show err, show mb, show stderr]) >> return ""
               _ -> let dateNew = delete "\"\"" $ trim $ U.toString mb in
                      if isDate dateNew then return dateNew else error $ "Metadata.Format.guessDateFromString: date-guesser.py returned an invalid date: " ++ dateNew ++ "; input: " ++ u

