{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Annotation.PDF where

import System.Directory (doesFileExist)
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.UTF8 as U (toString) -- TODO: why doesn't using U.toString fix the Unicode problems?
import Data.Aeson (eitherDecode, FromJSON)
import Data.FileStore.Utils (runShellCommand)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL (length, concat)

import LinkAuto (linkAutoHtml5String)
import LinkMetadataTypes (MetadataItem, Path, Failure(Permanent))
import MetadataFormat (cleanAbstractsHTML, trimTitle, filterMeta, cleanAuthors, processDOI, pageNumberParse)
import Paragraph (processParagraphizer)
import Utils (printGreen, printRed, replace, trim)
import Config.Misc as C (cd)

pdf :: Path -> IO (Either Failure (Path, MetadataItem))
pdf "" = error "Fatal error: `Annotation.PDF.pdf` called on empty string argument; this should never happen."
pdf p = do C.cd
           let p' = takeWhile (/='#') $ if head p == '/' then tail p else p
           existsp <- doesFileExist p'
           unless existsp $ error $ "PDF file doesn't exist? Tried to query " ++ p
           let pageNumber = pageNumberParse p
           let pageNumber' = if pageNumber == p then "" else pageNumber

           (_,_,mbTitle)  <- runShellCommand "./" Nothing "exiftool" ["-printFormat", "$Title",  "-Title",  p']
           (_,_,mbAuthor) <- runShellCommand "./" Nothing "exiftool" ["-printFormat", "$Author", "-Author", p']
           (_,_,mbCreator)<- runShellCommand "./" Nothing "exiftool" ["-printFormat", "$Creator", "-Creator", p']
           (_,_,mbDate)   <- runShellCommand "./" Nothing "exiftool" ["-printFormat", "$Date",    "-dateFormat", "%F", "-Date", p']
           (_,_,mbDoi)    <- runShellCommand "./" Nothing "exiftool" ["-printFormat", "$DOI",     "-DOI", p']
           if BL.length (BL.concat [mbTitle,mbAuthor,mbDate,mbDoi]) > 0 then
             do printGreen (show [mbTitle,mbCreator,mbAuthor,mbDate,mbDoi])
                let titleBase = filterMeta (trimTitle $ cleanAbstractsHTML $ U.toString mbTitle)
                let title = titleBase ++ (if null pageNumber' || null titleBase then "" else " § pg" ++ pageNumber')
                let edoi = trim $ U.toString mbDoi
                let edoi' = if null edoi then "" else processDOI edoi
                -- PDFs have both a 'Creator' and 'Author' metadata field sometimes. Usually Creator refers to the (single) person who created the specific PDF file in question, and Author refers to the (often many) authors of the content; however, sometimes PDFs will reverse it: 'Author' means the PDF-maker and 'Creators' the writers. If the 'Creator' field is longer than the 'Author' field, then it's a reversed PDF and we want to use that field instead of omitting possibly scores of authors from our annotation.
                let ecreator = filterMeta $ U.toString mbCreator
                let eauthor' = filterMeta  $ U.toString mbAuthor
                let author = linkAutoHtml5String $ cleanAbstractsHTML $ cleanAuthors $ trim $ if length eauthor' > length ecreator then eauthor' else ecreator
                let ts = [] -- TODO: replace with ML call to infer tags
                printGreen $ "PDF: " ++ p ++" DOI: " ++ edoi'
                at <- fmap (fromMaybe "") $ doi2Abstract edoi'
                if not (null (title ++ author ++ U.toString mbDate ++ edoi')) then
                  return $ Right (p, (title, author, trim $ replace ":" "-" (U.toString mbDate), "", [("doi",edoi')], ts, at))
                  else
                  return (Left Permanent)
           else printRed "PDF annotation failed, insufficient data or unreadable file; exiftool returned: " >> putStrLn ("title/author/date: " ++ show mbTitle ++ " ; DOI: " ++ show mbDoi) >> return (Left Permanent)


-- nested JSON object: eg. 'jq .message.abstract'
newtype Crossref = Crossref { message :: Message } deriving (Show,Generic)
instance FromJSON Crossref
newtype Message = Message { abstract :: Maybe String } deriving (Show,Generic)
instance FromJSON Message
doi2Abstract :: String -> IO (Maybe String)
doi2Abstract doi = if length doi < 7 then return Nothing
                   else do (_,_,bs) <- runShellCommand "./" Nothing "curl" ["--location", "--silent", "https://api.crossref.org/works/"++doi, "--user-agent", "gwern+crossrefscraping@gwern.net"]
                           if bs=="Resource not found." then return Nothing
                           else let j = eitherDecode bs :: Either String Crossref
                                in case j of -- start unwrapping...
                                    Left e -> printRed ("Error: Crossref request failed: "++doi++" "++e) >> return Nothing
                                    Right j' -> let j'' = abstract $ message j' in
                                      case j'' of
                                       Nothing -> return Nothing
                                       Just a -> do trimmedAbstract <- fmap cleanAbstractsHTML $ processParagraphizer doi $ linkAutoHtml5String $ cleanAbstractsHTML a
                                                    return $ Just trimmedAbstract
