module Annotation.OpenReview where

import Data.List (intercalate)
import System.Exit (ExitCode(ExitFailure))
import qualified Data.ByteString.Lazy.UTF8 as U (toString) -- TODO: why doesn't using U.toString fix the Unicode problems?
import Data.FileStore.Utils (runShellCommand)
import LinkAuto (linkAutoHtml5String)

import LinkMetadataTypes (MetadataItem, Failure(..), Path)
import MetadataFormat (cleanAbstractsHTML, trimTitle, checkURL, cleanAuthors)
import Utils (replace, trim, printRed)

import Annotation.Arxiv (processArxivAbstract)

openreview :: Path -> IO (Either Failure (Path, MetadataItem))
openreview p = do checkURL p
                  let p' = replace "/pdf?id=" "/forum?id=" p
                  (status,_,bs) <- runShellCommand "./" Nothing "openReviewAbstract.sh" [p']
                  case status of
                      ExitFailure _ -> printRed ("OpenReview download failed: " ++ p) >> return (Left Permanent)
                      _ -> do
                             let (title:author:date:tldr:desc:keywords) = lines $ U.toString bs
                             let keywords'
                                    | null keywords || keywords == [""] = ""
                                    | length keywords > 1 =
                                      unlines (init keywords) ++ "\n[Keywords: " ++ last keywords ++ "]"
                                    | otherwise = "[Keywords: " ++ concat keywords ++ "]"
                             let tldr' = cleanAbstractsHTML $ processArxivAbstract tldr
                             let desc' = cleanAbstractsHTML $ processArxivAbstract desc
                             let abstractCombined = trim $ intercalate "\n" [tldr', desc', linkAutoHtml5String $ cleanAbstractsHTML $ processArxivAbstract keywords']
                             return $ Right (p, (trimTitle title, cleanAuthors $ trim author, date, "", [], [],
                                                 -- due to pseudo-LaTeX
                                                   abstractCombined))
