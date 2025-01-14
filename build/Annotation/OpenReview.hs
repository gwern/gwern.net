module Annotation.OpenReview where

import Data.List (intercalate)
import System.Exit (ExitCode(ExitFailure))
import qualified Data.ByteString.Lazy.UTF8 as U (toString) -- TODO: why doesn't using U.toString fix the Unicode problems?
import Data.FileStore.Utils (runShellCommand)
import LinkAuto (linkAutoHtml5String)

import LinkMetadataTypes (Metadata, MetadataItem, Failure(..), Path)
import Metadata.Format (cleanAbstractsHTML, trimTitle, checkURL)
import Metadata.Author (cleanAuthors)

import Utils (replace, trim, printRed)
import Paragraph (processParagraphizer)

import Annotation.Arxiv (processArxivAbstract)

openreview :: Metadata -> Path -> IO (Either Failure (Path, MetadataItem))
openreview md p =
               do checkURL p
                  let p' = replace "/pdf?id=" "/forum?id=" p
                  (status,_,bs) <- runShellCommand "./" Nothing "openReviewAbstract.sh" [p']
                  case status of
                      ExitFailure _ -> printRed ("OpenReview download failed: " ++ p) >> return (Left Permanent)
                      _ -> do
                             let results = lines $ U.toString bs
                             case results of
                               (title:author:date:tldr:desc:keywords) -> do
                                    let keywords'
                                           | null keywords || keywords == [""] = ""
                                           | length keywords > 1 =
                                             unlines (init keywords) ++ "\n<!-- [Keywords: " ++ last keywords ++ "] -->"
                                           | otherwise = "<!-- [Keywords: " ++ concat keywords ++ "] -->"
                                    let tldr' = cleanAbstractsHTML $ processArxivAbstract tldr
                                    let desc' = cleanAbstractsHTML $ processArxivAbstract desc
                                    abstract <- processParagraphizer md p' $ linkAutoHtml5String $ cleanAbstractsHTML $ processArxivAbstract keywords'
                                    let abstractCombined = trim $ intercalate "\n" [tldr', desc', abstract]
                                    return $ Right (p, (trimTitle title, cleanAuthors $ trim author, date, "", [], [],
                                                        -- due to pseudo-LaTeX
                                                          abstractCombined))
                               _ -> printRed ("OpenReview failed to parse enough metadata fields? " ++ unlines results) >> return (Left Permanent)
