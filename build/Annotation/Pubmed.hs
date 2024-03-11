module Annotation.Pubmed where

import Data.List (intercalate)
import Text.Pandoc (readerExtensions, writeHtml5String, readMarkdown, def, runPure, pandocExtensions)
import Text.Show.Pretty (ppShow)
import System.Exit (ExitCode(ExitFailure))
import qualified Data.ByteString.Lazy.UTF8 as U (toString) -- TODO: why doesn't using U.toString fix the Unicode problems?
import qualified Data.Text as T (pack, unpack)
import Data.FileStore.Utils (runShellCommand)
import LinkAuto (linkAutoHtml5String)

import LinkMetadataTypes (MetadataItem, Failure(..), Path)
import MetadataFormat (cleanAbstractsHTML, processDOI, trimTitle, checkURL, cleanAuthors)
import Utils (replace, trim, safeHtmlWriterOptions, sed, printGreen)
import Paragraph (processParagraphizer)

-- handles both PM & PLOS right now:
pubmed  :: Path -> IO (Either Failure (Path, MetadataItem))
pubmed l = do checkURL l
              (status,_,mb) <- runShellCommand "./" Nothing "Rscript" ["static/build/linkAbstract.R", l]
              case status of
                ExitFailure err -> printGreen (intercalate " : " [l, ppShow status, ppShow err, ppShow mb]) >> return (Left Permanent)
                _ -> do
                        let parsed = lines $ replace " \n" "\n" $ trim $ U.toString mb
                        if length parsed < 5 then return (Left Permanent) else
                          do let (title:author:date:doi:abstrct) = parsed
                             let ts = [] -- TODO: replace with ML call to infer tags
                             abstract' <- fmap cleanAbstractsHTML $ processParagraphizer l $ linkAutoHtml5String $ processPubMedAbstract $ unlines abstrct
                             return $ Right (l, (cleanAbstractsHTML $ trimTitle title, cleanAuthors $ trim author, trim date, "", [("doi",trim $ processDOI doi)], ts, abstract'))

processPubMedAbstract :: String -> String
processPubMedAbstract abst = let clean = runPure $ do
                                   -- strip overly-indented PLOS formatting like:
                                   -- <abstract>
                                   --   <sec>
                                   --     <title>Background</title>
                                   --     <p>Systematic reviews (SRs) of TCM have become increasingly popular in China and have been published in large numbers. This review provides the first examination of epidemiological characteristics of these SRs as well as compliance with the PRISMA and AMSTAR guidelines.</p>
                                   --   </sec>
                                   --   <sec>
                                   --     <title>Objectives</title>...
                                   pandoc <- readMarkdown def{readerExtensions=pandocExtensions} (T.pack $ sed "^ +" "" abst)
                                   html <- writeHtml5String safeHtmlWriterOptions pandoc
                                   return $ T.unpack html
                             in case clean of
                                  Left e -> error $ ppShow e ++ " : " ++ abst
                                  Right output -> cleanAbstractsHTML $ replace "<br />" "" output
