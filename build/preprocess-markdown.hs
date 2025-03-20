{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless, void)
import qualified Data.Text as T (unpack, isInfixOf)
import qualified Data.Text.IO as TIO (getContents)

import Text.Pandoc (def, pandocExtensions, runPure, readerExtensions, readMarkdown, writeHtml5String, Pandoc)

import LinkMetadata (cleanAbstractsHTML)
import LinkAuto (linkAuto)
import Interwiki (convertInterwikiLinks, isWPArticle)
import qualified GenerateSimilar as GS (singleShotRecommendations)
import Utils (replace, safeHtmlWriterOptions, sed)
import qualified Config.Misc as C (cd)
import Query (extractURLs)

main :: IO ()
main = do originalMarkdown <- TIO.getContents

          let pandocCleanEither = runPure $ do
                pandoc      <- readMarkdown def{readerExtensions=pandocExtensions} originalMarkdown
                let pandoc'  = convertInterwikiLinks pandoc
                let pandoc'' = linkAuto pandoc'
                return pandoc''
          case pandocCleanEither of
            Left       e           -> error $ show e ++ ": " ++ show originalMarkdown
            Right      pandocClean ->
              do void $ checkWP pandocClean
                 let clean = runPure $ writeHtml5String safeHtmlWriterOptions pandocClean
                 let html = case clean of
                       Left       e -> error $ show e ++ ": " ++ show originalMarkdown
                       Right output -> cleanAbstractsHTML $ T.unpack output
                 putStrLn html

                 -- guarantee we are in the right working directory to read the various metadata databases for extracting recommendations from:
                 C.cd

                 matchList <- GS.singleShotRecommendations html
                 unless (matchList == "") $ putStrLn $ "<div class=\"aux-links-append see-also-append collapse\">\n\n<p><strong>See Also</strong>:</p>\n\n" ++
                   (replace "'" "’" $ sed "'(.*)'" "‘\\1’" $ sed "<span>(.*)</span>" "\\1" $
                     T.unpack matchList) ++
                   "\n</div>"

-- Quickly validate Wikipedia links in a newly-processed annotation, to avoid finding out much later that an assumed article or redirect didn't exist:
checkWP :: Pandoc -> IO ()
checkWP p = do let links = filter ("wikipedia.org"`T.isInfixOf`) $ extractURLs p
               mapM_ isWPArticle links
