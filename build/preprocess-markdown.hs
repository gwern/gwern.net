{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as TIO (getContents)

import Text.Pandoc (def, pandocExtensions, runPure, readerExtensions, readMarkdown, writeHtml5String)

import Config.Misc as C (cd)
import LinkMetadata (cleanAbstractsHTML)
import LinkAuto (linkAuto)
import Interwiki (convertInterwikiLinks)
import qualified GenerateSimilar as GS (singleShotRecommendations)
import Utils (replace, safeHtmlWriterOptions, sed)

main :: IO ()
main = do originalMarkdown <- TIO.getContents

          let clean = runPure $ do
                pandoc <- readMarkdown def{readerExtensions=pandocExtensions} originalMarkdown
                let pandoc' = convertInterwikiLinks pandoc
                let pandoc'' = linkAuto pandoc'
                writeHtml5String safeHtmlWriterOptions pandoc''
          let html = case clean of
                 Left e -> error $ show e ++ ": " ++ show originalMarkdown
                 Right output -> cleanAbstractsHTML $ T.unpack output
          putStrLn html

          -- guarantee we are in the right working directory to read the various metadata databases for extracting recommendations from:
          C.cd

          matchList <- GS.singleShotRecommendations html
          unless (matchList == "") $ putStrLn $ "<div class=\"aux-links-append see-also-append collapse\">\n\n<p><strong>See Also</strong>:</p>\n\n" ++ (replace "'" "’" $ sed "'(.*)'" "‘\\1’" $ sed "<span>(.*)</span>" "\\1" $ T.unpack matchList) ++ "\n</div>"
