{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as TIO (getContents)
import System.Directory (setCurrentDirectory)

import Text.Pandoc (def, pandocExtensions, runPure, readerExtensions, readMarkdown, writeHtml5String)

import LinkMetadata (cleanAbstractsHTML)
import LinkAuto (linkAuto)
import Interwiki (convertInterwikiLinks)
import qualified GenerateSimilar as GS (singleShotRecommendations)
import Utils (safeHtmlWriterOptions)

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
          setCurrentDirectory "/home/gwern/wiki/"

          matchList <- GS.singleShotRecommendations html
          unless (matchList == "") $ putStrLn $ "<div class=\"collapse aux-links-container\">\n<div class=\"aux-links-append see-also-append\">\n\n<p><strong>See Also</strong>:</p>\n\n" ++ T.unpack matchList ++ "\n</div>\n</div>"
