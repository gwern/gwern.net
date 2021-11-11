module Main where

import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as TIO (getContents)

import Text.Pandoc (def, pandocExtensions, runPure, readerExtensions, readMarkdown, writeHtml5String)
import Text.Pandoc.Walk (walk)

import LinkMetadata (cleanAbstractsHTML, safeHtmlWriterOptions)
import LinkAuto (linkAuto)
import Interwiki (convertInterwikiLinks)

main :: IO ()
main = do originalMarkdown <- TIO.getContents
          let clean = runPure $ do
                pandoc <- readMarkdown def{readerExtensions=pandocExtensions} originalMarkdown
                let pandoc' = linkAuto . walk convertInterwikiLinks $ pandoc
                writeHtml5String safeHtmlWriterOptions pandoc'
          case clean of
                 Left e -> error $ show e ++ ": " ++ show originalMarkdown
                 Right output -> putStrLn $ cleanAbstractsHTML $ T.unpack output
