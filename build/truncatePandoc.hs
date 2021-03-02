#!/usr/bin/env runhaskell
-- dependencies: libghc-pandoc-dev

-- usage: './truncatePandoc.hs n'; parses HTML5 input into Pandoc Markdown AST, takes the first _n_ block-level elements, appends an ellipsis to denote truncation if truncation was necessary, and compiles back to HTML5

module Main where

import Text.Pandoc (readerExtensions, writerExtensions, Pandoc(Pandoc), pandocExtensions, def, runPure, nullMeta, readHtml, writeHtml5String, Block(Para), Inline(Str))
import System.Environment (getArgs)
import System.IO (getContents)
import qualified Data.Text as T (pack)
import qualified Data.Text.IO as TIO (putStrLn)
import Debug.Trace (trace)

main :: IO ()
main = do trunc:_ <- getArgs
          let truncationLength = read trunc :: Int
          doc <- getContents

          let (Pandoc m blocks) = let pandocMaybe = runPure $ readHtml def{readerExtensions = pandocExtensions} (T.pack doc) in
                                     case pandocMaybe of
                                       Left err -> Debug.Trace.trace (show err) (Pandoc nullMeta [])
                                       Right pandoc -> pandoc

          let blocks' = take truncationLength blocks ++ if length blocks > truncationLength then [Para [Str $ T.pack "…"]] else []

          let docEither =  runPure $ writeHtml5String def{writerExtensions = pandocExtensions} (Pandoc m blocks')
          case docEither of
            Left e -> error $ show e ++ show blocks'
            Right doc' -> TIO.putStrLn doc'
