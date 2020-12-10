#! /usr/bin/env runhaskell
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE OverloadedStrings #-}
-- example usage: $ find ~/wiki/ -name "*.page" -exec runhaskell markdown-footnote-length {} \;
-- default: looks for footnotes >2400 characters long (too long on my screen)
import System.Environment (getArgs)
import Text.Pandoc (bottomUpM, def, nullMeta, readMarkdown, pandocExtensions, writePlain,
                    readerExtensions, runPure, Inline(Note), Pandoc(..))
import Control.Monad (void, when)
import qualified Data.Text as Text (length, unpack, Text)
import qualified Data.Text.IO as TIO (readFile, putStrLn)

main :: IO ()
main = do (file:_) <- getArgs
          void (TIO.readFile file >>= processLint file)

processLint :: FilePath -> Text.Text -> IO Pandoc
processLint f x = do let parsed = runPure $ readMarkdown def{readerExtensions=pandocExtensions} x
                     case parsed of
                      Right x' -> bottomUpM (footNoteCheck f) x'
                      Left _ -> error ("Could not parse: "++f)

footNoteCheck :: FilePath -> Inline -> IO Inline
footNoteCheck f x@(Note cntnts) = do let md = runPure $ writePlain def (Pandoc nullMeta cntnts)
                                     case md of
                                       Left _ -> return x
                                       Right md' -> do when (Text.length md' > 2400) $ putStrLn (f ++ ": " ++ Text.unpack md')
                                                       return x
footNoteCheck _ x = return x
