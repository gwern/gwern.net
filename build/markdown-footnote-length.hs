#! /usr/bin/env runhaskell
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE OverloadedStrings #-}
-- example usage: $ find ~/wiki/ -name "*.page" -exec runhaskell markdown-footnote-length {} \;
-- default: looks for footnotes >2400 characters long (too long on my screen)
import System.Environment (getArgs)
import Text.Pandoc (bottomUpM, def, nullMeta, readMarkdown, pandocExtensions, writePlain,
                    readerExtensions, runPure, Inline(Note), Pandoc(..))
import Control.Monad (void, when)
import Data.Text as Text (drop, length, unpack, Text)
import Data.Text.IO as TIO (readFile)

main :: IO ()
main = do (file:_) <- getArgs
          void (TIO.readFile file >>= processLint file)

-- 'drop 3' to avoid the near-infinite loop when files start with Hakyll metadata
processLint :: FilePath -> Text.Text -> IO Pandoc
processLint f x = do let parsed = runPure $ readMarkdown def{readerExtensions=pandocExtensions} (Text.drop 3 x)
                     case parsed of
                      Right x' -> bottomUpM (footNoteCheck f) x'
                      Left _ -> error ("Could not parse: "++f)

footNoteCheck :: FilePath -> Inline -> IO Inline
footNoteCheck f x@(Note cntnts) = do let md = runPure $ writePlain def (Pandoc nullMeta cntnts)
                                     case md of
                                       Left _ -> return x
                                       Right md' -> do when (Text.length md' > 2400) $ error (f ++ ": " ++ Text.unpack md')
                                                       return x
footNoteCheck _ x = return x
