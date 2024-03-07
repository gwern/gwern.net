#!/usr/bin/env runghc
{-# LANGUAGE OverloadedStrings #-}

-- Parallel string literal search-and-replace.
--
-- This is exact: it is case-sensitive, handles all special characters like '#' or '&' (as long as they were passed into the program unmangled), does not do regexps or normalization or anything. (This turns out to be harder than one would expect in sed/Bash/perl/etc unless you drop into a real programming language—in which case might as well compile & parallelize it for efficiency.)
--
-- If 3 arguments are specified, the third is the file; if only 2, then filenames are read in newline-delimited from stdin. Directories are ignored.
--
-- $ find . -name "*.txt" | ./stringReplace.hs 'foo' 'bar'
-- $ ./stringReplace.hs 'foo' 'bar' a.txt
--
-- For greater speed, compile & parallelize.

module Main where

import Control.Monad (when)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)

import Data.Containers.ListUtils (nubOrd)
import qualified Data.Text.IO as TIO (readFile, writeFile)
import qualified Data.Text as T (pack, replace, Text)

import Control.Concurrent (getNumCapabilities, setNumCapabilities)
import Control.Monad.Parallel as Par (mapM_)

main :: IO ()
main = do (original:new:file) <- getArgs
          files <- if null file then fmap (nubOrd . lines) getContents else return file
          cores <- getNumCapabilities
          setNumCapabilities ((cores - 1) `max` 1)
          Par.mapM_ (replace (T.pack original) (T.pack new)) files

replace :: T.Text -> T.Text -> FilePath -> IO ()
replace "" _ _ = error "First argument to `replace` was an empty string, which is meaningless."
replace _ _ "" = error "Third argument to `replace`, the filename to rewrite, was an empty string, which is meaningless."
replace o n f  = do when (o == n) $ error $ "First & second arguments to `replace` were identical and no rewrite can be done: " ++ show o
                    isFile <- doesFileExist f
                    if not isFile then
                      do isDirectory <- doesDirectoryExist f
                         if isDirectory then return () -- we silently skip directories for convenience in shell operations like `stringReplace foo bar *`.
                           else error $ "Attempted to run `replace` on a file which doesn’t exist (and isn’t a directory either)! Filename: " ++ f
                    else do old <- TIO.readFile f
                            let new = T.replace o n old
                            when (old /= new) $ TIO.writeFile f new
