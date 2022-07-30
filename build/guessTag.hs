#!/usr/bin/env runghc

import System.Environment (getArgs)

import LinkMetadata (listTagsAll, guessTagFromShort)

main :: IO ()
main = do tags    <- listTagsAll
          (arg:_) <- getArgs
          putStr (guessTagFromShort tags arg)
