module Main where

import Control.Monad (void)

import LinkMetadata (readLinkMetadataAndCheck)

main :: IO ()
main = void readLinkMetadataAndCheck
