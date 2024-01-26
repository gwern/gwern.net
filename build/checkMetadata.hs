module Main where

import LinkMetadata (readLinkMetadataAndCheck)

main :: IO ()
main = readLinkMetadataAndCheck >> return ()
