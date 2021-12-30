#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Containers.ListUtils (nubOrd)
import Data.List (sort)
import qualified Control.Monad.Parallel as Par (mapM, mapM_)

import GenerateSimilar (bestNEmbeddings, embed, embeddings2Forest, findN, missingEmbeddings, readEmbeddings, writeEmbeddings, writeOutMatch)
import LinkMetadata (readLinkMetadata)
import Utils (printGreen)

main :: IO ()
main = do md  <- readLinkMetadata
          edb <- readEmbeddings
          printGreen "Read databases."

          -- update for any missing embeddings, and return updated DB for computing distances & writing out fragments:
          let todo = sort $ missingEmbeddings md edb
          edb'' <- if null todo then printGreen "All databases up to date." >> return edb else
                     do
                       printGreen $ "Embedding…\n" ++ unlines (map show todo)
                       newEmbeddings <- Par.mapM embed todo
                       printGreen "Generated embeddings."
                       let edb' = nubOrd (edb ++ newEmbeddings)
                       writeEmbeddings edb'
                       printGreen "Wrote embeddings."
                       return edb'

          -- rp-tree supports serializing the tree to disk, but unclear how to update it, and it's fast enough to construct that it's not a bottleneck, so we recompute it from the embeddings every time.
          let ddb  = embeddings2Forest edb''
          printGreen "Begin computing & writing out similarity-rankings…"
          Par.mapM_ (writeOutMatch md . findN ddb bestNEmbeddings) edb''
          printGreen "Done."
