#!/usr/bin/env runghc
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless, when, filterM)
import Data.Containers.ListUtils (nubOrd)
import Data.List (sort)
import Data.List.Split (chunksOf)
import qualified Control.Monad.Parallel as Par (mapM_)
import System.Environment (getArgs)
import Data.Map.Strict as M (fromList, lookup, keys, filter)

import GenerateSimilar (embed, embeddings2Forest, findN, missingEmbeddings, readEmbeddings, similaritemExistsP, writeEmbeddings, writeOutMatch, pruneEmbeddings, expireMatches, sortSimilars, readListSortedMagic)
import qualified Config.GenerateSimilar as C (bestNEmbeddings, iterationLimit)
import LinkBacklink (readBacklinksDB)
import LinkMetadata (readLinkMetadata)
import Utils (printGreen)
import qualified Config.Misc (cd)

maxEmbedAtOnce :: Int
maxEmbedAtOnce = 750

main :: IO ()
main = do Config.Misc.cd
          md  <- readLinkMetadata
          let mdl = sort $ M.keys $ M.filter (\(_,_,_,_,_,_,abst) -> abst /= "") md -- to iterate over the annotation database's URLs, and skip outdated URLs still in the embedding database
          mdlMissing <- filterM (fmap not . similaritemExistsP) mdl
          bdb <- readBacklinksDB
          edb <- readEmbeddings
          let edbDB = M.fromList $ map (\(a,b,c,d,e) -> (a,(b,c,d,e))) edb
          printGreen "Read databases."

          -- update for any missing embeddings, and return updated DB for computing distances & writing out fragments:
          let todo = take maxEmbedAtOnce $ sort $ missingEmbeddings md edb
          let todoLinks = map fst todo -- just the paths
          edb'' <- if null todo then printGreen "All databases up to date." >> return edb else
                     do
                       printGreen $ "Embedding…\n" ++ unlines (map show todo)
                       newEmbeddings <- mapM (embed edb md bdb) todo
                       printGreen "Generated embeddings."
                       let edb' = nubOrd (edb ++ newEmbeddings)
                       -- clean up by removing any outdated embeddings whose path/URL no longer corresponds to any annotations (typically because renamed):
                       let edb'' = pruneEmbeddings md edb'
                       writeEmbeddings edb''
                       printGreen "Wrote embeddings."
                       return edb''

          -- if we are only updating the embeddings, then we stop there and do nothing more. (This
          -- is useful for using `inotifywait` (from the `inotifytools` Debian package) to 'watch' the
          -- GTX databases for new entries, and immediately embed them then & there, so
          -- `preprocess-markdown.hs`'s single-shot mode gets updated quickly with recently-written
          -- annotations, instead of always waiting for the nightly rebuild. When doing batches of
          -- new annotations, they are usually all relevant to each other, but won't appear in the
          -- suggested-links.)
          --
          -- eg. in a crontab, this would work:
          -- $ `@reboot screen -d -m -S "embed" bash -c 'cd ~/wiki/; while true; do inotifywait ~/wiki/metadata/*.gtx -e attrib && sleep 10s && date && runghc -istatic/build/ ./static/build/generateSimilarLinks.hs --only-embed; done'`
          --
          -- [ie.: 'at boot, start a background daemon which monitors the annotation files and
          -- whenever one is modified, kill the monitor, wait 10s, and check for new annotations to
          -- embed & save; if nothing, exit & restart the monitoring.']
          args <- getArgs
          -- Otherwise, we keep going & compute all the suggestions.
          -- rp-tree supports serializing the tree to disk, but unclear how to update it, and it's fast enough to construct that it's not a bottleneck, so we recompute it from the embeddings every time.
          ddb <- embeddings2Forest edb''
          sortDB <- readListSortedMagic
          unless (args == ["--only-embed"]) $ do

              printGreen $ "Begin computing & writing out " ++ show (length mdlMissing) ++ " missing similarity-rankings…"
              let mdlMissingChunks = chunksOf 15 mdlMissing
              mapM_
                    (Par.mapM_ (\f ->       case M.lookup f edbDB of
                                                 Nothing        -> return ()
                                                 Just (b,c,d,e) -> do let (path,hits) = findN ddb C.bestNEmbeddings C.iterationLimit Nothing (f,b,c,d,e)
                                                                      -- rerank the _n_ matches to put them into a more internally-coherent ordering by pairwise distance minimization, rather than merely minimizing distance to the target URL:
                                                                      hitsSorted <- sortSimilars edb sortDB (head hits) hits
                                                                      let nmatchesSorted = (path, hitsSorted)
                                                                      when (f `elem` todoLinks) $ expireMatches (snd nmatchesSorted)
                                                                      putStrLn $ "gSL.nmatchesSorted: " ++ show nmatchesSorted
                                                                      writeOutMatch md bdb nmatchesSorted
                        )) mdlMissingChunks

              printGreen "Wrote out missing."
              unless (args == ["--update-only-missing-embeddings"]) $ do
                let chunkSize = 550
                let mdlChunks = chunksOf chunkSize mdlMissing
                printGreen "Rewriting all embeddings…"
                printGreen "Rewriting all edb''…"
                Par.mapM_ (writeOutMatch md bdb . findN ddb C.bestNEmbeddings C.iterationLimit Nothing) edb''
                printGreen "Rewriting all mdlChunks…"
                Par.mapM_ (mapM_ (\f ->
                                    case M.lookup f edbDB of
                                       Nothing        -> return ()
                                       Just (b,c,d,e) -> do let (path,hits) = findN ddb C.bestNEmbeddings C.iterationLimit Nothing (f,b,c,d,e)
                                                            hitsSorted <- sortSimilars edb sortDB (head hits) hits
                                                            let nmatchesSorted = (path, hitsSorted)
                                                            writeOutMatch md bdb nmatchesSorted
                                      ))
                  mdlChunks
                printGreen "Done."
