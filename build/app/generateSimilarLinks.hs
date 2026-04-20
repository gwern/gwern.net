#!/usr/bin/env runghc
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (filterM, when)
import Data.Containers.ListUtils (nubOrd)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as M (elems, filter, fromList, lookup, toList)
import qualified Data.Set as S (empty, fromList, member, notMember, Set)
import System.Environment (getArgs)


import LinkBacklink (Backlinks, readBacklinksDB)
import LinkMetadata (readLinkMetadata, sortItemPathDateModified)
import LinkMetadataTypes (Metadata, MetadataItem)
import Utils (printGreen)

import qualified GenerateSimilar as GS (Embeddings, EmbeddingIndex, embed, expireMatches, embeddings2Index, lookupPathK, missingEmbeddings, pruneEmbeddings, readEmbeddings, seriateGreedy, similaritemExistsP, stripEmbedding, writeEmbeddings, writeOutMatch)
import qualified Config.GenerateSimilar as C (bestNEmbeddings, maxDistance, maxEmbedAtOnce)
import qualified Config.Misc as CM (cd)

data Mode = MissingOnly | EmbedOnly | RewriteAll deriving (Eq, Show)

parseMode :: [String] -> Mode
parseMode [] = MissingOnly
parseMode ["--update-only-missing-embeddings"] = MissingOnly
parseMode ["--only-embed"] = EmbedOnly
parseMode ["--all"] = RewriteAll
parseMode ["--rewrite-all"] = RewriteAll
parseMode args =
  error $ "generateSimilarLinks: unrecognized arguments: " ++ show args ++
          "; supported modes are [], --update-only-missing-embeddings, --only-embed, --all, --rewrite-all"

main :: IO ()
main = do
  CM.cd

  args <- getArgs
  let !mode = parseMode args

  md <- readLinkMetadata
  let annotatedPaths = annotatedMetadataPaths md

  bdb <- readBacklinksDB
  edb <- GS.readEmbeddings
  printGreen $ "Read databases. Mode: " ++ show mode

  let todo = take C.maxEmbedAtOnce $ missingEmbeddingsByRecency md edb annotatedPaths
      todoLinks = map fst todo
      newlyEmbedded = S.fromList todoLinks

  edbUpdated <- updateEmbeddings md bdb edb todo
  let compactEmbeddings = map GS.stripEmbedding $ GS.pruneEmbeddings md edbUpdated
      embeddedPaths = embeddingPathSet compactEmbeddings

  case mode of
    EmbedOnly ->
      printGreen "Updated embeddings only."

    MissingOnly -> do
      let !ix = GS.embeddings2Index compactEmbeddings
      pathsMissingSimilar <- filterM (fmap not . GS.similaritemExistsP) annotatedPaths

      let pathsRequested = nubOrd (todoLinks ++ pathsMissingSimilar)
          pathsToWrite = filter (`S.member` embeddedPaths) pathsRequested
          pathsToWriteSet = S.fromList pathsToWrite
          skipped = length pathsRequested - length pathsToWrite

      printGreen $
        "Begin computing & writing out " ++ show (length pathsToWrite) ++
        " missing or newly-embedded similarity-rankings" ++
        (if skipped == 0 then "" else " (skipping " ++ show skipped ++ " without embeddings)") ++
        "…"

      expired <- fmap concat $
        mapM (writeSimilarForPath md bdb ix newlyEmbedded pathsToWriteSet) pathsToWrite

      let reciprocalPaths = nubOrd $
            filter (\p -> p `S.member` embeddedPaths && p `S.notMember` pathsToWriteSet) expired

      when (not $ null reciprocalPaths) $
        printGreen $ "Rewriting " ++ show (length reciprocalPaths) ++
                     " reciprocal similarity-rankings expired by new embeddings…"

      mapM_ (writeSimilarForPath_ md bdb ix S.empty) reciprocalPaths
      printGreen "Done."

    RewriteAll -> do
      let !ix = GS.embeddings2Index compactEmbeddings
          targets = filter (`S.member` embeddedPaths) annotatedPaths

      printGreen $
        "Rewriting all similarity-rankings with exact linear-scan lookup: " ++ show (length targets)

      mapM_ (writeSimilarForPath_ md bdb ix S.empty) targets
      printGreen "Done."

annotatedMetadataPaths :: Metadata -> [FilePath]
annotatedMetadataPaths md =
  filter (not . indexPath) $
  map fst $
  reverse $
  sortItemPathDateModified $
  M.toList $
  M.filter (\(_, _, _, _, _, _, abst) -> abst /= "") md
 where
  indexPath :: FilePath -> Bool
  indexPath f = not (null f) && head f == '/' && "/index" `isSuffixOf` f

missingEmbeddingsByRecency :: Metadata -> GS.Embeddings -> [FilePath] -> [(FilePath, MetadataItem)]
missingEmbeddingsByRecency md edb annotatedPaths =
  let missingByPath = M.fromList $ GS.missingEmbeddings md edb
  in [ (p, mi)
     | p <- annotatedPaths
     , Just mi <- [M.lookup p missingByPath]
     ]

updateEmbeddings :: Metadata -> Backlinks -> GS.Embeddings -> [(FilePath, MetadataItem)] -> IO GS.Embeddings
updateEmbeddings _ _ edb [] = do
  printGreen "All embeddings up to date."
  return edb
updateEmbeddings md bdb edb todo = do
  printGreen $ "Embedding…\n" ++ unlines (map show todo)
  newEmbeddings <- mapM (GS.embed edb md bdb) todo
  printGreen "Generated embeddings."

  let edbMerged = mergeEmbeddingsByPath $ edb ++ newEmbeddings
      edbPruned = GS.pruneEmbeddings md edbMerged

  GS.writeEmbeddings edbPruned
  printGreen "Wrote embeddings."
  return edbPruned

mergeEmbeddingsByPath :: GS.Embeddings -> GS.Embeddings
mergeEmbeddingsByPath es =
  M.elems $ M.fromList [(p, e) | e@(p, _, _, _, _) <- es]

embeddingPathSet :: GS.Embeddings -> S.Set FilePath
embeddingPathSet es =
  S.fromList [p | (p, _, _, _, vec) <- es, p /= "", not (null vec)]

writeSimilarForPath_ :: Metadata -> Backlinks -> GS.EmbeddingIndex -> S.Set FilePath -> FilePath -> IO ()
writeSimilarForPath_ md bdb ix newlyEmbedded path = do
  _ <- writeSimilarForPath md bdb ix newlyEmbedded S.empty path
  return ()

writeSimilarForPath :: Metadata -> Backlinks -> GS.EmbeddingIndex -> S.Set FilePath -> S.Set FilePath -> FilePath -> IO [FilePath]
writeSimilarForPath md bdb ix newlyEmbedded protectedTargets path =
  case similarMatches ix path of
    Nothing ->
      return []

    Just nmatches@(_, hitsSorted) -> do
      expired <-
        if path `S.member` newlyEmbedded
          then do
            let reciprocalHits = filter (`S.notMember` protectedTargets) hitsSorted
            expireExistingMatches reciprocalHits
            return reciprocalHits
          else return []

      putStrLn $ "gSL.nmatchesSorted: " ++ show nmatches
      GS.writeOutMatch md bdb nmatches
      return expired

expireExistingMatches :: [FilePath] -> IO ()
expireExistingMatches paths = do
  existing <- filterM GS.similaritemExistsP paths
  GS.expireMatches existing

similarMatches :: GS.EmbeddingIndex -> FilePath -> Maybe (FilePath, [FilePath])
similarMatches ix path =
  let hits =
        map fst $
        filter ((< C.maxDistance) . snd) $
        GS.lookupPathK ix C.bestNEmbeddings path

      hitsSorted =
        case hits of
          []    -> []
          h : _ -> GS.seriateGreedy ix hits h
  in if null hitsSorted
       then Nothing
       else Just (path, hitsSorted)

