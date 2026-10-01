-- Indexed matching for app/redirectGuesser.hs. Distances count Haskell Chars,
-- preserving the original edit-distance package's Unicode semantics.
module RedirectGuesser (RedirectIndex, buildIndex, bestMatch, redirectGuesserTest) where

import Control.DeepSeq (NFData(rnf))
import Control.Monad (replicateM)
import qualified Data.IntMap.Strict as IM (IntMap, findWithDefault, fromListWith)
import Data.List (foldl')
import qualified Data.Map.Strict as M (Map, fromListWith, lookup, toList)
import System.FilePath (takeFileName)

import Text.EditDistance (defaultEditCosts, levenshteinDistance)

data RedirectIndex = RedirectIndex !(M.Map FilePath FilePath) !(M.Map String String) !(IM.IntMap [(String,String)])

instance NFData RedirectIndex where
  rnf (RedirectIndex files exact buckets) = rnf files `seq` rnf exact `seq` rnf buckets

-- The file lookup must retain the first traversal result. Redirect ties instead
-- use the lexicographically smallest target, as in sorting (distance,source,target).
buildIndex :: [(FilePath,FilePath)] -> [(String,String)] -> RedirectIndex
buildIndex files redirects = RedirectIndex fileMap exact buckets
  where
    fileMap = M.fromListWith (\_ old -> old) files
    exact = M.fromListWith min redirects
    buckets = IM.fromListWith (++) [(length source, [(source,target)]) | (source,target) <- M.toList exact]

bestMatch :: RedirectIndex -> String -> Maybe (Int,String,String)
bestMatch (RedirectIndex files exact buckets) err =
  case M.lookup (takeFileName err) files of
    Just target -> Just (0, err, target ++ "\";")
    Nothing -> case M.lookup err exact of
      Just target -> Just (0, err, target)
      Nothing -> foldl' search Nothing candidates
  where
    size = length err
    candidates = [(n,source,target) | n <- [size-2 .. size+2]
                                    , (source,target) <- IM.findWithDefault [] n buckets]
    search best (n,source,target) =
      let limit = maybe 2 (\(d,_,_) -> d) best
          distance = distanceWithin limit size err n source
          candidate = (distance,source,target)
      in if distance > limit then best else Just (maybe candidate (min candidate) best)

-- Return the exact distance when it is <= budget, otherwise the sentinel 3.
-- A common prefix can be matched for free. At the first mismatch, the three
-- possible edits exhaust the Levenshtein recurrence. With a fixed budget <= 2,
-- branching has at most 3^2 leaves, while length bounds prune impossible edits.
-- This avoids computing distances beyond the acceptance threshold or building
-- new strings; matching prefixes only traverse existing list tails.
distanceWithin :: Int -> Int -> String -> Int -> String -> Int
distanceWithin budget sizeA a sizeB b
  | abs (sizeA-sizeB) > budget = 3
  | otherwise = case (a,b) of
      ([],_) -> sizeB
      (_,[]) -> sizeA
      (x:xs,y:ys)
        | x == y -> distanceWithin budget (sizeA-1) xs (sizeB-1) ys
        | budget == 0 -> 3
        | otherwise ->
            let lowerBound = max 1 (abs (sizeA-sizeB))
                substitution = 1 + distanceWithin (budget-1) (sizeA-1) xs (sizeB-1) ys
                deletion = 1 + distanceWithin (budget-1) (sizeA-1) xs sizeB b
                insertion = 1 + distanceWithin (budget-1) sizeA a (sizeB-1) ys
            in if substitution == lowerBound then substitution
               else if deletion == lowerBound then deletion
               else min 3 (min substitution (min deletion insertion))

-- testing: exhaustive short-string comparisons against the original library,
-- including all budgets and Unicode; explicit ranking and basename regressions.
redirectGuesserTest :: [String]
redirectGuesserTest = distanceFailures ++ matchFailures
  where
    samples = concatMap (`replicateM` "ab") [0..6] ++
              ["é", "e\x0301", "😀", "a😀b", "a猫b", "/café", "/cafe\x0301"]
    pairs = [(a,b) | a <- samples, b <- samples] ++
            [(replicate n 'a', replicate (n+delta) 'a') | n <- [63,64,65,128], delta <- [-3..3]] ++
            [(replicate 128 'a' ++ "bc" ++ replicate 128 'a', replicate 128 'a' ++ "cb" ++ replicate 128 'a')
            , ("x" ++ replicate 128 'a', replicate 128 'a' ++ "x")
            , (replicate 64 'a' ++ "xyz" ++ replicate 64 'b', replicate 64 'a' ++ "uvw" ++ replicate 64 'b')
            , (replicate 63 'a' ++ "😀", replicate 63 'a' ++ "猫")]
    distanceFailures =
      [ "RedirectGuesser.distanceWithin: " ++ show (budget,a,b,actual,expected)
      | (a,b) <- pairs
      , let reference = levenshteinDistance defaultEditCosts a b
      , budget <- [0..2]
      , let expected = if reference <= budget then reference else 3
      , let actual = distanceWithin budget (length a) a (length b) b
      , actual /= expected]
    files = [("same.pdf","/z/first.pdf"), ("same.pdf","/a/second.pdf")]
    redirects = [("/requested/same.pdf","/redirect\";")
                , ("/cab","/z\";"), ("/cab","/a\";"), ("/bat","/b\";")
                , ("/abc","/abc\";"), ("/a😀b","/emoji\";"), ("","/empty\";")]
    index = buildIndex files redirects
    fixtures = [("/requested/same.pdf", Just (0,"/requested/same.pdf","/z/first.pdf\";"))
               , ("/missing/same.pdf?q", Nothing)
               , ("/cab", Just (0,"/cab","/a\";"))
               , ("/cxb", Just (1,"/cab","/a\";"))
               , ("/cat", Just (1,"/bat","/b\";"))
               , ("/ab", Just (1,"/abc","/abc\";"))
               , ("/axy", Just (2,"/abc","/abc\";"))
               , ("/xyz", Nothing)
               , ("/abc12", Just (2,"/abc","/abc\";"))
               , ("/abc123", Nothing)
               , ("/a猫b", Just (1,"/a😀b","/emoji\";"))
               , ("", Just (0,"","/empty\";"))]
    matchFailures = ["RedirectGuesser.bestMatch: " ++ show (err,actual,expected)
                    | (err,expected) <- fixtures
                    , let actual = bestMatch index err
                    , actual /= expected]
