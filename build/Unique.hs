-- Config checking: checking for various kinds of uniqueness/duplications.
-- Enable additional runtime checks to very long config lists which risk error from overlap or redundancy. Prints out the duplicates.
-- (Since the config lists are static, they can in theory be checked at compile-time, but my attempt to do that with Template Haskell
-- for XMonad keymap configs many years ago ran into a lot of pain, so I won't bother even trying again.)

module Unique where

import Data.List (foldl')
import qualified Data.Set as Set (empty, insert, member)

-- Helper function to check uniqueness & report the offending list:
-- Optimized helper function to get duplicates
getDuplicates :: Ord a => [a] -> [a]
getDuplicates = snd . foldl' go (Set.empty, [])
  where
    go (seen, duplicates) x
      | x `Set.member` seen = (seen, x : duplicates)
      | otherwise = (Set.insert x seen, duplicates)
throwError :: Show a => String -> [a] -> b
throwError msg xs = error $ "Error: " ++ msg ++ " " ++ show xs
checkUniqueOrThrow :: (Eq a, Ord a, Show a) => String -> [a] -> [a]
checkUniqueOrThrow msg xs
  | null duplicates = xs
  | otherwise = throwError msg duplicates
  where duplicates = getDuplicates xs

-- 0. check a simple list for uniqueness in the only way possible:
isUniqueList :: (Eq a, Ord a, Show a) => [a] -> [a]
isUniqueList = checkUniqueOrThrow "Simple list contains duplicates:"

-- Association-list checks:
-- 1. isUnique: all key-value pairs are unique and there are no duplicates
isUnique :: (Eq a, Show a, Eq b, Ord a, Ord b, Show b) => [(a,b)] -> [(a,b)]
isUnique = checkUniqueOrThrow "Unique.isUnique: Association List contains duplicate key-value pairs:"

-- 2. isUniqueKeys: all keys are unique and there are no duplicates
isUniqueKeys :: (Eq a, Ord a, Show a, Show b) => [(a,b)] -> [(a,b)]
isUniqueKeys xs
  | null duplicates = xs
  | otherwise = throwError "Unique.isUniqueKeys: Association List contains duplicate keys:" duplicates
  where duplicates = getDuplicates (map fst xs)
-- special-case:
isUniqueKeys3 :: (Eq a, Ord a, Show a) => [(a,b,c)] -> [(a,b,c)]
isUniqueKeys3 xs
  | null duplicates = xs
  | otherwise = throwError "Unique.isUniqueKeys3: Association List contains duplicate keys (ie. 'a' of '(a,b,c)'):" duplicates
  where duplicates = getDuplicates (map (\(a,_,_) -> a) xs)
isUniqueKeys4 :: (Eq a, Ord a, Show a) => [(a,b,c,d)] -> [(a,b,c,d)]
isUniqueKeys4 xs
  | null duplicates = xs
  | otherwise = throwError "Unique.isUniqueKeys4: Association List contains duplicate keys (ie. 'a' of '(a,b,c,d)'):" duplicates
  where duplicates = getDuplicates (map (\(a,_,_,_) -> a) xs)

isUniqueMiddle3 :: (Eq a, Ord a, Ord b, Show b, Show a) => [(a,b,c)] -> [(a,b,c)]
isUniqueMiddle3 xs
  | null duplicates = xs
  | otherwise = throwError "Unique.isUniqueMiddle3: Association List contains duplicate middle-keys (ie. 'b' of '(a,b,c)'):" duplicates
  where duplicates = getDuplicates (map (\(_,b,_) -> b) xs)

-- 3. isUniqueValues: all values are unique and there are no duplicates
isUniqueValues :: (Show a, Ord a, Eq b, Ord b, Show b) => [(a,b)] -> [(a,b)]
isUniqueValues xs
  | null duplicates = xs
  | otherwise = throwError "Unique.isUniqueValues: Association List contains duplicate values:" duplicates
  where duplicates = getDuplicates (map snd xs)

-- 4. isUniqueAll: all keys, values, and key-value pairs are unique
isUniqueAll :: (Eq a, Ord a, Show a, Eq b, Ord b, Show b) => [(a,b)] -> [(a,b)]
isUniqueAll xs = isUniqueValues $ isUniqueKeys $ isUnique xs
