-- Detect cycles in graphs (such as lists of rewrite rules)
module Cycle where

import Data.Graph (flattenSCC, stronglyConnComp)

import Config.Misc as C (cycleTestCases)
import Utils (fixedPoint)

-- simple test for infinite loops in infix string rewrites: we take the list of before→after rewrites, and we try to rewrite the 'before'
-- using some given function. If it infinite-loops...
testInfixRewriteLoops :: (Show a, Eq a) => [(a,a)] -> (a -> a) -> [(a,a,a)]
testInfixRewriteLoops rewrites f = map (\(a,b) -> (a,b,fixedPoint f a)) $ reverse rewrites

isCycleLess :: (Eq a, Ord a, Show a) => [(a,a)] -> [(a,a)]
isCycleLess xs = if not (cycleExists xs) then xs else
  error $ "Error: Association list of rewrite-rules has cycles! Errors related to:" ++ (show $ findCycles xs)

cycleExists :: Ord a => [(a, a)] -> Bool
cycleExists tuples = any (uncurry (==)) tuples ||
    -- There's a cycle if one of the strongly connected components has more than one node
    any ((> 1) . length . flattenSCC)
       -- Generate strongly connected components from edges
       (stronglyConnComp $
        -- Create edges by converting a tuple (a, b) to (a, b, [b]) to reflect a -> b
        map (\(a, b) -> (a, a, [b])) tuples)

-- *Which* rewrite rules are responsible for an infinite loop?
-- Here's one way to find bad nodes easily (albeit inefficiently):
-- start with the list of rewrites and two empty temporary lists;
-- from the rewrite list, take & incrementally add rules to the first list if they do not create a cycle in the first list;
-- if they do, add them to the second list instead (otherwise ignoring the second list);
-- when all rules are used up, return the second list. Those are the bad rules.
findCycles :: Ord a => [(a, a)] -> [(a, a)]
findCycles xs = snd $ foldl f ([], []) xs
  where
    f (good, bad) rule
      | cycleExists (rule : good) = (good, rule : bad)
      | otherwise = (rule : good, bad)

-- `cycleExists` testsuite:
testCycleExists :: [([(Int,Int)], Bool)] -> [[(Int,Int)]]
testCycleExists testCases = [ rules | (rules, expected) <- testCases, cycleExists rules /= expected]
testCycleDetection :: [[(Int,Int)]]
testCycleDetection = testCycleExists C.cycleTestCases
