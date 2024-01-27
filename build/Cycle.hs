-- Detect cycles in graphs (such as lists of rewrite rules)
module Cycle where

import Data.Graph (flattenSCC, stronglyConnComp)

import Utils (fixedPoint)

-- simple test for infinite loops in infix string rewrites: we take the list of before→after rewrites, and we try to rewrite the 'before'
-- using some given function. If it infinite-loops...
testInfixRewriteLoops :: [(String,String)] -> (String -> String) -> [(String,String,String)]
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
testCycleDetection = testCycleExists cycleTestCases
cycleTestCases :: [([(Int, Int)], Bool)]
cycleTestCases = [ ([], False) -- no rules, no cycles
     , ([(1, 2)], False) -- one rule, no cycles
     , ([(1, 1)], True), ([(1, 2), (2, 3), (3, 4), (5, 5)], True), ([(1, 2), (2, 3), (4, 4), (5, 6)], True) -- self loop
     , ([(1, 2), (2, 3), (3, 4)], False) -- rules with no cycles
     , ([(1, 2), (2, 1)], True) -- simple cycle
     , ([(1, 2), (2, 3), (3, 1)], True) -- cycle with more than 2 nodes: where there is a cycle of nodes that all point to one another, but no node points to itself
     , ([(1, 2), (2, 3), (3, 4), (4, 1)], True) -- larger cycle
     , ([(1, 2), (2, 1), (3, 4), (4, 3), (5, 6), (6, 5)], True) -- Multiple disjoint cycles within a larger rule set
     , ([(1, 2), (1, 3), (2, 4), (2, 5), (3, 6), (3, 7)], False)
     , ([(1, 2), (2, 3), (4, 5), (5, 6)], False) -- separate set of rules, no cycles
     , ([(1, 2), (2, 3), (3, 1), (4, 5), (5, 6), (6, 4)], True) -- separate set of rules with cycles
     , ([(1, 2), (2, 3), (3, 2), (4, 5), (5, 4)], True) -- there is a cycle within subset of rules
     , ([(1, 2), (3, 4), (5, 6)], False) -- separate set of rules, no cycles
     , ([(1, 2), (1, 2), (2, 3), (2, 3)], False) -- repetition
     , ([(1, 2), (1, 3), (2, 4), (3, 4)], False) -- Multiple paths to the same node, but no cycles
     , ([(1, 2), (1, 3), (2, 4), (3, 4), (4, 1)], True) -- where there are multiple paths leading to a node that is part of a cycle.
     , ([(1, 1), (2, 2), (3, 3)], True) --where every node in the list points to itself (simple loop for every node)
     ]
