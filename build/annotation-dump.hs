#!/usr/bin/env runghc
-- Print out single-line-formatted annotations for easier grepping

import LinkMetadata (authorsToCite, sortItemPathDate, readYamlFast, MetadataItem)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import Data.List.Utils (replace)
import Data.Map as M (union, toList, fromList)

type Path = String

main :: IO ()
main = do custom  <- readYamlFast "/home/gwern/wiki/metadata/custom.yaml"  -- for hand created definitions, to be saved; since it's handwritten and we need line errors, we use YAML:
          partial <- readYamlFast "/home/gwern/wiki/metadata/partial.yaml" -- tagged but not handwritten/cleaned-up
          auto    <- readYamlFast "/home/gwern/wiki/metadata/auto.yaml"    -- auto-generated cached definitions; can be deleted if gone stale
          let final = sortItemPathDate $ M.toList $ M.union (M.fromList $ blacklist "c" custom) $ M.union (M.fromList $ blacklist "p" partial) (M.fromList $ blacklist "a" auto)

          mapM_ printSingleLine final

blacklist :: String -> [(Path,MetadataItem)] -> [(String,(MetadataItem,String))]
blacklist sourceLabel = map (\(a,b) -> (a,(b,sourceLabel))) . filter (\(f,(title,_,_,_,_,_)) -> not (title=="" ||
                                                                                                  "en.wikipedia.org" `isInfixOf` f ||
                                                                                                  ("/docs/"`isPrefixOf`f && "/index" `isSuffixOf` f)))

printSingleLine :: (Path,(MetadataItem,String)) -> IO ()
printSingleLine (f,((b,c,d,_,tags,abst),label)) = putStrLn $ ("\x1b[36m"++label++"\x1b[0m: ") ++ intercalate "; "
  [ authorsToCite f c d,
    "\x1b[32m"++f++"\x1b[0m ",
    "\x1b[35m\""++b++"\"\x1b[0m",
    " (" ++ c ++ ")",
    d,
    show tags,
    replace "\n" " " abst]
