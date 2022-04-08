#!/usr/bin/env runhaskell
-- Print out single-line-formatted annotations for easier grepping

import LinkMetadata (authorsToCite, sortItemPathDate, readYamlFast, MetadataItem)
import Data.List (intercalate, isInfixOf)
import Data.List.Utils (replace)
import Data.Map as M (union, toList, fromList)

type Path = String

main :: IO ()
main = do custom  <- readYamlFast "/home/gwern/wiki/metadata/custom.yaml"  -- for hand created definitions, to be saved; since it's handwritten and we need line errors, we use YAML:
          partial <- readYamlFast "/home/gwern/wiki/metadata/partial.yaml" -- tagged but not handwritten/cleaned-up
          auto    <- readYamlFast "/home/gwern/wiki/metadata/auto.yaml"    -- auto-generated cached definitions; can be deleted if gone stale
          let final = sortItemPathDate $ M.toList $ M.union (M.fromList $ blacklist custom) $ M.union (M.fromList $ blacklist partial) (M.fromList $ blacklist auto)

          mapM_ printSingleLine final

blacklist :: [(Path,MetadataItem)] -> [(String,MetadataItem)]
blacklist = filter (\(f,(b,_,_,_,_,_)) -> not (b=="" || "en.wikipedia.org" `isInfixOf` f))

printSingleLine :: (Path,MetadataItem) -> IO ()
printSingleLine (f,(b,c,d,_,tags,abst)) = putStrLn $ intercalate "; "
  [" \x1b[32m"++f++"\x1b[0m ",
    "\x1b[35m\""++b++"\"\x1b[0m",
    authorsToCite f c d ++ " (" ++ c ++ ")",
    d,
    show tags,
    replace "\n" " " abst]
