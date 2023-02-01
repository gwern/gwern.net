#!/usr/bin/env runghc
-- Print out single-line-formatted annotations for easier grepping.
-- If arguments or stdin are provided, they are read as URLs/paths to query for; their annotation (if non-empty) is provided, as well as any other annotation which contains them as a substring.
-- If an argument has an empty annotation, it is printed out as well, as a separate line. (This enables use-cases like parsing all the links out of a file using link-extracter.hs and passing it into annotation-dump.hs to see which ones do not so much as have a tag, so a tag can be added.)

import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub)
import Data.Map as M (lookup, union, toList, fromList)
import Data.Text as T (unpack)
import Data.Text.IO as TIO (getContents)

import LinkID (authorsToCite, generateURL)
import LinkMetadata (authorsTruncate, sortItemPathDate, readYamlFast)
import LinkMetadataTypes (MetadataItem)
import Tags (uniqTags)
import Utils (anyInfix, replace, sed)

type Path = String

main :: IO ()
main = do custom  <- readYamlFast "/home/gwern/wiki/metadata/full.yaml"  -- for hand created definitions, to be saved; since it's handwritten and we need line errors, we use YAML:
          half <- readYamlFast "/home/gwern/wiki/metadata/half.yaml" -- tagged but not handwritten/cleaned-up
          auto    <- readYamlFast "/home/gwern/wiki/metadata/auto.yaml"    -- auto-generated cached definitions; can be deleted if gone stale
          let incompleteDB = M.union (M.fromList custom) $ M.union (M.fromList half) (M.fromList auto)
          let finalDB = M.union (M.fromList $ blacklist "c" custom) $ M.union (M.fromList $ blacklist "p" half) (M.fromList $ blacklist "a" auto)
          let final = sortItemPathDate $ M.toList finalDB
          let finalSingleLine = map toSingleLine final

          stdin <- fmap (nub . lines . T.unpack) TIO.getContents
          if null stdin then putStrLn (unlines finalSingleLine)
            else do let lookups = map (\p -> case M.lookup p incompleteDB of
                                              Nothing -> toSingleLine (p, (("","","","",[],""),""))
                                              Just a  -> toSingleLine (p,(a,"a"))
                                      ) stdin
                    let hits = filter (`anyInfix` stdin) finalSingleLine
                    putStrLn $ unlines $ hits ++ lookups

blacklist :: String -> [(Path,MetadataItem)] -> [(String,(MetadataItem,String))]
blacklist sourceLabel = map (\(a,b) -> (a,(b,sourceLabel))) . filter (\(f,(title,_,_,_,_,_)) -> not (title=="" ||
                                                                                                  "en.wikipedia.org" `isInfixOf` f ||
                                                                                                  ("/doc/"`isPrefixOf`f && "/index" `isSuffixOf` f)))

toSingleLine :: (Path,(MetadataItem,String)) -> String
toSingleLine ("",_) = ""
toSingleLine (f,(("",_,_,_,[],_),_)) = f ++ " []" -- we insert '[]' to parallel links with barebones auto-metadata but lacking even a tag; this lets us grep output for all untagged links (as opposed to only being able to grep for the smaller & much more arbitrary subset, 'untagged but has an auto-title')
toSingleLine (f,(mi@(b,c,d,_,tags,abst),label)) = intercalate "; "
  ([ label,
     authorsToCite f c d,
    "\x1b[32m "++f++" \x1b[0m",
    show (uniqTags tags),
    "\x1b[35m\""++b++"\"\x1b[0m",
    " (" ++ authorsTruncate c ++ ")",
    d,
    sed " +" " " $ replace "\n" " " abst] ++
    (let url = generateURL f mi in if null url then [] else ["\x1b[32m "++url++"\x1b[0m"])
  )
