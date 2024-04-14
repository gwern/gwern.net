#!/usr/bin/env runghc
-- Print out single-line-formatted annotations for easier grepping.
-- If arguments or stdin are provided, they are read as URLs/paths to query for; their annotation (if non-empty) is provided, as well as any other annotation which contains them as a substring.
-- If an argument has an empty annotation, it is printed out as well, as a separate line. (This enables use-cases like parsing all the links out of a file using link-extracter.hs and passing it into annotation-dump.hs to see which ones do not so much as have a tag, so a tag can be added.)

import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import Data.Containers.ListUtils (nubOrd)
import Data.Map as M (lookup, union, toList, fromList)
import qualified Data.Text as T (unpack)
import Data.Text.IO as TIO (getContents)

import Config.Misc as C (root)
import LinkID (authorsToCite, generateURL)
import LinkMetadata (sortItemPathDate)
import GTX (readGTXSlow)
import LinkMetadataTypes (MetadataItem, MetadataList)
import Utils (anyInfix, replace, sed)
import MetadataAuthor (authorsTruncateString)

type Path = String

main :: IO ()
main = do full <- readGTXSlow (C.root ++ "metadata/full.gtx") -- for hand created definitions, to be saved; since it's handwritten and we need line errors, we use GTX:
          half <- readGTXSlow (C.root ++ "metadata/half.gtx") -- tagged but not handwritten/cleaned-up
          auto <- readGTXSlow (C.root ++ "metadata/auto.gtx") -- auto-generated cached definitions; can be deleted if gone stale
          let incompleteDB = M.union (M.fromList full) $ M.union (M.fromList half) (M.fromList auto)
          let finalDB = M.union (M.fromList $ blacklist "f" full) $ M.union (M.fromList $ blacklist "h" half) (M.fromList $ blacklist "a" auto)
          let final = sortItemPathDate $ M.toList finalDB
          let finalSingleLine = map toSingleLine final

          stdin <- fmap (nubOrd . lines . T.unpack) TIO.getContents
          if null stdin then putStrLn (unlines finalSingleLine)
            else do let lookups = map (\p -> case M.lookup p incompleteDB of
                                              Nothing -> toSingleLine (p, (("","","","",[],[],""),""))
                                              Just a  -> toSingleLine (p,(a,"a"))
                                      ) stdin
                    let hits = filter (`anyInfix` stdin) finalSingleLine
                    putStrLn $ unlines $ hits ++ lookups

blacklist :: String -> MetadataList -> [(String,(MetadataItem,String))]
blacklist sourceLabel = map (\(a,b) -> (a,(b,sourceLabel))) . filter (\(f,(title,_,_,_,_,_,_)) -> not (title=="" ||
                                                                                                  "en.wikipedia.org" `isInfixOf` f ||
                                                                                                  ("/doc/"`isPrefixOf`f && "/index" `isSuffixOf` f)))

toSingleLine :: (Path,(MetadataItem,String)) -> String
toSingleLine ("",_) = ""
toSingleLine (f,(("",_,_,_,_,[],_),_)) = f ++ " []" -- we insert '[]' to parallel links with barebones auto-metadata but lacking even a tag; this lets us grep output for all untagged links (as opposed to only being able to grep for the smaller & much more arbitrary subset, 'untagged but has an auto-title')
toSingleLine (f,(mi@(b,c,d,_,_,tags,abst),label)) = intercalate "; "
  ([ label,
     authorsToCite f c d,
    "\x1b[32m "++f++" \x1b[0m",
    show tags,
    "\x1b[35m\""++b++"\"\x1b[0m",
    " (" ++ authorsTruncateString c ++ ")",
    d,
    sed " +" " " $ replace "\n" " " abst] ++
    (let url = generateURL f mi in if null url then [] else ["\x1b[32m "++url++"\x1b[0m"])
  )
