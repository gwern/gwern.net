#!/usr/bin/env runghc

-- CLI tool to add/remove link-tags to specified URLs/paths. eg. to add one tag: 'changeTag.hs
-- "https://en.wikipedia.org/wiki/Experience_curve_effects" "economics/experience-curve"'
-- To remove the tag, negate with a hyphen '-' prefix: "-economics/experience-curve"

-- This supports multiple mixed arguments; if there are multiple links and/or multiple tags
-- specified (as identified by links starting with '/' or 'http', and tags not starting with those &
-- also the necessary tag-directory existing on disk), then we change all tags on all links (order
-- irrelevant).
--
-- eg. 'changeTag.hs "https://en.wikipedia.org/wiki/Experience_curve_effects"
-- "economics/experience-curve" "genetics/heritable"
-- "https://www.genome.gov/about-genomics/fact-sheets/DNA-Sequencing-Costs-Data"' would add 2 tags for the 2
-- links.
module Main where

import Control.Monad (when)
import Utils (replace)
import Data.List (isPrefixOf)
import Data.Maybe (isJust, fromJust)
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist, doesFileExist)

import LinkMetadata (annotateLink, readLinkMetadata, readYaml, writeYaml, MetadataList, MetadataItem)

main :: IO ()
main = do args <- fmap (map $ (\a -> if "docs/"`isPrefixOf`a then "/"++a else a) . replace ".page" "" . replace "/home/gwern/wiki/" "/" . replace "https://www.gwern.net/" "/") $ getArgs
          when (length args < 2) $ error "Error: Insufficient arguments (<2)."

          let links = filter (\arg -> head arg == '/' || "http" `isPrefixOf` arg) $ args
          when (null links) $ error ("Error: Forgot links?" ++ show args)
          let tags = map (filter (/=',')) $ -- we store tags comma-separated so sometimes we might leave in a stray tag when copy-pasting
                filter (\arg -> (not (arg `elem` links))) args
          when (null tags) $ error ("Error: Forgot tags? " ++ show args)

          mapM_ (\arg' -> do filep <- doesDirectoryExist ("docs/"++ if head arg' == '-' then tail arg' else arg')
                             if not filep then error ("Error: Specified tag not defined? '" ++ arg' ++ "'") else return arg') tags
          mapM_ (\link -> mapM_ (changeOneTag link) tags) links

changeOneTag :: String -> String -> IO ()
changeOneTag link tag = do
          let link' = replace "https://www.gwern.net/" "/" link
          -- allow shortcut additions like 'changeTag.hs docs/foo.pdf psychology'
          link'' <- if not (head link' /= '/' && take 4 link' /= "http") then return link' else
                     do existP <- doesFileExist link'
                        if existP then return $ "/" ++ link' else
                          error $ "File does not exist? : '" ++ link' ++ "'"
          when (head tag == '/' || take 4 tag == "http") $ error $ "Arguments not 'changeTag.hs *tag* link'? : '" ++ tag ++ "'"
          [custom,partial,auto] <- mapM readYaml ["metadata/custom.yaml", "metadata/partial.yaml", "metadata/auto.yaml"]
          changeAndWriteTags tag link'' custom partial auto

-- If an annotation is in custom.yaml, we only want to write that. If it's in partial.yaml,
-- likewise. If it's in auto.yaml, now that we've added a tag to it, it is no longer disposable and
-- must be preserved by moving it from auto.yaml to partial.yaml. If it's not in any metadata file
-- (such as a Wikipedia link, which is normally suppressed), then we add it to partial.yaml.
changeAndWriteTags :: String -> String -> MetadataList -> MetadataList -> MetadataList -> IO ()
changeAndWriteTags t i c p a = do let cP = hasItem i c
                                      pP = hasItem i p
                                      aP = hasItem i a
                                  if cP then writeYaml "metadata/custom.yaml" (changeTag i c t) else
                                    if pP then writeYaml "metadata/partial.yaml" (changeTag i p t) else
                                      if aP then let (autoNew,partialNew) = mvItem a p i in writeYaml "metadata/auto.yaml" autoNew >> writeYaml "metadata/partial.yaml" (changeTag i partialNew t)
                                      else addNewLink t i

-- what if a link is completely new and is not in either custom.yaml (handwritten) or auto.yaml
-- (often auto-annotated)? If we write it directly into partial.yaml, then for many links like
-- Arxiv/Biorxiv, we'd skip creating an automatic annotation!
--
-- So instead we hook back into the main link annotation workflow, create a new annotation for that
-- (which will be in auto.yaml), and then run changeTag.hs *again*, so this time it has an annotation
-- to work with (and will do auto.yaml → partial.yaml).
addNewLink :: String -> String -> IO ()
addNewLink tag p = do md <- readLinkMetadata
                      _ <- annotateLink md p
                      return () -- if returnValue then changeOneTag p tag else error ("annotateLink returned False! " ++ show tag ++ " : " ++ show p)

changeTag, addTag, removeTag :: String -> MetadataList -> String -> MetadataList
changeTag i ml tag = if head tag /= '-' then addTag i ml tag else removeTag i ml (tail tag)
addTag i ml tag = map (\(path,item@(a,b,c,d,e,f)) -> if i /= path || (tag `elem` e) then (path,item) else
                                                      (path,(a,b,c,d,e++[tag],f)) ) ml
removeTag i ml tag = map (\(path,item@(a,b,c,d,e,f)) -> if i /= path || (tag `notElem` e) then (path,item) else
                                                      (path,(a,b,c,d,filter (/= tag) e,f)) ) ml

mvItem :: MetadataList -> MetadataList -> String -> (MetadataList,MetadataList)
mvItem original new i = (removeItem original i,
                          new ++ [(i, fromJust $ getItem original i)])

getItem :: MetadataList -> String -> Maybe MetadataItem
getItem ml i = lookup i ml

removeItem :: MetadataList -> String -> MetadataList
removeItem ml i = filter (\(p,_) -> p /= i) ml

hasItem :: String -> MetadataList -> Bool
hasItem i ml = isJust $ getItem ml i
