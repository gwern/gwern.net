#!/usr/bin/env runghc

-- CLI tool to add/remove link-tags to specified URLs/paths. eg. to add one tag: 'changeTag.hs
-- "https://en.wikipedia.org/wiki/Experience_curve_effects" "economics/experience-curve"'
-- To remove the tag, negate with a hyphen '-' prefix: "-economics/experience-curve"

-- This supports multiple mixed arguments; if there are multiple links and/or multiple tags
-- specified (as identified by links starting with '/' or 'http', and tags not starting with those &
-- also the necessary tag existing on disk), then we change all tags on all links (order
-- irrelevant).
--
-- eg. '$ changeTag.hs "https://en.wikipedia.org/wiki/Experience_curve_effects"
-- "economics/experience-curve" "genetics/heritable"
-- "https://www.genome.gov/about-genomics/fact-sheets/DNA-Sequencing-Costs-Data"' would add both tags on both
-- links.
-- or piping in URLs:
--
--  $ echo 'https://en.wikipedia.org/wiki/Subvocalization' | changeTag.hs psychology/inner-monologue
-- # Executing: psychology/inner-monologue tag on link: https://en.wikipedia.org/wiki/Subvocalization
-- # https://en.wikipedia.org/wiki/Subvocalization : "https://en.wikipedia.org/wiki/Subvocalization" : ("https://en.wikipedia.org/wiki/Subvocalization",("Subvocalization","","","",[],""))
-- # https://en.wikipedia.org/wiki/Subvocalization : ( "Subvocalization" , "" , "" , "" , [] , "" )
-- # Executing: psychology/inner-monologue tag on link: https://en.wikipedia.org/wiki/Subvocalization
--
-- Tags can be tagged by tagging their directory-index file (`/doc/$TAG1/index`); to do a one-way tag (to make $TAG1 'see also' $TAG2),
-- one does `changeTag.hs $TAG1 /doc/$TAG2/index`. Since it's common to want tags to be reciprocal or bidirectional, this is a shortcut,
-- just `changeTag.hs $TAG1 $TAG2`.
module Main where

import Control.Monad (when)
import Data.List (isPrefixOf)
import Data.Maybe (isJust, fromJust)
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist, doesFileExist)
import Text.Pandoc (Inline(Link), nullAttr)
import qualified Data.Text as T (pack)

import Config.Misc as C (root)
import LinkMetadata (annotateLink, readLinkMetadata)
import GTX (readGTXFast, writeGTX)
import LinkMetadataTypes (MetadataList, MetadataItem, Failure(Temporary, Permanent))
import Tags (guessTagFromShort, listTagsAll)
import Utils (printGreen, printRed, replace, sed)

main :: IO ()
main = do
          -- read the regular CLI arguments
          args <- fmap (map $ (\a -> if "doc/"`isPrefixOf`a then "/"++a else a) . sed "\\.md$" "" . replace C.root "/" . replace "https://gwern.net/" "/") getArgs

          when (length args == 0) $ printRed "Error: 0 arguments (need 2)." >> error ""
          when (length args == 1) $ printRed $ "Error: only 1 argument (need ≥2): " >> error (show (head args))
          when ("gwt" `elem` args || "t" `elem` args) $ error "Invalid tag/URL 'gwt'/'t' detected! Is this entire command malformed? Exiting immediately."

          let links = filter (\arg -> " "/= arg && ""/=arg && (head arg == '/' || "http" `isPrefixOf` arg)) args
          allTags <- listTagsAll
          let tags = (map (\t -> if head t == '-' then "-" ++ guessTagFromShort allTags (filter (/=',') $ tail t)
                                                                                            else guessTagFromShort allTags $ filter (/=',') t) $ -- we store tags comma-separated so sometimes we might leave in a stray tag when copy-pasting
                filter (\t -> t `notElem` links && ("-"++t) `notElem` links) args) :: [String]

          when (null tags) $ error ("Error: Forgot tags? " ++ show args)
          mapM_ (\arg' -> do filep <- doesDirectoryExist ("doc/"++ if head arg' == '-' then tail arg' else arg')
                             if not filep then error ("Error: Specified tag not defined? '" ++ arg' ++ "'") else return arg') tags
          if length tags == 2 && null links then
            -- setting up a bidirectional tag, equivalent to: changeTag.hs /doc/$TAG1/index $TAG2 && changeTag.hs /doc/$TAG2/index $TAG1
            changeOneTag ("/doc/" ++ head tags ++ "/index") (tags !! 1) >>
            changeOneTag ("/doc/" ++ (tags !! 1) ++ "/index") (head tags)
           else do
            when (null links) $ error ("Error: Forgot links?" ++ show args)
            mapM_ (\link -> mapM_ (changeOneTag link) tags) links

changeOneTag :: String -> String -> IO ()
changeOneTag link tag = do
          let link' = replace "https://gwern.net/" "/" link
          -- allow shortcut additions like 'changeTag.hs doc/foo.pdf psychology'
          link'' <- if not (head link' /= '/' && take 4 link' /= "http") then return link' else
                     do existP <- doesFileExist link'
                        if existP then return $ "/" ++ link' else
                          error $ "File does not exist? : '" ++ link' ++ "'"
          when (head tag == '/' || take 4 tag == "http") $ error $ "Arguments not 'changeTag.hs *tag* link'? : '" ++ tag ++ "'"
          [full,half,auto] <- mapM readGTXFast ["metadata/full.gtx", "metadata/half.gtx", "metadata/auto.gtx"]
          printGreen ("Executing: " ++ tag ++ " tag on link: " ++ link'')
          changeAndWriteTags tag link'' full half auto

-- If an annotation is in full.gtx, we only want to write that. If it's in half.gtx,
-- likewise. If it's in auto.gtx, now that we've added a tag to it, it is no longer disposable and
-- must be preserved by moving it from auto.gtx to half.gtx. If it's not in any metadata file
-- (such as a Wikipedia link, which is normally suppressed), then we add it to half.gtx.
changeAndWriteTags :: String -> String -> MetadataList -> MetadataList -> MetadataList -> IO ()
changeAndWriteTags t i c p a = do let cP = hasItem i c
                                      pP = hasItem i p
                                      aP = hasItem i a
                                  if cP then writeUpdatedGTX c "metadata/full.gtx" (changeTag i c t) else
                                    if pP then writeUpdatedGTX p "metadata/half.gtx" (changeTag i p t) else
                                      if aP then let (autoNew,halfNew) = mvItem a p i in writeUpdatedGTX a "metadata/auto.gtx" autoNew >> writeUpdatedGTX a "metadata/half.gtx" (changeTag i halfNew t)
                                      else addNewLink t i

writeUpdatedGTX :: MetadataList -> String -> MetadataList -> IO ()
writeUpdatedGTX oldList target newList = when (oldList /= newList) $ writeGTX target newList

-- what if a link is completely new and is not in either full.gtx (handwritten) or auto.gtx
-- (often auto-annotated)? If we write it directly into half.gtx, then for many links like
-- Arxiv/BioRxiv, we'd skip creating an automatic annotation!
--
-- So instead we hook back into the main link annotation workflow, create a new annotation for that
-- (which will be in auto.gtx), and then run changeTag.hs *again*, so this time it has an annotation
-- to work with (and will do auto.gtx → half.gtx).
addNewLink :: String -> String -> IO ()
addNewLink tag p = do md <- readLinkMetadata
                      returnValue <- annotateLink md (Link nullAttr [] (T.pack p, T.pack ""))
                      case returnValue of
                        Left Temporary -> error ("annotateLink returned a Temporary error! " ++ show tag ++ " : " ++ show p)
                        Left Permanent -> changeOneTag p tag
                        Right _ -> changeOneTag p tag

changeTag, addTag, removeTag :: String -> MetadataList -> String -> MetadataList
changeTag "" a b  = error $ "changeTag called with empty arguments: " ++ "\"\""  ++ ":" ++ show a  ++ ":" ++ show b  ++ "; this should never happen."
changeTag  a [] b = error $ "changeTag called with empty arguments: " ++ show a  ++ ":" ++ "[]"    ++ ":" ++ show b  ++ "; this should never happen."
changeTag  a b "" = error $ "changeTag called with empty arguments: " ++ show a  ++ ":" ++ show b  ++ ":" ++ "\"\""  ++ "; this should never happen."
changeTag i ml tag = if head tag /= '-' then addTag i ml tag else removeTag i ml (tail tag)
addTag i ml tag = map (\(path,item@(a,b,c,d,dc,e,f)) -> if i /= path || (tag `elem` e) then (path,item) else
                                                      (path,(a,b,c,d,dc,e++[tag],f)) ) ml
removeTag i ml tag = map (\(path,item@(a,b,c,d,dc,e,f)) -> if i /= path || (tag `notElem` e) then (path,item) else
                                                      (path,(a,b,c,d,dc,filter (/= tag) e,f)) ) ml

mvItem :: MetadataList -> MetadataList -> String -> (MetadataList,MetadataList)
mvItem original new i = (removeItem original i,
                          new ++ [(i, fromJust $ getItem original i)])

getItem :: MetadataList -> String -> Maybe MetadataItem
getItem ml i = lookup i ml

removeItem :: MetadataList -> String -> MetadataList
removeItem ml i = filter (\(p,_) -> p /= i) ml

hasItem :: String -> MetadataList -> Bool
hasItem i ml = isJust $ getItem ml i
