#!/usr/bin/env runghc
{-# LANGUAGE OverloadedStrings #-}

-- link-titler.hs: add titles to bare links in a Markdown file using a database of link metadata
-- Author: Gwern Branwen
-- Date: 2022-04-01
-- When:  Time-stamp: "2024-05-11 17:51:07 gwern"
-- License: CC-0
--
-- Read a Markdown page, parse links out, look up their titles, generate a standard Gwern.net-style citation ('"Title", Author1 et al Year[a-z]'),
-- and edit the raw text file to insert that title after links which have no title set in the Markdown source.
-- This will also update the link metadata database itself.
-- (Note: while LinkMetadata does generate & create these tooltip titles automatically if missing, that is not helpful while *writing*
-- because you'll only see them in the live website's compiled HTML.)
--
-- This makes it much easier to read a Markdown file with a lot of opaque URL references, without the extraordinarily tedious
-- mechanical busywork of hand-writing the tooltip title every time.
-- This makes searches of the full codebase more useful as well since
-- you'll get hits on the lead author/title/year instead of needing the exact URL. (And probably makes document text embeddings better.)
--
-- This can be run regularly in a cron job to keep files up to date.
--
-- WARNING: This tends to add too many titles, because deciding equality/similarity of anchor text & the candidate title text is hard. So any additional markup or text difference can lead to a redundant insertion.
-- This does not handle raw HTML links (`<a>`) or some of the Pandoc link variations <https://pandoc.org/MANUAL.html#links-1> ('automatic links', 'reference links', or 'shortcut reference links').
-- This can break 'simple' tables which are space-separated (rare because it is difficult to write links inside space-separated tables except as 'reference links' which will be skipped by this script).
--
-- The reason for the editing-raw-text-file is because Pandoc does not preserve the original Markdown formatting/syntax (only semantics)
-- and its converted version I find uglier. The linebreaks make it harder to search. And full conversion would severely clutter the VCS history.

import Control.Monad.Parallel as Par (mapM_)

import Data.Char (isPunctuation, isSpace, toLower)
import qualified Data.Map.Strict as M (lookup, fromListWith, toList)
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Text as T (append, replace, pack, unpack, Text)

import LinkID (authorsToCite)
import LinkMetadata (walkAndUpdateLinkMetadata, readLinkMetadata)
import LinkMetadataTypes (Metadata, MetadataItem)
import Query (extractURLsAndAnchorTooltips, parseMarkdownOrHTML)
import Utils (printGreen, replaceMany, writeUpdatedFile, replace)

main :: IO ()
main = do md <- readLinkMetadata

          -- update specified Markdown files:
          args <- getArgs
          printGreen "Updating Markdown arguments…"
          Par.mapM_ (addTitlesToFile md) args

          -- update annotations; reminder: `walkAndUpdateLinkMetadata :: Bool -> ((Path, MetadataItem) -> IO (Path, MetadataItem)) -> IO ()`
          printGreen "Updating all HTML annotations as well…"
          walkAndUpdateLinkMetadata False (addTitlesToHTML md)

addTitlesToFile :: Metadata -> String -> IO ()
addTitlesToFile md filepath = do
          fileContents <- TIO.readFile filepath

          let pandoc = parseMarkdownOrHTML True fileContents
          let links = extractURLsAndAnchorTooltips pandoc

          let untitled = map (\(u,titles') -> (u, head titles')) $ filter (\(_,titles) -> length titles == 1) links :: [(T.Text, T.Text)]

          let titled = filter (\(u',t'') -> not (u' == "" || t'' == "")) $
                                 map (\(u,t') -> case M.lookup (T.unpack u) md of
                                                   Nothing -> ("","")
                                                   Just ("",_,_,_,_,_,_) -> ("","")
                                                   Just (_,"",_,_,_,_,_) -> ("","")
                                                   Just (_,_,"",_,_,_,_) -> ("","")
                                                   Just (t,aut,dt,_,_,_,_) -> if T.pack t == t' ||
                                                                                textSimplifier (T.pack t) == textSimplifier t'
                                                                            then ("","") else
                                                                              let authorCite = authorsToCite (T.unpack u) aut dt in
                                                                              (u, T.pack $
                                                                                  if textSimplifier t' == textSimplifier (T.pack authorCite) then t
                                                                                  else "‘" ++ (replace "’’" "’" t) ++ "’, " ++ authorCite)
                               ) untitled :: [(T.Text, T.Text)]

          let updatedFile = foldr (\(url,titleNew) text -> T.replace (url `T.append` ")") -- TODO: `replaceCheckedT`
                                                                     (url `T.append` " \"" `T.append` titleNew `T.append` "\")")
                                                                     text)
                             fileContents titled

          writeUpdatedFile "link-titler" filepath updatedFile
          return ()

-- TODO: refactor; most of this is redundant
addTitlesToHTML :: Metadata -> (String,MetadataItem) -> IO (String,MetadataItem)
addTitlesToHTML md (path,(title,author,date,dc,kvs,tags,abstract))
  = let pandoc = parseMarkdownOrHTML False (T.pack abstract)
        links = M.toList $ M.fromListWith (++) $ extractURLsAndAnchorTooltips pandoc

        untitled = map (\(u,titles') -> (u, head titles')) $ filter (\(_,titles) -> length titles == 1) links :: [(T.Text, T.Text)]

        titled = filter (\(u',t'') -> not (u' == "" || t'' == "")) $
                           map (\(u,t') -> case M.lookup (T.unpack u) md of
                         Nothing -> ("","")
                         Just ("",  _,_, _,_,_,_) -> ("","")
                         Just (_,  "",_, _,_,_,_) -> ("","")
                         Just (_,   _,"",_,_,_,_) -> ("","")
                         Just (t, aut,dt,_,_,_,_) -> if T.pack t == t' ||
                                                     textSimplifier (T.pack t) == textSimplifier t'
                                                  then ("","") else
                                                    let authorCite = authorsToCite (T.unpack u) aut dt in
                                                      (u, T.pack $
                                                        if textSimplifier t' == textSimplifier (T.pack authorCite) then ""
                                                        else "‘" ++ t ++ "’, " ++ authorCite)
                         ) untitled :: [(T.Text, T.Text)]

        updatedAbstract = foldr (\(url,titleNew) text -> T.replace (url `T.append` "\"") -- TODO: `replaceCheckedT`
                                                               (url `T.append` "\" title=\"" `T.append` titleNew `T.append` "\"")
                                                               text)
                       (T.pack abstract) titled
    in return (path,(title,author,date,dc,kvs,tags,T.unpack updatedAbstract))

-- simplify a title as much as possible to find similar title/anchor pairs to skip rewriting:
textSimplifier :: T.Text -> T.Text
textSimplifier = T.pack .
                 map toLower .
                 filter (\c -> not (isPunctuation c || isSpace c))  .
                 replaceMany [("<em>",""), ("</em>",""), ("<sub>",""), ("</sub>",""), ("<sup>",""), ("</sup>",""), ("<strong>",""), ("</strong>",""), ("&#39;","")] .
                 T.unpack
