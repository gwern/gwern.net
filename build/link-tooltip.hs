#!/usr/bin/env runghc
{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import System.Environment (getArgs)
import Text.Pandoc (queryWith, Pandoc, Inline(Link))
import qualified Data.Map.Strict as M (lookup, fromList, Map)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as TIO (readFile)

import LinkMetadata (walkAndUpdateLinkMetadata)
import Annotation (tooltipToMetadata)
import LinkMetadataTypes (MetadataItem)
import Query (parseMarkdownOrHTML)

main :: IO ()
main = do -- update specified Markdown files:
          args <- getArgs
          triplets <- extractMetadataCandidates (head args)
          -- update annotations; reminder: `walkAndUpdateLinkMetadata :: ((String, MetadataItem) → IO (String, MetadataItem)) → IO ()`
          walkAndUpdateLinkMetadata False (upgradeMetadata (M.fromList triplets))

extractAnchorTooltips :: Pandoc -> [(String,String)]
extractAnchorTooltips = queryWith extractURLSquashed
 where
   extractURLSquashed :: Inline -> [(String,String)]
   extractURLSquashed (Link _ _ (_,""))     = []
   extractURLSquashed (Link _ _ (u,tooltip)) = [(T.unpack u, T.unpack tooltip)]
   extractURLSquashed _ = []

upgradeMetadata :: M.Map String (String,String,String) -> (String, MetadataItem) -> IO (String, MetadataItem)
upgradeMetadata db x@(p,(t,a,d,dc,kvs,ts,abst)) = if t/= "" then return x else
                                                 case M.lookup p db of
                                                   Nothing -> return x
                                                   Just (t',a',d') -> return (p, (t', a `m` a', d `m` d', dc,
                                                                                      kvs,ts,abst))
  where m y z = if length y > length z then y else z

extractMetadataCandidates :: String -> IO [(String, (String,String,String))]
extractMetadataCandidates filepath = do
          fileContents <- TIO.readFile filepath
          let pandoc = parseMarkdownOrHTML True fileContents
          let tooltips = extractAnchorTooltips pandoc
          let new = sort $ map (\(u,tt) -> (u, tooltipToMetadata filepath tt)) tooltips
          let new' = filter (\(_,(t,_,_)) -> t /= "") new
          return new'
