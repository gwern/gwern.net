{- Query.hs: utility module for extracting links from Pandoc documents.
Author: Gwern Branwen
Date: 2021-12-14
When:  Time-stamp: "2021-12-14 16:42:04 gwern"
License: CC-0
-}

{-# LANGUAGE OverloadedStrings #-}
module Query (extractLinks, extractURLs, extractURLsAndAnchorTooltips, parseMarkdownOrHTML) where

import qualified Data.Text as T (init, drop, head, last, Text)
import Text.Pandoc (def, pandocExtensions, queryWith, readerExtensions, readHtml, readMarkdown, Inline(Link), runPure, Pandoc(..))
import Text.Pandoc.Walk (walk)

import Interwiki (convertInterwikiLinks, inlinesToString)

parseMarkdownOrHTML :: Bool -> T.Text -> Pandoc
parseMarkdownOrHTML md txt = let parsedEither = if md then runPure $ readMarkdown def{readerExtensions = pandocExtensions } txt
                                         else runPure $ readHtml def{readerExtensions = pandocExtensions } txt
                   in case parsedEither of
                              Left e    -> error $ "Failed to parse document: " ++ show md ++ show txt ++ show e
                              Right doc -> doc

-- | Parse one Text string as a Pandoc Markdown document and return its URLs (as Strings)
extractLinks :: Bool -> T.Text -> [T.Text]
extractLinks md txt  = extractURLs $ parseMarkdownOrHTML md txt

-- | Read 1 Pandoc AST and return its URLs as Strings
extractURLs :: Pandoc -> [T.Text]
extractURLs = queryWith (map (\(url,_,_) -> url) . extractURL) . walk convertInterwikiLinks

extractURL :: Inline -> [(T.Text,T.Text,T.Text)]
extractURL (Link _ anchorText (url,tooltip)) = if url=="" || T.head url == '$' || T.head url == '\8383' then [] -- ignore inflation-adjuster 'links'
                                               else [(url,inlinesToString anchorText,tooltip)]
extractURL _ = []

-- | Read 1 Pandoc AST and return its URLs/anchor-text pairs;
-- if a URL has both a title and an anchor text, we return 2 pairs because both might be valid (eg '[GPT-3](https://arxiv.org/foo "Language Models are Few-Shot Learners")' - we would like to do similar-links on both the short noun 'GPT-3' and the paper title, but we can't if we arbitrarily return one but not the other).
extractURLsAndAnchorTooltips :: Pandoc -> [(T.Text,[T.Text])]
extractURLsAndAnchorTooltips = queryWith extractURLSquashed . walk convertInterwikiLinks
 where
   extractURLSquashed :: Inline -> [(T.Text,[T.Text])]
   extractURLSquashed (Link _ il (u,""))     = [(u, [cleanURL $ inlinesToString il])]
   extractURLSquashed (Link _ il (u,target)) = [(u, [cleanURL $ inlinesToString il]), (u, [target])]
   extractURLSquashed _ = []

   -- NOTE: apparently due to nested Spans (from the smallcaps) and the RawInline issue (yet again), some link suggestions look like ">ADHD<". Very undesirable replacement targets. So we special-case clean those:
   cleanURL :: T.Text -> T.Text
   cleanURL "" = ""
   cleanURL u = if T.head u == '>' && T.last u == '<' then T.init $ T.drop 1 u else u
