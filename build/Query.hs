{- Query.hs: utility module for extracting links from Pandoc documents.
Author: Gwern Branwen
Date: 2021-12-14
When:  Time-stamp: "2024-04-07 17:17:59 gwern"
License: CC-0
-}

{-# LANGUAGE OverloadedStrings #-}
module Query (extractImages, extractLinks, extractLinksWith, extractURLs, extractURLsWith, extractURL, extractURLWith, extractURLsAndAnchorTooltips, parseMarkdownOrHTML, truncateTOCHTML, extractLinksInlines, extractLinkIDsWith) where

import qualified Data.Text as T (append, init, drop, head, last, takeWhile, Text)
import Text.Pandoc (def, pandocExtensions, queryWith, readerExtensions, readHtml, readMarkdown, Inline(Image, Link), runPure, Pandoc(..), Block(BulletList, OrderedList), nullMeta)
import Text.Pandoc.Walk (query, walk)

import Interwiki (convertInterwikiLinks)
import Utils (inlinesToText)

parseMarkdownOrHTML :: Bool -> T.Text -> Pandoc
parseMarkdownOrHTML mdp txt = let parsedEither = if mdp then runPure $ readMarkdown def{readerExtensions = pandocExtensions } txt
                                         else runPure $ readHtml def{readerExtensions = pandocExtensions } txt
                   in case parsedEither of
                              Left e    -> error $ "Failed to parse document: " ++ show mdp ++ show txt ++ show e
                              Right doc -> doc

-- | Parse one Text string as a Pandoc Markdown (True) or HTML (False) document and return its URLs (as Strings). Note: this can return duplicates.
extractLinks :: Bool -> T.Text -> [T.Text]
extractLinks = extractLinksWith (const True)

-- | takes a filter function (to ignore & take all links, just `(const True)` it); boolean option: True: Markdown; False: must be HTML
extractLinksWith :: (Inline -> Bool) -> Bool -> T.Text -> [T.Text]
extractLinksWith rule md txt = extractURLsWith rule $ parseMarkdownOrHTML md txt

extractURLsWith :: (Inline -> Bool) -> Pandoc -> [T.Text]
extractURLsWith rule = queryWith (map (\(url,_,_) -> url) . extractURLWith rule) . convertInterwikiLinks

-- | Read 1 Pandoc AST and return its URLs as Strings
extractURLs :: Pandoc -> [T.Text]
extractURLs = extractURLsWith (const True)

extractURLWith :: (Inline -> Bool) -> Inline -> [(T.Text,T.Text,T.Text)]
extractURLWith _ x@(Link _ _ ("", _)) = error $ "Invalid link used in extractURLWith: " ++ show x
extractURLWith rule x@(Link _ anchorText (url, tooltip))
    | url == "" || T.head url == '$' || T.head url == '\8383' = []
    | rule x = [(url, inlinesToText anchorText, tooltip)]
    | otherwise = []
extractURLWith _ _ = []

-- Note: does not count images; for that, see `extractImages`
extractLinkIDsWith :: (Inline -> Bool) -> T.Text  -> Pandoc -> [(T.Text, T.Text)]
extractLinkIDsWith rule filename pndc = queryWith extractIDs $ convertInterwikiLinks pndc
  where extractIDs :: Inline -> [(T.Text, T.Text)]
        extractIDs x@(Link ("",_,_)    _ (url,_)) = if rule x then [(url, filename)] else []
        extractIDs x@(Link (ident,_,_) _ (url,_)) = if rule x then [(url, (T.takeWhile (/='#') $ filename) `T.append` "#" `T.append` ident)] else []
        extractIDs _ = []

extractURL :: Inline -> [(T.Text,T.Text,T.Text)]
extractURL = extractURLWith (const True)

-- | Read 1 Pandoc AST and return its URLs/anchor-text pairs;
-- if a URL has both a title and an anchor text, we return 2 pairs because both might be valid (eg.
-- `[GPT-3](https://arxiv.org/abs/2005.14165#openai "Language Models are Few-Shot Learners")`—we would like to do
-- similar-links on both the short noun 'GPT-3' and the paper title, but we can't if we arbitrarily
-- return one but not the other).
--
-- Special links: this will convert interwiki links to the full URLs, but it will leave alone any local links (it will not prefix 'https://gwern.net') or any inflation-adjusted links (currently, links starting with '$' or '₿').
extractURLsAndAnchorTooltips :: Pandoc -> [(T.Text,[T.Text])]
extractURLsAndAnchorTooltips = queryWith extractURLSquashed . convertInterwikiLinks
 where
   extractURLSquashed :: Inline -> [(T.Text,[T.Text])]
   extractURLSquashed (Link _ il (u,""))     = [(u, [cleanURL $ inlinesToText il])]
   extractURLSquashed (Link _ il (u,tooltip)) = [(u, (cleanURL $ inlinesToText il) : [tooltip])]
   extractURLSquashed _ = []

   -- NOTE: apparently due to nested Spans (from the smallcaps) and the RawInline issue (yet again), some link suggestions look like ">ADHD<". Very undesirable replacement targets. So we special-case clean those:
   cleanURL :: T.Text -> T.Text
   cleanURL "" = ""
   cleanURL u = if T.head u == '>' && T.last u == '<' then T.init $ T.drop 1 u else u

-- Extract 'Image' Inline elements from a Pandoc. Note, this does not extract solely <figure> images, which are Images inside their own Paragraph Block:
--
-- > $ echo -e 'baz\n\n![foo](/foo.jpg)\n\nquux' | pandoc -w native
-- > → [Para [Str "baz"]
-- >   ,Para [Image ("",[],[]) [Str "foo"] ("/foo.jpg","fig:")]
-- >   ,Para [Str "quux"]]
--
-- (That would be harder to match, and I don't use inline non-block Images much, so I can usually assume that every 'Image' is a <figure>.)
--
-- > extractImages (Pandoc nullMeta [Para [Str "baz"] ,Para [Image ("",[],[]) [Str "foo"] ("/foo.jpg","fig:")] ,Para [Str "quux"]])
-- → [Image ("",[],[]) [Str "foo"] ("/foo.jpg","fig:")]
extractImages :: Pandoc -> [Inline] -- [Image]
extractImages = queryWith extractImages'
 where extractImages' :: Inline -> [Inline]
       extractImages' x@Image{} = [x]
       extractImages' _ = []

-- TODO: an extractImages which operates like `extractLinkIDsWith`, and can be used in generateBacklinks so images count as links; this is useful for annotations *of* image files, to get their origins. (eg. we annotate /doc/2022-foo-figure1.jpg for the annotation of /doc/2022-foo.pdf, and we make the JPG an annotation due to its long complex caption; obviously, the annotation of the JPG should in some way point back to /doc/2022-foo.pdf, and we'd rather not hardwire a reference inside the image caption, as that would be weird when one reads the annotation of /doc/2022-foo.pdf - it's linking to itself...?)

extractLinksInlines :: Pandoc -> [Inline] -- [Link]
extractLinksInlines = queryWith extractLinksInlines'
 where extractLinksInlines' :: Inline -> [Inline]
       extractLinksInlines' x@Link{} = [x]
       extractLinksInlines' _ = []

-- testList :: [Block]
-- testList = [BulletList [[Para [Link ("",[],[]) [Str "Foo"] ("#foo","title")]],[Para [Link ("",[],[]) [Str "Bar"] ("#bar","title")],BulletList[[Plain [Link ("",[],[]) [Str "Quux"] ("#quux","title")]]]],[Para [Link ("",[],[]) [Str "Baz"] ("#baz","title")]]]]
filterListBlocksContainingAnchor :: T.Text -> [Block] -> [Block]
filterListBlocksContainingAnchor i blocks = concat $ query (listContainsAnchor i) blocks
listContainsAnchor :: T.Text -> Block -> [[Block]]
listContainsAnchor i (BulletList lists) = map (containsAnchor i) lists
listContainsAnchor i (OrderedList _ lists) = map (containsAnchor i) lists
listContainsAnchor _ _ = []
containsAnchor :: T.Text -> [Block] -> [Block]
containsAnchor i list = let urls = extractURLs (Pandoc nullMeta list) in if ("#"`T.append`i) `elem` urls then list
                                                                         else []

-- remove IDs from Links in a set of Blocks and avoid resetting them
idsRemove :: [Block] -> [Block]
idsRemove = walk idRemove
idRemove :: Inline -> Inline
idRemove (Link ("",a,b) c d) = Link ("","id-not":a,b) c d
idRemove (Link (_,a,b) c d)  = Link ("","id-not":a,b) c d
idRemove x = x

-- truncateTOCHTML "appendix" "<div class=\"columns\" id=\"TOC\"> <ul> <li> <a href=\"#far-from-the-madding-crowd\"><span>Far From the Madding Crowd</span></a> </li> <li> <a href=\"#egypt\"><span>Egypt</span></a> </li> <li> <a href=\"#more\"><span>More</span></a> </li> <li> <a href=\"#are-cats-domesticated\"><span>Are Cats Domesticated?</span></a> <ul> <li> <a href=\"#dysgenics\"><span>Dysgenics</span></a> <ul> <li> <a href=\"#what-is-to-be-done\"><span>What is to be done?</span></a> </li> </ul> </li> </ul> </li> <li> <a href=\"#bibliography\"><span>Bibliography</span></a> </li> <li> <a href=\"#external-links\"><span>External Links</span></a> </li> <li> <a href=\"#appendix\"><span>Appendix</span></a> <ul> <li> <a href=\"#fuzz-testing\"><span>Fuzz Testing</span></a> </li> <li> <a href=\"#toys\"><span>Toys</span></a> </li> </ul> </li> <li><a href=\"#footnotes\"><span>Footnotes</span></a></li></ul></div>"
-- [Plain [Link ("",["id-not"],[]) [Span ("",[],[]) [Str "Appendix"]] ("#appendix","")],BulletList [[Plain [Link ("",["id-not"],[]) [Span ("",[],[]) [Str "Fuzz",Space,Str "Testing"]] ("#fuzz-testing","")]],[Plain [Link ("",["id-not"],[]) [Span ("",[],[]) [Str "Toys"]] ("#toys","")]]]]
truncateTOCHTML :: T.Text -> T.Text -> [Block]
truncateTOCHTML i toc = let (Pandoc _ blocks) = parseMarkdownOrHTML False toc in idsRemove $ -- we strip IDs from the ToC as unnecessary and leading to collisions with the abstract when the abstract links to sections
                                                                                 filterListBlocksContainingAnchor i blocks
