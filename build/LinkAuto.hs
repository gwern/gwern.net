{-# LANGUAGE OverloadedStrings #-}
module LinkAuto (linkAuto, linkAutoHtml5String, linkAutoFiltered, linkAutoTest) where

{- LinkAuto.hs: search a Pandoc document for pre-defined regexp patterns, and turn matching text into a hyperlink.
Author: Gwern Branwen
Date: 2021-06-23
When:  Time-stamp: "2024-01-28 17:59:23 gwern"
License: CC-0

This is useful for automatically defining concepts, terms, and proper names using a single master
updated list of regexp/URL pairs. (Terms like "BERT" or "GPT-3" or "RoBERTa" are too hard to all
define manually on every appearance, particularly in abstracts/annotations which themselves may be
generated automatically, so it makes more sense to try to do it automatically.)

Regexps are guarded with space/punctuation/string-end-begin delimiters, to try to avoid problems of
greedy rewrites (eg. "GAN" vs "BigGAN"). Regexps are sorted by length, longest-first, to further try
to prioritize (so "BigGAN" would match before "GAN"). For efficiency, we avoid String type
conversion as much as possible. Regexp matching is done only within a Str node; therefore,
mixed-formatting strings will not match. If a match is all inside italics/bold/smallcaps (eg. 'Emph
[Str x]'), then it will match; if a match is split (eg. '...Str x1, Emph [Str x2], ...'), then it
will fail.

A document is queried for URLs and all URLs already present or regexps without plain text matches
are removed from the rewrite dictionary. This usually lets a document be skipped entirely as having
no possible non-redundant matches.

Then, we walk the AST, running each remaining regexp against Str nodes. When there is a match, the
matching substring is then rewritten to be a Link with the URL & class `link-auto` for the first
regexp that matches.

After the regexp pass, we do an additional cleanup pass. How should we handle the case of a phrase
like "GAN" or "GPT-3" appearing potentially scores or hundreds of times in page? Do we really want
to hyperlink *all* of them? Probably not. For the cleanup pass, we track 'seen' `link-auto` links in
a Set, and if a link has been seen before, we remove it, leaving the text annotated with a simple
Span 'link-auto-skipped' class. This can be deleted in post-processing to avoid cluttering the HTML
with metadata only relevant to compile-time.

Bugs: will annotate phrases inside `Header` nodes, which breaks HTML validation. Does not attempt to
handle `RawInline` or `RawBlock`, so writing raw HTML like `<a href="/modafinil">foo</a>` will not
be detected for the purposes of rewrite-short-circuiting or possibly rewriting at all.

Dependencies: Pandoc, text, regex-tdfa, /static/build/Utils.hs, /static/build/Query.hs
-}

import Data.Char (isPunctuation, isSpace)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Set as S (empty, fromList, insert, member, Set)
import qualified Data.Text as T (append, head, intercalate, length, last, replace, singleton, tail, init, pack, unpack, Text)
import Control.Monad.State (evalState, get, put, State)

import Text.Pandoc (topDown, nullAttr, readerExtensions, def, writeHtml5String, pandocExtensions, runPure, readHtml, Pandoc(..), Inline(Link,Image,Code,Span,Str), nullMeta, Block(Para))
import Text.Pandoc.Walk (walk, walkM)
import Text.Regex.TDFA as R (makeRegex, match, matchTest, Regex) -- regex-tdfa supports `(T.Text,T.Text,T.Text)` instance, to avoid packing/unpacking String matches; it is maybe 4x slower than pcre-heavy, but should have fewer Unicode & correctness/segfault/strange-closure issues (native Text, and useful splitting), so to save my sanity... BUG: TDFA seems to have slow Text instances: https://github.com/haskell-hvr/regex-tdfa/issues/9

import Utils (addClass, frequency, simplifiedDoc, safeHtmlWriterOptions, cleanUpDivsEmpty, cleanUpSpans, inlinesToText)
import Query (extractURLs)
import Typography (mergeSpaces)
import qualified Config.LinkAuto as C (customSorted, linkAutoTests)

-----------

-- turn first instance of a list of regex matches into hyperlinks in a Pandoc document. NOTE: this is best run as early as possible, because it is doing raw string matching, and any formatting or changing of phrases may break a match, but after running link syntax rewrites like the interwiki links (otherwise you'll wind up inserting WP links into pages that already have that WP link, just linked as `[foo](!W)`.)
linkAuto :: Pandoc -> Pandoc
linkAuto p@(Pandoc _ []) = p
linkAuto p = linkAutoFiltered id p

linkAutoInline2Doc :: [Inline] -> Pandoc
linkAutoInline2Doc test = let doc = Pandoc nullMeta [Para test] in
                            linkAuto doc

linkAutoTest :: [([Inline], Pandoc, Pandoc)]
linkAutoTest = map (\(inline', doc') -> (inline', doc', linkAutoInline2Doc inline')) $ filter (\(inline, doc) -> linkAutoInline2Doc inline /= doc) C.linkAutoTests

-- wrapper convenience function: run LA over a HTML string, return HTML string
linkAutoHtml5String :: String -> String
linkAutoHtml5String "" = ""
linkAutoHtml5String s = let clean = runPure $ do
                                   pandoc <- readHtml def{readerExtensions=pandocExtensions} (T.pack s)
                                   let pandoc' = linkAuto pandoc
                                   fmap T.unpack $ writeHtml5String safeHtmlWriterOptions pandoc'
                             in case clean of
                                  Left e -> error $ show e ++ " : " ++ s
                                  Right output -> output

-- if we want to run on just a subset of links (eg. remove all resulting links to Wikipedia, or delete a specific regexp match), we can pass in a filter:
linkAutoFiltered :: ([(T.Text, T.Text)] -> [(T.Text, T.Text)]) -> Pandoc -> Pandoc
linkAutoFiltered subsetter p =
  let plain = simplifiedDoc p :: T.Text -- cache the plain text of the document
  in
       let customDefinitions' = filterMatches plain $ filterDefinitions p (customDefinitions subsetter) in
         if null customDefinitions' then p else topDown cleanUpDivsEmpty $ topDown cleanUpSpans $ cleanupNestedLinks $ annotateFirstDefinitions $ walk (defineLinks customDefinitions') p

-----------

-- Walk a Pandoc document; find the first instance of every auto-definition and mark it with the HTML/CSS class `definition-auto-first`; skip any further examples of that particular defined word.
-- This lets one add CSS to highlight *just* the first definition and skip the rest; this is difficult/impossible to do in CSS alone, so requires either preprocessing  or runtime JS
annotateFirstDefinitions :: Pandoc -> Pandoc
annotateFirstDefinitions doc = evalState (walkM addFirstDefn doc) S.empty
  where addFirstDefn :: Inline -> State (S.Set T.Text) Inline
        addFirstDefn x@(Link a@(_,classes,_) il c@(t,_)) = if "link-auto" `elem` classes then
            do st <- get
               if S.member t st then return $ addClass "link-auto-skipped" $ Span nullAttr il -- Useful for debugging to annotate spans of text which *would* have been Links.
                 else do let st' = S.insert t st
                         put st'
                         return $ addClass "link-auto-first" $ Span nullAttr [Link a il c]
            else return x
        addFirstDefn x = return x

-- HACK: Somehow we can, very rarely on Gwern.net (maybe a dozen cases site-wide) wind up with Links nested inside of Links, despite attempts to block the substitution going too deep in `defineLinks`. This is bad, and also generates invalid HTML of nested <a><a></a></a>s.
-- I can't figure out what is going on, and this may be related to various weird issues which makes me suspect that Pandoc's traverse operations aren't *quite* defined right.
-- So, as a workaround, let's walk the AST looking for any nested Links, and erasing the Link wrapper.
cleanupNestedLinks :: Pandoc -> Pandoc
cleanupNestedLinks = topDown go
  where go :: Inline -> Inline
        go (Link (a,b,c) is (f,g)) =  Link (a,b,c) (walk goDeeper is) (f,g)
        go x = x
        -- we must be inside a Link's [Inline], so strip any Links we find for their [Inline] anchor text
        goDeeper :: Inline -> Inline
        goDeeper (Link _ is _) = Str $ inlinesToText is -- Span nullAttr is
        goDeeper x = x


-----------

defineLinks :: [(T.Text, R.Regex, T.Text)] -> [Inline] -> [Inline]
defineLinks [] x = x
defineLinks dict is = concatMap go $ mergeSpaces is
  where
   go :: Inline -> [Inline]
   go (Str "")   = []
   -- TODO: all these guards don't work; we want to skip recursion into some Inline types to avoid useless markup, but both `bottomUp`/`walk` create links anyway, and `topDown` seems to infinitely loop?
   go x@Link{}   = [x] -- skip links because can't have link inside link
   go x@Image{}  = [x] -- likewise
   go x@Code{}   = [x]
   go (Span a x) = [Span a (concatMap go x)]
   go x@(Str a)  = case findRegexMatch dict (T.replace " " " " a) of
                     Nothing   -> [x]
                     Just (before,"",after, _) -> go (Str before) ++ go (Str after)
                     -- NOTE: our regexps must delimit on space/punctuation, which puts the matched character *inside* `matched` instead of `before`/`after`;
                     -- unfortunately, if we move it inside the Link, this will look bad when Links get their underlining decoration
                     -- in-browser (if it's a space, it'll be a weird extended underline, and if it's punctuation, it's not usually included in a link and looks inconsistent).
                     -- So we do this song & dance to figure out if the link was *before* or *after*, remove it from the Link,
                     -- and stick a prefix or suffix replacement space/punctuation. In retrospect, it might've been better to use capture groups...
                     Just (before,matched,after, defn) ->
                       go (Str before) ++ -- NOTE: we need to recurse *before* as well after, because 'findRegexMatch' short-circuits on the first match
                                          -- but there may be a later regexp which would match somewhere in the prefix.
                       let frst = T.head matched in let lst = T.last matched in
                        (if isSpace frst || isPunctuation frst then
                                        if isSpace lst || isPunctuation lst then
                                          -- NOTE: we do *not* set .backlink-not because we want automatic links to count just as much as a hand-written link (eg. if an Arxiv abstract mentions GPT-3, then when it gets auto-linked to Brown et al 2020, then that should count just as much as if I had edited the abstract by hand - both mentions are relevant and should show up in Brown et al 2020's backlinks); cases where backlinks are not desirable from a link should be handled elsewhere, like Wikipedia links where backlinks are undesirable & that is already handled by the interwiki code
                                          [Str $ T.singleton frst, Link ("",["link-auto"],[]) [Str $ T.init $ T.tail matched] (defn, ""), Str $ T.singleton lst] else
                                          [Str $ T.singleton frst, Link ("",["link-auto"],[]) [Str $ T.tail matched] (defn, "")]
                                      else if lst == ' ' || isPunctuation lst then
                                             [Link ("",["link-auto"],[]) [Str $ T.init matched] (defn, ""), Str $ T.singleton lst]
                                           else
                                             [Link ("",["link-auto"],[]) [Str matched] (defn, "")])
                       ++ go (Str after)
   go x          = [x]

-- Recurse through the dictionary (which should be long-first) to find the first matching regexp, since the master regexp blob matched the string.
findRegexMatch :: [(T.Text, R.Regex, T.Text)] -> T.Text -> Maybe (T.Text, T.Text, T.Text, T.Text)
findRegexMatch [] _ = Nothing
findRegexMatch ((_,r,u):rs) s = let (a,b,c) = R.match r s in
                                   if b/="" then Just (a,b,c,u) else findRegexMatch rs s

-- Optimization: take a set of definitions, and a document; query document for existing URLs; if a
-- URL is already present, drop it from the definition list.
-- This avoids redundancy with links added by hand or other filters.
--
-- NOTE: This can be used to disable link rewrites by manually adding a link. In cases of self-links
-- (eg. /modafinil will contain the word 'modafinil' and get a rewrite to /modafinil, leading to a
-- useless self-link), it is easier to add a link to disable the rewrite than to figure out how to
-- filter out that one exact rewrite only on that page. This link can be hidden to avoid distracting
-- the reader.
-- So to disable the modafinil rewrite on /modafinil, one could insert into the Markdown a line like:
-- `<span style="display:none;">[null](/modafinil)</span> <!-- LinkAuto override: disable self-linking -->`
filterDefinitions :: Pandoc -> [(T.Text, R.Regex, T.Text)] -> [(T.Text, R.Regex, T.Text)]
filterDefinitions p = let allLinks = S.fromList $ map (T.replace "https://gwern.net/" "/") $ extractURLs p in
                                          filter (\(_,_,linkTarget) -> linkTarget `notElem` allLinks)

-- Optimization: try to prune a set of definitions and a document. Convert document to plain text,
-- and do a global search; if a regexp matches the plain text, it may or may not match the AST, but
-- if it does not match the plain text, it should never match the AST?
-- Since generally <1% of regexps will match anywhere in the document, doing a single global check
-- lets us discard that regexp completely, and not check at every node. So we can trade off doing
-- 𝒪(R × Nodes) regexp checks for doing 𝒪(R + Nodes) + plain-text-compilation, which in practice
-- turns out to be a *huge* performance gain (>30×?) here.
-- Hypothetically, we can optimize this further: we can glue together regexps to binary search the
-- list for matching regexps, giving something like 𝒪(log R) passes. Alternately, it may be possible
-- to create a 'regexp trie' where the leaves are associated with each original regexp, and search
-- the trie in parallel for all matching leaves.
filterMatches :: T.Text -> [(T.Text, R.Regex, T.Text)] -> [(T.Text, R.Regex, T.Text)]
filterMatches plain definitions  = if T.length plain < 10000 then
                                       -- for short texts like annotations, the recursive tree is extremely expensive, so just do the straight-line version:
                                       if not (matchTest allRegex plain) then []
                                       else filter (\(_,r,_) -> matchTest r plain) definitions
                                       -- if long (>10k characters), we start the tree slog:
                                     else filterMatch True definitions
  where

   allRegex :: R.Regex -- in the default case of all regexes are valid (because nothing could be filtered out), use the precompiled top-level all-regex Regex value for efficiency, else, create a new one:
   allRegex = masterRegex definitions -- if map (\(a,_,_) -> a) definitions == map fst custom then masterRegexAll else masterRegex definitions

   -- Optimization: we can glue together regexps to binary search the list for matching regexps, giving something like 𝒪(log R) passes.
   -- divide-and-conquer recursion: if we have 1 regexp left to test, test it and return if matches or empty list otherwise;
   -- if we have more than one regexp, test the full list; if none match, return empty list, otherwise, split in half, and recurse on each half.
   filterMatch :: Bool -> [(T.Text, R.Regex, T.Text)] -> [(T.Text, R.Regex, T.Text)]
   filterMatch _ []  = []
   filterMatch _ [d] = [d | matchTest (masterRegex [d]) plain] -- only one match left, base case
   -- if none of the regexps match, quit; if any match, then decide whether the remaining list is short enough to check 1 by 1, or if
   -- it is long enough that we should try to split it up into sublists and fork out the recursive call; doing a 'wide' recursion *should* be a lot faster than a binary tree
   filterMatch skipCheck ds
    -- for the very first iteration (called from `filterMatches`), we want to skip the master regex because it will be huge and slow.
    -- So, immediately descend:
    | skipCheck = concatMap (filterMatch False . return) ds
    | not (matchTest (masterRegex ds) plain) = []
    | otherwise = concatMap (filterMatch False . return) ds

-- create a simple heuristic master regexp using alternation out of all possible regexes, for the heuristic check 'filterMatches'. WARNING: Depending on the regex library, just alternating regexes (rather than using a regexp trie) could potentially trigger an exponential explosion in RAM usage...
masterRegex :: [(T.Text, R.Regex, T.Text)] -> R.Regex
masterRegex ds = R.makeRegex $ T.intercalate "|" $ map (\(a,_,_) -> a) ds

-- masterRegexAll :: R.Regex
-- masterRegexAll = masterRegex (customDefinitions id)

-- We want to match our given regexps by making them 'word-level' and matching on punctuation/whitespace delimiters. This avoids subword matches, for example, matching 'GAN' in 'StyleGAN' is undesirable.
customDefinitionsR :: [(T.Text, T.Text)] -> [(T.Text, R.Regex, T.Text)]
customDefinitionsR = map (\(a,b) -> (a,
                                      R.makeRegex $ "[[:punct:][:blank:]]"`T.append`a`T.append`"[[:punct:][:blank:]]",
                                      b))

-----------

-- validate and error out immediately if there are bad rewrites defined
definitionsValidate :: [(T.Text, T.Text)] -> [(T.Text, T.Text)]
definitionsValidate defs
    | nubOrd (map fst defs) /= map fst defs = error $ "LinkAuto fatal error: Definition keys are not unique! Definitions: "   ++ show (frequency $ map fst defs)
    | nubOrd (map snd defs) /= map snd defs = error $ "LinkAuto fatal error: Definition values are not unique! Definitions: " ++ show (frequency $ map snd defs)
    | otherwise = defs

-- Create sorted (by length) list of (string/compiled-regexp/substitution) tuples.
-- This can be filtered on the third value to remove redundant matches, and the first value can be
-- concatenated into a single master regexp.
-- Possible future feature: instead of returning a simple 'T.Text' value as the definition, which is
-- substituted by the rewrite code into a 'Link' element (the knowledge of which is hardwired), one
-- could instead return a 'T.Text -> Inline' function instead (making the type '[(T.Text, R.Regex,
-- (T.Text -> Inline))]'), to insert an arbitrary 'Inline' (not necessarily a Link, or possibly a
-- custom kind of Link). This would be a much more general form of text rewriting, which could
-- support other features, such as turning into multiple links (eg. one link for each word in a
-- phrase), abbreviated phrases (a shorthand could be expanded to a Span containing arbitrary
-- '[Inline]'), transclusion of large blocks of text, simplified DSLs of sorts, etc. The standard
-- link substitution boilerplate would be provided by a helper function like 'link :: T.Text ->
-- (T.Text -> Inline); link x = \match -> Link ... [Str match] (x,...)'.
-- I'm not sure how crazy I want to get with the rewrites, though. The regexp rewriting is expensive
-- since it must look at all text. If you're doing those sorts of other rewrites, it'd generally be
-- more sensible to require them to be marked up explicitly, which is vastly easier to program &
-- more efficient. We'll see.
customDefinitions :: ([(T.Text, T.Text)] -> [(T.Text, T.Text)]) -> [(T.Text, R.Regex, T.Text)]
customDefinitions subsetter = if length C.customSorted > 1007 then error ("LinkAuto.hs (customDefinitions): 'C.customSorted' too long (" ++ show (length C.customSorted) ++ "), which will trigger the LA slowdown, making site compiles unacceptably slow. Delete some unused regexp rewrite rules!")
                              else customDefinitionsR $ definitionsValidate $ subsetter C.customSorted -- delimit & compile
