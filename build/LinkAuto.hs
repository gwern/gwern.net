{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module LinkAuto (linkAuto, testDoc) where

-- LinkAuto.hs: search a Pandoc document for pre-defined regexp patterns, and turn matching text into a hyperlink.
--
-- This is useful for automatically defining concepts, terms, and proper names using a single master updated list of regexp/URL pairs.
-- (Terms like "BERT" or "GPT-3" or "RoBERTa" are too hard to all define manually on every appearance, particularly in abstracts/annotations which themselves may be generated automatically, so it makes more sense to try to do it automatically.)
--
-- Regexps are guarded with space/punctuation/string-end-begin delimiters, to try to avoid problems of greedy rewrites (eg "GAN" vs "BigGAN").
-- Regexps are sorted by length, longest-first, to further try to prioritize (so "BigGAN" would match before "GAN").
-- For efficiency, we avoid String & conversion as much as possible.
-- A document is queried for URLs and all URLs already present are removed from the rewrite dictionary.
-- Then, all remaining regexps are compiled into a single master regexp, and this is used to grovel through the AST, munging Str/Space nodes together to create single long Str elements; once the master regexp hits, we fall back to checking each original regexp against the string match.
-- The matching string is then rewritten to be a Link with the URL & class `link-auto` for the first regexp that matches.
-- After the regexp pass, we do an additional cleanup pass. How should we handle the case of a phrase like "GAN" or "GPT-3" appearing potentially scores or hundreds of times in page? Do we really want to  hyperlink *all* of them? Probably not.
-- For the cleanup pass, we track 'seen' `link-auto` links in a Set, and if a link has been seen before, we remove it.
-- (In the future, we may drop this clean up pass, if we can find a good way to dynamically hide 'excess' links; one idea is define `.link-auto` CSS to de-style links, and then, on browser screen scroll, use JS to re-link-style the first instance of each URL. So only the first instance would be visible on each screen, minimizing redundancy/clutter/over-linking.)
--
-- Dependencies: Pandoc, regex-tdfa-text

import Data.List (intersperse, sortBy)
import Text.Pandoc (bottomUp, queryWith, nullMeta, Pandoc(..), Block(Para), Inline(Emph,Link,Image,Code,Space,Span,Str))
import Text.Pandoc.Walk (walkM)
import Text.Regex.TDFA as R (makeRegex, match, Regex, (=~))
import Text.Regex.TDFA.Text () -- for the `(T.Text,T.Text,T.Text)` instance, to avoid packing/unpacking String matches
import qualified Data.Set as S (empty, fromList, insert, member, Set)
import Control.Monad.State (evalState, get, put, State)
import qualified Data.Text as T (append, concat, head, length, reverse, strip, Text)

import Debug.Trace as Trace

test :: [Inline]
test = [Str "bigGAN means", Emph [Str "BIG"], Str "GAN; you will have an easier time training a GAN on a good GPU like a P100 or a TPUv3.", Space, Str "(See",Space,Str "WP",Space,Str "on",Space,Link ("",[],[]) [Str "GAN"] ("https://en.wikipedia.org/wiki/Generative_adversarial_network",""),Str ")", Space, Str "Nevertheless, expensive is a GAN. See Barack Obama's presidency. Still, we shouldn't put too much weight on Barack Obama. More efficient is DistilBERT, not to be confused with", Space, Span ("",["smallcaps-auto"],[]) [Str "BERT"], Str "."]
testDoc :: Pandoc
testDoc = let doc = Pandoc nullMeta [Para test] in
            linkAuto doc

-----------

-- Turn first instance of a list of regex matches into hyperlinks in a Pandoc document.
linkAuto :: Pandoc -> Pandoc
linkAuto p = let customDefinitions' = filterDefinitions p customDefinitions in
               let master = definitionsToRegexp customDefinitions' in
                 annotateFirstDefinitions $ bottomUp (defineLinks customDefinitions' master) p

-----------

-- Walk a Pandoc document; find the first instance of every auto-definition and mark it with the HTML/CSS class `definition-auto-first`; skip any further examples of that particular defined word.
-- This lets one add CSS to highlight *just* the first definition and skip the rest; this is difficult/impossible to do in CSS alone, so requires either preprocessing  or runtime JS
annotateFirstDefinitions :: Pandoc -> Pandoc
annotateFirstDefinitions doc = evalState (walkM addFirstDefn doc) S.empty
  where addFirstDefn :: Inline -> State (S.Set T.Text) Inline
        addFirstDefn x@(Link (ident,classes,values) il (t,tool)) = if "link-auto" `elem` classes then
            do st <- get
               if S.member t st then return (Span ("", ["link-auto-skipped"], []) il)
                 else do let st' = S.insert t st
                         put st'
                         return $ Link (ident,classes++["link-auto-first"],values) il (t,tool)
            else return x
        addFirstDefn x = return x

-----------

defineLinks :: [(T.Text, R.Regex, T.Text)] -> R.Regex -> [Inline] -> [Inline]
defineLinks dict masterRegexp = concatMap go . mergeSpaces
  where
    go :: Inline -> [Inline]
    go (Str "")  = []
    -- TODO: all these guards don't work; we want to skip recursion into some Inline types to avoid useless markup, but both `bottomUp`/`walk` create links anyway, and `topDown` seems to infinitely loop?
    go x@Link{}  = [x] -- skip links because can't have link inside link
    go x@Image{} = [x] -- likewise
    go x@Code{}  = [x]
    go (Span a x) = [Span a (concatMap go x)]
    go x@(Str a) = let r@(before,matched,after) = R.match masterRegexp a :: (T.Text,T.Text,T.Text)
                   in if matched==""
                      then [x] -- no acronym anywhere in x
                      else if T.length matched > 90 then error ("Too long LinkAuto match! " ++ show r ++ ": " ++ show a) else
                             let definition = findRegexMatch dict matched in
                             case definition of
                              Nothing   -> Str (before`T.append`matched) : go (Str after)
                              -- NOTE: our regexps must delimit on space, which puts the matched space inside `matched` instead of `before`/`after`;
                              -- unfortunately, if we move the Space inside the Link, this will look bad when Links get their underlining decoration
                              -- in-browser. So we do this song & dance to figure out if the link was *before* or *after*, remove it from the Link,
                              -- and stick a prefix or suffix replacement Space.
                              Just defn -> Str before : if T.head matched == ' ' then
                                                               if T.head (T.reverse matched) == ' ' then
                                                                  [Link ("",["link-auto"],[]) [Str $ T.strip matched] (defn, ""), Space] else
                                                                 [Space, Link ("",["link-auto"],[]) [Str $ T.strip matched] (defn, "")]
                                                             else
                                [Link ("",["link-auto"],[]) [Str matched] (defn, "")]
      ++ go (Str after)
    go x = [x]

-- step through the dictionary (which should be long-first) to find the first matching regexp, since the master regexp blob matched the string
findRegexMatch :: [(T.Text, R.Regex, T.Text)] -> T.Text -> Maybe T.Text
findRegexMatch [] _ = Nothing
findRegexMatch ((_,r,u):rs) s = if R.match r s then Just u else findRegexMatch rs s

-- Pandoc breaks up strings as much as possible, like [Str "ABC", Space, "notation"], which makes it impossible to match on them, so we remove Space
mergeSpaces :: [Inline] -> [Inline]
mergeSpaces []                     = []
mergeSpaces (Str x:Str y:xs)       = Str (x`T.append`y):mergeSpaces xs
mergeSpaces (Space:Str x:Space:xs) = mergeSpaces (Str (" "`T.append`x`T.append`" "):xs)
mergeSpaces (Space:Str x:xs)       = mergeSpaces (Str (" "`T.append`x):xs)
mergeSpaces (Str x:Space:xs)       = mergeSpaces (Str (x`T.append`" "):xs)
mergeSpaces (Str "":xs)            = mergeSpaces xs
mergeSpaces (x:xs)                 = x:mergeSpaces xs

-- take a set of definitions, and a document; query document for existing URLs; if a URL is already present, drop it from the definition list.
-- This avoids redundancy with links added by hand or other filters.
filterDefinitions :: Pandoc -> [(T.Text, R.Regex, T.Text)] -> [(T.Text, R.Regex, T.Text)]
filterDefinitions (Pandoc _ markdown) = let allLinks = S.fromList $ queryWith extractLink markdown in
                                          filter (\(_,_,linkTarget) -> linkTarget `notElem` allLinks)
  where
   extractLink :: Inline -> [T.Text]
   extractLink (Link _ _ (path, _)) = [path]
   extractLink _ = []

-- R.makeRegex ("[[:alnum:]]+"::String)
-- create a single master regexp which matches all possible definition keys, whether 'GAN' or 'reinforcement learning'
definitionsToRegexp :: [(T.Text, b, T.Text)] -> R.Regex
definitionsToRegexp = R.makeRegex . T.concat . intersperse "|" .
                      map ((\r -> "[[:punct:][:blank:]]"`T.append`r`T.append`"[[:punct:][:blank:]]|"
                                  `T.append`"^"`T.append`r`T.append`"$")
                            . (\(a,_,_) -> a))

customDefinitionsR :: [(T.Text, T.Text)] -> [(T.Text, R.Regex, T.Text)]
customDefinitionsR = map (\(a,b) -> (a, R.makeRegex a, b))

-----------

-- Create sorted (by length) list of (string/compiled-regexp/substitution) tuples.
-- This can be filtered on the third value to remove redundant matches, and the first value can be concatenated into a single master regexp.
customDefinitions :: [(T.Text, R.Regex, T.Text)]
customDefinitions = customDefinitionsR $
                    sortBy (\a b -> compare (T.length $ fst b) (T.length $ fst a)) -- descending order, longest match to shortest (for regex priority):
  [
  -- valid classes are '[:alnum:]', '[:digit:]', '[:punct:]', '[:alpha:]', '[:graph:]', '[:space:]', '[:blank:]', '[:lower:]', '[:upper:]', '[:cntrl:]', '[:print:]', '[:xdigit:]', '[:word:]'.
  ("GAN", "https://en.wikipedia.org/wiki/Generative_adversarial_network")
  , ("Barack Obama", "https://en.wikipedia.org/wiki/Barack_Obama")
  , ("President Obama", "https://en.wikipedia.org/wiki/Barack_Obama")
  , ("BERT", "https://arxiv.org/abs/1810.04805#google")
  ]
