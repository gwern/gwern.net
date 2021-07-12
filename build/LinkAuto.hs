{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module LinkAuto (linkAuto) where

{- LinkAuto.hs: search a Pandoc document for pre-defined regexp patterns, and turn matching text into a hyperlink.
Author: Gwern Branwen
Date: 2021-06-23
When:  Time-stamp: "2021-07-11 17:07:59 gwern"
License: CC-0

This is useful for automatically defining concepts, terms, and proper names using a single master updated list of regexp/URL pairs.
(Terms like "BERT" or "GPT-3" or "RoBERTa" are too hard to all define manually on every appearance, particularly in abstracts/annotations which themselves may be generated automatically, so it makes more sense to try to do it automatically.)

Regexps are guarded with space/punctuation/string-end-begin delimiters, to try to avoid problems of greedy rewrites (eg "GAN" vs "BigGAN").
Regexps are sorted by length, longest-first, to further try to prioritize (so "BigGAN" would match before "GAN").
For efficiency, we avoid String type conversion as much as possible.
Regexp matching is done only within a Str node; therefore, mixed-formatting strings will not match.
If a match is all inside italics/bold/smallcaps (eg 'Emph [Str x]'), then it will match; if a match is split (eg '...Str x1, Emph [Str x2], ...'), then it will fail.

A document is queried for URLs and all URLs already present or regexps without plain text matches are removed from the rewrite dictionary.
This usually lets a document be skipped entirely as having no possible non-redundant matches.

Then, we walk the AST, running each remaining regexp against Str nodes.
When there is a match, the matching substring is then rewritten to be a Link with the URL & class `link-auto` for the first regexp that matches.

After the regexp pass, we do an additional cleanup pass. How should we handle the case of a phrase like "GAN" or "GPT-3" appearing potentially scores or hundreds of times in page? Do we really want to  hyperlink *all* of them? Probably not.
For the cleanup pass, we track 'seen' `link-auto` links in a Set, and if a link has been seen before, we remove it, leaving the text annotated with a simple Span 'link-auto-skipped' class.
(In the future, we may drop this clean up pass, if we can find a good way to dynamically hide 'excess' links; one idea is define `.link-auto` CSS to de-style links, and then, on browser screen scroll, use JS to re-link-style the first instance of each URL. So only the first instance would be visible on each screen, minimizing redundancy/clutter/over-linking.)

Bugs: will annotate phrases inside `Header` nodes, which breaks HTML validation.

Dependencies: Pandoc, text, regex-tdfa, /static/build/Columns.hs
-}

import Data.Char (isPunctuation)
import Data.List (nub, sortBy)
import qualified Data.Set as S (empty, fromList, insert, member, Set)
import qualified Data.Text as T (append, head, intercalate, length, last, replace, singleton, tail, init, Text)
import Control.Monad.State (evalState, get, put, State)

import Text.Pandoc (topDown, queryWith, nullAttr, Pandoc(..), Inline(Link,Image,Code,Space,Span,Str))
import Text.Pandoc.Walk (walkM, walk)
import Text.Regex.TDFA as R (makeRegex, match, matchTest, Regex) -- regex-tdfa supports `(T.Text,T.Text,T.Text)` instance, to avoid packing/unpacking String matches; it is maybe 4x slower than pcre-heavy, but should have fewer Unicode & correctness issues (native Text, and useful splitting), so to save my sanity... BUG: TDFA seems to have slow Text instances: https://github.com/haskell-hvr/regex-tdfa/issues/9

import Columns (simplifiedDoc)

-- test,test2 :: [Inline]
-- -- test3 = [Link ("",[],[]) [Quoted DoubleQuote [Str "Self-improving",Space,Str "reactive",Space,Str "agents",Space,Str "based",Space,Str "on",Space,Str "reinforcement",Space,Str "learning,",Space,Str "planning",Space,Str "and",Space,Str "teaching"]] ("http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.75.7884&rep=rep1&type=pdf",""),Str ",",Space,Str "Lin",Space,Str "1992"]
-- test2 = [Str "It's a dilemma: at small or easy domains, StyleGAN is much faster (if not better); but at large or hard domains, mode collapse is too risky and endangers the big investment necessary to surpass StyleGAN. MuZero vs Muesli."]
-- test = [Str "bigGAN means", Space, Str "BIG", Str "GAN; you will have an easier time training a GAN on a good GPU like a P100 or a TPUv3.", Space, Str "(See",Space,Str "WP",Space,Str "on",Space,Link ("",[],[]) [Link ("",[],[]) [Str "GAN"] ("https://en.wikipedia.org/wiki/Generative_adversarial_network","")] ("https://en.wikipedia.org/wiki/Generative_adversarial_network",""),Str ").", Space, Str "Nevertheless, expensive is a GAN. See Barack Obama's presidency. Still, we shouldn't put too much weight on Barack Obama. More efficient is DistilBERT, not to be confused with", Space, Str "BERT", Str "."]
-- testDoc :: Pandoc
-- testDoc = let doc = Pandoc nullMeta [Para test] in
--             linkAuto doc

-----------

-- turn first instance of a list of regex matches into hyperlinks in a Pandoc document. NOTE: this is best run as early as possible, because it is doing raw string matching, and any formatting or changing of phrases may break a match, but after running link syntax rewrites like the interwiki links (otherwise you'll wind up inserting WP links into pages that already have that WP link, just linked as `[foo](!Wikipedia)`.)
linkAuto :: Pandoc -> Pandoc
linkAuto p = let customDefinitions' = filterMatches p $ filterDefinitions p customDefinitions in
               if null customDefinitions' then p else cleanupNestedLinks $ annotateFirstDefinitions $ walk (defineLinks customDefinitions') p

-----------

-- Walk a Pandoc document; find the first instance of every auto-definition and mark it with the HTML/CSS class `definition-auto-first`; skip any further examples of that particular defined word.
-- This lets one add CSS to highlight *just* the first definition and skip the rest; this is difficult/impossible to do in CSS alone, so requires either preprocessing  or runtime JS
annotateFirstDefinitions :: Pandoc -> Pandoc
annotateFirstDefinitions doc = evalState (walkM addFirstDefn doc) S.empty
  where addFirstDefn :: Inline -> State (S.Set T.Text) Inline
        addFirstDefn x@(Link (ident,classes,values) il (t,tool)) = if "link-auto" `elem` classes then
            do st <- get
               if S.member t st then return (Span ("", ["link-auto-skipped"], []) il) -- Useful for debugging to annotate spans of text which *would* have been Links.
                 else do let st' = S.insert t st
                         put st'
                         return $ Link (ident,classes++["link-auto-first"],values) il (t,tool)
            else return x
        addFirstDefn x = return x

-- HACK: Somehow we can, very rarely on gwern.net (maybe a dozen cases site-wide) wind up with Links nested inside of Links, despite attempts to block the substitution going too deep in `defineLinks`. This is bad, and also generates invalid HTML of nested <a><a></a></a>s.
-- I can't figure out what is going on, and this may be related to various weird issues which makes me suspect that Pandoc's traverse operations aren't *quite* defined right.
-- So, as a workaround, let's walk the AST looking for any nested Links, and erasing the Link wrapper.
cleanupNestedLinks :: Pandoc -> Pandoc
cleanupNestedLinks = topDown go
  where go :: Inline -> Inline
        go (Link (a,b,c) is (f,g)) =  Link (a,b,c) (walk goDeeper is) (f,g)
        go x = x
        -- we must be inside a Link's [Inline], so strip any Links we find for their [Inline] anchor text
        goDeeper :: Inline -> Inline
        goDeeper (Link _ is _) = Span nullAttr is
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
   go x@(Str a)  = case findRegexMatch dict a of
                     Nothing   -> [x]
                     -- NOTE: our regexps must delimit on space/punctuation, which puts the matched character *inside* `matched` instead of `before`/`after`;
                     -- unfortunately, if we move it inside the Link, this will look bad when Links get their underlining decoration
                     -- in-browser (if it's a space, it'll be a weird extended underline, and if it's punctuation, it's not usually included in a link and looks inconsistent).
                     -- So we do this song & dance to figure out if the link was *before* or *after*, remove it from the Link,
                     -- and stick a prefix or suffix replacement space/punctuation. In retrospect, it might've been better to use capture groups...
                     Just (before,matched,after, defn) ->
                       go (Str before) ++ -- NOTE: we need to recurse *before* as well after, because 'findRegexMatch' short-circuits on the first match
                                          -- but there may be a later regexp which would match somewhere in the prefix.
                       let frst = T.head matched in let lst = T.last matched in
                        (if frst == ' ' || isPunctuation frst then
                                        if lst == ' ' || isPunctuation lst then
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

-- Pandoc breaks up strings as much as possible, like [Str "ABC", Space, "notation"], which makes it impossible to match on them, so we remove Space
mergeSpaces :: [Inline] -> [Inline]
mergeSpaces []                     = []
mergeSpaces (Str x:Str y:xs)       = mergeSpaces (Str (x`T.append`y) : xs)
mergeSpaces (Space:Str x:Space:xs) = mergeSpaces (Str (" "`T.append`x`T.append`" "):xs)
mergeSpaces (Space:Str x:xs)       = mergeSpaces (Str (" "`T.append`x):xs)
mergeSpaces (Str x:Space:xs)       = mergeSpaces (Str (x`T.append`" "):xs)
mergeSpaces (Str "":xs)            = mergeSpaces xs
mergeSpaces (x:xs)                 = x:mergeSpaces xs

-- Optimization: take a set of definitions, and a document; query document for existing URLs; if a URL is already present, drop it from the definition list.
-- This avoids redundancy with links added by hand or other filters.
filterDefinitions :: Pandoc -> [(T.Text, R.Regex, T.Text)] -> [(T.Text, R.Regex, T.Text)]
filterDefinitions (Pandoc _ markdown) = let allLinks = S.fromList $ map (T.replace "https://www.gwern.net/" "/") $ queryWith extractLink markdown in
                                          filter (\(_,_,linkTarget) -> linkTarget `notElem` allLinks)
  where
   extractLink :: Inline -> [T.Text]
   extractLink (Link _ _ (path, _)) = [path]
   extractLink _ = []

-- Optimization: try to prune a set of definitions and a document. Convert document to plain text, and do a global search; if a regexp matches the plain text, it may or may not match the AST, but if it does not match the plain text, it should never match the AST?
-- Since generally <1% of regexps will match anywhere in the document, doing a single global check lets us discard that regexp completely, and not check at every node. So we can trade off doing 𝑂(R × Nodes) regexp checks for doing 𝑂(R + Nodes) plus compiling to plain, which in practice turns out to be a *huge* performance gain (>30×?) here.
-- Hypothetically, we can optimize this further: we can glue together regexps to binary search the list for matching regexps, giving something like 𝑂(log R) passes. Alternately, it may be possible to create a 'regexp trie' where the leaves are associated with each original regexp, and search the trie in parallel for all matching leaves.
filterMatches :: Pandoc -> [(T.Text, R.Regex, T.Text)] -> [(T.Text, R.Regex, T.Text)]
filterMatches p definitions  = let plain = simplifiedDoc p
  in if not (matchTest masterRegex plain) -- see if document matches *any* regex, to try to bail out early
     then []
     else filter (\(_,b,_) -> matchTest b plain) definitions -- if so, test regexes one by one

-- We want to match our given regexps by making them 'word-level' and matching on punctuation/whitespace delimiters. This avoids subword matches, for example, matching 'GAN' in 'StyleGAN' is undesirable.
customDefinitionsR :: [(T.Text, T.Text)] -> [(T.Text, R.Regex, T.Text)]
customDefinitionsR = map (\(a,b) -> (a,
                                      R.makeRegex $ "[[:punct:][:blank:]]"`T.append`a`T.append`"[[:punct:][:blank:]]",
                                      b))

-----------

-- create a simple heuristic master regexp using alternation out of all possible regexes, for the heuristic check 'filterMatches'. WARNING: Depending on the regex library, just alternating regexes (rather than using a regexp trie) could potentially trigger an exponential explosion in RAM usage...
masterRegex :: R.Regex
masterRegex = R.makeRegex $ T.intercalate "|" $ map (\(a,_,_) -> a) $ customDefinitions

-- Create sorted (by length) list of (string/compiled-regexp/substitution) tuples.
-- This can be filtered on the third value to remove redundant matches, and the first value can be concatenated into a single master regexp.
-- Possible future feature: instead of returning a simple 'T.Text' value as the definition, which is substituted by the rewrite code into a 'Link' element (the knowledge of which is hardwired), one could instead return a 'T.Text -> Inline' function instead (making the type '[(T.Text, R.Regex, (T.Text -> Inline))]'), to insert an arbitrary 'Inline' (not necessarily a Link, or possibly a custom kind of Link). This would be a much more general form of text rewriting, which could support other features, such as turning into multiple links (eg one link for each word in a phrase), abbreviated phrases (a shorthand could be expanded to a Span containing arbitrary '[Inline]'), transclusion of large blocks of text, simplified DSLs of sorts, etc. The standard link substitution boilerplate would be provided by a helper function like 'link :: T.Text -> (T.Text -> Inline); link x = \match -> Link ... [Str match] (x,...)'.
-- I'm not sure how crazy I want to get with the rewrites, though. The regexp rewriting is expensive since it must look at all text. If you're doing those sorts of other rewrites, it'd generally be more sensible to require them to be marked up explicitly, which is vastly easier to program & more efficient. We'll see.
customDefinitions :: [(T.Text, R.Regex, T.Text)]
customDefinitions = customDefinitionsR $ -- delimit & compile
                    nub $ -- unique
                    -- descending order, longest match to shortest (for regex priority):
                    sortBy (\a b -> compare (T.length $ fst b) (T.length $ fst a))
  ([
  ("15\\.ai", "https://fifteen.ai/")
  , ("ABalytics", "https://github.com/danmaz74/ABalytics")
  , ("Ackermann function", "https://en.wikipedia.org/wiki/Ackermann_function")
  , ("additive regression models", "https://en.wikipedia.org/wiki/Generalized_additive_model")
  , ("advpng", "https://en.wikipedia.org/wiki/AdvanceCOMP")
  , ("A\\. ?E\\. Housman", "https://en.wikipedia.org/wiki/A._E._Housman")
  , ("Agent57", "https://arxiv.org/abs/2003.13350#deepmind")
  , ("Agrippina", "https://en.wikipedia.org/wiki/Agrippina_(opera)")
  , ("AI Dungeon", "https://aidungeon.io/")
  , ("AIXI\\.?js", "https://arxiv.org/abs/1705.07615")
  , ("Alan Perlis", "https://en.wikipedia.org/wiki/Alan_Perlis")
  , ("Alexey Guzey", "https://guzey.com/")
  , ("Alfred W. McCoy", "https://en.wikipedia.org/wiki/Alfred_W._McCoy")
  , ("ALIGN", "https://arxiv.org/abs/2102.05918#google")
  , ("Allegrini et al 2018", "https://www.biorxiv.org/content/10.1101/418210v1.full-text")
  , ("Andy Matuschak", "https://andymatuschak.org/")
  , ("Andy Warhol", "https://en.wikipedia.org/wiki/Lonesome_Cowboys")
  , ("AniSeg", "https://github.com/jerryli27/AniSeg/")
  , ("anise", "https://en.wikipedia.org/wiki/Anise")
  , ("Apollo 11", "https://en.wikipedia.org/wiki/Apollo_11")
  , ("Arab slave trade", "https://en.wikipedia.org/wiki/Barbary_slave_trade")
  , ("arbtt", "http://arbtt.nomeata.de/")
  , ("Artbreeder", "https://artbreeder.com/")
  , ("Arthur C\\. Clarke", "https://en.wikipedia.org/wiki/Arthur_C._Clarke")
  , ("[Aa]ssurance [Cc]ontract", "https://en.wikipedia.org/wiki/Assurance_contract")
  , ("[Dd]ominant [Aa]ssurance [Cc]ontract", "https://en.wikipedia.org/wiki/Assurance_contract#Dominant_assurance_contracts")
  , ("ATreeC", "https://arxiv.org/abs/1710.11417")
  , ("Bandai", "https://en.wikipedia.org/wiki/Bandai")
  , ("Barlow Twins?", "https://arxiv.org/abs/2103.03230#facebook")
  , ("BART", "https://arxiv.org/abs/1910.13461#facebook")
  , ("Baskerville", "https://en.wikipedia.org/wiki/Baskerville")
  , ("Berkson's paradox", "https://en.wikipedia.org/wiki/Berkson%27s_paradox")
  , ("BERT", "https://arxiv.org/abs/1810.04805#google")
  , ("B-heaps?", "https://en.wikipedia.org/wiki/B-heap")
  , ("Bias in Mental Testing", "https://en.wikipedia.org/wiki/Bias_in_Mental_Testing")
  , ("BigBird", "https://arxiv.org/abs/2007.14062#google")
  , ("(Big [Ff]ive|OCEAN|Big 5)", "https://en.wikipedia.org/wiki/Big_Five_personality_traits")
  , ("Openness to [Ee]xperience", "https://en.wikipedia.org/wiki/Openness_to_experience")
  , ("(Extraversion|Introversion)", "https://en.wikipedia.org/wiki/Extraversion_and_introversion")
  , ("Agreeableness", "https://en.wikipedia.org/wiki/Agreeableness")
  , ("Neuroticism", "https://en.wikipedia.org/wiki/Neuroticism")
  , ("Conscientiousness", "https://en.wikipedia.org/wiki/Conscientiousness")
  , ("BigGAN(-deep)s?", "https://arxiv.org/abs/1809.11096")
  , ("[Bb]irthday paradoxe?s?", "https://en.wikipedia.org/wiki/Birthday_problem")
  , ("BiT", "https://arxiv.org/abs/1912.11370#google")
  , ("[Bb]itter [Ll]essons?", "http://www.incompleteideas.net/IncIdeas/BitterLesson.html")
  , ("Blair Braverman", "https://en.wikipedia.org/wiki/Blair_Braverman")
  , ("Blender", "https://arxiv.org/abs/2004.13637#facebook")
  , ("[Bb]ody double", "https://en.wikipedia.org/wiki/Political_decoy")
  , ("Book of Job", "https://en.wikipedia.org/wiki/Book_of_Job")
  , ("[Bb]ottlenose dolphins?", "https://en.wikipedia.org/wiki/Bottlenose_dolphin")
  , ("Brad Leithauser", "https://en.wikipedia.org/wiki/Brad_Leithauser")
  , ("Brain Workshop", "http://brainworkshop.sourceforge.net/")
  , ("brms", "https://github.com/paul-buerkner/brms")
  , ("Brock et al 2018", "https://arxiv.org/abs/1809.11096")
  , ("[Bb]rown adipose tissues?", "https://en.wikipedia.org/wiki/Brown_adipose_tissue")
  , ("[Bb]rown-nosed coatis", "https://en.wikipedia.org/wiki/South_American_coati")
  , ("Busy Beaver functions?", "https://en.wikipedia.org/wiki/Busy_beaver")
  , ("ByT5", "https://arxiv.org/abs/2105.13626#google")
  , ("C4\\.5", "https://en.wikipedia.org/wiki/C4.5_algorithm")
  , ("[Cc]ache-oblivious", "https://en.wikipedia.org/wiki/Cache-oblivious_algorithm")
  , ("[Cc]anned dog food", "https://en.wikipedia.org/wiki/Dog_food#Wet_food")
  , ("[Cc]ard marking", "https://en.wikipedia.org/wiki/Card_marking")
  , ("John Carreyrou", "https://en.wikipedia.org/wiki/John_Carreyrou")
  , ("Catherynne M. Valente", "https://en.wikipedia.org/wiki/Catherynne_M._Valente")
  , ("CC[ -]12M", "https://arxiv.org/abs/2102.08981#google")
  , ("[Cc]eiling effects?", "https://en.wikipedia.org/wiki/Ceiling_effect_(statistics)")
  , ("[Cc]erebral cortexe?s?", "https://en.wikipedia.org/wiki/Cerebral_cortex")
  , ("Christopher Murray", "https://en.wikipedia.org/wiki/Christopher_J.L._Murray")
  , ("citronellol", "https://en.wikipedia.org/wiki/Citronellol")
  , ("Clarence Birdseye", "https://en.wikipedia.org/wiki/Clarence_Birdseye")
  , ("CLIP", "https://openai.com/blog/clip/")
  , ("Clune 2019", "https://arxiv.org/abs/1905.10985#uber")
  , ("CNVs?", "https://en.wikipedia.org/wiki/Copy-number_variation")
  , ("CogView", "https://arxiv.org/abs/2105.13290")
  , ("[Cc]ompressed [Aa]ir [Ee]nergy [Ss]torage", "https://en.wikipedia.org/wiki/Compressed-air_energy_storage")
  , ("[Cc]omputational fluid dynamics?", "https://en.wikipedia.org/wiki/Computational_fluid_dynamics")
  , ("Conformer", "https://arxiv.org/abs/2005.08100#google")
  , ("[Cc]onsanguineous marriages?", "https://en.wikipedia.org/wiki/Consanguine_marriage")
  , ("ConViT", "https://arxiv.org/abs/2103.10697#facebook")
  , ("CPM", "https://arxiv.org/abs/2012.00413")
  , ("CPM-2", "https://arxiv.org/abs/2106.10715")
  , ("CRISPR/Cas9", "https://en.wikipedia.org/wiki/Cas9")
  , ("C\\. ?S\\. ?Lewis", "https://en.wikipedia.org/wiki/C._S._Lewis")
  , ("CTRL", "https://arxiv.org/abs/1909.05858#salesforce")
  , ("[Cc]urare", "https://en.wikipedia.org/wiki/Curare")
  , ("Dactyl", "https://openai.com/blog/learning-dexterity/")
  , ("DALL[-·]E", "https://openai.com/blog/dall-e/")
  , ("Darkleaks?", "https://github.com/darkwallet/darkleaks")
  , ("Daryl Bem", "https://en.wikipedia.org/wiki/Daryl_Bem#%22Feeling_the_Future%22_controversy")
  , ("[Dd]asatinib", "https://en.wikipedia.org/wiki/Dasatinib")
  , ("David Foster Wallace", "https://en.wikipedia.org/wiki/David_Foster_Wallace")
  , ("David Lewis'", "https://en.wikipedia.org/wiki/David_Lewis_(philosopher)")
  , ("DDPMs?", "https://arxiv.org/abs/2006.11239")
  , ("DeBERTa", "https://arxiv.org/abs/2006.03654#microsoft")
  , ("Decision Transformers?", "https://sites.google.com/berkeley.edu/decision-transformer")
  , ("[Dd]eep brain stimulation", "https://en.wikipedia.org/wiki/Deep_brain_stimulation")
  , ("DeepDanbooru", "https://github.com/KichangKim/DeepDanbooru")
  , ("Deep TAMER", "https://arxiv.org/abs/1709.10163")
  , ("Deep Voice 2", "https://arxiv.org/abs/1705.08947")
  , ("[Dd]efault [Mm]ode [Nn]etwork", "https://en.wikipedia.org/wiki/Default_mode_network")
  , ("del\\.icio\\.us", "https://en.wikipedia.org/wiki/Delicious_(website)")
  , ("Demis Hassabis", "https://en.wikipedia.org/wiki/Demis_Hassabis")
  , ("development of the Marcellus Shale", "https://en.wikipedia.org/wiki/Marcellus_natural_gas_trend")
  , ("DFAs?", "https://arxiv.org/abs/1609.01596")
  , ("Frank Dikötter", "https://en.wikipedia.org/wiki/Frank_Dik%C3%B6tter")
  , ("Disney animators' strike", "https://en.wikipedia.org/wiki/Disney_animators%27_strike")
  , ("[Dd]itche?s?", "https://en.wikipedia.org/wiki/Ditch_(fortification)")
  , ("DLRM", "https://arxiv.org/abs/2104.05158#facebook")
  , ("[Dd]octrine [Oo]f ([Tt]he )?[Dd]ouble [Ee]ffect", "https://en.wikipedia.org/wiki/Principle_of_double_effect")
  , ("Domenico Starnone", "https://en.wikipedia.org/wiki/Domenico_Starnone")
  , ("DQN", "https://arxiv.org/abs/1312.5602#deepmind")
  , ("Drake equation", "https://en.wikipedia.org/wiki/Drake_equation")
  , ("Drôme", "https://en.wikipedia.org/wiki/Dr%C3%B4me")
  , ("drop-?caps?", "https://en.wikipedia.org/wiki/Initial")
  , ("[Dd]ynamic programming", "https://en.wikipedia.org/wiki/Dynamic_programming")
  , ("Edward O\\. Thorp", "https://en.wikipedia.org/wiki/Edward_O._Thorp")
  , ("Edward Thorndike", "https://en.wikipedia.org/wiki/Edward_Thorndike")
  , ("Edward Tufte", "https://en.wikipedia.org/wiki/Edward_Tufte")
  , ("EfficientNet", "https://arxiv.org/abs/1905.11946#google")
  , ("Ehrlich-Simon wager", "https://en.wikipedia.org/wiki/Simon%E2%80%93Ehrlich_wager")
  , ("EigenGAN", "https://arxiv.org/abs/2104.12476")
  , ("Eleme", "https://en.wikipedia.org/wiki/Ele.me")
  , ("Elena Ferrante", "https://en.wikipedia.org/wiki/Elena_Ferrante")
  , ("El Ten Eleven", "https://en.wikipedia.org/wiki/El_Ten_Eleven")
  , ("[Ee]nd-[Tt]o-[Ee]nd", "/notes/End-to-end")
  , ("entorhinal-hippocampal", "https://en.wikipedia.org/wiki/EC-hippocampus_system")
  , ("E\\. ?O\\. ?Wilson", "https://en.wikipedia.org/wiki/E._O._Wilson")
  , ("Epigrams in Programming", "/docs/cs/1982-perlis.pdf")
  , ("Equivalence [Pp]rinciple", "https://en.wikipedia.org/wiki/Equivalence_principle")
  , ("Eric S\\. Raymond", "https://en.wikipedia.org/wiki/Eric_S._Raymond")
  , ("Eriksen Flanker", "https://en.wikipedia.org/wiki/Eriksen_flanker_task")
  , ("[Ee]thogram", "https://en.wikipedia.org/wiki/Ethogram")
  , ("E\\. ?T\\. ?Jaynes", "https://en.wikipedia.org/wiki/Edwin_Thompson_Jaynes")
  , ("Evangelion 2\\.0", "https://en.wikipedia.org/wiki/Evangelion:_2.0_You_Can_(Not)_Advance")
  , ("Evangelion: 3\\.0", "https://en.wikipedia.org/wiki/Evangelion:_3.0_You_Can_(Not)_Redo")
  , ("Daniel Everett", "https://en.wikipedia.org/wiki/Daniel_Everett")
  , ("Hugh Everett", "https://en.wikipedia.org/wiki/Hugh_Everett_III")
  , ("[Ee]xenatide", "https://en.wikipedia.org/wiki/Exenatide")
  , ("[Ee]xome sequencing", "https://en.wikipedia.org/wiki/Exome_sequencing")
  , ("[Ee]xomes?", "https://en.wikipedia.org/wiki/Exome_sequencing")
  , ("[Ee]xperience curves?", "https://en.wikipedia.org/wiki/Experience_curve_effects")
  , ("Explosions [Ii]n [Tt]he Sky", "https://en.wikipedia.org/wiki/Explosions_in_the_Sky")
  , ("FAVOR\\+", "https://arxiv.org/abs/2009.14794#google")
  , ("[Ff]eline facial pheromones?", "https://en.wikipedia.org/wiki/Cat_pheromone#Feline_facial_pheromone")
  , ("Fischer Black", "https://en.wikipedia.org/wiki/Fischer_Black")
  , ("[Ff]ixing effect", "https://en.wikipedia.org/wiki/Functional_fixedness")
  , ("Flash", "https://en.wikipedia.org/wiki/Adobe_Flash")
  , ("Flexner Report", "https://en.wikipedia.org/wiki/Flexner_Report")
  , ("[Ff]luorinert", "https://en.wikipedia.org/wiki/Fluorinert")
  , ("Fourier identity", "https://en.wikipedia.org/wiki/Fourier_transform")
  , ("Frankie Muniz", "https://en.wikipedia.org/wiki/Frankie_Muniz")
  , ("Frank P. Ramsey", "https://en.wikipedia.org/wiki/Frank_Ramsey_(mathematician)")
  , ("Franz Ferdinand", "https://en.wikipedia.org/wiki/Archduke_Franz_Ferdinand_of_Austria")
  , ("Friendship [Ii]s Optimal", "http://www.fimfiction.net/story/62074/Friendship-is-Optimal")
  , ("Fujiwara no Teika", "https://en.wikipedia.org/wiki/Fujiwara_no_Teika")
  , ("GAIL", "https://arxiv.org/abs/1606.03476")
  , ("GANs?", "https://en.wikipedia.org/wiki/Generative_adversarial_network")
  , ("GANSpace", "https://github.com/harskish/ganspace")
  , ("Ge et al 2016", "https://www.biorxiv.org/content/10.1101/070177v1.full-text")
  , ("[Gg]enetic [Cc]orrelations?", "https://en.wikipedia.org/wiki/Genetic_correlation")
  , ("Gene Wolfe", "https://en.wikipedia.org/wiki/Gene_Wolfe")
  , ("[Gg]enome-[Ww]ide [Cc]omplex [Tt]rait [Aa]nalysis", "https://en.wikipedia.org/wiki/Genome-wide_complex_trait_analysis")
  , ("Georgia Tech", "https://en.wikipedia.org/wiki/Georgia_Institute_of_Technology")
  , ("Gerard Manley Hopkins", "https://en.wikipedia.org/wiki/Gerard_Manley_Hopkins")
  , ("Gitit", "https://github.com/jgm/gitit")
  , ("Global Burden of Disease", "https://en.wikipedia.org/wiki/Global_burden_of_disease")
  , ("[Gg]lucagon", "https://en.wikipedia.org/wiki/Glucagon")
  , ("GLUE", "https://arxiv.org/abs/1804.07461")
  , ("gMLP", "https://arxiv.org/abs/2105.08050#google")
  , ("God [Ii]s [Aa]n Astronaut", "https://en.wikipedia.org/wiki/God_Is_an_Astronaut")
  , ("GODIVA", "https://arxiv.org/abs/2104.14806#microsoft")
  , ("Go[- ]?Explore", "https://arxiv.org/abs/1901.10995#uber")
  , ("GPipe", "https://arxiv.org/abs/1811.06965#google")
  , ("GPT-1", "https://openai.com/blog/language-unsupervised/")
  , ("GPT-2", "/docs/ai/2019-radford.pdf#openai")
  , ("GPT-3", "https://arxiv.org/abs/2005.14165#openai")
  , ("GPT-f", "https://arxiv.org/abs/2009.03393#openai")
  , ("GPT-J", "https://arankomatsuzaki.wordpress.com/2021/06/04/gpt-j/")
  , ("Grover", "https://arxiv.org/abs/1905.12616")
  , ("gscan2pdf", "http://gscan2pdf.sourceforge.net/")
  , ("GShard", "https://arxiv.org/abs/2006.16668#google")
  , ("GSPMD", "https://arxiv.org/abs/2105.04663#google")
  , ("Guzey", "https://guzey.com/")
  , ("[Hh]eavy water", "https://en.wikipedia.org/wiki/Heavy_water")
  , ("Henry Darger", "https://en.wikipedia.org/wiki/Henry_Darger")
  , ("Herbert Hoover", "https://en.wikipedia.org/wiki/Herbert_Hoover")
  , ("Hex", "https://en.wikipedia.org/wiki/Hex_(board_game)")
  , ("[Hh]idden-variable theor(y|ies)", "https://en.wikipedia.org/wiki/Hidden-variable_theory")
  , ("Hideaki Anno", "https://en.wikipedia.org/wiki/Hideaki_Anno")
  , ("Hideo Azuma", "https://en.wikipedia.org/wiki/Hideo%20Azuma")
  , ("[Hh]igh jumping", "https://en.wikipedia.org/wiki/High_jump")
  , ("Hiroshi Miyauchi", "https://en.wikipedia.org/wiki/Hiroshi%20Miyauchi")
  , ("Hokkaido", "https://en.wikipedia.org/wiki/Hokkaido")
  , ("[Hh]oly [Ww]ars?", "http://www.catb.org/jargon/html/H/holy-wars.html")
  , ("Homeric Question", "https://en.wikipedia.org/wiki/Homeric_Question")
  , ("Hsu 2014", "https://arxiv.org/abs/1408.3421")
  , ("Hwang Woo-suk", "https://en.wikipedia.org/wiki/Hwang_Woo-suk")
  , ("[Hh]yalin", "https://en.wikipedia.org/wiki/Hyalin")
  , ("[Hh]yper ?[Nn]etworks", "https://arxiv.org/abs/1609.09106#google")
  , ("ID3", "https://en.wikipedia.org/wiki/ID3_algorithm")
  , ("[Ii]diopathic hypersomnia", "https://en.wikipedia.org/wiki/Idiopathic_hypersomnia")
  , ("iGPT", "https://openai.com/blog/image-gpt/")
  , ("Image[Nn]et", "https://arxiv.org/abs/1409.0575")
  , ("IMPALA", "https://arxiv.org/abs/1802.01561#deepmind")
  , ("[Ii]nclusionists?", "http://meta.wikimedia.org/wiki/Inclusionism")
  , ("[Ii]ncretin", "https://en.wikipedia.org/wiki/Incretin")
  , ("[Ii]nequality of arithmetic and geometric means", "https://en.wikipedia.org/wiki/Inequality_of_arithmetic_and_geometric_means")
  , ("[Ii]nferotemporal \\(IT\\) [Cc]ortex", "https://en.wikipedia.org/wiki/Inferior_temporal_gyrus")
  , ("[Ii]nferotemporal [Cc]ortex", "https://en.wikipedia.org/wiki/Inferior_temporal_gyrus")
  , ("Inflation\\.hs", "/static/build/Inflation.hs")
  , ("[Ii]yashikei", "https://tvtropes.org/pmwiki/pmwiki.php/Main/Iyashikei")
  , ("James C\\. Scott", "https://en.wikipedia.org/wiki/James_C._Scott")
  , ("J\\. ?B\\. ?S\\. Haldane", "https://en.wikipedia.org/wiki/J._B._S._Haldane")
  , ("[Jj]ello", "https://en.wikipedia.org/wiki/Gelatin_dessert")
  , ("Jensen's inequality", "https://en.wikipedia.org/wiki/Jensen%27s_inequality")
  , ("J\\. ?G\\. ?Ballard", "https://en.wikipedia.org/wiki/J._G._Ballard")
  , ("J\\. ?K\\. ?Rowling", "https://en.wikipedia.org/wiki/J._K._Rowling")
  , ("John D\\. Arnold", "https://en.wikipedia.org/wiki/John_D._Arnold")
  , ("John L\\. Leal", "https://en.wikipedia.org/wiki/John_L._Leal")
  , ("Jorge Luis Borges", "https://en.wikipedia.org/wiki/Jorge_Luis_Borges")
  , ("Juke[Bb]ox", "https://openai.com/blog/jukebox/")
  , ("Julian Simon", "https://en.wikipedia.org/wiki/Julian_Lincoln_Simon")
  , ("[Jj]umping [Ss]piders?", "https://en.wikipedia.org/wiki/Jumping_spider")
  , ("Portia", "https://en.wikipedia.org/wiki/Portia_(spider)")
  , ("Portia fimbriata", "https://en.wikipedia.org/wiki/Portia_fimbriata")
  , ("[Jj]ustified text", "https://en.wikipedia.org/wiki/Typographic_alignment#Justified")
  , ("Kaplan et al 2020", "https://arxiv.org/abs/2001.08361#openai")
  , ("Karras et al 2020", "https://arxiv.org/abs/2006.06676#nvidia")
  , ("Kazutaka Miyatake", "https://en.wikipedia.org/wiki/Kazutaka%20Miyatake")
  , ("Kelly criterion", "https://en.wikipedia.org/wiki/Kelly_criterion")
  , ("Kicks Condor", "https://www.kickscondor.com/")
  , ("Kinect", "https://en.wikipedia.org/wiki/Kinect")
  , ("Kirk Allen", "https://en.wikipedia.org/wiki/Kirk_Allen")
  , ("Krummhörn", "https://en.wikipedia.org/wiki/Krummh%C3%B6rn")
  , ("Lady Jane", "https://en.wikipedia.org/wiki/Lady_Jane_%281986_film%29")
  , ("Laplace approximations?", "https://en.wikipedia.org/wiki/Laplace%27s_method")
  , ("[Ll]atent", "https://en.wikipedia.org/wiki/Latent_variable")
  , ("[Ll]avaan", "http://lavaan.ugent.be/")
  , ("Lawrence Bragg", "https://en.wikipedia.org/wiki/Lawrence_Bragg")
  , ("lbpcascade_animeface", "https://github.com/nagadomi/lbpcascade_animeface")
  , ("LD ?Hub", "http://ldsc.broadinstitute.org/about/")
  , ("Leonard Horner", "https://en.wikipedia.org/wiki/Leonard_Horner")
  , ("Le Roy Ladurie", "https://en.wikipedia.org/wiki/Emmanuel_Le_Roy_Ladurie")
  , ("Less ?Wrong", "https://www.lesswrong.com")
  , ("[Ll]evamisole", "https://en.wikipedia.org/wiki/Levamisole")
  , ("(Lewis Terman|Terman)", "https://en.wikipedia.org/wiki/Lewis_Terman")
  , ("linkchecker", "https://github.com/linkchecker/linkchecker")
  , ("[Ll]iraglutide", "https://en.wikipedia.org/wiki/Liraglutide")
  , ("Lisp [Mm]achines?", "https://en.wikipedia.org/wiki/Lisp_machine")
  , ("[Ll]oad leveling", "https://en.wikipedia.org/wiki/Load_management")
  , ("[Ll]ocus [Cc]oeruleus", "https://en.wikipedia.org/wiki/Locus_coeruleus")
  , ("[Ll]ow [Ll]evel [Ll]aser [Tt]herapy", "https://en.wikipedia.org/wiki/Low_level_laser_therapy")
  , ("LLLT", "https://en.wikipedia.org/wiki/Low_level_laser_therapy")
  , ("On the Nature of Things", "https://en.wikipedia.org/wiki/De_rerum_natura")
  , ("Machiavelli", "https://en.wikipedia.org/wiki/Niccol%C3%B2_Machiavelli")
  , ("Mahiro Maeda", "https://en.wikipedia.org/wiki/Mahiro%20Maeda")
  , ("mathjax-node-page", "https://github.com/pkra/mathjax-node-page/")
  , ("Matthew effects?", "https://en.wikipedia.org/wiki/Matthew_effect")
  , ("McRaven", "https://en.wikipedia.org/wiki/William_H._McRaven")
  , ("(MCTS|Monte Carlo [Tt]ree [Ss]earch)", "https://en.wikipedia.org/wiki/Monte_Carlo_tree_search")
  , ("Meena", "https://arxiv.org/abs/2001.09977#google")
  , ("MegatronLM", "https://nv-adlr.github.io/MegatronLM")
  , ("MEMORIZE", "http://learning.mpi-sws.org/memorize/")
  , ("Merchant fees?", "https://en.wikipedia.org/wiki/Interchange_fee")
  , ("Meta[Mm]ath", "https://en.wikipedia.org/wiki/Metamath")
  , ("[Mm]eta [Pp]seudo [Ll]abels", "https://arxiv.org/abs/2003.10580#google")
  , ("Met HD", "https://en.wikipedia.org/wiki/Metropolitan_Opera_Live_in_HD")
  , ("Michael Nielsen", "http://michaelnielsen.org/")
  , ("Mike Power", "http://mikepower.pressfolios.com/")
  , ("Mind Sparke", "http://www.mindsparke.com/")
  , ("[Mm]inuets?", "https://en.wikipedia.org/wiki/Minuet")
  , ("MLP:?FIM", "https://en.wikipedia.org/wiki/My_Little_Pony:_Friendship_Is_Magic")
  , ("MLP-?Mixer", "https://arxiv.org/abs/2105.01601#google")
  , ("[Mm]odal [Rr]ealism", "https://en.wikipedia.org/wiki/Modal_realism")
  , ("Modern Synthesis", "https://en.wikipedia.org/wiki/Neo-Darwinism")
  , ("Moebius-like", "https://en.wikipedia.org/wiki/Jean_Giraud")
  , ("[Mm]olecular breeding", "https://en.wikipedia.org/wiki/Molecular_breeding")
  , ("Montaillou: The Promised Land of Error", "https://en.wikipedia.org/wiki/Montaillou_(book)")
  , ("(H?P?:?MoR|Methods [Oo]f Rationality)", "http://www.hpmor.com/")
  , ("[Mm]ozjpeg", "https://github.com/mozilla/mozjpeg")
  , ("(COCO|MS[- ]?COCO)", "https://arxiv.org/abs/1405.0312#microsoft")
  , ("MSG-GAN", "https://arxiv.org/abs/1903.06048")
  , ("mT5", "https://arxiv.org/abs/2010.11934#google")
  , ("Mt\\. ?Gox", "https://en.wikipedia.org/wiki/Mt._Gox")
  , ("Muesli", "https://arxiv.org/abs/2104.06159")
  , ("[Mm]ultiple comparisons?", "https://en.wikipedia.org/wiki/Multiple_comparisons_problem")
  , ("[Mm]ultiple discover(y|ies)", "https://en.wikipedia.org/wiki/Multiple_discovery")
  , ("Münster rebellion", "https://en.wikipedia.org/wiki/M%C3%BCnster_rebellion")
  , ("Murphy's [Ll]aw", "https://en.wikipedia.org/wiki/Murphy%27s_law")
  , ("Muse[Nn]et", "https://openai.com/blog/musenet/")
  , ("Music Transformers?", "https://arxiv.org/abs/1809.04281#google")
  , ("Musth", "https://en.wikipedia.org/wiki/Musth")
  , ("Mu[Zz]ero", "https://arxiv.org/abs/1911.08265#deepmind")
  , ("Mu[Zz]ero Unplugged", "https://arxiv.org/abs/2104.06294#deepmind")
  , ("My Little Pony: Friendship is Magic", "https://en.wikipedia.org/wiki/My_Little_Pony:_Friendship_Is_Magic")
  , ("[Mm]yxoma virus", "https://en.wikipedia.org/wiki/Myxoma_virus")
  , ("[Nn][- ]?body problems?", "https://en.wikipedia.org/wiki/N-body_problem")
  , ("Neo-Pop", "https://en.wikipedia.org/wiki/Neo-pop")
  , ("NeRF", "https://arxiv.org/abs/2003.08934")
  , ("Newcomb's Problem", "https://en.wikipedia.org/wiki/Newcomb%27s_paradox")
  , ("Newgrounds", "https://en.wikipedia.org/wiki/Newgrounds")
  , ("[Nn]ucleus [Ss]ampling", "https://arxiv.org/abs/1904.09751")
  , ("NVAE", "https://arxiv.org/abs/2007.03898#nvidia")
  , ("ocrmypdf", "https://github.com/jbarlow83/OCRmyPDF")
  , ("[Oo]ld-style numerals?", "https://en.wikipedia.org/wiki/Text_figures")
  , ("Omni[Nn]et", "https://arxiv.org/abs/2103.01075")
  , ("(OpenAI 5|OA ?5)", "https://openai.com/projects/five/")
  , ("OpenAI API", "https://openai.com/blog/openai-api/")
  , ("OpenAI Gym", "https://github.com/openai/gym")
  , ("[Oo]perant conditioning", "https://en.wikipedia.org/wiki/Operant_conditioning")
  , ("[Oo]ptogenetics?", "https://en.wikipedia.org/wiki/Optogenetics")
  , ("[Oo]utside [Vv]iew", "https://www.lesswrong.com/tag/inside-outside-view")
  , ("Overcoming ?Bias", "https://www.overcomingbias.com/")
  , ("Owarimonogatari", "https://en.wikipedia.org/wiki/List_of_Monogatari_episodes#Owarimonogatari")
  , ("PaintsTransfer/style2paints", "https://github.com/lllyasviel/style2paints")
  , ("Parker-Hulme murder case", "https://en.wikipedia.org/wiki/Parker%E2%80%93Hulme_murder_case")
  , ("[Pp]article filtering", "https://en.wikipedia.org/wiki/Particle_filter")
  , ("[Pp]asteurellosis", "https://en.wikipedia.org/wiki/Pasteurellosis")
  , ("Paul Celan", "https://en.wikipedia.org/wiki/Paul_Celan")
  , ("Paul Ehrlich", "https://en.wikipedia.org/wiki/Paul_R._Ehrlich")
  , ("Paul F?\\.? Christiano", "https://paulfchristiano.com/")
  , ("[Pp]emmican", "https://en.wikipedia.org/wiki/Pemmican")
  , ("[Pp]entobarbital", "https://en.wikipedia.org/wiki/Pentobarbital")
  , ("Performance Curve Database", "http://pcdb.santafe.edu/")
  , ("[Pp]erpetual futures", "https://en.wikipedia.org/wiki/Perpetual_futures")
  , ("Peter Singer", "https://en.wikipedia.org/wiki/Peter_Singer")
  , ("Philip K\\. Dick", "https://en.wikipedia.org/wiki/Philip_K._Dick")
  , ("[Pp]hotoplethysmography", "https://en.wikipedia.org/wiki/Photoplethysmogram")
  , ("[Pp]oint biserial", "https://en.wikipedia.org/wiki/Point-biserial_correlation_coefficient")
  , ("Polderman et al 2015", "/docs/genetics/heritable/2015-polderman.pdf")
  , ("[Pp]olicy [Dd]istillation", "https://arxiv.org/abs/1511.06295")
  , ("popups\\.js", "/static/js/popups.js")
  , ("Practical Typography", "https://practicaltypography.com/")
  , ("Prediction[Bb]ook.com", "https://predictionbook.com/")
  , ("[Pp]redictron", "https://arxiv.org/abs/1612.08810")
  , ("(President|Barack) Obama", "https://en.wikipedia.org/wiki/Barack_Obama")
  , ("ProGANs?", "https://arxiv.org/abs/1710.10196")
  , ("Project Malmo", "https://github.com/microsoft/malmo")
  , ("Puccini", "https://en.wikipedia.org/wiki/Giacomo_Puccini")
  , ("[Pp]unctuated equilibriums?", "https://en.wikipedia.org/wiki/Punctuated_equilibrium")
  , ("Q[ -]factor", "https://en.wikipedia.org/wiki/Q_factor")
  , ("R2D2", "https://openreview.net/forum?id=r1lyTjAqYX#deepmind")
  , ("R\\. ?A\\. ?Fisher", "https://en.wikipedia.org/wiki/Ronald_Fisher")
  , ("R\\. ?A\\. ?Lafferty", "https://en.wikipedia.org/wiki/R._A._Lafferty")
  , ("[Rr]amjet", "https://en.wikipedia.org/wiki/Ramjet")
  , ("Randall Jarrell", "https://en.wikipedia.org/wiki/Randall_Jarrell")
  , ("Ravelry", "https://en.wikipedia.org/wiki/Ravelry")
  , ("\\/r\\/DecisionTheory", "https://old.reddit.com/r/DecisionTheory/")
  , ("REALM", "https://kentonl.com/pub/gltpc.2020.pdf#google")
  , ("Reformer", "https://arxiv.org/abs/2001.04451#google")
  , ("Registered Reports?", "https://en.wikipedia.org/wiki/Preregistration_(science)#Registered_reports")
  , ("RegNet", "https://arxiv.org/abs/2003.13678#facebook")
  , ("[Rr]egression [Dd]iscontinuity", "https://en.wikipedia.org/wiki/Regression_discontinuity_design")
  , ("REINFORCE", "/docs/rl/1992-williams.pdf")
  , ("Res[Nn]e[Xx]t", "https://arxiv.org/abs/1907.07640")
  , ("Re[Zz]ero", "https://arxiv.org/abs/2003.04887")
  , ("[Rr]ice futures market", "https://en.wikipedia.org/wiki/D%C5%8Djima_Rice_Exchange")
  , ("Rietveld et al 2013", "/docs/iq/2013-rietveld.pdf")
  , ("R[Oo]BERT[aA]", "https://arxiv.org/abs/1907.11692#facebook")
  , ("Robert Bakewell", "https://en.wikipedia.org/wiki/Robert_Bakewell_(agriculturalist)")
  , ("[Rr]obots\\.txt", "https://en.wikipedia.org/wiki/Robots_exclusion_standard")
  , ("Rotten\\.com", "https://en.wikipedia.org/wiki/Rotten.com")
  , ("R\\. Scott Bakker", "https://en.wikipedia.org/wiki/R._Scott_Bakker")
  , ("RUDDER", "https://arxiv.org/abs/1806.07857")
  , ("Russian domesticated foxe?s?", "https://en.wikipedia.org/wiki/Domesticated_red_fox")
  , ("SAGAN", "https://arxiv.org/abs/1805.08318")
  , ("Sandia National Laboratories", "https://en.wikipedia.org/wiki/Sandia_National_Laboratories")
  , ("SAT [Ss]olv(ing|ers?)", "https://en.wikipedia.org/wiki/Boolean_satisfiability_problem#Algorithms_for_solving_SAT")
  , ("Schelling point", "https://en.wikipedia.org/wiki/Focal_point_(game_theory)")
  , ("Scholes", "https://en.wikipedia.org/wiki/Myron_Scholes")
  , ("[Ss]cramjet", "https://en.wikipedia.org/wiki/Scramjet")
  , ("[Ss]eam carving", "https://en.wikipedia.org/wiki/Seam_carving")
  , ("[Ss]emaglutide", "https://en.wikipedia.org/wiki/Semaglutide")
  , ("[Ss]enolytics?", "https://en.wikipedia.org/wiki/Senolytic")
  , ("Sestero", "https://en.wikipedia.org/wiki/Greg_Sestero")
  , ("Seymour Cray", "https://en.wikipedia.org/wiki/Seymour_Cray")
  , ("Shawn Presser", "https://nitter.hu/theshawwn")
  , ("Shikishi", "https://en.wikipedia.org/wiki/Princess_Shikishi")
  , ("Shonen Sunday", "https://en.wikipedia.org/wiki/Shonen%20Sunday")
  , ("Shortformer", "https://arxiv.org/abs/2012.15832")
  , ("short[- ]?s(ale|elling)", "https://en.wikipedia.org/wiki/Short_(finance)")
  , ("SimC[Ll][Rr]", "https://arxiv.org/abs/2002.05709#google")
  , ("Simpson's [Pp]aradox", "https://en.wikipedia.org/wiki/Simpson%27s_paradox")
  , ("Single[Ff]ile", "https://github.com/gildas-lormeau/SingleFile/")
  , ("(SMPY|Study [Oo]f Mathematically Precocious Youth)", "/SMPY")
  , ("SMYRF", "https://arxiv.org/abs/2010.05315")
  , ("social[- ]engineering", "https://en.wikipedia.org/wiki/Social_engineering_(security)")
  , ("Source Sans Pro", "https://en.wikipedia.org/wiki/Source_Sans_Pro")
  , ("Source Serif Pro", "https://en.wikipedia.org/wiki/Source_Serif_Pro")
  , ("[Ss]peedrunning", "https://en.wikipedia.org/wiki/Speedrun")
  , ("SR3", "https://arxiv.org/abs/2104.07636#google")
  , ("StackGAN", "https://arxiv.org/abs/1612.03242")
  , ("StackGAN\\+\\+", "https://arxiv.org/abs/1710.10916")
  , ("(WGAN|Wasserstein GAN)s?", "https://arxiv.org/abs/1701.07875")
  , ("(Soft Actor-Critic|SAC)", "https://arxiv.org/abs/1801.01290")
  , ("PlaNet", "https://arxiv.org/abs/1811.04551#google")
  , ("RandAugment", "https://arxiv.org/abs/1909.13719#google")
  , ("(LDSC|LD [Ss]core [Rr]egression)", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4495769/")
  , ("Stephen Schneider", "https://en.wikipedia.org/wiki/Stephen_Schneider")
  , ("Stroop (effect|task)", "https://en.wikipedia.org/wiki/Stroop_effect")
  , ("Style2Paints", "https://github.com/lllyasviel/style2paints")
  , ("StyleGAN2s?", "https://arxiv.org/abs/1912.04958#nvidia")
  , ("StyleGANs?", "https://arxiv.org/abs/1812.04948")
  , ("(StyleGAN3|StyleGAN2-ADA)s?", "https://arxiv.org/abs/2006.06676#nvidia")
  , ("LAMBADA", "https://arxiv.org/abs/1606.06031")
  , ("style[- ]transfers?", "https://arxiv.org/abs/1508.06576")
  , ("[Ss]upercuts?", "https://en.wikipedia.org/wiki/Supercut")
  , ("[Ss]uperflat", "https://en.wikipedia.org/wiki/Superflat")
  , ("Super(GLUE|Glue)", "https://arxiv.org/abs/1905.00537")
  , ("[Ss]uperpressure balloons?", "https://en.wikipedia.org/wiki/Superpressure_balloon")
  , ("Suphx", "https://arxiv.org/abs/2003.13590#msr")
  , ("SWA", "https://arxiv.org/abs/1803.05407")
  , ("Swee[Tt]ango", "https://en.wikipedia.org/wiki/SweeTango")
  , ("Switch Transformers?", "https://arxiv.org/abs/2101.03961#google")
  , ("T5s?", "https://arxiv.org/abs/1910.10683#google")
  , ("textgenrnn", "https://github.com/minimaxir/textgenrnn")
  , ("The Browser", "https://thebrowser.com/")
  , ("The Pile", "https://arxiv.org/abs/2101.00027")
  , ("(TADNE|This Anime Does Not Exist\\.?a?i?)", "https://thisanimedoesnotexist.ai/")
  , ("(TFDNE|This Fursona Does Not Exist)", "https://www.thisfursonadoesnotexist.com")
  , ("(TPDNE|This Pony Does Not Exist)", "https://thisponydoesnotexist.net/")
  , ("(TWDNE|TWDNEv2|This Waifu Does Not Exist|This ?Waifu ?Does ?Not ?Exist(\\.net)?)", "https://www.thiswaifudoesnotexist.net/")
  , ("Thompson [Ss]ampling", "https://en.wikipedia.org/wiki/Thompson_sampling")
  , ("Timothy C\\. May", "https://en.wikipedia.org/wiki/Timothy_C._May")
  , ("Tim Powers", "https://en.wikipedia.org/wiki/Tim_Powers")
  , ("TinyBERT", "https://arxiv.org/abs/1909.10351")
  , ("[Tt]okusatsu", "https://en.wikipedia.org/wiki/Tokusatsu")
  , ("Tommy Wiseau", "https://en.wikipedia.org/wiki/Tommy_Wiseau")
  , ("Tom Wolfe", "https://en.wikipedia.org/wiki/Tom_Wolfe")
  , ("torch-rnn", "https://github.com/jcjohnson/torch-rnn")
  , ("[Tt]orsion balance", "https://en.wikipedia.org/wiki/Torsion_spring#Torsion_balance")
  , ("[Tt]ragedy of the anticommons", "https://en.wikipedia.org/wiki/Tragedy_of_the_anticommons")
  , ("[Tt]ransfer RNAs?", "https://en.wikipedia.org/wiki/Transfer_RNA")
  , ("Transformers?", "https://arxiv.org/abs/1706.03762#google")
  , ("Transformer-XLs?", "https://arxiv.org/abs/1901.02860")
  , ("TransGAN", "https://arxiv.org/abs/2102.07074")
  , ("TF?RC", "https://sites.research.google/trc/")
  , ("[Tt]ree induction", "https://en.wikipedia.org/wiki/Decision_tree_learning")
  , ("TreeQN", "https://arxiv.org/abs/1710.11417")
  , ("Trithemius", "https://en.wikipedia.org/wiki/Johannes_Trithemius")
  , ("[Tt]rolley problem", "https://en.wikipedia.org/wiki/Trolley_problem")
  , ("[Tt]rophic level", "https://en.wikipedia.org/wiki/Trophic_level")
  , ("Tufte[- ]CSS", "https://edwardtufte.github.io/tufte-css/")
  , ("Uber", "https://en.wikipedia.org/wiki/Uber")
  , ("uBlock [Oo]rigin", "https://github.com/gorhill/uBlock")
  , ("UCI [Rr]epository", "https://en.wikipedia.org/wiki/University_of_California,_Irvine#Machine_Learning_Repository")
  , ("Universal Transformers?", "https://arxiv.org/abs/1807.03819#googledeepmind")
  , ("[Uu]nlikelihood training", "https://arxiv.org/abs/1908.04319")
  , ("Unsong", "https://unsongbook.com/")
  , ("V100", "https://en.wikipedia.org/wiki/Volta_(microarchitecture)#Products")
  , ("[Vv]alue [Ii]teration [Nn]etworks?", "https://arxiv.org/abs/1602.02867")
  , ("VideoGPT", "https://arxiv.org/abs/2104.10157")
  , ("(Vision Transformers?|ViT)", "https://openreview.net/forum?id=YicbFdNTTy#google")
  , ("Vi[Zz][Dd]oom", "https://arxiv.org/abs/1605.02097")
  , ("VQ-VAE", "https://arxiv.org/abs/1906.00446#deepmind")
  , ("waifu2x", "https://github.com/nagadomi/waifu2x")
  , ("Waifu Labs", "https://waifulabs.com/")
  , ("Walt Disney", "https://en.wikipedia.org/wiki/Walt_Disney")
  , ("Warren Buffett", "https://en.wikipedia.org/wiki/Warren_Buffett")
  , ("Wasserstein GAN", "https://arxiv.org/abs/1701.07875")
  , ("[Ww]ater chlorination", "https://en.wikipedia.org/wiki/Water_chlorination")
  , ("Web[Vv]ision", "https://arxiv.org/abs/1708.02862")
  , ("Wen[Ll]an", "https://arxiv.org/abs/2103.06561")
  , ("[Ww]hole-exome sequencing", "https://en.wikipedia.org/wiki/Exome_sequencing")
  , ("[Ww]ikipedia-popups.js", "https://share.obormot.net/misc/gwern/wikipedia-popups.js")
  , ("WISC(-I|-II|-III|-IV|-V|-VI)?", "https://en.wikipedia.org/wiki/Wechsler_Intelligence_Scale_for_Children")
  , ("Worm", "http://parahumans.wordpress.com/category/stories-arcs-1-10/arc-1-gestation/1-01/")
  , ("XLM-R", "https://arxiv.org/abs/1911.02116#facebook")
  , ("XL[Nn]et", "https://arxiv.org/abs/1906.08237")
  , ("XMC-GAN", "https://arxiv.org/abs/2101.04702#google")
  , ("Your[Mm]orals\\.org", "http://yourmorals.org/")
  , ("[Bb]lessings [Oo]f [Ss]cale", "/Scaling-hypothesis#blessings-of-scale")
  , ("[Ss]caling [Hh]ypothesis", "/Scaling-hypothesis")
  , ("[Ss]caling [Ll]aws?", "/notes/Scaling")
  , ("[Cc]ommoditize [Yy]our [Cc]omplement", "/Complement")
  , ("[Vv]alue-[Ee]quivalence", "https://arxiv.org/abs/2011.03506#deepmind")
  , ("DistilBERT", "https://arxiv.org/abs/1910.01108")
  , ("Internet Archive", "https://en.wikipedia.org/wiki/Internet_Archive")
  , ("[Oo]rphan [Ww]ork", "https://en.wikipedia.org/wiki/Orphan_work")
  , ("[Pp]ublic [Dd]omain", "https://en.wikipedia.org/wiki/Public_domain")
  , ("[Dd]emand [Cc]haracteristics", "https://en.wikipedia.org/wiki/Demand_characteristics")
  , ("[Ee]pistasis", "https://en.wikipedia.org/wiki/Epistasis")
  , ("Wisconsin Longitudinal Study", "https://www.ssc.wisc.edu/wlsresearch/about/description.php")
  , ("[Cc]ommon ?[Cc]rawl", "https://en.wikipedia.org/wiki/Common_Crawl")
  , ("wav2vec 2\\.0", "https://arxiv.org/abs/2006.11477#facebook")
  , ("Alpha ?Go", "https://en.wikipedia.org/wiki/AlphaGo")
  , ("(Alpha ?Zero|Alpha0)", "/docs/rl/2018-silver.pdf#deepmind")
  , ("AlphaGo Master", "https://en.wikipedia.org/wiki/Master_(software)")
  , ("Libri-Light", "https://arxiv.org/abs/1912.07875#facebook")
  , ("BLEU-?[0-9]?", "https://en.wikipedia.org/wiki/BLEU")
  , ("ROUGE", "https://en.wikipedia.org/wiki/ROUGE_(metric)")
  , ("[Ll]ithium", "https://en.wikipedia.org/wiki/Lithium")
  , ("[Ll]lithium [Oo]rotate", "https://en.wikipedia.org/wiki/Lithium_orotate")
  , ("([Ar]modafinil|[Mm]odafinil)", "/Modafinil")
  , ("[Nn]icotine", "/Nicotine")
  , ("Zeo", "/Zeo")
  , ("[Ff]ast Fourier [Tt]ransform", "https://en.wikipedia.org/wiki/Fast_Fourier_transform")
  , ("(MCMC|[Mm]arkov [Cc]hain [Mm]onte [Cc]arlo)", "https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo")
  , ("Monte Carlo algorithm", "https://en.wikipedia.org/wiki/Monte_Carlo_algorithm")
  , ("Monte Carlo (simulates?|estimates?|simulations?|approximations?|implementations?|methods?)?", "https://en.wikipedia.org/wiki/Monte_Carlo_method")
  , ("(Hamiltonian Monte Carlo|[Hh]ybrid Monte Carlo)", "https://en.wikipedia.org/wiki/Hamiltonian_Monte_Carlo")
  , ("AIXI", "https://www.lesswrong.com/tag/aixi")
  , ("([Bb]andit sampling|[Bb]andit models?|[Mm]ulti-[Aa]rm?e?d [Bb]andit)", "https://en.wikipedia.org/wiki/Multi-armed_bandit")
  , ("[Rr]andom [Ff]orests?", "https://en.wikipedia.org/wiki/Random_forest")
  , ("(SVM|[Ss]upport [Vv]ector [Mm]achines?)", "https://en.wikipedia.org/wiki/Support-vector_machine")
  , ("(LSTM|Long short-term memory)", "https://en.wikipedia.org/wiki/Long_short-term_memory")
  , ("(RNN|[Rr]ecurrent [Nn]eural [Nn]etwork|[Rr]ecurrent network)", "https://en.wikipedia.org/wiki/Recurrent_neural_network")
  , ("[Rr]einforcement [Ll]earning", "https://en.wikipedia.org/wiki/Reinforcement_learning")
  , ("(CNN|[Cc]onvolutional [Nn]eural [Nn]etwork)", "https://en.wikipedia.org/wiki/Convolutional_neural_network")
  , ("Hans Moravec", "https://en.wikipedia.org/wiki/Hans_Moravec")
  , ("Moravec's paradox", "https://en.wikipedia.org/wiki/Moravec%27s_paradox")
  , ("Conceptual Captions", "/docs/ai/2018-sharma.pdf#google")
  , ("DeepSpeed", "https://github.com/microsoft/DeepSpeed")
  , ("TPU-?v3s?", "https://en.wikipedia.org/wiki/Tensor_Processing_Unit#Third_generation_TPU")
  , ("TPU-?v4s?", "https://en.wikipedia.org/wiki/Tensor_Processing_Unit#Fourth_generation_TPU")
  , ("TPU-?v2s?", "https://en.wikipedia.org/wiki/Tensor_Processing_Unit#Second_generation_TPU")
  , ("TPUs?", "/docs/ai/2020-jouppi.pdf#google")
  , ("VGG(-?16)", "https://arxiv.org/abs/1409.1556")
  , ("(ResNet-(18|34|50|101|152)|[Rr]es[Nn]et|[Rr]esidual[ -][Nn]etwork)s?", "https://arxiv.org/abs/1512.03385")
  , ("[Bb]atch-?[Nn]orm(alization)?", "https://en.wikipedia.org/wiki/Batch_normalization")
  , ("[Bb]ack ?prop(agation)?", "https://en.wikipedia.org/wiki/Backpropagation")
  , ("(SGD|[Ss]tochastic [Gg]radient [Dd]escent)", "https://en.wikipedia.org/wiki/Stochastic_gradient_descent")
  , ("[Dd]ifferentiable", "https://en.wikipedia.org/wiki/Differentiable_function")
  , ("[Oo]ptimal stopping", "https://en.wikipedia.org/wiki/Optimal_stopping")
  , ("([Pp]olygenic [Ss]core|PGS|[Pp]olygenic [Rr]isk [Ss]core|PRS|[Gg]enetic [Rr]isk [Ss]core|GRS|[Gg]enome-[Ww]ide [Ss]core|[Pp]olygenic [Ii]ndex|PGI)", "https://en.wikipedia.org/wiki/Polygenic_score")
  , ("[Cc]ase.?[Cc]ontrol", "https://en.wikipedia.org/wiki/Case%E2%80%93control_study")
  , ("(PTSD|[Pp]ost.?traumatic stress disorder)", "https://en.wikipedia.org/wiki/Post-traumatic_stress_disorder")
  , ("(W?GWAS?|[Gg]enome-[Ww]ide [Aa]ssociation [Aa]nalys(is|e|es)|[Gg]enome-[Ww]ide [Aa]ssociation [Ss]tud(y|ies))", "https://en.wikipedia.org/wiki/Genome-wide_association_study")
  , ("Million Veteran Program", "https://www.research.va.gov/mvp/")
  , ("23[A]nd[Mm]e", "https://en.wikipedia.org/wiki/23andMe")
  , ("(EHR|[Ee]lectronic [Hh]ealth [Rr]ecords?)", "https://en.wikipedia.org/wiki/Electronic_health_record")
  , ("(GSEM|[Gg]enomic SEM|[Gg]enomic [Ss]tructural [Ee]quation [Mm]odeling)", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6520146/")
  , ("(Deep[Mm]ind.?Lab|DM[Ll]ab-30|DM[Ll]ab)", "https://arxiv.org/abs/1612.03801")
  , ("[Dd]endritic spines?", "https://en.wikipedia.org/wiki/Dendritic_spine")
  , ("[Cc]ontrastive", "https://arxiv.org/abs/2010.05113")
  , ("[Dd]ouble descent", "https://openai.com/blog/deep-double-descent/")
  , ("[Ff]entanyl", "https://en.wikipedia.org/wiki/Fentanyl")
  , ("[Cc]arfentanil", "https://en.wikipedia.org/wiki/Carfentanil")
  , ("[Oo]pioids?", "https://en.wikipedia.org/wiki/Opioid")
  , ("(PBT|[Pp]opulation[ -][Bb]ased [Tt]raining|population[ -]based (deep reinforcement)? ?learning)", "https://science.sciencemag.org/content/364/6443/859#deepmind")
  , ("[Ss]enescen(ce|t).?", "https://en.wikipedia.org/wiki/Cellular_senescence")
  , ("[Ss]enolytic.?", "https://en.wikipedia.org/wiki/Senolytic")
  , ("Scott Alexander", "https://astralcodexten.substack.com/")
  , ("Gaussian process", "https://en.wikipedia.org/wiki/Gaussian_process")
  , ("OpenAI", "https://en.wikipedia.org/wiki/OpenAI")
  , ("SPIRAL", "https://arxiv.org/abs/1804.01118#deepmind")
  , ("([Pp]rompt programming|[Pp]rompt engineering)", "/GPT-3#prompt-programming")
  , ("(Hans J\\. Eysenck|Hans Jürgen Eysenck|Hans Eysenck|Eysenck[ian]?)", "https://en.wikipedia.org/wiki/Hans_Eysenck")
  , ("(ADHD|[Aa]ttention[ -][Dd]eficit [Hh]yperactivity [Dd]isorder)s?", "https://en.wikipedia.org/wiki/Attention_deficit_hyperactivity_disorder")
  , ("(SCZ|[Ss]chizophreni[ac]s?)", "https://en.wikipedia.org/wiki/Schizophrenia")
  , ("[Ee]verything [Ii]s [Cc]orrelated", "/Everything")
  , ("(AFQT|ASVAB|Armed Forces Qualification Test|Armed Services Vocational Aptitude Battery)", "https://en.wikipedia.org/wiki/Armed_Services_Vocational_Aptitude_Battery")
  , ("Project 100,000", "https://en.wikipedia.org/wiki/Project_100,000")
  , ("Kaplan-Meier", "https://en.wikipedia.org/wiki/Kaplan%E2%80%93Meier_estimator")
  , ("inbreeding depression", "https://en.wikipedia.org/wiki/Inbreeding_depression")
  , ("PILCO", "/docs/rl/2011-deisenroth.pdf")
  , ("DDPG", "https://arxiv.org/abs/1509.02971#deepmind")
  , ("(PPO|[Pp]roximal [Pp]olicy [Oo]ptimization)", "https://arxiv.org/abs/1707.06347#openai")
  , ("(A2C|A3C|[Aa]synchronous [Aa]dvantage [Aa]ctor-[Cc]ritic)", "https://arxiv.org/abs/1602.01783#deepmind")
  , ("Rainbow", "https://arxiv.org/abs/1710.02298#deepmind")
  , ("D4PG", "https://arxiv.org/abs/1804.08617#deepmind")
  , ("([Dd]ark [Nn]et [Mm]arket|[Dd]arknet [Mm]arket|DNM|[Cc]ryptomarket|[Cc]rypto-[Mm]arket)", "https://en.wikipedia.org/wiki/Darknet_market")
  , ("[Mm]aximum [Ll]ikelihood", "https://en.wikipedia.org/wiki/Maximum_likelihood_estimation")
  , ("BYOL", "https://arxiv.org/abs/2006.07733#deepmind")
  , ("SimSiam", "https://arxiv.org/abs/2011.10566#facebook")
  , ("([Ss]elf-[Ss]upervised [Ll]earning|[Ss]emi-[Ss]upervised [Ll]earning)", "https://en.wikipedia.org/wiki/Semi-supervised_learning")
  , ("([Ee]xponential [Mm]oving [Aa]verages?|EMA)", "https://arxiv.org/abs/1806.04498")
  , ("[Ww]eight [Dd]ecay", "https://en.wikipedia.org/wiki/Tikhonov_regularization")
  , ("STL-10", "https://cs.stanford.edu/~acoates/stl10/")
  , ("(John Tukey|John W\\. Tukey|Tukey)", "https://en.wikipedia.org/wiki/John_Tukey")
  , ("[Ss]chizotyp(y|ical)", "https://en.wikipedia.org/wiki/Schizotypy")
  , ("PNSR", "https://en.wikipedia.org/wiki/Peak_signal-to-noise_ratio")
  , ("SSIM", "https://en.wikipedia.org/wiki/Structural_similarity")
  , ("(Fr[ée]chet inception distance|FID)", "https://en.wikipedia.org/wiki/Fr%C3%A9chet_inception_distance")
  , ("AMD", "https://en.wikipedia.org/wiki/Advanced_Micro_Devices")
  , ("TSMC", "https://en.wikipedia.org/wiki/TSMC")
  , ("Intel", "https://en.wikipedia.org/wiki/Intel")
  , ("[Mm]etformin", "https://en.wikipedia.org/wiki/Metformin")
  , ("([Ss]tructural [Ee]quation [Mm]odel(s|ing)?|SEM)s?", "https://en.wikipedia.org/wiki/Structural_equation_modeling") -- SEM can also refer to 'scanning electron microscope' or 'standard error of the mean' in Gwern.net content, but in practice, those uses seem far rarer
  , ("(Fermi [Pp]roblem.?|Fermi [Qq]uestion.?)", "https://en.wikipedia.org/wiki/Fermi_problem")
  , ("(CBT|[Cc]ognitive[ -][Bb]ehaviou?r(al)? [Tt]herap(y|ies))", "https://en.wikipedia.org/wiki/Cognitive_behavioral_therapy")
  ] :: [(T.Text,T.Text)] )
