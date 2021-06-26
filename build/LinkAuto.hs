{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module LinkAuto (linkAuto, testDoc) where

-- LinkAuto.hs: search a Pandoc document for pre-defined regexp patterns, and turn matching text into a hyperlink.
--
-- This is useful for automatically defining concepts, terms, and proper names using a single master updated list of regexp/URL pairs.
-- (Terms like "BERT" or "GPT-3" or "RoBERTa" are too hard to all define manually on every appearance, particularly in abstracts/annotations which themselves may be generated automatically, so it makes more sense to try to do it automatically.)
--
-- Regexps are guarded with space/punctuation/string-end-begin delimiters, to try to avoid problems of greedy rewrites (eg "GAN" vs "BigGAN").
-- Regexps are sorted by length, longest-first, to further try to prioritize (so "BigGAN" would match before "GAN").
-- For efficiency, we avoid String type conversion as much as possible.
--
-- A document is queried for URLs and all URLs already present or regexps without plain text matches are removed from the rewrite dictionary.
-- This usually lets a document be skipped entirely as having no possible non-redundant matches.
--
-- Then, we walk the AST, running each remaining regexp against Str nodes.
-- When there is a match, the matching substring is then rewritten to be a Link with the URL & class `link-auto` for the first regexp that matches.
--
-- After the regexp pass, we do an additional cleanup pass. How should we handle the case of a phrase like "GAN" or "GPT-3" appearing potentially scores or hundreds of times in page? Do we really want to  hyperlink *all* of them? Probably not.
-- For the cleanup pass, we track 'seen' `link-auto` links in a Set, and if a link has been seen before, we remove it.
-- (In the future, we may drop this clean up pass, if we can find a good way to dynamically hide 'excess' links; one idea is define `.link-auto` CSS to de-style links, and then, on browser screen scroll, use JS to re-link-style the first instance of each URL. So only the first instance would be visible on each screen, minimizing redundancy/clutter/over-linking.)
--
-- Dependencies: Pandoc, regex-tdfa

import Data.List (nub, sortBy)
import Text.Pandoc -- (queryWith, nullMeta, Pandoc(..), Block(Para), Inline(Link,Image,Code,Space,Span,Str))
import Text.Pandoc.Walk (walkM, walk)
import Text.Regex.TDFA as R (makeRegex, match, matchTest, Regex) -- regex-tdfa supports `(T.Text,T.Text,T.Text)` instance, to avoid packing/unpacking String matches; it is maybe 4x slower than pcre-heavy, but should have fewer Unicode & correctness issues (native Text, and useful splitting), so to save my sanity... BUG: TDFA seems to have bad Text instances: https://github.com/haskell-hvr/regex-tdfa/issues/9
import qualified Data.Set as S (empty, fromList, insert, member, Set)
import Control.Monad.State (evalState, get, put, State)
import qualified Data.Text as T (append, head, length, last, strip, Text)
import Debug.Trace as Trace

import Columns (simplifiedDoc)

test,test2 :: [Inline]
-- test3 = [Link ("",[],[]) [Quoted DoubleQuote [Str "Self-improving",Space,Str "reactive",Space,Str "agents",Space,Str "based",Space,Str "on",Space,Str "reinforcement",Space,Str "learning,",Space,Str "planning",Space,Str "and",Space,Str "teaching"]] ("http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.75.7884&rep=rep1&type=pdf",""),Str ",",Space,Str "Lin",Space,Str "1992"]
test2 = [Str "It's a dilemma: at small or easy domains, StyleGAN is much faster (if not better); but at large or hard domains, mode collapse is too risky and endangers the big investment necessary to surpass StyleGAN. MuZero vs Muesli."]
test = [Str "bigGAN means", Space, Str "BIG", Str "GAN; you will have an easier time training a GAN on a good GPU like a P100 or a TPUv3.", Space, Str "(See",Space,Str "WP",Space,Str "on",Space,Link ("",[],[]) [Link ("",[],[]) [Str "GAN"] ("https://en.wikipedia.org/wiki/Generative_adversarial_network","")] ("https://en.wikipedia.org/wiki/Generative_adversarial_network",""),Str ")", Space, Str "Nevertheless, expensive is a GAN. See Barack Obama's presidency. Still, we shouldn't put too much weight on Barack Obama. More efficient is DistilBERT, not to be confused with", Space, Str "BERT", Str "."]
testDoc :: Pandoc
testDoc = let doc = Pandoc nullMeta [Para test] in
            linkAuto doc

-----------

-- turn first instance of a list of regex matches into hyperlinks in a Pandoc document. NOTE: this is best run as early as possible, because it is doing raw string matching, and any formatting or changing of phrases may break a match.
linkAuto :: Pandoc -> Pandoc
linkAuto p = -- Trace.trace ("Doc") $ walk (defineLinks customDefinitions) p
  let customDefinitions' = filterMatches p $ filterDefinitions p customDefinitions in
                -- if null customDefinitions' then p else Trace.trace (show $ map (\(a,_,_) ->a) customDefinitions') $ annotateFirstDefinitions $ walk (defineLinks customDefinitions') p
    if null customDefinitions' then p else cleanupNestedLinks $ annotateFirstDefinitions $ walk (defineLinks customDefinitions') p

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
                     -- NOTE: our regexps must delimit on space, which puts the matched space inside `matched` instead of `before`/`after`;
                     -- unfortunately, if we move the Space inside the Link, this will look bad when Links get their underlining decoration
                     -- in-browser. So we do this song & dance to figure out if the link was *before* or *after*, remove it from the Link,
                     -- and stick a prefix or suffix replacement Space.
                     Just (before,matched,after, defn) ->
                       go (Str before) ++ -- NOTE: we need to recurse *before* as well after, because 'findRegexMatch' short-circuits on the first match
                                          -- but there may be a later regexp which would match somewhere in the prefix.
                        (if T.head matched == ' ' then
                                        if T.last matched == ' ' then
                                          [Space, Link ("",["link-auto"],[]) [Str $ T.strip matched] (defn, ""), Space] else
                                          [Space, Link ("",["link-auto"],[]) [Str $ T.strip matched] (defn, "")]
                                      else if T.last matched == ' ' then
                                             [Link ("",["link-auto"],[]) [Str $ T.strip matched] (defn, ""), Space]
                                           else
                                             [Link ("",["link-auto"],[]) [Str matched] (defn, "")])
                       ++ go (Str after)
   go x          = [x]

-- Recurse through the dictionary (which should be long-first) to find the first matching regexp, since the master regexp blob matched the string.
findRegexMatch :: [(T.Text, R.Regex, T.Text)] -> T.Text -> Maybe (T.Text, T.Text, T.Text, T.Text)
findRegexMatch [] _ = Nothing
findRegexMatch y@((_,r,u):rs) s = let (a,b,c) = R.match r s in
                                   if b/="" then Just (a,b,c,u) else findRegexMatch rs s

-- Pandoc breaks up strings as much as possible, like [Str "ABC", Space, "notation"], which makes it impossible to match on them, so we remove Space
mergeSpaces :: [Inline] -> [Inline]
mergeSpaces []                     = []
mergeSpaces (Str x:Str y:xs)       = mergeSpaces ([Str (x`T.append`y)] ++ xs)
mergeSpaces (Space:Str x:Space:xs) = mergeSpaces (Str (" "`T.append`x`T.append`" "):xs)
mergeSpaces (Space:Str x:xs)       = mergeSpaces (Str (" "`T.append`x):xs)
mergeSpaces (Str x:Space:xs)       = mergeSpaces (Str (x`T.append`" "):xs)
mergeSpaces (Str "":xs)            = mergeSpaces xs
-- ???
-- mergeSpaces ((Quoted DoubleQuote x) : xs) = [Str "“"] ++ mergeSpaces x ++ [Str "”"]  ++ mergeSpaces xs
-- mergeSpaces ((Quoted SingleQuote x) : xs) = [Str "‘"] ++ mergeSpaces x ++ [Str "’"]  ++ mergeSpaces xs
mergeSpaces (x:xs)                 = x:mergeSpaces xs

-- Optimization: take a set of definitions, and a document; query document for existing URLs; if a URL is already present, drop it from the definition list.
-- This avoids redundancy with links added by hand or other filters.
filterDefinitions :: Pandoc -> [(T.Text, R.Regex, T.Text)] -> [(T.Text, R.Regex, T.Text)]
filterDefinitions (Pandoc _ markdown) = let allLinks = S.fromList $ queryWith extractLink markdown in
                                          filter (\(_,_,linkTarget) -> linkTarget `notElem` allLinks)
  where
   extractLink :: Inline -> [T.Text]
   extractLink (Link _ _ (path, _)) = [path]
   extractLink _ = []

-- Optimization: try to prune a set of definitions and a document. Convert document to plain text, and do a global search; if a regexp matches the plain text, it may or may not match the AST, but if it does not match the plain text, it should never match the AST?
-- Since generally <1% of regexps will match anywhere in the document, doing a single global check lets us discard that regexp completely, and not check at every node. So we can trade off doing 𝑂(R × Nodes) regexp checks for doing 𝑂(R + Nodes) plus compiling to plain, which in practice turns out to be a *huge* performance gain (>30×?) here.
filterMatches :: Pandoc -> [(T.Text, R.Regex, T.Text)] -> [(T.Text, R.Regex, T.Text)]
filterMatches p definitions  = let plain = simplifiedDoc p
  in filter (\(_,b,_) -> matchTest b plain) definitions

-- We want to match our given regexps by making them 'word-level' and matching on puncutation/whitespace delimiters. This avoids subword matches, for example, matching 'GAN' in 'StyleGAN' is undesirable.
customDefinitionsR :: [(T.Text, T.Text)] -> [(T.Text, R.Regex, T.Text)]
customDefinitionsR = map (\(a,b) -> (a,
                                      R.makeRegex $ "[[:punct:][:blank:]]"`T.append`a`T.append`"[[:punct:][:blank:]]",
                                      b))

-----------

-- Create sorted (by length) list of (string/compiled-regexp/substitution) tuples.
-- This can be filtered on the third value to remove redundant matches, and the first value can be concatenated into a single master regexp.
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
  , ("assurance contract", "https://en.wikipedia.org/wiki/Assurance_contract")
  , ("ATreeC", "https://arxiv.org/abs/1710.11417")
  , ("Averroistic", "https://en.wikipedia.org/wiki/Averroes")
  , ("Bandai", "https://en.wikipedia.org/wiki/Bandai")
  , ("Barack Obama", "https://en.wikipedia.org/wiki/Barack_Obama")
  , ("Barlow Twins?", "https://arxiv.org/abs/2103.03230#facebook")
  , ("BART", "https://arxiv.org/abs/1910.13461#facebook")
  , ("Baskerville", "https://en.wikipedia.org/wiki/Baskerville")
  , ("bat-borne viruse?s?", "https://en.wikipedia.org/wiki/Bat-borne_virus")
  , ("Berkson's paradox", "https://en.wikipedia.org/wiki/Berkson%27s_paradox")
  , ("Berlin Alexanderplatz", "https://en.wikipedia.org/wiki/Berlin_Alexanderplatz_station")
  , ("BERT", "https://arxiv.org/abs/1810.04805#google")
  , ("B-heaps?", "https://en.wikipedia.org/wiki/B-heap")
  , ("Bias in Mental Testing", "https://en.wikipedia.org/wiki/Bias_in_Mental_Testing")
  , ("BigBird", "https://arxiv.org/abs/2007.14062#google")
  , ("(Big [Ff]ive|OCEAN)", "https://en.wikipedia.org/wiki/Big_Five_personality_traits")
  , ("BigGAN", "https://arxiv.org/abs/1809.11096")
  , ("[Bb]irthday paradoxe?s?", "https://en.wikipedia.org/wiki/Birthday_problem")
  , ("BiT", "https://arxiv.org/abs/1912.11370#google")
  , ("[Bb]itter [Ll]essons?", "http://www.incompleteideas.net/IncIdeas/BitterLesson.html")
  , ("Blair Braverman", "https://en.wikipedia.org/wiki/Blair_Braverman")
  , ("Blender", "https://arxiv.org/abs/2004.13637#facebook")
  , ("[Bb]ody double", "https://en.wikipedia.org/wiki/Political_decoy")
  , ("Book of Job", "https://en.wikipedia.org/wiki/Book_of_Job")
  , ("bottlenose dolphins?", "https://en.wikipedia.org/wiki/Bottlenose_dolphin")
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
  , ("Carreyrou", "https://en.wikipedia.org/wiki/John_Carreyrou")
  , ("Catherynne M. Valente", "https://en.wikipedia.org/wiki/Catherynne_M._Valente")
  , ("CC-12M", "https://arxiv.org/abs/2102.08981#google")
  , ("[Cc]eiling effects?", "https://en.wikipedia.org/wiki/Ceiling_effect_(statistics)")
  , ("[Cc]erebral cortexe?s?", "https://en.wikipedia.org/wiki/Cerebral_cortex")
  , ("[Cc]haracter designs?", "https://en.wikipedia.org/wiki/Miss_Kobayashi%27s_Dragon_Maid")
  , ("Chiba, Japan", "https://en.wikipedia.org/wiki/Chiba_(city)")
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
  , ("DDPM", "https://arxiv.org/abs/2006.11239")
  , ("DeBERTa", "https://arxiv.org/abs/2006.03654#microsoft")
  , ("Decision Transformer", "https://sites.google.com/berkeley.edu/decision-transformer")
  , ("[Dd]eep brain stimulation", "https://en.wikipedia.org/wiki/Deep_brain_stimulation")
  , ("DeepDanbooru", "https://github.com/KichangKim/DeepDanbooru")
  , ("Deep TAMER", "https://arxiv.org/abs/1709.10163")
  , ("Deep Voice 2", "https://arxiv.org/abs/1705.08947")
  , ("[Dd]efault [Mm]ode [Nn]etwork", "https://en.wikipedia.org/wiki/Default_mode_network")
  , ("del\\.icio\\.us", "https://en.wikipedia.org/wiki/Delicious_(website)")
  , ("Demis Hassabis", "https://en.wikipedia.org/wiki/Demis_Hassabis")
  , ("development of the Marcellus Shale", "https://en.wikipedia.org/wiki/Marcellus_natural_gas_trend")
  , ("DFAs?", "https://arxiv.org/abs/1609.01596")
  , ("Dikötter", "https://en.wikipedia.org/wiki/Frank_Dik%C3%B6tter")
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
  , ("Explosions in the Sky", "https://en.wikipedia.org/wiki/Explosions_in_the_Sky")
  , ("FAVOR\\+", "https://arxiv.org/abs/2009.14794#google")
  , ("feline facial pheromone", "https://en.wikipedia.org/wiki/Cat_pheromone#Feline_facial_pheromone")
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
  , ("GAN", "https://en.wikipedia.org/wiki/Generative_adversarial_network")
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
  , ("Go[- ]?Explore", "https://arxiv.org/abs/1901.10995")
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
  , ("[Hh]idden-variable theories", "https://en.wikipedia.org/wiki/Hidden-variable_theory")
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
  , ("HyperNetworks", "https://arxiv.org/abs/1609.09106#google")
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
  , ("Judy Sheindlin", "https://en.wikipedia.org/wiki/Judy_Sheindlin")
  , ("Juke[Bb]ox", "https://openai.com/blog/jukebox/")
  , ("Julian Simon", "https://en.wikipedia.org/wiki/Julian_Lincoln_Simon")
  , ("[Jj]umping [Ss]piders?", "https://en.wikipedia.org/wiki/Jumping_spider")
  , ("[Jj]ustified text", "https://en.wikipedia.org/wiki/Typographic_alignment#Justified")
  , ("Kaplan et al 2020", "https://arxiv.org/abs/2001.08361#openai")
  , ("Karras et al 2020", "https://arxiv.org/abs/2006.06676#nvidia")
  , ("Kazutaka Miyatake", "https://en.wikipedia.org/wiki/Kazutaka%20Miyatake")
  , ("Kelly criterion", "https://en.wikipedia.org/wiki/Kelly_criterion")
  , ("Kicks Condor", "https://www.kickscondor.com/")
  , ("Kinect", "https://en.wikipedia.org/wiki/Kinect")
  , ("Kinki University", "https://en.wikipedia.org/wiki/Kinki%20University")
  , ("Kirk Allen", "https://en.wikipedia.org/wiki/Kirk_Allen")
  , ("Krummhörn", "https://en.wikipedia.org/wiki/Krummh%C3%B6rn")
  , ("Lady Jane", "https://en.wikipedia.org/wiki/Lady_Jane_%281986_film%29")
  , ("Laplace approximation", "https://en.wikipedia.org/wiki/Laplace%27s_method")
  , ("[Ll]atent", "https://en.wikipedia.org/wiki/Latent_variable")
  , ("[Ll]avaan", "http://lavaan.ugent.be/")
  , ("Lawrence Bragg", "https://en.wikipedia.org/wiki/Lawrence_Bragg")
  , ("lbpcascade_animeface", "https://github.com/nagadomi/lbpcascade_animeface")
  , ("LD ?Hub", "http://ldsc.broadinstitute.org/about/")
  , ("Leonard Horner", "https://en.wikipedia.org/wiki/Leonard_Horner")
  , ("Le Roy Ladurie", "https://en.wikipedia.org/wiki/Emmanuel_Le_Roy_Ladurie")
  , ("Less ?Wrong", "https://www.lesswrong.com/")
  , ("[Ll]evamisole", "https://en.wikipedia.org/wiki/Levamisole")
  , ("Lewis Terman", "https://en.wikipedia.org/wiki/Lewis_Terman")
  , ("linkchecker", "https://github.com/linkchecker/linkchecker")
  , ("[Ll]iraglutide", "https://en.wikipedia.org/wiki/Liraglutide")
  , ("Lisp [Mm]achines?", "https://en.wikipedia.org/wiki/Lisp_machine")
  , ("[Ll]ithium", "https://en.wikipedia.org/wiki/Lithium_(medication)")
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
  , ("MCTS", "https://en.wikipedia.org/wiki/Monte_Carlo_tree_search")
  , ("Meena", "https://arxiv.org/abs/2001.09977#google")
  , ("MegatronLM", "https://nv-adlr.github.io/MegatronLM")
  , ("MEMORIZE", "http://learning.mpi-sws.org/memorize/")
  , ("Merchant fees?", "https://en.wikipedia.org/wiki/Interchange_fee")
  , ("Meta[Mm]ath", "https://en.wikipedia.org/wiki/Metamath")
  , ("[Mm]eta [Pp]seudo [Ll]abels", "https://arxiv.org/abs/2003.10580#google")
  , ("Met HD", "https://en.wikipedia.org/wiki/Metropolitan_Opera_Live_in_HD")
  , ("Methods [Oo]f Rationality", "http://www.hpmor.com/")
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
  , ("MoR", "http://www.hpmor.com/")
  , ("HP:MoR", "http://www.hpmor.com/")
  , ("[Mm]ozjpeg", "https://github.com/mozilla/mozjpeg")
  , ("MS[- ]?COCO", "https://arxiv.org/abs/1405.0312#microsoft")
  , ("MSG-GAN", "https://arxiv.org/abs/1903.06048")
  , ("mT5", "https://arxiv.org/abs/2010.11934#google")
  , ("Mt\\. ?Gox", "https://en.wikipedia.org/wiki/Mt._Gox")
  , ("Muesli", "https://arxiv.org/abs/2104.06159")
  , ("[Mm]ultiple comparison", "https://en.wikipedia.org/wiki/Multiple_comparisons_problem")
  , ("[Mm]ultiple discovery", "https://en.wikipedia.org/wiki/Multiple_discovery")
  , ("Münster rebellion", "https://en.wikipedia.org/wiki/M%C3%BCnster_rebellion")
  , ("Murphy's law", "https://en.wikipedia.org/wiki/Murphy%27s_law")
  , ("Muse[Nn]et", "https://openai.com/blog/musenet/")
  , ("Music Transformer", "https://arxiv.org/abs/1809.04281#google")
  , ("Musth", "https://en.wikipedia.org/wiki/Musth")
  , ("Mu[Zz]ero", "https://arxiv.org/abs/1911.08265#deepmind")
  , ("Mu[Zz]ero Unplugged", "https://arxiv.org/abs/2104.06294#deepmind")
  , ("My Little Pony: Friendship is Magic", "https://en.wikipedia.org/wiki/My_Little_Pony:_Friendship_Is_Magic")
  , ("[Mm]yxoma virus", "https://en.wikipedia.org/wiki/Myxoma_virus")
  , ("[Nn][- ]?body problem", "https://en.wikipedia.org/wiki/N-body_problem")
  , ("Neo-Pop", "https://en.wikipedia.org/wiki/Neo-pop")
  , ("NeRF", "https://arxiv.org/abs/2003.08934")
  , ("Newcomb's Problem", "https://en.wikipedia.org/wiki/Newcomb%27s_paradox")
  , ("Newgrounds", "https://en.wikipedia.org/wiki/Newgrounds")
  , ("[Nn]ucleus [Ss]ampling", "https://arxiv.org/abs/1904.09751")
  , ("NVAE", "https://arxiv.org/abs/2007.03898#nvidia")
  , ("OA ?5", "https://openai.com/projects/five/")
  , ("ocrmypdf", "https://github.com/jbarlow83/OCRmyPDF")
  , ("[Oo]ld-style numerals?", "https://en.wikipedia.org/wiki/Text_figures")
  , ("Omni[Nn]et", "https://arxiv.org/abs/2103.01075")
  , ("OpenAI 5", "https://openai.com/projects/five/")
  , ("OpenAI API", "https://openai.com/blog/openai-api/")
  , ("OpenAI Gym", "https://github.com/openai/gym")
  , ("[Oo]perant conditioning", "https://en.wikipedia.org/wiki/Operant_conditioning")
  , ("[Oo]ptogenetics?", "https://en.wikipedia.org/wiki/Optogenetics")
  , ("Osaka Electro-Communication University", "https://en.wikipedia.org/wiki/Osaka%20Electro-Communication%20University")
  , ("Osaka University of Arts", "https://en.wikipedia.org/wiki/Osaka%20University%20of%20Arts")
  , ("[Oo]utside [Vv]iew", "http://wiki.lesswrong.com/wiki/Outside_view")
  , ("Overcoming Bias", "https://www.overcomingbias.com/")
  , ("Owarimonogatari", "https://en.wikipedia.org/wiki/List_of_Monogatari_episodes#Owarimonogatari")
  , ("PaintsTransfer/style2paints", "https://github.com/lllyasviel/style2paints")
  , ("Parker-Hulme murder case", "https://en.wikipedia.org/wiki/Parker%E2%80%93Hulme_murder_case")
  , ("[Pp]article filtering", "https://en.wikipedia.org/wiki/Particle_filter")
  , ("[Pp]asteurellosis", "https://en.wikipedia.org/wiki/Pasteurellosis")
  , ("Paul Celan", "https://en.wikipedia.org/wiki/Paul_Celan")
  , ("Paul Christiano", "https://paulfchristiano.com/")
  , ("Paul Ehrlich", "https://en.wikipedia.org/wiki/Paul_R._Ehrlich")
  , ("Paul F\\. Christiano", "https://paulfchristiano.com/")
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
  , ("President Obama", "https://en.wikipedia.org/wiki/Barack_Obama")
  , ("ProGAN", "https://arxiv.org/abs/1710.10196")
  , ("Project Malmo", "https://github.com/microsoft/malmo")
  , ("Puccini", "https://en.wikipedia.org/wiki/Giacomo_Puccini")
  , ("punctuated equilibrium", "https://en.wikipedia.org/wiki/Punctuated_equilibrium")
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
  , ("SimCLR", "https://arxiv.org/abs/2002.05709#google")
  , ("Simpson's [Pp]aradox", "https://en.wikipedia.org/wiki/Simpson%27s_paradox")
  , ("Single[Ff]ile", "https://github.com/gildas-lormeau/SingleFile/")
  , ("SMPY", "/SMPY")
  , ("SMYRF", "https://arxiv.org/abs/2010.05315")
  , ("social[- ]engineering", "https://en.wikipedia.org/wiki/Social_engineering_(security)")
  , ("Source Sans Pro", "https://en.wikipedia.org/wiki/Source_Sans_Pro")
  , ("Source Serif Pro", "https://en.wikipedia.org/wiki/Source_Serif_Pro")
  , ("[Ss]peedrunning", "https://en.wikipedia.org/wiki/Speedrun")
  , ("SR3", "https://arxiv.org/abs/2104.07636#google")
  , ("StackGAN", "https://arxiv.org/abs/1612.03242")
  , ("StackGAN\\+\\+", "https://arxiv.org/abs/1710.10916")
  , ("Stephen Schneider", "https://en.wikipedia.org/wiki/Stephen_Schneider")
  , ("Stroop (effect|task)", "https://en.wikipedia.org/wiki/Stroop_effect")
  , ("Style2Paints", "https://github.com/lllyasviel/style2paints")
  , ("StyleGAN2", "https://arxiv.org/abs/1912.04958#nvidia")
  , ("StyleGAN", "https://arxiv.org/abs/1812.04948")
  , ("style[- ]transfer", "https://arxiv.org/abs/1508.06576")
  , ("[Ss]upercut", "https://en.wikipedia.org/wiki/Supercut")
  , ("[Ss]uperflat", "https://en.wikipedia.org/wiki/Superflat")
  , ("Super(GLUE|Glue)", "https://arxiv.org/abs/1905.00537")
  , ("[Ss]uperpressure balloon", "https://en.wikipedia.org/wiki/Superpressure_balloon")
  , ("Suphx", "https://arxiv.org/abs/2003.13590#msr")
  , ("SWA", "https://arxiv.org/abs/1803.05407")
  , ("Swee[Tt]ango", "https://en.wikipedia.org/wiki/SweeTango")
  , ("Switch Transformers?", "https://arxiv.org/abs/2101.03961#google")
  , ("T5", "https://arxiv.org/abs/1910.10683#google")
  , ("TADNE", "https://thisanimedoesnotexist.ai/")
  , ("textgenrnn", "https://github.com/minimaxir/textgenrnn")
  , ("TFDNE", "https://www.thisfursonadoesnotexist.com")
  , ("TFRC", "https://www.tensorflow.org/tfrc")
  , ("The Browser", "https://thebrowser.com/")
  , ("The Pile", "https://arxiv.org/abs/2101.00027")
  , ("This Anime Does Not Exist\\.ai", "https://thisanimedoesnotexist.ai/")
  , ("This Fursona Does Not Exist", "https://www.thisfursonadoesnotexist.com")
  , ("This Pony Does Not Exist", "https://thisponydoesnotexist.net/")
  , ("This Waifu Does Not Exist", "https://www.thiswaifudoesnotexist.net/")
  , ("This ?Waifu ?Does ?Not ?Exist(\\.net)?", "https://www.thiswaifudoesnotexist.net/")
  , ("Thompson [Ss]ampling", "https://en.wikipedia.org/wiki/Thompson_sampling")
  , ("Timothy C\\. May", "https://en.wikipedia.org/wiki/Timothy_C._May")
  , ("Tim Powers", "https://en.wikipedia.org/wiki/Tim_Powers")
  , ("TinyBERT", "https://arxiv.org/abs/1909.10351")
  , ("[Tt]okusatsu", "https://en.wikipedia.org/wiki/Tokusatsu")
  , ("Tommy Wiseau", "https://en.wikipedia.org/wiki/Tommy_Wiseau")
  , ("Tom Wolfe", "https://en.wikipedia.org/wiki/Tom_Wolfe")
  , ("torch-rnn", "https://github.com/jcjohnson/torch-rnn")
  , ("[Tt]orsion balance", "https://en.wikipedia.org/wiki/Torsion_spring#Torsion_balance")
  , ("TPDNE", "https://thisponydoesnotexist.net/")
  , ("[Tt]ragedy of the anticommons", "https://en.wikipedia.org/wiki/Tragedy_of_the_anticommons")
  , ("[Tt]ransfer RNAs?", "https://en.wikipedia.org/wiki/Transfer_RNA")
  , ("Transformer", "https://arxiv.org/abs/1706.03762#google")
  , ("Transformer-XL", "https://arxiv.org/abs/1901.02860")
  , ("TransGAN", "https://arxiv.org/abs/2102.07074")
  , ("TRC", "https://www.tensorflow.org/tfrc")
  , ("[Tt]ree induction", "https://en.wikipedia.org/wiki/Decision_tree_learning")
  , ("TreeQN", "https://arxiv.org/abs/1710.11417")
  , ("Trithemius", "https://en.wikipedia.org/wiki/Johannes_Trithemius")
  , ("[Tt]rolley problem", "https://en.wikipedia.org/wiki/Trolley_problem")
  , ("[Tt]rophic level", "https://en.wikipedia.org/wiki/Trophic_level")
  , ("Tufte[- ]CSS", "https://edwardtufte.github.io/tufte-css/")
  , ("TWDNE", "https://www.thiswaifudoesnotexist.net/")
  , ("TWDNEv2", "https://www.thiswaifudoesnotexist.net/")
  , ("Uber", "https://en.wikipedia.org/wiki/Uber")
  , ("uBlock [Oo]rigin", "https://github.com/gorhill/uBlock")
  , ("UCI [Rr]epository", "https://en.wikipedia.org/wiki/University_of_California,_Irvine#Machine_Learning_Repository")
  , ("Universal Transformers?", "https://arxiv.org/abs/1807.03819#googledeepmind")
  , ("[Uu]nlikelihood training", "https://arxiv.org/abs/1908.04319")
  , ("Unsong", "https://unsongbook.com/")
  , ("V100", "https://en.wikipedia.org/wiki/Volta_(microarchitecture)#Products")
  , ("[Vv]alue [Ii]teration [Nn]etworks?", "https://arxiv.org/abs/1602.02867")
  , ("VideoGPT", "https://arxiv.org/abs/2104.10157")
  , ("Vision Transformer", "https://openreview.net/forum?id=YicbFdNTTy#google")
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
  , ("[Cc]ommoditize [Yy]our [Cc]omplement", "/Complement")
  , ("[Vv]alue-[Ee]quivalence", "https://arxiv.org/abs/2011.03506#deepmind")
  , ("DistilBERT", "https://arxiv.org/abs/1910.01108")
  , ("Internet Archive", "https://en.wikipedia.org/wiki/Internet_Archive")
  , ("[Oo]rphan [Ww]ork", "https://en.wikipedia.org/wiki/Orphan_work")
  , ("[Pp]ublic [Dd]omain", "https://en.wikipedia.org/wiki/Public_domain")
  , ("[Dd]emand [Cc]haracteristics", "https://en.wikipedia.org/wiki/Demand_characteristics")
  ] :: [(T.Text,T.Text)] )
