{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Definition where --  (dictionary, defineAbbreviations) where

import qualified Data.Map as M (fromList, keys, lookup, toList, map, filterWithKey, Map)
import LinkMetadata (readLinkMetadata)
import Data.List (isInfixOf, intersperse, sortBy)
import Text.Pandoc -- (def, readHtml, runPure, writePlain, Inline(..), writerWrapText, WrapOption(WrapNone))
import Text.Pandoc.Walk
import qualified Data.Text as T (pack, unpack)
import qualified Text.Regex.Posix as R (makeRegex, match, Regex)
-- import HTMLEntities.Text as Esc
import Data.Char (isLower, toLower, toUpper)
import qualified Data.Set as S
import Control.Monad.State
import qualified Data.Text as T

-- List of terms to define in `<abbr>` HTML5 tags; `<abbr>` is rendered as a tooltip popup, and displays text (but not HTML, so the popup is not clickable).
dictionary :: IO (M.Map String String)
dictionary = do db <- readLinkMetadata
                let wp = M.filterWithKey (\url _ -> "en.wikipedia.org" `isInfixOf` url) db
                let wpDefinitions = filter (\(w,d) -> w/="" && d/="") $ map snd $ M.toList $ M.map (\(name,_,_,_,definition) -> (name, definition)) wp
                let combined = wpDefinitions ++ customDefinitions
                -- clean the definitions by stripping any HTML; run the blacklist
                let combinedClean = filter (\(w'', _) -> not (w'' `elem` blacklistDefinitions)) $  map (\(w',d') -> (w', clean d')) combined
                return $ M.fromList combinedClean -- or just, `customDefinitions`?

-- manual review: $ find . -type f -name "*.page" | sed -e 's/\.page//' | parallel cat | sed -e 's/\<span/\n<span/g' |fgrep definition-auto | sed -e 's/.* class="definition-auto" data-defined-term="\(.*\)" title=.*/\1/g' | sort | uniq -c | sort -g | tac | less
-- List of terms to *exclude* (eg WP links which are too dangerous to try to auto-define)
blacklistDefinitions :: [String]
blacklistDefinitions = ["Pi", "Ls", "Zen",  "Meh", "Eel", "Cel", "Intel", "Google", "Reddit", "Yama", "Anime", "Tokyo", "Sleep", "Moon", "Machine", "Apple", "Denmark", "Terrorism", "Star Wars", "Prison", "Honey", "Exercise", "Chocolate", "Doug", "Initial", "Lead", "Habit", "Byte", "Amber", "Blog", "Accounting", "Leading", "Obesity", "Almond", "Beaver", "Baseball", "Rewriting", "Charly", "World Wide Web", "Sparring", "Memento", "Kettle", "Bullshit", "Wi-Fi", "Diff", "3G" ]

-- convert any HTML (often included in Wikipedia API summaries) to plain text, since `<span>`/`<abbr>` can't display HTML; TODO: would it be better to reuse popups.js instead of the limited capabilities of tooltips? Do we really need full HTML formatting inside definitions? Maybe!
clean :: String -> String
clean input = let cleaned = runPure $ do
                                    html <- readHtml def{ readerExtensions = pandocExtensions } (T.pack input)
                                    txt <- writePlain def{writerWrapText=WrapNone} html
                                    return $ T.unpack txt
              in case cleaned of
                 Left _ -> ""
                 Right output -> output

customDefinitions :: [(String, String)]
customDefinitions = [
  ("GAN", "Generative Adversarial Network (GAN): a neural net architecture used to learn to generate data samples by training a Discriminator (D) to detect fake samples from real samples, and then a Generator (G) which adjusts its fake samples towards realness based on the D feedback, and vice-versa, G/D bootstrapping each other.")
  , ("P100", "NVIDIA P100: high-end GPU released mid-2016, often used in datacenters for AI training (16nm, 12–16GB VRAM, Tesla family, >$6000)")
  , ("BigGAN", "A type of neural network GAN published in 2018 for image generation; BigGAN's models are unusually large and employ special regularization to stabilize the neural networks, and self-attention blocks to increase its ability to understand & model fine details of images (like dogs having only 2 eyes). It can achieve near-photorealistic image generation on ImageNet.")
  ]

-- topDown (defineAbbreviations dictionary) [Str "bigGAN means", Emph [Str "BIG"], Str "GAN; you will have an easier time training it on a good GPU like a P100 or a TPUv3."]
-- → [Str "bigGAN",Str " means",Emph [Str "BIG"],Str "",RawInline (Format "html") "<abbr title=\"Generative Adversarial Network (GAN): a neural net architecture used to learn to generate data samples by training a Discriminator (D) to detect fake samples from real samples, and then a Generator (G) which adjusts its fake samples towards realness based on the D feedback, and vice-versa, G/D bootstrapping each other.\">",Str "GAN",RawInline (Format "html") "</abbr>",Str "; you",Str " will",Str " have",Str " an",Str " easier",Str " time",Str " training",Str " it",Str " on",Str " a",Str " good",Str " GPU",Str " like",Str " a",Str " ",RawInline (Format "html") "<abbr title=\"NVIDIA P100: high-end GPU released mid-2016, often used in datacenters for AI training (16nm, 12\8211\&16GB VRAM, Tesla family, >$6000)\">",Str "P100",RawInline (Format "html") "</abbr>",Str " or",Str " a",Str " TPUv3",Str "."]
--
-- $ xclip -o | pandoc -f native -w html
-- bigGAN means<em>BIG</em><abbr title="Generative Adversarial Network (GAN): a neural net architecture used to learn to generate data samples by training a Discriminator (D) to detect fake samples from real samples, and then a Generator (G) which adjusts its fake samples towards realness based on the D feedback, and vice-versa, G/D bootstrapping each other.">GAN</abbr>; you will have an easier time training it on a good GPU like a <abbr title="NVIDIA P100: high-end GPU released mid-2016, often used in datacenters for AI training (16nm, 12–16GB VRAM, Tesla family, >$6000)">P100</abbr> or a TPUv3.
defineAbbreviations :: (M.Map T.Text T.Text) -> [Inline] -> [Inline]
defineAbbreviations dict = concatMap go
  where
    definitionsRegexp = definitionsToRegexp dict :: R.Regex -- undefined :: R.Regex --
    go :: Inline -> [Inline]
    -- go (Str []) = []
    -- TODO: all these guards don't work; we want to skip recursion into some Inline types to avoid useless markup, but both `bottomUp`/`walk` create abbreviations anyway, and `topDown` seems to infinitely loop?
    go x@(Link _ _ _) = [x] -- skip links because links will have their own titles/annotations and override definitions TODO: all the
    go x@(Image _ _ _) = [x] -- likewise
    -- go   (Span at il) = [Span at (concatMap go il)]
    go x@(Code _ _) = [x] -- <abbr> in code blocks doesn't make sense... does it?
    go x@(Str a) = let (before,matched,after) = R.match definitionsRegexp a :: (T.Text,T.Text,T.Text)
                                 in if matched==""
                                    then [x] -- no acronym anywhere in x
                                    else let definition = M.lookup (trim matched) dict in
                                           case definition of
                                             Nothing   -> Str (before++matched) : go (Str after)
                                             -- no Inline in the Pandoc AST for `<abbr>` (aside from overloading Span), so insert it manually:
                                             Just defn -> [Str (before), Span ("",["definition-auto"],[("defined-term",trim matched),("title",defn)]) [Str matched]] ++ go (Str after)
    go x = [x]
    -- capture groups are a pain to use; the regexp requires a space before & after, so when doing the lookup, we just drop the first/last characters:
    trim x = if head x == ' ' then tail x else x -- tail -- reverse . tail . reverse . tail

-- Pandoc breaks up strings as much as possible, like [Str "ABC", Space, "notation"], which makes it impossible to match on them, so we remove Space
mergeSpaces :: [Inline] -> [Inline]
mergeSpaces [] = []
mergeSpaces ((Str x):(Str y):xs) = (Str (x++y)):mergeSpaces xs
mergeSpaces (Space:(Str x):Space:xs) = mergeSpaces ((Str (" "++x++" ")):xs)
mergeSpaces (Space:(Str x):xs) = mergeSpaces ((Str (" "++x)):xs)
mergeSpaces ((Str x):Space:xs) = mergeSpaces ((Str (x++" ")):xs)
mergeSpaces (x:xs) = x:(mergeSpaces xs)

-- R.makeRegex ("[[:alnum:]]+"::String)
-- create a single master regexp which matches all possible definition keys, whether 'GAN' or 'reinforcement learning'
definitionsToRegexp :: M.Map T.Text T.Text -> R.Regex
definitionsToRegexp d =  R.makeRegex $ concat $ intersperse "|" definitions'''
  where -- extract all keys:
        definitions = M.keys d :: [T.Text]
        -- sort by length, longest definitions first (I'm not sure if the regexp library is eager or matches entries in order, but if it does, longer should override shorter):
        definitions' = sortBy (\a b -> compare (length b) (length a)) definitions :: [T.Text]
        -- escape every character by backslashing (easier than trying to figure out how to escape just special characters): TODO: unnecessary?
        -- definitions'' = definitions' -- map (intersperse '\\') definitions' :: [String]
        -- make each definition match lowercase first letter, because they will typically be defined with capitals:
        -- ie the definition 'Reinforcement learning' should match 'reinforcement learning' in the text:
        definitions''' = map (\dfn -> let h = head dfn in
                                       "([ \n[:punct:]]|^)[" ++ [swapCase h, h] ++ "]"  ++ tail dfn) -- why do [:blank:]/[:space:] etc fail?
                          definitions'
        swapCase :: Char -> Char
        swapCase c = if isLower c then toUpper c else toLower c

-- Walk a Pandoc document; find the first instance of every auto-definition and mark it with the HTML/CSS class `definition-auto-first`; skip any further examples of that particular defined word.
-- This lets one add CSS to highlight *just* the first definition and skip the rest; this is difficult/impossible to do in CSS alone, so requires either preprocessing or runti
annotateFirstDefinitions :: Pandoc -> Pandoc
annotateFirstDefinitions doc = evalState (walkM addFirstDefn doc) S.empty
  where addFirstDefn :: Inline -> State (S.Set T.Text) Inline
        addFirstDefn x@(Span (ident,classes,values) il) = if ("definition-auto" `elem` classes) then
            -- see `defineAbbreviations`; 'defined-term' is always the first key/value pair in a `definition-auto`
            do let definedTerm = snd $ head values :: T.Text
               st <- get
               if S.member definedTerm st then return x
                 else do let st' = S.insert definedTerm st
                         put st'
                         return $ Span (ident,classes++["definition-auto-first"],values) il
            else return x
        addFirstDefn x = return x
