{-# LANGUAGE OverloadedStrings #-}

-- Module for typographic enhancements of text:
-- 1. add link-live (cross-domain iframe popups) & link icon classes to links: split out to LinkIcon.hs
-- 2. adding line-break tags (`<wbr>` as Unicode ZERO WIDTH SPACE) to slashes so web browsers break at slashes in text
-- 3. Adding classes to horizontal rulers (nth ruler modulo 3, allowing CSS to decorate it in a
--    cycling pattern, like `<div class="horizontal-rule-nth-0"><hr></div>`/`class="horizontal-rule-nth-1"`/`class="horizontal-rule-nth-2"`/`class="horizontal-rule-nth-0"`..., (the div wrapper is necessary because Pandoc's 'HorizontalRule' Block element supports no attributes)
--    like a repeating pattern of stars/moon/sun/stars/moon/sun... CSS can do this with :nth, but only
--    for immediate sub-children, it can't count elements *globally*, and since Pandoc nests horizontal
--    rulers and other block elements within each section, it is not possible to do the usual trick
--    like with blockquotes/lists).
module Typography (linebreakingTransform, typographyTransform, titlecase', titlecaseInline, identUniquefy, mergeSpaces) where

import Control.Monad.State.Lazy (evalState, get, put, State)
import Data.Char (toUpper)
import qualified Data.Text as T (any, append, concat, pack, unpack, replace, splitOn, strip, Text)
import Data.Text.Read (decimal)
import Text.Regex.TDFA ((=~), Regex, makeRegex, match) -- WARNING: avoid the native Posix 'Text.Regex' due to bugs and segfaults/strange-closure GHC errors
import qualified Data.Map.Strict as M

import Data.Text.Titlecase (titlecase)

import Text.Pandoc (Inline(..), Block(..), Pandoc, topDown, nullAttr)
import Text.Pandoc.Walk (walk, walkM)

import LinkIcon (linkIcon)
import LinkLive (linkLive)

import Utils (addClass, sed, currentYear, replaceMany)

typographyTransform :: Pandoc -> Pandoc
typographyTransform = let year = currentYear in
                        walk (citefyInline year . linkLive . linkIcon) .
                        walk mergeSpaces .
                        linebreakingTransform .
                        rulersCycle 3

linebreakingTransform :: Pandoc -> Pandoc
linebreakingTransform = walk (breakSlashes . breakEquals)

-- Pandoc breaks up strings as much as possible, like [Str "ABC", Space, "notation"], which makes it impossible to match on them, so we remove Space
mergeSpaces :: [Inline] -> [Inline]
mergeSpaces []                         = []
mergeSpaces (Str x:Str y:xs)           = mergeSpaces (Str (x`T.append`y) : xs)
mergeSpaces (Space:Str x:Space:xs)     = mergeSpaces (Str (" "`T.append`x`T.append`" "):xs)
mergeSpaces (Space:Str x:xs)           = mergeSpaces (Str (" "`T.append`x):xs)
mergeSpaces (Str x:Space:xs)           = mergeSpaces (Str (x`T.append`" "):xs)
mergeSpaces (Str "":xs)                = mergeSpaces xs
mergeSpaces (Str x:SoftBreak:Str y:xs) = mergeSpaces (Str (x`T.append`" "`T.append`y):xs)
mergeSpaces (SoftBreak:Str x:xs)       = mergeSpaces (Str (" "`T.append`x):xs)
mergeSpaces (SoftBreak:xs)             = mergeSpaces (Space:xs)
mergeSpaces (x:xs)                     = x:mergeSpaces xs
citefyInline :: Int -> Inline -> Inline
citefyInline year x@(Str s) = let rewrite = go s in if [Str s] == rewrite then x else Span nullAttr rewrite
  where
    go :: T.Text -> [Inline]
    go "" = []
    go a = let matched'' = match citefyRegexMultiple a :: [[T.Text]]
               matched'  = match citefyRegexDouble a :: [[T.Text]]
               matched   = match citefyRegexSingle a :: [[T.Text]]
               matchAll  = matched'' ++ matched' ++ matched
           in if null matchAll then [Str a] -- no citation anywhere
              else
                let (fullMatch:first:second:third:_) = head matchAll
                    (before:after) = T.splitOn fullMatch a
                    citeYear = case decimal third :: Either String (Int, T.Text) of
                                  Left _ -> 0
                                  Right (y,_) -> y
                in
                  if citeYear > year+3 || -- sanity-check the cite year: generally, a citation can't be for more than 2 years ahead: ie on 31 December 2020, a paper may well have an official date anywhere in 2021, but it would be *highly* unusual for it to be pushed all the way out to 2022 (only the most sluggish of periodicals like annual reviews might do that), so ≥2023 *should* be right out. If we have a 'year' bigger than that, it is probably a false positive, eg. 'Atari 2600' is a video game console and not a paper published by Dr. Atari 6 centuries hence.
                     first `elem` ["Et", "et", "Al", "al", "Accurate", "Aesthetics", "Africa", "After", "Alert", "America", "An", "Apr", "April", "At", "Atari", "Atlas", "August", "Aug", "Autumn", "Before", "British", "Challenge", "Chat", "Codex", "Cohort", "Commodore", "Competition", "Considered", "Copyright", "Counterfactual", "Crypto", "Daily", "Dear", "Dec", "December", "Diaries", "Differences", "Early", "Enterprise", "Esthetics", "Evolution", "Expo", "Fair", "Fall", "Fanime", "Fanimecon", "Feb", "February", "First", "For", "Friday", "Impacts", "Jan", "January", "Jul", "July", "June", "Last", "Late", "Library", "Making", "Mar", "March", "May", "Memoirs", "Monday", "Monthly", "Ms", "Nov", "November", "Oct", "October", "Original", "Otakon", "Our", "Ours", "Over", "Predicting", "Reviews", "Sample", "Saturday", "Sci", "Security", "Sep", "September", "Since", "Since", "Spring", "Standard", "Statistics", "Suisse", "Summer", "Sunday", "Surface", "Survey", "Syntheses", "Than", "The", "Things", "Throughout", "Thursday", "Tuesday", "Until", "Wednesday", "Weekly", "Winter", "Writing", "Year", "Yearly", "Zilch", "In", "Judging", "From", "Study", "Experiment"] then -- dates like "January 2020" are false positives, although unfortunately there are real surnames like 'May', where 'May 2020' is just ambiguous and this will have a false negative.
                    [Str a]
                  else
                          [Str before] ++
                          [Span ("", ["cite"], []) ((if T.strip second == "" then
                                                       -- the easy single/double author case (we only mess with the date, nothing else)
                                                       [Span ("", ["cite-author"], []) [Str $ T.replace " " " " first]] -- condense with THIN SPACE
                                                       -- et-al case: different span class to select on, stash the et al joiner in a span to suppress:
                                                       else [Span ("", ["cite-author-plural"], [("title","et al")]) [Str first]] ++
                                                             [Span ("", ["cite-joiner"], []) [Str $ " " `T.append` (T.replace " " " " $ T.strip second) `T.append` " "]]) ++
                                                    [Span ("", ["cite-date"], []) [Str third]])
                          ] ++
                          go (T.concat after)
citefyInline _ x = x

citefyRegexSingle, citefyRegexDouble, citefyRegexMultiple :: Regex
citefyRegexSingle = makeRegex ("([A-Z][" `T.append`  lowercaseUnicode `T.append`  "-]?[A-Z]?[" `T.append`  lowercaseUnicode `T.append`  "-]+)([    \8203]+)([12][0-9][0-9][0-9][a-z]?)" :: T.Text) -- match one-author citations like "Foo 2020" or "Foo 2020a"; we avoid using [:punct:] to avoid matching on en-dashes in date ranges; need to also handle mixed-case like 'McDermot'
citefyRegexDouble = makeRegex ("([A-Z][" `T.append`  lowercaseUnicode `T.append`  "-]?[A-Z]?[" `T.append`  lowercaseUnicode `T.append`  "-]+[    \8203]+&[    \8203]+[A-Z][a-z-]+)([    \8203]+)([12][0-9][0-9][0-9][a-z]?)" :: T.Text) -- match two-author citations like "Foo & Bar 2020"
citefyRegexMultiple = makeRegex ("([A-Z][" `T.append`  lowercaseUnicode `T.append`  "-]?[A-Z]?[" `T.append`  lowercaseUnicode `T.append`  "-]+)([    \8203]+[Ee]t[    \8203][Aa]l[    \8203]+)([12][0-9][0-9][0-9][a-z]?)" :: T.Text)

-- sourced from /Lorem#unicode-characters - this *should* be pretty much all the lowercase Unicode characters which might turn up in a surname:
lowercaseUnicode :: T.Text
lowercaseUnicode = "a-zàáâãäåæçèéêëìíîïðñòóôõöøùúûüýāăąćčēęěğīİıłńņŋōŏőœřśŠšūŮůźžƆǎǐǔǿșɔəʒḥṇṣίαγδεηθλμνοπρστυφχψωϩавгдежзийклмнопрстухцщыьэюя"

-------------------------------------------

-- add '<wbr>'/ZERO WIDTH SPACE (https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr) HTML element to inline uses of forward slashes, such as in lists, to tell Chrome to linebreak there (see https://www.gwern.net/Lorem#inline-formatting in Chrome for examples of how its linebreaking is incompetent, sadly).
--
-- WARNING: this will affect link texts like '[AC/DC](!W)', so make sure you do the rewrite after
-- the interwiki and any passes which insert inline HTML - right now 'breakSlashes' tests for
-- possible HTML and bails out to avoid damaging it.
breakSlashes :: Block -> Block
-- skip CodeBlock/RawBlock/Header/Table: enabling line-breaking on slashes there is a bad idea or not possible:
breakSlashes x@CodeBlock{} = x
breakSlashes x@RawBlock{}  = x
breakSlashes x@Header{}    = x
breakSlashes x@Table{}     = x
breakSlashes x = topDown breakSlashesInline x
breakSlashesInline, breakSlashesPlusHairSpaces :: Inline -> Inline
breakSlashesInline x@Code{}        = x
breakSlashesInline (Link a [Str ss] (t,"")) = if ss == t then
                                                -- if an autolink like '<https://example.com>' which
                                                -- converts to 'Link () [Str "https://example.com"]
                                                -- ("https://example.com","")' or '[Para [Link
                                                -- ("",["uri"],[]) [Str "https://www.example.com"]
                                                -- ("https://www.example.com","")]]' (NOTE: we
                                                -- cannot rely on there being a "uri" class), then
                                                -- we mark it up as Code and skip it:
                                                 addClass "uri" $ Link a [Code nullAttr ss] (t,"")
                                                else
                                                 Link a (walk breakSlashesPlusHairSpaces [Str ss]) (t,"")
breakSlashesInline (Link a ss ts) = Link a (walk breakSlashesPlusHairSpaces ss) ts
breakSlashesInline x@(Str s) = if T.any (\t -> t=='/' && not (t=='<' || t=='>' || t ==' ' || t == '\8203')) s then -- things get tricky if we mess around with raw HTML, so we bail out for anything that even *looks* like it might be HTML tags & has '<>' or a HAIR SPACE or ZERO WIDTH SPACE already
                                 Str (T.replace " /\8203 " " / " $ T.replace " /\8203" " /" $ T.replace "/\8203 " "/ " $ -- fix redundant \8203s to make HTML source nicer to read; 2 cleanup substitutions is easier than using a full regexp rewrite
                                                   T.replace "/" "/\8203" s) else x
breakSlashesInline x = x
-- the link-underlining hack, using drop-shadows, causes many problems with characters like slashes
-- 'eating' nearby characters; a phrase like "A/B testing" is not usually a problem because the
-- slash is properly kerned, but inside a link, the '/' will eat at 'B' and other characters where
-- the top-left comes close to the top of the slash. (NOTE: We may be able to drop this someday if
-- CSS support for underlining with skip-ink ever solidifies.)
--
-- The usual solution is to insert a HAIR SPACE or THIN SPACE. Here, we descend inside Link nodes to
-- their Str to add both <wbr> (line-breaking is still an issue AFAIK) and HAIR SPACE (THIN SPACE
-- proved to be too much).
breakSlashesPlusHairSpaces x@(Str s) = if T.any (\t -> t=='/' && not (t=='<' || t=='>' || t ==' ')) s then
                                 Str (T.replace " /\8203 " " / " $ T.replace " /\8203" " /" $ T.replace "/\8203 " "/ " $
                                                   T.replace "/" " / \8203" s) else x
breakSlashesPlusHairSpaces x = x

breakEquals :: Block -> Block
breakEquals x@CodeBlock{} = x
breakEquals x@RawBlock{}  = x
breakEquals x@Header{}    = x
breakEquals x@Table{}     = x
breakEquals x = walk breakEqualsInline x
breakEqualsInline :: Inline -> Inline
breakEqualsInline (Str s) = Str $ T.pack $ sed "([=≠])([a-zA-Z0-9])" " \\1 \\2" $ T.unpack s
breakEqualsInline x = x

-------------------------------------------

-- Annotate body horizontal rulers with a class based on global count: '<div class="horizontal-rule-nth-0"> /
-- <hr /> / </div>' / '<div class="horizontal-rule-nth-1"> / <hr /> / </div>' / '<div class="horizontal-rule-nth-2"> /
-- <hr /> / </div>' etc (cycling). Allows CSS decoration of "every second ruler" or "every fourth
-- ruler" etc. I use it for cycling rulers in 3 levels, similar to the rest of Gwern.net's visual
-- design.
--
-- Generalized versions for arbitrary Inline/Block types using generic programming:
-- https://groups.google.com/g/pandoc-discuss/c/x1IXyfC2tfU/m/sXnHU7DIAgAJ (not currently necessary,
-- but worth noting should I need to number anything in the future).
---
-- (NOTE: As a rewrite pass, this does not affect the horizontal ruler in the endnotes section, nor
-- any horizontal rulers in the outer HTML document.)
rulersCycle :: Int -> Pandoc -> Pandoc
rulersCycle modulus doc = evalState (walkM addHrNth doc) 0
 where addHrNth :: Block -> State Int Block
       addHrNth HorizontalRule = do
         count <- get
         put (count + 1)
         let nth = count `mod` modulus
         let nthClass = T.pack $ "horizontal-rule" ++ "-nth-" ++ show nth
         return $ Div ("", [nthClass], []) [HorizontalRule]
       addHrNth x = return x

-- Walk a document, and de-duplicate overlapping IDs by appending "-n" for the nth use. This should not be used on regular content pages, where duplicate links should either be de-linked (replaced by a within-page anchor link, say) or given unique IDs by hand. This is useful for auto-generated pages like link-bibliographies or tags, where arbitrarily many different annotations will be inserted and it would be difficult or impossible to remove duplicates or override. So, somewhat analogous to `gensym`, we walk the doc and simply assign new IDs on demand.
identUniquefy :: Pandoc -> Pandoc
identUniquefy doc = evalState (walkM addIdentNth doc) M.empty
 where addIdentNth :: Inline -> State (M.Map T.Text Int) Inline
       addIdentNth x@(Link ("",_,_) _ _) = return x
       addIdentNth x@(Link (ident,b,c) d (e,f)) = do
         db <- get
         case M.lookup ident db of
           Nothing    -> do put (M.insert ident 1 db)
                            return x
           Just count -> do put (M.insert ident (count + 1) db)
                            return $ Link (ident `T.append` "-" `T.append` T.pack (show (count + 1)),
                                            b,c) d (e,f)
       addIdentNth x = return x

-- rewrite a string (presumably an annotation title) into a mixed-case 'title case'
-- https://en.wikipedia.org/wiki/Title_case as we expect from headlines/titles
--
-- uses <https://hackage.haskell.org/package/titlecase>
--
-- TODO: This wrapper function exists to temporarily work around `titlecase`'s lack of hyphen
-- handling: <https://github.com/peti/titlecase/issues/5>. We crudely just uppercase every lowercase
-- letter after a hyphen, not bothering with skipping prepositions/conjunctions/particles/etc.
-- Hopefully titlecase will do better and we can remove this.
titlecase' :: String -> String
titlecase' "" = ""
titlecase' t = let t' = titlecase $ titlecase'' t
  -- TODO: `titlecase` strips whitespace <https://github.com/peti/titlecase/issues/7> so have to restore it
                   t'' = if head t == ' ' then " " ++ t' else t'
                   t''' = if last t == ' ' then t'' ++ " " else t''
             in t'''
   where titlecase'' :: String -> String
         titlecase'' "" = ""
         titlecase'' t' = let (before,matched,after) =  t' =~ ("[ $^][A-za-z]\\-[a-z][ $^]"::String) :: (String,String,String)
                          in replaceMany [("Cite-author", "cite-author"), ("Cite-date", "cite-date"), ("Cite-joiner", "cite-joiner"), ("Class=","class=")] $ -- HACK
                             before ++ map toUpper matched ++ titlecase'' after

-- lift `titlecase'` to Inline so it can be walked, such as in Headers
titlecaseInline :: Inline -> Inline
titlecaseInline (Str s) = Str $ T.pack $ titlecase' $ T.unpack s
titlecaseInline       x = x
