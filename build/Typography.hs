{-# LANGUAGE OverloadedStrings #-}

-- Module for typographic enhancements of text:
-- 1. adding smallcaps to capitalized phrases
-- 2. adding line-break tags (`<wbr>`) to slashes so web browsers break at slashes in text
-- 3. adding soft hyphens to enable hyphenation on broken web browsers like Google Chrome
-- 4. Adding classes to horizontal rulers (nth ruler modulo 3, allowing CSS to decorate it in a cycling pattern, like `class="ruler-1"`/`class="ruler-2"`/`class="ruler-3"`/`class="ruler-1"`..., like a repeating pattern of stars/moon/sun/stars/moon/sun... CSS can do this with :nth, but only for immediate sub-children, it can't count elements *globally*, and since Pandoc nests horizontal rulers and other block elements within each section, it is not possible to do the usual trick like with blockquotes/lists).
module Typography where

import Control.Monad.State.Lazy (evalState, get, put, State)
import Data.ByteString.Lazy.Char8 as B8 (unpack)
import Data.List (intercalate, isPrefixOf)
import System.Directory(removeFile)
import System.Exit (ExitCode(ExitFailure))
import System.Posix.Temp (mkstemp)
import qualified Data.Text as T (any, append, pack, unpack, replace, Text)
import qualified Text.Regex.Posix as R (makeRegex, match, Regex)

import Data.FileStore.Utils (runShellCommand)
import qualified Text.Hyphenation as H (hyphenate, hyphenatorLeftMin, english_US)

import Text.Pandoc (Inline(..), Block(..), Pandoc)
import Text.Pandoc.Walk (walk, walkM)

typographyTransform :: Pandoc -> Pandoc
typographyTransform = walk hyphenate . walk smallcapsfy . walk breakSlashes . rulersCycle 3

-- Bringhurst & other typographers recommend using smallcaps for acronyms/initials of 3 or more capital letters because with full capitals, they look too big and dominate the page (eg Bringhurst 2004, _Elements_ pg47; cf https://en.wikipedia.org/wiki/Small_caps#Uses http://theworldsgreatestbook.com/book-design-part-5/ http://webtypography.net/3.2.2 )
-- This can be done by hand in Pandoc by using the span syntax like `[ABC]{.smallcaps}`, but quickly grows tedious. It can also be done reasonably easily with a query-replace regexp eg in Emacs `(query-replace-regexp "\\([[:upper:]][[:upper:]][[:upper:]]+\\)" "[\\1]{.smallcaps}\\2" nil begin end)`, but still must be done manually because while almost all uses in regular text can be smallcaps-ed, a blind regexp will wreck a ton of things like URLs & tooltips, code blocks, etc.
-- However, if we walk a Pandoc AST and check for only acronyms/initials inside a `Str`, where they *can't* be part of a Link or CodeBlock, then looking over gwern.net ASTs, they seem to always be safe to substitute in SmallCaps elements. Unfortunately, we can't use the regular `Inline -> Inline` replacement pattern because SmallCaps takes a `[Inline]` argument, and so we are doing `Str String -> SmallCaps [Inline]` in theory and changing the size/type.
-- So we instead walk the Pandoc AST, use a regexp to split on 3 capital letters, and inline the HTML span, skipping `Span` and `SmallCaps` entirely as the former causes serious problems in infinite loops & duplication when tree-walking, and the latter works incorrectly for capitalized phrases.
--
-- Why? For HTML output, simply using the regular `smallcaps` HTML class (which is how the default Pandoc HTML output does it) is not enough, because using smallcaps on a capital letter is a null-op. We *could* just rewrite the capitals to lowercase with `map toLower` etc, but then that breaks copy-paste: the underlying text for a 'Big[GAN]{.smallcaps}' is now '[Biggan]{.smallcaps}' etc. So instead of using native SmallCaps AST elements, we create a new HTML span class for *just* all-caps separate from the pre-existing standard Pandoc 'smallcaps' CSS class, 'smallcaps-auto'; we annotate capitals with that new class in a Span rather than SmallCaps, and then in CSS, we do `span.smallcaps-auto { font-feature-settings: 'smcp'; text-transform: lowercase; }` - smallcaps is enabled for this class, but we also lowercase everything, thereby forcing the intended smallcaps appearance while ensuring that copy-paste produces 'BigGAN' (as written) instead of 'Biggan'. That will work for most fonts but may have a few bugs (does your font support italic smallcaps? if it doesn't, then automatically lowercasing & applying smallcaps in an italicized phrase will just produce a lowercase phrase). The SSfP font used on gwern.net supports an additional font feature: 'c2sc', which is intended for exactly the purpose of converting an all-caps phrase to smallcaps, so for use with SSfP, it can be simplified to `font-feature-settings: 'smcp', 'c2sc'`.
--
-- Regexp examples:
--
-- "BigGAN" =~ "[A-Z][A-Z][A-Z]+" :: (String,String,String)
-- → ("Big","GAN","")
--  "BigGANNN BigGAN" =~ "[A-Z][A-Z][A-Z]+" :: (String,String,String)
-- → ("Big","GANNN"," BigGAN")
--  "NSFW BigGAN" =~ "[A-Z][A-Z][A-Z]+" :: (String,String,String)
-- → ("","NSFW"," BigGAN")
--  "BigGANNN BigGAN" =~ "[A-Z][A-Z][A-Z]" :: (String,String,String)
-- → ("Big","GAN","NN BigGAN")
--
-- This regexp can be extended to handle mixed alphanumeric examples like "GPT-2-117M" where not smallcapsing the 'M' would make it larger than 'GPT' and look odd, by alternation and handling the 3 possible cases of a number at the beginning/middle/end.
--
-- Function examples:
--
-- walk smallcapsfyInline [Str "BigGAN"]
-- → [RawInline (Format "html") "Big<span class=\"smallcaps-auto\">GAN</span>"]
-- walk smallcapsfyInline [Str "BigGANNN means big"]
-- → [RawInline (Format "html") "Big<span class=\"smallcaps-auto\">GANNN</span> means big"]
-- walk smallcapsfyInline [Str "biggan means big"]
-- → [Str "biggan means big"]
-- walk smallcapsfyInline [Str "GPT-2-117M is a neural language model with ~117,000,000 parameters (fitting in 150MB) but smaller than GPT-2-1.5b and easily trained on a P100 using FP16; it is difficult to reach GPT-like levels"]
-- → RawInline (Format "html") "<span class=\"smallcaps-auto\">GPT-2-117M</span> is a neural language model with ~117,000,000 parameters (fitting in 150MB) but smaller than <span class=\"smallcaps-auto\">GPT-2-1</span>.5b and easily trained on a P100 using FP16; it is difficult to reach <span class=\"smallcaps-auto\">GPT</span>-like levels"
--
-- Whole-document examples:
--
--  walk smallcapsfyInline [Str "bigGAN means", Emph [Str "BIG"]]
-- → [RawInline (Format "html") "big<span class=\"smallcaps-auto\">GAN</span> means",Emph [RawInline (Format "html") "<span class=\"smallcaps-auto\">BIG</span>"]]
--
-- We exclude headers because on gwern.net, headers are uppercased, which makes auto-smallcaps look odd. So we skip header Block elements before doing the replacement on all other Block elements
smallcapsfy :: Block -> Block
smallcapsfy h@Header{} = h
smallcapsfy x                = walk smallcapsfyInline x
smallcapsfyInline :: Inline -> Inline
smallcapsfyInline x@(Str s) = let rewrite = go s in if s /= rewrite then RawInline "html" rewrite else x
  where
    go :: T.Text -> T.Text
    go "" = ""
    go a = let (before,matched,after) = R.match smallcapsfyRegex (T.unpack a) :: (String,String,String)
                                 in if matched==""
                                    then a -- no acronym anywhere in it
                                    else (T.pack before)
                                         `T.append` "<span class=\"smallcaps-auto\">"`T.append` (T.pack matched) `T.append` "</span>"
                                         `T.append` go (T.pack after)
smallcapsfyInline x = x

-- define at top level to memoize / avoid recomputation for efficiency since it runs everywhere
smallcapsfyRegex :: R.Regex
smallcapsfyRegex = R.makeRegex
  -- match standard acronyms like ABC or ABCDEF:
  ("[A-Z][A-Z][A-Z]+|" ++
    -- match hyphen-separated acronyms like 'GPT-2-117M' but not small mixed like '150MB'/'P100'/'FP16'
    -- or hyphenated expressions with lowercase letters like 'BigGAN-level':
   "[A-Z][A-Z][A-Z]+(-[[:digit:]]+|[A-Z]+)+|" ++
   -- but we do want to continue across hyphens of all-uppercase strings like "YYYY-MM-DD" or "X-UNITER":
   "[A-Z][A-Z][A-Z]+(-[A-Z]+)+|" ++ "[A-Z]+-[A-Z][A-Z][A-Z]+|" ++
   -- special-case AM/PM like "9:30AM" or "1PM" or "5:55 PM" (WARNING: Pandoc will typically parse spaces into 'Space' AST nodes, making it hard to match on things like "5 PM")
   "[[:digit:]]+ ?[AP]M|" ++
   "\\??[AP]M|" ++ -- special-case handling for all the "?AM--?PM" in /Morning-writing:
   -- according to https://en.wikipedia.org/wiki/Small_caps#Uses http://theworldsgreatestbook.com/book-design-part-5/ http://webtypography.net/3.2.2 , smallcaps is also often specially applied to a few two-letter initialisms/acronyms
   -- special-case AD/BC as well, "1AD", "10 BC", "1955 AD":
   "^AD.?$|"  ++ "^BC.?$|"  ++
   "[[:digit:]]+ ?ADE?|" ++ "[[:digit:]]+ ?BCE?"::String)

-- add '<wbr>' (https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr) HTML element to inline uses of forward slashes, such as in lists, to tell Chrome to linebreak there (see https://www.gwern.net/Lorem#inline-formatting in Chrome for examples of how its linebreaking & hyphenation is incompetent, sadly).
-- NOTE: this will affect link texts like '[AC/DC](!Wikipedia)', so do the rewrite after the interwiki and any passes which insert inline HTML - right now 'breakSlashes' tests for possible HTML and bails out to avoid damaging it
breakSlashes :: Block -> Block
-- skip CodeBlock/RawBlock/Header/Table: enabling line-breaking on slashes there is a bad idea or not possible:
breakSlashes x@CodeBlock{} = x
breakSlashes x@RawBlock{}  = x
breakSlashes x@Header{}    = x
breakSlashes x@Table{}     = x
breakSlashes x = walk breakSlashesInline x
breakSlashesInline :: Inline -> Inline
breakSlashesInline x@(SmallCaps _) = x
breakSlashesInline x@(Str s) = if T.any (\t -> t=='/' && not (t=='<' || t=='>')) s then -- things get tricky if we mess around with raw HTML
                                 RawInline "html" (T.replace "/" "/<wbr>" s) else x
breakSlashesInline x = x

-- Why try to support fully-justified text when desktop Chrome makes it so hard, the soft hyphen hack does come with costs (bloated HTML source, copy-paste issues, bots misbehaving, unfixable edge-cases like X.org middle-click-to-copy), and few will notice? Isn't fully-justified (rather than the usual left-justified ragged-right) rather fussy and excessive? Sure, designers and the like *claim* it looks better, but why believe them? Do we really like it for any reason other than typography tradition and it being associated with professionally-typeset books? Perhaps we'd find the hyphens and split-up words to be confusing clutter if not for inertia and historical reasons. Should we do it because it's hard and to show off?
--
-- After thinking about it, I think there is a principled justification for full-justification. (It is not a large or impressive reason, but we shouldn't expect it to be: there's only so much that different ways of fiddling with line length can do, after all. But it is a reason.) Ragged margins create lines of varying lengths, and can lead to visual artifacts like zig-zag snakes down the side, a staccato sense from the alternating long/short lines, or just blobby chunks of whitespace ("...a grey and muddled pattern of isolated spats..."); none of these apparent structures, however, *means* anything. They are brute facts, chance accident of whatever words and punctuation happened to be used and their interaction. Fully-justified text has no misleading ebb and flow on the ragged edge, nor any noticeable differences in densities, and fewer hacks like s t r e t c h e d out text; someone reading fully-justified text would have a much harder time guessing where they are reading solely from a blurry outline of the text.
--
-- In design, we want uniformity, and to show only "differences that make a difference"; "form follows function" implies that lack of form should follow lack of function (or to put it another way, if 'differences in form follow differences in function', then 'lack of difference in form should follow from lack of difference in function'). In Tufte's terms, a good data visualization or chart avoids 'chart junk' and maximizes 'data-ink ratio' by making all variations map onto variation in the underlying data; if two pixels are drawn at different X/Y locations, that's because they differ on two variables; if they differ in color, then they differ on a third variable as well; and so on. But they do not change positions, colors, icon shapes or whatever at random, arbitrarily. A motto for the designer: "show differences that make a difference, and hide differences that don't."
--
-- Usually, we are concerned with *adding* variation which reflects the structure & semantics of the document: splitting text into logical paragraphs, lists, footnotes, sections, subtitles, and so on. Full-justification happens to be a case where we are *removing* pseudo-structure. (This is somewhat unusual because our tools, like our fonts, generally have done such a good job of hiding such low-level irregularities - we don't notice how they do things like size or kerning right, avoiding similar problems at other scales.) Full-justification doesn't necessarily do a perfect job of removing bad variation (it can worsen 'rivers'), but few things are perfect, and full-justification does greatly even out text density and make a more uniform 'color'.
hyphenate :: Block -> Block
hyphenate x@CodeBlock{} = x
hyphenate x@RawBlock{}  = x
hyphenate x@Header{}    = x
hyphenate x@Table{}     = x
hyphenate x = walk hyphenateInline x
hyphenateInline :: Inline -> Inline
hyphenateInline x@(Str s) = if T.any (=='\173') s then x else -- U+00AD SOFT HYPHEN, HTML: &#173; &shy;
                              Str $ T.replace "-\173" "-" $ -- odd edge-case exposed on Safari: because hyphenator breaks on hyphens (why?), interspersing the soft hyphen results in *two* hyphens being displayed should the line break there! since the regular hyphen already ensures a linebreak opportunity
                                                            -- the soft hyphen is unnecessary, so just delete it.
                                                            -- https://github.com/ekmett/hyphenation/issues/16
                              -- preserve font ligatures at vshabanov's suggestion: "f"++"filjt" (SSfP supports these)
                              T.replace "f\173f" "ff" $
                              T.replace "f\173i" "fi" $
                              T.replace "f\173l" "fl" $
                              T.replace "f\173j" "fj" $
                              T.replace "f\173t" "ft" $
                              T.pack $ unwords $ map (intercalate "\173" . H.hyphenate H.english_US{H.hyphenatorLeftMin=3}) $ words $ T.unpack s
hyphenateInline x = x


-- Look at mean color of image, 0-1: if it's close to 0, then it's a monochrome-ish white-heavy image. Such images look better in HTML/CSS dark mode when inverted, so we can use this to check every image for color, and set an 'invertible-auto' HTML class on the ones which are low. We can manually specify a 'invertible' class on images which don't pass the heuristic but should.
invertImageInline :: Inline -> IO Inline
invertImageInline x@(Image (htmlid, classes, kvs) xs (p,t)) = do
                                       let p' = T.unpack p
                                       let p'' = if head p' == '/' then tail p' else p'
                                       color <- invertImage p''
                                       if not color then return x else
                                         return (Image (htmlid, "invertible-auto":classes, kvs) xs (p,t))
invertImageInline x = return x
invertImage :: FilePath -> IO Bool
invertImage f | "http" `isPrefixOf` f = do (temp,_) <- mkstemp "/tmp/image-invertible"
                                           (status,_,_) <- runShellCommand "./" Nothing "curl" ["--location", "--silent", "--user-agent", "gwern+wikipediascraping@gwern.net", f, "--output", temp]
                                           case status of
                                             ExitFailure _ -> do print ("Download failed (unable to check image invertibility): " ++ f)
                                                                 removeFile temp
                                                                 return False
                                             _ -> do c <- imageMagickColor temp
                                                     removeFile temp
                                                     return $ c < threshold
              | otherwise = do c <- imageMagickColor f
                               return $ c < threshold
              where threshold = 0.09

imageMagickColor :: FilePath -> IO Float
imageMagickColor f = do (status,_,bs) <- runShellCommand "./" Nothing "convert" [f, "-colorspace", "HSL", "-channel", "g", "-separate", "+channel", "-format", "%[fx:mean]", "info:"]
                        case status of
                          ExitFailure err -> error $ f ++ ": ImageMagick color read error: " ++ show err
                          _ -> do let color = read (take 4 $ unpack bs) :: Float -- WARNING: for GIFs, ImageMagick returns the mean for each frame; 'take 4' should give us the first frame, more or less
                                  return color

-- Annotate body horizontal rulers with a class based on global count: '<div class="ruler-nth-0"> / <hr /> / </div>' / '<div class="ruler-nth-1"> / <hr /> / </div>' / '<div class="ruler-nth-2"> / <hr /> / </div>' etc (cycling). Allows CSS decoration of "every second ruler" or "every fourth ruler" etc. I use it for cycling rulers in 3 levels, similar to the rest of gwern.net's visual design.
--
-- (NOTE: As a rewrite pass, this does not affect the horizontal ruler in the endnotes section, nor any horizontal rulers in the outer HTML document.)
--
-- No reason this couldn't be generalized to arbitrary Block/Inline elements (well, maybe—might require complex typing, and so be easier to just case-match on the existing Block/Inline types which shouldn't change anytime soon). But do I need it for anything else?
rulersCycle :: Int -> Pandoc -> Pandoc
rulersCycle modulus doc = evalState (walkM addHrNth doc) 0
 where addHrNth :: Block -> State Int Block
       addHrNth HorizontalRule = do
         count <- get
         put (count + 1)
         let nth = count `mod` modulus
         let nthClass = T.pack $ "horizontalRule" ++ "-nth-" ++ show nth
         return $ Div ("", [nthClass], []) [HorizontalRule]
       addHrNth x = return x
