{-# LANGUAGE OverloadedStrings #-}

-- Module for typographic enhancements of text:
-- 1. adding smallcaps to capitalized phrases
-- 2. adding line-break tags (`<wbr>` as Unicode ZERO WIDTH SPACE) to slashes so web browsers break at slashes in text
-- 3. Adding classes to horizontal rulers (nth ruler modulo 3, allowing CSS to decorate it in a cycling pattern, like `class="ruler-1"`/`class="ruler-2"`/`class="ruler-3"`/`class="ruler-1"`..., like a repeating pattern of stars/moon/sun/stars/moon/sun... CSS can do this with :nth, but only for immediate sub-children, it can't count elements *globally*, and since Pandoc nests horizontal rulers and other block elements within each section, it is not possible to do the usual trick like with blockquotes/lists).
module Typography (invertImageInline, typographyTransform, imageMagickDimensions) where

import Control.Monad.State.Lazy (evalState, get, put, State)
import Control.Monad (void, when)
import Data.ByteString.Lazy.Char8 as B8 (unpack)
import Data.List (isPrefixOf)
import Data.List.Utils (replace)
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDay)
import System.Directory (getModificationTime, removeFile)
import System.Exit (ExitCode(ExitFailure))
import System.Posix.Temp (mkstemp)
import qualified Data.Text as T (any, isInfixOf, isSuffixOf, pack, unpack, replace, Text)
import qualified Text.Regex.Posix as R (makeRegex, match, Regex)
import Text.Regex (subRegex, mkRegex)
import System.IO (stderr, hPrint)
import Control.Concurrent (threadDelay)

import Data.FileStore.Utils (runShellCommand)

import Text.Pandoc (Inline(..), Block(..), Pandoc, topDown, nullAttr)
import Text.Pandoc.Walk (walk, walkM)

typographyTransform :: Pandoc -> Pandoc
typographyTransform = walk (breakSlashes . breakEquals) .
                      walk smallcapsfyInlineCleanup . walk smallcapsfy .
                      rulersCycle 3

-- Bringhurst & other typographers recommend using smallcaps for acronyms/initials of 3 or more capital letters because with full capitals, they look too big and dominate the page (eg. Bringhurst 2004, _Elements_ pg47; cf https://en.wikipedia.org/wiki/Small_caps#Uses http://theworldsgreatestbook.com/book-design-part-5/ http://webtypography.net/3.2.2 )
-- This can be done by hand in Pandoc by using the span syntax like `[ABC]{.smallcaps}`, but quickly grows tedious. It can also be done reasonably easily with a query-replace regexp eg. in Emacs `(query-replace-regexp "\\([[:upper:]][[:upper:]][[:upper:]]+\\)" "[\\1]{.smallcaps}\\2" nil begin end)`, but still must be done manually because while almost all uses in regular text can be smallcaps-ed, a blind regexp will wreck a ton of things like URLs & tooltips, code blocks, etc.
-- However, if we walk a Pandoc AST and check for only acronyms/initials inside a `Str`, where they *can't* be part of a Link or CodeBlock, then looking over Gwern.net ASTs, they seem to always be safe to substitute in SmallCaps elements. Unfortunately, we can't use the regular `Inline -> Inline` replacement pattern because SmallCaps takes a `[Inline]` argument, and so we are doing `Str String -> SmallCaps [Inline]` in theory and changing the size/type.
-- So we instead walk the Pandoc AST, use a regexp to split on 3 capital letters, and inline the HTML span, skipping `Span` and `SmallCaps` entirely as the former causes serious problems in infinite loops & duplication when tree-walking, and the latter works incorrectly for capitalized phrases.
--
-- Why? For HTML output, simply using the regular `smallcaps` HTML class (which is how the default Pandoc HTML output does it) is not enough, because using smallcaps on a capital letter is a null-op. We *could* just rewrite the capitals to lowercase with `map toLower` etc, but then that breaks copy-paste: the underlying text for a 'Big[GAN]{.smallcaps}' is now '[Biggan]{.smallcaps}' etc. So instead of using native SmallCaps AST elements, we create a new HTML span class for *just* all-caps separate from the pre-existing standard Pandoc 'smallcaps' CSS class, 'smallcaps-auto'; we annotate capitals with that new class in a Span rather than SmallCaps, and then in CSS, we do `span.smallcaps-auto { font-feature-settings: 'smcp'; text-transform: lowercase; }` - smallcaps is enabled for this class, but we also lowercase everything, thereby forcing the intended smallcaps appearance while ensuring that copy-paste produces 'BigGAN' (as written) instead of 'Biggan'. That will work for most fonts but may have a few bugs (does your font support italic smallcaps? if it doesn't, then automatically lowercasing & applying smallcaps in an italicized phrase will just produce a lowercase phrase). The SSfP font used on Gwern.net supports an additional font feature: 'c2sc', which is intended for exactly the purpose of converting an all-caps phrase to smallcaps, so for use with SSfP, it can be simplified to `font-feature-settings: 'smcp', 'c2sc'`.
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
-- walk smallcapsfyInline [Str "MS COCO"]
-- → [Span ("",[],[]) [Span ("",["smallcaps-auto"],[]) [Str "MS COCO"]]]
--
-- Whole-document examples:
--
--  walk smallcapsfyInline [Str "bigGAN means", Emph [Str "BIG"]]
-- → [RawInline (Format "html") "big<span class=\"smallcaps-auto\">GAN</span> means",Emph [RawInline (Format "html") "<span class=\"smallcaps-auto\">BIG</span>"]]
--
-- We exclude headers because on Gwern.net, headers are uppercase already, which makes auto-smallcaps look odd. So we skip header Block elements before doing the replacement on all other Block elements
smallcapsfy :: Block -> Block
smallcapsfy h@Header{} = h
smallcapsfy x          = walk smallcapsfyInline x
smallcapsfyInline, smallcapsfyInlineCleanup :: Inline -> Inline
smallcapsfyInline x@(Str s) = let rewrite = go s in if [Str s] == rewrite then x else Span nullAttr rewrite
  where
    go :: T.Text -> [Inline]
    go "" = []
    go a = let (before,matched,after) = R.match smallcapsfyRegex (T.unpack a) :: (String,String,String)
                                 in if matched==""
                                    then [Str a] -- no acronym anywhere in it
                                    else (if before==""then[] else [Str (T.pack before)]) ++
                                         [Span ("", ["smallcaps-auto"], []) [Str $ T.pack matched]] ++
                                         (if after==""then[] else go (T.pack after))
smallcapsfyInline x = x
-- Hack: collapse redundant span substitutions (this happens when we apply `typographyTransform` repeatedly eg. if we scrape a Gwern.net abstract (which will already be smallcaps) as an annotation, and then go to inline it elsewhere like a link to that page on a different page):
smallcapsfyInlineCleanup x@(Span (_,["smallcaps-auto"],_) [y@(RawInline _ t)]) = if "<span class=\"smallcaps-auto\">" `T.isInfixOf` t then y else x
smallcapsfyInlineCleanup (Span (_,["smallcaps-auto"],_) (y@(Span (_,["smallcaps-auto"],_) _):_)) = y
smallcapsfyInlineCleanup (Span (_,["smallcaps-auto"], _) [Span ("",[],[]) y]) = Span ("",["smallcaps-auto"], []) y
smallcapsfyInlineCleanup (Span ("",[], []) [Span (_,["smallcaps-auto"],_) y]) = Span ("",["smallcaps-auto"], []) y
smallcapsfyInlineCleanup x@(Span (_,["smallcaps-auto"],_) _) = x
smallcapsfyInlineCleanup x = x

-- define at top level to memoize / avoid recomputation for efficiency since it runs everywhere
smallcapsfyRegex :: R.Regex
smallcapsfyRegex = R.makeRegex
  -- match standard acronyms like ABC or ABCDEF:
  ("[A-Z&][A-Z&][A-Z&]+|" ++
    -- match hyphen-separated acronyms like 'GPT-2-117M' but not small mixed like '150MB'/'P100'/'FP16'
    -- or hyphenated expressions with lowercase letters like 'BigGAN-level':
   "[A-Z&][A-Z&][A-Z&]+(-[[:digit:]]+|[A-Z&]+)+|" ++
   -- smallcaps entirety of "TPUv3", oldstyle numbers look odd when juxtaposed against smallcaps+lowercase
   "[A-Z&][A-Z&][A-Z&]+([[:digit:]]+|[a-zA-Z&]+)+|" ++
   -- but we do want to continue across hyphens of all-uppercase strings like "YYYY-MM-DD" or "X-UNITER" or "DALL·E":
   "[A-Z&][A-Z&][A-Z&]+(-[A-Z&]+)+|" ++ "[A-Z&]+-[A-Z&][A-Z&][A-Z&]+|" ++ "[A-Z&]+\183[A-Z&]+|" ++
   -- or slashed acronyms like "TCP/IP": eg
   -- walk smallcapsfyInline [Str "Connecting using TCP/IP"]
   -- → [RawInline (Format "html") "Connecting using <span class=\"smallcaps-auto\">TCP/IP</span>"]
   -- walk smallcapsfyInline [Str "Connecting using TCP"]
   -- → [RawInline (Format "html") "Connecting using <span class=\"smallcaps-auto\">TCP</span>"]
   -- walk smallcapsfyInline [Str "Connecting using IP"]
   -- → [Str "Connecting using IP"]
   "[A-Z&][A-Z&][A-Z&]+(/[A-Z&]+)+|" ++ "[A-Z&]+/[A-Z&][A-Z&][A-Z&]+|" ++
   -- It looks odd when you have 'AB XYZ' or 'XYZ AB' with partial smallcaps, so we treat them as a single range (although in practice this may not work due to the Space splitting):
   -- walk smallcapsfyInline [Str "Connecting using TCP IP"]
   -- → [RawInline (Format "html") "Connecting using <span class=\"smallcaps-auto\">TCP IP</span>"]
   "[A-Z&][A-Z&][A-Z&] [A-Z&][A-Z&]|" ++
   -- walk smallcapsfyInline [Str "WP RSS bot"]
   -- → [RawInline (Format "html") "<span class=\"smallcaps-auto\">WP RSS</span> bot"]
   "[A-Z&][A-Z&] [A-Z&][A-Z&][A-Z&]+|" ++
   -- special-case AM/PM like "9:30AM" or "1PM" or "5:55 PM" (WARNING: Pandoc will typically parse spaces into 'Space' AST nodes, making it hard to match on things like "5 PM")
   "[[:digit:]]+ ?[AP]M|" ++
   "\\??[AP]M|" ++ -- special-case handling for all the "?AM--?PM" in /Morning-writing:
   -- according to https://en.wikipedia.org/wiki/Small_caps#Uses http://theworldsgreatestbook.com/book-design-part-5/ http://webtypography.net/3.2.2 , smallcaps is also often specially applied to a few two-letter initialisms/acronyms
   -- special-case AD/BC as well, "1AD", "10 BC", "1955 AD":
   "^AD.?$|"  ++ "^BC.?$|"  ++
   "[[:digit:]]+ ?ADE?|" ++ "[[:digit:]]+ ?BCE?"::String)

-------------------------------------------

-- add '<wbr>'/ZERO WIDTH SPACE (https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr) HTML element to inline uses of forward slashes, such as in lists, to tell Chrome to linebreak there (see https://www.gwern.net/Lorem#inline-formatting in Chrome for examples of how its linebreaking is incompetent, sadly).
-- WARNING: this will affect link texts like '[AC/DC](!W)', so make sure you do the rewrite after the interwiki and any passes which insert inline HTML - right now 'breakSlashes' tests for possible HTML and bails out to avoid damaging it.
breakSlashes :: Block -> Block
-- skip CodeBlock/RawBlock/Header/Table: enabling line-breaking on slashes there is a bad idea or not possible:
breakSlashes x@CodeBlock{} = x
breakSlashes x@RawBlock{}  = x
breakSlashes x@Header{}    = x
breakSlashes x@Table{}     = x
breakSlashes x = topDown breakSlashesInline x
breakSlashesInline, breakSlashesPlusHairSpaces :: Inline -> Inline
breakSlashesInline x@(SmallCaps _) = x
breakSlashesInline x@Code{}        = x
breakSlashesInline (Link a@(i,c,ks) [Str ss] (t,""))  = if ss == t then
                                                -- if an autolink like '<https://example.com>' which converts to 'Link () [Str "https://example.com"] ("https://example.com","")' or '[Para [Link ("",["uri"],[]) [Str "https://www.example.com"] ("https://www.example.com","")]]' (NOTE: we cannot rely on there being a "uri" class), then we mark it up as Code and skip it:
                                                (Link (i,["uri"]++c,ks) [Code nullAttr ss] (t,""))
                                                else
                                                Link a (walk breakSlashesPlusHairSpaces [Str ss]) (t,"")
breakSlashesInline (Link a ss ts) = Link a (walk breakSlashesPlusHairSpaces ss) ts
breakSlashesInline x@(Str s) = if T.any (\t -> t=='/' && not (t=='<' || t=='>' || t ==' ' || t == '\8203')) s then -- things get tricky if we mess around with raw HTML, so we bail out for anything that even *looks* like it might be HTML tags & has '<>' or a HAIR SPACE or ZERO WIDTH SPACE already
                                 Str (T.replace " /\8203 " " / " $ T.replace " /\8203" " /" $ T.replace "/\8203 " "/ " $ -- fix redundant \8203s to make HTML source nicer to read; 2 cleanup substitutions is easier than using a full regexp rewrite
                                                   T.replace "/" "/\8203" s) else x
breakSlashesInline x = x
-- the link-underlining hack, using drop-shadows, causes many problems with characters like slashes 'eating' nearby characters; a phrase like "A/B testing" is not usually a problem because the slash is properly kerned, but inside a link, the '/' will eat at 'B' and other characters where the top-left comes close to the top of the slash. (NOTE: We may be able to drop this someday if CSS support for underlining with skip-ink ever solidifies.)
-- The usual solution is to insert a HAIR SPACE or THIN SPACE. Here, we descend inside Link nodes to their Str to  add both <wbr> (line-breaking is still an issue AFAIK) and HAIR SPACE (THIN SPACE proved to be too much).
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
breakEqualsInline (Str s) = let s' = T.unpack s in Str $ T.pack $ subRegex equalsRegex s' " \\1 \\2"
breakEqualsInline x = x
equalsRegex :: R.Regex
equalsRegex = mkRegex "([=≠])([a-zA-Z0-9])"

-------------------------------------------

-- Look at mean color of image, 0-1: if it's close to 0, then it's a monochrome-ish white-heavy image. Such images look better in HTML/CSS dark mode when inverted, so we can use this to check every image for color, and set an 'invertible-auto' HTML class on the ones which are low. We can manually specify a 'invertible' class on images which don't pass the heuristic but should.
invertImageInline :: Inline -> IO Inline
invertImageInline x@(Image (htmlid, classes, kvs) xs (p,t)) =
  if notInvertibleP classes then
    return x else do
                   (color,_,_) <- invertFile p
                   if not color then return x else
                     return (Image (htmlid, "invertible-auto":classes, kvs++[("loading","lazy")]) xs (p,t))
invertImageInline x@(Link (htmlid, classes, kvs) xs (p, t)) =
  if notInvertibleP classes || not (".png" `T.isSuffixOf` p || ".jpg" `T.isSuffixOf` p)  then
                                                          return x else
                                                            do (color,_,_) <- invertFile p
                                                               if not color then return x else
                                                                 return (Link (htmlid, "invertible-auto":classes, kvs) xs (p,t))
invertImageInline x = return x

invertFile :: T.Text -> IO (Bool, String, String)
invertFile p = do let p' = T.unpack p
                  let p'' = if head p' == '/' then tail p' else p'
                  invertImage p''

notInvertibleP :: [T.Text] -> Bool
notInvertibleP classes = "invertible-not" `elem` classes

invertImage :: FilePath -> IO (Bool, String, String) -- invertible / height / width
invertImage f | "https://www.gwern.net/" `isPrefixOf` f = invertImageLocal $ Data.List.Utils.replace "https://www.gwern.net/" "" f
              | "http" `isPrefixOf` f = do (temp,_) <- mkstemp "/tmp/image-invertible"
                                           -- NOTE: while wget preserves it, curl erases the original modification time reported by server in favor of local file creation; this is useful for `invertImagePreview` --- we want to check downloaded images manually before their annotation gets stored permanently.
                                           (status,_,_) <- runShellCommand "./" Nothing "curl" ["--location", "--silent", "--user-agent", "gwern+wikipediascraping@gwern.net", f, "--output", temp]
                                           case status of
                                             ExitFailure _ -> do hPrint stderr ("Download failed (unable to check image invertibility): " ++ f)
                                                                 removeFile temp
                                                                 return (False, "320", "320") -- NOTE: most WP thumbnails are 320/320px squares, so to be safe we'll use that as a default value
                                             _ -> do c <- imageMagickColor f temp
                                                     (h,w) <- imageMagickDimensions temp
                                                     let invertp = c < invertThreshold
                                                     when invertp $ invertImagePreview temp
                                                     removeFile temp
                                                     return (invertp, h, w)
              | otherwise = invertImageLocal f

invertImageLocal :: FilePath -> IO (Bool, String, String)
invertImageLocal "" = return (False, "0", "0")
invertImageLocal f = do c <- imageMagickColor f f
                        (h,w) <- imageMagickDimensions f
                        let invertp = c < invertThreshold
                        return (invertp, h, w)
invertThreshold :: Float
invertThreshold = 0.09

-- Manually check the inverted version of new images which trigger the inversion heuristic. I don't want to store a database of image inversion status, so I'll use the cheaper heuristic of just opening up every image modified <1 day ago (which should catch all of the WP thumbnails + any images added).
invertImagePreview :: FilePath -> IO ()
invertImagePreview f = do utcFile <- getModificationTime f
                          utcNow  <- getCurrentTime
                          let age  = utcNow `diffUTCTime` utcFile
                          when (age < nominalDay) $ do
                            let f' = f++"-inverted.png"
                            void $ runShellCommand "./" Nothing "convert" ["-negate", f, f']
                            void $ runShellCommand "./" Nothing "firefox" [f']
                            threadDelay 5000000
                            removeFile f'
                          return ()

imageMagickColor :: FilePath -> FilePath -> IO Float
imageMagickColor f f' = do (status,_,bs) <- runShellCommand "./" Nothing "convert" [f', "-colorspace", "HSL", "-channel", "g", "-separate", "+channel", "-format", "%[fx:mean]", "info:"]
                           case status of
                             ExitFailure err ->  putStrLn (f ++ " : ImageMagick color read error: " ++ show err ++ " " ++ f') >> return 1.0
                             _ -> do let color = read (take 4 $ unpack bs) :: Float -- WARNING: for GIFs, ImageMagick returns the mean for each frame; 'take 4' should give us the first frame, more or less
                                     return color

-- | Use FileStore utility to run imageMagick's 'identify', & extract the height/width dimensions
-- Note that for animated GIFs, 'identify' returns width/height for each frame of the GIF, which in
-- most cases will all be the same, so we take the first line of whatever dimensions 'identify' returns.
imageMagickDimensions :: FilePath -> IO (String,String)
imageMagickDimensions f =
  let f'
        | "/" `isPrefixOf` f && not ("/tmp" `isPrefixOf` f) = tail f
        | "https://www.gwern.net/" `isPrefixOf` f = drop 22 f
        | otherwise = f
  in
    do (status,_,bs) <- runShellCommand "./" Nothing "identify" ["-format", "%h %w\n", f']
       case status of
         ExitFailure _ -> error f
         _             -> do let [height, width] = words $ head $ lines $ B8.unpack bs
                             return (height, width)

-------------------------------------------

-- Annotate body horizontal rulers with a class based on global count: '<div class="ruler-nth-0"> / <hr /> / </div>' / '<div class="ruler-nth-1"> / <hr /> / </div>' / '<div class="ruler-nth-2"> / <hr /> / </div>' etc (cycling). Allows CSS decoration of "every second ruler" or "every fourth ruler" etc. I use it for cycling rulers in 3 levels, similar to the rest of Gwern.net's visual design.
--
-- Generalized versions for arbitrary Inline/Block types using generic programming: https://groups.google.com/g/pandoc-discuss/c/x1IXyfC2tfU/m/sXnHU7DIAgAJ (not currently necessary, but worth noting should I need to number anything in the future).
---
-- (NOTE: As a rewrite pass, this does not affect the horizontal ruler in the endnotes section, nor any horizontal rulers in the outer HTML document.)
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
