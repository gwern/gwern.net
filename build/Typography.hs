{-# LANGUAGE OverloadedStrings #-}

-- Module for typographic enhancements of text:
-- 1. add link-live (cross-domain iframe popups) & link icon classes to links
-- 2. adding line-break tags (`<wbr>` as Unicode ZERO WIDTH SPACE) to slashes so web browsers break at slashes in text
-- 3. Adding classes to horizontal rulers (nth ruler modulo 3, allowing CSS to decorate it in a
--    cycling pattern, like `<div class="horizontal-rule-nth-0"><hr></div>`/`class="horizontal-rule-nth-1"`/`class="horizontal-rule-nth-2"`/`class="horizontal-rule-nth-0"`..., (the div wrapper is necessary because Pandoc's 'HorizontalRule' Block element supports no attributes)
--    like a repeating pattern of stars/moon/sun/stars/moon/sun... CSS can do this with :nth, but only
--    for immediate sub-children, it can't count elements *globally*, and since Pandoc nests horizontal
--    rulers and other block elements within each section, it is not possible to do the usual trick
--    like with blockquotes/lists).
module Typography (invertImage, invertImageInline, linebreakingTransform, typographyTransform, imageMagickDimensions, titlecase', identUniquefy, mergeSpaces, addImgDimensions, imageSrcset) where

import Control.Monad.State.Lazy (evalState, get, put, State)
import Control.Monad (void, when)
import Data.ByteString.Lazy.Char8 as B8 (unpack)
import Data.Char (toUpper)
import Data.List (isPrefixOf)
import Utils (replace)
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDay)
import System.Directory (doesFileExist, getModificationTime, removeFile)
import System.Exit (ExitCode(ExitFailure))
import System.Posix.Temp (mkstemp)
import qualified Data.Text as T (any, append, concat, isSuffixOf, pack, unpack, replace, splitOn, strip, Text, isPrefixOf, takeWhile)
import Data.Text.Read (decimal)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~), Regex, makeRegex, match) -- WARNING: avoid the native Posix 'Text.Regex' due to bugs and segfaults/strange-closure GHC errors
import System.IO (stderr, hPrint)
import System.IO.Temp (emptySystemTempFile)
import qualified Data.Map.Strict as M
import Text.HTML.TagSoup (renderTagsOptions, parseTags, renderOptions, optMinimize, optRawTag, Tag(TagOpen))
import System.FilePath (takeExtension)
import Control.Concurrent (forkIO)
import Data.Char (toLower)
import Data.Maybe (isNothing)
import Network.HTTP (urlDecode)
import Control.Exception (onException)
import Data.List (nubBy, sort)

import Data.Text.Titlecase (titlecase)

import Data.FileStore.Utils (runShellCommand)

import Text.Pandoc (Inline(..), Block(..), Pandoc, topDown, nullAttr)
import Text.Pandoc.Walk (walk, walkM)

import LinkIcon (linkIcon)
import LinkLive (linkLive)

import Utils (addClass, sed, printRed, currentYear)

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
                     first `elem` ["Et", "et", "Al", "al", "Accurate", "Aesthetics", "Africa", "After", "Alert", "America", "An", "Apr", "April", "At", "Atari", "Atlas", "August", "Autumn", "Before", "British", "Challenge", "Chat", "Codex", "Cohort", "Commodore", "Competition", "Considered", "Copyright", "Counterfactual", "Crypto", "Daily", "Dear", "Dec", "December", "Diaries", "Differences", "Early", "Enterprise", "Esthetics", "Evolution", "Expo", "Fair", "Fall", "Fanime", "Fanimecon", "Feb", "February", "First", "For", "Friday", "Impacts", "Jan", "January", "Jul", "July", "June", "Last", "Late", "Library", "Making", "Mar", "March", "May", "Memoirs", "Monday", "Monthly", "Ms", "Nov", "November", "Oct", "October", "Original", "Otakon", "Our", "Ours", "Over", "Predicting", "Reviews", "Sample", "Saturday", "Sci", "Security", "Sep", "September", "Since", "Since", "Spring", "Standard", "Statistics", "Suisse", "Summer", "Sunday", "Surface", "Survey", "Syntheses", "Than", "The", "Things", "Throughout", "Thursday", "Tuesday", "Until", "Wednesday", "Weekly", "Winter", "Writing", "Year", "Yearly", "Zilch", "In", "Judging", "From"] then -- dates like "January 2020" are false positives, although unfortunately there are real surnames like 'May', where 'May 2020' is just ambiguous and this will have a false negative.
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

-- Look at mean color of image, 0-1: if it's close to 0, then it's a monochrome-ish white-heavy
-- image. Such images look better in HTML/CSS dark mode when inverted, so we can use this to check
-- every image for color, and set an 'invert-auto' HTML class on the ones which are low. We can
-- manually specify a 'invert' class on images which don't pass the heuristic but should.
invertImageInline :: Inline -> IO Inline
invertImageInline x@(Image (htmlid, classes, kvs) xs (p,t)) =
  if notInvertP classes then
    return x else do
                   (color,_,_) <- invertFile p
                   if not color then return x else
                     return (Image (htmlid, "invert-auto":classes, kvs++[("loading","lazy")]) xs (p,t))
invertImageInline x@(Link (htmlid, classes, kvs) xs (p, t)) =
  if notInvertP classes || not (".png" `T.isSuffixOf` p || ".jpg" `T.isSuffixOf` p)  then
                                                          return x else
                                                            do (color,_,_) <- invertFile p
                                                               if not color then return x else
                                                                 return $ addClass "invert-auto" $ Link (htmlid, classes, kvs) xs (p, t)
invertImageInline x = return x

invertFile :: T.Text -> IO (Bool, String, String)
invertFile p = do let p' = T.unpack p
                  let p'' = if head p' == '/' then tail p' else p'
                  invertImage p''

notInvertP :: [T.Text] -> Bool
notInvertP classes = "invert-not" `elem` classes

invertImage :: FilePath -> IO (Bool, String, String) -- invert / height / width
invertImage f | "https://www.gwern.net/" `isPrefixOf` f = invertImageLocal $ Utils.replace "https://www.gwern.net/" "" f
              | "http" `isPrefixOf` f = do (temp,_) <- mkstemp "/tmp/image-invert"
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
invertImageLocal f = do does <- doesFileExist f
                        if not does then printRed ("invertImageLocal: " ++ f ++ " " ++ "does not exist") >> return (False, "0", "0") else
                          do c <- imageMagickColor f f
                             (h,w) <- imageMagickDimensions f
                             let invertp = c <= invertThreshold
                             return (invertp, h, w)
invertThreshold :: Float
invertThreshold = 0.09

-- Manually check the inverted version of new images which trigger the inversion heuristic. I don't
-- want to store a database of image inversion status, so I'll use the cheaper heuristic of just
-- opening up every image modified <1 day ago (which should catch all of the WP thumbnails + any
-- images added).
invertImagePreview :: FilePath -> IO ()
invertImagePreview f = do utcFile <- getModificationTime f
                          utcNow  <- getCurrentTime
                          let age  = utcNow `diffUTCTime` utcFile
                          when (age < nominalDay) $ do
                            f' <- emptySystemTempFile "inverted"
                            void $ runShellCommand "./" Nothing "convert" ["-negate", f, f']
                            void $ runShellCommand "./" Nothing "firefox" [f']

imageMagickColor :: FilePath -> FilePath -> IO Float
imageMagickColor f f' = do (status,_,bs) <- runShellCommand "./" Nothing "convert" [f', "-colorspace", "HSL", "-channel", "g", "-separate", "+channel", "-format", "%[fx:mean]", "info:"]
                           case status of
                             ExitFailure err ->  printRed ("imageMagickColor: " ++ f ++ " : ImageMagick color read error: " ++ show err ++ " " ++ f') >> return 1.0
                             _ -> do let color = readMaybe (take 4 $ unpack bs) :: Maybe Float -- WARNING: for GIFs, ImageMagick returns the mean for each frame; 'take 4' should give us the first frame, more or less
                                     case color of
                                       Nothing -> printRed ("imageMagickColor: " ++ f ++ " : ImageMagick parsing error: " ++ show (unpack bs) ++ " " ++ f') >> return 1.0
                                       Just c -> return c

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
    do exists <- doesFileExist f'
       if not exists then return ("","") else
        do (status,_,bs) <- runShellCommand "./" Nothing "identify" ["-format", "%h %w\n", f']
           case status of
             ExitFailure exit -> error $ f ++ ":" ++ f' ++ ":" ++ show exit ++ ":" ++ B8.unpack bs
             _             -> do let [height, width] = words $ head $ lines $ B8.unpack bs
                                 return (height, width)

-- Example: Image ("",["width-full"],[]) [Str "..."] ("/images/gan/thiswaifudoesnotexist.png","fig:")
-- type Text.Pandoc.Definition.Attr = (T.Text, [T.Text], [(T.Text, T.Text)])
-- WARNING: image hotlinking is a bad practice: hotlinks will often break, sometimes just because of hotlinking. We assume that all images are locally hosted! Woe betide the cheapskate parasite who fails to heed this.
imageSrcset :: Inline -> IO Inline
imageSrcset x@(Image (c, t, pairs) inlines (target, title)) =
  if not (".png" `T.isSuffixOf` target || ".jpg" `T.isSuffixOf` target) then return x else
  do let ext = takeExtension $ T.unpack target
     let target' = replace "%2F" "/" $ T.unpack target
     exists <- doesFileExist $ tail $ T.unpack target
     if not exists then printRed ("imageSrcset (Image): " ++ show x ++ " does not exist?") >> return x else
      do (_,w) <- imageMagickDimensions $ tail target'
         if w=="" || (read w :: Int) <= 768 then return x else do
             let smallerPath = (tail target')++"-768px"++ext
             notExist <- fmap not $ doesFileExist smallerPath
             when notExist $ do
               (status,_,bs) <-  runShellCommand "./" Nothing "convert" [tail target', "-resize", "768x768", smallerPath]
               case status of
                 ExitFailure _ -> error $ show status ++ show bs
                 _ -> void $ forkIO $ if ext == ".png" then -- lossily optimize using my pngnq/mozjpeg scripts:
                                        void $ runShellCommand "./" Nothing "/home/gwern/bin/bin/png" [smallerPath]
                                      else
                                        void $ runShellCommand "./" Nothing "/home/gwern/bin/bin/compressJPG" [smallerPath]
             let srcset = T.pack ("/"++smallerPath++" 768w, " ++ target'++" "++w++"w")
             return $ Image (c, t, pairs++[("srcset", srcset), ("sizes", T.pack ("(max-width: 768px) 100vw, "++w++"px"))])
                            inlines (target, title)
-- For Links to images rather than regular Images, which are not displayed (but left for the user to hover over or click-through), we still get their height/width but inline it as data-* attributes for popups.js to avoid having to reflow as the page loads. (A minor point, to be sure, but it's nicer when everything is laid out correctly from the start & doesn't reflow.)
imageSrcset x@(Link (htmlid, classes, kvs) xs (p,t)) = let p' = T.takeWhile (/='#') p in -- it is possible to have links which have '.png' or '.jpg' infix, but are not actually images, such as, in tag-directories, section headers for images: '/docs/statistics/survival-analysis/index#filenewbie-survival-by-semester-rows.png'; special-case that
                                                         if (".png" `T.isSuffixOf` p' || ".jpg" `T.isSuffixOf` p') &&
                                                          ("https://www.gwern.net/" `T.isPrefixOf` p || "/" `T.isPrefixOf` p) then
                                                         do exists <- doesFileExist $ tail $ replace "https://www.gwern.net" "" $ T.unpack  p'
                                                            if not exists then printRed ("imageSrcset (Link): " ++ show x ++ " does not exist?") >> return x else
                                                              do (h,w) <- imageMagickDimensions $ T.unpack p'
                                                                 return (Link (htmlid, classes,
                                                                               kvs++[("image-height",T.pack h),
                                                                                      ("image-width",T.pack w)])
                                                                         xs (p,t))
                                                       else return x
imageSrcset x = return x

-- FASTER HTML RENDERING BY STATICLY SPECIFYING ALL IMAGE DIMENSIONS
-- read HTML string with TagSoup, process `<img>` tags to read the file's dimensions, and hardwire them;
-- this optimizes HTML rendering since browsers know before downloading the image how to layout the page.
-- Further, specify 'async' decoding & 'lazy-loading' for all images: the lazy attribute was introduced by Chrome 76 ~August 2019, and adopted by Firefox 75 ~February 2020 (<https://bugzilla.mozilla.org/show_bug.cgi?id=1542784>), standardized as <https://html.spec.whatwg.org/multipage/urls-and-fetching.html#lazy-loading-attributes> with >63% global availability + backwards compatibility (<https://caniuse.com/#feat=loading-lazy-attr> <https://github.com/whatwg/html/pull/3752> <https://web.dev/native-lazy-loading/>).
-- Async decoding: when an image *does* load, can it be decoded to pixels in parallel with the text? For us, yes. Docs: <https://html.spec.whatwg.org/multipage/images.html#decoding-images> <https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/decoding>.
-- Pandoc feature request to push the lazy loading upstream: <https://github.com/jgm/pandoc/issues/6197>
addImgDimensions :: String -> IO String
addImgDimensions = fmap (renderTagsOptions renderOptions{optMinimize=whitelist, optRawTag = (`elem` ["script", "style"]) . map toLower}) . mapM staticImg . parseTags
                 where whitelist s = s /= "div" && s /= "script" && s /= "style"

{- example illustration:
 TagOpen "img" [("src","/images/traffic/201201-201207-traffic-history.png")
                ("alt","Plot of page-hits (y-axis) versus date (x-axis)")],
 TagOpen "figcaption" [],TagText "Plot of page-hits (y-axis) versus date (x-axis)",
 TagClose "figcaption",TagText "\n",TagClose "figure" -}
staticImg :: Tag String -> IO (Tag String)
staticImg x@(TagOpen "img" xs) = do
  let h = lookup "height" xs
  let w = lookup "width" xs
  let lazy = lookup "loading" xs
  let Just p = lookup "src" xs
  if (isNothing h || isNothing w || isNothing lazy) &&
     not ("//" `isPrefixOf` p || "http" `isPrefixOf` p) &&
     ("/" `isPrefixOf` p && not ("data:image/" `isPrefixOf` p)) then
    if (takeExtension p == ".svg") then
      -- for SVGs, only set the lazy-loading attribute, since height/width is not necessarily meaningful for vector graphics
            return (TagOpen "img" (uniq (("loading", "lazy"):xs)))
    else
       do
         let p' = urlDecode $ if head p == '/' then tail p else p
         exists <- doesFileExist p'
         if not exists then printRed ("staticImg: File does not exist: " ++ p') >> return x else
          do (height,width) <- imageMagickDimensions p' `onException` printRed p
             -- body max-width is 1600 px, sidebar is 150px, so any image wider than ~1400px
             -- will wind up being reflowed by the 'img { max-width: 100%; }' responsive-image CSS declaration;
             -- let's avoid that specific case by lying about its width, although this doesn't fix all the reflowing.
             -- No images should be more than a screen in height either, so we'll set a maximum of 1400
             let width' =  readMaybe width  ::Maybe Int
             let height' = readMaybe height ::Maybe Int
             case width' of
                Nothing       -> printRed ("staticImg: Image width can't be read: " ++ show x) >> return x
                Just width'' -> case height' of
                                 Nothing       -> printRed ("staticImg: Image height can't be read: " ++ show x) >> return x
                                 Just height'' -> return (TagOpen "img" (uniq ([("loading", "lazy"), -- lazy load & async render all images
                                                                                ("decoding", "async"),
                                                                                ("height", show (height'' `min` 1400)), ("width", show (width'' `min` 1400))]++xs)))
      else return x
  where uniq = nubBy (\a b -> fst a == fst b) . sort
staticImg x = return x

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
                            return $ Link (ident `T.append` "-" `T.append` (T.pack (show (count + 1))),
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
titlecase' t = titlecase $ titlecase'' t
   where titlecase'' :: String -> String
         titlecase'' "" = ""
         titlecase'' t' = let (before,matched,after) =  t' =~ ("\\-[a-z]"::String) :: (String,String,String)
                          in before ++ map toUpper matched ++ titlecase'' after
