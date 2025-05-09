{-# LANGUAGE OverloadedStrings #-}
-- Optimize or stylize images
module Image where

import Control.Exception (onException)
import Control.Monad (void, when, unless)
import Data.ByteString.Lazy.Char8 as B8 (unpack)
import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf, nubBy, sort)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map as M
import Data.Ratio as R (denominator, numerator, (%))
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDay)
import Network.HTTP (urlDecode)
import System.Directory (doesFileExist, getModificationTime, removeFile)
import System.Exit (ExitCode(ExitFailure))
import System.FilePath (takeExtension)
import System.IO.Temp (emptySystemTempFile)
import System.Posix.Temp (mkstemp)
import Text.HTML.TagSoup (renderTagsOptions, parseTags, renderOptions, optRawTag, Tag(TagOpen)) -- optMinimize,
import Text.Read (readMaybe)
import qualified Data.Text as T (isSuffixOf, pack, takeWhile, unpack, Text, replace, head)

import Data.FileStore.Utils (runShellCommand)

import Text.Pandoc (Inline(Image, Link))

import LinkMetadataTypes (Metadata)
import Utils (addClass, printRed, anySuffix, isLocal, kvLookup, delete, trim)

-- does the filename claim to be an image-type we support? (ignores hash-anchors, so `/doc/rl/2024-foo.jpg#deepmind` → True)
-- excludes ".psd"
isImageFilename :: FilePath -> Bool
isImageFilename "" = error "Image.isImageFilename: passed empty string as filepath!"
isImageFilename i  = not ("http" `isPrefixOf` i) && -- we need to avoid doppelganger URLs like Wikimedia Commons where a '.jpg' or '.png' suffix is the HTML page *for* a file, but we also don't want to do IO to check arbitrary paths like 'doc/foo.jpg' for validity, so we just ignore anything starting with 'http' as that would have to be a hotlink and we avoid those to begin with
                     anySuffix (takeWhile (/='#') i) [".bmp", ".gif", ".ico", ".jpg", ".png", ".svg", ".xcf"]

isVideoFilename :: FilePath -> Bool
isVideoFilename "" = error "Image.isVideoFilename: passed empty string as filepath!"
isVideoFilename i  = not ("http" `isPrefixOf` i) && anySuffix (takeWhile (/='#') i) [".mp4", ".webm", ".avi"] -- we support only 2 types of video on Gwern.net at present

-------------------------------------------
-- Dark-mode

-- Look at mean color of image, 0-1: if it's close to 0, then it's a monochrome-ish white-heavy
-- image. Such images look better in HTML/CSS dark mode when inverted, so we can use this to check
-- every image for color, and set an 'invert-auto' HTML class on the ones which are low. We can
-- manually specify a 'invert' class on images which don't pass the heuristic but should.
--
-- TODO: the long-term plan is to remove the ImageMagick heuristic in favor of Invertornot.com, but to cache the ION calls in the LinkMetadata DB inside the 'miscellaneous' field; then all images without a manual .invert/.invert-not simply get a lookup or local ION call. This removes as much work from runtime browsers as possible and reduces load on ION, as well as robustness for when ION inevitably goes offline someday. (We cannot remove ION completely from the runtime, because we still need it for dynamic sources like Wikipedia. But most images are static.)
invertImageInline :: Metadata -> Inline -> IO Inline
invertImageInline _ x@(Image _ _ ("",_)) = error $ "Image.invertImageInline called with a malformed image with no filename; this should never happen. Argument: " ++ show x
invertImageInline md x@(Image (htmlid, classes, kvs) xs (p,t)) =
  do let inverted = addLazyLoadingImage $ Image (htmlid, "invert-auto":classes, kvs) xs (p,t)
     case invertGlobalOverride md (T.unpack p) of
       Just True -> return inverted
       Just False -> return x
       Nothing -> if notInvertP classes then
                      return x else do
                                     (color,_,_) <- invertFile p
                                     if not color then return x else
                                       return inverted
invertImageInline _ x@(Link (htmlid, classes, kvs) xs (p, t)) =
  if notInvertP classes || not (".png" `T.isSuffixOf` p || ".jpg" `T.isSuffixOf` p)  then
                                                          return x else
                                                            do (color,_,_) <- invertFile p
                                                               if not color then return x else
                                                                 return $ addClass "invert-auto" $ Link (htmlid, classes, kvs) xs (p, t)
invertImageInline _ x = return x

invertGlobalOverride :: Metadata -> FilePath -> Maybe Bool
invertGlobalOverride md p = case M.lookup p md of
  Nothing -> Nothing
  Just (_,_,_,_,kvs,_,_) -> case kvLookup "invert" kvs  of
               "" -> Nothing
               "True" -> Just True
               "False" -> Just False
               _ -> error $ "Image.invertGlobalOverride: impossible pattern match result for: " ++ show p ++ " : " ++ show kvs

invertFile :: T.Text -> IO (Bool, String, String)
invertFile "" = error "Image.invertFile called with empty filename; that should never happen."
invertFile p = do let p' = T.unpack p
                  let p'' = if head p' == '/' then tail p' else p'
                  when (p'' == "") $ error $ "Image.invertFile: file argument became empty string, rendering it invalid; input was: " ++ show p
                  invertImage p''

notInvertP :: [T.Text] -> Bool
notInvertP classes = "invert-not" `elem` classes

invertImage :: FilePath -> IO (Bool, String, String) -- invert / height / width
invertImage f | "" == f = error "Image.invertImage: argument was empty! This should never happen,"
              | "https://gwern.net/" `isPrefixOf` f = invertImageLocal $ Utils.delete "https://gwern.net/" f
              | "http" `isPrefixOf` f =
                -- TODO: after localizing all legacy hotlinked images, remove the download code & replace with a fatal error.
                do (_,_,mimetype) <- runShellCommand "./" Nothing "curl" ["--silent", "--user-agent", "gwern+imagescraping@gwern.net", f, "--write-out", "'%{content_type}'"]
                   if not ("image/" `isPrefixOf` unpack mimetype) then return (False, "320", "320") else
                     do (temp,_) <- mkstemp "/tmp/image-invert"
                        -- NOTE: while wget preserves it, curl erases the original modification time reported by server in favor of local file creation; this is useful for `invertImagePreview` --- we want to check downloaded images manually before their annotation gets stored permanently.
                        (status,_,_) <- runShellCommand "./" Nothing "curl" ["--location", "--silent", "--user-agent", "gwern+imagescraping@gwern.net", f, "--output", temp]
                        case status of
                          ExitFailure _ -> do printRed ("Download failed (unable to check image invertibility): " ++ f)
                                              removeFile temp
                                              return (False, "320", "320") -- NOTE: most WP thumbnails are 320/320px squares, so to be safe we'll use that as a default value
                          _ -> do c <- imageMagickColor f temp
                                  (h,w) <- imageMagickDimensions temp
                                  let invertp = c < invertThreshold
                                  when invertp $ invertImagePreview temp
                                  removeFile temp
                                  return (invertp, h, w)
              | otherwise = invertImageLocal f

-- unfortunately, this heuristic often fails, eg. all of </doc/ai/nn/diffusion/midjourney/black-sun/*.jpg>
invertImageLocal :: FilePath -> IO (Bool, String, String)
invertImageLocal "" = error "Image.invertImageLocal called with an empty filename, that should never happen!"
invertImageLocal f = do let f' = takeWhile (/='#') f
                        if not (anySuffix f' [".png", ".jpg", ".webp"]) then return (False, "0", "0")  else
                         do does <- doesFileExist f'
                            if not does then printRed ("invertImageLocal: " ++ f ++ " " ++ f' ++ " does not exist") >> return (False, "0", "0") else
                              do c <- imageMagickColor f' f'
                                 (h,w) <- imageMagickDimensions f'
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
                            void $ runShellCommand "./" Nothing "x-www-browser" [f']

imageMagickColor :: FilePath -> FilePath -> IO Float
imageMagickColor f f' = do let temp = if null f' then f else f'
                           (status,_,bs) <- runShellCommand "./" Nothing "convert" [temp, "-colorspace", "HSL", "-channel", "g", "-separate", "+channel", "-format", "%[fx:mean]", "info:"]
                           case status of
                             ExitFailure err ->  printRed ("imageMagickColor: " ++ f ++ " : ImageMagick color read error: " ++ show err ++ " " ++ temp) >> return 1.0
                             _ -> do let color = readMaybe (take 4 $ unpack bs) :: Maybe Float -- WARNING: for GIFs, ImageMagick returns the mean for each frame; 'take 4' should give us the first frame, more or less
                                     case color of
                                       Nothing -> printRed ("imageMagickColor: " ++ f ++ " : ImageMagick parsing error: " ++ show (unpack bs) ++ " " ++ temp) >> return 1.0
                                       Just c -> return c

outlineImage :: FilePath -> IO Bool
outlineImage "" = error "Image.outlineImage called with an empty filename, that should never happen!"
outlineImage f  = do let f' = (if head f /= '/' then id else tail) $ takeWhile (/='#') f
                     unless (isImageFilename f) $ error $ "Image.outlineImage called on a non-image filename? " ++ f
                     does <- doesFileExist f'
                     unless does $ error $ "Image.outlineImage called on a file which doesn't exist? File was: " ++ f ++ "; converted to: " ++ f'
                     x@(status,_,bs) <- runShellCommand "./" Nothing "php" ["static/build/should_image_have_outline.php", f']
                     case status of
                              ExitFailure _ ->  error $ "Image.outlineImage PHP command returned failure!" ++ show x ++ "; " ++ f
                              _ -> do
                                let outline = take 1 $ trim $ unpack bs
                                when (outline == "") $ error $ "Image.outlineImage called on a file but 'should_image_have_outline.php' returned nothing? File was: " ++ f ++ "; converted to: " ++ f'
                                return $ outline == "1"

outlineImageInline :: Inline -> IO Inline
outlineImageInline x@(Image _ _ ("",_)) = error $ "Image.outlineImageInline called with a malformed image with no filename; this should never happen. Argument: " ++ show x
outlineImageInline x@(Image (htmlid, classes, kvs) xs (p,t)) =
  if "outline" `elem` classes || "outline-not" `elem` classes then return x else
       if T.head p /= '/' then return x else
         do outline <- outlineImage (T.unpack p)
            let outlineClass = if outline then "outline" else "outline-not"
            return $ Image (htmlid, outlineClass:classes, kvs) xs (p,t)
outlineImageInline x = return x

-- | Use FileStore utility to run imageMagick's 'identify', & extract the height/width dimensions
-- Note that for animated GIFs, 'identify' returns width/height for each frame of the GIF, which in
-- most cases will all be the same, so we take the first line of whatever dimensions 'identify' returns.
--
-- For an SVG, there is in fact a 'width' and 'height', set in the 'viewbox', like `<svg xmlns="https://www.w3.org/2000/svg" viewBox="0 0 1200 830">`.
-- This doesn't mean the same thing as in a raster image because that's the point of vector graphics, but this does still tell us useful things like the aspect ratio (in this case, 1200 / 830 = 1.44578).
imageMagickDimensions :: FilePath -> IO (String,String)
imageMagickDimensions f =
  let f'
        | "/" `isPrefixOf` f && not ("/tmp" `isPrefixOf` f) = tail f
        | "https://gwern.net/" `isPrefixOf` f = drop 22 f
        | otherwise = f
  in
    do exists <- doesFileExist f'
       if not exists then return ("","") else
        do let f'' = if isVideoFilename f' then f' ++ "-poster.jpg" else f'
           (status,_,bs) <- runShellCommand "./" Nothing "identify" ["-ping", "-format", "%h %w\n", f'']
           case status of
             ExitFailure exit -> error $ "Image.imageMagickDimensions: `identify` exited with a failure on `f`=" ++ f ++ " : `f''=`" ++ f'' ++ "; exit status: " ++ show exit ++ ": raw exit status: " ++ B8.unpack bs
             _             -> do let string = B8.unpack bs
                                 let dimensions = words $ head $ lines string
                                 case dimensions of
                                   [height, width] -> return (height, width)
                                   _ -> error $ "Image.imageMagickDimensions: parsing of returned image dimensions failed, was: " ++ show dimensions

-- FASTER HTML RENDERING BY STATICLY SPECIFYING ALL IMAGE DIMENSIONS
-- read HTML string with TagSoup, process `<img>` tags to read the file's dimensions, and hardwire them;
-- this optimizes HTML rendering since browsers know before downloading the image how to layout the page.

-- Async decoding: when an image *does* load, can it be decoded to pixels in parallel with the text? For us, yes. Docs: <https://html.spec.whatwg.org/multipage/images.html#decoding-images> <https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/decoding>.
-- ~~For convenience, this will also run `addVideoPoster` on the HTML stream as well, to do the equivalent thing for `<video>` files (ie. generate a 'poster' image thumbnail, adding it to the video tags, and including the poster's image dimensions inasmuch as it is not inside a `<img>` & so is not covered by the `<img>`-processing code).~~~
--
-- Pandoc feature request to push the lazy loading upstream: <https://github.com/jgm/pandoc/issues/6197>
addImgDimensions :: String -> IO String
addImgDimensions html =
  if not ("<img " `isInfixOf` html) then return html
  else do let stream  = parseTags html
          dimensionized <- mapM staticImg stream
          -- posterized    <- addVideoPoster dimensionized
          let stream' = renderTagsOptions renderOptions{ -- optMinimize= const False, -- don't minimize anything at all, this seems to just cause a lot of problems later on
                                                        optRawTag = (`elem` ["script", "style"]) . map toLower}
                        dimensionized
          return stream'
          -- fmap (renderTagsOptions renderOptions{optMinimize=whitelist, optRawTag = (`elem` ["script", "style"]) . map toLower}) . mapM staticImg <=< addVideoPoster . parseTags

{- example illustration:
 TagOpen "img" [("src","/doc/traffic/2012-01-02-2012-07-02-gwern-gwern-traffic-history.png")
                ("alt","Plot of page-hits (y-axis) versus date (x-axis)")],
 TagOpen "figcaption" [],TagText "Plot of page-hits (y-axis) versus date (x-axis)",
 TagClose "figcaption",TagText "\n",TagClose "figure" -}
staticImg :: Tag String -> IO (Tag String)
staticImg x@(TagOpen "img" xs) = do
  let h = lookup "height" xs
  let w = lookup "width" xs
  let lazy = lookup "loading" xs
  let loading = if isJust lazy then [] else [("loading","lazy")] -- don't override a loading="eager"
  let src = lookup "src" xs
  case src of
    Nothing -> error $ "Image.staticImg: no 'src' set on '<img>' tag? This should never happen. Original: " ++ show x
    Just p ->
     if (isNothing h || isNothing w || isNothing lazy) &&
       not ("//" `isPrefixOf` p || "http" `isPrefixOf` p) &&
       ("/" `isPrefixOf` p && not ("data:image/" `isPrefixOf` p)) then
         do
           let p' = urlDecode $ takeWhile (/='#') $ if head p == '/' then tail p else p
           exists <- doesFileExist p'
           if not exists then printRed "staticImg: File does not exist: " >> putStrLn p >> return x else
            do (height,width) <- imageMagickDimensions p' `onException` printRed p
               -- body max-width is 1600 px, sidebar is 150px, so any image wider than ~1400px
               -- will wind up being reflowed by the 'img { max-width: 100%; }' responsive-image CSS declaration;
               -- let's avoid that specific case by lying about its width, although this doesn't fix all the reflowing.
               let width' =  readMaybe width  ::Maybe Int
               let height' = readMaybe height ::Maybe Int
               case width' of
                  Nothing       -> printRed "staticImg: Image width can't be read: " >> print x >> return x
                  Just width'' -> case height' of
                                   Nothing       -> printRed "staticImg: Image height can't be read: " >> print x >> return x
                                   Just height'' ->
                                     let dims = sizeAspectRatioKV width'' height''
                                     in
                                       if (takeExtension p == ".svg") then
                                         -- for SVGs, only set the lazy-loading attribute, since height/width is not necessarily meaningful for vector graphics
                                         return $ TagOpen "img" $ uniq $ loading ++ dims ++ xs
                                       else
                                         -- lazy load & async render all images
                                         return $ TagOpen "img" $ uniq $ loading ++ [("decoding", "async")] ++ dims ++ xs
    else return x
  where uniq = nubBy (\a b -> fst a == fst b) . sort
staticImg x = return x

-- return image dimensions packed into data-aspect-ratio/height/width key-value dictionary suitable for a <a> or <img>:
sizeAspectRatioKV :: Int -> Int -> [(String,String)]
sizeAspectRatioKV width height = -- preserve aspect ratio when we have to shrink to the minimum width:
                             let imageWidth = width `min` 1400
                                 imageShrunk = width /= imageWidth
                                 imageShrinkRatio = (1400::Float) / (fromIntegral width :: Float)
                                 imageRatioReduced = imageWidth % imageHeight
                                 imageHeight = if not imageShrunk then height else round (fromIntegral height * imageShrinkRatio)
                              in [("data-aspect-ratio", show (R.numerator imageRatioReduced) ++ " / " ++ show (R.denominator imageRatioReduced)),
                                  ("height", show imageHeight),
                                  ("width", show imageWidth) ]

-- For Links to images rather than regular Images, which are not displayed (but left for the user to hover over or click-through), we still get their height/width but inline it as data-* attributes for popups.js to avoid having to reflow as the page loads. (A minor point, to be sure, but it's nicer when everything is laid out correctly from the start & doesn't reflow.)
imageLinkHeightWidthSet :: Inline -> IO Inline
imageLinkHeightWidthSet x@(Link _ _ ("",_)) = error $ "Image.imageLinkHeightWidthSet: passed an Inline path with no URL set? Input was: " ++ show x
imageLinkHeightWidthSet x@(Link (htmlid, classes, kvs) xs (p,t)) =
  let dimensionp = lookup "image-height" kvs in
    if isJust dimensionp then return x else
     let p' = T.unpack $ T.takeWhile (/='#') $ T.replace "https://gwern.net/" "/" p in
      if p' == "" then return x -- if it was an empty string after the `takeWhile` but wasn't before, then `p` was an anchor self-link, presumably, and we skip it.
      else
        if (isImageFilename p' || isVideoFilename p') && isLocal (T.pack p') then
        do exists <- doesFileExist $ tail p'
           if not exists then printRed "imageLinkHeightWidthSet: " >> putStr (show x) >> printRed " does not exist?" >> return x else
             do let p'' = if isVideoFilename p' then p' ++ "-poster.jpg" else p'
                (h,w) <- imageMagickDimensions p''
                let aspectratio = map (\(a,b) -> (T.pack a, T.pack b)) $ take 1 $ sizeAspectRatioKV (read w::Int) (read h::Int)
                return (Link (htmlid, classes,
                              kvs++[("image-height",T.pack h),
                                    ("image-width", T.pack w)] ++ aspectratio)
                        xs (p,t))
        else return x
imageLinkHeightWidthSet x = return x

-- Further, specify 'async' decoding & 'lazy-loading' for all images: the lazy attribute was introduced by Chrome 76 ~August 2019, and adopted by Firefox 75 ~February 2020 (<https://bugzilla.mozilla.org/show_bug.cgi?id=1542784>), standardized as <https://html.spec.whatwg.org/multipage/urls-and-fetching.html#lazy-loading-attributes> with >63% global availability + backwards compatibility (<https://caniuse.com/#feat=loading-lazy-attr> <https://github.com/whatwg/html/pull/3752> <https://web.dev/native-lazy-loading/>).
-- Negation: `loading="eager"`
addLazyLoadingImage :: Inline -> Inline
addLazyLoadingImage i@(Image (c, t, pairs) inlines (target, title)) = let lazy = lookup "loading" pairs in
                                                                      if isJust lazy then i else Image (c, t, ("loading","lazy"):pairs) inlines (target, title)
addLazyLoadingImage x = x
