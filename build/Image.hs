{-# LANGUAGE OverloadedStrings #-}
-- Optimize or stylize images
module Image where

import Control.Exception (onException)
import Control.Monad (unless, void, when) -- (<=<)
import Data.ByteString.Lazy.Char8 as B8 (unpack)
import Data.Char (toLower)
import Data.List (isPrefixOf, nubBy, sort)
import Data.Maybe (isNothing)
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDay)
import Network.HTTP (urlDecode)
import System.Directory (doesFileExist, getModificationTime, removeFile)
import System.Exit (ExitCode(ExitFailure))
import System.FilePath (takeExtension)
import System.IO.Temp (emptySystemTempFile)
import System.Posix.Temp (mkstemp)
import Text.HTML.TagSoup (renderTagsOptions, parseTags, renderOptions, optMinimize, optRawTag, Tag(TagOpen))
import Text.Read (readMaybe)
import qualified Data.Text as T (isPrefixOf, isSuffixOf, pack, takeWhile, unpack, Text)

import Data.FileStore.Utils (runShellCommand)

import Text.Pandoc (Inline(Image, Link))

import Utils (addClass, printRed, replace, anySuffix)

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
                     return (addLazyLoadingImage $ Image (htmlid, "invert-auto":classes, kvs) xs (p,t))
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
invertImage f | "https://gwern.net/" `isPrefixOf` f = invertImageLocal $ Utils.replace "https://gwern.net/" "" f
              | "http" `isPrefixOf` f =
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

invertImageLocal :: FilePath -> IO (Bool, String, String)
invertImageLocal "" = return (False, "0", "0")
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
                            void $ runShellCommand "./" Nothing "$X_WWW_BROWSER" [f']

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
--
-- For an SVG, there is in fact a 'width' and 'height', set in the 'viewbox', like `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1200 830">`.
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
        do (status,_,bs) <- runShellCommand "./" Nothing "identify" ["-format", "%h %w\n", f']
           case status of
             ExitFailure exit -> error $ f ++ ":" ++ f' ++ ":" ++ show exit ++ ":" ++ B8.unpack bs
             _             -> do let [height, width] = words $ head $ lines $ B8.unpack bs
                                 return (height, width)

-- FASTER HTML RENDERING BY STATICLY SPECIFYING ALL IMAGE DIMENSIONS
-- read HTML string with TagSoup, process `<img>` tags to read the file's dimensions, and hardwire them;
-- this optimizes HTML rendering since browsers know before downloading the image how to layout the page.

-- Async decoding: when an image *does* load, can it be decoded to pixels in parallel with the text? For us, yes. Docs: <https://html.spec.whatwg.org/multipage/images.html#decoding-images> <https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/decoding>.
-- For convenience, this will also run `addVideoPoster` on the HTML stream as well, to do the equivalent thing for `<video>` files (ie. generate a 'poster' image thumbnail, adding it to the video tags, and including the poster's image dimensions inasmuch as it is not inside a `<img>` & so is not covered by the `<img>`-processing code).
--
-- Pandoc feature request to push the lazy loading upstream: <https://github.com/jgm/pandoc/issues/6197>
addImgDimensions :: String -> IO String
addImgDimensions html = do let stream  = parseTags html
                           dimensionized <- mapM staticImg stream
                           posterized    <- addVideoPoster dimensionized
                           let stream' = renderTagsOptions renderOptions{optMinimize=whitelist,
                                                                         optRawTag = (`elem` ["script", "style"]) . map toLower}
                                         posterized
                           return stream'
                           -- fmap (renderTagsOptions renderOptions{optMinimize=whitelist, optRawTag = (`elem` ["script", "style"]) . map toLower}) . mapM staticImg <=< addVideoPoster . parseTags
                 where whitelist s = s /= "div" && s /= "script" && s /= "style"

-- x = "          <section id=\"video\" class=\"level2\">\n            <h2><a href=\"#video\" title=\"Link to section: § 'Video'\">Video</a></h2>\n            <figure>\n              <video controls=\"controls\" preload=\"none\" loop=\"\"><source src=\"/doc/ai/nn/gan/biggan/2019-06-03-gwern-biggan-danbooru1k-256px.mp4\" type=\"video/mp4\"></video>\n              <figcaption>\n                <a href=\"/doc/ai/nn/gan/biggan/2019-06-03-gwern-biggan-danbooru1k-256px.mp4\">Training montage</a> of the 256px Danbooru2018-1K<a href=\"#fn4\" class=\"footnote-ref\" id=\"fnref4\" role=\"doc-noteref\"><sup>4</sup></a>\n              </figcaption>\n            </figure>\n          </section>"

-- VIDEO POSTER images
-- use tagsoup to go through a list of HTML tags looking for a `<video>` tag set of the form
-- "... <figure><video controls='controls' preload='none' loop><source src='/doc/ai/nn/gan/biggan/2019-06-03-gwern-biggan-danbooru1k-256px.mp4' type='video/mp4'></video><figcaption><a href='/doc/ai/nn/gan/biggan/2019-06-03-gwern-biggan-danbooru1k-256px.mp4'>Training montage</a> of the 256px Danbooru2018-1K^[Footnote 9.]</figcaption></figure> ..."
-- where the `<source>` can be parsed for the `src` attribute specifying the absolute video filepath. Then ffmpeg is called on it to dump the second frame (to avoid initial black frames), to filepath+"-poster.jpg".
-- This lets readers see what a video looks like beforehand, and also helps avoid layout shift when there is no poster & the video starts with much larger dimensions than the browser guessed.
--
-- | Process a list of HTML tags, looking for consecutive <video> and <source> tags.
-- If found, extract the video file path from the <source> tag and generate a poster image
-- from the second frame of the video. Then, add the poster image as an attribute to the <video> tag,
-- along with the image dimensions.
-- (GPT-4-rewritten.)
addVideoPoster :: [Tag String] -> IO [Tag String]
addVideoPoster [] = return []
addVideoPoster (videoTag@(TagOpen "video" _):sourceTag@(TagOpen "source" sourceAttrs):xs) = do
    print $ "Image.hs: addVideoPoster: sourceTag: " ++ show sourceTag
    updatedVideoTag <- updateVideoTag videoTag sourceAttrs
    rest <- addVideoPoster xs
    return (updatedVideoTag : sourceTag : rest)
addVideoPoster (x:xs) = (x :) <$> addVideoPoster xs
-- | Update the given 'videoTag' with a poster attribute and dimensions, if a valid 'sourceAttrs' is provided.
-- Extract the video file path from 'sourceAttrs', generate a poster image, and add the poster image
-- and dimensions as attributes to the 'videoTag'.
updateVideoTag :: Tag String -> [(String, String)] -> IO (Tag String)
updateVideoTag videoTag sourceAttrs =
    case lookup "src" sourceAttrs of
        Nothing -> error "Image.hs: addVideoPoster: updateVideoTag: video-path not found in source tag."
        Just videoPath | head videoPath == '/' -> do
            posterPath <- generatePoster (tail videoPath)
            (height, width) <- imageMagickDimensions ("/" ++ posterPath)
            let updatedAttrs = [("poster", "/" ++ posterPath), ("height", height), ("width", width)]
            return $ updateTagAttributes videoTag updatedAttrs
        Just videoPath -> error $ "Image.hs: addVideoPoster: updateVideoTag: video-path (" ++ videoPath ++ ") didn't start with a '/'. Videos must be localized."
-- | Generate the poster image for the given video path.
-- If the poster image file does not already exist, use ffmpeg to create it from the second frame of the video.
generatePoster :: FilePath -> IO FilePath
generatePoster videoPath = do
    let posterPath = videoPath ++ "-poster.jpg"
    existsp <- doesFileExist posterPath
    unless existsp $ void $ runShellCommand "./" Nothing "ffmpeg" ["-i", videoPath, "-vf", "select=eq(n\\,1),scale=iw*sar:ih,setsar=1", "-vframes", "1", posterPath]
    return posterPath
-- | Update the attributes of a given tag by appending the provided new attributes.
updateTagAttributes :: Tag String -> [(String, String)] -> Tag String
updateTagAttributes (TagOpen tagType attrs) newAttrs = TagOpen tagType (attrs ++ newAttrs)
updateTagAttributes tag _ = tag

{- example illustration:
 TagOpen "img" [("src","/doc/traffic/201201-201207-gwern-traffic-history.png")
                ("alt","Plot of page-hits (y-axis) versus date (x-axis)")],
 TagOpen "figcaption" [],TagText "Plot of page-hits (y-axis) versus date (x-axis)",
 TagClose "figcaption",TagText "\n",TagClose "figure" -}
staticImg :: Tag String -> IO (Tag String)
staticImg x@(TagOpen "img" xs) = do
  let h = lookup "height" xs
  let w = lookup "width" xs
  let lazy = lookup "loading" xs
  let loading = if isNothing lazy then [("loading","lazy")] else [] -- don't override a loading="eager"
  let Just p = lookup "src" xs
  if (isNothing h || isNothing w || isNothing lazy) &&
     not ("//" `isPrefixOf` p || "http" `isPrefixOf` p) &&
     ("/" `isPrefixOf` p && not ("data:image/" `isPrefixOf` p)) then
       do
         let p' = urlDecode $ if head p == '/' then tail p else p
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
                                   let -- preserve aspect ratio when we have to shrink to the minimum width:
                                       imageWidth = width'' `min` 1400
                                       imageShrunk = width'' /= imageWidth
                                       imageShrinkRatio = (1400::Float) / (fromIntegral width'' :: Float)
                                       imageHeight = if not imageShrunk then height'' else round (fromIntegral height'' * imageShrinkRatio)
                                   in
                                     if (takeExtension p == ".svg") then
                                       -- for SVGs, only set the lazy-loading attribute, since height/width is not necessarily meaningful for vector graphics
                                       return (TagOpen "img" (uniq (("loading", "lazy"):
                                                                    ("data-aspect-ratio", show width'' ++ " / " ++ show height''):xs)))
                                     else
                                       return (TagOpen "img" (uniq (loading ++  -- lazy load & async render all images
                                                                   [("decoding", "async"),
                                                                     ("height", show imageHeight), ("width", show imageWidth)]++xs)))
    else return x
  where uniq = nubBy (\a b -> fst a == fst b) . sort
staticImg x = return x

-- For Links to images rather than regular Images, which are not displayed (but left for the user to hover over or click-through), we still get their height/width but inline it as data-* attributes for popups.js to avoid having to reflow as the page loads. (A minor point, to be sure, but it's nicer when everything is laid out correctly from the start & doesn't reflow.)
imageLinkHeightWidthSet :: Inline -> IO Inline
imageLinkHeightWidthSet x@(Link (htmlid, classes, kvs) xs (p,t)) =
                                                        let p' = T.takeWhile (/='#') p in
                                                         if (".png" `T.isSuffixOf` p' || ".jpg" `T.isSuffixOf` p') &&
                                                          ("https://gwern.net/" `T.isPrefixOf` p || "/" `T.isPrefixOf` p) then
                                                         do exists <- doesFileExist $ tail $ replace "https://gwern.net" "" $ T.unpack  p'
                                                            if not exists then printRed "imageLinkHeightWidthSet: " >> putStr (show x) >> printRed " does not exist?" >> return x else
                                                              do (h,w) <- imageMagickDimensions $ T.unpack p'
                                                                 return (Link (htmlid, classes,
                                                                               kvs++[("image-height",T.pack h),
                                                                                      ("image-width",T.pack w)])
                                                                         xs (p,t))
                                                       else return x
imageLinkHeightWidthSet x = return x

-- Further, specify 'async' decoding & 'lazy-loading' for all images: the lazy attribute was introduced by Chrome 76 ~August 2019, and adopted by Firefox 75 ~February 2020 (<https://bugzilla.mozilla.org/show_bug.cgi?id=1542784>), standardized as <https://html.spec.whatwg.org/multipage/urls-and-fetching.html#lazy-loading-attributes> with >63% global availability + backwards compatibility (<https://caniuse.com/#feat=loading-lazy-attr> <https://github.com/whatwg/html/pull/3752> <https://web.dev/native-lazy-loading/>).
-- Negation: `loading="eager"`
addLazyLoadingImage :: Inline -> Inline
addLazyLoadingImage i@(Image (c, t, pairs) inlines (target, title)) = let lazy = lookup "loading" pairs in
                                                                      if isNothing lazy then Image (c, t, ("loading","lazy"):pairs) inlines (target, title) else i
addLazyLoadingImage x = x
