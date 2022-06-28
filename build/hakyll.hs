#!/usr/bin/env runghc
{-# LANGUAGE OverloadedStrings #-}

{-
Hakyll file for building Gwern.net
Author: gwern
Date: 2010-10-01
When: Time-stamp: "2022-06-27 18:59:05 gwern"
License: CC-0

Debian dependencies:
$ sudo apt-get install libghc-hakyll-dev libghc-pandoc-dev libghc-filestore-dev libghc-tagsoup-dev libghc-yaml-dev imagemagick s3cmd git libghc-aeson-dev libghc-missingh-dev libghc-digest-dev tidy gridsite-clients

(GHC is needed for Haskell; Hakyll & Pandoc do the heavy lifting of compiling Markdown files to HTML; tag soup & ImageMagick are runtime dependencies used to help optimize images, and s3cmd/git upload to hosting/Github respectively.)
Demo command (for the full script, with all static checks & generation & optimizations, see `sync-gwern.net.sh`):

$ cd ~/wiki/ && ghc -rtsopts -threaded -O2 -fforce-recomp -optl-s --make hakyll.hs &&
  ./hakyll rebuild +RTS -N3 -RTS && echo -n -e '\a'  &&
  s3cmd -v -v --human-readable-sizes --reduced-redundancy --no-mime-magic --guess-mime-type --default-mime-type=text/html
        --add-header="Cache-Control: max-age=604800, public" --delete-removed sync _site/ s3://www.gwern.net/ &&
  rm -rf ~/wiki/_cache/ ~/wiki/_site/ && rm ./hakyll *.o *.hi ;
  git push; echo -n -e '\a'

Explanations:

- we could run Hakyll with a command like `./hakyll.hs build` but this would run much slower than if we compile an optimized parallelized binary & run it with multiple threads; this comes at the cost of considerable extra complexity in the invocation, though, since we need to compile it with fancy options, run it with other options, and then at the end clean up by deleting the compiled binary & intermediates (GHC cannot take care of them on its own: https://ghc.haskell.org/trac/ghc/ticket/4114 )
- `rebuild` instead of 'build' because IIRC there was some problem where Hakyll didn't like extension-less files so incremental syncs/builds don't work; this tells Hakyll
 to throw everything away without even trying to check
- s3cmd:

    - `--reduced-redundancy` saves a bit of money; no need for high-durability since everything is backed up locally in the git repo, after all
    - s3cmd's MIME type detection has been unreliable in the past due to long-standing bugs in `python-magic` (particularly with CSS), so we need to force a default, especially for the extension-less (HTML) files
- after that, we clean up after ourselves and sync with the Github mirror as well
- the 'echo' calls are there to ring the terminal bell and notify the user that he needs to edit the Modafinil file or that the whole thing is done
-}

import Control.Concurrent (forkIO)
import Control.Exception (onException)
import Control.Monad (when, unless, void)
import Data.Char (toLower)
import Data.IORef (newIORef, IORef)
import Data.List (intercalate, isInfixOf, isSuffixOf, isPrefixOf, nubBy, sort)
import qualified Data.Map.Strict as M (lookup)
import Data.Maybe (isNothing)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)
import Data.FileStore.Utils (runShellCommand)
import Hakyll (compile, composeRoutes, constField,
               symlinkFileCompiler, dateField, defaultContext, defaultHakyllReaderOptions, field, getMetadata, getMetadataField, lookupString,
               defaultHakyllWriterOptions, getRoute, gsubRoute, hakyll, idRoute, itemIdentifier,
               loadAndApplyTemplate, match, modificationTimeField, mapContext,
               pandocCompilerWithTransformM, route, setExtension, pathField, preprocess, boolField, toFilePath,
               templateCompiler, version, Compiler, Context, Item, unsafeCompiler, noResult)
import System.Exit (ExitCode(ExitFailure))
import Text.HTML.TagSoup (renderTagsOptions, parseTags, renderOptions, optMinimize, optRawTag, Tag(TagOpen))
import Text.Read (readMaybe)
import Text.Pandoc.Shared (blocksToInlines)
import Text.Pandoc (nullAttr, runPure, runWithDefaultPartials, compileTemplate,
                    def, pandocExtensions, readerExtensions, readMarkdown, writeHtml5String,
                    Block(..), HTMLMathMethod(MathJax), defaultMathJaxURL, Inline(..),
                    ObfuscationMethod(NoObfuscation), Pandoc(..), WriterOptions(..), nullMeta)
import Text.Pandoc.Walk (walk, walkM)
import Network.HTTP (urlDecode, urlEncode)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text as T (append, isInfixOf, isPrefixOf, isSuffixOf, pack, unpack, length)

-- local custom modules:
import Inflation (nominalToRealInflationAdjuster)
import Interwiki (convertInterwikiLinks, inlinesToText, interwikiTestSuite)
import LinkMetadata (addLocalLinkWalk, readLinkMetadataAndCheck, writeAnnotationFragments, Metadata, createAnnotations, hasAnnotation, simplifiedHTMLString, tagsToLinksDiv, safeHtmlWriterOptions)
import LinkArchive (archivePerRunN, localizeLink, readArchiveMetadata, ArchiveMetadata)
import Typography (linebreakingTransform, typographyTransform, invertImageInline, imageMagickDimensions)
import LinkAuto (linkAuto)
import LinkIcon (rebuildSVGIconCSS)
import LinkLive (linkLiveTest, linkLivePrioritize)
import Utils (printGreen, printRed, replace)

main :: IO ()
main = hakyll $ do

             preprocess $ printGreen ("Testing link icon matches & updating inlined CSS…" :: String)
             preprocess rebuildSVGIconCSS

             preprocess $ printGreen ("Testing live-link-popup rules…" :: String)
             let livelinks = linkLiveTest
             unless (null livelinks) $ preprocess $ printRed ("Live link pop rules have errors in: " ++ show livelinks)
             _ <- preprocess linkLivePrioritize -- generate testcases for new live-link targets

             preprocess $ printGreen ("Testing interwiki rewrite rules…" :: String)
             let interwikiPopupTestCases = interwikiTestSuite
             unless (null interwikiPopupTestCases) $ preprocess $ printRed ("Interwiki rules have errors in: " ++ show interwikiPopupTestCases)

             preprocess $ printGreen ("Local archives parsing…" :: String)
             am <- preprocess readArchiveMetadata

             -- popup metadata:
             preprocess $ printGreen ("Annotations parsing…" :: String)
             meta <- preprocess readLinkMetadataAndCheck
             preprocess $ printGreen ("Writing annotations…" :: String)
             hasArchivedOnce <- preprocess $ newIORef archivePerRunN
             preprocess $ writeAnnotationFragments am meta hasArchivedOnce True
             preprocess $ printGreen ("Begin site compilation…" :: String)
             match "**.page" $ do
                 -- strip extension since users shouldn't care if HTML3-5/XHTML/etc (cool URLs); delete apostrophes/commas & replace spaces with hyphens
                 -- as people keep screwing them up endlessly:
                 route $ gsubRoute "," (const "") `composeRoutes` gsubRoute "'" (const "") `composeRoutes` gsubRoute " " (const "-") `composeRoutes`
                          setExtension ""
                 -- https://groups.google.com/forum/#!topic/pandoc-discuss/HVHY7-IOLSs
                 let readerOptions = defaultHakyllReaderOptions
                 compile $ pandocCompilerWithTransformM readerOptions woptions (unsafeCompiler . pandocTransform meta am hasArchivedOnce)
                     >>= loadAndApplyTemplate "static/templates/default.html" (postCtx meta)
                     >>= imgUrls

             -- handle the simple static non-.page files; we define this after the pages because the pages' compilation has side-effects which may create new static files (archives & downsized images)
             let static = route idRoute >> compile symlinkFileCompiler -- WARNING: custom optimization requiring forked Hakyll installation; see https://github.com/jaspervdj/hakyll/issues/786
             version "static" $ mapM_ (`match` static) [
                                     "docs/**",
                                     "images/**",
                                     "**.hs",
                                     "**.sh",
                                     "**.txt",
                                     "**.html",
                                     "**.page",
                                     "**.css",
                                     "**.R",
                                     "**.conf",
                                     "**.php",
                                     "**.svg",
                                     "**.png",
                                     "**.jpg",
                                     -- skip "static/build/**" because of the temporary files
                                     "static/css/**",
                                     "static/font/**",
                                     "static/img/**",
                                     "static/includes/**",
                                     "static/nginx/**",
                                     "static/redirects/**",
                                     "static/templates/**",
                                     "static/**.conf",
                                     "static/**.css",
                                     "static/**.gif",
                                     "static/**.git",
                                     "static/**.gitignore",
                                     "static/**.hs",
                                     "static/**.html",
                                     "static/**.ico",
                                     "static/**.js",
                                     "static/**.net",
                                     "static/**.png",
                                     "static/**.png-768px",
                                     "static/**.R",
                                     "static/**.sh",
                                     "static/**.svg",
                                     "static/**.ttf",
                                     "static/**.otf",
                                     "static/**.php",
                                     "static/**.py",
                                     "static/**.wasm",
                                     "**.yaml",
                                     "metadata/**",
                                     "static/build/.htaccess",
                                     "atom.xml"] -- copy stub of deprecated RSS feed

             match "static/templates/*.html" $ compile templateCompiler

woptions :: WriterOptions
woptions = defaultHakyllWriterOptions{ writerSectionDivs = True,
                                       writerTableOfContents = True,
                                       writerColumns = 130,
                                       writerTemplate = Just tocTemplate,
                                       writerTOCDepth = 4,
                                       -- we use MathJax directly to bypass Texmath; this enables features like colored equations:
                                       -- https://docs.mathjax.org/en/latest/input/tex/extensions/color.html http://mirrors.ctan.org/macros/latex/required/graphics/color.pdf#page=4 eg. "Roses are $\color{red}{\text{beautiful red}}$, violets are $\color{blue}{\text{lovely blue}}$" or "${\color{red} x} + {\color{blue} y}$"
                                       writerHTMLMathMethod = MathJax defaultMathJaxURL,
                                       writerEmailObfuscation = NoObfuscation }
   where
    -- below copied from https://github.com/jaspervdj/hakyll/blob/e8ed369edaae1808dffcc22d1c8fb1df7880e065/web/site.hs#L73 because god knows I don't know what this type bullshit is either:
    -- "When did it get so hard to compile a string to a Pandoc template?"
    tocTemplate =
        either error id $ either (error . show) id $
        runPure $ runWithDefaultPartials $
        compileTemplate "" "<div id=\"TOC\" class=\"TOC\">$toc$</div>\n<div id=\"markdownBody\" class=\"markdownBody\">$body$</div>"


imgUrls :: Item String -> Compiler (Item String)
imgUrls item = do
    rte <- getRoute $ itemIdentifier item
    case rte of
        Nothing -> return item
        Just _  -> traverse (unsafeCompiler . addImgDimensions) item

postCtx :: Metadata -> Context String
postCtx md =
    fieldsTagPlain md <>
    fieldsTagHTML  md <>
    titlePlainField "titlePlain" <>
    descField "title" <>
    descField "description" <> -- constField "description" "N/A" <>
    -- NOTE: as a hack to implement conditional loading of JS/metadata in /index, in default.html, we switch on an 'index' variable; this variable *must* be left empty (and not set using `constField "index" ""`)! (It is defined in the YAML front-matter of /index.page as `index: true` to set it to a non-null value.) Likewise, "error404" for generating the 404.html page.
    -- similarly, 'author': default.html has a conditional to set 'Gwern Branwen' as the author in the HTML metadata if 'author' is not defined, but if it is, then the HTML metadata switches to the defined author & the non-default author is exposed in the visible page metadata as well for the human readers.
    defaultContext <>
    boolField "backlinksNo" backlinkCheck <>
    dateField "created" "%F" <>
    -- if no manually set last-modified time, fall back to checking file modification time:
    dateField "modified" "%F" <>
    modificationTimeField "modified" "%F" <>
    -- page navigation defaults:
    constField "next" "/index" <>
    constField "previous" "/index" <>
    -- metadata:
    constField "status" "notes" <>
    constField "confidence" "log" <>
    constField "importance" "0" <>
    constField "cssExtension" "drop-caps-de-zs" <>
    imageDimensionWidth "thumbnailHeight" <>
    imageDimensionWidth "thumbnailWidth" <>
    -- for use in templating, `<body class="$safeURL$">`, allowing page-specific CSS:
    escapedTitleField "safeURL" <>
    (mapContext (\p -> (urlEncode $ concatMap (\t -> if t=='/'||t==':' then urlEncode [t] else [t]) $ ("/" ++ replace ".page" ".html" p))) . pathField) "escapedURL" -- for use with backlinks ie 'href="/metadata/annotations/backlinks/$escapedURL$"', so 'Bitcoin-is-Worse-is-Better.page' → '/metadata/annotations/backlinks/%2FBitcoin-is-Worse-is-Better.html', 'notes/Faster.page' → '/metadata/annotations/backlinks/%2Fnotes%2FFaster.html'

fieldsTagHTML :: Metadata -> Context a
fieldsTagHTML m = field "tagsHTML" $ \item -> do
  let path = "/" ++ (replace ".page" "" $ toFilePath $ itemIdentifier item)
  case M.lookup path m of
    Nothing                 -> return "" -- noResult "no description field"
    Just x@(_,_,_,_,tags,_) -> case (runPure $ writeHtml5String safeHtmlWriterOptions (Pandoc nullMeta [tagsToLinksDiv $ map T.pack tags])) of
                                 Left e -> error ("Failed to compile tags to HTML fragment: " ++ show path ++ show x ++ show e)
                                 Right html -> return (T.unpack html)

fieldsTagPlain :: Metadata -> Context a
fieldsTagPlain m = field "tagsPlain" $ \item -> do
  let path = "/" ++ (replace ".page" "" $ toFilePath $ itemIdentifier item)
  case M.lookup path m of
    Nothing               -> return "" -- noResult "no description field"
    Just (_,_,_,_,tags,_) -> return $ intercalate ", " tags

-- should backlinks be in the metadata? We skip backlinks for newsletters & indexes (excluded from the backlink generation process as well) due to lack of any value of looking for backlinks to hose.
-- HACK: uses unsafePerformIO. Not sure how to check up front without IO... Read the backlinks DB and thread it all the way through `postCtx`, and `main`?
backlinkCheck :: Item a -> Bool
backlinkCheck i = let p = toFilePath (itemIdentifier i) in unsafePerformIO (doesFileExist (("metadata/annotations/backlinks/" ++ replace "/" "%2F" (replace ".page" "" ("/"++p))) ++ ".html")) && not ("newsletter/" `isInfixOf` p || "index" `isSuffixOf` p)

imageDimensionWidth :: String -> Context a
imageDimensionWidth d = field d $ \item -> do
                  metadataMaybe <- getMetadataField (itemIdentifier item) "thumbnail"
                  let (h,w) = case metadataMaybe of
                        Nothing -> ("1400","1238") -- /static/img/logo/logo-whitebg-large-border.png dimensions
                        Just thumbnailPath -> unsafePerformIO $ imageMagickDimensions $ tail thumbnailPath
                  if d == "thumbnailWidth" then return w else return h

escapedTitleField :: String -> Context a
escapedTitleField t = (mapContext (map toLower . replace "/" "-" . replace ".page" "") . pathField) t

-- for 'title' metadata, they can have formatting like <em></em> italics; this would break when substituted into <title> or <meta> tags.
-- So we render a simplified ASCII version of every 'title' field, '$titlePlain$', and use that in default.html when we need a non-display
-- title.
titlePlainField :: String -> Context a
titlePlainField d = field d $ \item -> do
                  metadataMaybe <- getMetadataField (itemIdentifier item) "title"
                  case metadataMaybe of
                    Nothing -> noResult "no title field"
                    Just t -> return (simplifiedHTMLString t)

descField :: String -> Context a
descField d = field d $ \item -> do
                  metadata <- getMetadata (itemIdentifier item)
                  let descMaybe = lookupString d metadata
                  case descMaybe of
                    Nothing -> noResult "no description field"
                    Just desc ->
                     let cleanedDesc = runPure $ do
                              pandocDesc <- readMarkdown def{readerExtensions=pandocExtensions} (T.pack desc)
                              let pandocDesc' = linebreakingTransform pandocDesc
                              htmlDesc <- writeHtml5String def pandocDesc' -- NOTE: we can skip 'safeHtmlWriterOptions' use here because descriptions are always very simple & will never have anything complex like tables
                              return $ T.unpack htmlDesc
                      in case cleanedDesc of
                         Left _          -> noResult "no description field"
                         Right finalDesc -> return $ reverse $ drop 4 $ reverse $ drop 3 finalDesc -- strip <p></p>

pandocTransform :: Metadata -> ArchiveMetadata -> IORef Integer -> Pandoc -> IO Pandoc
pandocTransform md adb archived p = -- linkAuto needs to run before convertInterwikiLinks so it can add in all of the WP links and then convertInterwikiLinks will add link-annotated as necessary
                           do let pw = walk (footnoteAnchorChecker . convertInterwikiLinks) $ walk linkAuto $ walk marginNotes p
                              _ <- createAnnotations md pw
                              let pb = walk (hasAnnotation md True) pw
                              pbt <- fmap typographyTransform . walkM (localizeLink adb archived) $ walk (map (nominalToRealInflationAdjuster . addAmazonAffiliate)) pb
                              let pbth = addLocalLinkWalk $ walk headerSelflink pbt
                              pbth' <- walkM invertImageInline pbth
                              pbth'' <- walkM imageSrcset pbth'
                              return pbth''

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
imageSrcset x@(Link (htmlid, classes, kvs) xs (p,t)) = if (".png" `T.isSuffixOf` p || ".jpg" `T.isSuffixOf` p) &&
                                                          ("https://www.gwern.net/" `T.isPrefixOf` p || "/" `T.isPrefixOf` p) then
                                                         do exists <- doesFileExist $ tail $ replace "https://www.gwern.net" "" $ T.unpack  p
                                                            if not exists then printRed ("imageSrcset (Link): " ++ show x ++ " does not exist?") >> return x else
                                                              do (h,w) <- imageMagickDimensions $ T.unpack p
                                                                 return (Link (htmlid, classes,
                                                                               kvs++[("image-height",T.pack h),
                                                                                      ("image-width",T.pack w)])
                                                                         xs (p,t))
                                                       else return x
imageSrcset x = return x

-- For Amazon links, there are two scenarios: there are parameters (denoted by a
-- '?' in the URL), or there are not. In the former, we need to append the tag as
-- another item ('&tag='), while in the latter, we need to set up our own
-- parameter ('?tag='). The transform may be run many times since
-- they are supposed to be pure, so we
-- need to also check a tag hasn't already been appended.
--
-- For non-Amazon links, we just return them unchanged.
addAmazonAffiliate :: Inline -> Inline
addAmazonAffiliate x@(Link attr r (l, t)) = if (("www.amazon.com/" `T.isInfixOf` l) && not ("tag=gwernnet-20" `T.isInfixOf` l)) then
                                        if ("?" `T.isInfixOf` l) then Link attr r (l `T.append` "&tag=gwernnet-20", t) else Link attr r (l `T.append` "?tag=gwernnet-20", t)
                                       else x
addAmazonAffiliate x = x

-- | Make headers into links to themselves, so they can be clicked on or copy-pasted easily.
-- BUG: Pandoc uses the Span trick to remove the Link from the generated ToC, which leaves behind redundant meaningless <span></span> wrappers. <https://github.com/jgm/pandoc/issues/8020>
headerSelflink :: Block -> Block
headerSelflink (Header a (href,b,c) d) = Header a (href,b,c) [Link nullAttr d ("#"`T.append`href,
                                                                               "Link to section: § '" `T.append` inlinesToText d `T.append` "'")]
headerSelflink x = x

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

-- https://edwardtufte.github.io/tufte-css/#sidenotes
-- support Tufte-CSS-style margin notes with a syntax like
-- 'Foo bar.^[!Margin: Short explanation.] Baz quux burble…'
--
-- > marginNotes (Note [Para [Str "!Margin:", Space, Str "Test."]])
-- → Span ("",["marginnote"],[]) [Para [Space, Str "Test."]]
--
-- This sets a 'marginnote' HTML class on the footnote anchor. `sidenotes.js` specially supports these and renders them differently, removing the numerals. We strip the 'Note' because otherwise it will get a number despite being hidden, and that's confusing.
-- Margin notes can be used for general comments on a region of text which aren't intended to refer to a specific word or sentence, and avoiding spurious precision makes it a little easier for the reader.
-- I intend to experiment with them as a way to summarize paragraphs in a short sentence, somewhat like http://www.pgbovine.net/PhD-memoir/pguo-PhD-grind.pdf or how https://ebooks.adelaide.edu.au/c/coleridge/samuel_taylor/rime/#part1 glosses each section of verse, which can be a little tricky to follow.
marginNotes :: Inline -> Inline
marginNotes x@(Note (bs:cs)) =
  case bs of
    Para (Str m:ms) -> if "!Margin:" /= m then x else
                            Span ("", ["marginnote"], []) (blocksToInlines $ Para ms:cs)
    _ -> x
marginNotes x = x

-- Check for footnotes which may be broken and rendering wrong, with the content inside the body rather than as a footnote. (An example was present for an embarrassingly long time in /GPT-3…)
footnoteAnchorChecker :: Inline -> Inline
footnoteAnchorChecker n@(Note [Para [Str s]]) = if " " `T.isInfixOf` s || T.length s > 10 then n else error ("Warning: a short spaceless footnote! May be a broken anchor (ie. swapping the intended '[^abc]:' for '^[abc]:'): " ++ show n)
footnoteAnchorChecker n = n
