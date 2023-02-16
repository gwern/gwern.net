#!/usr/bin/env runghc
{-# LANGUAGE OverloadedStrings #-}

{-
Hakyll file for building Gwern.net
Author: gwern
Date: 2010-10-01
When: Time-stamp: "2023-02-15 16:12:26 gwern"
License: CC-0

Debian dependencies:
$ sudo apt-get install libghc-hakyll-dev libghc-pandoc-dev libghc-filestore-dev libghc-tagsoup-dev libghc-yaml-dev imagemagick s3cmd git libghc-aeson-dev libghc-missingh-dev libghc-digest-dev tidy gridsite-clients

(GHC is needed for Haskell; Hakyll & Pandoc do the heavy lifting of compiling Markdown files to HTML; tag soup & ImageMagick are runtime dependencies used to help optimize images, and s3cmd/git upload to hosting/Github respectively.)
Demo command (for the full script, with all static checks & generation & optimizations, see `sync-gwern.net.sh`):

$ cd ~/wiki/ && ghc -rtsopts -threaded -O2 -fforce-recomp -optl-s --make hakyll.hs &&
  ./hakyll rebuild +RTS -N3 -RTS && echo -n -e '\a'  &&
  s3cmd -v -v --human-readable-sizes --reduced-redundancy --no-mime-magic --guess-mime-type --default-mime-type=text/html
        --add-header="Cache-Control: max-age=604800, public" --delete-removed sync _site/ s3://gwern.net/ &&
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

import Control.Monad (when, unless)
import Data.Char (toLower)
import Data.IORef (newIORef, IORef)
import Data.List (intercalate, isInfixOf, isSuffixOf)
import qualified Data.Map.Strict as M (lookup)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Hakyll (compile, composeRoutes, constField,
               symlinkFileCompiler, dateField, defaultContext, defaultHakyllReaderOptions, field, getMetadata, getMetadataField, lookupString,
               defaultHakyllWriterOptions, getRoute, gsubRoute, hakyll, idRoute, itemIdentifier,
               loadAndApplyTemplate, match, modificationTimeField, mapContext,
               pandocCompilerWithTransformM, route, setExtension, pathField, preprocess, boolField, toFilePath,
               templateCompiler, version, Compiler, Context, Item, unsafeCompiler, noResult, getUnderlying)
import Text.Pandoc.Shared (blocksToInlines)
import Text.Pandoc (nullAttr, runPure, runWithDefaultPartials, compileTemplate,
                    def, pandocExtensions, readerExtensions, readMarkdown, writeHtml5String,
                    Block(..), HTMLMathMethod(MathJax), defaultMathJaxURL, Inline(..),
                    ObfuscationMethod(NoObfuscation), Pandoc(..), WriterOptions(..), nullMeta)
import Text.Pandoc.Walk (walk, walkM)
import Network.HTTP (urlEncode)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text as T (append, isInfixOf, pack, unpack, length)

-- local custom modules:
import Annotation (tooltipToMetadataTest)
import Image (invertImageInline, imageMagickDimensions, addImgDimensions, imageSrcset)
import Inflation (nominalToRealInflationAdjuster)
import Interwiki (convertInterwikiLinks, inlinesToText, interwikiTestSuite)
import LinkArchive (archivePerRunN, localizeLink, readArchiveMetadata, ArchiveMetadata)
import LinkAuto (linkAuto)
import LinkBacklink (getBackLinkCheck, getLinkBibLinkCheck, getSimilarLinkCheck)
import LinkIcon (rebuildSVGIconCSS)
import LinkLive (linkLiveTest, linkLivePrioritize)
import LinkMetadata (addPageLinkWalk, readLinkMetadata, readLinkMetadataAndCheck, writeAnnotationFragments, createAnnotations, hasAnnotation, simplifiedHTMLString)
import LinkMetadataTypes (Metadata)
import QuoteOfTheDay (qotd)
import Tags (tagsToLinksDiv)
import Typography (linebreakingTransform, typographyTransform, titlecaseInline)
import Utils (printGreen, printRed, replace, safeHtmlWriterOptions)

main :: IO ()
main =
 do arg <- lookupEnv "SLOW" -- whether to do the more expensive stuff; Hakyll eats the CLI arguments, so we pass it in as an exported environment variable instead
    let slow = "--slow" == fromMaybe "" arg
    hakyll $ do
               preprocess $ printGreen ("Generating quote-of-the-day…" :: String)
               preprocess qotd

               when slow $ do preprocess $ printGreen ("Testing link icon matches & updating inlined CSS…" :: String)
                              preprocess rebuildSVGIconCSS

                              preprocess $ printGreen ("Testing live-link-popup rules…" :: String)
                              let livelinks = linkLiveTest
                              unless (null livelinks) $ preprocess $ printRed ("Live link pop rules have errors in: " ++ show livelinks)
                              _ <- preprocess linkLivePrioritize -- generate testcases for new live-link targets

                              preprocess $ printGreen ("Testing interwiki rewrite rules…" :: String)
                              let interwikiPopupTestCases = interwikiTestSuite
                              unless (null interwikiPopupTestCases) $ preprocess $ printRed ("Interwiki rules have errors in: " ++ show interwikiPopupTestCases)

                              unless (null tooltipToMetadataTest) $ preprocess $ printRed ("Tooltip-parsing rules have errors in: " ++ show tooltipToMetadataTest)

               preprocess $ printGreen ("Local archives parsing…" :: String)
               am           <- preprocess readArchiveMetadata
               hasArchivedN <- preprocess $ if slow then newIORef archivePerRunN else newIORef 0

               preprocess $ printGreen ("Popup annotations parsing…" :: String)
               meta <- preprocess $ if slow then readLinkMetadataAndCheck else readLinkMetadata
               preprocess $ if slow then do printGreen ("Writing all annotations…" :: String)
                                            writeAnnotationFragments am meta hasArchivedN False
                                       else do printGreen ("Writing only missing annotations…" :: String)
                                               writeAnnotationFragments am meta hasArchivedN True

               preprocess $ printGreen ("Begin site compilation…" :: String)
               match "**.page" $ do
                   -- strip extension since users shouldn't care if HTML3-5/XHTML/etc (cool URLs); delete apostrophes/commas & replace spaces with hyphens
                   -- as people keep screwing them up endlessly:
                   route $ gsubRoute "," (const "") `composeRoutes` gsubRoute "'" (const "") `composeRoutes` gsubRoute " " (const "-") `composeRoutes`
                            setExtension ""
                   -- https://groups.google.com/forum/#!topic/pandoc-discuss/HVHY7-IOLSs
                   let readerOptions = defaultHakyllReaderOptions
                   compile $ do
                              ident <- getUnderlying
                              indexpM <- getMetadataField ident "index"
                              let indexp = fromMaybe "" indexpM
                              pandocCompilerWithTransformM readerOptions woptions (unsafeCompiler . pandocTransform meta am hasArchivedN indexp)
                                >>= loadAndApplyTemplate "static/template/default.html" (postCtx meta)
                                >>= imgUrls

               -- handle the simple static non-.page files; we define this after the pages because the pages' compilation has side-effects which may create new static files (archives & downsized images)
               let static = route idRoute >> compile symlinkFileCompiler -- WARNING: custom optimization requiring forked Hakyll installation; see https://github.com/jaspervdj/hakyll/issues/786
               version "static" $ mapM_ (`match` static) [
                                       "doc/**",
                                       "image/**",
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
                                       "static/include/**",
                                       "static/nginx/**",
                                       "static/redirect/**",
                                       "static/template/**",
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
                                       "static/**768px.png",
                                       "static/**768px.jpg",
                                       "static/**530px.jpg",
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

               match "static/template/*.html" $ compile templateCompiler

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
        compileTemplate "" "<div id=\"TOC\" class=\"TOC\">$toc$</div> <div id=\"markdownBody\" class=\"markdownBody\">$body$" -- we do the main $body$ substitution inside default.html so we can inject stuff inside the #markdownBody wrapper; the div is closed there

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
    boolField "backlinksYes" (check notNewsletterOrIndex getBackLinkCheck)    <>
    boolField "similarsYes"  (check notNewsletterOrIndex getSimilarLinkCheck) <>
    boolField "linkbibYes"   (check (const True)         getLinkBibLinkCheck) <>
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
    thumbnailSmallTransform "thumbnailSmall" <>
    imageDimensionWidth "thumbnailHeight" <>
    imageDimensionWidth "thumbnailWidth" <>
    -- for use in templating, `<body class="$safeURL$">`, allowing page-specific CSS:
    escapedTitleField "safeURL" <>
    (mapContext (\p -> urlEncode $ concatMap (\t -> if t=='/'||t==':' then urlEncode [t] else [t]) ("/" ++ replace ".page" ".html" p)) . pathField) "escapedURL" -- for use with backlinks ie 'href="/metadata/annotation/backlink/$escapedURL$"', so 'Bitcoin-is-Worse-is-Better.page' → '/metadata/annotation/backlink/%2FBitcoin-is-Worse-is-Better.html', 'notes/Faster.page' → '/metadata/annotation/backlink/%2Fnotes%2FFaster.html'

fieldsTagHTML :: Metadata -> Context String
fieldsTagHTML m = field "tagsHTML" $ \item -> do
  let path = "/" ++ replace ".page" "" (toFilePath $ itemIdentifier item)
  case M.lookup path m of
    Nothing                 -> return "" -- noResult "no description field"
    Just x@(_,_,_,_,tags,_) -> case runPure $ writeHtml5String safeHtmlWriterOptions (Pandoc nullMeta [tagsToLinksDiv $ map T.pack tags]) of
                                 Left e -> error ("Failed to compile tags to HTML fragment: " ++ show path ++ show x ++ show e)
                                 Right html -> return (T.unpack html)

fieldsTagPlain :: Metadata -> Context String
fieldsTagPlain m = field "tagsPlain" $ \item -> do
  let path = "/" ++ replace ".page" "" (toFilePath $ itemIdentifier item)
  case M.lookup path m of
    Nothing               -> return "" -- noResult "no description field"
    Just (_,_,_,_,tags,_) -> return $ intercalate ", " tags

thumbnailSmallTransform :: String -> Context String
thumbnailSmallTransform d = field d $ \item -> do
                  metadata <- getMetadata (itemIdentifier item)
                  let thumbMaybe = lookupString "thumbnail" metadata
                  case thumbMaybe of
                    Nothing -> noResult "no thumbnail field"
                    Just img -> return $ thumbnailSmall img

thumbnailSmall :: String -> String
thumbnailSmall = (++"-530px.jpg")

-- should backlinks be in the metadata? We skip backlinks for newsletters & indexes (excluded from the backlink generation process as well) due to lack of any value of looking for backlinks to hose.
-- HACK: uses unsafePerformIO. Not sure how to check up front without IO... Read the backlinks DB and thread it all the way through `postCtx`, and `main`?
check :: (String -> Bool) -> (String -> IO (String, String)) -> Item a -> Bool
check filterfunc checkfunc i = unsafePerformIO $ do let p = pageIdentifierToPath i
                                                    (_,path) <- checkfunc p
                                                    return $ path /= "" && filterfunc p
notNewsletterOrIndex :: String -> Bool
notNewsletterOrIndex p = not ("newsletter/" `isInfixOf` p || "index" `isSuffixOf` p)

pageIdentifierToPath :: Item a -> String
pageIdentifierToPath i = "/" ++ replace ".page" "" (toFilePath $ itemIdentifier i)

imageDimensionWidth :: String -> Context String
imageDimensionWidth d = field d $ \item -> do
                  metadataMaybe <- getMetadataField (itemIdentifier item) "thumbnail"
                  let (h,w) = case metadataMaybe of
                        Nothing -> ("530","441") -- /static/img/logo/logo-whitebg-large-border.png-530px.jpg dimensions
                        Just thumbnailPath -> unsafePerformIO $ imageMagickDimensions $ tail $ thumbnailSmall thumbnailPath
                  if d == "thumbnailWidth" then return w else return h

escapedTitleField :: String -> Context String
escapedTitleField = mapContext (map toLower . replace "/" "-" . replace ".page" "") . pathField

-- for 'title' metadata, they can have formatting like <em></em> italics; this would break when substituted into <title> or <meta> tags.
-- So we render a simplified ASCII version of every 'title' field, '$titlePlain$', and use that in default.html when we need a non-display
-- title.
titlePlainField :: String -> Context String
titlePlainField d = field d $ \item -> do
                  metadataMaybe <- getMetadataField (itemIdentifier item) "title"
                  case metadataMaybe of
                    Nothing -> noResult "no title field"
                    Just t -> return (simplifiedHTMLString t)

descField :: String -> Context String
descField d = field d $ \item -> do
                  metadata <- getMetadata (itemIdentifier item)
                  let descMaybe = lookupString d metadata
                  case descMaybe of
                    Nothing -> noResult "no description field"
                    Just desc ->
                     let cleanedDesc = runPure $ do
                              pandocDesc <- readMarkdown def{readerExtensions=pandocExtensions} (T.pack desc)
                              let pandocDesc' = walk convertInterwikiLinks $ linebreakingTransform pandocDesc
                              htmlDesc <- writeHtml5String def pandocDesc' -- NOTE: we can skip 'safeHtmlWriterOptions' use here because descriptions are always very simple & will never have anything complex like tables
                              return $ T.unpack htmlDesc
                      in case cleanedDesc of
                         Left _          -> noResult "no description field"
                         Right finalDesc -> return $ reverse $ drop 4 $ reverse $ drop 3 finalDesc -- strip <p></p>

pandocTransform :: Metadata -> ArchiveMetadata -> IORef Integer -> String -> Pandoc -> IO Pandoc
pandocTransform md adb archived indexp' p = -- linkAuto needs to run before `convertInterwikiLinks` so it can add in all of the WP links and then convertInterwikiLinks will add link-annotated as necessary; it also must run before `typographyTransform`, because that will decorate all the 'et al's into <span>s for styling, breaking the LinkAuto regexp matches for paper citations like 'Brock et al 2018'
                           -- tag-directories/link-bibliographies special-case: we don't need to run all the heavyweight passes, and LinkAuto has a regrettable tendency to screw up section headers, so we check to see if we are processing a document with 'index: true' set in the YAML metadata, and if we are, we slip several of the rewrite transformations:
  do let indexp = indexp' == "true"
     let pw
           = if indexp then walk convertInterwikiLinks p else
               walk (footnoteAnchorChecker . convertInterwikiLinks) $
                 walk linkAuto $ walk marginNotes p
     unless indexp $ createAnnotations md pw
     let pb = walk (hasAnnotation md) $ addPageLinkWalk pw  -- we walk local link twice: we need to run it before 'hasAnnotation' so essays don't get overridden, and then we need to add it later after all of the archives have been rewritten, as they will then be local links
     pbt <- fmap typographyTransform . walkM (localizeLink adb archived)
              $
              if indexp then pb else
                walk (map (nominalToRealInflationAdjuster . addAmazonAffiliate)) pb
     let pbth = addPageLinkWalk $ walk headerSelflink pbt
     if indexp then return pbth else
       do pbth' <- walkM invertImageInline pbth
          walkM imageSrcset pbth'

-- For Amazon links, there are two scenarios: there are parameters (denoted by a
-- '?' in the URL), or there are not. In the former, we need to append the tag as
-- another item ('&tag='), while in the latter, we need to set up our own
-- parameter ('?tag='). The transform may be run many times since
-- they are supposed to be pure, so we
-- need to also check a tag hasn't already been appended.
--
-- For non-Amazon links, we just return them unchanged.
addAmazonAffiliate :: Inline -> Inline
addAmazonAffiliate x@(Link attr r (l, t)) = if ("www.amazon.com/" `T.isInfixOf` l) && not ("tag=gwernnet-20" `T.isInfixOf` l) then
                                        if "?" `T.isInfixOf` l then Link attr r (l `T.append` "&tag=gwernnet-20", t) else Link attr r (l `T.append` "?tag=gwernnet-20", t)
                                       else x
addAmazonAffiliate x = x

-- | Make headers into links to themselves, so they can be clicked on or copy-pasted easily. Put the displayed text into title-case if not already.
headerSelflink :: Block -> Block
headerSelflink (Header a (href,b,c) d) = Header a (href,b,c) [Link nullAttr (walk titlecaseInline d) ("#"`T.append`href,
                                                                               "Link to section: § '" `T.append` inlinesToText d `T.append` "'")]
headerSelflink x = x

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

-- Check for footnotes which may be broken and rendering wrong, with the content inside the body rather than as a footnote. (An example was present for an embarrassingly long time in /gpt-3…)
footnoteAnchorChecker :: Inline -> Inline
footnoteAnchorChecker n@(Note [Para [Str s]]) = if " " `T.isInfixOf` s || T.length s > 10 then n else error ("Warning: a short spaceless footnote! May be a broken anchor (ie. swapping the intended '[^abc]:' for '^[abc]:'): " ++ show n)
footnoteAnchorChecker n = n
