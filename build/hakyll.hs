#!/usr/bin/env runghc
{-# LANGUAGE OverloadedStrings #-}

{-
Hakyll file for building Gwern.net
Author: gwern
Date: 2010-10-01
When: Time-stamp: "2025-06-03 10:10:46 gwern"
License: CC-0

Debian dependencies:
$ sudo apt-get install libghc-hakyll-dev libghc-pandoc-dev libghc-filestore-dev libghc-tagsoup-dev imagemagick rsync git libghc-aeson-dev libghc-missingh-dev libghc-digest-dev tidy gridsite-clients

(GHC is needed for Haskell; Hakyll & Pandoc do the heavy lifting of compiling Markdown files to HTML; tag soup & ImageMagick are runtime dependencies used to help optimize images, and rsync for the server/git upload to hosting/Github respectively.)
Demo command (for the full script, with all static checks & generation & optimizations, see `sync.sh`):
-}

import Control.Monad (when, unless, (<=<))
import Data.Char (toLower)
import Data.List (intercalate, isInfixOf, isSuffixOf, group, sort)
import qualified Data.Map.Strict as M (lookup) -- keys
import Data.Maybe (fromMaybe)
import System.Environment (getArgs, withArgs, lookupEnv)
import Hakyll (compile, composeRoutes, constField, fromGlob,
               symlinkFileCompiler, copyFileCompiler, dateField, defaultContext, defaultHakyllReaderOptions, field, getMetadata, getMetadataField, lookupString,
               defaultHakyllWriterOptions, getRoute, gsubRoute, hakyll, idRoute, itemIdentifier,
               loadAndApplyTemplate, match, modificationTimeField, mapContext,
               pandocCompilerWithTransformM, route, setExtension, pathField, preprocess, boolField, toFilePath,
               templateCompiler, version, Compiler, Context, Item, unsafeCompiler, noResult, getUnderlying, escapeHtml, (.&&.), complement)
import Text.Pandoc (nullAttr, runPure, runWithDefaultPartials, compileTemplate,
                    def, pandocExtensions, readerExtensions, readMarkdown, writeHtml5String,
                    Block(..), HTMLMathMethod(MathJax), defaultMathJaxURL, Inline(..),
                    ObfuscationMethod(NoObfuscation), Pandoc(..), WriterOptions(..), nullMeta) -- unMeta
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (walk, walkM, query)
import Network.HTTP (urlEncode)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text as T (append, filter, isInfixOf, pack, unpack, length, strip)

-- local custom modules:
import Image (imageMagickDimensions, addImgDimensions, imageLinkHeightWidthSet)
import Inflation (nominalToRealInflationAdjuster)
import Interwiki (convertInterwikiLinks)
import LinkArchive (localizeLink, readArchiveMetadataAndCheck, ArchiveMetadata)
import LinkAuto (linkAuto)
import LinkBacklink (getBackLinkCheck, getLinkBibLinkCheck, getSimilarLinkCheck)
import LinkMetadata (addPageLinkWalk, readLinkMetadataSlow, writeAnnotationFragments, createAnnotations, hasAnnotation, addCanPrefetch, annotationSizeDB, addSizeToLinks)
import LinkMetadataTypes (Metadata, SizeDB)
import Tags (tagsToLinksDiv)
import Typography (linebreakingTransform, typographyTransformTemporary, titlecaseInline, completionProgressHTML)
import Utils (printGreen, replace, deleteMany, replaceChecked, safeHtmlWriterOptions, simplifiedHTMLString, inlinesToText, flattenLinksInInlines, delete, toHTML, getMostRecentlyModifiedDir)
import Test (testAll)
import qualified Config.Misc as C (cd, currentYear, todayDayStringUnsafe, isOlderThan, isNewWithinNDays)
import Metadata.Date (dateRangeDuration)
import LinkID (writeOutID2URLdb)
import Blog (writeOutBlogEntries)

main :: IO ()
main =
 do arg <- System.Environment.lookupEnv "SLOW" -- whether to do the more expensive stuff; Hakyll eats the CLI arguments, so we pass it in as an exported environment variable instead
    let slow = "true" == fromMaybe "" arg
    args <- getArgs
    let args' = filter (/="build") args
    let annotationBuildAllForce = filter (=="--annotation-rebuild") args'
    let annotationOneShot       = filter (=="--annotation-missing-one-shot") args'

    C.cd

    printGreen ("Local archives parsing…" :: String)
    am           <- readArchiveMetadataAndCheck

    printGreen ("Popup annotations parsing…" :: String)
    meta <- readLinkMetadataSlow

    sizes <- annotationSizeDB meta am :: IO SizeDB

    printGreen ("Writing blog entries…" :: String)
    writeOutBlogEntries meta

    -- NOTE: reset the `getArgs` to pass through just the first argument (ie. "build", converting it back to `hakyll build`), as `hakyll` internally calls `getArgs` and will fatally error out if we don't delete our own arguments:
    withArgs [head args] $ hakyll $ do

       if not (null annotationBuildAllForce) then
         preprocess $ do printGreen ("Rewriting all annotations…" :: String)
                         writeAnnotationFragments am meta sizes False
       else do
               preprocess $ do printGreen ("Writing missing annotations…" :: String)
                               writeAnnotationFragments am meta sizes True
               if not (null annotationOneShot) then preprocess $ printGreen "Finished writing missing annotations, and one-shot mode specified, so exiting now." else do

                 when slow $ preprocess testAll

                 -- for '/ref/' cache updating & expiring:
                 preprocess $ writeOutID2URLdb meta
                 timestamp <- preprocess $ getMostRecentlyModifiedDir "metadata/annotation/id/"

                 preprocess $ printGreen ("Begin site compilation…" :: String)
                 let targets = if null args' then fromGlob "**.md" .&&. complement "doc/www/**.md" -- exclude any temporary Markdown files in /doc/www/misc/ or mirrored somehow, but compile ones anywhere else
                                else fromGlob $ head args'
                 unless (null args') $ preprocess (printGreen "Essay targets specified, so compiling just: " >> print targets)
                 match targets $ do
                     -- strip extension since users shouldn't care if HTML3-5/XHTML/etc (cool URLs); delete apostrophes/commas & replace spaces with hyphens
                     -- as people keep screwing them up endlessly: (and in nginx, we auto-replace all EN DASH & EM DASH in URLs with hyphens)
                     route $ gsubRoute "," (const "") `composeRoutes` gsubRoute "'" (const "") `composeRoutes` gsubRoute " " (const "-") `composeRoutes`
                              setExtension ""
                     -- <https://groups.google.com/forum/#!topic/pandoc-discuss/HVHY7-IOLSs>
                     let readerOptions = defaultHakyllReaderOptions
                     compile $ do
                                ident <- getUnderlying
                                indexpM <- getMetadataField ident "index"
                                let indexp = fromMaybe "" indexpM
                                pandocCompilerWithTransformM readerOptions woptions (unsafeCompiler . pandocTransform meta am sizes indexp)
                                  >>= loadAndApplyTemplate "static/template/default.html" (postCtx meta timestamp)
                                  >>= imgUrls

                 let static        = route idRoute >> compile copyFileCompiler
                 when (null args') $ version "static" $ mapM_ (`match` static) ["metadata/**"] -- we want to overwrite annotations in-place with various post-processing things

                 -- handle the simple static non-.md files; we define this after the pages because the pages' compilation has side-effects which may create new static files (archives & downsized images)
                 let staticSymlink = route idRoute >> compile symlinkFileCompiler -- WARNING: custom optimization requiring forked Hakyll installation; see https://github.com/jaspervdj/hakyll/issues/786
                 when (null args') $ version "static" $ mapM_ (`match` staticSymlink) [
                                         "doc/**",
                                         "**.hs",
                                         "**.sh",
                                         "**.txt",
                                         "**.html",
                                         "**.md",
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
                                         "static/**.R",
                                         "static/**.sh",
                                         "static/**.svg",
                                         "static/**.ttf",
                                         "static/**.otf",
                                         "static/**.php",
                                         "static/**.py",
                                         "static/**.wasm",
                                         "static/**.el",
                                         "static/LICENSE",
                                         "static/build/.htaccess",
                                         "static/build/upload",
                                         "static/build/newsletter-lint",
                                         "static/build/gwa",
                                         "static/build/crossref",
                                         "static/build/compressPdf",
                                         "static/build/compressJPG2",
                                         "test-include",
                                         "atom.xml"] -- copy stub of deprecated RSS feed

                 match "static/template/*.html" $ compile templateCompiler

woptions :: Text.Pandoc.WriterOptions
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
        compileTemplate "" $ T.pack $ "<div id=\"TOC\" class=\"TOC\">$toc$</div> <div id=\"markdownBody\" class=\"markdownBody\">" ++
                              "$body$" -- we do the main $body$ substitution inside default.html so we can inject stuff inside the #markdownBody wrapper; the div is closed there

imgUrls :: Item String -> Compiler (Item String)
imgUrls item = do
    rte <- getRoute $ itemIdentifier item
    case rte of
        Nothing -> return item
        Just _  -> traverse (unsafeCompiler . addImgDimensions) item

postCtx :: Metadata -> String -> Context String
postCtx md rts =
    fieldsTagPlain md <>
    fieldsTagHTML  md <>
    titlePlainField "title-plain" <>
    descField True "title" "title-escaped" <>
    descField False "title" "title" <>
    descField True "description" "description-escaped" <>
    descField False "description" "description" <>
    constField "refMapTimestamp" rts <>
    -- NOTE: as a hack to implement conditional loading of JS/metadata in /index, in default.html, we switch on an 'index' variable; this variable *must* be left empty (and not set using `constField "index" ""`)! (It is defined in the YAML front-matter of /index.md as `index: True` to set it to a non-null value.) Likewise, "error404" for generating the 404.html page.
    -- similarly, 'author': default.html has a conditional to set 'Gwern' as the author in the HTML metadata if 'author' is not defined, but if it is, then the HTML metadata switches to the defined author & the non-default author is exposed in the visible page metadata as well for the human readers.
    defaultContext <>
    boolField "backlinks-yes" (check notNewsletterOrIndex getBackLinkCheck)    <>
    boolField "similars-yes"  (check notNewsletterOrIndex getSimilarLinkCheck) <>
    boolField "linkbib-yes"   (check (const True)         getLinkBibLinkCheck) <>
    dateRangeHTMLField "date-range-HTML" <>
    isNewField               <> -- CSS 'body.page-created-recently'
    dateField "created" "%F" <>
    -- constField "created" "N/A"  <> -- NOTE: we make 'created' a mandatory field by not setting a default, so template compilation will crash
    -- if no manually set last-modified time, fall back to checking file modification time:
    dateField "modified" "%F" <>
    modificationTimeField "modified" "%F" <>
    -- metadata:
    progressField "status" "status-plus-progress" <>
    constField "status" "notes" <>
    progressField "confidence" "confidence-plus-progress" <>
    constField "confidence" "log" <>
    constField "importance" "0" <>
    constField "css-extension" "dropcaps-de-zs" <>
    constField "thumbnail-css" "" <> -- constField "thumbnail-css" "outline-not" <> -- TODO: all uses of `thumbnail-css` should be migrated to GTX
    imageDimensionWidth "thumbnail-height" <>
    imageDimensionWidth "thumbnail-width" <>
    -- for use in templating, `<body class="page-$safe-url$">`, allowing page-specific CSS like `.page-sidenote` or `.page-slowing-moores-law`:
    escapedTitleField "safe-url" <>
    (mapContext (\p -> urlEncode $ concatMap (\t -> if t=='/'||t==':' then urlEncode [t] else [t]) ("/" ++ replaceChecked ".md" ".html" p)) . pathField) "escaped-url" -- for use with backlinks ie 'href="/metadata/annotation/backlink/$escaped-url$"', so 'bitcoin-is-worse-is-better.md' → '/metadata/annotation/backlink/%2Fbitcoin-is-worse-is-better.html', 'note/faster.md' → '/metadata/annotation/backlink/%2Fnote%2Ffaster.html'

lookupTags :: Metadata -> Item a -> Compiler (Maybe [String])
lookupTags m item = do
  let path = "/" ++ delete ".md" (toFilePath $ itemIdentifier item)
  case M.lookup path m of
    Nothing                 -> return Nothing
    Just (_,_,_,_,_,tags,_) -> return $ Just tags

fieldsTagHTML :: Metadata -> Context String
fieldsTagHTML m = field "tagsHTML" $ \item -> do
  maybeTags <- lookupTags m item
  case maybeTags of
    Nothing   -> return "" -- noResult "no tag field"
    Just []   -> return ""
    Just [""] -> return ""
    Just tags -> case runPure $ writeHtml5String safeHtmlWriterOptions (Pandoc nullMeta [tagsToLinksDiv $ map T.pack tags]) of
                   Left e -> error ("Failed to compile tags to HTML fragment: " ++ show item ++ show tags ++ show e)
                   Right html -> return (T.unpack html)

fieldsTagPlain :: Metadata -> Context String
fieldsTagPlain m = field "tags-plain" $ \item -> do
    maybeTags <- lookupTags m item
    case maybeTags of
      Nothing -> return "" -- noResult "no tag field"
      Just tags -> return $ intercalate ", " tags

-- should backlinks be in the metadata? We skip backlinks for newsletters & indexes (excluded from the backlink generation process as well) due to lack of any value of looking for backlinks to hose.
-- HACK: uses unsafePerformIO. Not sure how to check up front without IO... Read the backlinks DB and thread it all the way through `postCtx`, and `main`?
check :: (String -> Bool) -> (String -> IO (String, String)) -> Item a -> Bool
check filterfunc checkfunc i = unsafePerformIO $ do let p = pageIdentifierToPath i
                                                    (_,path) <- checkfunc p
                                                    return $ path /= "" && filterfunc p
notNewsletterOrIndex :: String -> Bool
notNewsletterOrIndex p = not ("newsletter/" `isInfixOf` p || "index" `isSuffixOf` p)

pageIdentifierToPath :: Item a -> String
pageIdentifierToPath i = "/" ++ (delete "." (delete ".md" (toFilePath (itemIdentifier i))))

imageDimensionWidth :: String -> Context String
imageDimensionWidth d = field d $ \item ->
  do
   metadataMaybe <- getMetadataField (itemIdentifier item) "thumbnail"
   let (h,w) = case metadataMaybe of
         Nothing -> ("530","441")
         Just thumbnailPath -> if null thumbnailPath || head thumbnailPath /= '/'
           then error ("hakyll.imageDimensionWidth: thumbnailPath is invalid. Was: " ++ show thumbnailPath)
           else let x@(result,_) = unsafePerformIO $ imageMagickDimensions $ tail thumbnailPath in
                  if result/="" then x
                  else error ("hakyll.imageDimensionWidth: Image.imageMagickDimensions failed to read dimensions of an image‽ " ++ show (tail thumbnailPath) ++ " : " ++ show x)
   if d == "thumbnail-width" then return w else return h

escapedTitleField :: String -> Context String
escapedTitleField = mapContext (map toLower . replace "." "" . replace "/" "-" . delete ".md") . pathField

-- for 'title' metadata, they can have formatting like <em></em> italics; this would break when substituted into <title> or <meta> tags.
-- So we render a simplified ASCII version of every 'title' field, '$title-plain$', and use that in default.html when we need a non-display
-- title.
titlePlainField :: String -> Context String
titlePlainField d = field d $ \item -> do
                  metadataMaybe <- getMetadataField (itemIdentifier item) "title"
                  case metadataMaybe of
                    Nothing -> noResult "no title field"
                    Just t -> return (simplifiedHTMLString t)

progressField :: String -> String -> Context String
progressField d d' = field d' $ \item -> do
 metadataMaybe <- getMetadataField (itemIdentifier item) d
 case metadataMaybe of
   Nothing -> noResult ""
   Just progress -> return $ completionProgressHTML progress

-- for 'page-created-recently' CSS body switch (eg. disable the recently-modified black-star link highlighting, which is a bad idea if many links on a page are 'new')
-- $page-created-recently$ → " page-created recently" when the page is ≤ 90 d old, else ""; we always insert this into `default.html`, we just return either a space-prefixed string (so it can be concatenated) or an empty string, and we skip trying to do a conditional at all.
isNewField :: Context String
isNewField = field "page-created-recently" $ \item -> do
    mCreated <- getMetadataField (itemIdentifier item) "created"
    case mCreated of
      Nothing -> pure ""
      Just created ->  let today   = C.todayDayStringUnsafe
                           recent  = if created == "N/A" || created == "\"N/A\"" || created == "'N/A'" then False else
                             maybe False (\c -> not (C.isOlderThan C.isNewWithinNDays c today)) mCreated
                       in pure $ if recent then " page-created-recently" else ""

dateRangeHTMLField :: String -> Context String
dateRangeHTMLField d = field d $ \item -> do
 metadataMaybe1 <- getMetadataField (itemIdentifier item) "created"
 metadataMaybe2 <- getMetadataField (itemIdentifier item) "modified"
 case (metadataMaybe1, metadataMaybe2) of
   (Just created, Just modified) -> let dateString = Str $ T.pack $ if created == modified then created else created ++ "–" ++ modified
                                        range = dateRangeDuration C.currentYear dateString
                                    in return (toHTML range)
   (_,_) -> noResult "missing created and/or modified field, so could not adjust the date range subscript."

descField :: Bool -> String -> String -> Context String
descField escape d d' = field d' $ \item -> do
                  metadata <- getMetadata (itemIdentifier item)
                  let descMaybe = lookupString d metadata
                  case descMaybe of
                    Nothing -> noResult "no description field"
                    Just desc ->
                     let cleanedDesc = runPure $ do
                              pandocDesc <- readMarkdown def{readerExtensions=pandocExtensions} (T.pack desc)
                              let pandocDesc' = convertInterwikiLinks $ linebreakingTransform pandocDesc
                              htmlDesc <- writeHtml5String safeHtmlWriterOptions pandocDesc' -- NOTE: we need 'safeHtmlWriterOptions' here because while descriptions are always very simple & will never have anything complex like tables, they *usually* are long enough to trigger line-wrapping, which causes problems for anyone parsing <meta> tags
                              return $ (\t -> if escape then escapeHtml t else t) $ T.unpack htmlDesc
                      in case cleanedDesc of
                         Left _          -> noResult "no description field"
                         Right finalDesc -> return $ deleteMany ["<p>", "</p>", "&lt;p&gt;", "&lt;/p&gt;"] finalDesc -- strip <p></p> wrappers (both forms)

pandocTransform :: Metadata -> ArchiveMetadata -> SizeDB -> String -> Pandoc -> IO Pandoc
pandocTransform md adb sizes indexp' p = -- linkAuto needs to run before `convertInterwikiLinks` so it can add in all of the WP links and then convertInterwikiLinks will add link-annotated as necessary; it also must run before `typographyTransformTemporary`, because that will decorate all the 'et al's into <span>s for styling, breaking the LinkAuto regexp matches for paper citations like 'Brock et al 2018'
                           -- tag-directories/link-bibliographies special-case: we don't need to run all the heavyweight passes, and LinkAuto has a regrettable tendency to screw up section headers, so we check to see if we are processing a document with 'index: True' set in the YAML metadata, and if we are, we slip several of the rewrite transformations:
  do
     let duplicateHeaders = duplicateTopHeaders p in
       unless (null duplicateHeaders) $
       error "Warning: Duplicate top-level headers found: " >> print duplicateHeaders
     let indexp = indexp' == "True"
     let pw
           = if indexp then convertInterwikiLinks p else
               walk footnoteAnchorChecker $ convertInterwikiLinks $
                 walk linkAuto p
     unless indexp $ createAnnotations md pw
     let pb = addPageLinkWalk pw  -- we walk local link twice: we need to run it before 'hasAnnotation' so essays don't get overridden, and then we need to add it later after all of the archives have been rewritten, as they will then be local links
     pbt <- fmap typographyTransformTemporary . walkM (localizeLink adb) $ walk (hasAnnotation md)
              $ walk (addSizeToLinks sizes)
              $ if indexp then pb else
                walk (map nominalToRealInflationAdjuster) pb
     let pbth = wrapInParagraphs $ addPageLinkWalk $ walk headerSelflinkAndSanitize pbt
     walkM (addCanPrefetch <=< imageLinkHeightWidthSet) pbth

-- check that a Gwern.net Pandoc Markdown file has the mandatory metadata fields (title, created, status, confidence), and does not have any unknown fields:
-- checkEssayPandocMetadata :: Pandoc -> IO ()
-- checkEssayPandocMetadata (Pandoc meta _) = let fields = M.keys (unMeta meta)
--                                                mandatoryFields = ["title", "created", "status", "confidence", "importance"]
--                                                permittedFields = ["modified", "thumbnail", "thumbnail-text", "thumbnail-css", "description", "css-extension" ] ++ mandatoryFields
--                                            in
--                                              if not (all (`elem` fields) mandatoryFields) then error $ "hakyll.checkEssayPandocMetadata: mandatory fields were not present in essay Pandoc YAML metadata field? Meta was: " ++ show meta
--                                              else if not (all (`elem` fields) permittedFields) then error $ "hakyll.checkEssayPandocMetadata: unknown fields were present in essay Pandoc YAML metadata field? Meta was: " ++ show meta
--                                              else return ()

-- | Make headers into links to themselves, so they can be clicked on or copy-pasted easily. Put the displayed text into title-case if not already.
--
-- While processing Headers, ensure that they have valid CSS IDs. (Pandoc will happily generate invalid HTML IDs, which contain CSS-forbidden characters like periods; this can cause fatal errors in JS/CSS without dangerous workarounds. So the author needs to manually add a period-less ID. This is an outstanding issue: <https://github.com/jgm/pandoc/issues/6553>.)
-- NOTE: We could instead require the author to manually assign an ID like `# Foo.bar {#foobar}`, which would be reliable & compatible with other Markdown systems, but this would not solve the problem on *generated* pages, like the tag-directories which put paper titles in headers & will routinely incur this problem. So we have to automate it as a Pandoc rewrite.
headerSelflinkAndSanitize :: Block -> Block
headerSelflinkAndSanitize x@(Header _ _ [Link{}]) = x
headerSelflinkAndSanitize x@(Header _ _ []) = error $ "hakyll.hs: headerSelflinkAndSanitize: Invalid header with no visible text‽ This should be impossible: " ++ show x
headerSelflinkAndSanitize x@(Header _ ("",_,_) _) = error $ "hakyll.hs: headerSelflinkAndSanitize: Invalid header with no specified ID‽ This should be impossible: " ++ show x
headerSelflinkAndSanitize x@(Header a (href,b,c) d) =
  let href' = T.filter (`notElem` ['.', '#', ':']) href in
    unsafePerformIO $ do
      when (href' /= href) $ error $ "hakyll.hs: headerSelflinkAndSanitize: Invalid ID for header after filtering! The header text must be changed or a valid ID manually set: " ++ show x
      if href' == "" then error $ "hakyll.hs: headerSelflinkAndSanitize: Invalid ID for header after filtering! The header text must be changed or a valid ID manually set: " ++ show x else
        return $ Header a (href',b,c) [Link nullAttr (walk titlecaseInline $ flattenLinksInInlines d)
                                       ("#"`T.append`href', "Link to section: § '" `T.append` inlinesToText d `T.append` "'")]
headerSelflinkAndSanitize x = x

-- Check for footnotes which may be broken and rendering wrong, with the content inside the body rather than as a footnote. (An example was present for an embarrassingly long time in /gpt-3…)
footnoteAnchorChecker :: Inline -> Inline
footnoteAnchorChecker n@(Note [Para [Str s]]) = if " " `T.isInfixOf` s || T.length s > 10 then n else error ("Warning: a short spaceless footnote! May be a broken anchor (ie. swapping the intended '[^abc]:' for '^[abc]:'): " ++ show n)
footnoteAnchorChecker n = n

-- HACK: especially in list items, we wind up with odd situations like '<li>text</li>' instead of '<li><p>text</p></li>'. This *seems* to be due to the HTML/Markdown AST roundtripping resulting in 'loose' elements which Pandoc defaults to 'Plain'. I do not use 'Plain' anywhere wittingly, so it should be safe to blindly rewrite all instances of Plain to Para?
wrapInParagraphs :: Pandoc -> Pandoc
wrapInParagraphs = walk go
  where
    go :: Block -> Block
    go (Plain strs) = Para strs
    go x = x

-- Look for duplicated top-level headers, which are almost always an error.
--
-- Example:
-- > runIOorExplode (readMarkdown def ("# test\n\n# test\n\n# unique" :: T.Text)) >>= print . duplicateTopHeaders
-- ["test"]
duplicateTopHeaders :: Pandoc -> [String]
duplicateTopHeaders = duplicates . query topHeaderTexts
  where
    topHeaderTexts :: Block -> [String]
    topHeaderTexts (Header 1 _ inls) = [normalize inls]
    topHeaderTexts _                 = []

    normalize = T.unpack . T.strip . stringify

    duplicates :: Ord a => [a] -> [a]
    duplicates = map head . filter ((>1) . length) . group . sort
