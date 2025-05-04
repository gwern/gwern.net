-- A Gwern.net module for writing out annotations of my writings off-site as standalone pages on Gwern.net, in the 'blog/' subdirectory.
--
-- The pages are generated as Markdown at compile-time like the tag-directory pages, and simply transclude a specified annotation.
-- They are similar to the `/ref/` annotation lookups in spirit, but intended to act more like normal browsable essays, including having a `/blog/index` that readers can go to or monitor, and remove some of the need for /note/ or appendixes.
-- This should be called in </static/build/hakyll.hs> to update the blog entries before running the main compilation.
--
-- Background & design rationale: The 'blog' annotation-transclusion pages address a long-standing problem in the Gwern.net paradigm: it is not easy to add in 'small' or 'one-off' blog-like writings.
-- A top-level essay involves a lot of metadata & formatting, and also tends to imply a certain level of commitment: it must be named, given a stable permanent URL, summarized, formatted, updated, etc. Meanwhile, several of my most popular writings have been long comments on LW/HN/Reddit/etc, which I simply wrote as one-offs.
-- I have found that turning them into (relatively) lightweight annotations has been a good way to store copies of them for local search, and makes linking to them a lot more useful, as they accumulate backlinks, can now be embedded for similar-links, get mirrored by the local-archive system, and so on. So storing all such off-site writings as annotations is attractive.
-- But annotations have their own drawbacks: they are not easily findable or linkable. A regular reader has no idea where to find a list of these annotations, and if they have one in mind, the only URLs they would find are tag-directory section indexes, which are both unstable (the tag might change at any moment) and come with a huge overhead in terms of loading an entire tag-directory page. (Given our problems with making sure transclusion doesn't cause the page to jump around, a link to a section might not even work as far as a reader can tell!)
--
-- The `/ref/$ID` feature inspires an alternative: since these off-site writings have to get a unique manual ID to avoid them all being named 'gwern-YYYY', we can simply exploit those useful IDs to create a standalone page for each one, nested in a `/blog/YYYY/` directory to indicate their lower status; and then each standalone page simply displays the annotation as a standard annotation-transclude.
-- So any time an off-site comment gets saved as an annotation in the natural course of site maintenance, it automatically shows up as a lightweight 'blog' essay. If a LW.com comment at $URL gets saved as an annotation with an ID like `gwern-2025-drl-scaling` (defined in the miscellaneous key-value field like `[("id","gwern-2025-drl-scaling")]`), then a page will be created at <https://gwern.net/blog/2025/drl-scaling> which simply does a `[]($URL){.include-annotation)`, in effect. And this page will be nice to read, linkable, findable via <https://gwern.net/blog/index>, can be edited into a proper full essay, etc.
--
-- And since the annotation is written separately from the /blog/ shell, strictly speaking, there is nothing barring a 'recursive' blog post: one just writes a '/blog/20xx/foo' annotation... which then creates itself. This allows for true standalone blog pages.
--
-- For multi-user websites, the obvious extension is to split by author-ID (cf. <https://gwern.net/blog/2024/multiuser-wiki>).

{-# LANGUAGE OverloadedStrings #-}
module Blog (writeOutBlogEntries) where

import Data.Char (isAlphaNum)
import Control.Monad (when, unless)
import Data.List (isPrefixOf)
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M (toList, filterWithKey)
import qualified Data.Text as T (append, pack, unpack)
import Text.Pandoc (pandocExtensions, writerExtensions, writeMarkdown, def, nullMeta, runPure, Block(BulletList, Header, Para, Div), Inline(Link, Span, Str, Strong, RawInline), Format(Format), Pandoc(Pandoc))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Metadata.Date (isYear)
import LinkID (metadataItem2ID)
import LinkMetadata (sortByDatePublished)
import LinkMetadataTypes (Metadata, MetadataList, MetadataItem, Path)
import Unique (isUniqueList)
import Utils (writeUpdatedFile, printRed, replace, delete, printGreen, truncateString)
import qualified Config.Misc as C (cd, currentYear, author, authorL, currentYearS, lastYearS)

prefix, authorU, authorID :: String
prefix   = "blog"
authorU  = C.author
authorID = C.authorL
lengthMin, titleMax :: Int
lengthMin = 600
titleMax = 71 -- chosen empirically based on what lengths seem to trigger line-wrapping when transcluded on /index in Firefox/Chromium

writeOutBlogEntries :: Metadata -> IO ()
writeOutBlogEntries md =
  do let writings = filterForAuthoredAnnotations md

     let dates = map (\(_,(_,_,dc,_,_,_,_)) -> dc) writings
     let badDates = filter (\x -> length x /= 10) dates
     let badEntries = filter (\(_,(_,_,dc,_,_,_,_)) -> dc `elem` badDates) writings
     unless (null badDates) $ error $ "Blog.writeOutBlogEntries: invalid dates of blog posts detected; bad dates were: " ++ show badDates ++ "; entries: " ++ show badEntries
     let badTitles =  filter (\(_,(t,_,_,_,_,_,_)) -> length t > titleMax ) writings
     unless (null badTitles) $ printRed $ "Blog.writeOutBlogEntries: warning, entry title awkwardly long, please prune down: " ++ show badTitles

     let idents = map (\(u,mi) -> T.unpack $ metadataItem2ID md u mi) writings
     let identsInvalid = filter (not . checkIdent) idents
     unless (null identsInvalid) $ error $ "Blog.writeOutBlogEntries: invalid IDs which can’t be turned into good /blog/YYYY/slug paths. Offending entries: " ++ show identsInvalid ++ ". Expected format: 'gwern-YYYY/slug' or 'gwern-YYYY-slug'."

     -- Construct paths like 'blog/YYYY/slug.md' regardless of input ID format
     let paths = isUniqueList $ map (\ident ->
                                        let rest = drop (length authorID + 1) ident -- "YYYY/slug" or "YYYY-slug"
                                            year = take 4 rest                     -- "YYYY"
                                            separatorAndSlug = drop 4 rest         -- "/slug" or "-slug" or ""
                                            slug = drop 5 rest                     -- "slug" or ""
                                            separator = if null separatorAndSlug then '\0' else head separatorAndSlug
                                        -- Validate parsed components (partially redundant with checkIdent but good safety)
                                        in if not (isYear year) then error $ "Blog.writeOutBlogEntries: Invalid year '"++year++"' parsed from ID '" ++ ident ++ "'"
                                           else if separator /= '/' && separator /= '-' then error $ "Blog.writeOutBlogEntries: Invalid separator '"++[separator]++"' after year in ID '" ++ ident ++ "'. Expected '/' or '-'."
                                           else if null slug then error $ "Blog.writeOutBlogEntries: Empty slug parsed from ID '" ++ ident ++ "'"
                                           else if not (all (\c -> isAlphaNum c || c=='-') slug) then error $ "Blog.writeOutBlogEntries: Invalid characters in slug '"++slug++"' parsed from ID '" ++ ident ++ "'"
                                           -- Construct the consistent output path:
                                           else prefix ++ "/" ++ year ++ "/" ++ slug ++ ".md" -- Always "blog/YYYY/slug.md"
                                     )
                 idents

     let targets = zipWith (\a (b,c) -> (a, b, c)) paths writings :: [(FilePath, Path, MetadataItem)]
     C.cd
     mapM_ writeOutBlogEntry targets
     printGreen "Wrote out individual /blog/YYYY/slug.md entries."

     generateDirectoryBlog targets
     printGreen "Wrote /blog/index.md."

     generateDirectoryBlogSimplified targets
     printGreen "Wrote /blog/newest.md."

-- Checks if an ID is valid for blog post generation.
-- Expects format 'gwern-YYYY/slug' OR 'gwern-YYYY-slug'
checkIdent :: String -> Bool
checkIdent "" = False
checkIdent ident
  | length ident < (length authorID + 1 + 4 + 1 + 1) = False -- Minimum length for 'gwern-YYYY?s'
  | length ident > 45 = False -- Arbitrary upper limit
  -- Check prefix and overall structure
  | authorID `isPrefixOf` ident &&
    (let rest = drop (length authorID + 1) ident -- "YYYY/slug" or "YYYY-slug"
         year = take 4 rest                     -- "YYYY"
         separatorAndSlug = drop 4 rest         -- "/slug" or "-slug" or ""
         slug = drop 5 rest                     -- "slug" or ""
         separator = if null separatorAndSlug then '\0' else head separatorAndSlug
     in -- Check Year:
        year /= "" && isYear year && year >= "2009" && year < show (C.currentYear+1) &&
        -- Check Separator: Must be '-' or '/'
        (separator == '/' || separator == '-') &&
        -- Check Slug: Must exist and have valid characters
        not (null slug) && all (\c -> isAlphaNum c || c=='-') slug
    ) = True
  | otherwise = False

filterForAuthoredAnnotations :: Metadata -> MetadataList
filterForAuthoredAnnotations md =
  M.toList $ M.filterWithKey (\url (_,aut,_,_,_,_,abst) ->
                                 (head url /= '/' || "/blog/" `isPrefixOf` url) &&
                                  authorU `isPrefixOf` aut &&
                                  length abst > lengthMin)
  md

writeOutBlogEntry :: (FilePath, Path, MetadataItem) -> IO ()
writeOutBlogEntry (filepath, p,mi) = do
    createDirectoryIfMissing True (takeDirectory filepath)
    writeUpdatedFile prefix filepath $ T.pack $ annotation2Markdown p mi

annotation2Markdown :: Path -> MetadataItem -> String
annotation2Markdown url (title, author, dateCreated, dateModified, kvs, _, _) =
  let get k defalt = fromMaybe defalt (lookup k kvs)
      description = get "description"   "N/A"
      status      = get "status"         "finished"
      importance  = get "importance"     "0"
      confidence  = get "confidence"     "log"
      cssExt      = get "css-extension"  "dropcaps-de-zs"
      thumbnail   = get "thumbnail"      ""
      thumbnailT  = get "thumbnail-text" ""
  in unlines $
       [ "---"
       , "title: '"              ++ replace "'" "’" title ++ "'"
       , "author: "              ++ author
       , "description: "         ++ ("\"" ++ replace "'" "’" description ++ "\"")
       ] ++
       (if null thumbnail then [] else ["thumbnail: " ++ thumbnail]) ++
       (if null thumbnailT then [] else ["thumbnail-text: \"" ++ thumbnailT ++ "\""]) ++
       [
       "created: "             ++ dateCreated
       , "modified: "            ++ dateModified
       , "status: "              ++ status
       , "importance: "          ++ importance
       , "confidence: "          ++ confidence
       , "css-extension: "       ++ cssExt
       , "backlink: False"
       , "placeholder: True"
       , "index: True"
       , "..."
       , ""
       , "[" ++ (if description /= "N/A" then description else "**Original page.**") ++
         "](" ++ url ++ "){.include-annotation .include-strict" ++
         (if "/blog" `isPrefixOf` url then "" else " rel='canonical'") ++
         " data-include-template='annotation-blockquote-not' .include-spinner-not .id-not}"
       , ""
       , "<div class='text-center' id='return-to-blog-index-link'>[<a href='/blog/index' class='link-page link-tag directory-indexes-upwards link-annotated-not' data-link-icon='arrow-up-left' data-link-icon-type='svg' rel='tag' title='Link to blog directory'>Return to blog index</a>]</div>"
       ]

generateDirectoryBlog :: [(FilePath, Path, MetadataItem)] -> IO ()
generateDirectoryBlog targets = do
  when (null targets) $ error "generateDirectory.generateDirectoryBlog: no blog entries found! This should be impossible."
  let sortedPosts = sortByDatePublished $ map (\(fp, origPath, mi) -> ("/" ++ delete ".md" fp, mi, origPath)) targets
  when (null sortedPosts) $ error "generateDirectory.generateDirectoryBlog: sortedPosts is empty, processing failed."

  let lastEntryDate = (\(_,(_,_,date,_,_,_,_),_) -> date) $ head sortedPosts
  let listLinksByYear = BulletList $ generateBlogLinksByYears sortedPosts
  let listTranscludesByYear = BulletList $ generateBlogTranscludes (zip (True : repeat False) sortedPosts)

  let header = unlines ["---", "title: Blog Posts"
                       , "description: 'Index of my longer off-site writings, presented as annotations. (Sorted in reverse chronological order.)'"
                       , "created: 2009-01-27", "modified: " ++ lastEntryDate
                       , "status: log", "importance: 0", "css-extension: dropcaps-de-zs"
                       , "backlink: False", "placeholder: True", "index: True"
                       , "...\n"]

  let blogSectionTransclude = Header 1 ("", [], []) [Str "View Full Posts"]
  let document = Pandoc nullMeta [listLinksByYear, blogSectionTransclude, listTranscludesByYear]
  let p = runPure $ writeMarkdown def{writerExtensions = pandocExtensions} document

  case p of
    Left e   -> printRed (show e)
    Right p' -> do let contentsNew = T.pack header `T.append` p'
                   writeUpdatedFile "directory" "blog/index.md" contentsNew

generateBlogLinksByYears :: [(FilePath, MetadataItem, Path)] -> [[Block]]
generateBlogLinksByYears sortedPosts = let years = nubOrd $ map (\(_, (_,_,dc,_,_,_,_), _) -> take 4 dc) sortedPosts
                                       in map (\y -> Para [Strong [Span (T.pack y,[],[]) [Str (T.pack y)], Str ":"]] : [generateBlogLinksByYear y]) years
  where
    generateBlogLinksByYear :: String -> Block
    generateBlogLinksByYear year = let hits = filter (\(_, (_,_,dc,_,_,_,_), _) -> year `isPrefixOf` dc) sortedPosts
                                       list = BulletList $ map generateBlogLink hits
                                   in if year == C.currentYearS then Div ("year-current",[],[]) [list]
                                      else if year == C.lastYearS then Div ("year-last",[],[]) [list]
                                           else list

generateBlogLink :: (FilePath, MetadataItem, Path) -> [Block]
generateBlogLink (urlPath, (tle,_,dc,_,_,_,_), _) =
  let link = Link (T.pack dc, ["link-annotated-not", "icon-not"], [("data-include-selector-not", "#return-to-blog-index-link")])
                                      [RawInline (Format "html") (T.pack $ truncateString 70 tle)] (T.pack urlPath,"")
  in [Para [Str (T.pack (drop 5 dc ++ ": ")), Strong [link]]]

generateBlogTranscludes :: [(Bool, (FilePath, MetadataItem, Path))] -> [[Block]]
generateBlogTranscludes sortedPostsWithBool = let years = nubOrd $ map (\(_, (_, (_,_,dc,_,_,_,_), _)) -> take 4 dc) sortedPostsWithBool
                                       in map (\y -> Para [Strong [Span (T.pack $ "transclude-"++y,[],[]) [Str (T.pack y)], Str ":"]] : [generateBlogTranscludesByYear y]) years
  where
    generateBlogTranscludesByYear :: String -> Block
    generateBlogTranscludesByYear year = let hits = filter (\(_, (_,(_,_,dc,_,_,_,_), _)) -> year `isPrefixOf` dc) sortedPostsWithBool
                                       in Div ("",["collapse"],[]) $ concatMap generateBlogTransclude hits

generateBlogTransclude :: (Bool, (FilePath, MetadataItem, Path)) -> [Block]
generateBlogTransclude (firstp, (urlPath, (tle,_,_,_,_,_,_), _)) =
  let link = Link (""
                  , ["id-not", "link-annotated-not", "icon-not", "include-content"]++ (if firstp then ["include-even-when-collapsed"] else [])
                  , [("data-include-selector-not", "#return-to-blog-index-link")])
                                      [RawInline (Format "html") (T.pack tle)] (T.pack urlPath,"")
  in [Para [Strong [link]]]

generateDirectoryBlogSimplified :: [(FilePath, Path, MetadataItem)] -> IO ()
generateDirectoryBlogSimplified [] = error "Blog.generateDirectoryBlogSimplified: passed no blog posts, which is impossible!"
generateDirectoryBlogSimplified items =
  do let items' = take 29 $ sortByDatePublished $ map (\(fp, u, mi) -> (fp,mi,u)) items
     when (null items') $ error "Blog.generateDirectoryBlogSimplified: items' list is empty after sorting/taking."

     let lastEntryDate = (\(_,(_,_,date,_,_,_,_),_) -> date) $ head items'
     let header = unlines ["---", "title: Recent Blog Posts"
                          , "description: 'Index of my most recent longer off-site writings, presented as annotations. (Sorted in reverse chronological order. Intended for transclusion onto the homepage index.)'"
                          , "created: 2009-01-27", "modified: " ++ lastEntryDate
                          , "status: log", "importance: 0", "confidence: log", "css-extension: dropcaps-de-zs"
                          , "backlink: False", "placeholder: False", "index: True"
                          , "...\n"]
     let body = Div ("newest-list",["columns"],[]) [
           BulletList (
               map (\(f, (tle, _, _, _, _, _, _), u) ->
                   [Para [Link ("",["link-modified-recently-not", "link-annotated-not", "icon-not"],[])
                        [RawInline (Format "html") (T.pack tle)]
                        (T.pack ("/" ++ delete ".md" f), if head u == '/' then "" else T.pack $ "Original URL: <" ++ u ++ ">")]]) items' ++
                [ [Para [Link ("",["link-modified-recently-not", "link-annotated-not", "icon-not"],[]) [Str "[…]"] ("/blog/index", "Full index of blog entries.")]] ]
               )
             ]
     let document = Pandoc nullMeta [body]
     let p = runPure $ writeMarkdown def{writerExtensions = pandocExtensions} document
     case p of
       Left e   -> printRed (show e)
       Right p' -> do let contentsNew = T.pack header `T.append` p'
                      writeUpdatedFile "directory" "blog/newest.md" contentsNew
