-- A Gwern.net module for writing out annotations of my writings off-site as standalone pages on Gwern.net, in the 'blog/' subdirectory.
--
-- The pages are generated as Markdown at compile-time like the tag-directory pages, and simply transclude a specified annotation.
-- They are similar to the `/ref/` annotation lookups in spirit, but intended to act more like normal browsable essays, including having a `/blog/index` that readers can go to or monitor, and remove some of the need for /note/ or appendixes.
-- This should be called in <hakyll.hs> to update the blog entries before running the main compilation.
--
-- Background & design rationale: The 'blog' annotation-transclusion pages address a long-standing problem in the Gwern.net paradigm: it is not easy to add in 'small' or 'one-off' blog-like writings.
-- A top-level essay involves a lot of metadata & formatting, and also tends to imply a certain level of commitment: it must be named, given a stable permanent URL, summarized, formatted, updated, etc. Meanwhile, several of my most popular writings have been long comments on LW/HN/Reddit/etc, which I simply wrote as one-offs.
-- I have found that turning them into (relatively) lightweight annotations has been a good way to store copies of them for local search, and makes linking to them a lot more useful, as they accumulate backlinks, can now be embedded for similar-links, get mirrored by the local-archive system, and so on. So storing all such off-site writings as annotations is attractive.
-- But annotations have their own drawbacks: they are not easily findable or linkable. A regular reader has no idea where to find a list of these annotations, and if they have one in mind, the only URLs they would find are tag-directory section indexes, which are both unstable (the tag might change at any moment) and come with a huge overhead in terms of loading an entire tag-directory page. (Given our problems with making sure transclusion doesn't cause the page to jump around, a link to a section might not even work as far as a reader can tell!)
--
-- The `/ref/$ID` feature inspires an alternative: since these off-site writings have to get a unique manual ID to avoid them all being named 'gwern-YYYY', we can simply exploit those useful IDs to create a standalone page for each one, nested in a `/blog/` directory to indicate their lower status; and then each standalone page simply displays the annotation as a standard annotation-transclude.
-- So any time an off-site comment gets saved as an annotation in the natural course of site maintenance, it automatically shows up as a lightweight 'blog' essay. If a LW.com comment at $URL gets saved as an annotation with an ID like `gwern-2025-drl-scaling` (defined in the miscellaneous key-value field like `[("id","gwern-2025-drl-scaling")]`), then a page will be created at <https://gwern.net/blog/2025-drl-scaling> which simply does a `[]($URL){.include-annotation)`, in effect. And this page will be nice to read, linkable, findable via <https://gwern.net/blog/index>, can be edited into a proper full essay, etc.
--
-- And since the annotation is written separately from the /blog/ shell, strictly speaking, there is nothing barring a 'recursive' blog post: one just writes a '/blog/20xx-foo' annotation... which then creates itself. This allows for true standalone blog pages.
--
-- Future work: Depending on volume, it may make sense to split into subdirectories by year. For multi-user websites, the obvious extension is to split by author-ID (cf. <https://gwern.net/blog/2024-multiuser-wiki>)

{-# LANGUAGE OverloadedStrings #-}
module Blog (writeOutBlogEntries) where

import Data.Char (isAlphaNum)
import Control.Monad (when, unless)
import Data.List (isPrefixOf)
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M (toList, filterWithKey, lookup)
import qualified Data.Text as T (append, pack, unpack)
import Text.Pandoc (pandocExtensions, writerExtensions, writeMarkdown, def, nullMeta, runPure, Block(BulletList, Header, Para, Div), Inline(Link, Span, Str, Strong, RawInline), Format(Format), Pandoc(Pandoc))
import System.Directory (listDirectory)

import Metadata.Date (isYear)
import LinkID (metadataItem2ID, id2URLdb)
import LinkMetadata (sortByDatePublished)
import LinkMetadataTypes (Metadata, MetadataList, MetadataItem, Path)
import Unique (isUniqueList)
import Utils (sed, writeUpdatedFile, printRed, replace, delete, printGreen, truncateString)
import qualified Config.Misc as C (cd, currentYear, author, authorL, currentYearS, lastYearS)

prefix, authorU, authorID :: String
prefix   = "blog"
authorU  = C.author
authorID = C.authorL
lengthMin :: Int
lengthMin = 1000

writeOutBlogEntries :: Metadata -> IO ()
writeOutBlogEntries md =
  do let writings = filterForAuthoredAnnotations md

     -- Dates are required to be full YYYY-MM-DD dates & unique for IDs; date validity in general is checked in LinkMetadata, so we only need to check for length here:
     let dates = map (\(_,(_,_,dc,_,_,_,_)) -> dc) writings
     let badDates = filter (\x -> length x /= 10) dates
     let badEntries = filter (\(_,(_,_,dc,_,_,_,_)) -> dc `elem` badDates) writings
     unless (null badDates) $ error $ "Blog.writeOutBlogEntries: invalid dates of blog posts detected; bad dates were: " ++ show badDates ++ "; entries: " ++ show badEntries
     -- we'd also like titles to not take up >1 line, to maintain the classic blog-index look of 1 line = 1 post
     let badTitles =  filter (\(_,(t,_,_,_,_,_,_)) -> length t > 73 ) writings
     unless (null badTitles) $ printRed $ "Blog.writeOutBlogEntries: warning, entry title awkwardly long, please prune down: " ++ show badTitles

     let idents = map (\(u,mi) -> T.unpack $ metadataItem2ID md u mi) writings
     let identsInvalid = filter (not . checkIdent) idents
     unless (null identsInvalid) $ error $ "Blog.writeOutBlogEntries: invalid IDs which can’t be turned into good /blog/$SLUG. Offending entries: " ++ show identsInvalid

     let paths = isUniqueList $ map (\ident -> prefix ++ "/" ++
                                                      sed ("^"++authorID++"-") "" ident ++
                                                      ".md")
                 idents
     let targets = zipWith (\a (b,c) -> (a, b, c)) paths writings :: [(FilePath, Path, MetadataItem)]
     C.cd -- ensure the relative directory prefix is valid
     mapM_ writeOutBlogEntry targets
     printGreen "Wrote out individual /blog/20*.md entries."

     generateDirectoryBlog md
     printGreen "Wrote /blog/index.md."

     generateDirectoryBlogSimplified targets
     printGreen "Wrote /blog/newest.md."

checkIdent :: String -> Bool
checkIdent "" = False
checkIdent ident
  | length ident < 7 || length ident > 37 = False
  | not (all (\c -> isAlphaNum c || c=='-') ident) = False
  | authorID `isPrefixOf` ident &&
    (let year = takeWhile (/='-') (drop (length authorID + 1) ident)
     in year/="" &&
        isYear year &&
        year >= "2009" && year < show (C.currentYear+1)) = True
  | otherwise = False

filterForAuthoredAnnotations :: Metadata -> MetadataList
filterForAuthoredAnnotations md =
  M.toList $ M.filterWithKey (\url (_,aut,_,_,_,_,abst) ->
                                 (head url /= '/' || "/blog/" `isPrefixOf` url) &&
                                  authorU `isPrefixOf` aut &&
                                  length abst > lengthMin)
  md

writeOutBlogEntry :: (FilePath, Path, MetadataItem) -> IO ()
writeOutBlogEntry (filepath, p,mi) = writeUpdatedFile prefix filepath $ T.pack $ annotation2Markdown p mi

-- cf. `generateDirectory.generateYAMLHeader`
annotation2Markdown :: Path -> MetadataItem -> String
annotation2Markdown url (title, author, dateCreated, dateModified, kvs, _, _) =
  let get k defalt = fromMaybe defalt (lookup k kvs)
      description = get "description"   "N/A" -- TODO: maybe do a LLM call? a one-sentence summary should be easy
      status      = get "status"        "finished"
      importance  = get "importance"    "0"
      confidence  = get "confidence"    "log"
      cssExt      = get "css-extension" "dropcaps-de-zs"
  in unlines
       [ "---"
       , "title: '"              ++ replace "'" "’" title ++ "'"
       , "author: "              ++ author
       , "description: "         ++ replace "'" "’" description
       , "created: "             ++ dateCreated
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
       , "<div class='text-center' id='return-to-blog-index-link'>[<a href='/blog/index' class='link-page link-tag directory-indexes-upwards link-annotated-not' data-link-icon='arrow-up-left' data-link-icon-type='svg' rel='tag' title='Link to blog directory'>Return to blog index</a>]</div>" -- we set an ID to allow the transclusion calls in /blog/index to hide it
       ]

generateDirectoryBlog :: Metadata -> IO ()
generateDirectoryBlog md = do

  let iddb = id2URLdb md -- [(ID, Path)]
  direntries <- fmap (filter (\f -> f/="index.md" && f/="newest.md")) $ -- filter out self
                listDirectory "blog/" -- eg. '2024-writing-online.md'
  let absolutePaths = map (\m -> "/blog/" ++ delete ".md" m) direntries -- eg. '/blog/2024-writing-online'
  let idents = zip (map ("gwern-"++) $ map (delete ".md") direntries) absolutePaths -- eg. '("gwern-2024-writing-online", "/blog/2024-writing-online")'
  let paths = map (\(ident,absolute) -> (ident, absolute, fromMaybe (error ("generateDirectoryBlog.blog: something went wrong. idents: " ++ show idents)) $ lookup ident iddb)) idents -- eg. '("gwern-2024-writing-online","/blog/2024-writing-online","https://www.lesswrong.com/posts/PQaZiATafCh7n5Luf/gwern-s-shortform?commentId=KAtgQZZyadwMitWtb")'
  let doublets = map (\(a,b,_) -> (a,b)) $ -- we don't need 'path' anywhere after AFAICT
                       sortByDatePublished $
                       map (\x@(_ident,absolute,path) -> case M.lookup path md of
                        Nothing -> error $ "generateDirectory.generateDirectoryBlog: tried to look up the annotation corresponding to a /blog/ Markdown entry, yet there was none. This should be impossible! Variables: " ++ show x ++ "; " ++ show paths ++ ";" ++ show idents ++ "; " ++ show direntries
                        Just mi -> (absolute, mi, path)
                    ) paths
  when (null doublets) $ error "generateDirectory.generateDirectoryBlog: no blog entries found! This should be impossible."
  let lastEntryDate = (\(_,(_,_,date,_,_,_,_)) -> date) $ head doublets
  let list1 = BulletList $ generateBlogLinksByYears doublets
  let list2 = BulletList $ generateBlogTranscludes (zip (True : repeat False) doublets) -- note: we may at some point want to split by year, and then wrap in a `div.collapse`, to allow selective uncollapsing.

  let header = unlines ["---", "title: Blog Posts"
                       , "description: 'Index of my longer off-site writings, presented as annotations. (Sorted in reverse chronological order.)'"
                       -- N/A: author, thumbnail, thumbnail-text, thumbnail-css
                       , "created: 2009-01-27"
                       , "modified: " ++ lastEntryDate
                       , "status: log"
                       , "importance: 0"
                       , "css-extension: dropcaps-de-zs"
                       , "backlink: False"
                       , "placeholder: True"
                       , "index: True"
                       , "...\n"]

  let blogSectionTransclude = Header 1 ("", [], []) [Str "View Full Posts"]
  let document = Pandoc nullMeta [list1,
                                  blogSectionTransclude,
                                  list2]
  let p = runPure $ writeMarkdown def{writerExtensions = pandocExtensions} document

  case p of
    Left e   -> printRed (show e)
    -- compare with the old version, and update if there are any differences:
    Right p' -> do let contentsNew = T.pack header `T.append` p'
                   writeUpdatedFile "directory" "blog/index.md" contentsNew

generateBlogLinksByYears :: [(FilePath, MetadataItem)] -> [[Block]]
generateBlogLinksByYears doublets = let years = nubOrd $ map (\(_, (_,_,dc,_,_,_,_)) -> take 4 dc) doublets
                                       in map (\y -> Para [Strong [Span (T.pack y,[],[]) [Str (T.pack y)], Str ":"]] : [generateBlogLinksByYear y]) years
  where
    generateBlogLinksByYear :: String -> Block
    generateBlogLinksByYear year = let hits = filter (\(_, (_,_,dc,_,_,_,_)) -> year `isPrefixOf` dc) doublets
                                       -- we may at some point want to wrap these sub-lists in a `div.collapse`
                                       list = BulletList $ map generateBlogLink hits
                                       -- to allow transclusion of 'most recent blog entries', we wrap the first entry, this year, as 'div#year-current', and then since that might not contain many entries (what if it's 01 January?), we also wrap the prior year as 'div#year-last':
                                   in if year == C.currentYearS then Div ("year-current",[],[]) [list]
                                      else if year == C.lastYearS then
                                             Div ("year-last",[],[]) [list]
                                           else list

generateBlogLink :: (FilePath, MetadataItem) -> [Block]
generateBlogLink (f, (tle,_,dc,_,_,_,_)) =
  let link = Link (T.pack dc, ["link-annotated-not", "icon-not"], [("data-include-selector-not", "#return-to-blog-index-link")])
                                      [RawInline (Format "html") (T.pack $ truncateString 70 tle)] (T.pack f,"")
  in
    [Para [Str (T.pack (drop 5 dc ++ ": ")),
           Strong [link]]]

generateBlogTranscludes :: [(Bool, (FilePath, MetadataItem))] -> [[Block]]
generateBlogTranscludes doublets = let years = nubOrd $ map (\(_, (_, (_,_,dc,_,_,_,_))) -> take 4 dc) doublets
                                       in map (\y -> Para [Strong [Span (T.pack $ "transclude-"++y,[],[]) [Str (T.pack y)], Str ":"]] : [generateBlogTranscludesByYear y]) years
  where
    generateBlogTranscludesByYear :: String -> Block
    generateBlogTranscludesByYear year = let hits = filter (\(_, (_,(_,_,dc,_,_,_,_))) -> year `isPrefixOf` dc) doublets
                                       in Div ("",["collapse"],[]) $ concatMap generateBlogTransclude hits

generateBlogTransclude :: (Bool, (FilePath, MetadataItem)) -> [Block]
generateBlogTransclude (firstp, (f, (tle,_,_,_,_,_,_))) =
  let link = Link (""
                  , ["id-not", "link-annotated-not", "icon-not", "include-content"]++
                    (if firstp then ["include-even-when-collapsed"] else []) -- Micro-optimization in annotation evaluation order: force the very first entry to be pre-rendered, for a faster popup if they hover over the logical entry in the first section (ie. the first one), or to mask how long it takes to load them all if they uncollapse the second section.
                  , [("data-include-selector-not", "#return-to-blog-index-link")]) -- exclude as redundant if you're viewing on /blog/index, especially since it'll be transcluded repeatedly after each post
                                      [RawInline (Format "html") (T.pack tle)] (T.pack f,"")
  in
    [Para [Strong [link]]]

-- Create a list of the most recent 30 blog post items, which can be transcluded onto </index> to accompany the Newest/Popular/Notable section.
-- The value of 30 is chosen to create 3×10 columns to parallel those 3 sections (and to let it reflow nicely and do the same trick of lopping off the last third if the screen is too narrow); but we want to link to the blog post index at the end as an ellipsis, so it's actually just 29 blog post entries being linked.
-- It would be annoying and a bit bloated to make this a separate section on </blog/index> (do we hide it with CSS?), so we generate it as a separate page similar to that but simplified and specialized.
generateDirectoryBlogSimplified :: [(FilePath, FilePath, MetadataItem)] -> IO ()
generateDirectoryBlogSimplified [] = error $ "Blog.generateDirectoryBlogSimplified: passed no blog posts, which is impossible!"
generateDirectoryBlogSimplified items =
  do let items' = take 29 $ sortByDatePublished $ map (\(fp, u, mi) -> (fp,mi,u)) items
     let lastEntryDate = (\(_,(_,_,date,_,_,_,_),_) -> date) $ head items'
     let header = unlines ["---", "title: Recent Blog Posts"
                          , "description: 'Index of my most recent longer off-site writings, presented as annotations. (Sorted in reverse chronological order. Intended for transclusion onto the homepage index.)'"
                          -- N/A: author, thumbnail, thumbnail-text, thumbnail-css
                          , "created: 2009-01-27"
                          , "modified: " ++ lastEntryDate
                          , "status: log"
                          , "importance: 0"
                          , "css-extension: dropcaps-de-zs"
                          , "backlink: False"
                          , "placeholder: False"
                          , "index: True"
                          , "...\n"]
     let body = Div ("newest-list",[],[]) [
           BulletList (
               map (\(_, (tle, _, _, _, _, _, _), u) -> [Para [Link ("",[],[]) [RawInline (Format "html") (T.pack tle)] (T.pack u,"")]]) items' ++
                [ [Para [Link ("",[],[]) [Strong [Str "…"]] ("/blog/index", "Full index of blog entries.")]] ]
               )
             ]
     let document = Pandoc nullMeta [body]
     let p = runPure $ writeMarkdown def{writerExtensions = pandocExtensions} document
     case p of
       Left e   -> printRed (show e)
       -- compare with the old version, and update if there are any differences:
       Right p' -> do let contentsNew = T.pack header `T.append` p'
                      writeUpdatedFile "directory" "blog/newest.md" contentsNew


