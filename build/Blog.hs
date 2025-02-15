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
-- Future work: Depending on volume, it may make sense to split into subdirectories by year. For multi-user websites, the obvious extension is to split by author-ID.

module Blog (writeOutBlogEntries) where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M (toList, filterWithKey)
import qualified Data.Text as T (pack, unpack)

import LinkID (metadataItem2ID)
import LinkMetadataTypes (Metadata, MetadataList, MetadataItem, Path)
import Test (isUniqueList)
import Utils (sed, writeUpdatedFile)
import Config.Misc as C (cd)

prefix, authorU, authorID :: String
prefix   = "blog"
authorU  = "Gwern"
authorID = "gwern"
lengthMin :: Int
lengthMin = 1000

writeOutBlogEntries :: Metadata -> IO ()
writeOutBlogEntries md =
  do let writings = filterForAuthoredAnnotations md
     let paths = isUniqueList $ map (\(u,mi) -> prefix ++ "/" ++ (sed ("^"++authorID++"-") "" $
                                                                  T.unpack $ metadataItem2ID md u mi) ++ ".md") writings
     let targets = zip paths writings
     C.cd -- ensure the relative directory prefix is valid
     mapM_ writeOutBlogEntry targets

filterForAuthoredAnnotations :: Metadata -> MetadataList
filterForAuthoredAnnotations md =
  M.toList $ M.filterWithKey (\url (_,aut,_,_,_,_,abst) ->
                                 head url /= '/' && authorU `isPrefixOf` aut && length abst > lengthMin) md

writeOutBlogEntry :: (String, (Path, MetadataItem)) -> IO ()
writeOutBlogEntry (filepath, m) = writeUpdatedFile prefix filepath $ T.pack $ annotation2Markdown m

-- cf. `generateDirectory.generateYAMLHeader`
annotation2Markdown :: (Path, MetadataItem) -> String
annotation2Markdown (url, (title, author, dateCreated, dateModified, kvs, _, _)) =
  let get k def = fromMaybe def (lookup k kvs)
      description = get "description"   "N/A" -- TODO: maybe do a LLM call? a one-sentence summary should be easy
      status      = get "status"        "finished"
      importance  = get "importance"    "0"
      confidence  = get "confidence"    "log"
      cssExt      = get "css-extension" "dropcaps-de-zs"
  in unlines
       [ "---"
       , "title: \""             ++ title ++ "\""
       , "author: "              ++ author
       , "description: "         ++ description
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
       , "[**Original page.**](" ++ url ++ "){.include-annotation .include-strict .include-spinner-not .id-not}"
       ]
