{-# LANGUAGE OverloadedStrings #-}
module Tags where

import Data.Char (toLower)
import Control.Monad (filterM, unless)
import Data.Containers.ListUtils (nubOrd)
import Data.List (isSuffixOf, isInfixOf, isPrefixOf, sort, intersperse)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, (</>))
import Text.Pandoc (Inline(Str, RawInline, Link, Span), Format(..), Block(Para, Div), nullAttr)
import qualified Data.Map.Strict as M (elems, map, toList )
import qualified Data.Text as T (append, pack, unpack, Text)

import Cycle (isCycleLess)
import LinkMetadataTypes (Metadata)
import Utils (anyInfix, replace, replaceChecked, sed, sedMany, trim, split, replaceMany, frequency, pairs, fixedPoint)
import Config.Tags as C
import Config.Misc (cd)

-- inline `dir-traverse` package to remove dependency since it's so small, & hasn't changed since release:
-- original code: `import System.Directory.Recursive (getDirFiltered, getSubdirsRecursive) -- dir-traverse`
-- NOTE: removed `unsafeInterleaveIO` <https://hackage.haskell.org/package/base-4.19.1.0/docs/src/GHC.IO.Unsafe.html#unsafeInterleaveIO> because we read tags strictly, for immediate use, and there's no benefit to deferring it.
import Data.Foldable (fold)

-- | Recursively get all subdirectories in the given directory.
getSubdirsRecursive :: FilePath -> IO [FilePath]
getSubdirsRecursive = getDirFiltered doesDirectoryExist

-- | Recursively get all files and subdirectories in the given directory that
-- satisfy the given predicate. Note that the content of subdirectories not
-- matching the filter is ignored. In particular, that means something like
-- @getDirFiltered doesFileExist@ will /not/ recursively return all files.
getDirFiltered :: (FilePath -> IO Bool) -- ^ Filepath filter
               -> FilePath
               -> IO [FilePath]
getDirFiltered p fp = do
    all' <- listDirectory fp
    all'' <- filterM p (mkRel <$> all')
    dirs <- filterM doesDirectoryExist all''
    case dirs of
        [] -> pure all''
        ds -> do
            next <- foldMapA (getDirFiltered p) ds
            pure $ all'' ++ next
    where mkRel = (fp </>)
          foldMapA = (fmap fold .) . traverse

-- Remind to refine link tags: should be <100. (We count using the annotation database instead of counting files inside each directory because so many are now cross-tagged or virtual.)
tagMax, tagPairMax :: Int
tagMax = 100
tagPairMax = 11
tagCount :: Metadata -> [(Int,String)]
tagCount = frequency . concatMap (\(_,(_,_,_,_,_,tags,_)) -> tags) . M.toList
tagPairsCount :: Metadata -> [(Int,(String,String))]
tagPairsCount md = reverse $ frequency $ concatMap pairs $ M.elems $ M.map (\(_,_,_,_,_,ts,abst) -> if null abst || null ts then [] else ts) md

-- Compile tags down into a Span containing a list of links to the respective /doc/ directory indexes which will contain a copy of all annotations corresponding to that tag/directory.
--
-- Simple version:
-- > tagsToLinksSpan "economics genetics/heritable psychology/writing"
-- →
-- Span ("",["link-tags"],[])
--   [Link ("",["link-tag"],[]) [Str "economics"] ("/doc/economics/index",""),Str ", ",
--     Link ("",["link-tag"],[]) [Str "genetics/heritable"] ("/doc/genetics/heritable/index",""),Str ", ",
--     Link ("",["link-tag"],[]) [Str "psychology/writing"] ("/doc/psychology/writing/index","")
--   ]
-- Markdown:
-- →
-- [[economics](/doc/economics/index){.link-tag}, [genetics/heritable](/doc/genetics/heritable/index){.link-tag}, [psychology/writing](/doc/psychology/writing/index){.link-tag}]{.link-tags}
-- HTML:
-- →
-- <span class="link-tags">
--   <a href="/doc/economics/index" class="link-tag">economics</a>,
--   <a href="/doc/genetics/heritable/index" class="link-tag">genetics/heritable</a>,
--   <a href="/doc/psychology/writing/index" class="link-tag">psychology/writing</a>
-- </span>
tagsToLinksSpan :: [T.Text] -> Inline
tagsToLinksSpan [] = Span nullAttr []
tagsToLinksSpan [""] = Span nullAttr []
tagsToLinksSpan ts =
                       Span ("", ["link-tags"], []) (tagsToLinks ts)
-- Ditto; but since a Div is a Block element, we copy-paste a separate function:
tagsToLinksDiv :: [T.Text] -> Block
tagsToLinksDiv [] = Div nullAttr []
tagsToLinksDiv [""] = Div nullAttr []
tagsToLinksDiv ts = Div ("", ["link-tags"], []) [Para $ tagsToLinks ts]
tagsToLinks :: [T.Text] -> [Inline]
tagsToLinks [] = []
tagsToLinks ts = let tags = sort ts in
                   intersperse (Str ", ") $
                   map (\tag ->
                          Link ("", ["link-tag", "link-page", "link-annotated", "icon-not"], [("rel","tag")]) [RawInline (Format "html") $ abbreviateTag tag] ("/doc/"`T.append`tag`T.append`"/index", "Link to "`T.append`tag`T.append`" tag index")
                       ) tags

-- if a local '/doc/*' file and no tags available, try extracting a tag from the path; eg. '/doc/ai/2021-santospata.pdf' → 'ai', '/doc/ai/anime/2021-golyadkin.pdf' → 'ai/anime' etc; tags must be lowercase to map onto directory paths, but we accept uppercase variants (it's nicer to write 'economics sociology Japanese' than 'economics sociology japanese')
tag2TagsWithDefault :: String -> String -> [String]
tag2TagsWithDefault path tags = let tags' = map (trim . map toLower) $ split " " $ replace "," "" tags
                                    defTag = if ("/doc/" `isPrefixOf` path) && not (C.tagGuessBlacklist path) then tag2Default path else ""
                                in
                                  if defTag `elem` tags' || defTag == "" || defTag == "/doc" then tags' else defTag:tags'

tag2Default :: String -> String
tag2Default path = if "/doc/" `isPrefixOf` path && not ("/doc/" `isPrefixOf` path && ("/index" `isSuffixOf` path || "/index#" `isInfixOf` path)) then replace "/doc/" "" $ takeDirectory path else ""

-- de-duplicate tags: uniquefy, and remove the more general tags in favor of nested (more specific) tags. eg. ["ai", "ai/nn/transformer/gpt", "reinforcement-learning"] → ["ai/nn/transformer/gpt", "reinforcement-learning"]
uniqTags :: [String] -> [String]
uniqTags tags = nubOrd $ sort $ filter(\t -> not (any ((t++"/") `isPrefixOf`) tags)) tags

-- guess tag based on URL
pages2Tags :: String -> [String] -> [String]
pages2Tags path oldTags = url2Tags path ++ oldTags

-- We also do general-purpose heuristics on the path/URL: any page in a domain might be given a specific tag, or perhaps any URL with the string "deepmind" might be given a 'reinforcement-learning/deepmind' tag—that sort of thing.
url2Tags :: String -> [String]
url2Tags p = concatMap (\(match,tag) -> [tag | match p]) C.urlTagDB

-- Abbreviate displayed tag names to make tag lists more readable.
-- For some tags, like 'reinforcement-learning/*' or 'genetics/*', they might be used very heavily and densely, leading to cluttered unreadable tag lists, and discouraging use of meaningful directory names: 'reinforcement-learning/exploration, reinforcement-learning/alphago, reinforcement-learning/meta-learning, reinforcement-learning/...' would be quite difficult to read. But we also would rather not abbreviate the tag itself down to just 'rl/', as that is not machine-readable or explicit.
-- But every problem in CS can be solved by another layer of indirection, so we can abbreviate them just for 'display', while rendering the tags to Inline elements.
abbreviateTag :: T.Text -> T.Text
abbreviateTag = T.pack . sedMany C.wholeTagRewritesRegexes . replaceMany C.tagsLong2Short . replace "/doc/" "" . T.unpack

listTagsAll :: IO [String]
listTagsAll = do Config.Misc.cd
                 fmap (map (replace "doc/" "") . sort . filter (\f' -> not $ anyInfix f' C.tagListBlacklist) ) $ getDirFiltered (\f -> doesFileExist (f++"/index.md")) "doc/"

-- given a list of ["doc/foo/index.md"] directories, convert them to what will be the final absolute path ("/doc/foo/index"), while checking they exist (typos are easy, eg. dropping 'doc/' is common).
-- Bool argument = whether to include all sub-directories recursively.
listTagDirectories :: Bool -> [FilePath] -> IO [FilePath]
listTagDirectories allp direntries' = do
                       directories <- if allp then mapM (getSubdirsRecursive . sed "^/" "" . sed "/index$" "/" . replaceChecked "/index.md" "/") direntries'
                                      else return [map (sed "^/" "" . sed "/index$" "/" . replaceChecked "/index.md" "/") direntries']
                       let directoriesMi = map (replace "//" "/" . (++"/index")) (concat directories)
                       directoriesVerified <- filterM (\f -> doesFileExist (f++".md")) directoriesMi
                       return $ sort $ map ("/"++) directoriesVerified
-- a `listTagDirectories` which includes all sub-directory/children as well:
listTagDirectoriesAll :: [FilePath] -> IO [FilePath]
listTagDirectoriesAll = listTagDirectories True

-- try to infer a long tag from a short tag, first by exact match, then by suffix, then by prefix, then by infix, then give up.
-- so eg. 'sr1' → 'SR1' → 'darknet-markets/silk-road/1', 'road/1' → 'darknet-markets/silk-road/1', 'darknet-markets/silk' → 'darknet-markets/silk-road', 'silk-road' → 'darknet-markets/silk-road'
guessTagFromShort :: [String] -> String -> String
guessTagFromShort _ "" = ""
guessTagFromShort l s = fixedPoint (f l) (replace "=" "-" s)
 where f m t = let allTags = nubOrd $ sort m in
                 if t `elem` allTags then t else -- exact match, no guessing required
                 case lookup t tagsShort2Long of
                   Just tl -> tl -- is an existing short/petname
                   Nothing -> let shortFallbacks =
                                    (map (\a->(a,"")) $ filter (\tag -> ("/"++ t) `isSuffixOf` tag) allTags) ++
                                    (map (\a->(a,"")) $ filter (\tag -> ("/"++ t++"/") `isInfixOf` tag) allTags) ++ -- look for matches by path segment eg. 'transformer' → 'ai/nn/transformer' (but not 'ai/nn/transformer/alphafold' or 'ai/nn/transformer/gpt')
                                    (map (\a->(a,"")) $ filter (\tag -> ("/"++t) `isSuffixOf` tag || (t++"/") `isInfixOf` tag) allTags) ++ -- look for matches by partial path segment eg. 'bias' → ' psychology/cognitive-bias/illusion-of-depth'
                                    filter (\(short,_) -> t `isSuffixOf` short) C.tagsShort2Long ++
                                    filter (\(short,_) -> t `isPrefixOf` short) C.tagsShort2Long ++
                                    filter (\(short,_) -> t `isInfixOf` short) C.tagsShort2Long
                              in if not (null shortFallbacks) then fst $ head shortFallbacks else
                                   let longFallbacks = filter (t `isSuffixOf`) allTags ++ filter (t `isPrefixOf`) allTags ++ filter (t `isInfixOf`) allTags in
                                     if not (null longFallbacks) then head longFallbacks else t

testTags :: IO ()
testTags = do
              tags <- listTagsAll
              let results = shortTagTest tags
              unless (null results) $ error ("Tags.hs: test suite errored out with some rewrites going awry; results: " ++ show results)
              let results' = isCycleLess tagsShort2LongRewrites
              unless (null results) $ error ("Tags.hs: test suite errored out with cycles detected in `tagsShort2Long`." ++ show results')

shortTagTest ::[String] -> [(String, String, String)]
shortTagTest alltags = filter (\(_, realOutput, shouldbeOutput) -> realOutput /= shouldbeOutput) $
  map (\(input,output) -> (input, guessTagFromShort alltags input, output)) (C.shortTagTestSuite ++ selfTagTestSuite)
  where selfTagTestSuite :: [(String,String)] -- every long tag should rewrite to itself, of course
        selfTagTestSuite = zip alltags alltags
