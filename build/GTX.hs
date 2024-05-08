{- GTX: custom document format for Gwern.net, for lightweight writing of annotations.

Author: Gwern Branwen
Date: 2024-02-28
When:  Time-stamp: "2024-05-07 11:26:47 gwern"
License: CC-0

A 'GTX' (short for 'Gwern text' until I come up with a better name) text file is a UTF-8 text file
with the extension '.gtx'. It is designed for writing Gwern.net 'annotations': document excerpts/summaries
used for the popups. (It replaces earlier solutions using Haskell Read/Show tuple formatted data files & YAML.)
It avoids named fields, quoting/escaping, or error-prone indentation/nesting/hierarchy, to make it as easy to hand-write entries as possible,
and allow easily appending new entries.

A GTX is a newline-delimited format of records delimited by '---\n' separators. A record contains the following lines:

0. initial '---\n' separator (mandatory)
1. a naked URL (valid URI, text); this is the only mandatory field, all others can be left blank.
2. a title (UTF-8 HTML string)
3. comma-separated list of authors (UTF-8 HTML string)
4. date (YYYY[-MM[-DD]] digit-hyphen ASCII)
5. a 'naked DOI' or a Haskell key-value dictionary

    A line for key-value dictionaries (association-lists), for storing miscellaneous information. The most common case is a DOI global identifier. This line must parse by GHC Haskell as a `[(String, String)]` list. It is written out as a blank line for empty lists `[]`, and as a sorted, unique, key-value association list otherwise.

    A naked DOI must be: UTF-8 text, no white-space, must contain one '/' forward slash. It must not start with a LEFT SQUARE BRACKET character (which may be technically allowed by the DOI standard), or it will be misparsed as a K-V. Naked DOIs can be read, but will be written out as a key-value list. The purpose of this is to allow convenient writing, without having to generate all of the wrapper like `[("doi", "...")]`; it is then converted to the canonical list at some point later to enable easier editing, like inserting an additional entry.
6. tags (space-separated alpha-numerical strings, which *must* correspond to on-disk directories in `doc/*`); parses into a list of strings. (Tags are semi-mandatory on Gwern.net: ideally every URL would have at least one tag.)
7. an 'abstract': an 'abstract' is all HTML (TODO: permit Markdown as well, as defined by whether the abstract begins with '<' or not) text after the tags and until the next '---\n' separator; it, and no other field, may contain arbitrarily many lines.

An example GTX file with 3 entries of increasing completeness:

```
---
https://jackcook.com/2024/02/23/mamba.html




ai/nn/rnn

---
http://www.demarcken.org/carl/papers/ITA-software-travel-complexity/ITA-software-travel-complexity.pdf
Computational Complexity of Air Travel Planning
Carl de Marcken
2003

cs/computable

---
https://arxiv.org/abs/1003.0358#schmidhuber
Deep Big Simple Neural Nets Excel on Handwritten Digit Recognition
Dan Claudiu Ciresan, Ueli Meier, Luca Maria Gambardella, Juergen Schmidhuber
2010-03-01
10.1162/NECO_a_00052
ai/nn/fully-connected ai/scaling/hardware
<p>Good old on-line back-propagation for plain multi-layer perceptrons yields
a very low 0.35% error rate on the famous <a href="https://en.wikipedia.org/wiki/MNIST_database">MNIST</a>
handwritten digits benchmark.</p> <p>All we need to achieve this best result so
far are many hidden layers, many neurons per layer, numerous deformed training
images, and [Nvidia] graphics cards to greatly speed up learning.</p
```

Note that this format is intended to be extremely inflexible and tailored to the exact use case of writing annotations for Gwern.net, which avoids any need to care about newlines, quote marks, colons, indentation depth lining up, complex parsing etc, that were constant papercuts in writing annotations in more powerful formats like YAML.
Problems with YAML: indentation was easy to get wrong; the YAML writer alternates seemingly at random between single & double-quotes, so editing old annotations (eg. to add a link) would unpredictably break them; the *lack* of quotes also caused the same problem, when editing added a colon character & broke it; the YAML writer forcibly wraps lines at a rather short line length, which breaks many searches (which would work if it only line-broke at a more logical place like the end of a block element like `</p>`); dates could be frustrating to write because while `2000-01-01` would parse as a string, `2000` would *not* and had to be quoted as `"2000"` to ensure it wasn’t turned into an integer; it was easy to omit a field (because if I had required them to be labeled as key-value pairs, that would have meant a lot more typing/repetition); it was impossible to think about having Markdown entries (which would be ideal for writing convenience) because it would have to be wrapped in quotes and whitespace escaped and rendered un-editable natively since they are then hard to read & you might indentation...
The previous format, Haskell, lacked many of YAML’s "helpful" shortcuts and was highly regular; but unfortunately, it still requires whitespace & quotes to be escaped, was slow to parse/write, and the usual encoding, as a list, meant that it could not be appended to (because one would have to move the closing-bracket).
One alternative I didn’t explore too thoroughly was the idea of writing each annotation as a Markdown section; this might have worked but would have required somewhat unnatural formatting like requiring newlines between each field (so they could be unambiguously parsed as separate `Para` AST nodes) or ordered/unordered lists. This would have worked poorly with the increasingly-complicated HTML inside many annotations.
-}

{-# LANGUAGE OverloadedStrings #-}
module GTX where

import Data.Char (isSpace)
import Data.Containers.ListUtils (nubOrd)
import Control.Monad (void)
import qualified Data.Map.Strict as M (fromList, toList)
import qualified Data.Text.IO as TIO (appendFile, readFile)
import qualified Data.Text    as T (drop, lines, splitOn, pack, strip, unlines, unpack, words, Text)
import System.Directory (doesFileExist)

import Text.Show.Pretty (ppShow)
import System.GlobalLock as GL (lock) -- global-lock

import Config.Misc as C (cd, root, todayDayString, yesterdayDayString, lateNight)
import LinkMetadataTypes (Metadata, MetadataList, MetadataItem, Path)
import Tags (listTagsAll, guessTagFromShort, uniqTags, pages2Tags, tag2TagsWithDefault, tag2Default)
import MetadataAuthor (authorsCanonicalize)
import MetadataFormat (cleanAuthors, guessDateFromLocalSchema)
import Utils (sed, printGreen, printRed, replace, writeUpdatedFile)

readGTX :: ((FilePath, MetadataItem) -> (FilePath, MetadataItem)) -> FilePath -> IO MetadataList
readGTX hook f = do f' <- do filep <- doesFileExist f
                             if filep then return f
                               else do fileAbsoluteP <- doesFileExist (C.root ++ f)
                                       if not fileAbsoluteP then printRed ("GTX path does not exist: " ++ f ++ "; refusing to continue. Create an empty or otherwise initialize the file to retry.") >> return f
                                         else return (C.root ++ f)
                    content <- TIO.readFile f'
                    return $ map hook $ parseGTX content

readGTXFast :: FilePath -> IO MetadataList
readGTXFast = readGTX id

readGTXSlow :: FilePath -> IO MetadataList
readGTXSlow path = do C.cd
                      allTags <- listTagsAll
                      results <- readGTX (postprocessing allTags) path
                      let badEntries = filter (\(p,_) -> p `elem` ["---", "---.md", ""]) results
                      if null badEntries then return results else error ("readGTXSlow: invalid entries found in " ++ path ++ ": " ++ show badEntries)
     where postprocessing :: [FilePath] -> ((FilePath, MetadataItem) -> (FilePath, MetadataItem))
           postprocessing allTags' (u, (t, a, d, dc, kvs, ts, s)) = (stripUnicodeWhitespace u,
                                                     (reformatTitle t, cleanAuthors a,guessDateFromLocalSchema u d, dc, kvs,
                                                      map (guessTagFromShort allTags') $ uniqTags $ pages2Tags u $ tag2TagsWithDefault u (unwords ts), s))
           stripUnicodeWhitespace, reformatTitle :: String -> String
           stripUnicodeWhitespace = replace "⁄" "/" . filter (not . isSpace)
           reformatTitle = sed "“(.*)”" "‘\\1’"-- we avoid double-quotes in titles because they are usually being substituted into double-quote wrappers blindly, so you wind up with problems like `““Foo” Bar Baz”`. We do not substitute anything but double-curly quotes, because there are way too many edge-cases and other ways to use quotes (eg. citation HTML fragments in titles).

parseGTX :: T.Text -> MetadataList
parseGTX content = let subContent = T.splitOn "\n---\n" $ T.drop 4 content -- delete the first 4 characters, which are the mandatory '---\n' header, then split at the '---' separators into sublists of "title\nauthor\ndate\ndoi\ntags\nabstract..."
                       sublists   = map T.lines subContent
                       sublists'  = map tupleize sublists
                   in filter (\(f,_) -> f /= "---") sublists' -- guard against off-by-one & misparsing

tupleize :: [T.Text] -> (Path, MetadataItem)
tupleize x@(f:t:a:d:dc:kvs:tags:abstract) = (T.unpack f,
                                        (T.unpack t, T.unpack a, T.unpack d, T.unpack dc, doiOrKV x $ T.unpack kvs, map T.unpack $ T.words tags, if abstract==[""] then "" else T.unpack $ T.unlines abstract))
tupleize [] = error   "tuplize: empty list"
tupleize x  = error $ "tuplize: missing mandatory list entries: " ++ show x

writeGTX :: FilePath -> MetadataList -> IO ()
writeGTX f ml = do today <- todayDayString -- 'writeGTX' is usually used interactively, so missing-dates are going to be 'today'
                   let lists = concatMap (untupleize today) ml
                   void $ GL.lock $ writeUpdatedFile "gtx" f $ T.unlines lists

untupleize :: String -> (Path, MetadataItem) -> [T.Text]
untupleize today (f, (t, aut, d, dc, kvs, tags, abstract)) =
  map (T.strip . T.pack) ["---"
                         , f
                         , t
                         , authorsCanonicalize aut
                         , d
                         , if null dc then today else dc
                         , if null kvs then "" else show $ nubOrd kvs
                         , unwords tags
                         , abstract
                         ]

doiOrKV :: [T.Text] -> String -> [(String,String)]
doiOrKV mi s | s == ""       = []
             | s == "[]"     = []
             | head s == '[' = read s
             | '/' `elem` s  = [("doi",s)]
             | otherwise     = error $ "doiOrKV parsing: " ++ s ++ " : " ++ show mi

-- clean a YAML metadata file by sorting & unique-ing it (this cleans up the various appends or duplicates):
rewriteLinkMetadata :: MetadataList -> MetadataList -> Path -> IO ()
rewriteLinkMetadata half full gtx
  = do old <- readGTXFast gtx
       -- de-duplicate by removing anything in auto.gtx which has been promoted to full/half:
       let (halfURLs,fullURLs) = (map fst half, map fst full)
       let betterURLs = nubOrd (halfURLs ++ fullURLs) -- these *should* not have any duplicates, but...
       let old' = filter (\(p,_) -> p `notElem` betterURLs) old
       let new = M.fromList old' :: Metadata -- NOTE: constructing a Map data structure automatically sorts/dedupes
       let newGTX = map (\(a,(b,c,d,dc,kvs,ts,f)) -> let defTag = tag2Default a in (a,(b,c,d,dc,kvs, filter (/=defTag) ts, f))) $ -- flatten [(Path, (String, String, String, String, String))]
                     M.toList new
       writeGTX gtx newGTX

-- append (rather than rewrite entirely) a new automatic annotation if its Path is not already in the auto-annotation database:
appendLinkMetadata :: Path -> MetadataItem -> IO ()
appendLinkMetadata l i@(t,a,d,dc,kvs,ts,abst) = do printGreen (l ++ " : " ++ ppShow i)
                                                   overnight <- lateNight
                                                   -- in comparison to `writeGTX`, entries generated using `appendLinkMetadata` have usually been found by compiling the site, which means, given slowness of compilation, they may well have been added the previous day, or day before that;
                                                   -- if we are running after midnight, assume all new links are from the *previous* day:
                                                   today <- if overnight then yesterdayDayString else todayDayString

                                                   -- GTX removes as much delimiting as possible for easier editing/generation. This lack of explicit closing becomes a problem in one case: hand-editing 'auto.gtx' if the final entry lacks a tag and our text editor removes all but the final blank line (which then truncates the GTX entry). We solve this by simply tagging the last entry by hand, so there is exactly one trailing newline but that's the 'abstract' line and so it's fine.
                                                   let newGTX = T.unlines $ untupleize today (l, (t,a,d,dc,kvs,ts,abst))
                                                   void $ GL.lock $ TIO.appendFile "metadata/auto.gtx" newGTX
