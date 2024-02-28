{- Gtx.hs: custom document format for Gwern.net, for lightweight writing of annotations.

Author: Gwern Branwen
Date: 2024-02-28
When:  Time-stamp: "2024-02-28 16:57:03 gwern"
License: CC-0

A 'gtx' (short for 'Gwern text' until I come up with a better name) text file is a UTF-8 text file
with the extension '.gtx'. It is designed for writing Gwern.net 'annotations': document excerpts/summaries
used for the popups. (It replaces earlier solutions using Haskell Read/Show tuple formatted data files & YAML.)
It avoids named fields, quoting/escaping, or error-prone indentation/nesting/hierarchy, to make it as easy to hand-write entries as possible,
and allow easily appending new entries.

A gtx is a newline-delimited format of records delimited by '---\n' separators. A record contains the following lines:

0. initial '---\n' separator (mandatory)
1. a naked URL (valid URI, text); this is the only mandatory field, all others can be left blank.
2. a title (UTF-8 HTML string)
3. comma-separated list of authors (UTF-8 HTML string)
4. date (YYYY[-MM[-DD]] digit-hyphen ASCII)
5. DOI (UTF-8 text, no white-space, must contain one '/' forward slash)

    TODO: replace the DOI field with a 'key-value dictionary' field which will store miscellaneous metadata in a Haskell `[(Text, Text)]` list
6. tags (space-separated alpha-numerical strings, which *must* correspond to on-disk directories in `doc/*`); parses into a list of strings
7. an 'abstract': an 'abstract' is all HTML (TODO: permit Markdown as well, as defined by whether the abstract begins with '<' or not) text after the tags and until the next '---\n' separator; it, and no other field, may contain arbitrarily many lines.

Example gtx entries:

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
Problems with YAML: indentation was easy to get wrong; the YAML writer alternates seemingly at random between single & double-quotes, so editing old annotations (eg. to add a link) would unpredictably break them; the *lack* of quotes also caused the same problem, when editing added a colon character & broke it; the YAML writer forcibly wraps lines at a rather short line length, which breaks many searches (which would work if it only line-broke at a more logical place like the end of a block element like `</p>`); it was easy to omit a field (because if I had required them to be labeled as key-value pairs, that would have meant a lot more typing/repetition); it was impossible to think about having Markdown entries (which would be ideal for writing convenience) because it would have to be wrapped in quotes and whitespace escaped and rendered un-editable natively since they are then hard to read & you might indentation...
-}

{-# LANGUAGE OverloadedStrings #-}
module Gtx where

import Data.Char (isPunctuation, toLower, isSpace, isNumber)
import Control.Monad
import Data.List
import qualified Data.Text.IO as TIO
import qualified Data.Text    as T
import System.Directory (doesFileExist)

import System.GlobalLock as GL (lock)

import Config.Misc as C (root)
import LinkMetadataTypes
import Tags (uniqTags, guessTagFromShort, tag2TagsWithDefault, guessTagFromShort, tag2Default, pages2Tags, listTagsAll, tagsToLinksSpan)
import MetadataFormat (cleanAuthors, guessDateFromLocalSchema)
import Utils

readGtx :: ((FilePath, MetadataItem) -> (FilePath, MetadataItem)) -> FilePath -> IO MetadataList
readGtx hook f = do f' <- do filep <- doesFileExist f
                             if filep then return f
                               else do fileAbsoluteP <- doesFileExist (C.root ++ f)
                                       if not fileAbsoluteP then printRed ("Gtx path does not exist: " ++ f ++ "; refusing to continue. Create an empty or otherwise initialize the file to retry.") >> return f
                                         else return (C.root ++ f)
                    content <- TIO.readFile f'
                    return $ parseGtx content

readGtxFast :: FilePath -> IO MetadataList
readGtxFast = readGtx id

readGtxSlow :: FilePath -> IO MetadataList
readGtxSlow path = do allTags <- listTagsAll
                      readGtx (postprocessing allTags) path
     where -- postprocessing :: ((FilePath, MetadataItem) -> (FilePath, MetadataItem))
           postprocessing allTags' (u, (t, a, d, di, ts, s)) = (stripUnicodeWhitespace u,
                                                     (reformatTitle t, cleanAuthors a,guessDateFromLocalSchema u d, di,
                                                      map (guessTagFromShort allTags') $ uniqTags $ pages2Tags u $ tag2TagsWithDefault u (intercalate " " ts), s))
           stripUnicodeWhitespace, reformatTitle :: String -> String
           stripUnicodeWhitespace = replace "⁄" "/" . filter (not . isSpace)
           reformatTitle = sed "“(.*)”" "‘\\1’"-- we avoid double-quotes in titles because they are usually being substituted into double-quote wrappers blindly, so you wind up with problems like `““Foo” Bar Baz”`. We do not substitute anything but double-curly quotes, because there are way too many edge-cases and other ways to use quotes (eg. citation HTML fragments in titles).

parseGtx :: T.Text -> MetadataList
parseGtx content = let subContent = tail $ T.splitOn "---\n" content -- split at the '---' separators into sublists of "title\nauthor\ndate\ndoi\ntags\nabstract..."; drop the first empty one
                       sublists = map T.lines subContent
                       sublists' = map tupleize sublists
                   in sublists'

tupleize :: [T.Text] -> (Path, MetadataItem)
tupleize (f:t:a:d:doi:tags:abstract) = (T.unpack f,
                                        (T.unpack t, T.unpack a, T.unpack d, T.unpack doi, map T.unpack $ T.words tags, if abstract==[""] then "" else T.unpack $ T.unlines abstract))
tupleize [] = error "tuplize: empty list"
tupleize x  = error $ "tuplize: missing mandatory list entries: " ++ show x

writeGtx :: FilePath -> MetadataList -> IO ()
writeGtx f ml = do let lists = concatMap untupleize ml
                   void $ GL.lock $ TIO.writeFile f $ T.unlines lists

untupleize :: (Path, MetadataItem) -> [T.Text]
untupleize (f, (t, a, d, doi, tags, abstract)) = map T.pack ["---"
                                                , f
                                                , t, a, d, doi
                                                , intercalate " " tags
                                                , abstract
                                                  ]
