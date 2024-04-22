{-# LANGUAGE OverloadedStrings #-}
module XOfTheDay where

-- A simple 3-tuple database for storing text snippets which can be transcluded (with usage variable). The initial use of this was for 'quote of the day' snippet generation.
--
-- 'Quote of the day' is an old website feature where, for visitors' edification or amusement, a random quote from a list of quotes would be display, often in the website footer or homepage.
-- An example is <https://en.wikiquote.org/wiki/Wikiquote:Quote_of_the_day> which is transcluded in the middle of <https://en.wikiquote.org/wiki/Main_Page>, or <https://web.archive.org/web/20150410044208/http://bbs.stardestroyer.net/SiteBanner.php?display=history>.
-- A common source of quotes used to be <https://en.wikipedia.org/wiki/Fortune_(Unix)>; see also <https://www.lesswrong.com/tag/rationality-quotes>.
--
-- A QOTD database is a `[Quote]` Haskell file, read with read/show. A `Quote` is a 3-tuple `(String, String, Bool)`: HTML "quote", HTML "attribution [or other commentary/metadata]", and whether it has been "used yet" (not entirely necessary, since one could sample randomly, but tracked to minimize reuse of quotes).† When all quotes have been used and are `True`, they get reset to `False` and the cycle begins again (with, presumably, new quotes added since the last time).
-- Quotes & attributions do not contain double-quote delimiters or other HTML wrapping; that will be added when they are formatted as Gwern.net-style 'epigraphs' to be written.
--
-- The QOTD db is used by read in the QOTDB, selecting the first unused quote, marking it used & writing out the updated db, and then writing out the quote to a particular file in a HTML format; that file is used by downstream users such as Hakyll websites which template or transclude it.
-- With the use of `transclude.js`, this can be as simple as:
--
-- `<div class="qotd"><a class="include" href="/metadata/today-quote.html">Quote Of The Day</a></div>`
--
-- † The probability that a daily visitor would see a duplicate quote under simple random sampling grows rapidly with time; see <https://en.wikipedia.org/wiki/Birthday_problem>. If eg. there were 366 quotes, then after only 23 visits, the reader would have a 50-50 chance of seeing ≥1 duplicate!
--
-- This module supports 2 more 'X of the day' features, with analogous functionality: 'annotation of the day', which goes through the annotated links by length; and 'site of the day', which is similar to a 'quote' but just is a single hyperlink to a website.

import Control.Monad (unless, when)
import qualified Data.Set as S (delete, empty, filter, fromList, toList, insert, map)
import System.Directory (doesFileExist)
import Text.Show.Pretty (ppShow)
import qualified Data.Text as T (isInfixOf, pack, unpack, Text)
import qualified Data.Map.Strict as M (toList, filterWithKey, map, fromListWith)
import Data.List (isSuffixOf, sortOn, sort)
import Text.Pandoc (Inline(Link,Str), runPure, writeHtml5String, Pandoc(..), nullMeta, Block(Div,Para))
import System.IO.Unsafe as Unsafe (unsafePerformIO)

import LinkMetadataTypes (Metadata)
import LinkMetadata (typesetHtmlField)
import LinkBacklink (readBacklinksDB)
import Utils (host, anyInfixT, safeHtmlWriterOptions)
import LinkArchive (readArchiveMetadata, localizeLink, ArchiveMetadata)
import LinkIcon (linkIcon)
import qualified Config.XOfTheDay as C (siteLinkMin, siteBlackList, minAnnotationAbstractLength, quoteDBPath, quotePath, siteDBPath, sitePath, annotDayDB, annotPath)

type TTDB = [Snippet]
type Snippet = (String, String, Bool)

quoted :: Snippet -> String
quoted (quote,attribution,_) = "<div class=\"epigraph quote-of-the-day\">\n<blockquote><p>" ++ typesetHtmlField quote ++ "</p>" ++
                               (if null attribution then "" else "\n<p>" ++ typesetHtmlField attribution ++ "</p>") ++
                               "</blockquote>\n</div>"

sited :: Snippet -> String
sited (site,name,_) = "<div class=\"site-of-the-day\">\n<blockquote><p><a href=\"" ++ site ++ "\">" ++ typesetHtmlField name ++ "</a></p></blockquote>\n</div>"

readTTDB :: FilePath -> IO TTDB
readTTDB path = do exists <- doesFileExist path
                   if not exists then return [] else fmap read $ readFile path

writeTTDB :: FilePath -> TTDB -> IO ()
writeTTDB path = writeFile path . ppShow

writeSnippet :: FilePath -> (Snippet -> String) -> Snippet -> IO ()
writeSnippet path formatter x = writeFile path (formatter x)

generateSnippetAndWriteTTDB :: FilePath -> FilePath -> (Snippet -> String) -> IO ()
generateSnippetAndWriteTTDB dbpath path formatter =
  do dblist <- readTTDB dbpath
     when (null dblist) $ error $ "Fatal error: tuple database " ++ path ++ " is empty?"
     unless (not $ any (\(q,_,_) -> null q) dblist) $ error $ "Fatal error: tuple database has empty first-fields? " ++ show dblist
     let db = S.fromList dblist

     -- get set of usable items, and if there are none, reset the entire set and use that:
     let dbUnused = S.filter (\(_,_,status) -> not status) db
     let dbReset = if dbUnused /= S.empty then db else S.map snegate db
     let dbUnused' = S.filter (\(_,_,status) -> not status) dbReset

     let snippet = head $ sortOn (\(q, _, _) -> length q) $ S.toList dbUnused' -- take the smallest quote, for symmetry with the annotation being largest (for sites, won't matter how it's sorted, really)
     writeSnippet path formatter snippet

     let db'' = S.insert (snegate snippet) $ S.delete snippet dbReset -- update the now-used item
     writeTTDB dbpath $ S.toList db''

 where snegate :: Snippet -> Snippet
       snegate (a,b,s) = (a,b,not s)

qotd, sotd :: IO ()
qotd    = generateSnippetAndWriteTTDB   C.quoteDBPath C.quotePath quoted
sotd    = generateSnippetAndWriteTTDB   C.siteDBPath  C.sitePath  sited

-------

aotd :: Metadata -> IO ()
aotd md = do am <- readArchiveMetadata
             generateAnnotationOfTheDay md C.annotDayDB C.annotPath (annotated am)

-- same idea: each build, we pick an annotation which hasn't been shown before (uses are tracked in a simple Haskell DB), currently picking by what is the 'longest annotation' (as measured by raw string length) as a crude proxy for 'best', and—tag-directory style—write an `{.annotation-include-partial}` snippet for transcluding into the footer of each page after the quote-of-the-day.

type AotD = [String]

-- it is important to run the archive pass on the annotation link for cases like Arxiv. Although this is quite ugly...
annotated :: ArchiveMetadata -> String -> String
annotated a url = Unsafe.unsafePerformIO $ do
  lnk <- localizeLink a $ linkIcon $ Link ("", ["include-annotation-partial", "link-annotated", "backlink-not", "include-spinner-not"], [("include-template", "annotation-blockquote-outside")]) [Str "Annotation Of The Day"] (T.pack url,"")
  let htmlE = runPure $ writeHtml5String safeHtmlWriterOptions $
        Pandoc nullMeta [Div ("", ["annotation-of-the-day"], []) [Para [lnk]]]
  case htmlE of
    Left err   -> error ("XOfTheDay.hs: annotated: failed to properly Pandoc-parse today's annotation-of-the-day? error:" ++ show err ++ " : " ++ show url)
    Right html -> return $ T.unpack html

readAnnotDayDB :: FilePath -> IO AotD
readAnnotDayDB path = do exists <- doesFileExist path
                         if not exists then return [] else fmap read $ readFile path
writeAnnotDayDB :: FilePath -> AotD -> IO ()
writeAnnotDayDB path = writeFile path . ppShow

generateAnnotationOfTheDay :: Metadata -> FilePath -> FilePath -> (String -> String) -> IO ()
generateAnnotationOfTheDay md dbpath annotpath formatter =
  do db <- readAnnotDayDB dbpath
     let md' = M.toList $ M.filterWithKey (\k (_,author,_,_,_,_,abstract1) ->
                                              length abstract1 > C.minAnnotationAbstractLength &&
                                              author /= "Gwern Branwen" && author /= "gwern" && author /= "Gwern" &&
                                              k `notElem` db &&
                                              not ("/index" `isSuffixOf` k)) md
     let lengthList = sortOn (\(_, (_,_,_,_,_,_,abstract2)) -> length abstract2) md' -- ascending order (ie. largest last)
     if null lengthList then writeFile [] dbpath else
       do let (url,_) = last lengthList -- grab the largest
          let db' = db ++ [url]
          writeFile annotpath (formatter url)
          writeAnnotDayDB dbpath db'

-----------
--- site-of-the-day prioritizing

-- to find URLs worth considering for sotd use, , pass through a list of URLs (perhaps extracted
-- from the backlinks database) and return domains with at least `siteLinkMin` matches.
--
-- The original raw results are particularly useful when piped into <https://gwern.net/haskell/lcp.hs> to
-- get suggested prefixes/domains, or one can just look at the domains by `host`:
sitePrioritize :: IO [T.Text]
sitePrioritize = do sotdb <- readTTDB C.siteDBPath
                    let sotdbl = map (\(u,_,_) -> T.pack u) sotdb
                    b <- LinkBacklink.readBacklinksDB
                    let b' = M.toList $ M.map length b
                    let b'' = map (\(y,z) -> (host y,z)) $ filter (\(url,_) ->  host url `notElem` C.siteBlackList &&
                                                                                not (anyInfixT url sotdbl) &&
                                                                                ("." `T.isInfixOf` url)) b'
                    let b''' =  M.fromListWith (+) b''
                    return $ map snd $ reverse $ sort $ filter (\(e,f) -> e >= C.siteLinkMin && f /="") $ map (\(c,d) -> (d,c)) $ M.toList b'''
