{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Control.Monad (when)
import Data.Char (isSpace)
import Data.Graph (flattenSCC, stronglyConnComp)
import Data.List (group, intercalate, intersperse, sort, isInfixOf, isPrefixOf, isSuffixOf, tails, nub, foldl')
import Data.Text.IO as TIO (readFile, writeFile)
import Data.Time.Calendar (toGregorian, toModifiedJulianDay)
import Data.Time.Clock (getCurrentTime, utctDay)
import Network.URI (parseURIReference, uriAuthority, uriPath, uriRegName)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (takeDirectory, takeExtension)
import System.IO (stderr, hPutStr)
import System.IO.Temp (emptySystemTempFile)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Pretty (ppShow)
import qualified Data.Text as T (Text, concat, pack, unpack, isInfixOf, isPrefixOf, isSuffixOf, replace, head, append)
import System.Exit (ExitCode(ExitFailure))
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import Data.FileStore.Utils (runShellCommand)
import qualified Data.Set as Set

import Numeric (showFFloat)

import Text.Regex (subRegex, mkRegex)
import Text.Regex.TDFA ((=~))

import Text.Pandoc (def, nullAttr, nullMeta, runPure,
                    writerColumns, writePlain, Block(Div, RawBlock), Pandoc(Pandoc), Inline(..), MathType(InlineMath), Block(Para), readerExtensions, writerExtensions, readHtml, writeMarkdown, pandocExtensions, WriterOptions, Extension(Ext_shortcut_reference_links), enableExtension, Attr, Format(..), topDown)
import Text.Pandoc.Walk (walk)

-- Auto-update the current year.
currentYear :: Int
currentYear = unsafePerformIO $ fmap ((\(year,_,_) -> fromInteger year) . toGregorian . utctDay) Data.Time.Clock.getCurrentTime -- 2024

currentDay :: IO Integer
currentDay = fmap (toModifiedJulianDay . utctDay) Data.Time.Clock.getCurrentTime

-- Write only when changed, to reduce sync overhead; creates parent directories as necessary; writes
-- to a temp file in /tmp/ (at a specified template name), and does an atomic rename to the final
-- file.
writeUpdatedFile :: String -> FilePath -> T.Text -> IO ()
writeUpdatedFile template target contentsNew =
  do existsOld <- doesFileExist target
     if not existsOld then do
       createDirectoryIfMissing True (takeDirectory target)
       TIO.writeFile target contentsNew
       else do contentsOld <- TIO.readFile target
               when (contentsNew /= contentsOld) $ do tempPath <- emptySystemTempFile ("hakyll-"++template)
                                                      TIO.writeFile tempPath contentsNew
                                                      renameFile tempPath target

trim :: String -> String
trim = reverse . dropWhile badChars . reverse . dropWhile badChars -- . filter (/='\n')
  where badChars c = isSpace c || (c=='-')

simplifiedString :: String -> String
simplifiedString s = trim $ -- NOTE: 'simplified' will return a trailing newline, which is unhelpful when rendering titles.
                     T.unpack $ simplified $ Para [Str $ T.pack s]

simplified :: Block -> T.Text
simplified i = simplifiedDoc (Pandoc nullMeta [i])

simplifiedDoc :: Pandoc -> T.Text
simplifiedDoc p = let md = runPure $ writePlain def{writerColumns=100000} p in -- NOTE: it is important to make columns ultra-wide to avoid formatting-newlines being inserted to break up lines mid-phrase, which would defeat matches in LinkAuto.hs.
                         case md of
                           Left _ -> error $ "Failed to render: " ++ show md
                           Right md' -> md'

toMarkdown :: String -> String
toMarkdown abst = let clean = runPure $ do
                                   pandoc <- readHtml def{readerExtensions=pandocExtensions} (T.pack abst)
                                   md <- writeMarkdown def{writerExtensions = pandocExtensions, writerColumns=100000} pandoc
                                   return $ T.unpack md
                             in case clean of
                                  Left e -> error $ ppShow e ++ ": " ++ abst
                                  Right output -> output

toPandoc :: String -> Pandoc
toPandoc abst = let clean = runPure $ readHtml def{readerExtensions=pandocExtensions} $ T.pack abst
                in case clean of
                     Left e -> error $ ppShow e ++ ": " ++ abst
                     Right output -> output

parseRawAllClean :: Pandoc -> Pandoc
parseRawAllClean = topDown cleanUpDivsEmpty .
                   walk cleanUpSpans .
                   -- walk (parseRawInline nullAttr) .
                   walk (parseRawBlock nullAttr)

parseRawBlock :: Attr -> Block -> Block
parseRawBlock attr x@(RawBlock (Format "html") h) = let pandoc = runPure $ readHtml def{readerExtensions = pandocExtensions} h in
                                          case pandoc of
                                            Left e -> error (show x ++ " : " ++ show e)
                                            Right (Pandoc _ blocks) -> Div attr blocks
parseRawBlock _ x = x
-- WARNING: appears to break some instances of inline HTML, especially subsup instances. I was unable to debug why.
-- parseRawInline :: Attr -> Inline -> Inline
-- parseRawInline attr x@(RawInline (Format "html") h) = let pandoc = runPure $ readHtml def{readerExtensions = pandocExtensions} h in
--                                           case pandoc of
--                                             Left e -> error (show x ++ " : " ++ show e)
--                                             Right (Pandoc _ [Para inlines]) -> Span attr inlines
--                                             Right (Pandoc _ [Plain inlines]) -> Span attr inlines
--                                             Right (Pandoc _ inlines) -> Span attr (extractAndFlattenInlines inlines)
-- parseRawInline _ x = x
-- extractAndFlattenInlines :: [Block] -> [Inline]
-- extractAndFlattenInlines [RawBlock (Format "html") x]  = [RawInline (Format "html") x]
-- extractAndFlattenInlines x = error ("extractAndFlattenInlines: hit a RawBlock which couldn't be parsed? : " ++ show x)

-- we probably want to remove the link-auto-skipped Spans if we are not actively debugging, because they inflate the markup & browser DOM.
-- We can't just remove the Span using a 'Inline -> Inline' walk, because a Span is an Inline with an [Inline] payload, so if we just remove the Span wrapper, it is a type error: we've actually done 'Inline -> [Inline]'.
-- Block elements always have [Inline] (or [[Inline]]) and not Inline arguments if they have Inline at all; likewise, Inline element also have only [Inline] arguments.
-- So, every instance of a Span *must* be inside an [Inline]. Therefore, we can walk an [Inline], and remove the wrapper, and then before++payload++after :: [Inline] and it typechecks and doesn't change the shape.
--
-- > cleanUpSpans [Str "foo", Span ("",["link-auto-skipped"],[]) [Str "Bar", Emph [Str "Baz"]], Str "Quux"]
--                               [Str "foo",                                     Str "Bar", Emph [Str "Baz"],  Str "Quux"]
-- > walk cleanUpSpans $ Pandoc nullMeta [Para [Str "foo", Span ("",["link-auto-skipped"],[]) [Str "Bar", Emph [Str "Baz"]], Str "Quux"]]
-- Pandoc (Meta {unMeta = fromList []}) [Para [Str "foo",Str "Bar",Emph [Str "Baz"],Str "Quux"]]
--
-- NOTE: might need to generalize this to clean up other Span crud?
cleanUpSpans :: [Inline] -> [Inline]
cleanUpSpans [] = []
cleanUpSpans   (Span ("",[],[]) payload : rest)                             = payload ++ rest
cleanUpSpans x@(Span (_,[],_) _ : _)                                        = x
cleanUpSpans   (Span (_,["link-auto-skipped"],_) payload : rest)            = payload ++ rest
cleanUpSpans   (Span (_,["link-auto-first", "link-auto"],_) payload : rest) = payload ++ rest
cleanUpSpans   (Span (a,classes,b) c : rest) = let classes' = filter (\cl -> cl `notElem` ["link-auto","link-auto-first","link-auto-skipped"]) classes in
                                                                              Span (a,classes',b) c : rest
cleanUpSpans (x@Link{} : rest) =  removeClass "link-auto" x : cleanUpSpans rest
cleanUpSpans (r:rest) = r : cleanUpSpans rest

cleanUpDivsEmpty :: [Block] -> [Block]
cleanUpDivsEmpty [] = []
cleanUpDivsEmpty (Div ("",[],[]) payload : rest) = payload ++ rest
cleanUpDivsEmpty (r:rest) = r : cleanUpDivsEmpty rest -- if it is not a nullAttr, then it is important and carrying a class like "abstract" or something, and must be preserved.

simplifiedHTMLString :: String -> String
simplifiedHTMLString arg = trim $ T.unpack $ simplified $ parseRawBlock nullAttr (RawBlock (Text.Pandoc.Format "html") (T.pack arg))

-- Add or remove a class to a Link or Span; this is a null op if the class is already present or it
-- is not a Link/Span.
addClass :: T.Text -> Inline -> Inline
addClass clss x@(Code  (i, clsses, ks) code)        = if clss `elem` clsses then x else Code  (i, clss:clsses, ks) code
addClass clss x@(Image (i, clsses, ks) s (url, tt)) = if clss `elem` clsses then x else Image (i, clss:clsses, ks) s (url, tt)
addClass clss x@(Link  (i, clsses, ks) s (url, tt)) = if clss `elem` clsses then x else Link  (i, clss:clsses, ks) s (url, tt)
addClass clss x@(Span  (i, clsses, ks) s)           = if clss `elem` clsses then x else Span  (i, clss:clsses, ks) s
addClass _    x = x
removeClass :: T.Text -> Inline -> Inline
removeClass clss x@(Code  (i, clsses, ks) code)        = if clss `notElem` clsses then x else Code  (i, filter (/=clss) clsses, ks) code
removeClass clss x@(Image (i, clsses, ks) s (url, tt)) = if clss `notElem` clsses then x else Image (i, filter (/=clss) clsses, ks) s (url, tt)
removeClass clss x@(Link (i, clsses, ks) s (url, tt))  = if clss `notElem` clsses then x else Link  (i, filter (/=clss) clsses, ks) s (url, tt)
removeClass clss x@(Span (i, clsses, ks) s)            = if clss `notElem` clsses then x else Span  (i, filter (/=clss) clsses, ks) s
removeClass _    x = x

removeKey :: T.Text -> Inline -> Inline
removeKey key (Code  (i, cl, ks) code)        = Code  (i, cl, filter (\(k,_) -> k/=key) ks) code
removeKey key (Image (i, cl, ks) s (url, tt)) = Image (i, cl, filter (\(k,_) -> k/=key) ks) s (url, tt)
removeKey key (Link  (i, cl, ks) s (url, tt)) = Link  (i, cl, filter (\(k,_) -> k/=key) ks) s (url, tt)
removeKey key (Span  (i, cl, ks) s)           = Span  (i, cl, filter (\(k,_) -> k/=key) ks) s
removeKey _ x = x
addKey :: (T.Text,T.Text) -> Inline -> Inline
addKey key (Code  (i, cl, ks) code)        = Code  (i, cl, nub (key : ks)) code
addKey key (Image (i, cl, ks) s (url, tt)) = Image (i, cl, nub (key : ks)) s (url, tt)
addKey key (Link  (i, cl, ks) s (url, tt)) = Link  (i, cl, nub (key : ks)) s (url, tt)
addKey key (Span  (i, cl, ks) s)           = Span  (i, cl, nub (key : ks)) s
addKey _ x = x

-- enable printing of normal vs dangerous log messages to terminal stderr:
green, red :: String -> String
green s = "\x1b[32m" ++ s ++ "\x1b[0m"
red   s = "\x1b[41m" ++ s ++ "\x1b[0m"

-- print normal progress messages to stderr in bold green:
putStrGreen, printGreen :: String -> IO ()
putStrGreen s = putStrStdErr $ green s
printGreen  s = putStrGreen (s ++ "\n")

-- print danger or error messages to stderr in red background:
putStrRed, printRed :: String -> IO ()
putStrRed s = do when (length s > 2048) $ printRed "Warning: following error message was extremely long & truncated at 2048 characters!"
                 putStrStdErr $ red $ take 2048 s
printRed s = putStrRed (s ++ "\n")
-- special-case: the error message, then useful values:
printRed' :: String -> String -> IO ()
printRed' e l = putStrRed e >> printGreen l

putStrStdErr :: String -> IO ()
putStrStdErr = hPutStr stderr

-- Repeatedly apply `f` to an input until the input stops changing. Show constraint for better error
-- reporting on the occasional infinite loop.
-- Note: set to 5000 iterations by default. However, if you are using a list of _n_ simple rewrite rules, the limit can be set to _n_+1 rewrites
-- as any more than that implies a cycle/infinite-loop.
fixedPoint :: (Show a, Eq a) => (a -> a) -> a -> a
fixedPoint = fixedPoint' 5000
 where fixedPoint' :: (Show a, Eq a) => Int -> (a -> a) -> a -> a
       fixedPoint' 0 _ i = error $ "Hit recursion limit: still changing after 5,000 iterations! Infinite loop? Last result: " ++ show i
       fixedPoint' n f i = let i' = f i in if i' == i then i else fixedPoint' (n-1) f i'

sed :: String -> String -> String -> String
sed before after s = if before == after then error ("Fatal error in `sed`: before == after: \"" ++ before ++ "\"") else subRegex (mkRegex before) s after

-- list of regexp string rewrites
sedMany :: [(String,String)] -> (String -> String)
sedMany regexps s = foldr (uncurry sed) s regexps

-- list of fixed string rewrites
replaceMany :: [(String,String)] -> (String -> String)
replaceMany rewrites s = foldr (uncurry replace) s rewrites

replaceT :: T.Text -> T.Text -> T.Text -> T.Text
replaceT = T.replace

-- list of fixed string rewrites
replaceManyT :: [(T.Text,T.Text)] -> (T.Text -> T.Text)
replaceManyT rewrites s = foldr (uncurry replaceT) s rewrites

-- replace/split/hasKeyAL copied from https://hackage.haskell.org/package/MissingH-1.5.0.1/docs/src/Data.List.Utils.html to avoid MissingH's dependency of regex-compat
replace :: (Eq a, Show a) => [a] -> [a] -> [a] -> [a]
replace before after = if before == after then error ("Fatal error in `replace`: identical args (before == after): " ++ show before ++ "") else intercalate after . split before
split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (isPrefixOf delim) str
        in
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [[]]
                                        else split delim
                                                 (drop (length delim) x)
  where
    breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
    breakList func = spanList (not . func)
    spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
    spanList _ [] = ([],[])
    spanList func list@(x:xs) =
        if func list
           then (x:ys,zs)
           else ([],list)
        where (ys,zs) = spanList func xs
hasKeyAL :: Eq a => a -> [(a, b)] -> Bool
hasKeyAL key list = key `elem` map fst list

frequency :: Ord a => [a] -> [(Int,a)]
frequency list = sort $ map (\l -> (length l, head l)) (group (sort list))

pairs :: [b] -> [(b, b)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

host :: T.Text -> T.Text
host p = case parseURIReference (T.unpack p) of
              Nothing -> ""
              Just uri' -> case uriAuthority uri' of
                                Nothing -> ""
                                Just uridomain' -> T.pack $ uriRegName uridomain'

anyInfix, anyPrefix, anySuffix :: String -> [String] -> Bool
anyInfix p = any (`isInfixOf` p)
anyPrefix p = any (`isPrefixOf` p)
anySuffix p = any (`isSuffixOf` p)

anyInfixT, anyPrefixT, anySuffixT :: T.Text -> [T.Text] -> Bool
anyInfixT p = any (`T.isInfixOf` p)
anyPrefixT p = any (`T.isPrefixOf` p)
anySuffixT p = any (`T.isSuffixOf` p)

{- | Returns true if the given list contains any of the elements in the search
list. -}
hasAny :: Eq a => [a]           -- ^ List of elements to look for
       -> [a]                   -- ^ List to search
       -> Bool                  -- ^ Result
hasAny [] _          = False             -- An empty search list: always false
hasAny _ []          = False             -- An empty list to scan: always false
hasAny search (x:xs) = x `elem` search || hasAny search xs

-- HACK: this is a workaround for an edge-case: Pandoc reads complex tables as 'grid tables', which then, when written using the default writer options, will break elements arbitrarily at newlines (breaking links in particular). We set the column width *so* wide that it should never need to break, and also enable 'reference links' to shield links by sticking their definition 'outside' the table. See <https://github.com/jgm/pandoc/issues/7641>.
-- This also gives us somewhat cleaner HTML by making Pandoc not insert '\n'.
safeHtmlWriterOptions :: WriterOptions
safeHtmlWriterOptions = def{writerColumns = 9999, writerExtensions = enableExtension Ext_shortcut_reference_links pandocExtensions}


dateRegex, footnoteRegex, sectionAnonymousRegex, badUrlRegex :: String
dateRegex             = "^[1-2][0-9][0-9][0-9](-[0-2][0-9](-[0-3][0-9])?)?$"
footnoteRegex         = "^/?[[:alnum:]-]+#fn[1-9][0-9]*$" -- '/Foo#fn3', 'Foo#fn1', 'Foo-Bar-2020#fn999' etc
sectionAnonymousRegex = "^#section-[0-9]+$" -- unnamed sections which receive Pandoc positional auto-names like "#section-1", "#section-15"; unstable, should be named if ever to be linked to, etc.
badUrlRegex           = "http.*http|doc/.*doc/"::String

-- Heuristic checks for specific link sources:
checkURL :: String -> IO ()
checkURL u = do let doubleURL = u =~ badUrlRegex -- I keep accidentally concatenating Arxiv URLs when tagging.
                if not doubleURL then return () else error u

processDOI, processDOIArxiv :: String -> String
processDOI = replace "–" "-" . replace "—" "-" . replace "https://doi.org/" "" . sed "^doi:" ""
 -- Arxiv has some weird URLs and edge-cases like <https://arxiv.org/abs/hep-ph/0204295> (note double-subdirectory & lack of period-separation).
processDOIArxiv url = "10.48550/arXiv." ++
                               sed "https://arxiv.org/[a-z-]+/([0-9]+\\.[0-9]+).*" "\\1" -- regular current Arxiv URL pattern
                               (sed "https://arxiv.org/abs/[a-z-]+/([0-9]+).*" "\\1" url) -- old-style like 'hep-ph'

-- handle initials consistently as period+space-separated; delete titles; delete the occasional final Oxford 'and' cluttering up author lists
cleanAuthors :: String -> String
cleanAuthors = trim . replaceMany cleanAuthorsFixedRewrites  .
                       sedMany (isUniqueKeys [
                         ("([a-zA-Z]+),([A-Z][a-z]+)", "\\1, \\2"), -- "Foo Bar,Quuz Baz" → "Foo Bar, Quuz Baz"
                         (",$", ""),
                         (", +", ", "),
                         ("^([A-Z][a-z]+), ([A-Z]\\.)$", "\\2 \\1"), -- "Smith, J." → "J. Smith"; for single words followed by a single letter, we can assume that it is a 'surname, initial' rather than 2 authors, 'surname1, surname2'
                         ("^([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.)$", "\\2 \\1, \\4 \\3"), -- likewise, but for the 2-author case: 'Smith, J.; Doe, J.'
                         ("^([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.)$", "\\2 \\1, \\4 \\3, \\6 \\5"), -- 3-author
                         ("^([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.)$", "\\2 \\1, \\4 \\3, \\6 \\5, \\8 \\7"), -- 4-author, and I won't try for more
                         ("([A-Z]\\.)([A-Za-z]+)", "\\1 \\2"),                              -- "A.Smith"      → "A. Smith"
                         (" ([A-Z])([A-Z]) ([A-Za-z]+)", " \\1. \\2. \\3"),             -- " LK Barnes"   → " L. K. Barnes"
                         ("([A-Z]\\.)([A-Z]\\.) ([A-Za-z]+)", "\\1 \\2 \\3"),               -- "A.B. Smith"   → "A. B. Smith"
                         ("([A-Z]\\.)([A-Z]\\.)([A-Z]\\.) ([A-Za-z]+)", "\\1 \\2 \\3 \\4"), -- "C.A.B. Smith" → "C. A. B. Smith"
                         (" ([A-Z])([A-Z])([A-Z]) ", " \\1. \\2. \\3. "),                   -- "John HAB Smith" → "John H. A. B. Smith"
                         (" ([A-Z])([A-Z]) ", " \\1. \\2. "),                               -- "John HA Smith"  → "John H. A. Smith"
                         (" ([A-Z]\\.) ([A-Z]) ", " \\1 \\2. "),                            -- "John H. A Smith"  → "John H. A. Smith"
                         (" ([A-Z]) ([A-Z]\\.) ", " \\1. \\2 "),                            -- "John H A. Smith"  → "John H. A. Smith"
                         (" ([A-Z]) ", " \\1. ")                                             -- "John H Smith"   → "John H. Smith"
                         ])
cleanAuthorsFixedRewrites :: [(String,String)]
cleanAuthorsFixedRewrites = isUniqueKeys [(". . ", ". "), ("?",""), (",,", ","), (", ,", ", "), (" MA,", ","), (", MA,", ","), (" MS,", ","),
                                           ("Dr ", ""), (" PhD", ""), (" Ph.D.", ""), (" MRCGP", ""), (" OTR/L", ""), (" OTS", ""),
                                           (" FMedSci", ""), ("Prof ", ""), (" FRCPE", ""), (" FRCP", ""), (" FRS", ""), (" MD", ""),
                                           (",, ,", ", "), ("; ", ", "), (" ; ", ", "), (" , ", ", "), (" and ", ", "), (", & ", ", "),
                                           (", and ", ", "), (" MD,", " ,"), (" M. D.,", " ,"), (" MSc,", " ,"), (" M. Sc.", ""), (" B. Sc.", ""),
                                           (" PhD,", " ,"), (" Ph.D.,", " ,"), (" BSc,", ","), (" BSc(Hons)", ""), (" MHSc,", ","),
                                           (" BScMSc,", ","), (" ,,", ","), (" PhD1", ""), (" BA(Hons),1", ""), (" , BSc(Hons),1", ","),
                                           (" , MHSc,", ","), ("PhD,1,2 ", ""), ("PhD,1", ""), (" , BSc", ", "), (",1 ", ","), (" & ", ", "),
                                           ("BA(Hons),", ","), (", (Hons),", ","), (", ,2 ", ","), (",2", ","), (" MSc", ","), (" , PhD,", ","),
                                           (" JD,", ","), ("MS,", ","), (" BS,", ","), (" MB,", ","), (" ChB", ""), ("Meena", "M."), (", PhD1", ","),
                                           ("  DMSc", ""), (",, ", ", "), (", ,,", ", "), ("\"", ""), ("'", "’"), ("OpenAI, :, ", ""), (" et al", ""),
                                           (" et al.", ""), (", et al.", ""), ("Jr.", "Junior"), (", Jr.", " Junior"), (", Junior", " Junior"),
                                           (" DO,", ","), ("M. D. MPH", ""), (" ", " "), (" M. D. MBA", ""), (" Esq.", ""), (" Esq,", ","),
                                           (" M. D. MMM", "")]
cleanAuthorsTest :: [(String,String,String)]
cleanAuthorsTest = testInfixRewriteLoops cleanAuthorsFixedRewrites cleanAuthors

-- convert a LaTeX expression to Unicode/HTML/CSS by an OA API script.
-- > Text.Pandoc.Walk.walkM inlineMath2Text [Math InlineMath "a + b = c"]
-- [RawInline (Format "html") "<em>a</em> + <em>b</em> = <em>c</em>"]
inlineMath2Text :: Inline -> IO Inline
inlineMath2Text x@(Math InlineMath a) =
  do (status,_,mb) <- runShellCommand "./" Nothing "python3" ["static/build/latex2unicode.py", T.unpack a]
     let mb' = T.pack $ trim $ replace "Converted output: " "" $ U.toString mb
     case status of
       ExitFailure err -> printGreen (intercalate " : " [T.unpack a, T.unpack mb', ppShow status, ppShow err, ppShow mb']) >> printRed "latex2unicode.py failed!" >> return x
       _ -> return $ if mb' == a then x else RawInline (Format "html") mb'
inlineMath2Text x = return x

-- test for possible infinite-loops in the rewrite suite; our initial source of cases is just the fixed-string rewrites.
-- The cycle detector is not enough because the rewrites operate infix, not by replacing the *whole* string, so it's possible to have expansion/contraction which produces loops.
cleanAbstractsHTMLTest :: [(String,String,String)]
cleanAbstractsHTMLTest = testInfixRewriteLoops htmlRewriteFixed cleanAbstractsHTML

-- run all necessary rewrites on a string to clean up malformation, inconsistent formatting, errors, convert to house style, etc
cleanAbstractsHTML :: String -> String
cleanAbstractsHTML = fixedPoint cleanAbstractsHTML'
 where cleanAbstractsHTML' :: String -> String
       cleanAbstractsHTML' = trim . sedMany (isUniqueKeys htmlRewriteRegexp) . replaceMany (isUniqueKeys htmlRewriteFixed)

htmlRewriteRegexp, htmlRewriteFixed :: [(String, String)]
htmlRewriteRegexp = [
         ("from ([0-9\\.]+) to ([0-9\\.]+)", "\\1 → \\2") -- "when moving from 8 to 256 GPUs" → "when moving 8 → 256 GPUs"
         , ("<li>([a-zA-Z0-9].*[^>])</li>", "<li><p>\\1</p></li>") -- work around Pandoc generating naked-text list items, which causes perennial downstream errors in the JS
         , ("([0-9.]+)E10[-−–—]([0-9]+)", "\\1 × 10<sup>−\\2")
         , ("([0-9])- (millisecond|second|minute|hour|day|week|month|year)", "\\1-\\2") -- line-break errors like 'we observed the mice for 2- minutes or 10-minutes afterwards'
         , ("\\\\emph{([a-zA-Z0-9-]+)}", "<em>\\1</em>")
         , ("\\\\textit{([a-zA-Z0-9-]+)}", "<em>\\1</em>")
         -- rewrite *Markdown italics* to <em>HTML italics</em>, and strong/bold:
         , ("(.*)\\*(.+)\\*(.*)", "\\1<em>\\2</em>\\3")
         , ("(.*)\\*\\*(.+)\\*\\*(.*)", "\\1<strong>\\2</strong>\\3")
         , ("<p>This paper was accepted by [A-Z][a-z]+ [A-Z][a-z]+, .*\\.</p>", "")
         , (" ### Author Declarations .*$", "")
         , (" ### Competing Interest Statement .*$", "")
         , (" Study ([0-9][a-z]?)", " <strong>Study \\1</strong>")
         , (" Experiment ([0-9][a-z]?)", " <strong>Experiment \\1</strong>")
         , ("<strong>Section ([a-zA-Z0-9.-]+)</strong>", "§\\1")
         , ("\\*\\*Section ([a-zA-Z0-9.-]+)\\*\\*", "§\\1")
         -- <https://en.wikipedia.org/wiki/ClinicalTrials.gov>
         , (" (NCT[0-9]+)", "<a href=\"https://clinicaltrials.gov/show/\\1\">\\1</a>")
         , (" (NCT[0-9]+)</p>", "<a href=\"https://clinicaltrials.gov/show/\\1\">\\1</a>.</p>")
         -- cleanup bare URLs (particularly common in Arxiv abstracts when linking to Github):
         , (" (https?://[a-zA-Z0-9_\\.\\?/-]+)$", " <a href=\"\\1\">\\1</a>$")
         , (" (https?://[a-zA-Z0-9_\\.\\?/-]+)</p>", " <a href=\"\\1\">\\1</a></p>")
         , (" (https?://[a-zA-Z0-9_\\.\\?/-]+)\\)", " <a href=\"\\1\">\\1</a> )")
         , (" (https?://[a-zA-Z0-9_\\.\\?/-]+) \\.", " <a href=\"\\1\">\\1</a>.")
         , (" (https?://[a-zA-Z0-9_\\.\\?/-]+) ?\\.</p>", " <a href=\"\\1\">\\1</a>.</p>")
         , ("at:? (github.com/.*).</p>", "at <a href=\"https://\\1\">\\1</a>.</p>") -- "Code is available at github.com/microsoft/SPACH.</p>" / "Code will be released at: github.com/NVlabs/SegFormer.</p>"
         , (" (https://github.com/[a-zA-Z0-9_\\.\\?/-]+) ?\\.</p>", " <a href=\"\\1\">Github</a>.</p>")
         -- try to rewrite half-parenthesis lists like '(( 1) foo; 2) bar' into '(1) foo; (2) bar' for consistency & parentheses-checking:
         , ("\\(10\\) (.*) 11\\)", " (10) \\1 (11)")
         , (" 10\\) (.*) 11\\)", " (10) \\1 (11)")
         , ("\\(9\\) (.*) 10\\)", " (9) \\1 (10)")
         , (" 9\\) (.*) 10\\)", " (9) \\1 (10)")
         , ("\\(8\\) (.*) 9\\)", " (8) \\1 (9)")
         , (" 8\\) (.*) 9\\)", " (8) \\1 (9)")
         , ("\\(7\\) (.*) 8\\)", " (7) \\1 (8)")
         , (" 7\\) (.*) 8\\)", " (7) \\1 (8)")
         , ("\\(6\\) (.*) 7\\)", " (6) \\1 (7)")
         , (" 6\\) (.*) 7\\)", " (6) \\1 (7)")
         , ("\\(5\\) (.*) 6\\)", " (5) \\1 (6)")
         , (" 5\\) (.*) 6\\)", " (5) \\1 (6)")
         , ("\\(4\\) (.*) 5\\)", " (4) \\1 (5)")
         , (" 4\\) (.*) 5\\)", " (4) \\1 (5)")
         , ("\\(3\\) (.*) 4\\)", " (3) \\1 (4)")
         , (" 3\\) (.*) 4\\)", " (3) \\1 (4)")
         , ("\\(2\\) (.*) 3\\)", " (2) \\1 (3)")
         , (" 2\\) (.*) 3\\)", " (2) \\1 (3)")
         , ("\\(1\\) (.*) 2\\)", " (1) \\1 (2)")
         , (" 1\\) (.*) 2\\)", " (1) \\1 (2)")
         -- citations: eg '...biochemical programs (preconditioning)2,3,4. Under...'; we require 2 because 1 number is ambiguous & collides with chemistry/genetics.
         , ("([a-z[:punct:]])([0-9]+,[0-9]+)\\.", "\\1<sup>\\2</sup>.")
         , ("([a-z[:punct:]])([0-9]+,[0-9]+,[0-9]+)\\.", "\\1<sup>\\2</sup>.") -- '2,3,4.'
         , ("([a-z[:punct:]])([0-9]+,[0-9]+,[0-9]+,[0-9]+)\\.", "\\1<sup>\\2</sup>.")
         , ("([a-z[:punct:]])([0-9]+,[0-9]+,[0-9]+,[0-9]+,[0-9]+)\\.", "\\1<sup>\\2</sup>.")
         , ("([a-z[:punct:]])([0-9]+,[0-9]+,[0-9]+,[0-9]+,[0-9]+,[0-9]+)\\.", "\\1<sup>\\2</sup>.")
         , ("([a-z[:punct:]])([0-9]+,[0-9]+,[0-9]+,[0-9]+,[0-9]+,[0-9]+,[0-9]+)\\.", "\\1<sup>\\2</sup>.")
         , ("([0-9]) x ([0-9]+)\\(([0-9−-]+)\\)", "\\1 × \\2<sup>\\3</sup>") -- '~2.5 x 10(13)' → '~2.5 × 10<sup>13</sup>'
         -- common spelling error, 'a' → 'an':
         , (" a ([aeio][a-z]+)", " an \\1")
         -- - comma-separate at thousands for consistency:
         -- skip thousands, since clobbers citations like 'Herring 2009' (which turns into 'Herring 2,009')
         , (" ([0-9]+)([0-9][0-9][0-9])([0-9][0-9][0-9])",                                   " \\1,\\2,\\3")         -- millions
         , (" ([0-9]+)([0-9][0-9][0-9])([0-9][0-9][0-9])([0-9][0-9][0-9])",                  " \\1,\\2,\\3,\\4")     -- billions
         , (" ([0-9]+)([0-9][0-9][0-9])([0-9][0-9][0-9])([0-9][0-9][0-9])([0-9][0-9][0-9])", " \\1,\\2,\\3,\\4,\\5") -- trillions
         , ("([0-9]+) ([0-9]+)",                                                             "\\1,\\2") -- '50 000' → '50,000'
         , ("([0-9]+) percent([ [:punct:]])", "\\1%\\2") -- eg '$22,000 (46 percent) higher annual early-career wages than they would'
         , ("([0-9][0-9]+) [xX] ([0-9][0-9]+) ", "\\1×\\2") -- "high fidelity generation of 1024 x 1024 images" / "0.85 X 30 mEq/kg"
         , ("([0-9][0-9]+) ?[xX] ?([0-9][0-9]+) ?px", "\\1×\\2px") --  "Alexnet performance for 16 x16 px features)."
         , ("([0-9]+)[ -]fold", "\\1×")
         , ("([0-9]+)[ -]times", "\\1×")
         , ("<br /> <strong>([A-Z][a-z]+)<\\/strong><p>", "<p><strong>\\1</strong>: ") --         <br /> <strong>Background</strong><p>
         , ("</p><strong>([A-Z][a-z]+)<\\/strong><p>", "</p> <p><strong>\\1</strong>: ")
         , ("<p><strong>([A-Z][a-z]+)<\\/strong>:</p> <p>", "<p><strong>\\1</strong>: ")
         , ("<p><strong>([A-Z][a-z]+ [A-Za-z]+)<\\/strong>:</p> <p>", "<p><strong>\\1</strong>: ")
         , ("<p><strong>([A-Z][a-z]+ [A-Za-z]+ [A-Za-z]+)<\\/strong>:</p> <p>", "<p><strong>\\1</strong>: ")
         , ("<p><strong>([A-Z][a-z]+ [A-Za-z]+ [A-Za-z]+ [A-Za-z]+)<\\/strong>:</p> <p>", "<p><strong>\\1</strong>: ")
         , ("<xref rid=\"sec[0-9]+\" ref-type=\"sec\">([A-Za-z]+ [0-9]+)</xref>", "<strong>\\1</strong>") -- PLOS: '<xref rid="sec022" ref-type="sec">Experiment 3</xref>' etc.
         , ("^en$", "")
         , (" ([0-9]) h ", " \\1h ") -- hour abbreviation
         , ("range: ([0-9%.]+) to ([0-9%.]+)", "range: \\1–\\2") -- eg. "range: 0.59 to 43.89"
         , ("between ([0-9%]+) and ([0-9]+)", "\\1–\\2") -- "range between 2 and 10" → "range 2–10"
         , ("([0-9%]) – ([0-9])", "\\1–\\2") -- space-separated en-dash ranges eg. "with a range of ~0.60 – 0.71 for height"
         , ("([0-9%]) – ([a-z])", "\\1—\\2") -- a number-alphabet en-dash is usually an em-dash eg. "a Fréchet Inception Distance (FID) of 10.59 – beating the baseline BigGAN model—at"
         , ("([a-zA-Z]) – ([[:punct:]])", "\\1—\\2") -- en dash errors in WP abstracts: usually meant em-dash. eg. 'disc format – <a href="https://en.wikipedia.org/wiki/Universal_Media_Disc">Universal'
         , ("([[:punct:]]) – ([a-zA-Z])", "\\1—\\2")
         , ("([a-zA-Z’]) – ([a-zA-Z])", "\\1—\\2") -- eg: "Aspects of General Intelligence – a Deep Phenotyping Approach"
         , ("([a-zA-Z’]) - ([a-zA-Z])", "\\1—\\2") -- spaced hyphens: also usually em dashes: "Towards personalized human AI interaction - adapting the behavior of AI agents"
         , (" -([0-9])", " −\\1") -- eg. 'β = -0.08', HYPHEN to MINUS SIGN; review of annotations shows that this is almost always safe to do except in a few very rare cases like a psychiatry paper abbreviating 'DSM-3, -4, and -5'.
         , ("×10[-–—]([0-9]+)", " × 10<sup>−\\1</sup>")
         , (" × 10[-–—]([0-9]+)", " × 10<sup>−\\1</sup>") -- the Unicode '×' seems to never match when used inside a range...?
         , ("([0-9]) [x×] 10[-–—]([0-9]+)", "\\1 × 10<sup>−\\2</sup>")
         , ("([0-9]) [x×] 10\\([-–—]([0-9]+)\\)", "\\1 × 10<sup>−\\2</sup>")
         , ("<sup>-([0-9]+)</sup>", "<sup>−\\1</sup>") -- eg. '10<sup>-7</sup>', HYPHEN to MINUS SIGN
         , ("([0-9]+)\\^([0-9\\.]+)", "\\1<sup>\\2</sup>") -- eg '(p<10^4)'
         , ("([0-9]+)\\^[-−]([0-9\\.]+)", "\\1<sup>−\\2</sup>") -- eg '(p<10^-4)'
         , (" ([0-9]+%?)-([0-9]+) ", " \\1–\\2 ")
         , ("([0-9]) %", "\\1%")
         , (" ([0-9]+) out of the ([0-9]+) ", " \\1⁄\\2 ")
         , (" ([0-9]+) out of ([0-9]+) ", " \\1⁄\\2 ") -- need space-separation due to examples like 'smartphones are now used by 5.8 out of 7.0 billion people on earth'
         , (" ([0-9][0-9]?[0-9]?) of ([0-9][0-9]?[0-9]?) ", " \\1⁄\\2 ")
         , ("([0-9]+) of ([0-9]+)", "\\1⁄\\2")
         , (" ([.0-9]+)[xX]", " \\1×")
         , ("=-\\.([.0-9]+)", " = -0.\\1")
         , ("([0-9]*[02456789])th ", "\\1<sup>th</sup> ")
         , ("([0-9]*[1])st ",        "\\1<sup>st</sup> ")
         , ("([0-9]*[3])rd ",        "\\1<sup>rd</sup> ")
         , ("\\(JEL [A-Z][0-9][0-9]+\\)\\.?", "")
         , (" \\(JEL [A-Z][0-9][0-9], .* [A-Z][0-9][0-9]\\)", "") -- rm AERA classification tags they stick into the Crossref abstracts
         , ("CI=([.0-9])", "CI = \\1") -- 'CI=0.90' → 'CI = 0.90'
         , ("RR=([.0-9])", "RR = \\1") -- 'RR=2.9' → 'RR = 2.09'
         , ("OR=([.0-9])", "OR = \\1") -- 'OR=2.9' → 'OR = 2.09'
         , ("AOR=([.0-9])", "AOR = \\1") -- 'AOR=2.9' → 'AOR = 2.09'
         -- NOTE TO SELF: all math-inline expressions may be deletable if the OA API latex2unicode.py script works out well.
         -- math regexes
         , ("<span class=\"math inline\">\\\\\\(([a-zA-Z])\\\\\\)</span>", "<em>\\1</em>") -- '<span class="math inline">\(d\)</span>', 'the state matrix <span class="math inline">\(A\)</span>'
         , ("<span class=\"math inline\">\\\\\\(([0-9.]+)\\\\\\)</span>", "\\1") -- '<span class="math inline">\(30\)</span>'
         , ("<span class=\"math inline\">\\\\\\(\\(\\\\leq ([0-9.]+)\\)\\\\\\)</span>", "(≤\\1)") -- "<span class=\"math inline\">\\((\\leq 500)\\)</span>" -> '(≤500)'
         , ("\\$([.0-9]+) \\\\cdot ([.0-9]+)\\^([.0-9]+)\\$",             "\\1 × \\2^\\3^")
         , ("\\$([.0-9]+) \\\\cdot ([.0-9]+)\\^\\{([.0-9]+)\\}\\$",       "\\1 × \\2^\\3^")
         , ("<span class=\"math inline\">\\\\\\(([0-9.]+) ?\\\\times\\\\\\)</span>", "\\1×") -- '<span class="math inline">\(1.5\times\)</span>'
         , ("<span class=\"math inline\">\\\\\\(([0-9.]+) ?\\\\times ([0-9.]+)\\\\\\)</span>", "\\1×\\2") -- '<span class="math inline">\(224\times\ 224)</span>'
         , ("<span class=\"math inline\">\\\\\\(([0-9.]+) \\\\\\%\\\\\\)</span>", "\\1%") -- '<span class="math inline">\(90 \%\)</span>'
         , ("<span class=\"math inline\">\\\\\\(([0-9.]+)\\\\\\%\\\\\\)</span>", "\\1%") -- '<span class=\"math inline\">\\(83.6\\%\\)</span>'
         , ("<span class=\"math inline\">\\\\\\(\\\\texttt\\{([A-Za-z]+)\\}\\\\\\)</span>", "<code>\\1</code>") -- 'we present the <span class=\"math inline\">\\(\\texttt{GamePhysics}\\)</span> dataset'
         , ("<span class=\"math inline\">\\\\\\(\\\\textbf\\{([A-Za-z]+)\\}\\\\\\)</span>", "<strong>\\1</strong>") -- 'We dub ρ the <span class="math inline">\(\textbf{polarity}\)</span> parameter'
         , ("<span class=\"math inline\">\\\\\\(\\\\times\\\\\\)</span>", "×") -- '<span class="math inline">\(\times\)</span>'
         , ("<span class=\"math inline\">\\\\\\(([0-9]*)\\^([0-9]*)\\\\\\)</span>", "\\1<sup>\\2</sup>") -- '<span class="math inline">\(10^4\)</span>'
         , ("<span class=\"math inline\">\\\\\\(([0-9]*)\\^{([0-9]*)}\\\\\\)</span>", "\\1<sup>\\2</sup>") -- '<span class="math inline">\(10^{40}\)</span>'
         , ("([A-z][a-z]+) ?et ?al ?\\(([0-9][0-9][0-9][0-9])\\)", "\\1 et al \\2") -- 'Dette et al (2013)'
         , ("([A-Z][a-z]+) and ([A-Z][a-z]+),? ([0-9]+)", "\\1 & \\2 \\3") -- 'Foo and Bar 1999', 'Foo and Bar, 1999' → 'Foo & Bar 1999'; 'et al' is handled by Pandoc already
         , ("([A-Z][a-z]+) &amp; ([A-Z][a-z]+), ([12][0-9][0-9][0-9])", "\\1 & \\2 \\3")
         , ("([A-Z][a-z]+) & ([A-Z][a-z]+), ([12][0-9][0-9][0-9])",     "\\1 & \\2 \\3")
         , ("\\. Copyright.*[12][0-9][0-9][0-9] John .* Ltd\\.", ".") -- 'Copyright © 2013 John Wiley &amp; Sons, Ltd.'
         , ("<br />    <strong>([a-zA-Z]+)</strong><br /><p>", "<p><strong>\\1</strong>: ")
         , ("<strong>([a-zA-Z0-9_]+)</strong>:<p>", "<p><strong>\\1</strong>: ")
         , ("<jats:title>([a-zA-Z0-9_]+):</jats:title><jats:p>", "<p><strong>\\1</strong>: ")
         , ("<jats:title>([a-zA-Z0-9_]+)\\.</jats:title><jats:p>", "<p><strong>\\1</strong>: ")
         , ("<jats:styled-content style=\"fixed-case\">([A-Z.]+)</jats:styled-content>", "\\1") -- odd acronym markup
         , ("<jats:sec id=\"[a-zA-Z0-9_]+\">", "")
         , ("<sec id=\"[a-zA-Z0-9_]+\">", "")
         , ("<jats:sec id=\"[a-zA-Z0-9_]+\" sec-type=\"[a-z]+\">", "")
         , (" © [0-9]+ European Association of Personality Psychology", "")
         ]

-- simple string substitutions:
htmlRewriteFixed =
         [
         ("<strong>One Sentence Summary</strong></p>\n<p>", "<strong>One Sentence Summary</strong>: ")
         , ("<strong>One Sentence Summary</strong></p> <p>", "<strong>One Sentence Summary</strong>: ")
         , ("R<sup>2</sup>D2", "R2D2")
         , ("</p> ?<p>", "</p>\n<p>")
         , ("</p>\n<p>", "</p> <p>")
         , ("</p>\n \n<p>", "</p>\n<p>")
         , ("  *", " ") -- squeeze whitespace
         , (" \\( ", " (")
         , (" \\) ", " )")
         , (" </p>", "</p>")
         , ("</a></p>", "</a>.</p>")
         , ("<br /> *</p>", "</p>")
         , ("<p> *", "<p>")
         , (" *</p>", "</p>")
         , ("<em>R</em>  *<sup>2</sup>", "<em>R</em><sup>2</sup>")
         , ("<span style=\"font-weight:normal\"> </span>", "")
         , ("href=\"github.com", "href=\"https://github.com")
         , ("https://github.com/deepmind/ deepmind-research/", "https://github.com/deepmind/deepmind-research/")
         , ("i . e .,", "ie.")
         , ("<p><strong>Conclusion</strong></p>\n<p>", "<p><strong>Conclusion</strong>: ")
         , ("<p><strong>Motivation</strong></p>\n<p>", "<p><strong>Motivation</strong>: ")
         , ("<p><strong>Availability</strong></p>\n<p>", "<p><strong>Availability</strong>: ")
         , ("<p><strong>Importance</strong></p>\n<p>", "<p><strong>Motivation</strong>: ")
         , ("<p><strong>Main Outcomes & Measures</strong></p>\n<p>", "<p><strong>Outcomes & Measures</strong>: ")
         , ("<p>[<strong>Keywords</strong>: ]</p>", "")
         , ("<strong>Null</strong>: ", "")
         , ("<em>p</em>=", "<em>p</em> = ")
         , ("β=", "β = ")
         , ("\8217=", "\8217 = ")
         , (" the the ", " the ")
         , (" a imbalance", " an imbalance")
         , ("<span style=\"display:inline-block;vertical-align:-0.4em;font-size:80%;text-align:left\"><sup></sup><br /><sub>", "")
         , ("<sup>+</sup>", "⁺")
         , ("</sub>=", "</sub> = ") -- eg. '<em>r</em><sub>A</sub>=0.28'
         , ("<sup></sup>", "")
         , ("<sub></sub>", "")
         , ("<i>", "<em>")
         , ("</i>", "</em>")
           -- math substitutions:
         , ("$e=mc^2$", "<em>e</em> = <em>mc</em><sup>2</sup>")
         , ("$\frac{4}{3} \\cdot \\pi \\cdot r^3$", "4⁄3 × π × _r_^3^")
         , ("$f(x; x_0,\\gamma)$", "<em>f(x; x<sub>0</sub>,γ")
         , ("$\\mathcal{O}(log n)$", "𝒪(log <em>n</em>)")
         , ("$\\mathrm{sinc}(0)=1$", "sinc(0) = 1")
         , ("$\\mathrm{sinc}(ax)$", "sinc(<em>ax</em>)")
         , ("$\\mathrm{sinc}(x)=\\sin(x)/x$", "sinc(<em>x</em>) = sin(<em>x</em>)⁄<em>x</em>")
         , ("$\\mu$", "μ")
         , ("$O(log n)$", "𝒪(log <em>n</em>)")
         , ("$x$", "<em>x</em>")
         , ("$(x_0,\\gamma)$", "<em>(x<sub>0</sub>, γ)</em>")
         , ("a n layer", "a <em>n</em> layer")
         , ("{\\epsilon}-greedy", "ε-greedy")
         , (" L0", " 𝓁<sub>0</sub>")
         , ("l1-distance", "𝓁<sub>1</sub>-distance")
         , ("L1 loss", "𝓁<sub>1</sub> loss")
         , (" L1", " 𝓁<sub>1</sub>")
         , ("L1-penalized", "𝓁<sub>1</sub>-penalized")
         , ("L1-regularized", "𝓁<sub>1</sub>-regularized")
         , ("l2-distance", "𝓁<sub>2</sub>-distance")
         , (" L2", " 𝓁<sub>2</sub>")
         , ("({\\lambda})", "(λ)")
         , (" L-infinity", " 𝓁<sub>∞</sub>")
         , (" L-∞", " 𝓁<sub>∞</sub>")
         , (" L∞", " 𝓁<sub>∞</sub>")
         , (" Lp", " 𝓁<sub><em>p</em></sub>")
         , ("<math>A</math>", "<em>A</em>")
         , ("<math>B</math>", "<em>B</em>")
         , ("<math>C</math>", "<em>C</em>")
         , ("<math>S</math>", "<em>S</em>")
         , (" N pixels", " <em>N</em> pixels")
         , (" n-step", " <em>n</em>-step")
         , (" O(1)", " 𝒪(1)")
         , (" O<em>n</em> log n)", " 𝒪(<em>n</em> log <em>n</em>)")
         , ("O((log n log log n)^2)", "𝒪(log<sup>2</sup> <em>n</em> log log <em>n</em>)")
         , (" O(log n)", " 𝒪(log <em>n</em>)")
         , ("O(log n) ", "𝒪(log <em>n</em>) ")
         , ("O(m log^2 n)", "𝒪(<em>m</em> log <em>n</em> + <em>n</em> log<sup>2</sup> <em>n</em>)")
         , ("O(n^2)", "𝒪(<em>n</em><sup>2</sup>)")
         , (" O(n log n)", " 𝒪(<em>n</em> log <em>n</em>)")
         , ("O(nm)", "𝒪(<em>n</em>×<em>m</em>)")
         , ("O(n)", "𝒪(<em>n</em>)")
         , (" O(N)", " 𝒪(<em>N</em>)")
         , ("O(N) ", "𝒪(<em>N</em>) ")
         , ("O(<span class=\"math inline\">\\(L^2\\)</span>", "𝒪(<em>L</em><sup>2</sup>)")
         , ("O(<span class=\"math inline\">\\(L\\log L\\)</span>", "𝒪(<em>L</em> log <em>L</em>)")
         , (" O(sqrt(n)) ", " 𝒪(√<em>n</em>) ")
         , (" O(T)", " 𝒪(<em>T</em>)")
         , ("<span class=\"math inline\">\\(0.15{-}1.3 {\\rm M}_{\\odot}\\)</span>", "0.15–1.3M<sub>☉</sub>")
         , ("<span class=\"math inline\">\\([0,1]\\)</span>", "[0,1]")
         , ("<span class=\"math inline\">\\(0.96\\)</span>", "0.96")
         , ("<span class=\"math inline\">\\(0&lt;p&lt;1\\)</span>", "0 &lt; <em>p</em> &lt; 1")
         , ("<span class=\"math inline\">\\(100,\\!000\\)</span>", "100,000")
         , ("<span class=\"math inline\">\\(1,\\!000\\times\\)</span>", "1,000×")
         , ("<span class=\"math inline\">\\(1,000\\times\\)</span>", "1,000×")
         , ("<span class=\"math inline\">\\(10^5\\times\\)</span>", "10<sup>5</sup>×")
         , ("<span class=\"math inline\">\\(\119978(<em>n</em><sup>2</sup>)\\)</span>", "𝒪(<em>n</em><sup>2</sup>)")
         , ("<span class=\"math inline\">\\(124/144\\)</span>", "124⁄144")
         , ("<span class=\"math inline\">\\(128/255\\)</span>", "128⁄255")
         , ("<span class=\"math inline\">\\(1/2 H_n\\)</span>", "1⁄2<em>H<sub>n</sub></em>")
         , ("<span class=\"math inline\">\\(1/(3n)\\)</span>", "1⁄(3<em>n</em>)")
         , ("<span class=\"math inline\">\\(1.644934\\approx \\pi^2/6\\)</span>", "1.644934 ≈ π<sup>2</sup>⁄6")
         , ("<span class=\"math inline\">\\(_{16}\\)</span>", "<sub>16</sub>")
         , ("<span class=\"math inline\">\\(1 - \\frac{1}{e}\\)</span>", "1 − 1⁄<em>e</em>")
         , ("<span class=\"math inline\">\\(1/n\\)</span>", "1⁄<em>n</em>")
         , ("<span class=\"math inline\">\\(1,...,n\\)</span>", "1,...,<em>n</em>")
         , ("<span class=\"math inline\">\\(1-\\tilde \\Omega(n^{-1/3})\\)</span>", "1 − Ω̃(<em>n</em><sup>−1⁄3</sup>)")
         , ("<span class=\"math inline\">\\(1-\\tilde O(n^{-1/3})\\)</span>",       "1 − 𝑂̃(<em>n</em><sup>−1⁄3</sup>)")
         , ("<span class=\"math inline\">\\(2.4\\)</span>", "2.4")
         , ("<span class=\"math inline\">\\(2\\cdot \\mathtt{OPT}\\)</span>", "2 · <strong>OPT</strong>")
         , ("<span class=\"math inline\">\\(2^{\\Omega(k)}\\)</span>", "2<sup>Ω(<em>k</em>)</sup>")
         , ("<span class=\"math inline\">\\(2^S \\to \\mathbb{R}\\)</span>", "2<sup><em>S</em></sup> ⟶ ℝ")
         , ("<span class=\"math inline\">\\(3,\\!000\\times\\)</span>", "3,000×")
         , ("<span class=\"math inline\">\\(42.5\\)</span>", "42.5")
         , ("<span class=\"math inline\">\\(4.5\\%\\)</span>", "4.5%")
         , ("<span class=\"math inline\">\\(47.1\\)</span>", "47.1")
         , ("<span class=\"math inline\">\\(4\\sim 16\\times\\)</span>", "4–16×")
         , ("<span class=\"math inline\">\\(_{50}\\)</span>", "<sub>50</sub>")
         , ("<span class=\"math inline\">\\(5\\%-35\\%\\)</span>", "5%–35%")
         , ("<span class=\"math inline\">\\(6\\times 10^{-6}\\)</span>", "6×10<sup>−6</sup>")
         , ("<span class=\"math inline\">\\(7.5\\sim9.5\\times\\)</span>", "7.5–9.5×")
         , ("<span class=\"math inline\">\\(8/255\\)</span>", "8⁄255")
         , ("<span class=\"math inline\">\\(86\\%\\)</span>", "86%")
         , ("<span class=\"math inline\">\\(9.3\\%\\)</span>", "9.3%")
         , ("<span class=\"math inline\">\\9<em>r</em> &gt; 1</span>", "<em>r</em> &gt; 1")
         , ("<span class=\"math inline\">\\(\\alpha &gt; 0\\)</span>", "α > 0")
         , ("<span class=\"math inline\">\\(\\alpha\\)</span>", "α")
         , ("<span class=\"math inline\">\\(a^nb^{2n}\\)</span>", "<em>a<sup>n</sup>b<sup>2n</sup>")
         , ("<span class=\"math inline\">\\(a^nb^mc^{n+m}\\)</span>", "<em>a<sup>n</sup>b<sup>m</sup>c<sup>n+m</sup>")
         , ("<span class=\"math inline\">\\(a^nb^nc^n\\)</span>", "<em>a<sup>n</sup>b<sup>n</sup>c<sup>n</sup>")
         , ("<span class=\"math inline\">\\(a^nb^n\\)</span>", "<em>a<sup>n</sup>b<sup>n</sup>")
         , ("<span class=\"math inline\">\\(\\approx\\)</span>", "~")
         , ("<span class=\"math inline\">\\(\\beta&gt;0\\)</span>", "<em>β</em> > 0")
         , ("<span class=\"math inline\">\\(\\boldsymbol{sponge} \\sim\\boldsymbol{examples}\\)</span>", "<strong>sponge examples</strong>")
         , ("<span class=\"math inline\">\\(B(u,u)\\)</span>", "<em>B</em>(<em>u</em>, <em>u</em>)")
         , ("<span class=\"math inline\">\\(c&gt;0\\)</span>", "<em>c</em> &gt; 0")
         , ("<span class=\"math inline\">\\(C &gt; 1\\)</span>", "<em>C</em> &gt; 1")
         , ("<span class=\"math inline\">\\(^\\circ\\)</span>", "°")
         , ("<span class=\"math inline\">\\(c n^{1/3}\\)</span>", "<em>cn</em><sup>1⁄3</sup>")
         , ("<span class=\"math inline\">\\(c\\)</span>", "<em>c</em>")
         , ("<span class=\"math inline\">\\(d^{3/2}\\)</span>", "<em>d</em><sup>3⁄2</sup>")
         , ("<span class=\"math inline\">\\(d^{5/4}\\)</span>", "<em>d</em><sup>5⁄5</sup>")
         , ("<span class=\"math inline\">\\(\\Delta^0_n\\)</span>", "Δ<span class=\"subsup\"><sup>0</sup><sub><em>n</em></sub></span>")
         , ("<span class=\"math inline\">\\(\\dot{M}\\)</span>", "<em>̇M</em>")
         , ("<span class=\"math inline\">\\(\\dot{x} = Ax + Bu, y = Cx + Du\\)</span>", "<em>͘x</em> = <em>Ax</em> + <em>Bu</em>, <em>y</em> = <em>Cx</em> + <em>Du</em>")
         , ("<span class=\"math inline\">\\(D\\)</span>", "<em>D</em>")
         , ("<span class=\"math inline\">\\(e^{-kq^2}.\\)</span>", "<em>e</em><sup>−<em>kq</em><sup>2</sup></sup>")
         , ("<span class=\"math inline\">\\(\\ell_1/\\ell_2\\)</span>", "𝓁<sub>1</sub>/𝓁<sub>2</sub>")
         , ("<span class=\"math inline\">\\(\\ell_1\\)</span>", "𝓁<sub>1</sub>")
         , ("<span class=\"math inline\">\\(\\ell_2\\)</span>", "𝓁<sub>2</sub>")
         , ("<span class=\"math inline\">\\(\\ell_\\infty\\)</span>", "𝓁<sub>∞</sub>")
         , ("<span class=\"math inline\">\\(\\ell_p\\)</span>", "𝓁<sub>p</sub>")
         , ("<span class=\"math inline\">\\<em>n</em> \\geq 3f+1\\)</span>", "<em>n</em> ≥ 3<em>f</em> + 1")
         , ("<span class=\"math inline\">\\<em>n</em> \\geq 6f+1\\)</span>", "<em>n</em> ≥ 6<em>f</em> + 1")
         , ("<span class=\"math inline\">\\(<em>r</em> = 0.99\\)</span>", "<em>r</em> = 0.99")
         , ("<span class=\"math inline\">\\(\\epsilon <em>n</em> \\log <em>n</em> + \\U0001D4AA(<em>n</em>)\\)</span>", "ε <em>n</em> log <em>n</em> + 𝒪(<em>n</em>)")
         , ("<span class=\"math inline\">\\(\\epsilon=\\epsilon(\\eta)\\)</span>", "ε = ε(η)")
         , ("<span class=\"math inline\">\\(\\epsilon \\leq C\\cdot\\mathtt{OPT} + O(\\mathtt{VC}(\\mathcal{H})\\cdot \\eta)\\)</span>", "ε ≤ <em>C</em> · <strong>OPT</strong> + 𝒪(<strong>VC</strong>(ℋ) · η)")
         , ("<span class=\"math inline\">\\(\\epsilon\\)</span>", "ε")
         , ("<span class=\"math inline\">\\(\\epsilon=\\Theta_{\\mathcal{H}}(\\eta)\\)</span>", "ε = Θ<sub>ℋ</sub>(η)")
         , ("<span class=\"math inline\">\\(\\epsilon=\\Theta(\\mathtt{VC}(\\mathcal{H})\\cdot \\eta)\\)</span>", "ε = Θ(<strong>VC</strong>(ℋ)·η)")
         , ("<span class=\"math inline\">\\(\\eta\\)</span>", "η")
         , ("<span class=\"math inline\">\\(\\exp({\\mathcal{O}}(k))\\)</span>", "exp(𝒪(<em>k</em>))")
         , ("<span class=\"math inline\">\\(\\exp({\\Omega}(d))\\)</span>", "exp(Ω(<em>d</em>))")
         , ("<span class=\"math inline\">\\(F(G(X)) \\approx X\\)</span>", "<em>F(G(X)) ≈ X</em>")
         , ("<span class=\"math inline\">\\(f &lt; n\\)</span>", "<em>f</em> &lt; <em>n</em>")
         , ("<span class=\"math inline\">\\(f_\\theta\\)</span>", "<em>f</em><sub>θ</sub>")
         , ("<span class=\"math inline\">\\(f(x) = x \\cdot \\text{sigmoid}(\\beta x)\\)</span>", "<em>f(x)</em> = <em>x</em> × sigmoid(β <em>x</em>)")
         , ("<span class=\"math inline\">\\(F: Y \\rightarrow X\\)</span>", "<em>F : Y → X</em>")
         , ("<span class=\"math inline\">\\(\\gamma = 0.999\\)</span>", "γ = 0.999")
         , ("<span class=\"math inline\">\\(\\gamma = 0.99\\)</span>", "γ = 0.99")
         , ("<span class=\"math inline\">\\(G\\)</span>", "<em>G</em>")
         , ("<span class=\"math inline\">\\(&gt;\\)</span>", "&gt;")
         , ("<span class=\"math inline\">\\(G: X \\rightarrow Y\\)</span>", "<em>G : X → Y</em>")
         , ("<span class=\"math inline\">\\(G(X)\\)</span>", "<em>G(X)</em>")
         , ("<span class=\"math inline\">\\(\\hbar\\)</span>", "ℏ")
         , ("<span class=\"math inline\">\\(H_n \\sim \\ln n\\)</span>", "<em>H<sub>n</sub></em> ln <em>n</em>")
         , ("<span class=\"math inline\">\\(iii\\)</span>", "3")
         , ("<span class=\"math inline\">\\(ii\\)</span>", "2")
         , ("<span class=\"math inline\">\\(i\\)</span>", "1")
         , ("<span class=\"math inline\">\\(\\infty\\)</span>", "∞")
         , ("<span class=\"math inline\">\\(\\it and\\)</span>", "<span class=\"math inline\">\\(\\it also\\)</span> <em>and also</em>")
         , ("<span class=\"math inline\">\\(\\it<br />performance\\)</span>", "<em>performance</em>")
         , ("<span class=\"math inline\">\\(iv\\)</span>", "4")
         , ("<span class=\"math inline\">\\(k=2\\)</span>", "<em>k</em> = 2")
         , ("<span class=\"math inline\">\\(k=3\\)</span>", "<em>k</em> = 3")
         , ("<span class=\"math inline\">\\(\\kappa\\)</span>", "𝜅")
         , ("<span class=\"math inline\">\\(k \\leq d\\)</span>", "<em>k</em> ≤ <em>d</em>")
         , ("<span class=\"math inline\">\\(k \\ll d\\)</span>", "<em>k</em> ≪ <em>d</em>")
         , ("<span class=\"math inline\">\\(k = \\log n\\)</span>", "<em>k</em> = log <em>n</em>")
         , ("<span class=\"math inline\">\\(k \\rightarrow \\infty\\)</span>", "<em>k</em> → ∞")
         , ("<span class=\"math inline\">\\(k\\)</span>", "<em>k</em>")
         , ("<span class=\"math inline\">\\(K\\)</span>", "<em>K</em>")
         , ("<span class=\"math inline\">\\(L_0\\)</span>", "<em>L</em><sub>0</sub>")
         , ("<span class=\"math inline\">\\(L_1\\)</span>", "<em>L</em><sub>1</sub>")
         , ("<span class=\"math inline\">\\(l_1\\)</span>", "𝓁<sub>1</sub>")
         , ("<span class=\"math inline\">\\(L_2\\)</span>", "<em>L</em><sub>2</sub>")
         , ("<span class=\"math inline\">\\(\\langle B(u,u), u\\rangle=0\\)</span>", "〈<em>B</em>(<em>u</em>, <em>u</em>), <em>u</em>〉 = 0")
         , ("<span class=\"math inline\">\\(\\leftarrow\\)</span>", "←")
         , ("<span class=\"math inline\">\\(L_\\infty\\)</span>", "𝓁<sub>∞</sub>")
         , ("<span class=\"math inline\">\\(\\log n\\)</span>", "log <em>n</em>")
         , ("<span class=\"math inline\">\\(L\\)</span>", "<em>L</em>")
         , ("<span class=\"math inline\">\\(&lt;2\\%\\)</span>", "&lt;2%")
         , ("<span class=\"math inline\">\\(&lt;4\\%\\)</span>", "&lt;4%")
         , ("<span class=\"math inline\">\\(m^{1+o(1)}\\)</span>", "<em>m</em><sup>1+<em>o</em>(1)</sup>")
         , ("<span class=\"math inline\">\\(\\mathbf{R}^3\\)</span>", "𝐑<sup>3</sup>")
         , ("<span class=\"math inline\">\\(\\mathcal{H}\\)</span>", "ℋ")
         , ("<span class=\"math inline\">\\(\\mathcal{M}\\)</span>", "𝓜")
         , ("<span class=\"math inline\">\\(\\mathcal{O}(100)\\)</span>", "𝒪(100)")
         , ("<span class=\"math inline\">\\(\\mathcal{O}(1)\\)</span>", "𝒪(1)")
         , ("<span class=\"math inline\">\\(\\mathcal{O}(1/\\sqrt{d})\\)</span>", "𝒪(1⁄√<em>d</em>)")
         , ("<span class=\"math inline\">\\(\\mathcal{O}(L^2)\\)</span>", "𝒪(<em>L</em><sup>2</sup>)")
         , ("<span class=\"math inline\">\\(\\mathcal{O}(L\\log(L))\\)</span>", "𝒪(log <em>L</em>)")
         , ("<span class=\"math inline\">\\(\\mathcal{O}(L\\sqrt{L})\\)</span>", "𝒪(√<em>L</em>)")
         , ("<span class=\"math inline\">\\(\\mathcal{O}(n\\log n)\\)</span>", "𝒪(<em>n</em> log <em>n</em>)")
         , ("<span class=\"math inline\">\\(\\mathsf L = \\mathsf P\\)</span>", "<strong>L</strong> = <strong>P</strong>")
         , ("<span class=\"math inline\">\\(\\mathsf{TC}^0\\)</span>", "<strong>TC</strong><sup>0</sup>")
         , ("<span class=\"math inline\">\\(\\mathtt{VC}(\\mathcal{H})\\)</span>", "<strong>VC</strong>(ℋ)")
         , ("<span class=\"math inline\">\\(\\mu\\)</span>", "μ")
         , ("<span class=\"math inline\">\\mu</span>", "μ")
         , ("<span class=\"math inline\">\\(n^{-1/2}\\)</span>", "<em>n</em><sup>−1⁄2</sup>")
         , ("<span class=\"math inline\">\\(n^{1/2}\\)</span>", "<em>n</em><sup>1⁄2</sup>")
         , ("<span class=\"math inline\">\\(n^{1/3}\\)</span>", "<em>n</em><sup>1⁄3</sup>")
         , ("<span class=\"math inline\">\\(n^{1/4}\\)</span>", "<em>n</em><sup>1⁄4</sup>")
         , ("<span class=\"math inline\">\\(n^{-1}\\)</span>", "<em>n</em><sup>−1</sup>")
         , ("<span class=\"math inline\">\\(n^3{(n^2)}^n\\)</span>", "<em>n</em><sup>3</sup>(<em>n</em><sup>3</sup>)<sup><em>n</em></sup>")
         , ("<span class=\"math inline\">\\(\\nabla \\log p_t\\)</span>", "∇ log <em>p<sub>t</sub></em>")
         , ("<span class=\"math inline\">\\(N_A \\ll N_B\\)</span>", "<em>N<sub>A</sub></em> ≪ <em>N<sub>B</sub></em>")
         , ("<span class=\"math inline\">\\(n^{-\\alpha}\\)</span>", "<em>n</em><sup>−α</sup>")
         , ("<span class=\"math inline\">\\(N_A/(N_A+N_B)\\)</span>", "<em>N<sub>a</sub></em> / (<em>N<sub>A</sub></em> + <em>N<sub>B</sub></em>)")
         , ("<span class=\"math inline\">\\(n \\approx d^p\\)</span>", "<em>n</em> ≈ <em>d<sup>p</sup></em>")
         , ("<span class=\"math inline\">\\(n \\approx d\\)</span>", "<em>n</em> ≈ <em>d</em>")
         , ("<span class=\"math inline\">\\(N_A\\)</span>", "<em>N<sub>A</sub></em>")
         , ("<span class=\"math inline\">\\(n^{-\beta}\\)</span>", "<em>n<sup>−β</sup></em>")
         , ("<span class=\"math inline\">\\(N_B\\)</span>", "<em>N<sub>B</sub></em>")
         , ("<span class=\"math inline\">\\(n^{n^{n^{n^{n^n}}}}\\)</span>", "<em>n</em><sup><em>n</em><sup><em>n</em><sup><em>n</em><sup><em>n</em><sup><em>n</em></sup></sup></sup></sup></sup>")
         , ("<span class=\"math inline\">\\(n^{O(k)}\\)</span>", "<em>n</em><sup>𝒪(<em>k</em>)</sup>")
         , ("<span class=\"math inline\">\\(n\\)</span>", "<em>n</em>")
         , ("<span class=\"math inline\">\\(N\\)</span>", "<em>N</em>")
         , ("<span class=\"math inline\">\\(N \\times T\\)</span>", "<em>N</em> × <em>T</em>")
         , ("<span class=\"math inline\">\\(\\nu\\)</span>", "ν")
         , ("<span class=\"math inline\">\\(O(1)\\)</span>", "𝒪(1)")
         , ("<span class=\"math inline\">\\(O(C)\\)</span>", "𝒪(<em>C</em>)")
         , ("<span class=\"math inline\">\\(O(D^3)\\)</span>", "𝒪(<em>D</em><sup>3</sup>)")
         , ("<span class=\"math inline\">\\(O(<em>n</em> \\log n)\\)</span>", "𝒪(<em>n</em> log <em>n</em>)")
         , ("<span class=\"math inline\">\\(O(<em>n</em><sup>2</sup>)\\)</span>", "𝒪(<em>n</em><sup>2</sup>)")
         , ("<span class=\"math inline\">\\(O(K^2 \\log T)\\)</span>", "𝒪(<em>K</em><sup>2</sup> log <em>T</em>)")
         , ("<span class=\"math inline\">\\(O(k\\cdot n\\log (n/k))\\)</span>", " 𝒪(<em>k</em> × log(<em>n</em>⁄<em>k</em>))")
         , ("<span class=\"math inline\">\\(O(K \\log T + K^2 \\log \\log T)\\)</span>", "𝒪(<em>K</em> log <em>T</em> + <em>K</em><sup>2</sup> log log <em>T</em>)")
         , ("<span class=\"math inline\">\\(O(L(\\log L)^{2})\\)</span>", "𝒪(<em>L</em>(log <em>L</em>)<sup>2</sup>)")
         , ("<span class=\"math inline\">\\(O(\\log n)\\)</span>", "𝒪(log <em>n</em>)")
         , ("<span class=\"math inline\">\\(O(\\log T)\\)</span>", "𝒪(log <em>T</em>)")
         , ("<span class=\"math inline\">\\(O(n^2 \\log T)\\)</span>", "𝒪(<em>n</em><sup>2</sup> log<em>T</em>")
         , ("<span class=\"math inline\">\\(O(n^2)\\)</span>", "𝒪(<em>n</em><sup>2</sup>)")
         , ("<span class=\"math inline\">\\(O(N^2)\\)</span>", "𝒪(<em>n</em><sup>2</sup>)")
         , ("𝒪(n^2)", "𝒪(<em>n</em><sup>2)")
         , ("<span class=\"math inline\">\\(O(n \\sqrt{n})\\)</span>", "𝒪(<em>n</em> √<em>n</em>)")
         , ("<span class=\"math inline\">\\(\\operatorname{bessel0}(10)\\approx \\frac{\\sin(10)+\\cos(10)}{\\sqrt{\\pi x}}\\)</span>", "<code>bessel0(<em>x</em>) ≈ sin(<em>x</em>)+cos(<em>x</em>) / √π<em>x</em>")
         , ("<span class=\"math inline\">\\(O\\sqrt{n})\\)</span>", "𝒪(√<em>n</em>)")
         , ("<span class=\\\"math inline\\\">\\\\(O(\\\\sqrt{n})\\\\)</span>", "𝒪(√<em>n</em>)")
         , ("<span class=\"math inline\">\\(O(\\sqrt{nT})\\)</span>", "𝒪(√<em>nT</em>)")
         , ("<span class=\"math inline\">\\(O(\\sqrt{T})\\)</span>", "𝒪(√<em>T</em>)")
         , ("<span class=\"math inline\">\\(O(l)\\)</span>", "𝒪(<em>l</em>)")
         , ("<span class=\"math inline\">\\(O(l^2)\\)</span>", "𝒪(<em>l</em><sup>2</sup></em>")
         , ("<span class=\"math inline\">\\(O(l \\log_{2} l)\\)</span>", "𝒪(<em>l</em> log<sub>2</sub> <em>l</em>")
         , ("<span class=\"math inline\">\\(o(T)\\)</span>", "<em>o</em>(<em>T</em>)")
         , ("<span class=\"math inline\">\\(\\partial_t u = \\Delta u + B(u,u)\\)</span>", "∂<sub><em>t</em></sub><em>u</em> = Δ<em>u</em> + <em>B</em>(<em>u</em>, <em>u</em>)")
         , ("<span class=\"math inline\">\\(\\partial_t u = \\Delta u + \\tilde B(u,u)\\)</span>", "∂<sub><em>t</em></sub><em>u</em> = Δ<em>u</em> + <em>B̃</em>(<em>u</em>, <em>u</em>)")
         , ("<span class=\"math inline\">\\(P_B(f\\mid S)\\)</span>", "<em>P</em><sub><em>b</em></sub>(<em>f</em>|<em>S</em>)")
         , ("<span class=\"math inline\">\\(\\perp\\)</span>", "⟂")
         , ("<span class=\"math inline\">\\(^{\\perp}\\)</span>", "<sup>⟂</sup>")
         , ("<span class=\"math inline\">\\(\\Phi(10)\\)</span>", "Φ(<em>x</em>)")
         , ("<span class=\"math inline\">\\(\\pi_1\\)</span>", "π<sub>1</sub>")
         , ("<span class=\"math inline\">\\(\\pi\\)</span>", "π")
         , ("<span class=\"math inline\">\\(\\pi^*\\)</span>", "π<sup>✱</sup>")
         , ("<span class=\"math inline\">\\(\\Pr(\text{text} | \\alpha)\\)</span>", "Pr(text | α)")
         , ("<span class=\"math inline\">\\(P_{SGD}(f\\mid S)\\)</span>", "<em>P</em><sub><em>SGD</em></sub>(<em>f</em>|<em>S</em>)")
         , ("<span class=\"math inline\">\\(p_T\\)</span>", "<em>p<sub>T</sub></em>")
         , ("<span class=\"math inline\">\\(Q\\)</span>", "<em>Q</em>")
         , ("<span class=\"math inline\">\\(R^2\\)</span>", "<em>R</em><sup>2</sup>")
         , ("<span class=\"math inline\">\\({\\raise.17ex\\hbox{<span class=\"math inline\">~</span>}}\\)</span>", "~")
         , ("<span class=\"math inline\">\\(rho &gt; 0\\)</span>", "ρ > 0")
         , ("<span class=\"math inline\">\\(rho &lt; 0\\)</span>", "ρ < 0")
         , ("<span class=\"math inline\">\\(rho\\)</span>", "ρ")
         , ("<span class=\"math inline\">\\(\\rightarrow\\)</span>", "→")
         , ("<span class=\"math inline\">\\({\\rm M}_{\\odot}\\)</span>", "M<sub>☉</sub>")
         , ("<span class=\"math inline\">\\(_r\\)</span>", "<sub><em>r</em></sub>")
         , ("<span class=\"math inline\">\\(r \\to\\infty\\)</span>", "<em>r</em> → ∞")
         , ("<span class=\"math inline\">\\(S&#39;\\)</span>", "<em>S</em>′")
         , ("<span class=\"math inline\">\\(S&#39; \\subset S\\)</span>", "<em>S</em>′ ⊂ <em>S</em>")
         , ("<span class=\"math inline\">\\(S^3\\)</span>", "<em>S</em><sup>3</sup>")
         , ("<span class=\"math inline\">\\(S^</em>(0.8)\\)</span>", "<em>S</em><sup>✱</sup>(0.8)")
         , ("<span class=\"math inline\">\\(S^</em>(0)\\)</span>", "<em>S</em><sup>✱</sup>(0)")
         , ("<span class=\"math inline\">\\(S^<em>(0)\\)</span>", "<em>S</em><sup>✱</sup>(0)")
         , ("<span class=\"math inline\">\\({\\sim}0.02 {\\rm M}_{\\mathrm{Ceres}}\\)</span>", "~0.02M<sub><a href=\"https://en.wikipedia.org/wiki/Ceres_(dwarf_planet)\">Ceres</a></sub>")
         , ("<span class=\"math inline\">\\({\\sim} 0.3 {\\rm M}_{\\odot}\\)</span>", "~0.3M<sub>☉</sub>")
         , ("<span class=\"math inline\">\\(\\sim 10^3\\)</span>", "~10<sup>3</sup>")
         , ("<span class=\"math inline\">\\(\\sim5\\%\\)</span>", "~5%")
         , ("<span class=\"math inline\">\\(\\sim 6\\)</span>", "~6")
         , ("<span class=\"math inline\">\\(\\sim\\)</span>", "~")
         , ("<span class=\"math inline\">\\({\\sim}\\)</span>", "~")
         , ("<span class=\"math inline\">\\(\\sin\\Theta\\)</span>", "sinΘ")
         , (" <span class=\"math inline\">\\(-\\)</span> ", "—")
         , ("<span class=\"math inline\">\\(\\sqrt{H}\\)</span>", "√<em>H</em>")
         , ("<span class=\"math inline\">\\(\\sqrt{n/k}\\)</span>", "√<em>n</em>⁄<em>k</em>")
         , ("<span class=\"math inline\">\\(\\sqrt{T}\\)</span>", "√<em>T</em>")
         , ("<span class=\"math inline\">\\(tanh\\)</span>", "<em>tanh</em>")
         , ("<span class=\"math inline\">\\(TC^0\\)</span>", "<em>TC</em><sup>0</sup>")
         , ("<span class=\"math inline\">\\(\\textit{Embedded agents}\\)</span>", "<em>Embedded agents</em>")
         , ("<span class=\"math inline\">\\(\\textit{Magic}\\)</span>", "<em>Magic</em>")
         , ("<span class=\"math inline\">\\(\\textit{Magic: The Gathering}\\)</span>", "<em>Magic: The Gathering</em>")
         , ("<span class=\"math inline\">\\(\\textit{wirehead}\\)</span>", "<em>wirehead</em>")
         , ("<span class=\"math inline\">\\(\textit{zero-shot}\\)</span>", "<em>zero-shot</em>")
         , ("<span class=\"math inline\">\\(\\text{RL}^2\\)</span>", "RL<sup>2</sup>")
         , ("<span class=\"math inline\">\\(\\Theta_{\\mathcal{H}}\\)</span>", "Θ<sub>ℋ</sub>")
         , ("<span class=\"math inline\">\\(\\tilde B\\)</span>", "<em>B̃</em>")
         , ("<span class=\"math inline\">\\(\\tilde O(n^{-1/4})\\)</span>",         "1 − 𝑂̃(<em>n</em><sup>−1⁄4</sup>)")
         , ("<span class=\"math inline\">\\(\\tilde{O}(\\sqrt{H^3 SAT})\\)</span>", "𝒪(√<em>H</em><sup>3</sup><em>SAT</em>)")
         , ("<span class=\"math inline\">\\(\\times\\)</span>", "×")
         , ("<span class=\"math inline\">\times</span>", "×")
         , ("<span class=\"math inline\">\\(\\tt KRISSBERT\\)</span>", "<code>KRISSBERT</code>")
         , ("<span class=\"math inline\">\\(\\tt KRISS\\)</span>", "<code>KRISS</code>")
         , ("<span class=\"math inline\">\\(u \\mapsto y\\)</span>", "<em>u</em> ↦ <em>y</em>")
         , (" <span class=\"math inline\">\\(\\unicode{x2014}\\)</span> ", "—")
         , ("<span class=\"math inline\">\\(\\unicode{x2014}\\)</span>", "—")
         , ("<span class=\"math inline\">\\(\\varphi\\)</span>", "ϕ")
         , ("<span class=\"math inline\">\\(W \\in {\\mathbb R}^{p \\times d}\\)</span>", "<em>W</em> ∈ ℝ<sup><em>p</em>×<em>d</em></sup>")
         , ("<span class=\"math inline\">\\(W z_i\\)</span>", "<em>Wz<sub>i</sub></em>")
         , ("<span class=\"math inline\">\\(X_1,\\ldots,X_p\\)</span>", "<em>X</em><sub>1</sub>,...,<em>X</em><sub><em>p</em></sub>")
         , ("<span class=\"math inline\">\\(x&#39;(t) = Ax(t) + Bu(t), y(t) = Cx(t) + Du(t)\\)</span>", "<em>x&#39;(t)</em> = <em>Ax(t)</em> + <em>Bu(t)</em>, <em>y(t)</em> = <em>Cx(t)</em> + <em>Du(t)</em>")
         , ("<span class=\"math inline\">\\((x, f(10))\\)</span>", "(<em>x</em>, <em>f</em>(10))")
         , ("<span class=\"math inline\">\\(x_i \\in {\\mathbb R}^p\\)</span>", "<em>x<sub>i</sub></em> ∈ ℝ<sup><em>p</em></sup>")
         , ("<span class=\"math inline\">\\(x_i = \\Sigma^{1/2} z_i\\)</span>", "<em>x<sub>i</sub></em> = ∑<sup>1⁄2</sup><em>z<sub>i</sub></em>")
         , ("<span class=\"math inline\">\\(x_i = \\varphi(W z_i)\\)</span>", "<em>x<sub>i</sub></em> = ϕ(<em>Wz<sub>i</sub></em>")
         , ("<span class=\"math inline\">\\(x &lt; 1\\)</span>", "<em>x</em> &lt; 1")
         , ("<span class=\"math inline\">\\(x\\mathbf{1}_{x&gt;0}\\)</span>", "<em>x</em><strong>1</strong><sub><em>x</em>&gt;0</sub>")
         , ("<span class=\"math inline\">\\(x\\Phi(10)\\)</span>", "<em>x</em>Φ(<em>x</em>)")
         , ("<span class=\"math inline\">\\(z=0\\)</span>", "<em>z</em> = 0")
         , ("<span class=\"math inline\">\\(z_i \\in {\\mathbb R}^d\\)</span>", "<em>z<sub>i</sub></em> ∈ ℝ<sup><em>d</em></sup>")
         , ("<span class=\"math inline\">\\(z_i \\in {\\mathbb R}^p\\)</span>", "<em>z<sub>i</sub></em> ∈ ℝ<sup><em>p</em></sup>")
         , ("<span class=\"math inline\">\\(μ\\)</span>", "𝜇")
         , ("<span class=\"texhtml \">2 + 3<i>i</i></span>", "2 + 3<em>i</em>")
         , ("<span class=\"texhtml \">ℂ</span>", "ℂ")
         , ("<span class=\"texhtml \"><i>a</i> + <i>b i</i></span>", "<em>a</em> + <em>b i</em>")
         , ("<span class=\"texhtml \"><i>X</i> = exp(<i>Y</i>)</span>", "<em>X</em> = exp(<em>Y</em>)")
         , ("<span class=\"texhtml \"><i>x</i><sup>2</sup> + 1 = 0</span>", "<em>x</em><sup>2</sup> + 1 = 0")
         , ("<span class=\"texhtml \"><i>Y</i> = ln(<i>X</i>)</span>", "<em>Y</em> = ln(<em>X</em>)")
         , ("<span class=\"texhtml \">\\mathcal{O}(log <i>n</i>)</span>", "𝒪(log <em>n</em>)")
         , ("<span class=\"texhtml \">\\mathrm{sinc}(ax)</span>", "sinc(<em>ax</em>)")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">a</span>", "<em>a</em>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">b</span>", "<em>b</em>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">c</span>", "<em>c</em>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">i</span>", "<em>i</em>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">n</span>", "<em>n</em>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">p</span>", "<em>p</em>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\"><strong>c</strong></span>", "<strong><em>c</em></strong>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">x</span>", "<em>x</em>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">Y</span>", "<em>Y</em>")
         , ("<span class=\"texhtml mvar\" style=\"font-style:italic\">π</span>", "<em>π</em>")
         , ("<span class=\"texhtml \">O(log <i>n</i>)</span>", "𝒪(log <em>n</em>)")
         , ("<span class=\"texhtml \"><strong>C</strong></span>", "<strong>C</strong>")
         , ("<span><span class=\"texhtml mvar\" style=\"font-style:italic\">π</span></span>", "<em>π</em>")
         , ("<span></span>-greedy", "ε-greedy")
         , ("<sup>*</sup>", "<sup>✱</sup>")
         , (" TD()", " TD(λ)")

           -- rest:
         , ("(PsycINFO Database Record", "")
         , ("</p> <p>. ", ".</p> <p>")
         , ("</p><p>", "</p> <p>")
         , ("</li> <li>", "</li>\n<li>")
         , ("</p> <figure>", "</p>\n<figure>")
         , ("</figure> <p>", "</figure>\n<p>")
         , ("/><figcaption", "/>\n    <figcaption")
         , ("</p> <table>", "</p>\n<table>")
         , ("</table> <p>", "</table>\n<p>")
         , ("</p> <div", "</p>\n<div")
         , ("<p><strong>Abstract</strong>:T", "<p>T")
         , ("<strong>ABSTRACT</strong>: ", "")
         , ("<strong>Abstract</strong>: <p>", "<p>")
         , ("<p><strong>ABSTRACT</strong><p>", "")
         , ("<p><strong>Abstract</strong><p>", "")
         , (":</strong>", "</strong>:")
         , (":</strong></p> <p>", "</strong>: ")
         , (" :</strong>", "</strong>:")
         , (" </sec>", "")
         , ("   <title/>    <p>", "<p>")
         , ("  <p>", "<p>")
         , ("I(2)", "I<sup>2</sup>")
         , (" h2",     " <em>h</em><sup>2</sup>")
         , ("h(2)",    "<em>h</em><sup>2</sup>")
         , ("</h2>", "</strong></p>")
         , ("<h2>", "<p><strong>")
         , ("</h3>", "</strong></p>")
         , ("<h3>", "<p><strong>")
         , ("</p>\\n<p>", "</p> <p>")
         , ("<br /></p>", "</p>")
         , ("<br /><br />", "</p> <p>")
         , ("Alzheimer9", "Alzheimer'")
         , ("<br /> <br />", "<br />")
         , ("1.<p>", "<p>")
         , ("<li> <p>➢ ", "<li>")
         , ("<p><ul></p>", "<ul>")
         , ("</strong> :</p>", "</strong>:</p>")
         , ("</strong> :</p> <p>", "</strong>: ")
         , ("</strong> :</p>\n<p>", "</strong>: ")
         , ("<list list-type=\"bullet\">", "<ul>")
         , ("</list>", "</ul>")
         , ("</list-item>", "</li>")
         , ("<list-item>", "<li>")
         , ("<list list-type=\"simple\">", "<ul>")
         , ("<p> ", "<p>")
         , (" <p>", "<p>")
         , ("</p> ", "</p>")
         , (" </p>", "</p>")
         , ("</p><br />", "</p>")
         , ("</p> <br />", "</p>")
         , ("<p><br />", "<p>")
         , ("<p></p>", "")
         , ("<p></li> </ul> </p>", "</li> </ul>")
         , ("</li><br />", "</li>")
         , ("<jats:sec>", "")
         , ("<jats:sec>\n ", "")
         , ("</p>\n\n<jats:sec>\n<strong>", "</p> <p><strong>")
         , ("</p>\n \n <jats:sec><p>", "</p> <p>")
         , ("</p>\n \n <jats:sec>\n<p>", "</p> <p>")
         , ("<strong>Abstract</strong>\n <jats:sec>\n<p>", "<p>")
         , ("<sec>", "")
         , ("</sec>", "")
         , ("  </sec><br />  ", "")
         , ("<sec><br />    ", "")
         , ("</jats:sec>", "")
         , ("<jats:sec><br />", "")
         , ("</jats:sec><br />", "")
         , ("  </sec> <br />", "")
         , ("<sec id=\"sec001\">", "")
         , ("<sec id=\"sec002\">", "")
         , ("<sec id=\"sec003\">", "")
         , ("<sec id=\"sec004\">", "")
         , ("<sec id=\"sec005\">", "")
         , ("<sec id=\"sec006\">", "")
         , ("<sec id=\"sec007\">", "")
         , ("<sec id=\"sec008\">", "")
         , ("<sec id=\"sec009\">", "")
         , ("<sec id=\"sec010\">", "")
         , ("<sec id=\"english\">", "")
         , ("<sec id=\"st1\">", "")
         , ("<sec id=\"st2\">", "")
         , ("<sec id=\"st3\">", "")
         , ("<sec id=\"sb1a\">", "")
         , ("<sec id=\"sb1b\">", "")
         , ("<sec id=\"sb1c\">", "")
         , ("<sec id=\"sb1d\">", "")
         , ("<sec id=\"sb1e\">", "")
         , ("<sec sec-type=\"headed\">", "")
         , ("<p><sec sec-type=\"headed\"></p>", "")
         , ("<strong>Abstract</strong>\n<jats:sec>\n<strong>", "<p><strong>")
         , ("</strong></p>    <p>", "</strong> ")
         , ("</title>", "</strong>:</p>")
         , ("<title/>", "")
         , ("<title>", "<p><strong>")
         , ("</title><br />", "</title>")
         , ("<p>\n\n", "<p>")
         , ("<br></p>", "</p>")
         , ("\n<br />\n", "")
         , ("<br /><p>", "<p>")
         , ("non–", "non-")
         , ("---", "&mdash;")
         , (" - ", "—")
         , (" — ", "—")
         , (" -- ", "—") -- eg. 'Darknet Data Mining -- A Canadian Cyber-crime Perspective'
         , (" statistically insignificant", " non-statistically-significant")
         , ("was significantly diminished", "was statistically-significantly diminished")
         , ("decreased significantly", "decreased statistically-significantly")
         , ("is significantly better than", "is statistically-significantly better than")
         , (" significant increase", " statistically-significant increase")
         , (" significantly less", " statistically-significantly less")
         , (" significantly more", " statistically-significantly more")
         , ("boundary of significance", "boundary of statistical-significance")
         , ("robustly significant", "robustly statistically-significant")
         , (" significant trend", " statistically-significant trend")
         , (" non-significant trend", " non-statistically-significant trend")
         , (" significant difference", " statistically-significant difference")
         , (" significant genetic correlation", " statistically-significant genetic correlation")
         , (" significant allele-phenotype associations", " statistically-significant allele-phenotype associations")
         , (" significant association", " statistically-significant association")
         , (" significant correlation", " statistically-significant correlation")
         , ("the significant SNPs", "the statistically-significant SNPs")
         , (" significantly associated", " statistically-significantly associated")
         , (" significantly correlated", " statistically-significantly correlated")
         , (" significantly higher (", " statistically-significantly higher (")
         , (" significant interaction effect", "  statistically-significant interaction effect")
         , (" significant effect", " statistically-significant effect")
         , (" significance testing", " statistical-significance testing")
         , ("nominally significant", "nominally statistically-significant")
         , (" nonsignificant result", " non-statistically-significant result")
         , (" significant excess", " statistically-significant excess")
         , (" significantly enriched", " statistically-significantly enriched")
         , (" significant at the ", " statistically-significant at the ")
         , ("statistically significant", "statistically-significant")
         , ("genome-wide significance", "genome-wide statistical-significance")
         , ("genome-wide significant", "genome-wide statistically-significant")
         , ("statistical significance", "statistical-significance")
         , ("clinical significance", "clinical-significance")
         , ("clinically significant", "clinically-significant")
         , ("<p><strong>Significance Statement</strong></p>\n<p>", "<p><strong>Significance Statement</strong>: ")
         , (". <strong>Conclusion</strong>: ", ".</p> <p><strong>Conclusio</strong>: ")
         , (". <strong>Conclusions</strong>: ", ".</p> <p><strong>Conclusion</strong>: ")
         , ("<strong>Conclusions & Significance</strong>", "<strong>Conclusion</strong>")
         , ("<strong>Conclusions and Significance</strong>", "<strong>Conclusion</strong>")
         , ("<strong>Conclusions</strong>\n<p>", "<p><strong>Conclusion</strong>: ")
         , ("<p>Conclusions: ", "<p><strong>Conclusion</strong>: ")
         , ("\n <strong>Conclusion</strong>\n<p>", "<p><strong>Conclusion</strong>: ")
         , (". <strong>Results</strong>: ", ".</p> <p><strong>Results</strong>: ")
         , ("\n <strong>Results</strong>\n<p>", "<p><strong>Results</strong>: ")
         , ("<p>Results: ", "<p><strong>Results</strong>: ")
         , ("<p>Aims: ", "<p><strong>Aims</strong>: ")
         , ("<p>Background. ", "<p><strong>Background</strong>: ")
         , ("<strong>Background</strong>\n<p>", "<p><strong>Background</strong>: ")
         , ("<p>Background: ", "<p><strong>Background</strong>: ")
         , (" Interpretation. ", "</p> <p><strong>Interpretation</strong>: ")
         , (" Findings. ", "</p> <p><strong>Results</strong>: ")
         , ("<strong>Methods</strong>\n<p>", "<p><strong>Method</strong>: ")
         , (" Methods. ", "</p> <p><strong>Method</strong>: ")
         , (". <strong>Methods</strong>: ", ".</p> <p><strong>Method</strong>: ")
         , (" \n <strong>Methods</strong>\n<p>", "<p><strong>Method</strong>: ")
         , ("<p>Methods: ", "<p><strong>Method</strong>: ")
         , ("<p>Outcomes: ", "<p><strong>Outcomes</strong>: ")
         , ("<p>Interpretation: ", "<p><strong>Interpretation</strong>: ")
         , ("<p>Funding: ", "<p><strong>Funding</strong>: ")
         , (" N1 =", " <em>n</em><sub>1</sub> =")
         , (" N2 =", " <em>n</em><sub>2</sub> =")
         , ("(N1 =", "(<em>n</em><sub>1</sub> =")
         , ("(N2 =", "(<em>n</em><sub>2</sub> =")
         , ("M full-scale", "M<sub>full-scale</sub>")
         , ("M age", "M<sub>age</sub>")
         , ("( <em>n</em> =", "(<em>n</em> =")
         , ("<em>N</em> =", "<em>n</em> =")
         , ("N = ", "<em>N</em> = ")
         , ("n = ", "<em>n</em> = ")
         , (" (x) ",   " (10) ")
         , (" (ix) ",  " (9) ")
         , (" (viii) "," (8) ")
         , (" (vii) ", " (7) ")
         , (" (vi) ",  " (6) ")
         , (" (v) ",   " (5) ")
         , (" (iv) ",  " (4) ")
         , (" (iii) ", " (3) ")
         , (" (ii) ",  " (2) ")
         , (" (i) ",   " (1) ")
         , ("(i)",     "(1)")
         , (" i)",     " (1)")
         , ("(ii)",    "(2)")
         , (" ii)",    " (2)")
         , ("(iii)",   "(3)")
         , (" iii)",   " (3)")
         , ("(iv)",    "(4)")
         , (" iv)",    " (4)")
         , ("(v)",     "(5)")
         , (" v)",     " (5)")
         , ("(vi)",    "(6)")
         , (" vi)",    " (6)")
         , ("(vii)",   "(7)")
         , (" vii)",   " (7)")
         , ("(viii)",  "(8)")
         , (" viii)",  " (8)")
         , ("(ix)",    "(9)")
         , (" ix)",    " (9)")
         , ("(x)",     "(10)")
         , (" x)",     " (10)")
         , (" a) ", " (1) ")
         , (" b) ", " (2) ")
         , (" c) ", " (3) ")
         , (" d) ", " (4) ")
         , (" e) ", " (5) ")
         , (" f) ", " (6) ")
         , (" h) ", " (7) ")
         -- i excluded due to ambiguity with Roman numeral i/ii/iii etc numbering
         , (" j) ", " (9) ")
         , (" k) ", " (10) ")
         , (" =  ", " = ")
         , ("</strong>\n<p>", "</strong>: ")
         , ("< /b>", "</strong>")
         , ("<b>", "<strong>")
         , ("</b>", "</strong>")
         , ("<jats:sec><strong>", "<strong>")
         , ("<jats:title>Abstract</jats:title><br />               ", "")
         , ("</jats:p>", "</p>")
         , ("< sub>", "<sub>")
         , ("<jats:sub>", "<sub>")
         , ("</jats:sub>", "</sub>")
         , ("<jats:sup>", "<sup>")
         , ("</jats:sup>", "</sup>")
         , ("<jats:title content-type=\"abstract-subheading\">", "<strong>")
         , ("<jats:title>", "<strong>")
         , ("</jats:title>", "</strong>")
         , (".</jats:title>", "</strong>")
         , ("<jats:p xml:lang=\"en\">", "<p>")
         , ("<jats:p>", "<p>")
         , ("</Emphasis>", "</em>")
         , ("<Emphasis Type=\"Italic\">", "<em>")
         , (" <i> </i>", " ") -- Wikipedia {{ety}} weirdness, but just in Ancient Greek instances?
         , ("<jats:italics>", "<em>")
         , ("</jats:italics>", "</em>")
         , ("<jats:italic>", "<em>")
         , ("</jats:italic>", "</em>")
         , ("<italic>", "<em>")
         , ("</ italic>", "</em>")
         , ("< /italic>", "</em>")
         , ("</italic>", "</em>")
         , ("< /i>", "</i>")
         , ("<jats:bold>", "<strong>")
         , ("<bold>", "<strong>")
         , ("</jats:bold>", "</strong>")
         , ("</bold>", "</strong>")
         , ("<jats:title>Abstract</jats:title>\n\t  <jats:p>", "<p>")
         , ("<jats:title>Abstract</jats:title><jats:p>The</jats:p>", "")
         , ("<strong>AUTHOR SUMMARY</strong>", "<strong>Author Summary</strong>")
         , ("<p><strong>Author Summary</strong></p> <p>", "<p><strong>Author Summary</strong>: ")
         , ("<p><strong>Author Summary</strong></p>\n<p>", "<p><strong>Author Summary</strong>: ")
         , ("<strong>Author Summary</strong> :", "<strong>Author Summary</strong>:")
         , ("strong>Author summary</strong>", "strong>Author Summary</strong>")
         , ("<jats:title>SUMMARY</jats:title>", "")
         , ("<jats:title>Summary</jats:title>", "")
         , ("</abstract>", "")
         , ("</strong><p>", "</strong>: <p>")
         , ("<abstract abstract-type=\"editor\">", "")
         , ("<abstract abstract-type=\"summary\">", "")
         , ("<abstract abstract-type=\"summary\"><br />", "")
         , ("<abstract abstract-type=\"synopsis\">", "")
         , ("<abstract abstract-type=\"toc\">", "")
         , ("<abstract>", "")
         , ("<abstract>\n  ", "")
         , ("<h3>ABSTRACT</h3>", "")
         , ("<h3>Abstract:</h3>", "")
         , ("<h3>Abstract</h3>", "")
         , ("<h3>SUMMARY</h3>", "")
         , ("<h3>Summary/Abstract</h3>", "")
         , ("<h3>Summary</h3>", "")
         , ("<p></abstract></p>", "")
         , ("<p><abstract abstract-type=\"short\"></p>", "")
         , ("<p><strong>Abstract</strong>: ", "<p>")
         , ("<p><strong>Abstract</strong>: <strong>Objective</strong>: ", "<p><strong>Objective</strong>: ")
         , ("<p><strong>Abstract</strong></p>", "")
         , ("<p><strong>\nAuthor summary\n</strong>:</p>\n<p>\n", "<p><strong>\nAuthor summary\n</strong>: ")
         , ("<p>Abstract. ", "<p>")
         , ("<strong>ABSTRACT</strong><br />              <p>", "<p>")
         , ("<strong>ABSTRACT</strong><br />", "")
         , ("<strong>Abstract</jats:title>", "")
         , ("<strong>Abstract</strong>:        ", "")
         , ("<strong>Abstract</strong>: ", "")
         , ("<strong>Abstract</strong>:<p>", "<p>")
         , ("<strong>Abstract</strong><br />", "")
         , ("<strong>Abstract</strong>\n \n ", "")
         , ("<strong>Abstract</strong>\n<p>", "<p>")
         , ("<strong>SUMMARY</jats:title>", "")
         , ("\n</abstract>", "")
         , ("▪ Abstract", "")
         , ("<strong>AIM:</strong>", "<strong>Aim</strong>:")
         , ("<strong>METHODS:</strong>", "<strong>Method</strong>:")
         , ("<strong>RESULTS:</strong>", "<strong>Results</strong>:")
         , ("<strong>CONCLUSION:</strong>", "<strong>Conclusion</strong>:")
         , ("<strong>AIM</strong>:", "<strong>Aim</strong>:")
         , ("<strong>METHODS</strong>:", "<strong>Method</strong>:")
         , ("<strong>RESULTS</strong>:", "<strong>Results</strong>:")
         , ("<strong>CONCLUSION</strong>:", "<strong>Conclusion</strong>:")
         , ("\nHighlights: ", "\n<strong>Highlights</strong>: ")
         , ("\nBackground: ", "\n<strong>Background</strong>: ")
         , ("\nAbstract: ", "\n<strong>Abstract</strong>: ")
         , ("<p>Abstract: ", "<p>")
         , ("\nContext: ", "\n<strong>Context</strong>: ")
         , ("<strong>Purpose</strong>\n<p>", "<p><strong>Purpose</strong>: ")
         , ("\nPurpose: ", "\n<strong>Purpose</strong>: ")
         , ("<p>Purpose. ", "\n<strong>Purpose</strong>: ")
         , ("\nRationale: ", "\n<strong>Rationale</strong>: ")
         , ("<strong>ANIMALS</strong>: ", "<strong>Animals</strong>: ")
         , ("<strong>OBJECTIVE</strong>: ", "<strong>Objective</strong>: ")
         , ("<strong>METHOD</strong>: ", "<strong>Method</strong>: ")
         , ("<strong>RESULTS</strong>: ", "<strong>Results</strong>: ")
         , ("<strong>CONCLUSIONS</strong>: ", "<strong>Conclusion</strong>: ")
         , ("<strong>CLINICAL RELEVANCE</strong>: ", "<strong>Clinical Relevance</strong>: ")
         , ("<strong>PROCEDURES</strong>: ", "<strong>Procedures</strong>: ")
         , ("</p><strong>Setting & Participants</strong>:<p>", "</p><p><strong>Setting & Participants</strong>: ")
         , ("<strong>OBJECTIVE</strong></p>\n", "<strong>Objective</strong>: ")
         , ("<strong>METHOD</strong></p>\n", "<strong>Method</strong>: ")
         , ("<strong>RESULTS</strong></p>\n", "<strong>Results</strong>: ")
         , ("<strong>CONCLUSIONS</strong></p>\n         ", "<strong>Conclusion</strong>: ")
         , ("<strong>CLINICAL RELEVANCE</strong></p>\n         ", "<strong>Clinical Relevance</strong>: ")
         , ("<p><strong>OBJECTIVE</strong></p>\n<p>", "<p><strong>Objective</strong>: ")
         , ("<p><strong>METHOD</strong></p>\n<p>", "<p><strong>Method</strong>: ")
         , ("<p><strong>RESULTS</strong></p>\n<p>", "<p><strong>Results</strong>: ")
         , ("<p><strong>CONCLUSIONS</strong></p>\n<p>         ", "<p><strong>Conclusion</strong>: ")
         , ("<p><strong>CLINICAL RELEVANCE</strong></p>\n<p>         ", "<p><strong>Clinical Relevance</strong>: ")
         , ("\nObjective: ", "\n<strong>Objective</strong>: ")
         , ("\nObjectives: ", "\n<strong>Objectives</strong>: ")
         , ("\nQuestion: ", "\n<strong>Question</strong>: ")
         , ("\nDescription: ", "\n<strong>Description</strong>: ")
         , ("Design and</p>\n<p><strong>Methods</strong>: ", "</p> <p><strong>Method</strong>: ")
         , ("\nDesign: ", "\n<strong>Method</strong>: ")
         , ("\nMethods: ", "\n<strong>Method</strong>: ")
         , ("\nDesign and Methods: ", "\n<strong>Method</strong>: ")
         , ("<strong>Materials and Methods</strong>", "<strong>Method</strong>")
         , ("<strong>Materials and methods</strong>", "<strong>Method</strong>")
         , ("\nSetting: ", "\n<strong>Setting</strong>: ")
         , ("\nParticipants: ", "\n<strong>Participants</strong>: ")
         , ("\nMeaning: ", "\n<strong>Meaning</strong>: ")
         , ("Setting and Participants", "Setting & Participants")
         , ("\nDesign, Setting, and Participants: ", "\n<strong>Methods, Setting, & Participants</strong>: ")
         , ("\nIntervention: ", "\n<strong>Intervention</strong>: ")
         , ("\nData Sources: ", "\n<strong>Data Sources</strong>: ")
         , ("\nMain Outcomes & Measures: ", "\n<strong>Main Outcomes & Measures</strong>: ")
         , ("<strong>Main Outcomes and Measures</strong", "<strong>Main Outcomes & Measures</strong")
         , ("\nMeasurements: ", "\n<strong>Measurements</strong>: ")
         , (". Results. ", ".</p> <p><strong>Results</strong>: ")
         , ("\nResults: ", "\n<strong>Results</strong>: ")
         , ("\nSignificance: ", "\n<strong>Significance</strong>: ")
         , (". Conclusions. ", ".</p> <p><strong>Conclusion</strong>: ")
         , ("\nConclusion: ", "\n<strong>Conclusion</strong>: ")
         , ("\nConclusions: ", "\n<strong>Conclusion</strong>: ")
         , ("\nConclusions & Relevance: ", "\n<strong>Conclusion & Relevance</strong>: ")
         , ("\nIntroduction: ", "\n<strong>Background</strong>: ")
         , ("\nTrial Registration: ", "\n<strong>Trial Registration</strong>: ")
         , ("<h3>Highlights</h3>\n<p>", "<p><strong>Highlights</strong>: ")
         , ("<h3>Background</h3>\n<p>", "<p><strong>Background</strong>: ")
         , ("<h3>Abstract</h3>\n<p>", "<p><strong>Abstract</strong>: ")
         , ("<h3>Context</h3>\n<p>", "<p><strong>Context</strong>: ")
         , ("<h3>Purpose</h3>\n<p>", "<p><strong>Purpose</strong>: ")
         , ("<h3>Rationale</h3>\n<p>", "<p><strong>Rationale</strong>: ")
         , ("<h3>Objective</h3>\n<p>", "<p><strong>Objective</strong>: ")
         , ("<h3>Objectives</h3>\n<p>", "<p><strong>Objectives</strong>: ")
         , ("<h3>Question</h3>\n<p>", "<p><strong>Question</strong>: ")
         , ("<h3>Description</h3>\n<p>", "<p><strong>Description</strong>: ")
         , ("<h3>Design</h3>\n<p>", "<p><strong>Method</strong>: ")
         , ("<h3>Methods</h3>\n<p>", "<p><strong>Method</strong>: ")
         , ("<h3>Setting</h3>\n<p>", "<p><strong>Setting</strong>: ")
         , ("<h3>Participants</h3>\n<p>", "<p><strong>Participants</strong>: ")
         , ("<h3>Meaning</h3>\n<p>", "<p><strong>Meaning</strong>: ")
         , ("<h3>Design, Setting, and Participants</h3>\n<p>", "<p><strong>Methods, Setting, & Participants</strong>: ")
         , ("<strong>Design, Setting, and Participants</strong>", "<strong>Methods, Setting, & Participants</strong>")
         , ("<h3>Intervention</h3>\n<p>", "<p><strong>Intervention</strong>: ")
         , ("<h3>Data Sources</h3>\n<p>", "<p><strong>Data Sources</strong>: ")
         , ("<h3>Main Outcomes & Measures</h3>\n<p>", "<p><strong>Main Outcomes & Measures</strong>: ")
         , ("<h3>Measurements</h3>\n<p>", "<p><strong>Measurements</strong>: ")
         , ("<h3>Results</h3>\n<p>", "<p><strong>Results</strong>: ")
         , ("<h3>Significance</h3>\n<p>", "<p><strong>Significance</strong>: ")
         , ("<h3>Conclusion</h3>\n<p>", "<p><strong>Conclusion</strong>: ")
         , ("<h3>Conclusions</h3>\n<p>", "<p><strong>Conclusion</strong>: ")
         , ("<h3>Conclusions & relevance</h3>\n<p>", "<p><strong>Conclusion & Relevance</strong>: ")
         , ("<h3>Conclusions & Relevance</h3>\n<p>", "<p><strong>Conclusion & Relevance</strong>: ")
         , ("<h3>Trial Registration</h3>\n<p>", "<p><strong>Trial Registration</strong>: ")
         , ("</h3><br />", "</h3>")
         , ("<br /><h3>", "<h3>")
         , ("<em>Background:</em>", "<strong>Background</strong>:")
         , ("<em>Objective:</em> ", "<strong>Objective</strong>:")
         , ("<em>Results:</em> ", "<strong>Results</strong>:")
         , ("<em>Conclusions:</em>", "<strong>Conclusion</strong>:")
         , ("<strong>Originality/value</strong>", "<strong>Conclusion</strong>:")
         , ("<strong>Conclusions and Relevance</strong>", "<strong>Conclusion</strong>:")
         , ("<strong>Conclusions and relevance</strong>", "<strong>Conclusion</strong>:")
         , ("\91Keywords: ", "\91<strong>Keywords</strong>: ")
         , ("&lt;/i&gt;&lt;/b&gt;", "</em>")
         , ("&lt;b&gt;&lt;i&gt;", "<em>")
         , ("acc.:", "accuracy:")
         , (" m/s", " m⁄s")
         , (" 1/100 ", " 1⁄100 ")
         , ("~1/250", "~1⁄250")
         , (" 2/3 ", " 2⁄3 ")
         , (" 3/3 ", " 3⁄3 ")
         , (" 1/10 ", " 1⁄10 ")
         , (" (4/8 ", " (4⁄8 ")
         , (" (5/8 ", " (5⁄8 ")
         , (" 1/2 ", " 1⁄2 ")
         , (" 1/3 ", " 1⁄3 ")
         , (" 1/4 ", " 1⁄4 ")
         , (" 4/5 ", " 4⁄5 ")
         , (" 5/8 ", " 5⁄8 ")
         , (" 5/9 ", " 5⁄9 ")
         , (" 6/13 ", " 6⁄13 ")
         , (" 7/13 ", " 7⁄13 ")
         , (" 8/13 ", " 8⁄13 ")
         , (" 9/13 ", " 9⁄13 ")
         , (" 15/16 ", " 15⁄16 ")
         , (" 5/16 ", " 5⁄16 ")
         , (" 15/20 ", " 15⁄20 ")
         , (" (23/96) ", " (23⁄96) ")
         , (" (24/50) ", " (24⁄50) ")
         , (" (30/96) ", " (30⁄96) ")
         , (" (35/96) ", " (35⁄96) ")
         , (" (39/50) ", " (39⁄50) ")
         , (" (41/50) ", " (41⁄50) ")
         , (" (43/50) ", " (43⁄50) ")
         , (" (48/96) ", " (48⁄96) ")
         , (" (50/96) ", " (50⁄96) ")
         , (" (6/96), ", " (6⁄96), ")
         , (" (68/96) ", " (68⁄96) ")
         , (" (90/96) ", " (90⁄96) ")
         , (" 11/90 ", " 11⁄90 ")
         , (" 33/96 ", " 33⁄96 ")
         , (" 42/50 ", " 42⁄50 ")
         , ("(11/31)", "(11⁄31)")
         , ("(9/11)", "(9⁄11)")
         , ("(2/7)", "(2⁄7)")
         , ("(28/31)", "(28⁄31)")
         , ("(9/10)", "(9⁄10)")
         , ("10(-10)", "10<sup>−10</sup>")
         , ("10(-11)", "10<sup>−11</sup>")
         , ("10(-13)", "10<sup>−13</sup>")
         , ("10(-15)", "10<sup>−15</sup>")
         , ("10(-19)", "10<sup>−19</sup>")
         , ("10(-26)", "10<sup>−26</sup>")
         , ("10(-3)", "10<sup>−3</sup>")
         , ("10(-4)", "10<sup>−4</sup>")
         , ("10(-5)", "10<sup>−5</sup>")
         , ("10(-6)", "10<sup>−6</sup>")
         , ("10(-7)", "10<sup>−7</sup>")
         , ("10(-8)", "10<sup>−8</sup>")
         , ("10(-9)", "10<sup>−9</sup>")
         , ("10(-)(3)", "10<sup>−3</sup>")
         , ("10(-)(4)", "10<sup>−4</sup>")
         , ("10(-)(5)", "10<sup>−5</sup>")
         , ("10(-)(6)", "10<sup>−6</sup>")
         , ("10(-)(7)", "10<sup>−7</sup>")
         , ("10(-)(8)", "10<sup>−8</sup>")
         , ("10(-)(9)", "10<sup>−9</sup>")
         , ("10(-)(10)", "10<sup>−10</sup>")
         , ("10(-16)", "10<sup>−16</sup>")
         , ("10(-33)", "10<sup>−33</sup>")
         , ("10(-14)", "10<sup>−14</sup>")
         , ("R (2) ", "R<sup>2</sup> ")
         , ("CO(2)", "CO<sub>2</sub>")
         , ("2^n-1",  "2<sup><em>n</em>−1</sup>")
         , ("2^n-1)", "2<sup><em>n</em>−1</sup>)")
         , ("d(-1)", "d<sup>−1</sup>")
         , ("min(-1)", "min<sup>−1</sup>")
         , (" = .",    " = 0.")
         , ("=−", " = −")
         , (" gf ", " <em>gf</em> ")
         , (" gc ", " <em>gc</em> ")
         , ("( g = ", "(<em>g</em> = ")
         , (" g-factor", " <em>g</em>-factor")
         , ("<i><em>h</em><sup>2</sup></i>", "<em>h</em><sup>2</sup>")
         , ("<i><em>h</em><sup>2</sup><sub>SNP</sub></i>", "<em>h</em><span class=\"subsup\"><sub>SNP</sub><sup>2</sup></span>")
         , ("h2SNP", "<em>h</em><span class=\"subsup\"><sub>SNP</sub><sup>2</sup></span>")
         , ("ηp2", "η<span class=\"subsup\"><sub>p</sub><sup>2</sup></span>")
         , ("h<sup>2</sup>", "<em>h</em><sup>2</sup>")
         , ("|rA|", "|r<sub>A</sub>|")
         , ("|rE|", "|r<sub>E</sub>|")
         , ("R <sup>2</sup>", "R<sup>2</sup>")
         , ("r g =", "<em>r<sub>g</sub></em> =")
         , ("<em>r</em> <sub>g</sub>", "<em>r</em><sub>g</sub>")
         , ("r(g)",    "<em>r</em><sub><em>g</em></sub>")
         , (" rg:", " <em>r</em><sub><em>g</em></sub>:")
         , (" rg ", " <em>r</em><sub><em>g</em></sub> ")
         , (" rg=", " <em>r</em><sub><em>g</em></sub> = ")
         , (" rg = ", " <em>r</em><sub><em>g</em></sub> = ")
         , ("(rg)", "(<em>r</em><sub><em>g</em></sub>)")
         , ("(rg", "(<em>r</em><sub><em>g</em></sub>")
         , ("|rg|=", "|<em>r</em><sub><em>g</em></sub>| = ")
         , ("U_d=", "<em>U<sub>d</sub></em>")
         , ("U_d&gt;", "<em>U<sub>d</sub></em> > ")
         , ("U_d>", "<em>U<sub>d</sub></em> > ")
         , ("U_d<", "<em>U<sub>d</sub></em> < ")
         , ("U_d&lt;", "<em>U<sub>d</sub></em> < ")
         , ("U_d~", "<em>U<sub>d</sub></em> ~ ")
         , ("-&gt;", "→")
         , (" ≥ .", " ≥ 0.")
         , (" r = ", " <em>r</em> = ")
         , (" r=", "<em>r</em> = ")
         , (" r>", "<em>r</em> > ")
         , (" r&gt;", "<em>r</em> &gt; ")
         , (" r<", " <em>r</em> < ")
         , ("r≥", "<em>r</em> ≥ ")
         , ("r≤", "<em>r</em> ≤ ")
         , ("≤n≤", " ≤ <em>n</em> ≤ ")
         , ("<var>", "<em>")
         , ("</var>", "</em>")
         , ("</monospace>", "</code>")
         , ("<monospace>", "<code>")
         , ("<code class=\"mw-highlight mw-highlight-lang-text mw-content-ltr\" dir=\"ltr\"", "<code")
         , ("<wbr />", "")
         , ("<wbr/>", "")
         , ("<wbr>", "")
         , ("<wbr />&#8203;", "")
         , ("<wbr></wbr>", "")
         , ("<wbr></wbr>\8203", "")
         , ("<abbr>", "<span>")
         , ("</abbr>", "</span>")
         , ("</a> .", "</a>.")
         , ("</jats:ext-link>", "</a>")
         , ("<jats:ext-link xmlns:xlink=\"http://www.w3.org/1999/xlink\" ext-link-type=\"uri\" xlink:href=", "<a href=")
         , ("<ext-link xlink:href=", "<a href=")
         , ("<ext-link ext-link-type=\"uri\"", "<a")
         , ("<ext-link ext-link-type=\"uri\" xlink:href=", "<a href=")
         , ("xlink:type=\"simple\"", "")
         , ("</ext-link>", "</a>")
         , ("beta=", "β = ")
         , (" = 0", " = 0")
         , ("cm3", "cm<sup>3</sup>")
         , (" cm(3)", " cm<sup>3</sup>")
         , ("R2 <", "R<sup>2</sup> <")
         , ("R2 ", "R<sup>2</sup> ")
         , (" R2", " R<sup>2</sup>")
         , (" R(2)", " R<sup>2</sup>")
         , ("top-k", "top-<em>k</em>")
         , ("Top-k", "Top-<em>k</em>")
         , ("Top-K", "Top-<em>k</em>")
         , (" z = ", " <em>z</em> = ")
         , ("z-score", "<em>z</em>-score")
         , ("Z-score", "<em>z</em>-score")
         , ("z-scores", "<em>z</em>-scores")
         , (" z-latent", "<em>z</em>-latent")
         , (" w-latent", "<em>w</em>-latent")
         , (" &lt; .0", " &lt; 0.0")
         , (" 5e-8/53", "(5 × 10<sup>−8</sup>) / 53") -- https://www.biorxiv.org/content/10.1101/2023.09.10.557084v1.full.pdf#page=2
         , (" p &amp;gt; ", " <em>p</em> &gt; ")
         , (" p &amp;lt; ", " <em>p</em> &lt; ")
         , ("<em>p<\\/em>=", "<em>p</em> = ")
         , ("P = ", "<em>p</em> = ")
         , ("P values", "<em>p</em>-values")
         , (" p value", " <em>p</em>-value")
         , (" P &lt; .", " <em>p</em> &lt; 0.")
         , (" P &lt;", " <em>p</em> &lt;")
         , (" P &lt;", " <em>p</em> &lt;")
         , ("≤p≤",     " ≤ <em>p</em> ≤ ")
         , (" d = ", " <em>d</em> = ")
         , ("( d = ", "(<em>d</em> = ")
         , ("(d = ", "(<em>d</em> = ")
         , ("(d < ", "(<em>d</em> < ")
         , ("(d > ", "(<em>d</em> > ")
         , ("(rs)", "(<em>r</em>s)")
         , ("(r=",     "(<em>r</em> = ")
         , ("(R=",     "(<em>r</em> = ")
         , ("(R = ",   "(<em>r</em> = ")
         , ("(r = ",   "(<em>r</em> = ")
         , ("(N = ",   "(<em>N</em> = ")
         , ("(n = ",   "(<em>n</em> = ")
         , ("(n=",     "(<em>n</em> = ")
         , ("(N=",     "(<em>N</em> = ")
         , ("(r&gt;", "9<em>r</em> &gt; ")
         , (" N ~ ",     " <em>n</em> ~ ")
         , ("( N = ", "(<em>n</em> = ")
         , ("( n = ", "(<em>n</em> = ")
         , ("( ns = ", "(<em>ns</em> = ")
         , ("( n = ", "(<em>n</em> = ")
         , ("n = ", "<em>n</em> = ")
         , ("(minimum p ", "(minimum <em>p</em> ")
         , ("<em>P</em> = 0", "<em>p</em> = 0")
         , ("(p = ", "(<em>p</em> = ")
         , (" p&lt;", " <em>p</em> < ")
         , (" p&gt;", " <em>p</em> > ")
         , (" p&gte;", " <em>p</em> ≥ ")
         , (" p&lte;", " <em>p</em> ≤ ")
         , (" P&lt;", " <em>p</em> < ")
         , (" P&gt;", " <em>p</em> > ")
         , (" P&gte;", " <em>p</em> ≥ ")
         , (" P&lte;", " <em>p</em> ≤ ")
         , ("<em>p</em> = .", "<em>p</em> = 0.")
         , ("<em>p</em> < .", "<em>p</em> < 0.")
         , (" n-vertex", " <em>n</em>-vertex")
         , ("(N~", "(<em>n</em> ≈ ")
         , (" n)", " <em>n</em>)")
         , (" n ", " <em>n</em> ")
         , ("(n ", "(<em>n</em> ") -- it's safe to replace 'n', but not 'N' because 'N' is used fairly often in chemistry for nitrogen, and chemical names often have parentheses, so we can't search-and-replace it in general
         , (" N=",     " <em>N</em> = ")
         , (" N = ",     " <em>N</em> = ")
         , (" n = ",     " <em>n</em> = ")
         , ("(p=",     "(<em>p</em> = ")
         , (" n=",     " <em>n</em> = ")
         , ("( n=", "( <em>n</em> = ")
         , ("Neffective", "<em>n</em><sub>effective</sub>")
         , ("Neffective=", "<em>n</em><sub>effective</sub> = ")
         , ("Padjusted", "<em>p</em><sub>adjusted</sub>")
         , ("Padjusted=", "<em>p</em><sub>adjusted</sub> = ")
         , (" n-gram", " <em>n</em>-gram")
         , (" N-gram", " <em>n</em>-gram")
         , (" k &gt; ", " <em>nk</em> &gt; ")
         , (" k > ", " <em>k</em> > ")
         , (" N &gt; ", " <em>n</em> &gt; ")
         , (" N > ", " <em>n</em> > ")
         , (" N<sub>effective</sub>", " <em>N<sub>effective</sub>")
         , (" N-weighted", " <em>N</em>-weighted")
         , (" n-back", " <em>n</em>-back")
         , ("N-back", "<em>N</em>-back")
         , ("(P &amp;lt;", "(<em>p</em> &lt;")
         , ("<em>p</em> &lt; .05", "<em>p</em> &lt; 0.05")
         , (" &gt; .05", " &gt; 0.05")
         , (" &gt; .01", " &gt; 0.01")
         , ("( <em>p</em>", "(<em>p</em>")
         , ("p = 0",   "<em>p</em> = 0")
         , (" P=",     " <em>p</em> = ")
         , (" P = ",   " <em>p</em> = ")
         , ("(P = ",   "(<em>p</em> = ")
         , (" p = ",   " <em>p</em> = ")
         , (" p=",     " <em>p</em> = ")
         , (" P<",     " <em>p</em> < ")
         , ("P ≤ ", "<em>p</em> ≤ ")
         , ("(P<",     "(<em>p</em> < ")
         , ("(<em>P</em> &lt;", "(<em>p</em> &lt;")
         , ("<span>0,1</span>^n", "{0,1}<sup><em>n</em></sup>")
         , (" n&lt;", " <em>n</em> &lt; ")
         , ("(n = ", "(<em>n</em> = ")
         , ("(n = ", "(<em>n</em> = ")
         , ("(p &lt; .", "(<em>p</em> &lt; 0.")
         , (" r ≥ 0.", " <em>r</em> ≥ 0.")
         , (" P(t) ", " <em>P(t)</em> ")
         , (" t ", " <em>t</em> ")
         , (" t test", " <em>t</em>-test")
         , ("(P≤", "(<em>p</em> ≤ ")
         , ("(P&lt;", "(<em>p</em> &lt; ")
         , ("(P&gt;", "(<em>p</em> &gt; ")
         , ("(P &lt;", "(<em>p</em> &lt;")
         , ("(P &gt;", "(<em>p</em> &gt;")
         , ("(p≤", "(<em>p</em> ≤ ")
         , ("&gt; &gt;0", "≫0")
         , ("(p&lt;", "(<em>p</em> &lt; ")
         , ("(p&gt;", "(<em>p</em> &gt; ")
         , ("(p &lt;", "(<em>p</em> &lt;")
         , ("(p &gt;", "(<em>p</em> &gt;")
         , (" p &lt;", " <em>p</em> &lt;")
         , (" p &gt;", " <em>p</em> &gt;")
         , (" P < ",   " <em>p</em> < ")
         , (" p < ",   " <em>p</em> < ")
         , (" p<",     " <em>p</em> < ")
         , (" p<.",    " <em>p</em> < 0.")
         , ("(P < 0.", "(<em>p</em> < 0.")
         , ("(P < .", "(<em>p</em> < 0.")
         , ("(P &lt; ", "(<em>p</em> <")
         , ("(P=",     "(<em>p</em> = ")
         , ("P-value", "<em>p</em>-value")
         , ("p-value", "<em>p</em>-value")
         , (" p-level ", " <em>p</em>-level ")
         , ("P for linear trend", "<em>p</em><sub>linear trend</sub>")
         , ("P for quadratic trend", "<em>p</em><sub>quadratic trend</sub>")
         , ("level-k ", "level-<em>k</em> ")
         , (" level-k", " level-<em>k</em>")
         , ("<em>b</em> = ", "β = ")
         , (" ", " ")
         , (" LaTeX", " <span class=\"logotype-latex\">L<span class=\"logotype-latex-a\">a</span>T<span class=\"logotype-latex-e\">e</span>X</span>")
         , (">LaTeX</a>", "><span class=\"logotype-latex\">L<span class=\"logotype-latex-a\">a</span>T<span class=\"logotype-latex-e\">e</span>X</span></a>")
         , (" TeX", " <span class=\"logotype-tex\">T<sub>e</sub>X</span>")
         , (">TeX</a>", "><span class=\"logotype-tex\">T<sub>e</sub>X</span></a>")
         , ("DALL-E", "DALL·E")
         -- many uses of 'approximately' are weasel-wordy which add nothing whatsoever semantically, so we can drop the '~':
         , ("~linearly", "linearly")
         , ("~every", "every")
         , ("~isometrically", "isometrically")
         , ("~solving", "solving")
         , ("~simulate", "simulate")
         , ("~this", "Approximately this")
         , ("~equivalent", "equivalent")
         , ("~the", "the")
         , ("~one ", "~1")
         , (" one tenth ", " 1⁄10<sup>th</sup>")
         , (" two thirds ", " 2⁄4<sup>rds</sup>")
         , ("~zero ", "~0")
         , ("~identical", "near-identical")
         , ("approximately ", "~")
         , ("Approximately ", "~")
         , ("approx ", "~")
         , ("∼", "~")
         , ("GxE", "G×E")
         , (" 10x", " 10×")
         , (" 100x", " 100×")
         , (" 2x", " 2×")
         , (" 3x", " 3×")
         , ("~twice", "~2×")
         , ("five times", "5×")
         , ("fifty-five years", "55 years")
         , ("Fifty-five years", "55 years")
         , ("<p> ", "<p>")
         , ("+/-", "±")
         , (" +- ", "±")
         , ("<sup>~</sup>", "~")
         , ("one-third", "1⁄3<sup>rd</sup>")
         , ("one-quarter", "1⁄4<sup>th</sup>")
         , ("three and a half", "3.5")
         , (" 11th", " 11<sup>th</sup>")
         , (" 12th", " 12<sup>th</sup>")
         , (" 13th", " 13<sup>th</sup>")
         , (" 14th", " 14<sup>th</sup>")
         , (" 15th", " 15<sup>th</sup>")
         , (" 16th", " 16<sup>th</sup>")
         , (" 17th", " 17<sup>th</sup>")
         , (" 18th", " 18<sup>th</sup>")
         , (" 19th", " 19<sup>th</sup>")
         , (" 20th", " 20<sup>th</sup>")
         , (" 21st", " 21<sup>st</sup>")
         , ("\"21st", "\"21<sup>st</sup>")
         , ("early-12th", "early-12<sup>th</sup>")
         , ("mid-21st", "mid-21<sup>st</sup>")
         , ("early-20th-century", "early-20<sup>th</sup>-century")
         , ("<i>25th", "<i>25<sup>th</sup>")
         , (">15th", ">15<sup>th</sup>")
         , ("mid-17th", "mid-17<sup>th</sup>")
         , ("mid-16th", "mid-16<sup>th</sup>")
         , (">21st", ">21<sup>st</sup>")
         , ("–19th", "–19<sup>th</sup>")
         , ("late-20th", "late-20<sup>th</sup>")
         , ("64,000th", "64,000<sup>th</sup>")
         , ("(5th", "(5<sup>th</sup>")
         , ("(12th", "(12<sup>th</sup>")
         , (" 14th ", " 14<sup>th</sup> ")
         , (" 22nd", " 22<sup>nd</sup>")
         , (" 30th", " 30<sup>th</sup>")
         , (" 50th", " 50<sup>th</sup>")
         , (" 1st", " 1<sup>st</sup>")
         , (" 2nd", " 2<sup>nd</sup>")
         , (" 3rd", " 3<sup>rd</sup>")
         , (" 5th", " 5<sup>th</sup>")
         , (" 4th", " 4<sup>th</sup>")
         , ("<code class=\"mw-highlight mw-highlight-lang-bash mw-content-ltr\" dir=\"ltr\">", "<code>")
         , ("ml-1", "ml<sup>−1</sup>")
         , ("10(9)", "10<sup>9</sup>")
         , ("(10(9))", "(10<sup>9</sup>)")
         , ("kg/m(2)", "kg⁄m<sup>2</sup>")
         , ("kg/m2", "kg⁄m<sup>2</sup>")
         , ("cd/m(2)", "cd/m<sup>2</sup>")
         , ("Cmax", "C<sub>max</sub>")
         , ("<small></small>", "")
         , ("Hazard Ratio", "hazard ratio")
         , ("tertile", "third")
         , (" et al ", " et al ") -- et al: try to ensure no linebreaking of citations
         , (" et al. ", " et al ")
         , (" et al., ", " et al ")
         , ("<em>i</em>.<em>e</em>.,", "ie.")
         , ("(ie, ", "(ie. ")
         , ("(ie ", "(ie. ")
         , ("(i.e.,", "(ie.")
         , ("(i.e.", "(ie.")
         , ("<em>e</em>.<em>g</em>.,", "eg.")
         , (" e.g.", " eg.")
         , ("(e.g.", "(eg.")
         , ("(eg ", "(eg. ")
         , (" eg ", " eg. ")
         , ("eg., ", "eg. ")
         , ("e.g., ", "eg. ")
         , ("e.g. ", "eg. ")
         , ("labell", "label")
         , ( "optimise", "optimize")
         , ("organise", "organize")
         , ("totall ", "total ")
         , ("minimis", "minimiz")
         , ("maximis", "maximiz")
         , ("heterogenous", "heterogeneous")
         , ("Streptococcus sanguis", "<em>Streptococcus sanguis</em>")
         , ("S. sanguis", "<em>S. sanguis</em>")
         , ("S. mutans", "<em>S. mutans</em>")
         , (" Streptococcus mutans", "<em>Streptococcus mutans</em>")
         , ("(Canis Familiaris)", "(<em>Canis Familiaris</em>)")
         , ("(Canis familiaris)", "(<em>Canis familiaris</em>)")
         , (" Canis familiaris", " <em>Canis familiaris</em>")
         , (" Escherichia coli", " <em>Escherichia coli</em>")
         , (" Saccharomyces cerevisiae", " <em>Saccharomyces cerevisiae</em>")
         , ("(Calcarius lapponicus) ", "(<em>Calcarius lapponicus)</em> ")
         , ("(Corvus brachyrhynchos) ", "(<em>Corvus brachyrhynchos)</em> ")
         , ("(Felis Catus)", "(<em>Felis Catus</em>)")
         , ("(Felis catus)", "(<em>Felis catus</em>)")
         , ("(Felis silvestris catus)", "(<em>Felis silvestris catus</em>)")
         , ("(Panthera leo)", "(<em>Panthera leo</em>)")
         , ("(Macaca fascicularis) ", "(<em>Macaca fascicularis)</em> ")
         , ("(Orcinus orca) ", "(<em>Orcinus orca)</em> ")
         , ("(Plectrophenax nivalis)", "(<em>Plectrophenax nivalis</em>)")
         , ("(Poecilia reticulata)", "(<em>Poecilia reticulata</em>)")
         , (" Pan Troglodytes", " <em>Pan Troglodytes</em>")
         , ("(Bacopa monniera)", "(<em>Bacopa monniera</em>)")
         , ("(Canis latrans)", "(<em>Canis latrans</em>)")
         , ("(Herpestes ichneumon)", "(<em>Herpestes ichneumon</em>)")
         , ("(Lynx canadensis)", "(<em>Lynx canadensis</em>)")
         , ("(Mammuthus primigenius)", "(<em>Mammuthus primigenius</em>)")
         , ("(Pan Troglodytes)", "(<em>Pan Troglodytes</em>)")
         , ("(Psilocybe cubensis)", "(<em>Psilocybe cubensis</em>)")
         , ("(Rattus norvegicus) ", "(<em>Rattus norvegicus)</em> ")
         , ("(Mus musculus)", "(<em>Mus musculus</em>)")
         , ("(Taxidea taxus)", "(<em>Taxidea taxus</em>)")
         , ("(Peromyscus leucopus)", "(<em>Peromyscus leucopus</em>)")
         , ("(Globicephala melas)", "(<em>Globicephala melas</em>)")
         , (" Arabidopsis thaliana", " <em>Arabidopsis thaliana</em>")
         , ("(Heterocephalus glaber)", "(<em>Heterocephalus glaber</em>)")
         , ("(Drosophila melanogaster", "(<em>Drosophila melanogaster</em>")
         , (" Drosophila melanogaster", " <em>Drosophila melanogaster</em>")
         , (" Arabidopsis Thaliana", " <em>Arabidopsis Thaliana</em>")
         , ("<em>C</em>. <em>elegans</em>", "<em>C. elegans</em>")
         , (" Caenorhabditis elegans ", " <em>Caenorhabditis elegans</em> ")
         , (" C. elegans", " <em>C. elegans</em>")
         , (" Bacillus subtilis", " <em>Bacillus subtilis</em>")
         , (" Octopus insularis", " <em>Octopus insularis</em>")
         , (" T. gondii", " <em>T. gondii</em>")
         , (" Equus ", " <em>Equus</em> ")
         , ("Lempel–Ziv–Markov", "Lempel-Ziv-Markov")
         , ("learn-ing", "learning")
         , ("Per- formance", "Performance")
         , ("per- formance", "performance")
         , ("Swed-ish", "Swedish")
         , (" itis ", " it is ")
         , (" k ", " <em>k</em> ")
         , (" k-shot", " <em>k</em>-shot")
         , (" k-nearest", " <em>k</em>-nearest")
         , ("one- or five-shot", "one-shot or five-shot")
         , ("lan- guage", "language")
         , ("pro-posed", "proposed")
         , ("case- control", "case-control")
         , ("high- g", "high-<em>g</em>")
         , ("semi– structured", "semi-structure")
         , ("ap-proach", "approach")
         , ("AsRL", "As RL")
         , ("spaceusing", "space using")
         , ("withits", "with its")
         , ("languagemodel", "language model")
         , ("questiongeneration", "question generation")
         , ("genomewide", "genome-wide")
         , ("regularise", "regularize")
         , (" standardised", " standardized")
         , (" Standardised", " Standardized")
         , (" memoise", " memoize")
         , (" hypothesise" ," hypothesize")
         , ("factorise" ,"factorize")
         , ("initialis", "initializ")
         , ("wethen", "we then")
         , ("successfullylearns", "successfully learns")
         , ("n-of-1", "<em>n</em>-of-1")
         , ("“ ", "“")
         , ("% ,", "%,")
         , (") ,", "),")
         , ("\t", "")
         , ("\t\t", "")
         , ("\t\t\t\t\t", "")
         , ("longstanding", "long-standing")
         , ("value- added", "value-added")
         , ("dis -ambiguate", "disambiguate")
         , ("chrono- logical", "chronological")
         , ("co- occurring", "co-occurring")
         , ("CLIP- based", "CLIP-based")
         , ("zero- and few-shot", "zero-shot & few-shot")
         , ("within- and cross", "within & cross")
         , ("self- and parent-reported", "self & parent-reported")
         , ("self- and psychiatrist-rated", "self-rated & psychiatrist-rated")
         , ("full- and half-sibling", "full & half-sibling")
         , ("full- and half-sibling pairs", "full & half-sibling pairs")
         , ("Human–Cat", "Human-Cat")
         , ("D1 receptor- and dopamine D2 receptor-expressing", "D1 receptor-expression & dopamine D2 receptor-expressing")
         , ("sex- and age-matched", "sex & age-matched")
         , ("signifi- cantly", "significantly")
         , ("be- tween", "between")
         , ("self– and informant", "self & informant")
         , ("quasi–causal", "quasi-causal")
         , ("self–reported", "self-reported")
         , ("peer–reported", "peer-reported")
         , ("profit-able", "profitable")
         , ("real- world", "real-world")
         , (" touse", " to use")
         , (" Copyright</p>", "")
         , (" GPT2", " GPT-2")
         , (" GPT3", " GPT-3")
         , ("GPT 4", "GPT-4")
         , ("GPT 2", "GPT-2")
         , ("GPT 3", "GPT-3")
         , (" target=\"_blank\"", "")
         , ("</sup><br />", "</sup>")
         , (" < jats:sub>", "<sub>")
         , ("<italic toggle=\"yes\">", "<em>")
         , ("\n \n ", "\n")
         , ("\n            <jats:italic>k</jats:italic>\n            ", "<em>k</em>")
         , ("\n            <jats:sup>–6</jats:sup>\n            ", "<sup>–6</sup>")
         , ("\n            <jats:italic>in vitro</jats:italic>\n", " <em>in vitro</em>")
         , ("\n            <jats:italic>R</jats:italic>\n", "<em>R</em>")
         , ("_X_s", "<em>X</em>s")
         , ("Yann Le Cun", "Yann LeCun")
         , ("Oliver Bryne", "Oliver Byrne")
         , ("UCF-101", "UCF101")
         , ("WikiText103", "WikiText-103")
         , (" an universe", " a universe")
         , ("eyetracking", "eye tracking")
         , ("PsychINFO", "PsycINFO")
         , ("teachinga", "teaching a")
         , ("introducea", "introduce a")
         , ("andevaluate", "and evaluate")
         , ("Norbert Weiner",  "Norbert Wiener")
         , ("mulitple",  "multiple")
         , (" a improvement",  " an improvement")
         , (" a enrichment", " an enrichment")
         , (" a insertional", " an insertional")
         , (" a overlap", " an overlap")
         , (" a audio", " an audio")
         , (" a interaction", " an interaction")
         , ("non-significant", "non-statistically-significant")
         , ("non-significance", "non-statistical-significance")
         , ("statistically statistically-significant", "statistically-significant")
         , ("GW significance", "genome-wide statistical-significance")
         , ("Most of the significance for", "Most of the statistical-significance for")
         , ("a significance test", "a statistical-significance test")
         , (" significance test", " statistical-significance test")
         , ("The significance of melatonergic", "The importance of melatonergic")
         , (", with significance for the ", ", with implications for the")
         , ("variants of uncertain significance", "variants of uncertain importance")
         , ("philosophical significance", "philosophical importance")
         , ("study-wide significance", "study-wide statistical-significance")
         , (" significance threshold", " statistical-significance threshold")
         , (" significance measures", " statistical-significance measures")
         , ("but not significance criteria", "but not statistical-significance criteria")
         , ("unique biological significance of", "unique biological importance of")
         , ("epidemiological significance", "epidemiological importance")
         , ("assess its significance", "assess its importance")
         , ("nominal significance level", "nominal statistical-significance level")
         , ("strict significance level", "strict statistical-significance level")
         , ("levels of significance", "levels of statistical-significance")
         , ("Excess significance", "Excess statistical-significance")
         , ("their scientific significance", "their scientific importance")
         , ("behavioral significance", "behavioral importance")
         , (" practise", " practice")
         , (" aesthetic", " esthetic")
         , (" utilise", "use")
         , (" utilize", "use")
         , (" utilising", " using")
         , (" utilizing", " using")
         , (" utilisation", " usage")
         , (" utilization", " usage")
         , ("synthesising", "synthesizing")
         , (" rivall", " rival")
         , ("hospitalisation", "hospitalization")
         , ("apriori", "a priori")
         , ("nonstationary", "non-stationary")
         , ("posttraumatic", "post-traumatic")
         , (" localis", " localiz")
         , ("antisociality", "anti-sociality")
         , ("capitalise ", "capitalize ")
         , ("capitalised", "capitalized")
         , ("capitalisation", "capitalization")
         , ("capitalising", "capitalizing")
         , ("capitalises", "capitalizes")
         , ("crystallised", "crystallized")
         , ("discretise", "discretize")
         , ("memorisation", "memorization")
         , ("Memorisation", "Memorization")
         , ("nonnatural", "unnatural")
         , ("risktaking", "risk-taking")
         , ("assocation", "association")
         , ("foscussing", "focusing")
         , ("areused", "are used")
         , ("insteaduse", "instead use")
         , ("humanlike", "human-like")
         , ("nevermind", "never mind")
         , ("parametris", "parameteriz")
         , ("parameterise", "parameterize")
         , ("normalis", "normaliz")
         , ("generalizt", "generalist")
         , ("generalise", "generalize")
         , ("generalisi", "generalizi")
         , ("generalisa", "generaliza")
         , (" visualisation", " visualization")
         , (" disincentivis", " disincentiviz")
         , (" incentivis", " incentiviz")
         , (" randomis", " randomiz")
         , (" Randomis", " Randomiz")
         , ("stabilising", "stabilizing")
         , ("stabilisation", "stabilization")
         , ("benefitt", "benefit")
         , ("noninsight", "non-insight")
         , ("personalised ", "personalized ")
         , ("favouritism", "favoritism")
         , ("schizohrenia", "schizophrenia")
         , ("quantitive", "quantitative")
         , ("non-institutionalised", "non-institutionalized")
         , ("nonadherence", "non-adherence")
         , ("Langrangians", "Lagrangians")
         , ("regularisation", "regularization")
         , ("urbanisatio", "nurbanization")
         , ("organisation", "organization")
         , ("vectorised", "vectorized")
         , ("nonspecifically", "non-specifically")
         , ("attentionless", "attention-less")
         , ("disfavour", "disfavor")
         , ("nonpredictive", "non-predictive")
         , ("subquadratic-time", "sub-quadratic-time")
         , ("webscale", "web-scale")
         , ("tobenchmarks", "to benchmarks")
         , ("fertilised", "fertilized")
         , ("metabolising", "metabolizing")
         , ("polygeneti", "cpolygenic")
         , ("submodels", "sub-models")
         , ("Nonblind", "Non-blind")
         , (" colour", " color")
         , (" Colour", " Color")
         , (" UNet", " U-Net")
         , ("PsycArticles", "PsycARTICLES")
         , ("behaviour", "behavior")
         , ("de Novo", "De Novo")
         , ("small saple", "small sample")
         , (" model s ", " model’s ")
         , (" ofsmoking", " of smoking")
         , (" u.s. ", " U.S. ")
         , (" ofthe", " of the")
         , (" ofdata", " of data")
         , ("thatuse", "that use")
         , ("nuture", "nurture")
         , ("\8201", " ")
         , ("ADE20k", "ADE20K")
         , (" XSUM", " XSum")
         , (" Xsum", " XSum")
         , (" xsum", " XSum")
         , ("CityScapes", "Cityscapes")
         , ("Fr’echet", "Fréchet")
         , ("Frechet", "Fréchet")
         , ("h20ttps://", "https://")
         , ("p16INK4a", "p16<sup>INK4a</sup>")
         , ("mm(2)", "mm<sup>2</sup>")
         , ("²", "<sup>2</sup>")
         , ("₂", "<sub>2</sub>")
         , ("\173", "") -- all web browsers now do hyphenation so strip soft-hyphens
         , ("‐", "-")
         , ("‰", "%") -- PER MILLE SIGN https://en.wikipedia.org/wiki/Per_mille - only example I've ever seen was erroneous
         , ("FROH", "<em>F<sub>ROH</sub></em>")
         , (" Ne ", " <em>N<sub>e</sub></em> ")
         , (" CO2", " CO<sub>2</sub>")
         , (" O2", " O<sub>2</sub>")
         , ("NAD+", "NAD⁺")
         , (" controled", " controlled")
         , ("amp#x02019;", "’")
         , ("Oamp#x02019;", "O’")
         , ("amp#x000ED;", "í")
         , ("amp#x000E9", "é")
         , ("amp#x000E9,", "é")
         , ("amp#x00142;", "ł")
         , ("amp#x000F6;", "ö")
         , ("amp#x000E9;", "é")
         , ("\\aka", "a.k.a.")
         , ("\\xmlpi{\\\\}", "")
         , (" three ", " 3 ")
         , (" Three ", " 3 ")
         , (" four ", " 4 ")
         , (" Four ", " 4 ")
         , (" five ", " 5 ")
         , (" Five ", " 5 ")
         , (" six ", " 6 ")
         , (" Six ", " 6 ")
         , (" seven ", " 7 ")
         , (" Seven ", " 7 ")
         , (" eight ", " 8 ")
         , (" Eight ", " 8 ")
         , (" nine ", " 9 ")
         , (" Nine ", " 9 ")
         , (" ten ", " 10 ")
         , (" Ten ", " 10 ")
         , (" eleven", " 11")
         , (" Eleven", " 11")
         , (" twelve", " 12")
         , (" Twelve", " 12")
         , (" thirteen", " 13")
         , (" Thirteen", " 13")
         , (" fourteen", " 14")
         , (" Fourteen", " 14")
         , (" fifteen", " 15")
         , (" Fifteen", " 15")
         , (" sixteen", " 16")
         , (" Sixteen", " 16")
         , (" seventeen", " 17")
         , (" Seventeen", " 17")
         , (" eighteen", " 18")
         , (" Eighteen", " 18")
         , (" nineteen", " 19")
         , (" Nineteen", " 19")
         , (" twenty", " 20")
         , (" Twenty", " 20")
         , ("twenty six", "26")
         , ("<br/>", "<br />")
         , ("<br>", "<br />")
         , ("<em><em>", "<em>")
         , ("</em></em>", "</em>")
         , ("<strong><strong>", "<strong")
         , ("</strong></strong>", "</strong")
         , ("ﬀ", "ff")
         , ("ﬄ", "ffl")
         , ("ﬁ", "fi")
         , ("ﬂ", "fl")
         , ("ﬅ", "ft")
         , ("ﬃ", "ffi")
         , (",”", "”,")
         , (",’", "’,")
         , (" (”", " (“")
         , ("\160", " ") -- NO BREAK SPACE
         ]

linkCanonicalize :: String -> String
linkCanonicalize l | "https://gwern.net/" `isPrefixOf` l = replace "https://gwern.net/" "/" l
                   -- | head l == '#' = l
                   | otherwise = l

filterMeta :: String -> String
filterMeta ea = if anyInfix ea badSubstrings || elem ea badWholes then "" else ea
 where badSubstrings, badWholes :: [String]
       badSubstrings = isUniqueList ["ABBYY", "Adobe", "InDesign", "Arbortext", "Unicode", "Total Publishing", "pdftk", "aBBYY", "FineReader", "LaTeX", "hyperref", "Microsoft", "Office Word", "Acrobat", "Plug-in", "Capture", "ocrmypdf", "tesseract", "Windows", "JstorPdfGenerator", "Linux", "Mozilla", "Chromium", "Gecko", "QuarkXPress", "AppleWorks", "Apache", ".tif", "2001", "2014", "3628", "4713", "AR PPG", "ActivePDF", "Administrator", "Administratör", "American Association for the Advancement of Science", "Appligent", "BAMAC6", "CDPUBLICATIONS", "CDPublications", "Chennai India", "Copyright", "DesktopOperator", "Emacs", "G42", "GmbH", "IEEE", "Image2PDF", "J-00", "JN-00", "LSA User", "LaserWriter", "Org-mode", "PDF Generator", "PScript5.dll", "PageMaker", "PdfCompressor", "Penta", "Preview", "PrimoPDF", "PrincetonImaging.com", "Print Plant", "Radical Eye", "RealPage", "SDK", "SYSTEM400", "Sci Publ Svcs", "Scientific American", "Springer", "TIF", "Unknown", "Utilities", "XPP", "apark", "bhanson", "cairo 1", "cairographics.org", "dvips", "easyPDF", "eguise", "epfeifer", "fdz", "ftfy", "gscan2pdf", "jsalvatier", "jwh1975", "kdx", "pdf", " OVID ", "imogenes", "firefox", "Firefox", "Mac1", "EBSCO", "faculty.vp", ".book", "PII", "Typeset", ".pmd", "affiliations", "list of authors", ".doc", "untitled", "Untitled", "FrameMaker", "PSPrinter", "qxd", "INTEGRA", "Xyvision", "CAJUN", "PPT Extended", "Secure Data Services", "MGS V", "mgs;", "COPSING", "- AAAS", "Science Journals", "Serif Affinity", "Google Analytics", "rnvb085", ".indd", "hred_", "penta@", "WorkStation", "ORDINATO+", ":Gold:", "XeTeX", "Aspose", "Abbyy", "Archetype Publishing Inc.", "AmornrutS", "OVID-DS", "PAPER Template", "IATED", "TECHBOOKS", "Word 6.01", "TID Print Plant", "8.indd", "pdftk-java", "OP-ESRJ", "JRC5", "klynch", "pruich", "Micron", "Anonymous Submission", "Asterisk", "KBarry2", ",-0", "fi-5530C2dj", "FUJIT S. U.", "LEVET_Layout", "Digitized by the ", "shaniahl", ".orig.pdf", ".dvi", ".qxd", ".ps", "doi:10", "DOI", ".tmp", ".pdf", ".eps", ".PDF", "APA template", "Author Guidelines", "156x234mm", "C:\\", "D:\\", "CUP_JBS", ".vp", ".wpd", "EBSCOhost", ".docx", ".qxp", "PDF_Banner", "MergedFile", "No Job Name", "PII: ", "ProQuest Dissertation", "ScanGate", "Science Magazine", ".CHP", ".chp", ".tex", ".fm", "http://content.nejm.org/cgi/content/", "stdin", "Corel PHOTO-PAINT", "Thomson Press India", "B L Ganju", "Meta˚Analytic", "RealObjects", "PDFreactor(R)", "Licensed for: Oxford University", "CoVantage", "RYALS327-SCAN", "WWS-5ZM9", "<unknown>", "[ M1C44 ]", "WWS-5ZM", "html2ps ", "version 1.0 beta2", "jason.richwine", "jmaynard", "jmcfadde", "k.albert", "kstange", "macftz01", "markj", "mcdonaldm", "mchahino", "meiersa", "mkc43", "pilc2501", "pm016", "pm025", "pm054", "pubdat", "randerer", "renee.lojewski", "tiff2ps", "yeh", "Admin", "C U. P. Printing", "Debenu ", "Quick P. D. F. Library 9.12", "www.debenu.com", "JPL 99", "MinnickD", "Office", "Owner", "SPDF", "Writer", "jcpham", "DLE4&lt;8", "8AB@0B&gt", "Paperless", "psjoin 0.2", "Apex", "Elsevier Science", "PsycINFO", "kristine gallo", "TeX", "PDFplus", "Elsevier", "N/A", "OmniPage", "scansoft", "Articlizer", "ARTICLIZER", "c:/ncn"]
       badWholes = isUniqueList ["P", "b", "cretu", "user", "yeh", "Canon", "times", "is2020", "downes", "American Medical Association", "om", "lhf", "comp", "Science Magazine", "Josh Lerner, Scott Stern (Editors)", "arsalan", "rssa_a0157 469..482", "Schniederjans_lo", "mcdonaldm", "ET35-4G.vp", "spco_037.fm", "mchahino", "LaTeX2e", "Paperless", "fulvio", "Winter", "markj", "Vahrenhorst", "vahrenhorst", "Vahrenhorst 2004", "Vahrenhorst 2008", "pilc2501", "yeh 2008", "markj 2009", "021186U129", "02_ASQ523602 1..33", "03_gbb155 240..248", "1)", "1.0b", "110s(6) Science. 555-", "1247", "2913 | 01 Chorney", "301246", "378787 1100..1105", "4559", "459119", "4AD004/Prov9 FMV/4P", "52457938", "7.0 psym 7#1 2002", "72508-danigelis.q41", "9757 van Stuijvenberg", "99202", "BBS1200319 661..726", "BBS1300119 297..350", "Backtalk", "Backups", "BatesTalk", "Brookings draft v", "CAM200", "CDP370136 177..182", "CMMI10", "COLLINCH01", "COMM34$U44", "COPSINGOLEVET", "DO00010384 643..650", "DP 14-009", "Digestion", "Final_EXIT_text", "Gerontotherapeutics", "Harrison J", "II", "IMD JATS", "ISO/IEC 9899:yyyy", "Information", "JC162160 1642..1651", "JEOBP_14_2_2011", "JMCB16U208", "Journal06-04b.cdr", "Latvala", "Layout 1", "MASTER THESIS 5", "MIT-LCS:MIT/LCS/TR-164", "MSS21283 927..934", "Masters' Thesis", "Nowicka, R", "OP-ESRJ170071 1..13", "OP-QJEC150001 571..614 ++", "P:TEXSICOMP 9-1 8849 8849", "PEDS20142707_pap_peds 1..9", "PEDS_20173872 1..11", "S0747-5632(00)00043-1", "PME00042", "PSCI13124", "Print", "RAAG_A_1310021_O", "RULES-2014-P-CON", "Review", "SIA", "Slide 1", "Standards 05-06", "TF-TPDR120086 1..8 ++", "Title", "Title:", "Tobler_CameraReady", "US020180204116A120180719", "Unknown", "VJMB_A_1327416_O", "Ventura - 12JBR8", "Vol 9#5", "WR02015.fm", "Wildcats", "ZP577", "ajps_461_HR", "anp060-79 407..506", "bhm211 1737..1747", "btn127 1381..1385", "c011.tex", "cns_317_LR", "ddl206 2721..2731", "default", "desc_745.fm", "e08:9", "ejn_5217 3532..3540", "emon-84-04-01 3..28", "es-2013-03688w 1..9", "foo", "hcrj901068 151..157", "hred_91_110.42_66", "inhy_60_205.213_218", "ipg1200217a", "jasar08282 841..854", "jcp20186 373..378", "jcp25202 175..179", "jcpp_1798 1192..1199", "jn169326 872..877", "khan", "mbev_16_1218.1791_1798", "mgs;01jan95", "osteo-1203 257..264", "oup_cercor_bhy157 1..11 ++", "pnas201201895 1..8", "pnp06457 1125..1128", "s00221-005-2334-6ca-web 23..30", "stdin", "template", "title", "vsp0092 187..211", "ÿþ1", "ÿþ14-226", "“Alt", "chowe", "comp5", "dan", "decosta", "gottfredson", "van den Hurk", "Word", "pdftk-java 3.0.9", "bar", "tmp"]

-- title clean up: delete the period at the end of many titles, extraneous colon spacing, remove Arxiv's newline+double-space, and general whitespace cleaning
trimTitle :: String -> String
trimTitle [] = ""
trimTitle t = let t' = reverse $ sedMany [("†.*", ""), -- eg "Relation of Serum 25-Hydroxyvitamin D to Heart Rate and Cardiac Work (from the National Health and Nutrition Examination Surveys)†\n†Conflict of interest: Dr. Simpson receives support from Abbott Laboratories, Chicago, Illinois"
                                          ("([a-z])_ ", "\\1: ")] $ -- a lot of Elsevier papers replace colons with underscores (‽) in PDF metadata eg. "Compensatory conspicuous communication_ Low status increases jargon use"
                       replaceMany [(" : ", ": "), ("\n ", " ")] $ trim t in
                if not (null t') then reverse (if head t' == '.' then tail t' else t') else ""

 -- eg "foo.pdf#page=50&org=openai" → "50"; "foo.pdf" → ""
pageNumberParse :: String -> String
pageNumberParse u = let pg = sed ".*\\.pdf#page=([0-9]+).*" "\\1" u
                    in if u == pg then "" else pg

-- Compact lists of authors to abbreviate personal names, but preserve the full name in a span tooltip for on-hover like usual.
--
-- eg. > authorsInitialize "J. Smith, Foo Bar"
-- → [Str "J. Smith",Str ", ",Span ("",[],[("title","Foo Bar")]) [Str "F. Bar"]]
--
-- > authorsInitialize "John Jacob Jingleheimer Schmidt"
-- → [Str "John Jacob Jingleheimer Schmidt"]
-- vs:
-- > authorsInitialize "John Jacob Jingleheimer Schmidt, John Jacob Jingleheimer Schmidt, John Jacob Jingleheimer Schmidt"
-- → [Span ("",[],[("title","John Jacob Jingleheimer Schmidt")]) [Str "J. Schmidt"],Str ", ",
--    Span ("",[],[("title","John Jacob Jingleheimer Schmidt")]) [Str "J. Schmidt"],Str ", ",
--    Span ("",[],[("title","John Jacob Jingleheimer Schmidt")]) [Str "J. Schmidt"]]
--
-- BUG: does not initialize names with Unicode. This is because the regex library with search-and-replace does not support Unicode, and the regex libraries which support Unicode do not provide search-and-replace. Presumably I can hack up a search-and-replace on the latter.
-- Example: > authorsInitialize "Robert Geirhos, Jörn-Henrik Jacobsen, Claudio Michaelis, ..."
-- → [Span ("",[],[("title","Robert Geirhos")]) [Str "R. Geirhos"],Str ", ",Str "J\246rn-Henrik Jacobsen", ...]
authorsInitialize :: String -> [Inline]
authorsInitialize aut = let authors = split ", " aut in
                          if length authors == 1 then [Str $ T.pack aut]
                          else
                              intersperse (Str ", ") $
                              map (\a -> let short = sedMany (reverse [
                                               -- three middle-names:
                                                 ("^([A-Z.-])[A-za-z.-]+ [A-za-z.-]+ [A-za-z.-]+ [A-za-z.-]+ (.*)", "\\1. \\2")
                                               -- two middle-names:
                                               , ("^([A-Z.-])[A-za-z.-]+ [A-za-z.-]+ [A-za-z.-]+ (.*)", "\\1. \\2")
                                               -- one middle-name:
                                               , ("^([A-Z.-])[A-za-z.-]+ [A-za-z.-]+ (.*)", "\\1. \\2")
                                               -- no middle-name:
                                               , ("^([A-Z.-])[A-za-z.-]+ (.*)", "\\1. \\2")]) a in
                                           if a==short then Str (T.pack a) else Span ("", [], [("title", T.pack a)]) [Str (T.pack short)]) authors

-- check whether brackets are balanced in a text string:
balanced :: String -> String
balanced str = helper str "" 0 0
  where
    helper [] stack _ idx = if null stack then "" else drop idx str
    helper (s:ss) stack n idx
      | s `elem` openBrackets = helper ss (s:stack) (n+1) (if null stack then n else idx)
      | s `elem` closeBrackets =
          if not (null stack) && head stack == matchingBracket s
            then helper ss (tail stack) (n+1) (if null stack then n else idx)
            else drop n str
      | otherwise = helper ss stack (n+1) idx
    openBrackets = "([{"::String
    closeBrackets = ")]}"::String
    matchingBracket ')' = '('
    matchingBracket ']' = '['
    matchingBracket '}' = '{' -- TODO: if this approach works, add double-quotes as delimiters that should be balanced
    matchingBracket _ = error "Invalid bracket"

-- Enable additional runtime checks to very long config lists which risk error from overlap or redundancy. Prints out the duplicates.
-- (Since the config lists are static, they can in theory be checked at compile-time, but my attempt to do that with Template Haskell
-- for XMonad keymap configs many years ago ran into a lot of pain, so I won't bother even trying again.)
-- Helper function to check uniqueness & report the offending list:

-- Optimized helper function to get duplicates
getDuplicates :: Ord a => [a] -> [a]
getDuplicates = snd . foldl' go (Set.empty, [])
  where
    go (seen, duplicates) x
      | x `Set.member` seen = (seen, duplicates)
      | otherwise = (Set.insert x seen, duplicates)
throwError :: Show a => String -> [a] -> b
throwError msg xs = error $ "Error: " ++ msg ++ " " ++ show xs
checkUniqueOrThrow :: (Eq a, Ord a, Show a) => String -> [a] -> [a]
checkUniqueOrThrow msg xs
  | null duplicates = xs
  | otherwise = throwError msg duplicates
  where duplicates = getDuplicates xs

-- 0. check a simple list for uniqueness in the only way possible:
isUniqueList :: (Eq a, Ord a, Show a) => [a] -> [a]
isUniqueList = checkUniqueOrThrow "Simple list contains duplicates:"

-- Association-list checks:
-- 1. isUnique: all key-value pairs are unique and there are no duplicates
isUnique :: (Eq a, Show a, Eq b, Ord a, Ord b, Show b) => [(a,b)] -> [(a,b)]
isUnique = checkUniqueOrThrow "Association List contains duplicate key-value pairs:"

-- 2. isUniqueKeys: all keys are unique and there are no duplicates
isUniqueKeys :: (Eq a, Ord a, Show a, Show b) => [(a,b)] -> [(a,b)]
isUniqueKeys xs
  | null duplicates = xs
  | otherwise = throwError "Association List contains duplicate keys:" duplicates
  where duplicates = getDuplicates (map fst xs)
-- special-case:
isUniqueKeys3 :: (Eq a, Ord a, Show a) => [(a,b,c)] -> [(a,b,c)]
isUniqueKeys3 xs
  | null duplicates = xs
  | otherwise = throwError "Association List contains duplicate keys:" duplicates
  where duplicates = getDuplicates (map (\(a,_,_) -> a) xs)

-- 3. isUniqueValues: all values are unique and there are no duplicates
isUniqueValues :: (Show a, Ord a, Eq b, Ord b, Show b) => [(a,b)] -> [(a,b)]
isUniqueValues xs
  | null duplicates = xs
  | otherwise = throwError "Association List contains duplicate values:" duplicates
  where duplicates = getDuplicates (map snd xs)

-- 4. isUniqueAll: all keys, values, and key-value pairs are unique
isUniqueAll :: (Eq a, Ord a, Show a, Eq b, Ord b, Show b) => [(a,b)] -> [(a,b)]
isUniqueAll xs = isUniqueValues $ isUniqueKeys $ isUnique xs

-- simple test for infinite loops in infix string rewrites: we take the list of before→after rewrites, and we try to rewrite the 'before'
-- using some given function. If it infinite-loops...
testInfixRewriteLoops :: [(String,String)] -> (String -> String) -> [(String,String,String)]
testInfixRewriteLoops rewrites f = map (\(a,b) -> (a,b,fixedPoint f a)) $ reverse rewrites

isCycleLess :: (Eq a, Ord a, Show a) => [(a,a)] -> [(a,a)]
isCycleLess xs = if not (cycleExists xs) then xs else
  throwError "Association list of rewrite-rules has cycles! Errors related to:" (show $ findCycles xs)

cycleExists :: Ord a => [(a, a)] -> Bool
cycleExists tuples = any (uncurry (==)) tuples ||
    -- There's a cycle if one of the strongly connected components has more than one node
    any ((> 1) . length . flattenSCC)
       -- Generate strongly connected components from edges
       (stronglyConnComp $
        -- Create edges by converting a tuple (a, b) to (a, b, [b]) to reflect a -> b
        map (\(a, b) -> (a, a, [b])) tuples)

-- *Which* rewrite rules are responsible for an infinite loop?
-- Here's one way to find bad nodes easily (albeit inefficiently):
-- start with the list of rewrites and two empty temporary lists;
-- from the rewrite list, take & incrementally add rules to the first list if they do not create a cycle in the first list;
-- if they do, add them to the second list instead (otherwise ignoring the second list);
-- when all rules are used up, return the second list. Those are the bad rules.
findCycles :: Ord a => [(a, a)] -> [(a, a)]
findCycles xs = snd $ foldl f ([], []) xs
  where
    f (good, bad) rule
      | cycleExists (rule : good) = (good, rule : bad)
      | otherwise = (rule : good, bad)

-- `cycleExists` testsuite:
testCycleExists :: [([(Int,Int)], Bool)] -> [[(Int,Int)]]
testCycleExists testCases = [ rules | (rules, expected) <- testCases, cycleExists rules /= expected]
testCycleDetection :: [[(Int,Int)]]
testCycleDetection = testCycleExists testCases
 where testCases :: [([(Int, Int)], Bool)]
       testCases = isUniqueKeys [ ([], False) -- no rules, no cycles
            , ([(1, 2)], False) -- one rule, no cycles
            , ([(1, 1)], True), ([(1, 2), (2, 3), (3, 4), (5, 5)], True), ([(1, 2), (2, 3), (4, 4), (5, 6)], True) -- self loop
            , ([(1, 2), (2, 3), (3, 4)], False) -- rules with no cycles
            , ([(1, 2), (2, 1)], True) -- simple cycle
            , ([(1, 2), (2, 3), (3, 1)], True) -- cycle with more than 2 nodes: where there is a cycle of nodes that all point to one another, but no node points to itself
            , ([(1, 2), (2, 3), (3, 4), (4, 1)], True) -- larger cycle
            , ([(1, 2), (2, 1), (3, 4), (4, 3), (5, 6), (6, 5)], True) -- Multiple disjoint cycles within a larger rule set
            , ([(1, 2), (1, 3), (2, 4), (2, 5), (3, 6), (3, 7)], False)
            , ([(1, 2), (2, 3), (4, 5), (5, 6)], False) -- separate set of rules, no cycles
            , ([(1, 2), (2, 3), (3, 1), (4, 5), (5, 6), (6, 4)], True) -- separate set of rules with cycles
            , ([(1, 2), (2, 3), (3, 2), (4, 5), (5, 4)], True) -- there is a cycle within subset of rules
            , ([(1, 2), (3, 4), (5, 6)], False) -- separate set of rules, no cycles
            , ([(1, 2), (1, 2), (2, 3), (2, 3)], False) -- repetition
            , ([(1, 2), (1, 3), (2, 4), (3, 4)], False) -- Multiple paths to the same node, but no cycles
            , ([(1, 2), (1, 3), (2, 4), (3, 4), (4, 1)], True) -- where there are multiple paths leading to a node that is part of a cycle.
            , ([(1, 1), (2, 2), (3, 3)], True) --where every node in the list points to itself (simple loop for every node)
            ]

-- must handle both "https://twitter.com/grantslatton/status/1703913578036904431" and "https://twitter.com/grantslatton":
extractTwitterUsername :: String -> String
extractTwitterUsername = sed "^https:\\/\\/x\\.com\\/([a-z0-9]+)$" "\\1" . sed "^https:\\/\\/twitter\\.com\\/([a-z0-9]+)$" "\\1" . sed "^https:\\/\\/twitter\\.com\\/([^\\/]+)/status/[0-9]+$" "\\1"

-- print out Doubles long-form, not in scientific notation. By default, Haskell will print values like '10e8', which is bad for downstream users like the inflation-adjuster JavaScript. But it turns out to be surprisingly hard to print out the literal of a Double/Float without rounding, scientific notation, fractions, precision limitations, or other issues. This tries to do so using Numeric.showFFloat, and includes a test-suite of examples to ensure the String is as expected. For very small amounts like '1.0000000000000002', they will be rounded (to '1').
-- Precision '0' means nothing after the decimal point, like '0'; '1' means 1 digit after the decimal like '0.1', etc.
printDouble :: Int -> Double -> String
printDouble precision x = if x > 1.7976931348623157e308 || x < -1.7976931348623157e308
               then error $ "printDouble: Extreme unsupported value past what showFFloat supports; you'll have to handle this differently: " ++ show x
               else removeTrailingZeros $ showFFloat (Just precision) x ""
    where removeTrailingZeros, drop1IfDot :: String -> String
          removeTrailingZeros "-0"   = "0"
          removeTrailingZeros "-0."  = "0"
          removeTrailingZeros "-0.0" = "0"
          removeTrailingZeros "0.0"  = "0"
          removeTrailingZeros "0"  = "0"
          removeTrailingZeros y = drop1IfDot $ reverse $ dropWhile (== '0') $ reverse y
          drop1IfDot xs = if last xs == '.' then init xs else xs
          -- removeTrailingZeros y
          --   | take 2 y == ".0" = "0"
          --   | otherwise = drop1IfDot $ reverse $ dropWhile (== '0') $ reverse y
          -- drop1IfDot xs = if not (null xs) && last xs == '.' then init xs else xs

printDoubleTestSuite :: [(Double, Int, String, String)]
printDoubleTestSuite = filter (\(_,_,expected,actual) -> expected /= actual) $ map (\(n,prec,s) -> (n,prec,s, printDouble prec n )) printDoubleTests
 where
  printDoubleTests :: [(Double, Int, String)]
  printDoubleTests =
    -- no `isUnique` check for zeros because keys are not unique by value (eg. −0.0 == 0 == 0.00 etc) but we need to test that they print out the same
              [
              (-0, 0, "0")
              , (-0.0, 0, "0")
              , (-0.00, 0, "0")
              , (0, 0, "0")
              , (0.0, 0, "0")
              , (0.00, 0, "0")
              ] ++ [
              (1.0, 0, "1")
              , (1.1, 1, "1.1")
              , (10.01, 2, "10.01")
              , (1000000.01, 2, "1000000.01")
              , (123456789.123456789, 15, "123456789.12345679")
              , (1.000000000000001, 15, "1.000000000000001")
              , (3.141592653589793, 15, "3.141592653589793")
              , (-3.141592653589793, 15, "-3.141592653589793")
              , (-1.000000000000001, 15, "-1.000000000000001")
              , (-123456789.123456789, 15, "-123456789.12345679")
              , (-1000000.01, 2, "-1000000.01")
              , (-10.01, 2, "-10.01")
              , (-1.1, 2, "-1.1")
              , (-1.0, 2, "-1")
              , (0.000000000000001, 15, "0.000000000000001")
              , (-0.000000000000001, 15, "-0.000000000000001")
              , (0.9999999999999999, 15, "1")
              , (-0.9999999999999999, 15, "-1")
              , (1.0000000000000002, 15, "1")
              , (-1.0000000000000002, 15, "-1")
              , (0.224, 0, "0")
              , (0.224, 1, "0.2")
              , (0.224, 2, "0.22")
              ]

flattenLinksInInlines :: [Inline] -> [Inline]
flattenLinksInInlines = map flattenLinks
  where flattenLinks :: Inline -> Inline
        flattenLinks x@Link{} = Str (inlinesToText [x])
        flattenLinks x = x

-- | Convert a list of inlines into a string.
inlinesToText :: [Inline] -> T.Text
inlinesToText = -- HACK: dealing with RawInline pairs like [RawInline "<sup>", Text "th", RawInline "</sup>"] is a PITA to do properly (have to process to HTML and then back into AST), so we'll just handle special cases for now...
  replaceManyT [("<sup>",""), ("</sup>",""), ("<sub>",""),("</sub>","")] .
                T.concat . map go
  where go x = case x of
               -- reached the literal T.Text:
               Str s    -> s
               -- strip & recurse on the [Inline]:
               Emph        x' -> inlinesToText x'
               Underline   x' -> inlinesToText x'
               Strong      x' -> inlinesToText x'
               Strikeout   x' -> inlinesToText x'
               Superscript x' -> inlinesToText x'
               Subscript   x' -> inlinesToText x'
               SmallCaps   x' -> inlinesToText x'
               -- throw away attributes and recurse on the [Inline]:
               Span _      x' -> inlinesToText x' -- eg. [foo]{.smallcaps} -> foo
               Quoted _    x' -> inlinesToText x'
               Cite _      x' -> inlinesToText x'
               Link _   x' _  -> inlinesToText x'
               Image _  x' _  -> inlinesToText x'
               -- throw away attributes, return the literal T.Text:
               Math _      x' -> x'
               RawInline _ x' -> x'
               Code _      x' -> x'
               -- fall through with a blank:
               _        -> " "::T.Text

hasExtension :: T.Text -> T.Text -> Bool
hasExtension ext p = extension p == ext

extension :: T.Text -> T.Text
extension = T.pack . maybe "" (takeExtension . uriPath) . parseURIReference . T.unpack

isLocal :: T.Text -> Bool
isLocal "" = error "LinkIcon: isLocal: Invalid empty string used as link."
isLocal s = T.head s == '/'

isHostOrArchive :: T.Text -> T.Text -> Bool
isHostOrArchive domain url = let h = host url in
                                h == domain || ("/doc/www/"`T.append`domain) `T.isPrefixOf` url
