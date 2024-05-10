{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (group, intercalate, sort, isInfixOf, isPrefixOf, isSuffixOf, tails)
import qualified Data.Map as M
import Data.Containers.ListUtils (nubOrd)
import Data.Text.IO as TIO (readFile, writeFile)
import Network.URI (parseURIReference, uriAuthority, uriPath, uriRegName, parseURI, uriScheme, uriAuthority, uriPath, uriRegName, isURIReference)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (takeDirectory, takeExtension)
import System.IO (stderr, hPutStr)
import System.IO.Temp (emptySystemTempFile)
import Text.Show.Pretty (ppShow)
import qualified Data.Text as T (Text, concat, pack, unpack, isInfixOf, isPrefixOf, isSuffixOf, replace, head, append, reverse, takeWhile)
import System.Exit (ExitCode(ExitFailure))
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import Data.FileStore.Utils (runShellCommand)
import Control.DeepSeq (deepseq, NFData)
import System.Posix.Files (touchFile)

import Text.Regex (subRegex, mkRegex) -- WARNING: for Unicode support, this needs to be 'regex-compat-tdfa' package, otherwise, the search-and-replaces will go badly awry!
import Control.Exception (catch, evaluate, SomeException)
import System.IO.Unsafe (unsafePerformIO)

import Text.Pandoc (def, nullAttr, nullMeta, runPure,
                    writerColumns, writePlain, Block(Div, RawBlock), Pandoc(Pandoc), Inline(..), MathType(InlineMath), Block(Para), readerExtensions, writerExtensions, readHtml, writeMarkdown, pandocExtensions, WriterOptions, Extension(Ext_shortcut_reference_links), enableExtension, Attr, Format(..), topDown, writeHtml5String)
import Text.Pandoc.Walk (walk)

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
               if (contentsNew /= contentsOld) then do tempPath <- emptySystemTempFile ("hakyll-"++template)
                                                       TIO.writeFile tempPath contentsNew
                                                       renameFile tempPath target
               else touchFile target -- mark as up to date

trim :: String -> String
trim = reverse . dropWhile badChars . reverse . dropWhile badChars -- . filter (/='\n')
  where badChars :: Char -> Bool
        badChars c = isSpace c || (c=='-')

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

-- write an Inline to a HTML string fragment; strip the `<p></p>` Pandoc wrapper
-- > toHTML $ Span nullAttr [Str "foo"]
-- → "<span>foo</span>"
-- > toHTML $ Str "foo"
-- → "foo"
toHTML :: Inline -> String
toHTML il = let clean = runPure $ do
                                   md <- writeHtml5String def (Pandoc nullMeta [Para [il]])
                                   return $ sed "^<span>(.*)</span>$" "\\1" $ sed "^<p>(.*)</p>$" "\\1" $ replace "\n" " " $ T.unpack md
                             in case clean of
                                  Left e -> error $ ppShow e ++ ": " ++ show il
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

-- WARNING: this is deliberately `readHtml`, even though that will erase some forms of HTML constructs when Pandoc reads it,
-- because `readMarkdown`, while more permissive in that respect, results in *other* forms of breakage, apparently linked to lingering Raw* blocks
-- which then disable most downstream rewrites (eg. if you switch, the inflation-adjustments will all spontaneously stop working).
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

-- HACK: this is a workaround for an edge-case: Pandoc reads complex tables as 'grid tables', which then, when written using the default writer options, will break elements arbitrarily at newlines (breaking links in particular). We set the column width *so* wide that it should never need to break, and also enable 'reference links' to shield links by sticking their definition 'outside' the table. See <https://github.com/jgm/pandoc/issues/7641>.
-- This also gives us somewhat cleaner HTML by making Pandoc not insert '\n'.
safeHtmlWriterOptions :: Text.Pandoc.WriterOptions
safeHtmlWriterOptions = def{writerColumns = 9999, writerExtensions = enableExtension Ext_shortcut_reference_links pandocExtensions}

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

inline2Path :: Inline -> T.Text
inline2Path (Link _ _ (path,_)) = path
inline2Path (Image _ _ (path,_)) = path
inline2Path x = error $ "Utils.inline2Path: called on an Inline for which there is no filepath target‽ " ++ show x

-- Add or remove a class to a Link or Span; this is a null op if the class is already present or it is not a Link/Span.
addClass :: T.Text -> Inline -> Inline
addClass clss x@(Code  (i, clsses, ks) code)        = if clss `elem` clsses then x else Code  (i, clss:clsses, ks) code
addClass clss x@(Image (i, clsses, ks) s (url, tt)) = if clss `elem` clsses then x else Image (i, clss:clsses, ks) s (url, tt)
addClass clss x@(Link  (i, clsses, ks) s (url, tt)) = if clss `elem` clsses then x else Link  (i, clss:clsses, ks) s (url, tt)
addClass clss x@(Span  (i, clsses, ks) s)           = if clss `elem` clsses then x else Span  (i, clss:clsses, ks) s
addClass clss x = error $ "Utils.addClass: attempted to add a class of an Inline where that makes no sense? " ++ show clss ++ " : " ++ show x
removeClass :: T.Text -> Inline -> Inline
removeClass clss x@(Code  (i, clsses, ks) code)        = if clss `notElem` clsses then x else Code  (i, filter (/=clss) clsses, ks) code
removeClass clss x@(Image (i, clsses, ks) s (url, tt)) = if clss `notElem` clsses then x else Image (i, filter (/=clss) clsses, ks) s (url, tt)
removeClass clss x@(Link (i, clsses, ks) s (url, tt))  = if clss `notElem` clsses then x else Link  (i, filter (/=clss) clsses, ks) s (url, tt)
removeClass clss x@(Span (i, clsses, ks) s)            = if clss `notElem` clsses then x else Span  (i, filter (/=clss) clsses, ks) s
removeClass clss x = error $ "Utils.removeClass: attempted to remove a class of an Inline where that makes no sense? " ++ show clss ++ " : " ++ show x

hasClass :: T.Text -> Inline -> Bool
hasClass clss (Code  (_, clsses, _) _)   = clss `elem` clsses
hasClass clss (Image (_, clsses, _) _ _) = clss `elem` clsses
hasClass clss (Link (_, clsses, _) _ _)  = clss `elem` clsses
hasClass clss (Span (_, clsses, _) _)    = clss `elem` clsses
hasClass clss x = error $ "Utils.hasClass: attempted to check the class of an Inline where that makes no sense? " ++ show clss ++ " : " ++ show x

removeKey :: T.Text -> Inline -> Inline
removeKey key (Code  (i, cl, ks) code)        = Code  (i, cl, filter (\(k,_) -> k/=key) ks) code
removeKey key (Image (i, cl, ks) s (url, tt)) = Image (i, cl, filter (\(k,_) -> k/=key) ks) s (url, tt)
removeKey key (Link  (i, cl, ks) s (url, tt)) = Link  (i, cl, filter (\(k,_) -> k/=key) ks) s (url, tt)
removeKey key (Span  (i, cl, ks) s)           = Span  (i, cl, filter (\(k,_) -> k/=key) ks) s
removeKey key x = error $ "Utils.removeKey: attempted to remove a key from the key-value dict of an Inline where that makes no sense? " ++ show key ++ " : " ++ show x
addKey :: (T.Text,T.Text) -> Inline -> Inline
addKey key (Code  (i, cl, ks) code)        = Code  (i, cl, nubOrd (key : ks)) code
addKey key (Image (i, cl, ks) s (url, tt)) = Image (i, cl, nubOrd (key : ks)) s (url, tt)
addKey key (Link  (i, cl, ks) s (url, tt)) = Link  (i, cl, nubOrd (key : ks)) s (url, tt)
addKey key (Span  (i, cl, ks) s)           = Span  (i, cl, nubOrd (key : ks)) s
addKey key x = error $ "Utils.addKey: attempted to add a key from the key-value dict of an Inline where that makes no sense? " ++ show key ++ " : " ++ show x

hasExtension :: T.Text -> T.Text -> Bool
hasExtension ext p = extension p == ext

hasExtensionS :: String -> String -> Bool
hasExtensionS ext p = hasExtension (T.pack ext) (T.pack p)

extension :: T.Text -> T.Text
extension = T.pack . maybe "" (System.FilePath.takeExtension . uriPath) . parseURIReference . T.unpack

isLocal :: T.Text -> Bool
isLocal "" = error "LinkIcon: isLocal: Invalid empty string used as link."
isLocal s = T.head s == '/'

-- throw a fatal error if any entry in a list fails a test; uses `NFData`/`deepseq` to guarantee that the test gets evaluated
-- and will kill as soon as possible.
ensure :: (Show a, NFData a) => String -> String -> (a -> Bool) -> [a] -> [a]
ensure location fString f xs = deepseq evaluatedList evaluatedList
  where
    evaluatedList = map (\i -> if f i then i
                               else error (location ++ ": failed property check '" ++ fString ++ "'; input was: " ++ show i)) xs

-- Check if a string is a plausible domain or subdomain
isDomain :: String -> Bool
isDomain domain = case parseURI ("http://" ++ domain) of
    Just uri -> case uriAuthority uri of
        Just auth -> null (uriPath uri) && not (null (uriRegName auth))
        Nothing -> False
    Nothing -> False
isDomainT :: T.Text -> Bool
isDomainT = isDomain . T.unpack

-- Check if a string is a valid HTTP or HTTPS URL. To check local paths as well, use `isURIReference`/`isURIReferenceT`.
isURL :: String -> Bool
isURL url = case parseURI url of
    Just uri -> scheme == "http:" || scheme == "https:"
        where scheme = uriScheme uri
    Nothing -> False
isURLT :: T.Text -> Bool
isURLT = isURL . T.unpack

isURIReferenceT :: T.Text -> Bool
isURIReferenceT = isURIReference . T.unpack

isHostOrArchive :: T.Text -> T.Text -> Bool
isHostOrArchive domain url = let h = host url in
                                h == domain || ("/doc/www/"`T.append`domain) `T.isPrefixOf` url

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

-- because the regex libraries throw fatal exceptions, which are highly uninformative, we have to do a lot of work to catch exceptions and print out useful debug info for identifying *what* regexp went wrong, rather than unhelpfully reporting "Exception 13" or whatever.
sed :: String -> String -> String -> String
sed before after s = unsafePerformIO $ do
  let action = if before == after
                 then error $ "Fatal error in `sed`: before == after: \"" ++ before ++ "\""
                 else do
                   let regex = mkRegex before
                   let result = subRegex regex s after
                   _ <- evaluate (length result)  -- Force full evaluation, so we catch it here and now, rather than it happening later and skipping the debugging info
                   return result
  catch action handleExceptions
    where
      handleExceptions :: SomeException -> IO String
      handleExceptions e = return $ "Error occurred. Exception: " ++ show e ++
                                    "; arguments were: '" ++ before ++
                                    "' : '" ++ after ++ "' : '" ++ s ++ "'"

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

-- (`replace`/`split`/`hasKeyAL` copied from <https://hackage.haskell.org/package/MissingH-1.5.0.1/docs/src/Data.List.Utils.html> to avoid MissingH's dependency on regex-compat)
-- replace requires that the 2 replacements be different, but otherwise does not impose any requirements like non-nullness or that any replacement happened. So it can be used to delete strings without replacement (`replace "foo" ""`), or 'just in case'.
-- For search-and-replace where you *know* you meant to change the input, use `replaceChecked`.
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


kvLookup :: String -> [(String, String)] -> String
kvLookup key xs = maybe "" id (lookup key xs)

kvLookupT :: T.Text -> [(T.Text, T.Text)] -> T.Text
kvLookupT key xs = maybe "" id (lookup key xs)

kvDOI :: [(String,String)] -> String
kvDOI = kvLookup "doi"

kvDOIT :: [(T.Text,T.Text)] -> T.Text
kvDOIT = kvLookupT "doi"

replaceExact :: Eq a => [(a, a)] -> [a] -> [a]
replaceExact assoc xs = [maybe x id (lookup x assoc) | x <- xs]

-- more rigid `replace`, intended for uses where a replacement is not optional but *must* happen.
-- `replaceChecked` will error out if any of these are violated: all arguments & outputs are non-null, unique, and the replacement happened.
replaceChecked :: (Eq a, Show a) => [a] -> [a] -> [a] -> [a]
replaceChecked before after str
  | any null variables                               = error $ "replaceChecked: some argument or output was null/empty: " ++ variablesS
  | before == after || after == str || str == before = error $ "replaceChecked: arguments were not unique: " ++ variablesS
  | not (after `isInfixOf` result)                   = error $ "replaceChecked: replacement did not happen! " ++ variablesS
  | otherwise                                        = result
  where result = replace before after str
        variables = [before, after, str, result]
        variablesS = show variables
-- TODO: would it be useful to have a 'replaceDeleteStrict' which allows a "" `after` argument, since that's one of the most common use-cases?

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


-- Data.Text equivalent of System.FilePath.takeExtension
takeExtension :: T.Text -> T.Text
takeExtension = T.reverse . T.takeWhile ((/=) '.') . T.reverse


-- | 'repeated' finds only the elements that are present more than once in the list.
-- Example:
--
-- > repeated  "foo bar" == "o"
repeated :: Ord a => [a] -> [a]
repeated xs = M.keys $ M.filter (> (1::Int)) $ M.fromListWith (+) [(x,1) | x <- xs]
