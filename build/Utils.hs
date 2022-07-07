{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (group, intercalate, sort, isInfixOf, isPrefixOf, isSuffixOf, tails)
import Data.Text.IO as TIO (readFile, writeFile)
import Data.Time.Calendar (toGregorian, toModifiedJulianDay)
import Data.Time.Clock (getCurrentTime, utctDay)
import Network.URI (parseURIReference, uriAuthority, uriRegName)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (takeDirectory)
import System.IO (stderr, hPutStrLn)
import System.IO.Temp (emptySystemTempFile)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Pretty (ppShow)
import qualified Data.Text as T (Text, pack, unpack, isInfixOf, isPrefixOf, isSuffixOf, replace)

import Text.Regex (subRegex, mkRegex) -- WARNING: avoid the native Posix 'Text.Regex' due to bugs and segfaults/strange-closure GHC errors: `$ cabal install regex-compat-tdfa && ghc-pkg --user hide regex-compat-0.95.2.1`

import Text.Pandoc (def, nullMeta, runPure,
                    writerColumns, writePlain, Block, Pandoc(Pandoc), Inline(Code, Image, Link, Span, Str), Block(Para), readerExtensions, writerExtensions, readHtml, writeMarkdown, pandocExtensions)

-- Auto-update the current year.
{-# NOINLINE currentYear #-}
currentYear :: Int
currentYear = unsafePerformIO $ fmap ((\(year,_,_) -> fromInteger year) . toGregorian . utctDay) Data.Time.Clock.getCurrentTime

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


-- Add or remove a class to a Link or Span; this is a null op if the class is already present or it
-- is not a Link/Span.
addClass :: T.Text -> Inline -> Inline
addClass clss x@(Span  (i, clsses, ks) s)           = if clss `elem` clsses then x else Span (i, clss:clsses, ks) s
addClass clss x@(Link  (i, clsses, ks) s (url, tt)) = if clss `elem` clsses then x else Link (i, clss:clsses, ks) s (url, tt)
addClass clss x@(Image (i, clsses, ks) s (url, tt)) = if clss `elem` clsses then x else Image (i, clss:clsses, ks) s (url, tt)
addClass clss x@(Code  (i, clsses, ks) code)        = if clss `elem` clsses then x else Code (i, clss:clsses, ks) code
addClass _    x = x
removeClass :: T.Text -> Inline -> Inline
removeClass clss x@(Span (i, clsses, ks) s)            = if clss `notElem` clsses then x else Span (i, filter (/=clss) clsses, ks) s
removeClass clss x@(Link (i, clsses, ks) s (url, tt))  = if clss `notElem` clsses then x else Link (i, filter (/=clss) clsses, ks) s (url, tt)
removeClass clss x@(Image (i, clsses, ks) s (url, tt)) = if clss `notElem` clsses then x else Image (i, filter (/=clss) clsses, ks) s (url, tt)
removeClass clss x@(Code  (i, clsses, ks) code)        = if clss `notElem` clsses then x else Code (i, filter (/=clss) clsses, ks) code
removeClass _    x = x

-- print normal progress messages to stderr in bold green:
printGreen :: String -> IO ()
printGreen s = hPutStrLn stderr $ "\x1b[32m" ++ s ++ "\x1b[0m"

-- print danger or error messages to stderr in red background:
printRed :: String -> IO ()
printRed s = hPutStrLn stderr $ "\x1b[41m" ++ s ++ "\x1b[0m"

-- Repeatedly apply `f` to an input until the input stops changing. Show constraint for better error
-- reporting on the occasional infinite loop.
fixedPoint :: (Show a, Eq a) => (a -> a) -> a -> a
fixedPoint = fixedPoint' 10000
 where fixedPoint' :: (Show a, Eq a) => Int -> (a -> a) -> a -> a
       fixedPoint' 0 _ i = error $ "Hit recursion limit: still changing after 10,000 iterations! Infinite loop? Last result: " ++ show i
       fixedPoint' n f i = let i' = f i in if i' == i then i else fixedPoint' (n-1) f i'

sed :: String -> String -> String -> String
sed before after s = subRegex (mkRegex before) s after

-- list of regexp string rewrites
sedMany :: [(String,String)] -> (String -> String)
sedMany regexps s = foldr (uncurry sed) s regexps

-- list of fixed string rewrites
replaceMany :: [(String,String)] -> (String -> String)
replaceMany rewrites s = foldr (uncurry replace) s rewrites

-- list of fixed string rewrites
replaceManyT :: [(T.Text,T.Text)] -> (T.Text -> T.Text)
replaceManyT rewrites s = foldr (uncurry T.replace) s rewrites

-- replace/split/hasKeyAL copied from https://hackage.haskell.org/package/MissingH-1.5.0.1/docs/src/Data.List.Utils.html to avoid MissingH's dependency of regex-compat
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . split old
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
hasAny search (x:xs) = if x `elem` search then True else hasAny search xs
