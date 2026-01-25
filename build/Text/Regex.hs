{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

------
-- GWERN.NET: VENDORED FROM <https://hackage.haskell.org/package/regex-compat-tdfa-0.95.1.4/docs/src/Text-Regex.html> DUE TO CABAL DEPENDENCY VERSIONING PROBLEMS
------

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex
-- Copyright   :  (c) Chris Kuklewicz 2006, (c) shelarcy 2012, derived from (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (regex-base needs MPTC+FD)
--
-- Regular expression matching.  Uses the POSIX regular expression
-- interface in "Text.Regex.TDFA".
--
---------------------------------------------------------------------------

--
-- Modified by Chris Kuklewicz to be a thin layer over the regex-posix
-- package, and moved into a regex-compat package.
--
module Text.Regex (
    -- * Regular expressions
    Regex,
    mkRegex,
    mkRegexWithOpts,
    matchRegex,
    matchRegexAll,
    subRegex,
    splitRegex
  ) where

import Data.Array((!))
import Text.Regex.Base(RegexMaker(makeRegexOpts),defaultCompOpt,defaultExecOpt,RegexLike(matchAll,matchAllText),RegexContext(matchM),MatchText)
import Text.Regex.TDFA(Regex,caseSensitive,multiline,newSyntax)

-- | Makes a regular expression with the default options (multi-line,
-- case-sensitive).  The syntax of regular expressions is
-- otherwise that of @egrep@ (ie. POSIX \"extended\" regular
-- expressions).
mkRegex :: String -> Regex
mkRegex s = makeRegexOpts opt defaultExecOpt s
  where opt = defaultCompOpt { newSyntax = True, multiline = True }

-- | Makes a regular expression, where the multi-line and
-- case-sensitive options can be changed from the default settings.
mkRegexWithOpts
   :: String  -- ^ The regular expression to compile
   -> Bool    -- ^ 'True' @\<=>@ @\'^\'@ and @\'$\'@ match the beginning and
              -- end of individual lines respectively, and @\'.\'@ does /not/
              -- match the newline character.
   -> Bool    -- ^ 'True' @\<=>@ matching is case-sensitive
   -> Regex   -- ^ Returns: the compiled regular expression

mkRegexWithOpts s single_line case_sensitive
  = let opt = defaultCompOpt
                { multiline    = (if single_line then True else False)
                , caseSensitive = (if case_sensitive then True else False)
                , newSyntax     = True }
    in makeRegexOpts opt defaultExecOpt s

-- | Match a regular expression against a string
matchRegex
   :: Regex     -- ^ The regular expression
   -> String    -- ^ The string to match against
   -> Maybe [String]    -- ^ Returns: @'Just' strs@ if the match succeeded
                        -- (and @strs@ is the list of subexpression matches),
                        -- or 'Nothing' otherwise.
matchRegex p str = fmap (\(_,_,_,str) -> str) (matchRegexAll p str)

-- | Match a regular expression against a string, returning more information
-- about the match.
matchRegexAll
   :: Regex     -- ^ The regular expression
   -> String    -- ^ The string to match against
   -> Maybe ( String, String, String, [String] )
                -- ^ Returns: 'Nothing' if the match failed, or:
                --
                -- >  Just ( everything before match,
                -- >         portion matched,
                -- >         everything after the match,
                -- >         subexpression matches )

matchRegexAll p str = matchM p str

{- | Replaces every occurance of the given regexp with the replacement string.

In the replacement string, @\"\\1\"@ refers to the first substring;
@\"\\2\"@ to the second, etc.; and @\"\\0\"@ to the entire match.
@\"\\\\\\\\\"@ will insert a literal backslash.

This does not advance if the regex matches an empty string.  This
misfeature is here to match the behavior of the the original
Text.Regex API.
-}

subRegex :: Regex                          -- ^ Search pattern
         -> String                         -- ^ Input string
         -> String                         -- ^ Replacement text
         -> String                         -- ^ Output string
subRegex _ "" _ = ""
subRegex regexp inp repl =
  let compile _i str [] = \ _m ->  (str++)
      compile i str (("\\",(off,len)):rest) =
        let i' = off+len
            pre = take (off-i) str
            str' = drop (i'-i) str
        in if null str' then \ _m -> (pre ++) . ('\\':)
             else \  m -> (pre ++) . ('\\' :) . compile i' str' rest m
      compile i str ((xstr,(off,len)):rest) =
        let i' = off+len
            pre = take (off-i) str
            str' = drop (i'-i) str
            x = read xstr
        in if null str' then \ m -> (pre++) . ((fst (m!x))++)
             else \ m -> (pre++) . ((fst (m!x))++) . compile i' str' rest m
      compiled :: MatchText String -> String -> String
      compiled = compile 0 repl findrefs where
        -- bre matches a backslash then capture either a backslash or some digits
        bre = mkRegex "\\\\(\\\\|[0-9]+)"
        findrefs = map (\m -> (fst (m!1),snd (m ! 0))) (matchAllText bre repl)
      go _i str [] = str
      go i str (m:ms) =
        let (_,(off,len)) = m ! 0
            i' = off+len
            pre = take (off-i) str
            str' = drop (i'-i) str
        in if null str' then pre ++ (compiled m "")
             else pre ++ (compiled m (go i' str' ms))
  in go 0 inp (matchAllText regexp inp)

{- | Splits a string based on a regular expression.  The regular expression
should identify one delimiter.

This does not advance and produces an infinite list of [] if the regex
matches an empty string.  This misfeature is here to match the
behavior of the the original Text.Regex API.
-}

splitRegex :: Regex -> String -> [String]
splitRegex _ [] = []
splitRegex delim strIn =
  let matches = map (! 0) (matchAll delim strIn)
      go _i str [] = str : []
      go i str ((off,len):rest) =
        let i' = off+len
            firstline = take (off-i) str
            remainder = drop (i'-i) str
        in seq i' $
           if null remainder then [firstline,""]
             else firstline : go i' remainder rest
  in go 0 strIn matches

{-

-- These are the older versions which failed on (correct answer:)
-- let r = mkRegex "^(.)" in subRegex2 r "abc\ndef" "|\\1"
-- "|abc\n|def"

subRegex :: Regex                          -- ^ Search pattern
      -> String                         -- ^ Input string
      -> String                         -- ^ Replacement text
      -> String                         -- ^ Output string
subRegex _ "" _ = ""
subRegex regexp inp repl =
  let -- bre matches a backslash then capture either a backslash or some digits
      bre = mkRegex "\\\\(\\\\|[0-9]+)"
      lookup _ [] _ = []
      lookup [] _ _ = []
      lookup match repl groups =
        case matchRegexAll bre repl of
          Nothing -> repl
          Just (lead, _, trail, bgroups) ->
            let newval =
                 if (head bgroups) == "\\"
                   then "\\"
                   else let index :: Int
                            index = (read (head bgroups)) - 1
                        in if index == -1
                             then match
                             else groups !! index
            in lead ++ newval ++ lookup match trail groups
  in case matchRegexAll regexp inp of
       Nothing -> inp
       Just (lead, match, trail, groups) ->
         lead ++ lookup match repl groups ++ (subRegex regexp trail repl)

splitRegex :: Regex -> String -> [String]
splitRegex _ [] = []
splitRegex delim strIn = loop strIn where
  loop str = case matchOnceText delim str of
                Nothing -> [str]
                Just (firstline, _, remainder) ->
                  if null remainder
                    then [firstline,""]
                    else firstline : loop remainder

-}
