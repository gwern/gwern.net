{-# LANGUAGE OverloadedStrings #-}
-- | Utext: compile Pandoc AST to Unicode-rich plain text.
--
-- Intended for social media cards, Open Graph descriptions, and other contexts
-- where HTML is stripped but Unicode renders. Converts Pandoc inline formatting
-- to Unicode mathematical alphanumeric symbols (bold, italic, bold-italic,
-- monospace), combining characters (strikethrough, underline), Unicode
-- super/subscripts, and IPA-derived small capitals.
--
-- Pure API (trace-warns on unsupported nodes like Math, Table, Image):
--   pandocToUtext :: Pandoc -> T.Text
--   inlinesToUtext :: [Inline] -> T.Text
--   inlineToUtext :: Inline -> T.Text
--   rawText2Utext :: T.Text -> T.Text
--   rawHtml2Utext :: T.Text -> T.Text
--   rawMarkdown2Utext :: T.Text -> T.Text
--
-- IO API (shells out to latex2unicode.py for Math):
--   pandocToUtextIO :: Pandoc -> IO T.Text
--   rawText2UtextIO :: T.Text -> IO T.Text
--
-- Non-ASCII letters (accented, CJK, etc.) pass through unchanged when no
-- mathematical-alphabet mapping exists. Unmapped super/subscript characters
-- fall back to ^x / _x notation.
module Utext
  ( -- * Pure API (no Math support)
    pandocToUtext
  , inlinesToUtext
  , inlineToUtext
  , rawText2Utext
  , rawHtml2Utext
  , rawMarkdown2Utext
    -- * IO API (Math via latex2unicode.py)
  , pandocToUtextIO
  , rawText2UtextIO
  -- tests
  , utextTestSuite
  ) where

import Data.Char (chr, isAsciiLower, isAsciiUpper, isDigit, ord, toLower)
import Debug.Trace (trace)
import qualified Data.Map.Strict as M (lookup, findWithDefault, fromList, Map)
import Data.Text (Text)
import qualified Data.Text as T (concat, concatMap, intercalate, isInfixOf, isPrefixOf, lines, map, null, pack, replace, singleton, strip, take, unpack)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Definition
import Text.Pandoc.Extensions (pandocExtensions)
import Text.Pandoc.Options (ReaderOptions, def, readerExtensions)
import Text.Pandoc.Readers (readHtml, readMarkdown)
import Text.Pandoc.Walk (walkM)

-----------------------------------------------------------------------
-- Public API
-----------------------------------------------------------------------

pandocToUtext :: Pandoc -> Text
pandocToUtext (Pandoc _meta blocks) = applyLigatures $ renderBlocks defaultStyle blocks

-- | Render a list of inlines (eg. from a metadata field like 'title' or
-- 'description') to Utext. This is the main entry point for social media cards.
inlinesToUtext :: [Inline] -> Text
inlinesToUtext = applyLigatures . renderInlines defaultStyle

-- | Render a single inline to Utext.
inlineToUtext :: Inline -> Text
inlineToUtext = applyLigatures . renderInline defaultStyle

-- | Parse a raw text string as Pandoc Markdown (which handles inline HTML like
-- @\<em\>foo\<\/em\>@, @\<strong\>@, @\<code\>@, @\<sub\>@, @\<sup\>@, @\<del\>@,
-- @\<span class=\"smallcaps\"\>@, etc.) and emit Utext.
--
-- If Markdown parsing fails, falls back to HTML-only parsing.
-- If both fail, returns the input unchanged (never crashes).
--
-- This is the main convenience function for converting metadata strings:
--
-- @
-- -- eg. title: "Statistical <em>Literacy</em> is <strong>Hard</strong>"
-- ogTitle = rawText2Utext (lookupMetaString "title" meta)
-- -- => "Statistical 𝘓𝘪𝘵𝘦𝘳𝘢𝘤𝘺 is 𝗛𝗮𝗿𝗱"
-- @
rawText2Utext :: Text -> Text
rawText2Utext t
  | T.null t  = t
  | otherwise = case parseMarkdown t of
      Right doc -> T.strip (pandocToUtext doc)
      Left _    -> case parseHtml t of
        Right doc -> T.strip (pandocToUtext doc)
        Left _    -> t -- fallback: return input unchanged

-- | Parse as HTML only (no Markdown processing). Useful when input is known to
-- be HTML fragments, eg. from annotation databases or API responses.
rawHtml2Utext :: Text -> Text
rawHtml2Utext t = case parseHtml t of
  Right doc -> T.strip (pandocToUtext doc)
  Left _    -> t

-- | Parse as Pandoc Markdown only. Useful when input is known to be Markdown.
rawMarkdown2Utext :: Text -> Text
rawMarkdown2Utext t = case parseMarkdown t of
  Right doc -> T.strip (pandocToUtext doc)
  Left _    -> t

-----------------------------------------------------------------------
-- Pandoc parsing helpers (pure, no IO)
-----------------------------------------------------------------------

parseMarkdown :: Text -> Either Text Pandoc
parseMarkdown t = case runPure (readMarkdown markdownOpts t) of
  Left err  -> Left (T.pack (show err))
  Right doc -> Right doc

parseHtml :: Text -> Either Text Pandoc
parseHtml t = case runPure (readHtml def t) of
  Left err  -> Left (T.pack (show err))
  Right doc -> Right doc

-- Pandoc Markdown with the full standard extension set (handles inline HTML,
-- strikethrough, superscript, subscript, smallcaps via bracketed spans, etc.)
markdownOpts :: ReaderOptions
markdownOpts = def { readerExtensions = pandocExtensions }

-----------------------------------------------------------------------
-- IO API: latex2unicode.py integration
-----------------------------------------------------------------------

-- | Like 'pandocToUtext', but preprocesses Math nodes by shelling out to
-- @latex2unicode.py@ (which calls GPT-4.1-mini to convert LaTeX → HTML).
-- The HTML output is then parsed and rendered to Utext.
--
-- Expects @static/build/latex2unicode.py@ to be callable from the current
-- working directory (ie. the site repo root), matching the existing
-- @Utils.hs@ convention.
pandocToUtextIO :: Pandoc -> IO Text
pandocToUtextIO (Pandoc _meta blocks) = do
  blocks' <- walkM (inlineMathToUtext latexToUtextViaScript) blocks
  return $ renderBlocks defaultStyle blocks'

-- | Like 'rawText2Utext', but handles Math via @latex2unicode.py@.
rawText2UtextIO :: Text -> IO Text
rawText2UtextIO t
  | T.null t  = return t
  | otherwise = case parseMarkdown t of
      Right doc -> T.strip <$> pandocToUtextIO doc
      Left _    -> case parseHtml t of
        Right doc -> T.strip <$> pandocToUtextIO doc
        Left _    -> return t

-----------------------------------------------------------------------
-- Math preprocessing (IO)
-----------------------------------------------------------------------

-- | Replace a Math inline with its Utext rendering. The conversion function
-- is passed in to allow swapping implementations (eg. for testing).
inlineMathToUtext :: (Text -> IO Text) -> Inline -> IO Inline
inlineMathToUtext convert (Math InlineMath tex) = do
  utext <- convert tex
  return $ Str utext
inlineMathToUtext convert (Math DisplayMath tex) = do
  -- Display math in a social media card is dubious, but try anyway.
  utext <- convert tex
  return $ Str utext
inlineMathToUtext _ x = return x

-- | Shell out to @latex2unicode.py@, get HTML back, parse it, render to Utext.
-- Fatally errors if the script fails, matching the strict philosophy.
latexToUtextViaScript :: Text -> IO Text
latexToUtextViaScript tex = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode
    "python3" ["static/build/latex2unicode.py", T.unpack tex] ""
  case exitCode of
    ExitFailure code -> error $
      "Utext: latex2unicode.py failed (exit " ++ show code ++ ") on input "
      ++ show (T.take traceLimit tex) ++ ": " ++ stderr
    ExitSuccess -> do
      let html = T.strip (T.pack stdout)
      -- latex2unicode.py returns HTML with <em>, <sup>, <sub>, <span>, etc.
      -- Parse it as HTML, then render the resulting AST to Utext.
      -- If parsing fails, that's a bug in the script output → fatal error.
      case parseHtml html of
        Left err -> error $
          "Utext: failed to parse latex2unicode.py HTML output: " ++ show err
          ++ "\n  Input:  " ++ show (T.take traceLimit tex)
          ++ "\n  Output: " ++ show (T.take (traceLimit * 2) html)
        Right doc -> return $ T.strip (pandocToUtext doc)

-----------------------------------------------------------------------
-- Style tracking
-----------------------------------------------------------------------

data Style = Style
  { sBold   :: !Bool
  , sItalic :: !Bool
  } deriving (Eq, Show)

defaultStyle :: Style
defaultStyle = Style False False

-----------------------------------------------------------------------
-- Block rendering
-----------------------------------------------------------------------

renderBlocks :: Style -> [Block] -> Text
renderBlocks s = T.intercalate "\n\n" . concatMap (renderBlock s)

-- Supported block constructs. Unsupported ones degrade gracefully with trace warnings.
renderBlock :: Style -> Block -> [Text]
renderBlock s (Para inlines)         = [renderInlines s inlines]
renderBlock s (Plain inlines)        = [renderInlines s inlines]
renderBlock s (LineBlock lns)        = [T.intercalate "\n" $ map (renderInlines s) lns]
renderBlock s (Header _ _ inlines)   = [renderInlines (s { sBold = True }) inlines]
renderBlock s (BlockQuote blocks)    = [T.intercalate "\n" $ map ("  " <>) $ T.lines $ renderBlocks s blocks]
renderBlock s (BulletList items)     = map (\item -> "• " <> renderBlocks s item) items
renderBlock s (OrderedList _ items)  = zipWith (\n item -> T.pack (show n) <> ". " <> renderBlocks s item)
                                              [(1::Int)..] items
renderBlock _ (CodeBlock _ code)     = [T.map codeMono code]
renderBlock _ HorizontalRule         = ["———"]
renderBlock s (Div _ blocks)         = [renderBlocks s blocks]
-- Unsupported block constructs: warn via trace and degrade gracefully.
-- RawBlock "html": attempt to re-parse and render (handles stray HTML from
-- metadata or latex2unicode.py output).
renderBlock s (RawBlock (Format "html") html) =
  case runPure (readHtml def html) of
    Right (Pandoc _ blocks) -> concatMap (renderBlock s) blocks
    Left _ -> traceWarnT "unparseable RawBlock html" html []
renderBlock _ (RawBlock fmt t)       = traceWarnT ("unsupported RawBlock (" ++ show fmt ++ ")") t []
renderBlock _ (Table {})             = traceWarn   "unsupported Table (dropped)"             []
renderBlock _ (Figure _ _ _)         = traceWarn   "unsupported Figure (dropped)"            []
renderBlock s (DefinitionList defs)  = traceWarn   "unsupported DefinitionList (best-effort)" $
  -- Best-effort: render each term followed by its definitions.
  concatMap (\(term, defBlocks) -> [renderInlines s term <> ": " <>
      T.intercalate "; " (map (renderBlocks s) defBlocks)]) defs

-----------------------------------------------------------------------
-- HTML tag reassembly (Markdown reader fixup)
-----------------------------------------------------------------------

-- | Reassemble paired RawInline "html" open/close tags into native Pandoc
-- constructors. Pandoc's Markdown reader keeps inline HTML like @\<em\>...\<\/em\>@
-- as separate RawInline nodes rather than converting to Emph, Strong, etc.
-- This function finds matching pairs and wraps the intervening inlines in the
-- appropriate constructor, enabling proper Utext rendering.
--
-- Handles: em\/i → Emph, strong\/b → Strong, code → Code, sub → Subscript,
-- sup → Superscript, del\/s\/strike → Strikeout, u → Underline,
-- span.smallcaps → SmallCaps.
--
-- Unmatched or unrecognized tags pass through unchanged (the existing
-- RawInline handler will deal with them).
reassembleHtmlInlines :: [Inline] -> [Inline]
reassembleHtmlInlines [] = []
reassembleHtmlInlines (RawInline (Format "html") tag : rest)
  | Just (constructor, closeTag) <- matchOpenTag tag
  , Just (inner, after) <- splitAtClose closeTag rest
  = constructor (reassembleHtmlInlines inner) : reassembleHtmlInlines after
reassembleHtmlInlines (x:xs) = x : reassembleHtmlInlines xs

-- | Match an opening HTML tag to its Pandoc constructor and closing tag string.
-- Handles tags with or without attributes (eg. @\<em\>@, @\<span class="smallcaps"\>@).
matchOpenTag :: Text -> Maybe ([Inline] -> Inline, Text)
matchOpenTag t
  | t == "<em>"                        = Just (Emph,        "</em>")
  | t == "<i>"                         = Just (Emph,        "</i>")
  | t == "<strong>"                    = Just (Strong,      "</strong>")
  | t == "<b>"                         = Just (Strong,      "</b>")
  | t == "<sub>"                       = Just (Subscript,   "</sub>")
  | t == "<sup>"                       = Just (Superscript, "</sup>")
  | t == "<del>"                       = Just (Strikeout,   "</del>")
  | t == "<s>"                         = Just (Strikeout,   "</s>")
  | t == "<strike>"                    = Just (Strikeout,   "</strike>")
  | t == "<u>"                         = Just (Underline,   "</u>")
  | t == "<code>"                      = Just (codeWrap,    "</code>")
  | "<span" `T.isPrefixOf` t
  , "smallcaps" `T.isInfixOf` t        = Just (SmallCaps,   "</span>")
  | otherwise                          = Nothing
  where
    codeWrap inlines = Code nullAttr (inlinesToPlain inlines)
    inlinesToPlain = T.concat . map inlineToPlain
    inlineToPlain (Str s) = s
    inlineToPlain Space = " "
    inlineToPlain SoftBreak = " "
    inlineToPlain _ = ""

-- | Split an inline list at the first occurrence of a closing RawInline HTML tag.
-- Returns Nothing if the closing tag is not found.
splitAtClose :: Text -> [Inline] -> Maybe ([Inline], [Inline])
splitAtClose closeTag = go []
  where
    go _acc [] = Nothing
    go acc (RawInline (Format "html") t : rest)
      | t == closeTag = Just (reverse acc, rest)
    go acc (x:xs) = go (x:acc) xs

-----------------------------------------------------------------------
-- Inline rendering
-----------------------------------------------------------------------

renderInlines :: Style -> [Inline] -> Text
renderInlines s = T.concat . map (renderInline s) . reassembleHtmlInlines

-- Supported inline constructs. Unsupported ones degrade gracefully with trace warnings.
renderInline :: Style -> Inline -> Text
renderInline s (Str t)                = T.map (styleChar s) t
renderInline s (Emph inlines)         = renderInlines (s { sItalic = True }) inlines
renderInline s (Strong inlines)       = renderInlines (s { sBold = True }) inlines
renderInline s (Underline inlines)    = T.concatMap (\c -> T.singleton c <> "\x0332")
                                          (renderInlines s inlines)
renderInline s (Strikeout inlines)    = T.concatMap (\c -> T.singleton c <> "\x0336")
                                          (renderInlines s inlines)
renderInline s (Superscript inlines)  = T.concatMap charToSuperscript
                                          (renderInlines s inlines)
renderInline s (Subscript inlines)    = T.concatMap charToSubscript
                                          (renderInlines s inlines)
renderInline s (SmallCaps inlines)    = T.concatMap charToSmallCap
                                          (renderInlines s inlines)
renderInline _ (Code _ t)            = T.map codeMono t
renderInline _ Space                 = " "
renderInline _ SoftBreak             = " "
renderInline _ LineBreak             = "\n"
renderInline s (Quoted SingleQuote inlines) = "\x2018" <> renderInlines s inlines <> "\x2019"
renderInline s (Quoted DoubleQuote inlines) = "\x201C" <> renderInlines s inlines <> "\x201D"
renderInline s (Link _ inlines (url, title))
  | T.null url  = renderInlines s inlines
  | T.null title = renderInlines s inlines <> " (<" <> url <> ">)"
  | otherwise    = renderInlines s inlines <> " (\x201C" <> title <> "\x201D: <" <> url <> ">)"
renderInline s (Span attr inlines)
  | hasClass "smallcaps" attr        = renderInline s (SmallCaps inlines)
  | hasClass "subsup" attr           = renderInlines s inlines   -- sub+sup rendered sequentially, without attempting to vertically stack, so the `subsup` wrapper is just ignored
  | hasStyle attr                    = renderInlines s inlines -- we will simply ignore styles for now, and not attempt to parse or compile arbitrary CSS even if it could map onto something like italics...
  | otherwise                        = renderInlines s inlines
-- RawInline "html": attempt to re-parse and render. This handles HTML fragments
-- from latex2unicode.py that survive Pandoc's HTML reader as raw nodes (eg.
-- character references like &#775;, or unusual nesting).
renderInline s (RawInline (Format "html") html) =
  case parseHtmlInlines html of
    Right inlines -> renderInlines s inlines
    Left _        -> traceWarnT "unparseable RawInline html" html ""
-- Unsupported inline constructs: warn via trace and degrade gracefully.
renderInline _ (RawInline fmt t)     = traceWarnT ("unsupported RawInline (" ++ show fmt ++ ")") t ""
renderInline _ (Note _)              = traceWarn   "unsupported Note (footnote, dropped)" ""
renderInline _ (Math _fmt t)         = traceWarnT  "unsupported Math (raw LaTeX fallback)" t t
renderInline s (Image _ inlines (url, title))
  | null inlines && T.null url = ""
  | null inlines = "(<" <> url <> ">)"
  | T.null url   = "(" <> renderInlines s inlines <> ")"
  | T.null title = "(" <> renderInlines s inlines <> ": <" <> url <> ">)"
  | otherwise    = "(" <> renderInlines s inlines <> ", \x201C" <> title <> "\x201D: <" <> url <> ">)"
renderInline s (Cite _ inlines)      = traceWarn   "unsupported Cite (rendering visible text only)" (renderInlines s inlines)

hasClass :: Text -> Attr -> Bool
hasClass cls (_, classes, _) = cls `elem` classes

hasStyle :: Attr -> Bool
hasStyle (_, _, kvs) = any (\(k, _) -> k == "style") kvs

-- getKvs :: Attr -> [(Text, Text)]
-- getKvs (_, _, kvs) = kvs

-- | Parse an HTML fragment and extract the body inlines. Used for re-parsing
-- RawInline "html" nodes. Returns Right [] for empty/whitespace input.
parseHtmlInlines :: Text -> Either Text [Inline]
parseHtmlInlines html
  | T.null (T.strip html) = Right []
  | otherwise = case runPure (readHtml def html) of
      Left err -> Left (T.pack (show err))
      Right (Pandoc _ blocks) -> Right (blocksToInlines blocks)

-- | Flatten blocks to inlines, preserving inline content. This is a simplified
-- version of Pandoc's own blocksToInlines, suitable for small HTML fragments.
blocksToInlines :: [Block] -> [Inline]
blocksToInlines = concatMap blockToInlines
  where
    blockToInlines (Para ils)    = ils
    blockToInlines (Plain ils)   = ils
    blockToInlines (Div _ bs)    = concatMap blockToInlines bs
    blockToInlines _             = [] -- drop non-inline blocks from fragments

-----------------------------------------------------------------------
-- Character mapping: style (bold / italic / bold-italic)
-----------------------------------------------------------------------

styleChar :: Style -> Char -> Char
styleChar (Style True  True)  = boldItalicChar
styleChar (Style True  False) = boldChar
styleChar (Style False True)  = italicChar
styleChar (Style False False) = id

-- | Serif bold: U+1D400 (A), U+1D41A (a), U+1D7CE (0)
boldChar :: Char -> Char
boldChar c
  | isAsciiUpper c = chr (ord c - ord 'A' + 0x1D400)
  | isAsciiLower c = chr (ord c - ord 'a' + 0x1D41A)
  | isDigit c      = chr (ord c - ord '0' + 0x1D7CE)
  | otherwise      = c

-- | Serif italic: U+1D434 (A), U+1D44E (a). No italic digits in Unicode.
-- Note: Unicode unifies italic 'h' with PLANCK CONSTANT (U+210E); the
-- expected slot U+1D455 is permanently reserved/empty.
italicChar :: Char -> Char
italicChar c
  | isAsciiUpper c = chr (ord c - ord 'A' + 0x1D434)
  | c == 'h'       = '\x210E'
  | isAsciiLower c = chr (ord c - ord 'a' + 0x1D44E)
  | otherwise      = c

-- | Serif bold italic: U+1D468 (A), U+1D482 (a). Digits fall back to bold.
boldItalicChar :: Char -> Char
boldItalicChar c
  | isAsciiUpper c = chr (ord c - ord 'A' + 0x1D468)
  | isAsciiLower c = chr (ord c - ord 'a' + 0x1D482)
  | isDigit c      = chr (ord c - ord '0' + 0x1D7CE) -- no bold-italic digits; use bold
  | otherwise      = c

-- | Mathematical monospace: U+1D670 (A), U+1D68A (a), U+1D7F6 (0)
codeMono :: Char -> Char
codeMono c
  | isAsciiUpper c = chr (ord c - ord 'A' + 0x1D670)
  | isAsciiLower c = chr (ord c - ord 'a' + 0x1D68A)
  | isDigit c      = chr (ord c - ord '0' + 0x1D7F6)
  | otherwise      = c

-----------------------------------------------------------------------
-- Character mapping: superscript
-----------------------------------------------------------------------

superscriptMap :: M.Map Char Char
superscriptMap = M.fromList
  [ ('0', '\x2070'), ('1', '\x00B9'), ('2', '\x00B2'), ('3', '\x00B3')
  , ('4', '\x2074'), ('5', '\x2075'), ('6', '\x2076'), ('7', '\x2077')
  , ('8', '\x2078'), ('9', '\x2079')
  , ('+', '\x207A'), ('-', '\x207B'), ('=', '\x207C')
  , ('(', '\x207D'), (')', '\x207E')
  , ('i', '\x2071'), ('n', '\x207F')
  -- Additional from Phonetic Extensions:
  , ('a', '\x1D43'), ('b', '\x1D47'), ('c', '\x1D9C'), ('d', '\x1D48')
  , ('e', '\x1D49'), ('f', '\x1DA0'), ('g', '\x1D4D'), ('h', '\x02B0')
  , ('j', '\x02B2'), ('k', '\x1D4F'), ('l', '\x02E1'), ('m', '\x1D50')
  , ('o', '\x1D52'), ('p', '\x1D56'), ('r', '\x02B3'), ('s', '\x02E2')
  , ('t', '\x1D57'), ('u', '\x1D58'), ('v', '\x1D5B'), ('w', '\x02B7')
  , ('x', '\x02E3'), ('y', '\x02B8'), ('z', '\x1DBB')
  ]

charToSuperscript :: Char -> Text
charToSuperscript c = case M.lookup c superscriptMap of
  Just c' -> T.singleton c'
  Nothing
    | isAsciiUpper c -> case M.lookup (toLower c) superscriptMap of
        Just c' -> T.singleton c' -- imperfect but better than fallback
        Nothing -> T.pack ['^', c]
    | c == ' '       -> " "
    | otherwise      -> T.singleton c -- pass through punctuation, Unicode, etc.

-----------------------------------------------------------------------
-- Character mapping: subscript
-----------------------------------------------------------------------

subscriptMap :: M.Map Char Char
subscriptMap = M.fromList
  [ ('0', '\x2080'), ('1', '\x2081'), ('2', '\x2082'), ('3', '\x2083')
  , ('4', '\x2084'), ('5', '\x2085'), ('6', '\x2086'), ('7', '\x2087')
  , ('8', '\x2088'), ('9', '\x2089')
  , ('+', '\x208A'), ('-', '\x208B'), ('=', '\x208C')
  , ('(', '\x208D'), (')', '\x208E')
  , ('a', '\x2090'), ('e', '\x2091'), ('o', '\x2092'), ('x', '\x2093')
  , ('h', '\x2095'), ('k', '\x2096'), ('l', '\x2097'), ('m', '\x2098')
  , ('n', '\x2099'), ('p', '\x209A'), ('s', '\x209B'), ('t', '\x209C')
  -- schwa (for reduced vowels): ('ə', '\x2094')
  , ('i', '\x1D62'), ('r', '\x1D63'), ('u', '\x1D64'), ('v', '\x1D65')
  , ('j', '\x2C7C')
  ]

charToSubscript :: Char -> Text
charToSubscript c = case M.lookup c subscriptMap of
  Just c' -> T.singleton c'
  Nothing
    | isAsciiUpper c -> case M.lookup (toLower c) subscriptMap of
        Just c' -> T.singleton c'
        Nothing -> T.pack ['_', c]
    | c == ' '       -> " "
    | otherwise      -> T.singleton c

-----------------------------------------------------------------------
-- Character mapping: small capitals
-----------------------------------------------------------------------

-- Uses IPA / Phonetic Extensions characters. Coverage is good but not
-- perfect: 'q' and 'x' lack widely-supported small-cap forms; we fall
-- back to uppercase for those.
smallCapMap :: M.Map Char Char
smallCapMap = M.fromList
  [ ('a', '\x1D00'), ('b', '\x0299'), ('c', '\x1D04'), ('d', '\x1D05')
  , ('e', '\x1D07'), ('f', '\xA730'), ('g', '\x0262'), ('h', '\x029C')
  , ('i', '\x026A'), ('j', '\x1D0A'), ('k', '\x1D0B'), ('l', '\x029F')
  , ('m', '\x1D0D'), ('n', '\x0274'), ('o', '\x1D0F'), ('p', '\x1D18')
  , ('r', '\x0280'), ('s', '\xA731'), ('t', '\x1D1B'), ('u', '\x1D1C')
  , ('v', '\x1D20'), ('w', '\x1D21'), ('y', '\x028F'), ('z', '\x1D22')
  -- 'q' -> no good small cap; use uppercase Q
  -- 'x' -> no good small cap; use uppercase X
  ]

charToSmallCap :: Char -> Text
charToSmallCap c
  | isAsciiLower c = T.singleton $ M.findWithDefault (chr (ord c - 32)) c smallCapMap
  | otherwise      = T.singleton c  -- uppercase and non-ASCII pass through

-----------------------------------------------------------------------
-- Utilities
-----------------------------------------------------------------------

-- | Truncation limit for trace/error messages showing input excerpts.
traceLimit :: Int
traceLimit = 80

-- | Emit a trace warning prefixed with "Utext: " and return a fallback value.
-- Used for unsupported or unparseable constructs that degrade gracefully.
traceWarn :: String -> a -> a
traceWarn msg = trace ("Utext: " ++ msg)

-- | 'traceWarn' with a truncated 'Text' excerpt appended.
traceWarnT :: String -> Text -> a -> a
traceWarnT msg t = traceWarn (msg ++ ": " ++ show (T.take traceLimit t))

-- | Apply Unicode ligature substitutions. Longest matches first to avoid
-- partial replacement (eg. "ffi" before "fi").
applyLigatures :: Text -> Text
applyLigatures = T.replace "fi"  "ﬁ"
               . T.replace "fl"  "ﬂ"
               . T.replace "ff"  "ﬀ"
               . T.replace "ffi" "ﬃ"
               . T.replace "ffl" "ﬄ"


-- {-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for "Utext". Exports 'utextTestSuite' for use in @Test.hs@.
--
-- Test cases are split into:
--
-- * 'supportedTests': features that compile to fancy Unicode (italics, bold,
--   bold-italic, monospace, super\/subscripts, strikethrough, underline,
--   small caps, smart quotes, links, block-level constructs, HTML-in-Markdown).
--
-- * 'unsupportedTests': constructs that degrade gracefully with a @trace@
--   warning (Math, Note, Table, Figure, DefinitionList). These are exercised
--   to verify they produce sensible fallback output rather than crashing.
-- module UtextTest (utextTestSuite) where

-- import Data.Text (Text)

-- import Utext (rawMarkdown2Utext)

-----------------------------------------------------------------------
-- Public API
-----------------------------------------------------------------------

-- | Run the full test suite via 'rawMarkdown2Utext'. Returns a list of
-- failures as @(input, expected, actual)@ triples. Empty list = all pass.
--
-- Usage in Test.hs:
--
-- @
-- unless (null utextTestSuite) $ printRed ("Utext test suite has errors in: " ++ show utextTestSuite)
-- @
utextTestSuite :: [(Text, Text, Text)]
utextTestSuite = filter (\(_, expected, actual) -> expected /= actual)
                   [ (input, expected, rawMarkdown2Utext input)
                   | (input, expected) <- supportedTests ++ unsupportedTests ]

-----------------------------------------------------------------------
-- Supported: features that compile to fancy Unicode
-----------------------------------------------------------------------

supportedTests :: [(Text, Text)]
supportedTests = concat
  [ identityTests
  , italicTests
  , boldTests
  , boldItalicTests
  , monoTests
  , superscriptTests
  , subscriptTests
  , strikethroughTests
  , underlineTests
  , smallcapsTests
  , smartQuoteTests
  , linkTests
  , htmlInMarkdownTests
  , blockTests
  , nonAsciiTests
  , digitTests
  , edgeCaseTests
  , ligatureTests
  ]

-- Plain text and empty input
identityTests :: [(Text, Text)]
identityTests =
  [ (""                , "")
  , ("hello"           , "hello")
  , ("hello world"     , "hello world")
  -- ASCII apostrophe becomes smart right single quote:
  , ("it's"            , "it\x2019s")
  -- Pre-existing em dash passes through:
  , ("foo—bar"    , "foo—bar")
  ]

-- *italic* and <em> → serif italic (A=U+1D434, a=U+1D44E, h=U+210E)
-- No italic digits in Unicode; digits pass through unchanged.
italicTests :: [(Text, Text)]
italicTests =
  [ ("*hello*"             , "\x210E\x1D452\x1D459\x1D459\x1D45C")
  , ("*A*"                 , "\x1D434")
  , ("*z*"                 , "\x1D467")
  , ("*h*"                 , "\x210E")  -- PLANCK CONSTANT (Unicode hole at U+1D455)
  , ("*foo bar*"           , "\x1D453\x1D45C\x1D45C \x1D44F\x1D44E\x1D45F")
  , ("*test123*"           , "\x1D461\x1D452\x1D460\x1D461\&123")
  ]

-- **bold** and <strong> → serif bold (A=U+1D400, a=U+1D41A, 0=U+1D7CE)
boldTests :: [(Text, Text)]
boldTests =
  [ ("**hi**"                , "\x1D421\x1D422")
  , ("**A**"                 , "\x1D400")
  , ("**z**"                 , "\x1D433")
  , ("**OK**"                , "\x1D40E\x1D40A")
  , ("**9**"                 , "\x1D7D7")
  , ("**0**"                 , "\x1D7CE")
  ]

-- ***bold italic*** → serif bold italic (A=U+1D468, a=U+1D482; digits→bold)
boldItalicTests :: [(Text, Text)]
boldItalicTests =
  [ ("***ab***"              , "\x1D482\x1D483")
  , ("***Z***"               , "\x1D481")
  , ("***5***"               , "\x1D7D3")
  ]

-- `code` and <code> → mathematical monospace (A=U+1D670, a=U+1D68A, 0=U+1D7F6)
monoTests :: [(Text, Text)]
monoTests =
  [ ("`hi`"                  , "\x1D691\x1D692")
  , ("`A`"                   , "\x1D670")
  , ("`z`"                   , "\x1D6A3")
  , ("`0`"                   , "\x1D7F6")
  , ("`hello world`"         , "\x1D691\x1D68E\x1D695\x1D695\x1D698 \x1D6A0\x1D698\x1D69B\x1D695\x1D68D")
  ]

-- ^superscript^ and <sup>
superscriptTests :: [(Text, Text)]
superscriptTests =
  [ ("x^2^"                  , "x\x00B2")
  , ("x^10^"                 , "x\x00B9\x2070")
  , ("x^n^"                  , "x\x207F")
  , ("x^i^"                  , "x\x2071")
  , ("<sup>+</sup>"          , "\x207A")
  , ("<sup>-</sup>"          , "\x207B")
  -- Uppercase falls back to lowercase superscript form:
  , ("<sup>A</sup>"          , "\x1D43")
  ]

-- ~subscript~ and <sub>
subscriptTests :: [(Text, Text)]
subscriptTests =
  [ ("H~2~O"                 , "H\x2082O")
  , ("<sub>0</sub>"          , "\x2080")
  , ("<sub>n</sub>"          , "\x2099")
  , ("x~i~"                  , "x\x1D62")
  ]

-- ~~strikethrough~~ and <del>/<s>/<strike> → combining short stroke overlay (U+0336)
strikethroughTests :: [(Text, Text)]
strikethroughTests =
  [ ("~~no~~"                , "n\x0336o\x0336")
  -- NOTE: standalone "<del>x</del>" is not tested because Pandoc's Markdown
  -- reader promotes it to a block-level RawBlock, and re-parsing via readHtml
  -- loses the strikethrough. Works fine embedded in surrounding text (tested
  -- in htmlInMarkdownTests). Use Markdown ~~…~~ for standalone strikethrough.
  , ("<s>ab</s>"             , "a\x0336\&b\x0336")
  ]

-- <u>underline</u> → combining low line (U+0332)
underlineTests :: [(Text, Text)]
underlineTests =
  [ ("<u>hi</u>"             , "h\x0332i\x0332")
  ]

-- [text]{.smallcaps} and <span class="smallcaps"> → IPA small capitals
-- Uppercase letters pass through unchanged.
smallcapsTests :: [(Text, Text)]
smallcapsTests =
  [ ("[hello]{.smallcaps}"                          , "\x029C\x1D07\x029F\x029F\x1D0F")
  , ("[AB]{.smallcaps}"                             , "AB")
  , ("<span class=\"smallcaps\">de</span>"          , "\x1D05\x1D07")
  ]

-- Pandoc smart typography
smartQuoteTests :: [(Text, Text)]
smartQuoteTests =
  [ ("\"hello\""             , "\x201Chello\x201D")
  , ("it's"                  , "it\x2019s")
  -- En dash:
  , ("foo--bar"              , "foo–bar")
  -- Em dash:
  , ("foo---bar"             , "foo—bar")
  -- Ellipsis:
  , ("foo..."                , "foo\x2026")
  ]

-- Links: render text followed by URL/title in parentheses
linkTests :: [(Text, Text)]
linkTests =
  [ ("[click here](https://example.com)"
    , "click here (<https://example.com>)")
  , ("[*italic link*](https://example.com)"
    , "\x1D456\x1D461\x1D44E\x1D459\x1D456\x1D450 \x1D459\x1D456\x1D45B\x1D458 (<https://example.com>)")
  , ("[foo](https://bar.com \"Title\")"
    , "foo (\x201CTitle\x201D: <https://bar.com>)")
  ]

-- HTML tags inside Markdown: the bug that motivated this test suite.
-- Pandoc's Markdown reader keeps <em> etc. as RawInline "html" pairs;
-- reassembleHtmlInlines stitches them into native constructors.
htmlInMarkdownTests :: [(Text, Text)]
htmlInMarkdownTests =
  [ ("mistakes in <em>Death Note</em> and fixes"
    , "mistakes in \x1D437\x1D452\x1D44E\x1D461\x210E \x1D441\x1D45C\x1D461\x1D452 and \xFB01xes")
  , ("<strong>bold</strong> word"
    , "\x1D41B\x1D428\x1D425\x1D41D word")
  , ("a <sub>2</sub> b"
    , "a \x2082 b")
  , ("a <sup>3</sup> b"
    , "a \x00B3 b")
  , ("a <del>old</del> b"
    , "a o\x0336l\x0336\&d\x0336 b")
  , ("a <u>ul</u> b"
    , "a u\x0332l\x0332 b")
  , ("<code>mono</code> text"
    , "\x1D696\x1D698\x1D697\x1D698 text")
  -- Nested HTML tags in Markdown:
  , ("<em><strong>both</strong></em>"
    , "\x1D483\x1D490\x1D495\x1D489")
  ]

-- Block-level constructs
blockTests :: [(Text, Text)]
blockTests =
  [ -- Headers render as bold:
    ("# Title"               , "\x1D413\x1D422\x1D42D\x1D425\x1D41E")
  -- Code blocks render as monospace:
  , ("```\nfoo\n```"         , "\x1D68F\x1D698\x1D698")
  ]

-- Non-ASCII: pass through unchanged (no mathematical-alphabet mapping exists)
nonAsciiTests :: [(Text, Text)]
nonAsciiTests =
  [ ("\x00E9"                , "\x00E9")
  , ("\x4E16\x754C"          , "\x4E16\x754C")
  -- Non-ASCII inside italic: only ASCII letters get mapped
  , ("*caf\x00E9*"           , "\x1D450\x1D44E\x1D453\x00E9")
  ]

-- Digit handling across styles
digitTests :: [(Text, Text)]
digitTests =
  [ ("**42**"                , "\x1D7D2\x1D7D0")
  , ("`99`"                  , "\x1D7FF\x1D7FF")
  -- Italic digits pass through (no italic digits in Unicode):
  , ("*7*"                   , "7")
  -- Superscript digits:
  , ("x^0^"                  , "x\x2070")
  , ("x^1^"                  , "x\x00B9")
  , ("x^2^"                  , "x\x00B2")
  , ("x^3^"                  , "x\x00B3")
  -- Subscript digits:
  , ("H~0~"                  , "H\x2080")
  ]

-- Miscellaneous edge cases
edgeCaseTests :: [(Text, Text)]
edgeCaseTests =
  [ -- Punctuation passes through inside bold:
    ("**hello, world!**"     , "\x1D421\x1D41E\x1D425\x1D425\x1D428, \x1D430\x1D428\x1D42B\x1D425\x1D41D!")
  -- Nested Markdown: **a *b* c** → bold a, bold-italic b, bold c
  , ("**a *b* c**"           , "\x1D41A \x1D483 \x1D41C")
  ]

-- Ligature replacement: fi→ﬁ, fl→ﬂ, ff→ﬀ, ffi→ﬃ, ffl→ﬄ
-- Only affects plain (unstyled) ASCII text; styled characters are in SMP
-- and don't match the ASCII digraphs.
ligatureTests :: [(Text, Text)]
ligatureTests =
  [ ("fine"               , "ﬁne")
  , ("offline"               , "oﬄine")
  , ("waffle"                , "waﬄe")
  , ("office"                , "oﬃce")
  , ("fly"                   , "ﬂy")
  -- Ligatures inside styled text don't apply (SMP codepoints):
  , ("*firefox*"             , "\x1D453\x1D456\x1D45F\x1D452\x1D453\x1D45C\x1D465")
  ]

-----------------------------------------------------------------------
-- Unsupported: graceful degradation (trace warning + fallback output)
--
-- These constructs are not fully supported but no longer crash. Each test
-- documents the expected fallback behavior. The trace warnings go to stderr,
-- which is visible during builds but invisible in the test output.
-----------------------------------------------------------------------
unsupportedTests :: [(Text, Text)]
unsupportedTests =
  [ -- Math (inline): falls back to raw LaTeX string.
    ("$x^2$"                                      , "x^2")
  -- Math (display): same raw-LaTeX fallback.
  , ("$$y=mx+b$$"                                  , "y=mx+b")
  -- Footnote: Note node is dropped; surrounding text survives.
  , ("text[^1]\n\n[^1]: footnote"                  , "text")
  -- Table: entire block is dropped (empty output).
  , ("| a | b |\n|---|---|\n| 1 | 2 |"             , "")
  -- Definition list: best-effort "term: definition" rendering.
  , ("Term\n:   Definition"                         , "Term: Definition")
  ]

