{-# LANGUAGE OverloadedStrings #-}
-- | Utext: compile Pandoc AST to Unicode-rich plain text.
--
-- Intended for social media cards, Open Graph descriptions, and other contexts
-- where HTML is stripped but Unicode renders. Converts Pandoc inline formatting
-- to Unicode mathematical alphanumeric symbols (bold, italic, bold-italic,
-- monospace), combining characters (strikethrough, underline), Unicode
-- super/subscripts, and IPA-derived small capitals.
--
-- Pure API (errors on Math nodes):
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
  ) where

import Data.Char (chr, isAsciiLower, isAsciiUpper, isDigit, ord, toLower)
import qualified Data.Map.Strict as M (lookup, findWithDefault, fromList, Map)
import Data.Text (Text)
import qualified Data.Text as T (concat, concatMap, intercalate, lines, map, null, pack, singleton, strip, take, unpack)
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
pandocToUtext (Pandoc _meta blocks) = renderBlocks defaultStyle blocks

-- | Render a list of inlines (eg. from a metadata field like 'title' or
-- 'description') to Utext. This is the main entry point for social media cards.
inlinesToUtext :: [Inline] -> Text
inlinesToUtext = renderInlines defaultStyle

-- | Render a single inline to Utext.
inlineToUtext :: Inline -> Text
inlineToUtext = renderInline defaultStyle

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
      ++ show (T.take 80 tex) ++ ": " ++ stderr
    ExitSuccess -> do
      let html = T.strip (T.pack stdout)
      -- latex2unicode.py returns HTML with <em>, <sup>, <sub>, <span>, etc.
      -- Parse it as HTML, then render the resulting AST to Utext.
      -- If parsing fails, that's a bug in the script output → fatal error.
      case parseHtml html of
        Left err -> error $
          "Utext: failed to parse latex2unicode.py HTML output: " ++ show err
          ++ "\n  Input:  " ++ show (T.take 80 tex)
          ++ "\n  Output: " ++ show (T.take 200 html)
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

-- Supported block constructs. Anything not listed here is a fatal error.
renderBlock :: Style -> Block -> [Text]
renderBlock s (Para inlines)         = [renderInlines s inlines]
renderBlock s (Plain inlines)        = [renderInlines s inlines]
renderBlock s (LineBlock lns)        = [T.intercalate "\n" $ map (renderInlines s) lns]
renderBlock s (Header _ _ inlines)   = [renderInlines (s { sBold = True }) inlines]
renderBlock s (BlockQuote blocks)    = [T.intercalate "\n" $ map ("  " <>) $ T.lines $ renderBlocks s blocks]
renderBlock s (BulletList items)     = map (\item -> "• " <> renderBlocks s item) items
renderBlock s (OrderedList _ items)  = zipWith (\n item -> T.pack (show n) <> ". " <> renderBlocks s item)
                                              [(1::Int)..] items
renderBlock _ (CodeBlock _ code)     = [mapText codeMono code]
renderBlock _ HorizontalRule         = ["———"]
renderBlock s (Div _ blocks)         = [renderBlocks s blocks]
-- Unsupported block constructs: error immediately so we know what needs handling.
-- RawBlock "html": attempt to re-parse and render (handles stray HTML from
-- metadata or latex2unicode.py output).
renderBlock s (RawBlock (Format "html") html) =
  case runPure (readHtml def html) of
    Right (Pandoc _ blocks) -> concatMap (renderBlock s) blocks
    Left _ -> error $ "Utext: unparseable RawBlock html: " ++ show (T.take 80 html)
renderBlock _ (RawBlock fmt t)       = error $ "Utext: unsupported RawBlock (" ++ show fmt ++ "): " ++ show (T.take 80 t)
renderBlock _ (Table {})             = error   "Utext: unsupported Table"
renderBlock _ (Figure _ _ _)         = error   "Utext: unsupported Figure"
renderBlock _ (DefinitionList _)     = error   "Utext: unsupported DefinitionList"

-----------------------------------------------------------------------
-- Inline rendering
-----------------------------------------------------------------------

renderInlines :: Style -> [Inline] -> Text
renderInlines s = T.concat . map (renderInline s)

-- Supported inline constructs. Anything not listed here is a fatal error.
renderInline :: Style -> Inline -> Text
renderInline s (Str t)                = mapText (styleChar s) t
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
renderInline _ (Code _ t)            = mapText codeMono t
renderInline _ Space                 = " "
renderInline _ SoftBreak             = " "
renderInline _ LineBreak             = "\n"
renderInline s (Quoted SingleQuote inlines) = "\x2018" <> renderInlines s inlines <> "\x2019"
renderInline s (Quoted DoubleQuote inlines) = "\x201C" <> renderInlines s inlines <> "\x201D"
renderInline s (Link _ inlines _)    = renderInlines s inlines -- intentional: extract text only
renderInline s (Span attr inlines)
  | hasClass "smallcaps" attr        = renderInline s (SmallCaps inlines)
  | hasClass "subsup" attr           = renderInlines s inlines   -- sub+sup rendered sequentially
  | hasStyle attr                    = error $ "Utext: unsupported Span with inline style: "
                                        ++ show (getKvs attr)
  | otherwise                        = renderInlines s inlines
-- RawInline "html": attempt to re-parse and render. This handles HTML fragments
-- from latex2unicode.py that survive Pandoc's HTML reader as raw nodes (eg.
-- character references like &#775;, or unusual nesting).
renderInline s (RawInline (Format "html") html) =
  case parseHtmlInlines html of
    Right inlines -> renderInlines s inlines
    Left _        -> error $ "Utext: unparseable RawInline html: " ++ show (T.take 80 html)
-- Unsupported inline constructs: error immediately.
renderInline _ (RawInline fmt t)     = error $ "Utext: unsupported RawInline (" ++ show fmt ++ "): " ++ show (T.take 80 t)
renderInline _ (Note _)              = error   "Utext: unsupported Note (footnote)"
renderInline _ (Math fmt t)          = error $ "Utext: unsupported Math (" ++ show fmt ++ "): " ++ show (T.take 80 t)
renderInline _ (Image _ _ (url, _))  = error $ "Utext: unsupported Image: " ++ show (T.take 80 url)
renderInline _ (Cite cs _)          = error $ "Utext: unsupported Cite: " ++ show (length cs) ++ " citation(s)"

hasClass :: Text -> Attr -> Bool
hasClass cls (_, classes, _) = cls `elem` classes

hasStyle :: Attr -> Bool
hasStyle (_, _, kvs) = any (\(k, _) -> k == "style") kvs

getKvs :: Attr -> [(Text, Text)]
getKvs (_, _, kvs) = kvs

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

-- | Sans-serif bold: U+1D5D4 (A), U+1D5EE (a), U+1D7EC (0)
boldChar :: Char -> Char
boldChar c
  | isAsciiUpper c = chr (ord c - ord 'A' + 0x1D5D4)
  | isAsciiLower c = chr (ord c - ord 'a' + 0x1D5EE)
  | isDigit c      = chr (ord c - ord '0' + 0x1D7EC)
  | otherwise      = c

-- | Sans-serif italic: U+1D608 (A), U+1D622 (a). No italic digits in Unicode.
italicChar :: Char -> Char
italicChar c
  | isAsciiUpper c = chr (ord c - ord 'A' + 0x1D608)
  | isAsciiLower c = chr (ord c - ord 'a' + 0x1D622)
  | otherwise      = c

-- | Sans-serif bold italic: U+1D63C (A), U+1D656 (a). Digits fall back to bold.
boldItalicChar :: Char -> Char
boldItalicChar c
  | isAsciiUpper c = chr (ord c - ord 'A' + 0x1D63C)
  | isAsciiLower c = chr (ord c - ord 'a' + 0x1D656)
  | isDigit c      = chr (ord c - ord '0' + 0x1D7EC) -- no bold-italic digits; use bold
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

mapText :: (Char -> Char) -> Text -> Text
mapText = T.map
