{-# LANGUAGE OverloadedStrings #-}
-- | Utext: compile Pandoc AST to Unicode-rich plain text.
-- Author: gwern
-- Date: 2026-04-06
-- When: Time-stamp: "2026-04-19 17:41:10 gwern"
-- License: CC-0
--
-- Intended for social media cards, Open Graph descriptions, and other contexts
-- where HTML is stripped but Unicode renders. Converts Pandoc inline formatting
-- to Unicode mathematical alphanumeric symbols (bold, italic, bold-italic,
-- monospace), combining characters (strikethrough, underline), Unicode
-- super/subscripts, and IPA-derived small capitals.
-- Background: <https://gwern.net/utext>
--
-- Output grammar:
--
-- * Links render as @label (<url>)@ or @label (“title”: <url>)@.
-- * Images render as @(label: <url>)@ or @(label, “title”: <url>)@.
-- * Simple image figures render like images; when both caption and alt text are
--   present, the rendered caption wins.
-- * Ligatures apply to visible text and quoted titles, never to literal URLs.
--
-- Examples:
--
-- * @[office](https://example.com/office)@ renders as
--   @oﬃce (<https://example.com/office>)@.
-- * A figure whose caption is @Caption@, alt text is @Alt@, URL is @image.png@,
--   and title is @Title@ renders as @(Caption, “Title”: <image.png>)@.
--
-- Known lossy behavior:
--
-- * Pure mode leaves Math as raw LaTeX with a trace warning; IO mode shells out
--   to @latex2unicode.py@ first.
-- * Footnotes are dropped.
-- * Tables are dropped (TODO: flatten simple tables to plain text).
-- * Definition lists are flattened to @term: definition@ lines.
-- * Ordered-list start numbers / numbering styles are ignored.
-- * Arbitrary CSS in @Span@/@style=@ is ignored.
-- * Standalone block-level raw HTML wrappers like @<del>...</del>@ are still
--   only partially handled (TODO: block-level analogue of
--   'reassembleHtmlInlines').
-- * Complex figures are rendered best-effort only (with a trace warning).
--
-- Pragmatic note: Unicode's official mathematical italic small @h@ is @ℎ@
-- (U+210E), but some browser/font combinations render it badly beside the
-- other mathematical italic letters. Utext uses sans-serif italic @𝘩@ instead
-- for more consistent prose output.
--
-- Pure API (Math falls back to raw LaTeX with a trace warning):
--   pandocToUtext :: Pandoc -> T.Text
--   inlinesToUtext :: [Inline] -> T.Text
--   inlineToUtext :: Inline -> T.Text
--   rawText2Utext :: T.Text -> T.Text
--   rawHtml2Utext :: T.Text -> T.Text
--   rawMarkdown2Utext :: T.Text -> T.Text
--
-- IO API (Math via latex2unicode.py):
--   pandocToUtextIO :: Pandoc -> IO T.Text
--   rawText2UtextIO :: T.Text -> IO T.Text
--   rawHtml2UtextIO :: T.Text -> IO T.Text
--   rawMarkdown2UtextIO :: T.Text -> IO T.Text
--
-- Non-ASCII letters (accented, CJK, etc.) pass through unchanged when no
-- mathematical-alphabet mapping exists. Unmapped super/subscript characters
-- generally pass through unchanged; unmapped ASCII uppercase letters fall
-- back to the lowercase super/sub form when available, otherwise to ^X / _X
-- notation.
{- Claude-4.6-opus suggestions on extensions:

#. **Footnote markers instead of dropping**: currently `Note` is silently dropped. Could render the footnote reference as a superscript circled number (➀➁➂ or ❶❷❸, as the page specifically suggests for footnotes versus hyperlink numbering).

    The footnote body can be appended at the end or dropped for OG-description contexts, but the marker appearing inline preserves reading flow. Requires a counter threaded through rendering (or switch to `State`).
#. **Fancier ordered-list markers**: the page mentions PARENTHESIZED DIGIT ONE ⑴⑵⑶⑷ and circled digits ①②③…⑳. Currently just `"1. "`. Unicode provides circled digits 1--20 (U+2460--U+2473) and more; fall back to `"N. "` beyond 20.
#. **Fancier horizontal rules**: `"———"` works but the page showcases box-drawing characters. `"─────────"` (BOX DRAWINGS LIGHT HORIZONTAL, U+2500) or ornamental dingbats like `"✦ ✦ ✦"` would be more in the Utext spirit.

    Trivial one-line change.
#. **Double underline**: the page explicitly demonstrates d̳o̳u̳b̳l̳e̳ underline using COMBINING DOUBLE LOW LINE (U+0333). Currently no Pandoc construct maps to this, but a `{.double-underline}` span class could trigger it.

    Very cheap to add the character mapping; the question is what triggers it.
#. **Header hierarchy**: all header levels render as bold. The page describes a progression: italic → underlined → bold → bold+underlined. Could differentiate: H1 = bold+underline, H2 = bold, H3 = italic+bold, H4+ = italic.

    Small change to `renderBlock (Header level _ inlines)`.
#. **Table rendering**: the page discusses box-drawing tables at length.

    A minimal version: render each cell as plain text, separate columns with ` │ `, rows with newlines, and optionally a `─┼─` separator after the header row. Pandoc provides full table structure (headers, alignment, rows). Even a pipe-separated fallback (`a | b`) would beat dropping.
#. **Fraktur / Script / Double-struck alphabet variants**: the page lists these as stylistic variations. Unicode provides Mathematical Fraktur (U+1D504), Script/Calligraphic (U+1D49C), Double-Struck (U+1D538).

    Could be triggered by `{.fraktur}`, `{.script}`, `{.double-struck}` span classes, analogous to `{.smallcaps}`. Each needs a character mapping table like `boldChar`/`italicChar`.
#. **Monochrome code syntax highlighting**: the page notes bold/italic/underline can reproduce ALGOL 60-style monochrome syntax highlighting.

    Pandoc's `CodeBlock` carries language info and the `skylighting` library can tokenize. Map token types: keywords → bold, comments → italic, strings → underline, etc. All output stays in monospace (mathematical monospace bold, italic variants exist but are sparse).
#. **ZWSP soft line-wrapping**: insert U+200B at word boundaries for viewers that don't word-wrap.
#. **Soft hyphenation**: insert U+00AD SOFT HYPHEN using Knuth-Liang dictionaries. Same scope caveat as ZWSP.
#. **Unicode Tags for comments**: the page describes using the Tags block (U+E0001--U+E007F) to embed invisible ASCII comments.
#. **ASCII art headers / dropcaps**: the page envisions large headers rendered as text-art using FIGlet-style fonts, and dropcaps as boxed upscaled letters.

    Deep rabbit hole; depends on an ASCII art font library.
#. **LLM-based text rewriting for justification**: the page's most speculative feature: using LLMs to rewrite text with synonyms for better monospace justification.

    Research project, not a compiler TODO.
-}
module Utext
  ( -- * Pure API (Math falls back to raw LaTeX)
    pandocToUtext
  , inlinesToUtext
  , inlineToUtext
  , rawText2Utext
  , rawHtml2Utext
  , rawMarkdown2Utext
    -- * IO API (Math via latex2unicode.py)
  , pandocToUtextIO
  , rawText2UtextIO
  , rawHtml2UtextIO
  , rawMarkdown2UtextIO
  ) where

import Data.Char (chr, isAlphaNum, isAsciiLower, isAsciiUpper, isDigit, ord, toLower)
import Data.List (foldl') -- NOTE: re-exported from Prelude since base 4.16 / GHC 9.2; drop this import if it causes a redundant-import warning
import Debug.Trace (trace)
import qualified Data.Map.Strict as M (Map, findWithDefault, fromList, lookup)
import Data.Text (Text)
import qualified Data.Text as T
  ( concat
  , concatMap
  , drop
  , dropEnd
  , intercalate
  , isInfixOf
  , isPrefixOf
  , isSuffixOf
  , lines
  , map
  , null
  , pack
  , replace
  , singleton
  , span
  , strip
  , stripEnd
  , stripStart
  , take
  , toLower
  , unpack
  )
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Definition
import Text.Pandoc.Extensions (pandocExtensions)
import Text.Pandoc.Options (ReaderOptions, def, readerExtensions)
import Text.Pandoc.Readers (readHtml, readMarkdown)
import Text.Pandoc.Walk (walkM)

import qualified Config.Utext as C (defaultStyle, Style(..), traceLimit)

-----------------------------------------------------------------------
-- Public API
-----------------------------------------------------------------------

renderPandoc :: Pandoc -> Text
renderPandoc (Pandoc _meta blocks) = renderBlocks C.defaultStyle blocks

-- | Render a Pandoc document to Utext.
pandocToUtext :: Pandoc -> Text
pandocToUtext = renderPandoc

-- | Render a list of inlines (eg. from a metadata field like 'title' or
-- 'description') to Utext. This is the main entry point for social media cards.
inlinesToUtext :: [Inline] -> Text
inlinesToUtext = renderInlines C.defaultStyle

-- | Render a single inline to Utext.
inlineToUtext :: Inline -> Text
inlineToUtext = renderInline C.defaultStyle

renderParsed :: (Pandoc -> Text) -> Text -> Either Text Pandoc -> Text
renderParsed render fallback = either (const fallback) (T.strip . render)

renderParsedIO :: (Pandoc -> IO Text) -> Text -> Either Text Pandoc -> IO Text
renderParsedIO render fallback = either (const (return fallback)) (fmap T.strip . render)

parseTextFallback :: Text -> Either Text Pandoc
parseTextFallback t = case parseMarkdown t of
  Right doc -> Right doc
  Left _    -> parseHtml t

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
-- -- => "Statistical 𝐿𝑖𝑡𝑒𝑟𝑎𝑐𝑦 is 𝐇𝐚𝐫𝐝"
-- @
rawText2Utext :: Text -> Text
rawText2Utext t
  | T.null t  = t
  | otherwise = renderParsed pandocToUtext t (parseTextFallback t)

-- | Parse as HTML only (no Markdown processing). Useful when input is known to
-- be HTML fragments, eg. from annotation databases or API responses.
rawHtml2Utext :: Text -> Text
rawHtml2Utext t = renderParsed pandocToUtext t (parseHtml t)

-- | Parse as Pandoc Markdown only. Useful when input is known to be Markdown.
rawMarkdown2Utext :: Text -> Text
rawMarkdown2Utext t = renderParsed pandocToUtext t (parseMarkdown t)

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

-- | Parse an HTML fragment and return its top-level blocks.
-- Returns Right [] for empty/whitespace input.
parseHtmlBlocks :: Text -> Either Text [Block]
parseHtmlBlocks html
  | T.null (T.strip html) = Right []
  | otherwise = case parseHtml html of
      Left err                -> Left err
      Right (Pandoc _ blocks) -> Right blocks

-- Pandoc Markdown with the full standard extension set (handles inline HTML,
-- strikethrough, superscript, subscript, smallcaps via bracketed spans, etc.)
markdownOpts :: ReaderOptions
markdownOpts = def { readerExtensions = pandocExtensions }

-----------------------------------------------------------------------
-- IO API: latex2unicode.py integration
-----------------------------------------------------------------------

-- | Like 'pandocToUtext', but preprocesses Math nodes by shelling out to
-- @latex2unicode.py@ to convert LaTeX to HTML. The HTML output is then parsed
-- and rendered to Utext.
--
-- Expects @static/build/latex2unicode.py@ to be callable from the current
-- working directory (ie. the site repo root), matching the existing
-- @Utils.hs@ convention.
pandocToUtextIO :: Pandoc -> IO Text
pandocToUtextIO doc = pandocToUtext <$> walkM (inlineMathToUtext latexToUtextViaScript) doc

-- | Like 'rawText2Utext', but handles Math via @latex2unicode.py@.
rawText2UtextIO :: Text -> IO Text
rawText2UtextIO t
  | T.null t  = return t
  | otherwise = renderParsedIO pandocToUtextIO t (parseTextFallback t)

-- | Like 'rawHtml2Utext', but handles Math via @latex2unicode.py@.
rawHtml2UtextIO :: Text -> IO Text
rawHtml2UtextIO t = renderParsedIO pandocToUtextIO t (parseHtml t)

-- | Like 'rawMarkdown2Utext', but handles Math via @latex2unicode.py@.
rawMarkdown2UtextIO :: Text -> IO Text
rawMarkdown2UtextIO t = renderParsedIO pandocToUtextIO t (parseMarkdown t)

-----------------------------------------------------------------------
-- Math preprocessing (IO)
-----------------------------------------------------------------------

-- | Replace a Math inline with its Utext rendering. The conversion function
-- is passed in to allow swapping implementations (eg. for testing).
inlineMathToUtext :: (Text -> IO Text) -> Inline -> IO Inline
inlineMathToUtext convert (Math _ tex) = Str <$> convert tex
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
      ++ show (T.take C.traceLimit tex) ++ ": " ++ stderr
    ExitSuccess -> do
      let html = T.strip (T.pack stdout)
      -- latex2unicode.py returns HTML with <em>, <sup>, <sub>, <span>, etc.
      -- Parse it as HTML, then render the resulting AST to Utext.
      -- If parsing fails, that's a bug in the script output → fatal error.
      case parseHtmlBlocks html of
        Left err -> error $
          "Utext: failed to parse latex2unicode.py HTML output: " ++ show err
          ++ "\n  Input:  " ++ show (T.take C.traceLimit tex)
          ++ "\n  Output: " ++ show (T.take (C.traceLimit * 2) html)
        Right blocks -> return $ T.strip (renderBlocks C.defaultStyle blocks)

-----------------------------------------------------------------------
-- Block rendering
-----------------------------------------------------------------------

renderBlocks :: C.Style -> [Block] -> Text
renderBlocks s = T.intercalate "\n\n" . concatMap (renderBlock s)

-- Supported block constructs. Unsupported ones degrade gracefully with trace warnings.
renderBlock :: C.Style -> Block -> [Text]
renderBlock s (Para inlines)         = [renderInlines s inlines]
renderBlock s (Plain inlines)        = [renderInlines s inlines]
renderBlock s (LineBlock lns)        = [T.intercalate "\n" $ map (renderInlines s) lns]
renderBlock s (Header _ _ inlines)   = [renderInlines (s { C.sBold = True }) inlines]
renderBlock s (BlockQuote blocks)    = [T.intercalate "\n" $ map ("  " <>) $ T.lines $ renderBlocks s blocks]
renderBlock s (BulletList items)     = map (\item -> "• " <> renderBlocks s item) items
-- NOTE: ignores Pandoc's start number / style / delimiter metadata.
renderBlock s (OrderedList _ items)  = zipWith (\n item -> T.pack (show n) <> ". " <> renderBlocks s item)
                                              [(1 :: Int) ..] items
renderBlock _ (CodeBlock _ code)     = [T.map codeMono code]
renderBlock _ HorizontalRule         = ["———"]
renderBlock s (Div _ blocks)         = [renderBlocks s blocks]
-- Unsupported block constructs: warn via trace and degrade gracefully.
-- RawBlock "html": attempt to re-parse and render (handles stray HTML from
-- metadata or latex2unicode.py output).
--
-- TODO: standalone wrapper tags like "<del>x</del>" still tend to arrive as
-- RawBlock wrappers; fixing that cleanly needs a block-level analogue of
-- 'reassembleHtmlInlines'.
renderBlock s (RawBlock (Format "html") html) =
  case parseHtmlBlocks html of
    Right blocks -> concatMap (renderBlock s) blocks
    Left _       -> traceWarnT "unparseable RawBlock html" html []
renderBlock _ (RawBlock fmt t)       = traceWarnT ("unsupported RawBlock (" ++ show fmt ++ ")") t []
renderBlock _ (Table {})             = traceWarn   "unsupported Table (dropped)" []
renderBlock s (Figure _ caption blocks) =
  case T.strip (renderFigure s caption blocks) of
    "" -> []
    t  -> [t]
renderBlock s (DefinitionList defs)  = traceWarn   "lossy DefinitionList fallback" $
  concatMap (\(term, defBlocks) -> [renderInlines s term <> ": " <>
      T.intercalate "; " (map (renderBlocks s) defBlocks)]) defs

renderFigure :: C.Style -> Caption -> [Block] -> Text
renderFigure s caption blocks =
  case extractFigureImage blocks of
    Just (alt, url, title) ->
      let label = chooseText (T.strip (renderCaption s caption))
                             (T.strip (renderInlines s alt))
      in renderParenthesizedTarget label url title
    Nothing ->
      traceWarn "complex Figure (best-effort fallback)" $
      joinFigureParts (T.strip (renderBlocks s blocks))
                      (T.strip (renderCaption s caption))

renderCaption :: C.Style -> Caption -> Text
renderCaption s (Caption mShort blocks) =
  let longCaption = T.strip (renderBlocks s blocks)
  in if T.null longCaption
       then maybe "" (T.strip . renderInlines s) mShort
       else longCaption

extractFigureImage :: [Block] -> Maybe ([Inline], Text, Text)
extractFigureImage [Para  [Image _ alt (url, title)]] = Just (alt, url, title)
extractFigureImage [Plain [Image _ alt (url, title)]] = Just (alt, url, title)
extractFigureImage _                                   = Nothing

joinFigureParts :: Text -> Text -> Text
joinFigureParts body cap
  | T.null body = cap
  | T.null cap  = body
  | otherwise   = body <> "—" <> cap

chooseText :: Text -> Text -> Text
chooseText primary fallback
  | T.null primary = fallback
  | otherwise      = primary

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
-- RawInline handler will deal with them). Matching is nesting-aware for
-- repeated tags like @\<em\>foo \<em\>bar\<\/em\> baz\<\/em\>@, and
-- recognized tags may carry arbitrary attributes (eg. @\<code class=\"x\"\>@).
data HtmlWrapper = HtmlWrapper !Text ([Inline] -> Inline)

data HtmlTag = HtmlTag !Bool !Text !Text

reassembleHtmlInlines :: [Inline] -> [Inline]
reassembleHtmlInlines [] = []
reassembleHtmlInlines (RawInline (Format "html") tag : rest)
  | Just (HtmlWrapper tagName constructor) <- matchOpenTag tag
  , Just (inner, after) <- splitAtClose tagName rest
  = constructor (reassembleHtmlInlines inner) : reassembleHtmlInlines after
reassembleHtmlInlines (x:xs) = x : reassembleHtmlInlines xs

-- | Parse a simple HTML tag. We only care about tag name, whether it closes,
-- and the raw attribute text.
parseHtmlTag :: Text -> Maybe HtmlTag
parseHtmlTag raw
  | not ("<" `T.isPrefixOf` raw && ">" `T.isSuffixOf` raw) = Nothing
  | otherwise =
      let inner0 = T.strip (T.drop 1 (T.dropEnd 1 raw))
          (isClosing, inner1)
            | "/" `T.isPrefixOf` inner0 = (True,  T.stripStart (T.drop 1 inner0))
            | otherwise                 = (False, inner0)
          inner2 = T.stripEnd inner1
          inner3
            | not isClosing && "/" `T.isSuffixOf` inner2 = T.stripEnd (T.dropEnd 1 inner2)
            | otherwise                                  = inner2
          (name, attrs) = T.span isHtmlTagNameChar inner3
      in if T.null name
           then Nothing
           else Just (HtmlTag isClosing (T.toLower name) attrs)

isHtmlTagNameChar :: Char -> Bool
isHtmlTagNameChar c = isAlphaNum c || c == '-' || c == ':'

-- | Match an opening HTML tag to its Pandoc constructor.
matchOpenTag :: Text -> Maybe HtmlWrapper
matchOpenTag t = case parseHtmlTag t of
  Just (HtmlTag False name attrs)
    | name == "em"                        -> Just (HtmlWrapper name Emph)
    | name == "i"                         -> Just (HtmlWrapper name Emph)
    | name == "strong"                    -> Just (HtmlWrapper name Strong)
    | name == "b"                         -> Just (HtmlWrapper name Strong)
    | name == "sub"                       -> Just (HtmlWrapper name Subscript)
    | name == "sup"                       -> Just (HtmlWrapper name Superscript)
    | name == "del"                       -> Just (HtmlWrapper name Strikeout)
    | name == "s"                         -> Just (HtmlWrapper name Strikeout)
    | name == "strike"                    -> Just (HtmlWrapper name Strikeout)
    | name == "u"                         -> Just (HtmlWrapper name Underline)
    | name == "code"                      -> Just (HtmlWrapper name codeWrap)
    | name == "span"
    , "smallcaps" `T.isInfixOf` T.toLower attrs
                                           -> Just (HtmlWrapper name SmallCaps)
  _                                        -> Nothing
  where
    codeWrap inlines = Code nullAttr (renderPlainInlines inlines)

-- | Split an inline list at the matching closing RawInline HTML tag.
-- Returns Nothing if the closing tag is not found.
splitAtClose :: Text -> [Inline] -> Maybe ([Inline], [Inline])
splitAtClose closeName = go 0 []
  where
    go :: Int -> [Inline] -> [Inline] -> Maybe ([Inline], [Inline])
    go _ _ [] = Nothing
    go depth acc (raw@(RawInline (Format "html") t) : rest)
      | Just (HtmlTag isClosing name _) <- parseHtmlTag t
      , name == closeName
      = case (isClosing, depth) of
          (False, _) -> go (depth + 1) (raw : acc) rest
          (True, 0)  -> Just (reverse acc, rest)
          (True, _)  -> go (depth - 1) (raw : acc) rest
    go depth acc (x:xs) = go depth (x:acc) xs

-----------------------------------------------------------------------
-- Inline rendering
-----------------------------------------------------------------------

renderInlines :: C.Style -> [Inline] -> Text
renderInlines s = T.concat . map (renderInline s) . reassembleHtmlInlines

-- | Render inline content as plain text, deliberately stripping style wrappers
-- like Emph, Strong, SmallCaps, Superscript, and Subscript before any later
-- character-by-character transform.
--
-- This is a policy choice: when Unicode lacks a composed form like “bold small
-- caps” or “italic superscript”, the character-level transform wins and nested
-- style wrappers are discarded. So @*x^n^*@ becomes italic @x@ followed by a
-- plain superscript @n@, rather than attempting a nonexistent italic
-- superscript @n@.
renderPlainInlines :: [Inline] -> Text
renderPlainInlines = T.concat . map renderInlinePlain . reassembleHtmlInlines

renderInlinePlain :: Inline -> Text
renderInlinePlain (Str t)                      = t
renderInlinePlain (Emph inlines)               = renderPlainInlines inlines
renderInlinePlain (Strong inlines)             = renderPlainInlines inlines
renderInlinePlain (Underline inlines)          = renderPlainInlines inlines
renderInlinePlain (Strikeout inlines)          = renderPlainInlines inlines
renderInlinePlain (Superscript inlines)        = renderPlainInlines inlines
renderInlinePlain (Subscript inlines)          = renderPlainInlines inlines
renderInlinePlain (SmallCaps inlines)          = renderPlainInlines inlines
renderInlinePlain (Code _ t)                   = t
renderInlinePlain Space                        = " "
renderInlinePlain SoftBreak                    = " "
renderInlinePlain LineBreak                    = "\n"
renderInlinePlain (Quoted SingleQuote inlines) = "\x2018" <> renderPlainInlines inlines <> "\x2019"
renderInlinePlain (Quoted DoubleQuote inlines) = "\x201C" <> renderPlainInlines inlines <> "\x201D"
renderInlinePlain (Link _ inlines _)           = renderPlainInlines inlines
renderInlinePlain (Span _ inlines)             = renderPlainInlines inlines
renderInlinePlain (RawInline (Format "html") html) =
  case parseHtmlInlines html of
    Right inlines -> renderPlainInlines inlines
    Left _        -> ""
renderInlinePlain (RawInline _ _)              = ""
renderInlinePlain (Note _)                     = ""
renderInlinePlain (Math _ t)                   = t
renderInlinePlain (Image _ inlines _)          = renderPlainInlines inlines
renderInlinePlain (Cite _ inlines)             = renderPlainInlines inlines

-- Supported inline constructs. Unsupported ones degrade gracefully with trace warnings.
renderInline :: C.Style -> Inline -> Text
renderInline s (Str t) = renderStyledText s t
renderInline s (Emph inlines)         = renderInlines (s { C.sItalic = True }) inlines
renderInline s (Strong inlines)       = renderInlines (s { C.sBold = True }) inlines
renderInline s (Underline inlines)    = applyCombining '\x0332' (renderInlines s inlines)
renderInline s (Strikeout inlines)    = applyCombining '\x0336' (renderInlines s inlines)
renderInline _ (Superscript inlines)  = T.concatMap charToSuperscript
                                          (renderPlainInlines inlines)
renderInline _ (Subscript inlines)    = T.concatMap charToSubscript
                                          (renderPlainInlines inlines)
renderInline _ (SmallCaps inlines)    = T.concatMap charToSmallCap
                                          (renderPlainInlines inlines)
renderInline _ (Code _ t)             = T.map codeMono t
renderInline _ Space                  = " "
renderInline _ SoftBreak              = " "
renderInline _ LineBreak              = "\n"
renderInline s (Quoted SingleQuote inlines) = "\x2018" <> renderInlines s inlines <> "\x2019"
renderInline s (Quoted DoubleQuote inlines) = "\x201C" <> renderInlines s inlines <> "\x201D"
renderInline s (Link _ inlines (url, title)) =
  renderLinkedTarget (renderInlines s inlines) url title
renderInline s (Span attr inlines)
  | hasClass "smallcaps" attr        = renderInline s (SmallCaps inlines)
  | hasClass "subsup" attr           = renderInlines s inlines
  | hasStyle attr                    = renderInlines s inlines
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
renderInline s (Image _ inlines (url, title)) =
  renderParenthesizedTarget (renderInlines s inlines) url title
renderInline s (Cite _ inlines)      = traceWarn   "lossy Cite fallback (visible text only)" (renderInlines s inlines)

renderStyledText :: C.Style -> Text -> Text
renderStyledText s t =
  let styled = T.map (styleChar s) t
  in if C.sLigature s
       then applyLigatures styled
       else styled

-- | Apply a single combining mark after every codepoint in the input.
applyCombining :: Char -> Text -> Text
applyCombining mark = T.concatMap (\c -> T.singleton c <> T.singleton mark)

renderLinkedTarget :: Text -> Text -> Text -> Text
renderLinkedTarget label url title
  | T.null url   = label
  | T.null title = label <> " (<" <> url <> ">)"
  | otherwise    = label <> " (" <> renderTitle title <> ": <" <> url <> ">)"

renderParenthesizedTarget :: Text -> Text -> Text -> Text
renderParenthesizedTarget label url title
  | T.null label && T.null url = ""
  | T.null label               = "(<" <> url <> ">)"
  | T.null url                 = "(" <> label <> ")"
  | T.null title               = "(" <> label <> ": <" <> url <> ">)"
  | otherwise                  = "(" <> label <> ", " <> renderTitle title <> ": <" <> url <> ">)"

-- | Render a quoted link/image title. Titles are visible text, so ligatures
-- apply here even though literal URLs stay untouched.
renderTitle :: Text -> Text
renderTitle title = "\x201C" <> applyLigatures title <> "\x201D"

hasClass :: Text -> Attr -> Bool
hasClass cls (_, classes, _) = cls `elem` classes

hasStyle :: Attr -> Bool
hasStyle (_, _, kvs) = any (\(k, _) -> k == "style") kvs

-- | Parse an HTML fragment and extract the body inlines. Used for re-parsing
-- RawInline "html" nodes.
parseHtmlInlines :: Text -> Either Text [Inline]
parseHtmlInlines = fmap blocksToInlines . parseHtmlBlocks

-- | Flatten blocks to inlines, preserving inline content. This is a simplified
-- version of Pandoc's own blocksToInlines, suitable for small HTML fragments.
blocksToInlines :: [Block] -> [Inline]
blocksToInlines = concatMap blockToInlines
  where
    blockToInlines (Para ils)  = ils
    blockToInlines (Plain ils) = ils
    blockToInlines (Div _ bs)  = concatMap blockToInlines bs
    blockToInlines _           = []

-----------------------------------------------------------------------
-- Character mapping: style (bold / italic / bold-italic)
-----------------------------------------------------------------------

styleChar :: C.Style -> Char -> Char
styleChar (C.Style True  True _)  = boldItalicChar
styleChar (C.Style True  False _) = boldChar
styleChar (C.Style False True _)  = italicChar
styleChar (C.Style False False _) = id

-- | Serif bold: U+1D400 (A), U+1D41A (a), U+1D7CE (0)
boldChar :: Char -> Char
boldChar c
  | isAsciiUpper c = chr (ord c - ord 'A' + 0x1D400)
  | isAsciiLower c = chr (ord c - ord 'a' + 0x1D41A)
  | isDigit c      = chr (ord c - ord '0' + 0x1D7CE)
  | otherwise      = c

-- | Serif italic: U+1D434 (A), U+1D44E (a). No italic digits in Unicode.
-- Unicode canonically reuses ℎ (U+210E) for mathematical italic h because the
-- nominal U+1D455 slot is reserved. Utext deliberately uses 𝘩 (U+1D629), a
-- sans-serif italic h, for better visual consistency in prose-like output.
italicChar :: Char -> Char
italicChar c
  | isAsciiUpper c = chr (ord c - ord 'A' + 0x1D434)
  | c == 'h'       = '\x1D629'
  | isAsciiLower c = chr (ord c - ord 'a' + 0x1D44E)
  | otherwise      = c

-- | Serif bold italic: U+1D468 (A), U+1D482 (a). Digits fall back to bold.
boldItalicChar :: Char -> Char
boldItalicChar c
  | isAsciiUpper c = chr (ord c - ord 'A' + 0x1D468)
  | isAsciiLower c = chr (ord c - ord 'a' + 0x1D482)
  | isDigit c      = chr (ord c - ord '0' + 0x1D7CE)
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
charToSuperscript = charToScript superscriptMap '^'

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
charToSubscript = charToScript subscriptMap '_'

-- | Shared fallback logic for super/subscript rendering.
-- If a character has no dedicated script form, ASCII uppercase tries the
-- lowercase form first; failing that, it falls back to ^X / _X. Other
-- characters pass through unchanged.
charToScript :: M.Map Char Char -> Char -> Char -> Text
charToScript table fallbackPrefix c = case M.lookup c table of
  Just c' -> T.singleton c'
  Nothing
    | isAsciiUpper c -> case M.lookup (toLower c) table of
        Just c' -> T.singleton c'
        Nothing -> T.pack [fallbackPrefix, c]
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
  | otherwise      = T.singleton c

-----------------------------------------------------------------------
-- Utilities
-----------------------------------------------------------------------

-- | Emit a trace warning prefixed with "Utext: " and return a fallback value.
-- Used for unsupported or unparseable constructs that degrade gracefully.
traceWarn :: String -> a -> a
traceWarn msg = trace ("Utext: " ++ msg)

-- | 'traceWarn' with a truncated 'Text' excerpt appended.
traceWarnT :: String -> Text -> a -> a
traceWarnT msg t = traceWarn (msg ++ ": " ++ show (T.take C.traceLimit t))

-- | Apply Unicode ligature substitutions to plain-text segments only.
-- The replacement table is ordered longest-first so "ffl"/"ffi" win before
-- the shorter "ff"/"fl"/"fi" digraphs.
applyLigatures :: Text -> Text
applyLigatures t = foldl' (\acc (ascii, ligature) -> T.replace ascii ligature acc)
                          t ligatureReplacements

ligatureReplacements :: [(Text, Text)]
ligatureReplacements =
  [ ("ffl", "ﬄ")
  , ("ffi", "ﬃ")
  , ("ff",  "ﬀ")
  , ("fl",  "ﬂ")
  , ("fi",  "ﬁ")
  ]

