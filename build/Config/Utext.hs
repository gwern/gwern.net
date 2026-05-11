{-# LANGUAGE OverloadedStrings #-}
-- | Utext: Markdown to Unicode-rich plain text configuration data & unit-tests
-- Author: gwern
-- Date: 2026-04-06
-- When: Time-stamp: "2026-05-11 14:58:01 gwern"
-- License: CC-0

module Config.Utext where

import qualified Data.Text as T (Text)

-- | Truncation limit for trace/error messages showing input excerpts.
traceLimit :: Int
traceLimit = 80

-----------------------------------------------------------------------
-- Style tracking
-----------------------------------------------------------------------

data Style = Style
  { sBold   :: !Bool
  , sItalic :: !Bool
  , sLigature :: !Bool
  } deriving (Eq, Show)

defaultStyle :: Style
defaultStyle = Style False False False

-----------------------------------------------------------------------
-- Tests
-----------------------------------------------------------------------

-- | Run the full test suite. Returns a list of failures as
-- @(input, expected, actual)@ triples. Empty list = all pass.
--
-- Takes two render functions because the ligature feature is an opt-in
-- style flag (off in 'defaultStyle'): the first renderer exercises the
-- default style for the bulk of tests; the second has @sLigature = True@
-- and exercises ligature-substitution cases.
utextTestSuite :: (T.Text -> T.Text)  -- ^ render with 'defaultStyle'
               -> (T.Text -> T.Text)  -- ^ render with @defaultStyle { sLigature = True }@
               -> [(T.Text, T.Text, T.Text)]
utextTestSuite plainRender ligatureRender =
  filter (\(_, expected, actual) -> expected /= actual)
    $  [ (i, e, plainRender    i) | (i, e) <- supportedTests ++ unsupportedTests ]
    ++ [ (i, e, ligatureRender i) | (i, e) <- ligatureSupportedTests ]

-----------------------------------------------------------------------
-- Supported: features that compile to fancy Unicode
-----------------------------------------------------------------------

supportedTests :: [(T.Text, T.Text)]
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
  ]

-- | Cases that exercise ligature substitution. Run with
-- @defaultStyle { sLigature = True }@ to actually fire the substitution.
ligatureSupportedTests :: [(T.Text, T.Text)]
ligatureSupportedTests = concat
  [ ligatureTests
  , ligatureIntegrationTests
  ]

-- Plain text and empty input
identityTests :: [(T.Text, T.Text)]
identityTests =
  [ (""                , "")
  , ("hello"           , "hello")
  , ("hello world"     , "hello world")
  -- ASCII apostrophe becomes smart right single quote:
  , ("it's"            , "it\x2019s")
  -- Pre-existing em dash passes through:
  , ("foo—bar"         , "foo—bar")
  ]

-- *italic* and <em> → mathematical italic letters, with a pragmatic
-- sans-serif-italic override for h (U+1D629).
-- No italic digits in Unicode; digits pass through unchanged.
italicTests :: [(T.Text, T.Text)]
italicTests =
  [ ("*hello*"             , "\x1D629\x1D452\x1D459\x1D459\x1D45C")
  , ("*A*"                 , "\x1D434")
  , ("*z*"                 , "\x1D467")
  , ("*h*"                 , "\x1D629")  -- pragmatic h override; Unicode-correct would be ℎ
  , ("*foo bar*"           , "\x1D453\x1D45C\x1D45C \x1D44F\x1D44E\x1D45F")
  , ("*test123*"           , "\x1D461\x1D452\x1D460\x1D461\&123")
  ]

-- **bold** and <strong> → serif bold (A=U+1D400, a=U+1D41A, 0=U+1D7CE)
boldTests :: [(T.Text, T.Text)]
boldTests =
  [ ("**hi**"                , "\x1D421\x1D422")
  , ("**A**"                 , "\x1D400")
  , ("**z**"                 , "\x1D433")
  , ("**OK**"                , "\x1D40E\x1D40A")
  , ("**9**"                 , "\x1D7D7")
  , ("**0**"                 , "\x1D7CE")
  ]

-- ***bold italic*** → serif bold italic (A=U+1D468, a=U+1D482; digits→bold)
boldItalicTests :: [(T.Text, T.Text)]
boldItalicTests =
  [ ("***ab***"              , "\x1D482\x1D483")
  , ("***Z***"               , "\x1D481")
  , ("***5***"               , "\x1D7D3")
  ]

-- `code` and <code> → mathematical monospace (A=U+1D670, a=U+1D68A, 0=U+1D7F6)
monoTests :: [(T.Text, T.Text)]
monoTests =
  [ ("`hi`"                  , "\x1D691\x1D692")
  , ("`A`"                   , "\x1D670")
  , ("`z`"                   , "\x1D6A3")
  , ("`0`"                   , "\x1D7F6")
  , ("`hello world`"         , "\x1D691\x1D68E\x1D695\x1D695\x1D698 \x1D6A0\x1D698\x1D69B\x1D695\x1D68D")
  ]

-- ^superscript^ and <sup>
superscriptTests :: [(T.Text, T.Text)]
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
subscriptTests :: [(T.Text, T.Text)]
subscriptTests =
  [ ("H~2~O"                 , "H\x2082O")
  , ("<sub>0</sub>"          , "\x2080")
  , ("<sub>n</sub>"          , "\x2099")
  , ("x~i~"                  , "x\x1D62")
  ]

-- ~~strikethrough~~ and <del>/<s>/<strike> → combining short stroke overlay (U+0336)
strikethroughTests :: [(T.Text, T.Text)]
strikethroughTests =
  [ ("~~no~~"                , "n\x0336o\x0336")
  -- NOTE: standalone "<del>x</del>" is not tested because Pandoc's Markdown
  -- reader promotes it to a block-level RawBlock, and re-parsing via readHtml
  -- loses the strikethrough. Works fine embedded in surrounding text (tested
  -- in htmlInMarkdownTests). Use Markdown ~~…~~ for standalone strikethrough.
  , ("<s>ab</s>"             , "a\x0336\&b\x0336")
  ]

-- <u>underline</u> → combining low line (U+0332)
underlineTests :: [(T.Text, T.Text)]
underlineTests =
  [ ("<u>hi</u>"             , "h\x0332i\x0332")
  ]

-- [text]{.smallcaps} and <span class="smallcaps"> → IPA small capitals
-- Uppercase letters pass through unchanged.
smallcapsTests :: [(T.Text, T.Text)]
smallcapsTests =
  [ ("[hello]{.smallcaps}"                 , "\x029C\x1D07\x029F\x029F\x1D0F")
  , ("[AB]{.smallcaps}"                    , "AB")
  , ("<span class=\"smallcaps\">de</span>" , "\x1D05\x1D07")
  ]

-- Pandoc smart typography
smartQuoteTests :: [(T.Text, T.Text)]
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

-- Links: render text followed by URL/title in parentheses.
-- See 'ligatureIntegrationTests' for ligature-on link/title rendering.
linkTests :: [(T.Text, T.Text)]
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
htmlInMarkdownTests :: [(T.Text, T.Text)]
htmlInMarkdownTests =
  [ ("mistakes in <em>Death Note</em> and fixes"
    , "mistakes in \x1D437\x1D452\x1D44E\x1D461\x1D629 \x1D441\x1D45C\x1D461\x1D452 and fixes")
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
  , ("<code class=\"sourceCode haskell\">mono</code> text"
    , "\x1D696\x1D698\x1D697\x1D698 text")
  -- Nested HTML tags in Markdown:
  , ("<em><strong>both</strong></em>"
    , "\x1D483\x1D490\x1D495\x1D489")
  , ("<em>foo <em>bar</em> baz</em>"
    , "\x1D453\x1D45C\x1D45C \x1D44F\x1D44E\x1D45F \x1D44F\x1D44E\x1D467")
  -- Tag variants handled by matchOpenTag:
  , ("a <i>word</i> b"
    , "a \x1D464\x1D45C\x1D45F\x1D451 b")
  , ("a <b>word</b> b"
    , "a \x1D430\x1D428\x1D42B\x1D41D b")
  , ("a <strike>xy</strike> b"
    , "a x\x0336y\x0336 b")
  ]

-- Block-level constructs
blockTests :: [(T.Text, T.Text)]
blockTests =
  [ -- Headers render as bold:
    ("# Title"               , "\x1D413\x1D422\x1D42D\x1D425\x1D41E")
  -- Code blocks render as monospace:
  , ("```\nfoo\n```"         , "\x1D68F\x1D698\x1D698")
  -- Standalone images become Figures with implicit_figures; render them like
  -- images-with-captions either way.
  , ("![Caption](image.png)"
    , "(Caption: <image.png>)")
  , ("![Caption](image.png \"Title\")"
    , "(Caption, \x201CTitle\x201D: <image.png>)")
  -- Bullet list:
  , ("- one\n- two"
    , "• one\n\n• two")
  -- Ordered list:
  , ("1. first\n2. second"
    , "1. first\n\n2. second")
  -- Block quote (needs surrounding context so T.strip doesn't eat the indent):
  , ("before\n\n> quote\n\nafter"
    , "before\n\n  quote\n\nafter")
  -- Line block:
  , ("| line one\n| line two"
    , "line one\nline two")
  -- Horizontal rule (needs context to avoid being swallowed by T.strip):
  , ("before\n\n***\n\nafter"
    , "before\n\n\x2014\x2014\x2014\n\nafter")
  ]

-- Non-ASCII: pass through unchanged (no mathematical-alphabet mapping exists)
nonAsciiTests :: [(T.Text, T.Text)]
nonAsciiTests =
  [ ("\x00E9"                , "\x00E9")
  , ("\x4E16\x754C"          , "\x4E16\x754C")
  -- Non-ASCII inside italic: only ASCII letters get mapped
  , ("*caf\x00E9*"           , "\x1D450\x1D44E\x1D453\x00E9")
  ]

-- Digit handling across styles
digitTests :: [(T.Text, T.Text)]
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
edgeCaseTests :: [(T.Text, T.Text)]
edgeCaseTests =
  [ -- Punctuation passes through inside bold:
    ("**hello, world!**"     , "\x1D421\x1D41E\x1D425\x1D425\x1D428, \x1D430\x1D428\x1D42B\x1D425\x1D41D!")
  -- Nested Markdown: **a *b* c** → bold a, bold-italic b, bold c
  , ("**a *b* c**"           , "\x1D41A \x1D483 \x1D41C")
  -- Character transforms take precedence over nested bold/italic wrappers.
  , ("*x^n^*"                , "\x1D465\x207F")
  , ("**H~2~O**"             , "\x1D407\x2082\x1D40E")
  , ("**[ab]{.smallcaps}**"  , "\x1D00\x0299")
  ]

-- Ligature replacement: fi→ﬁ, fl→ﬂ, ff→ﬀ, ffi→ﬃ, ffl→ﬄ
-- Only affects plain (unstyled) ASCII text and quoted titles; styled
-- characters are in the SMP and do not match the ASCII digraphs.
ligatureTests :: [(T.Text, T.Text)]
ligatureTests =
  [ ("fine"                  , "ﬁne")
  , ("offline"               , "oﬄine")
  , ("waffle"                , "waﬄe")
  , ("office"                , "oﬃce")
  , ("fly"                   , "ﬂy")
  -- Ligatures inside styled text don't apply (SMP codepoints):
  , ("*firefox*"             , "\x1D453\x1D456\x1D45F\x1D452\x1D453\x1D45C\x1D465")
  ]

-- Integration cases that exercise ligature substitution through other
-- constructs (links, embedded HTML, lists, definition lists). The same
-- inputs (or near-equivalents) appear in linkTests / htmlInMarkdownTests /
-- blockTests / unsupportedTests with non-ligated expected output, so each
-- construct is covered under both styles.
ligatureIntegrationTests :: [(T.Text, T.Text)]
ligatureIntegrationTests =
  -- Ligatures apply in link label and quoted title, never in the literal URL:
  [ ("[office](https://example.com/office \"Office filing\")"
    , "oﬃce (\x201COﬃce ﬁling\x201D: <https://example.com/office>)")
  , ("[find](https://fi.example.com \"Official\")"
    , "ﬁnd (\x201COﬃcial\x201D: <https://fi.example.com>)")
  -- Plain text after an HTML-reassembled tag still gets ligated:
  , ("mistakes in <em>Death Note</em> and fixes"
    , "mistakes in \x1D437\x1D452\x1D44E\x1D461\x1D629 \x1D441\x1D45C\x1D461\x1D452 and \xFB01xes")
  -- Inside ordered list items:
  , ("1. first\n2. second"
    , "1. \xFB01rst\n\n2. second")
  -- Inside definition list definitions (best-effort flatten):
  , ("Term\n:   Definition"
    , "Term: De\xFB01nition")
  ]

-----------------------------------------------------------------------
-- Unsupported: graceful degradation (trace warning + fallback output)
--
-- These constructs are not fully supported but no longer crash. Each test
-- documents the expected fallback behavior. The trace warnings go to stderr,
-- which is visible during builds but invisible in the test output.
-----------------------------------------------------------------------
unsupportedTests :: [(T.Text, T.Text)]
unsupportedTests =
  [ -- Math (inline): falls back to raw LaTeX string.
    ("$x^2$"                                    , "x^2")
  -- Math (display): same raw-LaTeX fallback.
  , ("$$y=mx+b$$"                               , "y=mx+b")
  -- Footnote: Note node is dropped; surrounding text survives.
  , ("text[^1]\n\n[^1]: footnote"               , "text")
  -- Table: entire block is dropped (empty output).
  , ("| a | b |\n|---|---|\n| 1 | 2 |"          , "")
  -- Definition list: best-effort "term: definition" rendering.
  , ("Term\n:   Definition"                     , "Term: Definition")
  ]

