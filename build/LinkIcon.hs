{-# LANGUAGE OverloadedStrings #-}

module LinkIcon where

import Data.Text as T
import Text.Pandoc
import Network.URI
import System.FilePath (takeExtension)

-- Statically, at compile-time, define the link-icons for links. Doing this at runtime with CSS is entirely possible and originally done by links.css, but the logic becomes increasingly convoluted & bug-prone because of CSS properties like cascading & longest-matches, and exceptions like 'organization icon overrides PDF icon' become fertile sources of errors & regressions.
-- Doing this at runtime in Haskell is easier and also reduces performance burden on the client browser.

-- Based on <links.js>.
-- The idea is to annotate every `<a>` with two new `data-` attributes, `data-link-icon` and `data-link-icon-type` which jointly specify the type & content of the icon. The link-icon for 'svg' type is overloaded to be a filename in `/static/img/icons/$LINKICON.svg`.
-- λ linkIcon $ Link nullAttr [Str "foo"] ("/docs/foo.pdf", "Foo & Bar 2022")
-- → TODO
-- → TODO
linkIcon :: Inline -> Inline
linkIcon x@(Link (_,cl,_) _ (u, _))
 | "no-icon" `elem` cl = x
 | hasIcon x           = x
 | "deepmind" `T.isInfixOf` u  = addIcon "deepmind" "svg" x
 | "facebook" `T.isInfixOf` u  = addIcon "facebook" "svg" x
 | "google" `T.isInfixOf` u    = addIcon "google" "svg" x
 | "nvidia" `T.isInfixOf` u    = addIcon "nvidia" "text" x
 | "openai" `T.isInfixOf` u    = addIcon "openai" "svg" x
 | "microsoft" `T.isInfixOf` u = addIcon "microsoft" "text,sans,italic" x
 | "microsoft" `T.isInfixOf` u = addIcon "microsoft" "text,sans,italic" x
 | host u == "psyarxiv.com" = addIcon "ψ" "text" x -- GREEK SMALL LETTER PSI
 | host u == "unsongbook.com" = addIcon "ℵ" "text" x -- SSC’s book: (ℵ) ALEF SYMBOL (We use the math symbol instead of the Hebrew deliberately, to avoid triggering bizarre Hebrew bidirectional text-related layout bugs on Mac Firefox.)
 | host u == "andrewgelman.com" || host u == "statmodeling.stat.columbia.edu" = addIcon "▅▇▃" "text" x -- Favicon is a little normal distribution/histogram (▅▇▃) LOWER FIVE EIGHTHS BLOCK, LOWER SEVEN EIGHTHS BLOCK, LOWER THREE EIGHTHS BLOCK
 | host u == "meltingasphalt.com" = addIcon "▲" "text" x -- Kevin Simler’s Melting Asphalt blog uses 3 triangles but that's too many, so we just use one. (▲) BLACK UP-POINTING TRIANGLE
 | anyInfix (extension u) ["opml", "txt", "xml", "json", "jsonl", "page"] = addIcon "txt" "svg" x
 | anyInfix (extension u) ["css", "hs", "js", "conf", "sh", "r", "patch", "diff"] = addIcon "code" "svg" x
 | anyInfix (extension u) ["doc", "docx"] = addIcon "worddoc" "svg" x
 | anyInfix (extension u) ["xls", "xlsx", "ods"] = addIcon "spreadsheet" "svg" x
 | anyInfix (extension u) ["csv"] = addIcon "csv" "svg" x
 | anyInfix (extension u) ["gif", "bmp", "ico", "jpg", "jpeg", "png", "svg", "xcf"] = addIcon "image" "svg" x
 | anyInfix (extension u) ["mp3", "wav", "flac", "ogg", "rm"] = addIcon "audio" "svg" x
 | anyInfix (extension u) ["swf", "mp4", "mkv", "webm"] = addIcon "file-video" "svg" x
 | anyInfix (extension u) ["tar", "zip", "xz", "img", "bin", "pkl", "onnx", "pt"] = addIcon "archive" "svg" x
 | anyInfix (extension u) ["ebt", "mdb", "mht", "ttf"] = addIcon "misc" "svg" x
 | anyInfix (extension u) ["epub"] = addIcon "EPUB" "text,sans,quad" x
 | host u == "docs.google.com" = addIcon "worddoc" "svg" x
 | host u == "imgur.com"       = addIcon "image" "svg" x
 | "/static/" `T.isPrefixOf` u && hasExtension ".html" u  = addIcon "code" "svg" x
 | isLocal u && hasExtension ".php" u                     = addIcon "code" "svg" x
 | anyInfix u [".pdf", "/pdf", "type=pdf", "pdfs.semanticscholar.org", "citeseerx.ist.psu.edu", "eprint.iacr.org",
                              "pdfs.semanticscholar.org"] = addIcon "pdf"  "svg" x
 | otherwise = x
linkIcon x = x

hasIcon :: Inline -> Bool
hasIcon (Link (_,_,ks) _ (_,_)) =
  case lookup "link-icon" ks of
    Just _ -> True
    Nothing -> case lookup "link-icon-type" ks of
                 Just _ -> True
                 Nothing -> False
hasIcon _ = True

addIcon :: T.Text -> T.Text -> Inline -> Inline
addIcon icon iconType x@(Link (idt,cl,ks) a (b,c)) =
  if hasIcon x then x else Link (idt,cl,
                                  [("link-icon",icon), ("link-icon-type",iconType)] ++
                                  ks) a (b,c)
addIcon _ _ x = x

anyInfix :: T.Text -> [T.Text] -> Bool
anyInfix string = Prelude.any (`T.isInfixOf` string)

isLocal :: T.Text -> Bool
isLocal s = T.head s == '/'

hasExtension :: T.Text -> T.Text -> Bool
hasExtension ext p = extension p == ext

extension :: T.Text -> T.Text
extension = T.pack . maybe "" (takeExtension . uriPath) . parseURIReference . T.unpack

host :: T.Text -> T.Text
host p = do let uri = parseURIReference (T.unpack p)
            case uri of
              Nothing -> ""
              Just uri' -> do let uridomain = uriAuthority uri'
                              case uridomain of
                                Nothing -> ""
                                Just uridomain' -> T.pack $ uriRegName uridomain'
