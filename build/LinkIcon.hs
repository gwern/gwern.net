{-# LANGUAGE OverloadedStrings #-}

module LinkIcon (linkIcon) where

import Data.Text as T (append, head, isInfixOf, isPrefixOf, isSuffixOf, pack, unpack, Text)
import Text.Pandoc (Inline(Link))
import Network.URI (parseURIReference, uriAuthority, uriPath, uriRegName)
import System.FilePath (takeExtension)

-- Statically, at compile-time, define the link-icons for links. Doing this at runtime with CSS is entirely possible and originally done by links.css, but the logic becomes increasingly convoluted & bug-prone because of CSS properties like cascading & longest-matches, and exceptions like 'organization icon overrides PDF icon' become fertile sources of errors & regressions.
-- Doing this at runtime in Haskell is easier and also reduces performance burden on the client browser.

-- Based on <links.js>.
-- The idea is to annotate every `<a>` with two new `data-` attributes, `data-link-icon` and `data-link-icon-type` which jointly specify the type & content of the icon. The link-icon for 'svg' type is overloaded to be a filename in `/static/img/icons/$LINKICON.svg`.
--
-- λ linkIcon $ Link nullAttr [Str "foo"] ("https://forums.evageeks.org/forum?id=2222", "")
-- Link ("",[],[("link-icon","EG"),("link-icon-type","text")]) [Str "foo"] ("https://forums.evageeks.org/forum?id=2222","")
-- λ linkIcon $ Link nullAttr [Str "foo"] ("/docs/foo.pdf", "Foo & Bar 2022")
-- → Link ("",[],[("link-icon","pdf"),("link-icon-type","svg")]) [Str "foo"] ("/docs/foo.pdf","Foo & Bar 2022")
-- → <a href="/docs/foo.pdf" data-link-icon="pdf" data-link-icon-type="svg" title="Foo &amp; Bar 2022">foo</a>
--
-- TODO: the PDF checks are incomplete (and only look for ".pdf" essentially) but since I'm trying to remove all weird non-standard PDFs and host locally all PDFs with clean names & extensions, maybe that's a vestigial concern?
linkIcon :: Inline -> Inline
linkIcon x@(Link (_,cl,_) _ (u, _))
 -- Short-circuits for manual control (one can either disable icons with a `[Foo](URL){.no-icon}` class, or specify a preferred icon on a link, like `[Foo](URL){.link-icon="deepmind" .link-icon-type="svg"}` by specifying the attributes directly):
 | "no-icon" `elem` cl = x
 | hasIcon x           = x
 -- organizational mentions or affiliations take precedence over domain or filetypes; typically matches anywhere in the URL.
 | u' "deepmind"  = aI "deepmind" "svg" -- DeepMind; match articles or anchors about DM too. Primary user: deepmind.com, DM papers on Arxiv
 | u' "facebook"  = aI "facebook" "svg"
 | u' "google"    = aI "google" "svg" -- Google searches, other tools. Note that there are many Google subdomains, which we may wish to iconify differently, so we narrow down with just ‘www’. Google Brain doesn’t have any consistent or recognizable logo, don’t bother trying to replicate one of the dots (no one will recognize it); use ‘GB’ would not be a bad idea, but I suspect that would also confuse people. So reusing the ‘G’ is the least bad option.
 | u' "nvidia"    = aI "nvidia" "text" -- Nvidia: https://en.wikipedia.org/wiki/Nvidia#cite_note-2 yeah no
 | u' "openai"    = aI "openai" "svg" -- OpenAI; match articles or anchors about OA too. primary user: openai.com, Arxiv papers
 | u' "microsoft" = aI "microsoft" "text,sans,italic" -- Microsoft: I don’t think https://en.wikipedia.org/wiki/File:Microsoft_logo_(2012).svg  s all that recognizable, so make a logotype more like https://en.wikipedia.org/wiki/File:Microsoft_logo_(1987).svg : an italic sans "MS".
 -- Domains:
 | u'' "psyarxiv.com" = aI "ψ" "text" -- Unicode trickery icons: GREEK SMALL LETTER PSI
 | u'' "unsongbook.com" = aI "ℵ" "text" -- SSC’s book: (ℵ) ALEF SYMBOL (We use the math symbol instead of the Hebrew deliberately, to avoid triggering bizarre Hebrew bidirectional text-related layout bugs on Mac Firefox.)
 | u'' "andrewgelman.com" || u'' "statmodeling.stat.columbia.edu" = aI "▅▇▃" "text" -- Favicon is a little normal distribution/histogram (▅▇▃) LOWER FIVE EIGHTHS BLOCK, LOWER SEVEN EIGHTHS BLOCK, LOWER THREE EIGHTHS BLOCK
 | u'' "meltingasphalt.com" = aI "▲" "text" -- Kevin Simler’s Melting Asphalt blog uses 3 triangles but that's too many, so we just use one. (▲) BLACK UP-POINTING TRIANGLE
 | "www.bloomberg.com" `T.isSuffixOf` host u = aI "𝐁" "text" -- Bloomberg: no usable logo, just an inset-B (𝐁) MATHEMATICAL BOLD CAPITAL B
 | u'' "link.springer.com" = aI "♘" "text"  -- (♘) WHITE CHESS KNIGHT
 | u'' "www.tinyletter.com" = aI "✉" "text" -- TinyLetter’s icon, without color, isn’t memorable enough; throw in the other email services (✉) ENVELOPE
 | u'' "groups.google.com" = aI "✉" "text"
 | u'' "groups.yahoo.com" = aI "✉" "text"
 | u'' "www.mail-archive.com" = aI "✉" "text"
 | u'' "medium.com" = aI "𝐌" "text" -- Medium: cheaper to abuse Unicode (𝐌) MATHEMATICAL BOLD CAPITAL M
 | u'' "marginalrevolution.com" = aI "M𝐑" "text" -- MR: cheaper to abuse Unicode (𝐑) MATHEMATICAL BOLD CAPITAL R
 | u'' "haskell.org" && (extension u /= "hs") = aI "𝛌" "text" -- Haskell: simplify logo; the double-lambda is too busy when used for link icons (𝛌) MATHEMATICAL BOLD SMALL LAMBDA primary user: hackage.haskell.org; we make an exception for .hs files hosted on Haskell.org, like config files, where the source code-ness is more relevant than the organization/domain
 | u'' "arxiv.org" || u'' "ar5iv.org" = aI "𝛘" "text" --  ArXiv: Their skull+smiley logo is too bizarre & off-putting to use, in addition to not working as a tiny monochrome image (𝛘) MATHEMATICAL BOLD SMALL CHI (bold makes it show up better when tiny)
 | "theatlantic.com" `T.isSuffixOf` host u = aI "A" "text,italic" -- The Atlantic: replicate sloping by italics
 | "alignmentforum.org" `T.isSuffixOf` host u = aI "AF" "text"
 | "animenewsnetwork.com" `T.isSuffixOf` host u = aI "ANN" "text"
 | u'' "arstechnica.com" = aI "ars" "text,sans" -- Ars is an orange box, not usable
 | "bbc.com" `T.isSuffixOf` host u || "bbc.co.uk" `T.isSuffixOf` host u = aI "BBC" "text,sans" -- BBC: no usable logo
 | u'' "www.cdc.gov" = aI "CDC" "text"
 | u'' "justice.gov" = aI "DoJ" "text" -- US federal Department of Justice
 | u'' "www.edge.org" = aI "E" "text,italic"
 | u'' "economist.com" = aI "E" "text,sans" -- Economist: logo is just ‘Economist’...
 | u'' "sciencedirect.com" = aI "E" "text" -- Elsevier/Sciencedirect.com: also an ‘E’
 | u'' "evageeks.org" || u'' "evamonkey.com" = aI "EG" "text" -- Evangelion: we’ll split this into EGF-related and other NGE sites
 | u'' "fda.gov" = aI "FDA" "text,sans" -- U.S. Food & Drug Administration
 | u'' "www.fanfiction.net" = aI "FF" "text" -- The FF.net logo is pretty crazy, and I don’t think anyone would recognize it in monochrome
 | u'' "goodreads.com" = aI "GR" "text" -- GoodReads: logo doesn’t make sense as a grayscale
 | u'' "harney.com" = aI "H" "text" -- The Harney & Sons logo is too fancy to scale down reasonably
 | u'' "kk.org" = aI "KK" "text,sans" -- Kevin Kelly
 | u'' "lesswrong.com" || u'' "www.greaterwrong.com" = aI "LW" "text" -- LW logo is just a colored ‘LW’, so no point in converting. Other user: wiki.lesswrong.com
 | u'' "myanimelist.net" = aI "MAL" "text,sans" -- MAL: the blue of their logo doesn’t work, so just text
 | u'' "www.motherjones.com" = aI "MJ" "text,sans"
 | u'' "nature.com" = aI "n" "text" -- Nature
 | anyInfix u ["onegeek.org", "eva-fan.com", "evaotaku.com", "khara.co.jp", "gainax.co.jp", "17th-angel.tumblr.com", "gainax.com"] = aI "NGE" "text" -- Primary user: forum.evageeks.org wiki.evageeks.org
 | u'' "www.overcomingbias.com" = aI "OB" "text" -- OB logo too bad to use
 | u'' "academic.oup.com" = aI "OUP" "text" -- Oxford Academic Journals / OUP
 | u'' "poniesatdawn.bandcamp.com" = aI "P@D" "text"
 | u'' "theparisreview.org" = aI "PR" "text" -- The Paris Review: not even going to try to make their weird bird logo work
 | u'' "r-project.org" || u'' "rstudio.com" = aI "R" "text" -- R: at this point R Studio has taken over a lot of control of the R ecosystem, so might as well treat them as official too... primary user: cran.r-project.org
 | u'' "science.org" || u'' "sciencemag.org" = aI "S" "text" -- Science is just typeset in red
 | u'' "slate.com" = aI "S" "text,sans"
 | u'' "salon.com" = aI "s" "text"
 | u'' "scholars-stage.org" = aI "Ss" "text" -- Avoid the unfortunate connotations of ‘SS’
 | u'' "slatestarscratchpad.tumblr.com" || u'' "astralcodexten.substack.com" || (isLocal u && (u' "yvain" ||  u' "slatestarcodex")) || (u'' "slatestarcodex.com" && (extension u /= "pdf")) = aI "SSC" "text" -- SSC logo too bad to use; NOTE: we want PDFs merely hosted on SSC to not match, and fall through to get a PDF icon instead
 | u'' "technologyreview.com" = aI "T" "text,sans" -- Technology Review (their logo has a little slash in it which you probably can’t see at low-res) but is otherwise just a ‘T’ so meh
 | u'' "tvtropes.org" = aI "TV" "text" -- TV Tropes: their lampshade icon is unrecognizable & hard to see small
 | u'' "urth.net" = aI "U" "text" -- Gene Wolfe mailing list; no logo; primary user: lists.urth.net
 | u'' "www.vanityfair.com" = aI "VF" "text"
 | u'' "www.vox.com" = aI "Vox" "text,italic"
 | u'' "wiley.com" = aI "W" "text,sans" -- Wiley & Sons’s ‘W’ unfortunately overlaps with the WP ‘W’ but if we sans it, maybe that’ll help. primary user: onlinelibrary.wiley.com
 | u'' "wsj.com" = aI "WSJ" "text" -- The Wall Street Journal
 | anyInfix u ["longbets.org", "longnow.org", "rosettaproject.org", "theinterval.org"] = aI "X" "text,overline" -- Long Now Foundation projects
 | u'' "yunnansourcing.com" = aI "ys" "text"
 | u'' "predictionbook.com" = aI "?" "text,sans,bold" -- PB logo is confusing. A purple question mark...?
 -- Quad-letter-square icons.
 | u'' "www.cell.com" = aI "CELL" "text,quad,sans" -- Cell: their logo is unrecognizable (and dumb)
 | u'' "mlp.fandom.com" = aI "MLPW" "text,quad,sans,italic"
 | u'' "nber.org" && (extension u /= "pdf") = aI "NBER" "text,quad"
 | u'' "pnas.org" = aI "PNAS" "text,quad" -- PNAS: they don’t have a real logo, but their favicon does a nice little compact square (white text on blue background), and we can replicate that in CSS (but just as black text on white background, per our monochrome theme) [On second thought, all of the icons using background squares, like HN/YC, are very intense and hard to visually balance. It's probably better to leave PNAS as just a quad-letter.]
 | u'' "rand.org" = aI "RAND" "text,quad,sans"
 | u'' "sagepub.com" = aI "SAGE" "text,quad,sans" -- Sage Journals’s logo is a circled S... but would anyone recognize it? Primary user: journals.sagepub.com
 | u'' "publicdomainreview.org" = aI "TPDR" "text,quad"
 | u'' "xkcd.com" = aI "XKCD" "text,quad,sans" -- covers explainxkcd.com, what-if.xkcd.com...
 -- SVG icons (remember the link-icon name is substituted in as part of the URL to the SVG icon)
 | u'' "amazon.com" || u' "amazon.co." = aI "amazon" "svg"
 | u'' "bitcoin.it" || u'' "bitcointalk.org" = aI "bitcoin" "svg"
 | u'' "biorxiv.org" || u'' "medrxiv.org" = aI "chi-dna" "svg" -- BioRxiv (custom icon: italic Chi with DNA cross-strands).
 | u'' "distill.pub" = aI "distillpub" "svg" -- Distill ML journal.
 | u'' "www.dropbox.com" || u'' "dl.dropboxusercontent.com" = aI "dropbox" "svg" -- Dropbox: old file-host, deprecated since they’ve started killing inactive accounts & their UI become awful. primary user: dl.dropboxusercontent.com
 | u'' "erowid.org" = aI "erowid" "svg"
 | u'' "github.com" = aI "github" "svg" -- Github; I exclude github.io because that’s blogs.
 | u'' "scholar.google.com" = aI "google-scholar" "svg" -- Google Scholar.
 | u'' "paulgraham.com" || u' "ycombinator.com" = aI "hn" "svg" -- PG/HN/YC (shared logo). primary user: news.ycombinator.com
 | anyInfix u ["webcitation.org", "mementoweb.org", "archive.org", "archive-it.org", "archive-team.org", "waybackmachine.org"] || ("local-archive-link" `elem` cl && extension u /= "pdf") = aI "internetarchive" "svg"
 | u'' "mega.nz" = aI "mega" "svg" -- MegaUpload/Mega: filesharing (used for big files).
 | u'' "intelligence.org" = aI "miri" "svg" -- MIRI/intelligence.org.
 | u'' "nytimes.com" = aI "newyorktimes" "svg" -- The New York Times: manual edit, reducing full 'NEW YORK TIMES' SVG logo to just the ‘T’ they use as an icon.
 | u'' "nlm.nih.gov" = aI "nlm-ncbi" "svg" -- NCBI/Pubmed: simplification of their logo (https://upload.wikimedia.org/wikipedia/commons/0/07/US-NLM-NCBI-Logo.svg). primary user: ncbi.nlm.nih.gov
 | u'' "patreon.com" = aI "patreon" "svg" -- Patreon. (Used the old one (https://upload.wikimedia.org/wikipedia/commons/9/94/Patreon_logo.svg) because I don’t like the new one.)
 | anyInfix u ["plos.org", "plosone.org", "plosbiology.org", "plosmedicine.org"] = aI "plos" "svg" -- PLOS ONE in all their domain permutations... primary user: journals.plos.org
 | u' "reddit.com" = aI "reddit" "svg" -- old.reddit.com
 | anyInfix u ["overflow.net", "overflow.com", "stackexchange.com"] = aI "stackexchange" "svg" -- The *Exchange/*Overflow family of websites.
 | u' "substack.com" = aI "substack" "svg" -- gwern.substack.com
 | u'' "theguardian.com" || u'' "www.guardian.co.uk" = aI "theguardian" "svg" -- El Grauniad.
 | u'' "newyorker.com" = aI "thenewyorker" "svg" -- The New Yorker: the Dandy SVG, simplified & rotated more vertically.
 | u' "tumblr.com" = aI "tumblr" "svg"
 | u'' "twitter.com" || u'' "nitter.hu" = aI "twitter" "svg"
 | u'' "uptontea.com" = aI "uptontea" "svg"
 | u'' "soundcloud.com" = aI "audio" "svg"
 | u'' "bandcamp.com" = aI "audio" "svg"
 | u'' "washingtonpost.com" = aI "washingtonpost" "svg" -- The Washington Post: truncated their blackletter to ‘WP’.
 | anyInfix u ["wikipedia.org", "wikimedia.org", "wiktionary.org", "wikisource.org", "wikimediafoundation.org"] = aI "wikipedia" "svg" -- primary user: en.wikipedia.org, meta.wikimedia.org, en.wiktionary.org, en.wikisource.org
 | u'' "wired.com" = aI "wired" "svg"
 | u'' "www.youtube.com" || u'' "www.youtu.be" = aI "youtube" "svg"
 -- Filetypes: (we need to parse & extract the extension because many would be too short and match too many URLs if mere infix matching was used)
 | iE ["opml", "txt", "xml", "json", "jsonl", "page"] = aI "txt" "svg"
 | iE ["css", "hs", "js", "conf", "sh", "r", "patch", "diff"] = aI "code" "svg"
 | iE ["doc", "docx"] = aI "worddoc" "svg"
 | iE ["xls", "xlsx", "ods"] = aI "spreadsheet" "svg"
 | iE ["csv"] = aI "csv" "svg"
 | iE ["gif", "bmp", "ico", "jpg", "jpeg", "png", "svg", "xcf"] = aI "image" "svg"
 | iE ["mp3", "wav", "flac", "ogg", "rm"] = aI "audio" "svg"
 | iE ["swf", "mp4", "mkv", "webm"] = aI "file-video" "svg"
 | iE ["tar", "zip", "xz", "img", "bin", "pkl", "onnx", "pt"] = aI "archive" "svg"
 | iE ["ebt", "mdb", "mht", "ttf"] = aI "misc" "svg"
 | iE ["epub"] = aI "EPUB" "text,sans,quad"
 | u'' "docs.google.com" = aI "worddoc" "svg"
 | u'' "imgur.com" || u'' "i.imgur.com"       = aI "image" "svg"
 | "/static/" `T.isPrefixOf` u && hasExtension ".html" u  = aI "code" "svg"
 | isLocal u && hasExtension ".php" u                     = aI "code" "svg"
 | anyInfix u [".pdf", "/pdf", "type=pdf", "pdfs.semanticscholar.org", "citeseerx.ist.psu.edu", "eprint.iacr.org", "pdfs.semanticscholar.org"] = aI "pdf" "svg"
 | otherwise = x
 where u', u'' :: T.Text -> Bool
       -- simplest check for string anywhere
       u' v = v `T.isInfixOf` u
       -- more careful check:
       u'' v = isHostOrArchive v u
       aI :: T.Text -> T.Text -> Inline
       aI = addIcon x
       iE :: [T.Text] -> Bool
       iE = anyInfix (extension u)
linkIcon x = x

hasIcon :: Inline -> Bool
hasIcon (Link (_,_,ks) _ (_,_)) =
  case lookup "link-icon" ks of
    Just _ -> True
    Nothing -> case lookup "link-icon-type" ks of
                 Just _ -> True
                 Nothing -> False
hasIcon _ = True

addIcon :: Inline -> T.Text -> T.Text -> Inline
addIcon x@(Link (idt,cl,ks) a (b,c)) icon iconType  =
  if hasIcon x then x else Link (idt,cl,
                                  [("link-icon",icon), ("link-icon-type",iconType)] ++
                                  ks) a (b,c)
addIcon x _ _ = x

anyInfix :: T.Text -> [T.Text] -> Bool
anyInfix string = Prelude.any (`T.isInfixOf` string)

isLocal :: T.Text -> Bool
isLocal s = T.head s == '/'

hasExtension :: T.Text -> T.Text -> Bool
hasExtension ext p = extension p == ext

extension :: T.Text -> T.Text
extension = T.pack . maybe "" (takeExtension . uriPath) . parseURIReference . T.unpack

host :: T.Text -> T.Text
host p = do case parseURIReference (T.unpack p) of
              Nothing -> ""
              Just uri' -> do case uriAuthority uri' of
                                Nothing -> ""
                                Just uridomain' -> T.pack $ uriRegName uridomain'

isHostOrArchive :: T.Text -> T.Text -> Bool
isHostOrArchive pattern url = let h = host url in
                                h == pattern || ("/docs/www/"`T.append`pattern) `T.isPrefixOf` url
