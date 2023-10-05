{-# LANGUAGE OverloadedStrings #-}

module LinkIcon (linkIcon, linkIconTest, linkIconPrioritize) where

import Data.List (sort)
import qualified Data.Map.Strict as M (toList, fromListWith, map)
import Data.Maybe (fromJust)
import qualified Data.Text as T (append, drop, head, isInfixOf, isPrefixOf, pack, unpack, Text)
import Text.Pandoc (Inline(Link), nullAttr)
import Network.URI (parseURIReference, uriPath)
import System.FilePath (takeExtension)

import LinkBacklink (readBacklinksDB)
import Utils (host, hasKeyAL, anyPrefixT)
import qualified Config.LinkIcon as C (prioritizeLinkIconMin, prioritizeLinkIconBlackList, overrideLinkIcons, linkIconTestUnitsText)

-- Statically, at site 'compile-time', define the link-icons for links. Doing this at runtime with CSS is
-- entirely possible and originally done by links.css, but the logic becomes increasingly convoluted
-- & bug-prone because of CSS properties like cascading & longest-matches, and exceptions like
-- 'organization icon overrides PDF icon' become fertile sources of errors & regressions.
-- Doing this at compile-time in Haskell is easier and also reduces performance burden on the client
-- browser. For a more detailed discussion of the problems & solution, and history of prior link-icon
-- implementations, see <https://gwern.net/design-graveyard#link-icon-css-regexps>.

-- Rules for URL→icon. All supported examples: <https://gwern.net/lorem-link>
-- Supported icon types:
-- - "svg" (+$NAME of the SVG filename in </static/img/icons/$NAME>; must be dark-mode compatible);
-- - "text"+(1-4 Unicode characters) + comma-separated modifiers; text supports additional control:
--   - "sans" (given Gwern.net's default font is Source Serif Pro, it is serif by default, while many logotypes are deliberately sans so this enables Source Sans Pro),
--   - "mono" (IBM Plex),
--   - "italic" (serif italic weight),
--   - "overline",
--   - "tri" (for 3-letters, squeezing in horizontally),
--   - "quad" (turned into a 2×2 grid).
--  Most combinations will be valid so one can write "text,quad,mono" (eg. for a computing organization like 'IEEE'). Text effects beyond this can usually be achieved by some Unicode trickery, such as adding in HAIR SPACEs or using BOLD versions of characters. Emoji should also work with appropriate combining-characters but can be tricky to get working reliably cross-platform.
--
-- Rules: arbitrary pure Haskell can be used to match, and the order of rules matters to allow overrides/precedence (first rule to match wins, so higher=stronger); convenience helpers are provided to match a domain(s), anywhere(s) infix, or by extension(s). These also check for malformedness.
--
-- All rules MUST have a test-case exercising each sub-rule (if multiple domains are matched, each domain should have a test-case). Only one testcase is necessary in /lorem-link (because that's just testing that the link-icon itself looks right rendered by browsers, and not that said link-icon is being put on all the links it should be).
--
-- HTML/CSS implementation details:
-- Based on </static/js/links.js>. Text and SVG are styled as groups in </static/css/links.css>, and individual text-strings or SVGs can be styled individually (as is usually required).
-- The idea is to annotate every `<a>` with two new `data-` attributes, `data-link-icon` and
-- `data-link-icon-type` which jointly specify the type & content of the icon. The link-icon for
-- 'svg' type is overloaded to be a filename in `/static/img/icon/$LINKICON.svg`.
--
-- λ linkIcon $ Link nullAttr [Str "foo"] ("https://forum.evageeks.org/forum?id=2222", "")
-- Link ("",[],[("link-icon","EG"),("link-icon-type","text")]) [Str "foo"] ("https://forum.evageeks.org/forum?id=2222","")
-- λ linkIcon $ Link nullAttr [Str "foo"] ("/doc/foo.pdf", "Foo & Bar 2022")
-- → Link ("",[],[("link-icon","pdf"),("link-icon-type","svg")]) [Str "foo"] ("/doc/foo.pdf","Foo & Bar 2022")
-- → <a href="/doc/foo.pdf" data-link-icon="pdf" data-link-icon-type="svg" title="Foo &amp; Bar 2022">foo</a>
--
-- URL rewrite handling:
-- In cases of local archive links, matches on the `/doc/www/$DOMAIN/$ARCHIVE.html` aren't necessarily *exactly*
-- as powerful; local archives deliberately throw away sub-directory structure for simpler addresses, so 2 matches for
-- 'foo.com/bar/*' and 'foo.com/quux/*' would collide when trying to match just '/doc/www/foo.com/$ARCHIVE.html'.
-- For this case, we detect & exploit the `data-original-URL` attribute which is around for just such problems,
-- and we run matches on the original URL, and everything should work as expected then.
--
-- TODO: the PDF checks are incomplete (and only look for ".pdf" essentially) but it would require IO or perhaps
-- a caching database to actually detect what MIME type a live URL returns, which is a PITA, and since I'm trying
-- to remove all weird non-standard PDFs and host locally all PDFs with clean names & extensions,
-- maybe that's a vestigial concern?
-- TODO: refactor into multiple functions, like 'linkIconOrg', 'linkIconQuad' etc, and then move into Config.LinkIcon:
linkIcon :: Inline -> Inline
linkIcon x@(Link (_,cl,_) _ (u, _))
 -- Short-circuits for manual control (one can either disable icons with a `[Foo](URL){.icon-not}`
 -- class, or specify a preferred icon on a link, like `[Foo](URL){.link-icon="deepmind"
 -- .link-icon-type="svg"}` by specifying the attributes directly), or define a global URL/(link
 -- icon, link icon type) rewrite:
 | "icon-not" `elem` cl = x
 -- NOTE: 'gwern': the Fraktur '𝔊' SVG logo (used to be the Unicode icon but looks a bit fuzzy & squashed as a link-icon so has been replaced by an edit of the Gwern.net logo) for local essay links (where 'local' is defined as '/' but with no '.' in it) is set dynamically client-side by rewrite.js:l1075 (`designateSpecialLinkIcons`) and so we do not handle it here. (It is also overridden by 'icon-not'; WARNING: 'icon-not' is used at runtime and should not be erased!)
 | hasIcon x           = x
 | hasKeyAL u C.overrideLinkIcons = let (i,it) = fromJust $ lookup u C.overrideLinkIcons in addIcon x i it
 | anyPrefixT u ["/metadata/annotation/"] = x

 | "directory-indexes-upwards"   `elem` cl = aI "arrow-up-left"    "svg"
 | "directory-indexes-downwards" `elem` cl = aI "arrow-down-right" "svg"
 | "directory-indexes-sideways"  `elem` cl = aI "arrow-right"      "svg"
 -- organizational mentions or affiliations take precedence over domain or filetypes; typically matches anywhere in the URL.
 | u' "deepmind"  = aI "deepmind" "svg" -- DeepMind; match articles or anchors about DM too. Primary user: deepmind.com, DM papers on Arxiv
 | u' "schmidhuber" || u' "people.idsia.ch/~juergen/" = aI "SMDH" "text,quad,sans" -- Jürgen Schmidhuber homepage & tagged links; should override Arxiv, Bloomberg, NYT, Facebook etc
 | u' "facebook" || u' ".fb.com"  = aI "facebook" "svg"
 | u' "sites.google.com/berkeley.edu" || aU'' ["ml.berkeley.edu", "people.eecs.berkeley.edu", "bair.berkeley.edu"] = aI "BAIR" "text,quad,mono" -- Berkeley AI Research: Chelsea Finn, Sergey Levine, robotics, Decision Transformer, etc. Overrides Google Sites.
 | u' "pandoc" && not (u' "tufte") = aI "PNDC" "text,quad,sans" -- general catch-all, overrides Google Groups (`pandoc-discuss`), with a further override for Tufte-related stuff (which get 'ET')
 | u'' "groups.google.com" = aI "✉" "text"
 | u'' "scholar.google.com" = aI "google-scholar" "svg" -- Google Scholar.
 | u'' "docs.google.com" = aI "worddoc" "svg"
 | u' "google" || u'' "magenta.tensorflow.org" = aI "alphabet" "svg" -- Google searches, other tools. Note that there are many Google subdomains, which we may wish to iconify differently, so we narrow down with just ‘www’. Google Brain doesn’t have any consistent or recognizable logo, don’t bother trying to replicate one of the dots (no one will recognize it); use ‘GB’ would not be a bad idea, but I suspect that would also confuse people. So reusing the ‘G’ is the least bad option. [the SVG has been renamed 'alphabet' instead of the expected 'google' because two default uBlock lists block the regexp 'icons/google.*' as it is usually abused for social-media spamming icons]
 | aU' ["twitter.com/sigfpe/", "blog.sigfpe.com", "github.com/dpiponi"] = aI "sgfp" "text,quad,monospace" -- sigfpe/Dan Piponi: Haskell, math, computer graphics etc
 | u' "nvidia"  || aU'' ["nvlabs.github.io", "nv-adlr.github.io", "nv-tlabs.github.io"] = aI "n" "text,sans,italic" -- Nvidia: <https://en.wikipedia.org/wiki/Nvidia#cite_note-2> yeah no. Disambiguate from Nature's "n" by italicizing (Nvidia *did* italicize the lowercase 'n' for a long time, so seems reasonable)
 | u' "openai" || u'' "gptprompts.wikidot.com" = aI "openai" "svg" -- OpenAI; match articles or anchors about OA too. primary user: openai.com, Arxiv papers. Brockman's GPT-prompts wiki is semi-official IMO.
 | u' "microsoft" = aI "MS" "text,sans,italic" -- Microsoft: I don’t think <https://en.wikipedia.org/wiki/File:Microsoft_logo_(2012).svg> is all that recognizable, so make a logotype more like <https://en.wikipedia.org/wiki/File:Microsoft_logo_(1987).svg>: an italic sans "MS".
 | u' "#anthropic" || u' "twitter.com/jackclarkSF/" || aU'' ["transformer-circuits.pub", "www.anthropic.com", "jack-clark.net"] = aI "anthropic" "svg" -- need to override Arxiv; handle Jack Clark (co-founder) newsletter & social media
 | u' "#laion"  || u' "LAION-AI" || u'' "laion.ai" = aI "laion" "svg" -- <https://laion.ai/favicon.svg>; need to override Arxiv & Github & Hugging Face

 -- Domains:
 | aU'' ["psyarxiv.com", "files.osf.io", "osf.io"] = aI "ψ" "text" -- Unicode trickery icons: GREEK SMALL LETTER PSI
 | u'' "unsongbook.com" = aI "ℵ" "text" -- SSC’s book: (ℵ) ALEF SYMBOL (We use the math symbol instead of the Hebrew deliberately, to avoid triggering bizarre Hebrew bidirectional text-related layout bugs on Mac Firefox.)
 | u'' "andrewgelman.com" || u'' "statmodeling.stat.columbia.edu" = aI "▅▇▃" "text" -- Favicon is a little normal distribution/histogram (▅▇▃) LOWER FIVE EIGHTHS BLOCK, LOWER SEVEN EIGHTHS BLOCK, LOWER THREE EIGHTHS BLOCK
 | u'' "meltingasphalt.com" = aI "▲" "text" -- Kevin Simler’s Melting Asphalt blog uses 3 triangles but that's too many, so we just use one. (▲) BLACK UP-POINTING TRIANGLE
 | u'' "www.tinyletter.com" = aI "✉" "text" -- TinyLetter’s icon, without color, isn’t memorable enough; throw in the other email services (✉) ENVELOPE
 | aU'' ["blog.givewell.org", "www.givewell.org", "files.givewell.org"] || u' "groups.yahoo.com/group/givewell/" = aI "GW" "text"
 | u'' "groups.yahoo.com" = aI "✉" "text"
 | u'' "www.mail-archive.com" = aI "✉" "text"
 | u' "carryiton.net/chain-letter/" = aI "✉" "text" -- linked only for the archive, so this is an appropriate icon
 | u'' "marginalrevolution.com" = aI "M𝐑" "text" -- MR: cheaper to abuse Unicode (𝐑) MATHEMATICAL BOLD CAPITAL R
 | u'' "www.frontiersin.org" = aI "FS" "text,sans" -- <https://en.wikipedia.org/wiki/Frontiers_Media> multiple-cubes logo too busy for an icon, no Unicode equivalent
 | aU'' ["www.gutenberg.org", "gutenberg.ca", "gutenberg.net.au", "www.fadedpage.com"] = aI "PG" "text" -- Faded Pages isn't strictly-speaking a Project Gutenberg org, but they work with Distributed Proofreaders & their work is in PG Canada and they do similar things so meh.
 | u'' "guzey.com" = aI "A.G." "text,sans"
 | u'' "www.forbes.com" = aI "F" "text"
 | (u' "haskell.org" && (extension u /= ".hs")) || u' "haskellers.com" = aI "𝛌" "text" -- Haskell: simplify logo; the double-lambda is too busy when used for link icons (𝛌) MATHEMATICAL BOLD SMALL LAMBDA primary user: hackage.haskell.org; we make an exception for .hs files hosted on Haskell.org, like config files, where the source code-ness is more relevant than the organization/domain
 | u'' "arxiv.org" || u'' "ar5iv.labs.arxiv.org" || u'' "proceedings.mlr.press" = aI "𝛘" "text" --  ArXiv: Their skull+smiley logo is too bizarre & off-putting to use, in addition to not working as a tiny monochrome image (𝛘) MATHEMATICAL BOLD SMALL CHI (bold makes it show up better when tiny); I lump in 'PMLR' ("Proceedings of Machine Learning Research") because many PMLR were just Arxiv preprints beforehand & it amounts to about the same thing, really.
 | u' ".bloomberg.com" || u'' "www.businessweek.com" = aI "𝐁" "text" -- Bloomberg: no usable logo, just an inset-B (𝐁) MATHEMATICAL BOLD CAPITAL B
 | u' "theatlantic.com" = aI "A" "text,italic" -- The Atlantic: replicate sloping by italics
 | u' "alignmentforum.org" || (u'' "www.greaterwrong.com" && u' "view=alignment-forum") = aI "AF" "text,sans"
 | u'' "boingboing.net" = aI "bb" "text,mono"
 | u'' "nymag.com" = aI "𝒩𝒴" "text"
 | u'' "thebrowser.com" = aI "TB" "text"
 | u'' "crookedtimber.org" = aI "CT" "text"
 | u' ".latimes.com" = aI "𝔏A" "text"
 | u'' "www.dailymail.co.uk" = aI "𝔐" "text" -- 𝔐 MATHEMATICAL FRAKTUR CAPITAL M
 | aU'' ["danbooru.donmai.us", "derpibooru.org", "safebooru.org"] = aI "❐" "text" -- ❐ U+2750 UPPER RIGHT DROP-SHADOWED WHITE SQUARE
 | u'' "www.edge.org" = aI "E" "text,italic"
 | u'' "www.economist.com" = aI "E" "text,sans" -- Economist: logo is just ‘Economist’… There is a sibling magazine <https://en.wikipedia.org/wiki/1843_(magazine)> which I don't seem to link to.
 | u'' "everything2.com" = aI "E2" "text"
 | u'' "examine.com" = aI "Eχ" "text,sans"
 | u'' "www.sciencedirect.com" = aI "E" "text" -- Elsevier/Sciencedirect.com: also an ‘E’
 | u'' "www.esquire.com" = aI "ℰ" "text"
 | aU'' ["wiki.evageeks.org","forum.evageeks.org","www.evamonkey.com"] || u' "twitter.com/EvaMonkey/" = aI "EG" "text" -- Evangelion: we’ll split this into EGF-related and other NGE sites
 | aU'' ["www.fda.gov","fis.fda.gov","clinicaltrials.gov", "classic.clinicaltrials.gov"] = aI "FDA" "text,tri,sans" -- U.S. Food & Drug Administration
 | u'' "hpmor.com" || u' "www.fanfiction.net/r/5782108/" || u' "www.reddit.com/r/HPMOR/" = aI "MoR" "text,tri,italic"
 | u' "mozilla.org" = aI "FF" "text,sans" -- none of the available Firefox SVG logos worked well as a link icon; typically, too much detail, the swirly-spikes too indistinct & under-emphasized, and confusable with DeepMind.
 | u'' "www.goodreads.com" = aI "GR" "text" -- GoodReads: logo doesn’t make sense as a grayscale
 | u'' "www.harney.com" = aI "H" "text" -- The Harney & Sons logo is too fancy to scale down reasonably
 | u'' "www.hustwit.com" = aI "H" "text,sans" -- design documentarian
 | u'' "kk.org" = aI "KK" "text,sans" -- Kevin Kelly
 | aU'' ["www.lesswrong.com", "sl4.org", "wiki.lesswrong.com", "www.greaterwrong.com"] = aI "LW" "text" -- LW logo is just a colored ‘LW’, so no point in converting. Other user: wiki.lesswrong.com
 | u'' "www.longecity.org" = aI "⧖" "text" -- Longecity “⧖” U+29D6 WHITE HOURGLASS UNICODE
 | aU'' ["michaelnielsen.org", "quantum.country", "numinous.productions", "cognitivemedium.com", "neuralnetworksanddeeplearning.com"] = aI "MN" "text"
 | u'' "www.motherjones.com" = aI "MJ" "text,sans"
 | u' ".nature.com" = aI "n" "text" -- Nature
 | u'' "openreview.net" = aI "OR" "text,sans" -- doesn't seem to have any real logo or wordmark: <https://openreview.net/about>
 | u'' "www.overcomingbias.com" || u' "mason.gmu.edu/~rhanson/" = aI "OB" "text" -- OB logo too bad to use
 | u'' "www.theparisreview.org" = aI "PR" "text" -- The Paris Review: not even going to try to make their weird bird logo work
 | u'' "www.theverge.com" = aI "▽" "text" -- The Verge uses a sort of delta Escher triangle-esque 'V' stylization <https://en.wikipedia.org/wiki/The_Verge> which looks like a triangle pointing down, so, ▽ WHITE DOWN-POINTING TRIANGLE (Nabla operator) &#x25BD; &#9661;
 | u'' "www.quora.com" = aI "Q" "text" -- surprisingly, no one's taken 'Q' yet
 | aU'' ["www.edwardtufte.com", "edwardtufte.github.io"] || aU' ["github.com/edwardtufte/", "github.com/jez/tufte-pandoc-jekyll", "github.com/jez/tufte", "github.com/clayh53/tufte", "/doc/design/typography/rubrication/1990-tufte-envisioninginformation-ch5-byrneseuclid.pdf", "cran.r-project.org/web/packages/tufte/"] = aI "ET" "text"
 | aU'' ["cran.r-project.org", "www.r-project.org", "lme4.r-forge.r-project.org", "www.metafor-project.org", "rstudio.com"] || u' "github.com/paul-buerkner/brms" = aI "R" "text" -- R: at this point R Studio has taken over a lot of control of the R ecosystem, so might as well treat them as official too… primary user: cran.r-project.org
 | u'' "www.science.org" || u'' "sciencemag.org" = aI "S" "text" -- Science is just typeset in red
 | u'' "www.sciencedaily.com" = aI "SD" "text,sans"
 | u'' "www.sciencenews.org" = aI "SN" "text,sans" -- <https://en.wikipedia.org/wiki/Science_News>
 | u'' "sethroberts.net" = aI "SR" "text,sans" -- Logo is a sans 'S' on a red circle background; can't use 'S' because already used by Slate.
 | u'' "slate.com" = aI "S" "text,sans"
 | u'' "www.salon.com" = aI "s" "text"
 | u'' "scholars-stage.org" = aI "Ss" "text" -- Avoid the unfortunate connotations of ‘SS’
 | u'' "www.technologyreview.com" = aI "T" "text,sans" -- Technology Review (their logo has a little slash in it which you probably can’t see at low-res) but is otherwise just a ‘T’ so meh
 | aU'' ["texample.net", "ctan.org", "www.tug.org", "tug.org"] = aI "tex" "svg" -- Properly turning the 'TeX' logotype in a link icon is hard. You can't use the official logo: <https://commons.wikimedia.org/wiki/File:TeX_logo.svg> is unworkable as a tiny icon, Computer Modern's thinness issues are massively exacerbated & it's unreadable (it's not great on computer screens to begin with, and shrunk down to a link-icon, even worse); you can cheat in pure Unicode with 'TₑX' (LATIN SUBSCRIPT SMALL LETTER E U+2091;, there is no 'LARGE LETTER E' unfortunately) but this took winds up looking pretty bad in practice. So what I did was create my own SVG TeX link-icon in Inkscape, using Source Serif Pro bold letters, arranged by hand like the logotype, and then rescaled horizontally ~120% to make the strokes thick enough that they'd survive downscaling. *That* works.
 | u'' "tvtropes.org" = aI "TV" "text" -- TV Tropes: their lampshade icon is unrecognizable & hard to see small
 | aU'' ["www.urth.net", "lists.urth.net", "www.wolfewiki.com"] = aI "U" "text" -- Gene Wolfe mailing list; no logo; primary user: lists.urth.net
 | u'' "www.vanityfair.com" = aI "VF" "text"
 | u' "onlinelibrary.wiley.com" = aI "W" "text,sans" -- Wiley & Sons’s ‘W’ unfortunately overlaps with the WP ‘W’ but if we sans it, maybe that’ll help. primary user: onlinelibrary.wiley.com
 | aU' ["longbets.org", "longnow.org", "rosettaproject.org", "theinterval.org"] = aI "X" "text,overline" -- Long Now Foundation projects
 | u'' "yunnansourcing.com" || u'' "yunnansourcing.us" = aI "ys" "text"
 | u'' "predictionbook.com" = aI "?" "text,sans,bold" -- PB logo is confusing. A purple question mark…?
 | u'' "silkroadvb5piz3r.onion" || u'' "silkroad5v7dywlc.onion" = aI "SR1" "text,sans"
 | u'' "beepb00p.xyz" = aI "\129302" "text" -- ROBOT FACE U+1F916
 | u'' "antilop.cc" = aI "෴" "text" -- SINHALA PUNCTUATION KUNDDALIYA 0x0DF4 - because it's written by "Moustache", get it
 | u'' "memteaimports.com" = aI "MT" "text,sans"
 | u'' "forum.effectivealtruism.org" || u'' "www.effectivealtruism.org" = aI "EA" "text"
 | u'' "boards.fireden.net" || u'' "archive.foolz.us" || u' "4channel.org"  = aI "4CH" "text,sans"
 | u'' "www.kaggle.com" = aI "k" "text,sans"
 | u'' "www.jneurosci.org" = aI "JN" "text"
 | u'' "www.discovermagazine.com" = aI "D" "text"
 | u'' "tl.net" = aI "TL" "text,sans"
 | u'' "www.businessinsider.com" = aI "BI" "text,sans"
 | u'' "dnstats.net" = aI "dn" "text,sans"
 | u'' "www.newsweek.com" = aI "NW" "text" -- logo is 'N' but there are too many 'N's floating around, so abbreviate 'Newsweek' as 'NW'
 | u'' "www.thecut.com" = aI "TC" "text"
 | u'' "www.scientificamerican.com" = aI "SA" "text"
 | u'' "www.mirror.co.uk" = aI "M" "text,sans"
 | u'' "www.stuff.co.nz" = aI "NZ" "text,sans" -- even their official name 'Stuff' is lazy and unmemorable. I just think of them as 'that New Zealand website reporting on crime & DNM stuff'…
 | u'' "chronopause.com" = aI "M.D." "text,sans" -- Mike Darwin, similarly TODO: experiment with initials using periods - does this work as-is? How about quad? '﹒' SMALL FULL STOP U+FE52 does not work.
 | u'' "vitalik.ca" || u' "/doc/economics/mechanism-design/quadratic-voting/2018-buterin.pdf" = aI "V.B." "text,sans" -- Vitalik Buterin, similarly
 | u'' "unenumerated.blogspot.com" || u' "szabo.best.vwh.net" || u' "nick-szabo" = aI "N.S." "text,sans" -- Nick Szabo
 | u'' "scottaaronson.blog" || u'' "www.scottaaronson.com" = aI "S.A." "text,sans" -- Scott Aaronson
 | u'' "www.rifters.com" = aI "P.W." "text,sans" -- Peter Watts
 | u'' "www.antipope.org" = aI "C.S." "text,sans" -- Charles Stross
 | u'' "www.ribbonfarm.com" = aI "ℝ𝔽" "text,sans"
 | aU'' ["en.touhouwiki.net", "touhou.fandom.com", "w.atwiki.jp"] || u' "www.reddit.com/r/TOUHOUMUSIC/" = aI "☯" "text" -- NOTE: override Fandom catch-all
 | u'' "www.reuters.com" = aI "R" "text,sans" -- the official Reuters logo <https://en.wikipedia.org/wiki/File:Reuters_Logo.svg> looks like it's summoning a seraphim
 | u'' "www.deviantart.com" = aI "DA" "text,sans" -- the official logo <https://en.wikipedia.org/wiki/File:DeviantArt_Logo.svg> isn't *too* bad and is at least 8 years old, but I don't recognize it so I doubt most readers would.
 | u'' "www.smithsonianmag.com" = aI "SM" "text"
 | u'' "scienceblogs.com" = aI "Sᵇ" "text,sans,italic"
 | u'' "www.theage.com.au" = aI "A" "text"
 | u'' "www.dailydot.com" = aI "D." "text,sans"
 | u'' "www.johndcook.com" = aI "JC" "text,sans"
 | u'' "www.candyjapan.com"  = aI "🍬" "text"
 | aU'' ["www.cambridge.org", "journals.cambridge.org", "static.cambridge.org"] = aI "⛨" "text" -- ⛨ BLACK CROSS ON SHIELD U+26E8, roughly imitating <https://en.wikipedia.org/wiki/Coat_of_arms_of_the_University_of_Cambridge>
 | u' "royalsocietypublishing.org" = aI "RS" "text" -- <https://en.wikipedia.org/wiki/Royal_Society>
 | u'' "www.sequentialtart.com" = aI "ST" "text,sans"
 | u'' "www.psychologytoday.com" = aI "PT" "text,sans"
 | u'' "www.independent.co.uk" = aI "TI" "text" -- <https://en.wikipedia.org/wiki/File:The_Independent_news_logo.svg> swooping-hawk icon would be illegible as link icon
 | u'' "www.fastcompany.com" = aI "FC" "text"
 | u'' "elifesciences.org" = aI "eL" "text,sans"
 | u'' "www.w3.org" = aI "W3" "text,sans"
 | u'' "www.metafilter.com" || u'' "ask.metafilter.com" = aI "MF" "text,sans,italic"
 | u'' "qz.com" = aI "QZ" "text,sans"
 | u'' "blog.23andme.com" || u'' "23andme.com" = aI "23" "text"
 | u'' "www.ft.com" = aI "FT" "text"
 | u'' "techcrunch.com" = aI "TC" "text,mono"
 | u'' "www.nzherald.co.nz" = aI "𝕳" "text"
 | u'' "nintil.com" = aI "𝓝" "text" -- @ArtirKel José Luis Ricón Fernández de la Puente
 | u' "livejournal.com" = aI "LJ" "text,sans"
 | u'' "www.newscientist.com" = aI "NS" "text,sans"
 | u'' "www.brookings.edu" = aI "B" "text"
 | u'' "variety.com" = aI "𝓥" "text"
 | u'' "theconversation.com" = aI "🗨" "text"
 | u'' "patch.com" = aI "P" "text,sans"
 | u'' "www.palladiummag.com" = aI "Pd" "text,sans" -- "P" is their logo but that is too generic and collides, so take 'palladium' literally & use the element abbreviation
 | u'' "www.gq.com" = aI "GQ" "text,sans"
 | aU'' ["bls.gov", "data.bls.gov", "www.bls.gov"] = aI "BLS" "text,sans"
 | u'' "thegradient.pub" = aI "∇" "text"
 | u'' "www.projectrho.com" = aI "ρ" "text"
 | u'' "harpers.org" = aI "H" "text"
 | u'' "foreignpolicy.com" = aI "FP" "text"
 | u'' "www.unqualified-reservations.org" = aI "UR" "text"
 | u'' "www.thenewatlantis.com" = aI "NA" "text"
 | aU'' ["www.supermemo.com", "super-memory.com"] = aI "SM" "text,sans"
 | u'' "qwantz.com" = aI "DC" "text,sans"
 | u'' "qualiacomputing.com" = aI "QC" "text,sans"
 | u'' "www.thelancet.com" = aI "L" "text"
 | u'' "www.nngroup.com" = aI "NN" "text,sans"
 | u'' "replicationindex.com" = aI "RI" "text,sans"
 | u' ".yahoo.com" = aI "Y!" "text,sans"
 | u' "github.com/huggingface/" || u' "medium.com/huggingface/" || u'' "huggingface.co" = aI "\129303" "text" -- "🤗" HUGGING FACE U+1F917
 | u'' "quantifiedself.com" || u'' "forum.quantifiedself.com" || u' "www.reddit.com/r/QuantifiedSelf/" = aI "QS" "text,sans"
 | u'' "www.pragmatic.ml" = aI "𝕄" "text" -- Madison May, machine learning blog
 | u'' "www.research.va.gov" = aI "VA" "text,sans" -- US Department of Veterans Affair (mostly linked for Million Veteran Project)
 | u'' "apnews.com" = aI "AP" "text,sans"
 | aU' ["www.unz.com/gnxp/", "razib.substack.com", "www.razib.com", "www.razibkhan.com", "www.gnxp.com", "twitter.com/razibkhan"] = aI "RK" "text,sans" -- Razib Khan
 | u'' "www.outsideonline.com" = aI "𝕆" "text,sans" -- imitate the shadowing on Outside Online's 'O' <https://www.outsideonline.com/wp-content/uploads/2021/07/favicon-194x194-1.png>
 | u'' "jaymans.wordpress.com" = aI "J👨🏾" "text,sans" -- JayMan
 | u'' "norvig.com" = aI "N" "text,sans" -- Google Director of Research <https://en.wikipedia.org/wiki/Peter_Norvig>; <https://norvig.com/favicon.ico> is actually instantly recognizable & well-chosen, but unfortunately, only works because of the *colors*... so we'll settle for a plain sans capital N.
 | u'' "novelai.net" || u'' "blog.novelai.net" = aI "🖋" "text" -- LOWER LEFT FOUNTAIN PEN (U+1F58B); NovelAI logo is a fountain pen nib.
 | u'' "www.thebeliever.net" = aI "𝐁" "text,serif" -- _The Believer_ magazine <https://en.wikipedia.org/wiki/The_Believer_(magazine)>, McSweeney's spinoff (formerly <https://believermag.com>): logo is a dropshadow serif capital B logo
 | u'' "solar.lowtechmagazine.com" = aI "☀" "text,sans" -- Low Tech Magazine (U+2600 BLACK SUN WITH RAYS)
 | u'' "www.rollingstone.com" = aI "𝓡𝐒" "text" -- Rolling Stone <https://www.rollingstone.com/wp-content/uploads/2022/08/cropped-Rolling-Stone-Favicon.png> <https://en.wikipedia.org/wiki/File:Rolling_Stone_2022.svg>
 | u'' "www.popsci.com" = aI "PS" "text,sans" -- Popular Science magazine (no usable or recognizable logos)

 -- Tri/triple TLAs
 | u' "animenewsnetwork.com" = aI "ANN" "text,tri"
 | u'' "www.catb.org" || u'' "esr.ibiblio.org" = aI "ESR" "text,tri,sans"
 | u'' "arstechnica.com" = aI "ars" "text,tri,sans" -- Ars is an orange box, not usable
 | u' ".bbc.com" || u' ".bbc.co.uk" = aI "BBC" "text,tri,sans" -- BBC: no usable logo
 | u' ".bmj.com" = aI "bmj" "text,tri,sans" -- British Medical Journal or just ‘bmj’
 | u'' "www.cdc.gov" = aI "CDC" "text,tri"
 | u'' "boardgamegeek.com" = aI "BGG" "text,tri,sans" -- puzzle-piece logo would be unrecognizable as link icon <https://cf.geekdo-static.com/images/logos/navbar-logo-bgg-b2.svg>
 | u'' "thehub7dnl5nmcz5.onion" = aI "Hub" "text,tri,sans"
 | u'' "www.abc.net.au" || u'' "abcnews.go.com" = aI "ABC" "text,tri,sans" -- <https://en.wikipedia.org/wiki/Australian_Broadcasting_Corporation>
 | u'' "www.odt.co.nz" = aI "ODT" "text,tri"
 | u'' "knowyourmeme.com" = aI "KYM" "text,tri"
 | u'' "freakonomics.com" = aI "FRK" "text,tri,sans" -- hybrid apple-orange icon (get it, "comparing apples & oranges") doesn't work as favicon or link
 | u'' "aiimpacts.org" = aI "AII" "text,tri"
 | u'' "scp-wiki.wikidot.com" = aI "SCP" "text,tri,sans"
 | aU'' ["latitude.io", "play.aidungeon.io", "aidungeon.medium.com"] || u' "www.reddit.com/r/AIDungeon"  || u' "www.patreon.com/AIDungeon" = aI "AID" "text,tri,sans"
 | u'' "nap.nationalacademies.org" = aI "NAP" "text,tri"
 | u' ".cnn.com" = aI "CNN" "text,tri,sans"
 | u'' "www.npr.org" || u'' "text.npr.org" = aI "npr" "text,tri,sans" -- NPR styles it in lowercase in their |n|p|r| logo
 | u'' "www.filfre.net" = aI "TDA" "text,tri,sans" -- Filfre.net/The Digital Antiquarian has no logo or usable substitute…
 | u'' "lwn.net" = aI "LWN" "text,tri,sans"
 | u' ".fast.ai" ||  u' "github.com/fastai/" = aI "F.ai" "text,tri"
 | u'' "www.sfgate.com" = aI "SFG" "text,tri,sans"
 | u' ".cbslocal.com" || u'' "www.cbsnews.com" = aI "CBS" "text,tri,sans"
 | u'' "nypost.com" = aI "NYP" "text,tri,sans,italic"
 | u'' "www.justice.gov" = aI "DoJ" "text,tri" -- US federal Department of Justice
 | u'' "www.fanfiction.net" = aI "FFN" "text,tri,sans" -- The FF.net logo is pretty crazy (<https://en.wikipedia.org/wiki/File:Fanfictionnetlogo.jpeg> is the *normal* one!), and I don’t think anyone would recognize it in monochrome. 'FF' as an abbreviation is confusing with FireFox, so expand to "FFN".
 | u'' "myanimelist.net" = aI "MAL" "text,tri,sans" -- MAL: the blue of their logo doesn’t work, so just text
 | aU' ["onegeek.org", "eva-fan.com", "evaotaku.com", "khara.co.jp", "gainax.co.jp", "17th-angel.tumblr.com", "gainax.com", "johakyu.net", "kanzaki.sub.jp", "homepage3.nifty.com", "www.cjas.org", "www.dummy-system.com", "www.evalegend.com", "www.usagi.org", "animekritik.wordpress.com", "fullfrontal.moe", "wavemotioncannon.com", "www.angelfire.com/anime4/"] = aI "NGE" "text,tri" -- Primary user: forum.evageeks.org wiki.evageeks.org
 | u'' "academic.oup.com" || u' ".nutrition.org" || u' ".oxfordjournals.org" = aI "OUP" "text,tri" -- Oxford Academic Journals / OUP
 | u'' "poniesatdawn.bandcamp.com" = aI "P@D" "text,tri"
 | u'' "slatestarscratchpad.tumblr.com" || u'' "www.astralcodexten.com" || (u'' "slatestarcodex.com" && (extension u /= ".pdf")) || (isLocal x u && (u' "yvain" ||  u' "slatestarcodex")) = aI "SSC" "text,tri" -- SSC logo too bad to use; NOTE: we want PDFs merely hosted on SSC to not match, and fall through to get a PDF icon instead
 | u'' "plato.stanford.edu" = aI "SEP" "text,tri"
 | u'' "www.vox.com" = aI "Vox" "text,tri,italic"
 | aU'' ["blogs.wsj.com", "online.wsj.com", "www.wsj.com"] = aI "WSJ" "text,tri" -- The Wall Street Journal
 | u'' "gameprogrammingpatterns.com" = aI "GPP" "text,tri,sans"
 | u'' "www.metopera.org" = aI "Met" "text,tri"
 | u'' "www.schneier.com" = aI "SOS" "text,tri,sans" -- "Bruce Schneier", who writes "Schneier On Security" or "SOS" (Easter egg: the Schneier.com favicon encodes Morse code into its edges, which says… "SOS")
 | u'' "hbr.org" = aI "HBR" "text,tri,sans" -- Harvard Business Review
 | aU'' ["dl.acm.org", "queue.acm.org", "cacm.acm.org"] = aI "acm" "text,tri,sans" -- <https://en.wikipedia.org/wiki/File:Association_for_Computing_Machinery_(ACM)_logo.svg> 'acm' sans in a circle inside a diamond; can't fake it with Unicode joiners (they'd only put one character into a circle+diamond), and I probably don't want to bother with a SVG.
 | u' "www.cs.utexas.edu/~EWD/" = aI "EWD" "text,tri,sans" -- Edsger W. Dijkstra, of course, wrote in sans
 | u'' "iopscience.iop.org" = aI "IOP" "text,tri,sans" -- <https://en.wikipedia.org/wiki/IOP_Publishing> Institute of Physics Publishing
 | u'' "80000hours.org" = aI "80k" "text,tri,sans" -- 80,000 Hours (Centre for Effective Altruism, FHI, Oxford)
 | u'' "researchers.wls.wisc.edu" || u' "www.ssc.wisc.edu/wlsresearch/" = aI "WLS" "text,tri,sans" -- Wisconsin Longitudinal Study
 | u' "host.robots.ox.ac.uk/pascal/VOC" = aI "VOC" "text,tri,sans" -- PASCAL VOC (Visual Object Classes) machine learning image dataset/competition
 | u'' "www.tandfonline.com" = aI "T&F" "text,tri,sans" -- Taylor & Francis: their icon is a small white oil lamp on a blue background, but it's illegible as a favicon and just looks like a white blob on a blue square; since these need to be monochrome, that makes it useless. Plus I recognize 'Taylor & Francis' (sans serif, as usual for STEM publishers) more anyway, so 'T&F' is the natural tri-text icon. A possible Unicode alternative for the AMPERSAND if it is too big is 'U+FE60 ﹠ SMALL AMPERSAND'.
 | u' "omega0.xyz/omega8008/" || aU' ["/doc/statistics/bayes/1988-jaynes-maximumentropyandbayesianmethods.pdf", "www-biba.inrialpes.fr/Jaynes/cc18i.pdf"] = aI "ETJ" "text,tri,sans" -- E. T. Jaynes book/paper website
 | u' "paperswithcode.com" = aI "PwC" "text,tri,sans" -- 'Papers With Code' does have a weird '[|⏐|⏐|]' icon (supposed to be a bar graph of different performances, I guess) which would work monochrome, but I don't recognize it and I doubt anyone else would either, especially as a link icon, but 'PwC' *might* be recognizable, so we'll go with that for now.
 | u'' "www.pewresearch.org" = aI "Pew" "text,tri" -- Pew Research Center: logo <https://en.wikipedia.org/wiki/File:Pew_Research_Center.svg>. While very cool, and worthy of a Scandinavian black death metal band, it is unrecognizable and would 'shimmer' intensely if scaled down to a link icon & would have to be recreated. So, another text icon it is. Everyone knows what "Pew" means.
 | u'' "thelastpsychiatrist.com" = aI "TLP" "text,tri,sans"
 | u'' "www.lrb.co.uk" = aI "LRB" "text,tri" -- London Review of Books <https://en.wikipedia.org/wiki/London_Review_of_Books>
 | u'' "archiveofourown.org" = aI "Ao3" "text,tri" -- Archive of Our Own <https://archiveofourown.org/> <https://en.wikipedia.org/wiki/Archive_of_Our_Own>
 | u'' "www.nlsinfo.org" = aI "NLS" "text,tri,sans" -- the National Longitudinal Surveys (BLS), eg. NLSY79 <https://en.wikipedia.org/wiki/National_Longitudinal_Surveys>

 -- Quad-letter-square icons.
 | aU'' ["jamanetwork.com", "jama.jamanetwork.com", "archinte.jamanetwork.com"]  = aI "JAMA" "text,sans,quad" -- The Journal of the American Medical Association (JAMA)
 | u'' "www.cell.com" = aI "CELL" "text,quad,sans" -- Cell: their logo is unrecognizable (and dumb)
 | u'' "mlp.fandom.com" = aI "MLPW" "text,quad,sans" -- NOTE: override Fandom catch-all
 | u'' "www.fimfiction.net" = aI "FIMF" "text,quad,mono"
 | u'' "www.nber.org" && (extension u /= ".pdf") || u'' "ideas.repec.org" = aI "NBER" "text,quad" -- IDEAS/RePEc doesn't seem to actually be run by or affiliated with NBER, but it's so close topically that I think readers can forgive it.
 | u'' "www.pnas.org" = aI "PNAS" "text,quad" -- PNAS: they don’t have a real logo, but their favicon does a nice little compact square (white text on blue background), and we can replicate that in CSS (but just as black text on white background, per our monochrome theme) [On second thought, all of the icons using background squares, like HN/YC, are very intense and hard to visually balance. It's probably better to leave PNAS as just a quad-letter.]
 | u'' "www.rand.org" = aI "RAND" "text,quad,sans"
 | u' ".sagepub.com" = aI "SAGE" "text,quad,sans" -- Sage Journals’s logo is a circled S… but would anyone recognize it? Primary user: journals.sagepub.com
 | u'' "publicdomainreview.org" = aI "TPDR" "text,quad"
 | u' "xkcd.com" = aI "XKCD" "text,quad,sans" -- covers explainxkcd.com, what-if.xkcd.com…
 | u'' "www.imdb.com" = aI "IMDb" "text,sans,quad"
 | u'' "www.nejm.org" = aI "NEJM" "text,quad"
 | u'' "spectrum.ieee.org" || u'' "ieeexplore.ieee.org" = aI "IEEE" "text,mono,quad"
 | u'' "rjlipton.wordpress.com" = aI "P = NP" "text,quad" -- NOTE: not 4 letters because we need the spacing for a more reasonable look. 'FULLWIDTH EQUALs SIGN' turns out to be *too* big and stack up three high. using 2 HAIR SPACE will separate the '=' slightly from the 'P' while not causing the 3-layer layout.
 | u' "mitpress.mit.edu/sites/default/files/sicp/" = aI "SI CP" "text,quad,sans"
 | u' "mitpress.mit.edu/" = aI "MIT" "text,tri,mono" -- if it's not _SICP_, fall back.
 | u'' "jaspervdj.be" = aI "JVDJ" "text,quad,mono"
 | u'' "gizmodo.com" = aI "GIZM" "text,quad,mono"
 | u'' "www.mdpi.com" = aI "MDPI" "text,quad,sans" -- <https://en.wikipedia.org/wiki/MDPI> chemical subscript+superscript probably not recognized by anyone & too bulky even as SVG NOTE: doesn't wrap right with serif, so has to be ans
 | u'' "mattlakeman.org" = aI "MATT" "text,quad,sans"
 | u'' "www.poetryfoundation.org" = aI "POET" "text,quad,sans" -- <https://www.poetryfoundation.org/> <https://en.wikipedia.org/wiki/Poetry_Foundation> logo is a 2×3 grid "POETRY"; fortunately, 'POET' is a real word and works nicely as a quad
 | u'' "papers.ssrn.com" = aI "SSRN" "text,quad"
 | u'' "www.vice.com" || u'' "motherboard.vice.com" = aI "VICE" "text,quad,italic"
 | aU'' ["www.courtlistener.com", "archive.recapthelaw.org", "storage.courtlistener.com", "www.courtlistener.com", "www.pacer.uscourts.gov", "www.pacer.gov", "pcl.uscourts.gov"] = aI "PACR" "text,quad"
 | u'' "www.nybooks.com" = aI "NYRB" "text,quad"
 | u'' "www.jstor.org" = aI "JTOR" "text,quad" -- quad looks better skipping the thin 'S'
 | u'' "thisanimedoesnotexist.ai" = aI "TADE" "text,quad,sans"
 | u'' "www.thisfursonadoesnotexist.com" = aI "TFDE" "text,quad,sans"
 | u'' "www.thiswaifudoesnotexist.net" = aI "TWDE" "text,quad,sans"
 | u'' "thisponydoesnotexist.net" = aI  "TPDE" "text,quad,sans"
 | u'' "pcdb.santafe.edu" = aI "PCDB" "text,quad,sans"
 | u'' "vndb.org" = aI "VNDB" "text,quad,sans"
 | u'' "www.huffpost.com" = aI "HUFF" "text,quad,sans"
 | u'' "longreads.com" = aI "Long" "text,quad"
 | u'' "warontherocks.com" = aI "WOTR" "text,quad,sans"
 | u'' "krebsonsecurity.com" = aI "Kreb" "text,quad,sans" -- KrebsOnSecurity: 'KOS' unrecognizable, favicon a baffling mystery, Brian Krebs is generally known as 'Krebs', so abbreviate that
 | u'' "www.nextplatform.com" = aI "NEXT" "text,quad,sans" -- The Next Platform's double-cube logo *could* work as an SVG but not convinced it'd be recognizable
 | u'' "www.vetta.org" = aI "Legg" "text,quad,sans" -- Shane Legg (DeepMind)
 | u'' "www.spiegel.de" = aI "SPGL" "text,quad" -- Der Spiegel, major German newspaper; the 'S' logo is unrecognizable given the sheer number of 'S' logos out there, so abbreviation instead
 | u'' "tasvideos.org" = aI "TASV" "text,quad" -- TASVideos.org: tool-assisted game movies
 | u'' "habr.com" = aI "Habr" "text,quad,sans" -- Russian tech collaborative blog <https://en.wikipedia.org/wiki/Habr>
 | u'' "www.teds.ac.uk" = aI "TEDS" "text,quad,sans" -- UK twin registry founded by Robert Plomin, heavily used in behavioral genetics & sociology/psychology; it has a clever little logo (https://www.teds.ac.uk/Content/Images/TEDSlogo.png) & a monochrome SVG version would work… but unfortunately no one ever uses it & it is always known as "Twins Early Development Study (TEDS)"
 | u'' "stability.ai" || u' "#stability" || u' "&org=stability" = aI "SD" "text,sans"
 | u'' "patrickcollison.com" = aI "PC" "text,sans"
 | u'' "oeis.org" = aI "OEIS" "text,quad,sans" -- On-Line Encyclopedia of Integer Sequences
 | u'' "bldgblog.com" = aI "BLDG" "text,quad,monospace" -- BLDGBLOG (“building blog”, 2004), by Geoff Manaugh <https://en.wikipedia.org/wiki/BLDGBLOG>
 | u' "twitter.com/patio11" || aU'' ["www.bitsaboutmoney.com", "training.kalzumeus.com", "www.kalzumeus.com"] = aI "pt11" "text,quad,monospace" -- patio11 / Patrick McKenzie / Bingo Card Creator / Bits About Money / Stripe. The 'dragon' icon for Kalzumeus.com would be illegible & probably not recognizable at this point even by long-time readers, but a stripped down 'pt11' should look enough like 'patio11'...
 | u'' "mathshistory.st-andrews.ac.uk" = aI "M  T" "text,quad,sans" -- MacTutor History of Mathematics Archive: a weird one, <https://mathshistory.st-andrews.ac.uk/static/img/logo.png> - crude sans but only 2 letters kinda like a diagonal in a square or a TeX. Experiment with using EN SPACE to force a diagonal quad layout.
 | u'' "scale.com" = aI "SCLE" "text,quad,monospace" -- Scale, a large data-labeling company heavily used behind-the-scenes by FANG & OpenAI etc for outsourcing evaluating text, labeling images, and so on.
 | u'' "nunosempere.com" = aI "nuno" "text,quad,sans" -- Nuño Sempere
 | u'' "ourworldindata.org" = aI "OWI  D" "text,quad,sans" -- Our World In Data (OWID) <https://en.wikipedia.org/wiki/Our_World_in_Data>; NOTE: uses THIN SPACE+THIN SPACE because the 'W' is so wide

 -- SVG icons (remember the link-icon name is substituted in as part of the URL to the SVG icon)
 | aU'' ["www.amazon.com", "aws.amazon.com", "amazon.com", "smile.amazon.com", "aboutamazon.com"] || u' "amazon.co." = aI "amazon" "svg"
 | u'' "en.bitcoin.it" || u'' "bitcointalk.org" || u'' "www.blockchain.com" = aI "bitcoin" "svg"
 | u'' "www.biorxiv.org" || u'' "www.medrxiv.org" = aI "chi-dna" "svg" -- BioRxiv (custom icon: italic Chi with DNA cross-strands).
 | u'' "distill.pub" = aI "distillpub" "svg" -- Distill ML journal.
 | u'' "www.dropbox.com" || u'' "dl.dropboxusercontent.com" = aI "dropbox" "svg" -- Dropbox: old file-host, deprecated since they’ve started killing inactive accounts & their UI become awful. primary user: dl.dropboxusercontent.com
 | u'' "www.erowid.org" || u'' "www.drugsdata.org" = aI "erowid" "svg"
 | aU' [".tensorflow.org", "github.com/tensorflow/", "medium.com/tensorflow/"] = aI "tensorflow" "svg" -- <https://simpleicons.org/?q=tensorflow>; NOTE: hosted on Github, so override Github
 | aU'' ["github.com", "copilot.github.com", "archiveprogram.github.com", "gist.github.com", "github.blog", "compvis.github.io"] = aI "github" "svg" -- Github; I exclude *.github.io & raw.githubusercontent.com because that’s blogs/papers.
 | u'' "www.paulgraham.com" = aI "pg" "text,monospace" -- Paul Graham, known by username 'pg' on HN
 | u' "ycombinator.com" = aI "hn" "svg" -- HN/YC (shared logo). primary user: news.ycombinator.com
 | aU' ["webcitation.org", "mementoweb.org", "archive.org", "archive-it.org", "wiki.archiveteam.org", "waybackmachine.org", "archive.is", "archive.md", "archive.ph", "archive.today", "babel.hathitrust.org"] = aI "internetarchive" "svg" -- HathiTrust <https://en.wikipedia.org/wiki/HathiTrust> is confusingly nebulous but its cute elephant logo is unrecognizable and I regard it as basically a wrapper around Google Books+Internet Archive, so I think it's less confusing to put it under the IA logo.
 | u'' "mega.nz" = aI "mega" "svg" -- MegaUpload/Mega: filesharing (used for big files).
 | u'' "intelligence.org" = aI "miri" "svg" -- MIRI/intelligence.org.
 | u' ".nytimes.com" = aI "newyorktimes" "svg" -- The New York Times: manual edit, reducing full 'NEW YORK TIMES' SVG logo to just the ‘T’ they use as an icon.
 | aU'' ["www.ncbi.nlm.nih.gov", "pubmed.ncbi.nlm.nih.gov"] = aI "nlm-ncbi" "svg" -- NCBI/Pubmed: simplification of their logo (https://upload.wikimedia.org/wikipedia/commons/0/07/US-NLM-NCBI-Logo.svg). primary user: ncbi.nlm.nih.gov
 | u'' "www.patreon.com" = aI "patreon" "svg" -- Patreon. (Used the old one (https://upload.wikimedia.org/wikipedia/commons/9/94/Patreon_logo.svg) because I don’t like the new one.)
 | aU' ["plos.org", "plosone.org", "plosmedicine.org"] = aI "plos" "svg" -- PLOS ONE in all their domain permutations… primary user: journals.plos.org
 | u' "reddit.com" = aI "reddit" "svg" -- www.reddit.com
 | aU' ["overflow.net", "overflow.com", "stackexchange.com"] = aI "stackexchange" "svg" -- The *Exchange/*Overflow family of websites.
 | u' "substack.com" = aI "substack" "svg" -- gwern.substack.com
 | u'' "www.theguardian.com" || u'' "www.guardian.co.uk" = aI "theguardian" "svg" -- El Grauniad.
 | u'' "www.newyorker.com" = aI "thenewyorker" "svg" -- The New Yorker: the Dandy SVG, simplified & rotated more vertically.
 | u' "tumblr.com" = aI "tumblr" "svg"
 | aU'' ["twitter.com", "blog.twitter.com", "developer.twitter.com"] = aI "twitter" "svg"
 | u'' "www.uptontea.com" = aI "uptontea" "svg"
 | u'' "soundcloud.com" = aI "audio" "svg"
 | u' ".bandcamp.com" = aI "audio" "svg"
 | u'' "www.washingtonpost.com" = aI "washingtonpost" "svg" -- The Washington Post: truncated their blackletter to ‘WP’.
 | aU' ["wikipedia.org", "wikimedia.org", "wiktionary.org", "wikisource.org", "wikimediafoundation.org", "stats.grok.se", "wikibooks.org", "wikiquote.org", "xtools.wmflabs.org", "www.mediawiki.org"] = aI "wikipedia" "svg" -- primary user: en.wikipedia.org, meta.wikimedia.org, en.wiktionary.org, en.wikisource.org
 | u' ".fandom.com" = aI "♡" "text" -- formerly known as Wikia, renamed to 'Fandom' and adopted a heart-based logo: <https://en.wikipedia.org/wiki/Fandom_(website)#2016%E2%80%932018:_Fandom_brand>; this is an umbrella covering all the subdomains; more specific Fandom wikis go before in the list (like MLP)
 | u' "www.wired.com" || u' "www.wired.co.uk" = aI "wired" "svg" -- an inverse "W" on a black background (Wiley is just a "W")
 | u'' "www.youtube.com" = aI "youtube" "svg"
 | aU'' ["vimeo.com", "player.vimeo.com"] = aI "file-video" "svg"
 | u'' "www.telegraph.co.uk" = aI "the-telegraph" "svg" -- edited from <https://en.wikipedia.org/wiki/File:The_Telegraph.svg>
 | u'' "www.openphilanthropy.org" = aI "open-philanthropy" "svg"
 | u'' "www.atlasobscura.com" = aI "atlas-obscura" "svg"
 | aU'' ["blog.eleuther.ai", "www.eleuther.ai", "pile.eleuther.ai", "6b.eleuther.ai"] || u' "arankomatsuzaki.wordpress.com/2021/06/04/gpt-j/" = aI "eleutherai" "svg"
 | u'' "arankomatsuzaki.wordpress.com" = aI "ak" "text,sans" -- known with the other ak on Twitter; put after EAI in the SVG section because the GPT-J announcement is an EAI project
 | u' ".apple.com" = aI "apple" "svg"
 | u'' "www.quantamagazine.org" = aI "quanta" "svg"
 | u'' "creativecommons.org" = aI "creative-commons" "svg" -- <https://creativecommons.org/about/downloads>
 | u'' "www.alcor.org" = aI "alcor" "svg"
 | aU'' ["www.econlib.org", "www.econtalk.org", "betonit.blog"] = aI "econlib" "svg" -- EconLib/EconLog/EconTalk torch icon <https://3ijp5i2qkzo4hq4yrxfteqh-wpengine.netdna-ssl.com/wp-content/themes/econlib/assets/icons/torch-icon.svg>
 | u' ".cochrane.org" || aU'' ["www.cochrane.org.uk", "www.cochranelibrary.com"] = aI "cochrane-collaboration" "svg" -- <https://upload.wikimedia.org/wikipedia/commons/archive/a/a1/20200122144949%21Cclogo.svg> (Newer version is not actually an SVG; reported on Talk page)
 | u'' "www.connectedpapers.com" = aI "connected-papers" "svg"
 | u' "nasa.gov" = aI "nasa" "svg" -- NASA has way too many subdomains to try to whitelist them individually. SVG is a quad version of <https://commons.wikimedia.org/wiki/File:NASA_Worm_logo_(black).svg>
 | aU'' ["link.springer.com", "rd.springer.com"] || u' ".biomedcentral.com" = aI "springerlink" "svg"  -- (♘) WHITE CHESS KNIGHT as SVG
 | u'' "www.metaculus.com" = aI "metaculus" "svg"
 | u'' "wandb.ai" = aI "wandb" "svg" -- Weights & Biases/WandB: blog/podcasts, writeups etc; complicated 4-dot grid logo intended to evoke NN layers with large/small weights, <view-source:https://assets.website-files.com/5ac6b7f2924c656f2b13a88c/6066c22135b8983b61ad7939_weights-and-biases-logo.svg>; edited into BW, enlarged the large dots to make viewable as a link icon
 | aU'' ["libgen.rs", "libgen.org", "library.bz"] = aI "raven" "svg" -- Libgen/Sci-Hub raven+key icon <https://en.wikipedia.org/wiki/File:Scihub_raven.png>, while pretty, is too detailed for a link-icon so fall back to just the raven. There are many LG+SH domains, but these are the only ones we link.
 | u'' "www.hoover.org" = aI "hoover-institution" "svg" -- <https://en.wikipedia.org/wiki/Hoover_Institution_Library_and_Archives> <https://en.wikipedia.org/wiki/Hoover_Tower> <https://en.wikipedia.org/wiki/New_Cathedral_of_Salamanca>
 | u'' "www.statnews.com" = aI "stat-news" "svg" -- STAT News <https://en.wikipedia.org/wiki/Stat_(website)> based on <https://www.statnews.com/wp-content/themes/stat/images/stat-logo.svg>; using Unicode '𝐴' to replicate the 'A' in 'STAT' is probably unreliable cross-platform so we transform the 'STAT' logotype into a quad SVG icon instead.
 | aU'' ["thepiratebay.org", "rss.thepiratebay.se", "thepiratebay.se",  "thepiratebay.sx"] = aI "the-pirate-bay" "svg" -- in theory, you can get a skull & crossbones by Unicode Emoji: BLACK FLAG + SKULL AND CROSSBONES + ZWJ = PIRATE FLAG <https://emojipedia.org/pirate-flag/> (and if that doesn't work try adding U+FE0F to the end). This turns out to be too unreliable across systems (fonts? OSes? browser versions?) that we replaced it with a proper SVG version of The Pirate Bay's cassette-tape (yes, really) + cross-bones.
 | u'' "retractionwatch.com" = aI "magnifying-glass" "svg" -- Retraction Watch <https://en.wikipedia.org/wiki/Retraction_Watch> LEFT-POINTING HOUR GLASS
 | u'' "www.yudkowsky.net" = aI "yud" "svg" -- but of course: י HEBREW LETTER YUD 0x05D9; we use an SVG icon here for the same reason we use a math alef elsewhere instead of the Hebrew one (the RTL of Hebrew script will screw up some browsers, like Mac Firefox)
 | u'' "nautil.us" = aI "nautilus" "svg" -- modeled after 🐚 SPIRAL SHELL (U+1F41A), but turned into monochrome SVG (this icon is usually rendered in color & differently across platforms, so we ship another SVG)
 | u'' "www.scholarpedia.org" = aI "scholarpedia" "svg" -- Scholarpedia <https://en.wikipedia.org/wiki/Scholarpedia>; Adobe trace of their PNG favicon
 | u'' "radiolab.org" = aI "audio-waveform-lines" "svg" -- Radiolab WNYC/NPR <https://en.wikipedia.org/wiki/Radiolab>; <https://fontawesome.com/icons/waveform-lines>/<https://www.svgrepo.com/svg/342965/audio-wave>
 | u'' "maggieappleton.com" = aI "maggie-appleton" "svg"  -- <https://twitter.com/Mappletons> Maggie Appleton, designer (Elicit/Ought), blogger about hypermedia/personal wikis/PKM

 -- many orgs will use a medium subdomain, so we fall back here for Medium and override above:
 | u'' "medium.com" || u'' "towardsdatascience.com" = aI "𝐌" "text" -- Medium: cheaper to abuse Unicode (𝐌) MATHEMATICAL BOLD CAPITAL M

 -- Filetypes: (we need to parse & extract the extension because many would be too short and match too many URLs if mere infix matching was used)
 | iE ["tar", "zip", "xz", "img", "bin", "pkl", "onnx", "pt", "maff"] = aI "archive" "svg"
 | iE ["opml", "txt", "xml", "json", "jsonl", "page"] || u'' "pastebin.com" = aI "txt" "svg"
 | iE ["css", "hs", "js", "conf", "sh", "r", "R", "patch", "diff"] = aI "code" "svg"
 | iE ["doc", "docx"] = aI "worddoc" "svg"
 | iE ["xls", "xlsx", "ods"] = aI "spreadsheet" "svg"
 | iE ["csv"] = aI "csv" "svg"
 | iE ["gif", "bmp", "ico", "jpg", "jpeg", "png", "svg", "xcf", "psd"] = aI "image" "svg"
 | iE ["mp3", "flac", "ogg"] = aI "audio" "svg"
 | iE ["swf", "mp4", "mkv", "webm"] = aI "file-video" "svg"
 | iE ["ebt", "mdb", "mht", "ttf"] = aI "misc" "svg"
 | iE ["epub"] = aI "EPUB" "text,quad,sans"
 | u'' "imgur.com" || u'' "i.imgur.com"       = aI "image" "svg"
 | "/static/" `T.isPrefixOf` u && hasExtension ".html" u  = aI "code" "svg"
 | isLocal x u && hasExtension ".php" u                     = aI "code" "svg"
 | aU' [".pdf", ".PDF", "/pdf", "type=pdf", "pdfs.semanticscholar.org", "citeseerx.ist.psu.edu", "pdfs.semanticscholar.org"] = aI "pdf" "svg"

 -- Fallback
 | otherwise = x
 where u', u'' :: T.Text -> Bool
       -- simplest check for string anywhere; note that if it is a full domain name like `https://foo.com` (intended to match `https://foo.com/xyz.html`), then it will *not* match when the local-archive code fires and the URL gets rewritten to "/doc/foo.com/$HASH.html". So we error out if the user tries this, having forgotten that u' ≠ u'' in that respect.
       u' v = if "http://" `T.isPrefixOf` v || "https://" `T.isPrefixOf` v then error ("LinkIcon.hs: Overly strict prefix in infix matching (u'): " ++ show u ++ ":" ++ show v) else
         v `T.isInfixOf` u
       -- more stringent check, matching exactly the domain name:
       u'' v = if "http://" `T.isPrefixOf` v || "https://" `T.isPrefixOf` v then error ("LinkIcon.hs: Overly strict prefix in infix matching (u''): " ++ show u ++ ":" ++ show v) else
                 isHostOrArchive v u
       aI :: T.Text -> T.Text -> Inline
       aI = addIcon x
       iE :: [T.Text] -> Bool
       iE = elem (T.drop 1 $ extension u)
       aU', aU'' :: [T.Text] -> Bool
       aU'  = any u'
       aU'' = any u''
linkIcon x = x

hasIcon :: Inline -> Bool
hasIcon (Link (_,_,ks) _ (_,_)) =
  case lookup "link-icon" ks of
    Just _ -> True
    Nothing -> case lookup "link-icon-type" ks of
                 Just _ -> True
                 Nothing -> False
hasIcon _ = True

hasIconURL :: T.Text -> Bool
hasIconURL = hasIcon . getIcon

getIcon :: T.Text -> Inline
getIcon u = linkIcon $ Link nullAttr [] (u,"")

addIcon :: Inline -> T.Text -> T.Text -> Inline
addIcon x@(Link (idt,cl,ks) a (b,c)) icon iconType  =
  if hasIcon x then x else Link (idt,cl,
                                  [("link-icon",icon), ("link-icon-type",iconType)] ++
                                  ks) a (b,c)
addIcon x _ _ = x

isLocal :: Inline -> T.Text -> Bool
isLocal x "" = error $ "Invalid empty string used as link in isLocal: " ++ show x
isLocal _ s = T.head s == '/'

hasExtension :: T.Text -> T.Text -> Bool
hasExtension ext p = extension p == ext

extension :: T.Text -> T.Text
extension = T.pack . maybe "" (takeExtension . uriPath) . parseURIReference . T.unpack

isHostOrArchive :: T.Text -> T.Text -> Bool
isHostOrArchive domain url = let h = host url in
                                h == domain || ("/doc/www/"`T.append`domain) `T.isPrefixOf` url

-- to find URLs worth defining new link icons for, pass through a list of URLs (perhaps extracted
-- from the backlinks database) and return domains with at least `linkIconMin` matches. (Link icons
-- are enough work that below a certain level of prevalence, they are not worthwhile even if completely
-- transparent/self-explanatory.)
--
-- The original raw results are particularly useful when piped into <https://gwern.net/haskell/lcp.hs> to
-- get suggested prefixes/domains worth adding link-icons for, or one can just look at the domains by `host`:
linkIconPrioritize :: IO [(Int,T.Text)]
linkIconPrioritize = do b <- LinkBacklink.readBacklinksDB
                        let b' = M.toList $ M.map length b
                        let b'' = map (\(y,z) -> (host y,z)) $ filter (\(url,_) ->  host url `notElem` C.prioritizeLinkIconBlackList &&
                                                                                    not (hasIconURL url) &&
                                                                                    ("." `T.isInfixOf` url)) b'
                        let b''' =  M.fromListWith (+) b''
                        return $ reverse $ sort $ filter (\(e,f) -> e >= C.prioritizeLinkIconMin && f /="") $ map (\(c,d) -> (d,c)) $ M.toList b'''

-- Test suite:
--
-- Test the /lorem#link-icons test cases as unit-tests of `linkIcon`: it should, for every URL
-- unit-test, generate the specified link-icon/link-icon-type. Return the list of mismatches for
-- fixing.
-- Here we test that URLs get assigned the appropriate icons; on /lorem, we render them to check for
-- CSS/visual glitches. Any new test-cases should be added to both (with different URLs where possible).
linkIconTest :: [(T.Text,T.Text,T.Text)]
linkIconTest = filter (\(url, li, lit) -> linkIcon (Link nullAttr [] (url,""))
                                          /=
                                          Link ("",[], [("link-icon",li), ("link-icon-type", lit)]) [] (url,"")
                                                   )
               C.linkIconTestUnitsText
