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
 | hasKeyAL u C.overrideLinkIcons = let i = fromJust $ lookup u C.overrideLinkIcons in addIcon x i
 | anyPrefixT u ["/metadata/annotation/"] = x

 | "directory-indexes-upwards"   `elem` cl = addIcon x ("arrow-up-left", "svg")
 | "directory-indexes-downwards" `elem` cl = addIcon x ("arrow-down-right", "svg")
 | "directory-indexes-sideways"  `elem` cl = addIcon x ("arrow-right", "svg")

 | otherwise = addIcon x $ linkIconRules u
linkIcon x = x

linkIconRules :: T.Text -> (T.Text, T.Text)
linkIconRules u
 -- organizational mentions or affiliations take precedence over domain or filetypes; typically matches anywhere in the URL.
 | u' u "deepmind"  = ("deepmind", "svg") -- DeepMind; match articles or anchors about DM too. Primary user: deepmind.com, DM papers on Arxiv
 | u' u "schmidhuber" || u' u "people.idsia.ch/~juergen/" = ("SMDH", "text,quad,sans") -- Jürgen Schmidhuber homepage & tagged links; should override Arxiv, Bloomberg, NYT, Facebook etc
 | u' u "facebook" || u' u ".fb.com"  = ("facebook", "svg")
 | u' u "sites.google.com/berkeley.edu" || aU'' u ["ml.berkeley.edu", "people.eecs.berkeley.edu", "bair.berkeley.edu"] = ("BAIR", "text,quad,mono") -- Berkeley AI Research: Chelsea Finn, Sergey Levine, robotics, Decision Transformer, etc. Overrides Google Sites.
 | u' u "pandoc" && not (u' u "tufte") = ("PNDC", "text,quad,sans") -- general catch-all, overrides Google Groups (`pandoc-discuss`), with a further override for Tufte-related stuff (which get 'ET')
 | u'' u "groups.google.com" = ("✉", "text")
 | u'' u "scholar.google.com" = ("google-scholar", "svg") -- Google Scholar.
 | u'' u "docs.google.com" = ("worddoc", "svg")
 | u' u "google" || u'' u "magenta.tensorflow.org" = ("alphabet", "svg") -- Google searches, other tools. Note that there are many Google subdomains, which we may wish to iconify differently, so we narrow down with just ‘www’. Google Brain doesn’t have any consistent or recognizable logo, don’t bother trying to replicate one of the dots (no one will recognize it); use ‘GB’ would not be a bad idea, but I suspect that would also confuse people. So reusing the ‘G’ is the least bad option. [the SVG has been renamed 'alphabet' instead of the expected 'google' because two default uBlock lists block the regexp 'icons/google.*' as it is usually abused for social-media spamming icons]
 | aU' u ["twitter.com/sigfpe/", "blog.sigfpe.com", "github.com/dpiponi"] = ("sgfp", "text,quad,monospace") -- sigfpe/Dan Piponi: Haskell, math, computer graphics etc
 | u' u "nvidia"  || aU'' u ["nvlabs.github.io", "nv-adlr.github.io", "nv-tlabs.github.io"] = ("n", "text,sans,italic") -- Nvidia: <https://en.wikipedia.org/wiki/Nvidia#cite_note-2> yeah no. Disambiguate from Nature's "n" by italicizing (Nvidia *did* italicize the lowercase 'n' for a long time, so seems reasonable)
 | u' u "openai" || u'' u "gptprompts.wikidot.com" = ("openai", "svg") -- OpenAI; match articles or anchors about OA too. primary user: openai.com, Arxiv papers. Brockman's GPT-prompts wiki is semi-official IMO.
 | u' u "microsoft" = ("MS", "text,sans,italic") -- Microsoft: I don’t think <https://en.wikipedia.org/wiki/File:Microsoft_logo_(2012).svg> is all that recognizable, so make a logotype more like <https://en.wikipedia.org/wiki/File:Microsoft_logo_(1987).svg>: an italic sans "MS".
 | u' u "#anthropic" || u' u "twitter.com/jackclarkSF/" || aU'' u ["transformer-circuits.pub", "www.anthropic.com", "jack-clark.net"] = ("anthropic", "svg") -- need to override Arxiv; handle Jack Clark (co-founder) newsletter & social media
 | u' u "#laion"  || u' u "LAION-AI" || u'' u "laion.ai" = ("laion", "svg") -- <https://laion.ai/favicon.svg>; need to override Arxiv & Github & Hugging Face

 -- Domains:
 | aU'' u ["psyarxiv.com", "files.osf.io", "osf.io"] = ("ψ", "text") -- Unicode trickery icons: GREEK SMALL LETTER PSI
 | u'' u "unsongbook.com" = ("ℵ", "text") -- SSC’s book: (ℵ) ALEF SYMBOL (We use the math symbol instead of the Hebrew deliberately, to avoid triggering bizarre Hebrew bidirectional text-related layout bugs on Mac Firefox.)
 | u'' u "andrewgelman.com" || u'' u "statmodeling.stat.columbia.edu" = ("▅▇▃", "text") -- Favicon is a little normal distribution/histogram (▅▇▃) LOWER FIVE EIGHTHS BLOCK, LOWER SEVEN EIGHTHS BLOCK, LOWER THREE EIGHTHS BLOCK
 | u'' u "meltingasphalt.com" = ("▲", "text") -- Kevin Simler’s Melting Asphalt blog uses 3 triangles but that's too many, so we just use one. (▲) BLACK UP-POINTING TRIANGLE
 | u'' u "www.tinyletter.com" = ("✉", "text") -- TinyLetter’s icon, without color, isn’t memorable enough; throw in the other email services (✉) ENVELOPE
 | aU'' u ["blog.givewell.org", "www.givewell.org", "files.givewell.org"] || u' u "groups.yahoo.com/group/givewell/" = ("GW", "text")
 | u'' u "groups.yahoo.com" = ("✉", "text")
 | u'' u "www.mail-archive.com" = ("✉", "text")
 | u' u "carryiton.net/chain-letter/" = ("✉", "text") -- linked only for the archive, so this is an appropriate icon
 | u'' u "marginalrevolution.com" = ("M𝐑", "text") -- MR: cheaper to abuse Unicode (𝐑) MATHEMATICAL BOLD CAPITAL R
 | u'' u "www.frontiersin.org" = ("FS", "text,sans") -- <https://en.wikipedia.org/wiki/Frontiers_Media> multiple-cubes logo too busy for an icon, no Unicode equivalent
 | aU'' u ["www.gutenberg.org", "gutenberg.ca", "gutenberg.net.au", "www.fadedpage.com"] = ("PG", "text") -- Faded Pages isn't strictly-speaking a Project Gutenberg org, but they work with Distributed Proofreaders & their work is in PG Canada and they do similar things so meh.
 | u'' u "guzey.com" = ("A.G.", "text,sans")
 | u'' u "www.forbes.com" = ("F", "text")
 | (u' u "haskell.org" && (extension u /= ".hs")) || u' u "haskellers.com" = ("𝛌", "text") -- Haskell: simplify logo; the double-lambda is too busy when used for link icons (𝛌) MATHEMATICAL BOLD SMALL LAMBDA primary user: hackage.haskell.org; we make an exception for .hs files hosted on Haskell.org, like config files, where the source code-ness is more relevant than the organization/domain
 | u'' u "arxiv.org" || u'' u "ar5iv.labs.arxiv.org" || u'' u "proceedings.mlr.press" = ("𝛘", "text") --  ArXiv: Their skull+smiley logo is too bizarre & off-putting to use, in addition to not working as a tiny monochrome image (𝛘) MATHEMATICAL BOLD SMALL CHI (bold makes it show up better when tiny); I lump in 'PMLR' ("Proceedings of Machine Learning Research") because many PMLR were just Arxiv preprints beforehand & it amounts to about the same thing, really.
 | u' u ".bloomberg.com" || u'' u "www.businessweek.com" = ("𝐁", "text") -- Bloomberg: no usable logo, just an inset-B (𝐁) MATHEMATICAL BOLD CAPITAL B
 | u' u "theatlantic.com" = ("A", "text,italic") -- The Atlantic: replicate sloping by italics
 | u' u "alignmentforum.org" || (u'' u "www.greaterwrong.com" && u' u "view=alignment-forum") = ("AF", "text,sans")
 | u'' u "boingboing.net" = ("bb", "text,mono")
 | u'' u "nymag.com" = ("𝒩𝒴", "text")
 | u'' u "thebrowser.com" = ("TB", "text")
 | u'' u "crookedtimber.org" = ("CT", "text")
 | u' u ".latimes.com" = ("𝔏A", "text")
 | u'' u "www.dailymail.co.uk" = ("𝔐", "text") -- 𝔐 MATHEMATICAL FRAKTUR CAPITAL M
 | aU'' u ["danbooru.donmai.us", "derpibooru.org", "safebooru.org"] = ("❐", "text") -- ❐ U+2750 UPPER RIGHT DROP-SHADOWED WHITE SQUARE
 | u'' u "www.edge.org" = ("E", "text,italic")
 | u'' u "www.economist.com" = ("E", "text,sans") -- Economist: logo is just ‘Economist’… There is a sibling magazine <https://en.wikipedia.org/wiki/1843_(magazine)> which I don't seem to link to.
 | u'' u "everything2.com" = ("E2", "text")
 | u'' u "examine.com" = ("Eχ", "text,sans")
 | u'' u "www.sciencedirect.com" = ("E", "text") -- Elsevier/Sciencedirect.com: also an ‘E’
 | u'' u "www.esquire.com" = ("ℰ", "text")
 | aU'' u ["wiki.evageeks.org","forum.evageeks.org","www.evamonkey.com"] || u' u "twitter.com/EvaMonkey/" = ("EG", "text") -- Evangelion: we’ll split this into EGF-related and other NGE sites
 | aU'' u ["www.fda.gov","fis.fda.gov","clinicaltrials.gov", "classic.clinicaltrials.gov"] = ("FDA", "text,tri,sans") -- U.S. Food & Drug Administration
 | u'' u "hpmor.com" || u' u "www.fanfiction.net/r/5782108/" || u' u "www.reddit.com/r/HPMOR/" = ("MoR", "text,tri,italic")
 | u' u "mozilla.org" = ("FF", "text,sans") -- none of the available Firefox SVG logos worked well as a link icon; typically, too much detail, the swirly-spikes too indistinct & under-emphasized, and confusable with DeepMind.
 | u'' u "www.goodreads.com" = ("GR", "text") -- GoodReads: logo doesn’t make sense as a grayscale
 | u'' u "www.harney.com" = ("H", "text") -- The Harney & Sons logo is too fancy to scale down reasonably
 | u'' u "www.hustwit.com" = ("H", "text,sans") -- design documentarian
 | u'' u "kk.org" = ("KK", "text,sans") -- Kevin Kelly
 | aU'' u ["www.lesswrong.com", "sl4.org", "wiki.lesswrong.com", "www.greaterwrong.com"] = ("LW", "text") -- LW logo is just a colored ‘LW’, so no point in converting. Other user: wiki.lesswrong.com
 | u'' u "www.longecity.org" = ("⧖", "text") -- Longecity “⧖” U+29D6 WHITE HOURGLASS UNICODE
 | aU'' u ["michaelnielsen.org", "quantum.country", "numinous.productions", "cognitivemedium.com", "neuralnetworksanddeeplearning.com"] = ("MN", "text")
 | u'' u "www.motherjones.com" = ("MJ", "text,sans")
 | u' u ".nature.com" = ("n", "text") -- Nature
 | u'' u "openreview.net" = ("OR", "text,sans") -- doesn't seem to have any real logo or wordmark: <https://openreview.net/about>
 | u'' u "www.overcomingbias.com" || u' u "mason.gmu.edu/~rhanson/" = ("OB", "text") -- OB logo too bad to use
 | u'' u "www.theparisreview.org" = ("PR", "text") -- The Paris Review: not even going to try to make their weird bird logo work
 | u'' u "www.theverge.com" = ("▽", "text") -- The Verge uses a sort of delta Escher triangle-esque 'V' stylization <https://en.wikipedia.org/wiki/The_Verge> which looks like a triangle pointing down, so, ▽ WHITE DOWN-POINTING TRIANGLE (Nabla operator) &#x25BD; &#9661;
 | u'' u "www.quora.com" = ("Q", "text") -- surprisingly, no one's taken 'Q' yet
 | aU'' u ["www.edwardtufte.com", "edwardtufte.github.io"] || aU' u ["github.com/edwardtufte/", "github.com/jez/tufte-pandoc-jekyll", "github.com/jez/tufte", "github.com/clayh53/tufte", "/doc/design/typography/rubrication/1990-tufte-envisioninginformation-ch5-byrneseuclid.pdf", "cran.r-project.org/web/packages/tufte/"] = ("ET", "text")
 | aU'' u ["cran.r-project.org", "www.r-project.org", "lme4.r-forge.r-project.org", "www.metafor-project.org", "rstudio.com"] || u' u "github.com/paul-buerkner/brms" = ("R", "text") -- R: at this point R Studio has taken over a lot of control of the R ecosystem, so might as well treat them as official too… primary user: cran.r-project.org
 | u'' u "www.science.org" || u'' u "sciencemag.org" = ("S", "text") -- Science is just typeset in red
 | u'' u "www.sciencedaily.com" = ("SD", "text,sans")
 | u'' u "www.sciencenews.org" = ("SN", "text,sans") -- <https://en.wikipedia.org/wiki/Science_News>
 | u'' u "sethroberts.net" = ("SR", "text,sans") -- Logo is a sans 'S' on a red circle background; can't use 'S' because already used by Slate.
 | u'' u "slate.com" = ("S", "text,sans")
 | u'' u "www.salon.com" = ("s", "text")
 | u'' u "scholars-stage.org" = ("Ss", "text") -- Avoid the unfortunate connotations of ‘SS’
 | u'' u "www.technologyreview.com" = ("T", "text,sans") -- Technology Review (their logo has a little slash in it which you probably can’t see at low-res) but is otherwise just a ‘T’ so meh
 | aU'' u ["texample.net", "ctan.org", "www.tug.org", "tug.org"] = ("tex", "svg") -- Properly turning the 'TeX' logotype in a link icon is hard. You can't use the official logo: <https://commons.wikimedia.org/wiki/File:TeX_logo.svg> is unworkable as a tiny icon, Computer Modern's thinness issues are massively exacerbated & it's unreadable (it's not great on computer screens to begin with, and shrunk down to a link-icon, even worse); you can cheat in pure Unicode with 'TₑX' (LATIN SUBSCRIPT SMALL LETTER E U+2091;, there is no 'LARGE LETTER E' unfortunately) but this took winds up looking pretty bad in practice. So what I did was create my own SVG TeX link-icon in Inkscape, using Source Serif Pro bold letters, arranged by hand like the logotype, and then rescaled horizontally ~120% to make the strokes thick enough that they'd survive downscaling. *That* works.
 | u'' u "tvtropes.org" = ("TV", "text") -- TV Tropes: their lampshade icon is unrecognizable & hard to see small
 | aU'' u ["www.urth.net", "lists.urth.net", "www.wolfewiki.com"] = ("U", "text") -- Gene Wolfe mailing list; no logo; primary user: lists.urth.net
 | u'' u "www.vanityfair.com" = ("VF", "text")
 | u' u "onlinelibrary.wiley.com" = ("W", "text,sans") -- Wiley & Sons’s ‘W’ unfortunately overlaps with the WP ‘W’ but if we sans it, maybe that’ll help. primary user: onlinelibrary.wiley.com
 | aU' u ["longbets.org", "longnow.org", "rosettaproject.org", "theinterval.org"] = ("X", "text,overline") -- Long Now Foundation projects
 | u'' u "yunnansourcing.com" || u'' u "yunnansourcing.us" = ("ys", "text")
 | u'' u "predictionbook.com" = ("?", "text,sans,bold") -- PB logo is confusing. A purple question mark…?
 | u'' u "silkroadvb5piz3r.onion" || u'' u "silkroad5v7dywlc.onion" = ("SR1", "text,sans")
 | u'' u "beepb00p.xyz" = ("\129302", "text") -- ROBOT FACE U+1F916
 | u'' u "antilop.cc" = ("෴", "text") -- SINHALA PUNCTUATION KUNDDALIYA 0x0DF4 - because it's written by "Moustache", get it
 | u'' u "memteaimports.com" = ("MT", "text,sans")
 | u'' u "forum.effectivealtruism.org" || u'' u "www.effectivealtruism.org" = ("EA", "text")
 | u'' u "boards.fireden.net" || u'' u "archive.foolz.us" || u' u "4channel.org"  = ("4CH", "text,sans")
 | u'' u "www.kaggle.com" = ("k", "text,sans")
 | u'' u "www.jneurosci.org" = ("JN", "text")
 | u'' u "www.discovermagazine.com" = ("D", "text")
 | u'' u "tl.net" = ("TL", "text,sans")
 | u'' u "www.businessinsider.com" = ("BI", "text,sans")
 | u'' u "dnstats.net" = ("dn", "text,sans")
 | u'' u "www.newsweek.com" = ("NW", "text") -- logo is 'N' but there are too many 'N's floating around, so abbreviate 'Newsweek' as 'NW'
 | u'' u "www.thecut.com" = ("TC", "text")
 | u'' u "www.scientificamerican.com" = ("SA", "text")
 | u'' u "www.mirror.co.uk" = ("M", "text,sans")
 | u'' u "www.stuff.co.nz" = ("NZ", "text,sans") -- even their official name 'Stuff' is lazy and unmemorable. I just think of them as 'that New Zealand website reporting on crime & DNM stuff'…
 | u'' u "chronopause.com" = ("M.D.", "text,sans") -- Mike Darwin, similarly TODO: experiment with initials using periods - does this work as-is? How about quad? '﹒' SMALL FULL STOP U+FE52 does not work.
 | u'' u "vitalik.ca" || u' u "/doc/economics/mechanism-design/quadratic-voting/2018-buterin.pdf" = ("V.B.", "text,sans") -- Vitalik Buterin, similarly
 | u'' u "unenumerated.blogspot.com" || u' u "szabo.best.vwh.net" || u' u "nick-szabo" = ("N.S.", "text,sans") -- Nick Szabo
 | u'' u "scottaaronson.blog" || u'' u "www.scottaaronson.com" = ("S.A.", "text,sans") -- Scott Aaronson
 | u'' u "www.rifters.com" = ("P.W.", "text,sans") -- Peter Watts
 | u'' u "www.antipope.org" = ("C.S.", "text,sans") -- Charles Stross
 | u'' u "www.ribbonfarm.com" = ("ℝ𝔽", "text,sans")
 | aU'' u ["en.touhouwiki.net", "touhou.fandom.com", "w.atwiki.jp"] || u' u "www.reddit.com/r/TOUHOUMUSIC/" = ("☯", "text") -- NOTE: override Fandom catch-all
 | u'' u "www.reuters.com" = ("R", "text,sans") -- the official Reuters logo <https://en.wikipedia.org/wiki/File:Reuters_Logo.svg> looks like it's summoning a seraphim
 | u'' u "www.deviantart.com" = ("DA", "text,sans") -- the official logo <https://en.wikipedia.org/wiki/File:DeviantArt_Logo.svg> isn't *too* bad and is at least 8 years old, but I don't recognize it so I doubt most readers would.
 | u'' u "www.smithsonianmag.com" = ("SM", "text")
 | u'' u "scienceblogs.com" = ("Sᵇ", "text,sans,italic")
 | u'' u "www.theage.com.au" = ("A", "text")
 | u'' u "www.dailydot.com" = ("D.", "text,sans")
 | u'' u "www.johndcook.com" = ("JC", "text,sans")
 | u'' u "www.candyjapan.com"  = ("🍬", "text")
 | aU'' u ["www.cambridge.org", "journals.cambridge.org", "static.cambridge.org"] = ("⛨", "text") -- ⛨ BLACK CROSS ON SHIELD U+26E8, roughly imitating <https://en.wikipedia.org/wiki/Coat_of_arms_of_the_University_of_Cambridge>
 | u' u "royalsocietypublishing.org" = ("RS", "text") -- <https://en.wikipedia.org/wiki/Royal_Society>
 | u'' u "www.sequentialtart.com" = ("ST", "text,sans")
 | u'' u "www.psychologytoday.com" = ("PT", "text,sans")
 | u'' u "www.independent.co.uk" = ("TI", "text") -- <https://en.wikipedia.org/wiki/File:The_Independent_news_logo.svg> swooping-hawk icon would be illegible as link icon
 | u'' u "www.fastcompany.com" = ("FC", "text")
 | u'' u "elifesciences.org" = ("eL", "text,sans")
 | u'' u "www.w3.org" = ("W3", "text,sans")
 | u'' u "www.metafilter.com" || u'' u "ask.metafilter.com" = ("MF", "text,sans,italic")
 | u'' u "qz.com" = ("QZ", "text,sans")
 | u'' u "blog.23andme.com" || u'' u "23andme.com" = ("23", "text")
 | u'' u "www.ft.com" = ("FT", "text")
 | u'' u "techcrunch.com" = ("TC", "text,mono")
 | u'' u "www.nzherald.co.nz" = ("𝕳", "text")
 | u'' u "nintil.com" = ("𝓝", "text") -- @ArtirKel José Luis Ricón Fernández de la Puente
 | u' u "livejournal.com" = ("LJ", "text,sans")
 | u'' u "www.newscientist.com" = ("NS", "text,sans")
 | u'' u "www.brookings.edu" = ("B", "text")
 | u'' u "variety.com" = ("𝓥", "text")
 | u'' u "theconversation.com" = ("🗨", "text")
 | u'' u "patch.com" = ("P", "text,sans")
 | u'' u "www.palladiummag.com" = ("Pd", "text,sans") -- "P" is their logo but that is too generic and collides, so take 'palladium' literally & use the element abbreviation
 | u'' u "www.gq.com" = ("GQ", "text,sans")
 | aU'' u ["bls.gov", "data.bls.gov", "www.bls.gov"] = ("BLS", "text,sans")
 | u'' u "thegradient.pub" = ("∇", "text")
 | u'' u "www.projectrho.com" = ("ρ", "text")
 | u'' u "harpers.org" = ("H", "text")
 | u'' u "foreignpolicy.com" = ("FP", "text")
 | u'' u "www.unqualified-reservations.org" = ("UR", "text")
 | u'' u "www.thenewatlantis.com" = ("NA", "text")
 | aU'' u ["www.supermemo.com", "super-memory.com"] = ("SM", "text,sans")
 | u'' u "qwantz.com" = ("DC", "text,sans")
 | u'' u "qualiacomputing.com" = ("QC", "text,sans")
 | u'' u "www.thelancet.com" = ("L", "text")
 | u'' u "www.nngroup.com" = ("NN", "text,sans")
 | u'' u "replicationindex.com" = ("RI", "text,sans")
 | u' u ".yahoo.com" = ("Y!", "text,sans")
 | u' u "github.com/huggingface/" || u' u "medium.com/huggingface/" || u'' u "huggingface.co" = ("\129303", "text") -- "🤗" HUGGING FACE U+1F917
 | u'' u "quantifiedself.com" || u'' u "forum.quantifiedself.com" || u' u "www.reddit.com/r/QuantifiedSelf/" = ("QS", "text,sans")
 | u'' u "www.pragmatic.ml" = ("𝕄", "text") -- Madison May, machine learning blog
 | u'' u "www.research.va.gov" = ("VA", "text,sans") -- US Department of Veterans Affair (mostly linked for Million Veteran Project)
 | u'' u "apnews.com" = ("AP", "text,sans")
 | aU' u ["www.unz.com/gnxp/", "razib.substack.com", "www.razib.com", "www.razibkhan.com", "www.gnxp.com", "twitter.com/razibkhan"] = ("RK", "text,sans") -- Razib Khan
 | u'' u "www.outsideonline.com" = ("𝕆", "text,sans") -- imitate the shadowing on Outside Online's 'O' <https://www.outsideonline.com/wp-content/uploads/2021/07/favicon-194x194-1.png>
 | u'' u "jaymans.wordpress.com" = ("J👨🏾", "text,sans") -- JayMan
 | u'' u "norvig.com" = ("N", "text,sans") -- Google Director of Research <https://en.wikipedia.org/wiki/Peter_Norvig>; <https://norvig.com/favicon.ico> is actually instantly recognizable & well-chosen, but unfortunately, only works because of the *colors*... so we'll settle for a plain sans capital N.
 | u'' u "novelai.net" || u'' u "blog.novelai.net" = ("🖋", "text") -- LOWER LEFT FOUNTAIN PEN (U+1F58B); NovelAI logo is a fountain pen nib.
 | u'' u "www.thebeliever.net" = ("𝐁", "text,serif") -- _The Believer_ magazine <https://en.wikipedia.org/wiki/The_Believer_(magazine)>, McSweeney's spinoff (formerly <https://believermag.com>): logo is a dropshadow serif capital B logo
 | u'' u "solar.lowtechmagazine.com" = ("☀", "text,sans") -- Low Tech Magazine (U+2600 BLACK SUN WITH RAYS)
 | u'' u "www.rollingstone.com" = ("𝓡𝐒", "text") -- Rolling Stone <https://www.rollingstone.com/wp-content/uploads/2022/08/cropped-Rolling-Stone-Favicon.png> <https://en.wikipedia.org/wiki/File:Rolling_Stone_2022.svg>
 | u'' u "www.popsci.com" = ("PS", "text,sans") -- Popular Science magazine (no usable or recognizable logos)

 -- Tri/triple TLAs
 | u' u "animenewsnetwork.com" = ("ANN", "text,tri")
 | u'' u "www.catb.org" || u'' u "esr.ibiblio.org" = ("ESR", "text,tri,sans")
 | u'' u "arstechnica.com" = ("ars", "text,tri,sans") -- Ars is an orange box, not usable
 | u' u ".bbc.com" || u' u ".bbc.co.uk" = ("BBC", "text,tri,sans") -- BBC: no usable logo
 | u' u ".bmj.com" = ("bmj", "text,tri,sans") -- British Medical Journal or just ‘bmj’
 | u'' u "www.cdc.gov" = ("CDC", "text,tri")
 | u'' u "boardgamegeek.com" = ("BGG", "text,tri,sans") -- puzzle-piece logo would be unrecognizable as link icon <https://cf.geekdo-static.com/images/logos/navbar-logo-bgg-b2.svg>
 | u'' u "thehub7dnl5nmcz5.onion" = ("Hub", "text,tri,sans")
 | u'' u "www.abc.net.au" || u'' u "abcnews.go.com" = ("ABC", "text,tri,sans") -- <https://en.wikipedia.org/wiki/Australian_Broadcasting_Corporation>
 | u'' u "www.odt.co.nz" = ("ODT", "text,tri")
 | u'' u "knowyourmeme.com" = ("KYM", "text,tri")
 | u'' u "freakonomics.com" = ("FRK", "text,tri,sans") -- hybrid apple-orange icon (get it, "comparing apples & oranges") doesn't work as favicon or link
 | u'' u "aiimpacts.org" = ("AII", "text,tri")
 | u'' u "scp-wiki.wikidot.com" = ("SCP", "text,tri,sans")
 | aU'' u ["latitude.io", "play.aidungeon.io", "aidungeon.medium.com"] || u' u "www.reddit.com/r/AIDungeon"  || u' u "www.patreon.com/AIDungeon" = ("AID", "text,tri,sans")
 | u'' u "nap.nationalacademies.org" = ("NAP", "text,tri")
 | u' u ".cnn.com" = ("CNN", "text,tri,sans")
 | u'' u "www.npr.org" || u'' u "text.npr.org" = ("npr", "text,tri,sans") -- NPR styles it in lowercase in their |n|p|r| logo
 | u'' u "www.filfre.net" = ("TDA", "text,tri,sans") -- Filfre.net/The Digital Antiquarian has no logo or usable substitute…
 | u'' u "lwn.net" = ("LWN", "text,tri,sans")
 | u' u ".fast.ai" ||  u' u "github.com/fastai/" = ("F.ai", "text,tri")
 | u'' u "www.sfgate.com" = ("SFG", "text,tri,sans")
 | u' u ".cbslocal.com" || u'' u "www.cbsnews.com" = ("CBS", "text,tri,sans")
 | u'' u "nypost.com" = ("NYP", "text,tri,sans,italic")
 | u'' u "www.justice.gov" = ("DoJ", "text,tri") -- US federal Department of Justice
 | u'' u "www.fanfiction.net" = ("FFN", "text,tri,sans") -- The FF.net logo is pretty crazy (<https://en.wikipedia.org/wiki/File:Fanfictionnetlogo.jpeg> is the *normal* one!), and I don’t think anyone would recognize it in monochrome. 'FF' as an abbreviation is confusing with FireFox, so expand to "FFN".
 | u'' u "myanimelist.net" = ("MAL", "text,tri,sans") -- MAL: the blue of their logo doesn’t work, so just text
 | aU' u ["onegeek.org", "eva-fan.com", "evaotaku.com", "khara.co.jp", "gainax.co.jp", "17th-angel.tumblr.com", "gainax.com", "johakyu.net", "kanzaki.sub.jp", "homepage3.nifty.com", "www.cjas.org", "www.dummy-system.com", "www.evalegend.com", "www.usagi.org", "animekritik.wordpress.com", "fullfrontal.moe", "wavemotioncannon.com", "www.angelfire.com/anime4/"] = ("NGE", "text,tri") -- Primary user: forum.evageeks.org wiki.evageeks.org
 | u'' u "academic.oup.com" || u' u ".nutrition.org" || u' u ".oxfordjournals.org" = ("OUP", "text,tri") -- Oxford Academic Journals / OUP
 | u'' u "poniesatdawn.bandcamp.com" = ("P@D", "text,tri")
 | u'' u "slatestarscratchpad.tumblr.com" || u'' u "www.astralcodexten.com" || (u'' u "slatestarcodex.com" && (extension u /= ".pdf")) || (isLocal u && (u' u "yvain" ||  u' u "slatestarcodex")) = ("SSC", "text,tri") -- SSC logo too bad to use; NOTE: we want PDFs merely hosted on SSC to not match, and fall through to get a PDF icon instead
 | u'' u "plato.stanford.edu" = ("SEP", "text,tri")
 | u'' u "www.vox.com" = ("Vox", "text,tri,italic")
 | aU'' u ["blogs.wsj.com", "online.wsj.com", "www.wsj.com"] = ("WSJ", "text,tri") -- The Wall Street Journal
 | u'' u "gameprogrammingpatterns.com" = ("GPP", "text,tri,sans")
 | u'' u "www.metopera.org" = ("Met", "text,tri")
 | u'' u "www.schneier.com" = ("SOS", "text,tri,sans") -- "Bruce Schneier", who writes "Schneier On Security" or "SOS" (Easter egg: the Schneier.com favicon encodes Morse code into its edges, which says… "SOS")
 | u'' u "hbr.org" = ("HBR", "text,tri,sans") -- Harvard Business Review
 | aU'' u ["dl.acm.org", "queue.acm.org", "cacm.acm.org"] = ("acm", "text,tri,sans") -- <https://en.wikipedia.org/wiki/File:Association_for_Computing_Machinery_(ACM)_logo.svg> 'acm' sans in a circle inside a diamond; can't fake it with Unicode joiners (they'd only put one character into a circle+diamond), and I probably don't want to bother with a SVG.
 | u' u "www.cs.utexas.edu/~EWD/" = ("EWD", "text,tri,sans") -- Edsger W. Dijkstra, of course, wrote in sans
 | u'' u "iopscience.iop.org" = ("IOP", "text,tri,sans") -- <https://en.wikipedia.org/wiki/IOP_Publishing> Institute of Physics Publishing
 | u'' u "80000hours.org" = ("80k", "text,tri,sans") -- 80,000 Hours (Centre for Effective Altruism, FHI, Oxford)
 | u'' u "researchers.wls.wisc.edu" || u' u "www.ssc.wisc.edu/wlsresearch/" = ("WLS", "text,tri,sans") -- Wisconsin Longitudinal Study
 | u' u "host.robots.ox.ac.uk/pascal/VOC" = ("VOC", "text,tri,sans") -- PASCAL VOC (Visual Object Classes) machine learning image dataset/competition
 | u'' u "www.tandfonline.com" = ("T&F", "text,tri,sans") -- Taylor & Francis: their icon is a small white oil lamp on a blue background, but it's illegible as a favicon and just looks like a white blob on a blue square; since these need to be monochrome, that makes it useless. Plus I recognize 'Taylor & Francis' (sans serif, as usual for STEM publishers) more anyway, so 'T&F' is the natural tri-text icon. A possible Unicode alternative for the AMPERSAND if it is too big is 'U+FE60 ﹠ SMALL AMPERSAND'.
 | u' u "omega0.xyz/omega8008/" || aU' u ["/doc/statistics/bayes/1988-jaynes-maximumentropyandbayesianmethods.pdf", "www-biba.inrialpes.fr/Jaynes/cc18i.pdf"] = ("ETJ", "text,tri,sans") -- E. T. Jaynes book/paper website
 | u' u "paperswithcode.com" = ("PwC", "text,tri,sans") -- 'Papers With Code' does have a weird '[|⏐|⏐|]' icon (supposed to be a bar graph of different performances, I guess) which would work monochrome, but I don't recognize it and I doubt anyone else would either, especially as a link icon, but 'PwC' *might* be recognizable, so we'll go with that for now.
 | u'' u "www.pewresearch.org" = ("Pew", "text,tri") -- Pew Research Center: logo <https://en.wikipedia.org/wiki/File:Pew_Research_Center.svg>. While very cool, and worthy of a Scandinavian black death metal band, it is unrecognizable and would 'shimmer' intensely if scaled down to a link icon & would have to be recreated. So, another text icon it is. Everyone knows what "Pew" means.
 | u'' u "thelastpsychiatrist.com" = ("TLP", "text,tri,sans")
 | u'' u "www.lrb.co.uk" = ("LRB", "text,tri") -- London Review of Books <https://en.wikipedia.org/wiki/London_Review_of_Books>
 | u'' u "archiveofourown.org" = ("Ao3", "text,tri") -- Archive of Our Own <https://archiveofourown.org/> <https://en.wikipedia.org/wiki/Archive_of_Our_Own>
 | u'' u "www.nlsinfo.org" = ("NLS", "text,tri,sans") -- the National Longitudinal Surveys (BLS), eg. NLSY79 <https://en.wikipedia.org/wiki/National_Longitudinal_Surveys>

 -- Quad-letter-square icons.
 | aU'' u ["jamanetwork.com", "jama.jamanetwork.com", "archinte.jamanetwork.com"]  = ("JAMA", "text,sans,quad") -- The Journal of the American Medical Association (JAMA)
 | u'' u "www.cell.com" = ("CELL", "text,quad,sans") -- Cell: their logo is unrecognizable (and dumb)
 | u'' u "mlp.fandom.com" = ("MLPW", "text,quad,sans") -- NOTE: override Fandom catch-all
 | u'' u "www.fimfiction.net" = ("FIMF", "text,quad,mono")
 | u'' u "www.nber.org" && (extension u /= ".pdf") || u'' u "ideas.repec.org" = ("NBER", "text,quad") -- IDEAS/RePEc doesn't seem to actually be run by or affiliated with NBER, but it's so close topically that I think readers can forgive it.
 | u'' u "www.pnas.org" = ("PNAS", "text,quad") -- PNAS: they don’t have a real logo, but their favicon does a nice little compact square (white text on blue background), and we can replicate that in CSS (but just as black text on white background, per our monochrome theme) [On second thought, all of the icons using background squares, like HN/YC, are very intense and hard to visually balance. It's probably better to leave PNAS as just a quad-letter.]
 | u'' u "www.rand.org" = ("RAND", "text,quad,sans")
 | u' u ".sagepub.com" = ("SAGE", "text,quad,sans") -- Sage Journals’s logo is a circled S… but would anyone recognize it? Primary user: journals.sagepub.com
 | u'' u "publicdomainreview.org" = ("TPDR", "text,quad")
 | u' u "xkcd.com" = ("XKCD", "text,quad,sans") -- covers explainxkcd.com, what-if.xkcd.com…
 | u'' u "www.imdb.com" = ("IMDb", "text,sans,quad")
 | u'' u "www.nejm.org" = ("NEJM", "text,quad")
 | u'' u "spectrum.ieee.org" || u'' u "ieeexplore.ieee.org" = ("IEEE", "text,mono,quad")
 | u'' u "rjlipton.wordpress.com" = ("P = NP", "text,quad") -- NOTE: not 4 letters because we need the spacing for a more reasonable look. 'FULLWIDTH EQUALs SIGN' turns out to be *too* big and stack up three high. using 2 HAIR SPACE will separate the '=' slightly from the 'P' while not causing the 3-layer layout.
 | u' u "mitpress.mit.edu/sites/default/files/sicp/" = ("SI CP", "text,quad,sans")
 | u' u "mitpress.mit.edu/" = ("MIT", "text,tri,mono") -- if it's not _SICP_, fall back.
 | u'' u "jaspervdj.be" = ("JVDJ", "text,quad,mono")
 | u'' u "gizmodo.com" = ("GIZM", "text,quad,mono")
 | u'' u "www.mdpi.com" = ("MDPI", "text,quad,sans") -- <https://en.wikipedia.org/wiki/MDPI> chemical subscript+superscript probably not recognized by anyone & too bulky even as SVG NOTE: doesn't wrap right with serif, so has to be ans
 | u'' u "mattlakeman.org" = ("MATT", "text,quad,sans")
 | u'' u "www.poetryfoundation.org" = ("POET", "text,quad,sans") -- <https://www.poetryfoundation.org/> <https://en.wikipedia.org/wiki/Poetry_Foundation> logo is a 2×3 grid "POETRY"; fortunately, 'POET' is a real word and works nicely as a quad
 | u'' u "papers.ssrn.com" = ("SSRN", "text,quad")
 | u'' u "www.vice.com" || u'' u "motherboard.vice.com" = ("VICE", "text,quad,italic")
 | aU'' u ["www.courtlistener.com", "archive.recapthelaw.org", "storage.courtlistener.com", "www.courtlistener.com", "www.pacer.uscourts.gov", "www.pacer.gov", "pcl.uscourts.gov"] = ("PACR", "text,quad")
 | u'' u "www.nybooks.com" = ("NYRB", "text,quad")
 | u'' u "www.jstor.org" = ("JTOR", "text,quad") -- quad looks better skipping the thin 'S'
 | u'' u "thisanimedoesnotexist.ai" = ("TADE", "text,quad,sans")
 | u'' u "www.thisfursonadoesnotexist.com" = ("TFDE", "text,quad,sans")
 | u'' u "www.thiswaifudoesnotexist.net" = ("TWDE", "text,quad,sans")
 | u'' u "thisponydoesnotexist.net" = ("TPDE", "text,quad,sans")
 | u'' u "pcdb.santafe.edu" = ("PCDB", "text,quad,sans")
 | u'' u "vndb.org" = ("VNDB", "text,quad,sans")
 | u'' u "www.huffpost.com" = ("HUFF", "text,quad,sans")
 | u'' u "longreads.com" = ("Long", "text,quad")
 | u'' u "warontherocks.com" = ("WOTR", "text,quad,sans")
 | u'' u "krebsonsecurity.com" = ("Kreb", "text,quad,sans") -- KrebsOnSecurity: 'KOS' unrecognizable, favicon a baffling mystery, Brian Krebs is generally known as 'Krebs', so abbreviate that
 | u'' u "www.nextplatform.com" = ("NEXT", "text,quad,sans") -- The Next Platform's double-cube logo *could* work as an SVG but not convinced it'd be recognizable
 | u'' u "www.vetta.org" = ("Legg", "text,quad,sans") -- Shane Legg (DeepMind)
 | u'' u "www.spiegel.de" = ("SPGL", "text,quad") -- Der Spiegel, major German newspaper; the 'S' logo is unrecognizable given the sheer number of 'S' logos out there, so abbreviation instead
 | u'' u "tasvideos.org" = ("TASV", "text,quad") -- TASVideos.org: tool-assisted game movies
 | u'' u "habr.com" = ("Habr", "text,quad,sans") -- Russian tech collaborative blog <https://en.wikipedia.org/wiki/Habr>
 | u'' u "www.teds.ac.uk" = ("TEDS", "text,quad,sans") -- UK twin registry founded by Robert Plomin, heavily used in behavioral genetics & sociology/psychology; it has a clever little logo (https://www.teds.ac.uk/Content/Images/TEDSlogo.png) & a monochrome SVG version would work… but unfortunately no one ever uses it & it is always known as "Twins Early Development Study (TEDS)"
 | u'' u "stability.ai" || u' u "#stability" || u' u "&org=stability" = ("SD", "text,sans")
 | u'' u "patrickcollison.com" = ("PC", "text,sans")
 | u'' u "oeis.org" = ("OEIS", "text,quad,sans") -- On-Line Encyclopedia of Integer Sequences
 | u'' u "bldgblog.com" = ("BLDG", "text,quad,monospace") -- BLDGBLOG (“building blog”, 2004), by Geoff Manaugh <https://en.wikipedia.org/wiki/BLDGBLOG>
 | u' u "twitter.com/patio11" || aU'' u ["www.bitsaboutmoney.com", "training.kalzumeus.com", "www.kalzumeus.com"] = ("pt11", "text,quad,monospace") -- patio11 / Patrick McKenzie / Bingo Card Creator / Bits About Money / Stripe. The 'dragon' icon for Kalzumeus.com would be illegible & probably not recognizable at this point even by long-time readers, but a stripped down 'pt11' should look enough like 'patio11'...
 | u'' u "mathshistory.st-andrews.ac.uk" = ("M  T", "text,quad,sans") -- MacTutor History of Mathematics Archive: a weird one, <https://mathshistory.st-andrews.ac.uk/static/img/logo.png> - crude sans but only 2 letters kinda like a diagonal in a square or a TeX. Experiment with using EN SPACE to force a diagonal quad layout.
 | u'' u "scale.com" = ("SCLE", "text,quad,monospace") -- Scale, a large data-labeling company heavily used behind-the-scenes by FANG & OpenAI etc for outsourcing evaluating text, labeling images, and so on.
 | u'' u "nunosempere.com" = ("nuno", "text,quad,sans") -- Nuño Sempere
 | u'' u "ourworldindata.org" = ("OWI  D", "text,quad,sans") -- Our World In Data (OWID) <https://en.wikipedia.org/wiki/Our_World_in_Data>; NOTE: uses THIN SPACE+THIN SPACE because the 'W' is so wide

 -- SVG icons (remember the link-icon name is substituted in as part of the URL to the SVG icon)
 | aU'' u ["www.amazon.com", "aws.amazon.com", "amazon.com", "smile.amazon.com", "aboutamazon.com"] || u' u "amazon.co." = ("amazon", "svg")
 | u'' u "en.bitcoin.it" || u'' u "bitcointalk.org" || u'' u "www.blockchain.com" = ("bitcoin", "svg")
 | u'' u "www.biorxiv.org" || u'' u "www.medrxiv.org" = ("chi-dna", "svg") -- BioRxiv (custom icon: italic Chi with DNA cross-strands).
 | u'' u "distill.pub" = ("distillpub", "svg") -- Distill ML journal.
 | u'' u "www.dropbox.com" || u'' u "dl.dropboxusercontent.com" = ("dropbox", "svg") -- Dropbox: old file-host, deprecated since they’ve started killing inactive accounts & their UI become awful. primary user: dl.dropboxusercontent.com
 | u'' u "www.erowid.org" || u'' u "www.drugsdata.org" = ("erowid", "svg")
 | aU' u [".tensorflow.org", "github.com/tensorflow/", "medium.com/tensorflow/"] = ("tensorflow", "svg") -- <https://simpleicons.org/?q=tensorflow>; NOTE: hosted on Github, so override Github
 | aU'' u ["github.com", "copilot.github.com", "archiveprogram.github.com", "gist.github.com", "github.blog", "compvis.github.io"] = ("github", "svg") -- Github; I exclude *.github.io & raw.githubusercontent.com because that’s blogs/papers.
 | u'' u "www.paulgraham.com" = ("pg", "text,monospace") -- Paul Graham, known by username 'pg' on HN
 | u' u "ycombinator.com" = ("hn", "svg") -- HN/YC (shared logo). primary user: news.ycombinator.com
 | aU' u ["webcitation.org", "mementoweb.org", "archive.org", "archive-it.org", "wiki.archiveteam.org", "waybackmachine.org", "archive.is", "archive.md", "archive.ph", "archive.today", "babel.hathitrust.org"] = ("internetarchive", "svg") -- HathiTrust <https://en.wikipedia.org/wiki/HathiTrust> is confusingly nebulous but its cute elephant logo is unrecognizable and I regard it as basically a wrapper around Google Books+Internet Archive, so I think it's less confusing to put it under the IA logo.
 | u'' u "mega.nz" = ("mega", "svg") -- MegaUpload/Mega: filesharing (used for big files).
 | u'' u "intelligence.org" = ("miri", "svg") -- MIRI/intelligence.org.
 | u' u ".nytimes.com" = ("newyorktimes", "svg") -- The New York Times: manual edit, reducing full 'NEW YORK TIMES' SVG logo to just the ‘T’ they use as an icon.
 | aU'' u ["www.ncbi.nlm.nih.gov", "pubmed.ncbi.nlm.nih.gov"] = ("nlm-ncbi", "svg") -- NCBI/Pubmed: simplification of their logo (https://upload.wikimedia.org/wikipedia/commons/0/07/US-NLM-NCBI-Logo.svg). primary user: ncbi.nlm.nih.gov
 | u'' u "www.patreon.com" = ("patreon", "svg") -- Patreon. (Used the old one (https://upload.wikimedia.org/wikipedia/commons/9/94/Patreon_logo.svg) because I don’t like the new one.)
 | aU' u ["plos.org", "plosone.org", "plosmedicine.org"] = ("plos", "svg") -- PLOS ONE in all their domain permutations… primary user: journals.plos.org
 | u' u "reddit.com" = ("reddit", "svg") -- www.reddit.com
 | aU' u ["overflow.net", "overflow.com", "stackexchange.com"] = ("stackexchange", "svg") -- The *Exchange/*Overflow family of websites.
 | u' u "substack.com" = ("substack", "svg") -- gwern.substack.com
 | u'' u "www.theguardian.com" || u'' u "www.guardian.co.uk" = ("theguardian", "svg") -- El Grauniad.
 | u'' u "www.newyorker.com" = ("thenewyorker", "svg") -- The New Yorker: the Dandy SVG, simplified & rotated more vertically.
 | u' u "tumblr.com" = ("tumblr", "svg")
 | aU'' u ["twitter.com", "blog.twitter.com", "developer.twitter.com"] = ("twitter", "svg")
 | u'' u "www.uptontea.com" = ("uptontea", "svg")
 | u'' u "soundcloud.com" = ("audio", "svg")
 | u' u ".bandcamp.com" = ("audio", "svg")
 | u'' u "www.washingtonpost.com" = ("washingtonpost", "svg") -- The Washington Post: truncated their blackletter to ‘WP’.
 | aU' u ["wikipedia.org", "wikimedia.org", "wiktionary.org", "wikisource.org", "wikimediafoundation.org", "stats.grok.se", "wikibooks.org", "wikiquote.org", "xtools.wmflabs.org", "www.mediawiki.org"] = ("wikipedia", "svg") -- primary user: en.wikipedia.org, meta.wikimedia.org, en.wiktionary.org, en.wikisource.org
 | u' u ".fandom.com" = ("♡", "text") -- formerly known as Wikia, renamed to 'Fandom' and adopted a heart-based logo: <https://en.wikipedia.org/wiki/Fandom_(website)#2016%E2%80%932018:_Fandom_brand>; this is an umbrella covering all the subdomains; more specific Fandom wikis go before in the list (like MLP)
 | u' u "www.wired.com" || u' u "www.wired.co.uk" = ("wired", "svg") -- an inverse "W" on a black background (Wiley is just a "W")
 | u'' u "www.youtube.com" = ("youtube", "svg")
 | aU'' u ["vimeo.com", "player.vimeo.com"] = ("file-video", "svg")
 | u'' u "www.telegraph.co.uk" = ("the-telegraph", "svg") -- edited from <https://en.wikipedia.org/wiki/File:The_Telegraph.svg>
 | u'' u "www.openphilanthropy.org" = ("open-philanthropy", "svg")
 | u'' u "www.atlasobscura.com" = ("atlas-obscura", "svg")
 | aU'' u ["blog.eleuther.ai", "www.eleuther.ai", "pile.eleuther.ai", "6b.eleuther.ai"] || u' u "arankomatsuzaki.wordpress.com/2021/06/04/gpt-j/" = ("eleutherai", "svg")
 | u'' u "arankomatsuzaki.wordpress.com" = ("ak", "text,sans") -- known with the other ak on Twitter; put after EAI in the SVG section because the GPT-J announcement is an EAI project
 | u' u ".apple.com" = ("apple", "svg")
 | u'' u "www.quantamagazine.org" = ("quanta", "svg")
 | u'' u "creativecommons.org" = ("creative-commons", "svg") -- <https://creativecommons.org/about/downloads>
 | u'' u "www.alcor.org" = ("alcor", "svg")
 | aU'' u ["www.econlib.org", "www.econtalk.org", "betonit.blog"] = ("econlib", "svg") -- EconLib/EconLog/EconTalk torch icon <https://3ijp5i2qkzo4hq4yrxfteqh-wpengine.netdna-ssl.com/wp-content/themes/econlib/assets/icons/torch-icon.svg>
 | u' u ".cochrane.org" || aU'' u ["www.cochrane.org.uk", "www.cochranelibrary.com"] = ("cochrane-collaboration", "svg") -- <https://upload.wikimedia.org/wikipedia/commons/archive/a/a1/20200122144949%21Cclogo.svg> (Newer version is not actually an SVG; reported on Talk page)
 | u'' u "www.connectedpapers.com" = ("connected-papers", "svg")
 | u' u "nasa.gov" = ("nasa", "svg") -- NASA has way too many subdomains to try to whitelist them individually. SVG is a quad version of <https://commons.wikimedia.org/wiki/File:NASA_Worm_logo_(black).svg>
 | aU'' u ["link.springer.com", "rd.springer.com"] || u' u ".biomedcentral.com" = ("springerlink", "svg")  -- (♘) WHITE CHESS KNIGHT as SVG
 | u'' u "www.metaculus.com" = ("metaculus", "svg")
 | u'' u "wandb.ai" = ("wandb", "svg") -- Weights & Biases/WandB: blog/podcasts, writeups etc; complicated 4-dot grid logo intended to evoke NN layers with large/small weights, <view-source:https://assets.website-files.com/5ac6b7f2924c656f2b13a88c/6066c22135b8983b61ad7939_weights-and-biases-logo.svg>; edited into BW, enlarged the large dots to make viewable as a link icon
 | aU'' u ["libgen.rs", "libgen.org", "library.bz"] = ("raven", "svg") -- Libgen/Sci-Hub raven+key icon <https://en.wikipedia.org/wiki/File:Scihub_raven.png>, while pretty, is too detailed for a link-icon so fall back to just the raven. There are many LG+SH domains, but these are the only ones we link.
 | u'' u "www.hoover.org" = ("hoover-institution", "svg") -- <https://en.wikipedia.org/wiki/Hoover_Institution_Library_and_Archives> <https://en.wikipedia.org/wiki/Hoover_Tower> <https://en.wikipedia.org/wiki/New_Cathedral_of_Salamanca>
 | u'' u "www.statnews.com" = ("stat-news", "svg") -- STAT News <https://en.wikipedia.org/wiki/Stat_(website)> based on <https://www.statnews.com/wp-content/themes/stat/images/stat-logo.svg>; using Unicode '𝐴' to replicate the 'A' in 'STAT' is probably unreliable cross-platform so we transform the 'STAT' logotype into a quad SVG icon instead.
 | aU'' u ["thepiratebay.org", "rss.thepiratebay.se", "thepiratebay.se",  "thepiratebay.sx"] = ("the-pirate-bay", "svg") -- in theory, you can get a skull & crossbones by Unicode Emoji: BLACK FLAG + SKULL AND CROSSBONES + ZWJ = PIRATE FLAG <https://emojipedia.org/pirate-flag/> (and if that doesn't work try adding U+FE0F to the end). This turns out to be too unreliable across systems (fonts? OSes? browser versions?) that we replaced it with a proper SVG version of The Pirate Bay's cassette-tape (yes, really) + cross-bones.
 | u'' u "retractionwatch.com" = ("magnifying-glass", "svg") -- Retraction Watch <https://en.wikipedia.org/wiki/Retraction_Watch> LEFT-POINTING HOUR GLASS
 | u'' u "www.yudkowsky.net" = ("yud", "svg") -- but of course: י HEBREW LETTER YUD 0x05D9; we use an SVG icon here for the same reason we use a math alef elsewhere instead of the Hebrew one (the RTL of Hebrew script will screw up some browsers, like Mac Firefox)
 | u'' u "nautil.us" = ("nautilus", "svg") -- modeled after 🐚 SPIRAL SHELL (U+1F41A), but turned into monochrome SVG (this icon is usually rendered in color & differently across platforms, so we ship another SVG)
 | u'' u "www.scholarpedia.org" = ("scholarpedia", "svg") -- Scholarpedia <https://en.wikipedia.org/wiki/Scholarpedia>; Adobe trace of their PNG favicon
 | u'' u "radiolab.org" = ("audio-waveform-lines", "svg") -- Radiolab WNYC/NPR <https://en.wikipedia.org/wiki/Radiolab>; <https://fontawesome.com/icons/waveform-lines>/<https://www.svgrepo.com/svg/342965/audio-wave>
 | u'' u "maggieappleton.com" = ("maggie-appleton", "svg")  -- <https://twitter.com/Mappletons> Maggie Appleton, designer (Elicit/Ought), blogger about hypermedia/personal wikis/PKM

 -- many orgs will use a medium subdomain, so we fall back here for Medium and override above:
 | u'' u "medium.com" || u'' u "towardsdatascience.com" = ("𝐌", "text") -- Medium: cheaper to abuse Unicode (𝐌) MATHEMATICAL BOLD CAPITAL M

 -- Filetypes: (we need to parse & extract the extension because many would be too short and match too many URLs if mere infix matching was used)
 | iE u ["tar", "zip", "xz", "img", "bin", "pkl", "onnx", "pt", "maff"] = ("archive", "svg")
 | iE u ["opml", "txt", "xml", "json", "jsonl", "page"] || u'' u "pastebin.com" = ("txt", "svg")
 | iE u ["css", "hs", "js", "conf", "sh", "r", "R", "patch", "diff"] = ("code", "svg")
 | iE u ["doc", "docx"] = ("worddoc", "svg")
 | iE u ["xls", "xlsx", "ods"] = ("spreadsheet", "svg")
 | iE u ["csv"] = ("csv", "svg")
 | iE u ["gif", "bmp", "ico", "jpg", "jpeg", "png", "svg", "xcf", "psd"] = ("image", "svg")
 | iE u ["mp3", "flac", "ogg"] = ("audio", "svg")
 | iE u ["swf", "mp4", "mkv", "webm"] = ("file-video", "svg")
 | iE u ["ebt", "mdb", "mht", "ttf"] = ("misc", "svg")
 | iE u ["epub"] = ("EPUB", "text,quad,sans")
 | u'' u "imgur.com" || u'' u "i.imgur.com"       = ("image", "svg")
 | "/static/" `T.isPrefixOf` u && hasExtension ".html" u  = ("code", "svg")
 | isLocal u && hasExtension ".php" u                     = ("code", "svg")
 | aU' u [".pdf", ".PDF", "/pdf", "type=pdf", "pdfs.semanticscholar.org", "citeseerx.ist.psu.edu", "pdfs.semanticscholar.org"] = ("pdf", "svg")

 -- Fallback
 | otherwise = ("", "")

u', u'' :: T.Text -> T.Text -> Bool
-- simplest check for string anywhere; note that if it is a full domain name like `https://foo.com` (intended to match `https://foo.com/xyz.html`), then it will *not* match when the local-archive code fires and the URL gets rewritten to "/doc/foo.com/$HASH.html". So we error out if the user tries this, having forgotten that u' ≠ u'' in that respect.
u' url v = if "http://" `T.isPrefixOf` v || "https://" `T.isPrefixOf` v then error ("LinkIcon.hs: Overly strict prefix in infix matching (u'): " ++ show url ++ ":" ++ show v) else
  v `T.isInfixOf` url
-- more stringent check, matching exactly the domain name:
u'' url v = if "http://" `T.isPrefixOf` v || "https://" `T.isPrefixOf` v then error ("LinkIcon.hs: Overly strict prefix in infix matching (u''): " ++ show url ++ ":" ++ show v) else
          isHostOrArchive v url

iE :: T.Text -> [T.Text] -> Bool
iE url = elem (T.drop 1 $ extension url)
aU', aU'' :: T.Text -> [T.Text] -> Bool
aU'  url = any (u' url)
aU'' url = any (u'' url)

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

addIcon :: Inline -> (T.Text, T.Text) -> Inline
addIcon x ("", "") = x
addIcon x@(Link (idt,cl,ks) a (b,c)) (icon, iconType)  =
  if hasIcon x then x else Link (idt,cl,
                                  [("link-icon",icon), ("link-icon-type",iconType)] ++
                                  ks) a (b,c)
addIcon x _ = x

isLocal :: T.Text -> Bool
isLocal "" = error $ "LinkIcon: isLocal: Invalid empty string used as link."
isLocal s = T.head s == '/'

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
