{-# LANGUAGE OverloadedStrings #-}

module LinkIcon (linkIcon, rebuildSVGIconCSS, linkIconPrioritize) where

import Control.Monad (unless)
import Data.List (sort)
import qualified Data.Map.Strict as M (toList, fromListWith, map)
import Data.Maybe (fromJust)
import Data.Text as T (append, drop, head, isInfixOf, isPrefixOf, pack, unpack, Text)
import Text.Pandoc (Inline(Link,Str), nullAttr)
import Network.URI (parseURIReference, uriPath)
import System.FilePath (takeExtension)
import Data.Containers.ListUtils (nubOrd)
import System.Directory (doesFileExist)

import LinkBacklink (readBacklinksDB)
import Utils (host, writeUpdatedFile, hasKeyAL, anyPrefixT)

-- Statically, at site 'compile-time', define the link-icons for links. Doing this at runtime with CSS is
-- entirely possible and originally done by links.css, but the logic becomes increasingly convoluted
-- & bug-prone because of CSS properties like cascading & longest-matches, and exceptions like
-- 'organization icon overrides PDF icon' become fertile sources of errors & regressions.
-- Doing this at compile-time in Haskell is easier and also reduces performance burden on the client
-- browser.

-- Generate a HTML <style>-delimited CSS block written to
-- `static/includes/inlined-graphical-linkicon-styles.html` for transclusion into `default.html`.
-- The SVG icons need to be specified like `("wikipedia","svg")` → `a[data-link-icon='wikipedia'] {
-- --link-icon-url: url('/static/img/icons/wikipedia.svg'); }`.
-- These could be written by hand every time a SVG-related icon is added/deleted/renamed, but that
-- risks getting out of sync and triggering the bugs that moving our CSS link icons to compile-time
-- server generation was supposed to end. We want more of a single source of truth.
-- So, we generate them using the test-suite: every SVG icon must have a corresponding test. (You
-- don't have a test? You come back later! One test one icon!) The test necessarily is redundant
-- with the original definition, otherwise it doesn't test anything. But since it's there, we can
-- reuse it. This lets us have our cake—writing a big `linkIcon` function mixing the SVG rules with
-- the text rules freely, in whatever order is most convenient for expressing
-- precedence/overriding—while still generating CSS code from normal data (the test suite entries).
rebuildSVGIconCSS :: IO ()
rebuildSVGIconCSS = do unless (null linkIconTest) $ error ("Error! Link icons failed match! : " ++ show linkIconTest)
                       let svgs1 = nubOrd $ map (\(_,icon,_) -> T.unpack icon) $ filter (\(_, _, icontype) -> icontype == "svg") linkIconTestUnitsText
                       let svgs2 = nubOrd $ map (\(_,icon,_) -> T.unpack icon) $ filter (\(_, _, icontype) -> icontype == "svg") linkIconTestUnitsLink
                       let svg = svgs1++svgs2
                       mapM_ (\s -> do existsP <- doesFileExist $ "static/img/icons/" ++ s ++ ".svg"
                                       unless existsP (error ("ERROR: SVG icon " ++ s ++ " does not exist!")))
                         svg
                       let html = unlines $ ["<style id=\"graphical-link-icons\">"] ++
                             map (\s -> "a[data-link-icon='" ++ s ++ "'] { --link-icon-url: url('/static/img/icons/" ++ s ++ ".svg'); }") svg ++
                             ["</style>"]
                       writeUpdatedFile "svgicons" "static/includes/inlined-graphical-linkicon-styles.html" (T.pack html)

-- Based on <links.js>.
-- The idea is to annotate every `<a>` with two new `data-` attributes, `data-link-icon` and
-- `data-link-icon-type` which jointly specify the type & content of the icon. The link-icon for
-- 'svg' type is overloaded to be a filename in `/static/img/icons/$LINKICON.svg`.
--
-- λ linkIcon $ Link nullAttr [Str "foo"] ("https://forum.evageeks.org/forum?id=2222", "")
-- Link ("",[],[("link-icon","EG"),("link-icon-type","text")]) [Str "foo"] ("https://forum.evageeks.org/forum?id=2222","")
-- λ linkIcon $ Link nullAttr [Str "foo"] ("/docs/foo.pdf", "Foo & Bar 2022")
-- → Link ("",[],[("link-icon","pdf"),("link-icon-type","svg")]) [Str "foo"] ("/docs/foo.pdf","Foo & Bar 2022")
-- → <a href="/docs/foo.pdf" data-link-icon="pdf" data-link-icon-type="svg" title="Foo &amp; Bar 2022">foo</a>
--
-- In cases of local archive links, matches on the `/docs/www/$DOMAIN/$ARCHIVE.html` aren't necessarily *exactly*
-- as powerful; local archives deliberately throw away directory structure for simpler addresses, so 2 matches for
-- 'foo.com/bar/*' and 'foo.com/quux/*' would collide when trying to match just '/docs/www/foo.com/$ARCHIVE.html'.
-- For this case, we detect & exploit the `data-original-URL` attribute which is around for just such problems,
-- and we run matches on the original URL, and everything should work as expected then.
--
-- TODO: the PDF checks are incomplete (and only look for ".pdf" essentially) but since I'm trying
-- to remove all weird non-standard PDFs and host locally all PDFs with clean names & extensions,
-- maybe that's a vestigial concern?
linkIcon :: Inline -> Inline
linkIcon x@(Link (_,cl,attributes) _ (u, _))
 -- Short-circuits for manual control (one can either disable icons with a `[Foo](URL){.icon-not}`
 -- class, or specify a preferred icon on a link, like `[Foo](URL){.link-icon="deepmind"
 -- .link-icon-type="svg"}` by specifying the attributes directly), or define a global URL/(link
 -- icon, link icon type) rewrite:
 | "icon-not" `elem` cl = x
 -- NOTE: 'gwern': the Fraktur '𝔊' for local essay links (where 'local' is defined as '/' but with no '.' in it) is set dynamically clients-ide by rewrite.js:l1075 (`designateSpecialLinkIcons`) and so we do not handle it here. (It is also overridden by 'icon-not'; WARNING: 'icon-not' is used at runtime and should not be erased!)
 | hasIcon x           = x
 | hasKeyAL u overrideLinkIcons = let (i,it) = fromJust $ lookup u overrideLinkIcons in addIcon x i it
 | anyPrefixT u ["/metadata/annotations/"] = x

 | "directory-indexes-upwards"   `elem` cl = aI "arrow-up-left"    "svg"
 | "directory-indexes-downwards" `elem` cl = aI "arrow-down-right" "svg"
 | "directory-indexes-sideways"  `elem` cl = aI "arrow-right"      "svg"

 -- organizational mentions or affiliations take precedence over domain or filetypes; typically matches anywhere in the URL.
 | u' "deepmind"  = aI "deepmind" "svg" -- DeepMind; match articles or anchors about DM too. Primary user: deepmind.com, DM papers on Arxiv
 | u' "schmidhuber" || u' "people.idsia.ch/~juergen/" = aI "SMDH" "text,quad,sans" -- Jürgen Schmidhuber homepage & tagged links; should override Arxiv, Bloomberg, NYT, Facebook etc
 | u' "facebook" || u' ".fb.com"  = aI "facebook" "svg"
 | u' "sites.google.com/berkeley.edu" || aU'' ["ml.berkeley.edu", "people.eecs.berkeley.edu", "bair.berkeley.edu"] = aI "BAIR" "text,quad,mono" -- Berkeley AI Research: Chelsea Finn, Sergey Levine, robotics, Decision Transformer, etc. Overrides Google Sites.
 | u'' "groups.google.com" = aI "✉" "text"
 | u'' "scholar.google.com" = aI "google-scholar" "svg" -- Google Scholar.
 | u'' "docs.google.com" = aI "worddoc" "svg"
 | u' "google" || u'' "magenta.tensorflow.org" = aI "google" "svg" -- Google searches, other tools. Note that there are many Google subdomains, which we may wish to iconify differently, so we narrow down with just ‘www’. Google Brain doesn’t have any consistent or recognizable logo, don’t bother trying to replicate one of the dots (no one will recognize it); use ‘GB’ would not be a bad idea, but I suspect that would also confuse people. So reusing the ‘G’ is the least bad option.
 | u' "nvidia"  || aU'' ["nvlabs.github.io", "nv-adlr.github.io", "nv-tlabs.github.io"] = aI "n" "text,sans,italic" -- Nvidia: <https://en.wikipedia.org/wiki/Nvidia#cite_note-2> yeah no. Disambiguate from Nature's "n" by italicizing (Nvidia *did* italicize the lowercase 'n' for a long time, so seems reasonable)
 | u' "openai" || u'' "gptprompts.wikidot.com" = aI "openai" "svg" -- OpenAI; match articles or anchors about OA too. primary user: openai.com, Arxiv papers. Brockman's GPT-prompts wiki is semi-official IMO.
 | u' "microsoft" = aI "MS" "text,sans,italic" -- Microsoft: I don’t think <https://en.wikipedia.org/wiki/File:Microsoft_logo_(2012).svg> is all that recognizable, so make a logotype more like <https://en.wikipedia.org/wiki/File:Microsoft_logo_(1987).svg>: an italic sans "MS".
 | u' "#anthropic" || u'' "transformer-circuits.pub" || u'' "www.anthropic.com" = aI "anthropic" "svg" -- need to override Arxiv

 -- Domains:
 | aU'' ["psyarxiv.com", "files.osf.io", "osf.io"] = aI "ψ" "text" -- Unicode trickery icons: GREEK SMALL LETTER PSI
 | u'' "unsongbook.com" = aI "ℵ" "text" -- SSC’s book: (ℵ) ALEF SYMBOL (We use the math symbol instead of the Hebrew deliberately, to avoid triggering bizarre Hebrew bidirectional text-related layout bugs on Mac Firefox.)
 | u'' "andrewgelman.com" || u'' "statmodeling.stat.columbia.edu" = aI "▅▇▃" "text" -- Favicon is a little normal distribution/histogram (▅▇▃) LOWER FIVE EIGHTHS BLOCK, LOWER SEVEN EIGHTHS BLOCK, LOWER THREE EIGHTHS BLOCK
 | u'' "meltingasphalt.com" = aI "▲" "text" -- Kevin Simler’s Melting Asphalt blog uses 3 triangles but that's too many, so we just use one. (▲) BLACK UP-POINTING TRIANGLE
 | u'' "www.tinyletter.com" = aI "✉" "text" -- TinyLetter’s icon, without color, isn’t memorable enough; throw in the other email services (✉) ENVELOPE
 | aU'' ["blog.givewell.org", "www.givewell.org", "files.givewell.org"] || u' "groups.yahoo.com/group/givewell/" = aI "GW" "text"
 | u'' "groups.yahoo.com" = aI "✉" "text"
 | u'' "www.mail-archive.com" = aI "✉" "text"
 | u'' "marginalrevolution.com" = aI "M𝐑" "text" -- MR: cheaper to abuse Unicode (𝐑) MATHEMATICAL BOLD CAPITAL R
 | u'' "www.frontiersin.org" = aI "FS" "text,sans" -- <https://en.wikipedia.org/wiki/Frontiers_Media> multiple-cubes logo too busy for an icon, no Unicode equivalent
 | aU'' ["www.gutenberg.org", "gutenberg.ca", "gutenberg.net.au", "www.fadedpage.com"] = aI "PG" "text" -- Faded Pages isn't strictly-speaking a Project Gutenberg org, but they work with Distributed Proofreaders & their work is in PG Canada and they do similar things so meh.
 | u'' "guzey.com" = aI "A" "text,sans"
 | u'' "www.forbes.com" = aI "F" "text"
 | (u' "haskell.org" && (extension u /= ".hs")) || u' "haskellers.com" = aI "𝛌" "text" -- Haskell: simplify logo; the double-lambda is too busy when used for link icons (𝛌) MATHEMATICAL BOLD SMALL LAMBDA primary user: hackage.haskell.org; we make an exception for .hs files hosted on Haskell.org, like config files, where the source code-ness is more relevant than the organization/domain
 | u'' "arxiv.org" || u'' "ar5iv.labs.arxiv.org" = aI "𝛘" "text" --  ArXiv: Their skull+smiley logo is too bizarre & off-putting to use, in addition to not working as a tiny monochrome image (𝛘) MATHEMATICAL BOLD SMALL CHI (bold makes it show up better when tiny)
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
 | aU'' ["wiki.evageeks.org","forum.evageeks.org","www.evamonkey.com"] || u' "www.evacommentary.org" || u' "nitter.hu/EvaMonkey/" = aI "EG" "text" -- Evangelion: we’ll split this into EGF-related and other NGE sites
 | aU'' ["www.fda.gov","fis.fda.gov","clinicaltrials.gov"] = aI "FDA" "text,tri,sans" -- U.S. Food & Drug Administration
 | u'' "www.hpmor.com" || u' "www.fanfiction.net/r/5782108/" || u' "old.reddit.com/r/HPMOR/" = aI "MoR" "text,tri,italic"
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
 | aU'' ["www.edwardtufte.com", "edwardtufte.github.io"] || aU' ["github.com/edwardtufte/", "github.com/jez/tufte-pandoc-jekyll", "github.com/jez/tufte", "github.com/clayh53/tufte", "/docs/design/typography/rubrication/1990-tufte-envisioninginformation-ch5-byrneseuclid.pdf", "cran.r-project.org/web/packages/tufte/"] = aI "ET" "text"
 | aU'' ["cran.r-project.org", "www.r-project.org", "lme4.r-forge.r-project.org", "www.metafor-project.org", "rstudio.com"] || u' "github.com/paul-buerkner/brms" = aI "R" "text" -- R: at this point R Studio has taken over a lot of control of the R ecosystem, so might as well treat them as official too… primary user: cran.r-project.org
 | u'' "www.science.org" || u'' "sciencemag.org" = aI "S" "text" -- Science is just typeset in red
 | u'' "www.sciencedaily.com" = aI "SD" "text,sans"
 | u'' "www.sciencenews.org" = aI "SN" "text,sans" -- <https://en.wikipedia.org/wiki/Science_News>
 | u'' "sethroberts.net" = aI "SR" "text,sans" -- Logo is a sans 'S' on a red circle background; can't use 'S' because already used by Slate.
 | u'' "slate.com" = aI "S" "text,sans"
 | u'' "www.salon.com" = aI "s" "text"
 | u'' "scholars-stage.org" = aI "Ss" "text" -- Avoid the unfortunate connotations of ‘SS’
 | u'' "www.technologyreview.com" = aI "T" "text,sans" -- Technology Review (their logo has a little slash in it which you probably can’t see at low-res) but is otherwise just a ‘T’ so meh
 | aU'' ["texample.net", "ctan.org", "www.tug.org", "tug.org"] = aI "TₑX" "text" -- ₑ LATIN SUBSCRIPT SMALL LETTER E U+2091; can't use the official logo: <https://commons.wikimedia.org/wiki/File:TeX_logo.svg> is unworkable as a tiny icon, Computer Modern's thinness issues are massively exacerbated & it's unreadable
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
 | u'' "www.memteaimports.com" = aI "MT" "text,sans"
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
 | u'' "vitalik.ca" || u' "/docs/economics/2018-buterin.pdf" = aI "V.B." "text,sans" -- Vitalik Buterin, similarly
 | u'' "unenumerated.blogspot.com" || u' "szabo.best.vwh.net" || u' "nick-szabo" = aI "N.S." "text,sans" -- Nick Szabo
 | u'' "scottaaronson.blog" || u'' "www.scottaaronson.com" = aI "S.A." "text,sans" -- Scott Aaronson
 | u'' "www.rifters.com" = aI "P.W." "text,sans" -- Peter Watts
 | u'' "www.antipope.org" = aI "C.S." "text,sans" -- Charles Stross
 | u'' "www.ribbonfarm.com" = aI "ℝ𝔽" "text,sans"
 | aU'' ["en.touhouwiki.net", "touhou.fandom.com", "w.atwiki.jp"] || u' "old.reddit.com/r/TOUHOUMUSIC/" = aI "☯" "text" -- NOTE: override Fandom catch-all
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
 | u'' "www.gq.com" = aI "GQ" "text,sans"
 | aU'' ["bls.gov", "data.bls.gov", "www.bls.gov"] = aI "BLS" "text,sans"
 | u'' "thegradient.pub" = aI "∇" "text"
 | u'' "www.projectrho.com" = aI "ρ" "text"
 | u'' "harpers.org" = aI "H" "text"
 | u'' "foreignpolicy.com" = aI "FP" "text"
 | u'' "www.unqualified-reservations.org" = aI "UR" "text"
 | u'' "www.thenewatlantis.com" = aI "NA" "text"
 | u'' "www.supermemo.com" = aI "SM" "text,sans"
 | u'' "qwantz.com" = aI "DC" "text,sans"
 | u'' "qualiacomputing.com" = aI "QC" "text,sans"
 | u'' "www.thelancet.com" = aI "L" "text"
 | u'' "www.nngroup.com" = aI "NN" "text,sans"
 | u'' "replicationindex.com" = aI "RI" "text,sans"
 | u' ".yahoo.com" = aI "Y!" "text,sans"
 | u' "github.com/huggingface/" || u' "medium.com/huggingface/" || u'' "huggingface.co" = aI "\129303" "text" -- "🤗" HUGGING FACE U+1F917
 | u'' "quantifiedself.com" || u'' "forum.quantifiedself.com" || u' "old.reddit.com/r/QuantifiedSelf/" = aI "QS" "text,sans"
 | u'' "www.pragmatic.ml" = aI "𝕄" "text" -- Madison May, machine learning blog
 | u'' "www.research.va.gov" = aI "VA" "text,sans" -- US Department of Veterans Affair (mostly linked for Million Veteran Project)
 | u'' "apnews.com" = aI "AP" "text,sans"

 -- Tri/triple TLAs
 | u' "animenewsnetwork.com" = aI "ANN" "text,tri"
 | u'' "www.catb.org" || u'' "esr.ibiblio.org" = aI "ESR" "text,tri,sans"
 | u'' "arstechnica.com" = aI "ars" "text,tri,sans" -- Ars is an orange box, not usable
 | u' ".bbc.com" || u' ".bbc.co.uk" = aI "BBC" "text,tri,sans" -- BBC: no usable logo
 | u' ".bmj.com" = aI "bmj" "text,tri,sans" -- British Medical Journal or just ‘bmj’
 | u'' "www.cdc.gov" = aI "CDC" "text,tri"
 | u'' "boardgamegeek.com" = aI "BGG" "text,tri,sans" -- puzzle-piece logo would be unrecognizable as link icon <https://cf.geekdo-static.com/images/logos/navbar-logo-bgg-b2.svg>
 | u'' "thehub7dnl5nmcz5.onion" = aI "Hub" "text,tri,sans"
 | u'' "www.abc.net.au" = aI "ABC" "text,tri,sans" -- <https://en.wikipedia.org/wiki/Australian_Broadcasting_Corporation>
 | u'' "www.odt.co.nz" = aI "ODT" "text,tri"
 | u'' "knowyourmeme.com" = aI "KYM" "text,tri"
 | u'' "freakonomics.com" = aI "FRK" "text,tri,sans" -- hybrid apple-orange icon (get it, "comparing apples & oranges") doesn't work as favicon or link
 | u'' "aiimpacts.org" = aI "AII" "text,tri"
 | u'' "scp-wiki.wikidot.com" = aI "SCP" "text,tri,sans"
 | aU'' ["latitude.io", "play.aidungeon.io", "aidungeon.medium.com"] || u' "old.reddit.com/r/AIDungeon"  || u' "www.patreon.com/AIDungeon" = aI "AID" "text,tri,sans"
 | u'' "www.nap.edu" = aI "NAP" "text,tri"
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
 | u'' "slatestarscratchpad.tumblr.com" || u'' "astralcodexten.substack.com" || (u'' "slatestarcodex.com" && (extension u /= ".pdf")) || (isLocal x u && (u' "yvain" ||  u' "slatestarcodex")) = aI "SSC" "text,tri" -- SSC logo too bad to use; NOTE: we want PDFs merely hosted on SSC to not match, and fall through to get a PDF icon instead
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
 | u' "www.ssc.wisc.edu/wlsresearch/" = aI "WLS" "text,tri,sans" -- Wisconsin Longitudinal Study
 | u' "host.robots.ox.ac.uk/pascal/VOC" = aI "VOC" "text,tri,sans" -- PASCAL VOC (Visual Object Classes) machine learning image dataset/competition
 | u'' "www.tandfonline.com" = aI "T&F" "text,tri,sans" -- Taylor & Francis: their icon is a small white oil lamp on a blue background, but it's illegible as a favicon and just looks like a white blob on a blue square; since these need to be monochrome, that makes it useless. Plus I recognize 'Taylor & Francis' (sans serif, as usual for STEM publishers) more anyway, so 'T&F' is the natural tri-text icon. A possible Unicode alternative for the AMPERSAND if it is too big is 'U+FE60 ﹠ SMALL AMPERSAND'.
 | u' "omega0.xyz/omega8008/" || aU' ["/docs/statistics/bayes/1988-jaynes-maximumentropyandbayesianmethods.pdf", "www-biba.inrialpes.fr/Jaynes/cc18i.pdf"] = aI "ETJ" "text,tri,sans" -- E. T. Jaynes book/paper website
 | u' "paperswithcode.com" = aI "PwC" "text,tri,sans" -- 'Papers With Code' does have a weird '[|⏐|⏐|]' icon (supposed to be a bar graph of different performances, I guess) which would work monochrome, but I don't recognize it and I doubt anyone else would either, especially as a link icon, but 'PwC' *might* be recognizable, so we'll go with that for now.
 | u'' "www.pewresearch.org" = aI "Pew" "text,tri" -- Pew Research Center: logo <https://en.wikipedia.org/wiki/File:Pew_Research_Center.svg>. While very cool, and worthy of a Scandinavian black death metal band, it is unrecognizable and would 'shimmer' intensely if scaled down to a link icon & would have to be recreated. So, another text icon it is. Everyone knows what "Pew" means.

 -- Quad-letter-square icons.
 | aU'' ["jamanetwork.com", "jama.jamanetwork.com", "archinte.jamanetwork.com"]  = aI "JAMA" "text,sans,quad" -- The Journal of the American Medical Association (JAMA)
 | u'' "www.cell.com" = aI "CELL" "text,quad,sans" -- Cell: their logo is unrecognizable (and dumb)
 | u'' "mlp.fandom.com" = aI "MLPW" "text,quad,sans" -- NOTE: override Fandom catch-all
 | u'' "www.fimfiction.net" = aI "FIMF" "text,quad,mono"
 | u'' "www.nber.org" && (extension u /= ".pdf") = aI "NBER" "text,quad"
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
 | u' "mitpress.mit.edu/books/" = aI "MIT" "text,tri,mono" -- if it's not _SICP_, fall back.x
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
 | u'' "stability.ai" || u' "#stability" = aI "SD" "text,sans"
 | u'' "patrickcollison.com" = aI "PC" "text,sans"

 -- SVG icons (remember the link-icon name is substituted in as part of the URL to the SVG icon)
 | aU'' ["www.amazon.com", "aws.amazon.com", "amazon.com", "smile.amazon.com", "aboutamazon.com"] || u' "amazon.co." = aI "amazon" "svg"
 | u'' "en.bitcoin.it" || u'' "bitcointalk.org" || u' "www.blockchain.com/btc/" = aI "bitcoin" "svg"
 | u'' "www.biorxiv.org" || u'' "www.medrxiv.org" = aI "chi-dna" "svg" -- BioRxiv (custom icon: italic Chi with DNA cross-strands).
 | u'' "distill.pub" = aI "distillpub" "svg" -- Distill ML journal.
 | u'' "www.dropbox.com" || u'' "dl.dropboxusercontent.com" = aI "dropbox" "svg" -- Dropbox: old file-host, deprecated since they’ve started killing inactive accounts & their UI become awful. primary user: dl.dropboxusercontent.com
 | u'' "www.erowid.org" || u'' "www.drugsdata.org" = aI "erowid" "svg"
 | aU' [".tensorflow.org", "github.com/tensorflow/", "medium.com/tensorflow/"] = aI "tensorflow" "svg" -- <https://simpleicons.org/?q=tensorflow>; NOTE: hosted on Github, so override Github
 | aU'' ["github.com", "copilot.github.com", "archiveprogram.github.com", "gist.github.com", "github.blog", "compvis.github.io"] = aI "github" "svg" -- Github; I exclude *.github.io & raw.githubusercontent.com because that’s blogs/papers.
 | u'' "www.paulgraham.com" || u' "ycombinator.com" = aI "hn" "svg" -- PG/HN/YC (shared logo). primary user: news.ycombinator.com
 | aU' ["webcitation.org", "mementoweb.org", "archive.org", "archive-it.org", "wiki.archiveteam.org", "waybackmachine.org", "archive.is", "archive.md", "archive.ph", "archive.today"] = aI "internetarchive" "svg"
 | u'' "mega.nz" = aI "mega" "svg" -- MegaUpload/Mega: filesharing (used for big files).
 | u'' "intelligence.org" = aI "miri" "svg" -- MIRI/intelligence.org.
 | u' ".nytimes.com" = aI "newyorktimes" "svg" -- The New York Times: manual edit, reducing full 'NEW YORK TIMES' SVG logo to just the ‘T’ they use as an icon.
 | aU'' ["www.ncbi.nlm.nih.gov", "pubmed.ncbi.nlm.nih.gov", "www.clinicaltrials.gov"] = aI "nlm-ncbi" "svg" -- NCBI/Pubmed: simplification of their logo (https://upload.wikimedia.org/wikipedia/commons/0/07/US-NLM-NCBI-Logo.svg). primary user: ncbi.nlm.nih.gov
 | u'' "www.patreon.com" = aI "patreon" "svg" -- Patreon. (Used the old one (https://upload.wikimedia.org/wikipedia/commons/9/94/Patreon_logo.svg) because I don’t like the new one.)
 | aU' ["plos.org", "plosone.org", "plosmedicine.org"] = aI "plos" "svg" -- PLOS ONE in all their domain permutations… primary user: journals.plos.org
 | u' "reddit.com" = aI "reddit" "svg" -- old.reddit.com
 | aU' ["overflow.net", "overflow.com", "stackexchange.com"] = aI "stackexchange" "svg" -- The *Exchange/*Overflow family of websites.
 | u' "substack.com" = aI "substack" "svg" -- gwern.substack.com
 | u'' "www.theguardian.com" || u'' "www.guardian.co.uk" = aI "theguardian" "svg" -- El Grauniad.
 | u'' "www.newyorker.com" = aI "thenewyorker" "svg" -- The New Yorker: the Dandy SVG, simplified & rotated more vertically.
 | u' "tumblr.com" = aI "tumblr" "svg"
 | aU'' ["twitter.com", "blog.twitter.com", "developer.twitter.com", "nitter.hu"] = aI "twitter" "svg"
 | u'' "www.uptontea.com" = aI "uptontea" "svg"
 | u'' "soundcloud.com" = aI "audio" "svg"
 | u' ".bandcamp.com" = aI "audio" "svg"
 | u'' "www.washingtonpost.com" = aI "washingtonpost" "svg" -- The Washington Post: truncated their blackletter to ‘WP’.
 | aU' ["wikipedia.org", "wikimedia.org", "wiktionary.org", "wikisource.org", "wikimediafoundation.org", "stats.grok.se", "wikibooks.org", "wikiquote.org", "xtools.wmflabs.org"] = aI "wikipedia" "svg" -- primary user: en.wikipedia.org, meta.wikimedia.org, en.wiktionary.org, en.wikisource.org
 | u' ".fandom.com" = aI "♡" "text" -- formerly known as Wikia, renamed to 'Fandom' and adopted a heart-based logo: <https://en.wikipedia.org/wiki/Fandom_(website)#2016%E2%80%932018:_Fandom_brand>; this is an umbrella covering all the subdomains; more specific Fandom wikis go before in the list (like MLP)
 | u' "www.wired.com" || u' "www.wired.co.uk" = aI "wired" "svg" -- an inverse "W" on a black background (Wiley is just a "W")
 | u'' "www.youtube.com" || u'' "www.youtu.be" = aI "youtube" "svg"
 | u'' "vimeo.com" = aI "file-video" "svg"
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
 | iE ["mp3", "wav", "flac", "ogg", "rm"] = aI "audio" "svg"
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
       -- simplest check for string anywhere; note that if it is a full domain name like `https://foo.com` (intended to match `https://foo.com/xyz.html`), then it will *not* match when the local-archive code fires and the URL gets rewritten to "/docs/foo.com/$HASH.html". So we error out if the user tries this, having forgotten that u' ≠ u'' in that respect.
       u' v = if "http://" `T.isPrefixOf` v || "https://" `T.isPrefixOf` v then error ("Overly strict prefix in infix matching: " ++ show u ++ ":" ++ show v) else
         if originalURL=="" then v `T.isInfixOf` u else v `T.isInfixOf` originalURL
       -- more stringent check, matching exactly the domain name:
       u'' v = if originalURL=="" then isHostOrArchive v u else isHostOrArchive v originalURL
       originalURL :: T.Text
       originalURL = case lookup "data-url-original" attributes of
                       Nothing -> ""
                       Just url -> url
       aI :: T.Text -> T.Text -> Inline
       aI = addIcon x
       iE :: [T.Text] -> Bool
       iE = elem (T.drop 1 $ extension u)
       aU', aU'' :: [T.Text] -> Bool
       aU'  = any u'
       aU'' = any u''

linkIcon x = x

-- hardwire globally icons for exact-matches of specific URLs (`[(URL, (Link icon, Link icon type))]`)
overrideLinkIcons :: [(T.Text, (T.Text,T.Text))]
overrideLinkIcons = []

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
                                h == domain || ("/docs/www/"`T.append`domain) `T.isPrefixOf` url

linkIconTestUnitsLink :: [(Inline,T.Text,T.Text)]
linkIconTestUnitsLink = [(Link ("", ["directory-indexes-upwards"],     []) [Str "Test"] ("/docs/index", "Link to parent directory (ascending)"),
                           "arrow-up-left", "svg")
                        , (Link ("", ["directory-indexes-downwards"],  []) [Str "Test"] ("/docs/zeo/index", "Link to child directory zeo (descending)"),
                           "arrow-down-right", "svg")
                          , (Link ("", ["directory-indexes-sideways"], []) [Str "Test"] ("/docs/ai/nn/transformer/alphafold/index", "Link to other directory ai/nn/transformer/alphafold (descending)"),
                           "arrow-right", "svg")
                          ]

-- to find URLs worth defining new link icons for, pass through a list of URLs (perhaps extracted
-- from the backlinks database) and return domains with at least `linkIconMin` matches. (Link icons
-- are enough work that below a certain level of prevalence, they are not worthwhile even if completely
-- transparent/self-explanatory.)
--
-- The original raw results are particularly useful when piped into <https://www.gwern.net/haskell/lcps.hs> to
-- get suggested prefixes/domains worth adding link-icons for, or one can just look at the domains by `host`:
linkIconPrioritize :: IO [(Int,T.Text)]
linkIconPrioritize = do b <- LinkBacklink.readBacklinksDB
                        let b' = M.toList $ M.map length b
                        let b'' = map (\(y,z) -> (host y,z)) $ filter (\(url,_) ->  host url `notElem` blackList &&
                                                                                    not (hasIconURL url) &&
                                                                                    ("." `T.isInfixOf` url)) b'
                        let b''' =  M.fromListWith (+) b''
                        return $ reverse $ sort $ filter (\(e,f) -> e >= linkIconMin && f /="") $ map (\(c,d) -> (d,c)) $ M.toList b'''
  where blackList :: [T.Text] -- dead, icon-less, bad icon, overly-obscure, no real unifying nature worth knowing, etc:
        blackList = ["lilianweng.github.io", "digital.library.unt.edu", "www.smartpowders.com", "www.silverhandmeadery.com",
                     "forums.animesuki.com", "philip.greenspun.com", "eli.thegreenplace.net", "danluu.com", "www.theregister.com",
                     "www.thedailybeast.com", "www.teanobi.com", "www.straighttalkonevidence.org", "www.joelonsoftware.com",
                     "www.jstage.jst.go.jp", "blog.codinghorror.com", "intrade.com", "abandonedfootnotes.blogspot.com", "arr.am",
                     "ascii.textfiles.com", "blog.johantibell.com", "humanvarieties.org", "ilovetypography.com",
                     "new.cognitivefun.net", "findarticles.com", "dataprivacylab.org", "www.thefreelibrary.com", "www.unitedpharmacies-uk.md",
                     "www.petforums.co.uk", "www.e-codices.unifr.ch", "www.bartleby.com", "wellcomecollection.org", "darcs.net",
                     "annals.org", "www.smh.com.au", "www.rrauction.com", "www.replicatedtypo.com", "www.mangaupdates.com",
                     "www.instructables.com", "www.baltimoresun.com", "www.aleph.se", "www.cs.virginia.edu", "mujoco.org", "www.incompleteideas.net",
                     "www.artbreeder.com", "waifulabs.com", "practicaltypography.com", "danwang.co", "www.worldcat.org", "www.thestranger.com",
                     "www.nausicaa.net", "www.hindawi.com", "www.eugenewei.com", "www.buzzfeed.com", "web.mit.edu", "karpathy.github.io",
                     "infoproc.blogspot.com", "hal.archives-ouvertes.fr", "demos.obormot.net", "blog.acolyer.org", "arbtt.nomeata.de",
                     "www.wakapoetry.net", "www.wunderground.com", "www.standard.co.uk", "www.rte.ie", "www.orlandosentinel.com",
                     "www.mercurynews.com", "www.links.org", "www.math.uwaterloo.ca", "sourceforge.net", "shkspr.mobi", "ro.ecu.edu.au",
                     "repository.upenn.edu","proceedings.neurips.cc","polisen.se", "latanyasweeney.org", "highnoongmt.wordpress.com",
                     "alumni.media.mit.edu", "ralphmerkle.com", "www.mentalfloss.com", "www.lightspeedmagazine.com", "ajp.psychiatryonline.org",
                     "agtb.wordpress.com", "aeon.co", "digitalcommons.unl.edu", "emilkirkegaard.dk", "gazette.com", "ohtori.nu",
                     "www.austlii.edu.au", "www.animenewsservice.com", "www.animeigo.com", "www.alexa.com", "vividness.live",
                     "thepharmacyexpress.com", "thegrandnarrative.com", "srconstantin.wordpress.com", "penelope.uchicago.edu",
                     "bmk.sh","www.jstatsoft.org","www.japantimes.co.jp","www.impactcybertrust.org", "www.ex.org", "www.eetimes.com",
                     "www.chronicle.com", "www.aging-us.com", "philpapers.org", "paulfchristiano.com", "parahumans.wordpress.com",
                     "palladiummag.com", "mathworld.wolfram.com", "soranews24.com", "caniuse.com", "www.silcom.com", "esolangs.org",
                     "www.aiweirdness.com", "etherscan.io", "www.theringer.com", "cs.stanford.edu", "mmlab.ie.cuhk.edu.hk", "www.cs.toronto.edu"]
        linkIconMin = 4 :: Int

-- Test suite:
--
-- Test the /Lorem#link-icons test cases as unit-tests of `linkIcon`: it should, for every URL
-- unit-test, generate the specified link-icon/link-icon-type. Return the list of mismatches for
-- fixing.
-- Here we test that URLs get assigned the appropriate icons; on /Lorem, we render them to check for
-- CSS/visual glitches. Any new test-cases should be added to both (with different URLs where possible).
linkIconTest, linkIconTestUnitsText :: [(T.Text,T.Text,T.Text)]
linkIconTest = filter (\(url, li, lit) -> linkIcon (Link nullAttr [] (url,""))
                                          /=
                                          Link ("",[], [("link-icon",li), ("link-icon-type", lit)]) [] (url,"")
                                                   )
               linkIconTestUnitsText
-- in /Lorem order:
linkIconTestUnitsText =
        [("/static/img/icons/deepmind.svg",  "deepmind","svg")
         , ("https://academic.oup.com/ije/article/43/3/775/758445",  "OUP","text,tri")
         , ("https://ajcn.nutrition.org/content/69/5/842.full", "OUP", "text,tri")
         , ("https://ageing.oxfordjournals.org/content/36/5/507.long", "OUP", "text,tri")
         , ("https://www.amazon.co.jp/%E6%AE%8B%E9%85%B7%E3%81%AA%E5%A4%A9%E4%BD%BF%E3%81%AE%E3%82%88%E3%81%86%E3%81%AB%E2%80%95%E6%96%B0%E4%B8%96%E7%B4%80%E3%82%A8%E3%83%B4%E3%82%A1%E3%83%B3%E3%82%B2%E3%83%AA%E3%82%AA%E3%83%B3JUNE%E8%AA%AD%E6%9C%AC-SUN%E3%83%BCMAGAZINE-MOOK-JUNE%E7%B7%A8%E9%9B%86%E9%83%A8/dp/490601125X",  "amazon","svg")
         , ("https://www.amazon.com/gp/product/B0050MYHBQ/",  "amazon","svg")
         , ("https://statmodeling.stat.columbia.edu/2004/12/29/type_1_type_2_t/",  "\9605\9607\9603","text")
         , ("https://statmodeling.stat.columbia.edu/2013/12/17/replication-backlash/",  "\9605\9607\9603","text")
         , ("https://www.animenewsnetwork.com/anime-spotlight/2018/summer/revue-starlight/.132471",  "ANN","text,tri")
         , ("https://arstechnica.com/gadgets/2011/09/the-post-jobs-era-tim-cook-brings-philanthropy-back-to-apple/",  "ars","text,tri,sans")
         , ("https://arxiv.org/abs/0707.1051",  "\120536","text")
         , ("https://ar5iv.labs.arxiv.org/html/2112.11848",  "\120536","text")
         , ("https://www.bbc.com/news/business-43365710",  "BBC","text,tri,sans")
         , ("http://news.bbc.co.uk/2/hi/8448731.stm",  "BBC","text,tri,sans")
         , ("https://www.biorxiv.org/content/10.1101/013896.full",  "chi-dna","svg")
         , ("https://www.medrxiv.org/content/10.1101/2020.05.18.20100685.full",  "chi-dna","svg")
         , ("https://en.bitcoin.it/wiki/Witcoin",  "bitcoin","svg")
         , ("https://bitcointalk.org/index.php?topic=82952.0;all",  "bitcoin","svg")
         , ("https://www.blockchain.com/btc/address/15bD6fYs6p9D9wmniDtTBcQSyWXDYNDCwv", "bitcoin","svg")
         , ("https://www.bloomberg.com/news/articles/2011-03-31/why-unemployment-rose-so-much-dropped-so-fast-commentary-by-alan-krueger",  "\119809","text")
         , ("http://www.businessweek.com/ap/financialnews/D9KQL7CG0.htm", "\119809","text")
         , ("https://bjo.bmj.com/content/93/8/997",  "bmj","text,tri,sans")
         , ("https://danbooru.donmai.us/posts?tags=death_flag", "❐", "text")
         , ("https://derpibooru.org/tags/artist-colon-thisponydoesnotexist", "❐", "text")
         , ("https://safebooru.org/index.php?page=post&s=list&tags=heterochromia", "❐", "text")
         , ("https://www.deepmind.com/blog/alphastar-mastering-the-real-time-strategy-game-starcraft-ii",  "deepmind","svg")
         , ("/docs/reinforcement-learning/model/alphago/2016-silver.pdf#deepmind",  "deepmind","svg")
         , ("https://arxiv.org/abs/1612.08810#deepmind",  "deepmind","svg")
         , ("https://distill.pub/2016/augmented-rnns/",  "distillpub","svg")
         , ("https://dl.dropboxusercontent.com/u/182368464/umineko-compress.tar.xz",  "dropbox","svg")
         , ("https://www.economist.com/briefing/2017/02/18/hello-again-dolly",  "E","text,sans")
         , ("https://www.erowid.org/",  "erowid","svg")
         , ("https://www.drugsdata.org/results.php?start=0&search_field=all&s=modafinil", "erowid","svg")
         , ("https://everything2.com/title/2015+%253A+The+Last+Year+of+Ryoji+Kaji", "E2", "text")
         , ("https://examine.com/supplements/Bacopa+monnieri/", "Eχ", "text,sans")
         , ("/docs/ai/scaling/2020-bell.pdf#facebook",  "facebook","svg")
         , ("https://ai.facebook.com/blog/a-highly-efficient-real-time-text-to-speech-system-deployed-on-cpus/",  "facebook","svg")
         , ("https://engineering.fb.com/2014/11/14/production-engineering/solving-the-mystery-of-link-imbalance-a-metastable-failure-state-at-scale/",  "facebook","svg")
         , ("https://arxiv.org/abs/2004.13637#facebook",  "facebook","svg")
         , ("https://fis.fda.gov/sense/app/d10be6bb-494e-4cd2-82e4-0135608ddc13/sheet/45beeb74-30ab-46be-8267-5756582633b4/state/analysis",  "FDA","text,tri,sans")
         , ("http://clinicaltrials.gov/show/NCT03429075",  "FDA","text,tri,sans")
         , ("https://www.filfre.net/2016/08/ibms-new-flavor/",  "TDA","text,tri,sans")
         , ("https://archiveprogram.github.com/",  "github","svg")
         , ("https://compvis.github.io/taming-transformers/",  "github","svg")
         , ("https://www.goodreads.com/api",  "GR","text")
         , ("https://about.google/",  "google","svg")
         , ("https://arxiv.org/abs/1706.04972#google",  "google","svg")
         , ("https://arxiv.org/pdf/2009.06732.pdf#org=google&page=6",  "google","svg")
         , ("https://www.gwern.net/docs/cs/hardware/2015-kanev.pdf#google",  "google","svg")
         , ("https://www.lesswrong.com/posts/37sHjeisS9uJufi4u/scholarship-how-to-do-it-efficiently",  "LW","text")
         , ("https://www.lesswrong.com",  "LW","text")
         , ("http://sl4.org/archive/0812/index.html#19570",  "LW","text")
         , ("https://www.harney.com/",  "H","text")
         , ("https://www.haskell.org/",  "\120524","text")
         , ("https://web.archive.org/web/20110415182316/http://packdeps.haskellers.com/",  "\120524","text")
         , ("https://intelligence.org/2013/10/03/proofs/",  "miri","svg")
         , ("https://wayback.archive-it.org/org-350/20180911191924/https://www.nlm.nih.gov/news/calhoun_papers_released.html",  "internetarchive","svg")
         , ("https://blog.archive.org/2011/08/17/scanning-a-braille-playboy/",  "internetarchive","svg")
         , ("https://wiki.archiveteam.org/index.php?title=Google_Reader",  "internetarchive","svg")
         , ("http://timetravel.mementoweb.org/",  "internetarchive","svg")
         , ("https://web.archive.org/web/19981202185145/http://www.ex.org/2.4/11-news.html",  "internetarchive","svg")
         , ("https://www.webcitation.org/6Qj7v6mqd",  "internetarchive","svg")
         , ("https://www.justice.gov/archive/usao/cac/Pressroom/2012/045.html",  "DoJ","text,tri")
         , ("https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/414283", "JAMA", "text,sans,quad")
         , ("https://jamanetwork.com/journals/jama/fullarticle/201218", "JAMA", "text,sans,quad")
         , ("https://jama.jamanetwork.com/article.aspx?articleid=183162", "JAMA", "text,sans,quad")
         , ("https://kk.org/books/out-of-control/",  "KK","text,sans")
         , ("https://link.springer.com/article/10.1007/BF02253535",  "springerlink", "svg")
         , ("https://rd.springer.com/article/10.1007/s10071-021-01530-3",  "springerlink", "svg")
         , ("https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-4-13",  "springerlink", "svg")
         , ("https://longbets.org/",  "X","text,overline")
         , ("https://longnow.org/ideas/02014/08/21/lenski-long-term-evolution-experiment/",  "X","text,overline")
         , ("https://www.longecity.org/forum/topic/10464-modalert-is-this-what-modafinil-is-like/?&p=108566#entry108566", "⧖", "text")
         , ("https://groups.google.com/group/ankisrs/",  "\9993","text")
         , ("https://groups.yahoo.com/group/tiffanygrantfanclub/message/5697",  "\9993","text")
         , ("https://gwern.substack.com/",  "substack","svg")
         , ("https://www.tinyletter.com/",  "\9993","text")
         , ("https://www.mail-archive.com/cryptography@metzdowd.com/msg09959.html",  "\9993","text")
         , ("https://marginalrevolution.com/",  "M\119825","text")
         , ("https://www.econlib.org/archives/2016/10/what_do_crimina.html", "econlib", "svg")
         , ("https://www.econtalk.org/adam-cifu-on-ending-medical-reversal/", "econlib", "svg")
         , ("https://betonit.blog/2022/03/02/make-desertion-fast/", "econlib", "svg")
         , ("http://esr.ibiblio.org/?p=7183", "ESR","text,tri,sans")
         , ("http://www.catb.org/jargon/html/R/religious-issues.html", "ESR","text,tri,sans")
         , ("https://www.frontiersin.org/articles/10.3389/fnhum.2011.00134/full", "FS", "text,sans")
         , ("https://gutenberg.ca/ebooks/smithcordwainer-onthegemplanet/smithcordwainer-onthegemplanet-00-h.html", "PG","text")
         , ("https://gutenberg.net.au/ebooks02/0201141h.html", "PG","text")
         , ("https://www.gutenberg.org/files/31663/31663-h/31663-h.htm", "PG","text")
         , ("https://www.fadedpage.com/showbook.php?pid=20160325", "PG", "text")
         , ("https://guzey.com/how-life-sciences-actually-work/", "A", "text,sans")
         , ("https://www.forbes.com/sites/andygreenberg/2013/09/05/follow-the-bitcoins-how-we-got-busted-buying-drugs-on-silk-roads-black-market/", "F", "text")
         , ("https://mattlakeman.org/2020/01/22/hill-billy-elegy-the-culture-of-white-american-poverty/",  "MATT", "text,quad,sans")
         , ("https://www.mdpi.com/2220-9964/8/5/232/htm", "MDPI","text,quad,sans")
         , ("https://medium.com/craft-ventures/the-sharp-startup-when-paypal-found-product-market-fit-5ba47ad35d0b",  "\119820","text")
         , ("https://towardsdatascience.com/stylegan2-projection-a-reliable-method-for-image-forensics-700922579236", "\119820","text")
         , ("https://mega.nz/#!0JVxHQCD!C7ijBpRWNpcL_gubWFR-GTBDJTW1jXI6ThzSxwaw2aE",  "mega","svg")
         , ("https://meltingasphalt.com/interactive/going-critical/",  "\9650","text")
         , ("https://michaelnielsen.org/blog/three-myths-about-scientific-peer-review/", "MN", "text")
         , ("https://quantum.country/qcvc", "MN", "text")
         , ("https://numinous.productions/ttft/", "MN", "text")
         , ("https://cognitivemedium.com/srs-mathematics", "MN", "text")
         , ("http://neuralnetworksanddeeplearning.com/chap6.html", "MN", "text")
         , ("https://www.microsoft.com/en-us/research/blog/turing-nlg-a-17-billion-parameter-language-model-by-microsoft/",  "MS","text,sans,italic")
         , ("https://arxiv.org/abs/2003.13590#microsoft",  "MS","text,sans,italic")
         , ("https://mlp.fandom.com/wiki/A_Canterlot_Wedding_-_Part_1",  "MLPW","text,quad,sans")
         , ("https://hacks.mozilla.org/2021/05/improving-firefox-stability-on-linux/", "FF", "text,sans")
         , ("https://myanimelist.net/anime/1370/Atama_Yama",  "MAL","text,tri,sans")
         , ("https://blogs.nature.com/news/2011/09/reliability_of_new_drug_target.html",  "n","text")
         , ("https://www.newyorker.com/books/page-turner/the-mystery-of-s-the-man-with-an-impossible-memory",  "thenewyorker","svg")
         , ("https://17th-angel.tumblr.com/post/11409371268/anno-a-transfer-student-opens-the-door-with-a",  "NGE","text,tri")
         , ("https://forum.evageeks.org/index.php",  "EG","text")
         , ("https://www.evamonkey.com/ask-john/has-evangelion-influenced-contemporary-gundam-anime.php",  "EG","text")
         , ("https://nitter.hu/EvaMonkey/", "EG", "text")
         , ("https://web.archive.org/web/20151106005148/http://www.evacommentary.org/appendix/character-names.html", "EG", "text")
         , ("http://evaotaku.com/html/programbooks.html",  "NGE","text,tri")
         , ("http://gainax.co.jp/",  "NGE","text,tri")
         , ("https://www.khara.co.jp/hideakianno/personal-biography/",  "NGE","text,tri")
         , ("https://eva-fan.com/blog-entry-1198.html",  "NGE","text,tri")
         , ("https://eva.onegeek.org/",  "NGE","text,tri")
         , ("https://web.archive.org/web/20080127001226/https://web.archive.org/web/20080127001226/http://johakyu.net/lib/2007/07/2007-07-27-000535.php", "NGE","text,tri")
         , ("http://kanzaki.sub.jp/archives/000272.html", "NGE", "text,tri")
         , ("https://web.archive.org/web/20090713005058/http://homepage3.nifty.com/mana/ecom4.htm", "NGE", "text,tri")
         , ("http://www.cjas.org/~leng/daihist.htm", "NGE", "text,tri")
         , ("http://www.dummy-system.com/2013/04/01/intervista-megumi-hayashibara-evangelion-3-0/", "NGE", "text,tri")
         , ("http://www.evalegend.com/interview_anno96.php", "NGE", "text,tri")
         , ("http://www.usagi.org/doi/seiyuu/tv/1997eva.html", "NGE", "text,tri")
         , ("https://animekritik.wordpress.com/2011/05/12/evangelion-2-0-surnames-statements-and-makinami/", "NGE", "text,tri")
         , ("https://fullfrontal.moe/interview-mahiro-maeda/", "NGE", "text,tri")
         , ("https://wavemotioncannon.com/2016/11/08/interview-hideaki-anno-vs-yoshiyuki-tomino-animage-071994/", "NGE", "text,tri")
         , ("https://www.angelfire.com/anime4/mdwigs/Asuka.html", "NGE", "text,tri")
         , ("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2793346/",  "nlm-ncbi","svg")
         , ("https://www.clinicaltrials.gov/ct2/show/NCT01684306",  "nlm-ncbi","svg")
         , ("https://blogs.nvidia.com/blog/2019/03/18/gaugan-photorealistic-landscapes-nvidia-research/",  "n","text,sans,italic")
         , ("https://nv-adlr.github.io/MegatronLM",  "n","text,sans,italic")
         , ("https://nv-tlabs.github.io/big-datasetgan/",  "n","text,sans,italic")
         , ("https://nvlabs.github.io/stylegan2/versions.html",  "n","text,sans,italic")
         , ("https://archive.nytimes.com/6thfloor.blogs.nytimes.com/2013/03/20/a-sham-procedure-leads-to-disappointing-m-s-news/",  "newyorktimes","svg")
         , ("https://www.wsj.com/articles/SB10000872396390443696604577647870908169992",  "WSJ","text,tri")
         , ("/docs/ai/nn/transformer/gpt/dall-e/2020-chen.pdf#openai",  "openai","svg")
         , ("https://openai.com/blog/better-language-models/",  "openai","svg")
         , ("https://openreview.net/forum?id=xTJEN-ggl1b", "OR", "text,sans")
         , ("https://arxiv.org/abs/1611.02779#openai",  "openai","svg")
         , ("http://gptprompts.wikidot.com/context-stuffing", "openai","svg")
         , ("https://www.overcomingbias.com/2009/07/stupider-than-you-realize.html",  "OB","text")
         , ("https://mason.gmu.edu/~rhanson/ideafutures.html",  "OB","text")
         , ("https://www.poetryfoundation.org/poems/44399/pied-beauty", "POET", "text,quad,sans")
         , ("https://www.patreon.com/gwern",  "patreon","svg")
         , ("https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.1000451",  "plos","svg")
         , ("https://speakingofmedicine.plos.org/2012/06/25/less-research-is-needed/",  "plos","svg")
         , ("https://journals.plos.org/plosmedicine/article/fetchObject.action?uri=info:doi/10.1371/journal.pmed.0020124.t004&representation=PNG_M",  "plos","svg")
         , ("https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0023175",  "plos","svg")
         , ("https://www.pnas.org/doi/10.1073/pnas.0610941104",  "PNAS","text,quad")
         , ("https://predictionbook.com/",  "?","text,sans,bold")
         , ("https://publicdomainreview.org/essay/the-lost-world-of-the-london-coffeehouse/",  "TPDR","text,quad")
         , ("https://psyarxiv.com/gjh95/",   "ψ", "text")
         , ("https://osf.io/dhx48/",         "ψ", "text")
         , ("https://cran.r-project.org/web/packages/censReg/index.html",  "R","text")
         , ("https://github.com/paul-buerkner/brms#overview",  "R","text")
         , ("https://www.rand.org/pubs/monographs/MG1026.html",  "RAND","text,quad,sans")
         , ("https://old.reddit.com/r/Supplements/comments/mr0h1/taking_melatonin_forever/",  "reddit","svg")
         , ("https://cro.sagepub.com/content/15/5/252.full.pdf+html",  "SAGE","text,quad,sans")
         , ("https://www.salon.com/2007/11/01/whistleblowers/",  "s","text")
         , ("https://plato.stanford.edu/entries/naturalism-india/", "SEP", "text,tri")
         , ("https://scholar.google.com/citations?user=9hEhCHYAAAAJ&oi=ao",  "google-scholar","svg")
         , ("https://scholars-stage.org/meditations-on-maoism-ye-fus-hard-road-home/",  "Ss","text")
         , ("https://www.sciencedaily.com/releases/2007/05/070525204143.htm",  "SD","text,sans")
         , ("https://www.sciencedirect.com/science/article/pii/S0002929717301076",  "E","text")
         , ("https://www.science.org/doi/10.1126/sciadv.aar3620",  "S","text")
         , ("https://www.sciencenews.org/article/sleep-debt-exacts-deceptive-cost", "SN","text,sans")
         , ("https://sethroberts.net/2008/10/03/diet-and-acne-continued/", "SR", "text,sans")
         , ("https://slate.com/health-and-science/2017/06/daryl-bem-proved-esp-is-real-showed-science-is-broken.html",  "S","text,sans")
         , ("https://slatestarcodex.com/2015/01/15/depression-is-not-a-proxy-for-social-dysfunction/",  "SSC","text,tri")
         , ("https://unsongbook.com/",  "\8501","text")
         , ("/docs/fiction/poetry/2011-yvain-iliadaslawsuit.html",  "SSC","text,tri")
         , ("https://astralcodexten.substack.com/p/know-your-amphetamines",  "SSC","text,tri")
         , ("https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3774989", "SSRN", "text,quad")
         , ("https://stackoverflow.com/questions/1197575/can-scripts-be-inserted-with-innerhtml",  "stackexchange","svg")
         , ("https://mathoverflow.net/questions/32967/have-any-long-suspected-irrational-numbers-turned-out-to-be-rational",  "stackexchange","svg")
         , ("https://crypto.stackexchange.com/questions/2507/can-i-encrypt-user-input-in-a-way-i-cant-decrypt-it-for-a-certain-period-of-tim",  "stackexchange","svg")
         , ("https://ctan.org/pkg/marginnote", "TₑX","text")
         , ("https://tug.org/FontCatalogue/goudyinitialen/", "TₑX","text")
         , ("https://www.tug.org/whatis.html", "TₑX","text")
         , ("https://texample.net/tikz/examples/andler-optimal-lot-size/", "TₑX","text")
         , ("https://www.technologyreview.com/2011/06/21/193829/the-measured-life/",  "T","text,sans")
         , ("https://www.alignmentforum.org/posts/HhWhaSzQr6xmBki8F/birds-planes-brains-and-ai-against-appeals-to-the-complexity", "AF","text,sans")
         , ("https://www.theatlantic.com/business/archive/2011/06/beware-the-stunning-pilot-program/240352/",  "A","text,italic")
         , ("https://www.theguardian.com/books/2013/jul/10/man-behind-dickens-dostoevsky-hoax",  "theguardian","svg")
         , ("https://www.theparisreview.org/blog/2018/04/25/the-strange-history-of-the-king-pine/",  "PR","text")
         , ("https://www.theverge.com/2021/8/7/22614450/unopened-copy-super-mario-bros-sells-2-million-record", "▽", "text")
         , ("https://econlolcats.tumblr.com/",  "tumblr","svg")
         , ("https://tvtropes.org/pmwiki/pmwiki.php/Anime/MobileSuitGundamCharscounterattack",  "TV","text")
         , ("https://developer.twitter.com/en/docs/twitter-api/v1/rules-and-filtering/search-operators",  "twitter","svg")
         , ("https://nitter.hu/intent/user?screen_name=Hiramatz&tw_i=303521521249447936",  "twitter","svg")
         , ("https://www.uptontea.com/formosa-oolong-tea/taiwan-loose-leaf-oolong-tea/p/V00252/",  "uptontea","svg")
         , ("http://lists.urth.net/pipermail/urth-urth.net/2010-December/019108.html",  "U","text")
         , ("https://www.wolfewiki.com/pmwiki/pmwiki.php?n=Stories.SuzanneDelage", "U","text")
         , ("https://www.vice.com/en/article/paabgg/i-bought-a-book-about-the-internet-from-1994-and-none-of-the-links-worked", "VICE", "text,quad,italic")
         , ("https://www.vice.com/en/article/aee8xa/the-silk-road-is-showing-cracks", "VICE", "text,quad,italic")
         , ("https://www.washingtonpost.com/graphics/2018/investigations/dog-auction-rescue-groups-donations/",  "washingtonpost","svg")
         , ("https://diff.wikimedia.org/2009/11/26/wikipedias-volunteer-story/",  "wikipedia","svg")
         , ("https://foundation.wikimedia.org/wiki/Privacy_policy",  "wikipedia","svg")
         , ("https://en.wikipedia.org/wiki/File:Energy_density.svg",  "wikipedia","svg")
         , ("https://en.wikisource.org/wiki/Essays_on_Political_Economy/That_Which_Is_Seen,_and_That_Which_Is_Not_Seen",  "wikipedia","svg")
         , ("https://en.wiktionary.org/wiki/bien_pensant",  "wikipedia","svg")
         , ("http://stats.grok.se/en/201109/Accountancy", "wikipedia","svg")
         , ("https://en.wikibooks.org/wiki/Category:Book:Accountancy", "wikipedia","svg")
         , ("https://en.wikiquote.org/wiki/Talk:Edsger_W._Dijkstra#Telescope", "wikipedia","svg")
         , ("https://xtools.wmflabs.org/pages/index.php?name=Gwern&lang=en&wiki=wikipedia&namespace=0&redirects=noredirects",  "wikipedia","svg")
         , ("https://samuraijack.fandom.com/wiki/Episode_XL:_Jack_vs._the_Ninja", "♡","text")
         , ("https://onlinelibrary.wiley.com/doi/full/10.1111/brv.12407",  "W","text,sans")
         , ("https://www.wired.com/2012/01/everything-about-learning/",  "wired","svg")
         , ("https://www.wired.co.uk/article/lsd-microdosing-drugs-silicon-valley", "wired","svg")
         , ("https://www.cdc.gov/nchs/nvss/births.htm",  "CDC","text,tri")
         , ("https://www.dailymail.co.uk/health/article-2126761/Bertold-Wiesner-British-scientist-fathered-600-children-donating-sperm-fertility-clinic.html", "𝔐", "text")
         , ("https://www.cell.com/ajhg/fulltext/S0002-9297(18)30405-1",  "CELL","text,quad,sans")
         , ("https://www.edge.org/conversation/alex_sandy_pentland-the-human-strategy",  "E","text,italic")
         , ("https://www.fanfiction.net/s/10360716/1/The-Metropolitan-Man",  "FFN","text,tri,sans")
         , ("https://www.motherjones.com/kevin-drum/2018/02/an-updated-lead-crime-roundup-for-2018/",  "MJ","text,sans")
         , ("https://www.nber.org/papers/w16082",  "NBER","text,quad")
         , ("https://www.npr.org/2011/04/16/135450214/eight-is-too-much-for-short-sleepers", "npr", "text,tri,sans")
         , ("https://text.npr.org/974534021", "npr", "text,tri,sans")
         , ("https://www.vanityfair.com/news/2012/10/michael-lewis-profile-barack-obama",  "VF","text")
         , ("https://www.vox.com/2015/5/27/8660249/bill-gates-spanish-flu-pandemic",  "Vox","text,tri,italic")
         , ("https://www.wsj.com/articles/SB10001424052702303380004579521482247869874",  "WSJ","text,tri")
         , ("http://www.paulgraham.com/hundred.html",  "hn","svg")
         , ("https://news.ycombinator.com/item?id=10012625",  "hn","svg")
         , ("https://aino.bandcamp.com/track/--2",  "audio","svg")
         , ("https://poniesatdawn.bandcamp.com/",  "P@D","text,tri")
         , ("https://soundcloud.com/leggysalad/girls-afternoon-appointments",  "audio","svg")
         , ("https://www.youtube.com/watch?v=cG7v9eCq2u4&t=33m49s",  "youtube","svg")
         , ("https://www.youtube.com/channel/UCeNwyKuv5SMnN6ovlpbz1SQ",  "youtube","svg")
         , ("https://vimeo.com/28735982", "file-video", "svg")
         , ("https://yunnansourcing.com/",  "ys","text")
         , ("https://yunnansourcing.us/", "ys","text")
         , ("https://what-if.xkcd.com/145/",  "XKCD","text,quad,sans")
         , ("/docs/cat/catnip/survey/2017-07-30-gs-pilot.csv",  "csv","svg")
         , ("/docs/iq/2014-tenijenhuis-supplement.doc",  "worddoc","svg")
         , ("/docs/genetics/heritable/2015-mosing-supplement.docx",  "worddoc","svg")
         , ("/docs/dual-n-back/2012-zhong.ebt",  "misc","svg")
         , ("/docs/ai/1992-dreyfus-whatcomputerstillcantdo.epub",  "EPUB","text,quad,sans")
         , ("/docs/touhou/2013-c84-downloads.json",  "txt","svg")
         , ("/docs/ai/anime/danbooru/2020-06-08-danbooru2019-palm-handannotations-export.jsonl",  "txt","svg")
         , ("/docs/touhou/2013-06-08-acircle-tohoarrange.mdb",  "misc","svg")
         , ("/docs/cs/linkrot/2009-08-20-b3ta-fujitsuhtml.mht",  "misc","svg")
         , ("/docs/psychology/2019-01-21-eric-socksurvey.ods",  "spreadsheet","svg")
         , ("/docs/personal/rss-subscriptions.opml",  "txt","svg")
         , ("/Lorem.page",  "txt","svg")
         , ("/docs/ai/poetry/2019-10-17-117m-poetry-cleanprojectgutenberg-samples.txt",  "txt","svg")
         , ("/static/font/drop-cap/de-zs/DeutscheZierschrift-M.ttf",  "misc","svg")
         , ("/docs/creatine/2009-ling-data.xls",  "spreadsheet","svg")
         , ("/docs/cs/2010-nordhaus-nordhaus2007twocenturiesofproductivitygrowthincomputing-appendix.xlsx",  "spreadsheet","svg")
         , ("/docs/personal/google-cse.xml",  "google","svg")
         , ("https://docs.google.com/document/d/1MhA3M5ucBD7ZXcWk57_MKZ5jEgPX6_YiKye_EFP-adg/edit",  "worddoc","svg")
         , ("/docs/ai/1986-michie-onmachineintelligence.pdf#page=99",  "pdf","svg")
         , ("/docs/ai/1962-bryson.pdf",  "pdf","svg")
         , ("https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.108.7127&rep=rep1&type=pdf",  "pdf","svg")
         , ("https://pdfs.semanticscholar.org/00d3/6b267777b670abd1a3b98a21bf662245a7c4.pdf",  "pdf","svg")
         , ("https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.75.2289&rep=rep1&type=pdf",  "pdf","svg")
         , ("/docs/rotten.com/library/bio/hackers/robert-morris/morris.bmp",  "image","svg")
         , ("/docs/rotten.com/library/sex/sexual-ethics-in-psychology/55403_m.gif",  "image","svg")
         , ("/static/img/favicon.ico",  "image","svg")
         , ("/docs/anime/eva/2010-1000enpark-tokyo-oota-heiwajimakoen.jpg",  "image","svg")
         , ("/images/iq/2011-gensowski-figure7-totaleffectofiqandpersonalityonlifetimeearnings.png",  "image","svg")
         , ("/static/img/icons/video.svg",  "image","svg")
         , ("/docs/personal/businesscard-front-draft.xcf",  "image","svg")
         , ("https://i.imgur.com/3Jb0b.jpg",  "image","svg")
         , ("/static/build/linkAbstract.R",  "code","svg")
         , ("/static/css/links.css",  "code","svg")
         , ("/static/build/hakyll.hs",  "code","svg")
         , ("https://wiki.haskell.org/Xmonad/Config_archive/Gwern's_xmonad.hs",  "code","svg")
         , ("/static/templates/default.html",  "code","svg")
         , ("/static/js/sidenotes.js",  "code","svg")
         , ("/docs/ai/music/2019-12-22-gpt2-preferencelearning-gwern-abcmusic.patch",  "code","svg")
         , ("/static/build/markdown-lint.sh",  "code","svg")
         , ("/static/build/build_css.php",  "code","svg")
         , ("http://www.metafor-project.org/doku.php",  "R","text")
         , ("/static/nginx/twdne.conf",  "code","svg")
         , ("/docs/zeo/firmware-v2.6.3R-zeo.img",  "archive","svg")
         , ("https://hivemind-repo.s3-us-west-2.amazonaws.com/twdne3/twdne3.onnx",  "archive","svg")
         , ("/docs/psychology/spaced-repetition/michaellee-memoryretentionexperiments-data.tar",  "archive","svg")
         , ("/docs/ai/nn/rnn/2015-06-03-karpathy-charrnn-visualization.tar.xz",  "archive","svg")
         , ("/docs/ai/anime/danbooru/2019-02-10-stylegan-holo-handselectedsamples.zip",  "archive","svg")
         , ("/docs/darknet-markets/usareshipper-profile.maff", "archive","svg")
         , ("/images/cs/2017-reddit-dhieno-theplace-timelapseevolution.mp4",  "file-video","svg")
         , ("http://iqtest.dk/main.swf",  "file-video","svg")
         , ("/images/tea/tea-mineralwaters-bestarm-sequential.webm",  "file-video","svg")
         , ("/docs/ai/music/2020-03-06-fifteenai-fluttershy-sithcode.mp3",  "audio","svg")
         , ("/docs/rotten.com/library/culture/batman/theme-song/batmantv.rm",  "audio","svg")
         , ("/docs/rotten.com/library/bio/entertainers/comic/david-letterman/letterman_any_sense.wav",  "audio","svg")
         , ("http://silkroadvb5piz3r.onion/index.php/silkroad/user/69a6bec290", "SR1", "text,sans")
         , ("http://silkroad5v7dywlc.onion/index.php?topic=2889.0", "SR1", "text,sans")
         , ("https://beepb00p.xyz/pkm-search.html", "\129302", "text")
         , ("https://antilop.cc/sr/#assassination_plot", "෴", "text")
         , ("https://www.memteaimports.com/tea/fern-stream-amber-oolong", "MT", "text,sans")
         , ("https://forum.effectivealtruism.org/posts/nSot23sAjoZRgaEwa/2016-ai-risk-literature-review-and-charity-comparison", "EA", "text")
         , ("https://www.effectivealtruism.org/articles/prospecting-for-gold-owen-cotton-barratt#heavy-tailed-distributions", "EA", "text")
         , ("https://boards.fireden.net/a/thread/185257999/", "4CH", "text,sans")
         , ("https://boards.4channel.org/jp/", "4CH", "text,sans")
         , ("http://archive.foolz.us/a/thread/77196171/#77207238", "4CH", "text,sans")
         , ("https://www.kaggle.com/ultrajack/modern-renaissance-poetry", "k", "text,sans")
         , ("https://www.jneurosci.org/content/32/12/4156.full", "JN", "text")
         , ("https://www.discovermagazine.com/mind/the-brain-a-body-fit-for-a-freaky-big-brain", "D", "text")
         , ("https://tl.net/blogs/283221-worker-rush-part-4-rising-up?view=all", "TL", "text,sans")
         , ("https://www.businessinsider.com/this-is-what-happens-when-you-track-your-sleep-obsessively-2012-2", "BI", "text,sans")
         , ("https://gameprogrammingpatterns.com/singleton.html", "GPP", "text,tri,sans")
         , ("https://dnstats.net/market/Amazon+Dark", "dn", "text,sans")
         , ("https://www.newsweek.com/gene-editing-chinese-scientist-he-jiankui-missing-house-arrest-1240749", "NW", "text")
         , ("https://www.thecut.com/2019/05/the-tinder-hacker.html", "TC", "text")
         , ("https://www.scientificamerican.com/article/the-mind-of-an-octopus/", "SA", "text")
         , ("https://www.metopera.org/season/2019-20-season/madama-butterfly/", "Met", "text,tri")
         , ("https://www.imdb.com/title/tt0923592/", "IMDb", "text,sans,quad")
         , ("https://www.nejm.org/doi/full/10.1056/NEJM199604043341416", "NEJM", "text,quad")
         , ("https://groups.yahoo.com/group/givewell/message/287", "GW", "text")
         , ("https://files.givewell.org/files/DWDA%202009/Interventions/Iodine/Bautista%20et%20al%201982.pdf", "GW", "text")
         , ("https://blog.givewell.org/2012/08/23/how-we-evaluate-a-study/", "GW", "text")
         , ("https://www.givewell.org/giving101", "GW", "text")
         , ("https://www.stuff.co.nz/manawatu-standard/news/69472334/kiwi-man-jailed-for-posting-drugs-from-las-vegas-to-mothers-house", "NZ", "text,sans")
         , ("https://www.schneier.com/blog/archives/2011/08/terrorism_in_th.html", "SOS", "text,tri,sans")
         , ("https://www.ribbonfarm.com/2011/09/23/the-milo-criterion/", "ℝ𝔽", "text,sans")
         , ("https://en.touhouwiki.net/wiki/Iyokan", "☯", "text")
         , ("https://touhou.fandom.com/wiki/Category:Music", "☯", "text")
         , ("https://old.reddit.com/r/TOUHOUMUSIC/search?q=author%3Agwern&sort=new&restrict_sr=on&t=all", "☯", "text")
         , ("https://w.atwiki.jp/toho/pages/948.html", "☯", "text")
         , ("https://www.reuters.com/article/us-astrazeneca-targacept/astrazeneca-targacept-drug-fails-depression-test-idUSTRE7A71KO20111108", "R", "text,sans")
         , ("https://www.deviantart.com/caji9i/art/stylegan-neural-ahegao-842847987", "DA", "text,sans")
         , ("https://boardgamegeek.com/boardgame/148931/coup-reformation", "BGG", "text,tri,sans")
         , ("http://thehub7dnl5nmcz5.onion/index.php?topic=2261.msg17459", "Hub", "text,tri,sans")
         , ("https://www.telegraph.co.uk/culture/books/3601644/Adultery-was-his-thing.html", "the-telegraph", "svg")
         , ("https://www.smithsonianmag.com/history/native-intelligence-109314481/", "SM", "text")
         , ("https://www.openphilanthropy.org/brain-computation-report", "open-philanthropy", "svg")
         , ("https://scienceblogs.com/clock/2006/10/16/what-is-a-natural-sleep-patter", "Sᵇ", "text,sans,italic")
         , ("http://chronopause.com/chronopause.com/index.php/2011/08/05/science-fiction-double-feature-2-part-2/index.html", "M.D.", "text,sans")
         , ("https://www.theage.com.au/national/victoria/bitcoin-drug-millions-seized-in-victoria-20141015-116bby.html", "A", "text")
         , ("https://www.rifters.com/real/2009/01/iterating-towards-bethlehem.html", "P.W.", "text,sans")
         , ("https://www.abc.net.au/news/2013-08-23/police-turn-attention-to-online-drug-trade/4908264", "ABC", "text,tri,sans")
         , ("https://ieeexplore.ieee.org/xpls/abs_all.jsp?arnumber=602492", "IEEE", "text,mono,quad")
         , ("https://spectrum.ieee.org/classical-chinese", "IEEE", "text,mono,quad")
         , ("https://www.dailydot.com/crime/dark-web-black-market-reloaded-adam-bunger-gun-sales-arrest/", "D.", "text,sans")
         , ("https://www.yudkowsky.net/rational/technical", "yud", "svg")
         , ("https://www.johndcook.com/blog/2010/09/13/applied-topology-and-dante-an-interview-with-robert-ghrist/", "JC", "text,sans")
         , ("https://www.candyjapan.com/2013-year-in-review", "🍬", "text")
         , ("http://journals.cambridge.org/production/action/cjoGetFulltext?fulltextid=1463440", "⛨", "text")
         , ("https://static.cambridge.org/binary/version/id/urn:cambridge.org:id:binary-alt:20181009171208-81978-mediumThumb-S0033291718001873_fig1g.jpg?pub-status=live", "⛨", "text")
         , ("https://www.cambridge.org/core/journals/journal-of-economic-history/article/two-centuries-of-productivity-growth-in-computing/856EC5947A5857296D3328FA154BA3A3", "⛨", "text")
         , ("https://www.atlasobscura.com/articles/cyoa-choose-your-own-adventure-maps", "atlas-obscura", "svg")
         , ("http://www.antipope.org/charlie/blog-static/2007/03/why_the_commercial_ebook_marke.html", "C.S.", "text,sans")
         , ("http://rspb.royalsocietypublishing.org/content/284/1851/20162562", "RS", "text")
         , ("http://rstb.royalsocietypublishing.org/content/365/1537/73.full", "RS", "text")
         , ("https://royalsocietypublishing.org/doi/10.1098/rsos.181393", "RS", "text")
         , ("https://nautil.us/mapping-the-human-exposome-10595/", "nautilus", "svg")
         , ("http://www.sequentialtart.com/archive/july00/grant.shtml", "ST", "text,sans")
         , ("https://www.psychologytoday.com/us/blog/life-bilingual/201906/the-bilingual-advantage-three-years-later", "PT", "text,sans")
         , ("https://www.odt.co.nz/news/dunedin/student-drug-dealer-jailed", "ODT", "text,tri")
         , ("https://www.independent.co.uk/news/uk/this-britain/the-jousting-accident-that-turned-henry-viii-into-a-tyrant-1670421.html", "TI", "text")
         , ("https://www.fastcompany.com/40438376/after-a-comeback-23andme-faces-its-next-test", "FC", "text")
         , ("https://rjlipton.wordpress.com/2015/07/28/playing-chess-with-the-devil/", "P = NP", "text,quad")
         , ("https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node13.html", "SI CP", "text,quad,sans")
         , ("https://mitpress.mit.edu/books/book-ground", "MIT", "text,tri,mono")
         , ("https://blog.eleuther.ai/announcing-20b/", "eleutherai", "svg")
         , ("https://blog.eleuther.ai/year-one/", "eleutherai", "svg")
         , ("https://6b.eleuther.ai/", "eleutherai", "svg")
         , ("https://www.eleuther.ai/projects/gpt-neo/", "eleutherai", "svg")
         , ("https://pile.eleuther.ai/", "eleutherai", "svg")
         , ("https://arankomatsuzaki.wordpress.com/2021/06/04/gpt-j/", "eleutherai", "svg")
         , ("https://knowyourmeme.com/memes/navy-seal-copypasta", "KYM", "text,tri")
         , ("https://apps.apple.com/app/id307920888", "apple", "svg")
         , ("https://machinelearning.apple.com/research/hey-siri", "apple", "svg")
         , ("https://jaspervdj.be/hakyll/reference/Hakyll-Web-Redirect.html", "JVDJ", "text,quad,mono")
         , ("https://gizmodo.com/weird-and-wonderful-movies-that-youll-never-get-to-see-5877874", "GIZM", "text,quad,mono")
         , ("https://www.edwardtufte.com/bboard/images/0000yO-774.gif", "ET", "text")
         , ("/docs/design/typography/rubrication/1990-tufte-envisioninginformation-ch5-byrneseuclid.pdf", "ET", "text")
         , ("https://cran.r-project.org/web/packages/tufte/index.html", "ET", "text")
         , ("https://edwardtufte.github.io/tufte-css/#epigraphs", "ET", "text")
         , ("https://github.com/clayh53/tufte-jekyll", "ET", "text")
         , ("https://github.com/edwardtufte/tufte-css", "ET", "text")
         , ("https://github.com/jez/tufte-pandoc-jekyll", "ET", "text")
         , ("https://freakonomics.com/2007/05/what-do-you-have-to-say-about-ron-paul/", "FRK", "text,tri,sans")
         , ("https://elifesciences.org/articles/16351", "eL", "text,sans")
         , ("https://www.w3.org/International/wiki/Case_folding", "W3", "text,sans")
         , ("https://www.hpmor.com/notes/progress-report-2013-11-01/", "MoR", "text,tri,italic")
         , ("https://www.fanfiction.net/r/5782108/5/1/", "MoR", "text,tri,italic")
         , ("https://old.reddit.com/r/HPMOR/", "MoR", "text,tri,italic")
         , ("https://www.hpmor.com/chapter/2", "MoR", "text,tri,italic")
         , ("https://ask.metafilter.com/16136/Fog-Gun-Shower", "MF", "text,sans,italic")
         , ("https://www.metafilter.com/183095/On-having-sufficient-complexity-to-allow-for-arbitrary-computation", "MF", "text,sans,italic")
         , ("https://www.quantamagazine.org/the-busy-beaver-game-illuminates-the-fundamental-limits-of-math-20201210/", "quanta", "svg")
         , ("https://qz.com/1028528/custos-startup-uses-bitcoin-bounties-to-make-pirates-rat-on-one-another/", "QZ", "text,sans")
         , ("https://blog.23andme.com/23andme-research/you-scream-i-scream-our-genes-scream-for-ice-cream/", "23", "text")
         , ("https://www.ft.com/content/009050e4-75ea-11e2-9891-00144feabdc0", "FT", "text")
         , ("https://techcrunch.com/2013/02/23/the-chinese-are-coming-the-chinese-are-coming/", "TC", "text,mono")
         , ("https://www.nzherald.co.nz/nz/drug-mail-or-mule-risks-the-same/QHX3IGRINL7AN5QZR3JRSOQ3NA/", "𝕳", "text")
         , ("https://aiimpacts.org/2019-recent-trends-in-gpu-price-per-flops/", "AII", "text,tri")
         , ("http://archive.recapthelaw.org/paed/203025/", "PACR", "text,quad")
         , ("https://www.courtlistener.com/docket/16288633/1/united-states-v-takowsky/", "PACR", "text,quad")
         , ("https://nintil.com/epigenetic-clocks", "𝓝", "text")
         , ("https://blottyparchment.livejournal.com/7541.html?thread=233845", "LJ", "text,sans")
         , ("https://boingboing.net/2011/02/03/cosmic-commodities-h.html", "bb", "text,mono")
         , ("https://nymag.com/intelligencer/2018/07/how-fortnite-became-the-most-popular-video-game-on-earth.html", "𝒩𝒴", "text")
         , ("https://www.sfgate.com/bayarea/article/Test-lab-called-1-billion-over-budget-2921620.php", "SFG", "text,tri,sans")
         , ("https://www.nybooks.com/articles/2020/01/16/alma-mahler-it-had-to-be-her/", "NYRB", "text,quad")
         , ("https://www.newscientist.com/article/2133095-boom-in-human-gene-editing-as-20-crispr-trials-gear-up/", "NS", "text,sans")
         , ("https://www.mirror.co.uk/news/uk-news/first-picture-teenager-accused-plotting-6124856", "M", "text,sans")
         , ("https://www.brookings.edu/research/expectations-of-sustained-effects-from-scaled-up-pre-k-challenges-from-the-tennessee-study/", "B", "text")
         , ("https://variety.com/2014/film/news/tokyo-festival-hideaki-anno-warns-of-trouble-ahead-for-japanese-animation-1201339991/", "𝓥", "text")
         , ("http://unenumerated.blogspot.com/2011/01/singularity.html", "N.S.", "text,sans")
         , ("https://web.archive.org/web/20110724123419/szabo.best.vwh.net/bearer_contracts.html", "N.S.", "text,sans")
         , ("https://scottaaronson.blog/?p=1438", "S.A.", "text,sans")
         , ("https://www.scottaaronson.com/democritus/", "S.A.", "text,sans")
         , ("https://theconversation.com/altruism-in-birds-magpies-have-outwitted-scientists-by-helping-each-other-remove-tracking-devices-175246", "🗨", "text")
         , ("https://patch.com/california/davis/davis-pair-arrested-after-cops-intercept-3-000-suspected-ecstasy-pills-mail-serve", "P", "text,sans")
         , ("http://www.jstor.org/stable/10.1086/468061", "JTOR", "text,quad")
         , ("https://scp-wiki.wikidot.com/antimemetics-division-hub", "SCP", "text,tri,sans")
         , ("https://www.ieee-security.org/TC/SPW2014/papers/5103a209.PDF", "pdf", "svg")
         , ("https://thisanimedoesnotexist.ai/", "TADE", "text,quad,sans")
         , ("https://www.thiswaifudoesnotexist.net/", "TWDE", "text,quad,sans")
         , ("https://www.thisfursonadoesnotexist.com/", "TFDE", "text,quad,sans")
         , ("https://thisponydoesnotexist.net/", "TPDE", "text,quad,sans")
         , ("https://aidungeon.medium.com/introducing-ai-dungeon-translate-a50e35f6df83", "AID", "text,tri,sans")
         , ("https://latitude.io/blog/how-we-accidentally-gave-our-bots-their-personalities/", "AID", "text,tri,sans")
         , ("https://old.reddit.com/r/AIDungeon/comments/i1qhg0/the_dragon_ai_just_got_worse/", "AID", "text,tri,sans")
         , ("https://old.reddit.com/r/AIDungeon/comments/i1qhg0/the_dragon_ai_just_got_worse/", "AID", "text,tri,sans")
         , ("https://www.patreon.com/AIDungeon", "AID", "text,tri,sans")
         , ("https://www.fimfiction.net/story/62074/Friendship-is-Optimal", "FIMF", "text,quad,mono")
         , ("https://magenta.tensorflow.org/music-transformer", "google", "svg")
         , ("https://www.gq.com/story/the-last-true-hermit", "GQ", "text,sans")
         , ("https://bls.gov/news.release/archives/ecec_031986.pdf", "BLS", "text,sans")
         , ("https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=100&year1=1992&year2=2011", "BLS", "text,sans")
         , ("https://www.bls.gov/cps/duration.htm", "BLS", "text,sans")
         , ("https://thegradient.pub/gpt2-and-the-nature-of-intelligence/", "∇", "text")
         , ("https://creativecommons.org/licenses/by-nc/3.0/", "creative-commons", "svg")
         , ("http://www.projectrho.com/public_html/rocket/futurelang.php", "ρ", "text")
         , ("http://pcdb.santafe.edu/", "PCDB", "text,quad,sans")
         , ("https://harpers.org/archive/1954/12/the-jet-propelled-couch/?single=1", "H", "text")
         , ("https://foreignpolicy.com/2010/11/23/death-by-a-thousand-cuts-2/", "FP", "text")
         , ("https://www.unqualified-reservations.org/2007/08/james-burnhams-dante-politics-as-wish/", "UR", "text")
         , ("https://www.thenewatlantis.com/publications/correlation-causation-and-confusion", "NA", "text")
         , ("https://medium.com/tensorflow/fitting-larger-networks-into-memory-583e3c758ff9", "tensorflow", "svg")
         , ("https://playground.tensorflow.org/", "tensorflow", "svg")
         , ("https://www.tensorflow.org/tensorboard/get_started", "tensorflow", "svg")
         , ("https://www.supermemo.com/en/archives1990-2015/articles/20rules", "SM", "text,sans")
         , ("https://qwantz.com/index.php?comic=1896", "DC", "text,sans")
         , ("https://vndb.org/c582", "VNDB", "text,quad,sans")
         , ("https://qualiacomputing.com/2015/05/22/how-to-secretly-communicate-with-people-on-lsd/", "QC", "text,sans")
         , ("http://www.thelancet.com/journals/lancet/article/PIIS0140-6736%2811%2960693-4/abstract", "L", "text")
         , ("https://www.nngroup.com/articles/aesthetic-usability-effect/", "NN", "text,sans")
         , ("https://replicationindex.com/2016/04/18/is-replicability-report-ego-depletionreplicability-report-of-165-ego-depletion-articles/", "RI", "text,sans")
         , ("https://au.news.yahoo.com/brothers-in-court-over-online-drugs-14980079.html", "Y!", "text,sans")
         , ("https://vitalik.ca/general/2017/09/14/prehistory.html", "V.B.", "text,sans")
         , ("/docs/economics/2018-buterin.pdf", "V.B.", "text,sans")
         , ("https://github.com/huggingface/transformers", "\129303", "text")
         , ("https://huggingface.co/calculator/", "\129303", "text")
         , ("https://medium.com/huggingface/distilbert-8cf3380435b5", "\129303", "text")
         , ("https://forum.quantifiedself.com/t/indoor-air-quality-monitoring-health/799/40", "QS", "text,sans")
         , ("https://quantifiedself.com/2014/04/eric-jain-sleep-moon-phases/", "QS", "text,sans")
         , ("https://old.reddit.com/r/QuantifiedSelf/comments/1mfn0a/trying_to_detect_modafinils_stimulant_effect/", "QS", "text,sans")
         , ("https://www.alcor.org/library/alcor-membership-statistics/", "alcor", "svg")
         , ("https://arankomatsuzaki.wordpress.com/2021/03/04/state-of-the-art-image-generative-models/", "ak", "text,sans")
         , ("https://www.nap.edu/catalog/25762/reflecting-sunlight-recommendations-for-solar-geoengineering-research-and-research-governance", "NAP", "text,tri")
         , ("https://pastebin.com/GrV3uYh5", "txt", "svg")
         , ("http://archives.cnn.com/2000/HEALTH/aging/04/19/hearing.loss.wmd/index.html", "CNN", "text,tri,sans")
         , ("https://lwn.net/Articles/286233/", "LWN", "text,tri,sans")
         , ("https://www.huffpost.com/entry/bill-zeller-dead-princeto_n_805689", "HUFF", "text,quad,sans")
         , ("http://summaries.cochrane.org/CD007176/antioxidant-supplements-for-prevention-of-mortality-in-healthy-participants-and-patients-with-various-diseases", "cochrane-collaboration", "svg")
         , ("https://www.fast.ai/2018/04/30/dawnbench-fastai/", "F.ai", "text,tri")
         , ("https://github.com/fastai/numerical-linear-algebra/blob/master/README.md","F.ai", "text,tri")
         , ("https://www.esquire.com/entertainment/a36439327/planet-hollywood-origin-story-history-interview/", "ℰ", "text")
         , ("https://www.cbsnews.com/colorado/news/man-allegedly-bought-pot-from-colorado-to-sell-in-maryland/", "CBS", "text,tri,sans")
         , ("https://www.cbsnews.com/news/california-biobank-dna-babies-who-has-access/", "CBS", "text,tri,sans")
         , ("https://nypost.com/2019/06/27/north-carolina-couple-paid-25k-to-clone-their-beloved-pet-cat/", "NYP", "text,tri,sans,italic")
         , ("https://longreads.com/2015/01/28/friendship-is-complicated/", "Long", "text,quad")
         , ("https://crookedtimber.org/2012/05/30/in-soviet-union-optimization-problem-solves-you/", "CT", "text")
         , ("http://articles.latimes.com/print/1988-07-17/magazine/tm-9636_1_x-ray-laser", "𝔏A", "text")
         , ("/images/fiction/batman/2022-04-15-manasuka-artdecobatmantriptych-batman.psd",  "image","svg")
         , ("https://warontherocks.com/2021/08/foreign-fighters-and-cheese-bells/", "WOTR", "text,quad,sans")
         , ("https://www.connectedpapers.com/main/1ffe143b40a9f8c01940c7397280de4cf666d635/Lessons-from-AlphaZero-for-Optimal%2C-Model-Predictive%2C-and-Adaptive-Control/graph", "connected-papers","svg")
         , ("https://krebsonsecurity.com/2013/07/mail-from-the-velvet-cybercrime-underground/", "Kreb", "text,quad,sans")
         , ("https://www.hustwit.com/urbanized", "H", "text,sans")
         , ("https://www.nextplatform.com/2019/08/20/big-blue-open-sources-power-chip-instruction-set/", "NEXT", "text,quad,sans")
         , ("http://www.vetta.org/2009/12/tick-tock-tick-tock-bing/", "Legg", "text,quad,sans")
         , ("https://www.spiegel.de/panorama/justiz/amokschuetze-von-muenchen-tatwaffe-aus-dem-darknet-a-1104461.html", "SPGL", "text,quad")
         , ("https://www.pragmatic.ml/sparse-sinkhorn-attention/", "𝕄", "text")
         , ("https://hbr.org/2019/12/can-you-know-too-much-about-your-organization", "HBR", "text,tri,sans")
         , ("https://thepiratebay.org/description.php?id=14045031", "the-pirate-bay", "svg")
         , ("https://history.nasa.gov/rogersrep/v2appf.htm",                                    "nasa", "svg")
         , ("https://science.nasa.gov/science-news/science-at-nasa/2005/03jun_naps/",           "nasa", "svg")
         , ("https://www.nasa.gov/mission_pages/station/expeditions/expedition30/tryanny.html", "nasa", "svg")
         , ("https://queue.acm.org/detail.cfm?ref=rss&id=2856460", "acm", "text,tri,sans")
         , ("https://cacm.acm.org/magazines/2017/8/219606-the-science-of-brute-force/fulltext", "acm", "text,tri,sans")
         , ("https://dl.acm.org/doi/10.1145/3266037.3266090", "acm", "text,tri,sans")
         , ("https://www.research.va.gov/", "VA", "text,sans")
         , ("https://link.springer.com/article/10.3758/s13423-021-01927-8", "springerlink", "svg")
         , ("https://www.cs.utexas.edu/~EWD/transcriptions/EWD03xx/EWD340.html", "EWD", "text,tri,sans")
         , ("https://iopscience.iop.org/article/10.1088/1748-9326/aabf9b", "IOP", "text,tri,sans")
         , ("https://tasvideos.org/3653M", "TASV", "text,quad")
         , ("https://www.metaculus.com/questions/notebooks/8702/the-promise-and-impact-of-the-next-generation-of-weight-loss-drugs/", "metaculus", "svg")
         , ("https://habr.com/ru/post/516190/", "Habr", "text,quad,sans")
         , ("https://sites.google.com/berkeley.edu/decision-transformer", "BAIR", "text,quad,mono")
         , ("https://ml.berkeley.edu/blog/posts/clip-art/", "BAIR", "text,quad,mono")
         , ("https://people.eecs.berkeley.edu/~janner/trajectory-transformer/files/trajectory-transformer.pdf", "BAIR", "text,quad,mono")
         , ("https://bair.berkeley.edu/blog/2020/07/11/auction/", "BAIR", "text,quad,mono")
         , ("https://wandb.ai/wandb_fc/gradient-dissent/reports/What-could-make-AI-conscious-with-Wojciech-Zaremba-co-founder-of-OpenAI--Vmlldzo3NDk3MDI", "wandb", "svg")
         , ("http://libgen.org/search.php?req=%22wheel+of+time%22", "raven", "svg")
         , ("http://libgen.rs/", "raven", "svg")
         , ("https://library.bz/main/upload/", "raven", "svg")
         , ("https://www.hoover.org/research/optimistic-thought-experiment", "hoover-institution", "svg")
         , ("https://80000hours.org/podcast/episodes/sam-bankman-fried-high-risk-approach-to-crypto-and-doing-good/", "80k", "text,tri,sans")
         , ("https://retractionwatch.com/2011/02/28/crystal-myth-11-more-retractions-from-crystallography-journal-after-2010-fakery/", "magnifying-glass", "svg")
         , ("https://www.statnews.com/2021/11/09/largest-psilocybin-trial-finds-psychedelic-effective-treating-serious-depression/", "stat-news", "svg")
         , ("https://apnews.com/269b3de1af34e17c1941a514f78d764c", "AP", "text,sans")
         , ("https://people.idsia.ch/~juergen/creativity.html", "SMDH", "text,quad,sans")
         , ("https://arxiv.org/abs/1404.7828#schmidhuber", "SMDH", "text,quad,sans")
         , ("https://innsbigdata.wordpress.com/2015/02/09/interview-with-juergen-schmidhuber/", "SMDH", "text,quad,sans")
         , ("/docs/ai/nn/rnn/1991-schmidhuber.pdf", "SMDH", "text,quad,sans")
         , ("https://www.bloomberg.com/news/features/2018-05-15/google-amazon-and-facebook-owe-j-rgen-schmidhuber-a-fortune","SMDH", "text,quad,sans")
         , ("https://www.nytimes.com/2016/11/27/technology/artificial-intelligence-pioneer-jurgen-schmidhuber-overlooked.html", "SMDH", "text,quad,sans")
         , ("https://www.ssc.wisc.edu/wlsresearch/about/description.php", "WLS", "text,tri,sans")
         , ("http://host.robots.ox.ac.uk/pascal/VOC/", "VOC", "text,tri,sans")
         , ("https://www.anthropic.com/news/announcement", "anthropic", "svg")
         , ("https://transformer-circuits.pub/2022/in-context-learning-and-induction-heads/index.html#anthropic", "anthropic", "svg")
         , ("https://arxiv.org/abs/2207.05221#anthropic", "anthropic", "svg")
         , ("https://www.teds.ac.uk/about-teds", "TEDS", "text,quad,sans")
         , ("https://www.tandfonline.com/doi/abs/10.1080/02783190209554137", "T&F", "text,tri,sans")
         , ("https://omega0.xyz/omega8008/JaynesBookPdf.html", "ETJ", "text,tri,sans")
         , ("/docs/statistics/bayes/1988-jaynes-maximumentropyandbayesianmethods.pdf", "ETJ", "text,tri,sans")
         , ("http://www-biba.inrialpes.fr/Jaynes/cc18i.pdf", "ETJ", "text,tri,sans")
         , ("https://paperswithcode.com/method/dilated-convolution", "PwC", "text,tri,sans")
         , ("https://stability.ai/blog/stable-diffusion-public-release", "SD", "text,sans")
         , ("https://patrickcollison.com/labs", "PC", "text,sans")
         , ("https://www.pewresearch.org/social-trends/2012/02/16/the-rise-of-intermarriage/", "Pew", "text,tri")
        ]
