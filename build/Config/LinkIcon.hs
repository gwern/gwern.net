{-# LANGUAGE OverloadedStrings #-}
module Config.LinkIcon (prioritizeLinkIconMin, prioritizeLinkIconBlackList, overrideLinkIcons, linkIconTestUnitsText, linkIconRules) where

import qualified Data.Text as T (drop, isInfixOf, isPrefixOf, Text)
import Utils (extension, isLocal, hasExtension, isHostOrArchive)

-- hardwire globally icons for exact-matches of specific URLs (`[(URL, (Link icon, Link icon type))]`), in cases where we can't or won't edit the link directly to set link-icons (eg. in /fiction/clippy, I do a manual override of link-icons to rickroll the reader: `[HQU Colab notebook](https://​tinyurl.com/hquv34 "Colab notebook: HQU-v3.4-light (Jax TPU)"){link-icon="alphabet" link-icon-type="svg" .link-live-not .archive-not}`)
overrideLinkIcons :: [(T.Text, (T.Text,T.Text))]
overrideLinkIcons = [("/index#abstract", ("",""))]

------------------------------------------------------------------------------------------

-- don't bother trying to come up with link-icons until a domain has been used repeatedly:
prioritizeLinkIconMin :: Int
prioritizeLinkIconMin = 4

prioritizeLinkIconBlackList :: [T.Text] -- dead, icon-less, bad icon, overly-obscure, no real unifying nature worth knowing, etc:
prioritizeLinkIconBlackList = ["lilianweng.github.io", "digital.library.unt.edu", "www.smartpowders.com", "www.silverhandmeadery.com",
                     "forums.animesuki.com", "philip.greenspun.com", "eli.thegreenplace.net", "danluu.com", "www.theregister.com",
                     "www.thedailybeast.com", "www.straighttalkonevidence.org", "www.joelonsoftware.com",
                     "www.jstage.jst.go.jp", "blog.codinghorror.com", "intrade.com", "abandonedfootnotes.blogspot.com", "arr.am",
                     "ascii.textfiles.com", "blog.johantibell.com", "humanvarieties.org", "ilovetypography.com",
                     "cognitivefun.net", "findarticles.com", "dataprivacylab.org", "www.thefreelibrary.com", "www.unitedpharmacies-uk.md",
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
                     "mathworld.wolfram.com", "soranews24.com", "caniuse.com", "www.silcom.com", "esolangs.org",
                     "www.aiweirdness.com", "etherscan.io", "www.theringer.com", "cs.stanford.edu", "mmlab.ie.cuhk.edu.hk", "www.cs.toronto.edu",
                     "www.centauri-dreams.org", "www.alexirpan.com", "linuxmafia.com", "wiki.obormot.net", "www.marxists.org",
                     "takimag.com", "oll.libertyfund.org", "every.to", "www.eoht.info", "mssprovenance.blogspot.com",
                     "www.acpjournals.org", "www.inverse.com", "hal.science", "www.findarticles.com", "super.gluebenchmark.com", "gluebenchmark.com",
                     "mattmahoney.net", "dataverse.harvard.edu", "projecteuclid.org", "datacolada.org", "pubs.aip.org", "nyaa.si", "memteaimports.com",
                     "jetpress.org", "www.sudowrite.com", "tylervigen.com", "pubs.acs.org", "www.dafont.com", "geminiprotocol.net",
                     "www.1001fonts.com", "andrewmayne.com", "www.benkuhn.net", "sive.rs", "itre.cis.upenn.edu", "conservancy.umn.edu", "www.crd.york.ac.uk"]
------------------------------------------------------------------------------------------

-- Helper functions for URL matches:
u', u'' :: T.Text -> T.Text -> Bool
-- Loose check: simplest check for string anywhere; note that if it is a full domain name like `https://foo.com` (intended to match `https://foo.com/xyz.html`), then it will *not* match when the local-archive code fires and the URL gets rewritten to "/doc/foo.com/$HASH.html". So we error out if the user tries this, having forgotten that u' ≠ u'' in that respect.
u' url v = if "http://" `T.isPrefixOf` v || "https://" `T.isPrefixOf` v then error ("LinkIcon.hs: Overly strict prefix in infix matching (u'): " ++ show url ++ ":" ++ show v) else
  v `T.isInfixOf` url
-- Stricter check: more stringent check, matching exactly the domain name:
u'' url v = if "http://" `T.isPrefixOf` v || "https://" `T.isPrefixOf` v then error ("LinkIcon.hs: Overly strict prefix in infix matching (u''): " ++ show url ++ ":" ++ show v) else
          isHostOrArchive v url

iE :: T.Text -> [T.Text] -> Bool
iE url = elem (T.drop 1 $ extension url)
aU', aU'' :: T.Text -> [T.Text] -> Bool
aU'  url = any (u' url)
aU'' url = any (u'' url)

-- The URL matches:
linkIconRules, linkIconRulesOverrides, linkIconRulesSingle, linkIconRulesDouble, linkIconRulesTriple, linkIconRulesQuad,
  linkIconRulesSVG, linkIconRulesFiletypes :: T.Text -> (T.Text, T.Text)
-- run all the rules in order, and take the first one that returns a non-empty tuple (because it matched):
linkIconRules "" = error "Config.LinkIcon: linkIconRules: passed an empty URL; this should be impossible!"
linkIconRules u = let result = filter (/=("","")) $ map (\f -> f u) [linkIconRulesOverrides, linkIconRulesSingle, linkIconRulesDouble,
                                                                linkIconRulesTriple, linkIconRulesQuad, linkIconRulesSVG, linkIconRulesFiletypes]
                               in if null result then ("","") else head result

-- organizational mentions or affiliations take precedence over domain or filetypes; typically matches anywhere in the URL. This must be matched first.
linkIconRulesOverrides u
 | u' u "deepmind"  = ("deepmind", "svg") -- DeepMind; match articles or anchors about DM too. Primary user: <deepmind.com>, <deepmind.google>, DM papers on Arxiv (`#deepmind` & `org=deepmind`)
 | u' u "schmidhuber" || u' u "people.idsia.ch/~juergen/" = ("SMDH", "text,quad,sans") -- Jürgen Schmidhuber homepage & tagged links; should override Arxiv, Bloomberg, NYT, Facebook etc
 | u' u "facebook" || u' u ".fb.com"  = ("facebook", "svg")
 | u' u "sites.google.com/berkeley.edu" || aU'' u ["ml.berkeley.edu", "people.eecs.berkeley.edu", "bair.berkeley.edu"] = ("BAIR", "text,quad,mono") -- Berkeley AI Research: Chelsea Finn, Sergey Levine, robotics, Decision Transformer, etc. Overrides Google Sites.
 | u' u "pandoc" && not (u' u "tufte") = ("PNDC", "text,quad,sans") -- general catch-all, overrides Google Groups (`pandoc-discuss`), with a further override for Tufte-related stuff (which get 'ET')
 | aU'' u ["www.edwardtufte.com", "edwardtufte.github.io"] || aU' u ["github.com/edwardtufte/", "github.com/jez/tufte-pandoc-jekyll", "github.com/jez/tufte", "github.com/clayh53/tufte", "/doc/design/typography/rubrication/1990-tufte-envisioninginformation-ch5-byrneseuclid.pdf", "cran.r-project.org/web/packages/tufte/"] = ("ET", "text") -- override CRAN
 | u'' u "groups.google.com" = ("✉", "text")
 | u'' u "scholar.google.com" = ("google-scholar", "svg") -- Google Scholar.
 | u'' u "docs.google.com" = ("word-doc", "svg")
 | u'' u "www.theinformation.com" = ("the-information", "svg") -- <https://en.wikipedia.org/wiki/The_Information_(website)> <https://en.wikipedia.org/wiki/File:The_Information_logo.svg> <https://ti-assets.theinformation.com/assets/favicon_prod/safari-pinned-tab-bef60b8749b324326ffc2c49b9f5ab190b1ab3e10c5ecd33bbc710838bc84e72.svg> Some sort of Greek capital letter 'I'? Overrides Microsoft & other tech companies
 | u'' u "www.semafor.com" = ("SMFR", "text,quad") -- Semafor <https://www.semafor.com/> <https://en.wikipedia.org/wiki/Semafor_(website)>; somewhat like _The Information_; official logo is a boring serif wordmark (<https://en.wikipedia.org/wiki/File:Semafor_logo.png>), and the favicon is an interesting 'cut off' 'S'-silhouette-in-square <https://www.semafor.com/safari-pinned-tab-icon.svg> but they've done a bad enough job branding it no one would recognize it, so we use a quad-abbreviation of 'SEMAFOR'
 | u' u "google" || u'' u "magenta.tensorflow.org" = ("alphabet", "svg") -- Google searches, other tools. Note that there are many Google subdomains, which we may wish to iconify differently, so we narrow down with just ‘www’. Google Brain doesn’t have any consistent or recognizable logo, don’t bother trying to replicate one of the dots (no one will recognize it); use ‘GB’ would not be a bad idea, but I suspect that would also confuse people. So reusing the ‘G’ is the least bad option. [the SVG has been renamed 'alphabet' instead of the expected 'google' because two default uBlock lists block the regexp 'icons/google.*' as it is usually abused for social-media spamming icons]
 | aU' u ["twitter.com/sigfpe/", "blog.sigfpe.com", "github.com/dpiponi"] = ("sgfp", "text,quad,monospace") -- sigfpe/Dan Piponi: Haskell, math, computer graphics etc
 | u' u "nvidia"  || aU'' u ["nvlabs.github.io", "nv-adlr.github.io", "nv-tlabs.github.io"] = ("n", "text,sans,italic") -- Nvidia: <https://en.wikipedia.org/wiki/Nvidia#cite_note-2> yeah no. Disambiguate from Nature's "n" by italicizing (Nvidia *did* italicize the lowercase 'n' for a long time, so seems reasonable)
 | aU'' u ["gptprompts.wikidot.com"] || aU' u ["openai.com", "#openai", "org=openai"] = ("openai", "svg") -- OpenAI; match articles or anchors about OA too. primary user: openai.com, Arxiv papers. Brockman's GPT-prompts wiki is semi-official IMO.
 | aU' u ["microsoft.com", "#microsoft", "org=microsoft", "github.com/microsoft/"] = ("MS", "text,sans,italic") -- Microsoft: I don’t think <https://en.wikipedia.org/wiki/File:Microsoft_logo_(2012).svg> is all that recognizable, so make a logotype more like <https://en.wikipedia.org/wiki/File:Microsoft_logo_(1987).svg>: an italic sans "MS".
 | u' u "#anthropic" || u' u "twitter.com/jackclarkSF/" || aU'' u ["transformer-circuits.pub", "www.anthropic.com", "jack-clark.net", "/doc/ai/nn/anthropic/"] = ("anthropic", "svg") -- need to override Arxiv; handle Jack Clark (co-founder) newsletter & social media
 | u' u "#laion"  || u' u "LAION-AI" || u'' u "laion.ai" = ("laion", "svg") -- <https://laion.ai/favicon.svg>; need to override Arxiv & Github & Hugging Face
 | aU'' u ["blog.givewell.org", "www.givewell.org", "files.givewell.org"] || u' u "groups.yahoo.com/group/givewell/" = ("GW", "text") -- override Yahoo! email
 | otherwise = ("","")

linkIconRulesSingle u
 | aU'' u ["psyarxiv.com", "files.osf.io", "osf.io"] = ("ψ", "text") -- Unicode trickery icons: GREEK SMALL LETTER PSI
 | u'' u "unsongbook.com" = ("ℵ", "text") -- SSC’s book: (ℵ) ALEF SYMBOL (We use the math symbol instead of the Hebrew deliberately, to avoid triggering bizarre Hebrew bidirectional text-related layout bugs on Mac Firefox.)
 | u'' u "meltingasphalt.com" = ("▲", "text") -- Kevin Simler’s Melting Asphalt blog uses 3 triangles but that's too many, so we just use one. (▲) BLACK UP-POINTING TRIANGLE
 | u'' u "www.tinyletter.com" = ("✉", "text") -- TinyLetter’s icon, without color, isn’t memorable enough; throw in the other email services (✉) ENVELOPE
 | u'' u "groups.yahoo.com" = ("✉", "text")
 | u'' u "www.mail-archive.com" = ("✉", "text")
 | u' u "carryiton.net/chain-letter/" = ("✉", "text") -- linked only for the archive, so this is an appropriate icon
 | u'' u "www.forbes.com" = ("F", "text") -- red capital F serif
 | u'' u "fortune.com" = ("F", "text,sans") -- red capital F *sans* (good god, could Forbes/Fortune be *any more* indistinguishable or boring or bland?)
 | (u' u "haskell.org" && (extension u /= ".hs")) || u' u "haskellers.com" = ("𝛌", "text") -- Haskell: simplify logo; the double-lambda is too busy when used for link icons (𝛌) MATHEMATICAL BOLD SMALL LAMBDA primary user: hackage.haskell.org; we make an exception for .hs files hosted on Haskell.org, like config files, where the source code-ness is more relevant than the organization/domain
 | u'' u "arxiv.org" || u'' u "browse.arxiv.org" || u'' u "proceedings.mlr.press" = ("𝛘", "text") --  ArXiv: Their skull+smiley logo is too bizarre & off-putting to use, in addition to not working as a tiny monochrome image (𝛘) MATHEMATICAL BOLD SMALL CHI (bold makes it show up better when tiny); I lump in 'PMLR' ("Proceedings of Machine Learning Research") because many PMLR were just Arxiv preprints beforehand & it amounts to about the same thing, really.
 | u' u ".bloomberg.com" || u'' u "www.businessweek.com" = ("𝐁", "text") -- Bloomberg: no usable logo, just an inset-B (𝐁) MATHEMATICAL BOLD CAPITAL B
 | u' u "theatlantic.com" = ("A", "text,italic") -- The Atlantic: replicate sloping by italics
 | u'' u "www.dailymail.co.uk" = ("𝔐", "text") -- 𝔐 MATHEMATICAL FRAKTUR CAPITAL M
 | aU'' u ["danbooru.donmai.us", "derpibooru.org", "safebooru.org"] = ("❐", "text") -- ❐ U+2750 UPPER RIGHT DROP-SHADOWED WHITE SQUARE
 | u'' u "www.edge.org" = ("E", "text,italic")
 | u'' u "www.economist.com" = ("E", "text,sans") -- Economist: logo is just ‘Economist’… There is a sibling magazine <https://en.wikipedia.org/wiki/1843_(magazine)> which I don't seem to link to.
 | u'' u "www.sciencedirect.com" = ("E", "text") -- Elsevier/Sciencedirect.com: also an ‘E’
 | u'' u "www.esquire.com" = ("ℰ", "text")
 | u'' u "www.harney.com" = ("H", "text") -- The Harney & Sons logo is too fancy to scale down reasonably
 | u'' u "www.hustwit.com" = ("H", "text,sans") -- design documentarian
 | u'' u "www.longecity.org" = ("⧖", "text") -- Longecity “⧖” U+29D6 WHITE HOURGLASS UNICODE
 | u' u ".nature.com" = ("n", "text") -- Nature
 | u'' u "www.theverge.com" = ("▽", "text") -- The Verge uses a sort of delta Escher triangle-esque 'V' stylization <https://en.wikipedia.org/wiki/The_Verge> which looks like a triangle pointing down, so, ▽ WHITE DOWN-POINTING TRIANGLE (Nabla operator) &#x25BD; &#9661;
 | u'' u "www.quora.com" = ("Q", "text") -- surprisingly, no one's taken 'Q' yet
 | aU'' u ["cran.r-project.org", "www.r-project.org", "lme4.r-forge.r-project.org", "www.metafor-project.org", "rstudio.com"] || u' u "github.com/paul-buerkner/brms" = ("R", "text") -- R: at this point R Studio has taken over a lot of control of the R ecosystem, so might as well treat them as official too… primary user: cran.r-project.org
 | u'' u "www.science.org" || u'' u "sciencemag.org" = ("S", "text") -- Science is just typeset in red
 | u'' u "slate.com" = ("S", "text,sans")
 | u'' u "www.salon.com" = ("s", "text")
 | u'' u "www.technologyreview.com" = ("T", "text,sans") -- Technology Review (their logo has a little slash in it which you probably can’t see at low-res) but is otherwise just a ‘T’ so meh
 | aU'' u ["time.com", "healthland.time.com"] = ("T", "text") -- Time Magazine
 | aU'' u ["www.urth.net", "lists.urth.net", "www.wolfewiki.com"] = ("U", "text") -- Gene Wolfe mailing list; no logo; primary user: lists.urth.net
 | u' u "onlinelibrary.wiley.com" = ("W", "text,sans") -- Wiley & Sons’s ‘W’ unfortunately overlaps with the WP ‘W’ but if we sans it, maybe that’ll help. primary user: onlinelibrary.wiley.com
 | aU' u ["longbets.org", "longnow.org", "rosettaproject.org", "theinterval.org"] = ("X", "text,overline") -- Long Now Foundation projects
 | u'' u "predictionbook.com" = ("?", "text,sans,bold") -- PB logo is confusing. A purple question mark…?
 | u'' u "beepb00p.xyz" = ("\129302", "text") -- ROBOT FACE U+1F916
 | u'' u "antilop.cc" = ("෴", "text") -- SINHALA PUNCTUATION KUNDDALIYA 0x0DF4 - because it's written by "Moustache", get it
 | u'' u "forum.effectivealtruism.org" || u'' u "www.effectivealtruism.org" = ("EA", "text")
 | u'' u "boards.fireden.net" || u'' u "archive.foolz.us" || u' u "4channel.org" || u' u "boards.4chan.org"  = ("4CH", "text,sans")
 | u'' u "www.kaggle.com" = ("k", "text,sans")
 | u'' u "www.discovermagazine.com" = ("D", "text")
 | u'' u "www.mirror.co.uk" = ("M", "text,sans")
 | aU'' u ["en.touhouwiki.net", "touhou.fandom.com", "w.atwiki.jp"] || u' u "www.reddit.com/r/TOUHOUMUSIC/" = ("☯", "text") -- NOTE: override Fandom catch-all
 | u'' u "www.reuters.com" = ("R", "text,sans") -- the official Reuters logo <https://en.wikipedia.org/wiki/File:Reuters_Logo.svg> looks like it's summoning a seraphim
 | u'' u "www.theage.com.au" = ("A", "text")
 | u'' u "www.candyjapan.com"  = ("🍬", "text")
 | aU'' u ["www.cambridge.org", "journals.cambridge.org", "static.cambridge.org"] = ("⛨", "text") -- ⛨ BLACK CROSS ON SHIELD U+26E8, roughly imitating <https://en.wikipedia.org/wiki/Coat_of_arms_of_the_University_of_Cambridge>
 | u'' u "www.nzherald.co.nz" = ("𝕳", "text")
 | u'' u "nintil.com" = ("𝓝", "text") -- @ArtirKel José Luis Ricón Fernández de la Puente
 | u'' u "www.brookings.edu" = ("B", "text")
 | u'' u "variety.com" = ("𝓥", "text")
 | u'' u "theconversation.com" = ("🗨", "text")
 | u'' u "patch.com" = ("P", "text,sans")
 | u'' u "thegradient.pub" = ("∇", "text")
 | u'' u "www.projectrho.com" = ("ρ", "text")
 | u'' u "harpers.org" = ("H", "text")
 | u'' u "www.thelancet.com" = ("L", "text")
 | u' u "github.com/huggingface/" || u' u "medium.com/huggingface/" || u'' u "huggingface.co" = ("\129303", "text") -- "🤗" HUGGING FACE U+1F917
 | u'' u "www.pragmatic.ml" = ("𝕄", "text") -- Madison May, machine learning blog
 | u'' u "www.outsideonline.com" = ("𝕆", "text") -- imitate the shadowing on Outside Online's 'O' <https://www.outsideonline.com/wp-content/uploads/2021/07/favicon-194x194-1.png>
 | u'' u "norvig.com" = ("N", "text,sans") -- Google Director of Research <https://en.wikipedia.org/wiki/Peter_Norvig>; <https://norvig.com/favicon.ico> is actually instantly recognizable & well-chosen, but unfortunately, only works because of the *colors*... so we'll settle for a plain sans capital N.
 | u'' u "novelai.net" || u'' u "blog.novelai.net" = ("🖋", "text") -- LOWER LEFT FOUNTAIN PEN (U+1F58B); NovelAI logo is a fountain pen nib.
 | u'' u "www.thebeliever.net" = ("𝐁", "text") -- _The Believer_ magazine <https://en.wikipedia.org/wiki/The_Believer_(magazine)>, McSweeney's spinoff (formerly <https://believermag.com>): logo is a dropshadow serif capital B logo
 | u'' u "solar.lowtechmagazine.com" = ("☀", "text") -- Low Tech Magazine (U+2600 BLACK SUN WITH RAYS)
 | u'' u "www.nobelprize.org" = ("🏅", "text") -- Nobel Prize, SPORTS MEDAL
 | otherwise = ("", "")

linkIconRulesDouble u
 | aU'' u ["marginalrevolution.com", "conversationswithtyler.com"] = ("M𝐑", "text") -- MR: cheaper to abuse Unicode (𝐑) MATHEMATICAL BOLD CAPITAL R
 | u'' u "www.frontiersin.org" = ("FS", "text,sans") -- <https://en.wikipedia.org/wiki/Frontiers_Media> multiple-cubes logo too busy for an icon, no Unicode equivalent
 | aU'' u ["www.gutenberg.org", "gutenberg.ca", "gutenberg.net.au", "www.fadedpage.com"] = ("PG", "text") -- Faded Pages isn't strictly-speaking a Project Gutenberg org, but they work with Distributed Proofreaders & their work is in PG Canada and they do similar things so meh.
 | u'' u "guzey.com" = ("A.G.", "text,sans")
 | u' u "alignmentforum.org" || (u'' u "www.greaterwrong.com" && u' u "view=alignment-forum") = ("AF", "text,sans")
 | u'' u "boingboing.net" = ("bb", "text,mono")
 | u'' u "nymag.com" = ("𝒩𝒴", "text")
 | u'' u "thebrowser.com" = ("TB", "text")
 | u'' u "crookedtimber.org" = ("CT", "text")
 | u' u ".latimes.com" = ("𝔏A", "text")
 | u'' u "everything2.com" = ("E2", "text")
 | u'' u "examine.com" = ("Eχ", "text,sans")
 | aU'' u ["wiki.evageeks.org","forum.evageeks.org","www.evamonkey.com"] || u' u "twitter.com/EvaMonkey/" = ("EG", "text") -- Evangelion: we’ll split this into EGF-related and other NGE sites
 | u' u "mozilla.org" = ("FF", "text,sans") -- none of the available Firefox SVG logos worked well as a link icon; typically, too much detail, the swirly-spikes too indistinct & under-emphasized, and confusable with DeepMind.
 | u'' u "www.goodreads.com" = ("GR", "text") -- GoodReads: logo doesn’t make sense as a grayscale
 | u'' u "kk.org" = ("KK", "text,sans") -- Kevin Kelly
 | aU'' u ["www.lesswrong.com", "sl4.org", "wiki.lesswrong.com", "www.greaterwrong.com"] = ("LW", "text") -- LW logo is just a colored ‘LW’, so no point in converting. Other user: wiki.lesswrong.com
 | aU'' u ["michaelnielsen.org", "quantum.country", "numinous.productions", "cognitivemedium.com", "neuralnetworksanddeeplearning.com"] = ("MN", "text")
 | u'' u "www.motherjones.com" = ("MJ", "text,sans")
 | u'' u "openreview.net" = ("OR", "text,sans") -- doesn't seem to have any real logo or wordmark: <https://openreview.net/about>
 | u'' u "www.overcomingbias.com" || u' u "mason.gmu.edu/~rhanson/" = ("OB", "text") -- OB logo too bad to use
 | u'' u "www.theparisreview.org" = ("PR", "text") -- The Paris Review: not even going to try to make their weird bird logo work
 | u'' u "www.sciencedaily.com" = ("SD", "text,sans")
 | u'' u "www.sciencenews.org" = ("SN", "text,sans") -- <https://en.wikipedia.org/wiki/Science_News>
 | u'' u "sethroberts.net" = ("SR", "text,sans") -- Logo is a sans 'S' on a red circle background; can't use 'S' because already used by Slate.
 | u'' u "scholars-stage.org" = ("Ss", "text") -- Avoid the unfortunate connotations of ‘SS’
 | u'' u "tvtropes.org" = ("TV", "text") -- TV Tropes: their lampshade icon is unrecognizable & hard to see small
 | u'' u "www.vanityfair.com" = ("VF", "text")
 | u'' u "yunnansourcing.com" || u'' u "yunnansourcing.us" = ("ys", "text")
 | u'' u "memteaimports.com" = ("MT", "text,sans")
 | u'' u "www.jneurosci.org" = ("JN", "text")
 | u'' u "tl.net" = ("TL", "text,sans")
 | u'' u "www.businessinsider.com" = ("BI", "text,sans")
 | u'' u "dnstats.net" = ("dn", "text,sans")
 | u'' u "www.newsweek.com" = ("NW", "text") -- logo is 'N' but there are too many 'N's floating around, so abbreviate 'Newsweek' as 'NW'
 | u'' u "www.thecut.com" = ("TC", "text")
 | u'' u "www.scientificamerican.com" = ("SA", "text")
 | u'' u "www.stuff.co.nz" = ("NZ", "text,sans") -- even their official name 'Stuff' is lazy and unmemorable. I just think of them as 'that New Zealand website reporting on crime & DNM stuff'…
 | u'' u "chronopause.com" = ("M.D.", "text,sans") -- Mike Darwin, similarly TODO: experiment with initials using periods - does this work as-is? How about quad? '﹒' SMALL FULL STOP U+FE52 does not work.
 | u'' u "vitalik.eth.limo" || u' u "/doc/economics/mechanism-design/quadratic-voting/2018-buterin.pdf" = ("V.B.", "text,sans") -- Vitalik Buterin, similarly
 | u'' u "unenumerated.blogspot.com" || u' u "szabo.best.vwh.net" || u' u "nick-szabo" = ("N.S.", "text,sans") -- Nick Szabo
 | u'' u "scottaaronson.blog" || u'' u "www.scottaaronson.com" = ("S.A.", "text,sans") -- Scott Aaronson
 | u'' u "www.rifters.com" = ("P.W.", "text,sans") -- Peter Watts
 | u'' u "www.antipope.org" = ("C.S.", "text,sans") -- Charles Stross
 | u'' u "www.ribbonfarm.com" = ("ℝ𝔽", "text,sans")
 | u'' u "www.deviantart.com" = ("DA", "text,sans") -- the official logo <https://en.wikipedia.org/wiki/File:DeviantArt_Logo.svg> isn't *too* bad and is at least 8 years old, but I don't recognize it so I doubt most readers would.
 | u'' u "www.smithsonianmag.com" = ("SM", "text")
 | u'' u "scienceblogs.com" = ("Sᵇ", "text,sans,italic")
 | u'' u "www.dailydot.com" = ("D.", "text,sans")
 | u'' u "www.johndcook.com" = ("JC", "text,sans")
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
 | u' u "livejournal.com" = ("LJ", "text,sans")
 | u'' u "www.newscientist.com" = ("NS", "text,sans")
 | u'' u "www.palladiummag.com" = ("Pd", "text,sans") -- "P" is their logo but that is too generic and collides, so take 'palladium' literally & use the element abbreviation
 | u'' u "www.gq.com" = ("GQ", "text,sans")
 | u'' u "foreignpolicy.com" = ("FP", "text")
 | u'' u "www.unqualified-reservations.org" = ("UR", "text")
 | u'' u "www.thenewatlantis.com" = ("NA", "text")
 | aU'' u ["www.supermemo.com", "super-memory.com"] = ("SM", "text,sans")
 | u'' u "qwantz.com" = ("DC", "text,sans")
 | u'' u "qualiacomputing.com" = ("QC", "text,sans")
 | u'' u "www.nngroup.com" = ("NN", "text,sans")
 | u'' u "replicationindex.com" = ("RI", "text,sans")
 | u' u ".yahoo.com" = ("Y!", "text,sans")
 | u'' u "quantifiedself.com" || u'' u "forum.quantifiedself.com" || u' u "www.reddit.com/r/QuantifiedSelf/" = ("QS", "text,sans")
 | u'' u "www.research.va.gov" = ("VA", "text,sans") -- US Department of Veterans Affair (mostly linked for Million Veteran Project)
 | u'' u "apnews.com" = ("AP", "text,sans")
 | aU' u ["www.unz.com/gnxp/", "razib.substack.com", "www.razib.com", "www.razibkhan.com", "www.gnxp.com", "twitter.com/razibkhan"] = ("RK", "text,sans") -- Razib Khan
 | u'' u "jaymans.wordpress.com" = ("J👨🏾", "text,sans") -- JayMan
 | u'' u "www.rollingstone.com" = ("𝓡𝐒", "text") -- Rolling Stone <https://www.rollingstone.com/wp-content/uploads/2022/08/cropped-Rolling-Stone-Favicon.png> <https://en.wikipedia.org/wiki/File:Rolling_Stone_2022.svg>
 | u'' u "www.popsci.com" = ("PS", "text,sans") -- Popular Science magazine (no usable or recognizable logos)
 | u'' u "www.crunchbase.com" = ("cb", "text,sans") -- Crunchbase <https://en.wikipedia.org/wiki/Crunchbase> <https://en.wikipedia.org/wiki/File:Crunchbase_wordmark_dark_blue.svg>
 | u'' u "newcriterion.com" = ("NC", "text") -- The New Criterion <https://en.wikipedia.org/wiki/The_New_Criterion>
 | otherwise = ("", "")

-- Tri/triple TLAs
linkIconRulesTriple u
 | u'' u "andrewgelman.com" || u'' u "statmodeling.stat.columbia.edu" = ("▅▇▃", "text") -- Favicon is a little normal distribution/histogram (▅▇▃) LOWER FIVE EIGHTHS BLOCK, LOWER SEVEN EIGHTHS BLOCK, LOWER THREE EIGHTHS BLOCK
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
 | u'' u "hpmor.com" || u' u "www.fanfiction.net/r/5782108/" || u' u "www.reddit.com/r/HPMOR/" = ("MoR", "text,tri,italic") -- override FanFiction.net
 | u'' u "www.fanfiction.net" = ("FFN", "text,tri,sans") -- The FF.net logo is pretty crazy (<https://en.wikipedia.org/wiki/File:Fanfictionnetlogo.jpeg> is the *normal* one!), and I don’t think anyone would recognize it in monochrome. 'FF' as an abbreviation is confusing with Firefox, so expand to "FFN".
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
 | aU'' u ["www.fda.gov","fis.fda.gov","clinicaltrials.gov", "classic.clinicaltrials.gov"] = ("FDA", "text,tri,sans") -- U.S. Food & Drug Administration
 | u'' u "silkroadvb5piz3r.onion" || u'' u "silkroad5v7dywlc.onion" = ("SR1", "text,sans")
 | aU'' u ["bls.gov", "data.bls.gov", "www.bls.gov"] = ("BLS", "text,sans")
 | otherwise = ("","")

 -- Quad-letter (square) icons.
linkIconRulesQuad u
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
 | u' u "mitpress.mit.edu/sites/default/files/sicp/" = ("SI CP", "text,quad,sans") -- overrides IA
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
 | u'' u "krebsonsecurity.com" = ("Krebs", "text,quad,sans") -- KrebsOnSecurity: 'KOS' unrecognizable, favicon a baffling mystery, Brian Krebs is generally known as 'Krebs', so abbreviate that
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
 | u'' u "nunosempere.com" = ("nuno", "text,quad,monospace") -- Nuño Sempere
 | u'' u "ourworldindata.org" = ("OWID", "text,quad,monospace") -- Our World In Data (OWID) <https://en.wikipedia.org/wiki/Our_World_in_Data>; NOTE: uses monospace because the 'W' is so wide
 | u'' u "www.cnbc.com" = ("CNBC", "text,quad,sans") -- CNBC: peacock logo/favicon <https://en.wikipedia.org/wiki/File:CNBC_2023.svg> doesn't seem viable as a small monochrome link-icon
 | u'' u "www.scmp.com" = ("SCM", "text,tri") -- South China Morning Post (SCMP) <https://en.wikipedia.org/wiki/South_China_Morning_Post>; major HK newspaper, partially CCP-censored post-2016 Alibaba acquisition; logo is a yellow square next to a blue square, so monochrome version would be hard (light gray next to black?); 'SCMP' unfortunately doesn't work as a quad, because the width of 'MP' is far larger than 'SC' and playing around with it, I can't get it to look good, so we settle for just the first three
 | aU'' u ["magazine.atavist.com", "read.atavist.com"] = ("Atvt", "text,quad") -- Atavist Magazine <https://en.wikipedia.org/wiki/Atavist>; can't use the italic-capital serif A logo because it looks identical to _The Atlantic_, so disemvowel the name to a 4-letter abbreviation. Annoyingly, they move around and use multiple sub-domains.
 | u'' u "qntm.org" || u == "https://scp-wiki.wikidot.com/antimemetics-division-hub" || u == "https://scp-wiki.wikidot.com/qntm-s-author-page#toc2" = ("qntm", "text,quad,mono") -- qntm/Sam Hughes: programming & SF
 | otherwise = ("", "")

-- SVG icons (remember the link-icon name is substituted in as part of the URL to the SVG icon)
linkIconRulesSVG u
 | aU'' u ["texample.net", "ctan.org", "www.tug.org", "tug.org"] = ("tex", "svg") -- Properly turning the 'TeX' logotype in a link icon is hard. You can't use the official logo: <https://commons.wikimedia.org/wiki/File:TeX_logo.svg> is unworkable as a tiny icon, Computer Modern's thinness issues are massively exacerbated & it's unreadable (it's not great on computer screens to begin with, and shrunk down to a link-icon, even worse); you can cheat in pure Unicode with 'TₑX' (LATIN SUBSCRIPT SMALL LETTER E U+2091;, there is no 'LARGE LETTER E' unfortunately) but this took winds up looking pretty bad in practice. So what I did was create my own SVG TeX link-icon in Inkscape, using Source Serif Pro bold letters, arranged by hand like the logotype, and then rescaled horizontally ~120% to make the strokes thick enough that they'd survive downscaling. *That* works.
 | aU'' u ["www.amazon.com", "aws.amazon.com", "amazon.com", "smile.amazon.com", "aboutamazon.com"] || u' u "amazon.co." = ("amazon", "svg")
 | u'' u "en.bitcoin.it" || u'' u "bitcointalk.org" || u'' u "www.blockchain.com" = ("bitcoin", "svg")
 | u'' u "www.biorxiv.org" || u'' u "www.medrxiv.org" = ("chi-dna", "svg") -- BioRxiv (custom icon: italic Chi with DNA cross-strands).
 | u'' u "distill.pub" = ("distillpub", "svg") -- Distill ML journal.
 | u'' u "www.dropbox.com" || u'' u "dl.dropboxusercontent.com" = ("dropbox", "svg") -- Dropbox: old file-host, deprecated since they’ve started killing inactive accounts & their UI become awful. primary user: dl.dropboxusercontent.com
 | u'' u "www.erowid.org" || u'' u "www.drugsdata.org" = ("erowid", "svg")
 | aU' u [".tensorflow.org", "github.com/tensorflow/", "medium.com/tensorflow/"] = ("tensorflow", "svg") -- <https://simpleicons.org/?q=tensorflow>; NOTE: hosted on Github, so override Github
 | aU'' u ["github.com", "copilot.github.com", "archiveprogram.github.com", "gist.github.com", "github.blog", "compvis.github.io"] = ("github", "svg") -- Github; I exclude *.github.io & raw.githubusercontent.com because that’s blogs/papers.
 | u'' u "paulgraham.com" = ("pg", "text,monospace") -- Paul Graham, known by username 'pg' on HN
 | u' u "ycombinator.com" = ("hacker-news", "svg") -- HN/YC (shared logo). primary user: news.ycombinator.com
 | aU' u ["webcitation.org", "mementoweb.org", "archive.org", "archive-it.org", "wiki.archiveteam.org", "waybackmachine.org", "archive.is", "archive.md", "archive.ph", "archive.today", "babel.hathitrust.org"] = ("internet-archive", "svg") -- HathiTrust <https://en.wikipedia.org/wiki/HathiTrust> is confusingly nebulous but its cute elephant logo is unrecognizable and I regard it as basically a wrapper around Google Books+Internet Archive, so I think it's less confusing to put it under the IA logo. Note: overriden by SICP
 | u'' u "mega.nz" = ("mega", "svg") -- MegaUpload/Mega: filesharing (used for big files).
 | u'' u "intelligence.org" = ("miri", "svg") -- MIRI/intelligence.org.
 | u' u ".nytimes.com" = ("new-york-times", "svg") -- The New York Times: manual edit, reducing full 'NEW YORK TIMES' SVG logo to just the ‘T’ they use as an icon.
 | aU'' u ["www.ncbi.nlm.nih.gov", "pubmed.ncbi.nlm.nih.gov"] = ("nlm-ncbi", "svg") -- NCBI/Pubmed: simplification of their logo (https://upload.wikimedia.org/wikipedia/commons/0/07/US-NLM-NCBI-Logo.svg). primary user: ncbi.nlm.nih.gov
 | u'' u "www.patreon.com" = ("patreon", "svg") -- Patreon. (Used the old one (https://upload.wikimedia.org/wikipedia/commons/9/94/Patreon_logo.svg) because I don’t like the new one.)
 | aU' u ["plos.org", "plosone.org", "plosmedicine.org"] = ("plos", "svg") -- PLOS ONE in all their domain permutations… primary user: journals.plos.org
 | aU' u ["overflow.net", "overflow.com", "stackexchange.com"] = ("stack-exchange", "svg") -- The *Exchange/*Overflow family of websites.
 | u' u "substack.com" = ("substack", "svg") -- gwern.substack.com
 | u'' u "www.theguardian.com" || u'' u "www.guardian.co.uk" = ("the-guardian", "svg") -- El Grauniad.
 | u'' u "www.newyorker.com" = ("the-new-yorker", "svg") -- The New Yorker: the Dandy SVG, simplified & rotated more vertically.
 | u' u "tumblr.com" = ("tumblr", "svg")
 | aU'' u ["twitter.com", "blog.twitter.com", "developer.twitter.com"] ||
   -- we host local HTML mirrors of Twitter/Nitter for archiving & annotation-override reasons; rather than give them an uninformative HTML icon, we detect & override here to assign them the bird icon. They follow the schema `/doc/foo/$DATE-$AUTHOR-twitter-$TITLE.html`:
   (isLocal u && hasExtension ".html" u && "-twitter-" `T.isInfixOf` u) = ("twitter", "svg")
 | u'' u "www.uptontea.com" = ("upton-tea", "svg")
 | u'' u "soundcloud.com" = ("audio", "svg")
 | u' u ".bandcamp.com" = ("audio", "svg")
 | u'' u "www.washingtonpost.com" = ("washington-post", "svg") -- The Washington Post: truncated their blackletter to ‘WP’.
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
 | u'' u "www.emacswiki.org" || aU' u ["www.reddit.com/r/emacs/", "www.gnu.org/software/emacs"] = ("emacs", "svg")
 | u'' u "www.chicagotribune.com" = ("chicago-tribune", "svg") -- fraktur capital 'C', letter-mark extracted & made black from <https://en.wikipedia.org/wiki/File:Chicago_Tribune_Logo.svg>
 | u'' u "www.tiktok.com" = ("tiktok", "svg")

 -- FINAL MATCHES:
 -- many orgs will use a medium subdomain, so we fall back here for Medium as the lowest-priority, and override case by case above:
 | u'' u "medium.com" || u'' u "towardsdatascience.com" = ("𝐌", "text") -- Medium: cheaper to abuse Unicode (𝐌) MATHEMATICAL BOLD CAPITAL M
 | u' u "reddit.com" = ("reddit", "svg") -- www.reddit.com
 | otherwise = ("", "")

-- Filetypes: (we need to parse & extract the extension because many would be too short and match too many URLs if mere infix matching was used)
linkIconRulesFiletypes u
 | iE u ["tar", "zip", "xz", "img", "bin", "pkl", "onnx", "pt", "maff"] = ("archive", "svg")
 | iE u ["opml", "txt", "xml", "json", "jsonl", "md"] || u'' u "pastebin.com" = ("txt", "svg")
 | iE u ["css", "hs", "js", "conf", "sh", "r", "R", "patch", "diff"] = ("code", "svg")
 | iE u ["doc", "docx"] = ("word-doc", "svg")
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
 | aU' u [".pdf", ".PDF", "/pdf", "type=pdf", "pdfs.semanticscholar.org", "citeseerx.ist.psu.edu", "pdfs.semanticscholar.org", "www.semanticscholar.org"] = ("pdf", "svg")
 | otherwise = ("", "")

------------------------------------------------------------------------------------------

-- linkIconTestUnitsLink :: [(Inline,T.Text,T.Text)]
-- linkIconTestUnitsLink = [(Link ("", ["directory-indexes-upwards"],      []) [Str "Test"] ("/doc/index", "Link to parent directory (ascending)"),
--                            "arrow-up-left", "svg")
--                         , (Link ("", ["directory-indexes-downwards"],   []) [Str "Test"] ("/doc/zeo/index", "Link to child directory zeo (descending)"),
--                            "arrow-down-right", "svg")
--                           , (Link ("", ["directory-indexes-sideways"],  []) [Str "Test"] ("/doc/ai/nn/transformer/alphafold/index", "Link to other directory ai/nn/transformer/alphafold (descending)"),
--                            "arrow-right", "svg")
--                           , (Link ("", ["directory-indexes-upwards"],   []) [Str "Test"] ("/doc/index", ""), "arrow-up-left", "svg")
--                           , (Link ("", ["directory-indexes-downwards"], []) [Str "Test"] ("/doc/index", ""), "arrow-down-right", "svg")
--                           , (Link ("", ["directory-indexes-sideways"],  []) [Str "Test"] ("/doc/index", ""), "arrow-right", "svg")
--                           ]

-- in /lorem order:
-- testing: unique-keys, first is URI
linkIconTestUnitsText :: [(T.Text,T.Text,T.Text)]
linkIconTestUnitsText =
        [("/static/img/icon/deepmind.svg",  "deepmind","svg")
         , ("/batman/2022-04-15-manasuka-artdecobatmantriptych-batman.psd",  "image","svg")
         , ("/doc/ai/1962-bryson.pdf",  "pdf","svg")
         , ("/doc/ai/1986-michie-onmachineintelligence.pdf#page=99",  "pdf","svg")
         , ("/doc/ai/1992-dreyfus-whatcomputerstillcantdo.epub",  "EPUB","text,quad,sans")
         , ("/doc/ai/anime/danbooru/2019-02-10-stylegan-holo-handselectedsamples.zip",  "archive","svg")
         , ("/doc/ai/anime/danbooru/2020-06-08-danbooru2019-palm-handannotations-export.jsonl",  "txt","svg")
         , ("/doc/ai/music/2019-12-22-gpt2-preferencelearning-gwern-abcmusic.patch",  "code","svg")
         , ("/doc/ai/music/2020-03-06-fifteenai-fluttershy-sithcode.mp3",  "audio","svg")
         , ("/doc/ai/nn/rnn/1991-schmidhuber.pdf", "SMDH", "text,quad,sans")
         , ("/doc/ai/nn/rnn/2015-06-03-karpathy-charrnn-visualization.tar.xz",  "archive","svg")
         , ("/doc/ai/nn/transformer/gpt/dall-e/1/2020-chen-2.pdf#openai",  "openai","svg")
         , ("/doc/ai/poetry/2019-10-17-117m-poetry-cleanprojectgutenberg-samples.txt",  "txt","svg")
         , ("/doc/ai/scaling/2020-bell.pdf#facebook",  "facebook","svg")
         , ("/doc/anime/eva/2010-1000enpark-tokyo-oota-heiwajimakoen.jpg",  "image","svg")
         , ("/doc/cat/psychology/drug/catnip/survey/2017-07-30-gs-pilot.csv",  "csv","svg")
         , ("/doc/creatine/2009-ling-data.xls",  "spreadsheet","svg")
         , ("/doc/cs/hardware/2010-nordhaus-nordhaus2007twocenturiesofproductivitygrowthincomputing-appendix.xlsx",  "spreadsheet","svg")
         , ("/doc/cs/linkrot/2009-08-20-b3ta-fujitsuhtml.mht",  "misc","svg")
         , ("/doc/darknet-market/usareshipper-profile.maff", "archive","svg")
         , ("/doc/design/typography/rubrication/1990-tufte-envisioninginformation-ch5-byrneseuclid.pdf", "ET", "text")
         , ("/doc/dual-n-back/2012-zhong.ebt",  "misc","svg")
         , ("/doc/economics/mechanism-design/quadratic-voting/2018-buterin.pdf", "V.B.", "text,sans")
         , ("/doc/fiction/poetry/2011-yvain-iliadaslawsuit.html",  "SSC","text,tri")
         , ("/doc/genetics/heritable/2015-mosing-supplement.docx",  "word-doc","svg")
         , ("/doc/iq/ses/2011-gensowski-figure7-totaleffectofiqandpersonalityonlifetimeearnings.png",  "image","svg")
         , ("/doc/iq/2014-tenijenhuis-supplement.doc",  "word-doc","svg")
         , ("/doc/personal/businesscard-front-draft.xcf",  "image","svg")
         , ("/doc/personal/google-cse.xml",  "alphabet","svg")
         , ("/doc/personal/rss-subscriptions.opml",  "txt","svg")
         , ("/doc/psychology/2019-01-21-eric-socksurvey.ods",  "spreadsheet","svg")
         , ("/doc/psychology/spaced-repetition/michaellee-memoryretentionexperiments-data.tar",  "archive","svg")
         , ("/doc/reinforcement-learning/model/alphago/2016-silver.pdf#deepmind",  "deepmind","svg")
         , ("/doc/rotten.com/library/bio/hackers/robert-morris/morris.bmp",  "image","svg")
         , ("/doc/rotten.com/library/sex/sexual-ethics-in-psychology/55403_m.gif",  "image","svg")
         , ("/doc/sociology/technology/2017-reddit-dhieno-theplace-timelapseevolution.mp4",  "file-video","svg")
         , ("/doc/statistics/bayes/1988-jaynes-maximumentropyandbayesianmethods.pdf", "ETJ", "text,tri,sans")
         , ("/doc/tea/gwern-tea-mineralwaters-bestarm-sequential.webm",  "file-video","svg")
         , ("/doc/touhou/2013-06-08-acircle-tohoarrange.mdb.xz",  "archive","svg")
         , ("/doc/touhou/2013-c84-downloads.json",  "txt","svg")
         , ("/doc/zeo/firmware-v2.6.3R-zeo.img",  "archive","svg")
         , ("http://archive.recapthelaw.org/paed/203025/", "PACR", "text,quad")
         , ("http://archives.cnn.com/2000/HEALTH/aging/04/19/hearing.loss.wmd/index.html", "CNN", "text,tri,sans")
         , ("http://blog.sigfpe.com/2005/08/absence-of-evidence-is-evidence-of.html", "sgfp", "text,quad,monospace")
         , ("http://chronopause.com/chronopause.com/index.php/2011/08/05/science-fiction-double-feature-2-part-2/index.html", "M.D.", "text,sans")
         , ("http://esr.ibiblio.org/?p=7183", "ESR","text,tri,sans")
         , ("https://evaotaku.com/html/programbooks.html",  "NGE","text,tri")
         , ("http://gptprompts.wikidot.com/context-stuffing", "openai","svg")
         , ("http://host.robots.ox.ac.uk/pascal/VOC/", "VOC", "text,tri,sans")
         , ("http://iqtest.dk/main.swf",  "file-video","svg")
         , ("http://journals.cambridge.org/production/action/cjoGetFulltext?fulltextid=1463440", "⛨", "text")
         , ("https://kanzaki.sub.jp/archives/000272.html", "NGE", "text,tri")
         , ("http://libgen.org/search.php?req=%22wheel+of+time%22", "raven", "svg")
         , ("https://libgen.rs/", "raven", "svg")
         , ("http://lists.urth.net/pipermail/urth-urth.net/2010-December/019108.html",  "U","text")
         , ("http://neuralnetworksanddeeplearning.com/chap6.html", "MN", "text")
         , ("https://news.bbc.co.uk/2/hi/8448731.stm",  "BBC","text,tri,sans")
         , ("https://norvig.com/experiment-design.html", "N", "text,sans")
         , ("http://rspb.royalsocietypublishing.org/content/284/1851/20162562", "RS", "text")
         , ("https://17th-angel.tumblr.com/post/11409371268/anno-a-transfer-student-opens-the-door-with-a",  "NGE","text,tri")
         , ("https://6b.eleuther.ai/", "eleutherai", "svg")
         , ("https://80000hours.org/podcast/episodes/sam-bankman-fried-high-risk-approach-to-crypto-and-doing-good/", "80k", "text,tri,sans")
         , ("https://abcnews.go.com/Health/MedicalMysteries/story?id=3679532&page=1", "ABC", "text,tri,sans")
         , ("https://about.google/",  "alphabet","svg")
         , ("https://academic.oup.com/ageing/article/36/5/507/40586", "OUP", "text,tri")
         , ("https://academic.oup.com/ije/article/43/3/775/758445",  "OUP","text,tri")
         , ("https://aidungeon.medium.com/introducing-ai-dungeon-translate-a50e35f6df83", "AID", "text,tri,sans")
         , ("https://ai.facebook.com/blog/a-highly-efficient-real-time-text-to-speech-system-deployed-on-cpus/",  "facebook","svg")
         , ("https://aiimpacts.org/2019-recent-trends-in-gpu-price-per-flops/", "AII", "text,tri")
         , ("https://aino.bandcamp.com/track/--2",  "audio","svg")
         , ("https://ajcn.nutrition.org/content/69/5/842.full", "OUP", "text,tri")
         , ("https://animekritik.wordpress.com/2011/05/12/evangelion-2-0-surnames-statements-and-makinami/", "NGE", "text,tri")
         , ("https://antilop.cc/sr/#assassination_plot", "෴", "text")
         , ("https://apnews.com/article/ap-top-news-international-news-weekend-reads-china-health-269b3de1af34e17c1941a514f78d764c", "AP", "text,sans")
         , ("https://apps.apple.com/us/app/better-brain-lite/id307920888", "apple", "svg")
         , ("https://browse.arxiv.org/html/2112.11848",  "\120536","text")
         , ("https://arankomatsuzaki.wordpress.com/2021/03/04/state-of-the-art-image-generative-models/", "ak", "text,sans")
         , ("https://arankomatsuzaki.wordpress.com/2021/06/04/gpt-j/", "eleutherai", "svg")
         , ("https://archive.foolz.us/a/thread/77196171/#77207238", "4CH", "text,sans")
         , ("https://archive.nytimes.com/6thfloor.blogs.nytimes.com/2013/03/20/a-sham-procedure-leads-to-disappointing-m-s-news/",  "new-york-times","svg")
         , ("https://archiveofourown.org/works/17356235", "Ao3", "text,tri")
         , ("https://archiveprogram.github.com/",  "github","svg")
         , ("https://arstechnica.com/gadgets/2011/09/the-post-jobs-era-tim-cook-brings-philanthropy-back-to-apple/",  "ars","text,tri,sans")
         , ("https://arxiv.org/abs/0707.1051",  "\120536","text")
         , ("https://arxiv.org/abs/1404.7828#schmidhuber", "SMDH", "text,quad,sans")
         , ("https://arxiv.org/abs/1611.02779#openai",  "openai","svg")
         , ("https://arxiv.org/abs/1612.08810#deepmind",  "deepmind","svg")
         , ("https://arxiv.org/abs/1706.04972#google",  "alphabet","svg")
         , ("https://arxiv.org/abs/2003.13590#microsoft",  "MS","text,sans,italic")
         , ("https://arxiv.org/abs/2004.13637#facebook",  "facebook","svg")
         , ("https://arxiv.org/abs/2111.02114#laion", "laion", "svg")
         , ("https://arxiv.org/abs/2207.05221#anthropic", "anthropic", "svg")
         , ("https://arxiv.org/pdf/2009.06732.pdf#org=google&page=6",  "alphabet","svg")
         , ("https://ask.metafilter.com/16136/Fog-Gun-Shower", "MF", "text,sans,italic")
         , ("https://www.astralcodexten.com/p/know-your-amphetamines",  "SSC","text,tri")
         , ("https://babel.hathitrust.org/cgi/pt?id=uc1.c101988734&view=1up&seq=1", "internet-archive","svg")
         , ("https://bair.berkeley.edu/blog/2020/07/11/auction/", "BAIR", "text,quad,mono")
         , ("https://beepb00p.xyz/pkm-search.html", "\129302", "text")
         , ("https://betonit.blog/2022/03/02/make-desertion-fast/", "econlib", "svg")
         , ("https://bitcointalk.org/index.php?topic=82952.0;all",  "bitcoin","svg")
         , ("https://bjo.bmj.com/content/93/8/997",  "bmj","text,tri,sans")
         , ("https://bldgblog.com/2015/12/four-floor-war/", "BLDG", "text,quad,monospace")
         , ("https://blog.23andme.com/articles/genes-scream-for-ice-cream", "23", "text")
         , ("https://blog.archive.org/2011/08/17/scanning-a-braille-playboy/",  "internet-archive","svg")
         , ("https://blog.eleuther.ai/announcing-20b/", "eleutherai", "svg")
         , ("https://blog.eleuther.ai/year-one/", "eleutherai", "svg")
         , ("https://blog.givewell.org/2012/08/23/how-we-evaluate-a-study/", "GW", "text")
         , ("https://blogs.nature.com/news/2011/09/reliability_of_new_drug_target.html",  "n","text")
         , ("https://blogs.nvidia.com/blog/gaugan-photorealistic-landscapes-nvidia-research/",  "n","text,sans,italic")
         , ("https://blottyparchment.livejournal.com/7541.html?thread=233845", "LJ", "text,sans")
         , ("https://bls.gov/news.release/archives/ecec_031986.pdf", "BLS", "text,sans")
         , ("https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-4-13",  "springerlink", "svg")
         , ("https://boardgamegeek.com/boardgame/148931/coup-reformation", "BGG", "text,tri,sans")
         , ("https://boards.4chan.org/jp/", "4CH", "text,sans")
         , ("https://boards.fireden.net/a/thread/185257999/", "4CH", "text,sans")
         , ("https://boingboing.net/2011/02/03/cosmic-commodities-h.html", "bb", "text,mono")
         , ("https://cacm.acm.org/research/the-science-of-brute-force/", "acm", "text,tri,sans")
         , ("https://carryiton.net/chain-letter/bibliography.htm", "✉", "text")
         , ("https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.108.7127&rep=rep1&type=pdf",  "pdf","svg")
         , ("https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.75.2289&rep=rep1&type=pdf",  "pdf","svg")
         , ("https://classic.clinicaltrials.gov/ct2/show/NCT01684306", "FDA","text,tri,sans")
         , ("https://clinicaltrials.gov/study/NCT03429075",  "FDA","text,tri,sans")
         , ("https://cognitivemedium.com/srs-mathematics", "MN", "text")
         , ("https://commons.wikimedia.org/wiki/File:Energy_density.svg",  "wikipedia","svg")
         , ("https://compvis.github.io/taming-transformers/",  "github","svg")
         , ("https://cran.r-project.org/web/packages/censReg/index.html",  "R","text")
         , ("https://cran.r-project.org/web/packages/tufte/index.html", "ET", "text")
         , ("https://creativecommons.org/licenses/by-nc/3.0/", "creative-commons", "svg")
         , ("https://crookedtimber.org/2012/05/30/in-soviet-union-optimization-problem-solves-you/", "CT", "text")
         , ("https://cro.sagepub.com/content/15/5/252.full.pdf+html",  "SAGE","text,quad,sans")
         , ("https://crypto.stackexchange.com/questions/2507/can-i-encrypt-user-input-in-a-way-i-cant-decrypt-it-for-a-certain-period-of-tim",  "stack-exchange","svg")
         , ("https://ctan.org/pkg/marginnote", "tex","svg")
         , ("https://danbooru.donmai.us/posts?tags=death_flag", "❐", "text")
         , ("https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=1&year1=199201&year2=201101", "BLS", "text,sans")
         , ("https://derpibooru.org/tags/generator-colon-thisponydoesnotexist", "❐", "text")
         , ("https://developer.twitter.com/en/doc/twitter-api/v1/rules-and-filtering/search-operators",  "twitter","svg")
         , ("https://diff.wikimedia.org/2009/11/26/wikipedias-volunteer-story/",  "wikipedia","svg")
         , ("https://distill.pub/2016/augmented-rnns/",  "distillpub","svg")
         , ("https://dl.acm.org/doi/10.1145/3266037.3266090", "acm", "text,tri,sans")
         , ("https://dl.dropboxusercontent.com/u/182368464/umineko-compress.tar.xz",  "dropbox","svg")
         , ("https://dnstats.net/market/Amazon+Dark", "dn", "text,sans")
         , ("https://docs.google.com/document/d/1MhA3M5ucBD7ZXcWk57_MKZ5jEgPX6_YiKye_EFP-adg/edit",  "word-doc","svg")
         , ("https://econlolcats.tumblr.com/",  "tumblr","svg")
         , ("https://edwardtufte.github.io/tufte-css/#epigraphs", "ET", "text")
         , ("https://elifesciences.org/articles/16351", "eL", "text,sans")
         , ("https://en.bitcoin.it/wiki/Witcoin",  "bitcoin","svg")
         , ("https://engineering.fb.com/2014/11/14/production-engineering/solving-the-mystery-of-link-imbalance-a-metastable-failure-state-at-scale/",  "facebook","svg")
         , ("https://en.touhouwiki.net/wiki/Iyokan", "☯", "text")
         , ("https://en.wikibooks.org/wiki/Category:Book:Accountancy", "wikipedia","svg")
         , ("https://en.wikiquote.org/wiki/Talk:Edsger_W._Dijkstra#Telescope", "wikipedia","svg")
         , ("https://en.wikisource.org/wiki/Essays_on_Political_Economy/That_Which_Is_Seen,_and_That_Which_Is_Not_Seen",  "wikipedia","svg")
         , ("https://en.wiktionary.org/wiki/bien_pensant",  "wikipedia","svg")
         , ("https://eva-fan.com/blog-entry-1198.html",  "NGE","text,tri")
         , ("https://eva.onegeek.org/",  "NGE","text,tri")
         , ("https://everything2.com/title/2015+%253A+The+Last+Year+of+Ryoji+Kaji", "E2", "text")
         , ("https://examine.com/supplements/bacopa-monnieri/", "Eχ", "text,sans")
         , ("https://files.givewell.org/files/DWDA%202009/Interventions/Iodine/Bautista%20et%20al%201982.pdf", "GW", "text")
         , ("https://fis.fda.gov/sense/app/d10be6bb-494e-4cd2-82e4-0135608ddc13/sheet/45beeb74-30ab-46be-8267-5756582633b4/state/analysis",  "FDA","text,tri,sans")
         , ("https://foreignpolicy.com/2010/11/23/death-by-a-thousand-cuts-2/", "FP", "text")
         , ("https://forum.effectivealtruism.org/posts/nSot23sAjoZRgaEwa/2016-ai-risk-literature-review-and-charity-comparison", "EA", "text")
         , ("https://forum.evageeks.org/index.php",  "EG","text")
         , ("https://forum.quantifiedself.com/t/indoor-air-quality-monitoring-health/799/40", "QS", "text,sans")
         , ("https://foundation.wikimedia.org/wiki/Privacy_policy",  "wikipedia","svg")
         , ("https://freakonomics.com/2007/05/what-do-you-have-to-say-about-ron-paul/", "FRK", "text,tri,sans")
         , ("https://fullfrontal.moe/interview-mahiro-maeda/", "NGE", "text,tri")
         , ("https://gameprogrammingpatterns.com/singleton.html", "GPP", "text,tri,sans")
         , ("https://github.com/clayh53/tufte-jekyll", "ET", "text")
         , ("https://github.com/edwardtufte/tufte-css", "ET", "text")
         , ("https://github.com/fastai/numerical-linear-algebra/blob/master/README.md","F.ai", "text,tri")
         , ("https://github.com/huggingface/transformers", "\129303", "text")
         , ("https://github.com/jez/tufte-pandoc-jekyll", "ET", "text")
         , ("https://github.com/LAION-AI/laion-datasets/blob/main/laion-aesthetic.md", "laion", "svg")
         , ("https://github.com/paul-buerkner/brms#overview",  "R","text")
         , ("https://gizmodo.com/weird-and-wonderful-movies-that-youll-never-get-to-see-5877874", "GIZM", "text,quad,mono")
         , ("https://groups.google.com/g/pandoc-discuss/c/jgb-Q0F2p1Y/m/1DogIEAtAQAJ", "PNDC", "text,quad,sans")
         , ("https://groups.google.com/group/ankisrs/",  "\9993","text")
         , ("https://groups.yahoo.com/group/givewell/message/287", "GW", "text")
         , ("https://groups.yahoo.com/group/tiffanygrantfanclub/message/5697",  "\9993","text")
         , ("https://gutenberg.ca/ebooks/smithcordwainer-onthegemplanet/smithcordwainer-onthegemplanet-00-h.html", "PG","text")
         , ("https://gutenberg.net.au/ebooks02/0201141h.html", "PG","text")
         , ("https://guzey.com/how-life-sciences-actually-work/", "A.G.", "text,sans")
         , ("https://gwern.net/doc/cs/hardware/2015-kanev.pdf#google",  "alphabet","svg")
         , ("https://gwern.substack.com/",  "substack","svg")
         , ("https://habr.com/ru/articles/516190/", "Habr", "text,quad,sans")
         , ("https://hacks.mozilla.org/2021/05/improving-firefox-stability-on-linux/", "FF", "text,sans")
         , ("https://harpers.org/archive/1954/12/the-jet-propelled-couch/?single=1", "H", "text")
         , ("https://hbr.org/2019/12/can-you-know-too-much-about-your-organization", "HBR", "text,tri,sans")
         , ("https://www.nasa.gov/history/rogersrep/v2appf.htm",                                    "nasa", "svg")
         , ("https://hivemind-repo.s3-us-west-2.amazonaws.com/twdne3/twdne3.onnx",  "archive","svg")
         , ("https://hpmor.com/chapter/2", "MoR", "text,tri,italic")
         , ("https://hpmor.com/notes/progress-report-2013-11-01/", "MoR", "text,tri,italic")
         , ("https://huggingface.co/spaces/teven-projects/calculator", "\129303", "text")
         , ("https://ideas.repec.org/p/nbr/nberwo/27053.html", "NBER", "text,quad")
         , ("https://ieeexplore.ieee.org/xpls/abs_all.jsp?arnumber=602492", "IEEE", "text,mono,quad")
         , ("https://i.imgur.com/atMz0jg.png",  "image","svg")
         , ("http://silkroad5v7dywlc.onion/index.php?topic=2889.0", "SR1", "text,sans")
         , ("http://silkroadvb5piz3r.onion/index.php/silkroad/user/69a6bec290", "SR1", "text,sans")
         , ("https://innsbigdata.wordpress.com/2015/02/09/interview-with-juergen-schmidhuber/", "SMDH", "text,quad,sans")
         , ("https://intelligence.org/2013/10/03/proofs/",  "miri","svg")
         , ("https://iopscience.iop.org/article/10.1088/1748-9326/aabf9b", "IOP", "text,tri,sans")
         , ("https://jack-clark.net/2022/10/31/import-ai-308-recursively-self-improving-lms-3-1tb-of-code-data-dall-e2-makes-alien-errors/", "anthropic", "svg")
         , ("https://jamanetwork.com/journals/jama/fullarticle/183162", "JAMA", "text,sans,quad")
         , ("https://jamanetwork.com/journals/jama/fullarticle/201218", "JAMA", "text,sans,quad")
         , ("https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/414283", "JAMA", "text,sans,quad")
         , ("https://jaspervdj.be/hakyll/reference/Hakyll-Web-Redirect.html", "JVDJ", "text,quad,mono")
         , ("https://jaymans.wordpress.com/2015/07/04/demography-is-destiny/", "J👨🏾", "text,sans")
         , ("https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.1000451",  "plos","svg")
         , ("https://journals.plos.org/plosmedicine/article/fetchObject.action?uri=info:doi/10.1371/journal.pmed.0020124.t004&representation=PNG_M",  "plos","svg")
         , ("https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0023175",  "plos","svg")
         , ("https://kk.org/books/out-of-control",  "KK","text,sans")
         , ("https://knowyourmeme.com/memes/navy-seal-copypasta", "KYM", "text,tri")
         , ("https://krebsonsecurity.com/2013/07/mail-from-the-velvet-cybercrime-underground/", "Krebs", "text,quad,sans")
         , ("http://sl4.org/archive/0812/index.html#19570",  "LW","text")
         , ("https://laion.ai/blog/coca/", "laion", "svg")
         , ("https://latitude.io/blog/how-we-accidentally-gave-our-bots-their-personalities/", "AID", "text,tri,sans")
         , ("https://library.bz/main/upload/", "raven", "svg")
         , ("https://link.springer.com/article/10.1007/BF02253535",  "springerlink", "svg")
         , ("https://link.springer.com/article/10.3758/s13423-021-01927-8", "springerlink", "svg")
         , ("https://longbets.org/",  "X","text,overline")
         , ("https://longnow.org/ideas/lenski-long-term-evolution-experiment/",  "X","text,overline")
         , ("https://longreads.com/2015/01/28/friendship-is-complicated/", "Long", "text,quad")
         , ("https://lwn.net/Articles/286233/", "LWN", "text,tri,sans")
         , ("https://machinelearning.apple.com/research/hey-siri", "apple", "svg")
         , ("https://magenta.tensorflow.org/music-transformer", "alphabet", "svg")
         , ("https://marginalrevolution.com/",  "M\119825","text")
         , ("https://conversationswithtyler.com/episodes/seth-godin/",  "M\119825","text")
         , ("https://mason.gmu.edu/~rhanson/ideafutures.html",  "OB","text")
         , ("https://mathoverflow.net/questions/32967/have-any-long-suspected-irrational-numbers-turned-out-to-be-rational",  "stack-exchange","svg")
         , ("https://mathshistory.st-andrews.ac.uk/Extras/Poincare_Intuition/", "M  T", "text,quad,sans")
         , ("https://mattlakeman.org/2020/01/22/hill-billy-elegy-the-culture-of-white-american-poverty/",  "MATT", "text,quad,sans")
         , ("https://medium.com/craft-ventures/the-sharp-startup-when-paypal-found-product-market-fit-5ba47ad35d0b",  "\119820","text")
         , ("https://medium.com/huggingface/distilbert-8cf3380435b5", "\129303", "text")
         , ("https://medium.com/tensorflow/fitting-larger-networks-into-memory-583e3c758ff9", "tensorflow", "svg")
         , ("https://mega.nz/#!0JVxHQCD!C7ijBpRWNpcL_gubWFR-GTBDJTW1jXI6ThzSxwaw2aE",  "mega","svg")
         , ("https://meltingasphalt.com/interactive/going-critical/",  "\9650","text")
         , ("https://michaelnielsen.org/blog/three-myths-about-scientific-peer-review/", "MN", "text")
         , ("https://mitpress.mit.edu/9780262536226/", "MIT", "text,tri,mono")
         , ("https://web.archive.org/web/20211103153805/https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node13.html", "SI CP", "text,quad,sans")
         , ("https://ml.berkeley.edu/blog/posts/clip-art/", "BAIR", "text,quad,mono")
         , ("https://mlp.fandom.com/wiki/A_Canterlot_Wedding_-_Part_1",  "MLPW","text,quad,sans")
         , ("https://myanimelist.net/anime/1370/Atama_Yama",  "MAL","text,tri,sans")
         , ("https://nap.nationalacademies.org/read/25762/chapter/1", "NAP", "text,tri")
         , ("https://nautil.us/mapping-the-human-exposome-236726/", "nautilus", "svg")
         , ("https://news.ycombinator.com/item?id=10012625",  "hacker-news","svg")
         , ("https://nintil.com/epigenetic-clocks", "𝓝", "text")
         , ("https://blog.novelai.net/novelai-improvements-on-stable-diffusion-e10d38db82ac", "🖋", "text")
         , ("https://numinous.productions/ttft/", "MN", "text")
         , ("https://nunosempere.com/blog/2023/01/30/an-in-progress-experiment-to-test-how-laplace-s-rule-of/", "nuno", "text,quad,monospace")
         , ("https://nv-adlr.github.io/MegatronLM",  "n","text,sans,italic")
         , ("https://nvlabs.github.io/stylegan2/versions.html",  "n","text,sans,italic")
         , ("https://nv-tlabs.github.io/big-datasetgan/",  "n","text,sans,italic")
         , ("https://nymag.com/intelligencer/2018/07/how-fortnite-became-the-most-popular-video-game-on-earth.html", "𝒩𝒴", "text")
         , ("https://nypost.com/2019/06/27/north-carolina-couple-paid-25k-to-clone-their-beloved-pet-cat/", "NYP", "text,tri,sans,italic")
         , ("https://oeis.org/A001006", "OEIS", "text,quad,sans")
         , ("https://omega0.xyz/omega8008/JaynesBookPdf.html", "ETJ", "text,tri,sans")
         , ("https://onlinelibrary.wiley.com/doi/full/10.1111/brv.12407",  "W","text,sans")
         , ("https://openai.com/research/better-language-models",  "openai","svg")
         , ("https://openreview.net/forum?id=xTJEN-ggl1b", "OR", "text,sans")
         , ("https://osf.io/dhx48/",         "ψ", "text")
         , ("https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3774989", "SSRN", "text,quad")
         , ("https://paperswithcode.com/method/dilated-convolution", "PwC", "text,tri,sans")
         , ("https://pastebin.com/GrV3uYh5", "txt", "svg")
         , ("https://patch.com/california/davis/davis-pair-arrested-after-cops-intercept-3-000-suspected-ecstasy-pills-mail-serve", "P", "text,sans")
         , ("https://patrickcollison.com/labs", "PC", "text,sans")
         , ("https://pcdb.santafe.edu/", "PCDB", "text,quad,sans")
         , ("https://pdfs.semanticscholar.org/00d3/6b267777b670abd1a3b98a21bf662245a7c4.pdf",  "pdf","svg")
         , ("https://people.eecs.berkeley.edu/~janner/trajectory-transformer/files/trajectory-transformer.pdf", "BAIR", "text,quad,mono")
         , ("https://people.idsia.ch/~juergen/creativity.html", "SMDH", "text,quad,sans")
         , ("https://pile.eleuther.ai/", "eleutherai", "svg")
         , ("https://plato.stanford.edu/entries/naturalism-india/", "SEP", "text,tri")
         , ("https://player.vimeo.com/video/218478638", "file-video", "svg")
         , ("https://playground.tensorflow.org/", "tensorflow", "svg")
         , ("https://poniesatdawn.bandcamp.com/album/dreamwalkers",  "P@D","text,tri")
         , ("https://predictionbook.com/",  "?","text,sans,bold")
         , ("https://proceedings.mlr.press/v37/xuc15.pdf",  "\120536","text")
         , ("https://osf.io/preprints/psyarxiv/gjh95/",   "ψ", "text")
         , ("https://publicdomainreview.org/essay/the-lost-world-of-the-london-coffeehouse/",  "TPDR","text,quad")
         , ("https://qualiacomputing.com/2015/05/22/how-to-secretly-communicate-with-people-on-lsd/", "QC", "text,sans")
         , ("https://quantifiedself.com/2014/04/eric-jain-sleep-moon-phases/", "QS", "text,sans")
         , ("https://quantum.country/qcvc", "MN", "text")
         , ("https://queue.acm.org/detail.cfm?ref=rss&id=2856460", "acm", "text,tri,sans")
         , ("https://qwantz.com/index.php?comic=1896", "DC", "text,sans")
         , ("https://qz.com/1028528/custos-startup-uses-bitcoin-bounties-to-make-pirates-rat-on-one-another", "QZ", "text,sans")
         , ("https://radiolab.org/podcast/91725-words/transcript", "audio-waveform-lines", "svg")
         , ("https://www.razibkhan.com/p/get-lucky", "RK", "text,sans")
         , ("https://rd.springer.com/article/10.1007/s10071-021-01530-3",  "springerlink", "svg")
         , ("https://replicationindex.com/2016/04/18/is-replicability-report-ego-depletionreplicability-report-of-165-ego-depletion-articles/", "RI", "text,sans")
         , ("https://retractionwatch.com/2011/02/28/crystal-myth-11-more-retractions-from-crystallography-journal-after-2010-fakery/", "magnifying-glass", "svg")
         , ("https://rjlipton.wordpress.com/2015/07/28/playing-chess-with-the-devil/", "P = NP", "text,quad")
         , ("https://rstb.royalsocietypublishing.org/content/365/1537/73.full", "RS", "text")
         , ("https://safebooru.org/index.php?page=post&s=list&tags=heterochromia", "❐", "text")
         , ("https://samuraijack.fandom.com/wiki/Episode_XL:_Jack_vs._the_Ninja", "♡","text")
         , ("https://scale.com/", "SCLE", "text,quad,monospace")
         , ("https://scholar.google.com/citations?user=9hEhCHYAAAAJ&oi=ao",  "google-scholar","svg")
         , ("https://scholars-stage.org/meditations-on-maoism-ye-fus-hard-road-home/",  "Ss","text")
         , ("https://scienceblogs.com/clock/2006/10/16/what-is-a-natural-sleep-patter", "Sᵇ", "text,sans,italic")
         , ("https://science.nasa.gov/science-news/science-at-nasa/2005/03jun_naps/",           "nasa", "svg")
         , ("https://scottaaronson.blog/?p=1438", "S.A.", "text,sans")
         , ("https://scp-wiki.wikidot.com/antimemetics-division-hub", "SCP", "text,tri,sans")
         , ("https://sethroberts.net/2008/10/03/diet-and-acne-continued/", "SR", "text,sans")
         , ("https://sg.news.yahoo.com/japan-makes-bitcoin-linked-drug-arrest-165138422--finance.html", "Y!", "text,sans")
         , ("https://sites.google.com/berkeley.edu/decision-transformer", "BAIR", "text,quad,mono")
         , ("https://slate.com/health-and-science/2017/06/daryl-bem-proved-esp-is-real-showed-science-is-broken.html",  "S","text,sans")
         , ("https://slatestarcodex.com/2015/01/15/depression-is-not-a-proxy-for-social-dysfunction/",  "SSC","text,tri")
         , ("https://soundcloud.com/leggysalad/girls-afternoon-appointments",  "audio","svg")
         , ("https://speakingofmedicine.plos.org/2012/06/25/less-research-is-needed/",  "plos","svg")
         , ("https://spectrum.ieee.org/classical-chinese", "IEEE", "text,mono,quad")
         , ("https://stability.ai/news/stable-diffusion-public-release", "SD", "text,sans")
         , ("https://stackoverflow.com/questions/1197575/can-scripts-be-inserted-with-innerhtml",  "stack-exchange","svg")
         , ("https://static.cambridge.org/binary/version/id/urn:cambridge.org:id:binary-alt:20181009171208-81978-mediumThumb-S0033291718001873_fig1g.jpg?pub-status=live", "⛨", "text")
         , ("https://statmodeling.stat.columbia.edu/2004/12/29/type_1_type_2_t/",  "\9605\9607\9603","text")
         , ("https://statmodeling.stat.columbia.edu/2013/12/17/replication-backlash/",  "\9605\9607\9603","text")
         , ("https://super-memory.com/articles/theory.htm", "SM", "text,sans")
         , ("https://tasvideos.org/3653M", "TASV", "text,quad")
         , ("https://stats.grok.se/en/201109/Accountancy", "wikipedia","svg")
         , ("https://techcrunch.com/2013/02/23/the-chinese-are-coming-the-chinese-are-coming/", "TC", "text,mono")
         , ("https://texample.net/tikz/examples/andler-optimal-lot-size/", "tex","svg")
         , ("https://text.npr.org/974534021", "npr", "text,tri,sans")
         , ("https://theconversation.com/altruism-in-birds-magpies-have-outwitted-scientists-by-helping-each-other-remove-tracking-devices-175246", "🗨", "text")
         , ("https://thegradient.pub/gpt2-and-the-nature-of-intelligence/", "∇", "text")
         , ("https://thelastpsychiatrist.com/2011/01/why_chinese_mothers_are_not_su.html", "TLP", "text,tri,sans")
         , ("https://thepiratebay.org/description.php?id=14045031", "the-pirate-bay", "svg")
         , ("https://thisanimedoesnotexist.ai/", "TADE", "text,quad,sans")
         , ("https://thisponydoesnotexist.net/", "TPDE", "text,quad,sans")
         , ("https://tl.net/blogs/283221-worker-rush-part-4-rising-up?view=all", "TL", "text,sans")
         , ("https://touhou.fandom.com/wiki/Category:Music", "☯", "text")
         , ("https://towardsdatascience.com/stylegan2-projection-a-reliable-method-for-image-forensics-700922579236", "\119820","text")
         , ("https://training.kalzumeus.com/newsletters/archive/saas_pricing", "pt11", "text,quad,monospace")
         , ("https://transformer-circuits.pub/2022/in-context-learning-and-induction-heads/index.html#anthropic", "anthropic", "svg")
         , ("https://tug.org/FontCatalogue/goudyinitialen/", "tex","svg")
         , ("https://tvtropes.org/pmwiki/pmwiki.php/Anime/MobileSuitGundamCharscounterattack",  "TV","text")
         , ("https://twitter.com/EvaMonkey/", "EG", "text")
         , ("https://twitter.com/intent/user?screen_name=Hiramatz&tw_i=303521521249447936",  "twitter","svg")
         , ("/doc/reinforcement-learning/openai/2023-11-22-karaswisher-twitter-onsamaltman.html","twitter","svg")
         , ("https://twitter.com/jackclarkSF/status/1571125410108407808", "anthropic", "svg")
         , ("https://twitter.com/patio11/status/1635413289449721856", "pt11", "text,quad,monospace")
         , ("https://twitter.com/razibkhan/status/1463204399954776073", "RK", "text,sans")
         , ("http://summaries.cochrane.org/CD007176/antioxidant-supplements-for-prevention-of-mortality-in-healthy-participants-and-patients-with-various-diseases", "cochrane-collaboration", "svg")
         , ("https://unsongbook.com/",  "\8501","text")
         , ("https://variety.com/2014/film/news/tokyo-festival-hideaki-anno-warns-of-trouble-ahead-for-japanese-animation-1201339991/", "𝓥", "text")
         , ("https://vimeo.com/28735982", "file-video", "svg")
         , ("https://vitalik.eth.limo/general/2017/09/14/prehistory.html", "V.B.", "text,sans")
         , ("https://vndb.org/c582", "VNDB", "text,quad,sans")
         , ("https://wandb.ai/wandb_fc/gradient-dissent/reports/What-could-make-AI-conscious-with-Wojciech-Zaremba-co-founder-of-OpenAI--Vmlldzo3NDk3MDI", "wandb", "svg")
         , ("https://warontherocks.com/2021/08/foreign-fighters-and-cheese-bells/", "WOTR", "text,quad,sans")
         , ("https://w.atwiki.jp/toho/pages/948.html", "☯", "text")
         , ("https://wavemotioncannon.com/2016/11/08/interview-hideaki-anno-vs-yoshiyuki-tomino-animage-071994/", "NGE", "text,tri")
         , ("https://web.archive.org/web/20120702152514/https://www.nlm.nih.gov/news/calhoun_papers_released.html",  "internet-archive","svg")
         , ("https://web.archive.org/web/19981202185145/http://www.ex.org/2.4/11-news.html",  "internet-archive","svg")
         , ("https://web.archive.org/web/20080127001226/http://johakyu.net/lib/2007/07/2007-07-27-000535.php", "NGE","text,tri")
         , ("https://web.archive.org/web/20090713005058/http://homepage3.nifty.com/mana/ecom4.htm", "NGE", "text,tri")
         , ("https://web.archive.org/web/20110415182316/http://packdeps.haskellers.com/",  "\120524","text")
         , ("https://web.archive.org/web/20110724123419/szabo.best.vwh.net/bearer_contracts.html", "N.S.", "text,sans")
         , ("https://what-if.xkcd.com/145/",  "XKCD","text,quad,sans")
         , ("https://wiki.archiveteam.org/index.php?title=Google_Reader",  "internet-archive","svg")
         , ("https://wiki.haskell.org/Xmonad/Config_archive/Gwern's_xmonad.hs",  "code","svg")
         , ("https://www.abc.net.au/news/2013-08-23/police-turn-attention-to-online-drug-trade/4908264", "ABC", "text,tri,sans")
         , ("https://www.alcor.org/library/alcor-membership-statistics/", "alcor", "svg")
         , ("https://www.alignmentforum.org/posts/HhWhaSzQr6xmBki8F/birds-planes-brains-and-ai-against-appeals-to-the-complexity", "AF","text,sans")
         , ("https://www.amazon.co.jp/%E6%AE%8B%E9%85%B7%E3%81%AA%E5%A4%A9%E4%BD%BF%E3%81%AE%E3%82%88%E3%81%86%E3%81%AB%E2%80%95%E6%96%B0%E4%B8%96%E7%B4%80%E3%82%A8%E3%83%B4%E3%82%A1%E3%83%B3%E3%82%B2%E3%83%AA%E3%82%AA%E3%83%B3JUNE%E8%AA%AD%E6%9C%AC-SUN%E3%83%BCMAGAZINE-MOOK-JUNE%E7%B7%A8%E9%9B%86%E9%83%A8/dp/490601125X",  "amazon","svg")
         , ("https://www.amazon.com/gp/product/B0050MYHBQ/",  "amazon","svg")
         , ("https://www.angelfire.com/anime4/mdwigs/Asuka.html", "NGE", "text,tri")
         , ("https://www.animenewsnetwork.com/anime-spotlight/2018/summer/revue-starlight/.132471",  "ANN","text,tri")
         , ("https://www.anthropic.com/news/anthropic-raises-124-million-to-build-more-reliable-general-ai-systems", "anthropic", "svg")
         , ("https://www.antipope.org/charlie/blog-static/2007/03/why_the_commercial_ebook_marke.html", "C.S.", "text,sans")
         , ("https://www.atlasobscura.com/articles/cyoa-choose-your-own-adventure-maps", "atlas-obscura", "svg")
         , ("https://www.bbc.com/news/business-43365710",  "BBC","text,tri,sans")
         , ("https://www.biorxiv.org/content/10.1101/013896.full",  "chi-dna","svg")
         , ("https://www.bitsaboutmoney.com/archive/the-infrastructure-behind-atms/", "pt11", "text,quad,monospace")
         , ("https://www.blockchain.com/explorer/addresses/btc/15bD6fYs6p9D9wmniDtTBcQSyWXDYNDCwv", "bitcoin","svg")
         , ("https://www.bloomberg.com/businessweek/ap/financialnews/D9KQL7CG0.htm", "\119809","text")
         , ("https://www.bloomberg.com/news/articles/2011-03-31/why-unemployment-rose-so-much-dropped-so-fast-commentary-by-alan-krueger",  "\119809","text")
         , ("https://www.bloomberg.com/news/features/2018-05-15/google-amazon-and-facebook-owe-j-rgen-schmidhuber-a-fortune","SMDH", "text,quad,sans")
         , ("https://www.bls.gov/cps/duration.htm", "BLS", "text,sans")
         , ("https://www.brookings.edu/articles/expectations-of-sustained-effects-from-scaled-up-pre-k-challenges-from-the-tennessee-study/", "B", "text")
         , ("https://www.businessinsider.com/this-is-what-happens-when-you-track-your-sleep-obsessively-2012-2", "BI", "text,sans")
         , ("https://www.cambridge.org/core/journals/journal-of-economic-history/article/two-centuries-of-productivity-growth-in-computing/856EC5947A5857296D3328FA154BA3A3", "⛨", "text")
         , ("https://www.candyjapan.com/2013-year-in-review", "🍬", "text")
         , ("https://www.cbsnews.com/colorado/news/man-allegedly-bought-pot-from-colorado-to-sell-in-maryland/", "CBS", "text,tri,sans")
         , ("https://www.cbsnews.com/news/california-biobank-dna-babies-who-has-access/", "CBS", "text,tri,sans")
         , ("https://www.cdc.gov/nchs/nvss/births.htm",  "CDC","text,tri")
         , ("https://www.cell.com/ajhg/fulltext/S0002-9297(18)30405-1",  "CELL","text,quad,sans")
         , ("https://www.connectedpapers.com/main/1ffe143b40a9f8c01940c7397280de4cf666d635/Lessons-from-AlphaZero-for-Optimal%2C-Model-Predictive%2C-and-Adaptive-Control/graph", "connected-papers","svg")
         , ("https://www.courtlistener.com/docket/16288633/1/united-states-v-takowsky/", "PACR", "text,quad")
         , ("https://www.cs.utexas.edu/~EWD/transcriptions/EWD03xx/EWD340.html", "EWD", "text,tri,sans")
         , ("https://www.dailydot.com/unclick/dark-web-black-market-reloaded-adam-bunger-gun-sales-arrest/", "D.", "text,sans")
         , ("https://www.dailymail.co.uk/health/article-2126761/Bertold-Wiesner-British-scientist-fathered-600-children-donating-sperm-fertility-clinic.html", "𝔐", "text")
         , ("https://deepmind.google/discover/blog/alphastar-mastering-the-real-time-strategy-game-starcraft-ii/",  "deepmind","svg")
         , ("https://www.deviantart.com/caji9i/art/stylegan-neural-ahegao-842847987", "DA", "text,sans")
         , ("https://www.discovermagazine.com/mind/the-brain-a-body-fit-for-a-freaky-big-brain", "D", "text")
         , ("https://www.drugsdata.org/results.php?start=0&search_field=all&s=modafinil", "erowid","svg")
         , ("https://www.dummy-system.com/2013/04/01/intervista-megumi-hayashibara-evangelion-3-0/", "NGE", "text,tri")
         , ("https://www.econlib.org/archives/2016/10/what_do_crimina.html", "econlib", "svg")
         , ("https://www.economist.com/briefing/2017/02/18/hello-again-dolly",  "E","text,sans")
         , ("https://www.econtalk.org/adam-cifu-on-ending-medical-reversal/", "econlib", "svg")
         , ("https://www.edge.org/conversation/alex_sandy_pentland-the-human-strategy",  "E","text,italic")
         , ("https://www.edwardtufte.com/bboard/images/0000yO-774.gif", "ET", "text")
         , ("https://www.effectivealtruism.org/articles/prospecting-for-gold-owen-cotton-barratt#heavy-tailed-distributions", "EA", "text")
         , ("https://www.erowid.org/",  "erowid","svg")
         , ("https://www.esquire.com/entertainment/a36439327/planet-hollywood-origin-story-history-interview/", "ℰ", "text")
         , ("https://www.evamonkey.com//ask-john/has-evangelion-influenced-contemporary-gundam-anime.php",  "EG","text")
         , ("https://www.fadedpage.com/showbook.php?pid=20160325", "PG", "text")
         , ("https://www.fanfiction.net/r/5782108/5/1/", "MoR", "text,tri,italic")
         , ("https://www.fanfiction.net/s/10360716/1/The-Metropolitan-Man",  "FFN","text,tri,sans")
         , ("https://www.fast.ai/2018/04/30/dawnbench-fastai/", "F.ai", "text,tri")
         , ("https://www.fastcompany.com/40438376/after-a-comeback-23andme-faces-its-next-test", "FC", "text")
         , ("https://www.filfre.net/2016/08/ibms-new-flavor/",  "TDA","text,tri,sans")
         , ("https://www.fimfiction.net/story/62074/Friendship-is-Optimal", "FIMF", "text,quad,mono")
         , ("https://www.forbes.com/sites/andygreenberg/2013/09/05/follow-the-bitcoins-how-we-got-busted-buying-drugs-on-silk-roads-black-market/", "F", "text")
         , ("https://fortune.com/2023/01/10/microsoft-investment-10-billion-openai-chatgpt/", "F", "text,sans")
         , ("https://www.frontiersin.org/articles/10.3389/fnhum.2011.00134/full", "FS", "text,sans")
         , ("https://www.ft.com/content/009050e4-75ea-11e2-9891-00144feabdc0", "FT", "text")
         , ("https://www.givewell.org/giving101", "GW", "text")
         , ("https://www.gnxp.com/WordPress/2017/12/12/most-people-say-they-think-nurture-is-more-important-than-nature-especially-white-americans/", "RK", "text,sans")
         , ("https://www.goodreads.com/api",  "GR","text")
         , ("https://www.gq.com/story/the-last-true-hermit", "GQ", "text,sans")
         , ("https://www.gutenberg.org/files/31663/31663-h/31663-h.htm", "PG","text")
         , ("https://www.harney.com/",  "H","text")
         , ("https://www.haskell.org/",  "\120524","text")
         , ("https://www.hoover.org/research/optimistic-thought-experiment", "hoover-institution", "svg")
         , ("https://www.huffpost.com/entry/bill-zeller-dead-princeto_n_805689", "HUFF", "text,quad,sans")
         , ("https://www.hustwit.com/urbanized", "H", "text,sans")
         , ("https://www.ieee-security.org/TC/SPW2014/papers/5103a209.PDF", "pdf", "svg")
         , ("https://www.imdb.com/title/tt0923592/", "IMDb", "text,sans,quad")
         , ("https://www.independent.co.uk/news/uk/this-britain/the-jousting-accident-that-turned-henry-viii-into-a-tyrant-1670421.html", "TI", "text")
         , ("https://www.jneurosci.org/content/32/12/4156.full", "JN", "text")
         , ("https://www.johndcook.com/blog/2010/09/13/applied-topology-and-dante-an-interview-with-robert-ghrist/", "JC", "text,sans")
         , ("https://www.justice.gov/archive/usao/cac/Pressroom/2012/045.html",  "DoJ","text,tri")
         , ("https://www.kaggle.com/datasets/ultrajack/modern-renaissance-poetry", "k", "text,sans")
         , ("https://www.kalzumeus.com/2018/10/19/japanese-hometown-tax/", "pt11", "text,quad,monospace")
         , ("https://www.khara.co.jp/hideakianno/personal-biography/",  "NGE","text,tri")
         , ("https://www.latimes.com/archives/la-xpm-1988-07-17-tm-9636-story.html", "𝔏A", "text")
         , ("https://www.lesswrong.com/",  "LW","text")
         , ("https://www.lesswrong.com/posts/37sHjeisS9uJufi4u/scholarship-how-to-do-it-efficiently",  "LW","text")
         , ("https://www.longecity.org/forum/topic/10464-modalert-is-this-what-modafinil-is-like/?&p=108566#entry108566", "⧖", "text")
         , ("https://www.lrb.co.uk/the-paper/v42/n18/james-lasdun/bats-on-the-ceiling", "LRB", "text,tri")
         , ("https://www.mail-archive.com/cryptography@metzdowd.com/msg09959.html",  "\9993","text")
         , ("https://www.mdpi.com/2220-9964/8/5/232/htm", "MDPI","text,quad,sans")
         , ("https://www.mediawiki.org/wiki/Multilingual_MediaWiki", "wikipedia","svg")
         , ("https://www.medrxiv.org/content/10.1101/2020.05.18.20100685.full",  "chi-dna","svg")
         , ("https://memteaimports.com/tea/fern-stream-amber-oolong", "MT", "text,sans")
         , ("https://www.metaculus.com/questions/notebooks/8702/the-promise-and-impact-of-the-next-generation-of-weight-loss-drugs/", "metaculus", "svg")
         , ("https://www.metafilter.com/183095/On-having-sufficient-complexity-to-allow-for-arbitrary-computation", "MF", "text,sans,italic")
         , ("https://www.metopera.org/season/2019-20-season/madama-butterfly/", "Met", "text,tri")
         , ("https://www.microsoft.com/en-us/research/blog/turing-nlg-a-17-billion-parameter-language-model-by-microsoft/",  "MS","text,sans,italic")
         , ("https://www.mirror.co.uk/news/uk-news/first-picture-teenager-accused-plotting-6124856", "M", "text,sans")
         , ("https://www.motherjones.com/kevin-drum/2018/02/an-updated-lead-crime-roundup-for-2018/",  "MJ","text,sans")
         , ("https://www.nasa.gov/mission_pages/station/expeditions/expedition30/tryanny.html", "nasa", "svg")
         , ("https://www.nber.org/papers/w16082",  "NBER","text,quad")
         , ("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2793346/",  "nlm-ncbi","svg")
         , ("https://www.nejm.org/doi/full/10.1056/NEJM199604043341416", "NEJM", "text,quad")
         , ("https://www.newscientist.com/article/2133095-boom-in-human-gene-editing-as-20-crispr-trials-gear-up/", "NS", "text,sans")
         , ("https://www.newsweek.com/gene-editing-chinese-scientist-he-jiankui-missing-house-arrest-1240749", "NW", "text")
         , ("https://www.newyorker.com/books/page-turner/the-mystery-of-s-the-man-with-an-impossible-memory",  "the-new-yorker","svg")
         , ("https://www.nextplatform.com/2019/08/20/big-blue-open-sources-power-chip-instruction-set/", "NEXT", "text,quad,sans")
         , ("https://www.nlsinfo.org/content/cohorts/nlsy97", "NLS", "text,tri,sans")
         , ("https://www.nngroup.com/articles/aesthetic-usability-effect/", "NN", "text,sans")
         , ("https://www.npr.org/2011/04/16/135450214/eight-is-too-much-for-short-sleepers", "npr", "text,tri,sans")
         , ("https://www.nybooks.com/articles/2020/01/16/alma-mahler-it-had-to-be-her/", "NYRB", "text,quad")
         , ("https://www.nytimes.com/2016/11/27/technology/artificial-intelligence-pioneer-jurgen-schmidhuber-overlooked.html", "SMDH", "text,quad,sans")
         , ("https://www.nzherald.co.nz/nz/drug-mail-or-mule-risks-the-same/QHX3IGRINL7AN5QZR3JRSOQ3NA/", "𝕳", "text")
         , ("https://www.odt.co.nz/news/dunedin/student-drug-dealer-jailed", "ODT", "text,tri")
         , ("https://www.openphilanthropy.org/research/how-much-computational-power-does-it-take-to-match-the-human-brain/", "open-philanthropy", "svg")
         , ("https://www.outsideonline.com/culture/books-media/how-athletes-get-great/", "𝕆", "text")
         , ("https://www.overcomingbias.com/p/stupider-than-you-realizehtml",  "OB","text")
         , ("https://www.palladiummag.com/2019/05/09/what-botswana-can-teach-us-about-political-stability/", "Pd", "text,sans")
         , ("https://www.patreon.com/AIDungeon", "AID", "text,tri,sans")
         , ("https://www.patreon.com/gwern",  "patreon","svg")
         , ("https://www.pewresearch.org/social-trends/2012/02/16/the-rise-of-intermarriage/", "Pew", "text,tri")
         , ("https://www.pnas.org/doi/10.1073/pnas.0610941104",  "PNAS","text,quad")
         , ("https://www.poetryfoundation.org/poems/44399/pied-beauty", "POET", "text,quad,sans")
         , ("https://www.pragmatic.ml/sparse-sinkhorn-attention/", "𝕄", "text")
         , ("https://www.projectrho.com/public_html/rocket/futurelang.php", "ρ", "text")
         , ("https://www.psychologytoday.com/us/blog/life-bilingual/201906/the-bilingual-advantage-three-years-later", "PT", "text,sans")
         , ("https://www.quantamagazine.org/how-the-slowest-computer-programs-illuminate-maths-fundamental-limits-20201210/", "quanta", "svg")
         , ("https://www.rand.org/pubs/monographs/MG1026.html",  "RAND","text,quad,sans")
         , ("https://www.reddit.com/r/AIDungeon/comments/i1qhg0/the_dragon_ai_just_got_worse/", "AID", "text,tri,sans")
         , ("https://www.reddit.com/r/HPMOR/", "MoR", "text,tri,italic")
         , ("https://www.reddit.com/r/QuantifiedSelf/comments/1mfn0a/trying_to_detect_modafinils_stimulant_effect/", "QS", "text,sans")
         , ("https://www.reddit.com/r/Supplements/comments/mr0h1/taking_melatonin_forever/",  "reddit","svg")
         , ("https://www.reddit.com/r/TOUHOUMUSIC/search/?q=author%3Agwern&sort=new&restrict_sr=on&t=all", "☯", "text")
         , ("https://www.research.va.gov/", "VA", "text,sans")
         , ("https://www.reuters.com/article/us-russia-kant-shooting/man-shot-in-russia-in-argument-over-kant-idUSBRE98F0DI20130916", "R", "text,sans")
         , ("https://www.ribbonfarm.com/2011/09/23/the-milo-criterion/", "ℝ𝔽", "text,sans")
         , ("https://www.rifters.com/real/2009/01/iterating-towards-bethlehem.html", "P.W.", "text,sans")
         , ("https://www.salon.com/2007/11/01/whistleblowers/",  "s","text")
         , ("https://www.schneier.com/blog/archives/2011/08/terrorism_in_th.html", "SOS", "text,tri,sans")
         , ("https://www.sciencedaily.com/releases/2007/05/070525204143.htm",  "SD","text,sans")
         , ("https://www.sciencedirect.com/science/article/pii/S0002929717301076",  "E","text")
         , ("https://www.sciencenews.org/article/sleep-debt-exacts-deceptive-cost", "SN","text,sans")
         , ("https://www.science.org/doi/10.1126/sciadv.aar3620",  "S","text")
         , ("https://www.scientificamerican.com/article/the-mind-of-an-octopus/", "SA", "text")
         , ("https://www.scottaaronson.com/democritus/", "S.A.", "text,sans")
         , ("https://www.sfgate.com/bayarea/article/test-lab-called-1-billion-over-budget-2921620.php", "SFG", "text,tri,sans")
         , ("https://www.smithsonianmag.com/history/native-intelligence-109314481/", "SM", "text")
         , ("https://www.spiegel.de/panorama/justiz/amokschuetze-von-muenchen-tatwaffe-aus-dem-darknet-a-1104461.html", "SPGL", "text,quad")
         , ("https://researchers.wls.wisc.edu/about/history/", "WLS", "text,tri,sans")
         , ("https://www.statnews.com/2021/11/09/largest-psilocybin-trial-finds-psychedelic-effective-treating-serious-depression/", "stat-news", "svg")
         , ("https://www.stuff.co.nz/manawatu-standard/news/69472334/kiwi-man-jailed-for-posting-drugs-from-las-vegas-to-mothers-house", "NZ", "text,sans")
         , ("https://www.supermemo.com/en/blog/twenty-rules-of-formulating-knowledge", "SM", "text,sans")
         , ("https://www.tandfonline.com/doi/abs/10.1080/02783190209554137", "T&F", "text,tri,sans")
         , ("https://www.technologyreview.com/2011/06/21/193829/the-measured-life/",  "T","text,sans")
         , ("https://www.teds.ac.uk/about-teds", "TEDS", "text,quad,sans")
         , ("https://www.telegraph.co.uk/culture/books/3601644/Adultery-was-his-thing.html", "the-telegraph", "svg")
         , ("https://www.tensorflow.org/tensorboard/get_started", "tensorflow", "svg")
         , ("https://www.theage.com.au/national/victoria/bitcoin-drug-millions-seized-in-victoria-20141015-116bby.html", "A", "text")
         , ("https://www.theatlantic.com/business/archive/2011/06/beware-the-stunning-pilot-program/240352/",  "A","text,italic")
         , ("https://magazine.atavist.com/whatsoever-things-are-true/", "Atvt", "text,quad")
         , ("https://magazine.atavist.com/whatsoever-things-are-true/", "Atvt", "text,quad")
         , ("https://read.atavist.com/american-hippopotamus", "Atvt", "text,quad")
         , ("https://www.thebeliever.net/mithradites-of-fond-du-lac/", "𝐁", "text")
         , ("https://www.thecut.com/2019/05/the-tinder-hacker.html", "TC", "text")
         , ("https://www.theguardian.com/books/2013/jul/10/man-behind-dickens-dostoevsky-hoax",  "the-guardian","svg")
         , ("https://www.thenewatlantis.com/publications/correlation-causation-and-confusion", "NA", "text")
         , ("https://www.theparisreview.org/blog/2018/04/25/the-strange-history-of-the-king-pine/",  "PR","text")
         , ("https://www.theverge.com/2021/8/7/22614450/unopened-copy-super-mario-bros-sells-2-million-record", "▽", "text")
         , ("https://www.thisfursonadoesnotexist.com/", "TFDE", "text,quad,sans")
         , ("https://www.thiswaifudoesnotexist.net/", "TWDE", "text,quad,sans")
         , ("https://www.tinyletter.com/",  "\9993","text")
         , ("https://www.tug.org/whatis.html", "tex","svg")
         , ("https://www.unqualified-reservations.org/2007/08/james-burnhams-dante-politics-as-wish/", "UR", "text")
         , ("https://www.unz.com/gnxp/through-the-wormhole-are-we-here-for-a-reason-premier-may-13th/", "RK", "text,sans")
         , ("https://www.uptontea.com/formosa-oolong-tea/taiwan-loose-leaf-oolong-tea/p/V00252/",  "upton-tea","svg")
         , ("https://www.vanityfair.com/news/2012/10/michael-lewis-profile-barack-obama",  "VF","text")
         , ("https://www.vice.com/en/article/aee8xa/the-silk-road-is-showing-cracks", "VICE", "text,quad,italic")
         , ("https://www.vice.com/en/article/paabgg/i-bought-a-book-about-the-internet-from-1994-and-none-of-the-links-worked", "VICE", "text,quad,italic")
         , ("https://www.vox.com/2015/5/27/8660249/bill-gates-spanish-flu-pandemic",  "Vox","text,tri,italic")
         , ("https://www.w3.org/International/wiki/Case_folding", "W3", "text,sans")
         , ("https://www.washingtonpost.com/graphics/2018/investigations/dog-auction-rescue-groups-donations/",  "washington-post","svg")
         , ("https://www.webcitation.org/6Qj7v6mqd",  "internet-archive","svg")
         , ("https://www.wired.com/2012/01/everything-about-learning/",  "wired","svg")
         , ("https://www.wired.co.uk/article/lsd-microdosing-drugs-silicon-valley", "wired","svg")
         , ("https://www.wolfewiki.com/pmwiki/pmwiki.php?n=Stories.SuzanneDelage", "U","text")
         , ("https://www.wsj.com/articles/SB10000872396390443696604577647870908169992",  "WSJ","text,tri")
         , ("https://www.wsj.com/articles/SB10001424052702303380004579521482247869874",  "WSJ","text,tri")
         , ("https://www.youtube.com/channel/UCeNwyKuv5SMnN6ovlpbz1SQ",  "youtube","svg")
         , ("https://www.youtube.com/watch?v=cG7v9eCq2u4&t=33m49s",  "youtube","svg")
         , ("https://www.yudkowsky.net/rational/technical", "yud", "svg")
         , ("https://xtools.wmcloud.org/pages/en.wikipedia.org/Gwern", "wikipedia","svg")
         , ("https://en.wikipedia.org/wiki/Named-entity_recognition", "wikipedia","svg")
         , ("https://en.wikipedia.org/wiki/Document_classification", "wikipedia","svg")
         , ("https://yunnansourcing.com/",  "ys","text")
         , ("https://yunnansourcing.us/", "ys","text")
         , ("http://thehub7dnl5nmcz5.onion/index.php?topic=2261.msg17459", "Hub", "text,tri,sans")
         , ("https://timetravel.mementoweb.org/",  "internet-archive","svg")
         , ("http://unenumerated.blogspot.com/2011/01/singularity.html", "N.S.", "text,sans")
         , ("http://www-biba.inrialpes.fr/Jaynes/cc18i.pdf", "ETJ", "text,tri,sans")
         , ("http://www.catb.org/jargon/html/R/religious-issues.html", "ESR","text,tri,sans")
         , ("https://www.cjas.org/~leng/daihist.htm", "NGE", "text,tri")
         , ("http://www.evalegend.com/interview_anno96.php", "NGE", "text,tri")
         , ("http://www.gainax.co.jp/wp/",  "NGE","text,tri")
         , ("http://www.jstor.org/stable/10.1086/468061", "JTOR", "text,quad")
         , ("https://www.metafor-project.org/doku.php",  "R","text")
         , ("https://paulgraham.com/hundred.html",  "pg","text,monospace")
         , ("http://www.scholarpedia.org/article/Applications_of_algorithmic_information_theory", "scholarpedia", "svg")
         , ("http://www.sequentialtart.com/archive/july00/grant.shtml", "ST", "text,sans")
         , ("http://www.thelancet.com/journals/lancet/article/PIIS0140-6736%2811%2960693-4/abstract", "L", "text")
         , ("http://www.usagi.org/doi/seiyuu/tv/1997eva.html", "NGE", "text,tri")
         , ("https://www.vetta.org/2009/12/tick-tock-tick-tock-bing/", "Legg", "text,quad,sans")
         , ("/lorem.md",  "txt","svg")
         , ("/static/build/anchor-checker.php",  "code","svg")
         , ("/static/build/hakyll.hs",  "code","svg")
         , ("/doc/darknet-market/2013-05-05-moore-bitcoinexchangesurvivalanalysis.R",  "code","svg")
         , ("/static/build/markdown-lint.sh",  "code","svg")
         , ("/static/css/links.css",  "code","svg")
         , ("/static/font/dropcap/de-zs/DeutscheZierschrift-M.ttf",  "misc","svg")
         , ("/static/img/favicon.ico",  "image","svg")
         , ("/static/img/icon/video.svg",  "image","svg")
         , ("/static/js/sidenotes.js",  "code","svg")
         , ("/static/nginx/twdne.conf",  "code","svg")
         , ("/static/template/default.html",  "code","svg")
         , ("https://solar.lowtechmagazine.com/2015/12/fruit-walls-urban-farming-in-the-1600s/", "☀", "text")
         , ("https://www.rollingstone.com/culture/culture-features/elon-musk-the-architect-of-tomorrow-120850/", "𝓡𝐒", "text")
         , ("https://ourworldindata.org/grapher/burden-disease-from-each-mental-illness", "OWID", "text,quad,monospace")
         , ("https://maggieappleton.com/bidirectionals", "maggie-appleton", "svg")
         , ("https://www.popsci.com/deadly-splinter-antibiotic-resistance/", "PS", "text,sans")
         , ("https://www.emacswiki.org/emacs/MarkdownMode", "emacs", "svg")
         , ("https://www.reddit.com/r/emacs/comments/1530yh8/kalman_reti_the_last_symbolics_developer_speaks/", "emacs", "svg")
         , ("https://www.chicagotribune.com/news/ct-xpm-2004-07-23-0407240014-story.html", "chicago-tribune", "svg")
         , ("https://www.theinformation.com/", "the-information", "svg")
         , ("https://www.semafor.com/article/03/24/2023/the-secret-history-of-elon-musk-sam-altman-and-openai", "SMFR", "text,quad")
         , ("https://www.nobelprize.org/?p=2688", "🏅", "text")
         , ("https://time.com/6337437/sam-altman-openai-fired-why-microsoft-musk/", "T", "text")
         , ("https://www.cnbc.com/2017/11/15/microsoft-and-github-unveil-pair-programming-tools.html", "CNBC", "text,quad,sans")
         , ("https://www.crunchbase.com/person/james-c-gaither", "cb", "text,sans")
         , ("https://www.tiktok.com/@dale_ebert/video/7301073510267407658", "tiktok", "svg")
         , ("https://www.scmp.com/news/china/science/article/3002346/chinas-first-cloned-police-dog-reports-duty", "SCM", "text,tri")
         , ("https://newcriterion.com/issues/2006/10/a-good-list", "NC", "text")
         , ("https://qntm.org/invisibility", "qntm", "text,quad,mono")
        ]

-- TODO: more complex link-icon testing: suppression of redundant link-icons
-- linkIcon $ Link nullAttr [Str "WSJ"] ("https://www.wsj.com/articles/world-chess-championship-magnus-carlsen-ian-nepomniachtchi-seconds-11638167905", "") →
-- Link ("",["icon-not"],[]) [Str "WSJ"] ("https://www.wsj.com/articles/world-chess-championship-magnus-carlsen-ian-nepomniachtchi-seconds-11638167905","")
