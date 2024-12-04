{-# LANGUAGE OverloadedStrings #-}
module Config.LinkIcon (prioritizeLinkIconMin, prioritizeLinkIconBlackList, overrideLinkIcons, linkIconTestUnitsText, linkIconRules, linkIconTypes) where

import qualified Data.Text as T (drop, isInfixOf, isPrefixOf, Text)

import Utils (extension, isLocal, hasExtension, isHostOrArchive)

-- hardwire globally icons for exact-matches of specific URLs (`[(URL, (Link icon, Link icon type))]`).
-- Useful in cases where we can't or won't edit the link directly to set link-icons
-- (eg. like in </fiction/clippy>, where I do a manual override of link-icons to rickroll the reader:
-- `[HQU Colab notebook](https://​tinyurl.com/hquv34 "Colab notebook: HQU-v3.4-light (Jax TPU)"){link-icon="alphabet" link-icon-type="svg" .link-live-not .archive-not}`.
-- If we couldn't for some reason, we can hardwire that exact URL here.)
overrideLinkIcons :: [(T.Text, (T.Text,T.Text,T.Text))]
overrideLinkIcons = [("/index#abstract", ("","",""))]

------------------------------------------------------------------------------------------

-- don't bother trying to come up with link-icons until a domain has been used repeatedly:
prioritizeLinkIconMin :: Int
prioritizeLinkIconMin = 4

-- TODO: icon+color:
-- color-only:
-- icon-only:
prioritizeLinkIconBlackList :: [T.Text] -- dead, icon-less, bad icon, overly-obscure, no real unifying nature worth knowing, etc:
prioritizeLinkIconBlackList = ["lilianweng.github.io", "www.smartpowders.com", "www.silverhandmeadery.com"
                              , "philip.greenspun.com", "eli.thegreenplace.net", "danluu.com"
 , "www.joelonsoftware.com", "www.jstage.jst.go.jp", "intrade.com", "abandonedfootnotes.blogspot.com", "arr.am"
 , "blog.johantibell.com", "humanvarieties.org", "cognitivefun.net", "annals.org", "www.replicatedtypo.com"
 , "www.baltimoresun.com", "www.aleph.se", "www.cs.virginia.edu", "www.incompleteideas.net"
 , "www.artbreeder.com"
 -- TODO:
 , "waifulabs.com", "practicaltypography.com", "danwang.co", "www.worldcat.org", "www.thestranger.com"
 , "www.nausicaa.net", "www.hindawi.com", "www.eugenewei.com", "www.buzzfeed.com", "web.mit.edu", "karpathy.github.io"
 , "hal.archives-ouvertes.fr", "demos.obormot.net", "blog.acolyer.org", "arbtt.nomeata.de"
 , "www.wakapoetry.net", "www.wunderground.com", "www.standard.co.uk", "www.rte.ie", "www.orlandosentinel.com"
 , "www.mercurynews.com", "www.links.org", "www.math.uwaterloo.ca", "sourceforge.net", "shkspr.mobi", "ro.ecu.edu.au"
 , "repository.upenn.edu","proceedings.neurips.cc","polisen.se", "latanyasweeney.org", "highnoongmt.wordpress.com"
 , "alumni.media.mit.edu", "ralphmerkle.com", "www.mentalfloss.com", "www.lightspeedmagazine.com", "ajp.psychiatryonline.org"
 , "agtb.wordpress.com", "aeon.co", "digitalcommons.unl.edu", "emilkirkegaard.dk", "gazette.com", "ohtori.nu"
 , "www.austlii.edu.au", "www.animenewsservice.com", "www.animeigo.com", "www.alexa.com", "vividness.live"
 , "thepharmacyexpress.com", "thegrandnarrative.com", "srconstantin.wordpress.com", "penelope.uchicago.edu"
 , "bmk.sh","www.jstatsoft.org","www.japantimes.co.jp","www.impactcybertrust.org", "www.ex.org", "www.eetimes.com"
 , "www.chronicle.com", "www.aging-us.com", "philpapers.org", "paulfchristiano.com", "parahumans.wordpress.com"
 , "mathworld.wolfram.com", "soranews24.com", "caniuse.com", "www.silcom.com", "esolangs.org"
 , "www.aiweirdness.com", "etherscan.io", "www.theringer.com", "cs.stanford.edu", "mmlab.ie.cuhk.edu.hk", "www.cs.toronto.edu"
 , "www.centauri-dreams.org", "www.alexirpan.com", "linuxmafia.com", "wiki.obormot.net", "www.marxists.org"
 , "takimag.com", "oll.libertyfund.org", "every.to", "www.eoht.info", "mssprovenance.blogspot.com"
 , "www.inverse.com", "hal.science", "www.findarticles.com", "super.gluebenchmark.com", "gluebenchmark.com"
 , "mattmahoney.net", "projecteuclid.org", "datacolada.org", "pubs.aip.org", "nyaa.si", "memteaimports.com"
 , "jetpress.org", "www.sudowrite.com", "tylervigen.com", "pubs.acs.org", "www.dafont.com", "geminiprotocol.net"
 , "www.1001fonts.com", "andrewmayne.com", "www.benkuhn.net", "sive.rs", "itre.cis.upenn.edu", "conservancy.umn.edu", "www.crd.york.ac.uk"
 , "nces.ed.gov", "www.avclub.com", "members.madasafish.com", "www.aeaweb.org", "linkinghub.elsevier.com"
 , "www.cs.cmu.edu", "minimaxir.com", "dynomight.net", "www.spencergreenberg.com", "www.ed.ac.uk"
 , "www.researchgate.net", "www.chiark.greenend.org.uk", "www.rug.nl", "figshare.com"]
------------------------------------------------------------------------------------------

-- all legal types of link-icon types displays
linkIconTypes :: [T.Text]
linkIconTypes = ["text", "svg"
                , "quad", "tri"
                , "sans", "serif"
                , "mono", "italic", "bold", "overline"
                , "" -- allowed for color-only links
                ]

-- Color hex code shortcuts:
blueDM, blueFB, blueG, bluePatio11, blueYahoo, brownAnthropic, greenNV, orangeNGE, purpleHaskell, redAdobe, redR, redTufte, yellowMoR, yellowPG :: T.Text
redAdobe = "#f40f02"
redR     = "#1b61b1"
redTufte = "#b1282b"
yellowMoR = "#ca9310"
yellowPG  = "#aa873b"
brownAnthropic = "#d4a27f"
orangeNGE = "#f71a00"
greenNV = "#77ba00"
blueDM      = "#4185f4"
blueFB      = "#1877f2"
blueG       = "#4285f4"
bluePatio11 = "#3498db"
blueYahoo   = "#669bb5"
purpleHaskell = "#5e5086"

-- Helper functions for URL matches:
u', u'' :: T.Text -> T.Text -> Bool
-- Loose check: simplest check for string anywhere; note that if it is a full domain name like `https://foo.com` (intended to match `https://foo.com/xyz.html`), then it will *not* match when the local-archive code fires and the URL gets rewritten to "/doc/foo.com/$HASH.html". So we error out if the user tries this, having forgotten that u' ≠ u'' in that respect.
u' ""  v  = error $ "Config.LinkIcon.u': empty string passed as argument; url was '', domain was " ++ show v
u' url "" = error $ "Config.LinkIcon.u': empty string passed as argument; url was " ++ show url ++ ", domain was ''"
u' url v = if "http://" `T.isPrefixOf` v || "https://" `T.isPrefixOf` v
           then error ("Config.LinkIcon: Overly strict prefix in infix matching (u'): " ++ show url ++ ":" ++ show v)
           else v `T.isInfixOf` url
-- Stricter check: more stringent check, matching exactly the domain name:
u'' ""  v  = error $ "Config.LinkIcon.u'': empty string passed as argument; url was '', domain was " ++ show v
u'' url "" = error $ "Config.LinkIcon.u'': empty string passed as argument; url was " ++ show url ++ ", domain was ''"
u'' url v = if "http://" `T.isPrefixOf` v || "https://" `T.isPrefixOf` v
            then error ("Config.LinkIcon: Overly strict prefix in infix matching (u''): " ++ show url ++ ":" ++ show v)
            else Utils.isHostOrArchive v url

iE :: T.Text -> [T.Text] -> Bool
iE ""  args = error $ "Config.LinkIcon.iE: passed an empty string as the first argument; rest were: " ++ show args
iE url args = T.drop 1 (extension url) `elem` args
aU', aU'' :: T.Text -> [T.Text] -> Bool
aU'  url = any (u'  url)
aU'' url = any (u'' url)

-- The URL matches:
linkIconRules, linkIconRulesOverrides, linkIconRulesSingle, linkIconRulesDouble, linkIconRulesTriple, linkIconRulesQuad,
  linkIconRulesSVG, linkIconRulesFiletypes, linkIconRulesColors :: T.Text -> (T.Text, T.Text, T.Text)
-- run all the rules in order, and take the first one that returns a non-empty tuple (because it matched):
linkIconRules "" = error "Config.LinkIcon: linkIconRules: passed an empty URL; this should be impossible!"
linkIconRules u = let result = filter (/=("","","")) $ map (\f -> f u) [linkIconRulesOverrides, linkIconRulesSingle, linkIconRulesDouble,
                                                                linkIconRulesTriple, linkIconRulesQuad, linkIconRulesSVG, linkIconRulesFiletypes, linkIconRulesColors]
                               in if null result then ("","","") else head result

linkIconRulesOverrides "" = error "Config.LinkIcon.linkIconRulesOverrides: passed empty string as the URL; this should never happen!"
-- organizational mentions or affiliations take precedence over domain or filetypes; typically matches anywhere in the URL. This must be matched first.
linkIconRulesOverrides u
 | u' u "deepmind"  = ("deepmind", "svg", blueDM) -- DeepMind; match articles or anchors about DM too. Primary user: <deepmind.com>, <deepmind.google>, DM papers on Arxiv (`#deepmind` & `org=deepmind`)
 | u' u "schmidhuber" || u' u "people.idsia.ch/~juergen/" = ("SMDH", "text,quad,sans", "") -- Jürgen Schmidhuber homepage & tagged links; should override Arxiv, Bloomberg, NYT, Facebook etc
 | u' u "facebook" || u' u ".fb.com"  = ("facebook", "svg", blueFB)
 | u' u "sites.google.com/berkeley.edu" || aU'' u ["ml.berkeley.edu", "people.eecs.berkeley.edu", "bair.berkeley.edu"] = ("BAIR", "text,quad,mono", "") -- Berkeley AI Research: Chelsea Finn, Sergey Levine, robotics, Decision Transformer, etc. Overrides Google Sites.
 | u' u "pandoc" && not (u' u "tufte") = ("PNDC", "text,quad,sans", "") -- general catch-all, overrides Google Groups (`pandoc-discuss`), with a further override for Tufte-related stuff (which get 'ET')
 | aU'' u ["www.edwardtufte.com", "edwardtufte.github.io"] || aU' u ["github.com/edwardtufte/", "github.com/jez/tufte-pandoc-jekyll", "github.com/jez/tufte", "github.com/clayh53/tufte", "/doc/design/typography/rubrication/1990-tufte-envisioninginformation-ch5-byrneseuclid.pdf", "cran.r-project.org/web/packages/tufte/"] = ("ET", "text", redTufte) -- override CRAN
 | u'' u "groups.google.com" = ("✉", "text", blueG)
 | u'' u "scholar.google.com" || u'' u "research.com" = ("google-scholar", "svg", blueG) -- Google Scholar.
 | u'' u "docs.google.com" = ("word-doc", "svg", blueG)
 | u'' u "www.theinformation.com" = ("the-information", "svg", "#f32a52") -- <https://en.wikipedia.org/wiki/The_Information_(website)> <https://en.wikipedia.org/wiki/File:The_Information_logo.svg> <https://ti-assets.theinformation.com/assets/favicon_prod/safari-pinned-tab-bef60b8749b324326ffc2c49b9f5ab190b1ab3e10c5ecd33bbc710838bc84e72.svg> Some sort of Greek capital letter 'I'? Overrides Microsoft & other tech companies
 | u'' u "www.semafor.com" = ("SMFR", "text,quad", "#ffe1be") -- Semafor <https://www.semafor.com/> <https://en.wikipedia.org/wiki/Semafor_(website)>; somewhat like _The Information_; official logo is a boring serif wordmark (<https://en.wikipedia.org/wiki/File:Semafor_logo.png>), and the favicon is an interesting 'cut off' 'S'-silhouette-in-square <https://www.semafor.com/safari-pinned-tab-icon.svg> but they've done a bad enough job branding it no one would recognize it, so we use a quad-abbreviation of 'SEMAFOR'; color: salmon yellow
 | u' u "google" || u'' u "magenta.tensorflow.org" = ("alphabet", "svg", blueG) -- Google searches, other tools. Note that there are many Google subdomains, which we may wish to iconify differently, so we narrow down with just ‘www’. Google Brain doesn’t have any consistent or recognizable logo, don’t bother trying to replicate one of the dots (no one will recognize it); use ‘GB’ would not be a bad idea, but I suspect that would also confuse people. So reusing the ‘G’ is the least bad option. [the SVG has been renamed 'alphabet' instead of the expected 'google' because two default uBlock lists block the regexp 'icons/google.*' as it is usually abused for social-media spamming icons]
 | aU' u ["x.com/sigfpe/", "blog.sigfpe.com", "github.com/dpiponi"] = ("sgfp", "text,quad,mono", "") -- sigfpe/Dan Piponi: Haskell, math, computer graphics etc
 | u' u "nvidia"  || aU'' u ["nvlabs.github.io", "nv-adlr.github.io", "nv-tlabs.github.io"] = ("n", "text,sans,italic", greenNV) -- Nvidia: <https://en.wikipedia.org/wiki/Nvidia#cite_note-2> yeah no. Disambiguate from Nature's "n" by italicizing (Nvidia *did* italicize the lowercase 'n' for a long time, so seems reasonable); color: green
 | aU'' u ["gptprompts.wikidot.com"] || aU' u ["openai.com", "#openai", "org=openai", "chatgpt.com"] = ("openai", "svg", "") -- OpenAI; match articles or anchors about OA too. primary user: openai.com, Arxiv papers. Brockman's GPT-prompts wiki is semi-official IMO.
 | aU' u ["microsoft.com", "#microsoft", "org=microsoft", "github.com/microsoft/"] = ("MS", "text,sans,italic", "") -- Microsoft: I don’t think <https://en.wikipedia.org/wiki/File:Microsoft_logo_(2012).svg> is all that recognizable, so make a logotype more like <https://en.wikipedia.org/wiki/File:Microsoft_logo_(1987).svg>: an italic sans "MS". color: none, because the MS window pane icon uses red/green/yellow/blue, none dominating, and I can't replicate that; TODO: color SVG icon
 | u' u "#anthropic" || u' u "x.com/jackclarkSF/" || aU'' u ["transformer-circuits.pub", "www.anthropic.com", "jack-clark.net", "/doc/ai/nn/anthropic/"] = ("anthropic", "svg", brownAnthropic) -- need to override Arxiv; handle Jack Clark (co-founder) newsletter & social media. color: Claude brown
 | u' u "#laion"  || u' u "LAION-AI" || u'' u "laion.ai" = ("laion", "svg", "#1d374e") -- <https://laion.ai/favicon.svg>; need to override Arxiv & Github & Hugging Face; color: dark blue
 | aU'' u ["blog.givewell.org", "www.givewell.org", "files.givewell.org"] || u' u "groups.yahoo.com/group/givewell/" = ("GW", "text", blueYahoo) -- override Yahoo! email; color: light blue
 | otherwise = ("","", "")

linkIconRulesSingle "" = error "Config.LinkIcon.linkIconRulesSingle: passed empty string as the URL; this should never happen!"
linkIconRulesSingle u
 | aU'' u ["psyarxiv.com", "files.osf.io", "osf.io"] = ("ψ", "text", "#cf1d35") -- Unicode trickery icons: GREEK SMALL LETTER Ψ. Color: red. TODO: color SVG icon?
 | u'' u "unsongbook.com" = ("ℵ", "text", "#b47810") -- SSC’s book: (ℵ) ALEF SYMBOL (We use the math symbol instead of the Hebrew deliberately, to avoid triggering bizarre Hebrew bidirectional text-related layout bugs on Mac Firefox.); color: mustard yellow background; TODO: color SVG icon?
 | u'' u "meltingasphalt.com" = ("▲", "text", "#aa0000") -- Kevin Simler’s Melting Asphalt blog uses 3 triangles but that's too many, so we just use one. (▲) BLACK UP-POINTING TRIANGLE; color: red
 | u'' u "www.tinyletter.com" = ("✉", "text", "#e72223") -- TinyLetter’s icon, without color, isn’t memorable enough; throw in the other email services (✉) ENVELOPE; color: red (from heart-envelope icon)
 | u'' u "groups.yahoo.com" = ("✉", "text", "#5e21cf") -- color: Yahoo dark purple
 | u'' u "www.mail-archive.com" = ("✉", "text", "#a01e1e") -- color: red
 | u' u "carryiton.net/chain-letter/" = ("✉", "text", "") -- linked only for the archive, so this is an appropriate icon
 | u'' u "www.forbes.com" = ("F", "text", "#dc0000") -- red capital F serif; color: red
 | u'' u "fortune.com" = ("F", "text,sans", "#dc0000") -- red capital F *sans* (good god, could Forbes/Fortune be *any more* indistinguishable or boring or bland?)
 | (u' u "haskell.org" && (extension u /= ".hs")) || u' u "haskellers.com" = ("𝛌", "text", purpleHaskell) -- Haskell: simplify logo; the double-lambda is too busy when used for link icons (𝛌) MATHEMATICAL BOLD SMALL LAMBDA \120524 primary user: hackage.haskell.org; we make an exception for .hs files hosted on Haskell.org, like config files, where the source code-ness is more relevant than the organization/domain; color: faded purple
 | u' u "cerebras" = ("C", "text,sans", "#f05a29") -- Cerebras: <https://www.cerebras.net/>, <#cerebras> affiliation; overrides Arxiv.org; the Cerebras logo is a odd set of 4 semi-concentric circles <https://cerebras.ai/wp-content/uploads/2022/03/cerebras-white-01.png> which is both unfamiliar/odd and looks terrible at link-icon resolution (like some sort of small animal like a chipmunk or gerbil)
 | u'' u "arxiv.org" || u'' u "proceedings.mlr.press" || u'' u "aclanthology.org" = ("𝛘", "text", "#b31b1b") --  ArXiv: Their skull+smiley logo is too bizarre & off-putting to use, in addition to not working as a tiny monochrome image (𝛘) MATHEMATICAL BOLD SMALL CHI (bold makes it show up better when tiny); I lump in 'ACL' & 'PMLR' ("Proceedings of Machine Learning Research", "") because many PMLR were just Arxiv preprints beforehand & it amounts to about the same thing, really. color: red
 | u' u ".bloomberg.com" || u'' u "www.businessweek.com" = ("𝐁", "text", "") -- Bloomberg: no usable logo, just an inset-B (𝐁) MATHEMATICAL BOLD CAPITAL B; TODO: white-on-black text background
 | u' u "theatlantic.com" = ("A", "text,italic", "#e7131a") -- The Atlantic: replicate sloping by italics
 | u'' u "www.dailymail.co.uk" = ("𝔐", "text", "#004db3") -- 𝔐 MATHEMATICAL FRAKTUR CAPITAL M; color: blue
 | aU'' u ["danbooru.donmai.us", "derpibooru.org", "safebooru.org"] = ("❐", "text", "#ba9570") -- ❐ U+2750 UPPER RIGHT DROP-SHADOWED WHITE SQUARE; color: brown
 | u'' u "www.edge.org" = ("E", "text,italic", "#414143") -- color: dark purple
 | u'' u "www.economist.com" = ("E", "text,sans", "#e3120b") -- Economist: logo is just ‘Economist’… There is a sibling magazine <https://en.wikipedia.org/wiki/1843_(magazine)> which I don't seem to link to.; color: red; TODO: white-on-red text background
 | u'' u "www.sciencedirect.com" = ("E", "text", "#eb6500") -- Elsevier/Sciencedirect.com: also an ‘E’; color: orange
 | u'' u "www.esquire.com" = ("ℰ", "text", "#ff3a30") -- color: orange
 | u'' u "www.harney.com" = ("H", "text", "#c9ad54") -- The Harney & Sons logo is too fancy to scale down reasonably; color: tan yellow
 | u'' u "www.hustwit.com" = ("H", "text,sans", "") -- design documentarian
 | u'' u "www.longecity.org" = ("⧖", "text", "") -- Longecity “⧖” U+29D6 WHITE HOURGLASS UNICODE
 | u' u ".nature.com" = ("n", "text", "") -- Nature; color: none, website sometimes uses blue but not seriously
 | u'' u "www.theverge.com" = ("▽", "text", "#5200ff") -- The Verge uses a sort of delta Escher triangle-esque 'V' stylization <https://en.wikipedia.org/wiki/The_Verge> which looks like a triangle pointing down, so, ▽ WHITE DOWN-POINTING TRIANGLE (Nabla operator) &#x25BD; &#9661;. color: purple
 | u'' u "www.quora.com" = ("Q", "text", "#b92b27") -- surprisingly, no one's taken 'Q' yet; color: red
 | aU'' u ["cran.r-project.org", "www.r-project.org", "lme4.r-forge.r-project.org", "www.metafor-project.org", "rstudio.com"] || u' u "github.com/paul-buerkner/brms" = ("R", "text", redR) -- R: at this point R Studio has taken over a lot of control of the R ecosystem, so might as well treat them as official too… primary user: cran.r-project.org; color: red
 | u'' u "www.science.org" || u'' u "sciencemag.org" = ("S", "text", "#ca2015") -- Science is just typeset in red; color: red; TODO: white-on-red text background
 | u'' u "slate.com" = ("S", "text,sans", "#2c0022") -- color: dark purple; TODO: white-on-purple text background
 | u'' u "www.salon.com" = ("s", "text", "#ed2c1d") -- color: red
 | u'' u "www.technologyreview.com" = ("T", "text,sans", "") -- Technology Review (their logo has a little slash in it which you probably can’t see at low-res) but is otherwise just a ‘T’ so meh; color: none, no consistent use
 | aU'' u ["time.com", "healthland.time.com"] = ("T", "text", "#e90606") -- Time Magazine; color: red
 | aU'' u ["www.urth.net", "lists.urth.net", "www.wolfewiki.com"] = ("U", "text", "") -- Gene Wolfe mailing list; no logo; primary user: lists.urth.net
 | u' u "onlinelibrary.wiley.com" = ("W", "text,sans", "") -- Wiley & Sons’s ‘W’ unfortunately overlaps with the WP ‘W’ but if we sans it, maybe that’ll help. primary user: onlinelibrary.wiley.com; TODO: white-on-black text background
 | aU' u ["longbets.org", "longnow.org", "rosettaproject.org", "theinterval.org"] = ("X", "text,overline", "#2a393d") -- Long Now Foundation projects; color: very dark graphite bluel; TODO: white-on-blue text background
 | u'' u "predictionbook.com" = ("?", "text,sans,bold", "#695173") -- PB logo is confusing. A purple question mark…? color: light purple
 | u'' u "beepb00p.xyz" = ("\129302", "text", "") -- ROBOT FACE U+1F916
 | u'' u "antilop.cc" = ("෴", "text", "") -- SINHALA PUNCTUATION KUNDDALIYA 0x0DF4 - because it's written by "Moustache", get it
 | u'' u "forum.effectivealtruism.org" || u'' u "www.effectivealtruism.org" = ("EA", "text", "#06819a") -- color: blue-green from lightbulb
 | u'' u "boards.fireden.net" || u'' u "archive.foolz.us" || u' u "4channel.org" || u' u "boards.4chan.org"  = ("4CH", "text,sans", "#69ac46") -- color: light green
 | u'' u "www.kaggle.com" = ("k", "text,sans", "#20beff") -- color: blue-green
 | u'' u "www.discovermagazine.com" = ("D", "text", "") -- TODO: white-on-black text background text
 | u'' u "www.mirror.co.uk" = ("M", "text,sans", "#e90e0e") -- color: red; TODO: white-on-red text background
 | aU'' u ["en.touhouwiki.net", "touhou.fandom.com", "w.atwiki.jp"] || u' u "www.reddit.com/r/TOUHOUMUSIC/" = ("☯", "text", "#e44031") -- NOTE: override Fandom catch-all; color: surprisingly, AFAICT, there is no official Touhou logo or icon or color, not even Reimu's miko red; so we'll just pick an arbitrary bright red
 | u'' u "www.reuters.com" = ("R", "text,sans", "#e56218") -- the official Reuters logo <https://en.wikipedia.org/wiki/File:Reuters_Logo.svg> looks like it's summoning a seraphim; color: red
 | u'' u "www.theage.com.au" = ("A", "text", "") -- TODO: white-on-black text background
 | u'' u "www.candyjapan.com"  = ("🍬", "text", "#e0423e") -- color: red
 | aU'' u ["www.cambridge.org", "journals.cambridge.org", "static.cambridge.org"] = ("⛨", "text", "#ce0101") -- ⛨ BLACK CROSS ON SHIELD U+26E8, roughly imitating <https://en.wikipedia.org/wiki/Coat_of_arms_of_the_University_of_Cambridge>; color: red
 | u'' u "www.nzherald.co.nz" = ("𝕳", "text", "")
 | u'' u "nintil.com" = ("𝓝", "text", "#c05b4d") -- @ArtirKel José Luis Ricón Fernández de la Puente
 | u'' u "www.brookings.edu" = ("B", "text", "#003a70")
 | u'' u "variety.com" = ("𝓥", "text", "") -- color: none
 | u'' u "theconversation.com" = ("🗨", "text", "#d8352a")
 | u'' u "patch.com" = ("P", "text,sans", "#005d8f") -- color: blue
 | u'' u "thegradient.pub" = ("∇", "text", "") -- TODO: white-on-black text background
 | u'' u "www.projectrho.com" = ("ρ", "text", "#00fcfa") -- color: radioactive neon green
 | u'' u "harpers.org" = ("H", "text", "") -- color: none; Harpers historically sometimes used rubrication or red, but not now
 | u'' u "www.thelancet.com" = ("L", "text", "#004582") -- color: blue; TODO: white-on-blue text background
 | u' u "github.com/huggingface/" || u' u "medium.com/huggingface/" || u'' u "huggingface.co" = ("\129303", "text", "#ffcc4d") -- "🤗" HUGGING FACE U+1F917; color: emoji yellow
 | u'' u "www.pragmatic.ml" = ("𝕄", "text", "") -- Madison May, machine learning blog; color: none (too many)
 | u'' u "www.outsideonline.com" = ("𝕆", "text", "#ffd204") -- imitate the shadowing on Outside Online's 'O' <https://www.outsideonline.com/wp-content/uploads/2021/07/favicon-194x194-1.png>; color: yellow
 | u'' u "norvig.com" = ("N", "text,sans", blueG) -- Google Director of Research <https://en.wikipedia.org/wiki/Peter_Norvig>; <https://norvig.com/favicon.ico> is actually instantly recognizable & well-chosen, but unfortunately, only works because of the *colors*... so we'll settle for a plain sans capital N.
 | u'' u "novelai.net" || u'' u "blog.novelai.net" = ("🖋", "text", "") -- LOWER LEFT FOUNTAIN PEN (U+1F58B); NovelAI logo is a fountain pen nib.
 | u'' u "www.thebeliever.net" = ("𝐁", "text", "") -- _The Believer_ magazine <https://en.wikipedia.org/wiki/The_Believer_(magazine)>, McSweeney's spinoff (formerly <https://believermag.com>): logo is a dropshadow serif capital B logo
 | u'' u "solar.lowtechmagazine.com" = ("☀", "text", "") -- Low Tech Magazine (U+2600 BLACK SUN WITH RAYS)
 | u'' u "www.nobelprize.org" = ("🏅", "text", "#cc9b40") -- Nobel Prize, SPORTS MEDAL; color: copper-gold
 | u'' u "waitbutwhy.com" = ("♔", "text", "#fd992c") -- Wait But Why: longform blog: logo is a playing card king (black, king of clubs?); approximate it with a "♔" WHITE CHESS KING (BLACK CHESS KING looks like a blob at link-icon size). If that doesn't work, a 'WBW' tri-text icon is feasible. color: orange
 | u'' u "senseis.xmp.net" = ("❍", "text", "") -- Sensei's Library (Go wiki); Unicode: SHADOWED WHITE CIRCLE U+274D; we can't use a solid black/white circle to represent a Go stone, because then how would it look in dark-mode vs light-mode? However, a 'shadowed' circle' ought to be legible in both. (The official icon is some horrible cartoon character, and the wordmark is 'SL' with 2 red lines, which is unfamiliar and hard to replicate well, while a 'Go stone' lets me lump in other Go websites as need be.)
 | u'' u "messybeast.com" = ("🐾", "text", "#fafa02") -- Sarah Hartwell's cat compilations; color: yellow (from <http://messybeast.com/favicon.ico>); use Unicode PAW PRINTS for now because not sure I can clean up the logo. TODO: black-on-yellow text background
 | u'' u "www.animesuki.com" || u'' u "forums.animesuki.com" = ("̅□", "text", "#008080") -- color: teal geometric icon; didn't try to replicate as an SVG, but settled for WHITE SQUARE + COMBINING OVERLINE
 | u'' u "www.theregister.com" = ("𓅐", "text", "#ff0000") -- The Register: color, red; icon: vulture (EGYPTIAN HIEROGLYPH G014)
 | u'' u "ki.se" = ("☤", "text", "#830154") -- Karolinska Institute <https://en.wikipedia.org/wiki/Karolinska_Institute>; we skip openarchive.ki.se because it's just PDFs; icon: complex <https://en.wikipedia.org/wiki/File:Karolinska_Institutet_seal.svg>, but the caduceus seems recognizable; color: purple
 | u'' u "www.e-codices.unifr.ch" = ("e", "text,sans", "#e7d7a5") -- e-codices - Virtual Manuscript Library of Switzerland; color: yellow; 'e' circular logo illegible so just an 'e'
 | u'' u "www.bartleby.com" = ("b", "text", "#101269") -- Bartleby: no good logo, favicon is just a blue 'b' (sort weird serif)
 | u'' u "www.smh.com.au" = ("S", "text", "#096dd2") -- The Sydney Morning Herald: should be a fancy fraktur S but the Unicode renders wrong, and none of the MATHEMATICAL * CAPITAL S Unicode points look right either, so we just use 'S'; color: dark blue
 | u'' u "www.mangaupdates.com" = ("M", "text,sans", "#f8922b") -- Baka-Updates: 'M', color: yellow
 | u'' u "www.instructables.com" = ("🤖", "text", "#fac62d") -- Autodesk Instructables: robot logo, color: yellow
 | u'' u "mujoco.org" = ("M", "text,sans", "#0053d6") -- MuJoCo: advanced physics simulator used heavily in reinforcement learning research; logo just a 'M' on dark blue; color: white on blue
 | u'' u "www.artbreeder.com" = ("✤", "text", "#8ccaff") -- Ganbreeder/Artbreeder: three-lobed icon hard to replicate, HEAVY FOUR BALLOON-SPOKED ✱ is closest I got; color: light-blue
 | otherwise = ("", "", "")

linkIconRulesDouble "" = error "Config.LinkIcon.linkIconRulesDouble: passed empty string as the URL; this should never happen!"
linkIconRulesDouble u
 | aU'' u ["marginalrevolution.com", "conversationswithtyler.com"] = ("M𝐑", "text", "#00c79f") -- MR: cheaper to abuse Unicode (𝐑) MATHEMATICAL BOLD CAPITAL R; color: light green; TODO: color SVG icon for background?
 | u'' u "www.frontiersin.org" = ("FS", "text,sans", "") -- <https://en.wikipedia.org/wiki/Frontiers_Media> multiple-cubes logo too busy for an icon, no Unicode equivalent; color: none, too busy
 | aU'' u ["www.gutenberg.org", "gutenberg.ca", "gutenberg.net.au", "www.fadedpage.com"] = ("PG", "text", yellowPG) -- Faded Pages isn't strictly-speaking a Project Gutenberg org, but they work with Distributed Proofreaders & their work is in PG Canada and they do similar things so meh.; color: mustard yellow
 | u'' u "guzey.com" = ("A.G.", "text,sans", "")
 | u' u "alignmentforum.org" || (u'' u "www.greaterwrong.com" && u' u "view=alignment-forum") = ("AF", "text,sans", "#3f51b5") -- color: royal blue
 | u'' u "boingboing.net" = ("bb", "text,mono", "#ff0202") -- color: bright red
 | u'' u "nymag.com" = ("𝒩𝒴", "text", "") -- color: none. (It does use rubrication a little but not as part of the branding.)
 | u'' u "thebrowser.com" = ("TB", "text", "#ff9900") -- color: giraffe yellow
 | u'' u "crookedtimber.org" = ("CT", "text", "")
 | u' u ".latimes.com" = ("𝔏A", "text", "")
 | u'' u "everything2.com" = ("E2", "text", "#38495e") -- color: very pale blue
 | u'' u "examine.com" = ("Eχ", "text,sans", "#5e3b76")
 | aU'' u ["wiki.evageeks.org","forum.evageeks.org","www.evamonkey.com"] || u' u "x.com/EvaMonkey/" = ("EG", "text", orangeNGE) -- Evangelion: we’ll split this into EGF-related and other NGE sites; color: NGE orange (none of their own, because the main site / wiki / forum all have different colors, so just copy the NGE one)
 | u' u "mozilla.org" = ("FF", "text,sans", "#e66000") -- none of the available Firefox SVG logos worked well as a link icon; typically, too much detail, the swirly-spikes too indistinct & under-emphasized, and confusable with DeepMind. color: orange-red (the swirly Firefox has several shades of orange/yellow; went with Spanish Orange, the darkest, because link-icons are so small which usually means colors look lighter)
 | u'' u "www.goodreads.com" = ("GR", "text,sans", "#8a5e4a") -- GoodReads: logo doesn’t make sense as a grayscale; color: light brown
 | u'' u "kk.org" = ("KK", "text,sans", "#f4eb00") -- Kevin Kelly; color: yellow (book covers like _Cool Tools_ or the 'KK' favicon seem to favor a bright yellow)
 | aU'' u ["www.lesswrong.com", "sl4.org", "wiki.lesswrong.com", "www.greaterwrong.com"] = ("LW", "text", "#7faf83") -- LW logo is just a colored ‘LW’, so no point in converting. Other user: wiki.lesswrong.com. Color: green.
 | aU'' u ["michaelnielsen.org", "quantum.country", "numinous.productions", "cognitivemedium.com", "neuralnetworksanddeeplearning.com"] = ("MN", "text", "") -- color: none (each project has a different color)
 | u'' u "www.motherjones.com" = ("MJ", "text,sans", "")
 | u'' u "openreview.net" = ("OR", "text,sans", "#8c1b13") -- doesn't seem to have any real logo or wordmark: <https://openreview.net/about>; color: red (wonder if it's supposed to look like Arxiv?)
 | u'' u "www.overcomingbias.com" || u' u "mason.gmu.edu/~rhanson/" = ("OB", "text", "#263f5d") -- OB logo too bad to use; color: light royal blue (current Substack theme seems to be a light royal blue, by extending the Odysseus-bound-to-mast water color)
 | u'' u "www.theparisreview.org" = ("PR", "text", "") -- The Paris Review: not even going to try to make their weird bird logo work; color: none (does use rubrication sometimes but not particularly consistently)
 | u'' u "www.sciencedaily.com" = ("SD", "text,sans", "#004276") -- color: darker blue
 | u'' u "www.sciencenews.org" = ("SN", "text,sans", "#225483") -- <https://en.wikipedia.org/wiki/Science_News>; color: medium blue
 | u'' u "sethroberts.net" = ("SR", "text,sans", "") -- Logo is a sans 'S' on a red circle background; can't use 'S' because already used by Slate. color: none (no one would recognize it anymore so why bother)
 | u'' u "scholars-stage.org" = ("Ss", "text", "#3f1d0b") -- Avoid the unfortunate connotations of ‘SS’; color: brown
 | u'' u "tvtropes.org" = ("TV", "text", "#1c6486") -- TV Tropes: their lampshade icon is unrecognizable & hard to see small; color: bright blue (very washed out blue, so switched to a brighter blue also on their homepage)
 | u'' u "www.vanityfair.com" = ("VF", "text", "#e7131a") -- color: bright red
 | u'' u "yunnansourcing.com" || u'' u "yunnansourcing.us" = ("ys", "text", "#e99114") -- color: mustard yellow
 | u'' u "memteaimports.com" = ("MT", "text,sans", "#951d1f") -- color: dark copper red
 | u'' u "www.jneurosci.org" = ("JN", "text", "#104b7d") -- color: royal blue
 | u'' u "tl.net" = ("TL", "text,sans", "#254673") -- color: royal blue
 | u'' u "www.businessinsider.com" = ("BI", "text,sans", "#002aff") -- color: bright blue
 | u'' u "dnstats.net" = ("dn", "text,sans", "")
 | u'' u "www.newsweek.com" = ("NW", "text", "#f72210") -- logo is 'N' but there are too many 'N's floating around, so abbreviate 'Newsweek' as 'NW'; color: bright red
 | u'' u "www.thecut.com" = ("TC", "text", "")
 | u'' u "www.scientificamerican.com" = ("SA", "text", "#0376a1") -- color: medium blue
 | u'' u "www.stuff.co.nz" = ("NZ", "text,sans", "#d1a3ff") -- even their official name 'Stuff' is lazy and unmemorable. I just think of them as 'that New Zealand website reporting on crime & DNM stuff'… color: light purple
 | u'' u "chronopause.com" = ("M.D.", "text,sans", "") -- Mike Darwin, similarly TODO: experiment with initials using periods - does this work as-is? How about quad? '﹒' SMALL FULL STOP U+FE52 does not work.
 | u'' u "vitalik.eth.limo" || u' u "/doc/economics/mechanism-design/quadratic-voting/2018-buterin.pdf" = ("V.B.", "text,sans", "#337ab7") -- Vitalik Buterin, similarly; color: light blue (possibly from the Ethereum logo...?)
 | u'' u "unenumerated.blogspot.com" || u' u "szabo.best.vwh.net" || u' u "nick-szabo" = ("N.S.", "text,sans", "") -- Nick Szabo
 | u'' u "scottaaronson.blog" || u'' u "www.scottaaronson.com" = ("S.A.", "text,sans", "#4181b7") -- Scott Aaronson; color: light blue
 | u'' u "www.rifters.com" = ("P.W.", "text,sans", "#737500") -- Peter Watts; color: bright cyberpunk electronic yellow
 | u'' u "www.antipope.org" = ("C.S.", "text,sans", "#921712") -- Charles Stross; color: dark red
 | u'' u "www.ribbonfarm.com" = ("ℝ𝔽", "text,sans", "") -- color: none (too inconsistent over the years)
 | u'' u "www.deviantart.com" = ("DA", "text,sans", "#00fe8c") -- the official logo <https://en.wikipedia.org/wiki/File:DeviantArt_Logo.svg> isn't *too* bad and is at least 8 years old, but I don't recognize it so I doubt most readers would. color: light green (the brightest lightest green part of the transition fades)
 | u'' u "www.smithsonianmag.com" = ("SM", "text", "")
 | u'' u "scienceblogs.com" = ("Sᵇ", "text,sans,italic", "")
 | u'' u "www.dailydot.com" = ("D.", "text,sans", "#2a9461") -- color: medium green
 | u'' u "www.johndcook.com" = ("JC", "text,sans", "#1ab6f1") -- color: bright blue
 | u' u "royalsocietypublishing.org" = ("RS", "text", "#d31245") -- <https://en.wikipedia.org/wiki/Royal_Society>; color: pinkish-red
 | u'' u "www.sequentialtart.com" = ("ST", "text,sans", "#ff0000") -- color: bright comic red
 | u'' u "www.psychologytoday.com" = ("PT", "text,sans", "#477be4") -- color: bright blue; TODO: white-on-blue text background
 | u'' u "www.independent.co.uk" = ("TI", "text", "") -- <https://en.wikipedia.org/wiki/File:The_Independent_news_logo.svg> swooping-hawk icon would be illegible as link icon
 | u'' u "www.fastcompany.com" = ("FC", "text", "")
 | u'' u "elifesciences.org" = ("eL", "text,sans", "")
 | u'' u "www.w3.org" = ("W3", "text,sans", "#005a9c") -- color: dark blue
 | u'' u "www.metafilter.com" || u'' u "ask.metafilter.com" = ("MF", "text,sans,italic", "#065a8f") -- color: dark blue; TODO: white-on-blue text background (or more elaborately, green-white 'MF' on blue background square?)
 | u'' u "qz.com" = ("QZ", "text,sans", "#105b8e") -- color: dark blue
 | u'' u "blog.23andme.com" || u'' u "23andme.com" = ("23", "text", "#a40e7b") -- color: dark purple
 | u'' u "www.ft.com" = ("FT", "text", "#e3b68e") -- Financial Times; color: tan (official tan too light to see, so manually darkened)
 | u'' u "techcrunch.com" = ("TC", "text,mono", "#0a8935") -- color: green
 | u' u "livejournal.com" = ("LJ", "text,sans", "#004359") -- color: dark blue
 | u'' u "www.newscientist.com" = ("NS", "text,sans", "")
 | u'' u "www.palladiummag.com" = ("Pd", "text,sans", "") -- "P" is their logo but that is too generic and collides, so take 'palladium is a catalyst' literally & use the element abbreviation
 | u'' u "www.gq.com" = ("GQ", "text,sans", "#c6a348") -- color: light mustard yellow (from the 'Q' in 'GQ')
 | u'' u "foreignpolicy.com" = ("FP", "text", "#ed3725") -- color: bright red
 | u'' u "www.unqualified-reservations.org" = ("UR", "text", "")
 | u'' u "www.thenewatlantis.com" = ("NA", "text", "#2c6cbd") -- color: medium blue (issues are inconsistent, but predominantly some sort of medium blue)
 | aU'' u ["www.supermemo.com", "super-memory.com"] = ("SM", "text,sans", "#f7921e") -- color: dark yellow
 | u'' u "qwantz.com" = ("DC", "text,sans", "#40d53a") -- color: green (sampled a color from the iconic T-Rex MS Paint art)
 | u'' u "qualiacomputing.com" = ("QC", "text,sans", "")
 | u'' u "www.nngroup.com" = ("NN", "text,sans", "#600c20") -- color: wine-red (the rubricated 'NN' is a bit too bright so went with a wine color). TODO: color SVG icon for black-N+red-N version
 | u'' u "replicationindex.com" = ("RI", "text,sans", "#bf4520") -- color: orange
 | u' u ".yahoo.com" = ("Y!", "text,sans", "#5e21cf")
 | u'' u "quantifiedself.com" || u'' u "forum.quantifiedself.com" || u' u "www.reddit.com/r/QuantifiedSelf/" = ("QS", "text,sans", "#387cc0") -- color: light blue
 | u'' u "www.research.va.gov" = ("VA", "text,sans", "#112e51") -- US Department of Veterans Affair (mostly linked for Million Veteran Project); color: dark blue
 | u'' u "apnews.com" = ("AP", "text,sans", "") -- TODO: SVG icon for underlined AP black+red logo?
 | aU' u ["www.unz.com/gnxp/", "www.razib.com", "www.razibkhan.com", "www.gnxp.com", "x.com/razibkhan"] = ("RK", "text,sans", "") -- Razib Khan
 | u'' u "jaymans.wordpress.com" = ("J👨🏾", "text,sans", "") -- JayMan
 | u'' u "www.rollingstone.com" = ("𝓡 𝐒", "text", "#d71921") -- Rolling Stone <https://www.rollingstone.com/wp-content/uploads/2022/08/cropped-Rolling-Stone-Favicon.png> <https://en.wikipedia.org/wiki/File:Rolling_Stone_2022.svg>
 | u'' u "www.popsci.com" = ("PS", "text,sans", "") -- Popular Science magazine (no usable or recognizable logos); color: none (they are doing orange right now but doesn't seem historical)
 | u'' u "www.crunchbase.com" = ("cb", "text,sans", "#146aff") -- Crunchbase <https://en.wikipedia.org/wiki/Crunchbase> <https://en.wikipedia.org/wiki/File:Crunchbase_wordmark_dark_blue.svg>; TODO: white-on-blue text background
 | u'' u "newcriterion.com" = ("NC", "text", "") -- The New Criterion <https://en.wikipedia.org/wiki/The_New_Criterion>; color: none (like The Paris Review, NC seems to try to change color each issue)
 | u'' u "www.rrauction.com" = ("RR", "text,sans", "#29648a") -- RR Auction; color: dark blue
 | otherwise = ("", "", "")

-- Tri/triple TLAs
linkIconRulesTriple "" = error "Config.LinkIcon.linkIconRulesTriple: passed empty string as the URL; this should never happen!"
linkIconRulesTriple u
 | u'' u "andrewgelman.com" || u'' u "statmodeling.stat.columbia.edu" = ("▅▇▃", "text", "") -- Favicon is a little normal distribution/histogram (▅▇▃) LOWER FIVE EIGHTHS BLOCK, LOWER SEVEN EIGHTHS BLOCK, LOWER THREE EIGHTHS BLOCK
 | u' u "animenewsnetwork.com" = ("ANN", "text,tri", "#006598") -- color: blue; TODO: color SVG icon for the three-circle green-blue logo
 | u'' u "www.catb.org" || u'' u "esr.ibiblio.org" = ("ESR", "text,tri,sans", "#0000ee") -- color: bright blue (used in weird favicon logo)
 | u'' u "arstechnica.com" = ("ars", "text,tri,sans", "#ff4e00") -- Ars is an orange box, not usable; TODO: white-on-orange text background
 | u' u ".bbc.com" || u' u ".bbc.co.uk" = ("BBC", "text,tri,sans", "") -- BBC: no usable logo; TODO: white-on-black text background
 | u' u ".bmj.com" = ("bmj", "text,tri,sans", "#2a6ebb") -- British Medical Journal or just ‘bmj’; TODO: white-on-blue text background
 | u'' u "www.cdc.gov" = ("CDC", "text,tri", "#0057b7") -- TOOD: white-on-blue text background
 | u'' u "boardgamegeek.com" = ("BGG", "text,tri,sans", "#ff5100") -- no logo because puzzle-piece logo would be unrecognizable as link icon <https://cf.geekdo-static.com/images/logos/navbar-logo-bgg-b2.svg>; color: bright orange
 | u'' u "thehub7dnl5nmcz5.onion" = ("Hub", "text,tri,sans", "")
 | u'' u "www.abc.net.au" || u'' u "abcnews.go.com" = ("ABC", "text,tri,sans", "#fdc605") -- <https://en.wikipedia.org/wiki/Australian_Broadcasting_Corporation>; color: bright yellow
 | u'' u "www.odt.co.nz" = ("ODT", "text,tri", "#1a65ad") -- color: blue
 | u'' u "knowyourmeme.com" = ("KYM", "text,tri", "#13133e") -- color: dark purple; TODO: white-on-purple text background
 | u'' u "freakonomics.com" = ("FRK", "text,tri,sans", "#c25700") -- hybrid apple-orange icon (get it, "comparing apples & oranges", "") doesn't work as favicon or link; TODO: color SVG icon
 | u'' u "aiimpacts.org" = ("AII", "text,tri", "#2396ce") -- light blue; TODO: white-on-blue text background
 | u'' u "scp-wiki.wikidot.com" = ("SCP", "text,tri,sans", "#823f3f") -- color: light red-brown
 | aU'' u ["latitude.io", "play.aidungeon.io", "aidungeon.medium.com"] || u' u "www.reddit.com/r/AIDungeon"  || u' u "www.patreon.com/AIDungeon" = ("AID", "text,tri,sans", "")
 | u'' u "nap.nationalacademies.org" = ("NAP", "text,tri", "#1d1646") -- color: dark purple-blue
 | u' u ".cnn.com" = ("CNN", "text,tri,sans", "#cc0000") -- color: bright red; TODO: color SVG 'CNN'-worm icon
 | u'' u "www.npr.org" || u'' u "text.npr.org" = ("npr", "text,tri,sans", "#237bbd") -- NPR styles it in lowercase in their |n|p|r| logo; color: light-blue from 'r' in 'npr' logo; TODO: color SVG icon logo
 | u'' u "www.filfre.net" = ("TDA", "text,tri,sans", "#3a2820") -- Filfre.net/The Digital Antiquarian has no logo or usable substitute… color: dark brown (from background of theme)
 | u'' u "lwn.net" = ("LWN", "text,tri,sans", "#fed050") -- color: yellow (from Tux penguin feet)
 | u' u ".fast.ai" ||  u' u "github.com/fastai/" = ("F.ai", "text,tri", "#3399f3") -- color: light blue
 | u'' u "www.sfgate.com" = ("SFG", "text,tri,sans", "#ff1d46") -- color: bright red
 | u'' u "nypost.com" = ("NYP", "text,tri,sans,italic", "#c60800") -- color: bright dark red; TODO: white-on-red text background
 | u'' u "www.justice.gov" = ("DoJ", "text,tri", "#162e51") -- US federal Department of Justice ; color: dark blue
 | u'' u "hpmor.com" || u' u "www.fanfiction.net/r/5782108/" || u' u "www.reddit.com/r/HPMOR/" = ("MoR", "text,tri,italic", yellowMoR) -- override FanFiction.net ; color: dark gold
 | u'' u "www.fanfiction.net" = ("FFN", "text,tri,sans", "#333399") -- The FF.net logo is pretty crazy (<https://en.wikipedia.org/wiki/File:Fanfictionnetlogo.jpeg> is the *normal* one!), and I don’t think anyone would recognize it in monochrome. 'FF' as an abbreviation is confusing with Firefox, so expand to "FFN". color: dark purple-blue
 | u'' u "myanimelist.net" = ("MAL", "text,tri,sans", "#2b498e") -- MAL: the blue of their logo doesn’t work, so just text. color: royal blue. TODO: white-on-blue text background
 | aU' u ["onegeek.org", "eva-fan.com", "evaotaku.com", "khara.co.jp", "gainax.co.jp", "17th-angel.tumblr.com", "gainax.com", "johakyu.net", "kanzaki.sub.jp", "homepage3.nifty.com", "www.cjas.org", "www.dummy-system.com", "www.evalegend.com", "www.usagi.org", "animekritik.wordpress.com", "fullfrontal.moe", "wavemotioncannon.com", "www.angelfire.com/anime4/", "evacommentary"] = ("NGE", "text,tri", orangeNGE) -- Primary user: forum.evageeks.org wiki.evageeks.org ; color: dark orange (from the original classic splash logo); TODO: color SVG icon (the NERV leaf logo)
 | u'' u "academic.oup.com" || u' u ".nutrition.org" || u' u ".oxfordjournals.org" || u' u "www.robots.ox.ac.uk" = ("OUP", "text,tri", "#011d3f") -- Oxford Academic Journals / OUP; color: very dark blue; TODO: white-on-blue text background
 | u'' u "poniesatdawn.bandcamp.com" = ("P@D", "text,tri", "#27050e") -- color: dark brown
 | u'' u "slatestarscratchpad.tumblr.com" || u'' u "www.astralcodexten.com" || (u'' u "slatestarcodex.com" && (extension u /= ".pdf")) || (isLocal u && (u' u "yvain" ||  u' u "slatestarcodex")) = ("SSC", "text,tri", "#5175c2") -- SSC logo too bad to use; NOTE: we want PDFs merely hosted on SSC to not match, and fall through to get a PDF icon instead; color: light blue; TODO: white-on-blue text background
 | u'' u "plato.stanford.edu" = ("SEP", "text,tri", "#8c1515") -- no icon, we avoid SEP's Rodin's "The Thinker" lgoo, as appropriate as it is, because it won't render at link-icon scale without major revision. color: red;
 | u'' u "www.vox.com" = ("Vox", "text,tri,italic", "#fff200") -- color: bright yellow; TODO: black-on-yellow text background
 | aU'' u ["blogs.wsj.com", "online.wsj.com", "www.wsj.com"] = ("WSJ", "text,tri", "") -- The Wall Street Journal
 | u'' u "gameprogrammingpatterns.com" = ("GPP", "text,tri,sans", "#1487c1") -- color: medium blue
 | u'' u "www.metopera.org" = ("Met", "text,tri", "#9c9899") -- color: dark yellow
 | u'' u "www.schneier.com" = ("SOS", "text,tri,sans", "#6b0000") -- "Bruce Schneier", who writes "Schneier On Security" or "SOS" (Easter egg: the Schneier.com favicon encodes Morse code into its edges, which says… "SOS"); color: dark red; TODO: white-on-red text background
 | u'' u "hbr.org" = ("HBR", "text,tri,sans", "#a51c30") -- Harvard Business Review (official Harvard crimson: <https://seas.harvard.edu/office-communications/brand-style-guide/color-palette>); TODO: white-on-black text background?
 | u' u "harvard.edu" = ("H", "text", "#a51c30") -- Harvard catch-all; <https://en.wikipedia.org/wiki/File:Harvard_University_coat_of_arms.svg> is too complex without a custom shield-H icon?
 | aU'' u ["dl.acm.org", "queue.acm.org", "cacm.acm.org"] = ("acm", "text,tri,sans", "#3795c4") -- <https://en.wikipedia.org/wiki/File:Association_for_Computing_Machinery_(ACM)_logo.svg> 'acm' sans in a circle inside a diamond; can't fake it with Unicode joiners (they'd only put one character into a circle+diamond), and I probably don't want to bother with a SVG.; color: blue (chosen from bottom of SVG logo)
 | u' u "www.cs.utexas.edu/~EWD/" = ("EWD", "text,tri,sans", "") -- Edsger W. Dijkstra, of course, wrote in sans
 | u'' u "iopscience.iop.org" = ("IOP", "text,tri,sans", "#cc0000") -- <https://en.wikipedia.org/wiki/IOP_Publishing> Institute of Physics Publishing; color: red
 | u'' u "80000hours.org" = ("80k", "text,tri,sans", "#2ebdd1") -- 80,000 Hours (Centre for Effective Altruism, FHI, Oxford)
 | u'' u "researchers.wls.wisc.edu" || u' u "www.ssc.wisc.edu/wlsresearch/" = ("WLS", "text,tri,sans", "#c5050c") -- Wisconsin Longitudinal Study; color: red; TODO: white-on-red text background
 | u' u "host.robots.ox.ac.uk/pascal/VOC" = ("VOC", "text,tri,sans", "") -- PASCAL VOC (Visual Object Classes) machine learning image dataset/competition
 | u'' u "www.tandfonline.com" = ("T&F", "text,tri,sans", "#0068b1") -- Taylor & Francis: their icon is a small white oil lamp on a blue background, but it's illegible as a favicon and just looks like a white blob on a blue square; since these need to be monochrome, that makes it useless. Plus I recognize 'Taylor & Francis' (sans serif, as usual for STEM publishers) more anyway, so 'T&F' is the natural tri-text icon. A possible Unicode alternative for the AMPERSAND if it is too big is 'U+FE60 ﹠ SMALL AMPERSAND'. color: blue
 | u' u "omega0.xyz/omega8008/" || aU' u ["/doc/statistics/bayes/1988-jaynes-maximumentropyandbayesianmethods.pdf", "www-biba.inrialpes.fr/Jaynes/cc18i.pdf"] = ("ETJ", "text,tri,sans", "") -- E. T. Jaynes book/paper website
 | u' u "paperswithcode.com" = ("PwC", "text,tri,sans", "#21a7ea") -- 'Papers With Code' does have a weird '[|⏐|⏐|]' icon (supposed to be a bar graph of different performances, I guess) which would work monochrome, but I don't recognize it and I doubt anyone else would either, especially as a link icon, but 'PwC' *might* be recognizable, so we'll go with that for now. color: green.
 | u'' u "www.pewresearch.org" = ("Pew", "text,tri", "") -- Pew Research Center: logo <https://en.wikipedia.org/wiki/File:Pew_Research_Center.svg>. While very cool, and worthy of a Scandinavian black death metal band, it is unrecognizable and would 'shimmer' intensely if scaled down to a link icon & would have to be recreated. So, another text icon it is. Everyone knows what "Pew" means.
 | u'' u "thelastpsychiatrist.com" = ("TLP", "text,tri,sans", "")
 | u'' u "www.lrb.co.uk" = ("LRB", "text,tri", "") -- London Review of Books <https://en.wikipedia.org/wiki/London_Review_of_Books>
 | u'' u "archiveofourown.org" = ("Ao3", "text,tri", "#9c0000") -- Archive of Our Own <https://archiveofourown.org/> <https://en.wikipedia.org/wiki/Archive_of_Our_Own>; color: red
 | u'' u "www.nlsinfo.org" = ("NLS", "text,tri,sans", "#0071bc") -- the National Longitudinal Surveys (BLS), eg. NLSY79 <https://en.wikipedia.org/wiki/National_Longitudinal_Surveys>
 | aU'' u ["www.fda.gov","fis.fda.gov","clinicaltrials.gov", "classic.clinicaltrials.gov"] = ("FDA", "text,tri,sans", "#0078b4") -- U.S. Food & Drug Administration; color: green-blue; TODO: white-on-blue text background
 | u'' u "silkroadvb5piz3r.onion" || u'' u "silkroad5v7dywlc.onion" = ("SR1", "text,tri,sans", "#105a0a") -- color: dark green
 | aU'' u ["bls.gov", "data.bls.gov", "www.bls.gov"] = ("BLS", "text,tri,sans", "#173380") -- no icon because the cute star+time-series logo would be illegible; color: dark blue
 | u'' u "ocw.mit.edu" = ("OCW", "text,tri,sans", "") -- OpenCourseWare: MIT MOOCs <https://en.wikipedia.org/wiki/OpenCourseWare>
 | u'' u "psycnet.apa.org" = ("APA", "text,tri,sans", "#2c72b7") -- APA (American Psychological Association <https://en.wikipedia.org/wiki/American_Psychological_Association>) PsycNET <https://en.wikipedia.org/wiki/PsycINFO>; the APA does have an interesting logo <https://en.wikipedia.org/wiki/File:American_Psychological_Association_logo.svg> which is a capital 'Ψ' on a lined background, but the lines would not work as a link-icon and I am doubtful anyone would recognize 'ψ' on a solid background either, while the 'APA' abbreviation is at least semi-familiar. color: blue; TODO: white-on-blue text background
 | aU'' u ["worksinprogress.co", "www.worksinprogress.news", "books.worksinprogress.co"] = ("WiP", "text,tri,mono", "") -- Works in Progress; color: none (monochrome website with occasional per-issue color theme highlights); icon: the mustached-man is unusable at favicon or link-icon scale and not particularly recognizable either IMO.
 | u'' u "digital.library.unt.edu" = ("UNT", "text,tri", "#00863c") -- University of North Texas library; logo is a <https://en.wikipedia.org/wiki/North_Texas_Mean_Green> green hawk icon, which is cool and would work as a link-icon, but I doubt even sports fans would understand it out of context compared to just the 'UNT' acronym; color: green (black outline)
 | u'' u "infoproc.blogspot.com" || u'' u "www.manifold1.com" = ("Hsu", "text,sans", "") -- Steve Hsu's "Information Processing" blog (defunct) & "Manifold" podcast; no known recognizable icon ('brain network' icon of Manifold is not recognizable, wouldn't work as a link-icon, and overlaps too much with Connected Papers etc); color: none
 | otherwise = ("","", "")

 -- Quad-letter (square) icons.
linkIconRulesQuad "" = error "Config.LinkIcon.linkIconRulesQuad: passed empty string as the URL; this should never happen!"
linkIconRulesQuad u
 | aU'' u ["jamanetwork.com", "archinte.jamanetwork.com"]  = ("JAMA", "text,sans,quad", "#d71635") -- The Journal of the American Medical Association (JAMA); color: bright red; TODO: white-on-red text background
 | u'' u "www.cell.com" = ("CELL", "text,quad,sans", "#007dbc") -- Cell: their logo is unrecognizable (and dumb); color: blue; TODO: white-on-blue text background
 | u'' u "mlp.fandom.com" = ("MLPW", "text,quad,sans", "#abe9e8") -- NOTE: override Fandom catch-all; color: light blue
 | u'' u "www.fimfiction.net" = ("FIMF", "text,quad,mono", "#3b68af") -- color: dark blue
 | u'' u "www.nber.org" && (extension u /= ".pdf") || u'' u "ideas.repec.org" = ("NBER", "text,quad", "#075dba") -- IDEAS/RePEc doesn't seem to actually be run by or affiliated with NBER, but it's so close topically that I think readers can forgive it.; color: dark blue; TODO: white-on-blue text background
 | u'' u "www.pnas.org" = ("PNAS", "text,quad", "#1f75b9") -- PNAS: they don’t have a real logo, but their favicon does a nice little compact square (white text on blue background), and we can replicate that in CSS (but just as black text on white background, per our monochrome theme) [On second thought, all of the icons using background squares, like HN/YC, are very intense and hard to visually balance. It's probably better to leave PNAS as just a quad-letter.] color: blue
 | u'' u "www.rand.org" = ("RAND", "text,quad,sans", "#751ddb") -- color: wine-red; TODO: white-on-red text background
 | u' u ".sagepub.com" = ("SAGE", "text,quad,sans", "#046ff8") -- Sage Journals’s logo is a circled S… but would anyone recognize it? Primary user: journals.sagepub.com; color: dark blue; TODO: white-on-blue text background
 | u'' u "publicdomainreview.org" = ("TPDR", "text,quad", "")
 | u' u "xkcd.com" = ("XKCD", "text,quad,sans", "") -- covers explainxkcd.com, what-if.xkcd.com…
 | u'' u "www.imdb.com" = ("IMDb", "text,sans,quad", "#f5c518") -- color: dark yellow
 | u'' u "www.nejm.org" = ("NEJM", "text,quad", "#ff3300") -- color: red
 | u'' u "spectrum.ieee.org" || u'' u "ieeexplore.ieee.org" = ("IEEE", "text,mono,quad", "#006699") -- color: blue; TODO: white-on-blue text background
 | u'' u "rjlipton.com" = ("P = NP", "text,quad", "") -- NOTE: not 4 letters because we need the spacing for a more reasonable look. 'FULLWIDTH EQUALS SIGN' turns out to be *too* big and stack up three high. using 2 HAIR SPACE will separate the '=' slightly from the 'P' while not causing the 3-layer layout.
 | u' u "mitpress.mit.edu/sites/default/files/sicp/" || u' u "mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/" = ("SI CP", "text,quad,sans", "") -- overrides IA
 | u' u "mitpress.mit.edu/" || u' u "people.csail.mit.edu" = ("MIT", "text,tri,mono", "") -- if it's not _SICP_, fall back.
 | u'' u "jaspervdj.be" = ("JVDJ", "text,quad,mono", "")
 | u'' u "gizmodo.com" = ("GIZM", "text,quad,mono", "")
 | u'' u "www.mdpi.com" = ("MDPI", "text,quad,sans", "") -- <https://en.wikipedia.org/wiki/MDPI> chemical subscript+superscript probably not recognized by anyone & too bulky even as SVG NOTE: doesn't wrap right with serif, so has to be sans
 | u'' u "mattlakeman.org" = ("MATT", "text,quad,sans", "")
 | u'' u "www.poetryfoundation.org" = ("POET", "text,quad,sans", "#ed1c24") -- <https://www.poetryfoundation.org/> <https://en.wikipedia.org/wiki/Poetry_Foundation> logo is a 2×3 grid "POETRY"; fortunately, 'POET' is a real word and works nicely as a quad; color: red
 | u'' u "papers.ssrn.com" = ("SSRN", "text,quad", "#007398") -- color: dark blue; TODO: white-on-blue text background
 | u'' u "www.vice.com" || u'' u "motherboard.vice.com" = ("VICE", "text,quad,italic", "") -- TODO: white-on-black text background
 | aU'' u ["www.courtlistener.com", "archive.recapthelaw.org", "storage.courtlistener.com", "www.courtlistener.com", "www.pacer.uscourts.gov", "www.pacer.gov", "pcl.uscourts.gov"] = ("PACR", "text,quad", "")
 | u'' u "www.nybooks.com" = ("NYRB", "text,quad", "#990910") -- color: red; TODO: white-on-red text background
 | u'' u "www.jstor.org" = ("JTOR", "text,quad", "#900000") -- quad looks better skipping the thin 'S'; color: red; TODO: white-on-red text background
 | u'' u "thisanimedoesnotexist.ai" = ("TADE", "text,quad,sans", "")
 | u'' u "www.thisfursonadoesnotexist.com" = ("TFDE", "text,quad,sans", "")
 | u'' u "www.thiswaifudoesnotexist.net" = ("TWDE", "text,quad,sans", "")
 | u'' u "thisponydoesnotexist.net" = ("TPDE", "text,quad,sans", "")
 | u'' u "pcdb.santafe.edu" = ("PCDB", "text,quad,sans", "#a57030") -- color: reddish brown
 | u'' u "vndb.org" = ("VNDB", "text,quad,sans", "")
 | u'' u "www.huffpost.com" = ("HUFF", "text,quad,sans", "#0dbe98") -- color: blue-green
 | u'' u "longreads.com" = ("Long", "text,quad", "#cc0000") -- logo: 'L' in a red circle; color: red; TODO: white-on-red text background
 | u'' u "warontherocks.com" = ("WOT R", "text,quad,sans", "")
 | u'' u "krebsonsecurity.com" = ("Krbs", "text,quad,sans", "") -- KrebsOnSecurity: 'KOS' unrecognizable, favicon a baffling mystery, Brian Krebs is generally known as 'Krbs', so abbreviate that
 | u'' u "www.nextplatform.com" = ("NEXT", "text,quad,sans", "#ff7200") -- The Next Platform's double-cube logo *could* work as an SVG but not convinced it'd be recognizable; color: orange
 | u'' u "www.vetta.org" = ("Legg", "text,quad,sans", blueDM) -- Shane Legg (DeepMind)
 | u'' u "www.spiegel.de" = ("SPGL", "text,quad", "#e64415") -- Der Spiegel, major German newspaper; the 'S' logo is unrecognizable given the sheer number of 'S' logos out there, so abbreviation instead; color: orange; TODO: orange-on-white text background
 | u'' u "tasvideos.org" = ("TASV", "text,quad", "") -- TASVideos.org: tool-assisted game movies
 | u'' u "habr.com" = ("Habr", "text,quad,sans", "") -- Russian tech collaborative blog <https://en.wikipedia.org/wiki/Habr>
 | u'' u "www.teds.ac.uk" = ("TEDS", "text,quad,sans", "#45c1a7") -- UK twin registry founded by Robert Plomin, heavily used in behavioral genetics & sociology/psychology; it has a clever little logo (<https://www.teds.ac.uk/Content/Images/TEDSlogo.png>) & a monochrome SVG version would work… but unfortunately no one ever uses it & it is always known as "Twins Early Development Study (TEDS)"; color: blue
 | u'' u "stability.ai" || u' u "#stability" || u' u "&org=stability" = ("SD", "text,sans", "")
 | u' u "stripe.com" = ("S", "text,sans", "#635bff") -- use color from Stripe logo: color, brown; TODO: white-on-brown text background
 | u'' u "patrickcollison.com" = ("PC", "text,sans", "#635bff") -- use color from Stripe logo: color, brown; TODO: white-on-brown text background
 | u'' u "oeis.org" = ("OEIS", "text,quad,sans", "") -- On-Line Encyclopedia of Integer Sequences; replicate their existing quad icon.
 | u'' u "bldgblog.com" = ("BLDG", "text,quad,mono", "") -- BLDGBLOG (“building blog”, 2004), by Geoff Manaugh <https://en.wikipedia.org/wiki/BLDGBLOG>
 | u' u "x.com/patio11" || aU'' u ["www.bitsaboutmoney.com", "training.kalzumeus.com", "www.kalzumeus.com"] = ("pt11", "text,quad,mono", bluePatio11) -- patio11 / Patrick McKenzie / Bingo Card Creator / Bits About Money / Stripe. The 'dragon' icon for Kalzumeus.com would be illegible & probably not recognizable at this point even by long-time readers, but a stripped down 'pt11' should look enough like 'patio11'...; color: blue from kalzumeus.com, echoed in Bits About Money. TODO: white-on-blue text background
 | u'' u "mathshistory.st-andrews.ac.uk" = ("M  T", "text,quad,sans", "") -- MacTutor History of Mathematics Archive: a weird one, <https://mathshistory.st-andrews.ac.uk/static/img/logo.png> - crude sans but only 2 letters kinda like a diagonal in a square or a TeX. Experiment with using EN SPACE to force a diagonal quad layout.
 | u'' u "scale.com" = ("SCLE", "text,quad,mono", "") -- Scale, a large data-labeling company heavily used behind-the-scenes by FANG & OpenAI etc for outsourcing evaluating text, labeling images, and so on.
 | u'' u "nunosempere.com" = ("nuno", "text,quad,mono", "") -- Nuño Sempere
 | u'' u "ourworldindata.org" = ("OWID", "text,quad,mono", "") -- Our World In Data (OWID) <https://en.wikipedia.org/wiki/Our_World_in_Data>; NOTE: uses monospace because the 'W' is so wide
 | u'' u "www.cnbc.com" = ("CNBC", "text,quad,sans", "") -- CNBC: peacock logo/favicon <https://en.wikipedia.org/wiki/File:CNBC_2023.svg> doesn't seem viable as a small monochrome link-icon; TODO: color SVG icon
 | u'' u "www.scmp.com" = ("SCM", "text,tri", "#ffca05") -- South China Morning Post (SCMP) <https://en.wikipedia.org/wiki/South_China_Morning_Post>; major HK newspaper, partially CCP-censored post-2016 Alibaba acquisition; logo is a yellow square next to a blue square, so monochrome version would be hard (light gray next to black?); 'SCMP' unfortunately doesn't work as a quad, because the width of 'MP' is far larger than 'SC' and playing around with it, I can't get it to look good, so we settle for just the first three; color: yellow; TODO: color SVG icon yellow-black flag color
 | aU'' u ["magazine.atavist.com", "read.atavist.com"] = ("Atvt", "text,quad", "") -- Atavist Magazine <https://en.wikipedia.org/wiki/Atavist>; can't use the italic-capital serif A logo because it looks identical to _The Atlantic_, so disemvowel the name to a 4-letter abbreviation. Annoyingly, they move around and use multiple sub-domains. TODO: white-on-black text background
 | u'' u "qntm.org" || u == "https://scp-wiki.wikidot.com/antimemetics-division-hub" || u == "https://scp-wiki.wikidot.com/qntm-s-author-page#toc2" = ("qntm", "text,quad,mono", "") -- qntm/Sam Hughes: programming & SF
 | aU'' u ["blog.samaltman.com", "samaltman.com"] = ("sama", "text,quad,mono", "") -- Sam Altman, username 'sama'
 | u' u "a16z" = ("az16", "text,quad,sans", "#ed8c00") -- Andreessen Horowitz/a16z (reworded to 'az16' because quad splits it badly: 'a1/6z' doesn't read easily); color: orange; TODO: white-on-orange text background
 | u'' u "www.dwarkeshpatel.com" = ("Dwkh", "text,sans,quad", "#f3c016") -- Dwarkesh Patel podcast (formerly, "Lunar Society"); icon: big portrait is unusable, and it is known primarily by 'Dwarkesh', so we just quad it; color: yellow (Substack theme?)
 | u'' u "ascii.textfiles.com" = ("ASCI", "text,quad,mono", "#006309") -- Jason Scott, ASCII web log; surprisingly, I do not seem to have linked any other textfiles.com subdomain? color: green (from background, because the dark-green would make a terrible link color)
 | otherwise = ("", "", "")

-- SVG icons (remember the link-icon name is substituted in as part of the URL to the SVG icon)
linkIconRulesSVG "" = error "Config.LinkIcon.linkIconRulesSVG: passed empty string as the URL; this should never happen!"
linkIconRulesSVG u
 | aU'' u ["texample.net", "ctan.org", "www.tug.org", "tug.org"] = ("tex", "svg", "") -- Properly turning the 'TeX' logotype in a link icon is hard. You can't use the official logo: <https://commons.wikimedia.org/wiki/File:TeX_logo.svg> is unworkable as a tiny icon, Computer Modern's thinness issues are massively exacerbated & it's unreadable (it's not great on computer screens to begin with, and shrunk down to a link-icon, even worse); you can cheat in pure Unicode with 'TₑX' (LATIN SUBSCRIPT SMALL LETTER E U+2091;, there is no 'LARGE LETTER E' unfortunately) but this took winds up looking pretty bad in practice. So what I did was create my own SVG TeX link-icon in Inkscape, using Source Serif Pro bold letters, arranged by hand like the logotype, and then rescaled horizontally ~120% to make the strokes thick enough that they'd survive downscaling. *That* works.
 | aU'' u ["www.amazon.com", "aws.amazon.com", "amazon.com", "smile.amazon.com", "aboutamazon.com"] || u' u "amazon.co." = ("amazon", "svg", "#ffce53") -- icon: 'a'+swoosh-underline; color: yellow; TODO: color SVG icon with just the underline swoosh yellow
 | u'' u "en.bitcoin.it" || u'' u "bitcointalk.org" || u'' u "www.blockchain.com" = ("bitcoin", "svg", "#ef8e19") -- <https://en.wikipedia.org/wiki/File:Bitcoin.svg>
 | u'' u "www.biorxiv.org" || u'' u "www.medrxiv.org" = ("chi-dna", "svg", "#bd2736") -- BioRxiv (custom icon: italic Chi with DNA cross-strands).; color: red
 | u'' u "distill.pub" = ("distillpub", "svg", "") -- Distill ML journal.
 | u'' u "www.dropbox.com" || u'' u "dl.dropboxusercontent.com" = ("dropbox", "svg", "#0061fe") -- Dropbox: old file-host, deprecated since they’ve started killing inactive accounts & their UI become awful. primary user: dl.dropboxusercontent.com; color: blue
 | u'' u "www.erowid.org" || u'' u "www.drugsdata.org" = ("erowid", "svg", "#a06929") -- color: copper-orange (<https://www.erowid.org/general/about/about_article10.shtml>)
 | aU' u [".tensorflow.org", "github.com/tensorflow/", "medium.com/tensorflow/"] = ("tensorflow", "svg", "#ff6f00") -- <https://simpleicons.org/?q=tensorflow>; NOTE: hosted on Github, so override Github
 | aU'' u ["github.com", "copilot.github.com", "archiveprogram.github.com", "gist.github.com", "github.blog", "compvis.github.io"] = ("github", "svg", "") -- Github; I exclude *.github.io & raw.githubusercontent.com because that’s blogs/papers.
 | u'' u "paulgraham.com" = ("pg", "text,mono", "#666699") -- Paul Graham, known by username 'pg' on HN; color: purple; TODO: white-on-purple text background
 | u' u "ycombinator.com" || u' u "hn.algolia.com" = ("hacker-news", "svg", "#f26522") -- HN/YC (shared logo). primary user: news.ycombinator.com; color: orange
 | aU' u ["webcitation.org", "mementoweb.org", "archive.org", "archive-it.org", "wiki.archiveteam.org", "waybackmachine.org", "archive.is", "archive.md", "archive.ph", "archive.today", "babel.hathitrust.org"] = ("internet-archive", "svg", "") -- HathiTrust <https://en.wikipedia.org/wiki/HathiTrust> is confusingly nebulous but its cute elephant logo is unrecognizable and I regard it as basically a wrapper around Google Books+Internet Archive, so I think it's less confusing to put it under the IA logo. Note: overridden by SICP
 | u'' u "mega.nz" = ("mega", "svg", "#dd1405") -- MegaUpload/Mega: filesharing (used for big files).; color: orange
 | u'' u "intelligence.org" = ("miri", "svg", "#234e80") -- MIRI/intelligence.org. color: blue
 | u' u ".nytimes.com" = ("new-york-times", "svg", "") -- The New York Times: manual edit, reducing full 'NEW YORK TIMES' SVG logo to just the ‘T’ they use as an icon.
 | aU'' u ["www.ncbi.nlm.nih.gov", "pubmed.ncbi.nlm.nih.gov", "pmc.ncbi.nlm.nih.gov"] = ("nlm-ncbi", "svg", "#20558a") -- NCBI/Pubmed: simplification of their logo (<https://upload.wikimedia.org/wikipedia/commons/0/07/US-NLM-NCBI-Logo.svg>). primary user: ncbi.nlm.nih.gov; color: blue-green
 | u'' u "www.patreon.com" = ("patreon", "svg", "#f76159") -- Patreon. (Used the old one (<https://upload.wikimedia.org/wikipedia/commons/9/94/Patreon_logo.svg>) because I don’t like the new one.); color: red-orange
 | aU' u ["plos.org", "plosone.org"] = ("plos", "svg", "") -- PLOS ONE in all their domain permutations… primary user: journals.plos.org; no consistent overall color
 | aU' u ["overflow.net", "overflow.com", "stackexchange.com"] = ("stack-exchange", "svg", "#f48024") -- The *Exchange/*Overflow family of websites. Color: orange. (The individual sites have different colors, but orange is the one I always think of.)
 | u' u "substack.com" = ("substack", "svg", "#ff6719") -- gwern.substack.com; color: orange
 | u'' u "www.theguardian.com" || u'' u "www.guardian.co.uk" = ("the-guardian", "svg", "#052962") -- El Grauniad.; color: dark blue
 | u'' u "www.newyorker.com" = ("the-new-yorker", "svg", "") -- The New Yorker: the Dandy SVG, simplified & rotated more vertically.
 | u' u "tumblr.com" = ("tumblr", "svg", "#001935") -- color: dark blue
 | aU'' u ["x.com", "blog.x.com", "developer.x.com" ] ||
   -- we host local HTML mirrors of Twitter/Nitter for archiving & annotation-override reasons; rather than give them an uninformative HTML icon, we detect & override here to assign them the bird icon. They follow the schema `/doc/foo/$DATE-$AUTHOR-twitter-$TITLE.html`:
   (isLocal u && hasExtension ".html" u && "-twitter-" `T.isInfixOf` u) = ("twitter", "svg", "#1da1f2")
 | u'' u "www.uptontea.com" = ("upton-tea", "svg", "#365888") -- color: blue
 | u'' u "soundcloud.com" = ("audio", "svg", "#ff5500") -- color: orange
 | u' u ".bandcamp.com" = ("audio", "svg", "#1da0c3") -- color: blue
 | u'' u "www.washingtonpost.com" = ("washington-post", "svg", "") -- The Washington Post: truncated their blackletter to ‘WP’.
 | aU' u ["wikipedia.org", "wikimedia.org", "wiktionary.org", "wikisource.org", "wikimediafoundation.org", "stats.grok.se", "wikibooks.org", "wikiquote.org", "xtools.wmflabs.org", "www.mediawiki.org"] = ("wikipedia", "svg", "") -- primary user: en.wikipedia.org, meta.wikimedia.org, en.wiktionary.org, en.wikisource.org
 | u' u ".fandom.com" = ("♡", "text", "#fa005a") -- formerly known as Wikia, renamed to 'Fandom' and adopted a heart-based logo: <https://en.wikipedia.org/wiki/Fandom_(website)#2016%E2%80%932018:_Fandom_brand>; updated to a pixel-art flame logo at some point...? this is an umbrella covering all the subdomains; more specific Fandom wikis go before in the list (like MLP); color: orange
 | u' u "www.wired.com" || u' u "www.wired.co.uk" = ("wired", "svg", "") -- an inverse "W" on a black background (Wiley is just a "W", "")
 | u'' u "www.youtube.com" = ("youtube", "svg", "#ff0033")
 | aU'' u ["vimeo.com", "player.vimeo.com"] = ("file-video", "svg", "#17d5ff") -- color: green
 | u'' u "www.telegraph.co.uk" = ("the-telegraph", "svg", "") -- edited from <https://en.wikipedia.org/wiki/File:The_Telegraph.svg>
 | u'' u "www.openphilanthropy.org" = ("open-philanthropy", "svg", "")
 | u'' u "www.atlasobscura.com" = ("atlas-obscura", "svg", "#ad8f68") -- Atlas Obscura; color: orange-bronze
 | aU'' u ["blog.eleuther.ai", "www.eleuther.ai", "pile.eleuther.ai", "6b.eleuther.ai"] || u' u "arankomatsuzaki.wordpress.com/2021/06/04/gpt-j/" = ("eleutherai", "svg", "")
 | u'' u "arankomatsuzaki.wordpress.com" = ("ak", "text,sans", "") -- known with the other ak on Twitter; put after EAI in the SVG section because the GPT-J announcement is an EAI project
 | u' u ".apple.com" = ("apple", "svg", "")
 | u'' u "www.quantamagazine.org" = ("quanta", "svg", "#ff8600") -- color: orange/gold
 | u'' u "creativecommons.org" = ("creative-commons", "svg", "") -- <https://creativecommons.org/about/downloads>
 | u'' u "www.alcor.org" = ("alcor", "svg", "")
 | aU'' u ["www.econlib.org", "www.econtalk.org", "betonit.blog"] = ("econlib", "svg", "") -- EconLib/EconLog/EconTalk torch icon <https://3ijp5i2qkzo4hq4yrxfteqh-wpengine.netdna-ssl.com/wp-content/themes/econlib/assets/icons/torch-icon.svg>
 | u' u ".cochrane.org" || aU'' u ["www.cochrane.org.uk", "www.cochranelibrary.com"] = ("cochrane-collaboration", "svg", "#b226b1") -- <https://upload.wikimedia.org/wikipedia/commons/archive/a/a1/20200122144949%21Cclogo.svg> (Newer version is not actually an SVG; reported on Talk page); color: red
 | u'' u "www.connectedpapers.com" = ("connected-papers", "svg", "#74b7b8") -- color: green-blue
 | u' u "nasa.gov" = ("nasa", "svg", "#dc3329") -- NASA has way too many subdomains to try to whitelist them individually. SVG is a quad version of <https://commons.wikimedia.org/wiki/File:NASA_Worm_logo_(black).svg>; color: red (from <https://commons.wikimedia.org/wiki/Category:NASA_%22worm%22_logotype#/media/File:NASA_Worm_logo.svg>)
 | aU'' u ["link.springer.com", "rd.springer.com"] || u' u ".biomedcentral.com" = ("springerlink", "svg", "")  -- (♘) WHITE CHESS KNIGHT as SVG
 | u'' u "www.metaculus.com" = ("metaculus", "svg", "#283441") -- color: dark blue-black
 | u'' u "wandb.ai" = ("wandb", "svg", "#ffcc33") -- Weights & Biases/WandB: blog/podcasts, writeups etc; complicated 4-dot grid logo intended to evoke NN layers with large/small weights, <view-source:https://assets.website-files.com/5ac6b7f2924c656f2b13a88c/6066c22135b8983b61ad7939_weights-and-biases-logo.svg>; edited into BW, enlarged the large dots to make viewable as a link icon; color: yellow
 | aU'' u ["libgen.li", "libgen.org", "library.bz"] = ("raven", "svg", "") -- Libgen/Sci-Hub raven+key icon <https://en.wikipedia.org/wiki/File:Scihub_raven.png>, while pretty, is too detailed for a link-icon so fall back to just the raven. There are many LG+SH domains, but these are the only ones we link.
 | u'' u "www.hoover.org" = ("hoover-institution", "svg", "") -- <https://en.wikipedia.org/wiki/Hoover_Institution_Library_and_Archives> <https://en.wikipedia.org/wiki/Hoover_Tower> <https://en.wikipedia.org/wiki/New_Cathedral_of_Salamanca>
 | u'' u "www.statnews.com" = ("stat-news", "svg", "#008299") -- STAT News <https://en.wikipedia.org/wiki/Stat_(website)> based on <https://www.statnews.com/wp-content/themes/stat/images/stat-logo.svg>; using Unicode '𝐴' to replicate the 'A' in 'STAT' is probably unreliable cross-platform so we transform the 'STAT' logotype into a quad SVG icon instead.
 | aU'' u ["thepiratebay.org", "rss.thepiratebay.se", "thepiratebay.se",  "thepiratebay.sx"] = ("the-pirate-bay", "svg", "") -- in theory, you can get a skull & crossbones by Unicode Emoji: BLACK FLAG + SKULL AND CROSSBONES + ZWJ = PIRATE FLAG <https://emojipedia.org/pirate-flag/> (and if that doesn't work try adding U+FE0F to the end). This turns out to be too unreliable across systems (fonts? OSes? browser versions?) that we replaced it with a proper SVG version of The Pirate Bay's cassette-tape (yes, really) + cross-bones.
 | u'' u "retractionwatch.com" = ("magnifying-glass", "svg", "") -- Retraction Watch <https://en.wikipedia.org/wiki/Retraction_Watch> LEFT-POINTING HOUR GLASS
 | u'' u "quoteinvestigator.com" = ("magnifying-glass", "svg", "") -- basically the same Sherlock Holmes magnifying-glass idea
 | u'' u "www.yudkowsky.net" = ("yud", "svg", "") -- but of course: י HEBREW LETTER YUD 0x05D9; we use an SVG icon here for the same reason we use a math alef elsewhere instead of the Hebrew one (the RTL of Hebrew script will screw up some browsers, like Mac Firefox)
 | u'' u "nautil.us" = ("nautilus", "svg", "#ffce00") -- modeled after 🐚 SPIRAL SHELL (U+1F41A), but turned into monochrome SVG (this icon is usually rendered in color & differently across platforms, so we ship another SVG); color: orange-bronze
 | u'' u "www.scholarpedia.org" = ("scholarpedia", "svg", "") -- Scholarpedia <https://en.wikipedia.org/wiki/Scholarpedia>; Adobe trace of their PNG favicon
 | u'' u "radiolab.org" = ("audio-waveform-lines", "svg", "#8c2ec5") -- Radiolab WNYC/NPR <https://en.wikipedia.org/wiki/Radiolab>; <https://fontawesome.com/icons/waveform-lines>/<https://www.svgrepo.com/svg/342965/audio-wave>; color: red
 | u'' u "maggieappleton.com" = ("maggie-appleton", "svg", "#04a4ba")  -- <https://x.com/Mappletons> Maggie Appleton, designer (Elicit/Ought), blogger about hypermedia/personal wikis/PKM; color: blue
 | u'' u "www.emacswiki.org" || aU' u ["www.reddit.com/r/emacs/", "www.gnu.org/software/emacs"] = ("emacs", "svg", "#c04c39") -- color: red (from <https://en.wikipedia.org/wiki/File:Emacs-logo.svg>)
 | u'' u "www.chicagotribune.com" = ("chicago-tribune", "svg", "#024c83") -- fraktur capital 'C', letter-mark extracted & made black from <https://en.wikipedia.org/wiki/File:Chicago_Tribune_Logo.svg>; color: dark blue
 | u'' u "www.tiktok.com" = ("tiktok", "svg", "#65c3c9") -- color: green
 | u' u ".cbslocal.com" || u'' u "www.cbsnews.com" = ("cbs", "svg", "") -- <https://commons.wikimedia.org/wiki/File:CBS_News_logo_(2020).svg>; color: none (individual CBS affiliates/stations seem to pick their own colors but the overall brand is just black)
 | u' u "midjourney.com" = ("midjourney", "svg", "") -- <https://en.wikipedia.org/wiki/File:Midjourney_Emblem.svg>; color: none
 | u' u "darcs.net" = ("darcs", "svg", "#6ef701") -- Darcs DVCS: green squid/squirt/virus/blob/tentacle SVG from <https://www.svgrepo.com/svg/373545/darcs>
 | u' u "virginia.edu" = ("internet-archive", "svg", "#e57200") -- Department of Computer Science, University of Virginia: library-like logo, so just reuse the IA icon; color: orange-gold

 -- FINAL MATCHES:
 -- many orgs will use a 'medium.com' subdomain, so we fall back here for Medium as the lowest-priority, and override case by case above:
 | u'' u "medium.com" || u'' u "towardsdatascience.com" = ("𝐌", "text", "") -- Medium: cheaper to abuse Unicode (𝐌) MATHEMATICAL BOLD CAPITAL M; color: none (Medium once tried a green logo c. 2015 but seems to have quickly abandoned it); TODO: white-on-black text background
 | u' u "reddit.com" = ("reddit", "svg", "#ff4500") -- www.reddit.com; color: orange
 | otherwise = ("", "", "")

-- Filetypes: (we need to parse & extract the extension because many would be too short and match too many URLs if mere infix matching was used)
linkIconRulesFiletypes "" = error "Config.LinkIcon.linkIconRulesFiletypes: passed empty string as the URL; this should never happen!"
linkIconRulesFiletypes u
 | iE u ["tar", "zip", "xz", "img", "bin", "pkl", "onnx", "pt"] = ("archive", "svg", "")
 | iE u ["maff"] = ("archive", "svg", "#e66000") -- Mozilla Archive File Format; color: Firefox orange
 | iE u ["opml", "txt", "xml", "json", "jsonl", "md"] || u'' u "pastebin.com" = ("txt", "svg", "")
 | iE u ["conf", "sh", "patch", "diff"] = ("code", "svg", "")
 | iE u ["r", "R"] = ("code", "svg", redR)
 | iE u ["js"] = ("code", "svg", "#f6da19") -- color: yellow; the unofficial 2011 logo canonized by Ecmascript
 | iE u ["css"] = ("code", "svg", "#2465f1") -- color: blue <https://en.wikipedia.org/wiki/File:CSS3_logo_and_wordmark.svg>
 | iE u ["hs"] = ("code", "svg", purpleHaskell)
 | iE u ["doc", "docx"] = ("word-doc", "svg", "#277dd4") -- Microsoft Office Word doc; color: green (<https://en.m.wikipedia.org/wiki/File:Microsoft_Office_Word_(2019%E2%80%93present).svg>)
 | iE u ["xls", "xlsx"] = ("spreadsheet", "svg", "#1ba566") -- Microsoft Excel spreadsheet file format; color: green (picked middle green from <https://commons.wikimedia.org/wiki/File:Microsoft_Office_Excel_(2019%E2%80%93present).svg>)
 | iE u ["ods"] = ("spreadsheet", "svg", "#0586ce") -- Apache OpenOffice ODS spreadsheet file format; color: green-blue (<https://en.m.wikipedia.org/wiki/File:Apache_OpenOffice_logo_and_wordmark_(2014).svg>)
 | iE u ["csv"] = ("csv", "svg", "")
 | u'' u "imgur.com" || u'' u "i.imgur.com"                       = ("image", "svg", "#1bb76e") -- color: green
 | iE u ["gif", "bmp", "ico", "jpg", "jpeg", "png", "xcf"] = ("image", "svg", "")
 | iE u ["svg"] = ("image", "svg", "#ffb338") -- color: yellow (<https://en.wikipedia.org/wiki/File:SVG_Logo.svg>)
 | iE u ["psd"] = ("image", "svg", "#01c3f6") -- Adobe Photoshop logo; color: green
 | iE u ["mp3", "flac", "ogg"] = ("audio", "svg", "") -- MP3 logo has no color, surprisingly
 | iE u ["mp4"] = ("file-video", "svg", "")
 | iE u ["webm"] = ("file-video", "svg", "#acd147") -- WebM; color: green (picked most average-looking green from <https://en.wikipedia.org/wiki/File:WebM_logo.svg>)
 | iE u ["mkv"] = ("file-video", "svg", "#0067ad") -- Matroska video format; color: green-blue (<https://en.m.wikipedia.org/wiki/File:Matroska_Logo.svg>)
 | iE u ["swf"] = ("file-video", "svg", "#490202") -- color: red-purple (<https://en.wikipedia.org/wiki/File:Flash_Player_34_SWF_icon.png>)
 | iE u ["ebt", "mdb", "mht", "ttf"] = ("misc", "svg", "")
 | iE u ["mht"] = ("misc", "svg", "") -- TODO: Microsoft
 | iE u ["epub"] = ("EPUB", "text,quad,sans", "#87ba11") -- color: neon green <https://commons.wikimedia.org/wiki/File:Epub_logo.svg>
 | "/static/" `T.isPrefixOf` u && hasExtension ".html" u  = ("code", "svg", "")
 | isLocal u && hasExtension ".php" u                     = ("code", "svg", "#787cb4") -- color: light purple <https://commons.wikimedia.org/wiki/File:PHP-logo.svg>
 | aU' u [".pdf", ".PDF", "/pdf", "type=pdf", "pdfs.semanticscholar.org", "citeseerx.ist.psu.edu", "pdfs.semanticscholar.org", "www.semanticscholar.org"] = ("pdf", "svg", redAdobe) -- color: red (Adobe); NOTE: we do not attempt to check for PDFs very thoroughly because we assume that there are no treacherous URLs or that they are covered by LinkArchive mirroring PDFs locally by default to a '/doc/www/.../$HASH.pdf' URL which will match this reliably.
 | otherwise = ("", "", "")

-- lowest priority: color-only hover links
linkIconRulesColors "" = error "Config.LinkIcon.linkIconRulesColors: passed empty string as the URL; this should never happen!"
linkIconRulesColors u
 | u'' u "danluu.com" = ("", "", "#0000ee") -- Dan Luu: no icon, but color: default link blue is iconic at this point!
 | u'' u "www.thedailybeast.com" = ("","", "#ea0606") -- red
 | u'' u "www.straighttalkonevidence.org" = ("", "", "#102659") -- dark blue
 | u'' u "blog.codinghorror.com" || u'' u "www.codinghorror.com" = ("","", "#57a3e8") -- light blue
 | u'' u "fonts.ilovetypography.com" || u'' u "ilovetypography.com" = ("", "", "#e05f4c") -- orange-red
 | u'' u "dataprivacylab.org" = ("", "", "#3366cc") --  blue
 | u'' u "www.thefreelibrary.com" = ("", "", "#064c82") -- blue
 | u'' u "www.unitedpharmacies-uk.md" = ("", "", "#27c1da") -- light-blue
 | u'' u "www.petforums.co.uk" = ("", "", "#295b52") -- dark green
 | u'' u "wellcomecollection.org" = ("", "", "#ffce3c") -- yellow
 | u'' u "www.acpjournals.org" = ("", "", "#007377") -- teal
 | otherwise = ("", "", "")

------------------------------------------------------------------------------------------

-- linkIconTestUnitsLink :: [(Inline,T.Text,T.Text)]
-- linkIconTestUnitsLink = [(Link ("", ["directory-indexes-upwards"],      []) [Str "Test"] ("/doc/index", "Link to parent directory (ascending)", ""),
--                            "arrow-up-left", "svg", "")
--                         , (Link ("", ["directory-indexes-downwards"],   []) [Str "Test"] ("/doc/zeo/index", "Link to child directory zeo (descending)", ""),
--                            "arrow-down-right", "svg", "")
--                           , (Link ("", ["directory-indexes-sideways"],  []) [Str "Test"] ("/doc/ai/nn/transformer/alphafold/index", "Link to other directory ai/nn/transformer/alphafold (descending)", ""),
--                            "arrow-right", "svg", "")
--                           , (Link ("", ["directory-indexes-upwards"],   []) [Str "Test"] ("/doc/index", "", ""), "arrow-up-left", "svg", "")
--                           , (Link ("", ["directory-indexes-downwards"], []) [Str "Test"] ("/doc/index", "", ""), "arrow-down-right", "svg", "")
--                           , (Link ("", ["directory-indexes-sideways"],  []) [Str "Test"] ("/doc/index", "", ""), "arrow-right", "svg", "")
--                           ]

-- in /lorem order:
-- testing: unique-keys, first is URI
linkIconTestUnitsText :: [(T.Text,T.Text,T.Text,T.Text)]
linkIconTestUnitsText =
        [
         ("http://www.jstor.org/stable/10.1086/468061", "JTOR", "text,quad", "#900000")
         , ("https://meltingasphalt.com/interactive/going-critical/",  "\9650","text", "#aa0000")
         , ("https://www.schneier.com/blog/archives/2011/08/terrorism_in_th.html", "SOS", "text,tri,sans", "#6b0000")
         , ("https://archiveofourown.org/works/17356235", "Ao3", "text,tri", "#9c0000")
         , ("https://longreads.com/2015/01/28/friendship-is-complicated/", "Long", "text,quad", "#cc0000")
         , ("http://archives.cnn.com/2000/HEALTH/aging/04/19/hearing.loss.wmd/index.html", "CNN", "text,tri,sans", "#cc0000")
         , ("https://iopscience.iop.org/article/10.1088/1748-9326/aabf9b", "IOP", "text,tri,sans", "#cc0000")
         , ("https://fortune.com/2023/01/10/microsoft-investment-10-billion-openai-chatgpt/", "F", "text,sans", "#dc0000")
         , ("https://www.forbes.com/sites/andygreenberg/2013/09/05/follow-the-bitcoins-how-we-got-busted-buying-drugs-on-silk-roads-black-market/", "F", "text", "#dc0000")
         , ("http://www.sequentialtart.com/archive/july00/grant.shtml", "ST", "text,sans", "#ff0000")
         , ("https://hacks.mozilla.org/2021/05/improving-firefox-stability-on-linux/", "FF", "text,sans", "#e66000")
         , ("https://boardgamegeek.com/boardgame/148931/coup-reformation", "BGG", "text,tri,sans", "#ff5100")
         , ("https://www.nextplatform.com/2019/08/20/big-blue-open-sources-power-chip-instruction-set/", "NEXT", "text,quad,sans", "#ff7200")
         , ("https://www.vox.com/2015/5/27/8660249/bill-gates-spanish-flu-pandemic",  "Vox","text,tri,italic", "#fff200")
         , ("https://www.nejm.org/doi/full/10.1056/NEJM199604043341416", "NEJM", "text,quad", "#ff3300")
         , ("https://www.reddit.com/r/Supplements/comments/mr0h1/taking_melatonin_forever/",  "reddit","svg", "#ff4500")
         , ("https://soundcloud.com/leggysalad/girls-afternoon-appointments",  "audio","svg", "#ff5500")
         , ("https://www.sciencedirect.com/science/article/pii/S0002929717301076",  "E","text", "#eb6500")
         , ("https://www.rifters.com/real/2009/01/iterating-towards-bethlehem.html", "P.W.", "text,sans", "#737500")
         , ("https://www.quantamagazine.org/how-the-slowest-computer-programs-illuminate-maths-fundamental-limits-20201210/", "quanta", "svg", "#ff8600")
         , ("https://freakonomics.com/2007/05/what-do-you-have-to-say-about-ron-paul/", "FRK", "text,tri,sans", "#c25700")
         , ("https://nypost.com/2019/06/27/north-carolina-couple-paid-25k-to-clone-their-beloved-pet-cat/", "NYP", "text,tri,sans,italic", "#c60800")
         , ("https://thebrowser.com/", "TB", "text", "#ff9900")
         , ("https://www.dummy-system.com/2013/04/01/intervista-megumi-hayashibara-evangelion-3-0/", "NGE", "text,tri", orangeNGE)
         , ("https://wavemotioncannon.com/2016/11/08/interview-hideaki-anno-vs-yoshiyuki-tomino-animage-071994/", "NGE", "text,tri", orangeNGE)
         , ("https://fullfrontal.moe/interview-mahiro-maeda/", "NGE", "text,tri", orangeNGE)
         , ("https://17th-angel.tumblr.com/post/11409371268/anno-a-transfer-student-opens-the-door-with-a",  "NGE", "text,tri", orangeNGE)
         , ("https://eva.onegeek.org/",  "NGE", "text,tri", orangeNGE)
         , ("https://animekritik.wordpress.com/2011/05/12/evangelion-2-0-surnames-statements-and-makinami/", "NGE", "text,tri", orangeNGE)
         , ("https://kanzaki.sub.jp/archives/000272.html", "NGE", "text,tri", orangeNGE)
         , ("https://eva-fan.com/blog-entry-1198.html",  "NGE", "text,tri", orangeNGE)
         , ("https://www.angelfire.com/anime4/mdwigs/Asuka.html", "NGE", "text,tri", orangeNGE)
         , ("http://www.usagi.org/doi/seiyuu/tv/1997eva.html", "NGE", "text,tri", orangeNGE)
         , ("https://evaotaku.com/html/programbooks.html",  "NGE", "text,tri", orangeNGE)
         , ("https://web.archive.org/web/20090713005058/http://homepage3.nifty.com/mana/ecom4.htm", "NGE", "text,tri", orangeNGE)
         , ("https://www.cjas.org/~leng/daihist.htm", "NGE", "text,tri", orangeNGE)
         , ("https://web.archive.org/web/20080127001226/http://johakyu.net/lib/2007/07/2007-07-27-000535.php", "NGE", "text,tri", orangeNGE)
         , ("http://www.evalegend.com/interview_anno96.php", "NGE", "text,tri", orangeNGE)
         , ("http://www.gainax.co.jp/wp/",  "NGE", "text,tri", orangeNGE)
         , ("https://www.khara.co.jp/hideakianno/personal-biography/",  "NGE", "text,tri", orangeNGE)
         , ("https://www.evamonkey.com/ask-john/has-evangelion-influenced-contemporary-gundam-anime.php",  "EG","text", orangeNGE)
         , ("https://web.archive.org/web/20151106005148/http://www.evacommentary.org/appendix/character-names.html","NGE", "text,tri", orangeNGE)
         , ("https://forum.evageeks.org/index.php",  "EG","text", orangeNGE)
         , ("https://x.com/EvaMonkey/", "EG", "text", orangeNGE)
         , ("https://blogs.nvidia.com/blog/gaugan-photorealistic-landscapes-nvidia-research/",  "n","text,sans,italic", greenNV)
         , ("https://nvlabs.github.io/stylegan2/versions.html",  "n","text,sans,italic", greenNV)
         , ("https://nv-adlr.github.io/MegatronLM",  "n","text,sans,italic", greenNV)
         , ("https://nv-tlabs.github.io/big-datasetgan/",  "n","text,sans,italic", greenNV)
         , ("https://kk.org/books/out-of-control",  "KK","text,sans", "#f4eb00")
         , ("https://arstechnica.com/gadgets/2011/09/the-post-jobs-era-tim-cook-brings-philanthropy-back-to-apple/",  "ars","text,tri,sans", "#ff4e00")
         , ("https://nautil.us/mapping-the-human-exposome-236726/", "nautilus", "svg", "#ffce00")
         , ("https://medium.com/tensorflow/fitting-larger-networks-into-memory-583e3c758ff9", "tensorflow", "svg", "#ff6f00")
         , ("https://www.tensorflow.org/tensorboard/get_started", "tensorflow", "svg", "#ff6f00")
         , ("https://playground.tensorflow.org/", "tensorflow", "svg", "#ff6f00")
         , ("https://www.newsweek.com/gene-editing-chinese-scientist-he-jiankui-missing-house-arrest-1240749", "NW", "text", "#f72210")
         , ("https://hpmor.com/notes/progress-report-2013-11-01/", "MoR", "text,tri,italic", yellowMoR)
         , ("https://www.fanfiction.net/r/5782108/5/1/", "MoR", "text,tri,italic", yellowMoR)
         , ("https://hpmor.com/chapter/2", "MoR", "text,tri,italic", yellowMoR)
         , ("https://www.reddit.com/r/HPMOR/", "MoR", "text,tri,italic", yellowMoR)
         , ("https://unsongbook.com/",  "\8501","text", "#b47810")
         , ("https://www.nybooks.com/articles/2020/01/16/alma-mahler-it-had-to-be-her/", "NYRB", "text,quad", "#990910")
         , ("https://replicationindex.com/2016/04/18/is-replicability-report-ego-depletionreplicability-report-of-165-ego-depletion-articles/", "RI", "text,sans", "#bf4520")
         , ("https://www.filfre.net/2016/08/ibms-new-flavor/",  "TDA","text,tri,sans", "#3a2820")
         , ("https://www.nngroup.com/articles/aesthetic-usability-effect/", "NN", "text,sans", "#600c20")
         , ("https://pcdb.santafe.edu/", "PCDB", "text,quad,sans", "#a57030")
         , ("https://www.esquire.com/entertainment/a36439327/planet-hollywood-origin-story-history-interview/", "ℰ", "text", "#ff3a30")
         , ("https://www.nobelprize.org/?p=2688", "🏅", "text", "#cc9b40")
         , ("https://lwn.net/Articles/286233/", "LWN", "text,tri,sans", "#fed050")
         , ("https://safebooru.org/index.php?page=post&s=list&tags=heterochromia", "❐", "text", "#ba9570")
         , ("https://danbooru.donmai.us/posts?tags=death_flag", "❐", "text", "#ba9570")
         , ("https://derpibooru.org/tags/generator-colon-thisponydoesnotexist", "❐", "text", "#ba9570")
         , ("https://www.brookings.edu/articles/expectations-of-sustained-effects-from-scaled-up-pre-k-challenges-from-the-tennessee-study/", "B", "text", "#003a70")
         , ("https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=1&year1=199201&year2=201101", "BLS", "text,tri,sans", "#173380")
         , ("https://bls.gov/news.release/archives/ecec_031986.pdf", "BLS", "text,tri,sans", "#173380")
         , ("https://www.bls.gov/cps/duration.htm", "BLS", "text,tri,sans", "#173380")
         , ("https://intelligence.org/2013/10/03/proofs/",  "miri","svg", "#234e80")
         , ("https://forum.quantifiedself.com/t/indoor-air-quality-monitoring-health/799/40", "QS", "text,sans", "#387cc0")
         , ("https://quantifiedself.com/blog/the-false-god-of-coffee/", "QS", "text,sans", "#387cc0")
         , ("https://www.reddit.com/r/QuantifiedSelf/comments/1mfn0a/trying_to_detect_modafinils_stimulant_effect/", "QS", "text,sans", "#387cc0")
         , ("http://journals.cambridge.org/production/action/cjoGetFulltext?fulltextid=1463440", "⛨", "text", "#ce0101")
         , ("https://www.cambridge.org/core/journals/journal-of-economic-history/article/two-centuries-of-productivity-growth-in-computing/856EC5947A5857296D3328FA154BA3A3", "⛨", "text", "#ce0101")
         , ("https://static.cambridge.org/binary/version/id/urn:cambridge.org:id:binary-alt:20181009171208-81978-mediumThumb-S0033291718001873_fig1g.jpg?pub-status=live", "⛨", "text", "#ce0101")
         , ("/doc/ai/1992-dreyfus-whatcomputerstillcantdo.epub",  "EPUB","text,quad,sans", "#87ba11")
         , ("https://www.rollingstone.com/culture/culture-features/elon-musk-the-architect-of-tomorrow-120850/", "𝓡 𝐒", "text", "#d71921")
         , ("https://www.metaculus.com/questions/notebooks/8702/the-promise-and-impact-of-the-next-generation-of-weight-loss-drugs/", "metaculus", "svg", "#283441")
         , ("https://www.research.va.gov/", "VA", "text,sans", "#112e51")
         , ("https://www.justice.gov/archive/usao/cac/Pressroom/2012/045.html",  "DoJ","text,tri", "#162e51")
         , ("https://www.dailydot.com/unclick/dark-web-black-market-reloaded-adam-bunger-gun-sales-arrest/", "D.", "text,sans", "#2a9461")
         , ("https://www.scientificamerican.com/article/the-mind-of-an-octopus/", "SA", "text", "#0376a1")
         , ("/doc/darknet-market/2013-05-05-moore-bitcoinexchangesurvivalanalysis.R",  "code","svg", redR)
         , ("https://cran.r-project.org/web/packages/censReg/index.html",  "R","text", redR)
         , ("https://www.metafor-project.org/doku.php",  "R","text", redR)
         , ("https://github.com/paul-buerkner/brms#overview",  "R","text", redR)
         , ("http://summaries.cochrane.org/CD007176/antioxidant-supplements-for-prevention-of-mortality-in-healthy-participants-and-patients-with-various-diseases", "cochrane-collaboration", "svg", "#b226b1")
         , ("https://www.tandfonline.com/doi/abs/10.1080/02783190209554137", "T&F", "text,tri,sans", "#0068b1")
         , ("https://gameprogrammingpatterns.com/singleton.html", "GPP", "text,tri,sans", "#1487c1")
         , ("https://80000hours.org/podcast/episodes/sam-bankman-fried-high-risk-approach-to-crypto-and-doing-good/", "80k", "text,tri,sans", "#2ebdd1")
         , ("/static/css/links.css",  "code","svg", "#2465f1")
         , ("https://www.johndcook.com/blog/2010/09/13/applied-topology-and-dante-an-interview-with-robert-ghrist/", "JC", "text,sans", "#1ab6f1")
         , ("https://iqtest.dk/main.swf",  "file-video","svg", "#490202")
         , ("https://boingboing.net/2011/02/03/cosmic-commodities-h.html", "bb", "text,mono", "#ff0202")
         , ("/doc/ai/1986-michie-onmachineintelligence.pdf#page=99",  "pdf","svg", redAdobe)
         , ("https://pdfs.semanticscholar.org/00d3/6b267777b670abd1a3b98a21bf662245a7c4.pdf",  "pdf","svg", redAdobe)
         , ("https://www.ieee-security.org/TC/SPW2014/papers/5103a209.PDF", "pdf", "svg", redAdobe)
         , ("https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.108.7127&rep=rep1&type=pdf",  "pdf","svg", redAdobe)
         , ("https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.75.2289&rep=rep1&type=pdf",  "pdf","svg", redAdobe)
         , ("/doc/ai/1962-bryson.pdf",  "pdf","svg", redAdobe)
         , ("https://www.antipope.org/charlie/blog-static/2007/03/why_the_commercial_ebook_marke.html", "C.S.", "text,sans", "#921712")
         , ("https://slate.com/health-and-science/2017/06/daryl-bem-proved-esp-is-real-showed-science-is-broken.html",  "S","text,sans", "#2c0022")
         , ("https://news.ycombinator.com/item?id=10012625",  "hacker-news","svg", "#f26522")
         , ("https://hn.algolia.com/#!/story/forever/prefix/0/gameprogrammingpatterns.com", "hacker-news","svg", "#f26522")
         , ("https://www.theinformation.com/", "the-information", "svg", "#f32a52")
         , ("https://www.theguardian.com/books/2013/jul/10/man-behind-dickens-dostoevsky-hoax",  "the-guardian","svg", "#052962")
         , ("http://www.thelancet.com/journals/lancet/article/PIIS0140-6736%2811%2960693-4/abstract", "L", "text", "#004582")
         , ("/doc/fiction/poetry/2011-yvain-iliadaslawsuit.html",  "SSC","text,tri", "#5175c2")
         , ("https://slatestarcodex.com/2015/01/15/depression-is-not-a-proxy-for-social-dysfunction/",  "SSC","text,tri", "#5175c2")
         , ("https://www.astralcodexten.com/p/know-your-amphetamines",  "SSC","text,tri", "#5175c2")
         , ("https://x.com/intent/user?screen_name=Hiramatz&tw_i=303521521249447936",  "twitter","svg", "#1da1f2")
         , ("/doc/reinforcement-learning/openai/2023-11-22-karaswisher-twitter-onsamaltman.html","twitter","svg", "#1da1f2")
         , ("https://developer.x.com/en/doc/twitter-api/v1/rules-and-filtering/search-operators","twitter","svg", "#1da1f2")
         , ("https://x.com/DingchangLin/status/1448809886477860865", "twitter","svg", "#1da1f2")
         , ("https://x.com/quasimondo/status/1064230996793614338",  "twitter","svg", "#1da1f2")
         , ("https://engineering.fb.com/2014/11/14/production-engineering/solving-the-mystery-of-link-imbalance-a-metastable-failure-state-at-scale/",  "facebook","svg", blueFB)
         , ("https://arxiv.org/abs/2004.13637#facebook",  "facebook","svg", blueFB)
         , ("/doc/ai/scaling/2020-bell.pdf#facebook",  "facebook","svg", blueFB)
         , ("https://ai.facebook.com/blog/a-highly-efficient-real-time-text-to-speech-system-deployed-on-cpus/",  "facebook","svg", blueFB)
         , ("https://openreview.net/forum?id=xTJEN-ggl1b", "OR", "text,sans", "#8c1b13")
         , ("https://www.tinyletter.com/",  "\9993","text", "#e72223")
         , ("https://www.youtube.com/channel/UCeNwyKuv5SMnN6ovlpbz1SQ",  "youtube","svg", "#ff0033")
         , ("https://www.youtube.com/watch?v=cG7v9eCq2u4&t=33m49s",  "youtube","svg", "#ff0033")
         , ("https://wandb.ai/wandb_fc/gradient-dissent/reports/What-could-make-AI-conscious-with-Wojciech-Zaremba-co-founder-of-OpenAI--Vmlldzo3NDk3MDI", "wandb", "svg", "#ffcc33")
         , ("https://www.edge.org/conversation/alex_sandy_pentland-the-human-strategy",  "E","text,italic", "#414143")
         , ("https://www.amazon.com/gp/product/B0050MYHBQ/",  "amazon","svg", "#ffce53")
         , ("https://www.amazon.co.jp/%E6%AE%8B%E9%85%B7%E3%81%AA%E5%A4%A9%E4%BD%BF%E3%81%AE%E3%82%88%E3%81%86%E3%81%AB%E2%80%95%E6%96%B0%E4%B8%96%E7%B4%80%E3%82%A8%E3%83%B4%E3%82%A1%E3%83%B3%E3%82%B2%E3%83%AA%E3%82%AA%E3%83%B3JUNE%E8%AA%AD%E6%9C%AC-SUN%E3%83%BCMAGAZINE-MOOK-JUNE%E7%B7%A8%E9%9B%86%E9%83%A8/dp/490601125X",  "amazon","svg", "#ffce53")
         , ("https://predictionbook.com/",  "?","text,sans,bold", "#695173")
         , ("https://tl.net/blogs/283221-worker-rush-part-4-rising-up?view=all", "TL", "text,sans", "#254673")
         , ("https://www.sciencenews.org/article/sleep-debt-exacts-deceptive-cost", "SN","text,sans", "#225483")
         , ("https://www.chicagotribune.com/2004/07/23/hes-a-mamas-boy-at-heart/", "chicago-tribune", "svg", "#024c83")
         , ("http://sl4.org/archive/0812/index.html#19570",  "LW","text", "#7faf83")
         , ("https://www.lesswrong.com/",  "LW","text", "#7faf83")
         , ("https://www.lesswrong.com/posts/37sHjeisS9uJufi4u/scholarship-how-to-do-it-efficiently",  "LW","text", "#7faf83")
         , ("https://www.dailymail.co.uk/health/article-2126761/Bertold-Wiesner-British-scientist-fathered-600-children-donating-sperm-fertility-clinic.html", "𝔐", "text", "#004db3")
         , ("https://aino.bandcamp.com/track/--2",  "audio","svg", "#1da0c3")
         , ("https://github.com/fastai/numerical-linear-algebra/blob/master/README.md","F.ai", "text,tri", "#3399f3")
         , ("https://www.fast.ai/2018/04/30/dawnbench-fastai/", "F.ai", "text,tri", "#3399f3")
         , ("https://www.outsideonline.com/culture/books-media/how-athletes-get-great/", "𝕆", "text", "#ffd204")
         , ("https://yunnansourcing.com/",  "ys","text", "#e99114")
         , ("https://yunnansourcing.us/", "ys","text", "#e99114")
         , ("https://mathoverflow.net/questions/32967/have-any-long-suspected-irrational-numbers-turned-out-to-be-rational",  "stack-exchange","svg", "#f48024")
         , ("https://stackoverflow.com/questions/1197575/can-scripts-be-inserted-with-innerhtml",  "stack-exchange","svg", "#f48024")
         , ("https://crypto.stackexchange.com/questions/2507/can-i-encrypt-user-input-in-a-way-i-cant-decrypt-it-for-a-certain-period-of-tim",  "stack-exchange","svg", "#f48024")
         , ("https://www.poetryfoundation.org/poems/44399/pied-beauty", "POET", "text,quad,sans", "#ed1c24")
         , ("https://www.harney.com/",  "H","text", "#c9ad54")
         , ("https://clinicaltrials.gov/study/NCT03429075",  "FDA","text,tri,sans", "#0078b4")
         , ("https://classic.clinicaltrials.gov/ct2/show/NCT01684306", "FDA","text,tri,sans", "#0078b4")
         , ("https://fis.fda.gov/sense/app/d10be6bb-494e-4cd2-82e4-0135608ddc13/sheet/45beeb74-30ab-46be-8267-5756582633b4/state/analysis",  "FDA","text,tri,sans", "#0078b4")
         , ("/static/build/anchor-checker.php",  "code","svg", "#787cb4")
         , ("https://queue.acm.org/detail.cfm?ref=rss&id=2856460", "acm", "text,tri,sans", "#3795c4")
         , ("https://dl.acm.org/doi/10.1145/3266037.3266090", "acm", "text,tri,sans", "#3795c4")
         , ("https://cacm.acm.org/research/the-science-of-brute-force/", "acm", "text,tri,sans", "#3795c4")
         , ("/doc/iq/2014-tenijenhuis-supplement.doc",  "word-doc","svg", "#277dd4")
         , ("/doc/genetics/heritable/2015-mosing-supplement.docx",  "word-doc","svg", "#277dd4")
         , ("https://www.psychologytoday.com/us/blog/life-bilingual/201906/the-bilingual-advantage-three-years-later", "PT", "text,sans", "#477be4")
         , ("/static/img/icon/deepmind.svg",  "deepmind","svg", blueDM)
         , ("https://arxiv.org/abs/1612.08810#deepmind",  "deepmind","svg", blueDM)
         , ("/doc/reinforcement-learning/model/alphago/2016-silver.pdf#deepmind",  "deepmind","svg", blueDM)
         , ("https://deepmind.google/discover/blog/alphastar-mastering-the-real-time-strategy-game-starcraft-ii/",  "deepmind","svg", blueDM)
         , ("https://docs.google.com/document/d/1MhA3M5ucBD7ZXcWk57_MKZ5jEgPX6_YiKye_EFP-adg/edit",  "word-doc","svg", blueG)
         , ("https://research.com/u/gudmar-thorleifsson-1",  "google-scholar","svg", blueG)
         , ("https://scholar.google.com/citations?user=9hEhCHYAAAAJ",  "google-scholar","svg", blueG)
         , ("https://arxiv.org/pdf/2009.06732#org=google&page=6",  "alphabet","svg", blueG)
         , ("https://arxiv.org/abs/1706.04972#google",  "alphabet","svg", blueG)
         , ("https://gwern.net/doc/cs/hardware/2015-kanev.pdf#google",  "alphabet","svg", blueG)
         , ("https://about.google/",  "alphabet","svg", blueG)
         , ("/doc/personal/google-cse.xml",  "alphabet","svg", blueG)
         , ("https://magenta.tensorflow.org/music-transformer", "alphabet", "svg", blueG)
         , ("https://norvig.com/experiment-design.html", "N", "text,sans", blueG)
         , ("https://groups.google.com/group/ankisrs/",  "\9993","text", blueG)
         , ("https://mega.nz/#!0JVxHQCD!C7ijBpRWNpcL_gubWFR-GTBDJTW1jXI6ThzSxwaw2aE",  "mega","svg", "#dd1405")
         , ("https://abcnews.go.com/Health/MedicalMysteries/story?id=3679532&page=1", "ABC", "text,tri,sans", "#fdc605")
         , ("https://www.abc.net.au/news/2013-08-23/police-turn-attention-to-online-drug-trade/4908264", "ABC", "text,tri,sans", "#fdc605")
         , ("https://www.scmp.com/news/china/science/article/3002346/chinas-first-cloned-police-dog-reports-duty", "SCM", "text,tri", "#ffca05")
         , ("https://www.science.org/doi/10.1126/sciadv.aar3620",  "S","text", "#ca2015")
         , ("https://www.spiegel.de/panorama/justiz/amokschuetze-von-muenchen-tatwaffe-aus-dem-darknet-a-1104461.html", "SPGL", "text,quad", "#e64415")
         , ("https://plato.stanford.edu/entries/naturalism-india/", "SEP", "text,tri", "#8c1515")
         , ("https://foreignpolicy.com/2010/11/23/death-by-a-thousand-cuts-2/", "FP", "text", "#ed3725")
         , ("https://jamanetwork.com/journals/jama/fullarticle/183162", "JAMA", "text,sans,quad", "#d71635")
         , ("https://jamanetwork.com/journals/jamainternalmedicine/fullarticle/414283", "JAMA", "text,sans,quad", "#d71635")
         , ("https://jamanetwork.com/journals/jama/fullarticle/201218", "JAMA", "text,sans,quad", "#d71635")
         , ("https://econlolcats.tumblr.com/",  "tumblr","svg", "#001935")
         , ("https://techcrunch.com/2013/02/23/the-chinese-are-coming-the-chinese-are-coming/", "TC", "text,mono", "#0a8935")
         , ("https://osf.io/preprints/psyarxiv/gjh95/",   "ψ", "text", "#cf1d35")
         , ("https://rspb.royalsocietypublishing.org/content/284/1851/20162562", "RS", "text", "#d31245")
         , ("https://rstb.royalsocietypublishing.org/content/365/1537/73.full", "RS", "text", "#d31245")
         , ("https://www.alignmentforum.org/posts/HhWhaSzQr6xmBki8F/birds-planes-brains-and-ai-against-appeals-to-the-complexity", "AF","text,sans", "#3f51b5")
         , ("https://www.givewell.org/giving101", "GW", "text", blueYahoo)
         , ("https://groups.yahoo.com/group/givewell/message/287", "GW", "text", blueYahoo)
         , ("https://files.givewell.org/files/DWDA%202009/Interventions/Iodine/Bautista%20et%20al%201982.pdf", "GW", "text", blueYahoo)
         , ("https://blog.givewell.org/2012/08/23/how-we-evaluate-a-study/", "GW", "text", blueYahoo)
         , ("https://radiolab.org/podcast/91725-words/transcript", "audio-waveform-lines", "svg", "#8c2ec5")
         , ("https://time.com/6337437/sam-altman-openai-fired-why-microsoft-musk/", "T", "text", "#e90606")
         , ("https://www.medrxiv.org/content/10.1101/2020.05.18.20100685.full",  "chi-dna","svg", "#bd2736")
         , ("https://www.biorxiv.org/content/10.1101/013896.full",  "chi-dna","svg", "#bd2736")
         , ("https://nap.nationalacademies.org/read/25762/chapter/1", "NAP", "text,tri", "#1d1646")
         , ("https://archive.foolz.us/a/thread/77196171/#77207238", "4CH", "text,sans", "#69ac46")
         , ("https://boards.fireden.net/a/thread/185257999/", "4CH", "text,sans", "#69ac46")
         , ("https://boards.4chan.org/jp/", "4CH", "text,sans", "#69ac46")
         , ("https://www.sfgate.com/bayarea/article/test-lab-called-1-billion-over-budget-2921620.php", "SFG", "text,tri,sans", "#ff1d46")
         , ("/doc/creatine/2009-ling-data.xls",  "spreadsheet","svg", "#1ba566")
         , ("/doc/cs/hardware/2010-nordhaus-nordhaus2007twocenturiesofproductivitygrowthincomputing-appendix.xlsx",  "spreadsheet","svg", "#1ba566")
         , ("https://www.sciencedaily.com/releases/2007/05/070525204143.htm",  "SD","text,sans", "#004276")
         , ("https://examine.com/supplements/bacopa-monnieri/", "Eχ", "text,sans", "#5e3b76")
         , ("https://wiki.haskell.org/Xmonad/Config_archive/Gwern's_xmonad.hs",  "code","svg", purpleHaskell)
         , ("/static/build/hakyll.hs",  "code","svg", purpleHaskell)
         , ("https://www.haskell.org/",  "\120524","text", purpleHaskell)
         , ("https://web.archive.org/web/20110415182316/http://packdeps.haskellers.com/",  "\120524","text", purpleHaskell)
         , ("https://tvtropes.org/pmwiki/pmwiki.php/Anime/MobileSuitGundamCharscounterattack",  "TV","text", "#1c6486")
         , ("/batman/2022-04-15-manasuka-artdecobatmantriptych-batman.psd",  "image","svg", "#01c3f6")
         , ("https://www.quora.com/", "Q", "text", "#b92b27")
         , ("/doc/tea/gwern-tea-mineralwaters-bestarm-sequential.webm",  "file-video","svg", "#acd147")
         , ("https://www.teds.ac.uk/about-teds", "TEDS", "text,quad,sans", "#45c1a7")
         , ("https://scottaaronson.blog/?p=1438", "S.A.", "text,sans", "#4181b7")
         , ("https://www.scottaaronson.com/democritus/", "S.A.", "text,sans", "#4181b7")
         , ("https://psycnet.apa.org/fulltext/2024-33486-001.html", "APA", "text,tri,sans", "#2c72b7")
         , ("https://www.cdc.gov/nchs/nvss/births.htm",  "CDC","text,tri", "#0057b7")
         , ("/doc/economics/mechanism-design/quadratic-voting/2018-buterin.pdf", "V.B.", "text,sans", "#337ab7")
         , ("https://vitalik.eth.limo/general/2017/09/14/prehistory.html", "V.B.", "text,sans", "#337ab7")
         , ("https://www.reuters.com/article/us-russia-kant-shooting/man-shot-in-russia-in-argument-over-kant-idUSBRE98F0DI20130916", "R", "text,sans", "#e56218")
         , ("https://www.imdb.com/title/tt0923592/", "IMDb", "text,sans,quad", "#f5c518")
         , ("/static/img/icon/video.svg",  "image","svg", "#ffb338")
         , ("https://www.gq.com/story/the-last-true-hermit", "GQ", "text,sans", "#c6a348")
         , ("https://www.atlasobscura.com/articles/cyoa-choose-your-own-adventure-maps", "atlas-obscura", "svg", "#ad8f68")
         , ("https://www.uptontea.com/oolong-tea/c/formosa-oolong-tea/",  "upton-tea","svg", "#365888")
         , ("https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3774989", "SSRN", "text,quad", "#007398")
         , ("https://www.animenewsnetwork.com/anime-spotlight/2018/summer/revue-starlight/.132471",  "ANN","text,tri", "#006598")
         , ("https://www.huffpost.com/entry/bill-zeller-dead-princeto_n_805689", "HUFF", "text,quad,sans", "#0dbe98")
         , ("https://www.connectedpapers.com/main/1ffe143b40a9f8c01940c7397280de4cf666d635/Lessons-from-AlphaZero-for-Optimal%2C-Model-Predictive%2C-and-Adaptive-Control/graph", "connected-papers","svg", "#74b7b8")
         , ("https://mlp.fandom.com/wiki/A_Canterlot_Wedding_-_Part_1",  "MLPW","text,quad,sans", "#abe9e8")
         , ("https://cro.sagepub.com/content/15/5/252.full.pdf+html",  "SAGE","text,quad,sans", "#046ff8")
         , ("https://gwern.substack.com/",  "substack","svg", "#ff6719")
         , ("/static/js/sidenotes.js",  "code","svg", "#f6da19")
         , ("https://bitcointalk.org/index.php?topic=82952.0;all",  "bitcoin","svg", "#ef8e19")
         , ("https://en.bitcoin.it/wiki/Witcoin",  "bitcoin","svg", "#ef8e19")
         , ("https://www.blockchain.com/explorer/addresses/btc/15bD6fYs6p9D9wmniDtTBcQSyWXDYNDCwv", "bitcoin","svg", "#ef8e19")
         , ("https://www.nasa.gov/mission_pages/station/expeditions/expedition30/tryanny.html", "nasa", "svg", "#dc3329")
         , ("https://www.nasa.gov/history/rogersrep/v2appf.htm",                                    "nasa", "svg", "#dc3329")
         , ("https://science.nasa.gov/science-news/science-at-nasa/2005/03jun_naps/",           "nasa", "svg", "#dc3329")
         , ("https://www.cerebras.net/press-release/cerebras-announces-third-generation-wafer-scale-engine", "C", "text,sans", "#f05a29")
         , ("https://arxiv.org/abs/2309.10818#cerebras", "C", "text,sans", "#f05a29")
         , ("https://www.emacswiki.org/emacs/MarkdownMode", "emacs", "svg", "#c04c39")
         , ("https://www.gnu.org/software/emacs/manual/html_node/emacs/Keyboard-Macro-Counter.html", "emacs", "svg", "#c04c39")
         , ("https://www.reddit.com/r/emacs/comments/1530yh8/kalman_reti_the_last_symbolics_developer_speaks/", "emacs", "svg", "#c04c39")
         , ("https://blottyparchment.livejournal.com/7541.html?thread=233845", "LJ", "text,sans", "#004359")
         , ("https://www.statnews.com/2021/11/09/largest-psilocybin-trial-finds-psychedelic-effective-treating-serious-depression/", "stat-news", "svg", "#008299")
         , ("https://www.fanfiction.net/s/10360716/1/The-Metropolitan-Man",  "FFN","text,tri,sans", "#333399")
         , ("https://ieeexplore.ieee.org/xpls/abs_all.jsp?arnumber=602492", "IEEE", "text,mono,quad", "#006699")
         , ("https://spectrum.ieee.org/classical-chinese", "IEEE", "text,mono,quad", "#006699")
         , ("https://paulgraham.com/hundred.html",  "pg","text,mono", "#666699")
         , ("https://www.metopera.org/season/2019-20-season/madama-butterfly/", "Met", "text,tri", "#9c9899")
         , ("https://www.pnas.org/doi/10.1073/pnas.0610941104",  "PNAS","text,quad", "#1f75b9")
         , ("https://www.tiktok.com/@dale_ebert/video/7301073510267407658", "tiktok", "svg", "#65c3c9")
         , ("http://silkroadvb5piz3r.onion/index.php/silkroad/user/69a6bec290", "SR1", "text,tri,sans", "#105a0a")
         , ("http://silkroad5v7dywlc.onion/index.php?topic=2889.0", "SR1", "text,tri,sans", "#105a0a")
         , ("https://www.theatlantic.com/business/archive/2011/06/beware-the-stunning-pilot-program/240352/",  "A","text,italic", "#e7131a")
         , ("https://www.vanityfair.com/news/2012/10/michael-lewis-profile-barack-obama",  "VF","text", "#e7131a")
         , ("https://theconversation.com/altruism-in-birds-magpies-have-outwitted-scientists-by-helping-each-other-remove-tracking-devices-175246", "🗨", "text", "#d8352a")
         , ("https://qwantz.com/index.php?comic=1896", "DC", "text,sans", "#40d53a")
         , ("https://www.goodreads.com/api",  "GR","text,sans", "#8a5e4a")
         , ("https://samuraijack.fandom.com/wiki/Episode_XL:_Jack_vs._the_Ninja", "♡","text", "#fa005a")
         , ("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2793346/",  "nlm-ncbi","svg", "#20558a")
         , ("https://pmc.ncbi.nlm.nih.gov/articles/PMC10714284/",  "nlm-ncbi","svg", "#20558a")
         , ("https://forum.effectivealtruism.org/posts/nSot23sAjoZRgaEwa/2016-ai-risk-literature-review-and-charity-comparison", "EA", "text", "#06819a")
         , ("https://www.effectivealtruism.org/articles/prospecting-for-gold-owen-cotton-barratt#heavy-tailed-distributions", "EA", "text", "#06819a")
         , ("https://maggieappleton.com/bidirectionals", "maggie-appleton", "svg", "#04a4ba")
         , ("https://www.nber.org/papers/w16082",  "NBER","text,quad", "#075dba")
         , ("https://ideas.repec.org/p/nbr/nberwo/27053.html", "NBER", "text,quad", "#075dba")
         , ("https://paperswithcode.com/method/dilated-convolution", "PwC", "text,tri,sans", "#21a7ea")
         , ("https://www.projectrho.com/public_html/rocket/futurelang.php", "ρ", "text", "#00fcfa")
         , ("https://www.economist.com/briefing/2017/02/18/hello-again-dolly",  "E","text,sans", "#e3120b")
         , ("https://scholars-stage.org/meditations-on-maoism-ye-fus-hard-road-home/",  "Ss","text", "#3f1d0b")
         , ("https://arxiv.org/abs/0707.1051",  "\120536","text", "#b31b1b")
         , ("https://aclanthology.org/2021.naacl-main.97/",  "\120536","text", "#b31b1b")
         , ("https://proceedings.mlr.press/v37/xuc15.pdf",  "\120536","text", "#b31b1b")
         , ("/doc/design/typography/rubrication/1990-tufte-envisioninginformation-ch5-byrneseuclid.pdf", "ET", "text", redTufte)
         , ("https://www.edwardtufte.com/bboard/images/0000yO-774.gif", "ET", "text", redTufte)
         , ("https://github.com/jez/tufte-pandoc-jekyll", "ET", "text", redTufte)
         , ("https://github.com/clayh53/tufte-jekyll", "ET", "text", redTufte)
         , ("https://cran.r-project.org/web/packages/tufte/index.html", "ET", "text", redTufte)
         , ("https://edwardtufte.github.io/tufte-css/#epigraphs", "ET", "text", redTufte)
         , ("https://github.com/edwardtufte/tufte-css", "ET", "text", redTufte)
         , ("https://www.fadedpage.com/showbook.php?pid=20160325", "PG", "text", yellowPG)
         , ("https://gutenberg.ca/ebooks/smithcordwainer-onthegemplanet/smithcordwainer-onthegemplanet-00-h.html", "PG","text", yellowPG)
         , ("https://gutenberg.net.au/ebooks02/0201141h.html", "PG","text", yellowPG)
         , ("https://www.gutenberg.org/files/31663/31663-h/31663-h.htm", "PG","text", yellowPG)
         , ("https://blog.23andme.com/articles/genes-scream-for-ice-cream", "23", "text", "#a40e7b")
         , ("https://bjo.bmj.com/content/93/8/997",  "bmj","text,tri,sans", "#2a6ebb")
         , ("https://x.com/patio11/status/1635413289449721856", "pt11", "text,quad,mono", bluePatio11)
         , ("https://training.kalzumeus.com/newsletters/archive/saas_pricing", "pt11", "text,quad,mono", bluePatio11)
         , ("https://www.bitsaboutmoney.com/archive/the-infrastructure-behind-atms/", "pt11", "text,quad,mono", bluePatio11)
         , ("https://www.kalzumeus.com/2018/10/19/japanese-hometown-tax/", "pt11", "text,quad,mono", bluePatio11)
         , ("https://www.rand.org/pubs/monographs/MG1026.html",  "RAND","text,quad,sans", "#751ddb")
         , ("https://researchers.wls.wisc.edu/about/history/", "WLS", "text,tri,sans", "#c5050c")
         , ("https://waitbutwhy.com/whatsourproblem", "♔", "text", "#fd992c")
         , ("https://www.deviantart.com/caji9i/art/stylegan-neural-ahegao-842847987", "DA", "text,sans", "#00fe8c")
         , ("https://www.w3.org/International/wiki/Case_folding", "W3", "text,sans", "#005a9c")
         , ("https://www.nlsinfo.org/content/cohorts/nlsy97", "NLS", "text,tri,sans", "#0071bc")
         , ("https://www.cell.com/ajhg/fulltext/S0002-9297(18)30405-1",  "CELL","text,quad,sans", "#007dbc")
         , ("https://www.vice.com/en/article/i-bought-a-book-about-the-internet-from-1994-and-none-of-the-links-worked/", "VICE", "text,quad,italic", "")
         , ("https://www.vice.com/en/article/the-silk-road-is-showing-cracks/", "VICE", "text,quad,italic", "")
         , ("https://scienceblogs.com/clock/2006/10/16/what-is-a-natural-sleep-patter", "Sᵇ", "text,sans,italic", "")
         , ("https://arxiv.org/abs/2003.13590#microsoft",  "MS","text,sans,italic", "")
         , ("https://www.microsoft.com/en-us/research/blog/turing-nlg-a-17-billion-parameter-language-model-by-microsoft/",  "MS","text,sans,italic", "")
         , ("https://www.salon.com/2007/11/01/whistleblowers/",  "s","text", "#ed2c1d")
         , ("https://longbets.org/",  "X","text,overline", "#2a393d")
         , ("https://longnow.org/ideas/lenski-long-term-evolution-experiment/",  "X","text,overline", "#2a393d")
         , ("https://nintil.com/epigenetic-clocks", "𝓝", "text", "#c05b4d")
         , ("https://medium.com/huggingface/distilbert-8cf3380435b5", "\129303", "text", "#ffcc4d")
         , ("https://huggingface.co/spaces/teven-projects/calculator", "\129303", "text", "#ffcc4d")
         , ("https://github.com/huggingface/transformers", "\129303", "text", "#ffcc4d")
         , ("https://www.overcomingbias.com/p/stupider-than-you-realizehtml",  "OB","text", "#263f5d")
         , ("https://mason.gmu.edu/~rhanson/ideafutures.html",  "OB","text", "#263f5d")
         , ("https://www.jneurosci.org/content/32/12/4156.full", "JN", "text", "#104b7d")
         , ("https://www.odt.co.nz/news/dunedin/student-drug-dealer-jailed", "ODT", "text,tri", "#1a65ad")
         , ("/doc/science/1966-mathematicalassociationofamerica-documentary-maavideoclassics2-johnvonneumanadocumentary.mkv", "file-video", "svg", "#0067ad")
         , ("https://rjlipton.com/2015/07/28/playing-chess-with-the-devil/", "P = NP", "text,quad", "")
         , ("http://archive.recapthelaw.org/paed/203025/", "PACR", "text,quad", "")
         , ("https://www.courtlistener.com/docket/16288633/1/united-states-v-takowsky/", "PACR", "text,quad", "")
         , ("https://publicdomainreview.org/essay/the-lost-world-of-the-london-coffeehouse/",  "TPDR","text,quad", "")
         , ("https://magazine.atavist.com/whatsoever-things-are-true/", "Atvt", "text,quad", "")
         , ("https://read.atavist.com/american-hippopotamus", "Atvt", "text,quad", "")
         , ("https://tasvideos.org/3653M", "TASV", "text,quad", "")
         , ("https://text.npr.org/974534021", "npr", "text,tri,sans", "#237bbd")
         , ("https://www.npr.org/2011/04/16/135450214/eight-is-too-much-for-short-sleepers", "npr", "text,tri,sans", "#237bbd")
         , ("https://www.thenewatlantis.com/publications/correlation-causation-and-confusion", "NA", "text", "#2c6cbd")
         , ("https://poniesatdawn.bandcamp.com/album/dreamwalkers",  "P@D","text,tri", "#27050e")
         , ("https://www.mirror.co.uk/news/uk-news/first-picture-teenager-accused-plotting-6124856", "M", "text,sans", "#e90e0e")
         , ("https://www.supermemo.com/en/blog/twenty-rules-of-formulating-knowledge", "SM", "text,sans", "#f7921e")
         , ("https://super-memory.com/articles/theory.htm", "SM", "text,sans", "#f7921e")
         , ("https://www.mail-archive.com/cryptography@metzdowd.com/msg09959.html",  "\9993","text", "#a01e1e")
         , ("https://www.candyjapan.com/2013-year-in-review", "🍬", "text", "#e0423e")
         , ("https://knowyourmeme.com/memes/navy-seal-copypasta", "KYM", "text,tri", "#13133e")
         , ("https://laion.ai/blog/coca/", "laion", "svg", "#1d374e")
         , ("https://github.com/LAION-AI/laion-datasets/blob/main/laion-aesthetic.md", "laion", "svg", "#1d374e")
         , ("https://arxiv.org/abs/2111.02114#laion", "laion", "svg", "#1d374e")
         , ("https://everything2.com/title/2015+%253A+The+Last+Year+of+Ryoji+Kaji", "E2", "text", "#38495e")
         , ("https://i.imgur.com/atMz0jg.png",  "image","svg", "#1bb76e")
         , ("https://www.ft.com/content/009050e4-75ea-11e2-9891-00144feabdc0", "FT", "text", "#e3b68e")
         , ("https://myanimelist.net/anime/1370/Atama_Yama",  "MAL","text,tri,sans", "#2b498e")
         , ("https://qz.com/1028528/custos-startup-uses-bitcoin-bounties-to-make-pirates-rat-on-one-another", "QZ", "text,sans", "#105b8e")
         , ("https://www.semafor.com/article/03/24/2023/the-secret-history-of-elon-musk-sam-altman-and-openai", "SMFR", "text,quad", "#ffe1be")
         , ("/doc/psychology/willpower/2019-01-21-eric-socksurvey.ods",  "spreadsheet","svg", "#0586ce")
         , ("https://aiimpacts.org/2019-recent-trends-in-gpu-price-per-flops/", "AII", "text,tri", "#2396ce")
         , ("http://esr.ibiblio.org/?p=7183", "ESR","text,tri,sans", "#0000ee")
         , ("https://dl.dropboxusercontent.com/u/182368464/umineko-compress.tar.xz",  "dropbox","svg", "#0061fe")
         , ("https://memteaimports.com/tea/fern-stream-amber-oolong", "MT", "text,sans", "#951d1f")
         , ("https://academic.oup.com/ije/article/43/3/775/758445",  "OUP","text,tri", "#011d3f")
         , ("https://academic.oup.com/ageing/article/36/5/507/40586", "OUP", "text,tri", "#011d3f")
         , ("https://ajcn.nutrition.org/content/69/5/842.full", "OUP", "text,tri", "#011d3f")
         , ("https://www.robots.ox.ac.uk/~vgg/data/fgvc-aircraft/", "OUP", "text,tri", "#011d3f")
         , ("https://scp-wiki.wikidot.com/antimemetics-division-hub", "SCP", "text,tri,sans", "#823f3f")
         , ("https://x.com/jackclarkSF/status/1571125410108407808", "anthropic", "svg", brownAnthropic)
         , ("https://arxiv.org/abs/2207.05221#anthropic", "anthropic", "svg", brownAnthropic)
         , ("https://transformer-circuits.pub/2022/in-context-learning-and-induction-heads/index.html#anthropic", "anthropic", "svg", brownAnthropic)
         , ("https://www.anthropic.com/news/anthropic-raises-124-million-to-build-more-reliable-general-ai-systems", "anthropic", "svg", brownAnthropic)
         , ("https://jack-clark.net/2022/10/31/import-ai-308-recursively-self-improving-lms-3-1tb-of-code-data-dall-e2-makes-alien-errors/", "anthropic", "svg", brownAnthropic)
         , ("https://www.metafilter.com/183095/On-having-sufficient-complexity-to-allow-for-arbitrary-computation", "MF", "text,sans,italic", "#065a8f")
         , ("https://ask.metafilter.com/16136/Fog-Gun-Shower", "MF", "text,sans,italic", "#065a8f")
         , ("https://patch.com/california/davis/davis-pair-arrested-after-cops-intercept-3-000-suspected-ecstasy-pills-mail-serve", "P", "text,sans", "#005d8f")
         , ("https://marginalrevolution.com/",  "M\119825","text", "#00c79f")
         , ("https://conversationswithtyler.com/episodes/seth-godin/",  "M\119825","text", "#00c79f")
         , ("https://www.fimfiction.net/story/62074/Friendship-is-Optimal", "FIMF", "text,quad,mono", "#3b68af")
         , ("https://sg.finance.yahoo.com/news/japan-makes-bitcoin-linked-drug-arrest-165138422--finance.html", "Y!", "text,sans", "#5e21cf")
         , ("https://groups.yahoo.com/group/tiffanygrantfanclub/message/5697",  "\9993","text", "#5e21cf")
         , ("https://www.theverge.com/2021/8/7/22614450/unopened-copy-super-mario-bros-sells-2-million-record", "▽", "text", "#5200ff")
         , ("https://www.stuff.co.nz/manawatu-standard/news/69472334/kiwi-man-jailed-for-posting-drugs-from-las-vegas-to-mothers-house", "NZ", "text,sans", "#d1a3ff")
         , ("https://vimeo.com/28735982", "file-video", "svg", "#17d5ff")
         , ("https://player.vimeo.com/video/218478638", "file-video", "svg", "#17d5ff")
         , ("https://www.businessinsider.com/this-is-what-happens-when-you-track-your-sleep-obsessively-2012-2", "BI", "text,sans", "#002aff")
         , ("https://www.crunchbase.com/person/james-c-gaither", "cb", "text,sans", "#146aff")
         , ("https://patrickcollison.com/labs", "PC", "text,sans", "#635bff")
         , ("https://press.stripe.com/the-art-of-doing-science-and-engineering", "S", "text,sans", "#635bff")
         , ("https://www.kaggle.com/datasets/ultrajack/modern-renaissance-poetry", "k", "text,sans", "#20beff")
         , ("https://www.patreon.com/gwern",  "patreon","svg", "#f76159")
         , ("https://en.wikiquote.org/wiki/Talk:Edsger_W._Dijkstra#Telescope", "wikipedia","svg", "")
         , ("https://commons.wikimedia.org/wiki/File:Energy_density.svg",  "wikipedia","svg", "")
         , ("https://www.mediawiki.org/wiki/Multilingual_MediaWiki", "wikipedia","svg", "")
         , ("https://en.wikisource.org/wiki/Essays_on_Political_Economy/That_Which_Is_Seen,_and_That_Which_Is_Not_Seen",  "wikipedia","svg", "")
         , ("https://en.wikipedia.org/wiki/Document_classification", "wikipedia","svg", "")
         , ("https://en.wikipedia.org/wiki/Named-entity_recognition", "wikipedia","svg", "")
         , ("https://xtools.wmcloud.org/pages/en.wikipedia.org/Gwern", "wikipedia","svg", "")
         , ("https://en.wiktionary.org/wiki/bien_pensant",  "wikipedia","svg", "")
         , ("https://foundation.wikimedia.org/wiki/Privacy_policy",  "wikipedia","svg", "")
         , ("https://stats.grok.se/en/201109/Accountancy", "wikipedia","svg", "")
         , ("https://en.wikibooks.org/wiki/Category:Book:Accountancy", "wikipedia","svg", "")
         , ("https://diff.wikimedia.org/2009/11/26/wikipedias-volunteer-story/",  "wikipedia","svg", "")
         , ("http://www.scholarpedia.org/article/Applications_of_algorithmic_information_theory", "scholarpedia", "svg", "")
         , ("https://www.econtalk.org/adam-cifu-on-ending-medical-reversal/", "econlib", "svg", "")
         , ("https://www.econlib.org/archives/2016/10/what_do_crimina.html", "econlib", "svg", "")
         , ("https://betonit.blog/2022/03/02/make-desertion-fast/", "econlib", "svg", "")
         , ("https://archiveprogram.github.com/",  "github","svg", "")
         , ("https://compvis.github.io/taming-transformers/",  "github","svg", "")
         , ("https://distill.pub/2016/augmented-rnns/",  "distillpub","svg", "")
         , ("/static/font/dropcap/de-zs/DeutscheZierschrift-M.ttf",  "misc","svg", "")
         , ("/doc/dual-n-back/2012-zhong.ebt",  "misc","svg", "")
         , ("/doc/cs/linkrot/2009-08-20-b3ta-fujitsuhtml.mht",  "misc","svg", "")
         , ("https://www.wired.com/2012/01/everything-about-learning/",  "wired","svg", "")
         , ("https://www.wired.com/story/lsd-microdosing-drugs-silicon-valley/", "wired","svg", "")
         , ("https://www.erowid.org/",  "erowid","svg", "#a06929")
         , ("https://www.drugsdata.org/results.php?start=0&search_field=all&s=modafinil", "erowid","svg", "#a06929")
         , ("https://www.yudkowsky.net/rational/technical", "yud", "svg", "")
         , ("/static/nginx/twdne.conf",  "code","svg", "")
         , ("/doc/ai/music/2019-12-22-gpt2-preferencelearning-gwern-abcmusic.patch",  "code","svg", "")
         , ("/static/build/markdown-lint.sh",  "code","svg", "")
         , ("/static/template/default.html",  "code","svg", "")
         , ("/doc/personal/businesscard-front-draft.xcf",  "image","svg", "")
         , ("/doc/rotten.com/library/sex/sexual-ethics-in-psychology/55403_m.gif",  "image","svg", "")
         , ("/doc/iq/ses/2011-gensowski-figure7-totaleffectofiqandpersonalityonlifetimeearnings.png",  "image","svg", "")
         , ("/doc/anime/eva/2010-1000enpark-tokyo-oota-heiwajimakoen.jpg",  "image","svg", "")
         , ("/static/img/favicon.ico",  "image","svg", "")
         , ("/doc/rotten.com/library/bio/hackers/robert-morris/morris.bmp",  "image","svg", "")
         , ("https://apps.apple.com/us/app/better-brain-lite/id307920888", "apple", "svg", "")
         , ("https://machinelearning.apple.com/research/hey-siri", "apple", "svg", "")
         , ("/doc/darknet-market/usareshipper-profile.maff", "archive","svg", "#e66000")
         , ("/doc/zeo/firmware-v2.6.3R-zeo.img",  "archive","svg", "")
         , ("/doc/ai/anime/danbooru/2019-02-10-stylegan-holo-handselectedsamples.zip",  "archive","svg", "")
         , ("/doc/psychology/spaced-repetition/michaellee-memoryretentionexperiments-data.tar",  "archive","svg", "")
         , ("https://babel.hathitrust.org/cgi/pt?id=uc1.c101988734&view=1up&seq=1", "internet-archive","svg", "")
         , ("https://www.webcitation.org/6Qj7v6mqd",  "internet-archive","svg", "")
         , ("https://timetravel.mementoweb.org/",  "internet-archive","svg", "")
         , ("https://web.archive.org/web/20120702152514/https://www.nlm.nih.gov/news/calhoun_papers_released.html",  "internet-archive","svg", "")
         , ("https://web.archive.org/web/19981202185145/http://www.ex.org/2.4/11-news.html",  "internet-archive","svg", "")
         , ("https://wiki.archiveteam.org/index.php?title=Google_Reader",  "internet-archive","svg", "")
         , ("https://blog.archive.org/2011/08/17/scanning-a-braille-playboy/",  "internet-archive","svg", "")
         , ("https://hivemind-repo.s3-us-west-2.amazonaws.com/twdne3/twdne3.onnx",  "archive","svg", "")
         , ("/doc/touhou/2013-06-08-acircle-tohoarrange.mdb.xz",  "archive","svg", "")
         , ("/doc/ai/nn/rnn/2015-06-03-karpathy-charrnn-visualization.tar.xz",  "archive","svg", "")
         , ("https://www.telegraph.co.uk/culture/books/3601644/Adultery-was-his-thing.html", "the-telegraph", "svg", "")
         , ("https://chatgpt.com/share/261034ff-f5d5-404c-b354-c9d58e3af509", "openai","svg", "")
         , ("https://gptprompts.wikidot.com/context-stuffing",                "openai","svg", "")
         , ("https://arxiv.org/abs/1611.02779#openai",                        "openai","svg", "")
         , ("/doc/ai/nn/transformer/gpt/dall-e/1/2020-chen-2.pdf#openai",     "openai","svg", "")
         , ("https://openai.com/index/better-language-models/",               "openai","svg", "")
         , ("https://blog.eleuther.ai/announcing-20b/", "eleutherai", "svg", "")
         , ("https://blog.eleuther.ai/year-one/", "eleutherai", "svg", "")
         , ("https://6b.eleuther.ai/", "eleutherai", "svg", "")
         , ("https://pile.eleuther.ai/", "eleutherai", "svg", "")
         , ("https://arankomatsuzaki.wordpress.com/2021/06/04/gpt-j/", "eleutherai", "svg", "")
         , ("https://rd.springer.com/article/10.1007/s10071-021-01530-3",  "springerlink", "svg", "")
         , ("https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-4-13",  "springerlink", "svg", "")
         , ("https://link.springer.com/article/10.1007/BF02253535",  "springerlink", "svg", "")
         , ("https://link.springer.com/article/10.3758/s13423-021-01927-8", "springerlink", "svg", "")
         , ("http://libgen.org/search.php?req=%22wheel+of+time%22", "raven", "svg", "")
         , ("https://library.bz/main/upload/", "raven", "svg", "")
         , ("https://libgen.li/", "raven", "svg", "")
         , ("https://www.hoover.org/research/optimistic-thought-experiment", "hoover-institution", "svg", "")
         , ("/doc/sociology/technology/2017-reddit-dhieno-theplace-timelapseevolution.mp4",  "file-video","svg", "")
         , ("/doc/ai/music/2020-03-06-fifteenai-fluttershy-sithcode.mp3",  "audio","svg", "")
         , ("https://www.newyorker.com/books/page-turner/the-mystery-of-s-the-man-with-an-impossible-memory",  "the-new-yorker","svg", "")
         , ("https://www.alcor.org/library/alcor-membership-statistics/", "alcor", "svg", "")
         , ("https://archive.nytimes.com/6thfloor.blogs.nytimes.com/2013/03/20/a-sham-procedure-leads-to-disappointing-m-s-news/",  "new-york-times","svg", "")
         , ("https://creativecommons.org/licenses/by-nc/3.0/", "creative-commons", "svg", "")
         , ("https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.1000451",  "plos","svg", "")
         , ("https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0023175",  "plos","svg", "")
         , ("https://speakingofmedicine.plos.org/2012/06/25/less-research-is-needed/",  "plos","svg", "")
         , ("https://journals.plos.org/plosmedicine/article/fetchObject.action?uri=info:doi/10.1371/journal.pmed.0020124.t004&representation=PNG_M",  "plos","svg", "")
         , ("https://quoteinvestigator.com/2012/11/11/exhaust-alternatives/", "magnifying-glass", "svg", "")
         , ("https://retractionwatch.com/2011/02/28/crystal-myth-11-more-retractions-from-crystallography-journal-after-2010-fakery/", "magnifying-glass", "svg", "")
         , ("/design#backlink", "arrows-pointing-inwards-to-dot", "svg", "")
         , ("/metadata/annotation/backlink/https%3A%2F%2Fmathstodon.xyz%2F%40tao%2F110172426733603359.html", "arrows-pointing-inwards-to-dot", "svg", "")
         , ("#backlinks", "arrows-pointing-inwards-to-dot", "svg", "")
         , ("https://www.washingtonpost.com/graphics/2018/investigations/dog-auction-rescue-groups-donations/",  "washington-post","svg", "")
         , ("https://pastebin.com/GrV3uYh5", "txt", "svg", "")
         , ("/lorem.md",  "txt","svg", "")
         , ("/doc/personal/rss-subscriptions.opml",  "txt","svg", "")
         , ("/doc/ai/anime/danbooru/2020-06-08-danbooru2019-palm-handannotations-export.jsonl",  "txt","svg", "")
         , ("/doc/touhou/2013-c84-downloads.json",  "txt","svg", "")
         , ("/doc/ai/poetry/2019-10-17-117m-poetry-cleanprojectgutenberg-samples.txt",  "txt","svg", "")
         , ("/doc/cat/psychology/drug/catnip/survey/2017-07-30-gs-pilot.csv",  "csv","svg", "")
         , ("https://ctan.org/pkg/marginnote", "tex","svg", "")
         , ("https://texample.net/tikz/examples/andler-optimal-lot-size/", "tex","svg", "")
         , ("https://www.tug.org/whatis.html", "tex","svg", "")
         , ("https://tug.org/FontCatalogue/goudyinitialen/", "tex","svg", "")
         , ("https://thepiratebay.org/description.php?id=14045031", "the-pirate-bay", "svg", "")
         , ("/metadata/annotation/link-bibliography/%252Freview%252Fmovie.html", "bibliography", "svg", "")
         , ("/design#link-bibliographies", "bibliography", "svg", "")
         , ("#link-bibliography", "bibliography", "svg", "")
         , ("https://www.openphilanthropy.org/research/how-much-computational-power-does-it-take-to-match-the-human-brain/", "open-philanthropy", "svg", "")
         , ("https://www.lrb.co.uk/the-paper/v42/n18/james-lasdun/bats-on-the-ceiling", "LRB", "text,tri", "")
         , ("https://www.wsj.com/articles/SB10000872396390443696604577647870908169992",  "WSJ","text,tri", "")
         , ("https://www.pewresearch.org/social-trends/2012/02/16/the-rise-of-intermarriage/", "Pew", "text,tri", "")
         , ("https://blog.samaltman.com/value-is-created-by-doing", "sama", "text,quad,mono", "")
         , ("https://ourworldindata.org/grapher/burden-disease-from-each-mental-illness", "OWID", "text,quad,mono", "")
         , ("https://scale.com/", "SCLE", "text,quad,mono", "")
         , ("https://bldgblog.com/2015/12/four-floor-war/", "BLDG", "text,quad,mono", "")
         , ("https://jaspervdj.be/hakyll/reference/Hakyll-Web-Redirect.html", "JVDJ", "text,quad,mono", "")
         , ("https://qntm.org/invisibility", "qntm", "text,quad,mono", "")
         , ("https://gizmodo.com/weird-and-wonderful-movies-that-youll-never-get-to-see-5877874", "GIZM", "text,quad,mono", "")
         , ("https://nunosempere.com/blog/2023/01/30/an-in-progress-experiment-to-test-how-laplace-s-rule-of/", "nuno", "text,quad,mono", "")
         , ("http://blog.sigfpe.com/2005/08/absence-of-evidence-is-evidence-of.html", "sgfp", "text,quad,mono", "")
         , ("https://people.eecs.berkeley.edu/~janner/trajectory-transformer/files/trajectory-transformer.pdf", "BAIR", "text,quad,mono", "")
         , ("https://bair.berkeley.edu/blog/2020/07/11/auction/", "BAIR", "text,quad,mono", "")
         , ("https://sites.google.com/berkeley.edu/decision-transformer", "BAIR", "text,quad,mono", "")
         , ("https://mitpress.mit.edu/9780262536226/", "MIT", "text,tri,mono", "")
         , ("https://people.csail.mit.edu/mrub/VisualMic/", "MIT", "text,tri,mono", "")
         -- Arxiv overrides:
         , ("https://vndb.org/c582", "VNDB", "text,quad,sans", "")
         , ("https://www.cnbc.com/2017/11/15/microsoft-and-github-unveil-pair-programming-tools.html", "CNBC", "text,quad,sans", "")
         , ("https://groups.google.com/g/pandoc-discuss/c/jgb-Q0F2p1Y/m/1DogIEAtAQAJ", "PNDC", "text,quad,sans", "")
         , ("https://what-if.xkcd.com/145/",  "XKCD","text,quad,sans", "")
         , ("https://thisanimedoesnotexist.ai/", "TADE", "text,quad,sans", "")
         , ("https://www.thisfursonadoesnotexist.com/", "TFDE", "text,quad,sans", "")
         , ("https://thisponydoesnotexist.net/", "TPDE", "text,quad,sans", "")
         , ("https://www.thiswaifudoesnotexist.net/", "TWDE", "text,quad,sans", "")
         , ("https://www.vetta.org/2009/12/tick-tock-tick-tock-bing/", "Legg", "text,quad,sans", blueDM)
         , ("https://www.bloomberg.com/news/features/2018-05-15/google-amazon-and-facebook-owe-j-rgen-schmidhuber-a-fortune","SMDH", "text,quad,sans", "")
         , ("/doc/ai/nn/rnn/1991-schmidhuber.pdf", "SMDH", "text,quad,sans", "")
         , ("https://www.nytimes.com/2016/11/27/technology/artificial-intelligence-pioneer-jurgen-schmidhuber-overlooked.html", "SMDH", "text,quad,sans", "")
         , ("https://people.idsia.ch/~juergen/creativity.html", "SMDH", "text,quad,sans", "")
         , ("https://arxiv.org/abs/1404.7828#schmidhuber", "SMDH", "text,quad,sans", "")
         , ("https://innsbigdata.wordpress.com/2015/02/09/interview-with-juergen-schmidhuber/", "SMDH", "text,quad,sans", "")
         , ("https://people.idsia.ch/~juergen/metalearning.html", "SMDH", "text,quad,sans", "")
         , ("https://www.mdpi.com/2220-9964/8/5/232", "MDPI","text,quad,sans", "")
         , ("https://web.archive.org/web/20211105092005/https://mitpress.mit.edu/sites/default/files/sicp/full-text/sicp/book/node13.html", "SI CP", "text,quad,sans", "")
         , ("https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/sicp.html", "SI CP", "text,quad,sans", "")
         , ("https://habr.com/ru/articles/516190/", "Habr", "text,quad,sans", "")
         , ("https://warontherocks.com/2021/08/foreign-fighters-and-cheese-bells/", "WOT R", "text,quad,sans", "")
         , ("https://krebsonsecurity.com/2013/07/mail-from-the-velvet-cybercrime-underground/", "Krbs", "text,quad,sans", "")
         , ("https://oeis.org/A001006", "OEIS", "text,quad,sans", "")
         , ("https://mathshistory.st-andrews.ac.uk/Extras/Poincare_Intuition/", "M  T", "text,quad,sans", "")
         , ("https://mattlakeman.org/2020/01/22/hill-billy-elegy-the-culture-of-white-american-poverty/",  "MATT", "text,quad,sans", "")
         , ("http://thehub7dnl5nmcz5.onion/index.php?topic=2261.msg17459", "Hub", "text,tri,sans", "")
         , ("https://www.bbc.com/news/business-43365710",  "BBC","text,tri,sans", "")
         , ("https://news.bbc.co.uk/2/hi/8448731.stm",  "BBC","text,tri,sans", "")
         , ("http://host.robots.ox.ac.uk/pascal/VOC/", "VOC", "text,tri,sans", "")
         , ("https://aidungeon.medium.com/introducing-ai-dungeon-translate-a50e35f6df83", "AID", "text,tri,sans", "")
         , ("https://www.reddit.com/r/AIDungeon/comments/i1qhg0/the_dragon_ai_just_got_worse/", "AID", "text,tri,sans", "")
         , ("https://www.patreon.com/AIDungeon", "AID", "text,tri,sans", "")
         , ("https://latitude.io/blog/how-we-accidentally-gave-our-bots-their-personalities/", "AID", "text,tri,sans", "")
         , ("https://www.cs.utexas.edu/~EWD/transcriptions/EWD03xx/EWD340.html", "EWD", "text,tri,sans", "")
         , ("http://www-biba.inrialpes.fr/Jaynes/cc18i.pdf", "ETJ", "text,tri,sans", "")
         , ("/doc/statistics/bayes/1988-jaynes-maximumentropyandbayesianmethods.pdf", "ETJ", "text,tri,sans", "")
         , ("https://omega0.xyz/omega8008/JaynesBookPdf.html", "ETJ", "text,tri,sans", "")
         , ("https://omega0.xyz/omega8008/ETJ-PDF/cc5d.pdf", "ETJ", "text,tri,sans", "")
         , ("https://thelastpsychiatrist.com/2011/01/why_chinese_mothers_are_not_su.html", "TLP", "text,tri,sans", "")
         , ("https://hbr.org/2019/12/can-you-know-too-much-about-your-organization", "HBR", "text,tri,sans", "#a51c30")
         , ("https://dataverse.harvard.edu/dataset.%C3%97html?persistentId=doi:10.7910/DVN/MVDWCS", "H", "text", "#a51c30")
         , ("https://www.cbsnews.com/colorado/news/man-allegedly-bought-pot-from-colorado-to-sell-in-maryland/", "cbs", "svg", "")
         , ("https://www.cbsnews.com/news/california-biobank-dna-babies-who-has-access/", "cbs", "svg", "")
         , ("https://ocw.mit.edu/courses/18-01sc-single-variable-calculus-fall-2010/", "OCW", "text,tri,sans", "")
         , ("https://qualiacomputing.com/2015/05/22/how-to-secretly-communicate-with-people-on-lsd/", "QC", "text,sans", "")
         , ("http://chronopause.com/chronopause.com/index.php/2011/08/05/science-fiction-double-feature-2-part-2/index.html", "M.D.", "text,sans", "")
         , ("https://www.palladiummag.com/2019/05/09/what-botswana-can-teach-us-about-political-stability/", "Pd", "text,sans", "")
         , ("https://stability.ai/news/stable-diffusion-public-release", "SD", "text,sans", "")
         , ("https://www.ribbonfarm.com/2011/09/23/the-milo-criterion/", "ℝ𝔽", "text,sans", "")
         , ("https://guzey.com/how-life-sciences-actually-work/", "A.G.", "text,sans", "")
         , ("https://www.hustwit.com/urbanized", "H", "text,sans", "")
         , ("https://www.motherjones.com/kevin-drum/2018/02/an-updated-lead-crime-roundup-for-2018/",  "MJ","text,sans", "")
         , ("https://jaymans.wordpress.com/2015/07/04/demography-is-destiny/", "J👨🏾", "text,sans", "")
         , ("https://arankomatsuzaki.wordpress.com/2021/03/04/state-of-the-art-image-generative-models/", "ak", "text,sans", "")
         , ("https://x.com/razibkhan/status/1463204399954776073", "RK", "text,sans", "")
         , ("https://www.unz.com/gnxp/through-the-wormhole-are-we-here-for-a-reason-premier-may-13th/", "RK", "text,sans", "")
         , ("https://www.gnxp.com/WordPress/2017/12/12/most-people-say-they-think-nurture-is-more-important-than-nature-especially-white-americans/", "RK", "text,sans", "")
         , ("https://www.razibkhan.com/p/get-lucky", "RK", "text,sans", "")
         , ("https://elifesciences.org/articles/16351", "eL", "text,sans", "")
         , ("https://dnstats.net/market/Amazon+Dark", "dn", "text,sans", "")
         , ("https://apnews.com/article/ap-top-news-international-news-weekend-reads-china-health-269b3de1af34e17c1941a514f78d764c", "AP", "text,sans", "")
         , ("https://sethroberts.net/2008/10/03/diet-and-acne-continued/", "SR", "text,sans", "")
         , ("https://www.frontiersin.org/journals/human-neuroscience/articles/10.3389/fnhum.2011.00134/full", "FS", "text,sans", "")
         , ("https://web.archive.org/web/20110724123419/szabo.best.vwh.net/bearer_contracts.html", "N.S.", "text,sans", "")
         , ("http://unenumerated.blogspot.com/2011/01/singularity.html", "N.S.", "text,sans", "")
         , ("https://www.newscientist.com/article/2133095-boom-in-human-gene-editing-as-20-crispr-trials-gear-up/", "NS", "text,sans", "")
         , ("https://www.popsci.com/deadly-splinter-antibiotic-resistance/", "PS", "text,sans", "")
         , ("https://www.technologyreview.com/2011/06/21/193829/the-measured-life/",  "T","text,sans", "")
         , ("https://onlinelibrary.wiley.com/doi/full/10.1111/brv.12407",  "W","text,sans", "")
         , ("https://towardsdatascience.com/stylegan2-projection-a-reliable-method-for-image-forensics-700922579236", "\119820","text", "")
         , ("https://medium.com/craft-ventures/the-sharp-startup-when-paypal-found-product-market-fit-5ba47ad35d0b",  "\119820","text", "")
         , ("https://beepb00p.xyz/pkm-search.html", "\129302", "text", "")
         , ("https://statmodeling.stat.columbia.edu/2013/12/17/replication-backlash/",  "\9605\9607\9603","text", "")
         , ("https://statmodeling.stat.columbia.edu/2004/12/29/type_1_type_2_t/",  "\9605\9607\9603","text", "")
         , ("https://www.longecity.org/forum/topic/10464-modalert-is-this-what-modafinil-is-like/?&p=108566#entry108566", "⧖", "text", "")
         , ("https://www.bloomberg.com/businessweek/ap/financialnews/D9KQL7CG0.htm", "\119809","text", "")
         , ("https://www.bloomberg.com/news/articles/2011-03-31/why-unemployment-rose-so-much-dropped-so-fast-commentary-by-alan-krueger",  "\119809","text", "")
         , ("https://www.latimes.com/archives/la-xpm-1988-07-17-tm-9636-story.html", "𝔏A", "text", "")
         , ("https://www.theage.com.au/national/victoria/bitcoin-drug-millions-seized-in-victoria-20141015-116bby.html", "A", "text", "")
         , ("https://www.thebeliever.net/mithradites-of-fond-du-lac/", "𝐁", "text", "")
         , ("https://blog.novelai.net/novelai-improvements-on-stable-diffusion-e10d38db82ac", "🖋", "text", "")
         , ("https://www.fastcompany.com/40438376/after-a-comeback-23andme-faces-its-next-test", "FC", "text", "")
         , ("https://touhou.fandom.com/wiki/Category:Music", "☯", "text", "#e44031")
         , ("https://newcriterion.com/article/a-good-list/", "NC", "text", "")
         , ("https://www.thecut.com/2019/05/the-tinder-hacker.html", "TC", "text", "")
         , ("https://www.discovermagazine.com/mind/the-brain-a-body-fit-for-a-freaky-big-brain", "D", "text", "")
         , ("https://thegradient.pub/gpt2-and-the-nature-of-intelligence/", "∇", "text", "")
         , ("https://harpers.org/archive/1954/12/the-jet-propelled-couch/?single=1", "H", "text", "")
         , ("https://www.nzherald.co.nz/nz/drug-mail-or-mule-risks-the-same/QHX3IGRINL7AN5QZR3JRSOQ3NA/", "𝕳", "text", "")
         , ("https://www.independent.co.uk/news/uk/this-britain/the-jousting-accident-that-turned-henry-viii-into-a-tyrant-1670421.html", "TI", "text", "")
         , ("https://www.reddit.com/r/TOUHOUMUSIC/search/?q=author%3Agwern&sort=new&restrict_sr=on&t=all", "☯", "text", "#e44031")
         , ("https://w.atwiki.jp/toho/pages/948.html", "☯", "text", "#e44031")
         , ("/metadata/annotation/similar/https%3A%2F%2Fgithub.com%2Fnyu-mll%2Fpretraining-learning-curves%2Fblob%2Fmain%2FWhen%2520Do%2520You%2520Need%2520Billions%2520of%2520Words%2520of%2520Pretraining%2520Data.pdf.html", "≈", "text", "")
         , ("https://www.pragmatic.ml/sparse-sinkhorn-attention/", "𝕄", "text", "")
         , ("https://www.smithsonianmag.com/history/native-intelligence-109314481/", "SM", "text", "")
         , ("https://carryiton.net/chain-letter/bibliography.htm", "✉", "text", "")
         , ("https://carryiton.net/chain-letter/evolution.html", "✉", "text", "")
         , ("https://en.touhouwiki.net/wiki/Iyokan", "☯", "text", "#e44031")
         , ("https://blogs.nature.com/news/2011/09/reliability_of_new_drug_target.html",  "n","text", "")
         , ("https://quantum.country/qcvc", "MN", "text", "")
         , ("http://neuralnetworksanddeeplearning.com/chap6.html", "MN", "text", "")
         , ("https://cognitivemedium.com/srs-mathematics", "MN", "text", "")
         , ("https://numinous.productions/ttft/", "MN", "text", "")
         , ("https://michaelnielsen.org/blog/three-myths-about-scientific-peer-review/", "MN", "text", "")
         , ("https://www.theparisreview.org/blog/2018/04/25/the-strange-history-of-the-king-pine/",  "PR","text", "")
         , ("https://www.unqualified-reservations.org/2007/08/james-burnhams-dante-politics-as-wish/", "UR", "text", "")
         , ("https://solar.lowtechmagazine.com/2015/12/fruit-walls-urban-farming-in-the-1600s/", "☀", "text", "")
         , ("/design#similar-links", "≈", "text", "")
         , ("#similars", "≈", "text", "")
         , ("https://crookedtimber.org/2012/05/30/in-soviet-union-optimization-problem-solves-you/", "CT", "text", "")
         , ("https://senseis.xmp.net/", "❍", "text", "")
         , ("https://antilop.cc/sr/#assassination_plot", "෴", "text", "")
         , ("https://www.wolfewiki.com/pmwiki/pmwiki.php?n=Stories.SuzanneDelage", "U","text", "")
         , ("http://lists.urth.net/pipermail/urth-urth.net/2010-December/019108.html",  "U","text", "")
         , ("https://variety.com/2014/film/news/tokyo-festival-hideaki-anno-warns-of-trouble-ahead-for-japanese-animation-1201339991/", "𝓥", "text", "")
         , ("https://nymag.com/intelligencer/2018/07/how-fortnite-became-the-most-popular-video-game-on-earth.html", "𝒩𝒴", "text", "")
         , ("https://worksinprogress.co/issue/getting-materials-out-of-the-lab/", "WiP", "text,tri,mono", "")
         , ("https://www.worksinprogress.news/p/cheap-ornament-and-status-games#%C2%A7did-rich-people-actually-lead-the-flight-from-premodernist-styles", "WiP", "text,tri,mono", "")
         , ("https://books.worksinprogress.co/book/maintenance-of-everything/communities-of-practice/the-soul-of-maintaining-a-new-machine/1", "WiP", "text,tri,mono", "")
         , ("http://messybeast.com/cats-meat-man.htm", "🐾", "text", "#fafa02")
         , ("https://a16z.com/author/scott-kupor/", "az16", "text,quad,sans", "#ed8c00")
         , ("https://a16z.com/politics-and-the-future/", "az16", "text,quad,sans", "#ed8c00")
         , ("https://docs.midjourney.com/docs/weird", "midjourney", "svg", "")
         , ("https://www.midjourney.com/", "midjourney", "svg", "")
         , ("https://danluu.com/web-bloat/", "", "", "#0000ee")
         , ("https://www.thedailybeast.com/vegan-youtube-is-imploding-as-stars-like-rawvana-bonny-rebecca-and-stella-rae-change-diets", "","", "#ea0606")
         , ("https://www.straighttalkonevidence.org/2017/09/22/disappointing-findings-on-conditional-cash-transfers-as-a-tool-to-break-the-poverty-cycle-in-the-united-states/", "", "", "#102659")
         , ("https://blog.codinghorror.com/the-large-display-paradox/", "","", "#57a3e8")
         , ("http://www.codinghorror.com/blog/archives/001076.html", "","", "#57a3e8")
         , ("https://ilovetypography.com/2009/12/08/graphic-masterpieces-of-yakov-g-chernikhov-the-collection-of-dmitry-y-chernikhov/", "", "", "#e05f4c")
         , ("https://dataprivacylab.org/projects/identifiability/pharma1.html", "", "", "#3366cc")
         , ("https://www.thefreelibrary.com/E+unibus+pluram%3A+television+and+U.S.+fiction.-a013952319", "", "", "#064c82")
         , ("https://www.unitedpharmacies-uk.md/Modavigil-Modafinil-100mg-30-Tablets-p-759.html", "", "", "#27c1da")
         , ("https://www.petforums.co.uk/threads/do-any-of-your-cats.225353/#post-1061870480", "", "", "#295b52")
         , ("https://wellcomecollection.org/articles/XV_E7BEAACIAo9Vz", "", "", "#ffce3c")
         , ("https://www.acpjournals.org/doi/10.7326/0003-4819-152-11-201006010-00232?articleid=745807", "", "", "#007377")
         , ("https://digital.library.unt.edu/explore/partners/TAMS/browse/?q=%22Julian+Stanley%22+OR+%22SMPY%22&sort=date_a&t=fulltext", "UNT", "text,tri", "#00863c")
         , ("https://forums.animesuki.com/showpost.php?p=3996631&postcount=387", "̅□", "text", "#008080")
         , ("https://www.theregister.com/2019/01/29/how_i_caught_silk_road_mastermind/?page=2", "𓅐", "text", "#ff0000")
         , ("https://infoproc.blogspot.com/2014/02/hints-of-genomic-dark-matter-rare.html", "Hsu", "text,sans", "")
         , ("https://www.manifold1.com/episodes/robin-hanson-prediction-markets-the-future-of-civilization-and-polymathy-66/transcript#elon-musk", "Hsu", "text,sans", "")
         , ("https://ki.se/en/people/paul-lichtenstein", "☤", "text", "#830154")
         , ("https://www.dwarkeshpatel.com/p/progress-update", "Dwkh", "text,sans,quad", "#f3c016")
         , ("https://ascii.textfiles.com/archives/2229", "ASCI", "text,quad,mono", "#006309")
         , ("https://www.e-codices.unifr.ch/en/vad/0296/079r", "e", "text,sans", "#e7d7a5")
         , ("https://www.bartleby.com/lit-hub/poetry-of-byron/greece-iv/", "b", "text", "#101269")
         , ("https://bugs.darcs.net/issue346", "darcs", "svg", "#6ef701")
         , ("https://www.smh.com.au/technology/underweb-anger-as-silk-road-seller-does-a-runner-20130226-2f36q.html", "S", "text", "#096dd2")
         , ("https://www.rrauction.com/auctions/categories/318/", "RR", "text,sans", "#29648a")
         , ("https://www.mangaupdates.com/series.html?id=2701", "M", "text,sans", "#f8922b")
         , ("https://www.instructables.com/Solar-Powered-Raspberry-Pi/", "🤖", "text", "#fac62d")
         , ("https://www.cs.virginia.edu/~robins/YouAndYourResearch.html", "internet-archive", "svg", "#e57200")
         , ("https://mujoco.org/", "M", "text,sans", "#0053d6")
         , ("https://www.artbreeder.com/browse", "✤", "text", "#8ccaff")
        ]

-- TODO: more complex link-icon testing: suppression of redundant link-icons
-- linkIcon $ Link nullAttr [Str "WSJ"] ("https://www.wsj.com/articles/world-chess-championship-magnus-carlsen-ian-nepomniachtchi-seconds-11638167905", "", "") →
-- Link ("",["icon-not"],[]) [Str "WSJ"] ("https://www.wsj.com/articles/world-chess-championship-magnus-carlsen-ian-nepomniachtchi-seconds-11638167905","","")
