{- LinkLive.hs: Specify domains which can be popped-up "live" in a frame by adding a link class.
Author: Gwern Branwen
Date: 2022-02-26
When:  Time-stamp: "2022-02-26 18:31:24 gwern"
License: CC-0

Based on LinkIcon.hs. At compile-time, set the HTML class `link-live` on URLs from domains verified
to work (reasonably) well as cross-site popups inside a frame.
`extracts-contents.js` at runtime reads the class to decide which links will be live-popup-able.

Live popups are an alternative to, or a further step from, annotations. They let the reader preview
a link instantly. This is useful when an annotation is not available, or when the reader has read
the annotation and wants to go further.

However, due to the March of Web Progress™, many websites set X headers or just plain
don't work in a frame (often due to JS, and then often due to extremely reader-unfriendly design like giant
headers or stickies). Perhaps only a quarter of external links work as live popups.
So we can't just offer it as an option on all links, that will waste reader time & trust, and they will
learn to avoid the feature entirely and resent the visual clutter and trap of this 'feature'.

We instead whitelist domains based on manual testing using the list of links in /Lorem#live-link-popups.
Since there are so many domains, we need a testsuite to keep track of what domains have been tested & found good,
tested & found bad, and testing is not done or ambiguous (due to practical issues like a test link having become
a local archive or 404 or changed domain entirely). -}

{-# LANGUAGE OverloadedStrings #-}
module LinkLive (linkLive, linkLiveTest, urlLive) where

import Data.Text as T (isPrefixOf, isSuffixOf, Text)
import Text.Pandoc (Inline(Link), nullAttr)

import LinkIcon (host)
import Utils (addClass)

linkLive :: Inline -> Inline
linkLive x@(Link (_,cl,_) _ (u, _))
 | "link-live-not" `elem` cl = x
 | u `elem` overrideLinkLive = aL x
 | "/" `isPrefixOf` u = x -- local links shouldn't match anything, but to be safe, we'll check anyway.
 | otherwise = case urlLive u of
                 Just True -> aL x
                 _         -> x
 where aL :: Inline -> Inline
       aL = addClass "link-live"
linkLive x = x

-- hardwire URLs which should be live
overrideLinkLive :: [T.Text]
overrideLinkLive = []

-- Nothing = unknown/untested; Just True = known good; Just False = known bad
urlLive :: T.Text -> Maybe Bool
urlLive u | u'                    `elem`    goodDomainsSimple = Just True
          | any (\d -> d `T.isSuffixOf` u') goodDomainsSub    = Just True
          | u'                       `elem` badDomainsSimple  = Just False
          | any (\d -> d `T.isSuffixOf` u') badDomainsSub     = Just False
          | otherwise                                         = Nothing
   where u' = host u

-- b <- LinkBacklink.readBacklinksDB
-- sort $ Utils.frequency $ map LinkIcon.host $ filter (Data.Maybe.isNothing . LinkLive.urlLive) $ filter ("."`Data.Text.isInfixOf`) $ Data.Map.keys b

goodDomainsSub, goodDomainsSimple, badDomainsSub, badDomainsSimple :: [T.Text]
goodDomainsSub = [".allennlp.org",
        ".archive.org",
        ".archiveteam.org",
        ".bandcamp.com",
        ".eleuther.ai",
        ".fandom.com",
        ".github.io",
        ".givewell.org",
        ".greenspun.com",
        ".humanprogress.org",
        ".imagemagick.org",
        ".mementoweb.org",
        ".metafilter.com",
        ".nomeata.de",
        ".obormot.net",
        ".tumblr.com",
        ".xkcd.com",
        ".wikipedia.org"]
goodDomainsSimple =
  ["1dollarscan.com",
    "80000hours.org",
    "abandonedfootnotes.blogspot.com",
    "academic.oup.com",
    "academic.oup.com",
    "academictorrents.com",
    "ageing.oxfordjournals.org",
    "ai.googleblog.com",
    "aje.oxfordjournals.org",
    "apenwarr.ca",
    "archive.org",
    "archive.ph",
    "archivebox.io",
    "bam-dataset.org",
    "bam.kalzumeus.com",
    "beepb00p.xyz",
    "bellard.org",
    "blog.beeminder.com",
    "blog.google.com",
    "blog.otoro.net",
    "blog.pinboard.in",
    "blogs.nvidia.com",
    "bmk.sh",
    "boingboing.net",
    "camelcamelcamel.com",
    "cat-unbound.org",
    "causal-effects.com",
    "citeseerx.ist.psu.edu",
    "clinicaltrials.gov",
    "conifer.rhizome.org",
    "cran.r-project.org",
    "ctan.org",
    "danluu.com",
    "danwang.co",
    "distill.pub",
    "docs.google.com",
    "duckduckgo.com",
    "dwarffortresswiki.org",
    "en.bitcoin.it",
    "en.touhouwiki.net",
    "en.wikibooks.org",
    "en.wikichip.org",
    "en.wikifur.com",
    "en.wikiquote.org",
    "en.wikisource.org",
    "en.wiktionary.org",
    "equilibriabook.com",
    "eurekamaga.com",
    "everything2.com",
    "explorabl.es",
    "feeds.feedburner.com",
    "files.eric.ed.gov",
    "forum.effectivealtruism.org",
    "forum.evageeks.org",
    "foundation.wikimedia.org",
    "fullfrontal.moe",
    "greaterwrong.com",
    "guzey.com",
    "idlewords.com",
    "iqcomparisonsite.com",
    "jamanetwork.com",
    "jasoncrawford.org",
    "jtoomim.org",
    "kalzumeus.com",
    "keras.io",
    "kill-the-newsletter.com",
    "kk.org",
    "knightcolumbia.org",
    "ledge-cli.org",
    "lesswrong.com",
    "libgen.rs",
    "library.bz",
    "marginalrevolution.com",
    "mattlakeman.org",
    "mc-stan.org",
    "meta.wikimedia.org",
    "michaelnielsen.org",
    "ncase.me",
    "nintil.com",
    "nitter.hu",
    "norvig.com",
    "notes.pinboard.in",
    "numinous.productions",
    "nymag.com",
    "openai.com",
    "ourworldindata.org",
    "pandoc.org",
    "papers.ssrn.com",
    "parametric.press",
    "patrickcollison.com",
    "pdfs.semanticscholar.org",
    "personalitytest.net",
    "philpapers.org",
    "pinboard.in",
    "plato.stanford.edu",
    "playground.tensorflow.org",
    "popcon.ubuntu.com",
    "press.etc.cmu.edu",
    "progressstudies.school",
    "psychcentral.com",
    "publicdomainreview.org",
    "publishing.cdlib.org",
    "qntm.org",
    "quantum.country",
    "qwantz.com",
    "racket-lang.org",
    "rationality.org",
    "rdiff-backup.net",
    "ricon.dev",
    "rootsofprogress.org",
    "row1.ca",
    "safebooru.org",
    "scholars-stage.org",
    "sciencebasedmedicine.org",
    "sevensecularsermons.org",
    "shiny.app",
    "sifter.org",
    "slatestarcodex.com",
    "spreadsheets.google.com",
    "statmodeling.stat.columbia.edu",
    "stats.grok.se",
    "text.npr.org",
    "thefirstaibook.com",
    "thisanimedoesnotexist.ai",
    "thiscatdoesnotexist.com",
    "thisrentaldoesnotexist.com",
    "training.kalzumeus.com",
    "unsongbook.com",
    "upload.wikimedia.org",
    "vast.ai",
    "videolectures.net",
    "wayback.archive-it.org",
    "web.archive.org",
    "wiki.evageeks.org",
    "wiki.haskell.org",
    "www.aleph.se",
    "www.antipope.org",
    "www.archive-it.org",
    "www.barnesandnoble.com",
    "www.buzzricksons.jp",
    "www.cdc.gov",
    "www.cogmed.com",
    "www.cogtest.com",
    "www.cram.com",
    "www.cryonicscalculator.com",
    "www.cylab.cmu.edu",
    "www.dafont.com",
    "www.davidsongifted.org",
    "www.deeplearningbook.org",
    "www.discoverbooks.com",
    "www.doc88.com",
    "www.drmaciver.com",
    "www.e-codices.unifr.ch",
    "www.ecologyandsociety.org",
    "www.econlib.org",
    "www.economist.com",
    "www.econtalk.org",
    "www.equator-network.org",
    "www.equestriadaily.com",
    "www.evamonkey.com",
    "www.filfre.net",
    "www.find-more-books.com",
    "www.frontiersin.org",
    "www.genetics.org",
    "www.gizmodo.com.au",
    "www.gnxp.com",
    "www.google-melange.com",
    "www.greaterwrong.com",
    "www.gutenberg.org",
    "www.haskell.org",
    "www.hpmor.com",
    "www.html-tidy.org",
    "www.iarpa.gov",
    "www.iqtest.com",
    "www.joelonsoftware.com",
    "www.johndcook.com",
    "www.kalzumeus.com",
    "www.lesswrong.com",
    "www.librarything.com",
    "www.mail-archive.com",
    "www.mediawiki.org",
    "www.metafor-project.org",
    "www.motherjones.com",
    "www.ncbi.nlm.nih.gov",
    "www.npr.org",
    "www.ohyouprettythings.com",
    "www.overcomingbias.com",
    "www.poetryfoundation.org",
    "www.proquest.com",
    "www.psychiatryinvestigation.org",
    "www.r-bloggers.com",
    "www.rdocumentation.org",
    "www.ribbonfarm.com",
    "www.rifters.com",
    "www.sapa-project.org",
    "www.schneier.com",
    "www.sciencedirect.com",
    "www.sciencenews.org",
    "www.sciencenewsline.com",
    "www.shawwn.com",
    "www.simplify.so",
    "www.snpedia.com",
    "www.stat.columbia.edu",
    "www.stat.columbia.edu",
    "www.straighttalkonevidence.org",
    "www.tarsnap.com",
    "www.theatlantic.com",
    "www.theindiaforum.in",
    "www.theparisreview.org",
    "www.thisfursonadoesnotexist.com",
    "www.thispersondoesnotexist.com",
    "www.thiswaifudoesnotexist.net",
    "www.thisworddoesnotexist.com",
    "www.thriftbooks.com",
    "www.urbandictionary.com",
    "www.vanityfair.com",
    "www.vocativ.com",
    "www.w3.org",
    "www.washingtonpost.com",
    "www.whichfaceisreal.com",
    "www.winehq.org",
    "www.wolfewiki.com",
    "www.wsj.com",
    "www.yalelawjournal.org",
    "www.youtube.com",
    "xkcd.com",
    "xtools.wmflabs.org",
    "mail.haskell.org",
    "hackage.haskell.org"
    , ("online.wsj.com")
    , ("www.microsoft.com")
    , ("intelligence.org")
    , ("eprint.iacr.org")
    , ("www.explainxkcd.com")
    , ("www.silverhandmeadery.com")
    , ("www.nickbostrom.com")
    , ("well.blogs.nytimes.com")
    , ("www.gwern.net")
    , ("rjlipton.wordpress.com")
    , ("jaspervdj.be")
    , ("jama.jamanetwork.com")
    , ("blog.codinghorror.com")
    , ("aiimpacts.org")
    , ("web.archive.org")
    , ("www.fhi.ox.ac.uk")
    , ("www.cjas.org")
    , ("blog.google")
    , ("archinte.jamanetwork.com")
    , ("aclanthology.org")
    , ("www.clinicaltrials.gov")
    , ("proceedings.mlr.press")
    , ("diff.wikimedia.org")
    , ("www.scottaaronson.com")
    , ("www.eugenewei.com")
    , ("www.alignmentforum.org")
    ]

badDomainsSub = [".plos.org", ".royalsocietypublishing.org",  ".substack.com", ".stackexchange.com", ".oxfordjournals.org"]
badDomainsSimple = ["1d4chan.org",
   "abebooks.com",
   "academia.edu",
   "ai.facebook.com",
   "ajcn.nutrition.org",
   "anidb.net",
   "ankiweb.net",
   "annals.org",
   "antilop.cc",
   "app.inferkit.com",
   "archive.foolz.us",
   "archive.is",
   "archive.recapthelaw.org",
   "archpsyc.ama-assn.org",
   "arstechnica.com",
   "artbreeder.com",
   "arxiv-vanity.com",
   "arxiv.org",
   "aur.archlinux.org",
   "aurellem.org",
   "babel.hathitrust.org",
   "bakabt.me",
   "betterworldbooks.com",
   "bibliophilly.library.upenn.edu",
   "bigquery.cloud.google.com",
   "biomedcentral.com",
   "bit-player.org",
   "blog.fc2.com",
   "book.realworldhaskell.org",
   "books.google.com",
   "ccc.de",
   "cdm16630.contentdm.oclc.org",
   "ciechanow.ski",
   "clickotron.com",
   "colab.research.google.com",
   "creativecommons.org",
   "cryptome.org",
   "danbooru.donmai.us",
   "darkdata.bc.edu",
   "darknetlive.com",
   "darwin-online.org.uk",
   "de1.erowid.org",
   "deepmind.com",
   "derpibooru.org",
   "dev.kanotype.net",
   "developer.mozilla.org",
   "discord.com",
   "drive.google.com",
   "dspace.mit.edu",
   "duolingo.com",
   "ectoranoana.jp",
   "elifesciences.org",
   "emacswiki.org",
   "eric.ed.gov",
   "erowid.org",
   "eva.onegeek.org",
   "examine.com",
   "f1000research.com",
   "fifteen.ai",
   "fightaging.org",
   "fis.fda.gov",
   "flatisjustice.moe",
   "folding.stanford.edu",
   "folkrnn.org",
   "fred.stlouisfed.org",
   "gallica.bnf.fr",
   "getlamp.com",
   "gitcoin.co",
   "github.com",
   "gitlab.com",
   "gmane.org",
   "goo.gl",
   "goproblems.com",
   "gptprompts.wikidot.com",
   "groups.google.com",
   "gwern.shinyapps.io",
   "halshs.archives-ouvertes.fr",
   "haveibeenpwned.com",
   "hn.algolia.com",
   "httparchive.org",
   "huggingface.co",
   "hutter1.net",
   "imgur.com",
   "incompleteideas.net",
   "inklingmarkets.com",
   "iqout.com",
   "iqtest.dk",
   "isfdb.org",
   "jacurutu.com",
   "journals.sagepub.com",
   "jwz.org",
   "koeln.ccc.de",
   "leaderboard.allenai.org",
   "learnyouahaskell.com",
   "libgen.org",
   "liebertpub.com",
   "link.springer.com",
   "lists.urth.net",
   "listudy.org",
   "longbets.org",
   "longreads.com",
   "lwn.net",
   "make.girls.moe",
   "mathoverflow.net",
   "mayoclinicproceedings.org",
   "media.springernature.com",
   "medium.com",
   "mega.nz",
   "meltingasphalt.com",
   "millionshort.com",
   "mnemosyne-proj.org",
   "mru.org",
   "myanimelist.net",
   "nearlyfreespeech.net",
   "neuralnetworksanddeeplearning.com",
   "newegg.com",
   "nicovideo.jp",
   "nixnote.org",
   "oglaf.com",
   "old.reddit.com",
   "onlinelibrary.wiley.com",
   "openreview.net",
   "orbis.stanford.edu",
   "osf.io",
   "paperswithcode.com",
   "patch.com",
   "pcdb.santafe.edu",
   "pcpartpicker",
   "peerj.com",
   "perma.cc",
   "philanthropy.com",
   "philarchive.org",
   "physicstoday.scitation.org",
   "play.aidungeon.io",
   "plos",
   "pluralsight.com",
   "popcon.debian.org",
   "practicaltypography.com",
   "predictionbook.com",
   "programmablesearchengine.google.com",
   "projecteuclid.org",
   "proofofexistence.com",
   "psyarxiv.com",
   "publicsearch.ndcourts.gov",
   "r-inla.org",
   "readonlymemory.vg",
   "rpubs.com",
   "scholar.google.com",
   "scienceblogs.com",
   "scp-wiki.wikidot.com",
   "serendipityrecs.com",
   "sethroberts.net",
   "silkroadvb5piz3r.onion",
   "slashdot.org",
   "slate.com",
   "snopes.com",
   "soundcloud.com",
   "sourceforge.net",
   "sparkfun.com",
   "stackexchange.com",
   "stackoverflow.com",
   "tasvideos.org",
   "thecleverest.com",
   "thegradient.pub",
   "thepiratebay.org",
   "thesecatsdonotexist.com",
   "thessgac.org",
   "thiscardoesnotexist",
   "thismarketingblogdoesnotexist.com",
   "thisponydoesnotexist.net",
   "thisstorydoesnotexist.com",
   "thisvesseldoesnotexist.com",
   "tineye.com",
   "tinyletter.com",
   "tl.net",
   "tom7.org",
   "tomshardware.com",
   "torservers.net",
   "translate.google.com",
   "treasurydirect.gov",
   "trends.google.com",
   "tryhaskell.org",
   "tvtropes.org",
   "unesdoc.unesco.org",
   "urth.net",
   "uscourts.gov",
   "usesthis.com",
   "vimeo.com",
   "vision-explorer.allenai.org",
   "vizhub.healthdata.org",
   "waifu2x.udp.jp",
   "waifulabs.com",
   "websitedownloader.io",
   "wellcomecollection.org",
   "wikiwix.com",
   "wolframlpha.com",
   "worrydream.com",
   "www.alexa.com",
   "www.animenewsnetwork.com",
   "www.bbc.co.uk",
   "www.bbc.com",
   "www.biorxiv.org",
   "www.blog.sethroberts.net",
   "www.bloomberg.com",
   "www.bls.gov",
   "www.bmj.com",
   "www.catb.org",
   "www.edge.org",
   "www.erowid.org",
   "www.fanfiction.net",
   "www.goodreads.com",
   "www.harney.com",
   "www.imdb.com",
   "www.impactcybertrust.org",
   "www.jneurosci.org",
   "www.jstatsoft.org",
   "www.kaggle.com",
   "www.kickstarter.com",
   "www.mdpi.com",
   "www.medrxiv.org",
   "www.metaculus.com",
   "www.moma.org",
   "www.nature.com",
   "www.nejm.org",
   "www.newyorker.com",
   "www.nytimes.com",
   "www.patreon.com",
   "www.paulgraham.com",
   "www.pnas.org",
   "www.projectrho.com",
   "www.quora.com",
   "www.rand.org",
   "www.researchgate.net",
   "www.rocketpunk-manifesto.com",
   "www.scholarpedia.org",
   "www.sciencedaily.com",
   "www.scientificamerican.com",
   "www.sethroberts.net",
   "www.smbc-comics.com",
   "www.smithsonianmag.com",
   "www.ted.com",
   "www.tensorflow.org",
   "www.theguardian.com",
   "www.theverge.com",
   "www.timeanddate.com",
   "www.uptontea.com",
   "www.urth.net",
   "www.vice.com",
   "www.vox.com",
   "www.wunderground.com",
   "thehub7dnl5nmcz5.onion",
   "silkroad5v7dywlc.onion",
   "lacbzxobeprssrfx.onion"
   , "www.amazon.com"
   , "hoogle.haskell.org"
   , "www.science.org"
   , "www.nber.org"
   , "addons.mozilla.org"
   , "www.discovermagazine.com"
   , ("motherboard.vice.com")
   , ("pubmed.ncbi.nlm.nih.gov")
   , ("www.newsweek.com")
   , ("www.tandfonline.com")
   , ("www.usenix.org")
   , ("boardgamegeek.com")
   , ("www.openphilanthropy.org")
   , ("www.psychologytoday.com")
   , ("news.bbc.co.uk")
   , ("www.facebook.com")
   , ("chronopause.com")
   , ("gist.github.com")
   , ("www.atlasobscura.com")
   , ("if50.substack.com")
   , ("escholarship.org")
   , ("johakyu.net")
   , ("knowyourmeme.com")
   , ("gizmodo.com")
   , ("aws.amazon.com")
   , ("www.courtlistener.com")
   , ("www.dtic.mil")
   , ("www.teanobi.com")
   , ("static-content.springer.com")
   , ("developer.nvidia.com")
   , ("homepage3.nifty.com")
   , ("i.imgur.com")
   , ("www.jstor.org")
   , ("www.berkshirehathaway.com")
   , ("www.buzzfeed.com")
   , ("new.cognitivefun.net")
   , ("intrade.com")
   , ("gitlab.haskell.org")
   , ("ascii.textfiles.com")
   , ("www.rte.ie")
   , ("www.jstatsoft.org")
   , ("www.indiana.edu")
   , ("www.fimfiction.net")
   , ("www.ex.org")
   ]

url :: T.Text -> Inline
url u = linkLive (Link nullAttr [] (u,""))

-- URLs which fail their test:
linkLiveTest :: [(T.Text,Bool)]
linkLiveTest = filter (\(u, bool) -> bool /=
                                       (url u == Link ("",["link-live"], []) [] (u,""))
                      )
               linkLiveTestUnits

linkLiveTestUnits, goodLinks, badLinks :: [(T.Text,Bool)]
linkLiveTestUnits = goodLinks ++ badLinks
goodLinks = [("https://demo.allennlp.org/next-token-lm", True)
    , ("http://arbtt.nomeata.de/", True)
    , ("http://dwarffortresswiki.org/index.php/User:BaronW#The_Almighty_Dwarven_Calculator", True)
    , ("http://feeds.feedburner.com/longbets", True)
    , ("http://libgen.rs/scimag/", True)
    , ("http://norvig.com/norvigs-law.html", True)
    , ("http://philip.greenspun.com/teaching/teaching-software-engineering", True)
    , ("http://stats.grok.se/en/201109/Talk%3ABarack_Obama", True)
    , ("http://timetravel.mementoweb.org/", True)
    , ("http://videolectures.net/rldm2015_silver_reinforcement_learning/", True)
    , ("http://www.aleph.se/andart/archives/2012/09/flaws_in_the_perfection.html", True)
    , ("http://www.antipope.org/charlie/blog-static/2011/08/usenix-2011-keynote-network-se.html", True)
    , ("http://www.johndcook.com/blog/2011/11/22/norris-number/", True)
    , ("http://www.metafor-project.org/doku.php", True)
    , ("http://www.tarsnap.com/scrypt.html", True)
    , ("https://1dollarscan.com/", True)
    , ("https://80000hours.org/podcast/episodes/brian-christian-algorithms-to-live-by/", True)
    , ("https://abandonedfootnotes.blogspot.com/2011/04/qaddafis-chickens.html", True)
    , ("https://academic.oup.com/endo/article/160/5/1057/5381910", True)
    , ("https://academic.oup.com/ije/article/50/5/1615/6274255", True)
    , ("https://academictorrents.com/details/0d366035664fdf51cfbe9f733953ba325776e667/tech", True)
    , ("https://ageing.oxfordjournals.org/content/37/1/25.long", True)
    , ("https://ai.googleblog.com/2021/04/evolving-reinforcement-learning.html", True)
    , ("https://aje.oxfordjournals.org/content/156/11/985.full", True)
    , ("https://apenwarr.ca/log/?m=201707#04", True)
    , ("https://archive.ph/VY8e2", True)
    , ("https://archivebox.io/", True)
    , ("https://bam-dataset.org/", True)
    , ("https://bam.kalzumeus.com/archive/financial-innovation-is-happening/", True)
    , ("https://bam.kalzumeus.com/archive/financial-innovation-is-happening/", True)
    , ("https://beepb00p.xyz/pkm-search.html#appendix_cloudmacs", True)
    , ("https://bellard.org/jslinux/tech.html", True)
    , ("https://blog.beeminder.com/hieroglyphs/", True)
    , ("https://blog.eleuther.ai/factored-cognition/", True)
    , ("https://blog.otoro.net/2017/11/12/evolving-stable-strategies/", True)
    , ("https://blogs.nvidia.com/blog/2019/03/18/gaugan-photorealistic-landscapes-nvidia-research/", True)
    , ("https://bmk.sh/2020/08/17/Building-AGI-Using-Language-Models/", True)
    , ("https://boingboing.net/2012/08/09/make-yourself-healthy-searchi.html", True)
    , ("https://camelcamelcamel.com/", True)
    , ("https://clinicaltrials.gov/ct2/show/NCT03548935", True)
    , ("https://conifer.rhizome.org/", True)
    , ("https://ctan.org/pkg/yinit?lang=en", True)
    , ("https://danluu.com/input-lag/", True)
    , ("https://danwang.co/college-girardian-terror/", True)
    , ("https://demos.obormot.net/these-waifus-do-not-exist-v2-alt", True)
    , ("https://distill.pub/2017/aia/", True)
    , ("https://docs.google.com/spreadsheet/ccc?key=0Ao3RpZe396VZdE5udDB0S1dqcFpPY0o2Sk5LVkNvX0E#gid=0", True)
    , ("https://duckduckgo.com/bang#bangs-list", True)
    , ("https://en.bitcoin.it/wiki/Proof_of_Stake", True)
    , ("https://en.touhouwiki.net/wiki/%E5%87%8B%E5%8F%B6%E6%A3%95", True)
    , ("https://en.wikibooks.org/wiki/Category:Book:Accountancy", True)
    , ("https://en.wikichip.org/wiki/intel/core_i9/i9-7900x", True)
    , ("https://en.wikifur.com/wiki/History", True)
    , ("https://en.wikiquote.org/wiki/Tao_Te_Ching", True)
    , ("https://en.wikisource.org/wiki/Meditation_XVII", True)
    , ("https://en.wiktionary.org/wiki/steward#Etymology", True)
    , ("https://enki2.tumblr.com/stories", True)
    , ("https://equilibriabook.com/molochs-toolbox/", True)
    , ("https://explorabl.es/", True)
    , ("https://files.givewell.org/files/conversations/Stanley%20Young%20slides%20on%20multiple%20testing.pdf", True)
    , ("https://forum.evageeks.org/post/500631/Who-Can-Be-The-Seele-Children/#500631", True)
    , ("https://foundation.wikimedia.org/wiki/File:UK_BOARD_MEETING.pdf", True)
    , ("https://fullfrontal.moe/interview-mahiro-maeda/", True)
    , ("https://guzey.com/books/why-we-sleep/#no-shorter-sleep-does-not-imply-shorter-life-span", True)
    , ("https://idlewords.com/2010/03/scott_and_scurvy.htm", True)
    , ("https://iqcomparisonsite.com/", True)
    , ("https://it.wikipedia.org/wiki/Gualtiero_Cannarsi", True)
    , ("https://jamanetwork.com/journals/jamapsychiatry/fullarticle/2569454", True)
    , ("https://jasoncrawford.org/", True)
    , ("https://keras.io/", True)
    , ("https://kill-the-newsletter.com/", True)
    , ("https://kk.org/thetechnium/the-shirky-prin/", True)
    , ("https://knightcolumbia.org/content/the-case-for-digital-public-infrastructure", True)
    , ("https://legacy.imagemagick.org/Usage/crop/#crop", True)
    , ("https://library.bz/main/upload/", True)
    , ("https://linyanghe.github.io/publications/files/Imagined_Speech_Decoding.pdf", True)
    , ("https://marginalrevolution.com/marginalrevolution/2013/04/trade-vs-technology-in-terms-of-their-labor-market-effects.html", True)
    , ("https://mattlakeman.org/2020/01/22/the-new-epidemic-my-experience-of-losing-a-friend-to-heroin/", True)
    , ("https://mc-stan.org/", True)
    , ("https://meta.wikimedia.org/wiki/Research:Wiki_Participation_Challenge#Dissemination", True)
    , ("https://michaelnielsen.org/blog/three-myths-about-scientific-peer-review/", True)
    , ("https://mlp.fandom.com/wiki/The_Perfect_Pear", True)
    , ("https://ncase.me/", True)
    , ("https://nintil.com/science-ending-frontier", True)
    , ("https://nitter.hu/advadnoun/status/1458894698974638111", True)
    , ("https://nitter.hu/search?f=tweets&q=http%3A%2F%2Fwww.thiswaifudoesnotexist.net&src=typd", True)
    , ("https://notes.pinboard.in/u:vitorio/05dec9f04909d9b6edff", True)
    , ("https://numinous.productions/ttft/", True)
    , ("https://nymag.com/news/features/70830/#print", True)
    , ("https://openai.com/blog/learning-to-summarize-with-human-feedback/", True)
    , ("https://ourworldindata.org/happiness-and-life-satisfaction", True)
    , ("https://pandoc.org/", True)
    , ("https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3819317", True)
    , ("https://parametric.press/issue-01/unraveling-the-jpeg/", True)
    , ("https://personalitytest.net/ipip/index.html", True)
    , ("https://philpapers.org/browse/the-reflection-principle", True)
    , ("https://plato.stanford.edu/entries/moral-anti-realism/index.html", True)
    , ("https://playground.tensorflow.org/", True)
    , ("https://poniesatdawn.bandcamp.com/track/peace-at-last", True)
    , ("https://popcon.ubuntu.com/", True)
    , ("https://press.etc.cmu.edu/index.php/product/well-played-vol-4-no-1/", True)
    , ("https://progressstudies.school/", True)
    , ("https://psychcentral.com/pro/sparlon-and-adhd-the-power-of-a-7-year-old/002889.html", True)
    , ("https://publicdomainreview.org/essay/emma-willard-maps-of-time/", True)
    , ("https://publishing.cdlib.org/ucpressebooks/view?docId=ft8489p25j&chunk.id=d0e2683&toc.id=d0e2575&brand=eschol", True)
    , ("https://qntm.org/invisibility", True)
    , ("https://quantum.country/qcvc", True)
    , ("https://qwantz.com/index.php?comic=2366", True)
    , ("https://racket-lang.org/", True)
    , ("https://rationality.org/", True)
    , ("https://rdiff-backup.net/", True)
    , ("https://ricon.dev/", True)
    , ("https://rootsofprogress.org/nuclear-physics", True)
    , ("https://row1.ca/pixels-and-their-neighbors", True)
    , ("https://safebooru.org/index.php?page=post&s=list&tags=heterochromia", True)
    , ("https://scholars-stage.org/reflections-on-chinas-stalinist-heritage-ii-just-how-totalitarian-is-modern-china/", True)
    , ("https://sciencebasedmedicine.org/antioxidants-and-exercise-more-harm-than-good/", True)
    , ("https://sevensecularsermons.org/about/", True)
    , ("https://sifter.org/iqtest/", True)
    , ("https://slatestarcodex.com/2015/09/23/vegetarianism-for-meat-eaters/", True)
    , ("https://spreadsheets.google.com/ccc?key=rOfijmsJ-hxPbzmbi4dmHVg", True)
    , ("https://statmodeling.stat.columbia.edu/2014/08/28/publication-bias-social-sciences-unlocking-file-drawer2/", True)
    , ("https://text.npr.org/974534021", True)
    , ("https://thefirstaibook.com/#post-inner", True)
    , ("https://thisanimedoesnotexist.ai/", True)
    , ("https://thiscatdoesnotexist.com/", True)
    , ("https://thisrentaldoesnotexist.com/", True)
    , ("https://training.kalzumeus.com/newsletters/archive/saas_pricing", True)
    , ("https://unsongbook.com/chapter-4-tools-were-made-and-born-were-hands/", True)
    , ("https://vast.ai/", True)
    , ("https://web.archive.org/web/20150211211107/https://support.google.com/news/answer/1638638", True)
    , ("https://web.archive.org/web/20170721094633/http://1731298478.tumblr.com/post/52689158259/sadamoto-i-first-met-him-when-i-worked", True)
    , ("https://web.archive.org/web/20190415123208/https://thiscardoesnotexist.glitch.me/", True)
    , ("https://what-if.xkcd.com/76/", True)
    , ("https://wiki.archiveteam.org/index.php/Google_Reader", True)
    , ("https://wiki.evageeks.org/Episode_26%27#.22Last_B.22", True)
    , ("https://wiki.haskell.org/index.php?title=&search=&fulltext=Search", True)
    , ("https://www.archive-it.org/", True)
    , ("https://www.barnesandnoble.com/", True)
    , ("https://www.buzzricksons.jp/", True)
    , ("https://www.cdc.gov/trendstatement/", True)
    , ("https://www.cogmed.com/", True)
    , ("https://www.cogtest.com/coglib_demtest.html", True)
    , ("https://www.cram.com/", True)
    , ("https://www.cryonicscalculator.com/", True)
    , ("https://www.cylab.cmu.edu/", True)
    , ("https://www.dafont.com/deutsche-zierschrif.font", True)
    , ("https://www.davidsongifted.org/Search-Database/entry/A10489", True)
    , ("https://www.deeplearningbook.org/contents/rnn.html", True)
    , ("https://www.discoverbooks.com/", True)
    , ("https://www.doc88.com/p-397166703921.html", True)
    , ("https://www.drmaciver.com/2019/05/how-to-do-hard-things/", True)
    , ("https://www.e-codices.unifr.ch/en/vad/0296/093v", True)
    , ("https://www.ecologyandsociety.org/vol9/iss1/art6/main.html", True)
    , ("https://www.econlib.org/archives/2012/03/the_roots_of_le.html", True)
    , ("https://www.economist.com/science-and-technology/2009/04/08/wired?story_id=13437729", True)
    , ("https://www.econtalk.org/matt-ridley-on-how-innovation-works/", True)
    , ("https://www.equator-network.org/reporting-guidelines/", True)
    , ("https://www.equestriadaily.com/search/label/Music", True)
    , ("https://www.evamonkey.com/ask-john/has-evangelion-influenced-contemporary-gundam-anime.php", True)
    , ("https://everything2.com/title/A+crow+shook+down+on+me", True)
    , ("https://www.filfre.net/2020/01/master-of-orion/", True)
    , ("https://www.find-more-books.com/", True)
    , ("https://www.frontiersin.org/articles/10.3389/fendo.2019.00845/full", True)
    , ("https://www.genetics.org/content/genetics/144/1/205.full.pdf", True)
    , ("https://www.gizmodo.com.au/2020/05/the-internet-furry-drama-raising-big-questions-about-artificial-intelligence/", True)
    , ("https://www.gnxp.com/WordPress/2017/12/12/most-people-say-they-think-nurture-is-more-important-than-nature-especially-white-americans/", True)
    , ("https://www.google-melange.com/gsoc/project/google/gsoc2011/refold/31001", True)
    , ("https://www.gutenberg.org/files/5978/5978-h/5978-h.htm#c15", True)
    , ("https://www.haskell.org/cabal/", True)
    , ("https://www.hpmor.com/chapter/64", True)
    , ("https://www.html-tidy.org/", True)
    , ("https://www.humanprogress.org/", True)
    , ("https://www.iarpa.gov/index.php/research-programs/ace", True)
    , ("https://www.iqtest.com/", True)
    , ("https://www.joelonsoftware.com/2000/11/22/20001122/", True)
    , ("https://www.kalzumeus.com/2012/08/13/doubling-saas-revenue/", True)
    , ("https://www.lesswrong.com/posts/GytPrQ9cT46k9etoz/living-forever-is-hard-or-the-gompertz-curve", True)
    , ("https://www.lesswrong.com/posts/wTKjRFeSjKLDSWyww/possible-takeaways-from-the-coronavirus-pandemic-for-slow-ai", True)
    , ("https://www.librarything.com/work/13068", True)
    , ("https://www.mail-archive.com/cryptography@metzdowd.com/msg09975.html", True)
    , ("https://www.mediawiki.org/wiki/Multilingual_MediaWiki", True)
    , ("https://www.metafilter.com/91797/working-working-memory-with-dual-nback#3108845", True)
    , ("https://www.motherjones.com/politics/2016/06/cca-private-prisons-corrections-corporation-inmates-investigation-bauer/", True)
    , ("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4959137/", True)
    , ("https://www.npr.org/sections/alltechconsidered/2017/06/08/531796329/eager-to-burst-his-own-bubble-a-techie-made-apps-to-randomize-his-life", True)
    , ("https://www.ohyouprettythings.com/free", True)
    , ("https://www.overcomingbias.com/2014/10/why-not-egg-futures.html", True)
    , ("https://www.poetryfoundation.org/poems/45173/jubilate-agno", True)
    , ("https://www.proquest.com/docview/305288545", True)
    , ("https://www.psychiatryinvestigation.org/journal/view.php?number=865", True)
    , ("https://www.r-bloggers.com/2014/01/visualization-series-using-scatterplots-and-models-to-understand-the-diamond-market-so-you-dont-get-ripped-off/", True)
    , ("https://www.rdocumentation.org/packages/EnvStats/versions/2.1.0/topics/evNormOrdStats", True)
    , ("https://www.rifters.com/crawl/?p=791", True)
    , ("https://www.sapa-project.org/", True)
    , ("https://www.schneier.com/blog/archives/2008/10/the_seven_habit.html", True)
    , ("https://www.sciencedirect.com/science/article/pii/S0191886921003536", True)
    , ("https://www.sciencenews.org/view/generic/id/65600/title/Fish_oil_fails_to_hold_off_heart_arrhythmia", True)
    , ("https://www.sciencenewsline.com/medicine/2012010922310082.html", True)
    , ("https://www.shawwn.com/swarm", True)
    , ("https://www.simplify.so/", True)
    , ("https://www.snpedia.com/index.php/Genomes", True)
    , ("https://www.straighttalkonevidence.org/2017/09/22/disappointing-findings-on-conditional-cash-transfers-as-a-tool-to-break-the-poverty-cycle-in-the-united-states/", True)
    , ("https://www.theatlantic.com/technology/archive/2012/05/the-perfect-milk-machine-how-big-data-transformed-the-dairy-industry/256423/", True)
    , ("https://www.theindiaforum.in/article/revolt-upper-castes", True)
    , ("https://www.theparisreview.org/interviews/4155/the-art-of-the-essay-no-1-e-b-white", True)
    , ("https://www.thisfursonadoesnotexist.com/", True)
    , ("https://www.thispersondoesnotexist.com/", True)
    , ("https://www.thisworddoesnotexist.com/", True)
    , ("https://www.thriftbooks.com/", True)
    , ("https://www.urbandictionary.com/define.php?term=Death%20flag", True)
    , ("https://www.vanityfair.com/culture/2010/02/exile-201002", True)
    , ("https://www.vocativ.com/interactive/underworld/drugs/darknet-arrests-map/", True)
    , ("https://www.w3.org/International/wiki/Case_folding", True)
    , ("https://www.washingtonpost.com/news/worldviews/wp/2014/12/17/denmark-stakes-its-claim-in-the-war-for-the-north-pole/", True)
    , ("https://www.whichfaceisreal.com/", True)
    , ("https://www.winehq.org/pipermail/wine-devel/2002-February/003912.html", True)
    , ("https://www.wolfewiki.com/pmwiki/pmwiki.php?n=WolfeWiki.Introduction", True)
    , ("https://www.wsj.com/articles/SB10001424052702304432304576371462612272884", True)
    , ("https://www.yalelawjournal.org/note/amazons-antitrust-paradox", True)
    , ("https://xkcd.com/481/", True)
    , ("https://xtools.wmflabs.org/pages/index.php?name=Rhwawn&lang=en&wiki=wikipedia&namespace=0&redirects=noredirects", True)
    , ("https://cran.r-project.org/web/packages/BradleyTerry2/index.html", True)
    , ("https://files.eric.ed.gov/fulltext/EJ746292.pdf", True)
    , ("https://mail.haskell.org/pipermail/haskell-cafe/2013-April/107389.html", True)
    , ("https://hackage.haskell.org/package/archiver", True)
    , ("https://online.wsj.com/article/SB10001424053111903480904576512250915629460.html", True)
    , ("https://www.microsoft.com/en-us/research/blog/zero-2-deepspeed-shattering-barriers-of-deep-learning-speed-scale/", True)
    , ("https://intelligence.org/2016/09/12/new-paper-logical-induction/", True)
    , ("https://eprint.iacr.org/2021/1273", True)
    , ("https://www.explainxkcd.com/wiki/index.php/Randall", True)
    , ("https://www.silverhandmeadery.com/portfolio-posts/dream-by-the-fire/", True)
    , ("https://www.nickbostrom.com/astronomical/waste.html", True)
    , ("https://well.blogs.nytimes.com/2009/06/08/worries-about-antioxidant-use-by-breast-cancer-patients/#more-6629", True)
    , ("https://rjlipton.wordpress.com/the-gdel-letter/", True)
    , ("https://jaspervdj.be/files/2011-gsoc-text-utf8-proposal.html", True)
    , ("https://jama.jamanetwork.com/article.aspx?articleid=183580", True)
    , ("https://blog.codinghorror.com/why-cant-programmers-program/", True)
    , ("https://aiimpacts.org/wp-content/uploads/2019/02/image2.png", True)
    , ("https://web.archive.org/web/20170419194138/https://www.princeton.edu/mudd/finding_aids/mathoral/pmcxrota.htm", True)
    , ("https://www.fhi.ox.ac.uk/", True)
    , ("http://www.cjas.org/~leng/anno-ikuhara.txt", True)
    , ("https://blog.google/outreach-initiatives/small-business/google-ads-helping-businesses/", True)
    , ("https://archinte.jamanetwork.com/article.aspx?articleid=414784", True)
    , ("https://aclanthology.org/D15-1002/", True)
    , ("https://www.clinicaltrials.gov/show/NCT02140554", True)
    , ("https://proceedings.mlr.press/v119/huang20f.html", True)
    , ("https://diff.wikimedia.org/2012/03/27/analysis-of-the-quality-of-newcomers-in-wikipedia-over-time/", True)
    , ("https://www.scottaaronson.com/democritus/", True)
    , ("https://www.eugenewei.com/blog/2017/5/11/jpeg-your-ideas", True)
    , ("https://www.alignmentforum.org/posts/Haawpd5rZrzkzvYRC/an-162-foundation-models-a-paradigm-shift-within-ai", True)
    ]

badLinks = [("https://1d4chan.org/wiki/Tale_of_an_Industrious_Rogue,_Part_I", False)
    , ("http://annals.org/article.aspx?articleid=745807", False)
    , ("http://archive.foolz.us/a/thread/77196171/#77207238", False)
    , ("http://archive.recapthelaw.org/paed/205626/", False)
    , ("http://archpsyc.ama-assn.org/cgi/content/abstract/46/1/73", False)
    , ("http://aurellem.org/vba-clojure/html/total-control.html", False)
    , ("http://bakabt.me/159362-umineko-no-naku-koro-ni-music-collection-flac.html", False)
    , ("http://bit-player.org/2021/three-months-in-monte-carlo", False)
    , ("http://boinngerionn.blog.fc2.com/blog-entry-203.html", False)
    , ("http://book.realworldhaskell.org/read/data-structures.html#id637702", False)
    , ("http://clickotron.com/", False)
    , ("http://darkdata.bc.edu/", False)
    , ("http://darwin-online.org.uk/content/frameset?pageseq=1&itemID=F1548.1&viewtype=text", False)
    , ("http://dev.kanotype.net:8003/deepdanbooru/", False)
    , ("http://dspace.mit.edu/handle/1721.1/10589", False)
    , ("http://folding.stanford.edu/English/FAQ-Diseases", False)
    , ("http://gptprompts.wikidot.com/linguistics:word-in-context", False)
    , ("http://incompleteideas.net/sutton/book/the-book.html", False)
    , ("http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids", False)
    , ("http://libgen.org/search.php?req=%22wheel+of+time%22", False)
    , ("http://lists.urth.net/pipermail/urth-urth.net/2010-December/019137.html", False)
    , ("http://neuralnetworksanddeeplearning.com/", False)
    , ("http://pcdb.santafe.edu/index.php", False)
    , ("http://permalink.gmane.org/gmane.science.linguistics.wikipedia.english/110790", False)
    , ("http://prize.hutter1.net/", False)
    , ("http://publicsearch.ndcourts.gov/default.aspx", False)
    , ("http://rstb.royalsocietypublishing.org/content/365/1537/73.full", False)
    , ("http://serendipityrecs.com/", False)
    , ("http://subs2srs.sourceforge.net/", False)
    , ("http://thesecatsdonotexist.com/", False)
    , ("http://thismarketingblogdoesnotexist.com/", False)
    , ("http://tom7.org/mario/", False)
    , ("http://waifu2x.udp.jp/", False)
    , ("http://worrydream.com/LearnableProgramming/", False)
    , ("http://www.blog.sethroberts.net/2011/01/29/the-buttermind-experiment/", False)
    , ("http://www.catb.org/jargon/html/R/religious-issues.html", False)
    , ("http://www.getlamp.com/", False)
    , ("http://www.goproblems.com/", False)
    , ("http://www.iqout.com/", False)
    , ("http://www.iqtest.dk/main.swf", False)
    , ("http://www.isfdb.org/cgi-bin/pl.cgi?261005", False)
    , ("http://www.jacurutu.com/viewtopic.php?f=21&t=1112#p34878", False)
    , ("http://www.nicovideo.jp/watch/sm22678844", False)
    , ("http://www.nixnote.org/", False)
    , ("http://www.paulgraham.com/stuff.html", False)
    , ("http://www.projectrho.com/public_html/rocket/spacegunexotic.php#propulsion", False)
    , ("http://www.rocketpunk-manifesto.com/2009/06/space-warfare-i-gravity-well.html", False)
    , ("http://www.scholarpedia.org/article/N-body_choreographies", False)
    , ("http://www.wikiwix.com/", False)
    , ("http://www.wunderground.com/history/airport/KNHK/2012/7/11/CustomHistory.html?dayend=22&monthend=3&yearend=2013&req_city=NA&req_state=NA&req_statename=NA&format=1", False)
    , ("https://ai.facebook.com/blog/harmful-content-can-evolve-quickly-our-new-ai-system-adapts-to-tackle-it", False)
    , ("https://ajcn.nutrition.org/content/69/5/842.full", False)
    , ("https://almanac.httparchive.org/en/2019/", False)
    , ("https://anidb.net/perl-bin/animedb.pl?show=lexicon&mode=character&vtype=ctag&relid=2296", False)
    , ("https://antilop.cc/sr/vendors/24bb54ca7e.htm", False)
    , ("https://app.inferkit.com/demo", False)
    , ("https://apps.ankiweb.net/docs/manual.html#filtered", False)
    , ("https://archive.is/", False)
    , ("https://arstechnica.com/information-technology/2013/04/the-rise-and-fall-of-amd-how-an-underdog-stuck-it-to-intel/3/", False)
    , ("https://aur.archlinux.org/packages/anki20-bin/", False)
    , ("https://babel.hathitrust.org/cgi/pt?id=mdp.39015039380632&view=1up&seq=333", False)
    , ("https://bibliophilly.library.upenn.edu/viewer.php?id=Ms.%20Codex%201248#page/244/mode/2up", False)
    , ("https://bigquery.cloud.google.com/table/fh-bigquery:reddit_comments.2015_05", False)
    , ("https://bmcvetres.biomedcentral.com/articles/10.1186/s12917-017-0987-6", False)
    , ("https://books.google.com/books?id=Cr251kFBy5QC", False)
    , ("https://cdm16630.contentdm.oclc.org/digital/collection/p16630coll2/id/534", False)
    , ("https://ciechanow.ski/lights-and-shadows/", False)
    , ("https://colab.research.google.com/drive/1LiWxqJJMR5dg4BxwUgighaWp2U_enaFd#offline=true&sandboxMode=true", False)
    , ("https://creativecommons.org/licenses/by-nc/3.0/us/", False)
    , ("https://cryptome.org/2013/10/sadler-white-complaint.pdf", False)
    , ("https://danbooru.donmai.us/wiki_pages/34230", False)
    , ("https://darknetlive.com/arrested-darknet-vendors/", False)
    , ("https://derpibooru.org/tags/artist-colon-thisponydoesnotexist", False)
    , ("https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme", False)
    , ("https://discord.com/invite/7RgdJZy", False)
    , ("https://drive.google.com/file/d/1Ovgl5bIwoIV5xDZvPcmOKUYwKNTaf88P/view", False)
    , ("https://elifesciences.org/articles/66920", False)
    , ("https://examine.com/supplements/Piracetam/", False)
    , ("https://f1000research.com/articles/3-82/v1", False)
    , ("https://fifteen.ai/", False)
    , ("https://fis.fda.gov/sense/app/d10be6bb-494e-4cd2-82e4-0135608ddc13/sheet/45beeb74-30ab-46be-8267-5756582633b4/state/analysis", False)
    , ("https://flatisjustice.moe/TADNE", False)
    , ("https://folkrnn.org/", False)
    , ("https://fred.stlouisfed.org/series/JPNURYNAA", False)
    , ("https://gallica.bnf.fr/ark:/12148/bpt6k851127r/", False)
    , ("https://gitcoin.co/blog/gitcoin-grants-clr-matching/", False)
    , ("https://gitlab.com/cryptsetup/cryptsetup/-/issues/19", False)
    , ("https://goo.gl/eR7fbX", False)
    , ("https://groups.google.com/g/brain-training/c/DdeZzeXEMyE", False)
    , ("https://gwern.shinyapps.io/orderStatisticsIncreasedVariance/", False)
    , ("https://halshs.archives-ouvertes.fr/halshs-02180182/document", False)
    , ("https://haveibeenpwned.com/", False)
    , ("https://hn.algolia.com/?query=%22Long%20Bets%22&sort=byDate&prefix&page=0&dateRange=all&type=all", False)
    , ("https://home.inklingmarkets.com/recent/markets", False)
    , ("https://huggingface.co/calculator/", False)
    , ("https://imgur.com/a/LE80ogv", False)
    , ("https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0201246", False)
    , ("https://journals.sagepub.com/doi/full/10.1177/2515245920919667", False)
    , ("https://koeln.ccc.de/archiv/cyphernomicon/chapter14/14.5.html", False)
    , ("https://leaderboard.allenai.org/break_high_level/submissions/public", False)
    , ("https://link.springer.com/article/10.1007/BF02253535", False)
    , ("https://listudy.org/en", False)
    , ("https://longbets.org/363/", False)
    , ("https://longreads.com/2019/10/22/the-final-five-percent/", False)
    , ("https://lwn.net/Articles/286233/", False)
    , ("https://make.girls.moe/", False)
    , ("https://math.stackexchange.com/questions/349155/how-often-does-it-happen-that-the-oldest-person-alive-dies/388131#388131", False)
    , ("https://mathoverflow.net/questions/879/most-interesting-mathematics-mistake", False)
    , ("https://media.springernature.com/full/springer-static/image/art%3A10.1186%2Fs12917-017-0987-6/MediaObjects/12917_2017_987_Fig7_HTML.gif", False)
    , ("https://meltingasphalt.com/hallucinated-gods/", False)
    , ("https://mnemosyne-proj.org/", False)
    , ("https://mru.org/development-economics", False)
    , ("https://myanimelist.net/mangalist/gwern", False)
    , ("https://old.reddit.com/r/haskell/comments/fid5w/haskell_summers_of_code_retrospective_updated_for/c1gutlo", False)
    , ("https://onlinelibrary.wiley.com/doi/full/10.1111/acel.12880", False)
    , ("https://openreview.net/forum?id=qVyeW-grC2k#google", False)
    , ("https://orbis.stanford.edu/", False)
    , ("https://osf.io/preprints/socarxiv/mbj9p/", False)
    , ("https://paperswithcode.com/task/language-modelling", False)
    , ("https://patch.com/california/davis/davis-pair-arrested-after-cops-intercept-3-000-suspected-ecstasy-pills-mail-serve", False)
    , ("https://pcpartpicker.com/list/nwQnZ8", False)
    , ("https://peerj.com/preprints/27137v1/", False)
    , ("https://perma.cc/", False)
    , ("https://philarchive.org/archive/SOTAOAv1", False)
    , ("https://physicstoday.scitation.org/do/10.1063/PT.6.1.20180822a/full/", False)
    , ("https://play.aidungeon.io/main/home", False)
    , ("https://popcon.debian.org/", False)
    , ("https://practicaltypography.com/web-and-email-addresses.html", False)
    , ("https://predictionbook.com/predictions/3693", False)
    , ("https://programmablesearchengine.google.com/about//cse?cx=009114923999563836576%3A1eorkzz2gp4&q=Firefly+%22Cowboy+Bebop%22", False)
    , ("https://projecteuclid.org/journals/bernoulli/volume-2/issue-4/Exponential-convergence-of-Langevin-distributions-and-their-discrete-approximations/bj/1178291835.full", False)
    , ("https://readonlymemory.vg/shop/book/arcade-game-typography/", False)
    , ("https://rpubs.com/EmilOWK/232493", False)
    , ("https://scholar.google.com/scholar?q=cat%20earwax%20OR%20%22ear%20wax%22%20smell%20OR%20taste%20%2D%22CAT%20scan%22", False)
    , ("https://scienceblogs.com/cognitivedaily/2009/04/16/a-quick-eye-exercise-can-impro", False)
    , ("https://scp-wiki.wikidot.com/scp-988", False)
    , ("https://deepmind.com/blog/agents-imagine-and-plan/", False)
    , ("https://slashdot.org/story/07/11/18/1319201/do-tiny-url-services-weaken-net-architecture", False)
    , ("https://slate.com/articles/life/seed/2001/04/the_rise_of_the_smart_sperm_shopper.single.html", False)
    , ("https://soundcloud.com/leggysalad/girls-afternoon-appointments", False)
    , ("https://stackoverflow.com/questions/504823/has-anyone-actually-implemented-a-fibonacci-heap-efficiently", False)
    , ("https://substack.com/", False)
    , ("https://tasvideos.org/6347S", False)
    , ("https://thecleverest.com/judgefakepeople/main.php?sort=highest", False)
    , ("https://thegradient.pub/understanding-evaluation-metrics-for-language-models/", False)
    , ("https://thepiratebay.org/description.php?id=18368760", False)
    , ("https://thiscardoesnotexist.glitch.me", False)
    , ("https://thisponydoesnotexist.net/model/network-ponies-1024-151552.pkl", False)
    , ("https://thisvesseldoesnotexist.com/", False)
    , ("https://tineye.com/", False)
    , ("https://tl.net/blogs/286351-worker-rush-part-5-live-to-win?view=all", False)
    , ("https://trends.google.com/trends/explore?q=Long%20Bets", False)
    , ("https://tvtropes.org/pmwiki/pmwiki.php/VisualNovel/UminekoWhenTheyCry", False)
    , ("https://unesdoc.unesco.org/ark:/48223/pf0000220391", False)
    , ("https://usesthis.com/", False)
    , ("https://vimeo.com/groups/45234/videos/11912761", False)
    , ("https://vision-explorer.allenai.org/text_to_image_generation", False)
    , ("https://vizhub.healthdata.org/gbd-compare/#settings=0766b73db1d02f22ea6e150ced632f13345caaee", False)
    , ("https://waifulabs.com/blog/ax", False)
    , ("https://websitedownloader.io/", False)
    , ("https://wellcomecollection.org/", False)
    , ("https://www.abebooks.com/", False)
    , ("https://www.academia.edu/24382120", False)
    , ("https://www.alexa.com/siteinfo/mnemosyne-proj.org", False)
    , ("https://www.animenewsnetwork.com/encyclopedia/company.php?id=14634", False)
    , ("https://www.artbreeder.com/", False)
    , ("https://www.arxiv-vanity.com/", False)
    , ("https://www.bbc.com/news/health-35262535", False)
    , ("https://www.betterworldbooks.com/", False)
    , ("https://www.biorxiv.org/content/10.1101/274654v2.full", False)
    , ("https://www.bloomberg.com/view/articles/2017-05-03/why-i-lost-my-bet-with-warren-buffett", False)
    , ("https://www.bls.gov/opub/mlr/2016/article/the-life-of-american-workers-in-1915.htm", False)
    , ("https://www.bmj.com/content/361/bmj.K2270", False)
    , ("https://www.duolingo.com/", False)
    , ("https://www.edge.org/conversation/robert_plomin-why-were-different", False)
    , ("https://www.emacswiki.org/emacs/MarkdownMode", False)
    , ("https://www.erowid.org/", False)
    , ("https://www.fanfiction.net/s/5588986/1/", False)
    , ("https://www.fightaging.org/archives/2018/09/thoughts-on-attending-raadfest-2018-in-san-diego/", False)
    , ("https://www.goodreads.com/interviews/show/21.Malcolm_Gladwell", False)
    , ("https://www.harney.com/products/organic-green-with-citrus-ginkgo", False)
    , ("https://www.imdb.com/name/nm0663050/bio", False)
    , ("https://www.impactcybertrust.org/dataset_view?idDataset=812", False)
    , ("https://www.jneurosci.org/content/jneuro/22/9/3656.full.pdf", False)
    , ("https://www.jstatsoft.org/index.php/jss/article/download/v048i10/602", False)
    , ("https://www.jwz.org/blog/2004/03/when-the-database-worms-eat-into-your-brain/", False)
    , ("https://www.kaggle.com/ultrajack/modern-renaissance-poetry", False)
    , ("https://www.kickstarter.com/projects/upperstory/spintronics-build-mechanical-circuits", False)
    , ("https://www.liebertpub.com/doi/10.1089/hs.2021.0083", False)
    , ("https://www.mayoclinicproceedings.org/article/S0025-6196%2813%2900405-9/fulltext", False)
    , ("https://www.mdpi.com/2073-4425/11/6/648", False)
    , ("https://www.medrxiv.org/content/10.1101/2021.03.29.21253866v2.full", False)
    , ("https://www.metaculus.com/questions/", False)
    , ("https://www.millionshort.com/", False)
    , ("https://www.moma.org/interactives/exhibitions/2012/inventingabstraction/?work=42", False)
    , ("https://www.nature.com/articles/s41366-021-00894-3", False)
    , ("https://www.nearlyfreespeech.net/", False)
    , ("https://www.nejm.org/doi/full/10.1056/NEJMoa1511939", False)
    , ("https://www.newegg.com/acer-aspire-v15-nitro-black-edition-vn7-591g-70rt-gaming-entertainment/p/N82E16834314849", False)
    , ("https://www.newyorker.com/magazine/2017/10/16/russias-house-of-shadows", False)
    , ("https://www.nytimes.com/2014/07/30/world/africa/ransoming-citizens-europe-becomes-al-qaedas-patron.html", False)
    , ("https://www.oglaf.com/glindr/", False)
    , ("https://www.patreon.com/gwern", False)
    , ("https://www.philanthropy.com/article/FinancialLeadership-Woes/138335/", False)
    , ("https://www.pluralsight.com/search?q=R", False)
    , ("https://www.pnas.org/content/111/24/8788.full", False)
    , ("https://www.proofofexistence.com/", False)
    , ("https://www.quora.com/Do-you-have-any-plans-for-optimizing-Duolingos-vocabulary-learning-using-spaced-repetition", False)
    , ("https://www.r-inla.org/", False)
    , ("https://www.rand.org/commentary/2011/05/29/ND.html", False)
    , ("https://www.researchgate.net/profile/James-Swanson-4/publication/7447194_Efficacy_and_Safety_of_Modafinil_Film-Coated_Tablets_in_Children_and_Adolescents_With_Attention-DeficitHyperactivity_Disorder_Results_of_a_Randomized_Double-Blind_Placebo-Controlled_Flexible-Dose_Stud/links/549adef50cf2fedbc30e3798/Efficacy-and-Safety-of-Modafinil-Film-Coated-Tablets-in-Children-and-Adolescents-With-Attention-Deficit-Hyperactivity-Disorder-Results-of-a-Randomized-Double-Blind-Placebo-Controlled-Flexible-Dose.pdf#page=3", False)
    , ("https://www.sciencedaily.com/releases/2008/05/080505075642.htm", False)
    , ("https://www.scientificamerican.com/article/the-mind-of-an-octopus/", False)
    , ("https://www.smbc-comics.com/?id=3169#comic", False)
    , ("https://www.smithsonianmag.com/history/crockfords-club-how-a-fishmonger-built-a-gambling-hall-and-bankrupted-the-british-aristocracy-148268691/", False)
    , ("https://www.snopes.com/fact-check/school-lemonade-laxatives/", False)
    , ("https://www.sparkfun.com/tutorials", False)
    , ("https://www.ted.com/talks/christopher_ryan_are_we_designed_to_be_sexual_omnivores", False)
    , ("https://www.tensorflow.org/tensorboard/get_started", False)
    , ("https://www.theguardian.com/technology/2014/may/30/life-after-silk-road-how-the-darknet-drugs-market-is-booming", False)
    , ("https://www.thessgac.org/data", False)
    , ("https://www.theverge.com/2021/10/28/22750337/shadow-planet-ai-robin-sloan-jesse-solomon-clark", False)
    , ("https://www.thisstorydoesnotexist.com/", False)
    , ("https://www.timeanddate.com/date/durationresult.html?m1=9&d1=16&y1=2012&m2=3&d2=9&y2=2013&ti=on", False)
    , ("https://www.tinyletter.com/", False)
    , ("https://www.tomshardware.com/news/google-removing-minix-management-engine-intel,35876.html", False)
    , ("https://www.torservers.net/donate.html", False)
    , ("https://www.treasurydirect.gov/govt/reports/pd/histdebt/histdebt_histo5.htm", False)
    , ("https://www.tryhaskell.org/", False)
    , ("https://www.uptontea.com/shopcart/item.asp?itemID=ZH80", False)
    , ("https://www.uscourts.gov/services-forms/federal-court-reporting-program", False)
    , ("https://www.vice.com/en/article/gv5x4q/court-docs-show-a-university-helped-fbi-bust-silk-road-2-child-porn-suspects", False)
    , ("https://www.vox.com/xpress/2014/10/2/6875031/chickens-breeding-farming-boilers-giant", False)
    , ("https://psyarxiv.com/kq4mn/", False)
    , ("https://www.amazon.com/Watamote-Complete-Collection-Blu-ray/dp/B00JXBLM72/", False)
    , ("https://hoogle.haskell.org/?hoogle=IO_a_-%3E_IO_ThreadId", False)
    , ("https://www.science.org/content/article/plan-replicate-50-high-impact-cancer-papers-shrinks-just-18", False)
    , ("https://www.nber.org/papers/w13711", False)
    , ("https://addons.mozilla.org/en-US/firefox/addon/lastpass-password-manager/", False)
    , ("https://www.discovermagazine.com/planet-earth/brain-training-games-get-a-d-at-brain-training-tests", False)
    , ("https://ije.oxfordjournals.org/content/30/6/1251.full", False)
    , ("https://motherboard.vice.com/read/this-researcher-is-tallying-arrests-from-dark-web-markets", False)
    , ("https://pubmed.ncbi.nlm.nih.gov/8588288/", False)
    , ("https://www.newsweek.com/i-cloned-my-dog-puppies-have-different-personalities-1674290", False)
    , ("https://www.tandfonline.com/doi/full/10.1080/03949370.2021.1893826", False)
    , ("https://www.usenix.org/legacy/events/sec99/full_papers/whitten/whitten.ps", False)
    , ("https://boardgamegeek.com/boardgame/148931/coup-reformation", False)
    , ("https://www.openphilanthropy.org/focus/global-catastrophic-risks/potential-risks-advanced-artificial-intelligence/what-should-we-learn-past-ai-forecasts", False)
    , ("https://www.psychologytoday.com/us/blog/pristine-inner-experience/201110/not-everyone-conducts-inner-speech", False)
    , ("http://news.bbc.co.uk/2/hi/uk_news/3723839.stm", False)
    , ("https://www.facebook.com/permalink.php?story_fbid=224735391342335&id=100014176268390", False)
    , ("http://chronopause.com/index.php/2011/02/11/thus-spake-curtis-henderson-part-5/", False)
    , ("https://gist.github.com/SigridK/c16ddc7b0f2a5bc01ea23d69569c6c0b", False)
    , ("https://www.atlasobscura.com/articles/what-bread-did-ancient-egyptians-eat", False)
    , ("https://if50.substack.com/p/1999-king-of-dragon-pass", False)
    , ("https://escholarship.org/uc/item/5bv8c7p3", False)
    , ("http://johakyu.net/lib/2007/07/2007-07-27-000535.php", False)
    , ("https://knowyourmeme.com/memes/tendies-stories", False)
    , ("https://gizmodo.com/generation-cryo-fighting-death-in-the-frozen-unknown-1786446378", False)
    , ("https://aws.amazon.com/blogs/opensource/keeping-open-source-open-open-distro-for-elasticsearch/", False)
    , ("https://www.courtlistener.com/docket/4353251/united-states-v-ulbricht/", False)
    , ("http://www.dtic.mil/cgi-bin/GetTRDoc?AD=ADA099503", False)
    , ("http://www.teanobi.com/category_s/228.htm", False)
    , ("https://static-content.springer.com/esm/art%3A10.1186%2Fs12917-017-0987-6/MediaObjects/12917_2017_987_MOESM2_ESM.xlsx", False)
    , ("https://developer.nvidia.com/cuda-downloads", False)
    , ("http://homepage3.nifty.com/mana/miyazaki-annno.html", False)
    , ("https://i.imgur.com/jzZKreU.png", False)
    , ("https://www.jstor.org/stable/10.1086/468061", False)
    , ("https://www.berkshirehathaway.com/letters/letters.html", False)
    , ("https://www.buzzfeed.com/joshdean/are-we-warming-up-to-cryonics", False)
    , ("https://new.cognitivefun.net/", False)
    , ("https://intrade.com/jsp/intrade/common/c_cd.jsp?conDetailID=702407&z=1285870999458", False)
    , ("https://gitlab.haskell.org/ghc/ghc/-/issues/2143", False)
    , ("http://ascii.textfiles.com/archives/1717", False)
    , ("https://www.rte.ie/archives/2018/0322/949314-donegal-victorian-romantics/", False)
    , ("https://www.jstatsoft.org/index.php/jss/article/download/v048i09/601", False)
    , ("https://www.indiana.edu/~pcl/rgoldsto/interrelated/interrelated.html", False)
    , ("https://www.fimfiction.net/story/62074/Friendship-is-Optimal", False)
    , ("http://www.ex.org/2.4/09-jpopconference_1.html", False)
    ]
