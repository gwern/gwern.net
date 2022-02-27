{- LinkLive.hs: Specify domains which can be popped-up "live" in a frame by adding a link class.
Author: Gwern Branwen
Date: 2022-02-26
When:  Time-stamp: "2022-02-26 20:48:14 gwern"
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
module LinkLive (linkLive, linkLiveTest, urlLive, linkLivePrioritize) where

import Data.List (sort)
import Data.Maybe (isNothing)
import Data.Map.Strict as M (keys)
import Data.Text as T (isInfixOf, isPrefixOf, isSuffixOf, Text)
import Text.Pandoc (Inline(Link), nullAttr)

import LinkBacklink (readBacklinksDB)
import Utils (addClass, frequency, host)

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
urlLive u | u'            `elem`    goodDomainsSimple = Just True
          | any (`T.isSuffixOf` u') goodDomainsSub    = Just True
          | u'               `elem` badDomainsSimple  = Just False
          | any (`T.isSuffixOf` u') badDomainsSub     = Just False
          | otherwise                                 = Nothing
   where u' = host u

linkLivePrioritize :: IO [(Int, T.Text)]
linkLivePrioritize = do b <- readBacklinksDB
                        let urls = filter ("."`T.isInfixOf`) $ M.keys b
                        return $ reverse $ sort $ Utils.frequency $ filter (/="") $ map host $ filter (isNothing . urlLive) urls

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
        ".wikipedia.org",
        ".wordpress.com"]
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
    , "online.wsj.com"
    , "www.microsoft.com"
    , "intelligence.org"
    , "eprint.iacr.org"
    , "www.explainxkcd.com"
    , "www.silverhandmeadery.com"
    , "www.nickbostrom.com"
    , "well.blogs.nytimes.com"
    , "www.gwern.net"
    , "rjlipton.wordpress.com"
    , "jaspervdj.be"
    , "jama.jamanetwork.com"
    , "blog.codinghorror.com"
    , "aiimpacts.org"
    , "web.archive.org"
    , "www.fhi.ox.ac.uk"
    , "www.cjas.org"
    , "blog.google"
    , "archinte.jamanetwork.com"
    , "aclanthology.org"
    , "www.clinicaltrials.gov"
    , "proceedings.mlr.press"
    , "diff.wikimedia.org"
    , "www.scottaaronson.com"
    , "www.eugenewei.com"
    , "www.alignmentforum.org"
    , "www.wired.com"
    , "www.evaotaku.com"
    , "www.stuff.co.nz"
    , "texample.net"
    , "www.dailymail.co.uk"
    , "www.memteaimports.com"
    , "boards.fireden.net"
    , "webcitation.org"
    , "www.reuters.com"
    , "mail.haskell.org"
    , "gameprogrammingpatterns.com"
    , "digital.library.unt.edu"
    , "www.salon.com"
    , "www.metopera.org"
    , "dnstats.net"
    , "www.thecut.com"
    , "animekritik.wordpress.com"
    , "www.fadedpage.com"
    , "www.dailydot.com"
    , "www.candyjapan.com"
    , "nautil.us"
    , "emilkirkegaard.dk"
    , "www.independent.co.uk"
    , "www.edwardtufte.com"
    , "www.brookings.edu"
    , "www.blockchain.com"
    , "web.stanford.edu"
    , "mitpress.mit.edu"
    , "freakonomics.com"
    , "forums.animesuki.com"
    , "eli.thegreenplace.net"
    , "www.theregister.com"
    , "www.alcor.org"
    , "the-liliquarium.livejournal.com"
    , "socghop.appspot.com"
    , "people.csail.mit.edu"
    , "findarticles.com"
    , "dataprivacylab.org"
    , "blog.23andme.com"
    , "andrewmayneblog.wordpress.com"
    , "www.thefreelibrary.com"
    , "www.sfgate.com"
    , "www.rrauction.com"
    , "www.merkle.com"
    , "www.links.org"
    , "www.bartleby.com"
    , "wavemotioncannon.com"
    , "www.baltimoresun.com"
    , "unenumerated.blogspot.com"
    , "scottaaronson.blog"
    , "rjlipton.wpcomstaging.com"
    , "mason.gmu.edu"
    , "ilovetypography.com"
    , "humanvarieties.org"
    , "googlesystem.blogspot.com"
    , "www.yudkowsky.net"
    , "arr.am"
    , "www.worldcat.org"
    , "www.supermemo.com"
    , "www.standard.co.uk"
    , "www.orlandosentinel.com"
    , "www.nbcnews.com"
    , "www.mercurynews.com"
    , "www.math.uwaterloo.ca"
    , "www.jefftk.com"
    , "www.gq.com"
    , "www.businessweek.com"
    , "www.austlii.edu.au"
    , "www.aiweirdness.com"
    , "w.atwiki.jp"
    , "vitalik.ca"
    , "unqualified-reservations.blogspot.com"
    , "thegrandnarrative.com"
    , "sre.google"
    , "signalvnoise.com"
    , "shkspr.mobi"
    , "qualiacomputing.com"
    , "penelope.uchicago.edu"
    , "parahumans.wordpress.com"
    , "palladiummag.com"
    , "packdeps.haskellers.com"
    , "ohtori.nu"
    , "my.vanderbilt.edu"
    , "mathworld.wolfram.com"
    , "magenta.tensorflow.org"
    , "infoproc.blogspot.com"
    , "highnoongmt.wordpress.com"
    , "googleprojectzero.blogspot.com"
    , "forum.quantifiedself.com"
    , "foreignpolicy.com"
    , "engineering.fb.com"
    , "cdn.openai.com"
    , "cdn.discordapp.com"
    , "blog.acolyer.org"
    , "web.archive.org"
    , "articles.latimes.com"
    , "alumni.media.mit.edu"
    , "agtb.wordpress.com"
    , "zlkj.in"
    , "www.wakapoetry.net"
    , "www.vetta.org"
    , "www.unz.com"
    , "www.unicode.org"
    , "www.unc.edu"
    , "www.tor.com"
    , "www.tomodachi.de"
    , "www.thestranger.com"
    , "www.the-scientist.com"
    , "www.tabletmag.com"
    , "www.syracuse.com"
    , "www.sun-modalert.com"
    , "www.spiegel.de"
    , "www.sankakucomplex.com"
    , "www.sacbee.com"
    , "www.rwagner.net"
    , "www.richardcarrier.info"
    , "www.rationaloptimist.com"
    , "www.pragmatic.ml"
    , "www.popsci.com"
    , "www.owenstephens.co.uk"
    , "www.nydailynews.com"
    , "www.oregonlive.com"
    , "www.miamiherald.com"
    , "www.lrb.co.uk"
    , "www.livestrong.com"
    , "www.karger.com"
    , "www.japansociety.org"
    , "www.japaninc.com"
    , "www.grandforksherald.com"
    , "www.genealogy.math.ndsu.nodak.edu"
    , "www.gawker.com"
    , "www.fathomevents.com"
    , "www.dartmouth.edu"
    , "www.culhwch.info"
    , "www.cs.virginia.edu"
    , "www.cnn.com"
    , "www.chicagotribune.com"
    , "www.cbsnews.com"
    , "www.bemmu.com"
    , "www.4nrx-uk.md"
    , "warontherocks.com"
    , "venturebeat.com"
    , "time.com"
    , "threadreaderapp.com"
    , "thelastpsychiatrist.com"
    , "taooftea.com"
    , "takimag.com"
    , "synapse.koreamed.org"
    , "stratechery.com"
    , "srconstantin.wordpress.com"
    , "spikejapan.wordpress.com"
    , "soranews24.com"
    , "senrigan.io"
    , "retractionwatch.com"
    , "replicationindex.com"
    , "queue.acm.org"
    , "phys.org"
    , "originstamp.com"
    , "opinionator.blogs.nytimes.com"
    , "okmij.org"
    , "web.archive.org"
    , "web.archive.org"
    , "newcriterion.com"
    , "neurosciencenews.com"
    , "my.pgp-hms.org"
    , "meteuphoric.com"
    , "meehl.umn.edu"
    , "mathshistory.st-andrews.ac.uk"
    , "longtermrisk.org"
    , "jtauber.com"
    , "journal.stuffwithstuff.com"
    , "ideas.repec.org"
    , "harpers.org"
    , "hapgood.us"
    , "googleblog.blogspot.com"
    , "globalguerrillas.typepad.com"
    , "felinegenetics.missouri.edu"
    , "eva-fan.com"
    , "esolangs.org"
    , "eileenormsby.com"
    , "diyhpl.us"
    , "egamebook.com"
    , "donsbot.com"
    , "cs.stanford.edu"
    , "crookedtimber.org"
    , "care.diabetesjournals.org"
    , "caniuse.com"
    , "bldgblog.com"
    , "betabeat.com"
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
   , "motherboard.vice.com"
   , "pubmed.ncbi.nlm.nih.gov"
   , "www.newsweek.com"
   , "www.tandfonline.com"
   , "www.usenix.org"
   , "boardgamegeek.com"
   , "www.openphilanthropy.org"
   , "www.psychologytoday.com"
   , "news.bbc.co.uk"
   , "www.facebook.com"
   , "chronopause.com"
   , "gist.github.com"
   , "www.atlasobscura.com"
   , "if50.substack.com"
   , "escholarship.org"
   , "johakyu.net"
   , "knowyourmeme.com"
   , "gizmodo.com"
   , "aws.amazon.com"
   , "www.courtlistener.com"
   , "www.dtic.mil"
   , "www.teanobi.com"
   , "static-content.springer.com"
   , "developer.nvidia.com"
   , "homepage3.nifty.com"
   , "i.imgur.com"
   , "www.jstor.org"
   , "www.berkshirehathaway.com"
   , "www.buzzfeed.com"
   , "new.cognitivefun.net"
   , "intrade.com"
   , "gitlab.haskell.org"
   , "ascii.textfiles.com"
   , "www.rte.ie"
   , "www.jstatsoft.org"
   , "www.indiana.edu"
   , "www.fimfiction.net"
   , "www.ex.org"
    , "news.ycombinator.com"
    , "www.science.org"
    , "www.longecity.org"
    , "www.cell.com"
    , "yunnansourcing.com"
    , "www.businessinsider.com"
    , "www.forbes.com"
    , "www.deviantart.com"
    , "bitcointalk.org"
    , "www.usagi.org"
    , "www.telegraph.co.uk"
    , "www.cambridge.org"
    , "www.apa.org"
    , "kanzaki.sub.jp"
    , "www.smartpowders.com"
    , "www.fastcompany.com"
    , "www.theage.com.au"
    , "www.fda.gov"
    , "www.odt.co.nz"
    , "www.sequentialtart.com"
    , "spectrum.ieee.org"
    , "www.abc.net.au"
    , "towardsdatascience.com"
    , "qz.com"
    , "techcrunch.com"
    , "astralcodexten.substack.com"
    , "www.cs.utexas.edu"
    , "www.nzherald.co.nz"
    , "www.quantamagazine.org"
    , "www.thedailybeast.com"
    , "www.angelfire.com"
    , "ije.oxfordjournals.org"
    , "jn.nutrition.org"
    , "blog.johantibell.com"
    , "darcs.net"
    , "www.unitedpharmacies-uk.md"
    , "www.replicatedtypo.com"
    , "www.petforums.co.uk"
    , "www.nybooks.com"
    , "www.newscientist.com"
    , "www.mirror.co.uk"
    , "www.jstor.org"
    , "www.instructables.com"
    , "www.buzzfeednews.com"
    , "web.mit.edu"
    , "variety.com"
    , "stats.stackexchange.com"
    , "schizophreniabulletin.oxfordjournals.org"
    , "politicalscience.osu.edu"
    , "hal.archives-ouvertes.fr"
    , "code.google.com"
    , "cardcaptor.moekaku.com"
    , "wyclif.substack.com"
    , "www.thenewatlantis.com"
    , "www.nola.com"
    , "www.nausicaa.net"
    , "www.nap.edu"
    , "www.japantimes.co.jp"
    , "www.huffpost.com"
    , "www.hindawi.com"
    , "www.girlscouts.org"
    , "www.esquire.com"
    , "www.eetimes.com"
    , "www.dcscience.net"
    , "www.chronicle.com"
    , "www.amazon.co.jp"
    , "vndb.org"
    , "vividness.live"
    , "thepharmacyexpress.com"
    , "ro.ecu.edu.au"
    , "repository.upenn.edu"
    , "plus.google.com"
    , "pastebin.com"
    , "omega0.xyz"
    , "nutritionj.biomedcentral.com"
    , "media.nature.com"
    , "latanyasweeney.org"
    , "handbook.cochrane.org"
    , "groups.yahoo.com"
    , "genomebiology.biomedcentral.com"
    , "cloud.google.com"
    , "digitalcommons.unl.edu"
    , "bmcpublichealth.biomedcentral.com"
    , "beta.openai.com"
    , "aeon.co"
    , "ajp.psychiatryonline.org"
    , "www.xinhuanet.com"
    , "www.wolframalpha.com"
    , "www.walmart.com"
    , "www.upi.com"
    , "www.ukbiobank.ac.uk"
    , "www.trixietracker.com"
    , "www.theringer.com"
    , "www.thelancet.com"
    , "www.teavana.com"
    , "www.spiceandtea.com"
    , "www.snpp.com"
    , "www.scotthyoung.com"
    , "www.rightstufanime.com"
    , "www.psychologicalscience.org"
    , "www.optimox.com"
    , "www.nobelprize.org"
    , "www.mayoclinic.org"
    , "www.marxists.org"
    , "www.mania.com"
    , "www.mangauk.com"
    , "www.labone.tech"
    , "www.jameslindlibrary.org"
    , "www.isteve.com"
    , "www.imagesco.com"
    , "www.iherb.com"
    , "www.holidaymead.com"
    , "www.genome.gov"
    , "www.flickr.com"
    , "www.fasebj.org"
    , "www.eric.ed.gov"
    , "www.emcdda.europa.eu"
    , "www.drugs.com"
    , "www.dummy-system.com"
    , "www.cochranelibrary.com"
    , "www.cehd.umn.edu"
    , "www.bulletproof.com"
    , "www.bayesianinvestor.com"
    , "www.avclub.com"
    , "www.animevice.com"
    , "www.animenewsservice.com"
    , "www.anandtech.com"
    , "www.afp.gov.au"
    , "www.adafruit.com"
    , "web-archive-org.translate.goog"
    , "wcfcourier.com"
    , "today.yougov.com"
    , "timesofindia.indiatimes.com"
    , "thenextweb.com"
    , "stroke.ahajournals.org"
    , "support.google.com"
    , "sigbovik.org"
    , "reason.com"
    , "raw.githubusercontent.com"
    , "qa.debian.org"
    , "pubs.acs.org"
    , "patents.google.com"
    , "patch-tag.com"
    , "opensnp.org"
    , "oll.libertyfund.org"
    , "nypost.com"
    , "nmteaco.com"
    , "new.nubrain.com"
    , "medlineplus.gov"
    , "mattsclancy.substack.com"
    , "lostpinesyaupontea.com"
    , "lobste.rs"
    , "lists.wikimedia.org"
    , "ldsc.broadinstitute.org"
    , "itre.cis.upenn.edu"
    , "iopscience.iop.org"
    , "ideas.4brad.com"
    , "hplusmagazine.com"
    , "hbr.org"
    , "habr.com"
    , "fivethirtyeight.com"
    , "gigascience.biomedcentral.com"
    , "eprints.whiterose.ac.uk"
    , "fortune.com"
    , "data.worldbank.org"
    , "crypto.stackexchange.com"
    , "community.haskell.org"
    , "bugs.darcs.net"
    , "bmcneurosci.biomedcentral.com"
    , "blog.sigfpe.com"
    , "blog.darcs.net"
    , "bifunctor.homelinux.net"
    , "betsofbitco.in"
    , "benbest.com"
    , "bayes.wustl.edu"
    , "bactra.org"
    , "nitro.biosci.arizona.edu"
    , "cse.google.com"
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
    , ("https://www.wired.com/story/sleep-no-more-crusade-genetic-killer/", True)
    , ("http://www.evaotaku.com/html/kaibunsho-main.html#Sect1c", True)
    , ("http://www.stuff.co.nz/national/crime/10232509/Doing-time-for-drugs-bought-online", True)
    , ("https://texample.net/tikz/examples/hydrogen-splitting/", True)
    , ("https://www.dailymail.co.uk/health/article-2126761/Bertold-Wiesner-British-scientist-fathered-600-children-donating-sperm-fertility-clinic.html", True)
    , ("https://www.memteaimports.com/tea/bei-dou", True)
    , ("https://boards.fireden.net/ic/thread/3820909/", True)
    , ("https://webcitation.org/5mYown8VS", True)
    , ("https://www.reuters.com/article/2011/11/26/us-vitamind-heartdisease-idUSTRE7AO1UM20111126", True)
    , ("https://mail.haskell.org/pipermail/haskell-cafe/2011-February/089183.html", True)
    , ("https://gameprogrammingpatterns.com/dirty-flag.html", True)
    , ("https://digital.library.unt.edu/explore/partners/TAMS/browse/?q=%22Julian+Stanley%22+OR+%22SMPY%22&sort=date_a&t=fulltext", True)
    , ("https://www.salon.com/2007/04/12/castaneda/", True)
    , ("https://www.metopera.org/season/in-cinemas/2019-20-season/akhnaten-live-in-hd/", True)
    , ("https://dnstats.net/market/Nucleus+Market", True)
    , ("https://www.thecut.com/2019/05/the-tinder-hacker.html", True)
    , ("https://animekritik.wordpress.com/2011/12/03/imperialism-translation-gunbuster-episode-five/", True)
    , ("https://www.fadedpage.com/showbook.php?pid=20160325#Page_107", True)
    , ("https://www.dailydot.com/crime/dark-web-black-market-reloaded-adam-bunger-gun-sales-arrest/", True)
    , ("https://www.candyjapan.com/results-from-box-design-ab-test", True)
    , ("https://nautil.us/will-90-become-the-new-60-rp-5956/", True)
    , ("http://emilkirkegaard.dk/en/?p=5574", True)
    , ("https://www.independent.co.uk/news/uk/home-news/smile-a-perfect-smile-but-don-t-laugh-we-have-officially-lost-our-dentures-1336158.html", True)
    , ("https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003wk#bboard_content", True)
    , ("https://www.brookings.edu/opinions/2010/0501_youth_programs_sawhill.aspx", True)
    , ("https://www.blockchain.com/btc/address/1AZvaBEJMiK8AJ5GvfvLWgHjWgL59TRPGy", True)
    , ("https://web.stanford.edu/dept/HPS/transplant/html/murray.html", True)
    , ("https://mitpress.mit.edu/sites/default/files/sicp/index.html", True)
    , ("https://freakonomics.com/2011/05/mining-for-correlations-it-works/#comment-244672", True)
    , ("https://forums.animesuki.com/showpost.php?p=3584198", True)
    , ("https://eli.thegreenplace.net/2007/06/28/sicp-section-122/", True)
    , ("https://www.theregister.com/2001/01/10/nsa_runs_best_fab/", True)
    , ("https://www.alcor.org/library/chemical-brain-preservation/", True)
    , ("https://the-liliquarium.livejournal.com/1482.html", True)
    , ("https://socghop.appspot.com/gsoc/student_project/show/google/gsoc2009/haskell/t124022467805", True)
    , ("https://people.csail.mit.edu/mrub/VisualMic/", True)
    , ("http://findarticles.com/p/articles/mi_hb4384/is_3_38/ai_n29083511/", True)
    , ("https://dataprivacylab.org/dataprivacy/projects/ssnwatch/index.html", True)
    , ("https://blog.23andme.com/health-traits/chocolate-its-complicated/", True)
    , ("https://andrewmayneblog.wordpress.com/2020/07/08/openai-api-alchemy-turn-a-script-into-a-novel-and-vice-versa/", True)
    , ("https://www.thefreelibrary.com/Sleep+debt+exacts+deceptive+cost.+%28Behavior%29.-a0100110931", True)
    , ("https://www.sfgate.com/news/article/viral-video-bear-dogs-terriers-california-home-16104193.php", True)
    , ("https://www.rrauction.com/auctions/lot-detail/31909050347196/", True)
    , ("http://www.merkle.com/cryo/techFeas.html", True)
    , ("https://www.links.org/?p=1171#comment-415465", True)
    , ("https://www.bartleby.com/205/45.html", True)
    , ("https://wavemotioncannon.com/2017/01/03/yoh-yoshinari-interview-animestyle-032013-part-33/", True)
    , ("https://www.baltimoresun.com/news/crime/bs-md-silk-road-employee-20131107-story.html", True)
    , ("https://unenumerated.blogspot.com/2011/05/bitcoin-what-took-ye-so-long.html", True)
    , ("https://scottaaronson.blog/?p=346", True)
    , ("https://rjlipton.wpcomstaging.com/2014/07/21/shifts-in-algorithm-design/", True)
    , ("http://mason.gmu.edu/~rhanson/greatfilter.html", True)
    , ("https://ilovetypography.com/2019/03/14/the-first-printed-math-books/", True)
    , ("https://humanvarieties.org/2013/01/15/100-years-of-testing-negro-intelligence/", True)
    , ("https://googlesystem.blogspot.com/2013/07/google-alerts-drops-rss-feeds.html", True)
    , ("https://www.yudkowsky.net/", True)
    , ("https://arr.am/2020/07/14/elon-musk-by-dr-seuss-gpt-3/", True)
    , ("https://www.worldcat.org/title/educational-and-vocational-preferences-of-a-cohort-spatially-gifted-females-and-males-from-the-study-of-mathematically-precocious-youth/oclc/42465636&referer=brief_results", True)
    , ("https://www.supermemo.com/en/archives1990-2015/english/ol/sm2", True)
    , ("https://www.standard.co.uk/news/crime/drugdealing-prison-librarian-bought-gun-on-the-dark-web-a3259961.html", True)
    , ("https://www.orlandosentinel.com/news/breaking-news/os-ucf-student-marijuana-arrest-towers-20150216-story.html", True)
    , ("https://www.nbcnews.com/storyline/isis-terror/isis-head-baghdadi-wife-fell-love-line-say-sources-n260291", True)
    , ("https://www.mercurynews.com/2016/01/27/san-jose-former-postal-inspector-to-serve-3-years-for-possessing-stolen-mail-marijuana-trafficking/", True)
    , ("http://www.math.uwaterloo.ca/tsp/pla85900/index.html", True)
    , ("https://www.jefftk.com/p/breaking-down-cryonics-probabilities", True)
    , ("https://www.gq.com/story/fake-hitman-murder-for-hire?printable=true", True)
    , ("http://www.businessweek.com/ap/financialnews/D9KQL7CG0.htm", True)
    , ("http://www.austlii.edu.au/cgi-bin/sinodisp/au/cases/vic/VSCA/2015/35.html", True)
    , ("https://www.aiweirdness.com/this-is-the-openai-api-it-makes-spookily-20-06-11/", True)
    , ("https://w.atwiki.jp/toho/pages/727.html", True)
    , ("https://vitalik.ca/general/2019/11/22/progress.html", True)
    , ("https://unqualified-reservations.blogspot.com/2008/01/how-i-stopped-believing-in-democracy.html", True)
    , ("https://thegrandnarrative.com/about-2/", True)
    , ("https://sre.google/sre-book/eliminating-toil/", True)
    , ("https://signalvnoise.com/posts/2942-exit-interview-founders-look-back-at-acquisitions-by-google-aol-microsoft-and-more", True)
    , ("https://shkspr.mobi/blog/2013/03/preparing-for-the-collapse-of-digital-civilization/", True)
    , ("https://qualiacomputing.com/2019/11/15/break-out-of-the-simulation-day-televised-entity-contact-injection-pulling-experiments-and-the-brain-as-a-game-engine/", True)
    , ("https://penelope.uchicago.edu/hydrionoframes/hydrion.html", True)
    , ("https://parahumans.wordpress.com/2011/06/21/gestation-1-4/", True)
    , ("https://palladiummag.com/2018/11/29/a-week-in-xinjiangs-absolute-surveillance-state/", True)
    , ("https://packdeps.haskellers.com/reverse/push-notify", True)
    , ("http://ohtori.nu/creators/contributors.html", True)
    , ("https://my.vanderbilt.edu/smpy/publications/camilla-benbow/", True)
    , ("https://mathworld.wolfram.com/FermatsLastTheorem.html", True)
    , ("https://magenta.tensorflow.org/piano-transformer", True)
    , ("https://infoproc.blogspot.com/2010/10/wigner-recollections.html", True)
    , ("https://highnoongmt.wordpress.com/2015/08/13/deep-learning-for-assisting-the-process-of-music-composition-part-3/", True)
    , ("https://googleprojectzero.blogspot.com/2017/07/trust-issues-exploiting-trustzone-tees.html", True)
    , ("https://forum.quantifiedself.com/thread-zeo-shutting-down-export-your-data?pid=3412#pid3412", True)
    , ("https://foreignpolicy.com/2012/09/28/aircraft-carriers-in-space/", True)
    , ("https://engineering.fb.com/2017/06/14/ml-applications/deal-or-no-deal-training-ai-bots-to-negotiate/", True)
    , ("https://cdn.openai.com/API/English_Bash_Python.mp4", True)
    , ("https://cdn.discordapp.com/attachments/693736350561861633/837461131991318609/unknown.png", True)
    , ("https://blog.acolyer.org/2018/02/22/dynamic-word-embeddings-for-evolving-semantic-discovery/", True)
    , ("https://web.archive.org/web/20130810215355/http://au.news.yahoo.com/today-tonight/lifestyle/article/-/17821047/online-black-market", True)
    , ("http://articles.latimes.com/1986-07-30/business/fi-18840_1_laser-defense", True)
    , ("http://alumni.media.mit.edu/~cahn/life/gian-carlo-rota-10-lessons.html#mistakes", True)
    , ("https://agtb.wordpress.com/2012/02/17/john-nashs-letter-to-the-nsa/#comment-5458", True)
    , ("https://zlkj.in/", True)
    , ("https://www.wakapoetry.net/mys-viii-1426/", True)
    , ("http://www.vetta.org/2009/12/tick-tock-tick-tock-bing/", True)
    , ("https://www.unz.com/gnxp/the-cost-of-inbreeding-in-terms-of-health/", True)
    , ("https://www.unicode.org/history/publicationdates.html", True)
    , ("https://www.unc.edu/courses/2008spring/psyc/270/001/counterbalancing.html", True)
    , ("https://www.tor.com/2012/09/26/day-of-the-kraken/", True)
    , ("http://www.tomodachi.de/", True)
    , ("https://www.thestranger.com/seattle/the-lying-disease/Content?oid=15337239", True)
    , ("https://www.the-scientist.com/features/can-destroying-senescent-cells-treat-age-related-disease--67136", True)
    , ("https://www.tabletmag.com/sections/news/articles/orthodox-jews-attacked-brooklyn-hate-crime", True)
    , ("https://www.syracuse.com/crime/2015/05/cops_seize_809_pills_170k_in_cash_in_raid_suny_oswego_student_charged.html", True)
    , ("https://www.sun-modalert.com/faq.php#shipping", True)
    , ("https://www.spiegel.de/panorama/ermittler-zerschlagen-internationalen-drogenring-a-910221.html", True)
    , ("https://www.sankakucomplex.com/2011/11/01/which-are-worse-the-seiyuu-or-their-creepy-fans/", True)
    , ("https://www.sacbee.com/news/local/crime/article2598075.html", True)
    , ("http://www.rwagner.net/libretti/parsifal/e-pars-a3.html", True)
    , ("https://www.richardcarrier.info/archives/14522", True)
    , ("https://www.rationaloptimist.com/blog/the-wealth-of-urban-biodiversity/", True)
    , ("https://www.pragmatic.ml/a-survey-of-methods-for-incorporating-long-term-context/", True)
    , ("https://www.popsci.com/woolly-mammoth-dna-brought-life-elephant-cells/", True)
    , ("https://www.owenstephens.co.uk/blog/2011/09/03/gsoc_darcs_bridge__results.html", True)
    , ("https://www.nydailynews.com/news/national/texas-man-arrested-dark-web-attempt-buy-explosives-article-1.2800015", True)
    , ("https://www.oregonlive.com/portland/2015/08/players_in_biggest_silk_road_m.html", True)
    , ("http://www.miamiherald.com/news/local/community/miami-dade/article199044679.html", True)
    , ("https://www.lrb.co.uk/the-paper/v27/n17/steven-shapin/what-did-you-expect", True)
    , ("https://www.livestrong.com/article/283811-vitamin-d-deficiency-heart-palpitations/", True)
    , ("https://www.karger.com/Article/Abstract/119229", True)
    , ("https://www.japansociety.org/otaku_talk", True)
    , ("https://www.japaninc.com/cpj/magazine/issues/1998/mar98/ohsuga.html", True)
    , ("https://www.grandforksherald.com/news/crime-and-courts/3704033-oregon-man-indicted-fatal-grand-forks-overdose-case", True)
    , ("https://www.genealogy.math.ndsu.nodak.edu/id.php?id=42477", True)
    , ("https://www.gawker.com/5926440/are-authorities-closing-in-on-the-online-drug-market-silk-road", True)
    , ("https://www.fathomevents.com/series/the-met-live-in-hd", True)
    , ("https://www.dartmouth.edu/~matc/MathDrama/reading/Wigner.html", True)
    , ("http://www.culhwch.info/index.html#footnote349-ref", True)
    , ("https://www.cs.virginia.edu/~robins/YouAndYourResearch.html", True)
    , ("https://www.cnn.com/2009/WORLD/asiapcf/06/05/japan.herbivore.men/index.html", True)
    , ("https://www.chicagotribune.com/news/ct-xpm-1989-02-28-8903090006-story.html", True)
    , ("https://www.cbsnews.com/stories/2011/03/16/business/main20043737.shtml", True)
    , ("https://www.bemmu.com/first-year-of-candy-japan", True)
    , ("https://www.4nrx-uk.md/general-health/modalert-modafinil.html", True)
    , ("https://warontherocks.com/2021/08/foreign-fighters-and-cheese-bells/", True)
    , ("https://venturebeat.com/2017/10/01/globalfoundries-next-generation-chip-factories-will-cost-at-least-10-billion/view-all/", True)
    , ("https://time.com/time/printout/0,8816,1893946,00.html", True)
    , ("https://threadreaderapp.com/thread/1187161460033458177.html", True)
    , ("https://thelastpsychiatrist.com/2011/09/how_to_be_mean_to_your_kids.html", True)
    , ("https://taooftea.com/product/frozen-summit/", True)
    , ("https://takimag.com/article/the_second_least_glamorous_job_in_showbiz", True)
    , ("https://synapse.koreamed.org/articles/1101514", True)
    , ("https://stratechery.com/2019/the-google-squeeze/", True)
    , ("https://srconstantin.wordpress.com/2014/05/07/beyond-the-one-percent-categorizing-extreme-elites/", True)
    , ("https://spikejapan.wordpress.com/about/", True)
    , ("https://soranews24.com/2019/02/14/video-shows-off-hundreds-of-beautiful-ai-created-anime-girls-in-less-than-a-minute%E3%80%90video%E3%80%91/", True)
    , ("https://senrigan.io/blog/how-writeupai-runs-behind-the-scenes/", True)
    , ("https://retractionwatch.com/2014/07/08/sage-publications-busts-peer-review-and-citation-ring-60-papers-retracted/", True)
    , ("https://replicationindex.com/", True)
    , ("https://queue.acm.org/detail.cfm?ref=rss&id=2856460", True)
    , ("https://phys.org/news/2019-12-mouse-pups-born-eggs-derived.html", True)
    , ("https://originstamp.com/s/7306a744a285474742f4f9ae8ddae8214fb7625348d578fb3077fb0bae92b8f1", True)
    , ("https://opinionator.blogs.nytimes.com/2009/11/24/the-end-of-music/", True)
    , ("https://okmij.org/ftp/Streams.html", True)
    , ("https://web.archive.org/web/20171020041641/http://nitro.biosci.arizona.edu/zbook/NewVolume_2/newvol2.html", True)
    , ("https://web.archive.org/web/20171020041641/http://nitro.biosci.arizona.edu/zbook/NewVolume_2/newvol2.html", True)
    , ("https://newcriterion.com/issues/2006/10/a-good-list", True)
    , ("https://neurosciencenews.com/brain-connectome-artificial-neural-networks/", True)
    , ("https://my.pgp-hms.org/users", True)
    , ("https://meteuphoric.com/2010/08/08/on-the-hostility-of-wives/#comment-1427", True)
    , ("https://meehl.umn.edu/files/aumeehl2003sigtests-trimmedmp3#.mp3", True)
    , ("https://mathshistory.st-andrews.ac.uk/Extras/Keynes_Newton/", True)
    , ("https://longtermrisk.org/the-importance-of-wild-animal-suffering/", True)
    , ("https://jtauber.com/blog/2004/11/26/programmed_vocabulary_learning_as_a_travelling_salesman_problem/", True)
    , ("http://journal.stuffwithstuff.com/2020/04/05/crafting-crafting-interpreters/", True)
    , ("https://ideas.repec.org/a/eee/ecolet/v91y2006i3p395-401.html", True)
    , ("https://harpers.org/archive/2013/09/the-devils-bait/?single=1", True)
    , ("https://hapgood.us/2019/03/28/network-heuristics/", True)
    , ("https://googleblog.blogspot.com/2011/10/fall-sweep.html", True)
    , ("https://globalguerrillas.typepad.com/", True)
    , ("http://felinegenetics.missouri.edu/feline-genome-project-2/cat-genomic-resources-strs-snps", True)
    , ("https://eva-fan.com/blog-entry-1198.html", True)
    , ("https://esolangs.org/wiki/Linear_bounded_automaton", True)
    , ("https://eileenormsby.com/2013/09/26/the-fall-of-atlantis-a-moderator-tells/", True)
    , ("http://diyhpl.us/wiki/transcripts/hgp-write/2016-05-10/ultra-safe-cell-line/", True)
    , ("https://egamebook.com/lochness/", True)
    , ("https://donsbot.com/2007/05/17/roll-your-own-window-manager-tracking-focus-with-a-zipper/", True)
    , ("https://cs.stanford.edu/people/karpathy/reinforcejs/gridworld_dp.html", True)
    , ("https://crookedtimber.org/2012/05/30/in-soviet-union-optimization-problem-solves-you/#comment-415931", True)
    , ("https://care.diabetesjournals.org/content/37/9/2557.full", True)
    , ("https://caniuse.com/?search=hyphenate", True)
    , ("https://bldgblog.com/2017/01/the-season-of-burning-trucks/", True)
    , ("http://betabeat.com/2011/07/another-midtown-restaurant-hudson-eatery-now-accepts-bitcoin/", True)
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
    , ("https://news.ycombinator.com/item?id=17048329", False)
    , ("https://www.science.org/content/article/plan-replicate-50-high-impact-cancer-papers-shrinks-just-18", False)
    , ("https://www.longecity.org/forum/topic/54856-modafinil-use-prosecution-convictions/", False)
    , ("https://www.cell.com/iscience/fulltext/S2589-0042(21)00473-9", False)
    , ("https://yunnansourcing.com/collections/flower-and-herbal-teas/products/yunnan-sun-dried-wild-rose-buds-from-wenshan", False)
    , ("https://www.businessinsider.com/deepfake-tech-create-fictitious-faces-cats-airbnb-listings-2019-2", False)
    , ("https://www.forbes.com/sites/andygreenberg/2013/11/07/sting-operation-nabs-alleged-online-arms-dealer-via-silk-road-competitor-site/", False)
    , ("https://www.deviantart.com/tootootaloo/art/Celestia-Vector-212081002", False)
    , ("https://bitcointalk.org/index.php?topic=14828.0", False)
    , ("http://www.usagi.org/doi/seiyuu/radio/boogie/19970118.html", False)
    , ("https://www.telegraph.co.uk/news/2016/09/16/french-intelligence-cracks-telegram-account-of-most-notorious-is/", False)
    , ("https://www.cambridge.org/core/journals/journal-of-economic-history/article/two-centuries-of-productivity-growth-in-computing/856EC5947A5857296D3328FA154BA3A3", False)
    , ("https://www.apa.org/monitor/2011/09/achievement.aspx", False)
    , ("http://kanzaki.sub.jp/archives/000282.html", False)
    , ("http://www.smartpowders.com/p-5327-l-theanine-powder-20-grams.aspx", False)
    , ("https://www.fastcompany.com/90659827/the-bootleg-fire-is-burning-through-trees-that-are-being-used-as-carbon-offsets", False)
    , ("https://www.theage.com.au/politics/queensland/strawberry-growers-shut-the-farm-gate-after-tampering-crisis-20190402-p519zk.html", False)
    , ("https://www.fda.gov/news-events/press-announcements/fda-approves-first-oral-glp-1-treatment-type-2-diabetes", False)
    , ("https://www.odt.co.nz/news/dunedin/student-drug-dealer-jailed", False)
    , ("http://www.sequentialtart.com/archive/mar04/cv_0304_2.shtml", False)
    , ("https://spectrum.ieee.org/lasers-and-lunar-arks-cryopreservation-heats-up", False)
    , ("https://www.abc.net.au/news/2015-01-07/charlie-hebdo-satirical-newspaper-shooting-paris-12-killed/6005524", False)
    , ("https://towardsdatascience.com/gpt-3-creative-potential-of-nlp-d5ccae16c1ab", False)
    , ("https://qz.com/1311732/openai-built-gaming-bots-that-can-work-as-a-team-with-inhuman-precision/", False)
    , ("https://techcrunch.com/2013/02/23/the-chinese-are-coming-the-chinese-are-coming/", False)
    , ("https://astralcodexten.substack.com/", False)
    , ("https://www.cs.utexas.edu/~EWD/transcriptions/EWD05xx/EWD594.html", False)
    , ("https://www.nzherald.co.nz/nz/officers-link-to-net-drug-market/3MQUK5H6PDVBIZHTDZUG3TITMA/?c_id=1&objectid=10819029", False)
    , ("https://www.quantamagazine.org/the-busy-beaver-game-illuminates-the-fundamental-limits-of-math-20201210/", False)
    , ("https://www.thedailybeast.com/vegan-youtube-is-imploding-as-stars-like-rawvana-bonny-rebecca-and-stella-rae-change-diets", False)
    , ("https://www.angelfire.com/anime4/mdwigs/livesequence.html", False)
    , ("https://ije.oxfordjournals.org/content/30/6/1251.full", False)
    , ("http://jn.nutrition.org/content/141/2/261", False)
    , ("https://blog.johantibell.com/2011/08/results-from-state-of-haskell-2011.html", False)
    , ("http://darcs.net/GSoC/2010-Cache", False)
    , ("http://www.unitedpharmacies-uk.md/Modavigil-Modafinil-100mg-30-Tablets-p-759.html", False)
    , ("http://www.replicatedtypo.com/chocolate-consumption-traffic-accidents-and-serial-killers/5718.html", False)
    , ("https://www.petforums.co.uk/threads/strange-things-that-you-cats-do.475528/page-3#post-1065130574", False)
    , ("https://www.nybooks.com/articles/2011/03/10/how-we-know/", False)
    , ("https://www.newscientist.com/article/2133095-boom-in-human-gene-editing-as-20-crispr-trials-gear-up/", False)
    , ("https://www.mirror.co.uk/news/uk-news/man-jailed-trying-smuggle-550000-3592428", False)
    , ("https://www.jstor.org/stable/10.1086/468061", False)
    , ("https://www.instructables.com/Low-power-doorwindow-sensor/", False)
    , ("https://www.buzzfeednews.com/article/justinesharrock/whats-wrong-with-google-alerts", False)
    , ("http://web.mit.edu/remy/", False)
    , ("https://variety.com/2017/digital/news/netflix-thumbs-vs-stars-1202010492/", False)
    , ("https://stats.stackexchange.com/users/2392/probabilityislogic", False)
    , ("https://schizophreniabulletin.oxfordjournals.org/content/33/6/1277.full", False)
    , ("https://politicalscience.osu.edu/faculty/jmueller/MID11TSM.PDF", False)
    , ("https://hal.archives-ouvertes.fr/hal-00904097/document", False)
    , ("https://code.google.com/archive/p/amphetype", False)
    , ("http://cardcaptor.moekaku.com/?p=112", False)
    , ("https://wyclif.substack.com/p/the-natural-selection-paper-part-908", False)
    , ("https://www.thenewatlantis.com/publications/shop-class-as-soulcraft", False)
    , ("https://www.nola.com/news/crime_police/article_bd297d74-f2e1-5c15-9db3-c96545f38688.html", False)
    , ("http://www.nausicaa.net/miyazaki/interviews/aboutanime.html", False)
    , ("https://www.nap.edu/read/25259/chapter/2", False)
    , ("https://www.japantimes.co.jp/news/2017/09/12/national/social-issues/1-20-infants-born-vitro-fertilization-japan-survey/", False)
    , ("https://www.huffpost.com/entry/photographs-redheads-of-color_n_55db9929e4b0a40aa3abf017", False)
    , ("https://www.hindawi.com/journals/ijg/2018/5121540/", False)
    , ("https://www.girlscouts.org/program/gs_cookies/cookie_faqs.asp#bestselling", False)
    , ("https://www.esquire.com/news-politics/a20903/hugh-hefner-interview-0413/", False)
    , ("https://www.eetimes.com/document.asp?doc_id=1333422", False)
    , ("http://www.dcscience.net/2015/12/11/placebo-effects-are-weak-regression-to-the-mean-is-the-main-reason-ineffective-treatments-appear-to-work/", False)
    , ("https://www.chronicle.com/article/Google-Begins-to-Scale-Back/131109/", False)
    , ("https://www.amazon.co.jp/%E7%B4%85%E4%B8%80%E7%82%B9%E8%AB%96%E2%80%95%E3%82%A2%E3%83%8B%E3%83%A1%E3%83%BB%E7%89%B9%E6%92%AE%E3%83%BB%E4%BC%9D%E8%A8%98%E3%81%AE%E3%83%92%E3%83%AD%E3%82%A4%E3%83%B3%E5%83%8F-%E3%81%A1%E3%81%8F%E3%81%BE%E6%96%87%E5%BA%AB-%E6%96%8E%E8%97%A4-%E7%BE%8E%E5%A5%88%E5%AD%90/dp/4480036660", False)
    , ("https://vndb.org/c582", False)
    , ("https://vividness.live/protestant-buddhism", False)
    , ("http://thepharmacyexpress.com/Products2.asp?Brand=MODALERT+%28+Provigil%2C+Modapro%2C+Modvigil%2C+Generic+Modafinil+%29&T=d", False)
    , ("http://ro.ecu.edu.au/cgi/viewcontent.cgi?article=1025&context=spsyc_pres", False)
    , ("https://repository.upenn.edu/cgi/viewcontent.cgi?article=1038&context=neuroethics_pubs", False)
    , ("https://plus.google.com/u/0/103530621949492999968/posts/AThvaCXCSp2", False)
    , ("https://pastebin.com/GrV3uYh5", False)
    , ("https://omega0.xyz/omega8008/ETJ-PS/cc5d.ps", False)
    , ("https://nutritionj.biomedcentral.com/articles/10.1186/s12937-017-0269-y", False)
    , ("https://media.nature.com/original/nature-assets/nature/journal/v533/n7604/extref/nature17671-s2.xlsx", False)
    , ("http://latanyasweeney.org/cv.html#survey", False)
    , ("https://handbook.cochrane.org/front_page.htm", False)
    , ("https://groups.yahoo.com/neo/groups/catsandkittens/conversations/messages/188981", False)
    , ("https://genomebiology.biomedcentral.com/articles/10.1186/s13059-018-1506-1", False)
    , ("https://cloud.google.com/tpu/pricing", False)
    , ("https://digitalcommons.unl.edu/cgi/viewcontent.cgi?article=1024&context=vpc15", False)
    , ("https://bmcpublichealth.biomedcentral.com/articles/10.1186/1471-2458-7-159#B28", False)
    , ("https://beta.openai.com/", False)
    , ("https://aeon.co/essays/why-fake-miniatures-depicting-islamic-science-are-everywhere", False)
    , ("https://ajp.psychiatryonline.org/doi/abs/10.1176/appi.ajp.2020.19080834", False)
    , ("http://www.xinhuanet.com/english/2019-08/23/c_138332084.htm", False)
    , ("https://www.wolframalpha.com/input/?i=male+height+distribution", False)
    , ("https://www.walmart.com/ip/Great-Value-Fudge-Mint-Cookies-10-oz/11997740", False)
    , ("https://www.upi.com/Health_News/2011/04/27/Omega-3-may-up-aggressive-prostate-cancer/31131303903320/?u3L=1", False)
    , ("https://www.ukbiobank.ac.uk/frontiers-meeting-london-2014", False)
    , ("http://www.trixietracker.com/pricing/", False)
    , ("https://www.theringer.com/features/2021/6/21/22542839/disc-golf-niche-sports-million-dollar-endorsement-deals", False)
    , ("https://www.thelancet.com/journals/ebiom/article/PIIS2352-3964(19)30591-2/fulltext", False)
    , ("https://www.teavana.com/us/en/tea/green-tea/gyokuro-genmaicha-green-tea-31440.html", False)
    , ("https://www.spiceandtea.com/coconut-oolong-p-587.html?zenid=ot0nli0t5cfvbitsmhi3f2pv16", False)
    , ("http://www.snpp.com/", False)
    , ("https://www.scotthyoung.com/blog/", False)
    , ("https://www.rightstufanime.com/Utena-Revolutionary-Girl-DVD-Set-3-Apocalypse-Saga-Hyb-Limited-Edition", False)
    , ("https://www.psychologicalscience.org/observer/in-appreciation-julian-stanley", False)
    , ("https://www.optimox.com/iodine-study-12", False)
    , ("https://www.nobelprize.org/prizes/chemistry/2020/press-release/", False)
    , ("https://www.mayoclinic.org/drugs-supplements/metformin-oral-route/proper-use/drg-20067074", False)
    , ("https://www.marxists.org/reference/archive/hegel/works/nl/ch03.htm", False)
    , ("http://www.mania.com/aodvb/showthread.php?p=1502313", False)
    , ("http://www.mangauk.com/?p=annos-dominus", False)
    , ("https://www.labone.tech/anime-generative-model-part-3/", False)
    , ("https://www.jameslindlibrary.org/articles/inventing-the-randomized-double-blind-trial-the-nurnberg-salt-test-of-1835/", False)
    , ("http://www.isteve.com/ManlyMolecule.htm", False)
    , ("https://www.imagesco.com/catalog/DigitalCompass/DigitalCompass.html", False)
    , ("https://www.iherb.com/pr/now-foods-kelp-150-mcg-200-tablets/685", False)
    , ("http://www.holidaymead.com/#!product-page/cba4/f8f43032-665c-5a4c-6756-1ac355b32549", False)
    , ("https://www.genome.gov/about-genomics/fact-sheets/DNA-Sequencing-Costs-Data", False)
    , ("https://www.flickr.com/groups/wiredsky/pool/", False)
    , ("http://www.fasebj.org/cgi/content/meeting_abstract/26/1_MeetingAbstracts/114.4", False)
    , ("https://www.eric.ed.gov/ERICWebPortal/custom/portlets/recordDetails/detailmini.jsp?_nfpb=true&_&ERICExtSearch_SearchValue_0=EJ724232&ERICExtSearch_SearchType_0=no&accno=EJ724232", False)
    , ("https://www.emcdda.europa.eu/publications/posters/2018/darknet-markets-ecosystem_en", False)
    , ("https://www.drugs.com/dosage/metformin.html", False)
    , ("http://www.dummy-system.com/2013/04/23/intervista-a-maaya-sakamoto-su-evangelion-3-0/", False)
    , ("https://www.cochranelibrary.com/doi/10.1002/14651858.CD008661/full", False)
    , ("http://www.cehd.umn.edu/CAREI/Reports/summary.html#SchoolStart", False)
    , ("https://www.bulletproof.com/why-you-are-suffering-from-a-modafinil-deficiency/", False)
    , ("http://www.bayesianinvestor.com/blog/index.php/2008/11/13/automated-market-maker-results/", False)
    , ("https://www.avclub.com/ducktales-invented-a-new-animated-wonderland-that-quick-1798236288", False)
    , ("http://www.animevice.com/death-note-hollywood-live-action/13-1374/rumor-alert-death-note-movie-script-leaked/97-207040/#js-post-body-116650", False)
    , ("http://www.animenewsservice.com/archives/dec13.htm", False)
    , ("https://www.anandtech.com/show/12535/power-outage-at-samsungs-fab-destroys-3-percent-of-global-nand-flash-output", False)
    , ("https://www.afp.gov.au/media-centre/news/afp/2015/may/four-australians-charged-in-international-illegal-firearm-sting", False)
    , ("https://www.adafruit.com/product/328", False)
    , ("https://web-archive-org.translate.goog/web/20070301005837/homepage3.nifty.com/kyrie/story26.html?_x_tr_sch=http&_x_tr_sl=ja&_x_tr_tl=en&_x_tr_hl=en-US", False)
    , ("https://wcfcourier.com/news/local/crime-and-courts/waterloo-man-pleads-guilty-to-selling-ecstacy/article_a57aab93-b9c5-5e3e-8438-6e229c9bd036.html", False)
    , ("https://today.yougov.com/topics/politics/articles-reports/2021/07/15/why-wont-americans-get-vaccinated-poll-data", False)
    , ("https://timesofindia.indiatimes.com/city/bengaluru/Peddlers-procure-narcotics-from-darknet/articleshow/48368502.cms", False)
    , ("https://thenextweb.com/news/google-alerts-regains-rss-delivery-option-it-lost-after-google-readers-demise", False)
    , ("http://stroke.ahajournals.org/content/39/10/2824.full", False)
    , ("https://support.google.com/websearch/answer/2466433", False)
    , ("http://sigbovik.org/2019/", False)
    , ("https://reason.com/2017/11/28/in-search-of-the-elusive-bitco/", False)
    , ("https://raw.githubusercontent.com/google-research/google-research/master/automl_zero/best_algo.gif", False)
    , ("https://qa.debian.org/popcon.php?package=mnemosyne", False)
    , ("http://pubs.acs.org/doi/abs/10.1021/es0719071", False)
    , ("https://patents.google.com/patent/US20150124107", False)
    , ("https://patch-tag.com/", False)
    , ("https://opensnp.org/", False)
    , ("https://oll.libertyfund.org/title/detmold-the-historical-political-and-diplomatic-writings-vol-2#lf0076-02_label_005", False)
    , ("https://nypost.com/2019/11/02/stanford-professor-who-changed-america-with-just-one-study-was-also-a-liar/", False)
    , ("https://nmteaco.com/Plum-Oolong_p_380.html", False)
    , ("http://new.nubrain.com/shop/index.php?route=product/product&keyword=modafinil&category_id=0&product_id=58", False)
    , ("https://medlineplus.gov/druginfo/meds/a602016.html#side-effects", False)
    , ("https://mattsclancy.substack.com/p/remote-work-and-the-future-of-innovation", False)
    , ("https://lostpinesyaupontea.com/products/light-roast-yaupon-tea", False)
    , ("https://lobste.rs/s/1d7whd/tales_from_trenches_ai_disaster_stories#c_le6tsr", False)
    , ("https://lists.wikimedia.org/hyperkitty/list/wikimedia-l@lists.wikimedia.org/message/DC4YYUFMYS6CSOOPINPBR4FPS2L53BVE/", False)
    , ("http://ldsc.broadinstitute.org/about/", False)
    , ("http://itre.cis.upenn.edu/~myl/languagelog/archives/005526.html", False)
    , ("https://iopscience.iop.org/article/10.1088/1748-3190/ac253a", False)
    , ("https://ideas.4brad.com/has-uber-already-beaten-private-ownership-cost", False)
    , ("https://hplusmagazine.com/2012/04/12/transhumanism-and-the-human-expansion-into-space-a-conflict-with-physics/", False)
    , ("https://hbr.org/2019/12/can-you-know-too-much-about-your-organization", False)
    , ("https://habr.com/en/post/429602/", False)
    , ("https://fivethirtyeight.com/features/the-complicated-legacy-of-a-panda-who-was-really-good-at-sex/", False)
    , ("https://gigascience.biomedcentral.com/articles/10.1186/2047-217X-3-10", False)
    , ("https://eprints.whiterose.ac.uk/97780/", False)
    , ("https://fortune.com/2013/05/15/dirty-medicine/", False)
    , ("https://data.worldbank.org/indicator/NY.GNP.PCAP.PP.CD", False)
    , ("https://crypto.stackexchange.com/questions/5831/what-is-the-progress-on-the-mit-lcs35-time-capsule-crypto-puzzle", False)
    , ("https://community.haskell.org/~gwern/hcorpus/", False)
    , ("http://bugs.darcs.net/issue346", False)
    , ("https://bmcneurosci.biomedcentral.com/articles/10.1186/1471-2202-6-23", False)
    , ("http://blog.sigfpe.com/2012/12/shuffles-bayes-theorem-and-continuations.html", False)
    , ("http://blog.darcs.net/2010/11/coming-in-darcs-28-new-features.html", False)
    , ("http://bifunctor.homelinux.net/~roel/cgi-bin/hackage-scripts/revdeps/iteratee#direct", False)
    , ("http://betsofbitco.in/list?status=available&category=All&sorting=-moderationTime", False)
    , ("https://benbest.com/nutrceut/melatonin.html#negative", False)
    , ("https://bayes.wustl.edu/etj/articles/general.background.ps.gz", False)
    , ("http://bactra.org/weblog/algae-2012-09.html", False)
    ]
