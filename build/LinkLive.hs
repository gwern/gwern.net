{- LinkLive.hs: Specify domains which can be popped-up "live" in a frame by adding a link class.
Author: Gwern Branwen
Date: 2022-02-26
When:  Time-stamp: "2022-12-08 23:23:11 gwern"
License: CC-0

Based on LinkIcon.hs. At compile-time, set the HTML class `link-live` on URLs from domains verified
to work (reasonably) well as cross-site popups inside a frame.
`extracts-contents.js` at runtime reads the class to decide which links will be live-popup-able.

Live popups are an alternative to, or a further step from, annotations. They let the reader preview
a link instantly. This is useful when an annotation is not available, or when the reader has read
the annotation and wants to go further.

However, due to the March of Web Progress™, many websites set X headers <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options>
or just plain don't work in a frame (often due to JS, and then often due to extremely reader-unfriendly design
like giant headers or stickies), or are 'mixed content' - that is, the HTTP (or HTTPS) version would work perfectly
*except browsers block it* in the name of security, so a reader who visits HTTPS gwern.net can't load the HTTP version
and I have to test the HTTPS version instead, which even when it exists, will often be broken by things like
self-signed certificates etc.
Perhaps only a quarter of external links work as live popups.
So we can't just offer it as an option on all links, that will waste reader time & trust, and they will
learn to avoid the feature entirely and resent the visual clutter and trap of this 'feature'.

We instead whitelist domains based on manual testing using the list of links in /Lorem#live-link-popups.
Since there are so many domains, we need a testsuite to keep track of what domains have been tested & found good,
tested & found bad, and testing is not done or ambiguous (due to practical issues like a test link having become
a local archive or 404 or changed domain entirely).

Finally, to keep up to date with new domains, each sync we rank domains by # of uses, and above a threshold,
automatically generate a live-link testcase appended to /Lorem for manual review.

For an independent JS NPM library implementation, see <https://github.com/Stvad/link-summoner>.
-}

{-# LANGUAGE OverloadedStrings #-}
module LinkLive (linkLive, linkLiveString, linkLiveTest, linkLiveTestHeaders, urlLive, linkLivePrioritize) where

import Control.Monad (forM_, when, unless)
import Data.Char (toLower)
import Data.List (isInfixOf, sort)
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as M (fromListWith, toList, map, keys)
import qualified Data.Text as T (append, isInfixOf, isPrefixOf, pack, unpack, Text)
import Data.Text.IO as TIO (appendFile)
import Text.Pandoc (Inline(Link), nullAttr)
import Data.FileStore.Utils (runShellCommand)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import System.Exit (ExitCode(ExitFailure))

import Interwiki (wpPopupClasses)
import LinkBacklink (readBacklinksDB, Backlinks)
import Utils (addClass, host, anySuffixT, printRed)

linkLive :: Inline -> Inline
linkLive x@(Link (_,cl,_) _ (u, _))
 | "link-live-not" `elem` cl = x
 | u `elem` overrideLinkLive = aL x
 | "/" `T.isPrefixOf` u = x -- local links shouldn't match anything, but to be safe, we'll check anyway.
 | otherwise = case urlLive u of
                 Just True -> aL x
                 _         -> x
 where aL :: Inline -> Inline
       aL = addClass "link-live"
linkLive x = x

linkLiveString :: String -> Inline
linkLiveString u = linkLive (Link nullAttr [] (T.pack u,""))

-- hardwire URLs which should be live
overrideLinkLive :: [T.Text]
overrideLinkLive = []

-- Nothing = unknown/untested; Just True = known good; Just False = known bad
urlLive :: T.Text -> Maybe Bool
urlLive u | u'      `elem`  goodDomainsSimple = Just True
          | anySuffixT u'   goodDomainsSub    = Just True
          | u'     `elem`   badDomainsSimple  = Just False
          | anySuffixT u'   badDomainsSub     = Just False
          | ".wikipedia.org" `T.isInfixOf` u  = wikipedia u
          | otherwise                         = Nothing
   where u' = host u

linkLivePrioritize :: IO [(Int, T.Text)]
linkLivePrioritize = do b <- readBacklinksDB
                        let b' = M.toList $ M.map length b
                        let b'' = map (\(c,d) -> (host c,d)) $ filter (\(url',count) -> count >= linkLiveMinimum &&
                                                                                       not ("pdf" `T.isInfixOf` url' || "PDF" `T.isInfixOf` url' || ".ps" `T.isInfixOf` url') &&
                                                                                       host url' `notElem` blackList &&
                                                                                       (isNothing . urlLive) url' &&
                                                                                       ("." `T.isInfixOf` url')) b'
                        let b''' =  M.fromListWith (+) b''
                        let hits = reverse $ sort $ Prelude.filter ((/="") . snd) $ map (\(e,f) -> (f,e)) $ M.toList b'''
                        unless (null hits) $ mapM_ (\(_,l) -> writeLinkLiveTestcase b l) hits
                        return hits
  where blackList = ["omega.albany.edu"]
        linkLiveMinimum = 3
        -- Append an example of a prioritized link to /Lorem#link-testcases for manual review, to skip copy-paste hassle
        writeLinkLiveTestcase :: Backlinks -> T.Text -> IO ()
        writeLinkLiveTestcase b l = let link = head $ filter (l `T.isInfixOf`) $ M.keys b in -- take the first URL which matches the domain:
                                      TIO.appendFile "Lorem.page" $ "\n- <" `T.append` link `T.append` ">{.archive-not .link-annotated-not .link-live}"

-- Wikipedia link-live capabilities are page-dependent: anything in the Special namespace is blocked by headers (which makes sense given how many queries/capabilities are inside it). But it looks like pretty much all other namespaces (see Interwiki.hs's nonArticleNamespace for a list) should be live?
wikipedia :: T.Text -> Maybe Bool
wikipedia u = Just $ "link-live" `elem` wpPopupClasses u

goodDomainsSub, goodDomainsSimple, badDomainsSub, badDomainsSimple :: [T.Text]
goodDomainsSub = [".allennlp.org", ".archive.org", ".archiveteam.org", ".bandcamp.com", ".eleuther.ai", ".fandom.com",
                   ".github.io", ".givewell.org", ".greenspun.com", ".humanprogress.org", ".imagemagick.org", ".mementoweb.org",
                   ".metafilter.com", ".nomeata.de", ".obormot.net", ".tumblr.com", ".xkcd.com", ".wordpress.com",
                   ".blogspot.com"]
goodDomainsSimple =
  ["1dollarscan.com"
    , "6thfloor.blogs.nytimes.com"
    , "80000hours.org"
    , "abandonedfootnotes.blogspot.com"
    , "abcnews.go.com"
    , "abcnotation.com"
    , "about.google"
    , "academic.oup.com"
    , "academictorrents.com"
    , "aclanthology.org"
    , "advertising-effects.chicagobooth.edu"
    , "ageing.oxfordjournals.org"
    , "ageofem.com"
    , "agtb.wordpress.com"
    , "ai.google"
    , "ai.googleblog.com"
    , "aiimpacts.org"
    , "aleph.se"
    , "alexanderetz.com"
    , "altjapan.typepad.com"
    , "alumni.media.mit.edu"
    , "andrewmayneblog.wordpress.com"
    , "andymatuschak.org"
    , "animekritik.wordpress.com"
    , "ansuz.sooke.bc.ca"
    , "apenwarr.ca"
    , "apnews.com"
    , "archinte.jamanetwork.com"
    , "architext.design"
    , "archive.nytimes.com"
    , "archive.org"
    , "archive.ph"
    , "archive.seattletimes.com"
    , "archivebox.io"
    , "arima.cylab.cmu.edu"
    , "arr.am"
    , "articles.latimes.com"
    , "asktog.com"
    , "b-ok.cc"
    , "bair.berkeley.edu"
    , "bam-dataset.org"
    , "bam.kalzumeus.com"
    , "beepb00p.xyz"
    , "behavioralscientist.org"
    , "bellard.org"
    , "beza1e1.tuxen.de"
    , "bitcoin-otc.com"
    , "bits.blogs.nytimes.com"
    , "bldgblog.com"
    , "blog.8faces.com"
    , "blog.aboutamazon.com"
    , "blog.acolyer.org"
    , "blog.beeminder.com"
    , "blog.codinghorror.com"
    , "blog.cr.yp.to"
    , "blog.cryptographyengineering.com"
    , "blog.csdn.net"
    , "blog.ethereum.org"
    , "blog.google"
    , "blog.google.com"
    , "blog.nuclearsecrecy.com"
    , "blog.otoro.net"
    , "blog.pinboard.in"
    , "blog.regehr.org"
    , "blog.thinkst.com"
    , "blog.youtube"
    , "blogs.nvidia.com"
    , "blogs.wsj.com"
    , "bmk.sh"
    , "boards.fireden.net"
    , "boingboing.net"
    , "bwc.thelab.dc.gov"
    , "cacm.acm.org"
    , "calhoun.nps.edu"
    , "caniuse.com"
    , "carbonplan.org"
    , "care.diabetesjournals.org"
    , "casual-effects.com"
    , "cat-unbound.org"
    , "catonmat.net"
    , "causal-effects.com"
    , "cdn.discordapp.com"
    , "cdn.openai.com"
    , "citeseerx.ist.psu.edu"
    , "clinicaltrials.gov"
    , "cognitivemedium.com"
    , "commons.wikimedia.org"
    , "compdemocracy.org"
    , "complearn.org"
    , "conifer.rhizome.org"
    , "constancecrozier.com"
    , "copilot.github.com"
    , "corpgov.law.harvard.edu"
    , "courses.csail.mit.edu"
    , "cran.r-project.org"
    , "crookedtimber.org"
    , "cs.stanford.edu"
    , "ctan.org"
    , "culture.org"
    , "daniellakens.blogspot.com"
    , "danluu.com"
    , "danwang.co"
    , "data.bls.gov"
    , "datacolada.org"
    , "dataprivacylab.org"
    , "davidepstein.com"
    , "dealbook.nytimes.com"
    , "defector.com"
    , "dialnet.unirioja.es"
    , "diff.wikimedia.org"
    , "digital.library.unt.edu"
    , "distill.pub"
    , "diyhpl.us"
    , "dnstats.net"
    , "docs.google.com"
    , "dominiccummings.com"
    , "donsbot.com"
    , "downloads.haskell.org"
    , "dresdencodak.com"
    , "dumps.wikimedia.org"
    , "dwarffortresswiki.org"
    , "e2eml.school"
    , "egamebook.com"
    , "eileenormsby.com"
    , "eli.thegreenplace.net"
    , "emilkirkegaard.dk"
    , "en.bitcoin.it"
    , "en.touhouwiki.net"
    , "en.wikibooks.org"
    , "en.wikichip.org"
    , "en.wikifur.com"
    , "en.wikiquote.org"
    , "en.wikisource.org"
    , "en.wiktionary.org"
    , "endlessvn.io"
    , "energycontrol.org"
    , "engineering.fb.com"
    , "eprint.iacr.org"
    , "equilibriabook.com"
    , "esolangs.org"
    , "etienne.se"
    , "eukaryotewritesblog.com"
    , "eurekamag.com"
    , "eurekamaga.com"
    , "eva-fan.com"
    , "evaotaku.com"
    , "ew.com"
    , "exac.broadinstitute.org"
    , "examples.yourdictionary.com"
    , "explorabl.es"
    , "extras.denverpost.com"
    , "familiarcycle.net"
    , "fastmoe.ai"
    , "feeds.feedburner.com"
    , "felinegenetics.missouri.edu"
    , "fibery.io"
    , "files.eric.ed.gov"
    , "findarticles.com"
    , "foreignpolicy.com"
    , "forre.st"
    , "forum.effectivealtruism.org"
    , "forum.evageeks.org"
    , "forum.quantifiedself.com"
    , "forums.animesuki.com"
    , "foundation.wikimedia.org"
    , "fras.uk"
    , "freakonomics.com"
    , "freedomdefined.org"
    , "fs.blog"
    , "fullfrontal.moe"
    , "futurism.com"
    , "galois.com"
    , "gamefaqs.gamespot.com"
    , "gameprogrammingpatterns.com"
    , "gaotianyu.xyz"
    , "github.blog"
    , "globalguerrillas.typepad.com"
    , "globalvoices.org"
    , "gondwanaland.com"
    , "google-summer-of-code-2009-haskell.googlecode.com"
    , "googleblog.blogspot.com"
    , "googleprojectzero.blogspot.com"
    , "googlesystem.blogspot.com"
    , "googlesystem.blogspot.com"
    , "gpt3demo.com"
    , "gradientscience.org"
    , "greaterwrong.com"
    , "gutenberg.ca"
    , "gutenberg.net.au"
    , "guzey.com"
    , "hackage.haskell.org"
    , "hakaimagazine.com"
    , "handbook-5-1.cochrane.org"
    , "hapgood.us"
    , "harpers.org"
    , "hdsr.mitpress.mit.edu"
    , "healthland.time.com"
    , "herbsutter.com"
    , "highnoongmt.wordpress.com"
    , "historycooperative.org"
    , "hub.darcs.net"
    , "humanvarieties.org"
    , "ideas.repec.org"
    , "idlewords.com"
    , "ifdo.ca"
    , "iforcedabot.com"
    , "ilovetypography.com"
    , "image-net.org"
    , "infoproc.blogspot.com"
    , "inhumanexperiment.blogspot.com"
    , "intelligence.org"
    , "iqcomparisonsite.com"
    , "isomerdesign.com"
    , "jakewestfall.org"
    , "jama.jamanetwork.com"
    , "jamanetwork.com"
    , "jamesyu.org"
    , "jasoncrawford.org"
    , "jaspervdj.be"
    , "jax.readthedocs.io"
    , "jaypsong.blog"
    , "jdlm.info"
    , "jessegalef.com"
    , "jetpress.org"
    , "jgeekstudies.org"
    , "joa.sh"
    , "johncwright.livejournal.com"
    , "jonmillward.com"
    , "joshmitteldorf.scienceblog.com"
    , "journal.stuffwithstuff.com"
    , "journals.biologists.com"
    , "jov.arvojournals.org"
    , "jtauber.com"
    , "jtoomim.org"
    , "justgetflux.com"
    , "kajsotala.fi"
    , "kalzumeus.com"
    , "keras.io"
    , "kev.town"
    , "kevinlynagh.com"
    , "kill-the-newsletter.com"
    , "kk.org"
    , "knightcolumbia.org"
    , "kojimars.at.webry.info"
    , "komonews.com"
    , "krebsonsecurity.com"
    , "laion.ai"
    , "langsec.org"
    , "larryniven.net"
    , "latimesblogs.latimes.com"
    , "lavaan.ugent.be"
    , "learning.mpi-sws.org"
    , "ledge-cli.org"
    , "lesswrong.com"
    , "lettersofnote.com"
    , "libgen.rs"
    , "library.bz"
    , "lifescivc.com"
    , "lithub.com"
    , "littlebiggy.org"
    , "longitudinal.blog"
    , "longtermrisk.org"
    , "lucumr.pocoo.org"
    , "magazine.atavist.com"
    , "magenta.tensorflow.org"
    , "mail.haskell.org"
    , "mail.haskell.org"
    , "mailchi.mp"
    , "mako.cc"
    , "marginalrevolution.com"
    , "mason.gmu.edu"
    , "mathbabe.org"
    , "mathshistory.st-andrews.ac.uk"
    , "mathworld.wolfram.com"
    , "mattlakeman.org"
    , "mc-stan.org"
    , "medicalxpress.com"
    , "meta.wikimedia.org"
    , "meteuphoric.com"
    , "metropolitician.blogs.com"
    , "michaelnielsen.org"
    , "mikepower.pressfolios.com"
    , "milan.cvitkovic.net"
    , "minimaxir.com"
    , "mitpress.mit.edu"
    , "mkv25.net"
    , "ml.berkeley.edu"
    , "mlg.eng.cam.ac.uk"
    , "mmlab.ie.cuhk.edu.hk"
    , "mosaicscience.com"
    , "mssv.net"
    , "my.pgp-hms.org"
    , "my.vanderbilt.edu"
    , "nautil.us"
    , "nbc-2.com"
    , "ncase.me"
    , "neojaponisme.com"
    , "neurosciencenews.com"
    , "newcriterion.com"
    , "news.nationalgeographic.com"
    , "ngm.nationalgeographic.com"
    , "nickbostrom.com"
    , "nintil.com"
    , "nootropicsdepot.com"
    , "norvig.com"
    , "notes.pinboard.in"
    , "numinous.productions"
    , "nyaa.si"
    , "nymag.com"
    , "ohtori.nu"
    , "okmij.org"
    , "online.wsj.com"
    , "ooo.ghostbows.ooo"
    , "openai.com"
    , "opensource.adobe.com"
    , "opinionator.blogs.nytimes.com"
    , "originstamp.com"
    , "orionmagazine.org"
    , "orwell.ru"
    , "oscarbonilla.com"
    , "ourworldindata.org"
    , "packdeps.haskellers.com"
    , "palladiummag.com"
    , "palmerlab.org"
    , "pandoc.org"
    , "papers.ssrn.com"
    , "parahumans.wordpress.com"
    , "parametric.press"
    , "patrickcollison.com"
    , "pauillac.inria.fr"
    , "paulfchristiano.com"
    , "pdfs.semanticscholar.org"
    , "pediatrics.aappublications.org"
    , "penelope.uchicago.edu"
    , "people.com"
    , "people.csail.mit.edu"
    , "people.csail.mit.edu"
    , "people.idsia.ch"
    , "personal.math.ubc.ca"
    , "personalitytest.net"
    , "petertodd.org"
    , "pharmacyreviewer.co"
    , "philpapers.org"
    , "phys.org"
    , "pinboard.in"
    , "plato.stanford.edu"
    , "playground.tensorflow.org"
    , "poemanalysis.com"
    , "poets.org"
    , "politicalcalculations.blogspot.com"
    , "popcon.ubuntu.com"
    , "press.etc.cmu.edu"
    , "proceedings.mlr.press"
    , "programme.exordo.com"
    , "progressstudies.school"
    , "projects.jsonline.com"
    , "projects.tampabay.com"
    , "psych.hanover.edu"
    , "psychcentral.com"
    , "psychonautwiki.org"
    , "publicdomainreview.org"
    , "publishing.cdlib.org"
    , "pudding.cool"
    , "pytorch.org"
    , "qntm.org"
    , "quadrant.org.au"
    , "qualiacomputing.com"
    , "quantifiedself.com"
    , "quantum.country"
    , "queue.acm.org"
    , "quillette.com"
    , "quinndunki.com"
    , "qwantz.com"
    , "r6.ca"
    , "racket-lang.org"
    , "rationality.org"
    , "rdiff-backup.net"
    , "read-the-samples.netlify.app"
    , "readwrite.com"
    , "reflectivedisequilibrium.blogspot.com"
    , "replicationindex.com"
    , "retractionwatch.com"
    , "retractionwatch.com"
    , "ricon.dev"
    , "rjlipton.wordpress.com"
    , "rjlipton.wordpress.com"
    , "rootsofprogress.org"
    , "row1.ca"
    , "ruder.io"
    , "safebooru.org"
    , "samoburja.com"
    , "scarybeastsecurity.blogspot.com"
    , "scholars-stage.org"
    , "schoolgirlmilkycrisis.com"
    , "science.ksc.nasa.gov"
    , "sciencebasedmedicine.org"
    , "sciencebulletin.org"
    , "scientistatwork.blogs.nytimes.com"
    , "scottaaronson.blog"
    , "senrigan.io"
    , "seths.blog"
    , "sevensecularsermons.org"
    , "sf-encyclopedia.com"
    , "shiny.app"
    , "shkspr.mobi"
    , "sifter.org"
    , "signalvnoise.com"
    , "slatestarcodex.com"
    , "slimemoldtimemold.com"
    , "socghop.appspot.com"
    , "socghop.appspot.com"
    , "socialsciences.mcmaster.ca"
    , "sociologicalscience.com"
    , "soranews24.com"
    , "spikejapan.wordpress.com"
    , "spp.fas.org"
    , "srconstantin.wordpress.com"
    , "sre.google"
    , "stackroboflow.com"
    , "statmodeling.stat.columbia.edu"
    , "statmodeling.stat.columbia.edu"
    , "stats.grok.se"
    , "status451.com"
    , "stephenmalina.com"
    , "stevenson.lab.uconn.edu"
    , "story.californiasunday.com"
    , "stratechery.com"
    , "strategy.wikimedia.org"
    , "svilentodorov.xyz"
    , "synapse.koreamed.org"
    , "takimag.com"
    , "taooftea.com"
    , "teageegeepea.tripod.com"
    , "texample.net"
    , "text.npr.org"
    , "the-liliquarium.livejournal.com"
    , "the-toast.net"
    , "thebaffler.com"
    , "thebrowser.com"
    , "thefirstaibook.com"
    , "thegrandnarrative.com"
    , "thehardestscience.com"
    , "thelastpsychiatrist.com"
    , "thenewstatistics.com"
    , "thisanimedoesnotexist.ai"
    , "thiscatdoesnotexist.com"
    , "thiseyedoesnotexist.com"
    , "thispersondoesnotexist.com"
    , "thisrentaldoesnotexist.com"
    , "thoughtcrime.crummy.com"
    , "threadreaderapp.com"
    , "time.com"
    , "tomcritchlow.com"
    , "tools.wmflabs.org"
    , "tosche.net"
    , "training.kalzumeus.com"
    , "transformer-circuits.pub"
    , "trixter.oldskool.org"
    , "tug.org"
    , "unenumerated.blogspot.com"
    , "unenumerated.blogspot.com"
    , "unqualified-reservations.blogspot.com"
    , "unsongbook.com"
    , "upload.wikimedia.org"
    , "vasilishynkarenka.com"
    , "vast.ai"
    , "vastabrupt.com"
    , "vdfresearch.org"
    , "venturebeat.com"
    , "vfxblog.com"
    , "videolectures.net"
    , "vinoshipper.com"
    , "vitalik.ca"
    , "w.atwiki.jp"
    , "warontherocks.com"
    , "washingtonmonthly.com"
    , "wavemotioncannon.com"
    , "wayback.archive-it.org"
    , "web.archive.org"
    , "web.media.mit.edu"
    , "web.stanford.edu"
    , "webcitation.org"
    , "well.blogs.nytimes.com"
    , "whyevolutionistrue.com"
    , "wiki.haskell.org"
    , "wiki.lesswrong.com"
    , "wiki.openttdcoop.org"
    , "wikimediafoundation.org"
    , "writeswith.com"
    , "writings.stephenwolfram.com"
    , "www-cs-faculty.stanford.edu"
    , "www.4nrx-uk.md"
    , "www.abcb.com"
    , "www.adamsmith.org"
    , "www.adelaidenow.com.au"
    , "www.aging-us.com"
    , "www.aiweirdness.com"
    , "www.alcor.org"
    , "www.aleph.se"
    , "www.alessonislearned.com"
    , "www.alexirpan.com"
    , "www.alicemaz.com"
    , "www.alignmentforum.org"
    , "www.allencheng.com"
    , "www.andzuck.com"
    , "www.antipope.org"
    , "www.archive-it.org"
    , "www.artnome.com"
    , "www.austlii.edu.au"
    , "www.baka-tsuki.org"
    , "www.baltimoresun.com"
    , "www.bartleby.com"
    , "www.becker-posner-blog.com"
    , "www.belfasttelegraph.co.uk"
    , "www.bemmu.com"
    , "www.benkuhn.net"
    , "www.blockchain.com"
    , "www.bostonglobe.com"
    , "www.brainpreservation.org"
    , "www.broadinstitute.org"
    , "www.brookings.edu"
    , "www.bu.edu"
    , "www.businessweek.com"
    , "www.buzzricksons.jp"
    , "www.c82.net"
    , "www.candyjapan.com"
    , "www.catalogtree.net"
    , "www.cato-unbound.org"
    , "www.cbsnews.com"
    , "www.cdc.gov"
    , "www.chicagotribune.com"
    , "www.chinadaily.com.cn"
    , "www.chrisplaysgames.com"
    , "www.chrisstucchio.com"
    , "www.cia.gov"
    , "www.cjas.org"
    , "www.cleveland.com"
    , "www.clinicaltrials.gov"
    , "www.cnn.com"
    , "www.cnn.com"
    , "www.coderelay.io"
    , "www.cogmed.com"
    , "www.cogtest.com"
    , "www.collectorsweekly.com"
    , "www.couriermail.com.au"
    , "www.coyneoftherealm.com"
    , "www.cram.com"
    , "www.cryonicscalculator.com"
    , "www.cs.dartmouth.edu"
    , "www.cs.odu.edu"
    , "www.cs.virginia.edu"
    , "www.culhwch.info"
    , "www.cylab.cmu.edu"
    , "www.daemonology.net"
    , "www.dafont.com"
    , "www.dagbladet.no"
    , "www.dailydot.com"
    , "www.dailymail.co.uk"
    , "www.dailymail.co.uk"
    , "www.dartmouth.edu"
    , "www.davidbordwell.net"
    , "www.davidbrin.com"
    , "www.davidsongifted.org"
    , "www.deeplearningbook.org"
    , "www.depauw.edu"
    , "www.deseret.com"
    , "www.discoverbooks.com"
    , "www.discoverteas.com"
    , "www.doc88.com"
    , "www.drmaciver.com"
    , "www.e-codices.unifr.ch"
    , "www.ecologyandsociety.org"
    , "www.econlib.org"
    , "www.economist.com"
    , "www.econtalk.org"
    , "www.eduref.net"
    , "www.edwardtufte.com"
    , "www.equator-network.org"
    , "www.equestriadaily.com"
    , "www.eugenewei.com"
    , "www.evacommentary.org"
    , "www.evamonkey.com"
    , "www.evaotaku.com"
    , "www.explainxkcd.com"
    , "www.fadedpage.com"
    , "www.fast.ai"
    , "www.fathomevents.com"
    , "www.fhi.ox.ac.uk"
    , "www.filfre.net"
    , "www.find-more-books.com"
    , "www.freakonomicsexperiments.com"
    , "www.frontiersin.org"
    , "www.ft.com"
    , "www.galbithink.org"
    , "www.gawker.com"
    , "www.genealogy.math.ndsu.nodak.edu"
    , "www.genetics.org"
    , "www.ghibli.jp"
    , "www.gizmodo.com.au"
    , "www.globalsecurity.org"
    , "www.gnxp.com"
    , "www.google-melange.com"
    , "www.gq.com"
    , "www.grandforksherald.com"
    , "www.greaterwrong.com"
    , "www.gutenberg.org"
    , "www.gwern.net"
    , "www.harrowell.org.uk"
    , "www.haskell.org"
    , "www.hbs.edu"
    , "www.heraldsun.com.au"
    , "www.hpmor.com"
    , "www.hsx.com"
    , "www.html-tidy.org"
    , "www.iarpa.gov"
    , "www.incompleteideas.net"
    , "www.independent.co.uk"
    , "www.independent.ie"
    , "www.infranken.de"
    , "www.inkandswitch.com"
    , "www.intechopen.com"
    , "www.iqtest.com"
    , "www.itmedia.co.jp"
    , "www.jamesfadiman.com"
    , "www.japaninc.com"
    , "www.japansociety.org"
    , "www.jdmoyer.com"
    , "www.jefftk.com"
    , "www.joelonsoftware.com"
    , "www.johndcook.com"
    , "www.johnsonessays.com"
    , "www.justinpinkney.com"
    , "www.kalzumeus.com"
    , "www.karger.com"
    , "www.khara.co.jp"
    , "www.kooslooijesteijn.net"
    , "www.koreatimes.co.kr"
    , "www.ledger-cli.org"
    , "www.lehighvalleylive.com"
    , "www.lesswrong.com"
    , "www.librarything.com"
    , "www.lifeview.com"
    , "www.links.org"
    , "www.livestrong.com"
    , "www.mail-archive.com"
    , "www.math.uwaterloo.ca"
    , "www.mcall.com"
    , "www.mediawiki.org"
    , "www.memteaimports.com"
    , "www.mentalfloss.com"
    , "www.mercurynews.com"
    , "ralphmerkle.com"
    , "www.metafor-project.org"
    , "www.metopera.org"
    , "www.metzdowd.com"
    , "www.miamiherald.com"
    , "www.michaellight.net"
    , "www.microdose.me"
    , "www.moserware.com"
    , "www.motherjones.com"
    , "www.nationalgeographic.com"
    , "www.nbcnews.com"
    , "www.newsday.com"
    , "www.newstatesman.com"
    , "www.nextbigfuture.com"
    , "www.nextplatform.com"
    , "www.noisebridge.net"
    , "www.nplusonemag.com"
    , "www.npr.org"
    , "www.nps.gov"
    , "www.nyaa.eu"
    , "www.nydailynews.com"
    , "www.ohyouprettythings.com"
    , "www.orbuch.com"
    , "www.oregonlive.com"
    , "www.oricon.co.jp"
    , "www.orlandosentinel.com"
    , "www.otakustudy.com"
    , "www.overcomingbias.com"
    , "www.owenstephens.co.uk"
    , "www.pcmag.com"
    , "www.pelleas.net"
    , "www.pewresearch.org"
    , "www.poetryfoundation.org"
    , "www.politico.com"
    , "www.popsci.com"
    , "www.pragmatic.ml"
    , "www.princeton.edu"
    , "www.project-imas.com"
    , "www.prolific.co"
    , "www.proquest.com"
    , "www.psychiatryinvestigation.org"
    , "www.punchlinedesign.net"
    , "www.r-bloggers.com"
    , "www.rationaloptimist.com"
    , "www.rdocumentation.org"
    , "www.reg.ru"
    , "www.reuters.com"
    , "www.rfreitas.com"
    , "www.ribbonfarm.com"
    , "www.richardcarrier.info"
    , "www.rifters.com"
    , "www.righto.com"
    , "www.rosebud.ai"
    , "www.rwagner.net"
    , "www.rxshop.md"
    , "www.sacbee.com"
    , "www.sacbee.com"
    , "www.salon.com"
    , "www.sankakucomplex.com"
    , "www.sankakucomplex.com"
    , "www.sapa-project.org"
    , "www.schneier.com"
    , "www.scielo.br"
    , "www.scienceagogo.com"
    , "www.sciencemadness.org"
    , "www.sciencenews.org"
    , "www.sciencenewsline.com"
    , "www.scottaaronson.com"
    , "www.sfgate.com"
    , "www.shawlocal.com"
    , "www.shawwn.com"
    , "www.siliconera.com"
    , "www.silverhandmeadery.com"
    , "www.simplify.so"
    , "www.sirlin.net"
    , "www.sjsu.edu"
    , "www.slate.com"
    , "www.sleep-journal.com"
    , "www.snpedia.com"
    , "www.spiegel.de"
    , "www.spiked-online.com"
    , "www.spring.org.uk"
    , "www.sqlite.org"
    , "www.ssc.wisc.edu"
    , "www.standard.co.uk"
    , "www.stat.columbia.edu"
    , "www.stat.columbia.edu"
    , "www.statnews.com"
    , "www.straighttalkonevidence.org"
    , "www.stripes.com"
    , "www.stuff.co.nz"
    , "www.sumsar.net"
    , "www.sun-modalert.com"
    , "www.supermemo.com"
    , "www.syracuse.com"
    , "www.systutorials.com"
    , "www.tabletmag.com"
    , "www.talyarkoni.org"
    , "www.tarsnap.com"
    , "www.technologyreview.com"
    , "www.the-scientist.com"
    , "www.thecut.com"
    , "www.thedenverchannel.com"
    , "www.thefreelibrary.com"
    , "www.theindiaforum.in"
    , "www.themtank.org"
    , "www.theparisreview.org"
    , "www.theregister.com"
    , "www.thesmokinggun.com"
    , "www.thestranger.com"
    , "www.thisfursonadoesnotexist.com"
    , "www.thispersondoesnotexist.com"
    , "www.thiswaifudoesnotexist.net"
    , "www.thisworddoesnotexist.com"
    , "www.thriftbooks.com"
    , "www.tlmc.eu"
    , "www.tomodachi.de"
    , "www.tomshardware.com"
    , "www.tor.com"
    , "www.tranquiltuesdays.com"
    , "www.trentonbricken.com"
    , "www.trubrain.com"
    , "www.tt-forums.net"
    , "www.ubu.com"
    , "www.uk-anime.net"
    , "www.uliwestphal.de"
    , "www.unc.edu"
    , "www.unf.edu"
    , "www.unicode.org"
    , "www.unqualified-reservations.org"
    , "www.unz.com"
    , "www.urbandharma.org"
    , "www.urbandictionary.com"
    , "www.usnews.com"
    , "www.vanityfair.com"
    , "www.vesta.earth"
    , "www.vetta.org"
    , "www.vocativ.com"
    , "www.w3.org"
    , "www.wakapoetry.net"
    , "www.washingtonpost.com"
    , "www.washingtontimes.com"
    , "www.webmd.com"
    , "www.wesjones.com"
    , "www.wheels.org"
    , "www.whichfaceisreal.com"
    , "www.winehq.org"
    , "www.wired.co.uk"
    , "www.wired.com"
    , "www.wolfewiki.com"
    , "www.writingroutines.com"
    , "www.wsj.com"
    , "www.wtnh.com"
    , "www.yalelawjournal.org"
    , "www.yf.io"
    , "www.youtube.com"
    , "www.yudkowsky.net"
    , "www.zeit.de"
    , "www2.biology.ualberta.ca"
    , "xkcd.com"
    , "xtools.wmflabs.org"
    , "zlkj.in"
    , "www.cs.toronto.edu"
    ]

badDomainsSub = [".plos.org", ".royalsocietypublishing.org",  ".substack.com", ".stackexchange.com",
                  ".oxfordjournals.org", ".medium.com", ".translate.goog"]
badDomainsSimple = [ "2chan.us"
   , "5onwnspjvuk7cwvk.tor2web.org"
   , "a16z.com"
   , "about.netflix.com"
   , "adage.com"
   , "adcohearing.com"
   , "addons.mozilla.org"
   , "advancedfertility.com"
   , "aeon.co"
   , "ai-alignment.com"
   , "aidungeon.medium.com"
   , "ajp.psychiatryonline.org"
   , "ajph.aphapublications.org"
   , "aka.ms"
   , "almanac.httparchive.org"
   , "amstat.tandfonline.com"
   , "andrewbadr.com"
   , "animesuperhero.com"
   , "answers.google.com"
   , "antiagingcentral.com"
   , "apastyle.apa.org"
   , "api.research-repository.uwa.edu.au"
   , "apps.ankiweb.net"
   , "apps.apple.com"
   , "apps.dtic.mil"
   , "ar5iv.labs.arxiv.org"
   , "arbital.com"
   , "archinte.ama-assn.org"
   , "archive-ouverte.unige.ch"
   , "archive.ilr.cornell.edu"
   , "archiveofourown.org"
   , "archives.haskell.org"
   , "ascii.textfiles.com"
   , "ase.uva.nl"
   , "astralcodexten.substack.com"
   , "au.news.yahoo.com"
   , "augmentingcognition.com"
   , "authenticorganizations.com"
   , "aws.amazon.com"
   , "azure.microsoft.com"
   , "bactra.org"
   , "bartokdesign.com"
   , "bastiat.org"
   , "bayes.wustl.edu"
   , "beautifultaiwantea.com"
   , "beerconnoisseur.com"
   , "bellroy.com"
   , "benbest.com"
   , "berkshirehathaway.com"
   , "beta.openai.com"
   , "betsofbitco.in"
   , "betterhumans.coach.me"
   , "bgr.com"
   , "bifunctor.homelinux.net"
   , "bioinfo.pl"
   , "bitbucket.org"
   , "bitcoincharts.com"
   , "bitcoins-code.de"
   , "bitcointalk.org"
   , "bitcoinweekly.com"
   , "bjo.bmj.com"
   , "bjp.rcpsych.org"
   , "bjsm.bmj.com"
   , "blockchain.info"
   , "blog.23andme.com"
   , "blog.dansimons.com"
   , "blog.darcs.net"
   , "blog.darknedgy.net"
   , "blog.jaibot.com"
   , "blog.johantibell.com"
   , "blog.ml.cmu.edu"
   , "blog.mozilla.org"
   , "blog.nuclino.com"
   , "blog.platypope.org"
   , "blog.sigfpe.com"
   , "blog.torproject.org"
   , "blog.twitter.com"
   , "blogs.microsoft.com"
   , "blogs.nature.com"
   , "blogs.princeton.edu"
   , "blogs.scientificamerican.com"
   , "bluelight.org"
   , "bmcmedresmethodol.biomedcentral.com"
   , "bmcneurosci.biomedcentral.com"
   , "bmcpublichealth.biomedcentral.com"
   , "bmcvetres.biomedcentral.com"
   , "boa.unimib.it"
   , "boardgamegeek.com"
   , "boinngerionn.blog.fc2.com"
   , "book.webtypography.net"
   , "boston.conman.org"
   , "brainworkshop.sourceforge.net"
   , "bsapubs.onlinelibrary.wiley.com"
   , "buddhism-for-vampires.com"
   , "bugs.darcs.net"
   , "bugs.debian.org"
   , "bugs.launchpad.net"
   , "buttercupfestival.com"
   , "cabinetmagazine.org"
   , "caes.ucdavis.edu"
   , "calgarysun.com"
   , "camelcamelcamel.com"
   , "capitalteas.com"
   , "capsuleconnection.com"
   , "case.edu"
   , "casetext.com"
   , "catb.org"
   , "catless.ncl.ac.uk"
   , "cep.lse.ac.uk"
   , "cfa.org"
   , "chem.tufts.edu"
   , "chrome.google.com"
   , "chroniclingamerica.loc.gov"
   , "chronopause.com"
   , "circ.ahajournals.org"
   , "cis.org"
   , "clagnut.com"
   , "clarkesworldmagazine.com"
   , "clinicalepigeneticsjournal.biomedcentral.com"
   , "cloud.google.com"
   , "code.google.com"
   , "code.visualstudio.com"
   , "colab.research.google.com"
   , "collections.library.yale.edu"
   , "collider.com"
   , "comicsalliance.com"
   , "community.arm.com"
   , "community.haskell.org"
   , "console.aws.amazon.com"
   , "console.cloud.google.com"
   , "corante.com"
   , "core.ac.uk"
   , "cosmosmagazine.com"
   , "course.fast.ai"
   , "coveryourtracks.eff.org"
   , "cro.sagepub.com"
   , "crypto.stackexchange.com"
   , "cse.google.com"
   , "csimq-journals.rtu.lv"
   , "csrc.nist.gov"
   , "ctc.usma.edu"
   , "cyber.harvard.edu"
   , "dailyvoice.com"
   , "dalspace.library.dal.ca"
   , "danfrank.ca"
   , "darcs.net"
   , "data.worldbank.org"
   , "datasetsearch.research.google.com"
   , "deadline.com"
   , "deadspin.com"
   , "dee.su"
   , "dept.wofford.edu"
   , "devblogs.microsoft.com"
   , "developer.nvidia.com"
   , "developer.twitter.com"
   , "dharmaoverground.org"
   , "digitalcommons.unl.edu"
   , "disease-connect.org"
   , "dl.acm.org"
   , "dl.dropboxusercontent.com"
   , "dnd.wizards.com"
   , "docs.microsoft.com"
   , "donate.torproject.org"
   , "drugs-forum.com"
   , "dual-n-back.com"
   , "duckduckgo.com"
   , "duplicity.nongnu.org"
   , "e621.net"
   , "eab.sagepub.com"
   , "ec.toranoana.jp"
   , "econ.vt.edu"
   , "economics.stanford.edu"
   , "eepurl.com"
   , "eg-2.hatenablog.com"
   , "ehp.niehs.nih.gov"
   , "ehp.niehs.nih.gov"
   , "eiga.com"
   , "ejlt.org"
   , "eml.berkeley.edu"
   , "en.chessbase.com"
   , "en.wik%20ipedia.org"
   , "en.wikip%20edia.org"
   , "eng.uber.com"
   , "environhealthprevmed.biomedcentral.com"
   , "eprints.lincoln.ac.uk"
   , "eprints.nottingham.ac.uk"
   , "eprints.whiterose.ac.uk"
   , "ergodox-ez.com"
   , "ero.sagepub.com"
   , "esajournals.onlinelibrary.wiley.com"
   , "escholarship.org"
   , "espace.library.uq.edu.au"
   , "esr.ibiblio.org"
   , "eth.wiki"
   , "ethics.emory.edu"
   , "ethos.bl.uk"
   , "eurfa.org.uk"
   , "evacommentary.org"
   , "every.to"
   , "everyoneishappy.com"
   , "everything2.com"
   , "evigio.com"
   , "exrx.net"
   , "eztest.com"
   , "faculty.virginia.edu"
   , "fdaaa.trialstracker.net"
   , "finance.yahoo.com"
   , "fivethirtyeight.com"
   , "floatingleaves.com"
   , "fn.bmj.com"
   , "fnb.sagepub.com"
   , "fontsinuse.com"
   , "food52.com"
   , "fortune.com"
   , "forum.bitcoin.org"
   , "forums.somethingawful.com"
   , "freedom-to-tinker.com"
   , "ftp.cs.ucla.edu"
   , "fursona.app"
   , "gainax.co.jp"
   , "gainax.fr"
   , "garote.bdmonkeys.net"
   , "gazette.com"
   , "gcta.freeforums.net"
   , "geneatlas.roslin.ed.ac.uk"
   , "genepi.qimr.edu.au"
   , "geneticalliance.org.uk"
   , "genius.com"
   , "genomebiology.biomedcentral.com"
   , "genomemedicine.biomedcentral.com"
   , "georgianjournal.ge"
   , "geroprotectors.org"
   , "getd.libs.uga.edu"
   , "gettermario.dynamicforum.net"
   , "gigaom.com"
   , "gigascience.biomedcentral.com"
   , "gigazine.net"
   , "gist.github.com"
   , "git-scm.com"
   , "git.io"
   , "gitgud.io"
   , "gitlab.developers.cam.ac.uk"
   , "gitlab.haskell.org"
   , "gitlab.torproject.org"
   , "gizmodo.com"
   , "glench.com"
   , "godanriver.com"
   , "granta.com"
   , "grantland.com"
   , "greekreporter.com"
   , "groups.yahoo.com"
   , "gsejournal.biomedcentral.com"
   , "h01-dot-neuroglancer-demo.appspot.com"
   , "habr.com"
   , "hacks.mozilla.org"
   , "hal.archives-ouvertes.fr"
   , "handbook.cochrane.org"
   , "hansard.parliament.uk"
   , "hardcoresoftware.learningbyshipping.com"
   , "harmreductionjournal.biomedcentral.com"
   , "hashcat.net"
   , "hazuma.hatenablog.com"
   , "hbr.org"
   , "hcommons.org"
   , "help.duckduckgo.com"
   , "help.myspace.com"
   , "history.nasa.gov"
   , "hivemind-repo.s3-us-west-2.amazonaws.com"
   , "hivemind-repo.s3-us-west-2.amazonaws.com"
   , "hivemind-repo.s3-us-west-2.amazonaws.com"
   , "hn-sicp.pbworks.com"
   , "hoaxes.org"
   , "hollisarchives.lib.harvard.edu"
   , "home.inklingmarkets.com"
   , "homepage3.nifty.com"
   , "hoogle.haskell.org"
   , "host.robots.ox.ac.uk"
   , "hplusmagazine.com"
   , "hrcak.srce.hr"
   , "hum.sagepub.com"
   , "i.imgur.com"
   , "icosahedron.website"
   , "ide.mit.edu"
   , "ideas.4brad.com"
   , "ieeexplore.ieee.org"
   , "if50.substack.com"
   , "ignorethecode.net"
   , "ije.oxfordjournals.org"
   , "imagelibrary.bgu.ac.il"
   , "images.google.com"
   , "inews.co.uk"
   , "infidels.org"
   , "informallogic.ca"
   , "interviews.slashdot.org"
   , "intrade.com"
   , "iopscience.iop.org"
   , "irs.princeton.edu"
   , "it.slashdot.org"
   , "iterative.capital"
   , "itre.cis.upenn.edu"
   , "jacobsschool.ucsd.edu"
   , "jalopnik.com"
   , "jamestown.org"
   , "japanintercultural.com"
   , "jasbsci.biomedcentral.com"
   , "jcsm.aasm.org"
   , "jidinews.com"
   , "jigsaw.w3.org"
   , "jme.bmj.com"
   , "jn.nutrition.org"
   , "jnnp.bmj.com"
   , "johakyu.net"
   , "jotengine.com"
   , "journals.ametsoc.org"
   , "journals.lww.com"
   , "journals.physiology.org"
   , "journaltimes.com"
   , "jpet.aspetjournals.org"
   , "jukebox.openai.com"
   , "justpaste.it"
   , "jyllands-posten.dk"
   , "kanzaki.sub.jp"
   , "kettl.co"
   , "kilthub.cmu.edu"
   , "knowyourmeme.com"
   , "kotaku.com"
   , "kyunghyuncho.me"
   , "lair.lighton.ai"
   , "lareviewofbooks.org"
   , "latanyasweeney.org"
   , "latitude.io"
   , "ldsc.broadinstitute.org"
   , "leaps.org"
   , "learn.adafruit.com"
   , "leme.me"
   , "letterformarchive.org"
   , "letters.temporarystate.net"
   , "lichess.org"
   , "linuxmafia.com"
   , "lists.wikimedia.org"
   , "listserv.brown.edu"
   , "lizadaly.com"
   , "lobste.rs"
   , "longnow.org"
   , "lostpinesyaupontea.com"
   , "ludix.com"
   , "lukemuehlhauser.com"
   , "machinelearning.apple.com"
   , "mangans.blogspot.com"
   , "marco.org"
   , "markets.nitle.org"
   , "martinottaway.com"
   , "math.stackexchange.com"
   , "mathoverflow.net"
   , "mattsclancy.substack.com"
   , "mbio.asm.org"
   , "meaningness.com"
   , "mecha-guignol.com"
   , "media.ccc.de"
   , "media.githubusercontent.com"
   , "media.nature.com"
   , "medicine.osu.edu"
   , "medieval.bodleian.ox.ac.uk"
   , "medlineplus.gov"
   , "medsforbitcoin.com"
   , "meehl.umn.edu"
   , "messybeast.com"
   , "metacpan.org"
   , "metrics.torproject.org"
   , "millercenter.org"
   , "money.cnn.com"
   , "motherboard.vice.com"
   , "moz.com"
   , "mpra.ub.uni-muenchen.de"
   , "mujoco.org"
   , "mymodafinil.net"
   , "mysbfiles.stonybrook.edu"
   , "n.neurology.org"
   , "naldc.nal.usda.gov"
   , "nansenundpiccard.de"
   , "nationalinterest.org"
   , "nces.ed.gov"
   , "nearcyan.com"
   , "nebia.com"
   , "nectarcreek.com"
   , "neomarxisme.com"
   , "new.cognitivefun.net"
   , "new.nubrain.com"
   , "newhumanist.org.uk"
   , "newrepublic.com"
   , "news.bbc.co.uk"
   , "news.gallup.com"
   , "news.harvard.edu"
   , "news.mit.edu"
   , "news.samsung.com"
   , "news.slashdot.org"
   , "news.yahoo.com"
   , "news.ycombinator.com"
   , "newsroom.collegeboard.org"
   , "nitro.biosci.arizona.edu"
   , "nitter.hu"
   , "nmteaco.com"
   , "nori.com"
   , "noscript.net"
   , "npc.people.com.cn"
   , "nrl.northumbria.ac.uk"
   , "nutritionj.biomedcentral.com"
   , "nypost.com"
   , "ods.od.nih.gov"
   , "oll.libertyfund.org"
   , "omdia.tech.informa.com"
   , "omega.albany.edu:8008"
   , "omega0.xyz"
   , "openaipublic.blob.core.windows.net"
   , "openscience.bmj.com"
   , "opensnp.org"
   , "opensource.org"
   , "orbit.dtu.dk"
   , "osnadocs.ub.uni-osnabrueck.de"
   , "otago.ourarchive.ac.nz"
   , "otakumode.com"
   , "ourstarblazers.com"
   , "p2pfoundation.ning.com"
   , "packages.debian.org"
   , "pages.jh.edu"
   , "pain.wustl.edu"
   , "papers.nips.cc"
   , "par.nsf.gov"
   , "partner.steamgames.com"
   , "paste.laravel.io"
   , "pastebin.com"
   , "patch-tag.com"
   , "patents.google.com"
   , "pay.reddit.com"
   , "pcpartpicker.com"
   , "people.tamu.edu"
   , "permalink.gmane.org"
   , "phabricator.wikimedia.org"
   , "pharmrev.aspetjournals.org"
   , "philsci-archive.pitt.edu"
   , "physics.aps.org"
   , "physoc.onlinelibrary.wiley.com"
   , "pingpong.ki.se"
   , "pioneer.app"
   , "pirate.london"
   , "pixelpoppers.com"
   , "pjreddie.com"
   , "play.google.com"
   , "player.vimeo.com"
   , "plaza.harmonix.ne.jp"
   , "plewis.info"
   , "plus.google.com"
   , "pni.princeton.edu"
   , "pol.is"
   , "polisen.se"
   , "politicalscience.osu.edu"
   , "pone.dev"
   , "popsych.org"
   , "poststar.com"
   , "pps.sagepub.com"
   , "precedings.nature.com"
   , "priceonomics.com"
   , "prisons.org.uk"
   , "prize.hutter1.net"
   , "proceedings.neurips.cc"
   , "project-rainbowcrack.com"
   , "psi.sagepub.com"
   , "psmag.com"
   , "pss.sagepub.com"
   , "psych.wustl.edu"
   , "psychology.gatech.edu"
   , "psycnet.apa.org"
   , "psycnet.apa.org"
   , "public.tableau.com"
   , "pubmed.ncbi.nlm.nih.gov"
   , "pubs.acs.org"
   , "pubs.aeaweb.org"
   , "pubsonline.informs.org"
   , "pulitzercenter.org"
   , "pure.au.dk"
   , "pure.tue.nl"
   , "pure.uva.nl"
   , "pure.uvt.nl"
   , "purse.io"
   , "qa.debian.org"
   , "qualitysafety.bmj.com"
   , "quoteinvestigator.com"
   , "qz.com"
   , "rachelbythebay.com"
   , "radar.oreilly.com"
   , "radiopublic.com"
   , "raw.githubusercontent.com"
   , "rbej.biomedcentral.com"
   , "rd.springer.com"
   , "reason.com"
   , "repec.org"
   , "repository.si.edu"
   , "repository.uel.ac.uk"
   , "repository.upenn.edu"
   , "research.facebook.com"
   , "research.vu.nl"
   , "reset.me"
   , "risk-engineering.org"
   , "ro.ecu.edu.au"
   , "royalsocietypublishing.org"
   , "rss.onlinelibrary.wiley.com"
   , "runrepeat.com"
   , "samsaffron.com"
   , "scholar.harvard.edu"
   , "science.nasa.gov"
   , "science.sciencemag.org"
   , "scp-wiki.wikidot.com"
   , "searchengineland.com"
   , "secretlaboratory.org"
   , "secure.flickr.com"
   , "sembr.org"
   , "sensebridge.net"
   , "sgo.sagepub.com"
   , "siepr.stanford.edu"
   , "sigbovik.org"
   , "sites.google.com"
   , "sites.google.com"
   , "sites.research.google"
   , "sive.rs"
   , "skeb.jp"
   , "skeptics.stackexchange.com"
   , "skift.com"
   , "skymind.ai"
   , "sparky.haskell.org"
   , "spectrum.ieee.org"
   , "spectrum.library.concordia.ca"
   , "spreadsheets.google.com"
   , "ssgac.org"
   , "sss.sagepub.com"
   , "static-content.springer.com"
   , "stats.stackexchange.com"
   , "stats.stackexchange.com"
   , "steamcommunity.com"
   , "stevecoast.com"
   , "store.steampowered.com"
   , "streamable.com"
   , "stripe.com"
   , "stroke.ahajournals.org"
   , "studenttheses.universiteitleiden.nl"
   , "subterraneanpress.com"
   , "suffolk.onthehub.com"
   , "summerofcode.withgoogle.com"
   , "sundhedsdatastyrelsen.dk"
   , "super-memory.com"
   , "super.gluebenchmark.com"
   , "superuser.com"
   , "support.google.com"
   , "support.mozilla.org"
   , "support.torproject.org"
   , "swombat.com"
   , "t.ly"
   , "t.me"
   , "tails.boum.org"
   , "taylorandfrancis.com"
   , "teahabitat.com"
   , "tealet.com"
   , "techcrunch.com"
   , "tfhub.dev"
   , "thecatsite.com"
   , "thechineseteashop.com"
   , "thecodelesscode.com"
   , "theconversation.com"
   , "thecorrespondent.com"
   , "theeagle.com"
   , "theintercept.com"
   , "themorningnews.org"
   , "thenextweb.com"
   , "thepharmacyexpress.com"
   , "thesession.org"
   , "thesocietypages.org"
   , "thetakeout.com"
   , "thewebconf.org"
   , "thomasbarker.com"
   , "thoughtbot.com"
   , "tigersophia.blogspot.com"
   , "timarit.is"
   , "timesofindia.indiatimes.com"
   , "tinyurl.com"
   , "tobaccocontrol.bmj.com"
   , "today.yougov.com"
   , "top500.org"
   , "torch.ch"
   , "towardsdatascience.com"
   , "trialsjournal.biomedcentral.com"
   , "tribune.com.pk"
   , "truewetsuits.jp"
   , "tuts4you.com"
   , "tweelingenregister.vu.nl"
   , "type-r.hatenablog.com"
   , "ubc-emotionlab.ca"
   , "uberty.org"
   , "uitspraken.rechtspraak.nl"
   , "uk.pi-supply.com"
   , "umichrl.pbworks.com"
   , "undark.org"
   , "understandinguncertainty.org"
   , "us.dantelabs.com"
   , "vanishingpoint.air-nifty.com"
   , "variety.com"
   , "vault.si.com"
   , "vgl.ucdavis.edu"
   , "vgmdb.net"
   , "vinecon.ucdavis.edu"
   , "vividness.live"
   , "vk.com"
   , "vndb.org"
   , "vosswater.com"
   , "voxday.blogspot.com"
   , "wandb.ai"
   , "warosu.org"
   , "wcfcourier.com"
   , "wci.llnl.gov"
   , "web-archive-org.translate.goog"
   , "web-japan.org"
   , "web.econ.ku.dk"
   , "web.maths.unsw.edu.au"
   , "web.mit.edu"
   , "whatis.suburbansenshi.com"
   , "wiki.evageeks.org"
   , "wordpress.org"
   , "works.bepress.com"
   , "wudao.aminer.cn"
   , "ww2.arb.ca.gov"
   , "www-biba.inrialpes.fr"
   , "www.1001fonts.com"
   , "www.1m.co"
   , "www.aaai.org"
   , "www.aaronsw.com"
   , "www.abbiotec.com"
   , "www.abc.net.au"
   , "www.abc10.com"
   , "www.abebooks.com"
   , "www.abetterpage.com"
   , "www.academia.edu"
   , "www.acpjournals.org"
   , "www.actionnewsnow.com"
   , "www.adafruit.com"
   , "www.advrider.com"
   , "www.aeaweb.org"
   , "www.aei.org"
   , "www.aera.net"
   , "www.afp.gov.au"
   , "www.afr.com"
   , "www.ahajournals.org"
   , "www.aiwriter.email"
   , "www.ajmadison.com"
   , "www.ajnr.org"
   , "www.alchemistowl.org"
   , "www.alljapaneseallthetime.com"
   , "www.alphagomovie.com"
   , "www.alternatehistory.com"
   , "www.alzchem.com"
   , "www.alzforum.org"
   , "www.ama-assn.org"
   , "www.amazon.co.jp"
   , "www.amazon.com"
   , "www.ams.org"
   , "www.anandtech.com"
   , "www.angelfire.com"
   , "www.anime-planet.com"
   , "www.animeigo.com"
   , "www.animenewsservice.com"
   , "www.annualreviews.org"
   , "www.antonhowes.com"
   , "www.apa.org"
   , "www.apa.org"
   , "www.appbrain.com"
   , "www.arkansasonline.com"
   , "www.arknights.global"
   , "www.army.mil"
   , "www.artbreeder.com"
   , "www.artofmanliness.com"
   , "www.arxiv-vanity.com"
   , "www.atlasobscura.com"
   , "www.atsjournals.org"
   , "www.avalonmagicplants.com"
   , "www.avclub.com"
   , "www.awe.gov.au"
   , "www.backblaze.com"
   , "www.baen.com"
   , "www.baltcoffee.com"
   , "www.barnesandnoble.com"
   , "www.barnstormjournal.org"
   , "www.bartokdesign.com"
   , "www.bayesianinvestor.com"
   , "www.beelinereader.com"
   , "www.behance.net"
   , "www.belfastlive.co.uk"
   , "www.ben-evans.com"
   , "www.berkshireeagle.com"
   , "www.berkshirehathaway.com"
   , "www.berlin.de"
   , "www.betterworldbooks.com"
   , "www.bfi.org"
   , "www.bfmtv.com"
   , "www.bioworld.com"
   , "www.birminghammail.co.uk"
   , "www.bizjournals.com"
   , "www.blender.org"
   , "www.bnlearn.com"
   , "www.brandonsanderson.com"
   , "www.bronxbanterblog.com"
   , "www.buffalonews.com"
   , "www.bulletproof.com"
   , "www.businessinsider.com"
   , "www.businessinsider.jp"
   , "www.businesswire.com"
   , "www.buttercupfestival.com"
   , "www.buzzfeed.com"
   , "www.buzzfeednews.com"
   , "www.bvp.com"
   , "www.byrnehobart.com"
   , "www.cabinetmagazine.org"
   , "www.calnewport.com"
   , "www.cambridge.org"
   , "www.cambridgebrainsciences.com"
   , "www.campbellrivermirror.com"
   , "www.cancer.gov"
   , "www.cancerresearchuk.org"
   , "www.cap-lore.com"
   , "www.carolinacoastonline.com"
   , "www.cato.org"
   , "www.cebm.ox.ac.uk"
   , "www.cehd.umn.edu"
   , "www.cell.com"
   , "www.census.gov"
   , "www.change.org"
   , "www.channel4.com"
   , "www.chathamstartribune.com"
   , "www.chemistryworld.com"
   , "www.chess.com"
   , "www.chessclub.com"
   , "www.chicagobooth.edu"
   , "www.chinafile.com"
   , "www.chronicle.com"
   , "www.churchofjesuschrist.org"
   , "www.cisco.com"
   , "www.citizenaudit.org"
   , "www.city-journal.org"
   , "www.clippershipteaco.com"
   , "www.clubindustry.com"
   , "www.cmajopen.ca"
   , "www.cmu.edu"
   , "www.cnbc.com"
   , "www.cnet.com"
   , "www.cochranelibrary.com"
   , "www.codespaces.com"
   , "www.collisiondetection.net"
   , "www.comicbox.co.jp"
   , "www.comicconnect.com"
   , "www.computerworld.com"
   , "www.consumerlab.com"
   , "www.copenhagenconsensus.com"
   , "www.copyright.gov"
   , "www.counterpunch.org"
   , "www.coursera.org"
   , "www.courtlistener.com"
   , "www.courts.mo.gov"
   , "www.courts.sa.gov.au"
   , "www.cracked.com"
   , "www.crd.york.ac.uk"
   , "www.cs.cornell.edu"
   , "www.cs.purdue.edu"
   , "www.cs.utexas.edu"
   , "www.cs.york.ac.uk"
   , "www.csail.mit.edu"
   , "www.csub.edu"
   , "www.dailyfinance.com"
   , "www.dailystar.co.uk"
   , "www.dantelabs.com"
   , "www.darkowl.com"
   , "www.dcscience.net"
   , "www.dea.gov"
   , "www.delawareonline.com"
   , "www.designboom.com"
   , "www.detectiveconanworld.com"
   , "www.devever.net"
   , "www.deviantart.com"
   , "www.dharmaoverground.org"
   , "www.dichtbij.nl"
   , "www.discovermagazine.com"
   , "www.ditext.com"
   , "www.dobuusagi.com"
   , "www.dr.dk"
   , "www.dropbox.com"
   , "www.drugs.com"
   , "www.drugsdata.org"
   , "www.dtic.mil"
   , "www.dummy-system.com"
   , "www.duolingo.com"
   , "www.e-sanitas.edu.co"
   , "www.ebay.co.uk"
   , "www.ebay.com"
   , "www.ecns.cn"
   , "www.ed.ac.uk"
   , "www.eetimes.com"
   , "www.effectuation.org"
   , "www.elastic.co"
   , "www.elon.edu"
   , "www.emacswiki.org"
   , "www.emcdda.europa.eu"
   , "www.energy.gov"
   , "www.engadget.com"
   , "www.english.upenn.edu"
   , "www.enworld.org"
   , "www.epjournal.net"
   , "www.equilibretechnologies.com"
   , "www.eric.ed.gov"
   , "www.escholar.manchester.ac.uk"
   , "www.esquire.com"
   , "www.eurekalert.org"
   , "www.eurojust.europa.eu"
   , "www.eusprig.org"
   , "www.evalegend.com"
   , "www.ex.org"
   , "www.expert-reviews.com"
   , "www.express.co.uk"
   , "www.expressandstar.com"
   , "www.facebook.com"
   , "www.faqs.org"
   , "www.fasebj.org"
   , "www.fastcompany.com"
   , "www.fda.gov"
   , "www.ff7citadel.com"
   , "www.fightaging.org"
   , "www.fimfiction.net"
   , "www.fincen.gov"
   , "www.findagrave.com"
   , "www.firstthings.com"
   , "www.flashback.org"
   , "www.flashgamehistory.com"
   , "www.flickr.com"
   , "www.foliosociety.com"
   , "www.fool.com"
   , "www.forbes.com"
   , "www.fordfoundation.org"
   , "www.foreignaffairs.com"
   , "www.fox6now.com"
   , "www.foxcarolina.com"
   , "www.foxnews.com"
   , "www.frbsf.org"
   , "www.frc.ri.cmu.edu"
   , "www.freehaven.net"
   , "www.fs.fed.us"
   , "www.fsigenetics.com"
   , "www.ftc.gov"
   , "www.garda.ie"
   , "www.genome.gov"
   , "www.genwaybio.com"
   , "www.getlamp.com"
   , "www.girlschase.com"
   , "www.girlscouts.org"
   , "www.global.toshiba"
   , "www.globaltimes.cn"
   , "www.gloucestershirelive.co.uk"
   , "www.gnu.org"
   , "www.goodtherapy.org"
   , "www.google.com"
   , "www.goproblems.com"
   , "www.gov.uk"
   , "www.greenexercise.org"
   , "www.gsb.stanford.edu"
   , "www.guilford.com"
   , "www.hakalalabs.com"
   , "www.hamilton.edu"
   , "www.hanselman.com"
   , "www.harvardmagazine.com"
   , "www.heraldnews.com"
   , "www.highbeam.com"
   , "www.highflightfoundation.org"
   , "www.hindawi.com"
   , "www.hindustantimes.com"
   , "www.history.com"
   , "www.historytoday.com"
   , "www.holidaymead.com"
   , "www.holidaymead.com"
   , "www.hoover.org"
   , "www.hrw.org"
   , "www.htrnews.com"
   , "www.huffpost.com"
   , "www.ibtimes.co.uk"
   , "www.ice.gov"
   , "www.idnes.cz"
   , "www.ietf.org"
   , "www.iflscience.com"
   , "www.iherb.com"
   , "www.illumina.com"
   , "www.imagesco.com"
   , "www.imf.org"
   , "www.imminst.org"
   , "www.inc.com"
   , "www.indiana.edu"
   , "www.industrydocuments.ucsf.edu"
   , "www.infinitychess.com"
   , "www.infinityplus.co.uk"
   , "www.infoplease.com"
   , "www.informit.com"
   , "www.inputmag.com"
   , "www.insidehighered.com"
   , "www.instagram.com"
   , "www.instructables.com"
   , "www.intel.com"
   , "www.iqout.com"
   , "www.irishcentral.com"
   , "www.irishexaminer.com"
   , "www.irishtimes.com"
   , "www.irrodl.org"
   , "www.isfdb.org"
   , "www.isteve.com"
   , "www.ivfbabble.com"
   , "www.jacc.org"
   , "www.jackkinsella.ie"
   , "www.jacurutu.com"
   , "www.jameslindlibrary.org"
   , "www.janelia.org"
   , "www.japantimes.co.jp"
   , "www.jetbrains.com"
   , "www.joshdean.com"
   , "www.journalnow.com"
   , "www.jstage.jst.go.jp"
   , "www.jstatsoft.org"
   , "www.jstor.org"
   , "www.jstor.org"
   , "www.jstor.org"
   , "www.justice.gov"
   , "www.jwz.org"
   , "www.kadokawa.co.jp"
   , "www.kctv5.com"
   , "www.kgw.com"
   , "www.kokos.cz"
   , "www.kptv.com"
   , "www.l-iz.de"
   , "www.labone.tech"
   , "www.lanl.gov"
   , "www.laphamsquarterly.org"
   , "www.latimes.com"
   , "www.law.cornell.edu"
   , "www.lef.org"
   , "www.lemonde.fr"
   , "www.leontiadis.info"
   , "www.liebertpub.com"
   , "www.lightspeedmagazine.com"
   , "www.limeadery.com"
   , "www.linode.com"
   , "www.loc.gov"
   , "www.locusmag.com"
   , "www.longecity.org"
   , "www.longevityhistory.com"
   , "www.loudountimes.com"
   , "www.lrb.co.uk"
   , "www.lshtm.ac.uk"
   , "www.lyrn.ai"
   , "www.maa.org"
   , "www.macrumors.com"
   , "www.madboa.com"
   , "www.manchestereveningnews.co.uk"
   , "www.mangauk.com"
   , "www.mangaupdates.com"
   , "www.mania.com"
   , "www.mansfieldnewsjournal.com"
   , "www.marxists.org"
   , "www.mathematica.org"
   , "www.mayoclinic.org"
   , "www.mayoclinicproceedings.org"
   , "www.mayofamily.com"
   , "www.mcsweeneys.net"
   , "www.mediafire.com"
   , "www.medicaldaily.com"
   , "www.medicines.org.uk"
   , "www.megaverse.info"
   , "www.mercatus.org"
   , "www.mesacc.edu"
   , "www.mha.gov.sg"
   , "www.mhlw.go.jp"
   , "www.microsoft.com"
   , "www.millionshort.com"
   , "www.mindsparke.com"
   , "www.mining.com"
   , "www.mirror.co.uk"
   , "www.mit.edu"
   , "www.mja.com.au"
   , "www.mobihealthnews.com"
   , "www.morinaga.co.jp"
   , "www.msri.org"
   , "www.muckrock.com"
   , "www.myfonts.com"
   , "www.nap.edu"
   , "www.nationaldefensemagazine.org"
   , "www.nato.int"
   , "www.nausicaa.net"
   , "www.nba.com"
   , "www.nber.org"
   , "www.nbr.co.nz"
   , "www.ncbi.nlm.nih.gov"
   , "www.ncbi.nlm.nih.gov"
   , "www.ndss-symposium.org"
   , "www.nearlyfreespeech.net"
   , "www.neuroscience.cam.ac.uk"
   , "www.newadvent.org"
   , "www.newegg.com"
   , "www.newsandstar.co.uk"
   , "www.newscientist.com"
   , "www.newsweek.com"
   , "www.newyorkfed.org"
   , "www.nextnewdeal.net"
   , "www.nicovideo.jp"
   , "www.nicvape.com"
   , "www.nimh.nih.gov"
   , "www.nitrd.gov"
   , "www.nlsinfo.org"
   , "www.nngroup.com"
   , "www.nobelprize.org"
   , "www.nola.com"
   , "www.northjersey.com"
   , "www.northwestgeorgianews.com"
   , "www.notion.so"
   , "www.nottinghampost.com"
   , "www.nrdc.org"
   , "www.nsa.gov"
   , "www.ntticc.or.jp"
   , "www.nyaa.se"
   , "www.nybooks.com"
   , "www.nzherald.co.nz"
   , "www.odt.co.nz"
   , "www.oglaf.com"
   , "www.ohri.ca"
   , "www.olin.edu"
   , "www.oliverwinery.com"
   , "www.om.nl"
   , "www.openphilanthropy.org"
   , "www.optimizely.com"
   , "www.optimox.com"
   , "www.oreilly.com"
   , "www.osti.gov"
   , "www.outsideonline.com"
   , "www.outsideonline.com"
   , "www.overthinkingit.com"
   , "www.packtpub.com"
   , "www.pbs.org"
   , "www.pcworld.com"
   , "www.petcarerx.com"
   , "www.petco.com"
   , "www.peterbloem.nl"
   , "www.petforums.co.uk"
   , "www.philanthropy.com"
   , "www.phillymag.com"
   , "www.pixiv.net"
   , "www.pluralsight.com"
   , "www.plymouthherald.co.uk"
   , "www.poetrynook.com"
   , "www.politie.nl"
   , "www.polizei.bayern.de"
   , "www.polizei.sachsen.de"
   , "www.polygon.com"
   , "www.popularmechanics.com"
   , "www.postandcourier.com"
   , "www.povertyactionlab.org"
   , "www.preclinicaltrials.eu"
   , "www.pressandjournal.co.uk"
   , "www.princeton.edu"
   , "www.prnewswire.com"
   , "www.progressive.org"
   , "www.proofofexistence.com"
   , "www.propublica.org"
   , "www.prospecbio.com"
   , "www.protoculture.ca"
   , "www.psychologicalscience.org"
   , "www.psychologytoday.com"
   , "www.quantamagazine.org"
   , "www.r-inla.org"
   , "www.ratbehavior.org"
   , "www.rbmojournal.com"
   , "www.rean-wings.net"
   , "www.rechem.ca"
   , "www.reddit.com"
   , "www.redliongrantchester.co.uk"
   , "www.regruntled.com"
   , "www.replicatedtypo.com"
   , "www.repository.cam.ac.uk"
   , "www.rescuetime.com"
   , "www.research.va.gov"
   , "www.researchandmarkets.com"
   , "www.rightstufanime.com"
   , "www.roangelo.net"
   , "www.rollingstone.com"
   , "www.rrauction.com"
   , "www.rsm.nl"
   , "www.rte.ie"
   , "www.rug.nl"
   , "www.salesforce.com"
   , "www.samharris.org"
   , "www.sandia.gov"
   , "www.science.org"
   , "www.science.org"
   , "www.sciencedirect.com"
   , "www.sciencemag.org"
   , "www.scifiscripts.com"
   , "www.scmp.com"
   , "www.scotthyoung.com"
   , "www.scq.ubc.ca"
   , "www.screendaily.com"
   , "www.sdfertility.com"
   , "www.seacoastonline.com"
   , "www.sebastianmarshall.com"
   , "www.sec.gov"
   , "www.seistronix.com"
   , "www.semanticscholar.org"
   , "www.sendspace.com"
   , "www.sequentialtart.com"
   , "www.sfawardswatch.com"
   , "www.shine.cn"
   , "www.shroomery.org"
   , "www.si.edu"
   , "www.silcom.com"
   , "www.simonsfoundation.org"
   , "www.sld.cu"
   , "www.slideshare.net"
   , "www.smarternootropics.com"
   , "www.smarthome.com"
   , "www.smartpowders.com"
   , "www.smh.com.au"
   , "www.snopes.com"
   , "www.snpp.com"
   , "www.soci.org"
   , "www.sophos.com"
   , "www.southwales-eveningpost.co.uk"
   , "www.spacedrepetition.com"
   , "www.sparkfun.com"
   , "www.spectator.co.uk"
   , "www.speedtest.net"
   , "www.spermbankcalifornia.com"
   , "www.spiceandtea.com"
   , "www.ssa.gov"
   , "www.ssi.shimadzu.com"
   , "www.startupschool.org"
   , "www.stat.colostate.edu"
   , "www.stats.govt.nz"
   , "www.stevepetersen.net"
   , "www.stroudnewsandjournal.co.uk"
   , "www.stuartcheshire.org"
   , "www.sudowrite.com"
   , "www.tandfonline.com"
   , "www.taylorusa.com"
   , "www.teamten.com"
   , "www.teanobi.com"
   , "www.teasetc.com"
   , "www.teavana.com"
   , "www.telegraph.co.uk"
   , "www.tennessean.com"
   , "www.tga.gov.au"
   , "www.the-tls.co.uk"
   , "www.theadvertiser.com"
   , "www.theage.com.au"
   , "www.theannals.com"
   , "www.theatlantic.com"
   , "www.thedailybeast.com"
   , "www.thediff.co"
   , "www.thehomesecuritysuperstore.com"
   , "www.theladders.com"
   , "www.thelancet.com"
   , "www.thelocal.de"
   , "www.themarshallproject.org"
   , "www.thenewatlantis.com"
   , "www.theonion.com"
   , "www.theringer.com"
   , "www.theroot.com"
   , "www.thessgac.org"
   , "www.thestar.com"
   , "www.thetimes.co.uk"
   , "www.thinkinginanutshell.com"
   , "www.thisamericanlife.org"
   , "www.thisstorydoesnotexist.com"
   , "www.thoughtco.com"
   , "www.thv11.com"
   , "www.ti.com"
   , "www.timesofisrael.com"
   , "www.tinyletter.com"
   , "www.tn.gov"
   , "www.topic.com"
   , "www.toplessrobot.com"
   , "www.torproject.org"
   , "www.torservers.net"
   , "www.townandcountrymag.com"
   , "www.treasurydirect.gov"
   , "www.trixietracker.com"
   , "www.tryhaskell.org"
   , "www.tuftandneedle.com"
   , "www.tweaktown.com"
   , "www.twitch.tv"
   , "www.typografie.info"
   , "www.typography.com"
   , "www.uber.com"
   , "www.ukbiobank.ac.uk"
   , "www.ummah.com"
   , "www.unicef.org"
   , "www.unirioja.es"
   , "www.unitedpharmacies-uk.md"
   , "www.upi.com"
   , "www.upjohn.org"
   , "www.usagi.org"
   , "www.uscourts.gov"
   , "www.usenix.org"
   , "www.usgs.gov"
   , "www.uso.org"
   , "www.uv.es"
   , "www.verywellhealth.com"
   , "www.vg.no"
   , "www.vitacost.com"
   , "www.wahpetondailynews.com"
   , "www.walesonline.co.uk"
   , "www.walmart.com"
   , "www.walmart.com"
   , "www.wangafu.net"
   , "www.washingtonexaminer.com"
   , "www.watercoolertrivia.com"
   , "www.wcscanada.org"
   , "www.wdaz.com"
   , "www.wdrb.com"
   , "www.webcitation.org"
   , "www.weidai.com"
   , "www.weizmann.ac.il"
   , "www.welt.de"
   , "www.whio.com"
   , "www.whirlpool.com"
   , "www.who.int"
   , "www.wickedlocal.com"
   , "www.wikiwix.com"
   , "www.willatworklearning.com"
   , "www.williamsburgmarketplace.com"
   , "www.williamsondailynews.com"
   , "www.wine-searcher.com"
   , "www.winonadailynews.com"
   , "www.wireheading.com"
   , "www.wnycstudios.org"
   , "www.wolframalpha.com"
   , "www.worksinprogress.co"
   , "www.worldcat.org"
   , "www.wsmv.com"
   , "www.wwltv.com"
   , "www.wzzm13.com"
   , "www.xilinx.com"
   , "www.xinhuanet.com"
   , "www.xn--4dbcyzi5a.com"
   , "www.ycombinator.com"
   , "www.york.ac.uk"
   , "www0.us.ioccc.org"
   , "www108.lamp.le.ac.uk"
   , "www2.bfi.org.uk"
   , "www2.ed.gov"
   , "www2.guidestar.org"
   , "www2.psy.uq.edu.au"
   , "www3.ntu.edu.sg"
   , "wwwcn.cs.uni-duesseldorf.de"
   , "wyclif.substack.com"
   , "yourmorals.org"
   , "yp.flutterguy.org"
   , "yunnansourcing.com"
   , "yunnansourcing.us"
   , "zerocoin.org"
  , "1d4chan.org"
  , "abebooks.com"
  , "academia.edu"
  , "ai.facebook.com"
  , "ajcn.nutrition.org"
  , "anidb.net"
  , "ankiweb.net"
  , "annals.org"
  , "antilop.cc"
  , "app.inferkit.com"
  , "archive.foolz.us"
  , "archive.is"
  , "archive.recapthelaw.org"
  , "archpsyc.ama-assn.org"
  , "arstechnica.com"
  , "artbreeder.com"
  , "arxiv-vanity.com"
  , "arxiv.org"
  , "aur.archlinux.org"
  , "aurellem.org"
  , "babel.hathitrust.org"
  , "bakabt.me"
  , "betterworldbooks.com"
  , "bibliophilly.library.upenn.edu"
  , "bigquery.cloud.google.com"
  , "biomedcentral.com"
  , "bit-player.org"
  , "blog.fc2.com"
  , "book.realworldhaskell.org"
  , "books.google.com"
  , "ccc.de"
  , "cdm16630.contentdm.oclc.org"
  , "ciechanow.ski"
  , "clickotron.com"
  , "colab.research.google.com"
  , "creativecommons.org"
  , "cryptome.org"
  , "danbooru.donmai.us"
  , "darkdata.bc.edu"
  , "darknetlive.com"
  , "darwin-online.org.uk"
  , "de1.erowid.org"
  , "www.deepmind.com"
  , "derpibooru.org"
  , "dev.kanotype.net"
  , "developer.mozilla.org"
  , "discord.com"
  , "drive.google.com"
  , "dspace.mit.edu"
  , "duolingo.com"
  , "ectoranoana.jp"
  , "elifesciences.org"
  , "emacswiki.org"
  , "eric.ed.gov"
  , "erowid.org"
  , "eva.onegeek.org"
  , "examine.com"
  , "f1000research.com"
  , "fifteen.ai"
  , "fightaging.org"
  , "fis.fda.gov"
  , "flatisjustice.moe"
  , "folding.stanford.edu"
  , "folkrnn.org"
  , "fred.stlouisfed.org"
  , "gallica.bnf.fr"
  , "getlamp.com"
  , "gitcoin.co"
  , "github.com"
  , "gitlab.com"
  , "gmane.org"
  , "goo.gl"
  , "goproblems.com"
  , "gptprompts.wikidot.com"
  , "groups.google.com"
  , "gwern.shinyapps.io"
  , "halshs.archives-ouvertes.fr"
  , "haveibeenpwned.com"
  , "hn.algolia.com"
  , "httparchive.org"
  , "huggingface.co"
  , "hutter1.net"
  , "imgur.com"
  , "incompleteideas.net"
  , "inklingmarkets.com"
  , "iqout.com"
  , "iqtest.dk"
  , "isfdb.org"
  , "jacurutu.com"
  , "journals.sagepub.com"
  , "jwz.org"
  , "koeln.ccc.de"
  , "lacbzxobeprssrfx.onion"
  , "leaderboard.allenai.org"
  , "learnyouahaskell.com"
  , "libgen.org"
  , "liebertpub.com"
  , "link.springer.com"
  , "lists.urth.net"
  , "listudy.org"
  , "longbets.org"
  , "longreads.com"
  , "lwn.net"
  , "make.girls.moe"
  , "mathoverflow.net"
  , "mayoclinicproceedings.org"
  , "media.springernature.com"
  , "medium.com"
  , "mega.nz"
  , "meltingasphalt.com"
  , "millionshort.com"
  , "mnemosyne-proj.org"
  , "mru.org"
  , "myanimelist.net"
  , "nearlyfreespeech.net"
  , "neuralnetworksanddeeplearning.com"
  , "newegg.com"
  , "nicovideo.jp"
  , "nixnote.org"
  , "oglaf.com"
  , "old.reddit.com"
  , "onlinelibrary.wiley.com"
  , "openreview.net"
  , "orbis.stanford.edu"
  , "osf.io"
  , "paperswithcode.com"
  , "patch.com"
  , "pcdb.santafe.edu"
  , "pcpartpicker"
  , "peerj.com"
  , "perma.cc"
  , "philanthropy.com"
  , "philarchive.org"
  , "physicstoday.scitation.org"
  , "play.aidungeon.io"
  , "plos"
  , "pluralsight.com"
  , "popcon.debian.org"
  , "practicaltypography.com"
  , "predictionbook.com"
  , "programmablesearchengine.google.com"
  , "projecteuclid.org"
  , "proofofexistence.com"
  , "psyarxiv.com"
  , "publicsearch.ndcourts.gov"
  , "r-inla.org"
  , "readonlymemory.vg"
  , "rpubs.com"
  , "scholar.google.com"
  , "scienceblogs.com"
  , "scp-wiki.wikidot.com"
  , "serendipityrecs.com"
  , "sethroberts.net"
  , "silkroad5v7dywlc.onion"
  , "silkroadvb5piz3r.onion"
  , "slashdot.org"
  , "slate.com"
  , "snopes.com"
  , "soundcloud.com"
  , "sourceforge.net"
  , "sparkfun.com"
  , "stackexchange.com"
  , "stackoverflow.com"
  , "tasvideos.org"
  , "thecleverest.com"
  , "thegradient.pub"
  , "thehub7dnl5nmcz5.onion"
  , "thepiratebay.org"
  , "thesecatsdonotexist.com"
  , "thessgac.org"
  , "thiscardoesnotexist"
  , "thismarketingblogdoesnotexist.com"
  , "thisponydoesnotexist.net"
  , "thisstorydoesnotexist.com"
  , "thisvesseldoesnotexist.com"
  , "tineye.com"
  , "tinyletter.com"
  , "tl.net"
  , "tom7.org"
  , "torservers.net"
  , "translate.google.com"
  , "treasurydirect.gov"
  , "trends.google.com"
  , "tryhaskell.org"
  , "tvtropes.org"
  , "unesdoc.unesco.org"
  , "urth.net"
  , "uscourts.gov"
  , "usesthis.com"
  , "vimeo.com"
  , "vision-explorer.allenai.org"
  , "vizhub.healthdata.org"
  , "waifu2x.udp.jp"
  , "waifulabs.com"
  , "websitedownloader.io"
  , "wellcomecollection.org"
  , "wikiwix.com"
  , "wolframlpha.com"
  , "worrydream.com"
  , "www.alexa.com"
  , "www.animenewsnetwork.com"
  , "www.bbc.co.uk"
  , "www.bbc.com"
  , "www.biorxiv.org"
  , "www.blog.sethroberts.net"
  , "www.bloomberg.com"
  , "www.bls.gov"
  , "www.bmj.com"
  , "www.catb.org"
  , "www.edge.org"
  , "www.erowid.org"
  , "www.fanfiction.net"
  , "www.goodreads.com"
  , "www.harney.com"
  , "www.imdb.com"
  , "www.impactcybertrust.org"
  , "www.jneurosci.org"
  , "www.jstatsoft.org"
  , "www.kaggle.com"
  , "www.kickstarter.com"
  , "www.mdpi.com"
  , "www.medrxiv.org"
  , "www.metaculus.com"
  , "www.moma.org"
  , "www.nature.com"
  , "www.nejm.org"
  , "www.newyorker.com"
  , "www.nytimes.com"
  , "www.patreon.com"
  , "www.paulgraham.com"
  , "www.pnas.org"
  , "www.projectrho.com"
  , "www.quora.com"
  , "www.rand.org"
  , "www.researchgate.net"
  , "www.rocketpunk-manifesto.com"
  , "www.scholarpedia.org"
  , "www.sciencedaily.com"
  , "www.scientificamerican.com"
  , "www.sethroberts.net"
  , "www.smbc-comics.com"
  , "www.smithsonianmag.com"
  , "www.ted.com"
  , "www.tensorflow.org"
  , "www.theguardian.com"
  , "www.theverge.com"
  , "www.timeanddate.com"
  , "www.uptontea.com"
  , "www.urth.net"
  , "www.vice.com"
  , "www.vox.com"
  , "www.wunderground.com"
  , "www.teds.ac.uk"
  , "stability.ai"
   ]

url :: T.Text -> Inline
url u = linkLive (Link nullAttr [] (u,""))

-- URLs which fail their test:
linkLiveTest :: [(T.Text,Bool)]
linkLiveTest = filter (\(u, bool) -> bool /=
                                       (url u == Link ("",["link-live"], []) [] (u,""))
                      )
               linkLiveTestUnits

-- check the live test-cases with curl for X-Frame HTTP headers; the presence of these guarantees liveness no longer works and they need to be updated.
linkLiveTestHeaders :: IO ()
linkLiveTestHeaders = forM_ (map fst goodLinks)
  (\u -> do (status,_,bs) <- runShellCommand "./" Nothing "curl" ["--insecure", "--user-agent", "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:96.0) Gecko/20100101 Firefox/96.1", "--location","--silent","--head", T.unpack u]
            case status of
                ExitFailure _ -> printRed ("Error: curl download failed on URL " ++ T.unpack u) >> print (" : " ++ show status ++ " : " ++ show bs)
                _ -> do let s = map toLower $ U.toString bs
                        when ("x-frame" `isInfixOf` s && not ("x-archive-orig-x-frame-options" `isInfixOf` s)) $
                          printRed (T.unpack u) >> print (" : X-FRAME option detected on URL : " ++ show bs)
  )

linkLiveTestUnits, goodLinks, badLinks :: [(T.Text,Bool)]
linkLiveTestUnits = goodLinks ++ badLinks
goodLinks = [("https://demo.allennlp.org/next-token-lm", True)
            , ("http://aleph.se/andart2/neuroscience/energetics-of-the-brain-and-ai/", True)
            , ("http://alumni.media.mit.edu/~cahn/life/gian-carlo-rota-10-lessons.html#mistakes", True)
            , ("http://beza1e1.tuxen.de/articles/accidentally_turing_complete.html", True)
            , ("http://blog.nuclearsecrecy.com/2013/05/17/the-price-of-the-manhattan-project/", True)
            , ("http://courses.csail.mit.edu/6.857/2012/files/nash.py", True)
            , ("http://datacolada.org/", True)
            , ("http://diyhpl.us/wiki/transcripts/hgp-write/2016-05-10/ultra-safe-cell-line/", True)
            , ("http://dwarffortresswiki.org/index.php/User:BaronW#The_Almighty_Dwarven_Calculator", True)
            , ("http://evaotaku.com/html/programbooks.html", True)
            , ("http://feeds.feedburner.com/longbets", True)
            , ("http://felinegenetics.missouri.edu/feline-genome-project-2/cat-genomic-resources-strs-snps", True)
            , ("http://findarticles.com/p/articles/mi_hb4384/is_3_38/ai_n29083511/", True)
            , ("http://forre.st/storage#hdd", True)
            , ("http://google-summer-of-code-2009-haskell.googlecode.com/files/Niklas_Broberg.tar.gz", True)
            , ("http://inhumanexperiment.blogspot.com/2009/03/increasing-intelligence-by-playing.html", True)
            , ("http://jakewestfall.org/ivy/", True)
            , ("http://jov.arvojournals.org/content/12/9/177.abstract", True)
            , ("http://langsec.org/", True)
            , ("http://learning.mpi-sws.org/memorize/", True)
            , ("http://libgen.rs/scimag/", True)
            , ("http://mlg.eng.cam.ac.uk/yarin/blog_3d801aa532c1ce.html", True)
            , ("http://neojaponisme.com/", True)
            , ("http://ngm.nationalgeographic.com/print/2011/10/teenage-brains/dobbs-text", True)
            , ("http://norvig.com/norvigs-law.html", True)
            , ("http://ohtori.nu/creators/contributors.html", True)
            , ("http://pauillac.inria.fr/~weis/info/commandline.html", True)
            , ("http://pediatrics.aappublications.org/content/122/2/e472.short", True)
            , ("http://programme.exordo.com/isir2017/delegates/presentation/29/", True)
            , ("http://quinndunki.com/blondihacks/?p=3023", True)
            , ("http://r6.ca/blog/20090522T015739Z.html", True)
            , ("http://stats.grok.se/en/201109/Talk%3ABarack_Obama", True)
            , ("http://timetravel.mementoweb.org/", True)
            , ("http://videolectures.net/rldm2015_silver_reinforcement_learning/", True)
            , ("http://wiki.openttdcoop.org/Logic", True)
            , ("http://www.abcb.com/newspaper/1999-12-30_asahi_01.htm", True)
            , ("http://www.aleph.se/andart/archives/2012/09/flaws_in_the_perfection.html", True)
            , ("http://www.alessonislearned.com/index.php?comic=14", True)
            , ("http://www.antipope.org/charlie/blog-static/2011/08/usenix-2011-keynote-network-se.html", True)
            , ("http://www.austlii.edu.au/cgi-bin/sinodisp/au/cases/vic/VSCA/2015/35.html", True)
            , ("http://www.businessweek.com/ap/financialnews/D9KQL7CG0.htm", True)
            , ("http://www.chinadaily.com.cn/opinion/2011-09/17/content_13725092.htm", True)
            , ("http://www.cjas.org/~leng/anno-ikuhara.txt", True)
            , ("http://www.coyneoftherealm.com/2014/09/25/critique-claims-blood-test-depression/", True)
            , ("http://www.culhwch.info/index.html#footnote349-ref", True)
            , ("http://www.davidbordwell.net/blog/2012/02/13/pandoras-digital-box-pix-and-pixels/", True)
            , ("http://www.davidbrin.com/starwarsontrial.html", True)
            , ("http://www.evaotaku.com/html/kaibunsho-main.html#Sect1c", True)
            , ("http://www.galbithink.org/names/agnames.htm", True)
            , ("http://www.incompleteideas.net/book/the-book-2nd.html", True)
            , ("http://www.koreatimes.co.kr/www/news/nation/2008/05/117_24521.html", True)
            , ("http://www.metafor-project.org/doku.php", True)
            , ("http://www.michaellight.net/suns-intro/", True)
            , ("http://www.moserware.com/2008/04/towards-moores-law-software-part-3-of-3.html", True)
            , ("http://www.nyaa.eu/?page=torrentinfo&tid=255825", True)
            , ("http://www.pelleas.net/aniTOP/index.php?title=recent_stuff&more=1&c=1&tb=1&pb=1#c10309", True)
            , ("http://www.project-imas.com/wiki/index.php?title=Miki_Hoshii", True)
            , ("http://www.rfreitas.com/Astro/Xenopsychology.htm", True)
            , ("http://www.righto.com/2015/11/macbook-charger-teardown-surprising.html#ref8", True)
            , ("http://www.rwagner.net/libretti/parsifal/e-pars-a3.html", True)
            , ("http://www.rxshop.md/products/antinarcoleptic/buy-modafinil-online-order-modvigil", True)
            , ("http://www.scienceagogo.com/news/20040819224859data_trunc_sys.shtml", True)
            , ("http://www.sciencemadness.org/talk/viewthread.php?tid=6401", True)
            , ("http://www.slate.com/articles/health_and_science/the_mouse_trap/2011/11/lab_mice_are_they_limiting_our_understanding_of_human_disease_.html", True)
            , ("http://www.tarsnap.com/scrypt.html", True)
            , ("http://www.thesmokinggun.com/documents/silk-road-dealer-cooperating-567432", True)
            , ("http://www.tlmc.eu/", True)
            , ("http://www.tomodachi.de/", True)
            , ("http://www.uk-anime.net/articles/The_Takeshi_Honda_interview/3", True)
            , ("http://www.urbandharma.org/udharma4/mpe.html", True)
            , ("http://www.vetta.org/2009/12/tick-tock-tick-tock-bing/", True)
            , ("http://www.wesjones.com/eoh.htm", True)
            , ("http://www2.biology.ualberta.ca/locke.hp/dougandbill.htm", True)
            , ("https://1dollarscan.com/", True)
            , ("https://archive.nytimes.com/6thfloor.blogs.nytimes.com/2013/03/20/a-sham-procedure-leads-to-disappointing-m-s-news/", True)
            , ("https://80000hours.org/podcast/episodes/brian-christian-algorithms-to-live-by/", True)
            , ("https://abandonedfootnotes.blogspot.com/2011/04/qaddafis-chickens.html", True)
            , ("https://abcnews.go.com/Business/steve-jobs-death-billions-remain-private-topic/story?id=14682218", True)
            , ("https://abcnotation.com/wiki/abc:standard:v2.1#comments_and_remarks", True)
            , ("https://about.google/", True)
            , ("https://academic.oup.com/endo/article/160/5/1057/5381910", True)
            , ("https://academic.oup.com/ije/article/50/5/1615/6274255", True)
            , ("https://academictorrents.com/details/0d366035664fdf51cfbe9f733953ba325776e667/tech", True)
            , ("https://aclanthology.org/D15-1002/", True)
            , ("https://advertising-effects.chicagobooth.edu/", True)
            , ("https://ageing.oxfordjournals.org/content/37/1/25.long", True)
            , ("https://ageofem.com/", True)
            , ("https://agtb.wordpress.com/2012/02/17/john-nashs-letter-to-the-nsa/#comment-5458", True)
            , ("https://ai.google/research/pubs/pub46180", True)
            , ("https://ai.googleblog.com/2021/04/evolving-reinforcement-learning.html", True)
            , ("https://aiimpacts.org/wp-content/uploads/2019/02/image2.png", True)
            , ("https://alexanderetz.com/2015/08/30/the-bayesian-reproducibility-project/", True)
            , ("https://altjapan.typepad.com/my_weblog/2007/05/better_stronger.html", True)
            , ("https://andrewmayneblog.wordpress.com/2020/07/08/openai-api-alchemy-turn-a-script-into-a-novel-and-vice-versa/", True)
            , ("https://andymatuschak.org/", True)
            , ("https://animekritik.wordpress.com/2011/12/03/imperialism-translation-gunbuster-episode-five/", True)
            , ("https://ansuz.sooke.bc.ca/entry/23", True)
            , ("https://apenwarr.ca/log/?m=201707#04", True)
            , ("https://apnews.com/a6a67fb761304e3cae7497faa32dcdc9", True)
            , ("https://arbtt.nomeata.de/", True)
            , ("https://archinte.jamanetwork.com/article.aspx?articleid=414784", True)
            , ("https://architext.design/about/", True)
            , ("https://archive.nytimes.com/dealbook.nytimes.com/2011/09/03/the-survivor-who-saw-the-future-for-cantor-fitzgerald/", True)
            , ("https://archive.nytimes.com/scientistatwork.blogs.nytimes.com/2012/09/06/lost-in-time-in-the-antarctic-ice-age/", True)
            , ("https://archive.nytimes.com/www.nytimes.com/books/first/b/budiansky-lion.html", True)
            , ("https://archive.ph/VY8e2", True)
            , ("https://archive.seattletimes.com/archive/?date=19930513&slug=1701067", True)
            , ("https://archivebox.io/", True)
            , ("https://arima.cylab.cmu.edu/markets/cybercrime.php", True)
            , ("https://arr.am/2020/07/14/elon-musk-by-dr-seuss-gpt-3/", True)
            , ("https://asktog.com/atc/the-third-user/", True)
            , ("https://b-ok.cc/fulltext/", True)
            , ("https://bair.berkeley.edu/blog/2020/07/11/auction/", True)
            , ("https://bam-dataset.org/", True)
            , ("https://bam.kalzumeus.com/archive/financial-innovation-is-happening/", True)
            , ("https://beepb00p.xyz/pkm-search.html#appendix_cloudmacs", True)
            , ("https://behavioralscientist.org/mindware-the-high-cost-of-not-doing-experiments/", True)
            , ("https://bellard.org/jslinux/tech.html", True)
            , ("https://bitcoin-otc.com/", True)
            , ("https://bits.blogs.nytimes.com/2012/06/07/good-night-moon-good-night-little-bird/", True)
            , ("https://bldgblog.com/2017/01/the-season-of-burning-trucks/", True)
            , ("https://blog.8faces.com/post/132017260619/eric-gill-advance", True)
            , ("https://blog.aboutamazon.com/company-news/2016-letter-to-shareholders", True)
            , ("https://blog.acolyer.org/2018/02/22/dynamic-word-embeddings-for-evolving-semantic-discovery/", True)
            , ("https://blog.beeminder.com/hieroglyphs/", True)
            , ("https://blog.codinghorror.com/why-cant-programmers-program/", True)
            , ("https://blog.cr.yp.to/20151120-batchattacks.html", True)
            , ("https://blog.cryptographyengineering.com/2013/04/11/zerocoin-making-bitcoin-anonymous/", True)
            , ("https://blog.csdn.net/DLW__/article/details/104243506", True)
            , ("https://blog.eleuther.ai/factored-cognition/", True)
            , ("https://blog.ethereum.org/2014/07/05/stake/", True)
            , ("https://blog.google/outreach-initiatives/small-business/google-ads-helping-businesses/", True)
            , ("https://blog.otoro.net/2017/11/12/evolving-stable-strategies/", True)
            , ("https://blog.regehr.org/archives/861", True)
            , ("https://blog.thinkst.com/p/if-nsa-has-been-hacking-everything-how.html?m=1", True)
            , ("https://blog.youtube/news-and-events/five-stars-dominate-ratings/", True)
            , ("https://blogs.nvidia.com/blog/2019/03/18/gaugan-photorealistic-landscapes-nvidia-research/", True)
            , ("https://blogs.wsj.com/economics/2011/05/26/google-correlate-linking-the-fed-to-nausea-remedies/", True)
            , ("https://bmk.sh/2020/08/17/Building-AGI-Using-Language-Models/", True)
            , ("https://boards.fireden.net/ic/thread/3820909/", True)
            , ("https://boingboing.net/2012/08/09/make-yourself-healthy-searchi.html", True)
            , ("https://bwc.thelab.dc.gov/", True)
            , ("https://cacm.acm.org/magazines/2017/8/219606-the-science-of-brute-force/fulltext", True)
            , ("https://calhoun.nps.edu/handle/10945/14838", True)
            , ("https://caniuse.com/?search=hyphenate", True)
            , ("https://carbonplan.org/research/forest-offsets", True)
            , ("https://care.diabetesjournals.org/content/37/9/2557.full", True)
            , ("https://casual-effects.com/markdeep/", True)
            , ("https://catonmat.net/proof-that-sed-is-turing-complete", True)
            , ("https://cdn.discordapp.com/attachments/693736350561861633/837461131991318609/unknown.png", True)
            , ("https://cdn.openai.com/API/English_Bash_Python.mp4", True)
            , ("https://cognitivemedium.com/vme", True)
            , ("https://commons.wikimedia.org/wiki/Category:Variations_on_the_national_flag_of_Japan", True)
            , ("https://compdemocracy.org/algorithms/", True)
            , ("https://complearn.org/thesis.html", True)
            , ("https://conifer.rhizome.org/", True)
            , ("https://constancecrozier.com/2020/04/16/forecasting-s-curves-is-hard/", True)
            , ("https://copilot.github.com/", True)
            , ("https://corpgov.law.harvard.edu/2017/01/31/the-common-law-corporation-the-power-of-the-trust-in-anglo-american-business-history/", True)
            , ("https://cran.r-project.org/web/packages/BradleyTerry2/index.html", True)
            , ("https://crookedtimber.org/2012/05/30/in-soviet-union-optimization-problem-solves-you/#comment-415931", True)
            , ("https://cs.stanford.edu/people/karpathy/reinforcejs/gridworld_dp.html", True)
            , ("https://ctan.org/pkg/yinit?lang=en", True)
            , ("https://culture.org/a-natural-mother/", True)
            , ("https://daniellakens.blogspot.com/2017/07/impossibly-hungry-judges.html", True)
            , ("https://danluu.com/input-lag/", True)
            , ("https://danwang.co/college-girardian-terror/", True)
            , ("https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=100&year1=1992&year2=2011", True)
            , ("https://dataprivacylab.org/dataprivacy/projects/ssnwatch/index.html", True)
            , ("https://davidepstein.com/david-epstein-the-sports-gene/", True)
            , ("https://defector.com/in-the-future-of-collecting-is-anyone-having-fun/", True)
            , ("https://demos.obormot.net/these-waifus-do-not-exist-v2-alt", True)
            , ("https://dialnet.unirioja.es/servlet/articulo?codigo=3425319", True)
            , ("https://diff.wikimedia.org/2012/03/27/analysis-of-the-quality-of-newcomers-in-wikipedia-over-time/", True)
            , ("https://digital.library.unt.edu/explore/partners/TAMS/browse/?q=%22Julian+Stanley%22+OR+%22SMPY%22&sort=date_a&t=fulltext", True)
            , ("https://distill.pub/2017/aia/", True)
            , ("https://dnstats.net/market/Nucleus+Market", True)
            , ("https://docs.google.com/spreadsheets/d/19D8JUgf95t-f-oUAHqh8Nn2G90KO3gUiua9yAjBSSqI/edit", True)
            , ("https://dominiccummings.com/2019/03/01/on-the-referendum-31-project-maven-procurement-lollapalooza-results-nuclear-agi-safety/", True)
            , ("https://donsbot.com/2007/05/17/roll-your-own-window-manager-tracking-focus-with-a-zipper/", True)
            , ("https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.15.0.0/Prelude.html#v:until", True)
            , ("https://dresdencodak.com/2009/09/22/caveman-science-fiction/", True)
            , ("https://dumps.wikimedia.org/enwiki/latest/", True)
            , ("https://e2eml.school/transformers.html", True)
            , ("https://egamebook.com/lochness/", True)
            , ("https://eileenormsby.com/2013/09/26/the-fall-of-atlantis-a-moderator-tells/", True)
            , ("https://eli.thegreenplace.net/2007/06/28/sicp-section-122/", True)
            , ("https://emilkirkegaard.dk/en/?p=5574", True)
            , ("https://en.bitcoin.it/wiki/Proof_of_Stake", True)
            , ("https://en.touhouwiki.net/wiki/%E5%87%8B%E5%8F%B6%E6%A3%95", True)
            , ("https://en.wikibooks.org/wiki/Category:Book:Accountancy", True)
            , ("https://en.wikichip.org/wiki/intel/core_i9/i9-7900x", True)
            , ("https://en.wikifur.com/wiki/History", True)
            , ("https://en.wikipedia.org/wiki/Talk:Small_caps", True)
            , ("https://en.wikipedia.org/wiki/User:Gwern", True)
            , ("https://en.wikiquote.org/wiki/Tao_Te_Ching", True)
            , ("https://en.wikisource.org/wiki/Meditation_XVII", True)
            , ("https://en.wiktionary.org/wiki/steward#Etymology", True)
            , ("https://endlessvn.io/", True)
            , ("https://energycontrol.org/quienes-somos/proyectos/", True)
            , ("https://engineering.fb.com/2017/06/14/ml-applications/deal-or-no-deal-training-ai-bots-to-negotiate/", True)
            , ("https://enki2.tumblr.com/stories", True)
            , ("https://eprint.iacr.org/2021/1273", True)
            , ("https://equilibriabook.com/molochs-toolbox/", True)
            , ("https://esolangs.org/wiki/Linear_bounded_automaton", True)
            , ("https://etienne.se/cfnse/", True)
            , ("https://eukaryotewritesblog.com/2017/06/30/book-review-barriers/", True)
            , ("https://eurekamag.com/", True)
            , ("https://eva-fan.com/blog-entry-1198.html", True)
            , ("https://everything2.com/title/A+crow+shook+down+on+me", False)
            , ("https://ew.com/tv/2017/10/12/frankie-muniz-memory-loss/", True)
            , ("https://exac.broadinstitute.org/faq", True)
            , ("https://examples.yourdictionary.com/acrostic-poem-examples.html", True)
            , ("https://explorabl.es/", True)
            , ("https://extras.denverpost.com/stateofhope/", True)
            , ("https://familiarcycle.net/2020/how-to-finetune-gpt2-on-podcast-transcripts.html", True)
            , ("https://fastmoe.ai/", True)
            , ("https://fibery.io/blog/hypertext-tools-from-the-80s/", True)
            , ("https://files.eric.ed.gov/fulltext/EJ746292.pdf", True)
            , ("https://files.givewell.org/files/conversations/Stanley%20Young%20slides%20on%20multiple%20testing.pdf", True)
            , ("https://foreignpolicy.com/2012/09/28/aircraft-carriers-in-space/", True)
            , ("https://forum.evageeks.org/post/500631/Who-Can-Be-The-Seele-Children/#500631", True)
            , ("https://forum.quantifiedself.com/thread-zeo-shutting-down-export-your-data?pid=3412#pid3412", True)
            , ("https://forums.animesuki.com/showpost.php?p=3584198", True)
            , ("https://foundation.wikimedia.org/wiki/File:UK_BOARD_MEETING.pdf", True)
            , ("https://fras.uk/ml/large%20prior-free%20models/code/transformer-vae/2020/08/25/Transformer-VAE-for-Program-Synthesis.html", True)
            , ("https://freakonomics.com/2011/05/mining-for-correlations-it-works/#comment-244672", True)
            , ("https://freedomdefined.org/Licenses/NC", True)
            , ("https://fs.blog/decision-journal/", True)
            , ("https://fullfrontal.moe/interview-mahiro-maeda/", True)
            , ("https://futurism.com/neural-network-draw-doom-guy-high-res", True)
            , ("https://galois.com/blog/2008/9/17/parsing-the-linux-kernel-with-haskell-experience-with-langua.html", True)
            , ("https://gamefaqs.gamespot.com/boards", True)
            , ("https://gameprogrammingpatterns.com/dirty-flag.html", True)
            , ("https://gaotianyu.xyz/prompting/", True)
            , ("https://github.blog/2019-01-07-new-year-new-github/", True)
            , ("https://globalguerrillas.typepad.com/", True)
            , ("https://globalvoices.org/2011/11/05/japans-it-exodus-a-personal-perspective-part-2/print/", True)
            , ("https://gondwanaland.com/mlog/2011/10/21/almost-innovation/", True)
            , ("https://googleblog.blogspot.com/2011/10/fall-sweep.html", True)
            , ("https://googleprojectzero.blogspot.com/2017/07/trust-issues-exploiting-trustzone-tees.html", True)
            , ("https://googlesystem.blogspot.com/2013/07/google-alerts-drops-rss-feeds.html", True)
            , ("https://gpt3demo.com/apps/magicemail-io", True)
            , ("https://gradientscience.org/data_rep_bias/", True)
            , ("https://gutenberg.ca/ebooks/smithcordwainer-onthegemplanet/smithcordwainer-onthegemplanet-00-h.html", True)
            , ("https://gutenberg.net.au/ebooks03/0300151h.html", True)
            , ("https://guzey.com/books/why-we-sleep/#no-shorter-sleep-does-not-imply-shorter-life-span", True)
            , ("https://hackage.haskell.org/package/archiver", True)
            , ("https://hakaimagazine.com/features/can-we-really-be-friends-octopus/", True)
            , ("https://handbook-5-1.cochrane.org/chapter_8/8_assessing_risk_of_bias_in_included_studies.htm", True)
            , ("https://hapgood.us/2019/03/28/network-heuristics/", True)
            , ("https://harpers.org/archive/2013/09/the-devils-bait/?single=1", True)
            , ("https://hdsr.mitpress.mit.edu/pub/wi9yky5c/release/2", True)
            , ("https://healthland.time.com/2012/04/05/frozen-assets-why-u-s-sperm-is-a-hot-commodity/", True)
            , ("https://herbsutter.com/welcome-to-the-jungle/", True)
            , ("https://highnoongmt.wordpress.com/2015/08/13/deep-learning-for-assisting-the-process-of-music-composition-part-3/", True)
            , ("https://historycooperative.org/a-short-history-on-sleep-before-the-industrial-revolution/", True)
            , ("https://hub.darcs.net/simon/darcsden", True)
            , ("https://humanvarieties.org/2013/01/15/100-years-of-testing-negro-intelligence/", True)
            , ("https://ideas.repec.org/a/eee/ecolet/v91y2006i3p395-401.html", True)
            , ("https://idlewords.com/2010/03/scott_and_scurvy.htm", True)
            , ("https://ifdo.ca/~seymour/runabc/abcguide/abc2midi_guide.html", True)
            , ("https://iforcedabot.com/what-can-a-fake-news-detector-do/", True)
            , ("https://ilovetypography.com/2019/03/14/the-first-printed-math-books/", True)
            , ("https://image-net.org/challenges/beyond_ilsvrc.php", True)
            , ("https://infoproc.blogspot.com/2010/10/wigner-recollections.html", True)
            , ("https://intelligence.org/2016/09/12/new-paper-logical-induction/", True)
            , ("https://iqcomparisonsite.com/", True)
            , ("https://isomerdesign.com/PiHKAL/read.php?domain=tk&id=35", True)
            , ("https://it.wikipedia.org/wiki/Gualtiero_Cannarsi", True)
            , ("https://jamanetwork.com/journals/jama/fullarticle/183580", True)
            , ("https://jamanetwork.com/journals/jamapsychiatry/fullarticle/2569454", True)
            , ("https://jamesyu.org/about/", True)
            , ("https://jasoncrawford.org/", True)
            , ("https://jaspervdj.be/files/2011-gsoc-text-utf8-proposal.html", True)
            , ("https://jax.readthedocs.io/en/latest/notebooks/xmap_tutorial.html", True)
            , ("https://jaypsong.blog/2011/11/15/poverty-by-moon-byung-ran/", True)
            , ("https://jdlm.info/articles/2018/03/18/markov-decision-process-2048.html", True)
            , ("https://jessegalef.com/2013/01/27/messing-with-time-why-the-flash-is-in-hell/", True)
            , ("https://jetpress.org/volume1/moravec.htm", True)
            , ("https://jgeekstudies.org/2016/05/19/great-attractor-ttgl/", True)
            , ("https://joa.sh/posts/2015-09-14-prerender-mathjax.html", True)
            , ("https://johncwright.livejournal.com/117818.html", True)
            , ("https://jonmillward.com/blog/studies/deep-inside-a-study-of-10000-porn-stars/", True)
            , ("https://joshmitteldorf.scienceblog.com/2014/03/25/life-extension-supplements-a-reality-check/", True)
            , ("https://journal.stuffwithstuff.com/2020/04/05/crafting-crafting-interpreters/", True)
            , ("https://journals.biologists.com/jeb/article/218/1/123/13627/The-developmental-origins-of-chronic-physical", True)
            , ("https://jtauber.com/blog/2004/11/26/programmed_vocabulary_learning_as_a_travelling_salesman_problem/", True)
            , ("https://justgetflux.com/", True)
            , ("https://kajsotala.fi/2012/10/technology-will-destroy-human-nature/", True)
            , ("https://keras.io/", True)
            , ("https://kev.town/2013/04/30/why-did-why-the-lucky-stiff-quit/", True)
            , ("https://kevinlynagh.com/notes/pricing-niche-products/", True)
            , ("https://kill-the-newsletter.com/", True)
            , ("https://kk.org/thetechnium/the-shirky-prin/", True)
            , ("https://knightcolumbia.org/content/the-case-for-digital-public-infrastructure", True)
            , ("https://kojimars.at.webry.info/201004/article_16.html", True)
            , ("https://komonews.com/news/local/Federal-drug-charges-for-Bellevue-man-involved-in-Silk-Road-226387671.html", True)
            , ("https://krebsonsecurity.com/", True)
            , ("https://laion.ai/laion-400-open-dataset/", True)
            , ("https://larryniven.net/stories/roentgen.shtml", True)
            , ("https://latimesblogs.latimes.com/the_big_picture/2010/05/who-do-korean-moviegoers-get-to-see-iron-man-2-way-ahead-of-japan-.html", True)
            , ("https://lavaan.ugent.be/", True)
            , ("https://legacy.imagemagick.org/Usage/crop/#crop", True)
            , ("https://lettersofnote.com/2012/03/28/the-skills-of-leonardo-da-vinci/", True)
            , ("https://library.bz/main/upload/", True)
            , ("https://lifescivc.com/2011/03/academic-bias-biotech-failures/", True)
            , ("https://linyanghe.github.io/publications/files/Imagined_Speech_Decoding.pdf", True)
            , ("https://lithub.com/here-are-the-biggest-fiction-bestsellers-of-the-last-100-years/?single=true", True)
            , ("https://littlebiggy.org/viewSubject/p/4756248", True)
            , ("https://longitudinal.blog/co2-series-part-2-co2-removal/", True)
            , ("https://longtermrisk.org/the-importance-of-wild-animal-suffering/", True)
            , ("https://lucumr.pocoo.org/2013/7/23/licensing/", True)
            , ("https://magazine.atavist.com/an-arrogant-way-of-killing/", True)
            , ("https://magenta.tensorflow.org/piano-transformer", True)
            , ("https://mail.haskell.org/pipermail/haskell-cafe/2011-February/089183.html", True)
            , ("https://mail.haskell.org/pipermail/haskell-cafe/2013-April/107389.html", True)
            , ("https://mailchi.mp/459b1e4f860d/an-152how-weve-overestimated-few-shot-learning-capabilities", True)
            , ("https://mako.cc/copyrighteous/revisiting-the-rise-and-decline", True)
            , ("https://marginalrevolution.com/marginalrevolution/2013/04/trade-vs-technology-in-terms-of-their-labor-market-effects.html", True)
            , ("https://mason.gmu.edu/~rhanson/greatfilter.html", True)
            , ("https://mathbabe.org/2012/11/20/columbia-data-science-course-week-12-predictive-modeling-data-leakage-model-evaluation/", True)
            , ("https://mathshistory.st-andrews.ac.uk/Extras/Keynes_Newton/", True)
            , ("https://mathworld.wolfram.com/FermatsLastTheorem.html", True)
            , ("https://mattlakeman.org/2020/01/22/the-new-epidemic-my-experience-of-losing-a-friend-to-heroin/", True)
            , ("https://mc-stan.org/", True)
            , ("https://medicalxpress.com/news/2011-08-vitamin-pills-undermine.html", True)
            , ("https://meta.wikimedia.org/wiki/Research:Wiki_Participation_Challenge#Dissemination", True)
            , ("https://meteuphoric.com/2010/08/08/on-the-hostility-of-wives/#comment-1427", True)
            , ("https://metropolitician.blogs.com/scribblings_of_the_metrop/2010/06/facebook-taking-over-korea-as-predicted.html", True)
            , ("https://michaelnielsen.org/blog/three-myths-about-scientific-peer-review/", True)
            , ("https://mikepower.pressfolios.com/", True)
            , ("https://milan.cvitkovic.net/writing/things_youre_allowed_to_do/", True)
            , ("https://minimaxir.com/2017/06/imgur-decline/", True)
            , ("https://mitpress.mit.edu/sites/default/files/sicp/index.html", True)
            , ("https://mkv25.net/dfma/map-8269", True)
            , ("https://ml.berkeley.edu/blog/posts/clip-art/", True)
            , ("https://mlp.fandom.com/wiki/The_Perfect_Pear", True)
            , ("https://mmlab.ie.cuhk.edu.hk/projects/CelebA.html", True)
            , ("https://mosaicscience.com/story/my-deja-vu-so-extreme-i-cant-tell-whats-real-any-more/", True)
            , ("https://mssv.net/2020/08/02/what-args-can-teach-us-about-qanon/", True)
            , ("https://my.pgp-hms.org/users", True)
            , ("https://my.vanderbilt.edu/smpy/publications/camilla-benbow/", True)
            , ("https://nautil.us/will-90-become-the-new-60-rp-5956/", True)
            , ("https://nbc-2.com/news/crime/2022/01/04/man-arrested-in-desoto-county-drug-bust-on-new-years-eve/", True)
            , ("https://ncase.me/", True)
            , ("https://neurosciencenews.com/brain-connectome-artificial-neural-networks/", True)
            , ("https://newcriterion.com/issues/2006/10/a-good-list", True)
            , ("https://news.nationalgeographic.com/news/2014/10/141015-better-beef-genetics-science-agriculture-environment-ngfood/", True)
            , ("https://nickbostrom.com/astronomical/waste.html", True)
            , ("https://nintil.com/science-ending-frontier", True)
            , ("https://nootropicsdepot.com/bacopa/", True)
            , ("https://notes.pinboard.in/u:vitorio/05dec9f04909d9b6edff", True)
            , ("https://numinous.productions/ttft/", True)
            , ("https://nyaa.si/view/395795", True)
            , ("https://nymag.com/news/features/70830/#print", True)
            , ("https://okmij.org/ftp/Streams.html", True)
            , ("https://online.wsj.com/article/SB10001424053111903480904576512250915629460.html", True)
            , ("https://ooo.ghostbows.ooo/", True)
            , ("https://openai.com/blog/learning-to-summarize-with-human-feedback/", True)
            , ("https://opensource.adobe.com/dc-acrobat-sdk-docs/#page=5", True)
            , ("https://opinionator.blogs.nytimes.com/2009/11/24/the-end-of-music/", True)
            , ("https://originstamp.com/s/7306a744a285474742f4f9ae8ddae8214fb7625348d578fb3077fb0bae92b8f1", True)
            , ("https://orionmagazine.org/2011/11/interviews-with-an-octopus/", True)
            , ("https://orwell.ru/library/articles/nose/english/e_nose", True)
            , ("https://oscarbonilla.com/2009/05/visualizing-bayes-theorem/", True)
            , ("https://ourworldindata.org/happiness-and-life-satisfaction", True)
            , ("https://palladiummag.com/2018/11/29/a-week-in-xinjiangs-absolute-surveillance-state/", True)
            , ("https://palmerlab.org/neuroticism-and-depression-gwas-consortium-paper-accepted-for-publication-in-jama-psychiatry-abraham-palmer-harriet-de-wit-and-amy-hart-are-co-authors/", True)
            , ("https://pandoc.org/", True)
            , ("https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3819317", True)
            , ("https://parahumans.wordpress.com/2011/06/21/gestation-1-4/", True)
            , ("https://parametric.press/issue-01/unraveling-the-jpeg/", True)
            , ("https://paulfchristiano.com/ea/", True)
            , ("https://penelope.uchicago.edu/hydrionoframes/hydrion.html", True)
            , ("https://people.com/tv/frankie-muniz-memory-loss-malcolm-in-middle/", True)
            , ("https://people.csail.mit.edu/mrub/VisualMic/", True)
            , ("https://people.idsia.ch/~juergen/creativity.html", True)
            , ("https://personal.math.ubc.ca/~cass/Euclid/byrne.html", True)
            , ("https://personalitytest.net/ipip/index.html", True)
            , ("https://petertodd.org/2016/opentimestamps-announcement", True)
            , ("https://pharmacyreviewer.co/?q=modafinil&option=com_medicine&view=search", True)
            , ("https://philip.greenspun.com/teaching/teaching-software-engineering", True)
            , ("https://philpapers.org/browse/the-reflection-principle", True)
            , ("https://phys.org/news/2019-12-mouse-pups-born-eggs-derived.html", True)
            , ("https://plato.stanford.edu/entries/moral-anti-realism/index.html", True)
            , ("https://playground.tensorflow.org/", True)
            , ("https://poemanalysis.com/edward-estlin-cummings/r-p-o-p-h-e-s-s-a-g-r/", True)
            , ("https://poets.org/poem/design", True)
            , ("https://politicalcalculations.blogspot.com/2011/07/how-much-are-geezers-displacing-teens.html", True)
            , ("https://poniesatdawn.bandcamp.com/track/peace-at-last", True)
            , ("https://popcon.ubuntu.com/", True)
            , ("https://press.etc.cmu.edu/index.php/product/well-played-vol-4-no-1/", True)
            , ("https://proceedings.mlr.press/v119/huang20f.html", True)
            , ("https://progressstudies.school/", True)
            , ("https://projects.jsonline.com/news/2016/12/11/the-price-of-being-wrong.html", True)
            , ("https://projects.tampabay.com/projects/2016/food/farm-to-fable/restaurants/", True)
            , ("https://psych.hanover.edu/JavaTest/CLE/Cognition/Cognition/MentalRotation.html", True)
            , ("https://psychcentral.com/pro/sparlon-and-adhd-the-power-of-a-7-year-old/002889.html", True)
            , ("https://psychonautwiki.org/wiki/Wake_initiated_lucid_dream", True)
            , ("https://publicdomainreview.org/essay/emma-willard-maps-of-time/", True)
            , ("https://publishing.cdlib.org/ucpressebooks/view?docId=ft8489p25j&chunk.id=d0e2683&toc.id=d0e2575&brand=eschol", True)
            , ("https://pudding.cool/2021/03/love-and-ai/", True)
            , ("https://pytorch.org/", True)
            , ("https://qntm.org/invisibility", True)
            , ("https://quadrant.org.au/magazine/2015/05/eugenics-ready/", True)
            , ("https://qualiacomputing.com/2019/11/15/break-out-of-the-simulation-day-televised-entity-contact-injection-pulling-experiments-and-the-brain-as-a-game-engine/", True)
            , ("https://quantifiedself.com/blog/spaced-repetition-and-learning/", True)
            , ("https://quantum.country/qcvc", True)
            , ("https://queue.acm.org/detail.cfm?ref=rss&id=2856460", True)
            , ("https://quillette.com/2021/11/29/the-universal-structure-of-storytelling/", True)
            , ("https://qwantz.com/index.php?comic=2366", True)
            , ("https://racket-lang.org/", True)
            , ("https://rationality.org/", True)
            , ("https://rdiff-backup.net/", True)
            , ("https://read-the-samples.netlify.app/sample_1353/", True)
            , ("https://readwrite.com/2011/11/03/armed_with_social_signals_google_moves_back_toward/", True)
            , ("https://reflectivedisequilibrium.blogspot.com/", True)
            , ("https://replicationindex.com/", True)
            , ("https://retractionwatch.com/2014/07/08/sage-publications-busts-peer-review-and-citation-ring-60-papers-retracted/", True)
            , ("https://ricon.dev/", True)
            , ("https://rjlipton.wordpress.com/2014/07/21/shifts-in-algorithm-design/", True)
            , ("https://rjlipton.wordpress.com/the-gdel-letter/", True)
            , ("https://rootsofprogress.org/nuclear-physics", True)
            , ("https://row1.ca/pixels-and-their-neighbors", True)
            , ("https://ruder.io/recent-advances-lm-fine-tuning/", True)
            , ("https://safebooru.org/index.php?page=post&s=list&tags=heterochromia", True)
            , ("https://samoburja.com/the-youtube-revolution-in-knowledge-transfer/", True)
            , ("https://scarybeastsecurity.blogspot.com/2016/11/0day-exploit-compromising-linux-desktop.html", True)
            , ("https://scholars-stage.org/reflections-on-chinas-stalinist-heritage-ii-just-how-totalitarian-is-modern-china/", True)
            , ("https://schoolgirlmilkycrisis.com/2011/10/10/get-lost/#comment-236173", True)
            , ("https://sciencebasedmedicine.org/antioxidants-and-exercise-more-harm-than-good/", True)
            , ("https://sciencebulletin.org/archives/9946.html", True)
            , ("https://scottaaronson.blog/?p=346", True)
            , ("https://senrigan.io/blog/how-writeupai-runs-behind-the-scenes/", True)
            , ("https://seths.blog/2005/03/dont_shave_that/", True)
            , ("https://sevensecularsermons.org/about/", True)
            , ("https://sf-encyclopedia.com/entry/smith_cordwainer", True)
            , ("https://shkspr.mobi/blog/2013/03/preparing-for-the-collapse-of-digital-civilization/", True)
            , ("https://sifter.org/iqtest/", True)
            , ("https://signalvnoise.com/posts/2942-exit-interview-founders-look-back-at-acquisitions-by-google-aol-microsoft-and-more", True)
            , ("https://slatestarcodex.com/2015/09/23/vegetarianism-for-meat-eaters/", True)
            , ("https://slimemoldtimemold.com/2022/01/27/like-a-lemon-to-a-lime-a-lime-to-a-lemon/", True)
            , ("https://socghop.appspot.com/gsoc/student_project/show/google/gsoc2009/haskell/t124022467805", True)
            , ("https://socialsciences.mcmaster.ca/econ/ugcm/3ll3/menger/money.txt", True)
            , ("https://sociologicalscience.com/articles-v6-9-219/", True)
            , ("https://soranews24.com/2019/02/14/video-shows-off-hundreds-of-beautiful-ai-created-anime-girls-in-less-than-a-minute%E3%80%90video%E3%80%91/", True)
            , ("https://spikejapan.wordpress.com/about/", True)
            , ("https://spp.fas.org/starwars/offdocs/rrspch.htm", True)
            , ("https://srconstantin.wordpress.com/2014/05/07/beyond-the-one-percent-categorizing-extreme-elites/", True)
            , ("https://sre.google/sre-book/eliminating-toil/", True)
            , ("https://stackroboflow.com/", True)
            , ("https://statmodeling.stat.columbia.edu/2010/11/03/some_thoughts_o_8/", True)
            , ("https://statmodeling.stat.columbia.edu/2014/08/28/publication-bias-social-sciences-unlocking-file-drawer2/", True)
            , ("https://status451.com/2017/01/20/days-of-rage/", True)
            , ("https://stephenmalina.com/post/2021-07-01-energetic-aliens-among-us/", True)
            , ("https://stevenson.lab.uconn.edu/scaling/", True)
            , ("https://story.californiasunday.com/cosmic-crisp-apple-launch/", True)
            , ("https://stratechery.com/2019/the-google-squeeze/", True)
            , ("https://strategy.wikimedia.org/wiki/Editor_Trends_Study/Results#Cohort_Analysis", True)
            , ("https://svilentodorov.xyz/blog/gpt-15b-chat-finetune/", True)
            , ("https://synapse.koreamed.org/articles/1101514", True)
            , ("https://takimag.com/article/the_second_least_glamorous_job_in_showbiz", True)
            , ("https://taooftea.com/product/frozen-summit/", True)
            , ("https://teageegeepea.tripod.com/maumau.html", True)
            , ("https://texample.net/tikz/examples/hydrogen-splitting/", True)
            , ("https://text.npr.org/974534021", True)
            , ("https://the-liliquarium.livejournal.com/1482.html", True)
            , ("https://the-toast.net/2013/09/03/another-lifeless-planet-found/", True)
            , ("https://thebaffler.com/latest/stick-to-the-plan-james", True)
            , ("https://thebrowser.com/", True)
            , ("https://thefirstaibook.com/#post-inner", True)
            , ("https://thegrandnarrative.com/about-2/", True)
            , ("https://thehardestscience.com/2011/09/29/does-psilocybin-cause-changes-in-personality-maybe-but-not-so-fast/", True)
            , ("https://thelastpsychiatrist.com/2011/09/how_to_be_mean_to_your_kids.html", True)
            , ("https://thenewstatistics.com/itns/2018/04/03/weve-been-here-before-the-replication-crisis-over-the-pygmalion-effect/", True)
            , ("https://thisanimedoesnotexist.ai/", True)
            , ("https://thiscatdoesnotexist.com/", True)
            , ("https://thiseyedoesnotexist.com/story/", True)
            , ("https://thispersondoesnotexist.com/", True)
            , ("https://thisrentaldoesnotexist.com/", True)
            , ("https://thoughtcrime.crummy.com/2009/Error.html", True)
            , ("https://threadreaderapp.com/thread/1187161460033458177.html", True)
            , ("https://time.com/time/printout/0,8816,1893946,00.html", True)
            , ("https://tomcritchlow.com/2020/06/09/quotebacks/", True)
            , ("https://tools.wmflabs.org/pageviews/?project=en.wikipedia.org&platform=all-access&agent=user&start=2017-02-01&end=2017-03-31&pages=Long_Bets", True)
            , ("https://tosche.net/blog/ink-traps-and-pals", True)
            , ("https://training.kalzumeus.com/newsletters/archive/saas_pricing", True)
            , ("https://transformer-circuits.pub/2021/framework/index.html#anthropic", True)
            , ("https://trixter.oldskool.org/2014/06/19/8088-domination-post-mortem-part-1/", True)
            , ("https://tug.org/FontCatalogue/otherfonts.html#initials", True)
            , ("https://unenumerated.blogspot.com/2011/05/bitcoin-what-took-ye-so-long.html", True)
            , ("https://unqualified-reservations.blogspot.com/2008/01/how-i-stopped-believing-in-democracy.html", True)
            , ("https://unsongbook.com/chapter-4-tools-were-made-and-born-were-hands/", True)
            , ("https://vasilishynkarenka.com/gpt-3/", True)
            , ("https://vast.ai/", True)
            , ("https://vastabrupt.com/2018/08/07/time-war-briefing-for-neolemurian-agents/", True)
            , ("https://vdfresearch.org/", True)
            , ("https://venturebeat.com/2017/10/01/globalfoundries-next-generation-chip-factories-will-cost-at-least-10-billion/view-all/", True)
            , ("https://vfxblog.com/2017/08/23/the-tech-of-terminator-2-an-oral-history/", True)
            , ("https://vinoshipper.com/shop/silver_hand_meadery/raspberry_passion_22,497", True)
            , ("https://vitalik.ca/general/2019/11/22/progress.html", True)
            , ("https://w.atwiki.jp/toho/pages/727.html", True)
            , ("https://warontherocks.com/2021/08/foreign-fighters-and-cheese-bells/", True)
            , ("https://washingtonmonthly.com/features/2007/0709.careycascadia.html", True)
            , ("https://wavemotioncannon.com/2017/01/03/yoh-yoshinari-interview-animestyle-032013-part-33/", True)
            , ("https://web.archive.org/web/20071229061158/https://www.japansociety.org/otaku_talk", True)
            , ("https://web.archive.org/web/20100126083055/https://www.unc.edu/courses/2008spring/psyc/270/001/counterbalancing.html", True)
            , ("https://web.archive.org/web/20110415182316/http://packdeps.haskellers.com/reverse/push-notify", True)
            , ("https://web.archive.org/web/20110429055151/http://www.google-melange.com/gsoc/project/google/gsoc2011/refold/31001", True)
            , ("https://web.archive.org/web/20130810215355/http://au.news.yahoo.com/today-tonight/lifestyle/article/-/17821047/online-black-market", True)
            , ("https://web.archive.org/web/20140128121712/http://articles.latimes.com/1986-07-30/business/fi-18840_1_laser-defense", True)
            , ("https://web.archive.org/web/20150211211107/https://support.google.com/news/answer/1638638", True)
            , ("https://web.archive.org/web/20151106005148/http://www.evacommentary.org/appendix/character-names.html", True)
            , ("https://web.archive.org/web/20160205133519/https://www.baka-tsuki.org/project/index.php?title=Utsuro_no_Hako:Volume_1", True)
            , ("https://web.archive.org/web/20170419194138/https://www.princeton.edu/mudd/finding_aids/mathoral/pmcxrota.htm", True)
            , ("https://web.archive.org/web/20170721094633/http://1731298478.tumblr.com/post/52689158259/sadamoto-i-first-met-him-when-i-worked", True)
            , ("https://web.archive.org/web/20170722004635/http://silverhandmeadery.com/portfolio-posts/dream-by-the-fire/", True)
            , ("https://web.archive.org/web/20171020041641/http://nitro.biosci.arizona.edu/zbook/NewVolume_2/newvol2.html", True)
            , ("https://web.archive.org/web/20190415123208/https://thiscardoesnotexist.glitch.me/", True)
            , ("https://web.media.mit.edu/~minsky/papers/Why%20programming%20is--.html", True)
            , ("https://web.stanford.edu/dept/HPS/transplant/html/murray.html", True)
            , ("https://webcitation.org/5mYown8VS", True)
            , ("https://well.blogs.nytimes.com/2009/06/08/worries-about-antioxidant-use-by-breast-cancer-patients/#more-6629", True)
            , ("https://what-if.xkcd.com/76/", True)
            , ("https://whyevolutionistrue.com/2018/03/04/a-human-chimera/", True)
            , ("https://wiki.archiveteam.org/index.php/Google_Reader", True)
            , ("https://wiki.haskell.org/index.php?title=&search=&fulltext=Search", True)
            , ("https://wiki.lesswrong.com/wiki/Egan%27s_law", True)
            , ("https://wikimediafoundation.org/wiki/Planned_Spending_Distribution_2008-2009", True)
            , ("https://writeswith.com/", True)
            , ("https://writings.stephenwolfram.com/2012/03/the-personal-analytics-of-my-life/", True)
            , ("https://www-cs-faculty.stanford.edu/~knuth/faq.html", True)
            , ("https://www.4nrx-uk.md/general-health/modalert-modafinil.html", True)
            , ("https://www.adamsmith.org/research/back-in-the-ussr", True)
            , ("https://www.adelaidenow.com.au/news/south-australia/man-arrested-after-220000-worth-of-wizard-drug-25inbome-intercepted/story-fni6uo1m-1227075205971", True)
            , ("https://www.aging-us.com/article/100415/text", True)
            , ("https://www.aiweirdness.com/this-is-the-openai-api-it-makes-spookily-20-06-11/", True)
            , ("https://www.alcor.org/library/chemical-brain-preservation/", True)
            , ("https://www.alexirpan.com/2015/09/24/how-an-audio-play-about-a-time-traveling-pony-turned-me-into-a-fanboy.html", True)
            , ("https://www.alicemaz.com/writing/minecraft.html", True)
            , ("https://www.alignmentforum.org/posts/Haawpd5rZrzkzvYRC/an-162-foundation-models-a-paradigm-shift-within-ai", True)
            , ("https://www.allencheng.com/starting-a-business-around-gpt-3-is-a-bad-idea/", True)
            , ("https://www.andzuck.com/blog/sfw/", True)
            , ("https://www.archive-it.org/", True)
            , ("https://www.artnome.com/news/2019/3/27/why-is-ai-art-copyright-so-complicated", True)
            , ("https://www.baka-tsuki.org/project/index.php?title=Interview_with_Tanigawa_Nagaru", True)
            , ("https://www.baltimoresun.com/news/crime/bs-md-silk-road-employee-20131107-story.html", True)
            , ("https://www.bartleby.com/205/45.html", True)
            , ("https://www.becker-posner-blog.com/2006/12/charitable-foundations--posners-comment.html", True)
            , ("https://www.belfasttelegraph.co.uk/news/republic-of-ireland/article30724056.ece", True)
            , ("https://www.bemmu.com/first-year-of-candy-japan", True)
            , ("https://www.benkuhn.net/11/", True)
            , ("https://www.blockchain.com/btc/address/1AZvaBEJMiK8AJ5GvfvLWgHjWgL59TRPGy", True)
            , ("https://www.bostonglobe.com/ideas/2014/03/15/the-poor-neglected-gifted-child/rJpv8G4oeawWBBvXVtZyFM/story.html", True)
            , ("https://www.brainpreservation.org/?path=prize", True)
            , ("https://www.broadinstitute.org/news/broad-institute-sequences-its-100000th-whole-human-genome-national-dna-day", True)
            , ("https://www.brookings.edu/opinions/2010/0501_youth_programs_sawhill.aspx", True)
            , ("https://www.bu.edu/globalbeat/nuclear/Kristensen1097.html", True)
            , ("https://www.buzzricksons.jp/", True)
            , ("https://www.c82.net/euclid/", True)
            , ("https://www.candyjapan.com/results-from-box-design-ab-test", True)
            , ("https://www.catalogtree.net/about", True)
            , ("https://www.cato-unbound.org/2007/09/10/robin-hanson/cut-medicine-half/", True)
            , ("https://www.cbsnews.com/stories/2011/03/16/business/main20043737.shtml", True)
            , ("https://www.cdc.gov/trendstatement/", True)
            , ("https://www.chicagotribune.com/news/ct-xpm-1989-02-28-8903090006-story.html", True)
            , ("https://www.chrisplaysgames.com/gadgets/2019/02/25/how-i-learned-to-stop-worrying-and-love-transfer-learning/", True)
            , ("https://www.chrisstucchio.com/blog/2014/equal_weights.html", True)
            , ("https://www.cia.gov/news-information/press-releases-statements/2014-press-releases-statements/cia-declassifies-agency-role-in-publishing-doctor-zhivago.html", True)
            , ("https://www.cleveland.com/lifestyles/2008/01/some_girl_scout_cookies_change.html", True)
            , ("https://www.clinicaltrials.gov/ct2/show/NCT03548935", True)
            , ("https://www.clinicaltrials.gov/show/NCT02140554", True)
            , ("https://www.cnn.com/2009/WORLD/asiapcf/06/05/japan.herbivore.men/index.html", True)
            , ("https://www.coderelay.io/fontemon.html", True)
            , ("https://www.cogmed.com/", True)
            , ("https://www.cogtest.com/coglib_demtest.html", True)
            , ("https://www.collectorsweekly.com/articles/cast-iron-doorstops/", True)
            , ("https://www.couriermail.com.au/news/queensland/gladstone/gladstone-man-arrested-in-worldwide-firearms-bust/news-story/f5bd03cfa834aad581b828fba8c07af2", True)
            , ("https://www.cram.com/", True)
            , ("https://www.cryonicscalculator.com/", True)
            , ("https://www.cs.dartmouth.edu/~sergey/wm/", True)
            , ("https://www.cs.odu.edu/~mln/pubs/widm-2006/lazyp-widm06.pdf", True)
            , ("https://www.cs.virginia.edu/~robins/YouAndYourResearch.html", True)
            , ("https://www.cylab.cmu.edu/", True)
            , ("https://www.daemonology.net/blog/2011-06-03-insecurity-in-the-jungle.html", True)
            , ("https://www.dafont.com/deutsche-zierschrif.font", True)
            , ("https://www.dagbladet.no/nyheter/politiet-mener-fire-norske-narkoselgere-har-omsatt-for-millioner-i-virtuell-valuta-pa-morkenettet/60700845", True)
            , ("https://www.dailydot.com/crime/dark-web-black-market-reloaded-adam-bunger-gun-sales-arrest/", True)
            , ("https://www.dailymail.co.uk/health/article-2126761/Bertold-Wiesner-British-scientist-fathered-600-children-donating-sperm-fertility-clinic.html", True)
            , ("https://www.dartmouth.edu/~milton/reading_room/conscience/text.shtml", True)
            , ("https://www.davidsongifted.org/Search-Database/entry/A10489", True)
            , ("https://www.deeplearningbook.org/contents/rnn.html", True)
            , ("https://www.depauw.edu/sfs/interviews/wolfe46interview.htm", True)
            , ("https://www.deseret.com/1995/7/20/19183190/teller-regrets-failure-to-seek-a-demonstration-option", True)
            , ("https://www.discoverbooks.com/", True)
            , ("https://www.discoverteas.com/246/218/tea/oolong-teas/p-glenburn-moonshine-oolong", True)
            , ("https://www.doc88.com/p-397166703921.html", True)
            , ("https://www.drmaciver.com/2019/05/how-to-do-hard-things/", True)
            , ("https://www.e-codices.unifr.ch/en/vad/0296/093v", True)
            , ("https://www.ecologyandsociety.org/vol9/iss1/art6/main.html", True)
            , ("https://www.econlib.org/archives/2012/03/the_roots_of_le.html", True)
            , ("https://www.economist.com/science-and-technology/2009/04/08/wired?story_id=13437729", True)
            , ("https://www.econtalk.org/matt-ridley-on-how-innovation-works/", True)
            , ("https://www.eduref.net/features/what-grades-can-ai-get-in-college/", True)
            , ("https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003wk#bboard_content", True)
            , ("https://www.equator-network.org/reporting-guidelines/", True)
            , ("https://www.equestriadaily.com/search/label/Music", True)
            , ("https://www.eugenewei.com/blog/2017/5/11/jpeg-your-ideas", True)
            , ("https://www.evamonkey.com/ask-john/has-evangelion-influenced-contemporary-gundam-anime.php", True)
            , ("https://www.explainxkcd.com/wiki/index.php/Randall", True)
            , ("https://www.fadedpage.com/showbook.php?pid=20160325#Page_107", True)
            , ("https://www.fast.ai/2018/08/10/fastai-diu-imagenet/", True)
            , ("https://www.fathomevents.com/series/the-met-live-in-hd", True)
            , ("https://www.fhi.ox.ac.uk/", True)
            , ("https://www.filfre.net/2020/01/master-of-orion/", True)
            , ("https://www.find-more-books.com/", True)
            , ("https://www.freakonomicsexperiments.com/", True)
            , ("https://www.frontiersin.org/articles/10.3389/fendo.2019.00845/full", True)
            , ("https://www.ft.com/content/da7b86a3-a8a7-4a0b-a26f-38abda7e7f86", True)
            , ("https://www.gawker.com/5926440/are-authorities-closing-in-on-the-online-drug-market-silk-road", True)
            , ("https://www.genealogy.math.ndsu.nodak.edu/id.php?id=42477", True)
            , ("https://www.genetics.org/content/genetics/144/1/205.full.pdf", True)
            , ("https://www.ghibli.jp/kazetachinu/character.html", True)
            , ("https://www.gizmodo.com.au/2020/05/the-internet-furry-drama-raising-big-questions-about-artificial-intelligence/", True)
            , ("https://www.globalsecurity.org/wmd/world/russia/arzamas_nuc.htm", True)
            , ("https://www.gnxp.com/WordPress/2017/12/12/most-people-say-they-think-nurture-is-more-important-than-nature-especially-white-americans/", True)
            , ("https://www.gq.com/story/fake-hitman-murder-for-hire", True)
            , ("https://www.grandforksherald.com/news/crime-and-courts/3704033-oregon-man-indicted-fatal-grand-forks-overdose-case", True)
            , ("https://www.gutenberg.org/files/5978/5978-h/5978-h.htm#c15", True)
            , ("https://www.harrowell.org.uk/blog/2018/01/31/in-the-eternal-inferno-fiends-torment-ronald-coase-with-the-fate-of-his-ideas/", True)
            , ("https://www.haskell.org/cabal/", True)
            , ("https://www.hbs.edu/faculty/Pages/default.aspx", True)
            , ("https://www.heraldsun.com.au/news/law-order/man-faces-jail-after-2700-drug-tablets-on-notorious-site-silk-road/news-story/4ec3a87919d9d591057f3d7494336dd0", True)
            , ("https://www.hpmor.com/chapter/64", True)
            , ("https://www.hsx.com/security/feature.php?type=opening", True)
            , ("https://www.html-tidy.org/", True)
            , ("https://www.humanprogress.org/", True)
            , ("https://www.iarpa.gov/index.php/research-programs/ace", True)
            , ("https://www.independent.co.uk/news/uk/home-news/smile-a-perfect-smile-but-don-t-laugh-we-have-officially-lost-our-dentures-1336158.html", True)
            , ("https://www.independent.ie/irish-news/suspected-online-international-drugs-ring-smashed-by-gardai-30724000.html", True)
            , ("https://www.infranken.de/lk/forchheim/im-darknet-mit-drogen-gehandelt-kripo-entlarvt-neun-maenner-aus-franken-art-1355609", True)
            , ("https://www.inkandswitch.com/index.html", True)
            , ("https://www.intechopen.com/source/html/17370/media/image3.jpeg", True)
            , ("https://www.iqtest.com/", True)
            , ("https://www.itmedia.co.jp/news/articles/1711/28/news020.html", True)
            , ("https://www.jamesfadiman.com/", True)
            , ("https://www.japaninc.com/cpj/magazine/issues/1998/mar98/ohsuga.html", True)
            , ("https://www.jdmoyer.com/2010/03/04/sleep-experiment-a-month-with-no-artificial-light/", True)
            , ("https://www.jefftk.com/p/breaking-down-cryonics-probabilities", True)
            , ("https://www.joelonsoftware.com/2000/11/22/20001122/", True)
            , ("https://www.johndcook.com/blog/2011/11/22/norris-number/", True)
            , ("https://www.johnsonessays.com/the-rambler/no-21-the-anxieties-of-literature-not-less-than-those-of-publick-stations-the-inequality-of-authors-writings/", True)
            , ("https://www.justinpinkney.com/stylegan-network-blending/", True)
            , ("https://www.kalzumeus.com/2012/08/13/doubling-saas-revenue/", True)
            , ("https://www.karger.com/Article/Abstract/119229", True)
            , ("https://www.khara.co.jp/hideakianno/personal-biography/", True)
            , ("https://www.kooslooijesteijn.net/blog/semantic-sidenotes", True)
            , ("https://www.ledger-cli.org/", True)
            , ("https://www.lehighvalleylive.com/bethlehem/2015/02/west_bethlehem_drugraid_prompt.html", True)
            , ("https://www.lesswrong.com/posts/GytPrQ9cT46k9etoz/living-forever-is-hard-or-the-gompertz-curve", True)
            , ("https://www.lesswrong.com/posts/wTKjRFeSjKLDSWyww/possible-takeaways-from-the-coronavirus-pandemic-for-slow-ai", True)
            , ("https://www.librarything.com/work/13068", True)
            , ("https://www.lifeview.com/epgt", True)
            , ("https://www.links.org/?p=1171#comment-415465", True)
            , ("https://www.livestrong.com/article/283811-vitamin-d-deficiency-heart-palpitations/", True)
            , ("https://www.mail-archive.com/cryptography@metzdowd.com/msg09975.html", True)
            , ("https://www.math.uwaterloo.ca/tsp/pla85900/index.html", True)
            , ("https://www.mcall.com/news/police/mc-n-east-stroudsburg-dark-web-lsd-20160505-story.html", True)
            , ("https://www.mediawiki.org/wiki/Multilingual_MediaWiki", True)
            , ("https://www.memteaimports.com/tea/bei-dou", True)
            , ("https://www.mentalfloss.com/article/94569/clair-patterson-scientist-who-determined-age-earth-and-then-saved-it", True)
            , ("https://www.mercurynews.com/2016/01/27/san-jose-former-postal-inspector-to-serve-3-years-for-possessing-stolen-mail-marijuana-trafficking/", True)
            , ("https://ralphmerkle.com/cryo/techFeas.html", True)
            , ("https://www.metafilter.com/91797/working-working-memory-with-dual-nback#3108845", True)
            , ("https://www.metopera.org/season/in-cinemas/2019-20-season/akhnaten-live-in-hd/", True)
            , ("https://www.metzdowd.com/pipermail/cryptography/2015-January/024433.html", True)
            , ("https://www.miamiherald.com/news/local/community/miami-dade/article199044679.html", True)
            , ("https://www.microdose.me/", True)
            , ("https://www.motherjones.com/politics/2016/06/cca-private-prisons-corrections-corporation-inmates-investigation-bauer/", True)
            , ("https://www.nationalgeographic.com/magazine/article/evolution-genetics-medicine-brain-technology-cyborg", True)
            , ("https://www.nbcnews.com/storyline/isis-terror/isis-head-baghdadi-wife-fell-love-line-say-sources-n260291", True)
            , ("https://www.newsday.com/news/region-state/robert-c-cardillo-jr-picked-up-12-pounds-of-pot-worth-50-000-from-post-office-cops-say-a50126", True)
            , ("https://www.newstatesman.com/politics/uk-politics/2017/06/many-terrorists-first-victims-are-their-wives-were-not-allowed-talk-about", True)
            , ("https://www.nextbigfuture.com/2011/03/global-health-risks.html", True)
            , ("https://www.nextplatform.com/2019/08/20/big-blue-open-sources-power-chip-instruction-set/", True)
            , ("https://www.noisebridge.net/wiki/Compass_Vibro_Anklet", True)
            , ("https://www.nplusonemag.com/issue-3/reviews/adventures-of-a-man-of-science/", True)
            , ("https://www.npr.org/sections/alltechconsidered/2017/06/08/531796329/eager-to-burst-his-own-bubble-a-techie-made-apps-to-randomize-his-life", True)
            , ("https://www.nps.gov/subjects/bears/safety.htm", True)
            , ("https://www.nydailynews.com/news/national/texas-man-arrested-dark-web-attempt-buy-explosives-article-1.2800015", True)
            , ("https://www.ohyouprettythings.com/free", True)
            , ("https://www.orbuch.com/nets-reading-list/", True)
            , ("https://www.oregonlive.com/portland/2015/08/players_in_biggest_silk_road_m.html", True)
            , ("https://www.oricon.co.jp/news/2075319/full/", True)
            , ("https://www.orlandosentinel.com/news/breaking-news/os-ucf-student-marijuana-arrest-towers-20150216-story.html", True)
            , ("https://www.otakustudy.com/books/2020/08/skeb-artwork-commissioning-website-review/", True)
            , ("https://www.overcomingbias.com/2014/10/why-not-egg-futures.html", True)
            , ("https://www.owenstephens.co.uk/blog/2011/09/03/gsoc_darcs_bridge__results.html", True)
            , ("https://www.pcmag.com/archive/how-a-chip-fab-is-built-249666", True)
            , ("https://www.pewresearch.org/social-trends/2012/02/16/the-rise-of-intermarriage/", True)
            , ("https://www.poetryfoundation.org/poems/45173/jubilate-agno", True)
            , ("https://www.politico.com/magazine/story/2018/11/11/republican-party-anti-pornography-politics-222096/", True)
            , ("https://www.popsci.com/woolly-mammoth-dna-brought-life-elephant-cells/", True)
            , ("https://www.pragmatic.ml/a-survey-of-methods-for-incorporating-long-term-context/", True)
            , ("https://www.princeton.edu/~graphicarts/2012/04/feuillets_dart.html", True)
            , ("https://www.prolific.co/", True)
            , ("https://www.proquest.com/docview/305288545", True)
            , ("https://www.psychiatryinvestigation.org/journal/view.php?number=865", True)
            , ("https://www.punchlinedesign.net/pun_generator", True)
            , ("https://www.r-bloggers.com/2014/01/visualization-series-using-scatterplots-and-models-to-understand-the-diamond-market-so-you-dont-get-ripped-off/", True)
            , ("https://www.rationaloptimist.com/blog/the-wealth-of-urban-biodiversity/", True)
            , ("https://www.rdocumentation.org/packages/EnvStats/versions/2.1.0/topics/evNormOrdStats", True)
            , ("https://www.reg.ru/blog/anime-generation-with-stylegan/", True)
            , ("https://www.reuters.com/article/2011/11/26/us-vitamind-heartdisease-idUSTRE7AO1UM20111126", True)
            , ("https://www.richardcarrier.info/archives/14522", True)
            , ("https://www.rifters.com/crawl/?p=791", True)
            , ("https://www.rosebud.ai/", True)
            , ("https://www.sacbee.com/news/local/crime/article2598075.html", True)
            , ("https://www.salon.com/2007/04/12/castaneda/", True)
            , ("https://www.sankakucomplex.com/2011/11/01/which-are-worse-the-seiyuu-or-their-creepy-fans/", True)
            , ("https://www.sapa-project.org/", True)
            , ("https://www.schneier.com/blog/archives/2008/10/the_seven_habit.html", True)
            , ("https://www.scielo.br/j/rbp/a/fCXVCnz7PGRpbwNgX6DkJwC/?format=pdf", True)
            , ("https://www.sciencenews.org/view/generic/id/65600/title/Fish_oil_fails_to_hold_off_heart_arrhythmia", True)
            , ("https://www.sciencenewsline.com/medicine/2012010922310082.html", True)
            , ("https://www.scottaaronson.com/democritus/", True)
            , ("https://www.sfgate.com/news/article/viral-video-bear-dogs-terriers-california-home-16104193.php", True)
            , ("https://www.shawlocal.com/northwest-herald/2016/06/27/kane-county-sheriffs-deputy-wife-of-ex-mchenry-county-undersheriff-faces-drug-charges/axibe17/", True)
            , ("https://www.shawwn.com/swarm", True)
            , ("https://www.siliconera.com/", True)
            , ("https://www.simplify.so/", True)
            , ("https://www.sirlin.net/articles/playing-to-win", True)
            , ("https://www.sjsu.edu/faculty/watkins/longevity.htm", True)
            , ("https://www.snpedia.com/index.php/Genomes", True)
            , ("https://www.spiegel.de/panorama/ermittler-zerschlagen-internationalen-drogenring-a-910221.html", True)
            , ("https://www.spiked-online.com/2014/11/18/a-colourful-history-of-progress/", True)
            , ("https://www.spring.org.uk/2014/11/autism-new-studies-identify-dozens-more-associated-genes.php", True)
            , ("https://www.sqlite.org/fasterthanfs.html", True)
            , ("https://www.ssc.wisc.edu/wlsresearch/about/description.php", True)
            , ("https://www.standard.co.uk/news/crime/drugdealing-prison-librarian-bought-gun-on-the-dark-web-a3259961.html", True)
            , ("https://www.statnews.com/2020/09/24/crows-possess-higher-intelligence-long-thought-primarily-human/", True)
            , ("https://www.straighttalkonevidence.org/2017/09/22/disappointing-findings-on-conditional-cash-transfers-as-a-tool-to-break-the-poverty-cycle-in-the-united-states/", True)
            , ("https://www.stripes.com/news/pet-cloning-lab-in-s-korea-starts-military-dog-program-1.425640", True)
            , ("https://www.stuff.co.nz/national/crime/10232509/Doing-time-for-drugs-bought-online", True)
            , ("https://www.sumsar.net/blog/2015/04/the-non-parametric-bootstrap-as-a-bayesian-model/", True)
            , ("https://www.sun-modalert.com/faq.php#shipping", True)
            , ("https://www.supermemo.com/en/archives1990-2015/english/ol/sm2", True)
            , ("https://www.syracuse.com/crime/2015/05/cops_seize_809_pills_170k_in_cash_in_raid_suny_oswego_student_charged.html", True)
            , ("https://www.systutorials.com/docs/linux/man/1-midi2abc/", True)
            , ("https://www.tabletmag.com/sections/news/articles/orthodox-jews-attacked-brooklyn-hate-crime", True)
            , ("https://www.talyarkoni.org/blog/2011/01/10/the-psychology-of-parapsychology-or-why-good-researchers-publishing-good-articles-in-good-journals-can-still-get-it-totally-wrong/", True)
            , ("https://www.technologyreview.com/2020/02/17/844721/ai-openai-moonshot-elon-musk-sam-altman-greg-brockman-messy-secretive-reality/", True)
            , ("https://www.the-scientist.com/features/can-destroying-senescent-cells-treat-age-related-disease--67136", True)
            , ("https://www.thecut.com/2019/05/the-tinder-hacker.html", True)
            , ("https://www.thedenverchannel.com/news/colorado-springs-area/air-force-academy-cadet-3rd-class-nathaniel-penalosa-accused-of-using-and-distributing-drugs-on-base", True)
            , ("https://www.thefreelibrary.com/Sleep+debt+exacts+deceptive+cost.+%28Behavior%29.-a0100110931", True)
            , ("https://www.theindiaforum.in/article/revolt-upper-castes", True)
            , ("https://www.themtank.org/a-year-in-computer-vision", True)
            , ("https://www.theparisreview.org/interviews/4155/the-art-of-the-essay-no-1-e-b-white", True)
            , ("https://www.theregister.com/2001/01/10/nsa_runs_best_fab/", True)
            , ("https://www.thestranger.com/seattle/the-lying-disease/Content?oid=15337239", True)
            , ("https://www.thisfursonadoesnotexist.com/", True)
            , ("https://www.thispersondoesnotexist.com/", True)
            , ("https://www.thisworddoesnotexist.com/", True)
            , ("https://www.thriftbooks.com/", True)
            , ("https://www.tomshardware.com/news/google-removing-minix-management-engine-intel,35876.html", True)
            , ("https://www.tor.com/2012/09/26/day-of-the-kraken/", True)
            , ("https://www.tranquiltuesdays.com/product/white-peony-white-tea/", True)
            , ("https://www.trentonbricken.com/Tail-Free-Sampling/", True)
            , ("https://www.trubrain.com/", True)
            , ("https://www.tt-forums.net/viewtopic.php?f=29&t=37902&sid=75000c8f5f3b17f607335077dad6ce94&view=print", True)
            , ("https://www.ubu.com/concept/racter.html", True)
            , ("https://www.uliwestphal.de/elephas-anthropogenus/index.html", True)
            , ("https://www.unf.edu/mudlark/posters/hartzler.html", True)
            , ("https://www.unicode.org/history/publicationdates.html", True)
            , ("https://www.unqualified-reservations.org/2007/08/james-burnhams-dante-politics-as-wish/", True)
            , ("https://www.unz.com/gnxp/the-cost-of-inbreeding-in-terms-of-health/", True)
            , ("https://www.urbandictionary.com/define.php?term=Death%20flag", True)
            , ("https://www.usnews.com/mobile/articles_mobile/computers-might-make-learning-lip-reading-easier/index.html", True)
            , ("https://www.vanityfair.com/culture/2010/02/exile-201002", True)
            , ("https://www.vesta.earth/", True)
            , ("https://www.vocativ.com/interactive/underworld/drugs/darknet-arrests-map/", True)
            , ("https://www.w3.org/International/wiki/Case_folding", True)
            , ("https://www.wakapoetry.net/mys-viii-1426/", True)
            , ("https://www.washingtonpost.com/news/worldviews/wp/2014/12/17/denmark-stakes-its-claim-in-the-war-for-the-north-pole/", True)
            , ("https://www.washingtontimes.com/news/2018/jun/2/minnesota-girl-who-cant-feel-pain-battles-insuranc/", True)
            , ("https://www.webmd.com/prostate-cancer/news/20090324/fatty-fish-may-cut-prostate-cancer-risk", True)
            , ("https://www.wheels.org/spacewar/stone/rolling_stone.html", True)
            , ("https://www.whichfaceisreal.com/", True)
            , ("https://www.winehq.org/pipermail/wine-devel/2002-February/003912.html", True)
            , ("https://www.wired.co.uk/article/lsd-microdosing-drugs-silicon-valley", True)
            , ("https://www.wired.com/story/sleep-no-more-crusade-genetic-killer/", True)
            , ("https://www.wolfewiki.com/pmwiki/pmwiki.php?n=WolfeWiki.Introduction", True)
            , ("https://www.writingroutines.com/routines/", True)
            , ("https://www.wsj.com/articles/SB10001424052702304432304576371462612272884", True)
            , ("https://www.wtnh.com/2014/11/06/fbi-agents-raid-west-haven-home/", True)
            , ("https://www.yalelawjournal.org/note/amazons-antitrust-paradox", True)
            , ("https://www.yf.io/p/lsun", True)
            , ("https://www.yudkowsky.net/", True)
            , ("https://web.archive.org/web/20140314232216/https://www.zeit.de/2014/12/drogenhandel-silk-road-pfandleiher", True)
            , ("https://xkcd.com/481/", True)
            , ("https://xtools.wmflabs.org/pages/index.php?name=Rhwawn&lang=en&wiki=wikipedia&namespace=0&redirects=noredirects", True)
            , ("https://zlkj.in/", True)
            , ("https://www.cs.toronto.edu/~kriz/cifar.html", True)
            ]

badLinks = [("https://1d4chan.org/wiki/Tale_of_an_Industrious_Rogue,_Part_I", False)
            , ("/docs/iq/2004-gottfredson.pdf", False)
            , ("/docs/sociology/2013-feldman.pdf", False)
            , ("/docs/statistics/bayes/2003-korb.pdf", False)
            , ("/docs/sunk-cost/2005-parker.pdf", False)
            , ("/docs/vitamin-d/2005-grant.pdf", False)
            , ("/images/gan/stylegan/2021-li-dplase-ganlatentspaceeditingvideo.mp4", False)
            , ("http://2chan.us/wordpress/2012/07/12/c82-genre-stats/", False)
            , ("http://antiagingcentral.com/store/catalog/index.php?cPath=29", False)
            , ("http://archinte.ama-assn.org/cgi/content/abstract/172/6/494", False)
            , ("http://archive.foolz.us/a/thread/77196171/#77207238", False)
            , ("http://ascii.textfiles.com/archives/1717", False)
            , ("http://augmentingcognition.com/ltm.html", False)
            , ("http://aurellem.org/vba-clojure/html/total-control.html", False)
            , ("http://authenticorganizations.com/harquail/2009/08/03/wal-mart-knocks-off-the-girl-scouts/#comment-1214", False)
            , ("http://bactra.org/weblog/algae-2012-09.html", False)
            , ("http://bakabt.me/159362-umineko-no-naku-koro-ni-music-collection-flac.html", False)
            , ("http://bastiat.org/en/twisatwins.html", False)
            , ("http://betsofbitco.in/list?status=available&category=All&sorting=-moderationTime", False)
            , ("http://bit-player.org/2021/three-months-in-monte-carlo", False)
            , ("http://bitcoinweekly.com/", False)
            , ("http://bjp.rcpsych.org/cgi/content/full/179/1/46", False)
            , ("http://blog.dansimons.com/2013/03/what-effect-size-would-you-expect.html", False)
            , ("http://blog.darcs.net/2010/11/coming-in-darcs-28-new-features.html", False)
            , ("http://blog.platypope.org/2012/4/5/restore-features/", False)
            , ("http://blog.sigfpe.com/2012/12/shuffles-bayes-theorem-and-continuations.html", False)
            , ("http://boinngerionn.blog.fc2.com/blog-entry-203.html", False)
            , ("http://book.realworldhaskell.org/read/data-structures.html#id637702", False)
            , ("http://book.webtypography.net/", False)
            , ("http://boston.conman.org/2013/01/22.2", False)
            , ("http://brainworkshop.sourceforge.net/tutorial.html", False)
            , ("http://bugs.darcs.net/issue346", False)
            , ("http://buttercupfestival.com/green/index.htm", False)
            , ("http://catb.org/~esr/writings/taoup/html/ch05s01.html", False)
            , ("http://catless.ncl.ac.uk/risks/16.41.html", False)
            , ("http://chronopause.com/chronopause.com/index.php/2011/02/11/thus-spake-curtis-henderson-part-5/", False)
            , ("http://clagnut.com/blog/2395", False)
            , ("http://clickotron.com/", False)
            , ("http://colab.research.google.com", False)
            , ("http://danfrank.ca/the-most-loved-and-hated-classics-according-to-goodreads-users/", False)
            , ("http://darcs.net/GSoC/2010-Cache", False)
            , ("http://darkdata.bc.edu/", False)
            , ("http://darwin-online.org.uk/content/frameset?pageseq=1&itemID=F1548.1&viewtype=text", False)
            , ("http://dev.kanotype.net:8003/deepdanbooru/", False)
            , ("http://duplicity.nongnu.org/", False)
            , ("http://eepurl.com/cGAN7L", False)
            , ("http://eprints.lincoln.ac.uk/id/eprint/1932/1/MetaAnalysisPaper.pdf", False)
            , ("http://eprints.nottingham.ac.uk/439/1/Willmot_NO_synthase_JFRBM.pdf", False)
            , ("http://esr.ibiblio.org/?p=7183", False)
            , ("http://eurfa.org.uk/pangur_ban.php", False)
            , ("http://evacommentary.org/episode-01/episode-01A-scene3.html", False)
            , ("http://eztest.com/ez-test-tubes/", False)
            , ("http://folding.stanford.edu/English/FAQ-Diseases", False)
            , ("http://forum.bitcoin.org/index.php?topic=29737.0", False)
            , ("http://ftp.cs.ucla.edu/pub/stat_ser/r414.pdf", False)
            , ("http://gainax.co.jp/", False)
            , ("http://gainax.fr/", False)
            , ("http://garote.bdmonkeys.net/commandline/index.html", False)
            , ("http://geneatlas.roslin.ed.ac.uk/", False)
            , ("http://geroprotectors.org/?page=8&q%5Bs%5D=organism_name+asc", False)
            , ("http://gettermario.dynamicforum.net/t974p15-entretien-go-nagai-hideaki-anno", False)
            , ("http://glench.com/closed-source/dictionaryofnumbers/", False)
            , ("http://gptprompts.wikidot.com/linguistics:word-in-context", False)
            , ("http://grantland.com/one-hundred-years-arm-bars-gracie-jiu-jitsu-mma/", False)
            , ("http://hn-sicp.pbworks.com/w/page/9077106/FrontPage", False)
            , ("http://hoaxes.org/archive/permalink/the_great_moon_hoax", False)
            , ("http://homepage3.nifty.com/mana/miyazaki-annno.html", False)
            , ("http://host.robots.ox.ac.uk/pascal/VOC/", False)
            , ("http://ignorethecode.net/blog/2010/04/20/footnotes/", False)
            , ("http://images.google.com/images?amp;q=henry+darger&gbv=2&biw=1218&bih=673", False)
            , ("http://incompleteideas.net/sutton/book/the-book.html", False)
            , ("http://itre.cis.upenn.edu/~myl/languagelog/archives/005526.html", False)
            , ("http://jidinews.com/innernews/innernews/event/1204.html", False)
            , ("http://jn.nutrition.org/content/141/2/261", False)
            , ("http://kanzaki.sub.jp/archives/000282.html", False)
            , ("http://latanyasweeney.org/cv.html#survey", False)
            , ("http://ldsc.broadinstitute.org/about/", False)
            , ("http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids", False)
            , ("http://libgen.org/search.php?req=%22wheel+of+time%22", False)
            , ("http://linuxmafia.com/faq/Essays/marriage.html", False)
            , ("http://lists.urth.net/pipermail/urth-urth.net/2010-December/019137.html", False)
            , ("http://ludix.com/moriarty/paul.html", False)
            , ("http://lukemuehlhauser.com/industrial-revolution/", False)
            , ("http://markets.nitle.org/markets", False)
            , ("http://medsforbitcoin.com/", False)
            , ("http://messybeast.com/cats-meat-man.htm", False)
            , ("http://nectarcreek.com/", False)
            , ("http://neomarxisme.com/biography.html", False)
            , ("http://neuralnetworksanddeeplearning.com/", False)
            , ("http://new.nubrain.com/shop/index.php?route=product/product&keyword=modafinil&category_id=0&product_id=58", False)
            , ("http://news.bbc.co.uk/2/hi/uk_news/3723839.stm", False)
            , ("http://npc.people.com.cn/n1/2020/1227/c14576-31980014.html", False)
            , ("http://nrl.northumbria.ac.uk/id/eprint/875/1/Scholey_Consumption%20of%20cocoa%20flavanols.pdf", False)
            , ("https://omega0.xyz/omega8008/JaynesBookPdf.html", False)
            , ("http://p2pfoundation.ning.com/forum/topics/bitcoin-open-source", False)
            , ("http://pcdb.santafe.edu/index.php", False)
            , ("http://people.tamu.edu/~stevesmith/SmithMemory/SmithRothkopf1984.pdf", False)
            , ("http://permalink.gmane.org/gmane.science.linguistics.wikipedia.english/110790", False)
            , ("http://philsci-archive.pitt.edu/5314/1/Griffiths_%26_Wilkins.doc", False)
            , ("http://plaza.harmonix.ne.jp/~onizuka/literal/EVA26.txt", False)
            , ("http://plewis.info/", False)
            , ("http://popsych.org/should-psychological-neuroscience-research-be-funded/", False)
            , ("http://precedings.nature.com/documents/3697/version/1", False)
            , ("http://prize.hutter1.net/", False)
            , ("http://publicsearch.ndcourts.gov/default.aspx", False)
            , ("http://pubs.acs.org/doi/abs/10.1021/es0719071", False)
            , ("http://radar.oreilly.com/2011/05/anonymize-data-limits.html", False)
            , ("http://repec.org/sed2006/up.30684.1139268077.pdf", False)
            , ("http://rstb.royalsocietypublishing.org/content/365/1537/73.full", False)
            , ("http://secretlaboratory.org/?p=9543", False)
            , ("http://serendipityrecs.com/", False)
            , ("http://sethroberts.net/2011/01/29/the-buttermind-experiment/", False)
            , ("http://sigbovik.org/2019/", False)
            , ("http://ssgac.org/documents/CHIC_Summary_Benyamin2014.txt.gz", False)
            , ("http://stroke.ahajournals.org/content/39/10/2824.full", False)
            , ("http://subs2srs.sourceforge.net/", False)
            , ("http://swombat.com/2012/2/27/modafinil-and-startups", False)
            , ("http://thecodelesscode.com/case/96", False)
            , ("http://thepharmacyexpress.com/Products2.asp?Brand=MODALERT+%28+Provigil%2C+Modapro%2C+Modvigil%2C+Generic+Modafinil+%29&T=d", False)
            , ("http://thesecatsdonotexist.com/", False)
            , ("http://thismarketingblogdoesnotexist.com/", False)
            , ("http://tom7.org/mario/", False)
            , ("http://torch.ch/", False)
            , ("http://uberty.org/wp-content/uploads/2015/07/Norbert_Wiener_Cybernetics.pdf", False)
            , ("http://umichrl.pbworks.com/w/page/7597597/Successes%20of%20Reinforcement%20Learning", False)
            , ("http://vanishingpoint.air-nifty.com/blog/2008/08/post_7c10.html", False)
            , ("http://waifu2x.udp.jp/", False)
            , ("http://whatis.suburbansenshi.com/index.php?title=Gemini_Sunrise", False)
            , ("http://worrydream.com/LearnableProgramming/", False)
            , ("http://www-biba.inrialpes.fr/Jaynes/cc18i.pdf", False)
            , ("http://www.aaronsw.com/weblog/000404", False)
            , ("http://www.abetterpage.com/wt/euro/BraunT3.html", False)
            , ("http://www.ajnr.org/content/33/1/49.full", False)
            , ("http://www.alljapaneseallthetime.com/blog/you-dont-have-a-foreign-language-problem-you-have-an-adult-literacy-problem/", False)
            , ("http://www.animenewsservice.com/archives/dec13.htm", False)
            , ("http://www.bartokdesign.com/japan/0-blog_news/spherical_drain_plug_1.php", False)
            , ("http://www.bayesianinvestor.com/blog/index.php/2008/11/13/automated-market-maker-results/", False)
            , ("http://www.berlin.de/polizei/polizeimeldungen/pressemitteilung.261025.php", False)
            , ("http://www.bronxbanterblog.com/2013/10/01/the-power-and-the-gory/", False)
            , ("http://www.buffalonews.com/city-region/police-courts/former-ub-student-admits-using-bitcoins-while-dealing-in-drugs-20160823", False)
            , ("http://www.buttercupfestival.com/poetry.htm", False)
            , ("http://www.byrnehobart.com/blog/why-are-middlebrow-dismissals-so-tempting/", False)
            , ("http://www.cap-lore.com/Hardware/Wheel.html", False)
            , ("http://www.catb.org/jargon/html/R/religious-issues.html", False)
            , ("http://www.cehd.umn.edu/CAREI/Reports/summary.html#SchoolStart", False)
            , ("http://www.collisiondetection.net/mt/archives/2013/07/wired_love_a_ta.php", False)
            , ("http://www.comicbox.co.jp/e-nau/toren.html", False)
            , ("http://www.cs.cornell.edu/~caruana/compression.kdd06.pdf", False)
            , ("http://www.dailyfinance.com/2010/02/06/girl-scout-cookies-by-the-numbers-just-where-do-all-those-dolla/", False)
            , ("http://www.dcscience.net/2015/12/11/placebo-effects-are-weak-regression-to-the-mean-is-the-main-reason-ineffective-treatments-appear-to-work/", False)
            , ("http://www.ditext.com/moore/common-sense.html", False)
            , ("http://www.dobuusagi.com/", False)
            , ("http://www.dummy-system.com/2013/04/23/intervista-a-maaya-sakamoto-su-evangelion-3-0/", False)
            , ("http://www.ecns.cn/news/cns-wire/2019-03-19/detail-ifzfmzhu2193677.shtml", False)
            , ("http://www.elon.edu/docs/e-web/academics/business/economics/faculty/rouse/2011-02-14%20The%20Impact%20of%20year%20round%20schooling.pdf", False)
            , ("http://www.epjournal.net/blog/2011/08/glucose-is-not-willpower-fuel/", False)
            , ("http://www.eusprig.org/horror-stories.htm", False)
            , ("http://www.evalegend.com/interview_anno97.php", False)
            , ("http://www.expert-reviews.com/doi/full/10.1586/ern.12.36", False)
            , ("http://www.faqs.org/faqs/compression-faq/", False)
            , ("http://www.fasebj.org/cgi/content/meeting_abstract/26/1_MeetingAbstracts/114.4", False)
            , ("http://www.ff7citadel.com/press/int_edge.shtml", False)
            , ("http://www.foxcarolina.com/story/28631102/teen-arrested-after-allegedly-buying-lsd-online", False)
            , ("http://www.genwaybio.com/recombinant-human-beta-ngf", False)
            , ("http://www.getlamp.com/", False)
            , ("http://www.hakalalabs.com/testing.html", False)
            , ("http://www.highbeam.com/doc/1P2-869220.html", False)
            , ("http://www.holidaymead.com/#!product-page/cba4/f8f43032-665c-5a4c-6756-1ac355b32549", False)
            , ("http://www.imminst.org/", False)
            , ("http://www.infinitychess.com/Page/Public/Article/DefaultArticle.aspx?id=118", False)
            , ("http://www.infinityplus.co.uk/stories/colderwar.htm", False)
            , ("http://www.iqout.com/", False)
            , ("http://www.iqtest.dk/main.swf", False)
            , ("http://www.irrodl.org/index.php/irrodl/article/view/1928/3131", False)
            , ("http://www.isfdb.org/cgi-bin/pl.cgi?261005", False)
            , ("http://www.isteve.com/ManlyMolecule.htm", False)
            , ("http://www.jackkinsella.ie/articles/janki-method", False)
            , ("http://www.jacurutu.com/viewtopic.php?f=21&t=1112#p34878", False)
            , ("http://www.jacurutu.com/viewtopic.php?t=2690", False)
            , ("http://www.kptv.com/story/15084456/battle-ground-schools-let-students-sleep-in", False)
            , ("http://www.locusmag.com/2002/Issue09/GaimanWolfe.html", False)
            , ("http://www.longevityhistory.com/read-the-book-online/", False)
            , ("http://www.mangauk.com/?p=annos-dominus", False)
            , ("http://www.mania.com/aodvb/showthread.php?p=1502313", False)
            , ("http://www.mayofamily.com/RLM/txt_Clarke_Superiority.html", False)
            , ("http://www.mediafire.com/error.php?errno=320&origin=download", False)
            , ("http://www.mindsparke.com/", False)
            , ("http://www.mit.edu/people/rei/manga-inoue.html", False)
            , ("http://www.nausicaa.net/miyazaki/interviews/aboutanime.html", False)
            , ("http://www.neuroscience.cam.ac.uk/publications/download.php?id=11205", False)
            , ("http://www.nicovideo.jp/watch/sm22678844", False)
            , ("http://www.nicovideo.jp/watch/sm23047550", False)
            , ("http://www.nixnote.org/", False)
            , ("http://www.northjersey.com/news/222120551_High_schools__early_starts_deprive_students_of_crucial_sleep.html?page=all", False)
            , ("http://www.nyaa.se/?page=view&tid=438733", False)
            , ("http://www.ohri.ca/programs/clinical_epidemiology/nos_manual.pdf", False)
            , ("http://www.oliverwinery.com/index.cfm?method=storeproducts.showdrilldown&productid=565D8BC6-1CC4-FBB6-23C6-013703DB5D6A", False)
            , ("http://www.paulgraham.com/stuff.html", False)
            , ("http://www.polizei.sachsen.de/de/MI_2015_34938.htm", False)
            , ("http://www.postandcourier.com/article/20130709/PC16/130709464/1177/lawyer-charleston-man-denies-connection-to-dea-bitcoin-seizure-and-illicit-silk-road-drug-marketplace", False)
            , ("http://www.progressive.org/images/pdf/1179.pdf", False)
            , ("http://www.projectrho.com/public_html/rocket/spacegunexotic.php#propulsion", False)
            , ("http://www.protoculture.ca/PA/edito42.htm", False)
            , ("http://www.ratbehavior.org/DumboRatMutation.htm", False)
            , ("http://www.rean-wings.net/chara01.html", False)
            , ("http://www.regruntled.com/2008/10/21/selling-delusion-short/", False)
            , ("http://www.replicatedtypo.com/chocolate-consumption-traffic-accidents-and-serial-killers/5718.html", False)
            , ("http://www.roangelo.net/logwitt/logwitt5.html", False)
            , ("http://www.rocketpunk-manifesto.com/2009/06/space-warfare-i-gravity-well.html", False)
            , ("http://www.scholarpedia.org/article/N-body_choreographies", False)
            , ("http://www.scifiscripts.com/scripts/2001.txt", False)
            , ("http://www.sebastianmarshall.com/a-slightly-more-cautious-take-on-modafinil", False)
            , ("http://www.seistronix.com/", False)
            , ("http://www.sequentialtart.com/archive/mar04/cv_0304_2.shtml", False)
            , ("http://www.sfawardswatch.com/?page_id=106", False)
            , ("http://www.silcom.com/~barnowl/chain-letter/evolution.html#3-5origin_of_testimonials", False)
            , ("http://www.sld.cu/galerias/pdf/sitios/revsalud/observational_research,_randomised.pdf", False)
            , ("http://www.smarternootropics.com/2012/01/cephalon-europe-discontinues-olmifon-adrafinil/", False)
            , ("http://www.smarthome.com/seco-larm-sm-226l-garage-door-contacts-for-closed-circuits.html", False)
            , ("http://www.smartpowders.com/p-5327-l-theanine-powder-20-grams.aspx", False)
            , ("http://www.snpp.com/", False)
            , ("http://www.southwales-eveningpost.co.uk/Gorseinon-man/story-28084851-detail/story.html", False)
            , ("http://www.stuartcheshire.org/rants/latency.html", False)
            , ("http://www.teanobi.com/category_s/228.htm", False)
            , ("http://www.theannals.com/content/45/4/476", False)
            , ("http://www.thinkinginanutshell.com/nootropics", False)
            , ("http://www.trixietracker.com/pricing/", False)
            , ("http://www.unitedpharmacies-uk.md/Modavigil-Modafinil-100mg-30-Tablets-p-759.html", False)
            , ("http://www.usagi.org/doi/seiyuu/radio/boogie/19970118.html", False)
            , ("http://www.wangafu.net/~nickm/", False)
            , ("http://www.wdaz.com/news/3691219-number-defendants-charged-fentanyl-overdoses-5", False)
            , ("http://www.weidai.com/bmoney.txt", False)
            , ("http://www.whirlpool.com/-%5BACQ189XS%5D-1004030/ACQ189XS/", False)
            , ("http://www.wikiwix.com/", False)
            , ("http://www.willatworklearning.com/2005/11/research_review.html", False)
            , ("http://www.williamsburgmarketplace.com/webapp/wcs/stores/servlet/ProductView?categoryId=14332&catalogId=12122", False)
            , ("http://www.xinhuanet.com/english/2019-08/23/c_138332084.htm", False)
            , ("http://www2003.org/cdrom/papers/refereed/p097/P97%20sources/p97-fetterly.html", False)
            , ("http://zerocoin.org/", False)
            , ("https://5onwnspjvuk7cwvk.tor2web.org/", False)
            , ("https://a16z.com/author/scott-kupor/", False)
            , ("https://about.netflix.com/en/news/2017-on-netflix-a-year-in-bingeing", False)
            , ("https://academic.oup.com/schizophreniabulletin/article/33/6/1277/1902355", True)
            , ("https://adage.com/node/1206366/printable/print", False)
            , ("https://adcohearing.com/bvs_hear_loss.html", False)
            , ("https://addons.mozilla.org/en-US/firefox/addon/lastpass-password-manager/", False)
            , ("https://advancedfertility.com/2020/08/17/weight-bmi-and-fertility-and-ivf-success/", False)
            , ("https://aeon.co/essays/why-fake-miniatures-depicting-islamic-science-are-everywhere", False)
            , ("https://ai-alignment.com/", False)
            , ("https://ai.facebook.com/blog/harmful-content-can-evolve-quickly-our-new-ai-system-adapts-to-tackle-it", False)
            , ("https://aidungeon.medium.com/ai-dungeon-dragon-model-upgrade-7e8ea579abfe", False)
            , ("https://ajcn.nutrition.org/content/69/5/842.full", False)
            , ("https://ajp.psychiatryonline.org/doi/abs/10.1176/appi.ajp.2020.19080834", False)
            , ("https://ajph.aphapublications.org/cgi/content/abstract/94/9/1580", False)
            , ("https://aka.ms/beit", False) -- is actually Github underneath
            , ("https://almanac.httparchive.org/en/2019/", False)
            , ("https://andrewbadr.com/log/11/anonymizing-bitcoin/", False)
            , ("https://anidb.net/perl-bin/animedb.pl?show=lexicon&mode=character&vtype=ctag&relid=2296", False)
            , ("https://animesuperhero.com/forums/install/index.php?upgrade/", False)
            , ("https://answers.google.com/answers/threadview/id/777105.html", False)
            , ("https://antilop.cc/sr/vendors/24bb54ca7e.htm", False)
            , ("https://apastyle.apa.org/products/publication-manual-7th-edition", False)
            , ("https://api.research-repository.uwa.edu.au/ws/portalfiles/portal/11790041/Flematti_MS.pdf", False)
            , ("https://app.inferkit.com/demo", False)
            , ("https://apps.apple.com/us/app/ankisrs/id373493387", False)
            , ("https://apps.dtic.mil/dtic/tr/fulltext/u2/a224049.pdf", False)
            , ("https://apps.dtic.mil/sti/pdfs/ADA099503.pdf", False)
            , ("https://ar5iv.labs.arxiv.org/html/2001.04642?fallback=original", False)
            , ("https://arbital.com/p/edge_instantiation/", False)
            , ("https://archive-ouverte.unige.ch/files/downloads/0/0/0/2/5/6/4/3/unige_25643_attachment01.pdf", False)
            , ("https://archive.ilr.cornell.edu/download/9851", False)
            , ("https://archive.is/", False)
            , ("https://archiveofourown.org/works/3673335", False)
            , ("https://archives.haskell.org/code.haskell.org/monadius/Monadius/Demo.hs", False)
            , ("https://arstechnica.com/information-technology/2013/04/the-rise-and-fall-of-amd-how-an-underdog-stuck-it-to-intel/3/", False)
            , ("https://ase.uva.nl/binaries/content/assets/subsites/amsterdam-school-of-economics/research/uva-econometrics/dp-2013/1303.pdf", False)
            , ("https://astralcodexten.substack.com/", False)
            , ("https://au.news.yahoo.com/drug-warning-for-teens-17887955.html", False)
            , ("https://aur.archlinux.org/packages/anki20-bin/", False)
            , ("https://aws.amazon.com/blogs/opensource/keeping-open-source-open-open-distro-for-elasticsearch/", False)
            , ("https://azure.microsoft.com/en-us/blog/microsoft-showcases-the-azure-cloud-switch-acs/", False)
            , ("https://babel.hathitrust.org/cgi/pt?id=mdp.39015039380632&view=1up&seq=333", False)
            , ("https://bartokdesign.com/", False)
            , ("https://bayes.wustl.edu/etj/articles/general.background.ps.gz", False)
            , ("https://beautifultaiwantea.com/", False)
            , ("https://beerconnoisseur.com/articles/how-milwaukees-famous-beer-became-infamous", False)
            , ("https://bellroy.com/", False)
            , ("https://benbest.com/nutrceut/melatonin.html#negative", False)
            , ("https://berkshirehathaway.com/letters/2016ltr.pdf", False)
            , ("https://beta.openai.com/", False)
            , ("https://betterhumans.coach.me/the-effects-of-caffeine-alcohol-and-exercise-on-sleep-analyzing-the-surprising-results-117330af2480g", False)
            , ("https://bgr.com/general/google-services-shut-down-study/", False)
            , ("https://bibliophilly.library.upenn.edu/viewer.php?id=Ms.%20Codex%201248#page/244/mode/2up", False)
            , ("https://bigquery.cloud.google.com/table/fh-bigquery:reddit_comments.2015_05", False)
            , ("https://bioinfo.pl/pmid:17597168", False)
            , ("https://bitbucket.org/djhshih/argparser", False)
            , ("https://bitcoincharts.com/charts/mtgoxUSD#rg180zczsg2013-07-23zeg2013-07-23ztgSzm1g10zm2g25zv", False)
            , ("https://bitcoins-code.de/", False)
            , ("https://bitcointalk.org/index.php?topic=14828.0", False)
            , ("https://bjo.bmj.com/content/93/8/997", False)
            , ("https://bjsm.bmj.com/content/46/16/1144.2.abstract", False)
            , ("https://blockchain.info/tx/4e4364800426f6d601afeb1d1f34f1b93c6e599e7cb6e496416958a3364630bf", False)
            , ("https://blog.23andme.com/health-traits/chocolate-its-complicated/", False)
            , ("https://blog.darknedgy.net/technology/2020/05/02/0/", False)
            , ("https://blog.jaibot.com/the-copenhagen-interpretation-of-ethics/", False)
            , ("https://blog.johantibell.com/2011/08/results-from-state-of-haskell-2011.html", False)
            , ("https://blog.ml.cmu.edu/2022/01/21/why-spectral-normalization-stabilizes-gans-analysis-and-improvements/", False)
            , ("https://blog.mozilla.org/security/2010/03/31/plugging-the-css-history-leak/", False)
            , ("https://blog.nuclino.com/the-simple-genius-of-checklists-from-b-17-to-the-apollo-missions", False)
            , ("https://blog.torproject.org/trip-report-october-fbi-conference/", False)
            , ("https://blog.twitter.com/en_us/topics/company/2021/imperfect-by-design", False)
            , ("https://blogs.microsoft.com/ai/openai-azure-supercomputer/", False)
            , ("https://blogs.nature.com/news/2011/09/reliability_of_new_drug_target.html", False)
            , ("https://blogs.princeton.edu/notabilia/2020/05/20/what-could-be-better-pairing-and-comparing-the-scheide-and-kane-copies-of-fifteenth-century-books/", False)
            , ("https://blogs.scientificamerican.com/observations/what-the-history-of-math-can-teach-us-about-the-future-of-ai/", False)
            , ("https://bluelight.org/xf/", False)
            , ("https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-4-13", False)
            , ("https://bmcneurosci.biomedcentral.com/articles/10.1186/1471-2202-6-23", False)
            , ("https://bmcpublichealth.biomedcentral.com/articles/10.1186/1471-2458-7-159#B28", False)
            , ("https://bmcvetres.biomedcentral.com/articles/10.1186/s12917-017-0987-6", False)
            , ("https://boa.unimib.it/retrieve/handle/10281/23046/29556/Watching_alone_relational_goods.pdf", False)
            , ("https://boardgamegeek.com/boardgame/148931/coup-reformation", False)
            , ("https://books.google.com/books?id=Cr251kFBy5QC", False)
            , ("https://bsapubs.onlinelibrary.wiley.com/doi/full/10.2307/2656597", False)
            , ("https://buddhism-for-vampires.com/", False)
            , ("https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=217243", False)
            , ("https://bugs.launchpad.net/ubuntu/+source/unity-greeter/+bug/1538615", False)
            , ("https://cabinetmagazine.org/issues/64/lucas.php", False)
            , ("https://caes.ucdavis.edu/news/articles/2018/february/isolating-embryonic-stem-cells-in-cows-just-got-easier", False)
            , ("https://calgarysun.com/2012/08/25/dmt-suspected-in-drug-lab", False)
            , ("https://camelcamelcamel.com/", False)
            , ("https://capitalteas.com/Himalayan_Golden_Monkey_organic_p/23495.htm", False)
            , ("https://capsuleconnection.com/", False)
            , ("https://case.edu/ech/articles/r/rowfant-club", False)
            , ("https://casetext.com/case/united-states-v-5044-bitcoins", False)
            , ("https://cdm16630.contentdm.oclc.org/digital/collection/p16630coll2/id/534", False)
            , ("https://cep.lse.ac.uk/pubs/download/dp1490.pdf", False)
            , ("https://cfa.org/cfa-history/", False)
            , ("https://chem.tufts.edu/answersinscience/relativityofwrong.htm", False)
            , ("https://chrome.google.com/webstore/detail/singlefile/mpiodijhokgodhhofbcjdecpffjipkle", False)
            , ("https://chroniclingamerica.loc.gov/newspapers/", False)
            , ("https://ciechanow.ski/lights-and-shadows/", False)
            , ("https://circ.ahajournals.org/cgi/content/meeting_abstract/120/18_MeetingAbstracts/S461-a", False)
            , ("https://cis.org/articles/2010/teen-study.pdf", False)
            , ("https://clarkesworldmagazine.com/watts_01_10/", False)
            , ("https://clinicalepigeneticsjournal.biomedcentral.com/articles/10.1186/s13148%E2%80%93021%E2%80%9301218-y#Tab1", False)
            , ("https://cloud.google.com/tpu/pricing", False)
            , ("https://code.google.com/archive/p/amphetype", False)
            , ("https://code.visualstudio.com/blogs/2021/09/29/bracket-pair-colorization", False)
            , ("https://colab.research.google.com/drive/1LiWxqJJMR5dg4BxwUgighaWp2U_enaFd#offline=true&sandboxMode=true", False)
            , ("https://collections.library.yale.edu/catalog/2037169", False)
            , ("https://collider.com/hideaki-anno-evangelion-interview/", False)
            , ("https://comicsalliance.com/ask-chris-45-my-little-pony-meets-the-justice-league/", False)
            , ("https://community.arm.com/arm-community-blogs/b/high-performance-computing-blog/posts/deep-learning-episode-4-supercomputer-vs-pong-ii", False)
            , ("https://community.haskell.org/~gwern/hcorpus/", False)
            , ("https://console.aws.amazon.com/ec2/v2/home?region=us-east-1#LaunchInstanceWizard:ami=ami-b36981d8", False)
            , ("https://console.cloud.google.com/bigquery?project=danbooru1", False)
            , ("https://corante.com/drug-prices/erooms-law/", False)
            , ("https://core.ac.uk/download/pdf/160476861.pdf", False)
            , ("https://cosmosmagazine.com/biology/don-t-believe-the-mice", False)
            , ("https://course.fast.ai/videos/?lesson=7", False)
            , ("https://coveryourtracks.eff.org/static/browser-uniqueness.pdf", False)
            , ("https://creativecommons.org/licenses/by-nc/3.0/us/", False)
            , ("https://cro.sagepub.com/content/15/5/252.full.pdf+html", False)
            , ("https://crypto.stackexchange.com/questions/5831/what-is-the-progress-on-the-mit-lcs35-time-capsule-crypto-puzzle", False)
            , ("https://cryptome.org/2013/10/sadler-white-complaint.pdf", False)
            , ("https://csimq-journals.rtu.lv/article/download/csimq.2019-21.04/1744", False)
            , ("https://csrc.nist.gov/csrc/media/publications/conference-paper/1998/10/08/proceedings-of-the-21-nissc-1998/documents/early-cs-papers/karg74.pdf", False)
            , ("https://ctc.usma.edu/wp-content/uploads/2016/11/Cradle-to-Grave2.pdf", False)
            , ("https://cyber.harvard.edu/events/luncheon/2011/10/makohill", False)
            , ("https://dailyvoice.com/connecticut/norwalk/police-fire/norwalk-police-bust-major-marijuana-operation-after-finding-pot-in-mail/582175/", False)
            , ("https://dalspace.library.dal.ca/bitstream/handle/10222/56021/Heffernan-Amy-MSc-KINE-Nov-14.pdf?sequence=1", False)
            , ("https://danbooru.donmai.us/wiki_pages/34230", False)
            , ("https://darknetlive.com/arrested-darknet-vendors/", False)
            , ("https://data.worldbank.org/indicator/NY.GNP.PCAP.PP.CD", False)
            , ("https://datasetsearch.research.google.com/search?query=poem+OR+poetry&docid=T3haTlmLU9Dl6xqYAAAAAA%3D%3D", False)
            , ("https://deadline.com/2011/02/scott-rudin-closes-la-office-is-sony-move-imminent-105993/#comment-709877", False)
            , ("https://deadspin.com/the-myth-of-the-myth-of-the-hot-hand-1588112937", False)
            , ("https://dee.su/liberte", False)
            , ("https://dept.wofford.edu/neuroscience/neuroseminar/pdffall2006/rusted-mem3.pdf", False)
            , ("https://derpibooru.org/tags/artist-colon-thisponydoesnotexist", False)
            , ("https://devblogs.microsoft.com/oldnewthing/", False)
            , ("https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme", False)
            , ("https://developer.nvidia.com/cuda-downloads", False)
            , ("https://developer.twitter.com/en/docs/twitter-api/v1/rules-and-filtering/search-operators", False)
            , ("https://dharmaoverground.org/web/guest/discussion/-/message_boards/message/2718243#_com_liferay_message_boards_web_portlet_MBPortlet_message_2718243", False)
            , ("https://digitalcommons.unl.edu/cgi/viewcontent.cgi?article=1024&context=vpc15", False)
            , ("https://discord.com/invite/7RgdJZy", False)
            , ("https://disease-connect.org/", False)
            , ("https://dl.dropboxusercontent.com/u/85192141/music/surreacheese-feta-magicalcolorbossa.ogg", False)
            , ("https://dnd.wizards.com/articles/news/dungeons-dragons-teams-my-little-pony", False)
            , ("https://docs.ankiweb.net/filtered-decks.html#filtered-decks--cramming", False)
            , ("https://docs.microsoft.com/en-us/archive/blogs/larryosterman/one-in-a-million-is-next-tuesday", False)
            , ("https://donate.torproject.org/", False)
            , ("https://drive.google.com/drive/folders/1NY3MTkOSodz_5eCkjtoHUGYSulR25QGU", False)
            , ("https://drugs-forum.com/threads/116671/", False)
            , ("https://dspace.mit.edu/handle/1721.1/10589", False)
            , ("https://dual-n-back.com/nback.html", False)
            , ("https://duckduckgo.com/bang#bangs-list", False)
            , ("https://e621.net/posts", False)
            , ("https://eab.sagepub.com/content/23/1/3.short", False)
            , ("https://ec.toranoana.jp/joshi_r/ec/item/040030048682/", False)
            , ("https://economics.stanford.edu/sites/default/files/economics_of_scaleup_20180316.pdf", False)
            , ("https://eg-2.hatenablog.com/", False)
            , ("https://ehp.niehs.nih.gov/1104789/", False)
            , ("https://ehp.niehs.nih.gov/doi/full/10.1289/ehp.1510037", False)
            , ("https://eiga.com/news/20110830/11/", False)
            , ("https://ejlt.org/article/view/320/424", False)
            , ("https://elifesciences.org/articles/66920", False)
            , ("https://en.chessbase.com/post/a-history-of-cheating-in-chess-4", False)
            , ("https://en.wik%20ipedia.org/wiki/Va%20nishing_bird_cage", False)
            , ("https://en.wikip%20edia.org/wiki/The_%20Prestige_(film)", False)
            , ("https://en.wikipedia.org/wiki/Special:Random", False)
            , ("https://eng.uber.com/tag/forecasting/", False)
            , ("https://environhealthprevmed.biomedcentral.com/articles/10.1186/s12199-020-00865-6", False)
            , ("https://eprints.whiterose.ac.uk/97780/", False)
            , ("https://ergodox-ez.com/", False)
            , ("https://ero.sagepub.com/content/1/3/2332858415599972?full", False)
            , ("https://esajournals.onlinelibrary.wiley.com/doi/pdf/10.1890/ES10-00142.1", False)
            , ("https://escholarship.org/uc/item/5bv8c7p3", False)
            , ("https://espace.library.uq.edu.au/view/UQ:281052", False)
            , ("https://eth.wiki/en/concepts/proof-of-stake-faqs", False)
            , ("https://ethics.emory.edu/who-we-are/our-people/director.html", False)
            , ("https://ethos.bl.uk/OrderDetails.do?uin=uk.bl.ethos.486142", False)
            , ("https://every.to/cybernaut/caught-in-the-study-web", False)
            , ("https://everyoneishappy.com", False)
            , ("https://evigio.com/post/generating-new-watch-designs-with-stylegan", False)
            , ("https://examine.com/supplements/Piracetam/", False)
            , ("https://exrx.net/Nutrition/Antioxidants/Antioxidants", False)
            , ("https://f1000research.com/articles/3-82/v1", False)
            , ("https://faculty.virginia.edu/cogage/links/publications/", False)
            , ("https://fdaaa.trialstracker.net/", False)
            , ("https://fifteen.ai/", False)
            , ("https://finance.yahoo.com/news/Construction-Of-Chip-twst-2711924876.html", False)
            , ("https://fis.fda.gov/sense/app/d10be6bb-494e-4cd2-82e4-0135608ddc13/sheet/45beeb74-30ab-46be-8267-5756582633b4/state/analysis", False)
            , ("https://fivethirtyeight.com/features/the-complicated-legacy-of-a-panda-who-was-really-good-at-sex/", False)
            , ("https://flatisjustice.moe/TADNE", False)
            , ("https://floatingleaves.com/index.php?main_page=index&cPath=29", False)
            , ("https://fn.bmj.com/content/93/1/F45.short", False)
            , ("https://fnb.sagepub.com/content/24/4_suppl2/S129.full.pdf", False)
            , ("https://folkrnn.org/", False)
            , ("https://fontsinuse.com/typefaces/40498/ed-interlock", False)
            , ("https://food52.com/blog/15159-we-taste-tested-17-types-of-sparkling-water-here-s-what-happened", False)
            , ("https://fortune.com/2013/05/15/dirty-medicine/", False)
            , ("https://forums.somethingawful.com/showthread.php?threadid=3882695", False)
            , ("https://fred.stlouisfed.org/series/JPNURYNAA", False)
            , ("https://freedom-to-tinker.com/2011/05/24/you-might-also-privacy-risks-collaborative-filtering/", False)
            , ("https://fursona.app/", False)
            , ("https://gallica.bnf.fr/ark:/12148/bpt6k851127r/", False)
            , ("https://gazette.com/crime/air-force-academy-cadet-sentenced-for-dealing-drugs/article_76f47501-506c-52b0-8a33-f30632240e61.html", False)
            , ("https://gcta.freeforums.net/thread/213/analysis-greml-results-multiple-cohorts", False)
            , ("https://genepi.qimr.edu.au/general/TwinPowerCalculator/", False)
            , ("https://geneticalliance.org.uk/information/services-and-testing/how-can-i-access-preimplantation-genetic-diagnosis/#Question6", False)
            , ("https://genius.com/The-notorious-big-ten-crack-commandments-lyrics", False)
            , ("https://genomebiology.biomedcentral.com/articles/10.1186/s13059-018-1506-1", False)
            , ("https://genomemedicine.biomedcentral.com/articles/10.1186/s13073-014-0091-5", False)
            , ("https://georgianjournal.ge/society/34644-michel-houellebecq-a-french-author-shares-his-views-with-georgian-audience.html", False)
            , ("https://getd.libs.uga.edu/pdfs/sutherland_pierre_201212_ma.pdf", False)
            , ("https://gigaom.com/2013/03/13/chris-wetherll-google-reader/", False)
            , ("https://gigascience.biomedcentral.com/articles/10.1186/2047-217X-3-10", False)
            , ("https://gigazine.net/news/20190812-death-note-anonymity-entropy/", False)
            , ("https://gist.github.com/SigridK/c16ddc7b0f2a5bc01ea23d69569c6c0b", False)
            , ("https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks", False)
            , ("https://gitcoin.co/blog/gitcoin-grants-clr-matching/", False)
            , ("https://gitgud.io/AuroraPurgatio/aurorapurgatio", False)
            , ("https://github.com/google-research/google-research/tree/master/supcon", False)
            , ("https://github.com/lightonai/akronomicon", False)
            , ("https://github.com/microsoft/unadversarial", False)
            , ("https://gitlab.com/cryptsetup/cryptsetup/-/issues/19", False)
            , ("https://gitlab.developers.cam.ac.uk/cst/dtg/trvedata/local-first/-/commit/7a8819e817a85173af7033e64bcdcc5054b9af50?expanded=1#e74ca419c79e4a87cb170f6eb8a6c0d2719e1c73_0_726", False)
            , ("https://gitlab.haskell.org/ghc/ghc/-/issues/2143", False)
            , ("https://gizmodo.com/generation-cryo-fighting-death-in-the-frozen-unknown-1786446378", False)
            , ("https://godanriver.com/news/danville/three-suspects-arrested-charged-with-mailing-meth-into-county/article_d73f9598-c839-11e4-9dbd-974577bb449e.html", False)
            , ("https://granta.com/the-man-with-two-heads/", False)
            , ("https://greekreporter.com/2010/12/19/inside-the-minds-of-the-parlapanides-brothers/", False)
            , ("https://groups.google.com/g/brain-training/c/DdeZzeXEMyE", False)
            , ("https://groups.yahoo.com/neo/groups/catsandkittens/conversations/messages/188981", False)
            , ("https://gsejournal.biomedcentral.com/articles/10.1186/s12711-016-0280-3", False)
            , ("https://gwern.shinyapps.io/orderStatisticsIncreasedVariance/", False)
            , ("https://h01-dot-neuroglancer-demo.appspot.com/#!gs://h01-release/assets/library_state.json", False)
            , ("https://habr.com/en/post/429602/", False)
            , ("https://hacks.mozilla.org/2021/05/improving-firefox-stability-on-linux/", False)
            , ("https://hal.archives-ouvertes.fr/hal-00904097/document#pdf", False)
            , ("https://halshs.archives-ouvertes.fr/halshs-02180182/document", False)
            , ("https://training.cochrane.org/handbook/current", False)
            , ("https://hansard.parliament.uk/Lords/1986-06-12/debates/56392d7a-e823-45be-80b6-26711300c7aa/GruinardIslandCostOfDecontamination", False)
            , ("https://hardcoresoftware.learningbyshipping.com/p/061-bsod-to-watson-the-reliability", False)
            , ("https://harmreductionjournal.biomedcentral.com/articles/10.1186/s12954-019-0308-4", False)
            , ("https://hashcat.net/wiki/doku.php?id=mask_attack", False)
            , ("https://haveibeenpwned.com/", False)
            , ("https://hazuma.hatenablog.com/entry/20090511/1242018681", False)
            , ("https://hbr.org/2019/12/can-you-know-too-much-about-your-organization", False)
            , ("https://hcommons.org/deposits/objects/hc:20220/datastreams/CONTENT/content?download=true#pdf", False)
            , ("https://help.duckduckgo.com/duckduckgo-help-pages/results/syntax/", False)
            , ("https://help.myspace.com/hc/en-us/articles/202233310-Where-Is-All-My-Old-Stuff-", False)
            , ("https://history.nasa.gov/rogersrep/v2appf.htm", False)
            , ("https://hivemind-repo.s3-us-west-2.amazonaws.com/twdne3/twdne3.onnx", False)
            , ("https://hivemind-repo.s3-us-west-2.amazonaws.com/twdne3/twdne3.pt", False)
            , ("https://hn.algolia.com/?query=%22Long%20Bets%22&sort=byDate&prefix&page=0&dateRange=all&type=all", False)
            , ("https://hollisarchives.lib.harvard.edu/repositories/4/archival_objects/1033884", False)
            , ("https://home.inklingmarkets.com/recent/markets", False)
            , ("https://hoogle.haskell.org/?hoogle=IO_a_-%3E_IO_ThreadId", False)
            , ("https://hplusmagazine.com/2012/04/12/transhumanism-and-the-human-expansion-into-space-a-conflict-with-physics/", False)
            , ("https://hrcak.srce.hr/file/210", False)
            , ("https://huggingface.co/calculator/", False)
            , ("https://hum.sagepub.com/content/30/5/431.full.pdf+html", False)
            , ("https://i.imgur.com/jzZKreU.png", False)
            , ("https://icosahedron.website/@halcy/101654023952557254", False)
            , ("https://ide.mit.edu/sites/default/files/publications/Multi-Sided%20Platform%20Strategy,%20Taxation%20and%20Regulation%20October%202019.pdf#page=14", False)
            , ("https://ideas.4brad.com/has-uber-already-beaten-private-ownership-cost", False)
            , ("https://ieeexplore.ieee.org/xpls/abs_all.jsp?arnumber=602492", False)
            , ("https://if50.substack.com/p/1999-king-of-dragon-pass", False)
            , ("https://ije.oxfordjournals.org/content/30/6/1251.full", False)
            , ("https://imagelibrary.bgu.ac.il/pf.tlx/O6ORSOx-nut", False)
            , ("https://imgur.com/a/LE80ogv", False)
            , ("https://inews.co.uk/opinion/why-i-donate-my-sperm-over-facebook-and-have-fathered-23-children-194142", False)
            , ("https://infidels.org/library/modern/richard-carrier-kooks/", False)
            , ("https://interviews.slashdot.org/story/11/09/06/1458254/Kevin-Kelly-Answers-Your-Questions", False)
            , ("https://intrade.com/jsp/intrade/common/c_cd.jsp?conDetailID=702407&z=1285870999458", False)
            , ("https://iopscience.iop.org/article/10.1088/1748-3190/ac253a", False)
            , ("https://irs.princeton.edu/sites/irs/files/event/uploads/economics_of_scaleup_20171004.pdf", False)
            , ("https://it.slashdot.org/comments.pl?sid=2679325&cid=39089165", False)
            , ("https://iterative.capital/thesis/", False)
            , ("https://jacobsschool.ucsd.edu/news/release/791?id=791", False)
            , ("https://jalopnik.com/you-have-no-idea-how-insanely-complex-modern-headlights-1840509448", False)
            , ("https://jamestown.org/wp-content/uploads/2021/05/The-Prize-Why-Taiwan-and-its-Place-in-the-Global-Semiconductor-Supply-Chian-Matter-to-the-United-States.pdf#page=21", False)
            , ("https://japanintercultural.com/free-resources/articles/over-worked-and-underpaid-japanese-employees-feel-the-burden-of-sabisu-zangyo/", False)
            , ("https://jasbsci.biomedcentral.com/articles/10.1186/s40104-018-0304-7", False)
            , ("https://jcsm.aasm.org/doi/10.5664/jcsm.7132", False)
            , ("https://jigsaw.w3.org/css-validator/#validate-by-input", False)
            , ("https://jme.bmj.com/content/37/4/249.abstract", False)
            , ("https://jnnp.bmj.com/content/72/2/179.full", False)
            , ("https://joshdean.com/sites/default/files/articles/feat_clones44.pdf", False)
            , ("https://jotengine.com/transcriptions/Q0I0i33TaqCa9w4In0ZQCg", False)
            , ("https://journals.ametsoc.org/view/journals/clim/18/23/jcli3593.1.xml", False)
            , ("https://journals.lww.com/greenjournal/Abstract/2003/03000/A_Randomized_Trial_of_Docosahexaenoic_Acid.11.aspx", False)
            , ("https://journals.physiology.org/doi/full/10.1152/ajpregu.2000.278.4.r905", False)
            , ("https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0201246", False)
            , ("https://journals.sagepub.com/doi/full/10.1177/2515245920919667", False)
            , ("https://journaltimes.com/news/local/crime-and-courts/woman-accused-of-receiving-pot-by-mail/article_3da750f2-96f9-53e9-b771-b1d88bb27368.html", False)
            , ("https://jpet.aspetjournals.org/content/296/3/849", False)
            , ("https://jukebox.openai.com/", False)
            , ("https://justpaste.it/7eovk", False)
            , ("https://jyllands-posten.dk/indland/politiretsvaesen/ECE7542044/To-m%C3%A6nd-afsl%C3%B8ret-med-over-et-ton-doping/", False)
            , ("https://kettl.co/", False)
            , ("https://kilthub.cmu.edu/articles/What_Went_Wrong_Reflections_on_Science_by_Observation_and_The_Bell_Curve/6493139/files/11937863.pdf#page=2", False)
            , ("https://knowyourmeme.com/memes/tendies-stories", False)
            , ("https://koeln.ccc.de/archiv/cyphernomicon/chapter14/14.5.html", False)
            , ("https://kotaku.com/dont-let-a-little-thing-like-a-sex-video-slow-you-down-5856321", False)
            , ("https://kyunghyuncho.me/brief-summary-of-the-panel-discussion-at-dl-workshop-icml-2015/", False)
            , ("https://lareviewofbooks.org/article/the-secret-history-of-dune/", False)
            , ("https://latitude.io/blog/introducing-ai-dungeon-translate/", False)
            , ("https://leaderboard.allenai.org/break_high_level/submissions/public", False)
            , ("https://leaps.org/a-star-surgeon-left-a-trail-of-dead-patients-and-his-whistleblowers-were-punished/", False)
            , ("https://learn.adafruit.com/adafruit-feather-32u4-adalogger", False)
            , ("https://leme.me/verah/mp3/?C93/Lost%20Garden%20%E2%80%94%20ENIGMATIC%20LINER%20%5BMP3-V0%5D%5BC93%5D#trk6", False)
            , ("https://letterformarchive.org/news/view/the-complete-commercial-artist", False)
            , ("https://letters.temporarystate.net/entry/3/", False)
            , ("https://lichess.org/blog/YafSBxEAACIAr0ZA/exact-exacting-who-is-the-most-accurate-world-champion", False)
            , ("https://link.springer.com/article/10.1007/BF02253535", False)
            , ("https://link.springer.com/article/10.1007/s10645-019-09339-9", False)
            , ("https://lists.wikimedia.org/hyperkitty/list/wikimedia-l@lists.wikimedia.org/message/DC4YYUFMYS6CSOOPINPBR4FPS2L53BVE/", False)
            , ("https://listserv.brown.edu/archives/cgi-bin/wa?A2=ind0001B&L=NAUSICAA&P=R748", False)
            , ("https://listudy.org/en", False)
            , ("https://lizadaly.com/", False)
            , ("https://lobste.rs/s/1d7whd/tales_from_trenches_ai_disaster_stories#c_le6tsr", False)
            , ("https://longbets.org/363/", False)
            , ("https://longnow.org/essays/richard-feynman-and-connection-machine/", False)
            , ("https://longreads.com/2019/10/22/the-final-five-percent/", False)
            , ("https://lostpinesyaupontea.com/products/light-roast-yaupon-tea", False)
            , ("https://lwn.net/Articles/286233/", False)
            , ("https://machinelearning.apple.com/research/hey-siri", False)
            , ("https://make.girls.moe/", False)
            , ("https://marco.org/2011/04/09/facebooks-open-compute-project", False)
            , ("https://martinottaway.com/rhemmen/really-big-picture/", False)
            , ("https://math.stackexchange.com/questions/349155/how-often-does-it-happen-that-the-oldest-person-alive-dies/388131#388131", False)
            , ("https://math.stackexchange.com/questions/89030/expectation-of-the-maximum-of-gaussian-random-variables/89147#89147", False)
            , ("https://mathoverflow.net/questions/35468/widely-accepted-mathematical-results-that-were-later-shown-to-be-wrong/35554#35554", False)
            , ("https://mathoverflow.net/questions/879/most-interesting-mathematics-mistake", False)
            , ("https://mattsclancy.substack.com/p/remote-work-and-the-future-of-innovation", False)
            , ("https://mbio.asm.org/content/3/2/e00036-12", False)
            , ("https://meaningness.com/geeks-mops-sociopaths", False)
            , ("https://mecha-guignol.com/2013/02/20/no-flcl-for-oldtaku/", False)
            , ("https://media.ccc.de/v/35c3-9778-open_source_firmware", False)
            , ("https://media.githubusercontent.com/media/sarabander/sicp-pdf/master/sicp.pdf", False)
            , ("https://media.nature.com/original/nature-assets/nature/journal/v533/n7604/extref/nature17671-s2.xlsx", False)
            , ("https://media.springernature.com/full/springer-static/image/art%3A10.1186%2Fs12917-017-0987-6/MediaObjects/12917_2017_987_Fig7_HTML.gif", False)
            , ("https://medicine.osu.edu/neuroscience/Documents/Nocturnal%20Light%20Exposure.pdf", False)
            , ("https://medieval.bodleian.ox.ac.uk/catalog/manuscript_336", False)
            , ("https://medlineplus.gov/druginfo/meds/a602016.html#side-effects", False)
            , ("https://meehl.umn.edu/files/aumeehl2003sigtests-trimmedmp3#.mp3", False)
            , ("https://meltingasphalt.com/hallucinated-gods/", False)
            , ("https://metacpan.org/release/LBROCARD/Net-VNC-0.36/view/bin/vnccapture", False)
            , ("https://metrics.torproject.org/data.html", False)
            , ("https://millercenter.org/the-presidency/presidential-speeches/march-23-1962-address-university-california-berkeley#dp-expandable-text", False)
            , ("https://mnemosyne-proj.org/", False)
            , ("https://money.cnn.com/magazines/fortune/fortune_archive/2006/04/03/8373034/index.htm", False)
            , ("https://motherboard.vice.com/read/this-researcher-is-tallying-arrests-from-dark-web-markets", False)
            , ("https://moz.com/ugc/google-alerts-vs-mention-vs-talkwalker", False)
            , ("https://mpra.ub.uni-muenchen.de/74268/1/MPRA_paper_74268.pdf", False)
            , ("https://mru.org/development-economics", False)
            , ("https://mujoco.org/", False)
            , ("https://myanimelist.net/mangalist/gwern", False)
            , ("https://mymodafinil.net/armodafinil/", False)
            , ("https://n.neurology.org/content/78/2/91.full", False)
            , ("https://naldc.nal.usda.gov/download/CAT71326739/PDF", False)
            , ("https://nansenundpiccard.de/de/Wir/Hanske", False)
            , ("https://nationalinterest.org/bookreview/the-fallacy-human-freedom-8652?nopaging=1", False)
            , ("https://nces.ed.gov/naal/sample.asp", False)
            , ("https://nearcyan.com/this-anime-does-not-exist/", False)
            , ("https://nebia.com/", False)
            , ("https://new.cognitivefun.net/", False)
            , ("https://newhumanist.org.uk/2365/lies-damn-lies-and-chinese-science", False)
            , ("https://newrepublic.com/article/120178/problem-international-development-and-plan-fix-it", False)
            , ("https://news.gallup.com/poll/222578/americans-weigh-shun-overweight-label.aspx", False)
            , ("https://news.harvard.edu/gazette/story/2010/10/thinking-like-an-octopus/", False)
            , ("https://news.mit.edu/2014/algorithm-recovers-speech-from-vibrations-0804", False)
            , ("https://news.samsung.com/global/sock-horror-mystery-of-missing-socks-is-solved-scientists-reveal-why-socks-go-missing-in-the-wash-and-how-likely-it-is-to-happen", False)
            , ("https://news.slashdot.org/story/07/10/31/0328239/call-for-halt-to-wikipedia-webcomic-deletions", False)
            , ("https://news.yahoo.com/shattered-inside-the-secret-battle-to-save-americas-undercover-spies-in-the-digital-age-100029026.html", False)
            , ("https://news.ycombinator.com/item?id=17048329", False)
            , ("https://newsroom.collegeboard.org/", False)
            , ("https://nitter.hu/advadnoun/status/1458894698974638111", False)
            , ("https://nitter.hu/search?f=tweets&q=http%3A%2F%2Fwww.thiswaifudoesnotexist.net&src=typd", False)
            , ("https://nmteaco.com/Plum-Oolong_p_380.html", False)
            , ("https://nori.com/", False)
            , ("https://noscript.net/", False)
            , ("https://nutritionj.biomedcentral.com/articles/10.1186/s12937-017-0269-y", False)
            , ("https://nypost.com/2019/11/02/stanford-professor-who-changed-america-with-just-one-study-was-also-a-liar/", False)
            , ("https://ods.od.nih.gov/factsheets/Magnesium-HealthProfessional/", False)
            , ("https://old.reddit.com/r/haskell/comments/fid5w/haskell_summers_of_code_retrospective_updated_for/c1gutlo", False)
            , ("https://oll.libertyfund.org/title/detmold-the-historical-political-and-diplomatic-writings-vol-2#lf0076-02_label_005", False)
            , ("https://omdia.tech.informa.com/", False)
            , ("https://omega0.xyz/omega8008/ETJ-PS/cc5d.ps", False)
            , ("https://onlinelibrary.wiley.com/doi/full/10.1111/acel.12880", False)
            , ("https://openaipublic.blob.core.windows.net/webgpt-answer-viewer/index.html", False)
            , ("https://openreview.net/forum?id=qVyeW-grC2k#google", False)
            , ("https://openscience.bmj.com/content/4/1/e100051", False)
            , ("https://opensnp.org/", False)
            , ("https://opensource.org/licenses", False)
            , ("https://orbis.stanford.edu/", False)
            , ("https://orbit.dtu.dk/fedora/objects/orbit:134384/datastreams/file_a24346bd-b582-483e-b3f4-2efbc42682aa/content", False)
            , ("https://osf.io/preprints/socarxiv/mbj9p/", False)
            , ("https://osnadocs.ub.uni-osnabrueck.de/bitstream/urn:nbn:de:gbv:700-2008112111/2/E-Diss839_thesis.pdf", False)
            , ("https://otago.ourarchive.ac.nz/handle/10523/2341", False)
            , ("https://otakumode.com/news/5458c7dadb7b183963e68abf/Evangelion-20th-Anniversary-Bombshell-a-Mysterious-Work-Just-Like-Attack-on-Titan-and%E2%80%A6-Hideaki-Anno-Talk-Show-%E2%80%9CDirector-Edition%E2%80%9D-Report", False)
            , ("https://ourstarblazers.com/vault/78/", False)
            , ("https://packages.debian.org/sid/x11/xprintidle", False)
            , ("https://pages.jh.edu/jhumag/0697web/whiz.html", False)
            , ("https://pain.wustl.edu/c/BasicResearch/documents/Chennature2001.pdf", False)
            , ("https://papers.nips.cc/paper/2003/file/9fb7b048c96d44a0337f049e0a61ff06-Paper.pdf", False)
            , ("https://paperswithcode.com/task/language-modelling", False)
            , ("https://par.nsf.gov/servlets/purl/10013134", False)
            , ("https://partner.steamgames.com/doc/steamdeck/faq", False)
            , ("https://paste.laravel.io/f2419e15-ea7d-408a-8ff2-b8ee6d00ddd1/raw", False)
            , ("https://pastebin.com/GrV3uYh5", False)
            , ("https://patch-tag.com/", False)
            , ("https://patch.com/california/davis/davis-pair-arrested-after-cops-intercept-3-000-suspected-ecstasy-pills-mail-serve", False)
            , ("https://patents.google.com/patent/US20150124107", False)
            , ("https://pay.reddit.com/r/DarkNetMarkets/comments/2i07pq/the_number_of_multisig_transactions_doubled_on_evo/", False)
            , ("https://pcl.sitehost.iu.edu/rgoldsto/interrelated/interrelated.html", False)
            , ("https://pcpartpicker.com/list/nwQnZ8", False)
            , ("https://peerj.com/preprints/27137v1/", False)
            , ("https://perma.cc/", False)
            , ("https://phabricator.wikimedia.org/T270650", False)
            , ("https://pharmrev.aspetjournals.org/content/57/1/79.long", False)
            , ("https://philarchive.org/archive/SOTAOAv1", False)
            , ("https://physics.aps.org/articles/v7/47", False)
            , ("https://physicstoday.scitation.org/do/10.1063/PT.6.1.20180822a/full/", False)
            , ("https://physoc.onlinelibrary.wiley.com/doi/pdf/10.1113/jphysiol.2011.217919", False)
            , ("https://pioneer.app/", False)
            , ("https://pirate.london/real-fake-money-real-insecure-markets-updated-8760f5254645", False)
            , ("https://pixelpoppers.com/2010/12/why-i-quit-wow-and-started-working-out/", False)
            , ("https://pjreddie.com/yolo/", False)
            , ("https://play.aidungeon.io/main/home", False)
            , ("https://play.google.com/store/apps/details?id=com.ankerl.singlenback", False)
            , ("https://player.vimeo.com/video/427943452", False)
            , ("https://plus.google.com/u/0/103530621949492999968/posts/AThvaCXCSp2", False)
            , ("https://pni.princeton.edu/john-hopfield/john-j.-hopfield-now-what", False)
            , ("https://pol.is/home", False)
            , ("https://polisen.se/Aktuellt/Nyheter/2015/Mars/Polisen-stoppade-droghandel-pa-natet/", False)
            , ("https://politicalscience.osu.edu/faculty/jmueller/MID11TSM.PDF", False)
            , ("https://pone.dev/", False)
            , ("https://popcon.debian.org/", False)
            , ("https://poststar.com/news/local/crime-and-courts/former-computer-shop-operator-pleads-guilty-to-drug-charges/article_b42b976e-8bf9-11e3-a353-001a4bcf887a.html", False)
            , ("https://pps.sagepub.com/content/7/6/643.full", False)
            , ("https://practicaltypography.com/web-and-email-addresses.html", False)
            , ("https://predictionbook.com/predictions/3693", False)
            , ("https://priceonomics.com/why-is-art-expensive/", False)
            , ("https://prisons.org.uk/worker-at-brixton-jail-charged-with-trying-to-buy-gun-and-ammo/", False)
            , ("https://proceedings.neurips.cc/paper/2016/hash/7ef605fc8dba5425d6965fbd4c8fbe1f-Abstract.html", False)
            , ("https://programmablesearchengine.google.com/about//cse?cx=009114923999563836576%3A1eorkzz2gp4&q=Firefly+%22Cowboy+Bebop%22", False)
            , ("https://project-rainbowcrack.com/buy.php", False)
            , ("https://projecteuclid.org/journals/bernoulli/volume-2/issue-4/Exponential-convergence-of-Langevin-distributions-and-their-discrete-approximations/bj/1178291835.full", False)
            , ("https://psi.sagepub.com/content/12/1/3.full", False)
            , ("https://psmag.com/social-justice/diy-diagnosis-extreme-athlete-uncovered-genetic-flaw-88763", False)
            , ("https://pss.sagepub.com/content/24/4/562.full", False)
            , ("https://psyarxiv.com/kq4mn/", False)
            , ("https://psych.wustl.edu/memory/Roddy%20article%20PDF%27s/Lyle%20et%20al%20(2008)_PBR.pdf", False)
            , ("https://psychology.gatech.edu/renglelab/2010/shipsteadredickengle.pdf", False)
            , ("https://psycnet.apa.org/index.cfm?fa=search.searchResults", False)
            , ("https://psycnet.apa.org/record/1982-13030-001", False)
            , ("https://public.tableau.com/app/profile/jurijfedorov#!/vizhome/AnthropologysScienceWars/Field", False)
            , ("https://pubmed.ncbi.nlm.nih.gov/8588288/", False)
            , ("https://pubs.aeaweb.org/doi/pdfplus/10.1257/aer.91.5.1539", False)
            , ("https://pubsonline.informs.org/doi/suppl/10.1287/mnsc.2018.3032/suppl_file/mnsc.2018.3032-sm.pdf", False)
            , ("https://pulitzercenter.org/stories/right-not-know-when-ignorance-bliss-deadly", False)
            , ("https://pure.au.dk/portal/en/publications/low-risk-of-suicide-and-lithium-in-drinking-water-a-danish-individuallevel-cohort-study-using-spatial-analysis(5855ed41-d812-4ce4-b089-078be11c11fa).html", False)
            , ("https://pure.tue.nl/ws/portalfiles/portal/142614149/J.R.Ubbink_09_09_2019_thesis_final.pdf", False)
            , ("https://pure.uva.nl/ws/files/3216102/15320_Thesis.pdf#page=103", False)
            , ("https://pure.uvt.nl/ws/portalfiles/portal/29125573/MTO_Flore_influence_of_gender_stereotype_CRiSP_2019.pdf#page=3", False)
            , ("https://purse.io/", False)
            , ("https://qa.debian.org/popcon.php?package=mnemosyne", False)
            , ("https://qualitysafety.bmj.com/content/21/10/819.abstract", False)
            , ("https://quoteinvestigator.com/2012/12/30/yogi-didnt-say/", False)
            , ("https://qz.com/1311732/openai-built-gaming-bots-that-can-work-as-a-team-with-inhuman-precision/", False)
            , ("https://rachelbythebay.com/", False)
            , ("https://radiopublic.com/longform-MG7XaW/s1!316e8", False)
            , ("https://raw.githubusercontent.com/google-research/google-research/master/automl_zero/best_algo.gif", False)
            , ("https://rbej.biomedcentral.com/articles/10.1186/s12958-015-0029-9", False)
            , ("https://rd.springer.com/chapter/10.1007/978-3-030-42504-3_15", False)
            , ("https://readonlymemory.vg/shop/book/arcade-game-typography/", False)
            , ("https://reason.com/2017/11/28/in-search-of-the-elusive-bitco/", False)
            , ("https://repository.si.edu/bitstream/handle/10088/18355/nzp_changing_world_mcshea.pdf", False)
            , ("https://repository.uel.ac.uk/download/489cc10ec6c30da0e7bbdfb33898364c46ac9dcc727169bd1850016053e45278/347037/2013_Dawkins_e-cig_survey.pdf", False)
            , ("https://repository.upenn.edu/cgi/viewcontent.cgi?article=1038&context=neuroethics_pubs", False)
            , ("https://research.facebook.com/blog/2016/03/do-jobs-run-in-families/", False)
            , ("https://reset.me/story/benefits-of-microdosing-with-lsd-and-psilocybin-mushrooms/", False)
            , ("https://risk-engineering.org/concept/Rasmussen-practical-drift", False)
            , ("https://ro.ecu.edu.au/cgi/viewcontent.cgi?article=1025&context=spsyc_pres", False)
            , ("https://royalsocietypublishing.org/doi/10.1098/rsos.170988", False)
            , ("https://rpubs.com/EmilOWK/232493", False)
            , ("https://rss.onlinelibrary.wiley.com/doi/full/10.1111/j.1740-9713.2006.00202.x", False)
            , ("https://runrepeat.com/height-evolution-in-the-nba", False)
            , ("https://samsaffron.com/archive/2012/06/07/testing-3-million-hyperlinks-lessons-learned#comment-31366", False)
            , ("https://scholar.google.com/scholar?q=cat%20earwax%20OR%20%22ear%20wax%22%20smell%20OR%20taste%20%2D%22CAT%20scan%22", False)
            , ("https://scholar.harvard.edu/files/rpande/files/moving_isolation_aer_submission.pdf", False)
            , ("https://science.nasa.gov/science-news/science-at-nasa/2005/03jun_naps/", False)
            , ("https://scienceblogs.com/cognitivedaily/2009/04/16/a-quick-eye-exercise-can-impro", False)
            , ("https://scp-wiki.wikidot.com/scp-988", False)
            , ("https://searchengineland.com/google-alerts-arent-working-148642", False)
            , ("https://secure.flickr.com/photos/michellemaabelle/4630940195/", False)
            , ("https://sembr.org/", False)
            , ("https://sensebridge.net/projects/northpaw/", False)
            , ("https://sgo.sagepub.com/content/5/2/2158244015579724", False)
            , ("https://siepr.stanford.edu/system/files/shared/Final_Rosston_Savage_Waldman_02_04_10__1_.pdf", False)
            , ("https://sites.google.com/site/computationalstylistics/home", False)
            , ("https://sites.google.com/view/mend-editing", False)
            , ("https://sites.google.com/view/multi-agent-competition", False)
            , ("https://sites.research.google/trc/", False)
            , ("https://sites.suffolk.edu/legaltech/2014/03/11/serious-an-lpti-supported-project-to-improve-students-learning-and-bar-performance/", False)
            , ("https://sive.rs/srs", False)
            , ("https://skeb.jp/", False)
            , ("https://skeptics.stackexchange.com/questions/1700/do-bigger-or-more-monitors-increase-productivity/1701#1701", False)
            , ("https://skift.com/oral-history-of-booking-acquisition/", False)
            , ("https://skymind.ai/wiki/generative-adversarial-network-gan", False)
            , ("https://slashdot.org/story/07/11/18/1319201/do-tiny-url-services-weaken-net-architecture", False)
            , ("https://slate.com/articles/life/seed/2001/04/the_rise_of_the_smart_sperm_shopper.single.html", False)
            , ("https://soundcloud.com/leggysalad/girls-afternoon-appointments", False)
            , ("https://spectrum.ieee.org/lasers-and-lunar-arks-cryopreservation-heats-up", False)
            , ("https://spectrum.library.concordia.ca/id/eprint/36253/1/2010_Mining_Writeprints_from_Anonymous_E-mails.pdf", False)
            , ("https://spreadsheets.google.com/ccc?key=rOfijmsJ-hxPbzmbi4dmHVg", False)
            , ("https://sss.sagepub.com/content/44/4/638.long", False)
            , ("https://stackoverflow.com/questions/504823/has-anyone-actually-implemented-a-fibonacci-heap-efficiently", False)
            , ("https://static-content.springer.com/esm/art%3A10.1186%2Fs12917-017-0987-6/MediaObjects/12917_2017_987_MOESM2_ESM.xlsx", False)
            , ("https://stats.stackexchange.com/questions/9001/approximate-order-statistics-for-normal-random-variables/9010#9010", False)
            , ("https://stats.stackexchange.com/users/2392/probabilityislogic", False)
            , ("https://steamcommunity.com/groups/steamworks/announcements/detail/1697229969000435735", False)
            , ("https://stevecoast.com/2015/03/27/the-world-will-only-get-weirder/", False)
            , ("https://store.steampowered.com/app/497780/Recursed/", False)
            , ("https://streamable.com/87z73", False)
            , ("https://stripe.com/blog/negative-emissions-commitment", False)
            , ("https://studenttheses.universiteitleiden.nl/handle/1887/52994", False)
            , ("https://substack.com/", False)
            , ("https://subterraneanpress.com/magazine/fall_2013/the_truth_of_fact_the_truth_of_feeling_by_ted_chiang", False)
            , ("https://summerofcode.withgoogle.com/", False)
            , ("https://sundhedsdatastyrelsen.dk/da/404/?item=%2fda%2ftal-og-analyser%2fanalyser-og-rapporter%2fandre-analyser-og-rapporter%2fassisteret-reproduktion&user=extranet%5cAnonymous&site=sds", False)
            , ("https://super.gluebenchmark.com/leaderboard/", False)
            , ("https://superuser.com/questions/1633073/why-are-tar-xz-files-15x-smaller-when-using-pythons-tar-library-compared-to-mac", False)
            , ("https://support.google.com/websearch/answer/2466433", False)
            , ("https://support.mozilla.org/en-US/kb/firefox-page-info-window", False)
            , ("https://support.torproject.org/", False)
            , ("https://support.torproject.org/#better-anonymity", False)
            , ("https://t.me/alexeyguzey", False)
            , ("https://tails.boum.org/", False)
            , ("https://tasvideos.org/6347S", False)
            , ("https://taylorandfrancis.com/", False)
            , ("https://teahabitat.com/", False)
            , ("https://tealet.com/", False)
            , ("https://techcrunch.com/2013/02/23/the-chinese-are-coming-the-chinese-are-coming/", False)
            , ("https://tfhub.dev/google/collections/gtr/1", False)
            , ("https://thecatsite.com/threads/my-cat-likes-earwax.69070/", False)
            , ("https://thechineseteashop.com/", False)
            , ("https://thecleverest.com/judgefakepeople/main.php?sort=highest", False)
            , ("https://theconversation.com/intelligence-inheritance-three-genes-that-add-to-your-iq-score-31397", False)
            , ("https://thecorrespondent.com/100/the-new-dot-com-bubble-is-here-its-called-online-advertising/13228924500-22d5fd24", False)
            , ("https://theeagle.com/news/local/cc-world-s-first-cloned-cat-turns-years-old/article_d2aeac6e-2471-11ea-a5f2-7b6c21b2b4b4.html", False)
            , ("https://thegradient.pub/understanding-evaluation-metrics-for-language-models/", False)
            , ("https://theintercept.com/2016/08/03/gop-lawyer-chinese-owned-company-us-presidential-politics/", False)
            , ("https://themorningnews.org/article/the-heretic", False)
            , ("https://thenextweb.com/news/google-alerts-regains-rss-delivery-option-it-lost-after-google-readers-demise", False)
            , ("https://thepiratebay.org/description.php?id=18368760", False)
            , ("https://thesession.org/", False)
            , ("https://thesocietypages.org/socimages/2008/02/06/correlations-of-iq-with-income-and-wealth/", False)
            , ("https://thetakeout.com/inside-black-market-vintage-kool-aid-packet-collectors-1835123510", False)
            , ("https://thiscardoesnotexist.glitch.me", False)
            , ("https://thisponydoesnotexist.net/model/network-ponies-1024-151552.pkl", False)
            , ("https://thisvesseldoesnotexist.com/", False)
            , ("https://thomasbarker.com/13/06/bitcoin-little-slice-future-shock", False)
            , ("https://thoughtbot.com/blog/arduino-bathroom-occupancy-detector", False)
            , ("https://timarit.is/page/131542", False)
            , ("https://timesofindia.indiatimes.com/city/bengaluru/Peddlers-procure-narcotics-from-darknet/articleshow/48368502.cms", False)
            , ("https://tineye.com/", False)
            , ("https://tinyurl.com/hquv34", False)
            , ("https://tl.net/blogs/286351-worker-rush-part-5-live-to-win?view=all", False)
            , ("https://tobaccocontrol.bmj.com/content/14/1/49.full", False)
            , ("https://today.yougov.com/topics/politics/articles-reports/2021/07/15/why-wont-americans-get-vaccinated-poll-data", False)
            , ("https://top500.org/news/fugaku-holds-top-spot-exascale-remains-elusive/", False)
            , ("https://towardsdatascience.com/gpt-3-creative-potential-of-nlp-d5ccae16c1ab", False)
            , ("https://trends.google.com/trends/explore?q=Long%20Bets", False)
            , ("https://trialsjournal.biomedcentral.com/articles/10.1186/s13063-015-1080-8", False)
            , ("https://tribune.com.pk/story/1967033/3-amid-animal-cruelty-debate-80-south-koreas-sniffer-dogs-cloned", False)
            , ("https://truewetsuits.jp/travel_guide/tokyo_books_music/manga/Mangano-mori+Ikebukuro/1995", False)
            , ("https://tuts4you.com/download.php?view.2348", False)
            , ("https://tvtropes.org/pmwiki/pmwiki.php/VisualNovel/UminekoWhenTheyCry", False)
            , ("https://tweelingenregister.vu.nl/gpc", False)
            , ("https://type-r.hatenablog.com/entries/2012/12/05", False)
            , ("https://ubc-emotionlab.ca/wp-content/uploads/2012/09/Schimmack-2012-Effect-of-Significance-on-Article-Credibility.pdf", False)
            , ("https://uitspraken.rechtspraak.nl/inziendocument?id=ECLI:NL:RBMNE:2014:4792", False)
            , ("https://uk.pi-supply.com/products/pijuice-solar?v=7516fd43adaa", False)
            , ("https://undark.org/2016/05/25/the-death-of-a-study-national-childrens-study/", False)
            , ("https://understandinguncertainty.org/what-does-13-increased-risk-death-mean", False)
            , ("https://unesdoc.unesco.org/ark:/48223/pf0000220391", False)
            , ("https://practicaltypography.com/rebuilding-the-typographic-society.html", False)
            , ("https://usesthis.com/", False)
            , ("https://variety.com/2017/digital/news/netflix-thumbs-vs-stars-1202010492/", False)
            , ("https://vault.si.com/vault/2009/03/23/how-and-why-athletes-go-broke", False)
            , ("https://vgl.ucdavis.edu/tests?field_species_target_id=216", False)
            , ("https://vgmdb.net/product/9", False)
            , ("https://vimeo.com/groups/45234/videos/11912761", False)
            , ("https://vinecon.ucdavis.edu/wp-content/uploads/2019/04/cwe1201.pdf", False)
            , ("https://vision-explorer.allenai.org/text_to_image_generation", False)
            , ("https://vividness.live/protestant-buddhism", False)
            , ("https://vizhub.healthdata.org/gbd-compare/#settings=0766b73db1d02f22ea6e150ced632f13345caaee", False)
            , ("https://vk.com/alexeyguzey", False)
            , ("https://vndb.org/c582", False)
            , ("https://vosswater.com/files/VOSS-Water-Quality-Report.pdf", False)
            , ("https://waifulabs.com/blog/ax", False)
            , ("https://wandb.ai/wandb_fc/gradient-dissent/reports/What-could-make-AI-conscious-with-Wojciech-Zaremba-co-founder-of-OpenAI--Vmlldzo3NDk3MDI", False)
            , ("https://warosu.org/g/thread/69872559", False)
            , ("https://wcfcourier.com/news/local/crime-and-courts/waterloo-man-pleads-guilty-to-selling-ecstacy/article_a57aab93-b9c5-5e3e-8438-6e229c9bd036.html", False)
            , ("https://wci.llnl.gov/fac/site300/cff/index.html", False)
            , ("https://web-archive-org.translate.goog/web/20070301005837/homepage3.nifty.com/kyrie/story26.html?_x_tr_sch=http&_x_tr_sl=ja&_x_tr_tl=en&_x_tr_hl=en-US", False)
            , ("https://web-japan.org/trends00/honbun/tj010207.html", False)
            , ("https://web.econ.ku.dk/tyran/Teaching/BEecon_MA/readings_BEecon/readings%20MA_Expecon/Wolfers%20and%20Zitzewitz_Prediction%20Markets_JEP2004.pdf", False)
            , ("https://web.maths.unsw.edu.au/~jim/wrongthoughts.html", False)
            , ("https://web.mit.edu/remy/", False)
            , ("https://websitedownloader.io/", False)
            , ("https://wellcomecollection.org/", False)
            , ("https://wiki.evageeks.org/Episode_26%27#.22Last_B.22", False)
            , ("https://wordpress.org/plugins/broken-link-checker/", False)
            , ("https://works.bepress.com/laura_stambaugh/6/download/", False)
            , ("https://wudao.aminer.cn/CogView/index.html", False)
            , ("https://ww2.arb.ca.gov/our-work/programs/cap-and-trade-program", False)
            , ("https://www-sydsvenskan-se.translate.goog/2016-07-15/atal-for-storskalig-narkotikahandel?_x_tr_sl=auto&_x_tr_tl=en&_x_tr_hl=en-US", False)
            , ("https://www.1001fonts.com/goudy-initialen-font.html", False)
            , ("https://www.1m.co/details/drugs/45641/waklert-150-mg", False)
            , ("https://www.aaai.org/ocs/index.php/WS/AAAIW17/paper/viewFile/15205/14661", False)
            , ("https://www.abbiotec.com/proteins/human-ngf-beta", False)
            , ("https://www.abc.net.au/news/2015-01-07/charlie-hebdo-satirical-newspaper-shooting-paris-12-killed/6005524", False)
            , ("https://www.abc10.com/news/article/251775/2/Cross-county-drug-ring-using-dark-Internet-site-busted-by-feds", False)
            , ("https://www.abebooks.com/", False)
            , ("https://www.academia.edu/24382120", False)
            , ("https://www.acpjournals.org/doi/10.7326/M19-1326", False)
            , ("https://www.actionnewsnow.com/news/durham-couple-indicted-as-part-of-silk-road-2-0-crackdown/", False)
            , ("https://www.adafruit.com/product/328", False)
            , ("https://www.advrider.com/f/threads/seattle-to-argentina-on-a-klr650.136505/", False)
            , ("https://www.aeaweb.org/conference/2016/", False)
            , ("https://www.aei.org/publication/abolish-the-sat-2/", False)
            , ("https://www.aera.net/Research-Policy-Advocacy/AERA-Shaping-Research-Policy", False)
            , ("https://www.afp.gov.au/media-centre/news/afp/2015/may/four-australians-charged-in-international-illegal-firearm-sting", False)
            , ("https://www.afr.com/technology/ernst--young-16-million-bitcoin-auction-20160527-gp53i0", False)
            , ("https://www.ahajournals.org/doi/full/10.1161/01.cir.94.5.878", False)
            , ("https://www.aiwriter.email/", False)
            , ("https://www.ajmadison.com/cgi-bin/ajmadison/ACQ189XS.html", False)
            , ("https://www.alchemistowl.org/arrigo/Papers/Arrigo-Triulzi-PACSEC08-Project-Maux-II.pdf", False)
            , ("https://www.alexa.com/siteinfo/mnemosyne-proj.org", False)
            , ("https://www.alphagomovie.com/#deepmind", False)
            , ("https://www.alternatehistory.com/forum/threads/victoria.10/", False)
            , ("https://www.alzchem.com/", False)
            , ("https://www.alzforum.org/news/research-news/preimplantation-genetic-diagnosis-its-no-walk-park", False)
            , ("https://www.ama-assn.org/", False)
            , ("https://www.amazon.co.jp/%E7%B4%85%E4%B8%80%E7%82%B9%E8%AB%96%E2%80%95%E3%82%A2%E3%83%8B%E3%83%A1%E3%83%BB%E7%89%B9%E6%92%AE%E3%83%BB%E4%BC%9D%E8%A8%98%E3%81%AE%E3%83%92%E3%83%AD%E3%82%A4%E3%83%B3%E5%83%8F-%E3%81%A1%E3%81%8F%E3%81%BE%E6%96%87%E5%BA%AB-%E6%96%8E%E8%97%A4-%E7%BE%8E%E5%A5%88%E5%AD%90/dp/4480036660", False)
            , ("https://www.amazon.com/Watamote-Complete-Collection-Blu-ray/dp/B00JXBLM72/", False)
            , ("https://www.ams.org/notices/200605/fea-lang.pdf#page=12", False)
            , ("https://www.anandtech.com/show/12535/power-outage-at-samsungs-fab-destroys-3-percent-of-global-nand-flash-output", False)
            , ("https://www.angelfire.com/anime4/mdwigs/livesequence.html", False)
            , ("https://www.anime-planet.com/anime/years/1992", False)
            , ("https://www.animeigo.com/about/secret-history-animeigo", False)
            , ("https://www.animenewsnetwork.com/encyclopedia/company.php?id=14634", False)
            , ("https://www.annualreviews.org/doi/10.1146/annurev-psych-010416-044022", False)
            , ("https://www.antonhowes.com/", False)
            , ("https://www.apa.org/monitor/2011/09/achievement.aspx", False)
            , ("https://www.appbrain.com/app/n-back-maestro/org.urbian.android.games.nback", False)
            , ("https://www.arkansasonline.com/news/2019/nov/28/airport-beagles-sniff-out-illicit-foods/", False)
            , ("https://www.arknights.global/", False)
            , ("https://www.army.mil/article/56965/military_working_dogs_guardians_of_the_night", False)
            , ("https://www.artbreeder.com/", False)
            , ("https://www.artofmanliness.com/character/manly-lessons/the-churchill-school-of-adulthood-lesson-2-establish-a-daily-routine/", False)
            , ("https://www.arxiv-vanity.com/", False)
            , ("https://www.atlasobscura.com/articles/what-bread-did-ancient-egyptians-eat", False)
            , ("https://www.atsjournals.org/doi/full/10.1164/ajrccm.162.3.9908040", False)
            , ("https://www.avalonmagicplants.com/health/drug-test/en-cool-stuff-ez-drug-test-lsd", False)
            , ("https://www.avclub.com/ducktales-invented-a-new-animated-wonderland-that-quick-1798236288", False)
            , ("https://www.awe.gov.au/biosecurity-trade/cats-dogs/frequently-asked-questions", False)
            , ("https://www.backblaze.com/blog/hard-drive-cost-per-gigabyte/", False)
            , ("https://www.baen.com/Chapters/9781618249203/9781618249203___2.htm", False)
            , ("https://www.baltcoffee.com/catalog/loose-tea-ribbon-bags", False)
            , ("https://www.barnesandnoble.com/", False)
            , ("https://www.barnstormjournal.org/poetry/the-river-bridge/", False)
            , ("https://www.bbc.com/news/health-35262535", False)
            , ("https://www.beelinereader.com/", False)
            , ("https://www.behance.net/gallery/35437979/Velocipedia", False)
            , ("https://www.belfastlive.co.uk/news/belfast-news/northern-ireland-men-jailed-over-9147435", False)
            , ("https://www.ben-evans.com/benedictevans/2016/4/28/winning-and-losing", False)
            , ("https://www.berkshireeagle.com/ci_26681481/former-bard-college-at-simons-rock-student-avoids/", False)
            , ("https://www.berkshirehathaway.com/letters/letters.html", False)
            , ("https://www.betterworldbooks.com/", False)
            , ("https://www.bfi.org/about-bucky/resources/books/grunch-giants/foreword", False)
            , ("https://www.bfmtv.com/police-justice/info-bfmtv-loire-un-cyberdealer-interpelle-premiere-en-france_AN-201312260067.html", False)
            , ("https://www.biorxiv.org/content/10.1101/274654.full", False)
            , ("https://www.bioworld.com/content/another-miss-targacept-tc-5619-fails-adhd-trial-0", False)
            , ("https://www.birminghammail.co.uk/news/midlands-news/man-who-bought-glock-pistol-11538095", False)
            , ("https://www.bizjournals.com/albany/stories/2010/01/11/focus2.html?page=all", False)
            , ("https://www.blender.org/user-stories/japanese-anime-studio-khara-moving-to-blender/", False)
            , ("https://www.bloomberg.com/view/articles/2017-05-03/why-i-lost-my-bet-with-warren-buffett", False)
            , ("https://www.bls.gov/opub/mlr/2016/article/the-life-of-american-workers-in-1915.htm", False)
            , ("https://www.bmj.com/content/361/bmj.K2270", False)
            , ("https://www.bnlearn.com/", False)
            , ("https://www.brandonsanderson.com/sandersons-first-law/", False)
            , ("https://www.bulletproof.com/why-you-are-suffering-from-a-modafinil-deficiency/", False)
            , ("https://www.businessinsider.com/deepfake-tech-create-fictitious-faces-cats-airbnb-listings-2019-2", False)
            , ("https://www.businessinsider.jp/post-185786", False)
            , ("https://www.businesswire.com/news/home/20171121005280/en/Amazon-Celebrates-10th-Holiday-Season-Frustration-Free-Packaging", False)
            , ("https://www.buzzfeed.com/joshdean/are-we-warming-up-to-cryonics", False)
            , ("https://www.buzzfeednews.com/article/justinesharrock/whats-wrong-with-google-alerts", False)
            , ("https://www.bvp.com/anti-portfolio", False)
            , ("https://www.cabinetmagazine.org/issues/42/wiles.php", False)
            , ("https://www.calnewport.com/books/deep-work/", False)
            , ("https://www.cambridge.org/core/journals/journal-of-economic-history/article/two-centuries-of-productivity-growth-in-computing/856EC5947A5857296D3328FA154BA3A3", False)
            , ("https://www.cambridgebrainsciences.com/", False)
            , ("https://www.campbellrivermirror.com/news/257558681.html", False)
            , ("https://www.cancer.gov/about-cancer/causes-prevention/risk/tobacco/smokeless-fact-sheet#q2", False)
            , ("https://www.cancerresearchuk.org/about-us", False)
            , ("https://www.carolinacoastonline.com/news_times/article_87cc5f2c-3e8d-11e4-84f2-338083e94e20.html", False)
            , ("https://www.cato.org/pubs/pas/pa-298.html", False)
            , ("https://www.cebm.ox.ac.uk/resources/ebm-tools/critical-appraisal-tools", False)
            , ("https://www.cell.com/cell-genomics/fulltext/S2666-979X(22)00063-5", False)
            , ("https://www.cell.com/iscience/fulltext/S2589-0042(21)00473-9", False)
            , ("https://www.census.gov/newsroom/releases/archives/income_wealth/cb11-157.html", False)
            , ("https://www.change.org/p/google-keep-google-reader-running", False)
            , ("https://www.channel4.com/news/drugs-dark-web-atlantis-silk-road-police-fbi", False)
            , ("https://www.chathamstartribune.com/news/article_92645de2-c7ed-11e4-8649-679fa666eaf8.html", False)
            , ("https://www.chemistryworld.com/features/step-by-step-synthesis-of-dna/3008753.article", False)
            , ("https://www.chess.com/article/view/how-rybka-and-i-tried-to-beat-the-strongest-chess-computer-in-the-world", False)
            , ("https://www.chessclub.com/ucc", False)
            , ("https://www.chicagobooth.edu/review/what-success-rock-climbing-tells-us-about-economic-growth", False)
            , ("https://www.chinafile.com/library/nyrb-china-archive/chinese-shadows-bureaucracy-happiness-history", False)
            , ("https://www.chronicle.com/article/Google-Begins-to-Scale-Back/131109/", False)
            , ("https://www.churchofjesuschrist.org/study/scriptures/dc-testament/dc/121.45?lang=eng", False)
            , ("https://www.cisco.com/c/en/us/obsolete/routers/cisco-7505-router.html", False)
            , ("https://www.citizenaudit.org/", False)
            , ("https://www.city-journal.org/html/it-hurts-therefore-i-am-12341.html", False)
            , ("https://www.clippershipteaco.com/", False)
            , ("https://www.clubindustry.com/studies/ihrsa-reports-57-million-health-club-members-276-billion-industry-revenue-2016", False)
            , ("https://www.cmajopen.ca/content/2/2/E69.full", False)
            , ("https://www.cnbc.com/2018/03/23/elon-musk-spacex-and-tesla-were-two-of-the-dumbest-business-ideas.html", False)
            , ("https://www.cnet.com/13506_3-20040008-17.html", False)
            , ("https://www.cochranelibrary.com/doi/10.1002/14651858.CD008661/full", False)
            , ("https://www.codespaces.com/power-searching-with-google.html", False)
            , ("https://www.comicconnect.com/item/916042", False)
            , ("https://www.computerworld.com/article/2590745/is-history-repeating-itself--with-antitrust-battle-.html", False)
            , ("https://www.consumerlab.com/reviews/melatonin-supplements/melatonin/", False)
            , ("https://www.copenhagenconsensus.com/publication/second-copenhagen-consensus-micronutrient-supplements-child-survival-best-practice", False)
            , ("https://www.copyright.gov/comp3/chap300/ch300-copyrightable-authorship.pdf#Compendium%20300.indd%3A.122046%3A96431", False)
            , ("https://www.counterpunch.org/2009/03/24/the-most-dangerous-person-in-the-world/", False)
            , ("https://www.coursera.org/learn/probabilistic-graphical-models", False)
            , ("https://www.courtlistener.com/docket/4353251/united-states-v-ulbricht/", False)
            , ("https://www.courtlistener.com/docket/4367922/apotex-inc-v-cephalon-inc/", False)
            , ("https://www.courts.mo.gov/cnet/welcome.do", False)
            , ("https://www.courts.sa.gov.au/SentencingRemarks/Pages/lightbox.aspx?IsDlg=1&Filter=4029", False)
            , ("https://www.cracked.com/article_19497_6-terrifying-things-nobody-tells-you-about-donating-sperm.html", False)
            , ("https://www.crd.york.ac.uk/prospero/display_record.php?RecordID=85216", False)
            , ("https://www.cs.purdue.edu/homes/pfonseca/papers/eurosys2017-dsbugs.pdf", False)
            , ("https://www.cs.utexas.edu/~EWD/transcriptions/EWD05xx/EWD594.html", False)
            , ("https://www.cs.york.ac.uk/fp/ART/download.html", False)
            , ("https://www.csail.mit.edu/news/programmers-solve-mits-20-year-old-cryptographic-puzzle", False)
            , ("https://www.csub.edu/~mdulcich/documents/diffusion_of_responsibility.pdf", False)
            , ("https://www.dailystar.co.uk/news/weird-news/billionaires-cloned-dog-saves-lives-20895267", False)
            , ("https://www.dantelabs.com/collections/our-tests/products/whole-genome-sequencing", False)
            , ("https://www.darkowl.com/blog-content/darknet-whackamole/", False)
            , ("https://www.dea.gov/druginfo/ftp3.shtml", False)
            , ("https://www.deepmind.com/blog/agents-that-imagine-and-plan", False)
            , ("https://www.delawareonline.com/story/news/local/2015/01/13/doctor-sentenced-months-silk-road-drug-case/21716135/", False)
            , ("https://www.designboom.com/technology/evolution-desk-harvard-innovation-lab-09-30-2014/", False)
            , ("https://www.detectiveconanworld.com/wiki/The_Last_Wizard_of_the_Century#Situation", False)
            , ("https://www.devever.net/~hl/growupdown", False)
            , ("https://www.deviantart.com/tootootaloo/art/Celestia-Vector-212081002", False)
            , ("https://www.dharmaoverground.org/dharma-wiki/-/wiki/Main/MCTB/en", False)
            , ("https://www.dichtbij.nl/woerden/regionaal-nieuws/artikel/3731781/woerdenaar-47-voor-5-jaar-de-cel-in-voor-drugshandel-en-opdracht-tot-moord.aspx", False)
            , ("https://www.discovermagazine.com/planet-earth/brain-training-games-get-a-d-at-brain-training-tests", False)
            , ("https://www.dr.dk/nyheder/indland/fbi-aktion-lukker-danske-narkohandlere-paa-nettet", False)
            , ("https://www.dropbox.com/s/m748tqn9ypwiz0z/%E6%9D%B1%E6%96%B9%E7%88%86%E9%9F%B3%E3%82%B8%E3%83%A3%E3%82%BA4-%E4%BB%A4%E7%9E%91%E3%82%B3%E3%83%89%E9%9B%B6.ogg", False)
            , ("https://www.drugs.com/dosage/metformin.html", False)
            , ("https://www.drugsdata.org/faq.php", False)
            , ("https://www.duolingo.com/", False)
            , ("https://www.ebay.co.uk/itm/48-LED-illuminator-light-CCTV-IR-Infrared-Night-Vision-/180410200537", False)
            , ("https://www.ebay.com/itm/251266500210", False)
            , ("https://www.ed.ac.uk/generation-scotland/", False)
            , ("https://www.edge.org/conversation/robert_plomin-why-were-different", False)
            , ("https://www.eetimes.com/document.asp?doc_id=1333422", False)
            , ("https://www.effectuation.org/wp-content/uploads/2017/05/Do-serial-entrepreneurs-1.pdf", False)
            , ("https://www.elastic.co/blog/why-license-change-AWS", False)
            , ("https://www.emacswiki.org/emacs/MarkdownMode", False)
            , ("https://www.emcdda.europa.eu/publications/posters/2018/darknet-markets-ecosystem_en", False)
            , ("https://www.energy.gov/node/2518899", False)
            , ("https://www.engadget.com/2010-09-18-intel-wants-to-charge-50-to-unlock-stuff-your-cpu-can-already-d.html", False)
            , ("https://www.english.upenn.edu/people/paul-korshin", False)
            , ("https://www.enworld.org/threads/dungeons-ponies-at-last.664106/", False)
            , ("https://www.equilibretechnologies.com/", False)
            , ("https://www.eric.ed.gov/ERICWebPortal/custom/portlets/recordDetails/detailmini.jsp?_nfpb=true&_&ERICExtSearch_SearchValue_0=EJ724232&ERICExtSearch_SearchType_0=no&accno=EJ724232", False)
            , ("https://www.erowid.org/", False)
            , ("https://www.escholar.manchester.ac.uk/api/datastream?publicationPid=uk-ac-man-scw:227658&datastreamId=FULL-TEXT.PDF#pg=140", False)
            , ("https://www.esquire.com/news-politics/a20903/hugh-hefner-interview-0413/", False)
            , ("https://www.eurekalert.org/news-releases/636717", False)
            , ("https://www.eurojust.europa.eu/press/PressReleases/Pages/2014/2014-11-07.aspx", False)
            , ("https://www.express.co.uk/news/uk/44940/Caribbean-island-where-red-haired-locals-speak-with-Scottish-accent", False)
            , ("https://www.expressandstar.com/news/local-news/2016/05/30/gun-importer-told-to-reveal-why-he-ordered-weapon-online/", False)
            , ("https://www.facebook.com/permalink.php?story_fbid=224735391342335&id=100014176268390", False)
            , ("https://www.fanfiction.net/s/5588986/1/", False)
            , ("https://www.fastcompany.com/90659827/the-bootleg-fire-is-burning-through-trees-that-are-being-used-as-carbon-offsets", False)
            , ("https://www.fda.gov/news-events/press-announcements/fda-approves-first-oral-glp-1-treatment-type-2-diabetes", False)
            , ("https://www.fightaging.org/archives/2011/05/more-on-body-temperature-and-calorie-restriction/", False)
            , ("https://www.fightaging.org/archives/2018/09/thoughts-on-attending-raadfest-2018-in-san-diego/", False)
            , ("https://www.fimfiction.net/story/62074/Friendship-is-Optimal", False)
            , ("https://www.fincen.gov/news/news-releases/fincen-awards-recognize-partnership-between-law-enforcement-and-financial", False)
            , ("https://www.findagrave.com/memorial/7333144/john-von_neumann", False)
            , ("https://www.firstthings.com/article/2019/12/notes-on-summer-camp", False)
            , ("https://www.flashback.org/sp50195930", False)
            , ("https://www.flashgamehistory.com/", False)
            , ("https://www.flickr.com/groups/wiredsky/pool/", False)
            , ("https://www.foliosociety.com/usa/the-book-of-the-new-sun.html", False)
            , ("https://www.fool.com/investing/general/2014/12/12/122-things-everyone-should-know-about-investing-an.aspx", False)
            , ("https://www.forbes.com/sites/andygreenberg/2013/11/07/sting-operation-nabs-alleged-online-arms-dealer-via-silk-road-competitor-site/", False)
            , ("https://www.fordfoundation.org/media/2976/roads-and-bridges-the-unseen-labor-behind-our-digital-infrastructure.pdf", False)
            , ("https://www.foreignaffairs.com/articles/middle-east/2013-08-14/business-habits-highly-effective-terrorists?page=show", False)
            , ("https://www.fox6now.com/news/brookfield-man-faces-multiple-charges-related-to-drugs-trafficking", False)
            , ("https://www.foxnews.com/world/germany-3-charged-for-illegally-making-selling-firearms", False)
            , ("https://www.frbsf.org/economic-research/files/wp2017-25.pdf", False)
            , ("https://www.frc.ri.cmu.edu/~hpm/project.archive/general.articles/1991/TempComp.html", False)
            , ("https://www.freehaven.net/anonbib/topic.html", False)
            , ("https://www.fs.fed.us/rm/pubs_journals/2000/rmrs_2000_mcdaniel_g001.pdf", False)
            , ("https://www.fsigenetics.com/article/S1872-4973(18)30248-5/fulltext", False)
            , ("https://www.ftc.gov/reports/new-drug-development-estimating-entry-human-clinical-trials", False)
            , ("https://www.garda.ie/en/?Page=14304", False)
            , ("https://www.genome.gov/about-genomics/fact-sheets/DNA-Sequencing-Costs-Data", False)
            , ("https://www.girlschase.com/content/how-have-sex-asian-girls", False)
            , ("https://www.girlscouts.org/en/cookies/how-to-buy-cookies/cookies-frequently-asked-questions.html#bestselling", False)
            , ("https://www.global.toshiba/ww/news/corporate/2010/07/pr1401.html", False)
            , ("https://www.globaltimes.cn/content/1161960.shtml", False)
            , ("https://www.gloucestershirelive.co.uk/Cheltenham-student-jailed-importing-cocaine-Costa/story-28599085-detail/story.html", False)
            , ("https://www.gnu.org/software/emacs/manual/html_node/emacs/Keyboard-Macro-Counter.html", False)
            , ("https://www.goodreads.com/interviews/show/21.Malcolm_Gladwell", False)
            , ("https://www.goodtherapy.org/blog/natural-lighting-increases-productivity-0104112/", False)
            , ("https://www.google.com/reviews/w", False)
            , ("https://www.goproblems.com/", False)
            , ("https://www.gov.uk/government/news/phe-publishes-independent-expert-e-cigarettes-evidence-review", False)
            , ("https://www.greenexercise.org/", False)
            , ("https://www.gsb.stanford.edu/insights/itamar-simonson-what-makes-people-collect-things", False)
            , ("https://www.guilford.com/books/Handbook-of-Psychopathy/Christopher-Patrick/9781462541232", False)
            , ("https://www.hamilton.edu/documents/An-Analysis-of-the-Accuracy-of-Forecasts-in-the-Political-Media.pdf", False)
            , ("https://www.hanselman.com/blog/everythings-broken-and-nobodys-upset", False)
            , ("https://www.harney.com/products/organic-green-with-citrus-ginkgo", False)
            , ("https://www.harvardmagazine.com/2012/03/twilight-of-the-lecture", False)
            , ("https://www.heraldnews.com/story/news/crime/2015/06/03/fall-river-police-intercept-package/34419316007/", False)
            , ("https://www.highflightfoundation.org/", False)
            , ("https://www.hindawi.com/journals/ijg/2018/5121540/", False)
            , ("https://www.hindustantimes.com/punjab/financial-fraud-punjab-cops-brush-with-darknet-bitcoins/story-w4YDmUJFDgkV12scPzWWxO.html", False)
            , ("https://www.history.com/news/how-portraiture-shaped-royal-marriages", False)
            , ("https://www.historytoday.com/archive/feature/heads-will-roll", False)
            , ("https://www.hoover.org/publications/policy-review", False)
            , ("https://www.hrw.org/news/2017/10/22/china-voice-biometric-collection-threatens-privacy", False)
            , ("https://www.htrnews.com/story/news/local/2015/03/10/man-arrested-marijuana-shipments-two-rivers/24694327/", False)
            , ("https://www.huffpost.com/entry/photographs-redheads-of-color_n_55db9929e4b0a40aa3abf017", False)
            , ("https://www.ibtimes.co.uk/dark-web-brixton-prison-worker-charged-trying-buy-glock-19-handgun-online-1546621", False)
            , ("https://www.ice.gov/doclib/news/releases/2013/131107baltimore1.pdf", False)
            , ("https://www.idnes.cz/brno/zpravy/bitcoiny-jihomoravska-policie-rozhovor.A160816_2266665_brno-zpravy_krut", False)
            , ("https://www.ietf.org/rfc/rfc5005.txt", False)
            , ("https://www.iflscience.com/technology/man-creates-traffic-jams-by-wheeling-around-99-cell-phones-in-a-trailer/", False)
            , ("https://www.iherb.com/pr/now-foods-kelp-150-mcg-200-tablets/685", False)
            , ("https://www.illumina.com/content/dam/illumina-marketing/documents/products/brochures/datasheet_omni_whole-genome_arrays.pdf", False)
            , ("https://www.imagesco.com/catalog/DigitalCompass/DigitalCompass.html", False)
            , ("https://www.imdb.com/name/nm0663050/bio", False)
            , ("https://www.imf.org/~/media/Files/Publications/WP/2018/wp18268.ashx#pdf", False)
            , ("https://www.impactcybertrust.org/dataset_view?idDataset=812", False)
            , ("https://www.inc.com/minda-zetlin/netflix-blockbuster-meeting-marc-randolph-reed-hastings-john-antioco.html", False)
            , ("https://www.industrydocuments.ucsf.edu/tobacco/docs/#id=ssnl0112", False)
            , ("https://www.infoplease.com/us/government/elections/presidential-election-of-2008-electoral-and-popular-vote-summary", False)
            , ("https://www.informit.com/articles/article.aspx?p=2213858", False)
            , ("https://www.inputmag.com/culture/secretly-vibrant-world-of-audio-porn-mp3s", False)
            , ("https://www.insidehighered.com/news/2010/05/25/science", False)
            , ("https://www.instagram.com/nathanwpylestrangeplanet/", False)
            , ("https://www.instructables.com/Low-power-doorwindow-sensor/", False)
            , ("https://www.intel.com/pressroom/archive/releases/2007/20071025corp.htm", False)
            , ("https://www.ioccc.org/2000/bellard.hint", False)
            , ("https://www.irishcentral.com/news/the-secret-irishman-likely-behind-bitcoin-the-internet-currency-code-131039168-237415801", False)
            , ("https://www.irishexaminer.com/news/arid-30650036.html", False)
            , ("https://www.irishtimes.com/news/let-s-be-clear-i-didn-t-invent-bitcoin-1.614532", False)
            , ("https://www.ivfbabble.com/on-the-40th-anniversary-of-the-first-ivf-in-the-usa-the-first-baby-elizabeth-jordan-carr-looks-at-how-science-today-has-produced-a-new-world-first-baby-aurea/", False)
            , ("https://www.jacc.org/doi/10.1016/j.jcin.2015.01.039", False)
            , ("https://www.jackkinsella.ie/articles/autodidactism", False)
            , ("https://www.jameslindlibrary.org/articles/inventing-the-randomized-double-blind-trial-the-nurnberg-salt-test-of-1835/", False)
            , ("https://www.janelia.org/project-team/flyem/hemibrain", False)
            , ("https://www.japantimes.co.jp/news/2017/09/12/national/social-issues/1-20-infants-born-vitro-fertilization-japan-survey/", False)
            , ("https://www.jetbrains.com/lp/mono/", False)
            , ("https://www.jneurosci.org/content/jneuro/22/9/3656.full.pdf", False)
            , ("https://www.journalnow.com/business/business_news/local/targacept-sale-to-catalyst-is-complete/article_8a0008c4-4dc2-5770-81db-758e527f177b.html", False)
            , ("https://www.jstage.jst.go.jp/article/nikkashi1948/90/6/90_6_507/_pdf", False)
            , ("https://www.jstatsoft.org/index.php/jss/article/download/v048i09/601", False)
            , ("https://www.jstatsoft.org/index.php/jss/article/download/v048i10/602", False)
            , ("https://www.jstor.org/stable/10.1086/468061", False)
            , ("https://www.justice.gov/usao-edca/press-release/file/918811/download", False)
            , ("https://www.jwz.org/blog/2004/03/when-the-database-worms-eat-into-your-brain/", False)
            , ("https://www.kadokawa.co.jp/product/201012000009/", False)
            , ("https://www.kaggle.com/ultrajack/modern-renaissance-poetry", False)
            , ("https://www.kctv5.com/story/28250778/kansas-city-man-accused-of-possessing-35000-prescription-pills/", False)
            , ("https://www.kgw.com/news/Wash-county-tactical-police-conducting-3-dawn-raids-236190011.html", False)
            , ("https://www.kickstarter.com/projects/upperstory/spintronics-build-mechanical-circuits", False)
            , ("https://www.kokos.cz/bradkoun/movies/8mm.txt", False)
            , ("https://www.l-iz.de/leben/faelle-unfaelle/2015/03/leipziger-ermittler-nehmen-drogenversand-shiny-flakes-hoch-78227", False)
            , ("https://www.labone.tech/anime-generative-model-part-3/", False)
            , ("https://www.lanl.gov/bdit/html/projects/AHF.htm", False)
            , ("https://www.laphamsquarterly.org/future/trust-issues", False)
            , ("https://www.latimes.com/archives/la-xpm-2001-aug-15-fo-34311-story.html", False)
            , ("https://www.law.cornell.edu/uscode/text/10/520", False)
            , ("https://www.lef.org/Vitamins-Supplements/Item01602/Neuro-Mag-Magnesium-L-Threonate-with-Calcium-and-Vitamin-D3.html", False)
            , ("https://www.lemonde.fr/pixels/visuel/2015/03/06/google-memorial-le-petit-musee-des-projets-google-abandonnes_4588392_4408996.html", False)
            , ("https://www.leontiadis.info/NLeontiadisEPP2014.pdf", False)
            , ("https://www.liebertpub.com/doi/10.1089/hs.2021.0083", False)
            , ("https://www.liebertpub.com/doi/abs/10.1089/neu.2016.4846", False)
            , ("https://www.lightspeedmagazine.com/fiction/exhalation/", False)
            , ("https://www.limeadery.com/", False)
            , ("https://www.linode.com/", False)
            , ("https://www.loc.gov/collections/edison-company-motion-pictures-and-sound-recordings/about-this-collection/#vocal", False)
            , ("https://www.longecity.org/forum/topic/54856-modafinil-use-prosecution-convictions/", False)
            , ("https://www.loudountimes.com/news/drug-charges-dropped-against-prominent-leesburg-businessman/article_4fd16e3f-53f7-547e-94fd-8183cc465f75.html", False)
            , ("https://www.lrb.co.uk/the-paper/v27/n17/steven-shapin/what-did-you-expect", False)
            , ("https://www.lshtm.ac.uk/ncdeu/currentresearch/researchprojects/bwhhs/", False)
            , ("https://www.lyrn.ai/2018/12/26/a-style-based-generator-architecture-for-generative-adversarial-networks/", False)
            , ("https://www.maa.org/sites/default/files/images/images/upload_library/22/Polya/07468342.di020715.02p0066x.pdf", False)
            , ("https://www.macrumors.com/2011/09/08/apple-institutes-new-charitable-matching-program-for-employees/", False)
            , ("https://www.madboa.com/geek/gpg-quickstart/", False)
            , ("https://www.manchestereveningnews.co.uk/news/greater-manchester-news/montgomery-byrne-jailed-dark-web-10743557", False)
            , ("https://www.mangaupdates.com/series.html?id=2701", False)
            , ("https://www.mansfieldnewsjournal.com/story/news/local/2015/03/11/court-hole-wall-suspects-tough-luck/70152120/", False)
            , ("https://www.marxists.org/reference/archive/hegel/works/nl/ch03.htm", False)
            , ("https://www.mathematica.org/-/media/publications/pdfs/nonexperimentalreps.pdf", False)
            , ("https://www.mayoclinic.org/drugs-supplements/metformin-oral-route/proper-use/drg-20067074", False)
            , ("https://www.mayoclinicproceedings.org/article/S0025-6196%2813%2900405-9/fulltext", False)
            , ("https://www.mcsweeneys.net/articles/back-from-yet-another-globetrotting-adventure-indiana-jones-checks-his-mail-and-discovers-that-his-bid-for-tenure-has-been-denied", False)
            , ("https://www.mdpi.com/2073-4425/11/6/648", False)
            , ("https://www.medicaldaily.com/psychologists-discover-how-people-subconsciously-become-their-favorite-fictional-characters-240435", False)
            , ("https://www.medicines.org.uk/emc/medicine/23244/SPC", False)
            , ("https://www.megaverse.info", False)
            , ("https://www.mercatus.org/emergent-ventures", False)
            , ("https://www.mesacc.edu/~thoqh49081/handouts/talmudpage.html", False)
            , ("https://www.metaculus.com/questions/", False)
            , ("https://www.mha.gov.sg/", False)
            , ("https://www.mhlw.go.jp/english/database/db-hw/report/5.html", False)
            , ("https://www.microsoft.com/en-us/research/blog/zero-2-deepspeed-shattering-barriers-of-deep-learning-speed-scale/", False)
            , ("https://www.millionshort.com/", False)
            , ("https://www.mining.com/fifty-killed-in-a-knife-attack-at-a-chinese-colliery/", False)
            , ("https://www.mirror.co.uk/news/uk-news/man-jailed-trying-smuggle-550000-3592428", False)
            , ("https://www.mja.com.au/journal/1999/171/9/iodine-deficiency-ambulatory-participants-sydney-teaching-hospital-australia", False)
            , ("https://www.mobihealthnews.com/20772/exclusive-sleep-coach-company-zeo-is-shutting-down/", False)
            , ("https://www.moma.org/interactives/exhibitions/2012/inventingabstraction/?work=42", False)
            , ("https://www.morinaga.co.jp/in/jelly/", False)
            , ("https://www.msri.org/workshops/220", False)
            , ("https://www.muckrock.com/news/archives/2014/feb/03/dea-parallel-construction-guides/", False)
            , ("https://www.myfonts.com/fonts/urw/hangulatin-en/", False)
            , ("https://www.nap.edu/read/25259/chapter/2", False)
            , ("https://www.nationaldefensemagazine.org/articles/2006/3/31/2006april--soldiers-marines-team-up-in-trailblazer-patrols", False)
            , ("https://www.nato.int/docu/review/2008/04/AP_COST/EN/index.htm", False)
            , ("https://www.nature.com/articles/s41366-021-00894-3", False)
            , ("https://www.nba.com/news/takeaways-2018-19-nba-roster-survey", False)
            , ("https://www.nber.org/papers/w13711", False)
            , ("https://www.nbr.co.nz/article/chch-man-sentenced-after-buying-amazon-illegal-drugs-website-ck-135659", False)
            , ("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4959137/", False)
            , ("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6167335/", False)
            , ("https://www.ndss-symposium.org/wp-content/uploads/2019/02/ndss2019_02B-5_Wampler_paper.pdf", False)
            , ("https://www.nearlyfreespeech.net/", False)
            , ("https://www.nejm.org/doi/full/10.1056/NEJMoa1511939", False)
            , ("https://www.newadvent.org/fathers/03061.htm", False)
            , ("https://www.newegg.com/acer-aspire-v15-nitro-black-edition-vn7-591g-70rt-gaming-entertainment/p/N82E16834314849", False)
            , ("https://www.newsandstar.co.uk/news/carlisle-man-who-had-ecstasy-tablets-sent-to-friend-s-house-jailed-1.1061538/", False)
            , ("https://www.newscientist.com/article/2133095-boom-in-human-gene-editing-as-20-crispr-trials-gear-up/", False)
            , ("https://www.newsweek.com/i-cloned-my-dog-puppies-have-different-personalities-1674290", False)
            , ("https://www.newyorker.com/magazine/2017/10/16/russias-house-of-shadows", False)
            , ("https://www.newyorkfed.org/medialibrary/media/research/staff_reports/sr166.pdf", False)
            , ("https://www.nextnewdeal.net/finance/", False)
            , ("https://www.nicvape.com/store/p/48-1-liter-of-100-mg-ml.aspx", False)
            , ("https://www.nimh.nih.gov/about/directors/thomas-insel/blog/2011/the-economics-of-health-care-reform", False)
            , ("https://www.nitrd.gov/pubs/bluebooks/2001/asci.html", False)
            , ("https://www.nlsinfo.org/content/cohorts/nlsy79-children", False)
            , ("https://www.nngroup.com/articles/computer-skill-levels/", False)
            , ("https://www.nobelprize.org/prizes/chemistry/2020/press-release/", False)
            , ("https://www.nola.com/news/crime_police/article_bd297d74-f2e1-5c15-9db3-c96545f38688.html", False)
            , ("https://www.northwestgeorgianews.com/road-check-leads-to-meth-charges-for-rome-woman/article_367ea18e-1532-11eb-92a7-cf0f8ab22367.html", False)
            , ("https://www.notion.so/GPT3-Dataset-Task-Model-b97a267d6f5f44e688ba4f7ec85c00cc", False)
            , ("https://www.nottinghampost.com/Nottingham-philosophy-student-caught-importing/story-27890513-detail/story.html", False)
            , ("https://www.nrdc.org/issues/minimize-harm-and-security-risks-nuclear-energy", False)
            , ("https://www.nsa.gov/news-features/press-room/Article/1630570/national-cryptologic-museum-opens-new-exhibit-on-dr-john-nash/", False)
            , ("https://www.ntticc.or.jp/pub/ic_mag/ic018/intercity/higashi_E.html", False)
            , ("https://www.nybooks.com/articles/2011/03/10/how-we-know/", False)
            , ("https://www.nytimes.com/2014/07/30/world/africa/ransoming-citizens-europe-becomes-al-qaedas-patron.html", False)
            , ("https://www.nzherald.co.nz/nz/officers-link-to-net-drug-market/3MQUK5H6PDVBIZHTDZUG3TITMA/?c_id=1&objectid=10819029", False)
            , ("https://www.odt.co.nz/news/dunedin/student-drug-dealer-jailed", False)
            , ("https://www.oglaf.com/glindr/", False)
            , ("https://www.olin.edu/", False)
            , ("https://www.om.nl/actueel/nieuwsberichten/@88570/aanhoudingen/", False)
            , ("https://www.openphilanthropy.org/research/what-should-we-learn-from-past-ai-forecasts/", False)
            , ("https://www.optimizely.com/insights/blog/how-does-page-load-time-impact-engagement/", False)
            , ("https://www.optimox.com/iodine-study-12", False)
            , ("https://www.oreilly.com/library/view/cjkv-information-processing/9780596156114/", False)
            , ("https://www.osti.gov/biblio/10172219", False)
            , ("https://www.outsideonline.com/culture/books-media/how-athletes-get-great/", False)
            , ("https://www.outsideonline.com/outdoor-adventure/exploration-survival/snakebite-antivenom-tim-friede/", False)
            , ("https://www.overthinkingit.com/2012/11/08/my-little-pony-plato/", False)
            , ("https://www.packtpub.com/product/exploring-gpt-3/9781800563193", False)
            , ("https://www.patreon.com/gwern", False)
            , ("https://www.pbs.org/wgbh/nova/article/crispr-grapes/", False)
            , ("https://www.pcworld.com/article/422832/italian-police-shutter-dark-web-marketplace.html", False)
            , ("https://www.petcarerx.com/pcrx/ProductPages/Product.aspx?pid=10983", False)
            , ("https://www.petco.com/shop/en/petcostore/product/friskies-salmon-canned-cat-food", False)
            , ("https://www.peterbloem.nl/blog/transformers", False)
            , ("https://www.petforums.co.uk/threads/strange-things-that-you-cats-do.475528/page-3#post-1065130574", False)
            , ("https://www.philanthropy.com/article/FinancialLeadership-Woes/138335/", False)
            , ("https://www.phillymag.com/news/2007/06/18/medicine-this-will-keep-you-awake/", False)
            , ("https://www.pixiv.net/users/38983753", False)
            , ("https://www.pluralsight.com/search?q=R", False)
            , ("https://www.bbc.com/news/uk-england-35690394", False)
            , ("https://www.pnas.org/doi/full/10.1073/pnas.1320040111", False)
            , ("https://www.poetrynook.com/poem/birdless-country", False)
            , ("https://www.politie.nl/nieuws/2015/maart/9/01-onderzoek-naar-verzilveren-440.000-euro-aan-bitcoins.html", False)
            , ("https://www.polizei.bayern.de/news/presse/aktuell/index.html/222614", False)
            , ("https://www.polygon.com/2020/1/14/21064608/microsoft-kinect-history-rise-and-fall", False)
            , ("https://www.popularmechanics.com/cars/a17477/why-the-hell-do-they-still-make-car-alarms/", False)
            , ("https://www.povertyactionlab.org/sites/default/files/publications/49%20Does%20Price%20Matter%20in%20Charitable%20Giving%20Project.pdf", False)
            , ("https://www.preclinicaltrials.eu/", False)
            , ("https://www.pressandjournal.co.uk/fp/news/aberdeen-aberdeenshire/865699/top-banker-cleared-of-defrauding-pensioners-but-fined-for-selling-details-on-dark-web/", False)
            , ("https://www.prnewswire.com/news-releases/400-million-investment-programme-positions-ireland-for-global-leadership-in-genomic-research-and-advanced-life-sciences-300755716.html", False)
            , ("https://www.proofofexistence.com/", False)
            , ("https://www.propublica.org/article/when-evidence-says-no-but-doctors-say-yes", False)
            , ("https://www.prospecbio.com/beta_ngf_human", False)
            , ("https://www.psychologicalscience.org/observer/in-appreciation-julian-stanley", False)
            , ("https://www.psychologytoday.com/us/blog/pristine-inner-experience/201110/not-everyone-conducts-inner-speech", False)
            , ("https://www.quantamagazine.org/the-busy-beaver-game-illuminates-the-fundamental-limits-of-math-20201210/", False)
            , ("https://www.quora.com/Do-you-have-any-plans-for-optimizing-Duolingos-vocabulary-learning-using-spaced-repetition", False)
            , ("https://www.r-inla.org/", False)
            , ("https://www.rand.org/commentary/2011/05/29/ND.html", False)
            , ("https://www.rbmojournal.com/article/S1472-6483(19)30381-5/fulltext", False)
            , ("https://www.rechem.ca/index.php?_a=viewDoc&docId=8", False)
            , ("https://www.reddit.com/comments/1x0p1b", False)
            , ("https://www.redliongrantchester.co.uk/", False)
            , ("https://www.repository.cam.ac.uk/bitstream/handle/1810/3484/RamseyText.html?sequence=5", False)
            , ("https://www.rescuetime.com/teams", False)
            , ("https://www.research.va.gov/mvp/", False)
            , ("https://www.researchandmarkets.com/reportinfo.asp?report_id=305358", False)
            , ("https://www.researchgate.net/profile/Carole-Nowicke/publication/236212779_Secondary_and_Tertiary_Citing_A_Study_of_Referencing_Behavior_in_the_Literature_of_Citation_Analysis_Deriving_from_the_%27Ortega_Hypothesis%27_of_Cole_and_Cole/links/571e631208aead26e71a88a5/Secondary-and-Tertiary-Citing-A-Study-of-Referencing-Behavior-in-the-Literature-of-Citation-Analysis-Deriving-from-the-Ortega-Hypothesis-of-Cole-and-Cole.pdf#page=2", False)
            , ("https://www.rightstufanime.com/Utena-Revolutionary-Girl-DVD-Set-3-Apocalypse-Saga-Hyb-Limited-Edition", False)
            , ("https://www.rollingstone.com/culture/culture-features/smiley-face-murder-serial-killer-drowning-death-882042/", False)
            , ("https://www.rrauction.com/auctions/lot-detail/31909050347196/", False)
            , ("https://www.rsm.nl/fileadmin/Images_NEW/Departments/TOM/Calis_Thijmen_Thesis.pdf", False)
            , ("https://www.rte.ie/archives/2018/0322/949314-donegal-victorian-romantics/", False)
            , ("https://www.rug.nl/ggdc/productivity/pwt/pwt-releases/pwt-6.1?lang=en", False)
            , ("https://www.salesforce.com/products/einstein/ai-research/tl-dr-reinforced-model-abstractive-summarization/", False)
            , ("https://www.samharris.org/blog/the-fireplace-delusion", False)
            , ("https://www.sandia.gov/research/facilities/technology_deployment_centers/explosive_component.html", False)
            , ("https://www.science.org/content/article/plan-replicate-50-high-impact-cancer-papers-shrinks-just-18", False)
            , ("https://www.science.org/content/article/these-lab-grown-human-eggs-could-combat-infertility-if-they-prove-healthy", False)
            , ("https://www.science.org/doi/full/10.1126/science.aaf6850", False)
            , ("https://www.sciencedaily.com/releases/2008/05/080505075642.htm", False)
            , ("https://www.sciencedirect.com/science/article/pii/S0191886921003536", False)
            , ("https://www.scientificamerican.com/article/the-mind-of-an-octopus/", False)
            , ("https://www.scmp.com/news/china/science/article/3002346/chinas-first-cloned-police-dog-reports-duty", False)
            , ("https://www.scotthyoung.com/blog/", False)
            , ("https://www.scq.ubc.ca/files/63401nature.pdf", False)
            , ("https://www.screendaily.com/awards/hideaki-anno-evangelion/5081255.article?blocktitle=INTERVIEWS&contentID=41866", False)
            , ("https://www.sdfertility.com/fertility-treatments/genetic-testing/pgd", False)
            , ("https://www.seacoastonline.com/story/news/local/hampton-union/2014/05/07/hampton-man-to-plead-guilty/37408765007/", False)
            , ("https://www.sec.gov/Archives/edgar/data/1766502/000119312519124430/d665122ds1.htm", False)
            , ("https://www.semanticscholar.org/paper/Donor-Sibling-Networks-as-a-Vehicle-for-Expanding-Hertz-Nelson/6e736c71cd499fa03af6dabad3009335a8259745?p2df#page=14", False)
            , ("https://www.sendspace.com/file/ozr19p", False)
            , ("https://www.shine.cn/news/metro/1909101776/", False)
            , ("https://www.shroomery.org/forums/showflat.php/Number/18993960", False)
            , ("https://www.si.edu/", False)
            , ("https://www.simonsfoundation.org/2009/07/20/mathematicians-solve-45-year-old-kervaire-invariant-puzzle/", False)
            , ("https://www.slideshare.net/codeblue_jp/igor-skochinsky-enpub", False)
            , ("https://www.smbc-comics.com/?id=3169#comic", False)
            , ("https://www.smh.com.au/technology/teens-visit-hidden-website-for-drugs-20130319-2gddg.html", False)
            , ("https://www.smithsonianmag.com/history/crockfords-club-how-a-fishmonger-built-a-gambling-hall-and-bankrupted-the-british-aristocracy-148268691/", False)
            , ("https://www.snopes.com/fact-check/school-lemonade-laxatives/", False)
            , ("https://www.soci.org/Chemistry-and-Industry/CnI-Data/2010/24/Brussels-a-bittersweet-story", False)
            , ("https://www.sophos.com/fr-fr/medialibrary/PDFs/technical%20papers/samosseikovb2009paper.pdf", False)
            , ("https://www.spacedrepetition.com/", False)
            , ("https://www.sparkfun.com/tutorials", False)
            , ("https://www.spectator.co.uk/article/how-a-fraudulent-experiment-set-psychiatry-back-decades", False)
            , ("https://www.speedtest.net/", False)
            , ("https://www.spermbankcalifornia.com/sperm-bank-cost.html#Cryopreservation", False)
            , ("https://www.spiceandtea.com/coconut-oolong-p-587.html?zenid=ot0nli0t5cfvbitsmhi3f2pv16", False)
            , ("https://www.ssa.gov/oact/STATS/table4c6.html", False)
            , ("https://www.ssi.shimadzu.com/products/powders-particle-size-analyzers/index.html", False)
            , ("https://www.startupschool.org/", False)
            , ("https://www.stat.colostate.edu/~hooten/papers/pdf/Hooten_Hobbs_EcolMono_2015.pdf", False)
            , ("https://www.stats.govt.nz/information-releases/births-and-deaths-year-ended-december-2018", False)
            , ("https://www.stevepetersen.net/wittgenstein-fog.html", False)
            , ("https://www.stroudnewsandjournal.co.uk/news/14491113.Bussage_cannabis_dealer_Chance_Gough_avoids_jail_after_ordering_cocaine_delivery_from_Holland/", False)
            , ("https://www.sudowrite.com/", False)
            , ("https://www.super-memory.com/articles/theory.htm", False)
            , ("https://www.tandfonline.com/doi/full/10.1080/03949370.2021.1893826", False)
            , ("https://www.taylorusa.com/kitchen/thermometers/5-commercial-anti-microbial-instant-read-thermometer.html", False)
            , ("https://www.teamten.com/lawrence/writings/coding-machines/", False)
            , ("https://www.teasetc.com/", False)
            , ("https://www.teavana.com/us/en/tea/green-tea/gyokuro-genmaicha-green-tea-31440.html", False)
            , ("https://www.ted.com/talks/christopher_ryan_are_we_designed_to_be_sexual_omnivores", False)
            , ("https://www.telegraph.co.uk/news/2016/09/16/french-intelligence-cracks-telegram-account-of-most-notorious-is/", False)
            , ("https://www.tennessean.com/story/news/crime/2014/06/01/xanax-pills-seized-hermitage/9847299/", False)
            , ("https://www.tensorflow.org/tensorboard/get_started", False)
            , ("https://www.tga.gov.au/personal-importation-scheme", False)
            , ("https://www.the-tls.co.uk/articles/alternative-to-peer-review-essay-timothy-gowers/", False)
            , ("https://www.theadvertiser.com/story/news/crime/2016/08/30/lafayette-man-sentenced-18-months-prison-shipping-drugs-via-silk-road/89604606/", False)
            , ("https://www.theage.com.au/politics/queensland/strawberry-growers-shut-the-farm-gate-after-tampering-crisis-20190402-p519zk.html", False)
            , ("https://www.theatlantic.com/technology/archive/2012/05/the-perfect-milk-machine-how-big-data-transformed-the-dairy-industry/256423/", False)
            , ("https://www.thedailybeast.com/vegan-youtube-is-imploding-as-stars-like-rawvana-bonny-rebecca-and-stella-rae-change-diets", False)
            , ("https://www.thediff.co/p/how-bubbles-and-megaprojects-parallelize", False)
            , ("https://www.theguardian.com/technology/2014/may/30/life-after-silk-road-how-the-darknet-drugs-market-is-booming", False)
            , ("https://www.thehomesecuritysuperstore.com/collections/driveway-alarms", False)
            , ("https://www.theladders.com/research-2/3556/", False)
            , ("https://www.thelancet.com/journals/ebiom/article/PIIS2352-3964(19)30591-2/fulltext", False)
            , ("https://www.thelocal.de/20160729/german-darknet-weapons-dealer-sentenced-to-5-years-jail/", False)
            , ("https://www.themarshallproject.org/2018/04/19/framed-for-murder-by-his-own-dna", False)
            , ("https://www.thenewatlantis.com/publications/shop-class-as-soulcraft", False)
            , ("https://www.theonion.com/study-wolf-attacks-still-leading-cause-of-death-in-u-s-1819574862", False)
            , ("https://www.theringer.com/features/2021/6/21/22542839/disc-golf-niche-sports-million-dollar-endorsement-deals", False)
            , ("https://www.theroot.com/exactly-how-black-is-black-america-1790895185", False)
            , ("https://www.thessgac.org/data", False)
            , ("https://www.thestar.com/vancouver/2018/07/05/bc-lays-claim-to-14-million-us-in-bitcoin-from-drug-dealer-over-alleged-links-to-silk-road.html", False)
            , ("https://www.thetimes.co.uk/article/dark-net-dealers-dragged-into-the-light-kx05bc3jd", False)
            , ("https://www.theverge.com/2021/10/28/22750337/shadow-planet-ai-robin-sloan-jesse-solomon-clark", False)
            , ("https://www.thisamericanlife.org/668/transcript", False)
            , ("https://www.thisstorydoesnotexist.com/", False)
            , ("https://www.thoughtco.com/is-distance-learning-right-for-you-1098087", False)
            , ("https://www.thv11.com/news/crime/23-yr-old-allegedly-managed-ark-drug-ring-through-postal-service-/100936404", False)
            , ("https://www.ti.com/product/MSP430F2003", False)
            , ("https://www.timeanddate.com/date/durationresult.html?m1=9&d1=16&y1=2012&m2=3&d2=9&y2=2013&ti=on", False)
            , ("https://www.timesofisrael.com/police-investigating-toddler-death-from-nicotine-overdose/", False)
            , ("https://www.tinyletter.com/", False)
            , ("https://www.tn.gov/education/early-learning/voluntary-pre-k.html", False)
            , ("https://www.topic.com/the-62-year-old-child-genius", False)
            , ("https://www.toplessrobot.com/2010/09/the_12_most_anti-american_anime.php", False)
            , ("https://www.torproject.org/download/", False)
            , ("https://www.torservers.net/donate.html", False)
            , ("https://www.townandcountrymag.com/society/a12108750/personal-protection-dogs/", False)
            , ("https://www.treasurydirect.gov/govt/reports/pd/histdebt/histdebt_histo5.htm", False)
            , ("https://www.tryhaskell.org/", False)
            , ("https://www.tuftandneedle.com/", False)
            , ("https://www.tweaktown.com/news/32703/industry-analyst-still-unsure-of-the-significance-of-hynix-fire/index.html", False)
            , ("https://www.twitch.tv/videos/416276005#openai", False)
            , ("https://www.typografie.info/3/Schriften/fonts.html/deutsche-zierschrift-r250/", False)
            , ("https://www.typography.com/blog/text-for-proofing-fonts", False)
            , ("https://www.uber.com/us/en/beacon/", False)
            , ("https://www.ukbiobank.ac.uk/frontiers-meeting-london-2014", False)
            , ("https://www.ummah.com/forum/forum/library/learn-arabic-and-other-languages/qur-an-and-islamic/390413-how-to-memorize-the-quran-and-never-forget-it?381181-How-to-Memorize-the-Quran-and-Never-Forget-it=", False)
            , ("https://www.unicef.org/pon95/nutr0009.html", False)
            , ("https://www.unirioja.es/cu/anromero/MofC12.pdf", False)
            , ("https://www.upi.com/Health_News/2011/04/27/Omega-3-may-up-aggressive-prostate-cancer/31131303903320/?u3L=1", False)
            , ("https://www.upjohn.org/data-tools/employment-research-data-center/continuous-longitudinal-manpower-surveys", False)
            , ("https://www.uptontea.com/shopcart/item.asp?itemID=ZH80", False)
            , ("https://www.uscourts.gov/services-forms/federal-court-reporting-program", False)
            , ("https://www.usenix.org/legacy/events/sec99/full_papers/whitten/whitten.ps", False)
            , ("https://www.usgs.gov/special-topics/water-science-school/science/total-water-use-united-states", False)
            , ("https://www.uso.org/about", False)
            , ("https://www.uv.es/sestio/TechRep/tr14-03.pdf", False)
            , ("https://www.verywellhealth.com/thyroid-disease-diagnosis-4013578", False)
            , ("https://www.vg.no/nyheter/innenriks/i/Ovnm1/fire-paagrepet-for-produksjon-og-nettsalg-av-narkotika", False)
            , ("https://www.vice.com/en/article/gv5x4q/court-docs-show-a-university-helped-fbi-bust-silk-road-2-child-porn-suspects", False)
            , ("https://www.vitacost.com/natures-answer-valerian-root-alcohol-free-1-fl-oz", False)
            , ("https://www.vox.com/xpress/2014/10/2/6875031/chickens-breeding-farming-boilers-giant", False)
            , ("https://www.wahpetondailynews.com/oregon-man-accused-of-selling-fentanyl-that-led-to-nd/article_de0df64e-cf41-11e4-af1c-1b8df43f199d.html", False)
            , ("https://www.walesonline.co.uk/news/wales-news/silk-road-20-drug-dealer-9507820", False)
            , ("https://www.walmart.com/ip/Great-Value-Fudge-Mint-Cookies-10-oz/11997740", False)
            , ("https://www.washingtonexaminer.com/weekly-standard/the-history-of-russian-terrorism-dagger-and-swagger", False)
            , ("https://www.watercoolertrivia.com/blog/gpt-3-vs-water-cooler-trivia-participants-a-human-vs-robot-showdown", False)
            , ("https://www.wcscanada.org/portals/42/media/file/WSB_Ocelot.pdf", False)
            , ("https://www.wdrb.com/news/louisville-man-accused-of-obtaining-potential-date-rape-drug-online/article_c0f1b63f-c52e-50b5-8233-7403082ac0af.html", False)
            , ("https://www.webcitation.org/6Qj7v6mqd", False)
            , ("https://www.welt.de/regionales/muenchen/article117869257/Fahnder-zerschlagen-Bitcoin-Drogenring-DarkNet.html", False)
            , ("https://www.whio.com/news/news/crime-law/police-4-pound-of-pot-mailed-to-oxford/nkgqT/", False)
            , ("https://www.who.int/classifications/classification-of-diseases", False)
            , ("https://www.wickedlocal.com/story/weymouth-news/2015/03/27/police-weymouth-man-got-mdma/34892786007/", False)
            , ("https://www.williamsondailynews.com/news/4846/two-pill-dealers-sentenced-to-federal-prison-for-drug-crimes/", False)
            , ("https://www.wine-searcher.com/find/jackson+triggs+vidal+ice+rsrv+niagara+peninsula+ontario+canada/1", False)
            , ("https://www.winonadailynews.com/news/local/crime-and-courts/winona-postal-inspector-sniffs-out-dope/article_1b24cfe9-75ef-5407-9d02-a916907f7259.html", False)
            , ("https://www.wireheading.com/", False)
            , ("https://www.wnycstudios.org/podcasts/radiolab/episodes/91725-words#ember29180666", False)
            , ("https://www.wolframalpha.com/input/?i=male+height+distribution", False)
            , ("https://www.worksinprogress.co/issue/better-eats/", False)
            , ("https://www.worldcat.org/title/educational-and-vocational-preferences-of-a-cohort-spatially-gifted-females-and-males-from-the-study-of-mathematically-precocious-youth/oclc/42465636&referer=brief_results", False)
            , ("https://www.wsmv.com/story/25667210/metro-police-seize", False)
            , ("https://www.wunderground.com/history/airport/KNHK/2012/7/11/CustomHistory.html?dayend=22&monthend=3&yearend=2013&req_city=NA&req_state=NA&req_statename=NA&format=1", False)
            , ("https://www.wwltv.com/news/Laundry-List-of-Drugs-at-Uptown-Frat-House-Leads-to-Tulane-Student-Arrests-193450441.html", False)
            , ("https://www.wzzm13.com/mb/news/crime/meth-laden-express-mail-parcel-brings-charges-for-holland-man/287645651", False)
            , ("https://www.xilinx.com/prs_rls/silicon_spart/0333spartan3.htm", False)
            , ("https://www.xn--4dbcyzi5a.com/%d7%9e%d7%94%d7%95%d7%9c%d7%a0%d7%93-%d7%91%d7%90%d7%94%d7%91%d7%94-%d7%a0%d7%aa%d7%a4%d7%a1-%d7%a6%d7%a2%d7%99%d7%a8-%d7%a9%d7%94%d7%96%d7%9e%d7%99%d7%9f-%d7%97%d7%91%d7%99%d7%9c%d7%95%d7%aa-%d7%a7/", False)
            , ("https://www.ycombinator.com/documents/", False)
            , ("https://www.york.ac.uk/depts/maths/histstat/fisher274.pdf", False)
            , ("https://www108.lamp.le.ac.uk/ojs1/index.php/pst/issue/archive", False)
            , ("https://www2.bfi.org.uk/films-tv-people/4ce2b707390e5", False)
            , ("https://www2.ed.gov/rschstat/eval/tech/evidence-based-practices/finalreport.pdf", False)
            , ("https://www2.guidestar.org/profile/13-1624016", False)
            , ("https://www2.psy.uq.edu.au/~uqbziets/Mosing%20et%20al%202015%20Did%20sexual%20selection%20shape%20human%20music.pdf", False)
            , ("https://www3.ntu.edu.sg/czzhao/iq/test.htm", False)
            , ("https://wwwcn.cs.uni-duesseldorf.de/publications/publications/library/Jerschow2010a.pdf", False)
            , ("https://wyclif.substack.com/p/the-natural-selection-paper-part-908", False)
            , ("https://yourmorals.org/", False)
            , ("https://yp.flutterguy.org/", False)
            , ("https://yunnansourcing.com/collections/flower-and-herbal-teas/products/yunnan-sun-dried-wild-rose-buds-from-wenshan", False)
            , ("https://yunnansourcing.us/", False)
            , ("https://annals.org/article.aspx?articleid=745807", False)
            , ("https://www.teds.ac.uk/about-teds", False)
            , ("https://stability.ai/blog/stable-diffusion-public-release", False)
            ]
