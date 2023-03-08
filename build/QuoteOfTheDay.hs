module QuoteOfTheDay where

-- A simple 2-tuple database for storing text snippets which can be transcluded. The initial use of this was for 'quote of the day' snippet generation.
--
-- 'Quote of the day' is an old website feature where, for visitors' edification or amusement, a random quote from a list of quotes would be display, often in the website footer or homepage.
-- An example is <https://en.wikiquote.org/wiki/Wikiquote:Quote_of_the_day> which is transcluded in the middle of <https://en.wikiquote.org/wiki/Main_Page>, or <https://web.archive.org/web/20150410044208/http://bbs.stardestroyer.net/SiteBanner.php?display=history>.
-- A common source of quotes used to be <https://en.wikipedia.org/wiki/Fortune_(Unix)>; see also <https://www.lesswrong.com/tag/rationality-quotes>.
--
-- A QOTD database is a `[Quote]` Haskell file, read with read/show. A `Quote` is a 3-tuple `(String, String, Bool)`: HTML "quote", HTML "attribution [or other commentary/metadata]", and whether it has been "used yet" (not entirely necessary, since one could sample randomly, but tracked to minimize reuse of quotes).† When all quotes have been used and are `True`, they get reset to `False` and the cycle begins again (with, presumably, new quotes added since the last time).
-- Quotes & attributions do not contain double-quote delimiters or other HTML wrapping; that will be added when they are formatted as Gwern.net-style 'epigraphs' to be written.
--
-- The QOTD db is used by read in the QOTDB, selecting the first unused quote, marking it used & writing out the updated db, and then writing out the quote to a particular file in a HTML format; that file is used by downstream users such as Hakyll websites which template or transclude it.
-- With the use of `transclude.js`, this can be as simple as:
--
-- `<div class="qotd"><a class="include" href="/metadata/today-quote.html">Quote Of The Day</a></div>`
--
-- † The probability that a daily visitor would see a duplicate quote under simple random sampling grows rapidly with time; see <https://en.wikipedia.org/wiki/Birthday_problem>. If eg. there were 366 quotes, then after only 23 visits, the reader would have a 50-50 chance of seeing ≥1 duplicate!

import Control.Monad (unless, when)
import qualified Data.Set as S (delete, empty, filter, fromList, toList, insert, map)
import System.Directory (doesFileExist)
import Text.Show.Pretty (ppShow)
import qualified Data.Text as T (isInfixOf, pack, Text)
import qualified Data.Map.Strict as M (toList, filterWithKey, map, fromListWith)
import Data.List (isSuffixOf, sortOn, sort)

import LinkMetadataTypes (Metadata)
import LinkMetadata (typesetHtmlField)
import LinkBacklink (readBacklinksDB)
import Utils (host, anyInfixT)

type TTDB = [Snippet]
type Snippet = (String, String, Bool)

quoteDBPath, quotePath :: FilePath
quoteDBPath = "metadata/quotes.hs"
quotePath   = "metadata/today-quote.html"
quoted :: Snippet -> String
quoted (quote,attribution,_) = "<div class=\"epigraph\">\n<blockquote><p>" ++ typesetHtmlField quote ++ "</p>" ++ if null attribution then "" else ("\n<p>" ++ typesetHtmlField attribution ++ "</p>") ++ "</blockquote>\n</div>"

linkDBPath, linkPath :: FilePath
linkDBPath = "metadata/links.hs"
linkPath   = "metadata/today-link.html"
linked :: Snippet -> String
linked (link,attribution,_) = "<div class=\"link-of-the-day\">\n<blockquote><p><a href=\"" ++ link ++ "\">" ++ typesetHtmlField attribution ++ "</a></p></blockquote>\n</div>"

readTTDB :: FilePath -> IO TTDB
readTTDB path = do exists <- doesFileExist path
                   if not exists then return [] else fmap read $ readFile path

writeTTDB :: FilePath -> TTDB -> IO ()
writeTTDB path = writeFile path . ppShow

writeSnippet :: FilePath -> (Snippet -> String) -> Snippet -> IO ()
writeSnippet path formatter x = writeFile path (formatter x)

generateSnippetAndWriteTTDB :: FilePath -> FilePath -> (Snippet -> String) -> IO ()
generateSnippetAndWriteTTDB dbpath path formatter =
  do dblist <- readTTDB dbpath
     when (null dblist) $ error $ "Fatal error: tuple database " ++ path ++ " is empty?"
     unless (not $ any (\(q,_,_) -> null q) dblist) $ error $ "Fatal error: tuple database has empty first-fields? " ++ show dblist
     let db = S.fromList dblist

     -- get set of usable quotes, and if there are none, reset the entire set and use that:
     let dbUnused = S.filter (\(_,_,status) -> not status) db
     let dbReset = if dbUnused /= S.empty then db else S.map snegate db
     let dbUnused' = S.filter (\(_,_,status) -> not status) dbReset

     let snippet = head $ sortOn (\(q, _, _) -> length q) $ S.toList dbUnused' -- take the smallest quote, for symmetry with the annotation being largest (for links, won't matter how it's sorted, really)
     writeSnippet path formatter snippet

     let db'' = S.insert (snegate snippet) $ S.delete snippet dbReset -- update the now-used quote
     writeTTDB dbpath $ S.toList db''

 where snegate :: Snippet -> Snippet
       snegate (a,b,s) = (a,b,not s)

aotd :: Metadata -> IO ()
aotd md = generateAnnotationOfTheDay md annotDayDB  annotPath annotated
qotd, lotd :: IO ()
qotd    = generateSnippetAndWriteTTDB   quoteDBPath quotePath quoted
lotd    = generateSnippetAndWriteTTDB   linkDBPath  linkPath  linked

-------

-- same idea: each build, we pick an annotation which hasn't been shown before (uses are tracked in a simple Haskell DB), currently picking by what is the 'longest annotation' (as measured by raw string length) as a crude proxy for 'best', and—tag-directory style—write an `{.annotation-include-partial}` snippet for transcluding into the footer of each page after the quote-of-the-day.

annotDayDB, annotPath :: String
annotDayDB = "metadata/annotations.hs"
annotPath = "metadata/today-annotation.html"

minAbstractLength :: Int
minAbstractLength = 500

type AotD = [String]

annotated :: String -> String
annotated url = "<div class=\"annotation-of-the-day\">\n<p><a href=\"" ++ url ++ "\" class=\"include-annotation-partial backlink-not include-spinner-not icon-not\">[Annotation Of The Day]</a></p></blockquote>\n</div>"

readAnnotDayDB :: FilePath -> IO AotD
readAnnotDayDB path = do exists <- doesFileExist path
                         if not exists then return [] else fmap read $ readFile path
writeAnnotDayDB :: FilePath -> AotD -> IO ()
writeAnnotDayDB path = writeFile path . ppShow

generateAnnotationOfTheDay :: Metadata -> FilePath -> FilePath -> (String -> String) -> IO ()
generateAnnotationOfTheDay md dbpath annotpath formatter =
  do db <- readAnnotDayDB dbpath
     let md' = M.toList $ M.filterWithKey (\k (_,author,_,_,_,abstract1) ->
                                              length abstract1 > minAbstractLength &&
                                              author /= "Gwern Branwen" &&
                                              k `notElem` db &&
                                              not ("/index" `isSuffixOf` k)) md
     let lengthList = sortOn (\(_, (_,_,_,_,_,abstract2)) -> length abstract2) md' -- ascending order (ie. largest last)
     if null lengthList then writeFile [] dbpath else
       do let (url,_) = last lengthList -- grab the largest
          let db' = url : db
          writeFile annotpath (formatter url)
          writeAnnotDayDB dbpath db'

-----------
--- link-of-the-day prioritizing

-- to find URLs worth considering for lotd use, , pass through a list of URLs (perhaps extracted
-- from the backlinks database) and return domains with at least `linkMin` matches. (Link icons
-- are enough work that below a certain level of prevalence, they are not worthwhile even if completely
-- transparent/self-explanatory.)
--
-- The original raw results are particularly useful when piped into <https://gwern.net/haskell/lcp.hs> to
-- get suggested prefixes/domains, or one can just look at the domains by `host`:
sitePrioritize :: IO [T.Text]
sitePrioritize = do b <- LinkBacklink.readBacklinksDB
                    lotdb <- readTTDB linkDBPath
                    let lotdbl = map (\(u,_,_) -> T.pack u) lotdb
                    let b' = M.toList $ M.map length b
                    let b'' = map (\(y,z) -> (host y,z)) $ filter (\(url,_) ->  host url `notElem` blackList &&
                                                                                not (anyInfixT url lotdbl) &&
                                                                                ("." `T.isInfixOf` url)) b'
                    let b''' =  M.fromListWith (+) b''
                    return $ map snd $ reverse $ sort $ filter (\(e,f) -> e >= linkMin && f /="") $ map (\(c,d) -> (d,c)) $ M.toList b'''
  where linkMin = 2 :: Int
        blackList :: [T.Text] -- definitely excluded from recs
        blackList = [
          "07th-expansion.fandom.com", "17th-angel.tumblr.com", "2chan.us", "abandonedfootnotes.blogspot.com", "abcnews.go.com",
          "abcnotation.com", "academic.oup.com", "academictorrents.com", "aclanthology.org", "acoup.blog",
          "actavet.vfu.cz", "addons.mozilla.org", "advancedfertility.com", "aeon.co", "ageing.oxfordjournals.org",
          "ageofem.com", "agtb.wordpress.com", "aidungeon.medium.com", "ai.facebook.com", "ai.google",
          "ai.stanford.edu", "ajp.psychiatryonline.org", "alexdanco.com", "algo2.iti.kit.edu", "algorithmsbook.com",
          "altjapan.typepad.com", "alumni.media.mit.edu", "andyljones.com", "andymatuschak.org", "animekritik.wordpress.com",
          "annals.org", "ansuz.sooke.bc.ca", "answers.google.com", "antiagingcentral.com", "antonhowes.tumblr.com",
          "apenwarr.ca", "apjcn.org", "apnews.com", "apps.apple.com", "apps.dtic.mil",
          "arankomatsuzaki.wordpress.com", "arbital.com", "arbtt.nomeata.de", "archinte.jamanetwork.com", "archive.ahrq.gov",
          "archive.computerhistory.org", "archive.nytimes.com", "archive.org", "archive.ph", "archive.recapthelaw.org",
          "archives.haskell.org", "arcsecond.wordpress.com", "arima.cylab.cmu.edu", "armenag.com", "arr.am",
          "arstechnica.com", "articles.latimes.com", "arxiv.org", "ascii.textfiles.com", "askakorean.blogspot.com",
          "ask-gpt.tumblr.com", "ask.metafilter.com", "asktog.com", "astateofsugar.bandcamp.com", "au.news.yahoo.com",
          "aur.archlinux.org", "authenticorganizations.com", "authors.library.caltech.edu", "avocado-slice.tumblr.com", "aws.amazon.com",
          "aydao.ai", "azure.microsoft.com", "babel.hathitrust.org", "bastiat.org", "bayes.wustl.edu",
          "beepb00p.xyz", "behavioralscientist.org", "benanne.github.io", "beneinstein.medium.com", "bentilly.blogspot.com",
          "beta.openai.com", "betsofbitco.in", "betterhumans.coach.me", "bifunctor.homelinux.net", "bitcoincharts.com",
          "bitcoin-otc.com", "bitcointalk.org", "bjp.rcpsych.org", "blakemasters.tumblr.com", "blockchain.info",
          "blog.23andme.com", "blog.8faces.com", "blog.aboutamazon.com", "blog.archive.org", "blog.codinghorror.com",
          "blog.cr.yp.to", "blog.cryptographyengineering.com", "blog.csdn.net", "blog.darcs.net", "blog.eleuther.ai",
          "blog.ethereum.org", "blog.givewell.org", "blog.google", "blog.johantibell.com", "blog.ml.cmu.edu",
          "blog.mozilla.org", "blog.novelai.net", "blog.nuclearsecrecy.com", "blog.platypope.org", "blog.regehr.org",
          "blogs.microsoft.com", "blogs.nvidia.com", "blogs.scientificamerican.com", "blogs.wsj.com", "blog.xkcd.com",
          "blog.youtube", "bluelight.org", "bmcneurosci.biomedcentral.com", "bmcpublichealth.biomedcentral.com", "bmcvetres.biomedcentral.com",
          "bmk.sh", "boardgamegeek.com", "boards.fireden.net", "boingboing.net", "booksearch.blogspot.com",
          "books.google.com", "book.webtypography.net", "brainworkshop.sourceforge.net", "buddhism-for-vampires.com", "bugs.darcs.net",
          "bugs.launchpad.net", "burrsettles.com", "cacm.acm.org", "caniuse.com", "capitalteas.com",
          "carbonplan.org", "care.diabetesjournals.org", "carryiton.net", "casual-effects.com", "catb.org",
          "cdn.discordapp.com", "cdn.harvardlawreview.org", "cdn.openai.com", "christina.kim", "chronopause.com",
          "circ.ahajournals.org", "citeseerx.ist.psu.edu", "clagnut.com", "classics.mit.edu", "cleantypecheck.wordpress.com",
          "clemenswinter.com", "clinicaltrials.gov", "cloud.google.com", "coalition4evidence.org", "codegolf.stackexchange.com",
          "code.google.com", "cognitivemedium.com", "cogracenotes.wordpress.com", "cogsci.mindmodeling.org", "colab.research.google.com",
          "colinraffel.com", "community.haskell.org", "compvis.github.io", "core.ac.uk", "cosmosmagazine.com",
          "courses.csail.mit.edu", "coveryourtracks.eff.org", "cpb-us-e1.wpmucdn.com", "cran.r-project.org", "creativecommons.org",
          "crookedtimber.org", "crypto.stackexchange.com", "crypto.stanford.edu", "cs231n.github.io", "cse.google.com",
          "cse-robotics.engr.tamu.edu", "cset.georgetown.edu", "cs.gmu.edu", "cs.nyu.edu", "cs.stanford.edu",
          "ctan.org", "culture.org", "cva.stanford.edu", "dailynous.com", "dalspace.library.dal.ca",
          "danbooru.donmai.us", "daniellakens.blogspot.com", "danijar.com", "darcs.net", "darenome.wordpress.com",
          "darrenjw.wordpress.com", "dash.harvard.edu", "data.bls.gov", "datacolada.org", "dataprivacylab.org",
          "data.worldbank.org", "david-abel.github.io", "davidbrin.blogspot.com", "davidreiley.com", "daviskedrosky.substack.com",
          "deadline.com", "deathnote.fandom.com", "deepblue.lib.umich.edu", "deepimagination.cc", "deepmindsafetyresearch.medium.com",
          "demos.obormot.net", "derpibooru.org", "desystemize.substack.com", "developer.mozilla.org", "developer.nvidia.com",
          "dev.kanotype.net", "dialnet.unirioja.es", "diff.wikimedia.org", "digitalcommons.unl.edu", "digital.library.unt.edu",
          "discovery.ucl.ac.uk", "disegnojournal.com", "diyhpl.us", "dl.acm.org", "dnstats.net",
          "docs.ankiweb.net", "docs.google.com", "dominiccummings.com", "dominiccummings.files.wordpress.com", "donsbot.com",
          "doughanley.com", "douthat.substack.com", "downloads.hindawi.com", "dresdencodak.com", "drive.google.com",
          "drugs-forum.com", "dspace.mit.edu", "duckduckgo.com", "dumps.wikimedia.org", "dwarffortresswiki.org",
          "dzone.com", "econstudentlog.wordpress.com", "edwardtufte.github.io", "egamebook.com", "ehp.niehs.nih.gov",
          "elifesciences.org", "eli.thegreenplace.net", "emilkirkegaard.dk", "en.bitcoin.it", "en.chessbase.com",
          "energycontrol.org", "engineering.fb.com", "eng.uber.com", "en.touhouwiki.net", "en.wikifur.com",
          "en.wikiquote.org", "en.wikisource.org", "en.wiktionary.org", "eprint.iacr.org", "eprints.lse.ac.uk",
          "eprints.nottingham.ac.uk", "eprints.whiterose.ac.uk", "eric.ed.gov", "escapingflatland.substack.com", "escholarship.org",
          "etherscan.io", "evacommentary.org", "eva-fan.com", "evaresources.wordpress.com", "eveningstarmusic.bandcamp.com",
          "everything2.com", "every.to", "ew.com", "exac.broadinstitute.org", "examine.com",
          "faculty.chicagobooth.edu", "faculty.haas.berkeley.edu", "familiarcycle.net", "felinegenetics.missouri.edu", "figshare.manchester.ac.uk",
          "files.eric.ed.gov", "files.givewell.org", "files.osf.io", "findarticles.com", "fis.fda.gov",
          "fivethirtyeight.com", "fma.fandom.com", "fontsinuse.com", "foreignpolicy.com", "forre.st",
          "fortune.com", "forum.bitcoin.org", "forum.effectivealtruism.org", "forum.evageeks.org", "forum.quantifiedself.com",
          "forums.animesuki.com", "freakonomics.com", "freedomdefined.org", "ftfy.readthedocs.io", "ftp.iza.org",
          "fullstackeconomics.com", "fursona.app", "gainax.co.jp", "gainax.fr", "galois.com",
          "galton.org", "gamefaqs.gamespot.com", "garote.bdmonkeys.net", "garrisonulrich.bandcamp.com", "gazette.com",
          "genepi.qimr.edu.au", "genius.com", "genomebiology.biomedcentral.com", "gettermario.dynamicforum.net", "gigaom.com",
          "gigascience.biomedcentral.com", "gigazine.net", "gist.github.com", "github.blog", "github.com",
          "git.io", "gitlab.com", "gitlab.haskell.org", "gizmodo.com", "globalguerrillas.typepad.com",
          "globalvoices.org", "go.gitcoin.co", "gondwanaland.com", "googleblog.blogspot.com", "google-summer-of-code-2009-haskell.googlecode.com",
          "googlesystem.blogspot.com", "gosling.psy.utexas.edu", "gptprompts.wikidot.com", "gradientscience.org", "granta.com",
          "greekreporter.com", "groups.google.com", "groups.yahoo.com", "gundam.fandom.com", "gutenberg.net.au",
          "gwern.substack.com", "habr.com", "hackage.haskell.org", "hal.archives-ouvertes.fr", "hal-enpc.archives-ouvertes.fr",
          "halshs.archives-ouvertes.fr", "handbook-5-1.cochrane.org", "hapgood.us", "harpers.org", "hbr.org",
          "healthland.time.com", "herbsutter.com", "hf.co", "highnoongmt.wordpress.com", "highscalability.com",
          "history.nasa.gov", "hivemind-repo.s3-us-west-2.amazonaws.com", "hn.algolia.com", "home.cs.colorado.edu", "home.inklingmarkets.com",
          "homepage3.nifty.com", "homepages.se.edu", "hoogle.haskell.org", "host.robots.ox.ac.uk", "huangwl18.github.io",
          "hub.darcs.net", "huggingface.co", "humanvarieties.org", "icml.cc", "ideas.4brad.com",
          "ideas.repec.org", "ieeexplore.ieee.org", "iforcedabot.com", "ignorethecode.net", "iiif.wellcomecollection.org",
          "ije.oxfordjournals.org", "imagen.research.google", "imatge-upc.github.io", "imgur.com", "incompleteideas.net",
          "inhumanexperiment.blogspot.com", "innovationandgrowth.wordpress.com", "intellectualmathematics.com", "intelligence.org", "intrade.com",
          "investor.lilly.com", "iopscience.iop.org", "itre.cis.upenn.edu", "jalammar.github.io", "jamanetwork.com",
          "jamesyu.org", "jaspervdj.be", "jeffhuang.com", "jenni.uchicago.edu", "jessegalef.com",
          "jetpress.org", "jigsaw.w3.org", "jmlr.org", "jn.nutrition.org", "johnhawks.net",
          "journals.ametsoc.org", "journals.lww.com", "journals.physiology.org", "journals.plos.org", "journals.sagepub.com",
          "journal.stuffwithstuff.com", "jov.arvojournals.org", "jtauber.com", "jtoomim.org", "jukebox.openai.com",
          "junyanz.github.io", "justpaste.it", "kancolle.fandom.com", "kanzaki.sub.jp", "kk.org",
          "knightcolumbia.org", "knowyourmeme.com", "koeln.ccc.de", "k-on.fandom.com", "kotaku.com",
          "labs.la.utexas.edu", "lacbzxobeprssrfx.onion", "laion.ai", "lambdalabs.com", "laplab.ucsd.edu",
          "lareviewofbooks.org", "latanyasweeney.org", "latitude.io", "lavaan.ugent.be", "ldsc.broadinstitute.org",
          "learn.microsoft.com", "lectoblix.bandcamp.com", "legacy.imagemagick.org", "letters.temporarystate.net", "libgen.rs",
          "library.mpib-berlin.mpg.de", "library.sciencemadness.org", "lifescivc.com", "link.springer.com", "lists.urth.net",
          "lists.wikimedia.org", "lithub.com", "lobste.rs", "longtermrisk.org", "lostpinesyaupontea.com",
          "lucumr.pocoo.org", "ludix.com", "lxj616.github.io", "magenta.tensorflow.org", "mailchi.mp",
          "mail.haskell.org", "make.girls.moe", "mako.cc", "maps.org", "marco.org",
          "markets.nitle.org", "mason.gmu.edu", "mathoverflow.net", "mathshistory.st-andrews.ac.uk", "math.stackexchange.com",
          "mathworld.wolfram.com", "mattlakeman.org", "mbe.oxfordjournals.org", "mc-stan.org", "mdpi-res.com",
          "meaningness.com", "media.nature.com", "medicalhypotheses.blogspot.com", "medieval.bodleian.ox.ac.uk", "medium.com",
          "medlineplus.gov", "meehl.dl.umn.edu", "meehl.umn.edu", "mega.nz", "messybeast.com",
          "meta.wikimedia.org", "meteuphoric.com", "metropolitician.blogs.com", "mikepower.pressfolios.com", "milan.cvitkovic.net",
          "minecraft.fandom.com", "minimaxir.com", "mitpress.mit.edu", "mlg.eng.cam.ac.uk", "mlp.fandom.com",
          "mmlab.ie.cuhk.edu.hk", "mnemosyne-proj.org", "money.cnn.com", "motherboard.vice.com", "moz.com",
          "mssv.net", "msu.edu", "mujoco.org", "muse.jhu.edu", "myanimelist.net",
          "mynationalistpony.tumblr.com", "my.pgp-hms.org", "my.vanderbilt.edu", "naldc.nal.usda.gov", "nap.nationalacademies.org",
          "ncase.me", "nces.ed.gov", "nearcyan.com", "neojaponisme.com", "neomarxisme.com",
          "netecon.seas.harvard.edu", "neurosciencenews.com", "new.cognitivefun.net", "newcriterion.com", "new.nubrain.com",
          "news.bbc.co.uk", "news.microsoft.com", "news.ycombinator.com", "ngm.nationalgeographic.com", "nickdrozd.github.io",
          "nitro.biosci.arizona.edu", "nitter.moomoo.me", "nmteaco.com", "n.neurology.org", "noahpinion.substack.com",
          "nonsymbolic.org", "nootropicsdepot.com", "norvig.com", "nostalgebraist.tumblr.com", "novelai.net",
          "nowak.ece.wisc.edu", "numinous.productions", "nutritionj.biomedcentral.com", "nv-adlr.github.io", "nyaa.si",
          "nymag.com", "nypost.com", "ohtori.nu", "okmij.org", "old.reddit.com",
          "oll.libertyfund.org", "omega0.xyz", "onlinelibrary.wiley.com", "online.ucpress.edu", "online.wsj.com",
          "openaccess.thecvf.com", "openai.com", "openreview.net", "opensnp.org", "opinionator.blogs.nytimes.com",
          "orbit.dtu.dk", "orbi.uliege.be", "originstamp.com", "orionmagazine.org", "oscarbonilla.com",
          "osf.io", "otago.ourarchive.ac.nz", "ourworldindata.org", "paddlehelix.baidu.com", "pages.jh.edu",
          "pages.ucsd.edu", "pair-code.github.io", "palmerlab.org", "pandoc.org", "papergains.co",
          "papers.nips.cc", "papers.ssrn.com", "papers.tinbergen.nl", "paradise.caltech.edu", "parti.research.google",
          "pastebin.com", "patch.com", "patch-tag.com", "patents.google.com", "pauillac.inria.fr",
          "paulfchristiano.com", "pavelfatin.com", "pcdb.santafe.edu", "pdf.guidestar.org", "pdfs.semanticscholar.org",
          "pediatrics.aappublications.org", "peerj.com", "penelope.uchicago.edu", "people.csail.mit.edu", "people.eecs.berkeley.edu",
          "people.idsia.ch", "people.virginia.edu", "personal.lse.ac.uk", "petertodd.org", "pharmacyreviewer.co",
          "pharmrev.aspetjournals.org", "philip.greenspun.com", "philpapers.org", "philsci-archive.pitt.edu", "physicsoffinance.blogspot.com",
          "physicstoday.scitation.org", "phys.org", "pinboard.in", "pirate.london", "plato.stanford.edu",
          "play.aidungeon.io", "player.vimeo.com", "play.google.com", "plaza.harmonix.ne.jp", "plewis.info",
          "plus.google.com", "polisen.se", "politicalcalculations.blogspot.com", "politicalscience.osu.edu", "polymatharchives.blogspot.com",
          "poniesatdawn.bandcamp.com", "ponyphonic.bandcamp.com", "popcon.ubuntu.com", "portal.idc.ac.il", "post45.org",
          "poststar.com", "pps.sagepub.com", "precedings.nature.com", "predictionbook.com", "priceonomics.com",
          "proceedings.mlr.press", "proceedings.neurips.cc", "proebsting.cs.arizona.edu", "programmablesearchengine.google.com", "programme.exordo.com",
          "projecteuclid.org", "project-rainbowcrack.com", "psmag.com", "psyarxiv.com", "psych.hanover.edu",
          "psychnet.wustl.edu", "psychonautwiki.org", "psych.wisc.edu", "psycnet.apa.org", "publicdomainreview.org",
          "publishing.cdlib.org", "pubmed.ncbi.nlm.nih.gov", "pubs.acs.org", "pubsonline.informs.org", "pure.tue.nl",
          "puzzles.nigelcoldwell.co.uk", "pytorch.org", "qa.debian.org", "quantifiedself.com", "quantum.country",
          "queue.acm.org", "quillette.com", "quoteinvestigator.com", "qz.com", "r6.ca",
          "rachelbythebay.com", "radar.oreilly.com", "rajivsethi.blogspot.com", "ralphmerkle.com", "rd.springer.com",
          "readwrite.com", "reason.com", "repository.si.edu", "repository.uel.ac.uk", "repository.upenn.edu",
          "researchdmr.com", "reviverestore.org", "rezero.fandom.com", "rheumatology.oxfordjournals.org", "ria.ru",
          "ro.ecu.edu.au", "royalsocietypublishing.org", "rpubs.com", "rspb.royalsocietypublishing.org", "rstb.royalsocietypublishing.org",
          "ruder.io", "s3.amazonaws.com", "s3.documentcloud.org", "s3-us-west-2.amazonaws.com", "samoburja.com",
          "samuraijack.fandom.com", "scale.com", "scarybeastsecurity.blogspot.com", "schizophreniabulletin.oxfordjournals.org", "scholar.google.com",
          "scholar.harvard.edu", "scholarspace.manoa.hawaii.edu", "scholars-stage.org", "schoolgirlmilkycrisis.com", "scienceblogs.com",
          "scottbarrykaufman.com", "scottlilienfeld.com", "searchengineland.com", "secretlaboratory.org", "secure.flickr.com",
          "selfie2anime.com", "semiengineering.com", "senrigan.io", "sensebridge.net", "senseis.xmp.net",
          "serre-lab.clps.brown.edu", "sethroberts.net", "sevensecularsermons.org", "sf-encyclopedia.com", "sgp.fas.org",
          "shkspr.mobi", "sigbovik.org", "signalvnoise.com", "silkroad5v7dywlc.onion", "silkroadvb5piz3r.onion",
          "sites.google.com", "sites.research.google", "sive.rs", "slashdot.org", "slate.com",
          "slatestarscratchpad.tumblr.com", "snpf.org", "socghop.appspot.com", "socialsciences.mcmaster.ca", "sociologicalscience.com",
          "soranews24.com", "soundcloud.com", "sourceforge.net", "sparky.haskell.org", "spectrum.ieee.org",
          "speechresearch.github.io", "spikejapan.wordpress.com", "spp.fas.org", "sre.google", "srush.github.io",
          "ssgac.org", "sss.sagepub.com", "stability.ai", "stackoverflow.com", "starwars.fandom.com",
          "stat.columbia.edu", "static1.squarespace.com", "static-content.springer.com", "static.googleusercontent.com", "stats.grok.se",
          "stats.org.uk", "stats.stackexchange.com", "steve-yegge.blogspot.com", "storage.googleapis.com", "story.californiasunday.com",
          "strategy.wikimedia.org", "stripe.com", "stroke.ahajournals.org", "super.gluebenchmark.com", "super-memory.com",
          "superuser.com", "support.google.com", "support.mozilla.org", "support.torproject.org", "svilentodorov.xyz",
          "takimag.com", "tannerlectures.utah.edu", "taooftea.com", "tasvideos.org", "techcrunch.com",
          "terrytao.wordpress.com", "tfhub.dev", "thebaffler.com", "theconversation.com", "thegrandnarrative.com",
          "thehardestscience.com", "thehub7dnl5nmcz5.onion", "theintercept.com", "thelastpsychiatrist.com", "the-liliquarium.livejournal.com",
          "thenextweb.com", "thepharmacyexpress.com", "thepiratebay.org", "thesecatsdonotexist.com", "thesession.org",
          "thisanimedoesnotexist.ai", "thispersondoesnotexist.com", "thisponydoesnotexist.net", "thisrentaldoesnotexist.com", "threadreaderapp.com",
          "tiltfactor.org", "time.com", "timesofindia.indiatimes.com", "timetravel.mementoweb.org", "tl.net",
          "today.yougov.com", "togetter.com", "tomcritchlow.com", "tools.wmflabs.org", "torch.ch",
          "towardsdatascience.com", "training.cochrane.org", "training.kalzumeus.com", "trajectory-transformer.github.io", "trixter.oldskool.org",
          "turner.substack.com", "ubc-emotionlab.ca", "uitspraken.rechtspraak.nl", "ultra.fandom.com", "undark.org",
          "universalprior.substack.com", "upload.wikimedia.org", "uweb.cas.usf.edu", "vanishingpoint.air-nifty.com", "variety.com",
          "vast.ai", "vault.si.com", "venturebeat.com", "vgmdb.net", "vimeo.com",
          "vinoshipper.com", "visualgenome.org", "vividness.live", "vizhub.healthdata.org", "vndb.org",
          "voltpon3.bandcamp.com", "waifulabs.com", "wandb.ai", "washingtonmonthly.com", "w.atwiki.jp",
          "wavemotioncannon.com", "wcfcourier.com", "wci.llnl.gov", "web.archive.org", "web-archive-org.translate.goog",
          "webcitation.org", "web.elastic.org", "web.maths.unsw.edu.au", "web.mit.edu", "web.stanford.edu",
          "well.blogs.nytimes.com", "wellcomecollection.org", "well-typed.com", "wiki.c2.com", "wiki.evageeks.org",
          "wiki.haskell.org", "wiki.lesswrong.com", "wikimediafoundation.org", "wiki.obormot.net", "wordcraft-writers-workshop.appspot.com",
          "works.bepress.com", "world.hey.com", "writeswith.com", "writings.stephenwolfram.com", "wudao.aminer.cn",
          "www.1m.co", "www2.math.upenn.edu", "www2.psych.ubc.ca", "www3.nd.edu", "www.4nrx-uk.md",
          "www.aaai.org", "www.abcb.com", "www.abc.net.au", "www.abetterpage.com", "www.acpjournals.org",
          "www.adafruit.com", "www.afp.gov.au", "www.aging-us.com", "www.aiweirdness.com", "www.alchemistowl.org",
          "www.alcor.org", "www.aleph.se", "www.alessonislearned.com", "www.alexa.com", "www.alignmentforum.org",
          "www.alljapaneseallthetime.com", "www.alternatehistory.com", "www.alzchem.com", "www.amazon.co.jp", "www.amazon.com",
          "www.ams.org", "www.anandtech.com", "www.andrew.cmu.edu", "www.andzuck.com", "www.angelfire.com",
          "www.animeigo.com", "www.animenewsnetwork.com", "www.animenewsservice.com", "www.antipope.org", "www.antiquark.com",
          "www.antonhowes.com", "www.apa.org", "www.archive-it.org", "www.austlii.edu.au", "www.avclub.com",
          "www.avmf.org", "www.awe.gov.au", "www.backblaze.com", "www.baen.com", "www.baka-tsuki.org",
          "www.baltimoresun.com", "www.bartleby.com", "www.bayesianinvestor.com", "www.bbc.com", "www.bbc.co.uk",
          "www.behance.net", "www.belfastlive.co.uk", "www.belfasttelegraph.co.uk", "www.bemmu.com", "www.benkuhn.net",
          "www.berkshirehathaway.com", "www.biorxiv.org", "www.biostat.jhsph.edu", "www.bizjournals.com", "www.blockchain.com",
          "www.bloomberg.com", "www.bls.gov", "www.bmj.com", "www.bostonglobe.com", "www.brainpreservation.org",
          "www.bronxbanterblog.com", "www.brookings.edu", "www.bu.edu", "www.bulletproof.com", "www.businessinsider.com",
          "www.businessweek.com", "www.buttercupfestival.com", "www.buzzfeed.com", "www.buzzfeednews.com", "www.buzzricksons.jp",
          "www.byrnehobart.com", "www.c82.net", "www.cambridgebrainsciences.com", "www.cambridge.org", "www.candyjapan.com",
          "www.cap-lore.com", "www.catalogtree.net", "www.catb.org", "www.cato.org", "www.cato-unbound.org",
          "www.cbsnews.com", "www.cdc.gov", "www.cehd.umn.edu", "www.cell.com", "www.census.gov",
          "www.chicagomag.com", "www.chicagotribune.com", "www.chinadaily.com.cn", "www.chronicle.com", "www.cia.gov",
          "www.city-journal.org", "www.cjas.org", "www.cjr.org", "www.cl.cam.ac.uk", "www.clinicaltrials.gov",
          "www.cnbc.com", "www.cnn.com", "www.cochranelibrary.com", "www.cognitiongroup.com", "www.collectorsweekly.com",
          "www.collisiondetection.net", "www.columbia.edu", "www.comicbox.co.jp", "www.communicationcache.com", "www.computerworld.com",
          "www.copenhagenconsensus.com", "www.copyright.gov", "www.couriermail.com.au", "www.courtlistener.com", "www.cram.com",
          "www.csail.mit.edu", "www.cs.cmu.edu", "www.cs.columbia.edu", "www.cs.cornell.edu", "www.cs.dartmouth.edu",
          "www-cs-faculty.stanford.edu", "www.cs.odu.edu", "www.cs.toronto.edu", "www.cs.utexas.edu", "www.cs.virginia.edu",
          "www.cs.york.ac.uk", "www.culhwch.info", "www.daemonology.net", "www.dafont.com", "www.dailydot.com",
          "www.dailymail.co.uk", "www.dailystar.co.uk", "www.danmurphydc.com", "www.dartmouth.edu", "www.davidbordwell.net",
          "www.davidbrin.com", "www.daviddarling.info", "www.davidsongifted.org", "www.dcscience.net", "www.dea.gov",
          "www.deepmind.com", "www.depauw.edu", "www.deseret.com", "www.deviantart.com", "www.dhammawiki.com",
          "www.direct-ms.org", "www.discovermagazine.com", "www.discoverteas.com", "www.ditext.com", "www.diva-portal.org",
          "www.dna.caltech.edu", "www.dobuusagi.com", "www.drugs.com", "www.drugsdata.org", "www.dtic.mil",
          "www.dummy-system.com", "www.eaton.com", "www.e-codices.unifr.ch", "www.econlib.org", "www.economist.com",
          "www.econstor.eu", "www.edge.org", "www.eleuther.ai", "www.emcdda.europa.eu", "www.equestriadaily.com",
          "www.escholar.manchester.ac.uk", "www.esquire.com", "www.eurekalert.org", "www.evalegend.com", "www.evamonkey.com",
          "www.evaotaku.com", "www.evstigneev.net", "www.ex.org", "www.explainxkcd.com", "www.expressandstar.com",
          "www.express.co.uk", "www.facebook.com", "www.fadedpage.com", "www.fanfiction.net", "www.fasebj.org",
          "www.fast.ai", "www.fastcase.com", "www.fastcompany.com", "www.fathomevents.com", "www.fda.gov",
          "www.fhi.ox.ac.uk", "www.filfre.net", "www.fimfiction.net", "www.fincen.gov", "www.firstthings.com",
          "www.flashback.org", "www.flashgamehistory.com", "www.flickr.com", "www.forbes.com", "www.fordfoundation.org",
          "www.fox6now.com", "www.foxnews.com", "www.freehaven.net", "www.frontiersin.org", "www.fsb.muohio.edu",
          "www.ft.com", "www.galbithink.org", "www.gao.gov", "www.gatsby.ucl.ac.uk", "www.gawker.com",
          "www.genealogy.math.ndsu.nodak.edu", "www.genome.gov", "www.ghibli.jp", "www.girlscouts.org", "www.givewell.org",
          "www.globalsecurity.org", "www.global.toshiba", "www.goodreads.com", "www.google.com", "www.gq.com",
          "www.grandforksherald.com", "www.greenexercise.org", "www.gutenberg.org", "www.harney.com", "www.harrowell.org.uk",
          "www.harvardmagazine.com", "www.haskell.org", "www.hbs.edu", "www.heraldsun.com.au", "www.highbeam.com",
          "www.hindawi.com", "www.holidaymead.com", "www.hoover.org", "www.hpmor.com", "www.hrw.org",
          "www.hse.gov.uk", "www.hsx.com", "www.huffpost.com", "www.hyrumslaw.com", "www.iacr.org",
          "www.iapsych.com", "www.ice.gov", "www.icir.org", "www.icsd.k12.ny.us", "www.ieee-security.org",
          "www.iherb.com", "www.imagesco.com", "www.imdb.com", "www.ime.usp.br", "www.impactcybertrust.org",
          "www.inc.com", "www.incompleteideas.net", "www.independent.co.uk", "www.independent.ie", "www.indiana.edu",
          "www.industrydocuments.ucsf.edu", "www.infinitychess.com", "www.infinityplus.co.uk", "www.inputmag.com", "www.instagram.com",
          "www.instructables.com", "www.intechopen.com", "www.ioccc.org", "www.irishtimes.com", "www.irrodl.org",
          "www.ism.ac.jp", "www.isteve.com", "www.jackkinsella.ie", "www.jacobinmag.com", "www.jacurutu.com",
          "www.jameslindlibrary.org", "www.japaninc.com", "www.japantimes.co.jp", "www.jefftk.com", "www.jmlr.org",
          "www.jneurosci.org", "www.johndcook.com", "www.johnsonessays.com", "www.jonathanstray.com", "www.jstage.jst.go.jp",
          "www.jstatsoft.org", "www.jstor.org", "www.juliansanchez.com", "www.justice.gov", "www.justinpinkney.com",
          "www.jwz.org", "www.kadokawa.co.jp", "www.kaggle.com", "www.kalzumeus.com", "www.karger.com",
          "www.kegel.com", "www.kellogg.northwestern.edu", "www.khara.co.jp", "www.kickstarter.com", "www.klingberglab.se",
          "www.kooslooijesteijn.net", "www.koreatimes.co.kr", "www.labone.tech", "www.lanl.gov", "www.larspenke.eu",
          "www.latimes.com", "www.law.nyu.edu", "www.lef.org", "www.lemonde.fr", "www.lesswrong.com",
          "www.lightspeedmagazine.com", "www.links.org", "www.lipreading.org", "www.livestrong.com", "www.longecity.org",
          "www.longevityhistory.com", "www.louischauvel.org", "www.lrb.co.uk", "www.lyrn.ai", "www.maa.org",
          "www.mail-archive.com", "www.mangauk.com", "www.mangaupdates.com", "www.mania.com", "www.marit.hinnosaar.net",
          "www.marxists.org", "www.math.tau.ac.il", "www.math.uwaterloo.ca", "www.mattblaze.org", "www.mcall.com",
          "www.mctb.org", "www.mdpi.com", "www.mdrc.org", "www.mediawiki.org", "www.med.mcgill.ca",
          "www.medrxiv.org", "www.memteaimports.com", "www.mentalfloss.com", "www.mercurynews.com", "www.metaculus.com",
          "www.metafilter.com", "www.metopera.org", "www.metzdowd.com", "www.michaellight.net", "www.microsoft.com",
          "www.mirror.co.uk", "www.mit.edu", "www.moma.org", "www.moserware.com", "www.motherjones.com",
          "www.nap.edu", "www.nature.com", "www.nausicaa.net", "www.nbcnews.com", "www.nber.org",
          "www.ncbi.nlm.nih.gov", "www.nejm.org", "www.nestle-watersna.com", "www.newscientist.com", "www.newstatesman.com",
          "www.newsweek.com", "www.newyorker.com", "www.nextbigfuture.com", "www.nextplatform.com", "www.nicvape.com",
          "www.nngroup.com", "www.nobelprize.org", "www.noisebridge.net", "www.nola.com", "www.northjersey.com",
          "www.notion.so", "www.novo-pi.com", "www.npr.org", "www.nps.gov", "www.nti.org",
          "www.nyaa.eu", "www.nyaa.se", "www.nybooks.com", "www.nydailynews.com", "www.nytimes.com",
          "www.nzherald.co.nz", "www.odt.co.nz", "www.offconvex.org", "www.oglaf.com", "www.ohri.ca",
          "www.openphilanthropy.org", "www.optimox.com", "www.orbuch.com", "www.oregonlive.com", "www.oreilly.com",
          "www.oricon.co.jp", "www.orlandosentinel.com", "www.osti.gov", "www.otakustudy.com", "www.overthinkingit.com",
          "www.owenstephens.co.uk", "www.patreon.com", "www.pcmag.com", "www.pcworld.com", "www.pelleas.net",
          "www.petco.com", "www.petforums.co.uk", "www.pewresearch.org", "www.pharma.uzh.ch", "www.pnas.org",
          "www.poetryfoundation.org", "www.politie.nl", "www.popsci.com", "www.popularmechanics.com", "www.pragmatic.ml",
          "www.pressandjournal.co.uk", "www.princeton.edu", "www.project-imas.com", "www.prolific.co", "www.propublica.org",
          "www.proquest.com", "www.protocol.com", "www.protoculture.ca", "www.psychologicalscience.org", "www.psychologytoday.com",
          "www.psy.lmu.de", "www.quora.com", "www.rand.org", "www.rationaloptimist.com", "www.rechem.ca",
          "www.regruntled.com", "www.replicatedtypo.com", "www.repository.cam.ac.uk", "www.rescuetime.com", "www.researchgate.net",
          "www.research.va.gov", "www.reuters.com", "www.rferl.org", "www.rfreitas.com", "www.richardcarrier.info",
          "www.righto.com", "www.rightstufanime.com", "www.r-inla.org", "www.robot-learning.uk", "www.rocketpunk-manifesto.com",
          "www.roft.io", "www.rollingstone.com", "www.rrauction.com", "www.rte.ie", "www.rug.nl",
          "www.rwagner.net", "www.rxshop.md", "www.sacbee.com", "www.salon.com", "www.samharris.org",
          "www.sankakucomplex.com", "www.sas.upenn.edu", "www.scielo.br", "www.scienceagogo.com", "www.sciencealert.com",
          "www.sciencedaily.com", "www.sciencedirect.com", "www.sciencemadness.org", "www.sciencenews.org", "www.science.org",
          "www.scientificamerican.com", "www.scifiscripts.com", "www.scmp.com", "www.scottaaronson.com", "www.scotthyoung.com",
          "www.sebastianmarshall.com", "www.seistronix.com", "www.semanticscholar.org", "www.sequentialtart.com", "www.sfgate.com",
          "www.shawlocal.com", "www.shawwn.com", "www.shroomery.org", "www.sirlin.net", "www.sjsu.edu",
          "www.slate.com", "www.slideshare.net", "www.smarternootropics.com", "www.smartpowders.com", "www.smbc-comics.com",
          "www.smh.com.au", "www.smithsonianmag.com", "www.snpedia.com", "www.snpp.com", "www.southwales-eveningpost.co.uk",
          "www.spectator.co.uk", "www.spiceandtea.com", "www.spiegel.de", "www.ssc.wisc.edu", "www.standard.co.uk",
          "www.stat.columbia.edu", "www.strobe-statement.org", "www.stuartcheshire.org", "www.stuff.co.nz", "www.sudowrite.com",
          "www.sumsar.net", "www.sun-modalert.com", "www.syracuse.com", "www.tabletmag.com", "www.talyarkoni.org",
          "www.tandfonline.com", "www.tarsnap.com", "www.teanobi.com", "www.teavana.com", "www.technologyreview.com",
          "www.ted.com", "www.teds.ac.uk", "www.telegraph.co.uk", "www.tensorflow.org", "www.terrierman.com",
          "www.theadvertiser.com", "www.theage.com.au", "www.theatlantic.com", "www.thecut.com", "www.thedailybeast.com",
          "www.thefreelibrary.com", "www.theguardian.com", "www.thelancet.com", "www.thelocal.de", "www.theparisreview.org",
          "www.theregister.com", "www.theringer.com", "www.the-scientist.com", "www.thesmokinggun.com", "www.thestranger.com",
          "www.theverge.com", "www.thisfursonadoesnotexist.com", "www.thiswaifudoesnotexist.net", "www.thoughtco.com", "www.timesofisrael.com",
          "www.tlmc.eu", "www.tomodachi.de", "www.tomshardware.com", "www.tor.com", "www.torproject.org",
          "www.tranquiltuesdays.com", "www.trixietracker.com", "www.tuftandneedle.com", "www.tweelingenregister.org", "www.uk-anime.net",
          "www.ukbiobank.ac.uk", "www.ultrasparky.org", "www.unf.edu", "www.unicode.org", "www.unirioja.es",
          "www.unitedpharmacies-uk.md", "www.unm.edu", "www.unqualified-reservations.org", "www.unz.com", "www.upi.com",
          "www.uptontea.com", "www.urbandharma.org", "www.urbandictionary.com", "www.urth.net", "www.usagi.org",
          "www.usenix.org", "www.vanityfair.com", "www.verywellhealth.com", "www.vesta.earth", "www.vetta.org",
          "www.vg.no", "www.vice.com", "www.vitacost.com", "www.vogue.com", "www.vox.com",
          "www.w3.org", "www.walmart.com", "www.washington.edu", "www.washingtonpost.com", "www.washingtontimes.com",
          "www.wdrb.com", "www.webcitation.org", "www.weidai.com", "www.wellbeingintlstudiesrepository.org", "www.whichfaceisreal.com",
          "www.whirlpool.com", "www.who.int", "www.wired.com", "www.wjh.harvard.edu", "www.wolfewiki.com",
          "www.wolframalpha.com", "www.worldcat.org", "www.wsj.com", "www.wunderground.com", "www.xinhuanet.com",
          "www.ycombinator.com", "www.yorku.ca", "www.youtube.com", "www.yudkowsky.net", "wyclif.substack.com",
          "xkcd.com", "xtools.wmflabs.org", "yunnansourcing.com", "yurideigin.medium.com", "zerocoin.org",
          "zlkj.in"]