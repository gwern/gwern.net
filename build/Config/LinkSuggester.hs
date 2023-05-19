{-# LANGUAGE OverloadedStrings #-}
module Config.LinkSuggester where

import Data.List (intercalate)
import qualified Data.Text as T (head, length, unpack, isPrefixOf, isSuffixOf, Text)
import qualified Data.Map.Strict as M (fromList, Map)
import Text.Regex.TDFA ((=~))
import Network.URI (isURI)
import Utils (anyInfixT, anyPrefixT)

hitsMinimum, anchorLengthMaximum :: Int
hitsMinimum = 4
anchorLengthMaximum = 80

-- return True if matches any blacklist conditions
filterURLs :: T.Text -> Bool
filterURLs u = anyPrefixT u ["$","\8383","#","/static/img/","/metadata/","/newsletter/20","dropbox.com","https://www.harney.com"] ||
               u `elem` ["https://www.reuters.com/article/us-germany-cyber-idUSKCN1071KW"] ||
               "/doc/" `T.isPrefixOf` u && "/index" `T.isSuffixOf` u ||
               "https://gwern.net/doc/" `T.isPrefixOf` u && "/index" `T.isSuffixOf` u

filterAnchors :: T.Text -> Bool
filterAnchors   t = T.length t > anchorLengthMaximum ||
                    -- filter out tag-directories: I don't usually want to link them because they are either redundant with the tag that will be on the annotation as a whole, or they have probably a better definition link. This comes up a lot with 'IQ' or 'heritability' or 'GPT' etc.
                    t =~ regex ||
                    anyInfixT t ["$","%","[","]"] ||
                    anyPrefixT t ["(",".", "Wikipedia link about "] ||
                    "&"==t ||
                    elem t badStrings
  where regex = intercalate "|" $ map (\r -> "^"++r++"$") ["[0-9]+[kmgbt]?", "[0-9]+[\\.,;–-][0-9]+", "pg[0-9]+", "p\\.[0-9]+", "[0-9]+[a-f]", "in [12][0-9][0-9][0-9]", "[Ff]igure S?[0-9]+[a-f]?", "[Tt]able S?[0-9]+[a-f]?", "[Cc]hapter [0-9]+"]
        badStrings = ["has shown", "the end", "in the", "One", "one", "ast", "sed", "le", "intro", "Quest", "in the",
                      "very fast", "it was noted", "ODS", "diff of", "AK", "#8", "DL", "DM", "GQ", "H+", "JS", "NS",
                      "OA", "TR", "TV", "Xs", "i2", "s9", "v3", "Ant", "Ash", "CDC", "CO2", "DFA", "DOI", "DoJ", "FAQ",
                      "FDA", "GOS", "Her", "III", "IRC", "Lab", "Law", "MP4", "Net", "No.", "OA5", "P&G", "PDF", "RNA",
                      "RSS", "SR1", "SVG", "TSG", "bre", "cev", "mor", "free", "Goog", "HTML", "Hero", "JSON", "Lili",
                      "Loui", "MIRI", "MeFi", "NYRB", "Open", "Poll", "& AI", "3\8211\&5\215", ">307", "S9E4", "S6E9",
                      "S5E9", "S5E5", "S5E2", "S5E1", "S3E4", "S3E3", "S2E8", "S2E4", "S1E2", "S1E1", "5\8211\&8",
                      "1\8211\&5", "1\8211\&4", "15\215", "SR1F", "SR2F", "Tofu", "Vice", "WaOi", "XSLT", "p.\160\&2",
                      "s1\8211\&8", "Alone", "Alpha", "Brave", "Dwarf", "Image", "Howes", "Karma", "Korff", "Okada",
                      "Paper", "Rotem", "Raman", "Quora", "Salon", "SciAm", "Share", "Slate", "Spent", "Slice", "Verge",
                      "Zagat", "as is", "aries", "anti-", "ac.uk", "gwern", "don\8217t", "is to", "blixt", "the E",
                      "thin,", "won\8217t", "0.45kg", "Bamboo", "Echoes", "Erowid", "Genius", "Hoogle", "I read", "Indeed",
                      "58,020", "4-item", "100GHz", "10-50k", "0.45kg", "\27531\12425\12378\12398\26862", "\26481\26041\22934\12293\22818",
                      "\24651\24515\12402\12392\12388", "S9E23", "S9E21", "S9E14", "S9E13", "S8E27", "S8E23", "S8E21",
                      "S8E20", "S8E14", "S7E24", "S7E14", "S7E13", "S7E10", "S6E25", "S6E22", "S5E25", "S5E21", "S5E18",
                      "S5E13", "S5E12", "S4E26", "S4E25", "S4E18", "S3E13", "S3E12", "S3E11", "S2E26", "S2E25", "S2E15",
                      "S2E14", "S1E26", "S1E14", "S1E11", "Part 1", "Singal", "Tumblr", "XMonad", "a book", "a scan",
                      "a vice", "and do", "c2.com", "ccc.de", "ch14.5", "game 1", "game 2", "goo.gl", "jttoto", "may be",
                      "muflax", "ponzis", "so low", "tality", "to why", "w3.org", "2 weeks", "BLDBLOG", "Hackage",
                      "Mark Xu", "ON FIRE", "Part II", "Table 4", "The Age", "USB hub", "Vaniver", "a third", "a rumor",
                      "a quarter", "a fifth", "a sixth", "a seventh", "a half", "help me", "held up", "be made", "bls.gov",
                      "by fans", "dataset", "doi.org", "fda.gov", "grok.se", "jwz.org", "lwn.net", "make it", "mega.nz",
                      "nih.gov", "nips.css", "npr.org", "or more", "oup.com", "over an", "r = 0.4", "row1.ca", "the USA",
                      "the BBC", "ted.com", "tale of", "vox.com", "Al-Qaeda", "Alzforum", "I set up", "March 11", "NBC News",
                      "NYT 2011", "Part Two", "Part One", "Pulp Mag", "Redditor", "Reset.me", "SD cards", "STRATFOR",
                      "See here", "T-shirts", "To quote", "U-shaped", "UOChris1", "a genius", "a review", "a saying",
                      "about it", "ajcn.org", "aleph.se", "dtic.mil", "fair use", "followup", "gnxp.com", "has said",
                      "has told", "he wrote", "her book", "iacr.org", "ieet.org", "ietf.org", "it sells", "imdb.com",
                      "leme.me/", "leme.me", "mdpi.com", "mid-2012", "nber.org", "ncase.me", "nejm.org", "one site",
                      "one user", "our book", "pet flap", "pre-2012", "pre-GWAS", "season 9", "synaptic", "tells us",
                      "the code", "the king", "the seen", "the size", "this one", "to solve", "type=pdf", "youtu.be",
                      "vice.com", "#facebook", "was also", "Ashkenazi", "Chapter 5", "Dune-like", "Gainax.fr", "How often",
                      "IQs lower", "LW review", "Lyons Den", "New Links", "OA claims", "SR forums", "Salon.com", "Some work",
                      "Top books", "US states", "acne page", "a service", "a network", "cdlib.org", "busted in", "demo quiz",
                      "doc88.com", "does know", "does seem", "gwern.net", "imgur.com", "isfdb.org", "libgen.is", "last very",
                      "long-term", "made bail", "many ways", "maybe not", "my config", "much less", "not heard", "odd study",
                      "of course", "often low", "oglaf.com", "on poetry", "one study", "only once", "only exit", "own films",
                      "peerj.com", "post-rock", "quora.com", "some harm", "still bad", "storks in", "summed to", "the heart",
                      "this link", "An article", "Anno again", "for my dog", "gatotsu911", "gitlab.com", "gitcoin.co",
                      "in Ireland", "in Bitcoin", "karthik bm", "kaggle.com", "is amusing", "libgen.org", "kriegerlie",
                      "logging-in", "medium.com", "much-cited", "more sober", "newegg.com", "flickr.com", "figure 2/3",
                      "fatal flaw", "fandom.com", "face crops", "fMRI study", "exigentsky", "erowid.org", "episode 18",
                      "episode 10", "encourager", "eepurl.com", "early 2012", "due solely", "discord.gg", "digit span",
                      "did arrive", "demo video", "One survey", "Part Three", "backing up", "as of 2014", "as of 2011",
                      "art & wine", "arrested 5", "are making", "are due to", "are banned", "archivenow", "archive.is",
                      "antilop.cc", "anonymized", "annals.org", "an attempt", "amazon.com", "amazon.co.", "a web page",
                      "a historic", "a commuter", "census.gov", "debian.org", "In a study", "If he does", "I found it",
                      "I expected", "I conclude", "ANN thread", "7th report", "4 examples", "23 in 2018", "2020-07-21",
                      "2019-07-05", "2019-06-26", "2016-08-16", "2015-03-26", "2015-03-23", "2014-12-26", "2014-06-09",
                      "2013-12-20", "2013-12-19", "2013-11-26", "2013-09-03", "2013-04-23", "2013-04-18", "2013-03-13",
                      "2013-02-16", "2013-01-04", "2012-11-07", "2012 draft", "2011 paper", "2010 trial", "2008-11-01",
                      "2006 paper", "2005 essay", "2 2.5-inch", "1933 paper", "150 people", "A trainer", "A chapter",
                      "6-7 years", "4chan.org", "4 million", "3\8260\&4s cost", "2.5 years", "2 servers", "1908/1966",
                      "100\8211\&1000\215", "qntm.org", "preprint", "premium\8217", "\12454\12469\12462\12488\12456\12468",
                      "\12402\12425\12426\21380\31070\27096", "9 years", "8 of 11", "53 cats", "50-100k", "5 years",
                      "3 years", "2TB one", "Sturm\8217s", "\37504\27827\12392\24651\33394\39764\27861", "\30495\29983\26410\20998\12398\19968\24515",
                      "\21380\31070\27096\12398\36890\23398\36335", "\12371\12393\12418\12398\12375\12367\12415", "\8383\&39,644",
                      "~20kj/g", "A-series", "900W UPS", "8mm film", "40mg/day", "40 bytes", "2channel", "175 days",
                      "12kb/day", "100 days", "/r/SilkRoad", "1 dead baby", "12-36 hours", "2 terabytes", "2004 SD was",
                      "2007 survey", "2008 handout", "2008 review", "2010 report", "2011 report", "2011 survey", "2013 review",
                      "2019 profile", "2021 profile", "20th century", "400,000 year", "Dell monitor", "Episode One",
                      "Episode Six", "I have been", "Jaeggi says", "One blogger", "One episode", "RSS version", "Steven Kaas",
                      "The Verge\8217s", "The article", "\24189\38597\12395\21682\12363\12379\12289\22696\26579\12398\26716",
                      "\8217perfection", "a blog post", "a few lines", "a reference", "a tradition", "affect mood",
                      "all of them", "arrest of 2", "biomedicine", "black women", "blood sugar", "can lead to", "can\8217t build",
                      "captainjojo", "expel elves", "for aspirin", "for example", "for novelty", "homeostasis", "in an essay",
                      "in my table", "is improved", "latest work", "legal highs", "less secure", "like murder", "looks like",
                      "makes sense", "many things", "media op-ed", "mostly when", "mouse study", "my own page", "my surprise",
                      "neuroticism", "Neuroticism", "not too bad", "of empathy", "off of SR2", "on scaling", "one article", "online demo",
                      "psychopathy", "push-button", "scaling up", "self-esteem", "simple SVG", "small study", "snowy halcy",
                      "source code", "speeding up", "steady fall", "still walk", "such files", "summer jobs", "than before",
                      "that config", "that could", "the Quakers", "the morning", "the seasons", "their graph", "there too!",
                      "to the mean", "told not to", "underspend", "until 2014", "usually not", "very tricky", "video demo",
                      "was minimal", "whitepaper", "winning big", "with itself", "with others", "worked out", "~2014-11-06",
                      "by his", "so many", "Part I", "long way", "Part Four", "part Five", "also with", "how often",
                      "believe it", "points out", "feline form", "first quote", "forum posts", "pretty safe", "nice slides",
                      "first result", "been in jail", "if available", "how critical", "their health", "such as Iran",
                      "site traffic","shallow moon","sees through","second quote","scaling laws","sanded floor", "was searched",
                       "vague musing", "the ambitions", "within a year", "wrote a paper", "my prediction", "more strongly",
                       "more peculiar","more coverage", "just one such","is reportedly", "was skeptical", "dental records",
                       "Lowe commentary", "I have recorded","I had predicted", "did not do much", "a later analysis",
                       "a curious appeal", "awesome as it is", "also interesting", "absolutely right", "link compilation",
                       "landmark address", "less on conflict", "the difference", "and identified", "other benefits",
                       "historical context", "as predicted", "and identified", "Some studies", "well-studied", "well studied",
                       "Colab", "historical survey", "the relationship", "high-dimensional", "unigrams", "health benefits",
                       "first proposed", "the solution", "It turns out", "an interview", "poorly studied", "deep learning",
                       "there is evidence", "fatal accidents", "in animal models", "psychiatric disorders", "summary statistics",
                       "are available", "small fraction", "FDA-approved", "June 2021", "large dataset", "cognitive training",
                       "reported that", "famously argued that", "2016 US presidential election", "GPS", "Blue light",
                       "research paper", "status hierarchies", "arxiv", "probably not", "brain plasticity", "September 2021",
                       "twin studies", "November 2019", "", "<5", "<10", "~16", "<10", "impact-factor", "Donald Trump",
                       "AI", "in China", "several studies", "John F. Kennedy", "interesting thing", "public datasets",
                       "May 2021", "side effects", "visualizations", "not necessarily", "Wikipedia pages", "found the opposite",
                       "matrix multiplication", "Silk Road 2", "Silk Road 2.0", "online", "Online", "side effects",
                       "status", "transcription/translation", "ADHD", "more difficult", "June 2016", "decline with age",
                       "criminal records", "the appendix", "API", "another page", "at least once", "must be", "expected from their",
                       "animal welfare", "psychiatry", "the initial screening", "average-case", "go", "been removed", "mystical experience",
                       "research in general", "been examined", "November 2021", "court records", "in a", "The Guardian", "suggests that",
                       "learn faster", "project page", "lifetime income", "December 2017", "January 2010", "another suggestion", "at all",
                       "how long it takes", "music generation", "LW", "HN", "survey results", "1kg", "~6",
                       "3D", "Rumi", ">>", "<<", "@9", "for example", "@8",
                       "a laptop", "This paper", "this paper", "The results", "The problem", "Co", "field experiment",
                       "@9", "@7", "@10", "body weight", "pdf", "on average", "one month",
                       "for example", "For example", "into English", "meta", "8 years", "better performance", "more samples",
                       "classifier", "I have", "an example", "scales with model size", "Appendix G", "delusional", "Code is available",
                       "de facto", "the problem", "an article", "increased mortality", "young adults", "at night", "raw data",
                       "into the abdomen", "well-known", "Human Intelligence", "Part 3", "autoregressive", "a new era", "vice-versa",
                       "another example", "struggled for years", "a study", "1990s", "more examples", "Appendix B", "trans",
                       "generalizability", "to test", "shut down", "English translation", "can influence", "some evidence", "as a teacher",
                       "a survey", "2000s", "who would", "last year", "1 Second", "he says", "a game",
                       "See also", "a meta-analysis", "we see", "smaller model", "This one", "as necessary", "URLs",
                       "other examples", "S1", "it uses", "The data", "index.html", "October 2021", "original paper",
                       "elderly individuals", "side-effects", "Tables 2", "This interview", "show off", "Why not", "SF",
                       "Mr", "basic idea", "dreams can", "this post", "used it", "just regression", "a collection",
                       "surprising abilities", "as usual", "at least 4", "1980s", "1960s",  "a good thing", "Codex models",
                       "scaled-up", "regression tasks", "~", "the movie", "book reviews", "been abandoned", "Section 5",
                       "inversely correlated", "Spring 2019", "CEOs", "the cellular level", "April 2019", "pregnancy rates", "school-based",
                       "There are many ways", "much more expensive", "world model", "had planned", "weak evidence", "review article", "the cause",
                       "a co-founder", "This technique", "summarization", "recent research", "a second experiment", "that low", "Multivariable",
                       "Data sources", "No benefits", "we have created", "those affected", "followup paper", "top journals", "Organizational Behavior and Human Decision Processes",
                       "DNA", "UK", "one buyer", "emphasis added", "is available", "smallcaps", "policy implications",
                       "ecology & evolution", "evidence of publication bias", "full translation", "bio", "many problems", "I love", "Appendix A",
                       "a lot of things", "August 2002", "a decade later", "January 2022", "blog post", "first calculation", "turns out to be",
                       "October 2022", "publicly available", "the mean", "video calls", "the devil is in the details", "rare mutations", "Caltech",
                       "New York", "financial incentive", "March 2022", "3.5 years", "annual survey", "April 2021", "wikipedia.org",
                       "png", "twitter.com", "arxiv.org", "nytimes.com", "independent replication", "finetuned", "analytics",
                       "rewriting history", "psychiatry", "July 2022", "great results", "especially well", "has announced", "allow an",
                       "learned representation", "in Minecraft", "1920s", "neurobiology", "the research literature", "background information", "web pages",
                       "instructional technology", "understand", "serious threat", "the paper",  "eg", "published version", "II", "original vision", "there are many ways", "movie adaptation", "of data", "an excerpt", "I make", "only so much", "I estimate", "an email interview", "an essay", "highly optimized", "a penalty", "can be improved", "run computers", "still is", "a lot", "a little work"]

-- a whitelist of (URL, [possible anchors]) pairs which would be filtered out normally by the heuristic checks, but are valid anyway. Instances can be found looking at the generated `linkSuggests-deleted.hs` database, or written by hand when I notice useful links not being suggested in the formatting phase of writing annotations.
whiteList :: M.Map T.Text [T.Text]
whiteList = M.fromList $ filter (\(k,_) -> (k /= "") && (T.head k == '/' || isURI (T.unpack k))) [
  ( "/crop#hands"
    , [ "PALM"
      , "PALM ('PALM Anime Locator Model') is a dataset of k=5,382 anime-style Danbooru2019 images annotated with the locations of _n_ = 14,394 hands, a YOLOv3 model trained using those annotations to detect hands in anime-style images, and a second dataset of _n_ = 96,534 hands cropped from the Danbooru2019 dataset using the PALM YOLO model and _n_ = 58,536 of them upscaled to \8805\&512px"
      , "PALM ('PALM Anime Locator Model') is a dataset of k=5,382 anime-style Danbooru2019 images annotated with the locations of n=14,394 hands, a YOLOv3 model trained using those annotations to detect hands in anime-style images, and a second dataset of n=96,534 hands cropped from the Danbooru2019 dataset using the PALM YOLO model and n=58,536 of them upscaled to \8805\&512px"
      , "PALM: The PALM Anime Location Model And Dataset"
      ]
    )
  , ("https://en.wikipedia.org/wiki/Sam_Altman", ["Sam Altman"])
  , ("https://en.wikipedia.org/wiki/Satya_Nadella", ["Satya Nadella"])
  , ("https://en.wikipedia.org/wiki/Replication_crisis", ["failed to replicate", "failure to replicate"])
  , ("https://en.wikipedia.org/wiki/Stochastic_gradient_descent#Adam", ["Adam"])
  , ("https://en.wikipedia.org/wiki/Big_data", ["big data"])
  , ("https://en.wikipedia.org/wiki/The_New_York_Times", ["NYT", "New York Times", "The New York Times"])
  , ("https://en.wikipedia.org/wiki/Jeff_Dean", ["Jeff Dean"])
  , ("https://arxiv.org/abs/2204.02311#google", ["PaLM"])
  , ("https://en.wikipedia.org/wiki/Medical_school", ["medical school"])
  , ("https://en.wikipedia.org/wiki/Reinforcement_learning", ["RL", "reinforcement learning"])
  , ("https://en.wikipedia.org/wiki/Derek_Lowe_(chemist)", ["Derek Lowe"])
  , ("https://en.wikipedia.org/wiki/California_Institute_of_Technology", ["Caltech"])
  , ("https://www.teds.ac.uk/about-teds", ["TEDS", "Twins Early Development Study"])
  , ("/doc/iq/2018-lee.pdf", ["Lee et al 2018"])
  , ("https://en.wikipedia.org/wiki/Reliability_(statistics)", ["reliability", "reliable"])
  , ("https://en.wikipedia.org/wiki/Functional_magnetic_resonance_imaging", ["fMRI", "functional magnetic resonance imaging"])
  , ("regret", ["https://en.wikipedia.org/wiki/Regret_(decision_theory)"])
  , ("https://en.wikipedia.org/wiki/Placebo", ["placebo effect", "placebo"])
  , ("https://en.wikipedia.org/wiki/Nocebo", ["nocebo effect", "nocebo"])
  , ("https://en.wikipedia.org/wiki/LinkedIn", ["LinkedIn"])
  , ("https://en.wikipedia.org/wiki/Open_source", ["open source", "open-source"])
  , ("https://arxiv.org/abs/1910.10683#google", ["T5", "T5 models"])
  , ("https://en.wikipedia.org/wiki/Gene%E2%80%93environment_interaction", ["gene-environment interaction", "gene–environment interaction", "GxE", "G×E"])
  , ("https://en.wikipedia.org/wiki/Autoregressive_model", ["autoregressive","autoregressive model", "autoregression"])
  , ("https://en.wikipedia.org/wiki/Benjamin_Franklin", ["Benjamin Franklin", "Ben Franklin"])
  , ("https://derpibooru.org/", ["Derpibooru"])
  , ("https://en.wikipedia.org/wiki/Mother_Jones_(magazine)", ["Mother Jones", "Mother Jones magazine"])
  , ("https://en.wikipedia.org/wiki/European_Union", ["EU", "European Union"] )
  , ( "/gpt-3#prompts-as-programming"
    , ["prompt programming", "prompt engineering" ]
    )
  , ("/twdne", ["TWDNE"])
  , ( "/turing-complete#security-implications"
    , [ "weird machines" ]
    )
    , ( "/doc/ai/scaling/2013-yudkowsky.pdf#miri"
    , [ "Intelligence Explosion Microeconomics" , "Yudkowsky 2013" ]
    )
  , ( "/doc/ai/nn/diffusion/2018-sharma.pdf#google" , [ "Conceptual Captions" ] )
  , ( "/doc/dual-n-back/2010-zhang.pdf"
    , [ "Chinese journal finds 31% of submissions plagiarized', Zhang 2010"])
      , ( "/doc/anime/eva/2002-takeda-notenkimemoirs#opening-the-general-products-store"
    , [ "General Products" ]
    )
  , ( "/doc/anime/eva/2002-takeda-notenkimemoirs#the-daicon-3-decision"
    , [ "DAICON III" ]
    )
  , ( "/doc/genetics/heritable/correlation/2014-mosing.pdf"
    , [ "Practice Does Not Make Perfect: No Causal Effect of Music Practice on Music Ability"
      ]
    )
  , ( "/doc/genetics/heritable/correlation/2015-krapohl.pdf"
    , [ "Krapohl et al 2015"
      , "Phenome-wide analysis of genome-wide polygenic scores"
      ]
    )
  , ( "/doc/genetics/heritable/correlation/2015-pettersson.pdf"
    , [ "Common psychiatric disorders [and violent crime] share the same genetic origin: a multivariate sibling study of the Swedish population"
      ]
    )
  , ( "/doc/genetics/heritable/correlation/2015-zhu.pdf"
    , [ "Educational attainment-related loci identified by GWAS are associated with select personality traits and mathematics and language abilities"
      , "Zhu et al 2015"
      ]
    )
  , ( "/doc/genetics/heritable/1987-plomin.pdf"
    , [ "Why are children in the same family so different from one another"
      ]
    )
  , ( "/doc/genetics/heritable/2014-pellegrino.pdf"
    , [ "A Novel BHLHE41 Variant is Associated with Short Sleep and Resistance to Sleep Deprivation in Humans"
      , "Pellegrino et al 2014"
      ]
    )
  , ( "/doc/genetics/selection/artificial/1933-student.pdf"
    , ["Evolution By Selection: The Implications of Winter’s Selection Experiment [in <em>Student's Collected Papers</em>]"
      , "Student 1933"
      ]
    )
  , ( "/doc/iq/2014-shulman.pdf"
    , [ "Embryo Selection for Cognitive Enhancement: Curiosity or Game-changer"
      , "Shulman & Bostrom 2014"
      ]
    )
  , ( "/doc/genetics/heritable/correlation/2015-zhu.pdf"
    , [ "Educational attainment-related loci identified by GWAS are associated with select personality traits and mathematics and language abilities"
      , "Zhu et al 2015"
      ]
    )
  , ( "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4883595/"
    , [ "Genome-wide association study identifies 74 [162] loci associated with educational attainment"
      , "Genome-wide association study identifies 74 loci associated with educational attainment"
      , "Okbay et al 2016"
      ]
    )
  , ( "/doc/psychiatry/lithium/2015-helbich.pdf"
    , [ "Helbich et al 2015"
      , "Lithium in drinking water and suicide mortality: interplay with lithium prescriptions"
      ]
    )
      , ( "/doc/nicotine/2009-lecacheux.pdf"
    , [ "Cognitive modifications associated with tobacco smoking [review]', Lecacheux et al 2009"
      ]
    )
  , ( "/doc/reinforcement-learning/model-free/1992-williams.pdf"
    , [ "REINFORCE" ]
    )
  , ( "/doc/reinforcement-learning/model-free/2016-graves.pdf#deepmind"
    , [ "Hybrid computing using a neural network with dynamic external memory"
      ]
    )
  , ( "/doc/reinforcement-learning/model/alphago/2017-silver.pdf#page=3&org=deepmind"
    , [ "AlphaGo Zero"
      , "Mastering the game of Go without human knowledge', Silver et al 2017"
      ]
    )
  , ( "/doc/reinforcement-learning/model/alphago/2017-silver.pdf#page=3&org=deepmind"
    , [ "AlphaGo Zero"
      , "Mastering the game of Go without human knowledge', Silver et al 2017"
      ]
    )
  , ( "/doc/reinforcement-learning/meta-learning/2018-wang.pdf#deepmind"
    , [ "Prefrontal cortex as a meta-reinforcement learning system"
      , "Wang et al 2018"
      ]
    )
  , ( "/doc/sociology/1987-rossi.pdf"
    , [ "The Iron Law Of Evaluation And Other Metallic Rules', Rossi 1987"      ]
    )
  , ( "/doc/sociology/1993-boehm.pdf"
    , [ "Egalitarian Behavior and Reverse Dominance Hierarchy [and Comments and Reply]', Boehm et al 1993"
      ]
    )
  , ( "/doc/sociology/2003-murray-humanaccomplishment.pdf"
    , [ "Human Accomplishment" ]
    )
  , ( "/doc/statistics/bayes/1994-falk.pdf"
    , [ "The Ups and Downs of the Hope Function In a Fruitless Search', Falk et al 1994"      ]
    )
  , ( "/doc/statistics/bayes/2009-kaas.html"
    , [ "A New Challenge to 98% Confidence"]
    )
  , ( "/doc/wikipedia/2018-teblunthuis.pdf"
    , [ "Revisiting The Rise and Decline in a Population of Peer Production Projects [769 wikis"
      ]
    )
      , ( "https://ero.sagepub.com/content/1/3/2332858415599972?full"
    , [ "Domingue et al 2015"
      , "Polygenic Influence on Educational Attainment"
      ]
    )
  , ( "http://garote.bdmonkeys.net/commandline/"
    , [ "In the Beginning was the Command Line" ]
    )
  , ( "https://hpmor.com/"
    , [ "Harry Potter and the Methods of Rationality" ]
    )
  , ( "http://leipper.org/manuals/zip-fill/safelocks_for_compscientist.pdf"
    , [ "Safecracking for the computer scientist" ]
    )
  , ( "https://mason.gmu.edu/~rhanson/extraord.pdf"
    , [ "When Do Extraordinary Claims Give Extraordinary Evidence" ]
    )
  , ( "http://pediatrics.aappublications.org/content/122/2/e472.short"
    , [ "Effect of Supplementing Pregnant and Lactating Mothers With n-3 Very-Long-Chain Fatty Acids on Children's IQ and Body Mass Index at 7 Years of Age"
      ]
    )
  , ( "https://people.csail.mit.edu/pkrafft/papers/krafft-thesis-final.pdf"
    , [ "A Rational Choice Framework for Collective Behavior" ]
    )
  , ( "http://people.seas.harvard.edu/~salil/research/timelock.pdf"
    , [ "Time-Lock Puzzles in the Random Oracle Model" ]
    )
  , ( "http://perfdynamics.blogspot.com/2013/09/laplace-bayesianista-and-mass-of-saturn.html"
    , [ "Laplace the Bayesianista and the Mass of Saturn" ]
    )
  , ( "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3270388/"
    , [ "The time resolution of the St Petersburg paradox" ]
    )
  , ( "https://www.avmf.org/clientuploads/documents/News%20Articles/Cat%20Health%20Network%20Feline%20SNP%20Chip%20Studies%20-%20Final%20Accomplishments%20MAFFINAL%2005-23-13.pdf"
    , [ "Genome-Wide Association Study for Catnip Response in Domestic Cats', Lyons 2013"
      , "catnip GWAS"
      ]
    )
  , ( "https://papers.nips.cc/paper/2012/file/c399862d3b9d6b76c8436e924a68c45b-Paper.pdf"
    , [ "AlexNet"
      , "ImageNet Classification with Deep Convolutional Neural Networks', Krizhevsky et al 2012"
      ]
    )
  , ( "https://www.fhi.ox.ac.uk/reports/2008-3.pdf"
    , [ "Sandberg & Bostrom 2008" , "Whole Brain Emulation Roadmap" ]
    )
  , ( "https://www.lightspeedmagazine.com/fiction/exhalation/"
    , [ "Exhalation" ]
    )
  , ( "/doc/psychology/writing/2009-sio.pdf"
    , [ "Does incubation enhance problem-solving? A meta-analytic review"
      , "Sio & Ormerod 2009"
      ]
    )
  , ( "https://ai.googleblog.com/2020/01/reformer-efficient-transformer.html"
    , [ "Reformer: The Efficient Transformer"]
    )
  , ( "https://arbtt.nomeata.de/" , [ "arbtt" ] )
    , ( "https://archive.org/details/in.ernet.dli.2015.207427"
    , [ "Animal Breeding Plans" ]
    )
    , ( "https://arxiv.org/abs/1801.01290" , [ "SAC" ] )
  , ( "https://arxiv.org/abs/1807.03819#googledeepmind"
    , [ "Dehghani et al 2018" , "Universal Transformers" ]
    )
  , ( "https://arxiv.org/abs/1707.02968#google"
    , [ "Revisiting Unreasonable Effectiveness of Data in Deep Learning Era"
      , "Sun et al 2017"
      ]
    )
  , ( "https://arxiv.org/abs/1909.08593#openai"
    , [ "Fine-Tuning Language Models from Human Preferences"
      , "Ziegler et al 2019"
      ]
    )
  , ( "https://arxiv.org/abs/1910.13038#google"
    , [ "Learning to Predict Without Looking Ahead: World Models Without Forward Prediction', Freeman et al 2019"
      ]
    )
  , ( "https://arxiv.org/abs/2007.02382"
    , [ "Decentralized Reinforcement Learning: Global Decision-Making via Local Economic Transactions"
      ]
    )
  , ( "https://casual-effects.com/markdeep/features.md.html#basicformatting/admonitions"
    , [ "admonitions" ]
    )
  , ( "https://danbooru.donmai.us/" , [ "Danbooru" ] )
  , ( "https://danluu.com/p95-skill/"
    , [ "95%-ile isn\8217t that good" ]
    )
  , ( "https://davidepstein.com/david-epstein-the-sports-gene/"
    , [ "The Sports Gene" ]
    )
  , ( "https://davidhume.org/texts/e/10" , [ "Of Miracles" ] )
  , ( "https://www.deepmind.com/blog/agent57-outperforming-the-human-atari-benchmark"
    , [ "Agent57"
      , "Agent57: Outperforming the Atari Human Benchmark', Badia et al 2020"
      ]
    )
  , ( "https://www.deepmind.com/blog/learning-through-human-feedback"
    , ["Learning through human feedback" ]
    )
  , ( "https://www.deepmind.com/blog/learning-through-human-feedback"
    , [ "Learning through human feedback" ]
    )
  , ( "https://www.deepmind.com/blog/prefrontal-cortex-meta-reinforcement-learning-system"
    , [
      "Prefrontal cortex as a meta-reinforcement learning system"
      ]
    )
  , ( "https://www.deepmind.com/blog/wavenet-a-generative-model-for-raw-audio"
    , [ "WaveNet: A Generative Model for Raw Audio" ]
    )
  , ( "https://edwardtufte.github.io/tufte-css/"
    , [ "Tufte CSS" , "Tufte-CSS" ]
    )
  , ( "https://elifesciences.org/articles/45183"
    , [ "Meta-Research: A comprehensive review of randomized clinical trials in three medical journals reveals 396 [13%] medical reversals"
      ]
    )
  , ( "https://en.wikipedia.org/wiki/A%2FB_tests" , [ "A/B tests" ] )
  , ( "https://en.wikipedia.org/wiki/A.E._Housman"
    , [ "A.E. Housman" ]
    )
  , ( "https://en.wikipedia.org/wiki/Attention_deficit_hyperactivity_disorder" , [ "ADHD", "Attention deficit hyperactivity disorder" ] )
  , ( "https://en.wikipedia.org/wiki/AI_Dungeon" , [ "AI Dungeon" ] )
  , ( "https://en.wikipedia.org/wiki/ALGOL_60" , [ "ALGOL" ] )
  , ( "https://en.wikipedia.org/wiki/Acrostic"
    , [ "acrostic" , "acrostics" ]
    )
  , ( "https://en.wikipedia.org/wiki/Adderall" , [ "Adderall" ] )
  , ( "https://en.wikipedia.org/wiki/AMD"
    , [ "AMD" ]
    )
  , ( "https://en.wikipedia.org/wiki/Agrippina_(opera)"
    , [ "Agrippina" ]
    )
  , ( "https://en.wikipedia.org/wiki/Albert_Einstein"
    , [ "Albert Einstein" , "Einstein" ]
    )
  , ( "https://en.wikipedia.org/wiki/AlexNet" , [ "AlexNet" ] )
  , ( "https://en.wikipedia.org/wiki/Alkaloids" , [ "alkaloids" ] )
  , ( "https://en.wikipedia.org/wiki/Amber" , [ "amber" ] )
  , ( "https://en.wikipedia.org/wiki/Amphetamine"
    , [ "amphetamine" ]
    )
  , ( "https://en.wikipedia.org/wiki/Substituted_amphetamine"
    , [ "amphetamines" ]
    )
  , ( "https://en.wikipedia.org/wiki/Anchoring_(cognitive_bias)"
    , [ "Anchor"
      , "Anchored"
      , "Anchoring"
      , "anchor"
      , "anchored"
      , "anchoring"
      ]
    )
  , ( "https://en.wikipedia.org/wiki/Andrew_Gelman"
    , [ "Andrew Gelman" ]
    )
  , ( "https://en.wikipedia.org/wiki/Apollo_11" , [ "Apollo 11" ] )
  , ( "https://en.wikipedia.org/wiki/Archive_Team"
    , [ "Archive Team" ]
    )
  , ( "https://en.wikipedia.org/wiki/Arduino" , [ "Arduino" ] )
  , ( "https://en.wikipedia.org/wiki/Ars_Technica"
    , [ "Ars Technica" ]
    )
  , ( "https://en.wikipedia.org/wiki/Assassination_markets"
    , [ "assassination markets", "assassination market" ]
    )
  , ( "https://en.wikipedia.org/wiki/Augur_%28software%29"
    , [ "Augur" ]
    )
  , ( "https://en.wikipedia.org/wiki/Bash_%28Unix_shell%29"
    , [ "Bash" ]
    )
  , ( "https://en.wikipedia.org/wiki/Batch_normalization"
    , [ "batchnorm" ]
    )
  , ( "https://en.wikipedia.org/wiki/Blade_Runner"
    , [ "Blade Runner" ]
    )
  , ( "https://en.wikipedia.org/wiki/Blocking_%28statistics%29"
    , [ "Blocking" , "blocking" , "blocks" ]
    )
  , ( "https://en.wikipedia.org/wiki/Breaking_Bad"
    , [ "Breaking Bad" ]
    )
  , ( "https://en.wikipedia.org/wiki/Byte" , [ "byte" ] )
  , ( "https://en.wikipedia.org/wiki/C.S._Lewis" , [ "C.S. Lewis" ] )
  , ( "https://en.wikipedia.org/wiki/CURL" , [ "cURL" , "curl" ] )
  , ( "https://en.wikipedia.org/wiki/Caffeine"
    , [ "Caffeine" , "caffeinated" , "caffeine" ]
    )
  , ( "https://en.wikipedia.org/wiki/Calibration_%28statistics%29"
    , [ "calibrated" ]
    )
  , ( "https://en.wikipedia.org/wiki/Carl_Friedrich_Gauss"
    , [ "Gauss" ]
    )
  , ( "https://en.wikipedia.org/wiki/Censoring_(statistics)"
    , [ "censoring" ]
    )
  , ( "https://en.wikipedia.org/wiki/Charles_Darwin"
    , [ "Charles Darwin" ]
    )
  , ( "https://en.wikipedia.org/wiki/Choose_Your_Own_Adventure"
    , [ "CYOA" , "Choose-Your-Own-Adventure" ]
    )
  , ( "https://en.wikipedia.org/wiki/Cicero" , [ "Cicero" ] )
  , ( "https://en.wikipedia.org/wiki/Claude_Shannon"
    , [ "Claude Shannon" ]
    )
  , ( "https://en.wikipedia.org/wiki/Clever_Hans"
    , [ "Clever Hans" ]
    )
  , ( "https://en.wikipedia.org/wiki/Confounding"
    , [ "Confounding" , "confounding" ]
    )

  , ( "https://en.wikipedia.org/wiki/Conscientiousness#Personality_models"
    , [ "Conscientiousness" , "conscientiousness", "Conscientious", "conscientious" ]
    )
  , ( "https://en.wikipedia.org/wiki/Cowpox"
    , [ "Cowpox" , "cowpox" ]
    )
  , ( "https://en.wikipedia.org/wiki/Cybernetics"
    , [ "cybernetics" ]
    )
  , ( "https://en.wikipedia.org/wiki/Decision_theory"
    , [ "decision theory" ]
    )
  , ( "https://en.wikipedia.org/wiki/Dexter_and_sinister"
    , [ "dexter" , "sinister" ]
    )
  , ( "https://en.wikipedia.org/wiki/Dose-response_relationship"
    , [ "dose-response relationship" ]
    )
  , ( "https://en.wikipedia.org/wiki/Dropout_%28neural_networks%29"
    , [ "dropout" ]
    )
  , ( "https://en.wikipedia.org/wiki/Duplicity_%28software%29"
    , [ "duplicity" ]
    )
  , ( "https://en.wikipedia.org/wiki/Earwax" , [ "earwax" ] )
  , ( "https://en.wikipedia.org/wiki/Eclipse_%28software%29"
    , [ "Eclipse" ]
    )
  , ( "https://en.wikipedia.org/wiki/Effect_size"
    , [ "effect size" ]
    )
  , ( "https://en.wikipedia.org/wiki/Emacs" , [ "Emacs" ] )
  , ( "https://en.wikipedia.org/wiki/English_Wikipedia"
    , [ "English Wikipedia" ]
    )
  , ( "https://en.wikipedia.org/wiki/Entropy_(information_theory)"
    , [ "Entropy" , "entropy" ]
    )
  , ( "https://en.wikipedia.org/wiki/Epilepsy"
    , [ "Epilepsy" , "epilepsy" ]
    )
  , ( "https://en.wikipedia.org/wiki/Escrow" , [ "escrow" ] )
  , ( "https://en.wikipedia.org/wiki/Experience_curve_effects"
    , [ "experience curve" ]
    )
  , ( "https://en.wikipedia.org/wiki/Extraversion_and_introversion"
    , [ "Extraversion" , "Introversion" ]
    )
  , ( "https://en.wikipedia.org/wiki/FLOSS" , [ "FLOSS" ] )
  , ( "https://en.wikipedia.org/wiki/Fixation_%28population_genetics%29"
    , [ "Fixation" , "fixation" , "reach fixation" ]
    )
  , ( "https://en.wikipedia.org/wiki/Flickr" , [ "Flickr" ] )
  , ( "https://en.wikipedia.org/wiki/GCTA" , [ "GCTA" ] )
  , ( "https://en.wikipedia.org/wiki/GNOME" , [ "GNOME" , "Gnome" ] )
  , ( "https://en.wikipedia.org/wiki/Gala_%28apple%29" , [ "Gala" ] )
  , ( "https://en.wikipedia.org/wiki/Genetic_recombination"
    , [ "Recombination" , "recombination" ]
    )
  , ( "https://en.wikipedia.org/wiki/Giacomo_Puccini"
    , [ "Puccini" ]
    )
  , ( "https://en.wikipedia.org/wiki/Gian-Carlo_Rota"
    , [ "Gian-Carlo Rota" ]
    )
  , ( "https://en.wikipedia.org/wiki/Github"
    , [ "GitHub" , "Github" ]
    )
  , ( "https://en.wikipedia.org/wiki/Global_Burden_of_Disease"
    , [ "Global Burden of Disease" ]
    )
  , ( "https://en.wikipedia.org/wiki/Global_Burden_of_Disease_Study"
    , [ "Global Burden of Disease" ]
    )
  , ( "https://en.wikipedia.org/wiki/Gompertz_function"
    , [ "Gompertz curve" ]
    )
  , ( "https://en.wikipedia.org/wiki/Google_Alerts"
    , [ "Google Alerts" ]
    )
  , ( "https://en.wikipedia.org/wiki/Hal_Finney_%28cypherpunk%29"
    , [ "Hal Finney" ]
    )
  , ( "https://en.wikipedia.org/wiki/Hangul"
    , [ "Hangul" , "hangul" ]
    )
  , ( "https://en.wikipedia.org/wiki/Hans_Eysenck" , [ "Eysenck" ] )
  , ( "https://en.wikipedia.org/wiki/Haskell"
    , [ "Haskell" ]
    )
  , ( "https://en.wikipedia.org/wiki/Hells_Angels_(manga)"
    , [ "HELLS" , "Hells" ]
    )
  , ( "https://en.wikipedia.org/wiki/Henry_Darger"
    , [ "Henry Darger" ]
    )
  , ( "https://en.wikipedia.org/wiki/His_and_Her_Circumstances"
    , [ "His and Her Circumstances" ]
    )
  , ( "https://en.wikipedia.org/wiki/Hubris" , [ "hubris" ] )
  , ( "https://en.wikipedia.org/wiki/Human_Accomplishment"
    , [ "Human Accomplishment" ]
    )
  , ( "https://en.wikipedia.org/wiki/Hutter_Prize"
    , [ "Hutter Prize" ]
    )
  , ( "https://en.wikipedia.org/wiki/IQ" , [ "IQ" ] )
  , ( "https://en.wikipedia.org/wiki/Iliad" , [ "Iliad" ] )
  , ( "https://en.wikipedia.org/wiki/Incompatible_Timesharing_System"
    , [ "ITS" ]
    )
  , ( "https://en.wikipedia.org/wiki/Infanticide"
    , [ "infanticide" ]
    )
  , ( "https://en.wikipedia.org/wiki/Isaac_Asimov"
    , [ "Isaac Asimov" ]
    )
  , ( "https://en.wikipedia.org/wiki/J._B._S._Haldane"
    , [ "J.B.S. Haldane" ]
    )
  , ( "https://en.wikipedia.org/wiki/J.K._Rowling"
    , [ "J.K. Rowling" ]
    )
  , ( "https://en.wikipedia.org/wiki/John_L._Fuller" , [ "Fuller" ] )
  , ( "https://en.wikipedia.org/wiki/Joseph_Louis_Lagrange"
    , [ "Lagrange" ]
    )
  , ( "https://en.wikipedia.org/wiki/Just_another_Gibbs_sampler"
    , [ "JAGS" ]
    )
  , ( "https://en.wikipedia.org/wiki/Kantai_Collection"
    , [ "Kancolle" , "Kantai Collection" ]
    )
  , ( "https://en.wikipedia.org/wiki/Karl_Pearson" , [ "Pearson" ] )
  , ( "https://en.wikipedia.org/wiki/Ketamine"
    , [  "ketamine" ]
    )
  , ( "https://en.wikipedia.org/wiki/Kevin_Kelly_(editor)"
    , [ "Kevin Kelly" ]
    )
  , ( "https://en.wikipedia.org/wiki/King_Records_%28Japan%29"
    , [ "King Records" ]
    )
  , ( "https://en.wikipedia.org/wiki/LaTeX" , [ "LaTeX" ] )
  , ( "https://en.wikipedia.org/wiki/Laplace%27s_rule_of_succession"
    , [ "Laplace\8217s rule of succession" ]
    )
  , ( "https://en.wikipedia.org/wiki/Lasso_%28statistics%29"
    , [ "lasso" ]
    )
  , ( "https://en.wikipedia.org/wiki/Liability_threshold"
    , [ "liability threshold" ]
    )
  , ( "https://en.wikipedia.org/wiki/Linux" , [ "Linux" ] )
  , ( "https://en.wikipedia.org/wiki/Lisp_(programming_language)"
    , [ "LISP" , "Lisp" ]
    )
  , ( "https://en.wikipedia.org/wiki/Lisp_machines"
    , [ "Lisp machines", "Lisp machine" ]
    )
  , ( "https://en.wikipedia.org/wiki/Lithium"
    , [ "Lithium" , "lithium" ]
    )
  , ( "https://en.wikipedia.org/wiki/Littlewood%27s_Law"
    , [ "Littlewood\8217s Law" ]
    )
  , ( "https://en.wikipedia.org/wiki/Love_%26_Pop"
    , [ "Love & Pop" ]
    )
  , ( "https://en.wikipedia.org/wiki/Lucid_dreaming"
    , [ "lucid dreaming" ]
    )
  , ( "https://en.wikipedia.org/wiki/Made_in_Abyss"
    , [ "Made in Abyss" ]
    )
  , ( "https://en.wikipedia.org/wiki/Manifold_%28magazine%29"
    , [ "Manifold" ]
    )
  , ( "https://en.wikipedia.org/wiki/Medici_bank"
    , [ "Medici bank" ]
    )
  , ( "https://en.wikipedia.org/wiki/Melatonin"
    , [ "Melatonin" , "melatonin" ]
    )
  , ( "https://en.wikipedia.org/wiki/Mescaline" , [ "mescaline" ] )
  , ( "https://en.wikipedia.org/wiki/Mnemosyne_%28software%29"
    , [ "Mnemosyne" ]
    )
  , ( "https://en.wikipedia.org/wiki/Modafinil"
    , [ "Modafinil" ,  "modafinil" ]
    )
  , ( "https://en.wikipedia.org/wiki/Modus_ponens"
    , [ "modus ponens" ]
    )
  , ( "https://en.wikipedia.org/wiki/Modus_tollens"
    , [ "modus tollens" ]
    )
  , ( "https://en.wikipedia.org/wiki/Moe_%28slang%29" , [ "moe" ] )
  , ( "https://en.wikipedia.org/wiki/Mortar_%28weapon%29"
    , [ "mortar" , "mortars" ]
    )
  , ( "https://en.wikipedia.org/wiki/Mosaic_%28web_browser%29"
    , [ "Mosaic" ]
    )
  , ( "https://en.wikipedia.org/wiki/Multi-level_model"
    , [ "multi-level model" ]
    )
  , ( "https://en.wikipedia.org/wiki/Multiple_comparisons_problem"
    , [ "multiple comparisons" ]
    )
  , ( "https://en.wikipedia.org/wiki/My_Little_Pony:_Friendship_Is_Magic"
    , [ "MLP:FiM" , "My Little Pony: Friendship is Magic" ]
    )
  , ( "https://en.wikipedia.org/wiki/NP-hard" , [ "NP-hard" ] )
  , ( "https://en.wikipedia.org/wiki/Narcissism"
    , [ "Narcissism" , "Narcissistic" ]
    )
  , ( "https://en.wikipedia.org/wiki/NeWS" , [ "NeWS" ] )
  , ( "https://en.wikipedia.org/wiki/Neon_Genesis_Evangelion_%28TV%29"
    , [ "Evangelion" , "Neon Genesis Evangelion" ]
    )
  , ( "https://en.wikipedia.org/wiki/Netflix" , [ "Netflix" ] )
  , ( "https://en.wikipedia.org/wiki/Neuroticism"
    , [ "Neuroticism", "neuroticism", "neurotic", "Neurotic", "emotional stability", "Emotional Stability" ]
    )
  , ("https://en.wikipedia.org/wiki/Agreeableness", ["agreeableness", "Agreeableness", "disagreeableness", "Disagreeableness"])
  , ("https://en.wikipedia.org/wiki/Circadian_rhythm", ["circadian rhythm", "circadian rhythms", "circadian cycle", "circadian cycles", "circadian"])
  , ( "https://en.wikipedia.org/wiki/Niccol%C3%B2_Machiavelli"
    , [ "Machiavelli" ]
    )
  , ( "https://en.wikipedia.org/wiki/Nicotine" , [ "nicotine" ] )
  , ( "https://en.wikipedia.org/wiki/Nim_Chimpsky"
    , [ "Project Nim" ]
    )
  , ( "https://en.wikipedia.org/wiki/Of_Miracles"
    , [ "Of Miracles" ]
    )
  , ( "https://en.wikipedia.org/wiki/Order_statistic"
    , [ "order statistics" ]
    )
  , ( "https://en.wikipedia.org/wiki/Paris_Review"
    , [ "Paris Review" ]
    )
  , ( "https://en.wikipedia.org/wiki/Parthenogenesis"
    , [ "parthenogenesis" ]
    )
  , ( "https://en.wikipedia.org/wiki/Pascal_%28programming_language%29"
    , [ "Pascal" ]
    )
  , ( "https://en.wikipedia.org/wiki/Patreon" , [ "Patreon" ] )
  , ( "https://en.wikipedia.org/wiki/PayPal"
    , [ "PayPal" , "Paypal" ]
    )
  , ( "https://en.wikipedia.org/wiki/Pedigree_chart"
    , [ "pedigree" , "pedigrees" ]
    )
  , ( "https://en.wikipedia.org/wiki/Petard" , [ "petard" ] )
  , ( "https://en.wikipedia.org/wiki/Polyamory" , [ "polyamory" ] )
  , ( "https://en.wikipedia.org/wiki/Polygenic_score"
    , [ "polygenic scores" ]
    )
  , ( "https://en.wikipedia.org/wiki/PostScript" , [ "PostScript" ] )
  , ( "https://en.wikipedia.org/wiki/Practice_(learning_method)#Deliberate_practice"
    , [ "deliberate practice" ]
    )
  , ( "https://en.wikipedia.org/wiki/Prediction_market"
    , [ "prediction market", "prediction markets"  ]
    )
  , ( "https://en.wikipedia.org/wiki/Priming_(psychology)"
    , [ "Priming" , "priming" ]
    )
  , ( "https://en.wikipedia.org/wiki/Principal-agent_problem"
    , [ "principal-agent problem" ]
    )
  , ( "https://en.wikipedia.org/wiki/Pugs" , [ "Pugs" ] )
  , ( "https://en.wikipedia.org/wiki/Pumping_Iron"
    , [ "Pumping Iron" ]
    )
  , ( "https://en.wikipedia.org/wiki/Quantified_Self"
    , [ "Quantified Self" ]
    )
  , ( "https://en.wikipedia.org/wiki/R.A._Fisher"
    , [ "R. A. Fisher" , "R.A. Fisher" ]
    )
  , ( "https://en.wikipedia.org/wiki/ROUGE_(metric)" , [ "ROUGE" ] )
  , ( "https://en.wikipedia.org/wiki/Radium"
    , [ "Radium" , "radium" ]
    )
  , ( "https://en.wikipedia.org/wiki/Random_forests"
    , [ "random forests", "random forest" ]
    )
  , ( "https://en.wikipedia.org/wiki/Rebus" , [ "rebus" ] )
  , ( "https://en.wikipedia.org/wiki/Reddit" , [ "Reddit" ] )
  , ( "https://en.wikipedia.org/wiki/Redshift_%28software%29"
    , [ "Redshift" ]
    )
  , ( "https://en.wikipedia.org/wiki/replication_Crisis"
    , [ "Replication Crisis" ]
    )
  , ( "https://en.wikipedia.org/wiki/Reproducibility"
    , [ "replicate" , "replicated" ]
    )
  , ( "https://en.wikipedia.org/wiki/Richard_Hamming"
    , [ "Hamming" , "Richard Hamming" ]
    )
      , ( "https://en.wikipedia.org/wiki/Robert_Bakewell_%28agriculturalist%29"
    , [ "Bakewell" , "Robert Bakewell" ]
    )
  , ( "https://en.wikipedia.org/wiki/Rotten.com" , [ "Rotten.com" ] )
  , ( "https://en.wikipedia.org/wiki/SQL" , [ "SQL" ] )
  , ( "https://en.wikipedia.org/wiki/Sadistic_personality_disorder"
    , [ "sadism" , "sadistic" ]
    )
  , ( "https://en.wikipedia.org/wiki/Samsung" , [ "Samsung" ] )
  , ( "https://en.wikipedia.org/wiki/Samuel_Johnson"
    , [ "Samuel Johnson" ]
    )
  , ( "https://en.wikipedia.org/wiki/Schizophrenia"
    , [ "Schizophrenia"
      , "schizophrenia"
      , "schizophrenic"
      , "schizophrenics"
      ]
    )
  , ( "https://en.wikipedia.org/wiki/Scott_Sumner"
    , [ "Scott Sumner" ]
    )
  , ( "https://en.wikipedia.org/wiki/Seamless_%28company%29"
    , [ "Seamless" ]
    )
  , ( "https://en.wikipedia.org/wiki/Siegfried_(opera)"
    , [ "Siegfried" ]
    )
  , ( "https://en.wikipedia.org/wiki/Silk_Road_%28anonymous_marketplace%29"
    , [ "Silk Road"  ]
    )
  , ( "https://en.wikipedia.org/wiki/Social_status"
    , [  "social status" ]
    )
  , ( "/spaced-repetition"
    , [ "spaced repetition", "Mnemosyne", "SRS" ]
    )
  , ( "https://en.wikipedia.org/wiki/Stewart_Brand"
    , [ "Stewart Brand" ]
    )
  , ( "https://en.wikipedia.org/wiki/Stripe_%28company%29"
    , [ "Stripe" ]
    )
  , ( "https://en.wikipedia.org/wiki/Study_heterogeneity"
    , [ "heterogeneity" ]
    )
  , ( "https://en.wikipedia.org/wiki/Study_of_Mathematically_Precocious_Youth"
    , [ "SMPY" , "Study of Mathematically Precocious Youth" ]
    )
  , ( "https://en.wikipedia.org/wiki/Sunk_cost_fallacy"
    , [ "sunk cost fallacy" ]
    )
  , ( "https://en.wikipedia.org/wiki/Superrationality"
    , [ "superrationality" ]
    )
  , ( "https://en.wikipedia.org/wiki/Syntax_highlighting"
    , [ "syntax highlighting" ]
    )
  , ( "https://en.wikipedia.org/wiki/Taliesin" , [ "Taliesin" ] )
  , ( "https://en.wikipedia.org/wiki/The_Better_Angels_of_Our_Nature"
    , [ "The Better Angels of Our Nature" ]
    )
  , ( "https://en.wikipedia.org/wiki/The_Great_Gatsby"
    , [ "The Great Gatsby" ]
    )
  , ( "https://en.wikipedia.org/wiki/The_Sandman_%28Vertigo%29"
    , [ "Sandman" ]
    )
  , ( "https://en.wikipedia.org/wiki/Thoroughbred"
    , [ "Thoroughbred" , "thoroughbred" ]
    )
  , ( "https://en.wikipedia.org/wiki/Threshold_model#Liability_threshold_model"
    , [ "liability threshold" , "liability-threshold" ]
    )
  , ( "https://en.wikipedia.org/wiki/Troll_%28Internet%29"
    , [ "trolling" ]
    )
  , ( "https://en.wikipedia.org/wiki/Twitter" , [ "Twitter" ] )
  , ( "https://en.wikipedia.org/wiki/Umineko_When_They_Cry"
    , [ "Umineko" ]
    )
  , ( "https://en.wikipedia.org/wiki/Unicode" , [ "Unicode" ] )
  , ( "https://en.wikipedia.org/wiki/Usenet"
    , [ "Usenet" ]
    )
  , ( "https://en.wikipedia.org/wiki/Valerian_(herb)"
    , [ "Valerian" , "valerian" ]
    )
  , ( "https://en.wikipedia.org/wiki/Valium" , [ "Valium" ] )
  , ( "https://en.wikipedia.org/wiki/Value_of_information"
    , [ "VoI", "value of information" ]
    )
  , ( "https://en.wikipedia.org/wiki/Vaping" , [ "vaping" ] )
  , ( "https://en.wikipedia.org/wiki/Variance"
    , [ "Variance" , "variance" ]
    )
  , ( "https://en.wikipedia.org/wiki/Vergina_Sun" , [ "Vergina sun" ] )
  , ( "https://en.wikipedia.org/wiki/Vermilion" , [ "vermilion" ] )
  , ( "https://en.wikipedia.org/wiki/Vitamin_D"
    , [ "Vitamin D" , "vitamin D" ]
    )
  , ( "https://en.wikipedia.org/wiki/Vocaloid" , [ "Vocaloid" ] )
  , ( "https://en.wikipedia.org/wiki/What_Technology_Wants"
    , [ "What Technology Wants" ]
    )
  , ( "https://en.wikipedia.org/wiki/WikiLeaks" , [ "Wikileaks", "WikiLeaks" ] )
  , ( "https://en.wikipedia.org/wiki/William_Gibson"
    , [ "Gibson" , "William Gibson" ]
    )
  , ( "https://en.wikipedia.org/wiki/William_James"
    , [ "William James" ]
    )
  , ( "https://en.wikipedia.org/wiki/Wine_%28software%29"
    , [ "WINE" , "Wine" ]
    )
  , ( "https://en.wikipedia.org/wiki/Wired_%28magazine%29"
    , [ "Wired" ]
    )
  , ( "https://en.wikipedia.org/wiki/WorldCat" , [ "WorldCat" ] )
  , ( "https://en.wikipedia.org/wiki/Xenophon" , [ "Xenophon" ] )
  , ( "https://eprint.iacr.org/2002/160.pdf"
    , [ "Cryptology and Physical Security: Rights Amplification in Master-Keyed Mechanical Locks"
      ]
    )
      , ( "https://github.com/KichangKim/DeepDanbooru"
    , [ "DeepDanbooru" ]
    )
  , ( "https://github.com/jgm/gitit" , [ "Gitit" , "gitit" ] )
  , ( "https://github.com/linkchecker/linkchecker"
    , [ "linkchecker" ]
    )
      , ( "https://hackage.haskell.org/package/gitit"
    , [ "Gitit" , "gitit" ]
    )
  , ( "https://hal.science/hal-00904097/document#pdf"
    , [ "Why do humans reason? Arguments for an argumentative theory" ]
    )
  , ( "https://github.com/oduwsdl/archivenow" , [ "archivenow" ] )
  , ( "https://jeffhuang.com/papers/HaloLearning_CHI13.pdf"
    , [ "Mastering the Art of War: How Patterns of Gameplay Influence Skill in Halo"
      ]
    ),
    ( "https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1000097"
      , [ "Preferred Reporting Items for Systematic Reviews and Meta-Analyses: The PRISMA Statement', Moher et al 2009"
        , "PRISMA" ])
  , ( "https://www.reddit.com/r/MachineLearning/comments/akbc11/p_tag_estimation_for_animestyle_girl_image/"
    , [ "DeepDanbooru" ]
    ), ( "https://openai.com/research/openai-five-defeats-dota-2-world-champions" , [ "OA5"  ] ), ( "https://pactwebserial.wordpress.com/" , [ "Pact" ] ), ( "https://pdfs.semanticscholar.org/2576/fd36efa9be01a26269e94925283de306cd83.pdf"
    , [ "Consumer Heterogeneity and Paid Search Effectiveness: A Large Scale Field Experiment"
      ]
    ), ( "https://pdfs.semanticscholar.org/5e20/ba33230aa7d6d3a4c3e835fcd5c28fbde529.pdf"
    , [ "A Novel BHLHE41 Variant is Associated with Short Sleep and Resistance to Sleep Deprivation in Humans"
      , "Pellegrino et al 2014"
      ]
    ),  ( "https://predictionbook.com/"
    , [ "PredictionBook" , "PredictionBook.com" ]
    ), ( "https://web.archive.org/web/20140527121332/http://www.infinityplus.co.uk/stories/under.htm"
    , [ "\"Understand\"" ]
    ), ( "https://web.archive.org/web/20161021035119/http://www.ibooksonline.com/88/Text/hell.html"
    , [ "Hell is the Absence of God" ]
    ), ( "https://www.nplusonemag.com/issue-3/reviews/adventures-of-a-man-of-science/"
    , [ "Adventures of a Man of Science" , "Batuman 2005" ]
    ), ( "/doc/genetics/selection/artificial/index-selection/2011-cole.pdf"
    , [ "Cole & VanRaden 2011"
      , "Use of haplotypes to estimate Mendelian sampling effects and selection limits"
      ]
    )
  ,( "https://learningtopredict.github.io/#google"
     , [ "Learning to Predict Without Looking Ahead: World Models Without Forward Prediction"
       , "Our agents are only given infrequent observations of the real environment. As a side effect for optimizing performance in this setting, a 'world model' emerges. We show the true dynamics in color, with full saturation denoting frames the policy can see. The black and white outline shows the state of the emergent world model. These world model exhibits similar, but not identical dynamics to forward predictive models but only model 'important' aspects of the environment"
       ]
     )
    , ( "https://www.lesswrong.com/" , [ "LessWrong" , "LessWrong.com" ] )
    , ( "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4417674/"
      , [ "CRISPR/Cas9-mediated gene editing in human tripronuclear zygotes"
        ]
                                                                         )
    , ( "https://longbets.org/661/"
      , [ "661, By 2020, Urban and vertical farms will replace 10% of city produce in Chicago"
        , "By 2020, Urban and vertical farms will replace 10% of city produce in Chicago"
      ]
                                                                            )
    , ( "https://michaelnielsen.org/" , [ "Michael Nielsen" ] )
    , ( "https://mlp.fandom.com/wiki/Maud_Pie" , [ "Maud Pie" ] )
    , ( "https://twitter.com/theshawwn" , [ "Shawn Presser" ] )
    , ( "https://www.amazon.com/Turings-Cathedral-Origins-Digital-Universe/dp/1400075998/"
      , [ "Turing\8217s Cathedral" ]
      )
    , ( "https://www.biorxiv.org/content/10.1101/016477.full"
      , [ "Eight thousand years of [human] natural selection in Europe" ]
      )
    , ( "https://www.biorxiv.org/content/10.1101/078014.full"
      , [ "Educational attainment and personality are genetically intertwined"
        ]
    )
    , ( "https://www.biorxiv.org/content/10.1101/106203.full"
    , [ "Genomic analysis of family data reveals additional genetic effects on intelligence and personality"
      , "Hill et al 2017"
      ]
    ), ( "https://www.biorxiv.org/content/10.1101/106203.full"
    , [ "Genomic analysis of family data reveals additional genetic effects on intelligence and personality"
      ]
    ), ( "https://www.biorxiv.org/content/10.1101/190124.full"
    , [ "Accurate Genomic Prediction Of Human Height"
      , "Lello et al 2017"
      ]
    ), ( "https://www.dougengelbart.org/pubs/papers/scanned/Doug_Engelbart-AugmentingHumanIntellect.pdf"
    , [ "Augmenting Human Intellect: A Conceptual Framework"
      , "Engelbart 1962"
      ]
    ), ( "https://www.escholar.manchester.ac.uk/api/datastream?publicationPid=uk-ac-man-scw:227658&datastreamId=FULL-TEXT.PDF"
    , [ "Decision Making Using Thompson Sampling" , "Mellor 2014" ]
    ), ( "https://www.fimfiction.net/story/62074/Friendship-is-Optimal"
    , [ "Friendship is Optimal" ]
    ), ( "https://www.lesswrong.com/"
    , [ "Less Wrong" , "LessWrong" , "LessWrong.com" ]
    ), ( "https://www.lesswrong.com/tag/computing-overhang"
    , [ "hardware overhang" ]
    ), ( "https://www.lesswrong.com/tag/inside-outside-view"
    , [ "Outside View" , "outside view" ]
    ), ( "https://www.nature.com/articles/mp2014188/"
    , [ "Genetic contributions to variation in general cognitive function: a meta-analysis of genome-wide association studies in the CHARGE consortium (<em>n</em> = 53949"
      ]
    ), ( "https://www.nature.com/articles/mp2016107"
    , [ "Predicting educational achievement from DNA" ]
    ), ( "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3652710/"
    , [ "Common DNA Markers Can Account for More Than Half of the Genetic Influence on Cognitive Abilities"
      ]
    ), ( "/doc/iq/2013-rietveld.pdf"
    , [ "GWAS of 126,559 Individuals Identifies Genetic Variants Associated with Educational Attainment"
      , "Rietveld et al 2013"
      ]
    ), ( "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3969807/"
    , [ "The 1% of the population accountable for 63% of all violent crime convictions"
      ]
    ), ( "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4096202/"
    , [ "A Novel BHLHE41 Variant is Associated with Short Sleep and Resistance to Sleep Deprivation in Humans"
      , "Pellegrino et al 2014"
      ]
    ), ( "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4313871/"
    , [ "The contribution of <em>de novo</em> coding mutations to autism spectrum disorder"
      ]
    ), ( "/doc/psychiatry/lithium/2015-helbich.pdf"
    , [ "Helbich et al 2015"
      , "Lithium in drinking water and suicide mortality: interplay with lithium prescriptions"
      ]
    ), ( "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4883595/"
    , [ "Genome-wide association study identifies 74 loci associated with educational attainment"
      , "Okbay et al 2016"
      ]
    )
    , ( "https://en.wikipedia.org/wiki/Standard_score"
    , ["z-score", "z-scores", "Z-score", "_z_-score", "_z_-scores", "_Z_-score", "<em>z</em>-score", "<em>z</em>-scores", "<em>Z</em>-score"]
    )
    , ("https://en.wikipedia.org/wiki/Silk_Road_(marketplace)#Silk_Road_2.0", ["Silk Road 2", "Silk Road 2.0"])
    , ("https://en.wikipedia.org/wiki/LSD", ["LSD-25", "LSD", "lysergic acid diethylamide"])
    , ("https://en.wikipedia.org/wiki/Animal_welfare", ["animal welfare"])
    , ("https://en.wikipedia.org/wiki/Dungeons_%26_Dragons", ["D&D"])
    , ("https://en.wikipedia.org/wiki/The_Guardian", ["_The Guardian_", "The Guardian"])
    , ("https://en.wikipedia.org/wiki/Openness_to_experience", ["Openness", "openness", "open to experience", "Open To Experience"])
  , ( "https://en.wikipedia.org/wiki/Wikileaks" , [ "Wikileaks" ] )
  , ( "https://en.wikipedia.org/wiki/Wine_(software)"
    , [ "WINE" ]
    )
  , ( "https://en.wikipedia.org/wiki/Wired_(magazine)"
    , [ "Wired" ]
    )
  , ( "https://en.wikipedia.org/wiki/Wozzeck" , [ "Wozzeck" ] )
    , ( "https://en.wikipedia.org/wiki/Troll_(Internet)"
    , [ "trolling" ]
    )
    , ("https://en.wikipedia.org/wiki/Rumi", ["Rumi"] )
  ]
