#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
-- dependencies: libghc-pandoc-dev, Unique, monad-parallel, Interwiki.hs

-- usage: 'find . -name "*.page" -or -name "*.html" | link-suggester.hs'; like 'link-extractor.hs', this extracts URLs from Markdown/HTML files
-- but with a focus on compiling `(anchor/target, URL)` pairs to be used for semi-automated link rewrites in Emacs.

module Main where

import Data.List (intercalate, nub, sort, sortBy)
import qualified Data.Map.Strict as M (difference, elems, filter, filterWithKey, fromListWith, toList, map, Map)
import Data.Maybe (isJust)
import qualified Data.Set as S (fromList, member, Set)
import qualified Data.Text as T (append, dropWhile, dropWhileEnd, length, lines, intercalate, pack, isInfixOf, isPrefixOf, toLower, unpack, Text)
import Data.Char (isSpace, isPunctuation)
import qualified Data.Text.IO as TIO (readFile)
import System.Environment (getArgs)

import Text.Show.Pretty (ppShow)

import Control.Monad.Parallel as Par (mapM) -- monad-parallel

import Data.List.Unique as U (repeated) -- Unique

import Text.Regex (mkRegex, matchRegex)

import LinkAuto (linkAuto)
import Query (extractURLsAndAnchorTooltips, parseMarkdownOrHTML)
import Utils (writeUpdatedFile, printGreen)

hitsMinimum, anchorLengthMaximum :: Int
hitsMinimum = 2
anchorLengthMaximum = 330

-- | Map over the filenames
main :: IO ()
main = do
  (outputTarget:_) <- getArgs
  fs         <- fmap lines getContents
  printGreen "Parsing all files for links…"
  pairs <- fmap concat $ Par.mapM parseURLs fs
  printGreen "Parsed all files for links."

  -- blacklist bad URLs, which don't count
  let db = M.filterWithKey (\k _ -> not $ filterURLs k) $ M.fromListWith (++) pairs :: M.Map T.Text [T.Text]

  -- we de-duplicate *after* checking for minimum. Particularly for citations, each use counts, but we don't need each instance of 'Foo et al 2021' in the DB (`/usr/share/dict/words`), so we unique the list of anchors
  let db' = M.map (nub . sort . cleanAnchors) $ M.filter (\texts -> length texts >= hitsMinimum) db

  -- We want to filter out any anchor text which is associated with more than 1 URL (those are too ambiguous to be useful), any text which is in the system dictionary, and anything in the blacklist patterns or list.
  let anchorTextsDupes = U.repeated $ concat $ M.elems db'
  dict <- dictionarySystem
  let db'' = M.filter (not . null) $ M.map (filter (\t -> not (t `elem` anchorTextsDupes || filterAnchors dict t))) db'

  -- record what we threw away for later inspection:
  let deletedDb = M.difference db' db''
  -- print $
  --   sortBy (\t1 t2 -> if (T.length t1 > T.length t2) then LT else if (T.length t1 == T.length t2) then if (t1 > t2) then LT else GT else GT) $
  --   filter (\t -> (" " `T.isInfixOf` t) && (all (\t'' -> S.member (T.toLower t'') dict) $ T.words t)) $ concat $ M.elems db''

  -- swap [(URL,[Anchor])] to [(Anchor,URL)] (which we need for doing 'search-replace before after'), by expanding/flattening the list:
  let reversedDB = concatMap (\(url,ts) -> zip ts (repeat url)) $ M.toList db''
  -- sort by length of anchor text in descending length: longer matches should come first, for greater specificity in doing rewrites.
  let reversedDBSorted = sortBy (\(t1,_) (t2,_) -> if T.length t1 > T.length t2 then LT else if T.length t1 == T.length t2 then if t1 > t2 then LT else GT else GT) reversedDB
  let elispDB = haskellListToElispList reversedDBSorted

  writeUpdatedFile "linkSuggestions.el.tmp" outputTarget elispDB
  writeUpdatedFile "linkSuggestions-deleted.hs.tmp" "metadata/linkSuggestions-deleted.hs" $ T.pack (ppShow deletedDb)
  printGreen "Wrote out link suggestion database."

-- format the pairs in Elisp `(setq rewrites '((foo bar)) )` style so it can be read in & executed directly by Emacs's `load-file`.
haskellListToElispList :: [(T.Text, T.Text)] -> T.Text
haskellListToElispList xs = "(setq markdown-rewrites '(\n                 "
                            `T.append`
                            T.intercalate "\n                 " (map (T.pack . (\(t,u) -> "(" ++ show t ++ " " ++ show u ++ ")")) xs)
                            `T.append`
                            "\n                 )\n      )\n"

-- clean/rewrite anchors to deal with whitespace/punctuation in links, eg. "RahXephon:" or "theanine," or "Bitcoin ".
cleanAnchors :: [T.Text] -> [T.Text]
cleanAnchors = map cleanAnchor
   where cleanAnchor :: T.Text -> T.Text
         cleanAnchor = T.dropWhileEnd trimText . T.dropWhile trimText
         trimText = (\c -> isSpace c || isPunctuation c || c == '=')

-- return True if matches any blacklist conditions
filterURLs :: T.Text -> Bool
filterURLs    u = "$"`T.isPrefixOf`u || "\8383"`T.isPrefixOf`u || "#"`T.isPrefixOf`u || "/static/img/"`T.isPrefixOf`u || "/newsletter/20"`T.isPrefixOf`u ||
                  u `elem` ["https://www.reuters.com/article/us-germany-cyber-idUSKCN1071KW"]
filterAnchors :: S.Set T.Text -> T.Text -> Bool
filterAnchors d t = T.length t > anchorLengthMaximum ||
                    S.member (T.toLower t) d ||
                    isJust (matchRegex regex (T.unpack t)) ||
                    "$"`T.isInfixOf`t || "%"`T.isInfixOf`t || "["`T.isInfixOf`t || "]"`T.isInfixOf`t ||
                    "("`T.isPrefixOf`t  || "."`T.isPrefixOf`t ||
                    "&"==t ||
                    elem t badStrings
  where regex = mkRegex $ intercalate "|" $ map (\r -> "^"++r++"$") ["[0-9]+[kmgbt]?", "[0-9]+[\\.,;–-][0-9]+", "pg[0-9]+", "p\\.[0-9]+", "[0-9]+[a-f]", "in [12][0-9][0-9][0-9]", "[Ff]igure S?[0-9]+[a-f]?", "[Tt]able S?[0-9]+[a-f]?", "[Cc]hapter [0-9]+"]
        badStrings = ["has shown", "the end", "in the", "One", "one", "ast", "sed", "le", "intro", "Quest", "in the", "very fast", "it was noted", "ODS", "diff of", "AK", "#8", "DL", "DM", "GQ", "H+", "JS", "NS", "OA", "TR", "TV", "Xs", "i2", "s9", "v3", "Ant", "Ash", "CDC", "CO2", "DFA", "DOI", "DoJ", "FAQ", "FDA", "GOS", "Her", "III", "IRC", "Lab", "Law", "MP4", "Net", "No.", "OA5", "P&G", "PDF", "RNA", "RSS", "SR1", "SVG", "TSG", "bre", "cev", "mor", "free", "Goog", "HTML", "Hero", "JSON", "Lili", "Loui", "MIRI", "MeFi", "NYRB", "Open", "Poll", "& AI", "3\8211\&5\215", ">307", "S9E4", "S6E9", "S5E9", "S5E5", "S5E2", "S5E1", "S3E4", "S3E3", "S2E8", "S2E4", "S1E2", "S1E1", "5\8211\&8", "1\8211\&5", "1\8211\&4", "15\215", "SR1F", "SR2F", "Tofu", "Vice", "WaOi", "XSLT", "p.\160\&2", "s1\8211\&8", "Alone", "Alpha", "Brave", "Dwarf", "Image", "Howes", "Karma", "Korff", "Okada", "Paper", "Rotem", "Raman", "Quora", "Salon", "SciAm", "Share", "Slate", "Spent", "Slice", "Verge", "Zagat", "as is", "aries", "anti-", "ac.uk", "gwern", "don\8217t", "is to", "blixt", "the E", "thin,", "won\8217t", "0.45kg", "Bamboo", "Echoes", "Erowid", "Genius", "Hoogle", "I read", "Indeed", "58,020", "4-item", "100GHz", "10-50k", "0.45kg", "\27531\12425\12378\12398\26862", "\26481\26041\22934\12293\22818", "\24651\24515\12402\12392\12388", "S9E23", "S9E21", "S9E14", "S9E13", "S8E27", "S8E23", "S8E21", "S8E20", "S8E14", "S7E24", "S7E14", "S7E13", "S7E10", "S6E25", "S6E22", "S5E25", "S5E21", "S5E18", "S5E13", "S5E12", "S4E26", "S4E25", "S4E18", "S3E13", "S3E12", "S3E11", "S2E26", "S2E25", "S2E15", "S2E14", "S1E26", "S1E14", "S1E11", "Part 1", "Singal", "Tumblr", "XMonad", "a book", "a scan", "a vice", "and do", "c2.com", "ccc.de", "ch14.5", "game 1", "game 2", "goo.gl", "jttoto", "may be", "muflax", "ponzis", "so low", "tality", "to why", "w3.org", "2 weeks", "BLDBLOG", "Hackage", "Mark Xu", "ON FIRE", "Part II", "Table 4", "The Age", "USB hub", "Vaniver", "a third", "a rumor", "a quarter", "a fifth", "a sixth", "a seventh", "a half", "help me", "held up", "be made", "bls.gov", "by fans", "dataset", "doi.org", "fda.gov", "grok.se", "jwz.org", "lwn.net", "make it", "mega.nz", "nih.gov", "nips.css", "npr.org", "or more", "oup.com", "over an", "r = 0.4", "row1.ca", "the USA", "the BBC", "ted.com", "tale of", "vox.com", "Al-Qaeda", "Alzforum", "I set up", "March 11", "NBC News", "NYT 2011", "Part Two", "Part One", "Pulp Mag", "Redditor", "Reset.me", "SD cards", "STRATFOR", "See here", "T-shirts", "To quote", "U-shaped", "UOChris1", "a genius", "a review", "a saying", "about it", "ajcn.org", "aleph.se", "dtic.mil", "fair use", "followup", "gnxp.com", "has said", "has told", "he wrote", "her book", "iacr.org", "ieet.org", "ietf.org", "it sells", "imdb.com", "leme.me/", "leme.me", "mdpi.com", "mid-2012", "nber.org", "ncase.me", "nejm.org", "one site", "one user", "our book", "pet flap", "pre-2012", "pre-GWAS", "season 9", "synaptic", "tells us", "the code", "the king", "the seen", "the size", "this one", "to solve", "type=pdf", "youtu.be", "vice.com", "#facebook", "was also", "Ashkenazi", "Chapter 5", "Dune-like", "Gainax.fr", "How often", "IQs lower", "LW review", "Lyons Den", "New Links", "OA claims", "SR forums", "Salon.com", "Some work", "Top books", "US states", "acne page", "a service", "a network", "cdlib.org", "busted in", "demo quiz", "doc88.com", "does know", "does seem", "gwern.net", "imgur.com", "isfdb.org", "libgen.is", "last very", "long-term", "made bail", "many ways", "maybe not", "my config", "much less", "not heard", "odd study", "of course", "often low", "oglaf.com", "on poetry", "one study", "only once", "only exit", "own films", "peerj.com", "post-rock", "quora.com", "some harm", "still bad", "storks in", "summed to", "the heart", "this link", "An article", "Anno again", "for my dog", "gatotsu911", "gitlab.com", "gitcoin.co", "in Ireland", "in Bitcoin", "karthik bm", "kaggle.com", "is amusing", "libgen.org", "kriegerlie", "logging-in", "medium.com", "much-cited", "more sober", "newegg.com", "flickr.com", "figure 2/3", "fatal flaw", "fandom.com", "face crops", "fMRI study", "exigentsky", "erowid.org", "episode 18", "episode 10", "encourager", "eepurl.com", "early 2012", "due solely", "discord.gg", "digit span", "did arrive", "demo video", "One survey", "Part Three", "backing up", "as of 2014", "as of 2011", "art & wine", "arrested 5", "are making", "are due to", "are banned", "archivenow", "archive.is", "antilop.cc", "anonymized", "annals.org", "an attempt", "amazon.com", "amazon.co.", "a web page", "a historic", "a commuter", "census.gov", "debian.org", "In a study", "If he does", "I found it", "I expected", "I conclude", "ANN thread", "7th report", "4 examples", "23 in 2018", "2020-07-21", "2019-07-05", "2019-06-26", "2016-08-16", "2015-03-26", "2015-03-23", "2014-12-26", "2014-06-09", "2013-12-20", "2013-12-19", "2013-11-26", "2013-09-03", "2013-04-23", "2013-04-18", "2013-03-13", "2013-02-16", "2013-01-04", "2012-11-07", "2012 draft", "2011 paper", "2010 trial", "2008-11-01", "2006 paper", "2005 essay", "2 2.5-inch", "1933 paper", "150 people", "A trainer", "A chapter", "6-7 years", "4chan.org", "4 million", "3\8260\&4s cost", "2.5 years", "2 servers", "1908/1966", "100\8211\&1000\215", "qntm.org", "preprint", "premium\8217", "\12454\12469\12462\12488\12456\12468", "\12402\12425\12426\21380\31070\27096", "9 years", "8 of 11", "53 cats", "50-100k", "5 years", "3 years", "2TB one", "Sturm\8217s", "\37504\27827\12392\24651\33394\39764\27861", "\30495\29983\26410\20998\12398\19968\24515", "\21380\31070\27096\12398\36890\23398\36335", "\12371\12393\12418\12398\12375\12367\12415", "\8383\&39,644", "~20kj/g", "A-series", "900W UPS", "8mm film", "40mg/day", "40 bytes", "2channel", "175 days", "12kb/day", "100 days", "/r/SilkRoad", "1 dead baby", "12-36 hours", "2 terabytes", "2004 SD was", "2007 survey", "2008 handout", "2008 review", "2010 report", "2011 report", "2011 survey", "2013 review", "2019 profile", "2021 profile", "20th century", "400,000 year", "Dell monitor", "Episode One", "Episode Six", "I have been", "Jaeggi says", "One blogger", "One episode", "RSS version", "Steven Kaas", "The Verge\8217s", "The article", "\24189\38597\12395\21682\12363\12379\12289\22696\26579\12398\26716", "\8217perfection", "a blog post", "a few lines", "a reference", "a tradition", "affect mood", "all of them", "arrest of 2", "biomedicine", "black women", "blood sugar", "can lead to", "can\8217t build", "captainjojo", "expel elves", "for aspirin", "for example", "for novelty", "homeostasis", "in an essay", "in my table", "is improved", "latest work", "legal highs", "less secure", "like murder", "looks like", "makes sense", "many things", "media op-ed", "mostly when", "mouse study", "my own page", "my surprise", "neuroticism", "not too bad", "of empathy", "off of SR2", "on scaling", "one article", "online demo", "psychopathy", "push-button", "scaling up", "self-esteem", "simple SVG", "small study", "snowy halcy", "source code", "speeding up", "steady fall", "still walk", "such files", "summer jobs", "than before", "that config", "that could", "the Quakers", "the morning", "the seasons", "their graph", "there too!", "to the mean", "told not to", "underspend", "until 2014", "usually not", "very tricky", "video demo", "was minimal", "whitepaper", "winning big", "with itself", "with others", "worked out", "~2014-11-06", "by his", "so many", "Part I", "long way", "Part Four", "part Five", "also with", "how often", "believe it", "points out", "feline form", "first quote", "forum posts", "pretty safe", "nice slides", "first result", "been in jail", "if available", "how critical", "their health", "such as Iran","site traffic","shallow moon","sees through","second quote","scaling laws","sanded floor", "was searched", "vague musing", "the ambitions", "within a year", "wrote a paper", "my prediction", "more strongly","more peculiar","more coverage", "just one such","is reportedly", "was skeptical", "dental records", "Lowe commentary", "I have recorded","I had predicted", "did not do much", "a later analysis", "a curious appeal", "awesome as it is", "also interesting", "absolutely right", "link compilation", "landmark address", "less on conflict", "the difference", "and identified", "other benefits", "historical context", "as predicted", "and identified", "Some studies", "well-studied", "well studied", "Colab", "historical survey", "the relationship", "high-dimensional", "unigrams", "health benefits", "first proposed", "the solution", "It turns out", "an interview", "poorly studied", "deep learning", "there is evidence", "fatal accidents", "in animal models", "psychiatric disorders", "summary statistics", "are available", "small fraction", "FDA-approved", "June 2021", "large dataset", "cognitive training", "reported that", "famously argued that", "2016 US presidential election", "GPS", "Blue light", "research paper", "status hierarchies", "arxiv", "probably not", "brain plasticity", "September 2021", "twin studies", "November 2019", "", "<5", "<10", "~16", "<10", "impact-factor", "Donald Trump", "AI", "in China", "several studies", "John F. Kennedy", "interesting thing", "public datasets", "May 2021", "side effects", "visualizations"]

-- We do case-insensitive matching to pick up dictionary words at beginning of sentence. We only look up entire phrases; if we split into words by whitespace and look up each word, then even when all words are in the dictionary, too many reasonable anchor texts would be filtered out.
dictionarySystem :: IO (S.Set T.Text)
dictionarySystem = fmap (S.fromList . T.lines . T.toLower) $ TIO.readFile "/usr/share/dict/words"

-- | Read 1 file and return its URL-pairs
parseURLs :: FilePath -> IO [(T.Text, [T.Text])]
parseURLs file = do
  input <- TIO.readFile file
  let converted = extractURLsAndAnchorTooltips $ linkAuto $ parseMarkdownOrHTML True input
  return converted
