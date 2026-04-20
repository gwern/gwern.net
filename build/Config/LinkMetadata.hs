{- Config.LinkMetadata: config data, unit-tests, for LinkMetadata
Author: Gwern Branwen
Date: 2019-08-20
When:  Time-stamp: "2026-04-19 20:19:39 gwern"
License: CC-0
-}

{-# LANGUAGE OverloadedStrings #-}
module Config.LinkMetadata where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T (Text)
import qualified Data.Map.Strict as M (empty, lookup)
import Text.Pandoc (Format(..), Block(Div, Para), Inline(Link, Str, Code, Strong, RawInline))
import LinkMetadataTypes (ArchiveMetadata, Metadata, MetadataItem)

annotationClasses, positiveAnnotationClasses :: [T.Text]
annotationClasses =
  [ "link-annotated-not"
  , "link-annotated"
  , "link-annotated-partial"
  , "image-annotated"
  , "image-annotated-not"
  , "image-annotated-partial"
  ]
positiveAnnotationClasses =
  [ "link-annotated"
  , "link-annotated-partial"
  , "image-annotated"
  , "image-annotated-partial"
  ]

-- do not prefetch large URLs:
maxPrefetchBytes :: Integer
maxPrefetchBytes = 10000000

-- magic numbers for 'scoring the quality of an annotation', used in 'LM.writeAnnotationFragments'
partialAnnotationIgnoredTagCount, partialAnnotationBacklinkThreshold, partialAnnotationSimilarThreshold, annotationURLWarningLength, annotationURLPreviewLength, missingTitleAbstractMinLength :: Int
partialAnnotationIgnoredTagCount = 2
partialAnnotationBacklinkThreshold = 1
partialAnnotationSimilarThreshold = 6
annotationURLWarningLength = 273
annotationURLPreviewLength = 247
missingTitleAbstractMinLength = 100

-- a date too many years in the future is suspicious:
futureYearSlack :: Int
futureYearSlack = 2

-- media magic variables
-- test: none
youtubeWatchPrefix, twitterHostPrefix, twitterStatusInfix, wikipediaArticleInfix :: String
youtubeWatchPrefix    = "https://www.youtube.com/watch?v="
twitterHostPrefix     = "https://x.com/"
twitterStatusInfix    = "/status/"
wikipediaArticleInfix = "wikipedia.org/wiki/"
-- all below: test: unique list
documentPreviewableExtensions, codePreviewableExtensions, fileViewableExtensions :: [String]
documentPreviewableExtensions = [".json", ".jsonl", ".opml", ".md", ".pdf", ".txt", ".xml"]
codePreviewableExtensions = [".R", ".css", ".hs", ".js", ".patch", ".sh", ".php", ".conf"]
fileViewableExtensions = [".html", ".pdf", ".txt"]

-- Metadata fields checking
-- all below: test: unique list
badDOISubstrings :: [String]
badDOISubstrings = ["–", "—", " ", ",", "{", "}", "!", "@", "#", "$", "\"", "'", "arxiv", ".org", "http"]

badTitleLeadingChars, badTitleTrailingChars :: String
badTitleLeadingChars = "<\\;,_~=-({"
badTitleTrailingChars = ">\\;,_~=+-)}:"

badAuthorSubstrings :: [String]
badAuthorSubstrings = [";", "&", "?", "!", " >", "< ", " <"]

allowedNonHttpURLPrefixes :: [String]
allowedNonHttpURLPrefixes = ["mailto:", "irc://", "rsync://"]

uriValidationExemptInfixes :: [String]
uriValidationExemptInfixes = ["wikipedia.org", "hoogle.haskell.org"]

ignoredMalformedURLPrefixes :: [String]
ignoredMalformedURLPrefixes = ["ttps://", "ttp://", "/wiki", "wiki/", "/http"]

duplicateAffiliationWhitelist :: [String]
duplicateAffiliationWhitelist = ["page=", "lilianweng.github.io"]

-- config testing: none? too many overlaps
fileTranscludesTest :: (ArchiveMetadata -> (FilePath, MetadataItem) -> [Block]) -> Metadata -> ArchiveMetadata -> [([Block], [Block])]
fileTranscludesTest f md am =
  let testFileTransclude md' am' path = let x = fromJustWithError path $ M.lookup path md'
                                             in f am' (path, x) -- 'f' is assumed to be LM.generateFileTransclusionBlock
      simpleTestEmpty = testFileTransclude md M.empty
      simpleTest = testFileTransclude md am
  in -- config test: unique-keys
    [ (simpleTest "/review/bakker", [])
    , (simpleTest "/index", [])
    , (simpleTest "/doc/newest/index", [])
    , (simpleTest "/doc/cs/algorithm/1986-aggarwal.pdf", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse","mobile-not"],[]) [Para [Strong [Str "View ",Str "PDF"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","pdf"),("link-icon-type","svg"),("link-icon-color","#f40f02")]) [RawInline (Text.Pandoc.Format "HTML") "Geometric applications of a matrix searching algorithm"] ("/doc/cs/algorithm/1986-aggarwal.pdf","")]]]])
    -- , (simpleTest "/doc/cs/algorithm/1986-aggarwal.pdf", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse","mobile-not"],[]) [Para [Strong [Str "View ",Str "PDF"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","pdf"),("link-icon-type","svg"),("link-icon-color","#f40f02")]) [RawInline (Text.Pandoc.Format "HTML") "Geometric applications of a matrix searching algorithm"] ("/doc/cs/algorithm/1986-aggarwal.pdf","")]]]])
    , (simpleTest "/doc/cs/algorithm/1990-galil.pdf", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse","mobile-not"],[]) [Para [Strong [Str "View ",Str "PDF"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","pdf"),("link-icon-type","svg"),("link-icon-color","#f40f02")]) [RawInline (Text.Pandoc.Format "HTML") "A linear-time algorithm for concave one-dimensional dynamic programming"] ("/doc/cs/algorithm/1990-galil.pdf","")]]]])
    , (simpleTest "/doc/economics/2010-mankiw.pdf", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse","mobile-not"],[]) [Para [Strong [Str "View ",Str "PDF"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","pdf"),("link-icon-type","svg"),("link-icon-color","#f40f02")]) [RawInline (Text.Pandoc.Format "HTML") "The Optimal Taxation of Height: A Case Study of Utilitarian Income Redistribution"] ("/doc/economics/2010-mankiw.pdf","")]]]])
    , (simpleTestEmpty "https://arxiv.org/abs/1505.03118", [])
    , (simpleTest "https://blog.codinghorror.com/if-you-dont-change-the-ui-nobody-notices/", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "External Link"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon-color","#57a3e8")]) [RawInline (Text.Pandoc.Format "HTML") "If You Don\8217t Change the UI, Nobody Notices: I saw a screenshot a few days ago that made me think Windows 7 Beta might actually be worth checking out."] ("https://blog.codinghorror.com/if-you-dont-change-the-ui-nobody-notices/","")]]]])
    , (simpleTest "https://harpers.org/archive/2022/04/night-shifts-dream-incubation-technology-sleep-research/", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "HTML"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","H"),("link-icon-type","text")]) [RawInline (Format "HTML") "Night Shifts: Can technology shape our dreams?"] ("/doc/www/harpers.org/f50360d6a34f28a00f78a7359ed4c3978afd211b.html","")]]]])
    , (simpleTestEmpty "https://news.ycombinator.com/item?id=31274155", [])
    , (simpleTestEmpty "https://founders.archives.gov/documents/Jefferson/03-06-02-0322", [])
    , (simpleTestEmpty "https://arxiv.org/abs/2311.17137", [])
    , (simpleTest "https://arxiv.org/abs/1212.6177", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse","mobile-not"],[]) [Para [Strong [Str "View ",Str "PDF"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","\120536"),("link-icon-type","text"),("link-icon-color","#b31b1b")]) [RawInline (Text.Pandoc.Format "HTML") "How Much of the Web Is Archived?"] ("/doc/www/arxiv.org/b9be349cb3d46669ca7d72f056d7180208257b74.pdf","")]]]])
    , (simpleTest "https://openreview.net/forum?id=-WsBmzWwPee", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse","mobile-not"],[]) [Para [Strong [Str "View ",Str "PDF"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","OR"),("link-icon-type","text,sans"),("link-icon-color","#8c1b13")]) [RawInline (Text.Pandoc.Format "HTML") "Realistic Face Reconstruction from Deep Embeddings"] ("/doc/www/openreview.net/f0c4012d829fbd5ff4e6187ce9dc5d3e3e656f89.pdf","")]]]])
    , (simpleTest "/lorem.md", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "Markdown"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","txt"),("link-icon-type","svg")]) [Code ("",[],[]) "/lorem.md"] ("/lorem.md","")]]]])
    , (simpleTest "/doc/ai/nn/gan/stylegan/2017-royer-cartoonset-randomsamples.png", [Div ("",["aux-links-transclude-file"],[]) [Para [Link ("",["link-annotated-not","include-content","width-full"],[]) [Code ("",[],[]) "/doc/ai/nn/gan/stylegan/2017-royer-cartoonset-randomsamples.png"] ("/doc/ai/nn/gan/stylegan/2017-royer-cartoonset-randomsamples.png","")]]])
    , (simpleTest "/doc/ai/nn/diffusion/midjourney/dropcap/dropcat/2023-10-21-gwern-midjourneyv5-cats-dark-mode-hissingkitten-edgeproblems-inverted.jpg", [Div ("",["aux-links-transclude-file"],[]) [Para [Link ("",["link-annotated-not","include-content","width-full"],[]) [Code ("",[],[]) "/doc/ai/nn/diffusion/midjourney/dropcap/dropcat/2023-10-21-gwern-midjourneyv5-cats-dark-mode-hissingkitten-edgeproblems-inverted.jpg"] ("/doc/ai/nn/diffusion/midjourney/dropcap/dropcat/2023-10-21-gwern-midjourneyv5-cats-dark-mode-hissingkitten-edgeproblems-inverted.jpg","")]]])
    , (simpleTest "/doc/ai/nn/gan/stylegan/2020-03-26-shawnpresser-stylegan2-imagenet-run52-1394688-interpolation-7.mp4", [Div ("",["aux-links-transclude-file"],[]) [Para [Link ("",["link-annotated-not","include-content","width-full"],[]) [Code ("",[],[]) "/doc/ai/nn/gan/stylegan/2020-03-26-shawnpresser-stylegan2-imagenet-run52-1394688-interpolation-7.mp4"] ("/doc/ai/nn/gan/stylegan/2020-03-26-shawnpresser-stylegan2-imagenet-run52-1394688-interpolation-7.mp4","")]]])
    , (simpleTest "/doc/anime/eva/notenki-memoirs/blue-blazes/10-3.webm", [Div ("",["aux-links-transclude-file"],[]) [Para [Link ("",["link-annotated-not","include-content","width-full"],[]) [Code ("",[],[]) "/doc/anime/eva/notenki-memoirs/blue-blazes/10-3.webm"] ("/doc/anime/eva/notenki-memoirs/blue-blazes/10-3.webm","")]]])
    , (simpleTest "/doc/ai/music/2020-04-18-gpt2-midi-bigdataset-124.mp3", [Div ("",["aux-links-transclude-file"],[]) [Para [Link ("",["link-annotated-not","include-content","width-full"],[]) [Code ("",[],[]) "/doc/ai/music/2020-04-18-gpt2-midi-bigdataset-124.mp3"] ("/doc/ai/music/2020-04-18-gpt2-midi-bigdataset-124.mp3","")]]])
    , (simpleTest "/doc/ai/nn/gan/stylegan/2020-05-05-tjukanov-mapdreameraicartography.html", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "HTML (63MB)"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","internet-archive"),("link-icon-type","svg")]) [Code ("",[],[]) "/doc/ai/nn/gan/stylegan/2020-05-05-tjukanov-mapdreameraicartography.html"] ("/doc/ai/nn/gan/stylegan/2020-05-05-tjukanov-mapdreameraicartography.html","")]]]])
    , (simpleTest "https://nyx-ai.github.io/stylegan2-flax-tpu/", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "HTML (19MB)"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","internet-archive"),("link-icon-type","svg")]) [Code ("",[],[]) "/doc/www/nyx-ai.github.io/a95f4c42e4300722b1adcf0f494ac943437fcc56.html"] ("/doc/www/nyx-ai.github.io/a95f4c42e4300722b1adcf0f494ac943437fcc56.html","")]]]])
    , (simpleTest "/doc/ai/anime/danbooru/2019-02-10-stylegan-holo-handselectedsamples.zip", [])
    , (simpleTest "/doc/anime/2019-05-06-stylegan-malefaces-1ksamples.tar", [])
    , (simpleTest "/doc/ai/anime/danbooru/2018-09-22-progan-holofaces-topdecile.tar.xz", [])
    , (simpleTest "http://dev.kanotype.net:8003/deepdanbooru/", [])
    , (simpleTest "https://x.com/AxSauer/status/1524325956030275586", [Div ("",["aux-links-transclude-file"],[]) [Para [Link ("",["id-not","include-content"],[]) [Code ("",[],[]) "/doc/www/localhost/a45010d731b0e6b20e5594567edcbb6978be49ab.html"] ("https://x.com/AxSauer/status/1524325956030275586","")]]])
    , (simpleTest "https://en.wikipedia.org/wiki/Amber_Heard",
       [Div ("",["aux-links-transclude-file"],[]) [Para [Link ("",["id-not","include-content"],[("include-template","$annotationFileIncludeTemplate")]) [RawInline (Format "HTML") "Amber Heard"] ("https://en.wikipedia.org/wiki/Amber_Heard","")]]])
    , (simpleTest "https://en.wikipedia.org/wiki/Category:American_Quakers",
       [Div ("",["aux-links-transclude-file"],[]) [Para [Link ("",["id-not","include-content"],[]) [RawInline (Format "HTML") "Category:American Quakers"] ("https://en.wikipedia.org/wiki/Category:American_Quakers","")]]])
    , (simpleTest "https://www.youtube.com/watch?v=D2zjc--sDaY", [Div ("",["aux-links-transclude-file"],[]) [Para [Link ("",["link-annotated-not","include-content","width-full"],[]) [Code ("",[],[]) "https://www.youtube.com/watch?v=D2zjc--sDaY"] ("https://www.youtube.com/watch?v=D2zjc--sDaY","")]]])
    , (simpleTest "https://www.reddit.com/r/MediaSynthesis/comments/tiil1b/xx_waifu_01_xx_loop_by_squaremusher/", [])
    , (simpleTest "https://caniuse.com/?search=text-wrap%3A%20pretty", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "External Link"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","CanI"),("link-icon-type","text,sans,quad"),("link-icon-color","#c75000")]) [RawInline (Format "HTML") "text-wrap: pretty"] ("https://caniuse.com/?search=text-wrap%3A%20pretty","")]]]])
    , (simpleTest "https://www.mdpi.com/2073-4409/10/7/1740/htm", [])
    , (simpleTest "/doc/ai/anime/danbooru/2020-05-31-danbooru2019-palm-handannotations-export.jsonl", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "JSON Lines"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","txt"),("link-icon-type","svg")]) [Code ("",[],[]) "/doc/ai/anime/danbooru/2020-05-31-danbooru2019-palm-handannotations-export.jsonl"] ("/doc/ai/anime/danbooru/2020-05-31-danbooru2019-palm-handannotations-export.jsonl","")]]]])
    , (simpleTest "/doc/touhou/2013-c85-download.json", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "JSON"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","txt"),("link-icon-type","svg")]) [Code ("",[],[]) "/doc/touhou/2013-c85-download.json"] ("/doc/touhou/2013-c85-download.json","")]]]])
    , (simpleTest "/doc/personal/rss-subscriptions.opml", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "OPML"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","txt"),("link-icon-type","svg")]) [Code ("",[],[]) "/doc/personal/rss-subscriptions.opml"] ("/doc/personal/rss-subscriptions.opml","")]]]])
    , (simpleTest "/doc/psychology/spaced-repetition/gwern-forgetting-curves.svg", [Div ("",["aux-links-transclude-file"],[]) [Para [Link ("",["link-annotated-not","include-content","width-full"],[]) [Code ("",[],[]) "/doc/psychology/spaced-repetition/gwern-forgetting-curves.svg"] ("/doc/psychology/spaced-repetition/gwern-forgetting-curves.svg","")]]])
    , (simpleTest "/doc/zeo/2018-01-04-zeo-zma.csv", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "CSV"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","csv"),("link-icon-type","svg")]) [Code ("",[],[]) "/doc/zeo/2018-01-04-zeo-zma.csv"] ("/doc/zeo/2018-01-04-zeo-zma.csv","")]]]])
    , (simpleTest "/doc/existential-risk/2011-05-10-givewell-holdenkarnofskyjaantallinn.doc", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "Word document"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","word-doc"),("link-icon-type","svg"),("link-icon-color","#277dd4")]) [Code ("",[],[]) "/doc/existential-risk/2011-05-10-givewell-holdenkarnofskyjaantallinn.doc"] ("/doc/existential-risk/2011-05-10-givewell-holdenkarnofskyjaantallinn.doc","")]]]])
    , (simpleTest "/doc/ai/music/2019-10-23-gwern-gpt2-folkrnn-irishmusic-samples.txt", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "text"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","txt"),("link-icon-type","svg")]) [Code ("",[],[]) "/doc/ai/music/2019-10-23-gwern-gpt2-folkrnn-irishmusic-samples.txt"] ("/doc/ai/music/2019-10-23-gwern-gpt2-folkrnn-irishmusic-samples.txt","")]]]])
    , (simpleTest "/doc/personal/google-cse.xml", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "XML"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","alphabet"),("link-icon-type","svg"),("link-icon-color","#4285f4")]) [Code ("",[],[]) "/doc/personal/google-cse.xml"] ("/doc/personal/google-cse.xml","")]]]])
    , (simpleTest "/doc/darknet-market/2013-05-05-moore-bitcoinexchangesurvivalanalysis.R", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "R code"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","code"),("link-icon-type","svg"),("link-icon-color","#1b61b1")]) [Code ("",[],[]) "/doc/darknet-market/2013-05-05-moore-bitcoinexchangesurvivalanalysis.R"] ("/doc/darknet-market/2013-05-05-moore-bitcoinexchangesurvivalanalysis.R","")]]]])
    , (simpleTest "/static/css/default.css", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "CSS"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","code"),("link-icon-type","svg"),("link-icon-color","#2465f1")]) [Code ("",[],[]) "/static/css/default.css"] ("/static/css/default.css","")]]]])
    , (simpleTest "/haskell/mnemo4.hs", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "Haskell"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","code"),("link-icon-type","svg"),("link-icon-color","#5e5086")]) [Code ("",[],[]) "/haskell/mnemo4.hs"] ("/haskell/mnemo4.hs","")]]]])
    , (simpleTest "/static/js/Hyphenopoly.js", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "JavaScript"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","code"),("link-icon-type","svg"),("link-icon-color","#f6da19")]) [Code ("",[],[]) "/static/js/Hyphenopoly.js"] ("/static/js/Hyphenopoly.js","")]]]])
    , (simpleTest "/doc/psychology/2023-kekecs-supplement-rsos191375_si_001.docx", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "Word document"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","word-doc"),("link-icon-type","svg"),("link-icon-color","#277dd4")]) [Code ("",[],[]) "/doc/psychology/2023-kekecs-supplement-rsos191375_si_001.docx"] ("/doc/psychology/2023-kekecs-supplement-rsos191375_si_001.docx","")]]]])
    , (simpleTest "/doc/psychology/willpower/2019-01-21-eric-socksurvey.ods", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "OpenOffice spreadsheet"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","spreadsheet"),("link-icon-type","svg"),("link-icon-color","#0586ce")]) [Code ("",[],[]) "/doc/psychology/willpower/2019-01-21-eric-socksurvey.ods"] ("/doc/psychology/willpower/2019-01-21-eric-socksurvey.ods","")]]]])
    , (simpleTest "/doc/dual-n-back/2012-05-30-kundu-dnbrapm.xls", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "spreadsheet"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","spreadsheet"),("link-icon-type","svg"),("link-icon-color","#1ba566")]) [Code ("",[],[]) "/doc/dual-n-back/2012-05-30-kundu-dnbrapm.xls"] ("/doc/dual-n-back/2012-05-30-kundu-dnbrapm.xls","")]]]])
    , (simpleTest "/doc/genetics/heritable/2015-polderman-supplement-2.xlsx", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "spreadsheet"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","spreadsheet"),("link-icon-type","svg"),("link-icon-color","#1ba566")]) [Code ("",[],[]) "/doc/genetics/heritable/2015-polderman-supplement-2.xlsx"] ("/doc/genetics/heritable/2015-polderman-supplement-2.xlsx","")]]]])
    , (simpleTest "/doc/ai/music/2019-12-22-gpt2-preferencelearning-gwern-abcmusic.patch", [Div ("",["aux-links-transclude-file"],[]) [Div ("",["collapse"],[]) [Para [Strong [Str "View ",Str "patch"],Str ":"],Para [Link ("",["id-not","link-annotated-not","include-content","include-lazy"],[("link-icon","code"),("link-icon-type","svg")]) [Code ("",[],[]) "/doc/ai/music/2019-12-22-gpt2-preferencelearning-gwern-abcmusic.patch"] ("/doc/ai/music/2019-12-22-gpt2-preferencelearning-gwern-abcmusic.patch","")]]]])
    ]
  where
        fromJustWithError :: (Show k, Ord k) => k -> Maybe v -> v
        fromJustWithError key maybeVal =
          fromMaybe (error $ "fromJust: Key not found: " ++ show key) maybeVal
