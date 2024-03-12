{-# LANGUAGE OverloadedStrings #-}
module Config.Misc where

import Data.Time.Calendar (toModifiedJulianDay, toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T (head, takeWhile, Text)
import System.Directory (setCurrentDirectory)
import System.IO.Unsafe (unsafePerformIO)

import Text.Pandoc.Definition (Inline(Link, Span, Str),
                               Block(Div, Header, Para))

import Utils (anyInfixT, anyPrefixT)

root :: FilePath
root = "/home/gwern/wiki/"

cd :: IO ()
cd = setCurrentDirectory root

currentYear :: Int
currentYear = unsafePerformIO $ fmap ((\(year,_,_) -> fromInteger year) . toGregorian . utctDay) Data.Time.Clock.getCurrentTime -- 2024

currentDay :: IO Integer
currentDay = fmap (toModifiedJulianDay . utctDay) Data.Time.Clock.getCurrentTime

currentDayString :: IO String
currentDayString = fmap (formatTime defaultTimeLocale "%Y-%m-%d") Data.Time.Clock.getCurrentTime

-- for Columns.hs:
listLengthMaxN :: Int
listLengthMaxN = 75
-- sublistsLengthMinN :: Int -- TODO: dead config variable?
-- sublistsLengthMinN = 8

-- LinkBacklinks:
sectionizeWhiteList :: [T.Text]
sectionizeWhiteList = ["/danbooru2021#danbooru2018", "/danbooru2021#danbooru2019", "/danbooru2021#danbooru2020"]
sectionizeMinN :: Int
sectionizeMinN = 3

-- generateBacklinks.hs:
backlinkBlackList :: T.Text -> Bool
backlinkBlackList "" = error "generateBacklinks.hs (Config.Misc): backlinkBlackList: Called with an empty string! This should never happen."
backlinkBlackList e
  | anyInfixT f ["/backlink/", "/link-bibliography/", "/similar/", "wikipedia.org/wiki/"] = True
  | anyPrefixT f ["$", "#", "!", "mailto:", "irc://", "\8383", "/doc/www/"] = True
                   -- WARNING: do not filter out 'metadata/annotation' because that leads to empty databases & infinite loops
  | otherwise = False
  where f = if T.head e == '#' then e else T.takeWhile (/= '#') e -- drop anchors to avoid spurious mismatches eg. '/index#backlink-id-of-some-sort' would bypass a mere '"/index" `isSuffixOf`' check without this.

-- generateLinkbibliography.hs:
-- don't waste the user's time if the annotation is not heavily linked, as most are not, or if all the links are WP links:
mininumLinkBibliographyFragment :: Int
mininumLinkBibliographyFragment = 3

userAgent :: String
userAgent = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:110.0) Gecko/20100101 Firefox/112.0"

-- testing: unique keys
tooltipToMetadataTestcases :: [((String,String),(String,String,String))]
tooltipToMetadataTestcases =
    [(("","‘Title1 Title2's First Word Title3’, Foo et al 2020a"),    ("Title1 Title2's First Word Title3","Foo, et al","2020"))
      , (("","‘Title1 Title2’s First Word Title3’, Foo et al 2020a"), ("Title1 Title2’s First Word Title3","Foo, et al","2020"))
      , (("","'Title1 Title2’s First Word Title3', Foo et al 2020a"), ("Title1 Title2’s First Word Title3","Foo, et al","2020"))
      , (("","“Title1 Title2's First Word Title3”, Foo et al 2020a"), ("Title1 Title2's First Word Title3","Foo, et al","2020"))
      , (("","'Title1 Title2's First Word Title3', Foo & Bar 2020a"), ("Title1 Title2's First Word Title3","Foo, Bar","2020"))
      , (("","'Title1 Title2's First Word Title3', Foo 2020a"),       ("Title1 Title2's First Word Title3","Foo","2020"))
      , (("","'Title1 Title2's First Word Title3', John Smith 2020"), ("Title1 Title2's First Word Title3","John Smith","2020"))
      , (("","'Montaillou: The Promised Land of Error: chapter 2, the <em>domus</em>', Le Roy Ladurie 1978"), ("Montaillou: The Promised Land of Error: chapter 2, the <em>domus</em>", "Le Roy Ladurie", "1978"))
      , (("","'Meta-meta-blinker', Adam P. Goucher 2016-12-15"), ("Meta-meta-blinker", "Adam P. Goucher", "2016-12-15"))
      , (("","'Formal Theory of Creativity & Fun & Intrinsic Motivation (1990-2010)', Jurgen Schmidhuber 2010"), ("Formal Theory of Creativity & Fun & Intrinsic Motivation (1990-2010)", "Jurgen Schmidhuber", "2010"))
      , (("", "$5"),      ("","",""))
      , (("", "$20, 2g"), ("","",""))
      , (("","!W"),       ("","",""))
      , (("","₿20"),      ("","",""))
      , (("","'LaMDA: Language Models for Dialog Applications', Thoppilan?et?al?2022 (Original URL: https://arxiv.org/abs/2201.08239#google )"), ("","",""))
      , (("","'A', John Smith 2020"), ("","John Smith","2020"))
      , (("","klynch 2011"),     ("","","2011"))
      , (("","Foo 2020"),        ("", "Foo", "2020"))
      , (("","Foo 2020-06-12"),  ("", "Foo", "2020-06-12"))
      , (("","John Smith 2020"), ("", "John Smith", "2020"))
      , (("/doc/cs/css/2007-adobe-parametersforopeningpdffiles.pdf#page=5","Parameters for Opening PDF Files: You can open a PDF document with a command or URL that specifies exactly what to display (a named destination or specific page), and how to display it (using such characteristics as a specific view, scrollbars, bookmarks, annotations, or highlighting)"), ("Parameters for Opening PDF Files: You can open a PDF document with a command or URL that specifies exactly what to display (a named destination or specific page), and how to display it (using such characteristics as a specific view, scrollbars, bookmarks, annotations, or highlighting)", "", ""))
      ]

arrowUp, arrowDown :: T.Text
arrowUp = "arrow-up"
arrowDown = "arrow-down"
arrowUpKV, arrowDownKV :: [(T.Text,T.Text)]
arrowUpKV = [("link-icon", arrowUp), ("link-icon-type", "svg")]
arrowDownKV = [("link-icon", arrowDown), ("link-icon-type", "svg")]

-- testing: unique all
arrowTestCases :: [([Block], [Block])]
arrowTestCases =
      [([Para [Link ("", [], []) [Str "simpleCase"] ("#target", "")]],
        [Para [Link ("", [], arrowDownKV) [Str "simpleCase"] ("#target", "")]])
      , ([Para [Link ("", [], []) [Str "sameBlockCase"] ("#target", ""), Span ("target", [], []) [Str "span"]]],
         [Para [Link ("", [], arrowDownKV) [Str "sameBlockCase"] ("#target", ""), Span ("target", [], []) [Str "span"]]])
      , ([Para [Link ("", [], []) [Str "differentBlockCase"] ("#target", "")], Para [Span ("target", [], []) [Str "span"]]],
         [Para [Link ("", [], arrowDownKV) [Str "differentBlockCase"] ("#target", "")], Para [Span ("target", [], []) [Str "span"]]])
      , ([Para [Link ("", [], []) [Str "nestedCase"] ("#target", "")], Div ("", [], []) [Para [Span ("target", [], []) [Str "span"]]]],
          [Para [Link ("", [], arrowDownKV) [Str "nestedCase"] ("#target", "")], Div ("", [], []) [Para [Span ("target", [], []) [Str "span"]]]])
      , ([Para [Link ("", [], []) [Str "headerCase"] ("#target", "")], Header 1 ("target", [], []) [Str "header"]],
         [Para [Link ("", [], arrowDownKV) [Str "headerCase"] ("#target", "")], Header 1 ("target", [], []) [Str "header"]])
      , ([Para [Span ("target", [], []) [Str "span"]], Para [Link ("", [], []) [Str "beforeLinkCase"] ("#target", "")]],
         [Para [Span ("target", [], []) [Str "span"]], Para [Link ("", [], arrowUpKV) [Str "beforeLinkCase"] ("#target", "")]])
      , ([Para [Link ("", [], []) [Str "multipleLinksCase"] ("#target", ""), Link ("", [], []) [Str "link2"] ("#target", "")], Para [Span ("target", [], []) [Str "span"]]],
          [Para [Link ("", [], arrowDownKV) [Str "multipleLinksCase"] ("#target", ""), Link ("", [], arrowDownKV) [Str "link2"] ("#target", "")], Para [Span ("target", [], []) [Str "span"]]])
      , ([Para [Link ("", [], []) [Str "multipleTargetsCase"] ("#target1", ""), Link ("", [], []) [Str "link2"] ("#target2", "")], Para [Span ("target1", [], []) [Str "span1"], Span ("target2", [], []) [Str "span2"]]],
         [Para [Link ("", [], arrowDownKV) [Str "multipleTargetsCase"] ("#target1", ""), Link ("", [], arrowDownKV) [Str "link2"] ("#target2", "")], Para [Span ("target1", [], []) [Str "span1"], Span ("target2", [], []) [Str "span2"]]])
      , ([Para [Link ("", [], []) [Str "nonExistentTargetCase"] ("#nonexistent", "")]],
         [Para [Link ("", [], arrowDownKV) [Str "nonExistentTargetCase"] ("#nonexistent", "")]])
      , ([Para [Str "noLinksCase: no links or targets here"]],
         [Para [Str "noLinksCase: no links or targets here"]])
      , ([Div ("", [], []) [Para [Span ("target", [], []) [Str "span"]]], Para [Link ("", [], []) [Str "beforeLinkNestedCase"] ("#target", "")]],
         [Div ("", [], []) [Para [Span ("target", [], []) [Str "span"]]], Para [Link ("", [], arrowUpKV) [Str "beforeLinkNestedCase"] ("#target", "")]])
      , ([Header 1 ("target", [], []) [Str "header"], Para [Link ("", [], []) [Str "beforeLinkHeaderCase"] ("#target", "")]],
         [Header 1 ("target", [], []) [Str "header"], Para [Link ("", [], arrowUpKV) [Str "beforeLinkHeaderCase"] ("#target", "")]])
      , ([Para [Span ("target1", [], []) [Str "span1"], Span ("target2", [], []) [Str "span2"]], Para [Link ("", [], []) [Str "multipleTargetsWithBeforeLinksCase"] ("#target1", ""), Link ("", [], []) [Str "link2"] ("#target2", "")]],
         [Para [Span ("target1", [], []) [Str "span1"], Span ("target2", [], []) [Str "span2"]], Para [Link ("", [], arrowUpKV) [Str "multipleTargetsWithBeforeLinksCase"] ("#target1", ""), Link ("", [], arrowUpKV) [Str "link2"] ("#target2", "")]])
      , ([Para [Span ("target1", [], []) [Str "span1"]], Para [Link ("", [], []) [Str "beforeAfterMixedCase"] ("#target1", ""), Link ("", [], []) [Str "link2"] ("#target2", "")], Para [Span ("target2", [], []) [Str "span2"]]],
         [Para [Span ("target1", [], []) [Str "span1"]], Para [Link ("", [], arrowUpKV) [Str "beforeAfterMixedCase"] ("#target1", ""), Link ("", [], arrowDownKV) [Str "link2"] ("#target2", "")], Para [Span ("target2", [], []) [Str "span2"]]])
      , ([Para [Link ("", [], []) [Str "mixedLinkOrderCase"] ("#target", ""), Link ("", [], []) [Str "link2"] ("#target", "")], Para [Span ("target", [], []) [Str "span"]], Para [Link ("", [], []) [Str "link3"] ("#target", "")]],
         [Para [Link ("", [], arrowDownKV) [Str "mixedLinkOrderCase"] ("#target", ""), Link ("", [], arrowDownKV) [Str "link2"] ("#target", "")], Para [Span ("target", [], []) [Str "span"]], Para [Link ("", [], arrowUpKV) [Str "link3"] ("#target", "")]])
      , ([Div ("", [], []) [Para [Link ("", [], []) [Str "sameDivBlockCase"] ("#target", "")], Para [Span ("target", [], []) [Str "span"]]]],
         [Div ("", [], []) [Para [Link ("", [], arrowDownKV) [Str "sameDivBlockCase"] ("#target", "")], Para [Span ("target", [], []) [Str "span"]]]])
      , ([Para [Link ("", [], []) [Str "simpleCase"] ("#top", "")]],
         [Para [Link ("", [], arrowUpKV) [Str "simpleCase"] ("#top", "")]])
      ]

-- testing: unique keys
cycleTestCases :: [([(Int, Int)], Bool)]
cycleTestCases = [ ([], False) -- no rules, no cycles
     , ([(1, 2)], False) -- one rule, no cycles
     , ([(1, 1)], True), ([(1, 2), (2, 3), (3, 4), (5, 5)], True), ([(1, 2), (2, 3), (4, 4), (5, 6)], True) -- self loop
     , ([(1, 2), (2, 3), (3, 4)], False) -- rules with no cycles
     , ([(1, 2), (2, 1)], True) -- simple cycle
     , ([(1, 2), (2, 3), (3, 1)], True) -- cycle with more than 2 nodes: where there is a cycle of nodes that all point to one another, but no node points to itself
     , ([(1, 2), (2, 3), (3, 4), (4, 1)], True) -- larger cycle
     , ([(1, 2), (2, 1), (3, 4), (4, 3), (5, 6), (6, 5)], True) -- Multiple disjoint cycles within a larger rule set
     , ([(1, 2), (1, 3), (2, 4), (2, 5), (3, 6), (3, 7)], False)
     , ([(1, 2), (2, 3), (4, 5), (5, 6)], False) -- separate set of rules, no cycles
     , ([(1, 2), (2, 3), (3, 1), (4, 5), (5, 6), (6, 4)], True) -- separate set of rules with cycles
     , ([(1, 2), (2, 3), (3, 2), (4, 5), (5, 4)], True) -- there is a cycle within subset of rules
     , ([(1, 2), (3, 4), (5, 6)], False) -- separate set of rules, no cycles
     , ([(1, 2), (1, 2), (2, 3), (2, 3)], False) -- repetition
     , ([(1, 2), (1, 3), (2, 4), (3, 4)], False) -- Multiple paths to the same node, but no cycles
     , ([(1, 2), (1, 3), (2, 4), (3, 4), (4, 1)], True) -- where there are multiple paths leading to a node that is part of a cycle.
     , ([(1, 1), (2, 2), (3, 3)], True) --where every node in the list points to itself (simple loop for every node)
     ]

-- convert a file extension like 'webm' to a human-readable name like 'WebM' (not always simply an upcase).
fileExtensionToEnglish :: String -> String
fileExtensionToEnglish ext = case lookup (takeWhile (/= '#') ext) extensionMapping of
                               Just name -> name
                               Nothing   -> ""
  where extensionMapping = map (\(a,b) -> ("."++a,b)) $ [("json", "JSON"), ("jsonl", "JSON Lines"), ("opml", "OPML"), ("md", "Markdown")
                           , ("pdf", "PDF"), ("txt", "text"), ("xml", "XML"), ("R", "R code"), ("css", "CSS")
                           , ("hs", "Haskell"), ("js", "Javascript"), ("patch", "patch"), ("sh", "Bash")
                           , ("php", "PHP"), ("conf", "configuration"), ("mp3", "MP3"), ("webm", "WebM")
                           , ("mp4", "MP4"), ("bmp", "bitmap"), ("gif", "GIF"), ("ico", "icon"), ("jpg", "JPG")
                           , ("png", "PNG"), ("svg", "SVG"), ("xcf", "XCF (GIMP)"), ("html", "HTML")
                           , ("csv", "CSV")
                           , ("dat", "data archive"), ("doc", "Word document"), ("docx", "Word document"), ("el", "Elisp")
                           , ("epub", "Epub"), ("ebt", "EBT document"), ("avi", "AVI video"), ("mkv", "video"),
                             ("gtx", "GTX text file"), ("htm", "HTML"), ("maff", "HTML archive"), ("mht", "HTML archive"),
                             ("ods", "OpenOffice spreadsheet"), ("odt", "OpenOffice doc")
                           , ("psd", "Photoshop"), ("py", "Python"), ("swf", "Flash"), ("tar", "tar archive"), ("tmpl", "HTML template")
                           , ("wasm", "WASM"), ("webp", "WebP"), ("otf", "font"), ("ttf", "font")
                           , ("woff", "font"), ("woff2", "font"), ("eot", "font")
                           , ("xls", "spreadsheet"), ("xlsx", "spreadsheet")
                           , ("xz", "XZ archive"), ("zip", "ZIP")
                           ]

-- What is a 'dangerous' file size? At what megabyte size should we warn readers about a file before they download it, eg. by uncollapsing & loading it? We want to avoid those silly warnings like 'PDF (warning: 0.11MB)', since no one is ever going to decide to *not* read an interesting paper if it's only a few MBs. And many webpages today think nothing of loading 10MB+ of assets, and no one demands warnings for those. So the pain point these days seems >10MB. We'll try >15MB for now.
minFileSizeWarning :: Int
minFileSizeWarning = 15

-- how long should a URL's annotation's abstract be if we consider it worth marking as 'annotated' and thus showing the reader & transcluding it etc?
minimumAnnotationLength :: Int
minimumAnnotationLength = 250
