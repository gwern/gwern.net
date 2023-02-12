module Annotation (linkDispatcher, tooltipToMetadata, tooltipToMetadataTest) where

import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T (unpack)
import Text.Pandoc (Inline(Link))
import Network.HTTP (urlDecode)

import Annotation.Biorxiv (biorxiv)
import Annotation.Gwernnet (gwern)
import Annotation.PDF (pdf)
import Annotation.Pubmed (pubmed)
import Annotation.OpenReview (openreview)
import Annotation.Arxiv (arxiv)
import LinkMetadataTypes (Failure(..), MetadataItem, Path)
import Utils (replace, trimTitle, cleanAbstractsHTML, pageNumberParse, sed, filterMeta, anyInfix, anyPrefix, linkCanonicalize)

linkDispatcher :: Inline -> IO (Either Failure (Path, MetadataItem))
linkDispatcher (Link _ _ (l, tooltip)) = do l' <- linkDispatcherURL (T.unpack l)
                                            case l' of
                                              Right _ -> return l'
                                              Left Permanent -> let (title,author,date) = tooltipToMetadata (T.unpack l) (T.unpack tooltip) in
                                                                  if title/="" then return (Right ((T.unpack l),(title,author,date,"",[],""))) else return l'
                                              Left Temporary -> return l'
linkDispatcher x = error ("linkDispatcher passed a non-Link Inline element: " ++ show x)

linkDispatcherURL :: Path -> IO (Either Failure (Path, MetadataItem))
linkDispatcherURL l | anyPrefix l ["/metadata/annotation/backlink/", "/metadata/annotation/similar/", "/doc/www/"] = return (Left Permanent)
                 -- WP is now handled by annotations.js calling the Mobile WP API; we pretty up the title for tags.
                 | "https://en.wikipedia.org/wiki/" `isPrefixOf` l = return $ Right (l, (wikipediaURLToTitle l, "", "", "", [], ""))
                 | "https://arxiv.org/abs/" `isPrefixOf` l = arxiv l
                 | "https://openreview.net/forum?id=" `isPrefixOf` l || "https://openreview.net/pdf?id=" `isPrefixOf` l = openreview l
                 | anyPrefix l ["https://www.biorxiv.org/content/", "https://www.medrxiv.org/content/"] = biorxiv l
                 | "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC" `isPrefixOf` l = pubmed l
                     -- WARNING: this is not a complete list of PLOS domains, just the ones currently used on Gwern.net; didn't see a complete list anywhere...
                 | anyInfix l ["journals.plos.org", "plosbiology.org", "ploscompbiology.org", "plosgenetics.org", "plosmedicine.org", "plosone.org"] = pubmed l
                 | null l = return (Left Permanent)
                 -- locally-hosted PDF?
                 | ".pdf" `isInfixOf` l = let l' = linkCanonicalize l in if head l' == '/' then pdf $ tail l' else return (Left Permanent)
                 | otherwise = let l' = linkCanonicalize l in if head l' == '/' then gwern $ tail l
                 -- And everything else is unhandled:
                    else return (Left Permanent)


-- Attempt to parse tooltips back into citation metadata:
tooltipToMetadata :: String -> String -> (String,String,String)
tooltipToMetadata _ "" = ("","","")
tooltipToMetadata path s
                    | head s `elem` ['/', '!', '$', '\8383'] || anyInfix s ["Original URL:"] = ("","","")
                    | otherwise =
                        let title  = filterMeta $ sed "['‘“](.*)['’”], .*" "\\1" s
                            author = filterMeta $  sed "^Branwen$" "Gwern Branwen" $ replace " et al" ", et al" $ replace " & " ", " $
                                     sed "^([A-Z].*) [0-9][0-9][0-9][0-9][0-9-]*[a-z]?$" "\\1" $
                                     sed "['‘“].*['’”], ([A-Z].+) [0-9][0-9][0-9][0-9][0-9-]*[a-z]?" "\\1" s
                            date   = filterMeta $ sed "^[A-Za-z].+ ([0-9][0-9][0-9][0-9][0-9-]*)[a-z]?$" "\\1" $ sed "['‘“].+['’”], [A-Za-z].+ ([0-9][0-9][0-9][0-9][0-9-]*)[a-z]?$" "\\1" s
                            pageNumber = let n = pageNumberParse path in if null n then "" else " § pg" ++ n
                            (t,a,d) = (minLength 6 (changed title) ++ pageNumber, changed author, minLength 4 $ changed date) in
                          if a==d then (s,"","") else (t,a,d)
                    where changed x = if s==x then "" else x
                          minLength n x = if length x < n then "" else x
tooltipToMetadataTest :: [((String,String),(String,String,String))]
tooltipToMetadataTest = filter (\((t1, t2), goodResult) -> tooltipToMetadata t1 t2 /= goodResult)
    [(("","‘Title1 Title2's First Word Title3’, Foo et al 2020a"),    ("Title1 Title2's First Word Title3","Foo, et al","2020"))
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
      , (("https://pdfobject.com/pdf/pdf_open_parameters_acro8.pdf#page=5","Parameters for Opening PDF Files: You can open a PDF document with a command or URL that specifies exactly what to display (a named destination or specific page), and how to display it (using such characteristics as a specific view, scrollbars, bookmarks, annotations, or highlighting)"), ("Parameters for Opening PDF Files: You can open a PDF document with a command or URL that specifies exactly what to display (a named destination or specific page), and how to display it (using such characteristics as a specific view, scrollbars, bookmarks, annotations, or highlighting)", "", ""))
      ]

wikipediaURLToTitle :: String -> String
wikipediaURLToTitle u = trimTitle $ cleanAbstractsHTML $ replace "#" " § " $ urlDecode $ replace "% " "%25 " $ replace "_" " " $ replace "https://en.wikipedia.org/wiki/" "" u
