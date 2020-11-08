{- LinkMetadata.hs: module for generating Pandoc links which are annotated with metadata, which can then be displayed to the user as 'popups' by /static/js/popups.js. These popups can be excerpts, abstracts, article introductions etc, and make life much more pleasant for the reader - hover over link, popup, read, decide whether to go to link.
Author: Gwern Branwen
Date: 2019-08-20
When:  Time-stamp: "2020-11-08 18:39:13 gwern"
License: CC-0
-}

-- TODO:
-- 1. fix Unicode handling: `shellToCommand` seems to mangle Unicode, screwing up abstracts
-- 2. scrape more sites: possibilities include  predictionbook.com, amazon.com, nature.com, longbets.org, wiley.com, bmj.com, cran.r-project.org, and rand.org
-- 3. bugs in packages: the WMF API omits the need for `-L` in curl but somehow their live demo works anyway (?!); rxvist doesn't appear to support all bioRxiv/medRxiv schemas, including the '/early/' links, forcing me to use curl+Tagsoup; the R library 'fulltext' crashes on examples like `ft_abstract(x = c("10.1038/s41588-018-0183-z"))`

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module LinkMetadata where

import Control.Monad(when)
import qualified Data.ByteString as B (appendFile)
import qualified Data.ByteString.Lazy as BL (length)
import qualified Data.ByteString.Lazy.UTF8 as U (toString) -- (encode, decode) -- TODO: why doesn't using U.toString fix the Unicode problems?
import Data.Aeson (eitherDecode, FromJSON, Object, Value(String))
import qualified Data.HashMap.Strict as HM (lookup)
import GHC.Generics (Generic)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub, sort, (\\))
import Data.Char (isAlpha, isSpace, toLower, toUpper)
import qualified Data.Map.Strict as M (fromList, lookup, union, Map)
import Text.Pandoc (readerExtensions, writerWrapText, writerHTMLMathMethod, Inline(Link, Span),
                    HTMLMathMethod(MathJax), defaultMathJaxURL, def, readLaTeX, writeHtml5String,
                    WrapOption(WrapNone), runPure, pandocExtensions, readHtml, writePlain)
import qualified Data.Text as T (head, length, unpack, pack, Text)
import Data.FileStore.Utils (runShellCommand)
import System.Exit (ExitCode(ExitFailure))
import Data.List.Utils (replace, split, uniq)
import Network.Api.Arxiv hiding (Link)
import Text.HTML.TagSoup (parseTags, isTagOpenName, renderTags, Tag(TagClose, TagOpen))
import Data.Yaml as Y (decodeFileEither, encode, ParseException)
import Data.Time.Clock as TC (getCurrentTime)
import Text.Regex (subRegex, mkRegex)
import Data.Maybe (Maybe)

import Typography

type Metadata = M.Map Path MetadataItem -- (Title, Author, Date, DOI, Abstract)
type MetadataItem = (String, String, String, String, String)
type MetadataList = [(Path, MetadataItem)]
type Path = String

readLinkMetadata :: IO Metadata
readLinkMetadata = do
             -- for hand created definitions, to be saved; since it's handwritten and we need line errors, we use YAML:
             custom <- readYaml "metadata/custom.yaml"

             -- Quality checks:
             -- - URLs, titles & annotations should all be unique, although author/date/DOI needn't be
             let urls = map (\(u,_) -> u) custom
             when (length (uniq (sort urls)) /=  length urls) $ error $ "Duplicate URLs in 'custom.yaml'!" ++ unlines (urls \\ nub urls)
             let brokenUrls = filter (\u -> not (head u == 'h' || head u == '/' || head u == '?')) urls in when (brokenUrls /= []) $ error $ "Broken URLs in 'custom.yaml'!" ++ unlines brokenUrls
             let titles = map (\(_,(t,_,_,_,_)) -> t) custom in when (length (uniq (sort titles)) /=  length titles) $ error $ "Duplicate titles in 'custom.yaml'!" ++ unlines (titles \\ nub titles)
             let annotations = map (\(_,(_,_,_,_,s)) -> s) custom in when (length (uniq (sort annotations)) /= length annotations) $ error $ "Duplicate annotations in 'custom.yaml'!" ++ unlines (annotations \\ nub annotations)
             -- - DOIs are optional since they usually don't exist, and dates are optional for always-updated things like WP; but everything else should:
             let emptyCheck = filter (\(u,(t,a,_,_,s)) -> any (=="") [u,t,a,s]) custom
             when (length emptyCheck /= 0) $ error $ "Link Annotation Error: empty mandatory fields! This should never happen: " ++ show emptyCheck

             -- auto-generated cached definitions; can be deleted if gone stale
             auto <- readYaml "metadata/auto.yaml"

             -- merge the hand-written & auto-generated link annotations, and return:
             return $ M.union (M.fromList custom) (M.fromList auto) -- left-biased, 'custom' overrides 'auto'

readYaml :: Path -> IO MetadataList
readYaml yaml = do file <- Y.decodeFileEither yaml :: IO (Either ParseException [[String]])
                   case file of
                     Left e -> error $ show e
                     Right y -> (return $ concatMap convertListToMetadata y) :: IO MetadataList
                where
                 convertListToMetadata :: [String] -> MetadataList
                 convertListToMetadata [u, t, a, d, di, s] = [(u, (t,a,d,di,s))]
                 convertListToMetadata e@_ = error $ "Pattern-match failed (too few fields?): " ++ show e

-- append a new automatic annotation if its Path is not already in the auto database:
writeLinkMetadata :: Path -> MetadataItem -> IO ()
writeLinkMetadata l i@(t,a,d,di,abst) = do auto <- readYaml "metadata/auto.yaml"
                                           when (not (l `elem` (map fst auto))) $ do
                                             print i
                                             let newYaml = Y.encode [(l,t,a,d,di,abst)]
                                             B.appendFile "metadata/auto.yaml" newYaml

annotateLink :: Metadata -> Inline -> IO Inline
-- Relevant Pandoc types: Link = Link Attr [Inline] Target
--                        Attr = (String, [String], [(String, String)])
--                        Target = (String, String)
annotateLink md x@(Link _ _ (target, _)) =
  do
     -- normalize: convert 'https://www.gwern.net/docs/foo.pdf' to '/docs/foo.pdf' and './docs/foo.pdf' to '/docs/foo.pdf'
     -- the leading '/' indicates this is a local gwern.net file
     let target' = replace "https://www.gwern.net/" "/" (T.unpack target)
     let target'' = if head target' == '.' then drop 1 target' else target'

     let annotated = M.lookup target'' md
     case annotated of
       -- the link has a valid annotation already defined, so build & return
       Just l  -> return $ constructAnnotation x l
       Nothing -> do new <- linkDispatcher target''
                     case new of
                       -- cache the failures too, so we don't waste time rechecking the PDFs every build:
                       Nothing -> writeLinkMetadata target'' ("", "", "", "", "") >> return x
                       Just (_,m) -> do
                                       writeLinkMetadata target'' m
                                       return $ constructAnnotation x m
annotateLink _ x = return x

constructAnnotation :: Inline -> MetadataItem -> Inline
constructAnnotation x@(Link (lid, classes, pairs) text (target, originalTooltip)) (title, author, date, doi, abstract) =
  if abstract == "" then x else -- if no abstract, don't bother
    let lid' = if lid=="" then generateID (T.unpack target) author date else lid in
    let annotationAttributes = (lid', "docMetadata":classes,
          (filter (\d -> (snd d) /= "") [("popup-title",      T.pack $ htmlToASCII title),
                                         ("popup-title-html", htmlToBetterHTML $ T.pack title),
                                         ("popup-author",     htmlToBetterHTML $ T.pack $ trimAuthors $ initializeAuthors author),
                                         ("popup-date",       T.pack date),
                                         ("popup-doi",        T.pack doi),
                                         ("popup-abstract",   htmlToBetterHTML $ T.pack abstract')
                                         ])++pairs) in
    if T.head target /= '?' then Link annotationAttributes text (target, newTooltip) else
      -- Special in-place annotation definition: `<span data-metadata="Full HTML version" title="ASCII version fallback">original text anchor</span>`
      Span ("", ["defnMetadata"], (third annotationAttributes) ++ [("title", newTooltip)]) text
   where
     abstract', abstractText, possibleTooltip :: String
    -- make sure every abstract is wrapped in paragraph tags for proper rendering:
     abstract' = if (take 3 abstract) == "<p>" then abstract else "<p>" ++ abstract ++ "</p>"
     -- Tooltip rewriting
     -- Progressive enhancement: we create a crude, shortened, ASCII version of the full annotation to use as a regular tooltip, for non-JS users (and possibly bots)
     -- This happens if the existing tooltip is empty; but we *also* override short tooltips (defined as one where the annotation-tooltip is >30% longer than the original tooltip).
     -- Why? Because many tooltips/link-titles are already written in the Markdown sources, like `[foo](/docs/bar.pdf "'On Dancing Angels', Quux 2020")`; these tooltips are important documentation while writing the Markdown page (so you can see at a glance what they are - the *author* can't mouse over them!), but are inferior to the generated tooltips. So if the original tooltip is not particularly long, that suggests it's not a special one (eg a Twitter tweet which has been inlined) and we should override it.
     abstractText = htmlToASCII abstract'
     possibleTooltip = "\""++title++"\", " ++ (trimAuthors author)++", " ++ date ++
                        (if doi /= "" then " (DOI:"++doi++")" else "")
                        ++ "; abstract: \""++(if (length abstractText)>300 then (take 300 abstractText) ++ "…" else abstractText)++"\""
     newTooltip :: T.Text
     newTooltip = if (fromIntegral (length possibleTooltip)::Float) > ((fromIntegral $ T.length originalTooltip)*1.3::Float)
                   then T.pack possibleTooltip else originalTooltip
     third :: (a,b,c) -> c
     third    (_,_,c)  = c
constructAnnotation b c = error $ "Error: a non-Link was passed into 'constructAnnotation'! This should never happen." ++ show b ++ " " ++ show c

-- some author lists are absurdly long; stop at a certain length, finish the author list through the current author (comma-delimited), and leave the rest as 'et al':
trimAuthors, initializeAuthors :: String -> String
trimAuthors a = let maxLength = 64 in if length a < maxLength then a else (take maxLength a) ++ (takeWhile (/=',') (drop maxLength a)) ++ " et al"
initializeAuthors a' = subRegex (mkRegex " ([A-Z]) ") a' " \\1. " -- "John H Smith" → "John H. Smith"

-- so after meditating on it, I think I've decided how duplicate annotation links should be handled:
--
-- 1. all citations like 'Foo & Bar 1990' or 'Quux et al 2020' should be hyperlinked (either as a internal anchor or fulltext link);
-- 2. annotated links get a predictable anchor ID generated from the metadata, like '#foo-et-al-2020' (ie grab the first 4 characters of the date, check
--    the number of commas in the author field to decide if 'foo 1990' or 'foo & bar 1990' or 'foo et al 1990' etc);
-- 3. duplicate links will, then, generate invalid HTML as two Foo et al 2020s (which must be links per #1) will both define id='#foo-et-al-2020', and this will trigger htmltidy errors/warnings on sync; so, one of them will be manually edited to either point to another instance which
--    is part of a larger discussion/context, or be given a manual ID like id='#foo-et-al-2020-2'. (since the annotation is based on the URL not the
--    ID, this doesn't affect the annotations.)
--
-- so, all citations have a hyperlink, supporting hypertextual reading or readers who didn't happen to
-- memorize the previous use in the page, independent instances of links remain independent while back/forward
-- references pop up the relevant section with the annotated link in context, htmltidy automatically detects links that need to be updated, and a
-- regexp can warn about citation-text which needs to be linkified.
generateID :: String -> String -> String -> T.Text
generateID url author date
  -- shikata ga nai:
  | author == "" = ""
  | date   == "" = ""
  -- skip the ubiquitous WP links: I don't repeat WP refs, and the identical author/dates impedes easy cites/links anyway.
  | "https://en.wikipedia.org/wiki/" `isPrefixOf` url = ""
  -- eg '/Faces' = '#gwern-faces'
  | "Gwern Branwen" == author = T.pack (replace "--" "-" $ replace "/" "-" $ replace "#" "-" $ map toLower $ replace "https://" "" $ replace "https://www.gwern.net/" "" $ "gwern-"++url)
  -- 'Foo 2020' → '#foo-2020'; 'Foo & Bar 2020' → '#foo-bar-2020'; 'foo et al 2020' → 'foo-et-al-2020'
  | otherwise = T.pack $ let year = if date=="" then "2020" else take 4 date in -- YYYY-MM-DD
                           let authors = split ", " $ head $ split " (" author in -- handle affiliations like "Tom Smith (Wired)"
                           let authorCount = length authors in
                             if authorCount == 0 then "" else
                               let firstAuthorSurname = filter isAlpha $ reverse $ takeWhile (/=' ') $ reverse $ head authors in
                                 map toLower $ if authorCount >= 3 then
                                                 firstAuthorSurname ++ "-et-al-" ++ year else
                                                   if authorCount == 2 then
                                                     let secondAuthorSurname = filter isAlpha $ reverse $ takeWhile (/=' ') $ reverse $ (authors !! 1) in
                                                       firstAuthorSurname ++ "-" ++ secondAuthorSurname ++ "-" ++ year
                                                   else
                                                     firstAuthorSurname ++ "-" ++ year

-- compile HTML strings to Pandoc's plaintext ASCII outputs (since tooltips can't render HTML like we get from Wikipedia or many hand-written annotations)
htmlToASCII :: String -> String
htmlToASCII input = let cleaned = runPure $ do
                                    html <- readHtml def{ readerExtensions = pandocExtensions } (T.pack input)
                                    txt <- writePlain def{writerWrapText=WrapNone} html
                                    return $ T.unpack txt
              in case cleaned of
                 Left _ -> ""
                 Right output -> trim output

-- clean up abstracts & titles with functions from Typography module: smallcaps & hyphenation (hyphenation is particularly important in popups because of the highly restricted horizontal width).
htmlToBetterHTML :: T.Text -> T.Text
htmlToBetterHTML html = let cleaned = runPure $ do
                                    pandoc <- readHtml def{ readerExtensions = pandocExtensions } html
                                    let pandoc' = typographyTransform pandoc
                                    html' <- writeHtml5String def{writerWrapText=WrapNone,writerHTMLMathMethod = MathJax defaultMathJaxURL} pandoc'
                                    return html'
              in case cleaned of
                 Left _ -> error (T.unpack html)
                 Right output -> T.pack $ trim $ T.unpack output

linkDispatcher, wikipedia, gwern, arxiv, biorxiv, pubmed :: Path -> IO (Maybe (Path, MetadataItem))
linkDispatcher l | "https://en.wikipedia.org/wiki/" `isPrefixOf` l = wikipedia l
                 | "https://arxiv.org/abs/" `isPrefixOf` l = arxiv l
                 | "https://www.biorxiv.org/content/" `isPrefixOf` l = biorxiv l
                 | "https://www.medrxiv.org/content/" `isPrefixOf` l = biorxiv l
                 | "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC" `isPrefixOf` l = pubmed l
                 -- WARNING: this is not a complete list of PLOS domains, just the ones currently used on gwern.net; didn't see a complete list anywhere...
                 | "journals.plos.org" `isInfixOf` l = pubmed l
                 | "plosbiology.org" `isInfixOf` l = pubmed l
                 | "ploscompbiology.org" `isInfixOf` l = pubmed l
                 | "plosgenetics.org" `isInfixOf` l = pubmed l
                 | "plosmedicine.org" `isInfixOf` l = pubmed l
                 | "plosone.org" `isInfixOf` l = pubmed l
                 | "https://www.gwern.net/" `isPrefixOf` l = gwern (drop 22 l)
                 | head l == '/' = gwern (drop 1 l)
                 | head l == '#' = gwern l
                 | otherwise = return Nothing

-- handles both PM & PLOS right now:
pubmed l = do (status,_,mb) <- runShellCommand "./" Nothing "Rscript" ["static/build/linkAbstract.R", l]
              case status of
                ExitFailure err -> (print $ intercalate " : " [l, show status, show err, show mb]) >> return Nothing
                _ -> do let (title:author:date:doi:abstract) = lines $ U.toString mb
                        return $ Just (l, (trim title, initializeAuthors $ trim author, trim date, trim doi, cleanAbstractsHTML (unlines abstract)))

pdf :: Path -> IO (Maybe (Path, MetadataItem))
pdf p = do (_,_,mb) <- runShellCommand "./" Nothing "exiftool" ["-printFormat", "$Title$/$Author$/$Date$/$DOI", "-Title", "-Author", "-Date", "-DOI", p]
           if BL.length mb > 0 then
             do let (etitle:eauthor:edate:edoi:_) = lines $ U.toString mb
                -- PDFs have both a 'Creator' and 'Author' metadata field sometimes. Usually Creator refers to the (single) person who created the specific PDF file in question, and Author refers to the (often many) authors of the content; however, sometimes PDFs will reverse it: 'Author' means the PDF-maker and 'Creators' the writers. If the 'Creator' field is longer than the 'Author' field, then it's a reversed PDF and we want to use that field instead of omitting possibly scores of authors from our annotation.
                (_,_,mb2) <- runShellCommand "./" Nothing "exiftool" ["-printFormat", "$Creator", "-Creator", p]
                let ecreator = U.toString mb2
                let author = initializeAuthors $ trim $ if length eauthor > length ecreator then eauthor else ecreator
                print $ "PDF: " ++ p ++" DOI: " ++ edoi
                aMaybe <- doi2Abstract edoi
                -- if there is no abstract, there's no point in displaying title/author/date since that's already done by tooltip+URL:
                case aMaybe of
                  Nothing -> return Nothing
                  Just a -> return $ Just (p, (trim etitle, author, trim edate, edoi, a))
           else return Nothing

-- nested JSON object: eg 'jq .message.abstract'
data Crossref = Crossref { message :: Message } deriving (Show,Generic)
instance FromJSON Crossref
data Message = Message { abstract :: Maybe String } deriving (Show,Generic)
instance FromJSON Message
doi2Abstract :: [Char] -> IO (Maybe String)
doi2Abstract doi = if length doi <7 then return Nothing
                   else do (_,_,bs) <- runShellCommand "./" Nothing "curl" ["--location", "--silent", "https://api.crossref.org/works/"++doi, "--user-agent", "gwern+crossrefscraping@gwern.net"]
                           if bs=="Resource not found." then return Nothing
                           else let j = eitherDecode bs :: Either String Crossref
                                in case j of -- start unwrapping...
                                    Left e -> putStrLn ("Error: Crossref request failed: "++doi++" "++e) >> return Nothing
                                    Right j' -> let j'' = abstract $ message j' in
                                      case j'' of
                                       Nothing -> return Nothing
                                       Just a -> let trimmedAbstract = cleanAbstractsHTML a
                                                 in return $ Just trimmedAbstract

-- WP REST API: https://en.wikipedia.org/api/rest_v1/#/Page_content/get_page_summary_title
data WP = WP { title :: !String, extract_html :: !String, thumbnail :: Maybe Object } deriving (Show,Generic)
instance FromJSON WP
wikipedia p
  | "https://en.wikipedia.org/wiki/Special" `isPrefixOf` p = return Nothing
  | "https://en.wikipedia.org/wiki/User:" `isPrefixOf` p = return Nothing
  | "https://en.wikipedia.org/wiki/Talk:" `isPrefixOf` p = return Nothing
  | "https://en.wikipedia.org/wiki/Category:" `isPrefixOf` p = return Nothing
  | otherwise = do let p' = replace "/" "%2F" $ replace "%20" "_" $ drop 30 p
                   let p'' = [toUpper (head p')] ++ tail p'
                   let p''' = if '#' `elem` p'' then head $ split "#" p'' else p''
                   -- print p''
                   let rq = "https://en.wikipedia.org/api/rest_v1/page/summary/"++p'''++"?redirect=true"
                   -- `--location` is required or redirects will not be followed by *curl*; '?redirect=true' only makes the *API* follow redirects
                   (status,_,bs) <- runShellCommand "./" Nothing "curl" ["--location", "--silent", rq, "--user-agent", "gwern+wikipediascraping@gwern.net"]
                   when ("\"type\":\"disambiguation\"" `isInfixOf` U.toString bs) $ error ("Linked to a Wikipedia disambiguation page! " ++ p)
                   today <- fmap (take 10 . show) $ TC.getCurrentTime -- create dates like "2020-08-31"
                   case status of
                     ExitFailure _ -> putStrLn ("Wikipedia tooltip failed: " ++ p''') >> return Nothing
                     _ -> let j = eitherDecode bs :: Either String WP
                          in case j of
                               Left e -> putStrLn ("WP request failed: " ++ e ++ " " ++ p ++ " " ++ p''') >> return Nothing
                               Right wp -> let wpTitle = title wp in
                                             let wpAbstract = extract_html wp in
                                               let wpThumbnail = case thumbnail wp of
                                                     Nothing -> ""
                                                     Just thumbnailObject -> case (HM.lookup "source" thumbnailObject) of
                                                                               Nothing -> ""
                                                                               Just (String href) -> "<p><figure><img class=\"float-right\" src=\"" ++ T.unpack href ++ " title=\"Wikipedia thumbnail image of " ++ wpTitle ++ "\"/></figure></p>"
                                                                               Just _ -> ""
                                                         in
                                            return $ Just (p, (wpTitle, "English Wikipedia", today, "", cleanAbstractsHTML wpAbstract ++ wpThumbnail))

-- handles medRxiv too (same codebase)
biorxiv p = do (status,_,bs) <- runShellCommand "./" Nothing "curl" ["--location", "--silent", p, "--user-agent", "gwern+biorxivscraping@gwern.net"]
               case status of
                 ExitFailure _ -> putStrLn ("BioRxiv download failed: " ++ p) >> return Nothing
                 _ -> do
                        let b = U.toString bs
                        let f = parseTags b
                        let metas = filter (isTagOpenName "meta") f
                        let title = concatMap (\(TagOpen _ (a:b)) -> if snd a == "DC.Title" then snd $ head b else "") metas
                        if (title=="") then print ("BioRxiv parsing failed: " ++ p ++ ": " ++ show metas) >> return Nothing
                          else do
                                 let date = concatMap (\(TagOpen _ (a:b)) -> if snd a == "DC.Date" then snd $ head b else "") metas
                                 let author = initializeAuthors $ intercalate ", " $ filter (/="") $ map (\(TagOpen _ (a:b)) -> if snd a == "DC.Contributor" then snd $ head b else "") metas
                                 let doi = concatMap (\(TagOpen _ (a:b)) -> if snd a == "citation_doi" then snd $ head b else "") metas
                                 let abstract = cleanAbstractsHTML $
                                                 concatMap (\(TagOpen _ (a:_:c)) ->
                                                                      if snd a == "citation_abstract" then snd $ head c else "") metas
                                 return $ Just (p, (title, author, date, doi, abstract))


arxiv url = do -- Arxiv direct PDF links are deprecated but sometimes sneak through
               let arxivid = takeWhile (/='#') $ if "/pdf/" `isInfixOf` url && ".pdf" `isSuffixOf` url
                                 then replace "https://arxiv.org/pdf/" "" $ replace ".pdf" "" url
                                 else replace "https://arxiv.org/abs/" "" url
               (status,_,bs) <- runShellCommand "./" Nothing "curl" ["--location","--silent","https://export.arxiv.org/api/query?search_query=id:"++arxivid++"&start=0&max_results=1", "--user-agent", "gwern+arxivscraping@gwern.net"]
               case status of
                 ExitFailure _ -> putStrLn ("Error: curl API call failed on Arxiv ID " ++ arxivid) >> return Nothing
                 _ -> do let tags = parseTags $ U.toString bs
                         let at = replace "\n  " " " $ getTitle $ drop 8 tags
                         if (at=="") then
                           error ("Error: no title on Arxiv ID " ++ arxivid ++ "; result: " ++ U.toString bs) >> return Nothing
                           else
                             do let aau = initializeAuthors $ intercalate ", " $ getAuthorNames tags
                                let ad = take 10 $ getPublished tags
                                let ado = getDoi tags
                                let aa = processArxivAbstract url $ getSummary tags
                                return $ Just (url, (at, aau, ad, ado, aa))

-- Arxiv makes multi-paragraph abstracts hard because the 'HTML' is actually LaTeX, so we need to special Pandoc preprocessing (for paragraph breaks, among other issues):
processArxivAbstract :: String -> String -> String
processArxivAbstract u a = let cleaned = runPure $ do
                                    pandoc <- readLaTeX def{ readerExtensions = pandocExtensions } $ T.pack $ replace "\n  " "\n\n" a
                                    html <- writeHtml5String def{writerWrapText=WrapNone, writerHTMLMathMethod = MathJax defaultMathJaxURL} pandoc
                                    return html
              in case cleaned of
                 Left e -> error $ u ++ " : " ++ show e ++ ": " ++ a
                 Right output -> cleanAbstractsHTML $ T.unpack output

cleanAbstractsHTML :: String -> String
cleanAbstractsHTML t = trim $
  -- regexp substitutions:
  (\s -> subRegex (mkRegex "([0-9]+)x([0-9]+)") s "\\1×\\2") $
  -- simple string substitutions:
  foldr (\(a,b) -> replace a b) t [
    ("<span style=\"font-weight:normal\"> </span>", "")
    , ("<abstract abstract-type=\"summary\"><br/>", "")
    , ("</p><br/>", "</p>")
    , ("</p> <br/>", "</p>")
    , ("  </sec><br/>  ", "")
    , ("<sec><br/>    ", "")
    , ("  </sec> <br/>", "")
    , ("</strong></p>    <p>", "</strong> ")
    , ("</title>", ":</strong></p>")
    , ("<title>", "<p><strong>")
    , ("</title><br/>", "</title>")
    , ("<p>\n\n", "<p>")
    , ("<br></p>", "</p>")
    , ("\n<br />\n", "")
    , ("<br/><p>", "<p>")
    , ("\n", "<br/>")
    , (" -- ", "&mdash;")
    , ("---", "&mdash;")
    , (" - ", "—")
    , ("<strong><strong>", "<strong>")
    , ("</strong></strong>", "</strong>")
    , ("<b>", "<strong>")
    , ("</b>", "</strong>")
    , ("<jats:title>Abstract</jats:title><br/>               ", "")
    , ("</jats:p>", "</p>")
    , ("<jats:p>", "<p>")
    , ("<jats:title>Abstract</jats:title>\n\t  <jats:p>", "")
    , ("<h3>ABSTRACT</h3>", "")
    , ("<h3>Abstract</h3>", "")
    , ("<h3>SUMMARY</h3>", "")
    , ("<abstract>", "")
    , ("<abstract>\n  ", "")
    , ("\n</abstract>", "")
    , ("\nHighlights: ", "\n<strong>Highlights</strong>: ")
    , ("\nBackground: ", "\n<strong>Background</strong>: ")
    , ("\nAbstract: ", "\n<strong>Abstract</strong>: ")
    , ("\nContext: ", "\n<strong>Context</strong>: ")
    , ("\nPurpose: ", "\n<strong>Purpose</strong>: ")
    , ("\nRationale: ", "\n<strong>Rationale</strong>: ")
    , ("\nObjective: ", "\n<strong>Objective</strong>: ")
    , ("\nObjectives: ", "\n<strong>Objectives</strong>: ")
    , ("\nQuestion: ", "\n<strong>Question</strong>: ")
    , ("\nDescription: ", "\n<strong>Description</strong>: ")
    , ("\nDesign: ", "\n<strong>Design</strong>: ")
    , ("\nMethods: ", "\n<strong>Methods</strong>: ")
    , ("\nSetting: ", "\n<strong>Setting</strong>: ")
    , ("\nParticipants: ", "\n<strong>Participants</strong>: ")
    , ("\nMeaning: ", "\n<strong>Meaning</strong>: ")
    , ("\nDesign, Setting, and Participants: ", "\n<strong>Design, Setting, & Participants</strong>: ")
    , ("\nIntervention: ", "\n<strong>Intervention</strong>: ")
    , ("\nData Sources: ", "\n<strong>Data Sources</strong>: ")
    , ("\nMain Outcomes & Measures: ", "\n<strong>Main Outcomes and Measures</strong>: ")
    , ("\nMeasurements: ", "\n<strong>Measurements</strong>: ")
    , ("\nResults: ", "\n<strong>Results</strong>: ")
    , ("\nSignificance: ", "\n<strong>Significance</strong>: ")
    , ("\nConclusion: ", "\n<strong>Conclusion</strong>: ")
    , ("\nConclusions: ", "\n<strong>Conclusion</strong>: ")
    , ("\nConclusions & Relevance: ", "\n<strong>Conclusions and Relevance</strong>: ")
    , ("\nTrial Registration: ", "\n<strong>Trial Registration</strong>: ")
    , ("<h3>Highlights</h3>\n<p>", "<p><strong>Highlights</strong>: ")
    , ("<h3>Background</h3>\n<p>", "<p><strong>Background</strong>: ")
    , ("<h3>Abstract</h3>\n<p>", "<p><strong>Abstract</strong>: ")
    , ("<h3>Context</h3>\n<p>", "<p><strong>Context</strong>: ")
    , ("<h3>Purpose</h3>\n<p>", "<p><strong>Purpose</strong>: ")
    , ("<h3>Rationale</h3>\n<p>", "<p><strong>Rationale</strong>: ")
    , ("<h3>Objective</h3>\n<p>", "<p><strong>Objective</strong>: ")
    , ("<h3>Objectives</h3>\n<p>", "<p><strong>Objectives</strong>: ")
    , ("<h3>Question</h3>\n<p>", "<p><strong>Question</strong>: ")
    , ("<h3>Description</h3>\n<p>", "<p><strong>Description</strong>: ")
    , ("<h3>Design</h3>\n<p>", "<p><strong>Design</strong>: ")
    , ("<h3>Methods</h3>\n<p>", "<p><strong>Methods</strong>: ")
    , ("<h3>Setting</h3>\n<p>", "<p><strong>Setting</strong>: ")
    , ("<h3>Participants</h3>\n<p>", "<p><strong>Participants</strong>: ")
    , ("<h3>Meaning</h3>\n<p>", "<p><strong>Meaning</strong>: ")
    , ("<h3>Design, Setting, and Participants</h3>\n<p>", "<p><strong>Design, Setting, & Participants</strong>: ")
    , ("<h3>Intervention</h3>\n<p>", "<p><strong>Intervention</strong>: ")
    , ("<h3>Data Sources</h3>\n<p>", "<p><strong>Data Sources</strong>: ")
    , ("<h3>Main Outcomes & Measures</h3>\n<p>", "<p><strong>Main Outcomes and Measures</strong>: ")
    , ("<h3>Measurements</h3>\n<p>", "<p><strong>Measurements</strong>: ")
    , ("<h3>Results</h3>\n<p>", "<p><strong>Results</strong>: ")
    , ("<h3>Significance</h3>\n<p>", "<p><strong>Significance</strong>: ")
    , ("<h3>Conclusion</h3>\n<p>", "<p><strong>Conclusion</strong>: ")
    , ("<h3>Conclusions</h3>\n<p>", "<p><strong>Conclusion</strong>: ")
    , ("<h3>Conclusions & Relevance</h3>\n<p>", "<p><strong>Conclusions and Relevance</strong>: ")
    , ("<h3>Trial Registration</h3>\n<p>", "<p><strong>Trial Registration</strong>: ")
    , ("\91Keywords: ", "<strong>\91Keywords</strong>: ")
    , (" = .",    " = 0.")
    , (" h2",     " <em>h</em><sup>2</sup>")
    , ("h2 ",     "<em>h</em><sup>2</sup> ")
    , ("≤p≤",     " ≤ <em>p</em> ≤ ")
    , ("\40r=",     "\40<em>r</em> = ")
    , ("\40R=",     "\40<em>r</em> = ")
    , ("\40R = ",   "\40<em>r</em> = ")
    , ("\40r = ",   "\40<em>r</em> = ")
    , ("\40N = ",   "\40<em>N</em> = ")
    , ("\40n = ",   "\40<em>n</em> = ")
    , ("\40n=",     "\40<em>n</em> = ")
    , ("\40N=",     "\40<em>N</em> = ")
    , ("<em>p</em> = .", "<em>p</em> = 0.")
    , ("<em>p</em> < .", "<em>p</em> < 0.")
    , (" N=",     " <em>N</em> = ")
    , ("\40p=",     "\40<em>p</em> = ")
    , (" n=",     " <em>n</em> = ")
    , (" P=",     " <em>p</em> = ")
    , (" P = ",   " <em>p</em> = ")
    , (" p = ",   " <em>p</em> = ")
    , (" p=",     " <em>p</em> = ")
    , (" P<",     " <em>p</em> < ")
    , ("\40P<",     "\40<em>p</em> < ")
    , (" P < ",   " <em>p</em> < ")
    , (" p < ",   " <em>p</em> < ")
    , (" p<",     " <em>p</em> < ")
    , (" p<.",    " <em>p</em> < 0.")
    , ("\40P=",     "\40<em>p</em> = ")
    , ("P-value", "<em>p</em>-value")
    , ("p-value", "<em>p</em>-value")
    , (" ", " ")
    , ("∼", "~")
      ]

trim :: String -> String
trim = reverse . dropWhile (isSpace) . reverse . dropWhile (isSpace) -- . filter (/='\n')

-- gwern :: Path -> IO (Maybe (Path, MetadataItem))
gwern p | ".pdf" `isInfixOf` p = pdf p
        | "#" `isInfixOf` p = return Nothing -- section links require custom annotations; we can't scrape any abstract/summary for them easily
        | or (map (`isInfixOf` p) [".avi", ".bmp", ".conf", ".css", ".csv", ".doc", ".docx", ".ebt", ".epub", ".gif", ".GIF", ".hi", ".hs", ".htm", ".html", ".ico", ".idx", ".img", ".jpeg", ".jpg", ".JPG", ".js", ".json", ".jsonl", ".maff", ".mdb", ".mht", ".mp3", ".mp4", ".o", ".ods", ".opml", ".pack", ".page", ".patch", ".png", ".R", ".rm", ".sh", ".svg", ".swf", ".tar", ".ttf", ".txt", ".wav", ".webm", ".xcf", ".xls", ".xlsx", ".xml", ".xz", ".yaml", ".zip"]) = return Nothing -- skip potentially very large archives
        | otherwise =
            do (status,_,bs) <- runShellCommand "./" Nothing "curl" ["--location", "--silent", "https://www.gwern.net/"++p, "--user-agent", "gwern+gwernscraping@gwern.net"]
               case status of
                 ExitFailure _ -> putStrLn ("Gwern.net download failed: " ++ p) >> return Nothing
                 _ -> do
                        let b = U.toString bs
                        let f = parseTags b
                        let metas = filter (isTagOpenName "meta") f
                        let title = concatMap (\(TagOpen _ (a:b)) -> if snd a == "title" then snd $ head b else "") metas
                        let date = concatMap (\(TagOpen _ (a:b)) -> if snd a == "dc.date.issued" then snd $ head b else "") metas
                        let author = initializeAuthors $ concatMap (\(TagOpen _ (a:b)) -> if snd a == "author" then snd $ head b else "") metas
                        let doi = ""
                        let abstract      = trim $ renderTags $ filter filterAbstract $ takeWhile takeToAbstract $ dropWhile dropToAbstract f
                        let description = concatMap (\(TagOpen _ (a:b)) -> if snd a == "description" then snd $ head b else "") metas
                        -- the description is inferior to the abstract, so we don't want to simply combine them, but if there's no abstract, settle for the description:
                        let abstract'     = if length description > length abstract then description else abstract

                        return $ Just (p, (title, author, date, doi, abstract'))
        where
          dropToAbstract (TagOpen "div" [("id", "abstract")]) = False
          dropToAbstract _                                    = True
          takeToAbstract (TagClose "div") = False
          takeToAbstract _                = True
          filterAbstract (TagOpen  "div" _)        = False
          filterAbstract (TagClose "div")          = False
          filterAbstract (TagOpen  "blockquote" _) = False
          filterAbstract (TagClose "blockquote")   = False
          filterAbstract _                         = True
