{- LinkMetadata.hs: module for generating Pandoc links which are annotated with metadata, which can then be displayed to the user as 'popups' by /static/js/popups.js. These popups can be excerpts, abstracts, article introductions etc, and make life much more pleasant for the reader - hxbover over link, popup, read, decide whether to go to link.
Author: Gwern Branwen
Date: 2019-08-20
When:  Time-stamp: "2021-01-24 22:58:02 gwern"
License: CC-0
-}

-- TODO:
-- 1. fix Unicode handling: `shellToCommand` seems to mangle Unicode, screwing up abstracts
-- 2. scrape more sites: possibilities include  predictionbook.com, amazon.com, nature.com, longbets.org, wiley.com, bmj.com, cran.r-project.org, and rand.org
-- 3. bugs in packages: the WMF API omits the need for `-L` in curl but somehow their live demo works anyway (?!); rxvist doesn't appear to support all bioRxiv/medRxiv schemas, including the '/early/' links, forcing me to use curl+Tagsoup; the R library 'fulltext' crashes on examples like `ft_abstract(x = c("10.1038/s41588-018-0183-z"))`

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module LinkMetadata (isLocalLink, readLinkMetadata, writeAnnotationFragments, Metadata, createAnnotations, hasAnnotation) where

import Control.Monad (when, void)
import qualified Data.ByteString as B (appendFile, writeFile)
import qualified Data.ByteString.Lazy as BL (length)
import qualified Data.ByteString.Lazy.UTF8 as U (toString) -- TODO: why doesn't using U.toString fix the Unicode problems?
import Data.Aeson (eitherDecode, FromJSON, Object, Value(String))
import qualified Data.HashMap.Strict as HM (lookup)
import GHC.Generics (Generic)
import Data.List (intercalate, intersperse, isInfixOf, isPrefixOf, isSuffixOf, sort, (\\))
import Data.Containers.ListUtils (nubOrd)
import Data.Char (isAlpha, isNumber, isSpace, toLower, toUpper)
import qualified Data.Map.Strict as M (fromList, toList, lookup, traverseWithKey, union, Map)
import Text.Pandoc (readerExtensions, writerWrapText, writerHTMLMathMethod, Inline(Link, Span),
                    HTMLMathMethod(MathJax), defaultMathJaxURL, def, readLaTeX, writeHtml5String,
                    WrapOption(WrapNone), runPure, pandocExtensions, readHtml, writerExtensions, nullAttr, nullMeta,
                    queryWith, Inline(Str, RawInline, Space), Pandoc(..), Format(..), Block(RawBlock, Para, BlockQuote))
import Text.Pandoc.Walk (walk)
import qualified Data.Text as T (append, isInfixOf, head, unpack, pack, Text)
import Data.FileStore.Utils (runShellCommand)
import System.Exit (ExitCode(ExitFailure))
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName, takeFileName, takeExtension)
import Data.List.Utils (replace, split, uniq)
import Text.HTML.TagSoup (isTagCloseName, isTagOpenName, parseTags, renderTags, Tag(TagClose, TagOpen, TagText))
import Data.Yaml as Y (decodeFileEither, encode, ParseException)
import Data.Time.Clock as TC (getCurrentTime)
import Text.Regex (subRegex, mkRegex)
import Data.Maybe (Maybe)
import Data.Text.IO as TIO (readFile, writeFile)
import System.IO (stderr, hPutStrLn)
import Typography (invertImage) -- TODO: 'typographyTransform'. This is semi-intractable. When we read in the HTML, the hyphenation pass breaks much of the LaTeX. Adding in guards in the walk to avoid Spans doesn't work like it ought to.
import Network.HTTP (urlDecode, urlEncode)

----
-- Should the current link get a 'G' icon because it's an essay or regular page of some sort?
-- we exclude several directories (docs/, static/, images/) entirely; a gwern.net page is then any link without a file extension (ie. a '.' in the URL - we guarantee that no Markdown essay has a period inside its URL).
-- Local links get the 'link-local' class.
isLocalLink :: Pandoc -> Pandoc
isLocalLink = walk isLocalLink'
  where isLocalLink' :: Inline -> Inline
        isLocalLink' y@(Link (a,b,c) e (f,g)) =
          let f' = replace "https://www.gwern.net" "" $ T.unpack f in
            if not ("/" `isPrefixOf` f') then y
            else
              if ("/images/" `isPrefixOf` f' || "/static/" `isPrefixOf` f') then y
              else
                if takeExtension f' /= "" then y
                else (Link (a,"link-local":b,c) e (f,g))
        isLocalLink' x = x


-------------------------------------------------------------------------------------------------------------------------------

readLinkMetadata :: IO Metadata
readLinkMetadata = do
             -- for hand created definitions, to be saved; since it's handwritten and we need line errors, we use YAML:
             custom <- readYaml "metadata/custom.yaml"

             -- Quality checks:
             -- - URLs, titles & annotations should all be unique, although author/date/DOI needn't be (we might annotate multiple parts of a single DOI)
             let urls = map (\(u,_) -> u) custom
             when (length (uniq (sort urls)) /=  length urls) $ error $ "Duplicate URLs in 'custom.yaml'!" ++ unlines (urls \\ nubOrd urls)
             let brokenUrls = filter (\u -> not (head u == 'h' || head u == '/' || head u == '?')) urls in when (brokenUrls /= []) $ error $ "Broken URLs in 'custom.yaml': " ++ unlines brokenUrls
             let titles = map (\(_,(t,_,_,_,_)) -> t) custom in when (length (uniq (sort titles)) /= length titles) $ error $ "Duplicate titles in 'custom.yaml': " ++ unlines (titles \\ nubOrd titles)
             let annotations = map (\(_,(_,_,_,_,s)) -> s) custom in when (length (uniq (sort annotations)) /= length annotations) $ error $ "Duplicate annotations in 'custom.yaml': " ++ unlines (annotations \\ nubOrd annotations)
             -- - DOIs are optional since they usually don't exist, and dates are optional for always-updated things like WP; but everything else should:
             let emptyCheck = filter (\(u,(t,a,_,_,s)) -> any (=="") [u,t,a,s]) custom
             when (length emptyCheck /= 0) $ error $ "Link Annotation Error: empty mandatory fields! This should never happen: " ++ show emptyCheck

             -- auto-generated cached definitions; can be deleted if gone stale
             rewriteLinkMetadata "metadata/auto.yaml" -- cleanup first
             auto <- readYaml "metadata/auto.yaml"

             -- merge the hand-written & auto-generated link annotations, and return:
             let final = M.union (M.fromList custom) (M.fromList auto) -- left-biased, 'custom' overrides 'auto'
             return final

writeAnnotationFragments :: Metadata -> IO ()
writeAnnotationFragments md = void $ M.traverseWithKey (writeAnnotationFragment md) md
writeAnnotationFragment :: Metadata -> Path -> MetadataItem -> IO ()
writeAnnotationFragment md u i@(_,_,_,_,e) = when (length e > 180) $
                                          do let u' = linkCanonicalize u
                                             let filepath = "metadata/annotations/" ++ urlEncode u' ++ ".html"
                                             let filepath' = take 274 filepath
                                             when (filepath /= filepath') $ hPutStrLn stderr $ "Warning, annotation fragment path → URL truncated! Was: " ++ filepath ++ " but truncated to: " ++ filepath' ++ "; (check that the truncated file name is still unique, otherwise some popups will be wrong)"
                                             let annotationPandoc = walk (hasAnnotation md False) $ generateAnnotationBlock (u', Just i)
                                             let annotationHTMLEither = runPure $ writeHtml5String def{writerExtensions = pandocExtensions} (Pandoc nullMeta annotationPandoc)
                                             case annotationHTMLEither of
                                               Left er -> error ("Writing annotation fragment failed! " ++ show u ++ ": " ++ show i ++ ": " ++ show er)
                                               Right annotationHTML -> writeUpdatedFile filepath' annotationHTML
   where -- write only when changed, to reduce sync overhead
    writeUpdatedFile :: FilePath -> T.Text -> IO ()
    writeUpdatedFile target contentsNew = do existsOld <- doesFileExist target
                                             if not existsOld then
                                               TIO.writeFile target contentsNew
                                               else do contentsOld <- TIO.readFile target
                                                       when (contentsNew /= contentsOld) $ TIO.writeFile target contentsNew

-- walk each page, extract the links, and create annotations as necessary for new links
createAnnotations :: Metadata -> Pandoc -> IO ()
createAnnotations md (Pandoc _ markdown) = mapM_ (annotateLink' md) $ queryWith extractLink markdown
  where extractLink :: Inline -> [String]
        extractLink (Link _ _ (path, _)) = [T.unpack path]
        extractLink _ = []

annotateLink' :: Metadata -> String -> IO Bool
annotateLink' md target =
  do when (target=="") $ error (show target)
     -- normalize: convert 'https://www.gwern.net/docs/foo.pdf' to '/docs/foo.pdf' and './docs/foo.pdf' to '/docs/foo.pdf'
     -- the leading '/' indicates this is a local gwern.net file
     let target' = replace "https://www.gwern.net/" "/" target
     let target'' = if head target' == '.' then drop 1 target' else target'

     let annotated = M.lookup target'' md
     case annotated of
       -- the link has a valid annotation already defined, so we're done: nothing changed.
       Just _  -> return False
       Nothing -> do new <- linkDispatcher target''
                     case new of
                       -- cache the failures too, so we don't waste time rechecking the PDFs every build; return False because we didn't come up with any new useful annotations
                       Nothing -> writeLinkMetadata target'' ("", "", "", "", "") >> return False
                       Just y@(f,m@(_,_,_,_,e)) -> do
                                       when (e=="") $ hPutStrLn stderr (f ++ ": " ++ show target ++ ": " ++ show y)
                                       -- return true because we *did* change the database & need to rebuild:
                                       writeLinkMetadata target'' m >> return True

-- walk the page, and modify each URL to specify if it has an annotation available or not:
hasAnnotation :: Metadata -> Bool -> Block -> Block
-- goddamn it Pandoc, why can't you read the very HTML you just wrote‽
hasAnnotation md idp x@(RawBlock (Format "html") h) = if not ("href=" `T.isInfixOf` h) then x else
                                                  let markdown = runPure $ readHtml def{readerExtensions = pandocExtensions} h in
                                                   case markdown of
                                                     Left e -> error (show x ++ ": " ++ show e)
                                                     Right markdown' -> let p@(Pandoc _ _) = walk (hasAnnotation md idp) markdown' in
                                                                          let p' = runPure $ do html <- writeHtml5String def{writerExtensions = pandocExtensions} p
                                                                                                let html' = T.pack $ restoreFloatRight (T.unpack h) (T.unpack html)
                                                                                                return $ RawBlock (Format "html") html'
                                                                          in case p' of
                                                                            Left e -> error (show x ++ ": " ++ show e)
                                                                            Right p'' -> p''
hasAnnotation md idp x = walk (hasAnnotationInline md idp) x
    where hasAnnotationInline :: Metadata -> Bool -> Inline -> Inline
          hasAnnotationInline mdb idBool y@(Link (a,b,c) e (f,g)) =
                                                        let f' = linkCanonicalize $ T.unpack f in
                                                        case M.lookup f' mdb of
                                                          Nothing               -> y
                                                          Just (_, _, _, _, "") -> y
                                                          Just (_,aut,dt,_,abs) -> let a' = if not idBool then "" else if a=="" then generateID (T.unpack f) aut dt else a in -- erase link ID?
                                                            if (length abs < 180) then y else
                                                              if T.head f == '?' then
                                                                Span (a', nubOrd (b++["defnMetadata"]), [("original-definition-id",f)]++c) e else
                                                                Link (a', nubOrd (b++["docMetadata"]), c) e (f,g)

          hasAnnotationInline _ _ y = y


generateAnnotationBlock :: (FilePath, Maybe LinkMetadata.MetadataItem) -> [Block]
generateAnnotationBlock (f, ann) = case ann of
                              Nothing -> nonAnnotatedLink
                              Just ("",   _, _,_ ,_) -> nonAnnotatedLink
                              Just (_,    _, _,_ ,"") -> nonAnnotatedLink
                              Just (tle,aut,dt,doi,abst) -> let lid = let tmpID = (generateID f aut dt) in if tmpID=="" then "" else (T.pack "linkBibliography-") `T.append` tmpID in
                                                            let author = if aut=="" then [Space] else [Space, Span ("", ["author"], []) [Str (T.pack aut)], Space] in
                                                              let date = if dt=="" then [] else [Str "(", Span ("", ["date"], []) [Str (T.pack dt)], Str ")"] in
                                                                let values = if doi=="" then [] else [("doi",T.pack doi)] in
                                                                  let link = if head f == '?' then
                                                                               Span (lid, ["defnMetadata"], [("original-definition-id",T.pack f)]++values) [RawInline (Format "html") (T.pack $ "“"++tle++"”")]
                                                                        else
                                                                               Link (lid, ["docMetadata"], values) [RawInline (Format "html") (T.pack $ "“"++tle++"”")] (T.pack f,"")
                                                                        in
                                                                    -- make sure every abstract is wrapped in paragraph tags for proper rendering:
                                                                     let abst' = let start = take 3 abst in if start == "<p>" || start == "<ul" || start == "<ol" || start=="<h2" || start=="<h3" || start=="<bl" || (take 7 abst) == "<figure" then abst else "<p>" ++ abst ++ "</p>" in
                                                                       -- check that float-right hasn't been deleted by Pandoc again:
                                                                       let abst'' = restoreFloatRight abst abst' in
                                                              [Para
                                                                ([link,
                                                                  Str ","] ++ author ++ date ++ [Str ":"]),
                                                           BlockQuote [RawBlock (Format "html") (rewriteAnchors f (T.pack abst''))]
                                                           ]
                             where
                               nonAnnotatedLink :: [Block]
                               nonAnnotatedLink = [Para [Link nullAttr [Str (T.pack f)] (T.pack f, "")]]

-- annotations, like /Faces, often link to specific sections or anchors, like 'I clean the data with [Discriminator Ranking](#discriminator-ranking)'; when transcluded into other pages, these links are broken. But we don't want to rewrite the original abstract as `[Discriminator Ranking](/Faces#discriminator-ranking)` to make it absolute, because that screws with section-popups/link-icons! So instead, when we write out the body of each annotation inside the link bibliography, while we still know what the original URL was, we traverse it looking for any links starting with '#' and rewrite them to be absolute:
-- WARNING: because of the usual RawHtml issues, reading with Pandoc doesn't help - it just results in RawInlines which still need to be parsed somehow. I settled for a braindead string-rewrite; in annotations, there shouldn't be *too* many cases where the href=# pattern shows up without being a div link...
rewriteAnchors :: FilePath -> T.Text -> T.Text
rewriteAnchors f = T.pack . replace "href=\"#" ("href=\""++f++"#") . T.unpack

-------------------------------------------------------------------------------------------------------------------------------

type Metadata = M.Map Path MetadataItem -- (Title, Author, Date, DOI, Abstract)
type MetadataItem = (String, String, String, String, String)
type MetadataList = [(Path, MetadataItem)]
type Path = String

readYaml :: Path -> IO MetadataList
readYaml yaml = do file <- Y.decodeFileEither yaml :: IO (Either ParseException [[String]])
                   case file of
                     Left e -> error $ "File: "++ yaml ++ "; parse error: " ++ show e
                     Right y -> (return $ concatMap convertListToMetadata y) :: IO MetadataList
                where
                 convertListToMetadata :: [String] -> MetadataList
                 convertListToMetadata [u, t, a, d, di, s] = [(u, (t,a,d,di,s))]
                 convertListToMetadata e@_ = error $ "Pattern-match failed (too few fields?): " ++ show e

-- clean a YAML metadata file by sorting & unique-ing it (this cleans up the various appends or duplicates):
rewriteLinkMetadata :: Path -> IO ()
rewriteLinkMetadata yaml = do old <- readYaml yaml
                              let new = M.fromList old :: Metadata -- NOTE: constructing a Map datastructure automatically sorts/dedupes
                              let newYaml = Y.encode $ map (\(a,(b,c,d,e,f)) -> (a,b,c,d,e,f)) $ -- flatten [(Path, (String, String, String, String, String))]
                                    M.toList new
                              B.writeFile yaml newYaml

-- append (rather than rewrite entirely) a new automatic annotation if its Path is not already in the auto-annotation database:
writeLinkMetadata :: Path -> MetadataItem -> IO ()
writeLinkMetadata l i@(t,a,d,di,abst) = do hPutStrLn stderr (l ++ " : " ++ show i)
                                           -- check very quickly whether the path/key is already in newest on-disk version, to avoid potential duplicate instances
                                           -- (duplicates do not cause errors, but they bloat the file, and the existence of duplicates makes it harder to lint & edit)
                                           newest <- TIO.readFile "metadata/auto.yaml"
                                           when (not $ T.pack ("\n- - " ++ l ++ "\n") `T.isInfixOf` newest) $
                                              do let newYaml = Y.encode [(l,t,a,d,di,abst)]
                                                 B.appendFile "metadata/auto.yaml" newYaml

-- WARNING: Pandoc erases attributes set on `<figure>` like 'float-right', so blindly restore a float-right class to all <figure>s if there was one in the original (it's a hack, but I generally don't use any other classes besides 'float-right', or more than one image per annotation or mixed float/non-float, and it's a lot simpler...):
restoreFloatRight :: String -> String -> String
restoreFloatRight original final = if ("<figure class=\"float-right\">" `isInfixOf` original) then replace "<figure>" "<figure class=\"float-right\">" final else final

-- some author lists are absurdly long; stop at a certain length, finish the author list through the current author (comma-delimited), and leave the rest as 'et al':
-- trimAuthors, initializeAuthors, trimTitle :: String -> String
-- trimAuthors a = let maxLength = 58 in if length a < maxLength then a else (take maxLength a) ++ (takeWhile (/=',') (drop maxLength a)) ++ " et al"
initializeAuthors, trimTitle :: String -> String
initializeAuthors a' = replace " and " ", " $ subRegex (mkRegex " ([A-Z]) ") a' " \\1. " -- "John H Smith" → "John H. Smith"
-- title clean up: delete the period at the end of many titles, extraneous colon spacing, remove Arxiv's newline+doublespace, and general whitespace cleaning
trimTitle [] = ""
trimTitle t = let t' = reverse $ replace " : " ": " $ replace "\n " "" $ trim t in
                if length t' > 0 then reverse (if head t' == '.' then tail t' else t') else ""

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
  | "Gwern Branwen" == author = T.pack (trim $ replace "." "-" $ replace "--" "-" $ replace "/" "-" $ replace "#" "-" $ map toLower $ replace "https://" "" $ replace "https://www.gwern.net/" "" $ "gwern-"++url')
  -- 'Foo 2020' → '#foo-2020'; 'Foo & Bar 2020' → '#foo-bar-2020'; 'foo et al 2020' → 'foo-et-al-2020'
  | otherwise = T.pack $ let year = if date=="" then "2020" else take 4 date in -- YYYY-MM-DD
                           let authors = split ", " $ head $ split " (" author in -- handle affiliations like "Tom Smith (Wired)"
                           let authorCount = length authors in
                             if authorCount == 0 then "" else
                               let firstAuthorSurname = filter isAlpha $ reverse $ takeWhile (/=' ') $ reverse $ head authors in
                                 -- handle cases like '/docs/statistics/peerreview/1975-johnson-2.pdf'
                                 let suffix = (let s = take 1 $ reverse $ takeBaseName url' in if not (s=="") && isNumber (head s) then "-" ++ s else "") in
                                   let suffix' = if suffix == "-1" then "" else suffix in
                                 filter (/='.') $ map toLower $ if authorCount >= 3 then
                                                 firstAuthorSurname ++ "-et-al-" ++ year ++ suffix' else
                                                   if authorCount == 2 then
                                                     let secondAuthorSurname = filter isAlpha $ reverse $ takeWhile (/=' ') $ reverse $ (authors !! 1) in
                                                       firstAuthorSurname ++ "-" ++ secondAuthorSurname ++ "-" ++ year ++ suffix'
                                                   else
                                                     firstAuthorSurname ++ "-" ++ year ++ suffix'
  where url' = replace "?" "definition-" url -- definition links, like '?Portia' or '?Killing-Rabbits' are invalid HTML selectors, so substitute there to a final ID like eg '#gwern-definition-portia'

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
                 | otherwise = let l' = linkCanonicalize l in if (head l' == '/') then gwern $ tail l else return Nothing

linkCanonicalize :: String -> String
linkCanonicalize l | "https://www.gwern.net/" `isPrefixOf` l = replace "https://www.gwern.net/" "/" l
                   -- | head l == '#' = l
                   | otherwise = l

-- handles both PM & PLOS right now:
pubmed l = do (status,_,mb) <- runShellCommand "./" Nothing "Rscript" ["static/build/linkAbstract.R", l]
              case status of
                ExitFailure err -> (hPutStrLn stderr $ intercalate " : " [l, show status, show err, show mb]) >> return Nothing
                _ -> do
                        let parsed = lines $ replace " \n" "\n" $ trim $ U.toString mb
                        if length parsed < 5 then return Nothing else
                          do let (title:author:date:doi:abstract) = parsed
                             return $ Just (l, (trimTitle title, initializeAuthors $ trim author, trim date, trim doi, replace "<br/>" " " $ cleanAbstractsHTML $ unlines abstract))

pdf :: Path -> IO (Maybe (Path, MetadataItem))
pdf p = do (_,_,mb) <- runShellCommand "./" Nothing "exiftool" ["-printFormat", "$Title$/$Author$/$Date$/$DOI", "-Title", "-Author", "-Date", "-DOI", p]
           if BL.length mb > 0 then
             do let (etitle:eauthor:edate:edoi:_) = lines $ U.toString mb
                -- PDFs have both a 'Creator' and 'Author' metadata field sometimes. Usually Creator refers to the (single) person who created the specific PDF file in question, and Author refers to the (often many) authors of the content; however, sometimes PDFs will reverse it: 'Author' means the PDF-maker and 'Creators' the writers. If the 'Creator' field is longer than the 'Author' field, then it's a reversed PDF and we want to use that field instead of omitting possibly scores of authors from our annotation.
                (_,_,mb2) <- runShellCommand "./" Nothing "exiftool" ["-printFormat", "$Creator", "-Creator", p]
                let ecreator = U.toString mb2
                let author = initializeAuthors $ trim $ if (length eauthor > length ecreator) || ("Adobe" `isInfixOf` ecreator || "InDesign" `isInfixOf` ecreator || "Arbortext" `isInfixOf` ecreator || "Unicode" `isInfixOf` ecreator || "Total Publishing" `isInfixOf` ecreator) then eauthor else ecreator
                hPutStrLn stderr $ "PDF: " ++ p ++" DOI: " ++ edoi
                aMaybe <- doi2Abstract edoi
                -- if there is no abstract, there's no point in displaying title/author/date since that's already done by tooltip+URL:
                case aMaybe of
                  Nothing -> return Nothing
                  Just a -> return $ Just (p, (trimTitle etitle, author, trim edate, edoi, a))
           else return Nothing

-- nested JSON object: eg 'jq .message.abstract'
data Crossref = Crossref { message :: Message } deriving (Show,Generic)
instance FromJSON Crossref
data Message = Message { abstract :: Maybe String } deriving (Show,Generic)
instance FromJSON Message
doi2Abstract :: [Char] -> IO (Maybe String)
doi2Abstract doi = if length doi < 7 then return Nothing
                   else do (_,_,bs) <- runShellCommand "./" Nothing "curl" ["--location", "--silent", "https://api.crossref.org/works/"++doi, "--user-agent", "gwern+crossrefscraping@gwern.net"]
                           if bs=="Resource not found." then return Nothing
                           else let j = eitherDecode bs :: Either String Crossref
                                in case j of -- start unwrapping...
                                    Left e -> hPutStrLn stderr ("Error: Crossref request failed: "++doi++" "++e) >> return Nothing
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
                   let rq = "https://en.wikipedia.org/api/rest_v1/page/summary/"++p'''++"?redirect=true"
                   -- `--location` is required or redirects will not be followed by *curl*; '?redirect=true' only makes the *API* follow redirects
                   (status,_,bs) <- runShellCommand "./" Nothing "curl" ["--location", "--silent", rq, "--user-agent", "gwern+wikipediascraping@gwern.net"]
                   when ("\"type\":\"disambiguation\"" `isInfixOf` U.toString bs) $ error ("Linked to a Wikipedia disambiguation page! " ++ p)
                   today <- fmap (take 10 . show) $ TC.getCurrentTime -- create dates like "2020-08-31"
                   case status of
                     ExitFailure _ -> hPutStrLn stderr ("Wikipedia tooltip failed: " ++ p''') >> return Nothing
                     _ -> let j = eitherDecode bs :: Either String WP
                          in case j of
                               Left e -> hPutStrLn stderr ("WP request failed: " ++ e ++ " " ++ p ++ " " ++ p''') >> return Nothing
                               Right wp -> do let wpTitle = title wp
                                              let wpAbstract = extract_html wp
                                              wpThumbnail <- case thumbnail wp of
                                                     Nothing -> return ""
                                                     Just thumbnailObject -> case (HM.lookup "source" thumbnailObject) of
                                                                               Nothing -> return ""
                                                                               Just (String href) -> do -- check whether the WP thumbnail should be auto-inverted in popups for dark mode users:
                                                                                                        newThumbnail <- downloadWPThumbnail $ T.unpack href
                                                                                                        (color,h,w) <- invertImage newThumbnail
                                                                                                        let imgClass = if color then "class=\"invertible-auto\" " else ""
                                                                                                        return ("<figure class=\"float-right\"><img " ++ imgClass ++ "height=\"" ++ h ++ "\" width=\"" ++ w ++ "\" src=\"/" ++ (replace "%20" " " $ replace "%2F" "/" $ urlEncode newThumbnail) ++ "\" alt=\"Wikipedia thumbnail image of '" ++ urlEncode wpTitle ++ "'\" /></figure> ")
                                                                               Just _ -> return ""
                                              return $ Just (p, (wpTitle, "English Wikipedia", today, "", replace "<br/>" "" $ -- NOTE: after manual review, '<br/>' in WP abstracts seems to almost always be an error in the formatting of the original article, or useless.
                                                                                                          let wpAbstract' = cleanAbstractsHTML wpAbstract in
                                                                                                          wpThumbnail ++ wpAbstract'))

downloadWPThumbnail :: FilePath -> IO FilePath
downloadWPThumbnail href = do
  let f = "images/thumbnails/wikipedia/"++(replace "--" "-" $ filter (not . (\c -> c=='?' || c=='!' || c=='\'' || c=='"' || c=='&')) $ takeFileName (urlDecode href))
  filep <- doesFileExist f
  when (not filep ) $ void $
    runShellCommand "./" Nothing "curl" ["--location", "--silent", "--user-agent", "gwern+wikipediascraping@gwern.net", href, "--output", f]
  let ext = map toLower $ takeExtension f
  if ext == ".png" then do -- lossily optimize using my pngnq/mozjpeg scripts:
                     void $ runShellCommand "./" Nothing "/home/gwern/bin/bin/png" [f]
                     -- remove any transparency (dark mode issues)
                     void $ runShellCommand "./" Nothing "mogrify" ["-background", "white", "-alpha", "remove", "-alpha", "off", f]
                   else if (ext == ".jpg") then
                        void $ runShellCommand "./" Nothing "/home/gwern/bin/bin/compressJPG" [f]
                        else when (ext == ".gif") $ void $ runShellCommand "./" Nothing "gifsicle" ["--optimize=3", "--colors=256", f, "--output", f]
  return f

-- handles medRxiv too (same codebase)
biorxiv p = do (status,_,bs) <- runShellCommand "./" Nothing "curl" ["--location", "--silent", p, "--user-agent", "gwern+biorxivscraping@gwern.net"]
               case status of
                 ExitFailure _ -> hPutStrLn stderr ("BioRxiv download failed: " ++ p) >> return Nothing
                 _ -> do
                        let b = U.toString bs
                        let f = parseTags b
                        let metas = filter (isTagOpenName "meta") f
                        let title = concatMap (\(TagOpen _ (a:b)) -> if snd a == "DC.Title" then snd $ head b else "") metas
                        if (title=="") then hPutStrLn stderr ("BioRxiv parsing failed: " ++ p ++ ": " ++ show metas) >> return Nothing
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
                 ExitFailure _ -> hPutStrLn stderr ("Error: curl API call failed on Arxiv ID " ++ arxivid) >> return Nothing
                 _ -> do let (tags,_) = element "entry" $ parseTags $ U.toString bs
                         let title = trimTitle $ findTxt $ fst $ element "title" tags
                         let authors = initializeAuthors $ intercalate ", " $ getAuthorNames tags
                         let published = take 10 $ findTxt $ fst $ element "published" tags -- "2017-12-01T17:13:14Z" → "2017-12-01"
                         let doi = findTxt $ fst $ element "arxiv:doi" tags
                         let abs = processArxivAbstract url $ findTxt $ fst $ element "summary" tags
                         return $ Just (url, (title,authors,published,doi,abs))
-- NOTE: we inline Tagsoup convenience code from Network.Api.Arxiv (https://hackage.haskell.org/package/arxiv-0.0.1/docs/src/Network-Api-Arxiv.html); because that library is unmaintained & silently corrupts data (https://github.com/toschoo/Haskell-Libs/issues/1), we keep the necessary code close at hand so at least we can easily patch it when errors come up
-- Get the content of a 'TagText'
findTxt :: [Tag String] -> String
findTxt [] = ""
findTxt (t:ts) = case t of
                   TagText x -> x
                   _         -> findTxt ts
getAuthorNames :: [Tag String] -> [String]
getAuthorNames = go
  where go s = case element "author" s of
                 ([],[]) -> []
                 (a,[])  -> [getString "name" a]
                 (a,r)   ->  getString "name" a : go r
        getString :: String -> [Tag String] -> String
        getString n soup = let (i,_) = element n soup
                      in if null i then "" else findTxt i
element :: String -> [Tag String] -> ([Tag String], [Tag String])
element _  []     = ([],[])
element nm (t:ts) | isTagOpenName nm t = let (r,rs) = closeEl 0 ts
                                          in (t:r,rs)
                  | otherwise          = element nm ts
  where closeEl :: Int -> [Tag String] -> ([Tag String], [Tag String])
        closeEl _ [] = ([],[])
        closeEl i (x:xs) = go i (isTagCloseName nm x) x xs
        go i b x xs | b && i == 0        = ([x],xs)
                    | b && i >  0        = let (r,rs) = closeEl (i-1) xs
                                            in (x:r,rs)
                    | isTagOpenName nm x = let (r,rs) = closeEl (i+1) xs
                                            in (x:r,rs)
                    | otherwise          = let (r,rs) = closeEl i     xs
                                            in (x:r,rs)

-- Arxiv makes multi-paragraph abstracts hard because the 'HTML' is actually LaTeX, so we need to special Pandoc preprocessing (for paragraph breaks, among other issues):
processArxivAbstract :: String -> String -> String
processArxivAbstract u a = let cleaned = runPure $ do
                                    pandoc <- readLaTeX def{ readerExtensions = pandocExtensions } $ T.pack $ replace "%" "\\%" $ replace "\n  " "\n\n" a
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
    , (" </sec>", "")
    , ("   <title/>    <p>", "<p>")
    , ("  <p>", "<p>")
    , ("<br/><h3>", "<h3>")
    , ("</p><p>", "</p> <p>")
    , ("<strong>ABSTRACT</strong><br/>              <p>", "<p>")
    , ("</strong><p>", "</strong>: <p>")
    , ("<strong>Abstract</strong>:        ", "")
    , ("<abstract abstract-type=\"summary\"><br/>", "")
    , ("<abstract abstract-type=\"toc\">", "")
    , ("<strong>SUMMARY</jats:title>", "")
    , ("<strong>Abstract</jats:title>", "")
    , ("<strong>Abstract</strong><br/>", "")
    , ("<h3>Abstract:</h3>", "")
    , ("<h3>Summary/Abstract</h3>", "")
    , ("Alzheimer9", "Alzheimer'")
    , ("<br/> <br/>", "</br>")
    , ("1.<p>", "<p>")
    , ("<p> ", "<p>")
    , (" <p>", "<p>")
    , ("</p> ", "</p>")
    , (" </p>", "</p>")
    , ("</p><br/>", "</p>")
    , ("</p> <br/>", "</p>")
    , ("<p><br/>", "<p>")
    , ("<p><br />", "<p>")
    , ("</li><br/>", "</li>")
    , ("  </sec><br/>  ", "")
    , ("<sec><br/>    ", "")
    , ("</jats:sec>", "")
    , ("<jats:sec><br/>", "")
    , ("</jats:sec><br/>", "")
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
    , ("<p>Background: ", "<p><strong>Background</strong>: ")
    , ("<p>Methods: ", "<p><strong>Methods</strong>: ")
    , ("<p>Outcomes: ", "<p><strong>Outcomes</strong>: ")
    , ("<p>Interpretation: ", "<p><strong>Interpretation</strong>: ")
    , ("<p>Funding: ", "<p><strong>Funding</strong>: ")
    , ("<em>N</em> =", "<em>n</em> =")
    , ("<strong><strong>", "<strong>")
    , ("</strong></strong>", "</strong>")
    , ("< /b>", "</strong>")
    , ("<b>", "<strong>")
    , ("</b>", "</strong>")
    , ("<jats:sec><strong>", "<strong>")
    , ("<jats:title>Abstract</jats:title><br/>               ", "")
    , ("</jats:p>", "</p>")
    , ("< sub>", "<sub>")
    , ("<jats:sub>", "<sub>")
    , ("</jats:sub>", "</sub>")
    , ("<jats:sup>", "<sup>")
    , ("</jats:sup>", "</sup>")
    , ("<jats:title content-type=\"abstract-subheading\">", "<strong>")
    , ("<jats:title>", "<strong>")
    , ("</jats:title>", "</strong>")
    , ("<jats:title>", "<strong>")
    , ("</jats:title>", "</strong>")
    , ("<jats:p xml:lang=\"en\">", "<p>")
    , ("<jats:p>", "<p>")
    , ("<jats:italics>", "<em>")
    , ("</jats:italics>", "</em>")
    , ("<jats:italic>", "<em>")
    , ("</jats:italic>", "</em>")
    , ("<italic>", "<em>")
    , ("</italic>", "</em>")
    , ("<jats:title>Abstract</jats:title>\n\t  <jats:p>", "")
    , ("<h3>ABSTRACT</h3>", "")
    , ("<h3>Abstract</h3>", "")
    , ("<h3>SUMMARY</h3>", "")
    , ("<h3>Summary</h3>", "")
    , ("<abstract>", "")
    , ("<abstract>\n  ", "")
    , ("\n</abstract>", "")
    , ("<p><strong>Abstract</strong>: ", "<p>")
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
    , ("</h3><br/>", "</h3>")
    , ("<br/><h3>", "<h3>")
    , ("\91Keywords: ", "<strong>\91Keywords</strong>: ")
    , ("10(-10)", "10<sup>−10</sup>")
    , ("10(-11)", "10<sup>−11</sup>")
    , ("10(-13)", "10<sup>−13</sup>")
    , ("10(-15)", "10<sup>−15</sup>")
    , ("10(-19)", "10<sup>−19</sup>")
    , ("10(-26)", "10<sup>−26</sup>")
    , ("10(-3)", "10<sup>−3</sup>")
    , ("10(-4)", "10<sup>−4</sup>")
    , ("10(-5)", "10<sup>−5</sup>")
    , ("10(-6)", "10<sup>−6</sup>")
    , ("10(-7)", "10<sup>−7</sup>")
    , ("10(-8)", "10<sup>−8</sup>")
    , ("10(-9)", "10<sup>−9</sup>")
    , ("10(-)(3)", "10<sup>−3</sup>")
    , ("10(-)(4)", "10<sup>−4</sup>")
    , ("10(-)(5)", "10<sup>−5</sup>")
    , ("10(-)(6)", "10<sup>−6</sup>")
    , ("10(-)(7)", "10<sup>−7</sup>")
    , ("10(-)(8)", "10<sup>−8</sup>")
    , ("10(-)(9)", "10<sup>−9</sup>")
    , ("10(-)(10)", "10<sup>−10</sup>")
    , ("R (2) ", "R<sup>2</sup> ")
    , ("CO(2)", "CO<sub>2</sub>")
    , (" = .",    " = 0.")
    , (" gf ", " <em>gf</em> ")
    , (" gc ", " <em>gc</em> ")
    , ("h<sup>2</sup>", "<em>h</em><sup>2</sup>")
    , (" h2",     " <em>h</em><sup>2</sup>")
    , ("h2 ",     "<em>h</em><sup>2</sup> ")
    , ("h(2)",    "<em>h</em><sup>2</sup>")
    , ("r(g)",    "<em>r</em><sub<em>g</em></sub>")
    , ("r=", "<em>r</em> = ")
    , ("r>", "<em>r</em> > ")
    , ("r<", "<em>r</em> < ")
    , ("r≥", "<em>r</em> ≥ ")
    , ("r≤", "<em>r</em> ≤ ")
    , ("≤p≤",     " ≤ <em>p</em> ≤ ")
    , ("\40r=",     "\40<em>r</em> = ")
    , ("\40R=",     "\40<em>r</em> = ")
    , ("\40R = ",   "\40<em>r</em> = ")
    , ("\40r = ",   "\40<em>r</em> = ")
    , ("\40N = ",   "\40<em>N</em> = ")
    , ("\40n = ",   "\40<em>n</em> = ")
    , ("\40n=",     "\40<em>n</em> = ")
    , ("\40N=",     "\40<em>N</em> = ")
    , (" N ~ ",     " <em>n</em> ~ ")
    , ("( N = ", "(<em>n</em> = ")
    , ("( n = ", "(<em>n</em> = ")
    , ("( ns = ", "(<em>ns</em> = ")
    , ("( n = ", "(<em>n</em> = ")
    , ("n = ", "<em>n</em> = ")
    , ("<em>p</em> = .", "<em>p</em> = 0.")
    , ("<em>p</em> < .", "<em>p</em> < 0.")
    , (" N=",     " <em>N</em> = ")
    , ("\40p=",     "\40<em>p</em> = ")
    , (" n=",     " <em>n</em> = ")
    , ("p = 0",   "<em>p</em> = 0")
    , (" P=",     " <em>p</em> = ")
    , (" P = ",   " <em>p</em> = ")
    , ("(P = ",   "(<em>p</em> = ")
    , ("(P=",     "(<em>p</em> = ")
    , (" p = ",   " <em>p</em> = ")
    , (" p=",     " <em>p</em> = ")
    , (" P<",     " <em>p</em> < ")
    , ("P ≤ ", "<em>p</em> ≤ ")
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
    , ("GxE", "G×E")
    , (" 10x", " 10×")
    , (" 100x", " 100×")
    , (" 2x", " 2×")
    , (" 3x", " 3×")
    , ("<p> ", "<p>")
    , ("+/-", "±")
    , ("ml-1", "ml<sup-1</sup>")
    , ("Cmax", "C<sub>max</sub>")
    , ("<small></small>", "")
    , ("\173", "") -- we do soft hyphenation at compile-time to keep the data sources clean & readable, and benefit from any upgrades
      ]

trim :: String -> String
trim = reverse . dropWhile badChars . reverse . dropWhile badChars -- . filter (/='\n')
  where badChars c = isSpace c || (c=='-')

-- gwern :: Path -> IO (Maybe (Path, MetadataItem))
gwern p | ".pdf" `isInfixOf` p = pdf p
        | "#" `isInfixOf` p = return Nothing -- section links require custom annotations; we can't scrape any abstract/summary for them easily
        | or (map (`isInfixOf` p) [".avi", ".bmp", ".conf", ".css", ".csv", ".doc", ".docx", ".ebt", ".epub", ".gif", ".GIF", ".hi", ".hs", ".htm", ".html", ".ico", ".idx", ".img", ".jpeg", ".jpg", ".JPG", ".js", ".json", ".jsonl", ".maff", ".mdb", ".mht", ".mp3", ".mp4", ".o", ".ods", ".opml", ".pack", ".page", ".patch", ".php", ".png", ".R", ".rm", ".sh", ".svg", ".swf", ".tar", ".ttf", ".txt", ".wav", ".webm", ".xcf", ".xls", ".xlsx", ".xml", ".xz", ".yaml", ".zip"]) = return Nothing -- skip potentially very large archives
        | otherwise =
            do (status,_,bs) <- runShellCommand "./" Nothing "curl" ["--location", "--silent", "https://www.gwern.net/"++p, "--user-agent", "gwern+gwernscraping@gwern.net"]

               case status of
                 ExitFailure _ -> hPutStrLn stderr ("Gwern.net download failed: " ++ p) >> return Nothing
                 _ -> do
                        let b = U.toString bs
                        let f = parseTags b
                        let metas = filter (isTagOpenName "meta") f
                        let title = concatMap (\(TagOpen _ (a:b)) -> if snd a == "title" then snd $ head b else "") metas
                        let date = concatMap (\(TagOpen _ (a:b)) -> if snd a == "dc.date.issued" then snd $ head b else "") metas
                        let keywords = concatMap (\(TagOpen _ (a:b)) -> if snd a == "keywords" then snd $ head b else "") metas
                        let keywords' = "<p>[<strong>Keywords</strong>: " ++ keywordsToLinks keywords ++ "]</p>"
                        let author = initializeAuthors $ concatMap (\(TagOpen _ (a:b)) -> if snd a == "author" then snd $ head b else "") metas
                        let thumbnail = if length (filter filterThumbnail metas) >0 then
                                          (\(TagOpen _ [_, ("content", thumb)]) -> thumb) $ head $ filter filterThumbnail metas else ""
                        let thumbnail' = (if (thumbnail == "https://www.gwern.net/static/img/logo/logo-whitebg-large-border.png" ) then "" else replace "https://www.gwern.net/" "" thumbnail) :: String
                        thumbnailFigure <- if thumbnail'=="" then return "" else do
                              (color,h,w) <- invertImage thumbnail'
                              let imgClass = if color then "class=\"invertible-auto\" " else ""
                              return ("<figure class=\"float-right\"><img " ++ imgClass ++ "height=\"" ++ h ++ "\" width=\"" ++ w ++ "\" src=\"/" ++ (replace "%F" "/" $ urlEncode thumbnail') ++ "\" alt=\"Gwern.net preview image for '" ++ title ++ "'\" /></figure> ")

                        let doi = ""
                        let abstract      = trim $ renderTags $ filter filterAbstract $ takeWhile takeToAbstract $ dropWhile dropToAbstract $ dropWhile dropToBody f
                        let description = concatMap (\(TagOpen _ (a:b)) -> if snd a == "description" then snd $ head b else "") metas
                        -- the description is inferior to the abstract, so we don't want to simply combine them, but if there's no abstract, settle for the description:
                        let abstract'     = if length description > length abstract then description else abstract
                        let abstract'' = if take 3 abstract' == "<p>" then abstract' else "<p>"++abstract'++"</p>"
                        if abstract'' == "404 Not Found Error: no page by this name!" then return Nothing else
                          return $ Just (p, (title, author, date, doi, thumbnailFigure++" "++abstract''++" "++keywords'))
        where
          dropToBody (TagOpen "body" _) = False
          dropToBody _ = True
          dropToAbstract (TagOpen "div" [("class", "abstract")]) = False
          dropToAbstract _                                    = True
          takeToAbstract (TagClose "div") = False
          takeToAbstract _                = True
          filterAbstract (TagOpen  "div" _)        = False
          filterAbstract (TagClose "div")          = False
          filterAbstract (TagOpen  "blockquote" _) = False
          filterAbstract (TagClose "blockquote")   = False
          filterAbstract _                         = True
          filterThumbnail (TagOpen "meta" [("property", "og:image"), _]) = True
          filterThumbnail _ = False

          -- ["statistics","NN","anime","shell","dataset"] ~> "<a href=\"/tags/statistics\">statistics</a>, <a href=\"/tags/NN\">NN</a>, <a href=\"/tags/anime\">anime</a>, <a href=\"/tags/shell\">shell</a>, <a href=\"/tags/dataset\">dataset</a>"
          keywordsToLinks :: String -> String
          keywordsToLinks = concat . intersperse ", " . map (\k -> "<a title=\"All pages tagged '"++k++"'\"" ++ " href=\"/tags/"++k++"\">"++k++"</a>") . words . replace "," ""
