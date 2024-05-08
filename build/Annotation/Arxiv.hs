module Annotation.Arxiv (arxiv, processArxivAbstract) where

import Data.List (intercalate, isInfixOf, isSuffixOf)
import qualified Data.Text as T (pack, unpack)
import Data.FileStore.Utils (runShellCommand)
import qualified Data.ByteString.Lazy.UTF8 as U (toString, ByteString) -- TODO: why doesn't using U.toString fix the Unicode problems?
import Text.Show.Pretty (ppShow)
import System.Exit (ExitCode(ExitFailure))
import Text.Pandoc (readerExtensions, writerWrapText, writerHTMLMathMethod, HTMLMathMethod(MathJax),
                    defaultMathJaxURL, def, readLaTeX, writeHtml5String, WrapOption(WrapNone), runPure, pandocExtensions)
import Text.Pandoc.Walk (walk)
import Text.HTML.TagSoup (isTagCloseName, isTagOpenName, parseTags, Tag(TagText))
import System.IO.Unsafe (unsafePerformIO)

import LinkAuto (linkAutoHtml5String)
import LinkMetadataTypes (Failure(..), MetadataItem, Path)
import MetadataFormat (checkURL, cleanAuthors, cleanAbstractsHTML, processDOI, trimTitle, processDOIArxiv)
import Utils (printRed, printGreen, replace, safeHtmlWriterOptions, replaceMany, sedMany, inlineMath2Text)
import Paragraph (processParagraphizer)
import Config.Misc as C -- (cd)

arxiv :: Path -> IO (Either Failure (Path, MetadataItem))
arxiv url = do -- Arxiv direct PDF links are deprecated but sometimes sneak through or are deliberate section/page links
               let url' = replace "https://browse.arxiv.org/html/" "https://arxiv.org/abs/" $ replace "http://" "https://" url
               (status,bs, arxivid) <- arxivDownload url'
               case status of
                 ExitFailure _ -> printRed ("Error: curl API call failed on Arxiv ID: " ++ arxivid ++ "; Result: " ++ show bs) >> return (Left Temporary)
                 _ -> do printGreen "Processing Arxiv metadata…"
                         let (tags,_) = element "entry" $ parseTags $ U.toString bs
                         -- compile the title string because it may include math (usually a superscript, like "S$^2$-MLP: Spatial-Shift MLP Architecture for Vision" or "RL$^2$" etc)
                         C.cd -- ensure we are in the right place to enable calling `latex2unicode.py`
                         let title = replace "<p>" "" $ replace "</p>" "" $ cleanAbstractsHTML $ processArxivAbstract $ trimTitle $ findTxt $ fst $ element "title" tags
                         let authors = cleanAuthors $ intercalate ", " $ getAuthorNames tags
                         let published = take 10 $ findTxt $ fst $ element "published" tags -- "2017-12-01T17:13:14Z" → "2017-12-01"
                         -- NOTE: Arxiv used to not provide its own DOIs; that changed in 2022: <https://blog.arxiv.org/2022/02/17/new-arxiv-articles-are-now-automatically-assigned-dois/>; so look for DOI and if not set, try to construct it automatically using their schema `10.48550/arXiv.2202.01037`
                         let doiTmp = processDOI $ findTxt $ fst $ element "arxiv:doi" tags
                         let doi = [("doi", if null doiTmp then processDOIArxiv url' else doiTmp)]
                         abst <- processParagraphizer url' $ linkAutoHtml5String $ cleanAbstractsHTML $ cleanAbstractsHTML $ processArxivAbstract $ findTxt $ fst $ element "summary" tags
                         let ts = [] :: [String] -- TODO: replace with ML call to infer tags
                         -- the API sometimes lags the website, and a valid Arxiv URL may not yet have obtainable abstracts, so it's a temporary failure:
                         if abst=="" || authors=="arXiv api core" || title=="Error" then
                           do printRed "Error: Arxiv parsing failed!"
                              printGreen ("Error details: failure on Arxiv URL "++url' ++"; Arxiv ID: " ++ arxivid ++ "; raw response: " ++ show bs ++ "; parsed data: " ++ show [show tags, title, authors, published, doiTmp, show doi, abst, show ts])
                              return (Left Temporary)
                           else return $ Right (url', (title,authors,published,"",doi,ts,abst))

arxivDownload :: String -> IO (ExitCode, U.ByteString, String)
arxivDownload url = do
               checkURL url
               let arxivid = takeWhile (/='#') $ if "/pdf/" `isInfixOf` url && ".pdf" `isSuffixOf` url
                                 then replaceMany [("https://arxiv.org/pdf/", ""), (".pdf", "")] url
                                 else replace "https://arxiv.org/abs/" "" url
               -- <https://info.arxiv.org/help/api/user-manual.html#_query_interface>
               (a,_,c) <- runShellCommand "./" Nothing "curl" ["--location","--silent","https://export.arxiv.org/api/query?id_list="++arxivid, "--user-agent", "gwern+arxivscraping@gwern.net"]
               return (a,c,arxivid)

-- NOTE: we inline Tagsoup convenience code from Network.Api.Arxiv (<https://hackage.haskell.org/package/arxiv-0.0.1/docs/src/Network-Api-Arxiv.html>); because that library is unmaintained & silently corrupts data (<https://github.com/toschoo/Haskell-Libs/issues/1>), we keep the necessary code close at hand so at least we can easily patch it when errors come up
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
processArxivAbstract :: String -> String
processArxivAbstract a = let cleaned = runPure $ do
                                     -- if we don't escape dollar signs, it breaks abstracts with dollar amounts like "a $700 GPU"; crude heuristic, if only 1 '$', then it's not being used for LaTeX math (eg. in <https://arxiv.org/abs/2108.05818#tencent>)
                                    let dollarSignsN = length $ filter (=='$') a
                                    let tex = sedMany C.arxivAbstractRegexps $
                                              replaceMany C.arxivAbstractFixedRewrites
                                                          $ (if dollarSignsN == 1 then replaceMany [("$", "\\$")] else id) a

                                    pandoc <- readLaTeX def{ readerExtensions = pandocExtensions } $ T.pack tex
                                      -- NOTE: an Arxiv API abstract can have any of '%', '\%', or '$\%$' in it. All of these are dangerous and potentially breaking downstream LaTeX parsers.

                                    writeHtml5String safeHtmlWriterOptions{writerWrapText=WrapNone, writerHTMLMathMethod = MathJax defaultMathJaxURL} $ walk (unsafePerformIO . inlineMath2Text) pandoc
              in case cleaned of
                 Left e -> error $ " : " ++ ppShow e ++ " : " ++ a
                 Right output -> cleanAbstractsHTML $ replaceMany C.cleanArxivAbstracts $ T.unpack output
