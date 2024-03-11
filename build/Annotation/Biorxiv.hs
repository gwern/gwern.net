module Annotation.Biorxiv where

import Data.List (isInfixOf, intercalate)
import Data.FileStore.Utils (runShellCommand)
import qualified Data.ByteString.Lazy.UTF8 as U (toString) -- TODO: why doesn't using U.toString fix the Unicode problems?
import Text.HTML.TagSoup (isTagOpenName, parseTags, Tag(TagOpen))
import Text.Show.Pretty (ppShow)
import System.Exit (ExitCode(ExitFailure))

import LinkAuto (linkAutoHtml5String)
import LinkMetadataTypes (Failure(..), MetadataItem, Path)
import MetadataFormat (checkURL, cleanAuthors, cleanAbstractsHTML, processDOI)
import Utils (printRed, replace)
import Paragraph (processParagraphizer)

-- handles medRxiv too (same codebase)
biorxiv  :: Path -> IO (Either Failure (Path, MetadataItem))
biorxiv p = do checkURL p
               if ".pdf" `isInfixOf` p then return (Right (p, ("", "", "", "", [], [], ""))) else
                do (status,_,bs) <- runShellCommand "./" Nothing "curl" ["--location", "--silent", p, "--user-agent", "gwern+biorxivscraping@gwern.net"]
                   case status of
                     ExitFailure _ -> printRed ("BioRxiv download failed: " ++ p) >> return (Left Permanent)
                     _ -> do
                            let b = U.toString bs
                            let f = parseTags b
                            let metas = filter (isTagOpenName "meta") f

                            let title = concat $ parseMetadataTagsoup "DC.Title" metas
                            if title=="" then printRed ("BioRxiv parsing failed: " ++ p ++ " : parsed metadata: " ++ ppShow metas ++ "\nParsed tags: " ++ show f) >> return (Left Permanent)
                              else do
                                     let date    = concat $ parseMetadataTagsoup "DC.Date" metas
                                     let doi     = processDOI $ concat $ parseMetadataTagsoup "citation_doi" metas
                                     let author  = cleanAuthors $ intercalate ", " $ filter (/="") $ parseMetadataTagsoup "DC.Contributor" metas
                                     let abstractRaw = concat $ parseMetadataTagsoupSecond "citation_abstract" metas
                                     let abstractRaw' = if not (null abstractRaw) then abstractRaw else concat $ parseMetadataTagsoup "DC.Description" metas
                                     abstrct <- processParagraphizer p $
                                                replace "9s" "s" $ -- BUG: BioRxiv abstracts have broken quote encoding. I reported this to them 2 years ago and they still have not fixed it.
                                                linkAutoHtml5String $ cleanAbstractsHTML abstractRaw'
                                     let ts = [] -- TODO: replace with ML call to infer tags
                                     if abstrct == "" then do printRed "BioRxiv parsing failed"
                                                              print ("Metas: " ++ show metas)
                                                              print abstractRaw
                                                              print abstractRaw'
                                                              return (Left Temporary)
                                       else
                                                       return $ Right (p, (title, author, date, "", [("doi",doi)], ts, abstrct))
  where
    parseMetadataTagsoup, parseMetadataTagsoupSecond :: String -> [Tag String] -> [String]
    parseMetadataTagsoup key = map (\(TagOpen _ (a:b)) ->  if snd a == key then snd $ head b else "")
      -- 'TagOpen "meta" [("name","citation_abstract"),("lang","en"),("content","<h3>ABSTRACT</h3>\n<p>The vast majority of human mutations have minor allele frequencies (MAF) under 1%, with the plurality observed only once (ie. \8220singletons\8221). While Mendelian diseases are predominantly caused by rare alleles, their role in complex phenotypes remains largely unknown. We develop and rigorously validate an approach to jointly estimate the contribution of alleles with different frequencies, including singletons, to phenotypic variation. We apply our approach to transcriptional regulation, an intermediate between genetic variation and complex disease. Using whole genome DNA and RNA sequencing data from 360 European individuals, we find that singletons alone contribute ~23% of all <i>cis</i>-heritability across genes (dwarfing the contributions of other frequencies). We then integrate external estimates of global MAF from worldwide samples to improve our inference, and find that average <i>cis</i>-heritability is 15.3%. Strikingly, 50.9% of <i>cis</i>-heritability is contributed by globally rare variants (MAF&lt;0.1%), implicating purifying selection as a pervasive force shaping the regulatory architecture of most human genes.</p><h3>One Sentence Summary</h3>\n<p>The vast majority of variants so far discovered in humans are rare, and together they have a substantial impact on gene regulation.</p>")]'
    parseMetadataTagsoupSecond key = map (\(TagOpen _ (a:b)) ->  if snd a == key then snd $ b!!1 else "")
