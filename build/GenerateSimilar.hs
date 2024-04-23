{-# LANGUAGE OverloadedStrings #-}

-- dependencies: pandoc, filestore, vector, rp-tree...

module GenerateSimilar where

import Text.Pandoc (def, nullMeta, pandocExtensions, readerExtensions, readHtml, writeHtml5String, Block(BulletList, Para), Inline(Link, RawInline, Span, Str, Strong), Format(..), runPure, Pandoc(..))
import Text.Pandoc.Walk (walk)
import qualified Data.Text as T  (append, intercalate, isPrefixOf, length, pack, strip, take, unlines, unpack, Text)
import qualified Data.Text.IO as TIO (readFile)
import Data.List ((\\), intercalate, tails, sort)
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map.Strict as M -- (filter, keys, lookup, fromList, toList, difference, withoutKeys, restrictKeys, member)
import System.Directory (doesFileExist, renameFile, removeFile)
import System.IO.Temp (emptySystemTempFile)
import Text.Read (readMaybe)
import Text.Show.Pretty (ppShow)
import Data.FileStore.Utils (runShellCommand)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import System.Exit (ExitCode(ExitFailure))
import qualified Data.Binary as DB (decodeFileOrFail, encodeFile)
import Network.HTTP (urlEncode)
import System.FilePath (takeBaseName)
import Control.Monad (when, foldM)
import qualified Data.Set as S (fromList, Set) -- toList

import qualified Data.Vector as V (toList, Vector)
import Control.Monad.Identity (runIdentity, Identity)
import Data.RPTree (knn, forest, metricL2, rpTreeCfg, fpMaxTreeDepth, fpDataChunkSize, fpProjNzDensity, fromListDv, DVector, Embed(..), RPForest, serialiseRPForest)
import Data.Conduit (ConduitT)
import Data.Conduit.List (sourceList)
-- import System.GlobalLock as GL (lock)

import LinkBacklink (getSimilarLink, getForwardLinks, readBacklinksDB, Backlinks)
import Columns as CL (listLength)
import LinkMetadata (readLinkMetadata, sortItemPathDate)
import LinkMetadataTypes (Metadata, MetadataItem)
import Typography (typographyTransform)
import Query (extractURLsAndAnchorTooltips, extractLinks)
import Utils (simplifiedDoc, simplifiedString, writeUpdatedFile, replace, safeHtmlWriterOptions, anyPrefixT, printRed, trim, sed, kvDOI)
import MetadataAuthor (authorsTruncateString)

import Config.Misc (todayDay, cd)
import Config.GenerateSimilar as C (bestNEmbeddings, iterationLimit, embeddingsPath, maximumLength, maxDistance, blackList, minimumSuggestions)

-- Make it easy to generate a HTML list of recommendations for an arbitrary piece of text. This is useful for eg. getting the list of recommendations while writing an annotation, to whitelist links or incorporate into the annotation directly (freeing up slots in the 'similar' tab for additional links). Used in `preprocess-markdown.hs`.
singleShotRecommendations :: String -> IO T.Text
singleShotRecommendations html =
  do md  <- readLinkMetadata
     edb <- readEmbeddings
     bdb <- readBacklinksDB

     newEmbedding <- embed [] md bdb ("",("","","","",[],[],html))
     ddb <- embeddings2Forest (newEmbedding:edb)
     let (_,hits) = findN ddb (2*C.bestNEmbeddings) C.iterationLimit (Just 1) newEmbedding :: (String,[String])
     sortDB <- readListSortedMagic
     hitsSorted <- sortSimilars edb sortDB (head hits) hits

     let matchListHtml = generateMatches md bdb True True "" html hitsSorted :: T.Text
     return matchListHtml

type Embedding  = (String, -- URL/path
                    Integer, -- Age: date created -- ModifiedJulianDay, eg. 2019-11-22 = 58810. Enables expiring
                    String, -- the as-embedded (mangled by 'formatDoc') text
                    String, -- OA API model embedding version/ID (important because comparing embeddings from different models is nonsense)
                    [Double]) -- the actual embedding vector; NOTE: 'Float' in Haskell is 32-bit single-precision float (FP32); OA API apparently returns 64-bit double-precision (FP64), so we use 'Double' instead. (Given the very small magnitude of the Doubles, it is probably a bad idea to try to save space/compute by casting to Float.)
type Embeddings = [Embedding]

last5 :: (a, b, c, d, e) -> e
last5  (_,_,_,_,e) = e
last4 :: (a, b, c, d) -> d
last4  (_,_,_,d) = d

readEmbeddings :: IO Embeddings
readEmbeddings = Config.Misc.cd >> readEmbeddingsPath C.embeddingsPath
readEmbeddingsPath :: FilePath -> IO Embeddings
readEmbeddingsPath p = do exists <- doesFileExist p
                          if not exists then return [] else
                            do eE <- DB.decodeFileOrFail p
                               case eE of
                                 Right e -> return e
                                 Left err -> error $ show err

writeEmbeddings :: Embeddings -> IO ()
writeEmbeddings es = do tempf <- emptySystemTempFile "hakyll-embeddings"
                        DB.encodeFile tempf es
                        es' <- readEmbeddingsPath tempf
                        if length es' < 0 then error "Embeddings corrupted! Not writing out." else renameFile tempf embeddingsPath

-- remove all embeddings without a corresponding Metadata entry; this generally means that it is a 'stale' embedding, corresponding to an outdated
-- URL, and so is a false positive or bloating the embeddings database.
pruneEmbeddings :: Metadata -> Embeddings -> Embeddings
pruneEmbeddings md edb = let edbDB = M.fromList $ map (\(a,b,c,d,e) -> (a,(b,c,d,e))) edb
                             invalidEmbeddings = S.fromList $ M.keys $ M.difference edbDB md
                             validEdbDB = M.withoutKeys edbDB invalidEmbeddings
                             in map (\(a,(b,c,d,e)) -> (a,b,c,d,e)) $ M.toList validEdbDB

missingEmbeddings :: Metadata -> Embeddings -> [(String, MetadataItem)]
missingEmbeddings md edb = let urlsToCheck = M.keys $ M.filter (\(_, _, _, _, _, _, abst) -> abst /= "") md
                               urlsEmbedded = map (\(u,_,_,_,_) -> u) edb :: [String]
                               missing      = urlsToCheck \\ urlsEmbedded
                               in map (\u -> (u, fromJust $ M.lookup u md)) missing

-- convert an annotated item into a single text string: concatenate the useful metadata in an OA API-aware way.
-- We need to avoid HTML, and try to write everything in an 'obvious' way that a NN model will understand 'out of the box' without finetuning.
-- Example of a processed text string for embedding:
--
-- > ‘Littlewood’s Law and the Global Media’, by Gwern Branwen (2018; updated 2020-01-01). Keywords: insight-porn, philosophy/epistemology, politics, psychology/cognitive-bias, psychology/personality/psychopathy, sociology/technology, statistics/bias.
-- >
-- > Selection effects in media become increasingly strong as populations and media increase, meaning that rare datapoints driven by unusual processes such as the mentally ill or hoaxers are increasingly unreliable as evidence of anything at all and must be ignored. At scale, anything that can happen will happen a small but nonzero times.
-- >
-- > Online & mainstream media and social networking have become increasingly misleading as to the state of the world by focusing on ‘stories’ and ‘events’ rather than trends and averages. This is because as the global population increases and the scope of media increases, media’s urge for narrative focuses on the most extreme outlier datapoints—but such datapoints are, at a global scale, deeply misleading as they are driven by unusual processes such as the mentally ill or hoaxers.
-- >
-- > At a global scale, anything that can happen will happen a small but nonzero times: this has been epitomized as “Littlewood’s Law: in the course of any normal person’s life, miracles happen at a rate of roughly one per month.” This must now be extended to a global scale for a hyper-networked global media covering anomalies from 8 billion people—all coincidences, hoaxes, mental illnesses, psychological oddities, extremes of continuums, mistakes, misunderstandings, terrorism, unexplained phenomena etc. Hence, there will be enough ‘miracles’ that all media coverage of events can potentially be composed of nothing but extreme outliers, even though it would seem like an ‘extraordinary’ claim to say that all media-reported events may be flukes.
-- >
-- > This creates an epistemic environment deeply hostile to understanding reality, one which is dedicated to finding arbitrary amounts of and amplifying the least representative datapoints.
-- >
-- > Given this, it is important to maintain extreme skepticism of any individual anecdotes or stories which are selectively reported but still claimed (often implicitly) to be representative of a general trend or fact about the world. Standard techniques like critical thinking, emphasizing trends & averages, and demanding original sources can help fight the biasing effect of news.
-- >
-- > -   Littlewood’s Law
-- >     -   Politics
-- >     -   Technology
-- >     -   Science
-- >     -   Media
-- >     -   Tails at Scales
-- > -   Epistemological Implications
-- > -   Coping
-- > -   See Also
-- > -   External Links
-- > -   Appendix
-- >     -   Origin Of “Littlewood’s Law of Miracles”
-- >
-- > Reverse citations:
-- >
-- > - "Blackmail fail", Gwern Branwen (2013)
-- > - "Hydrocephalus and Intelligence: The Hollow Men", Gwern Branwen (2015)
-- > - "Leprechaun Hunting & Citogenesis", Gwern Branwen (2014)
-- > - "Origin of ‘Littlewood’s Law of Miracles’", Gwern Branwen (2019)
-- > - "One Man’s Modus Ponens", Gwern Branwen (2012)
-- > - "How Should We Critique Research?", Gwern Branwen (2019)
-- > - "On Seeing Through and Unseeing: The Hacker Mindset", Gwern Branwen (2012)
-- > - "Lizardman Constant in Surveys", Gwern Branwen (2013)
-- > - "Book Reviews", Gwern Branwen (2013)
-- >
formatDoc :: (String,MetadataItem) -> T.Text
formatDoc (path,mi@(t,aut,dt,dtM,_,tags,abst)) =
    let dateModified = if dtM == "" then "" else "; updated " ++ dtM
        document = T.pack $ replace "\n" "\n\n" $ unlines [
          (if t=="" then "" else "'"++t++"'" ++
            if path=="" || head path == '/' then "" else " ("++path++")") ++
          (if aut=="" || aut=="N/A" then "" else ", by "++authorsTruncateString aut) ++
          (if dt==""then "." else" ("++take 4 dt++dateModified++")."),

          if null tags then "" else "Keywords: " ++ intercalate ", " tags ++ ".",

          replace "\n[]\n" "" $ replace "<hr>" "" $ replace "<hr />" "" abst]
        parsedEither = let parsed = runPure $ readHtml def{readerExtensions = pandocExtensions } document
                       in case parsed of
                          Left e -> error $ "Failed to parse HTML document into Pandoc AST: error: " ++ show e ++ " : " ++ show mi ++ " : " ++ T.unpack document
                          Right p -> p
        -- create a numbered list of URL references inside each document to expose it to the embedding model, as 'simplifiedDoc' necessarily strips URLs:
        documentURLs = filter (\(u,_) -> not (T.pack path `T.isPrefixOf` u) && anyPrefixT u ["/", "http"]) $ extractURLsAndAnchorTooltips parsedEither
        documentURLsText = if null documentURLs then "" else "\nReferences:\n\n" `T.append` T.unlines
          (map (\(n, (url,titles)) -> T.pack n `T.append` ". " `T.append` url `T.append` " " `T.append` T.intercalate ", " titles) $
            zip (map show [(1::Int)..]) documentURLs)
        -- simple plaintext ASCII-ish version, which hopefully is more comprehensible to NN models than full-blown HTML
        plainText = simplifiedDoc parsedEither `T.append` documentURLsText
        gptPlainText = T.take C.maximumLength $ T.strip plainText
    in
      gptPlainText

embed :: Embeddings -> Metadata -> Backlinks -> (String,MetadataItem) -> IO Embedding
embed edb mdb bdb i@(p,_) =
  -- an embedding may already exist for this, and the embedding firing because of a rename. As a heuristic, we check for any existing embedding with the same base filename, to catch cases of '/doc/foo/bar.pdf' → 'doc/foo/baz/bar.pdf'; when we bulk-rename large directories of annotated files, the false-positive embeddings can get expensive!
  if not (null olds) then let (_,b,c,d,e) = head olds in return (p,b,c,d,e)
    else do
            -- Flourish: look up the backlinks and append their title/author/date to the embedded document; should add useful keywords & context
            let backlinks = case M.lookup (T.pack p) bdb of
                              Nothing -> []
                              Just bl -> map T.unpack (concatMap snd bl)
            let backlinksMetadata = if null backlinks then "" else
                                      "\n\nReverse citations:\n\n- " ++ intercalate "\n- " (nubOrd $
                                        map (\b -> case M.lookup b mdb of
                                                    Nothing -> ""
                                                    Just (t,a,d,_,_,_,_) -> "\"" ++ t ++ "\", " ++ authorsTruncateString a ++ (if d=="" then "" else " (" ++ take 4 d ++ ")")) backlinks)

            let doc = formatDoc i `T.append` T.pack backlinksMetadata
            (modelType,embedding) <- oaAPIEmbed p doc
            today <- todayDay
            return (p,today,T.unpack doc,modelType,embedding)
 where new = takeBaseName p
       olds = filter (\(pold,_,_,_,_) -> if head pold == '/' then new == takeBaseName pold
                                         else dehttp new == dehttp pold) edb
       dehttp = replace "http://" "" . replace "https://" ""

-- we shell out to a Bash script `similar.sh` to do the actual curl + JSON processing; see it for details.
oaAPIEmbed :: FilePath -> T.Text -> IO (String,[Double])
oaAPIEmbed p doc = do (status,stderr,mb) <- runShellCommand "./" Nothing "bash" ["static/build/embed.sh", replace "\n" "\\n" $ -- JSON escaping of newlines
                                                                                                   T.unpack doc]
                      case status of
                        ExitFailure err -> error $ "Exit Failure: " ++ intercalate " ::: " [show (T.length doc), T.unpack doc, ppShow status, ppShow err, ppShow mb, show stderr]
                        _ -> do let results = lines $ U.toString mb
                                case results of
                                  [] -> error $ "Failed to read any embed.sh output at all for the path: " ++ p ++ "\n" ++ show (T.length doc) ++ "\n" ++ T.unpack doc ++ "\n" ++ U.toString mb ++ "\n" ++ show stderr
                                  (modelType:latents) -> let embeddingM = readMaybe (unlines latents) :: Maybe [Double] in
                                                           case embeddingM of
                                                             Nothing -> error $ "Failed to read embed.sh's generated embeddings? " ++ "\n" ++ show (T.length doc) ++ "\n" ++ T.unpack doc ++ "\n" ++ U.toString mb ++ "\n" ++ show stderr
                                                             Just embedding -> return (modelType, embedding)

type Distances = [(String, [String])]

type Forest = RPForest Double (V.Vector (Embed DVector Double String))

-- magic hyperparameters must be chosen based on embedding type. For 'ada-similarity', '21 5 12' works reasonably well; for 'curie-similarity', '60,1,32'; for 'davinci-similarity', recall was often high but results bad anyway so untrustworthy.
-- For curie, sweeping to find '60,1,32' produces a substantial gain compared to the ada-tuned '21,5,12', so well-worthwhile.
-- > hyperparameterSweep edb
-- [(0.8555555555555556,(60,1,32)),(0.46888888888888886,(21,5,12))]
-- TODO: I am not sure why it keeps picking '1' tree as optimum, and that seems like it might be related to the instances where no hits are returned?
embeddings2Forest :: Embeddings -> IO Forest
embeddings2Forest []     = error "embeddings2Forest called with no arguments, which is meaningless."
embeddings2Forest [_]    = error "embeddings2Forest called with only 1 arguments, which is useless."
embeddings2Forest e = do let f = embeddings2ForestConfigurable 16 4 32 e
                         let fl = serialiseRPForest f
                         when (length fl < 2) $ error "embeddings2Forest: serialiseRPForest returned an invalid empty result on the output of embeddings2ForestConfigurable‽"
                         return f

embeddings2ForestConfigurable :: Int -> Int -> Int -> Embeddings -> Forest
embeddings2ForestConfigurable ls nt pvd es =
  let minLeafSize = ls -- ???
      cfg = rpTreeCfg minLeafSize
              (length es) -- data N
              (length $ (\(_,_,_,_,embedding) -> embedding) $ head es) -- dimension of each datapoint (eg. 1024 for ada-similarity embeddings, 12288 for davinci)
      nTrees = nt -- ???
      projectionVectorDimension = pvd -- ???
      randSeed = 14
  in
    runIdentity $
    forest randSeed (fpMaxTreeDepth cfg) minLeafSize nTrees (fpDataChunkSize cfg) (fpProjNzDensity cfg) projectionVectorDimension $
    embeddings2Conduit es
 where
   embeddings2Conduit :: Embeddings -> ConduitT () (Embed DVector Double String) Identity ()
   embeddings2Conduit = sourceList . map (\(p,_,_,_,embedding) -> Embed (fromListDv embedding) p)


knnEmbedding :: Forest -> Int -> Embedding -> [(Double, Embed DVector Double String)]
knnEmbedding f k (_,_,_,_,embd) = V.toList $
                               -- NOTE: 'metricL2' *seems* to be the L2-normalized Euclidean distance? which is *proportional* to cosine similarity/distance: not identical, but produces the same ranking, and so just as good for my purpose here? or so claims https://stats.stackexchange.com/a/146279/16897
                               -- 'inner' also works, type-wise, but produces terrible results on the GPT-3-ada embeddings; this is apparently due to the extremely similar magnitude of the embeddings, and dot-product not working as well as cosine similarity on language model embeddings is apparently common and expected.
                              -- 'knn', 'knnH', 'knnPQ': knnH/knnPQ always perform way worse for me.
                               knn metricL2 k f (fromListDv embd :: DVector Double)


-- we'll filter based on acceptable distance
findNearest :: Forest -> Int -> Double -> Embedding -> [String]
findNearest f k maxDist e = map (\(_,Embed _ p) -> p) $ filter (\(dist,_) -> dist < maxDist) $ knnEmbedding f k e

findN :: Forest -> Int -> Int -> Maybe Double -> Embedding -> (String,[String])
findN _ 0 _    _ e = error ("findN called for k=0; embedding target: " ++ show e)
findN _ _ 0    _ e = error ("findN failed to return enough candidates within iteration loop limit. Something went wrong! Embedding target: " ++ show e)
findN f k iter Nothing e = findN f k iter (Just  C.maxDistance) e
findN f k iter j@(Just mx) e@(p1,_,_,_,_) = let results = take C.bestNEmbeddings $ nubOrd $ filter (\p2 -> p2/="" && not (C.blackList p2) && p1 /= p2) $ findNearest f k mx e in
                 -- NOTE: 'knn' is the fastest (and most accurate?), but seems to return duplicate results, so requesting 10 doesn't return 10 unique hits.
                 -- (I'm not sure why, the rp-tree docs don't mention or warn about this that I noticed…)
                 -- If that happens, back off and request more k up to a max of 50.
                 if k>50 then (p1, [])
                 else if length results < C.bestNEmbeddings then findN f (k*2) (iter - 1) j e else (p1,results)

-- hyperparameterSweep :: Embeddings -> [(Double, (Int,Int,Int))]
-- hyperparameterSweep edb =
--   -- sweep over `R> expand.grid(seq(30,100,by=10), seq(1,10,by=2), seq(2,200,by=10))` (n=800), ranges chosen by trying some hand optimization below
--   let gridsearchParameters = [(60,1,32), (21,5,12)]
--   -- let gridsearchParameters = [(30,1,2), (40,1,2), (50,1,2), (60,1,2), (70,1,2), (80,1,2), (90,1,2), (100,1,2), (30,3,2), (40,3,2), (50,3,2), (60,3,2), (70,3,2), (80,3,2), (90,3,2), (100,3,2), (30,5,2), (40,5,2), (50,5,2), (60,5,2), (70,5,2), (80,5,2), (90,5,2), (100,5,2), (30,7,2), (40,7,2), (50,7,2), (60,7,2), (70,7,2), (80,7,2), (90,7,2), (100,7,2), (30,9,2), (40,9,2), (50,9,2), (60,9,2), (70,9,2), (80,9,2), (90,9,2), (100,9,2), (30,1,12), (40,1,12), (50,1,12), (60,1,12), (70,1,12), (80,1,12), (90,1,12), (100,1,12), (30,3,12), (40,3,12), (50,3,12), (60,3,12), (70,3,12), (80,3,12), (90,3,12), (100,3,12), (30,5,12), (40,5,12), (50,5,12), (60,5,12), (70,5,12), (80,5,12), (90,5,12), (100,5,12), (30,7,12), (40,7,12), (50,7,12), (60,7,12), (70,7,12), (80,7,12), (90,7,12), (100,7,12), (30,9,12), (40,9,12), (50,9,12), (60,9,12), (70,9,12), (80,9,12), (90,9,12), (100,9,12), (30,1,22), (40,1,22), (50,1,22), (60,1,22), (70,1,22), (80,1,22), (90,1,22), (100,1,22), (30,3,22), (40,3,22), (50,3,22), (60,3,22), (70,3,22), (80,3,22), (90,3,22), (100,3,22), (30,5,22), (40,5,22), (50,5,22), (60,5,22), (70,5,22), (80,5,22), (90,5,22), (100,5,22), (30,7,22), (40,7,22), (50,7,22), (60,7,22), (70,7,22), (80,7,22), (90,7,22), (100,7,22), (30,9,22), (40,9,22), (50,9,22), (60,9,22), (70,9,22), (80,9,22), (90,9,22), (100,9,22), (30,1,32), (40,1,32), (50,1,32), (60,1,32), (70,1,32), (80,1,32), (90,1,32), (100,1,32), (30,3,32), (40,3,32), (50,3,32), (60,3,32), (70,3,32), (80,3,32), (90,3,32), (100,3,32), (30,5,32), (40,5,32), (50,5,32), (60,5,32), (70,5,32), (80,5,32), (90,5,32), (100,5,32), (30,7,32), (40,7,32), (50,7,32), (60,7,32), (70,7,32), (80,7,32), (90,7,32), (100,7,32), (30,9,32), (40,9,32), (50,9,32), (60,9,32), (70,9,32), (80,9,32), (90,9,32), (100,9,32), (30,1,42), (40,1,42), (50,1,42), (60,1,42), (70,1,42), (80,1,42), (90,1,42), (100,1,42), (30,3,42), (40,3,42), (50,3,42), (60,3,42), (70,3,42), (80,3,42), (90,3,42), (100,3,42), (30,5,42), (40,5,42), (50,5,42), (60,5,42), (70,5,42), (80,5,42), (90,5,42), (100,5,42), (30,7,42), (40,7,42), (50,7,42), (60,7,42), (70,7,42), (80,7,42), (90,7,42), (100,7,42), (30,9,42), (40,9,42), (50,9,42), (60,9,42), (70,9,42), (80,9,42), (90,9,42), (100,9,42), (30,1,52), (40,1,52), (50,1,52), (60,1,52), (70,1,52), (80,1,52), (90,1,52), (100,1,52), (30,3,52), (40,3,52), (50,3,52), (60,3,52), (70,3,52), (80,3,52), (90,3,52), (100,3,52), (30,5,52), (40,5,52), (50,5,52), (60,5,52), (70,5,52), (80,5,52), (90,5,52), (100,5,52), (30,7,52), (40,7,52), (50,7,52), (60,7,52), (70,7,52), (80,7,52), (90,7,52), (100,7,52), (30,9,52), (40,9,52), (50,9,52), (60,9,52), (70,9,52), (80,9,52), (90,9,52), (100,9,52), (30,1,62), (40,1,62), (50,1,62), (60,1,62), (70,1,62), (80,1,62), (90,1,62), (100,1,62), (30,3,62), (40,3,62), (50,3,62), (60,3,62), (70,3,62), (80,3,62), (90,3,62), (100,3,62), (30,5,62), (40,5,62), (50,5,62), (60,5,62), (70,5,62), (80,5,62), (90,5,62), (100,5,62), (30,7,62), (40,7,62), (50,7,62), (60,7,62), (70,7,62), (80,7,62), (90,7,62), (100,7,62), (30,9,62), (40,9,62), (50,9,62), (60,9,62), (70,9,62), (80,9,62), (90,9,62), (100,9,62), (30,1,72), (40,1,72), (50,1,72), (60,1,72), (70,1,72), (80,1,72), (90,1,72), (100,1,72), (30,3,72), (40,3,72), (50,3,72), (60,3,72), (70,3,72), (80,3,72), (90,3,72), (100,3,72), (30,5,72), (40,5,72), (50,5,72), (60,5,72), (70,5,72), (80,5,72), (90,5,72), (100,5,72), (30,7,72), (40,7,72), (50,7,72), (60,7,72), (70,7,72), (80,7,72), (90,7,72), (100,7,72), (30,9,72), (40,9,72), (50,9,72), (60,9,72), (70,9,72), (80,9,72), (90,9,72), (100,9,72), (30,1,82), (40,1,82), (50,1,82), (60,1,82), (70,1,82), (80,1,82), (90,1,82), (100,1,82), (30,3,82), (40,3,82), (50,3,82), (60,3,82), (70,3,82), (80,3,82), (90,3,82), (100,3,82), (30,5,82), (40,5,82), (50,5,82), (60,5,82), (70,5,82), (80,5,82), (90,5,82), (100,5,82), (30,7,82), (40,7,82), (50,7,82), (60,7,82), (70,7,82), (80,7,82), (90,7,82), (100,7,82), (30,9,82), (40,9,82), (50,9,82), (60,9,82), (70,9,82), (80,9,82), (90,9,82), (100,9,82), (30,1,92), (40,1,92), (50,1,92), (60,1,92), (70,1,92), (80,1,92), (90,1,92), (100,1,92), (30,3,92), (40,3,92), (50,3,92), (60,3,92), (70,3,92), (80,3,92), (90,3,92), (100,3,92), (30,5,92), (40,5,92), (50,5,92), (60,5,92), (70,5,92), (80,5,92), (90,5,92), (100,5,92), (30,7,92), (40,7,92), (50,7,92), (60,7,92), (70,7,92), (80,7,92), (90,7,92), (100,7,92), (30,9,92), (40,9,92), (50,9,92), (60,9,92), (70,9,92), (80,9,92), (90,9,92), (100,9,92), (30,1,102), (40,1,102), (50,1,102), (60,1,102), (70,1,102), (80,1,102), (90,1,102), (100,1,102), (30,3,102), (40,3,102), (50,3,102), (60,3,102), (70,3,102), (80,3,102), (90,3,102), (100,3,102), (30,5,102), (40,5,102), (50,5,102), (60,5,102), (70,5,102), (80,5,102), (90,5,102), (100,5,102), (30,7,102), (40,7,102), (50,7,102), (60,7,102), (70,7,102), (80,7,102), (90,7,102), (100,7,102), (30,9,102), (40,9,102), (50,9,102), (60,9,102), (70,9,102), (80,9,102), (90,9,102), (100,9,102), (30,1,112), (40,1,112), (50,1,112), (60,1,112), (70,1,112), (80,1,112), (90,1,112), (100,1,112), (30,3,112), (40,3,112), (50,3,112), (60,3,112), (70,3,112), (80,3,112), (90,3,112), (100,3,112), (30,5,112), (40,5,112), (50,5,112), (60,5,112), (70,5,112), (80,5,112), (90,5,112), (100,5,112), (30,7,112), (40,7,112), (50,7,112), (60,7,112), (70,7,112), (80,7,112), (90,7,112), (100,7,112), (30,9,112), (40,9,112), (50,9,112), (60,9,112), (70,9,112), (80,9,112), (90,9,112), (100,9,112), (30,1,122), (40,1,122), (50,1,122), (60,1,122), (70,1,122), (80,1,122), (90,1,122), (100,1,122), (30,3,122), (40,3,122), (50,3,122), (60,3,122), (70,3,122), (80,3,122), (90,3,122), (100,3,122), (30,5,122), (40,5,122), (50,5,122), (60,5,122), (70,5,122), (80,5,122), (90,5,122), (100,5,122), (30,7,122), (40,7,122), (50,7,122), (60,7,122), (70,7,122), (80,7,122), (90,7,122), (100,7,122), (30,9,122), (40,9,122), (50,9,122), (60,9,122), (70,9,122), (80,9,122), (90,9,122), (100,9,122), (30,1,132), (40,1,132), (50,1,132), (60,1,132), (70,1,132), (80,1,132), (90,1,132), (100,1,132), (30,3,132), (40,3,132), (50,3,132), (60,3,132), (70,3,132), (80,3,132), (90,3,132), (100,3,132), (30,5,132), (40,5,132), (50,5,132), (60,5,132), (70,5,132), (80,5,132), (90,5,132), (100,5,132), (30,7,132), (40,7,132), (50,7,132), (60,7,132), (70,7,132), (80,7,132), (90,7,132), (100,7,132), (30,9,132), (40,9,132), (50,9,132), (60,9,132), (70,9,132), (80,9,132), (90,9,132), (100,9,132), (30,1,142), (40,1,142), (50,1,142), (60,1,142), (70,1,142), (80,1,142), (90,1,142), (100,1,142), (30,3,142), (40,3,142), (50,3,142), (60,3,142), (70,3,142), (80,3,142), (90,3,142), (100,3,142), (30,5,142), (40,5,142), (50,5,142), (60,5,142), (70,5,142), (80,5,142), (90,5,142), (100,5,142), (30,7,142), (40,7,142), (50,7,142), (60,7,142), (70,7,142), (80,7,142), (90,7,142), (100,7,142), (30,9,142), (40,9,142), (50,9,142), (60,9,142), (70,9,142), (80,9,142), (90,9,142), (100,9,142), (30,1,152), (40,1,152), (50,1,152), (60,1,152), (70,1,152), (80,1,152), (90,1,152), (100,1,152), (30,3,152), (40,3,152), (50,3,152), (60,3,152), (70,3,152), (80,3,152), (90,3,152), (100,3,152), (30,5,152), (40,5,152), (50,5,152), (60,5,152), (70,5,152), (80,5,152), (90,5,152), (100,5,152), (30,7,152), (40,7,152), (50,7,152), (60,7,152), (70,7,152), (80,7,152), (90,7,152), (100,7,152), (30,9,152), (40,9,152), (50,9,152), (60,9,152), (70,9,152), (80,9,152), (90,9,152), (100,9,152), (30,1,162), (40,1,162), (50,1,162), (60,1,162), (70,1,162), (80,1,162), (90,1,162), (100,1,162), (30,3,162), (40,3,162), (50,3,162), (60,3,162), (70,3,162), (80,3,162), (90,3,162), (100,3,162), (30,5,162), (40,5,162), (50,5,162), (60,5,162), (70,5,162), (80,5,162), (90,5,162), (100,5,162), (30,7,162), (40,7,162), (50,7,162), (60,7,162), (70,7,162), (80,7,162), (90,7,162), (100,7,162), (30,9,162), (40,9,162), (50,9,162), (60,9,162), (70,9,162), (80,9,162), (90,9,162), (100,9,162), (30,1,172), (40,1,172), (50,1,172), (60,1,172), (70,1,172), (80,1,172), (90,1,172), (100,1,172), (30,3,172), (40,3,172), (50,3,172), (60,3,172), (70,3,172), (80,3,172), (90,3,172), (100,3,172), (30,5,172), (40,5,172), (50,5,172), (60,5,172), (70,5,172), (80,5,172), (90,5,172), (100,5,172), (30,7,172), (40,7,172), (50,7,172), (60,7,172), (70,7,172), (80,7,172), (90,7,172), (100,7,172), (30,9,172), (40,9,172), (50,9,172), (60,9,172), (70,9,172), (80,9,172), (90,9,172), (100,9,172), (30,1,182), (40,1,182), (50,1,182), (60,1,182), (70,1,182), (80,1,182), (90,1,182), (100,1,182), (30,3,182), (40,3,182), (50,3,182), (60,3,182), (70,3,182), (80,3,182), (90,3,182), (100,3,182), (30,5,182), (40,5,182), (50,5,182), (60,5,182), (70,5,182), (80,5,182), (90,5,182), (100,5,182), (30,7,182), (40,7,182), (50,7,182), (60,7,182), (70,7,182), (80,7,182), (90,7,182), (100,7,182), (30,9,182), (40,9,182), (50,9,182), (60,9,182), (70,9,182), (80,9,182), (90,9,182), (100,9,182), (30,1,192), (40,1,192), (50,1,192), (60,1,192), (70,1,192), (80,1,192), (90,1,192), (100,1,192), (30,3,192), (40,3,192), (50,3,192), (60,3,192), (70,3,192), (80,3,192), (90,3,192), (100,3,192), (30,5,192), (40,5,192), (50,5,192), (60,5,192), (70,5,192), (80,5,192), (90,5,192), (100,5,192), (30,7,192), (40,7,192), (50,7,192), (60,7,192), (70,7,192), (80,7,192), (90,7,192), (100,7,192), (30,9,192), (40,9,192), (50,9,192), (60,9,192), (70,9,192), (80,9,192), (90,9,192), (100,9,192)] :: [(Int,Int,Int)]
--       evalOne f'' i = recallWith metricL2 f'' 10 $ (\(_,_,embd) -> ((fromListDv embd)::DVector Double)) $ edb!!i
--       eval f' = sum (parMap rseq (evalOne f') [4585, 5768, 421, 6103, 1000, 2000, 4000, 5000, 6000]) / 9 -- spot check a few picked at random
--   in
--    reverse $ sort $
--      parMap rseq (\z@(a,b,c) -> let f = embeddings2ForestConfigurable a b c edb in (eval f, z) ) gridsearchParameters
-- [(0.8555555555555556,(80,1,32)),(0.8555555555555556,(70,1,32)),(0.8555555555555556,(60,1,32)),(0.8333333333333334,(90,1,32)),(0.8,(50,1,32)),(0.8,(40,1,32)),(0.7888888888888889,(90,1,142)),(0.7888888888888889,(30,1,32)),(0.7777777777777779,(100,1,32)),(0.7777777777777778,(80,1,142)),(0.7777777777777778,(60,1,82)),(0.7555555555555556,(90,1,82)),(0.7555555555555555,(100,1,142)),(0.7444444444444445,(30,1,2)),(0.7444444444444444,(80,1,82)),(0.7444444444444444,(70,1,82)),(0.7222222222222222,(90,1,22)),(0.7222222222222222,(40,1,2)),(0.7111111111111111,(90,1,62)),(0.7111111111111111,(80,1,62))]
-- [(0.8555555555555556,(80,1,32)),(0.8555555555555556,(70,1,32)),(0.8555555555555556,(60,1,32)),(0.8333333333333334,(90,1,32)),(0.8,(50,1,32)),(0.8,(40,1,32)),(0.7888888888888889,(90,1,142)),(0.7888888888888889,(30,1,32)),(0.7777777777777779,(100,1,32)),(0.7777777777777778,(80,1,142)),(0.7777777777777778,(60,1,82)),(0.7555555555555556,(90,1,82)),(0.7555555555555555,(100,1,142)),(0.7444444444444445,(30,1,2)),(0.7444444444444444,(80,1,82)),(0.7444444444444444,(70,1,82)),(0.7222222222222222,(90,1,22)),(0.7222222222222222,(40,1,2)),(0.7111111111111111,(90,1,62)),(0.7111111111111111,(80,1,62)),(0.7111111111111111,(50,1,2)),(0.7037037037037036,(70,3,12)),(0.7,(70,1,62)),(0.7,(70,1,22)),(0.7,(60,1,62)),(0.6999999999999998,(60,3,12)),(0.6962962962962963,(80,3,32)),(0.6925925925925926,(100,3,12)),(0.6925925925925925,(80,3,12)),(0.6888888888888888,(80,1,22)),(0.6888888888888888,(60,1,22)),(0.6814814814814815,(90,3,32)),(0.6777777777777778,(90,1,182)),(0.6777777777777778,(60,1,182)),(0.6777777777777777,(70,3,102)),(0.6777777777777777,(70,3,32)),(0.674074074074074,(80,3,102)),(0.673015873015873,(60,7,12)),(0.6714285714285714,(70,7,12)),(0.6703703703703704,(60,3,32)),(0.6666666666666667,(80,1,122)),(0.6666666666666666,(80,1,182)),(0.6666666666666666,(70,1,182)),(0.6666666666666666,(30,1,102)),(0.6654320987654321,(70,9,12)),(0.665079365079365,(80,7,12)),(0.6617283950617283,(60,9,12)),(0.6604938271604938,(80,9,12)),(0.6555555555555556,(90,5,52)),(0.6555555555555556,(90,1,122)),(0.6555555555555556,(70,1,122)),(0.6555555555555554,(90,1,102)),(0.6555555555555554,(80,1,102)),(0.6555555555555554,(70,1,102)),(0.6533333333333333,(70,5,52)),(0.6511111111111112,(70,5,12)),(0.6511111111111111,(80,5,52)),(0.6511111111111111,(60,5,12)),(0.6507936507936508,(90,7,12)),(0.6481481481481483,(90,3,132)),(0.6481481481481481,(70,3,52)),(0.6444444444444445,(100,1,52)),(0.6444444444444445,(90,1,132)),(0.6444444444444444,(90,3,52)),(0.6444444444444444,(60,1,122)),(0.6444444444444444,(50,1,102)),(0.6444444444444444,(40,1,102)),(0.6432098765432098,(90,9,12)),(0.6422222222222222,(80,5,12)),(0.6407407407407407,(80,3,52)),(0.6400000000000001,(60,5,52)),(0.6370370370370371,(90,3,102)),(0.637037037037037,(100,3,32)),(0.6365079365079366,(100,7,12)),(0.6333333333333334,(100,9,12)),(0.6333333333333334,(100,1,102)),(0.6333333333333333,(100,1,62)),(0.6333333333333333,(90,3,12)),(0.6333333333333333,(70,1,12)),(0.6333333333333333,(60,1,102)),(0.6285714285714286,(80,7,32)),(0.625925925925926,(80,3,132)),(0.625925925925926,(30,3,62)),(0.625925925925926,(30,3,2)),(0.6259259259259259,(60,3,52)),(0.6259259259259259,(40,3,2)),(0.6244444444444445,(90,5,182)),(0.6222222222222222,(100,3,132)),(0.6222222222222222,(80,1,132)),(0.6222222222222222,(70,1,42)),(0.6222222222222222,(60,1,12)),(0.6222222222222222,(40,3,102)),(0.6222222222222222,(40,1,82)),(0.6206349206349207,(70,7,32)),(0.6190476190476191,(60,7,32)),(0.6185185185185185,(50,3,2)),(0.6177777777777779,(60,5,182)),(0.6177777777777778,(70,5,82)),(0.6174603174603175,(90,7,92)),(0.6174603174603175,(60,7,182)),(0.6158730158730159,(80,7,182)),(0.6158730158730159,(60,7,92)),(0.6155555555555555,(80,5,182)),(0.6155555555555555,(80,5,32)),(0.6148148148148149,(90,3,182)),(0.6148148148148149,(60,3,172)),(0.6142857142857143,(70,7,182)),(0.6133333333333334,(60,5,82)),(0.6133333333333333,(90,5,82)),(0.6111111111111112,(100,3,102)),(0.6111111111111112,(100,1,132)),(0.6111111111111112,(100,1,82)),(0.6111111111111112,(100,1,22)),(0.6111111111111112,(90,9,82)),(0.6111111111111112,(90,7,52)),(0.6111111111111112,(90,3,172)),(0.6111111111111112,(90,1,52)),(0.6111111111111112,(90,1,12)),(0.6111111111111112,(80,7,52)),(0.6111111111111112,(80,1,42)),(0.6111111111111112,(80,1,12)),(0.6111111111111112,(70,7,92)),(0.6111111111111112,(70,7,52)),(0.6111111111111112,(70,3,182)),(0.6111111111111112,(70,3,132)),(0.6111111111111112,(70,1,52)),(0.6111111111111112,(60,1,132)),(0.6111111111111112,(60,1,52)),(0.6111111111111112,(60,1,42)),(0.6111111111111112,(30,3,42)),(0.6111111111111112,(30,1,182)),(0.6111111111111112,(30,1,42)),(0.611111111111111,(50,1,12)),(0.6095238095238096,(60,7,52)),(0.6088888888888889,(70,5,182)),(0.6088888888888888,(90,5,12)),(0.6079365079365079,(80,7,92)),(0.6074074074074075,(60,3,182)),(0.6074074074074074,(90,3,82)),(0.6074074074074074,(80,3,182)),(0.6074074074074074,(70,9,82)),(0.6074074074074074,(60,3,102)),(0.6074074074074074,(50,3,102)),(0.6074074074074073,(50,3,52)),(0.6061728395061728,(60,9,82)),(0.6047619047619048,(90,7,182)),(0.6047619047619048,(90,7,82)),(0.6044444444444443,(100,5,12)),(0.6044444444444443,(90,5,162)),(0.6044444444444443,(90,5,32)),(0.6044444444444443,(70,5,32)),(0.6037037037037037,(80,3,42)),(0.6037037037037037,(70,3,82)),(0.6037037037037037,(70,3,42)),(0.6024691358024692,(60,9,182)),(0.6012345679012346,(80,9,182)),(0.6012345679012346,(70,9,182)),(0.6000000000000001,(90,5,22)),(0.6000000000000001,(90,1,112)),(0.6000000000000001,(80,3,172)),(0.6000000000000001,(80,1,52)),(0.6000000000000001,(70,1,132)),(0.6000000000000001,(60,9,92)),(0.6000000000000001,(60,7,82)),(0.6000000000000001,(30,3,102)),(0.6,(30,1,22)),(0.5999999999999999,(100,5,22)),(0.5984126984126985,(70,7,82)),(0.5977777777777777,(40,5,152)),(0.5975308641975308,(80,9,82)),(0.5968253968253969,(90,7,32)),(0.5962962962962963,(60,3,162)),(0.5955555555555556,(80,5,162)),(0.5938271604938271,(60,9,32)),(0.5933333333333333,(80,5,82)),(0.5933333333333333,(60,5,32)),(0.5925925925925927,(90,3,162)),(0.5925925925925927,(70,9,92)),(0.5925925925925926,(60,3,82)),(0.5925925925925926,(40,3,52)),(0.5925925925925926,(40,3,42)),(0.591358024691358,(80,9,32)),(0.5911111111111111,(80,5,42)),(0.5911111111111109,(60,5,162)),(0.588888888888889,(60,1,192)),(0.588888888888889,(40,1,182)),(0.588888888888889,(40,1,42)),(0.5888888888888889,(80,3,82)),(0.5888888888888889,(60,3,42)),(0.5888888888888889,(50,1,82)),(0.5888888888888889,(50,1,22)),(0.5876543209876544,(90,9,92)),(0.5876543209876544,(80,9,92)),(0.5876543209876544,(70,9,32)),(0.5873015873015872,(100,7,182)),(0.5866666666666667,(100,5,52)),(0.5866666666666667,(70,5,42)),(0.5857142857142857,(80,7,82)),(0.5851851851851853,(70,3,172)),(0.5844444444444444,(100,5,32)),(0.5839506172839506,(90,9,182)),(0.5827160493827162,(60,9,72)),(0.5822222222222222,(100,5,182)),(0.5822222222222222,(90,5,92)),(0.5822222222222222,(70,5,92)),(0.5822222222222222,(60,5,92)),(0.5822222222222222,(50,5,152)),(0.5822222222222222,(30,5,42)),(0.5814814814814816,(50,3,62)),(0.5814814814814815,(80,9,42)),(0.5814814814814815,(40,3,32)),(0.5814814814814815,(30,3,52)),(0.5793650793650794,(100,7,32)),(0.5790123456790123,(70,9,42)),(0.577777777777778,(40,3,62)),(0.5777777777777778,(100,3,52)),(0.5777777777777778,(100,1,182)),(0.5777777777777778,(70,1,192)),(0.5777777777777778,(50,1,182)),(0.5777777777777778,(50,1,42)),(0.5777777777777778,(40,7,152)),(0.5777777777777778,(30,1,12)),(0.5777777777777777,(100,3,142)),(0.5777777777777777,(70,5,162)),(0.5777777777777777,(40,1,22)),(0.5755555555555555,(100,5,162)),(0.5746031746031746,(90,7,152)),(0.5740740740740741,(90,9,52)),(0.5740740740740741,(80,3,162)),(0.5740740740740741,(70,3,162)),(0.574074074074074,(80,9,72)),(0.574074074074074,(60,9,42)),(0.574074074074074,(50,3,42)),(0.573015873015873,(80,7,42)),(0.5728395061728395,(90,9,152)),(0.5716049382716049,(80,9,52)),(0.5711111111111111,(90,5,152)),(0.5711111111111111,(90,5,42)),(0.5711111111111111,(60,5,42)),(0.5711111111111111,(30,5,152)),(0.5703703703703704,(90,3,152)),(0.5703703703703704,(50,3,32)),(0.5703703703703703,(90,9,42)),(0.5703703703703703,(90,3,42)),(0.5698412698412699,(90,7,172)),(0.5698412698412698,(90,7,162)),(0.5698412698412698,(70,7,42)),(0.5691358024691358,(70,9,52)),(0.5688888888888889,(80,5,92)),(0.5688888888888889,(80,5,22)),(0.5688888888888889,(40,5,42)),(0.5666666666666668,(90,7,112)),(0.5666666666666668,(60,9,52)),(0.5666666666666668,(60,3,62)),(0.5666666666666668,(30,7,62)),(0.5666666666666667,(100,3,182)),(0.5666666666666667,(100,1,12)),(0.5666666666666667,(80,7,62)),(0.5666666666666667,(60,5,22)),(0.5666666666666667,(40,1,12)),(0.5654320987654322,(90,9,22)),(0.5650793650793651,(70,7,62)),(0.5644444444444444,(70,5,22)),(0.5641975308641975,(90,9,72)),(0.5634920634920635,(50,7,152)),(0.562962962962963,(80,9,22)),(0.562962962962963,(70,9,22)),(0.5629629629629629,(100,3,162)),(0.5629629629629629,(90,9,32)),(0.5629629629629629,(60,3,132)),(0.561904761904762,(80,7,172)),(0.561904761904762,(60,7,172)),(0.5604938271604938,(100,9,92)),(0.5604938271604938,(100,9,72)),(0.5604938271604938,(60,9,22)),(0.5603174603174605,(100,7,52)),(0.5603174603174603,(100,7,162)),(0.5603174603174603,(100,7,92)),(0.5603174603174603,(90,7,62)),(0.5603174603174603,(60,7,62)),(0.5603174603174603,(60,7,42)),(0.5603174603174603,(30,7,152)),(0.5603174603174602,(80,7,162)),(0.5592592592592593,(70,3,62)),(0.5592592592592592,(30,3,32)),(0.5587301587301587,(90,7,22)),(0.5587301587301586,(90,7,42)),(0.5580246913580247,(70,9,72)),(0.5577777777777778,(50,5,42)),(0.5577777777777777,(100,5,142)),(0.5571428571428572,(80,7,152)),(0.5571428571428572,(50,7,62)),(0.5555555555555557,(80,3,62)),(0.5555555555555557,(80,1,192)),(0.5555555555555556,(100,3,22)),(0.5555555555555556,(100,1,192)),(0.5555555555555556,(100,1,112)),(0.5555555555555556,(90,3,62)),(0.5555555555555556,(80,9,152)),(0.5555555555555556,(70,7,172)),(0.554320987654321,(100,9,182)),(0.553968253968254,(100,7,72)),(0.553968253968254,(60,7,162)),(0.5533333333333333,(100,5,82)),(0.5518518518518518,(90,9,162)),(0.5518518518518518,(40,3,82)),(0.5493827160493827,(100,9,82)),(0.5492063492063493,(60,7,72)),(0.5481481481481483,(100,3,152)),(0.5476190476190477,(100,7,22)),(0.546031746031746,(100,7,152)),(0.546031746031746,(100,7,82)),(0.5444444444444446,(60,9,172)),(0.5444444444444445,(100,3,82)),(0.5444444444444445,(100,1,122)),(0.5444444444444445,(40,7,62)),(0.5444444444444445,(30,7,42)),(0.5444444444444444,(90,3,142)),(0.5444444444444444,(30,1,62)),(0.5428571428571429,(70,7,162)),(0.5428571428571429,(50,7,42)),(0.5422222222222222,(80,5,152)),(0.5419753086419754,(100,9,152)),(0.5419753086419754,(70,9,102)),(0.5407407407407407,(100,9,22)),(0.5407407407407407,(100,3,172)),(0.5407407407407407,(100,3,42)),(0.5407407407407407,(80,9,162)),(0.5407407407407407,(80,9,62)),(0.5407407407407407,(50,3,82)),(0.54,(80,5,102)),(0.54,(70,5,102)),(0.54,(60,5,172)),(0.5399999999999999,(100,5,152)),(0.5395061728395062,(70,9,172)),(0.5395061728395061,(100,9,32)),(0.5382716049382716,(80,9,172)),(0.5382716049382716,(50,9,42)),(0.5382716049382716,(30,9,72)),(0.5380952380952382,(90,7,72)),(0.5380952380952382,(80,7,72)),(0.5380952380952382,(70,7,72)),(0.5377777777777778,(60,5,152)),(0.5370370370370371,(30,9,62)),(0.537037037037037,(50,9,72)),(0.5365079365079366,(40,7,42)),(0.5358024691358025,(50,9,62)),(0.5358024691358024,(40,9,152)),(0.534920634920635,(50,7,12)),(0.534567901234568,(100,9,52)),(0.534567901234568,(90,9,62)),(0.534567901234568,(60,9,162)),(0.5333333333333334,(100,1,42)),(0.5333333333333334,(90,5,132)),(0.5333333333333334,(50,5,182)),(0.5333333333333333,(100,5,102)),(0.5333333333333333,(100,5,92)),(0.5333333333333333,(80,7,22)),(0.5333333333333333,(60,9,62)),(0.5333333333333333,(30,3,82)),(0.5320987654320988,(80,9,102)),(0.5317460317460319,(80,7,112)),(0.5317460317460317,(100,7,142)),(0.5317460317460317,(70,7,22)),(0.5317460317460316,(60,7,22)),(0.5311111111111111,(90,5,172)),(0.5311111111111111,(50,5,12)),(0.5296296296296297,(30,9,42)),(0.5296296296296296,(90,9,172)),(0.5285714285714286,(30,7,72)),(0.5283950617283952,(40,9,72)),(0.5283950617283951,(70,9,62)),(0.5283950617283951,(40,9,62)),(0.528395061728395,(100,9,162)),(0.5266666666666667,(50,5,2)),(0.5266666666666667,(30,5,2)),(0.5266666666666666,(80,5,172)),(0.5259259259259259,(100,3,62)),(0.5259259259259259,(70,9,162)),(0.5259259259259259,(50,9,152)),(0.5259259259259259,(30,9,152)),(0.5253968253968254,(100,7,172)),(0.5246913580246914,(90,9,102)),(0.5244444444444444,(100,5,132)),(0.5244444444444444,(80,5,132)),(0.5244444444444444,(60,5,72)),(0.5244444444444444,(30,5,62)),(0.5222222222222223,(90,1,192)),(0.5222222222222223,(90,1,42)),(0.5222222222222223,(70,1,172)),(0.5222222222222223,(70,1,2)),(0.5222222222222223,(60,9,102)),(0.5222222222222223,(60,7,152)),(0.5222222222222223,(60,3,92)),(0.5222222222222223,(60,1,172)),(0.5222222222222223,(60,1,2)),(0.5222222222222223,(30,9,82)),(0.5222222222222221,(100,1,2)),(0.5222222222222221,(80,3,152)),(0.5222222222222221,(50,3,182)),(0.5222222222222221,(40,9,82)),(0.5222222222222221,(30,1,132)),(0.5209876543209876,(60,9,152)),(0.52,(90,5,112)),(0.52,(90,5,102)),(0.52,(70,5,172)),(0.52,(70,5,72)),(0.52,(60,5,62)),(0.52,(40,5,2)),(0.519753086419753,(40,9,42)),(0.5185185185185186,(30,3,182)),(0.5185185185185185,(80,3,142)),(0.5185185185185185,(70,3,92)),(0.5185185185185185,(60,3,152)),(0.5177777777777778,(70,5,152)),(0.5177777777777778,(50,5,112)),(0.5174603174603175,(70,7,102)),(0.5174603174603175,(40,7,102)),(0.5174603174603175,(40,7,72)),(0.5158730158730159,(100,7,102)),(0.5158730158730159,(40,7,12)),(0.5155555555555554,(40,5,32)),(0.5148148148148148,(90,3,22)),(0.5148148148148148,(40,3,152)),(0.5142857142857143,(50,7,72)),(0.5135802469135803,(70,9,152)),(0.5135802469135802,(50,9,12)),(0.5133333333333333,(70,5,62)),(0.5133333333333332,(50,5,162)),(0.5123456790123456,(50,9,82)),(0.5111111111111112,(100,9,102)),(0.5111111111111112,(100,7,112)),(0.5111111111111112,(100,5,42)),(0.5111111111111112,(70,7,112)),(0.5111111111111112,(30,7,102)),(0.5111111111111111,(90,5,62)),(0.5111111111111111,(80,3,92)),(0.5111111111111111,(50,3,22)),(0.5111111111111111,(40,3,182)),(0.5111111111111111,(40,3,22)),(0.5111111111111111,(40,1,132)),(0.5111111111111111,(30,1,122)),(0.5111111111111111,(30,1,82)),(0.5095238095238096,(30,7,12)),(0.5095238095238095,(70,7,152)),(0.508888888888889,(80,5,62)),(0.508888888888889,(40,5,102)),(0.5088888888888888,(30,5,32)),(0.5086419753086421,(60,9,192)),(0.508641975308642,(100,9,122)),(0.5079365079365079,(100,7,62)),(0.5074074074074074,(90,3,92)),(0.5074074074074074,(60,9,122)),(0.5074074074074073,(30,3,22)),(0.5066666666666667,(70,5,132)),(0.5066666666666666,(90,5,72)),(0.5066666666666666,(50,5,62)),(0.5066666666666666,(50,5,32)),(0.5063492063492064,(90,7,132)),(0.5063492063492064,(80,7,102)),(0.5061728395061729,(100,9,172)),(0.5061728395061729,(100,9,42)),(0.5049382716049382,(90,9,132)),(0.5049382716049382,(80,9,122)),(0.5047619047619049,(50,7,112)),(0.5047619047619049,(50,7,102)),(0.5047619047619047,(80,7,132)),(0.5047619047619046,(100,7,42)),(0.5044444444444445,(100,5,72)),(0.5044444444444445,(90,5,142)),(0.5044444444444445,(80,5,72)),(0.5044444444444445,(40,5,182)),(0.5037037037037037,(70,9,122)),(0.5037037037037037,(50,9,172)),(0.5024691358024692,(80,9,132)),(0.5024691358024691,(30,9,172)),(0.5022222222222222,(60,5,102)),(0.5022222222222222,(40,5,12)),(0.5015873015873016,(90,7,142)),(0.5015873015873016,(60,7,112)),(0.5012345679012347,(100,9,62)),(0.5012345679012346,(40,9,172)),(0.5012345679012346,(40,9,32)),(0.5,(90,7,102)),(0.5,(90,1,172)),(0.5,(80,1,172)),(0.5,(50,5,102)),(0.5,(50,1,122)),(0.5,(40,1,122)),(0.4999999999999999,(60,5,192)),(0.49876543209876545,(100,9,132)),(0.4984126984126984,(30,7,192)),(0.4977777777777777,(40,5,62)),(0.49753086419753095,(50,9,32)),(0.4975308641975309,(50,9,182)),(0.4968253968253967,(60,7,192)),(0.4962962962962964,(80,9,192)),(0.4962962962962963,(70,9,192)),(0.4962962962962963,(60,3,192)),(0.49523809523809526,(70,7,132)),(0.49523809523809514,(100,7,132)),(0.49506172839506174,(40,9,12)),(0.4938271604938272,(30,9,32)),(0.4938271604938271,(100,9,142)),(0.4933333333333333,(80,5,142)),(0.4933333333333333,(40,5,82)),(0.4933333333333333,(30,5,82)),(0.4933333333333333,(30,5,12)),(0.49259259259259264,(90,9,192)),(0.49259259259259264,(70,9,132)),(0.49259259259259264,(50,3,152)),(0.4925925925925925,(90,9,122)),(0.4920634920634921,(80,7,142)),(0.492063492063492,(30,7,172)),(0.4911111111111111,(40,5,112)),(0.4901234567901235,(100,9,192)),(0.48888888888888893,(60,7,102)),(0.48888888888888893,(60,3,22)),(0.48888888888888893,(50,1,172)),(0.48888888888888893,(30,9,12)),(0.4888888888888888,(60,1,162)),(0.4888888888888888,(50,7,172)),(0.4888888888888888,(50,1,132)),(0.4876543209876544,(30,9,102)),(0.48765432098765427,(50,9,112)),(0.48730158730158735,(50,7,182)),(0.48730158730158735,(40,7,32)),(0.48666666666666664,(40,5,162)),(0.4864197530864198,(40,9,122)),(0.48571428571428577,(40,7,192)),(0.48571428571428565,(50,7,162)),(0.48571428571428565,(40,7,172)),(0.48518518518518516,(100,3,122)),(0.48518518518518516,(70,3,152)),(0.48444444444444446,(30,5,182)),(0.4844444444444444,(80,5,112)),(0.4844444444444444,(30,5,192)),(0.4841269841269842,(50,7,192)),(0.4841269841269841,(80,7,192)),(0.4841269841269841,(70,7,192)),(0.4841269841269841,(40,7,82)),(0.4841269841269841,(30,7,82)),(0.482716049382716,(40,9,102)),(0.482716049382716,(30,9,192)),(0.4825396825396826,(30,7,52)),(0.4825396825396826,(30,7,32)),(0.4825396825396825,(50,7,32)),(0.4822222222222222,(100,5,172)),(0.4822222222222222,(70,5,192)),(0.4822222222222222,(30,5,162)),(0.48148148148148157,(70,3,192)),(0.48148148148148145,(80,3,22)),(0.48148148148148145,(70,3,22)),(0.48148148148148145,(50,3,162)),(0.48148148148148145,(50,3,122)),(0.4809523809523809,(100,7,122)),(0.48024691358024696,(90,9,142)),(0.4802469135802469,(30,9,122)),(0.48000000000000004,(50,5,82)),(0.4799999999999999,(30,5,102)),(0.4793650793650793,(90,7,192)),(0.4790123456790124,(50,9,192)),(0.47777777777777775,(100,3,92)),(0.47777777777777775,(60,3,72)),(0.47777777777777775,(50,1,62)),(0.47777777777777775,(40,9,142)),(0.47777777777777775,(40,5,192)),(0.47777777777777775,(40,1,172)),(0.47777777777777775,(40,1,62)),(0.47777777777777775,(30,3,122)),(0.47777777777777775,(30,1,172)),(0.47777777777777763,(70,5,112)),(0.4765432098765433,(50,9,122)),(0.4761904761904763,(50,7,52)),(0.4761904761904763,(40,7,52)),(0.47530864197530875,(40,9,182)),(0.47530864197530864,(50,9,102)),(0.4746031746031747,(100,7,192)),(0.4746031746031746,(70,7,122)),(0.4740740740740741,(70,3,72)),(0.4740740740740741,(60,9,132)),(0.4740740740740741,(30,9,142)),(0.474074074074074,(40,3,122)),(0.47333333333333333,(100,5,62)),(0.47333333333333333,(80,5,192)),(0.47333333333333333,(60,5,112)),(0.47333333333333333,(50,5,192)),(0.47333333333333333,(50,5,52)),(0.473015873015873,(60,7,122)),(0.473015873015873,(30,7,2)),(0.4728395061728396,(80,9,142)),(0.4728395061728395,(40,9,22)),(0.47160493827160505,(30,9,22)),(0.47142857142857153,(50,7,2)),(0.47111111111111115,(60,5,132)),(0.47111111111111115,(30,5,142)),(0.4703703703703704,(90,9,112)),(0.4703703703703704,(90,3,72)),(0.4703703703703704,(40,9,192)),(0.46984126984126984,(50,7,82)),(0.46913580246913583,(50,9,142)),(0.46913580246913583,(50,9,22)),(0.46888888888888886,(90,5,192)),(0.46888888888888886,(40,5,52)),(0.46888888888888886,(30,5,22)),(0.46888888888888874,(40,5,22)),(0.46825396825396826,(80,7,122)),(0.4679012345679012,(50,9,162)),(0.4666666666666667,(100,5,112)),(0.4666666666666667,(90,1,162)),(0.4666666666666667,(80,1,112)),(0.4666666666666667,(60,7,132)),(0.4666666666666667,(50,5,142)),(0.4666666666666667,(30,9,182)),(0.4666666666666667,(30,3,152)),(0.46666666666666656,(100,1,172)),(0.46666666666666656,(80,3,192)),(0.46666666666666656,(30,3,92)),(0.4650793650793651,(40,7,22)),(0.46444444444444444,(30,5,52)),(0.46419753086419757,(50,9,92)),(0.46222222222222215,(50,5,22)),(0.46190476190476193,(40,7,182)),(0.46190476190476193,(40,7,162)),(0.4619047619047618,(40,7,2)),(0.46031746031746024,(50,7,142)),(0.46031746031746024,(50,7,22)),(0.46031746031746024,(40,7,142)),(0.46031746031746024,(40,7,112)),(0.45999999999999996,(40,5,142)),(0.4592592592592592,(40,3,162)),(0.4592592592592592,(40,3,92)),(0.4571428571428571,(30,7,142)),(0.4555555555555556,(70,3,122)),(0.4555555555555555,(90,3,192)),(0.4555555555555555,(80,3,72)),(0.4555555555555555,(80,1,2)),(0.4555555555555555,(40,9,162)),(0.4555555555555555,(30,9,2)),(0.4555555555555555,(30,7,22)),(0.453968253968254,(70,7,142)),(0.453968253968254,(60,7,142)),(0.4539682539682539,(30,7,162)),(0.45333333333333337,(30,5,72)),(0.45333333333333325,(100,5,192)),(0.4530864197530864,(40,9,92)),(0.45238095238095244,(30,7,182)),(0.45238095238095233,(90,7,122)),(0.45185185185185195,(60,3,122)),(0.45185185185185184,(50,9,2)),(0.45185185185185184,(50,3,92)),(0.45185185185185184,(30,9,162)),(0.45185185185185184,(30,9,92)),(0.4506172839506173,(40,9,2)),(0.44814814814814813,(100,3,72)),(0.44814814814814813,(40,3,142)),(0.44814814814814813,(30,3,162)),(0.44814814814814813,(30,3,142)),(0.4469135802469136,(80,9,112)),(0.4469135802469136,(30,9,52)),(0.4466666666666666,(30,5,112)),(0.445679012345679,(100,9,112)),(0.4444444444444444,(80,3,122)),(0.4444444444444444,(80,1,162)),(0.4444444444444444,(50,3,142)),(0.4444444444444444,(40,5,72)),(0.44222222222222224,(50,5,72)),(0.44222222222222224,(40,5,92)),(0.4422222222222222,(30,5,92)),(0.44074074074074077,(70,9,142)),(0.44074074074074077,(50,3,12)),(0.4407407407407407,(40,9,112)),(0.44000000000000006,(50,5,92)),(0.4396825396825397,(50,7,122)),(0.4396825396825397,(40,7,122)),(0.4395061728395062,(40,9,52)),(0.4382716049382716,(50,9,52)),(0.43777777777777777,(60,5,142)),(0.4377777777777777,(70,5,142)),(0.43703703703703706,(50,3,72)),(0.4365079365079365,(50,7,92)),(0.43492063492063493,(30,7,122)),(0.4349206349206349,(30,7,92)),(0.43333333333333335,(100,1,152)),(0.43333333333333335,(90,1,2)),(0.43333333333333335,(70,9,112)),(0.43333333333333335,(70,1,162)),(0.43333333333333335,(70,1,142)),(0.43333333333333335,(60,9,142)),(0.43333333333333335,(40,7,92)),(0.43333333333333335,(30,1,142)),(0.4320987654320988,(50,9,132)),(0.4320987654320987,(30,9,112)),(0.4296296296296297,(100,3,192)),(0.4296296296296296,(30,3,172)),(0.42888888888888893,(40,5,172)),(0.4271604938271605,(60,9,112)),(0.4271604938271605,(40,9,132)),(0.42698412698412697,(30,7,112)),(0.42666666666666675,(30,5,172)),(0.42592592592592593,(90,3,122)),(0.4234567901234568,(30,9,132)),(0.4222222222222223,(60,1,142)),(0.4222222222222223,(50,5,172)),(0.4222222222222223,(50,3,172)),(0.4222222222222223,(30,1,192)),(0.4222222222222222,(100,1,162)),(0.4222222222222222,(50,1,192)),(0.4222222222222222,(40,3,172)),(0.4222222222222222,(40,1,142)),(0.4222222222222222,(40,1,72)),(0.4222222222222222,(30,3,192)),(0.42,(100,5,122)),(0.42,(70,5,2)),(0.41333333333333333,(60,5,2)),(0.41111111111111115,(70,3,142)),(0.41111111111111115,(50,1,72)),(0.41111111111111115,(40,1,192)),(0.4111111111111111,(70,3,2)),(0.4111111111111111,(50,1,142)),(0.4111111111111111,(40,3,72)),(0.4088888888888889,(80,5,2)),(0.4044444444444444,(90,5,2)),(0.40370370370370373,(60,3,142)),(0.40370370370370373,(50,3,192)),(0.4037037037037037,(40,3,192)),(0.4037037037037037,(30,3,72)),(0.4031746031746032,(70,7,2)),(0.40222222222222226,(50,5,122)),(0.4,(70,1,72)),(0.4,(60,7,2)),(0.4,(30,1,92)),(0.39999999999999997,(40,5,122)),(0.39999999999999997,(30,1,72)),(0.39629629629629637,(60,3,2)),(0.3962962962962963,(80,3,2)),(0.3962962962962963,(30,3,12)),(0.39629629629629626,(90,3,2)),(0.39629629629629626,(40,3,12)),(0.39555555555555555,(30,5,122)),(0.3933333333333333,(100,5,2)),(0.3925925925925926,(100,3,112)),(0.39206349206349217,(80,7,2)),(0.38888888888888895,(50,3,112)),(0.3888888888888889,(60,1,72)),(0.3888888888888889,(50,1,92)),(0.3888888888888889,(50,1,52)),(0.3888888888888889,(40,1,52)),(0.3844444444444444,(70,5,122)),(0.3825396825396826,(40,7,132)),(0.38253968253968257,(50,7,132)),(0.3822222222222222,(60,5,122)),(0.38148148148148153,(90,3,112)),(0.3814814814814815,(100,3,2)),(0.38,(50,5,132)),(0.37999999999999995,(80,5,122)),(0.3777777777777779,(30,7,132)),(0.3777777777777778,(70,9,2)),(0.3753086419753086,(60,9,2)),(0.37037037037037046,(80,9,2)),(0.3688888888888889,(40,5,132)),(0.3688888888888889,(30,5,132)),(0.36666666666666675,(90,7,2)),(0.3666666666666667,(60,1,152)),(0.36666666666666664,(80,1,92)),(0.36666666666666664,(40,1,92)),(0.36444444444444435,(90,5,122)),(0.35925925925925933,(50,3,132)),(0.35925925925925933,(30,3,132)),(0.3555555555555556,(40,3,132)),(0.35555555555555557,(100,1,72)),(0.35555555555555557,(90,1,152)),(0.35555555555555557,(90,1,72)),(0.35555555555555557,(80,1,152)),(0.35555555555555557,(80,1,72)),(0.35555555555555557,(70,1,152)),(0.35555555555555557,(70,1,112)),(0.35555555555555557,(60,1,112)),(0.3555555555555555,(90,1,92)),(0.3555555555555555,(40,3,112)),(0.34938271604938276,(90,9,2)),(0.34444444444444444,(70,1,92)),(0.34444444444444444,(60,1,92)),(0.34444444444444444,(30,3,112)),(0.3333333333333333,(80,3,112)),(0.3333333333333333,(50,1,162)),(0.3333333333333333,(40,1,162)),(0.3333333333333333,(30,1,162)),(0.33333333333333326,(100,7,2)),(0.3222222222222222,(70,3,112)),(0.3222222222222222,(30,1,52)),(0.31851851851851853,(60,3,112)),(0.3111111111111111,(50,1,112)),(0.30246913580246915,(100,9,2)),(0.30000000000000004,(100,1,92)),(0.30000000000000004,(50,1,152)),(0.3,(40,1,112)),(0.3,(30,1,112)),(0.2222222222222222,(40,1,152)),(0.2222222222222222,(30,1,152))]

{-
edb <- readEmbeddings
-- let f = embeddings2Forest edb -- embeddings2ForestConfigurable 21 5 12 edb -- embeddings2Forest edb -- embeddings2ForestConfigurable 60 10 150 edb

let f1 = embeddings2ForestConfigurable 21 5 12 edb
let f2 = embeddings2ForestConfigurable 60 1 32 edb
Data.RPTree.recallWith metricL2 f1 20 $ (\(_,_,embd) -> ((fromListDv embd)::DVector Double)) $ head edb
Data.RPTree.recallWith metricL2 f2 20 $ (\(_,_,embd) -> ((fromListDv embd)::DVector Double)) $ head edb

findNearest f 20 C.maxDistance $ head edb
findN f 20 C.iterationLimit $ head edb
-}

similaritemExistsP :: String -> IO Bool
similaritemExistsP = doesFileExist . fst . getSimilarLink

-- opposite of writeOutMatch: we delete the on-disk version to force a rebuild. This may be to clean up in general, or it may be because a new item got embedded & turned out to be a similar-hit, and since distance is reciprocal, we want to rebuild *both*. Deleting on-disk is an easy way to force a rebuild using the existing logic.
expireMatches :: [String] -> IO ()
expireMatches = mapM_ (removeFile . fst . getSimilarLink)

writeOutMatch :: Metadata -> Backlinks -> (String, [String]) -> IO ()
writeOutMatch md bdb (p,matches) =
  if length matches < C.minimumSuggestions then print ("GS.writeOutMatch: skipping " ++ p) >> return () else
  case M.lookup p md of
    Nothing                   -> return ()
    Just ( _,_,_,_,_,_,  "")  -> return ()
    Just ("",_,_,_,_,_,   _)  -> return ()
    Just ( _,_,_,_,_,_,abst)  -> do
          let similarLinksHtmlFragment = generateMatches md bdb False False p abst matches
          let f = take 274 $ "metadata/annotation/similar/" ++ urlEncode p ++ ".html"
          writeUpdatedFile "similar" f similarLinksHtmlFragment
          putStrLn $ "Wrote: " ++ p ++ " (" ++ f ++ ")"

generateMatches :: Metadata -> Backlinks -> Bool -> Bool -> String -> String -> [String] -> T.Text
generateMatches md bdb linkTagsP singleShot p abst matches =
         -- we don't want to provide as a 'see also' a link already in the annotation, of course, so we need to pull them out & filter by:
         let p' = T.pack p
             alreadyLinkedAbstract  = extractLinks False $ T.pack abst
             alreadyLinkedBody      = getForwardLinks bdb p'
             alreadyLinkedBacklinks = maybe [] (concatMap snd) (M.lookup p' bdb)
             alreadyLinked = [p'] ++ alreadyLinkedAbstract ++ alreadyLinkedBody ++ alreadyLinkedBacklinks
             matchesPruned = filter (\p2 -> T.pack p2 `notElem` alreadyLinked) matches

             similarItems = filter (not . null) $ map (generateItem md linkTagsP) matchesPruned
             googleScholar = case M.lookup p md of
               Nothing             -> []
               -- We require a title, to display as a link; and an abstract, to make it worth recommending (if it has no abstract, the embedding will also probably be garbage):
               Just ("",   _,_,_,  _,_, _) -> []
               Just (_,    _,_,_,  _,_,"") -> []
               Just (title,_,_,_,kvs,_, _) ->
                 let doi = kvDOI kvs
                     doiEscaped = urlEncode doi -- TEST: this is to work around /doc/psychology/cognitive-bias/sunk-cost/2001-nolet.pdf's crazy DOI '10.1890/0012-9658(2001)082[1655:SVITDB]2.0.CO;2' but might break other DOIs?
                     doiQuery = "doi:" ++ doiEscaped
                     title' = simplifiedString title -- need to strip out HTML formatting like "<em>Peep Show</em>—The Most Realistic Portrayal of Evil Ever Made"
                     titleQuery = urlEncode $ "\"" ++ title' ++ "\""
                     query
                       | null title' && not (null doi) = doiQuery
                       | null doi && not (null title) = titleQuery
                       | otherwise = doiQuery ++ "+OR+" ++ titleQuery
                     linkMetadataG  = ("",["backlink-not", "id-not", "link-live-not", "archive-not"],[("link-icon", "alphabet"), ("link-icon-type", "svg")])
                     linkMetadataGS = ("",["backlink-not", "id-not", "link-live-not", "archive-not"],[("link-icon", "google-scholar"), ("link-icon-type", "svg")])
                     linkMetadataCP = ("",["backlink-not", "id-not", "link-live-not", "archive-not"],[("link-icon", "connected-papers"), ("link-icon-type", "svg")])
                 in
                    [[Para [Span ("", ["similar-links-search"], []) (
                      [Strong [Str "Search"], Str ": ",
                        Link linkMetadataGS
                        [Str "GS"] (T.pack ("https://scholar.google.com/scholar?q=" ++ query),
                                     T.pack "Reverse citations of this paper in Google Scholar"),
                      Str "; "]
                       ++
                       (if null doi then [] else [Link linkMetadataCP
                                                 -- it would be nice to include <https://paperswithcode.com/> but their DOI lookup was broken and stateful last I checked. Someday?
                                                  [Str "CP"] (T.pack ("https://www.connectedpapers.com/api/redirect/doi/" ++ doiEscaped),
                                                              T.pack ("Connected Papers lookup for DOI ‘" ++ doiEscaped ++ "’.")),
                                                 Str "; "])
                       ++
                       [Link linkMetadataG
                         [Str "Google"] (T.pack ("https://www.google.com/search?q=" ++ titleQuery),
                                          T.pack ("Google search engine hits for ‘" ++ title' ++ "’.")),
                         Str "; ",
                         Link linkMetadataG
                         [Str "site"] (T.pack ("https://www.google.com/search?q=site:gwern.net+-site:gwern.net/metadata/" ++ urlEncode title'),
                                            T.pack ("Gwern.net site-wide search hits for ‘" ++ title' ++ "’."))
                       ]
                      )
                     ]
                   ]]

             preface = if singleShot then [] else [Para [Strong [Str "Similar Links"], Str ":"]]
             linkList = BulletList $ similarItems ++ googleScholar

             pandoc = walk typographyTransform $ Pandoc nullMeta $ preface ++ [linkList]
             html = let htmlEither = runPure $ writeHtml5String safeHtmlWriterOptions pandoc
                    in case htmlEither of
                                Left e -> error $ show e ++ ":" ++ show p ++ ":" ++ show matches ++ ":" ++ show similarItems
                                Right output -> output
             similarLinksHtmlFragment = if CL.listLength (BulletList similarItems) > 60 || length matchesPruned < 4 then html
                                        else "<div class=\"columns\">\n" `T.append` html `T.append` "\n</div>"
         in similarLinksHtmlFragment

generateItem :: Metadata -> Bool -> String -> [Block]
generateItem md linkTagsP p2 = case M.lookup p2 md of
                                  Nothing -> [] -- This shouldn't be possible. All entries in the embedding database should've had a defined annotation as a prerequisite. But file renames might cause trouble so we ignore mismatches.
                                  Just ("",_,_,_,_,_,_) -> []
                                  Just (_,_,_,_,_,_,"") -> []
                                  Just (t,_,_,_,_,tags,_) ->
                                    [Para -- NOTE: we set .backlink-not because similar-links suggestions, even curated ones, can be quite tangential & distant, so we don't want to clutter up backlinks with them.
                                      [Link ("", ["link-annotated", "backlink-not", "id-not"],
                                             -- link-tags are particularly useful when reviewing single-shot reccomendations while writing annotations
                                              if null tags || not linkTagsP then [] else [("link-tags", T.pack $ unwords tags) ]
                                            ) [RawInline (Format "html") $ T.pack t] (T.pack p2,"")]
                                    ]

-----------------------------------
-- 'sort by magic': a way to sort by loose 'topic' or 'cluster' using embeddings. Instead of being forced to create 1D lists of items by sorting solely on simple properties like date or alphabetical order, we can instead 'sort' by embedding, where we pick a (possibly arbitrary, but a good starting point might be the newest/oldest item) 'seed' item, and then look up *its* nearest-neighbor, and so on. This produces a list which essentially constructs clusters in a linearized fashion. See /design#tags

{-
md <- LinkMetadata.readLinkMetadata
let tagTest = "psychology/smell"
m <- sortTagByTopic md tagTest
m

let m' = map (map (\f -> Data.Maybe.fromJust $ M.lookup f md)) m
putStrLn $ Text.Show.Pretty.ppShow $ nubOrd $ map (map (\(t,_,_,_,_,_) -> t)) $ map LinkMetadata.sortItemDate m' -- by date
putStrLn $ Text.Show.Pretty.ppShow $ nubOrd $ map (map (\(t,_,_,_,_,_) -> t)) $ m' -- by topic
edb <- readEmbeddings
let ml = clusterIntoSublist edb m
putStrLn $ ppShow ml
let ml' = map (map (\f -> (\(t,_,_,_,_,_) -> t) $ Data.Maybe.fromJust $ M.lookup f md)) ml
putStrLn $ ppShow ml'

----

md <- LinkMetadata.readLinkMetadata
let tagTest = "psychology/smell"
let mdl = M.toList $ M.filter (\(_,_,_,_,tags,abstract) -> tagTest `elem` tags && abstract /= "") md
mls <- sortSimilarsStartingWithNewestWithTag md tagTest mdl
map fst mls
-}

sortTagByTopic :: Metadata -> String -> IO [FilePath]
sortTagByTopic md tag = do
                           edb <- readEmbeddings
                           let edbDB = M.fromList $ map (\(a,b,c,d,e) -> (a,(b,c,d,e))) edb
                           let mdl = M.filter (\(_,_,_,_,_,tags,abstract) -> tag `elem` tags && abstract /= "") md
                           let paths = M.keys mdl
                           let mdlSorted =  filter (\(f,_) -> M.member f edbDB) $ LinkMetadata.sortItemPathDate $ map (\(f,i) -> (f,(i,""))) $ M.toList mdl
                           let newest = fst $ head mdlSorted

                           sortDB <- readListSortedMagic
                           sortSimilars edb sortDB newest paths

-- what was the short name suggested for a list of URLs? quick DB letting us look up cached short names:
type ListName = M.Map [FilePath] String
readListName :: IO ListName
readListName = do let p = "metadata/listname.hs"
                  exists <- doesFileExist p
                  if not exists then return M.empty else
                            do ls <- fmap T.unpack $ TIO.readFile p
                               return $ if ls=="" then M.empty else M.fromList $ validateListName (read ls :: [([FilePath], String)])
   where validateListName :: [([FilePath], String)] -> [([FilePath], String)]
         validateListName l = let errors = filter (\(f,g) -> null g || length f < 1 || any null f) l in if null errors then l else error ("validateListName: read file failed sanity check: " ++ show errors)
writeListName :: ListName -> IO ()
writeListName = writeUpdatedFile "listname" "metadata/listname.hs" . T.pack . ppShow . map (\(fs,nick) -> (sort fs,nick)) . filter (\(_,nick) -> nick/="") . M.toList -- ensure consistent set-like lookups by sorting

-- what was the sort-by-magic list generated previously for a list of URLs? quick DB letting us look up cached magic-sorts:
type ListSortedMagicList = [(S.Set FilePath,   -- key: a set of URLs to query for a sort-by-embedding version
                              [FilePath])] -- value: the sorted-by-embedding version
type ListSortedMagic = M.Map (S.Set FilePath) [FilePath]
readListSortedMagic :: IO ListSortedMagic
readListSortedMagic = do Config.Misc.cd
                         let p = "metadata/listsortedmagic.hs"
                         exists <- doesFileExist p
                         if not exists then return M.empty else
                           do ls <- fmap T.unpack $ TIO.readFile p
                              return $ if ls=="" then M.empty else M.fromList (read ls :: ListSortedMagicList)
   where -- validateListSortedMagic :: ListSortedMagicList -> ListSortedMagicList
         -- validateListSortedMagic l = let errors = filter (\(f,g) -> let f' = S.toList f in null f' || null g  || any null f' || any null g || sort f' /= sort g) $ nubOrd l
         --                                 in if errors /= []  then error ("validateListSortedMagic: read file failed sanity check: " ++ show errors) else l
writeListSortedMagic :: ListSortedMagic -> IO ()
writeListSortedMagic x = Config.Misc.cd >> (writeUpdatedFile "listsortedmagic" "metadata/listsortedmagic.hs" $ T.pack $ ppShow $ nubOrd $ M.toList x)

-- instead of a `mapM`, we `foldM`: we need the list of 'all tags generated thus far' to pass into the blacklist option, so we don't wind up
-- with a bunch of duplicate auto-labels.
sortSimilarsStartingWithNewestWithTag :: ListName -> ListSortedMagic -> Metadata -> String -> [(FilePath, MetadataItem)] -> IO [(String, [(FilePath, MetadataItem)])]
sortSimilarsStartingWithNewestWithTag _ _ _ _ []     = return []
sortSimilarsStartingWithNewestWithTag _ _ _ _ [a]    = return [("",[a])]
sortSimilarsStartingWithNewestWithTag _ _ _ _ [a, b] = return [("",[a]), ("",[b])]
sortSimilarsStartingWithNewestWithTag ldb sortDB md parentTag items =
  do
    lists <- sortSimilarsStartingWithNewest md sortDB items
    (result, _) <- foldM (processWithBlacklistAccumulator ldb) ([], []) lists
    return result
  where
    processWithBlacklistAccumulator :: ListName ->
                                    ([(String, [(FilePath, MetadataItem)])], [String]) ->
                                    [(FilePath, MetadataItem)] ->
                                    IO ([(String, [(FilePath, MetadataItem)])], [String])
    processWithBlacklistAccumulator ldb' (acc, blacklist) fs = do
      let urlList = map fst fs
      suggestion <- case M.lookup (sort urlList) ldb' of
                      Just nickname -> return nickname -- use existing one, or generate new suggestion & cache it out:
                      Nothing -> do nicknameNew <- processTitles parentTag blacklist $ map (\(_,(t,_,_,_,_,_,_)) -> t) fs
                                    ldb'' <- readListName -- the in-memory DB may be stale due to other threads also trying to update the on-disk, so re-read
                                    let ldb''' = M.insert urlList nicknameNew ldb''
                                    writeListName ldb'''
                                    return nicknameNew
      let newAcc = mergeIntoAccumulator acc (suggestion, fs)
      return (newAcc, blacklist ++ [suggestion])
      where
        mergeIntoAccumulator :: [(String, [(FilePath, MetadataItem)])] -> (String, [(FilePath, MetadataItem)]) -> [(String, [(FilePath, MetadataItem)])]
        mergeIntoAccumulator accum (tag, fls) = -- despite the prompting, sometimes a duplicate tag-name will be returned anyway, so we try to merge those together
          case lookup tag accum of
            Just oldFls -> (tag, oldFls ++ fls) : filter ((/= tag) . fst) accum
            Nothing -> (tag, fls) : accum

processTitles :: String -> [String] -> [String] -> IO String
processTitles _ _ [] = return ""
processTitles parentTag blacklistTags a =
      do let a' = take (4096*3) $ unlines $ [parentTag, unwords blacklistTags] ++ a
         (status,_,mb) <- runShellCommand "./" Nothing "python3" ["static/build/tagguesser.py", a']
         case status of
           ExitFailure err -> printRed "tagguesser.py failed!" >> printRed (show err) >> print a' >> return "" -- printGreen (ppShow (intercalate " : " [a, a', ppShow status, ppShow err, ppShow mb])) >> printRed "tagguesser.py failed!" >> return ""
           _ -> return $ (last . lines . replace "'" "" . replace "." "" . replace "The suggested tag is " "" . replace "The best tag suggestion is: " "" . replace "Best tag: " "" . sed "[a-z0-9]\\) " "" -- NOTE: (can't quite seem to prompt away the tendency to leave in the list number like 'c) foo' or descriptions
                           . trim . U.toString) mb

sortSimilarsStartingWithNewest :: Metadata -> ListSortedMagic -> [(FilePath, MetadataItem)] -> IO [[(FilePath, MetadataItem)]]
sortSimilarsStartingWithNewest _ _ []     = return []
sortSimilarsStartingWithNewest _ _ [a]    = return [[a]]
sortSimilarsStartingWithNewest _ _ [a, b] = return [[a, b]]
sortSimilarsStartingWithNewest md sortDB items = do
  edb <- readEmbeddings
  let edbDB = M.fromList $ map (\(a,b,c,d,e) -> (a,(b,c,d,e))) edb
  let md' = M.restrictKeys md (S.fromList $ filter (`M.member` edbDB) $ map fst items)
  let mdlSorted = LinkMetadata.sortItemPathDate $ map (\(f,i) -> (f,(i,""))) $ M.toList md'
  if null mdlSorted then return [] else
    do let paths = M.keys md'
       let newest = fst $ head mdlSorted
       -- print "mdlSorted: " >> print mdlSorted >> print "newest: " >> print newest
       pathsSorted <- sortSimilars edb sortDB newest paths
       let pathsSorted' = clusterIntoSublist edb pathsSorted
       -- print "pathsSorted: " >> print pathsSorted
       return $ map (`restoreAssoc` items) pathsSorted'
  where
    restoreAssoc :: Eq a => [a] -> [(a,b)] -> [(a,b)]
    restoreAssoc keys list = map (\k -> (k, fromJust $ lookup k list)) keys

sortSimilarsT :: Embeddings -> ListSortedMagic -> T.Text -> [T.Text] -> IO [T.Text]
sortSimilarsT _ _ _ []    = return []
sortSimilarsT _ _ _ [a]   = return [a]
sortSimilarsT _ _ ""   _  = error "sortSimilarsT given an invalid seed!"
sortSimilarsT [] _ _   _  = error "sortSimilarsT given empty embeddings database!"
sortSimilarsT edb sortDB seed paths = do results <- sortSimilars edb sortDB (T.unpack seed) (map T.unpack paths)
                                         return $ map T.pack results

sortSimilars :: Embeddings -> ListSortedMagic -> FilePath -> [FilePath] -> IO [FilePath]
sortSimilars _ _ _ []    = return []
sortSimilars _ _ _ [a]   = return [a]
sortSimilars _ _ ""   _  = error "sortSimilars given an invalid seed!"
sortSimilars [] _ _   _  = error "sortSimilars given empty embeddings database!"
sortSimilars edb sortDB seed paths = do
                             let edbDB = M.fromList $ map (\(a,b,c,d,e) -> (a,(b,c,d,e))) edb
                             let edbDB' = M.restrictKeys edbDB (S.fromList (seed:paths))
                             let edb' = map (\(a,(b,c,d,e)) -> (a,b,c,d,e)) $ M.toList edbDB'
                             let paths' = filter (/= seed) paths
                             let newKey = S.fromList (seed:paths')
                             -- print "sortSimilars: begin"
                             -- putStrLn $ ppShow $ map (\(a,b,c,d,_) -> (a,b,take 100 c,d)) edb'
                             -- putStrLn $ "paths': " ++ ppShow paths'
                             -- print ("Seed: " ++ seed)
                             -- print "sortSimilars: done"
                             -- print ("edb' length: "  ++ show (length edb'))
                             case M.lookup newKey sortDB of
                               Nothing -> do paths'' <- lookupNextAndShrink paths' edb' seed
                                             -- print ("seed: " ++ show seed)
                                             -- print ("paths: " ++ show paths)
                                             -- print ("paths': " ++ show paths')
                                             -- print ("paths'': " ++ show paths'')
                                             sortDB' <- readListSortedMagic
                                             let sortDB'' = M.insert newKey paths'' sortDB'
                                             -- print ("writing new sortDB'': " ++ show sortDB'')
                                             writeListSortedMagic sortDB''
                                             -- print ("wrote new sortDB"::String)
                                             return paths''
                               Just paths''' -> return paths'''

-- in some lists, like of backlinks, there is no guarantee of an embedding. So we only sort the embedded ones, put them first, and append the leftover un-embedded links
sortListPossiblyUnembedded :: Embeddings -> ListSortedMagic -> (T.Text, [T.Text]) -> IO (T.Text, [T.Text])
sortListPossiblyUnembedded edb sortDB x@(url, hits) =
    let hits' = map T.unpack hits
        urlsEmbedded = map (\(u,_,_,_,_) -> u) edb :: [String]
        hitsEmbedded = hits' \\ urlsEmbedded
        hitsEmbeddedNot = hitsEmbedded \\ hits'
    in if length hitsEmbedded < 4 then return x else
      do sorted <- sortSimilarsT edb sortDB url (map T.pack hitsEmbedded)
         return (url, sorted ++ map T.pack hitsEmbeddedNot)

lookupNextAndShrink :: [FilePath]
              -> [(FilePath, Integer, String, String, [Double])]
              -> FilePath
              -> IO [FilePath]
lookupNextAndShrink     []   _ _ = return []
lookupNextAndShrink     [a]  _ previous = return $ previous:[a]
lookupNextAndShrink targets embeddings previous = do results <- go targets embeddings
                                                     return $ previous:results
  where go ta em = do ddb <- embeddings2Forest em
                      -- putStrLn ("remainingTargets: " ++ ppShow ta) >> putStrLn ("remainingEmbeddings: " ++ ppShow (map (\(a,b,c,d,_) -> (a,b,take 100 c,d)) em)) >> putStrLn ("previous: " ++ previous)
                      case M.lookup previous (M.fromList $ map (\(a,b,c,d,e) -> (a,(b,c,d,e))) em) of
                              Nothing ->  error $ "Exited at Nothing in lookupNextAndShrink, this should never happen? " ++ previous ++ " : " ++ show ta
                              Just (b,c,d,e) -> do -- putStrLn $ "findNearest: " ++ show (findNearest ddb 6 C.maxDistance (previous,b,c,d,e))
                                                   let matchs = filter (/=previous) $ findNearest ddb 6 C.maxDistance (previous,b,c,d,e) :: [FilePath]
                                                   if null matchs then
                                                          let fallback = head targets
                                                              targetsNewNull = filter (\f -> f/=previous && f/=fallback) ta in
                                                            lookupNextAndShrink targetsNewNull
                                                                                (Prelude.filter (\(f,_,_,_,_) -> f/= previous) embeddings)
                                                                                fallback
                                                   else
                                                    let match' = head matchs
                                                        targetsNewNotNull = filter (\f -> f/=previous && f/=match') ta in
                                                      lookupNextAndShrink targetsNewNotNull (Prelude.filter (\(f,_,_,_,_) -> f/= previous) embeddings) match'

---------------------

pairwiseDistance :: [Double] -> [Double] -> Double
pairwiseDistance [] _ = error "Empty list passed to pairwiseDistance"
pairwiseDistance _ [] = error "Empty list passed to pairwiseDistance"
pairwiseDistance a b = fromListDv a `metricL2` fromListDv b

pairwiseDistanceEmbedding :: Embedding -> Embedding -> Double
pairwiseDistanceEmbedding a b = last5 a `pairwiseDistance` last5 b

pairwiseDistances :: [(FilePath, Embedding)] -> [(Double, FilePath)]
pairwiseDistances xs = concatMap processPair $ zip xs (tail $ tails xs)
  where
    processPair ((fp1, (_, _, _, _, emb1)), (_, (_, _, _, _, emb2)):_)
      = [(pairwiseDistance emb1 emb2, fp1)]
    processPair _ = []

splitAtIndices :: [Int] -> [a] -> [[a]]
splitAtIndices [] xs = [xs]
splitAtIndices (i:is) xs = firstPart : splitAtIndices (map (subtract i) is) remaining
  where
    (firstPart, remaining) = splitAt i xs

-- merge empty or 1-item lists into the following list, to avoid what looks like spurious clusters generated by the heuristic.
mergeSingletons :: [[a]] -> [[a]]
mergeSingletons []   = []
mergeSingletons [[]] = []
mergeSingletons [x]  = [x]
mergeSingletons (x:y:xs)
  | length x < 2 = mergeSingletons ((x ++ y) : xs)
  | otherwise     = x : mergeSingletons (y : xs)

clusterIntoSublist :: Embeddings -> [FilePath] -> [[FilePath]]
clusterIntoSublist [] x    = error $ "clusterIntoSubList: passed empty embedding database for arguments " ++ show x
clusterIntoSublist _ []    = [[]]
clusterIntoSublist _ [a]   = [[a]]
clusterIntoSublist es list = let k = 1 `max` (round(sqrt(fromIntegral $ length list :: Double)) - 1) in
                                 if k == 1 then [list] else
                                  let edb = M.fromList $ map (\(a,b,c,d,e) -> (a,(a,b,c,d,e))) es
                                      list' = map (\f -> (f, fromMaybe ("",0,"","",[]) $ M.lookup f edb)) list
                                      listDistanceDescending = (sort $ zip (pairwiseDistances list') [0::Int ..]) :: [((Double,FilePath), Int)]
                                      distancesLargestIndices = map snd $ take k listDistanceDescending
                                    in mergeSingletons $ splitAtIndices distancesLargestIndices list

{-
Experiment: can we sort full.gtx by embedding for better browsing/serendipity?
result: sorta. Doing global greedy distance, which works well on short lists, seems to yield unfortunately random behavior when run globally across 7144 annotations in full.gtx. You need to nest within-tag, and then do a few iterations of bubble-sort to clean it up & reduce issues like date-inversions.

mdl <- GTX.readGTXSlow "metadata/full.gtx"
let seed = head $ map fst mdl
let paths = map fst mdl
edb <- readEmbeddings
sortDB <- readListSortedMagic -- cache of past sort-by-embedding

let filterByTag tags (_, (_, _, _, _, _, entryTags, _)) = not . null $ intersect tags entryTags
tags <- Tags.listTagsAll -- Get all tags
sortedEntries <- mapM (\tag -> let filteredEntries = filter (filterByTag [tag]) mdl in let paths = map fst filteredEntries in sortSimilars edb sortDB (fst $ head filteredEntries) paths) tags

let sortMetadataList sortedEntries mdl = Data.Maybe.mapMaybe (\e -> fmap (\m -> (e, m)) (lookup e mdl)) sortedEntries
partialSort = sortOn (\(_, (_, _, date, _, _, tags, title)) -> (head tags, date, title))
let mdl' = reverse $ Data.Containers.ListUtils.nubOrd $ partialSort $ partialSort $ partialSort $ partialSort $ partialSort $ partialSort $ partialSort $ partialSort $ partialSort $ sortMetadataList (concat sortedEntries) mdl
GTX.writeGTX "metadata/full.gtx" mdl
GTX.writeGTX "metadata/full-sorted.gtx" (sort mdl')
-}
