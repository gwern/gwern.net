#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Pandoc (def, pandocExtensions, queryWith, readerExtensions, readHtml, Inline(Link), runPure, Pandoc)
import Text.Pandoc.Walk (walk)
import qualified Data.Text as T  (append, drop, head, init, intercalate, last, length, pack, replace, take, unlines, unpack, Text)
import Data.List ((\\), intercalate, sort, sortOn)
import Data.List.Utils (replace)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M (filter, keys, lookup)
import System.Directory (doesFileExist, renameFile)
import Data.Containers.ListUtils (nubOrd)
import System.IO.Temp (emptySystemTempFile)
import Text.Read (readMaybe)
import qualified Control.Monad.Parallel as Par (mapM) -- mapM_
import Control.Parallel.Strategies (parMap, rseq)
import Text.Show.Pretty (ppShow)
import Data.FileStore.Utils (runShellCommand)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import System.Exit (ExitCode(ExitFailure))
import Data.Binary (decodeFile, encodeFile)

import LinkMetadata (readLinkMetadata, authorsTruncate, Metadata, MetadataItem)

import Columns (simplifiedDoc)

import Interwiki (convertInterwikiLinks, inlinesToString)

import qualified Data.Vector as V (toList, Vector)
import Control.Monad.Identity (runIdentity, Identity)
import Data.RPTree -- (knn, forest, inner, rpTreeCfg, fpMaxTreeDepth, fpDataChunkSize, fpProjNzDensity, fromListDv, DVector, Embed(..), RPForest)
import Data.Conduit (ConduitT)
import Data.Conduit.List (sourceList)
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, fromList, toList)

main :: IO ()
main = do md  <- readLinkMetadata
          edb <- readEmbeddings

          -- update for any missing embeddings, and return updated DB for computing distances & writing out fragments:
          let todo = sort $ missingEmbeddings md edb
          edb'' <- do if (length todo) == 0 then putStrLn "done" >> return edb else do
                       newEmbeddings <- Par.mapM embed todo
                       let edb' = nubOrd (edb ++ newEmbeddings)
                       writeEmbeddings edb'
                       return edb'

          let distances = embeddingDistances edb''
          putStrLn $ ppShow distances
          -- let targets   = M.keys distances
          -- let matches   = (map (\target -> (target, map fst $ take topNEmbeddings $
          --                                           fromJust $ M.lookup target distances) )
          --                   targets) :: [(String, [String])]
          -- writeOutMatches matches
          return ()

-- how many results do we want?
bestNEmbeddings :: Int
bestNEmbeddings = 10

embeddingsPath :: String
embeddingsPath = "metadata/embeddings.bin"

type Embedding  = (String, String, [Double]) -- NOTE: 'Float' in Haskell is 32-bit single-precision float (FP32); OA API apparently returns 64-bit double-precision (FP64), so we use 'Double' instead
type Embeddings = [Embedding]

readEmbeddings :: IO Embeddings
readEmbeddings = do exists <- doesFileExist embeddingsPath
                    if exists then decodeFile embeddingsPath else return []

writeEmbeddings :: Embeddings -> IO ()
writeEmbeddings es = do tempf <- emptySystemTempFile "hakyll-embeddings"
                        encodeFile tempf es
                        renameFile tempf embeddingsPath

missingEmbeddings :: Metadata -> Embeddings -> [(String, MetadataItem)]
missingEmbeddings md edb = let urlsToCheck = M.keys $ M.filter (\(t, aut, _, _, tags, abst) -> length (t++aut++show tags++abst) > minimumLength) md
                               urlsEmbedded = map (\(u,_,_) -> u) edb :: [String]
                               missing      = urlsToCheck \\ urlsEmbedded
                               in map (\u -> (u, fromJust $ M.lookup u md)) missing

  where minimumLength :: Int
        minimumLength = 400 -- how many characters long should metadata be before it is worth trying to embed?

-- convert an annotated item into a single text string: concatenate the useful metadata
formatDoc :: (String,MetadataItem) -> T.Text
formatDoc (path,mi@(t,aut,dt,_,tags,abst)) =
    let document = T.pack $ replace "\n" "\n\n" $ unlines ["'"++t++"' " ++ "("++path++")" ++ ", by "++authorsTruncate aut++" ("++dt++").", "Keywords: "++(intercalate ", " tags) ++ ".", "Abstract: "++abst]
        parsedEither = let parsed = runPure $ readHtml def{readerExtensions = pandocExtensions } document
                       in case parsed of
                          Left e -> error $ "Failed to parse HTML document into Pandoc AST: error: " ++ show e ++ " : " ++ show mi ++ " : " ++ T.unpack document
                          Right p -> p
        -- create a numbered list of URL references inside each document to expose it to the embedding model, as 'simplifiedDoc' necessarily strips URLs:
        documentURLs = extractURLs parsedEither
        documentURLsText = if null documentURLs then "" else "References:\n" `T.append` T.unlines (map (\(n, (url,titles)) -> T.pack n `T.append` ". " `T.append` url `T.append` " " `T.append` (T.intercalate ", " $ tail titles)) $ zip (map show [(1::Int)..]) documentURLs)
        -- simple plaintext ASCII-ish version, which hopefully is more comprehensible to NN models than full-blown HTML
        plainText = simplifiedDoc parsedEither `T.append` documentURLsText
        -- post-processing: 'We suggest replacing newlines (\n) in your input with a single space, as we have observed inferior results when newlines are present.' https://beta.openai.com/docs/api-reference/embeddings/create
        -- GPT-3 apparently doesn't do well with Unicode punctuation either (they get a bad BPE expansion factor too), so smart quotes are right out.
        gptPlainText = T.take maxLength $ T.replace "  " " " $ T.replace "  " " " $ T.replace "\n" "" $ T.replace "…" "..." $ T.replace "“" "'" $ T.replace "”" "'" $ T.replace "‘" "''" $ T.replace "’" "'" $ T.replace "\\" "" $ T.replace "\"" "'" $ T.replace "\"" "'" plainText
    in
      gptPlainText
  where
    maxLength :: Int
    maxLength = 8100 -- how long is too long? OA guesstimates 1 BPE = 4 characters on average (https://beta.openai.com/tokenizer), so 2047 BPEs ~ 8192 characters. If a call fails, the shell script will truncate the input and retry until it works so we don't need to set the upper limit too low.

    -- | Read 1 Pandoc AST and return its URLs/anchor-text pairs;
    -- if a URL has both a title and an anchor text, we return 2 pairs because both might be valid (eg '[GPT-3](https://arxiv.org/foo "Language Models are Few-Shot Learners")' - we would like to do link-suggestions on both the short noun 'GPT-3' and the paper title, but we can't if we arbitrarily return one but not the other).
    extractURLs :: Pandoc -> [(T.Text,[T.Text])]
    extractURLs = queryWith extractURL . walk convertInterwikiLinks
     where
       extractURL :: Inline -> [(T.Text,[T.Text])]
       extractURL (Link _ il (u,""))     = [(u, [cleanURL $ inlinesToString il])]
       extractURL (Link _ il (u,target)) = [(u, [cleanURL $ inlinesToString il]), (u, [target])]
       extractURL _ = []

       -- NOTE: apparently due to nested Spans (from the smallcaps) and the RawInline issue (yet again), some link suggestions look like ">ADHD<". Very undesirable replacement targets. So we special-case clean those:
       cleanURL :: T.Text -> T.Text
       cleanURL "" = ""
       cleanURL u = if T.head u == '>' && T.last u == '<' then T.init $ T.drop 1 u else u

embed :: (String,MetadataItem) -> IO Embedding
embed i@(p,_) = do
  let doc = formatDoc i
  (modelType,embedding) <- oaAPIEmbed doc
  return (p,modelType,embedding)

-- we shell out to a Bash script `similar.sh` to do the actual curl + JSON processing; see it for details.
oaAPIEmbed :: T.Text -> IO (String,[Double])
oaAPIEmbed doc = do (status,_,mb) <- runShellCommand "./" Nothing "bash" ["static/build/embed.sh", T.unpack doc]
                    case status of
                      ExitFailure err -> error $ "Exit Failure: " ++ (intercalate " : " [show (T.length doc), T.unpack doc, ppShow status, ppShow err, ppShow mb])
                      _ -> do let results = lines $ U.toString mb
                              case results of
                                [] -> error $ "Failed to read embed.sh output? " ++ "\n" ++ show (T.length doc) ++ "\n" ++ T.unpack doc ++ "\n" ++ U.toString mb
                                (modelType:latents) -> let embeddingM = readMaybe (unlines latents) :: Maybe [Double] in
                                                         case embeddingM of
                                                           Nothing -> error $ "Failed to read embed.sh output? " ++ "\n" ++ show (T.length doc) ++ "\n" ++ T.unpack doc ++ "\n" ++ U.toString mb
                                                           Just embedding -> return (modelType, embedding)

uniquePairs :: (Ord a) => [a] -> [(a,a)]
uniquePairs l = [(x,y) | x <- l, y <- l, x < y]

-- copied from https://ardoris.wordpress.com/2014/08/14/cosine-similarity-pearson-correlation-inner-products/
cosineSimilarity :: [Double] -> [Double] -> Double
cosineSimilarity = similarity dot
 where similarity ip xs ys = (ip xs ys) / ( (len xs) * (len ys) )
         where len xs' = sqrt (ip xs' xs')
       -- the inner products
       dot xs ys = sum $ zipWith (*) xs ys

-- type Distances = M.Map String [(String, Double)]
type Distances = [(String, [String])]

-- loop over pairwise distances; maintain a list of top-n candidates, and throw away matches which don't pass the cut. This cuts down enormously on storage needs, at the cost of less parallelism, more recomputation, and much obscurer code.
embeddingDistances :: Embeddings -> Distances
embeddingDistances es = parMap rseq (\e1@(p1,_,_) -> (p1, map fst $ foldr (distanceAndRank e1) [] es)) es

distanceAndRank :: Embedding -> Embedding -> [(String, Double)] -> [(String, Double)]
distanceAndRank (p1,md1,embed1) (p2,md2,embed2) accum = if p1==p2 || md1/=md2 then accum else
                                                          let distance = cosineSimilarity embed1 embed2 in
                                                            take bestNEmbeddings $ sortOn snd $ [(p2,distance)] ++ accum

writeOutMatches :: [(String, [String])] -> IO ()
writeOutMatches = Prelude.mapM_ writeOutMatch -- Par.mapM_ writeOutMatch

writeOutMatch :: (String, [String]) -> IO ()
writeOutMatch = putStrLn . show

type Forest = RPForest Double (V.Vector (Embed DVector Double String))

embeddings2Forest :: Embeddings -> Forest
embeddings2Forest es =
  let minLeafSize = 50 -- ???
      cfg = rpTreeCfg minLeafSize
              (length es) -- data N
              (length $ (\(_,_,embedding) -> embedding) $ head es) -- dimension of each datapoint (eg 512 for ada-similarity embeddings)
      nTrees = 10 -- ???
      projectionVectorDimension = 25 -- ???
  in
    runIdentity $
    forest 0 (fpMaxTreeDepth cfg) minLeafSize nTrees (fpDataChunkSize cfg) (fpProjNzDensity cfg) projectionVectorDimension $
    embeddings2Conduit es
 where
   embeddings2Conduit :: Embeddings -> ConduitT () (Embed DVector Double String) Identity ()
   embeddings2Conduit = sourceList . map (\(p,_,embedding) -> Embed (fromListDv embedding) p)

knnEmbedding :: Forest -> Int -> Embedding -> [(Double, Embed DVector Double String)]
knnEmbedding f k (_,_,embd) = V.toList $
                               knn (inner) k f ((fromListDv embd)::DVector Double)

-- normalize :: (VU.Unbox a, Inner v v, Double) => v a -> v a
normalize v = v /. metricL2 v v
  where -- (/.) :: (Scale v, VU.Unbox a, Fractional a) => v a -> a -> v a
        v /. a = (1 / a) .* v


findNearest :: Forest -> Int -> Embedding -> [(String,Double)]
findNearest f k e = map (\(dist,Embed _ p) -> (p,dist)) $ knnEmbedding f k e

{-
edb <- readEmbeddings
let f = embeddings2Forest edb
recallWith inner f 10 $ (\(_,_,embd) -> ((fromListDv embd)::DVector Double)) $ head edb

findNearest f 10 $ head edb

[(-0.27969392959494144,DV [7.46145797893405e-3,2.939111553132534e-2,-1.887066848576069e-2,4.9191880971193314e-2,1.3558771461248398e-2,1.1243859305977821e-2,1.9346050918102264e-2,-5.4720803163945675e-3,4.724901169538498e-2,2.542269602417946e-2,-6.5778642892837524e-3,-2.2095009684562683e-2,-3.4909702837467194e-2,-3.64391952753067e-2,-2.1536950021982193e-2,-2.2570393979549408e-2,-8.028559386730194e-4,3.7565650418400764e-3,2.0927218720316887e-2,3.331819921731949e-2,-6.148985121399164e-3,-2.6745501905679703e-2,2.5487286038696766e-3,1.9191034138202667e-2,3.5178396850824356e-2,3.2284755259752274e-2,3.4351643174886703e-2,3.001118265092373e-2,2.692118752747774e-3,-1.1946599930524826e-2,-1.9470063969492912e-2,-1.887066848576069e-2,1.2938705272972584e-2,3.296682983636856e-2,-2.294243313372135e-2,-1.3724122196435928e-2,-5.2374888211488724e-2,-1.6669433563947678e-2,-7.621641270816326e-3,-1.4840240590274334e-2,-3.439297899603844e-2,8.303713984787464e-3,1.6473080962896347e-2,2.8461018577218056e-2,-1.3331413269042969e-2,1.3868804089725018e-2,-5.630197003483772e-2,-2.1970996633172035e-2,-4.113102797418833e-3,2.3087114095687866e-2,-2.4327246472239494e-2,2.4843968451023102e-2,1.0086403228342533e-2,9.884881787002087e-3,-1.9325383007526398e-2,-4.799309000372887e-2,1.8364280462265015e-2,-2.149561233818531e-2,-3.063124790787697e-2,-1.3961813412606716e-2,-2.217768505215645e-2,-4.063497483730316e-2,-7.404618430882692e-3,-2.1206248551607132e-2,-4.9563921988010406e-2,9.746658615767956e-4,1.989377662539482e-2,8.61891359090805e-3,-3.538508713245392e-2,3.939484432339668e-2,-1.2690679170191288e-2,1.2690679170191288e-2,-1.20396101847291e-2,-2.1929658949375153e-2,2.0699862390756607e-2,-1.4713644050061703e-3,-1.0070901364088058e-2,-1.2235963717103004e-2,2.6683496311306953e-2,2.6228781789541245e-2,7.151424884796143e-3,-4.4412207789719105e-3,3.577779233455658e-2,-1.1130180209875107e-2,3.470301255583763e-2,2.1185580641031265e-2,-2.0751534029841423e-2,-2.922576479613781e-2,-1.2504659593105316e-2,-1.114051416516304e-2,3.981338813900948e-3,-7.451123557984829e-3,-1.530528999865055e-2,-4.862349014729261e-3,-1.3920475728809834e-2,-1.7749381950125098e-3,1.0432606562972069e-2,-9.574848227202892e-3,1.2597668915987015e-2,4.722834099084139e-3,3.2243418972939253e-3,1.4447531662881374e-2,-3.267746418714523e-2,-1.505726296454668e-2,-5.492749158293009e-3,-1.8663978204131126e-2,-2.7778945863246918e-2,-3.4289635717868805e-2,-3.0941281467676163e-2,-7.51829706132412e-3,6.231660954654217e-3,4.4810086488723755e-2,5.777591723017395e-4,-3.738996386528015e-2,7.796293497085571e-2,-5.089706275612116e-3,8.004015311598778e-3,1.503659412264824e-2,-4.2908549308776855e-2,8.007115870714188e-2,-1.733083836734295e-2,-3.8216717541217804e-2,0.12971775233745575,8.169366046786308e-3,2.8791720047593117e-2,-4.3249586597085e-3,-2.6786839589476585e-2,3.3442214131355286e-2,3.85887585580349e-2,-1.3052384369075298e-2,-2.451326511800289e-2,1.489191222935915e-2,-8.691255003213882e-3,-4.180276766419411e-3,1.1667570099234581e-2,-5.216819792985916e-2,-4.348727688193321e-2,1.1667570099234581e-2,3.734862804412842e-2,-2.486463636159897e-2,1.956307515501976e-2,9.01162251830101e-3,3.8485415279865265e-2,-1.3310744427144527e-2,1.8002575263381004e-2,-3.139082808047533e-3,-1.785789430141449e-2,-1.3362416997551918e-2,2.430657669901848e-2,-3.172669559717178e-2,-8.996120654046535e-3,9.89521574229002e-3,1.2029275298118591e-2,4.646359384059906e-2,-7.802493870258331e-3,6.862061098217964e-3,-1.6049368306994438e-2,-1.1274862103164196e-2,1.779588684439659e-2,-1.1243859305977821e-2,-3.5498763900250196e-3,-2.6993528008461e-2,-2.4451259523630142e-2,1.2907701544463634e-2,-2.4058550596237183e-2,-1.8873250810429454e-3,-3.335953876376152e-2,1.6731441020965576e-2,-2.715888060629368e-2,-3.352488949894905e-2,-1.1936265975236893e-2,1.5460305847227573e-2,-1.3031715527176857e-2,2.217768505215645e-2,-2.929810667410493e-3,-1.4571544714272022e-2,-1.986793940886855e-3,7.6113068498671055e-3,-4.253651201725006e-2,1.2225629761815071e-2,2.653881348669529e-2,-2.805797616019845e-3,-1.0339596308767796e-2,-5.642598494887352e-3,7.563510444015265e-4,-8.810101076960564e-3,1.6638431698083878e-2,2.20536720007658e-2,-1.9552739337086678e-2,5.4669128730893135e-3,-2.6972859632223845e-3,-3.424829989671707e-2,2.0493172109127045e-2,5.92162786051631e-3,2.1743638440966606e-2,-2.6580151170492172e-2,4.001491144299507e-2,-1.645241118967533e-2,-2.6662826538085938e-2,1.0127740679308772e-3,7.62680871412158e-3,1.6369735822081566e-2,1.57083326485008e-3,1.7702877521514893e-2,1.7506523057818413e-2,1.5749670565128326e-2,-1.2163623236119747e-2,-7.347778882831335e-3,-6.071477197110653e-3,2.6228781789541245e-2,3.919332288205624e-3,-5.187883600592613e-2,1.7806220799684525e-2,-1.799224130809307e-2,-1.4127165079116821e-2,6.769051309674978e-3,-8.572408929467201e-3,-5.423508584499359e-2,3.9043474942445755e-2,3.873344138264656e-2,-3.621184080839157e-2,-3.222275152802467e-2,-4.6546269208192825e-2,-3.2098736613988876e-2,1.161331427283585e-3,-6.262663751840591e-2,-2.827499806880951e-2,-1.0995832271873951e-2,7.789576193317771e-4,2.412055805325508e-2,2.7758276090025902e-2,9.466337040066719e-3,-3.0259208753705025e-2,-1.1398875154554844e-2,-2.1123573184013367e-2,-3.5317912697792053e-3,1.631806418299675e-2,3.9436180144548416e-2,4.0945008397102356e-2,3.565378114581108e-2,-1.296325004659593e-3,2.5009319186210632e-2,-2.1908989176154137e-2,4.121370241045952e-2,-8.593077771365643e-3,4.479329218156636e-4,2.1226918324828148e-2,-1.4550875872373581e-2,5.066453944891691e-3,1.2308305129408836e-2,1.710348017513752e-2,-3.654254227876663e-2,2.546403370797634e-2,-2.294243313372135e-2,3.521973267197609e-2,-3.268263302743435e-3,-2.955646812915802e-2,-1.4902247115969658e-2,3.521973267197609e-2,2.8812387958168983e-2,-1.3083387166261673e-2,1.1812252923846245e-2,-8.391556330025196e-3,1.7762300558388233e-3,-7.203096989542246e-3,2.500931965187192e-3,-8.510402403771877e-3,2.133026160299778e-2,-1.4468200504779816e-2,1.858130283653736e-2,3.953952714800835e-2,-1.8560634925961494e-2,-3.369024023413658e-2,-2.2570393979549408e-2,4.0945008397102356e-2,-8.174533024430275e-3,3.321485593914986e-2,3.025404177606106e-3,-1.9769763574004173e-2,2.4699285626411438e-2,7.1875955909490585e-3,3.089994378387928e-2,-1.0799478739500046e-2,2.2567808628082275e-3,2.738623647019267e-3,1.2814692221581936e-2,5.267975386232138e-3,-3.1375326216220856e-2,2.1973580587655306e-3,2.949446067214012e-2,-1.3465761207044125e-2,-1.5798758249729872e-3,1.3062718324363232e-2,-0.3077179491519928,6.464185193181038e-3,-7.745654787868261e-3,2.837834320962429e-2,-1.0163910686969757e-2,9.598101023584604e-4,-2.0420832559466362e-2,-2.358833560720086e-3,1.4612882398068905e-2,-1.0592790320515633e-2,-1.595635898411274e-2,1.0095445904880762e-3,2.641480043530464e-2,-8.024684153497219e-3,4.071765020489693e-2,5.849286913871765e-3,-2.8585031628608704e-2,-2.5443363934755325e-2,-2.00901310890913e-2,-2.3417815566062927e-2,2.451326511800289e-2,2.8688374906778336e-2,7.507962640374899e-3,-3.083793632686138e-2,-3.189205005764961e-2,-2.221902273595333e-2,2.670416422188282e-2,3.168535977602005e-2,-1.4519873075187206e-2,3.147866949439049e-2,5.6343305855989456e-2,-5.962965544313192e-3,2.3603836074471474e-2,4.2619187384843826e-2,-2.084454335272312e-2,...6.345339585095644e-3,-3.2450105994939804e-2,2.2467048838734627e-2,5.241622403264046e-2,1.4674888923764229e-2,-4.555416479706764e-2,2.3025108501315117e-2,4.534230567514896e-3,8.4659643471241e-2,3.6976587027311325e-2,-1.189492829144001e-2] "/docs/economics/1969-brooks-businessadventures-ch3-thefederalincometax.pdf")]
it :: V.Vector (Double, Embed DVector Double String)
-}
