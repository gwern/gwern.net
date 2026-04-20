{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- dependencies: pandoc, filestore, vector
-- exact-scan k-nearest neighbors lookup for GenerateSimilar's embedding search/seriation core.

module GenerateSimilar where

import Text.Pandoc (Block(BulletList, Para), Format(..), Inline(Link, RawInline, Span, Str, Strong), Pandoc(..), def, nullMeta, pandocExtensions, readerExtensions, readHtml, runPure, writeHtml5String)
import Text.Pandoc.Walk (walk)

import qualified Data.Text as T (Text, append, intercalate, isPrefixOf, length, pack, strip, take, unlines, unpack)
import qualified Data.Text.IO as TIO (readFile)

import Control.Monad (foldM)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Binary as DB (decodeFileOrFail, encodeFile)
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import Data.List ((\\), intercalate, sort, sortBy)
import qualified Data.Map.Strict as M (difference, elems, empty, filter, insert, keys, lookup, fromList, member, notMember, restrictKeys, toList, withoutKeys, Map)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S (empty, insert, fromList, member, notMember, size, Set)
import qualified Data.Vector as V (empty, foldl', fromList, imap, length, null, toList, (!), Vector)
import qualified Data.Vector.Unboxed as VU (ifoldl', fromList, length, map, null, unsafeIndex, Vector)
import Network.HTTP (urlEncode)
import System.Directory (doesFileExist, removeFile, renameFile)
import System.Exit (ExitCode(ExitFailure))
import System.FilePath (takeBaseName)
import System.IO.Temp (emptySystemTempFile)
import Text.Read (readMaybe)
import Text.Show.Pretty (ppShow)

import Data.FileStore.Utils (runShellCommand)

import LinkBacklink (Backlinks, getForwardLinks, getSimilarLink)
import qualified Columns as CL (listLength)
import LinkMetadata (readLinkMetadata)
import qualified LinkMetadata
import LinkMetadataTypes (Metadata, MetadataItem)
import Metadata.Author (authorsTruncateString)
import Query (extractLinks, extractURLsAndAnchorTooltips)
import Typography (typographyTransformTemporary)
import Utils (anyPrefixT, deleteMany, kvDOI, printRed, replace, safeHtmlWriterOptions, sed, simplifiedDoc, simplifiedString, trim, writeUpdatedFile, printGreen)

import qualified Config.GenerateSimilar as C (bestNEmbeddings, blackList, embeddingsPath, maxDistance, maxTitlesForTagGuessing, maximumLength, minimumSuggestions)
import qualified Config.Misc as CM (cd, todayDay)

--------------------------------------------------------------------------------
-- Public embedding storage type

-- | Serialized embedding record.
type Embedding =
  ( String   -- URL/path
  , Integer  -- ModifiedJulianDay
  , String   -- text as embedded
  , String   -- embedding model/version
  , [Double] -- embedding vector
  )

type Embeddings = [Embedding]

-- | Drop bulky/debug metadata, but keep the model ID: cross-model distances are
-- meaningless, and preserving the model lets the exact-scan code enforce that.
stripEmbedding :: Embedding -> Embedding
stripEmbedding (url, _, _, model, vec) = (url, 0, "", model, vec)

last5 :: (a, b, c, d, e) -> e
last5 (_, _, _, _, e) = e

last4 :: (a, b, c, d) -> d
last4 (_, _, _, d) = d

--------------------------------------------------------------------------------
-- Reading/writing the serialized embedding DB

readEmbeddings :: IO Embeddings
readEmbeddings = CM.cd >> readEmbeddingsPath C.embeddingsPath

readEmbeddingsPath :: FilePath -> IO Embeddings
readEmbeddingsPath p = do
  exists <- doesFileExist p
  if not exists
    then return []
    else do
      eE <- DB.decodeFileOrFail p
      case eE of
        Right e  -> return e
        Left err -> error $ show err

writeEmbeddings :: Embeddings -> IO ()
writeEmbeddings es = do
  tempf <- emptySystemTempFile "hakyll-embeddings"
  DB.encodeFile tempf es
  es' <- readEmbeddingsPath tempf
  if length es' /= length es
    then error "Embeddings corrupted! Not writing out."
    else renameFile tempf C.embeddingsPath

-- | Remove embeddings without a corresponding metadata entry.
pruneEmbeddings :: Metadata -> Embeddings -> Embeddings
pruneEmbeddings md edb =
  let edbDB = M.fromList $ map (\(a, b, c, d, e) -> (a, (b, c, d, e))) edb
      invalidEmbeddings = S.fromList $ M.keys $ M.difference edbDB md
      validEdbDB = M.withoutKeys edbDB invalidEmbeddings
  in map (\(a, (b, c, d, e)) -> (a, b, c, d, e)) $ M.toList validEdbDB

missingEmbeddings :: Metadata -> Embeddings -> [(String, MetadataItem)]
missingEmbeddings md edb =
  let urlsToCheck = M.keys $ M.filter (\(_, _, _, _, _, _, abst) -> abst /= "") md
      urlsEmbedded = map (\(u, _, _, _, _) -> u) edb :: [String]
      missing = urlsToCheck \\ urlsEmbedded
  in map (\u -> (u, fromJust $ M.lookup u md)) missing

--------------------------------------------------------------------------------
-- Embedding text generation and OpenAI API shell-out

-- | Convert an annotated item into a single text string.
formatDoc :: (String, MetadataItem) -> T.Text
formatDoc (path, mi@(t, aut, dt, dtM, _, tags, abst)) =
  let dateModified = if dtM == "" then "" else "; updated " ++ dtM
      document = T.pack $ replace "\n" "\n\n" $ unlines
        [ (if t == "" then "" else "'" ++ t ++ "'" ++
             if path == "" || head path == '/'
               then " (https://gwern.net" ++ path ++ ")"
               else " (" ++ path ++ ")") ++
          (if aut == "" || aut == "N/A" then "" else ", by " ++ authorsTruncateString aut) ++
          (if dt == "" then "." else " (" ++ take 4 dt ++ dateModified ++ ").")
        , if null tags then "" else "Keywords: " ++ intercalate ", " tags ++ "."
        , deleteMany ["\n[]\n", "<hr>"] abst
        ]
      parsedEither =
        let parsed = runPure $ readHtml def{readerExtensions = pandocExtensions} document
        in case parsed of
             Left e -> error $ "Failed to parse HTML document into Pandoc AST: error: " ++ show e ++
                               " : " ++ show mi ++ " : " ++ T.unpack document
             Right p -> p
      documentURLs = filter
        (\(u, _) -> not (T.pack path `T.isPrefixOf` u) && anyPrefixT u ["/", "http"])
        $ extractURLsAndAnchorTooltips parsedEither
      documentURLsText =
        if null documentURLs
          then ""
          else "\nReferences:\n\n" `T.append` T.unlines
            (map
              (\(n, (url, titles)) ->
                 T.pack n `T.append` ". " `T.append` url `T.append` " " `T.append` T.intercalate ", " titles)
              $ zip (map show [(1 :: Int)..]) documentURLs)
      plainText = simplifiedDoc parsedEither `T.append` documentURLsText
      gptPlainText = T.take C.maximumLength $ T.strip plainText
  in gptPlainText

embed :: Embeddings -> Metadata -> Backlinks -> (String, MetadataItem) -> IO Embedding
embed edb mdb bdb i@(p, _) =
  if not (null olds)
    then let (_, b, c, d, e) = head olds in return (p, b, c, d, e)
    else do
      let backlinks = case M.lookup (T.pack p) bdb of
                        Nothing -> []
                        Just bl -> map T.unpack (concatMap snd bl)
          backlinksMetadata =
            if null backlinks
              then ""
              else "\n\nReverse citations:\n\n- " ++ intercalate "\n- "
                (nubOrd $ map
                  (\b -> case M.lookup b mdb of
                           Nothing -> ""
                           Just (t, a, d, _, _, _, _) ->
                             "\"" ++ t ++ "\", " ++ authorsTruncateString a ++
                             (if d == "" then "" else " (" ++ take 4 d ++ ")"))
                  backlinks)
          doc = formatDoc i `T.append` T.pack backlinksMetadata
      (modelType, embedding) <- oaAPIEmbed p doc
      today <- CM.todayDay
      return (p, today, T.unpack doc, modelType, embedding)
 where
  new = takeBaseName p
  olds = filter
    (\(pold, _, _, _, _) ->
       if head pold == '/'
         then new == takeBaseName pold
         else dehttp new == dehttp pold)
    edb
  dehttp = deleteMany ["http://", "https://"]

-- | Shell out to static/build/embed.sh for curl + JSON processing.
oaAPIEmbed :: FilePath -> T.Text -> IO (String, [Double])
oaAPIEmbed p doc = do
  let args =
        [ "static/build/embed.sh"
        , replace "\"" "\\\"" $ replace "\n" "\\n" $ T.unpack doc
        ]
  (status, stderr, mb) <- runShellCommand "./" Nothing "bash" args
  case status of
    ExitFailure err ->
      error $ "Exit Failure: " ++ intercalate " ::: "
        [ show (T.length doc)
        , T.unpack doc
        , ppShow status
        , ppShow err
        , ppShow mb
        , show stderr
        ]
    _ -> do
      let results = lines $ U.toString mb
      case results of
        [] -> error $ "Failed to read any embed.sh output at all for the path: " ++ p ++
                      "\n" ++ show (T.length doc) ++ "\n" ++ T.unpack doc ++
                      "\n" ++ U.toString mb ++ "\n" ++ show stderr
        (modelType:latents) ->
          case readMaybe (unlines latents) :: Maybe [Double] of
            Nothing -> error $ "Failed to read embed.sh's generated embeddings? " ++
                               "\n" ++ show (T.length doc) ++ "\n" ++ T.unpack doc ++
                               "\n" ++ U.toString mb ++ "\n" ++ show stderr
            Just embedding -> return (modelType, embedding)

--------------------------------------------------------------------------------
-- Exact vector index and distance primitive

data EmbeddingRow = EmbeddingRow
  { erPath  :: !FilePath
  , erAge   :: !Integer
  , erModel :: !String
  , erVec   :: !(VU.Vector Double) -- normalized
  } deriving (Eq, Show)

data EmbeddingIndex = EmbeddingIndex
  { eiRows   :: !(V.Vector EmbeddingRow)
  , eiByPath :: !(M.Map FilePath Int)
  } deriving (Eq, Show)

-- Compatibility name: old callers expect a Forest, but this is now an exact
-- vector index, not an RP-tree.
type Forest = EmbeddingIndex

embeddings2Forest :: Embeddings -> IO Forest
embeddings2Forest []  = error "GenerateSimilar.embeddings2Forest: called with no arguments."
embeddings2Forest [_] = error "GenerateSimilar.embeddings2Forest: called with only 1 argument."
embeddings2Forest es  = return $ embeddings2Index es

embeddings2Index :: Embeddings -> EmbeddingIndex
embeddings2Index es =
  let deduped = M.elems $ M.fromList [(p, e) | e@(p, _, _, _, vec) <- es, p /= "", not (null vec)]
      rows = V.fromList $ map embeddingToRow deduped
      byPath = M.fromList $ zip (map erPath $ V.toList rows) [0..]
  in if V.null rows
       then error "GenerateSimilar.embeddings2Index: no non-empty embeddings."
       else EmbeddingIndex rows byPath

embeddingToRow :: Embedding -> EmbeddingRow
embeddingToRow (p, age, _, model, xs) =
  let v = VU.fromList xs
  in EmbeddingRow p age model (normalizeVector p v)

indexSize :: EmbeddingIndex -> Int
indexSize = V.length . eiRows

rowByPath :: EmbeddingIndex -> FilePath -> Maybe EmbeddingRow
rowByPath ix p = do
  i <- M.lookup p (eiByPath ix)
  let rows = eiRows ix
  if i < 0 || i >= V.length rows then Nothing else Just (rows V.! i)

rowDistance :: EmbeddingRow -> EmbeddingRow -> Maybe Double
rowDistance a b
  | erModel a /= erModel b = Nothing
  | VU.length (erVec a) /= VU.length (erVec b) =
      error $ "GenerateSimilar.rowDistance: dimension mismatch for " ++ erPath a ++
              " and " ++ erPath b ++ ": " ++ show (VU.length $ erVec a) ++
              " /= " ++ show (VU.length $ erVec b)
  | otherwise =
      let !d = cosineDistance (erVec a) (erVec b)
      in Just d

lookupPathK :: EmbeddingIndex -> Int -> FilePath -> [(FilePath, Double)]
lookupPathK ix k p
  | k <= 0 = []
  | otherwise =
      case rowByPath ix p of
        Nothing -> []
        Just q  -> nearestRowKWith (not . C.blackList) ix k q

embeddingQueryVector :: Embedding -> (FilePath, String, VU.Vector Double)
embeddingQueryVector (p, _, _, model, xs) = (p, model, normalizeVector p $ VU.fromList xs)

normalizeVector :: FilePath -> VU.Vector Double -> VU.Vector Double
normalizeVector p v
  | VU.null v = error $ "GenerateSimilar.normalizeVector: empty vector for " ++ show p
  | n2 <= 0   = error $ "GenerateSimilar.normalizeVector: zero vector for " ++ show p
  | otherwise = let !n = sqrt n2 in VU.map (/ n) v
 where
  !n2 = dotRaw v v

dotRaw :: VU.Vector Double -> VU.Vector Double -> Double
dotRaw a b
  | VU.length a /= VU.length b =
      error $ "GenerateSimilar.dotRaw: dimension mismatch: " ++ show (VU.length a) ++
              " /= " ++ show (VU.length b)
  | otherwise = VU.ifoldl' step 0 a
 where
  step !acc !i !x = acc + x * VU.unsafeIndex b i

cosineDistance :: VU.Vector Double -> VU.Vector Double -> Double
cosineDistance a b = 1 - clamp (-1) 1 (dotRaw a b)
 where
  clamp lo hi x = max lo (min hi x)

--------------------------------------------------------------------------------
-- Global top-k cache and exact lookup

-- | Map from query path to sorted nearest neighbors with distances.
-- Values are ascending by distance: smaller is better.
newtype Distances = Distances
  { unDistances :: M.Map FilePath (V.Vector (FilePath, Double))
  } deriving (Eq, Show)

-- | Build the default global top-k cache from serialized embeddings.
distances :: Embeddings -> Distances
distances = distancesK C.bestNEmbeddings

-- | Build a global top-k cache from serialized embeddings.
distancesK :: Int -> Embeddings -> Distances
distancesK k = distancesIndexK k . embeddings2Index

-- | Build a global top-k cache from an in-memory index.
distancesIndexK :: Int -> EmbeddingIndex -> Distances
distancesIndexK k ix
  | k <= 0 = Distances M.empty
  | otherwise = Distances $ M.fromList $ map oneRow [0 .. indexSize ix - 1]
 where
  rows = eiRows ix
  oneRow i =
    let q = rows V.! i
        ns = V.fromList $ nearestRowKWith (not . C.blackList) ix k q
    in (erPath q, ns)

lookupK :: Distances -> Int -> String -> [(String, Double)]
lookupK (Distances m) k p
  | k <= 0 = []
  | otherwise = maybe [] (take k . V.toList) $ M.lookup p m

-- | Exact k-NN lookup for a query embedding, which need not already be in the index.
lookupEmbeddingK :: EmbeddingIndex -> Int -> Embedding -> [(FilePath, Double)]
lookupEmbeddingK ix k e
  | k <= 0 = []
  | otherwise = topKToList $ V.foldl' step emptyTopK (eiRows ix)
 where
  (qPath, qModel, qVec) = embeddingQueryVector e

  step !acc r =
    case scoreRow r of
      Nothing  -> acc
      Just hit -> insertTopK k hit acc

  scoreRow r
    | erPath r == "" = Nothing
    | erPath r == qPath = Nothing
    | C.blackList (erPath r) = Nothing
    | erModel r /= qModel = Nothing
    | VU.length (erVec r) /= VU.length qVec =
        error $ "GenerateSimilar.lookupEmbeddingK: dimension mismatch for query " ++ show qPath ++
                " and candidate " ++ show (erPath r)
    | otherwise =
        let !d = cosineDistance qVec (erVec r)
        in Just (erPath r, d)


nearestRowKWith :: (FilePath -> Bool) -> EmbeddingIndex -> Int -> EmbeddingRow -> [(FilePath, Double)]
nearestRowKWith pathOK ix k q
  | k <= 0 = []
  | otherwise = topKToList $ V.foldl' step emptyTopK (eiRows ix)
 where
  step !acc r =
    case scoreRow r of
      Nothing  -> acc
      Just hit -> insertTopK k hit acc

  scoreRow r
    | erPath r == erPath q = Nothing
    | not (pathOK $ erPath r) = Nothing
    | otherwise =
        case rowDistance q r of
          Nothing -> Nothing
          Just d  -> Just (erPath r, d)

data TopK = TopK
  { tkSize  :: !Int
  , tkHits  :: ![(FilePath, Double)]              -- ascending, best first
  , tkWorst :: !(Maybe (FilePath, Double))        -- last item in tkHits
  } deriving (Eq, Show)

emptyTopK :: TopK
emptyTopK = TopK 0 [] Nothing

topKToList :: TopK -> [(FilePath, Double)]
topKToList (TopK _ xs _) = xs

insertTopK :: Int -> (FilePath, Double) -> TopK -> TopK
insertTopK !k !x tk@(TopK !len !xs !worst)
  | k <= 0 = tk
  | len >= k
  , Just w <- worst
  , compareNeighbor x w /= LT = tk
  | otherwise =
      let xsInserted = insertSortedNeighbor x xs
          xsKept     = take k xsInserted
          !len'      = min k (len + 1)
          !worst'    = lastMaybe xsKept
      in TopK len' xsKept worst'

insertSortedNeighbor :: (FilePath, Double) -> [(FilePath, Double)] -> [(FilePath, Double)]
insertSortedNeighbor !x [] = [x]
insertSortedNeighbor !x ys@(y:ys')
  | compareNeighbor x y /= GT = x : ys
  | otherwise                 = y : insertSortedNeighbor x ys'

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just $ last xs

sortNeighbors :: [(FilePath, Double)] -> [(FilePath, Double)]
sortNeighbors = sortBy compareNeighbor

compareNeighbor :: Ord a => (a, Double) -> (a, Double) -> Ordering
compareNeighbor (p1, d1) (p2, d2) = compare d1 d2 <> compare p1 p2

-- Compatibility wrapper. The old iteration limit is obsolete under exact scan.
findN :: Forest -> Int -> Int -> Maybe Double -> Embedding -> (String, [String])
findN _  0 _    _ e = error $ "findN called for k=0; embedding target: " ++ show e
findN _  _ 0    _ e = error $ "findN called with iteration limit 0; embedding target: " ++ show e
findN ix k _iter Nothing e = findN ix k 1 (Just C.maxDistance) e
findN ix k _iter (Just mx) e@(p1, _, _, _, _) =
  let hits = map fst $ filter ((< mx) . snd) $ lookupEmbeddingK ix k e
  in (p1, take C.bestNEmbeddings $ nubOrd hits)

findNearest :: Forest -> Int -> Double -> Embedding -> [String]
findNearest ix k maxDist e = map fst $ filter ((< maxDist) . snd) $ lookupEmbeddingK ix k e

--------------------------------------------------------------------------------
-- Complete local distance cache for greedy seriation

data LocalDistances = LocalDistances
  { ldPaths  :: !(V.Vector FilePath)
  , ldByPath :: !(M.Map FilePath Int)
  , ldRows   :: !(V.Vector (V.Vector (Int, Double)))
  } deriving (Eq, Show)

-- | Build a complete pairwise distance cache for the embedded subset of the
-- supplied paths. All included embeddings must have the same model ID.
distancesLocal :: EmbeddingIndex -> [FilePath] -> LocalDistances
distancesLocal ix paths =
  let uniquePaths = nubOrd paths
      indexed = mapMaybe (\p -> (p,) <$> M.lookup p (eiByPath ix)) uniquePaths
      rows0 = map (\(p, i) -> (p, eiRows ix V.! i)) indexed
  in case rows0 of
       [] -> LocalDistances V.empty M.empty V.empty
       _  ->
         let models = nubOrd $ map (erModel . snd) rows0
             modelCheck = if length models > 1
                   then error $ "GenerateSimilar.distancesLocal: mixed embedding models in local seriation set: " ++ show models
                   else ()
             pathsV = V.fromList $ map fst rows0
             rowsV = V.fromList $ map snd rows0
             byPath = M.fromList $ zip (V.toList pathsV) [0..]
             localRows = V.imap (oneRow rowsV) rowsV
         in modelCheck `seq` LocalDistances pathsV byPath localRows
 where
  oneRow rowsV i q =
    V.fromList $ sortBy compareNeighbor
      [ (j, d)
      | j <- [0 .. V.length rowsV - 1]
      , j /= i
      , Just d <- [rowDistance q (rowsV V.! j)]
      ]

lookupLocalK :: LocalDistances -> Int -> FilePath -> [(FilePath, Double)]
lookupLocalK ld k p
  | k <= 0 = []
  | otherwise =
      case M.lookup p (ldByPath ld) of
        Nothing -> []
        Just i  -> map resolve $ take k $ V.toList $ ldRows ld V.! i
 where
  resolve (j, d) = (ldPaths ld V.! j, d)

-- | Greedy nearest-neighbor seriation. The result starts with the seed, then
-- visits the closest unvisited local neighbor until all embedded candidates are
-- exhausted; unembedded candidates are appended in their input order.
seriateGreedy :: EmbeddingIndex -> [FilePath] -> FilePath -> [FilePath]
seriateGreedy ix paths seed
  | seed == "" = error "GenerateSimilar.seriateGreedy: empty seed."
  | M.notMember seed (eiByPath ix) =
      error $ "GenerateSimilar.seriateGreedy: seed has no embedding: " ++ seed
  | otherwise = embeddedOrder ++ unembedded
 where
  requested = nubOrd (seed : filter (/= seed) paths)
  embeddedSet = S.fromList $ M.keys $ eiByPath ix
  unembedded = filter (`S.notMember` embeddedSet) requested
  ld = distancesLocal ix requested
  embeddedOrder = go S.empty seed

  go !seen current
    | S.size seen >= V.length (ldPaths ld) = []
    | otherwise =
        let seen' = S.insert current seen
            next = case filter (\(p, _) -> p `S.notMember` seen') $ lookupLocalK ld maxBound current of
                     []       -> Nothing
                     ((p,_):_) -> Just p
        in current : maybe [] (go seen') next

--------------------------------------------------------------------------------
-- Similar-link generation

singleShotMaxDistance :: Double
singleShotMaxDistance = 1

singleShotRecommendations :: String -> IO T.Text
singleShotRecommendations html = do
  let emptyMetadata  = M.empty :: Metadata
      emptyBacklinks = M.empty :: Backlinks

  -- This is an anonymous query, not a corpus item.
  -- It does not need Metadata or Backlinks context.
  newEmbedding <- embed [] emptyMetadata emptyBacklinks ("", ("", "", "", "", [], [], html))

  edb <- readEmbeddings

  let !ix =
        embeddings2Index $
        map stripEmbedding edb

      hits =
        map fst $
        filter ((< singleShotMaxDistance) . snd) $
        lookupEmbeddingK ix C.bestNEmbeddings newEmbedding

      hitsSorted =
        case hits of
          []    -> []
          h : _ -> seriateGreedy ix hits h

  if null hitsSorted
    then return ""
    else do
      md <- readLinkMetadata
      return (generateMatches md emptyBacklinks True "" html hitsSorted)

similaritemExistsP :: String -> IO Bool
similaritemExistsP = doesFileExist . fst . getSimilarLink

expireMatches :: [String] -> IO ()
expireMatches = mapM_ (removeFile . fst . getSimilarLink)

writeOutMatch :: Metadata -> Backlinks -> (String, [String]) -> IO ()
writeOutMatch md bdb (p, matches) =
  if length matches < C.minimumSuggestions
    then printGreen ("GS.writeOutMatch: skipping " ++ p)
    else case M.lookup p md of
      Nothing                  -> return ()
      Just (_, _, _, _, _, _, "") -> return ()
      Just ("", _, _, _, _, _, _) -> return ()
      Just (_, _, _, _, _, _, abst) -> do
        let similarLinksHtmlFragment = generateMatches md bdb False p abst matches
            f = take 274 $ "metadata/annotation/similar/" ++ urlEncode p ++ ".html"
        writeUpdatedFile "similar" f similarLinksHtmlFragment
        putStrLn $ "Wrote: " ++ p ++ " (" ++ f ++ ")"

generateMatches :: Metadata -> Backlinks -> Bool -> String -> String -> [String] -> T.Text
generateMatches _  _   _          _ _    []      = ""
generateMatches md bdb singleShot p abst matches =
  let p' = T.pack p
      alreadyLinkedAbstract  = extractLinks False $ T.pack abst
      alreadyLinkedBody      = if p == "" then [] else getForwardLinks bdb p'
      alreadyLinkedBacklinks = maybe [] (concatMap snd) (M.lookup p' bdb)
      alreadyLinked = [p'] ++ alreadyLinkedAbstract ++ alreadyLinkedBody ++ alreadyLinkedBacklinks
      matchesPruned = filter (\p2 -> T.pack p2 `notElem` alreadyLinked) matches

      similarItems = filter (not . null) $ map (generateItem md) matchesPruned
      googleScholar = case M.lookup p md of
        Nothing -> []
        Just ("", _, _, _, _, _, _) -> []
        Just (_, _, _, _, _, _, "") -> []
        Just (title, _, _, _, kvs, _, _) ->
          let doi = kvDOI kvs
              doiEscaped = urlEncode doi
              doiQuery = "doi:" ++ doiEscaped
              title' = simplifiedString title
              titleQuery = urlEncode $ "\"" ++ title' ++ "\""
              query
                | null title' && not (null doi) = doiQuery
                | null doi && not (null title) = titleQuery
                | otherwise = doiQuery ++ "+OR+" ++ titleQuery
              linkMetadataG  = ("", ["backlink-not", "id-not", "link-live-not", "archive-not"], [("link-icon", "alphabet"), ("link-icon-type", "svg")])
              linkMetadataGS = ("", ["backlink-not", "id-not", "link-live-not", "archive-not"], [("link-icon", "google-scholar"), ("link-icon-type", "svg")])
              linkMetadataCP = ("", ["backlink-not", "id-not", "link-live-not", "archive-not"], [("link-icon", "connected-papers"), ("link-icon-type", "svg")])
          in [[Para [Span ("", ["similar-links-search"], [])
                ([ Strong [Str "Search"], Str ": "
                 , Link linkMetadataGS [Str "GS"]
                     (T.pack ("https://scholar.google.com/scholar?q=" ++ query),
                      T.pack "Reverse citations of this paper in Google Scholar")
                 , Str "; "
                 ] ++
                 (if null doi then [] else
                   [ Link linkMetadataCP [Str "CP"]
                       (T.pack ("https://www.connectedpapers.com/api/redirect/doi/" ++ doiEscaped),
                        T.pack ("Connected Papers lookup for DOI ‘" ++ doiEscaped ++ "’."))
                   , Str "; "
                   ]) ++
                 [ Link linkMetadataG [Str "Google"]
                     (T.pack ("https://www.google.com/search?q=" ++ titleQuery),
                      T.pack ("Google search engine hits for ‘" ++ title' ++ "’."))
                 , Str "; "
                 , Link linkMetadataG [Str "site"]
                     (T.pack ("https://www.google.com/search?q=site:gwern.net+-site:gwern.net/metadata/" ++ urlEncode title'),
                      T.pack ("Gwern.net site-wide search hits for ‘" ++ title' ++ "’."))
                 ])]]]

      preface = if singleShot then [] else [Para [Link ("", ["icon-special"], []) [Strong [Str "Similar Links:"]] ("/design#similar-links", "")]]
      linkList = BulletList $ similarItems ++ googleScholar
      pandoc = (if singleShot then id else walk typographyTransformTemporary) $ Pandoc nullMeta $ preface ++ [linkList]
      html = case runPure $ writeHtml5String safeHtmlWriterOptions pandoc of
               Left e -> error $ show e ++ ":" ++ show p ++ ":" ++ show matches ++ ":" ++ show similarItems
               Right output -> output
      similarLinksHtmlFragment =
        if CL.listLength (BulletList similarItems) > 60 || length matchesPruned < 4
          then html
          else "<div class=\"columns\">\n" `T.append` html `T.append` "\n</div>"
  in similarLinksHtmlFragment

generateItem :: Metadata -> String -> [Block]
generateItem md p2 = case M.lookup p2 md of
  Nothing -> []
  Just ("", _, _, _, _, _, _) -> []
  Just (_, _, _, _, _, _, "") -> []
  Just (t, _, _, _, _, _, _) ->
    [Para
      [Link ("", ["link-annotated", "id-not", "backlink-not"], [])
        [RawInline (Format "html") $ T.pack t] (T.pack p2, "")]
    ]

--------------------------------------------------------------------------------
-- Sort-by-magic lists and tags

type ListName = M.Map [FilePath] String

readListName :: IO ListName
readListName = do
  let p = "metadata/listname.hs"
  exists <- doesFileExist p
  if not exists
    then return M.empty
    else do
      ls <- fmap T.unpack $ TIO.readFile p
      return $ if ls == "" then M.empty else M.fromList $ validateListName (read ls :: [([FilePath], String)])
 where
  validateListName :: [([FilePath], String)] -> [([FilePath], String)]
  validateListName l =
    let errors = filter (\(f, g) -> null g || null f || any null f) l
    in if null errors then l else error ("validateListName: read file failed sanity check: " ++ show errors)

writeListName :: ListName -> IO ()
writeListName = writeUpdatedFile "listname" "metadata/listname.hs" . T.pack . ppShow .
  map (\(fs, nick) -> (sort fs, nick)) . filter (\(_, nick) -> nick /= "") . M.toList

type ListSortedMagicList = [(S.Set FilePath, [FilePath])]
type ListSortedMagic = M.Map (S.Set FilePath) [FilePath]

readListSortedMagic :: IO ListSortedMagic
readListSortedMagic = do
  CM.cd
  let p = "metadata/listsortedmagic.hs"
  exists <- doesFileExist p
  if not exists
    then return M.empty
    else do
      ls <- fmap T.unpack $ TIO.readFile p
      return $ if ls == "" then M.empty else M.fromList (read ls :: ListSortedMagicList)

writeListSortedMagic :: ListSortedMagic -> IO ()
writeListSortedMagic x = CM.cd >>
  writeUpdatedFile "listsortedmagic" "metadata/listsortedmagic.hs" (T.pack $ ppShow $ nubOrd $ M.toList x)

sortTagByTopic :: Metadata -> String -> IO [FilePath]
sortTagByTopic md tag = do
  edb <- readEmbeddings
  let edbDB = M.fromList $ map ((\ (a, b, c, d, e) -> (a, (b, c, d, e))) . stripEmbedding) edb
      mdl = M.filter (\(_, _, _, _, _, tags, abstract) -> tag `elem` tags && abstract /= "") md
      paths = M.keys mdl
      mdlSorted = filter (\(f, _) -> M.member f edbDB) $
        LinkMetadata.sortItemPathDate $ map (\(f, i) -> (f, (i, ""))) $ M.toList mdl
      newest = fst $ head mdlSorted
  sortDB <- readListSortedMagic
  sortSimilars edb sortDB newest paths

sortSimilarsStartingWithNewestWithTag
  :: ListName
  -> ListSortedMagic
  -> Metadata
  -> String
  -> [(FilePath, MetadataItem)]
  -> IO [(String, [(FilePath, MetadataItem)])]
sortSimilarsStartingWithNewestWithTag ldb sortDB md parentTag items = do
  edb <- readEmbeddings
  sortSimilarsStartingWithNewestWithTagEdb edb ldb sortDB md parentTag items


sortSimilarsStartingWithNewestWithTagEdb
  :: Embeddings
  -> ListName
  -> ListSortedMagic
  -> Metadata
  -> String
  -> [(FilePath, MetadataItem)]
  -> IO [(String, [(FilePath, MetadataItem)])]
sortSimilarsStartingWithNewestWithTagEdb _   _   _      _  _         []     = return []
sortSimilarsStartingWithNewestWithTagEdb _   _   _      _  _         [a]    = return [("", [a])]
sortSimilarsStartingWithNewestWithTagEdb _   _   _      _  _         [a, b] = return [("", [a]), ("", [b])]
sortSimilarsStartingWithNewestWithTagEdb edb ldb sortDB md parentTag items = do
  lists <- sortSimilarsStartingWithNewestEdb edb md sortDB items
  (result, _) <- foldM (processWithBlacklistAccumulator ldb) ([], []) lists
  return result
 where
  processWithBlacklistAccumulator
    :: ListName
    -> ([(String, [(FilePath, MetadataItem)])], [String])
    -> [(FilePath, MetadataItem)]
    -> IO ([(String, [(FilePath, MetadataItem)])], [String])
  processWithBlacklistAccumulator ldb' (acc, blacklist) fs = do
    let urlList = map fst fs
    suggestion <- case M.lookup (sort urlList) ldb' of
      Just nickname -> return nickname
      Nothing -> do
        nicknameNew <- processTitles parentTag blacklist $
          map (\(_, (t, _, _, _, _, _, _)) -> t) fs
        ldb'' <- readListName
        let ldb''' = M.insert (sort urlList) nicknameNew ldb''
        writeListName ldb'''
        return nicknameNew

    let newAcc = mergeIntoAccumulator acc (suggestion, fs)
    return (newAcc, blacklist ++ [suggestion])

  mergeIntoAccumulator
    :: [(String, [(FilePath, MetadataItem)])]
    -> (String, [(FilePath, MetadataItem)])
    -> [(String, [(FilePath, MetadataItem)])]
  mergeIntoAccumulator accum (tag, fls) =
    case lookup tag accum of
      Just oldFls -> (tag, oldFls ++ fls) : filter ((/= tag) . fst) accum
      Nothing     -> (tag, fls) : accum

sortSimilarsStartingWithNewest :: Metadata -> ListSortedMagic -> [(FilePath, MetadataItem)] -> IO [[(FilePath, MetadataItem)]]
sortSimilarsStartingWithNewest md sortDB items = do
  edb <- readEmbeddings
  sortSimilarsStartingWithNewestEdb edb md sortDB items

sortSimilarsStartingWithNewestEdb
  :: Embeddings
  -> Metadata
  -> ListSortedMagic
  -> [(FilePath, MetadataItem)]
  -> IO [[(FilePath, MetadataItem)]]
sortSimilarsStartingWithNewestEdb _   _  _      []     = return []
sortSimilarsStartingWithNewestEdb _   _  _      [a]    = return [[a]]
sortSimilarsStartingWithNewestEdb _   _  _      [a, b] = return [[a, b]]
sortSimilarsStartingWithNewestEdb edb md sortDB items = do
  let edbDB = M.fromList $
        map ((\ (a, b, c, d, e) -> (a, (b, c, d, e))) . stripEmbedding) edb

      md' = M.restrictKeys md $
        S.fromList $
        filter (`M.member` edbDB) $
        map fst items

      mdlSorted =
        LinkMetadata.sortItemPathDate $
        map (\(f, i) -> (f, (i, ""))) $
        M.toList md'

  if null mdlSorted
    then return []
    else do
      let paths = M.keys md'
          newest = fst $ head mdlSorted
      pathsSorted <- sortSimilars edb sortDB newest paths
      let pathsSorted' = clusterIntoSublist edb pathsSorted
      return $ map (`restoreAssoc` items) pathsSorted'
 where
  restoreAssoc :: Eq a => [a] -> [(a, b)] -> [(a, b)]
  restoreAssoc keys list = map (\k -> (k, fromJust $ lookup k list)) keys

processTitles :: String -> [String] -> [String] -> IO String
processTitles _ _ [] = return ""
processTitles parentTag blacklistTags a = do
  let a' = take (4096 * 3) $ unlines $ [parentTag, unwords blacklistTags] ++ take C.maxTitlesForTagGuessing a
  (status, _, mb) <- runShellCommand "./" Nothing "python3" ["static/build/tagguesser.py", a']
  case status of
    ExitFailure err -> printRed "tagguesser.py failed!" >> printRed (show err) >> print a' >> return ""
    _ -> return $ (last . lines . deleteMany ["'", ".", "The suggested tag is ", "The best tag suggestion is: ", "Best tag: "] .
                   sed "[a-z0-9]\\) " "" . trim . U.toString) mb

sortSimilarsT :: Embeddings -> ListSortedMagic -> T.Text -> [T.Text] -> IO [T.Text]
sortSimilarsT _ _ _ []    = return []
sortSimilarsT _ _ _ [a]   = return [a]
sortSimilarsT _ _ "" _    = error "sortSimilarsT given an invalid seed!"
sortSimilarsT [] _ _ _    = error "sortSimilarsT given empty embeddings database!"
sortSimilarsT edb sortDB seed paths = do
  results <- sortSimilars edb sortDB (T.unpack seed) (map T.unpack paths)
  return $ map T.pack results

sortSimilars :: Embeddings -> ListSortedMagic -> FilePath -> [FilePath] -> IO [FilePath]
sortSimilars _ _ _ []    = return []
sortSimilars _ _ _ [a]   = return [a]
sortSimilars _ _ "" _    = error "sortSimilars given an invalid seed!"
sortSimilars [] _ _ _    = error "sortSimilars given empty embeddings database!"
sortSimilars edb sortDB seed paths = do
  let paths' = filter (/= seed) paths
      newKey = S.fromList (seed : paths')
  case M.lookup newKey sortDB of
    Just cached -> return cached
    Nothing -> do
      let edbDB = M.fromList $ map (\e@(a, _, _, _, _) -> (a, e)) edb
          edbLocal = map snd $ M.toList $ M.restrictKeys edbDB newKey
          ix = embeddings2Index edbLocal
          paths'' = seriateGreedy ix paths' seed
      sortDB' <- readListSortedMagic
      let sortDB'' = M.insert newKey paths'' sortDB'
      writeListSortedMagic sortDB''
      return paths''

-- | Some lists, like backlinks, may contain URLs without embeddings. Sort only
-- embedded hits; append unembedded hits in their input order. The seed/target is
-- an anchor and is not returned unless it was already one of the hits.
sortListPossiblyUnembedded :: Embeddings -> ListSortedMagic -> (T.Text, [T.Text]) -> IO (T.Text, [T.Text])
sortListPossiblyUnembedded [] _ list = error $ "GS.sortListPossiblyUnembedded: passed an empty embedding database; tuple of URLs: " ++ show list
sortListPossiblyUnembedded _ _ x@(_, [_])   = return x
sortListPossiblyUnembedded _ _ x@(_, [_,_]) = return x
sortListPossiblyUnembedded _ _ list@("", _) = error $ "GS.sortListPossiblyUnembedded: passed an empty target URL; tuple of URLs: " ++ show list
sortListPossiblyUnembedded _ _ list@(_, []) = error $ "GS.sortListPossiblyUnembedded: passed an empty relevant-URL list; tuple of URLs: " ++ show list
sortListPossiblyUnembedded edb sortDB x@(url, hits) =
  let seed = T.unpack url
      hits' = map T.unpack hits
      urlsEmbedded = S.fromList $ map (\(u, _, _, _, _) -> u) edb
      hitsEmbedded = filter (`S.member` urlsEmbedded) hits'
      hitsUnembedded = filter (`S.notMember` urlsEmbedded) hits'
  in if length hitsEmbedded < 4 || seed `S.notMember` urlsEmbedded
       then return x
       else do
         sortedWithSeed <- sortSimilars edb sortDB seed hitsEmbedded
         let sortedHits = filter (/= seed) sortedWithSeed
         return (url, map T.pack sortedHits ++ map T.pack hitsUnembedded)

--------------------------------------------------------------------------------
-- Adjacent-distance clustering of already-seriated lists

pairwiseDistance :: [Double] -> [Double] -> Double
pairwiseDistance [] _ = error "Empty list passed to pairwiseDistance"
pairwiseDistance _ [] = error "Empty list passed to pairwiseDistance"
pairwiseDistance a b = cosineDistance (normalizeVector "pairwiseDistance/a" $ VU.fromList a)
                                      (normalizeVector "pairwiseDistance/b" $ VU.fromList b)

pairwiseDistanceEmbedding :: Embedding -> Embedding -> Double
pairwiseDistanceEmbedding a b = pairwiseDistance (last5 a) (last5 b)

-- | Adjacent distances in a seriated list. The Int is the split position after
-- the left item of the adjacent pair.
adjacentDistances :: EmbeddingIndex -> [FilePath] -> [(Double, Int)]
adjacentDistances ix ps = catMaybes $ zipWith gap [1..] (zip ps (drop 1 ps))
 where
  gap splitAtIndex (a, b) = do
    ra <- rowByPath ix a
    rb <- rowByPath ix b
    d <- rowDistance ra rb
    return (d, splitAtIndex)

-- Backwards-compatible name. It now reports adjacent, not all-pairs, distances.
pairwiseDistances :: [(FilePath, Embedding)] -> [(Double, FilePath)]
pairwiseDistances xs = catMaybes $ zipWith f xs (drop 1 xs)
 where
  f (fp1, e1) (_, e2) = Just (pairwiseDistanceEmbedding e1 e2, fp1)

splitAtIndices :: [Int] -> [a] -> [[a]]
splitAtIndices [] xs = [xs]
splitAtIndices (i:is) xs = firstPart : splitAtIndices (map (subtract i) is) remaining
 where
  (firstPart, remaining) = splitAt i xs

mergeSingletons :: [[a]] -> [[a]]
mergeSingletons []   = []
mergeSingletons [[]] = []
mergeSingletons [x]  = [x]
mergeSingletons (x:y:xs)
  | length x < 2 = mergeSingletons ((x ++ y) : xs)
  | otherwise    = x : mergeSingletons (y : xs)

clusterIntoSublist :: Embeddings -> [FilePath] -> [[FilePath]]
clusterIntoSublist [] x  = error $ "clusterIntoSubList: passed empty embedding database for arguments " ++ show x
clusterIntoSublist _ []  = [[]]
clusterIntoSublist _ [a] = [[a]]
clusterIntoSublist es list =
  let k = 1 `max` (round (sqrt (fromIntegral $ length list :: Double)) - 1)
  in if k == 1
       then [list]
       else
         let wanted = S.fromList list
             edbLocal = filter (\(p, _, _, _, _) -> p `S.member` wanted) es
             ix = embeddings2Index edbLocal
             gapsDescending = sortBy (flip $ comparing fst) $ adjacentDistances ix list
             splitPoints = sort $ map snd $ take k gapsDescending
         in mergeSingletons $ splitAtIndices splitPoints list
