module Config.GenerateSimilar where

import Data.List (isPrefixOf, isSuffixOf)

-- how many results do we want?
bestNEmbeddings :: Int
bestNEmbeddings = 20

-- how long is too long? OA guesstimates 1 BPE = 4 characters on average (<https://platform.openai.com/tokenizer>), so text-embedding-ada-002's 8,191 BPEs ~ 32,764 characters. If a call fails, the shell script will truncate the input and retry until it works so we don't need to set the upper limit too low.
maximumLength :: Int
maximumLength = 32700

-- how few suggestions is too few to bother the reader with the existence of a 'Similar Links' link? 1 is way too few, but 5 might be demanding too much?
minimumSuggestions :: Int
minimumSuggestions = 3

embeddingsPath :: String
embeddingsPath = "metadata/embeddings.bin"

-- in case of weird retrievals: for example, at one point, “Estimating the effect-size of gene dosage on cognitive ability across the coding genome” wsa somehow close to *every* embedding...?
blackList :: String -> Bool
blackList p = p `elem` blackListURLs
  || "/doc/" `isPrefixOf` p && "/index" `isSuffixOf` p
  || "/newsletter/20" `isPrefixOf` p
  || "/lorem" `isPrefixOf` p

-- (Only used in 'blackList' but pulled out as a separate binding for testing.)
-- Testing: `Test` as unique-list & valid URI (`isURIReference`)
blackListURLs :: [String]
blackListURLs = ["/index", "/changelog", "/help"]

maxDistance :: Double
maxDistance = 0.95 -- [chosen ad hoc by looking at a small sample and noting there seemed to be a cliff of relevancy at ~0.60 as of 2023-09-04 using text-embedding-ada-002 - WARNING: must be rechecked for every engine change!] [rechosen 2024-08-28 for new smaller truncated embedding]

-- tag title guessing of clusters
-- after 30 titles, I can't imagine that you get much better at suggesting a tag name, and then it's just wasting tokens:
maxTitlesForTagGuessing :: Int
maxTitlesForTagGuessing = 30

-- on directory pages, what should be the minimum number of auto-tags/clusters inferred before we bother to show the reader it?
-- Obviously, just 1 isn't very useful at all, but 2 might not be worth the overhead, and we usually use a '3' value.
-- (Used in 'generateDirectory.hs')
minTagAuto :: Int
minTagAuto = 3
