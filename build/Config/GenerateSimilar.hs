module Config.GenerateSimilar where

import Data.List (isPrefixOf, isSuffixOf)
import Utils (isUniqueList)

-- how many results do we want?
bestNEmbeddings :: Int
bestNEmbeddings = 20

-- how many characters long should a formatted annotation be before it is worth trying to embed?
minimumLength :: Int
minimumLength = 700

-- how long is too long? OA guesstimates 1 BPE = 4 characters on average (https://platform.openai.com/tokenizer), so text-embedding-ada-002's 8191 BPEs ~ 32764 characters. If a call fails, the shell script will truncate the input and retry until it works so we don't need to set the upper limit too low.
maximumLength :: Int
maximumLength = 32700

-- how few suggestions is too few to bother the reader with the existence of a 'Similar Links' link? 1 is way too few, but 5 might be demanding too much?
minimumSuggestions :: Int
minimumSuggestions = 3

-- prevent pathological loops by requesting no more than i times:
iterationLimit :: Int
iterationLimit = 4

embeddingsPath :: String
embeddingsPath = "metadata/embeddings.bin"

-- some weird cases: for example, “Estimating the effect-size of gene dosage on cognitive ability across the coding genome” is somehow close to *every* embedding...?
blackList :: String -> Bool
blackList p = p `elem` isUniqueList ["https://www.biorxiv.org/content/10.1101/2020.04.03.024554.full",
                                      "/doc/genetics/heritable/correlation/2019-kandler.pdf", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4210287/",
                                      "https://www.wired.com/1996/12/ffglass/", "https://andrewmayneblog.wordpress.com/2021/05/18/a-simple-method-to-keep-gpt-3-focused-in-a-conversation/",
                                      "https://www.dutchnews.nl/news/2022/07/german-fighter-pilot-identified-after-79-years-from-dna-on-envelope/",
                                      "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1065034/", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2653069/",
                                      "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2925254/", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2998793/",
                                      "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4763788/", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4921196/",
                                      "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6022844/", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8931369/",
                                      "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9232116/", "https://www.statnews.com/2022/07/28/abandoned-technique-revived-in-effort-to-make-artificial-human-eggs/",
                                      "https://www.thenationalnews.com/health/2022/09/07/woman-who-can-smell-parkinsons-helps-scientists-develop-new-test-for-condition/",
                                      "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4898064/"]
  || "/doc/" `isPrefixOf` p && "/index" `isSuffixOf` p

minDistance, maxDistance :: Double
maxDistance = 0.63 -- chosen ad hoc by looking at a small sample and noting there seemed to be a cliff of relevancy at ~0.60 as of 2023-09-04 using text-embedding-ada-002 - WARNING: must be rechecked for every engine change!
minDistance = 0.05 -- avoids self-matches and other odd errors
