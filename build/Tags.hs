{-# LANGUAGE OverloadedStrings #-}
module Tags where

import Control.Monad (filterM, unless)
import Data.Char (toLower)
import Data.Containers.ListUtils (nubOrd)
import Data.List (isSuffixOf, isInfixOf, isPrefixOf, sort, intersperse)
import System.Directory (doesFileExist)
import System.Directory.Recursive (getDirFiltered, getSubdirsRecursive) -- dir-traverse
import System.FilePath (takeDirectory)
import Text.Pandoc (Inline(Str, RawInline, Link, Span), Format(..), Block(Para, Div), nullAttr)
import qualified Data.Map.Strict as M (elems, map, toList )
import qualified Data.Text as T (append, pack, unpack, Text)

import LinkMetadataTypes (Metadata)
import Utils (anyInfix, replace, sed, sedMany, trim, split, replaceMany, frequency, pairs, fixedPoint)

-- Remind to refine link tags: should be <100. (We count using the annotation database instead of counting files inside each directory because so many are now cross-tagged or virtual.)
tagMax, tagPairMax :: Int
tagMax = 100
tagPairMax = 11
tagCount :: Metadata -> [(Int,String)]
tagCount = frequency . concatMap (\(_,(_,_,_,_,tags,_)) -> tags) . M.toList
tagPairsCount :: Metadata -> [(Int,(String,String))]
tagPairsCount md = reverse $ frequency $ concatMap pairs $ M.elems $ M.map (\(_,_,_,_,ts,abst) -> if null abst || null ts then [] else ts) md

-- Compile tags down into a Span containing a list of links to the respective /doc/ directory indexes which will contain a copy of all annotations corresponding to that tag/directory.
--
-- Simple version:
-- > tagsToLinksSpan "economics, genetics/heritable, psychology/writing"
-- →
-- Span ("",["link-tags"],[])
--   [Link ("",["link-tag"],[]) [Str "economics"] ("/doc/economics/index",""),Str ", ",
--     Link ("",["link-tag"],[]) [Str "genetics/heritable"] ("/doc/genetics/heritable/index",""),Str ", ",
--     Link ("",["link-tag"],[]) [Str "psychology/writing"] ("/doc/psychology/writing/index","")
--   ]
-- Markdown:
-- →
-- [[economics](/doc/economics/index){.link-tag}, [genetics/heritable](/doc/genetics/heritable/index){.link-tag}, [psychology/writing](/doc/psychology/writing/index){.link-tag}]{.link-tags}
-- HTML:
-- →
-- <span class="link-tags">
--   <a href="/doc/economics/index" class="link-tag">economics</a>,
--   <a href="/doc/genetics/heritable/index" class="link-tag">genetics/heritable</a>,
--   <a href="/doc/psychology/writing/index" class="link-tag">psychology/writing</a>
-- </span>
tagsToLinksSpan :: [T.Text] -> Inline
tagsToLinksSpan [] = Span nullAttr []
tagsToLinksSpan [""] = Span nullAttr []
tagsToLinksSpan ts = let tags = sort ts in
                       Span ("", ["link-tags"], []) $
                       intersperse (Str ", ") $ map (\tag -> Link ("", ["link-tag", "link-page", "link-annotated", "icon-not"], [("rel","tag")]) [RawInline (Format "html") $ abbreviateTag tag] ("/doc/"`T.append`tag`T.append`"/index", "Link to "`T.append`tag`T.append`" tag index") ) tags

-- Ditto; but since a Div is a Block element, we copy-paste a separate function:
tagsToLinksDiv :: [T.Text] -> Block
tagsToLinksDiv [] = Div nullAttr []
tagsToLinksDiv [""] = Div nullAttr []
tagsToLinksDiv ts = let tags = sort ts in
                       Div ("", ["link-tags"], []) $
                       [Para $ intersperse (Str ", ") $ map (\tag -> Link ("", ["link-tag", "link-page", "link-annotated", "icon-not"], [("rel","tag")]) [RawInline (Format "html") $ abbreviateTag tag] ("/doc/"`T.append`tag`T.append`"/index", "Link to "`T.append`tag`T.append`" tag index") ) tags]

-- if a local '/doc/*' file and no tags available, try extracting a tag from the path; eg. '/doc/ai/2021-santospata.pdf' → 'ai', '/doc/ai/anime/2021-golyadkin.pdf' → 'ai/anime' etc; tags must be lowercase to map onto directory paths, but we accept uppercase variants (it's nicer to write 'economics, sociology, Japanese' than 'economics, sociology, japanese')
tag2TagsWithDefault :: String -> String -> [String]
tag2TagsWithDefault path tags = let tags' = map trim $ split ", " $ map toLower tags
                                    defTag = if ("/doc/" `isPrefixOf` path) && (not ("/doc/biology/2000-iapac-norvir"`isPrefixOf`path || "/doc/rotten.com/"`isPrefixOf`path || "/doc/statistics/order/beanmachine-multistage"`isPrefixOf`path||"/doc/www/"`isPrefixOf`path)) then tag2Default path else ""
                                in
                                  if defTag `elem` tags' || defTag == "" || defTag == "/doc" then tags' else defTag:tags'

tag2Default :: String -> String
tag2Default path = if "/doc/" `isPrefixOf` path && not ("/doc/" `isPrefixOf` path && ("/index" `isSuffixOf` path || "/index#" `isInfixOf` path)) then replace "/doc/" "" $ takeDirectory path else ""

-- de-duplicate tags: uniquefy, and remove the more general tags in favor of nested (more specific) tags. eg. ["ai", "ai/nn/transformer/gpt", "reinforcement-learning"] → ["ai/nn/transformer/gpt", "reinforcement-learning"]
uniqTags :: [String] -> [String]
uniqTags tags = nubOrd $ sort $ filter(\t -> not (any ((t++"/") `isPrefixOf`) tags)) tags

-- guess tag based on URL
pages2Tags :: String -> [String] -> [String]
pages2Tags path oldTags = url2Tags path ++ oldTags

-- We also do general-purpose heuristics on the path/URL: any page in a domain might be given a specific tag, or perhaps any URL with the string "deepmind" might be given a 'reinforcement-learning/deepmind' tag—that sort of thing.
url2Tags :: String -> [String]
url2Tags p = concatMap (\(match,tag) -> if match p then [tag] else []) urlTagDB
 where -- we allow arbitrary string predicates (so one might use regexps as well)
        urlTagDB :: [((String -> Bool), String)]
        urlTagDB = [
            (("https://publicdomainreview.org"`isPrefixOf`),          "history/public-domain-review")
          , (("https://www.filfre.net/"`isPrefixOf`),                 "technology/digital-antiquarian")
          , (("https://abandonedfootnotes.blogspot.com"`isPrefixOf`), "sociology/abandoned-footnotes")
          , (("https://dresdencodak.com"`isPrefixOf`), "humor")
          , (("https://www.theonion.com"`isPrefixOf`), "humor")
          , (("https://tvtropes.org"`isPrefixOf`), "fiction")
          , ((\u -> anyInfix u ["evageeks.org","eva.onegeek.org"]),  "anime/eva")
          , (("evamonkey.com"`isInfixOf`), "anime/eva")
          , (("r-project.org"`isInfixOf`), "cs/r")
          , (("haskell.org"`isInfixOf`), "cs/haskell")
          ]

-- Abbreviate displayed tag names to make tag lists more readable. For some tags, like 'reinforcement-learning/*' or 'genetics/*', they might be used very heavily and densely, leading to cluttered unreadable tag lists, and discouraging use of meaningful directory names: 'reinforcement-learning/exploration, reinforcement-learning/alphago, reinforcement-learning/meta-learning, reinforcement-learning/...' would be quite difficult to read. But we also would rather not abbreviate the tag itself down to just 'rl/', as that is not machine-readable or explicit. So we can abbreviate them just for display, while rendering the tags to Inline elements.
abbreviateTag :: T.Text -> T.Text
abbreviateTag = T.pack . sedMany tagRewritesRegexes . replaceMany tagsLong2Short . replace "/doc/" "" . T.unpack
  where
        tagRewritesRegexes  :: [(String,String)]
        tagRewritesRegexes = [("^cs/", "CS/")
                             , ("^cs$", "CS")
                             , ("^cs/c$", "C")
                             , ("^cs/r$", "R")
                             , ("^ai/", "AI/")
                             , ("^ai$", "AI")
                             , ("^iq/", "IQ/")
                             , ("^iq$", "IQ")
                             , ("^iq/high$", "high IQ")
                             , ("^anime/eva$", "<em>NGE</em>")
                             , ("^gan$", "GAN")
                             , ("^psychology/", "psych/")
                             , ("^technology/", "tech/")
                             , ("^doc$", "Tags Index") -- NOTE: nothing is tagged this, so this just sets the <title> on /doc/index to something more useful than 'docs tag'.
                             ]

listTagsAll :: IO [String]
listTagsAll = fmap (map (replace "doc/" "") . sort . filter (\f' -> not $ anyInfix f' ["personal/2011-gwern-yourmorals.org", "rotten.com", "2000-iapac-norvir", "beanmachine-multistage", "doc/www/"]) ) $ getDirFiltered (\f -> doesFileExist (f++"/index.page")) "doc/"

-- given a list of ["doc/foo/index.page"] directories, convert them to what will be the final absolute path ("/doc/foo/index"), while checking they exist (typos are easy, eg. dropping 'doc/' is common).
listTagDirectories :: [FilePath] -> IO [FilePath]
listTagDirectories direntries' = do
                       directories <- mapM getSubdirsRecursive $ map (sed "^/" "" . sed "/index$" "/" . replace "/index.page" "/")  direntries'
                       let directoriesMi = map (replace "//" "/" . (++"/index")) (concat directories)
                       directoriesVerified <- filterM (\f -> doesFileExist (f++".page")) directoriesMi
                       return $ sort $ map ("/"++) directoriesVerified

-- try to infer a long tag from a short tag, first by exact match, then by suffix, then by prefix, then by infix, then give up.
-- so eg. 'sr1' → 'SR1' → 'darknet-markets/silk-road/1', 'road/1' → 'darknet-markets/silk-road/1', 'darknet-markets/silk' → 'darknet-markets/silk-road', 'silk-road' → 'darknet-markets/silk-road'
guessTagFromShort :: [String] -> String -> String
guessTagFromShort _ "" = ""
guessTagFromShort l s = fixedPoint (f l) s
 where f m t = let allTags = nubOrd $ sort m in
                 if t `elem` allTags then t else -- exact match, no guessing required
                 case lookup t tagsShort2Long of
                   Just tl -> tl -- is an existing short/petname
                   Nothing -> let shortFallbacks =
                                    (map (\a->(a,"")) $ filter (\tag -> ("/"++ t) `isSuffixOf` tag) allTags) ++
                                    (map (\a->(a,"")) $ filter (\tag -> ("/"++ t++"/") `isInfixOf` tag) allTags) ++ -- look for matches by path segment eg. 'transformer' → 'ai/nn/transformer' (but not 'ai/nn/transformer/alphafold' or 'ai/nn/transformer/gpt')
                                    (map (\a->(a,"")) $ filter (\tag -> ("/"++t) `isSuffixOf` tag || (t++"/") `isInfixOf` tag) allTags) ++ -- look for matches by partial path segment eg. 'bias' → ' psychology/cognitive-bias/illusion-of-depth'
                                    filter (\(short,_) -> t `isSuffixOf` short) tagsShort2Long ++
                                    filter (\(short,_) -> t `isPrefixOf` short) tagsShort2Long ++
                                    filter (\(short,_) -> t `isInfixOf` short) tagsShort2Long
                              in if not (null shortFallbacks) then fst $ head shortFallbacks else
                                   let longFallbacks = filter (t `isSuffixOf`) allTags ++ filter (t `isPrefixOf`) allTags ++ filter (t `isInfixOf`) allTags in
                                     if not (null longFallbacks) then head longFallbacks else t

-- intended for use with full literal fixed-string matches, not regexps/infix/suffix/prefix matches.
tagsLong2Short, tagsShort2Long :: [(String,String)]
tagsShort2Long = [("power", "statistics/power-analysis"), ("statistics/power", "statistics/power-analysis"), ("reinforcement-learning/robotics", "reinforcement-learning/robot"), ("reinforcement-learning/robotic", "reinforcement-learning/robot"), ("dogs", "dog"), ("dog/genetics", "genetics/heritable/dog"), ("dog/cloning", "genetics/cloning/dog"), ("genetics/selection/artificial/apple-breeding","genetics/selection/artificial/apple"), ("apples", "genetics/selection/artificial/apple"), ("T5", "ai/nn/transformer/t5"), ("link-rot", "cs/linkrot"), ("linkrot", "cs/linkrot"), ("ai/clip", "ai/nn/transformer/clip"), ("clip/samples", "ai/nn/transformer/clip/sample"), ("samples", "ai/nn/transformer/clip/sample"), ("japanese", "japan"), ("quantised", "ai/nn/sparsity/low-precision"), ("quantized", "ai/nn/sparsity/low-precision"), ("quantization", "ai/nn/sparsity/low-precision") , ("reduced-precision", "ai/nn/sparsity/low-precision"), ("mixed-precision", "ai/nn/sparsity/low-precision"), ("evolution", "genetics/selection/natural"), ("gpt-3", "ai/nn/transformer/gpt"), ("gpt3", "ai/nn/transformer/gpt"), ("gpt/nonfiction", "ai/nn/transformer/gpt/non-fiction"), ("red", "design/typography/rubrication"), ("self-attention", "ai/nn/transformer/attention"), ("efficient-attention", "ai/nn/transformer/attention"), ("ai/rnn", "ai/nn/rnn"), ("ai/retrieval", "ai/nn/retrieval"), ("mr", "genetics/heritable/correlation/mendelian-randomization"), ("japan/anime", "anime"), ("psychology/bird", "psychology/animal/bird"), ("psychology/birds/neuroscience", "psychology/animal/bird/neuroscience"), ("psychology/birds", "psychology/animal/bird"), ("dalle","dall-e"), ("dall-e", "ai/nn/transformer/gpt/dall-e"), ("silk-road-1", "darknet-markets/silk-road/1"), ("sr1", "darknet-markets/silk-road/1"), ("silk-road-2", "darknet-markets/silk-road/2"), ("sr2", "darknet-markets/silk-road/2"), ("psychology/neuroscience/bird", "psychology/animal/bird/neuroscience"), ("uighurs", "history/uighur"), ("ai/adversarial", "ai/nn/adversarial"), ("add", "psychiatry/adhd"), ("asperger", "psychiatry/autism"), ("aspergers", "psychiatry/autism"), ("personality/conscientiousness", "psychology/personality/conscientiousness"), ("conscientiousness", "psychology/personality/conscientiousness"), ("anorexia-nervosa", "psychiatry/anorexia"), ("anxiety-disorder", "psychiatry/anxiety"), ("masked-auto-encoder", "ai/nn/vae/mae"), ("masked-autoencoder", "ai/nn/vae/mae"), ("masked", "ai/nn/vae/mae"), ("alzheimer's", "psychiatry/alzheimers"), ("ad", "psychiatry/alzheimers"), ("alzheimers-disease", "psychiatry/alzheimers"), ("alzheimer", "psychiatry/alzheimers"), ("psychedelics", "psychedelic"), ("stylometric", "statistics/stylometry"), ("stylometrics", "statistics/stylometry"), ("dune", "fiction/science-fiction/frank-herbert"), ("herbert", "fiction/science-fiction/frank-herbert"), ("instruct-tuning", "instruction-tuning"), ("instruction-finetuning", "instruction-tuning"), ("psychopath", "psychology/personality/psychopathy"), ("sociopath", "psychology/personality/psychopathy"), ("psychopathic", "psychology/personality/psychopathy"), ("sociopathic", "psychology/personality/psychopathy"), ("cognitive-biases", "psychology/cognitive-bias"), ("sort", "cs/algorithm/sorting"), ("moe", "ai/scaling/mixture-of-experts"), ("ai/datasets", "ai/dataset"), ("ai/gan", "ai/nn/gan"), ("safety", "reinforcement-learning/safe"), ("ads", "economics/advertising"), ("rl/scaling", "reinforcement-learning/scaling"), ("rl/scale", "reinforcement-learning/scaling"), ("reinforcement-learning/scale", "reinforcement-learning/scaling"), ("rl-scaling", "reinforcement-learning/scaling"), ("scaling/rl", "reinforcement-learning/scaling"), ("scaling/reinforcement-learning", "reinforcement-learning/scaling"), ("reinforcement-learning/alphago", "reinforcement-learning/model/alphago"), ("evolution/human", "genetics/selection/natural/human"), ("rl/chess", "reinforcement-learning/chess"), ("xrisk", "existential-risk"), ("human-adversarial", "ai/nn/adversarial/human"), ("adversarial-human", "ai/nn/adversarial/human"), ("mlps", "ai/nn/fully-connected"), ("mlp", "ai/nn/fully-connected"), ("gpt-4", "ai/nn/transformer/gpt/4"), ("gpt4", "ai/nn/transformer/gpt/4"), ("gp-4", "ai/nn/transformer/gpt/4"), ("gp4", "ai/nn/transformer/gpt/4"), ("gpt-5", "ai/nn/transformer/gpt/5"), ("gpt5", "ai/nn/transformer/gpt/5"), ("gp-5", "ai/nn/transformer/gpt/5"), ("gp5", "ai/nn/transformer/gpt/5"), ("attention/sparse", "ai/nn/transformer/attention/sparsity"), ("gp4-4", "ai/nn/transformer/gpt/4"), ("gp4", "ai/nn/transformer/gpt/4"), ("gpt-4/nonfiction", "ai/nn/transformer/gpt/4/nonfiction"), ("ai/nn/transformer/gpt/4/non-fiction", "ai/nn/transformer/gpt/4/nonfiction"), ("gpt-4/non-fiction", "ai/nn/transformer/gpt/4/nonfiction"), ("4/non", "ai/nn/transformer/gpt/4/nonfiction"), ("gpt-4/fiction", "ai/nn/transformer/gpt/4/fiction"), ("gpt-4/poetry", "ai/nn/transformer/gpt/4/poetry"), ("gpt-4poetry", "ai/nn/transformer/gpt/4/poetry"), ("gpt4/poetry", "ai/nn/transformer/gpt/4/poetry"), ("gpt-4/poem", "ai/nn/transformer/gpt/4/poetry"), ("chess", "reinforcement-learning/chess"), ("animal", "psychology/animal"), ("artificial", "ai"), ("code", "cs"), ("for", "statistics/prediction"), ("forecast", "statistics/prediction"), ("forecasting", "statistics/prediction"), ("genetic", "genetics"), ("graph", "design/visualization"), ("hardware" , "cs/hardware"), ("human" , "genetics/selection/natural/human"), ("learning", "reinforcement-learning"), ("sf", "fiction/science-fiction"), ("text" , "fiction/text-game"), ("psych", "psychology"), ("psych/inner-monologue", "psychology/inner-monologue")] ++
                 -- ^ custom tag shortcuts, to fix typos etc
                 (map (\s -> (s, error s)) ["a", "al", "an", "analysis", "and", "are", "as", "at", "be", "box", "done", "e", "error", "f", "fine", "free", "g", "git", "if", "in", "is", "it", "of", "on", "option", "rm", "sed", "strong", "the", "to", "tr", "up", "we"]) ++ -- hopelessly ambiguous ones which should be error (for now)
                 -- attempt to infer short->long rewrites from the displayed tag names, which are long->short; but note that many of them are inherently invalid and the mapping only goes one way.
                  (map (\(a,b) -> (map toLower b,a)) $ filter (\(_,fancy) -> not (anyInfix fancy [" ", "<", ">", "(",")"])) tagsLong2Short)
tagsLong2Short = [
          ("reinforcement-learning", "RL")
          , ("music/music-distraction", "music distraction")
          , ("reinforcement-learning/chess", "AI chess")
          , ("ai/dataset", "ML dataset")
          , ("ai/tabular", "tabular ML")
          , ("ai/nn/adversarial", "adversarial examples (AI)")
          , ("ai/nn/adversarial/human", "adversarial examples (human)")
          , ("ai/anime", "anime AI")
          , ("ai/anime/danbooru", "Danbooru AI")
          , ("anime/eva/little-boy", "<em>Little Boy</em>")
          , ("ai/nn", "neural net")
          , ("ai/nn/sampling", "NN sampling")
          , ("ai/nn/cnn", "CNN")
          , ("ai/nn/rnn", "RNN")
          , ("ai/nn/fully-connected", "MLP NN")
          , ("ai/nn/transformer", "Transformer NN")
          , ("ai/nn/vae", "autoencoder NN")
          , ("ai/nn/vae/mae", "masked auto-encoder")
          , ("ai/scaling", "AI scaling")
          , ("ai/scaling/mixture-of-experts", "MoE NN")
          , ("ai/scaling/emergence", "emergence (AI)")
          , ("ai/nn/tokenization", "LM tokenization")
          , ("ai/nn/retrieval", "retrieval AI")
          , ("iq/animal", "animal cognition")
          , ("iq/ses", "IQ/SES")
          , ("iq/high/smpy", "SMPY")
          , ("iq/high/munich", "Munich Giftedness Study")
          , ("iq/high/fullerton", "Fullerton Longitudinal Study")
          , ("iq/high/anne-roe", "Anne Roe's Scientists")
          , ("ai/nn/transformer/clip", "CLIP")
          , ("ai/nn/transformer/clip/sample", "CLIP samples")
          , ("ai/nn/transformer/attention", "self-attention")
          , ("design/typography", "typography")
          , ("design/typography/rubrication", "rubricated typography")
          , ("design/visualization", "data visualization")
          , ("vitamin-d", "Vitamin D")
          , ("dual-n-back", "DNB")
          , ("ai/nn/diffusion", "diffusion model")
          , ("ai/nn/diffusion/discrete ", "discrete diffusion")
          , ("ai/nn/gan", "GAN")
          , ("ai/nn/gan/biggan", "BigGAN")
          , ("ai/nn/gan/stylegan", "StyleGAN")
          , ("ai/fiction", "fiction by AI")
          , ("ai/nn/transformer/gpt",                    "GPT")
          , ("ai/nn/transformer/gpt/non-fiction",        "GPT non-fiction")
          , ("ai/nn/transformer/gpt/inner-monologue",    "inner monologue (AI)")
          , ("ai/nn/transformer/gpt/codex",              "Codex")
          , ("ai/nn/transformer/gpt/lamda",              "LaMDA")
          , ("ai/nn/transformer/gpt/palm",               "PaLM")
          , ("ai/nn/transformer/gpt/dall-e",             "DALL·E")
          , ("ai/nn/transformer/gpt/fiction",            "GPT fiction")
          , ("ai/nn/transformer/gpt/poetry",             "GPT poetry")
          , ("ai/nn/transformer/gpt/jukebox",            "Jukebox")
          , ("ai/nn/transformer/gpt/instruction-tuning", "instruct-tuning LM")
          , ("ai/nn/transformer/gpt/4",                  "GPT-4")
          , ("ai/nn/transformer/gpt/4/fiction",          "GPT-4 fiction")
          , ("ai/nn/transformer/gpt/4/nonfiction",       "GPT-4 nonfiction")
          , ("ai/nn/transformer/gpt/4/poetry",           "GPT-4 poetry")
          , ("ai/nn/transformer/gpt/5",                  "GPT-5")
          , ("ai/nn/transformer/alphafold", "AlphaFold")
          , ("ai/nn/transformer/t5", "T5 Transformer")
          , ("ai/nn/transformer/attention/recurrent", "recurrent Transformer")
          , ("ai/nn/transformer/attention/compression", "compressed Transformer")
          , ("ai/nn/transformer/attention/linear-algebra", "Transformer matrix optimization")
          , ("ai/nn/transformer/attention/sparsity", "sparse Transformer")
          , ("ai/nn/transformer/attention/hierarchical", "multi-scale Transformer")
          , ("ai/highleyman", "Highleyman")
          , ("ai/nn/sparsity", "NN sparsity")
          , ("ai/nn/sparsity/low-precision", "reduced-precision NNs")
          , ("ai/nn/sparsity/pruning", "NN pruning")
          , ("ai/nn/sparsity/knowledge-distillation", "knowledge distillation")
          , ("existential-risk", "x-risk")
          , ("philosophy/ethics", "ethics")
          , ("philosophy/brethren-of-purity", "Brethren of Purity")
          , ("longevity/tirzepatide", "tirzepatide")
          , ("longevity/semaglutide", "glutide")
          , ("exercise/gravitostat", "the gravitostat")
          , ("japan/history/tominaga-nakamoto", "Tominaga Nakamoto")
          , ("conscientiousness", "Conscientiousness")
          , ("ai/text-style-transfer", "text style transfer")
          , ("ai/video/generation", "video generation")
          , ("ai/video/analysis", "video analysis")
          , ("modafinil/darknet-market", "modafinil DNM")
          , ("history/s-l-a-marshall", "SLAM")
          , ("cs/cellular-automaton", "cellular automata")
          , ("cs/shell", "shell")
          , ("cs/scheme", "Scheme")
          , ("cs/css", "CSS")
          , ("cs/cryptography", "crypto")
          , ("cs/js", "JS")
          , ("cs/haskell", "Haskell")
          , ("cs/python", "Python")
          , ("cs/end-to-end-principle", "end-to-end")
          , ("cs/algorithm/sorting", "sorting")
          , ("philosophy/frank-p-ramsey", "Frank Ramsey")
          , ("nootropic/quantified-self", "QS")
          , ("darknet-market", "DNM")
          , ("darknet-market/dnm-archive", "DNM Archives")
          , ("darknet-market/agora", "Agora DNM")
          , ("darknet-market/alphabay", "AlphaBay DNM")
          , ("darknet-market/atlantis", "Atlantis DNM")
          , ("darknet-market/blackmarket-reloaded", "BMR DNM")
          , ("darknet-market/evolution", "Evolution DNM")
          , ("darknet-market/sheep-marketplace", "Sheep DNM")
          , ("darknet-market/silk-road", "SR DNMs")
          , ("darknet-market/silk-road/1", "SR1 DNM")
          , ("darknet-market/silk-road/2", "SR2 DNM")
          , ("darknet-market/william-pickard", "William Pickard")
          , ("reinforcement-learning/model", "model-based RL")
          , ("reinforcement-learning/model-free", "model-free RL")
          , ("reinforcement-learning/model/decision-transformer", "Decision Transformer")
          , ("reinforcement-learning/model/muzero", "MuZero")
          , ("reinforcement-learning/model/alphago", "AlphaGo")
          , ("reinforcement-learning/model-free/alphastar", "AlphaStar")
          , ("reinforcement-learning/model-free/oa5", "OA5")
          , ("reinforcement-learning/nethack", "<em>Nethack</em> AI")
          , ("history/uighur", "Uighur genocide")
          , ("history/public-domain-review", "<em>Public Domain Review</em>")
          , ("technology", "tech")
          , ("technology/stevensinstituteoftechnology-satmnewsletter", "<em>SATM</em> archive")
          , ("technology/carbon-capture", "carbon capture")
          , ("technology/digital-antiquarian", "<em>Filfre</em>")
          , ("technology/google", "Google")
          , ("technology/security", "infosec")
          , ("technology/search", "Google-fu")
          , ("cs/linkrot/archiving", "archiving")
          , ("reinforcement-learning/openai", "OA")
          , ("reinforcement-learning/deepmind", "DM")
          , ("reinforcement-learning/meta-learning", "meta-learning")
          , ("reinforcement-learning/preference-learning", "preference learning")
          , ("reinforcement-learning/multi-agent", "MARL")
          , ("reinforcement-learning/imperfect-information/diplomacy", "<em>Diplomacy</em> AI")
          , ("reinforcement-learning/imperfect-information/hanabi", "<em>Hanabi</em> AI")
          , ("reinforcement-learning/imperfect-information/poker", "poker AI")
          , ("reinforcement-learning/robot", "robotics")
          , ("reinforcement-learning/safe", "AI safety")
          , ("reinforcement-learning/exploration", "RL exploration")
          , ("reinforcement-learning/exploration/active-learning", "active learning")
          , ("reinforcement-learning/scaling", "RL scaling")
          , ("statistics/prediction/election", "election forecast")
          , ("statistics/prediction", "forecasting")
          , ("statistics/bias/animal", "animal study biases")
          , ("psychology/cognitive-bias", "cognitive biases")
          , ("psychology/cognitive-bias/stereotype-threat", "stereotype threat")
          , ("psychology/dark-knowledge", "human dark knowledge")
          , ("psychology/nature", "psych of nature")
          , ("psychology/okcupid", "OKCupid")
          , ("psychology/personality", "personality")
          , ("psychology/personality/conscientiousness", "Conscientiousness")
          , ("psychology/chess", "chess psychology")
          , ("psychology/novelty", "novelty U-curve")
          , ("psychology/energy", "mental energy")
          , ("psychology/illusion-of-depth", "illusion of depth")
          , ("psychology/neuroscience", "neuroscience")
          , ("psychology/animal", "animal psych")
          , ("psychology/animal/bird", "bird")
          , ("psychology/animal/bird/neuroscience", "bird brains")
          , ("psychology/european-journal-of-parapsychology", "<em>EJP</em>")
          , ("psychology/spaced-repetition", "spaced repetition")
          , ("sociology/abandoned-footnotes", "<em>Abandoned Footnotes</em>")
          , ("sociology/preference-falsification", "preference falsification")
          , ("statistics/survival-analysis", "survival analysis")
          , ("statistics/variance-component", "variance components")
          , ("statistics/order/comparison", "statistical comparison")
          , ("philosophy/ethics/ethicists", "ethicists")
          , ("statistics/meta-analysis", "meta-analysis")
          , ("statistics/power-analysis", "power analysis")
          , ("statistics/bayes", "Bayes")
          , ("statistics/order", "order statistics")
          , ("statistics/decision", "decision theory")
          , ("statistics/stylometry", "stylometry")
          , ("psychiatry/alzheimers", "Alzheimer’s")
          , ("psychiatry/traumatic-brain-injury", "TBI")
          , ("psychiatry/anxiety", "anxiety")
          , ("psychiatry/adhd", "ADHD")
          , ("psychiatry/anorexia", "anorexia")
          , ("psychiatry/schizophrenia", "SCZ")
          , ("psychiatry/schizophrenia/rosenhan", "Rosenhan fraud")
          , ("psychiatry/bipolar", "BPD")
          , ("psychiatry/depression", "MDD")
          , ("psychiatry/meditation", "meditation")
          , ("psychology/personality/psychopathy", "psychopath")
          , ("longevity/johan-bjorksten", "Johan Bjorksten")
          , ("longevity/senolytic", "senolytics")
          , ("genetics/sequencing", "gene sequencing")
          , ("genetics/editing", "gene editing")
          , ("genetics/cloning", "cloning")
          , ("genetics/cloning/dog", "dog cloning")
          , ("genetics/heritable", "heritability")
          , ("genetics/heritable/dog", "dog genetics")
          , ("genetics/heritable/adoption", "adoption")
          , ("genetics/selection/artificial/apple", "apple breeding")
          , ("genetics/selection/artificial/index-selection", "index selection")
          , ("genetics/heritable/rare", "rare genes")
          , ("genetics/heritable/emergenesis", "emergenesis")
          , ("genetics/selection/natural/human", "human evolution")
          , ("genetics/selection/natural/human/dysgenics", "dysgenics")
          , ("genetics/genome-synthesis/virus-proof", "virus-proof cells")
          , ("genetics/gametogenesis", "gametogenesis")
          , ("genetics/heritable/correlation", "genetic correlation")
          , ("genetics/heritable/correlation/mendelian-randomization", "Mendelian Randomization")
          , ("genetics/microbiome", "microbiome")
          , ("economics/georgism", "Georgism")
          , ("economics/experience-curve", "experience curves")
          , ("economics/advertising", "ads")
          , ("fiction/criticism", "literary criticism")
          , ("fiction/humor", "humor")
          , ("fiction/text-game", "text game")
          , ("fiction/gene-wolfe", "Gene Wolfe")
          , ("fiction/science-fiction/frank-herbert", "<em>Dune</em>")
          , ("cat/valerian", "Valerian (cat)")
          , ("cat/tatarian-honeysuckle", "Tatarian honeysuckle (cat)")
          , ("cat/catnip", "catnip")
          , ("cat/catnip/survey", "catnip survey")
          , ("cat/genetics", "cat genetics")
          , ("cat/psychology", "cat psychology")
          , ("cat/earwax", "cats & earwax")
          , ("crime/terrorism", "terrorism")
          , ("crime/terrorism/rumiyah", "<em>Rumiyah</em> (ISIS)")
          , ("modafinil/survey", "modafinil surveys")
          , ("lesswrong-survey", "LW surveys")
          , ("lesswrong-survey/hpmor", "<em>HP:MoR</em> surveys")
          , ("history/medici", "Medici")
          , ("biology/portia", "<em>Portia</em> spider")
          , ("fiction/opera", "opera")
          , ("fiction/poetry", "poetry")
          , ("fiction/science-fiction", "Sci-Fi")
          , ("insight-porn", "insight porn")
          , ("wikipedia", "Wikipedia")
          , ("psychology/cognitive-bias/sunk-cost", "sunk cost bias")
          , ("radiance", "<em>Radiance</em>")
          , ("long-now", "Long Now")
          , ("japan", "Japan")
          , ("japan/history", "Japanese history")
          , ("japan/art", "Japanese art")
          , ("japan/poetry", "Japanese poetry")
          , ("japan/poetry/zeami",    "Zeami Motokiyo")
          , ("japan/poetry/shotetsu", "Shōtetsu")
          , ("japan/poetry/teika",    "Fujiwara no Teika")
          , ("algernon", "Algernon's Law")
          , ("cs/haskell", "Haskell")
          , ("borges", "J. L. Borges")
          , ("bitcoin", "Bitcoin")
          , ("bitcoin/nashx", "Nash eXchange")
          , ("bitcoin/pirateat40", "Pirateat40")
          , ("touhou", "Touhou")
          , ("zeo", "sleep")
          , ("zeo/short-sleeper", "short sleepers")
          , ("co2", "CO<sub>2</sub>")
          , ("traffic", "web traffic")
          , ("psychology/neuroscience/tcs", "TDCS")
          ]

testTags :: IO ()
testTags = do tags <- listTagsAll
              let results = shortTagTestSuite tags
              unless (null results) $ error ("Tags.hs: test suite errored out with some rewrites going awry; results: " ++ show results)

shortTagTestSuite ::[String] -> [(String, String)]
shortTagTestSuite alltags = filter (\(input,output) -> guessTagFromShort alltags input /= output) [("active-learning"
          , "reinforcement-learning/exploration/active-learning"
          )
        , ("add" , "psychiatry/adhd")
        , ("adhd" , "psychiatry/adhd")
        , ("adoption" , "genetics/heritable/adoption")
        , ("adversarial" , "ai/nn/adversarial")
        , ("advertising" , "economics/advertising")
        , ("agora" , "darknet-market/agora")
        , ("ai/adversarial" , "ai/nn/adversarial")
        , ("ai/clip" , "ai/nn/transformer/clip")
        , ("ai/gan" , "ai/nn/gan")
        , ("ai/retrieval" , "ai/nn/retrieval")
        , ("ai/rnn" , "ai/nn/rnn")
        , ("algorithm" , "cs/algorithm")
        , ("alphabay" , "darknet-market/alphabay")
        , ("alphafold" , "ai/nn/transformer/alphafold")
        , ("alphago" , "reinforcement-learning/model/alphago")
        , ("alzheimers" , "psychiatry/alzheimers")
        , ("animal" , "psychology/animal")
        , ("anorexia" , "psychiatry/anorexia")
        , ("anxiety" , "psychiatry/anxiety")
        , ("apple" , "genetics/selection/artificial/apple")
        , ("archiving" , "cs/linkrot/archiving")
        , ("artificial" , "ai")
        , ("aspirin" , "longevity/aspirin")
        , ("attention" , "ai/nn/transformer/attention")
        , ("attention/hierarchical"
          , "ai/nn/transformer/attention/hierarchical"
          )
        , ("attention/recurrent"
          , "ai/nn/transformer/attention/recurrent"
          )
        , ("autism" , "psychiatry/autism")
        , ("automation" , "economics/automation")
        , ("bayes" , "statistics/bayes")
        , ("bias" , "statistics/bias")
        , ("biggan" , "ai/nn/gan/biggan")
        , ("bipolar" , "psychiatry/bipolar")
        , ("bird" , "psychology/animal/bird")
        , ("bird/neuroscience" , "psychology/animal/bird/neuroscience")
        , ("brain-imitation-learning"
          , "reinforcement-learning/brain-imitation-learning"
          )
        , ("c" , "cs/c")
        , ("caffeine" , "nootropic/caffeine")
        , ("calibration" , "ai/nn/transformer/gpt/calibration")
        , ("carbon-capture" , "technology/carbon-capture")
        , ("catnip" , "cat/catnip")
        , ("causality" , "statistics/causality")
        , ("cellular-automaton" , "cs/cellular-automaton")
        , ("chess" , "reinforcement-learning/chess")
        , ("clip" , "ai/nn/transformer/clip")
        , ("clip/samples" , "ai/nn/transformer/clip/sample")
        , ("cloning" , "genetics/cloning")
        , ("cnn" , "ai/nn/cnn")
        , ("code" , "cs")
        , ("codex" , "ai/nn/transformer/gpt/codex")
        , ("cognitive-bias" , "psychology/cognitive-bias")
        , ("collecting" , "psychology/collecting")
        , ("comparison" , "statistics/order/comparison")
        , ("computable" , "cs/computable")
        , ("conscientiousness"
          , "psychology/personality/conscientiousness"
          )
        , ("copyright" , "economics/copyright")
        , ("correlation" , "genetics/heritable/correlation")
        , ("cost" , "psychology/cognitive-bias/sunk-cost")
        , ("cryptography" , "cs/cryptography")
        , ("css" , "cs/css")
        , ("dall-e" , "ai/nn/transformer/gpt/dall-e")
        , ("danbooru" , "ai/anime/danbooru")
        , ("dark-knowledge" , "psychology/dark-knowledge")
        , ("data" , "ai/dataset")
        , ("data-augmentation" , "ai/nn/gan/data-augmentation")
        , ("decision" , "statistics/decision")
        , ("decision-transformer"
          , "reinforcement-learning/model/decision-transformer"
          )
        , ("deepmind" , "reinforcement-learning/deepmind")
        , ("depression" , "psychiatry/depression")
        , ("des" , "design")
        , ("diff" , "ai/nn/diffusion")
        , ("diffusion" , "ai/nn/diffusion")
        , ("diplomacy"
          , "reinforcement-learning/imperfect-information/diplomacy"
          )
        , ("discrete" , "ai/nn/diffusion/discrete")
        , ("dnm-archive" , "darknet-market/dnm-archive")
        , ("do" , "dog")
        , ("dog/genetics" , "genetics/heritable/dog")
        , ("dream" , "psychology/vision/dream")
        , ("dune" , "fiction/science-fiction/frank-herbert")
        , ("editing" , "genetics/editing")
        , ("election" , "statistics/prediction/election")
        , ("emergence" , "ai/scaling/emergence")
        , ("emergenesis" , "genetics/heritable/emergenesis")
        , ("end-to-end" , "cs/end-to-end-principle")
        , ("end-to-end-principle" , "cs/end-to-end-principle")
        , ("energy" , "psychology/energy")
        , ("epigenetic" , "longevity/epigenetics")
        , ("epigenetics" , "longevity/epigenetics")
        , ("epistemology" , "philosophy/epistemology")
        , ("ethicists" , "philosophy/ethics/ethicists")
        , ("ethics" , "philosophy/ethics")
        , ("eva" , "anime/eva")
        , ("evolution" , "genetics/selection/natural")
        , ("evolution/human" , "genetics/selection/natural/human")
        , ("experience-curve" , "economics/experience-curve")
        , ("exploration" , "reinforcement-learning/exploration")
        , ("for" , "statistics/prediction")
        , ("frank-herbert" , "fiction/science-fiction/frank-herbert")
        , ("full" , "ai/nn/fully-connected")
        , ("fully-connected" , "ai/nn/fully-connected")
        , ("gametogenesis" , "genetics/gametogenesis")
        , ("gan" , "ai/nn/gan")
        , ("generation" , "ai/video/generation")
        , ("genetic" , "genetics")
        , ("gene-wolfe" , "fiction/gene-wolfe")
        , ("genome-synthesis" , "genetics/genome-synthesis")
        , ("georgism" , "economics/georgism")
        , ("google" , "technology/google")
        , ("gp-4" , "ai/nn/transformer/gpt/4")
        , ("gp4" , "ai/nn/transformer/gpt/4")
        , ("gpt" , "ai/nn/transformer/gpt")
        , ("gpt-3" , "ai/nn/transformer/gpt")
        , ("gpt-4" , "ai/nn/transformer/gpt/4")
        , ("gpt4" , "ai/nn/transformer/gpt/4")
        , ("gpt-4/fiction" , "ai/nn/transformer/gpt/4/fiction")
        , ("gpt-4/non" , "ai/nn/transformer/gpt/4/nonfiction")
        , ("gpt/4/non" , "ai/nn/transformer/gpt/4/nonfiction")
        , ("gpt-4/nonfiction" , "ai/nn/transformer/gpt/4/nonfiction")
        , ("gpt/4/non-fiction" , "ai/nn/transformer/gpt/4/nonfiction")
        , ("gpt/4/nonfiction" , "ai/nn/transformer/gpt/4/nonfiction")
        , ("gpt-4/poetry" , "ai/nn/transformer/gpt/4/poetry")
        , ("gpt/4/poetry" , "ai/nn/transformer/gpt/4/poetry")
        , ("gpt4/poetry" , "ai/nn/transformer/gpt/4/poetry")
        , ("gpt-4poetry" , "ai/nn/transformer/gpt/4/poetry")
        , ("gpt/codex" , "ai/nn/transformer/gpt/codex")
        , ("gpt/fiction" , "ai/nn/transformer/gpt/fiction")
        , ("gpt/inner-monologue"
          , "ai/nn/transformer/gpt/inner-monologue"
          )
        , ("gpt/non" , "ai/nn/transformer/gpt/non-fiction")
        , ("gpt/non-fiction" , "ai/nn/transformer/gpt/non-fiction")
        , ("gpt/nonfiction" , "ai/nn/transformer/gpt/non-fiction")
        , ("gpt/poetry" , "ai/nn/transformer/gpt/poetry")
        , ("graph" , "design/visualization")
        , ("hanabi"
          , "reinforcement-learning/imperfect-information/hanabi"
          )
        , ("hardware" , "cs/hardware")
        , ("haskell" , "cs/haskell")
        , ("heritable" , "genetics/heritable")
        , ("heritable/correlation" , "genetics/heritable/correlation")
        , ("hierarchical" , "ai/nn/transformer/attention/hierarchical")
        , ("highleyman" , "ai/highleyman")
        , ("human" , "genetics/selection/natural/human")
        , ("humor" , "fiction/humor")
        , ("illusion-of-depth"
          , "psychology/cognitive-bias/illusion-of-depth"
          )
        , ("imperfect-information"
          , "reinforcement-learning/imperfect-information"
          )
        , ("inner-monologue" , "ai/nn/transformer/gpt/inner-monologue")
        , ("instruction-tuning"
          , "ai/nn/transformer/gpt/instruction-tuning"
          )
        , ("japan/anime" , "anime")
        , ("japanese" , "japan")
        , ("jukebox" , "ai/nn/transformer/gpt/jukebox")
        , ("knowledge-distillation"
          , "ai/nn/sparsity/knowledge-distillation"
          )
        , ("lamda" , "ai/nn/transformer/gpt/lamda")
        , ("learning"
          , "reinforcement-learning"
          )
        , ("less" , "lesswrong-survey")
        , ("link-rot" , "cs/linkrot")
        , ("linkrot" , "cs/linkrot")
        , ("linkrot/archiving" , "cs/linkrot/archiving")
        , ("lisp" , "cs/lisp")
        , ("lithium" , "psychiatry/lithium")
        , ("logic" , "philosophy/logic")
        , ("low-precision" , "ai/nn/sparsity/low-precision")
        , ("mae" , "ai/nn/vae/mae")
        , ("meditation" , "psychiatry/meditation")
        , ("mendelian-randomization"
          , "genetics/heritable/correlation/mendelian-randomization"
          )
        , ("meta-analysis" , "statistics/meta-analysis")
        , ("meta-learning" , "reinforcement-learning/meta-learning")
        , ("microbiome" , "genetics/microbiome")
        , ("mind" , "philosophy/mind")
        , ("mixture" , "ai/scaling/mixture-of-experts")
        , ("mixture-of-experts" , "ai/scaling/mixture-of-experts")
        , ("model" , "reinforcement-learning/model")
        , ("model-free" , "reinforcement-learning/model-free")
        , ("moe" , "ai/scaling/mixture-of-experts")
        , ("multi-agent" , "reinforcement-learning/multi-agent")
        , ("music-distraction" , "music/music-distraction")
        , ("muzero" , "reinforcement-learning/model/muzero")
        , ("natural" , "genetics/selection/natural")
        , ("nature" , "psychology/nature")
        , ("n-back" , "dual-n-back")
        , ("nethack" , "reinforcement-learning/nethack")
        , ("neuroscience" , "psychology/neuroscience")
        , ("new" , "newest")
        , ("nn" , "ai/nn")
        , ("non-fiction" , "ai/nn/transformer/gpt/non-fiction")
        , ("novelty" , "psychology/novelty")
        , ("oa5" , "reinforcement-learning/model-free/oa5")
        , ("ontology" , "philosophy/ontology")
        , ("opera" , "fiction/opera")
        , ("order" , "statistics/order")
        , ("palm" , "ai/nn/transformer/gpt/palm")
        , ("peer-review" , "statistics/peer-review")
        , ("perpetuities" , "economics/perpetuities")
        , ("personality" , "psychology/personality")
        , ("personality/conscientiousness"
          , "psychology/personality/conscientiousness"
          )
        , ("poetry" , "fiction/poetry")
        , ("portia" , "biology/portia")
        , ("power" , "statistics/power-analysis")
        , ("power-analysis" , "statistics/power-analysis")
        , ("prediction" , "statistics/prediction")
        , ("prediction/election" , "statistics/prediction/election")
        , ("preference-falsification"
          , "sociology/preference-falsification"
          )
        , ("preference-learning"
          , "reinforcement-learning/preference-learning"
          )
        , ("probability" , "statistics/probability")
        , ("pruning" , "ai/nn/sparsity/pruning")
        , ("psycholog" , "psychology/animal/bird")
        , ("psychology/bird" , "psychology/animal/bird")
        , ("psychopath" , "psychology/personality/psychopathy")
        , ("public-domain-review" , "history/public-domain-review")
        , ("python" , "cs/python")
        , ("quantified-self" , "nootropic/quantified-self")
        , ("r" , "cs/r")
        , ("red" , "design/typography/rubrication")
        , ("reduced-precision" , "ai/nn/sparsity/low-precision")
        , ("reinforcement-learning/alphago"
          , "reinforcement-learning/model/alphago"
          )
        , ("religion" , "philosophy/religion")
        , ("repetition" , "psychology/spaced-repetition")
        , ("retrieval" , "ai/nn/retrieval")
        , ("review" , "history/public-domain-review")
        , ("risk" , "existential-risk")
        , ("rl-scaling" , "reinforcement-learning/scaling")
        , ("rl/scaling" , "reinforcement-learning/scaling")
        , ("rnn" , "ai/nn/rnn")
        , ("robot" , "reinforcement-learning/robot")
        , ("robotics" , "reinforcement-learning/robot")
        , ("rosenhan" , "psychiatry/schizophrenia/rosenhan")
        , ("rubrication" , "design/typography/rubrication")
        , ("rumiyah" , "crime/terrorism/rumiyah")
        , ("safe" , "reinforcement-learning/safe")
        , ("samples" , "ai/nn/transformer/clip/sample")
        , ("sampling" , "ai/nn/sampling")
        , ("scaling" , "ai/scaling")
        , ("scaling/economics" , "ai/scaling/economics")
        , ("scaling/hardware" , "ai/scaling/hardware")
        , ("schizophrenia" , "psychiatry/schizophrenia")
        , ("science-fiction" , "fiction/science-fiction")
        , ("security" , "cs/security")
        , ("selection" , "genetics/selection")
        , ("selection/artificial" , "genetics/selection/artificial")
        , ("selection/natural" , "genetics/selection/natural")
        , ("self-sinking" , "technology/self-sinking")
        , ("semaglutide" , "longevity/semaglutide")
        , ("sentence-spacing" , "design/typography/sentence-spacing")
        , ("sequencing" , "genetics/sequencing")
        , ("sf" , "fiction/science-fiction")
        , ("short-sleeper" , "zeo/short-sleeper")
        , ("silk-road" , "darknet-market/silk-road")
        , ("silk-road/1" , "darknet-market/silk-road/1")
        , ("silk-road/2" , "darknet-market/silk-road/2")
        , ("sleep" , "zeo")
        , ("smell" , "psychology/smell")
        , ("sort" , "cs/algorithm/sorting")
        , ("sorting" , "cs/algorithm/sorting")
        , ("spaced-repetition" , "psychology/spaced-repetition")
        , ("sparsity" , "ai/nn/sparsity")
        , ("sparsity/pruning" , "ai/nn/sparsity/pruning")
        , ("stereotype-threat"
          , "psychology/cognitive-bias/stereotype-threat"
          )
        , ("stylegan" , "ai/nn/gan/stylegan")
        , ("stylometrics" , "statistics/stylometry")
        , ("stylometry" , "statistics/stylometry")
        , ("sunk-cost" , "psychology/cognitive-bias/sunk-cost")
        , ("survival" , "statistics/survival-analysis")
        , ("survival-analysis" , "statistics/survival-analysis")
        , ("t5" , "ai/nn/transformer/t5")
        , ("tabular" , "ai/tabular")
        , ("tbi" , "psychiatry/traumatic-brain-injury")
        , ("tcs" , "psychology/neuroscience/tcs")
        , ("teika" , "japan/poetry/teika")
        , ("terrorism" , "crime/terrorism")
        , ("text" , "fiction/text-game")
        , ("text-game" , "fiction/text-game")
        , ("text-style-transfer" , "ai/text-style-transfer")
        , ("tirzepatide" , "longevity/tirzepatide")
        , ("tokenization" , "ai/nn/tokenization")
        , ("traction" , "music/music-distraction")
        , ("transformer" , "ai/nn/transformer")
        , ("transformer/attention" , "ai/nn/transformer/attention")
        , ("transformer/gpt" , "ai/nn/transformer/gpt")
        , ("traumatic-brain-injury"
          , "psychiatry/traumatic-brain-injury"
          )
        , ("typography" , "design/typography")
        , ("uighur" , "history/uighur")
        , ("vae" , "ai/nn/vae")
        , ("video/analysis" , "ai/video/analysis")
        , ("video/generatio" , "ai/video/generation")
        , ("video/generation" , "ai/video/generation")
        , ("vision" , "psychology/vision")
        , ("visual" , "design/visualization")
        , ("visualization" , "design/visualization")
        , ("willpower" , "psychology/willpower")
        , ("writing" , "psychology/writing")
        , ("psych/inner-monologue", "psychology/inner-monologue")
        ]
