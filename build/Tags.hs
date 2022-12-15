{-# LANGUAGE OverloadedStrings #-}
module Tags where

import Control.Monad (filterM)
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
import Utils (anyInfix, replace, sed, sedMany, trim, split, replaceMany, frequency, pairs)

-- Remind to refine link tags: should be <100. (We count using the annotation database instead of counting files inside each directory because so many are now cross-tagged or virtual.)
tagMax, tagPairMax :: Int
tagMax = 100
tagPairMax = 11
tagCount :: Metadata -> [(Int,String)]
tagCount = frequency . concatMap (\(_,(_,_,_,_,tags,_)) -> tags) . M.toList
tagPairsCount :: Metadata -> [(Int,(String,String))]
tagPairsCount md = reverse $ frequency $ concatMap pairs $ M.elems $ M.map (\(_,_,_,_,ts,abst) -> if null abst || null ts then [] else ts) md

-- Compile tags down into a Span containing a list of links to the respective /docs/ directory indexes which will contain a copy of all annotations corresponding to that tag/directory.
--
-- Simple version:
-- > tagsToLinksSpan "economics, genetics/heritable, psychology/writing"
-- →
-- Span ("",["link-tags"],[])
--   [Link ("",["link-tag"],[]) [Str "economics"] ("/docs/economics/index",""),Str ", ",
--     Link ("",["link-tag"],[]) [Str "genetics/heritable"] ("/docs/genetics/heritable/index",""),Str ", ",
--     Link ("",["link-tag"],[]) [Str "psychology/writing"] ("/docs/psychology/writing/index","")
--   ]
-- Markdown:
-- →
-- [[economics](/docs/economics/index){.link-tag}, [genetics/heritable](/docs/genetics/heritable/index){.link-tag}, [psychology/writing](/docs/psychology/writing/index){.link-tag}]{.link-tags}
-- HTML:
-- →
-- <span class="link-tags">
--   <a href="/docs/economics/index" class="link-tag">economics</a>,
--   <a href="/docs/genetics/heritable/index" class="link-tag">genetics/heritable</a>,
--   <a href="/docs/psychology/writing/index" class="link-tag">psychology/writing</a>
-- </span>
tagsToLinksSpan :: [T.Text] -> Inline
tagsToLinksSpan [] = Span nullAttr []
tagsToLinksSpan [""] = Span nullAttr []
tagsToLinksSpan ts = let tags = sort ts in
                       Span ("", ["link-tags"], []) $
                       intersperse (Str ", ") $ map (\tag -> Link ("", ["link-tag", "link-page", "link-annotated", "icon-not"], [("rel","tag")]) [RawInline (Format "html") $ abbreviateTag tag] ("/docs/"`T.append`tag`T.append`"/index", "Link to "`T.append`tag`T.append`" tag index") ) tags

-- Ditto; but since a Div is a Block element, we copy-paste a separate function:
tagsToLinksDiv :: [T.Text] -> Block
tagsToLinksDiv [] = Div nullAttr []
tagsToLinksDiv [""] = Div nullAttr []
tagsToLinksDiv ts = let tags = sort ts in
                       Div ("", ["link-tags"], []) $
                       [Para $ intersperse (Str ", ") $ map (\tag -> Link ("", ["link-tag", "link-page", "link-annotated"], [("rel","tag")]) [RawInline (Format "html") $ abbreviateTag tag] ("/docs/"`T.append`tag`T.append`"/index", "Link to "`T.append`tag`T.append`" tag index") ) tags]

-- if a local '/docs/*' file and no tags available, try extracting a tag from the path; eg. '/docs/ai/2021-santospata.pdf' → 'ai', '/docs/ai/anime/2021-golyadkin.pdf' → 'ai/anime' etc; tags must be lowercase to map onto directory paths, but we accept uppercase variants (it's nicer to write 'economics, sociology, Japanese' than 'economics, sociology, japanese')
tag2TagsWithDefault :: String -> String -> [String]
tag2TagsWithDefault path tags = let tags' = map trim $ split ", " $ map toLower tags
                                    defTag = if ("/docs/" `isPrefixOf` path) && (not ("/docs/biology/2000-iapac-norvir"`isPrefixOf`path || "/docs/rotten.com/"`isPrefixOf`path || "/docs/statistics/order/beanmachine-multistage"`isPrefixOf`path||"/docs/www/"`isPrefixOf`path)) then tag2Default path else ""
                                in
                                  if defTag `elem` tags' || defTag == "" || defTag == "/docs" then tags' else defTag:tags'

tag2Default :: String -> String
tag2Default path = if "/docs/" `isPrefixOf` path && not ("/docs/" `isPrefixOf` path && ("/index" `isSuffixOf` path || "/index#" `isInfixOf` path)) then replace "/docs/" "" $ takeDirectory path else ""

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
abbreviateTag = T.pack . sedMany tagRewritesRegexes . replaceMany tagsLong2Short . replace "/docs/" "" . T.unpack
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
                             , ("^tcs$", "TDCS")
                             , ("^gan$", "GAN")
                             , ("^psychology/", "psych/")
                             , ("^technology/", "tech/")
                             ]

listTagsAll :: IO [String]
listTagsAll = fmap (map (replace "docs/" "") . sort . filter (\f' -> not $ anyInfix f' ["personal/2011-gwern-yourmorals.org", "rotten.com", "2000-iapac-norvir", "beanmachine-multistage", "docs/www/"]) ) $ getDirFiltered (\f -> doesFileExist (f++"/index.page")) "docs/"

-- given a list of ["docs/foo/index.page"] directories, convert them to what will be the final absolute path ("/docs/foo/index"), while checking they exist (typos are easy, eg. dropping 'docs/' is common).
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
guessTagFromShort m t = let allTags = nubOrd $ sort m in
  if t `elem` allTags then t else -- exact match, no guessing required
   case lookup t tagsShort2Long of
     Just tl -> tl -- is an existing short/petname
     Nothing -> let shortFallbacks =
                      (map (\a->(a,"")) $ filter (\tag -> ("/"++t) `isSuffixOf` tag || (t++"/") `isInfixOf` tag) allTags) ++ -- look for matches by path segment eg. 'transformer' → 'ai/nn/transformer' (but not 'ai/nn/transformer/alphafold' or 'ai/nn/transformer/gpt')
                      filter (\(short,_) -> t `isSuffixOf` short) tagsShort2Long ++
                      filter (\(short,_) -> t `isPrefixOf` short) tagsShort2Long ++
                      filter (\(short,_) -> t `isInfixOf` short) tagsShort2Long
                in if not (null shortFallbacks) then fst $ head shortFallbacks else
                     let longFallbacks = filter (t `isSuffixOf`) allTags ++ filter (t `isPrefixOf`) allTags ++ filter (t `isInfixOf`) allTags in
                       if not (null longFallbacks) then head longFallbacks else t

-- intended for use with full literal fixed-string matches, not regexps/infix/suffix/prefix matches.
tagsLong2Short, tagsShort2Long :: [(String,String)]
tagsShort2Long = [("statistics/power", "statistics/power-analysis"), ("reinforcement-learning/robotics", "reinforcement-learning/robot"), ("reinforcement-learning/robotic", "reinforcement-learning/robot"), ("dog/genetics", "genetics/heritable/dog"), ("dog/cloning", "genetics/cloning/dog"), ("genetics/selection/artificial/apple-breeding","genetics/selection/artificial/apple"), ("T5", "ai/nn/transformer/t5"), ("link-rot", "cs/linkrot"), ("linkrot", "cs/linkrot"), ("ai/clip", "ai/nn/transformer/clip"), ("clip/samples", "ai/nn/transformer/clip/samples"), ("japanese", "japan"), ("quantised", "ai/nn/sparsity/low-precision"), ("quantized", "ai/nn/sparsity/low-precision"), ("quantization", "ai/nn/sparsity/low-precision") , ("reduced-precision", "ai/nn/sparsity/low-precision"), ("mixed-precision", "ai/nn/sparsity/low-precision"), ("evolution", "genetics/selection/natural"), ("gpt-3", "ai/nn/transformer/gpt"), ("gpt3", "ai/nn/transformer/gpt"), ("gpt/nonfiction", "ai/nn/transformer/gpt/non-fiction"), ("red", "design/typography/rubrication"), ("self-attention", "ai/nn/transformer/attention"), ("efficient-attention", "ai/nn/transformer/attention"), ("ai/rnn", "ai/nn/rnn"), ("ai/retrieval", "ai/nn/retrieval"), ("mr", "genetics/heritable/correlation/mendelian-randomization"), ("japan/anime", "anime"), ("psychology/bird", "psychology/animal/bird"), ("psychology/birds/neuroscience", "psychology/animal/bird/neuroscience"), ("psychology/birds", "psychology/animal/bird"), ("dalle","dall-e"), ("dall-e", "ai/nn/transformer/gpt/dall-e"), ("silk-road-1", "darknet-markets/silk-road/1"), ("sr1", "darknet-markets/silk-road/1"), ("silk-road-2", "darknet-markets/silk-road/2"), ("sr2", "darknet-markets/silk-road/2"), ("psychology/neuroscience/bird", "psychology/animal/bird/neuroscience"), ("uighurs", "history/uighur"), ("ai/adversarial", "ai/nn/adversarial"), ("add", "psychiatry/adhd"), ("asperger", "psychiatry/autism"), ("aspergers", "psychiatry/autism"), ("personality/conscientiousness", "psychology/personality/conscientiousness"), ("conscientiousness", "psychology/personality/conscientiousness"), ("anorexia-nervosa", "psychiatry/anorexia"), ("anxiety-disorder", "psychiatry/anxiety"), ("masked-auto-encoder", "ai/nn/vae/mae"), ("masked-autoencoder", "ai/nn/vae/mae"), ("masked", "ai/nn/vae/mae"), ("alzheimer's", "psychiatry/alzheimers"), ("ad", "psychiatry/alzheimers"), ("alzheimers-disease", "psychiatry/alzheimers"), ("alzheimer", "psychiatry/alzheimers"), ("psychedelics", "psychedelic"), ("stylometric", "statistics/stylometry"), ("stylometrics", "statistics/stylometry"), ("dune", "fiction/science-fiction/frank-herbert"), ("herbert", "fiction/science-fiction/frank-herbert"), ("instruct-tuning", "instruction-tuning"), ("instruction-finetuning", "instruction-tuning")] ++
                 -- ^ custom tag shortcuts, to fix typos etc
                 -- attempt to infer short->long rewrites from the displayed tag names, which are long->short; but note that many of them are inherently invalid and the mapping only goes one way.
                  (map (\(a,b) -> (map toLower b,a)) $ filter (\(_,fancy) -> not (anyInfix fancy [" ", "<", ">", "(",")"])) tagsLong2Short)
tagsLong2Short = [
          ("reinforcement-learning", "RL")
          , ("music/music-distraction", "music distraction")
          , ("reinforcement-learning/chess", "AI chess")
          , ("ai/dataset", "ML dataset")
          , ("ai/tabular", "tabular ML")
          , ("ai/nn/adversarial", "adversarial examples")
          , ("ai/anime", "anime AI")
          , ("ai/anime/danbooru", "Danbooru AI")
          , ("anime/eva/little-boy", "<em>Little Boy</em>")
          , ("ai/nn", "neural net")
          , ("ai/nn/cnn", "CNN")
          , ("ai/nn/rnn", "RNN")
          , ("ai/nn/fully-connected", "MLP NN")
          , ("ai/nn/transformer", "Transformer NN")
          , ("ai/nn/vae", "autoencoder NN")
          , ("ai/nn/vae/mae", "masked auto-encoder")
          , ("ai/scaling", "AI scaling")
          , ("ai/scaling/moe", "MoE NN")
          , ("ai/nn/tokenization", "LM tokenization")
          , ("ai/nn/retrieval", "retrieval AI")
          , ("iq/animal", "animal cognition")
          , ("iq/ses", "IQ/SES")
          , ("iq/high/smpy", "SMPY")
          , ("iq/high/munich", "Munich Giftedness Study")
          , ("iq/high/fullerton", "Fullerton Longitudinal Study")
          , ("iq/high/anne-roe", "Anne Roe's Scientists")
          , ("ai/nn/transformer/clip", "CLIP")
          , ("ai/nn/transformer/clip/samples", "CLIP samples")
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
          , ("tominaga-nakamoto", "Tominaga Nakamoto")
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
          , ("philosophy/frank-p-ramsey", "Frank Ramsey")
          , ("nootropic/quantified-self", "QS")
          , ("darknet-markets", "DNM")
          , ("darknet-markets/dnm-archives", "DNM Archives")
          , ("darknet-markets/agora", "Agora DNM")
          , ("darknet-markets/alphabay", "AlphaBay DNM")
          , ("darknet-markets/atlantis", "Atlantis DNM")
          , ("darknet-markets/blackmarket-reloaded", "BMR DNM")
          , ("darknet-markets/evolution", "Evolution DNM")
          , ("darknet-markets/sheep-marketplace", "Sheep DNM")
          , ("darknet-markets/silk-road", "SR DNMs")
          , ("darknet-markets/silk-road/1", "SR1 DNM")
          , ("darknet-markets/silk-road/2", "SR2 DNM")
          , ("darknet-markets/william-pickard", "William Pickard")
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
          , ("statistics/prediction/election", "election forecast")
          , ("statistics/prediction", "forecasting")
          , ("psychology/cognitive-bias", "cognitive biases")
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
          , ("psychiatry/bipolar", "BPD")
          , ("psychiatry/depression", "MDD")
          , ("psychiatry/meditation", "meditation")
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
          , ("genetics/heritable/rare-variants", "rare genes")
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
          , ("fiction/criticism", "literary criticism")
          , ("fiction/humor", "humor")
          , ("fiction/text-game", "text game")
          , ("fiction/gene-wolfe", "Gene Wolfe")
          , ("fiction/science-fiction/frank-herbert", "<em>Dune</em>")
          , ("cat/valerian", "Valerian (cats)")
          , ("cat/tartarian-honeysuckle", "Tartarian honeysuckle (cats)")
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
          , ("sunk-cost", "sunk cost")
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
          ]
