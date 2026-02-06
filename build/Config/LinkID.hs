{-# LANGUAGE OverloadedStrings #-}
module Config.LinkID where

import qualified Data.Text as T (Text)

import Utils (setLike)

-- testing: unique list
affiliationAnchors :: [String]
affiliationAnchors = setLike ["ai21", "adobe", "alibaba", "allen", "amazon", "anthropic", "apple", "baai", "baidu", "bair", "bytedance"
                     , "cerebras", "cohere", "deepmind", "deepseek", "eleutherai", "elementai", "facebook", "flickr", "github", "google"
                     , "google-graphcore", "googledeepmind", "graphcore", "huawei", "huggingface", "ibm", "intel", "jd", "kakao"
                     , "laion", "lighton", "microsoft", "microsoftnvidia", "miri", "moonshot", "naver", "nvidia", "openai", "pinterest", "pdf"
                     , "salesforce", "samsung", "sberbank", "schmidhuber", "sensetime", "snapchat", "sony", "spotify", "tencent"
                     , "tensorfork", "twitter", "uber", "yandex"]

-- testing: unique keys & unique values; keys are URIs, values are not URIs (to guard against swaps) & start with letters with no periods
linkIDOverrides :: [(String, T.Text)]
linkIDOverrides = setLike
  [
  ("/gpt-2-music", "gwern-presser-2019-music")
  , ("/blog/index", "gwern-2009-blog-index")
  , ("/fiction/your-hands-and-mine", "gwern-2025-hands")
  , ("/fiction/silver-bird", "gwern-2025-silver-bird")
  , ("/fiction/lab-animals", "gwern-2025-apollonian-1")
  , ("/fiction/tilakkhana", "gwern-2025-tilakkhana")
  , ("/fiction/this-last-pain-graveyard", "gwern-2025-this-last-pain-graveyard")
  , ("/fiction/this-last-pain", "gwern-2025-this-last-pain")
  , ("/system-prompts-2025", "gwern-2025-system-prompts")
  , ("/problem-14", "gwern-et-al-2022-problem-14")
  ]
