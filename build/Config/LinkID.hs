{-# LANGUAGE OverloadedStrings #-}
module Config.LinkID where

import qualified Data.Text as T (Text)

-- testing: unique list
affiliationAnchors :: [String]
affiliationAnchors = ["ai21", "adobe", "alibaba", "allen", "amazon", "anthropic", "apple", "baai", "baidu", "bair", "bytedance"
                     , "cerebras", "cohere", "deepmind", "eleutherai", "elementai", "facebook", "flickr", "github", "google"
                     , "google-graphcore", "googledeepmind", "graphcore", "huawei", "huggingface", "ibm", "intel", "jd", "kakao"
                     , "laion", "lighton", "microsoft", "microsoftnvidia", "miri", "naver", "nvidia", "openai", "pinterest", "pdf"
                     , "salesforce", "samsung", "sberbank", "schmidhuber", "sensetime", "snapchat", "sony", "spotify", "tencent"
                     , "tensorfork", "twitter", "uber", "yandex"]

-- testing: unique keys & unique values; keys are URIs, values are not URIs (to guard against swaps) & start with letters with no periods
linkIDOverrides :: [(String, T.Text)]
linkIDOverrides =
  [
    ("https://en.wikipedia.org/wiki/Donkey", "wp-donkey")
  , ("https://en.wikipedia.org/wiki/Przewalski%27s_horse", "wp-przewalskishorse")
  , ("/gpt-2-music", "gwern-presser-2019-music")
  ]
