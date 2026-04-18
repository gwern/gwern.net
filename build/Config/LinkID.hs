{-# LANGUAGE OverloadedStrings #-}
module Config.LinkID where

import qualified Data.Text as T (Text)

import Utils (setLike)

-- Testing: unique list
affiliationAnchors :: [String]
affiliationAnchors = setLike ["ai21", "adobe", "alibaba", "allen", "amazon", "anthropic", "apple", "baai", "baidu", "bair", "bytedance"
                     , "cerebras", "cohere", "deepmind", "deepseek", "eleutherai", "elementai", "facebook", "flickr", "github", "google"
                     , "google-graphcore", "googledeepmind", "graphcore", "huawei", "huggingface", "ibm", "intel", "jd", "kakao"
                     , "laion", "lighton", "microsoft", "microsoftnvidia", "miri", "moonshot", "naver", "nvidia", "openai", "pinterest", "pdf"
                     , "salesforce", "samsung", "sberbank", "schmidhuber", "sensetime", "snapchat", "sony", "spotify", "tencent"
                     , "tensorfork", "twitter", "uber", "yandex"]

-- Testing: unique keys & unique values; keys are URIs, values are not URIs (to guard against swaps) & start with letters with no periods
linkIDOverrides :: [(String, T.Text)]
linkIDOverrides = setLike
  [
  ]






