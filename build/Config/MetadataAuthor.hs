{-# LANGUAGE OverloadedStrings #-}

module Config.MetadataAuthor where

-- import qualified Data.Text as T (Text)
import Text.Pandoc (Inline(Span, Space, Str))

-- config testing: all unique
authorCollapseTestCases :: [(String, [Inline])]
authorCollapseTestCases =
  [ ("a", [Space,Span ("",["author","cite-author"],[]) [Str "a"]])
  , ("a, b", [Space,Span ("",["author","cite-author-plural"],[("title","a, b")]) [Str "a",Str ", ",Str "b"]])
  , ("a, b, c", [Space,Span ("",["author","cite-author-plural"],[("title","a, b, c")]) [Str "a",Str ", ",Str "b",Str ", ",Str "c"]])
  , ( "a, b, c, d", [Space,Span ("",["author","cite-author-plural"],[("title","a, b, c, d")]) [Str "a",Str ", ",Str "b",Str ", ",Str "c",Str ", ",Str "d"]])
  , ("a, b, c, d, e", [Space,Span ("",["author","collapse"],[]) [Span ("",["abstract-collapse","cite-author-plural"],[("title","a, b, c, d, e")]) [Str "a",Str ", ",Str "b",Str ", ",Str "c"],Span ("",[],[]) [Str ", ",Str "d",Str ", ",Str "e"]]])
  , ( "a, b, c, d, e, f", [Space,Span ("",["author","collapse"],[]) [Span ("",["abstract-collapse","cite-author-plural"],[("title","a, b, c, d, e, f")]) [Str "a",Str ", ",Str "b",Str ", ",Str "c"],Span ("",[],[]) [Str ", ",Str "d",Str ", ",Str "e",Str ", ",Str "f"]]])
  , ( "a, b, c, d, e, f, g", [Space,Span ("",["author","collapse"],[]) [Span ("",["abstract-collapse","cite-author-plural"],[("title","a, b, c, d, e, f, g")]) [Str "a",Str ", ",Str "b",Str ", ",Str "c"],Span ("",[],[]) [Str ", ",Str "d",Str ", ",Str "e",Str ", ",Str "f",Str ", ",Str "g"]]])
    ]

-- list of rewrites for 'alternative name' → 'canonical name'
-- Config tests: unique values, no loops
canonicals :: [(String, String)]
canonicals = map (\(a,b) -> (b,a))
  -- written down in reverse order to enable lexicographic sorting by canonical name:
  [
    ("Eliezer Yudkowsky", "ESYudkowsky")
  , ("Eliezer Yudkowsky", "Yudkowsky")
  , ("Eliezer Yudkowsky", "E. Yudkowsky")
  ]
