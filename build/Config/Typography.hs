{-# LANGUAGE OverloadedStrings #-}

module Config.Typography where

import Text.Pandoc (Inline(Str, Subscript, Superscript, Span))

-- how many 1–n we want to generate <hr> versions of, to let us cycle ruler stylings instead of using a single ruler appearance everywhere.
cycleCount :: Int
cycleCount = 3

-- testing: unique keys
titleCaseTestCases :: [(String, String)]
titleCaseTestCases = [
  ("‘Two Truths and a Lie’ As a Class-participation Activity", "‘Two Truths and a Lie’ As a Class-Participation Activity")
            , ("end-to-end testing", "End-To-End Testing")
            , ("mother-in-law", "Mother-In-Law")
            , ("state-of-the-art technology", "State-Of-The-Art Technology")
            , ("x-ray", "X-Ray")
            , ("e-commerce", "E-Commerce")
            , ("co-worker", "Co-Worker")
            , ("self-esteem", "Self-Esteem")
            , ("long-term plan", "Long-Term Plan")
            , ("high-quality product", "High-Quality Product")
            , ("no hyphen here", "No Hyphen Here")
            , ("double--hyphen", "Double--Hyphen")
            , ("--leading hyphen", "--Leading Hyphen")
            , ("trailing hyphen--", "Trailing Hyphen--")
            , ("hyphen-at-both-ends-", "Hyphen-At-Both-Ends-")
            , ("", "")
            , ("-", "-")
            , ("a-b-c-d-e-f", "A-B-C-D-E-F")
            , ("first-class mail", "First-Class Mail")
            , ("well-being", "Well-Being")
            , ("123-456", "123-456")
            , ("abc-def-123", "Abc-Def-123")
            , ("-start with hyphen", "-Start With Hyphen")
            , ("end with hyphen-", "End With Hyphen-")
            , ("hyphen--in--middle", "Hyphen--In--Middle")
            , ("test-case", "Test-Case")
            , ("test---case", "Test---Case")
            , ("test-Case", "Test-Case")
            , ("Test-case", "Test-Case")
            , ("TEST-CASE", "TEST-CASE")
            , ("West ‘has Not Recovered’", "West ‘Has Not Recovered’")
            , ("West 'has Not Recovered'", "West 'Has Not Recovered'")
            , ("West \"has Not Recovered\"", "West \"Has Not Recovered\"")
            , ("West “has Not Recovered”", "West “Has Not Recovered”")
            , ("Did I get Sam Altman fired from OpenAI?: Nathan\8217s red-teaming experience, noticing how the board was not aware of GPT-4 jailbreaks & had not even tried GPT-4 prior to its early release", "Did I Get Sam Altman Fired from OpenAI?: Nathan\8217s Red-Teaming Experience, Noticing How the Board Was Not Aware of GPT-4 Jailbreaks & Had Not Even Tried GPT-4 prior to Its Early Release")
            , ("Foo's bar", "Foo's Bar")
            , ("Foo’s bar", "Foo’s Bar")
            , ("Foo'Bar", "Foo'Bar")
            , ("Foo' bar", "Foo' Bar")
            , ("Foo's", "Foo's")
            , ("Foo's Bar's", "Foo's Bar's")
            , ("Foo'Bar's", "Foo'Bar's")
            , ("Foo'Bar's Baz", "Foo'Bar's Baz")
            , ("'foo bar'", "'Foo Bar'")
            , ("'foo bar's'", "'Foo Bar's'")
            , ("'foo's bar'", "'Foo's Bar'")
            , ("'foo's bar's'", "'Foo's Bar's'")
            , ("'foo'bar's'", "'Foo'Bar's'")
            , ("foo-bar's", "Foo-Bar's")
            , ("foo-bar's baz", "Foo-Bar's Baz")
            , ("foo-bar's-baz", "Foo-Bar's-Baz")
            , ("foo'bar-baz", "Foo'Bar-Baz")
            , ("foo-bar's baz-qux", "Foo-Bar's Baz-Qux")
            , ("foo'bar-baz'qux", "Foo'Bar-Baz'Qux")
            , ("foo'bar's-baz'qux", "Foo'Bar's-Baz'Qux")
            , ("foo'bar's baz'qux", "Foo'Bar's Baz'Qux")
            , ("foo'bar's-baz qux", "Foo'Bar's-Baz Qux")
            , ("Fading Hip-Hop Mogul—who’s Been Buffeted by Charges of Sexual Assault—to Salvage", "Fading Hip-Hop Mogul—Who’s Been Buffeted by Charges of Sexual Assault—To Salvage")
            , ("Scarlett Johansson says she is \"shocked, angered\" over new ChatGPT voice", "Scarlett Johansson Says She Is \"Shocked, Angered\" over New ChatGPT Voice")
            , ("'something is wrong'", "'Something Is Wrong'")
              ]

-- testing: unique list
surnameFalsePositivesWhiteList :: [String]
surnameFalsePositivesWhiteList = ["Et", "et", "Al", "al", "Accurate", "Aesthetics", "Africa", "After", "Alert", "America", "An", "Apr",
                                  "April", "At", "Atari", "Atlas", "August", "Aug", "Autumn", "Before", "British", "Challenge", "Chat",
                                  "Codex", "Cohort", "Commodore", "Competition", "Considered", "Copyright", "Counterfactual", "Crypto",
                                  "Daily", "Dear", "Dec", "December", "Diaries", "Differences", "Early", "Enterprise", "Esthetics", "Evolution",
                                  "Expo", "Fair", "Fall", "Fanime", "Fanimecon", "Feb", "February", "First", "For", "Friday", "Impacts",
                                  "Jan", "January", "Jul", "July", "June", "Last", "Late", "Library", "Making", "Mar", "March", "May",
                                  "Memoirs", "Monday", "Monthly", "Ms", "Nov", "November", "Oct", "October", "Original", "Otakon", "Our",
                                  "Ours", "Over", "Predicting", "Reviews", "Sample", "Saturday", "Sci", "Security", "Sep", "September",
                                  "Since", "Spring", "Standard", "Statistics", "Suisse", "Summer", "Sunday", "Surface", "Survey",
                                  "Syntheses", "Than", "The", "Things", "Throughout", "Thursday", "Tuesday", "Until", "Wednesday", "Weekly",
                                  "Winter", "Writing", "Year", "Yearly", "Zilch", "In", "Judging", "From", "Experiment",
                                  "Between", "Caseness", "Goodbye", "By", "Around", "Christmas", "One", "Wars", "Michigan", "Act", "Circa",
                                  "Horizon", "Study", "Alamos", "Dover", "War", "London", "York", "Paris", "Berlin", "Francisco", "As", "Why",
                                  "Indicators", "China", "China’s", "Books", "During", "Halloween", "Sweden", "Norway", "Finland", "Suicides",
                                  "Camps", "Tamala", "Math"]

-- testing: unique list
dateRangeDurationTestCases :: [(Int, Inline, Inline)]
dateRangeDurationTestCases = [
  (2024, Str "400 patients studied from 1984\8211\&1987, expanded to 500, also tested 1984\8211\&1987"
    , Span ("",[],[]) [Span ("",[],[]) [Str "400 patients studied from ",Span ("",["date-range"],[("title","The date range 1984\8211\&1987 lasted 3 years, ending 37 years ago.")]) [Str "1984",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "3"]],Str "1987",Subscript [Str "37ya"]],Str ", expanded to 500, also tested "],Span ("",["date-range"],[("title","The date range 1984\8211\&1987 lasted 3 years, ending 37 years ago.")]) [Str "1984",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "3"]],Str "1987",Subscript [Str "37ya"]]]
    )
  , (1987, Str "401 patients studied from 1984\8211\&1987, expanded to 500, also tested 1984\8211\&1987"
    , Span ("",[],[]) [Span ("",[],[]) [Str "401 patients studied from ",Span ("",["date-range"],[("title","The date range 1984\8211\&1987 lasted 3 years.")]) [Str "1984",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "3"]],Str "1987"],Str ", expanded to 500, also tested "],Span ("",["date-range"],[("title","The date range 1984\8211\&1987 lasted 3 years.")]) [Str "1984",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "3"]],Str "1987"]]
    )
  , (2000, Str "402 patients studied from 1984\8211\&1987, expanded to 500, also tested 1984\8211\&1987"
    , Span ("",[],[]) [Span ("",[],[]) [Str "402 patients studied from ",Span ("",["date-range"],[("title","The date range 1984\8211\&1987 lasted 3 years, ending 13 years ago.")]) [Str "1984",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "3"]],Str "1987",Subscript [Str "13ya"]],Str ", expanded to 500, also tested "],Span ("",["date-range"],[("title","The date range 1984\8211\&1987 lasted 3 years, ending 13 years ago.")]) [Str "1984",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "3"]],Str "1987",Subscript [Str "13ya"]]]
    )
  , (2010, Str "403 1984\8211\&1987"
    , Span ("",[],[]) [Str "403 ",Span ("",["date-range"],[("title","The date range 1984\8211\&1987 lasted 3 years, ending 23 years ago.")]) [Str "1984",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "3"]],Str "1987",Subscript [Str "23ya"]]]
    )
  , (1980, Str "404 1984\8211\&1986"
    , Span ("",[],[]) [Str "404 ",Span ("",["date-range"],[("title","The date range 1984\8211\&1986 lasted 2 years.")]) [Str "1984",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "2"]],Str "1986"]]

    )
  , (2025, Str "foo 2019-2024"
    , Span ("",[],[]) [Str "foo ",Span ("",["date-range"],[("title","The date range 2019\8211\&2024 lasted 5 years.")]) [Str "2019",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "5"]],Str "2024"]])
  , (2025, Str "foo 2019-2019"
    , Str "foo 2019-2019")
  , (2025, Str "foo 2019-2025"
    , Span ("",[],[]) [Str "foo ",Span ("",["date-range"],[("title","The date range 2019\8211\&2025 lasted 6 years.")]) [Str "2019",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "6"]],Str "2025"]])
  , (2025, Str "foo 2019-2563"
    , Str "foo 2019-2563")
  , (2025, Str "foo 2563-2560"
    , Str "foo 2563-2560")
  , (1980, Str "foo 1941-1962"
    , Span ("",[],[]) [Str "foo ",Span ("",["date-range"],[("title","The date range 1941\8211\&1962 lasted 21 years, ending 18 years ago.")]) [Str "1941",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "21"]],Str "1962",Subscript [Str "18ya"]]])
  , (2000,  Str "405 patients tested 1984-01-01--1987-01-01"
    , Span ("",[],[]) [Str "405 patients tested ",Span ("",["date-range"],[("title","The date range 1984-01-01\8211\&1987-01-01 lasted 3 years (1,097 days), ending 13 years ago.")]) [Str "1984-01-01",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "3y"]],Str "1987-01-01",Subscript [Str "13ya"]]]
    )
  , (2024, Str "1. WII was 1939--1945, or more specifically, 1939-09-01--1945-09-02."
    , Span ("",[],[]) [Str "1. WII was ",Span ("",["date-range"],[("title","The date range 1939\8211\&1945 lasted 6 years, ending 79 years ago.")]) [Str "1939",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "6"]],Str "1945",Subscript [Str "79ya"]],Span ("",[],[]) [Str ", or more specifically, ",Span ("",["date-range"],[("title","The date range 1939-09-01\8211\&1945-09-02 lasted 6 years (2,194 days), ending 79 years ago.")]) [Str "1939-09-01",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "6y"]],Str "1945-09-02",Subscript [Str "79ya"]],Str "."]]
    )
  , (2024, Str "2. WII was 1939-01-01--1939-06-06"
    , Span ("",[],[]) [Str "2. WII was ",Span ("",["date-range"],[("title","The date range 1939-01-01\8211\&1939-06-06 lasted (157 days), ending 85 years ago.")]) [Str "1939-01-01",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "5m"]],Str "1939-06-06",Subscript [Str "85ya"]]]
    )
  , (2024, Str "3. WII was 1939-01-01--1946-06-06"
    , Span ("",[],[]) [Str "3. WII was ",Span ("",["date-range"],[("title","The date range 1939-01-01\8211\&1946-06-06 lasted 7 years (2,714 days), ending 78 years ago.")]) [Str "1939-01-01",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "7y"]],Str "1946-06-06",Subscript [Str "78ya"]]]
    )
  , (2024, Span ("", ["date-range"], []) [Str "4. 1945-1946"]
    , Span ("",[],[]) [Str "4. ",Span ("",["date-range"],[("title","The date range 1945\8211\&1946 lasted 1 year, ending 78 years ago.")]) [Str "1945",Str "\8211",Str "1946",Subscript [Str "78ya"]]]
    )
  , (2024, Span ("", ["date-range"], []) [Str "5. 2001-01-01-2005-08-19"]
    , Span ("",[],[]) [Str "5. ",Span ("",["date-range"],[("title","The date range 2001-01-01\8211\&2005-08-19 lasted 4 years (1,692 days), ending 19 years ago.")]) [Str "2001-01-01",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "4y"]],Str "2005-08-19",Subscript [Str "19ya"]]]
    )
  , (2024, Str "The new program launched in 1980. 400 patients studied from 1984\8211\&1987, expanded to 500, also tested 1984-02-10\8211\&1987-11-30"
    , Span ("",[],[]) [Str "The new program launched in ",Span ("",["date-range"],[]) [Str "1980",Subscript [Str "44ya"]],Span ("",[],[]) [Str ". 400 patients studied from ",Span ("",["date-range"],[("title","The date range 1984\8211\&1987 lasted 3 years, ending 37 years ago.")]) [Str "1984",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "3"]],Str "1987",Subscript [Str "37ya"]],Span ("",[],[]) [Str ", expanded to 500, also tested ",Span ("",["date-range"],[("title","The date range 1984-02-10\8211\&1987-11-30 lasted 3 years (1,390 days), ending 37 years ago.")]) [Str "1984-02-10",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "3y"]],Str "1987-11-30",Subscript [Str "37ya"]]]]]
    )
  , (2024, Str "Test 1,850. Bar 1,900-2,000"
    , Str "Test 1,850. Bar 1,900-2,000"
    )
  , (2024, Str "Test 2,850. Bar 1,900-2000"
    , Str "Test 2,850. Bar 1,900-2000"
    )
  , (2024, Str "Test 3,850. Bar 1900-2,000"
    , Str "Test 3,850. Bar 1900-2,000"
    )
  ]
