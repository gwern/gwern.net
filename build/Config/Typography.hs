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
            , ("Simpler doesn't mean what Tesla thinks it means", "Simpler Doesn't Mean What Tesla Thinks It Means")
            , ("Yes, But Can the Steam Engine Do This? [invention of sandwiches]", "Yes, But Can the Steam Engine Do This? [Invention of Sandwiches]")
            , ("a janken (rock-paper-scissors) playing robot", "A Janken (Rock-Paper-Scissors) Playing Robot")
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
                                  "Syntheses", "Than", "The", "Then", "Things", "Throughout", "Thursday", "Tuesday", "Until", "Wednesday", "Weekly",
                                  "Winter", "Writing", "Year", "Yearly", "Zilch", "In", "Judging", "From", "Experiment",
                                  "Between", "Caseness", "Goodbye", "By", "Around", "Christmas", "One", "Wars", "Michigan", "Act", "Circa",
                                  "Horizon", "Study", "Alamos", "Dover", "War", "London", "York", "Paris", "Berlin", "Francisco", "As", "Why",
                                  "Indicators", "China", "China’s", "Books", "During", "Halloween", "Sweden", "Norway", "Finland", "Suicides",
                                  "Camps", "Tamala", "Math", "States", "Uniqueness", "On", "LessWrong", "Lesswrong", "Chemistry", "Physics", "Literature", "Biology", "Medicine", "Peace", "Economics", "Japan", "Building", "Papers"]


minRange, minDuration, maxDateSecond, minDateFirst :: Int
minRange = 4
minDuration = 11
maxDateSecond = 2100 -- the latest serious AD year I see on Gwern.net currently seems to be '2561 AD', from Charles Stross’s "USENIX 2011 Keynote: Network Security in the Medium Term, 2061–2561 AD" talk. But dates past 2100 AD are too rare to care about, and much more likely to be an ordinary number
minDateFirst = 1501 -- too many ordinary numbers <1,500 which are not comma-separated

-- testing: unique list
dateRangeDurationTestCases :: [(Int, Inline, Inline)]
dateRangeDurationTestCases = [
  (2024, Str "(1) 400 patients studied from 1984\8211\&1987, expanded to 500, also tested 1984\8211\&1987"
    , Span ("",[],[]) [Span ("",[],[]) [Str "(1) 400 patients studied from ",Span ("",["date-range"],[("title","The date range 1984\8211\&1987 lasted 3 years, ending 37 years ago.")]) [Str "1984",Str "\8211",Str "1987",Subscript [Span ("",[],[("title","1984 was 37 years ago.")]) [Str "37ya"]]],Str ", expanded to 500, also tested "],Span ("",["date-range"],[("title","The date range 1984\8211\&1987 lasted 3 years, ending 37 years ago.")]) [Str "1984",Str "\8211",Str "1987",Subscript [Span ("",[],[("title","1984 was 37 years ago.")]) [Str "37ya"]]]]
    )
  , (1987, Str "(2) 401 patients studied from 1984\8211\&1987, expanded to 500, also tested 1984\8211\&1987"
    , Str "(2) 401 patients studied from 1984\8211\&1987, expanded to 500, also tested 1984\8211\&1987"
    )
  , (2000, Str "(3) 402 patients studied from 1984\8211\&1987, expanded to 500, also tested 1984\8211\&1987"
    , Span ("",[],[]) [Span ("",[],[]) [Str "(3) 402 patients studied from ",Span ("",["date-range"],[("title","The date range 1984\8211\&1987 lasted 3 years, ending 13 years ago.")]) [Str "1984",Str "\8211",Str "1987",Subscript [Span ("",[],[("title","1984 was 13 years ago.")]) [Str "13ya"]]],Str ", expanded to 500, also tested "],Span ("",["date-range"],[("title","The date range 1984\8211\&1987 lasted 3 years, ending 13 years ago.")]) [Str "1984",Str "\8211",Str "1987",Subscript [Span ("",[],[("title","1984 was 13 years ago.")]) [Str "13ya"]]]]
    )
  , (2010, Str "(4) 403 1984\8211\&1987"
    , Span ("",[],[]) [Str "(4) 403 ",Span ("",["date-range"],[("title","The date range 1984\8211\&1987 lasted 3 years, ending 23 years ago.")]) [Str "1984",Str "\8211",Str "1987",Subscript [Span ("",[],[("title","1984 was 23 years ago.")]) [Str "23ya"]]]]
    )
  , (1980, Str "(5) 404 1984\8211\&1986"
    , Str "(5) 404 1984\8211\&1986"
    )
  , (2025, Str "(6) foo 2019-2024"
    , Span ("",[],[]) [Str "(6) foo ",Span ("",["date-range"],[("title","The date range 2019\8211\&2024 lasted 5 years.")]) [Str "2019",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "5"]],Str "2024"]])
  , (2025, Str "(7) foo 2019-2019"
    , Str "(7) foo 2019-2019")
  , (2025, Str "(8) foo 2019-2025"
    , Span ("",[],[]) [Str "(8) foo ",Span ("",["date-range"],[("title","The date range 2019\8211\&2025 lasted 6 years.")]) [Str "2019",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "6"]],Str "2025"]])
  , (2025, Str "(9) foo 2019-2563"
    , Str "(9) foo 2019-2563")
  , (2025, Str "(10) foo 2563-2560"
    , Str "(10) foo 2563-2560")
  , (1980, Str "(11) foo 1941-1962"
    , Span ("",[],[]) [Str "(11) foo ",Span ("",["date-range"],[("title","The date range 1941\8211\&1962 lasted 21 years, ending 18 years ago.")]) [Str "1941",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "21"]],Str "1962",Subscript [Span ("",[],[("title","1941 was 18 years ago.")]) [Str "18ya"]]]]
    )
  , (2000,  Str "(12) 405 patients tested 1984-01-01--1987-01-01"
    , Span ("",[],[]) [Str "(12) 405 patients tested ",Span ("",["date-range"],[("title","The date range 1984-01-01\8211\&1987-01-01 lasted 3 years (1,097 days), ending 13 years ago.")]) [Str "1984-01-01",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "3y"]],Str "1987-01-01",Subscript [Span ("",[],[("title","1984-01-01 was 13 years ago.")]) [Str "13ya"]]]]
    )
  , (2024, Str "1. (13) WII was 1939--1945, or more specifically, 1939-09-01--1945-09-02."
    , Span ("",[],[]) [Str "1. (13) WII was ",Span ("",["date-range"],[("title","The date range 1939\8211\&1945 lasted 6 years, ending 79 years ago.")]) [Str "1939",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "6"]],Str "1945",Subscript [Span ("",[],[("title","1939 was 79 years ago.")]) [Str "79ya"]]],Span ("",[],[]) [Str ", or more specifically, ",Span ("",["date-range"],[("title","The date range 1939-09-01\8211\&1945-09-02 lasted 6 years (2,194 days), ending 79 years ago.")]) [Str "1939-09-01",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "6y"]],Str "1945-09-02",Subscript [Span ("",[],[("title","1939-09-01 was 79 years ago.")]) [Str "79ya"]]],Str "."]]
    )
  , (2024, Str "2. (14) WII was 1939-01-01--1939-06-06"
    , Span ("",[],[]) [Str "2. (14) WII was ",Span ("",["date-range"],[("title","The date range 1939-01-01\8211\&1939-06-06 lasted 157 days, ending 85 years ago.")]) [Str "1939-01-01",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "5m"]],Str "1939-06-06",Subscript [Span ("",[],[("title","1939-01-01 was 85 years ago.")]) [Str "85ya"]]]]
    )
  , (2024, Str "3. (15) WII was 1939-01-01--1946-06-06"
    , Span ("",[],[]) [Str "3. (15) WII was ",Span ("",["date-range"],[("title","The date range 1939-01-01\8211\&1946-06-06 lasted 7 years (2,714 days), ending 78 years ago.")]) [Str "1939-01-01",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "7y"]],Str "1946-06-06",Subscript [Span ("",[],[("title","1939-01-01 was 78 years ago.")]) [Str "78ya"]]]]
    )
  , (2024, Span ("", ["date-range"], []) [Str "(16) 4. 1945-1946"]
    , Span ("",[],[]) [Str "(16) 4. ",Span ("",["date-range"],[("title","The date range 1945\8211\&1946 lasted 1 year, ending 78 years ago.")]) [Str "1945",Str "\8211",Str "1946",Subscript [Span ("",[],[("title","1945 was 78 years ago.")]) [Str "78ya"]]]]
    )
  , (2024, Span ("", ["date-range"], []) [Str "(17) 5. 2001-01-01-2005-08-19"]
    , Span ("",[],[]) [Str "(17) 5. ",Span ("",["date-range"],[("title","The date range 2001-01-01\8211\&2005-08-19 lasted 4 years (1,692 days), ending 19 years ago.")]) [Str "2001-01-01",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "4y"]],Str "2005-08-19",Subscript [Span ("",[],[("title","2001-01-01 was 19 years ago.")]) [Str "19ya"]]]]
    )
  , (2024, Str "(18) The new program launched in 1980. 400 patients studied from 1984\8211\&1987, expanded to 500, also tested 1984-02-10\8211\&1987-11-30"
    , Span ("",[],[]) [Str "(18) The new program launched in ",Span ("",["date-range"],[]) [Str "1980",Subscript [Span ("",[],[("title","1980 was 44 years ago.")]) [Str "44ya"]]],Span ("",[],[]) [Str ". 400 patients studied from ",Span ("",["date-range"],[("title","The date range 1984\8211\&1987 lasted 3 years, ending 37 years ago.")]) [Str "1984",Str "\8211",Str "1987",Subscript [Span ("",[],[("title","1984 was 37 years ago.")]) [Str "37ya"]]],Span ("",[],[]) [Str ", expanded to 500, also tested ",Span ("",["date-range"],[("title","The date range 1984-02-10\8211\&1987-11-30 lasted 3 years (1,390 days), ending 37 years ago.")]) [Str "1984-02-10",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "3y"]],Str "1987-11-30",Subscript [Span ("",[],[("title","1984-02-10 was 37 years ago.")]) [Str "37ya"]]]]]]
    )
  , (2024, Str "(19) Test 1,850. Bar 1,900-2,000"
    , Str "(19) Test 1,850. Bar 1,900-2,000"
    )
  , (2024, Str "(20) Test 2,850. Bar 1,900-2000"
    , Str "(20) Test 2,850. Bar 1,900-2000"
    )
  , (2024, Str "(21) Test 3,850. Bar 1900-2,000"
    , Str "(21) Test 3,850. Bar 1900-2,000"
    )
  , (2024, Str "(22) Hirohito reigned 1926–1989."
    , Span ("",[],[]) [Str "(22) Hirohito reigned ",Span ("",["date-range"],[("title","The date range 1926\8211\&1989 lasted 63 years, ending 35 years ago.")]) [Str "1926",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "63"]],Str "1989",Subscript [Span ("",[],[("title","1926 was 35 years ago.")]) [Str "35ya"]]],Str "."]
    )
  , (2025, Str "(23) The first atomic bombing to surrender speech interval was 1945-08-06–1945-08-15."
    , Span ("",[],[]) [Str "(23) The first atomic bombing to surrender speech interval was ",Span ("",["date-range"],[("title","The date range 1945-08-06\8211\&1945-08-15 lasted 10 days, ending 80 years ago.")]) [Str "1945-08-06",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "10d"]],Str "1945-08-15",Subscript [Span ("",[],[("title","1945-08-06 was 80 years ago.")]) [Str "80ya"]]],Str "."]
    )
  , (2024, Str "(24) Hirohito reigned 1926-01–1989-05."
    , Span ("",[],[]) [Str "(24) Hirohito reigned ",Span ("",["date-range"],[("title","The date range 1926-01\8211\&1989-05 lasted 63 years (23,132 days), ending 35 years ago.")]) [Str "1926-01",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "63y"]],Str "1989-05",Subscript [Span ("",[],[("title","1926-01 was 35 years ago.")]) [Str "35ya"]]],Str "."]
    )
  , (2024, Str "(25) William Shakespeare lived 1564-04--1616-04, dying in the Jacobean era."
    , Span ("",[],[]) [Str "(25) William Shakespeare lived ",Span ("",["date-range"],[("title","The date range 1564-04-\8211\&1616-04 lasted 52 years (18,994 days), ending 408 years ago.")]) [Str "1564-04-",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "52y"]],Str "1616-04",Subscript [Span ("",[],[("title","1564-04- was 408 years ago.")]) [Str "408ya"]]],Str ", dying in the Jacobean era."]
    )
  ,(2024, Str "(26) a good book costs $1541 to print in the medieval era"
        , Str "(26) a good book costs $1541 to print in the medieval era"
        )
  , (2020, Str "(27) a good book costs $1543--$1601 to print in the medieval era"
    , Str "(27) a good book costs $1543--$1601 to print in the medieval era"
    )
  , ( 2019, Str "(28) a good book costs $1542--1600 to print in the medieval era"
    , Str "(28) a good book costs $1542--1600 to print in the medieval era"
    )
  , ( 2019, Str "(28.5) a good book costs $1,542--1,600 to print in the medieval era"
    , Str "(28.5) a good book costs $1,542--1,600 to print in the medieval era"
    )
  , (2024, Str "(29) It is now the 2000s. But as a date range, does that mean 2000, 2009, or maybe a mean date like 2004? And if that means any of those, what does 1900s mean?"
    , Span ("",[],[]) [Span ("",[],[]) [Span ("",[],[]) [Str "(29) It is now the 2000s. But as a date range, does that mean ",Span ("",["date-range"],[]) [Str "2000",Subscript [Span ("",[],[("title","2000 was 24 years ago.")]) [Str "24ya"]]],Str ", "],Span ("",["date-range"],[]) [Str "2009",Subscript [Span ("",[],[("title","2009 was 15 years ago.")]) [Str "15ya"]]],Str ", or maybe a mean date like "],Span ("",["date-range"],[]) [Str "2004",Subscript [Span ("",[],[("title","2004 was 20 years ago.")]) [Str "20ya"]]],Str "? And if that means any of those, what does 1900s mean?"]
    )
  , (2024, Str "(30) So, in 2020, the Leela team raced to train larger networks. She sourced compute from corporate donors and friends’ GTX 1070s."
    , Span ("",[],[]) [Str "(30) So, in ",Str "2020",Str ", the Leela team raced to train larger networks. She sourced compute from corporate donors and friends\8217 GTX 1070s."]
    )
  , (1996, Str "(31) he wrote a history of the 1920’s called"
    , Str "(31) he wrote a history of the 1920’s called"
    )
  , (1997, Str "(32) he wrote a history of the 1920s called"
    , Str "(32) he wrote a history of the 1920s called"
    )
  , (2025, Str "(33) Honors, #1029."
    , Str "(33) Honors, #1029."
    )
  , (2026, Str "(34) 1600px image"
    , Str "(34) 1600px image"
    )
  , (2027, Str "(35) Has not been published since 2001."
    , Span ("",[],[]) [Str "(35) Has not been published since ",Span ("",["date-range"],[]) [Str "2001",Subscript [Span ("",[],[("title","2001 was 26 years ago.")]) [Str "26ya"]]],Str "."]
    )
  , (2028, Str "We sampled ~1000 ZIP codes."
    , Str "We sampled ~1000 ZIP codes."
    )
  , (2029, Str "The malformed date range 1998–1998."
    , Span ("",[],[]) [Str "The malformed date range ",Span ("",["date-range"],[("title","The date range 1998\8211\&1998 lasted, ending 31 years ago.")]) [Str "1998",Str "\8211",Str "1998",Subscript [Span ("",[],[("title","1998 was 31 years ago.")]) [Str "31ya"]]],Str "."]
    )
  , (2030, Str "The malformed date range 1900-1900."
    , Span ("",[],[]) [Str "The malformed date range ",Span ("",["date-range"],[("title","The date range 1900\8211\&1900 lasted, ending 130 years ago.")]) [Str "1900",Str "\8211",Str "1900",Subscript [Span ("",[],[("title","1900 was 130 years ago.")]) [Str "130ya"]]],Str "."]
    )
  , (2031, Str "Say, 1000 lines of code"
    , Str "Say, 1000 lines of code"
    )
  , (2032, Str "The date-range 2023-09-13–2023-12-31 was 110 days."
    , Span ("",[],[]) [Str "The date-range ",Span ("",["date-range"],[("title","The date range 2023-09-13\8211\&2023-12-31 lasted 110 days, ending 9 years ago.")]) [Str "2023-09-13",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "4m"]],Str "2023-12-31"],Str " was 110 days."]
    )
  , (2033, Str "The date-range 1995-10-04–1996-03-27 was NGE."
    , Span ("",[],[]) [Str "The date-range ",Span ("",["date-range"],[("title","The date range 1995-10-04\8211\&1996-03-27 lasted 176 days, ending 37 years ago.")]) [Str "1995-10-04",Span ("",["subsup"],[]) [Superscript [Str "\8211"],Subscript [Str "6m"]],Str "1996-03-27",Subscript [Span ("",[],[("title","1995-10-04 was 37 years ago.")]) [Str "37ya"]]],Str " was NGE."]
    )
  ]
