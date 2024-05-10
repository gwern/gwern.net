module Config.Typography where

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
                                  "Camps", "Tamala"]
