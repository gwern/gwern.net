{-# LANGUAGE OverloadedStrings #-}

module Config.Metadata.Author where

import qualified Data.Map.Strict as M (fromList, Map)
import Text.Pandoc (Inline(Link, Span, Space, Str))
import qualified Data.Text as T (Text)

import Interwiki (toWikipediaEnURL)

-- tests: unique keys, URL keys
extractTwitterUsernameTestSuite :: [(String,String)]
extractTwitterUsernameTestSuite = [("https://x.com/grantslatton/status/1703913578036904431", "grantslatton")
                                  , ("https://x.com/grantslatton", "grantslatton")
                                  , ("https://x.com/AndyAyrey/status/1792342948887290106", "AndyAyrey")
                                  , ("https://x.com/_AndyAyrey/status/1792342948887290106", "_AndyAyrey")
                                  , ("https://x.com/sakun135/status/1285408650052333568", "sakun135")
                                  , ("https://x.com/dseetharaman", "dseetharaman")
                                  , ("https://x.com/TylerAlterman/status/1900285728635969841", "TylerAlterman")
                                  ]

-- config testing: all unique
authorCollapseTestCases :: [(String, [Inline])]
authorCollapseTestCases =
  [ ("a", [Space,Span ("",["author","cite-author"],[]) [Str "a"]])
  , ("a, b", [Space,Span ("",["author"],[]) [Str "a",Str ", ",Str "b"]])
  , ("a, b, c", [Space,Span ("",["author"],[]) [Str "a",Str ", ",Str "b",Str ", ",Str "c"]])
  , ("a, b, c, d", [Space,Span ("",["author"],[]) [Str "a",Str ", ",Str "b",Str ", ",Str "c",Str ", ",Str "d"]])
  , ("a, b, c, d, e", [Space,Span ("",["author"],[]) [Str "a",Str ", ",Str "b",Str ", ",Str "c",Span ("",["collapse"],[]) [Str ", ",Str "d",Str ", ",Str "e"]]])
  , ("a, b, c, d, e, f", [Space,Span ("",["author"],[]) [Str "a",Str ", ",Str "b",Str ", ",Str "c",Span ("",["collapse"],[]) [Str ", ",Str "d",Str ", ",Str "e",Str ", ",Str "f"]]])
  , ("a, b, c, d, e, f, g", [Space,Span ("",["author"],[]) [Str "a",Str ", ",Str "b",Str ", ",Str "c",Span ("",["collapse"],[]) [Str ", ",Str "d",Str ", ",Str "e",Str ", ",Str "f",Str ", ",Str "g"]]])

  -- hash disambiguation rendering:
  , ("b#disambiguation", [Space,Span ("",["author","cite-author"],[]) [Str "b"]])

  -- test with link rewrites enabled:
  , ("a, b, c, d, e, f, George Washington", [Space,Span ("",["author"],[]) [Str "a",Str ", ",Str "b",Str ", ",Str "c",Span ("",["collapse"],[]) [Str ", ",Str "d",Str ", ",Str "e",Str ", ",Str "f",Str ", ",Link ("",[],[]) [Str "George Washington"] ("https://en.wikipedia.org/wiki/George_Washington","")]]])
  , ("a, b, c, d, e, f, George Washington#SS", [Space,Span ("",["author"],[]) [Str "a",Str ", ",Str "b",Str ", ",Str "c",Span ("",["collapse"],[]) [Str ", ",Str "d",Str ", ",Str "e",Str ", ",Str "f",Str ", ",Link ("",[],[]) [Str "George Washington"] ("https://en.wikipedia.org/wiki/SS_George_Washington","")]]])
     ]

-- infix rewrites
-- Testing: unique keys, test keys for regexp validity
cleanAuthorsRegexps, cleanAuthorsFixedRewrites :: [(String,String)]
cleanAuthorsRegexps = [
  ("([a-zA-Z]+),([A-Z][a-z]+)", "\\1, \\2") -- "Foo Bar,Quuz Baz" → "Foo Bar, Quuz Baz"
  , (",$", "")
  , (", +", ", ")
  , ("^By ", "") -- "By XYZ" → "XYZ"
  , ("^([A-Z][a-z]+), ([A-Z]\\.)$", "\\2 \\1") -- "Smith, J." → "J. Smith"; for single words followed by a single letter, we can assume that it is a 'surname, initial' rather than 2 authors, 'surname1, surname2'
  , ("^([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.)$", "\\2 \\1, \\4 \\3") -- likewise, but for the 2-author case: 'Smith, J.; Doe, J.'
  , ("^([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.)$", "\\2 \\1, \\4 \\3, \\6 \\5") -- 3-author
  , ("^([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.); ([A-Z][a-z]+), ([A-Z]\\.)$", "\\2 \\1, \\4 \\3, \\6 \\5, \\8 \\7") -- 4-author, and I won't try for more
  , ("([A-Z]\\.)([A-Za-z]+)", "\\1 \\2")                              -- "A.Smith"      → "A. Smith"
  , (" ([A-Z])([A-Z]) ([A-Za-z]+)", " \\1. \\2. \\3")             -- " LK Barnes"   → " L. K. Barnes"
  , ("([A-Z]\\.)([A-Z]\\.) ([A-Za-z]+)", "\\1 \\2 \\3")               -- "A.B. Smith"   → "A. B. Smith"
  , ("([A-Z]\\.)([A-Z]\\.)([A-Z]\\.) ([A-Za-z]+)", "\\1 \\2 \\3 \\4") -- "C.A.B. Smith" → "C. A. B. Smith"
  , (" ([A-Z])([A-Z])([A-Z]) ", " \\1. \\2. \\3. ")                   -- "John HAB Smith" → "John H. A. B. Smith"
  , (" ([A-Z])([A-Z]) ", " \\1. \\2. ")                               -- "John HA Smith"  → "John H. A. Smith"
  , (" ([A-Z]\\.) ([A-Z]) ", " \\1 \\2. ")                            -- "John H. A Smith"  → "John H. A. Smith"
  , (" ([A-Z]) ([A-Z]\\.) ", " \\1. \\2 ")                            -- "John H A. Smith"  → "John H. A. Smith"
  , (" ([A-Z]) ", " \\1. ")                                             -- "John H Smith"   → "John H. Smith"
  ]
cleanAuthorsFixedRewrites = [(". . ", ". "), ("?",""), (",,", ","), (", ,", ", "), (" MA,", ","), (", MA,", ","), (" MS,", ",")
                            , ("Dr ", ""), (" Eh.D.", ""), (" PhD", ""), (" Ph.D.", ""), (" MRCGP", ""), (" OTR/L", ""), (" OTS", "")
                            , (" FMedSci", ""), ("Prof ", ""), (" FRCPE", ""), (" FRCP", ""), (" FRS", ""), (" MD", "")
                            , (",, ,", ", "), ("; ", ", "), (" ; ", ", "), (" , ", ", "), (" and ", ", "), (", & ", ", ")
                            , (", and ", ", "), (" MD,", " ,"), (" M. D.,", " ,"), (" MSc,", " ,"), (" M. Sc.", ""), (" B. Sc.", "")
                            , (" PhD,", " ,"), (" Ph.D.,", " ,"), (" BSc,", ","), (" BSc(Hons)", ""), (" MHSc,", ",")
                            , (" BScMSc,", ","), (" ,,", ","), (" PhD1", ""), (" BA(Hons),1", ""), (" , BSc(Hons),1", ",")
                            , (" , MHSc,", ","), ("PhD,1,2 ", ""), ("PhD,1", ""), (" , BSc", ", "), (",1 ", ","), (" & ", ", ")
                            , ("BA(Hons),", ","), (", (Hons),", ","), (", ,2 ", ","), (",2", ","), (" MSc", ","), (" , PhD,", ",")
                            , (" JD,", ","), ("MS,", ","), (" BS,", ","), (" MB,", ","), (" ChB", ""), ("Meena", "M."), (", PhD1", ",")
                            , ("  DMSc", ""), (",, ", ", "), (", ,,", ", "), ("\"", ""), ("'", "’"), ("OpenAI, :, ", ""), (" et al", "")
                            , (" et al.", ""), (", et al.", ""), ("Jr.", "Junior"), (", Jr.", " Junior"), (", Junior", " Junior")
                            , (" DO,", ","), ("M. D. MPH", ""), (" ", " "), (" M. D. MBA", ""), (" Esq.", ""), (" Esq,", ",")
                            , (" CAAB,", ","), (" DVM,", ","), (" D.V.M.", ","), (" M. D. M. P. H.", "")
                            , (" M. D. MMM", ""), (" M. D. MHS", "")]

-- Config tests: unique list
authorLinkBlacklist :: [T.Text]
authorLinkBlacklist = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"] ++
                    ["1890\8211\&1974", "1902","1906","1916","1922","1928",":", "English Wikipedia", "Wiel", "Word", "Rau", "Qi"
                    , "Pontifex", "Postma", "Poinar", "Pier", "Pika", "van Buuren","van Os","van den Hurk", "van der Ploeg", "Anonymous"]

-- Odd author names which look like a typo or error, but are real or the best that can be done. Used in `LinkMetadata.readLinkMetadataAndCheck`.
-- Config tests: unique list
authorWhitelist :: [String]
authorWhitelist = ["K. U.", "6510#HN", "N. K.", "0xType", "3D_DLW"]

-- list of rewrites for 'alternative name' → 'canonical name'. This is a simple mapping which doesn't attempt to handle variants like initializations. For that, see `canonicalsWithInitials`.
-- Config tests: unique keys, no loops
canonicals :: M.Map String String
canonicals = M.fromList
  [ ("ESYudkowsky", "Eliezer Yudkowsky")
  , ("AaronKoelker", "Aaron Koelker")
  , ("alexalbert__", "Alex Albert")
  , ("alicemazzy", "Alice Maz")
  , ("Mr_AllenT", "Allen Tsui")
  , ("alyssamvance", "Alyssa M. Vance")
  , ("amandaaskell", "Amanda Askell")
  , ("amasad", "Amjad Masad")
  , ("karpathy", "Andrej Karpathy")
  , ("AndrewCurran_", "Andrew Curran")
  , ("andrewwhite01", "Andrew White")
  , ("AnitaJott", "Anita Jott")
  , ("AnthonyLeeZhang", "Anthony Zhang")
  , ("anton_bakhtin", "Anton Bakhtin")
  , ("miolini", "Artem Andreenko")
  , ("bindureddy", "Bindu Reddy")
  , ("tunguz", "Bojan Tunguz")
  , ("brenankeller", "Brenan Keller")
  , ("BrendanBycroft", "Brendan Bycroft")
  , ("BrianRoemmele", "Brian Roemmele")
  , ("bryanhpchiang", "Bryan Chiang")
  , ("charliebholtz", "Charlie B. Holtz")
  , ("sea_snell", "Charlie Snell")
  , ("chaseleantj", "Chase Lean")
  , ("ChengleiSi", "Chenglei Si")
  , ("ChrisJBakke", "Chris Bakke")
  , ("ClaireSilver12", "Claire Silver")
  , ("DanielColson6", "Daniel Colson")
  , ("David_Kasten", "David Kasten")
  , ("DavidSHolz", "David S. Holz")
  , ("davisblalock", "Davis Blalock")
  , ("literallydenis", "Denis Shiryaev")
  , ("depthsofwiki", "Depths of Wikipedia")
  , ("DorsaAmir", "Dorsa Amir")
  , ("DouglasLFarrar", "Douglas L. Farrar")
  , ("edleonklinger", "Ed Leon Klinger")
  , ("kmett", "Edward Kmett")
  , ("E. Yudkowsky", "Eliezer Yudkowsky")
  , ("Yudkowsky", "Eliezer Yudkowsky")
  , ("elonmusk", "Elon Musk")
  , ("EMostaque", "Emad Mostaque")
  , ("emad", "Emad Mostaque")
  , ("eshear", "Emmett Shear")
  , ("EricSchmidt", "Eric Schmidt")
  , ("emollick", "Ethan Mollick")
  , ("fabianstelzer", "Fabian Stelzer")
  , ("fchollet", "Francois Chollet")
  , ("francoisfleuret", "Francois Fleuret")
  , ("GjMcGowan", "G. J. McGowan")
  , ("garrynewman", "Garry Newman")
  , ("geoffreyirving", "Geoffrey Irving")
  , ("kindgracekind", "Grace Kind")
  , ("GrantSlatton", "Grant Slatton")
  , ("gdb", "Greg Brockman")
  , ("GregKamradt", "Greg Kamradt")
  , ("hannu", "Hannu Rajaniemi")
  , ("ilyasut", "Ilya Sutskever")
  , ("JacobJanerka", "Jacob Janerka")
  , ("jakezward", "Jake Ward")
  , ("mezaoptimizer", "James Campbell")
  , ("repligate", "Janus")
  , ("javilopen", "Javi Lopez")
  , ("mathemagic1an", "Jay Hack")
  , ("JeffDean", "Jeff Dean")
  , ("jeremyphoward", "Jeremy P. Howard")
  , ("Jill_hubley", "Jill Hubley")
  , ("DrJimFan", "Jim Fan")
  , ("_joelsimon", "Joel Simon")
  , ("john__allard", "John Allard")
  , ("ID_AA_Carmack", "John Carmack")
  , ("JSEllenberg", "Jordan Ellenberg")
  , ("joshwhiton", "Josh Whiton")
  , ("thebasepoint", "Joshua Batson")
  , ("joshua_xu_", "Joshua Xu")
  , ("voooooogel", "Jukka Luoma")
  , ("mealreplacer", "Julian Hazell")
  , ("juliewdesign_", "Julie W. Design")
  , ("venturetwins", "Justine Moore")
  , ("kanishkamisra", "Kanishka Misra")
  , ("kartographien", "Kart ographien")
  , ("kendrictonn", "Kendric Tonn")
  , ("kenneth0stanley", "Kenneth O. Stanley")
  , ("lmsysorg", "LMSYS Org")
  , ("LeelaChessZero", "Leela Chess Zero")
  , ("Letter_Library", "Letter Library")
  , ("LouisKnightWebb", "Louis Knight-Webb")
  , ("Malcolm_Ocean", "Malcolm Ocean")
  , ("realhashbreaker", "Marc Stevens")
  , ("MarginaliaNu", "Marginalia")
  , ("marvinvonhagen", "Marvin von Hagen")
  , ("mbateman", "Matt Bateman")
  , ("MatthewJBar", "Matthew Bar")
  , ("mckaywrigley", "Mckay Wrigley")
  , ("michael_nielsen", "Michael Nielsen")
  , ("misha_saul", "Misha Saul")
  , ("mayfer", "Murat Ayfer")
  , ("natfriedman", "Nat Friedman")
  , ("NathalieRach", "Nathalie Rach")
  , ("nathan_culley", "Nathan Culley")
  , ("nealagarwal", "Neal Agarwal")
  , ("thecaptain_nemo", "Nemo")
  , ("nickcammarata", "Nick Cammarata")
  , ("nikitabier", "Nikita Bier")
  , ("novelaiofficial", "NovelAI")
  , ("parisba", "Paris Buttfield-Addison")
  , ("patriciogv", "Patricio Gonzalez Vivo")
  , ("patio11", "Patrick McKenzie")
  , ("paulg", "Paul Graham")
  , ("petergyang", "Peter G. Yang")
  , ("npew", "Peter Welinder")
  , ("peterxichen", "Peter Xi Chen")
  , ("philhawksworth", "Phil Hawksworth")
  , ("skirano", "Pietro Schirano")
  , ("pika_labs", "Pika")
  , ("qdrant_engine", "Qdrant")
  , ("rharang", "Rich Harang")
  , ("RichardSocher", "Richard Socher")
  , ("RickLeeJames", "Rick Lee James")
  , ("goodside", "Riley Goodside")
  , ("ritageleta", "Rita Geleta")
  , ("robmen", "Rob Mensching")
  , ("robinsaikia", "Robin Saikia")
  , ("rogerkmoore", "Roger K. Moore")
  , ("krishnanrohit", "Rohit Krishnan")
  , ("rowancheung", "Rowan Cheung")
  , ("ryancbriggs", "Ryan C. Briggs")
  , ("sama", "Sam Altman")
  , ("thesamparr", "Sam Parr")
  , ("sdtoyer", "Sam Toyer")
  , ("sarah_cone", "Sarah Cone")
  , ("shawwn", "Shawn Presser")
  , ("shawwnpresser", "Shawn Presser")
  , ("joshwimmer", "Josh Wimmer")
  , ("daviddebeer", "David Debeer")
  , ("nickgevers", "Nick Gevers")
  , ("shawwwn", "Shawn Presser")
  , ("shawwwwn", "Shawn Presser")
  , ("swyx", "Shawn Wang")
  , ("ShayneRedford", "Shayne Redford")
  , ("sherjilozair", "Sherjil Ozair")
  , ("Suhail", "Suhail Doshi")
  , ("SullyOmarr", "Sully Omarr")
  , ("suchenzang", "Suzan Zhang")
  , ("nemocentric", "Thomas Moynihan")
  , ("fortelabs", "Tiago Forte")
  , ("timd_ca", "Tim Davison")
  , ("vykthur", "Victor Dibia")
  , ("VictorTaelin", "Victor Taelin")
  , ("willdepue", "Will Depue")
  , ("williamcusick", "William Cusick")
  , ("ylecun", "Yann LeCun")
  , ("yishan", "Yishan Wong")
  , ("zachweingarten", "Zach Weingarten")
  , ("wenquai", "Zachary Lee")
  , ("retvitr", "Zlatý Retvítr")
  , ("vi", "pinkddle")
  , ("Student", "William Sealy Gosset")
  , ("William Gosset", "William Sealy Gosset")
  , ("William Sealey Gosset", "William Sealy Gosset")
  , ("R.A. Fisher", "R. A. Fisher")
  , ("Fisher", "R. A. Fisher")
  , ("Sutskever", "Ilya Sutskever")
  , ("Engelbart", "Douglas Engelbart")
  , ("Anno", "Hideaki Anno")
  , ("gwern", "Gwern")
  , ("gwernbranwen", "Gwern")
  , ("gwern.branwen", "Gwern")
  , ("Gwern Branwen", "Gwern")
  , ("Branwen", "Gwern")
  , ("Donald E. Knuth", "Donald Knuth")
  , ("Knuth", "Donald Knuth")
  , ("Don Knuth", "Donald Knuth")
  , ("Hans J. Eysenck", "Hans Eysenck")
  , ("Hans Jürgen Eysenck", "Hans Eysenck")
  , ("Eysenck", "Hans Eysenck")
  , ("Frank P. Ramsey", "Frank Ramsey")
  , ("F. P. Ramsey", "Frank Ramsey")
  , ("Polu", "Stanislas Polu")
  , ("spolu", "Stanislas Polu")
  , ("Okada", "Toshio Okada")
  , ("Herbert Simon", "Herbert A. Simon")
  , ("Stephen D. H. Hsu", "Steve Hsu")
  , ("Social Science Genetic Association Consortium", "SSGAC")
  , ("Arm", "Arm Holdings")
  , ("anonymous", "Anonymous")
  , ("anon", "Anonymous")
  , ("anon.", "Anonymous")
  , ("23andMe Research Team", "23andMe")
  , ("sarah_cone", "Sarah Cone")
  , ("R. Impagliazzo", "Russell Impagliazzo")
  , ("Gary L. Drescher", "Gary Drescher")
  , ("Doug Hofstadter", "Douglas Hofstadter")
  , ("Douglas R. Hofstadter", "Douglas Hofstadter")
  , ("Emily Willoughby", "Emily A. Willoughby")
  , ("James Lee", "James J. Lee")
  , ("Timothy P. Lillicrap", "Timothy Lillicrap")
  , ("Samuel Ritter", "Sam Ritter")
  , ("A. Narayanan", "Arvind Narayanan")
  , ("Daniel Bernstein", "Daniel J. Bernstein")
  , ("djb", "Daniel J. Bernstein")
  , ("John Barrington [Ian Stewart]", "Ian Stewart")
  , ("Daniel C. Dennett", "Daniel Dennett")
  , ("K. Eric Drexler", "Eric Drexler")
  , ("DanHendrycks", "Dan Hendrycks")
  , ("Daniel Hendrycks", "Dan Hendrycks")
  , ("0xDesigner", "Timothy Drisdelle")
  , ("_MG_", "MG")
  , ("_akhaliq", "AK")
  , ("_vztu", "Zhengzhong Tu")
  , ("arankomatsuzaki", "Aran Komatsuzaki")
  , ("mattshumer_", "Matt Shumer")
  , ("R. H. Coase", "Ronald Coase")
  , ("L. A. Belady", "László Bélády")
  , ("Alice R. Spooner", "Alice Spooner")
  , ("hsu_steve", "Steve Hsu")
  , ("Darold Treffert", "Darold A. Treffert")
  , ("ryanqnorth", "Ryan North")
  , ("ryannorth", "Ryan North")
  , ("Robert Ladd Thorndike", "Robert L. Thorndike")
  , ("Robert Thorndike", "Robert L. Thorndike")
  , ("R. Thorndike", "Robert L. Thorndike")
  , ("R. L. Thorndike", "Robert L. Thorndike")
  , ("R.L. Thorndike", "Robert L. Thorndike")
  , ("chrlaf", "Christian Laforte")
  , ("F. Jelinek", "Frederick Jelinek")
  , ("Christopher Fettweis", "Christopher J. Fettweis")
  , ("J. P. Gordon", "James P. Gordon")
  , ("cademetz", "Cade Metz")
  , ("qntm", "Sam Hughes")
  , ("Dorret Boomsma", "Dorret I. Boomsma")
  , ("Sarah Medland", "Sarah E. Medland")
  , ("Ole Andreassen", "Ole A. Andreassen")
  , ("Grant Montgomery", "Grant W. Montgomery")
  , ("Benjamin Neale", "Benjamin M. Neale")
  , ("Tonu Esko", "Tõnu Esko")
  , ("Anders Borglum", "Anders Børglum")
  , ("Anders D. Børglum", "Anders Børglum")
  , ("Yuan YAO", "Yuan Yao")
  , ("Zhi Zheng (郑值)", "Zhi Zheng")
  , ("Ruth J. F. Loos", "Ruth Loos")
  , ("David M. Hougaard", "David Hougaard")
  , ("Andy Jones", "Andy L. Jones")
  , ("David M. Hougaard", "David Hougaard")
  , ("Nicolas Hees", "Nicolas Heess")
  , ("Elliot Tucker-Drob", "Elliot M. Tucker-Drob")
  , ("Jon L. Bentley", "Jon Bentley")
  , ("William G. Iacono", "William Iacono")
  , ("Tim Frayling", "Timothy Frayling")
  , ("Timothy M. Frayling", "Timothy Frayling")
  , ("nickwalton00", "Nick Walton")
  , ("matthew_d_green", "Matthew D. Green")
  , ("Jouke- Jan Hottenga", "Jouke-Jan Hottenga")
  , ("vgr", "Venkatesh Rao")
  , ("André Uitterlinden", "André G. Uitterlinden")
  , ("Andre Uitterlinden", "André G. Uitterlinden")
  , ("Andre G. Uitterlinden", "André G. Uitterlinden")
  , ("elonmusk", "Elon Musk")
  , ("DimitrisPapail", "Dimitris Papail")
  , ("JonBaronforMD", "Jon Baronfor")
  , ("Andrew Wood", "Andrew R. Wood")
  , ("xkcd", "Randall Munroe")
  , ("Randall Monroe", "Randall Munroe")
  , ("Alexander Berg", "Alexander C. Berg")
  , ("Phúc H. Lê Khắc", "Phuc H. Le Khac")
  , ("Phuc H. Le-Khac", "Phuc H. Le Khac")
  , ("Alan Smeaton", "Alan F. Smeaton")
  , ("Sam Bowman", "Samuel R. Bowman")
  , ("Sam R. Bowman", "Samuel R. Bowman")
  , ("Johan Gunnar Eriksson", "Johan G. Eriksson")
  , ("Steven Piantadosi", "Steven T. Piantadosi")
  , ("Daniel Benjamin", "Daniel J. Benjamin")
  , ("aidangomez", "Aidan Gomez")
  , ("Terho Lehtimaki", "Terho Lehtimäki")
  , ("T. Lehtimaki", "Terho Lehtimäki")
  , ("T. Lehtimäki", "Terho Lehtimäki")
  , ("Philipp D. Koellinger", "Philipp Koellinger")
  , ("Philipp D Koellinger", "Philipp Koellinger")
  , ("P D Koellinger", "Philipp Koellinger")
  , ("P. D. Koellinger", "Philipp Koellinger")
  , ("NoaNabeshima", "Noa Nabeshima")
  , ("Tamara Harris", "Tamara B. Harris")
  , ("Reedik Magi", "Reedik Mägi")
  , ("R. Magi", "Reedik Mägi")
  , ("R. Mägi", "Reedik Mägi")
  , ("tszzl", "Roon")
  , ("Cornelia M. van Duijn", "Cornelia van Duijn")
  , ("arfafax", "Arfafax")
  , ("Aleksander Mądry", "Aleksander Madry")
  , ("wallacetim", "Tim Wallace")
  , ("Loic Yengo", "Loïc Yengo")
  , ("Kari North", "Kari E. North")
  , ("Markus Nöthen", "Markus M. Nöthen")
  , ("Dale Nyholt", "Dale R. Nyholt")
  , ("MParakhin", "Mikhail Parakhin")
  , ("Henry Greely", "Hank Greely")
  , ("Henry T. Greely", "Hank Greely")
  , ("Lude H. Franke", "Lude Franke")
  , ("L. H. Franke", "Lude Franke")
  , ("Leon Gatys", "Leon A. Gatys")
  , ("Jerome Rotter", "Jerome I. Rotter")
  , ("Peter Joshi", "Peter K. Joshi")
  , ("aidan_mclau", "Aidan McLau")
  , ("Zoltan Kutalik", "Zoltán Kutalik")
  , ("smerity", "Stephen Merity")
  , ("S merity", "Stephen Merity")
  , ("S Merity", "Stephen Merity")
  , ("jek", "James Bradbury")
  , ("jekbradbury", "James Bradbury")
  , ("David Porteous", "David J. Porteous")
  , ("MR Jabalameli", "M. Reza Jabalameli")
  , ("Howard Phillips Lovecraft", "H. P. Lovecraft")
  , ("ramsey nasser", "Ramsey Nasser")
  , ("Cornelius Rietveld", "Cornelius A. Rietveld")
  , ("Michael A. Woodley of Menie", "Michael A. Woodley")
  , ("hsfzxjy", "Xie Jingyi")
  , ("Altimor", "Flo Crivello")
  , ("Cecilia Lindgren", "Cecilia M. Lindgren")
  , ("Okada Toshio", "Toshio Okada")
  , ("Michael Owen", "Michael J. Owen")
  , ("Jonathan RI Coleman", "Jonathan R. I. Coleman")
  , ("Jonathan R.I. Coleman", "Jonathan R. I. Coleman")
  , ("srush_nlp", "Alexander M. Rush")
  , ("Sasha Rush", "Alexander M. Rush")
  , ("Alexander Rush", "Alexander M. Rush")
  , ("Anlatan", "NovelAI")
  , ("CFGeek", "Charles Foster")
  , ("RokoMijic", "Roko Mijic")
  , ("JohnLaTwC", "John Lambert")
  , ("Nicholas John Timpson", "Nicholas J. Timpson")
  , ("Nicholas Timpson", "Nicholas J. Timpson")
  , ("Lester Hiatt", "Les Hiatt")
  , ("Lester Richard Hiatt", "Les Hiatt")
  , ("Lester R. Hiatt", "Les Hiatt")
  , ("Jian'an G. Luan", "Jian’an Luan")
  , ("Jian'an Luan", "Jian’an Luan")
  , ("David Hinds", "David A. Hinds")
  , ("Zvi Moshowitz", "Zvi Mowshowitz")
  , ("Zvi", "Zvi Mowshowitz")
  , ("TheZvi", "Zvi Mowshowitz")
  , ("Astralite Heart", "AstraliteHeart")
  , ("astraliteheart", "AstraliteHeart")
  , ("Carrier Michael A.", "Michael A. Carrier")
  , ("Pearce Elizabeth N.", "Elizabeth N. Pearce")
  , ("Samuel Messick (edt)", "Samuel Messick")
  , ("23Skidoo", "Mikhail Glushenkov")
  , ("timothycbates", "Timothy C. Bates")
  , ("Elizabeth Loftus", "Elizabeth F. Loftus")
  , ("Philip Tetlock", "Philip E. Tetlock")
  , ("yvain", "Scott Alexander")
  , ("Abraham A. Palmer", "Abraham Palmer")
  , ("Adebowale A. Adeyemo", "Adebowale Adeyemo")
  , ("Alkes L. Price", "Alkes Price")
  , ("Bengt Holmstrom", "Bengt Holmstr\246m")
  , ("Goncalo Abecasis", "Gon\231alo Abecasis")
  , ("Goncalo R. Abecasis", "Gon\231alo Abecasis")
  , ("Irenaus Eibl-Eibesfeldt", "Iren\228us Eibl-Eibesfeldt")
  , ("M. Csikszentmihalyi", "Mihaly Csikszentmihalyi")
  , ("John P. A. Ioannidis", "John Ioannidis")
  , ("John P.A. Ioannidis", "John Ioannidis")
  , ("David Geary", "David C. Geary")
  , ("Geoffrey E. Hinton", "Geoffrey Hinton")
  , ("Yann Lecun", "Yann LeCun")
  , ("Yann LeCunn", "Yann LeCun")
  , ("Stuart Ritchie", "Stuart J. Ritchie")
  , ("Robert Sternberg", "Robert J. Sternberg")
  , ("Richard Nisbett", "Richard E. Nisbett")
  , ("David Buss", "David M. Buss")
  , ("Arthur Jensen", "Arthur R. Jensen")
  , ("Jurgen Schmidhuber", "J\252rgen Schmidhuber")
  , ("Juergen Schmidhuber", "J\252rgen Schmidhuber")
  , ("Jurgen Schmidhuber", "J\252rgen Schmidhuber")
  , ("J. Schmidhuber", "J\252rgen Schmidhuber")
  , ("dlberes", "Damon Beres")
  , ("Michelle Meyer", "Michelle N. Meyer")
  , ("stefaesthesia", "Stefanie")
  , ("regehr", "John Regehr")
  , ("Bernard S. Greenberg", "Bernard Greenberg")
  , ("Sharon Kardia", "Sharon L. R. Kardia")
  , ("Mika Kähönen", "Kähönen Mika")
  , ("Kathryn Kemper", "Kathryn E. Kemper")
  , ("John Rice", "John P. Rice")
  , ("Jari Marko Lahti", "Jari Lahti")
  , ("James Tiptree Jr.", "Alice B. Sheldon")
  , ("James Tiptree, Jr.", "Alice B. Sheldon")
  , ("James Tiptree, Junior", "Alice B. Sheldon")
  , ("Alice Bradley Sheldon", "Alice B. Sheldon")
  , ("Larry A. Wasserman", "Larry Wasserman")
  , ("David Weir", "David R. Weir")
  , ("Jessica Faul", "Jessica D. Faul")
  , ("Jennifer Smith", "Jennifer A. Smith")
  , ("__anjor", "Anjor Kanekar")
  , ("Zygmunt Z.", "Zygmunt Zajc")
  , ("Nelson Repenning", "Nelson P. Repenning")
  , ("John Sterman", "John D. Sterman")
  , ("John David Sterman", "John D. Sterman")
  , ("mehran__jalali", "Mehran Jalali")
  , ("Outsideness", "Nick Land")
  , ("Henry Volzke", "Henry Völzke")
  , ("habryka", "Oliver Habryka")
  , ("MLP Wikia", "<em>My Little Pony</em> Wikia")
  , ("M L. P. Wikia", "<em>My Little Pony</em> Wikia")
  , ("Ben Domingue", "Benjamin W. Domingue")
  , ("Ben W. Domingue", "Benjamin W. Domingue")
  , ("ashleevance", "Ashlee Vance")
  , ("Richard P. Feynman", "Richard Feynman")
  , ("RobertMMetcalfe", "Robert Metcalfe")
  , ("Bob Metcalfe", "Robert Metcalfe")
  , ("miramurati", "Mira Murati")
  , ("Economist", "<em>The Economist</em>")
  , ("The Economist", "<em>The Economist</em>")
  , ("Economist Magazine", "<em>The Economist</em>")
  , ("DaveShapi", "David Shapiro")
  , ("Robert McGrew", "Bob McGrew")
  , ("bobmcgrewai", "Bob McGrew")
  , ("barret_zoph", "Barret Zoph")
  , ("Misha Laskin", "Michael Laskin")
  , ("Popular Science", "<em>Popular Science</em>")
  , ("Luke_Metz", "Luke Metz")
  , ("OriolVinyalsML", "Oriol Vinyals")
  , ("Colin N. A. Palmer", "Colin Palmer")
  , ("Lukasz Kaiser", "Łukasz Kaiser")
  , ("Benjamin K. Goodrich", "Ben Goodrich")
  , ("Benjamin Goodrich", "Ben Goodrich")
  , ("Joseph R. Biden Junior", "Joe Biden")
  , ("Joseph R. Biden",        "Joe Biden")
  , ("iamaheron_", "heron")
  , ("chillzaza_", "Zahid Khawaja")
  , ("colin_fraser", "Colin Fraser")
  , ("JAAKKO TUOMILEHTO", "Jaakko Tuomilehto")
  , ("Sauers_", "Sauers")
  , ("Tinca Polderman", "Tinca J. C. Polderman")
  , ("sharifshameem", "Sharif Shameem")
  , ("Themistocles (Tim) L. Assimes", "Themistocles L. Assimes")
  , ("Tim L. Assimes", "Themistocles L. Assimes")
  , ("Tim Assimes", "Themistocles L. Assimes")
  , ("Bret C. Devereaux", "Bret Devereaux")
  , ("QiaochuYuan", "Qiaochu Yuan")
  , ("Mohammad Arfan Ikram", "M. Arfan Ikram")
  , ("Arfan Ikram", "M. Arfan Ikram")
  , ("bmk", "Leo Gao")
  , ("bmk.sh", "Leo Gao")
  , ("G. Davies", "Gail Davies")
  , ("sashachapin", "Sasha Chapin")
  , ("Daníel Fannar Guðbjartsson", "Daniel F. Gudbjartsson")
  , ("Johan Wouter Jukema", "J. Wouter Jukema")
  , ("Johan W. Jukema", "J. Wouter Jukema")
  , ("J. W. Jukema", "J. Wouter Jukema")
  , ("Anno Hideaki", "Hideaki Anno")
  , ("RichardMCNgo", "Richard Ngo")
  , ("giffmana", "Lucas Beyer")
  , ("Alexander Tabarrok", "Alex Tabarrok")
  , ("andrew_n_carr", "Andrew N. Carr")
  , ("patrickc", "Patrick Collison")
  , ("Elisabeth Widén", "Elisabeth Widen")
  , ("Eco de Geus", "Eco J. C. de Geus")
  , ("Eco J. de Geus", "Eco J. C. de Geus")
  , ("Eco C. de Geus", "Eco J. C. de Geus")
  , ("Eco J C de Geus", "Eco J. C. de Geus")
  , ("Carolina Medina Gomez", "Carolina Medina-Gomez")
  , ("Brenda Penninx", "Brenda W. J. H. Penninx")
  , ("Anne Justice", "Anne E. Justice")
  , ("Raymond M. Smullyan", "Raymond Smullyan")
  , ("Karén Simonyan", "Karen Simonyan")
  , ("Robin L. Carhart-Harris", "Robin Carhart-Harris")
  , ("austinc3301", "Agus")
  , ("M. Kumari", "Meena Kumari")
  , ("M Kumari", "Meena Kumari")
  , ("KE Markon", "Kristian E. Markon")
  , ("Erwin P. Bottinger", "Erwin Böttinger")
  , ("E. P. Bottinger", "Erwin Böttinger")
  , ("Erwin Bottinger", "Erwin Böttinger")
  , ("Erwin P. Böttinger", "Erwin Böttinger")
  , ("E. P. Böttinger", "Erwin Böttinger")
  , ("J.R.R. Tolkien", "J. R. R. Tolkien")
  , ("JRR Tolkien", "J. R. R. Tolkien")
  , ("J R R Tolkien", "J. R. R. Tolkien")
  , ("Edward N. Luttwak", "Edward Luttwak")
  , ("Yuhuai(Tony) Wu", "Yuhuai Wu")
  , ("Yuhuai (Tony) Wu", "Yuhuai Wu")
  , ("Tony Wu", "Yuhuai Wu")
  , ("N. A. Smith", "Noah A. Smith")
  , ("M.E. Thomas", "M. E. Thomas")
  , ("Nicholas James Patterson", "Nick Patterson")
  , ("Nicholas J. Patterson", "Nick Patterson")
  , ("Nicholas Patterson", "Nick Patterson")
  , ("Satu Mannisto", "Satu Männistö")
  , ("Melissa García", "Melissa E. Garcia")
  , ("o1-pro", "ChatGPT-4 o1-pro")
  , ("__nmca__", "Nat McAleese")
  , ("Ines Barroso", "Inês Barroso")
  , ("Hinrich Schutze", "Hinrich Schütze")
  , ("Hinrich Schuetze", "Hinrich Schütze")
  , ("H. Schütze", "Hinrich Schütze")
  , ("United States Patent and Trademark Office", "USPTO")
  , ("hardmaru", "David Ha")
  , ("QuintinPope5", "Quintin Pope")
  , ("Beate St. Pourcain", "Beate St Pourcain")
  , ("Beate Pourcain", "Beate St Pourcain")
  , ("Beate S. Pourcain", "Beate St Pourcain")
  , ("B. S. Pourcain", "Beate St Pourcain")
  , ("Pourcain", "Beate St Pourcain")
  , ("St Pourcain", "Beate St Pourcain")
  , ("AmandaAskell", "Amanda Askell")
  , ("Peter Deutsch", "L. Peter Deutsch")
  , ("ChrisChipMonk", "Chris Lakin")
  , ("Henrik Gronberg", "Henrik Grönberg")
  , ("Henrik Groenberg", "Henrik Grönberg")
  , ("Douglas Summers Stay", "Douglas Summers-Stay")
  , ("_rockt", "Tim Rocktäschel")
  , ("Angela Duckworth", "Angela L. Duckworth")
  , ("M. Douglas McIlroy", "Douglas McIlroy")
  , ("420_gunna", "Sam")
  , ("Christiaan A. de Leeuw", "Christiaan de Leeuw")
  , ("DeepSeek-AI", "DeepSeek")
  , ("SpencrGreenberg", "Spencer Greenberg")
  , ("Aellagirl", "Aella")
  , ("aella_girl", "Aella")
  , ("_sinity", "Sinity")
  , ("nelhage", "Nelson Elhage")
  , ("Francis_YAO", "Francis Yao")
  , ("Francis_YAO_", "Francis Yao")
  , ("James T. Walters", "James T. R. Walters")
  , ("James R. Walters", "James T. R. Walters")
  , ("James Walters", "James T. R. Walters")
  , ("R.Wm. Gosper", "Bill Gosper")
  , ("R. W. Gosper", "Bill Gosper")
  , ("William Gosper", "Bill Gosper")
  , ("Per Hoffman", "Per Hoffmann")
  , ("chrisbarber", "Chris Barber")
  , ("Quoc Le", "Quoc V. Le")
  , ("Pamela Fine Mishkin", "Pamela Mishkin")
  , ("Marjo-Riitta Jarvelin", "Marjo-Riitta Järvelin")
  , ("tamaybes", "Tamay Besiroglu")
  , ("Will Highleyman", "Wilbur H. Highleyman")
  , ("Bill Highleyman", "Wilbur H. Highleyman")
  , ("William Highleyman", "Wilbur H. Highleyman")
  , ("William H. Highleyman", "Wilbur H. Highleyman")
  , ("DCM Liewald", "David C. M. Liewald")
  , ("D.C.M. Liewald", "David C. M. Liewald")
  , ("D Liewald", "David C. M. Liewald")
  , ("D C M Liewald", "David C. M. Liewald")
  , ("DCM. Liewald", "David C. M. Liewald")
  , ("David Cherry McLachlan Liewald", "David C. M. Liewald")
  , ("DeryaTR_", "Derya Unutmaz")
  , ("Sam Cohen", "Samuel T. Cohen")
  , ("Thomas J. Bouchard Junior", "Thomas J. Bouchard")
  , ("Thomas J. Bouchard, Junior", "Thomas J. Bouchard")
  , ("Thomas J. Bouchard Jr", "Thomas J. Bouchard")
  , ("Thomas J. Bouchard Jr.", "Thomas J. Bouchard")
  , ("0xtype", "0xType")
  , ("maxhodak_", "Max Hodak")
  , ("ArtIsLight_", "ArtIsLight")
  , ("_Borriss_", "Borriss")
  , ("ccanonne_", "Clément Canonne")
  , ("Clement Canonne", "Clément Canonne")
  , ("C. Canonne", "Clément Canonne")
  , ("JosephJacks_", "Joseph Jacks")
  , ("JoshuaBlake_", "Joshua Blake")
  , ("_markel___", "Mark Ermolov")
  , ("__meimiya__", "銘宮")
  , ("_Soilleir_", "Soilleir")
  , ("_via_getty_", "Getty")
  , ("VivaLaPanda_", "Panda")
  , ("_jasonwei", "Jason Wei")
  , ("_JeanLannes", "Jean Lannes")
  , ("michelangemoji", "MichelangEmoji Bot")
  , ("_dsevero", "Daniel Severo")
  , ("_Ryobot", "Ryobot")
  , ("Ryobot", "Ryo Nakamura")
  , ("0xfoobar", "foobar")
  , ("_JeanLannes", "Michael")
  , ("_aixile", "Aixile")
  , ("Roel Andre Ophoff", "Roel André Ophoff")
  , ("Paul Vitányi", "Paul M. B. Vitányi")
  , ("Pamela Madden", "Pamela A. F. Madden")
  , ("Ozren Polasek", "Ozren Polašek")
  , ("AaronBergman18", "Aaron Bergman")
  , ("lishali88", "Lisha Li")
  , ("Mikael Landen", "Mikael Landén")
  , ("Michael A. Woodley of Menie, Yr.", "Michael A. Woodley")
  , ("Michael A. Woodley of Menie, Younger", "Michael A. Woodley")
  , ("Michael A. Woodley of Menie", "Michael A. Woodley")
  , ("John W. S. Bradshaw", "John Bradshaw")
  , ("dylan522p", "Dylan Patel")
  , ("Edward Thompson Jaynes", "E. T. Jaynes")
  , ("E. Thompson Jaynes", "E. T. Jaynes")
  , ("Edward T. Jaynes", "E. T. Jaynes")
  , ("Hill Fung Ip", "Hill F. Ip")
  , ("TsviBT", "Tsvi Benson-Tilsen")
  , ("Anna Vinkhuyzen", "Anna A. E. Vinkhuyzen")
  , ("Steve_Yegge", "Steve Yegge")
  , ("Sara Hagg", "Sara Hägg")
  , ("Oleg", "Oleg Kiselyov")
  , ("linear", "nearcyan")
  , ("Linear", "nearcyan")
  , ("near", "nearcyan")
  , ("Near", "nearcyan")
  , ("Nearcyan", "nearcyan")
  , ("Robert A. Heinlein", "Robert Heinlein")
  , ("hippke", "Michael Hippke")
  , ("kundu", "Bornali Kundu")
  , ("gpt2", "GPT-2")
  , ("wrangham", "Richard W. Wrangham")
  , ("skylion", "Aaron Gokaslan")
  , ("Skylion", "Aaron Gokaslan")
  , ("SkyLi0n", "Aaron Gokaslan")
  , ("herbert", "Frank Herbert")
  , ("frankherbert", "Frank Herbert")
  , ("spinrad", "Norman Spinrad")
  , ("normanspinrad", "Norman Spinrad")
  , ("mcnelly", "Willis E. McNelly")
  , ("willismcnelly", "Willis E. McNelly")
  , ("willisemcnelly", "Willis E. McNelly")
  , ("Greg Park", "Gregory Park")
  , ("Bertram Muller-Myhsok", "Bertram Müller-Myhsok")
  , ("Bertram Mueller-Myhsok", "Bertram Müller-Myhsok")
  , ("Lyman Spitzer Junior", "Lyman Spitzer")
  , ("Alena Yaluri", "Alena Stančáková")
  , ("Alena Stančáková Yaluri", "Alena Stančáková")
  , ("Alena Yaluri Stančáková", "Alena Stančáková")
  , ("Alena Stancakova", "Alena Stančáková")
  , ("bryan_johnson", "Bryan Johnson")
  , ("Asa Johansson", "Åsa Johansson")
  , ("mark_riedl", "Mark Riedl")
  , ("Czeslaw Milosz", "Czesław Miłosz")
  , ("C.S. Lewis", "C. S. Lewis")
  , ("Locus Mag", "Locus")
  , ("Locus Magazine", "Locus")
  , ("Steven Levitt", "Steven D. Levitt")
  , ("U de Faire", "Ulf de Faire")
  , ("U. de Faire", "Ulf de Faire")
  , ("de Faire u", "Ulf de Faire")
  , ("Faire U", "Ulf de Faire")
  , ("defaire U", "Ulf de Faire")
  , ("elder_plinius", "Pliny the Liberator")
  , ("nptacek", "Nicholas Ptacek")
  , ("Karin Verweij", "Karin J. H. Verweij")
  , ("Shane Gu", "Shixiang Shane Gu")
  , ("shaneguML", "Shixiang Shane Gu")
  , ("michaeldirda", "Michael Dirda")
  , ("genewolfe", "Gene Wolfe")
  , ("_ceee_", "Cee")
  , ("layer07_yuxi", "Yuxi Liu")
  , ("elidourado", "Eli Dourado")
  , ("kajaykumarr_", "AKR")
  , ("nrehiew_", "wh")
  , ("Hastings", "Hastings Greer")
  , ("Donna Hart", "Donna Lee Hart")
  , ("Richard J. Fateman", "Richard Fateman")
  ]

-- tests: unique
canonicalsWithInitials :: [String]
canonicalsWithInitials =
 ["Ingrid Sigfrid Melle", "Ken K. Ong", "Lenore J. Launer", "Olli T. Raitakari"
  , "Andrew D. Grotzinger", "Penelope A. Lind", "Saul Justin Newman", "Alice B. Sheldon"
  , "Michael N. Weedon", "Felix R. Day", "John A. Peralta", "W. David Hill"
  , "Scott D. Gordon", "Michel G. Nivard", "Howard J. Edenberg", "Cristen Jennifer Willer"
  , "Bruce M. Psaty", "Benjamin W. Domingue", "Tune H. Pers", "Travis T. Mallard", "Lars L. Lind"
  , "Kenneth O. Stanley", "Sarah E. Harris", "Preben Bo Mortensen", "Rodney J. Scott", "Riccardo E. Marioni"
  , "Irving John Good", "Robert J. Cava", "Ilja M. Nolte", "David W. Scott", "Ben A. Oostra"
  , "Edmund L. Gettier", "Aidan N. Gomez", "Rona J. Strawbridge", "Joyce Y. Tung"
  , "Joshua B. Tenenbaum", "Daniel W. Belsky", "Albert Vernon Smith", "Zachary C. Lipton"
  , "Themistocles L. Assimes", "Nilesh J. Samani", "Daniel F. Gudbjartsson", "Kilian Q. Weinberger"
  , "Eli Ayumi Stahl", "Anne Uriu Jackson", "Andrew Tym Hattersley", "Anders Martin Dale"
  , "Alan R. Shuldiner", "Aditya A. Ramesh", "Sonja I. Berndt", "Nicholas A. Furlotte"
  , "Melissa E. Garcia", "Mary F. Feitosa", "John C. Chambers", "John Joseph McGrath"
  , "Ingrid B. Borecki", "Alfred C. Raphelson", "Thomas G. Schulze", "Raymond K. Walters"
  , "Narelle K. Hansell", "Pablo V. Gejman", "Lawrence F. Bielak", "Judith M. Vonk"
  , "Juan Pablo Frias", "Hilary Kiyo Finucane", "Elizabeth G. Holliday", "Devin M. Absher"
  , "Albertine J. Oldehinkel", "Olav B. Smeland", "Timo A. Lakka", "Steven A. McCarroll"
  , "Stephanie H. Witt", "Paul M. Thompson", "Patricia A. Peyser", "Joshua S. Gans"
  , "Nathan E. Sanders", "Andrew C. Yao", "James L. Kennedy", "Sydney A. Asdell"
  , "Emma C. Johnson", "Paul D. MacLean", "Wilbur H. Highleyman", "Caroline M. Nievergelt"
  , "Adam Edward Locke", "Samuel T. Cohen", "Thomas J. Bouchard", "Robert Mitchell Lindner"
  , "Sarah E. Bergen", "Roel André Ophoff", "Peter A. Holmans", "Patricia B. Munroe"
  , "Michael A. Woodley", "Mark J. Caulfield", "E. T. Jaynes", "Iris M. Heid", "Hill F. Ip"
  , "Heather M. Stringham", "Christopher A. Haiman", "Catharina A. Hartman", "Bernhard T. Baune"
  , "Karl T. Compton", "Tom B. Brown", "Saskia P. Hagenaars", "Robert R. McCrae", "Nathan A. Gillespie"
  , "Marian L. Hamshere", "Richard W. Wrangham", "John R. Attia", "Joel Z. Leibo", "Jaspal S. Kooner"
  , "Larry V. Hedges", "Hugh Christian Watkins", "Stephen Cole Kleene", "David C. Glahn"
  , "Bernhard Otto Boehm", "John W. Tukey", "Behrooz Z. Alizadeh", "Aki S. Havulinna"
  , "Richard Sharpe Shaver", "Wouter J. Peyrot", "Wendy L. McArdle", "Peter T. Leeson"
  , "Brian O. Bernstein", "Thorgeir E. Thorgeirsson", "Susan M. Ring", "Stephen S. Rich"
  , "Philip S. Dale", "Lee N. Robbins", "Hyman G. Rickover", "Lucia A. Hindorff"
  , "Peter P. Pramstaller", "Kent M. Pitman", "Louise S. Bicknell", "Laura Jean Bierut", "Grover S. Krantz"]

-- Config tests: unique all, no loops, all values are URLs, no overlap between the non-canonical rewrites & the canonicals, no '&' present in key (usually means a corrupted HTML entity which should be replaced by a Unicode literal)
authorLinkDB :: M.Map T.Text T.Text
authorLinkDB = M.fromList $
  zip authorWpLinkDB (map toWikipediaEnURL authorWpLinkDB) ++ -- we put the WP link first for easier reading/editing, but all WP entries are overridden by by an entry below:
   [ ("George Washington#SS", "https://en.wikipedia.org/wiki/SS_George_Washington")
    , ("Alexey Guzey","https://guzey.com/")
    , ("Carl Shulman","https://timelines.issarice.com/wiki/Timeline_of_Carl_Shulman_publications#Full_timeline")
    , ("I. Richard Savage","https://projecteuclid.org/journals/statistical-science/volume-14/issue-1/A-conversation-with-I-Richard-Savage-with-the-assistance-of/10.1214/ss/1009211808.full")
    , ("James Yu","https://jamesyu.org/")
    , ("Kicks Condor","https://www.kickscondor.com/")
    , ("Michael Nielsen","https://michaelnielsen.org/")
    , ("Nick Bostrom","https://nickbostrom.com/")
    , ("Said Achmiz","https://wiki.obormot.net/")
    , ("Shawn Presser","https://x.com/theshawwn")
    , ("Gwern", "/index#abstract")
    , ("Scott Alexander", "https://www.astralcodexten.com/")
    , ("Lucas Beyer", "https://scholar.google.com/citations?user=p2gwhK4AAAAJ")
    , ("Tanner Greer", "https://scholars-stage.org/")
    , ("Noah Smith", "https://en.wikipedia.org/wiki/Noah_Smith_(writer)")
    , ("William Chan", "http://williamchan.ca/")
    , ("Roberto Trotta", "https://robertotrotta.com/")
    , ("Hyeonwoo Kim", "https://hyunw.kim/")
    , ("Scott Cunningham", "https://en.wikipedia.org/wiki/Scott_Cunningham_(economist)")
    , ("Charles A. Taylor", "https://ca-taylor.com/")
    , ("Shi Feng", "https://ihsgnef.github.io/")
    , ("Jonathan Gruber", "https://en.wikipedia.org/wiki/Jonathan_Gruber_(economist)")
    , ("Jacob Andreas", "https://www.mit.edu/~jda/")
    , ("Max Bain", "https://maxbain.com/")
    , ("David Lewis", "https://en.wikipedia.org/wiki/David_Lewis_(philosopher)")
    , ("Xiang Cheng", "https://sites.google.com/berkeley.edu/xiangcheng/home")
    , ("FDA", "https://en.wikipedia.org/wiki/Food_and_Drug_Administration")
    , ("Sergey Slyusarev", "https://github.com/jemmybutton")
    , ("James Richardson", "https://en.wikipedia.org/wiki/James_Richardson_(poet)")
    , ("Morgan McGuire", "https://casual-effects.com/")
    , ("Emily A. Willoughby", "https://emilywilloughby.com/research")
    , ("Dan Luu", "https://danluu.com/")
    , ("James J. Lee", "https://jamesjlee.altervista.org/")
    , ("Ning Ding", "https://scholar.google.com/citations?user=uZXQuYAAAAAJ")
    , ("Yuan Yao", "https://scholar.google.com/citations?user=OOlHr-wAAAAJ")
    , ("Zhi Zheng", "https://scholar.google.com/citations?user=uoYLG8UAAAAJ")
    , ("Xu Han", "https://arxiv.org/search/cs?searchtype=author&query=Han,+X")
    , ("Ye Li", "https://scholar.google.com/citations?user=_gPcuqIAAAAJ")
    , ("Sam Ritter", "https://scholar.google.co.uk/citations?user=dg7wnfAAAAAJ")
    , ("Blake Richards", "https://mila.quebec/en/person/blake-richards/")
    , ("Arvind Narayanan", "https://en.wikipedia.org/wiki/Arvind_Narayanan")
    , ("Ian Stewart", "https://en.wikipedia.org/wiki/Ian_Stewart_(mathematician)")
    , ("Mark S. Miller", "https://en.wikipedia.org/wiki/Mark_S._Miller")
    , ("David Décary-Hétu", "https://www.cicc-iccc.org/en/people/regular_researchers/david_decary_hetu")
    , ("Mikel Olazaran", "https://scholar.google.com/citations?user=XoUNbRoAAAAJ")
    , ("Dan Hendrycks", "https://people.eecs.berkeley.edu/~hendrycks/")
    , ("Matt Shumer", "https://x.com/mattshumer_")
    , ("Richard Ngo", "https://www.richardcngo.com/")
    , ("M. M. Lehman", "https://en.wikipedia.org/wiki/Manny_Lehman_(computer_scientist)")
    , ("László Bélády", "https://en.wikipedia.org/wiki/L%C3%A1szl%C3%B3_B%C3%A9l%C3%A1dy")
    , ("Sanjiv Kumar", "https://scholar.google.com/citations?user=08CNqrYAAAAJ&view_op=list_works&sortby=pubdate")
    , ("Aapo Kyrola", "https://www.cs.cmu.edu/~akyrola/")
    , ("Abhishek Kumar", "https://scholar.google.com/citations?user=6vghMS0AAAAJ")
    , ("Abigail Fisher", "https://profiles.ucl.ac.uk/4836-abi-fisher")
    , ("Abigail Powers", "https://scholar.google.com/citations?user=f7gqpBIAAAAJ")
    , ("Adriana Munoz", "https://scholar.google.com/citations?user=jlfqfeEAAAAJ")
    , ("A. Jameson", "http://aero-comlab.stanford.edu/jameson/index.html")
    , ("Albert Wong", "https://scholar.google.com/citations?user=Z-JN7vIAAAAJ")
    , ("Alex Calderwood", "https://scholar.google.com/citations?user=TIUe46YAAAAJ")
    , ("Alice Spooner", "https://scholar.google.com/scholar?as_sdt=0%2C21&q=author%3A%22ALR+Spooner%22")
    , ("Jonathan S. Rosenfeld", "https://scholar.google.com/citations?user=8HYnMeYAAAAJ&view_op=list_works&sortby=pubdate")
    , ("Doug Rohrer", "https://scholar.google.com/citations?user=lK2WfTUAAAAJ")
    , ("Kevin Scott", "https://en.wikipedia.org/wiki/Kevin_Scott_(computer_scientist)")
    , ("Samuel J. Gershman", "https://gershmanlab.com/people/sam.html")
    , ("Blake Bordelon", "https://blakebordelon.github.io/")
    , ("Gretchen Krueger", "https://scholar.google.com/citations?user=astFxkwAAAAJ")
    , ("Luke Muehlhauser", "https://lukemuehlhauser.com/")
    , ("Rachel Metz", "https://x.com/rachelmetz")
    , ("Christian Laforte", "https://x.com/chrlaf")
    , ("Daniel Selsam", "https://scholar.google.com/citations?user=yaSqFaEAAAAJ")
    , ("Christopher J. Fettweis", "https://en.wikipedia.org/wiki/Christopher_Fettweis")
    , ("Cade Metz", "https://x.com/cademetz")
    , ("Craig Cannon", "https://craigc.org/")
    , ("Sam Hughes", "https://qntm.org/self")
    , ("Sergey Levine", "https://scholar.google.com/citations?user=8R35rCwAAAAJ")
    , ("Dorret I. Boomsma", "https://en.wikipedia.org/wiki/Dorret_Boomsma")
    , ("Sarah E. Medland", "https://en.wikipedia.org/wiki/Sarah_Medland")
    , ("Jian Yang", "https://en.wikipedia.org/wiki/Jian_Yang_(geneticist)")
    , ("Ole A. Andreassen", "https://scholar.google.com/citations?user=dilW0WsAAAAJ")
    , ("Thomas Werge", "https://scholar.google.com/citations?user=mRphVYYAAAAJ")
    , ("Grant W. Montgomery", "https://scholar.google.com/citations?user=4eOEw-IAAAAJ")
    , ("Benjamin M. Neale", "https://en.wikipedia.org/wiki/Benjamin_Neale")
    , ("David Silver", "https://en.wikipedia.org/wiki/David_Silver_(computer_scientist)")
    , ("Luke Zettlemoyer", "https://scholar.google.com/citations?user=UjpbO6IAAAAJ")
    , ("Caroline Hayward", "https://www.ed.ac.uk/profile/caroline-hayward")
    , ("Paul Lichtenstein", "https://ki.se/en/people/paul-lichtenstein")
    , ("Alec Radford", "https://scholar.google.com/citations?user=dOad5HoAAAAJ")
    , ("Tõnu Esko", "https://scholar.google.com/citations?user=pzykPaMAAAAJ")
    , ("Anders Børglum", "https://pure.au.dk/portal/en/persons/anders%40biomed.au.dk")
    , ("Ruth Loos", "https://profiles.mountsinai.org/ruth-loos")
    , ("Unnur Thorsteinsdottir", "https://scholar.google.com/citations?user=Dh2gs1gAAAAJ")
    , ("Koray Kavukcuoglu", "https://scholar.google.com/citations?user=sGFyDIUAAAAJ")
    , ("Gonneke Willemsen", "https://scholar.google.com/citations?user=fjolqosAAAAJ")
    , ("Gail Davies", "https://scholar.google.com/citations?user=N5VMS4cAAAAJ")
    , ("David Hougaard", "https://scholar.google.com/citations?user=UJ7aJC4AAAAJ")
    , ("Aarno Palotie", "https://researchers.mgh.harvard.edu/profile/14173445/Aarno-Palotie")
    , ("Colin Jarvis", "https://x.com/colintjarvis")
    , ("Veikko Salomaa", "https://thl.fi/en/research-and-development/research-professors/veikko-salomaa")
    , ("Nicolas Heess", "https://scholar.google.com/citations?user=79k7bGEAAAAJ")
    , ("Daniel Levy", "https://ai.stanford.edu/~danilevy/")
    , ("Andy L. Jones", "https://andyljones.com/")
    , ("Mike Lewis", "https://scholar.google.com/citations?user=SnQnQicAAAAJ")
    , ("Jon Bentley", "https://en.wikipedia.org/wiki/Jon_Bentley_(computer_scientist)")
    , ("Timothy Frayling", "https://scholar.google.com/citations?user=XWqva80AAAAJ")
    , ("Margaret J. Wright", "https://scholar.google.com/scholar?q=Margaret%20J.%20Wright")
    , ("Lili Milani", "https://www.etis.ee/CV/Lili_Milani/eng/")
    , ("Daniel Rock", "https://www.danielianrock.com/")
    , ("Razvan Pascanu", "https://sites.google.com/view/razp/home")
    , ("Jouke-Jan Hottenga", "https://scholar.google.com/citations?user=lG4IKN8AAAAJ")
    , ("Yi Tay", "https://www.yitay.net/")
    , ("Jianfeng Gao", "https://www.microsoft.com/en-us/research/people/jfgao/")
    , ("Stefano Ermon", "https://scholar.google.com/citations?user=ogXTOZ4AAAAJ")
    , ("André G. Uitterlinden", "https://scholar.google.com/scholar?q=author%3A%22AG+Uitterlinden%22")
    , ("Jakob Grove", "https://scholar.google.com/citations?user=bM9AJ-EAAAAJ")
    , ("Jared Kaplan", "https://sites.krieger.jhu.edu/jared-kaplan/")
    , ("David Warde-Farley", "https://dwf.name/")
    , ("Sherjil Ozair", "https://sherjil.ozair.ai/")
    , ("Srdjan Djurovic", "https://scholar.google.com/citations?user=NcCt9VwAAAAJ")
    , ("Omer Levy", "https://scholar.google.com/citations?user=PZVd2h8AAAAJ")
    , ("Ole Mors", "https://research.com/u/ole-mors")
    , ("Manuel Mattheisen", "https://scholar.google.com/citations?user=uhlDFm4AAAAJ")
    , ("Claire Jordan", "http://members.madasafish.com/~cj_whitehound/")
    , ("Andrew R. Wood", "https://medicine.exeter.ac.uk/people/profile/index.php?web_id=Andrew_Wood")
    , ("Albert Hofman", "https://en.wikipedia.org/wiki/Albert_Hofman_(epidemiologist)")
    , ("Eric Boerwinkle", "https://www.hgsc.bcm.edu/people/boerwinkle-e")
    , ("Jascha Sohl-Dickstein", "https://scholar.google.com/citations?user=-3zYIjQAAAAJ")
    , ("Jia Deng", "https://scholar.google.com/citations?user=U3Eub-EAAAAJ")
    , ("Sanjeev Satheesh", "https://scholar.google.com/citations?user=VUi7eM8AAAAJ")
    , ("Jonathan Krause", "https://scholar.google.com/citations?user=7DwDYzkAAAAJ")
    , ("Michael J. Bernstein", "https://scholar.google.com/citations?user=9CDiC-gAAAAJ")
    , ("Michael S. Bernstein", "https://hci.stanford.edu/msb/")
    , ("Leopold Aschenbrenner", "https://situational-awareness.ai/")
    , ("Hao Su", "https://cseweb.ucsd.edu/~haosu/")
    , ("Alexander C. Berg", "https://en.wikipedia.org/wiki/Alexander_Berg")
    , ("Zhiheng Huang", "https://scholar.google.com/citations?user=uW8JaBsAAAAJ")
    , ("Sean Ma", "https://scholar.google.com/citations?user=WLj6zycAAAAJ")
    , ("Aditya Khosla", "https://people.csail.mit.edu/khosla/")
    , ("Jong Wook Kim", "https://jongwook.kim/")
    , ("Sandhini Agarwal", "https://scholar.google.com/citations?user=8UZIqcoAAAAJ")
    , ("Phuc H. Le Khac", "https://lkhphuc.com/")
    , ("George Berzsenyi", "https://www.npr.org/2019/04/07/707326070/a-math-teachers-life-summed-up-by-the-gifted-students-he-mentored")
    , ("Alan F. Smeaton", "https://en.wikipedia.org/wiki/Alan_Smeaton")
    , ("Samuel R. Bowman", "https://cims.nyu.edu/~sbowman/")
    , ("Felix Hill", "https://fh295.github.io/")
    , ("Alex Wang", "https://w4ngatang.github.io/")
    , ("Julian Michael", "https://julianmichael.org/")
    , ("Amanpreet Singh", "https://apsdehal.in/")
    , ("Robert Geirhos", "https://scholar.google.com/citations?user=w3kGtMIAAAAJ")
    , ("Percy Liang", "https://cs.stanford.edu/~pliang/")
    , ("Johan G. Eriksson", "https://medicine.nus.edu.sg/researcher/johan-gunnar-eriksson/")
    , ("Matthias Bethge", "https://scholar.google.com/citations?user=0z0fNxUAAAAJ")
    , ("Jacob Steinhardt", "https://jsteinhardt.stat.berkeley.edu/")
    , ("Steven T. Piantadosi", "https://colala.berkeley.edu/people/piantadosi/")
    , ("Christian Gieger", "https://orcid.org/0000-0001-6986-9554")
    , ("Zhilin Yang", "https://scholar.google.com/citations?user=7qXxyJkAAAAJ")
    , ("Daniel J. Benjamin", "https://scholar.google.com/citations?user=wIEUEuYAAAAJ")
    , ("Harry Campbell", "https://scholar.google.com/citations?user=ihShJe0AAAAJ")
    , ("Steven Basart", "https://stevenbas.art/")
    , ("Najaf Amin", "https://scholar.google.com/citations?user=m1eR5vkAAAAJ")
    , ("Terho Lehtimäki", "https://www.tuni.fi/en/terho-lehtimaki")
    , ("Richard Socher", "https://www.socher.org/")
    , ("Jonathan Ho", "http://www.jonathanho.me/")
    , ("Colin Raffel", "https://colinraffel.com/")
    , ("Philipp Koellinger", "http://www.philipp-koellinger.com/")
    , ("Erik Ingelsson", "https://wavelifesciences.com/company/leadership/executives/erik-ingelsson-md-phd/")
    , ("Noa Nabeshima", "https://x.com/NoaNabeshima")
    , ("Ethan Mollick", "https://x.com/emollick")
    , ("Tamara B. Harris", "https://www.healthandagingpolicy.org/fellows/tamara-harris-md-ms/")
    , ("Reedik Mägi", "https://scholar.google.com/citations?user=mKbe4v0AAAAJ")
    , ("Caiming Xiong", "http://cmxiong.com/")
    , ("Andrew Snyder-Beattie", "https://www.openphilanthropy.org/about/team/andrew-snyder-beattie/")
    , ("Michael Page", "https://arxiv.org/search/cs?searchtype=author&query=Page,+M")
    , ("Ben Garfinkel", "https://www.benmgarfinkel.com/")
    , ("Miles Brundage", "https://www.milesbrundage.com/")
    , ("Jelena Luketina", "https://scholar.google.com/citations?user=zpil5xkAAAAJ")
    , ("Igor Mordatch", "https://scholar.google.com/citations?user=Vzr1RukAAAAJ")
    , ("Fernando Rivadeneira", "https://scholar.google.com/scholar?as_sdt=0%2C21&q=author%3A%22F+Rivadeneira%22")
    , ("Gudmar Thorleifsson", "https://research.com/u/gudmar-thorleifsson-1")
    , ("Arfafax", "https://x.com/arfafax")
    , ("Zihang Dai", "https://scholar.google.com/citations?user=uZqsVXkAAAAJ")
    , ("Hannaneh Hajishirzi", "https://homes.cs.washington.edu/~hannaneh/")
    , ("Alexander Teumer", "https://www2.medizin.uni-greifswald.de/psych/ueber-uns/leitungmitarbeiter/alexander-teumer/")
    , ("Aleksander Madry", "http://madry.mit.edu/")
    , ("Joaquin Quiñonero Candela", "https://quinonero.net/")
    , ("Lilian Weng", "https://lilianweng.github.io/")
    , ("Abdel Abdellaoui", "https://scholar.google.com/citations?user=hsyseKEAAAAJ")
    , ("Stella Biderman", "https://www.stellabiderman.com/")
    , ("Loïc Yengo", "https://researchers.uq.edu.au/researcher/14187")
    , ("Kari E. North", "https://scholar.google.com/citations?user=gelzK1EAAAAJ")
    , ("Emily Mullin", "https://www.emilymullin.com/")
    , ("Frank Hutter", "https://ml.informatik.uni-freiburg.de/profile/hutter/")
    , ("Yiming Yang", "https://www.cs.cmu.edu/~./yiming/")
    , ("Po-Ru Loh", "https://dms.hms.harvard.edu/people/po-ru-loh")
    , ("Daniel I. Chasman", "https://prevmed.bwh.harvard.edu/daniel-i-chasman-phd/")
    , ("Markus Perola", "https://scholar.google.com/citations?user=0m3tqBEAAAAJ")
    , ("Markus M. Nöthen", "https://scholar.google.com/citations?user=owiOkEkAAAAJ")
    , ("Henning Tiemeier", "https://www.hsph.harvard.edu/profile/henning-tiemeier/")
    , ("Dale R. Nyholt", "https://en.wikipedia.org/wiki/Dale_R._Nyholt")
    , ("Mikhail Parakhin", "https://x.com/MParakhin")
    , ("Adam Mastroianni", "https://www.adammastroianni.com/")
    , ("Wei Zhao", "https://en.wikipedia.org/wiki/Wei_Zhao_(computer_scientist)")
    , ("Vilmundur Gudnason", "https://scholar.google.com/citations?user=eDkARCwAAAAJ")
    , ("Lude Franke", "https://www.rug.nl/staff/l.h.franke/cv")
    , ("Leon A. Gatys", "https://scholar.google.com/citations?user=ADMVEmsAAAAJ")
    , ("Jerome I. Rotter", "https://lundquist.org/jerome-i-rotter-md")
    , ("Alexander S. Ecker", "https://openreview.net/profile?id=~Alexander_S_Ecker1")
    , ("Samuli Ripatti", "https://scholar.google.com/citations?user=BkWwC18AAAAJ")
    , ("Peter K. Joshi", "https://scholar.google.co.uk/citations?user=HRgG478AAAAJ")
    , ("Julio Rosenstock", "https://www.cardiometabolichealth.org/faculty/julio-rosenstock/")
    , ("John R. B. Perry", "https://www.johnomics.co.uk/about")
    , ("Aidan McLau", "https://x.com/aidan_mclau")
    , ("Zoltán Kutalik", "https://wp.unil.ch/sgg/zoltan/")
    , ("Stephen Merity", "https://state.smerity.com/")
    , ("Michel G. Nivard", "https://scholar.google.com/citations?user=omRGPjUAAAAJ")
    , ("Guy Lever", "https://scholar.google.com/citations?user=1XgR518AAAAJ")
    , ("Thomas Moynihan", "https://thomasmoynihan.xyz/")
    , ("James Bradbury", "https://x.com/jekbradbury")
    , ("David J. Porteous", "https://www.ed.ac.uk/profile/professor-david-porteous")
    , ("Andrew P. Morris", "https://www.liverpool.ac.uk/health-and-life-sciences/staff/andrew-morris/")
    , ("Stavroula Kanoni", "https://www.qmul.ac.uk/whri/people/academic-staff/items/kanonistavroula.html")
    , ("Luigi Ferrucci", "https://irp.nih.gov/pi/luigi-ferrucci")
    , ("John Schulman", "http://joschu.net/")
    , ("Jing Hua Zhao", "https://jinghuazhao.github.io/")
    , ("Jacob Hilton", "https://www.jacobh.co.uk/")
    , ("Hreinn Stefansson", "https://scholar.google.is/citations?user=BsBsqS0AAAAJ")
    , ("Deepa Seetharaman", "https://x.com/dseetharaman")
    , ("Yongmei Liu", "https://medicine.duke.edu/profile/yongmei-liu")
    , ("Cornelius A. Rietveld", "https://scholar.google.com/citations?user=EEqPJRYAAAAJ")
    , ("Sheng Shen", "https://sincerass.github.io/")
    , ("Steve Hsu", "https://stevehsu.substack.com/")
    , ("Spencer Greenberg", "https://www.spencergreenberg.com/")
    , ("Xie Jingyi", "https://github.com/hsfzxjy")
    , ("Flo Crivello", "https://flocrivello.com/")
    , ("Cecilia M. Lindgren", "https://en.wikipedia.org/wiki/Cecilia_Lindgren")
    , ("Toshio Okada", "https://en.wikipedia.org/wiki/Toshio_Okada")
    , ("Michael J. Owen", "https://en.wikipedia.org/wiki/Michael_Owen_(psychiatrist)")
    , ("Mantas Mazeika", "https://scholar.google.com/citations?user=fGeEmLQAAAAJ")
    , ("Jonathan R. I. Coleman", "https://www.kcl.ac.uk/people/jonathan-coleman")
    , ("Alexander M. Rush", "https://rush-nlp.com/")
    , ("W. David Hill", "https://scholar.google.com/citations?user=evn79GkAAAAJ")
    , ("Scott D. Gordon", "https://research.com/u/scott-d-gordon")
    , ("John Lambert", "https://x.com/JohnLaTwC")
    , ("Owain Evans", "https://owainevans.github.io/")
    , ("Ross Girshick", "https://scholar.google.com/citations?user=W8VIEZgAAAAJ")
    , ("Nicholas J. Timpson", "https://scholar.google.com/citations?user=jRYUWdUAAAAJ")
    , ("Kristian Hveem", "https://scholar.google.com/citations?user=epz0IZIAAAAJ")
    , ("Sasha Chapin", "https://www.sashachapin.com/")
    , ("Les Hiatt", "https://en.wikipedia.org/wiki/Lester_Hiatt")
    , ("Jian’an Luan", "https://www.mrc-epid.cam.ac.uk/people/jianan-luan/")
    , ("Jack Clark", "https://jack-clark.net/about/")
    , ("David A. Hinds", "https://scholar.google.com/citations?user=epTHxM0AAAAJ")
    , ("Aditya Grover", "https://scholar.google.com/citations?user=oOhnPUgAAAAJ")
    , ("Junxiong Wang", "https://www.cs.cornell.edu/~junxiong/")
    , ("Mikhail Glushenkov", "https://github.com/23Skidoo")
    , ("Tri Dao", "https://tridao.me/")
    , ("Antonio Torralba", "https://mitibmwatsonailab.mit.edu/people/antonio-torralba/")
    , ("Robin Sloan", "https://www.robinsloan.com/")
    , ("Yukinori Okada", "https://www.osaka-u.ac.jp/en/news/global_outlook/research_highlights/research_highlights201807_1")
    , ("Thomas Wolf", "https://thomwolf.io/")
    , ("Michelle N. Meyer", "https://www.michellenmeyer.com/")
    , ("Aaron Courville", "https://scholar.google.com/citations?user=km6CP8cAAAAJ")
    , ("Eric Jang", "https://evjang.com/about/")
    , ("Stefanie", "https://x.com/stefaesthesia")
    , ("John Regehr", "https://en.wikipedia.org/wiki/John_Regehr")
    , ("Sharon L. R. Kardia", "https://en.wikipedia.org/wiki/Sharon_Kardia")
    , ("Nikita Nangia", "https://scholar.google.com/citations?user=DoXtjzcAAAAJ")
    , ("Kähönen Mika", "https://scholar.google.com/citations?user=fZXV3C8AAAAJ")
    , ("Laurens van der Maaten", "https://lvdmaaten.github.io/")
    , ("H. Tracy Hall", "https://en.wikipedia.org/wiki/Tracy_Hall")
    , ("Kathryn E. Kemper", "https://imb.uq.edu.au/profile/1720/kathryn-kemper")
    , ("John P. Rice", "https://profiles.wustl.edu/en/persons/john-rice")
    , ("Jari Lahti", "https://scholar.google.com/citations?user=3qAsiw8AAAAJ")
    , ("F. Filce Leek", "/doc/history/1986-dixon.pdf")
    , ("Alice B. Sheldon", "https://en.wikipedia.org/wiki/James_Tiptree_Jr.")
    , ("Larry Wasserman", "https://en.wikipedia.org/wiki/Larry_A._Wasserman")
    , ("Michael L. Smith", "https://scholar.google.com/citations?user=lREVIHgAAAAJ")
    , ("David R. Weir", "https://hrs.isr.umich.edu/about/scientific-leadership/david-r-weir")
    , ("Andy Zou", "https://andyzoujm.github.io/")
    , ("Albert Gu", "https://scholar.google.com/citations?user=DVCHv1kAAAAJ")
    , ("Michael N. Weedon", "https://medicine.exeter.ac.uk/people/profile/index.php?web_id=Michael_Weedon")
    , ("Mordechai Guri", "https://www.wired.com/story/air-gap-researcher-mordechai-guri/")
    , ("Merete Nordentoft", "https://ikm.ku.dk/english/contact/specialties/psychiatry/?pure=en%2Fpersons%2Fmerete-nordentoft(6d0fe19c-7c81-463b-8bc8-822d71584b69).html")
    , ("John M. Starr", "https://research.com/u/john-m-starr")
    , ("Jessica D. Faul", "https://hrs.isr.umich.edu/about/scientific-leadership/jessica-faul")
    , ("Jennifer A. Smith", "https://sph.umich.edu/faculty-profiles/smith-jennifer.html")
    , ("Casey Handmer", "https://www.caseyhandmer.com/")
    , ("Kazutaka Kurihara", "https://sites.google.com/site/qurihara/top-english")
    , ("Koji Tsukada", "https://mobiquitous.com/index-e.html")
    , ("Anjor Kanekar", "https://anjor.xyz/")
    , ("Zygmunt Zajc", "https://fastml.com/about/")
    , ("Nelson P. Repenning", "https://mitsloan.mit.edu/faculty/directory/nelson-p-repenning")
    , ("John D. Sterman", "https://en.wikipedia.org/wiki/John_Sterman")
    , ("Mehran Jalali", "https://x.com/mehran__jalali")
    , ("Richard J. Sullivan", "https://en.wikipedia.org/wiki/Sir_Richard_Sullivan,_1st_Baronet")
    , ("Henry Völzke", "https://scholar.google.com/citations?user=k3hYjjYAAAAJ")
    , ("Felix R. Day", "https://scholar.google.com/citations?user=UtxHZp8AAAAJ")
    , ("Andrew D. Grotzinger", "https://www.colorado.edu/ibg/andrew-grotzinger")
    , ("Alan F. Wright", "https://research.com/u/alan-f-wright")
    , ("Oliver Habryka", "https://www.lesswrong.com/users/habryka4")
    , ("Ben Pace", "https://www.lesswrong.com/users/benito")
    , ("John A. Peralta", "http://www.johnperaltafineart.com/meet-the-artist")
    , ("<em>My Little Pony</em> Wikia", "https://mlp.fandom.com/wiki/My_Little_Pony_Friendship_is_Magic_Wiki")
    , ("Sven Cichon", "https://scholar.google.com/citations?user=oF24hDgAAAAJ")
    , ("Robin Rombach", "https://scholar.google.com/citations?user=ygdQhrIAAAAJ")
    , ("Peter Vollenweider", "https://orcid.org/0000-0002-0765-896X")
    , ("Penelope A. Lind", "https://www.qimrberghofer.edu.au/people/penelope-lind/")
    , ("Saul Justin Newman", "https://www.ageing.ox.ac.uk/people/view/552")
    , ("Olli T. Raitakari", "https://www.utu.fi/en/people/olli-raitakari")
    , ("Luke Metz", "https://scholar.google.com/citations?user=k_u5ULgAAAAJ")
    , ("Lenore J. Launer", "https://irp.nih.gov/pi/lenore-launer")
    , ("Ken K. Ong", "https://www.mrc-epid.cam.ac.uk/people/ken-ong/")
    , ("Ingrid Sigfrid Melle", "https://www.med.uio.no/klinmed/english/people/aca/imelle/")
    , ("Howard J. Edenberg", "https://medicine.iu.edu/faculty/6488/edenberg-howard")
    , ("George Dedoussis", "https://scholar.google.com/citations?user=5yBxRloAAAAJ")
    , ("Evelin Mihailov", "https://www.etis.ee/CV/Evelin_Mihailov/eng/")
    , ("Bruce M. Psaty", "https://gim.uw.edu/people/faculty/bruce-m-psaty")
    , ("Benjamin W. Domingue", "https://cepa.stanford.edu/ben-domingue/")
    , ("Tune H. Pers", "https://scholar.google.com/citations?user=S-ZRIq4AAAAJ")
    , ("Travis T. Mallard", "https://scholar.google.com/citations?user=lgaX5ooAAAAJ")
    , ("Thomas Illig", "https://www.mhh.de/en/institute-zentren-forschungseinrichtungen/hannover-unified-biobank-hub/about-us/prof-dr-thomas-illig")
    , ("Peter Kraft", "https://www.hsph.harvard.edu/profile/peter-kraft/")
    , ("Mark Chen", "https://event.technologyreview.com/emtech-mit-2023/speaker/901826/mark-chen")
    , ("Mohit Bansal", "https://www.cs.unc.edu/~mbansal/")
    , ("Lars L. Lind", "https://research.com/u/lars-lind")
    , ("Jonathan Frankle", "http://www.jfrankle.com/")
    , ("Wendell H. Oswalt", "https://obits.oregonlive.com/us/obituaries/oregon/name/wendell-oswalt-obituary?id=12843091")
    , ("Harm-Jan Westra", "https://scholar.google.com/citations?user=REHR8RQAAAAJ")
    , ("Barret Zoph", "https://barretzoph.github.io/")
    , ("Claes Ohlsson", "https://scholar.google.com/citations?user=RcHgovUAAAAJ")
    , ("Aysu Okbay", "https://scholar.google.com/citations?user=QK359x8AAAAJ")
    , ("Amanda Askell", "https://askell.io/")
    , ("Ajay Jain", "https://ajayj.com/")
    , ("Zhili Zheng", "https://scholar.google.com/citations?user=mLFZunUAAAAJ")
    , ("Veronique Vitart", "https://www.ed.ac.uk/mrc-human-genetics-unit/research/vitart-group")
    , ("Kannan Srinivasan", "https://www.analysisgroup.com/experts-and-consultants/affiliated-experts/kannan-srinivasan/")
    , ("Teresa Ferreira", "https://www.researchgate.net/profile/Teresa-Ferreira-7")
    , ("Stefania Bandinelli", "https://research.com/u/stefania-bandinelli")
    , ("Saurav Kadavath", "https://scholar.google.com/citations?user=Z2Uo_FcAAAAJ")
    , ("Sarah E. Harris", "https://www.ed.ac.uk/profile/sarah-harris")
    , ("<em>The Economist</em>", "https://en.wikipedia.org/wiki/The_Economist")
    , ("Preben Bo Mortensen", "https://www.emedevents.com/speaker-profile/preben-bo-mortensen")
    , ("Rodney J. Scott", "https://hmri.org.au/research/researchers/laureate-professor-rodney-scott/")
    , ("Riccardo E. Marioni", "https://www.research.ed.ac.uk/en/persons/riccardo-marioni")
    , ("David Shapiro", "https://www.youtube.com/@DaveShap")
    , ("Bob McGrew", "https://x.com/bobmcgrewai")
    , ("Niko McCarty", "https://blog.atomsonly.com/")
    , ("Gordon Stables", "https://en.wikipedia.org/wiki/William_Gordon_Stables")
    , ("Philippe Amouyel", "https://www.openaccessgovernment.org/contributor_profile/philippe-amouyel/")
    , ("Patrick Turley", "https://www.paturley.com/")
    , ("Michael Laskin", "https://scholar.google.com/citations?user=DOGDnwsAAAAJ")
    , ("Michael J. Gill", "https://scholar.google.com/citations?user=9R38cgIAAAAJ")
    , ("Michael Gill", "https://scholar.google.com/citations?user=kYY5xu8AAAAJ")
    , ("Polina Kuznetsova", "https://scholar.google.com/citations?user=JdEJEicAAAAJ")
    , ("David Pfau", "http://davidpfau.com/")
    , ("Jan Leike", "https://jan.leike.name/")
    , ("Massimo Mangino", "https://scholar.google.com/citations?user=i0m7948AAAAJ")
    , ("Jonas Bybjerg-Grauholm", "https://en.ssi.dk/about-us/contact/employees/j/jonas-bybjerg-grauholm-jogr")
    , ("Jeff Clune", "http://jeffclune.com/")
    , ("<em>Popular Science</em>", "https://en.wikipedia.org/wiki/Popular_Science")
    , ("Yu Cao", "https://scholar.google.com/citations?user=jkEWQIYAAAAJ")
    , ("Ilja M. Nolte", "https://www.rug.nl/umcg/research/departments/epidemiology/staff/ilja-nolte")
    , ("Geert Molenberghs", "/doc/statistics/survival-analysis/2017-hamasaki.pdf")
    , ("Furu Wei", "https://scholar.google.com/citations?user=G-V1VpwAAAAJ")
    , ("Eva Albrecht", "https://www.euengage.org/EvaA.html")
    , ("David W. Scott", "https://www.stat.rice.edu/~scottdw/")
    , ("Dan Rujescu", "https://scholar.google.com/citations?user=SsYIclwAAAAJ")
    , ("Colin Palmer", "https://discovery.dundee.ac.uk/en/persons/colin-palmer")
    , ("Ben A. Oostra", "https://research.com/u/ben-a-oostra")
    , ("jreyesr", "https://blog.jreyesr.com/")
    , ("Avital Balwit", "https://www.avitalbalwit.com/")
    , ("Edmund L. Gettier", "https://en.wikipedia.org/wiki/Edmund_Gettier")
    , ("Behnam Neyshabur", "https://www.neyshabur.net/")
    , ("Aiden Corvin", "https://www.tcd.ie/research/profiles/?profile=acorvin")
    , ("Tian Ge", "https://researchers.mgh.harvard.edu/profile/13053035/Tian-Ge")
    , ("Thomas Wang", "https://scholar.google.com/citations?user=NncsJNQAAAAJ")
    , ("Paul Darwin Foote", "/doc/science/1979-astin.pdf")
    , ("Jakob Uszkoreit", "https://scholar.google.com/citations?user=mOG0bwsAAAAJ")
    , ("Aidan N. Gomez", "https://en.wikipedia.org/wiki/Aidan_Gomez")
    , ("Łukasz Kaiser", "https://scholar.google.com/citations?user=JWmiQR0AAAAJ")
    , ("Ben Goodrich", "https://polisci.columbia.edu/content/benjamin-k-goodrich")
    , ("Llion Jones", "https://scholar.google.com/citations?user=_3_P5VwAAAAJ")
    , ("Illia Polosukhin", "https://scholar.google.com/citations?user=3SyxFIAAAAAJ")
    , ("Teven Le Scao", "https://scholar.google.com/citations?user=ik0_vxsAAAAJ")
    , ("Niki Parmar", "https://scholar.google.com/citations?user=q2YXPSgAAAAJ")
    , ("Richard Mott Gummere", "/doc/history/1969-allis.pdf#page=11")
    , ("Marcella Rietschel", "https://epws.org/individual-member/prof-marcella-rietschel/")
    , ("Sam McCandlish", "https://scholar.google.com/citations?user=gHp0pu4AAAAJ")
    , ("Rona J. Strawbridge", "https://www.gla.ac.uk/schools/healthwellbeing/staff/ronastrawbridge/#biography,researchinterests,publications,researchdatasets,professionalactivities%26recognition,teaching,supervision,grants,2019,2020,2021,articles")
    , ("Martina Müller-Nurasyid", "https://scholar.google.com/citations?user=-q3OQeUAAAAJ")
    , ("heron", "https://x.com/iamaheron_")
    , ("Zahid Khawaja", "https://github.com/zahidkhawaja")
    , ("Kimin Lee", "https://sites.google.com/view/kiminlee")
    , ("Juha Karjalainen", "https://scholar.google.com/citations?user=LEKc4vwAAAAJ")
    , ("Joyce Y. Tung", "https://typeset.io/authors/joyce-y-tung-2awr195zbq")
    , ("Joshua B. Tenenbaum", "https://en.wikipedia.org/wiki/Joshua_Tenenbaum")
    , ("Colin Fraser", "https://colin-fraser.net/")
    , ("Jaakko Tuomilehto", "https://www.scot-ship.ac.uk/professor-jaakko-tuomilehto.html")
    , ("Daniel W. Belsky", "https://scholar.google.com/citations?user=w3xyjf8AAAAJ")
    , ("Aravind Srinivas", "https://scholar.google.com/citations?user=GhrKC1gAAAAJ")
    , ("Anders Hamsten", "https://ki.se/en/people/anders-hamsten")
    , ("Sauers", "https://x.com/Sauers_")
    , ("Albert Vernon Smith", "https://scholar.google.com/citations?user=8b-433kAAAAJ")
    , ("Zachary C. Lipton", "https://en.wikipedia.org/wiki/Zachary_Lipton")
    , ("Victor Sanh", "https://scholar.google.com/citations?user=6STg_7IAAAAJ")
    , ("Tinca J. C. Polderman", "https://www.amsterdamumc.org/en/research/researchers/tinca-polderman-2.htm")
    , ("Sharif Shameem", "https://sharif.io/")
    , ("Cube Flipper", "https://smoothbrains.net/")
    , ("Thomas W. Mühleisen", "https://www.researchgate.net/profile/Thomas-Muehleisen")
    , ("Themistocles L. Assimes", "https://profiles.stanford.edu/tim_assimes")
    , ("Sailaja Vedantam", "https://scholar.google.com/citations?user=2lzF-1gAAAAJ")
    , ("Per O. J. Hall", "https://scholar.google.com/citations?user=NKaIzaAAAAAJ")
    , ("Per Hall", "https://ki.se/en/people/per-hall")
    , ("Bret Devereaux", "https://acoup.blog/about-the-pedant/")
    , ("Qiaochu Yuan", "https://x.com/QiaochuYuan")
    , ("Panos Roussos", "https://profiles.mountsinai.org/panagiotis-roussos")
    , ("Nilesh J. Samani", "https://en.wikipedia.org/wiki/Nilesh_Samani")
    , ("Michael Carbin", "https://people.csail.mit.edu/mcarbin/")
    , ("M. Arfan Ikram", "https://connects.catalyst.harvard.edu/Profiles/display/Person/79128")
    , ("Leo Gao", "https://scholar.google.com/citations?user=r6mBY50AAAAJ")
    , ("Johanna Kuusisto", "https://scholar.google.com/citations?user=gTixRW8AAAAJ")
    , ("Inga Prokopenko", "https://www.precidiab.org/en/interview/prof-inga-prokopenko/")
    , ("Danish Saleheen", "https://www.columbiacardiology.org/profile/danish-saleheen-mbbs")
    , ("Daniel F. Gudbjartsson", "https://scholar.google.com/citations?user=NzLGjWoAAAAJ")
    , ("Björn Ommer", "https://ommer-lab.com/people/ommer/")
    , ("Andrew D. Morris", "https://www.liverpool.ac.uk/people/andrew-morris")
    , ("Yoichiro Kamatani", "https://www.cbms.k.u-tokyo.ac.jp/en/labs/kamatani.yoichiro/")
    , ("Vedant Misra", "http://vedantmisra.com/")
    , ("Valgerdur Steinthorsdottir", "https://scholar.google.com/citations?user=6Y8_8f4AAAAJ")
    , ("Ulrike Peters", "https://www.fredhutch.org/en/faculty-lab-directory/peters-ulrike.html")
    , ("Yu Sun", "https://yueatsprograms.github.io/")
    , ("Toshiko Tanaka", "https://scholar.google.com/citations?user=_MV1F6kAAAAJ")
    , ("Tim Salimans", "https://research.google/people/106222/")
    , ("Stella Trompet", "https://scholar.google.com/citations?user=LQJFUR8AAAAJ")
    , ("Neil Houlsby", "https://neilhoulsby.github.io/")
    , ("Kilian Q. Weinberger", "https://www.cs.cornell.edu/~kilian/")
    , ("Katri Räikkönen", "https://researchportal.helsinki.fi/en/persons/katri-r%C3%A4ikk%C3%B6nen")
    , ("Kati Kristiansson", "https://finbb.fi/en/board-members")
    , ("Julia Sidorenko", "https://scholar.google.com/citations?user=NADqnegAAAAJ")
    , ("Mike Hadlow", "https://mikehadlow.com/top/about/")
    , ("Michael A. Bishop", "https://artsandsciences.fsu.edu/article/faculty-spotlight-michael-bishop")
    , ("Jianxin Shi", "https://dceg.cancer.gov/about/staff-directory/shi-jianxin")
    , ("J. Wouter Jukema", "https://www.universiteitleiden.nl/en/staffmembers/johan-jukema#tab-2")
    , ("Ilya Loshchilov", "http://www.loshchilov.com/")
    , ("Stuart Hall", "https://x.com/stuartkhall")
    , ("Richard Ngo", "https://80000hours.org/podcast/episodes/richard-ngo-large-language-models/")
    , ("Ethan W. Roland", "https://www.ethan-w-roland.com/")
    , ("Andrew N. Carr", "https://x.com/andrew_n_carr")
    , ("Kyunghyun Cho", "https://www.kyunghyuncho.me/")
    , ("Dzmitry Bahdanau", "https://rizar.github.io/")
    , ("Janus", "https://cyborgism.wiki/hypha/janus")
    , ("Helena Schmidt", "https://forschung.medunigraz.at/fodok/suchen.person_uebersicht?sprache_in=de&menue_id_in=101&id_in=90775539")
    , ("Elisabeth Widen", "https://projects.au.dk/nordic-embl-partnership/show-news/artikel/elisabeth-widen-group-leader-fimm")
    , ("Eli Ayumi Stahl", "https://scholar.google.com/citations?user=EP728cwAAAAJ")
    , ("Eco J. C. de Geus", "https://loop.frontiersin.org/people/163383/bio")
    , ("Charles Kooperberg", "https://www.fredhutch.org/en/faculty-lab-directory/kooperberg-charles.html")
    , ("Carolina Medina-Gomez", "https://www.erasmusmc.nl/en/research/researchers/medina-gomez-carolina#9cd7e534-984d-4144-80cd-ff74cf8d7e98")
    , ("Brenda W. J. H. Penninx", "https://en.wikipedia.org/wiki/Brenda_Penninx")
    , ("Anne E. Justice", "https://www.geisinger.edu/gchs/research/about-gchs-research/find-an-investigator/2018/04/04/13/27/anne-justice")
    , ("Sven Oskarsson", "https://www.uu.se/en/contact-and-organisation/staff?query=N96-2049")
    , ("Sam Greydanus", "https://greydanus.github.io/about_me/")
    , ("Julian Schrittwieser", "https://www.furidamu.org/")
    , ("Stacy Steinberg", "https://scholar.google.com/citations?user=ocNCQbkAAAAJ")
    , ("Sameer Singh", "https://scholar.google.com/citations?user=-hGZC54AAAAJ")
    , ("Ronald de Vlaming", "https://www.devlaming.eu/")
    , ("Preetum Nakkiran", "https://preetum.nakkiran.org/")
    , ("Karen Simonyan", "https://scholar.google.com/citations?user=L7lMQkQAAAAJ")
    , ("Lucas Baker", "https://stanfordirl.com/lucas-baker-bs11")
    , ("Max Lam", "https://www.bicomh.com/max-lam/")
    , ("Maria Dimitriou", "https://scholar.google.com/citations?user=-8Rt-d0AAAAJ")
    , ("Niina Eklund", "https://fi.linkedin.com/in/niina-eklund-747934bb")
    , ("Michiaki Kubo", "https://researchmap.jp/kubom")
    , ("Agus", "https://x.com/austinc3301")
    , ("Meena Kumari", "https://scholar.google.com/citations?user=sl2zlcYAAAAJ")
    , ("Laura J. Scott", "https://sph.umich.edu/faculty-profiles/scott-laura.html")
    , ("Kristian E. Markon", "https://cla.umn.edu/psychology/news-events/story/kristian-markon-elevated-research-associate-professor")
    , ("Jean-Baptiste Pingault", "https://profiles.ucl.ac.uk/42083")
    , ("Jason Wei", "https://www.jasonwei.net/")
    , ("Esben Agerbo", "https://scholar.google.com/citations?user=tAJK3AQAAAAJ")
    , ("Erwin Böttinger", "https://hpims.org/team/erwin-bottinger/")
    , ("Robert Karlsson", "https://ki.se/en/people/robert-karlsson")
    , ("Eric Tang", "https://erictang000.github.io/")
    , ("Aravind Rajeswaran", "https://aravindr93.github.io/")
    , ("Anne Uriu Jackson", "https://research.com/u/anne-u-jackson")
    , ("Andrew Tym Hattersley", "https://en.wikipedia.org/wiki/Andrew_Hattersley")
    , ("Andrea Santilli", "https://www.santilli.xyz/")
    , ("Anders Martin Dale", "https://en.wikipedia.org/wiki/Anders_Dale")
    , ("Alan R. Shuldiner", "https://www.medschool.umaryland.edu/profiles/shuldiner-alan/")
    , ("Aditya A. Ramesh", "https://scholar.google.com/citations?user=60K82BkAAAAJ")
    , ("Matt Sheehan", "https://carnegieendowment.org/people/matt-sheehan")
    , ("Zhuang Liu", "https://liuzhuang13.github.io/")
    , ("Yuhuai Wu", "https://yuhuaiwu.github.io/")
    , ("Yizhong Wang", "https://homes.cs.washington.edu/~yizhongw/")
    , ("Noah A. Smith", "https://nasmith.github.io/")
    , ("Yamini Bansal", "https://scholar.google.com/citations?user=uj1OljkAAAAJ")
    , ("M. E. Thomas", "http://www.sociopathworld.com/")
    , ("Tripp Mickle", "https://www.nytimes.com/by/tripp-mickle")
    , ("Thomas W. Winkler", "https://www.uni-regensburg.de/medizin/epidemiologie-praeventivmedizin/genetische-epidemiologie")
    , ("Thomas Meitinger", "https://scholar.google.com/citations?user=3Nf1sXQAAAAJ")
    , ("Benjamin Todd", "https://benjamintodd.org/#about")
    , ("Samuel Marks", "https://scholar.google.com/citations?user=fW7yK10AAAAJ")
    , ("Nick Patterson", "https://en.wikipedia.org/wiki/Nick_Patterson_(scientist)")
    , ("Sonja I. Berndt", "https://en.wikipedia.org/wiki/Sonja_Berndt")
    , ("Satu Männistö", "https://scholar.google.com/citations?user=BaGYiQQAAAAJ")
    , ("Peter R. Schofield", "https://neura.edu.au/researchers/prof-peter-schofield")
    , ("Nicholas A. Furlotte", "https://scholar.google.com/citations?user=Q0EQvCEAAAAJ")
    , ("Melissa E. Garcia", "https://www.researchgate.net/profile/Melissa-Garcia-43")
    , ("Mary F. Feitosa", "https://scholar.google.com/citations?user=OpNQ188AAAAJ")
    , ("Maciej Trzaskowski", "https://scholar.google.com/citations?user=PcnCBM8AAAAJ")
    , ("Paul Graham", "https://en.wikipedia.org/wiki/Paul_Graham_(programmer)")
    , ("Tero Karras", "https://research.nvidia.com/person/tero-karras")
    , ("Timo Aila", "https://scholar.google.com/citations?user=e7abmgkAAAAJ")
    , ("Samuli Laine", "https://users.aalto.fi/~laines9/")
    , ("Li Dong", "https://scholar.google.com/citations?user=wEfQgPgAAAAJ")
    , ("ChatGPT-4 o1-pro", "https://openai.com/index/introducing-chatgpt-pro/")
    , ("Brian Hayes", "https://en.wikipedia.org/wiki/Brian_Hayes_(scientist)")
    , ("Nat McAleese", "https://scholar.google.com/citations?user=crw6TeIAAAAJ")
    , ("Kevin Lu", "https://kevinlu.ai/")
    , ("Jonathan Berant", "https://www.cs.tau.ac.il//~joberant/")
    , ("John McKinley Chambers", "https://en.wikipedia.org/wiki/John_Chambers_(statistician)")
    , ("John C. Chambers", "https://scholar.google.com/citations?user=FAt7WY0AAAAJ")
    , ("Joel Lehman", "https://scholar.google.com/citations?user=GcvxHWQAAAAJ")
    , ("John Joseph McGrath", "https://scholar.google.com/citations?user=KR7K-XEAAAAJ")
    , ("Inês Barroso", "https://www.mrc-epid.cam.ac.uk/cds2024/faculty/ines-barroso/")
    , ("Ingrid B. Borecki", "https://scholar.google.com/scholar?q=Ingrid%20B.%20Borecki")
    , ("Hinrich Schütze", "https://scholar.google.com/citations?user=qIL9dWUAAAAJ")
    , ("Harald Grallert", "https://research.com/u/harald-grallert")
    , ("USPTO", "https://en.wikipedia.org/wiki/United_States_Patent_and_Trademark_Office")
    , ("George McMahon", "https://scholar.google.com/scholar?q=George+McMahon+genetics")
    , ("George Kirov", "https://profiles.cardiff.ac.uk/staff/kirov")
    , ("Gabriel Goh", "https://scholar.google.com/citations?user=2O3IZlkAAAAJ")
    , ("David Ha", "https://otoro.net/ml/")
    , ("Zhengdong Wang", "https://zhengdongwang.com/")
    , ("Quintin Pope", "https://www.lesswrong.com/users/quintin-pope")
    , ("David A. Bennett", "https://en.wikipedia.org/wiki/David_Bennett_(neurologist)")
    , ("Christopher Potts", "https://scholar.google.com/citations?user=3j08YoAAAAAJ")
    , ("Caglar Gulcehre", "https://www.caglarg.com/")
    , ("Beate St Pourcain", "https://scholar.google.com/citations?user=anzvUNIAAAAJ")
    , ("Eva Krapohl", "https://scholar.google.com/citations?user=MmKl9xsAAAAJ")
    , ("Alfred C. Raphelson", "https://obits.mlive.com/us/obituaries/annarbor/name/alfred-raphelson-obituary?id=8005875")
    , ("Yingchang Lu", "https://wag.app.vanderbilt.edu/PublicPage/Faculty/Details/43993")
    , ("Thomas G. Schulze", "https://scholar.google.com/citations?user=cpKQ8MAAAAAJ")
    , ("Yada Pruksachatkun", "https://scholar.google.com/citations?user=j_cqbm4AAAAJ")
    , ("Xiuqing Guo", "https://lundquist.org/xiuqing-guo-phd")
    , ("Brad Smith", "https://en.wikipedia.org/wiki/Brad_Smith_(American_lawyer)")
    , ("Ryan Teehan", "https://scholar.google.com/citations?user=mXo4IYQAAAAJ")
    , ("Robbee Wedow", "https://www.robbeewedow.com/")
    , ("Rewon Child", "https://rewonc.github.io/")
    , ("Raymond K. Walters", "https://psychology.nd.edu/assets/45969/")
    , ("Xe Iaso", "https://xeiaso.net/")
    , ("David Crawshaw", "https://crawshaw.io/")
    , ("Kate Moran", "https://www.nngroup.com/people/kate-moran/")
    , ("Prafulla Dhariwal", "https://prafulladhariwal.com/")
    , ("Patrick Sulem", "https://scholar.google.com/citations?user=7SUBeRMAAAAJ")
    , ("Nicolas Christin", "https://www.andrew.cmu.edu/user/nicolasc/")
    , ("Pablo V. Gejman", "https://www.emedevents.com/speaker-profile/pablo-v-gejman")
    , ("Narelle K. Hansell", "https://scholar.google.com/citations?user=89Ok-JUAAAAJ")
    , ("Mike Isaac", "https://www.nytimes.com/by/mike-isaac")
    , ("Lawrence F. Bielak", "https://sph.umich.edu/faculty-profiles/bielak-lawrence.html")
    , ("Kaili Rimfeld", "https://pure.royalholloway.ac.uk/en/persons/kaili-rimfeld")
    , ("Will DePue", "https://depue.notion.site/Will-DePue-8b7af67a213e429b91134d0529b6dfa7")
    , ("Amanda Askell", "https://askell.io/")
    , ("Jun Wang", "https://scholar.google.com/citations?user=wIE1tY4AAAAJ")
    , ("Jun Wang#genetics", "https://en.wikipedia.org/wiki/Wang_Jun_(scientist)")
    , ("Judith M. Vonk", "https://www.rug.nl/umcg/research/departments/epidemiology/staff/judith-vonk")
    , ("Juan Pablo Frias", "https://scholar.google.com/citations?user=GPOxOFkAAAAJ")
    , ("Jason Yosinski", "https://yosinski.com/")
    , ("Joel Veness", "https://scholar.google.co.uk/citations?user=_iYrAxEAAAAJ")
    , ("Chris Lakin", "https://chrislakin.blog/about")
    , ("Rupesh Kumar Srivastava", "https://rupeshks.cc/")
    , ("Hilary Kiyo Finucane", "https://en.wikipedia.org/wiki/Hilary_Finucane")
    , ("Henrik Larsson", "https://scholar.google.com/citations?user=jlcdEbUAAAAJ")
    , ("Henrik Grönberg", "https://ki.se/en/people/henrik-gronberg")
    , ("Gibran Hemani", "https://www.explodecomputer.com/about/")
    , ("Nikkin", "https://nikkin.dev/about.html")
    , ("John Tromp", "https://tromp.github.io/")
    , ("Douglas Summers-Stay", "https://scholar.google.com/citations?user=Q2JCPAkAAAAJ&view_op=list_works&sortby=pubdate")
    , ("Robert Yang", "https://debacle.us/")
    , ("rife", "https://www.lesswrong.com/users/rife")
    , ("Elizabeth G. Holliday", "https://scholar.google.com/citations?user=DUIE0sQAAAAJ")
    , ("Devin M. Absher", "https://researchbank.kaiserpermanente.org/our_team/devin-absher-phd/")
    , ("Tim Rocktäschel", "https://rockt.ai/")
    , ("Li-Jia Li", "https://scholar.google.com/citations?user=feX1fWAAAAAJ")
    , ("Wei Dong", "https://scholar.google.com/citations?user=8T5VDv8AAAAJ")
    , ("Daniel Khashabi", "https://danielkhashabi.com/")
    , ("Sam", "https://x.com/420_gunna")
    , ("Danbooru Community", "https://danbooru.donmai.us/wiki_pages/help:home")
    , ("Christiaan de Leeuw", "https://www.amsterdamumc.org/en/research/researchers/christiaan-de-leeuw.htm")
    , ("Charles Blundell", "https://scholar.google.com/citations?user=f31mvPsAAAAJ")
    , ("Bjarke Feenstra", "https://en.ssi.dk/about-us/contact/employees/b/bjarke-feenstra-fee")
    , ("DeepSeek", "https://www.deepseek.com/")
    , ("Nayuki", "https://www.nayuki.io/page/about")
    , ("Aella", "https://x.com/aella_girl")
    , ("Fernando Borretti", "https://borretti.me/about/")
    , ("Xavier Marquez", "http://abandonedfootnotes.blogspot.com/")
    , ("Nelson Elhage", "https://nelhage.com/")
    , ("Ege Erdil", "https://www.lesswrong.com/users/ege-erdil")
    , ("Anke Tönjes", "https://orcid.org/0009-0005-1944-1812")
    , ("Tineke Oldehinkel", "https://scholar.google.com/citations?user=58FENLIAAAAJ")
    , ("Albertine J. Oldehinkel", "https://orcid.org/0000-0003-3925-3913")
    , ("Trishala Neeraj", "https://trishalaneeraj.github.io/about/")
    , ("Timo Schick", "https://scholar.google.com/citations?user=k8CKy5UAAAAJ")
    , ("Per Hoffmann", "https://www.profiles-ngs-cn.uni-bonn.de/display/39922")
    , ("Olav B. Smeland", "https://www.ous-research.no/home/psychosis/Group+members/17880")
    , ("James T. R. Walters", "https://profiles.cardiff.ac.uk/staff/waltersjt")
    , ("Valentina Escott-Price", "https://www.ukdri.ac.uk/team/valentina-escott-price")
    , ("Timo A. Lakka", "https://uefconnect.uef.fi/en/timo.lakka/")
    , ("Thore Graepel", "https://thoregraepel.github.io/")
    , ("Steven A. McCarroll", "https://www.broadinstitute.org/bios/steve-mccarroll")
    , ("Stephanie H. Witt", "https://scholar.google.com/citations?user=n8HwFXQAAAAJ")
    , ("Renato Polimanti", "https://scholar.google.com/citations?user=bDHMgDYAAAAJ")
    , ("Rainer Rauramaa", "https://research.com/u/rainer-rauramaa")
    , ("Peter Welinder", "https://scholar.google.com/citations?user=B76dD7YAAAAJ")
    , ("Peter Lichtner", "https://scholar.google.com/citations?user=eir6jPcAAAAJ")
    , ("Paul M. Thompson", "https://en.wikipedia.org/wiki/Paul_Thompson_(neuroscientist)")
    , ("Patricia A. Peyser", "https://scholar.google.com/scholar?q=Patricia%20A.%20Peyser")
    , ("Joshua S. Gans", "https://en.wikipedia.org/wiki/Joshua_Gans")
    , ("Joshua Clymer", "https://x.com/joshua_clymer")
    , ("Tom Everitt", "https://www.tomeveritt.se/")
    , ("Nathan E. Sanders", "https://projects.iq.harvard.edu/climatefellowship/people/nathan-e-sanders")
    , ("Pamela Mishkin", "https://scholar.google.com/citations?user=fzLxuAIAAAAJ")
    , ("Noah Constant", "https://scholar.google.com/citations?user=PbgcS6AAAAAJ")
    , ("Michael Stumvoll", "https://www.helmholtz-munich.de/en/hi-mag/pi/michael-stumvoll")
    , ("Manan Dey", "https://scholar.google.com/citations?user=39CsfP8AAAAJ")
    , ("Lynda M. Rose", "https://research.com/u/lynda-m-rose")
    , ("Julien Bryois", "https://scholar.google.com/citations?user=6EAqNCQAAAAJ")
    , ("Armand Joulin", "https://scholar.google.com/citations?user=kRJkDakAAAAJ")
    , ("Jackson Kernion", "https://jacksonkernion.com/")
    , ("James L. Kennedy", "https://scholar.google.com/citations?user=L8B57mIAAAAJ")
    , ("Jeffrey Wu", "https://scholar.google.com/citations?user=x4JAvwMAAAAJ")
    , ("Jos Rozen", "https://scholar.google.com/citations?user=OxEDKogAAAAJ")
    , ("Dan Rhatigan", "https://ultrasparky.org/blog/")
    , ("Ina Giegling", "https://research.com/u/ina-giegling")
    , ("Ildiko Lingvay", "https://profiles.utsouthwestern.edu/profile/55880/ildiko-lingvay.html")
    , ("Gal Kaplun", "https://www.galkaplun.com/")
    , ("Frank Geller", "https://scholargps.com/scholars/75086406967602/frank-geller")
    , ("Sydney A. Asdell", "https://ecommons.cornell.edu/items/52ad998f-6ad3-485f-93ea-899a0a40fe64")
    , ("Ethan Perez", "https://ethanperez.net/")
    , ("Ethan Dyer", "https://scholar.google.com/citations?user=LWeVRdUAAAAJ")
    , ("Debajyoti Datta", "https://scholar.google.com/citations?user=L6lx408AAAAJ")
    , ("Emma C. Johnson", "https://www.emmacjohnson.com/")
    , ("Hunt Allcott", "https://scholar.google.com/citations?user=Y1f2unMAAAAJ")
    , ("Andrew Francl", "https://scholar.google.com/citations?user=u8APuDkAAAAJ")
    , ("Josh McDermott", "https://web.mit.edu/jhm/www/")
    , ("Tamay Besiroglu", "http://tamaybesiroglu.com/")
    , ("Ken Bertagnolli", "https://www.47g.org/people/ken-bertagnolli/")
    , ("Wilbur H. Highleyman", "https://www.argmin.net/p/revisiting-highleymans-data")
    , ("David C. M. Liewald", "https://scholar.google.com/citations?user=ahaYiFMAAAAJ")
    , ("Caroline M. Nievergelt", "https://profiles.ucsd.edu/caroline.nievergelt")
    , ("Akul Arora", "https://scholar.google.com/citations?user=zjFWUY0AAAAJ")
    , ("Adam Edward Locke", "https://scholar.google.com/citations?user=5ktn7rsAAAAJ")
    , ("Zhiyuan Liu", "https://nlp.csai.tsinghua.edu.cn/~lzy/")
    , ("Yuntao Bai", "https://scholar.google.com/citations?user=r7GUEVsAAAAJ")
    , ("William Fedus", "https://scholar.google.com/citations?user=-ZfwQOkAAAAJ")
    , ("Tristan Yang", "https://scholar.google.com/scholar?q=Tristan%20Yang")
    , ("Derya Unutmaz", "https://scholar.google.com/citations?user=aND7Gh0AAAAJ")
    , ("Daniel Cook", "https://lostgarden.com/worth-reading/")
    , ("Tengyu Ma", "https://ai.stanford.edu/~tengyuma/")
    , ("Swaroop Mishra", "https://swarooprm.github.io/")
    , ("Steven Gazal", "https://gazal-lab.org/lab-members")
    , ("0xType", "https://github.com/0xType")
    , ("RiversHaveWings", "https://github.com/crowsonkb")
    , ("Riley Goodside", "https://twitter.com/goodside")
    , ("Stefan Gustafsson", "https://research.com/u/stefan-gustafsson")
    , ("MichelangEmoji Bot", "https://x.com/somnai_dreams")
    , ("Daniel Severo", "https://github.com/dsevero")
    , ("Ryo Nakamura", "https://x.com/_Ryobot")
    , ("3D_DLW", "https://x.com/3D_DLW")
    , ("foobar", "https://0xfoobar.substack.com/")
    , ("Michael", "https://x.com/_JeanLannes")
    , ("Aixile", "https://x.com/_aixile")
    , ("Nikhil Gupta", "https://www.linkedin.com/in/gnikhil")
    , ("Gloria Chang", "https://www.linkedin.com/in/gloria-chang")
    , ("Sean Wharton", "https://scholar.google.com/citations?user=4C3Th6oAAAAJ")
    , ("Scott Gray", "https://scholar.google.com/citations?user=sRId4vsAAAAJ")
    , ("Sarah E. Bergen", "https://ki.se/en/people/sarah-bergen")
    , ("Mike Stay", "https://math.ucr.edu/~mike/")
    , ("Bent Petersen", "https://researchprofiles.ku.dk/en/persons/bent-petersen")
    , ("Bryne Hobart", "https://www.thediff.co/")
    , ("Peter A. Holmans", "https://scholar.google.com/citations?user=mOQF3UUAAAAJ")
    , ("Patricia B. Munroe", "https://www.qmul.ac.uk/whri/people/academic-staff/items/munroepatricia.html")
    , ("Pamela A. F. Madden", "https://psychiatry.wustl.edu/people/pamela-madden-phd/")
    , ("Ozren Polašek", "https://scholar.google.com/citations?user=d02gBZQAAAAJ")
    , ("Tim Taylor", "https://www.tim-taylor.com/about/")
    , ("Colm Massey", "https://scholar.google.com/scholar?q=Colm%20Massey")
    , ("Aaron Bergman", "https://x.com/AaronBergman18")
    , ("Lisha Li", "https://x.com/lishali88")
    , ("Yuandong Tian", "https://yuandong-tian.com/")
    , ("Niek Verweij", "https://scholar.google.com/citations?user=RZ0gMYoAAAAJ")
    , ("Mitchell Wortsman", "https://mitchellnw.github.io/")
    , ("Mikael Landén", "https://ki.se/en/people/mikael-landen")
    , ("Michael Tschannen", "https://mitscha.github.io/")
    , ("Michael A. Woodley", "https://scholar.google.com/citations?user=mmoY0-kAAAAJ")
    , ("Mark J. Caulfield", "https://www.qmul.ac.uk/whri/people/academic-staff/items/caulfieldmark.html")
    , ("Mark Daly", "https://en.wikipedia.org/wiki/Mark_Daly_(scientist)")
    , ("Maosong Sun", "https://www.cs.tsinghua.edu.cn/csen/info/1312/4394.htm")
    , ("Lili Chen", "https://www.lilichen.me/")
    , ("Laurent Sifre", "https://scholar.google.com/citations?user=0kVh58wAAAAJ")
    , ("John Bradshaw", "https://web.archive.org/web/20190623040230/http://www.bris.ac.uk/vetscience/people/john-w-bradshaw/index.html")
    , ("Joyce Ehrlinger", "https://scholar.google.com/citations?user=m3ORNxEAAAAJ")
    , ("Iris M. Heid", "https://www.uni-regensburg.de/medizin/epidemiologie-praeventivmedizin/genetische-epidemiologie/unser-team/profdririsheid/index.html")
    , ("Hill F. Ip", "https://scholar.google.com/scholar?q=Hill%20F.%20Ip")
    , ("Heather M. Stringham", "https://scholar.google.com/citations?user=wCvT4tMAAAAJ")
    , ("Georg Homuth", "https://research.com/u/georg-homuth")
    , ("Curiositry", "https://www.autodidacts.io/about/")
    , ("Frank J. A. van Rooij", "https://orcid.org/0000-0002-8600-9852")
    , ("Francesco Cucca", "https://irgb.cnr.it/people/francesco-cucca/")
    , ("Dhruv Batra", "https://dhruvbatra.com/")
    , ("Denny Zhou", "https://dennyzhou.github.io/")
    , ("Katelyn Brown", "https://scholar.google.com/citations?user=8vR9KJ8AAAAJ")
    , ("Deep Ganguli", "https://dganguli.github.io/pweb/")
    , ("Christopher A. Haiman", "https://keck.usc.edu/faculty-search/christopher-haiman/")
    , ("Catharina A. Hartman", "https://scholar.google.com/citations?user=tNp9BUsAAAAJ")
    , ("Bryan Catanzaro", "https://scholar.google.com/citations?user=UZ6kI2AAAAAJ")
    , ("Bernhard T. Baune", "https://www.uni-muenster.de/OCCMuenster/members/bernhard-baune.html")
    , ("Barbara Franke", "https://www.ru.nl/en/people/franke-b")
    , ("Anna A. E. Vinkhuyzen", "https://scholar.google.com/citations?user=CLXR124AAAAJ")
    , ("Adam Santoro", "https://scholar.google.com/citations?user=evIkDWoAAAAJ")
    , ("Mun Hou Won", "https://blog.munhou.com/")
    , ("Patrick McKenzie", "https://www.kalzumeus.com/start-here-if-youre-new/")
    , ("Nicolas Toper", "https://www.linkedin.com/in/nicolas/")
    , ("David Hugh-Jones", "https://wyclif.substack.com/about")
    , ("Amit Anand", "https://scholar.google.com/citations?user=tC0Q9doAAAAJ")
    , ("Adam Roberts", "https://scholar.google.com/citations?user=U5UpKq8AAAAJ")
    , ("Xiaohua Zhai", "https://sites.google.com/view/xzhai")
    , ("Xiaogang Wang", "https://scholar.google.com/citations?user=-B5JgjsAAAAJ")
    , ("Tom B. Brown", "https://scholar.google.com/citations?user=RLvsC94AAAAJ")
    , ("Thomas D. Als", "https://scholar.google.com/citations?user=X-SsyNoAAAAJ")
    , ("Susanne Lucae", "https://pure.mpg.de/cone/persons/resource/persons80426")
    , ("Seena Fazel", "https://www.psych.ox.ac.uk/team/seena-fazel")
    , ("Saskia P. Hagenaars", "https://scholar.google.com/citations?user=urYMTkYAAAAJ")
    , ("R. S. Richardson", "https://en.wikipedia.org/wiki/Robert_S._Richardson")
    , ("Sara Hägg", "https://ki.se/en/people/sara-hagg")
    , ("Rosanne Liu", "https://rosanneliu.com/")
    , ("Pim van der Harst", "https://research.umcutrecht.nl/researchers/pim-van-der-harst/")
    , ("Philip R. Jansen", "https://drphilipjansen.com/index.html")
    , ("Oleg Kiselyov", "https://okmij.org/ftp/")
    , ("Orhan Firat", "https://orhanfirat.com/")
    , ("Noam Brown", "https://scholar.google.com/citations?user=RLDbLcUAAAAJ")
    , ("Niklas Muennighoff", "https://muennighoff.github.io/")
    , ("nearcyan", "https://near.blog/")
    , ("Piotr Stanczyk", "https://scholar.google.com/citations?user=fKVK0dYAAAAJ")
    , ("Nathan A. Gillespie", "https://vipbg.vcu.edu/people/nathan-gillespie/")
    , ("Matt Lakeman", "https://mattlakeman.org/")
    , ("Marian L. Hamshere", "https://profiles.cardiff.ac.uk/staff/hamshereml")
    , ("Loic Le Marchand", "https://www.uhcancercenter.org/le-marchand-loic")
    , ("Michael Hippke", "https://www.lesswrong.com/users/hippke")
    , ("Aaron Gokaslan", "https://x.com/SkyLi0n")
    , ("Konstantin Strauch", "https://www.unimedizin-mainz.de/imbei/en/biometrie-genomische-statistik-und-bioinformatik/mitarbeiter/prof-dr-rer-nat-konstantin-strauch.html")
    , ("Klaus Berger", "https://research.com/u/klaus-berger")
    , ("Juha Sinisalo", "https://researchportal.helsinki.fi/en/persons/juha-sinisalo")
    , ("John R. Attia", "https://www.newcastle.edu.au/profile/john-attia")
    , ("Andrew Gordon Wilson", "https://cims.nyu.edu/~andrewgw/")
    , ("Joel Z. Leibo", "https://www.jzleibo.com/")
    , ("Jaspal S. Kooner", "https://scholar.google.com/scholar?q=Jaspal%20S.%20Kooner")
    , ("Ivana Kolcic", "https://scholar.google.com/citations?user=K3xvHdAAAAAJ")
    , ("Hado van Hasselt", "https://scholar.google.com/citations?user=W80oBMkAAAAJ")
    , ("Graham Neubig", "https://www.phontron.com/")
    , ("Geoffrey Irving", "https://naml.us/")
    , ("Gregory Park", "https://gregpark.io/")
    , ("William Aspray", "https://ethw.org/Oral-History:William_Aspray")
    , ("Frank Bellivier", "https://scholar.google.com/scholar?q=Frank%20Bellivier")
    , ("David C. Glahn" ,"https://research.childrenshospital.org/researchers/david-glahn")
    , ("Bertram Müller-Myhsok", "https://scholar.google.com/citations?user=tSFUTXwAAAAJ")
    , ("Bernhard Otto Boehm", "https://www.ntu.edu.sg/medicine/news-events/magazines-and-newsletters/the-lkcmedicine-february-2019/profile-meet-prof-bernhard-boehm-more-ammunition-to-fight-diabetes")
    , ("Behrooz Z. Alizadeh", "https://www.rug.nl/umcg/research/departments/epidemiology/staff/behrooz-alizadeh")
    , ("Barbara McKnight", "https://www.biostat.washington.edu/people/barbara-mcknight")
    , ("Augustus Odena", "https://www.augustusodena.com/")
    , ("Ashish Sabharwal", "https://www.cs.cornell.edu/~sabhar/")
    , ("Olin Shivers", "https://www.ccs.neu.edu/home/shivers/")
    , ("Stephen A. Ross", "https://en.wikipedia.org/wiki/Stephen_Ross_(economist)")
    , ("Alena Stančáková", "https://www.upjs.sk/en/LF/employee/alena.yaluri/")
    , ("Aki S. Havulinna", "https://scholar.google.com/citations?user=7_6uXDQAAAAJ")
    , ("Aitor Lewkowycz", "https://scholar.google.com/citations?user=Yum1ah0AAAAJ")
    , ("Adam Lerer", "https://adamlerer.github.io/")
    , ("Alejandro Cuevas", "https://alejandrocuevas.me/")
    , ("Jeff Duntemann", "http://www.duntemann.com/whoiam.htm")
    , ("Zaid Alyafeai","https://zaidalyafeai.github.io/")
    , ("Yonghui Wu", "https://scholar.google.com/citations?user=bMWMlk4AAAAJ")
    , ("Yoav Goldberg", "https://scholar.google.com/citations?user=0rskDKgAAAAJ")
    , ("Åsa Johansson", "https://www.uu.se/en/contact-and-organisation/staff?query=N1-979")
    , ("John Quan", "https://scholar.google.com/citations?user=rcojd6kAAAAJ")
    , ("Mark Riedl", "https://eilab.gatech.edu/mark-riedl.html")
    , ("Yarin Gal", "https://www.cs.ox.ac.uk/people/yarin.gal/website/")
    , ("Wouter J. Peyrot", "https://cncr.nl/people/wouter_peyrot/")
    , ("Wendy L. McArdle", "https://scholargps.com/scholars/35381776723079/wendy-l-mcardle")
    , ("Wei Li", "https://scholar.google.com/citations?user=7IUCbE4AAAAJ")
    , ("Peter T. Leeson", "https://en.wikipedia.org/wiki/Peter_Leeson")
    , ("eigenrobot", "https://x.com/eigenrobot")
    , ("Sarah Meiklejohn", "https://smeiklej.com/")
    , ("Locus", "https://en.wikipedia.org/wiki/Locus_(magazine)")
    , ("Brian O. Bernstein", "https://scholar.google.com/citations?user=tI-kMoQAAAAJ")
    , ("Prolific", "https://www.prolific.com/")
    , ("Diederik P. Kingma", "https://dpkingma.com/")
    , ("Nicholas Rougeux", "https://www.c82.net/about/")
    , ("Ulf de Faire", "https://ki.se/en/people/ulf-defaire")
    , ("Ulf Gyllensten", "https://www.uu.se/en/contact-and-organisation/staff?query=N96-4216")
    , ("Thorgeir E. Thorgeirsson", "https://scholar.google.com/citations?user=xpNtpxcAAAAJ")
    , ("Takeshi Terao", "https://scholar.google.com/citations?user=JnudCy0AAAAJ")
    , ("Nicholas Ptacek", "https://nptacek.medium.com/content-archive-for-nptacek-ec39ba2d6465")
    , ("Karin J. H. Verweij", "https://www.geneticsnetworkamsterdam.org/personal-pages/personal-pages-karin-verweij/")
    , ("Susan M. Ring", "https://research-information.bris.ac.uk/en/persons/susan-m-ring")
    , ("Stephen S. Rich", "https://med.virginia.edu/faculty/faculty-listing/ssr4n/")
    , ("Shixiang Shane Gu", "https://sites.google.com/view/gugurus/home")
    , ("Thomas D. Clareson", "https://sf-encyclopedia.com/entry/clareson_thomas_d")
    , ("Cee", "https://x.com/_ceee_")
    , ("Pierre Fontanillas", "https://scholar.google.com/citations?user=J0PyHOkAAAAJ")
    , ("Phillip Isola", "https://web.mit.edu/phillipi/")
    , ("Philip S. Dale", "https://shs.unm.edu/people/faculty/docs/cv-dale-philip-2016-12.pdf")
    , ("Yuxi Liu", "https://yuxi-liu-wired.github.io/")
    , ("Lee N. Robbins", "https://en.wikipedia.org/wiki/Lee_Robins")
    , ("Eli Dourado", "https://www.thecgo.org/people/eli-dourado/")
    , ("Tom Szilagyi", "https://tomscii.sig7.se/id/")
    , ("AKR", "https://x.com/kajaykumarr_")
    , ("wh", "https://x.com/nrehiew_")
    , ("Thomas Pornin", "https://dblp.org/pid/07/4667.html")
    , ("Hastings Greer", "https://www.lesswrong.com/users/hastings-greer")
    , ("Mark Horowitz#sociology", "https://www.shu.edu/profiles/markhorowitz.html")
    , ("Neil R. Robertson", "https://scholar.google.com/scholar?q=Neil%20R.%20Robertson")
    , ("Matthew R. Robinson", "https://scholar.google.com/citations?user=meuM090AAAAJ")
    , ("Lucia A. Hindorff", "https://loop.frontiersin.org/people/1008847/bio")
    , ("Peter P. Pramstaller", "https://www.eurac.edu/en/people/peter-p-pramstaller")
    , ("Joakim Dahlqvist", "https://piminski.com/")
    , ("Weider D. Yu", "https://www.sjsu.edu/engineering/docs/cv/cv_yu_weider_d.pdf")
    , ("Kent M. Pitman", "https://en.wikipedia.org/wiki/Kent_Pitman")
    , ("Jon Billing", "https://www.bigsandwoodworking.com/about/")
    , ("Stefano Marinelli", "https://my-notes.dragas.net/about/")
    , ("Elliot Richards", "https://www.maizegenetics.net/elliotrichards")
    , ("Louise S. Bicknell", "https://www.otago.ac.nz/dsm-pathology/people/profile?id=2236")
    , ("Krystal Hu", "https://www.reuters.com/authors/krystal-hu/")
    , ("Lu Qi", "https://en.wikipedia.org/wiki/Lu_Qi_(computer_scientist)")
    , ("Lu Qi#genetics", "https://scholar.google.com/citations?user=iWi5t54AAAAJ")
    , ("Lu Qi#Insta360", "http://luqi.info/")
    , ("Lavinia Paternoster", "https://research-information.bris.ac.uk/en/persons/lavinia-paternoster")
    , ("Laura Jean Bierut", "https://healthbehaviorcenter.wustl.edu/who-we-are/laura-j-bierut-m-d/")
    , ("Raymond F. Jones", "https://www.ebsco.com/research-starters/biography/raymond-f-jones")
    , ("Grover S. Krantz", "https://en.wikipedia.org/wiki/Grover_Krantz")
    , ("Gerald Goldstein", "https://scholar.google.com/scholar?q=Gerald%20Goldstein")
    ]

-- config tests: none, tested via `authorLinkDB` as a whole
authorWpLinkDB :: [T.Text]
authorWpLinkDB =
    ["A. A. Brill", "A. Bradford Hill", "A. C. Littleton", "A. G. W. Cameron", "Russell Impagliazzo"
    ,"A. K. Bera", "A. L. Barker", "A. L. Sadler", "A. Murat Eren"
    ,"A. Tversky", "Aanund Hylland", "Aaron Clauset", "Aaron Cochrane"
    ,"Aaron D. Ames", "Aaron E. Carroll", "Aaron Gordon", "Aaron Isaacs", "Aaron Klein"
    ,"Aaron Reeves", "Aaron Roth", "Aaron Streets", "Abdoulaye Diabate", "Abdul Basit"
    ,"Abdul Waheed", "Abeba Birhane", "Abhijit Banerjee"
    , "Abraham Flexner", "Abraham Loeb", "Abraham Palmer"
    ,"Ada Palmer", "Adam B. Jaffe", "Adam Cifu", "Adam D\8217Angelo", "Adam Frank"
    ,"Adam Gazzaley", "Adam M. Phillippy", "Adam Pearce", "Adam Platt", "Adam Siepel"
    ,"Adam Stein", "Adam Tauman Kalai", "Adam Twardoch", "Adam Zeman"
    ,"Adebowale Adeyemo", "Adeel Malik", "Adele Eskeles Gottfried", "Adi Shamir", "Aditi Rao"
    ,"Aditya Sinha", "Adnan Custovic", "Adolf Loos", "Adolfo Garc\237a", "Adrian Bejan"
    ,"Adrian Campos", "Adrian Cortes", "Adrian E. Raftery", "Adrian Furnham", "Adrian G. Barnett"
    ,"Adrian Liu", "Adrian Raine", "Adrian V. S. Hill", "Adrian Zenz", "Adriana Galv\225n"
    ,"Aggelos Kiayias", "Agnar Helgason", "Agus Salim", "Ahmed Arif"
    ,"Ahmed Elnaggar", "Ahmed Mustafa", "Ahmed Radwan", "Aja Huang", "Ajay Gupta"
    ,"Ajay Sharma", "Ajit Varki", "Ajoy Sarkar", "Akhilesh Jaiswal", "Akira Fujita"
    ,"Akiyoshi Kitaoka", "Akiyuki Nosaka", "Alain Pellet", "Alan Ashworth", "Alan B. Krueger"
    ,"Alan Bellows", "Alan Borning", "Alan Bovik", "Alan C. Evans", "Alan Cooper"
    ,"Alan Donaldson", "Alan J. Auerbach", "Alan J. Gow", "Alan J. Perlis", "Alan James"
    ,"Alan Kay", "Alan Lerner", "Alan Moore", "Alan S. Kaufman", "Alan Schatzberg"
    ,"Alan Sullivan", "Alan Turing", "Alan W. Black", "Alan Yuille", "Albert A. Bartlett"
    ,"Albert Einstein", "Albert J. Stunkard", "Albert Marcet", "Albert O. Hirschman", "Albert Walton"
    ,"Albert Wohlstetter", "Albert Ziegler", "Albert-L\225szl\243 Barab\225si", "Alberto F. Alesina"
    ,"Alcino J. Silva", "Aldert Vrij", "Aldo Rustichini", "Alec Smith", "Alejandro Pardo"
    ,"Alek Sigley", "Aleksei Timofeev", "Alessandra Voena", "Alessandro Liberati", "Alessandro Vespignani"
    ,"Alex Chen", "Alex Goldin", "Alex Graves", "Alex Heath"
    ,"Alex Honnold", "Alex Kacelnik", "Alex Krizhevsky", "Alex Pentland", "Alex R. Piquero"
    ,"Alex Ramirez", "Alex Tabarrok", "Alex de Voogt", "Alexa Beiser", "Alexander Binder"
    ,"Alexander C. Smith", "Alexander Grishin", "Alexander Hart", "Alexander Ljungqvist", "Alexander Long"
    ,"Alexander Lopez", "Alexander Mann", "Alexander Novikov", "Alexander Ogle", "Alexander Ploner"
    ,"Alexander Rich", "Alexander Richards", "Alexander Rives", "Alexander Roberts", "Alexander Rosenberg"
    ,"Alexander Rutherford", "Alexander Spiridonov", "Alexander Weiss", "Alexandra Barratt", "Alexandra Kelly"
    ,"Alexandra Mendes", "Alexandre Pouget", "Alexei A. Efros", "Alexey Kurakin", "Alexis C. Madrigal"
    ,"Alexis Ohanian", "Alfonso Valencia", "Alfred J. Lewy", "Alfred Moore", "Alfredo Ramirez"
    ,"Ali Farhadi", "Ali Ghodsi", "Ali Hassani", "Ali Jadbabaie", "Ali Madani"
    ,"Ali R. Rezai", "Alice Chang", "Alice H. Eagly", "Alice Roberts"
    ,"Alice Stanton", "Alina Stoica", "Alison Goate", "Alison Gopnik", "Alison Kraus"
    ,"Alison M. Goate", "Alkes Price", "All Things Considered", "Allan Rechtschaffen"
    ,"Allen Buchanan", "Allen Downey", "Allen Ginsberg", "Allen Neuringer", "Allen Roush"
    ,"Allen Wang", "Allyn A. Young", "Alon Cohen", "Alon Halevy", "Alon Orlitsky"
    ,"Alvaro Pascual-Leone", "Alvaro Sanchez", "Alvin E. Roth", "Alvin Toffler", "Alvin W. Gouldner"
    ,"Alyssa Panitch", "Alzheimer\8217s Disease Neuroimaging Initiative", "Al\225n Aspuru-Guzik", "Amanda Collins", "Amanda Feilding"
    ,"Amanda Gefter", "Ambrogio Fasoli", "American Medical Association", "American Physiological Society", "American Psychological Association"
    ,"Amit Agarwal", "Amit Lal", "Amit Sheth", "Ammar Al-Chalabi", "Amnon Rapoport"
    ,"Amnon Shashua", "Amos H. Hawley", "Amos Storkey", "Amos Tversky", "Amy Barrett"
    ,"Amy Dawes", "Amy Hauck Newman", "Amy J. Wagers", "Amy Mitchell", "Amy Moore"
    ,"Amy Orben", "Amy Peters", "Amy Price", "Amy Yang", "An Yang"
    ,"Ana Maria Cuervo", "Ana Miranda", "Anat Brunstein Klomek", "Anders D. Børglum"
    ,"Anders Fjell", "Anders Jonsson", "Anders Krogh", "Anders Lundmark", "Anders Sandberg"
    ,"Andre Barreto", "Andre Franke", "Andrea B. Troxel", "Andrea Basso", "Andrea Burns"
    ,"Andrea Crisanti", "Andrea Huber", "Andrea J. Liu", "Andrea L. Thomaz", "Andrea Levy"
    ,"Andrea Montanari", "Andrea Natale", "Andrea Repetto", "Andrea Rusnock", "Andrea Sanchez"
    ,"Andrea Urrutia", "Andreas Arnold", "Andreas Brandst\228tter", "Andreas Buja", "Andreas Demetriou"
    ,"Andreas Kappes", "Andreas Meyer-Lindenberg", "Andreas Wagner", "Andreas Winter", "Andrej Karpathy"
    ,"Andres Metspalu", "Andrew A. Brown", "Andrew Abbott", "Andrew Adamatzky", "Andrew B. Whinston"
    ,"Andrew Baker", "Andrew Berg", "Andrew Bolt", "Andrew Brock", "Andrew C. Heath"
    ,"Andrew Callahan", "Andrew Carroll", "Andrew Childs", "Andrew Cockburn", "Andrew Critch"
    ,"Andrew Crompton", "Andrew D. Gordon", "Andrew D. Huberman", "Andrew D. White", "Andrew Dahl"
    ,"Andrew Dalby", "Andrew Davison", "Andrew Dillin", "Andrew E. Budson", "Andrew Farrell"
    ,"Andrew Ferguson", "Andrew G. Barto", "Andrew G. Clark", "Andrew G. Walder", "Andrew G. White"
    ,"Andrew Gelman", "Andrew Goff", "Andrew Gregory", "Andrew Iwaniuk", "Andrew J. Scott"
    ,"Andrew J. Watson", "Andrew Jackson", "Andrew Keller", "Andrew M. Colman", "Andrew M. Davis"
    ,"Andrew M. McIntosh", "Andrew Marantz", "Andrew Mayne", "Andrew McCallum", "Andrew McNamara"
    ,"Andrew Moravcsik", "Andrew Murphy", "Andrew N. Iwaniuk", "Andrew Ng", "Andrew Odlyzko"
    ,"Andrew Owens", "Andrew O\8217Hagan", "Andrew Prout", "Andrew Rice", "Andrew Scull"
    ,"Andrew Simmons", "Andrew Singleton", "Andrew Sparkes", "Andrew Steptoe", "Andrew Tang"
    ,"Andrew Vaughn", "Andrew Vickers", "Andrew W. Lo", "Andrew Whiten", "Andrew Z. Fire"
    ,"Andrew Zisserman", "Andrey Korotayev", "Andries van Dam", "Andrzej Pajak", "Andr\225s Hajnal"
    ,"Andy Clark", "Andy Cox", "Andy Simmons", "Andy Williams", "Angela Brady"
    ,"Angela L. Duckworth", "Angela N. Brooks", "Angela Page", "Angela Park", "Angela Rose"
    ,"Angelica Ronald", "Angelika Steger", "Angelo Rizzo", "Angie Chen", "Angus Fletcher"
    ,"Angus Trumble", "Anil Madhavapeddy", "Anil Seth", "Anima Anandkumar", "Anita Thapar"
    ,"Anjan Chatterjee", "Ann Bartuska", "Ann Finkbeiner", "Ann L. Brown", "Ann Lee"
    ,"Ann Now\233", "Anna Becker", "Anna Belfer-Cohen", "Anna Cheung", "Anna Docherty"
    ,"Anna Dominiczak", "Anna Frebel", "Anna Grzymala-Busse", "Anna Middleton", "Anna Murray"
    ,"Anna Palmer", "Anna Rogers", "Anna Salamon", "Anna Schmidt", "Anna Sun"
    ,"Anna Sundstr\246m", "Anna Van Meter", "Annamaria Lusardi", "Anne Anastasi", "Anne B. Newman"
    ,"Anne Barton", "Anne Broadbent", "Anne Brunet", "Anne C. Stone", "Anne Carson"
    ,"Anne Case", "Anne Chao", "Anne Cutler", "Anne E. Carpenter", "Anne E. Pusey"
    ,"Anne Farmer", "Anne Harrington", "Anne K. Churchland", "Anne McLaren", "Anne Roe"
    ,"Anne Thomas", "Annette Lee", "Annette Peters", "Antal van den Bosch", "Anthony C. Davison"
    ,"Anthony Chen", "Anthony Francis", "Anthony G. Greenwald", "Anthony J. Bailey", "Anthony Jorm"
    ,"Anthony P. Monaco", "Anthony Trewavas", "Anthony Yeung", "Antoine Merle", "Antoine Roux"
    ,"Antoinette Schoar", "Anton Spiridonov", "Antoni Ribas", "Antonio Cano", "Antonio Damasio"
    ,"Antonio Ferretti", "Anu Realo", "Anubha Mahajan", "Anupam Joshi", "Anya Kamenetz"
    ,"Anya Samek", "Apoorva Khare", "Ara Darzi", "Arang Rhie", "Aravinda Chakravarti"
    ,"Architectural Digest", "Are Holen", "Ari Holtzman", "Arie W. Kruglanski", "Ariel Darvasi"
    ,"Ariel Knafo-Noam", "Ariel Shamir", "Arindrajit Dube", "Arlindo Oliveira", "Armand M. Leroi"
    ,"Armen A. Alchian", "Armin Falk", "Armin Ronacher", "Armin Wei\223", "Arnab Rai Choudhuri"
    ,"Arne Mastekaasa", "Arne Skaug", "Arno Villringer", "Arnold C. Cooper", "Arnold Thackray"
    ,"Arnout van de Rijt", "Arpana Agrawal", "Arren Bar-Even", "Art Pope", "Arthur A. Lumsdaine"
    ,"Arthur Beaudet", "Arthur F. Kramer", "Arthur L. Norberg", "Arthur L. Samuel", "Arthur R. Jensen"
    ,"Arthur W. Toga", "Arun Kumar Gupta", "Arvind Krishnamurthy", "Arvind Mahankali", "Arvind Narayanan"
    ,"Asa Fitch", "Asbj\248rn Hr\243bjartsson", "Ashish Arora", "Ashish Khetan", "Ashish Vaswani"
    ,"Ashish Yadav", "Ashlee Vance", "Ashley Freeman", "Ashley Stewart", "Ashok Goel"
    ,"Asya Rolls", "Athula Sumathipala", "Atsushi Takahashi", "Atul Gawande", "Aubrey Sheiham"
    ,"Aude Oliva", "Audrey Smith", "Augustine Kong", "Auke Tellegen", "Aurelio Jos\233 Figueredo"
    ,"Austan Goolsbee", "Austin Bradford Hill", "Austin Burke", "Austin Myers", "Austin Stone"
    ,"Aviv Regev", "Avshalom Caspi", "Axel D. Becke", "Axel Schmidt", "Ayelet Fishbach"
    ,"Azhar Sultan", "B. Douglas Bernheim", "B. F. Skinner", "B. M. Trost", "B. Timothy Walsh"
    ,"Babak Hassibi", "Babak Hodjat", "Bahad\305r Demir", "Bal\225zs Kov\225cs", "Barbara Davis"
    ,"Barbara Grosz", "Barbara J. Sahakian", "Barbara Maughan", "Barbara Mellers", "Barbara S. Burks"
    ,"Barbara Schneider", "Barbara Stoddard Burks", "Barbara Sullivan", "Barbara Wold", "Barry Cunliffe"
    ,"Barry Horowitz", "Baruch Fischhoff", "Basem Al-Shayeb", "Basil Hetzel", "Beat Keller"
    ,"Beatrice H. Hahn", "Beatrice de Gelder", "Beatriz Luna", "Bei Shi", "Ben Goldacre"
    , "Ben Higgins", "Ben Horowitz", "Ben Laurie", "Ben Rattray"
    ,"Ben Shneiderman", "Ben Silverman", "Benedict Jones", "Benedict Smith", "Benedicto Crespo-Facorro"
    ,"Beng Chin Ooi", "Bengt Holmstr\246m", "Benjamin A. Garcia", "Benjamin A. Olken"
    ,"Benjamin B", "Benjamin B. Lahey", "Benjamin Black", "Benjamin Breen", "Benjamin Djulbegovic"
    ,"Benjamin F. Jones", "Benjamin Goldstein", "Benjamin Graham", "Benjamin Hayden", "Benjamin Mako Hill"
    ,"Benjamin McMahon", "Benjamin M. Neale", "Benjamin S. Bloom", "Benjamin Tang", "Benjamin Williams"
    ,"Benjamin van Niekerk", "Bent Flyvbjerg", "Bernard Zinman", "Bernd Kraemer"
    ,"Bernd Kr\228mer", "Bernd Weber", "Bernd W\252rsig", "Bernhard Ludvik", "Bernhard Nebel"
    ,"Bernhard Sch\246lkopf", "Bernie Devlin", "Bernt Schiele", "Bert H\246lldobler", "Bertram Gilfoyle"
    ,"Bertrand Meyer", "Bessel van der Kolk", "Beth Martin", "Beth Shapiro", "Beth Stevens"
    ,"Beverly Rodriguez", "Bhramar Mukherjee", "Bhuvana Ramabhadran", "Bian Li", "Bilge Ebiri"
    ,"Bill Casselman", "Bill Dally", "Bill Deakin", "Bill Gates", "Bill Hicks"
    ,"Bin Jiang", "Bin Liu", "Bin Xu", "Bin Yu", "Bing Han"
    ,"Bing Zhang", "Blair Braverman", "Blaise Aguera y Arcas", "Blaise Ag\252era y Arcas", "Blake Richards"
    ,"Blake Ross", "Bluma Zeigarnik", "Bo Dai", "Bo Gao", "Bo Li"
    ,"Bo Peng", "Bo Yang", "Bo Zhou", "Boaz Barak", "Boaz Keysar"
    ,"Bob Sturm", "Boban Petrovi\263", "Boehringer Ingelheim", "Bogus\322aw Paw\322owski", "Bokyung Son"
    ,"Bonnie Berger", "Boris Katz", "Botond Roska", "Bowen Zhang", "Brad Leithauser"
    ,"Bradley Efron", "Bradley J. Nelson", "Bradley T. Hyman", "Brandon Carter", "Brandon Hill"
    ,"Brendan Burchell", "Brendan I. Koerner", "Brendan Nyhan", "Brendan O\8217Donoghue", "Brent Graham"
    ,"Brent W. Roberts", "Bret S. Weinstein", "Bret Taylor", "Bret Victor", "Brian A. Jacob"
    ,"Brian Cantwell Smith", "Brian Charlesworth", "Brian Curtis", "Brian D\8217Onofrio", "Brian Ferguson"
    ,"Brian Flanagan", "Brian Fuller", "Brian G. Wowk", "Brian Hare", "Brian Hicks"
    ,"Brian K. Kennedy", "Brian Krebs", "Brian L. Strom", "Brian Levine", "Brian M. D\8217Onofrio"
    ,"Brian Moriarty", "Brian Nosek", "Brian Uzzi", "Brian Vaughan", "Brian Wansink"
    ,"Brian Williams", "Brian Wong", "Brian Wowk", "British Medical Journal", "Broadridge Financial Solutions Inc"
    ,"Bruce Berger", "Bruce Bode", "Bruce Bueno de Mesquita", "Bruce C. Gibb", "Bruce Castle"
    ,"Bruce Charles Heezen", "Bruce E. Wampold", "Bruce F. Pennington", "Bruce G. Charlton", "Bruce J. Ellis"
    ,"Bruce L. McNaughton", "Bruce M. Cohen", "Bruce Pennington", "Bruce R. Rosen", "Bruce S. Weir"
    ,"Bruce Sacerdote", "Bruce Tognazzini", "Bruce Western", "Bruno Pereira", "Bruno S. Frey"
    ,"Bruno Studer", "Bruno de Finetti", "Bryan Gibson", "Bryan J. Traynor", "Bryan Kolb"
    ,"Bryan L. Roth", "Bryony James", "Burkhard Bilger", "Burkhard Rost", "Butler Lampson"
    ,"Butler W. Lampson", "Byeong Chun Lee", "B\233la Bollob\225s", "C. A. B. Smith", "C. A. R. Hoare"
    ,"C. Anandharamakrishnan", "C. Cooper", "C. D. Darlington", "C. H. Turner", "C. H. Waddington"
    ,"C. Radhakrishna Rao", "C. Robert Cloninger", "C. S. Franklin", "C. Scott Baker", "C. Smith"
    ,"C. Sue Carter", "C. W. J. Granger", "C. Wolf", "C. Wyatt Shields IV", "Caitlin Flanagan"
    ,"Caleb Robinson", "Calvin Lo", "Cameron Anderson", "Cameron Jones", "Cameron Miller"
    ,"Camilla Benbow", "Camilla P. Benbow", "Camilla Persson Benbow", "Camilla Stoltenberg", "Camille Landais"
    ,"Can Xu", "Candice Odgers", "Carel Le Roux", "Carel P. van Schaik", "Carl A. Anderson"
    ,"Carl Benedikt Frey", "Carl Feynman", "Carl I. Hovland", "Carl Lieberman", "Carl Sagan"
    ,"Carl Schreck", "Carl Shapiro", "Carl T. Bergstrom", "Carl Zimmer", "Carles Lalueza-Fox"
    ,"Carlos Cruchaga", "Carlos D. Bustamante", "Carlos M. Duarte", "Carlos Ribeiro", "Carmel Moore"
    ,"Carol Brayne", "Carol Chen", "Carol Mills", "Carol S. Dweck", "Carole Lieberman"
    ,"Carolina Lopez", "Caroline Fox", "Caroline Fraser", "Caroline Watt", "Carrick Flynn"
    ,"Carroll L. Wainwright", "Carsen Stringer", "Carsten Pedersen", "Casey B. Mulligan", "Casey Dunn"
    ,"Cass R. Sunstein", "Catharine R. Gale", "Catherine Dulac", "Catherine Jami", "Catherine Marshall"
    ,"Catherine McBride", "Catherine Plaisant", "Catherine Potenski", "Cathleen Schine", "Catholijn Jonker"
    ,"Cathryn Lewis", "Cathryn M. Lewis", "Cathy J. Price", "Cathy King", "Cathy Spatz Widom"
    , "Cathy Wu", "Cecil R. Reynolds", "Cecilia D\8217Anastasio"
    ,"Cecilia Magnusson", "Cees Dekker", "Celeste Kidd", "Celeste Lyn Paul", "Celia Greenwood"
    ,"Cell Press", "Celso Arango", "Chaim Goodman-Strauss", "Chandler Burr", "Chang Jiang"
    ,"Chang Xu", "Chantal Radimilahy", "Chao Agnes Hsiung", "Chao Chen", "Chao Dong"
    ,"Chao Min", "Chao Xu", "Charles A. Czeisler", "Charles A. Taylor", "Charles B. Nemeroff"
    ,"Charles Beattie", "Charles Curtis", "Charles Dickens", "Charles Duhigg", "Charles E. Leiserson"
    ,"Charles E. Osgood", "Charles F. Hockett", "Charles F. Reynolds III", "Charles Fuchs", "Charles G. Gross"
    ,"Charles George Herbermann", "Charles Geschke", "Charles Goodhart", "Charles H. Haskins", "Charles H. Martin"
    ,"Charles Hulme", "Charles I. Jones", "Charles Isbell", "Charles J. Lumsden", "Charles Murray"
    ,"Charles N. Rotimi", "Charles Naylor", "Charles Nicholas", "Charles Ofria", "Charles P. Davis"
    ,"Charles Paul Conn", "Charles Piller", "Charles Poole", "Charles Rotimi", "Charles Spearman"
    ,"Charles Spence", "Charles W. Rees", "Charles ffrench-Constant", "Charlie Manson", "Charlie Snell"
    ,"Charlotta Pisinger", "Charlotte Banks", "Charlotte Blease", "Charlotte Buhler", "Charlotte Drake"
    ,"Charlotte Harrison", "Chelsea Finn", "Chen Hu", "Chen Zhang"
    ,"Cheng Chen", "Cheng Li", "Cheng Xu", "Chengxiang Zhai", "Cheryl Dissanayake"
    ,"Chi Chung Lam", "Chi-Hung Hsu", "Chia-Che Chang", "Chiara Sabatti", "Chicago Urban League"
    ,"Ching Fang", "Chip Heath", "Chirag Jain", "Chirag Patel", "Chris Apps"
    ,"Chris Donahue", "Chris Eliasmith", "Chris Freeland", "Chris Lu", "Chris Newell"
    ,"Chris Offutt", "Chris P. Ponting", "Chris Power", "Chris Short", "Chris Simon"
    ,"Chris Street", "Chris Turney", "Chris Wallace", "Chris Walshaw", "Chris Wilkins"
    ,"Christian Becker", "Christian Bendixen", "Christian Cachin", "Christian Catalini", "Christian Fischer"
    ,"Christian Fong", "Christian Genest", "Christian Hammer", "Christian Roselius", "Christian Schneider"
    ,"Christian Seel", "Christian Sonne", "Christian T. Wentz", "Christian Voigt", "Christian Walder"
    ,"Christian Wirth", "Christina Kim", "Christina Paxson", "Christine E. Seidman", "Christine F. Baes"
    ,"Christine King", "Christine Payne", "Christine Van Broeckhoven", "Christof Koch", "Christoph Adami"
    ,"Christoph Hoerl", "Christoph Meinel", "Christoph Preuss", "Christophe Dessimoz", "Christopher A. Walsh"
    ,"Christopher Alexander", "Christopher B. Field", "Christopher Boehm", "Christopher Boone", "Christopher Chabris"
    ,"Christopher Chen", "Christopher Clark", "Christopher Copeland", "Christopher Cullen", "Christopher D. Gardner"
    ,"Christopher D. Manning", "Christopher F. Chabris", "Christopher Gardner", "Christopher Gillberg", "Christopher Honey"
    ,"Christopher I. Amos", "Christopher J. Ferguson", "Christopher Koch", "Christopher M. Andrew", "Christopher Malloy"
    ,"Christopher Olah", "Christopher O\8217Neill", "Christopher Patrick", "Christopher Pittenger", "Christopher R. Brand"
    ,"Christopher Re", "Christopher R\233", "Christopher Talbot", "Christopher Weber", "Christos Davatzikos"
    ,"Christos H. Papadimitriou", "Christos Kozyrakis", "Christos Pantelis", "Christos Papadimitriou", "Christos S. Mantzoros"
    ,"Christy L. Haynes", "Chu Chen", "Chuan He", "Chun Ye", "Chun Yuan"
    ,"Chunyu Wang", "Churchill Eisenhart", "Cindy Ramirez", "Cisca Wijmenga", "Claire Allen"
    ,"Claire Haworth", "Claire Massey", "Clancy Blair", "Clara Sousa-Silva", "Claude Bouchard"
    ,"Claude Gaillard", "Claude Marcus", "Claude Roux", "Claude Shannon", "Claudi L. H. Bockting"
    ,"Claudia Clopath", "Claudio Stampi", "Claudius Gros", "Claus Lamm", "Clay Shirky"
    ,"Clemens Meyer", "Cliff Arnall", "Cliff Stoll", "Clifford Geertz", "Clinical Psychological Science"
    ,"Cliodhna O\8217Connor", "Clive Ballard", "Clive D. L. Wynne", "Clive Holmes", "Clyde A. Hutchison III"
    ,"Coalition for Evidence-Based Policy", "Cody Wild", "Colin Allen", "Colin Berry", "Colin Burke"
    ,"Colin Camerer", "Colin Cherry", "Colin Drummond", "Colin F. Camerer", "Colin Flaherty"
    ,"Colin G. DeYoung", "Colin Hodgkinson", "Colin Humphreys", "Colin Lankshear", "Colin Mallows"
    ,"Colin Martindale", "Colin Masters", "Colin Mathers", "Colin Palmer", "Colin Skinner"
    ,"Colleen Lawless", "Collin Burns", "Collin Y. Ewald", "Colm McDonald", "Companies House"
    ,"Con Stough", "Cong Han", "Cong Zhou", "Connie Wang", "Constantine G. Lyketsos"
    ,"Cordelia Schmid", "Corey Powell", "Corinna Cortes", "Cormac \211 Gr\225da"
    ,"Cornelia M. Van Duijn", "Cornelia van Duijn", "Cosma Rohilla Shalizi", "Courtney Williams", "Craig Haney"
    ,"Craig Partridge", "Craig Raine", "Craig Ross", "Craig S. Smith", "Cristen Willer"
    ,"Cristobal Morales", "Cristopher Moore", "Csaba P. Kovesdy", "Cuilin Zhang", "Curtis C. Harris"
    ,"Curtis Huttenhower", "Curtis J. Milhaupt", "Curtis LeMay", "Cynthia Dwork", "Cynthia Fisher"
    ,"Cynthia Kenyon", "Cynthia M. Beall", "Cynthia M. Bulik", "Cynthia Mulrow", "Cynthia Rudin"
    ,"Cyril Burt", "Cyril Ponnamperuma", "Cyril Thomas", "Czes\322aw Mi\322osz", "D. C. Rao"
    ,"D. H. Mellor", "D. Johnson", "D. N. Jackson", "D. S. Falconer"
    ,"D. S. Hirschberg", "D. Taylor", "D. W. Fulker", "Dabeeru C. Rao", "Dacheng Tao"
    ,"Dacher Keltner", "Dagomar Degroot", "Daisy Zamora", "Dale Allison", "Dale Webster"
    ,"Dalton Conley", "Damien Broderick", "Damion Searls", "Dan Boneh", "Dan Brown"
    ,"Dan Coates", "Dan Hendrycks", "Dan Hesse", "Dan J. Stein", "Dan Jurafsky"
    ,"Dan Klein", "Dan Liu", "Dan M. Roden", "Dan Mazur", "Dan Roden"
    ,"Dan Roth", "Dan Schmidt", "Dan Schwartz", "Dan Zhang", "Dana Angluin"
    ,"Dana H. Ballard", "Dana H. Born", "Dana Klisanin", "Dana Scott", "Daniel A. Geller"
    ,"Daniel A. Spielman", "Daniel Acuna", "Daniel B. Wright", "Daniel Bates", "Daniel Dennett"
    ,"Daniel Campos", "Daniel D. Johnson", "Daniel Daneshvar", "Daniel Dennett", "Daniel E. Ho"
    ,"Daniel Eriksson", "Daniel Franklin", "Daniel Freeman", "Daniel Fried", "Daniel G. Goldstein"
    ,"Daniel Geschwind", "Daniel Gianola", "Daniel H. Geschwind", "Daniel Hoffman", "Daniel Hsu"
    ,"Daniel I. Rees", "Daniel J. Bauer", "Daniel J. Bernstein", "Daniel J. Carroll", "Daniel J. Drucker"
    ,"Daniel J. Kleitman", "Daniel J. Rader", "Daniel Kahneman", "Daniel Kang", "Daniel Kish"
    ,"Daniel Kokotajlo", "Daniel Kruger", "Daniel L. Schacter", "Daniel L. Schwartz", "Daniel Lehmann"
    ,"Daniel M. Wegner", "Daniel Marcus", "Daniel Munro", "Daniel Nettle", "Daniel O\8217Connell"
    ,"Daniel Peek", "Daniel Promislow", "Daniel R. Weinberger", "Daniel Rader", "Daniel Ritchie"
    ,"Daniel S. Hamermesh", "Daniel S. Weld", "Daniel Sawyer", "Daniel Schultz"
    ,"Daniel Schwartz", "Daniel T. Blumstein", "Daniel T. Gilbert", "Daniel T. Willingham", "Daniel Tranel"
    ,"Daniel Treisman", "Daniel Watson", "Daniel Zhang", "Daniel Ziegler", "Daniela Amodei"
    ,"Daniela Rus", "Danielle Dick", "Danielle M. Dick", "Danielle Posthuma", "Danielle S. McNamara"
    ,"Daniil Pakhomov", "Danny Hernandez", "Danqi Chen", "Danuta Wasserman", "Daphne Bavelier"
    ,"Darby Saxbe", "Daren Liu", "Dario Amodei", "Dario Benedetto", "Dario Maestripieri"
    ,"Dariush Mozaffarian", "Daron Acemoglu", "Darren Platt", "Daryl J. Bem", "Dashun Wang"
    ,"David  A. Steen", "David A. Fidock", "David A. Hughes", "David A. Kenny", "David A. Scheinberg"
    ,"David A. Sinclair", "David A. Turner", "David Ackerman", "David Amar", "David B. Allison"
    ,"David B. Audretsch", "David B. Richman", "David Balding", "David Becker", "David Bieber"
    ,"David Blitz", "David Borwein", "David C. Funder", "David C. Geary"
    ,"David C. McClelland", "David C. Parkes", "David C. Rowe", "David C. Schmittlein", "David Cameron"
    ,"David Card", "David Carmody", "David Cesarini", "David Cock", "David Corley"
    ,"David Cuthbertson", "David D. Friedman", "David D. Kirkpatrick", "David Dale", "David Deutsch"
    ,"David Ding", "David Do", "David Dunning", "David E. Culler", "David E. Harrison"
    ,"David E. Olson", "David E. Rumelhart", "David Ebert", "David Edward Goldberg", "David F. Bjorklund"
    ,"David F. Dinges", "David F. Horrobin", "David Faust", "David Foster Wallace", "David G. Rand"
    ,"David G. Simons", "David G. Stork", "David Gelles", "David Glover", "David Goodstein"
    ,"David Grove", "David H. Autor", "David H. Hackworth", "David H. Ledbetter", "David Hackett"
    ,"David Haden", "David Harker", "David Haussler", "David Heinemeier Hansson", "David Held"
    ,"David Hirshleifer", "David Hsu", "David Hunter", "David I. Laibson", "David I. Stuart"
    ,"David J. Beerling", "David J. C. MacKay", "David J. Chalmers", "David J. Cooper", "David J. Deming"
    ,"David J. Hunter", "David J. Kupfer", "David J. Stevenson", "David J. White", "David Krueger"
    ,"David L. Banks", "David L. Donoho", "David L. Fried", "David L. Hu", "David Laibson"
    ,"David Laitin", "David Lau", "David Lazer", "David Leiser", "David Li"
    ,"David Lobell", "David Lordkipanidze", "David Lowenthal", "David Lubinski", "David M. Blei"
    ,"David M. Buss", "David M. Evans", "David M. Lawson", "David M. Lee", "David M. Markowitz"
    ,"David Maimon", "David Mazi\232res", "David Metcalfe", "David Meyre", "David Moher"
    ,"David Moreau", "David Mowat", "David N. Weil", "David Nutt", "David O. Conover"
    ,"David Ong", "David Owen", "David P. Farrington", "David P. Strachan", "David R. Brillinger"
    ,"David R. Liu", "David R. Moore", "David Reichert", "David Rein", "David Richter"
    ,"David Rios Insua", "David S. Johnson", "David S. Ludwig", "David Salgado", "David Saxon"
    ,"David Schlessinger", "David Sexton", "David Sirlin", "David Smith Calverley", "David Stove"
    ,"David T. Lykken", "David Talbot", "David Tan", "David Thissen", "David Vlahov"
    ,"David Vokrouhlick\253", "David W. Clark", "David W. Craig", "David W. Cushman", "David W. Keith"
    ,"David W. Macdonald", "David W. Tank", "David Wechsler", "David Weitz", "David Wells"
    ,"David Whitney", "David Wisniewski", "David Wu", "David Xiao", "David de la Croix"
    ,"Davide Piffer", "Davide Scaramuzza", "Dawn Song", "Dawson R. Engler", "De Wet Swanepoel"
    ,"Dean Karlan", "Dean Keith Simonton", "Dean Rickles", "Debbie Lawlor", "Debby Herbenick"
    ,"Deborah A. Cobb-Clark", "Deborah A. Lawlor", "Deborah A. Nickerson", "Deborah H. Gruenfeld", "Deborah Jarvis"
    ,"Deborah Lowe Vandell", "Deborah Morgan", "Deborah Phillips", "Deborah Schofield", "Debra J. Skene"
    ,"Dedra Buchwald", "Deepak L. Bhatt", "Deepti Gurdasani", "Deirdre O\8217Brien", "Demis Hassabis"
    ,"Dena G. Hernandez", "Denis Davydov", "Denis Dimitrov", "Denis Tarasov", "Denis Walsh"
    ,"Denise C. Park", "Denise Robinson", "Denise Williams", "Dennis Brooks", "Dennis Coates"
    ,"Dennis Kim", "Dennis M. Levi", "Dennis Schmitt", "Dennis Van der Meer", "Denny Borsboom"
    ,"Denys Ovenden", "Department of Justice", "Depths of Wikipedia", "Derek Abbott", "Derek C. Angus"
    ,"Derek Chen", "Derek Lowe", "Derek Pang", "Derek de Solla Price", "Derk-Jan Dijk"
    ,"Desmond Elliott", "Detlef Weigel", "Devah Pager", "Devavrat Shah", "Devi Parikh"
    ,"Devon Rifkin", "Dharmendra Modha", "Dharmendra S. Modha", "Dharshan Kumaran", "Dian Donnai"
    ,"Diana Fleischman", "Diana Johnson", "Diana O. Perkins", "Diana Reiss", "Diane F. Halpern"
    ,"Diane M. Becker", "Diane Van Deren", "Didier Sornette", "Dieter Ebert", "Dieter Fox"
    ,"Diether Lambrechts", "Dileep George", "Dimitri Bertsekas", "Dimitri P. Bertsekas", "Dimitris Bertsimas"
    ,"Dimitris Metaxas", "Dina Katabi", "Dinei Florencio", "Dinesh Chugtai", "Dirk Bergemann"
    ,"Dirk Bezemer", "Dirk Englund", "Divya Mehta", "Diyi Yang", "Dmitry Akimov"
    ,"Dmitry Kazhdan", "Dmitry Molchanov", "Dobroslav Chrobak", "Doina Precup", "Dolores Albarrac\237n"
    ,"Dolores Malaspina", "Dominic King", "Dominic Masters", "Dominik Tatarka", "Don Curtis"
    ,"Don Syme", "Donald B. Lindsley", "Donald B. Rubin", "Donald F. Klein", "Donald G. Saari"
    ,"Donald Gurnett", "Donald I. Templer", "Donald Katz", "Donald Keene", "Donald Knuth"
    ,"Donald Michie", "Donald P. Green", "Donald R. Miller", "Donald T. Campbell", "Donald W. Black"
    ,"Donald W. Pfaff", "Dong Chen", "Dongju Zhang", "Dongmei Wang", "Donna K. Arnett"
    ,"Dora Akunyili", "Dora L. Costa", "Dorin Comaniciu", "Doris Tsao", "Doris Y. Tsao"
    ,"Dorothy Burlingham", "Dorothy Nevill", "Doug Downey", "Doug Lenat"
    ,"Douglas Blackwood", "Douglas C. Schmidt", "Douglas F. Easton", "Douglas Fraser", "Douglas G. Altman"
    ,"Douglas H. Fisher", "Douglas Hofstadter", "Douglas K. Detterman", "Douglas T. Kenrick"
    ,"Douglas W. Allen", "Dov Cohen", "Dragana Rogulja", "Dragomir Radev", "Drazen Prelec"
    ,"Drew McDermott", "Drew Weissman", "Duncan J. Watts", "Duncan Lawrence", "Duncan Ryuken Williams"
    ,"Dustin Wright", "Dwight Dickinson", "E. B. Titchener", "E. E. Salpeter", "E. L. Lehmann"
    ,"E. M. Purcell", "E. Mavis Hetherington", "E. Paul Torrance", "E. Robinson", "E. S. Pearson"
    ,"E. S. Pondiczery", "E. W. Brown", "Eamon McCrory", "Eamonn Sheridan", "Earl Miner"
    ,"Ed Boyden", "Ed Chi", "Ed H. Chi", "Ed O\8217Brien", "Ed Yong"
    ,"Edmund Fantino", "Edmund Sonuga-Barke", "Edmund T. Rolls", "Edouard Louis", "Edouard Machery"
    ,"Edsger W. Dijkstra", "Eduard Hovy", "Eduard Vieta", "Edvard I. Moser", "Edvard Johansson"
    ,"Edward Ames", "Edward Carmines", "Edward E. Leamer", "Edward E. Smith", "Edward F. Chang"
    ,"Edward F. Moore", "Edward Feigenbaum", "Edward Fredkin", "Edward G. Jones", "Edward G. Seidensticker"
    ,"Edward Gibson", "Edward Giovannucci", "Edward H. Adelson", "Edward J. Larson", "Edward Johns"
    ,"Edward L. Glaeser", "Edward L. Thorndike", "Edward M. Gramlich", "Edward M. Miller", "Edward M. Scolnick"
    ,"Edward Miguel", "Edward Neville da Costa Andrade", "Edward O. Thorp", "Edward P. Lazear", "Edward Pratt"
    ,"Edward Rosen", "Edward S. Boyden", "Edward S. Buckler", "Edward Scolnick", "Edward Slingerland"
    ,"Edward T. Bullmore", "Edward Teller", "Edward Tufte", "Edward W. Felten", "Edward Wong"
    ,"Edward Yang", "Edwin G. Boring", "Edythe London", "Edzard Ernst", "Eero P. Simoncelli"
    ,"Efim Zelmanov", "Efraim Benmelech", "Eileen Roberts", "Eivind Ystr\248m", "Ekaterina Orlova"
    ,"Elad Hazan", "Elaine Wyllie", "Elchanan Mossel", "Eldar Shafir", "Eleanor A. Maguire"
    ,"Eleanor Feingold", "Eleazar Eskin", "Eleftheria Zeggini", "Elena Shumskaya", "Eli Lilly"
    ,"Eli Somer", "Eli Y. Adashi", "Elias G. Carayannis", "Eliezer Yudkowsky", "Eliot A. Cohen"
    ,"Elisabeth West FitzHugh", "Elizabeth A. Fenn", "Elizabeth A. Phelps", "Elizabeth A. Stuart", "Elizabeth A. Thompson"
    ,"Elizabeth Barnes", "Elizabeth Bates", "Elizabeth Dunne", "Elizabeth F. Loftus", "Elizabeth Gaskell"
    ,"Elizabeth Gibney", "Elizabeth J. Perry", "Elizabeth K. Cahoon", "Elizabeth L. Bjork"
    ,"Elizabeth Pennisi", "Elizabeth S. Spelke", "Elizabeth Selvin", "Elizabeth Spelke", "Elizabeth Steele"
    ,"Elizabeth Weil", "Elizabeth Williamson", "Ella Fitzgerald", "Ellen Byron", "Ellen J. Langer"
    ,"Ellen Leibenluft", "Elliot S. Gershon", "Elliot S. Vesell", "Elliot M. Tucker-Drob"
    ,"Elon Musk", "Elsdon Storey", "Emad Mostaque", "Emanuel Miller", "Emanuele Felice"
    ,"Emelia J. Benjamin", "Emi Hasegawa", "Emi Nakamura", "Emil Hagstr\246m", "Emil O. W. Kirkegaard"
    ,"Emilie Kaufmann", "Emilio Ferrer", "Emily Chew", "Emily Gerard"
    ,"Emily Gould", "Emily Jane Fox", "Emily M. Bender", "Emily Mitchell", "Emily Morton"
    ,"Emily Oster", "Emily Parker", "Emily Pronin", "Emily Short", "Emily Weiss"
    ,"Emily Wilkinson", "Emma C. Teeling", "Emma Copley Eisenberg", "Emma Frans", "Emmanouil T. Dermitzakis"
    ,"Emmanuel Carr\232re", "Emmanuel Le Roy Ladurie", "Emmeline Edwards", "Emmett Shear", "Emory S. Bogardus"
    ,"English Wikipedia", "Enoch Callaway", "Enrique Santiago", "Equestria Daily", "Eran Elinav"
    ,"Eran Segal", "Eran Shor", "Erez Ben-Yosef", "Erhan Guven", "Eric A. Hanushek"
    ,"Eric A. Posner", "Eric B. Rimm", "Eric Brill", "Eric Chu", "Eric Drexler"
    ,"Eric Fombonne", "Eric Goles", "Eric Hambro", "Eric Hill", "Eric Horvitz"
    ,"Eric J. Johnson", "Eric J. Topol", "Eric Kaufmann", "Eric Langlois", "Eric M. Smith"
    ,"Eric N. Olson", "Eric Nestler", "Eric Newcomer", "Eric Nielsen", "Eric Nyberg"
    ,"Eric R. Gamazon", "Eric Ravussin", "Eric Rimm", "Eric Rubin", "Eric S. Lander"
    ,"Eric S. Raymond", "Eric S. Schmitt", "Eric Schadt", "Eric Schmidt", "Eric Schwitzgebel"
    ,"Eric Sunderland", "Eric Turkheimer", "Eric Verdin", "Eric Vilain", "Eric Wallace"
    ,"Eric Williams", "Eric Xing", "Eric von Hippel", "Eric-Jan Wagenmakers", "Erich D. Jarvis"
    ,"Erich Friedman", "Erik Brynjolfsson", "Erik Christensen", "Erik Christiansen", "Erik D. Demaine"
    ,"Erik Demaine", "Erik Hoel", "Erik Johnson", "Erik Lindgren", "Erik Lindqvist"
    ,"Erik Postma", "Erik Zimen", "Erika Jensen-Jarolim", "Erin D. Michos", "Erin M. Gibson"
    ,"Ernest Beaglehole", "Ernest Hartmann", "Ernest R. Hilgard", "Ernst Caspari", "Ernst Fehr"
    ,"Eshel Ben-Jacob", "Eske Willerslev", "Essi Viding", "Esther Duflo", "Esther Thelen"
    ,"Ethan Kaplan", "Ethan Kross", "Ethel M. Elderton", "Ethel Person", "Etzel Carde\241a"
    ,"Eudald Carbonell", "Eugene Galanter", "Eugene V. Koonin", "Eugene W. Myers", "Eugene Wu"
    ,"Eva Baker", "Eva Miranda", "Eva Vivalt", "Evan Armstrong", "Evan E. Eichler"
    ,"Evan Mast", "Evan Maxwell", "Evangelos Eleftheriou", "Evdokia Anagnostou", "Evelina Fedorenko"
    ,"Evelynn Hammonds", "Everett Mendelsohn", "Evgenii Nikishin", "Ewa Deelman", "Ewa Grabowska"
    ,"Ewald Ammende", "Ewan Birney", "Ewout W. Steyerberg", "Ezra Klein", "E\246rs Szathm\225ry"
    ,"Fabio Petroni", "Fahu Chen", "Faisal Mushtaq", "Fan Bao"
    ,"Fan Hui", "Fan Jiang", "Fan Li", "Fan Wu", "Fan Yi"
    ,"Fang Fang", "Fang Liu", "Farah Naz Talpur", "Farhad Moshiri", "Farinaz Koushanfar"
    ,"Faruk Ahmed", "Fatih Porikli", "Fei Peng", "Fei Xia", "Felix A. Gers"
    ,"Felix Creutzig", "Felix Heide", "Felix Oberholzer-Gee", "Feng Li", "Feng Shi"
    ,"Feng Yu", "Feng Zhang", "Ferenc Gombos", "Fergus Henderson", "Fergus Shanahan"
    ,"Fernand Gobet", "Fernando D\237az", "Fernando Pereira", "Fernando Reinares", "Fernando Rosas"
    ,"Fiery Cushman", "Filip Kovacevic", "Finale Doshi-Velez", "Finn Rasmussen", "Florence L. Goodenough"
    ,"Florent Krzakala", "Florian Fuchs", "Foster Provost", "Foutse Khomh", "Frances Griffiths"
    ,"Frances H. Arnold", "Francesca Dominici", "Francesca Gino", "Francesca Happ\233", "Francesco Barbieri"
    ,"Francesco Bettella", "Francesco Nori", "Francis Collins", "Francis Fukuyama", "Francis Galton"
    ,"Francis J. McMahon", "Francis S. Collins", "Francis T. McAndrew", "Francis Yates", "Francisco Guzman"
    ,"Franco Lucchini", "Franco Moretti", "Francois Balloux", "Francois Lanusse", "Frank A. Chervenak"
    ,"Frank B. Hu", "Frank C. J. McGurk", "Frank C. Worrell", "Frank Drake", "Frank Dudbridge"
    ,"Frank E. Speizer", "Frank Falkner", "Frank Hu", "Frank Keller", "Frank Key"
    ,"Frank L. Schmidt", "Frank Levy", "Frank M. Spinath", "Frank McCown", "Frank McSherry"
    ,"Frank N. Freeman", "Frank Ramsey", "Frank Rosenblatt", "Frank Ruskey", "Frank Wang"
    ,"Frank Zhang", "Frantisek Svantner", "Franz K\246nig", "Fran\231ois Balloux", "Fran\231ois Chollet"
    ,"Frazer Anderson", "Fred Brooks", "Fred H. Gage", "Fred W. Johnson", "Frederick Mosteller"
    ,"Frederico Finan", "Fredrik Ull\233n", "Freeman Dyson", "Freeman J. Dyson", "Fritz Cremer"
    ,"Fritz Heider", "Fruhling Rijsdijk", "Fr\252hling Rijsdijk", "Fujiwara no Teika", "G. A. Barnard"
    ,"G. Davey Smith", "G. E. Moore", "G. Thomson", "G. Warren Nutter", "Gabor Tardos"
    ,"Gabriella Juh\225sz", "Gabriella Morreale de Escobar", "Gad Saad", "Gail C. Murphy", "Gail P. Jarvik"
    ,"Ganesh Sittampalam", "Gang Hua", "Gang Liu", "Gao Huang", "Gardner Murphy"
    ,"Gareth Cross", "Garett Jones", "Garrett Ienner", "Garry Newman", "Garry Tan"
    ,"Garth Davies", "Garth Saloner", "Gary Alan Fine", "Gary Drescher", "Gary Greenberg"
    ,"Gary LaFree", "Gary Lynch", "Gary Marcus", "Gary S. Becker", "Gary Saul Morson"
    ,"Gavin A. Schmidt", "Gavin Andresen", "Gavin E. Crooks", "Gavin Wright", "Ge Li"
    ,"Ge Yang", "Gemma Modinos", "Gene E. Robinson", "Gene Tsudik", "Gene V. Glass"
    ,"Gene Wolfe", "Generation Scotland", "Geoff Davis", "Geoff MacDonald"
    ,"Geoffrey F. Miller", "Geoffrey Hinton", "Geoffrey Hodgson", "Geoffrey Holmes", "Geoffrey J. Gordon"
    ,"Geoffrey Ling", "Geoffrey M. Hodgson", "Geoffrey Wood", "Georg Ehret", "Georg Simmel"
    ,"George Altman", "George B. Dantzig", "George B. Grant", "George Boolos", "George D. Smith"
    ,"George D. Stoddard", "George D. Yancopoulos", "George Danezis", "George Davey Smith", "George Davey-Smith"
    ,"George E. Murphy", "George E. P. Box", "George F. Sutherland", "George Fitzmaurice", "George Heald"
    ,"George Hotz", "George J. Borjas", "George Kurian", "George L. Trager", "George Loewenstein"
    ,"George M. Church", "George Musser", "George Packer", "George Papandreou", "George Preti"
    ,"George Q. Daley", "George R. Price", "George S. Brown", "George Schlager Welsh", "George Stamatoyannopoulos"
    ,"George Stein", "George W. Ross", "George Washington", "George Yancey", "George Yancopoulos"
    ,"George Zimmerman", "George van Driem", "Georges El Fakhri", "Georgia Chenevix-Trench", "Georgia M. Dunston"
    ,"Georgios Athanasiadis", "Georgios Georgakis", "Georgios N. Yannakakis", "Georgios Papagiannis", "Geraint Rees"
    ,"Gerald E. McClearn", "Gerald Feinberg", "Gerald Friedland", "Gerald Holton", "Gerald M. Rubin"
    ,"Gerald R. Ferris", "Geraldine Dawson", "Gerard D. Schellenberg", "Gerard J. Tellis", "Gerard Manley Hopkins"
    ,"Gerard Sanacora", "Gerard Saucier", "Gerard van den Berg", "Gerhard Andersson", "Gerhard Neumann"
    ,"Gerhard Roth", "Gerome Breen", "Gian-Carlo Rota", "Giang Nguyen", "Gianpiero D. Palermo"
    ,"Gideon J. Mellenbergh", "Gil McVean", "Gilbert S. Omenn", "Gilbert Strang", "Gilles E. Gignac"
    ,"Gilles Saint-Paul", "Gillian Bennett", "Gillian Hadfield", "Gillian Tett", "Gina Choe"
    ,"Gina Kolata", "Giovanni Sala", "Girardin Jean-Louis", "Girishwar Misra", "Gitte Moos Knudsen"
    ,"Giulio Perugi", "Giulio Tononi", "Giuseppe Gabrielli", "Giuseppe Petrosino", "Giuseppe Soda"
    ,"Gjergji Kasneci", "Glen Baker", "Glen Owen", "Glenn C. Loury", "Glenn Loury"
    ,"Glenn MacDonald", "Glenn Turner", "Gloria Mark", "Glyn Lewis"
    ,"Godfrey Thomson", "Goldine C. Gleser", "Goldine Gleser"
    ,"Gon\231alo Abecasis", "Gon\231alo R. Abecasis", "Google Sheets", "Goran Knezevic", "Gordon Pennycook"
    ,"Gordon W. Schuett", "Grace Akello", "Grace Chu", "Graham Coop", "Graham Finlayson"
    ,"Graham Healy", "Grant Atkins", "Grant Goldman", "Grant Thornton", "Greg Brockman"
    ,"Greg Crawford", "Greg Gorman", "Greg Hampikian", "Greg J. Duncan", "Greg Kumparak"
    ,"Greger Larson", "Gregor Betz", "Gregor Hasler", "Gregory Andrews", "Gregory B. Northcraft"
    ,"Gregory Bateson", "Gregory Benford", "Gregory Burke", "Gregory D. Hager", "Gregory Dudek"
    ,"Gregory Francis", "Gregory M. Fahy", "Gregory S. Berns", "Gretchen Chapman", "Gudrun Wagner"
    ,"Guido Barbujani", "Guido Kroemer", "Guillaume Leclerc", "Guillermo Sapiro", "Gunther Eysenbach"
    ,"Guo Li", "Guoyao Wu", "Gustav Larsson", "Gustavo Turecki", "Guy Fournier"
    ,"Guy Katz", "Guy Montrose Whipple", "G\225bor Scheiring", "G\225bor Vajta"
    ,"G\233rard Ben Arous", "H. Andrew Schwartz", "H. B. Barlow", "H. M. Collins", "H. P\233tard"
    ,"H. V. Jagadish", "H. W. B. Joseph", "H. Wilkins", "Hagop S. Akiskal", "Haidar Khan"
    ,"Haifeng Fu", "Haim Sompolinsky", "Haim Weizman", "Haitao Zheng", "Haiyan Zhang"
    ,"Haizhou Li", "Hakon Hakonarson", "Hal R. Varian", "Hamed Haddadi", "Hamilton O. Smith"
    ,"Han Li", "Han Zhang", "Hana El-Samad", "Hang Zhou", "Hannah Devlin"
    ,"Hannah Kim", "Hannah Lee", "Hannah Miller", "Hannah Robinson", "Hannele Ruohola-Baker"
    ,"Hannes Baumann", "Hannes Petursson", "Hannu Lahtinen", "Hannu Rajaniemi", "Hans Clevers"
    ,"Hans Eiberg", "Hans Eriksson", "Hans Gruber", "Hans Eysenck", "Hans M. Kristensen"
    ,"Hans Moravec", "Hans Robert Sch\246ler", "Hans-Peter Kohler", "Hans-Ulrich Wittchen", "Hanspeter Pfister"
    ,"Hany Farid", "Hao Li", "Hao Ying", "Hao Zhou", "Harjeet Singh"
    ,"Harold Hotelling", "Harold M. Williams", "Harold Morowitz", "Harold Pashler", "Harold S. Stone"
    ,"Harold Schneider", "Harold Snieder", "Harris A. Lewin", "Harrison Hong", "Harry Bouwman"
    ,"Harry Brandt", "Harry H. Harman", "Harry Harper", "Harry Olson", "Harry Ostrer"
    ,"Hartmut Neven", "Haruko Obokata", "Harvey A. Carr", "Harvey Carr", "Hazel Brown"
    ,"He Ma", "He Zhang", "He Zhou", "Healthy Aging Study]", "Heather Huntington"
    ,"Heather Zar", "Heikki Kainulainen", "Heiner Rindermann", "Heinrich Jasper", "Heinrich Peters"
    ,"Heinz Feldmann", "Helen H. Hobbs", "Helen Leonard", "Helen M. Blau", "Helen Toner"
    ,"Helen Y. Chu", "Helge Kragh", "Helgi Jonsson", "Helmut Sch\228fer", "Hendrik Poinar"
    ,"Heng Ji", "Heng Li", "Heng Yang", "Henk Barendregt", "Henkjan Honing"
    ,"Henri Chabrol", "Henri Leridon", "Henrich R. Greve", "Henrik Anckars\228ter", "Henrik Kleven"
    ,"Henrik Zetterberg", "Henrique Rocha", "Henry Brinkerhoff", "Henry E. Garrett", "Henry H. Goddard"
    ,"Henry H. Richardson", "Henry J. Heimlich", "Henry J. Kelley", "Henry K. Beecher", "Henry Kranzler"
    ,"Henry L. Paulson", "Henry L. Roediger", "Henry L. Roediger III", "Henry Mayhew", "Henry W. Lin"
    ,"Henry Wong", "Herbert A. Simon", "Herbert Maschner", "Herbert Spencer Jennings", "Herbert Weissbach"
    ,"Herman Aguinis", "Herman H. Spitz", "Herman Pleij", "Herman Pontzer", "Hermann Wagner"
    ,"Hermine Maes", "Hernan Aguirre", "Herwig Baier", "Hessel Oosterbeek", "Hideyuki Okano"
    ,"Hilary Hoynes", "Hilary W. Hoynes", "Himabindu Lakkaraju", "Himanshu Thakur"
    ,"Hiram Stevens Maxim", "Hiroshi Ishikawa", "Hiroshi Kaneda", "Hirotaka Sugawara", "Hiroyuki Morita"
    ,"Hiroyuki Sasaki", "Hisham Al-Obaidi", "Hod Lipson", "Holden Karnofsky", "Holger Rootz\233n"
    ,"Hollis Robbins", "Holly A. Taylor", "Holly Jackson", "Hong Wei", "Hong Wu"
    ,"Hong Ye", "Hong Yu", "Hongkui Deng", "Hoon Chung", "Hopi E. Hoekstra"
    ,"Horace Barlow", "Horatio H. Newman", "Horst D. Simon", "Hovav Shacham", "Howard Gardner"
    ,"Howard Raiffa", "Howard S. Liddell", "Howard Wainer", "Howard Y. Chang", "Hoyt Bleakley"
    ,"Hsiao-Wuen Hon", "Hsinchun Chen", "Hu Chen", "Hu Haifeng", "Hua Chen"
    ,"Hua Hua", "Hua Shao", "Huan Liu", "Huan Xia", "Huang Huang"
    ,"Huanming Yang", "Hubert L. Dreyfus", "Huda Akil", "Hugh Calkins", "Hugh Gurling"
    ,"Hugh Gusterson", "Hugh McColl", "Hugh Sinclair", "Hugo Critchley", "Hui Chen"
    ,"Hui Lei", "Hui Liu", "Hui Shi", "Hui Wu", "Hui Xiong"
    ,"Hui Yang", "Hyeonwoo Kim", "Hyun Jin Kim", "Hyung Chul Kim", "H\224n Th\7871 Th\224nh"
    ,"I-Chen Wu", "I. Glenn Cohen", "I. J. Deary", "I. J. Good", "Ian Beer"
    ,"Ian D. Cameron", "Ian Ellis", "Ian Ford", "Ian Goodfellow", "Ian H. Gotlib"
    ,"Ian J. Deary", "Ian Meinertzhagen", "Ian Tomlinson", "Ian Watt", "Ibrahim Oweiss"
    ,"Ido Bachelet", "Iftikhar Khan", "Ignacio Alvarez", "Igor Douven", "Igor Fedorov"
    ,"Igor Larrosa", "Igor Rudan", "Ila Fiete", "Ilina Singh", "Ilona Kov\225cs"
    ,"Ilya A. Strebulaev", "Ilya Mironov", "Ilya Segal", "Ilya Sutskever", "Immaculata De Vivo"
    ,"Inflection AI", "Ingo Potrykus", "Ingo Rechenberg", "Ingvar Andersson", "Ingvild Alm\229s"
    ,"Insoo Hyun", "Intikhab Alam", "Ioanna Tzoulaki", "Ion Stoica", "Ionica Smeets"
    ,"Iren\228us Eibl-Eibesfeldt", "Irfan Essa", "Iroise Dumontheil", "Irving I. Gottesman"
    ,"Irving Kirsch", "Irving L. Janis", "Irving Lorge", "Irwin D. Waldman"
    ,"Irwin Silverman", "Irwin Waldman", "Iryna Gurevych", "Isaac Asimov", "Isabelle Augenstein"
    ,"Isabelle Boutron", "Isabelle Gallagher", "Isabelle Guyon", "Ishan Pandey", "Israel Ramirez"
    ,"Itai Yanai", "Italo Calvino", "Itamar Simonson", "Itsik Pe\8217er", "Itzhak Brook"
    ,"Ivan Curjuric", "Ivan Horvath", "Ivan Krasko", "Ivan Skorokhodov", "Ivan Sutherland"
    ,"Ivan Titov", "Ivan Vuli\263", "Iyad Rahwan", "J. A. Allen", "J. Allan Hobson"
    ,"J. B. MacKinnon", "J. B. Rhine", "J. B. S. Haldane", "J. Bradford DeLong", "J. C. Barnes"
    ,"J. C. DeFries", "J. C. White", "J. Craig Venter", "J. Doyne Farmer", "J. F. Price"
    ,"J. H. Saltzer", "J. K. Rowling", "J. Keith Joung", "J. Kevin O\8217Regan", "J. L. Austin"
    ,"J. M. Clark", "J. M. Hammersley", "J. M. Robson", "J. Mark G. Williams", "J. McAdam"
    ,"J. Michael Bailey", "J. P. Guilford", "J. Patrick Gray", "J. Philippe Rushton", "J. R. Anderson"
    ,"J. R. R. Tolkien", "J. R. Smith", "J. Rhodes", "J. Richard Gott III", "J. Russell"
    ,"J. Scott Armstrong", "J. W. Johnson", "J. Ward", "Jaakko Kaprio", "Jaap Murre"
    ,"Jack A. Naglieri", "Jack Block", "Jack Bowden", "Jack C. Taylor", "Jack D. Dunitz"
    ,"Jack Devine", "Jack Goldberg", "Jack Hitt", "Jack Humphrey", "Jack London"
    ,"Jack Lynch", "Jack Ogden", "Jack Porter", "Jack Rae", "Jackie Kay"
    ,"Jackson Peak", "Jacob Austin", "Jacob Hooker", "Jacob Katz", "Jacob Lawrence"
    ,"Jacob Marschak", "Jacob Plange-Rhule", "Jacob Reimer", "Jacob Rosenthal", "Jacob Taylor"
    ,"Jacob Wallace", "Jacqueline Robinson", "Jacques Fellay", "Jacques Hadamard", "Jacques Rossouw"
    ,"Jad Abumrad", "Jagmeet Singh", "Jaime Carbonell", "Jakub Konecny", "James A. Evans"
    ,"James A. Horne", "James A. King", "James A. Robinson", "James A. Thomson", "James Alexander Hamilton"
    ,"James Andreoni", "James B. Jordan", "James B. Stewart", "James Bessen", "James Cook"
    ,"James Cross", "James Crouse", "James D. Stewart", "James D. Weinrich", "James Demmel"
    ,"James Donald Weinrich", "James E. Bruce", "James E. Miller", "James E. Mitchell", "James E. Morgan"
    ,"James E. Rothman", "James F. Crow", "James F. Fries", "James F. Gusella", "James F. Wilson"
    ,"James Feyrer", "James G. Wilson", "James Glass", "James Griesemer", "James H. Cartwright"
    ,"James H. Leuba", "James H. Stark", "James H. Wyckoff", "James Hammill", "James Hansen"
    ,"James Heckman", "James J. Bull", "James J. Cimino", "James J. Heckman", "James J. Lee"
    ,"James J. Murphy", "James K. Galbraith", "James Koppel", "James L. McClelland", "James L. McGaugh"
    ,"James Landay", "James M. Cook", "James M. Davis", "James MacKillop", "James Manyika"
    ,"James Marston Fitch", "James McCracken", "James Molloy", "James P. Cook", "James P. Crutchfield"
    ,"James P. Noonan", "James Paulson", "James Pinkerton", "James R. Wilson", "James Randi"
    ,"James Ryley", "James Sales", "James Shelley", "James Surowiecki", "James T. Austin"
    ,"James Tiptree Junior", "James Torrance", "James V. Haxby", "James W. Vaupel", "James Wilkinson"
    ,"Jamie Hall", "Jamie Kerr", "Jamie Peters", "Jan Born", "Jan C. van Ours"
    ,"Jan Chovanec", "Jan Havl\237\269ek", "Jan K. Buitelaar", "Jan M. Rabaey", "Jan O. Korbel"
    ,"Jan Pawlowski", "Jan Richter", "Jan Vijg", "Jan te Nijenhuis", "Jan van der Laan"
    ,"Jan-Emmanuel De Neve", "Jan-Eric Gustafsson", "Jan-\197ke Gustafsson", "Jane A. Cauley", "Jane C. Charlton"
    ,"Jane Cauley", "Jane Gibson", "Jane Greaves", "Jane Hurst", "Jane Hutton"
    ,"Jane Kim", "Jane Loevinger", "Jane Murray", "Jane Phillips", "Jane Wardle"
    ,"Jane-Ling Wang", "Janet Coles", "Janet D. Elashoff", "Janet Kelso", "Janet M. Thornton"
    ,"Janet Pierrehumbert", "Janet Seeley", "Janet Shibley Hyde", "Janet Treasure", "Janey L. Wiggs"
    ,"Janice Chen", "Janice K. Kiecolt-Glaser", "Janina Jeff", "Janina M. Jeff", "Janos Galambos"
    ,"Janusz Jankowski", "Jared Diamond", "Jaroslav Flegr", "Jaroslav Sevcik", "Jaroslava Blazkova"
    ,"Jarrod Tanny", "Jasjeet S. Sekhon", "Jason Boardman", "Jason Collins", "Jason D. Boardman"
    ,"Jason Downer", "Jason H. Moore", "Jason H. Steffen", "Jason Huang", "Jason Liang"
    ,"Jason Liu", "Jason Mars", "Jason McIntyre", "Jason Weston", "Javier Benitez"
    ,"Javier de la Rosa", "Jay Adams", "Jay Bhattacharya", "Jay Joseph", "Jay L. Lush"
    ,"Jay McClelland", "Jay S. Kaufman", "Jay Shendure", "Jay Tischfield", "Jayanta Debnath"
    ,"Jazmine Hughes", "Jean Bousquet", "Jean Decety", "Jean Dussault", "Jean Harrington"
    ,"Jean M. Twenge", "Jean Maillard", "Jean Pelletier", "Jean Wactawski-Wende", "Jean-Claude Tardif"
    ,"Jean-Jacques Hublin", "Jean-Marc Moret", "Jean-Michel Gaillard", "Jean-Paul Delahaye", "Jean-Pierre Boissel"
    ,"Jeanette Taylor", "Jeanne Brooks-Gunn", "Jed McCaleb", "Jef Caers", "Jef D. Boeke"
    ,"Jeff Atwood", "Jeff Bezos", "Jeff Binder", "Jeff Dean", "Jeff Sachs"
    ,"Jeff Schneider", "Jeff W. Lichtman", "Jeffery Vance", "Jeffrey A. Lieberman", "Jeffrey Dean"
    ,"Jeffrey Jensen", "Jeffrey L. Bennetzen", "Jeffrey Lieberman", "Jeffrey Pennington", "Jeffrey R. Kling"
    ,"Jeffrey T. Hancock", "Jeffrey T. Leek", "Jehannine Austin", "Jelte M. Wicherts", "Jenae M. Neiderhiser"
    ,"Jennifer A. Doudna", "Jennifer Callahan", "Jennifer Clark", "Jennifer Doudna", "Jennifer Eccles"
    ,"Jennifer Harper", "Jennifer Jordan", "Jennifer Wilcox", "Jennifer Williamson", "Jennifer Wortman Vaughan"
    ,"Jennifer Young", "Jenny Morton", "Jenny Tung", "Jens Alber", "Jens-Christian Svenning"
    ,"Jeremy Atack", "Jeremy Avigad", "Jeremy Bernstein", "Jeremy Elson", "Jeremy Evans"
    ,"Jeremy Freese", "Jeremy Gibbons", "Jeremy J. Stone", "Jeremy Kahn", "Jeremy Nixon"
    ,"Jeremy P. E. Spencer", "Jeremy Tankard", "Jeremy Thomas", "Jermaine Hall", "Jerome Connor"
    ,"Jerome E. Singer", "Jerome Lewis", "Jerome Sarris", "Jerrold E. Marsden", "Jesper L\252tzen"
    ,"Jess Smith", "Jesse Chandler", "Jesse Graham", "Jesse Lopez", "Jesse M. Shapiro"
    ,"Jesse Prinz", "Jesse Rothstein", "Jesse Walker", "Jessica Cohen", "Jessica F. Cantlon"
    ,"Jessica Gurevitch", "Jessica Huang", "Jessica Hullman", "Jessica Mathews", "Jessica Pan"
    ,"Jessica Silver-Greenberg", "Jessica Tyler", "Jessica Wright", "Jesus Rios", "Ji Qi"
    ,"Jia Li", "Jia Liu", "Jia Xu", "Jia Zheng", "Jian Cui"
    ,"Jian Tang", "Jian Zeng", "Jian Zhou", "Jiang Li", "Jiang Wang"
    ,"Jiang Yang", "Jianhua Lu", "Jianjun Cheng", "Jianqing Fan", "Jianshu Li"
    ,"Jiawei Han", "Jiawei Shen", "Jiaya Jia", "Jie Fang", "Jie Lu"
    ,"Jie Tang", "Jie Wu", "Jie Yang", "Jie Yao", "Jie Zheng"
    ,"Jiebo Luo", "Jiewen Zhang", "Jill M. Hooley", "Jillian F. Banfield", "Jillian Fisher"
    ,"Jim Hoge", "Jim van Os", "Jimmy Maher", "Jimmy Wei", "Jin Li"
    ,"Jin Yang", "Jin Yuan", "Jinchao Xu", "Jing Chang", "Jing He"
    ,"Jing Luo", "Jing Ma", "Jing Sun", "Jing Wen", "Jingjing Wang"
    ,"Jinyu Li", "Jiong Zhu", "Jitendra Malik", "Jiwei Zhao", "JoAnn E. Manson"
    ,"Joan Bulman", "Joan M. Redwing", "Joan McCord", "Joanna M. Wardlaw", "Joanna Masel"
    ,"Joanna Wysocka", "Joanna Zajac", "Joao Pedro de Magalhaes", "Joaquim Radua", "Jodi White"
    ,"Jodie Ingles", "Joe Alessi", "Joe Boyd", "Joe Z. Tsien", "Joel Best"
    ,"Joel David Hamkins", "Joel Gelernter", "Joel Kramer", "Joel Mokyr", "Joel N. Hirschhorn"
    ,"Joel Paris", "Joel S. Demski", "Joel S. Schuman", "Joel Schwartz", "Joel Simon"
    ,"Joel Spolsky", "Joel Waldfogel", "Joelle Pineau", "Joerg Zimmermann", "Joeri Rogelj"
    ,"Jofish Kaye", "Johan Bollen", "Johan Jakobsson", "Johan Paulsson", "Johan Sundstrom"
    ,"Johan Sundstr\246m", "Johan Wagemans", "Johannes Fischer", "Johannes Haushofer", "Johannes Kornhuber"
    ,"Johannes Krause", "Johannes Michalak", "Johannes Smit", "Johannes Wagner", "John A. Bargh"
    ,"John A. Carpenter", "John A. Clausen", "John A. Hartigan", "John A. Jane", "John A. List"
    ,"John A. Morris", "John A. Pickett", "John A. Rogers", "John A. Wagner", "John Atherton"
    ,"John B. Calhoun", "John B. Carroll", "John B. Gibson", "John B. Harley", "John B. Hogenesch"
    ,"John B. Watson", "John Barnard", "John Blangero", "John Bohannon"
    ,"John Bound", "John Burden", "John C. Baez", "John C. Crabbe", "John C. DeFries"
    ,"John C. Loehlin", "John C. Whittaker", "John Canny", "John Carmack", "John Christodoulou"
    ,"John D. Mayer", "John D. Storey", "John Danesh", "John Dean", "John Deanfield"
    ,"John Downing", "John Draper", "John E. Anderson", "John E. Burke", "John E. Hayes"
    ,"John E. Hopcroft", "John E. Hunter", "John E. J. Rasko", "John E. Murray", "John E. Warnock"
    ,"John Etchemendy", "John F. Cryan", "John Fang", "John G. Gager", "John G. Roberts"
    ,"John Geanakoplos", "John H. Conway", "John H. Dessauer", "John H. Holland", "John H. R. Maunsell"
    ,"John Haltiwanger", "John Harrison", "John Hopcroft", "John Hopfield", "John Horwood"
    ,"John Huddleston", "John Ioannidis", "John J. Crowley", "John J. Donovan", "John J. Ely"
    ,"John J. Farrell", "John J. McArdle", "John J. Walsh", "John Jonides"
    ,"John K. Hewitt", "John K. Kruschke", "John Kemp", "John Kruschke", "John L. Fuller"
    ,"John L. Gustafson", "John L. Hennessy", "John Ledyard", "John M. Barry", "John M. Davis"
    ,"John M. Shelton", "John M. Wells", "John Markoff", "John Marshall", "John Massie"
    ,"John Maynard Keynes", "John Maynard Smith", "John McLean", "John McPhee", "John Money"
    ,"John Mueller", "John Novembre", "John Nuckolls", "John Obert Voll", "John O\8217Mahony"
    , "John P. Campbell", "John P. Donnelly", "John P. Perdew", "John Paul Scott"
    ,"John Paul Wright", "John Phan", "John Preskill", "John Q. Trojanowski"
    ,"John R. Alford", "John R. Conway", "John R. Hibbing", "John R. Hughes", "John R. Kramer"
    ,"John R. Lott", "John R. Platt", "John R. Pringle", "John R. Thompson", "John R. White"
    ,"John Rader Platt", "John Resig", "John Rettie", "John Roberts", "John Rust"
    ,"John S. Werner", "John Seabrook", "John Shawe-Taylor", "John Sievenpiper", "John Sudworth"
    ,"John T. Cacioppo", "John Tooby", "John Tran", "John V. Guttag", "John Van Reenen"
    ,"John W. Campbell", "John W. Emerson", "John W. Tukey", "John Wawrzynek", "John Wieting"
    ,"John Zaller", "John von Neumann", "John-Dylan Haynes", "Johnny Ryan", "Jon Crowcroft"
    ,"Jon F. Merz", "Jon H. Kaas", "Jon Kleinberg", "Jon Porter", "Jon Seger"
    ,"Jon Steinberg", "Jonah Berger", "Jonah Lehrer", "Jonah Rockoff", "Jonas F. Ludvigsson"
    ,"Jonas Pettersson", "Jonathan A. Coddington", "Jonathan A. Eisen", "Jonathan Balcombe", "Jonathan Browne"
    ,"Jonathan Cook", "Jonathan D. Cohen", "Jonathan Friedman", "Jonathan Gottschall", "Jonathan Haidt"
    ,"Jonathan Hartley", "Jonathan Hewitt", "Jonathan Hoefler", "Jonathan K. Pritchard", "Jonathan Koomey"
    ,"Jonathan L. Zittrain", "Jonathan Ling", "Jonathan M. Borwein", "Jonathan Marchini", "Jonathan Mayer"
    ,"Jonathan O\8217Callaghan", "Jonathan Rees", "Jonathan Rosand", "Jonathan Schooler", "Jonathan Stevens"
    ,"Jonathan Tremblay", "Jonathan Zinman", "Joop Hartog", "Jordan Anderson", "Jordan Bimm"
    ,"Jordan Ellenberg", "Jordan Pollack", "Jordan Smoller", "Jordan Todorov", "Jordan W. Smoller"
    ,"Jordi Bur\233s", "Jordi Cam\237", "Jordi Torres", "Jorge Barros", "Jorge L. Contreras"
    ,"Jorge Leite", "Jorge Luis Borges", "Jorge Nocedal", "Jorge Rocha", "Jose Antonio"
    ,"Josef Coresh", "Josef Parnas", "Josef Priller", "Josef Urban", "Josep Call"
    ,"Joseph A. Konstan", "Joseph Agassi", "Joseph Alsop", "Joseph B. Kadane", "Joseph Bernstein"
    ,"Joseph Biederman", "Joseph Boden", "Joseph E. Parisi", "Joseph F. Quinn", "Joseph Felsenstein"
    ,"Joseph Firth", "Joseph Hellerstein", "Joseph Henrich", "Joseph L. DeRisi", "Joseph L. Fleiss"
    ,"Joseph Lau", "Joseph Lee Rodgers", "Joseph Loscalzo", "Joseph M. Baker", "Joseph M. Horn"
    ,"Joseph Pemberton", "Joseph R. Ecker", "Joseph Rotblat", "Joseph Schmidt", "Joseph Uscinski"
    ,"Joseph W. Kable", "Joseph Wang", "Joseph Wong", "Joseph Zubin", "Joseph de la Vega"
    ,"Josephine Ball", "Josh Abramson", "Josh Arnold", "Josh Bongard", "Josh Hodge"
    ,"Josh Tobin", "Joshua Angrist", "Joshua Aronson", "Joshua C. Denny", "Joshua D. Greene"
    ,"Joshua D. Rauh", "Joshua Hicks", "Joshua M. Pearce", "Joshua M. Tybur"
    ,"Joshua Rauh", "Joshua T. Vogelstein", "Joshua Tybur", "Jos\233 Galindo"
    ,"Jos\233 Maldonado", "Jos\233 Mar\237a Berm\250dez de Castro", "Jos\233 Onuchic", "Jos\233 Scheinkman", "Jos\233-Alain Sahel"
    ,"Jos\233e Dupuis", "Joy Milne", "Joy Wang", "Joyce Carol Oates", "Joyce Lee"
    ,"Jozef Gecz", "Jo\227o Pedro de Magalh\227es", "Ju Hu", "Ju Li", "Juan Botella"
    ,"Juan Carlos Izpisua Belmonte", "Juan Frias", "Juan Mac\237as", "Juan Pino", "Judah Folkman"
    ,"Judith Campisi", "Judith Dunn", "Judith N. Shklar", "Judy Hoffman", "Jue Wang"
    , "Jugal Kalita", "Jules White", "Julia Hippisley-Cox", "Julia Marshall"
    ,"Julian Assange", "Julian Borger", "Julian C. Stanley", "Julian Parkhill", "Julian Peto"
    ,"Julian Roth", "Julian Savulescu", "Julian Stanley", "Julian Togelius", "Juliana Schroeder"
    ,"Julie Beaulieu", "Julie Cunningham", "Julie E. Buring", "Julie Hecht", "Julie Jordan"
    ,"Julie Larsen", "Julie Williams", "Julio J. Rotemberg", "Julio Rodr\237guez", "Julius Manger"
    ,"Jun Hu", "Jun Liu", "Jun S. Liu", "Jun Xie", "Jun Xu"
    ,"Jun Yu", "June Gruber", "Junyi Li", "Jure Leskovec", "Jurgen Del-Favero"
    , "Justin Chang", "Justin Kruger", "Justin Lin", "Justin Nelson"
    ,"Justin Wagner", "Justin Yifu Lin", "Justine Moore", "Justine Musk", "Jyoti Mishra"
    ,"J\225nos Kert\233sz", "J\225nos Koll\225r", "J\225nos Kram\225r", "J\243n Steinsson", "J\248rgen M\248ller"
    ,"J\248rn Ratts\248", "J\252rgen Glas", "J\252rgen Maier", "J\252rgen Schmidhuber", "J\252ri Allik"
    ,"J\252ri Parik", "K. Anders Ericsson", "K. C. Paul", "K. Christopher Garcia"
    ,"K. Paige Harden", "K. Patricia Cross", "K. T. Compton", "K. V. Mardia", "Kaare Christensen"
    ,"Kah Kay Sung", "Kai Chen", "Kai Zen", "Kai-Fu Lee", "Kai-Uwe Hinrichs"
    ,"Kaiming He", "Kaiping Zheng", "Kamal Gupta", "Kameshwar Prasad", "Kang Zhang"
    ,"Kang Zhao", "Kara Swisher", "Karan Goel", "Karanjeet Singh"
    ,"Karen A. Cerulo", "Karen C. Johnson", "Karen Carr", "Karen F. Berman", "Karen Hao"
    ,"Karen Horney", "Karen L. Mohlke", "Karen Oegema", "Karestan C. Koenen"
    ,"Karestan Koenen", "Kari Stefansson", "Karim R. Lakhani", "Karin Broberg", "Karin Strauss"
    ,"Karl Friston", "Karl J. Friston", "Karl J. Holzinger", "Karl Pearson", "Karl Popper"
    ,"Karl Zilles", "Karla Miller", "Karsten M\252ller", "Karthik Muralidharan", "Karthik Raman"
    ,"Katalin Karik\243", "Katalin Susztak", "Kate Devlin", "Kate Tchanturia"
    ,"Kate Tilling", "Katerina Douka", "Kath Smith", "Katherine A. Rawson"
    ,"Katherine Baicker", "Katherine Baker", "Katherine Belov", "Katherine Harvey", "Katherine Kirk"
    ,"Katherine L. Milkman", "Katherine Martin", "Katherine S. Brehme", "Katherine S. Pollard", "Kathleen D. Vohs"
    ,"Kathleen Kenealy", "Kathleen McGarry", "Kathleen Merikangas", "Kathleen Mullan Harris", "Kathryn Fox"
    ,"Kathryn Graddy", "Kathryn North", "Kathryn Paige Harden", "Kathryn Roeder", "Kathryn S. McKinley"
    ,"Kathryn T. Hall", "Kathy Niakan", "Kathy Yelick", "Katia Bertoldi", "Katia Sycara"
    ,"Katie A. McLaughlin", "Katie Collins", "Katja Grace", "Katja Hofmann", "Katrin Amunts"
    ,"Katrin Davidsdottir", "Katrina A. B. Goddard", "Katsuya Takahashi", "Kaushik Roy", "Kavita Bala"
    ,"Kay Amert", "Kay Redfield Jamison", "Kay Tee Khaw", "Kay-Tee Khaw", "Kay-Yut Chen"
    ,"Kazuhiko Yamamoto", "Kazuo Hara", "Kazuya Tsurumaki", "Ke Wang", "Ke Yuan"
    ,"Keely Shaw", "Keiji Tanaka", "Keith Baverstock", "Keith Bradsher", "Keith Dobney"
    ,"Keith E. Whitfield", "Keith F. Otterbein", "Keith Humphreys", "Keith J. Holyoak", "Keith Maddox"
    ,"Keith Payne", "Kelly Harrington", "Kelsey Smith", "Ken A. Paller", "Ken Kato"
    ,"Ken Nakayama", "Ken Norman", "Ken Randall", "Ken Silverstein", "Ken Suzuki"
    ,"Kenji Kobayashi", "Kenneth A. Dodge", "Kenneth B. Clark", "Kenneth E. Boulding", "Kenneth E. Stager"
    ,"Kenneth G. Libbrecht", "Kenneth J. Arrow", "Kenneth J. Gergen", "Kenneth L. Davis", "Kenneth Offit"
    ,"Kenneth S. Kendler", "Kenneth Silverman", "Kenneth Stanley", "Kent C. Berridge", "Kent M. Pitman"
    ,"Kerstin Lindblad-Toh", "Kevin Buzzard", "Kevin Chen", "Kevin Cheung", "Kevin Fu"
    ,"Kevin G. Lynch", "Kevin Holden", "Kevin Kelly", "Kevin Leyton-Brown", "Kevin Lin"
    ,"Kevin M. Beaver", "Kevin M. Esvelt", "Kevin M. Murphy", "Kevin N. Laland", "Kevin Patrick"
    ,"Kevin Perez", "Kevin Roose", "Kevin Stone", "Kevin Tran", "Kevin Xu"
    ,"Kevin Zhang", "Khaled Said", "Khanh Nguyen", "Kieran Healy", "Kieran O\8217Neill"
    ,"Kim Rose", "Kingsley Davis", "Kingsley Wong", "Kiran Musunuru", "Kirby Deater-Deckard"
    ,"Kirsten Grind", "Kiyoharu Aizawa", "Klaus M. Schmidt", "Klaus Stark", "Klaus-Peter Lesch"
    ,"Klaus-Robert M\252ller", "Knut Schmidt-Nielsen", "Koji Ishii", "Konrad Kording", "Konrad K\246rding"
    ,"Konstantinos Panagiotou", "Kostas Daniilidis", "Koushik Sen", "Krishna Chatterjee", "Krishna Mohan"
    ,"Krishna Shenoy", "Krishna V. Shenoy", "Krishnamachari Srinivasan", "Krishnan Srinivasan", "Krishnendu Chatterjee"
    ,"Krista Fischer", "Kristen Grauman", "Kristen Olson", "Kristian Kersting", "Kristian Tambs"
    ,"Kristine Beate Walhovd", "Kristine Yaffe", "Kristofer S. J. Pister", "Kristy Choi", "Kristy Lee"
    ,"Krste Asanovic", "Kun Zhou", "Kunle Olukotun", "Kuo-En Chang", "Kurt Benson"
    ,"Kurt Goldstein", "Kurt G\246del", "Kurt Keutzer", "Kurt Lewin", "Kushal Shah"
    ,"Kwanghee Lee", "Kyle Bagwell", "Kyle Carlson", "Kyle Cranmer", "Kyle Gibson"
    ,"Kyle Julian", "Kyle Myers", "K\229re Berg", "L. A. Levin", "L. Adrienne Cupples"
    ,"L. C. Knights", "L. L. Larmore", "Ladan Shams", "Ladislau B\246l\246ni", "Ladislav Mnacko"
    ,"Lajos Balint", "Lalji Singh", "Lan Zhang", "Lant Pritchett", "Larissa MacFarquhar"
    ,"Larry F. Abbott", "Larry J. Seidman", "Larry Lindsey", "Larry Page", "Larry Rudolph"
    ,"Larry S. Davis", "Larry Shepp", "Larry Shiner", "Larry Thompson", "Lars Arge"
    ,"Lars Bergman", "Lars B\228ckman", "Lars Chittka", "Lars Gustafsson", "Lars Klareskog"
    ,"Lars Nyberg", "Lars Penke", "Lars Smith", "Lars Vatten", "Lars Wallentin"
    ,"Lars-G\246ran Nilsson", "Laura Arrillaga-Andreessen", "Laura Crane", "Laura D. Kubzansky", "Laura Kelly"
    ,"Laura Kubzansky", "Laura L. Carstensen", "Laura Morelli", "Laura Owen", "Laura Spinney"
    ,"Laura Starks", "Lauren B. Alloy", "Lauren Meyers", "Lauren Scott", "Laurence Henry Tribe"
    ,"Laurence Myers", "Laurence R. Iannaccone", "Laurence Steinberg", "Laurent Itti", "Laurent Keller"
    ,"Laurent Mottron", "Lauri Bonacorsi", "Lawrence B. Schook", "Lawrence Badash", "Lawrence H. Keeley"
    ,"Lawrence H. White", "Lawrence K. Altman", "Lawrence M. Lidsky", "Lawrence Page", "Lawrence Paulson"
    ,"Lawrence Person", "Lawrence S. Phillips", "Lawrence Weschler", "Lawrence Wright", "Lawrence Zhang"
    ,"Le Jin", "Leah Boustan", "Leah Stokes", "Leanne M. Williams", "Leda Cosmides"
    ,"Lee A. Thompson", "Lee Altenberg", "Lee Ehrman", "Lee J. Cronbach", "Lee Jussim"
    ,"Lee Kuan Yew", "Lee Rainwater", "Leela Chess Zero", "Leena Peltonen", "Lei Chen"
    ,"Lei Gao", "Lei Han", "Lei Ma", "Lei Sun", "Lei Xu"
    ,"Lei Yang", "Leif Edward Ottesen Kennair", "Len Shustek", "Lenore Jacobson", "Leo Breiman"
    ,"Leon Bottou", "Leon J. Kamin", "Leon O. Chua", "Leona E. Tyler", "Leonard Carmichael"
    ,"Leonard J. Savage", "Leonard Neidorf", "Leonard Uhr", "Leonid A. Gavrilov", "Leonid A. Levin"
    ,"Leonid Kruglyak", "Leonid Sigal", "Leonidas Guibas", "Leroy Cronin", "Lesley Hoyles"
    ,"Leslie A. Lyons", "Leslie Ann Goldberg", "Leslie Bernstein", "Leslie C. Aiello", "Leslie D. Leve"
    ,"Leslie Ford", "Leslie Fritz", "Leslie Jamison", "Leslie Lamport", "Leslie Pack Kaelbling"
    ,"Lester Luborsky", "Lester Mackey", "Lev B. Levitin", "Lev Vaidman", "Lewis E. Braverman"
    ,"Lewis Hyde", "Lewis M. Terman", "Lewis Mitchell", "Lewis R. Goldberg", "Lewis Stevens"
    ,"Lexing Ying", "Leysia Palen", "Li Cai", "Li Ding", "Li Du"
    ,"Li Fei-Fei", "Li Huang", "Li Lian", "Li Shen", "Li Xing"
    ,"Li Yuan", "Li Zhao", "Liam Wright", "Liang Xu", "Lianne de Vries"
    ,"Lichao Wang", "Lifan Yuan", "Lila Gleitman", "Lin Lin", "Lin Miao"
    ,"Lin Sun", "Lin Tian", "Linda Goodman", "Linda Partridge", "Linda S. Gottfredson"
    ,"Linda Wolfe", "Lindon Eaves", "Lindsay Allen", "Lindsey A. Criswell", "Ling Yang"
    ,"Lingling Song", "Lionel Barnett", "Lior Pachter", "Liqun Luo", "Liran Carmel"
    ,"Lisa Baird", "Lisa Bero", "Lisa Feldman Barrett", "Lisa Jones", "Lisa Lee"
    ,"Lisa Mirabello", "Lisa Schut", "Lisa Tauxe", "Lisa Wang", "Lisa Wood"
    ,"Lise Eliot", "Liu Binyan", "Livia Puljak", "Lloyd G. Humphreys", "Lluis Quintana-Murci"
    ,"Llu\237s Quintana-Murci", "Logan Smith", "Lon Cardon", "Lon R. Cardon", "Long Now Foundation"
    ,"Long Zhu", "Lora Aroyo", "Lord Bowden", "Loren Eiseley", "Lori Shapiro"
    ,"Lorin Crawford", "Lorne L. Dawson", "Lorrie Cranor", "Lotte Hedeager", "Louis Bouvier"
    ,"Louis D. Matzel", "Louis Faury", "Louis Fox", "Louis Guttman", "Louis J. Muglia"
    ,"Louis J. Ptacek", "Louis L. Thurstone", "Louis Leon Thurstone", "Louis Thiry", "Louis-Philippe Morency"
    ,"Louise Arseneault", "Louise Fraser", "Louise Ross", "Louise Ryan", "Louise Sharpe"
    ,"Louise Slade", "Louise Wilson", "Lowell Wood", "Lu Cheng", "Lu Hou"
    ,"Lu Sheng", "Lu Xie", "Lu Xu", "Lu Xun", "Lu Yu"
    ,"Lubomir Feldek", "Luca Biggio", "Luca Cardelli", "Luca Cecchetti", "Luca Maria Gambardella"
    , "Lucas Chancel", "Luciano Floridi", "Lucy Blake", "Lucy Cooke"
    ,"Lucy Harrison", "Lucy Jenkins", "Lucy van Dorp", "Ludger Woessmann", "Ludovic Auger"
    ,"Ludwig Schmidt", "Luis Ceze", "Luis Guzman", "Luis Villa", "Luis W. Alvarez"
    ,"Luke Bates", "Luke Hunter", "Luke Miller", "Luke O\8217Connor"
    ,"Lutz J\228ncke", "Luyang Liu", "Lyle F. Schoenfeldt", "Lyle H. Ungar", "Lyle Ungar"
    ,"Lyn R. Griffiths", "Lynn B. Jorde", "Lynn DeLisi", "Lynn E. DeLisi", "Lynn Etheridge Davis"
    ,"Lynn Hasher", "Lynn M. LoPucki", "Lynn R. Goldman", "Lynn Vavreck", "L\233on Bottou"
    ,"M. Brent Donnellan", "M. Brewster Smith", "M. C. Bradbrook", "M. Frans Kaashoek"
    ,"M. Maria Glymour", "M. Thomas P. Gilbert", "M. Todd Henderson", "Mac King", "Maciej Ceglowski"
    ,"Maciej Henneberg", "Madeleine Thompson", "Madhu Khanna", "Madison Bentley", "Mads Melbye"
    ,"Mae Jemison", "Magdalena Zernicka-Goetz", "Magnus Johannesson", "Magnus Nordborg", "Magnus Tideman"
    ,"Mahzarin R. Banaji", "Maija Hassinen", "Maiken Nedergaard", "Makoto Hirata", "Mamoru Oshii"
    ,"Man Li", "Manasi Pradhan", "Maneesh Agrawala", "Manfred K. Warmuth", "Manfred Milinski"
    ,"Manik Varma", "Manje Gowda", "Manlio Vinciguerra", "Manolis Kellis", "Manu Sharma"
    ,"Manuel Blum", "Manuel Moyano", "Manuel Rivas", "Manuela Veloso", "Manvendra Singh"
    ,"Mara Mather", "Marc A. Suchard", "Marc Demers", "Marc H. Bornstein", "Marc Hauser"
    ,"Marc Lipsitch", "Marc N. Potenza", "Marc Sageman", "Marc Sangnier", "Marc Tischkowitz"
    ,"Marcelo O. Magnasco", "Marcia K. Johnson", "Marcia Ory", "Marcin Michalski", "Marco Avellaneda"
    ,"Marco Fabbri", "Marcus Hutter", "Marcus Munaf\242", "Margaret A. Meyer", "Margaret A. Pericak-Vance"
    ,"Margaret A. Tucker", "Margaret Burchinal", "Margaret Gatz", "Margaret J. Snowling", "Margaret Keyes"
    ,"Margaret Mitchell", "Margaret Rosario", "Margaret S. Livingstone", "Margarita Guerrero", "Margie E. Lachman"
    ,"Margit Osterloh", "Mari Ostendorf", "Maria Grande", "Maria Grazia Roncarolo", "Maria Katz"
    ,"Maria Lucia Yanguas", "Maria M. Klawe", "Maria Morena", "Maria Vlachou", "Maria-Florina Balcan"
    ,"Marian Croak", "Marian Knight", "Marian Neuhouser", "Marianne Bertrand", "Marianne Schmid Mast"
    ,"Marianne Simmel", "Marie Phillips", "Marie-Laure Ryan", "Marilyn Jager Adams", "Marilyn Strathern"
    ,"Marilynn B. Brewer", "Marina Butovskaya", "Mario Ivankovic", "Mario Maj", "Mario Szegedy"
    ,"Marion Leboyer", "Marion Roberts", "Marios Papaefthymiou", "Marius Lindauer", "Mariza de Andrade"
    ,"Marjo-Riitta J\228rvelin", "Marjolein Kriek", "Mark A. Davis", "Mark A. McDaniel", "Mark A. Murphy"
    ,"Mark Aldrich", "Mark Braverman", "Mark Caulfield", "Mark D. McDonnell"
    ,"Mark D. Shriver", "Mark D. West", "Mark Gerstein", "Mark Girolami", "Mark Granovetter"
    ,"Mark Alan Horowitz", "Mark I. McCarthy", "Mark J. Daly", "Mark Jenkinson", "Mark Keil"
    ,"Mark Kelly", "Mark Kirkpatrick", "Mark Lathrop", "Mark Lyons", "Mark Mazzetti"
    ,"Mark Miodownik", "Mark Monahan", "Mark Moss", "Mark Neumann", "Mark P. Mattson"
    ,"Mark P. Taylor", "Mark Phillips", "Mark Rowland", "Mark Russinovich", "Mark S. Allen"
    ,"Mark S. Blumberg", "Mark Sanderson", "Mark Schaller", "Mark Schmidt", "Mark Seal"
    ,"Mark Skidmore", "Mark Stoneking", "Mark T. Greenberg", "Mark Tester", "Mark Tushnet"
    ,"Mark Twain", "Mark Warschauer", "Mark Weiser", "Markku Laakso", "Markus Grompe"
    ,"Markus Gross", "Markus Reitzig", "Marlene Behrmann", "Marta Costa", "Marta Kwiatkowska"
    ,"Marta Sanchez", "Marta Zlatic", "Marten Scheffer", "Martha Cannon", "Martha Clare Morris"
    ,"Martha Mendoza", "Martha White", "Martie G. Haselton", "Martin A. Nowak", "Martin E. P. Seligman"
    ,"Martin Elliott", "Martin Ellison", "Martin Feldstein", "Martin Gardner", "Martin Hautzinger"
    ,"Martin Herrmann", "Martin Hinton", "Martin Hrab\283 de Angelis", "Martin J. Klein", "Martin Karafiat"
    ,"Martin L. West", "Martin L. Yarmush", "Martin Lucas", "Martin Lundberg", "Martin McKee"
    ,"Martin Picard", "Martin Pollard", "Martin Prince", "Martin Samuels", "Martin Schalling"
    ,"Martin Scherer", "Martin Shkreli", "Martin Steinegger", "Martin Vechev", "Martin Voracek"
    ,"Martin Waldseem\252ller", "Marvin D. Dunnette", "Mary Ann Glynn", "Mary Cannon", "Mary Cushman"
    ,"Mary Czerwinski", "Mary D\8217Alton", "Mary Evelyn Tucker", "Mary G. Dietz", "Mary Isaacson"
    ,"Mary K. Rothbart", "Mary L. Marazita", "Mary Lee Smith", "Mary Oliver", "Mary Whitton"
    ,"Mary Williamson", "Marylyn D. Ritchie", "Marylyn Ritchie", "Marzena Karpinska", "Masaki Kato"
    ,"Masao Miyazaki", "Masaru Tomita", "Masashi Yanagisawa", "Masataka Sugimoto", "Masato Fukushima"
    ,"Masayo Takahashi", "Masayoshi Ito", "Massimo Polidoro", "Matei Zaharia", "Mateusz Malinowski"
    ,"Matias D. Cattaneo", "Matja\382 Perc", "Matt Blaze", "Matt Gardner", "Matt Hoffman"
    ,"Matt Kaeberlein", "Matt McGue", "Matt Richardson", "Matt Sharifi", "Matteo Bianchi"
    ,"Matteo Carandini", "Matteo Fischetti", "Matthew A. Brown", "Matthew Allison", "Matthew Baldwin"
    ,"Matthew Butterick", "Matthew C. Keller", "Matthew Cobb", "Matthew Connelly", "Matthew Cook"
    ,"Matthew E. Kahn", "Matthew Gentzkow", "Matthew Haag", "Matthew Hill", "Matthew Hurles"
    ,"Matthew Hutchinson", "Matthew J. Hart", "Matthew J. Salganik", "Matthew K. Nock", "Matthew Kimble"
    ,"Matthew Lang", "Matthew McGue", "Matthew Meyer", "Matthew Nicholson", "Matthew O. Jackson"
    ,"Matthew P. Walker", "Matthew S. Johnson", "Matthew Waller", "Matthew Weeks", "Matthew Weinzierl"
    ,"Matthew Yu", "Matthias Doepke", "Matthias Egger", "Matthias Endres", "Matthias Grossglauser"
    ,"Matthias H. Tsch\246p", "Matthias Lange", "Matthias Meyer", "Matthias Niessner", "Matthias Schonlau"
    ,"Matthias Schwab", "Maureen Dowd", "Maureen E. Raymo", "Maurice Ptito", "Mauricio R. Delgado"
    ,"Mauricio Tohen", "Max Abrahms", "Max Bain", "Max Bazerman", "Max Norman"
    ,"Max Pfeiffer", "Max Reuter", "Max Rose", "Max Tegmark", "Max Weiss"
    ,"Max Welling", "Max Yates", "Maxwell Goldstein", "May Chen", "Maya Shankar"
    ,"Maya Yamazaki", "Mayank Mishra", "Megan Cooke", "Megan Shanahan", "Megan Smith"
    ,"Megan Twohey", "Mehdi Bennani", "Mehryar Mohri", "Mei Li", "Meike Bartels"
    ,"Melanie Davies", "Melanie Mitchell", "Melanie O\8217Leary", "Melanie Walsh", "Melinda Mills"
    ,"Melissa A. Wilson", "Melissa Dell", "Melissa Hardy", "Melvin Johnson", "Melvyn B. Nathanson"
    ,"Menachem Kaiser", "Menachem Stern", "Menelas N. Pangalos", "Meng Jiang", "Meredith Ringel Morris"
    ,"Meredith Yeager", "Met Opera", "Mette Thomsen", "Mi Zhang", "Miao Hua"
    ,"Michael A. Schneider", "Michael A. Sutton", "Michael A. Taylor", "Michael Allaby", "Michael B. Bracken"
    ,"Michael B. Elowitz", "Michael B. Fossel", "Michael Bang Petersen", "Michael Boehnke", "Michael Bolin"
    ,"Michael Bond", "Michael Bowling", "Michael Bronstein", "Michael Bunce", "Michael C. Frank"
    ,"Michael C. Jensen", "Michael C. Neale", "Michael C. O\8217Donovan", "Michael C. Seto", "Michael Cantor"
    ,"Michael Chang", "Michael Chung", "Michael Crossley", "Michael D. Fox", "Michael D. Rugg"
    ,"Michael Daniels", "Michael Deakin", "Michael E. Goddard", "Michael E. Greenberg", "Michael E. Hochberg"
    ,"Michael Elad", "Michael Elowitz", "Michael Erard", "Michael Ettlinger", "Michael Frank"
    ,"Michael Goddard", "Michael Greenstone", "Michael Gschwind", "Michael Hahn", "Michael Hauser"
    ,"Michael Hornberger", "Michael Hout", "Michael Huemer", "Michael H\228usser", "Michael I. Jordan"
    ,"Michael I. Norton", "Michael I. Posner", "Michael Inzlicht", "Michael J. Black", "Michael J. Joyner"
    ,"Michael J. Mina", "Michael J. Spivey", "Michael J. Tarr", "Michael J. Thun", "Michael J. Welsh"
    ,"Michael J. Wilkins", "Michael Kandel", "Michael Kearney", "Michael Kremer", "Michael Kuhn"
    ,"Michael Kumhof", "Michael L. Littman", "Michael L. Tushman", "Michael Lappert", "Michael Levitt"
    ,"Michael Lewis", "Michael Li", "Michael Littman", "Michael Lounsbury", "Michael Lynskey"
    ,"Michael Macy", "Michael Maschler", "Michael Mathieu", "Michael McCloskey", "Michael Mitzenmacher"
    ,"Michael Moritz", "Michael Moutoussis", "Michael Muthukrishna", "Michael Neale", "Michael O\8217Boyle"
    ,"Michael O\8217Donovan", "Michael P. Snyder", "Michael Preuss", "Michael Ristow", "Michael Roden"
    ,"Michael Rosenblum", "Michael Rubinstein", "Michael Rutter", "Michael S. Weisbach", "Michael Sheetz"
    ,"Michael Specter", "Michael Stokes Paulsen", "Michael Stringer", "Michael Swanwick", "Michael T. Gabbett"
    ,"Michael T. Hannan", "Michael Tomasello", "Michael Travisano", "Michael W. Clune", "Michael Wertheimer"
    ,"Michael Wigler", "Michael Zhang", "Michael von Gr\252nau", "Michal Irani", "Michal Kosinski"
    ,"Michel Boivin", "Michel Georges", "Michel Lang", "Michel Poulain", "Michele K. Evans"
    ,"Michele Knobel", "Michele Ramsay", "Michelle Chan", "Michelle Herman", "Michelle Kim"
    ,"Michelle LaRue", "Michelle Luciano", "Mich\232le Ramsay", "Microsoft Research", "Miguel A. L. Nicolelis"
    ,"Miguel Bernardo", "Miguel Casas", "Miguel Delibes", "Mihaela van der Schaar", "Mihaly Csikszentmihalyi"
    ,"Miia Kivipelto", "Mike Burrows", "Mike Darwin", "Mike Friedman", "Mike Markkula"
    ,"Mike Paterson", "Mike Stoolmiller", "Mike Tomkies", "Mikel Artetxe", "Mikhail Sablin"
    ,"Mikkel Thorup", "Mila Haugova", "Milan M. \262irkovi\263", "Milan Rufus", "Milan Vojnovic"
    ,"Miles A. Tinker", "Miles Hewstone", "Milo Urban", "Milos R. Popovic", "Milton Diamond"
    ,"Min Gu", "Min Sun", "Min Xu", "Min Yang", "Min Zhuo"
    ,"Ming C. Lin", "Ming Li", "Ming T. Tsuang", "Ming Tsuang", "Ming Yan"
    ,"Mingxuan Wang", "Mingyan Liu", "Minh Nguyen", "Mira Murati", "Mirella Lapata"
    ,"Miron Zuckerman", "Miroslav V\225lek", "Mitchell J. Nathan", "Mohamed Khamis", "Mohammad Azam Khan"
    ,"Mohammad Ishaq", "Mohammad Norouzi", "Mohammad Saleh", "Mohammed Fazil", "Mohammed Madouh"
    ,"Molly Crockett", "Molly Przeworski", "Mona Diab", "Mona Lynch", "Moni Naor"
    ,"Monica Driscoll", "Monica Morell", "Monica S. Lam", "Monique M. B. Breteler", "Montgomery Slatkin"
    ,"Montserrat Garc\237a-Closas", "Mor Naaman", "Morgan Stanley", "Morris H. DeGroot", "Morris Moscovitch"
    ,"Morten Johansen", "Morten Sorensen", "Morton H. Halperin", "Moses Charikar", "Mosharaf Chowdhury"
    ,"Moshe Szyf", "Mostafa Abdou", "Moustapha Cisse", "Moxie Marlinspike", "Mrinal Kalakrishnan"
    ,"Muhammad Ghous", "Muhammad Shahzad", "Muin J. Khoury", "Munir Pirmohamed", "Murielle Bochud"
    ,"Murray Shanahan", "Mustafa Suleyman", "Myrna M. Weissman", "N. Asokan", "N. Graham"
    ,"N. Gregory Mankiw", "Na Hu", "Na Luo", "Nachoem Wijnberg", "Naftali Tishby"
    ,"Nakul Verma", "Nambury S. Raju", "Nan Laird", "Nan Lin", "Nancy A. Moran"
    ,"Nancy Adler", "Nancy Bayley", "Nancy C. Andreasen", "Nancy Fulda", "Nancy J. Cox"
    ,"Nancy Kanwisher", "Nancy L. Pedersen", "Nancy L. Segal", "Nancy Pedersen", "Nancy Qian"
    ,"Nancy Wang", "Nando de Freitas", "Naoki Murata", "Naoki Sato", "Naomi Habib"
    ,"Naomi Matsumoto", "Naomi R. Wray", "Naomi Wray", "Nat Friedman", "Natalia Ponomareva"
    ,"Nathan A. Fox", "Nathan Brody", "Nathan Lambert", "Nathan W. Pyle", "Nathan Ward"
    ,"Nathaniel David", "Nathaniel Roberts", "Nathaniel Weyl", "National Academies of Sciences", "Naveed Sattar"
    ,"Navjot Kaur", "Navjot Singh", "Neal L. Benowitz", "Neal Stephenson", "Neale Mahoney"
    ,"Neil Malhotra", "Neil Perry", "Neil Risch", "Neil Robertson", "Neil Sloane"
    ,"Neil Small", "Neil Weste", "Nell Greenfieldboyce", "Nelson Cowan", "Nelson Goodman"
    ,"Nenad Sestan", "New York Times", "Nicholas A. Christakis", "Nicholas Bloom", "Nicholas Christakis"
    ,"Nicholas Evans", "Nicholas Fernando", "Nicholas G. Martin", "Nicholas Graham", "Nicholas Gray"
    ,"Nicholas H. Barton", "Nicholas Humphrey", "Nicholas J. Wareham", "Nicholas J. White", "Nicholas Johnson"
    ,"Nicholas Nicastro", "Nicholas Padilla", "Nicholas Polson", "Nicholas Schwab", "Nicholas Trainor"
    ,"Nicholas Wareham", "Nicholas Wheeler", "Nick Barton", "Nick Black", "Nick Chater"
    ,"Nick Craddock", "Nick Fortugno", "Nick Montfort", "Nick Morgan", "Nick Walton"
    ,"Nicky Best", "Nicola Clayton", "Nicola Persico", "Nicolai Rubinstein", "Nicolas Perrin"
    ,"Nicole Boivin", "Nicole Graf", "Nicole Lazar", "Nicole Pratt", "Nicole Schupf"
    ,"Nicole Soranzo", "Nigel Paneth", "Nigel R. Franks", "Nigel Slater", "Nigel Stocks"
    , "Nikhil Naik", "Nikhil Tandon", "Niklaus Wirth", "Nikolai Fyodorovich Fyodorov"
    ,"Nikolaos Makris", "Nikolaos Papanikolopoulos", "Nikolaos Pappas", "Nikolaus Rajewsky", "Nikolay Karpov"
    ,"Nilanjan Chatterjee", "Nilay Patel", "Nils Henriksson", "Nils Lid Hjort", "Nina G. Jablonski"
    ,"Nina Singh", "Nina Wedell", "Ning Ding", "Ning Lu", "Ning Zhang"
    ,"Nir Baram", "Nir Barzilai", "Nir Eyal", "Nir Friedman", "Nir Shavit"
    ,"Niren Murthy", "Nita G. Forouhi", "Noah A. Rosenberg", "Noah Carl", "Noah Fierer"
    ,"Noah Jones", "Nobuo Sasaki", "Nobuyuki Sato", "Nolan Miller", "Nora D. Volkow"
    ,"Nora S. Newcombe", "Norbert Benecke", "Norbert Schwarz", "Norbert Wiener", "Norman F. Maier"
    ,"Norman Hammond", "Norman Hardy", "Norman Jouppi", "Norman McQuown", "Norman P. Jouppi"
    ,"Norman R. F. Maier", "Norman Sartorius", "Norman Thomas di Giovanni", "Norwood Russell Hanson", "Novo Nordisk"
    ,"No\233mie Elhadad", "Nupur Lala", "Nuria Oliver", "Nutrition Reviews", "Ofer Dekel"
    ,"Oh You Pretty Things", "Ola Hansson", "Olaf Blanke", "Olaf Sporns", "Olav Dalgard"
    ,"Olav Sorenson", "Ole Henrik Magga", "Oleg Sushkov", "Olga Russakovsky"
    ,"Oliver Grimm", "Oliver Heaviside", "Oliver Martinez", "Oliver Zhang", "Olivia Johnson"
    ,"Olivier Fran\231ois", "Olivier Klein", "Olle H\228ggstr\246m", "Olof Johansson", "Olufunmilayo I. Olopade"
    ,"Omar Agha", "Omar Cortes", "Omar Wasow", "Onur G\252nt\252rk\252n", "Open Science Collaboration"
    ,"Ophelia Deroy", "Ophir Klein", "Oren Etzioni", "Oriol Vinyals", "Orrin Devinsky"
    ,"Oscar Kempthorne", "Oskar van Deventer", "Oystein Sorensen", "P. A. P. Moran"
    ,"P. C. Mahalanobis", "P. Eline Slagboom", "P. N. Suganthan", "P22 Type Foundry", "Pablo Kuri-Morales"
    ,"Pablo Villalobos", "Pak Chung Sham", "Pak Sham", "Pallab Ghosh", "Palwasha Khan"
    ,"Pamela A. Silver", "Pamela B. Davis", "Pamela Herd", "Pamela McCorduck", "Pamela Sklar"
    ,"Pan Wei", "Pan Zhang", "Panayiota Poirazi", "Pang Yao", "Pankaj Patel"
    ,"Pankaj Sharma", "Panos Deloukas", "Paola Giuliano", "Paola Sebastiani", "Paolo Boffetta"
    ,"Paolo Fusar-Poli", "Parag Pathak", "Paras Jain", "Pardis C. Sabeti", "Paresh Dandona"
    ,"Paris Buttfield-Addison", "Parker Barnes", "Parker Hill", "Partha Ghosh", "Parthasarathy Ranganathan"
    ,"Pascal Boyer", "Pascal Vincent", "Pascale Fung", "Pascaline Dupas", "Patricia A. Reuter-Lorenz"
    ,"Patricia Coogan", "Patricia Devine", "Patricia H. Miller", "Patricia J. Bauer", "Patricia L. Dudley"
    ,"Patricia M. Clancy", "Patricia Michie", "Patricia Moore", "Patricia T. Michie", "Patricio Saavedra"
    ,"Patrick Baud", "Patrick Bolton", "Patrick Collison", "Patrick D. Wall", "Patrick E. McGovern"
    ,"Patrick Esser", "Patrick F. Sullivan", "Patrick Fournier", "Patrick J. Curran", "Patrick J. McGrath"
    ,"Patrick J. Morris", "Patrick J. Stover", "Patrick Kline", "Patrick Kruger", "Patrick Leahy"
    ,"Patrick Markey", "Patrick Maynard Stuart Blackett", "Patrick McClure", "Patrick McDaniel", "Patrick McGorry"
    , "Patrick Phillips", "Patrick Reed", "Patrick Thiran", "Patrick Wyatt"
    ,"Patrik Magnusson", "Pattie Maes", "Paul A. David", "Paul A. Johnson", "Paul A. Samuelson"
    ,"Paul B. Baltes", "Paul Barham", "Paul Christiano", "Paul Covington", "Paul D. Adams"
    ,"Paul E. Meehl", "Paul E. Peterson", "Paul F. Lazarsfeld", "Paul Fussell", "Paul G. Kwiat"
    ,"Paul Glasziou", "Paul Grieco", "Paul H. Nitze", "Paul Irwing", "Paul J. Heald"
    ,"Paul J. LeBlanc", "Paul Krugman", "Paul L. Harris", "Paul M. Ridker", "Paul M. Romer"
    ,"Paul Milgrom", "Paul Nyquist", "Paul P. Glasziou", "Paul R. Jones", "Paul Resnick"
    ,"Paul Rozin", "Paul S. Appelbaum", "Paul S. Freemont", "Paul Samuelson", "Paul Sedille"
    ,"Paul Smolensky", "Paul Stamets", "Paul Syverson", "Paul Thagard", "Paul Thomas Young"
    ,"Paul Tough", "Paul Veyne", "Paul Vitanyi", "Paul W. Brand", "Paul W. Ewald"
    ,"Paul W. Franks", "Paul Webley", "Paul Weindling", "Paul van Geert", "Paul van Oorschot"
    ,"Paula R. Pietromonaco", "Paula Smith", "Paulien Hogeweg", "Pauline Smith", "Paulo Shakarian"
    ,"Pavel A. Pevzner", "Pavel Drozd", "Peder Mortensen", "Pedro Domingos", "Pedro Henrique Martins"
    ,"Pedro Mercado", "Pedro Sanchez", "Peer Bork", "Peggy J. Kleinplatz", "Pei-Chun Shih"
    ,"Peng Chen", "Peng Cheng", "Peng Lin", "Peng Qi", "Peng Shi"
    ,"Peng Sun", "Peng Tee Khaw", "Peng Wu", "Peng Zhao", "Peng Zhou"
    ,"Penn State University", "Penny Gordon-Larsen", "Perdita Barran", "Perminder Sachdev", "Persi Diaconis"
    ,"Pete Hatemi", "Peter Arcidiacono", "Peter B. Medawar", "Peter Beattie", "Peter C. B. Phillips"
    ,"Peter C. Fishburn", "Peter C. G\248tzsche", "Peter Craig", "Peter D. Eimas", "Peter Dayan"
    ,"Peter Donnelly", "Peter E. Hart", "Peter Eisenman", "Peter F. Buckley", "Peter Franks"
    ,"Peter Gacs", "Peter Godfrey-Smith", "Peter Goodhand", "Peter Grassberger", "Peter H. Rossi"
    ,"Peter H. Schonemann", "Peter Hagoort", "Peter Hajek", "Peter J. Bentley", "Peter J. Boettke"
    ,"Peter J. Campbell", "Peter J. Denning", "Peter J. Rentfrow", "Peter J. Richerson", "Peter J. Wagner"
    ,"Peter Jaros", "Peter K. Gregersen", "Peter K. Hatemi", "Peter Klatsky", "Peter Kun"
    ,"Peter M. Kogge", "Peter M. Lansdorp", "Peter M. Neumann", "Peter M. Visscher", "Peter McGuffin"
    ,"Peter McLeod", "Peter N. Devreotes", "Peter Naur", "Peter Norvig", "Peter Nowak"
    ,"Peter Palese", "Peter Pistanek", "Peter Propping", "Peter Richtarik", "Peter Riederer"
    ,"Peter S. Bearman", "Peter Sheridan Dodds", "Peter Shor", "Peter St George-Hyslop", "Peter Stansky"
    ,"Peter Straub", "Peter Szatmari", "Peter T. Campbell", "Peter T. Fox", "Peter T. Leeson"
    ,"Peter Thiel", "Peter Todd", "Peter Wade", "Peter Watts", "Peter Winkler"
    ,"Peter Wonka", "Peter Wu", "Peter Zajac", "Peter de Jonge", "Petr Beckmann"
    ,"Petra Moser", "Pew Research Center", "Peyman Milanfar", "Phil Lee", "Philip  L. De Jager"
    ,"Philip A. Vernon", "Philip Asherson", "Philip Awadalla", "Philip Ball", "Philip De Jager"
    ,"Philip E. Tetlock", "Philip E. Vernon", "Philip J. Davis", "Philip K. Maini", "Philip Kraft"
    ,"Philip L. De Jager", "Philip Mansfield", "Philip Sabin", "Philip Scheltens"
    ,"Philip Winston", "Philip Yancey", "Philip Zhao", "Philipp Kanske", "Philipp Koehn"
    ,"Philippe Autier", "Philippe Charlier", "Philippe Ciais", "Philippe Flajolet", "Philippe Froguel"
    ,"Philippe Kruchten", "Phillip A. Sharp", "Phyllis Chesler", "Pierre Baldi", "Pierre Desrochers"
    ,"Pierre Pinson", "Pierre-Simon Laplace", "Pierre-Yves Oudeyer", "Pieter Abbeel", "Pietro De Camilli"
    ,"Pietro Perona", "Pinchas Cohen", "Planet Money", "Polina Anikeeva"
    , "Pontiano Kaleebu", "Pontus Skoglund", "Pony Preservation Project"
    , "Poul-Henning Kamp", "Pradeep Dubey", "Pradeep Sharma", "Pranab Bardhan", "Prateek Jain"
    ,"Preslav Nakov", "Price V. Fishback", "Priya Krishna", "Przemyslaw Prusinkiewicz", "Psychiatric Genomics Consortium"
    ,"Pushmeet Kohli", "Pushpak Bhattacharyya", "Qi Cao", "Qi Dong", "Qi Guo"
    ,"Qi Sun", "Qi Tian", "Qi Zhou", "Qi Zhu", "Qian Liu"
    ,"Qian Qin", "Qian Tang", "Qian Zhang", "Qian Zhao", "Qiang Du"
    ,"Qiang Li", "Qiang Shen", "Qiang Yang", "Qiao Hu", "Qin Zhang"
    ,"Qingyun Wang", "Quan Gan", "Quan Yuan", "Quinn McNemar", "Quoc V. Le"
    ,"R. A. Fisher", "R. A. Lafferty", "R. A. McConnell", "R. A. Radford", "R. B. Braithwaite"
    ,"R. E. Peterson", "R. G. Collingwood", "R. J. Mical", "R. Kent Dybvig", "R. Knight"
    ,"R. L. Glass", "R. L. Gregory", "R. L. Mills", "R. L. Plackett", "R. M. May"
    ,"R. M. Murray", "R. N. Bracewell", "R. Preston McAfee", "R. S. Woodworth", "R. Scott Bakker"
    ,"R. Stanley Williams", "R. Travis Osborne", "R. W. Brockett", "R. Watts", "Rachel Barney"
    ,"Rachel Batterham", "Rachel Bernstein", "Rachel Laudan", "Rachelle S. Doody", "Rachid Guerraoui"
    ,"Radha Poovendran", "Rae Silver", "Rafael Salas", "Rafael de Cabo", "Raffi Khatchadourian"
    ,"Raghu Rajan", "Ragnar Bjarnason", "Raheem Beyah", "Rahul Desikan", "Rahul Gupta"
    ,"Rahul Joshi", "Rahul Sarpeshkar", "Raja Mukherjee", "Rajeev Motwani", "Rajesh P. N. Rao"
    ,"Rajiv Sharma", "Ralph A. Bagnold", "Ralph C. Merkle", "Ralph Hertwig", "Ralph L. Sacco"
    ,"Ralph Rugoff", "Ralph V. L. Hartley", "Ralph W. Hull", "Ramesh Karri", "Ramesh Menon"
    ,"Ramesh Raskar", "Ramesh Sitaraman", "Ramon Reyes", "Ran Tao", "Ran Tian"
    ,"Ranajit Chakraborty", "Randal Burns", "Randall Brown", "Randall Collins", "Randall Munroe"
    ,"Randall W. Engle", "Randi Hutter Epstein", "Randy Gomez", "Randy L. Buckner", "Ranveer Chandra"
    ,"Raphael Koster", "Rapha\235l Li\233geois", "Raquel E. Gur", "Raquel Fern\225ndez", "Raquel Gur"
    ,"Raquel Urtasun", "Rasmus Kleis Nielsen", "Rasmus Larsen", "Raul Gonzalez", "Ravi Gupta"
    ,"Ravi Kalhan", "Ravishankar Iyer", "Ray Bradbury", "Ray Bull", "Ray Kurzweil"
    ,"Ray L. Birdwhistell", "Ray Monk", "Rayhan Asat", "Raymond B. Cattell", "Raymond E. Fancher"
    , "Raymond J. Dolan", "Raymond J. Mooney", "Raymond Mooney", "Raymond Palmer"
    ,"Raymond S. Nickerson", "Raymond Walters", "Raza Muhammad", "Razib Khan", "Rebecca A. Hubbard"
    ,"Rebecca Andridge", "Rebecca D. Jackson", "Rebecca Diamond", "Rebecca Goldstein", "Rebecca Landa"
    ,"Rebecca Redfern", "Rebecca Saxe", "Rebecca Shiner", "Rebecca Sims", "Rebecca Struthers"
    ,"Rebecca Surman", "Rebecca W. Rimel", "Rebecca Walsh", "Rebecca Willett", "Regina Barzilay"
    ,"Regina G. Ziegler", "Regina James", "Reid Hoffman", "Reiji Suzuki", "Reiko Tomii"
    ,"Reinhard Wilhelm", "Reinhardt M\248bjerg Kristensen", "Reinhold Schmidt", "Reka Nagy", "Remi Gribonval"
    ,"Ren Ng", "Rene Hurlemann", "Rene J. Dubos", "Rene Vidal", "Renee Johnson"
    ,"Ren\233 J. Dubos", "Ren\233 S. Kahn", "Ren\233 Veenstra", "Reuben Hersh", "Reuven Gal"
    ,"Rex Jung", "Reza Malekzadeh", "Rich Sutton", "Richard A. Friedman", "Richard A. Gardner"
    ,"Richard A. Harshman", "Richard A. Jensen", "Richard A. Miller", "Richard A. Posner", "Richard A. Weinberg"
    ,"Richard Andrew", "Richard Baraniuk", "Richard Bonneau", "Richard Bruggeman", "Richard C. Trembath"
    ,"Richard D. Arvey", "Richard Danzig", "Richard Delgado", "Richard Dobson", "Richard Doll"
    ,"Richard E. Boyatzis", "Richard E. Carey", "Richard E. Lenski", "Richard E. Mayer", "Richard E. Nisbett"
    ,"Richard E. Snow", "Richard E. Tremblay", "Richard E. Turner", "Richard Everett", "Richard Feynman"
    ,"Richard G. Baraniuk", "Richard G. Frank", "Richard Gibbons", "Richard H. Thaler", "Richard Hamming"
    ,"Richard Hanania", "Richard Ho", "Richard J. Davidson", "Richard J. Haier", "Richard J. Hatchett"
    ,"Richard J. Lipton", "Richard J. Murnane", "Richard J. Rose", "Richard J. Samuels", "Richard J. Shaw"
    ,"Richard Jozsa", "Richard K. Wilson", "Richard Kronick", "Richard L. Pyle", "Richard L. Solomon"
    ,"Richard Landes", "Richard Lathe", "Richard Li", "Richard Liu", "Richard Lynn"
    ,"Richard M. Karp", "Richard M. Myers", "Richard Maudsley", "Richard N. Aslin", "Richard N. Holdaway"
    ,"Richard O. Duda", "Richard P. Bentall", "Richard P. Ebstein", "Richard P. Feynman", "Richard P. Lifton"
    ,"Richard Peto", "Richard S. Bird", "Richard S. Cooper", "Richard S. Sutton", "Richard Sandford"
    ,"Richard Schmalensee", "Richard Villems", "Richard W. Grant", "Richard W. Hamming", "Richard W. Murray"
    ,"Richard W. Wrangham", "Richard Wiseman", "Richard Zeckhauser", "Richard Zemel"
    ,"Richie Poulton", "Rick A. Kittles", "Rick Dale", "Rick Kittles", "Rik Vandenberghe"
    ,"Rishi Sharma", "Risto Miikkulainen", "Rita Patel", "Roald Dahl", "Rob M. van Dam"
    ,"Rob Reich", "Rob Stein", "Robb Willer", "Robert A. Baker", "Robert A. Baron"
    ,"Robert A. Bjork", "Robert A. Brightman", "Robert A. Edwards", "Robert A. Gordon", "Robert A. Millikan"
    ,"Robert A. Scott", "Robert A. Wild", "Robert B. Cialdini", "Robert B. Darnell", "Robert B. Wallace"
    ,"Robert Baldwin", "Robert Bjork", "Robert Brandon", "Robert Bringhurst", "Robert Brustein"
    ,"Robert C. Allen", "Robert C. Barber", "Robert C. Edgar", "Robert C. Elston", "Robert C. Green"
    ,"Robert C. Nichols", "Robert C. Thompson", "Robert C. Wilson", "Robert Cardona", "Robert Cialdini"
    ,"Robert D. Gibbons", "Robert Desimone", "Robert Doyle", "Robert E. Bixby", "Robert E. Emery"
    ,"Robert F. Krueger", "Robert Feldman", "Robert Frank", "Robert Gaskins", "Robert Gerber"
    ,"Robert Gerlai", "Robert Greenfield", "Robert H. Brower", "Robert Harcourt", "Robert Harry Socolow"
    ,"Robert Hobbs", "Robert Irvine", "Robert J. Aumann", "Robert J. Gordon", "Robert J. Harvey"
    ,"Robert J. LaLonde", "Robert J. MacCoun", "Robert J. Nemiroff", "Robert J. Shiller", "Robert J. Sternberg"
    ,"Robert Kaestner", "Robert Kanigel", "Robert Karlsson", "Robert Kleinberg", "Robert Kolker"
    ,"Robert Krulwich", "Robert Kurzban", "Robert L. Forward", "Robert L. Linn", "Robert L. Paarlberg"
    ,"Robert L. Sack", "Robert L. Spitzer", "Robert L. Thorndike", "Robert Langer"
    ,"Robert Leeper", "Robert Lerner", "Robert M. Bond", "Robert M. Hauser", "Robert M. May"
    ,"Robert M. Solow", "Robert M. Thorndike", "Robert M. Yerkes", "Robert Maier", "Robert Mankoff"
    ,"Robert Martinson", "Robert Meier", "Robert Metcalfe", "Robert Mullins", "Robert P. Abelson"
    ,"Robert Plomin", "Robert R. Jackson", "Robert R. Sears", "Robert Ralston", "Robert Root-Bernstein"
    ,"Robert Rudolf", "Robert S. Rosenson", "Robert S. Woodworth", "Robert Schlaifer", "Robert Schweitzer"
    ,"Robert Scoble", "Robert Seamans", "Robert Slimbach", "Robert Stickgold", "Robert T. Knight"
    ,"Robert T. Pennock", "Robert Tibshirani", "Robert W. Brooks", "Robert W. Cox", "Robert W. McCarley"
    ,"Robert W. Williams", "Robert Whelan", "Robert Wiblin", "Robert Wishart", "Robert \214stling"
    ,"Roberta Sinatra", "Roberto Azevedo", "Roberto Car", "Roberto Cipolla", "Roberto Colom"
    ,"Roberto Esposito", "Roberto Toro", "Robin Carhart-Harris", "Robin Goldstein", "Robin Hanson"
    ,"Robin Haring", "Robin Lovell-Badge", "Robin M. Murray", "Robin Saikia", "Robin Young"
    ,"Robyn Dawes", "Robyn Forbes", "Rochelle Buffenstein", "Roderic Broadhurst", "Rodney L. Lowman"
    ,"Roger B. Myerson", "Roger Broughton", "Roger C. Schank", "Roger Carpenter", "Roger D. Cone"
    ,"Roger G. Barker", "Roger Lewin", "Roger N. Shepard", "Roger R. Schell", "Roger T. Hanlon"
    ,"Roger Wattenhofer", "Rohan Gunaratna", "Rohan Taylor", "Rohit Prakash", "Rohit Varma"
    ,"Roland G. Fryer", "Roland R. Griffiths", "Rolf Dobelli", "Roman Kotov", "Ron Milo"
    ,"Ron Rosenbaum", "Ronald A. Fisher", "Ronald A. Howard", "Ronald C. Kessler", "Ronald Davis"
    ,"Ronald Duncan", "Ronald J. Williams", "Ronald K. Siegel", "Ronald Kessler", "Ronald Kim"
    , "Ronald L. Simons", "Ronald Rogowski", "Ronald S. Burt", "Ronald S. Wilson"
    ,"Ronald W. Davis", "Ronen Eldan", "Ronen Segev", "Rong Zhang", "Ronin Network"
    ,"Ronke Olabisi", "Rory Collins", "Rosalind Picard", "Rosalind Raine", "Rose Andrew"
    ,"Roshan Cools", "Roshan Rao", "Ross Goodwin", "Ross J. Baldessarini", "Ross L. Prentice"
    ,"Ross Levine", "Ross Taylor", "Ross Wightman", "Roula Khalaf", "Roy Baumeister"
    ,"Roy F. Baumeister", "Roy H. Campbell", "Roy M. Anderson", "Roy Pea", "Rudolf Arnheim"
    ,"Rudolf Fehrmann", "Rudolf Uher", "Rudolph E. Tanzi", "Rudyard Kipling", "Rui Xu"
    ,"Ruixiang Zhang", "Ruma Falk", "Rumena Bu\382arovska", "Rune Jacobsen", "Ruslan Salakhutdinov"
    ,"Russ Altman", "Russel J. Reiter", "Russell A. Poldrack", "Russell Coleman", "Russell Impagliazzo"
    ,"Russell Lande", "Russell Roberts", "Russell Spears", "Russell Thomson", "Ruth Armstrong"
    ,"Ruth E. Ley", "Ruth Faden", "Ruth Johnson", "Ruth M. J. Byrne", "Ruth Mace"
    ,"Ruth Nussinov", "Ruth O\8217Hara", "Ruth Shonle Cavan", "Ruut Veenhoven", "Ryan Bradley"
    ,"Ryan Dancey", "Ryan Espiritu", "Ryan Gallagher", "Ryan Henderson", "Ryan Holiday"
    ,"Ryan Julian", "Ryan Sullivan", "Ryusuke Hikawa", "Ryuta Kawashima", "Ryuzo Yanagimachi"
    ,"R\252diger Fahlenbrach", "S. Alexandra Burt", "S. E. Harris", "S. Kumar", "S. W. Lewis"
    ,"Saahil Jain", "Sabine Fuss", "Sachin Kumar", "Sachin Ravi", "Sadek Wahba"
    ,"Sagar Shah", "Sahib Singh", "Sahil Patel", "Salil Vadhan", "Salim Yusuf"
    ,"Sally Satel", "Salman Avestimehr", "Sam Altman", "Sam Blackwell", "Sam Dodge"
    ,"Sam Gross", "Sam Kean", "Sam Kwong", "Sam Mercer", "Sam Peltzman"
    ,"Sam Ritter", "Samantha Watson", "Sami Haddadin", "Samir Arora", "Samir Saba"
    ,"Samuel A. Stouffer", "Samuel D. Gosling", "Samuel Ginn", "Samuel Gosling", "Samuel J. Holmes"
    ,"Samuel L. Braunstein", "Samuel L. Smith", "Samuel R. Buss", "Samuel S. Kortum"
    ,"Samuel S. Wineburg", "Samuel T. Cohen", "Samy Bengio", "Sana Amanat", "Sandeep Singh"
    ,"Sander Greenland", "Sander van der Linden", "Sandra Scarr", "Sandra Weintraub", "Sandu Popescu"
    ,"Sang-Won Park", "Sanghamitra Mohanty", "Sanja Fidler", "Sanjay Asthana", "Sanjay Ghemawat"
    ,"Sanjay Jain", "Sanjay Srivastava", "Sanjeev Arora", "Sanjiv Kumar", "Santosh Vempala"
    ,"Sapna Maheshwari", "Sara A. Solla", "Sara Garcia", "Sara M. Lewis", "Sara Moreira"
    ,"Sara Seager", "Sarah A. Tishkoff", "Sarah Byford", "Sarah Chen", "Sarah E. Anderson"
    ,"Sarah Ennis", "Sarah Haider", "Sarah Henderson", "Sarah Jeong", "Sarah Kreps"
    ,"Sarah Lindsay", "Sarah Mathew", "Sarah Otto", "Sarah Wild"
    ,"Sarah York", "Sarah-Jayne Blakemore", "Sarit Kraus", "Sarnoff A. Mednick", "Satinder Singh"
    ,"Satoshi Iizuka", "Satoshi Matsuoka", "Satoshi Nakamoto", "Satya Nadella", "Saul Perlmutter"
    ,"Saul Rosenzweig", "Saurabh Sinha", "Saurabh Tiwary", "Sayan Ghosh", "Sayantan Das"
    ,"Scale AI", "Scott Aaronson", "Scott Cunningham", "Scott E. Fahlman", "Scott Heiner"
    ,"Scott Johnston", "Scott Keeney", "Scott Mahlke", "Scott McCoy", "Scott O. Lilienfeld"
    ,"Scott Pelley", "Scott Shannon", "Scott Small", "Scott W. Brady", "Seamus Heaney"
    ,"Sean Bell", "Sean F. Reardon", "Sean M. Carroll", "Sean M. Ryan", "Sean Mayes"
    ,"Sean O\8217Keeffe", "Sean Patterson", "Sean Stevens", "Sebastian Brather", "Sebastian Deffner"
    ,"Sebastian Deterding", "Sebastian Thrun", "Sebastien Bubeck", "Sebastien Lefebvre", "Sekar Kathiresan"
    ,"Sendhil Mullainathan", "Seon-Young Kim", "Seonghyeon Kim", "Sepp Hochreiter", "Serena Chen"
    ,"Serge Belongie", "Sergei Guriev", "Sergey Brin", "Sergey Gavrilets", "Sergey Koren"
    ,"Sergey Malitsky", "Sergey Markov", "Sergey Melnikov", "Sergey Titov", "Sergio Boixo"
    ,"Sergio Casas", "Sergio Della Sala", "Sergio Guadarrama", "Seth Godin", "Seth Lloyd"
    ,"Seth Roberts", "Sewall Wright", "Seymour Papert", "Shadi Sadr", "Shafi Goldwasser"
    ,"Shaker Ahmed", "Sham Kakade", "Shamsul Huda", "Shan Dong", "Shan Wei"
    ,"Shane Legg", "Shane McCarthy", "Shang Yang", "Shang-Hua Teng", "Shanshan Huang"
    ,"Shaomeng Wang", "Sharon Begley", "Sharon N. DeWitte", "Shaun Purcell", "Shawn Bayern"
    ,"Sheila McIlraith", "Sheldon C. Reed", "Shelley L. Berger", "Shelly Flagel", "Shen Li"
    ,"Sheng Liu", "Sheng Wang", "Sheri Fink", "Sherry Glied"
    ,"Sherry Shi", "Sherwin Rosen", "Shi Feng", "Shih-Chii Liu", "Shih-Fu Chang"
    ,"Shih-Jen Hwang", "Shinji Higuchi", "Shinya Hasegawa", "Shirley Ho", "Shirley Wu"
    ,"Shirly Pinto", "Shivendra Singh", "Shlomo Benartzi", "Shlomo Moran", "Sholto Douglas"
    ,"Shoukhrat Mitalipov", "Shuai Peng", "Shuai Zhang", "Shuai Zhao", "Shuang Zhao"
    ,"Shubham Agarwal", "Shuchi Chawla", "Shuo Wang", "Shwetak Patel", "Si Si"
    ,"Siddhartha Mukherjee", "Sidney Hemming", "Siegfried Bernfeld", "Silvia Paracchini", "Simin Liu"
    ,"Simine Vazire", "Simon Akam", "Simon Baron-Cohen", "Simon Campbell", "Simon Colton"
    ,"Simon E. Fisher", "Simon Fishel", "Simon Grimm", "Simon G\228chter", "Simon Karlsson"
    ,"Simon Kelner", "Simon Kirby", "Simon Lovestone", "Simon Tong", "Simone Bianco"
    ,"Simone Pika", "Simone Rossi", "Sing Bing Kang", "Siobhan Roberts", "Soham Ghosh"
    ,"Sohini Ramachandran", "Solomon E. Asch", "Song-Chun Zhu", "Sonia Shah", "Sonya Williams"
    ,"Sopan Deb", "Sophia Ananiadou", "Sophia Frangou", "Sophie von Stumm", "Soumen Chakrabarti"
    ,"Soumya Raychaudhuri", "Soung Chang Liew", "Sourav Chatterjee", "Souvik Ghosh", "Spectrum 10K"
    ,"Spencer Heath", "Spyros Makridakis", "Sreenivasa Murthy", "Sri Lakshmi", "St. Clair McKelway"
    ,"Stacey B. Gabriel", "Stacey Gabriel", "Staffan Burenstam Linder", "Stan Boutin", "Stan Nelson"
    ,"Stanislas Dehaene", "Stanislav Andreski", "Stanislav Morozov", "Stanislaw Jastrzebski", "Stanislaw Lem"
    ,"Stanis\322aw Lem", "Stanley A. Mulaik", "Stanley Coren", "Stanley L. Engerman", "Stanley M. Garn"
    ,"Stanley Reiter", "Stanley Schachter", "Stanley Zammit", "Stanton A. Glantz", "Stefan Bucher"
    ,"Stefan Johansson", "Stefan R. Bornstein", "Stefan Schaal", "Stefan Szeider", "Stefan Thurner"
    ,"Stefan Wachter", "Stefan Wurster", "Stefan Zary", "Stefania Serafin", "Stefanie Stantcheva"
    ,"Stefano DellaVigna", "Stefano Ugolini", "Steffen Schmidt", "Stella Chen", "Stephan Collishaw"
    ,"Stephan Lewandowsky", "Stephan Ripke", "Stephan Zipfel", "Stephanie Forrest", "Stephanie J. London"
    ,"Stephanie Lin", "Stephanie Lopez", "Stephen Ansolabehere", "Stephen C. Stearns"
    ,"Stephen Ceci", "Stephen Chanock", "Stephen E. Epstein", "Stephen E. Fienberg", "Stephen E. Harris"
    ,"Stephen Franks", "Stephen G. Waxman", "Stephen Hsu", "Stephen J. Ceci", "Stephen J. Elledge"
    ,"Stephen J. O\8217Brien", "Stephen J. Roberts", "Stephen J. Simpson", "Stephen J. Wright", "Stephen L. Carter"
    ,"Stephen L. Macknik", "Stephen LaBerge", "Stephen Laurence", "Stephen M. Omohundro", "Stephen M. Stigler"
    ,"Stephen Machin", "Stephen Marche", "Stephen Meader", "Stephen O\8217Rahilly", "Stephen Pacala"
    ,"Stephen R. Palumbi", "Stephen Schaffer", "Stephen Spencer", "Stephen Stearns", "Stephen T. Ziliak"
    ,"Stephen V. Faraone", "Stephen W. Raudenbush", "Stephen W. Scherer", "Stephen Wolfram", "Stevan J. Arnold"
    ,"Steve Blank", "Steve Hoffmann", "Steve Horvath", "Steve Jurvetson", "Steve Malone"
    ,"Steve Olson", "Steve O\8217Brien", "Steve Ramirez", "Steve Rayner", "Steve Yegge"
    ,"Steven A. Frank", "Steven Bakker", "Steven D. Hollon", "Steven D. Levitt", "Steven D\8217Hondt"
    ,"Steven E. Hyman", "Steven E. Nissen", "Steven Farber", "Steven G. Krantz", "Steven G. Vandenberg"
    ,"Steven Gangestad", "Steven H. Strogatz", "Steven Hsu", "Steven Hyman", "Steven J. Knapp"
    ,"Steven L. Salzberg", "Steven Laureys", "Steven Levy", "Steven Lin", "Steven Liu"
    ,"Steven M. Paul", "Steven N. Blair", "Steven N. Goodman", "Steven N. S. Cheung", "Steven Pinker"
    ,"Steven Reich", "Steven W. Gangestad", "Steven Wheelwright", "Steven Wiltshire", "Stewart Payne"
    ,"Stijn Van Nieuwerburgh", "Stuart Carlson", "Stuart Cheshire", "Stuart E. Dreyfus", "Stuart J. Ritchie"
    ,"Stuart Rogers", "Studio Khara", "Stylianos E. Antonarakis", "Sue Biggins", "Sue Gardner"
    ,"Sue Pedersen", "Suhas Diggavi", "Suji Kim", "Sujit Dey", "Sukhdeep Singh"
    ,"Sultan Mehmood", "Sumio Watanabe", "Sumit Jain", "Sunghyun Cho", "Sunil K. Ahuja"
    ,"Suresh Naidu", "Surya Ganguli", "Susan Anderson", "Susan Athey", "Susan C. Alberts"
    ,"Susan C. Baker", "Susan E. Gathercole", "Susan E. Jackson", "Susan E. Mayer", "Susan Golombok"
    ,"Susan M. Wolf", "Susan Martin", "Susan Nolen-Hoeksema", "Susan Redline", "Susan Smalley"
    ,"Susan Stepney", "Susan T. Fiske", "Susan Thomas", "Susan Whitfield-Gabrieli", "Susan Y. Bookheimer"
    ,"Susana Martinez-Conde", "Susannah Cahalan", "Suzana Herculano-Houzel", "Suzanne Bakken", "Svante P\228\228bo"
    ,"Sven Pettersson", "Svenn Torgersen", "Sy Montgomery", "Sylvia Wassertheil-Smoller", "S\233bastien Bubeck"
    ,"S\233bastien Rouault", "S\248ren Brunak", "T. C. Schneirla", "T. Douglas Price", "T. Hayashi"
    ,"T. J. Hamblin", "T. Lewis", "T. Rado", "T. W. Chaundy", "T. W. Robbins"
    ,"Tad Friend", "Tadayoshi Kohno", "Taha Yasseri", "Taissa S. Hauser", "Takahiro Ogawa"
    ,"Takahiro Suzuki", "Takao Suzuki", "Takashi Abe", "Takashi Gojobori", "Takashi Shinohara"
    ,"Takayuki Ito", "Takuro Yamashita", "Tali Sharot", "Tamara Berg", "Tamer Ba\351ar"
    ,"Tamsin Ford", "Tan Yu", "Tanja Schultz", "Tanya Khovanova", "Tao Li"
    ,"Tao Liu", "Tao Qin", "Tao Sheng", "Tao Yang", "Tao Zhu"
    ,"Tara Javidi", "Tariq Ahmad", "Tariq Ali", "Tarjinder Singh", "Tarun Khanna"
    ,"Tatiana Foroud", "Tatjana Rundek", "Tatsuo Tanaka", "Taylor Stevens", "Teck-Hua Ho"
    ,"Ted Kaehler", "Ted Nettelbeck", "Teddy Seidenfeld", "Teng Li", "Tengfei Shi"
    ,"Terah Lyons", "Terence Tao", "Teresa Amabile", "Teresa Wilson", "Terrence J. Sejnowski"
    ,"Terrie E. Moffitt", "Terrie Moffitt", "Terry Allen Winograd", "Terry Castle", "Terry Gaasterland"
    ,"Terry Winograd", "Tetsuo Najita", "Tetsuya Takahashi", "Thad Starner", "Thalia C. Eley"
    ,"The Associated Press", "The British Psychological Society", "The Express Tribune", "The Harris Poll", "The Metropolitan Opera"
    ,"The Modern House", "The New York Times", "The Onion", "The Public Domain Review", "The Royal Swedish Academy of Sciences"
    ,"Theodore C. Schneirla", "Theodore Caplow", "Theodore Groves", "Theodore P. Hill", "Theodore X. Barber"
    ,"Theodore von K\225rm\225n", "Theodosius Dobzhansky", "Thom Holwerda", "Thomas A. DiPrete", "Thomas A. Guglielmo"
    ,"Thomas A. Rando", "Thomas A. Wadden", "Thomas Blanchet", "Thomas Bourgeron", "Thomas Braun"
    ,"Thomas Buckley", "Thomas C. Edwards", "Thomas Crook", "Thomas Dietterich", "Thomas Duffield"
    ,"Thomas E. Martin", "Thomas E. Moore", "Thomas E. Nichols", "Thomas Eisner", "Thomas Erdbrink"
    ,"Thomas Espeseth", "Thomas Falconer", "Thomas G. Dietterich", "Thomas G. Dunn", "Thomas Gilovich"
    ,"Thomas Hills", "Thomas Hoffman", "Thomas Hofmann", "Thomas Huang", "Thomas I. Miller"
    ,"Thomas J. Bouchard", "Thomas J. Carew", "Thomas Jefferson", "Thomas J\248rgensen", "Thomas K. Landauer"
    ,"Thomas L. Griffiths", "Thomas Lancaster", "Thomas Leopold", "Thomas M. Cover", "Thomas M. Klap\246tke"
    ,"Thomas Mueller", "Thomas Nagel", "Thomas Neumann", "Thomas O\8217Rourke"
    ,"Thomas Paine", "Thomas R. Gingeras", "Thomas S. Dee", "Thomas S. Huang", "Thomas S. Ray"
    ,"Thomas Sch\228fer", "Thomas Warren", "Thomas Wisniewski", "Tian Li", "Tian Wu"
    ,"Tian Xia", "Tianyi Zhang", "Tie-Yan Liu", "Tiffany Stewart", "Tim Byrne"
    ,"Tim D. Spector", "Tim Ferriss", "Tim Finin", "Tim H. Clutton-Brock", "Tim Hesterberg"
    ,"Tim Hutton", "Tim Kraska", "Tim O\8217Reilly", "Tim Roughgarden", "Tim Spector"
    ,"Tim Tully", "Timo Ewalds", "Timothy A. Salthouse", "Timothy Baldwin", "Timothy C. Bates"
    ,"Timothy C. Beers", "Timothy C. May", "Timothy D. Wilson", "Timothy F. Bresnahan", "Timothy Gowers"
    ,"Timothy J. Bartik", "Timothy Jenkins", "Timothy Lillicrap", "Timothy M. Lenton", "Timothy R. Levine"
    ,"Timothy Z. Keith", "Timur Kuran", "Tina Eliassi-Rad", "Tina Goldstein", "Tina Thompson"
    ,"Tina Wang", "Ting Yao", "Ting Zhang", "Ting Zhao", "Tinglong Dai"
    ,"Tingting Jiang", "Tobias Frere-Jones", "Toby Johnson", "Toby Murray", "Toby Ord"
    ,"Todd D. Gould", "Todd K. Shackelford", "Todd Lencz", "Todd Shackelford", "Tom Boone"
    ,"Tom Bryan", "Tom Chadwick", "Tom DeMarco", "Tom Downey", "Tom Eccles"
    ,"Tom Goldstein", "Tom Hope", "Tom Jensen", "Tom Junod", "Tom Mes"
    ,"Tom Nicholas", "Tomas Chamorro-Premuzic", "Tomas Mikolov", "Tomas Olsson", "Tomaso Poggio"
    ,"Tomasz Smole\324", "Tomi Rantam\228ki", "Tommaso Toffoli", "Tom\225\353 \268erm\225k", "Tong Sun"
    ,"Tony Lee", "Toomas Kivisild", "Tor D. Wager", "Torkel Klingberg", "Torsten Hoefler"
    ,"Torsten Persson", "Tourette Syndrome", "Tracy Packiam Alloway", "Travis Hirschi", "Trevor Darrell"
    ,"Trevor Hastie", "Trevor W. Robbins", "Trisha Suppes", "Tristan Thrush", "Truman Lee Kelley"
    ,"Trung Nguyen", "Trygve J. B. Hoff", "Tsukasa Yoshida", "Tuomas Sandholm", "Tyler Anbinder"
    ,"Tyler Cowen", "Tyler Jacks", "Tyler Murray", "Tyrone D. Cannon", "U. S. Department of Justice"
    ,"Ufuk Akcigit", "Ulric Neisser", "Ulrich Kutschera", "Ulrich M\252ller", "Ulrich Preuss"
    ,"Ulrich Schmidt", "Ulrich Trautwein", "Ulrike Schmidt", "Uma Ramakrishnan", "Umberto Eco"
    ,"United States Commission on Civil Rights", "United States District Court Southern District of New York", "United States District Court for the Eastern District of Pennsylvania", "Unity Biotechnology", "Urho Kujala"
    ,"Uri Alon", "Uri Gneezy", "Uri Simonsohn", "Uri Zwick", "Urie Bronfenbrenner"
    ,"Urs Egger", "Urs Fischbacher", "Ursula K. Le Guin", "Ursula M. Staudinger", "Ursula Rothlisberger"
    ,"Uwe Scholz", "Uwe Sunde", "V. S. Sundar", "Vadim Borisov", "Vadim N. Gladyshev"
    ,"Vaibhav Mathur", "Valerie M. Weaver", "Valery N. Soyfer", "Valsamma Eapen"
    ,"Valter D. Longo", "Vaneet Aggarwal", "Vaughan R. Pratt", "Veena Kumari", "Vegard Skirbekk"
    ,"Venkatesh Rao", "Vera Demberg", "Vera Gorbunova", "Vera John-Steiner", "Vernon L. Smith"
    ,"Vernon Williams", "Vernor Vinge", "Veronica Galvan", "Veselin Stoyanov", "Victor Chan"
    ,"Victor Goertzel", "Victor Haghani", "Victor Shapiro", "Victor W. Turner", "Victoria Harrison"
    ,"Victoria Leong", "Victoria Perez", "Victoria Stodden", "Vijay Pereira", "Vikash Kumar"
    ,"Vinay Prasad", "Vince Calhoun", "Vincent Egan", "Vincent P. Crawford", "Vincent Savolainen"
    ,"Vinod Khosla", "Vinod Kumar", "Vinod Vaikuntanathan", "Virginia Heffernan", "Virginia Kidd"
    ,"Virginia Postrel", "Virpi Lummaa", "Vitalik Buterin", "Vitaly Shmatikov", "Vivek Kumar"
    ,"Viviane Slon", "Vladimir Kim", "Vladimir Kramnik", "Vladimir Vapnik", "Vladlen Koltun"
    ,"W. Bentley MacLeod", "W. Bruce Croft", "W. Grant Dahlstrom", "W. Keith Campbell", "W. Patrick McCray"
    ,"W. R. Ashby", "W. Ross Ashby", "W. Tecumseh Fitch", "W. Timothy Garvey", "Wallace E. Oates"
    ,"Walter Bodmer", "Walter C. Willett", "Walter Guerra", "Walter Isaacson", "Walter J. Ong"
    ,"Walter Mischel", "Walter R. Dornberger", "Walter Scheidel", "Walter Sinnott-Armstrong", "Walter Y. Oi"
    ,"Warner R. Schilling", "Warren Weaver", "Wei Bi", "Wei Dai", "Wei Fang"
    ,"Wei Fu", "Wei Gan", "Wei Gao", "Wei Han", "Wei He"
    ,"Wei Huang", "Wei Ji Ma", "Wei Lu", "Wei Song", "Wei Wen"
    ,"Wei Xiong", "Wei Xu", "Wei Yu", "Wei Zheng", "Weihua Zhang"
    ,"Weinan E", "Wellcome Trust Case Control Consortium", "Wells Tower", "Wen Gao", "Wen Qin"
    ,"Wen Shen", "Wen Sun", "Wen Wang", "Wen Xie", "Wen Zhou"
    ,"Wenbo Wang", "Wendy B. Mendes", "Wendy Chung", "Wendy Johnson", "Wendy M. Williams"
    ,"Wendy Post", "Wendy S. Post", "Wendy Shang", "Wendy Slutske", "Wenjun Zeng"
    ,"Wenyuan Xu", "Wesley R. Elsberry", "Wiebke Arlt", "Wiebke Bleidorn", "Wilfred Reilly"
    ,"Wilfrid Hodges", "Wilhelm Ketteler", "Will Kay", "Will Knight", "Will Martin"
    ,"Will Storr", "Will Wilkinson", "Willard Van Orman Quine", "Willem H. Ouwehand", "Willem Hendrik Ouwehand"
    ,"Willem Kuyken", "Willem M. de Vos", "William A. Roberts", "William B. Lawson", "William B. Provine"
    ,"William Beauchamp", "William Beggs", "William Bennett Bean", "William Bracken", "William Bright"
    ,"William Bunney", "William C. Roberts", "William Chow", "William Cusick", "William D. Hill"
    ,"William Dickens", "William Durden", "William E. Bunney", "William Easterly", "William F. Lamb"
    ,"William Fithian", "William Flack", "William Foran", "William G. Durden", "William G. Hill"
    ,"William Gibson", "William H. McClain", "William H. McRaven", "William H. Press", "William H. Warren"
    ,"William Harvey", "William Hirst", "William Iacono", "William Ickes", "William Isaac"
    ,"William J. Brady", "William J. Broad", "William J. Calhoun", "William J. Dally", "William J. Dunn"
    ,"William J. Gibbons", "William J. McGuire", "William James", "William K. Scott", "William L. Holland"
    ,"William Langewiesche", "William Lowe Bryan", "William Maurice Ewing", "William Olson", "William O\8217Donohue"
    ,"William R. Johnson", "William Revelle", "William S. Bush", "William S. Jewell"
    ,"William Sealy Gosset", "William Shockley", "William T. Cefalu", "William T. Dickens", "William T. Freeman"
    ,"William Turton", "William W. Cohen", "William W. Seeley", "William Wang", "Wim E. Crusio"
    ,"Wim van den Brink", "Winfried Denk", "Winfried Rief", "Wing Suen", "Winston Wang"
    ,"Wis\322awa Szymborska", "Wojciech Rytter", "Wojciech Zaremba", "Wolf Reik", "Wolfgang B\246hme"
    ,"Wolfgang Hoenig", "Wolfgang Hoffmann", "Wolfgang Maier", "Wolfgang Pesendorfer", "Wolfgang Stroebe"
    ,"Wolfgang Viechtbauer", "Wolfram Burgard", "Woo Suk Hwang", "Woo-Suk Hwang", "Wulfram Gerstner"
    ,"Xavier Gabaix", "Xi Chen", "Xi Wang", "Xi Yin", "Xia Hu"
    ,"Xia Li", "Xian Yang", "Xiang Chen", "Xiang Cheng", "Xiang Zhang"
    ,"Xiao Feng", "Xiao Wang", "Xiao Xiao", "Xiao-Li Meng", "Xiaojie Wang"
    , "Xiaoling Zhang", "Xiaoming Liu", "Xiaonan Zhang", "Xiaoou Tang"
    ,"Xiaowei Li", "Xiaoxin Chen", "Xie Chen", "Xifeng Wu", "Xihong Lin"
    ,"Xin Di", "Xin Huang", "Xin Jiang", "Xin Meng", "Xin Tong"
    ,"Xin Yu", "Xin Zhang", "Xin Zhou", "Xing Sun", "Xinjun Zhang"
    ,"Xinwen Zhu", "Xinyan Zhang", "Xinyi Xu", "Xinyu Zhang", "Xu Chen"
    ,"Xu Han", "Xu Li", "Xu Shi", "Xu Sun", "Xuan Chen"
    ,"Xuan Zang", "Xuedong Huang", "Xun Wang", "Yaacov Trope", "Yan Ding"
    ,"Yan Li", "Yan Liang", "Yan Long", "Yan Lu", "Yang An"
    ,"Yang Fu", "Yang Gao", "Yang Hu", "Yang Shi", "Yang Song"
    ,"Yang Wu", "Yang You", "Yang Yue", "Yang Zhao", "Yang Zhou"
    ,"Yaniv Altshuler", "Yaniv Erlich", "Yann LeCun", "Yannic Kilcher", "Yasheng Huang"
    ,"Yasin Ozcan", "Yasuhiro Sato", "Yasuhiro Takeda", "Yasuo Kuniyoshi", "Ye Feng"
    ,"Ye Li", "Ye Xia", "Yee Whye Teh", "Yejin Choi", "Yi Ji"
    ,"Yi Jiang", "Yi Ma", "Yi Mao", "Yi Rao", "Yi Shang"
    ,"Yi Wang", "Yi Wei", "Yi Wen", "Yi Wu", "Yi Yu"
    ,"Yi Zuo", "Yifan Xu", "Yihan Wang", "Yili Wu", "Yilin Fan"
    ,"Yilin Yang", "Yiling Chen", "Yilu Wang", "Yiming Zhang", "Ying Gu"
    ,"Ying Guo", "Ying Lu", "Ying Shan", "Ying Wu", "Ying Xu"
    ,"Ying Yang", "Ying Zhu", "Ying-Hui Fu", "Yingying Chen", "Yinsheng Wang"
    ,"Yishan Wong", "Yiwei Zhang", "Yixuan Li", "Yoav Benjamini", "Yoav Shoham"
    ,"Yoh Iwasa", "Yoichi Takahashi", "Yoji Enokido", "Yolanda Gil", "Yolanda Moses"
    ,"Yong Liu", "Yong Rui", "Yong Tan", "Yong Zhao", "Yonggang Huang"
    ,"Yonhap News Agency", "Yoshihiro Kawaoka", "Yoshiki Kuroda", "Yoshio Miki", "Yoshiyuki Tomino"
    ,"Yoshua Bengio", "Yossi Matias", "Young-Tae Chang", "Yu (Jeffrey) Hu"
    ,"Yu Gu", "Yu Hu", "Yu Shi", "Yu Song"
    ,"Yu Tian", "Yu Xie", "Yu Yamamoto", "Yuan Cao", "Yuan Chen"
    ,"Yuan Hao", "Yuan He", "Yuan Jiang", "Yuan Liang", "Yuan Xie"
    ,"Yuan Yao", "Yuan Yin", "Yuan-Tsong Chen", "Yudhanjaya Wijeratne", "Yue Li"
    ,"Yue Shan", "Yue Wan", "Yue Yang", "Yufeng Zhang", "Yuhua Wang"
    ,"Yuichi Shoda", "Yuji Ijiri", "Yuji Kato", "Yukiyasu Kamitani", "Yulia Kovas"
    ,"Yulin Liu", "Yun Wang", "Yunfeng Liu", "Yuri Matiyasevich", "Yurii Nesterov"
    ,"Yury Volkov", "Yusuke Matsui", "Yusuke Takahashi", "Yutaka Suzuki", "Yutaro Sugimoto"
    ,"Yutian Chen", "Yuval Elovici", "Yuval Peres", "Yuwei Li", "Yuwen Zhang"
    ,"Yuxuan Zhang", "Yves Bertrand", "Yves Moreau", "Y\333ji Enokido", "Zachary Anderson"
    ,"Zachary Charles", "Zachary Fisher", "Zaheer Abbas", "Zaida Luthey-Schulten", "Zehao Sun"
    ,"Zellig S. Harris", "Zeng Tao", "Zenobia Jacobs", "Zenon Kulpa", "Zhao Chen"
    ,"Zhao Song", "Zhao Xue", "Zhao Zhong", "Zhen Fan", "Zhen Tan"
    ,"Zhendong Wang", "Zheng Cao", "Zheng Tian", "Zheng Yan", "Zheng Zhu"
    ,"Zhengyou Zhang", "Zhi Zheng", "Zhi-Li Zhang", "Zhichao Li", "Zhihong Chen"
    ,"Zhihui Wang", "Zhiping Weng", "Zhiwei Wang", "Zhiwu Lu", "Zhiyi Zhang"
    ,"Zhou Ren", "Zhou Yu", "Zi Wang", "Zicheng Liu", "Zoe Kourtzi"
    ,"Zoe R. Donaldson", "Zong Chen", "Zoubin Ghahramani", "Zuzana Pavelkov\225", "Zvi Galil"
    ,"Zvi Griliches", "Zvika Brakerski", "\193d\225m Mikl\243si", "OpenAI", "Vladimir Vapnik", "Alexey Chervonenkis"
    ,"Ronald Coase", "Michael C. Jensen", "William H. Meckling", "John Langdon Down", "Darold A. Treffert"
    , "Andy Hertzfeld", "Ryan North", "Robert Crumb", "Aline Kominsky-Crumb", "Ralph Bakshi"
    , "Marvin Minsky", "Helen Keller", "Bret Taylor", "Frederick Jelinek", "James P. Gordon", "Simon Rich"
    , "Alexander Grothendieck", "Francois Duc De La Rochefoucauld", "Oskar Pfungst", "Kary B. Mullis"
    , "Dana Gioia", "Patrik K. E. Magnusson", "This American Life", "Mervyn O’Gorman", "Matthew Meselson"
    , "Jeffrey Snover", "Bennett Foddy", "Geoffrey Brock", "Dennis Sciama", "Hank Greely"
    , "Claudia Langenberg", "Patricia Briggs", "Julia Galef", "Guy Wetmore Carryl", "Woody Allen"
    , "Central Committee of the Communist Party of China", "William Vickrey", "23andMe"
    , "Ted Chiang", "Bernard Greenberg", "Robert E. Peary", "Richard P. Gabriel", "Mary Lamb"
    , "Bulletin of the Atomic Scientists", "Taylor Swift", "Nick Land", "Garry Kasparov"
    , "Robert Penn Warren", "Jim Steinman", "Cristen Jennifer Willer", "Kenneth O. Stanley"
    , "Robert J. Cava", "Zoe Laughlin", "Viktor Pelevin", "John Ousterhout", "Noam Shazeer"
    , "Hans Uszkoreit", "Seneca", "Joe Biden", "Bill Watterson", "Leroy Lowe", "Fischer Black"
    , "Jason Shiga", "Philip Larkin", "Frederik Pohl", "Jayme Odgers", "Saul Bass", "Dominic Cummings"
    , "J. D. Trout", "Hideaki Anno", "Raymond Smullyan", "Clarence Zener", "Edward Luttwak"
    , "Anthony Powell", "Carlos Fuentes", "Kenneth Koch", "Takashi Murakami", "Fabrice Bellard"
    , "WikiWikiWeb", "Andreas Reif", "Elizabeth Bear", "Winston Churchill", "Nicholas Carlini"
    , "Bob Mercer", "Peter Fitzhugh Brown", "Kimberly Kagan", "Georges Rey", "L. Peter Deutsch"
    , "Philip Kapleau", "Gerard Nolst Trenité", "Jerry Fodor", "Kai Li", "Douglas McIlroy"
    , "Kennedy Space Center", "Bill Gosper", "William K. Clifford", "Scott Sumner", "Donald Hall"
    , "Corridor Digital", "Bruce Schneier", "Marjo-Riitta Järvelin", "Chris Argyris", "Andrew C. Yao"
    , "Greg Egan", "Paul D. MacLean", "John Archibald Wheeler", "Samuel T. Cohen", "Carter Scholz"
    , "Hiroshi Nagai", "David Corfield", "Stan Kelly-Bootle", "N. David Mermin", "Simon Tatham"
    , "Roel André Ophoff", "Paul M. B. Vitányi", "Oliver Herford", "Erik Satie", "E. T. Jaynes"
    , "Kenneth E. Boulding", "Ronan Farrow", "Karl T. Compton", "Björn Kurtén", "Robert R. McCrae"
    , "Milton Rokeach", "Robert Heinlein", "Lauren Faust", "Walter Savage Landor", "Robert Laurence Binyon"
    , "David A. Freedman", "Larry V. Hedges", "Hugh Christian Watkins", "Frank Herbert"
    , "Norman Spinrad", "Willis E. McNelly", "Stephen Cole Kleene", "Warren S. McCulloch"
    , "Walter Pitts", "Paul Werbos", "Ralph P. Boas Junior", "Albert W. Tucker", "Paul Debevec"
    , "Lyman Spitzer", "Ethan Zuckerman", "Scott Adams", "Justin Wolfers", "Richard Palais", "Ken White"
    , "Reginald Bretnor", "Poul Anderson", "Hal Clement", "Alan E. Nourse", "Theodore Sturgeon"
    , "Jerry Pournelle", "Katherine MacLean", "James E. Gunn", "Larry Niven", "Jack Williamson"
    , "John Brunner", "Harlan Ellison", "Frederik Pohl", "A. E. Stallings", "Richard Sharpe Shaver"
    , "Karl Sims", "C. S. Lewis", "Reed Albergotti", "Hyman G. Rickover", "Alain de Benoist"
    , "Ken Perlin", "Martin J. Lercher", "Richard Fateman", "World Bank Group", "Adrian Hon", "Aslak Tveito", "A. A. Markov"]
