{-# LANGUAGE OverloadedStrings #-}

module Config.MetadataAuthor where

-- import qualified Data.Text as T (Text)
import Text.Pandoc (Inline(Link, Span, Space, Str))

import qualified Data.Text as T

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

  -- test with link rewrites enabled:
  , ("a, b, c, d, e, f, George Washington",
     [Space,Span ("",["author","collapse"],[]) [Span ("",["abstract-collapse","cite-author-plural"],[("title","a, b, c, d, e, f, George Washington")]) [Str "a",Str ", ",Str "b",Str ", ",Str "c"],Span ("",[],[]) [Str ", ",Str "d",Str ", ",Str "e",Str ", ",Str "f",Str ", ",Link ("",[],[]) [Str "George Washington"] ("https://en.wikipedia.org/wiki/George_Washington","")]]])
    ]

-- list of rewrites for 'alternative name' → 'canonical name'
-- Config tests: unique values, no loops
canonicals :: [(String, String)]
canonicals = map (\(a,b) -> (b,a))
  [
    ("ESYudkowsky", "Eliezer Yudkowsky")
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
  , ("R.A. Fisher", "R. A. Fisher")
  , ("Sutskever", "Ilya Sutskever")
  , ("Engelbart", "Douglas Engelbart")
  , ("Anno", "Hideaki Anno")
  ]

-- Config tests: unique all, no loops
authorLinkDB :: [(T.Text, T.Text)]
authorLinkDB = [ ("George Washington", "https://en.wikipedia.org/wiki/George_Washington")
               , ("Eliezer Yudkowsky", "https://www.yudkowsky.net/")
    , ( "Jorge Luis Borges"
      , "https://en.wikipedia.org/wiki/Jorge_Luis_Borges"
      )
    , ( "TeX" , "https://en.wikipedia.org/wiki/TeX" )
    , ( "Sam Altman"
      , "https://en.wikipedia.org/wiki/Sam_Altman"
      )
    , ( "Paul Graham"
      , "https://en.wikipedia.org/wiki/Paul_Graham_(programmer)"
      )
    , ( "Robin Hanson"
      , "https://en.wikipedia.org/wiki/Robin_Hanson"
      )
    , ( "Andrew Gelman"
      , "https://en.wikipedia.org/wiki/Andrew_Gelman"
      )
    , ( "I. J. Good"
      , "https://en.wikipedia.org/wiki/I._J._Good"
      )
    , ( "Shawn Presser" , "https://twitter.com/theshawwn" )
    , ( "Eliezer Yudkowsky"
      , "https://en.wikipedia.org/wiki/Eliezer_Yudkowsky"
      )
    , ( "William Shockley"
      , "https://en.wikipedia.org/wiki/William_Shockley"
      )
    , ( "Said Achmiz" , "https://wiki.obormot.net/" )
    , ( "Reddit" , "https://en.wikipedia.org/wiki/Reddit" )
    , ( "Takashi Murakami"
      , "https://en.wikipedia.org/wiki/Takashi_Murakami"
      )
    , ( "Richard Feynman"
      , "https://en.wikipedia.org/wiki/Richard_Feynman"
      )
    , ( "Kevin Kelly"
      , "https://en.wikipedia.org/wiki/Kevin_Kelly_(editor)"
      )
    , ( "Jeff Dean" , "https://en.wikipedia.org/wiki/Jeff_Dean" )
    , ( "Hideaki Anno"
      , "https://en.wikipedia.org/wiki/Hideaki_Anno"
      )
    , ( "Greg Brockman"
      , "https://en.wikipedia.org/wiki/Greg_Brockman"
      )
    , ( "Elon Musk" , "https://en.wikipedia.org/wiki/Elon_Musk" )
    , ( "Edward Tufte"
      , "https://en.wikipedia.org/wiki/Edward_Tufte"
      )
    , ( "Douglas Hofstadter"
      , "https://en.wikipedia.org/wiki/Douglas_Hofstadter"
      )
    , ( "David Foster Wallace"
      , "https://en.wikipedia.org/wiki/David_Foster_Wallace"
      )
    , ( "Brad Leithauser"
      , "https://en.wikipedia.org/wiki/Brad_Leithauser"
      )
    , ( "Ilya Sutskever"
      , "https://en.wikipedia.org/wiki/Ilya_Sutskever"
      )
    , ( "William Sealy Gosset"
      , "https://en.wikipedia.org/wiki/William_Sealy_Gosset"
      )
    , ( "Tim Ferriss"
      , "https://en.wikipedia.org/wiki/Tim_Ferriss"
      )
    , ( "Ted Chiang"
      , "https://en.wikipedia.org/wiki/Ted_Chiang"
      )
    , ( "Stanisław Lem"
      , "https://en.wikipedia.org/wiki/Stanis%C5%82aw_Lem"
      )
    , ( "Robert Bringhurst"
      , "https://en.wikipedia.org/wiki/Robert_Bringhurst"
      )
    , ( "R. A. Fisher"
      , "https://en.wikipedia.org/wiki/R.A._Fisher"
      )
    , ( "Peter Watts"
      , "https://en.wikipedia.org/wiki/Peter_Watts_(author)"
      )
    , ( "Donald Keene"
      , "https://en.wikipedia.org/wiki/Donald_Keene"
      )
    , ( "New York Times"
      , "https://en.wikipedia.org/wiki/The_New_York_Times"
      )
    , ( "I. Richard Savage"
      , "https://projecteuclid.org/journals/statistical-science/volume-14/issue-1/A-conversation-with-I-Richard-Savage-with-the-assistance-of/10.1214/ss/1009211808.full"
      )
    , ( "Kicks Condor" , "https://www.kickscondor.com/" )
    , ( "J\252rgen Schmidhuber"
      , "https://en.wikipedia.org/wiki/J%C3%BCrgen_Schmidhuber"
      )
    , ( "Haskell" , "https://en.wikipedia.org/wiki/Haskell" )
    , ( "Derek Lowe"
      , "https://en.wikipedia.org/wiki/Derek_Lowe_(chemist)"
      )
    , ( "Demis Hassabis"
      , "https://en.wikipedia.org/wiki/Demis_Hassabis"
      )
    , ( "Charles Murray"
      ,  "https://en.wikipedia.org/wiki/Charles_Murray_(political_scientist)"
      )
    , ( "Alexey Guzey" , "https://guzey.com/" )
    , ( "Alan Kay" , "https://en.wikipedia.org/wiki/Alan_Kay" )
    , ( "Yasuhiro Takeda"
      , "https://en.wikipedia.org/wiki/Yasuhiro_Takeda"
      )
    , ( "William James"
      , "https://en.wikipedia.org/wiki/William_James"
      )
    , ( "William Gibson"
      , "https://en.wikipedia.org/wiki/William_Gibson"
      )
    , ( "Tyler Cowen"
      , "https://en.wikipedia.org/wiki/Tyler_Cowen"
      )
    , ( "Toshio Okada"
      , "https://en.wikipedia.org/wiki/Toshio_Okada"
      )
    , ( "Timothy C. May"
      , "https://en.wikipedia.org/wiki/Timothy_C._May"
      )
    , ( "ChatGPT" , "https://openai.com/blog/chatgpt/" )
    , ( "The New York Times"
      , "https://en.wikipedia.org/wiki/The_New_York_Times"
      )
    , ( "TRC" , "https://sites.research.google/trc/" )
    , ( "Steven Pinker"
      , "https://en.wikipedia.org/wiki/Steven_Pinker"
      )
    , ( "Sh\333tetsu"
      , "https://en.wikipedia.org/wiki/Sh%C5%8Dtetsu"
      )
    , ( "Satya Nadella"
      , "https://en.wikipedia.org/wiki/Satya_Nadella"
      )
    , ( "Y\333ji Enokido"
      , "https://en.wikipedia.org/wiki/Y%C5%8Dji_Enokido"
      )
    , ( "Rotten.com"
      , "https://en.wikipedia.org/wiki/Rotten.com"
      )
    , ( "Richard Hamming"
      , "https://en.wikipedia.org/wiki/Richard_Hamming"
      )
    , ( "R. A. Lafferty"
      , "https://en.wikipedia.org/wiki/R._A._Lafferty"
      )
    , ( "Philip Tetlock"
      , "https://en.wikipedia.org/wiki/Philip_E._Tetlock"
      )
    , ( "Charles Dickens"
      , "https://en.wikipedia.org/wiki/Charles_Dickens"
      )
    , ( "Oliver Heaviside"
      , "https://en.wikipedia.org/wiki/Oliver_Heaviside"
      )
    , ( "Norbert Wiener"
      , "https://en.wikipedia.org/wiki/Norbert_Wiener"
      )
    , ( "Nick Bostrom"
      , "https://nickbostrom.com/"
      )
    , ( "Neal Stephenson"
      , "https://en.wikipedia.org/wiki/Neal_Stephenson"
      )
    , ( "Mike Darwin"
      , "https://en.wikipedia.org/wiki/Mike_Darwin"
      )
    , ( "Michael Nielsen" , "https://michaelnielsen.org/" )
    , ( "Matthew Butterick"
      , "https://en.wikipedia.org/wiki/Matthew_Butterick"
      )
    , ( "Mamoru Oshii"
      , "https://en.wikipedia.org/wiki/Mamoru_Oshii"
      )
    , ( "Lord Bowden" , "https://en.wikipedia.org/wiki/B._V._Bowden,_Baron_Bowden" )
    , ( "Yoji Enokido"
      , "https://en.wikipedia.org/wiki/Y%C5%8Dji_Enokido"
      )
    , ( "Julian Assange"
      , "https://en.wikipedia.org/wiki/Julian_Assange"
      )
    , ( "John von Neumann"
      , "https://en.wikipedia.org/wiki/John_von_Neumann"
      )
    , ( "John Carmack"
      , "https://en.wikipedia.org/wiki/John_Carmack"
      )
    , ( "Robyn Dawes"
      , "https://en.wikipedia.org/wiki/Robyn_Dawes"
      )
    , ( "James Yu" , "https://jamesyu.org/" )
    , ( "Italo Calvino"
      , "https://en.wikipedia.org/wiki/Italo_Calvino"
      )
    , ( "Isaac Asimov"
      , "https://en.wikipedia.org/wiki/Isaac_Asimov"
      )
    , ( "Yoshiyuki Tomino"
      , "https://en.wikipedia.org/wiki/Yoshiyuki_Tomino"
      )
    , ( "Hamming"
      , "https://en.wikipedia.org/wiki/Richard_Hamming"
      )
    , ( "Gian-Carlo Rota"
      , "https://en.wikipedia.org/wiki/Gian-Carlo_Rota"
      )
    , ( "Gerard Manley Hopkins"
      , "https://en.wikipedia.org/wiki/Gerard_Manley_Hopkins"
      )
    , ( "Gainax" , "https://en.wikipedia.org/wiki/Gainax" )
    , ( "Fujiwara no Teika"
      , "https://en.wikipedia.org/wiki/Fujiwara_no_Teika"
      )
    , ( "Freeman Dyson"
      , "https://en.wikipedia.org/wiki/Freeman_Dyson"
      )
    , ( "English Wikipedia"
      , "https://en.wikipedia.org/wiki/English_Wikipedia"
      )
    , ( "Engelbart"
      , "https://en.wikipedia.org/wiki/Douglas_Engelbart"
      )
    , ( "Edward Teller"
      , "https://en.wikipedia.org/wiki/Edward_Teller"
      )
    , ( "Donald Knuth"
      , "https://en.wikipedia.org/wiki/Donald_Knuth"
      )
    , ( "Czes\322aw Mi\322osz"
      , "https://en.wikipedia.org/wiki/Czes%C5%82aw_Mi%C5%82osz"
      )
    , ( "Claude Shannon"
      , "https://en.wikipedia.org/wiki/Claude_Shannon"
      )
    , ( "Anthropic" , "https://en.wikipedia.org/wiki/Anthropic" )
    , ( "Albert Einstein"
      , "https://en.wikipedia.org/wiki/Albert_Einstein")
    ]
