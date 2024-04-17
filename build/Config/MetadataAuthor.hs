{-# LANGUAGE OverloadedStrings #-}

module Config.MetadataAuthor where

import qualified Data.Map.Strict as M (fromList, Map)
import Text.Pandoc (Inline(Link, Span, Space, Str))
import qualified Data.Text as T (Text)

import Interwiki (toWikipediaEnURL)

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
  , ("gwern", "Gwern")
  , ("gwernbranwen", "Gwern")
  , ("gwern.branwen", "Gwern")
  , ("Gwern Branwen", "Gwern")
  , ("Donald E. Knuth", "Donald Knuth")
  , ("Knuth", "Donald Knuth")
  , ("Don Knuth", "Donald Knuth")
  , ("Hans J. Eysenck", "Hans Eysenck")
  , ("Hans Jürgen Eysenck", "Hans Eysenck")
  , ("Eysenck", "Hans Eysenck")
  , ("Frank P. Ramsey", "Frank Ramsey")
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
  , ("Noah A. Smith", "Noah Smith")
  , ("sarah_cone", "Sarah Cone")
  , ("R. Impagliazzo", "Russell Impagliazzo")
  , ("Gary L. Drescher", "Gary Drescher")
  , ("Doug Hofstadter", "Douglas Hofstadter")
  , ("Douglas R. Hofstadter", "Douglas Hofstadter")
  ]

-- Config tests: unique all, no loops, all values are URLs
authorLinkDB :: M.Map T.Text T.Text
authorLinkDB = M.fromList $
   [ ("Alexey Guzey","https://guzey.com/")
    , ("Carl Shulman","https://timelines.issarice.com/wiki/Timeline_of_Carl_Shulman_publications#Full_timeline")
    , ("I. Richard Savage","https://projecteuclid.org/journals/statistical-science/volume-14/issue-1/A-conversation-with-I-Richard-Savage-with-the-assistance-of/10.1214/ss/1009211808.full")
    , ("James Yu","https://jamesyu.org/")
    , ("Kicks Condor","https://www.kickscondor.com/")
    , ("Michael Nielsen","https://michaelnielsen.org/")
    , ("Nick Bostrom","https://nickbostrom.com/")
    , ("Said Achmiz","https://wiki.obormot.net/")
    , ("Shawn Presser","https://twitter.com/theshawwn")
    , ("Gwern", "https://gwern.net/index#abstract")
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
    ] ++
    zip authorWpLinkDB (map toWikipediaEnURL authorWpLinkDB)

-- Config tests: unique list
authorLinkBlacklist :: [T.Text]
authorLinkBlacklist = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"] ++
                    ["1890\8211\&1974", "1902","1906","1916","1922","1928",":", "English Wikipedia", "Wiel", "Word", "Rau", "Qi",
                    "Pontifex", "Postma", "Poinar", "Pier", "Pika", "van Buuren","van Os","van den Hurk", "van der Ploeg", "Anonymous"]

-- config tests: none, tested via `authorLinkDB` as a whole
authorWpLinkDB :: [T.Text]
authorWpLinkDB =
 ["A. A. Brill","A. Ali","A. Bradford Hill","A. C. Littleton","A. G. W. Cameron","A. J. Holmes","A. Jameson",
  "Russell Impagliazzo",
  "A. K. Bera","A. L. Barker","A. L. Sadler","A. Murat Eren","A. Narayanan","A. Tversky","Aanund Hylland",
  "Aapo Kyrola","Aaron Clauset","Aaron Cochrane","Aaron D. Ames","Aaron E. Carroll","Aaron Edwards","Aaron Gordon",
  "Aaron Isaacs","Aaron Klein","Aaron Naparstek","Aaron Reeves","Aaron Roth","Aaron Streets","Abdoulaye Diabate",
  "Abdul Basit","Abdul Waheed","Abeba Birhane","Abhijit Banerjee","Abhishek Kumar","Abigail Fisher","Abigail Powers",
  "Abraham A. Palmer","Abraham Flexner","Abraham Loeb","Abraham Palmer","Ada Palmer","Adam B. Jaffe","Adam Cifu",
  "Adam D\8217Angelo","Adam Frank","Adam Gazzaley","Adam Lopez","Adam M. Phillippy","Adam Pearce","Adam Platt",
  "Adam Siegel","Adam Siepel","Adam Stein","Adam Tauman Kalai","Adam Thomas","Adam Twardoch","Adam Zeman",
  "Adebowale A. Adeyemo","Adebowale Adeyemo","Adeel Malik","Adele Eskeles Gottfried","Adi Shamir","Aditi Rao","Aditya Rawal",
  "Aditya Sinha","Adnan Ahmed","Adnan Custovic","Adolf Loos","Adolfo Garc\237a","Adrian Bejan","Adrian Campos",
  "Adrian Cortes","Adrian E. Raftery","Adrian Furnham","Adrian G. Barnett","Adrian Liu","Adrian Raine","Adrian V. S. Hill",
  "Adrian Zenz","Adriana Galv\225n","Adriana Munoz","Adriana Romero","Aggelos Kiayias","Agnar Helgason","Agus Salim",
  "Ahmed Arif","Ahmed Elnaggar","Ahmed Khalifa","Ahmed Mustafa","Ahmed Radwan","Airlines for America","Aja Huang",
  "Ajay Gupta","Ajay Patel","Ajay Sharma","Ajit Varki","Ajoy Sarkar","Akhilesh Jaiswal","Akira Fujita",
  "Akiyoshi Kitaoka","Akiyuki Nosaka","Alain Pellet","Alan Ashworth","Alan B. Krueger","Alan Bellows","Alan Borning",
  "Alan Bovik","Alan C. Evans","Alan Cooper","Alan Donaldson","Alan Gow","Alan J. Auerbach","Alan J. Gow",
  "Alan J. Perlis","Alan James","Alan Kay","Alan Lerner","Alan Moore","Alan Price","Alan S. Kaufman",
  "Alan Schatzberg","Alan Sullivan","Alan Turing","Alan W. Black","Alan Yuille","Albert A. Bartlett","Albert Einstein",
  "Albert J. Stunkard","Albert Marcet","Albert O. Hirschman","Albert Walton","Albert Wohlstetter","Albert Wong","Albert Ziegler",
  "Albert-L\225szl\243 Barab\225si","Alberto F. Alesina","Alcino J. Silva","Aldert Vrij","Aldo Rustichini","Alec Smith","Alejandro Acosta",
  "Alejandro Pardo","Alek Sigley","Aleksandr Khokhlov","Aleksei Petrenko","Aleksei Timofeev","Alessandra Voena","Alessandro Liberati",
  "Alessandro Vespignani","Alessio Palumbo","Alethea Charlton","Alex Calderwood","Alex Castro","Alex Chen","Alex Goldin",
  "Alex Graves","Alex Heath","Alex Hirsch","Alex Honnold","Alex Kacelnik","Alex Krizhevsky","Alex Magee",
  "Alex Miller","Alex Pentland","Alex R. Piquero","Alex Ramirez","Alex Ross","Alex Tabarrok","Alex de Voogt",
  "Alexa Beiser","Alexander Binder","Alexander C. Smith","Alexander Grishin","Alexander Hart","Alexander Ljungqvist","Alexander Long",
  "Alexander Lopez","Alexander Mann","Alexander Novikov","Alexander Ogle","Alexander Ploner","Alexander Rich","Alexander Richards",
  "Alexander Rives","Alexander Roberts","Alexander Rosenberg","Alexander Rutherford","Alexander Spiridonov","Alexander Weiss","Alexandra Barratt",
  "Alexandra Kelly","Alexandra Mendes","Alexandre Borges","Alexandre Pouget","Alexei A. Efros","Alexey Kurakin","Alexis C. Madrigal",
  "Alexis Ohanian","Alfonso Valencia","Alfred J. Lewy","Alfred Moore","Alfredo Ramirez","Ali Farhadi","Ali Ghodsi",
  "Ali Hassani","Ali Jadbabaie","Ali Madani","Ali R. Rezai","Alice Chang","Alice H. Eagly","Alice Roberts",
  "Alice Spooner","Alice Stanton","Alicia Martin","Alina Stoica","Alison Fraser","Alison Goate","Alison Gopnik",
  "Alison Kraus","Alison M. Goate","Alkes L. Price","Alkes Price","All Things Considered","Allan Rechtschaffen","Allen Brown",
  "Allen Buchanan","Allen Downey","Allen Ginsberg","Allen Neuringer","Allen Roush","Allen Wang","Allyn A. Young",
  "Alon Cohen","Alon Halevy","Alon Orlitsky","Alun Evans","Alun Thomas","Alvaro Pascual-Leone","Alvaro Sanchez",
  "Alvin E. Roth","Alvin Toffler","Alvin W. Gouldner","Alyssa Panitch","Alzheimer\8217s Disease Neuroimaging Initiative","Al\225n Aspuru-Guzik","Amanda Collins",
  "Amanda Feilding","Amanda Gefter","Amanpreet Singh","Ambrogio Fasoli","American Medical Association","American Physiological Society","American Psychological Association",
  "Amit Agarwal","Amit Kumar","Amit Lal","Amit Sheth","Ammar Al-Chalabi","Amnon Rapoport","Amnon Shashua",
  "Amos H. Hawley","Amos Storkey","Amos Tversky","Amy Barrett","Amy Chow","Amy Dawes","Amy Ferguson",
  "Amy Hauck Newman","Amy J. Wagers","Amy Mitchell","Amy Moore","Amy Orben","Amy Peters","Amy Price",
  "Amy Yang","An Yang","Ana Maria Cuervo","Ana Miranda","Ana Romero","Anat Brunstein Klomek","Anders D. B&oslash;rglum",
  "Anders Dale","Anders Fjell","Anders Jonsson","Anders Krogh","Anders Lundmark","Anders Sandberg","Andre Barreto",
  "Andre Franke","Andrea B. Troxel","Andrea Basso","Andrea Burns","Andrea Crisanti","Andrea Huber","Andrea J. Liu",
  "Andrea L. Thomaz","Andrea Levy","Andrea Montanari","Andrea Natale","Andrea Repetto","Andrea Rusnock","Andrea Sanchez",
  "Andrea Urrutia","Andreas Arnold","Andreas Blum","Andreas Brandst\228tter","Andreas Buja","Andreas Demetriou","Andreas Kappes",
  "Andreas Meyer-Lindenberg","Andreas Steiner","Andreas Wagner","Andreas Winter","Andrej Karpathy","Andres Cardenas","Andres Metspalu",
  "Andrew A. Brown","Andrew Abbott","Andrew Adamatzky","Andrew B. Whinston","Andrew Baker","Andrew Beer","Andrew Berg",
  "Andrew Bolt","Andrew Brock","Andrew C. Heath","Andrew Callahan","Andrew Carroll","Andrew Childs","Andrew Cockburn",
  "Andrew Critch","Andrew Crompton","Andrew D. Gordon","Andrew D. Huberman","Andrew D. White","Andrew Dahl","Andrew Dalby",
  "Andrew Davison","Andrew Dillin","Andrew E. Budson","Andrew Farrell","Andrew Ferguson","Andrew Frew","Andrew G. Barto",
  "Andrew G. Clark","Andrew G. Walder","Andrew G. White","Andrew Gelman","Andrew Goff","Andrew Gregory","Andrew Howard",
  "Andrew Iwaniuk","Andrew J. Scott","Andrew J. Watson","Andrew Jackson","Andrew Keller","Andrew Krystal","Andrew M. Colman",
  "Andrew M. Davis","Andrew M. McIntosh","Andrew Marantz","Andrew Mayne","Andrew McCallum","Andrew McNamara","Andrew Moravcsik",
  "Andrew Murphy","Andrew N. Iwaniuk","Andrew Ng","Andrew Norman","Andrew Odlyzko","Andrew Owens","Andrew O\8217Hagan",
  "Andrew Prout","Andrew Rice","Andrew Ross Sorkin","Andrew Scull","Andrew Simmons","Andrew Singleton","Andrew Sparkes",
  "Andrew Steptoe","Andrew Tang","Andrew Tulloch","Andrew Vaughn","Andrew Vickers","Andrew W. Lo","Andrew Whiten",
  "Andrew Z. Fire","Andrew Zisserman","Andrey Anokhin","Andrey Gromov","Andrey Korotayev","Andrey Simonov","Andries van Dam",
  "Andrzej Pajak","Andr\225s Hajnal","Andr\233 Barreto","Andy Clark","Andy Cox","Andy Simmons","Andy Williams",
  "Angela Brady","Angela Douglas","Angela Duckworth","Angela Martini","Angela N. Brooks","Angela Page","Angela Park",
  "Angela Rose","Angelica Ronald","Angelika Steger","Angelo Rizzo","Angie Chen","Angus Fletcher","Angus Trumble",
  "Anil Madhavapeddy","Anil Seth","Anima Anandkumar","Anita Thapar","Anjan Chatterjee","Ankit Gupta","Ann Bartuska",
  "Ann Finkbeiner","Ann L. Brown","Ann Lee","Ann Now\233","Anna Becker","Anna Belfer-Cohen","Anna Cheung",
  "Anna Docherty","Anna Dominiczak","Anna Frebel","Anna Grzymala-Busse","Anna J\228rvinen","Anna Lebedeva","Anna Middleton",
  "Anna Murray","Anna Palmer","Anna Rogers","Anna Salamon","Anna Schmidt","Anna Sun","Anna Sundstr\246m",
  "Anna Van Meter","Annamaria Lusardi","Anne Anastasi","Anne B. Newman","Anne Barton","Anne Broadbent","Anne Brunet",
  "Anne C. Stone","Anne Carson","Anne Case","Anne Chao","Anne Cutler","Anne E. Carpenter","Anne E. Pusey",
  "Anne Farmer","Anne Harrington","Anne K. Churchland","Anne McLaren","Anne Roe","Anne Thomas","Annette Lee",
  "Annette Peters","Annie Chen","Antal van den Bosch","Anthony C. Davison","Anthony Chen","Anthony Downs","Anthony Francis",
  "Anthony G. Greenwald","Anthony J. Bailey","Anthony Jorm","Anthony P. Monaco","Anthony Tommasini","Anthony Trewavas","Anthony Yeung",
  "Antoine Merle","Antoine Richard","Antoine Roux","Antoinette Schoar","Anton Spiridonov","Antoni Ribas","Antonio Cano",
  "Antonio Damasio","Antonio Ferretti","Antonio Paoli","Anu Realo","Anubha Mahajan","Anupam Joshi","Anya Kamenetz",
  "Anya Samek","Apollo Robbins","Apoorva Khare","Ara Darzi","Arang Rhie","Aravinda Chakravarti","Architectural Digest",
  "Are Holen","Ari Holtzman","Arie W. Kruglanski","Ariel Darvasi","Ariel Knafo-Noam","Ariel Shamir","Arindrajit Dube",
  "Arlindo Oliveira","Armand M. Leroi","Armen A. Alchian","Armin Falk","Armin Ronacher","Armin Wei\223","Arnab Rai Choudhuri",
  "Arne Mastekaasa","Arne Skaug","Arno Villringer","Arnold C. Cooper","Arnold Thackray","Arnout van de Rijt","Arpana Agrawal",
  "Arren Bar-Even","Art Pope","Arthur A. Lumsdaine","Arthur Beaudet","Arthur F. Kramer","Arthur L. Norberg","Arthur L. Samuel",
  "Arthur R. Jensen","Arthur W. Toga","Arthur Woodruff","Arun Kumar Gupta","Arvind Krishnamurthy","Arvind Mahankali","Arvind Narayanan",
  "Asa Fitch","Asbj\248rn Hr\243bjartsson","Ashish Arora","Ashish Khetan","Ashish Vaswani","Ashish Yadav","Ashlee Vance",
  "Ashley Byrne","Ashley Freeman","Ashley Stewart","Ashley Whillans","Ashok Goel","Associated Press","Asya Rolls",
  "Athula Sumathipala","Atsushi Takahashi","Atul Gawande","Aubrey Sheiham","Aude Oliva","Audrey Smith","Augustine Kong",
  "Auke Tellegen","Aurelio Jos\233 Figueredo","Austan Goolsbee","Austin Bradford Hill","Austin Burke","Austin Myers","Austin Nichols",
  "Austin Reiter","Austin Stone","Aviv Regev","Avshalom Caspi","Axel D. Becke","Axel Schmidt","Ayelet Fishbach",
  "Azhar Sultan","B. Douglas Bernheim","B. F. Skinner","B. M. Trost","B. Timothy Walsh","Babak Hassibi","Babak Hodjat",
  "Bahad\305r Demir","Bal\225zs Kov\225cs","Barbara Davis","Barbara Grosz","Barbara J. Sahakian","Barbara Maughan","Barbara Mellers",
  "Barbara S. Burks","Barbara Schneider","Barbara Stoddard Burks","Barbara Sullivan","Barbara Wold","Barry Cunliffe","Barry Horowitz",
  "Baruch Fischhoff","Basem Al-Shayeb","Basil Hetzel","Beat Keller","Beat Meier","Beate Albrecht","Beatrice H. Hahn",
  "Beatrice de Gelder","Beatriz Luna","Bei Shi","Belle Chen","Ben Goldacre","Ben Goodrich","Ben Higgins",
  "Ben Horowitz","Ben Hutchinson","Ben Laurie","Ben Poole","Ben Rattray","Ben Shneiderman","Ben Silverman",
  "Benedict Jones","Benedict Smith","Benedicto Crespo-Facorro","Beng Chin Ooi","Bengt Holmstrom","Bengt Holmstr\246m","Benjamin A. Garcia",
  "Benjamin A. Olken","Benjamin B","Benjamin B. Lahey","Benjamin Black","Benjamin Breen","Benjamin Djulbegovic","Benjamin F. Jones",
  "Benjamin Goldstein","Benjamin Graham","Benjamin Hayden","Benjamin Jensen","Benjamin Mako Hill","Benjamin McMahon","Benjamin Neale",
  "Benjamin S. Bloom","Benjamin Tan","Benjamin Tang","Benjamin Williams","Benjamin van Niekerk","Bent Flyvbjerg","Bent Petersen",
  "Bernard Tan","Bernard Zinman","Bernd Kraemer","Bernd Kr\228mer","Bernd Weber","Bernd W\252rsig","Bernhard Ludvik",
  "Bernhard Nebel","Bernhard Sch\246lkopf","Bernie Devlin","Bernt Schiele","Bert H\246lldobler","Bertram Gilfoyle","Bertrand Meyer",
  "Bessel van der Kolk","Beth Martin","Beth Shapiro","Beth Stevens","Beverly Rodriguez","Bhramar Mukherjee","Bhuvana Ramabhadran",
  "Bian Li","Bilal Ashraf","Bilge Ebiri","Bill Casselman","Bill Dally","Bill Deakin","Bill Gates",
  "Bill Hicks","Bill Peck","Bin Jiang","Bin Liu","Bin Xu","Bin Yu","Bing Han",
  "Bing Zhang","Blair Braverman","Blaise Aguera y Arcas","Blaise Ag\252era y Arcas","Blake Camp","Blake Richards","Blake Ross",
  "Bluma Zeigarnik","Bo Dai","Bo Gao","Bo Li","Bo Peng","Bo Yang","Bo Zhou",
  "Boaz Barak","Boaz Keysar","Bob Sturm","Boban Petrovi\263","Boehringer Ingelheim","Bogus\322aw Paw\322owski","Bokyung Son",
  "Bonnie Berger","Boris Katz","Botond Roska","Bowen Zhang","Brad Leithauser","Bradley Efron","Bradley J. Nelson",
  "Bradley T. Hyman","Brandon Carter","Brandon Hill","Brendan Burchell","Brendan I. Koerner","Brendan Nyhan","Brendan O\8217Donoghue",
  "Brent Graham","Brent W. Roberts","Bret S. Weinstein","Bret Taylor","Bret Victor","Brian A. Jacob","Brian Bailey",
  "Brian Bell","Brian Cantwell Smith","Brian Charlesworth","Brian Curtis","Brian D\8217Onofrio","Brian Ferguson","Brian Flanagan",
  "Brian Fuller","Brian G. Wowk","Brian Hare","Brian Hicks","Brian Israel","Brian K. Kennedy","Brian Krebs",
  "Brian L. Schmidt","Brian L. Strom","Brian Levine","Brian M. D\8217Onofrio","Brian Moriarty","Brian Nosek","Brian Pittman",
  "Brian Tomlinson","Brian Uzzi","Brian Vaughan","Brian Wansink","Brian Williams","Brian Wong","Brian Wowk",
  "British Medical Journal","Brittany Haas","Broadridge Financial Solutions Inc","Bruce Berger","Bruce Bode","Bruce Bueno de Mesquita","Bruce C. Gibb",
  "Bruce Castle","Bruce Charles Heezen","Bruce Collie","Bruce E. Wampold","Bruce F. Pennington","Bruce G. Charlton","Bruce J. Ellis",
  "Bruce L. McNaughton","Bruce M. Cohen","Bruce Pennington","Bruce R. Rosen","Bruce S. Weir","Bruce Sacerdote","Bruce Tognazzini",
  "Bruce Western","Bruno Mota","Bruno Pereira","Bruno S. Frey","Bruno Studer","Bruno de Finetti","Bryan Brown",
  "Bryan Gibson","Bryan J. Traynor","Bryan Kolb","Bryan L. Roth","Bryan McCann","Bryony James","Burkhard Bilger",
  "Burkhard Rost","Butler Lampson","Butler W. Lampson","Byeong Chun Lee","Byung Chul Kim","B\233la Bollob\225s","C. A. B. Smith",
  "C. A. R. Hoare","C. Anandharamakrishnan","C. Cooper","C. D. Darlington","C. H. Turner","C. H. Waddington","C. Radhakrishna Rao",
  "C. Robert Cloninger","C. S. Franklin","C. Scott Baker","C. Smith","C. Sue Carter","C. W. J. Granger","C. Wolf",
  "C. Wyatt Shields IV","Caitlin Flanagan","Caleb Robinson","Calvin Lo","Cameron Anderson","Cameron Jones","Cameron Miller",
  "Camilla Benbow","Camilla P. Benbow","Camilla Persson Benbow","Camilla Stoltenberg","Camille Landais","Can Xu","Candice Odgers",
  "Carel Le Roux","Carel P. van Schaik","Carl A. Anderson","Carl Baker","Carl Benedikt Frey","Carl Feynman","Carl I. Hovland",
  "Carl Lieberman","Carl Sagan","Carl Schreck","Carl Shapiro","Carl T. Bergstrom","Carl Zimmer","Carla Gallo",
  "Carles Lalueza-Fox","Carlos Cruchaga","Carlos D. Bustamante","Carlos M. Duarte","Carlos Ribeiro","Carlos Riquelme","Carmel Moore",
  "Carol Brayne","Carol Chen","Carol Mills","Carol S. Dweck","Carole Lieberman","Carolina Lopez","Caroline Ellis",
  "Caroline Fox","Caroline Fraser","Caroline Hill","Caroline Watt","Carrick Flynn","Carroll L. Wainwright","Carsen Stringer",
  "Carsten Pedersen","Carter Scholz","Casey B. Mulligan","Casey Dunn","Cass R. Sunstein","Caterina Barbieri","Catharine R. Gale",
  "Catherine Dulac","Catherine Jami","Catherine Lord","Catherine Marshall","Catherine McBride","Catherine Plaisant","Catherine Potenski",
  "Cathleen Schine","Catholijn Jonker","Cathryn Lewis","Cathryn M. Lewis","Cathy J. Price","Cathy King","Cathy Spatz Widom",
  "Cathy Williams","Cathy Wu","Cecil R. Reynolds","Cecilia D\8217Anastasio","Cecilia Lindgren","Cecilia Magnusson","Cees Dekker",
  "Celeste Kidd","Celeste Lyn Paul","Celia Greenwood","Cell Press","Celso Arango","Chaim Goodman-Strauss","Chandler Burr",
  "Chang Jiang","Chang Xu","Chantal Radimilahy","Chao Agnes Hsiung","Chao Chen","Chao Dong","Chao Min",
  "Chao Xu","Charles A. Czeisler","Charles A. Taylor","Charles B. Crawford","Charles B. Nemeroff","Charles Beattie","Charles Benson",
  "Charles Curtis","Charles Dickens","Charles Duhigg","Charles E. Leiserson","Charles E. Osgood","Charles F. Hockett","Charles F. Reynolds III",
  "Charles Fuchs","Charles G. Gross","Charles George Herbermann","Charles Geschke","Charles Goodhart","Charles H. Haskins","Charles H. Martin",
  "Charles Hulme","Charles I. Jones","Charles Isbell","Charles J. Lumsden","Charles Murray","Charles N. Rotimi","Charles Naylor",
  "Charles Nicholas","Charles Ofria","Charles P. Davis","Charles Paul Conn","Charles Piller","Charles Poole","Charles Rotimi",
  "Charles Spearman","Charles Spence","Charles W. Rees","Charles ffrench-Constant","Charlie Griffin","Charlie Manson","Charlie Snell",
  "Charlotta Pisinger","Charlotte Banks","Charlotte Blease","Charlotte Buhler","Charlotte Drake","Charlotte Harrison","Chelsea Finn",
  "Chen Hu","Chen Zhang","Chen Zhao","Chen Zhu","Cheng Chen","Cheng Li","Cheng Xu",
  "Chengxiang Zhai","Cheryl Dissanayake","Chi Chung Lam","Chi-Hung Hsu","Chia-Che Chang","Chiara Sabatti","Chicago Urban League",
  "Chiho Saito","Ching Fang","Chip Heath","Chirag Jain","Chirag Patel","Chris Apps","Chris Cheng",
  "Chris Donahue","Chris Eliasmith","Chris Freeland","Chris Hawkey","Chris Horton","Chris Lu","Chris Newell",
  "Chris Offutt","Chris P. Ponting","Chris Petersen","Chris Power","Chris Short","Chris Simon","Chris Street",
  "Chris Turney","Chris Wallace","Chris Walshaw","Chris Wilkins","Christian Becker","Christian Bendixen","Christian Cachin",
  "Christian Catalini","Christian Fischer","Christian Fong","Christian Genest","Christian Hammer","Christian Roselius","Christian Rudder",
  "Christian Schneider","Christian Seel","Christian Sonne","Christian T. Wentz","Christian Voigt","Christian Walder","Christian Wirth",
  "Christina Chambers","Christina Chang","Christina Kim","Christina Paxson","Christina Schmidt","Christine E. Seidman","Christine F. Baes",
  "Christine King","Christine Payne","Christine Van Broeckhoven","Christof Koch","Christoph Adami","Christoph Hoerl","Christoph Meinel",
  "Christoph Preuss","Christophe Dessimoz","Christopher A. Walsh","Christopher Alexander","Christopher B. Field","Christopher Boehm","Christopher Boone",
  "Christopher Chabris","Christopher Chen","Christopher Clark","Christopher Copeland","Christopher Cullen","Christopher D. Gardner","Christopher D. Manning",
  "Christopher F. Chabris","Christopher Gardner","Christopher Gillberg","Christopher Grimm","Christopher Honey","Christopher I. Amos","Christopher J. Ferguson",
  "Christopher Koch","Christopher M. Andrew","Christopher Malloy","Christopher Nicholas","Christopher Nowinski","Christopher Olah","Christopher O\8217Neill",
  "Christopher Patrick","Christopher Pittenger","Christopher R. Brand","Christopher Re","Christopher Roth","Christopher R\233","Christopher Small",
  "Christopher Talbot","Christopher Weber","Christos Davatzikos","Christos H. Papadimitriou","Christos Kozyrakis","Christos Pantelis","Christos Papadimitriou",
  "Christos S. Mantzoros","Christy L. Haynes","Chu Chen","Chuan He","Chun Ye","Chun Yuan","Chunyu Wang",
  "Churchill Eisenhart","Cindy Ramirez","Cisca Wijmenga","Claire Allen","Claire Haworth","Claire Massey","Clancy Blair",
  "Clara Sousa-Silva","Claude Bouchard","Claude Gaillard","Claude Marcus","Claude Roux","Claude Shannon","Claudi L. H. Bockting",
  "Claudia Clopath","Claudio Stampi","Claudius Gros","Claus Lamm","Clay Shirky","Clemens Meyer","Cliff Arnall",
  "Cliff Stoll","Clifford Geertz","Clinical Psychological Science","Cliodhna O\8217Connor","Clive Ballard","Clive D. L. Wynne","Clive Holmes",
  "Clive Wilkins","Clyde A. Hutchison III","Coalition for Evidence-Based Policy","Cody Wild","Cole Hawkins","Colin Allen","Colin Berry",
  "Colin Burke","Colin Camerer","Colin Cherry","Colin Drummond","Colin F. Camerer","Colin Farrell","Colin Flaherty",
  "Colin G. DeYoung","Colin Hodgkinson","Colin Humphreys","Colin Lankshear","Colin Mallows","Colin Martindale","Colin Masters",
  "Colin Mathers","Colin Palmer","Colin Skinner","Colleen Lawless","Collin Burns","Collin Y. Ewald","Colm McDonald",
  "Companies House","Con Stough","Cong Han","Cong Zhou","Connie Wang","Constantine G. Lyketsos","Corby K. Martin [for the CALER I. E. Study Group]",
  "Cordelia Schmid","Corey Lynch","Corey Powell","Corinna Cortes","Cormac \211 Gr\225da","Cornelia M. Van Duijn","Cornelia van Duijn",
  "Cosma Rohilla Shalizi","Courtney Williams","Craig Ferguson","Craig Haney","Craig Morgan","Craig Partridge","Craig Raine",
  "Craig Ross","Craig S. Kaplan","Craig S. Smith","Cristen Willer","Cristian Canton Ferrer","Cristobal Morales","Cristopher Moore",
  "Csaba P. Kovesdy","Cuilin Zhang","Curtis C. Harris","Curtis Huttenhower","Curtis J. Milhaupt","Curtis LeMay","Cynthia Dwork",
  "Cynthia Fisher","Cynthia Kenyon","Cynthia M. Beall","Cynthia M. Bulik","Cynthia Mulrow","Cynthia Rudin","Cyril Burt",
  "Cyril Ponnamperuma","Cyril Thomas","Czes\322aw Mi\322osz","D. C. Rao","D. H. Mellor","D. I. Boomsma","D. Johnson",
  "D. N. Jackson","D. S. Falconer","D. S. Hirschberg","D. Taylor","D. W. Fulker","Dabeeru C. Rao","Dacheng Tao",
  "Dacher Keltner","Dagomar Degroot","Daisuke Inoue","Daisy Zamora","Dale Allison","Dale Webster","Dalton Conley",
  "Damien Broderick","Damion Searls","Dan Boneh","Dan Braun","Dan Brown","Dan Charles","Dan Coates",
  "Dan Hendrycks","Dan Hesse","Dan J. Stein","Dan Jackson","Dan Jurafsky","Dan Klein","Dan Liu",
  "Dan M. Roden","Dan Mason","Dan Mazur","Dan Morgan","Dan Roden","Dan Roth","Dan Schmidt",
  "Dan Schwartz","Dan Zhang","Dana Angluin","Dana H. Ballard","Dana H. Born","Dana Klisanin","Dana Scott",
  "Daniel A. Geller","Daniel A. Spielman","Daniel Acuna","Daniel B. Wright","Daniel Bates","Daniel Bernstein","Daniel C. Dennett",
  "Daniel Campos","Daniel D. Johnson","Daniel Daneshvar","Daniel Dennett","Daniel E. Ho","Daniel Eriksson","Daniel Franklin",
  "Daniel Freeman","Daniel Fried","Daniel G. Goldstein","Daniel Geschwind","Daniel Gianola","Daniel Glass","Daniel H. Geschwind",
  "Daniel Ho","Daniel Hoffman","Daniel Hsu","Daniel I. Rees","Daniel J. Bauer","Daniel J. Bernstein","Daniel J. Carroll",
  "Daniel J. Drucker","Daniel J. Kleitman","Daniel J. Rader","Daniel Jarrett","Daniel Kahneman","Daniel Kang","Daniel Kish",
  "Daniel Kokotajlo","Daniel Kruger","Daniel L. Schacter","Daniel L. Schwartz","Daniel Lehmann","Daniel Levey","Daniel Licht",
  "Daniel M. Wegner","Daniel Marcus","Daniel Munro","Daniel Nettle","Daniel O\8217Connell","Daniel Peek","Daniel Promislow",
  "Daniel R. Weinberger","Daniel Rader","Daniel Ritchie","Daniel Rock","Daniel Romer","Daniel Russo","Daniel S. Hamermesh",
  "Daniel S. Weld","Daniel Sawyer","Daniel Schultz","Daniel Schwartz","Daniel Silver","Daniel T. Blumstein","Daniel T. Gilbert",
  "Daniel T. Willingham","Daniel Tranel","Daniel Treisman","Daniel Valente","Daniel Vogel","Daniel Watson","Daniel Wolf",
  "Daniel Zhang","Daniel Ziegler","Daniela Amodei","Daniela Giordano","Daniela Rus","Danielle Dick","Danielle M. Dick",
  "Danielle Matthews","Danielle Posthuma","Danielle S. McNamara","Daniil Pakhomov","Danny Hernandez","Danqi Chen","Danuta Wasserman",
  "Daphne Bavelier","Daphne Martschenko","Darby Saxbe","Daren Liu","Dario Amodei","Dario Benedetto","Dario Maestripieri",
  "Dariush Mozaffarian","Daron Acemoglu","Darren Dunning","Darren Platt","Daryl J. Bem","Dashun Wang","Dave Cummings",
  "David  A. Steen","David A. Fidock","David A. Hughes","David A. Kenny","David A. Scheinberg","David A. Siegel","David A. Sinclair",
  "David A. Turner","David Ackerman","David Amar","David B. Allison","David B. Audretsch","David B. Richman","David Balding",
  "David Becker","David Berthelot","David Bieber","David Blitz","David Borwein","David Buss","David C. Funder",
  "David C. Geary","David C. McClelland","David C. Parkes","David C. Rowe","David C. Schmittlein","David Cameron","David Card",
  "David Carmody","David Cesarini","David Chan","David Chiang","David Choi","David Cicero","David Cock",
  "David Cope","David Corcoran","David Corley","David Crabb","David Cuthbertson","David D. Friedman","David D. Kirkpatrick",
  "David Dale","David Deutsch","David Ding","David Do","David Dunning","David E. Culler","David E. Harrison",
  "David E. Olson","David E. Rumelhart","David Ebert","David Edward Goldberg","David F. Bjorklund","David F. Dinges","David F. Horrobin",
  "David Faust","David Foster Wallace","David G. Rand","David G. Simons","David G. Stork","David Gelles","David Glover",
  "David Goodstein","David Grande","David Grove","David H. Autor","David H. Hackworth","David H. Ledbetter","David Hackett",
  "David Haden","David Harker","David Haussler","David Heinemeier Hansson","David Held","David Hinds","David Hirshleifer",
  "David Hsu","David Huffman","David Hunter","David I. Laibson","David I. Stuart","David J. Beerling","David J. C. MacKay",
  "David J. Chalmers","David J. Cooper","David J. Deming","David J. Hunter","David J. Kupfer","David J. Stevenson","David J. White",
  "David Jensen","David Krueger","David L. Banks","David L. Donoho","David L. Fried","David L. Hu","David Laibson",
  "David Laitin","David Lau","David Lazer","David Leiser","David Levine","David Li","David Lobell",
  "David Lordkipanidze","David Lowenthal","David Lubinski","David M. Blei","David M. Buss","David M. Evans","David M. Greenberg",
  "David M. Lawson","David M. Lee","David M. Markowitz","David Maimon","David Mayo","David Mazi\232res","David Metcalfe",
  "David Meyre","David Moher","David Moreau","David Mowat","David N. Weil","David Nutt","David O. Conover",
  "David Ong","David Owen","David P. Farrington","David P. Strachan","David Porteous","David R. Brillinger","David R. Liu",
  "David R. Moore","David Reichert","David Rein","David Richter","David Rios Insua","David S. Johnson","David S. Ludwig",
  "David Salgado","David Saxon","David Schlessinger","David Sexton","David Shiffman","David Sirlin","David Smith Calverley",
  "David So","David Stern","David Stove","David T. Lykken","David Talbot","David Tan","David Thissen",
  "David Valle","David Vlahov","David Vokrouhlick\253","David W. Clark","David W. Craig","David W. Cushman","David W. Keith",
  "David W. Macdonald","David W. Tank","David Waller","David Wechsler","David Weitz","David Wells","David Whitney",
  "David Wisniewski","David Wu","David Xiao","David de la Croix","Davide Piffer","Davide Scaramuzza","Dawn Song",
  "Dawson R. Engler","De Wet Swanepoel","Dean Karlan","Dean Keith Simonton","Dean Rickles","Debbie Lawlor","Debby Herbenick",
  "Deborah A. Cobb-Clark","Deborah A. Lawlor","Deborah A. Nickerson","Deborah H. Gruenfeld","Deborah Jarvis","Deborah Lowe Vandell","Deborah Morgan",
  "Deborah Phillips","Deborah Schofield","Debra J. Skene","Dedra Buchwald","Deepak L. Bhatt","Deepti Gurdasani","Deirdre Donnelly",
  "Deirdre O\8217Brien","Demis Hassabis","Dena G. Hernandez","Denis Davydov","Denis Dimitrov","Denis Tarasov","Denis Vaughan",
  "Denis Walsh","Denise C. Park","Denise Robinson","Denise Williams","Dennis Brooks","Dennis Coates","Dennis Kim",
  "Dennis M. Levi","Dennis Schmitt","Dennis Van der Meer","Denny Borsboom","Denys Ovenden","Department of Justice","Depths of Wikipedia",
  "Derek Abbott","Derek C. Angus","Derek Chen","Derek Lowe","Derek Pang","Derek de Solla Price","Derk-Jan Dijk",
  "Dermot Walsh","Desmond Elliott","Detlef Weigel","Devah Pager","Devavrat Shah","Devi Parikh","Devon Rifkin",
  "Dharmendra Modha","Dharmendra S. Modha","Dharshan Kumaran","Dian Donnai","Diana Fleischman","Diana Johnson","Diana O. Perkins",
  "Diana Reiss","Diane F. Halpern","Diane M. Becker","Diane Van Deren","Didier Sornette","Diego Reyes","Diego Vega",
  "Diego Villar","Dieter Ebert","Dieter Fox","Dieter Frey","Diether Lambrechts","Dileep George","Dimitar Kostadinov",
  "Dimitri Bertsekas","Dimitri P. Bertsekas","Dimitris Bertsimas","Dimitris Metaxas","Dina Katabi","Dinei Florencio","Dinesh Chugtai",
  "Diogo Almeida","Dirk Bergemann","Dirk Bezemer","Dirk Englund","Divya Mehta","Diyi Yang","Dmitry Akimov",
  "Dmitry Kazhdan","Dmitry Molchanov","Dobroslav Chrobak","Doina Precup","Dolores Albarrac\237n","Dolores Malaspina","Dominic Holland",
  "Dominic King","Dominic Masters","Dominik Tatarka","Don Curtis","Don Hopkins","Don Stark","Don Syme",
  "Donald B. Lindsley","Donald B. Rubin","Donald F. Klein","Donald G. Saari","Donald Gurnett","Donald I. Templer","Donald Katz",
  "Donald Keene","Donald Knuth","Donald Michie","Donald P. Green","Donald R. Miller","Donald T. Campbell","Donald W. Black",
  "Donald W. Pfaff","Dong Chen","Dongju Zhang","Dongmei Wang","Donna K. Arnett","Dora Akunyili","Dora L. Costa",
  "Dorin Comaniciu","Doris Tsao","Doris Y. Tsao","Dorothy Burlingham","Dorothy Nevill","Dorret Boomsma","Doug Downey",
  "Doug Lenat","Douglas Blackwood","Douglas C. Schmidt","Douglas F. Easton","Douglas Fraser","Douglas G. Altman","Douglas H. Fisher",
  "Douglas Hofstadter","Douglas K. Detterman","Douglas R. Hofstadter","Douglas T. Kenrick","Douglas W. Allen","Dov Cohen","Dragana Rogulja",
  "Dragomir Radev","Drazen Prelec","Drew McDermott","Drew Weissman","Duncan J. Watts","Duncan Lawrence","Duncan Ryuken Williams",
  "Dustin Wright","Dwight Dickinson","E. B. Titchener","E. E. Salpeter","E. L. Lehmann","E. M. Purcell","E. Mavis Hetherington",
  "E. Paul Torrance","E. Robinson","E. S. Pearson","E. S. Pondiczery","E. W. Brown","Eamon McCrory","Eamonn Sheridan",
  "Earl Miner","Ed Boyden","Ed Chi","Ed H. Chi","Ed O\8217Brien","Ed Yong","Edmund Fantino",
  "Edmund Sonuga-Barke","Edmund T. Rolls","Edouard Louis","Edouard Machery","Edsger W. Dijkstra","Eduard Hovy","Eduard Vieta",
  "Eduardo Baptista","Edvard I. Moser","Edvard Johansson","Edward Ames","Edward Carmines","Edward E. Leamer","Edward E. Smith",
  "Edward F. Chang","Edward F. Moore","Edward Feigenbaum","Edward Fredkin","Edward G. Jones","Edward G. Seidensticker","Edward Gibson",
  "Edward Giovannucci","Edward H. Adelson","Edward J. Larson","Edward Johns","Edward L. Glaeser","Edward L. Thorndike","Edward M. Gramlich",
  "Edward M. Miller","Edward M. Scolnick","Edward Miguel","Edward Neville da Costa Andrade","Edward O. Thorp","Edward P. Lazear","Edward Pratt",
  "Edward Rosen","Edward S. Boyden","Edward S. Buckler","Edward Scolnick","Edward Slingerland","Edward T. Bullmore","Edward Teller",
  "Edward Tufte","Edward W. Felten","Edward Wong","Edward Yang","Edwin G. Boring","Edythe London","Edzard Ernst",
  "Eero P. Simoncelli","Efim Zelmanov","Efraim Benmelech","Eileen Roberts","Eivind Ystr\248m","Ekaterina Orlova","Elad Hazan",
  "Elaine Wyllie","Elchanan Mossel","Eldar Shafir","Eleanor A. Maguire","Eleanor Feingold","Eleazar Eskin","Eleftheria Zeggini",
  "Elena Shumskaya","Eli Lilly","Eli Somer","Eli Y. Adashi","Elias G. Carayannis","Eliezer Yudkowsky","Eliot A. Cohen",
  "Elisabeth Welch","Elisabeth West FitzHugh","Elizabeth A. Fenn","Elizabeth A. Phelps","Elizabeth A. Stuart","Elizabeth A. Thompson","Elizabeth Barnes",
  "Elizabeth Barry","Elizabeth Bates","Elizabeth Dunne","Elizabeth F. Loftus","Elizabeth Gaskell","Elizabeth Gibney","Elizabeth Griffith",
  "Elizabeth J. Perry","Elizabeth K. Cahoon","Elizabeth L. Bjork","Elizabeth Loftus","Elizabeth Michael","Elizabeth Montgomery","Elizabeth Pennisi",
  "Elizabeth S. Spelke","Elizabeth Selvin","Elizabeth Spelke","Elizabeth Steele","Elizabeth Weil","Elizabeth Williamson","Ella Fitzgerald",
  "Ellen Byron","Ellen J. Langer","Ellen Leibenluft","Elliot Richards","Elliot S. Gershon","Elliot S. Vesell","Elliot Tucker-Drob",
  "Elon Musk","Elsdon Storey","Emad Mansoor","Emad Mostaque","Emanuel Miller","Emanuele Felice","Emelia J. Benjamin",
  "Emi Hasegawa","Emi Nakamura","Emil Hagstr\246m","Emil O. W. Kirkegaard","Emilie Kaufmann","Emilio Ferrer","Emilio Ros [The Walnuts",
  "Emily Allen","Emily Chew","Emily Gerard","Emily Gould","Emily Jane Fox","Emily Johnson","Emily M. Bender",
  "Emily Mitchell","Emily Morton","Emily Oster","Emily Parker","Emily Pronin","Emily Short","Emily Weiss",
  "Emily Wilkinson","Emma C. Teeling","Emma Cohen","Emma Copley Eisenberg","Emma Frans","Emma Rice","Emmanouil T. Dermitzakis",
  "Emmanuel Carr\232re","Emmanuel Le Roy Ladurie","Emmeline Edwards","Emmett Shear","Emory S. Bogardus","English Wikipedia","Enoch Callaway",
  "Enrique Santiago","Enzo Scifo","Equestria Daily","Eran Elinav","Eran Segal","Eran Shor","Erez Ben-Yosef",
  "Erhan Guven","Eric A. Hanushek","Eric A. Posner","Eric B. Rimm","Eric Brill","Eric Chu","Eric Drexler",
  "Eric Fombonne","Eric Goles","Eric Hambro","Eric Hill","Eric Horvitz","Eric J. Johnson","Eric J. Topol",
  "Eric Kaufmann","Eric Keen","Eric Langlois","Eric M. Smith","Eric N. Olson","Eric Nestler","Eric Newcomer",
  "Eric Nichols","Eric Nielsen","Eric Nyberg","Eric R. Gamazon","Eric Ravussin","Eric Rimm","Eric Rubin",
  "Eric S. Lander","Eric S. Raymond","Eric S. Schmitt","Eric Schadt","Eric Schmidt","Eric Schulz","Eric Schwitzgebel",
  "Eric Sunderland","Eric Turkheimer","Eric Verdin","Eric Vilain","Eric Wallace","Eric Wang","Eric Williams",
  "Eric Wong","Eric Xing","Eric von Hippel","Eric-Jan Wagenmakers","Erich D. Jarvis","Erich Friedman","Erik Brynjolfsson",
  "Erik Christensen","Erik Christiansen","Erik D. Demaine","Erik Demaine","Erik Frey","Erik Hoel","Erik Johnson",
  "Erik Lindgren","Erik Lindqvist","Erik Postma","Erik Zimen","Erika Jensen-Jarolim","Erin D. Michos","Erin M. Gibson",
  "Erin Sanders","Ernest Beaglehole","Ernest Hartmann","Ernest R. Hilgard","Ernst Caspari","Ernst Fehr","Eshel Ben-Jacob",
  "Eske Willerslev","Essi Viding","Esther Duflo","Esther Thelen","Ethan Kaplan","Ethan Kross","Ethel M. Elderton",
  "Ethel Person","Etzel Carde\241a","Euan McLean","Eudald Carbonell","Eugene Galanter","Eugene V. Koonin","Eugene W. Myers",
  "Eugene Wu","Eva Baker","Eva Green","Eva Miranda","Eva Vivalt","Evan Armstrong","Evan E. Eichler",
  "Evan Mast","Evan Maxwell","Evangelos Eleftheriou","Evdokia Anagnostou","Eve Armstrong","Evelina Fedorenko","Evelynn Hammonds",
  "Everett Mendelsohn","Evgenii Nikishin","Ewa Deelman","Ewa Grabowska","Ewald Ammende","Ewan Birney","Ewout W. Steyerberg",
  "Ezra Klein","E\246rs Szathm\225ry","F. P. Ramsey","Fabio Petroni","Fahu Chen","Faisal Mushtaq","Fan Bao",
  "Fan Hui","Fan Jiang","Fan Li","Fan Liang","Fan Wu","Fan Yi","Fang Fang",
  "Fang Liu","Farah Naz Talpur","Farhad Moshiri","Farinaz Koushanfar","Faruk Ahmed","Fatih Porikli","Federico Casarini",
  "Federico Moretti","Fei Peng","Fei Xia","Fei Yu","Fei-Yue Wang","Felix A. Gers","Felix Creutzig",
  "Felix Friedrich","Felix Heide","Felix M\252ller","Felix Oberholzer-Gee","Feng Li","Feng Qi","Feng Shi",
  "Feng Yu","Feng Zhang","Ferenc Gombos","Fergus Henderson","Fergus Shanahan","Fernand Gobet","Fernanda Vi\233gas",
  "Fernando D\237az","Fernando Pereira","Fernando Reinares","Fernando Rosas","Fiery Cushman","Filip Kovacevic","Finale Doshi-Velez",
  "Finn Rasmussen","Florence L. Goodenough","Florent Krzakala","Florian Alt","Florian Fuchs","Foster Provost","Foutse Khomh",
  "Frances Griffiths","Frances H. Arnold","Francesca Dominici","Francesca Gino","Francesca Happ\233","Francesco Barbieri","Francesco Bartoli",
  "Francesco Bettella","Francesco Nori","Francesco Salvi","Francis Collins","Francis Fukuyama","Francis Galton","Francis J. McMahon",
  "Francis S. Collins","Francis T. McAndrew","Francis Yates","Francisco Guzman","Franco Lucchini","Franco Moretti","Francois Balloux",
  "Francois Lanusse","Frank A. Chervenak","Frank B. Hu","Frank C. J. McGurk","Frank C. Worrell","Frank Drake","Frank Dudbridge",
  "Frank E. Speizer","Frank Falkner","Frank Hu","Frank Keller","Frank Key","Frank L. Schmidt","Frank Levy",
  "Frank M. Spinath","Frank McCown","Frank McSherry","Frank Middleton","Frank N. Freeman","Frank P. Ramsey","Frank Rosenblatt",
  "Frank Ruskey","Frank Wang","Frank Zhang","Frantisek Svantner","Franz K\246nig","Fran\231ois Balloux","Fran\231ois Chollet",
  "Frazer Anderson","Fred Brooks","Fred H. Gage","Fred W. Johnson","Frederick Mosteller","Frederico Finan","Fredrik Ull\233n",
  "Freeman Dyson","Freeman J. Dyson","Fritz Cremer","Fritz Heider","Frosti Jonsson","Fruhling Rijsdijk","Fr\252hling Rijsdijk",
  "Fujiwara no Teika","G. A. Barnard","G. Davey Smith","G. E. Moore","G. Thomson","G. Warren Nutter","Gabor Tardos",
  "Gabriella Juh\225sz","Gabriella Morreale de Escobar","Gad Saad","Gail C. Murphy","Gail Davies","Gail P. Jarvik","Ganesh Sittampalam",
  "Gang Hua","Gang Liu","Gao Huang","Gardner Murphy","Gareth Cross","Garett Jones","Garrett Ienner",
  "Garry Newman","Garry Tan","Garth Davies","Garth Saloner","Gary Alan Fine","Gary Drescher","Gary E. McPherson",
  "Gary Greenberg","Gary Hustwit","Gary LaFree","Gary Lynch","Gary Marcus","Gary S. Becker","Gary Saul Morson",
  "Gautam Gulati","Gavin A. Schmidt","Gavin Andresen","Gavin E. Crooks","Gavin Wright","Ge Li","Ge Wang",
  "Ge Yang","Gemma Modinos","Gene E. Robinson","Gene Tsudik","Gene V. Glass","Gene Wolfe","Generation Scotland",
  "Geoff Davis","Geoff MacDonald","Geoffrey E. Hinton","Geoffrey F. Miller","Geoffrey Hinton","Geoffrey Hodgson","Geoffrey Holmes",
  "Geoffrey J. Gordon","Geoffrey Ling","Geoffrey M. Hodgson","Geoffrey Wood","Georg Ehret","Georg Simmel","George Altman",
  "George B. Dantzig","George B. Grant","George Boolos","George D. Smith","George D. Stoddard","George D. Yancopoulos","George Danezis",
  "George Davey Smith","George Davey-Smith","George E. Murphy","George E. P. Box","George F. Sutherland","George Fitzmaurice","George Heald",
  "George Hotz","George J. Borjas","George Kurian","George L. Trager","George Loewenstein","George M. Church","George Musser",
  "George Packer","George Papandreou","George Preti","George Q. Daley","George R. Price","George S. Brown","George Schlager Welsh",
  "George Stamatoyannopoulos","George Stein","George W. Ross","George Washington","George Yancey","George Yancopoulos","George Zimmerman",
  "George van Driem","Georges El Fakhri","Georgia Chenevix-Trench","Georgia M. Dunston","Georgios Athanasiadis","Georgios Georgakis","Georgios N. Yannakakis",
  "Georgios Papagiannis","Geraint Rees","Gerald E. McClearn","Gerald Feinberg","Gerald Friedland","Gerald Holton","Gerald M. Rubin",
  "Gerald R. Ferris","Geraldine Dawson","Gerard D. Schellenberg","Gerard J. Tellis","Gerard Manley Hopkins","Gerard Sanacora","Gerard Saucier",
  "Gerard van den Berg","Gerardo Navarrete","Gerhard Andersson","Gerhard Neumann","Gerhard Roth","Gerome Breen","Gian-Carlo Rota",
  "Giang Nguyen","Gianpiero D. Palermo","Gideon J. Mellenbergh","Gideon Koren","Gil McVean","Gilbert S. Omenn","Gilbert Strang",
  "Gilles E. Gignac","Gilles Saint-Paul","Gillian Bennett","Gillian Hadfield","Gillian Tett","Gina Choe","Gina Kolata",
  "Giorgio Mariani","Giovanni Sala","Giovanni Veronesi","Giovanni Vigna","Girardin Jean-Louis","Girish Kumar","Girishwar Misra",
  "Gitte Moos Knudsen","Giulio Perugi","Giulio Tononi","Giuseppe Gabrielli","Giuseppe Petrosino","Giuseppe Soda","Gjergji Kasneci",
  "Glen Baker","Glen Owen","Glenn C. Loury","Glenn Loury","Glenn MacDonald","Glenn Turner","Gloria Chang",
  "Gloria Mark","Glyn Lewis","Godfrey Thomson","Golan Levin","Goldine C. Gleser","Goldine Gleser","Goncalo Abecasis",
  "Goncalo R. Abecasis","Gon\231alo Abecasis","Gon\231alo R. Abecasis","Google Sheets","Goran Knezevic","Gordon Pennycook","Gordon W. Schuett",
  "Grace Akello","Grace Chan","Grace Chang","Grace Chu","Graham Coop","Graham Finlayson","Graham Gower",
  "Graham Healy","Graham Law","Graham Taylor","Grant Atkins","Grant Goldman","Grant Thornton","Greg Brockman",
  "Greg Crawford","Greg Gorman","Greg Hampikian","Greg J. Duncan","Greg Kumparak","Greger Larson","Gregor Betz",
  "Gregor Hasler","Gregory Andrews","Gregory B. Northcraft","Gregory Bateson","Gregory Benford","Gregory Burke","Gregory D. Hager",
  "Gregory Dudek","Gregory Francis","Gregory M. Fahy","Gregory S. Berns","Gretchen Chapman","Gudrun Wagner","Guido Barbujani",
  "Guido Kroemer","Guillaume Leclerc","Guillermo Sapiro","Gunther Eysenbach","Guo Li","Guoyao Wu","Gustav Kuhn",
  "Gustav Larsson","Gustavo Turecki","Guy Fournier","Guy Katz","Guy Lever","Guy Madison","Guy Montrose Whipple",
  "G\225bor Scheiring","G\225bor Vajta","G\233rard Ben Arous","H. Andrew Schwartz","H. B. Barlow","H. M. Collins","H. P\233tard",
  "H. V. Jagadish","H. W. B. Joseph","H. Wilkins","Hagop S. Akiskal","Haidar Khan","Haifeng Fu","Haim Sompolinsky",
  "Haim Weizman","Haitao Zheng","Haiyan Zhang","Haizhou Li","Hakon Hakonarson","Hal R. Varian","Hamed Haddadi",
  "Hamed Zamani","Hamid Kazemi","Hamilton O. Smith","Han Li","Han Zhang","Hana El-Samad","Hang Zhou",
  "Hannah Devlin","Hannah Kim","Hannah Lee","Hannah Miller","Hannah Robinson","Hannele Ruohola-Baker","Hannes Baumann",
  "Hannes Petursson","Hannu Lahtinen","Hannu Rajaniemi","Hans Clevers","Hans Eiberg","Hans Eriksson","Hans Gruber",
  "Hans J. Eysenck","Hans M. Kristensen","Hans Moravec","Hans Robert Sch\246ler","Hans-Peter Kohler","Hans-Ulrich Wittchen","Hanspeter Pfister",
  "Hany Farid","Hao Li","Hao Wei","Hao Ying","Hao Zhou","Harjeet Singh","Harold Hotelling",
  "Harold M. Williams","Harold Matthews","Harold Morowitz","Harold Pashler","Harold S. Stone","Harold Schneider","Harold Snieder",
  "Harris A. Lewin","Harrison Edwards","Harrison Hong","Harry Bouwman","Harry Brandt","Harry H. Harman","Harry Harper",
  "Harry Olson","Harry Ostrer","Hartmut Neven","Haruko Obokata","Harvey A. Carr","Harvey Carr","Hazel Brown",
  "He Ma","He Zhang","He Zhou","Healthy Aging Study]","Heather Huntington","Heather Zar","Heikki Kainulainen",
  "Heiner Rindermann","Heinrich Jasper","Heinrich Peters","Heinz Feldmann","Helen Atkinson","Helen H. Hobbs","Helen Leonard",
  "Helen M. Blau","Helen Toner","Helen Y. Chu","Helene Benveniste","Helge Kragh","Helgi Jonsson","Helmut Sch\228fer",
  "Helmuth Nyborg","Hendrik Poinar","Heng Ji","Heng Li","Heng Yang","Henk Barendregt","Henkjan Honing",
  "Henri Chabrol","Henri Leridon","Henrich R. Greve","Henrik Anckars\228ter","Henrik Kleven","Henrik Larsson","Henrik Zetterberg",
  "Henrique Rocha","Henry Brinkerhoff","Henry E. Garrett","Henry H. Goddard","Henry H. Richardson","Henry J. Heimlich","Henry J. Kelley",
  "Henry K. Beecher","Henry Kranzler","Henry L. Paulson","Henry L. Roediger","Henry L. Roediger III","Henry Mayhew","Henry W. Lin",
  "Henry Wong","Herbert A. Simon","Herbert Maschner","Herbert Spencer Jennings","Herbert Van de Sompel","Herbert Weissbach","Herman Aguinis",
  "Herman H. Spitz","Herman Pleij","Herman Pontzer","Hermann Wagner","Hermine Maes","Hernan Aguirre","Herwig Baier",
  "Hessel Oosterbeek","Hideaki Anno","Hideyuki Okano","Hilary Finucane","Hilary Hoynes","Hilary W. Hoynes","Himabindu Lakkaraju",
  "Himanshu Thakur","Hiram Stevens Maxim","Hiroshi Ishikawa","Hiroshi Kaneda","Hirotaka Sugawara","Hiroyuki Matsumoto","Hiroyuki Morita",
  "Hiroyuki Sasaki","Hisham Al-Obaidi","Hod Lipson","Holden Karnofsky","Holger Rootz\233n","Hollis Robbins","Holly A. Taylor",
  "Holly Jackson","Hong Chen","Hong Wei","Hong Wu","Hong Ye","Hong Yu","Hongkui Deng",
  "Hoon Chung","Hopi E. Hoekstra","Horace Barlow","Horatio H. Newman","Horst D. Simon","Hovav Shacham","Howard Gardner",
  "Howard Raiffa","Howard S. Becker","Howard S. Liddell","Howard Wainer","Howard Y. Chang","Hoyt Bleakley","Hrishikesh Joshi",
  "Hsiao-Wuen Hon","Hsinchun Chen","Hu Chen","Hu Haifeng","Hua Chen","Hua Hua","Hua Shao",
  "Huan Liu","Huan Xia","Huang Huang","Huanming Yang","Hubert L. Dreyfus","Huda Akil","Hugh Calkins",
  "Hugh Gurling","Hugh Gusterson","Hugh McColl","Hugh Sinclair","Hugo Critchley","Hui Chen","Hui Lei",
  "Hui Liu","Hui Shi","Hui Wu","Hui Xiong","Hui Yang","Hyeonwoo Kim","Hyo Kang",
  "Hyun Jin Kim","Hyung Chul Kim","H\224n Th\7871 Th\224nh","I-Chen Wu","I. Glenn Cohen","I. J. Deary","I. J. Good",
  "Ian Anderson","Ian Beer","Ian D. Cameron","Ian Day","Ian Ellis","Ian Ford","Ian Goodfellow",
  "Ian H. Gotlib","Ian J. Deary","Ian Meinertzhagen","Ian Mercer","Ian Tomlinson","Ian Watt","Ibrahim Oweiss",
  "Ido Bachelet","Iftikhar Khan","Iggy Pop","Ignacio Alvarez","Igor Douven","Igor Fedorov","Igor Larrosa",
  "Igor Rudan","Ila Fiete","Ilina Singh","Ilona Kov\225cs","Ilya A. Strebulaev","Ilya Mironov","Ilya Segal",
  "Ilya Sutskever","Immaculata De Vivo","Inflection AI","Ingo Potrykus","Ingo Rechenberg","Ingvar Andersson","Ingvild Alm\229s",
  "Insoo Hyun","Internet Archive","Intikhab Alam","Ioanna Tzoulaki","Ion Stoica","Ionica Smeets","Iosif Szilagyi",
  "Irenaus Eibl-Eibesfeldt","Iren\228us Eibl-Eibesfeldt","Irfan Essa","Irina Blok","Iroise Dumontheil","Irving I. Gottesman","Irving John Good",
  "Irving Kirsch","Irving L. Janis","Irving Lorge","Irwin D. Waldman","Irwin Silverman","Irwin Waldman","Iryna Gurevych",
  "Isaac Asimov","Isabelle Augenstein","Isabelle Boutron","Isabelle Gallagher","Isabelle Guyon","Isabelle Peretz","Ishan Pandey",
  "Israel Ramirez","Itai Yanai","Italo Calvino","Itamar Simonson","Itsik Pe\8217er","Itzhak Brook","Ivan Curjuric",
  "Ivan Horvath","Ivan Krasko","Ivan Petkov","Ivan Skorokhodov","Ivan Sutherland","Ivan Titov","Ivan Vuli\263",
  "Iyad Rahwan","J. A. Allen","J. Allan Hobson","J. B. MacKinnon","J. B. Rhine","J. B. S. Haldane","J. Bradford DeLong",
  "J. C. Barnes","J. C. DeFries","J. C. White","J. Craig Venter","J. Doyne Farmer","J. F. Price","J. H. Saltzer",
  "J. K. Rowling","J. Karjalainen","J. Keith Joung","J. Kevin O\8217Regan","J. L. Austin","J. M. Clark","J. M. Hammersley",
  "J. M. Robson","J. Mark G. Williams","J. McAdam","J. Michael Bailey","J. P. Guilford","J. Patrick Gray","J. Philippe Rushton",
  "J. R. Anderson","J. R. R. Tolkien","J. R. Smith","J. Rhodes","J. Richard Gott III","J. Robbins","J. Russell",
  "J. Scott Armstrong","J. W. Johnson","J. Ward","J. Wilson","Jaakko Kaprio","Jaap Murre","Jack A. Naglieri",
  "Jack Block","Jack Bowden","Jack C. Taylor","Jack D. Dunitz","Jack Devine","Jack Goldberg","Jack Hitt",
  "Jack Humphrey","Jack London","Jack Lynch","Jack Ogden","Jack Porter","Jack Rae","Jack Samuels",
  "Jackie Crawford","Jackie Kay","Jackson Peak","Jacob Anderson","Jacob Austin","Jacob Hooker","Jacob Katz",
  "Jacob Lawrence","Jacob Marschak","Jacob Plange-Rhule","Jacob Reimer","Jacob Rosenthal","Jacob Steinhardt","Jacob Taylor",
  "Jacob Wallace","Jacqueline Robinson","Jacques Fellay","Jacques Hadamard","Jacques Lamy","Jacques Rossouw","Jad Abumrad",
  "Jagmeet Singh","Jaime Carbonell","Jakub Konecny","James A. Baldwin","James A. Evans","James A. Horne","James A. King",
  "James A. Robinson","James A. Thomson","James Alexander Hamilton","James Andreoni","James B. Jordan","James B. Stewart","James Bessen",
  "James Childs","James Clement","James Cook","James Cross","James Crouse","James D. Stewart","James D. Weinrich",
  "James Demmel","James Donald Weinrich","James E. Bruce","James E. Miller","James E. Mitchell","James E. Morgan","James E. Rothman",
  "James F. Crow","James F. Fries","James F. Gusella","James F. Wilson","James Feyrer","James G. Wilson","James Glass",
  "James Griesemer","James H. Cartwright","James H. Leuba","James H. Stark","James H. Wyckoff","James Hammill","James Hansen",
  "James Heckman","James J. Bull","James J. Cimino","James J. Heckman","James J. Lee","James J. Murphy","James Jordon",
  "James K. Galbraith","James Koppel","James L. McClelland","James L. McGaugh","James Landay","James M. Cook","James M. Davis",
  "James MacKillop","James Manyika","James Marston Fitch","James McCracken","James Molloy","James P. Cook","James P. Crutchfield",
  "James P. Noonan","James Paulson","James Pinkerton","James R. Wilson","James Randi","James Ryley","James Sales",
  "James Shelley","James Surowiecki","James T. Austin","James Tiptree Junior","James Torrance","James V. Haxby","James W. Vaupel",
  "James Wilkinson","Jamie Hall","Jamie Kerr","Jamie Peters","Jan Born","Jan C. van Ours","Jan Chovanec",
  "Jan Havl\237\269ek","Jan K. Buitelaar","Jan M. Rabaey","Jan O. Korbel","Jan Pawlowski","Jan Pettersson","Jan Richter",
  "Jan Seidel","Jan Vijg","Jan te Nijenhuis","Jan van der Laan","Jan-Emmanuel De Neve","Jan-Eric Gustafsson","Jan-\197ke Gustafsson",
  "Jana Hlav\225\269ov\225","Jana Schneider","Jane A. Cauley","Jane C. Charlton","Jane Cauley","Jane Gibson","Jane Greaves",
  "Jane Hurst","Jane Hutton","Jane Kim","Jane Loevinger","Jane Murray","Jane Phillips","Jane Wardle",
  "Jane-Ling Wang","Janet Coles","Janet D. Elashoff","Janet Kelso","Janet M. Thornton","Janet Pierrehumbert","Janet Seeley",
  "Janet Shibley Hyde","Janet Treasure","Janey L. Wiggs","Janice Chen","Janice K. Kiecolt-Glaser","Janina Jeff","Janina M. Jeff",
  "Janos Galambos","Janusz Jankowski","Jared Diamond","Jaroslav Flegr","Jaroslav Sevcik","Jaroslava Blazkova","Jarrod Tanny",
  "Jasjeet S. Sekhon","Jason Boardman","Jason Collins","Jason D. Boardman","Jason Downer","Jason Ferris","Jason Fletcher",
  "Jason H. Moore","Jason H. Steffen","Jason Huang","Jason Liang","Jason Liu","Jason Mars","Jason McIntyre",
  "Jason Sanders","Jason Weston","Javier Benitez","Javier Martin","Javier de la Rosa","Jay Adams","Jay Bhattacharya",
  "Jay Joseph","Jay L. Lush","Jay McClelland","Jay S. Kaufman","Jay Shendure","Jay Tischfield","Jayanta Debnath",
  "Jazmine Hughes","Jean Bousquet","Jean Decety","Jean Dussault","Jean Harrington","Jean M. Twenge","Jean Maillard",
  "Jean Pelletier","Jean Shin","Jean Wactawski-Wende","Jean-Claude Tardif","Jean-Jacques Hublin","Jean-Marc Moret","Jean-Michel Gaillard",
  "Jean-Paul Delahaye","Jean-Pierre Boissel","Jeanette Taylor","Jeanne Brooks-Gunn","Jed McCaleb","Jef Caers","Jef D. Boeke",
  "Jeff Atwood","Jeff Bezos","Jeff Binder","Jeff Dean","Jeff Potter","Jeff Sachs","Jeff Schneider",
  "Jeff W. Lichtman","Jeff Wang","Jeffery Vance","Jeffrey A. Lieberman","Jeffrey Dean","Jeffrey Jensen","Jeffrey L. Bennetzen",
  "Jeffrey Lieberman","Jeffrey Pennington","Jeffrey R. Kling","Jeffrey T. Hancock","Jeffrey T. Leek","Jehannine Austin","Jelte M. Wicherts",
  "Jenae M. Neiderhiser","Jennifer A. Doudna","Jennifer Callahan","Jennifer Clark","Jennifer Doudna","Jennifer Eccles","Jennifer Harper",
  "Jennifer Howell","Jennifer Jordan","Jennifer Wilcox","Jennifer Williamson","Jennifer Wortman Vaughan","Jennifer Young","Jenny Morton",
  "Jenny Tung","Jens Alber","Jens Bangsbo","Jens Ludwig","Jens Petersen","Jens-Christian Svenning","Jeremy Atack",
  "Jeremy Avigad","Jeremy Bernstein","Jeremy Cole","Jeremy Elson","Jeremy Evans","Jeremy Freese","Jeremy Gibbons",
  "Jeremy J. Stone","Jeremy Kahn","Jeremy Nixon","Jeremy P. E. Spencer","Jeremy Tankard","Jeremy Thomas","Jermaine Hall",
  "Jerome Connor","Jerome E. Singer","Jerome Lewis","Jerome Sarris","Jerrold E. Marsden","Jesper L\252tzen","Jess Smith",
  "Jesse Chandler","Jesse Graham","Jesse Lopez","Jesse M. Shapiro","Jesse Prinz","Jesse Rothstein","Jesse Walker",
  "Jessica Cohen","Jessica F. Cantlon","Jessica Gurevitch","Jessica Huang","Jessica Hullman","Jessica Lucas","Jessica Mathews",
  "Jessica Pan","Jessica Silver-Greenberg","Jessica Taylor","Jessica Tyler","Jessica Wright","Jesus Rios","Jes\250s Gil",
  "Ji Qi","Jia Li","Jia Liu","Jia Xu","Jia Zheng","Jian Cui","Jian Tang",
  "Jian Zeng","Jian Zhou","Jiang Li","Jiang Wang","Jiang Yang","Jianhua Lu","Jianjun Cheng",
  "Jianqing Fan","Jianshu Li","Jiawei Han","Jiawei Shen","Jiaya Jia","Jie Fang","Jie Lu",
  "Jie Tang","Jie Wu","Jie Yang","Jie Yao","Jie Zheng","Jiebo Luo","Jiewen Zhang",
  "Jill M. Hooley","Jillian F. Banfield","Jillian Fisher","Jim Cherry","Jim Hoge","Jim Steinman","Jim van Os",
  "Jimmy Lin","Jimmy Liu","Jimmy Maher","Jimmy Wei","Jin Li","Jin Yang","Jin Yuan",
  "Jinchao Xu","Jing Chang","Jing He","Jing Luo","Jing Ma","Jing Sun","Jing Tian",
  "Jing Wen","Jingjing Wang","Jinyu Li","Jiong Zhu","Jiro Nakamura","Jitendra Malik","Jiwei Zhao",
  "JoAnn E. Manson","Joan Bulman","Joan Capdevila","Joan M. Redwing","Joan McCord","Joan Sabat\233","Joanna M. Wardlaw",
  "Joanna Masel","Joanna Wysocka","Joanna Zajac","Joanne Cole","Joao Pedro de Magalhaes","Joaquim Radua","Joby Harris",
  "Jochen Seitz","Jodi White","Jodie Ingles","Joe Alessi","Joe Boyd","Joe Penna","Joe Z. Tsien",
  "Joel Bernstein","Joel Best","Joel David Hamkins","Joel Gelernter","Joel Hirschhorn","Joel Kramer","Joel Mokyr",
  "Joel N. Hirschhorn","Joel Paris","Joel S. Demski","Joel S. Schuman","Joel Schwartz","Joel Simon","Joel Spolsky",
  "Joel Waldfogel","Joelle Pineau","Joerg Zimmermann","Joeri Rogelj","Jofish Kaye","Johan Bollen","Johan Coenen",
  "Johan Jakobsson","Johan Paulsson","Johan Sundstrom","Johan Sundstr\246m","Johan Wagemans","Johannes Fischer","Johannes Haushofer",
  "Johannes Kepler","Johannes Kornhuber","Johannes Krause","Johannes Michalak","Johannes Smit","Johannes Wagner","John A. Bargh",
  "John A. Carpenter","John A. Clausen","John A. Hartigan","John A. Jane","John A. List","John A. Morris","John A. Pickett",
  "John A. Rogers","John A. Wagner","John Atherton","John B. Calhoun","John B. Carroll","John B. Gibson","John B. Harley",
  "John B. Hogenesch","John B. Watson","John Barnard","John Barrington [Ian Stewart]","John Blangero","John Bohannon","John Bound",
  "John Burden","John C. Baez","John C. Crabbe","John C. DeFries","John C. Loehlin","John C. Whittaker","John Canny",
  "John Carmack","John Cheng","John Christodoulou","John D. Mayer","John D. Storey","John Danesh","John DeLuca",
  "John Dean","John Deanfield","John Downing","John Draper","John E. Anderson","John E. Burke","John E. Hayes",
  "John E. Hopcroft","John E. Hunter","John E. J. Rasko","John E. Murray","John E. Warnock","John Etchemendy","John F. Cryan",
  "John Fang","John G. Gager","John G. Roberts","John Geanakoplos","John H. Conway","John H. Dessauer","John H. Holland",
  "John H. R. Maunsell","John Haltiwanger","John Harrison","John Hopcroft","John Hopfield","John Horwood","John Huddleston",
  "John Ioannidis","John J. Crowley","John J. Donovan","John J. Ely","John J. Farrell","John J. McArdle","John J. McGrath",
  "John J. Walsh","John Jeremiah Sullivan","John Jonides","John K. Hewitt","John K. Kruschke","John Kemp","John Kruschke",
  "John L. Fuller","John L. Gustafson","John L. Hennessy","John Ledyard","John M. Barry","John M. Davis","John M. Shelton",
  "John M. Wells","John Markoff","John Marshall","John Massie","John Maynard Keynes","John Maynard Smith","John McLean",
  "John McPhee","John McWhorter","John Money","John Mueller","John Novembre","John Nuckolls","John Obert Voll",
  "John O\8217Mahony","John P. A. Ioannidis","John P. Campbell","John P. Donnelly","John P. Perdew","John Paul Scott","John Paul Wright",
  "John Phan","John Preskill","John Q. Trojanowski","John Quan","John R. Alford","John R. Conway","John R. Hibbing",
  "John R. Hughes","John R. Kramer","John R. Lott","John R. Pierce","John R. Platt","John R. Pringle","John R. Thompson",
  "John R. White","John Rader Platt","John Resig","John Rettie","John Reynders","John Roberts","John Rust",
  "John S. Werner","John Seabrook","John Shawe-Taylor","John Sievenpiper","John Steakley","John Strauss","John Sudworth",
  "John T. Cacioppo","John Tooby","John Tran","John V. Guttag","John Van Reenen","John W. Campbell","John W. Emerson",
  "John W. Tukey","John Wawrzynek","John Wieting","John Zaller","John von Neumann","John-Dylan Haynes","Johnny Ryan",
  "Jon Borchardt","Jon Crowcroft","Jon F. Merz","Jon Green","Jon H. Kaas","Jon Kleinberg","Jon Porter",
  "Jon Seger","Jon Steinberg","Jonah Berger","Jonah Lehrer","Jonah Rockoff","Jonas F. Ludvigsson","Jonas Pettersson",
  "Jonathan A. Coddington","Jonathan A. Eisen","Jonathan Allen","Jonathan Balcombe","Jonathan Browne","Jonathan Chang","Jonathan Cook",
  "Jonathan D. Cohen","Jonathan Friedman","Jonathan Gottschall","Jonathan Haidt","Jonathan Hartley","Jonathan Hewitt","Jonathan Hoefler",
  "Jonathan Horowitz","Jonathan K. Pritchard","Jonathan Koomey","Jonathan L. Zittrain","Jonathan Larson","Jonathan Ling","Jonathan M. Borwein",
  "Jonathan Marchini","Jonathan May","Jonathan Mayer","Jonathan O\8217Callaghan","Jonathan Rees","Jonathan Rosand","Jonathan Schooler",
  "Jonathan Stevens","Jonathan Tremblay","Jonathan Zinman","Joop Hartog","Jordan Anderson","Jordan Bimm","Jordan Ellenberg",
  "Jordan Pollack","Jordan Smoller","Jordan Todorov","Jordan W. Smoller","Jordi Bur\233s","Jordi Cam\237","Jordi Torres",
  "Jorge Barros","Jorge L. Contreras","Jorge Leite","Jorge Luis Borges","Jorge Nocedal","Jorge Rocha","Jorge Vega",
  "Jose Antonio","Josef Coresh","Josef Parnas","Josef Priller","Josef Urban","Josep Call","Joseph A. Konstan",
  "Joseph Agassi","Joseph Alsop","Joseph B. Kadane","Joseph Bernstein","Joseph Biederman","Joseph Boden","Joseph E. Parisi",
  "Joseph F. Quinn","Joseph Felsenstein","Joseph Firth","Joseph Hellerstein","Joseph Henrich","Joseph L. DeRisi","Joseph L. Fleiss",
  "Joseph Lau","Joseph Lee Rodgers","Joseph Loscalzo","Joseph M. Baker","Joseph M. Horn","Joseph Pemberton","Joseph R. Ecker",
  "Joseph Rotblat","Joseph Schmidt","Joseph Uscinski","Joseph W. Kable","Joseph Wang","Joseph Wong","Joseph Zubin",
  "Joseph de la Vega","Josephine Ball","Josh Abramson","Josh Arnold","Josh Bongard","Josh Hodge","Josh Randall",
  "Josh Tobin","Joshua Angrist","Joshua Aronson","Joshua C. Denny","Joshua D. Greene","Joshua D. Rauh","Joshua Guerrero",
  "Joshua Hicks","Joshua John","Joshua M. Pearce","Joshua M. Tybur","Joshua Rauh","Joshua Schmidt","Joshua T. Vogelstein",
  "Joshua Tenenbaum","Joshua Tybur","Jos\233 Galindo","Jos\233 Maldonado","Jos\233 Mar\237a Berm\250dez de Castro","Jos\233 Onuchic","Jos\233 Scheinkman",
  "Jos\233-Alain Sahel","Jos\233e Dupuis","Joy Milne","Joy Wang","Joyce Carol Oates","Joyce Lee","Jozef Gecz",
  "Jo\227o Pedro de Magalh\227es","Jo\227o Sabino","Jo\227o Sacramento","Ju Hu","Ju Li","Juan Botella","Juan Burgue\241o",
  "Juan Carlos Izpisua Belmonte","Juan Carlos Tapia","Juan Frias","Juan Mac\237as","Juan Pino","Judah Folkman","Judith Campisi",
  "Judith Dunn","Judith N. Shklar","Judy Hoffman","Jue Wang","Juergen Schmidhuber","Jugal Kalita","Jules White",
  "Julia Hippisley-Cox","Julia Marshall","Julian Assange","Julian Borger","Julian C. Stanley","Julian Koch","Julian Parkhill",
  "Julian Peto","Julian Roth","Julian Savulescu","Julian Stanley","Julian Togelius","Juliana Schroeder","Julianna Pacheco",
  "Julie Beaulieu","Julie Cunningham","Julie E. Buring","Julie Gibson","Julie Hecht","Julie Jordan","Julie Larsen",
  "Julie Williams","Julio J. Rotemberg","Julio Rodr\237guez","Julius Manger","Julius Popp","Jun Hu","Jun Liu",
  "Jun S. Liu","Jun Wada","Jun Xie","Jun Xu","Jun Yu","June Gruber","Junyi Li",
  "Jure Leskovec","Jurgen Del-Favero","Jurgen Schmidhuber","Justin Chang","Justin Kruger","Justin Lin","Justin Nelson",
  "Justin Wagner","Justin Yifu Lin","Justine Moore","Justine Musk","Jyoti Mishra","J\225nos Kert\233sz","J\225nos Koll\225r",
  "J\225nos Kram\225r","J\243n Steinsson","J\248rgen M\248ller","J\248rn Ratts\248","J\252rgen Glas","J\252rgen Maier","J\252rgen Schmidhuber",
  "J\252ri Allik","J\252ri Parik","K. Anders Ericsson","K. C. Paul","K. Christopher Garcia","K. Eric Drexler","K. Paige Harden",
  "K. Patricia Cross","K. T. Compton","K. V. Mardia","Kaare Christensen","Kah Kay Sung","Kai Chen","Kai Zen",
  "Kai-Fu Lee","Kai-Uwe Hinrichs","Kaiming He","Kaiping Zheng","Kamal Gupta","Kameshwar Prasad","Kang Zhang",
  "Kang Zhao","Kannan Srinivasan","Kaoru Ito","Kara Swisher","Karan Goel","Karanjeet Singh","Karen A. Cerulo",
  "Karen C. Johnson","Karen Carr","Karen F. Berman","Karen Hao","Karen Horney","Karen L. Mohlke","Karen Oegema",
  "Karen Simonyan","Karestan C. Koenen","Karestan Koenen","Kari Stefansson","Karim R. Lakhani","Karin Broberg","Karin Meyer",
  "Karin Strauss","Karl Friston","Karl J. Friston","Karl J. Holzinger","Karl Pearson","Karl Popper","Karl Sims",
  "Karl Zilles","Karla Miller","Karsten M\252ller","Karthik Muralidharan","Karthik Raman","Katalin Karik\243","Katalin Susztak",
  "Kate Devlin","Kate Moran","Kate Tchanturia","Kate Tilling","Katelyn Brown","Katerina Douka","Kath Smith",
  "Katherine A. Rawson","Katherine Baicker","Katherine Baker","Katherine Belov","Katherine Harvey","Katherine Kirk","Katherine L. Milkman",
  "Katherine Martin","Katherine S. Brehme","Katherine S. Pollard","Kathleen D. Vohs","Kathleen Kenealy","Kathleen McGarry","Kathleen Merikangas",
  "Kathleen Mullan Harris","Kathryn Fox","Kathryn Graddy","Kathryn North","Kathryn Paige Harden","Kathryn Roeder","Kathryn S. McKinley",
  "Kathryn T. Hall","Kathryn Walker","Kathy Niakan","Kathy Yelick","Katia Bertoldi","Katia Sycara","Katie A. McLaughlin",
  "Katie Collins","Katja Grace","Katja Hofmann","Katrin Amunts","Katrin Davidsdottir","Katrina A. B. Goddard","Katsuya Takahashi",
  "Kaushik Roy","Kavita Bala","Kay Amert","Kay Redfield Jamison","Kay Tee Khaw","Kay-Tee Khaw","Kay-Yut Chen",
  "Kazuhiko Yamamoto","Kazuo Hara","Kazuo Hasegawa","Kazuya Tsurumaki","Ke Wang","Ke Yuan","Keely Shaw",
  "Kei Aoyama","Kei Ito","Keiji Tanaka","Keith Anderson","Keith Baverstock","Keith Bradsher","Keith Dobney",
  "Keith E. Whitfield","Keith F. Otterbein","Keith Humphreys","Keith J. Holyoak","Keith Maddox","Keith Payne","Keith Stevens",
  "Kelly Harrington","Kelly Parker","Kelsey Smith","Ken A. Paller","Ken Goldberg","Ken Kato","Ken Mackie",
  "Ken Nakayama","Ken Norman","Ken Randall","Ken Rinaldo","Ken Silverstein","Ken Suzuki","Kenichi Tanaka",
  "Kenji Kawaguchi","Kenji Kobayashi","Kenji Wakai","Kenneth A. Dodge","Kenneth B. Clark","Kenneth E. Boulding","Kenneth E. Stager",
  "Kenneth G. Libbrecht","Kenneth J. Arrow","Kenneth J. Gergen","Kenneth Kendler","Kenneth L. Davis","Kenneth Offit","Kenneth S. Kendler",
  "Kenneth Silverman","Kenneth Stanley","Kent C. Berridge","Kent M. Pitman","Kent Taylor","Kentaro Wada","Kerstin Lindblad-Toh",
  "Kevin Blackistone","Kevin Bowyer","Kevin Buzzard","Kevin Chen","Kevin Cheung","Kevin Davies","Kevin Doyle",
  "Kevin Fu","Kevin G. Lynch","Kevin Hartman","Kevin Holden","Kevin Kelly","Kevin Lee","Kevin Leyton-Brown",
  "Kevin Lin","Kevin M. Beaver","Kevin M. Esvelt","Kevin M. Murphy","Kevin N. Laland","Kevin Patrick","Kevin Perez",
  "Kevin Roose","Kevin Roth","Kevin Sharp","Kevin Smith","Kevin Stone","Kevin Tran","Kevin Waugh",
  "Kevin Xu","Kevin Zhang","Khaled Said","Khanh Nguyen","Kieran Healy","Kieran O\8217Neill","Kim Rose",
  "Kingsley Davis","Kingsley Wong","Kiran Musunuru","Kirby Deater-Deckard","Kirsten Grind","Kiyoharu Aizawa","Klaus M. Schmidt",
  "Klaus Stark","Klaus-Peter Lesch","Klaus-Robert M\252ller","Knut Schmidt-Nielsen","Koji Ishii","Konrad Kording","Konrad K\246rding",
  "Konstantinos Panagiotou","Kostas Daniilidis","Koushik Sen","Krishna Chatterjee","Krishna Mohan","Krishna Shenoy","Krishna V. Shenoy",
  "Krishnamachari Srinivasan","Krishnan Srinivasan","Krishnendu Chatterjee","Krista Fischer","Kristen Grauman","Kristen Kelly","Kristen Olson",
  "Kristian Kersting","Kristian Tambs","Kristine Beate Walhovd","Kristine Yaffe","Kristofer S. J. Pister","Kristy Choi","Kristy Lee",
  "Krste Asanovic","Kun Zhou","Kunihiko Ikuhara","Kunle Olukotun","Kuo-En Chang","Kurt Benson","Kurt Goldstein",
  "Kurt G\246del","Kurt Keutzer","Kurt Lewin","Kushal Shah","Kwanghee Lee","Kyle Bagwell","Kyle Carlson",
  "Kyle Cranmer","Kyle Gibson","Kyle Julian","Kyle McDonald","Kyle Myers","Kyle Richardson","K\229re Berg",
  "L. A. Levin","L. Adrienne Cupples","L. C. Knights","L. L. Larmore","Ladan Shams","Ladislau B\246l\246ni","Ladislav Mnacko",
  "Lajos Balint","Lalji Singh","Lan Yan","Lan Zhang","Lant Pritchett","Larissa MacFarquhar","Larry F. Abbott",
  "Larry J. Seidman","Larry Lindsey","Larry Page","Larry Rudolph","Larry S. Davis","Larry Shepp","Larry Shiner",
  "Larry Thompson","Lars Arge","Lars Bergman","Lars B\228ckman","Lars Chittka","Lars Gustafsson","Lars Klareskog",
  "Lars Nyberg","Lars Penke","Lars Smith","Lars Vatten","Lars Wallentin","Lars-G\246ran Nilsson","Laura Arrillaga-Andreessen",
  "Laura Crane","Laura D. Kubzansky","Laura Kelly","Laura Kubzansky","Laura L. Carstensen","Laura Morelli","Laura Owen",
  "Laura Perin","Laura Spinney","Laura Starks","Lauren B. Alloy","Lauren Collins","Lauren Kennedy","Lauren Meyers",
  "Lauren Scott","Laurence Henry Tribe","Laurence Myers","Laurence R. Iannaccone","Laurence Steinberg","Laurent Itti","Laurent Keller",
  "Laurent Mottron","Lauri Bonacorsi","Lawrence B. Schook","Lawrence Badash","Lawrence H. Keeley","Lawrence H. White","Lawrence K. Altman",
  "Lawrence M. Lidsky","Lawrence Page","Lawrence Paulson","Lawrence Person","Lawrence S. Phillips","Lawrence Weschler","Lawrence Wright",
  "Lawrence Zhang","Le Jin","Leah Boustan","Leah Stokes","Leanne M. Williams","Leda Cosmides","Lee A. Thompson",
  "Lee Altenberg","Lee Ehrman","Lee Harrison","Lee J. Cronbach","Lee Jussim","Lee Kuan Yew","Lee Rainwater",
  "Leela Chess Zero","Leena Peltonen","Lei Chen","Lei Gao","Lei Han","Lei Ma","Lei Sun",
  "Lei Xu","Lei Yang","Leif Edward Ottesen Kennair","Len Shustek","Lenore Jacobson","Leo Breiman","Leo White",
  "Leon Bottou","Leon J. Kamin","Leon O. Chua","Leona E. Tyler","Leonard Bernstein","Leonard Carmichael","Leonard J. Savage",
  "Leonard Neidorf","Leonard Uhr","Leonid A. Gavrilov","Leonid A. Levin","Leonid Kruglyak","Leonid Sigal","Leonidas Guibas",
  "Leroy Cronin","Lesley Hoyles","Leslie A. Lyons","Leslie Ann Goldberg","Leslie Bernstein","Leslie C. Aiello","Leslie D. Leve",
  "Leslie Ford","Leslie Fritz","Leslie Jamison","Leslie Lamport","Leslie Pack Kaelbling","Leslie Robinson","Lester Luborsky",
  "Lester Mackey","Lev B. Levitin","Lev Vaidman","Lewis E. Braverman","Lewis Hyde","Lewis M. Terman","Lewis Mitchell",
  "Lewis R. Goldberg","Lewis Stevens","Lex Fridman","Lexing Ying","Leysia Palen","Li Cai","Li Ding",
  "Li Dong","Li Du","Li Fei-Fei","Li Huang","Li Lian","Li Shen","Li Xing",
  "Li Yuan","Li Zhao","Liam Wright","Liang Xu","Lianne de Vries","Lichao Wang","Lifan Yuan",
  "Lila Gleitman","Lillian Lee","Lily James","Lin Lin","Lin Miao","Lin Sun","Lin Tian",
  "Linda Goodman","Linda Levi","Linda Partridge","Linda S. Gottfredson","Linda Wolfe","Lindon Eaves","Lindsay Allen",
  "Lindsey A. Criswell","Ling Yang","Lingling Song","Lionel Barnett","Lior Pachter","Liqun Luo","Liran Carmel",
  "Lisa Baird","Lisa Bero","Lisa Bradley","Lisa Feldman Barrett","Lisa Jones","Lisa Lee","Lisa Mirabello",
  "Lisa Schut","Lisa Tauxe","Lisa Wang","Lisa Wood","Lise Eliot","Liu Binyan","Liu Bo",
  "Livia Puljak","Lloyd G. Humphreys","Lluis Quintana-Murci","Llu\237s Quintana-Murci","Logan Smith","Lon Cardon","Lon R. Cardon",
  "Long Now Foundation","Long Zhu","Lora Aroyo","Lord Bowden","Loren Eiseley","Lori Shapiro","Lorin Crawford",
  "Lorne L. Dawson","Lorrie Cranor","Lotte Hedeager","Louis Bouvier","Louis D. Matzel","Louis Faury","Louis Fox",
  "Louis Guttman","Louis J. Muglia","Louis J. Ptacek","Louis L. Thurstone","Louis Leon Thurstone","Louis Thiry","Louis-Philippe Morency",
  "Louise Arseneault","Louise Fraser","Louise Ross","Louise Ryan","Louise Sharpe","Louise Slade","Louise Wilson",
  "Lowell Wood","Lu Cheng","Lu Hou","Lu Jiang","Lu Sheng","Lu Xie","Lu Xu",
  "Lu Xun","Lu Yu","Lubomir Feldek","Luca Biggio","Luca Cardelli","Luca Cecchetti","Luca Maria Gambardella",
  "Lucas Baker","Lucas Chancel","Luciano Floridi","Lucy Blake","Lucy Cooke","Lucy Harrison","Lucy Jenkins",
  "Lucy van Dorp","Ludger Woessmann","Ludovic Auger","Ludwig Schmidt","Luis Aguiar","Luis Ceze","Luis Guzman",
  "Luis Villa","Luis W. Alvarez","Lukas Haas","Luke Bates","Luke Hunter","Luke McNally","Luke Miller",
  "Luke Muehlhauser","Luke O\8217Connor","Lutz J\228ncke","Luyang Liu","Lyle F. Schoenfeldt","Lyle H. Ungar","Lyle Ungar",
  "Lyn R. Griffiths","Lynn B. Jorde","Lynn DeLisi","Lynn E. DeLisi","Lynn Etheridge Davis","Lynn Hasher","Lynn M. LoPucki",
  "Lynn R. Goldman","Lynn Vavreck","L\233on Bottou","M. Brent Donnellan","M. Brewster Smith","M. C. Bradbrook","M. Csikszentmihalyi",
  "M. Frans Kaashoek","M. Maria Glymour","M. Thomas P. Gilbert","M. Todd Henderson","Mac King","Maciej Ceglowski","Maciej Henneberg",
  "Madeleine Thompson","Madhu Khanna","Madison Bentley","Mads Melbye","Mae Jemison","Magdalena Zernicka-Goetz","Magnus Johannesson",
  "Magnus Nordborg","Magnus Rasmussen","Magnus Tideman","Mahzarin R. Banaji","Maija Hassinen","Maiken Nedergaard","Makoto Hirata",
  "Mala Murthy","Malcolm Reynolds","Mamoru Oshii","Man Li","Manasi Pradhan","Maneesh Agrawala","Manfred K. Warmuth",
  "Manfred Milinski","Manik Varma","Manje Gowda","Manlio Vinciguerra","Mannat Singh","Manoj Kumar","Manolis Kellis",
  "Manu Sharma","Manuel Blum","Manuel Castro","Manuel Moyano","Manuel Rivas","Manuel Serrano","Manuela Veloso",
  "Manvendra Singh","Mara Mather","Marc A. Suchard","Marc Demers","Marc H. Bornstein","Marc Hauser","Marc Lipsitch",
  "Marc N. Potenza","Marc Sageman","Marc Sangnier","Marc Tischkowitz","Marcelo O. Magnasco","Marcia K. Johnson","Marcia Ory",
  "Marcin Michalski","Marco Avellaneda","Marco Baroni","Marco Battaglia","Marco Costa","Marco Fabbri","Marcos Miranda",
  "Marcus Hutter","Marcus Munaf\242","Margaret A. Meyer","Margaret A. Pericak-Vance","Margaret A. Tucker","Margaret Burchinal","Margaret Gatz",
  "Margaret J. Snowling","Margaret Keyes","Margaret Mitchell","Margaret Rhee","Margaret Rosario","Margaret S. Livingstone","Margaret Sutherland",
  "Margarita Guerrero","Margie E. Lachman","Margit Osterloh","Mari Ostendorf","Maria Grande","Maria Grazia Roncarolo","Maria Katz",
  "Maria Keil","Maria Lucia Yanguas","Maria M. Klawe","Maria Martinez","Maria Morena","Maria Nordbrandt","Maria Vlachou",
  "Maria-Florina Balcan","Marian Croak","Marian Knight","Marian Neuhouser","Marianna Fontana","Marianne Bertrand","Marianne Schmid Mast",
  "Marianne Simmel","Marie Phillips","Marie-Laure Ryan","Marilyn Jager Adams","Marilyn Strathern","Marilynn B. Brewer","Marina Butovskaya",
  "Mario Ivankovic","Mario Lu\269i\263","Mario Maj","Mario Szegedy","Marion Leboyer","Marion Roberts","Marios Papaefthymiou",
  "Marius Lindauer","Marius Popescu","Mariusz Lewandowski","Mariza de Andrade","Marjo-Riitta J\228rvelin","Marjolein Kriek","Mark A. Davis",
  "Mark A. McDaniel","Mark A. Murphy","Mark Aldrich","Mark Braverman","Mark Caulfield","Mark Changizi","Mark Chen",
  "Mark D. McDonnell","Mark D. Shriver","Mark D. West","Mark Frye","Mark Gerstein","Mark Girolami","Mark Granovetter",
  "Mark Greene","Mark Horowitz","Mark I. McCarthy","Mark J. Daly","Mark Jenkinson","Mark Keil","Mark Kelly",
  "Mark Kirkpatrick","Mark Lathrop","Mark Lyons","Mark Mazzetti","Mark McDonnell","Mark Miodownik","Mark Monahan",
  "Mark Moss","Mark Neumann","Mark P. Mattson","Mark P. Taylor","Mark Parsons","Mark Phillips","Mark Rowland",
  "Mark Russinovich","Mark S. Allen","Mark S. Blumberg","Mark Sanderson","Mark Schaller","Mark Schmidt","Mark Seal",
  "Mark Skidmore","Mark Smyth","Mark Stoneking","Mark T. Greenberg","Mark Tester","Mark Tushnet","Mark Twain",
  "Mark Warschauer","Mark Weiser","Markku Laakso","Markus Grompe","Markus Gross","Markus Reitzig","Markus Scholz",
  "Marlene Behrmann","Marshall Burke","Marta Costa","Marta Kwiatkowska","Marta Sanchez","Marta Zlatic","Marten Scheffer",
  "Martha Cannon","Martha Clare Morris","Martha Mendoza","Martha White","Martie G. Haselton","Martin A. Nowak","Martin Brunner",
  "Martin E. P. Seligman","Martin Elliott","Martin Ellison","Martin Feldstein","Martin Gardner","Martin Hautzinger","Martin Herrmann",
  "Martin Hinton","Martin Hrab\283 de Angelis","Martin J. Klein","Martin Kampmann","Martin Karafiat","Martin L. West","Martin L. Yarmush",
  "Martin Lucas","Martin Lundberg","Martin McKee","Martin Petr","Martin Picard","Martin Pollard","Martin Prince",
  "Martin Samuels","Martin Schalling","Martin Scherer","Martin Schmid","Martin Schneider","Martin Shkreli","Martin Steinegger",
  "Martin Vechev","Martin Voracek","Martin Waldseem\252ller","Marvin D. Dunnette","Mary Ann Glynn","Mary Cannon","Mary Cushman",
  "Mary Czerwinski","Mary D\8217Alton","Mary Evelyn Tucker","Mary G. Dietz","Mary Isaacson","Mary K. Rothbart","Mary L. Marazita",
  "Mary Lee Smith","Mary Oliver","Mary Waldron","Mary Whitton","Mary Williamson","Marylyn D. Ritchie","Marylyn Ritchie",
  "Marzena Karpinska","Masaki Kato","Masao Miyazaki","Masaru Tomita","Masashi Yanagisawa","Masataka Sugimoto","Masato Fukushima",
  "Masayo Takahashi","Masayoshi Ito","Massimo Polidoro","Matei Zaharia","Mateusz Malinowski","Matias D. Cattaneo","Matja\382 Perc",
  "Matt Blaze","Matt Gardner","Matt Hoffman","Matt Kaeberlein","Matt McGue","Matt McQueen","Matt Richardson",
  "Matt Sharifi","Matt Simon","Matteo Bianchi","Matteo Carandini","Matteo Fischetti","Matthew A. Brown","Matthew Allison",
  "Matthew Allwood","Matthew Baldwin","Matthew Butterick","Matthew C. Keller","Matthew Cobb","Matthew Connelly","Matthew Cook",
  "Matthew Driscoll","Matthew E. Kahn","Matthew Gentzkow","Matthew Haag","Matthew Hill","Matthew Hurles","Matthew Hutchinson",
  "Matthew J. Hart","Matthew J. Salganik","Matthew K. Nock","Matthew Kimble","Matthew Knight","Matthew Lang","Matthew Lawrence",
  "Matthew McGue","Matthew Meyer","Matthew Nicholson","Matthew O. Jackson","Matthew P. Walker","Matthew S. Johnson","Matthew Waller",
  "Matthew Weeks","Matthew Weinzierl","Matthew Yu","Matthias Doepke","Matthias Egger","Matthias Endres","Matthias Grossglauser",
  "Matthias H. Tsch\246p","Matthias Lange","Matthias Meyer","Matthias Niessner","Matthias Schmid","Matthias Schonlau","Matthias Schwab",
  "Mattia Marchi","Maureen Dowd","Maureen E. Raymo","Maurice Ptito","Mauricio R. Delgado","Mauricio Tohen","Mauro Martino",
  "Max Abrahms","Max Bain","Max Bazerman","Max Norman","Max Pfeiffer","Max Reuter","Max Rose",
  "Max Tegmark","Max Weiss","Max Welling","Max Yates","Maxwell Goldstein","May Chen","Maya Shankar",
  "Maya Yamazaki","Mayank Mishra","Megan Campbell","Megan Cooke","Megan Shanahan","Megan Smith","Megan Twohey",
  "Mehdi Bennani","Mehrdad Abdi","Mehryar Mohri","Mei Li","Meike Bartels","Melanie Davies","Melanie Mitchell",
  "Melanie O\8217Leary","Melanie Walsh","Melinda Mills","Melissa A. Wilson","Melissa Dell","Melissa Hardy","Melvin Johnson",
  "Melvyn B. Nathanson","Menachem Kaiser","Menachem Stern","Menelas N. Pangalos","Meng Jiang","Meng Yang","Mercedes Ruehl",
  "Meredith Ringel Morris","Meredith Yeager","Met Opera","Mette Thomsen","Mi Zhang","Miao Hua","Michael A. Schneider",
  "Michael A. Sutton","Michael A. Taylor","Michael Allaby","Michael B. Bracken","Michael B. Elowitz","Michael B. Fossel","Michael Bang Petersen",
  "Michael Berk","Michael Betancourt","Michael Boehnke","Michael Bolin","Michael Bond","Michael Bowling","Michael Bronstein",
  "Michael Bunce","Michael C. Frank","Michael C. Jensen","Michael C. Neale","Michael C. O\8217Donovan","Michael C. Seto","Michael Cantor",
  "Michael Chang","Michael Chung","Michael Crossley","Michael D. Fox","Michael D. Rugg","Michael Daniels","Michael Deakin",
  "Michael E. Goddard","Michael E. Greenberg","Michael E. Hochberg","Michael Elad","Michael Elowitz","Michael Erard","Michael Ettlinger",
  "Michael Frank","Michael Franti","Michael Goddard","Michael Greenstone","Michael Gschwind","Michael Hahn","Michael Hauser",
  "Michael Hemmingson","Michael Hornberger","Michael Hout","Michael Huemer","Michael H\228usser","Michael I. Jordan","Michael I. Norton",
  "Michael I. Posner","Michael Inzlicht","Michael J. Black","Michael J. Joyner","Michael J. Mina","Michael J. Spivey","Michael J. Tarr",
  "Michael J. Thun","Michael J. Welsh","Michael J. Wilkins","Michael Kandel","Michael Kearney","Michael Knapp","Michael Kremer",
  "Michael Kuhn","Michael Kumhof","Michael L. Littman","Michael L. Tushman","Michael Langan","Michael Lappert","Michael Levitt",
  "Michael Lewis","Michael Li","Michael Littman","Michael Lounsbury","Michael Lynskey","Michael Macy","Michael Maschler",
  "Michael Mathieu","Michael McCloskey","Michael Mendl","Michael Mitzenmacher","Michael Moritz","Michael Moutoussis","Michael Muthukrishna",
  "Michael Neale","Michael Owen","Michael O\8217Boyle","Michael O\8217Donovan","Michael P. Snyder","Michael Pacher","Michael Page",
  "Michael Preuss","Michael Ristow","Michael Roden","Michael Rosenblum","Michael Rubinstein","Michael Rutter","Michael S. Weisbach",
  "Michael Sager","Michael Schulman","Michael Sheetz","Michael Small","Michael Specter","Michael Stokes Paulsen","Michael Stringer",
  "Michael Swanwick","Michael T. Gabbett","Michael T. Hannan","Michael Tomasello","Michael Travisano","Michael Valenzuela","Michael W. Clune",
  "Michael W. Smith","Michael Wertheimer","Michael Wigler","Michael Wimmer","Michael Zhang","Michael von Gr\252nau","Michal Irani",
  "Michal Kosinski","Michel Boivin","Michel Georges","Michel Lang","Michel Poulain","Michele K. Evans","Michele Knobel",
  "Michele Ramsay","Michelle Chan","Michelle Herman","Michelle Kim","Michelle LaRue","Michelle Luciano","Mich\232le Ramsay",
  "Mick Watson","Microsoft Research","Miguel A. L. Nicolelis","Miguel Bernardo","Miguel Calero","Miguel Casas","Miguel Cruz",
  "Miguel Delibes","Miguel Medina","Mihaela van der Schaar","Mihaly Csikszentmihalyi","Miia Kivipelto","Mike Burrows","Mike Darwin",
  "Mike Friedman","Mike Jay","Mike Markkula","Mike Paterson","Mike Stoolmiller","Mike Tomkies","Mikel Artetxe",
  "Mikhail Sablin","Miki Inoue","Mikkel Thorup","Mila Haugova","Milan M. \262irkovi\263","Milan Rufus","Milan Vojnovic",
  "Miles A. Tinker","Miles Hewstone","Miles Turpin","Milo Urban","Milos R. Popovic","Milton Diamond","Min Gu",
  "Min Sun","Min Xu","Min Yang","Min Zhuo","Ming C. Lin","Ming Li","Ming T. Tsuang",
  "Ming Tsuang","Ming Yan","Mingxuan Wang","Mingyan Liu","Minh Nguyen","Mira Murati","Mirella Lapata",
  "Miron Zuckerman","Miroslav V\225lek","Mitchell J. Nathan","Mohamed Khamis","Mohammad Azam Khan","Mohammad Ishaq","Mohammad Norouzi",
  "Mohammad Saleh","Mohammed Fazil","Mohammed Madouh","Mohiro Kitoh","Molly Crockett","Molly Przeworski","Mona Diab",
  "Mona Lynch","Moni Naor","Monica Driscoll","Monica Morell","Monica S. Lam","Monique M. B. Breteler","Montgomery Slatkin",
  "Montserrat Garc\237a-Closas","Mor Naaman","Morgan Conway","Morgan Kelly","Morgan Stanley","Morris H. DeGroot","Morris Moscovitch",
  "Morten Johansen","Morten Olsen","Morten Sorensen","Morton H. Halperin","Moses Charikar","Mosharaf Chowdhury","Moshe Szyf",
  "Mostafa Abdou","Moussa Doumbouya","Moustapha Cisse","Moxie Marlinspike","Mrinal Kalakrishnan","Muhammad Ghous","Muhammad Shahzad",
  "Muin J. Khoury","Munir Pirmohamed","Murielle Bochud","Murray Griffin","Murray Shanahan","Mustafa Suleyman","Myrna M. Weissman",
  "N. Asokan","N. Graham","N. Gregory Mankiw","Na Hu","Na Luo","Nachoem Wijnberg","Naftali Tishby",
  "Nakul Verma","Nambury S. Raju","Nan Laird","Nan Lin","Nancy A. Moran","Nancy Adler","Nancy Bayley",
  "Nancy C. Andreasen","Nancy Fulda","Nancy J. Cox","Nancy Kanwisher","Nancy L. Pedersen","Nancy L. Segal","Nancy Pedersen",
  "Nancy Qian","Nancy Wang","Nando de Freitas","Naoki Murata","Naoki Sato","Naomi Habib","Naomi Matsumoto",
  "Naomi R. Wray","Naomi Wray","Naoya Takahashi","Nat Friedman","Natalia Ponomareva","Natasha Vita-More","Nathan A. Fox",
  "Nathan Brody","Nathan Lambert","Nathan W. Pyle","Nathan Ward","Nathaniel David","Nathaniel Roberts","Nathaniel Weyl",
  "National Academies of Sciences","Naveed Sattar","Navjot Kaur","Navjot Singh","Neal L. Benowitz","Neal Stephenson","Neale Mahoney",
  "Neil Bailey","Neil Hartman","Neil Hunt","Neil Malhotra","Neil Perry","Neil Risch","Neil Robertson",
  "Neil Sloane","Neil Small","Neil Strauss","Neil Thompson","Neil Weste","Nell Greenfieldboyce","Nelson Cowan",
  "Nelson Goodman","Nenad Sestan","New York Times","Nia Imara","Nicholas A. Christakis","Nicholas Bloom","Nicholas Christakis",
  "Nicholas Evans","Nicholas Fernando","Nicholas G. Martin","Nicholas Graham","Nicholas Gray","Nicholas H. Barton","Nicholas Hamilton",
  "Nicholas Humphrey","Nicholas J. Wareham","Nicholas J. White","Nicholas Johnson","Nicholas King","Nicholas Nicastro","Nicholas Padilla",
  "Nicholas Polson","Nicholas Schwab","Nicholas Trainor","Nicholas Wareham","Nicholas Wheeler","Nick Barton","Nick Black",
  "Nick Chater","Nick Craddock","Nick Fortugno","Nick Montfort","Nick Moran","Nick Morgan","Nick Ryder",
  "Nick Walton","Nicky Best","Nicola Clayton","Nicola Persico","Nicolai Rubinstein","Nicolas Perrin","Nicole Boivin",
  "Nicole Graf","Nicole Lazar","Nicole Pratt","Nicole Schupf","Nicole Soranzo","Nicol\225s Herranz","Nidhi Shah",
  "Nigel Paneth","Nigel R. Franks","Nigel Slater","Nigel Stocks","Nikhil Ghosh","Nikhil Gupta","Nikhil Naik",
  "Nikhil Tandon","Niklaus Wirth","Nikolai Fyodorovich Fyodorov","Nikolaos Makris","Nikolaos Papanikolopoulos","Nikolaos Pappas","Nikolaus Rajewsky",
  "Nikolay Karpov","Nilanjan Chatterjee","Nilay Patel","Nils Henriksson","Nils Lid Hjort","Nina G. Jablonski","Nina Gold",
  "Nina Singh","Nina Wedell","Ning Ding","Ning Lu","Ning Zhang","Nir Baram","Nir Barzilai",
  "Nir Eyal","Nir Friedman","Nir Levine","Nir Shavit","Niren Murthy","Nita G. Forouhi","Noah A. Rosenberg",
  "Noah Carl","Noah Fierer","Noah Jones","Nobuo Sasaki","Nobuyuki Sato","Nolan Miller","Nora D. Volkow",
  "Nora S. Newcombe","Norbert Benecke","Norbert Schwarz","Norbert Wiener","Norman F. Maier","Norman Hammond","Norman Hardy",
  "Norman Jouppi","Norman McQuown","Norman P. Jouppi","Norman R. F. Maier","Norman Sartorius","Norman Thomas di Giovanni","Norwood Russell Hanson",
  "Novo Nordisk","No\233mie Elhadad","Nupur Lala","Nuria Oliver","Nutrition Reviews","Ofer Dekel","Oh You Pretty Things",
  "Ola Hansson","Olaf Blanke","Olaf Sporns","Olav Dalgard","Olav Sorenson","Ole Andreassen","Ole Henrik Magga",
  "Oleg Sushkov","Oleksandr Zinenko","Olga Russakovsky","Oliver Grimm","Oliver Heaviside","Oliver Lange","Oliver Martinez",
  "Oliver Mason","Oliver Zhang","Olivia Johnson","Olivier Fran\231ois","Olivier Klein","Olle H\228ggstr\246m","Olof Johansson",
  "Olufunmilayo I. Olopade","Omar Agha","Omar Cortes","Omar Wasow","Onur G\252nt\252rk\252n","Open Science Collaboration","Ophelia Deroy",
  "Ophir Klein","Oren Etzioni","Oriol Vinyals","Orrin Devinsky","Oscar Kempthorne","Oskar van Deventer","Osvald Nitski",
  "Oystein Sorensen","P. A. P. Moran","P. C. Mahalanobis","P. Eline Slagboom","P. N. Suganthan","P22 Type Foundry","Pablo Kuri-Morales",
  "Pablo Villalobos","Pak Chung Sham","Pak Sham","Pallab Ghosh","Palwasha Khan","Pamela A. Silver","Pamela B. Davis",
  "Pamela Herd","Pamela McCorduck","Pamela Sklar","Pan Wei","Pan Zhang","Panayiota Poirazi","Pang Yao",
  "Pankaj Patel","Pankaj Sharma","Panos Deloukas","Paola Giuliano","Paola Sebastiani","Paolo Boffetta","Paolo Fusar-Poli",
  "Paolo Napoletano","Paolo Parisi","Parag Pathak","Paras Jain","Pardis C. Sabeti","Paresh Dandona","Paris Buttfield-Addison",
  "Parker Barnes","Parker Hill","Partha Ghosh","Parthasarathy Ranganathan","Pascal Boyer","Pascal Vincent","Pascale Fung",
  "Pascaline Dupas","Pat Stanley","Patricia A. Reuter-Lorenz","Patricia Coogan","Patricia Devine","Patricia H. Miller","Patricia J. Bauer",
  "Patricia L. Dudley","Patricia M. Clancy","Patricia Michie","Patricia Moore","Patricia T. Michie","Patricio Saavedra","Patrick Baud",
  "Patrick Bolton","Patrick Chan","Patrick Collison","Patrick D. Wall","Patrick Dempsey","Patrick Drury","Patrick E. McGovern",
  "Patrick Esser","Patrick F. Sullivan","Patrick Fournier","Patrick J. Curran","Patrick J. McGrath","Patrick J. Morris","Patrick J. Stover",
  "Patrick Kline","Patrick Kruger","Patrick Leahy","Patrick Lewis","Patrick Markey","Patrick Maynard Stuart Blackett","Patrick McClure",
  "Patrick McDaniel","Patrick McGorry","Patrick McKenzie","Patrick Phillips","Patrick Reed","Patrick Thiran","Patrick Wyatt",
  "Patrik Magnusson","Pattie Maes","Pau Navarro","Paul A. David","Paul A. Johnson","Paul A. Samuelson","Paul B. Baltes",
  "Paul Barham","Paul Christiano","Paul Covington","Paul D. Adams","Paul E. Meehl","Paul E. Peterson","Paul F. Lazarsfeld",
  "Paul Ford","Paul Fussell","Paul G. Kwiat","Paul Glasziou","Paul Graham","Paul Grieco","Paul H. Nitze",
  "Paul Heald","Paul Irwing","Paul J. Heald","Paul J. LeBlanc","Paul Johns","Paul Krugman","Paul L. Harris",
  "Paul M. Ridker","Paul M. Romer","Paul Milgrom","Paul Nyquist","Paul P. Glasziou","Paul R. Jones","Paul Resnick",
  "Paul Rozin","Paul S. Appelbaum","Paul S. Freemont","Paul Samuelson","Paul Sedille","Paul Smolensky","Paul Stamets",
  "Paul Syverson","Paul Thagard","Paul Tholey","Paul Thomas Young","Paul Tough","Paul Veyne","Paul Vitanyi",
  "Paul W. Brand","Paul W. Ewald","Paul W. Franks","Paul Webley","Paul Weindling","Paul Weston","Paul van Geert",
  "Paul van Oorschot","Paula R. Pietromonaco","Paula Smith","Paulien Hogeweg","Pauline Smith","Paulo Shakarian","Pavel A. Pevzner",
  "Pavel Drozd","Pavel Kuznetsov","Pavlos Pavlidis","Peder Mortensen","Pedro Domingos","Pedro Henrique Martins","Pedro Mercado",
  "Pedro Ortega","Pedro Sanchez","Peer Bork","Peggy J. Kleinplatz","Pei-Chun Shih","Peng Chen","Peng Cheng",
  "Peng Lin","Peng Peng","Peng Qi","Peng Shi","Peng Sun","Peng Tee Khaw","Peng Wei",
  "Peng Wu","Peng Zhao","Peng Zhou","Penn State University","Penny Gordon-Larsen","Perdita Barran","Perminder Sachdev",
  "Persi Diaconis","Pete Hatemi","Peter Arcidiacono","Peter B. Medawar","Peter Beattie","Peter C. B. Phillips","Peter C. Fishburn",
  "Peter C. G\248tzsche","Peter Craig","Peter D. Eimas","Peter Dayan","Peter Donnelly","Peter E. Hart","Peter Eisenman",
  "Peter F. Buckley","Peter Franks","Peter Gacs","Peter Godfrey-Smith","Peter Goodhand","Peter Grassberger","Peter H. Rossi",
  "Peter H. Schonemann","Peter Hagoort","Peter Hajek","Peter Hawkins","Peter J. Bentley","Peter J. Boettke","Peter J. Campbell",
  "Peter J. Denning","Peter J. Rentfrow","Peter J. Richerson","Peter J. Wagner","Peter Jaros","Peter K. Gregersen","Peter K. Hatemi",
  "Peter Klatsky","Peter Kun","Peter M. Kogge","Peter M. Lansdorp","Peter M. Neumann","Peter M. Visscher","Peter McGuffin",
  "Peter McLeod","Peter Mooney","Peter N. Devreotes","Peter Naur","Peter Norvig","Peter Nowak","Peter Palese",
  "Peter Pistanek","Peter Propping","Peter Richtarik","Peter Riederer","Peter S. Bearman","Peter Sheridan Dodds","Peter Shor",
  "Peter St George-Hyslop","Peter Stansky","Peter Straub","Peter Szatmari","Peter T. Campbell","Peter T. Fox","Peter T. Leeson",
  "Peter Thiel","Peter Todd","Peter Wade","Peter Watts","Peter West","Peter Winkler","Peter Wonka",
  "Peter Wu","Peter Zajac","Peter de Jonge","Petr Beckmann","Petra Moser","Pew Research Center","Peyman Milanfar",
  "Phil Estes","Phil Lee","Phil Roth","Philip  L. De Jager","Philip A. Vernon","Philip Asherson","Philip Awadalla",
  "Philip Ball","Philip De Jager","Philip E. Tetlock","Philip E. Vernon","Philip Greene","Philip Hazel","Philip J. Davis",
  "Philip K. Maini","Philip Kraft","Philip L. De Jager","Philip Mansfield","Philip Sabin","Philip Scheltens","Philip Tetlock",
  "Philip Winston","Philip Yancey","Philip Zhao","Philipp Kanske","Philipp Koehn","Philipp Seidl","Philippe Autier",
  "Philippe Charlier","Philippe Ciais","Philippe Flajolet","Philippe Froguel","Philippe Kruchten","Phillip A. Sharp","Phyllis Chesler",
  "Pierre Baldi","Pierre Desrochers","Pierre Pinson","Pierre-Simon Laplace","Pierre-Yves Oudeyer","Pieter Abbeel","Pietro De Camilli",
  "Pietro Perona","Pilar Mu\241oz","Pinchas Cohen","Piotr Nawrot","Piotr Stanczyk","Planet Money","Polina Anikeeva",
  "Polina Kuznetsova","Pontiano Kaleebu","Pontus Skoglund","Pony Preservation Project","Popular Science","Poul-Henning Kamp","Prachi Mishra",
  "Pradeep Dubey","Pradeep Sharma","Pranab Bardhan","Prateek Jain","Pratik Chaudhari","Preslav Nakov","Price V. Fishback",
  "Priya Krishna","Przemyslaw Prusinkiewicz","Psychiatric Genomics Consortium","Pushmeet Kohli","Pushpak Bhattacharyya","P\228r Karlsson","Qi Cao",
  "Qi Dong","Qi Guo","Qi Sun","Qi Tian","Qi Zhou","Qi Zhu","Qian Liu",
  "Qian Qin","Qian Tang","Qian Zhang","Qian Zhao","Qiang Du","Qiang Li","Qiang Shen",
  "Qiang Yang","Qiao Hu","Qin Zhang","Qingyun Wang","Quan Gan","Quan Yuan","Quinn McNemar",
  "Quoc V. Le","Quynh Nguyen","R. A. Fisher","R. A. Lafferty","R. A. McConnell","R. A. Radford","R. B. Braithwaite",
  "R. C. Johnson","R. E. Peterson","R. G. Collingwood","R. J. Mical","R. J. Williams","R. Kent Dybvig","R. Knight",
  "R. L. Glass","R. L. Gregory","R. L. Mills","R. L. Plackett","R. M. May","R. M. Murray","R. N. Bracewell",
  "R. Preston McAfee","R. S. Woodworth","R. Scott Bakker","R. Stanley Williams","R. Travis Osborne","R. W. Brockett","R. Watts",
  "Rachel Barney","Rachel Batterham","Rachel Bernstein","Rachel Gordon","Rachel Laudan","Rachel Ward","Rachelle S. Doody",
  "Rachid Guerraoui","Radha Poovendran","Rae Silver","Rafael Salas","Rafael de Cabo","Raffi Khatchadourian","Raghu Rajan",
  "Ragnar Bjarnason","Raheem Beyah","Rahul Desikan","Rahul Gupta","Rahul Joshi","Rahul Khanna","Rahul Sarpeshkar",
  "Raja Mukherjee","Rajeev Motwani","Rajesh P. N. Rao","Rajiv Sharma","Ralph A. Bagnold","Ralph C. Merkle","Ralph Hertwig",
  "Ralph L. Sacco","Ralph Rugoff","Ralph V. L. Hartley","Ralph W. Hull","Ramesh Karri","Ramesh Menon","Ramesh Raskar",
  "Ramesh Sitaraman","Ramon Reyes","Ran Tao","Ran Tian","Ranajit Chakraborty","Randal Burns","Randall Brown",
  "Randall Collins","Randall Munroe","Randall W. Engle","Randi Hutter Epstein","Randy Gomez","Randy L. Buckner","Ranveer Chandra",
  "Raphael Koster","Rapha\235l Li\233geois","Raquel E. Gur","Raquel Fern\225ndez","Raquel Garza","Raquel Gur","Raquel Urtasun",
  "Rasmus Kleis Nielsen","Rasmus Larsen","Raul Gonzalez","Ravi Gupta","Ravi Kalhan","Ravishankar Iyer","Ray Bradbury",
  "Ray Bull","Ray Kurzweil","Ray L. Birdwhistell","Ray Monk","Rayhan Asat","Raymond B. Cattell","Raymond E. Fancher",
  "Raymond F. Jones","Raymond J. Dolan","Raymond J. Mooney","Raymond K. Wong","Raymond Mooney","Raymond Palmer","Raymond S. Nickerson",
  "Raymond Walters","Raza Muhammad","Razib Khan","Rebecca A. Hubbard","Rebecca Andridge","Rebecca D. Jackson","Rebecca Diamond",
  "Rebecca Goldstein","Rebecca Howard","Rebecca Landa","Rebecca Redfern","Rebecca Saxe","Rebecca Shiner","Rebecca Sims",
  "Rebecca Struthers","Rebecca Surman","Rebecca W. Rimel","Rebecca Walsh","Rebecca Willett","Reed Albergotti","Regina Barzilay",
  "Regina G. Ziegler","Regina James","Reid Hoffman","Reiji Suzuki","Reiko Tomii","Reinhard Wilhelm","Reinhardt M\248bjerg Kristensen",
  "Reinhold Schmidt","Reka Nagy","Remi Gribonval","Ren Ng","Rene Hurlemann","Rene J. Dubos","Rene Vidal",
  "Renee Johnson","Ren\233 J. Dubos","Ren\233 S. Kahn","Ren\233 Veenstra","Reuben Hersh","Reuven Gal","Rex Jung",
  "Reza Malekzadeh","Ricardo Palacios","Ricardo Ribeiro","Rich Sutton","Richard A. Friedman","Richard A. Gardner","Richard A. Harshman",
  "Richard A. Jensen","Richard A. Miller","Richard A. Posner","Richard A. Weinberg","Richard Andrew","Richard Baraniuk","Richard Bonneau",
  "Richard Bruggeman","Richard C. Trembath","Richard D. Arvey","Richard Danzig","Richard Delgado","Richard Dobson","Richard Doll",
  "Richard E. Boyatzis","Richard E. Carey","Richard E. Lenski","Richard E. Mayer","Richard E. Nisbett","Richard E. Snow","Richard E. Tremblay",
  "Richard E. Turner","Richard Everett","Richard Feynman","Richard G. Baraniuk","Richard G. Frank","Richard Gibbons","Richard H. Thaler",
  "Richard Hamming","Richard Hanania","Richard Ho","Richard J. Davidson","Richard J. Haier","Richard J. Hatchett","Richard J. Lipton",
  "Richard J. Murnane","Richard J. Rose","Richard J. Samuels","Richard J. Shaw","Richard Jozsa","Richard K. Wilson","Richard Kronick",
  "Richard L. Pyle","Richard L. Solomon","Richard Landes","Richard Lathe","Richard Li","Richard Liu","Richard Lynn",
  "Richard M. Karp","Richard M. Myers","Richard M. Ryan","Richard Maudsley","Richard N. Aslin","Richard N. Holdaway","Richard O. Duda",
  "Richard P. Bentall","Richard P. Ebstein","Richard P. Feynman","Richard P. Lifton","Richard Peto","Richard Rosen","Richard S. Bird",
  "Richard S. Cooper","Richard S. Sutton","Richard Sandford","Richard Schmalensee","Richard T. Scott","Richard Villems","Richard W. Grant",
  "Richard W. Hamming","Richard W. Murray","Richard W. Wrangham","Richard Wiseman","Richard Wrangham","Richard Zeckhauser","Richard Zemel",
  "Richie Poulton","Rick A. Kittles","Rick Dale","Rick Kittles","Rik Vandenberghe","Rishi Sharma","Risto Miikkulainen",
  "Rita Patel","Roald Dahl","Rob Horne","Rob M. van Dam","Rob Powers","Rob Reich","Rob Stein",
  "Robb Willer","Robert A. Baker","Robert A. Baron","Robert A. Bjork","Robert A. Brightman","Robert A. Edwards","Robert A. Gordon",
  "Robert A. Millikan","Robert A. Scott","Robert A. Wild","Robert B. Cialdini","Robert B. Darnell","Robert B. Wallace","Robert Baldwin",
  "Robert Bjork","Robert Brandon","Robert Bringhurst","Robert Brustein","Robert C. Allen","Robert C. Barber","Robert C. Edgar",
  "Robert C. Elston","Robert C. Green","Robert C. Nichols","Robert C. Thompson","Robert C. Wilson","Robert Cardona","Robert Cialdini",
  "Robert Clarke","Robert Colbert","Robert D. Gibbons","Robert Desimone","Robert Doyle","Robert E. Bixby","Robert E. Emery",
  "Robert Earle","Robert F. Krueger","Robert Feldman","Robert Frank","Robert Gaskins","Robert Gerber","Robert Gerlai",
  "Robert Greenfield","Robert H. Brower","Robert Harcourt","Robert Harland","Robert Harry Socolow","Robert Hobbs","Robert Hoffmann",
  "Robert Irvine","Robert J. Aumann","Robert J. Gordon","Robert J. Harvey","Robert J. LaLonde","Robert J. MacCoun","Robert J. Nemiroff",
  "Robert J. Shiller","Robert J. Sternberg","Robert Johnson","Robert Kaestner","Robert Kanigel","Robert Karlsson","Robert Kirby",
  "Robert Kleinberg","Robert Kolker","Robert Krulwich","Robert Kurzban","Robert Kushner","Robert L. Forward","Robert L. Linn",
  "Robert L. Paarlberg","Robert L. Sack","Robert L. Spitzer","Robert L. Thorndike","Robert Lachmann","Robert Ladd Thorndike","Robert Langer",
  "Robert Leeper","Robert Lerner","Robert M. Bond","Robert M. Hauser","Robert M. May","Robert M. Solow","Robert M. Thorndike",
  "Robert M. Yerkes","Robert Maier","Robert Mankoff","Robert Martinson","Robert Meier","Robert Metcalfe","Robert Miles",
  "Robert Mullins","Robert P. Abelson","Robert Plomin","Robert R. Jackson","Robert R. Sears","Robert Ralston","Robert Root-Bernstein",
  "Robert Rudolf","Robert S. Rosenson","Robert S. Woodworth","Robert Schlaifer","Robert Schweitzer","Robert Scoble","Robert Seamans",
  "Robert Slimbach","Robert Stickgold","Robert T. Knight","Robert T. Pennock","Robert Tibshirani","Robert W. Brooks","Robert W. Cox",
  "Robert W. McCarley","Robert W. Williams","Robert Whelan","Robert Wiblin","Robert Wishart","Robert \214stling","Roberta Sinatra",
  "Roberto Azevedo","Roberto Car","Roberto Cipolla","Roberto Colom","Roberto Esposito","Roberto Galbiati","Roberto Mosquera",
  "Roberto Toro","Roberto Trotta","Robin Carhart-Harris","Robin Goldstein","Robin Hanson","Robin Haring","Robin Lovell-Badge",
  "Robin M. Murray","Robin Saikia","Robin Young","Robyn Dawes","Robyn Forbes","Rochelle Buffenstein","Roderic Broadhurst",
  "Rodney L. Lowman","Rodolfo Galindo","Roger B. Myerson","Roger Broughton","Roger C. Schank","Roger Carpenter","Roger D. Cone",
  "Roger G. Barker","Roger Lewin","Roger N. Shepard","Roger R. Schell","Roger T. Hanlon","Roger Wattenhofer","Rohan Gunaratna",
  "Rohan Shah","Rohan Taylor","Rohit Prakash","Rohit Varma","Roland G. Fryer","Roland R. Griffiths","Rolf Dobelli",
  "Roman Kotov","Ron Evans","Ron Milo","Ron Rosenbaum","Ronald A. Fisher","Ronald A. Howard","Ronald C. Kessler",
  "Ronald Davis","Ronald Duncan","Ronald J. Williams","Ronald K. Siegel","Ronald Kessler","Ronald Kim","Ronald Klein",
  "Ronald L. Simons","Ronald Rogowski","Ronald S. Burt","Ronald S. Wilson","Ronald W. Davis","Ronan Farrow","Ronan Harris",
  "Ronen Eldan","Ronen Segev","Rong Zhang","Rong Zhu","Ronin Network","Ronke Olabisi","Rory Collins",
  "Rosalind Picard","Rosalind Raine","Rose Andrew","Rose Eveleth","Roshan Cools","Roshan Rao","Ross Goodwin",
  "Ross J. Baldessarini","Ross L. Prentice","Ross Levine","Ross Taylor","Ross Wightman","Roula Khalaf","Roy Baumeister",
  "Roy F. Baumeister","Roy Fox","Roy H. Campbell","Roy M. Anderson","Roy Pea","Rudolf Arnheim","Rudolf Fehrmann",
  "Rudolf Hanel","Rudolf Uher","Rudolph E. Tanzi","Rudyard Kipling","Rui Costa","Rui Xu","Ruixiang Zhang",
  "Ruma Falk","Rumena Bu\382arovska","Rune Jacobsen","Ruslan Salakhutdinov","Russ Altman","Russel J. Reiter","Russell A. Poldrack",
  "Russell Coleman","Russell Impagliazzo","Russell Lande","Russell Roberts","Russell Spears","Russell Thomson","Ruth Armstrong",
  "Ruth E. Ley","Ruth Faden","Ruth Heller","Ruth Johnson","Ruth M. J. Byrne","Ruth Mace","Ruth Nussinov",
  "Ruth O\8217Hara","Ruth Shonle Cavan","Ruut Veenhoven","Ryan Bradley","Ryan Dancey","Ryan Espiritu","Ryan Gallagher",
  "Ryan Henderson","Ryan Holiday","Ryan Julian","Ryan Lowe","Ryan Sullivan","Ryan Turner","Ryan Willie",
  "Ryohei Hayashi","Ryohei Suzuki","Ryota Watanabe","Ryusuke Hikawa","Ryuta Kawashima","Ryuzo Yanagimachi","R\252diger Fahlenbrach",
  "S. Alexandra Burt","S. E. Harris","S. Kumar","S. W. Lewis","Saahil Jain","Sabine Fuss","Sachin Kumar",
  "Sachin Ravi","Sadek Wahba","Sagar Shah","Sahib Singh","Sahil Patel","Sai Prashanth","Sakhavat Mammadov",
  "Salil Vadhan","Salim Yusuf","Sally Larsen","Sally Satel","Salman Avestimehr","Salman Khan","Sam Altman",
  "Sam Anderson","Sam Blackwell","Sam Dodge","Sam Gross","Sam Kean","Sam Kwong","Sam Mercer",
  "Sam Peltzman","Sam Ritter","Samantha McCarthy","Samantha Watson","Sami Haddadin","Samir Arora","Samir Saba",
  "Samuel A. Stouffer","Samuel D. Gosling","Samuel Ginn","Samuel Gosling","Samuel J. Holmes","Samuel L. Braunstein","Samuel L. Smith",
  "Samuel Marks","Samuel R. Buss","Samuel S. Kortum","Samuel S. Wineburg","Samuel T. Cohen","Samy Bengio","Sana Amanat",
  "Sandeep Singh","Sander Greenland","Sander van der Linden","Sandra Scarr","Sandra Weintraub","Sandu Popescu","Sang-Won Park",
  "Sanghamitra Mohanty","Sanja Fidler","Sanjay Asthana","Sanjay Ghemawat","Sanjay Jain","Sanjay Srivastava","Sanjeev Arora",
  "Sanjiv Kumar","Santosh Vempala","Sapna Maheshwari","Sara A. Solla","Sara Garcia","Sara M. Lewis","Sara Moreira",
  "Sara Seager","Sarah A. Tishkoff","Sarah Byford","Sarah Chen","Sarah E. Anderson","Sarah Ennis","Sarah Haider",
  "Sarah Henderson","Sarah Jeong","Sarah Kreps","Sarah Lindsay","Sarah Mathew","Sarah Medland","Sarah Oppenheimer",
  "Sarah Otto","Sarah Parish","Sarah Wild","Sarah York","Sarah-Jayne Blakemore","Sarit Kraus","Sarnoff A. Mednick",
  "Satinder Singh","Satoshi Iizuka","Satoshi Matsuoka","Satoshi Nakamoto","Satya Nadella","Saul Perlmutter","Saul Rosenzweig",
  "Saurabh Sinha","Saurabh Tiwary","Sayan Ghosh","Sayantan Das","Scale AI","Scott Aaronson","Scott Crow",
  "Scott Cunningham","Scott E. Fahlman","Scott Heiner","Scott Johnston","Scott Keeney","Scott Mahlke","Scott McCoy",
  "Scott O. Lilienfeld","Scott Orr","Scott Pelley","Scott Shannon","Scott Small","Scott W. Brady","Scott Wolf",
  "Seamus Heaney","Sean Bell","Sean Bradley","Sean F. Reardon","Sean Hogan","Sean M. Carroll","Sean M. Ryan",
  "Sean Mayes","Sean O\8217Keeffe","Sean Patterson","Sean Stevens","Sean Whalen","Sean Wharton","Sebastian Brather",
  "Sebastian Deffner","Sebastian Deterding","Sebastian Thrun","Sebastian Vollmer","Sebastien Bubeck","Sebastien Lefebvre","Sekar Kathiresan",
  "Sendhil Mullainathan","Seon-Young Kim","Seonghyeon Kim","Sepp Hochreiter","Serena Chen","Serge Belongie","Sergei Guriev",
  "Sergey Batalov","Sergey Brin","Sergey Feldman","Sergey Gavrilets","Sergey Koren","Sergey Malitsky","Sergey Markov",
  "Sergey Melnikov","Sergey Titov","Sergio Boixo","Sergio Casas","Sergio Della Sala","Sergio Guadarrama","Sergio Vargas",
  "Seth Godin","Seth Lloyd","Seth Roberts","Sewall Wright","Seymour Papert","Shadi Sadr","Shafi Goldwasser",
  "Shaker Ahmed","Sham Kakade","Shamsul Huda","Shan Dong","Shan Wei","Shane Legg","Shane McCarthy",
  "Shang Yang","Shang-Hua Teng","Shanshan Huang","Shaomeng Wang","Sharon Begley","Sharon Leal","Sharon N. DeWitte",
  "Shaun Purcell","Shawn Bayern","Shawn Yue","Sheila McIlraith","Sheldon C. Reed","Shelley L. Berger","Shelly Flagel",
  "Shen Li","Sheng Liu","Sheng Shen","Sheng Wang","Sheri Fink","Sherry Glied","Sherry Shi",
  "Sherwin Rosen","Shi Feng","Shih-Chii Liu","Shih-Fu Chang","Shih-Jen Hwang","Shinji Higuchi","Shinya Hasegawa",
  "Shirley Ho","Shirley Wu","Shirly Pinto","Shital Shah","Shivendra Singh","Shlomo Benartzi","Shlomo Dubnov",
  "Shlomo Moran","Sholto Douglas","Shoukhrat Mitalipov","Shu Yang","Shuai Peng","Shuai Zhang","Shuai Zhao",
  "Shuang Zhao","Shubham Agarwal","Shuchi Chawla","Shuo Wang","Shweta Bhardwaj","Shwetak Patel","Si Si",
  "Siddhartha Mukherjee","Sidney Hemming","Siegfried Bernfeld","Silvia Paracchini","Simin Liu","Simine Vazire","Simon Akam",
  "Simon Baron-Cohen","Simon Campbell","Simon Colton","Simon E. Fisher","Simon Fishel","Simon Gane","Simon Grimm",
  "Simon G\228chter","Simon Haworth","Simon Holden","Simon Karlsson","Simon Kelner","Simon Kirby","Simon Lovestone",
  "Simon Tong","Simone Bianco","Simone Pika","Simone Rossi","Sing Bing Kang","Siobhan Roberts","Siva Reddy",
  "Soham Ghosh","Sohini Ramachandran","Solomon E. Asch","Song-Chun Zhu","Sonia Shah","Sonya Williams","Sopan Deb",
  "Sophia Ananiadou","Sophia Frangou","Sophie von Stumm","Soumen Chakrabarti","Soumya Raychaudhuri","Soung Chang Liew","Sourav Chatterjee",
  "Souvik Ghosh","Spectrum 10K","Spencer Fox","Spencer Heath","Spruha Joshi","Spyros Makridakis","Sreenivasa Murthy",
  "Sri Lakshmi","St. Clair McKelway","Stacey B. Gabriel","Stacey Gabriel","Staffan Burenstam Linder","Stan Boutin","Stan Nelson",
  "Stanislas Dehaene","Stanislav Andreski","Stanislav Morozov","Stanislaw Jastrzebski","Stanislaw Lem","Stanis\322aw Lem","Stanley A. Mulaik",
  "Stanley Coren","Stanley L. Engerman","Stanley M. Garn","Stanley Reiter","Stanley Schachter","Stanley Zammit","Stanton A. Glantz",
  "Stefan Bucher","Stefan Johansson","Stefan Popov","Stefan R. Bornstein","Stefan Schaal","Stefan Szeider","Stefan Thurner",
  "Stefan Wachter","Stefan Wolf","Stefan Wurster","Stefan Zary","Stefania Serafin","Stefanie Stantcheva","Stefano DellaVigna",
  "Stefano Lombardi","Stefano Ugolini","Steffen Schmidt","Steffen Schneider","Stella Chen","Stephan Collishaw","Stephan Lewandowsky",
  "Stephan Ripke","Stephan Seiler","Stephan Zipfel","Stephanie Forrest","Stephanie J. London","Stephanie Lin","Stephanie Lopez",
  "Stephen A. Ross","Stephen Ansolabehere","Stephen Bell","Stephen Budiansky","Stephen C. Stearns","Stephen Ceci","Stephen Chanock",
  "Stephen E. Epstein","Stephen E. Fienberg","Stephen E. Harris","Stephen Franks","Stephen G. Waxman","Stephen Hsu","Stephen J. Ceci",
  "Stephen J. Elledge","Stephen J. O\8217Brien","Stephen J. Roberts","Stephen J. Simpson","Stephen J. Wright","Stephen L. Carter","Stephen L. Macknik",
  "Stephen LaBerge","Stephen Laurence","Stephen M. Omohundro","Stephen M. Stigler","Stephen Machin","Stephen Marche","Stephen McLaughlin",
  "Stephen Meader","Stephen O\8217Rahilly","Stephen Pacala","Stephen R. Palumbi","Stephen Richardson","Stephen Schaffer","Stephen Spencer",
  "Stephen Stearns","Stephen T. Ziliak","Stephen V. Faraone","Stephen W. Raudenbush","Stephen W. Scherer","Stephen Wolfram","Stevan J. Arnold",
  "Steve Blank","Steve Eyre","Steve Franks","Steve Hoffmann","Steve Horvath","Steve Jurvetson","Steve Lacy",
  "Steve Malone","Steve Olson","Steve O\8217Brien","Steve Ramirez","Steve Rayner","Steve Yegge","Steven A. Frank",
  "Steven Bakker","Steven Bell","Steven D. Hollon","Steven D. Levitt","Steven D\8217Hondt","Steven E. Hyman","Steven E. Nissen",
  "Steven Farber","Steven G. Krantz","Steven G. Vandenberg","Steven Gangestad","Steven H. Strogatz","Steven Hsu","Steven Hyman",
  "Steven J. Knapp","Steven L. Salzberg","Steven Laureys","Steven Levy","Steven Lin","Steven Liu","Steven M. Paul",
  "Steven N. Blair","Steven N. Goodman","Steven N. S. Cheung","Steven Pinker","Steven Reich","Steven Siegel","Steven W. Gangestad",
  "Steven Weber","Steven Wheelwright","Steven Wiltshire","Stewart Payne","Stijn Van Nieuwerburgh","Stuart Armstrong","Stuart Carlson",
  "Stuart Cheshire","Stuart E. Dreyfus","Stuart J. Ritchie","Stuart Rogers","Studio Khara","Stylianos E. Antonarakis","St\233phane Mallat",
  "Sudip Roy","Sue Biggins","Sue Gardner","Sue Pedersen","Sue Price","Suhas Diggavi","Suji Kim",
  "Sujit Dey","Sukhdeep Singh","Sultan Mehmood","Sumio Watanabe","Sumit Jain","Sunghyun Cho","Sunil K. Ahuja",
  "Suresh Naidu","Surya Ganguli","Susan Anderson","Susan Athey","Susan C. Alberts","Susan C. Baker","Susan E. Gathercole",
  "Susan E. Jackson","Susan E. Mayer","Susan Golombok","Susan Holmes","Susan M. Wolf","Susan Martin","Susan Nolen-Hoeksema",
  "Susan Redline","Susan Smalley","Susan Stepney","Susan T. Fiske","Susan Thomas","Susan Whitfield-Gabrieli","Susan Y. Bookheimer",
  "Susana Martinez-Conde","Susannah Cahalan","Susmita Datta","Suzana Herculano-Houzel","Suzanne Bakken","Svante P\228\228bo","Sven Pettersson",
  "Svenn Torgersen","Swati Rajput","Swati Sharma","Sy Montgomery","Sylvia Wassertheil-Smoller","S\233bastien Bubeck","S\233bastien Rouault",
  "S\248ren Brunak","T. C. Schneirla","T. Douglas Price","T. Hayashi","T. J. Hamblin","T. Lewis","T. Rado",
  "T. W. Chaundy","T. W. Robbins","Tad Friend","Tadayoshi Kohno","Taha Yasseri","Taissa S. Hauser","Takahiro Miki",
  "Takahiro Ogawa","Takahiro Suzuki","Takao Suzuki","Takashi Abe","Takashi Gojobori","Takashi Murakami","Takashi Onishi",
  "Takashi Shinohara","Takayuki Ito","Takeshi Okamoto","Takuro Yamashita","Takuya Yamamoto","Tali Sharot","Tamara Berg",
  "Tamer Ba\351ar","Tamsin Ford","Tan Yu","Tanja Schultz","Tanya Khovanova","Tao Li","Tao Liu",
  "Tao Qin","Tao Sheng","Tao Yang","Tao Zhu","Tara Javidi","Tariq Ahmad","Tariq Ali",
  "Tarjinder Singh","Tarun Khanna","Tasso Adamopoulos","Tatiana Foroud","Tatjana Rundek","Tatsuo Tanaka","Taylor Stevens",
  "Teck-Hua Ho","Ted Chiang","Ted Kaehler","Ted Nettelbeck","Teddy Seidenfeld","Teng Li","Tengfei Shi",
  "Terah Lyons","Terence Tao","Teresa Amabile","Teresa Wilson","Terrence J. Sejnowski","Terrie E. Moffitt","Terrie Moffitt",
  "Terry Allen Winograd","Terry Castle","Terry Gaasterland","Terry Winograd","Tetsuo Najita","Tetsuya Takahashi","Thad Starner",
  "Thalia C. Eley","Thao Nguyen","The Associated Press","The British Psychological Society","The Express Tribune","The Harris Poll","The Metropolitan Opera",
  "The Modern House","The New York Times","The Onion","The Public Domain Review","The Royal Swedish Academy of Sciences","Theodore C. Schneirla","Theodore Caplow",
  "Theodore Groves","Theodore P. Hill","Theodore X. Barber","Theodore von K\225rm\225n","Theodosius Dobzhansky","Thierry Moreau","Thom Holwerda",
  "Thomas A. DiPrete","Thomas A. Guglielmo","Thomas A. Rando","Thomas A. Wadden","Thomas Bachlechner","Thomas Blanchet","Thomas Bourgeron",
  "Thomas Braun","Thomas Buckley","Thomas C. Edwards","Thomas Crook","Thomas Dietterich","Thomas Duffield","Thomas E. Martin",
  "Thomas E. Moore","Thomas E. Nichols","Thomas Eisner","Thomas Erdbrink","Thomas Ernst","Thomas Espeseth","Thomas Falconer",
  "Thomas G. Dietterich","Thomas G. Dunn","Thomas Gilovich","Thomas Hills","Thomas Hoffman","Thomas Hofmann","Thomas Huang",
  "Thomas I. Miller","Thomas J. Bouchard","Thomas J. Carew","Thomas Jefferson","Thomas J\248rgensen","Thomas K. Landauer","Thomas Kruse",
  "Thomas L. Griffiths","Thomas Lancaster","Thomas Lang","Thomas Leopold","Thomas M. Cover","Thomas M. Klap\246tke","Thomas Moynihan",
  "Thomas Mueller","Thomas M\252ller","Thomas Nagel","Thomas Neumann","Thomas O\8217Rourke","Thomas Paine","Thomas R. Gingeras",
  "Thomas Renault","Thomas S. Dee","Thomas S. Huang","Thomas S. Ray","Thomas Sch\228fer","Thomas Vitale","Thomas Warren",
  "Thomas Wisniewski","Tiago Marques","Tiago Monteiro","Tian Li","Tian Wu","Tian Xia","Tianyi Zhang",
  "Tie-Yan Liu","Tiffany Stewart","Tim Byrne","Tim D. Spector","Tim Ferriss","Tim Finin","Tim Green",
  "Tim H. Clutton-Brock","Tim Hesterberg","Tim Hutton","Tim Kraska","Tim Menzies","Tim O\8217Reilly","Tim Roughgarden",
  "Tim Spector","Tim Tully","Timo Ewalds","Timothy A. Salthouse","Timothy Baldwin","Timothy C. Bates","Timothy C. Beers",
  "Timothy C. May","Timothy D. Wilson","Timothy F. Bresnahan","Timothy Gowers","Timothy J. Bartik","Timothy Jenkins","Timothy Lillicrap",
  "Timothy M. Lenton","Timothy R. Levine","Timothy Z. Keith","Timur Kuran","Tin Aung","Tina Eliassi-Rad","Tina Goldstein",
  "Tina Thompson","Tina Wang","Ting Yao","Ting Zhang","Ting Zhao","Tinglong Dai","Tingting Jiang",
  "Tobias Frere-Jones","Toby Johnson","Toby Murray","Toby Ord","Todd D. Gould","Todd K. Shackelford","Todd Lencz",
  "Todd Shackelford","Tom Boone","Tom Booth","Tom Bryan","Tom Chadwick","Tom DeMarco","Tom Downey",
  "Tom Eccles","Tom Goldstein","Tom Hope","Tom Jensen","Tom Johnstone","Tom Junod","Tom Mes",
  "Tom Nicholas","Tom Ward","Tomas Chamorro-Premuzic","Tomas Mikolov","Tomas Olsson","Tomaso Poggio","Tomasz Smole\324",
  "Tomi Rantam\228ki","Tommaso Toffoli","Tom\225\353 \268erm\225k","Tong Sun","Tony Burton","Tony Lee","Tony Liu",
  "Tony Robinson","Toomas Kivisild","Tor D. Wager","Torben Hansen","Torkel Klingberg","Torsten Hoefler","Torsten Persson",
  "Toshio Okada","Tourette Syndrome","Tracy Packiam Alloway","Travis Hirschi","Trevor Darrell","Trevor Davis","Trevor Hastie",
  "Trevor W. Robbins","Trisha Suppes","Tristan Thrush","Truman Lee Kelley","Trung Nguyen","Trygve J. B. Hoff","Tsukasa Yoshida",
  "Tuomas Sandholm","Tyler Anbinder","Tyler Cowen","Tyler Jacks","Tyler Murray","Tyrone D. Cannon","U. S. Department of Justice",
  "Ufuk Akcigit","Ulric Neisser","Ulrich Kutschera","Ulrich M\252ller","Ulrich Preuss","Ulrich Schmidt","Ulrich Trautwein",
  "Ulrike Schmidt","Uma Ramakrishnan","Umberto Eco","United States Commission on Civil Rights","United States District Court Southern District of New York","United States District Court for the Eastern District of Pennsylvania","Unity Biotechnology",
  "Urho Kujala","Uri Alon","Uri Gneezy","Uri Simonsohn","Uri Zwick","Urie Bronfenbrenner","Urs Egger",
  "Urs Fischbacher","Ursula K. Le Guin","Ursula M. Staudinger","Ursula Rothlisberger","Utkarsh Sharma","Uwe Scholz","Uwe Sunde",
  "V. S. Sundar","Vadim Borisov","Vadim N. Gladyshev","Vadim Sokolov","Vaibhav Mathur","Valentin Robu","Valerie M. Weaver",
  "Valery N. Soyfer","Valsamma Eapen","Valter D. Longo","Van Dongen","Vaneet Aggarwal","Varun Tej","Vaughan R. Pratt",
  "Veena Kumari","Vegard Skirbekk","Venkatesh Rao","Vera Demberg","Vera Gorbunova","Vera John-Steiner","Vernon L. Smith",
  "Vernon Williams","Vernor Vinge","Veronica Galvan","Veselin Stoyanov","Victor Andrade","Victor Chan","Victor Goertzel",
  "Victor Haghani","Victor Shapiro","Victor W. Turner","Victoria Harrison","Victoria Leong","Victoria Perez","Victoria Stodden",
  "Vijay Pereira","Vikas Anand","Vikas Verma","Vikash Kumar","Vinay Prasad","Vince Calhoun","Vincent Egan",
  "Vincent P. Crawford","Vincent Savolainen","Vincent Zhao","Vinod Khosla","Vinod Kumar","Vinod Vaikuntanathan","Virginia Gregg",
  "Virginia Heffernan","Virginia Kidd","Virginia Postrel","Virpi Lummaa","Vitalik Buterin","Vitaly Shmatikov","Vivek Kumar",
  "Viviane Slon","Vladimir Kim","Vladimir Kramnik","Vladimir Naumov","Vladimir Vapnik","Vladlen Koltun","V\237ctor Vald\233s",
  "W. Bentley MacLeod","W. Bruce Croft","W. Grant Dahlstrom","W. Keith Campbell","W. Patrick McCray","W. R. Ashby","W. Ross Ashby",
  "W. Tecumseh Fitch","W. Timothy Garvey","Waleed Khan","Wallace E. Oates","Walter Bodmer","Walter C. Willett","Walter Guerra",
  "Walter Isaacson","Walter J. Ong","Walter Mischel","Walter R. Dornberger","Walter Scheidel","Walter Sinnott-Armstrong","Walter Y. Oi",
  "Wang Fa","Warner R. Schilling","Warren Weaver","Wayne Dennis","Wei Bi","Wei Dai","Wei Fang",
  "Wei Fu","Wei Gan","Wei Gao","Wei Han","Wei He","Wei Huang","Wei Ji Ma",
  "Wei Lu","Wei Song","Wei Wen","Wei Xiong","Wei Xu","Wei Yu","Wei Zheng",
  "Weihua Zhang","Weinan E","Wellcome Trust Case Control Consortium","Wells Tower","Wen Gao","Wen Qin","Wen Shen",
  "Wen Sun","Wen Wang","Wen Xie","Wen Xue","Wen Zhang","Wen Zhou","Wenbo Wang",
  "Wendy B. Mendes","Wendy Benson","Wendy Chung","Wendy Johnson","Wendy M. Williams","Wendy Post","Wendy S. Post",
  "Wendy Shang","Wendy Slutske","Wenjun Zeng","Wenyuan Xu","Wesley R. Elsberry","Wiebke Arlt","Wiebke Bleidorn",
  "Wilder Penfield","Wilfred Reilly","Wilfrid Hodges","Wilhelm Ketteler","Will Kay","Will Knight","Will Martin",
  "Will Poole","Will Smith","Will Storr","Will Wilkinson","Willard Van Orman Quine","Willem H. Ouwehand","Willem Hendrik Ouwehand",
  "Willem Kuyken","Willem M. de Vos","William A. Roberts","William B. Lawson","William B. Provine","William Beauchamp","William Beggs",
  "William Bennett Bean","William Bracken","William Bright","William Bunney","William C. Roberts","William Chan","William Chow",
  "William Cusick","William D. Hill","William Dickens","William Durden","William E. Bunney","William Easterly","William F. Lamb",
  "William Fithian","William Flack","William Foran","William G. Durden","William G. Hill","William Gibson","William H. McClain",
  "William H. McRaven","William H. Press","William H. Warren","William Harvey","William Hirst","William Iacono","William Ickes",
  "William Isaac","William J. Brady","William J. Broad","William J. Calhoun","William J. Dally","William J. Dunn","William J. Gibbons",
  "William J. McGuire","William James","William K. Scott","William L. Holland","William Langewiesche","William Lowe Bryan","William Maurice Ewing",
  "William Olson","William O\8217Donohue","William R. Johnson","William Revelle","William S. Bush","William S. Jewell","William Sealey Gosset",
  "William Sealy Gosset","William Shockley","William T. Cefalu","William T. Dickens","William T. Freeman","William Turton","William W. Cohen",
  "William W. Seeley","William Wang","Wim E. Crusio","Wim van den Brink","Winfried Denk","Winfried Rief","Wing Suen",
  "Winnie Ng","Winston Wang","Wis\322awa Szymborska","Wojciech Rytter","Wojciech Zaremba","Wolf Reik","Wolfgang B\246hme",
  "Wolfgang Hoenig","Wolfgang Hoffmann","Wolfgang Maier","Wolfgang Pesendorfer","Wolfgang Stroebe","Wolfgang Viechtbauer","Wolfram Burgard",
  "Woo Suk Hwang","Woo-Suk Hwang","Wulfram Gerstner","Xavier Gabaix","Xavier Serra","Xi Chen","Xi Wang",
  "Xi Yin","Xia Hu","Xia Li","Xian Yang","Xiang Chen","Xiang Cheng","Xiang Gao",
  "Xiang Zhang","Xiao Feng","Xiao Tian","Xiao Wang","Xiao Xia","Xiao Xiao","Xiao-Li Meng",
  "Xiaojie Wang","Xiaoli Wang","Xiaoling Zhang","Xiaoming Liu","Xiaonan Zhang","Xiaoou Tang","Xiaowei Li",
  "Xiaoxin Chen","Xie Chen","Xifeng Wu","Xihong Lin","Xin Di","Xin Huang","Xin Jiang",
  "Xin Meng","Xin Tong","Xin Yu","Xin Zhang","Xin Zhou","Xing Sun","Xinjun Zhang",
  "Xinwen Zhu","Xinyan Zhang","Xinyi Xu","Xinyu Zhang","Xu Chen","Xu Han","Xu Li",
  "Xu Shi","Xu Sun","Xu Xu","Xuan Chen","Xuan Zang","Xuedong Huang","Xun Wang",
  "Xun Xu","Yaacov Trope","Yan Ding","Yan Li","Yan Liang","Yan Long","Yan Lu",
  "Yang An","Yang Fu","Yang Gao","Yang He","Yang Hu","Yang Shi","Yang Song",
  "Yang Wu","Yang You","Yang Yue","Yang Zhao","Yang Zhou","Yaniv Altshuler","Yaniv Erlich",
  "Yann LeCun","Yannic Kilcher","Yasheng Huang","Yasin Ozcan","Yasuhiro Sato","Yasuhiro Takeda","Yasuo Kuniyoshi",
  "Ye Feng","Ye Li","Ye Xia","Yee Whye Teh","Yejin Choi","Yi Ji","Yi Jiang",
  "Yi Ma","Yi Mao","Yi Rao","Yi Shang","Yi Wang","Yi Wei","Yi Wen",
  "Yi Wu","Yi Yu","Yi Zuo","Yifan Xu","Yihan Wang","Yili Wu","Yilin Fan",
  "Yilin Yang","Yiling Chen","Yilu Wang","Yiming Zhang","Ying Gu","Ying Guo","Ying Lu",
  "Ying Shan","Ying Wu","Ying Xu","Ying Yang","Ying Zhu","Ying-Hui Fu","Yingying Chen",
  "Yinsheng Wang","Yishan Wong","Yiwei Zhang","Yixuan Li","Yoav Benjamini","Yoav Shoham","Yoh Iwasa",
  "Yoichi Takahashi","Yoji Enokido","Yolanda Gil","Yolanda Moses","Yong Liu","Yong Rui","Yong Tan",
  "Yong Zhao","Yonggang Huang","Yonhap News Agency","Yoshihiro Kawaoka","Yoshiki Kuroda","Yoshio Miki","Yoshiyuki Tomino",
  "Yoshua Bengio","Yossi Matias","Young-Tae Chang","Yu (Jeffrey) Hu","Yu Cao","Yu Feng","Yu Gu",
  "Yu Hu","Yu Shi","Yu Song","Yu Su","Yu Sun","Yu Tian","Yu Xie",
  "Yu Yamamoto","Yuan Cao","Yuan Chen","Yuan Gong","Yuan Hao","Yuan He","Yuan Jiang",
  "Yuan Li","Yuan Liang","Yuan Xie","Yuan Xue","Yuan Yao","Yuan Yin","Yuan-Tsong Chen",
  "Yudhanjaya Wijeratne","Yue Huang","Yue Li","Yue Shan","Yue Wan","Yue Yang","Yufeng Zhang",
  "Yuhua Wang","Yuichi Shoda","Yuji Ijiri","Yuji Kato","Yukiyasu Kamitani","Yulia Kovas","Yulin Liu",
  "Yun Wang","Yunfeng Liu","Yuri Matiyasevich","Yurii Nesterov","Yury Volkov","Yusuke Matsui","Yusuke Murase",
  "Yusuke Takahashi","Yutaka Izubuchi","Yutaka Suzuki","Yutaka Tahara","Yutaka Yoshida","Yutaro Sugimoto","Yutian Chen",
  "Yuval Elovici","Yuval Peres","Yuwei Li","Yuwen Zhang","Yuxuan Zhang","Yves Bertrand","Yves Moreau",
  "Yvonne Harrison","Y\333ji Enokido","Zachary Anderson","Zachary Charles","Zachary Fisher","Zaheer Abbas","Zaida Luthey-Schulten",
  "Zehao Sun","Zellig S. Harris","Zeng Tao","Zenobia Jacobs","Zenon Kulpa","Zhao Chen","Zhao Song",
  "Zhao Xue","Zhao Zhong","Zhen Fan","Zhen Tan","Zhendong Wang","Zheng Cao","Zheng Tian",
  "Zheng Yan","Zheng Zhu","Zhengyou Zhang","Zhi Zheng","Zhi-Li Zhang","Zhichao Li","Zhihong Chen",
  "Zhihui Wang","Zhiping Weng","Zhiwei Wang","Zhiwu Lu","Zhiyi Zhang","Zhou Ren","Zhou Yu",
  "Zi Wang","Zicheng Liu","Zoe Kourtzi","Zoe Laughlin","Zoe R. Donaldson","Zong Chen","Zoubin Ghahramani",
  "Zuzana Pavelkov\225","Zvi Galil","Zvi Griliches","Zvika Brakerski","\193d\225m Mikl\243si"
  , "OpenAI"]
