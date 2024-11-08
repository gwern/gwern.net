#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# title-cleaner.py: remove cruft from titles of web pages like website name/domain or error messages
# Author: Gwern Branwen
# Date: 2024-06-11
# When:  Time-stamp: "2024-11-07 13:00:30 gwern"
# License: CC-0
#
# Usage: $ OPENAI_API_KEY="sk-XXX" xclip -o | python title-cleaner.py
#
# When scraping HTML web pages, using the <title> field naively gives bad results: many are covert error pages, or a fixed site-wide constant string, or uselessly vague/short, or prepend/postpend the site URL or the site name or a whole variety of bizarre things.
# Manually writing rules to clean them up turns out to be unmanageable due to the extreme long-tail of handling >20,000 URLs on gwern.net
# However, the badness in scraped titles is very much a "I know it when I see it" thing and can usually be done in isolation when looking just at the text.
# So that means it is feasible to call out to a LLM to clean up the title.

import sys
from openai import OpenAI
client = OpenAI()

if len(sys.argv) == 1:
    target = sys.stdin.read().strip()
else:
    target = sys.argv[1]

prompt = """
Task: Clean website titles parsed from <title> tags.
If a title input is useless or meaningless or an error, print out the empty string `""` instead of the original title.
If the title can be fixed, remove the junk (spam, cruft, boilerplate) from the title.
If the title looks good, then print out the original title.
If you are unsure how to fix it, then simply print out the original title.

Task examples:

- Input title to clean: "If I Sleep for an Hour, 30 People Will Die - The New York Times"
If I Sleep for an Hour, 30 People Will Die
- Input title to clean: "404"
""
- Input title to clean: "Gwern.net | Collecting New Socks Efficiently"
Collecting New Socks Efficiently
- Input title to clean: "worlds of DAVID BRIN"
""
- Input title to clean: "steve yegge · The Amazon Memo"
The Amazon Memo
- Input title to clean: "index"
""
- Input title to clean: ""
""
- Input title to clean: "After 92 years, millionaire miserâs heirs finally split $100M - TODAY People - TODAY.com"
After 92 years, millionaire miser’s heirs finally split $100M
- Input title to clean: "502 Bad Gateway"
""
- Input title to clean: "Faces2Anime: Cartoon Style Transfer in Faces using Generative Adversarial Networks. Masters Thesis 2021 @ NTUST."
Faces2Anime: Cartoon Style Transfer in Faces using Generative Adversarial Networks
- Input title to clean: "articles"
""
- Input title to clean: "Vercel Security Checkpoint"
""
- Input title to clean: "Fan Is A Tool-Using AnimalâdConstruct Conference Talk"
Fan Is A Tool-Using Animal
- Input title to clean: "CONTENTdm"
""
- Input title to clean: "The Illustrated Retrieval Transformer â Jay Alammar â Visualizing machine learning one concept at a time."
The Illustrated Retrieval Transformer
- Input title to clean: "Redirecting to https://now.tufts.edu/2016/07/14/moderately-reducing-calories-non-obese-people-reduces-inflammation"
""
- Input title to clean: "Not Found - Albion - Webflow HTML website template"
""
- Input title to clean: "Siberian Times"
""
- Input title to clean: "Stripe\\226\\128\\153s first carbon removal purchases\\nStripe logo\\nOpen mobile navigation\\nStripe logo\\nClose mobile navigation\\nPayments\\nCheckout\\nElements\\nRadar\\nConnect\\nBilling\\nInvoicing\\nTerminal\\nIdentity\\nClimate\\nBilling\\nInvoicing\\nTax\\nRevenue Recognition\\nSigma\\nAtlas\\nConnect\\nCapital\\nIssuing\\nTreasury\\nPayments\\nCheckout\\nElements\\nRadar\\nConnect\\nBilling\\nInvoicing\\nTerminal\\nIdentity\\nClimate\\nBilling\\nInvoicing\\nTax\\nRevenue Recognition\\nSigma\\nAtlas\\nConnect\\nCapital\\nIssuing\\nTreasury\\nStripe logo"
Stripe’s first carbon removal purchases
- Input title to clean: "308 Permanent Redirect"
""
- Input title to clean: "404 - Isomorphic Labs"
""
- Input title to clean: "Page not found - PsyPost - Psychology News"
""
- Input title to clean: "Japanese north\226\128\147south gradient in IQ predicts differences in stature, skin color, income, and homicide rate - ScienceDirect\nScienceDirect"
Japanese north-south gradient in IQ predicts differences in stature, skin color, income, and homicide rate
- Input title to clean: "Notion – The all-in-one workspace for your notes, tasks, wikis, and databases."
""
- Input title to clean: "It’s (Still) Really Hard for Robots to Autonomously Do Household Chores - IEEE Spectrum"
It’s (Still) Really Hard for Robots to Autonomously Do Household Chores
- Input title to clean: "504 Gateway Time-out"
""
- Input title to clean: "æ°æ°æ®é"
""
- Input title to clean: "dnmarchives directory listing\nInternet Archive logo\nDonate icon\nSearch icon\nSearch icon\nUpload icon\nUser icon\nWeb icon\nTexts icon\nVideo icon\nAudio icon\nSoftware icon\nImages icon\nDonate icon\nEllipses icon\nHamburger icon\nSearch icon\nDonate icon"
dnmarchives directory listing
- Input title to clean: "Page not found : Stanford University"
""
- Input title to clean: "07"
""
- Input title to clean: "Flashback Forum"
""
- Input title to clean: "Stuff"
""
- Input title to clean: "Humboldt &amp; Sonoma counties: Six arrested, 3,000 marijuana plants and 44 weapons seized in state DOJ raids - The Willits News"
Humboldt &amp; Sonoma counties: Six arrested, 3,000 marijuana plants and 44 weapons seized in state DOJ raids
- Input title to clean: "GoLocalPDX"
""
- Input title to clean: "Aurora’s Approach to Development. Self-driving cars are an appliedâ¦ by The Aurora Team Aurora Blog"
Aurora’s Approach to Development. Self-driving cars are an applied
- Input title to clean: "An open letter to Netflix from the authors of the de-anonymization paper « 33 Bits of Entropy"
An open letter to Netflix from the authors of the de-anonymization paper
- Input title to clean: "It\226\128\153s Probably Not Lithium\nspotify-podcast-badge-wht-blk-165x40"
It’s Probably Not Lithium
- Input title to clean: "Scunthorpe Sans \240\159\151\175\240\159\154\171 profanity-blocking font"
Scunthorpe Sans: a profanity-blocking font
- Input title to clean: "&#13;\n\tMedicine &amp; Science in Sports &amp; Exercise&#13;"
""
- Input title to clean: "Nadia Asparouhova How to do the jhanas"
How to do the jhanas
- Input title to clean: "Best-of-n with misaligned reward models for Math reasoning"
Best-of-<em>n</em> with misaligned reward models for Math reasoning
- Input title to clean: "The Universe of Discourse : A potpourri of cool-looking scripts"
A potpourri of cool-looking scripts
- Input title to clean: "FineWeb: decanting the web for the finest text data at scale - a Hugging Face Space by HuggingFaceFW"
FineWeb: decanting the web for the finest text data at scale
- Input title to clean: "Do life hacks work? The truth is, weâll never know Psychology"
Do life hacks work? The truth is, we’ll never know
- Input title to clean: "Operant Conditioning by Software Bugs – Embedded in Academia"
Operant Conditioning by Software Bugs
- Input title to clean: "The dream of an alpine waterway - Swiss National Museum - Swiss history blog"
The dream of an alpine waterway
- Input title to clean: "Andrew Wood The Medical School"
Andrew Wood
- Input title to clean: "Reddit"
""
- Input title to clean: "The Stigler Diet Problem �|� OR-Tools �"
The Stigler Diet Problem
- Input title to clean: "why why why why why why why"
""
- Input title to clean: "Optimized, Individualized Spaced Repetition in Hierarchical Knowledge Structures - Justin Skycak"
Optimized, Individualized Spaced Repetition in Hierarchical Knowledge Structures
- Input title to clean: "Exclusive"
""
- Input title to clean: "Patronage vs. Constituent Parties (Or Why Republican Party Leaders Matter More Than Democratic Ones) – The Scholar's Stage"
Patronage vs. Constituent Parties (Or Why Republican Party Leaders Matter More Than Democratic Ones)
- Input title to clean: "Screen Media Use and Mental Health of Children and Adolescents: A Secondary Analysis of a Randomized Clinical Trial Media and Youth JAMA Network Open"
Screen Media Use and Mental Health of Children and Adolescents: A Secondary Analysis of a Randomized Clinical Trial Media and Youth
- Input title to clean: "Joaquin Qui�onero Candela"
Joaquin Quiñonero Candela
- "A Visual Guide to Quantization - by Maarten Grootendorst"
A Visual Guide to Quantization
- "Time Use - Our World in Data"
Time Use
- "The secret of Minecraft. And its challenge to the rest of us by Robin Sloan The Message"
The secret of Minecraft. And its challenge to the rest of us
- "Attribution is Dying. Clicks are Dying. Marketing is Going Back to the 20th Century. - SparkToro"
Attribution is Dying. Clicks are Dying. Marketing is Going Back to the 20th Century
- "Your Book Review: Real Raw News - Astral Codex Ten"
Your Book Review: <em>Real Raw News</em>
- "Probably Overthinking It: There is still only one test"
There is still only one test
- "Randomly updated"
""
- "The Crystal Star Wookieepedia"
<em>The Crystal Star</em>
- "Abstract Heresies: Not Lisp again...."
Not Lisp again...
- "Morbid attraction to leopard urine in Toxoplasma-infected chimpanzees - ScienceDirect"
Morbid attraction to leopard urine in <em>Toxoplasma</em>-infected chimpanzees
- "EP021 - Bulbapedia, the community-driven PokÃ©mon encyclopedia"
Ep021
- "Your Book Review: Two Arms and a Head - Astral Codex Ten"
Your Book Review: <em>Two Arms and a Head</em>
- "Your Book Review: Two Arms and a Head - Astral Codex Ten"
Your Book Review: <em>Two Arms and a Head</em>
- "Redirecting"
""
- "From Bing to Sydney – Stratechery by Ben Thompson"
From Bing to Sydney
- "Seeing Centuries - by RJ Andrews - Chartography"
Seeing Centuries
- "dys2p › Random Mosaic – Detecting unauthorized physical access with beans, lentils and colored rice"
Random Mosaic – Detecting unauthorized physical access with beans, lentils and colored rice
- "Surely you can be serious - by Adam Mastroianni"
Surely you can be serious
- "Hormonal doping and androgenization of athletes: a secret program of the German Democratic Republic government Clinical Chemistry"
Hormonal doping and androgenization of athletes: a secret program of the German Democratic Republic government
- "Replacing my Right Hand with AI - Erik Schluntz"
Replacing my Right Hand with AI
- "Surprising Hacker News Data Analysis — The Data Point"
Surprising Hacker News Data Analysis
- "What's The Front Page of HackerNews Worth? – Terence Edenâs Blog"
What’s The Front Page of Hacker News Worth?
- "Words growing or shrinking in Hacker News titles: a tidy analysis – Variance Explained"
Words growing or shrinking in Hacker News titles: a tidy analysis
- "Web Page Under Construction"
""
- "Matvoz.com – Interesting facts about when you get hit by Hacker News tsunami"
Interesting facts about when you get hit by Hacker News tsunami
- "Mike Dellanoce's Blog: My first 'Hacker News Effect' experience"
My first 'Hacker News Effect' experience
- "Cognitive ability and tattoos and piercings – Clear Language, Clear Mind⁠:"
Cognitive ability and tattoos and piercings
- "Organization not found"
""
- "There’s a running theme in here of programming problems LLMs solve where it’s ack..."
There’s a running theme in here of programming problems LLMs solve where it’s...
- "Early evolution of small body size in Homo floresiensis"
Early evolution of small body size in <em>Homo floresiensis</em>
- ""Page not found - Rybka Forum""
""
- "Cinzel Decorative—Google Fonts"
Cinzel Decorative
- "Elsie Swash Caps - Google Fonts"
Elsie Swash Caps
- "Bodoni Classic Deco Caps Medium : Download For Free, View Sample Text, Rating And More On Fontsgeek.Com"
Bodoni Classic Deco Caps Medium
- "Run Deep: I Put a Toaster in the Dishwasher"
I Put a Toaster in the Dishwasher
- "æå­¦ããªãï¼ã¨ã´ã¡ã³ã²ãªãªã³RETAKE"
文学フリマ＆エヴァンゲリオンRETAKE
- "NicolÃ¡s GÃ³mez Davila: An Anthology : Various"
Nicolás Gómez Davila: <em>An Anthology</em>
- "Magyar H. P. Lovecraft PortÃ¡l"
""
- "DE102007030495A1—Verwendung einer eine Kreatin-Komponente enthaltende Zusammensetzung zur Verbesserung der GedÃ¤chtnisleistung, der MerkfÃ¤higkeit, des LangzeitgedÃ¤chtnisses und zur Vorbeugung geistiger ErmÃ¼dungszustÃ¤nde"
"Verwendung einer eine Kreatin-Komponente enthaltende Zusammensetzung zur Verbesserung der Gedächtnisleistung, der Merkfähigkeit, des Langzeitgedächtnisses und zur Vorbeugung geistiger Ermüdungszustände"i
- "Interview with China MiÃ©ville"
Interview with China Miéville
- "Interview with Junot DÃ­az"
Interview with Junot Díaz
- "Ce gÃ©nÃ©rateur de WAIFU "ThisWaifuDoesNotExist" sur le forum Blabla 18-25 ans - 20-02-2019 11:52:46 - jeuxvideo.com"
Ce générateur de WAIFU "ThisWaifuDoesNotExist" sur le forum Blabla 18-25 ans - 20-02-2019 11:52:46 - jeuxvideo.com
- "Steamed Hams But It's The Confrontation From Les MisÃ©rables"
Steamed Hams But It’s The Confrontation From <em>Les Misérables</em>
- "[Touhou Vocal] Raven's Jig—Une Semaine chez les Ãcarlates (Embodiment of Scarlet Devil)"
[東方 Touhou Vocal] Raven's Jig - Une Semaine chez les Écarlates (Embodiment of Scarlet Devil)
- "ãæ±æ¹ Post-Metalã denshÅ«to—LÃ¦vateinn"
【東方 Post-Metal】 denshūto - Lævateinn
- "2017 CODE Plenary Session 2: Susan Athey, RenÃ©e Richardson Gosline, and Ron Kohavi"
2017 CODE Plenary Session 2: Susan Athey, Renée Richardson Gosline, and Ron Kohavi
- "NGE: A version of the song "Komm, sÃ¼sser Tod" will be used in _Rebuild_"
NGE: A version of the song “Komm, süsser Tod” will be used in <em>Rebuild</em>
- "ramsey nasser / Yesterday's Pixels, Today"
Yesterday’s Pixels, Today
- "3�Backstory"
3. Backstory
- "THOMAS MOYNIHAN—HOMEPAGE"
Thomas Moynihan—Homepage
- "Twitter as the embodiment of the American ethos – Daniel Frank"
Twitter as the embodiment of the American ethos
- "When Nothing Ever Goes Out of Print: Maintaining Backlist Ebooks by Teresa Elsey"
When Nothing Ever Goes Out of Print: Maintaining Backlist Ebooks
- "Dwarkesh Podcast Progress Update - by Dwarkesh Patel"
Dwarkesh Podcast Progress Update
- "NYC subway math � Erik Bernhardsson"
NYC subway math
- "NovelAI Diffusion V1 Weights Release (EN) by Anlatan Aug, 2024"
NovelAI Diffusion V1 Weights Release (EN)
- "Layla - Yassine Meskhout"
Layla
- "Can AI Scaling Continue Through 2030?—Epoch AI"
Can AI Scaling Continue Through 2030?
- "Unsong Available In Paperback - by Scott Alexander"
<em>Unsong</em> Available In Paperback
- "What the humans like is responsiveness—by Sasha Chapin"
What the humans like is responsiveness
- "Seeing Like A Network - by Rohit Krishnan"
Seeing Like A Network
- "We should not let the Earth overheat! - Casey Handmer's blog⁠"
We should not let the Earth overheat!
- "System Prompts—Anthropic"
System Prompts
- "Introducing Cerebras Inference: AI at Instant Speed — Cerebras"
Introducing Cerebras Inference: AI at Instant Speed
- "How Anthropic built Artifacts—by Gergely Orosz"
How Anthropic built Artifacts
- "    "
""
- "!!!BREAKING NEWS!!!"
""
- "Click here to continue"
""
- "Loading... | Please Wait"
""
- "Lorem Ipsum dolor sit amet..."
""
- "New Tab - Browser Name"
""
- "Page 1 of 5"
""
- "Untitled Document"
""
- "Welcome to Our Website | Home"
""
- "Welcome, [Username]!"
""
- "[Object object]"
""
- "https://www.example.com/page.html"
""
- "The History of Ancient Egypt | History.com | HISTORY"
The History of Ancient Egypt
- "Amazon.com: Best Sellers: Books"
Best Selling Books
- "BREAKING: Major scientific discovery announced - CNN.com"
Major scientific discovery announced
- "Product Name - Buy Now! - OnlineStore.com"
Product Name
- "John Smith's Blog | Latest Post: 10 Tips for Better Sleep"
10 Tips for Better Sleep
- "[2023 Update] Complete Guide to Machine Learning - TechBlog"
Complete Guide to Machine Learning (2023 Update)
- "Official Website | Taylor Swift"
Taylor Swift Official Website
- "r/AskReddit - What's the most underrated movie of all time?"
What's the most underrated movie of all time?
- "YouTube - Never Gonna Give You Up (Official Music Video)"
Never Gonna Give You Up
- "COVID-19 Updates | WHO | World Health Organization"
COVID-19 Updates
- "New Study Reveals Surprising Results (Page 1 of 3) | ScienceDaily"
New Study Reveals Surprising Results
- "Top 10 Programming Languages to Learn in 2024 - Dev.to - The Practical Developer Community"
Top 10 Programming Languages to Learn in 2024
- "Have posted this before, but it really left an impression about crows, and the b…"
Have posted this before, but it really left an impression about crows, and the…
- "I've posted this comment before but I grew up in Florida on a decent amount of l..."
I've posted this comment before but I grew up in Florida on a decent amount of...
- "Zarf Updates: Tabbed out on the Oregon Trail"
Tabbed out on the Oregon Trail
- ""AI and Compute" trend isn't predictive of what is happening"
"AI and Compute" trend isn't predictive of what is happening
- ""Are you gonna publish that?" Peer-reviewed publication outcomes of doctoral dissertations in psychology"
"Are you gonna publish that?" Peer-reviewed publication outcomes of doctoral dissertations in psychology
- ""Body of a courtesan in nine stages": A 19<sup>th</sup> century study of decomposition"
"Body of a courtesan in nine stages": A 19<sup>th</sup> century study of decomposition
- ""Body of a courtesan in 9 stages": A 19<sup>th</sup> century study of decomposition"
"Body of a courtesan in 9 stages": A 19<sup>th</sup> century study of decomposition
- ""Can We Detect Substance Use Disorder?": Knowledge and Time Aware Classification on Social Media from Darkweb"
"Can We Detect Substance Use Disorder?": Knowledge and Time Aware Classification on Social Media from Darkweb
- ""Generate" the Future of Work through AI: Empirical Evidence from Online Labor Market"
"Generate" the Future of Work through AI: Empirical Evidence from Online Labor Market
- ""I Was a Starter Wife": Inside America’s Messiest Divorce"
"I Was a Starter Wife": Inside America’s Messiest Divorce
- ""Interaction-Free" Imaging"
"Interaction-Free" Imaging
- "ChatGPT: Everything You Need to Know - OpenAI"
ChatGPT: Everything You Need to Know
- ""Is Your Brain Really Necessary?", Revisited"
"Is Your Brain Really Necessary?", Revisited
- ""Less Than One"-Shot Learning: Learning <em>n</em> Classes From <em>M</em> &lt; <em>N</em> Samples"
"Less Than One"-Shot Learning: Learning <em>n</em> Classes From <em>M</em> &lt; <em>N</em> Samples
- "Less Than One"-Shot Learning: Learning <em>n</em> Classes From <em>M</em> < <em>N</em> Samples"
Less Than One"-Shot Learning: Learning <em>n</em> Classes From <em>M</em> < <em>N</em> Samples
- ""O Uommibatto": How the Pre-Raphaelites Became Obsessed with the Wombat"
"O Uommibatto": How the Pre-Raphaelites Became Obsessed with the Wombat
- "Squigglevision"
Squigglevision
- ""Student" as Statistician"
"Student" as Statistician
- "hyphenate"
hyphenate
- "ruby"
ruby
- "sanded floor"
sanded floor
- "sunrise"
sunrise
- "text-wrap: pretty"
text-wrap: pretty
- ""One in a million" is next Tuesday"
"One in a million" is next Tuesday
- ""We Are Afraid to Even Look for Them": Enforced Disappearances in the Wake of Xinjiang’s Protests"
"We Are Afraid to Even Look for Them": Enforced Disappearances in the Wake of Xinjiang’s Protests
- ""Other-Play" for Zero-Shot Coordination"
"Other-Play" for Zero-Shot Coordination
- ""TIHKAL" - #26 LSD-25"
<em>TIHKAL</em> § #26 LSD-25
- ""[EoE] gives the same end as the TV series"?"
"[EoE] gives the same end as the TV series"?
- ""n-back" AND ("fluid intelligence" OR "IQ")—Search Results"
"n-back" AND ("fluid intelligence" OR "IQ")—Search Results
- "#147: Forging the mRNA Revolution—Katalin Karikó § Education & Ambition"
#147: Forging the mRNA Revolution—Katalin Karikó § Education & Ambition
- "#157 gscan2pdf generates large file size PDFs"
#157 gscan2pdf generates large file size PDFs
- "#3843: Merge plugins into HEAD"
#3843: Merge plugins into HEAD
- "#GPT3 gives some interesting true and false answers to some questions. But it's important to note that it gives opposite answers just as often, I cheery picked the most ‘sensational’ ones. Usually it said the opposite thing, and it also role-plays sometimes (eg. as a spy)"
#GPT3 gives some interesting true and false answers to some questions. But it's important to note that it gives opposite answers just as often, I cheery picked the most ‘sensational’ ones. Usually it said the opposite thing, and it also role-plays sometimes (eg. as a spy)
- "#bitcoin-otc"
#bitcoin-otc
- "#bitcoin-otc gpg key data"
#bitcoin-otc gpg key data
- "#gpt3 has some reasonably impressive ability not only to detect nonsense, but to explain why something is nonsensical:"
#gpt3 has some reasonably impressive ability not only to detect nonsense, but to explain why something is nonsensical:
- "%5BInt%5D_-&gt;_Int"
%5BInt%5D_-&gt;_Int
- "%5Ba%5D_-&gt;_%5Ba%5D"
%5Ba%5D_-&gt;_%5Ba%5D
- "%5Ba%5D_-&gt;_a"
%5Ba%5D_-&gt;_a
- "&lt;a&gt;: The Anchor element"
&lt;a&gt;: The Anchor element
- "&lt;abbr&gt;: The Abbreviation element"
&lt;abbr&gt;: The Abbreviation element
- "&lt;q&gt;: The Inline Quotation element"
&lt;q&gt;: The Inline Quotation element
- "&lt;small&gt;: the side comment element"
&lt;small&gt;: the side comment element
- "&lt;table&gt;: The Table element"
&lt;table&gt;: The Table element
- "'Discounting' the future cost of climate change"
'Discounting' the future cost of climate change
- "'It's the screams of the damned!' The eerie AI world of deepfake music Music"
'It's the screams of the damned!' The eerie AI world of deepfake music Music
- "'NASA-Inspired Works of Fiction:' the masses speak!"
'NASA-Inspired Works of Fiction:' the masses speak!
- "'Perplexed ... Perplexed': On Mob Justice in Nigeria"
'Perplexed ... Perplexed': On Mob Justice in Nigeria
- "'Scared straight' and other juvenile awareness programs for preventing juvenile delinquency"
'Scared straight' and other juvenile awareness programs for preventing juvenile delinquency
- "'This Person Does Not Exist' Has Spawned a Host of A.I.-Powered Copycats"
'This Person Does Not Exist' Has Spawned a Host of A.I.-Powered Copycats
- ""body of a courtesan in 9 stages": A 19<sup>th</sup> century study of decomposition"
"Body of a courtesan in 9 stages": A 19<sup>th</sup> century study of decomposition
- ""Weird Al" Yankovic—Dare To Be Stupid (Official HD Video)"
Dare To Be Stupid
- ""Less Than One"-Shot Learning: Learning <em>n</em> Classes From <em>M</em> < <em>N</em> Samples"
"Less Than One"-Shot Learning: Learning <em>n</em> Classes From <em>M</em> < <em>N</em> Samples
- "[EoE] gives the same end as the TV series?"
[EoE] gives the same end as the TV series?
- "(X-post from /r/LSD) Guess I need to say goodbye, Psychonauts"
Guess I need to say goodbye, Psychonauts
- "*Kóryos"
*Kóryos
- "02. TOKONATSU"
TOKONATSU
- "/wsr/ - Worksafe Requests » Thread #661741"
/wsr/ - Worksafe Requests § Thread #661741
- "(Yiddish) an important person, often in the negative sense of self-important; a bigwig"
(Yiddish) an important person, often in the negative sense of self-important; a bigwig
- "My impression from reading this has always been "Almost there! Keep going." If y... | Hacker News"
My impression from reading this has always been "Almost there! Keep going." If...
- "The Carnivore Bar - Eat Meat"
The Carnivore Bar
- "Choices - Joel on Software"
Choices
- "graydon2"
""
- "Vimeo / CAPTCHA Challenge⁠"
""
- "Neal Stephenson—Why I Am a Bad Correspondent"
Why I Am a Bad Correspondent
- "error - core file server"
""
- "ORCID"
""
- "This page could not be found"
""
- "Jonas Hietala: Why I still blog after 15 years"
Why I still blog after 15 years
- "Are you a robot?"
""
- "Exploring Typst, a new typesetting system similar to LaTeX—jreyesr’s blog"
"Exploring Typst, a new typesetting system similar to LaTeX"
- "Flight From Perfection � ‘Better not to begin. Once begun, better to finish!’"
"‘Better not to begin. Once begun, better to finish!’"
- "A Knight at the Opera: The Best RPG Cover of all Time" https://knightattheopera.blogspot.com/2024/05/the-best-rpg-cover-of-all-time.html
"The Best RPG Cover of all Time"
- "ScienceDirect"
""
- "Res Obscura: Seven Weeks to Venice: History Through Isochronic Maps"
"Seven Weeks to Venice: History Through Isochronic Maps"
- "Prof. Marcella Rietschel • European Platform of Women Scientists"
"Prof. Marcella Rietschel"
- "Lippmann Security—HoloWiki—A Holography FAQ"
"Lippmann Security"
- "How China Is Like the 19th Century U.S.—by Brian Potter"
"How China Is Like the 19th Century U.S."
- "Why some brothers only date whites and 'others'.-Free Online Library"
"Why some brothers only date whites and 'others'."
- "Ebony: where the men are: the 10 best cities"
"Where the men are: the 10 best cities"
- "The Old Fools by Philip Larkin"
"The Old Fools"
- "About the Pedant - A Collection of Unmitigated Pedantry"
"About the Pedant"
- "Home"
""
- "Q&amp;A—Dominic Cummings substack"
"Q&amp;A"
- "https://mikehadlow.blogspot.com/2012/05/configuration-complexity-clock.html Code rant: The Configuration Complexity Clock"
"The Configuration Complexity Clock"

Task:

Input title to clean:

- """ + target + "\"\n"

completion = client.chat.completions.create(
  model="gpt-4o-mini", # we use GPT-4 because the outputs are short, we want the highest accuracy possible, we provide a lot of examples & instructions which may overload dumber models, and reviewing for correctness can be difficult, so we are willing to spend a few pennies to avoid the risk of a lower model
  messages=[
    {"role": "system", "content": "You are a researcher and web developer, compiling a bibliography."},
    {"role": "user", "content": prompt }
  ]
)

print(completion.choices[0].message.content)
