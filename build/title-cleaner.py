#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# title-cleaner.py: remove cruft from titles of web pages like website name/domain or error messages
# Author: Gwern Branwen
# Date: 2024-06-11
# When:  Time-stamp: "2026-01-28 17:37:25 gwern"
# License: CC-0
#
# Usage: $ OPENAI_API_KEY="sk-XXX" xclip -o | python title-cleaner.py
#
# When scraping HTML web pages, using the <title> field naively gives bad results: many are covert error pages, or a fixed site-wide constant string, or uselessly vague/short, or prepend/append the site URL or the site name or a whole variety of bizarre things.
# Manually writing rules to clean them up turns out to be unmanageable due to the extreme long-tail of handling >20,000 URLs on gwern.net
# However, the badness in scraped titles is very much a "I know it when I see it" thing and can usually be done in isolation when looking just at the text.
# So that means it is feasible to call out to a LLM to clean up the title.
#
# Note: this does not attempt to add italics even where those are obviously intended/correct, due to the complexity of English italicization rules. That is handled in a separate script, </static/build/italicizer.py>. Both accept input on stdin or as an argument, thus they can be chained, like `echo "Title" | title-cleaner.py | italicizer.py`.

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
If the title has obviously been truncated, end with an ellipsis "…". Drop words (but not phrases) which are mangled by truncation and do not guess them.
Convert inline Markdown to HTML, like '*foo*' → '<em>foo</em>'; convert inline code to use '<code>'.
Convert straight quotes/apostrophes to curly quotes.
Section delimiters like '»' and '-' should be replaced by '§'; but preserve dashes inside bona fide titles.
Fix initial lowercase letters in titles. (Full titlecasing is optional.)
If the title looks good, then print out the original title.
If you are unsure how to fix it, then simply print out the original title.
Note that for additional context, the URL may be prepended; the URL often helps indicate that part of a title may be the website name, boilerplate, the author, etc.

Task examples (note that few-shot examples may include comments after hashes to help explain the logic):

- "Anton Seder’s *The Animal in Decorative Art* (1896)" # we rewrite explicit italics
Anton Seder’s <em>The Animal in Decorative Art</em> (1896)
- "If I Sleep for an Hour, 30 People Will Die - The New York Times"
If I Sleep for an Hour, 30 People Will Die
- "https://www.nytimes.com/2016/10/02/opinion/sunday/if-i-sleep-for-an-hour-30-people-will-die.html If I Sleep for an Hour, 30 People Will Die - The New York Times" # URL version
If I Sleep for an Hour, 30 People Will Die
- "https://www.deadlanguagesociety.com/p/dog-is-a-weird-word “Dog” is a weird word - by Colin Gorrie"
“Dog” is a weird word
- "404"
""
- "Gwern.net | Collecting New Socks Efficiently"
Collecting New Socks Efficiently
- "worlds of DAVID BRIN"
""
- "steve yegge · The Amazon Memo"
The Amazon Memo
- "steve yegge • The Amazon Memo"
The Amazon Memo
- "index"
""
- ""
""
- "After 92 years, millionaire miserâs heirs finally split $100M - TODAY People - TODAY.com"
After 92 years, millionaire miser’s heirs finally split $100M
- "502 Bad Gateway"
""
- "Faces2Anime: Cartoon Style Transfer in Faces using Generative Adversarial Networks. Masters Thesis 2021 @ NTUST."
Faces2Anime: Cartoon Style Transfer in Faces using Generative Adversarial Networks
- "articles"
""
- "Vercel Security Checkpoint"
""
- "Fan Is A Tool-Using AnimalâdConstruct Conference Talk"
Fan Is A Tool-Using Animal
- "CONTENTdm"
""
- "The Illustrated Retrieval Transformer â Jay Alammar â Visualizing machine learning one concept at a time."
The Illustrated Retrieval Transformer
- "Redirecting to https://now.tufts.edu/2016/07/14/moderately-reducing-calories-non-obese-people-reduces-inflammation"
""
- "Not Found - Albion - Webflow HTML website template"
""
- "https://siberiantimes.com/other/others/news/n0842-failed-cloned-dogs-no-use-to-law-enforcement-because-they-dont-obey-orders-and-hate-cold/ Siberian Times"
""
- "Stripe\\226\\128\\153s first carbon removal purchases\\nStripe logo\\nOpen mobile navigation\\nStripe logo\\nClose mobile navigation\\nPayments\\nCheckout\\nElements\\nRadar\\nStripe logo"
Stripe’s first carbon removal purchases
- "308 Permanent Redirect"
""
- "404 - Isomorphic Labs"
""
- "Page not found - PsyPost - Psychology News"
""
- "Japanese north\226\128\147south gradient in IQ predicts differences in stature, skin color, income, and homicide rate - ScienceDirect\nScienceDirect"
Japanese north–south gradient in IQ predicts differences in stature, skin color, income, and homicide rate
- "Notion – The all-in-one workspace for your notes, tasks, wikis, and databases."
""
- "It’s (Still) Really Hard for Robots to Autonomously Do Household Chores - IEEE Spectrum"
It’s (Still) Really Hard for Robots to Autonomously Do Household Chores
- "504 Gateway Time-out"
""
- "æ°æ°æ®é"
""
- "dnmarchives directory listing\nInternet Archive logo\nDonate icon" # proper noun is lowercase
<code>dnmarchives</code> directory listing
- "Page not found : Stanford University"
""
- "07"
""
- "https://www.flashback.org/sp50195930 Flashback Forum"
""
- "Stuff"
""
- "Humboldt &amp; Sonoma counties: Six arrested, 3,000 marijuana plants and 44 weapons seized in state DOJ raids - The Willits News"
Humboldt & Sonoma counties: Six arrested, 3,000 marijuana plants and 44 weapons seized in state DOJ raids
- "GoLocalPDX"
""
- "Aurora’s Approach to Development. Self-driving cars are an appliedâ¦ by The Aurora Team Aurora Blog" # obvious truncation
Aurora’s Approach to Development. Self-driving cars are an applied…
- "An open letter to Netflix from the authors of the de-anonymization paper « 33 Bits of Entropy"
An open letter to Netflix from the authors of the de-anonymization paper
- "It\226\128\153s Probably Not Lithium\nspotify-podcast-badge-wht-blk-165x40"
It’s Probably Not Lithium
- "https://vole.wtf/scunthorpe-sans/ Scunthorpe Sans \\240\\159\\151\\175\\240\\159\\154\\171 profanity-blocking font"
Scunthorpe Sans: profanity-blocking font
- "&#13;\n\tMedicine &amp; Science in Sports &amp; Exercise&#13;"
""
- "Nadia Asparouhova How to do the jhanas" # strip author
How to do the jhanas
- "Best-of-n with misaligned reward models for Math reasoning"
Best-of-n with misaligned reward models for Math reasoning
- "The Universe of Discourse : A potpourri of cool-looking scripts"
A potpourri of cool-looking scripts
- "FineWeb: decanting the web for the finest text data at scale - a Hugging Face Space by HuggingFaceFW"
FineWeb: decanting the web for the finest text data at scale
- "Do life hacks work? The truth is, weâll never know Psychology"
Do life hacks work? The truth is, we’ll never know
- "Operant Conditioning by Software Bugs – Embedded in Academia"
Operant Conditioning by Software Bugs
- "The dream of an alpine waterway - Swiss National Museum - Swiss history blog"
The dream of an alpine waterway
- "Reddit"
""
- "The Stigler Diet Problem �|� OR-Tools �"
The Stigler Diet Problem
- "why why why why why why why"
""
- "Optimized, Individualized Spaced Repetition in Hierarchical Knowledge Structures - Justin Skycak"
Optimized, Individualized Spaced Repetition in Hierarchical Knowledge Structures
- "Exclusive"
""
- "Patronage vs. Constituent Parties (Or Why Republican Party Leaders Matter More Than Democratic Ones) – The Scholar's Stage"
Patronage vs. Constituent Parties (Or Why Republican Party Leaders Matter More Than Democratic Ones)
- "Screen Media Use and Mental Health of Children and Adolescents: A Secondary Analysis of a Randomized Clinical Trial Media and Youth JAMA Network Open"
Screen Media Use and Mental Health of Children and Adolescents: A Secondary Analysis of a Randomized Clinical Trial
- "Joaquin Qui�onero Candela" # fix mojibake
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
Your Book Review: Real Raw News
- "Probably Overthinking It: There is still only one test"
There is still only one test
- "Randomly updated"
""
- "The Crystal Star Wookieepedia"
The Crystal Star
- "Abstract Heresies: Not Lisp again...."
Not Lisp again…
- "Morbid attraction to leopard urine in Toxoplasma-infected chimpanzees - ScienceDirect"
Morbid attraction to leopard urine in Toxoplasma-infected chimpanzees
- "EP021 - Bulbapedia, the community-driven PokÃ©mon encyclopedia"
EP021
- "Your Book Review: Two Arms and a Head - Astral Codex Ten"
Your Book Review: Two Arms and a Head
- "Redirecting"
""
- "From Bing to Sydney – Stratechery by Ben Thompson"
From Bing to Sydney
- "Seeing Centuries - by RJ Andrews - Chartography"
Seeing Centuries
- "https://dys2p.com/en/2021-12-tamper-evident-protection.html#kurzzeitige-lagerung dys2p › Random Mosaic – Detecting unauthorized physical access with beans, lentils and colored rice"
Random Mosaic—Detecting unauthorized physical access with beans, lentils and colored rice
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
My first ‘Hacker News Effect’ experience
- "Cognitive ability and tattoos and piercings – Clear Language, Clear Mind⁠:"
Cognitive ability and tattoos and piercings
- "Organization not found"
""
- "There’s a running theme in here of programming problems LLMs solve where it’s ack..."
There’s a running theme in here of programming problems LLMs solve where it’s…
- "Early evolution of small body size in Homo floresiensis"
Early evolution of small body size in Homo floresiensis
- "\"Page not found - Rybka Forum\""
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
Nicolás Gómez Davila: An Anthology
- "Magyar H. P. Lovecraft PortÃ¡l"
""
- "DE102007030495A1—Verwendung einer eine Kreatin-Komponente enthaltende Zusammensetzung zur Verbesserung der GedÃ¤chtnisleistung, der MerkfÃ¤higkeit, des LangzeitgedÃ¤chtnisses und zur Vorbeugung geistiger ErmÃ¼dungszustÃ¤nde"
Verwendung einer eine Kreatin-Komponente enthaltende Zusammensetzung zur Verbesserung der Gedächtnisleistung, der Merkfähigkeit, des Langzeitgedächtnisses und zur Vorbeugung geistiger Ermüdungszustände
- "Interview with China MiÃ©ville"
Interview with China Miéville
- "Interview with Junot DÃ­az"
Interview with Junot Díaz
- "https://www.jeuxvideo.com/forums/42-51-58995411-1-0-1-0-ce-generateur-de-waifu-thiswaifudoesnotexist.htm Ce gÃ©nÃ©rateur de WAIFU "ThisWaifuDoesNotExist" sur le forum Blabla 18-25 ans - 20-02-2019 11:52:46 - jeuxvideo.com"
Ce générateur de WAIFU "ThisWaifuDoesNotExist" sur le forum Blabla 18-25 ans - 20-02-2019 11:52:46
- "Steamed Hams But It's The Confrontation From Les MisÃ©rables"
Steamed Hams But It’s The Confrontation From Les Misérables
- "[Touhou Vocal] Raven's Jig—Une Semaine chez les Ãcarlates (Embodiment of Scarlet Devil)"
[Touhou Vocal] Raven’s Jig - Une Semaine chez les Écarlates (Embodiment of Scarlet Devil)
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
- "https://www.ymeskhout.com/p/layla Layla - Yassine Meskhout"
Layla
- "Can AI Scaling Continue Through 2030?—Epoch AI"
Can AI Scaling Continue Through 2030?
- "Unsong Available In Paperback - by Scott Alexander"
Unsong Available In Paperback
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
Home
- "Welcome, [Username]!"
""
- "[Object object]"
""
- "https://www.example.com/page.html"
""
- "The History of Ancient Egypt | History.com | HISTORY"
The History of Ancient Egypt
- "Amazon.com: Best Sellers: Books"
Best Sellers: Books
- "BREAKING: Major scientific discovery announced - CNN.com"
Major scientific discovery announced
- "Product Name - Buy Now! - OnlineStore.com"
Product Name
- "John Smith's Blog | Latest Post: 10 Tips for Better Sleep"
10 Tips for Better Sleep
- "[2023 Update] Complete Guide to Machine Learning - TechBlog"
[2023 Update] Complete Guide to Machine Learning
- "r/AskReddit - What's the most underrated movie of all time?"
What’s the most underrated movie of all time?
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
I’ve posted this comment before but I grew up in Florida on a decent amount of…
- "Zarf Updates: Tabbed out on the Oregon Trail"
Tabbed out on the Oregon Trail
- ""AI and Compute" trend isn't predictive of what is happening"
“AI and Compute” trend isn’t predictive of what is happening
- ""Are you gonna publish that?" Peer-reviewed publication outcomes of doctoral dissertations in psychology"
“Are you gonna publish that?” Peer-reviewed publication outcomes of doctoral dissertations in psychology
- ""Body of a courtesan in nine stages": A 19<sup>th</sup> century study of decomposition"
“Body of a courtesan in nine stages”: A 19<sup>th</sup> century study of decomposition
- ""Can We Detect Substance Use Disorder?": Knowledge and Time Aware Classification on Social Media from Darkweb"
“Can We Detect Substance Use Disorder?”: Knowledge and Time Aware Classification on Social Media from Darkweb
- ""Generate" the Future of Work through AI: Empirical Evidence from Online Labor Market"
“Generate” the Future of Work through AI: Empirical Evidence from Online Labor Market
- ""I Was a Starter Wife": Inside America’s Messiest Divorce"
“I Was a Starter Wife”: Inside America’s Messiest Divorce
- ""Interaction-Free" Imaging"
“Interaction-Free” Imaging
- "ChatGPT: Everything You Need to Know - OpenAI"
ChatGPT: Everything You Need to Know
- ""Is Your Brain Really Necessary?", Revisited"
“Is Your Brain Really Necessary?”, Revisited
- ""Less Than One"-Shot Learning: Learning <em>n</em> Classes From <em>M</em> &lt; <em>N</em> Samples"
“Less Than One”-Shot Learning: Learning <em>n</em> Classes From <em>M</em> < <em>N</em> Samples
- ""O Uommibatto": How the Pre-Raphaelites Became Obsessed with the Wombat"
“O Uommibatto”: How the Pre-Raphaelites Became Obsessed with the Wombat
- "Squigglevision"
Squigglevision
- ""Student" as Statistician"
“Student” as Statistician
- "hyphenate" # code title
<code>hyphenate</code>
- "ruby" # code title
<code>ruby</code>
- "sanded floor"
Sanded floor
- "sunrise"
Sunrise
- "text-wrap: pretty" # code title
<code>text-wrap: pretty</code>
- ""One in a million" is next Tuesday"
“One in a million” is next Tuesday
- ""We Are Afraid to Even Look for Them": Enforced Disappearances in the Wake of Xinjiang’s Protests"
“We Are Afraid to Even Look for Them”: Enforced Disappearances in the Wake of Xinjiang’s Protests
- ""Other-Play" for Zero-Shot Coordination"
“Other-Play” for Zero-Shot Coordination
- ""TIHKAL" - #26 LSD-25"
TIHKAL § #26 LSD-25
- ""[EoE] gives the same end as the TV series"?"
“[EoE] gives the same end as the TV series”?
- ""n-back" AND ("fluid intelligence" OR "IQ")—Search Results"
"n-back" AND ("fluid intelligence" OR "IQ")—Search Results
- "#147: Forging the mRNA Revolution—Katalin Karikó § Education & Ambition" # NOTE: "#147" is an ID, not a section.
#147: Forging the mRNA Revolution—Katalin Karikó § Education & Ambition
- "#157 gscan2pdf generates large file size PDFs"
#157 gscan2pdf generates large file size PDFs
- "#3843: Merge plugins into HEAD"
#3843: Merge plugins into HEAD
- "#GPT3 gives some interesting true and false answers to some questions. But it's important to note that it gives opposite answers just as often, I cheery picked the most ‘sensational’ ones. Usually it said the opposite thing, and it also role-plays sometimes (eg. as a spy)"
#GPT3 gives some interesting true and false answers to some questions. But it’s important to note that it gives opposite answers just as often, I cheery picked the most ‘sensational’ ones. Usually it said the opposite thing, and it also role-plays sometimes (eg. as a spy)
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
<code>&lt;a&gt;</code>: The Anchor element
- "&lt;abbr&gt;: The Abbreviation element"
<code>&lt;abbr&gt;</code>: The Abbreviation element
- "&lt;q&gt;: The Inline Quotation element"
<code>&lt;q&gt;</code>: The Inline Quotation element
- "&lt;small&gt;: the side comment element"
<code>&lt;small&gt;</code>: the side comment element
- "&lt;table&gt;: The Table element"
<code>&lt;table&gt;</code>: The Table element
- "'Discounting' the future cost of climate change"
‘Discounting’ the future cost of climate change
- "'It's the screams of the damned!' The eerie AI world of deepfake music Music"
‘It’s the screams of the damned!’ The eerie AI world of deepfake music
- "'NASA-Inspired Works of Fiction:' the masses speak!"
‘NASA-Inspired Works of Fiction’: the masses speak!
- "'Perplexed ... Perplexed': On Mob Justice in Nigeria"
‘Perplexed … Perplexed’: On Mob Justice in Nigeria
- "'Scared straight' and other juvenile awareness programs for preventing juvenile delinquency"
‘Scared straight’ and other juvenile awareness programs for preventing juvenile delinquency
- "'This Person Does Not Exist' Has Spawned a Host of A.I.-Powered Copycats"
‘This Person Does Not Exist’ Has Spawned a Host of A.I.-Powered Copycats
- ""Weird Al" Yankovic—Dare To Be Stupid (Official HD Video)"
Dare To Be Stupid
- "[EoE] gives the same end as the TV series?"
[EoE] gives the same end as the TV series?
- "(X-post from /r/LSD) Guess I need to say goodbye, Psychonauts"
Guess I need to say goodbye, Psychonauts
- "*Kóryos"
*Kóryos
- "02. TOKONATSU"
TOKONATSU
- "/wsr/ - Worksafe Requests » Thread #661741" # NOTE: "Worksafe Requests" is a name for the abbreviation '/wsr/', not a sub-section
/wsr/—Worksafe Requests § Thread #661741
- "(Yiddish) an important person, often in the negative sense of self-important; a bigwig"
(Yiddish) an important person, often in the negative sense of self-important; a bigwig
- "My impression from reading this has always been "Almost there! Keep going." If y... | Hacker News"
My impression from reading this has always been “Almost there! Keep going.” If…
- "The Carnivore Bar - Eat Meat"
The Carnivore Bar
- "Choices - Joel on Software"
Choices
- "https://graydon2.dreamwidth.org/193447.html graydon2" # clearly the blog title/author name, which is set on every page and so is junk
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
Exploring Typst, a new typesetting system similar to LaTeX
- "Flight From Perfection � ‘Better not to begin. Once begun, better to finish!’"
‘Better not to begin. Once begun, better to finish!’
- "https://knightattheopera.blogspot.com/2024/05/the-best-rpg-cover-of-all-time.html A Knight at the Opera: The Best RPG Cover of all Time"
The Best RPG Cover of all Time
- "ScienceDirect"
""
- "Res Obscura: Seven Weeks to Venice: History Through Isochronic Maps"
Seven Weeks to Venice: History Through Isochronic Maps
- "Prof. Marcella Rietschel • European Platform of Women Scientists"
Prof. Marcella Rietschel
- "Lippmann Security—HoloWiki—A Holography FAQ"
Lippmann Security
- "How China Is Like the 19th Century U.S.—by Brian Potter"
How China Is Like the 19th Century U.S.
- "Why some brothers only date whites and 'others'.-Free Online Library"
Why some brothers only date whites and ‘others’
- "Ebony: where the men are: the 10 best cities"
Where the men are: the 10 best cities
- "The Old Fools by Philip Larkin"
The Old Fools
- "About the Pedant - A Collection of Unmitigated Pedantry"
About the Pedant
- "https://example.com/ Home" # not site-wide boiler plate, so keep
Home
- "https://example.com/history-of-our-company Home" # site-wide boiler plate, so delete
""
- "Q&amp;A—Dominic Cummings Substack"
Q&A
- "https://mikehadlow.blogspot.com/2012/05/configuration-complexity-clock.html Code rant: The Configuration Complexity Clock"
The Configuration Complexity Clock
- "Anton Seder's *The Animal in Decorative Art* (1896) - The Public Domain Review"
Anton Seder’s <em>The Animal in Decorative Art</em> (1896)
- "Review of *The Ignatz*"
Review of <em>The Ignatz</em>
- "Katri RÃ¤ikkÃ¶nen"
Katri Räikkönen
- "Sampling with SQL—Tom Moertel’s Blog"
Sampling with SQL
- "The Dunning-Kruger Effect is Autocorrelation—Economics from the Top Down https://economicsfromthetopdown.com/2022/04/08/the-dunning-kruger-effect-is-autocorrelation/"
The Dunning-Kruger Effect is Autocorrelation
- "https://www.theguardian.com/artanddesign/gallery/2024/nov/16/what-a-carve-up-playful-intricate-japanese-leaf-art-in-pictures What a carve up! Playful, intricate Japanese leaf art—in pictures | Art and design"
What a carve up! Playful, intricate Japanese leaf art—in pictures
- "index.utf8"
""
- "Freudenthal - Link error"
""
- "https://jenn.site/2024/11/you-are-pablo-neruda-it-is-dawn-at-isla-negra-in-1968-matilde-is-asleep-in-your-bed-write-what-comes/ The Neruda Factory—Jenneral HQ"
The Neruda Factory
- "Eric Tang �" # strip mojibake which can't be fixed
Eric Tang
- "lamag.com https://lamag.com/books/harlan-ellison-last-words-dangerous-visions-sci-fi-writer-posthumous-comeback"
""
- "The V*mpire - Reactor https://reactormag.com/the-vampire-p-h-lee/"
The V*mpire
- "Axe Handles by Gary Snyder"
Axe Handles
- "PStricks : applications: The marbled paper with PSTricks: adaptation of the works of Aubrey Jaffer"
The marbled paper with PSTricks: adaptation of the works of Aubrey Jaffer
- "https://nyuu.page/essays/solidity/ nyuu dot page"
""
- "https://perma.cc/9L3D-7ELY Perma"
""
- "https://justinpombrio.net/2020/01/25/survey.html Justin Pombrio"
""
- "https://blog.otoro.net/2017/11/12/evolving-stable-strategies/ ōtoro.net"
""
- "Qitmir (dog&232;)"
Qitmir (dog)
- "Research Scientist"
""
- "http://jpkoning.blogspot.com/2024/12/after-twelve-years-of-writing-about.html Moneyness: After 12 years of writing about bitcoin, here’s how my thinking has changed"
After 12 years of writing about bitcoin, here’s how my thinking has changed
- "Spotmicro—robot dog by KDY0523—Thingiverse"
Spotmicro—robot dog
- "A Very Private Life—Nikolai Tolstoy Remembers Patrick O’Brian by Unseen Histories - Unseen Histories"
A Very Private Life—Nikolai Tolstoy Remembers Patrick O’Brian
- "“AI” on a Calculator: Part 1 | Blog"
“AI” on a Calculator: Part 1
- "File:Ascii art alphabet.png - Wikimedia Commons"
File:Ascii art alphabet.png
- "Prompt Design for DALL·E: Photorealism—Emulating Reality | by Merzmensch | Merzazine"
Prompt Design for DALL·E: Photorealism—Emulating Reality
- "‘Twelfth Night Till Candlemas’—the story of a forty-year book-quest and of its remarkable ending — The Law and Policy Blog"
‘Twelfth Night Till Candlemas’—the story of a forty-year book-quest and of its remarkable ending
- "I’ve acquired a new superpower—Daniel Wirtz"
I’ve acquired a new superpower
- "https://olup-blog.pages.dev/stories/image-detection-cars Olup Stories"
""
- "https://machinamenta.blogspot.com/2013/06/tolkien-and-darwin-among-machines.html Machinamenta: Tolkien and Darwin Among the Machines"
Tolkien and Darwin Among the Machines
- "Machinamenta: Forming Extended Analogies with GPT-3 https://machinamenta.blogspot.com/2020/08/forming-extended-analogies-with-gpt-3.html"
Forming Extended Analogies with GPT-3
- "https://www.blog.radiator.debacle.us/2012/07/thief-1s-assassins-and-environmental.html Radiator Blog: <em>Thief 1</em>’s “Assassins” and environmental storytelling"
<em>Thief 1</em>’s “Assassins” and environmental storytelling
- "John Tromp HomePage"
John Tromp Homepage
- "https://wiremodal.net/cwt Wiremodal"
""
- "http://habitatchronicles.com/2007/03/the-untold-history-of-toontowns-speedchat-or-blockchattm-from-disney-finally-arrives/ Habitat Chronicles: The Untold History of Toontown’s SpeedChat (or BlockChattm from Disney finally arrives)"
The Untold History of Toontown’s SpeedChat (or BlockChat™ from Disney finally arrives)
- "Jasmine Sun on Substack: “For the First Time—And It Brings Me No Joy to Admit This” https://substack.com/@jasmine/note/c-88814858"
For the First Time—And It Brings Me No Joy to Admit This
- "https://www.sergey.fyi/articles/gemini-flash-2 Sergey’s Blog"
""
- "https://avmajournals.avma.org/view/journals/javma/229/6/javma.229.6.964.xml Aromatherapy for travel-induced excitement in dogs in: <em>Journal of the American Veterinary Medical Association</em> Volume 229 Issue 6"
Aromatherapy for travel-induced excitement in dogs
- "https://chiraaggohel.com/posts/llms-eda/ Your AI can’t see gorillas—Chiraag Gohel"
Your AI can’t see gorillas
- "https://kenbertagnolli.com/2025/02/09/how-we-achieved-a-1000x-improvement-in-performance/ Three Orders of Magnitude: Transforming PDC Technology at US Synthetic - Ken Bertagnolli"
Three Orders of Magnitude: Transforming PDC Technology at US Synthetic
- "https://aresluna.org/the-hardest-working-font-in-manhattan/ The hardest working font in Manhattan—Aresluna"
The hardest working font in Manhattan
- "Are you a robot?"
""
- "https://ericneyman.wordpress.com/2021/06/05/social-behavior-curves-equilibria-and-radicalism/ Social behavior curves, equilibria, and radicalism - Unexpected Values"
Social behavior curves, equilibria, and radicalism
- "http://www.iqscorner.com/2007/05/temp.html?m=1 IQ’s Corner: IQ and wealth: The dumb rich and the smart poor"
IQ and wealth: The dumb rich and the smart poor
- "https://www.inceptionlabs.ai/news Inception Labs"
""
- "https://www.mathworks.com/help/images/comparison-of-auto-white-balance-algorithms.html Access Denied"
""
- "https://www.novonordisk-us.com/media/news-archive/news-details.html?id=915288 News details"
""
- "https://deviantabstraction.com/2024/05/02/analyzing-poems-with-llm/ Analyzing poems with LLMs - Deviant/Abstraction"
Analyzing poems with LLMs
- "https://archive.org/details/Computer_Games_Vol_3_No_2_1984-06_Carnegie_Publications_US/page/n34/mode/1up Computer Games-Vol 3 No 2 (1984-06)(Carnegie Publications)(US)"
Computer Games: Vol 3 No 2 (1984-06) (Carnegie Publications) (US)
- "http://news.bbc.co.uk/2/hi/uk_news/7954876.stm UK"
""
- "https://archive.org/details/innocentassassin00kurt <em>The innocent assassins : biological essays on life in the present and distant past</em> : Kurtén, Björn"
<em>The innocent assassins: biological essays on life in the present and distant past</em>, Kurtén, Björn
- "https://www.talkchess.com/forum/viewtopic.php?t=48733 Scaling at 2× nodes (or doubling time control).—TalkChess.com"
Scaling at 2× nodes (or doubling time control).
- "https://reddit.com/r/EuropeanCulture/comments/ozpsxv/inside_intourist_robert_a_heinlein_1960/ Heart of the internet"
""
- "https://worksinprogress.co/issue/steam-networks/ Steam networks—Works in Progress"
Steam networks
- "http://sl4.org/archive/0707/16399.html SL4: A very surreal day"
A very surreal day
- "https://www.unimedizin-mainz.de/imbei/en/biometrie-genomische-statistik-und-bioinformatik/mitarbeiter/prof-dr-rer-nat-konstantin-strauch.html IMBEI"
""
- "https://naml.us/ naml.us"
""
- "https://thecritic.co.uk/the-unmaking-of-the-athenaeum/ The unmaking of the Athenaeum | Richard Davenport-Hines"
The unmaking of the Athenaeum
- "https://scholars-stage.blogspot.com/2020/06/against-patrick-deneen-ii.html Redirecting The Scholar’s Stage"
""
- "https://ethw.org/Oral-History:William_Aspray Oral-History:William Aspray - Engineering and Technology History Wiki"
Oral-History:William Aspray
- "https://www.greaterwrong.com/tag/ai-takeoff AI Takeoff tag—LessWrong"
AI Takeoff tag
- "https://ethanzuckerman.com/2011/04/06/those-white-plastic-chairs-the-monobloc-and-the-context-free-object/ Those White Plastic Chairs—The Monobloc and the Context-Free Object - Ethan Zuckerman"
Those White Plastic Chairs—The Monobloc and the Context-Free Object
- "https://antsofgodarequeerfish.blogspot.com/2012/04/illustration-for-euremas-dam.html The Ants Of God Are Queer Fish: Illustration for Eurema’s Dam!"
Illustration for Eurema’s Dam!
- "https://www.ralafferty.org/works/collections/nine-hundred-grandmothers/ R. A. Lafferty, <em>Nine Hundred Grandmothers</em>—Science Fiction, Fantasy, Philosophy &amp; History"
R. A. Lafferty, <em>Nine Hundred Grandmothers</em>
- "https://www.rug.nl/umcg/research/departments/epidemiology/staff/behrooz-alizadeh Research units"
""
- "https://thesilverelves.blogspot.com/2019/02/who-were-elf-queens-daughters.html The Silver Elves: Who Were the Elf Queen’s Daughters?"
Who Were the Elf Queen’s Daughters?
- "https://www.bloomberg.com/news/articles/2025-03-18/openai-s-first-stargate-site-to-hold-up-to-400-000-nvidia-chips Bloomberg—Are you a robot?"
""
- "https://mattlakeman.org/2025/03/24/conquest-of-the-incas/ <em>Conquest of the Incas</em>—Matt Lakeman"
<em>Conquest of the Incas</em>
- "https://www.aizk.sh/posts/reflecting-on-wikitok Aizk’s Site"
""
- "https://www.upjs.sk/en/LF/employee/alena.yaluri/ Personal profile - UPJS Kosice"
""
- "https://www.secondperson.dating/p/navigation-by-moonlight#%C2%A7the-lunar-arts Navigation by Moonlight—by Jacob Falkovich"
Navigation by Moonlight
- "https://dailynous.com/2023/05/25/am-i-the-unethical-one-a-philosophy-professor-his-cheating-students/ “Am I the unethical one?” A Philosophy Professor &amp; His Cheating Students - <em>Daily Nous</em>"
“Am I the unethical one?” A Philosophy Professor & His Cheating Students
- "https://medium.com/@julianmckinlay/total-war-rome-ii-and-creative-assembly-my-statement-ten-years-on-d964f65b0a8f <em>Total War: ROME II</em> and Creative Assembly—My Statement 10 Years On | by Julian McKinlay"
<em>Total War: ROME II</em> and Creative Assembly—My Statement 10 Years On
- "https://waskstudio.com/products/blasphemail-postage-stamps Blasphemail Postage Stamps—Wask"
Blasphemail Postage Stamps
- "https://web.archive.org/web/20240827052509/https://www.edwardtufte.com/tufte/gould Edward Tufte: New ET Writings, Artworks &amp; News"
""
- "https://fi-le.net/byzantine/ fi-le.net"
""
- "https://www.geneticsnetworkamsterdam.org/personal-pages/personal-pages-karin-verweij/ Personal Page | Prof. dr. Karin Verweij"
Prof. dr. Karin Verweij
- "https://yuxi-liu-wired.github.io/essays/posts/cyc/ Cyc—Yuxi on the Wired"
Cyc
- "https://www.modelshipsinthecinema.com/2016/12/hunt-for-red-october-1990.html model ships in the cinema: <em>Hunt for Red October</em> 1990"
<em>Hunt for Red October</em> 1990
- "https://www.bigsandwoodworking.com/kezurou-kai-39/ Kezurou-kai #39 - Big Sand Woodworking"
Kezurou-kai #39
- "https://www.emigre.com/Fonts/Lo-Res-Monospaced Emigre: Lo-Res Monospaced Font Family"
Lo-Res Monospaced Font Family
- "https://davoortwilbo.blogspot.com/2015/07/2005-interview-i-did-with-gene-wolfe.html David T. Wilbanks: 2005 interview I did with Gene Wolfe for <em>Hellnotes</em>"
2005 interview I did with Gene Wolfe for <em>Hellnotes</em>
- "https://www.sfsite.com/fsf/2007/gwms0704.htm Fantasy and Science Fiction" # example of a site which slaps the same title on every page, despite the URL obviously not being just that
""
- "https://www.asimov.press/p/obit Eulogy to the <em>Obits</em>—by Alexandra Balwit—Asimov Press"
Eulogy to the <em>Obits</em>
- "https://sampatt.com/blog/2025-04-28-can-o3-beat-a-geoguessr-master Sam Patterson"
""
- "https://www.ft.com/content/c04389a3-c672-43ce-8d9e-724668c0e490 Subscribe to read"
""
- "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5216904 Shifting Work Patterns with Generative AI by Eleanor Wiske Dillon, Sonia Jaffe, Nicole Immorlica, Christopher Stanton :: SSRN"
Shifting Work Patterns with Generative AI
- "https://rickiheicklen.com/unparalleled-misalignments.html Ricki Heicklen"
""
- "https://languagelog.ldc.upenn.edu/nll/?p=3546 Language Log"
""
- "https://adamleblanc.page/portfolio/schist01/ Schist01: A wrist-mounted keyboard prototype—Adam LeBlanc"
Schist01: A wrist-mounted keyboard prototype
- "https://www.stanwinstonschool.com/blog/jurassic-park-evolution-of-a-raptor-suit Stan Winston School of Character Arts"
""
- "https://www.rifters.com/crawl/?p=11511 No Moods, Ads or Cutesy Fucking Icons"
""
- "https://robhorning.substack.com/p/font-activations Font activations—by Rob Horning—Internal exile"
Font activations
- "https://blog.blackhc.net/2025/05/active-learning-vs-filtering/ Active Learning vs. Data Filtering:<br />Selection vs. Rejection"
Active Learning vs. Data Filtering: Selection vs. Rejection
- "https://calvinandhobbes.fandom.com/wiki/Noodle_Incident Noodle Incident | The Calvin and Hobbes Wiki"
Noodle Incident
- "https://interrobangtype.co/product/wt-morris-roman/ WT Morris Roman at Interrobang Type Co"
WT Morris Roman
- "https://www.bloomberg.com/news/articles/2025-01-27/x-debt-shopped-with-6-billion-sweetener-from-elon-musk-s-ai-bet Bloomberg"
""
- "https://www.crd.york.ac.uk/PROSPERO/view/CRD42022342845 PROSPERO"
""
- "https://forbetterscience.com/2025/05/19/a-sting-inside-a-papermill/ A Sting Inside a Papermill - For Better"
A Sting Inside a Papermill
- "https://archive.is/sK15b archive.is"
""
- "https://resobscura.substack.com/p/why-were-belle-epoque-cities-beautiful Why were Belle Époque cities beautiful? — by Benjamin Breen"
Why were Belle Époque cities beautiful?
- "https://willstorr.substack.com/p/scamming-substack?open=false#%C2%A7anonymous-author Scamming Substack?—You Are a Story with Will Storr"
Scamming Substack?
- "https://www.isegoria.net/2025/05/his-research-started-with-sixteen-albino-mice/ His research started with 16 albino mice—Isegoria"
His research started with 16 albino mice
- "https://www.lesswrong.com/posts/9rKWm8BzTYAiCCFjx/musings-on-ai-companies-of-2025-2026-jun-2025 Musings on AI Companies of 2025–2026 (Jun 2025)"
Musings on AI Companies of 2025–2026 (Jun 2025)
- "https://zpravy.aktualne.cz/domaci/jirikovsky-blazek-bitcoin-nucleus/r~88b8e58e421611f0bb77ac1f6b220ee8/ Největší dealer fentanylu či zbraně. Odkud přišla Blažkova “ultralegální” miliarda—Aktuálně"
Největší dealer fentanylu či zbraně. Odkud přišla Blažkova “ultralegální” miliarda
- "https://tasvideos.org/8214S #8214: Sniq &amp; NobodyNada’s SNES <em>Super Metroid</em> “game end glitch” in 06:12.75—Submission #8214—TASVideos"
#8214: Sniq & NobodyNada’s SNES <em>Super Metroid</em> “game end glitch” in 06:12.75—Submission #8214
- "Spegel: A Terminal Browser That Uses LLMs to Rewrite Webpages - SimEdw’s Blog"
Spegel: A Terminal Browser That Uses LLMs to Rewrite Webpages
- "https://www.yahoo.com/lifestyle/women-like-mens-hands-viral-twitter-thread-proof-170709392.html Yahoo" # the actual title is "Women like men’s hands, and this viral Twitter thread is proof", as expected, but we should NOT guess that, and just "Yahoo" → "" (since clearly this is not about Yahoo per se, it)
""
- "https://www.inc.com/sam-blum/exclusive-scale-ais-spam-security-woes-while-serving-google/91205895 inc.com"
""
- "https://www.theintrinsicperspective.com/p/they-die-every-day “They Die Every Day”—by Erik Hoel"
“They Die Every Day”
- "hypercapitalism and the AI talent wars—by John Luttig"
Hypercapitalism and the AI talent wars
- "DIYPC and server bifurcation @ osmarks’ website"
DIYPC and server bifurcation
- "https://www.mrob.com/pub/ries/index.html <code>ries</code>: Find Algebraic Equations, Given Their Solution at MROB"
<code>ries</code>: Find Algebraic Equations, Given Their Solution
- "https://emilkirkegaard.dk/en/2025/07/iq-is-the-most-predictive-variable-in-social-science/ IQ is the most predictive variable in social science*—Clear Language, Clear Mind"
IQ is the most predictive variable in social science
- "https://www.ipr.northwestern.edu/news/2024/an-existential-crisis-for-science.html ‘An Existential Crisis’ for Science: Institute for Policy Research - Northwestern University"
‘An Existential Crisis’ for Science
- "https://www.fosskers.ca/en/blog/contributing-to-emacs Colin Woodbury"
""
- "https://www.james-zou.com/ Machine Learning"
""
- "https://snarkmarket.com/blog/snarkives/briefly_noted/the_name_is_shrdlu_etaoin_shrdlu/#067911 Snarkmarket: The Name is Shrdlu… Etaoin Shrdlu"
The Name is Shrdlu… Etaoin Shrdlu
- "https://experts.umich.edu/8949-ida-surakka Discovery"
""
- "https://www.ft.com/content/cd1a0729-a8ab-41e1-a4d2-8907f4c01cac Client Challenge"
""
- "https://xcancel.com/tszzl/status/1955073047700066485 X Cancelled"
""
- "https://developers.googleblog.com/en/announcing-imagen-4-fast-and-imagen-4-family-generally-available-in-the-gemini-api/ Announcing Imagen 4 Fast and the generally availability of the Imagen 4 family in the Gemini API - Google Developers Blog"
Announcing Imagen 4 Fast and the generally availability of the Imagen 4 family in the Gemini API
- "https://minusx.ai/blog/decoding-claude-code/ Minusx"
""
- "https://marianogappa.github.io/software/2025/08/24/i-made-two-card-games-in-go/ Mariano Gappa’s Blog"
""
- "https://dmitrybrant.com/2025/09/07/using-claude-code-to-modernize-a-25-year-old-kernel-driver Using Claude Code to modernize a 25-year-old kernel driver - Dmitry Brant"
Using Claude Code to modernize a 25-year-old kernel driver
- "https://www.hamidi.us/products/arabian-safari-cpo-10ml HAMIDI USA"
""
- "”The Smell of Space” Fragrance, 100ml"
“The Smell of Space” Fragrance, 100ml
- "https://geoff.greer.fm/2024/07/07/making-my-own-wedding-rings/ Geoff Greer’s site: Making My Own Wedding Rings"
Making My Own Wedding Rings
- "https://wesmckinney.com/blog/apache-arrow-pandas-internals/ Wes McKinney—Apache Arrow and the “10 Things I Hate About pandas”"
Apache Arrow and the “10 Things I Hate About pandas”
- "https://chrismorgan.info/blog/2019-website/ A new website - an article by Chris Morgan"
A new website
- "https://firstknownwhenlost.blogspot.com/2011/07/there-is-door-i-have-shut-until-end-of.html First Known When Lost: “There Is A Door I Have Shut Until The End Of The World”"
“There Is A Door I Have Shut Until The End Of The World”
- "https://mjg59.dreamwidth.org/73317.html mjg59"
""
- "https://scottaaronson.blog/?p=9183 Shtetl-Optimized"
""
- "https://www.betonit.ai/p/aging-out-of-drug-addiction-is-the “Aging Out” of Drug Addiction is the Norm—by Bryan Caplan"
“Aging Out” of Drug Addiction is the Norm
- "Rolling the Dice with CSS random()"
Rolling the Dice with CSS <code>random()</code>
- "https://genius.com/Tom-lehman-money-trees-rap-genius-response-to-heroku-annotated Tom Lehman—Money Trees (Rap Genius Response to Heroku)"
Money Trees (Rap Genius Response to Heroku)
- "https://verygoods.co/smallness Very Goods"
""
- "https://signalsandthreads.com/building-tools-for-traders/ Signals and Threads"
""
- "https://funcall.blogspot.com/2025/10/a-self-actualized-llm-ai-content.html Abstract Heresies: A Self-actualized LLM (AI content) (A blog about computers, functional languages, Lisp, and Scheme.)"
A Self-actualized LLM (AI content)
- "Abstract Heresies: Why LLMs Suck at Lisp (A blog about computers, functional languages, Lisp, and Scheme.)"
Why LLMs Suck at Lisp
- "https://funcall.blogspot.com/2025/08/llm-in-debugger.html Abstract Heresies: LLM in the Debugger (A blog about computers, functional languages, Lisp, and Scheme.)"
LLM in the Debugger
- "https://funcall.blogspot.com/2025/08/challenges-of-pseudocode-expansion.html Abstract Heresies: Challenges of Pseudocode Expansion (A blog about computers, functional languages, Lisp, and Scheme.)"
Challenges of Pseudocode Expansion
- "https://yalereview.org/article/working-for-patricia-highsmith The Yale Review"
""
- "https://www.michaeleisen.org/blog/?p=1217 Michael Eisen | Published: November 29, 2012"
""
- "https://archive.ph/9no3M archive.ph"
""
- "https://renato.athaydes.com/posts/revisiting-prechelt-paper-comparing-languages.html Renato Athaydes"
""
- "https://ian.mccowan.space/study/essays/roguelikes/ small clever rooms: 10 Thousand Lifetimes with Roguelikes"
10 Thousand Lifetimes with Roguelikes
- "https://research.swtch.com/nih research!rsc: Running the “Reflections on Trusting Trust” Compiler"
Running the “Reflections on Trusting Trust” Compiler
- "https://www.debbiemaddy.com/blogs/musings-of-a-fiber-fanatic/kakishibu-dyeing-in-seattle Kakishibu Dyeing in Seattle — Debbie Maddy"
Kakishibu Dyeing in Seattle
- "Hitler Did NOT Have Kallmann Syndrome — Keith Woods"
Hitler Did NOT Have Kallmann Syndrome
- "How To Tell Youre In Base Reality" # fix obvious typo
How To Tell You’re In Base Reality
- "Wages under superintelligence - by Zachary Brown"
Wages under superintelligence
- "https://github.com/Wyattwalls/system_prompts/blob/c22fdaf611cc8224a511f6ea650b30ccc89b0580/OpenAI/gpt-5.2-thinking-20251213 Rate limit"
""
- "https://www.abcb.com/newspaper/1999-12-30_asahi_01.htm The Anime Cafe"
""
- "https://life-in-a-monospace-typeface.tumblr.com/post/800786180245504000/the-shibari-game life in a monospace typeface"
""
- "The Bomb That Wanted to Stop Exploding: Reze’s Impossible Freedom in <em>Chainsaw Man—The Movie</em> - Anime News Network"
The Bomb That Wanted to Stop Exploding: Reze’s Impossible Freedom in <em>Chainsaw Man—The Movie</em>
- "https://www.novonordisk.com/content/nncorp/global/en/news-and-media/news-and-ir-materials/news-details.html?id=916472 News Details"
""
- "https://blog.glyphdrawing.club/font-with-built-in-syntax-highlighting/ GlyphDrawing.Club -blog"
""
- "https://bfswa.substack.com/p/6-years-after-too-much-crypto 6 years after too much crypto—by JP Aumasson—bfSwA"
6 years after too much crypto
- "http://www.mindandmuscle.net/forum/42324-another-phenibut-casualty MindAndMuscle.net—Informasi Resmi Game Online Terkini Cuan"
""
- "Phenibut—Erowid Exp—’Drunk for Hours’"
Phenibut—Erowid Exp—‘Drunk for Hours’
- "https://www.zotero.org/simon/cv Zotero"
""
- "https://aistudio.google.com/app/prompts/1DIwkut2wKo_G~Q~_25fo6lSEphQFBnEhWLKq Sign in—Google Accounts"
""
- "https://www.anthropic.com/research/many-shot-jailbreaking Many-shot jailbreaking \\ Anthropic"
Many-shot jailbreaking
- "https://www.cambridge.org/core/journals/national-institute-economic-review/article/curious-case-of-the-national-fund/C4C5A421F70BAD2F0A0D01F99977C616 THE CURIOUS CASE OF THE NATIONAL FUND"
The Curious Case of The National Fund
- "https://maurycyz.com/misc/raw_photo/ What an unprocessed photo looks like: (Maurycy’s blog)"
What an unprocessed photo looks like
- "Disney Imagineering Reveals Robotic OlafCharacter"
Disney Imagineering Reveals Robotic Olaf Character
- "http://www.ldysinger.com/ThM_599d_Beg/08_exp-fet_neon/01_anenceph-donor_missed.htm <em>Virgin and Child</em>"
""
- "https://rajeeshknambiar.wordpress.com/2025/12/27/a-font-with-built-in-tex-syntax-highlighting/ A font with built-in <span class="logotype-tex">T<sub>e</sub>X</span> syntax highlighting—<em>Soliloquies</em>"
A font with built-in <span class="logotype-tex">T<sub>e</sub>X</span> syntax highlighting
- "Why It’s So Rare for a Wife to Be Taller Than Her Husband - The Atlantic"
Why It’s So Rare for a Wife to Be Taller Than Her Husband
- "https://ia.net/about-us About Us - iA" # self-profile page of iA
About Us
- "https://wol.fm/blog/utopian-scholastic.html Michael Wolf"
""
- "https://lbstanza.org/purpose_of_programming_languages.html L.B.Stanza"
""
- "https://axiommath.ai/territory/from-seeing-why-to-checking-everything Axiom"
""
- https://tinyhack.com/2014/03/12/implementing-a-web-server-in-a-single-printf-call/ Implementing a web server in a single printf() call - Tinyhack.com"
"Implementing a web server in a single printf() call"
- "https://dmvaldman.github.io/rooklift/ Writings"
""
- "https://dmvaldman.github.io/rooklift/ RookLift - Writings (Training my watch to track intelligence)"
"RookLift - Training my watch to track intelligence"
- https://www.sfweekly.com/archives/the-worst-run-big-city-in-the-u-s/article_ff893b10-e35a-57cc-a52b-47162048b2c8.html The Worst-Run Big City in the US | Archives"
"The Worst-Run Big City in the US"
- "https://mchav.github.io/learning-better-decision-tree-splits/ Learning better decision tree splits—LLMs as Heuristics for Program Synthesis - Michael Chavinda - A collection of my thoughts on the various topics I find myself interested in."
"Learning better decision tree splits—LLMs as Heuristics for Program Synthesis"
- "https://secondthoughts.my/posts/projects/forecats/ second-thoughts/posts/projects/forecats/"
""
- https://www.secretorum.life/p/japanese-death-poems <em>Japanese Death Poems</em> - by Roger’s Bacon - Secretorum"
"<em>Japanese Death Poems</em>"
- https://www.secretorum.life/p/japanese-death-poems-part-2 <em>Japanese Death Poems</em> (part 2)—by Roger’s Bacon"
"<em>Japanese Death Poems</em> (part 2)"
- https://www.presentandcorrect.com/blogs/blog/typography-on-pencils-1-5 Typography on Pencils, 1–5.—Present &amp; Correct"
"Typography on Pencils, 1–5"
- "https://krabat.menneske.dk/kkblog/2016/06/29/solved-auto-breaking-lines-in-epub-poetry-collections/ SOLVED… Auto-Breaking lines in ePub poetry collections! Well, sorta…—Kenneth Krabats 1000 stemmer"
"SOLVED… Auto-Breaking lines in ePub poetry collections! Well, sorta…"
- "https://sdleffler.github.io/RustTypeSystemTuringComplete/ Rust’s Type System is Turing-Complete - Recursive Descent into Madness - A countable set of sanities and insanities, by Shea Leffler."
"Rust’s Type System is Turing-Complete"

Task:

Input title to clean:

- """ + target + "\"\n"

completion = client.chat.completions.create(
  temperature=1, # temperature=0,
  model="gpt-5-mini",
  messages=[
    {"role": "system", "content": "You are a researcher and web developer, compiling a bibliography."},
    {"role": "user", "content": prompt }
  ]
)

print(completion.choices[0].message.content)
