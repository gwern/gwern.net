#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# title-cleaner.py: remove cruft from titles of web pages like website name/domain or error messages
# Author: Gwern Branwen
# Date: 2024-06-11
# When:  Time-stamp: "2024-07-20 21:54:17 gwern"
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

1. Input title to clean: "If I Sleep for an Hour, 30 People Will Die - The New York Times"
"If I Sleep for an Hour, 30 People Will Die"
2. Input title to clean: "404"
""
3. Input title to clean: "Gwern.net | Collecting New Socks Efficiently"
"Collecting New Socks Efficiently"
4. Input title to clean: "worlds of DAVID BRIN"
""
5. Input title to clean: "steve yegge · The Amazon Memo"
"The Amazon Memo"
6. Input title to clean: "index"
""
7. Input title to clean: ""
""
8. Input title to clean: "After 92 years, millionaire miserâs heirs finally split $100M - TODAY People - TODAY.com"
After 92 years, millionaire miser’s heirs finally split $100M
9. Input title to clean: "502 Bad Gateway"
""
10. Input title to clean: "Faces2Anime: Cartoon Style Transfer in Faces using Generative Adversarial Networks. Masters Thesis 2021 @ NTUST."
"Faces2Anime: Cartoon Style Transfer in Faces using Generative Adversarial Networks"
11. Input title to clean: "articles"
""
12. Input title to clean: "Vercel Security Checkpoint"
""
13. Input title to clean: "Fan Is A Tool-Using AnimalâdConstruct Conference Talk"
"Fan Is A Tool-Using Animal"
14. Input title to clean: "CONTENTdm"
""
15. Input title to clean: "The Illustrated Retrieval Transformer â Jay Alammar â Visualizing machine learning one concept at a time."
"The Illustrated Retrieval Transformer"
16. Input title to clean: "Redirecting to https://now.tufts.edu/2016/07/14/moderately-reducing-calories-non-obese-people-reduces-inflammation"
""
17. Input title to clean: "Not Found - Albion - Webflow HTML website template"
""
18. Input title to clean: "Siberian Times"
""
19. Input title to clean: "Stripe\\226\\128\\153s first carbon removal purchases\\nStripe logo\\nOpen mobile navigation\\nStripe logo\\nClose mobile navigation\\nPayments\\nCheckout\\nElements\\nRadar\\nConnect\\nBilling\\nInvoicing\\nTerminal\\nIdentity\\nClimate\\nBilling\\nInvoicing\\nTax\\nRevenue Recognition\\nSigma\\nAtlas\\nConnect\\nCapital\\nIssuing\\nTreasury\\nPayments\\nCheckout\\nElements\\nRadar\\nConnect\\nBilling\\nInvoicing\\nTerminal\\nIdentity\\nClimate\\nBilling\\nInvoicing\\nTax\\nRevenue Recognition\\nSigma\\nAtlas\\nConnect\\nCapital\\nIssuing\\nTreasury\\nStripe logo"
Stripe’s first carbon removal purchases
20. Input title to clean: "308 Permanent Redirect"
""
21. Input title to clean: "404 - Isomorphic Labs"
""
22. Input title to clean: "Page not found - PsyPost - Psychology News"
""
23. Input title to clean: "Japanese north\226\128\147south gradient in IQ predicts differences in stature, skin color, income, and homicide rate - ScienceDirect\nScienceDirect"
"Japanese north-south gradient in IQ predicts differences in stature, skin color, income, and homicide rate"
24. Input title to clean: "Notion – The all-in-one workspace for your notes, tasks, wikis, and databases."
""
25. Input title to clean: "It’s (Still) Really Hard for Robots to Autonomously Do Household Chores - IEEE Spectrum"
"It’s (Still) Really Hard for Robots to Autonomously Do Household Chores"
26. Input title to clean: "504 Gateway Time-out"
""
27. Input title to clean: "æ°æ°æ®é"
""
28. Input title to clean: "dnmarchives directory listing\nInternet Archive logo\nDonate icon\nSearch icon\nSearch icon\nUpload icon\nUser icon\nWeb icon\nTexts icon\nVideo icon\nAudio icon\nSoftware icon\nImages icon\nDonate icon\nEllipses icon\nHamburger icon\nSearch icon\nDonate icon"
"dnmarchives directory listing"
29. Input title to clean: "Page not found : Stanford University"
""
30. Input title to clean: "07"
""
31. Input title to clean: "Flashback Forum"
""
32. Input title to clean: "Stuff"
""
33. Input title to clean: "Humboldt &amp; Sonoma counties: Six arrested, 3,000 marijuana plants and 44 weapons seized in state DOJ raids - The Willits News"
Humboldt &amp; Sonoma counties: Six arrested, 3,000 marijuana plants and 44 weapons seized in state DOJ raids
34. Input title to clean: "GoLocalPDX"
""
35. Input title to clean: "Aurora’s Approach to Development. Self-driving cars are an appliedâ¦ by The Aurora Team Aurora Blog"
"Aurora’s Approach to Development. Self-driving cars are an applied"
36. Input title to clean: "An open letter to Netflix from the authors of the de-anonymization paper « 33 Bits of Entropy"
"An open letter to Netflix from the authors of the de-anonymization paper"
37. Input title to clean: "It\226\128\153s Probably Not Lithium\nspotify-podcast-badge-wht-blk-165x40"
"It’s Probably Not Lithium"
38. Input title to clean: "Scunthorpe Sans \240\159\151\175\240\159\154\171 profanity-blocking font"
"Scunthorpe Sans: a profanity-blocking font"
39. Input title to clean: "&#13;\n\tMedicine &amp; Science in Sports &amp; Exercise&#13;"
""
40. Input title to clean: "Nadia Asparouhova How to do the jhanas"
"How to do the jhanas"
41. Input title to clean: "Best-of-n with misaligned reward models for Math reasoning"
"Best-of-<em>n</em> with misaligned reward models for Math reasoning"
42. Input title to clean: "The Universe of Discourse : A potpourri of cool-looking scripts"
"A potpourri of cool-looking scripts"
43. Input title to clean: "FineWeb: decanting the web for the finest text data at scale—a Hugging Face Space by HuggingFaceFW"
"FineWeb: decanting the web for the finest text data at scale"
44. Input title to clean: "Do life hacks work? The truth is, weâll never know Psychology"
"Do life hacks work? The truth is, we’ll never know"
45. Input title to clean: "Operant Conditioning by Software Bugs – Embedded in Academia"
"Operant Conditioning by Software Bugs"
46. Input title to clean: "The dream of an alpine waterway - Swiss National Museum—Swiss history blog"
"The dream of an alpine waterway"
47. Input title to clean: "Andrew Wood The Medical School"
"Andrew Wood"
48. Input title to clean: "Reddit"
""
49. Input title to clean: "The Stigler Diet Problem �|� OR-Tools �"
"The Stigler Diet Problem"
50. Input title to clean: "why why why why why why why"
""
51. Input title to clean: "Optimized, Individualized Spaced Repetition in Hierarchical Knowledge Structures—Justin Skycak"
"Optimized, Individualized Spaced Repetition in Hierarchical Knowledge Structures"
52. Input title to clean: "Exclusive"
""
53. Input title to clean: "Patronage vs. Constituent Parties (Or Why Republican Party Leaders Matter More Than Democratic Ones) – The Scholar's Stage"
"Patronage vs. Constituent Parties (Or Why Republican Party Leaders Matter More Than Democratic Ones)"

Task:

Input title to clean: """ + target + "\n"

completion = client.chat.completions.create(
  model="gpt-4o", # we use GPT-4 because the outputs are short, we want the highest accuracy possible, we provide a lot of examples & instructions which may overload dumber models, and reviewing for correctness can be difficult, so we are willing to spend a few pennies to avoid the risk of a lower model
  messages=[
    {"role": "system", "content": "You are a researcher and web developer, compiling a bibliography."},
    {"role": "user", "content": prompt }
  ]
)

print(completion.choices[0].message.content)
