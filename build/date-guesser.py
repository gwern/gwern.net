#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# date-guesser.py: extract recent dates in YYYY[[-MM]-DD] format from natural language inputs or structured text like URLs
# Author: Gwern Branwen
# Date: 2024-06-11
# When:  Time-stamp: "2024-08-27 11:45:08 gwern"
# License: CC-0
#
# Usage: $ OPENAI_API_KEY="sk-XXX" echo 'https://erikbern.com/2016/04/04/nyc-subway-math' | python date-guesser.py
#          2016-04-04
#
# Many strings, like page titles or abstracts or URLs, contain a clear date. But they come in far too many formats to feasibly write regexps to extract without a ton of labor and risking inconsistency from overlapping matches.
# This is a good use-case for LLMs. We can provide a variety of date inputs with the known date, and it uses syntax+semantics to extract dates in general.

import sys
from openai import OpenAI
client = OpenAI()

if len(sys.argv) == 1:
    target = sys.stdin.read().strip()
else:
    target = sys.argv[1]

prompt = """
Task: Guess the date mentioned in the input.
The date should be formatted like 'YYYY[-MM[-DD]]' format, with the most available precision; if only the year is available, print the year like '2023'; if only year+month, then '2023-12'; otherwise, a full date like '2023-12-09'.
Dates are valid only between 1000AD and 2100AD; any dates outside that date range are probably mistakes, and do not print those.
If there is more than one valid date, print only the first one.
Do not make up dates; if you are unsure, print only the empty string "".

Task examples:

- "In 1492, Columbus sailed the ocean blue; he returned in 1499."
1492
- "1724256502"
2024-08-21
- "https://erikbern.com/2016/04/04/nyc-subway-math"
2016-04-04
- "https://www.dwarkeshpatel.com/p/progress-update"
""
- "When Nothing Ever Goes Out of Print: Maintaining Backlist Ebooks"
""
- "https://www.bbc.com/future/article/20240201-a-us-engineer-had-a-shocking-plan-to-improve-the-climate-burn-all-coal-on-earth"
2024-02-01
- "https://arxiv.org/abs/2408.08459"
2024-08
- "https://www.biorxiv.org/content/10.1101/2024.08.13.607810.full"
2024-08-16
- "Yesterday’s Pixels, Today"
""
- "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10066154/"
""
- "https://jdstillwater.blogspot.com/2012/05/i-put-toaster-in-dishwasher.html"
2012-05
- "https://3quarksdaily.com/3quarksdaily/2011/06/a-crab-canon-for-douglas-hofstadter.html"
2011-06
- "This page was published on 10 February 1991."
1991-02-10
- "We published this on the first of May after Obama's re-election."
2013-05-01
- "https://x.com/jconorgrogan/status/1820212444016345146"
""
- "https://www.nature.com/articles/s41586-021-03819-2#deepmind"
""
- " Jeffrey Arlo Brown, July 31, 2024"
2024-07-31
- "https://publicdomainreview.org/collection/alexander-graham-bell-s-tetrahedral-kites-1903-9/"
""
- "https://publicdomainreview.org/collection/the-model-book-of-calligraphy-1561-1596/"
""
- "https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.0020100Z"
""
- "https://www.explainxkcd.com/wiki/index.php/1393:_Timeghost"
""
- "https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.0040028"
""
- "https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1001747"
""
- "/newsletter/2022/index"
2022
- "https://academic.oup.com/aje/article/156/11/985/80696"
""
- "https://academic.oup.com/biomedgerontology/article/70/9/1097/2949096"
""
- "https://ctlj.colorado.edu/wp-content/uploads/2015/08/Meyer-final.pdf"
2015-07-05
- "http://www.synthesis.cc/synthesis/2016/15/on_dna_and_transistors"
2016
- "http://mbio.asm.org/content/7/4/e01018-16.full"
""
- "https://rstb.royalsocietypublishing.org/content/363/1503/2519"
""
- "https://web.archive.org/web/20110530014638/https://today.msnbc.msn.com/id/43098220/ns/today-today_people/t/after-years-millionaire-misers-heirs-finally-split-m/"
2011-05-27
- "https://web.archive.org/web/20110801232705/http://www.lifepact.com/history.htm"
2004-06
- "https://www.nejm.org/doi/full/10.1056/NEJMoa1715474"
""
- "https://academic.oup.com/molehr/article/24/3/135/4829657"
""
- "https://distill.pub/2020/01/32/circuits/branch-specialization/"
2020-01
- "https://jamanetwork.com/journals/jamapsychiatry/fullarticle/2569454"
""
- "https://jasbsci.biomedcentral.com/articles/10.1186/s40104-018-0304-7"
""
- "https://journals.biologists.com/jeb/article/218/1/123/13627/The-developmental-origins-of-chronic-physical"
""
- "https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1003864"
""
- "https://microbiomejournal.biomedcentral.com/articles/10.1186/s40168-017-0321-3"
""
- "https://web.archive.org/web/20141018022010/https://news.nationalgeographic.com/news/2014/10/141015-better-beef-genetics-science-agriculture-environment-ngfood/"
2014-10-15
- "https://if50.substack.com/p/1985-a-mind-forever-voyaging"
""
- "https://if50.substack.com/p/2005-shades-of-doom"
""
- "https://if50.substack.com/p/2013-a-family-supper"
""
- "https://if50.substack.com/p/2015-lifeline"
""
- "https://if50.substack.com/p/2017-universal-paperclips"
""
- "https://if50.substack.com/p/2020-scents-and-semiosis"
""
- "https://github.com/martinarjovsky/WassersteinGAN/issues/2#issuecomment-278710552"
""
- "https://gizmodo.com/generation-cryo-fighting-death-in-the-frozen-unknown-1786446378"
""
- "https://x.com/michelangemoji/status/1485356685896347648"
""
- "https://x.com/danielrussruss/status/1489947340026769412"
""
- "https://nutritionj.biomedcentral.com/articles/10.1186/s12937-017-0269-y"
""
- "https://nymag.com/news/features/synthetic-drugs-2013-4/"
2013-04-05
- "https://www.reddit.com/r/thisisthewayitwillbe/comments/e02gtg/ai_and_neuroscience_main2019_patrick_mineaults/"
2019-11-22
- "https://onlinelibrary.wiley.com/doi/10.1111/acel.13294"
""
- "https://onlinelibrary.wiley.com/doi/10.1111/acel.13296"
""
- "https://onlinelibrary.wiley.com/doi/10.1111/acel.13344"
""
- "https://onlinelibrary.wiley.com/doi/10.1111/rati.12233"
""
- "https://onlinelibrary.wiley.com/doi/abs/10.1111/rda.13358"
""
- "https://onlinelibrary.wiley.com/doi/abs/10.1111/rda.13358"
""
- "https://onlinelibrary.wiley.com/doi/pdf/10.1002/ajmg.b.32363"
""
- "https://onlinelibrary.wiley.com/doi/pdf/10.1002/ajmg.b.32388"
""
- "https://onlinelibrary.wiley.com/doi/pdf/10.1002/ajmg.b.32419"
""
- "https://openai.com/index/the-international-2018-results/"
2018-08-23
- "https://phys.org/news/2016-02-animals-tb-cancer-landmines.html"
2016-02-16
- "https://proceedings.mlr.press/v80/guez18a.html"
""
- "https://osf.io/preprints/psyarxiv/4q9gv/"
""
- "https://psychiatryonline.org/doi/10.1176/appi.ajp-rj.2021.170105"
""
- "https://publicdomainreview.org/blog/2021/12/all-sound-recordings-prior-to-1923-will-enter-the-us-public-domain-in-2022/"
2021-12-07
- "https://publicdomainreview.org/collection/chladni-figures-1787/"
""
- "https://publicdomainreview.org/collection/examples-of-chinese-ornament-1867/"
""
- "https://pubs.acs.org/doi/full/10.1021/acsptsci.0c00099"
""
- "https://publicdomainreview.org/collection/glossary-of-censored-words-from-a-1919-book-on-love/"
""
- "https://publicdomainreview.org/collection/japanese-designs-1902/"
""
- "https://publicdomainreview.org/collection/john-lockes-method-for-common-place-books-1685/"
""
- "https://publicdomainreview.org/collection/sarah-goodridges-beauty-revealed-1828/"
""
- "https://publicdomainreview.org/collection/on-the-writing-of-the-insane-1870/"
""
- "https://publicdomainreview.org/collection/the-unicorn-tapestries-1495-1505/"
""
- "https://pubs.acs.org/doi/full/10.1021/acsptsci.0c00099"
""
- "https://elifesciences.org/articles/57849"
""
- "https://en.wikipedia.org/wiki/Deadline_(1982_video_game)"
""
- "https://en.wikipedia.org/wiki/Land_Tax_Reform_(Japan_1873)"
""
- "https://en.wikipedia.org/wiki/Radical_Chic_%26_Mau-Mauing_the_Flak_Catchers"
""
- "https://en.wikipedia.org/wiki/Rams_(2018_film)"
""
- "https://en.wikipedia.org/wiki/Revolutions_of_1989"
""
- "https://academic.oup.com/ajcn/article/110/3/583/5512180"
""
- "https://academic.oup.com/aje/article/156/11/985/80696"
""
- "https://academic.oup.com/cercor/article/28/12/4136/4560155"
""
- "https://academic.oup.com/jhered/article/112/7/569/6412509"
""
- "https://academic.oup.com/jn/article/135/6/1347/4663828"
""
- "https://academic.oup.com/molehr/article/24/3/135/4829657"
""
- "https://acamh.onlinelibrary.wiley.com/doi/10.1111/jcpp.13528"
""
- "https://diabetes.diabetesjournals.org/content/67/5/831"
""
- "https://medium.com/ml-everything/using-gpt-3-to-explain-jokes-2001a5aefb68"
""
- "The summer solstice 4 years before 2024."
2020-06-20
- "Can AI Scaling Continue Through 2030?"
""
- "https://epochai.org/blog/prospects-for-search-scaling-laws"
""
- "Christmas 2024"
2024-12-25
- "https://arxiv.org/abs/2404.11018#google"
2024-04-17

Task:

- """ + target + "\n"

completion = client.chat.completions.create(
  model="gpt-4o-mini", # we use GPT-4 because the outputs are short, we want the highest accuracy possible, we provide a lot of examples & instructions which may overload dumber models, and reviewing for correctness can be difficult, so we are willing to spend a few pennies to avoid the risk of a lower model
  messages=[
    {"role": "system", "content": "You are a researcher and web developer, compiling a bibliography."},
    {"role": "user", "content": prompt }
  ]
)

print(completion.choices[0].message.content)
