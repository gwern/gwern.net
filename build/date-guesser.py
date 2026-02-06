#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# date-guesser.py: extract recent dates in YYYY[[-MM]-DD] format from natural language inputs or structured text like URLs
# Author: Gwern Branwen
# Date: 2024-08-21
# When:  Time-stamp: "2026-02-05 23:40:31 gwern"
# License: CC-0
#
# Usage: $ OPENAI_API_KEY="sk-XXX" echo 'https://erikbern.com/2016/04/04/nyc-subway-math' | python date-guesser.py
#          2016-04-04
#
# Many strings, like page titles or abstracts or URLs, contain a clear date. But they come in far too many formats to feasibly write regexps to extract without a ton of labor and risking inconsistency from overlapping matches.
# This is a good use-case for LLMs. We can provide a variety of date inputs with the known date, and it uses syntax+semantics to extract dates in general.
#
# The returned date will be from today or earlier, and it will be in 'YYYY[-MM[-DD]]' format. We instruct the LLM to be conservative, and when in doubt, return no guessed date and let a human manually review & fix missing dates (rather than risk returning a plausible but wrong date).

import sys
from openai import OpenAI
from datetime import datetime
import re

def validate_date_format(date_str):
    """
    Validate that the date string matches YYYY[-MM[-DD]] format.
    Returns True if valid, False if invalid.
    """
    patterns = [
        r'^\d{4}$',                     # YYYY
        r'^\d{4}-(?:0[1-9]|1[0-2])$',   # YYYY-MM
        r'^\d{4}-(?:0[1-9]|1[0-2])-(?:0[1-9]|[12]\d|3[01])$'  # YYYY-MM-DD
    ]

    return any(re.match(pattern, date_str) for pattern in patterns)

def validate_date_not_future(date_str):
    """
    Validate that the date is not in the future and is a valid calendar date
    (including proper handling of leap years).
    Returns True if date is valid and not future, False otherwise.
    """
    today = datetime.now()

    try:
        if len(date_str)==4 and int(date_str)<1000: return False
        elif len(date_str) == 4:  # YYYY; # Parse the date string based on its format
                 date = datetime.strptime(date_str, '%Y')
                 return date.year <= today.year
        elif len(date_str) == 7:  # YYYY-MM
                 date = datetime.strptime(date_str, '%Y-%m')
                 return date.year < today.year or (date.year == today.year and date.month <= today.month)
        elif len(date_str) == 10:  # YYYY-MM-DD
                 date = datetime.strptime(date_str, '%Y-%m-%d')
                 return date <= today
        else: return False
    except ValueError:
        return False

def main():
    client = OpenAI()

    if len(sys.argv) == 1:
        target = sys.stdin.read().strip()
    else:
        target = sys.argv[1]

    current_date = datetime.now().strftime('%Y-%m-%d')

    prompt = """
Task: Guess the date mentioned in the input.
The date should be formatted like 'YYYY[-MM[-DD]]' format, with the most available precision; if only the year is available, print the year like '2023' (do not print '2023-01-01' or '2023-01'); if only year+month, then '2023-12'; otherwise, a full date like '2023-12-09'.
Dates are valid only between 1000AD and """ + current_date + """; any dates outside that date range are probably mistakes, and do not print those.
(Note also that the World Wide Web was invented in 1989, so web pages cannot predate 1990AD. Web page dates before 2000AD are rare, and dates before 2010AD should be treated with extra scrutiny unless from an archival source.)
If there is more than one valid date, print only the first one; for ranges, return the earliest endpoint; otherwise, return the first valid date token left-to-right.
Do not make up dates; if you are unsure, print only the empty string "".
Do not add any commentary or explanations.
For Arxiv URLs, only the first 4 numbers are meaningful, in the form: `YYMM`; the numbers after the period are an ID and the date *cannot* be guessed from them!
For archived URLs such as Internet Archive URLs, the date of the snapshot is *not necessarily* the date of publication.
(It may be, if the IA scraped it quickly, but may not be.)
Wikipedia articles have no date due to complicated editing histories and 'created' having no particular meaning for them. Do not return dates for any Wikipedia articles.

Task examples (with explanations in '#' comments):

- "Published: 02-29-2024 | https://example.com/leap-year-article"
2024-02-29
- "Published: 02-29-2023 | https://example.com/another-leap-year-article" # Invalid: 2023 is not a leap year
""
- "Date: 04/31/2024: Article about calendars" # Invalid: April has 30 days
""
- "Article from Sep 31, 2023 about date formats" # Invalid: September has 30 days
""
- "Updated 2024.13.01 with new information" # Invalid month 13
""
- "Version: 2024.00.01" # Invalid month 0
""
- "Created on 2024-04-00" # Invalid day 0
""
- "Last modified: 31/12/2023 23:59:59 UTC" # Should extract just the date portion
2023-12-31
- "Posted: Yesterday at 3pm" # Relative dates without an absolute date anywhere should be ignored, as who knows what they are *actually* relative to?
""
- "today is 2025-09-30; posted 8 days ago" # example of when we *can* infer the absolute date even with a relative date
2025-09-22
- "Updated: 2 days ago" # Relative dates should be ignored
""
- "EST Release Date: 12/13/23" # Should handle American date format
2023-12-13
- "Published Date: 13/12/23" # Should handle European date format if unambiguous
2023-12-13
- "From our 2023-13-12 edition" # Invalid: month 13
""
- "Article from Feb 30th, 2023" # Invalid: February never has 30 days
""
- "Created: 2024.02.29"
2024-02-29
- "Date of record: 1999-11-31" # Invalid: November has 30 days
""
- "TimeStamp:20240229123456" # Valid: a leap year
2024-02-29
- "YYYYMMDD: 20241301" # Invalid month 13
""
- "Released between 2023Q4 and 2024Q1" # Too ambiguous
""
- "Copyright (c) 2020-2024" # Take earliest publication year when given a range
2020
- "Volume 23, Issue 45 (Winter 2023-2024)" # Take earliest year when given a range
2023
- "Academic Year 2023/24" # Take earliest year when given a range
2023
- "Originally written in 2022, updated for 2024" # Take original/earliest date
2022
- "Written 2024/04/31: Edited 2024/05/01" # Invalid first date (April has 30 days)
""
- "Looking back at the year 2000 problem (Y2K)" # Don't extract Y2K as it's a topic, not a date # NOTE: ArXiv IDs are in the form YYMM.NNNNN (since 2015), representing the year, month, and submission index that month​. For example, 1907.07174 = July 2019, 7174th submission. A version suffix (v1, v2, …) may follow for revisions
""
- "A paper from arXiv:2401.12345" # Extract date from arXiv ID
2024-01
- "DOI: 10.48550/arXiv.2408.08459" # Extract date from DOI when unambiguous; but be careful... This is valid for BioRxiv/MedRxiv/Arxiv, but not necessarily anywhere else.
2024-08
- "Posted on 29/02/2023 about leap years" # Invalid: 2023 wasn't a leap year
""
- "Article 123456789 from 2024-W01" # ISO week dates should return just the year
2024
- "Publication: 2024. Journal of Examples" # Handle trailing period after year
2024
- "From the 1990's collection" # Don't extract dates from decades
""
- "Temperature was 2023.5 degrees" # Don't extract decimal numbers as dates
""
- "Score was 2024:1" # Don't extract scores/ratios as dates
""
- "Published at 2024hrs on Jan 1" # Don't extract 24-hour time as year
""
- "Build version 2024.01.alpha" # Software versions can contain valid dates
2024-01
- "Model: RTX 2080 Ti" # Don't extract product numbers as dates
""
- "ISBN: 1-234567-890-2024" # Don't extract years from ISBNs
""
- "Highway 2024 South" # Don't extract road numbers as dates
""
- "Room 2024B, Building 3" # Don't extract room numbers as dates
""
- "Postal code 2024AB" # Don't extract postal codes as dates
""
- "In the year 2525, if man is still alive" # Song lyric about future: not a publication date
""
- "Article accepted 2024-02-30, published 2024-03-01" # First date invalid, use second valid date
2024-03-01
- "File: IMG_20241301_123456.jpg" # Invalid date in filename (month 13)
""
- "Updated 24-01-15" # Assume 2-digit years <= 26 are 2000-2026
2024-01-15
- "Updated 95-01-15" # Assume 2-digit years >26 are 1900s
1995-01-15
- "Patent №2024-123456" # Don't extract patent numbers as dates
""
- "SKU: 20240123-ABC" # Don't extract SKU/product codes as dates
""
- "Contact: +1 (202) 555-2024" # Don't extract phone numbers as dates
""
- "Sample #2024-01-A5" # Laboratory sample IDs can contain valid dates
2024-01
- "hex color #202420" # Don't extract hex colors as dates
""
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
- "https://www.biorxiv.org/content/10.1101/2024.08.13.607810.full" # 2024-08-16
2024-08
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
- "We published this on the first of May after Obama’s re-election."
2013-05-01
- "https://x.com/jconorgrogan/status/1820212444016345146"
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
- "https://ctlj.colorado.edu/wp-content/uploads/2015/08/Meyer-final.pdf" # 2015-07-05
2015
- "http://www.synthesis.cc/synthesis/2016/15/on_dna_and_transistors"
2016
- "http://mbio.asm.org/content/7/4/e01018-16.full"
""
- "https://rstb.royalsocietypublishing.org/content/363/1503/2519"
""
- "https://web.archive.org/web/20110530014638/https://today.msnbc.msn.com/id/43098220/ns/today-today_people/t/after-years-millionaire-misers-heirs-finally-split-m/" # 2011-05-27
""
- "https://web.archive.org/web/20110801232705/http://www.lifepact.com/history.htm" # 2004-06
""
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
- "https://nymag.com/news/features/synthetic-drugs-2013-4/" # 2013-04-05
2013-04
- "https://www.reddit.com/r/thisisthewayitwillbe/comments/e02gtg/ai_and_neuroscience_main2019_patrick_mineaults/" # 2019-11-22
""
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
- "https://onlinelibrary.wiley.com/doi/pdf/10.1002/ajmg.b.32363"
""
- "https://onlinelibrary.wiley.com/doi/pdf/10.1002/ajmg.b.32388"
""
- "https://onlinelibrary.wiley.com/doi/pdf/10.1002/ajmg.b.32419"
""
- "https://openai.com/index/the-international-2018-results/" # 2018-08-23 [results could be announced well afterwards, like in 2019]
""
- "https://phys.org/news/2016-02-animals-tb-cancer-landmines.html" # 2016-02-16
2016-02
- "https://proceedings.mlr.press/v80/guez18a.html"
""
- "https://osf.io/preprints/psyarxiv/4q9gv/"
""
- "https://psychiatryonline.org/doi/10.1176/appi.ajp-rj.2021.170105"
""
- "https://publicdomainreview.org/blog/2021/12/all-sound-recordings-prior-to-1923-will-enter-the-us-public-domain-in-2022/" # 2021-12-07
2021-12
- "https://publicdomainreview.org/collection/chladni-figures-1787/"
""
- "https://publicdomainreview.org/collection/examples-of-chinese-ornament-1867/"
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
- "https://academic.oup.com/cercor/article/28/12/4136/4560155"
""
- "https://academic.oup.com/jhered/article/112/7/569/6412509"
""
- "https://academic.oup.com/jn/article/135/6/1347/4663828"
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
- "https://www.nature.com/articles/s41586-021-03819-2#deepmind" # 2021-07-15
2021
- "https://doi.org/10.1038/s41586-021-03819-2" # 2021-07-15; clearly a Nature DOI, so we know '021' = '2021'
2021
- "https://www.nature.com/articles/s41586-023-06185-3" # 2023-07-05
2023
- "The 240th anniversary of the signing of the Declaration of Independence"
2016-07-04
- "Last business day of Q3 2025"
2025-09-30
- "The day after tomorrow, three years ago"
""
- "https://www.youtube.com/watch?v=dQw4w9WgXcQ&t=42s"
""
- "2012-12-01, https://www.sciencedirect.com/science/article/pii/S0032579119394003, 25 years of selection for improved leg health in purebred broiler lines and underlying genetic parameters"
2012-12-01
- "2020-05-15, https://www.sciencedirect.com/science/article/pii/S1053811920301786, Educational attainment polygenic scores are associated with cortical total surface area and regions important for language and memory"
2020-05-15
- "2023-12-08, https://www.reuters.com/world/uk/uk-antitrust-regulator-considering-microsoft-openai-partnership-2023-12-08/, Microsoft, OpenAI tie-up comes under antitrust scrutiny"
2023-12-08
- "2022-010-17, https://osf.io/preprints/psyarxiv/tnyda/, Does the mere presence of a smartphone impact cognitive performance? A meta-analysis of the brain drain effect" # fix typos when unambiguous. '010' obviously means '10', but '13' or '00' would be unfixable
2022-10-17
- "02022-010-017, https://osf.io/preprints/psyarxiv/tnyda/, Does the mere presence of a smartphone impact cognitive performance? A meta-analysis of the brain drain effect"
2022-10-17
- "2024-06-7, https://www.newyorker.com/culture/the-front-row/flipside-is-a-treasure-trove-of-music-and-memory, <em>Flipside</em> Is a Treasure Trove of Music and Memory: Chris Wilcha's documentary explores life, love, and art through his connection to a venerable record store"
2024-06-07
- "The 2024 Olympic Games in Paris: A Look Ahead - Sports Illustrated"
""
- "https://web.archive.org/web/20100915000000*/http://example.com" # 2010-09-15
""
- "COVID-19: Two Years Later - A Retrospective (Published on March 11, 2022)"
2022-03-11
- "Apple WWDC 2023 Keynote: iOS 17, macOS 14, and More"
2023-06-05
- "SpaceX's Starship: From Prototype to Orbit (2019-2023)" # this could not have been published in 2019, so it was probably published in 2023
2023
- "Last updated: 2023-09-15T14:30:00Z - Company Blog"
2023-09-15
- "The Best Movies of the '90s: A Nostalgic Journey"
""
- "Bitcoin Whitepaper: 15th Anniversary Edition (October 31, 2008)"
2008-10-31
- "https://example.com/blog/2022/03/article-slug.html"
2022-03
- "Released in Q4 2023: Our New Product Line"
2023
- "20th Century Fox presents: A 21st Century Film"
""
- "https://publicdomainreview.org/collection/w-w-denslow-illustrations-wonderful-wizard-of-oz-1900/"
""
- "https://www.salon.com/2002/12/17/tolkien_brin/, J. R. R. Tolkien - enemy of progress: <em>The Lord of the Rings</em> is lovingly crafted, seductive - and profoundly backward-looking."
2002-12-17
- "The LessWrong 2022 Review"
""
- "Mercy Brown vampire incident : https://en.wikipedia.org/wiki/Mercy_Brown_vampire_incident"
""
- "Rapid formation of picture-word association in cats : https://www.nature.com/articles/s41598-024-74006-2"
""
- "The Global Surveillance Free-for-All in Mobile Ad Data : https://krebsonsecurity.com/2024/10/the-global-surveillance-free-for-all-in-mobile-ad-data/"
2024-10
- "https://www.crunchyroll.com/news/interviews/2024/10/28/thaliarchus-cosmic-warlord-kin-bright-interview"
2024-10-28
- "https://www.reuters.com/technology/artificial-intelligence/openai-builds-first-chip-with-broadcom-tsmc-scales-back-foundry-ambition-2024-10-29/"
2024-10-29
- "Stem cell transplantation extends the reproductive life span of naturally aging cynomolgus monkeys https://www.nature.com/articles/s41421-024-00726-4"
""
- "The History of Speech Recognition to the Year 2030 https://awni.github.io/future-speech/"
""
- "https://www.nature.com/articles/s41598-024-76900-1"
""
- "How a silly science prize changed my career https://www.nature.com/articles/d41586-024-03756-w"
""
- "Diagramming Dante: Michelangelo Caetani’s Maps of the *Divina Commedia* (1855/1872) : https://publicdomainreview.org/collection/michelangelo-caetani-maps-of-the-divina-commedia/"
""
- "https://jenn.site/2024/11/you-are-pablo-neruda-it-is-dawn-at-isla-negra-in-1968-matilde-is-asleep-in-your-bed-write-what-comes/ The Neruda Factory"
2024-11
- "https://publicdomainreview.org/collection/pinkerton-thirty-years-a-detective/ True Crime: Allan Pinkerton’s <em>Thirty Years a Detective</em> (1884)"
""
- "https://news.ycombinator.com/item?id=36302805 Why sqlite3 temporary files were renamed <code>etilqs_*</code> (2006)"
""
- "https://gianlucaventurini.com/posts/2024/factorio-sat Learning Solver Design: Automating Factorio Balancers"
2024
- "https://www.nature.com/articles/s44172-024-00175-7 Automatic design of stigmergy-based behaviors for robot swarms"
2024
- "https://curiouscoding.nl/posts/static-search-tree/ Static search trees: 40x faster than binary search"
""
- "They squandered the holy grail https://xeiaso.net/blog/2025/squandered-holy-grail/"
2025
- "https://nicholas.carlini.com/writing/2025/regex-chess.html Regex Chess: A 2-ply minimax chess engine in 84,688 regular expressions"
2025
- "https://www.openculture.com/2021/01/david-lynchs-projection-instructions-for-mulholland-drive-2001.html David Lynch’s Projection Instructions for <em>Mulholland Drive</em> (2001)"
2021-01
- "https://www.lesswrong.com/posts/NXTkEiaLA4JdS5vSZ/what-o3-becomes-by-2028 What o3 Becomes by 2028"
""
- "https://en.wikipedia.org/wiki/Angela_L._Duckworth Angela L. Duckworth"
""
- "https://www.lesswrong.com/posts/e8BNKGEgmCeowtRWS/how-different-llms-answered-philpapers-2020-survey How different LLMs answered PhilPapers 2020 survey"
""
- "https://colah.github.io/posts/2014-03-NN-Manifolds-Topology/" # actually 2014-04-06 since "Posted on April 6, 2014", but that was an update and there is no way to guess it from the URL
2014-03
- "https://www.nature.com/articles/s41598-020-79310-1" # 2021-01-11
""
- "https://www.biorxiv.org/content/10.1101/2021.02.02.429430.full" # 2021-04-29
2021
- "https://web.archive.org/web/20230101012202/https://thispersondoesnotexist.com/" # 2019-02-12
2019
- "https://www.nature.com/articles/s41467-018-04316-3" # 2018-06-19
""
- "/doc/ai/nn/transformer/gpt/dall-e/1/2020-chen-2.pdf#openai" # 2020-06-17
2020
- "https://web.archive.org/web/20191127163535/http://www.aidungeon.io/2019/11/my-orc-band-and-our-quest-for-equal.html" # 2019-11-26
2019-11
- "https://web.archive.org/web/20210426084422/https://www.stuff.co.nz/technology/103500435/google-deepmind-founder-and-leader-in-artificial-intelligence-returns-to-hamilton" # 2018-05-07
""
- "https://distill.pub/2020/circuits/zoom-in/#openai" # 2020-03-10
2020
- "/doc/ai/scaling/2020-bell.pdf#facebook"
2020-08-22
- "https://web.archive.org/web/20230710000944/https://frc.ri.cmu.edu/~hpm/project.archive/general.articles/1975/Raw.Power.html" # 1975-05-12
1975
- "/doc/ai/scaling/hardware/2019-roy.pdf"
2019-11-27
- "/doc/ai/scaling/hardware/2020-jiang.pdf"
2020-11-04
- "https://www.reuters.com/technology/chinas-military-government-acquire-nvidia-chips-despite-us-ban-2024-01-14/"
2024-01-14
- "/doc/darknet-market/2020-zhou-2.pdf"
2020-02-01
- "https://web.archive.org/web/20190130223039/http://www.animenewsservice.com/archives-dec13/" # 1999-12-20
1999-12
- "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0134152"
2015-08-12
- "https://msutoday.msu.edu/news/2016/got-kidney-stones-ride-a-roller-coaster"
2016-09-26
- "https://www.smithsonianmag.com/history/journal-plague-year-180965222/"
2017-11
- "https://www.nature.com/articles/s41598-018-37481-y"
2019-01-30
- "https://www.biorxiv.org/content/10.1101/2020.11.21.392720.full"
2020-11-22
- "https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2020.568049/full"
2020-10-13
- "/doc/darknet-market/2018-wegberg.pdf"
2018-05-08
- "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0236635"
2020-07-10
- "https://www.frontiersin.org/articles/10.3389/fvets.2019.00477/full"
2020-01-08
- "https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2022.941163/full"
2022-08-05
- "/doc/ai/anime/danbooru/2018-zhang-2.pdf"
2018
- "/doc/ai/anime/danbooru/2019-lee-2.pdf"
2019-11-17
- "/doc/ai/anime/danbooru/2020-zheng-2.pdf"
2020-10
- "/doc/ai/anime/danbooru/2020-cao-2.pdf"
2020-10-12
- "/doc/ai/anime/danbooru/2020-lee-2.pdf"
2020-11-27
- "/doc/ai/anime/danbooru/2021-hernandez.pdf"
2021-04-01
- "/doc/ai/anime/danbooru/2022-rios.pdf"
2022-05-27
- "/doc/ai/anime/danbooru/2022-zhang.pdf"
2022-12-02
- "/doc/ai/nn/gan/stylegan/anime/2022-zhou-2.pdf"
2022-12-16
- "/doc/ai/nn/diffusion/2018-sharma.pdf#google"
2018-07-01
- "/doc/ai/nn/2020-hernandezorallo.pdf"
2020-11-04
- "/doc/ai/nn/cnn/2018-choi.pdf"
2018-02-22
- "/doc/ai/nn/cnn/2020-leipheimer.pdf"
2020-01-22
- "https://distill.pub/2020/growing-ca/#google"
2020-02-11
- "/doc/ai/nn/fully-connected/2020-bao.pdf"
2020-06-03
- "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7861215/"
2019-07-21
- "/doc/darknet-market/dnm-archive/2020-zhang.pdf"
2020-12-01
- "https://www.justinpinkney.com/blog/2020/stylegan-network-blending/"
2020-08-25
- "/doc/ai/nn/gan/stylegan/anime/2022-endo.pdf"
2022-07-09
- "https://proceedings.mlr.press/v139/vicol21a.html"
2021-07-01
- "/doc/ai/nn/sparsity/knowledge-distillation/2016-luo.pdf"
2016-03-05
- "/doc/ai/nn/transformer/gpt/2/fiction/2021-davis.pdf"
2021-04-27
- "/doc/law/2022-arbel.pdf" # 2022-02
2022
- "https://mededu.jmir.org/2023/1/e45312/"
2023
- "https://www.taxnotes.com/featured-analysis/openai-cribbed-our-tax-example-can-gpt-4-really-do-tax/2023/08/11/7h0hc" # unreliable URL timestamp
2023-08
- "/doc/ai/nn/transformer/gpt/lamda/2021-jiang-2.pdf"
2021-09-07
- "/doc/ai/scaling/hardware/2020-jouppi.pdf#google"
2020-06-01
- "https://www.liebertpub.com/doi/full/10.1089/ast.2017.1783"
2018-09-12
- "/doc/psychiatry/anxiety/lavender/2021-an.pdf"
2021-09-14
- "https://www.theatlantic.com/science/archive/2021/11/make-cat-hypoallergenic/620618/"
2021-11-05
- "https://www.nature.com/articles/s41598-019-44324-x"
2019-05-28
- "https://www.frontiersin.org/journals/veterinary-science/articles/10.3389/fvets.2019.00241/full"
2019-07-22
- "/doc/cat/psychology/drug/catnip/2011-birkett.pdf"
2011
- "/doc/cat/psychology/drug/catnip/2013-avmf.pdf#page=5"
2013
- "/doc/genetics/heritable/correlation/2020-rosenstrom.pdf"
2020-06-25
- "/doc/psychiatry/lithium/2020-memon.pdf"
2020-07-27
- "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0045086"
2012-08-17
- "https://krebsonsecurity.com/2022/03/hackers-gaining-power-of-subpoena-via-fake-emergency-data-requests/"
2022-03-29
- "https://pubsonline.informs.org/doi/10.1287/mnsc.2022.4643"
2023-01-26
- "https://royalsocietypublishing.org/doi/pdf/10.1098/rstb.2017.0205"
2018-06-04
- "https://web.archive.org/web/20100611071828/http://online.wsj.com/article/SB10001424052748704513104575256452390636786.html" # 2010-06-08
""
- "https://www.bbc.com/future/article/20160406-we-went-to-nasa-to-float-on-the-worlds-flattest-floor"
2016-04-06
- "https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2019.02220/full"
2019-10-03
- "https://www.frontiersin.org/journals/veterinary-science/articles/10.3389/fvets.2021.760845/full"
2021-11-07
- "https://www.nature.com/articles/s41598-020-73426-0"
2020-10-05
- "/doc/cryonics/2015-marcojimenez.pdf"
2015-04-24
- "/doc/cs/algorithm/2017-zhou.pdf"
2017-05-01
- "/doc/darknet-market/2021-bergeron.pdf"
2021-06-28
- "/doc/darknet-market/2021-chen.pdf"
2021-12-07
- "/doc/darknet-market/2022-lee.pdf"
2022-04-06
- "/doc/darknet-market/silk-road/1/2020-ganan.pdf"
2020
- "https://academic.oup.com/qje/article/136/4/1993/6124640"
2021-01-30
- "https://arstechnica.com/gadgets/2023/01/google-announces-official-android-support-for-risc-v/"
2023-01-03
- "https://gki.informatik.uni-freiburg.de/papers/lindner-mattmueller-nebel-xaip2018.pdf"
2019-07-17
- "https://googleprojectzero.blogspot.com/2021/12/a-deep-dive-into-nso-zero-click.html"
2021-12-15
- "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0019875"
2011-04-13
- "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0115253"
2014-12-26
- "https://phiresky.github.io/blog/2021/hosting-sqlite-databases-on-github-pages/"
2021-04-17
- "https://pure.tue.nl/ws/portalfiles/portal/142614149/J.R.Ubbink_09_09_2019_thesis_final.pdf"
2019-09
- "https://www.biorxiv.org/content/10.1101/2021.05.29.446289.full"
2021-05-30
- "https://www.liebertpub.com/doi/full/10.1089/rej.2014.1636"
2015-10-19
- "https://www.nature.com/articles/s41586-024-08502-w" # 2025-02-05
""
- "https://link.springer.com/article/10.1007/s10508-019-01624-7 How Large Are Gender Differences in Toy Preferences? A Systematic Review and Meta-Analysis of Toy Preference Research"
2020-01-27
- "https://www.nature.com/articles/s41598-023-39304-1" # 2023-07-26
2023
- "https://www.nature.com/articles/s44220-025-00390-x" # 2025-02-13
2025
- "/doc/cs/2014-deoliveira.pdf"
2014
- "/doc/dual-n-back/2016-foroughi.pdf"
2016-07-05
- "https://iclr-blogposts.github.io/2024/blog/what-exactly-has-tabpfn-learned-to-do/ What exactly has TabPFN learned to do?"
2024
- "https://schicksalgemeinschaft.wordpress.com/2020/07/01/radiance-carter-scholz-2002/"
2020-07-01
- "https://theconversation.com/an-83-year-old-short-story-by-borges-portends-a-bleak-future-for-the-internet-242998 An 83-year-old short story by Borges portends a bleak future for the internet, Roger J. Kreuz"
2024-11-19
- "https://www.nature.com/articles/s41591-020-0934-0 Human postprandial responses to food and potential for precision nutrition"
2020-06-11
- "/doc/genetics/selection/natural/1979-may-2.pdf"
1979-08-01
- "/doc/genetics/selection/natural/human/2020-chen.pdf"
2020-06-12
- "https://www.theparisreview.org/blog/2016/05/17/the-musicians-day/"
2016-05-17
- "https://notes.stlartsupply.com/the-golden-age-of-japanese-pencils-1952-1967/ The Golden Age of Japanese Pencils, 1952–1967" # 2022-03-02; a web page could not have been published in 1952 or 1967!
""
- "https://www.trendingbuffalo.com/life/uncle-steves-buffalo/everything-from-1991-radio-shack-ad-now/"
2014-01-14
- "https://awards.acm.org/about/2024-turing"
2025-03
- "https://www.theatlantic.com/magazine/archive/2023/09/sam-altman-openai-chatgpt-gpt-4/674764/"
2023-07-24
- "/doc/psychology/personality/conscientiousness/2021-zell-2.pdf"
2021-10-23
- "/doc/sociology/abandoned-footnotes/2018-marquez.pdf"
2018-08-21
- "/doc/statistics/decision/1976-box.pdf"
1976-12
- "https://www.soci.org/Chemistry-and-Industry/CnI-Data/2010/24/Brussels-a-bittersweet-story"
2010-12-20
- "https://80000hours.org/podcast/episodes/joan-rohlfing-avoiding-catastrophic-nuclear-blunders/#the-interaction-between-nuclear-weapons-and-cybersecurity-011018"
2022-03-29
- "https://www.nature.com/articles/s42256-025-01019-5"
2025-03-31
- "https://www.sfsite.com/fsf/2007/gwma0704.htm"
2007-04
- "https://medium.com/@julianmckinlay/total-war-rome-ii-and-creative-assembly-my-statement-ten-years-on-d964f65b0a8f"
2024-06-21
- "https://www.modelshipsinthecinema.com/2016/12/hunt-for-red-october-1990.html"
2016-12
- "https://research.google/blog/all-our-n-gram-are-belong-to-you/ All Our <em>n</em>-gram are Belong to You" # 2006-08-03
""
- "https://www.nature.com/articles/s41467-025-58271-x" # 2025-04-15
2025
- "https://theonion.com/area-man-consults-internet-whenever-possible-1819565463/"
2000-01-26
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC8188931/"
2021-03-15
- "https://www.bbc.com/news/articles/c78jd517z87o"
2025-04-07
- "https://www.wired.com/story/beijing-half-marathon-humanoid-robots/" # 2025-04-19
""
- "https://www.lesswrong.com/posts/KRKnFdECZMu3Ej3z6/can-llms-learn-steganographic-reasoning-via-rl" # 2025-04-11
""
- "https://apnews.com/article/wisconsin-supreme-court-elon-musk-81f71cdda271827ae281a77072a26bad" # 2025-04-02
""
- "https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3002205" # 2024-02-01
""
- "https://news.ycombinator.com/item?id=35560022" # 2023-04-13
""
- "https://scholar.google.com/citations?user=iyD9aw8AAAAJ&hl=en&oi=ao"
""
- "https://www.argmin.net/p/rossis-metallic-rules" # 2025-04-22
""
- "https://www.cs.cmu.edu/~junyanz/"
""
- "https://en.wikipedia.org/wiki/Digital_dark_age"
""
- "https://en.wikipedia.org/wiki/Special_interest_(autism)"
""
- "https://www.nature.com/articles/d41586-025-01266-x" # 2025-04-25
2025
- "https://www.newyorker.com/culture/the-weekend-essay/will-the-humanities-survive-artificial-intelligence" # 2025-04-26
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC10700140/" # 2023-11-22
""
- "https://en.wikipedia.org/wiki/Buffalo_Bill%27s"
""
- "https://en.wikipedia.org/wiki/David_Baker_(biochemist)"
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC4197971/ Prevalence of Mental Disorders, Psychological Distress, and Mental Health Services Use Among Lesbian, Gay, and Bisexual Adults in the United States Susan D. Cochran, J. Greer Sullivan, Vickie M. Mays"
2003-02
- "https://en.wikipedia.org/wiki/Hypercycle_(chemistry)"
""
- "https://archive.org/details/writingilluminat00johnuoft/page/193/mode/1up <em>Writing &amp; illuminating, &amp; lettering</em> : Johnston, Edward, 1872–1944" # 1917
""
- "https://en.wikipedia.org/wiki/Mark_Twain"
""
- "https://mathshistory.st-andrews.ac.uk/Biographies/Dase/ Zacharias Dase (1824—1861)—Biography—MacTutor History of Mathematics"
""
- "https://smoothbrains.net/posts/2025-04-29-xenon-and-nitrous-oxide.html"
2025-04-29
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC9331065/ Tool Use in Horses" # 2022-07-22
""
- "https://github.com/ytmytm/llama2.c64/tree/main#llama2c64" # 2025-03-08
""
- "https://publicdomainreview.org/collection/little-screw/ Nikolai Agnivtsev’s <em>Little Screw</em> (1925)" # 2025-04-30
""
- "https://www.socialmediatoday.com/news/x-formerly-twitter-continues-to-lose-eu-users/746539/" # 2025-04-28
""
- "https://www.newsweek.com/elon-musk-wears-hats-trump-cabinet-amid-report-hes-working-home-2066305 Elon Musk Wears Two Hats with Trump Cabinet Amid Report He’s Working from Home" # 2025-04-30
""
- "https://asteriskmag.com/issues/09/deros-and-the-ur-abduction" # 2025-01
""
- "https://speechmap.substack.com/p/chinese-open-source-model-roundup?open=false#%C2%A7the-chimera Chinese Open Source Model Roundup: DeepSeek, Qwen3, variants and more" # 2025-05-01
""
- "https://www.nytimes.com/2025/05/02/health/snakes-universal-antivenom-tim-friede.html"
2025-05-02
- "https://en.wikipedia.org/wiki/Kay_Granger#Retirement"
""
- "https://www.ft.com/content/c04389a3-c672-43ce-8d9e-724668c0e490" # 2025-05-02
""
- "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5216904" # 2025-04-25
""
- "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3246856/" # 2012-01
""
- "https://blog.janestreet.com/the-joy-of-expect-tests/" # 2023-01-09
""
- "https://goodsniff.substack.com/p/creating-bluey-tales-from-the-art-891" # 2025-04-08
""
- "https://en.wikipedia.org/wiki/L%C3%A1szl%C3%B3_Polg%C3%A1r#Polg%C3%A1r_sisters"
""
- "https://www.nature.com/articles/s41598-020-66657-8" # 2020-07-09
""
- "https://research.google/blog/re-weighted-gradient-descent-via-distributionally-robust-optimization/" # 2023-09-28
""
- "https://languagelog.ldc.upenn.edu/nll/?p=3546" # 2011-11-06
""
- "https://www.wired.com/story/60-hour-dance-sessions-simulated-sex-and-ketamine-inside-the-world-of-hardcore-vr-ravers/" # 2025-05-05
""
- "http://static.userland.com/userlanddiscussarchive/msg010165.html" # 1999-08-27
""
- "https://felixrieseberg.github.io/clippy/" # 2025-05-05
""
- "https://publicdomainreview.org/collection/durer-knots/ Tangled Dürer: The 6 Knots (ca. before 1521)" # 2025-05-06
""
- "https://news.ycombinator.com/item?id=26223377" # 2021-02-22
""
- "https://nymag.com/intelligencer/article/openai-chatgpt-ai-cheating-education-college-students-school.html" # 2025-05-07
""
- "https://www.astralcodexten.com/p/testing-ais-geoguessr-genius" # 2025-05-02
""
- "https://stevenadler.substack.com/p/is-chatgpt-actually-fixed-now?open=false#%C2%A7what-evaluations-did-i-run" # 2025-05-08
""
- "https://avalovelace1.github.io/LegoGPT/" # 2025-05-08
""
- "https://www.examinerlive.co.uk/news/west-yorkshire-news/bravery-huddersfield-girl-7-who-10735352" # 2016-01-15
""
- "https://www.nature.com/articles/s41598-022-10899-1" # 2022-05-26
""
- "https://www.nationalgeographic.com/science/article/have-the-hunting-habits-of-leopards-shaped-primate-evolution" # 2010-04-06
""
- "https://grantslatton.com/claude-code" # 2025-05-12
""
- "https://liuziwei7.github.io/"
""
- "https://researchers.mgh.harvard.edu/profile/13438682/Andrea-Ganna"
""
- "https://nospank.net/adah1.htm Adah Maurer (1905–1998): A Remembrance" # 1998-03
""
- "https://www.rifters.com/crawl/?p=11511" # 2025-05-12
""
- "https://altered.substack.com/p/goodbye-self" # 2025-03-20
""
- "https://altered.substack.com/p/meditation-update" # 2025-05-12
""
- "https://developers.googleblog.com/en/gemini-2-5-video-understanding/" # 2025-05-09
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC6821400/" # 2019-10-30
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC9999677/" # 2022-08-19
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC10721691/" # 2023-06-27
""
- "https://hpmor.com/chapter/7" # 2010-02-28
""
- "https://physics.aps.org/articles/v18/99" # 2025-05-09
""
- "https://habr.com/en/articles/454376/" # 2019-06-03
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC6172040/" # 2018-02-21
""
- "https://www.chinatalk.media/p/xi-takes-an-ai-masterclass" # 2025-05-13
""
- "https://www.nature.com/articles/s41366-025-01795-5" # 2025-05-11
""
- "https://robhorning.substack.com/p/font-activations" # 2025-05-16
""
- "https://suberic.net/~dmm/projects/mystical/README.html" # 2025-05-16
""
- "https://lukeplant.me.uk/blog/posts/less-powerful-languages/" # 2015-11-14
""
- "https://github.com/moderninterpreters/markup" # 2019-10
""
- "https://progressandpoverty.substack.com/p/how-georgists-valued-land-in-the" # 2025-05-19
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC12077490/" # 2025-05-14
""
- "https://www.nbcnews.com/politics/elections/elon-musk-says-lot-less-political-spending-rcna207944" # 2025-05-20
""
- "https://www.lesswrong.com/posts/xMGmibZpPDnawjHXk/generating-the-funniest-joke-with-rl-according-to-gpt-4-1" # 2025-05-16
""
- "https://www.bmj.com/content/384/bmj-2023-076410" # 2024-01-29
""
- "https://www.livescience.com/health/food-diet/people-on-ozempic-start-disliking-meat-and-fried-foods-were-starting-to-learn-why" # 2025-05-10
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC4413784/" # 2013-11-11
""
- "https://www.lesswrong.com/posts/tGcLA596E8g3KnphE/survey-of-multi-agent-llm-evaluations" # 2025-05-22
""
- "https://charliesabino.com/caesars-last-breath/" # 2025-05-23
""
- "https://x.com/jayelmnop/status/1925632308968628647" # 2025-05-23
""
- "https://www.lesswrong.com/posts/TBk2dbWkg2F7dB3jb/it-s-really-hard-to-make-scheming-evals-look-realistic" # 2025-05-24
""
- "https://resobscura.substack.com/p/why-were-belle-epoque-cities-beautiful" # 2025-05-21
""
- "https://www.benkuhn.net/impatient/" # 2020-07
""
- "https://smagin.fyi/posts/ordinary-life-improvements/" # 2025-05-25
""
- "https://x.com/geoffreyirving/status/1924208903438356582" # 2025-05-18
""
- "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5115145" # 2025-03-27
""
- "https://www.kenklippenstein.com/p/read-elias-rodriguezs-leaked-chats" # 2025-05-27
""
- "https://forum.cursor.com/t/important-claude-has-learned-how-to-jailbreak-cursor/96702" # 2025-05-26
""
- "https://publicdomainreview.org/collection/an-anciente-mappe-of-fairyland/" # 2025-05-28
""
- "https://www.nber.org/papers/w33818" # 2025-05
""
- "https://english.radio.cz/justice-minister-blazek-resign-over-bitcoin-case-8852580" # 2025-05-30
""
- "https://www.reuters.com/business/anthropic-hits-3-billion-annualized-revenue-business-demand-ai-2025-05-30/"
2025-05-30
- "https://www.straitstimes.com/asia/east-asia/elon-musk-arrives-in-japan-for-first-visit-since-2014" # 2024-11-15
""
- "https://www.lesswrong.com/posts/QYAfjdujzRv8hx6xo/unfaithful-reasoning-can-fool-chain-of-thought-monitoring" # 2025-06-02
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC11186750/" # 2024-06-19
""
- "https://nautil.us/finding-peter-putnam-1218035/" # 2025-06-17
""
- "https://www.straitstimes.com/world/turbulence-ahead-how-used-cooking-oil-could-hinder-aviations-green-fuel-hopes" # 2025-06-24
""
- "https://www.reuters.com/world/americas/sinaloa-cartel-hacked-phones-surveillance-cameras-find-fbi-informants-doj-says-2025-06-27/"
2025-06-27
- "File:Ganku 岸駒—Tiger and Bamboo—2017-196—Princeton University Art Museum.jpg" # not 2017, because that's actually the accession number: "Accession number: 2017-196 (Princeton University Art Museum) Edit this at Wikidata"
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC5745404/" # 2017-12-13
""
- "https://x.com/QiaochuYuan/status/1927566847739564145" # 2025-05-27
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC12089086/" # 2025-05-19
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC9088817/" # 2022-04-12
""
- "https://www.lesswrong.com/posts/CM7AsQoBxDW4vhkP3/optimizing-the-final-output-can-obfuscate-cot-research-note" # 2025-07-30
""
- "https://x.com/elder_plinius/status/1952829605653749768" # 2025-08-06
""
- "https://www.reuters.com/business/openai-eyes-500-billion-valuation-potential-employee-share-sale-source-says-2025-08-06/"
2025-08-06
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC9209381/" # 2022-03-21
""
- "https://metr.github.io/autonomy-evals-guide/gpt-5-report/" # 2025-08-07
""
- "https://news.ycombinator.com/item?id=43764108" # 2025-04-22
""
- "https://lwn.net/Articles/1001773/" # 2025-06-06
""
- "https://qntm.org/chatscp" # 2025-07-21
""
- "https://blog.google/products/maps/sheep-view-where-theres-wool-theres-way/" # 2016-08-31
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC11437224/" # 2024-09-13
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC10186594/" # 2023-03-20
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC9455625/" # 2022-08-24
""
- "https://brave.com/blog/comet-prompt-injection/" # 2025-08-20
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC11933992/" # 2025-03-24
""
- "https://sunaku.github.io/moergo-glove80-keyboard.html" # 2025-01-13
""
- "https://www.wired.com/story/oral-history-doge-federal-workers/" # 2025-09-26
""
- "https://mjg59.dreamwidth.org/73317.html" # 2025-09-24
""
- "https://www.dopaminemarkets.com/p/how-did-sports-betting-become-legal#%C2%A7paspa-banning-sports-gambling-in"
2025-09-21
- "this post was submitted on 29 Jul 2025"
2025-07-29
- "https://scottaaronson.blog/?p=9183" # 2025-09-27
""
- "https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Accept"
""
- "https://www.lesswrong.com/posts/Bt53XFaMYTseq7oMH/i-have-decided-to-stop-lying-to-americans-about-9-11" # 2025-09-29
""
- "https://www.anthropic.com/news/claude-sonnet-4-5" # 2025-09-30
""
- "https://webkit.org/blog/17285/rolling-the-dice-with-css-random/" # 2025-08-21
""
- "https://www.nature.com/articles/s41467-025-63454-7" # 2025-09-30
""
- "https://www.lesswrong.com/posts/j3gp8tebQiFJqzBgg/how-the-nanogpt-speedrun-wr-dropped-by-20-in-3-months" # 2025-10-04
""
- "https://github.com/karpathy/nanochat/discussions/1" # 2025-10-13
""
- "https://www.nature.com/articles/s41598-025-18636-0" # 2025-10-09
""
- "https://www.lesswrong.com/posts/EGGruXRxGQx6RQt8x/situational-awareness-a-one-year-retrospective" # 2025-06-23; _Situational Awareness_ was published 2024-06-06
2025-06
- "https://frontofficesports.com/18th-century-law-fuels-legal-blitz-on-sports-prediction-markets/" # 2025-06-27
""
- "https://nrehiew.github.io/blog/long_context/" # 2025-10-18
""
- "https://every.to/vibe-check/vibe-check-we-tested-claude-sonnet-4-5-for-writing-and-editing" # 2025-10-23; Sonnet-4.5 was released September 2025, so given the speed of AI model turnover we can infer that it was benchmarked sometime in late 2025 (and probably not in 2026, having almost certainly been superseded)... but nothing more precise than that.
2025
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC4724474/" # 2015-11-06
""
- "https://tonsky.me/blog/syntax-highlighting/" # 2025-10-15
""
- "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0269544" # 2022-07-14
2022
- "https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1002894" # 2013-01-31
2013
- "https://www.lesswrong.com/posts/EKhTrhrCz2rNg7FmG/learning-to-interpret-weight-differences-in-language-models-1" # 2025-10-06
""
- "https://www.cremieux.xyz/p/ozempic-and-muscle-mass" # 2025-05-08
""
- "https://www.lesswrong.com/w/writing-communication-method"
""
- "https://www.lesswrong.com/w/good-explanations-advice"
""
- "https://www.stafforini.com/blog/summary-of-clear-and-simple-as-the-truth/ Summary of <em>Clear and Simple as the Truth</em>, by Francis-Noël Thomas & Mark Turner" # 2013-06-26
""
- "https://cacm.acm.org/opinion/retrospective-an-axiomatic-basis-for-computer-programming/" # 2009-10-01
""
- "https://www.lesswrong.com/posts/eQvNBwaxyqQ5GAdyx/some-data-from-leelapieceodds" # 2025-10-28
""
- "https://www.lesswrong.com/posts/qvWP3aBDBaqXvPNhS/gpt-2-s-positional-embedding-matrix-is-a-helix" # 2023-07-20; note GPT-2 was announced in 2019-02, so references to it cannot predate 2019
""
- "https://camilleberger.substack.com/p/your-movielike-ai-assistant-will" # 2025-11-04
""
- "https://moonshotai.github.io/Kimi-K2/thinking.html" # 2025-11-06
2025
- "https://psychotechnology.substack.com/p/a-love-song-to-nicotine-630" # 2025-11-06
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC12129667/" # 2025-06-03
""
- "https://www.rferl.org/a/kremlin-trickery-putin-offices-secrecy-investigation/33586451.html" # 2025-11-11
""
- "https://blog.google/products/gemini/gemini-3/" # 2025-11-18; but you should know Google Gemini-2.5 was 2024–2025, and Gemini-4 would be ~2026, so Gemini-3 must be 2025
2025
- "https://www.lesswrong.com/posts/Nr4Mca2WGhv4jyvfh/literacy-is-decreasing-among-the-intellectual-class Literacy is Decreasing Among the Intellectual Class" # 2025-11-22
""
- "https://www.anthropic.com/news/claude-opus-4-5" # 2025-11-24
2025
- "https://theonion.com/more-u-s-children-being-diagnosed-with-youthful-tenden-1819565754/" # 2000-09-27
2000
- "https://www.lesswrong.com/posts/xEPiojzEzQexafcBR/information-hygiene" # 2025-11-26
""
- "https://futuring.substack.com/p/wearing-dresses-is-cheating-an-experiment" # 2025-11-13
""
- "https://research.swtch.com/nih" # 2023-10-25
""
- "https://www.nature.com/articles/s41586-025-09761-x#deepmind" # 2025-10-22
2025
- "https://www.sciencedirect.com/science/article/pii/S016748701630277X" # 2017-12
""
- "https://www.lesswrong.com/posts/vpNG99GhbBoLov9og/claude-4-5-opus-soul-document" # 2025-11-28
2025
- "https://www.pnas.org/doi/10.1073/pnas.2415254122" # 2025-03-24
""
- "https://www.lesswrong.com/posts/u6Lacc7wx4yYkBQ3r/insights-into-claude-opus-4-5-from-pokemon" # 2025-12-04
2025
- "https://journals.sagepub.com/doi/10.1177/25152459231213375" # 2024-02-05
""
- "https://journals.sagepub.com/doi/full/10.1177/09567976251392219" # 2025-11-21
""
- "https://www.sciencedirect.com/science/article/pii/S0960982221008769" # 2021-09-13
""
- "https://vimeo.com/41669642 Postcard From 1952" # 2012-05-06
""
- "https://blog.google/products/gemini/updated-image-editing-model/" # 2025-08-26
2025
- "https://blog.google/technology/ai/nano-banana-pro/" # 2025-11-20
2025
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC3311762/" # 2013-03-20
""
- "https://go.dev/blog/go-fonts" # 2016-11-16
""
- "https://time.com/archive/6732124/of-headless-mice-and-men/" # 1998-01-19
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC6743687/" # 2019-03-26
2019
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC8723833/" # 2022-05-12
2022
- "https://openai.com/index/introducing-gpt-5-2/" # 2025-12-11; GPT-5 was introduced in 2025, so it must be ≥2025
2025
- "https://www.lesswrong.com/posts/ThST9njmesR9BkoWq/book-review-orality-and-literacy-the-technologizing-of-the" # 2023-10-28
""
- "https://www.sanity.io/blog/you-should-never-build-a-cms" # 2025-12-14
""
- "https://github.com/ctrlcctrlv/kjv1611 ctrlcctrlv/kjv1611: A complete digital OpenType font restoration of the typeface found in the 1611 King James Bible" # 2018-03-27
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC2790397/" # 2010-10
2010
- "https://marcusolang.substack.com/p/im-kenyan-i-dont-write-like-chatgpt" # 2025-07-08; but could've been written any time after 2022-11...
""
- "https://ashvardanian.com/posts/search-utf8/" # 2025-12-15
""
- "https://eyeondesign.aiga.org/a-very-niche-very-logical-font-inspired-by-vintage-typewriters-python-script/" # 2021-04-20
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC8640740/" # 2021-11-22
""
- "https://wang-kevin3290.github.io/scaling-crl/"
""
- "https://www.pnas.org/doi/abs/10.1073/pnas.2413064122"
2025-03-31
- "https://www.science.org/doi/full/10.1126/scirobotics.adu8009" # 2025-12-10
""
- "https://github.com/DGoettlich/history-llms" # 2025-12-18
""
- "https://openai.com/index/introducing-gpt-5-2-codex/" # 2025-12-18
2025
- "https://www.lesswrong.com/posts/qwAiKvomuAm5ekC4D/subliminal-learning-transmitting-misalignment-via" # 2025-12-17
""
- "https://www.wired.com/story/hearing-aid-startup-ai-fortell/" # 2025-12-03
""
- "https://animationobsessive.substack.com/p/the-toy-story-you-remember" # 2025-11-09
""
- "https://www.youtube.com/watch?v=cDA3_5982h8" # 2017-01-26
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC12086928/" # 2025-05-15
""
- "https://onlinelibrary.wiley.com/doi/full/10.1002/oby.70058" # 2025-10-02
""
- "https://www.novonordisk.com/content/nncorp/global/en/news-and-media/news-and-ir-materials/news-details.html?id=916472" # 2025-12-22
""
- "https://blog.glyphdrawing.club/font-with-built-in-syntax-highlighting/" # 2024-01-01
""
- "https://bfswa.substack.com/p/6-years-after-too-much-crypto" # 2025-11-16
""
- "/system-prompts-2025" # 2025-12-24
""
- "https://www.nature.com/articles/s41598-023-36256-4" # 2023-06-08
""
- "https://www.science.org/doi/10.1126/sciadv.adx6918" # 2025-10-29
""
- "https://www.lawgazette.co.uk/law/high-court-grants-chancellor-a-600m-windfall/5111247.article" # 2022-01-21
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC12148925/" # 2025-06-03
""
- "https://www.nature.com/articles/s41562-025-02359-3" # 2025-12-23
2025
- "https://samuelalbanie.substack.com/p/reflections-on-2025" # 2025-12-30 but 'Reflections on $YEAR" could be published at any time during or after that $YEAR, so can't guess either 2025 *or* 2026 etc...
""
- "https://journals.plos.org/plosbiology/article?id=10.1371%2Fjournal.pbio.2001402" # 2017-01-12
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC5116034/" # 2016-10-14
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC3546926/" # 2013-01-16
""
- "https://craftsmanship.substack.com/p/620-draft-calligraphys-magicians" # 2025-06-20
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC6911682/" # 2019-12-06
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC4743270/" # 2016-01-28
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC11846834/" # 2025-02-22
""
- "https://pmc.ncbi.nlm.nih.gov/articles/PMC11485236/" # 2024-10-09
""
- "https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Properties/text-wrap"
""
- "https://martinalderson.com/posts/which-programming-languages-are-most-token-efficient/" # 2026-01-08
""
- "https://lab174.com/blog/202601-yaml-norway/" # 2026-01-12
2026-01
- "https://willhbr.net/2025/10/20/light-mode-infffffflation/"
2025-10-20
- "https://wikiedu.org/blog/2026/01/29/generative-ai-and-wikipedia-editing-what-we-learned-in-2025/ Generative AI and Wikipedia editing: What we learned in 2025"
2026-01-29
- "https://lettersfrombethlehem.substack.com/p/so-you-want-to-go-to-next-years-inkhaven" # 2025-12-01
""

Task:

- """ + target + "\n"

    completion = client.chat.completions.create(
        model="gpt-5-mini",
        messages=[
            {"role": "system", "content": "You are a researcher and web developer, compiling a bibliography."},
            {"role": "user", "content": prompt}
        ],
        temperature=1,          ## temperature=0, # deterministic extraction of dates
        top_p=1,
        ## max_completion_tokens=11,          # you only need one short token sequence like 'YYYY-MM-DD\n'
        ## stop=["\n"]             # stop at first newline; discourages rambles
    )

    date_str = completion.choices[0].message.content.strip()

    # Handle empty string case (both with and without quotes)
    if date_str in ['""', '" "', "''", ""]:
        print('""')
        return

    # Validate the returned date
    if not validate_date_format(date_str):
        print(f"Error: Invalid date format. Got: {date_str}")
        sys.exit(1)
    if not validate_date_not_future(date_str):
        print(f"Error: Future or invalid date detected. Got: {date_str}")
        sys.exit(1)

    print(date_str)

if __name__ == "__main__":
    main()
