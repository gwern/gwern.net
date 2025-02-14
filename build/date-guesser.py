#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# date-guesser.py: extract recent dates in YYYY[[-MM]-DD] format from natural language inputs or structured text like URLs
# Author: Gwern Branwen
# Date: 2024-08-21
# When:  Time-stamp: "2025-02-13 12:33:30 gwern"
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
        # Parse the date string based on its format
        if len(date_str) == 4:  # YYYY
            date = datetime.strptime(date_str, '%Y')
            return date.year <= today.year
        elif len(date_str) == 7:  # YYYY-MM
            date = datetime.strptime(date_str, '%Y-%m')
            return date.year < today.year or (date.year == today.year and date.month <= today.month)
        elif len(date_str) == 10:  # YYYY-MM-DD
            date = datetime.strptime(date_str, '%Y-%m-%d')
            return date <= today
        return False
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
If there is more than one valid date, print only the first one.
Do not make up dates; if you are unsure, print only the empty string "".
Do not add any commentary or explanations.
For Arxiv URLs, only the first 4 numbers are meaningful, in the form: `YYMM`; the numbers after the period are an ID and the date *cannot* be guessed from them!
For archived URLs such as Internet Archive URLs, the date of the snapshot is *not* the date of publication.

Task examples (with explanations in '#' comments):

"Published: 02-29-2024 | https://example.com/leap-year-article"
""
# Invalid: 2024 is not a leap year
- "Date: 04/31/2024: Article about calendars"
""
# Invalid: April has 30 days
- "Article from Sep 31, 2023 about date formats"
""
# Invalid: September has 30 days
- "Updated 2024.13.01 with new information"
""
# Invalid month 13
- "Version: 2024.00.01"
""
# Invalid month 0
- "Created on 2024-04-00"
""
# Invalid day 0
- "Last modified: 31/12/2023 23:59:59 UTC"
2023-12-31  # Should extract just the date portion
- "Posted: Yesterday at 3pm"
""
# Relative dates should be ignored, as who knows what they are *actually* relative to?
- "Updated: 2 days ago"
""
# Relative dates should be ignored
- "EST Release Date: 12/13/23"
2023-12-13  # Should handle American date format
- "Published Date: 13/12/23"
2023-12-13  # Should handle European date format if unambiguous
- "From our 2023-13-12 edition"
""
# Invalid: month 13
- "Article from Feb 30th, 2023"
""
# Invalid: February never has 30 days
- "Created: 2024.02.29"
""
# Invalid: 2024 is not a leap year
- "Date of record: 1999-11-31"
""
# Invalid: November has 30 days
- "TimeStamp:20240229123456"
""
# Invalid: not a leap year
- "YYYYMMDD: 20241301"
""
# Invalid month 13
- "Released between 2023Q4 and 2024Q1"
""
# Too ambiguous
- "Copyright (c) 2020-2024"
2020  # Take earliest year when given a range
- "Volume 23, Issue 45 (Winter 2023-2024)"
2023  # Take earliest year when given a range
- "Academic Year 2023/24"
2023  # Take earliest year when given a range
- "Originally written in 2022, updated for 2024"
2022  # Take original/earliest date
- "Written 2024/04/31: Edited 2024/05/01"
""
# Invalid first date (April has 30 days)
- "Looking back at the year 2000 problem (Y2K)"
"" # Don't extract Y2K as it's a topic, not a date
# NOTE: ArXiv IDs are in the form YYMM.NNNNN (since 2015), representing the year, month, and submission index that month​. For example, 1907.07174 = July 2019, 7174th submission. A version suffix (v1, v2, …) may follow for revisions
- "A paper from arXiv:2401.12345"
2024-01  # Extract date from arXiv ID
- "DOI: 10.1234/journal.2024.01.0123"
2024-01  # Extract date from DOI when unambiguous
- "Posted on 29/02/2023 about leap years"
""
# Invalid: 2023 wasn't a leap year
- "Article 123456789 from 2024-W01"
2024  # ISO week dates should return just the year
- "Publication: 2024. Journal of Examples"
2024  # Handle trailing period after year
- "From the 1990's collection"
""
# Don't extract dates from decades
- "Temperature was 2023.5 degrees"
""
# Don't extract decimal numbers as dates
- "Score was 2024:1"
""
# Don't extract scores/ratios as dates
- "Published at 2024hrs on Jan 1"
""
# Don't extract 24-hour time as year
- "Build version 2024.01.alpha"
2024-01  # Software versions can contain valid dates
- "Model: RTX 2080 Ti"
""
# Don't extract product numbers as dates
- "ISBN: 1-234567-890-2024"
""
# Don't extract years from ISBNs
- "Highway 2024 South"
""
# Don't extract road numbers as dates
- "Room 2024B, Building 3"
""
# Don't extract room numbers as dates
- "Postal code 2024AB"
""
# Don't extract postal codes as dates
- "In the year 2525, if man is still alive"
""
# Song lyric about future: not a publication date
- "Article accepted 2024-02-30, published 2024-03-01"
2024-03-01  # First date invalid, use second valid date
- "File: IMG_20241301_123456.jpg"
""
# Invalid date in filename (month 13)
- "Updated 24-01-15"
2024-01-15  # Assume 2-digit years >= 24 are 2024+
- "Updated 95-01-15"
1995-01-15  # Assume 2-digit years < 24 are 1900s
- "Patent №2024-123456"
""
# Don't extract patent numbers as dates
- "SKU: 20240123-ABC"
""
# Don't extract SKU/product codes as dates
- "Contact: +1 (202) 555-2024"
""
# Don't extract phone numbers as dates
- "Sample #2024-01-A5"
2024-01  # Laboratory sample IDs can contain valid dates
- "hex color #202420"
""
# Don't extract hex colors as dates
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
- "We published this on the first of May after Obama’s re-election."
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
- "The 3rd Thursday of the month after next year after 2024’s vernal equinox"
2025-03
- "https://www.nature.com/articles/s41586-023-06185-3"
""
- "Pi Day five years from the last leap year"
2029-03-14
- "The 100th day of the Chinese Year of the Dragon in the 2020s"
2024-04-09
- "Election Day 2028 in the United States"
2028-11-07
- "The day Halley’s Comet is next expected to be visible from Earth"
2061-06
- "https://en.wikipedia.org/wiki/Year_2038_problem"
2038-01-19
- "The 250th anniversary of the signing of the Declaration of Independence"
2026-07-04
- "Last business day of Q3 2025"
2025-09-30
- "Ramadan start date in 2030"
2030-01-05
- "The day after tomorrow, three years ago"
""
- "https://www.youtube.com/watch?v=dQw4w9WgXcQ&t=42s"
""
- "2012-21-01, https://www.sciencedirect.com/science/article/pii/S0032579119394003, 25 years of selection for improved leg health in purebred broiler lines and underlying genetic parameters"
2012-12-01
- "2020-05-1, https://www.sciencedirect.com/science/article/pii/S1053811920301786, Educational attainment polygenic scores are associated with cortical total surface area and regions important for language and memory"
2020-05-15
- "2023-009-04, https://www.frontiersin.org/journals/psychiatry/articles/10.3389/fpsyt.2023.1246149/full, Modafinil's effects on cognition and sleep quality in affectively-stable patients with bipolar disorder: a pilot study"
2023-09-03
- "2023-112-08, https://www.reuters.com/world/uk/uk-antitrust-regulator-considering-microsoft-openai-partnership-2023-12-08/, Microsoft, OpenAI tie-up comes under antitrust scrutiny"
2023-12-08
- "2022-010-17, https://osf.io/preprints/psyarxiv/tnyda/, Does the mere presence of a smartphone impact cognitive performance? A meta-analysis of the brain drain effect"
2022-10-17
- "2024-06-7, https://www.newyorker.com/culture/the-front-row/flipside-is-a-treasure-trove-of-music-and-memory, <em>Flipside</em> Is a Treasure Trove of Music and Memory: Chris Wilcha's documentary explores life, love, and art through his connection to a venerable record store"
2024-06-07
- "The 2024 Olympic Games in Paris: A Look Ahead - Sports Illustrated"
""
- "https://web.archive.org/web/20100915000000*/http://example.com"
2010-09-15
- "COVID-19: Two Years Later - A Retrospective (Published on March 11, 2022)"
2022-03-11
- "Apple WWDC 2023 Keynote: iOS 17, macOS 14, and More"
2023-06-05
- "SpaceX's Starship: From Prototype to Orbit (2019-2023)"
2023
- "https://doi.org/10.1038/s41586-021-03819-2"
""
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
- "https://arxiv.org/abs/2308.04445"
2023-07-31
- "https://arxiv.org/abs/1809.00946"
2018-08-26
- "https://arxiv.org/abs/1907.07174"
2020-01-08
- "https://arxiv.org/abs/2009.03300"
2020-09-07
- "https://arxiv.org/abs/2010.14571#google"
2020-10-27
- "https://arxiv.org/abs/2205.04596#google"
2022-05-09
- "https://arxiv.org/abs/2205.09073#google"
2022-05-18
- "https://arxiv.org/abs/1808.03715#google"
2018-08-10
- "https://arxiv.org/abs/2206.04658#nvidia"
2022-06-09
- "https://arxiv.org/abs/1710.05941#google"
2017-10-16
- "https://arxiv.org/abs/1805.08974#google"
2018-05-23
- "https://arxiv.org/abs/1808.08866"
2018-08-27
- "https://arxiv.org/abs/1809.01694"
2018-09-05
- "https://arxiv.org/abs/1909.07528#openai"
2019-09-17
- "https://colah.github.io/posts/2014-03-NN-Manifolds-Topology/"
2014-04-06
- "https://arxiv.org/abs/2006.07733#deepmind"
2020-06-13
- "https://arxiv.org/abs/2012.13349#deepmind"
2020-12-23
- "https://www.nature.com/articles/s41598-020-79310-1"
2021-01-11
- "https://arxiv.org/abs/2103.04689"
2021-03-08
- "https://arxiv.org/abs/1609.01596"
2016-09-06
- "https://arxiv.org/abs/1703.03664#deepmindopenai"
2017-03-10
- "https://arxiv.org/abs/1703.06870#facebook"
2017-03-20
- "https://arxiv.org/abs/1802.06416#huawei"
2018-02-18
- "https://arxiv.org/abs/1910.11015"
2019-11-07
- "https://arxiv.org/abs/2003.02139"
2020-03-04
- "https://arxiv.org/abs/2003.10580#google"
2021-01-05
- "https://arxiv.org/abs/2004.10802"
2020-04-22
- "https://arxiv.org/abs/2006.10029#google"
2020-06-17
- "https://arxiv.org/abs/2006.11239"
2020-06-19
- "https://arxiv.org/abs/2007.08558#google"
2020-07-16
- "https://arxiv.org/abs/2008.09037"
2020-08-20
- "https://arxiv.org/abs/2102.09672#openai"
2021-02-18
- "https://arxiv.org/abs/2103.06877#facebook"
2021-03-11
- "https://arxiv.org/abs/2105.05233#openai"
2021-05-11
- "https://arxiv.org/abs/2211.01324#nvidia"
2022-11-02
- "https://arxiv.org/abs/2311.09257#google"
2023-11-14
- "https://arxiv.org/pdf/2307.01952#page=3&org=stability"
2023-07-04
- "https://www.biorxiv.org/content/10.1101/2021.02.02.429430.full"
2021-04-29
- "https://arxiv.org/abs/1610.09027#deepmind"
2016-10-27
- "https://arxiv.org/abs/1611.02779#openai"
2016-11-09
- "https://arxiv.org/abs/1711.06744"
2017-11-17
- "https://arxiv.org/abs/1804.01118#deepmind"
2018-04-03
- "https://arxiv.org/abs/1804.07209"
2018-04-19
- "https://arxiv.org/abs/1806.04498"
2018-06-12
- "https://arxiv.org/abs/1810.01365"
2018-10-02
- "https://arxiv.org/abs/1812.02353#google"
2018-12-06
- "https://arxiv.org/abs/1905.12616#allen"
2019-05-29
- "https://arxiv.org/abs/1907.07171"
2019-07-16
- "https://arxiv.org/abs/1909.01387#deepmind"
2019-09-03
- "https://arxiv.org/abs/1909.05858#salesforce"
2019-09-11
- "https://arxiv.org/abs/1911.08265#deepmind"
2019-11-19
- "https://arxiv.org/abs/2001.08361#openai"
2020-01-23
- "https://arxiv.org/abs/2002.08909#google"
2020-02-10
- "https://arxiv.org/abs/2004.10450#google"
2020-04-22
- "https://arxiv.org/abs/2006.04768#facebook"
2020-06-08
- "https://arxiv.org/abs/2006.06676#nvidia"
2020-06-11
- "https://arxiv.org/abs/2006.10738"
2020-06-18
- "https://arxiv.org/abs/2006.15720"
2020-06-28
- "https://arxiv.org/abs/2007.13657"
2020-07-27
- "https://arxiv.org/abs/2010.05315"
2020-10-11
- "https://arxiv.org/abs/2101.04702#google"
2021-01-12
- "https://arxiv.org/abs/2103.03206#deepmind"
2021-03-04
- "https://arxiv.org/abs/2104.14830#google"
2021-04-30
- "https://arxiv.org/abs/2106.06981"
2021-06-13
- "https://arxiv.org/abs/2106.07477#baidu"
2021-06-14
- "https://arxiv.org/abs/2106.09488#amazon"
2021-06-11
- "https://arxiv.org/abs/2106.12423#nvidia"
2021-06-23
- "https://arxiv.org/abs/2107.01294#allen"
2021-07-02
- "https://arxiv.org/abs/2107.07566#facebook"
2021-07-15
- "https://arxiv.org/abs/2107.14795#deepmind"
2021-07-30
- "https://arxiv.org/abs/2108.07732#google"
2021-08-16
- "https://arxiv.org/abs/2203.08913#google"
2021-10-05
- "https://arxiv.org/abs/2207.10551#google"
2022-07-21
- "https://arxiv.org/pdf/1606.03498#page=3&org=openai"
2016-06-10
- "https://arxiv.org/pdf/1809.11096#page=6&org=deepmind"
2019-08-26
- "https://arxiv.org/pdf/1809.11096#page=8&org=deepmind"
2018-09-28
- "https://web.archive.org/web/20230101012202/https://thispersondoesnotexist.com/"
2019-02-12
- "https://www.nature.com/articles/s41467-018-04316-3"
2018-06-19
- "https://arxiv.org/abs/1810.01398"
2018-10-02
- "https://arxiv.org/abs/2002.12327"
2020-11-09
- "https://arxiv.org/abs/2012.12877#facebook"
2020-12-23
- "https://arxiv.org/abs/2106.05237#google"
2021-06-09
- "https://arxiv.org/abs/1802.02271"
2018-02-07
- "https://arxiv.org/abs/1908.10396#google"
2019-08-27
- "https://arxiv.org/abs/1910.01055#google"
2019-10-02
- "https://arxiv.org/abs/2002.05645#microsoft"
2020-10-16
- "https://arxiv.org/abs/2101.03961#google"
2021-01-11
- "https://arxiv.org/abs/2104.05158#facebook"
2021-04-12
- "https://arxiv.org/abs/1801.10447"
2018-01-31
- "https://arxiv.org/abs/2010.10499#amazon"
2020-10-20
- "https://arxiv.org/abs/2004.03720"
2020-04-07
- "https://arxiv.org/abs/2105.13626#google"
2021-05-28
- "https://arxiv.org/abs/2108.11193"
2021-08-25
- "https://arxiv.org/pdf/2204.06125#page=16&org=openai"
2022-04-13
- "https://arxiv.org/abs/2207.06991"
2022-07-14
- "https://arxiv.org/abs/2307.03381"
2023-07-07
- "https://arxiv.org/abs/1908.04577#alibaba"
2019-08-13
- "https://arxiv.org/abs/2005.12872#facebook"
2020-05-26
- "https://arxiv.org/abs/2008.02217"
2020-07-16
- "https://arxiv.org/abs/2011.13729#tencent"
2020-11-27
- "https://arxiv.org/abs/2012.08508#deepmind"
2020-12-15
- "https://arxiv.org/abs/2101.11986"
2021-01-28
- "https://arxiv.org/abs/2111.12233#microsoft"
2021-11-24
- "https://arxiv.org/abs/1904.10509#openai"
2019-04-23
- "https://arxiv.org/abs/1907.00235"
2019-06-29
- "https://arxiv.org/abs/1911.04070"
2019-11-11
- "https://arxiv.org/abs/2001.04451#google"
2020-01-13
- "https://arxiv.org/abs/2003.05997#google"
2020-03-12
- "https://arxiv.org/abs/2005.08100#google"
2020-05-16
- "https://arxiv.org/abs/2005.14165#openai"
2020-05-28
- "https://arxiv.org/abs/2007.14062#google"
2020-07-28
- "https://arxiv.org/abs/2009.06732#google"
2020-09-14
- "https://arxiv.org/abs/2009.14794#google"
2020-09-30
- "https://arxiv.org/abs/2010.10504#google"
2020-10-20
- "https://arxiv.org/abs/2010.11929#google"
2020-09-28
- "https://arxiv.org/abs/2012.07436"
2020-12-14
- "https://arxiv.org/abs/2012.11346"
2020-12-21
- "https://arxiv.org/abs/2105.12723"
2021-05-26
- "https://arxiv.org/abs/2106.12566"
2021-06-23
- "https://arxiv.org/abs/2107.05768#google"
2021-07-12
- "https://arxiv.org/abs/2204.05927"
2022-07-04
- "https://arxiv.org/pdf/1706.03741#page=15&org=openai"
2017-06-12
- "https://arxiv.org/abs/1909.08053#nvidia"
2019-09-17
- "https://arxiv.org/abs/1908.09203#openai"
2019-11-05
- "https://arxiv.org/abs/2010.14701#openai"
2020-10-28
- "https://arxiv.org/abs/2111.11904#microsoft"
2021-11-23
- "/doc/ai/nn/transformer/gpt/dall-e/1/2020-chen-2.pdf#openai"
2020-06-17
- "https://web.archive.org/web/20191127163535/http://www.aidungeon.io/2019/11/my-orc-band-and-our-quest-for-equal.html"
2019-11-26
- "https://arxiv.org/abs/2005.09980"
2020-09-08
- "https://web.archive.org/web/20210426084422/https://www.stuff.co.nz/technology/103500435/google-deepmind-founder-and-leader-in-artificial-intelligence-returns-to-hamilton"
2018-05-07
- "https://arxiv.org/abs/1910.02054#microsoft"
2019-10-04
- "https://distill.pub/2020/circuits/zoom-in/#openai"
2020-03-10
- "/doc/ai/scaling/2020-bell.pdf#facebook"
2020-08-22
- "https://arxiv.org/abs/1907.10641#allen"
2020-10-16
- "https://arxiv.org/abs/2103.14586#google"
2021-03-26
- "https://web.archive.org/web/20230710000944/https://frc.ri.cmu.edu/~hpm/project.archive/general.articles/1975/Raw.Power.html"
1976-05-12
- "/doc/ai/scaling/hardware/2019-roy.pdf"
2019-11-27
- "/doc/ai/scaling/hardware/2020-jiang.pdf"
2020-11-04
- "https://www.reuters.com/technology/chinas-military-government-acquire-nvidia-chips-despite-us-ban-2024-01-14/"
2024-01-15
- "/doc/darknet-market/2020-zhou-2.pdf"
2020-02-01
- "https://web.archive.org/web/20190130223039/http://www.animenewsservice.com/archives-dec13/"
1999-12-20
- "https://arxiv.org/abs/1104.4322"
2011-03-17
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
- "https://arxiv.org/abs/2308.04445"
2023-07-31
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
- "/doc/ai/nn/cnn/2018-choi.pdf"
2018-02-22
- "/doc/ai/nn/cnn/2020-leipheimer.pdf"
2020-01-22
- "https://distill.pub/2020/growing-ca/#google"
2020-02-11
- "https://arxiv.org/abs/2003.10580#google"
2021-01-05
- "/doc/ai/nn/fully-connected/2020-bao.pdf"
2020-06-03
- "https://arxiv.org/abs/1703.06676"
2017-03-20
- "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7861215/"
2019-07-21
- "/doc/darknet-market/dnm-archive/2020-zhang.pdf"
2020-12-01
- "https://arxiv.org/pdf/1809.11096#page=8&org=deepmind"
2018-09-28
- "https://www.justinpinkney.com/blog/2020/stylegan-network-blending/"
2020-08-25
- "/doc/ai/nn/gan/stylegan/anime/2022-endo.pdf"
2022-07-09
- "https://proceedings.mlr.press/v139/vicol21a.html"
2021-07-01
- "/doc/ai/nn/sparsity/knowledge-distillation/2016-luo.pdf"
2016-03-05
- "https://arxiv.org/abs/1911.04252#google"
2019-11-11
- "https://arxiv.org/abs/2101.03961#google"
2021-01-11
- "https://arxiv.org/abs/1911.02972#facebook"
2019-11-07
- "/doc/ai/nn/transformer/gpt/2/fiction/2021-davis.pdf"
2021-04-27
- "/doc/law/2022-arbel.pdf"
2022-02
- "https://mededu.jmir.org/2023/1/e45312/"
2023
- "https://www.taxnotes.com/featured-analysis/openai-cribbed-our-tax-example-can-gpt-4-really-do-tax/2023/08/11/7h0hc"
2023-08-14
- "/doc/ai/nn/transformer/gpt/lamda/2021-jiang-2.pdf"
2021-09-07
- "https://arxiv.org/abs/2111.11904#microsoft"
2021-11-23
- "https://web.archive.org/web/20191127163535/http://www.aidungeon.io/2019/11/my-orc-band-and-our-quest-for-equal.html"
2019-11-26
- "https://web.archive.org/web/20210426084422/https://www.stuff.co.nz/technology/103500435/google-deepmind-founder-and-leader-in-artificial-intelligence-returns-to-hamilton"
2018-05-07
- "https://arxiv.org/abs/1911.05289#google"
2019-11-13
- "/doc/ai/scaling/2020-bell.pdf#facebook"
2020-08-22
- "/doc/ai/scaling/hardware/2020-jouppi.pdf#google"
2020-06-01
- "https://arxiv.org/abs/1104.4322"
2011-03-17
- "https://www.liebertpub.com/doi/full/10.1089/ast.2017.1783"
2018-09-12
- "https://www.biorxiv.org/content/10.1101/2020.11.21.392720.full"
2020-11-22
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
- "https://web.archive.org/web/20100611071828/http://online.wsj.com/article/SB10001424052748704513104575256452390636786.html"
2010-06-08
- "https://www.bbc.com/future/article/20160406-we-went-to-nasa-to-float-on-the-worlds-flattest-floor"
2016-04-07
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
- "https://arxiv.org/abs/1904.09828"
2019-03-24
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
- "https://www.nature.com/articles/s41586-024-08502-w"
2025-02-05
- "https://link.springer.com/article/10.1007/s10508-019-01624-7 How Large Are Gender Differences in Toy Preferences? A Systematic Review and Meta-Analysis of Toy Preference Research"
2020-01-27
- "https://www.nature.com/articles/s41598-023-39304-1"
2023-07-26
- "/doc/cs/2014-deoliveira.pdf"
2014
- "https://www.nature.com/articles/s44220-025-00390-x"
2025-02-13

Task:

- """ + target + "\n"

    completion = client.chat.completions.create(
        model="gpt-4o-mini",
        messages=[
            {"role": "system", "content": "You are a researcher and web developer, compiling a bibliography."},
            {"role": "user", "content": prompt}
        ]
    )

    date_str = completion.choices[0].message.content.strip()

    # Handle empty string case (both with and without quotes)
    if date_str in ['""', "''", ""]:
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

