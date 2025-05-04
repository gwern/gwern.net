#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# daterange-checker.py: check dates in context for whether they are actually numbers
# Author: Gwern Branwen
# Date: 2024-08-04
# When:  Time-stamp: "2025-05-03 11:21:52 gwern"
# License: CC-0
#
# Usage: $ OPENAI_API_KEY="sk-XXX" echo [...] | python daterange-checker.py
#
#
# Example:
#

import sys
from openai import OpenAI
client = OpenAI()

if len(sys.argv) == 1:
    target = sys.stdin.read().strip()
else:
    target = sys.argv[1]

completion = client.chat.completions.create(
  model="gpt-4.1-mini",
  messages=[
    {"role": "system", "content": "You are a helpful and fastidious research assistant."},
      {"role": "user", "content":
f"""Task: checking dates for whether they are actually a quantity or number being incorrectly formatted as a date.

Summary: Gwern.net dates get special markup for CSS styling to provide context for readers: they are annotated with how many years ago they were.
In 2024, the date '1974' (which was 2024 − 1975 = 49 years before 2024) would be written as '<span class="date-range">1975<sub>49ya</sub></span>', and '1996' as '<span class="date-range">1996<sub>28ya</sub></span>', and so on.

Dates are detected by a regexp like '[12][0-9][0-9][0-9]', on the assumption that *numbers* >999 will be comma-separated, like '1,000 units' vs '1000 AD'.
But there are numbers which are not correctly comma-separated, and are incorrectly marked up as dates.

Classify the input by whether the dates are correctly dates, or numbers incorrectly treated as dates.
For correct inputs, do not print out the input; print out nothing and say nothing.
For incorrect inputs, print out the exact input; do not reformat or change it.
(If unsure, print out the input to be safe.)

Examples:

- Input: <text>Aditomo, Yoshiaki; Iwayama, Hiroshi, “Learning the Distribution of Phrases from Static Analysis”<span>, In Proceedings of the conference on
Empirical Methods in Natural Language Processing , pp.&nbsp;35–42, <span class="date-range">2011<sub>13ya</sub></span>.</span></p></text> →
""
- Input: <text>About the poet: Bryan Thao Worra was born in a refugee camp in Malaysia during the Vietnamese War. In <span class="date-range">1975<sub>49ya</sub></span>, Bryan’s family fled to Vermont and in <span class="date-range">1996<sub>28ya</sub></span> he graduated from Green Mountain College with a BA in Environmental Conservation. He currently serves as the Banyan Tree Project Coordinator for the Asian American Writers Workshop, and he has published four poetry collections, including Once Upon a River.</text> →
""
- Input: <text><p>1100px is close to my original A/B test indicating <span class="date-range">1000<sub>1,024ya</sub></span>px was the leading candidate, so that gives me additional confidence, as does the observation that <span class="date-range">1300<sub>724ya</sub></span>px and <span class="date-range">1200<sub>824ya</sub></span>px are the other leading candidates. (Curiously, the site conversion average before was 13.88%; perhaps my underlying traffic changed slightly around the time of the test? This would demonstrate why alternatives need to be tested simultaneously.) A quick and dirty R test of <span class="date-range">1100<sub>924ya</sub></span>px vs <span class="date-range">1300<sub>724ya</sub></span>px:</p></text> →
"<p>1100px is close to my original A/B test indicating <span class="date-range">1000<sub>1,024ya</sub></span>px was the leading candidate, so that gives me additional confidence, as does the observation that <span class="date-range">1300<sub>724ya</sub></span>px and <span class="date-range">1200<sub>824ya</sub></span>px are the other leading candidates. (Curiously, the site conversion average before was 13.88%; perhaps my underlying traffic changed slightly around the time of the test? This would demonstrate why alternatives need to be tested simultaneously.) A quick and dirty R test of <span class="date-range">1100<sub>924ya</sub></span>px vs <span class="date-range">1300<sub>724ya</sub></span>px:</p>"
- Input: <text>15 healthy smokers and 15 non-smokers were enrolled into this study investigating the effects of smoking on overnight performance. Subjects arrived at the test center at <span class="date-range">1930<sub>94ya</sub></span> hours and were assessed at baseline</text> →
"15 healthy smokers and 15 non-smokers were enrolled into this study investigating the effects of smoking on overnight performance. Subjects arrived at the test center at <span class="date-range">1930<sub>94ya</sub></span> hours and were assessed at baseline"
- Input: <text>19. Ziedonis D, Hitsman B, Beckham JC, et al. “Tobacco use and cessation in psychiatric disorders: National Institute of Mental Health report”. <em>Nic Tob Res</em> <span class="date-range">2008<sub>16ya</sub></span>;10:1691-715</text> →
""
- Input: <text>'About'</text> →
""
- Input: <text>poet:</text> →
""
- Input: <text>Bryan</text> →
""
- Input: <text>"Polar (June <span class="date-range">2009<sub>15ya</sub></span>)"</text> →
""
- Input: <text>"Age-related changes in size (mixed-sex BW and front view photos) of University of Alberta Meat Control strains unselected since <span class="date-range">1957<sub>67ya</sub></span> and <span class="date-range">1978<sub>46ya</sub></span>, and Ross 308 broilers (<span class="date-range">2005<sub>19ya</sub></span>). Within each strain, images are of the same bird at 0, 28, and 56 d of age."</text> →
- Input: <text><p>… But OTAKU NO VIDEO hardly bore an auspicious name as it sat on store racks. The name of Tsutomu Miyazaki – a serial killer arrested in <span class="date-range">1989<sub>35ya</sub></span> whose otaku background was exploited as a media circus (in a manner not unlike the live-action segments of ONV) – was still fresh in the public mind, and made Gainax, to any potential mainstream audience, appear to be merely exposing a distasteful pathological subculture.</p></text> →
""
- Input: <text>"Google Translate doesn’t help; tentatively assigned to <span class="date-range">1995<sub>29ya</sub></span>: if episode 8, might be <span class="date-range">1994<sub>30ya</sub></span>?"</text> →
""
- Input: <text><p>In response to severe budget shortfalls resulting from the economic recession beginning in <span class="date-range">2008<sub>16ya</sub></span>, officials in both cities seriously considered police layoffs as a potential component of their cutback strategies. The Newark Police Department terminated 13% of the police force in late <span class="date-range">2010<sub>14ya</sub></span> while Jersey City officials averted any layoffs from occurring.</p></text> →
""
- Input: <text>A quick and dirty R test of <span class="date-range">1100<sub>924ya</sub></span>px vs <span class="date-range">1300<sub>724ya</sub></span>px:</text> →
"A quick and dirty R test of <span class="date-range">1100<sub>924ya</sub></span>px vs <span class="date-range">1300<sub>724ya</sub></span>px:"
- Input: <text><p>4/10/<span class="date-range">2008<sub>16ya</sub></span> 6:45PM</p></text> →
""
- Input: <text><span class="date-range">2001<sub>23ya</sub></span>, 23:20:43, <a href="https://en.wikipedia.org/wiki/Incompatible_Timesharing_System" class="link-annotated-partial link-live" data-link-icon="wikipedia" data-link-icon-type="svg" data-url-iframe="https://en.m.wikipedia.org/wiki/Incompatible_Timesharing_System#bodyContent" title="Incompatible Timesharing System">ITS</a> Academic Media & Technology”</text> →
""
- Input: <text>Few lives were free from sudden setbacks. Every year, several villages and urban districts went up in smoke. An English traveller, crossing the Jura from Salins to Pontarlier in <span class="date-range">1738<sub>286ya</sub></span>, was told that ‘there is scarce a Village in all this Tract that does not perish by Flames once at least in 10 Years’</text> →
""
- Input: <text>Mark Reckless is noted for voting against EU budgets, for Brexit, and for leading party revolts; he “cast 56 votes against the whip between <span class="date-range">2010<sub>14ya</sub></span> and 2014, making him the 13th-most rebellious Conservative in the period”</text> →
""
- Input: <text>Because of the difficulty in recruiting sperm donors in Norway, clinics imported sperm for 10 years up to <span class="date-range">2005<sub>19ya</sub></span> from a Danish sperm bank</text> →
""
- Input: <text>However, it looks like the mortality only starts decreasing around <span class="date-range">2000<sub>24ya</sub></span> days,</text> →
"However, it looks like the mortality only starts decreasing around <span class="date-range">2000<sub>24ya</sub></span> days,"
- Input: <text><p>so for most such curves, if the peak is at <em>X</em> mg, then a dose at <em>X</em>/100 or <em>X</em>/<span class="date-range">1000<sub>1,024ya</sub></span> mg will do little;</p></text> →
"<p>so for most such curves, if the peak is at <em>X</em> mg, then a dose at <em>X</em>/100 or <em>X</em>/<span class="date-range">1000<sub>1,024ya</sub></span> mg will do little;</p>"
- Input: <text>Rinaldo <span class="date-range">2007<sub>17ya</sub></span>, <a href="https://strathprints.strath.ac.uk/37690/1/ANTENNAE_ISSUE_2.doc.pdf#page=8" data-link-icon="pdf" data-link-icon-type="svg" title="Ken Rinaldo's interdisciplinary media artworks look at the intersection between natural and technological systems.</text> →
""
- Input: <text><li>Osterman, J. E. Hopper, J. Heran, W. J. Keane, T. M. & van der Kolk, B. A. (<span class="date-range">2001<sub>23ya</sub></span>). “Awareness under anesthesia and the development of posttraumatic stress disorder”. <em>General Hospital Psychiatry</em>, 23(4), 198–204</li></text> →
""
- Input: <text>Behind the Scenes of the <span class="date-range">1957<sub>67ya</sub></span> Chapel Hill Conference on the Role of Gravitation in Physics</text> →
""
- Input: <text>…In January <span class="date-range">1992<sub>32ya</sub></span>, the newly unified German government made almost the entire archive</text> →
""
- Input: <text>Not clear when arrested, other than sometime during or before October <span class="date-range">2012<sub>12ya</sub></span>.</text> →
""
- Input: <text>“Our <span class="date-range">2006<sub>18ya</sub></span> research [Christian Stegbauer, ‘Wikipedia. Das Rätsel der Kooperation’ (‘Wikipedia: the mystery behind the cooperation’<span>), Wiesbaden: VS, <span class="date-range">2009<sub>15ya</sub></span>, p.&nbsp;279 et seq.] compared content on user pages from their original starting date to the present.</text> →
""
- Input: <text>Setting up the intermediate configurations between StyleGAN2 and our generators, as well as, the key parameter ablations was also quite expensive at ~15%. Training a single instance of Alias-Free-R at <span class="date-range">1024<sub>1,000ya</sub></span>×<span class="date-range">1024<sub>1,000ya</sub></span> is only slightly more expensive (0.9MWh) than training StyleGAN2 (0.7MWh)</text>
"Setting up the intermediate configurations between StyleGAN2 and our generators, as well as, the key parameter ablations was also quite expensive at ~15%. Training a single instance of Alias-Free-R at <span class="date-range">1024<sub>1,000ya</sub></span>×<span class="date-range">1024<sub>1,000ya</sub></span> is only slightly more expensive (0.9MWh) than training StyleGAN2 (0.7MWh)"
- Input: <text> In <span class="date-range">1927<sub>97ya</sub></span>, the Commonwealth Fund piloted a rural hospital program; there aren’t a lot of</span> “philanthropic opportunities” that look like that today.</text> →
""
- Input: <text>Average output losses due to short-run sectoral shocks are an order of magnitude larger than the welfare cost of business cycles calculated by <a href="https://en.wikipedia.org/wiki/Robert_Lucas_Jr." class="link-live" data-url-iframe="https://en.m.wikipedia.org/wiki/Robert_Lucas_Jr.#bodyContent" data-link-icon="wikipedia" data-link-icon-type="svg">Lucas (<span class="date-range">1987<sub>37ya</sub></span>)</a>. Nonlinearities can also cause shocks to critical sectors to have disproportionate macroeconomic effects, almost tripling the estimated impact of the 1970s oil shocks on world aggregate output</text> →
""
- Input: <text><p>Large and general upward trends in research collaboration are also found in journal publications (eg. Adams et al, <span class="date-range">2004<sub>20ya</sub></span>).</p></text> →
""
- Input: <text>…The foregoing examples highlight the ethical dimension of the hard problem of unconsciousness. The demonstrated natural possibility of i-zombies has implications for our treatment of individuals presumed unconscious. How should clinicians behave in the operating room given the demonstrated incidence of 1–2 individuals/<span class="date-range">1000<sub>1,024ya</sub></span> that may still experience qualia during a surgery?</text> →
"…The foregoing examples highlight the ethical dimension of the hard problem of unconsciousness. The demonstrated natural possibility of i-zombies has implications for our treatment of individuals presumed unconscious. How should clinicians behave in the operating room given the demonstrated incidence of 1–2 individuals/<span class="date-range">1000<sub>1,024ya</sub></span> that may still experience qualia during a surgery?"
- Input: <text>The Godin anecdote was in 2023, where he mentions he was 62yo; he also mentions that he was 24yo when it happened. This implies a date of 24 + (2023 − 62) = <span class="date-range">1985<sub>39ya</sub></span>, which is consistent with their working on the first draft</text> →
""
- Input: <text>Fred now revealed that a year earlier [also in <span class="date-range">1973<sub>51ya</sub></span>] he had forged documents indicating approval of a loan guarantee by the Enterprise Company without consent of the other board members, specifically his two sisters and Bobby Cox, the Enterprise secretary. Our respected leader admitted his culpability to the Federal Express board of directors and to the investors and lenders we were counting on to support the second round of the private placement financing</text> →
""
- Input: <text>>So I was quite excited to reach Göttingen in <span class="date-range">1927<sub>97ya</sub></span>. I was quickly and deeply disappointed.</text> →
""
- Input: <text>I found Hilbert painfully withdrawn. He had contracted pernicious anemia in <span class="date-range">1925<sub>99ya</sub></span> and was no longer an active thinker. The worst symptoms of pernicious anemia are not immediately obvious, and Hilbert’s case had not yet been diagnosed.</text> →
""
- Input: <text>Hazama, N: Cat and Matatabi. <em>Iden</em> (<em>Heredity</em>), 7: 33-37, <span class="date-range">1953<sub>71ya</sub></span> (Japanese)</text> →
""
- Input: <text>us discussions, I couldn’t find any clear verdict on what patch brands might be safer (in terms of nicotine evaporation through a cut or edge) than others, so I went with the cheapest Habitrol I could find as a first try of patches (<a href="https://www.amazon.com/gp/product/B000GU6ENQ/" data-link-icon="amazon" data-link-icon-type="svg" data-url-iframe="https://www.amazon.com/gp/product/B000GU6ENQ/?tag=gwernnet-20" title="$25.95">“Nicotine Transdermal System Patch, Stop Smoking Aid, 21 mg, Step 1, 14 patches”</a>) in May <span class="date-range">2013<sub>11ya</sub></span>. I am curious to what extent nicotine might improve a long time period</text> →
""
- Input: <text>"<span class=\"date-range\">2005<sub>19ya</sub></span> is, ±2 years, when they became part of the culture—they became de rigeur, text messaging services began popping up, they became incorporated with ‘<a href=\"https://en.wikipedia.org/wiki/Web_2.0\" class=\"link-annotated-partial link-live\" data-link-icon=\"wikipedia\" data-link-icon-type=\"svg\" data-url-iframe=\"https://en.m.wikipedia.org/wiki/Web_2.0#bodyContent\" title=\"Web 2.0\">Web 2.0</a>’, teenage girls would spend all their time messaging each other, etc.<a href=\"#fnref1\" class=\"footnote-back\" role=\"doc-backlink\">↩︎</a>"</text> →
""
- Input: <text>"<p><a href=\"https://en.wikipedia.org/wiki/W._H._Auden\" id=\"w-h-auden\" class=\"link-annotated-partial id-not link-live\" data-link-icon=\"wikipedia\" data-link-icon-type=\"svg\" data-url-iframe=\"https://en.m.wikipedia.org/wiki/W._H._Auden#bodyContent\" title=\"W. H. Auden\">W. H. Auden</a>, via <a href=\"https://en.wikipedia.org/wiki/Anthony_Hecht\" class=\"link-live\" data-link-icon=\"wikipedia\" data-link-icon-type=\"svg\" data-url-iframe=\"https://en.m.wikipedia.org/wiki/Anthony_Hecht#bodyContent\" title=\"Anthony Hecht\">Anthony Hecht</a> (<a href=\"https://www.theparisreview.org/interviews/2487/the-art-of-poetry-no-40-anthony-hecht\" class=\"link-live\" data-link-icon=\"PR\" data-link-icon-type=\"text\" title=\"The Art of Poetry No. 40\">The Art of Poetry No. 40</a>, <span class=\"date-range\">1988<sub>36ya</sub></span>)</p>"</text> →
""
- Input: <text>The problem of Jacobian Conjecture is very hard. Perhaps it will take human being another 100 years to solve it. Your attempt is noble, Maybe the Gods of Olympus will smile on you one day. Do not be too disappointed. B. Sagre has the honor of publishing three wrong proofs and C. Chevalley mistakes a wrong proof for a correct one in the <span class="date-range">1950<sub>74ya</sub></span>’s in his Math Review comments, and I.R. Shafarevich uses Jacobian Conjecture (to him it is a theorem) as a fact…</text> →
""
- Input: <text>In addition to writing, the visualization of class education became another form of preserving and reinforcing the collective memory of victimization. The theme was soon boiled down to 2 key words: bitterness (苦 <em>ku</em>) and hatred (仇 <em>chou</em>). The documentary “Never Forget Class Bitterness, Forever Remember the Hatred in the Sea of Blood (不忘阶级苦，永记血海仇 <em>Buwang jieji ku, yongji xiehai chou</em>)”, made in <span class="date-range">1965<sub>59ya</sub></span>, was based on an exhibition promoting class education in <a href="https://en.wikipedia.org/wiki/Shandong" class="id-not link-live" data-url-iframe="https://en.m.wikipedia.org/wiki/Shandong#bodyContent">Shandong Province</a>.</text> →
""
- Input: <text><text><p><a href="/doc/fiction/science-fiction/frank-herbert/1980-omni-july.pdf" class="link-annotated-partial">“Omni Magazine (July <span class="date-range">1980<sub>44ya</sub></span>)”</a>,<span class="date cite-date" title="1980-01-01">1980</span> (<span class="link-tags"><a href="/doc/fiction/science-fiction/frank-herbert/index" class="link-tag link-page link-annotated icon-not" rel="tag" title="Link to fiction/science-fiction/frank-herbert tag index"><em>Dune</em></a></span>; <span class="backlinks"><a href="/metadata/annotation/backlink/%252Fdoc%252Ffiction%252Fscience-fiction%252Ffrank-herbert%252F1980-omni-july.pdf.html" class="aux-links link-page backlinks icon-not" title="Reverse citations for this page.">backlinks</a></span>)</p></text></text> →
""
- Input: <text>The <span class="date-range">2001<sub>23ya</sub></span> <a href="https://en.wikipedia.org/wiki/Shijiazhuang_bombings" class="id-not link-live" data-link-icon="wikipedia" data-link-icon-type="svg" data-url-iframe="https://en.m.wikipedia.org/wiki/Shijiazhuang_bombings#bodyContent" title="Shijiazhuang bombings">Shijiazhuang bombings</a> <span>killed 108 people and was officially blamed on an unemployed deaf man.</text> →
""
- Input: <text>Extracted from search and social media metadata between <span class="date-range">1998<sub>26ya</sub></span> and 2017, these high-quality summaries demonstrate high diversity of summarization styles. In particular, the summaries combine abstractive and extractive strategies, borrowing words and phrases from articles at varying rates.</text> →
""
- Input: <text><p><a href="https://voltpon3.bandcamp.com/track/love-is-in-bloom-chiptune" class="link-live" data-link-icon="audio" data-link-icon-type="svg" title="Love Is In Bloom Chiptune">“Love Is In Bloom Chiptune”</a> (VoltPon3; <em>Chiptune Pony</em> <span class="date-range">2013<sub>11ya</sub></span>) [chiptune]</p></text> →
""
- Input: <text><span class="date-range" title="The date range 2006–2007 lasted 1 year, ending 17 years ago.">2006–2007<sub>17ya</sub></span></text> →
""
- Input: <text>...These calculations imply that, because of falling BLLs, U.S. preschool-aged children in the late 1990s had IQs that were, on average, 2.2–4.7 points higher than they would have been if they had the blood lead distribution observed among U.S. preschool-aged children in the late 1970s. We estimated that each IQ point raises worker productivity 1.76–2.38%. With discounted lifetime earnings of <span class="inflation-adjusted" data-year-original="2000" data-amount-original="723,300" data-year-current="2024" data-amount-current="1,313,286.93" title="CPI inflation-adjusted US dollar: from nominal $723,300 in 2000 → real $1,313,286.93 in 2024">$1,313,286.93<span class="subsup"><sup>$723,300</sup><sub>2000</sub></span></span> for each 2-year-old in <span class="date-range">2000<sub>24ya</sub></span> dollars, the estimated economic benefit for each year’s cohort of 3.8 million 2-year-old children ranges from...</text> →
""
- Input: <text>(As of February <span class="date-range">2013<sub>11ya</sub></span>, the bedside unit seems to’ve been discontinued; the most comparable Zeo Inc. product seems to be the...)</text> →
""
- Input: <text>On Christmas <span class="date-range">2010<sub>14ya</sub></span>, I received one of Zeo's bedside units.</text> →
""
- Input: <text>"LBC<span class="date-range">1936<sub>88ya</sub></span>: Lothian birth cohort 1936"</text> →
""
- Input: <text>"<a href="/doc/history/public-domain-review/index#section-16" id="toc-section-16" class="link-page">“Glossary of Censored Words from a <span class="date-range">1919<sub>105ya</sub></span> Treatise on Love”</a>"</text> →
""
- Input: <text>"<span class=\"date-range\">2007<sub>17ya</sub></span> paper in the journal Neuropharmacology"</text> →
""
- Input: <text>"<span class="date-range">1975<sub>49ya</sub></span>, for example, sheets for 100 stocks, with 3 volatility estimates for each stock, cost..."</text> →
""
- Input: <text>We aimed to investigate the heritability of dog ownership in a large twin sample including all twins in the Swedish Twin Registry born 1926<span class="subsup"><sup>–</sup><sub>70</sub></span>1996 and alive in 2006.</text> →
""
- Input: <text>In <span class="date-range">1998<sub>26ya</sub></span>, Robert Plomin and his Colorado Adoption Project (CAP) colleagues published the results of a longitudinal adoption study of personality. They found an average personality test score correlation of only 0.01 between birth-parents and their 240 adopted-away 16-year-old biological offspring, suggesting no genetic influences on personality. However, the researchers interpreted their results in the context of previous twin studies, produced an average 14% heritability estimate, and concluded that nonadditive genetic factors underlie personality traits.</text> →
""
- Input: <text><p>Almost 40,000 selected seventh-grade students from the Middle Atlantic region of the United States took the College Board Scholastic Aptitude Test as part of the Johns Hopkins regional talent search in <span class="date-range">1980<sub>44ya</sub></span>, <span class="date-range">1981<sub>43ya</sub></span>, and <span class="date-range">1982<sub>42ya</sub></span>. </text> →
""
- Input: <text>Percival constructs a password algorithm on his new hash function and then calculates costs using <span class="date-range">2002<sub>22ya</sub></span> circuit prices.</text> →
""
- Input: <text>The total sales volume on Silk Road was 192.7 million US dollars between June <span class="date-range">2012<sub>12ya</sub></span> and October <span class="date-range">2013<sub>11ya</sub></span>. The corresponding figures for Silk Road 2.0, Agora, Evolution, Nucleus, and Abraxas were 112.9, 220.7, 69.7, 88.3, and 35.6 million US dollars, respectively. The figures for AlphaBay was 166.0 million US dollars between December 2014 and February 2016.</text> →
""
- Input: <text><span class="date-range">2011<sub>13ya</sub></span> version of Shinji: he has more of a---</text> →
""
- Input: <text>That is not to say that your work as a travel writer has not been widely acclaimed. I’m sure the photographs of your excursions through Thebes and your thrilling accounts of The Court of the Crimson King are fascinating, but it is difficult for us to believe that your work has been entirely your own. Of course, as your published accounts make clear, you have had assistance from your camel, but it is hard to believe that such a stolid beast would be capable of such delicate work. Likewise, your dog’s account of “A Dog’s Travel Log” (c. <span class="date-range">1916<sub>108ya</sub></span>) may be delightful, but I’m not convinced that it is entirely truthful.</text> →
""
- Input: <text>"<p>For blacks, food’s share fell from 21.17% to 12.44%. ~0.8 point of the decline can be explained by measured income growth, and another point by movement in other regressors, and up to another 1 point by the decline in the food CPI. Thus the food-share decline left to be explained by <a href=\"https://en.wikipedia.org/wiki/Observational_error\" class=\"link-live\" data-link-icon=\"wikipedia\" data-link-icon-type=\"svg\">measurement error</a> is 5.9 points. I estimate the bias to be ~4% per year from <span class=\"date-range\">1974<sub>50ya</sub></span> through <span class=\"date-range\">1981<sub>43ya</sub></span> and about 3% per year since then.</p>"</text> →
""
- Input: <text>"<p>It’s the words of Anno’s translator at <span class="date-range">1996<sub>28ya</sub></span>’s Anime Expo.</p>"</text> →
""
- Input: <text>"…the bets, aside from the Buffett bet, prompted any substantial media interest or changed discussions. (Aside from the lack of comments or general activity on the site itself, we can also look at <a href="https://pageviews.wmcloud.org/?project=en.wikipedia.org&amp;platform=all-access&amp;agent=user&amp;start=2017-02-01&amp;end=2017-03-31&amp;pages=Long_Bets" data-link-icon="wikipedia" data-link-icon-type="svg" title="Pageviews Analysis">Wikipedia search queries</a>—almost immeasurably small and several of which were probably me—and at <a href="https://trends.google.com/trends/explore?q=Long%20Bets" data-link-icon="alphabet" data-link-icon-type="svg" title="Google Trends for ‘Long Bets’">Google Trends</a>, worldwide search trends never exceed 100 searches a day and average ~25 all the way back to <span class="date-range">2004<sub>20ya</sub></span>; on Google News, I could not find any mentions of “Long Bets” which weren’t normal expressions; and <a href="https://hn.algolia.com/?query=%22Long%20Bets%22&amp;sort=byDate&amp;prefix&amp;page=0&amp;dateRange=all&amp;type=all">even on Hacker News</a>, where the original coder of the LB site apparently hangs out, there are few mentions of it which aren’t provoked by ongoing interest in the Buffett bet.)"</text> →
""
- Input: <text>"In <span class="date-range">2000<sub>24ya</sub></span>, the cost to test each transistor was 10% of the cost to manufacture it. However, as transistors become cheaper and testing becomes more difficult, it is projected that by 2015 it will cost more to test a transistor than to make it."</text> →
""
- Input: <text>"His first attempt at replicating the <span class="date-range">1990<sub>34ya</sub></span> study, in <span class="date-range">1995<sub>29ya</sub></span>, resulted in an effect that was 30% smaller."</text> →
""
- Input: <text><p>Rapper Ricky Brown apparently set a rapping speed record in <span class="date-range">2005<sub>19ya</sub></span> with <a href="/doc/www/sparkplugged.net/1034cf05bc6dda4a91c14f8607142bd5e7258c56.html" class="link-live" data-url-archive="/doc/www/sparkplugged.net/1034cf05bc6dda4a91c14f8607142bd5e7258c56.html" data-url-original="http://sparkplugged.net/2007/10/outsider-speed-rap-extraordinaire/">“723 syllables in 51.27 seconds”</a></p></text> →
""
- Input: <text>"<a href=\"/doc/history/public-domain-review/index#section-9\" id=\"toc-section-9\" class=\"link-page\">“Chladni Figures (<span class=\"date-range\">1787<sub>237ya</sub></span>)”</a>"</text> →
""
- Input: <text>"She originally started working for <a href="/smpy" id="gwern-smpy" class="link-annotated link-page" title="‘SMPY Bibliography’, Branwen 2018">SMPY</a> in the 1970s along with Cohn/Pyryt/Benbow and for Lynn Fox & Julian Stanley, leaving in <span class="date-range">1991<sub>33ya</sub></span> for CTY. She specialized in “twice-exceptional students” (both gifted & disabled)."</text> →
""
- Input: <text>"At a seminar in the Bell Communications Research Colloquia Series, Dr.&nbsp;<a href="https://en.wikipedia.org/wiki/Richard_Hamming" class="id-not link-live link-annotated-partial" data-link-icon="wikipedia" data-link-icon-type="svg" data-url-iframe="https://en.m.wikipedia.org/wiki/Richard_Hamming#bodyContent" title="Richard Hamming">Richard W. Hamming</a> [<span class="date-range" title="The date range 1915–1998 lasted 83 years, ending 26 years ago.">1915<span class="subsup"><sup>–</sup><sub>83</sub></span>1998<sub>26ya</sub></span>; <a href="https://mathshistory.st-andrews.ac.uk/Biographies/Hamming/" class="link-live" data-link-icon="M  T" data-link-icon-type="text,quad,sans" title="Richard Hamming (1915 - 1998) - Biography">MacTutor</a>], a Professor at the <a href="https://en.wikipedia.org/wiki/Naval_Postgraduate_School" class="id-not link-live link-annotated-partial" data-link-icon="wikipedia" data-link-icon-type="svg" data-url-iframe="https://en.m.wikipedia.org/wiki/Naval_Postgraduate_School#bodyContent" title="Naval Postgraduate School">Naval Postgraduate School</a> in <a href="https://en.wikipedia.org/wiki/Monterey,_California" class="id-not link-live link-annotated-partial" data-link-icon="wikipedia" data-link-icon-type="svg" data-url-iframe="https://en.m.wikipedia.org/wiki/Monterey,_California#bodyContent" title="Monterey, California">Monterey, California</a> and a retired <a href="https://en.wikipedia.org/wiki/Bell_Labs" class="link-live link-annotated-partial" data-link-icon="wikipedia" data-link-icon-type="svg" data-url-iframe="https://en.m.wikipedia.org/wiki/Bell_Labs#bodyContent" title="Bell Labs">Bell Labs</a> scientist, gave a very interesting and stimulating talk, <a href="/doc/science/1986-hamming#the-talk-you-and-your-research" id="gwern-doc-science-1986-hamming--the-talk-you-and-your-research" class="link-page">“You and Your Research”</a> <span>to an overflow audience of some 200 Bellcore staff members and visitors at the Morris Research and Engineering Center on March 7, <span class="date-range">1986<sub>38ya</sub></span>.</span>"</text> →
""
- Input: <text>"Customs interceptions of their packages beginning in <span class="date-range">2012<sub>12ya</sub></span> were followed by a local undercover buy, which triggered the raid."</text> →
""
- Input: <text>"<p>[<a href="/doc/www/link.springer.com/c6e845ce8fdf8c6f7e6d1b39917f5ea7cb4ba5fb.html" id="tauber-et-al-2013" class="link-annotated" data-url-archive="/doc/www/link.springer.com/c6e845ce8fdf8c6f7e6d1b39917f5ea7cb4ba5fb.html" data-url-original="https://link.springer.com/article/10.3758/s13428-012-0307-9" data-link-icon="springerlink" data-link-icon-type="svg" title="‘General knowledge norms: Updated and expanded from the Nelson &amp; Narens 1980 norms’, Tauber et al 2013">updated <span class="date-range">2012<sub>12ya</sub></span> norms</a>] Normative data were collected on <a href="/doc/psychology/cognitive-bias/illusion-of-depth/1980-nelson.pdf#page=5" data-link-icon="pdf" data-link-icon-type="svg">300 general-information questions</a> from a wide variety of topics, including history, sports, art, geography, literature, and entertainment.</p>"</text> →
""
- Input: <text><p>10: Paul Vinogradoff, <a href="https://www.amazon.com/Historical-jurisprudence-Introduction-Paul-Vinogradoff/dp/B00087G9SC/" data-link-icon="amazon" data-link-icon-type="svg" data-url-iframe="https://www.amazon.com/Historical-jurisprudence-Introduction-Paul-Vinogradoff/dp/B00087G9SC/?tag=gwernnet-20"><em>Historical Jurisprudence</em></a> (London: Oxford University Press, <span class="date-range">1923<sub>101ya</sub></span>), p.&nbsp;327.</p></text> →
""
- Input: <text><p><a href="http://www.walterzorn.de/en/tooltip/tooltip_e.htm" id="zorn-2002" class="link-modified-recently" title="‘DHTML JavaScript Tooltips’, Zorn 2002">“DHTML JavaScript Tooltips (<span class="date-range" title="The date range 2002–2008 lasted 6 years, ending 16 years ago.">2002<span class="subsup"><sup>–</sup><sub>6</sub></span>2008<sub>16ya</sub></span>)”</a>, Walter Zorn (d. <span class="date-range">2009<sub>15ya</sub></span>)</p></text> →
""
- Input: <text><p><a href="https://en.wikipedia.org/wiki/Will_H._Bradley" class="link-live" data-link-icon="wikipedia" data-link-icon-type="svg" data-url-iframe="https://en.m.wikipedia.org/wiki/Will_H._Bradley#bodyContent" title="Will H. Bradley">Will H. Bradley</a> Art Nouveau cover of the noted printing magazine & showcase, <em>The Inland Printer</em> (v14, #3, <span class="date-range">1894<sub>130ya</sub></span></text> →
""
- Input: <text>"<p>One might wonder why I had so much traffic to an English page; do just that many Germans know English? No, it turns out my link in their page didn’t come with an “English” warning. I added this warning on 2012-05-20, and while there was a major traffic spike after that and then a long outage June-September <span class="date-range">2012<sub>12ya</sub></span> where the link was broken due to my own carelessness, the warning seems to have substantially reduced click-throughs according to my <a href="/doc/wikipedia/2012-gwern-dnbfaqwikipedia.pdf" data-link-icon="pdf" data-link-icon-type="svg">analytics</a>.<a href="#fnref28" class="footnote-back" role="doc-backlink">↩︎</a></p>"</text> →
""
- Input: <text>"<span class="date-range">2000<sub>24ya</sub></span> to September <span class="date-range">2012<sub>12ya</sub></span>"</text> →
""
- Input: <text><strong>A. Data</strong>: The source of data for this study is an internal database that the Italian DAP maintains on offenders under its care. We were granted access to the DAP database records on all the individuals released as a result of the collective pardon law between August 1, <span class="date-range">2006<sub>18ya</sub></span>, and February 28, <span class="date-range">2007<sub>17ya</sub></span>. The full sample includes 25,813 individuals; 81% were released on August 1, <span class="date-range">2006<sub>18ya</sub></span>. For each individual, the data provide information on whether or not he or she reoffends within the period between release from prison and February 28, <span class="date-range">2007<sub>17ya</sub></span>. This means that for most of the individuals the data report recidivism in the first 7 months after release from prison. Moreover, the data set contains information concerning a large set of variables at the individual and facility levels.</text> →
""
- Input: <text>With due allowance for style and age, Hadamard ably describes and defends the basic model of ‘work, incubation, illumination, verification’, with reference to his own discoveries, his many famous acquaintances, Poincaré’s lecture, and a very interesting survey of mathematicians. In fact, it’s a little depressing that we don’t seem to have gone much beyond that in the half-century since this was published back in <span class="date-range">1945<sub>79ya</sub></span> or so.</text> →
""
- Input: <text><p>We extracted 4,387 unique URLs referenced in 453 articles published July 1995–August <span class="date-range">2004<sub>20ya</sub></span>. The availability was checked 3 times a week for 25 weeks September 2004–February <span class="date-range">2005<sub>19ya</sub></span>.</p></text> →
""
- Input: <text>"<span class="date-range" title="The date range 1961–1986 lasted 25 years, ending 38 years ago.">1961<span class="subsup"><sup>–</sup><sub>25</sub></span>1986<sub>38ya</sub></span>"</text> →
""
- Input: <text><p>Many foods keep a long time and you can easily make use of a larger quantity. It’s somewhat unusual for something to be <em>too</em> big to buy and a bad idea due to spoilage/opportunity cost (usually something either perishable, like fruit, or ridiculously long-lasting and more expensive in opportunity cost than up front; eg. a few months ago, I finished off a bottle of molasses which dated, as best as I could infer from the copyrights on the label, from ~<span class="date-range">1995<sub>29ya</sub></span>, and it would not be a good idea to buy a big bottle of molasses if you only use it once in a while like I do, for baking rye bread).</p></text> →
""
- Input: <text>" We then apply our sampling method to the live YouTube system, and estimate that there are a total of roughly 500 millions YouTube videos by May <span class="date-range">2011<sub>13ya</sub></span>."</text> →
""
- Input: <text> The SF classic <em><a href="https://en.wikipedia.org/wiki/Stand_on_Zanzibar" class="link-annotated-partial link-live" data-link-icon="wikipedia" data-link-icon-type="svg" data-url-iframe="https://en.m.wikipedia.org/wiki/Stand_on_Zanzibar#bodyContent" title="Stand on Zanzibar">Stand on Zanzibar</a></em> drew its name from the observation that the 7 billion people alive in <span class="date-range">2010<sub>14ya</sub></span> would fit in Zanzibar only if they stood shoulder to shoulder—spread them out, and multiply that area by ~18…) This raises an issue that affects all 3: how much can the Death Note control?</text> →
""
- Input: <text>Some vague speculation about specifics of software/AI improvements to other sectors of the economy, badly handicapped by being written in <span class="date-range">2013<sub>11ya</sub></span> (hopefully Cowen could do a much better job now).</text> →
""
- Input: <text>Cleckley’s moralizing and occasional very old-fashioned comments are occasionally as interesting, and reading him in <span class="date-range">2012<sub>12ya</sub></span>, one feels very strongly just how distant (in a social mores sense) we are from him in the 1940s and earlier</text> →
""
- Input: <text>The second purpose is to offer a more refined estimate of CPI bias. Third, I will present evidence of strikingly different inflation rates by race. Using the PSID, I estimate a demand function for food at home for <span class="date-range">1974<sub>50ya</sub></span> through <span class="date-range">1991<sub>33ya</sub></span>. Using a standard measure of real income (total family income after federal taxes...</text> →
""
- Input: <text><p>…Solomon Veniaminovich Shereshevsky was born in <span class="date-range">1896<sub>128ya</sub></span> in Torzhok, a small town 145 miles north of Moscow, to a Jewish family.</text> →
""
- Input: <text><p>(~110k words; 2.5 hours) <span class="date-range">2008<sub>16ya</sub></span> anthropology/linguistic memoir by <a href="https://en.wikipedia.org/wiki/Daniel_Everett">Everett</p></text> →
""
- Input: <text>On the other hand, in June <span class="date-range">2012<sub>12ya</sub></span>, newspapers were reporting that Sinclair had separated from him, which is consistent with the interpretation that she felt it was her duty to not stab her husband in the back immediately but wait for the scandal to die down</text> →
""
- Input: <text>Amyloid deposits, they thought, are a response to the true cause of Alzheimer’s and therefore a marker of the disease—again, the gravestones of neurons and synapses, not the killers. The evidence? For one thing, although the brains of elderly Alzheimer’s patients had amyloid plaques, so did the brains of people the same age who died with no signs of dementia, a pathologist discovered in <span class="date-range">1991<sub>33ya</sub></span>. Why didn’t amyloid rob them of their memories?</text> →
""
- Input: <text>"<a href=\"/doc/anime/eva/index#section-149\" id=\"toc-section-149\" class=\"link-page\">“Okamoto vs Anno Interview [January <span class=\"date-range\">1997<sub>27ya</sub></span> <em>Animage</em> Interview/discussion between Anno and Film-Maker Kihachi Okamoto]”</a>"</text> →
""
- Input: <text>We take a step towards a general approach and study the general applicability of behavioral cloning on 12 video games, including 6 modern video games (published after <span class="date-range">2010<sub>14ya</sub></span>), by using human demonstrations as training data.</text> →
""
- Input: <text><strong>O</strong>: <span class="date-range">1993<sub>31ya</sub></span>…<span class="date-range">1992<sub>32ya</sub></span>, I think. And then later, back in Osaka, I gave my friend Takeshi Sawamura a call.</text> →
""
- Input: <text>Perhaps going in reverse from Feynman to the anecdote would help: the Caltech Feynman archives, which hold a comprehensive collection of Feynman papers because he made a practice of retaining & donating his papers there (including after the <span class="date-range">1974<sub>50ya</sub></span> speech), might hold the answer?</text> →
""
- Input: <text>In the interview on Thursday, Mr. Musk alternated between laughter and tears. He said he had been working up to 120 hours a week recently—echoing the reason he cited in a recent public apology to an analyst whom he had berated. In the interview, Mr. Musk said he had not taken more than a week off since <span class="date-range">2001<sub>23ya</sub></span>, when he was bedridden with malaria. “There were times when I didn’t leave the factory for 3–4 days—days when I didn’t go outside”, he said.</text> →
""
- Input: <text>One belief—that Redshift helped avoid bright light retarding the sleep cycle and enabling going to bed early—was borne: on Redshift days, I went to bed an average of 19 minutes earlier. (I had noticed this in my earliest Redshift experiment.)</text> →
""
- Input: <text><span class=\"date-range\" title=\"The date range 1967–1986 lasted 19 years, ending 38 years ago.\">1967<span class=\"subsup\"><sup>–</sup><sub>19</sub></span>1986<sub>38ya</sub></span></text> →
""
- Input: <text>Google Knol will be shutdown in <span class="date-range">2013<sub>11ya</sub></span>: <a href="https://predictionbook.com/predictions/3838" data-link-icon="?" data-link-icon-type="text,sans,bold" title="Google Knol will be shutdown in 2013">25%</a></text> →
""
- Input: <text>In the case of Reader, while Reader destroyed the original RSS reader market, there still exist some usable alternatives; the consequence is a shrinkage in the RSS audience as inevitably many users choose not to invest in a new reader or give up or interpret it as a deathblow to RSS, and an irreversible loss of Reader’s uniquely comprehensive RSS archives back to <span class="date-range">2005<sub>19ya</sub></span>. Although to be fair, I should mention 2 major points in favor of Google:</text> →
""
- Input: <text>Based on data from <span class="date-range">1987<sub>37ya</sub></span> through to <span class="date-range">2000<sub>24ya</sub></span>, Ruhm found that smoking and excess weight declined during economic downturns, whereas leisure-time physical activity increased.</text> →
""
- Input: <text>Some early experimental studies with LSD suggested that doses of LSD too small to cause any noticeable effects may improve mood and creativity. Prompted by recent discussion of this claim and the purely anecdotal subsequent evidence for it, I decided to run a <a href="https://en.wikipedia.org/wiki/Power_of_a_test" class="id-not link-live link-annotated-partial" data-link-icon="wikipedia" data-link-icon-type="svg" data-url-iframe="https://en.m.wikipedia.org/wiki/Power_of_a_test#bodyContent" title="Statistical power">well-powered</a> randomized blind trial of 3-day LSD microdoses from September <span class="date-range">2012<sub>12ya</sub></span> to March <span class="date-range">2013<sub>11ya</sub></span>.</text> →
""
- Input: <text>Q: Who was president of the United States in <span class="date-range">1955<sub>69ya</sub></span>?</text> →
""
- Input: <text><span class="date-range">1999<sub>25ya</sub></span></text> →
""
- Input: <text>A 2015 tutorial on how to do manual searches of the <span class="date-range">2013<sub>11ya</sub></span> Google Reader archives</text> →
""
- Input: <text><a href=\"/doc/statistics/prediction/index#lohr-brick-2017-section\" id=\"toc-lohr-brick-2017-section\" class=\"link-page\">“Roosevelt Predicted to Win: Revisiting the <span class=\"date-range\">1936<sub>88ya</sub></span> <em>Literary Digest</em> <span>Poll”, <span class=\"cite\"><span class=\"cite-author\">Lohr &amp; Brick</span><span class=\"cite-date\">2017</span></span></span></a></text> →
""
- Input: <text>Continuation of the <span class="date-range">2009<sub>15ya</sub></span> <a href="https://en.wikipedia.org/wiki/Haskell" class="link-annotated-partial link-live" data-link-icon="wikipedia" data-link-icon-type="svg" data-url-iframe="https://en.m.wikipedia.org/wiki/Haskell#bodyContent" title="Haskell">Haskell</a> Wikipedia link archiving bot tutorial, extending it from operating on a pre-specified list of articles to instead archiving links <em>live</em></text> →
""
- Input: <text>Ph.D. | Computational Neuroscience (Brandeis University, <span class="date-range">2005<sub>19ya</sub></span></text> →
""
- Input: <text> The collection provides a comprehensive snapshot of how photos and videos were taken, described, and shared over the years, from the inception of Flickr in <span class="date-range">2004<sub>20ya</sub></span> until early 2014.</text> →
""
- Input: <text> Every town and village was a living encyclopedia of crafts and trades. In <span class="date-range">1886<sub>138ya</sub></span>, most of the eight hundred and twenty-four inhabitants of the little town of Saint-Étienne-d’Orthe, on a low hill near the river Adour, were farmers and their dependents.</text> →
""
- Input: <text>“The following is a set of articles from Nikkei Entertainment, the August <span class=\"date-range\">1997<sub>27ya</sub></span> issue. I am in the process of translating the whole set.</text> →
""
- Input: <text>Walter Zorn (d. <span class="date-range">2009<sub>15ya</sub></span>)</text> →
""
- Input: <text>On June 4 <span class="date-range">2011<sub>13ya</sub></span></text> →
""
- Input: <text>Financial Times: “In <span class="date-range">2013<sub>11ya</sub></span>, according to the UK’s Human Fertilisation and Embryology Authority</text> →
""
- Input: <text>Albrecht Joseph, eventually Anna’s fifth husband, who was shocked by Alma’s dowdiness when he first met the legendary seductress in <span class="date-range">1931<sub>93ya</sub></span>, nevertheless noted that her “unique gift”</text> →
""
- Input: <text>Beginning with <a href="https://en.wikipedia.org/wiki/Advice_taker" class="link-live" data-url-iframe="https://en.m.wikipedia.org/wiki/Advice_taker#bodyContent" data-link-icon="wikipedia" data-link-icon-type="svg">McCarthy’s Advice Taker (<span class="date-range">1959<sub>65ya</sub></span>)</a></text> →
""
- Input: <text><span class="date-range">1919<sub>105ya</sub></span> Treatise on Love”</a></text> →
""
- Input: <text>"Yoshiyuki Sadamoto will release &gt;=9 stages of the <em>Evangelion</em> manga in <span class="date-range">2011<sub>13ya</sub></span> 4% <a href="https://predictionbook.com/predictions/2249" class="uri" data-link-icon="?" data-link-icon-type="text,sans,bold" title="Yoshiyuki Sadamoto will release &amp;gt;=9 stages of the _Evangelion_ manga in 2011">https://predictionbook.com/predictions/2249</a>"</text> →
""
- Input: <text>" Thus, the adoption of gunite—along with evolving bank lending rules—resulted in an incredibly rapid expansion of pool construction. In the 2 decades <span class="date-range" title="The date range 1940–1960 lasted 20 years, ending 64 years ago.">1940<span class="subsup"><sup>–</sup><sub>20</sub></span>1960<sub>64ya</sub></span>, pools transformed from luxury good to common household item in Southern California. New installations would peak in <span class="date-range">1964<sub>60ya</sub></span>, with numbers only slightly off that through the rest of the decade.<sup>27</sup></p>"</text> →
""
- Input: <text>20 (ending in <span class="date-range">1960<sub>64ya</sub></span> AD)</text> →
""
- Input: <text>“Panel discussion with Yasuyuki Ueda and Yoshitoshi ABe”</a>, Otakon <span class="date-range">2000<sub>24ya</sub></span>, English</text> →
""
- Input: <text>"Personal reflections on lessons learned from randomized trials involving newborn infants, <span class="date-range">1951<sub>73ya</sub></span> to 1967"</text> →
""
- Input: <text>"<p>Report from DDW, arrested “beginning of 2013” for <span class="date-range">2012<sub>12ya</sub></span> purchases; for simplicity, listed as 2013-01-01.</p>"</text> →
""
- Input: <text>Does Gwern.net follow the famous Benford’s law? A quick analysis suggests that it sort of does, except for the digit 2, probably due to the many citations to research from the past 2 decades (&gt;<span class="date-range">2000<sub>24ya</sub></span> AD).</text> →
""
- Input: <text><em>Human Accomplishment is a <span class="date-range">2003<sub>21ya</sub></span> book by</text> →
""
- Input: <text>public content appearing in Google searches; visibility eliminated October <span class="date-range">2009<sub>15ya</sub></span>, APIs deprecated December <span class="date-range">2010<sub>14ya</sub></span>. Unclear where the content went—seems to’ve been incorporated into other Google services.</text> →
""
- Input: <text>This Haskell tutorial was written in early March <span class="date-range">2011<sub>13ya</sub></span>, and while the below code worked then, it will not work with Github or the necessary Haskell libraries now.</text> →
""
- Input: <text>And, as a follow-up, toward the end of <span class="date-range">1957<sub>67ya</sub></span> Vicary invited 50 reporters to a film studio in New York where he projected some motion picture footage, and claimed that he had also projected a subliminal message.</text> →
""
- Input: <text>The results do not hold after <span class="date-range">2010<sub>14ya</sub></span>. This is critical as Zoom and Dropbox cloud file sharing emerged after <span class="date-range">2010<sub>14ya</sub></span>. </text> →
""
- Input: <text>and by November <span class="date-range">2013<sub>11ya</sub></span> had identified iWork</text> →
""
- Input: <text>Salins itself was almost totally destroyed in <span class="date-range">1825<sub>199ya</sub></span> by a fire that burned for three days. The city of Rennes disappeared in <span class="date-range">1720<sub>304ya</sub></span> and much of Limoges in <span class="date-range">1864<sub>160ya</sub></span></text> →
""
- Input: <text>Raspberry mead, <span class="date-range">2013<sub>11ya</sub></span> bottle</text> →
""
- Input: <text>McMurtry bought this place, the biggest home in town, after he won the Pulitzer Prize for <a href="https://en.wikipedia.org/wiki/Lonesome_Dove" class="link-live" data-link-icon="wikipedia" data-link-icon-type="svg" data-url-iframe="https://en.m.wikipedia.org/wiki/Lonesome_Dove#bodyContent" title="Lonesome Dove"><em>Lonesome Dove</em></a> [in <span class="date-range">1986<sub>38ya</sub></span>]</text> →
""
- Input: <text><a href="https://en.wikipedia.org/wiki/Samuel_Daniel" class="id-not link-live" data-link-icon="wikipedia" data-link-icon-type="svg" data-url-iframe="https://en.m.wikipedia.org/wiki/Samuel_Daniel#bodyContent" title="Samuel Daniel">Samuel Daniel</a> (<a href="https://en.wikipedia.org/wiki/Musophilus" class="link-live" data-link-icon="wikipedia" data-link-icon-type="svg" data-url-iframe="https://en.m.wikipedia.org/wiki/Musophilus#bodyContent" title="Musophilus"><em>Musophilus</em></a> <span class="date-range">1601<sub>423ya</sub></span>)</text> →
""
- Input: <text>Some provisions of the tax law have hurt artists. This includes the taxation of fellowship awards and unemployment compensation (both instituted in <span class="date-range">1986<sub>38ya</sub></span>), disallowance of income averaging (which had helped artists with volatile incomes), and the <span class="date-range">1986<sub>38ya</sub></span> restrictions on in-kind deductions for artworks given to museums.</text> →
""
- Input: <text>AI and Efficiency: We’re Releasing an Analysis Showing That Since <span class="date-range">2012<sub>12ya</sub></span> the Amount of Compute Needed to Train a Neural Net to the Same Performance</text> →
""

End of examples. Reminder: the task is to print out text with numbers malformatted as dates.

Task:

- Input: <text>{target}</text> →
"""}
  ]
)

print(completion.choices[0].message.content)
