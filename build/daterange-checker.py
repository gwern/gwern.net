#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# daterange-checker.py: check dates in context for whether they are actually numbers
# Author: Gwern Branwen
# Date: 2024-08-04
# When:  Time-stamp: "2024-08-04 18:58:49 gwern"
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
  model="gpt-4o-mini",
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
- Input: <text><span class="date-range">2001<sub>23ya</sub></span>, 23:20:43, <a href="https://en.wikipedia.org/wiki/Incompatible_Timesharing_System" class="link-annotated-partial link-live" data-link-icon="wikipedia" data-link-icon-type="svg" data-url-html="https://en.m.wikipedia.org/wiki/Incompatible_Timesharing_System#bodyContent" title="Incompatible Timesharing System">ITS</a> Academic Media & Technology”</text> →
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
- Input: <text>Average output losses due to short-run sectoral shocks are an order of magnitude larger than the welfare cost of business cycles calculated by <a href="https://en.wikipedia.org/wiki/Robert_Lucas_Jr." class="link-live" data-url-html="https://en.m.wikipedia.org/wiki/Robert_Lucas_Jr.#bodyContent" data-link-icon="wikipedia" data-link-icon-type="svg">Lucas (<span class="date-range">1987<sub>37ya</sub></span>)</a>. Nonlinearities can also cause shocks to critical sectors to have disproportionate macroeconomic effects, almost tripling the estimated impact of the 1970s oil shocks on world aggregate output</text> →
""
- Input: <text><p>Large and general upward trends in research collaboration are also found in journal publications (eg. Adams et al, <span class="date-range">2004<sub>20ya</sub></span>).</p></text> →
""

End of examples. Reminder: print out text with numbers malformatted as dates.

- Input: <text>{target}</text> →
"""}
  ]
)

print(completion.choices[0].message.content)
