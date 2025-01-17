#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# italicizer.py: reformat a string to add italics as semantically appropriate (eg. book titles) using LLMs
# Author: Gwern Branwen
# Date: 2025-01-17
# When:  Time-stamp: "2025-01-17 16:56:54 gwern"
# License: CC-0
#
# Usage: $ OPENAI_API_KEY="sk-XXX" echo [...] | python italicizer.py
#
# Italicizer tries to remove a common annoyance in PDF & HTML-sourced titles: loss of italics formatting.
# If you parse HTML pages for `<title>`, or use APIs like the Wikipedia API, or extract titles from XML PDF metadata, they usually omit italics or make them hard to get, where the correctly-formatted title is presented at all.
# (For example, the standard WP API will not provide the 'formatted' title; one has to use a different endpoint.)
# This is quite annoying if you link a lot of material about books, movies, or foreign topics, as I do, because the metadata will silently be mis-formatted according to standard English conventions. (You don't "read Moby-Dick", you "read <em>Moby Dick</em>".)
# This cannot be easily solved by more complicated scraping, because there is a long tail of metadata sources and in some cases there may be *no* metadata anywhere in the target which includes italics (the target may simply have not bothered to italicize anywhere!).
# But it is something which is easy for a knowledgeable human to do, because they know 'Moby-Dick is the name of a famous novel, and novel titles should be italicized'; it is just too tedious to do at scale.
# This makes it a perfect use-case for LLMs.
#
# Example:
#
# $ OPENAI_API_KEY="sk-XYZ" echo "Moby-Dick" | python italicizer.py
# <em>Moby-Dick</em>

import sys
from openai import OpenAI
client = OpenAI()

if len(sys.argv) == 1:
    target = sys.stdin.read().strip()
else:
    target = sys.argv[1]

completion = client.chat.completions.create(
  # temperature=0,
  model="o1", # the 4o models seem to get too confused & over-italicize
  messages=[
    {"role": "system", "content": "You are a Wikipedia copyeditor. When in doubt, consult MOS:ITALIC <https://en.wikipedia.org/wiki/Wikipedia:Manual_of_Style/Text_formatting#Italic_type>."},
      {"role": "user", "content":
f"""Task: reformatting text to add missing italics.

Summary: Add (HTML <em></em>) italics to text with un-italicized words or phrases.
Follow standard English formal writing style in italicizing.
Words which should be italicized include:

- titles: book, movie, long poem, TV show, periodical, game
- names: ship, plane, spaceship
- words: unfamiliar foreign word or phrase
- science: variables, scientific names like species names
- musical terms

Words which should not be italicized:

- personal names or location names (whether or not foreign)
- software/database
- short works: short stories, poems, symphonies
- loanwords (or other common foreign terms a reader would not need defined)
- brands
- punctuation which is not part of the italicized phrase
- abbreviated titles or acronyms

If the text is correct or you are unsure what to italicize, return the original text unaltered.
(It is better to leave text unchanged than italicize incorrectly.)
If punctuation follows an italicized phrase, keep punctuation outside the <em> tags.

Do not add quote marks, or make any other corrections.
Do not add extra explanation or disclaimers.

Preview of input:

- <text>{target}</text>
[...]

Examples:

- <text>Moby-Dick; or, The Whale, by Herman Melville</text>
<em>Moby-Dick; or, The Whale</em>, by Herman Melville
- <text>Herman Melville</text>
Herman Melville
- <text>Sudoku</text>
Sudoku
- <text>Seppuku</text>
<em>Seppuku</em>
- <text>1ac66705cf14104</text>
1ac66705cf14104
- <text>Hamnet</text>
Hamnet # NOTE: this is a proper name, the name of Shakespeare's son, and so should not be italicized
- <text>Dark Empire</text>
<em>Dark Empire</em>
- <text>Building Secure and Reliable Systems: Chapter 2: Understanding Adversaries § pg51</text>
<em>Building Secure and Reliable Systems</em>: Chapter 2: Understanding Adversaries § pg51
- <text>Going Rogue, Now Unavailable on Kindle: The publishing conspiracy that’s blocking an electronic version of Palin’s memoir</text>
<em>Going Rogue</em>, Now Unavailable on Kindle: The publishing conspiracy that’s blocking an electronic version of Palin’s memoir
- <text>OK Soda</text>
OK Soda
- <text>The USS Enterprise was a famous aircraft carrier</text>
The <em>USS Enterprise</em> was a famous aircraft carrier
- <text>US Enterprise</text>
US Enterprise
- <text>The Millennium Falcon completed the Kessel Run</text>
The <em>Millennium Falcon</em> completed the Kessel Run
- <text>My favorite Poe story is The Masque of the Red Death</text>
My favorite Poe story is The Masque of the Red Death
- <text>I read The New York Times every morning</text>
I read <em>The New York Times</em> every morning
- <text>Times New Roman</text>
Times New Roman
- <text>I ate dinner at El Celler de Can Roca</text>
I ate dinner at El Celler de Can Roca
- <text>Inception was directed by Christopher Nolan</text>
<em>Inception</em> was directed by Christopher Nolan
- <text>I stopped at the bakery café en route to work, which was a faux pas</text>
I stopped at the bakery café en route to work, which was a faux pas
- <text>Les Misérables by Victor Hugo</text>
<em>Les Misérables</em> by Victor Hugo
- <text>The Making of The Lord of the Rings</text>
<em>The Making of The Lord of the Rings</em>
- <text>A Review of Blade Runner</text>
A Review of <em>Blade Runner</em>
- <text>The Raven</text>
The Raven
- <text>The Iliad</text>
<em>The Iliad</em>
- <text>I connected my iPhone to my PlayStation to play my new Lego game, Lego Avengers</text>
I connected my iPhone to my PlayStation to play my new Lego game, <em>Lego Avengers</em>
- <text>Star Trek IV: The Voyage Home.</text>
<em>Star Trek IV: The Voyage Home</em>.
- <text>My favorite dim sum restaurant is Din Tai Fung</text>
My favorite dim sum restaurant is Din Tai Fung
- <text>Pride and Prejudice and Zombies</text>
<em>Pride and Prejudice and Zombies</em>
- <text>Drosophila melanogaster in the wild</text>
<em>Drosophila melanogaster</em> in the wild
- <text>The r variable represents rate</text>
The <em>r</em> variable represents rate
- <text>I ordered sushi and tempura at the restaurant</text>
I ordered sushi and tempura at the restaurant
- <text>She practices kung fu and yoga</text>
She practices kung fu and yoga
- <text>Star Trek The Next Generation: Episode Guide</text>
<em>Star Trek The Next Generation</em>: Episode Guide
- <text>The Lord of the Rings The Fellowship of the Ring</text>
<em>The Lord of the Rings The Fellowship of the Ring</em>
- <text>Have you read Moby-Dick?</text>
Have you read <em>Moby-Dick</em>?
- <text>I love Star Wars: A New Hope!</text>
I love <em>Star Wars: A New Hope</em>!
- <text>We ate at Le Bernardin in New York</text>
We ate at Le Bernardin in New York
- <text>The HMS Victory is in Portsmouth</text>
The <em>HMS Victory</em> is in Portsmouth
- <text>The algorithm uses machine learning to process data</text>
The algorithm uses machine learning to process data
- <text>In physics, we study quantum mechanics</text>
In physics, we study quantum mechanics
- <text>Harry Potter and the Sorcerer's Stone is part of the Harry Potter series</text>
<em>Harry Potter and the Sorcerer's Stone</em> is part of the Harry Potter series
- <text>Born to Run from the album Born to Run</text>
Born to Run from the album <em>Born to Run</em>
- <text>The article "The Making of Star Wars" in The Atlantic</text>
The article "The Making of <em>Star Wars</em>" in <em>The Atlantic</em>
- <text>The article The Making of Star Wars in The Atlantic</text>
The article "The Making of <em>Star Wars</em>" in <em>The Atlantic</em>
- <text>Star Wars Episode VI: Return of the Jedi</text>
<em>Star Wars Episode VI: Return of the Jedi</em>
- <text>Symphony No. 5 (Beethoven)</text>
Symphony No. 5 (Beethoven)
- <text>Some conductors take it in strict allegro tempo; others take the liberty of a weighty treatment, playing the motif in a much slower and more stately tempo; yet others take the motif molto ritardando.</text>
Some conductors take it in strict allegro tempo; others take the liberty of a weighty treatment, playing the motif in a much slower and more stately tempo; yet others take the motif <em>molto ritardando</em>.
- <text>The court ruled in Roe vs Wade that</text>
The court ruled in Roe vs Wade that
- <text>Monet's Water Lilies is displayed at MoMA</text>
<em>Water Lilies</em> is displayed at MoMA
- <text>The Making of Impressionism exhibition at the Met</text>
<em>The Making of Impressionism</em> exhibition at the Met
- <text>van Gogh's The Starry Night</text>
van Gogh's <em>The Starry Night</em>
- <text>Art After Dark: Experiencing The Night Watch</text>
Art After Dark: Experiencing <em>The Night Watch</em>
- <text>Guernica by Pablo Picasso</text>
<em>Guernica</em> by Pablo Picasso
- <text>Proceedings of the 2024 Conference on Computer Vision and Pattern Recognition</text>
<em>Proceedings of the 2024 Conference on Computer Vision and Pattern Recognition</em>
- <text>SIGGRAPH 2024: The Future of Graphics</text>
SIGGRAPH 2024: The Future of Graphics
- <text>Paper presented at the International Conference on Machine Learning</text>
Paper presented at the International Conference on Machine Learning
- <text>Workshop on Natural Language Processing at ACL 2024</text>
Workshop on Natural Language Processing at ACL 2024
- <text>The Treaty of Versailles ended World War I</text>
The Treaty of Versailles ended World War I
- <text>United Nations Declaration of Human Rights</text>
United Nations Declaration of Human Rights
- <text>The Paris Agreement on climate change</text>
The Paris Agreement on climate change
- <text>US Constitution Article I, Section 8</text>
US Constitution Article I, Section 8
- <text>Americans with Disabilities Act of 1990</text>
Americans with Disabilities Act of 1990
- <text>Grokking at the Edge of Numerical Stability</text>
Grokking at the Edge of Numerical Stability
- <text>Complexity Control Facilitates Reasoning-Based Compositional Generalization in Transformers</text>
Complexity Control Facilitates Reasoning-Based Compositional Generalization in Transformers
- <text>foo bar</text>
foo bar
- <text>OK Soda § Can design</text>
OK Soda § Can design
- <text>Thoughts On A Month With Devin</text>
Thoughts On A Month With Devin
- <text>Thoughts On A Month Alone</text>
Thoughts On A Month Alone
- <text>The Making of Community Notes: The team that built Twitter’s Community Notes talks about their design process</text>
The Making of Community Notes: The team that built Twitter’s Community Notes talks about their design process
- <text>Aging, Alzheimer’s Disease and Protein Crosslinking</text>
Aging, Alzheimer’s Disease and Protein Crosslinking
- <text>Designing the Sublime: Boullée and Ledoux’s Architectural Revolution</text>
Designing the Sublime: Boullée and Ledoux’s Architectural Revolution
- <text>Maybe Your Zoloft Stopped Working Because A Liver Fluke Tried To Turn Your Nth-Great-Grandmother Into A Zombie</text>
Maybe Your Zoloft Stopped Working Because A Liver Fluke Tried To Turn Your <em>N</em>th-Great-Grandmother Into A Zombie
- <text>I Am a Cat</text>
<em>I Am a Cat</em>
- <text>I am a cat</text>
I am a cat
- <text>I am a cat.</text>
I am a cat.
- <text>A critique of pure reason</text>
A critique of pure reason # NOTE: a 1987 paper by Drew McDermott, alluding to Kant's <em>Critique of Pure Reason</em>, but not the same (note lowercase & 'a')
- <text>Critik der reinen Vernunft</text>
<em>Critik der reinen Vernunft</em>
- <text>This Time with Feeling: Learning Expressive Musical Performance</text>
This Time with Feeling: Learning Expressive Musical Performance
- <text>Connectionist Music Composition Based on Melodic, Stylistic, and Psychophysical Constraints [Technical report CU-495-90]</text>
Connectionist Music Composition Based on Melodic, Stylistic, and Psychophysical Constraints [Technical report CU-495-90]
- <text>Parallel Distributed Processing: Implications for Cognition and Development</text>
Parallel Distributed Processing: Implications for Cognition and Development
- <text>Direct Fit to Nature: An Evolutionary Perspective on Biological and Artificial Neural Networks</text>
Direct Fit to Nature: An Evolutionary Perspective on Biological and Artificial Neural Networks
- <text>Men of Iron</text>
Men of Iron
- <text>Final Gifts</text>
Final Gifts
- <text>Duck Hunt</text>
<em>Duck Hunt</em>
- <text>The Mulberry Tree</text>
The Mulberry Tree
- <text>2014 Spirulina randomized self-experiment</text>
2014 Spirulina randomized self-experiment
- <text>The Scaling Hypothesis § It From Byte</text>
The Scaling Hypothesis § It From Byte
- <text>Drugs 2.0: Your Crack's in the Post</text>
<em>Drugs 2.0</em>: Your Crack's in the Post
- <text>What to Expect When You’re Expecting…GPT-4. What comes after ChatGPT? 7 predictions for 2023 § GPT-4</text>
What to Expect When You’re Expecting…GPT-4. What comes after ChatGPT? 7 predictions for 2023 § GPT-4
- <text>The Narrowing Circle</text>
The Narrowing Circle
- <text>GPT-2 Howl</text>
GPT-2 Howl
- <text>Intelligence Explosion Microeconomics</text>
Intelligence Explosion Microeconomics
- <text>Evolution of the Human Brain: From Matter to Mind</text>
Evolution of the Human Brain: From Matter to Mind
- <text>The Iron Law Of Evaluation And Other Metallic Rules</text>
The Iron Law Of Evaluation And Other Metallic Rules
- <text>GPT-3 Creative Fiction \167 Dare To Be Stupid?</text>
GPT-3 Creative Fiction \167 Dare To Be Stupid?
- <text>GPT-3 Creative Fiction \167 Book of Jobs</text>
GPT-3 Creative Fiction \167 Book of Jobs
- <text>Progress In Beauty</text>
Progress In Beauty
- <text>Vectors 3.0: Even More Aphorisms and Ten-Second Essays</text>
Vectors 3.0: Even More Aphorisms and Ten-Second Essays
- <text>The Second Apocalypse: Freedom In An Unfree Universe</text>
<em>The Second Apocalypse</em>: Freedom In An Unfree Universe
- <text>Amusing Ourselves to Death? \167 Waller Et Al <span class=\"date-range\">1995<sub><span title=\"1995 was 29 years ago.\">29ya</span></sub></span>, \8216Occupational and Leisure Time Interests, and Personality\8217</text>
Amusing Ourselves to Death? \167 Waller Et Al <span class=\"date-range\">1995<sub><span title=\"1995 was 29 years ago.\">29ya</span></sub></span>, \8216Occupational and Leisure Time Interests, and Personality\8217
- <text>The Kelly Coin-Flipping Game: Exact Solutions</text>
The Kelly Coin-Flipping Game: Exact Solutions
- <text>Amusing Ourselves to Death?</text>
Amusing Ourselves to Death?
- <text>D&D</text>
D&D
- <text>MLP:FiM: S9E23: The Big Mac Question</text>
MLP:FiM: S9E23: The Big Mac Question
- <text>Miscellaneous \167 D&amp;D Game #2 Log</text>
Miscellaneous \167 D&amp;D Game #2 Log
- <text>NGE TV, Episode 6: \"Showdown in Tokyo-3\"/\"Rei-3\"</text>
NGE TV, Episode 6: \"Showdown in Tokyo-3\"/\"Rei-3\"
- <text>Scott and Scurvy: How the Cure for Scurvy Was Lost</text>
Scott and Scurvy: How the Cure for Scurvy Was Lost
- <text>Nature\8217s Spoils: The underground food movement ferments revolution</text>
Nature\8217s Spoils: The underground food movement ferments revolution
- <text>On the Origin and Evolution of Life in the Galaxy</text>
On the Origin and Evolution of Life in the Galaxy
- <text>Why Cats Love Earwax \167 East Asian Survey</text>
Why Cats Love Earwax \167 East Asian Survey
- <text>Why To Not Write A Boo</text>
Why To Not Write A Boo
- <text>Cultural Evolution in Animals</text>
Cultural Evolution in Animals
- <text>The Science of Visual Data Communication: What Works</text>
The Science of Visual Data Communication: What Works
- <text>"Psychology at Michigan: The Pillsbury years, 1897\8211\&1947 \167 John F. Shepard"</text>
"Psychology at Michigan: The Pillsbury years, 1897\8211\&1947 \167 John F. Shepard"
- <text>Coprophagia and Allied Phenomena</text>
Coprophagia and Allied Phenomena

[End of examples. Reminder: your only task is to add missing italics.]

- <text>{target}</text>
"""}
  ]
)

print(completion.choices[0].message.content)
