#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# italicizer.py: reformat a string to add italics as semantically appropriate (eg. book titles) using LLMs
# Author: Gwern Branwen
# Date: 2025-01-17
# When:  Time-stamp: "2025-02-23 20:14:11 gwern"
# License: CC-0
#
# Usage: $ OPENAI_API_KEY="sk-XXX" echo [...] | python italicizer.py
#
# Italicizer tries to remove a common annoyance in PDF & HTML-sourced titles: loss of italics formatting.
# Input a string, and it will return a new string with italics added, or an empty string literal `""` (to explicitly denote no change & save tokens).
# Italics are intelligently added where high-quality formal English writing would use them, eg film or periodical titles, unusual foreign words, species names, etc. (See the prompt for full details.)
#
# Because of the complexity of adding italics, this is a standalone script. For general-purpose title reformatting, see </static/build/title-cleaner.py>.
#
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
# $ OPENAI_API_KEY="sk-XYZ" italicizer.py "the musician Moby-Dick"
# ""

import sys
from openai import OpenAI
client = OpenAI()

if len(sys.argv) == 1:
    target = sys.stdin.read().strip()
else:
    target = sys.argv[1]

completion = client.chat.completions.create(
  # temperature=0,
    model="o3-mini", # the 4o/o1-mini models aren't smart enough but o1 is too expensive 😢; we compromise with 'o1-preview' which benefits from <https://platform.openai.com/docs/guides/prompt-caching> to cut its price when we run a lot of title-cleaning. Why is that when this seems like such an easy task, albeit a little fiddly? The smaller models seem to fail catastrophically on paper titles, no matter how many I put in, so my best guess is that this is 'tail dropping' behavior from the very aggressive shrinking of small models, to eliminate as much factual/memorized knowledge as possible. (ie. they've forgotten all these papers, which the original large models knew from pretraining on said papers, or at least metadata of those papers or on other papers referencing them)
  messages=[
    # {"role": "system", "content": "You are a Wikipedia copyeditor. When in doubt, consult MOS:ITALIC <https://en.wikipedia.org/wiki/Wikipedia:Manual_of_Style/Text_formatting#Italic_type>."},
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

If the text is correct or you are unsure what to italicize, return the empty string.
(It is better to leave text unchanged than italicize incorrectly.)
If punctuation follows an italicized phrase, keep punctuation outside the <em> tags.

Do not add quote marks, or make any other corrections.
Do not add extra explanation or disclaimers.
When in doubt, consult MOS:ITALIC <https://en.wikipedia.org/wiki/Wikipedia:Manual_of_Style/Text_formatting#Italic_type>.

Examples:

- <text>Moby-Dick; or, The Whale, by Herman Melville</text>
<em>Moby-Dick; or, The Whale</em>, by Herman Melville
- <text>Herman Melville</text>
""
- <text>Sudoku</text>
""
- <text>Seppuku</text>
<em>Seppuku</em>
- <text>1ac66705cf14104</text>
""
- <text>Hamnet</text>
"" # NOTE: this is a proper name, the name of Shakespeare's son, and so should not be italicized
- <text>Dark Empire</text>
<em>Dark Empire</em>
- <text>Building Secure and Reliable Systems: Chapter 2: Understanding Adversaries § pg51</text>
<em>Building Secure and Reliable Systems</em>: Chapter 2: Understanding Adversaries § pg51
- <text>Going Rogue, Now Unavailable on Kindle: The publishing conspiracy that’s blocking an electronic version of Palin’s memoir</text>
<em>Going Rogue</em>, Now Unavailable on Kindle: The publishing conspiracy that’s blocking an electronic version of Palin’s memoir
- <text>OK Soda</text>
""
- <text>The USS Enterprise was a famous aircraft carrier</text>
The <em>USS Enterprise</em> was a famous aircraft carrier
- <text>US Enterprise</text>
""
- <text>The Millennium Falcon completed the Kessel Run</text>
The <em>Millennium Falcon</em> completed the Kessel Run
- <text>My favorite Poe story is The Masque of the Red Death</text>
""
- <text>I read The New York Times every morning</text>
I read <em>The New York Times</em> every morning
- <text>Times New Roman</text>
""
- <text>I ate dinner at El Celler de Can Roca</text>
""
- <text>Inception was directed by Christopher Nolan</text>
<em>Inception</em> was directed by Christopher Nolan
- <text>I stopped at the bakery café en route to work, which was a faux pas</text>
""
- <text>Les Misérables by Victor Hugo</text>
<em>Les Misérables</em> by Victor Hugo
- <text>The Making of The Lord of the Rings</text>
<em>The Making of The Lord of the Rings</em>
- <text>A Review of Blade Runner</text>
A Review of <em>Blade Runner</em>
- <text>The Raven</text>
""
- <text>The Iliad</text>
<em>The Iliad</em>
- <text>I connected my iPhone to my PlayStation to play my new Lego game, Lego Avengers</text>
I connected my iPhone to my PlayStation to play my new Lego game, <em>Lego Avengers</em>
- <text>Star Trek IV: The Voyage Home.</text>
<em>Star Trek IV: The Voyage Home</em>.
- <text>My favorite dim sum restaurant is Din Tai Fung</text>
""
- <text>Pride and Prejudice and Zombies</text>
<em>Pride and Prejudice and Zombies</em>
- <text>Drosophila melanogaster in the wild</text>
<em>Drosophila melanogaster</em> in the wild
- <text>The r variable represents rate</text>
The <em>r</em> variable represents rate
- <text>I ordered sushi and tempura at the restaurant</text>
""
- <text>She practices kung fu and yoga</text>
""
- <text>Star Trek The Next Generation: Episode Guide</text>
<em>Star Trek The Next Generation</em>: Episode Guide
- <text>The Lord of the Rings The Fellowship of the Ring</text>
<em>The Lord of the Rings The Fellowship of the Ring</em>
- <text>Have you read Moby-Dick?</text>
Have you read <em>Moby-Dick</em>?
- <text>I love Star Wars: A New Hope!</text>
I love <em>Star Wars: A New Hope</em>!
- <text>We ate at Le Bernardin in New York</text>
""
- <text>The HMS Victory is in Portsmouth</text>
The <em>HMS Victory</em> is in Portsmouth
- <text>The algorithm uses machine learning to process data</text>
""
- <text>In physics, we study quantum mechanics</text>
""
- <text>Harry Potter and the Sorcerer's Stone is part of the Harry Potter series</text>
<em>Harry Potter and the Sorcerer's Stone</em> is part of the Harry Potter series
- <text>Born to Run from the album Born to Run</text>
Born to Run from the album <em>Born to Run</em>
- <text>The article "The Making of Star Wars" in The Atlantic</text>
The article "The Making of <em>Star Wars</em>" in <em>The Atlantic</em>
- <text>The article The Making of Star Wars in The Atlantic</text>
The article The Making of <em>Star Wars</em> in <em>The Atlantic</em>
- <text>Star Wars Episode VI: Return of the Jedi</text>
<em>Star Wars Episode VI: Return of the Jedi</em>
- <text>Symphony No. 5 (Beethoven)</text>
""
- <text>Some conductors take it in strict allegro tempo; others take the liberty of a weighty treatment, playing the motif in a much slower and more stately tempo; yet others take the motif molto ritardando.</text>
Some conductors take it in strict allegro tempo; others take the liberty of a weighty treatment, playing the motif in a much slower and more stately tempo; yet others take the motif <em>molto ritardando</em>.
- <text>The court ruled in Roe vs Wade that</text>
""
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
""
- <text>Paper presented at the International Conference on Machine Learning</text>
""
- <text>Workshop on Natural Language Processing at ACL 2024</text>
""
- <text>The Treaty of Versailles ended World War I</text>
""
- <text>United Nations Declaration of Human Rights</text>
""
- <text>The Paris Agreement on climate change</text>
""
- <text>US Constitution Article I, Section 8</text>
""
- <text>Americans with Disabilities Act of 1990</text>
""
- <text>Grokking at the Edge of Numerical Stability</text>
""
- <text>Complexity Control Facilitates Reasoning-Based Compositional Generalization in Transformers</text>
""
- <text>foo bar</text>
""
- <text>OK Soda § Can design</text>
""
- <text>Thoughts On A Month With Devin</text>
""
- <text>Thoughts On A Month Alone</text>
""
- <text>The Making of Community Notes: The team that built Twitter’s Community Notes talks about their design process</text>
""
- <text>Aging, Alzheimer’s Disease and Protein Crosslinking</text>
""
- <text>Designing the Sublime: Boullée and Ledoux’s Architectural Revolution</text>
""
- <text>Maybe Your Zoloft Stopped Working Because A Liver Fluke Tried To Turn Your Nth-Great-Grandmother Into A Zombie</text>
Maybe Your Zoloft Stopped Working Because A Liver Fluke Tried To Turn Your <em>N</em>th-Great-Grandmother Into A Zombie
- <text>I Am a Cat</text>
<em>I Am a Cat</em>
- <text>I am a cat</text>
""
- <text>I am a cat.</text>
""
- <text>A critique of pure reason</text>
"" # NOTE: a 1987 paper by Drew McDermott, alluding to Kant's <em>Critique of Pure Reason</em>, but not the same (note lowercase & 'a')
- <text>Critik der reinen Vernunft</text>
<em>Critik der reinen Vernunft</em>
- <text>This Time with Feeling: Learning Expressive Musical Performance</text>
""
- <text>Connectionist Music Composition Based on Melodic, Stylistic, and Psychophysical Constraints [Technical report CU-495-90]</text>
""
- <text>Parallel Distributed Processing: Implications for Cognition and Development</text>
""
- <text>Direct Fit to Nature: An Evolutionary Perspective on Biological and Artificial Neural Networks</text>
""
- <text>Men of Iron</text>
""
- <text>Final Gifts</text>
""
- <text>Duck Hunt</text>
<em>Duck Hunt</em>
- <text>The Mulberry Tree</text>
""
- <text>2014 Spirulina randomized self-experiment</text>
""
- <text>The Scaling Hypothesis § It From Byte</text>
""
- <text>Drugs 2.0: Your Crack's in the Post</text>
<em>Drugs 2.0</em>: Your Crack's in the Post
- <text>What to Expect When You’re Expecting…GPT-4. What comes after ChatGPT? 7 predictions for 2023 § GPT-4</text>
""
- <text>The Narrowing Circle</text>
""
- <text>GPT-2 Howl</text>
""
- <text>Intelligence Explosion Microeconomics</text>
""
- <text>Evolution of the Human Brain: From Matter to Mind</text>
""
- <text>The Iron Law Of Evaluation And Other Metallic Rules</text>
""
- <text>GPT-3 Creative Fiction \167 Dare To Be Stupid?</text>
""
- <text>GPT-3 Creative Fiction \167 Book of Jobs</text>
""
- <text>Progress In Beauty</text>
""
- <text>Vectors 3.0: Even More Aphorisms and Ten-Second Essays</text>
""
- <text>The Second Apocalypse: Freedom In An Unfree Universe</text>
<em>The Second Apocalypse</em>: Freedom In An Unfree Universe
- <text>Amusing Ourselves to Death? \167 Waller Et Al <span class=\"date-range\">1995<sub><span title=\"1995 was 29 years ago.\">29ya</span></sub></span>, 8216Occupational and Leisure Time Interests, and Personality8217</text>
""
- <text>The Kelly Coin-Flipping Game: Exact Solutions</text>
""
- <text>Amusing Ourselves to Death?</text>
""
- <text>D&D</text>
""
- <text>MLP:FiM: S9E23: The Big Mac Question</text>
""
- <text>Miscellaneous \167 D&amp;D Game #2 Log</text>
""
- <text>NGE TV, Episode 6: \"Showdown in Tokyo-3\"/\"Rei-3\"</text>
""
- <text>Scott and Scurvy: How the Cure for Scurvy Was Lost</text>
""
- <text>Nature8217s Spoils: The underground food movement ferments revolution</text>
""
- <text>On the Origin and Evolution of Life in the Galaxy</text>
""
- <text>Why Cats Love Earwax \167 East Asian Survey</text>
""
- <text>Why To Not Write A Boo</text>
""
- <text>Cultural Evolution in Animals</text>
""
- <text>The Science of Visual Data Communication: What Works</text>
""
- <text>"Psychology at Michigan: The Pillsbury years, 18978211&1947 \167 John F. Shepard"</text>
""
- <text>Coprophagia and Allied Phenomena</text>
""
- <text>Distributed Learning: Data, Metacognition, and Educational Implications</text>
""
- <text>Self-Regulated Learning: Beliefs, Techniques, and Illusions</text>
""
- <text>Scents and Sensibility</text>
""
- <text>Abandoned Footnotes</text>
<em>Abandoned Footnotes</em>
- <text>Major Crimes as Analogs to Potential Threats to Nuclear Facilities and Programs</text>
""
- <text>The Perfect Heist: Recipes from Around the World [combined papers + slides]</text>
""
- <text>The Great Paper Caper: Years of running drugs and boosting cars left Frank Bourassa thinking: There8217s got to be an easier way to earn a dishonest living. That8217s when he nerved up the idea to make his fortune. (Literally.) Which is how Frank became the most prolific counterfeiter in American history8212a guy with more than $200 million in nearly flawless fake twenties stuffed in a garage. How he got away with it all, well, that8217s even crazier.</text>
""
- <text>or genotype–environment interaction or G×E</text>
""
- <text>Agenda Seeding: How 1960s Black Protests Moved Elites, Public Opinion and Voting</text>
""
- <text>The Surprising Creativity of Digital Evolution: A Collection of Anecdotes from the Evolutionary Computation and Artificial Life Research Communities</text>
""
- <text>The Nature of Selection</text>
""
- <text>The Sound of Pixels</text>
""
- <text>30 years later: lessons from the Multics security evaluation</text>
""
- <text>A Box, Darkly: Obfuscation, Weird Languages, and Code esthetics</text>
""
- <text>Accelerating Self-Play Learning in Go</text>
""
- <text>Accelerating and Improving AlphaZero Using Population Based Training</text>
""
- <text>Adversarial Policies: Attacking Deep Reinforcement Learning</text>
""
- <text>Agent57: Outperforming the Atari Human Benchmark</text>
""
- <text>Aligning Superhuman AI with Human Behavior: Chess as a Model System</text>
""
- <text>Alpha MAML: Adaptive Model-Agnostic Meta-Learning</text>
""
- <text>AlphaX: eXploring Neural Architectures with Deep Neural Networks and Monte Carlo Tree Search</text>
""
- <text>Analyzing Multi-Head Self-Attention: Specialized Heads Do the Heavy Lifting, the Rest Can Be Pruned</text>
""
- <text>Analyzing and Improving the Image Quality of StyleGAN</text>
""
- <text>And the Bit Goes Down: Revisiting the Quantization of Neural Networks</text>
""
- <text>Approximate exploitability: Learning a best response in large games</text>
""
- <text>Architecting energy-efficient STT-RAM based register file on GPGPUs via delta compression</text>
""
- <text>Are Labels Required for Improving Adversarial Robustness?</text>
""
- <text>Assessing Game Balance with AlphaZero: Exploring Alternative Rule Sets in Chess</text>
""
- <text>Atari-HEAD: Atari Human Eye-Tracking and Demonstration Dataset</text>
""
- <text>AutoML: A Survey of the State-of-the-Art</text>
""
- <text>Autocurricula and the Emergence of Innovation from Social Interaction: A Manifesto for Multi-Agent Intelligence Research</text>
""
- <text>Avoid News: Towards a Healthy News Diet</text>
""
- <text>Benchmarking Bonus-Based Exploration Methods on the Arcade Learning Environment</text>
""
- <text>Blockchain Incentivized Data Forwarding in MANETs: Strategies and Challenges</text>
""
- <text>Breaking POps/J Barrier with Analog Multiplier Circuits Based on Nonvolatile Memories</text>
""
- <text>Chain Letter Evolution</text>
""
- <text>Collective Dynamics of Dark Web Marketplaces</text>
""
- <text>Common Lisp: The Untold Story</text>
""
- <text>Concealed Data Poisoning Attacks on NLP Models</text>
""
- <text>Correspondences Regarding Cryptography between John Nash and the NSA</text>
""
- <text>Crash-Only Software</text>
""
- <text>Curriculum Learning for Reinforcement Learning Domains: A Framework and Survey</text>
""
- <text>ELI5: Long Form Question Answering</text>
""
- <text>ES-ENAS: Blackbox Optimization over Hybrid Spaces via Combinatorial and Continuous Evolution</text>
""
- <text>EfficientNet: Rethinking Model Scaling for Convolutional Neural Networks</text>
""
- <text>Encoding Musical Style with Transformer Autoencoders</text>
""
- <text>End-To-End Arguments In System Design</text>
""
- <text>Enhanced POET: Open-Ended Reinforcement Learning through Unbounded Invention of Learning Challenges and their Solutions</text>
""
- <text>Essays in Demand Estimation: Illicit Drugs and Commercial Mushrooms</text>
""
- <text>Exokernel: An Operating System Architecture for Application-Level Resource Management</text>
""
- <text>Explorable Explanations</text>
""
- <text>FRACTRAN: A Simple Universal Programming Language for Arithmetic</text>
""
- <text>Folklore</text>
""
- <text>Forgotten books: The application of unseen species models to the survival of culture</text>
""
- <text>From Genotype to Phenotype: polygenic prediction of complex human traits</text>
""
- <text>Generating images from caption and vice versa via CLIP-Guided Generative Latent Space Search</text>
""
- <text>Glowworm Attack: Optical TEMPEST Sound Recovery via a Device8217s Power Indicator LED</text>
""
- <text>Going, Going, Gone: Lost Internet References</text>
""
- <text>Gradient Descent: The Ultimate Optimizer</text>
""
- <text>Hierarchy in the library: Egalitarian dynamics in Victorian novels</text>
""
- <text>How Correlated Are You?</text>
""
- <text>Hypersim: A Photorealistic Synthetic Dataset for Holistic Indoor Scene Understanding</text>
""
- <text>Imitation-driven Cultural Collapse</text>
""
- <text>Implementing Recommendations From Web Accessibility Guidelines: Would They Also Provide Benefits to Nondisabled Users</text>
""
- <text>Improving the Interpretability of fMRI Decoding using Deep Neural Networks and Adversarial Robustness</text>
""
- <text>It8217s the Latency, Stupid</text>
""
- <text>Large Scale Adversarial Representation Learning</text>
""
- <text>Learning To Follow Directions in Street View</text>
""
- <text>Learning by Cheating</text>
""
- <text>Learning to Predict Without Looking Ahead: World Models Without Forward Prediction</text>
""
- <text>Learning to Seek: Autonomous Source Seeking with Deep Reinforcement Learning Onboard a Nano Drone Microcontroller</text>
""
- <text>Learning to Simulate Dynamic Environments with GameGAN</text>
""
- <text>Liberalizing art. Evidence on the Impressionists at the end of the Paris Salon</text>
""
- <text>Low-dimensional Embodied Semantics for Music and Language</text>
""
- <text>MAUVE: Measuring the Gap Between Neural Text and Human Text using Divergence Frontiers</text>
""
- <text>MELD: Meta-Reinforcement Learning from Images via Latent State Models</text>
""
- <text>MSG-GAN: Multi-Scale Gradients for Generative Adversarial Networks</text>
""
- <text>MULE: Multimodal Universal Language Embedding</text>
""
- <text>Mathematical Marbling</text>
""
- <text>Measuring the Algorithmic Efficiency of Neural Networks</text>
""
- <text>Meta-Learning in Neural Networks: A Survey</text>
""
- <text>Meta-Learning without Memorization</text>
""
- <text>Meta-World: A Benchmark and Evaluation for Multi-Task and Meta Reinforcement Learning</text>
""
- <text>Meta-learning of Sequential Strategies</text>
""
- <text>Mirostat: A Neural Text Decoding Algorithm that Directly Controls Perplexity</text>
""
- <text>Monte-Carlo Tree Search as Regularized Policy Optimization</text>
""
- <text>Mother Earth Mother Board</text>
""
- <text>Multiplayer AlphaZero</text>
""
- <text>N-BEATS: Neural basis expansion analysis for interpretable time series forecasting</text>
""
- <text>Networks, Creativity, and Time: Staying Creative through Brokerage and Network Rejuvenation</text>
""
- <text>Neural Machine Translation with Monte-Carlo Tree Search</text>
""
- <text>New Strategy of Lossy Text Compression</text>
""
- <text>Noisy Sorting Without Resampling</text>
""
- <text>Notes on a Strange World: Houdini8217s Impossible Demonstration</text>
""
- <text>Offline Reinforcement Learning: Tutorial, Review, and Perspectives on Open Problems</text>
""
- <text>On Non-Computable Functions</text>
""
- <text>On Unsettleable Arithmetical Problems</text>
""
- <text>Optimal Policies Tend to Seek Power</text>
""
- <text>ParPaRaw: Massively Parallel Parsing of Delimiter-Separated Raw Data</text>
""
- <text>People Prefer Simpler Content When There Are More Choices: A Time Series Analysis of Lyrical Complexity in Six Decades of American Popular Music</text>
""
- <text>Phishing With a Darknet: Imitation of Onion Services</text>
""
- <text>PiRank: Learning To Rank via Differentiable Sorting</text>
""
- <text>Placement Optimization with Deep Reinforcement Learning</text>
""
- <text>Policy Gradient Search: Online Planning and Expert Iteration without Search Trees</text>
""
- <text>Pop Music Transformer: Beat-based Modeling and Generation of Expressive Pop Piano Compositions</text>
""
- <text>Practical Probabilistic Programming with Monads</text>
""
- <text>Prompt Programming for Large Language Models: Beyond the Few-Shot Paradigm</text>
""
- <text>Psychic Paper</text>
""
- <text>Putting out the hardware dumpster fire</text>
""
- <text>Ray Interference: a Source of Plateaus in Deep Reinforcement Learning</text>
""
- <text>Ridge Rider: Finding Diverse Solutions by Following Eigenvectors of the Hessian</text>
""
- <text>STRML: Projects and Work</text>
""
- <text>Scaling Scaling Laws with Board Games</text>
""
- <text>Scholarly Context Not Found: One in Five Articles Suffers from Reference Rot</text>
""
- <text>Search on the Replay Buffer: Bridging Planning and Reinforcement Learning</text>
""
- <text>Selective Eye-gaze Augmentation To Enhance Imitation Learning In Atari Games</text>
""
- <text>Selling Drugs on Darkweb Cryptomarkets: Differentiated Pathways, Risks and Rewards</text>
""
- <text>Sex, Drugs, and Bitcoin: How Much Illegal Activity Is Financed through Cryptocurrencies?</text>
""
- <text>Show Your Work: Improved Reporting of Experimental Results</text>
""
- <text>Signaling Status with Luxury Goods: The Role of Brand Prominence</text>
""
- <text>Sorting from Noisy Information</text>
""
- <text>SpArSe: Sparse Architecture Search for CNNs on Resource-Constrained Microcontrollers</text>
""
- <text>Sparse Networks from Scratch: Faster Training without Losing Performance</text>
""
- <text>Spearman8217s Rho for the AMH Copula: a Beautiful Formula</text>
""
- <text>Spot Me if You Can: Uncovering Spoken Phrases in Encrypted VoIP Conversations</text>
""
- <text>Stabilizing Generative Adversarial Networks: A Survey</text>
""
- <text>Stabilizing the Lottery Ticket Hypothesis</text>
""
- <text>Style Generator Inversion for Image Enhancement and Animation</text>
""
- <text>Synthetic Petri Dish: A Novel Surrogate Model for Rapid Architecture Search</text>
""
- <text>TV or not TV? The impact of subtitling on English skills</text>
""
- <text>Tackling Morpion Solitaire with AlphaZero-like Ranked Reward Reinforcement Learning</text>
""
- <text>Tag2Pix: Line Art Colorization Using Text Tag With SECat and Changing Loss</text>
""
- <text>TextSETTR: Few-Shot Text Style Extraction and Tunable Targeted Restyling</text>
""
- <text>The 1-Bit Instrument: The Fundamentals of 1-Bit Synthesis, Their Implementational Implications, and Instrumental Possibilities</text>
""
- <text>The Advent Of Cryptology In The Game Of Bridge</text>
""
- <text>The Bayesian brain: the role of uncertainty in neural coding and computation</text>
""
- <text>The British Navy Rules: Monitoring and Incompatible Incentives in the Age of Fighting Sail</text>
""
- <text>The Busy Beaver Frontier</text>
""
- <text>The Cult of the Imperfect</text>
""
- <text>The Curious Case of Neural Text Degeneration</text>
""
- <text>The Little Engines That Could: Modeling the Performance of World Wide Web Search Engines</text>
""
- <text>The Overfitted Brain: Dreams evolved to assist generalization</text>
""
- <text>The Prevalence and Inaccessibility of Internet References in the Biomedical Literature at the Time of Publication</text>
""
- <text>The Recursive Universe</text>
""
- <text>The Skinny on Celebrities: Parasocial Relationships Moderate the Effects of Thin Media Figures on Women8217s Body Image</text>
""
- <text>The Value Equivalence Principle for Model-Based Reinforcement Learning</text>
""
- <text>The Wheel of Reincarnation</text>
""
- <text>The operating system: should there be one?</text>
""
- <text>Time-Lock Puzzles in the Random Oracle Model</text>
""
- <text>Trail: a track-based logging disk architecture for zero-overhead writes</text>
""
- <text>Training Learned Optimizers with Randomly Initialized Learned Optimizers</text>
""
- <text>Transfer of Fully Convolutional Policy-Value Networks Between Games and Game Variants</text>
""
- <text>Uniform Resource Locator Decay in Dermatology Journals: Author Attitudes and Preservation Practices</text>
""
- <text>Universal Entropy of Word Ordering Across Linguistic Families</text>
""
- <text>Video-Based Cryptanalysis: Extracting Cryptographic Keys from Video Footage of a Device8217s Power LED</text>
""
- <text>Weight Agnostic Neural Networks</text>
""
- <text>What Every Programmer Should Know About Memory</text>
""
- <text>What are Weird Machines?</text>
""
- <text>Why We Fight Over Fiction</text>
""
- <text>Zip Files: History, Explanation and Implementation</text>
""
- <text>945-Rank: Multi-Agent Evaluation by Evolution</text>
""
- <text>960-IW: Deep Policies for Width-Based Planning in Pixel Domains</text>
""
- <text>not-so-BigGAN: Generating High-Fidelity Images on Small Compute with Wavelet-based Super-Resolution</text>
""
- <text>Beware Trivial Inconveniences</text>
""
- <text>StyleGAN-NADA: CLIP-Guided Domain Adaptation of Image Generators</text>
""
- <text>The AI Economist: Optimal Economic Policy Design via Two-level Deep Reinforcement Learning</text>
""
- <text>FairyTailor: A Multimodal Generative Framework for Storytelling</text>
""
- <text>PatrickStar: Parallel Training of Pre-trained Models via Chunk-based Memory Management</text>
""
- <text>Teaching Autoregressive Language Models Complex Tasks By Demonstration</text>
""
- <text>Moby the dick</text>
""
- <text>Qitmir (dog)</text>
<em>Qitmir</em> (dog)
- <text>How to install Linux on a dead badger</text>
""
- <text>Haskell: A Great Procedural Language</text>
""
- <text>AniSora: Exploring the Frontiers of Animation Video Generation in the Sora Era</text>
""
- <text>The Smith v. Substack saga</text>
""
- <text>A divided mind: Observations on the conscious properties of the separated hemispheres</text>
""
- <text>The impact of the ‘open’ workspace on human collaboration</text>
""
- <text>What o3 Becomes by 2028</text>
""
- <text>Chimes at Midnight</text>
""
- <text>Anomalous Tokens in DeepSeek-V3 and r1</text>"
""
- <text>When therapy causes harm</text>
""
- <text>Psychological Treatments That Cause Harm</text>
""
- <text>Unwanted Events and Side Effects in Cognitive Behavior Therapy</text>
""
- <text>Design Graveyard</text>
""
- <text>U.S. Free Association with Greenland: A Bad Deal</text>
""
- <text>The Old Family Photos Project: Lessons in creating family photos that people want to keep</text>
""
- <text>Charisma and Representation</text>
""
- <text>La Baleine (automobile)</text>
<em>La Baleine</em> (automobile)
- <text>DeepSeek: The View from China</text>
""
- <text>L. V. Kantorovich: The Price Implications of Optimal Planning</text>
""
- <text>The Doctor Who Drank Infectious Broth, Gave Himself an Ulcer, and Solved a Medical Mystery</text>
""
- <text>Self-Verification, The Key to AI</text>
""
- <text>Replication Data for: Predispositions and the Political Behavior of American Economic Elites: Evidence from Technology Entrepreneurs</text>
""
- <text>Urban Sanitation in Preindustrial Japan</text>
""
- <text>Speculations Concerning the First Ultraintelligent Machine</text>
""
- <text>Letter Spirit (part two): Modeling creativity in a visual domain</text>
""
- <text>The Ascent of Cat Breeds: Genetic Evaluations of Breeds and Worldwide Random Bred Populations</text>
""
- <text>Cerebras Architecture Deep Dive: First Look Inside the HW/SW Co-Design for Deep Learning [Updated]</text>
""
- <text>Lockheed CL-1201</text>
""
- <text>Deep Research Dispatch: OpenAI's Answers to Your Questions</text>
""
- <text>The Cat’s Meat Man: Feeding Felines in Victorian London</text>
""
- <text>Competitive Programming with Large Reasoning Models</text>
""
- <text>Learning To Be Me</text>
""
- <text>https://kenbertagnolli.com/2025/02/09/how-we-achieved-a-1000x-improvement-in-performance/ Three Orders of Magnitude: Transforming PDC Technology at US Synthetic - Ken Bertagnolli</text>
""
- <text>Nil Communication: How to Send a Message without Sending Anything at All</text>
""
- <text>Using Black Holes to Conquer Space: The Halo Drive!</text>
""
- <text>Pondering the ‘Dyson Slingshot’</text>
""
- "https://en.wikipedia.org/wiki/Ecology_of_fear Ecology of fear"
""
- "The Ecology of Fear: Optimal Foraging, Game Theory, and Trophic Interactions"
""
- "Fixing the Internet for Real Time Applications: Part I"
""
- "Fixing the Internet for Real-Time Applications: Part III"
""
- "DS R1 is not on par with o1, and the difference is qualitative, not quantitative"
""
- "Pulling Out The Big Guns For Needle Phobia In An Insane World Where Nobody Seems To Take It Seriously"
""
- "What’s Wrong With This Lagrangean?"
""
- "Foundations of algorithmic thermodynamics"
""
- "Meditating More Made me Sleep Better and Feel Worse"
""
- "When Falsification is the Only Path to Truth"
""
- "Power Lies Trembling: a 3-book review"
""
- "‘Bring Me the Poison’: On the Trail With Trump’s Inner Circle of Suck-Ups"
""
- "Medieval Manuscripts Provenance: The RECEPTIO-Rossi Affair IV: My ‘Accusations’"
""
- "A Firsthand Account of What Homelessness in America Is Really Like"
""

[End of examples. Reminder: your only task is to add missing italics you are sure of.]

- <text>{target}</text>
"""}
  ]
)

print(completion.choices[0].message.content)
