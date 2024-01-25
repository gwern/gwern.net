#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# tagguesser.py: suggest a tag for links/annotations based on a list of titles fed into the OA API
# Author: Gwern Branwen
# Date: 2023-06-17
# When:  Time-stamp: "2024-01-24 17:36:42 gwern"
# License: CC-0
#
# Usage: $ OPENAI_API_KEY="sk-XXX" xclip -o | python tagguesser.py
#
# Used in sort-by-magic to label the implicit clusters found by resorting a list of annotations using pairwise nearest-neighbors.
# The clusters can then be extracted into separate sublists, and the sublists labeled one by one, by taking the titles of each
# annotation (the abstract is probably major overkill, when we just want essentially a single global keyword) and just asking
# GPT to summarize them all (shuffling to reduce ordering effects) as a single word/phrase. Can be used to suggest new tags
# (the name + candidate links to populate it).
# Input is newline delimited: first, the 'original' tag on 1 line, followed by a line for additional tags it *should not* use
# even if they are the best-seeming, then by the actual newline-delimited list of titles.
# See /design#tags for more.
#
# eg.
# $ echo -e 'psychology/smell\nJohn Smith\nArchaeometric Identification of a Perfume from Roman Times\nEau De Cleopatra: Mendesian Perfume and Tell Timai\nThe Scent of the Nile: Jean-Claude Ellena Creates a New Perfume\nThe Odor Value Concept in the Formal Analysis of Olfactory Art\nThe Aesthetics of Smelly Art\nArithmetic By Smell' | ./tagguesser.py
# olfactory-art

import random
import sys
from openai import OpenAI

client = OpenAI()

def shuffle_input(input_text):
    # lines = input_text.split('\n')
    return '\n'.join(random.sample(input_text, len(input_text)))

if len(sys.argv) == 1:
    input_text = sys.stdin.read().strip()
else:
    input_text = sys.argv[1]

# the first line is '$PARENT_TAG [$BLACKLIST_1 $BLACKLIST_2 ...]': we have a 'parent tag' (the current tag we're trying to break down into more refined clusters), and 'blacklisted tags', which are tag names which have been produced by previous tagguesser.py runs (probably) and shouldn't be reused and the call should try to think of something more novel & relevant. We feed those in as well.
lines = input_text.split('\n')
parent_tag = lines[0]
blacklist_tags = lines[1]
targetUnshuffled = lines[2:]
target = shuffle_input(targetUnshuffled)

prompt_text = f"You are a helpful research librarian. Below is a list of article titles (between the '<title>' and '</title>' labels) with some unifying theme or topic.\nPlease suggest a 1-word or 2-words-max phrase, which can be used as a tag for organizing documents, which is more specific than the current tag for them ('{parent_tag}'). The tag should summarize them in a simple comprehensible way, be easy to type, be singular not plural, be lowercase alphanumerical only, hyphen-separated English, and be command-line & URL safe.\nExample tags include 'video, fiction, psychedelic, scaling, discrete, bird, tabular, anxiety, hardware, heritable, t5, elon-musk, adversarial, dnm-archive, imitation-learning, mulberry-tree, muzero, nonfiction, long-now, sociology, prediction, linkrot, spacex-critique'.\nOutput tag suggestions on a single line, with no other formatting or padding such as quotation marks. Do not write any comments or suggestions. Do not print anything but your suggested tag. Do not use any of the following tags: '{blacklist_tags}'.\nFirst, step by step, generate 5 tag suggestions, which are unique and are not any of the previous tags. Then select one best tag out of the 5 tag suggestions, and print it on the final line by itself with no other formatting. Print only one tag like '\nfoo\n'.\n The input to summarize:\n<titles>\n{target}\n</titles>"

completion = client.chat.completions.create(
    # "model": "gpt-4", # TODO: once caching is implemented, switch to GPT-4 for the highest-possible quality. (Unfortunately, it'd cost way too much to run them all through GPT-4 each time, which is how the current sort-by-magic auto-tagging works.)
    model =  "gpt-4-1106-preview",
    messages=[
    {"role": "system", "content": "You are a helpful research librarian."},
    {"role": "user",   "content": prompt_text }
  ]
)

print(completion.choices[0].message.content)
