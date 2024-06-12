#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# title-cleaner.py: remove cruft from titles of web pages like website name/domain or error messages
# Author: Gwern Branwen
# Date: 2024-06-11
# When:  Time-stamp: "2024-06-11 22:09:10 gwern"
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
If I Sleep for an Hour, 30 People Will Die
2. Input title to clean: "404"
""
3. Input title to clean: "Gwern.net | Collecting New Socks Efficiently"
Collecting New Socks Efficiently
4. Input title to clean: "worlds of DAVID BRIN"
""
5. Input title to clean: "steve yegge · The Amazon Memo"
The Amazon Memo
6. Input title to clean: "index"
""

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
