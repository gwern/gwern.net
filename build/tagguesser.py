#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# tagguesser.py: suggest a tag for links/annotations based on a list of titles fed into the OA API
# Author: Gwern Branwen
# Date: 2023-06-17
# When:  Time-stamp: "2023-06-21 10:01:13 gwern"
# License: CC-0
#
# Usage: $ OPENAI_API_KEY="sk-XXX" xclip -o | python tagguesser.py
#
# Used in sort-by-magic to label the implicit clusters found by resorting a list of annotations using pairwise nearest-neighbors.
# The clusters can then be extracted into separate sublists, and the sublists labeled one by one, by taking the titles of each
# annotation (the abstract is probably major overkill, when we just want essentially a single global keyword) and just asking
# GPT to summarize them all (shuffling to reduce ordering effects) as a single word/phrase. Can be used to suggest new tags
# (the name + candidate links to populate it).
# See /design#tags for more.

import random
import signal
import sys
import openai

# define timeout handling:
def handler(signum, frame):
    raise TimeoutError("Function call timed out")

# define function to run with timeout:
def run_with_timeout(func_name, args=(), kwargs={}, timeout=5):
    func = getattr(openai.ChatCompletion, func_name)
    signal.signal(signal.SIGALRM, handler)
    signal.alarm(timeout)
    try:
        result = func(*args, **kwargs)
    except TimeoutError as e:
        result = None
    finally:
        signal.alarm(0)
    return result

def shuffle_input(input_text):
    lines = input_text.split('\n')
    return '\n'.join(random.sample(lines, len(lines)))

if len(sys.argv) == 1:
    input_text = sys.stdin.read().strip()
else:
    input_text = sys.argv[1]
target = shuffle_input(input_text)

messages = [
    {"role": "system", "content": "You are a helpful webmaster & research assistant "},
    {"role": "user", "content": f"Below is a list of article titles (between the '<title>' and '</title>' labels) with some unifying theme or topic.\nPlease suggest a 1-word or 2-words-max phrase, which can be used as a tag for organizing documents. The tag should summarize them in a simple comprehensible way, be easy to type, be singular not plural, be lowercase alphanumerical only, English, and be command-line & URL safe.\nExample tags include 'mead, video, fiction, psychedelic, scaling, discrete, bird, tabular, anxiety, hardware, heritable, t5, adversarial, dnm-archive, imitation-learning, mulberry-tree, muzero, nonfiction, long-now, sociology, prediction, linkrot'.\nOutput your tag suggestion on a single line by itself, with no other formatting or padding. Do not make any comments or suggestions. Do not print anything but your suggested tag.\n<titles>\n{target}\n</titles>"}
]

result = run_with_timeout(
    "create",
    kwargs={
        # "model": "gpt-4",
        "model": "gpt-3.5-turbo",
        "messages": messages,
        # "max_tokens": 4090,
        "temperature": 0
    },
    timeout=120
)

if result is None:
                          sys.stderr.write("Function call timed out")
else:
                          print(result['choices'][0]['message']['content'])
