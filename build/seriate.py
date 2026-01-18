#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# seriate.py: semantically sort, or 'seriate', a list in a logical fashion
# Author: Gwern Branwen
# Date: 2025-01-02
# When:  Time-stamp: "2026-01-17 16:53:11 gwern"
# License: CC-0
#
# Usage: $ OPENAI_API_KEY="sk-XXX" xclip -o | python seriate.py
#
# Many 'lists' can be ordered in a meaningful way, to bring similar items closer together and move dissimilar items further away, but in ways which do not follow a strict comparison sort which implements a full proper ordering.
#
# In a classic sorting, we sort 'ACB'→'ABC'; but in a seriation, we might instead seriate 'Dog, Horse, Kitty' → 'Kitty, Dog, Horse'. (It might be hard to say in exactly what sense the second seriated version could be considered 'sorted'—size? phylogenetic similarity?—but it clearly makes more sense and is less confusing to read.)
#
# We seriate by asking the LLM to resort the list in a logical way, whatever that might mean in a given context, and we keep doing so to a fixed point (ie. the list stops changing). After that, we do a simple weak check that nothing was lost by looking at character-based set (ie. that the final output is a permutation of the input), to try to ensure a lossless transformation which preserved everything aside from improved seriation.
#
# For example, images or paragraphs or lists of similar essays can be put into clearly more or less 'sorted' order, which cluster similar items, without obeying any obvious comparison function like a lexicographic sorting function. This sort of distance minimization is known as 'seriation' (or 'ordination'), and can be seen as a generalization of regular sorting; see <https://en.wikipedia.org/wiki/Seriation_(archaeology)>/<https://en.wikipedia.org/wiki/Ordination_(statistics)>/<https://www.jstatsoft.org/article/view/v025i03> (and its inverse, maximizing distance, can be seen as a kind of seriation too, although things get fuzzier there, see <https://gwern.net/unsort>).
#
# This can be done by hand, and should, because it makes such lists easier to read; but as usual, is too much work for a subtle benefit, and can only be done for static lists. So, we want to automate it.
# This may also be a useful primitive for LLM writing, by enabling a *seriation* pass: a first pass which cleans up text input by constraining edits to seriate it, *without modifying any words*. (For example, one could ask ChatGPT to clean up notes from a conversation or from brainstorming or jotting down text fragments, but invariably, ChatGPT will do more than just reorganize, and will wind up omitting parts, or rewriting into ChatGPTese. If one could first make ChatGPT seriate the notes, and then summarize it recursively to create a hierarchical Table of Contents & an abstract, and only *then* start rewriting it, the results might be much better.)

import sys
from openai import OpenAI
client = OpenAI()

def is_permutation(a, b):
    return sorted(a) == sorted(b)

def seriate_once(target):
    prompt = """Task: 'seriate' inputs, by sorting them into a best-effort context-dependent 'logical order', at each level of the input.
Inputs should be seriated, but otherwise not modified. Every item should be included.
Output only the reordered items inside '<output></output>' tags. No preface, no postscript, no additional tags or commentary.
If multiple orders are equally good, keep the original relative order (stable seriation).
If unsure how to seriate, or no seriation is possible, return the empty string.

Task examples:

# null example:
- <input>I like dogs and cats.</input>
<output>""</output>
# Single item: cannot be seriated (trivial)
- <input>apple</input>
<output>""</output>
# Already seriated: return unchanged
- <input>A, B, C, D</input>
<output>A, B, C, D</output>
# numbers seriated by magnitude despite different writing conventions:
- <input>10^3 01 10 googolplex 9 3</input>
<output>01 3 9 10 10^3 googolplex</output>
# Magnitude ordering (units)
- <input>mm; km; cm; m</input>
<output>mm; cm; m; km</output>
# colors of the rainbow:
- <input>red, purple, blue, green, orange, yellow</input>
<output>red, yellow, orange, green, blue, purple</output>
# by value:
- <input>'gold platinum silver copper'</input>
<output>'copper silver gold platinum'</output>
# Process/causal ordering
- <input>land | take off | taxi to runway | board passengers</input>
<output>board passengers | taxi to runway | take off | land</output>
# Hierarchical/progression ordering
- <input>PhD, Bachelor's, High School, Master's, Elementary</input>
<output>Elementary, High School, Bachelor's, Master's, PhD</output>
# Generational ordering
- <input>grandchild, parent, great-grandparent, child, grandparent</input>
<output>great-grandparent, grandparent, parent, child, grandchild</output>
# Priority ordering (high to low)
- <input>- low priority
- critical
- medium priority
- urgent</input>
<output>- critical
- urgent
- medium priority
- low priority</output>
# election order:
- <input>Washington, Trump, Clinton, Jefferson...</input>
<output>Washington, Jefferson, Clinton, Trump...</output>
# Temporal/historical ordering
- <input>Renaissance, Bronze Age, Modern Era, Iron Age, Stone Age</input>
<output>Stone Age, Bronze Age, Iron Age, Renaissance, Modern Era</output>
# Spatial ordering (distance from sun)
- <input>Mars, Earth, Jupiter, Venus, Mercury</input>
<output>Mercury, Venus, Earth, Mars, Jupiter</output>
# reordering story sentences to make it comprehensible
- <input>I calmly nominate the boy up the street where I grew up who was kinda a Ralph Wiggum like dull guy. Eventually this segues into a dream about being in a rural village area and participating in a 'the lottery' like religious execution rite. The End. And so the cultists think I'm one of them until I figure out that the cult secrets are on the top closed-off floor of the mayoral hall building in the mayor's office. I start a fire as a ruse to get in and release the capsule of little insects which power the brainwashing of the cult so they will get lost or destroyed in the fire as they buzz around confusedly. I flee shouting to the cultists that the restaurant must have caught on fire and to get out before it's too late.</input>
<output>Eventually this segues into a dream about being in a rural village area and participating in a 'the lottery' like religious execution rite. I calmly nominate the boy up the street where I grew up who was kinda a Ralph Wiggum like dull guy. And so the cultists think I'm one of them until I figure out that the cult secrets are on the top closed-off floor of the mayoral hall building in the mayor's office. I start a fire as a ruse to get in and release the capsule of little insects which power the brainwashing of the cult so they will get lost or destroyed in the fire as they buzz around confusedly. I flee shouting to the cultists that the restaurant must have caught on fire and to get out before it's too late. The End.</output>
# alphabetizing:
- <input>
- chocolate
- banana
- vanilla</input>
<output>- banana
- chocolate
- vanilla</output>
# seriating paragraphs at multiple levels, within and across:
- <input>7 8 6
5 3 4
2 1 0</input>
<output>0 1 2
3 4 5
6 7 8</output>
# seriating a hierarchy:
- <input>Dog

Horse

Kitty</input>
<output>Kitty

Dog

Horse</output>
- <input>- Fruit

    - Orange
    - Kiwi
    - Apple

        - Honeycrisp
        - Red Delicious
        - Granny Smith
        - Golden Delicious
        - Jazz
        - SweeTango
- Cat
- Banana
- Tiger
- Car</input>
<output>- Fruit
    - Apple
        - Golden Delicious
        - Granny Smith
        - Honeycrisp
        - Jazz
        - Red Delicious
        - SweeTango
    - Kiwi
    - Orange
- Banana
- Cat
- Tiger
- Car</output>

Input list to seriate:

- <input>""" + target + "</input>\n<output>"

    completion = client.chat.completions.create(
        model="gpt-5-mini",
        timeout=60,
        messages=[
            {"role": "developer", "content": "You are an analyst and editor, reordering inputs logically."},
            {"role": "user", "content": prompt}
        ],
    )
    return completion.choices[0].message.content.removeprefix("<output>").removesuffix("</output>")

maximum_iterations = 5

if __name__ == "__main__":
    if len(sys.argv) == 1:
        target = sys.stdin.read().rstrip('\n')
    else:
        target = " ".join(sys.argv[1:])

    original = target
    current = target
    seen = {current}
    for i in range(maximum_iterations):
        result = seriate_once(current)
        print(f"Iteration {i+1}:\n{result}\n---", file=sys.stderr)
        if result == current:
            break
        if result in seen:
            print(f"ERROR: cycle detected at iteration {i+1}", file=sys.stderr)
            sys.exit(1)
        seen.add(result)
        current = result

    if not is_permutation(original, current):
        print(f"ERROR: not a permutation", file=sys.stderr)
        sys.exit(1)

    print(current)
