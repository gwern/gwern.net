#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# clean-pdf.py: fix formatting & spelling errors in malformatted text (especially PDFs)
# Author: Gwern Branwen
# Date: 2020-07-03
# When:  Time-stamp: "2024-07-08 18:00:00 gwern"
# License: CC-0
#
# Usage: $ OPENAI_API_KEY="sk-XYZ" xclip -o | python clean-pdf.py
# Background: <https://gwern.net/gpt-3-nonfiction#pdf-cleaning>
#
# Examples:
#
# $ xclip -o
# Most intrigu-
# ingly, the effect of structural connectivity on fluid intelligence
# seems to be largely mediated by individual differences in process-
# ing speed and working memory (Ferrer et al., 2013; Fuhrmann et
# al., 2020; Kievit et al., 2016).
# $ OPENAI_API_KEY="sk-XYZ" xclip -o | python clean-pdf.py
# Most intriguingly, the effect of structural connectivity on fluid intelligence seems to be largely mediated by individual differences in processing speed and working memory (Ferrer et al., 2013; Fuhrmann et al., 2020; Kievit et al., 2016).

import sys
from openai import OpenAI
client = OpenAI()

if len(sys.argv) == 1:
    target = sys.stdin.read().strip()
else:
    target = sys.argv[1]

completion = client.chat.completions.create(
  model="gpt-4o",
  messages=[
    {"role": "system", "content": "You are a helpful research assistant."},
      {"role": "user", "content":
f"""Task: fixing PDF copy-pasted text.

Summary: Fix formatting and spelling errors caused by PDFs or OCR.

Task description: Fix ONLY the PDF OCR errors. Otherwise, copy exactly and do not rewrite or change anything like pronouns.

In particular, fix hyphens at the end of lines which are not in the original words and are from the PDF hard-wrapping lines. Replace ligatures or control codes with the original letters, like 'ﬄ' → 'ffl'.

Examples:
text>Numer-
ous approaches attempt to approximate the core attention layer to address its efﬁciency issues (Tay et al. 2022), such as</text>
→
Numerous approaches attempt to approximate the core attention layer to address its efficiency issues (Tay et al. 2022), such as

<text>Collective memories are sustained by communities, which could be
as large as all of the speakers of a language or as small as a fam-
ily.</text> →
Collective memories are sustained by communities, which could be as large as all of the speakers of a language or as small as a family.

<text>MANY OBSERVERS OF CONTEMPORARY ECONOMIC TRENDS HAVE
been perplexed by the contemporary conjuncture of rapid
technological innovation with disappointingly slow gains in
measured productivity.</text> →
Many observers of contemporary economic trends have been perplexed by the contemporary conjuncture of rapid technological innovation with disappointingly slow gains in measured productivity.

<text>Whichbitshouldtravelfirst?Thebitfromthebigendorthebitfromthelittleend?·CanawarbetweenBigEndiansandLittleEndiansbeavoided?</text> →
Which bit should travel first? The bit from the big end or the bit from the little end? Can a war between Big Endians and Little Endians be avoided?

<text>Thisarticlewaswritteninanattempttostopawar Ihopeitisnot toolate forpeace to prevailagain. Manybelieve that the centralquestionofthiswaris,Whatistheproperbyteorderinmessages?Morespecifically,thequestionis,Whichbitshouldtravelfirst-thebitfromthelittleendofthewordorthebitfromthebigendoftheword?</text> →
This article was written in an attempt to stop a war. I hope it is not too late for peace to prevail again. Many believe that the central question of this war is, What is the proper byte order in messages? More specifically, the question is, Which bit should travel first---the bit from the little end of the word or the bit from the big end of the word?

<text>Productivity is a simple concept. It is the amount of output produced per unit of input. While it is easy to define, it is notoriously diﬀicult to measure, especially in the modern econ-omy. In particular, there are two aspects of product— that haveincreasingly defied precise measurement: output and input.Properly measured, out-put should include not just the num-ber of widgets coming out of a</text> →
Productivity is a simple concept. It is the amount of output produced per unit of input. While it is easy to define, it is notoriously difficult to measure, especially in the modern economy. In particular, there are two aspects of productivity that have increasingly defied precise measurement: output and input. Properly measured, output should include not just the number of widgets coming out of a

<text>A central role has been attrib-
 uted to cognitive control processes---also referred to as executive
attention, attentional control, executive control,  inhibitory control,
or executive functions---that act as anumbrella term for self-
regulatory higher-order cognitive processes contributing to goal-
directed behavior (Diamond, 2013)·</text> →
A central role has been attributed to cognitive control processes---also referred to as executive attention, attentional control, executive control, inhibitory control, or executive functions---that act as an umbrella term for self-regulatory higher-order cognitive processes contributing to goal-directed behavior (Diamond, 2013).

Input:
<text>{target}</text> →
"""}
  ]
)

print(completion.choices[0].message.content)
