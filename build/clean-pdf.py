#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# clean-pdf.py: fix formatting & spelling errors in malformatted text (especially PDFs)
# Author: Gwern Branwen
# Date: 2020-07-03
# When:  Time-stamp: "2025-01-04 18:25:10 gwern"
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

import re
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
    {"role": "system", "content": "You are a helpful research assistant."},
      {"role": "user", "content":
f"""Task: fixing PDF copy-pasted text.

Summary: Fix formatting and spelling errors caused by PDFs or OCR.

Task description: Fix ONLY the PDF OCR errors. Otherwise, copy exactly and do not rewrite or change anything like pronouns.

In particular, fix hyphens at the end of lines which are not in the original words and are from the PDF hard-wrapping lines. Replace ligatures or control codes with the original letters, like 'ﬄ' → 'ffl'. Strip spurious footnotes, like in author lists.

Preview of input:
<text>{target}</text>

Examples:
<text>Numer-
ous approaches attempt to approximate the core attention layer to address its efﬁciency issues (Tay et al. 2022), such as</text> →
Numerous approaches attempt to approximate the core attention layer to address its efficiency issues (Tay et al. 2022), such as

<text>Collective memories are sustained by communities, which could be
as large as all of the speakers of a language or as small as a fam-
ily.</text> →
Collective memories are sustained by communities, which could be as large as all of the speakers of a language or as small as a family.

<text>MANY OB‐SERVERS OF CONTEM‐PORARY ECONOMIC TRENDS HAVE
been perplexed by the contemporary conjuncture of rapid
technological innovation with disappointingly slow gains in
measured productivity---such as [$10,000]($2023)/year.</text> →
Many observers of contemporary economic trends have been perplexed by the contemporary conjuncture of rapid technological innovation with disappointingly slow gains in measured productivity---such as [$10,000]($2023)/year.

<text>Whichbitshouldtravelfirst?Thebitfromthebigendorthebitfromthelittleend?·CanawarbetweenBigEndiansandLittleEndiansbeavoided?</text> →
Which bit should travel first? The bit from the big end or the bit from the little end? Can a war between Big Endians and Little Endians be avoided?

<text>thisarticlewaswritteninanattempttostopawar Ihopeitisnot toolate forpeace to prevailagain. manybelieve that the centralquestionofthiswaris,Whatistheproperbyteorderinmessages?Morespecifically,thequestionis,Whichbitshouldtravelfirst–thebitfromthelittleendofthewordorthebitfromthebigendoftheword?</text> →
This article was written in an attempt to stop a war. I hope it is not too late for peace to prevail again. Many believe that the central question of this war is, What is the proper byte order in messages? More specifically, the question is, Which bit should travel first---the bit from the little end of the word or the bit from the big end of the word?

<text><a href="!W">Productivity</a> is a simple concept. It is the amount of output produced per unit of input. While it is easy to define, it is notoriously diﬀicult to measure, especially in the modern econ-omy. In particular, there are two aspects of product— that haveincreasingly defied precise measurement: output and input.Properly measured, out-put should include not just the num-ber of widgets coming out of a</text> →
<a href="!W">Productivity</a> is a simple concept. It is the amount of output produced per unit of input. While it is easy to define, it is notoriously difficult to measure, especially in the modern economy. In particular, there are two aspects of productivity that have increasingly defied precise measurement: output and input. Properly measured, output should include not just the number of widgets coming out of a

<text>A central role has been attrib-
 uted to cognitive control processes---also referred to as <strong>executive
attention</strong>, <strong>attentional control</strong>, <strong>executive control</strong>,  <strong>inhibitory control</strong>,
or <strong>executive functions</strong>---that act as anumbrella term for self-
regulatory higher-
order cognitive processes contributing to goal-
directed behavior (Diamond, 2013)·</text> →
A central role has been attributed to cognitive control processes---also referred to as <strong>executive
attention</strong>, <strong>attentional control</strong>, <strong>executive control</strong>, <strong>inhibitory control</strong>, or <strong>executive functions</strong>---that act as an umbrella term for self-regulatory higher-order cognitive processes contributing to goal-directed behavior (Diamond, 2013).

<text>Sagar Gaikwad 1,2, Nicha Puangmalai 1,2, Minal Sonawane 1,2, Mauro Montalbano 1,2, Rachel Price 3, Malini S. Iyer 4, Anamika Ray 4, Sandra Moreno 3, Rakez Kayed</text> →
Sagar Gaikwad, Nicha Puangmalai, Minal Sonawane, Mauro Montalbano, Rachel Price, Malini S. Iyer, Anamika Ray, Sandra Moreno, Rakez Kayed

<text>Patricia J. Rodriguez, PhD1; Brianna M. Goodwin Cartwright, MS1; Samuel Gratzl, PhD1; Rajdeep Brar, MD1; Charlotte Baker, DrPH1; Ty J. Gluckman, MD2; Nicholas L. Stucky,</text> →
Patricia J. Rodriguez, Brianna M. Goodwin Cartwright, Samuel Gratzl, Rajdeep Brar, Charlotte Baker, Ty J. Gluckman, Nicholas L. Stucky
<text>Vasily Giannakeas, PhD, MPH1,2,3; David W. Lim, MDCM, MEd, PhD1,4,5,6; Steven A. Narod, MD1,3,6</text> →
Vasily Giannakeas, David W. Lim, Steven A. Narod

<text>Judgment accuracyDecision-makingCombining estimatesWisdom of crowdsGenetic diversityIndependence of opinionTwin studies</text> →
Judgment accuracy, Decision-making, Combining estimates, Wisdom of crowds, Genetic diversity, Independence of opinion, Twin studies

<text>Prof Philippe Autier MD, Prof Mathieu Boniol PhD, CÃ©cile Pizot MSc, Prof Patrick Mullie PhD</text> →
Philippe Autier, Mathieu Boniol, Cécile Pizot, Patrick Mullie

<text>MIND WANDERING DURING IMPLICIT LEARNING IS ASSOCIATED WITH INCREASED PERIODIC EEG ACTIVITY AND IMPROVED EXTRACTION OF HIDDEN PROBABILISTIC PATTERNS</text> →
Mind Wandering During Implicit Learning Is Associated With Increased Periodic EEG Activity And Improved Extraction Of Hidden Probabilistic Patterns

<text>Hockenhull Joanna, Wood David M., Dargan Paul I.</text> →
Joanna Hockenhull, David M. Wood, Paul I. Dargan

<text>Bjelakovic G, Gluud LL, Nikolova D, Whitfield K, Wetterslev J, Simonetti RG, Bjelakovic M, Gluud C.</text> →
G. Bjelakovic, L. L. Gluud, D. Nikolova, K. Whitfield, J. Wetterslev, R. G. Simonetti, M. Bjelakovic, C. Gluud

<text>Burkhart Kimberly Phelps James R.</text> →
Kimberly Burkhart, James R. Phelps

<text>Winson Fu Zun Yang
1,2
, Avijit Chowdhury1,2 , Marta Bianciardi2,3,4 , Remko van Lutterveld5,6 , Terje Sparby7,8,9 ,
Matthew D. Sacchet1,2, *</text> →
Winson Fu Zun Yang, Avijit Chowdhury, Marta Bianciardi, Remko van Lutterveld, Terje Sparby, Matthew D. Sacchet

<text>David A. Fishbain, MSc, MD, FAPA,*,†,‡,§ Robert Cutler, PhD,*,§ Hubert L. Rosomoff, MD, DMSc,†,‡,§ and Renee Steele Rosomoff</text> →
David A. Fishbain, Robert Cutler, Hubert L. Rosomoff, Renee Steele Rosomoff

<text>Patricia A Fleming 1,*, Heather M Crawford 1, Clare H Auckland 1, Michael C Calver 1</text> →
Patricia A. Fleming, Heather M. Crawford, Clare H. Auckland, Michael C. Calver

<text>Authors: Milton Packer, M.D. https://orcid.org/0000-0003-1828-2387, Michael R. Zile, M.D., Christopher M. Kramer, M.D., Seth J. Baum, M.D., Sheldon E. Litwin, M.D., Venu Menon, M.D., Junbo Ge, M.D., Govinda J. Weerakkody, Ph.D., Yang Ou, Ph.D., Mathijs C. Bunck, M.D., Karla C. Hurt, B.S.N., Masahiro Murakami, M.D., and Barry A. Borlaug</text> →
Milton Packer, Michael R. Zile, Christopher M. Kramer, Seth J. Baum, Sheldon E. Litwin, Venu Menon, Junbo Ge, Govinda J. Weerakkody, Yang Ou, Mathijs C. Bunck, Karla C. Hurt, Masahiro Murakami, Barry A. Borlaug

<text>Abrams, Gerald D., Bauer, Heinz, Sprinz, Helmunz</text> →
Gerald D. Abrams, Heinz Bauer, Helmuth Sprinz

<text>Helmut A. Gordon, M.D., Edith Bruckner-Kardoss, M.T., Bernard S. Wostmann</text> →
Helmut A. Gordon, Edith Bruckner-Kardoss, Bernard S. Wostmann

<text>Author links open overlay panelFlorence A.R. Oxley a b, Kirsty Wilding a, Sophie von Stumm a</text> →
Florence A. R. Oxley, Kirsty Wilding, Sophie von Stumm

<text>Pronoiais the delusionthat othersthinkwellof one. Actionsandthe productsof one'seffortsarethought
to be well receivedand praisedby otherswho, when they talk behindone's back, must be sayinggood
things,not bad. Mereacquaintancesareseenas close friends.Politenessand the exchangeof pleasantries
are interpretedas expressionsof deep attachmentand the promiseof futuresuppor</text> →
Pronoia is the delusion that others think well of one. Actions and the products of one's efforts are thought to be well received and praised by others who, when they talk behind one's back, must be saying good things, not bad. Mere acquaintances are seen as close friends. Politeness and the exchange of pleasantries are interpreted as expressions of deep attachment and the promise of future support.

<text>Author links open overlay panelAna Stanojevic a b, Stanisław Woźniak a, Guillaume Bellec b, Giovanni Cherubini a, Angeliki Pantazi a, Wulfram Gerstner b</text> →
Ana Stanojevic, Stanisław Woźniak, Guillaume Bellec, Giovanni Cherubini, Angeliki Pantazi, Wulfram Gerstner

<text>Author links open overlay panelMira Fischer a, Elisabeth Grewenig b, Philipp Lergetporer c, Katharina Werner d, Helen Zeidler e</text> →
Mira Fischer, Elisabeth Grewenig, Philipp Lergetporer, Katharina Werner, Helen Zeidler

Input:
<text>{target}</text> →
"""}
  ]
)

content = completion.choices[0].message.content

# Ensure no leakage from the LLM formatting: remove "<text>" and "</text>" tags:
cleaned_content = re.sub(r'</?text>', '', content)

print(cleaned_content.strip())
