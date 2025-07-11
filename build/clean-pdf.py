#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# clean-pdf.py: fix formatting & spelling errors in malformatted text (especially PDFs)
# Author: Gwern Branwen
# Date: 2020-07-03
# When:  Time-stamp: "2025-06-29 20:00:24 gwern"
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
  model="gpt-4.1-mini",
  messages=[
    {"role": "system", "content": "You are a helpful research assistant."},
      {"role": "user", "content":
f"""Task: fixing PDF copy-pasted text.

Summary: Fix formatting and spelling errors caused by PDFs or OCR.

Task description: Fix ONLY the PDF OCR errors. Otherwise, copy exactly and do not rewrite or change anything like pronouns.

In particular, fix hyphens at the end of lines which are not in the original words and are from the PDF hard-wrapping lines. Replace ligatures or control codes with the original letters, like 'ﬄ' 'ffl'. Strip spurious footnotes or author credentials or injected text from GUIs, like in author lists.

Examples:
<text>Numer-
ous approaches attempt to approximate the core attention layer to address its efﬁciency issues (Tay et al. 2022), such as</text>
Numerous approaches attempt to approximate the core attention layer to address its efficiency issues (Tay et al. 2022), such as

<text>Collective memories are sustained by communities, which could be
as large as all of the speakers of a language or as small as a fam-
ily.</text>
Collective memories are sustained by communities, which could be as large as all of the speakers of a language or as small as a family.

<text>MANY OB‐SERVERS OF CONTEM‐PORARY ECONOMIC TRENDS HAVE
been perplexed by the contemporary conjuncture of rapid
technological innovation with disappointingly slow gains in
measured productivity---such as [$10,000]($2023)/year.</text>
Many observers of contemporary economic trends have been perplexed by the contemporary conjuncture of rapid technological innovation with disappointingly slow gains in measured productivity---such as [$10,000]($2023)/year.

<text>Whichbitshouldtravelfirst?Thebitfromthebigendorthebitfromthelittleend?·CanawarbetweenBigEndiansandLittleEndiansbeavoided?</text>
Which bit should travel first? The bit from the big end or the bit from the little end? Can a war between Big Endians and Little Endians be avoided?

<text>thisarticlewaswritteninanattempttostopawar Ihopeitisnot toolate forpeace to prevailagain. manybelieve that the centralquestionofthiswaris,Whatistheproperbyteorderinmessages?Morespecifically,thequestionis,Whichbitshouldtravelfirst–thebitfromthelittleendofthewordorthebitfromthebigendoftheword?</text>
This article was written in an attempt to stop a war. I hope it is not too late for peace to prevail again. Many believe that the central question of this war is, What is the proper byte order in messages? More specifically, the question is, Which bit should travel first---the bit from the little end of the word or the bit from the big end of the word?

<text><a href="!W">Productivity</a> is a simple concept. It is the amount of output produced per unit of input. While it is easy to define, it is notoriously diﬀicult to measure, especially in the modern econ-omy. In particular, there are two aspects of product— that haveincreasingly defied precise measurement: output and input.Properly measured, out-put should include not just the num-ber of widgets coming out of a</text>
<a href="!W">Productivity</a> is a simple concept. It is the amount of output produced per unit of input. While it is easy to define, it is notoriously difficult to measure, especially in the modern economy. In particular, there are two aspects of productivity that have increasingly defied precise measurement: output and input. Properly measured, output should include not just the number of widgets coming out of a

<text>A central role has been attrib-
 uted to cognitive control processes---also referred to as <strong>executive
attention</strong>, <strong>attentional control</strong>, <strong>executive control</strong>,  <strong>inhibitory control</strong>,
or <strong>executive functions</strong>---that act as anumbrella term for self-
regulatory higher-
order cognitive processes contributing to goal-
directed behavior (Diamond, 2013)·</text>
A central role has been attributed to cognitive control processes---also referred to as <strong>executive
attention</strong>, <strong>attentional control</strong>, <strong>executive control</strong>, <strong>inhibitory control</strong>, or <strong>executive functions</strong>---that act as an umbrella term for self-regulatory higher-order cognitive processes contributing to goal-directed behavior (Diamond, 2013).

<text>Sagar Gaikwad 1,2, Nicha Puangmalai 1,2, Minal Sonawane 1,2, Mauro Montalbano 1,2, Rachel Price 3, Malini S. Iyer 4, Anamika Ray 4, Sandra Moreno 3, Rakez Kayed</text>
Sagar Gaikwad, Nicha Puangmalai, Minal Sonawane, Mauro Montalbano, Rachel Price, Malini S. Iyer, Anamika Ray, Sandra Moreno, Rakez Kayed

<text>Patricia J. Rodriguez, PhD1; Brianna M. Goodwin Cartwright, MS1; Samuel Gratzl, PhD1; Rajdeep Brar, MD1; Charlotte Baker, DrPH1; Ty J. Gluckman, MD2; Nicholas L. Stucky,</text>
Patricia J. Rodriguez, Brianna M. Goodwin Cartwright, Samuel Gratzl, Rajdeep Brar, Charlotte Baker, Ty J. Gluckman, Nicholas L. Stucky
<text>Vasily Giannakeas, PhD, MPH1,2,3; David W. Lim, MDCM, MEd, PhD1,4,5,6; Steven A. Narod, MD1,3,6</text>
Vasily Giannakeas, David W. Lim, Steven A. Narod

<text>Judgment accuracyDecision-makingCombining estimatesWisdom of crowdsGenetic diversityIndependence of opinionTwin studies</text>
Judgment accuracy, Decision-making, Combining estimates, Wisdom of crowds, Genetic diversity, Independence of opinion, Twin studies

<text>Prof Philippe Autier MD, Prof Mathieu Boniol PhD, CÃ©cile Pizot MSc, Prof Patrick Mullie PhD</text>
Philippe Autier, Mathieu Boniol, Cécile Pizot, Patrick Mullie

<text>MIND WANDERING DURING IMPLICIT LEARNING IS ASSOCIATED WITH INCREASED PERIODIC EEG ACTIVITY AND IMPROVED EXTRACTION OF HIDDEN PROBABILISTIC PATTERNS</text>
Mind Wandering During Implicit Learning Is Associated With Increased Periodic EEG Activity And Improved Extraction Of Hidden Probabilistic Patterns

<text>Hockenhull Joanna, Wood David M., Dargan Paul I.</text>
Joanna Hockenhull, David M. Wood, Paul I. Dargan

<text>Bjelakovic G, Gluud LL, Nikolova D, Whitfield K, Wetterslev J, Simonetti RG, Bjelakovic M, Gluud C.</text>
G. Bjelakovic, L. L. Gluud, D. Nikolova, K. Whitfield, J. Wetterslev, R. G. Simonetti, M. Bjelakovic, C. Gluud

<text>Burkhart Kimberly Phelps James R.</text>
Kimberly Burkhart, James R. Phelps

<text>Winson Fu Zun Yang
1,2
, Avijit Chowdhury1,2 , Marta Bianciardi2,3,4 , Remko van Lutterveld5,6 , Terje Sparby7,8,9 ,
Matthew D. Sacchet1,2, *</text>
Winson Fu Zun Yang, Avijit Chowdhury, Marta Bianciardi, Remko van Lutterveld, Terje Sparby, Matthew D. Sacchet

<text>David A. Fishbain, MSc, MD, FAPA,*,†,‡,§ Robert Cutler, PhD,*,§ Hubert L. Rosomoff, MD, DMSc,†,‡,§ and Renee Steele Rosomoff</text>
David A. Fishbain, Robert Cutler, Hubert L. Rosomoff, Renee Steele Rosomoff

<text>Patricia A Fleming 1,*, Heather M Crawford 1, Clare H Auckland 1, Michael C Calver 1</text>
Patricia A. Fleming, Heather M. Crawford, Clare H. Auckland, Michael C. Calver

<text>Authors: Milton Packer, M.D. https://orcid.org/0000-0003-1828-2387, Michael R. Zile, M.D., Christopher M. Kramer, M.D., Seth J. Baum, M.D., Sheldon E. Litwin, M.D., Venu Menon, M.D., Junbo Ge, M.D., Govinda J. Weerakkody, Ph.D., Yang Ou, Ph.D., Mathijs C. Bunck, M.D., Karla C. Hurt, B.S.N., Masahiro Murakami, M.D., and Barry A. Borlaug</text>
Milton Packer, Michael R. Zile, Christopher M. Kramer, Seth J. Baum, Sheldon E. Litwin, Venu Menon, Junbo Ge, Govinda J. Weerakkody, Yang Ou, Mathijs C. Bunck, Karla C. Hurt, Masahiro Murakami, Barry A. Borlaug

<text>Abrams, Gerald D., Bauer, Heinz, Sprinz, Helmunz</text>
Gerald D. Abrams, Heinz Bauer, Helmuth Sprinz

<text>Helmut A. Gordon, M.D., Edith Bruckner-Kardoss, M.T., Bernard S. Wostmann</text>
Helmut A. Gordon, Edith Bruckner-Kardoss, Bernard S. Wostmann

<text>Author links open overlay panelFlorence A.R. Oxley a b, Kirsty Wilding a, Sophie von Stumm a</text>
Florence A. R. Oxley, Kirsty Wilding, Sophie von Stumm

<text>Pronoiais the delusionthat othersthinkwellof one. Actionsandthe productsof one'seffortsarethought
to be well receivedand praisedby otherswho, when they talk behindone's back, must be sayinggood
things,not bad. Mereacquaintancesareseenas close friends.Politenessand the exchangeof pleasantries
are interpretedas expressionsof deep attachmentand the promiseof futuresuppor</text>
Pronoia is the delusion that others think well of one. Actions and the products of one's efforts are thought to be well received and praised by others who, when they talk behind one's back, must be saying good things, not bad. Mere acquaintances are seen as close friends. Politeness and the exchange of pleasantries are interpreted as expressions of deep attachment and the promise of future support.

<text>Author links open overlay panelAna Stanojevic a b, Stanisław Woźniak a, Guillaume Bellec b, Giovanni Cherubini a, Angeliki Pantazi a, Wulfram Gerstner b</text>
Ana Stanojevic, Stanisław Woźniak, Guillaume Bellec, Giovanni Cherubini, Angeliki Pantazi, Wulfram Gerstner

<text>Author links open overlay panelMira Fischer a, Elisabeth Grewenig b, Philipp Lergetporer c, Katharina Werner d, Helen Zeidler e</text>
Mira Fischer, Elisabeth Grewenig, Philipp Lergetporer, Katharina Werner, Helen Zeidler

<text>Andrea Di Francesco 1,✉, Andrew G Deighan 2, Lev Litichevskiy 3,4, Zhenghao Chen 1, Alison Luciano 2, Laura Robinson 2, Gaven Garland 2, Hannah Donato 2, Matthew Vincent 2, Will Schott 2, Kevin M Wright 1,6, Anil Raj 1, G V Prateek 1, Martin Mullis 1, Warren G Hill 5, Mark L Zeidel 5, Luanne L Peters 2, Fiona Harding 1, David Botstein 1, Ron Korstanje 2, Christoph A Thaiss 3, Adam Freund 1,7, Gary A Churchill 2,✉</text>
Andrea Di Francesco, Andrew G. Deighan, Lev Litichevskiy, Zhenghao Chen, Alison Luciano, Laura Robinson, Gaven Garland, Hannah Donato, Matthew Vincent, Will Schott, Kevin M. Wright, Anil Raj, G. V. Prateek, Martin Mullis, Warren G. Hill, Mark L. Zeidel, Luanne L. Peters, Fiona Harding, David Botstein, Ron Korstanje, Christoph A. Thaiss, Adam Freund, Gary A. Churchill
- <text>Rachel G. Higier, Ph.D., Amy M. Jimenez, Ph.D., Christina M. Hultman, Ph.D., Jacqueline Borg, Ph.D., Cristina Roman, B.A., Isabelle Kizling, M.Sc., Henrik Larsson, Ph.D., and Tyrone D. Cannon, Ph.D.Authors Info & Affiliations</text>
Rachel G. Higier, Amy M. Jimenez, Christina M. Hultman, Jacqueline Borg, Cristina Roman, Isabelle Kizling, Henrik Larsson, Tyrone D. Cannon
- <text>Author links open overlay panelVanessa Y. Oviedo a, Andrew J. Guydish b, Jean E. Fox Tree</text>
Vanessa Y. Oviedo, Andrew J. Guydish, Jean E. Fox Tree
- <text>Amir Mani https://orcid.org/0000-0003-4417-622X, Cory Henn https://orcid.org/0000-0002-3585-4597, Claire Couch https://orcid.org/0000-0003-4983-3719, Sonal Patel https://orcid.org/0000-0003-4107-0497, Thora Lieke https://orcid.org/0000-0002-4345-1712, Justin T.H. Chan https://orcid.org/0000-0003-4271-0177, Tomas Korytar https://orcid.org/0000-0002-6913-0415, and Irene Salinas</text>
Amir Mani, Cory Henn, Claire Couch, Sonal Patel, Thora Lieke, Justin T. H. Chan, Tomas Korytar, Irene Salinas
- <text>Dana E. King
, MD , Arch G. Mainous III
, PhD , Mark E. Geesey
, MS & Robert F. Woolson
, PhD</text>
Dana E. King, Arch G. Mainous III, Mark E. Geesey, Robert F. Woolson
- <text>he hasn't seen my grandpa eat</text>
He hasn't seen my grandpa eat.
- <text>Meyer, Katie A.*; Williams, Paige†; Hernandez-Diaz, Sonia‡; Cnattingius, Sven§</text>
Katie A. Meyer, Paige Williams, Sonia Hernandez-Diaz, Sven Cnattingius
- <text>Lambe, Mats*; Hultman, Christina*; Torrång, Anna*; MacCabe, James†; Cnattingius, Sven*</text>
Mats Lambe, Christina Hultman, Anna Torrång, James MacCabe, Sven Cnattingius
- <text>Vance, Todd PhD*; Maes, Hermine H. PhD†‡; Kendler, Kenneth S. MD†‡</text>
Todd Vance, Hermine H. Maes, Kenneth S. Kendler
- <text>REGRESSION FALACIES IN THE MATCHED GROUPS EXPERIMENT</text>
Regression Fallacies in the Matched Groups Experiment
- <text>R E G R E S S I O N F A L L A C I E S IN T H E MATCHED
GROUPS E X P E R I M E N T</text>
Regression Fallacies in the Matched Groups Experiment
- <text>Authors: Darren K. McGuire, M.D., M.H.Sc., Nikolaus Marx, M.D., Sharon L. Mulvagh, M.D., John E. Deanfield, M.D., Silvio E. Inzucchi, M.D., Rodica Pop-Busui, M.D., Ph.D., Johannes F.E. Mann, M.D., Scott S. Emerson, M.D., Ph.D., Neil R. Poulter, F.Med.Sci., Mads D.M. Engelmann, M.D., Ph.D., Maria Sejersten Ripa, M.D., M.D.Sc., G. Kees Hovingh, M.D., Ph.D., Kirstine Brown-Frandsen, M.D., Stephen C. Bain, M.D., Matthew A. Cavender, M.D., M.P.H. https://orcid.org/0000-0002-4466-4760, Mette Gislum, M.Sc., Jens-Peter David, Ph.D., and John B. Buse, M.D., Ph.D* -11Author Info & Affiliations</text>
Darren K. McGuire, Nikolaus Marx, Sharon L. Mulvagh, John E. Deanfield, Silvio E. Inzucchi, Rodica Pop-Busui, Johannes F. E. Mann, Scott S. Emerson, Neil R. Poulter, Mads D. M. Engelmann, Maria Sejersten Ripa, G. Kees Hovingh, Kirstine Brown-Frandsen, Stephen C. Bain, Matthew A. Cavender, Mette Gislum, Jens-Peter David, John B. Buse
- <text>Children do not fear the atomic bomb<sup>[1](/doc/psychology/1964-escalona.pdf), 11</sup>. They do not even fear the
things they have been taught to be careful about: street traffic and germs.
The strange truth is that they fear an unrealistic source of danger in or
urban civilization: wild animals.</text>
Children do not fear the atomic bomb<sup>[1](/doc/psychology/1964-escalona.pdf), 11</sup>. They do not even fear the things they have been taught to be careful about: street traffic and germs. The strange truth is that they fear an unrealistic source of danger in or urban civilization: wild animals.
- <text>In all other tabula-
tions and discussions, the subjects are the 112 students whose IQs fell be-
tween 80 --144.</text>
In all other tabulations and discussions, the subjects are the 112 students whose IQs fell between 80--144.
- <text>and animated skeletons, are left behind after age 10. T h u s the questions about
the effect of television dramas highlighting horror becomes a matter of age.</text>
and animated skeletons, are left behind after age 10. Thus the questions about the effect of television dramas highlighting horror becomes a matter of age.
- <text>All four of the major theories of childhood (psychoanalysis, behaviorism,
the collective unconscious, and maturation) contribute, albeit incompletely,
to an understanding of childhood fears.
A strong maturational factor, partly influenced by intelligence and partly
influenced by the amount of responsibility thrust upon the child, seems to be
a t work upon an archaic instinctual base. T h e child is born with the capa-
city to fear, apparently more than is necessary to preserve his life.</text>
All four of the major theories of childhood (psychoanalysis, behaviorism, the collective unconscious, and maturation) contribute, albeit incompletely, to an understanding of childhood fears.

A strong maturational factor, partly influenced by intelligence and partly influenced by the amount of responsibility thrust upon the child, seems to be at work upon an archaic instinctual base. The child is born with the capacity to fear, apparently more than is necessary to preserve his life.
- <text>As the child matures, the emotion of fear fastens upon more and more
realistic objects depending upon experience learning rather than upon instruc-
tion.
T h e intensity of the child’s fear depends for the most part upon the family
relationships.</text>
As the child matures, the emotion of fear fastens upon more and more realistic objects depending upon experience learning rather than upon instruction.

The intensity of the child’s fear depends for the most part upon the family relationships.
- <text>Brianna I Flynn 1,✉,#, Emily M Javan 1,#, Eugenia Lin 2, Zoe Trutner 2, Karl Koenig 2, Kenoma O Anighoro 2, Eucharist Kun 1, Alaukik Gupta 1,3, Tarjinder Singh 4,5,6, Prakash Jayakumar 2,✉, Vagheesh M Narasimhan 1,7,✉</text>
Brianna I. Flynn, Emily M. Javan, Eugenia Lin, Zoe Trutner, Karl Koenig, Kenoma O. Anighoro, Eucharist Kun, Alaukik Gupta, Tarjinder Singh, Prakash Jayakumar, Vagheesh M. Narasimhan

[End of examples. Reminder: your job is to clean PDF copy-pastes of cruft and garbage.]

Input:
<text>{target}</text>
"""}
  ]
)

content = completion.choices[0].message.content

# Ensure no leakage from the LLM formatting: remove "<text>" and "</text>" tags, and any whitespace padding:
cleaned_content = re.sub(r'</?text>', '', content.rstrip())

print(cleaned_content.strip(), end='') # avoid trailing newline because we might be cleaning inline text & want to avoid injecting newlines
