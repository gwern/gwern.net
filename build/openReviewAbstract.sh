#!/bin/bash

# openReviewAbstract.sh: scrape paper metadata from OpenReview
# Author: Gwern Branwen
# Date: 2021-10-12
# When:  Time-stamp: "2022-10-13 11:00:41 gwern"
# License: CC-0
#
# Shell script to scrape paper titles/date/author/abstract (TLDR if exists, then full
# abstract)/keywords from the OpenReview conference website.
#
# Examples:
#
# $ openReviewAbstract.sh 'https://openreview.net/forum?id=BkjLkSqxg'
# LipNet: End-to-End Sentence-level Lipreading
# Yannis M. Assael, Brendan Shillingford, Shimon Whiteson, Nando de Freitas
# 2016-12-16
# LipNet is the first end-to-end sentence-level lipreading model to simultaneously learn spatiotemporal visual features and a sequence model.
# Lipreading is the task of decoding text from the movement of a speaker's mouth. Traditional approaches separated the problem into two stages: designing or learning visual features, and prediction. More recent deep lipreading approaches are end-to-end trainable (Wand et al 2016; Chung & Zisserman 2016a). However, existing work on models trained end-to-end perform only word classification, rather than sentence-level sequence prediction. Studies have shown that human lipreading performance increases for longer words (Easton & Basala, 1982), indicating the importance of features capturing temporal context in an ambiguous communication channel. Motivated by this observation, we present LipNet, a model that maps a variable-length sequence of video frames to text, making use of spatiotemporal convolutions, a recurrent network, and the connectionist temporal classification loss, trained entirely end-to-end. To the best of our knowledge, LipNet is the first end-to-end sentence-level lipreading model that simultaneously learns spatiotemporal visual features and a sequence model. On the GRID corpus, LipNet achieves 95.2% accuracy in sentence-level, overlapped speaker split task, outperforming experienced human lipreaders and the previous 86.4% word-level state-of-the-art accuracy (Gergen et al 2016).
# Computer vision, Deep learning
# $ openReviewAbstract.sh 'https://openreview.net/forum?id=bwq6O4Cwdl'
# How Does SimSiam Avoid Collapse Without Negative Samples? Towards a Unified Understanding of Progress in SSL
# Anonymous
# 2021-10-06
#
# Towards avoiding collapse in self-supervised learning (SSL), contrastive loss is widely used but often requires a large number of negative samples. Without negative samples yet achieving competitive performance one recent work~\cite{chen2021exploring} has attracted significant attention for providing a minimalist simple Siamese (SimSiam) method to avoid collapse. However, the reason for its success remains not fully clear and our investigation starts by revisiting the explanatory claims in the SimSiam. After refuting their claims, we introduce vector decomposition for analyzing the collapse based on the gradient analysis of $l_2$ normalized vector. This yields a unified perspective on how negative samples and SimSiam predictor alleviate collapse and promote dimensional de-correlation. Such a unified perspective comes timely for understanding the recent progress in SSL.
# SimSiam, Negative samples, SSL, Collapse, Covariance
#
# Requires: curl, HTML Tidy, jq

# set -e
# set -x

JSON=$(curl --silent --location "$@" | \
    # normalizing with Tidy puts the JSON object on a single line, which we can then grep out without full-blown HTML parsing:
    tidy -quiet 2>/dev/null | grep -F "pageProps")
PARSED=""
if [[ $(echo "$JSON" | grep -F '"value":') ]]; then
   PARSED=$(echo "$JSON" |
    jq --raw-output '(.props.pageProps.forumNote.content.title.value),
       (.props.pageProps.forumNote.content.authors.value | join(", ")),
       (.props.pageProps.forumNote.tmdate / 1000 | strftime("%F")),
       .props.pageProps.forumNote.content."TL;DR",
       (.props.pageProps.forumNote.content.abstract.value | sub("\n"; " ")),
       (.props.pageProps.forumNote.content.keywords | join(", "))?' )
else
   PARSED=$(echo "$JSON" |
    jq --raw-output '(.props.pageProps.forumNote.content.title),
       (.props.pageProps.forumNote.content.authors | join(", ")),
       (.props.pageProps.forumNote.tmdate / 1000 | strftime("%F")),
       .props.pageProps.forumNote.content."TL;DR",
       (.props.pageProps.forumNote.content.abstract | sub("\n"; " ")),
       (.props.pageProps.forumNote.content.keywords | join(", "))?' )
fi
# cleanup:
# empty fields should be empty lines
 echo "$PARSED" | \
     sed -e 's/^null$//' | \
        # join hyphen-broken newlines
        sed -e ':again' -e '/[[:alpha:]]-$/ { N; s/-\n//; b again; }' | \
            # manually clean up some LaTeXisms that Pandoc ignores:
            sed -e 's/\\mbox{\([[:graph:]]*\)}/\\emph{\1}/g' -e 's/\\citep\?{\([[:graph:]]*\)}/\(\\texttt{\1}\)/g'
