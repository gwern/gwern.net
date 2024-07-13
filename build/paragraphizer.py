#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# paragraphizer.py: reformat a single paragraph into multiple paragraphs using OpenAI API neural nets
# Author: Gwern Branwen
# Date: 2022-02-18
# When:  Time-stamp: "2024-07-13 19:57:41 gwern"
# License: CC-0
#
# Usage: $ OPENAI_API_KEY="sk-XXX" echo [...] | python paragraphizer.py
#
# Paragraphizer attempts to reformat a single run-on paragraph into multiple shorter paragraphs,
# presumably split by topic. This is particularly useful for research paper abstracts, which are
# usually written in a sequential fashion (along the lines of 'Background / Question / Data /
# Methods / Results / Conclusion') but not always formatted in topic-separated paragraphs. A
# jargon-heavy run-on abstract can be near-impossible to skim.
#
# Paragraphizer does this by a call to the OA API; I have found that a simple 'rewrite this as'
# zero-shot prompt works well with davinci-instruct models (and is unreliable with smaller models or
# plain davinci). The main failure mode is that it will not copy the abstract exactly, and may
# reword or expand on parts, which is highly undesirable, and would mean that it cannot be used to
# automatically reformat abstracts. (And if you aren't going to use Paragraphizer automatically, why
# bother? It doesn't take long to add linebreaks by hand.) That failure mode can be removed by
# simply checking that after removing the new newlines, it equals the original input (ie. the *only*
# difference is the inserted newlines). The result can still be bad but it's probably at least
# better.
#
# Example:
#
# $ xclip -o
# Most deep reinforcement learning (RL) algorithms distill experience into parametric behavior
# policies or value functions via gradient updates. While effective, this approach has several
# disadvantages: (1) it is computationally expensive, (2) it can take many updates to integrate
# experiences into the parametric model, (3) experiences that are not fully integrated do not
# appropriately influence the agent's behavior, and (4) behavior is limited by the capacity of the
# model. In this paper we explore an alternative paradigm in which we train a network to map a
# dataset of past experiences to optimal behavior. Specifically, we augment an RL agent with a
# retrieval process (parameterized as a neural network) that has direct access to a dataset of
# experiences. This dataset can come from the agent's past experiences, expert demonstrations, or
# any other relevant source. The retrieval process is trained to retrieve information from the
# dataset that may be useful in the current context, to help the agent achieve its goal faster and
# more efficiently. We integrate our method into two different RL agents: an offline DQN agent and
# an online R2D2 agent. In offline multi-task problems, we show that the retrieval-augmented DQN
# agent avoids task interference and learns faster than the baseline DQN agent. On Atari, we show
# that retrieval-augmented R2D2 learns significantly faster than the baseline R2D2 agent and
# achieves higher scores. We run extensive ablations to measure the contributions of the components
# of our proposed method.
#
# $ OPENAI_API_KEY="sk-XYZ" xclip -o | python paragraphizer.py
# Most deep [reinforcement learning](https://en.wikipedia.org/wiki/Reinforcement_learning) (RL) algorithms distill experience into parametric behavior policies or value functions via gradient updates. While effective, this approach has several disadvantages: (1) it is computationally expensive, (2) it can take many updates to integrate experiences into the parametric model, (3) experiences that are not fully integrated do not appropriately influence the agent's behavior, and (4) behavior is limited by the capacity of the model.
#
# In this paper, we explore an alternative paradigm in which we train a network to map a dataset of past experiences to optimal behavior. Specifically, we augment an RL agent with a retrieval process (parameterized as a neural network) that has direct access to a dataset of experiences. This dataset can come from the agent's past experiences, expert demonstrations, or any other relevant source. The retrieval process is trained to retrieve information from the dataset that may be useful in the current context, to help the agent achieve its goal faster and more efficiently.
#
# We integrate our method into two different RL agents: an offline [DQN](https://en.wikipedia.org/wiki/Q-learning#Deep_Q-learning) agent and an online [R2D2](https://openreview.net/forum?id=r1lyTjAqYX) agent. In offline multi-task problems, we show that the retrieval-augmented DQN agent avoids task interference and learns faster than the baseline DQN agent. On [Atari](https://en.wikipedia.org/wiki/Atari), we show that retrieval-augmented R2D2 learns significantly faster than the baseline R2D2 agent and achieves higher scores.
#
# We run extensive ablations to measure the contributions of the components of our proposed method.

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
f"""Task: reformatting abstracts.

Summary: Add relevant HTML hyperlinks & formatting to text, and adds double-newlines to split abstracts into Markdown paragraphs (one topic per paragraph.)

Task description: Please process the following abstract (between the '<abstract>' and '</abstract>' tags), by adding double-newlines to split it into paragraphs (one topic per paragraph.) The order of topics should be: 1. background/introduction; 2. methods/data/approach; 3. results/benchmarks/outputs; 4. conclusion/discussion/implications; 5. supplementary information (eg. URLs, code, websites, datasets).

Additional formatting instructions: convert to American spelling & conventions. Do not add unnecessary italics; but italicize species names as appropriate. If a new term, concept, or system is introduced by this research paper, bold the first appearance using '<strong>NAME</strong>' formatting (and ONLY the first use), and bold only the most important new term. Please also add useful hyperlinks (such as Wikipedia articles) in HTML format to technical terminology or names (but do not hyperlink obvious familiar terms like "University" or "psychology"); do not duplicate links: include each link ONLY once; include only URLs you are sure of. Please include ONLY the resulting text with hyperlinks in your output, include ALL the original text, and include NO other conversation or comments.

Examples:

1. Input: <abstract><p>Previous theoretical results pertaining to meta-learning on sequences build on contrived assumptions and are somewhat convoluted. We introduce new information-theoretic tools that lead to an elegant and very general decomposition of error into 3 components: irreducible error, meta-learning error, and intra-task error. These tools unify analyses across many meta-learning challenges. To illustrate, we apply them to establish new results about in-context learning with transformers. Our theoretical results characterizes how error decays in both the number of training sequences and sequence lengths. Our results are very general; for example, they avoid contrived mixing time assumptions made by all prior results that establish decay of error with sequence length.</p></abstract> →
Previous theoretical results pertaining to meta-learning on sequences build on contrived assumptions and are somewhat convoluted.

We introduce new information-theoretic tools that lead to an elegant and very general decomposition of error into 3 components: irreducible error, meta-learning error, and intra-task error. These tools unify analyses across many meta-learning challenges.

To illustrate, we apply them to establish new results about in-context learning with transformers. Our theoretical results characterizes how error decays in both the number of training sequences and sequence lengths.

Our results are very general; for example, they avoid contrived mixing time assumptions made by all prior results that establish decay of error with sequence length.
2. Input: <abstract>Scalable oversight protocols aim to enable humans to accurately supervise superhuman AI. In this paper we study debate, where two AI's compete to convince a judge; consultancy, where a single AI tries to convince a judge that asks questions; and compare to a baseline of direct question-answering, where the judge just answers outright without the AI. We use large language models (LLMs) as both AI agents and as stand-ins for human judges, taking the judge models to be weaker than agent models. We benchmark on a diverse range of asymmetries between judges and agents, extending previous work on a single extractive QA task with information asymmetry, to also include mathematics, coding, logic and multimodal reasoning asymmetries. We find that debate outperforms consultancy across all tasks when the consultant is randomly assigned to argue for the correct/incorrect answer. Comparing debate to direct question answering, the results depend on the type of task: in extractive QA tasks with information asymmetry debate outperforms direct question answering, but in other tasks without information asymmetry the results are mixed. Previous work assigned debaters/consultants an answer to argue for. When we allow them to instead choose which answer to argue for, we find judges are less frequently convinced by the wrong answer in debate than in consultancy. Further, we find that stronger debater models increase judge accuracy, though more modestly than in previous studies.</abstract>
Scalable oversight protocols aim to enable humans to accurately supervise superhuman AI. In this paper we study debate, where two AI's compete to convince a judge; consultancy, where a single AI tries to convince a judge that asks questions; and compare to a baseline of direct question-answering, where the judge just answers outright without the AI. We use large language models (LLMs) as both AI agents and as stand-ins for human judges, taking the judge models to be weaker than agent models.

We benchmark on a diverse range of asymmetries between judges and agents, extending previous work on a single extractive QA task with information asymmetry, to also include mathematics, coding, logic and multimodal reasoning asymmetries.

We find that debate outperforms consultancy across all tasks when the consultant is randomly assigned to argue for the correct/incorrect answer.

Comparing debate to direct question answering, the results depend on the type of task: in extractive QA tasks with information asymmetry debate outperforms direct question answering, but in other tasks without information asymmetry the results are mixed.

Previous work assigned debaters/consultants an answer to argue for.

When we allow them to instead choose which answer to argue for, we find judges are less frequently convinced by the wrong answer in debate than in consultancy. Further, we find that stronger debater models increase judge accuracy, though more modestly than in previous studies.
3. Input: <abstract>If an individual entity endures a fixed probability μ &lt;1 of disappearing (“dying”) in a given fixed time period, then, as time approaches infinity, the probability of death approaches certainty. One approach to avoid this fate is for individuals to copy themselves into different locations; if the copies each have an independent probability of dying, then the total risk is much reduced. However, to avoid the same ultimate fate, the entity must continue copying itself to continually reduce the risk of death. In this paper, we show that to get a non-zero probability of ultimate survival, it suffices that the number of copies grows logarithmically with time. Accounting for expected copy casualties, the required rate of copying is hence bounded.</abstract> →
If an individual entity endures a fixed probability μ &lt;1 of disappearing (“dying”) in a given fixed time period, then, as time approaches infinity, the probability of death approaches certainty.
One approach to avoid this fate is for individuals to copy themselves into different locations; if the copies each have an independent probability of dying, then the total risk is much reduced. However, to avoid the same ultimate fate, the entity must continue copying itself to continually reduce the risk of death.
In this paper, we show that to get a non-zero probability of ultimate survival, it suffices that the number of copies grows logarithmically with time. Accounting for expected copy casualties, the required rate of copying is hence bounded.
4. Input: <abstract>We present a framework for translating unlabeled images from one domain into analog images in another domain. We employ a progressively growing skip-connected encoder-generator structure and train it with a <a href="https://en.wikipedia.org/wiki/Generative_adversarial_network">GAN</a> loss for realistic output, a cycle consistency loss for maintaining same-domain translation identity, and a semantic consistency loss that encourages the network to keep the input semantic features in the output. We apply our framework on the task of translating face images, and show that it is capable of learning semantic mappings for face images with no supervised one-to-one image mapping.</abstract> →
We present a framework for translating unlabeled images from one domain into analog images in another domain.
We employ a progressively growing skip-connected encoder-generator structure and train it with a <a href="https://en.wikipedia.org/wiki/Generative_adversarial_network">GAN</a> loss for realistic output, a cycle consistency loss for maintaining same-domain translation identity, and a semantic consistency loss that encourages the network to keep the input semantic features in the output.
We apply our framework on the task of translating face images, and show that it is capable of learning semantic mappings for face images with no supervised one-to-one image mapping.
5. Input: <abstract>We introduce a new resource: the SAYCam corpus. Infants aged 6–32 months wore a head-mounted camera for ~2 hours per week, over the course of ~two and a half years. The result is a large, naturalistic, longitudinal dataset of infant-perspective and child-perspective videos. Transcription efforts are underway, with over 200,000 words of naturalistic dialogue already transcribed. Similarly, the dataset is searchable using a number of criteria (eg. age of participant, location, setting, objects present). The resulting dataset will be of broad use to psychologists, linguists, and computer scientists.</abstract> →
We introduce a new resource: the SAYCam corpus.
Infants aged 6–32 months wore a head-mounted camera for ~2 hours per week, over the course of ~two and a half years.
The result is a large, naturalistic, longitudinal dataset of infant-perspective and child-perspective videos. Transcription efforts are underway, with over 200,000 words of naturalistic dialogue already transcribed. Similarly, the dataset is searchable using a number of criteria (eg. age of participant, location, setting, objects present).
The resulting dataset will be of broad use to psychologists, linguists, and computer scientists.
6. Input: <abstract>Subreddit devoted to discussion of <a href="https://en.wikipedia.org/wiki/Reinforcement_learning">reinforcement learning</a> research and projects, particularly deep reinforcement learning (more specialized than <code>/r/MachineLearning</code>). Major themes include deep learning, model-based vs model-free RL, robotics, multi-agent RL, exploration, meta-reinforcement learning, imitation learning, the psychology of RL in biological organisms such as humans, and safety/AI risk. Moderate activity level (as of 2019-09-11): ~10k subscribers, 2k pageviews/daily</abstract> →
Subreddit devoted to discussion of <a href="https://en.wikipedia.org/wiki/Reinforcement_learning">reinforcement learning</a> research and projects, particularly deep reinforcement learning (more specialized than <code>/r/MachineLearning</code>).
Major themes include deep learning, model-based vs model-free RL, robotics, multi-agent RL, exploration, meta-reinforcement learning, imitation learning, the psychology of RL in biological organisms such as humans, and safety/AI risk.
Moderate activity level (as of 2019-09-11): ~10k subscribers, 2k pageviews/daily
7. Input: <abstract>Large transformer models have shown extraordinary success in achieving state-of-the-art results in many natural language processing applications. However, training and deploying these models can be prohibitively costly for long sequences, as the standard self-attention mechanism of the <a href="https://arxiv.org/abs/1706.03762#google" title="‘Attention Is All You Need’, Vaswani et al 2017">Transformer</a> uses <em>𝒪(n<sup>2</sup>)</em> time and space with respect to sequence length. In this paper, we demonstrate that the self-attention mechanism can be approximated by a low-rank matrix. We further exploit this finding to propose a new self-attention mechanism, which reduces the overall self-attention complexity from <em>𝒪(n<sup>2</sup>)</em> to <em>𝒪(n)</em> in both time and space. The resulting linear transformer, the <strong>Linformer</strong>, performs on par with standard Transformer models, while being much more memory-efficient and time-efficient.</abstract> →
Large transformer models have shown extraordinary success in achieving state-of-the-art results in many natural language processing applications. However, training and deploying these models can be prohibitively costly for long sequences, as the standard self-attention mechanism of the <a href="https://arxiv.org/abs/1706.03762#google" title="‘Attention Is All You Need’, Vaswani et al 2017">Transformer</a> uses <em>𝒪(n<sup>2</sup>)</em> time and space with respect to sequence length.
In this paper, we demonstrate that the self-attention mechanism can be approximated by a low-rank matrix. We further exploit this finding to propose a new self-attention mechanism, which reduces the overall self-attention complexity from <em>𝒪(n<sup>2</sup>)</em> to <em>𝒪(n)</em> in both time and space.
The resulting linear transformer, the <strong>Linformer</strong>, performs on par with standard Transformer models, while being much more memory-efficient and time-efficient.
8. Input: <abstract>[Book review of an anthropologist text arguing for imitation and extensive cultural <a href="https://en.wikipedia.org/wiki/Group_selection">group selection</a> as the driving force of human civilization, with imitation of other humans being the unique human cognitive skill that gave us the edge over other primates and all animals, with any kind of raw intelligence being strictly minor. Further this extensive multi-level group selectionism implies that most knowledge is embodied in apparently-arbitrary cultural practices, such as traditional food preparation or divination or hunting rituals, which are effective despite lacking any observable rationale and the actual reasons for their efficacy are inaccessible to mere reason (except possibly by a far more advanced science).]</abstract> →
Book review of an anthropologist text arguing for imitation and extensive cultural <a href="https://en.wikipedia.org/wiki/Group_selection">group selection</a> as the driving force of human civilization.
Imitation of other humans is proposed as the unique human cognitive skill that gave us the edge over other primates and all animals, with any kind of raw intelligence being strictly minor.
Further, this extensive multi-level group selectionism implies that most knowledge is embodied in apparently-arbitrary cultural practices, such as traditional food preparation or divination or hunting rituals, which are effective despite lacking any observable rationale and the actual reasons for their efficacy are inaccessible to mere reason (except possibly by a far more advanced science).
9. Input: <abstract>Technologies to measure gaze direction and pupil reactivity have become efficient, cheap, and compact and are finding increasing use in many fields, including gaming, marketing, driver safety, military, and healthcare. Besides offering numerous useful applications, the rapidly expanding technology raises serious privacy concerns. Through the lens of advanced data analytics, gaze patterns can reveal much more information than a user wishes and expects to give away. Drawing from a broad range of scientific disciplines, this paper provides a structured overview of personal data that can be inferred from recorded eye activities. Our analysis of the literature shows that eye tracking data may implicitly contain information about a user’s biometric identity, gender, age, ethnicity, body weight, personality traits, drug consumption habits, emotional state, skills and abilities, fears, interests, and sexual preferences. Certain eye tracking measures may even reveal specific cognitive processes and can be used to diagnose various physical and mental health conditions. By portraying the richness and sensitivity of gaze data, this paper provides an important basis for consumer education, privacy impact assessments, and further research into the societal implications of eye tracking.</abstract> →
Technologies to measure gaze direction and pupil reactivity have become efficient, cheap, and compact and are finding increasing use in many fields, including gaming, marketing, driver safety, military, and healthcare. Besides offering numerous useful applications, the rapidly expanding technology raises serious privacy concerns. Through the lens of advanced data analytics, gaze patterns can reveal much more information than a user wishes and expects to give away.
Drawing from a broad range of scientific disciplines, this paper provides a structured overview of personal data that can be inferred from recorded eye activities. Our analysis of the literature shows that eye tracking data may implicitly contain information about a user’s biometric identity, gender, age, ethnicity, body weight, personality traits, drug consumption habits, emotional state, skills and abilities, fears, interests, and sexual preferences. Certain eye tracking measures may even reveal specific cognitive processes and can be used to diagnose various physical and mental health conditions.
By portraying the richness and sensitivity of gaze data, this paper provides an important basis for consumer education, privacy impact assessments, and further research into the societal implications of eye tracking.
10. Input: <abstract>There are at least three strategies we might take in approaching controversial issues: (1) we might accept the conclusions of experts on their authority, (2) we might evaluate the relevant evidence and arguments for ourselves, or (3) we might give up on finding the answers. Students of “critical thinking” are regularly advised to follow strategy (2). But strategies (1) and (3) are usually superior to (2), from the standpoint of the goal of gaining true beliefs and avoiding false ones.</abstract>
There are at least three strategies we might take in approaching controversial issues: (1) we might accept the conclusions of experts on their authority, (2) we might evaluate the relevant evidence and arguments for ourselves, or (3) we might give up on finding the answers.
Students of “critical thinking” are regularly advised to follow strategy (2).
But strategies (1) and (3) are usually superior to (2), from the standpoint of the goal of gaining true beliefs and avoiding false ones.

Input: <abstract>{target}</abstract>"""}
  ]
)

print(completion.choices[0].message.content)
