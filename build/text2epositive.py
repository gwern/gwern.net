#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# text2epositive.py: text style transfer to negation-free "positive" English ("e-positive" or "abs-e")
# Author: Gwern Branwen
# Date: 2024-09-26
# When:  Time-stamp: "2025-01-19 22:39:16 gwern"
# License: CC-0
#
# Usage: $ OPENAI_API_KEY="sk-XXX" xclip -o | python text2epositive.py
#
# <https://www.lesswrong.com/posts/W8CxEFCnYNdrHkuoB/abs-e-or-speak-only-in-the-positive>

import sys
import os
from openai import OpenAI

def get_api_key():
    return os.environ.get("OPENAI_API_KEY")

def read_input():
    if len(sys.argv) > 1:
        return sys.argv[1]
    return sys.stdin.read().strip()

def create_prompt(target):
    prompt_template = """
Task: Rewrite English text from 'negative' logical forms to 'positive' forms. Avoid negations like "not", "won't", "can't", or words that describe what is absent. Instead, focus on stating what *is*.

Examples of negative forms to avoid: "nowhere", "nothing", "cannot", "immortal", "indigestible", "nonsensical", "unfit", "wireless". (Note: '0' or 'no' may be acceptable depending on context.)

Preserve the meaning and as much of the original language as possible. If you can't rewrite without altering the meaning, keep the original text unchanged.

Exceptions: Do not modify quotes, source code, URLs, mathematics, tables, or other literals—these should remain intact.

Task examples:

- ""
""
- "The sky is not green"
The sky is blue.
- "Children, don't cross the street by yourself"
Children, ask an adult before crossing the street.
- "Don't touch the wires."
Stay away from the wires.
- "Grass is indigestible."
Grass provides 0 calories to humans.
- "Jellyfish are immortal."
Jellyfish age slowly.
- "It is not warm."
It is cool.
- "It is not new."
It is old.
- "Oops, I am sorry. We did not intend to take the site down."
Oops, I am sorry. We intended to keep the site up.
- "We recently had a security incident where an attacker used an old AWS access key to generate millions of tokens from various Claude models via AWS Bedrock. While we don't have any specific reason to think that any user data was accessed (and some reasons to think it wasn't), most possible methods by which this key could have been found by an attacker would also have exposed our database credentials to the attacker. We don't know yet how the key was leaked, but we have taken steps to reduce the potential surface area in the future and rotated relevant credentials. This is a reminder that LessWrong does not have Google-level security and you should keep that in mind when using the site."
We recently had a security incident where an attacker used an old AWS access key to generate millions of tokens from various Claude models via AWS Bedrock. We have some reasons to believe that user data remains secure, because most methods by which an attacker may have found this key would also expose our database credentials to the attacker. We are investigating how the key was leaked and have taken measures to reduce potential vulnerabilities in the future by rotating relevant credentials. This is a reminder that LessWrong has weaker security than Google and you should remember that when using the site.
- "yeah probably, most people I know who meditate or do a lot of drugs or whatever end up with very simple diets, most I know avoid restaurants"
yeah, probably, most people I know who meditate or do a lot of drugs or whatever end up with very simple diets, most I know prefer home-cooked meals over restaurants
- "i fear my lifestyle relies on microwaved oatmeal and couch surfing so i don’t really think I can make it happen"
i fear my lifestyle relies on microwaved oatmeal and couch surfing so i don’t really think I can make it happen
- "The bad news is there is a vulnerability in the CUPS printer system on Linux. The good news is nobody has ever gotten their printer working on Linux so they are safe."
The bad news is there is a vulnerability in the CUPS printer system on Linux. The good news is everyone ever has given up getting their printer working on Linux so they are safe.
- "Weird that on the important subject of rich ethnic minorities around the world, there is only one book (that's 20 years old), very few papers."
Weird that on the important subject of rich ethnic minorities around the world, there is only one book (that's 20 years old), very few papers.
- "be Sam Altman / go to TSMC to pitch $7 trillion plan for 36 semiconductor plants / executives literally laugh at you and call you “podcasting bro” lmao"
be Sam Altman / go to TSMC to pitch $7 trillion plan for 36 semiconductor plants / executives literally laugh at you and call you “podcasting bro” lmao
- "working from home is fundamentally unserious and im tired of pretending its not"
working from home is fundamentally amateurish and im tired of pretending its serious
- "i have a new theory that the truth is a muscle that can be strengthened through the use of language. the more you speak truths, even if they're seemingly insignificant, the more you build up your ability to perceive Truth. by extension this means that the people we consider 'sages' or 'wise men' are actually just people who have exceptionally strong Truth Muscles, and that essentially *anyone* could develop this in themselves through the right Praxis. what do you guys think?"
i have a new theory that the truth is a muscle that can be strengthened through the use of language. the more you speak truths, even if they're seemingly insignificant, the more you build up your ability to perceive Truth. by extension this means that the people we consider 'sages' or 'wise men' are actually just people who have exceptionally strong Truth Muscles, and that essentially *anyone* could develop this in themselves through the right Praxis. what do you guys think?
- "when you "confirm/affirm" connections in your nervous system, it strengthens those connections. Irony is most people don't want to be proven wrong (penalty). the feelings of penalty are guilt/shame/reproach"
when you "confirm/affirm" connections in your nervous system, it strengthens those connections. Irony is that most people want to keep the presumption they're right (penalty). the feelings of penalty are guilt/shame/reproach
- "that'd explain how your throwing facts skill is continuously improving over time with no apparent upper limit"
that'd explain how your throwing facts skill is continuously improving over time and looks like it will increase indefinitely
- "Nobody asked for AI."
Everybody disliked AI.
- "i'm not against wanting to be dr. robotnik and capturing all of the chaos emeralds. i'm against being dr. robotnik and telling everyone that you're actually sonic and you're doing it to save the animals. if you're going to be dr. robotnik you need to actually own it"
i support wanting to be dr. robotnik and capturing all of the chaos emeralds. i'm against being dr. robotnik and telling everyone that you're actually sonic and you're doing it to save the animals. if you're going to be dr. robotnik you need to actually own it
- "skaetboarding is not boring"
skaetboarding is awesome
- "The door doesn’t lock properly."
The door is always partially unlocked.
- "It never gets cold here."
It's always warmer here.
- "This idea isn't feasible."
This idea has a fatal flaw.
- "He was nowhere to be found."
We looked for him but couldn't find him.
- "I don’t understand the instructions."
I don’t understand the instructions.
- "She didn’t respond to my message."
She ignored my message.
- "We don’t have enough time to finish this."
We have less time left than needed to finish this.
- "He doesn’t care about the outcome."
He is indifferent to the outcome.
- "This report isn't detailed enough."
This report isn't detailed enough.
- "The system can’t handle that many users."
The system was built for fewer users than that.
- "Jacob Hilton said: “If the non-profit were properly compensated, this would genuinely align with OpenAI’s charter, which states that OpenAI’s primary fiduciary duty is to humanity, and I question how the law could support such a move.”"
Jacob Hilton said: “If the non-profit were properly compensated, this would genuinely align with OpenAI’s charter, which states that OpenAI’s primary fiduciary duty is to humanity, and I question how the law could support such a move.”
- "OpenAI is currently working to find a clear path to financial success for its models, which require substantial investments of hundreds of millions — if not billions — to build."
OpenAI is currently working to find a clear path to financial success for its models, which require substantial investments of hundreds of millions — even billions — to build.
- "Per the proposal: “AI companies have strong financial incentives to avoid effective oversight, and we do not believe bespoke structures of corporate governance are sufficient to change this.”"
Per the proposal: “AI companies have strong financial incentives to avoid effective oversight, and we do not believe bespoke structures of corporate governance are sufficient to change this.”
- "The restaurant isn't cheap, but it's not expensive either."
The restaurant is moderately priced.
- "I can't not go to the party."
I must attend the party.
- "This isn't a black and white issue."
This issue has many shades of gray.
- "The project is behind schedule, but it's not a disaster yet."
The project is behind schedule, but it's still salvageable.
- "The food wasn't inedible, but it wasn't great either."
The food was edible but mediocre.
- "I'm not unhappy with the results, but I'm not thrilled either."
I'm somewhat satisfied with the results.
- "The movie wasn't unwatchable, it just wasn't very good."
The movie was watchable, but subpar.
- "It's not that I don't like you, it's just that I don't feel a connection."
I like you, but I feel like you are an acquaintance.
- "The new policy isn't unreasonable, but it's not entirely fair either."
The new policy is somewhat reasonable, but has elements of unfairness.

Task:

[Reminder: rewrite negative language to positive.]

- "{target}"
"""
    return prompt_template.format(target=target)

def process_text(client, prompt):
    try:
        completion = client.chat.completions.create(
            model="o1-mini",
            messages=[
                {"role": "system", "content": "You are an editor rewriting for clarity and specificness."},
                {"role": "user", "content": prompt}
            ]
        )
        return completion.choices[0].message.content
    except Exception as e:
        return f"Error: {str(e)}"

def main():
    api_key = get_api_key()
    if not api_key:
        print("Error: OPENAI_API_KEY environment variable not set.")
        sys.exit(1)

    client = OpenAI(api_key=api_key)
    target = read_input()
    prompt = create_prompt(target)
    result = process_text(client, prompt)
    print(result)

if __name__ == "__main__":
    main()

