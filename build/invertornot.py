#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# invertornot.py: experimental GPT-4-V script for zero-shot classification of images for website dark-mode. Obsoleted by <https://InvertOrNot.com>.
# Author: Gwern Branwen
# Date: 2023-11-01
# When:  Time-stamp: "2024-03-26 10:57:05 gwern"
# License: CC-0
#
# Usage: $ invertornot.py foo.jpg
# NO
#
# Background: <https://gwern.net/idea#invertornot>. For website dark modes, the hardest part is how to treat images, which are often big bright white images that defeat the point of a ‘dark’ mode: should they be dimmed/faded out, or negated/inverted?
# Unfortunately, there is no reliable rule for how to do so; all heuristics like ‘count # of colors’ will fail. (A grayscale image with few colors may invert well—unless it’s a grayscale photograph of a human, in which case inversion backfires and the photo looks *awful*!)
# We can try to do it by hand, but this does not scale and for Gwern.net, this fails to handle *dynamic* sources of images, like Wikipedia popups. (Even if we could classify by hand 20,000+ WP thumbnails, WP articles are constantly being added or the articles themselves change.)
# It is a paradigmatic ‘I know it when I see it’ visual classification task, suitable for ML, and DL in particular.
# However, it is unsolved, and it seems all web designers give up and settle for either classifying every image by hand, or taking the safe way out and simply dimming *all* images rather than risk negation/inversion backfiring on them.
#
# Here we demonstrate the use of the OA API for GPT-4-V (released in early 2023 and opened up later in the year).
# GPT-4-V can be given arbitrary natural language prompts, like "should this image be inverted?"
# Unfortunately, in my preliminary tinkering, GPT-4-V did not seem to understand this well zero-shot—probably because it is quite rare online to have images with captions or associated text which would discuss whether that image should be inverted.
# There is probably a text prompt which *would* work—“sampling can show the presence of knowledge but not the absence”—but I didn’t find it.
# However, when a LLM fails zero-shot on a transformation description, it often works to provide the transformed version as well so it can ‘see’ the new version, and *then* ask for a comparison of which is better.
# GPT-4-V can accept multiple images (at greater cost, of course), and so we can simply take the target image, invert it, and provide the pair to GPT-4-V and ask it which one is better.
# This seems to work reasonably well.
#
# Downside: cost. This costs >1,000 tokens, and so would cost ~2¢ per image. This is acceptable on Gwern.net (at a one-time cost of ~$70), but would be painful to do for all possible images in WP popups (doing only the thumbnail for the ~22k first-level WP links would cost ~$440!).
# Further, there is no way to feed back in corrections in a cheap scalable fashion. (One could include them as few-shot examples, but each additional example would cost ~¢1 per future image…)
#
# Due to that cost, I hesitated to try this approach out more seriously, waiting for cost-decreases. (Historically, the OA API prices have dropped orders of magnitude every year.)
# This wait was obviated when Mattis Megevand contacted me about his prototype InvertOrNot.com API using a small finetuned EfficientNet which could be run almost for free on CPU, with what looked like higher accuracy, which could be further finetuned with errors to improve its accuracy & guarantee an instance never repeated, and which could be offered as a public service to benefit users beyond Gwern.net.
# So, we use that now. The GPT-4-V code is provided as a prototype/reference, as it may be useful for other similar tasks.
# (eg. deciding whether images should have outlines?)

import base64
import requests

from PIL import Image, ImageOps
import io
import os

api_key = os.getenv("OPENAI_API_KEY")

# Function to encode the image into a base-64 blob usable with HTTP APIs
def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')
# make an *inverted* or *negated* image for a dark-mode (comparable to ImageMagick `convert -negate`) version of `base64_image`, named `base64_image_inverted`
def invert_image(image_path):
    image = Image.open(image_path)
    if image.mode != 'RGB':
        image = image.convert('RGB')
    inverted_image = ImageOps.invert(image)
    byte_arr = io.BytesIO()
    inverted_image.save(byte_arr, format='JPEG')
    return base64.b64encode(byte_arr.getvalue()).decode('utf-8')

# Path to your image
image_path = "/home/gwern/wiki/doc/design/typography/rubrication/2012-butterick-stempelfontfoundrycatalogue-sample-23-rubrication.jpg"

# Getting the base64 string
base64_image = encode_image(image_path)
base64_image_inverted = invert_image(image_path)

headers = {
    "Content-Type": "application/json",
    "Authorization": f"Bearer {api_key}"
}

payload = {
    "model": "gpt-4-vision-preview",
    "messages": [
      {
        "role": "user",
        "content": [
          {
            "type": "text",
            "text": "This is the inverted image of the original, for use in a website or smartphone dark-mode. Does it look OK? Respond YES/NO."
          },
          {
            "type": "image_url",
            "image_url": { "url": f"data:image/jpeg;base64,{base64_image}" }
          },
            {
                "type": "image_url",
                "image_url": { "url": f"data:image/jpeg;base64,{base64_image_inverted}" }
            }
        ]
      }
    ],
    "max_tokens": 300
}

response = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=payload)

print(response.json())
