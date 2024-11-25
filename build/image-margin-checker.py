#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# image-margin-checker.py: check an image for missing margin [DEPRECATED]
# Author: Gwern Branwen
# Date: 2024-02-13
# When:  Time-stamp: "2024-11-24 17:54:30 gwern"
# License: CC-0
#
# CLI tool to check whether an image is too-tightly cropped and needs greater padding/margin.
# (Padding can be added via the Bash function `pad` & `pad-black`.)
#
# Usage: $ imagine-margin-checker.py file1.jpg [file2.png] [...]
# Output: "file : YES" or "file : NO" (or error message if neither)
#
# WARNING: this is not recommended for use as of early 2024. The results turned out to be bad when deployed at scale, and resulted in vastly too large margins. It's unclear if GPT-4-V or the prompt was bad. Regardless, it is not recommended to use as-is.
#
# Examples:
#
# $ image-margin-checker.py ./doc/fiction/humor/2020-01-24-gwern-meme-mickeymouse-perishlikeadog-template.jpg
#   ./doc/fiction/humor/2020-01-24-gwern-meme-mickeymouse-perishlikeadog-template.jpg : NO
# $ image-margin-checker.py ./doc/fiction/science-fiction/time-travel/1998-chiang-figure-1.png
#   ./doc/fiction/science-fiction/time-travel/1998-chiang-figure-1.png : YES
# $ python3 /home/gwern/image-margin-checker.py ./doc/statistics/bias/1986-henrion-figure1-historyofmeasurementofspeedoflight18751958.png
#   WARNING: Received an error for image ./doc/statistics/bias/1986-henrion-figure1-historyofmeasurementofspeedoflight18751958.png: 'Your input image may contain content that is not allowed by our safety system.'
#
# Uses OA API's GPT-4-V: <https://platform.openai.com/docs/guides/vision>;
# requires "$OPENAI_API_KEY" set in environment for API access.
# Cost: we use 'low' as the gestalt of margins should not require much visual detail; this means that each image costs '85 [GPT-4-turbo] tokens'; the prompt is ~1,150 tokens, and the binary YES/NO = 1 token, 1,150 total; GPT-4-turbo costs ~$0.01/1k, so each image should cost ¢1.2.

import base64
import io
import os
import re
import requests
import sys

def is_valid_api_key(key):
    # Simple syntactic check of 'API key': check if the key starts with 'sk-' followed by 40 alphanumeric characters.
    return bool(re.match(r'^sk-[A-Za-z0-9]{40}$', key))

def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')

def check_image_margin(api_key, image_path):
    base64_image = encode_image(image_path)
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {api_key}"
    }
    payload = {
        "model": "gpt-4o-mini",
        "messages": [
            {
                "role": "user",
                "content": [
                    {
                        "type": "text",
                        "text": "This image is a research figure/graph. Should it have some more margin or whitespace around the edges? Respond ONLY with 'YES' or 'NO'."
                    },
                    {
                        "type": "image_url",
                        "image_url": {"url": f"data:image/jpeg;base64,{base64_image}", "detail": "low"}
                    }
                ]
            }
        ],
        "temperature": 0,
        "max_tokens": 10
    }
    response = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=payload)
    if response.status_code != 200 and response.status_code != 400:
        print(f"ERROR: HTTP request failed with status code {response.status_code}")
    else:
        response_json = response.json()

        if 'error' in response_json:
            print(f"WARNING: Received an error for image {image_path}: '{response_json['error']['message']}'")
        elif 'choices' not in response_json or not response_json['choices'][0]['message']['content']:
            print(f"WARNING: No valid response received for image {image_path}.")
        else:
            content = response_json['choices'][0]['message']['content']
            if content not in ["YES", "NO"]:
                print(f"WARNING: Unexpected response for image {image_path}: '{content}'")
            else:
                print(image_path, ":", content)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python image-margin-checker.py <filename> [<filename> ...]")
        sys.exit(1)

    api_key = os.getenv("OPENAI_API_KEY")
    if not api_key or not is_valid_api_key(api_key):
        print("ERROR: Invalid or missing API key.")
        sys.exit(1)

    for image_path in sys.argv[1:]:
        if not os.path.exists(image_path):
            print(f"ERROR: File does not exist or is not accessible: {image_path}")
            continue  # Skip to the next file
        check_image_margin(api_key, image_path)
