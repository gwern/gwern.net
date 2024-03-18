# make an *inverted* or *negated* image for a dark-mode (comparable to ImageMagick `convert -negate`) version of `base64_image`, named `base64_image_inverted`

import base64
import requests

from PIL import Image, ImageOps
import io
import os

api_key = os.getenv("OPENAI_API_KEY")

# Function to encode the image
def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')
# Function to invert the image
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
