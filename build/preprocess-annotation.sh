#!/bin/bash

# Read from stdin and process the input suitable for feeding into an Emacs buffer for editing & storing in full/half.gtx.
cat - | ~/wiki/static/build/preprocess-markdown | \
    pandoc --mathjax --metadata title='Annotation preview' --to=html5 --from=html | \
    tidy -quiet --show-warnings no --show-body-only auto -wrap 180 \
         --clean yes --merge-divs no --break-before-br yes --logical-emphasis yes --quote-nbsp no
