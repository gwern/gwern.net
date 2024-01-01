#!/bin/bash

# Read from stdin and process the input suitable for feeding into an Emacs buffer for editing & storing in full/half.yaml.
cat - | ~/wiki/static/build/preprocess-markdown | \
    pandoc --mathjax --metadata title='Annotation preview' --to=html5 --from=html | \
    tidy -quiet --show-warnings no --show-body-only auto -indent -wrap 130 \
         --clean yes --merge-divs no --break-before-br yes --logical-emphasis yes --quote-nbsp no  | \
    sed 's/^/    /'
