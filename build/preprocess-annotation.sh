#!/bin/bash

# Read from stdin and process the input suitable for feeding into an Emacs buffer for editing & storing in full/half.gtx.
cat - | ~/wiki/static/build/preprocess-markdown | \
    pandoc --mathjax --metadata title='Annotation preview' --to=html5 --from=html | \
    tidy -quiet --show-warnings no --show-body-only auto -indent -wrap 180 \
         --clean yes --merge-divs no --break-before-br yes --logical-emphasis yes --quote-nbsp no || true # tidy always exits with exit-status=1 ('there were warnings') so we need to suppress that by either running another command in the pipeline (which would swallow that exit-status) or just explicitly ignoring it like `|| true`.
