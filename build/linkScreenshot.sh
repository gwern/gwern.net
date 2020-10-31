#!/bin/bash

# linkScreenshot.sh: screenshot URLs/PDFs
# Author: Gwern Branwen
# Date: 2019-08-25
# When:  Time-stamp: "2020-10-31 16:46:14 gwern"
# License: CC-0
#
# Shell script to screenshot URLs/PDFs for use with LinkMetadata.hs: we screenshot a path, optimize it, and save it to /metadata/previews/$SHA1($URL).png
#
# Example:
# $ linkScreenshot.sh 'http://forum.evageeks.org/post/464235/Evangelion-20-CRC-Anno-Interview/#464235' →
#
# Requires: wget, Google Chrome/Chromium, Ghostscript, ImageMagick; for compression: pngnq, AdvPNG ('advancecomp')

set -e
set -x

t() { timeout --kill-after=60s 60s "$@"; }

pdf() {
    FILE=$(echo "$1" | sed -e 's/#.*//') # delete any cute page-anchors like 'docs/math/2012-gowers.pdf#page=11'
    PAGE_NUMBER=1
    if [[ "$3" =~ .*\#page\=.* ]]; then
        PAGE_NUMBER=$(echo "$3" | sed -e 's/.*#page=\([[:digit:]]*\)/\1/') # but we want to screenshot specific pages if specified
    fi
    t gs -r300 -sDEVICE=pnggray -sOutputFile="$2" -dQUIET -dFirstPage="$PAGE_NUMBER" -dLastPage="$PAGE_NUMBER" -dNOPAUSE -dBATCH "$FILE"

    # crop down the PDF to eliminate the huge margins and focus on (probably) the title/abstract
    mogrify -gravity North -crop 85x85%+0+50 -gravity Center -scale 768x768 "$2"
    }

# we use SHA1 for turning a URL into an ID/hash because URL-encoding via `urlencode` causes Ghostscript to crash (and risks long-filename issues), MD5 isn't supported in the Subtle Crypto JS library most web browsers support, and SHA-256 & higher are just wastes of space in this context.
# WARNING: remember that 'echo' appends newlines by default!
HASH="$(echo -n "$@" | sed -e 's/^\///' | sha1sum - | cut -d ' ' -f 1).png"
rm /tmp/"$HASH"* || true

# do we want to abort early if there is already a screenshot, or do we want to overwrite it anyway? (Perhaps a whole run went bad.)
INVALIDATE_CACHED_SCREENSHOT="false"
if [[ (! "$INVALIDATE_CACHED_SCREENSHOT" == "true") && -s ~/wiki/metadata/previews/"$HASH" ]]; then
    exit 0
fi

# Local PDF:
if [[ "$@" =~ ^/?docs/.*\.pdf(#page=[[:digit:]]*)? ]]; then
    pdf ~/wiki/"$@" /tmp/"$HASH" "$@"
else
    # Local HTML:
    if [[ "$@" =~ ^docs/.*\.html$ ]]; then
        t chromium-browser --disable-background-networking --disable-background-timer-throttling --disable-breakpad --disable-client-side-phishing-detection --disable-default-apps --disable-dev-shm-usage --disable-extensions --disable-features=site-per-process --disable-hang-monitor --disable-popup-blocking --disable-prompt-on-repost --disable-sync --disable-translate --metrics-recording-only --no-first-run --safebrowsing-disable-auto-update --enable-automation --password-store=basic --use-mock-keychain --hide-scrollbars --mute-audio --headless --disable-gpu --hide-scrollbars --screenshot=/tmp/"$HASH" \
          --window-size=768,768 "$@"
    else
       # Remote HTML, which might actually be a PDF:
       MIME=$(timeout 20s curl --user-agent "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:69.0) Gecko/20100101 Firefox/69.0" --write-out '%{content_type}' --silent -L -o /dev/null "$@")
       if [[ "$@" =~ .*\.pdf.* || "$MIME" == "application/pdf" || "$MIME" == "application/octet-stream" ]] ; then

           echo "Headless Chrome does not support PDF viewing (https://github.com/GoogleChrome/puppeteer/issues/299 \
                        https://github.com/GoogleChrome/puppeteer/issues/1872), so downloading instead..."

           t wget --user-agent="Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:70.0) Gecko/20100101 Firefox/70.0" --quiet --output-file=/dev/null "$@" --output-document=/tmp/"$HASH".pdf
           pdf /tmp/"$HASH".pdf /tmp/"$HASH" "$@"
           rm /tmp/"$HASH".pdf
       else
           # Remote HTML, which is actually HTML:
           t chromium-browser --disable-background-networking --disable-background-timer-throttling --disable-breakpad --disable-client-side-phishing-detection --disable-default-apps --disable-dev-shm-usage --disable-extensions --disable-features=site-per-process --disable-hang-monitor --disable-popup-blocking --disable-prompt-on-repost --disable-sync --disable-translate --metrics-recording-only --no-first-run --safebrowsing-disable-auto-update --enable-automation --password-store=basic --use-mock-keychain --hide-scrollbars --mute-audio --headless --disable-gpu --hide-scrollbars --screenshot=/tmp/"$HASH" \
             --window-size=768,768 "$@"
       fi
    fi
fi

# Now that we have a PNG, somehow, optimize it so the bandwidth/storage isn't ruinous over thousands of hyperlinks:
pngnq -n 128 -s1 /tmp/"$HASH"

## WARNING: pngnq by default saves converted PNGs to $FILE-nq8.png. It has an option to do it in place, which would save effort, but the last time I tried it on Ubuntu, it was buggy and crashed because it erased the file before compressing! So we do it the hard way.
FILE="${HASH%.*}"
mv "/tmp/$FILE"-nq8.png /tmp/"$HASH"

advpng --iter 30 -z --shrink-insane /tmp/"$HASH"

mv /tmp/"$HASH" ~/wiki/metadata/previews/"$HASH"
