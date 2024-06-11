#!/bin/bash
# download-title.sh
# Author: Gwern Branwen
# Date: 2024-06-10
# When:  Time-stamp: "2024-06-10 21:07:12 gwern"
# License: CC-0
#
# download-title.sh: download a URL, and if it is a parseable HTML page, print out the contents of the title tag, `<title>TITLE</title>`.
#
# $ ./download-title.sh "http://catb.org/~esr/writings/taoup/html/ch05s01.html"
# The Importance of Being Textual
#
# dependencies: curl, iconv, xmllint, timeout

set -e

if [ $# -lt 1 ]; then echo "download-title.sh: Error: Not enough arguments" && exit 1; fi

# make sure xmllint is available for HTML parsing:
if ! command -v xmllint > /dev/null; then
    echo "download-title.sh: Error: xmllint is not installed. Please install the 'libxml2-utils'/'libxml2' package"
    exit 2
fi

# Function to extract the title from HTML
extract_title() {
    local URL="$1"
    local CONTENT_TYPE

    # Fetch the URL headers to check the content type
    CONTENT_TYPE=$(timeout 20s curl --silent --head "$URL" | grep --ignore-case "Content-Type:")

    # Check if the content type is HTML
    if echo "$CONTENT_TYPE" | grep --quiet --ignore-case "text/html"; then
        # Fetch the URL content and extract the title
        timeout 20s curl --silent "$URL" | iconv -c --to-code=utf8 | \
            xmllint --html --xpath '//title/text()' - 2>/dev/null
    fi
}

for PAGE in "$@"; do
    extract_title "$PAGE"
done
