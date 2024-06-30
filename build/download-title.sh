#!/bin/bash
# download-title.sh
# Author: Gwern Branwen
# Date: 2024-06-10
# When:  Time-stamp: "2024-06-30 17:44:06 gwern"
# License: CC-0
#
# download-title.sh: download a URL, and if it is a parseable HTML page, print out the contents of the title tag, `<title>TITLE</title>`.
#
# $ ./download-title.sh "http://catb.org/~esr/writings/taoup/html/ch05s01.html"
# The Importance of Being Textual
# $ ./download-title.sh  https://blog.nationalmuseum.ch/en/2024/06/the-dream-of-an-alpine-waterway/
# The dream of an alpine waterway – Swiss National Museum - Swiss history blog
#
# dependencies: curl, xmllint, timeout, file, iconv, perl

set -euo pipefail

if [ $# -lt 1 ]; then echo "download-title.sh: Error: Not enough arguments" && exit 1; fi

# make sure xmllint is available for HTML parsing:
if ! command -v xmllint > /dev/null; then
    echo "download-title.sh: Error: xmllint is not installed. Please install the 'libxml2-utils'/'libxml2' package"
    exit 2
fi

# Function to extract the title from HTML
extract_title() {
    local URL="$1"
    local TEMP_FILE
    TEMP_FILE=$(mktemp) || { echo "Failed to create temp file" >&2; return 1; }

    # Fetch the URL content and save to a temporary file
    timeout 20s curl --silent -L "$URL" > "$TEMP_FILE"

    # Use perl to decode the HTML entities and extract the title
    perl -MHTML::Entities -MHTML::TreeBuilder -e '
        my $tree = HTML::TreeBuilder->new;
        $tree->parse_file(shift);
        my $title = $tree->look_down(_tag => "title");
        if ($title) {
            print decode_entities($title->as_text);
        }
        $tree->delete;
    ' "$TEMP_FILE" | tr -d '\n' | sed 's/^[[:space:]]*//' | sed 's/[[:space:]]*$//'

    # Clean up
    rm "$TEMP_FILE"
}

for PAGE in "$@"; do
    extract_title "$PAGE"
done
