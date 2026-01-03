#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# htmlAttributesExtract.py: extract all CSS classes, data-attribute keys, and IDs from HTML files
# Author: Gwern Branwen
# Date: 2022-09-10
# When:  Time-stamp: "2026-01-02 10:01:31 gwern"
# License: CC-0
#
# BeautifulSoup utility to parse HTML & print all HTML classes, data-attribute keys, and IDs.
# The usecase is to run over compiled corpus to extract all HTML classes and filter through a grep whitelist of known classes; this frequently catchs typos and unnecessary classes.
# We check both classes & data-attribute keys for 'unknown' entries; we skip the data-attribute values because by design they will vary far too much to easily check.
# (We use htmlAttributesExtract.py in `sync.sh`; the whitelist serves as additional documentation for Gwern.net features.)
# We further extract IDs (prefixed with 'id:' to eliminate ambiguity) to allow checking for IDs which match whitelists/blacklists (eg. matching anything in the class or data-attribute whitelists, which is likely an error).
#
# Usage: $ python htmlAttributesExtract.py file1.html file2.html ...
#
# Examples:
#
# $ python htmlAttributesExtract.py index.html
# TOC
# abstract smallcaps-not dropcap-not
# ...
# data-filesize-bytes
# data-link-icon
# ...
# id:footnote-1
# id:introduction
# ...

from bs4 import BeautifulSoup
import sys
import os

# Validate arguments
if len(sys.argv) < 2:
    print("Error: No input files provided.", file=sys.stderr)
    print(f"Usage: $ {sys.argv[0]} file1.html [file2.html ...]", file=sys.stderr)
    sys.exit(1)

for filepath in sys.argv[1:]:
    if not os.path.exists(filepath):
        print(f"Error: File does not exist: {filepath}", file=sys.stderr)
        sys.exit(1)
    if not os.path.isfile(filepath):
        print(f"Error: Not a regular file: {filepath}", file=sys.stderr)
        sys.exit(1)
    if not os.access(filepath, os.R_OK):
        print(f"Error: File is not readable: {filepath}", file=sys.stderr)
        sys.exit(1)
    if os.path.getsize(filepath) == 0:
        print(f"Error: File is empty: {filepath}", file=sys.stderr)
        sys.exit(1)

class_list = set()
data_attr_list = set()
id_list = set()

for file_index in range(1, len(sys.argv)):
    with open(sys.argv[file_index], "r") as f:
        page = f.read()
    soup = BeautifulSoup(page, 'html.parser')
    # iterate all elements once
    for element in soup.find_all():
        if element.has_attr("class"):
            class_list.add(" ".join(element['class']))
        if element.has_attr("id"):
            id_list.add(element['id'])
        for attr in element.attrs:
            if attr.startswith("data-"):
                data_attr_list.add(attr)

# print("Classes:")
for val in sorted(class_list):
    print(f"{val}")

# print("\nData attributes:")
for attr in sorted(data_attr_list):
    print(f"{attr}")

## print("\nIDs:")
# for id_val in sorted(id_list):
#     print(f"id:{id_val}")

