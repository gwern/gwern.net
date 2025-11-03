#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# htmlClassesExtract.py: extract all CSS classes and data-attribute keys from HTML files
# Author: Gwern Branwen
# Date: 2022-09-10
# When:  Time-stamp: "2025-11-02 17:08:12 gwern"
# License: CC-0
#
# BeautifulSoup utility to parse HTML & print all HTML classes and the data-attribute keys.
# The usecase is to run over compiled corpus to extract all HTML classes and filter through a grep whitelist of known classes; this frequently catchs typos and unnecessary classes.
# We check both classes & data-attribute keys for 'unknown' entries; we skip the data-attribute values because by design they will vary far too much to easily check.
# (We use htmlClassesExtract.py in `sync.sh`; the whitelist serves as additional documentation for Gwern.net features.)
#
# Usage: $ python htmlClassesExtract.py file1.html file2.html ...
#
# Examples:
#
# $ python htmlClassesExtract.py index.html
# TOC
# abstract smallcaps-not dropcap-not
# ...
# data-filesize-bytes
# data-link-icon
# ...

from bs4 import BeautifulSoup
import sys

class_list = set()
data_attr_list = set()

for file_index in range(1, len(sys.argv)):
    with open(sys.argv[file_index], "r") as f:
        page = f.read()

    soup = BeautifulSoup(page, 'html.parser')

    # iterate all elements once
    for element in soup.find_all():
        if element.has_attr("class"):
            class_list.add(" ".join(element['class']))

        for attr in element.attrs:
            if attr.startswith("data-"):
                data_attr_list.add(attr)

# print("Classes:")
for val in sorted(class_list):
    print(f"{val}")

# print("\nData attributes:")
for attr in sorted(data_attr_list):
    print(f"{attr}")
