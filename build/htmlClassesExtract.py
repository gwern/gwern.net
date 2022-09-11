#!/usr/bin/env python3

from bs4 import BeautifulSoup
import requests
import sys


# class list set
class_list = set()

for fileIndex in range(1, len(sys.argv)):

    page = open(sys.argv[fileIndex], "r").read()

    # parse html content
    soup = BeautifulSoup(page, 'html.parser')

    # get all tags
    tags = {tag.name for tag in soup.find_all()}

    # iterate all tags
    for tag in tags:

        # find all element of tag
        for i in soup.find_all(tag):

            # if tag has attribute of class
            if i.has_attr("class"):

                if len(i['class']) != 0:
                    class_list.add(" ".join(i['class']))

for val in class_list:
    print(val)
