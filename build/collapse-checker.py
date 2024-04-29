#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# collapse-checker.py: check a HTML file for too-small/unnecessarily 'collapsed' HTML blocks
# Author: Gwern Branwen
# Date: 2024-01-04
# When:  Time-stamp: "2024-04-28 20:36:46 gwern"
# License: CC-0
#
# Usage: $ collapse-checker.py foo.html
#
# On Gwern.net, we make heavy use of 'collapses': code folding or disclosure-like regions which are shrunk by default,
# but the user can hover over or click to expand into the full section. This is useful for managing complexity and
# allowing readers to opt into levels of detail. (It is also useful for handling transclusions, as transcludes inside a collapse
# are lazy and will not transclude until the reader evaluates them.) But we can overuse collapses, especially by collapsing small items,
# like 1--2 item lists. (Especially common in annotations' "See Also" lists, where I've pruned them down to just a few relevant links,
# but then forgot to remove the 'collapse' class manually.) These are bad because it impose a lot of visual clutter
# (to denote a collapsed/uncollapsed region) and the reader has to think about them and activate them etc, which are burdens.
#
# So we use BeautifulSoup + heuristics to parse Gwern.net-style HTML files for collapsed blocks, try to guess if they are 'small' or 'large', and print out a warning if small.
#
# This is used in the site build process to check all essay & annotations.
#
# By default, it prints out the filename of files which have a bad-looking collapse. If the filename has percent signs in it, indicating that it is URL-encoded (eg. `/metadata/annotation/*.html` snippets), it is URL-decoded for easier reference/modification.

## Some examples of 'too small' collapses:
# html_content1 = """
# <div class="collapse">
# <ul>
# <li>one</li>
# <li>two</li>
# </ul>
# </div>
# """
# html_content = "<div class="collapse"><p>foo</p><p>bar</p></div>"
# html_content = "<p>Foo <span class="collapse">foo</span> bar.</p><p>bar</p></div>"

from bs4 import BeautifulSoup, Tag
import sys
from urllib.parse import unquote

def print_filename(filename):
    decoded_filename = unquote(filename)
    print(decoded_filename)

def print_red(message):
    """
    Prints a message to stderr in red.

    Args:
    - message (str): The message to print.
    """
    # ANSI escape code for red, then message, then reset color
    sys.stderr.write(f"\033[91m{message}\033[0m\n")

def read_and_parse_html(filename):
    """Read HTML content from a file and return a BeautifulSoup object."""
    try:
        with open(filename, "r") as file:
            html_content = file.read()
        return BeautifulSoup(html_content, "html.parser")
    except IOError as e:
        print_red(f"Error reading file {filename}: {e}")
        return None

def has_excluded_class(element, exclude_classes):
    """Check if the element or any of its parents have a class that should be excluded."""
    current_element = element
    while current_element is not None:
        if any(cls in exclude_classes for cls in current_element.get("class", [])):
            return True
        current_element = current_element.parent
    return False

def check_for_incorrect_collapse_usage(soup, filename):
    """Check the BeautifulSoup object for incorrect .collapse usage and log findings."""
    collapse_elements = soup.find_all(class_="collapse")

    exclude_elements = ["h1", "h2", "h3", "h4", "h5", "h6", "section", "span", "code", "pre", "figure", "img", "a"]
    exclude_classes = {"backlinks-append", "similars-append", "link-bibliography-append", "aux-links-transclude-file"} # we exclude .aux-links-append because 'See Also' have the most redundant collapses

    for element in collapse_elements:
        # Check if the element itself or any parent should be excluded based on classes
        if has_excluded_class(element, exclude_classes):
            continue

        # Skip if element itself or any inner element has excluded tags
        if element.name in exclude_elements or element.find(exclude_elements):
            continue

        # Initialize content volume assessment
        content_volume = 0
        content_volume += len(element.find_all("p")) + len(element.find_all("br"))

        # Add volume for figure and img tags due to their significant content contribution
        content_volume += 3 * len(element.find_all("figure"))  # Arbitrarily weighting figures more
        content_volume += 2 * len(element.find_all("img"))     # Weighting images less because might be inline rather than big <figure> blocks

        # Direct child tags that are meaningful for content volume
        direct_children = [child for child in element.children if isinstance(child, Tag)]
        direct_children_str = ", ".join(str(e) for e in direct_children)  # Convert each element to string and join with comma

        for child in direct_children:
            # Increment content volume for direct meaningful children
            if child.name not in ["ul", "ol", "div"]:
                content_volume += 1
            # For <ul>, <ol>, add the count of <li> items to content volume
            elif child.name in ["ul", "ol"]:
                content_volume += len(child.find_all("li"))
            # For <div> or similar containers, consider nested lists
            elif child.name == "div":
                nested_lists = child.find_all(["ul", "ol"])
                for lst in nested_lists:
                    content_volume += len(lst.find_all("li"))

        if content_volume <= 6:

            print_filename(filename)
            # print_red(element)
            # print_red("------------------------------------------------")
            # print_red(f"Incorrect use of '.collapse' class in {filename}!")
            # print_red("Debugging information... direct children elements: " + str(direct_children))
            # print_red("'.collapse' elements: " + str(collapse_elements))
            # print_red("------------------------------------------------")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print_red("Usage: python collapse-checker.py <filename1> [filename2] …")
        sys.exit(1)

    for filename in sys.argv[1:]:
        soup = read_and_parse_html(filename)
        check_for_incorrect_collapse_usage(soup, filename)
