#!/bin/bash

# wikipediaExtract.sh: download a English Wikipedia article's MediaWiki sources through the old API, and compile the introduction into HTML suitable for popup annotations
# Author: Gwern Branwen
# Date: 2021-02-28
# When:  Time-stamp: "2021-03-08 19:04:30 gwern"
# License: CC-0
#
# Shell script to take an WP article and extract the introduction.
#
# This was originally done using the Page Preview API (https://www.mediawiki.org/wiki/Page_Previews), which is how popups are done for logged-out users (eg `curl "https://en.wikipedia.org/api/rest_v1/page/summary/E._E._Cummings"`), however, that is by design crippled, as it removes all links: 'Flattening inline elements The API must replace all span and a tags with their text content, e.g. <span>Foo</span> should be flattened to Foo and <a href="/foo">Foo</a> would be flattened to Foo.' https://www.mediawiki.org/wiki/Page_Previews/API_Specification#Markup_allowed_in_an_intro As much of the value of WP, and gwern.net recursive popups in general, is being able to click/follow links, this eliminates much of the value of such extracts. The Page Preview extracts are also, IMO, far too short, often omitting worthwhile content in the introduction. (Since our annotations properly collapse/scroll long annotations, it is better to be too long rather than too short. Let the reader decide for themselves!)
# The logged-in user preview, Lupin's page navigation popup tool (https://en.wikipedia.org/wiki/Wikipedia:Tools/Navigation_popups https://phabricator.wikimedia.org/project/profile/2055/), *does* include the inline links. But close inspection of its source (https://en.wikipedia.org/wiki/MediaWiki:Gadget-popups.js) shows that there is no secret API returning the right HTML. Instead, it download the entire page MediaWiki source, and compiles it via a JS library (https://github.com/cscott/instaview) to HTML on its own! I attempted to work with this using Pandoc to compile, and for simple articles this works well enough, but it fails badly on any article which makes heavy use of templates (which is many of them, particularly STEM ones), and hand-substitution or replacement couldn't keep up with the infinite long tail of WP templates.
#
# Finally I switched to the 'mobile' HTML API (https://www.mediawiki.org/wiki/Page_Content_Service#HTML_endpoints https://en.wikipedia.org/api/rest_v1/#/Mobile): eg `curl --silent "https://en.wikipedia.org/api/rest_v1/page/mobile-sections-lead/Dog"`; this provides the raw HTML, for only the introduction, and the big benefit is bypassing template issues entirely as they will have been compiled to said complex HTML (and WP will handle all future templates as well). The Mobile API provides a simplified HTML presentation compared to the desktop, and it's not too hard to clean this up a little further to rewrite the relative links and remove references/footnotes (which are so much clutter).
#
# Example:
#
#    $ ./static/build/wikipediaExtract.sh 'https://en.wikipedia.org/wiki/Dog' # NOTE: or just 'Dog'
#      <p>The domestic <strong>dog</strong> (<em>Canis familiaris</em> or <em>Canis lupus familiaris</em>) is a <a href="https://en.wikipedia.org/wiki/Domestication" title="Domestication">domesticated</a> form of <a href="https://en.wikipedia.org/wiki/Wolf" title="Wolf">wolf</a>. The dog descended from an ancient, extinct wolf, with the modern <a href="https://en.wikipedia.org/wiki/Wolf" title="Wolf">wolf</a> being the dog's nearest living relative. The dog was the first species to be domesticated by <a href="https://en.wikipedia.org/wiki/Hunter–gatherers" class="mw-redirect" title="Hunter–gatherers">hunter–gatherers</a> more than 15,000 years ago, which predates agriculture. Their <a href="https://en.wikipedia.org/wiki/Human–canine_bond" title="Human–canine bond">long association with humans</a> has led dogs to be uniquely attuned to human behavior, cosmopolitan distribution, and are able to thrive on a starch-rich diet that would be inadequate for other canids.</p>
#      <div class="shortdescription nomobile noexcerpt noprint searchaux" about="#mwt8" style="display:none">
#      Domesticated canid species
#      </div>
#      <p><span> </span></p>
#      <p>Domestic dogs<br />
#      </p>
#      <div style="font-size: 85%;">
#      Temporal range: At least 14,200 years ago – present
#      </div>
#      <p><span><img src="https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Collage_of_Nine_Dogs.jpg/300px-Collage_of_Nine_Dogs.jpg" srcset="https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Collage_of_Nine_Dogs.jpg/450px-Collage_of_Nine_Dogs.jpg 1.5x, //upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Collage_of_Nine_Dogs.jpg/600px-Collage_of_Nine_Dogs.jpg 2x" width="300" height="264" /></span></p>
#      <div style="text-align: center">
#      <a href="https://en.wikipedia.org/wiki/Conservation_status" title="Conservation status">Conservation status</a>
#      </div>
#      <div style="text-align: center">
#      Domesticated <span class="small"></span>
#      </div>
#      ...
#
# Requires: Pandoc, jq, curl, sed

set -e
# set -x

ARTICLE=$(basename "$1" | sed -e 's/#.*//') # strip anchors because we always get the same introduction section regardless
curl --user-agent 'gwern+wikipediascraping@gwern.net' --location --silent \
     "https://en.wikipedia.org/api/rest_v1/page/mobile-sections-lead/$ARTICLE" | \

    # extract just the first section HTML:
    jq -r .sections[0].text | \

    # remove unwanted sections like the extremely-lengthy sidebars, or references:
    ./static/build/wikipediaFilter.hs | \

    # run through Pandoc to clean up the HTML a little, and convert the MediaWiki MathML default (apparently can't be changed in the API query) to MathJax:
    pandoc --mathjax -f html -t html | \

    # but we do need to rewrite the relative links to absolute links to En (substituting in the normalized $ARTICLE for self-link anchors), and delete any footnotes/references that might've escaped wikipediaFilter's cleaning:
    sed -e 's/<a href="\/wiki\/\([[:graph:]]\+\)"/<a href="https:\/\/en.wikipedia.org\/wiki\/\u\1"/g' \
        -e "s/<a href=\"\(#[[:graph:]]\)/<a href=\"https:\/\/en.wikipedia.org\/wiki\/$ARTICLE\1/g" \
        -e 's/ src="\/\// src="https:\/\//g' -e 's/ srcset="\/\// srcset="https:\/\//g' -e 's/ href="\/\// href="https:\/\//g' \
        -e 's/<span class="mw-ref reference" id="cite_ref-[[:graph:]]\+"><a href="#cite_note-[[:graph:]]\+-[0-9]\+" style="counter-reset: mw-Ref [0-9]\+;"><span class="mw-reflink-text">\[[0-9]\+\]<\/span><\/a><\/span>//g' \
        -e 's/<span class="mw-ref reference" id="cite_ref-[0-9]\+"><a href="#cite_note-[0-9]\+" style="counter-reset: mw-Ref [0-9]\+;"><span class="mw-reflink-text">\[[0-9]\+\]<\/span><\/a><\/span>//g' \
        -e 's/<span id="cite_ref-[[:graph:]]\+" class="mw-ref reference"><a href="#cite_note-[[:graph:]]\+"><span class="mw-reflink-text">\[[0-9]\+\]<\/span><\/a><\/span>//g' \
        -e 's/<span class="mw-ref reference"><a href="#cite_note-[[:graph:]]\+"><span class="mw-reflink-text">\[[[:graph:]]\+\]<\/span><\/a><\/span><\/span>//g' \
        -e 's/<span id="cite_ref-[[:graph:]]\+" class="mw-ref reference"><a href="#cite_note-[[:graph:]]\+"><span class="mw-reflink-text">\[[[:graph:]]\+ [[:graph:]]\+\]<\/span><\/a><\/span>//g' \
        -e 's/<span class="mw-ref reference"><a href="#cite_note[[:graph:]]\+"><span class="mw-reflink-text">\[[[:graph:]]\+\]<\/span><\/a><\/span>//g'
