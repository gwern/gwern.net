#!/bin/bash

# wikipediaExtract.sh: download a English Wikipedia article's MediaWiki sources through the old API, and compile the introduction into HTML suitable for popup annotations
# Author: Gwern Branwen
# Date: 2021-02-28
# When:  Time-stamp: "2021-03-02 16:32:20 gwern"
# License: CC-0
#
# Shell script to take an WP article and extract the introduction.
#
# This was originally done using the Page Preview API (https://www.mediawiki.org/wiki/Page_Previews), which is how popups are done for logged-out users (eg `curl "https://en.wikipedia.org/api/rest_v1/page/summary/E._E._Cummings"`), however, that is by design crippled, as it removes all links: 'Flattening inline elements The API must replace all span and a tags with their text content, e.g. <span>Foo</span> should be flattened to Foo and <a href="/foo">Foo</a> would be flattened to Foo.' https://www.mediawiki.org/wiki/Page_Previews/API_Specification#Markup_allowed_in_an_intro As much of the value of WP, and gwern.net recursive popups in general, is being able to click/follow links, this eliminates much of the value of such extracts. The Page Preview extracts are also, IMO, far too short, often omitting worthwhile content in the introduction. (Since our annotations properly collapse/scroll long annotations, it is better to be too long rather than too short. Let the reader decide for themselves!)
# The logged-in user preview, Lupin's page navigation popup tool (https://en.wikipedia.org/wiki/Wikipedia:Tools/Navigation_popups https://phabricator.wikimedia.org/project/profile/2055/), *does* include the inline links. But close inspection of its source (https://en.wikipedia.org/wiki/MediaWiki:Gadget-popups.js) shows that there is no secret API returning the right HTML. Instead, it download the entire page MediaWiki source, and compiles it via a JS library (https://github.com/cscott/instaview) to HTML on its own! Since I was unable to find any, I decided to imitate Lupin and just compile the MediaWiki to HTML via Pandoc and work with that instead (since at least it has everything).
#
# Example:
#
#    $ ./wikipediaExtract.sh 'https://en.wikipedia.org/wiki/Dog' # NOTE: or just 'Dog'
#    <p>The <a href="https://en.wikipedia.org/wiki/Domestication">domestic</a> <strong>dog</strong> (<em>Canis familiaris</em> when considered a separate <a href="https://en.wikipedia.org/wiki/species">species</a> or <em>Canis lupus familiaris</em> when considered a <a href="https://en.wikipedia.org/wiki/subspecies">subspecies</a> of the <a href="https://en.wikipedia.org/wiki/wolf">wolf</a>)</p>
#    <p>The dog has been <a href="https://en.wikipedia.org/wiki/Selective_breeding">selectively bred</a> over millennia for various behaviors, sensory capabilities and physical attributes. They perform many roles for humans, such as <a href="https://en.wikipedia.org/wiki/Hunting_dog">hunting</a>, <a href="https://en.wikipedia.org/wiki/Herding_dog">herding</a>, <a href="https://en.wikipedia.org/wiki/Sled_dog">pulling loads</a>, <a href="https://en.wikipedia.org/wiki/Guard_dog">protection</a>, <a href="https://en.wikipedia.org/wiki/Police_dog">assisting police</a> and the <a href="https://en.wikipedia.org/wiki/Dogs_in_warfare">military</a>, <a href="https://en.wikipedia.org/wiki/Pet">companionship</a> and, more recently, <a href="https://en.wikipedia.org/wiki/Service_dog">aiding disabled people</a> and <a href="https://en.wikipedia.org/wiki/Therapy_dog">therapeutic</a> roles. This influence on human society has given them the <a href="https://en.wikipedia.org/wiki/sobriquet">sobriquet</a> of "<a href="https://en.wikipedia.org/wiki/man&#39;s_best_friend">man's best friend</a>."</p>
#    <h2 id="taxonomy">Taxonomy</h2>
#    <p>In 1758, the Swedish botanist and zoologist <a href="https://en.wikipedia.org/wiki/Carl_Linnaeus">Carl Linnaeus</a> published in his <em><a href="https://en.wikipedia.org/wiki/10th_edition_of_Systema_Naturae">Systema Naturae</a></em> the <a href="https://en.wikipedia.org/wiki/binomial_nomenclature">binomial nomenclature</a> – or the two-word naming – of species. <em><a href="https://en.wikipedia.org/wiki/Canis">Canis</a></em> is the Latin word meaning "dog",</p>
#    <p>In 1999, a study of <a href="https://en.wikipedia.org/wiki/mitochondrial_DNA">mitochondrial DNA</a> (mtDNA) indicated that the domestic dog might have originated from multiple gray <a href="https://en.wikipedia.org/wiki/wolf">wolf</a> populations, with the <a href="https://en.wikipedia.org/wiki/dingo">dingo</a> and <a href="https://en.wikipedia.org/wiki/New_Guinea_singing_dog">New Guinea singing dog</a> "breeds" having developed at a time when human communities were more isolated from each other.</p>
#    <p>In 2019, a workshop hosted by the <a href="https://en.wikipedia.org/wiki/IUCN">IUCN</a>/Species Survival Commission's Canid Specialist Group considered the New Guinea singing dog and the dingo to be feral dogs <em>Canis familiaris</em>, and therefore should not be assessed for the <a href="https://en.wikipedia.org/wiki/IUCN_Red_List">IUCN Red List</a>.</p>
#    <h2 id="origin">Origin</h2>
#    <p>The domestic dog's origin includes the dog's <a href="https://en.wikipedia.org/wiki/genetic_divergence">genetic divergence</a> from the wolf, its <a href="https://en.wikipedia.org/wiki/domestication">domestication</a>, and the emergence of the first dogs. Genetic studies show that all ancient and modern dogs share a common ancestry and descended from an ancient, now-extinct wolf population - or closely related wolf populations - which was distinct from the <a href="https://en.wikipedia.org/wiki/Wolf">modern wolf</a> lineage.</p>
#    <p>The dog is a member of the <a href="https://en.wikipedia.org/wiki/Evolution_of_the_wolf#Wolf-like_canids">wolf-like canids</a></p>
#    <h2 id="biology">Biology</h2>
#    <p>…</p>
#
# Requires: Pandoc, jq, curl, sed

set -e
# set -x

ARTICLE=$(basename "$1")
curl --user-agent 'gwern+wikipediascraping@gwern.net' --location --silent \
     'https://en.wikipedia.org/w/api.php?format=json&formatversion=2&action=query&prop=revisions&rvprop=content&rvslots=main&redirects&titles='"$ARTICLE" | \

    # extract just the markup/source:
    jq -r '.query.pages[0].revisions[0].slots.main.content' | \

    # getting the raw image URL to hotlink or localize is too hard, so we'll just omit images beyond the thumbnail:
    egrep -v -e '^\[\[File\:' | \

    # convert math templates like '{{mvar|x}}' to '<em>x</em>':
    sed -e 's/{{mvar|\(.\)}}/<em>\1<\/em>/g' | \

    # MediaWiki templates & tables are not supported by Pandoc and will crash it, so strip those out: targeting '{{', '{-', '|-', ' |' etc ## TODO: parsing templates is really complex and regexps don't work too well so many pages get messed up or have garbage in them, but on the other hand, if we don't strip at all, Pandoc may crash on templates/tables... right now, we fall back to the simplistic Preview API version. We'll see if crashes are rare enough to make that the best tradeoff.
    # sed -e 's/^{{.*}}$//g' -e '/^{{/,/^}}/d' -e '/^{|/,/^|}/d' | \
    # egrep -v -e '^\|-' -e '^[[:blank:]]\+\|' -e '^\|' -e '^{' -e '^}' | \

    # Pandoc's MediaWiki capabilities aren't too bad, and produce sensible output on most pages (it drops the templates etc):
    pandoc --mathjax -f mediawiki -t html | \

    # but we do need to delete the footnotes, rewrite the relative links to absolute links to En, delete extraneous 'title=' parameters, clean up stray templates & references:
    sed -e 's/<a href\="#fn[0-9]\+" class\="footnote-ref" id\="fnref[0-9]\+" role\="doc-noteref"><sup>[0-9]\+<\/sup><\/a>//g' \
        -e 's/<a href\="#[[:graph:]]\+" title="wikilink">\([[:graph:]]\+\)<\/a>/<strong>\1<\/strong>/g' \
        -e 's/<a href="\([[:graph:]]\+\)" title="wikilink">/<a href="https:\/\/en.wikipedia.org\/wiki\/\u\1" title="wikilink">/g' \
        -e 's/ title="wikilink"//g' \
        -e "s/{{'}}/'/g" -e "s/{{' \"}}/'\"/g"  -e 's/<ref .*<\/p>$/<\/p>/g' -e 's/{{cite .*<\/p>$/<\/p>/g' -e 's/<ref>\*//g' | \
    fgrep -v -e '{{' -e '}}' -e '<br />' | \

    # truncate to first section or so:
    runhaskell -istatic/build/ static/build/truncatePandoc.hs 3;
