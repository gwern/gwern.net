#!/bin/bash

# memoriam.sh: generate a `X-Clacks-Overhead` HTTP header nginx configuration line based on day-of-year & list of deceased.
# Author: Gwern Branwen
# Date: 2024-02-24
# When:  Time-stamp: "2024-04-19 12:29:36 gwern"
# License: CC-0
#
# memoriam.sh is intended for generating HTTP headers (<https://en.wikipedia.org/wiki/List_of_HTTP_header_fields>) which memorialize a dead person.
# (For other exotic HTTP headers seen in the wild, see <http://www.nextthing.org/archives/2005/08/07/fun-with-http-headers> <https://www.pingdom.com/blog/fun-and-unusual-http-response-headers/>.)
# It was based on a Terry Pratchett steampunk anecdote about optical telegraph signaling semaphore towers,
# and originally introduced to commemorate Terry Pratchett's death:
# <http://xclacksoverhead.org/home/about> <https://www.reddit.com/r/discworld/comments/2yt9j6/gnu_terry_pratchett/>
# <https://www.theguardian.com/books/shortcuts/2015/mar/17/terry-pratchetts-name-lives-on-in-the-clacks-with-hidden-web-code>
# <http://www.gnuterrypratchett.com/> <https://news.ycombinator.com/item?id=31348217>
#
# The traditional `X-Clacks-Overhead` is hardwired to "Terry Pratchett" (which also substantially simplifies implementation).
# I generalize it to multiple people, with the obvious choice of commemorating death-date (picking randomly for tiebreakers).
#
# For efficiency, the header is hardwired into a 1-line nginx configuration file, which is called from the main nginx configuration.
# (nginx doesn't seem to have a reasonable 'native' way to do it, and shelling out to an executable like a Perl would cost
# at least 50ms.) The nginx header looks like `add_header X-Clacks-Overhead "$AUTHOR";`, and the call is
# `include /etc/nginx/conf.d/memoriam.conf;`.
# The separate memorial configuration file is simply overwritten each day by a root cron job, and nginx restarted:
# `@daily rm /etc/nginx/conf.d/memoriam.conf && /home/gwern/gwern.net/static/nginx/memoriam.sh > /etc/nginx/conf.d/memoriam.conf && systemctl reload nginx`.
#
# Thus, the header will change each day at 0 runtime cost while still being fairly straightforward to implement.
# The actual database/selection/overwriting is done by this Bash script.
# (Death-less days are handled automatically: the memorial configuration file is empty, and so the nginx inclusion simply does nothing, and no header is set.)
#
# Example:
# $ bash memoriam.sh
# add_header X-Clacks-Overhead "Claude Shannon";
#
# Requires: bash.

set -eo pipefail

# Define an associative array with MM-DD as keys and names separated by ", " as values
declare -A memorials=(
    ["01-01"]="Niklaus Wirth"
    ["01-02"]="Derek Parfit"
    ["01-04"]="T. S. Eliot"
    ["01-06"]="George R. Price"
    ["01-07"]="Richard Hamming"
    ["01-08"]="Robert Baden-Powell"
    ["01-11"]="Aaron Swartz"
    ["01-14"]="Kurt Gödel, Lewis Carroll"
    ["01-16"]="John C. Bogle"
    ["01-17"]="Francis Galton"
    ["01-19"]="Frank Ramsey"
    ["01-21"]="Terry Jones"
    ["01-22"]="Ursula K. Le Guin"
    ["01-24"]="Marvin Minsky"
    ["01-28"]="W. B. Yeats"
    ["01-31"]="A. A. Milne"
    ["02-02"]="Bertrand Russell"
    ["02-06"]="Raymond Smullyan"
    ["02-07"]="Alan Perlis"
    ["02-08"]="John von Neumann"
    ["02-09"]="Herbert A. Simon"
    ["02-11"]="Frank Herbert"
    ["02-14"]="Julian Huxley"
    ["02-15"]="Richard Feynman"
    ["02-19"]="Umberto Eco, Deng Xiaoping"
    ["02-21"]="Baruch Spinoza"
    ["02-23"]="John Keats"
    ["02-24"]="Claude Shannon"
    ["02-28"]="Freeman Dyson"
    ["02-29"]="Robert H. Brower"
    ["03-03"]="Sewall Wright"
    ["03-04"]="Gary Gygax"
    ["03-07"]="W. D. Hamilton"
    ["03-13"]="David Rumelhart"
    ["03-18"]="R. A. Lafferty"
    ["03-19"]="Richard Bellman"
    ["03-20"]="Vernor Vinge"
    ["03-23"]="Pierre-Simon Laplace"
    ["03-24"]="Gordon Moore"
    ["03-27"]="Stanisław Lem, Daniel Kahneman"
    ["04-05"]="I. J. Good, Isao Takahata"
    ["04-06"]="Isaac Asimov"
    ["04-07"]="Thomas Bayes"
    ["04-09"]="Francis Bacon"
    ["04-11"]="John Conway"
    ["04-14"]="Gene Wolfe"
    ["04-19"]="Charles Darwin, Daniel Dennett"
    ["04-27"]="Karl Pearson"
    ["04-28"]="Terry Pratchett"
    ["04-29"]="Ludwig Wittgenstein"
    ["04-30"]="E. T. Jaynes"
    ["05-05"]="Ritsuko Okazaki"
    ["05-07"]="John Stuart Mill"
    ["05-11"]="Douglas Adams"
    ["05-14"]="Walter Pitts"
    ["05-19"]="Stanislav Petrov"
    ["05-28"]="Phil Hartman"
    ["06-02"]="Alexander Shulgin"
    ["06-07"]="Alan Turing"
    ["06-14"]="Jorge Luis Borges, G. K. Chesterton"
    ["07-02"]="Douglas Engelbart"
    ["07-07"]="Arthur Conan Doyle"
    ["07-08"]="Howard Raiffa"
    ["07-11"]="Frank Rosenblatt"
    ["07-20"]="Bruno de Finetti, Neil Armstrong"
    ["07-23"]="Robert Ettinger"
    ["07-26"]="John Tukey"
    ["07-29"]="R. A. Fisher"
    ["08-11"]="Robin Williams"
    ["08-12"]="Julian C. Stanley"
    ["08-24"]="Satoshi Kon"
    ["08-25"]="David Hume"
    ["08-28"]="Hal Finney"
    ["08-30"]="Seamus Heaney, Mikhail Gorbachev"
    ["09-12"]="Norman Borlaug"
    ["09-16"]="Robert Jordan"
    ["09-20"]="J. R. R. Tolkien"
    ["09-26"]="Fujiwara no Teika"
    ["10-01"]="Robert Bakewell"
    ["10-07"]="Peter H. Rossi"
    ["10-19"]="Jonathan Swift"
    ["10-20"]="Andrey Kolmogorov"
    ["10-20"]="Richard Francis Burton"
    ["10-22"]="Arthur Jensen"
    ["10-24"]="John McCarthy"
    ["11-01"]="L. J. Savage, Yehuda Yudkowsky"
    ["11-17"]="Fred Brooks"
    ["12-07"]="Ray Solomonoff"
    ["12-26"]="Charles Babbage"
    ["12-28"]="Ian Murdock"
)
# Validate configuration data: Check for unique values and key-value pairs
declare -A value_check=()
declare -A pair_check=()
for key in "${!memorials[@]}"; do
    value="${memorials[$key]}"
    # Check for unique value
    if [[ -n "${value_check[$value]}" ]]; then
        echo "Duplicate value found: $value" >&2
        exit 1
    else
        value_check["$value"]=1
    fi
    # Check for unique key-value pair
    pair="$key: $value"
    if [[ -n "${pair_check[$pair]}" ]]; then
        echo "Duplicate key-value pair found: $pair" >&2
        exit 1
    else
        pair_check["$pair"]=1
    fi
done

today=$(TZ="America/New_York" date +%m-%d)

# Function to pick a random name from a list separated by ", "
pick_random_name() {
    # Replace ", " with newline; this assumes ", " does not appear within names themselves
    local names_string="${1//, /$'\n'}"
    # Read names into an array, splitting on newline
    IFS=$'\n' read -r -d '' -a NAMES <<< "$names_string"
    # Pick a random name from the array
    echo "${NAMES[$RANDOM % ${#NAMES[@]}]}"
}
# Check if today's date has an entry in the memorials
if [[ -v memorials[$today] ]]; then
    # If today's date has multiple names, pick one at random:
    author=$(pick_random_name "${memorials[$today]}")
    echo "add_header X-Clacks-Overhead \"$author\";"
fi
