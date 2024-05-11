#!/bin/bash

# Author: Gwern Branwen
# Date: 2016-10-01
# When:  Time-stamp: "2024-05-10 12:18:40 gwern"
# License: CC-0
#
# sync-gwern.net.sh: shell script which automates a full build and sync of Gwern.net. A full build is intricate, and requires several passes like generating link-bibliographies/tag-directories, running two kinds of syntax-highlighting, stripping cruft etc.
#
# This script automates all of that: it cleans up, compiles a hakyll binary for faster compilation,
# generates a sitemap XML file, optimizes the MathJax use, checks for many kinds of errors, uploads,
# and cleans up.

# key dependencies: GHC, Hakyll, emacs, curl, tidy (HTML5 version), git, regex-compat-tdfa (Unicode Haskell regexps), urlencode
# ('gridsite-clients' package), linkchecker, fdupes, ImageMagick, exiftool, mathjax-node-page (eg.
# `npm i -g mathjax-node-page`), parallel, xargs, php-cli, php-xml, libreoffice, gifsicle, tidy, libxml2-utils…

cd ~/wiki/
# shellcheck source=/home/gwern/wiki/static/build/bash.sh
. ./static/build/bash.sh

DEPENDENCIES=(bc curl dos2unix du elinks emacs exiftool fdupes feh ffmpeg file find firefox ghc ghci gifsicle git identify inotifywait jpegtran jq libreoffice linkchecker locate mogrify ocrmypdf pandoc parallel pdftk pdftotext php ping optipng rm rsync runghc sed tidy urlencode x-www-browser xargs xmllint static/build/anchor-checker.php static/build/generateBacklinks.hs static/build/generateDirectory.hs static/build/generateLinkBibliography.hs static/build/generateSimilarLinks.hs static/build/link-extractor.hs) # ~/src/node_modules/mathjax-node-page/bin/mjpage
DEPENDENCIES_MISSING=()
for DEP in "${DEPENDENCIES[@]}"; do
    if ! command -v "$DEP" &> /dev/null; then
        DEPENDENCIES_MISSING+=("$DEP")
    fi
done
if [ ${#DEPENDENCIES_MISSING[@]} -ne 0 ]; then
    red "Error: missing dependencies!"
    echo "$DEPENDENCIES_MISSING"
    exit 1
fi

if  [ -n "$(pgrep hakyll)" ]
then
    red "or Hakyll already running?"
else
    set -e

    # lower priority of everything we run (some of it is expensive):
    renice --priority 19 --pid "$$" &>/dev/null
    ionice --class 3     --pid "$$" &>/dev/null

    ## Parallelization: WARNING: post-2022-03 Hakyll uses parallelism which catastrophically slows down at >= # of physical cores; see <https://groups.google.com/g/hakyll/c/5_evK9wCb7M/m/3oQYlX9PAAAJ>
    N=6
    SLOW="true"
    SKIP_DIRECTORIES=""
    TODAY=$(date '+%F')

    for ARG in "$@"; do
        case "$ARG" in
            --fast) SLOW="" ;;
            --skip-directories) SKIP_DIRECTORIES="true" ;;
            *[!0-9]*) ;; # skip non-numbers
            *) N="$ARG" ;;
        esac
    done
    export SLOW SKIP_DIRECTORIES N

    if [ "$SLOW" ]; then (cd ~/wiki/ && git status) || true;
        bold "Checking metadata…"
        pkill checkMetadata || true
        rm ~/METADATA.txt &> /dev/null || true
        TMP_CHECK=$(mktemp /tmp/"XXXXX.txt"); ./static/build/checkMetadata >"$TMP_CHECK" 2>&1 && mv "$TMP_CHECK" ~/METADATA.txt || true &
    fi &
    bold "Pulling infrastructure updates…"
    # pull from Said Achmiz's repo, with his edits overriding mine in any conflict (`-Xtheirs`) & auto-merging with the default patch text (`--no-edit`), to make sure we have the latest JS/CSS. (This is a bit tricky because the use of versioning in the includes means we get a lot of merge conflicts, for some reason.)
    (cd ./static/ && git status && timeout 5m git pull -Xtheirs --no-edit --verbose 'https://gwern.obormot.net/static/.git/' master) || true

    if [ "$SLOW" ]; then

        bold "Executing string rewrite cleanups…" # automatically clean up some Gwern.net bad URL patterns, typos, inconsistencies, house-styles:
        ( alias s="gwsed"
          ## domain rewrites:
          s 'https://mobile.twitter.com' 'https://twitter.com'; s 'https://www.twitter.com' 'https://twitter.com'; s 'https://x.com/' 'https://twitter.com/'; s 'https://en.reddit.com/' 'https://www.reddit.com/'; s 'https://www.greaterwrong.com/posts/' 'https://www.lesswrong.com/posts'; s 'http://web.archive.org/web/' 'https://web.archive.org/web/'; s 'https://youtu.be/' 'https://www.youtube.com/watch?v='; s 'http://arxiv.org' 'https://arxiv.org'; s 'https://deepmind.com' 'https://www.deepmind.com'; s 'http://en.wikipedia.org' 'https://en.wikipedia.org'; s 'v1.full' '.full'; s 'v2.full' '.full'; s 'v3.full' '.full'; s 'v4.full' '.full'; s 'v5.full' '.full'; s 'v6.full' '.full'; s 'v7.full' '.full'; s 'v8.full' '.full'; s 'v9.full' '.full'; s '.full-text' '.full'; s '.full.full' '.full'; s '.full-text' '.full'; s '.full-text.full' '.full'; s '.full.full.full' '.full'; s '.full.full' '.full'; s '.gov/labs/pmc/articles/P' '.gov/pmc/articles/P';  s 'rjlipton.wpcomstaging.com' 'rjlipton.wordpress.com'; s 'www.super-memory.com' 'super-memory.com'; s 'https://www.bldgblog.com' 'https://bldgblog.com'; s 'https://www.clinicaltrials.gov' 'https://clinicaltrials.gov'; s 'https://arxiv.org/abs//' 'https://arxiv.org/abs/'; s 'http://paulgraham.com' 'https://paulgraham.com'; s 'http://www.paulgraham.com' 'https://paulgraham.com'; s "https://www.paulgraham.com" "https://paulgraham.com"; s 'https://scribe.rip' 'https://freedium.cfd';
          ## NOTE: domains which are bad or unfixable are handled by a later lint. This is only for safe rewrites.

          ## link cruft rewrites:
          s '&hl=en' ''; s '?hl=en&' '?'; s '?hl=en' ''; s '?usp=sharing' ''; s '?via%3Dihub' ''; s '.html?pagewanted=all' '.html'; s '&feature=youtu.be' ''; s ':443/' '/'; s ':80/' '/'; s '?s=r' ''; s '?s=61' ''; s '?sd=pf' ''; s '?ref=The+Browser-newsletter' ''; s '?ref=thebrowser.com' ''; s '?ignored=irrelevant' ''; s '](/docs/' '](/doc/'; s 'href="/docs/' 'href="/doc/'; s '.pdf#pdf' '.pdf'; s '#fromrss' ''; s '&amp;hl=en' ''; s '?rss=1' ''; s '/doc/statistics/decision-theory' '/doc/statistics/decision'; s '?ref=quillette.com' ''; s '?login=false' '';

          ## name/entity consistency:
          s 'EMBASE' 'Embase'; s 'Medline' 'MEDLINE'; s 'PsychINFO' 'PsycINFO'; s 'MSCOCO' 'MS COCO'; s 'Yann Le Cun' 'Yann LeCun'; s ' VQVAE' ' VQ-VAE'; s 'CIFAR 10' 'CIFAR-10'; s 'Jorges Luis Borges' 'Jorge Luis Borges'; s 'Rene Girard' 'René Girard'; s 'Anno Hideaki' 'Hideaki Anno'; s ' GPT2' ' GPT-2'; s ' Clinicaltrials.gov' ' ClinicalTrials.gov'; s ' clinicaltrials.gov' ' ClinicalTrials.gov'; s 'Dario Amodai' 'Dario Amodei'; s 'single nucleotide polymorph' 'single-nucleotide polymorph'; s 'Single Nucleotide Polymorph' 'Single-Nucleotide Polymorph'; s 'single nucleotide variant' 'single-nucleotide variant'; s ' CIFAR10' 'CIFAR-10'; s 'TyDi QA' 'TyDiQA'; s 'Türkiye' 'Turkey'; s ' Poincare' ' Poincaré'; s 'Francois de La Rochefoucauld' 'François de La Rochefoucauld'; s 'Moliere' 'Molière'; s 'behavioural genetic' 'behavioral genetic'; s ' gwern.net' ' Gwern.net'; s 'chain of thought' 'chain-of-thought'; s 'Chain Of Thought' 'Chain-Of-Thought'; s 'Chain of Thought' 'Chain-of-Thought'; s 'Chain of thought' 'Chain-of-thought'; s 'MS Marco' 'MS MARCO'; s 'MS-MARCO' 'MS MARCO'; s 'NLSY-79' 'NLSY79'; s 'NLSY-97' 'NLSY97'; s 'state of the art' 'state-of-the-art'; s 'State of the Art' 'State-of-the-Art'; s 'Enwik8' 'enwik8'; s 'G. M. Fahy' 'Gregory M. Fahy'; s 'Greg M. Fahy' 'Gregory M. Fahy'; s 'Gary Kasparov' 'Garry Kasparov'; s 'Fel D1' 'Fel D 1'; s 'Fel d1' 'Fel d 1'; s 'CIFAR10' 'CIFAR-10'; s 'ImageNet1k' 'ImageNet-1k'; s 'ImageNet21k' 'ImageNet-21k'; s ' LeGuin' ' Le Guin'; s 'DALL-E 1' 'DALL·E 1'; s 'DALL-E 2' 'DALL·E 2'; s 'DALL-E 3' 'DALL·E 3'; s 'FLAN-PALM' 'Flan-PaLM'; s 'GPT-4V' 'GPT-4-V'; s 'GPT-4 V' 'GPT-4-V'; s ' GPT4' ' GPT-4'; s 'drop cap' 'dropcap'; s 'Drop cap' 'Dropcap'; s 'Drop Cap' 'Dropcap'; s 'R.A. Fisher' 'R. A. Fisher'; s 'Larry Sumners' 'Larry Summers';

          ## abbreviation consistency:
          s '(ie,' '(ie.'; s '(ie ' '(ie. '; s 'i.e.,' 'ie.'; s 'ie., ' 'ie. '; s '(i.e.' '(ie.'; s '(eg, ' '(eg. '; s ' eg ' ' eg. '; s '(eg ' '(eg. '; s '[eg ' '[eg. '; s 'e.g. ' 'eg. '; s ' e.g. ' ' eg. '; s 'e.g.,' 'eg.'; s 'eg.,' 'eg.'; s 'E.g.,' 'Eg.'; s '(cf ' '(cf. '; s ' cf ' ' cf. '; s ' Feb ' ' February '; s ' Aug ' ' August '; s ', Jr.' ' Junior'; s ' Jr.' ' Junior'; s ', Junior' ' Junior';
          s '<sup>Th</sup>' '<sup>th</sup>'; s '<sup>St</sup>' '<sup>st</sup>'; s '<sup>Nd</sup>' '<sup>nd</sup>'; s '<sup>Rd</sup>' '<sup>rd</sup>';
          s ',”' '”,'; s ",’" "’,";

          ## spelling errors:
          s 'border colly' 'border collie'; s 'genomewide' 'genome-wide'; s 'regularise' 'regularize'; s ' residualis' ' residualiz'; s 'endelian randomisation' 'endelian randomization'; s 'mendelian randomization' 'Mendelian Randomization'; s 'Mendelian randomization' 'Mendelian Randomization'; s 'canalization' 'canalisation'; s 'Statistical significance' 'Statistical-significance'; s 'Statistical Significance' 'Statistical-Significance'; s 'statistical significance' 'statistical-significance'; s ' longstanding' ' long-standing'; s 'utilise' 'utilize'; s 'facebookok' 'facebook'; s 'Tartarian' 'Tatarian'; s 'tartarian' 'tatarian'; s ' an One' ' a One'; s ' an one' ' a one'; s '<p>he ' '<p>He '; s ' lik ' ' like '; s ' Behaviour ' ' Behavior '; s ' behaviour ' ' behavior '

          ## citation consistency:
          s ']^[' '] ^['; s 'et. al.' 'et al'; s 'et al. (' 'et al ('; s ' et al. 1'  ' et al 1'; s ' et al. 2'  ' et al 2'; s ' et al., ' ' et al '; s 'et al., ' 'et al ';
          ### WARNING: when using `+` in sed, by default, it is treated as an ordinary literal. It MUST be escaped to act as a regexp! Whereas in `grep -E`, it's the opposite. So remember: `\+` in sed, and `+` in grep.
          ### WARNING: remember that `sed -i` modifies the last-modified timestamp of all files it runs on, even when the file was not, in fact, modified!
          for file in $(find . -name "*.md" -or -name "*.gtx"); do
              if grep -qE "[A-Z][a-z]+ et al \([1-2][0-9]{3}[a-z]?\)" "$file"; then
                  sed -i -e 's/\([A-Z][a-z]\+\) et al (\([1-2][0-9][0-9][0-9][a-z]\?\))/\1 et al \2/g' "$file"
              fi

              if grep -qE "[A-Z][a-z]+ and [A-Z][a-z]+ \([1-2][0-9]{3}[a-z]?\)" "$file"; then
                  sed -i -e 's/\([A-Z][a-z]\+\) and \([A-Z][a-z]\+\) (\([1-2][0-9][0-9][0-9][a-z]\?\))/\1 \& \2 \3/g' "$file"
              fi
          done

          ## anchor errors:
          s '#allen#allen' '#allen'; s '#deepmind#deepmind' '#deepmind'; s '&org=deepmind&org=deepmind' '&org=deepmind'; s '#nvidia#nvidia' '#nvidia'; s '#openai#openai' '#openai'; s '#google#google' '#google'; s '#uber#uber' '#uber';

          ## HTML/Markdown formatting:
          s '<p> ' '<p>'; s ' _n_s' ' <em>n</em>s'; s ' (n = ' ' (<em>n</em> = '; s ' (N = ' ' (<em>n</em> = '; s ' de novo ' ' <em>de novo</em> '; s ' De Novo ' ' <em>De Novo</em> '; s 'backlinks-not' 'backlink-not'; s ',</a>' '</a>,'; s ':</a> ' '</a>: '; s ';</a>' '</a>;'; s ' <<a href' ' <a href'; s '_X_s' '<em>X</em>s'; s ' _r_s' ' <em>r</em>s'; s '<em ' '<em>'; s '# External links' '# External Links'; s '# See also' '# See Also'; s '"abstract-collapse abstract"' '"abstract abstract-collapse"'; s "‐" "-"; s 'class="link-auto"' ''; s '𝑂(' '𝒪('; s '</strong> and <strong>' '</strong> & <strong>'; s '<Sub>' '<sub>'; s '<Sup>' '<sup>';
          s 'augmentation,</a>' 'augmentation</a>,'; s 'Bitcoin,</a>' 'Bitcoin</a>,'; s 'class="invertible"' 'class="invert"'; s '”&gt;' '">'; s '<br/>' '<br />'; s '<br>' '<br />'; s ' id="cb1"' ''; s ' id="cb2"' ''; s ' id="cb3"' ''; s ' id="cb4"' '';
          s '.svg-530px.jpg' '.svg'; s ' (”' ' (“'; s '<A Href' '<a href'; s '</a>’s' '’s</a>'; s '-530px.jpg' ''; s '-768px.png' ''; s '-768px.jpg' ''; s '—-' '—'; s 'collapse-summary' 'abstract-collapse'; s 'collapse-abstract' 'abstract-collapse';
          s 'href="ttp' 'href="http'; s '\xmlpi{\\}' ''; s '°C' '℃'; s '° C' '℃'; s '°F' '℉'; s '° F' '℉'; s '℉ahrenheit' '℉'; s '℃elsius' '℃'; s ' ℃' '℃'; s ' ℉' '℉'; s 'marginnnote' 'marginnote'; s ' <br /></li>' '</li>';
          s ' <br /> </li>' '</li>'; s '<psna ' '<span '; s '……' '…'; s '</strong>::' '</strong>:'; s '](//' '[(/'; s '{.full-width' '{.width-full'; s '<div class="admonition">' '<div class="admonition note">'; s '](/home/gwern/wiki/' '](/';
          s '<a href="/home/gwern/wiki/' '<a href="/'; s '.png.png' '.png'; s '.jpg.jpg' '.jpg'; s '.’</p>' '’.</p>'; s 'Cite-Author' 'cite-author'; s 'Cite-Date' 'cite-date'; s 'Cite-Joiner' 'cite-joiner'; s 'class="Cite' 'class="cite'; s 'Logotype-Tex' 'logotype-tex'; s '</p></p>' '</p>'; s '’ ”' '’ ”'; s ' ”' ' “';
          s '[("doi","")]' ''; s '>/a>' '</a>'; s 'href="W!"' 'href="!W"'; s 'class="Logotype-Tex"' 'class="logotype-tex"'; s 'Class="Logotype-Tex"' 'class="logotype-tex"'; s '<span Class="' '<span class="';
          ## TODO: duplicate HTML classes from Pandoc reported as issue #8705 & fixed; fix should be in >pandoc 3.1.1 (2023-03-05), so can remove these two rewrites once I upgrade past that:
          s 'class="odd odd' 'class="odd'; s 'class="even even' 'class="even';
          s '  ' ' '; s '​ ' ' ';
        ) &> /dev/null &
    sed -i -e 's/ data-link-?[Tt]ags="[a-z0-9 \/-]\+">/>/' ./metadata/*.gtx;
    fi

    bold "Compiling…"
    cd ./static/build
    WARNINGS=""
    if [ "$SLOW" ]; then WARNINGS="-Wall"; fi # TODO: make -Werror clean for all the new GHC incomplete-pattern warnings
    compile () { ghc -O2 $WARNINGS -rtsopts -threaded --make "$@"; }
    compile hakyll.hs
    if [ -z "$SKIP_DIRECTORIES" ]; then
        compile generateLinkBibliography.hs
        compile generateDirectory.hs; fi
    compile preprocess-markdown.hs
    compile guessTag.hs &
    compile changeTag.hs &
    compile checkMetadata.hs &
    ## NOTE: generateSimilarLinks.hs & link-suggester.hs are done at midnight by a cron job because
    ## they are too slow to run during a regular site build & don't need to be super-up-to-date
    ## anyway
    cd ../../
    # cleanup:
    rm --recursive --force -- ./_cache/ ./_site/

  if [ "$SLOW" ]; then
    bold "Checking embeddings database…"
    ghci -istatic/build/ ./static/build/GenerateSimilar.hs  -e 'e <- readEmbeddings' &>/dev/null

    # duplicates a later check but if we have a fatal link error, we'd rather find out now rather than 30 minutes later while generating annotations:
    λ(){ gf -e 'href=""' -e 'href="!W"></a>' -e "href='!W'></a>" -- ./metadata/*.gtx || true; }
    wrap λ "Malformed empty link in annotations?"

    # another early fatal check: if there is a Markdown file 'foo.md' and also a subdirectory 'foo/' in the same directory, then this will result in, later, a fatal error when one tries to compile 'foo.md' → 'foo' (the HTML file) but 'foo' (the directory) already exists.
    # Check if any files collide with directories of the same name (without the .md extension).
    # Usage: find_colliding_files [path]
    function find_colliding_files() { # GPT-3 written:
      set -euo pipefail
      path="${1:-.}"
      find "$path" -depth -type f -name "*.md" -exec sh -c '
        for file do
          path="$(dirname "$file")/$(basename "$file" ".md")"
          if [ -e "$path" ] && [ ! -L "$path" ]; then
            if [ -d "$path" ]; then
              printf "Fatal error: Directory exists with the same name as file %s\n" "$file" >&2
              exit 1
            else
              printf "Fatal error: File exists with the same name as file %s\n" "$file" >&2
              exit 1
            fi
          fi
        done' sh {} +
    }
    find_colliding_files ./

    # We update the linkSuggestions.el in a cron job because too expensive, and vastly slows down build.

    # Update the directory listing index pages: there are a number of directories we want to avoid,
    # like the various mirrors or JS projects, or directories just of data like CSVs, or dumps of
    # docs, so we'll blacklist those:
    DIRECTORY_TAGS="$(find doc/ fiction/ haskell/ newsletter/ nootropic/ note/ review/ zeo/ -type d \
                      | gfv -e 'doc/www' -e 'doc/rotten.com' -e 'doc/genetics/selection/www.mountimprobable.com' \
                                        -e 'doc/biology/2000-iapac-norvir' -e 'doc/gwern.net-gitstats' -e 'doc/reinforcement-learning/armstrong-controlproblem' \
                                        -e 'doc/statistics/order/beanmachine-multistage' -e 'doc/personal/2011-gwern-yourmorals.org/' \
                                        -e 'confidential/' -e 'private/' -e 'secret/' -e 'newest/')"

    if [ -z "$SKIP_DIRECTORIES" ]; then
        bold "Writing missing annotations to support link-bibliography/tag-directory updates…"
        # We add new annotations daily, but all the code in link-bib/tag-directory deal with only the current annotations which have been written out to disk as HTML snippets; thus, since that is done in the main compilation phase, the default would be that annotations would be omitted the first day and only appear the next time. This is annoying and manually working around it is even more tedious, so we provide a 'one-shot' missing-annotation mode and call that phase immediately before the lb/tag phase:
        ./static/build/hakyll build +RTS -N"$N" -RTS --annotation-missing-one-shot ; ./static/build/hakyll build clean
        bold "Updating link bibliographies…"
        ./static/build/generateLinkBibliography +RTS -N"$N" -RTS || true

        # we want to generate all directories first before running Hakyll in case a new tag was created
        bold "Building directory indexes…"
        ./static/build/generateDirectory +RTS -N3 -RTS $DIRECTORY_TAGS
    fi
  fi

    bold "Check & update VCS…"
    (ping -q -c 5 google.com &> /dev/null && cd ./static/ && git status; git pull; git push --verbose &) || true

    # Cleanup pre:
    rm --recursive --force ./static/build/*.o ./static/build/*.hi ./static/build/generateDirectory ./static/build/generateLinkBibliography ./static/build/generateBacklinks || true

    cd ~/wiki/ # go to site root
    bold "Building site…"

    # make sure all videos have 'poster' preview images:
    for VIDEO in $(find . -type f -name "*.mp4" -or -name "*.webm" -or -name "*.avi" | gfv "doc/www/"); do # we skip posters for videos in /doc/www/* archives from split archives because nothing sets a poster on them, so just a waste of space
        POSTER="$VIDEO-poster.jpg"; if [ ! -f "$POSTER" ]; then
                                        echo "Generating poster image for $VIDEO…"
                                        # Problem: embedded videos (e.g. https://gwern.net/lorem-multimedia#video ) all look like generic small black rectangles. User has no idea what it is until they click to begin download the (possibly huge) video file. This also causes layout shift as the `<video>` element expands to the proper size of the video.
                                        #
                                        # We would like user to see, on load, preview of video: a single frame, displayed as still image (usually first frame is fine). (Even if the first frame or whatever frame we choose is uninformative, in any case this solves the layout shift problem.)
                                        #
                                        # Solution: the `poster="foo.jpg"` attribute of the HTML <video> element. Its value is URL of image file, to be shown as preview (and used for layout sizing of video element), prior to the user clicking to download and play video. Example:
                                        #
                                        # <video controls="controls" preload="none" loop="" poster="/doc/ai/nn/gan/biggan/2019-06-03-gwern-biggan-danbooru1k-256px.mp4.png"><source src="/doc/ai/nn/gan/biggan/2019-06-03-gwern-biggan-danbooru1k-256px.mp4" type="video/mp4"></video>
                                        #
                                        # How to generate poster image file? Use ffmpeg:
                                        #
                                        # ffmpeg -i 2019-06-03-gwern-biggan-danbooru1k-256px.mp4 -vf "select=eq(n\,0),scale=iw*sar:ih,setsar=1" -vframes 1 2019-06-03-gwern-biggan-danbooru1k-256px.mp4.png
                                        #
                                        # (A frame other than the first can be extracted by putting in a higher number in the `select=eq(n\,0)` part of the command, e.g. `select=eq(n\,99)` would give the 100th frame.) We then heavily compress them quality-wise (they're just placeholders).
                                        # Solved? No. Because posters are still reasonably large (eg. /face could load >1MB of posters because it has so many videos in it!), and because `<img>` lazy-loading doesn't apply to posters (due to poorly-conceived standards <https://github.com/whatwg/html/issues/6636> which short-sightedly implemented `poster=` as a hack without thinking about how this is reinventing `<img>` badly), we have a problem with bandwidth usage. So, we don't actually use `poster=` per se, we instead use a data attribute `data-video-poster` to store it, and hand-implement lazy-loading with IntersectionObservers /sigh).
                                        ffmpeg -i "$VIDEO" -vf "select=eq(n\\,5),scale=iw*sar:ih,setsar=1" -vframes 1 "$POSTER";
                                        mogrify -quality 15 "$POSTER"
                                    fi;
    done &

    time ./static/build/hakyll build +RTS -N"$N" -RTS || (red "Hakyll errored out!"; exit 1)

    if [ "$SLOW" ]; then

        bold "Updating X-of-the-day…"
        bold "Updating annotation-of-the-day…"
        ghci -istatic/build/ ./static/build/XOfTheDay.hs -e 'do {md <- LinkMetadata.readLinkMetadata; aotd md; }' | \
            gfv -e ' secs,' -e 'it :: [T.Text]' -e '[]' &

        bold "Updating quote-of-the-day…"
        ghci -istatic/build/ ./static/build/XOfTheDay.hs -e 'qotd' | \
            gfv -e ' secs,' -e 'it :: [T.Text]' -e '[]' &

        bold "Updating site-of-the-day…"
        ghci -istatic/build/ ./static/build/XOfTheDay.hs -e 'sotd' | \
            gfv -e ' secs,' -e 'it :: [T.Text]' -e '[]' &
    fi

    bold "Results size:"
    du -chs ./_cache/ ./_site/
    echo "Raw file count: $(find ./_site/ -type f | wc --lines)"
    echo "Total (including hardlinks) file count: $(find ./_site/ -type f -or -type l | wc --lines)"

    # cleanup post:
    rm -- ./static/build/hakyll ./static/build/*.o ./static/build/*.hi ./static/build/generateDirectory ./static/build/generateLinkBibliography ./static/build/generateBacklinks ./static/build/link-extractor &>/dev/null || true

    ## WARNING: this is a crazy hack to insert a horizontal rule 'in between' the first 3 sections
    ## on /index (Newest/Popular/Notable), and the rest (starting with Statistics); the CSS for
    ## making the rule a block dividing the two halves just doesn't work in any other way, but
    ## Pandoc Markdown doesn't let you write stuff 'in between' sections, either. So… a hack.
    sed -i -e 's/section id=\"statistics\"/hr class="horizontal-rule-nth-1" \/> <section id="statistics"/' ./_site/index

    bold "Building sitemap.xml…"
    ## generate a sitemap file for search engines:
    ## NOTE: we generate the sitemap *before* generating syntax-highlighted .html files of everything to avoid having to exclude those (which would be tricky because how do we know if any given 'foo.html' a standalone HTML file or merely a syntax-highlighted snippet?)
    (echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?> <urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
     ## very static files which rarely change: PDFs, images, site infrastructure:
     find -L _site/doc/ _site/ _site/static/ -not -name "*.md" -type f | gfv -e 'doc/www/' -e 'metadata/' -e '.git' -e '404' -e '/static/template/default.html' -e 'lorem' | gev -e '/doc/.*/index' -e 'static/.*\..*\.html$' -e 'doc/.*\..*\.html$' -e '.hi$' -e '.o$' | \
         xargs urlencode -m | sed -e 's/%20/\n/g' | \
         sed -e 's/_site\/\(.*\)/\<url\>\<loc\>https:\/\/gwern\.net\/\1<\/loc><changefreq>never<\/changefreq><\/url>/'
     ## Everything else changes once in a while:
     find -L _site/ -not -name "*.md" -type f | gfv -e 'static/' -e 'doc/' -e 'fulltext' -e 'lorem' -e 'metadata/' -e '.md.html' -e 'private/' | \
         gev -e '/.*/index' -e '.md$' | \
         xargs urlencode -m | sed -e 's/%20/\n/g' | \
         sed -e 's/_site\/\(.*\)/\<url\>\<loc\>https:\/\/gwern\.net\/\1<\/loc><changefreq>monthly<\/changefreq><\/url>/'
     echo "</urlset>") >> ./_site/sitemap.xml

    # For some document types, Pandoc doesn't support them, or syntax-highlighting wouldn't be too useful for preview popups. So we use LibreOffice to convert them to HTML.
    # <https://en.wikipedia.org/wiki/LibreOffice#Supported_file_formats>
    convert_to_html() {
        local EMBED_IMAGES="$1"
        shift # Remove the first argument to process remaining arguments
        if [[ -f "$1" ]]; then
            local FILE="$1"
            convert_file "$FILE"
        else
            local FILE_EXTS=("$@") # Remaining arguments are file extensions
            find ./doc/ -type f \( $(printf -- "-name *.%s -o " "${FILE_EXTS[@]}" | sed 's/ -o $//') \) | while read -r FILE; do
                convert_file "$FILE"
            done
        fi
    }
    convert_file() {
        local FILE="$1"
        if [ ! -f "_site/${FILE}.html" ]; then
            local TARGET
            TARGET="$(basename "$FILE")"
            local CONVERSION_OPTION="html"
            if [[ "$EMBED_IMAGES" == "true" ]]; then
                CONVERSION_OPTION="html:HTML:EmbedImages"
            fi
            timeout 5m libreoffice --headless --convert-to $CONVERSION_OPTION "$FILE" >/dev/null 2>&1 || echo "$FILE failed LibreOffice conversion?";
            mv "${TARGET%.*}.html" "_site/${FILE}.html" || echo "$FILE failed LibreOffice conversion?";
        fi
    }

    # Convert documents with embedded images
    # WARNING: LibreOffice seems to have race-conditions and can't convert >1 files at a time reliably, with sporadic failures or even a GUI popup error dialogue!
    # HACK: Libreoffice for some reason fails if you specify the 'HTML:EmbedImages' on a spreadsheet file, even though that obviously can't be a problem (it works fine on other documents, and spreadsheets don't have images to embed, so why is it a fatal error‽), and also Libreoffice lies about the error, exiting with success, so you can't simply try a second time with the `EmbedImages` removed...
    convert_to_html "true" "doc" "docx" # document
    # Convert spreadsheets without embedded images
    # Note: Specifying 'HTML:EmbedImages' for spreadsheets leads to failures despite being unnecessary.
    convert_to_html "false" "csv" "ods" "xls" "xlsx" && mv ./2010-nordhaus-nordhaus2007twocenturiesofproductivitygrowthincomputing-appendix_html*.png ./_site/doc/cs/hardware/ & # spreadsheet
    # NOTE: special-case: *very* complex multi-sheet spreadsheet with many images; HACK: LibreOffice also appears to ignore the embed option anyway, so we copy the images manually
    # convert_to_html "false" "./doc/cs/hardware/2010-nordhaus-nordhaus2007twocenturiesofproductivitygrowthincomputing-appendix.xlsx"

    wait
    set -e

    ## generate a syntax-highlighted HTML fragment (not whole standalone page) version of source code files for popup usage:
    ### We skip full conversion of .json/.jsonl because they are too large & Pandoc will choke; and we truncate at 1000 lines because such
    ### long source files are not readable as popups and their complexity makes browsers choke while rendering them.
    ### (We include plain text files in this in order to get truncated versions of them.)
    ###
    #### NOTE: for each new extension, add a `find` name, and an entry in `content.js`/`LinkMetadata.is{Doc,Code}Viewable`
    bold "Generating syntax-highlighted versions of source code files…"
    syntaxHighlight () {
        declare -A extensionToLanguage=( ["R"]="R" ["c"]="C" ["py"]="Python" ["css"]="CSS" ["hs"]="Haskell" ["js"]="Javascript" ["patch"]="Diff" ["diff"]="Diff" ["sh"]="Bash" ["bash"]="Bash" ["html"]="HTML" ["conf"]="Bash" ["php"]="PHP" ["opml"]="Xml" ["xml"]="Xml" ["md"]="Markdown"
                                         # NOTE: we do 'text' to get a 'syntax-highlighted' version which has wrapped columns etc.
                                         # NOTE: CSV is unsupported by Pandoc skylighting, so we convert to HTML instead
                                         ["txt"]="default" ["jsonl"]="JSON" ["json"]="JSON" )
        LENGTH="2000"
        for FILE in "$@"; do

            FILEORIGINAL=$(echo "$FILE" | sed -e 's/_site//')
            FILENAME=$(basename -- "$FILE")
            EXTENSION="${FILENAME##*.}"
            LANGUAGE=${extensionToLanguage[$EXTENSION]}
            FILELENGTH=$(cat "$FILE" | wc --lines)
            (echo -e "~~~~~~~~~~~~~~~~~~~~~{.$LANGUAGE}"; # NOTE: excessively long tilde-line is necessary to override/escape any tilde-blocks inside Markdown files: <https://pandoc.org/MANUAL.html#fenced-code-blocks>
            if [ $EXTENSION == "md" ] || [ $EXTENSION == "txt" ] ; then # the very long lines look bad in narrow popups, so we fold:
                cat "$FILE" | fold --spaces --width=70 | sed -e 's/~~~/∼∼∼/g' | head "-$LENGTH";
            else
                cat "$FILE" | head "-$LENGTH";
            fi
             echo -e "\n~~~~~~~~~~~~~~~~~~~~~"
             if (( $FILELENGTH >= "$LENGTH" )); then echo -e "\n\n…[File truncated due to length; see <a class=\"link-page\" href=\"$FILEORIGINAL\">original file</a>]…"; fi;
            ) | iconv -t utf8 -c | pandoc --from=markdown+smart --write=html5 --standalone \
                       --template=./static/template/pandoc/sourcecode.html5 \
                       --metadata title="$(echo $FILE | sed -e 's/_site\///g')"  | \
                ## delete annoying self-link links: Pandoc/skylighting doesn't make this configurable
                sed -e 's/<span id="cb[0-9]\+-[0-9]\+"><a href="#cb[0-9]\+-[0-9]\+" aria-hidden="true" tabindex="-1"><\/a>/<span>/' -e 's/id="mathjax-styles" type="text\/css"/id="mathjax-styles"/' >> $FILE.html || red "Pandoc syntax-highlighting failed on: $FILE $FILEORIGINAL $FILENAME $EXTENSION $LANGUAGE $FILELENGTH"
        done
    }
    export -f syntaxHighlight
    set +e
    find _site/static/ -type f,l -name "*.html" | parallel --jobs "$N" syntaxHighlight # NOTE: run .html first to avoid duplicate files like 'foo.js.html.html'
    find _site/ -type f,l \
         -name "*.R" -or -name "*.c" -or -name "*.css" -or -name "*.hs" -or -name "*.js" -or -name "*.patch" -or -name "*.diff" -or -name "*.py" -or -name "*.sh" -or -name "*.bash" -or -name "*.php" -or -name "*.conf" -or -name "*.opml" -or -name "*.md" -or -name "*.txt" -or -name "*.json" -or -name "*.jsonl" -or -name "*.gtx" -or -name "*.xml" | \
        gfv \
                 `# Pandoc fails on embedded Unicode/regexps in JQuery` \
                 -e 'mountimprobable.com/assets/app.js' -e 'jquery.min.js' -e 'index.md' \
                 -e 'metadata/backlinks.hs' -e 'metadata/embeddings.bin' -e 'metadata/archive.hs' -e 'doc/www/' -e 'sitemap.xml' | parallel --jobs "$N" syntaxHighlight

    ## Pandoc/Skylighting by default adds empty self-links to line-numbered code blocks to make them clickable (as opposed to just setting a span ID, which it also does). These links *would* be hidden except that self links get marked up with up/down arrows, so arrows decorate the code-blocks. We have no use for them and Pandoc/skylighting has no option or way to disable them, so we strip them.
    bold "Stripping self-links from syntax-highlighted HTML…"
    cleanCodeblockSelflinks () {
        if [[ $(gf -e 'class="sourceCode' "$@") ]]; then
            sed -i -e 's/<a href="\#cb[0-9]\+-[0-9]\+" aria-hidden="true" tabindex="-1"><\/a>//g' -e 's/<a href="\#cb[0-9]\+-[0-9]\+" aria-hidden="true" tabindex="-1" \/>//g' -- "$@";
        fi
    }
    export -f cleanCodeblockSelflinks
    find ./ -path ./_site -prune -type f -o -name "*.md" | gfv -e 'doc/www/' -e '#' | sed -e 's/\.md$//' -e 's/\.\/\(.*\)/_site\/\1/' | parallel --max-args=500 cleanCodeblockSelflinks || true

    bold "Reformatting HTML sources to look nicer using HTML Tidy…"
    # WARNING: HTML Tidy breaks the static-compiled MathJax. One of Tidy's passes breaks the mjpage-generated CSS (messes with 'center', among other things). So we do Tidy *before* the MathJax.
    # WARNING: HTML Tidy by default will wrap & add newlines for cleaner HTML in ways which don't show up in rendered HTML - *except* for when something is an 'inline-block', then the added newlines *will* show up, as excess spaces. <https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Whitespace#spaces_in_between_inline_and_inline-block_elements> <https://patrickbrosset.medium.com/when-does-white-space-matter-in-html-b90e8a7cdd33> And we use inline-blocks for the #page-metadata block, so naive HTML Tidy use will lead to the links in it having a clear visible prefixed space. We disable wrapping entirely by setting `-wrap 0` to avoid that.
    tidyUpFragment () { tidy -indent -wrap 0 --merge-divs no --break-before-br yes --logical-emphasis yes -quiet --show-warnings no --show-body-only yes --fix-style-tags no --drop-empty-elements no -modify "$@"; }
    ## tidy wants to dump whole well-formed HTML pages, not fragments to transclude, so switch.
    tidyUpWhole () {    tidy -indent -wrap 0 --merge-divs no --break-before-br yes --logical-emphasis yes -quiet --show-warnings no --show-body-only no --fix-style-tags no --drop-empty-elements no -modify "$@"; }
    export -f tidyUpFragment tidyUpWhole
    find ./_site/metadata/annotation/ -type f -name "*.html" | parallel --max-args=250 tidyUpFragment
    find ./ -type f -name "*.md" | sed -e 's/\.md$//' -e 's/\.\/\(.*\)/_site\/\1/' | gfv -e '#' -e 'Death-Note-script' | parallel --max-args=250 tidyUpWhole

    ## use https://github.com/pkra/mathjax-node-page/ to statically compile the MathJax rendering of the MathML to display math instantly on page load
    ## background: https://joashc.github.io/posts/2015-09-14-prerender-mathjax.html installation: `npm install --prefix ~/src/ mathjax-node-page`
    bold "Compiling LaTeX JS+HTML into static CSS+HTML…"
    staticCompileMathJax () {
        if [[ $(gf -e '<span class="math inline"' -e '<span class="math display"' "$@") ]]; then
            TARGET=$(mktemp /tmp/XXXXXXX.html)
            cat "$@" | ~/src/mathjax-node-page/bin/mjpage --output CommonHTML --fontURL '/static/font/mathjax' | \
            ## WARNING: experimental CSS optimization: can't figure out where MathJax generates its CSS which is compiled,
            ## but it potentially blocks rendering without a 'font-display: swap;' parameter (which is perfectly safe since the user won't see any math early on)
                sed -e 's/^\@font-face {/\@font-face {font-display: swap; /' \
                    -e 's/<style type="text\/css">\.mjx-chtml/<style id="mathjax-styles" type="text\/css">.mjx-chtml/' >> "$TARGET";

            if [[ -s "$TARGET" ]]; then
                mv "$TARGET" "$@" && echo "$@ succeeded";
            else red "$@ failed MathJax compilation";
            fi
        fi
    }
    export -f staticCompileMathJax
    (find ./ -path ./_site -prune -type f -o -name "*.md" | gfv -e 'doc/www/' -e '#' | sed -e 's/\.md$//' -e 's/\.\/\(.*\)/_site\/\1/';
     find _site/metadata/annotation/ -name '*.html') | \
        parallel --jobs "$N" --max-args=1 staticCompileMathJax

    # essays only:
    ## eg. './2012-election.md \n...\n ./doc/cs/cryptography/1955-nash.md \n...\n ./newsletter/2022/09.md \n...\n ./review/mcnamara.md \n...\n ./wikipedia-and-knol.md \n...\n ./zeo/zma.md'
    PAGES="$(find . -type f -name "*.md" | gfv -e '_site/' -e 'index' -e '#' | sort --unique)"
    # essays+tags+annotations+similars+backlinks:
    # eg. "_site/2012-election _site/2014-spirulina _site/3-grenades ... _site/doc/ai/text-style-transfer/index ... _site/doc/anime/2010-sarrazin ... _site/fiction/erl-king ... _site/lorem-admonition ... _site/newsletter/2013/12 ... _site/note/attention ... _site/review/umineko ... _site/zeo/zma"
    PAGES_ALL="$(find ./ -type f -name "*.md" | gfv -e '_site' -e '#' | sed -e 's/\.md$//' -e 's/\.\/\(.*\)/_site\/\1/') $(find _site/metadata/annotation/ -type f -name '*.html')"

    # 1. turn "As per Foo et al 2020, we can see." → "<p>As per Foo et al 2020, we can see.</p>" (&nbsp;); likewise for 'Foo 2020' or 'Foo & Bar 2020'
    # 2. add non-breaking character to punctuation after links to avoid issues with links like '[Foo](/bar);' where ';' gets broken onto the next line (this doesn't happen in regular text, but only after links, so I guess browsers have that builtin but only for regular text handling?), (U+2060 WORD JOINER (HTML &#8288; · &NoBreak; · WJ))
    # 3. add hair space ( U+200A   HAIR SPACE (HTML &#8202; · &hairsp;)) in slash-separated links or quotes, to avoid overlap of '/' with curly-quote
                               # -e 's/\([a-zA-Z‘’-]\)[   ]et[   ]al[   ]\([1-2][0-9][0-9a-z]\+\)/\1 <span class="etal"><span class="etalMarker">et al<\/span> <span class="etalYear">\2<\/span><\/span>/g' \
                           # -e 's/\([A-Z][a-zA-Z]\+\)[   ]\&[   ]\([A-Z][a-zA-Z]\+\)[   ]\([1-2][0-9][0-9a-z]\+\)/\1 \& \2 <span class="etalYear">\3<\/span>/g' \
    # bold "Adding non-breaking spaces…"
    # nonbreakSpace () { sed -i -e 's/\([a-zA-Z]\) et al \([1-2]\)/\1 et al \2/g' \
    #                           -e 's/\([A-Z][a-zA-Z]\+\) \([1-2]\)/\1 \2/g' \
    #                           `# "Foo & Quux 2020" Markdown → "Foo &amp; Quux 2020" HTML` \
    #                           -e 's/\([A-Z][a-zA-Z]\+\)[  ]\&amp\;[  ]\([A-Z][a-zA-Z]\+\)[  ]\([1-2][1-2][1-2][1-2]\)/\1 \&amp\;_\2 \3/g' \
    #                           `# "Foo & Quux 2020" Markdown → "Foo &amp; Quux&emsp14;2020" HTML` \
    #                           -e 's/\([A-Z][a-zA-Z]\+\) \&amp\; \([A-Z][a-zA-Z]\+\)\&emsp14\;\([1-2][1-2][1-2][1-2]\)/\1 \&amp\;_\2\&emsp14\;\3/g' \
    #                           -e 's/<\/a>;/<\/a>\⁠;/g' -e 's/<\/a>,/<\/a>\⁠,/g' -e 's/<\/a>\./<\/a>\⁠./g' -e 's/<\/a>\//<\/a>\⁠\/ /g' \
    #                           -e 's/\/<wbr><a /\/ <a /g' -e 's/\/<wbr>"/\/ "/g' \
    #                           -e 's/\([a-z]\)…\([0-9]\)/\1⁠…⁠\2/g' -e 's/\([a-z]\)…<sub>\([0-9]\)/\1⁠…⁠<sub>\2/g' -e 's/\([a-z]\)<sub>…\([0-9]\)/\1⁠<sub>…⁠\2/g' -e 's/\([a-z]\)<sub>…<\/sub>\([0-9]\)/\1⁠<sub>…⁠<\/sub>\2/g' \
    #                           -e 's/\([a-z]\)⋯\([0-9]\)/\1⁠⋯⁠\2/g' -e 's/\([a-z]\)⋯<sub>\([0-9]\)/\1⁠⋯⁠<sub>\2/g' \
    #                           -e 's/\([a-z]\)⋱<sub>\([0-9]\)/\1⁠⋱⁠<sub>\2/g' -e 's/\([a-z]\)<sub>⋱\([0-9]\)/\1<sub>⁠⋱⁠\2/g' \
    #                           -e 's/ \+/ /g' -e 's/​​\+/​/g' -e 's/​ ​​ ​\+/​ /g' -e 's/​ ​\+/ /g' -e 's/​ ​ ​ \+/ /g' -e 's/​ ​ ​ \+/ /g' -e 's/  / /g' -e 's/​ ​​ \+​/ /g' \
    #                           `# add HAIR SPACE to parenthetical links to avoid biting of the open-parenthesis (eg '( <a href="https://tvtropes.org...">TvTropes</a>)'); note that formatting can be *outside* the <a> as well as *inside*: ` \
    #                           -e 's/ (<a / ( <a /g' -e 's/ (<strong><a / ( <strong><a /g' -e 's/ (<em><a / ( <em><a /g' -e 's/ (<span class="smallcaps"><a / ( <span class="smallcaps"><a /g' \
    #                           `# and similarly, '[foo](http)/[bar](http)' bites the '/':` \
    #                           -e 's/<\/a>\/<a /<\/a> \/ <a /g' \
    #                           -e 's/““/“ “/g' -e 's/””/” ”/g' \
    #                           `# Big O notation: '𝒪(n)' in some browsers like my Chromium will touch the O/parenthesis (particularly noticeable in /Problem-14's abstract), so add a THIN SPACE (HAIR SPACE is not enough for the highly-tilted italic):` \
    #                           -e 's/𝒪(/𝒪 (/g' \
    #                         "$@"; }; export -f nonbreakSpace;
    # find ./ -path ./_site -prune -type f -o -name "*.md" | gfv -e '#' | sed -e 's/\.md$//' -e 's/\.\/\(.*\)/_site\/\1/' | parallel --max-args=500 nonbreakSpace || true
    # find ./_site/metadata/annotation/ -type f -name "*.html" | parallel --max-args=500 nonbreakSpace || true

    bold "Adding #footnotes section ID…" # Pandoc bug; see <https://github.com/jgm/pandoc/issues/8043>; fixed in <https://github.com/jgm/pandoc/commit/50c9848c34d220a2c834750c3d28f7c94e8b94a0>, presumably will be fixed in Pandoc >2.18
    footnotesIDAdd () { sed -i -e 's/<section class="footnotes footnotes-end-of-document" role="doc-endnotes">/<section class="footnotes" role="doc-endnotes" id="footnotes">/' "$@"; }; export -f footnotesIDAdd
    echo "$PAGES" | sed -e 's/\.md$//' -e 's/\.\/\(.*\)/_site\/\1/' | parallel --max-args=500 footnotesIDAdd || true

    bold "Stripping compile-time-only classes unnecessary at runtime…"
    cleanClasses () {
        sed -i -e 's/class=\"\(.*\)archive-not \?/class="\1/g' \
               -e 's/class=\"\(.*\)id-not \?/class="\1/g' \
               `# TODO: revert 9f246da03503b3c20d3c38eecd235b5aa7caa0b3 and remove .backlink-not clutter once all link-ID problems are resolved` \
               -e 's/class=\"\(.*\)link-annotated-not \?/class="\1/g' \
               -e 's/class=\"\(.*\)link-auto \?/class="\1/g' \
               -e 's/class=\"\(.*\)link-live-not \?/class="\1/g' \
               -e 's/class=\"\(.*\)link-modified-recently-not \?/class="\1/g' \
    "$@"; }; export -f cleanClasses
    echo "$PAGES" | sed -e 's/\.md$//' -e 's/\.\/\(.*\)/_site\/\1/' | parallel --max-args=500 cleanClasses || true
    # TODO: rewriting in place doesn't work because of the symbolic links. need to copy ./metadata/ instead of symlinking?
    find ./_site/metadata/ -type f -name "*.html" | parallel --max-args=500 cleanClasses || true

  if [ "$SLOW" ]; then
    # Testing compilation results:
    set +e

    λ(){
         echo "$PAGES_ALL" | xargs grep -F -l --color=always -e '<span class="math inline">' -e '<span class="math display">' -e '<span class="mjpage">' | \
                                     gfv -e '/1955-nash' -e '/backstop' -e '/death-note-anonymity' -e '/difference' \
                                                          -e '/lorem' -e '/modus' -e '/order-statistics' -e '/conscientiousness-and-online-education' \
                                -e 'doc%2Fmath%2Fhumor%2F2001-borwein.pdf' -e 'statistical_paradises_and_paradoxes.pdf' -e '1959-shannon.pdf' \
                                -e '/math-error' -e '/replication' \
                                -e 'performance-pay-nobel.html' -e '/coin-flip' \
                                -e '/nootropic/magnesium' -e '/selection' -e 'doc/statistics/bayes/1994-falk' -e '/zeo/zeo' \
                                -e '/mail-delivery' \
                                -e '/variable' -e '1400861560180858880' \
                                -e 'w28340%2Fw28340.pdf';
       }
    wrap λ "Warning: unauthorized LaTeX users somewhere"

    λ(){ VISIBLE_N=$(cat ./_site/sitemap.xml | wc --lines); [ "$VISIBLE_N" -le 36000 ] && echo "$VISIBLE_N" && exit 1; }
    wrap λ "Sanity-check number-of-public-site-files in sitemap.xml failed"

    λ(){ COMPILED_N="$(find -L ./_site/ -type f | wc --lines)"
         [ "$COMPILED_N" -le 115000 ] && echo "File count: $COMPILED_N" && exit 1;
         COMPILED_BYTES="$(du --summarize --total --dereference --bytes ./_site/ | tail --lines=1 | cut --field=1)"
         [ "$COMPILED_BYTES" -le 100000000000 ] && echo "Total filesize: $COMPILED_BYTES" && exit 1; }
    wrap λ "Sanity-check: number of files & file-size too small?"

    λ(){ SUGGESTIONS_N=$(cat ./metadata/linkSuggestions.el | wc --lines); [ "$SUGGESTIONS_N" -le 22000 ] && echo "$SUGGESTIONS_N"; }
    wrap λ "Link-suggestion database broken?"
    λ(){ BACKLINKS_N=$(cat ./metadata/backlinks.hs | wc --lines);         [ "$BACKLINKS_N"   -le 180000 ] && echo "$BACKLINKS_N"; }
    wrap λ "Backlinks database broken?"

    λ(){ ANNOTATION_FILES_N=$(find ./metadata/annotation/ -maxdepth 1 -type f | wc --lines);
         [ "$ANNOTATION_FILES_N"   -le 19000 ] && echo "$ANNOTATION_FILES_N"; }
    wrap λ "Annotation files are missing?"
    λ(){ BACKLINKS_FILES_N=$(find ./metadata/annotation/backlink/ -type f | wc --lines);
         [ "$BACKLINKS_FILES_N"    -le 28500 ] && echo "$BACKLINKS_FILES_N"; }
    wrap λ "Backlinks files are missing?"
    λ(){ SIMILARLINKS_FILES_N=$(find ./metadata/annotation/similar/ -type f | wc --lines);
         [ "$SIMILARLINKS_FILES_N" -le 12000 ] && echo "$SIMILARLINKS_FILES_N"; }
    wrap λ "Similar-links files are missing?"

    ## NOTE: transclude.js supports some special 'range' syntax for transclusions, so a link like '/note/lion#history#'/'/note/lion##history'/'/note/lion##'/'/note/lion#history#foo' is in fact valid
    λ(){ ge -e '#[[:alnum:]-]+#' -e '[[:alnum:]-]+##[[:alnum:]-]+' metadata/*.gtx metadata/*.hs | gev -e '#[[:alnum:]-]+#$'; }
    wrap λ "Broken double-hash anchors in links somewhere?"

    λ(){ ge -- '^http.*/[[:graph:]]+[0-9]–[0-9]' ./metadata/*.gtx ./metadata/*.hs || true;
         gf -- '–' ./metadata/*.hs || true; }
    wrap λ "En-dashes in URLs?"

    λ(){ gf -e 'http' ./metadata/*.hs ./metadata/*.gtx | gfv -e 'https://en.wikipedia.org/wiki/' -e '10/arc-1-gestation/1' -e 'the-elves-leave-middle-earth-' -e '2011/05/from-the-bookcase-no-2' -e 'd-a-rovinskiis-collection-of-russian-lubki-18th' -e 'commons.wikimedia.org/wiki/File:Flag_of_the_NSDAP_' | gf -e "%E2%80%93" -e "%E2%80%94" -e "%E2%88%92"; }
    wrap λ "*Escaped* En/em/minus dashes in URLs?"

    λ(){ gf -e '\\' ./static/css/*.css; }
    wrap λ "Warning: stray backslashes in CSS‽ (Dangerous interaction with minification!)"

    λ(){ find ./ -type f -name "*.md" | grep -F -v -e '_site' -e 'Modafinil' -e 'Blackmail' | sed -e 's/\.md$//' -e 's/\.\/\(.*\)/_site\/\1/' | xargs grep -F --with-filename --color=always -e '!Wikipedia' -e '!W'")" -e '!W \"' -e ']( http' -e ']( /' -e '](#fn' -e '!Margin' -e '<span></span>' -e '<span />' -e '<span/>' -e 'http://gwern.net' -e 'http://www.gwern.net' -e 'https://www.gwern.net' -e 'https//www' -e 'http//www'  -e 'hhttp://' -e 'hhttps://' -e ' _n_s' -e '/journal/vaop/ncurrent/' -e '://bit.ly/' -e 'remote/check_cookie.html' -e 'https://www.biorxiv.org/node/' -e '/article/info:doi/10.1371/' -e 'https://PaperCode.cc' -e '?mod=' -e 'www.researchgate.net' -e '.pdf&amp;rep=rep1&amp;type=pdf' -e '.pdf&rep=rep1&type=pdf' | \
         ge -e 'https://web.archive.org/web/.*gwern\.net.*' -e 'Blackmail';
       }
    wrap λ "Stray or bad URL links in Markdown-sourced HTML."

    ## Whitelist of HTML classes which are authorized for use. Any new classes should be added here.
    λ(){
        # parse all HTML files, print out classes to a unique newline-delimited list, and return classes not in the whitelist, for detailed examination as either bugs or new classes to whitelist:
        declare -a html_classes_whitelist=(
            "see-also-append" "archive-not" "archive-local" "author" "full-authors-list" "aux-links"
            "backlink-not" "backlinks" "backlinks-append" "aux-links-append" "aux-links-transclude-file" "bash"
            "Bash" "book-review-author" "book-review-date" "book-review-rating" "book-review-title" "cite-author"
            "cite-author-plural" "cite-date" "date" "display" "email" "external-page-embed"
            "id-not" "inflation-adjusted" "logotype-tex" "logotype-latex" "logotype-latex-a" "logotype-latex-e"
            "link-annotated" "link-live" "link-page" "link-page-not" "link-tag" "link-tags"
            "cite" "cite-joiner" "collapse" "columns" "directory-indexes-downwards" "directory-indexes-upwards"
            "epigraph" "even" "float-right" "float-left" "footnote-ref" "full-width"
            "haskell" "header" "horizontal-rule-nth-0" "horizontal-rule-nth-1" "horizontal-rule-nth-2" "icon-not"
            "link-modified-recently" "icon-single-white-star-on-black-circle" "inline" "invert" "invert-auto" "invert-not"
            "javascript" "link-annotated-not" "link-annotated-partial" "content-transform-not" "link-live-not" "tex-logotype"
            "math" "odd" "page-thumbnail" "pascal" "python" "reader-mode-selector-inline"
            "smallcaps" "sourceCode" "subsup" "table-small" "table-sort-not" "width-full"
            "TOC" "uri" "at" "bu" "c1" "c2"
            "c3" "c4" "c5" "c6" "c7" "c8"
            "c9" "c10" "cf" "co" "dv" "fu"
            "kw" "op" "s1" "st" "reader-mode" "scrape-abstract-not"
            "abstract" "abstract-collapse" "abstract-collapse-only" "admonition" "admonition-title" "book-review-meta"
            "book-review-review" "tip" "xml" "warning" "al" "an"
            "bn" "cn" "cv" "do" "dt" "er"
            "error" "ex" "fl" "im" "in" "ot"
            "pp" "re" "sc" "ss" "va" "citation"
            "directory-indexes" "directory-indexes-sideways" "display-pop-not" "footnote-back" "footnotes" "image-focus-not"
            "include" "include-strict" "include-lazy" "include-annotation" "include-even-when-collapsed" "include-spinner-not"
            "include-replace-container" "include-replace-container-not" "include-unwrap" "include-block-context" "include-omit-metadata" "include-content"
            "include-content-no-header" "include-block-context-expanded" "include-annotation-core" "include-content-core" "marginnote" "markdownBody"
            "mjpage" "mjpage__block" "mjx-base" "mjx-box" "MJXc-display" "mjx-cell"
            "mjx-char" "mjx-charbox" "mjx-chtml" "MJXc-space1" "MJXc-space2" "MJXc-space3"
            "MJXc-stacked" "MJXc-TeX-ams-R" "MJXc-TeX-cal-R" "MJXc-TeX-main-R" "MJXc-TeX-math-I" "MJXc-TeX-size1-R"
            "MJXc-TeX-size2-R" "MJXc-TeX-size3-R" "MJXc-TeX-size4-R" "mjx-delim-h" "mjx-delim-v" "mjx-denominator"
            "mjx-itable" "mjx-line" "mjx-math" "mjx-mfrac" "mjx-mi" "mjx-mn"
            "mjx-mo" "mjx-mrow" "mjx-mspace" "mjx-msqrt" "mjx-mstyle" "mjx-msubsup"
            "mjx-msup" "mjx-mtext" "mjx-munderover" "mjx-numerator" "mjx-op" "mjx-over"
            "mjx-row" "mjx-stack" "mjx-sub" "mjx-sup" "mjx-surd" "mjx-texatom"
            "mjx-TeXmathchoice" "mjx-under" "mjx-vsize" "new" "outline-not" "outline"
            "warning" "markdown-body" "similars" "similars-append" "similar-links-search" "text-center"
            "abstract-tag-directory" "page-description-annotation" "link-bibliography" "link-bibliography-append" "expand-on-hover" "tag-index-link-bibliography-block"
            "doc-index-tag-short" "decorate-not" "quote-of-the-day" "interview" "reader-mode-note"
            "dropcap-dropcat" "desktop-not" "mobile-not" "adsense" "years-since" "date-range"
        )
        html_classes_regexpattern=$(IFS='|'; echo "${html_classes_whitelist[*]}")
        html_classes=$(echo "$PAGES_ALL" | xargs --max-procs=0 --max-args=500 ./static/build/htmlClassesExtract.py | tr ' ' '\n' | sort --unique)

        echo "$html_classes" | gev --line-regexp "$html_classes_regexpattern" --

        # Check for whitelisted classes *not* present, suggesting a typo or stale entry in the whitelist:
        for class in "${html_classes_whitelist[@]}"; do
            if ! grep -E --invert-match --line-regexp --quiet "$class" <<< "$html_classes"; then
                echo "'""$class""' is not in HTML classes"
            fi
        done | less
    }
    wrap λ "Mysterious HTML classes in compiled HTML?"

    λ(){ echo "$PAGES_ALL" | gfv 'Hafu' | xargs grep -F --with-filename --invert-match -e ' tell what Asahina-san' -e 'contributor to the Global Fund to Fight AIDS' -e 'collective name of the project' -e 'model resides in the' -e '{.cite-' -e '<span class="op">?' -e '<td class="c' -e '<td style="text-align: left;">?' -e '>?</span>' -e '<pre class="sourceCode xml">' | \
             gfc -e ")'s " -e "}'s " -e '">?' -e '</a>s';
         echo "$PAGES_ALL" | gfv 'Hafu' | xargs grep -E --with-filename --color=always -e '<a .*href=".*">\?';
       }
    wrap λ "Punctuation like possessives should go *inside* the link (unless it is an apostrophe in which case it should go outside due to Pandoc bug #8381)."
    ## NOTE: 8381 <https://github.com/jgm/pandoc/issues/8381> is a WONTFIX by jgm, so no solution but to manually check for it. Fortunately, it is rare.

    # λ(){ echo "$PAGES_ALL" | xargs --max-args=100 elinks -dump |

    λ(){ find . -name "*.md" -type f -exec grep -E --with-filename 'thumbnail: /doc/.*/.*\.svg$' {} \; ; }
    wrap λ "SVGs don't work as page thumbnails in Twitter (and perhaps many other websites), so replace with a PNG."

    λ(){ ge 'http.*http' metadata/archive.hs  | gfv -e 'web.archive.org' -e 'https-everywhere' -e 'check_cookie.html' -e 'translate.goog' -e 'archive.md' -e 'webarchive.loc.gov' -e 'https://http.cat/' -e '//)' -e 'https://esolangs.org/wiki////' -e 'https://ansiwave.net/blog/sqlite-over-http.html' -e 'addons.mozilla.org/en-US/firefox/addon/' -e 'httparchive.org/' -e 'github.com/phiresky/' -e 'github.com/psanford/' -e 'stackoverflow.com/questions/'; }
    wrap λ "Bad URL links in archive database (and perhaps site-wide)."

    λ(){ echo "$PAGES_ALL" | xargs grep -F --with-filename --color=always -e '<div>' -e '<div class="horizontal-rule-nth-0" />' -e '<div class="horizontal-rule-nth-1" />' -e '<div class="horizontal-rule-nth-2" />' -e ':::' | gfv -e 'I got around this by adding in the Hakyll template an additional'; }
    wrap λ "Stray <div>?"

    λ(){ echo "$PAGES_ALL" | xargs --max-args=500 grep -F --with-filename --color=always -e 'invertible-not' -e 'invertible-auto' -e '.invertible' -e '.invertibleNot' -e '.invertible-Not' -e '{.Smallcaps}' -e '{.sallcaps}' -e '{.mallcaps}' -e '{.small}' -e '{.invertible-not}' -e 'no-image-focus' -e 'no-outline' -e 'idNot' -e 'backlinksNot' -e 'abstractNot' -e 'displayPopNot' -e 'small-table' -e '{.full-width' -e 'collapseSummary' -e 'collapse-summary' -e 'tex-logotype' -e ' abstract-not' -e 'localArchive' -e 'backlinks-not' -e '{.}' -e "bookReview-title" -e "bookReview-author" -e "bookReview-date" -e "bookReview-rating" -e 'class="epigraphs"' -e 'data-embedding-distance' -e 'data-embeddingdistance' -e 'data-linktags' -e 'link-auto-first' -e 'link-auto-skipped' -e 'local-archive-link' -e 'include-replace}' -e 'include-replace ' -e 'drop-caps-de-kanzlei' -e '.backlink-not)' -e 'link-annotated link-annotated-partial' -e 'link-annotated-partial link-annotated' -e '{.margin-note}' -e '{. ' -e 'collapse}' -e 'interview}' -e 'cssExtension' -e 'thumbnailText' -e 'thumbnailCSS' -e '!Margin'; }
    wrap λ "Misspelled/outdated classes in HTML."

    λ(){
        ghci -istatic/build/ static/build/LinkMetadata.hs -e 'do { md <- readLinkMetadata; putStrLn $ unlines $ map (\(f,(t,auts,_,_,_,_,_)) -> f ++ " : " ++ t ++ " : " ++ auts) $ M.toList md; }' | \
             `## blacklist of fraudsters or bad papers:` \
             gf \
                  `### authors:` \
                  -e 'Francesca Gino' -e 'Dan Ariely' -e 'Michael LaCour' -e 'David Rosenhan' -e 'Diederik Stapel' -e 'Didier Raoult' -e 'Brian Wansink' -e 'Marc Hauser' -e 'Robert Rosenthal' -e 'J. Hendrik Schön' -e 'Matthew Walker' -e 'Guéguen' -e 'Gueguen' -e 'Stephan Lewandowsky' -e 'Sander van der Linden' -e 'Bharat B. Aggarwal' -e 'Bharat Aggarwal' -e 'Changhwan Yoon' -e 'Sam Yoon' -e 'Juan Manuel Corchado' \
                  `### papers:` \
                  -e "A Fine is a Price" | \
             ## whitelist of papers to not warn about, because not dangerous or have appropriate warnings/caveats:
             gfv -e '/doc/economics/experience-curve/2020-kc.pdf' -e '/doc/food/2002-wansink.pdf' -e 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2244801/' -e 'https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0069258' -e '/doc/statistics/bias/2012-levelt.pdf' -e 'https://en.wikipedia.org/wiki/' -e 'https://guzey.com/books/why-we-sleep/' -e 'https://statmodeling.stat.columbia.edu/2019/11/' -e '/doc/psychiatry/schizophrenia/rosenhan/2020-01-25-andrewscull-howafraudulentexperimentsetpsychiatrybackdecades.html' -e 'https://osf.io/preprints/psyarxiv/m6s28/' -e '/doc/statistics/bias/1968-rosenthal-pygmalionintheclassroom.pdf' -e '/doc/statistics/bias/1976-rosenthal-experimenterexpectancyeffects.pdf' -e '/doc/statistics/bias/2023-amabile.pdf' -e 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7615113/';
       }
    wrap λ "Dishonest or serial fabricators detected as authors? (If a fraudulent publication should be annotated anyway, add a warning to the annotation & whitelist it.)" &

     λ(){ find ./ -type f -name "*.md" | gfv '/variable' | gfv '_site' | sed -e 's/\.md$//' -e 's/\.\/\(.*\)/_site\/\1/' | parallel --max-args=500 gf --with-filename --color=always -e '{#'; }
     wrap λ "Bad link ID overrides in Markdown."

    λ(){ find ./ -type f -name "*.md" | gfv '_site' | sed -e 's/\.md$//' -e 's/\.\/\(.*\)/_site\/\1/'  | parallel --max-args=500 ge --with-filename --color=always -e 'pdf#page[0-9]' -e 'pdf#pg[0-9]' -e '\#[a-z]+\#[a-z]+'; }
    wrap λ "Incorrect PDF page links in Markdown."

    λ(){ find ./ -type f -name "*.md" -type f -exec grep -E -e 'css-extension:' {} \; | \
       gfv -e 'css-extension: dropcaps-cheshire' -e 'css-extension: dropcaps-cheshire reader-mode' -e 'css-extension: dropcaps-de-zs' -e 'css-extension: dropcaps-goudy' -e 'css-extension: dropcaps-goudy reader-mode' -e 'css-extension: dropcaps-kanzlei' -e 'css-extension: "dropcaps-kanzlei reader-mode"' -e 'css-extension: dropcaps-yinit' -e 'css-extension: dropcaps-dropcat' -e 'css-extension: dropcaps-gene-wolfe'; }
    wrap λ "Incorrect dropcaps in Markdown."

    λ(){ find ./ -type f -name "*.md" | gfv '_site' | gfv -e 'lorem-code.md' -e 'ab-test.md' | sed -e 's/\.md$//' -e 's/\.\/\(.*\)/_site\/\1/'  | parallel --max-args=500 "grep --color=always -F --with-filename -- '<span class=\"er\">'"; } # NOTE: filtered out lorem-code.md's deliberate CSS test-case use of it in the syntax-highlighting section
    wrap λ "Broken code in Markdown."

    λ(){ find ./ -type f -name "*.md" | gfv -e '/lorem-inline' -e '/subscript' | parallel --max-args=500 "gf --with-filename -e '<span class=\"supsub\">' -e 'class=\"subsup\"><sup>' --"; }
    wrap λ "Incorrect use of 'supsub' name (should be 'subsup')."

    λ(){ find ./ -type f -name "*.md" | gfv -e '/lorem-inline' -e '/subscript' | parallel --max-args=500 "gf --with-filename -e 'class=\"subsup\"><sup>'"; }
    wrap λ "Incorrect ordering of '<sup>' (the superscript '<sup>' must come second, or else risk Pandoc misinterpreting as footnote while translating HTML↔Markdown)."

    λ(){ ge -e '<div class="admonition .*\?">[^$]' -e 'class="admonition"' -e '"admonition warn"' -e '<div class="epigrah">' -e 'class="epigraph>' -e '<span><div>' $PAGES; }
    wrap λ "Broken admonition paragraph or epigraph in Markdown."

    λ(){ ge -e '^   - '  -e '~~~[[:alnum:]]' $PAGES; }
    wrap λ "Markdown formatting problem: use of 3-space indented sub-list items instead of 4-space?"

    λ(){ gec -e ' a [aei]' $PAGES | gfv -e 'static/build/' -e '/gpt-3' -e '/gpt-2-preference-learning' -e 'sicp/' -e 'a eulogy' -e 'a eureka moment'; }
    wrap λ "Grammar: 'a' → 'an'?"

     λ(){ gec -e '<div class="text-center">$' -e '[A-Za-z]\.\. ' -e '– ' -e  ' –' -e '^> <div class="abstract">$' -e ' is is ' -- $PAGES | gfv '/utext'; }
     wrap λ "Markdown: miscellaneous regexp errors."

    λ(){ find -L . -type f -size 0  -printf 'Empty file: %p %s\n' | gfv -e '.git/FETCH_HEAD' -e './.git/modules/static/logs/refs/remotes/'; }
    wrap λ "Empty files somewhere."

    λ(){ check_dirs() {
             for dir in "$@"; do
                 if [ -d "$dir" ]; then
                     if find "$dir" -mindepth 1 -maxdepth 1 -type f ! -name "index.md" | read; then
                         echo "Directory $dir contains files other than 'index.md':"
                         find "$dir" -mindepth 1 -maxdepth 1 -type f ! -name "index.md"
                     fi
                 else
                     echo "$dir is not a valid directory!"
                 fi
             done
         }
         check_dirs "./doc/www/" "./doc/newest/" "./doc/newsletter/"; }
    wrap λ "Stray files in ./doc/ subdirectories that should not contain any files besides the 'index.md' tag-directory file."

    λ(){ find ./_site/ -type f -not -name "*.*" -exec grep --quiet --binary-files=without-match . {} \; -print0 | parallel --null --max-args=500 "gf --color=always --with-filename -- '————–'"; }
    wrap λ "Broken tables in HTML."

    λ(){ find ./ -type f -name "*.md" | gfv '_site' | sed -e 's/\.md$//' -e 's/\.\/\(.*\)/_site\/\1/'  | xargs --max-args=500 grep -F --with-filename --color=always -e '](/​image/​' -e '](/​images/​' -e '](/images/' -e '<p>[[' -e ' _</span><a ' -e ' _<a ' -e '{.marginnote}' -e '^[]' -e '‘’' -e '``' -e 'href="\\%' -e '**' --; }
    wrap λ "Miscellaneous fixed-string errors in compiled HTML."

    λ(){ find ./ -type f -name "*.md" | gfv -e '_site' -e '/index' -e '/lorem-block' | sed -e 's/\.md$//' -e 's/\.\/\(.*\)/_site\/\1/' | xargs --max-args=10 ./static/build/collapse-checker.py;
         find ./metadata/annotation -maxdepth 1 -type f | xargs --max-args=500 ./static/build/collapse-checker.py; }
    wrap λ "Overuse of '.collapse' class in compiled HTML?"

    λ(){ find ./ -type f -name "*.md" | gfv '_site' | sed -e 's/\.md$//' -e 's/\.\/\(.*\)/_site\/\1/'  | parallel --max-args=500 ge --with-filename --color=always -e ' __[A-Z][a-z]' -e 'href="/[a-z0-9-]#fn[0-9]+"' -e 'href="#fn[0-9]+"' -e '"></a>' -e '</p>[^ <"]' | gfv -e 'tabindex="-1"></a>'; }
    wrap λ "Miscellaneous regexp errors in compiled HTML."

    λ(){ ge -e '^"~/' -e '\$";$' -e '$" "doc' -e '\|' -e '\.\*\.\*' -e '\.\*";' -e '"";$' -e '.\*\$ doc' ./static/redirect/nginx*.conf | gfv -e 'default "";'; }
    wrap λ "Warning: empty result or caret/tilde-less Nginx redirect rule (dangerous—matches anywhere in URL!)"

    λ(){ ghci -istatic/build/ ./static/build/LinkMetadata.hs -e 'warnParagraphizeGTX "metadata/full.gtx"'; }
    wrap λ "Annotations that need to be rewritten into paragraphs." &

    λ(){ gwa | gf -- '[]' | gfv -e '/newsletter/' | sort; } # we exclude future newsletter issues as deliberately untagged to avoid appearing at the top of the newsletter tag # | gev --perl-regexp '\e\[36ma\e\[0m: '
    wrap λ "Untagged annotations." &

    λ(){ gwa | gev -e '</a></p> ?</li> ?<li> ?<p><a' | ge -e '<div class="aux-links-append see-also-append collapse">.*<p><strong>See Also</strong>:</p>.*<div class="columns"> ?<ul> ?<li>'; }
    wrap λ "Annotations with single-entry See-Alsos which are collapsed, which is pointless (as it is not any more compact); remove the '.collapse' class." &

    λ(){ gwa | gf -- '[("doi"'; }
    wrap λ "Broken annotations leaking into each other in GTX?" &

    λ(){ runghc -istatic/build/ ./static/build/link-prioritize.hs 20; }
    wrap λ "Links needing annotations by priority:" &

    λ(){ gec -e '[a-zA-Z]- ' -e 'PsycInfo Database Record' -e 'https://www.gwern.net' -e '/home/gwern/' -e 'https://goo.gl' -- ./metadata/*.gtx | \
         gfv -e 'https://web.archive.org/web/'; }
    wrap λ "Check possible typo in GTX metadata database." &

    λ(){ ge '  - .*[a-z]–[a-Z]' ./metadata/full.gtx ./metadata/half.gtx; }
    wrap λ "Look for en-dash abuse." &

    λ(){ gf -e ' ?' ./metadata/full.gtx; }
    wrap λ "Problem with question-marks (perhaps the crossref/Emacs copy-paste problem?)." &

    λ(){ gfv -e 'N,N-DMT' -e 'E,Z-nepetalactone' -e 'Z,E-nepetalactone' -e 'N,N-Dimethyltryptamine' -e 'N,N-dimethyltryptamine' -e 'h,s,v' -e ',VGG<sub>' -e 'data-link-icon-type="text,' -e 'data-link-icon-type=\"text,' -e '(R,S)' -e 'R,R-formoterol' -e '(18)F-FDG' -e '<em>N,N</em>' -e '"text,tri' -e '"text,quad' -e '"text,sans"' -- ./metadata/full.gtx ./metadata/half.gtx | \
             gec -e ',[A-Za-z]'; }
    wrap λ "Look for run-together commas (but exclude chemical names where that's correct)." &

    λ(){ gev '^- - http' ./metadata/*.gtx | ge '[a-zA-Z0-9>]-$'; }
    wrap λ "Look for GTX line breaking at a hyphen." &

    λ(){ ge -e '[.,:;-<]</a>' -e '\]</a>' -- ./metadata/*.gtx | gfv -e 'i.i.d.' -e 'sativum</em> L.</a>' -e 'this cloning process.</a>' -e '#' -e '[review]</a>' | ge -e '[.,:;-<]</a>'; }
    wrap λ "Look for punctuation inside links; unless it's a full sentence or a quote or a section link, generally prefer to put punctuation outside." &

    λ(){ gfc -e '**' -e ' _' -e '_ ' -e '!!' -e '*' -- ./metadata/full.gtx ./metadata/half.gtx | gfv '_search_algorithm'; } # need to exclude 'A* search'
    wrap λ "Look for italics errors." &

    λ(){ gf -e 'amp#' -- ./metadata/*.gtx; }
    wrap λ "Unicode/HTML entity encoding error?" &

    λ(){ gfc -e 'en.m.wikipedia.org' -- ./metadata/full.gtx; }
    wrap λ "Check possible syntax errors in full.gtx GTX metadata database (fixed string matches)." &

    λ(){ gec -e '^- - /docs/.*' -e '^  -  ' -e "\. '$" -e '[a-zA-Z]\.[0-9]+ [A-Z]' \
            -e 'href="[a-ce-gi-ln-zA-Z]' -e '>\.\.[a-zA-Z]' -e '\]\([0-9]' \
            -e '[⁰ⁱ⁴⁵⁶⁷⁸⁹⁻⁼⁽⁾ⁿ₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ₐₑₒₓₔₕₖₗₘₙₚₛₜ]' -e '<p>Table [0-9]' -e '<p>Figure [0-9]' \
            -e 'id="[0-9]' -e '</[a-z][a-z]+\?' -e 'via.*ihub' -e " '$" -e "’’" -e ' a [aei]' -e '</[0-9]+' \
            -e ' - 20[0-9][0-9]:[0-9][0-9]:[0-9][0-9]' -e '#googl$' -e "#googl$'" -e 'gwtag' -e ' <p><strong>[A-Z][A-Z][A-Z]+</strong>' \
            -e '&org=.*&org=' -e '[0-9]⁄[0-9]\.[0-9]' -e '[0-9]\.[0-9]⁄[0-9]' -e '\[[Kk]eywords\?: ' \
            -e ' 19[0-9][0-9]–[1-9][0-9]–[0-9][0-9]' -e ' 20[0-9][0-9]–[1-9][0-9]–[0-9][0-9]' -e "''.*''" \
            `# match both single & double-quotation versions of erroneous inflation-adjusters like "<a href='$2022'>148,749</a>":` \
            -e '<a href=.\$[12][0-9][0-9][0-9].>[0-9a-zA-Z,.-]' -e '<ul>[ a-zA-Z][ a-zA-Z]' -- ./metadata/*.gtx; }
    wrap λ "Check possible syntax errors in GTX metadata database (regexp matches)."

    λ(){ gfc -e ']{' -e 'id="cb1"' -e '<dd>' -e '<dl>' \
            -e '&lgt;/a>' -e '</a&gt;' -e '&lgt;/p>' -e '/p&gt;' -e '<i><i' -e '</e>' -e '>>' \
            -e '<abstract' -e '<em<' -e '< em>' -e '<em.' -e '<center' -e '<p/>' -e '</o>' -e '< sub>' -e '< /i>' \
            -e '</i></i>' -e '<i><i>' -e 'font-style:italic' -e '<p><p>' -e '</p></p>' -e 'fnref' \
            -e '<figure class="invertible">' -e '</a<' -e 'href="%5Bhttps' -e '<jats:inline-graphic' \
            -e '<figure-inline' -e '<small></small>' -e '<inline-formula' -e '<inline-graphic' -e '<ahref=' \
            -e '](/' -e '-, ' -e '<abstract abstract-type="' -e '- pdftk' -e 'thumb|' -e ' <span>' -e "''''" -- ./metadata/*.gtx; }
    wrap λ "#1: Check possible syntax errors in GTX metadata database (fixed string matches)."
    λ(){ gfc -e '<sec ' -e '<list' -e '</list>' -e '<wb<em>r</em>' -e '<abb<em>' -e '<ext-link' -e '<title>' -e '</title>' \
            -e ' {{' -e '<<' -e '[Formula: see text]' -e '<p><img' -e '<p> <img' -e '- - /./' -e '[Keyword' -e '[KEYWORD' \
            -e '[Key word' -e '<strong>[Keywords:' -e 'href="$"' -e '<em>Figure' \
            -e '<strongfigure' -e ' ,' -e ' ,' -e 'href="Wikipedia"' -e 'href="W"' -e 'href="(' -e '>/em>' -e '<figure>[' \
            -e '<figcaption></figcaption>' -e '&Ouml;' -e '&uuml;' -e '&amp;gt;' -e '&amp;lt;' -e '&amp;ge;' -e '&amp;le;' \
            -e '<ul class="columns"' -e '<ol class="columns"' -e ',/div>' -e '](https://' -e ' the the ' \
            -e 'Ꜳ' -e 'ꜳ'  -e 'ꬱ' -e 'Ꜵ' -e 'ꜵ' -e 'Ꜷ' -e 'ꜷ' -e 'Ꜹ' -e 'ꜹ' -e 'Ꜻ' -e 'ꜻ' -e 'Ꜽ' -e 'ꜽ' -- ./metadata/*.gtx; }
    wrap λ "#2: Check possible syntax errors in GTX metadata database (fixed string matches)."
    λ(){ gfc -e '🙰' -e 'ꭁ' -e 'ﬀ' -e 'ﬃ' -e 'ﬄ' -e 'ﬁ' -e 'ﬂ' -e 'ﬅ' -e 'ﬆ ' -e 'ᵫ' -e 'ꭣ' -e ']9h' -e ']9/' \
            -e ']https' -e 'STRONG>' -e '\1' -e '\2' -e '\3' -e ']($' -e '](₿' -e 'M age' -e '….' -e '((' -e ' %' \
            -e '<h1' -e '</h1>' -e '<h2' -e '</h2>' -e '<h3' -e '</h3>' -e '<h4' -e '</h4>' -e '<h5' -e '</h5>' \
            -e '</strong>::' -e ' bya ' -e '?gi=' -e ' ]' -e 'gwsed' -e 'full.full' -e ',,' \
            -e '"!"' -e '</sub<' -e 'xref>' -e '<xref' -e '<e>' -e '\\$' -e 'title="http' -e '%3Csup%3E' -e 'sup%3E' -e ' et la ' \
            -e '<strong>Abstract' -e ' ]' -e "</a>’s" -e 'title="&#39; ' -e 'collapseAbstract' -e 'utm_' \
            -e ' JEL' -e '(JEL' -e 'top-k' -e '</p> </p>' -e '</sip>' -e '<sip>' -e ',</a>' -e ' : ' -e " ' " -e '>/>a' -e '</a></a>' -e '(, ' \
            -e '&lt;figcaption' -e '{.' -e ' ?' -e " ’’" -e 'lt;/td&gt;' -e "‘’" -e "’‘" -e "’’" -e '<li></li>' -e '</em<em>' -e '𝑂' \
            -e '</a.>' -e ' . ' -e ' , ' -e ' ; ' -e 'class=”collapse”' -e "‘’" -e " ’" -e '<bold>' -e '</bold>' -e '<jats:bold>' \
            -e  '</jats:bold>' -e 'Ã©' -e '</a>s' -e '/&gt;'  -e '&lt;figcaption'  -e 'aria-hidden=">' -e '&gt;</a>' -e '<A Href' \
            -e '</strong>:,' -e ' et al.' -e '<em>et al</em>' -e '<span class="latex">LaTeX</span>' -e '<div>' -e '>LaTeX</a>' -e '>TeX</a>' -e '<em><em>' \
            -e '</em></em>' -e '<strong><strong>' -e '</strong></strong>' -e 'doi:' -e '\\\' -e 'href"http' \
            -e '… .' -e '... .'  -e '– ' -e  ' –' -e '### Competing' -e '<strong></strong>' -e '<span style="font-weight: 400;">' \
            -e '</p> </figcaption>' -e '</p></figcaption>' -e '<figcaption aria-hidden="true"><p>' -e '<figcaption aria-hidden="true"> <p>' \
            -e '<figcaption><p>' -e '<figcaption> <p>' -e 'Your input seems to be incomplete.' -e 'tercile' -e 'tertile' -e '\\x01' -e '&#' \
            -e '</strong>:. ' -e 'http://https://' -e '#"' -e "#'" -e '<strong>Highlights</strong>: ' -e 'jats:styled-content' \
            -e 'inline-formula' -e 'inline-graphic' -e '<sec' -e '”(' -e '’(' -e '#.' -e 'href="#page=' -e '%7E' -e '<p>. ' -e '<p>, ' -e '<p>; ' -- ./metadata/*.gtx | \
             gfv -e 'popular_shelves' -e 'Le corps dans les étoiles: l’homme zodiacal';
       }
    wrap λ "#3: Check possible syntax errors in GTX metadata database (fixed string matches)." &

    λ(){ ge -e ' [0-9]/[0-9]+ ' -- ./metadata/*.gtx | gfv -e 'Toll-like' -e 'Adam' -e '0/1' -e 'My Little Pony Seasons' -e '9/11' -e 'TRFK 31/8' -e 'TRFK 303/577' -e 'TRFK 6/8'; }
    wrap λ "Possible uses of FRACTION SLASH ⁄ or EN DASH –?" &

    λ(){ gf -e 'http://dl.dropbox' -e '.wiley.com/doi/abs/'  \
                 -e 'www.tandfonline.com/doi/abs/' -e 'jstor.org' -e 'springer.com' -e 'springerlink.com' \
                 -e 'www.mendeley.com' -e 'academia.edu' -e 'researchgate.net' -e 'pdf.yt' \
                 -e 'photobucket' -e 'imgur.com' -e 'hathitrust.org' -e 'emilkirkegaard.dk' -e 'arthurjensen.net' \
                 -e 'humanvarieties.org' -e 'libgen.io/' -e 'gen.lib.rus.ec/' -e 'sci-hub.bz/' -e '](http://www.scilogs.com/' \
                 -e 'sci-hub.cc/' -e "papers.nber.org/" -e '](!wikipedia' -e '](!wikipedia)'"'s" -e 'https://wwww.' -e 'http://wwww.' \
                 -e 'http://33bits.org' -e 'https://gwern.net' -e 'https://gwern.net' -e 'web.archive.org/web/2' \
                 -e 'webarchive.org.uk/wayback/' -e 'webcitation.org' -e 'plus.google.com' -e 'www.deepdotweb.com' -e 'raikoth.net' \
                 -e 'drive.google.com/file' -e 'ssrn.com' -e 'ardenm.us' -e 'gnxp.nofe.me' -e 'psycnet.apa.org' \
                 -e 'wellcomelibrary.org/item/' -e 'dlcs.io/pdf/' -e 'secure.wikimedia.org' \
                 -e 'https://biorxiv.org' \
                 -e 'fbclid=' -e '?gid=' -e 'twitter.com/#!' -e 'pay.reddit.com' -e 'europepmc.org' -e 'drugcite.com' \
                 -e 'guardian.co.uk' -e 'mlp.wikia.com' -e '฿' -e '!Wikipedia ""' -e 'temcauley.staff.shef.ac.uk' \
                 -e 'yahoo.com' -e 'bloomberg.com' -e '.wsj.com' -e 'extremelongevity.net' -e 'blog.openai.com' \
                 -e 'https://ww.gwern.net' -e 'https://w.gwern.net' -e 'www.heretical.com' -e 'books.google.ca' \
                 -e 'lesserwrong.com' -e 'au.news.yahoo.com' -e 'northjersey.com' -e 'tribune.com.pk' -e 'idsnews.com' \
                 -e 'catsensebook.com' -e 'whec.com' -e 'www.mercurynews.com' -e 'meetup.com' \
                 -e 'dlcs.io/' -e 'centerforcollegeaffordability.org' -e 'quora.com' -e 'times-news.com' -e 'www.cebp.nl' \
                 -e '#filmtv' -e 'nybooks.com' -e '<div id="columns">' -e 'annualreviews.org' \
                 -e 'dspace.mit.edu' -e 'shirky.com' -e '](http://www.nzherald.co.nz)' -e 'https://www.arxiv.org' \
                  -e 'goodreads.com/review/show' -e 'myanimelist.net/reviews.php?id='  \
                 -e 'cloudfront.net' -e 'https://www.amazon.com/s?ie=UTF8&field-isbn=&page=1&rh=i:stripbooks' -e 'http://ltimmelduchamp.com' \
                 -e 'thiswaifudoesnotexist.net)' -e 'thiswaifudoesnotexist.net"' -e 'www.wikilivres.ca' -e 'worldtracker.org' \
                 -e 'meaningness.wordpress.com' -e 'ibooksonline.com' -e 'tinypic.com' -e 'isteve.com' -e 'j-bradford-delong.net'\
                 -e 'cdn.discordapp.com' -e 'http://https://' -e '#"' -e "#'" -e '.comwww.' -- ./metadata/backlinks.hs;
         # NOTE: we do not need to ban bad domains which are handled by link rewrites like www.reddit.com or medium.com.
       ge 'https://arxiv.org/abs/[0-9]\{4\}\.[0-9]+v[0-9]' -- ./metadata/backlinks.hs | sort --unique; }
    wrap λ "Bad or banned blacklisted domains found? They should be removed or rehosted." &

    λ(){ gf -e '""' -- ./metadata/*.gtx | gfv -e ' alt=""' -e 'controls=""' -e 'loop=""' -e '[("doi","")]'; }
    wrap λ "Doubled double-quotes in GTX, usually an error." &

    λ(){ gf -e "'''" -- ./metadata/full.gtx ./metadata/half.gtx; }
    wrap λ "Triple quotes in GTX, should be curly quotes for readability/safety." &

    λ(){ gev '^- - ' -- ./metadata/*.gtx | gf -e ' -- '; }
    wrap λ "Markdown hyphen problems in GTX metadata database" &

    λ(){ ge -e '^    - _' $PAGES | gfv -e '_Additional Poems_' -e '_Aim for the Top!_' -e '_[Cognitive Surplus](!W)_' \
                                              -e '_Fontemon_' -e '_Forbes_' -e '_[Four Major Plays of Chikamatsu](!W)_' \
                                              -e '_[Neon Genesis Evangelion: Angelic Days](!W)_' -e '_Rebuild_' -e '_[Star Maker](!W)_' \
                                              -e '_[The Battles of Coxinga](!W)_' -e '_[The End of Evangelion](!W)_' -e '_The Fountain_' \
                                              -e '_[The Love Suicides at Sonezaki](!W)_' -e '_[The Pleasures of Japanese Literature](!W)_' \
                                              -e '_The Simple Men_'  -e '_Renaming of the Birds_' -e '_[The Love Suicides at Amijima](!W)_' \
                                              -e '_[The Uprooted Pine](!W)_' -e '_[Travelers of a Hundred Ages](!W)_' \
                                              -e '_[Seeds in the Heart: Japanese Literature from Earliest Times to the Late Sixteenth Century](!W)_' \
                                              -e '_[Neon Genesis Evangelion (manga)](!W)_' -e '_[Dendrocnide moroides](!W)_';
          }
    wrap λ "Markdown files: incorrect list nesting using italics for second-level list instead of smallcaps?" &

    λ(){ grep --with-filename --perl-regexp -e "[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]" $PAGES; }
    wrap λ "Markdown files: garbage or control characters detected?" &

    λ(){  find metadata/ -type f -name "*.html" -exec grep --with-filename --perl-regexp "[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]" {} \;; }
    wrap λ "Metadata HTML files: garbage or control characters detected?" &

    λ(){ ge -e '^- - https://en\.wikipedia\.org/wiki/' -- ./metadata/full.gtx; }
    wrap λ "Wikipedia annotations in GTX metadata database, but will be ignored by popups! Override with non-WP URL?" &

    λ(){ ge -e '^- - /[12][0-9][0-9]-[a-z]\.pdf$' -- ./metadata/*.gtx; }
    wrap λ "Wrong filepaths in GTX metadata database—missing prefix?" &

    λ(){ gec -e ' [0-9]*[02456789]th' -e ' [0-9]*[3]rd' -e ' [0-9]*[2]nd' -e ' [0-9]*[1]st' -- ./metadata/*.gtx | \
             gfv -e '%' -e '<figure>' -e 'alt="Figure ' -e http -e '- - /' -e "- - ! '" -e 'src=' -e "- - '#"; }
    wrap λ "Missing superscript abbreviations in GTX metadata database" &

    λ(){ ge -e 'up>T[Hh]<' -e 'up>R[Dd]<' -e 'up>N[Dd]<' -e 'up>S[Tt]<' -- ./metadata/*.gtx; }
    wrap λ "Superscript abbreviations are weirdly capitalized?" &

    λ(){ gf -e ' <sup>' -e ' <sub>' -e ' </sup>' -e ' </sub>' -- ./metadata/*.gtx | gfv -e ' <sup>242m</sup>Am' -e ' <sup>60</sup>Co' -e ' <sup>2</sup> This is because of the principle' -e ' <sup>3</sup> There are some who' -e ' <sup>4</sup> Such as setting' -e ' <sup>5</sup> Such as buying gifts' -e '  <sup>31</sup>P-Magnetic' -e ' <sup>242m</sup>Am' -e ' <sup>31</sup>P' -e ' <sup>60</sup>Co' -e ' <sup>31</sup>P-MRS' -e ' <sup>4</sup>He' ; }
    wrap λ "Superscripts/subscripts have spaces in front?" &

    λ(){ ge -e '<p><img ' -e '<img src="http' -e '<img src="[^h/].*"' ./metadata/*.gtx; }
    wrap λ "Check <figure> vs <img> usage, image hotlinking, non-absolute relative image paths in GTX metadata database" &

    λ(){ gf -e ' significant'  ./metadata/full.gtx; }
    wrap λ "Misleading language in full.gtx" &

    λ(){ gf -e '/doc/www/'  ./metadata/full.gtx; }
    wrap λ "Generated local archive links showing up in manual annotations." &

    λ(){ gf -e 'backlink/' -e 'metadata/annotation/' -e '?gi=' -- ./metadata/backlinks.hs; }
    wrap λ "Bad paths in backlinks databases: metadata paths are being annotated when they should not be!" &

    λ(){ ge -e '#[[:alnum:]]+#[[:alnum:]]+' -- ./metadata/*.hs ./metadata/*.gtx; }
    wrap λ "Bad paths in metadata databases: redundant anchors" &

    λ(){ find _site/ -type f -name "index" | gf -e '{#'; }
    wrap λ "Broken anchors in directory indexes." &

    λ(){ ls | gf -e '.pdf' -e '.jpg' -e '.png' 2> /dev/null; ls | ge -e '^[014-9]' 2> /dev/null; }
    wrap λ "Files in root wiki directory which should be in docs/ (perhaps a move gone awry)?" &

    λ(){ find ./ -type f -name '*gwner*' -or -name '*\.htm'; }
    wrap λ "Malformed filenames: dangerous strings in them?" &

    λ(){ find ./ -type f -wholename '*[^-a-zA-Z0-9_./~%#]*' | gfv -e 'cattleya幻想写景' -e '緑華野菜子'; }
    wrap λ "Malformed filenames: dangerous characters in them?" &

    λ(){ find . -type f | grep --perl-regexp -e "[^\x21-\x7E]" | gfv -e 'cattleya幻想写景' -e '緑華野菜子'; }
    wrap λ "Malformed filenames: dangerous non-ASCII/Unicode characters in them?" &

    λ(){ find ./ -type f -name "[12][0-9][0-9]-*" -or -name "[12][0-9][0-9][0-9][0-9]-*"; }
    wrap λ "Malformed filenames: year prefixes which have one too many or few digits?" &

    λ(){
        set +e;
        IFS=$(echo -en "\n\b");
        OTHERS="$(find metadata/annotation/ -name "*.html"; echo index)"
        for PAGE in $PAGES $OTHERS ./static/404; do
            HTML="${PAGE%.md}"
            TIDY=$(tidy -quiet -errors --fix-style-tags no --doctype html5 ./_site/"$HTML" >/dev/null 2>&1 | \
                       gfv -e '<link> proprietary attribute ' -e 'Warning: trimming empty <span>' \
                             -e "Error: missing quote mark for attribute value" -e 'Warning: <img> proprietary attribute "loading"' \
                             -e 'Warning: <svg> proprietary attribute "alt"' -e 'Warning: <source> proprietary attribute "alt"' \
                             -e 'Warning: missing <!DOCTYPE> declaration' -e 'Warning: inserting implicit <body>' \
                             -e "Warning: inserting missing 'title' element" -e 'Warning: <img> proprietary attribute "decoding"' \
                             -e 'Warning: <a> escaping malformed URI reference' -e 'Warning: <script> proprietary attribute "fetchpriority"' \
                             -e 'Warning: <img> lacks "alt" attribute' -e 'fix-style-tags: yes to move' )
            if [[ -n $TIDY ]]; then echo -e "\n\e[31m$PAGE\e[0m:\n$TIDY"; fi
        done

        set -e;
    }
    wrap λ "Markdown→HTML pages don't validate as HTML5" &

    ## anchor-checker.php doesn't work on HTML fragments, like the metadata annotations, and those rarely ever have within-fragment anchor links anyway, so skip those:
    λ() { for PAGE in $PAGES; do
              ANCHOR=$(./static/build/anchor-checker.php "$PAGE")
              if [[ -n $ANCHOR ]]; then echo -e "\n\e[31m$PAGE\e[0m:\n$ANCHOR"; fi
          done;
          }
    wrap λ "Anchors linked but not defined inside page?" &

    λ(){ find . -not -name "*#*" -xtype l -printf 'Broken symbolic link: %p\n'; }
    wrap λ "Broken symbolic links" &

    ## Is the remote server up?
    ping -q -c 5 gwern.net  &>/dev/null

    # Testing complete.
  fi

    # Sync:
    set -e
    wait;
    ## make sure nginx user can list all directories (x) and read all files (r)
    chmod a+x $(find ./ -type d) &
    chmod --recursive a+r ./* &
    ## sync to Hetzner server: (`--size-only` because Hakyll rebuilds mean that timestamps will always be different, forcing a slower rsync)
    ## If any links are symbolic links (such as to make the build smaller/faster), we make rsync follow the symbolic link (as if it were a hard link) and copy the file using `--copy-links`.
    ## NOTE: we skip time/size syncs because sometimes the infrastructure changes values but not file size, and it's confusing when JS/CSS doesn't get updated; since the infrastructure is so small (compared to eg. doc/*), just force a hash-based sync every time:
    bold "Syncing static/…"
    rsync --perms --exclude=".*" --exclude "*.hi" --exclude "*.o" --exclude "*.elc" --exclude '#*' --exclude='preprocess-markdown' --exclude 'generateLinkBibliography' --exclude='generateDirectory' --exclude='changeTag' --exclude='generateSimilar' --exclude='generateSimilarLinks' --exclude='hakyll' --exclude='guessTag' --exclude='changeTag' --exclude='link-extractor' --exclude='checkMetadata' --chmod='a+r' --recursive --checksum --copy-links --verbose --itemize-changes --stats ./static/ gwern@176.9.41.242:"/home/gwern/gwern.net/static" &
    ## Likewise, force checks of the Markdown pages but skip symlinks (ie. non-generated files):
    bold "Syncing pages…"
    rsync --perms --exclude=".*" --chmod='a+r' --recursive --checksum --quiet --info=skip0 ./_site/ gwern@176.9.41.242:"/home/gwern/gwern.net"
    ## Randomize sync type—usually, fast, but occasionally do a regular slow hash-based rsync which deletes old files:
    bold "Syncing everything else…"
    SPEED=""; if [ "$SLOW" ] || everyNDays 31; then SPEED="--delete --checksum"; else SPEED="--size-only"; fi
    rsync --perms --exclude=".*" --chmod='a+r' --recursive $SPEED --copy-links --verbose --itemize-changes --stats ./_site/ gwern@176.9.41.242:"/home/gwern/gwern.net" || true
    wait
    set +e

    bold "Expiring ≤100 updated files…"
    # expire CloudFlare cache to avoid hassle of manual expiration: (if more than 100, we've probably done some major systemic change & better to flush whole cache or otherwise investigate manually)
    # NOTE: 'bot-fighting' CloudFlare settings must be largely disabled, otherwise CF will simply CAPTCHA or block outright the various curl/linkchecker tests as 'bots'.
    EXPIRE="$(find . -type f -mtime -1 -not -wholename "*/\.*/*" -not -wholename "*/_*/*" | gfv -e '/doc/www' -e '/static/build/' -e '/static/template/' -e '/static/include/' -e '/metadata/annotation/backlink/' -e '/metadata/annotation/similar/' -e '.gtx' -e 'static/redirect/' -e 'static/nginx' -e '.hs' | xargs ls -t 2>/dev/null | sed -e 's/\.md$//' -e 's/^\.\/\(.*\)$/https:\/\/gwern\.net\/\1/' | head -50) https://gwern.net/sitemap.xml https://gwern.net/lorem https://gwern.net/ https://gwern.net/index https://gwern.net/metadata/today-quote.html https://gwern.net/metadata/today-annotation.html https://gwern.net/metadata/today-site.html"
    for URL in $EXPIRE; do
        echo -n "Expiring: $URL "
        ( curl --silent --request POST "https://api.cloudflare.com/client/v4/zones/57d8c26bc34c5cfa11749f1226e5da69/purge_cache" \
            --header "X-Auth-Email:gwern@gwern.net" \
            --header "Authorization: Bearer $CLOUDFLARE_CACHE_TOKEN" \
            --header "Content-Type: application/json" \
            --data "{\"files\":[\"$URL\"]}" > /dev/null; ) &
    done
    echo

 # test a random page modified in the past month for W3 validation & dead-link/anchor errors (HTML tidy misses some, it seems, and the W3 validator is difficult to install locally):
 if [ "$SLOW" ]; then
   CHECKED_URLS_FILE="./metadata/urls-linkchecker-checked.txt"
   # Filter URLs: Exclude URLs present in the checked list:
   FILTERED_PAGES=$(echo "$PAGES" | gfv -e '/fulltext' -e '/lorem' | sed -e 's/\.md$//' -e 's/^\.\/\(.*\)$/https:\/\/gwern\.net\/\1/' \
                        | while read -r URL; do
                        if ! grep -Fxq "$URL" "$CHECKED_URLS_FILE"; then
                            echo "$URL"
                        fi
                    done)

   CHECK_RANDOM_PAGE=$(echo "$FILTERED_PAGES" | shuf | head -1)
   echo "$CHECK_RANDOM_PAGE" >> "$CHECKED_URLS_FILE" # update checked-list
   CHECK_RANDOM_PAGE_ENCODED=$(echo "$CHECK_RANDOM_PAGE" | xargs urlencode)

   FILTERED_ANNOTATIONS=$(find metadata/annotation/ -maxdepth 1 -name "*.html" -type f -size +2k \
                 | sed -e 's/metadata\/annotation\/\(.*\)/\1/' \
                 | while read -r ANNOTATION_URL; do
                     if ! grep -Fxq "$ANNOTATION_URL" "$CHECKED_URLS_FILE"; then
                       echo "$ANNOTATION_URL"
                     fi
                   done; )
   CHECK_RANDOM_ANNOTATION_ENCODED=$(echo "$FILTERED_ANNOTATIONS" | shuf | head -1 | xargs urlencode | xargs urlencode | sed -e 's/^\(.*\)$/https:\/\/gwern\.net\/metadata\/annotation\/\1/'; ) # urlencode twice: once for the on-disk escaping, once for the URL argument to the W3C checker
   echo "$CHECK_RANDOM_ANNOTATION_ENCODED" >> "$CHECKED_URLS_FILE"

   ( curl --silent --request POST "https://api.cloudflare.com/client/v4/zones/57d8c26bc34c5cfa11749f1226e5da69/purge_cache" \
           --header "X-Auth-Email:gwern@gwern.net" \
           --header "Authorization: Bearer $CLOUDFLARE_CACHE_TOKEN" \
           --header "Content-Type: application/json" \
           --data "{\"files\":[\"$CHECK_RANDOM_PAGE\", \"$CHECK_RANDOM_ANNOTATION_ENCODED\"]}" > /dev/null; )

       # wait a bit for the CF cache to expire so it can refill with the latest version to be checked:
        everyNDays 7 && chromium "https://validator.w3.org/nu/?doc=$CHECK_RANDOM_PAGE_ENCODED" &
        sleep 5s; chromium "https://validator.w3.org/checklink?uri=$CHECK_RANDOM_PAGE_ENCODED&no_referer=on" &
        sleep 15s; chromium "https://validator.w3.org/checklink?uri=$CHECK_RANDOM_ANNOTATION_ENCODED&no_referer=on" &
        if everyNDays 100; then
            # check Google PageSpeed report for any regressions:
            chromium "https://pagespeed.web.dev/report?url=$CHECK_RANDOM_PAGE_ENCODED&form_factor=desktop" &
            bold "Checking print-mode CSS as well…"
            ## WARNING: we cannot simply use `--headless` & `--print-to-pdf` because Chrome does not, in fact, save the same PDF
            ## as it would if you print-to-PDF in-browser! It *mostly* does use the print CSS, but it skips things like transcludes
            ## that would correctly fire when you print in-browser, so is not useful for the purpose of checking for regressions.
            chromium "$CHECK_RANDOM_PAGE#reminder-print-out-and-check-the-page" && evince ~/"$TODAY"-gwernnet-printmode.pdf 2> /dev/null &
        fi

    (chromium --temp-profile "https://gwern.net/index#footer" &> /dev/null &) # check the x-of-the-day in a different & cache-free browser instance

    # once in a while, do a detailed check for accessibility issues using WAVE Web Accessibility Evaluation Tool:
    everyNDays 200 && chromium "https://wave.webaim.org/report#/$CHECK_RANDOM_PAGE" &

    # some of the live popups have probably broken, since websites keep adding X-FRAME options…
    everyNDays 50 && ghci -istatic/build/ ./static/build/LinkLive.hs -e 'linkLiveTestHeaders'

    # it is rare, but some duplicates might've crept into the X-of-the-day databases:
    everyNDays 30 && (runghc -istatic/build/ ./static/build/duplicatequotesitefinder.hs &)

    # Testing post-sync:
    bold "Checking MIME types, redirects, content…"
    c () { curl --compressed --silent --output /dev/null --head "$@"; }
    λ(){ cr () { [[ "$2" != $(c --location --write-out '%{url_effective}' "$1") ]] && echo "$1" "$2"; }
         cr 'https://gwern.net/DNM-archives' 'https://gwern.net/dnm-archive'
         cr 'https://gwern.net/doc/dnb/1978-zimmer.pdf' 'https://gwern.net/doc/music/music-distraction/1978-zimmer.pdf'
         cr 'https://gwern.net/AB%20testing' 'https://gwern.net/ab-test'
         cr 'https://gwern.net/Archiving%20URLs.html' 'https://gwern.net/archiving'
         cr 'https://gwern.net/Book-reviews' 'https://gwern.net/review/book'
         cr 'https://gwern.net/doc/ai/2019-10-21-gwern-gpt2-folkrnn-samples.ogg' 'https://gwern.net/doc/ai/music/2019-10-21-gwern-gpt2-folkrnn-samples.mp3';
         cr 'https://gwern.net/doc/sr/2013-06-07-premiumdutch-profile.htm' 'https://gwern.net/doc/darknet-market/silk-road/1/2013-06-07-premiumdutch-profile.html'
         cr 'https://gwern.net/doc/elections/2012-gwern-notes.txt' 'https://gwern.net/doc/statistics/prediction/election/2012-gwern-notes.txt'
         cr 'https://gwern.net/doc/statistics/peerreview/1976-rosenthal-experimenterexpectancyeffects-ch3.pdf' 'https://gwern.net/doc/statistics/peer-review/1976-rosenthal-experimenterexpectancyeffects-ch3.pdf'
         cr 'https://gwern.net/doc/longnow/form990-longnowfoundation-2001-12.pdf' 'https://gwern.net/doc/long-now/form990-longnowfoundation-2001-12.pdf'
         cr 'https://gwern.net/doc/eva/2011-house' 'https://gwern.net/doc/anime/eva/2011-house'
         cr 'https://gwern.net/doc/cs/1955-nash' 'https://gwern.net/doc/cs/cryptography/nash/1955-nash'
         cr 'https://gwern.net/doc/cs/cryptography/nash/1955-nash' 'https://gwern.net/doc/cs/cryptography/nash/1955-nash' # check www.gwern.net → gwern.net redirect
         cr 'https://gwern.net/dropcap.page' 'https://gwern.net/dropcap.md'

       }
    wrap λ "Check that some redirects go where they should"
    λ() { cm () { [[ "$1" != $(c --write-out '%{content_type}' "$2") ]] && echo "$1" "$2"; }
          ### check key pages:
          ## check every possible extension:
          ## check some arbitrary ones:
          cm "application/epub+zip" 'https://gwern.net/doc/anime/eva/notenki-memoirs/2002-takeda-notenkimemoirs.epub'
          cm "application/font-sfnt" 'https://gwern.net/static/font/dropcap/kanzlei/Kanzlei-Initialen-M.ttf'
          cm "application/javascript" 'https://gwern.net/doc/statistics/order/beanmachine-multistage/script.js'
          cm "application/javascript" 'https://gwern.net/static/js/rewrite.js'
          cm "application/javascript" 'https://gwern.net/static/js/sidenotes.js'
          cm "application/json" 'https://gwern.net/doc/touhou/2013-c84-downloads.json'
          cm "application/msword" 'https://gwern.net/doc/iq/2014-tenijenhuis-supplement.doc'
          cm "application/octet-stream" 'https://gwern.net/doc/zeo/firmware-v2.6.3R-zeo.img'
          cm "application/pdf" 'https://gwern.net/doc/cs/hardware/2010-bates.pdf'
          cm "application/pdf" 'https://gwern.net/doc/history/1694-gregory.pdf'
          cm "application/pdf" 'https://gwern.net/doc/statistics/decision/1994-benter.pdf'
          cm "application/vnd.ms-excel" 'https://gwern.net/doc/dual-n-back/2012-05-30-kundu-dnbrapm.xls'
          cm "application/vnd.oasis.opendocument.spreadsheet" 'https://gwern.net/doc/genetics/heritable/1980-osborne-twinsblackandwhite-appendix.ods'
          cm "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" 'https://gwern.net/doc/cs/hardware/2010-nordhaus-nordhaus2007twocenturiesofproductivitygrowthincomputing-appendix.xlsx'
          cm "application/vnd.openxmlformats-officedocument.wordprocessingml.document" 'https://gwern.net/doc/genetics/heritable/2015-mosing-supplement.docx'
          cm "application/x-maff" 'https://gwern.net/doc/anime/eva/2001-pulpmag-hernandez-2.html.maff'
          cm "application/x-shockwave-flash" 'https://gwern.net/doc/rotten.com/library/bio/entertainers/comic/patton-oswalt/patton.swf'
          cm "application/x-tar" 'https://gwern.net/doc/dual-n-back/2011-zhong.tar'
          cm "application/x-xz" 'https://gwern.net/doc/personal/2013-09-25-gwern-googlealertsemails.tar.xz'
          cm "application/zip" 'https://gwern.net/doc/statistics/bayes/2014-tenan-supplement.zip'
          cm "audio/mpeg" 'https://gwern.net/doc/history/1969-schirra-apollo11flighttothemoon.mp3'
          cm "audio/mpeg" 'https://gwern.net/doc/rotten.com/library/bio/crime/serial-killers/elmer-wayne-henley/areyouguilty.mp3'
          cm "image/gif" 'https://gwern.net/doc/gwern.net-gitstats/arrow-none.gif'
          cm "image/gif" 'https://gwern.net/doc/rotten.com/library/religion/creationism/creationism6.GIF'
          cm "image/jpeg" 'https://gwern.net/doc/personal/2011-gwern-yourmorals.org/schwartz_process.php_files/schwartz_graph.jpg'
          cm "image/jpeg" 'https://gwern.net/doc/rotten.com/library/bio/pornographers/al-goldstein/goldstein-fuck-you.jpeg'
          cm "image/jpeg" 'https://gwern.net/doc/rotten.com/library/religion/heresy/circumcellions/circumcellions-augustine.JPG'
          cm "image/png" 'https://gwern.net/doc/statistics/order/beanmachine-multistage/beanmachine-demo.png'
          cm "image/png" 'https://gwern.net/static/img/logo/logo.png'
          cm "image/svg+xml" 'https://gwern.net/doc/psychology/spaced-repetition/gwern-forgetting-curves.svg'
          cm "image/svg+xml" 'https://gwern.net/static/img/logo/logo-smooth.svg'
          cm "image/svg+xml" 'https://gwern.net/static/img/icon/alcor.svg'
          cm "image/svg+xml" 'https://gwern.net/doc/genetics/selection/www.mountimprobable.com/assets/images/verm_darkeryellow.svg'
          cm "image/svg+xml" 'https://gwern.net/static/img/triple-question-mark.svg'
          cm "image/svg+xml" 'https://gwern.net/static/img/icon/arrow-up.svg'
          cm "image/x-icon" 'https://gwern.net/static/img/favicon.ico'
          cm "image/x-ms-bmp" 'https://gwern.net/doc/rotten.com/library/bio/hackers/robert-morris/morris.bmp'
          cm "image/x-xcf" 'https://gwern.net/doc/personal/businesscard-front-draft.xcf'
          cm "message/rfc822" 'https://gwern.net/doc/cs/linkrot/2009-08-20-b3ta-fujitsuhtml.mht'
          cm "text/css; charset=utf-8" 'https://gwern.net/doc/gwern.net-gitstats/gitstats.css'
          cm "text/css; charset=utf-8" 'https://gwern.net/doc/statistics/order/beanmachine-multistage/offsets.css'
          cm "text/css; charset=utf-8" 'https://gwern.net/doc/statistics/order/beanmachine-multistage/style.css'
          cm "text/css; charset=utf-8" 'https://gwern.net/static/css/default.css'
          cm "text/css; charset=utf-8" 'https://gwern.net/static/css/fonts.css'
          cm "text/css; charset=utf-8" 'https://gwern.net/static/css/initial.css'
          cm "text/css; charset=utf-8" 'https://gwern.net/static/css/links.css'
          cm "text/css; charset=utf-8" 'https://gwern.net/static/css/colors.css'
          cm "text/csv; charset=utf-8" 'https://gwern.net/doc/statistics/2013-google-index.csv'
          cm "text/html" 'https://gwern.net/atom.xml'
          cm "text/html; charset=utf-8" 'https://gwern.net/doc/cs/security/2012-terencetao-anonymity.html'
          cm "text/html; charset=utf-8" 'https://gwern.net/doc/darknet-market/silk-road/1/2013-06-07-premiumdutch-profile.html'
          cm "text/html; charset=utf-8" 'https://gwern.net/'
          cm "text/html; charset=utf-8" 'https://gwern.net/note/attention'
          cm "text/html; charset=utf-8" 'https://gwern.net/note/faster'
          cm "text/html; charset=utf-8" 'https://gwern.net/nootropic/magnesium'
          cm "text/html; charset=utf-8" 'https://gwern.net/zeo/co2'
          cm "text/html; charset=utf-8" 'https://gwern.net/review/book'
          cm "text/html; charset=utf-8" 'https://gwern.net/review/anime'
          cm "text/html; charset=utf-8" 'https://gwern.net/review/movie'
          cm "text/html; charset=utf-8" 'https://gwern.net/doc/existential-risk/1985-hofstadter'
          cm "text/html; charset=utf-8" 'https://gwern.net/review/bakewell'
          cm "text/html; charset=utf-8" 'https://gwern.net/backfire-effect'
          cm "text/html; charset=utf-8" 'https://gwern.net/console-insurance'
          cm "text/xml" 'https://gwern.net/sitemap.xml'
          cm "text/markdown; charset=utf-8" 'https://gwern.net/2014-spirulina.md'
          cm "text/markdown; charset=utf-8" 'https://gwern.net/dnm-archive.md'
          cm "text/markdown; charset=utf-8" 'https://gwern.net/gpt-3.md'
          cm "text/markdown; charset=utf-8" 'https://gwern.net/catitecture.md'
          cm "text/plain; charset=utf-8" 'https://gwern.net/doc/personal/2009-sleep.txt'
          cm "text/plain; charset=utf-8" 'https://gwern.net/static/redirect/nginx.conf'
          cm "text/x-adobe-acrobat-drm" 'https://gwern.net/doc/dual-n-back/2012-zhong.ebt'
          cm "text/x-haskell; charset=utf-8" 'https://gwern.net/static/build/hakyll.hs'
          cm "text/x-opml; charset=utf-8" 'https://gwern.net/doc/personal/rss-subscriptions.opml'
          cm "text/x-patch; charset=utf-8" 'https://gwern.net/doc/ai/music/2019-12-22-gpt2-preferencelearning-gwern-abcmusic.patch'
          cm "text/x-r; charset=utf-8" 'https://gwern.net/static/build/linkAbstract.R'
          cm "text/plain; charset=utf-8" 'https://gwern.net/static/build/linkArchive.sh'
          cm "text/x-gtx; charset=utf-8" 'https://gwern.net/metadata/full.gtx'
          cm "video/mp4"  'https://gwern.net/doc/genetics/selection/artificial/2019-coop-illinoislongtermselectionexperiment-responsetoselection-animation.mp4'
          cm "video/webm" 'https://gwern.net/doc/statistics/2003-gwern-murray-humanaccomplishment-region-proportions-bootstrap.webm'
          cm "image/jpeg" 'https://gwern.net/doc/cs/security/lobel-frogandtoadtogether-thebox-crop.jpg'
          cm "image/png"  'https://gwern.net/doc/technology/google/gwern-googlesearch-tools-daterange.png'
          cm "image/png"  'https://gwern.net/doc/technology/google/gwern-15-predicted-survivorship-curves.png'
          cm "application/wasm"  'https://gwern.net/static/js/patterns/en-us.wasm'
          # special-case rewrite, handling double-slashed prefixes:
          cm "application/pdf" 'https://gwern.net//doc/sociology/2022-yuan.pdf'
          cm "text/html; charset=utf-8" 'https://gwern.net//index'
        }
    wrap λ "The live MIME types are incorrect" &

    ## known-content check:
    elinks -dump 'https://gwern.net/index'   | gf --quiet -e 'This Is The Website' || red "/index content-check failed"
    elinks -dump 'https://gwern.net/zeo/zeo' | gf --quiet -e 'lithium orotate'     || red "/zeo/zeo content-check failed"

    ## check that tag-directories have the right thumbnails (ie. *not* the fallback thumbnail):
    λ(){ curl --silent 'https://gwern.net/doc/sociology/index' 'https://gwern.net/doc/psychology/index' 'https://gwern.net/doc/economics/index' | \
             gf 'https://gwern.net/static/img/logo/logo-whitebg-large-border.png'; }
    wrap λ "Tag-directories missing their automatically-extracted-from-annotation thumbnails & instead having the site-wide default thumbnail?"

    ## Check that password protection is working:
    λ() { URL="https://gwern.net/private/canary"
          USERNAME="guest"
          PASSWORD="password"
          CANARY_TOKEN_SUBSTRING="c1e1908dac358d40e7e2"

          # Use curl with & without basic authentication to download the webpage:
          RESPONSE=$(curl --silent --user "$USERNAME:$PASSWORD" "$URL"; curl --silent "$URL")

          # Check if the canary token is present in the downloaded content:
          if echo "$RESPONSE" | gf --quiet "$CANARY_TOKEN_SUBSTRING"; then
              echo "Error: Canary token found! $RESPONSE"
          fi
    }
    wrap λ "Canary token was downloadable; nginx password-protection security failed?"

    λ() { OUTPUT=$(./static/nginx/memoriam.sh)
          if [[ ! -z "$OUTPUT" ]]; then
              HEADER=$(curl --head --silent -- 'https://gwern.net/index' | gf --ignore-case 'X-Clacks-Overhead')
              if [[ -z "$HEADER" ]]; then
                  echo "$OUTPUT"
              fi
          fi; }
    wrap λ "X-Clacks-Overhead HTTP header check failed."

    ## did any of the key pages mysteriously vanish from the live version?
    linkchecker --ignore-url='https://www.googletagmanager.com' --threads=5 --check-extern --recursion-level=1 'https://gwern.net/' &
    ## - traffic checks/alerts are done in Google Analytics: alerts on <900 pageviews/daily, <40s average session length/daily.
    ## - latency/downtime checks are done in `updown.io` (every 1h, 1s response-time for /index)
    set +e
  fi

    # Cleanup post:
    rm --recursive --force -- ./_cache/ ./_site/ || true

  if [ "$SLOW" ]; then
    # Testing files, post-sync
    bold "Checking for file anomalies…"
    λ(){ fdupes --quiet --sameline --size --nohidden $(find ./* -type d | gev -e 'static' -e '.git' -e 'gwern/wiki/$' -e 'metadata/annotation/backlink' -e 'metadata/annotation/similar' -e 'metadata/annotation/link-bibliography' -e 'doc/www/') | gfv -e 'bytes each' -e 'trimfill.png' | gev -e 'doc/www/.*/.*\.woff2'; }
    wrap λ "Duplicate file check"

    λ(){ find ./ -type f | gfv -e 'git/' -e 'newsletter/' -e 'doc/rotten.com/' -e 'doc/www/' -e 'metadata/annotation/' -e 'doc/personal/2011-gwern-yourmorals.org/' -e 'index.md' -e 'index.html' -e 'favicon.ico' -e 'generator_config.txt' -e '.gitignore' -e 'static/build/Config/' -e 'font/dropcap/' | xargs --max-procs=0 --max-args=1 basename  | sort | uniq --count | gev -e '^ +1 ' | sort --numeric-sort; }
    wrap λ "File base names are preferably globally-unique, to avoid issues with duplicate search results and clashing link IDs."

    λ() { find . -perm u=r -path '.git' -prune; }
    wrap λ "Read-only file check" ## check for read-only outside ./.git/ (weird but happened):

    λ() { find . -executable -type f | gfv -e 'static/build/' -e 'nginx/memoriam.sh' -e 'haskell/lcp.hs' -e '.git/hooks/post-commit'; }
    wrap λ "Executable bit set on files that shouldn't be executable?"

    λ(){ gf -e 'RealObjects' -e '404 Not Found Error: No Page' -e '403 Forbidden' ./metadata/auto.gtx; }
    wrap λ "Broken links, corrupt authors, or failed scrapes in auto.gtx."

    λ(){ (find . -type f -name "*--*"; find . -type f -name "*~*"; ) | gfv -e metadata/annotation/; }
    wrap λ "No files should have double hyphens or tildes in their names."

    λ(){ gf --before-context=1 -e 'Right Nothing' -e 'Just ""' -e '//"' ./metadata/archive.hs; }
    wrap λ "Links failed to archive (broken)."

    λ(){ find . -type f | gfv -e '.'; }
    wrap λ "Every file should have at least one period in them (extension)."

    λ(){ find . -type f -name "*\.*\.md" | gfv -e '404.md'; }
    wrap λ "Markdown files should have exactly one period in them."

    λ(){ find . -type f -name "*.html.html.html"; }
    wrap λ "Found a triple-.html HTML file; weird! Syntax-highlighting gone astray?"

    λ(){ find . -type f -mtime +2 -name "*#*" -or -type f -name "temp[0-9]*"; }
    wrap λ "Stale temporary files?"

    λ(){ find ./* -type d -empty; }
    wrap λ "Unused or empty directories?"

    bold "Checking for HTML anomalies…"
    λ(){ BROKEN_HTMLS="$(find ./ -type f -mtime -31 -name "*.html" | gfv 'static/' | \
                         parallel --max-args=500 "gf --ignore-case --files-with-matches \
                         -e '404 Not Found' -e '<title>Sign in - Google Accounts</title' -e 'Download Limit Exceeded' -e 'Access Denied' -e '403 Forbidden'")"
         for BROKEN_HTML in $BROKEN_HTMLS; do
             grep --before-context=3 "$BROKEN_HTML" ./metadata/archive.hs | gfv -e 'Right' -e 'Just';
         done; }
    wrap λ "Archives of broken links"

    ## 'file' throws a lot of false negatives on HTML pages, often detecting XML and/or ASCII instead, so we whitelist some:
    λ(){ find ./ -type f -mtime -31 -name "*.html" | gfv -e '4a4187fdcd0c848285640ce9842ebdf1bf179369' -e '5fda79427f76747234982154aad027034ddf5309' \
                                                -e 'f0cab2b23e1929d87f060beee71f339505da5cad' -e 'a9abc8e6fcade0e4c49d531c7d9de11aaea37fe5' \
                                                -e '2015-01-15-outlawmarket-index.html' -e 'ac4f5ed5051405ddbb7deabae2bce48b7f43174c.html' \
                                                -e '%3FDaicon-videos.html' -e '86600697f8fd73d008d8383ff4878c25eda28473.html' \
                                                -e '16aacaabe05dfc07c0e966b994d7dd0a727cd90e' -e 'metadata/today-quote.html' -e 'metadata/today-annotation.html' \
                                                -e '023a48cb80d48b1438d2accbceb5dc8ad01e8e02' -e '/Starr_Report/' -e '88b3f6424a0b31dcd388ef8364b11097e228b809.html' \
                                                -e '7f81f4ef122b83724448beb1f585025dbc8505d0' \
             | parallel --max-args=500 file | gfv -e 'HTML document, ' -e 'ASCII text' -e 'LaTeX document, UTF-8 Unicode text'; }
    wrap λ "Corrupted filetype HTMLs" &

    ## having noindex tags causes conflicts with the robots.txt and throws SEO errors; except in the ./doc/www/ mirrors, where we don't want them to be crawled:
    λ(){ find ./ -type f -mtime -31 -name "*.html" | gfv -e './doc/www/' -e './static/404' -e './static/template/default.html' -e 'lucky-luciano' | parallel gf --files-with-matches 'noindex'; }
    wrap λ "Noindex tags detected in HTML pages."

    λ(){ find ./doc/www/ -type f | gfv -e '.html' -e '.pdf' -e '.txt' -e 'www/misc/' -e '.gif' -e '.mp4' -e '.png' -e '.jpg' -e '.dat' -e '.bak' -e '.woff' -e '.webp' -e '.ico' -e '.svg' -e '.ttf' -e '.otf' -e '.js' -e '.mp3' -e '.ogg' -e '.wav' -e '.webm'; }
    wrap λ "Unexpected filetypes in /doc/www/ WWW archives."

    λ(){ find . -type f -name "*.txt" | parallel file | gf " CRLF"; }
    wrap λ "Corrupted text file? use 'dos2unix' on it."

    bold "Checking for PDF anomalies…"
    λ(){ BROKEN_PDFS="$(find ./ -type f -mtime -31 -name "*.pdf" -not -size 0 | parallel --max-args=500 file | \
                                grep --invert-match 'PDF document' | cut -d ':' -f 1)"
         for BROKEN_PDF in $BROKEN_PDFS; do
             echo "$BROKEN_PDF"; grep --before-context=3 "$BROKEN_PDF" ./metadata/archive.hs;
         done; }
    wrap λ "Corrupted or broken PDFs" &

    λ(){
        LL="$(curl --silent https://ifconfig.me/ip)"
        export LL # some academic publishers inject spam, but not in an easy-to-grep way; I've noticed they sometimes insert the download IP, however, so we add that dynamically as a search string.
        checkSpamHeader () {
            # extract text from first page, where the junk usually is (some insert at the end, but those are less of an issue & harder to check):
            HEADER=$(pdftotext -f 1 -l 1 "$@" - 2> /dev/null | \
                         gf -e 'INFORMATION TO USERS' -e 'Your use of the JSTOR archive indicates your acceptance of JSTOR' \
                               -e 'This PDF document was made available from www.rand.org as a public' -e 'A journal for the publication of original scientific research' \
                               -e 'This is a PDF file of an unedited manuscript that has been accepted for publication.' \
                               -e 'Additional services and information for ' -e 'Access to this document was granted through an Emerald subscription' \
                               -e 'PLEASE SCROLL DOWN FOR ARTICLE' -e 'ZEW Discussion Papers' -e "$LL" -e 'eScholarship.org' \
                  -e 'Full Terms & Conditions of access and use can be found at' -e 'This paper is posted at DigitalCommons' -e 'This paper is included in the Proceedings of the' \
                  -e 'See discussions, stats, and author profiles for this publication at' \
                  -e 'This article appeared in a journal published by Elsevier. The attached' \
                  -e 'The user has requested enhancement of the downloaded file' \
                  -e 'See discussions, stats, and author profiles for this publication at: https://www.researchgate.net' \
                  -e 'This paper has been accepted for publication in '\
                  -e 'Online First Publication, ' )
            if [ "$HEADER" != "" ]; then echo "Header: $@"; fi;
        }
        export -f checkSpamHeader
        find ./doc/ -type f -mtime -31 -name "*.pdf" | gfv -e 'doc/www/' | parallel checkSpamHeader
    }
    wrap λ "Remove academic-publisher wrapper junk from PDFs using 'pdfcut'. (Reminder: can use 'pdfcut-append' to move low-quality-but-not-deletion-worthy initial pages to the end.)" &

    removeEncryption () { ENCRYPTION=$(exiftool -quiet -quiet -Encryption "$@");
                          if [ "$ENCRYPTION" != "" ]; then
                              echo "Stripping encryption from $@…"
                              TEMP=$(mktemp /tmp/encrypted-XXXX.pdf)
                              pdftk "$@" input_pw output "$TEMP" && mv "$TEMP" "$@";
                          fi; }
    export -f removeEncryption
    find ./ -type f -mtime -31 -name "*.pdf" -not -size 0 | parallel removeEncryption &

    λ(){ find ./ -type f -name "*.djvu"; }
    wrap λ "Legacy DjVu detected (convert to JBIG2 PDF; see <https://gwern.net/design-graveyard#djvu-files>)."

    bold "Checking for image anomalies…"
    λ(){ find ./doc/ -type f -mtime -31 -name "*.jpg" | parallel --max-args=500 file | gfv 'JPEG image data'; }
    wrap λ "Corrupted JPGs" &

    λ(){ find ./doc/ -type f -mtime -31 -name "*.png" | parallel --max-args=500 file | gfv 'PNG image data'; }
    wrap λ "Corrupted PNGs" &

    λ(){ find ./ -name "*.svg" | xargs --max-procs="$N" -I {} sh -c 'xmllint --noout "{}" 2>/dev/null && \
                                identify "{}" >/dev/null 2>&1 || echo "{}"'; }
    wrap λ "Corrupted SVGs" &

    λ(){  find ./doc/ -type f -mtime -31 -name "*.png" | gfv -e '/static/img/' -e '/doc/www/misc/' | \
              xargs identify -format '%F %[opaque]\n' | gf ' false' | cut --delimiter=' ' --field=1 | \
              xargs mogrify -background white -alpha remove -alpha off; }
    wrap λ "Partially transparent PNGs (may break in dark mode, converting with 'mogrify -background white -alpha remove -alpha off'…)" &

    λ(){ find ./ -type f -name "*.gif" | gfv -e 'static/img/' -e 'doc/gwern.net-gitstats/' -e 'doc/rotten.com/' -e 'doc/genetics/selection/www.mountimprobable.com/' -e 'doc/www/' | \
             parallel --max-args=500 identify | ge '\.gif\[[0-9]\] '; }
    wrap λ "Animated GIF is deprecated; GIFs should be converted to WebMs/MP4s."

    λ(){ JPGS_BIG="$(find ./doc/ -type f -mtime -31 -name "*.jpg" | gfv -e 'doc/www/misc/' | parallel --max-args=500 "identify -format '%Q %F\n'" {} | sort --numeric-sort | ge -e '^[7-9][0-9] ' -e '^6[6-9]' -e '^100')"
         echo "$JPGS_BIG"
         compressJPG2 $(echo "$JPGS_BIG" | cut --delimiter=' ' --field=2); }
    wrap λ "Compressing high-quality JPGs to ≤65% quality…" &

    bold "Compressing new PNGs…"
    png.sh $(find ./doc/ -type f -name "*.png" -mtime -3 | gfv -e './doc/www/misc/') &> /dev/null &

    # because GIF videos are *so* big, we lossily-compress GIFs in the WWW split archives using `gifsicle`
    bold "Compressing new GIFs…"
    optimize_gif() { # update the original GIF only if >10% size reduction; NOTE: this also avoids the issue where `gifsicle` always changes the file metadata even if no real change was made (which is a WONTFIX by the maintainer: <https://github.com/kohler/gifsicle/issues/201>).
      local gif="$1"

      if [ ! -f "$gif" ]; then
        return
      fi

      local temp_gif=$(mktemp)

      gifsicle --colors=256 --optimize=3 "$gif" > "$temp_gif" 2>/dev/null

      local original_size=$(stat --printf="%s" "$gif")
      local optimized_size=$(stat --printf="%s" "$temp_gif")

      local size_diff=$((original_size - optimized_size))
      local min_reduction=$(echo "scale=0; $original_size * 0.1 / 1" | bc)

      if [ "$size_diff" -ge "$min_reduction" ]; then
        mv "$temp_gif" "$gif" && echo "Optimized $gif"
      else
        rm "$temp_gif"
      fi
    }
    export -f optimize_gif
    find ./doc/www/ -type f -name "*.gif" -mtime -3 | parallel optimize_gif

    ## Find JPGS which are too wide (1600px is an entire screen width on even wide monitors, which is too large for a figure/illustration):
    ## TODO: images currently sit uneasily between 'archival' full-resolution originals (suitable for research/design/close examination) and 'optimized' web images for pleasant fast efficient browsing.
    ## We should probably return to a regular image thumbnail system, so we never downscale the originals, and serve appropriate thumbnails instead.
    λ() { for IMAGE in $(find ./doc/ -type f -mtime -31 -name "*.jpg" -or -name "*.png" | gfv -e 'doc/www/' -e '2020-07-19-oceaninthemiddleofanisland-gpt3-chinesepoetrytranslation.png' -e '2020-05-22-caji9-deviantart-stylegan-ahegao.jpg' -e '2021-anonymous-meme-virginvschad-journalpapervsblogpost.jpg' -e 'tadne-l4rz-kmeans-k256-n120k-centroidsamples.jpg' -e '2009-august-newtype-rebuildinterview-maayasakamoto-pg090091.jpg' -e 'doc/fiction/science-fiction/batman/' -e 'dall-e' -e 'midjourney' -e 'stablediffusion' -e '2022-09-27-gwern-gwernnet-indentjustification2x2abtest.png' -e 'reinforcement-learning/2022-bakhtin' -e 'technology/2021-roberts-figure2' -e '2022-10-02-mollywhite-annotate-latecomersdesktopscreenshot.png' -e '/doc/anime/eva/' -e 'doc/www/misc/' -e '2021-power-poster.png' -e '2002-change-table2-preandposttestscoresultsfrommindmappingshowminimaleffect.png' -e 'genetics/selection/www.mountimprobable.com/assets/images/card.png' -e 'reinforcement-learning/imperfect-information/diplomacy/2022-bakhtin-figure6-successfulcicerohumandialogueexamplesfromtestgames.jpg' -e 'reinforcement-learning/imperfect-information/diplomacy/2022-bakhtin-figure3-differentcicerointentsleadtodifferentdialogues.jpg' -e 'reinforcement-learning/imperfect-information/diplomacy/2022-bakhtin-figure5-theeffectofdialogueoncicerosplanningandintents3possiblescenariosinanegotiationwithengland.jpg' -e 'reinforcement-learning/imperfect-information/diplomacy/2022-bakhtin-figure2-trainingandinferenceofcicerointentcontrolleddialogue.jpg' -e 'reinforcement-learning/imperfect-information/diplomacy/2022-bakhtin-figure1-architectureofcicerodiplomacyagent.jpg' -e '2021-roberts-figure2-manufacturingofhumanbloodbricks.jpg' -e 'gwern-gwernnet' -e '2023-11-03-gwern-googleimages-catwindowbox-imagequilt.jpg' -e '1999-marklombardi-olivernorthlakeresourcespanamairancontra198486-v4-detail.jpg' ); do
              SIZE_W=$(identify -format "%w" "$IMAGE")
              if (( SIZE_W > 1700 )); then
                  echo "Too wide image: $IMAGE $SIZE_W; shrinking…";
                  mogrify -resize 1700x10000 "$IMAGE";
              fi;
          done; }
    wrap λ "Too-wide images (downscale)" &

    λ() { find doc/ -type f -mtime -31 -name "*.png" | gfv -e 'static/' -e 'doc/www/' | parallel png2JPGQualityCheck; }
    wrap λ "PNGs that should be JPGs?" &

    bold "Running site functionality checks…"
    ## Look for domains that may benefit from link icons or link live status now:
    λ() { ghci -istatic/build/ ./static/build/LinkIcon.hs  -e 'linkIconPrioritize' | gfv -e ' secs,' -e 'it :: [(Int, T.Text)]' -e '[]'; }
    wrap λ "Need link icons?" &
    λ() { ghci -istatic/build/ ./static/build/LinkLive.hs  -e 'linkLivePrioritize' | gfv -e ' secs,' -e 'it :: [(Int, T.Text)]' -e '[]'; }
    wrap λ "Need link live whitelist/blacklisting?" &

    λ() { ghci -istatic/build/ ./static/build/LinkBacklink.hs  -e 'suggestAnchorsToSplitOut' | gfv -e ' secs,' -e 'it :: [(Int, T.Text)]' -e '[]'; }
    wrap λ "Refactor out pages?" &

    λ() { find ./metadata/annotation/similar/ -type f -name "*.html" | xargs --max-args=1000 grep -F --no-filename -e '<a href="' -- | sort | uniq --count | sort --numeric-sort | ge '^ +[4-9][0-9][0-9][0-9]+ +'; }
    wrap λ "Similar-links: overused links (>999) indicate pathological lookups; blacklist links as necessary." &

    λ(){ ghci -istatic/build/ ./static/build/XOfTheDay.hs -e 'sitePrioritize' | \
             gfv -e ' secs,' -e 'it :: [T.Text]' -e '[]' || true; }
    wrap λ "Site-of-the-day: check for recommendations?" &

    λ() { (cd ./static/build/ && find ./ -type f -name "*.hs" -exec ghc -O0 $WARNINGS -fno-code {} \; ) >/dev/null; }
    wrap λ "Test-compilation of all Haskell files in static/build: failure." &

    λ() { find ./static/build/ -type f -name "*.hs" -exec grep -F 'nub ' {} \; ; }
    wrap λ "Haskell blacklist functions: 'nub' (use 'Data.Containers.ListUtils.nubOrd' for safety instead)."

    # if the first of the month, download all pages and check that they have the right MIME type and are not suspiciously small or redirects.
    if [ "$(date +"%d")" == "1" ]; then

        bold "Checking all MIME types…"
        c () { curl --compressed --silent --output /dev/null --head "$@"; }
        for PAGE in $PAGES; do
            MIME=$(c --max-redirs 0 --write-out '%{content_type}' "https://gwern.net/$PAGE")
            if [ "$MIME" != "text/html; charset=utf-8" ]; then red "$PAGE : $MIME"; fi

            SIZE=$(curl --max-redirs 0 --compressed --silent "https://gwern.net/$PAGE" | wc --bytes)
            if [ "$SIZE" -lt 7500 ]; then red "$PAGE : $SIZE : $MIME"; fi
        done

        # check for any pages that could use multi-columns now:
        λ(){ (find . -name "*.md"; find ./metadata/annotation/ -maxdepth 1 -name "*.html") | \
                 parallel --max-args=500 runghc -istatic/build/ ./static/build/Columns.hs --print-filenames; }
        wrap λ "Multi-columns use?"
    fi
    # if the end of the month, expire all of the annotations to get rid of stale ones:
    if [ "$(date +"%d")" == "31" ]; then
        find ./metadata/annotation/ -maxdepth 1 -name "*.html" -delete
    fi

    # once a year, check all on-site local links to make sure they point to the true current URL; this avoids excess redirects and various possible bugs (such as an annotation not being applied because it's defined for the true current URL but not the various old ones, or going through HTTP nginx redirects first)
    if [ "$(date +"%j")" == "002" ]; then
        bold "Checking all URLs for redirects…"
        for URL in $(find . -type f -name "*.md" | parallel --max-args=500 runghc -istatic/build/ ./static/build/link-extractor.hs | \
                         ge -e '^/' | cut --delimiter=' ' --field=1 | sort --unique); do
            echo "$URL"
            MIME=$(curl --silent --max-redirs 0 --output /dev/null --write '%{content_type}' "https://gwern.net$URL");
            if [[ "$MIME" == "" ]]; then red "redirect! $URL (MIME: $MIME)"; fi;
        done

        for URL in $(find . -type f -name "*.md" | parallel --max-args=500 runghc -istatic/build/ ./static/build/link-extractor.hs | \
                         ge -e '^https://gwern.net' | sort --unique); do
            MIME=$(curl --silent --max-redirs 0 --output /dev/null --write '%{content_type}' "$URL");
            if [[ "$MIME" == "" ]]; then red "redirect! $URL"; fi;
        done
    fi
  fi

    rm static/build/generateLinkBibliography static/build/*.hi static/build/*.o &>/dev/null || true
    wait

    bold "Sync successful: $(date)"
    echo -n -e '\a'; # ring bell because done
fi
