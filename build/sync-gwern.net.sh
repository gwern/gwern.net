#!/bin/bash

# Author: Gwern Branwen
# Date: 2016-10-01
# When:  Time-stamp: "2023-08-30 10:16:17 gwern"
# License: CC-0
#
# sync-gwern.net.sh: shell script which automates a full build and sync of Gwern.net. A simple build
# can be done using 'runghc hakyll.hs build', but that is slow, semi-error-prone (did you
# remember to delete all intermediates?), and does no sanity checks or optimizations like compiling
# the MathJax to static CSS/fonts (avoiding multi-second JS delays).
#
# This script automates all of that: it cleans up, compiles a hakyll binary for faster compilation,
# generates a sitemap XML file, optimizes the MathJax use, checks for many kinds of errors, uploads,
# and cleans up.

bold () { echo -e "\033[1m$@\033[0m"; }
red  () { echo -e "\e[41m$@\e[0m"; }
## function to wrap checks and print red-highlighted warning if non-zero output (self-documenting):
wrap () { OUTPUT=$($1 2>&1)
          WARN="$2"
          if [ -n "$OUTPUT" ]; then
              echo -n "Begin: "; red "$WARN";
              echo -e "$OUTPUT";
              echo -n "End: "; red "$WARN";
          fi; }
ge () { grep -E --color=always "$@"; }
gf () { grep -F --color=always "$@"; }

# key dependencies: GHC, Hakyll, s3cmd, emacs, curl, tidy (HTML5 version), urlencode
# ('gridsite-clients' package), linkchecker, fdupes, ImageMagick, exiftool, mathjax-node-page (eg.
# `npm i -g mathjax-node-page`), parallel, xargs, php7…

if ! [[ -n $(command -v ghc) && -n $(command -v git) && -n $(command -v rsync) && -n $(command -v curl) && -n $(command -v ping) && \
          -n $(command -v tidy) && -n $(command -v linkchecker) && -n $(command -v du) && -n $(command -v rm) && -n $(command -v find) && \
          -n $(command -v fdupes) && -n $(command -v urlencode) && -n $(command -v sed) && -n $(command -v parallel) && -n $(command -v xargs) && \
          -n $(command -v file) && -n $(command -v exiftool) && -n $(command -v identify) && -n $(command -v pdftotext) && \
          -n $(command -v ~/src/node_modules/mathjax-node-page/bin/mjpage) && -n $(command -v static/build/link-extractor.hs) && \
          -n $(command -v static/build/anchor-checker.php) && -n $(command -v php) && -n $(command -v static/build/generateDirectory.hs) && \
          -n $(command -v static/build/generateLinkBibliography.hs) && \
          -n $(command -v static/build/generateBacklinks.hs) && \
          -n $(command -v static/build/generateSimilarLinks.hs) ]] && \
       [ -z "$(pgrep hakyll)" ];
then
    red "Dependencies missing or Hakyll already running?"
else
    set -e

    # lower priority of everything we run (some of it is expensive):
    renice --priority 19 --pid "$$" &>/dev/null
    ionice --class 3     --pid "$$" &>/dev/null

    ## Parallelization: WARNING: post-2022-03 Hakyll uses parallelism which catastrophically slows down at >= # of physical cores; see <https://groups.google.com/g/hakyll/c/5_evK9wCb7M/m/3oQYlX9PAAAJ>
    N="29" # "$(if [ ${#} == 0 ]; then echo 29; else echo "$1"; fi)"
    if [ "$1" == "--slow" ]; then export SLOW="--slow"; else SLOW=""; fi

    if [ "$SLOW" ]; then (cd ~/wiki/ && git status) || true; fi &
    bold "Pulling infrastructure updates…"
    # pull from Obormot's repo, with his edits overriding mine in any conflict (`-Xtheirs`) & auto-merging with the default patch text (`--no-edit`), to make sure we have the latest JS/CSS. (This is a bit tricky because the use of versioning in the includes means we get a lot of merge conflicts, for some reason.)
    (cd ./static/ && git status && timeout 10m git pull -Xtheirs --no-edit --verbose 'https://gwern.obormot.net/static/.git/' master) || true

    if [ "$SLOW" ]; then
        bold "Executing string rewrite cleanups…" # automatically clean up some Gwern.net bad URL patterns, typos, inconsistencies, house-styles:
        ( s() { gwsed "$@"; }
          ## domain rewrites:
          s 'https://mobile.twitter.com' 'https://twitter.com'; s 'https://www.twitter.com' 'https://twitter.com'; s 'https://en.reddit.com/' 'https://www.reddit.com/'; s 'https://en.m.wikipedia.org/' 'https://en.wikipedia.org/'; s 'https://www.greaterwrong.com/posts/' 'https://www.lesswrong.com/posts'; s 'http://web.archive.org/web/' 'https://web.archive.org/web/'; s 'https://youtu.be/' 'https://www.youtube.com/watch?v='; s 'http://arxiv.org' 'https://arxiv.org'; s 'https://deepmind.com' 'https://www.deepmind.com'; s 'http://en.wikipedia.org' 'https://en.wikipedia.org'; s 'v1.full' '.full'; s 'v2.full' '.full'; s 'v3.full' '.full'; s 'v4.full' '.full'; s 'v5.full' '.full'; s 'v6.full' '.full'; s 'v7.full' '.full'; s 'v8.full' '.full'; s 'v9.full' '.full'; s '.full-text' '.full'; s '.full.full' '.full'; s '.full-text' '.full'; s '.full-text.full' '.full'; s '.full.full.full' '.full'; s '.full.full' '.full'; s '.gov/labs/pmc/articles/P' '.gov/pmc/articles/P';  s 'rjlipton.wpcomstaging.com' 'rjlipton.wordpress.com'; s 'www.super-memory.com' 'super-memory.com'; s 'https://www.bldgblog.com' 'https://bldgblog.com'; s 'https://www.clinicaltrials.gov' 'https://clinicaltrials.gov'; s 'https://nitter.net/' 'https://twitter.com/'; s 'https://arxiv.org/abs//' 'https://arxiv.org/abs/';

          ## link cruft rewrites:
          s '&hl=en' ''; s '?hl=en&' '?'; s '?hl=en' ''; s '?usp=sharing' ''; s '?via%3Dihub' ''; s '.html?pagewanted=all' '.html'; s '&feature=youtu.be' ''; s ':443/' '/'; s ':80/' '/'; s '?s=r' ''; s '?s=61' ''; s '?sd=pf' ''; s '?ref=The+Browser-newsletter' ''; s '?ref=thebrowser.com' ''; s '?ignored=irrelevant' ''; s '](/docs/' '](/doc/'; s 'href="/docs/' 'href="/doc/'; s '.pdf#pdf' '.pdf'; s '#fromrss' ''; s '&amp;hl=en' ''; s '?rss=1' ''; s '/doc/statistics/decision-theory' '/doc/statistics/decision';

          ## name/entity consistency:
          s 'EMBASE' 'Embase'; s 'Medline' 'MEDLINE'; s 'PsychINFO' 'PsycINFO'; s 'MSCOCO' 'MS COCO'; s 'Yann Le Cun' 'Yann LeCun'; s ' VQVAE' ' VQ-VAE'; s 'CIFAR 10' 'CIFAR-10'; s 'Jorges Luis Borges' 'Jorge Luis Borges'; s 'Rene Girard' 'René Girard'; s 'Anno Hideaki' 'Hideaki Anno'; s ' GPT2' ' GPT-2'; s ' Clinicaltrials.gov' ' ClinicalTrials.gov'; s ' clinicaltrials.gov' ' ClinicalTrials.gov'; s 'Dario Amodai' 'Dario Amodei'; s 'single nucleotide polymorph' 'single-nucleotide polymorph'; s 'Single Nucleotide Polymorph' 'Single-Nucleotide Polymorph'; s 'single nucleotide variant' 'single-nucleotide variant'; s ' CIFAR10' 'CIFAR-10'; s 'TyDi QA' 'TyDiQA'; s 'Türkiye' 'Turkey'; s ' Poincare' ' Poincaré'; s 'Francois de La Rochefoucauld' 'François de La Rochefoucauld'; s 'Moliere' 'Molière'; s 'behavioural genetic' 'behavioral genetic'; s ' gwern.net' ' Gwern.net'; s 'chain of thought' 'chain-of-thought'; s 'Chain Of Thought' 'Chain-Of-Thought'; s 'Chain of Thought' 'Chain-of-Thought'; s 'Chain of thought' 'Chain-of-thought'; s 'MS Marco' 'MS MARCO'; s 'MS-MARCO' 'MS MARCO'; s 'NLSY-79' 'NLSY79'; s 'NLSY-97' 'NLSY97'; s 'state of the art' 'state-of-the-art'; s 'State of the Art' 'State-of-the-Art'; s 'Enwik8' 'enwik8'; s 'G. M. Fahy' 'Gregory M. Fahy'; s 'Greg M. Fahy' 'Gregory M. Fahy'; s 'Gary Kasparov' 'Garry Kasparov'; s 'Fel D1' 'Fel D 1'; s 'Fel d1' 'Fel d 1'; s 'CIFAR10' 'CIFAR-10'; s 'ImageNet1k' 'ImageNet-1k'; s 'ImageNet21k' 'ImageNet-21k'; s ' LeGuin' ' Le Guin';

          ## abbreviation consistency:
          s '(ie,' '(ie.'; s '(ie ' '(ie. '; s 'i.e.,' 'ie.'; s 'ie., ' 'ie. '; s '(i.e.' '(ie.'; s '(eg, ' '(eg. '; s ' eg ' ' eg. '; s '(eg ' '(eg. '; s '[eg ' '[eg. '; s 'e.g. ' 'eg. '; s ' e.g. ' ' eg. '; s 'e.g.,' 'eg.'; s 'eg.,' 'eg.'; s 'E.g.,' 'Eg.'; s '(cf ' '(cf. '; s ' cf ' ' cf. '; s ' Feb ' ' February '; s ' Aug ' ' August '; s ', Jr.' ' Junior'; s ' Jr.' ' Junior'; s ', Junior' ' Junior';
          s '<sup>Th</sup>' '<sup>th</sup>'; s '<sup>St</sup>' '<sup>st</sup>'; s '<sup>Nd</sup>' '<sup>nd</sup>'; s '<sup>Rd</sup>' '<sup>rd</sup>';
          s ',”' '”,'; s ",’" "’,";

          ## spelling errors:
          s 'border colly' 'border collie'; s 'genomewide' 'genome-wide'; s 'regularise' 'regularize'; s ' residualis' ' residualiz'; s 'endelian randomisation' 'endelian randomization'; s 'mendelian randomization' 'Mendelian Randomization'; s 'Mendelian randomization' 'Mendelian Randomization'; s 'canalization' 'canalisation'; s 'Statistical significance' 'Statistical-significance'; s 'Statistical Significance' 'Statistical-Significance'; s 'statistical significance' 'statistical-significance'; s ' longstanding' ' long-standing'; s 'utilise' 'utilize'; s 'facebookok' 'facebook'; s 'Tartarian' 'Tatarian'; s 'tartarian' 'tatarian'; s ' an One' ' a One'; s ' an one' ' a one'; s '<p>he ' '<p>He ';

          ## citation consistency:
          s ']^[' '] ^['; s 'et. al.' 'et al'; s 'et al. (' 'et al ('; s ' et al. 1'  ' et al 1'; s ' et al. 2'  ' et al 2'; s ' et al., ' ' et al '; s 'et al., ' 'et al '; sed -i -e 's/\([A-Z][a-z]\+\) et al (\([1-2][0-9][0-9][0-9][a-z]\?\))/\1 et al \2/g' metadata/*.yaml  `find . -name "*.page" -or -name "*.yaml"`; sed -i -e 's/\([A-Z][a-z]\+\) and \([A-Z][a-z]\+\) (\([1-2][0-9][0-9][0-9][a-z]\?\))/\1 \& \2 \3/g'  `find . -name "*.page" -or -name "*.yaml"`;

          ## anchor errors:
          s '#allen#allen' '#allen'; s '#deepmind#deepmind' '#deepmind'; s '&org=deepmind&org=deepmind' '&org=deepmind'; s '#nvidia#nvidia' '#nvidia'; s '#openai#openai' '#openai'; s '#google#google' '#google'; s '#uber#uber' '#uber';

          ## HTML/Markdown formatting:
          s '<p> ' '<p>'; s ' _n_s' ' <em>n</em>s'; s ' (n = ' ' (<em>n</em> = '; s ' (N = ' ' (<em>n</em> = '; s ' de novo ' ' <em>de novo</em> '; s ' De Novo ' ' <em>De Novo</em> '; s 'backlinks-not' 'backlink-not'; s ',</a>' '</a>,'; s ':</a>' '</a>:'; s ';</a>' '</a>;'; s ' <<a href' ' <a href'; s '_X_s' '<em>X</em>s'; s ' _r_s' ' <em>r</em>s'; s '# External links' '# External Links'; s '# See also' '# See Also'; s '"abstract-collapse abstract"' '"abstract abstract-collapse"'; s "‐" "-"; s 'class="link-auto"' ''; s '𝑂(' '𝒪('; s '</strong> and <strong>' '</strong> & <strong>'; s '<Sub>' '<sub>'; s '<Sup>' '<sup>'; s 'augmentation,</a>' 'augmentation</a>,'; s 'Bitcoin,</a>' 'Bitcoin</a>,'; s 'class="invertible"' 'class="invert"'; s '”&gt;' '">'; s '<br/>' '<br />'; s '<br>' '<br />'; s ' id="cb1"' ''; s ' id="cb2"' ''; s ' id="cb3"' ''; s ' id="cb4"' ''; s '.svg-530px.jpg' '.svg'; s ' (”' ' (“'; s '<A Href' '<a href'; s '</a>’s' '’s</a>'; s '-530px.jpg' ''; s '-768px.png' ''; s '-768px.jpg' ''; s '—-' '—'; s 'collapse-summary' 'abstract-collapse'; s 'href="ttp' 'href="http'; s '\xmlpi{\\}' ''; s '°C' '℃'; s '° C' '℃'; s '°F' '℉'; s '° F' '℉'; s '℉ahrenheit' '℉'; s '℃elsius' '℃'; s ' ℃' '℃'; s ' ℉' '℉'; s 'marginnnote' 'marginnote'; s ' <br /></li>' '</li>'; s ' <br /> </li>' '</li>'; s '<psna ' '<span '; s '……' '…'; s '</strong>::' '</strong>:'; s '](//' '[(/';
          ## TODO: duplicate HTML classes from Pandoc reported as issue #8705 & fixed; fix should be in >pandoc 3.1.1 (2023-03-05), so can remove these two rewrites once I upgrade past that:
          s 'class="odd odd' 'class="odd'; s 'class="even even' 'class="even';
          s '  ' ' '; s '​ ' ' ';
        ) &> /dev/null &
    sed -i -e 's/ data-link-?[Tt]ags="[a-z0-9 \/-]\+">/>/' ./metadata/*.yaml;
    fi

    bold "Compiling…"
    cd ./static/build
    compile () { ghc -O2 -Wall -rtsopts -threaded --make "$@"; }
    compile hakyll.hs
    compile generateLinkBibliography.hs
    compile generateDirectory.hs
    compile preprocess-markdown.hs &
    compile guessTag.hs &
    ## NOTE: generateSimilarLinks.hs & link-suggester.hs are done at midnight by a cron job because
    ## they are too slow to run during a regular site build & don't need to be super-up-to-date
    ## anyway
    cd ../../
    # cleanup:
    rm --recursive --force -- ./_cache/ ./_site/

  if [ "$SLOW" ]; then
    bold "Checking embeddings database…"
    ghci -i/home/gwern/wiki/static/build/ ./static/build/GenerateSimilar.hs  -e 'e <- readEmbeddings' &>/dev/null

    # duplicates a later check but if we have a fatal link error, we'd rather find out now rather than 30 minutes later while generating annotations:
    λ(){ grep -F -e 'href=""' -e 'href="!W"></a>' -e "href='!W'></a>" -- ./metadata/*.yaml || true; }
    wrap λ "Malformed empty link in annotations?"

    # another early fatal check: if there is a Markdown file 'foo.page' and also a subdirectory 'foo/' in the same directory, then this will result in, later, a fatal error when one tries to compile 'foo.page' → 'foo' (the HTML file) but 'foo' (the directory) already exists.
    # Check if any files collide with directories of the same name (without the .page extension).
    # Usage: find_colliding_files [path]
    function find_colliding_files() { # GPT-3 written:
      set -euo pipefail
      path="${1:-.}"
      find "$path" -depth -type f -name "*.page" -exec sh -c '
        for file do
          path="$(dirname "$file")/$(basename "$file" ".page")"
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
                      | sort | grep -F --invert-match -e 'doc/www' -e 'doc/rotten.com' -e 'doc/genetics/selection/www.mountimprobable.com' \
                                        -e 'doc/biology/2000-iapac-norvir' -e 'doc/gwern.net-gitstats' -e 'doc/reinforcement-learning/armstrong-controlproblem' \
                                        -e 'doc/statistics/order/beanmachine-multistage' -e 'doc/personal/2011-gwern-yourmorals.org/' \
                                        -e 'confidential/' -e 'private/' -e 'secret/' -e 'newest/')"

    # wait for generateLinkBibliography to finish to ensure the annotation link-bibs are all created:
    bold "Updating link bibliographies…"
    ./static/build/generateLinkBibliography +RTS -N"$N" -RTS

    # we want to generate all directories first before running Hakyll in case a new tag was created
    bold "Building directory indexes…"
    ./static/build/generateDirectory +RTS -N"$N" -RTS $DIRECTORY_TAGS
  fi

    bold "Check & update VCS…"
    (ping -q -c 5 google.com &> /dev/null && cd ./static/ && git status; git pull; git push --verbose &) || true

    # Cleanup pre:
    rm --recursive --force ./static/build/*.o ./static/build/*.hi ./static/build/generateDirectory ./static/build/generateLinkBibliography ./static/build/generateBacklinks || true

    cd ~/wiki/ # go to site root
    bold "Building site…"
    time ./static/build/hakyll build +RTS -N"$N" -RTS || (red "Hakyll errored out!"; exit 1)

    if [ "$SLOW" ]; then
        bold "Updating X-of-the-day…"
        ghci -i/home/gwern/wiki/static/build/ ./static/build/XOfTheDay.hs \
             -e 'do {md <- LinkMetadata.readLinkMetadata; aotd md; qotd; sotd; }' | \
            grep -F --invert-match -e ' secs,' -e 'it :: [T.Text]' -e '[]';
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
    ## possible alternative implementation in hakyll: https://www.rohanjain.in/hakyll-sitemap/
    (echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?> <urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
     ## very static files which rarely change: PDFs, images, site infrastructure:
     find -L _site/doc/ _site/ _site/static/ -not -name "*.page" -type f | grep -F --invert-match -e 'doc/www/' -e 'metadata/' -e '.git' -e '404' -e '/static/template/default.html' -e 'lorem' | grep -E --invert-match -e '/doc/.*/index' -e 'static/.*\..*\.html$' -e 'doc/.*\..*\.html$' | \
         sort | xargs urlencode -m | sed -e 's/%20/\n/g' | \
         sed -e 's/_site\/\(.*\)/\<url\>\<loc\>https:\/\/gwern\.net\/\1<\/loc><changefreq>never<\/changefreq><\/url>/'
     ## Everything else changes once in a while:
     find -L _site/ -not -name "*.page" -type f | grep -F --invert-match -e 'static/' -e 'doc/' -e 'fulltext' -e 'lorem' -e 'metadata/' -e '.page.html' -e 'private/' | \
         grep -E --invert-match -e '/.*/index' -e '.page$' | \
         sort | xargs urlencode -m | sed -e 's/%20/\n/g' | \
         sed -e 's/_site\/\(.*\)/\<url\>\<loc\>https:\/\/gwern\.net\/\1<\/loc><changefreq>monthly<\/changefreq><\/url>/'
     echo "</urlset>") >> ./_site/sitemap.xml

    ## generate a syntax-highlighted HTML fragment (not whole standalone page) version of source code files for popup usage:
    ### We skip .json/.jsonl/.csv because they are too large & Pandoc will choke; and we truncate at 1000 lines because such
    ### long source files are not readable as popups and their complexity makes browsers choke while rendering them.
    ### (We include plain text files in this in order to get truncated versions of them.)
    bold "Generating syntax-highlighted versions of source code files…"
    syntaxHighlight () {
        #### NOTE: for each new extension, add a `find` name, and an entry in `extracts-content.js`
        declare -A extensionToLanguage=( ["R"]="R" ["c"]="C" ["py"]="Python" ["css"]="CSS" ["hs"]="Haskell" ["js"]="Javascript" ["patch"]="Diff" ["diff"]="Diff" ["sh"]="Bash" ["bash"]="Bash" ["html"]="HTML" ["conf"]="Bash" ["php"]="PHP" ["opml"]="Xml" ["xml"]="Xml" ["page"]="Markdown"
                                         # NOTE: we do 'text' to get a 'syntax-highlighted' version which has wrapped columns etc.
                                         ["txt"]="default" ["yaml"]="YAML" ["jsonl"]="JSON" ["json"]="JSON" ["csv"]="CSV" )
        LENGTH="2000"
        for FILE in "$@"; do
            FILEORIGINAL=$(echo "$FILE" | sed -e 's/_site//')
            FILENAME=$(basename -- "$FILE")
            EXTENSION="${FILENAME##*.}"
            LANGUAGE=${extensionToLanguage[$EXTENSION]}
            FILELENGTH=$(cat "$FILE" | wc --lines)
            (echo -e "~~~~~~~~~~~~~~~~~~~~~{.$LANGUAGE}"; # NOTE: excessively long tilde-line is necessary to override/escape any tilde-blocks inside Markdown files: <https://pandoc.org/MANUAL.html#fenced-code-blocks>
            if [ $EXTENSION == "page" ]; then # the very long lines look bad in narrow popups, so we fold:
                cat "$FILE" | fold --spaces --width=70 | sed -e 's/~~~/∼∼∼/g' | head "-$LENGTH";
            else
                cat "$FILE" | head "-$LENGTH";
            fi
             echo -e "\n~~~~~~~~~~~~~~~~~~~~~"
             if (( $FILELENGTH >= "$LENGTH" )); then echo -e "\n\n…[File truncated due to length; see <a class=\"link-page\" href=\"$FILEORIGINAL\">original file</a>]…"; fi;
            ) | iconv -t utf8 -c | pandoc --from=markdown+smart --write=html5 --standalone \
                       --template=./static/template/pandoc/sourcecode.html5 \
                       --css=/static/css/colors.css --css=/static/css/initial.css --css=/static/css/default.css \
                       --metadata title="$(echo $FILE | sed -e 's/_site\///g')"  | \
                ## delete annoying self-link links: Pandoc/skylighting doesn't make this configurable
                sed -e 's/<span id="cb[0-9]\+-[0-9]\+"><a href="#cb[0-9]\+-[0-9]\+" aria-hidden="true" tabindex="-1"><\/a>//' -e 's/id="mathjax-styles" type="text\/css"/id="mathjax-styles"/' >> $FILE.html || red "Pandoc syntax-highlighting failed on: $FILE $FILEORIGINAL $FILENAME $EXTENSION $LANGUAGE $FILELENGTH"
        done
    }
    export -f syntaxHighlight
    set +e
    find _site/static/ -type f,l -name "*.html" | sort | parallel --jobs "$N" syntaxHighlight # NOTE: run .html first to avoid duplicate files like 'foo.js.html.html'
    find _site/ -type f,l -name "*.R" -or -name "*.c" -or -name "*.css" -or -name "*.hs" -or -name "*.js" -or -name "*.patch" -or -name "*.diff" -or -name "*.py" -or -name "*.sh" -or -name "*.bash" -or -name "*.php" -or -name "*.conf" -or -name "*.opml" -or -name "*.page" -or -name "*.txt" -or -name "*.json" -or -name "*.jsonl" -or -name "*.yaml" -or -name "*.xml" -or -name "*.csv"  | \
        sort |  grep -F --invert-match \
                 `# Pandoc fails on embedded Unicode/regexps in JQuery` \
                 -e 'mountimprobable.com/assets/app.js' -e 'jquery.min.js' -e 'index.page' \
                 -e 'metadata/backlinks.hs' -e 'metadata/embeddings.bin' -e 'metadata/archive.hs' -e 'doc/www/' -e 'sitemap.xml' | parallel  --jobs "$N" syntaxHighlight
    set -e

    bold "Stripping compile-time-only classes unnecessary at runtime…"
    cleanClasses () {
        sed -i -e 's/class=\"\(.*\)archive-not \?/class="\1/g' \
               -e 's/class=\"\(.*\)id-not \?/class="\1/g' \
               `# TODO: revert 9f246da03503b3c20d3c38eecd235b5aa7caa0b3 and remove .backlink-not clutter once all link-ID problems are resolved` \
               -e 's/class=\"\(.*\)link-annotated-not \?/class="\1/g' \
               -e 's/class=\"\(.*\)link-auto \?/class="\1/g' \
               -e 's/class=\"\(.*\)link-live-not \?/class="\1/g' \
    "$@"; }; export -f cleanClasses
    find ./ -path ./_site -prune -type f -o -name "*.page" | grep -F --invert-match -e '#' | sort | sed -e 's/\.page$//' -e 's/\.\/\(.*\)/_site\/\1/' | parallel --max-args=500 cleanClasses || true
    # TODO: rewriting in place doesn't work because of the symbolic links. need to copy ./metadata/ instead of symlinking?
    find ./_site/metadata/ -type f -name "*.html" | sort | parallel --max-args=500 cleanClasses || true

    ## Pandoc/Skylighting by default adds empty self-links to line-numbered code blocks to make them clickable (as opposed to just setting a span ID, which it also does). These links *would* be hidden except that self links get marked up with up/down arrows, so arrows decorate the codeblocks. We have no use for them and Pandoc/skylighting has no option or way to disable them, so we strip them.
    bold "Stripping self-links from syntax-highlighted HTML…"
    cleanCodeblockSelflinks () {
        if [[ $(grep -F -e 'class="sourceCode' "$@") ]]; then
            sed -i -e 's/<a href="\#cb[0-9]\+-[0-9]\+" aria-hidden="true" tabindex="-1"><\/a>//g' -e 's/<a href="\#cb[0-9]\+-[0-9]\+" aria-hidden="true" tabindex="-1" \/>//g' -- "$@";
        fi
    }
    export -f cleanCodeblockSelflinks
    find ./ -path ./_site -prune -type f -o -name "*.page" | grep -F --invert-match -e '#' | sort | sed -e 's/\.page$//' -e 's/\.\/\(.*\)/_site\/\1/' | parallel --max-args=500 cleanCodeblockSelflinks || true

    bold "Reformatting HTML sources to look nicer using HTML Tidy…"
    # WARNING: HTML Tidy breaks the static-compiled MathJax. One of Tidy's passes breaks the mjpage-generated CSS (messes with 'center', among other things). So we do Tidy *before* the MathJax.
    # WARNING: HTML Tidy by default will wrap & add newlines for cleaner HTML in ways which don't show up in rendered HTML - *except* for when something is an 'inline-block', then the added newlines *will* show up, as excess spaces. <https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Whitespace#spaces_in_between_inline_and_inline-block_elements> <https://patrickbrosset.medium.com/when-does-white-space-matter-in-html-b90e8a7cdd33> And we use inline-blocks for the #page-metadata block, so naive HTML Tidy use will lead to the links in it having a clear visible prefixed space. We disable wrapping entirely by setting `-wrap 0` to avoid that.
    tidyUpFragment () { tidy -indent -wrap 0 --clean yes --merge-divs no --break-before-br yes --logical-emphasis yes -quiet --show-warnings no --show-body-only yes -modify "$@" || true; }
    ## tidy wants to dump whole well-formed HTML pages, not fragments to transclude, so switch.
    tidyUpWhole () {    tidy -indent -wrap 0 --clean yes --merge-divs no --break-before-br yes --logical-emphasis yes -quiet --show-warnings no --show-body-only no  -modify "$@" || true; }
    export -f tidyUpFragment tidyUpWhole
    find ./_site/metadata/annotation/ -type f -name "*.html" |  parallel --max-args=250 tidyUpFragment
    find ./ -path ./_site -prune -type f -o -name "*.page" | sed -e 's/\.page$//' -e 's/\.\/\(.*\)/_site\/\1/' | grep -F --invert-match -e '#' -e 'Death-Note-script' | parallel --max-args=250 tidyUpWhole

    ## use https://github.com/pkra/mathjax-node-page/ to statically compile the MathJax rendering of the MathML to display math instantly on page load
    ## background: https://joashc.github.io/posts/2015-09-14-prerender-mathjax.html installation: `npm install --prefix ~/src/ mathjax-node-page`
    bold "Compiling LaTeX JS+HTML into static CSS+HTML…"
    staticCompileMathJax () {
        if [[ $(grep -F -e '<span class="math inline"' -e '<span class="math display"' "$@") ]]; then
            TARGET=$(mktemp /tmp/XXXXXXX.html)
            cat "$@" | ~/src/node_modules/mathjax-node-page/bin/mjpage --output CommonHTML --fontURL '/static/font/mathjax' | \
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
    (find ./ -path ./_site -prune -type f -o -name "*.page" | grep -F --invert-match -e '#' | sed -e 's/\.page$//' -e 's/\.\/\(.*\)/_site\/\1/';
     find _site/metadata/annotation/ -name '*.html') | shuf | \
        parallel --jobs "$N" --max-args=1 staticCompileMathJax

    # 1. turn "As per Foo et al 2020, we can see." → "<p>As per Foo et al 2020, we can see.</p>" (&nbsp;); likewise for 'Foo 2020' or 'Foo & Bar 2020'
    # 2. add non-breaking character to punctuation after links to avoid issues with links like '[Foo](/bar);' where ';' gets broken onto the next line (this doesn't happen in regular text, but only after links, so I guess browsers have that builtin but only for regular text handling?), (U+2060 WORD JOINER (HTML &#8288; · &NoBreak; · WJ))
    # 3. add hair space ( U+200A   HAIR SPACE (HTML &#8202; · &hairsp;)) in slash-separated links or quotes, to avoid overlap of '/' with curly-quote
                               # -e 's/\([a-zA-Z‘’-]\)[   ]et[   ]al[   ]\([1-2][0-9][0-9a-z]\+\)/\1 <span class="etal"><span class="etalMarker">et al<\/span> <span class="etalYear">\2<\/span><\/span>/g' \
                           # -e 's/\([A-Z][a-zA-Z]\+\)[   ]\&[   ]\([A-Z][a-zA-Z]\+\)[   ]\([1-2][0-9][0-9a-z]\+\)/\1 \& \2 <span class="etalYear">\3<\/span>/g' \
    bold "Adding non-breaking spaces…"
    nonbreakSpace () { sed -i -e 's/\([a-zA-Z]\) et al \([1-2]\)/\1 et al \2/g' \
                              -e 's/\([A-Z][a-zA-Z]\+\) \([1-2]\)/\1 \2/g' \
                              `# "Foo & Quux 2020" Markdown → "Foo &amp; Quux 2020" HTML` \
                              -e 's/\([A-Z][a-zA-Z]\+\)[  ]\&amp\;[  ]\([A-Z][a-zA-Z]\+\)[  ]\([1-2][1-2][1-2][1-2]\)/\1 \&amp\;_\2 \3/g' \
                              `# "Foo & Quux 2020" Markdown → "Foo &amp; Quux&emsp14;2020" HTML` \
                              -e 's/\([A-Z][a-zA-Z]\+\) \&amp\; \([A-Z][a-zA-Z]\+\)\&emsp14\;\([1-2][1-2][1-2][1-2]\)/\1 \&amp\;_\2\&emsp14\;\3/g' \
                              -e 's/<\/a>;/<\/a>\⁠;/g' -e 's/<\/a>,/<\/a>\⁠,/g' -e 's/<\/a>\./<\/a>\⁠./g' -e 's/<\/a>\//<\/a>\⁠\/ /g' \
                              -e 's/\/<wbr><a /\/ <a /g' -e 's/\/<wbr>"/\/ "/g' \
                              -e 's/\([a-z]\)…\([0-9]\)/\1⁠…⁠\2/g' -e 's/\([a-z]\)…<sub>\([0-9]\)/\1⁠…⁠<sub>\2/g' -e 's/\([a-z]\)<sub>…\([0-9]\)/\1⁠<sub>…⁠\2/g' -e 's/\([a-z]\)<sub>…<\/sub>\([0-9]\)/\1⁠<sub>…⁠<\/sub>\2/g' \
                              -e 's/\([a-z]\)⋯\([0-9]\)/\1⁠⋯⁠\2/g' -e 's/\([a-z]\)⋯<sub>\([0-9]\)/\1⁠⋯⁠<sub>\2/g' \
                              -e 's/\([a-z]\)⋱<sub>\([0-9]\)/\1⁠⋱⁠<sub>\2/g' -e 's/\([a-z]\)<sub>⋱\([0-9]\)/\1<sub>⁠⋱⁠\2/g' \
                              -e 's/ \+/ /g' -e 's/​​\+/​/g' -e 's/​ ​​ ​\+/​ /g' -e 's/​ ​\+/ /g' -e 's/​ ​ ​ \+/ /g' -e 's/​ ​ ​ \+/ /g' -e 's/  / /g' -e 's/​ ​​ \+​/ /g' \
                              `# add HAIR SPACE to parenthetical links to avoid biting of the open-parenthesis (eg '( <a href="https://tvtropes.org...">TvTropes</a>)'); note that formatting can be *outside* the <a> as well as *inside*: ` \
                              -e 's/ (<a / ( <a /g' -e 's/ (<strong><a / ( <strong><a /g' -e 's/ (<em><a / ( <em><a /g' -e 's/ (<span class="smallcaps"><a / ( <span class="smallcaps"><a /g' \
                              `# and similarly, '[foo](http)/[bar](http)' bites the '/':` \
                              -e 's/<\/a>\/<a /<\/a> \/ <a /g' \
                              -e 's/““/“ “/g' -e 's/””/” ”/g' \
                              `# Big O notation: '𝒪(n)' in some browsers like my Chromium will touch the O/parenthesis (particularly noticeable in /Problem-14's abstract), so add a THIN SPACE (HAIR SPACE is not enough for the highly-tilted italic):` \
                              -e 's/𝒪(/𝒪 (/g' \
                            "$@"; }; export -f nonbreakSpace;
    find ./ -path ./_site -prune -type f -o -name "*.page" | grep -F --invert-match -e '#' | sort | sed -e 's/\.page$//' -e 's/\.\/\(.*\)/_site\/\1/' | parallel --max-args=500 nonbreakSpace || true
    find ./_site/metadata/annotation/ -type f -name "*.html" | sort | parallel --max-args=500 nonbreakSpace || true

    bold "Adding #footnotes section ID…" # Pandoc bug; see <https://github.com/jgm/pandoc/issues/8043>; fixed in <https://github.com/jgm/pandoc/commit/50c9848c34d220a2c834750c3d28f7c94e8b94a0>, presumably will be fixed in Pandoc >2.18
    footnotesIDAdd () { sed -i -e 's/<section class="footnotes footnotes-end-of-document" role="doc-endnotes">/<section class="footnotes" role="doc-endnotes" id="footnotes">/' "$@"; }; export -f footnotesIDAdd
    find ./ -path ./_site -prune -type f -o -name "*.page" | grep -F --invert-match -e '#' | sort | sed -e 's/\.page$//' -e 's/\.\/\(.*\)/_site\/\1/' | parallel --max-args=500 footnotesIDAdd || true

  if [ "$SLOW" ]; then
    # Testing compilation results:
    set +e

    bold "Checking metadata…"
    ghci -istatic/build/ ./static/build/LinkMetadata.hs -e 'readLinkMetadataAndCheck' 1> /dev/null

    # essays only:
    ## eg. './2012-election.page \n...\n ./doc/cs/cryptography/1955-nash.page \n...\n ./newsletter/2022/09.page \n...\n ./review/mcnamara.page \n...\n ./wikipedia-and-knol.page \n...\n ./zeo/zma.page'
    PAGES="$(find . -type f -name "*.page" | grep -F --invert-match -e '_site/' -e 'index' | sort -u)"
    # essays+tags+annotations+similars+backlinks:
    # eg. "_site/2012-election _site/2014-spirulina _site/3-grenades ... _site/doc/ai/text-style-transfer/index ... _site/doc/anime/2010-sarrazin ... _site/fiction/erl-king ... _site/lorem-admonition ... _site/newsletter/2013/12 ... _site/note/attention ... _site/review/umineko ... _site/zeo/zma"
    PAGES_ALL="$(find ./ -type f -name "*.page" | grep -F --invert-match '_site' | sort | sed -e 's/\.page$//' -e 's/\.\/\(.*\)/_site\/\1/') $(find _site/metadata/annotation/ -type f -name '*.html' | sort)"
    λ(){
         echo "$PAGES_ALL" | xargs grep -F -l --color=always -e '<span class="math inline">' -e '<span class="math display">' -e '<span class="mjpage">' | \
                                     grep -F --invert-match -e '/1955-nash' -e '/backstop' -e '/death-note-anonymity' -e '/difference' \
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

    λ(){ VISIBLE_N=$(cat ./_site/sitemap.xml | wc --lines); [ "$VISIBLE_N" -le 15000 ] && echo "$VISIBLE_N" && exit 1; }
    wrap λ "Sanity-check number-of-public-site-files in sitemap.xml failed"

    λ(){ COMPILED_N="$(find -L ./_site/ -type f | wc --lines)"
         [ "$COMPILED_N" -le 86000 ] && echo "File count: $COMPILED_N" && exit 1;
         COMPILED_BYTES="$(du --summarize --total --dereference --bytes ./_site/ | tail --lines=1 | cut --field=1)"
         [ "$COMPILED_BYTES" -le 77000000000 ] && echo "Total filesize: $COMPILED_BYTES" && exit 1; }
    wrap λ "Sanity-check: number of files & file-size too small?"

    λ(){ SUGGESTIONS_N=$(cat ./metadata/linkSuggestions.el | wc --lines); [ "$SUGGESTIONS_N" -le 10000 ] && echo "$SUGGESTIONS_N"; }
    wrap λ "Link-suggestion database broken?"
    λ(){ BACKLINKS_N=$(cat ./metadata/backlinks.hs | wc --lines);         [ "$BACKLINKS_N"   -le 73000 ] && echo "$BACKLINKS_N"; }
    wrap λ "Backlinks database broken?"

    λ(){ ANNOTATION_FILES_N=$(find ./metadata/annotation/ -maxdepth 1 -type f | wc --lines);
         [ "$ANNOTATION_FILES_N"   -le 12500 ] && echo "$ANNOTATION_FILES_N"; }
    wrap λ "Annotation files are missing?"
    λ(){ BACKLINKS_FILES_N=$(find ./metadata/annotation/backlink/ -type f | wc --lines);
         [ "$BACKLINKS_FILES_N"    -le 24500 ] && echo "$BACKLINKS_FILES_N"; }
    wrap λ "Backlinks files are missing?"
    λ(){ SIMILARLINKS_FILES_N=$(find ./metadata/annotation/similar/ -type f | wc --lines);
         [ "$SIMILARLINKS_FILES_N" -le 9540 ] && echo "$SIMILARLINKS_FILES_N"; }
    wrap λ "Similar-links files are missing?"

    ## NOTE: transclude.js supports some special 'range' syntax for transclusions, so a link like '/note/lion#history#'/'/note/lion##history'/'/note/lion##'/'/note/lion#history#foo' is in fact valid
    λ(){ grep -E -e '#[[:alnum:]-]+#' -e '[[:alnum:]-]+##[[:alnum:]-]+' metadata/*.yaml metadata/*.hs | grep -E --invert-match -e '#[[:alnum:]-]+#$'; }
    wrap λ "Broken double-hash anchors in links somewhere?"

    λ(){ grep -E -- '/[[:graph:]]\+[0-9]–[0-9]' ./metadata/*.yaml ./metadata/*.hs || true;
         grep -F -- '–' ./metadata/*.hs || true; }
    wrap λ "En-dashes in URLs?"

    λ(){ grep -F -e 'http' ./metadata/*.hs ./metadata/*.yaml | grep -F --invert-match -e 'https://en.wikipedia.org/wiki/' -e '10/arc-1-gestation/1' -e 'the-elves-leave-middle-earth-' -e '2011/05/from-the-bookcase-no-2' -e 'd-a-rovinskiis-collection-of-russian-lubki-18th' -e 'commons.wikimedia.org/wiki/File:Flag_of_the_NSDAP_' | grep -F -e "%E2%80%93" -e "%E2%80%94" -e "%E2%88%92"; }
    wrap λ "*Escaped* En/em/minus dashes in URLs?"

    λ(){ gf -e '\\' ./static/css/*.css; }
    wrap λ "Warning: stray backslashes in CSS‽ (Dangerous interaction with minification!)"

    λ(){ find ./ -type f -name "*.page" | grep -F --invert-match -e '_site' -e 'Modafinil' -e 'Blackmail' | sort | sed -e 's/\.page$//' -e 's/\.\/\(.*\)/_site\/\1/' | xargs --max-args=500 grep -F --with-filename --color=always -e '!Wikipedia' -e '!W'")" -e '!W \"' -e ']( http' -e ']( /' -e '](#fn' -e '!Margin:' -e '<span></span>' -e '<span />' -e '<span/>' -e 'http://gwern.net' -e 'http://www.gwern.net' -e 'https://www.gwern.net' -e 'https//www' -e 'http//www'  -e 'hhttp://' -e 'hhttps://' -e ' _n_s' -e '/journal/vaop/ncurrent/' -e '://bit.ly/' -e 'remote/check_cookie.html' -e 'https://www.biorxiv.org/node/' -e '/article/info:doi/10.1371/' -e 'https://PaperCode.cc' -e '?mod=' -e 'www.researchgate.net' | \
         grep -E -e 'https://web.archive.org/web/.*www\.gwern\.net.*' -e 'Blackmail';
       }
    wrap λ "Stray or bad URL links in Markdown-sourced HTML."

    ## Whitelist of HTML classes which are authorized for use. Any new classes should be added here.
    λ(){ find metadata/annotation/ -name "*.html" \
             | shuf | xargs --max-procs=0 --max-args=500 ./static/build/htmlClassesExtract.py | tr ' ' '\n' | sort -u | \
             grep -E --invert-match -e '^see-also-append$' -e '^archive-not$' -e '^archive-local$' -e '^author$' -e '^full-authors-list$' -e '^aux-links$' -e '^backlink-not$' \
                   -e '^backlinks$' -e '^backlinks-append$' -e 'aux-links-append' -e '^bash$' -e '^Bash$' -e '^book-review-author$' \
                   -e '^book-review-date$' -e '^book-review-rating$' -e '^book-review-title$' -e '^cite-author$' -e '^cite-author-plural$' \
                   -e '^cite-date$' -e '^date$' -e '^display$' -e '^email$' -e '^external-page-embed$' -e '^id-not$' -e '^include$' \
                   -e '^include-strict$' -e '^inflation-adjusted$' -e '^logotype-tex$' -e '^logotype-latex$' -e '^logotype-latex-a$' -e '^logotype-latex-e$' \
                   -e '^link-annotated$' -e '^link-live$' -e '^link-page$' -e '^link-page-not$' \
                   -e '^link-tag$' -e '^link-tags$' -e '^cite$' -e '^cite-joiner$' -e '^collapse$' -e '^columns$' -e '^directory-indexes-downwards$' \
                   -e '^directory-indexes-upwards$' -e '^epigraph$' -e '^even$' -e '^float-right$' -e '^float-left$' -e '^footnote-ref$' \
                   -e '^full-width$' -e '^haskell$' -e '^header$' -e '^horizontal-rule-nth-0$' -e '^horizontal-rule-nth-1$' \
                   -e '^horizontal-rule-nth-2$' -e '^icon-not$' -e '^inline$' -e '^invert$' -e '^invert-auto$' -e '^invert-not$' \
                   -e '^javascript$' -e '^link-annotated-not$' -e '^link-annotated-partial$'  \
                   -e '^link-live-not$' -e '^tex-logotype$' -e '^math$' -e '^odd$' -e '^page-thumbnail$' \
                   -e '^pascal$' -e '^python$' -e '^reader-mode-selector-inline$' -e '^smallcaps$' -e '^sourceCode$' -e '^subsup$' \
                   -e '^table-small$' -e '^TOC$' -e '^uri$' -e '^width-full$' -e '^at$' -e '^bu$' -e '^c1$' -e '^c2$' -e '^c3$' -e '^c4$' \
                   -e '^c5$' -e '^c6$' -e '^c7$' -e '^c8$' -e '^c9$' -e '^c10$' -e '^cf$' -e '^co$' -e '^dv$' -e '^fu$' -e '^kw$' -e '^op$' -e '^s1$' -e '^st$' -e '^reader-mode$' \
                   -e '^scrape-abstract-not$'  -e '^abstract$' -e '^abstract-collapse$' -e '^admonition$' -e '^admonition-title$' \
                   -e '^book-review-meta$' -e '^book-review-review$' -e '^tip$' -e '^xml$' -e '^warning$' -e '^al$' -e '^an$' -e '^bn$' \
                   -e '^cn$' -e '^cv$' -e '^do$' -e '^dt$' -e '^er$' -e '^error$' -e '^ex$' -e '^fl$' -e '^im$' -e '^in$' -e '^ot$' -e '^pp$' \
                   -e '^re$' -e '^sc$' -e '^ss$' -e '^va$' -e '^citation$' -e '^directory-indexes$' -e '^directory-indexes-sideways$' \
                   -e '^display-pop-not$' -e '^footnote-back$' -e '^footnotes$' -e '^image-focus-not$' -e '^include-annotation$' -e '^include-even-when-collapsed$' \
                   -e '^include-spinner-not$' -e '^include-replace-container$' -e '^include-replace-container-not$' -e '^include-unwrap$' \
                   -e '^marginnote$' -e '^markdownBody$' -e '^mjpage$' -e '^mjpage__block$' -e '^mjx-base$' -e '^mjx-box$' -e '^MJXc-display$' \
                   -e '^mjx-cell$' -e '^mjx-char$' -e '^mjx-charbox$' -e '^mjx-chtml$' -e '^MJXc-space1$' -e '^MJXc-space2$' -e '^MJXc-space3$' \
                   -e '^MJXc-stacked$' -e '^MJXc-TeX-ams-R$' -e '^MJXc-TeX-cal-R$' -e '^MJXc-TeX-main-R$' -e '^MJXc-TeX-math-I$' \
                   -e '^MJXc-TeX-size1-R$' -e '^MJXc-TeX-size2-R$' -e '^MJXc-TeX-size3-R$' -e '^MJXc-TeX-size4-R$' -e '^mjx-delim-h$' \
                   -e '^mjx-delim-v$' -e '^mjx-denominator$' -e '^mjx-itable$' -e '^mjx-line$' -e '^mjx-math$' -e '^mjx-mfrac$' -e '^mjx-mi$' \
                   -e '^mjx-mn$' -e '^mjx-mo$' -e '^mjx-mrow$' -e '^mjx-mspace$' -e '^mjx-msqrt$' -e '^mjx-mstyle$' -e '^mjx-msubsup$' \
                   -e '^mjx-msup$' -e '^mjx-mtext$' -e '^mjx-munderover$' -e '^mjx-numerator$' -e '^mjx-op$' -e '^mjx-over$' -e '^mjx-row$' \
                   -e '^mjx-stack$' -e '^mjx-sub$' -e '^mjx-sup$' -e '^mjx-surd$' -e '^mjx-texatom$' -e '^mjx-TeXmathchoice$' -e '^mjx-under$' \
                   -e '^mjx-vsize$' -e '^new$' -e '^outline-not$' -e '^warning$' -e '^markdown-body$' -e '^similars$' -e '^similars-append$' -e '^similar-links-search$' \
                   -e '^text-center$' -e '^abstract-tag-directory$' -e '^page-description-annotation$' -e '^link-bibliography$' \
                   -e '^link-bibliography-append$' -e '^expand-on-hover$' -e '^include-block-context$' -e '^tag-index-link-bibliography-block$' \
                   -e '^decorate-not$' -e '^include-omit-metadata$' -e '^quote-of-the-day$' -e '^interview$' -e '^reader-mode-note$'; }
    wrap λ "Mysterious HTML classes in compiled HTML?"

    λ(){ echo "$PAGES_ALL" | grep -F --invert-match 'Hafu' | xargs --max-args=500 grep -F --with-filename --invert-match -e ' tell what Asahina-san' -e 'contributor to the Global Fund to Fight AIDS' -e 'collective name of the project' -e 'model resides in the' -e '{.cite-' -e '<span class="op">?' -e '<td class="c' -e '<td style="text-align: left;">?' -e '>?</span>' -e '<pre class="sourceCode xml">' | \
             grep -F --color=always -e ")'s " -e "}'s " -e '">?' -e '</a>s';
         echo "$PAGES_ALL" | grep -F --invert-match 'Hafu' | xargs --max-args=500 grep -E --with-filename --color=always -e '<a .*href=".*">\?';
       }
    wrap λ "Punctuation like possessives should go *inside* the link (unless it is an apostrophe in which case it should go outside due to Pandoc bug #8381)."
    ## NOTE: 8381 <https://github.com/jgm/pandoc/issues/8381> is a WONTFIX by jgm, so no solution but to manually check for it. Fortunately, it is rare.

    λ(){ grep -E 'http.*http' metadata/archive.hs  | grep -F --invert-match -e 'web.archive.org' -e 'https-everywhere' -e 'check_cookie.html' -e 'translate.goog' -e 'archive.md' -e 'webarchive.loc.gov' -e 'https://http.cat/' -e '//)' -e 'https://esolangs.org/wiki////' -e 'https://ansiwave.net/blog/sqlite-over-http.html'; }
    wrap λ "Bad URL links in archive database (and perhaps site-wide)."

    λ(){ find ./ -type f -name "*.page" | grep -F --invert-match '_site' | sort | sed -e 's/\.page$//' -e 's/\.\/\(.*\)/_site\/\1/' | xargs --max-args=500 grep -F --with-filename --color=always -e '<div>' | grep -F --invert-match -e 'I got around this by adding in the Hakyll template an additional'; }
    wrap λ "Stray <div>?"

    λ(){ find ./ -type f -name "*.page" | grep -F --invert-match '_site' | sort | sed -e 's/\.page$//' -e 's/\.\/\(.*\)/_site\/\1/' | xargs --max-args=500 grep -F --with-filename --color=always -e 'invertible-not' -e 'invertible-auto' -e '.invertible' -e '.invertibleNot' -e '.invertible-Not' -e '{.Smallcaps}' -e '{.sallcaps}' -e '{.mallcaps}' -e '{.small}' -e '{.invertible-not}' -e 'no-image-focus' -e 'no-outline' -e 'idNot' -e 'backlinksNot' -e 'abstractNot' -e 'displayPopNot' -e 'small-table' -e '{.full-width' -e 'collapseSummary' -e 'collapse-summary' -e 'tex-logotype' -e ' abstract-not' -e 'localArchive' -e 'backlinks-not' -e '{.}' -e "bookReview-title" -e "bookReview-author" -e "bookReview-date" -e "bookReview-rating" -e 'class="epigraphs"' -e 'data-embedding-distance' -e 'data-embeddingdistance' -e 'data-link-tags' -e 'data-linktags' -e 'link-auto-first' -e 'link-auto-skipped' -e 'local-archive-link' -e 'include-replace}' -e 'include-replace ' -e 'drop-caps-de-kanzlei' -e '.backlink-not)' -e 'link-annotated link-annotated-partial' -e 'link-annotated-partial link-annotated' -e '{.margin-note}'; }
    wrap λ "Misspelled/outdated classes in Markdown/HTML."

    λ(){ ghci -istatic/build/ static/build/LinkMetadata.hs -e 'do {md <- readLinkMetadata; putStrLn $ unlines $ map (\(f,(_,auts,_,_,_,_)) -> f ++ " : " ++ auts) $ M.toList md; }' | grep -F -e 'Francesca Gino' -e 'Dan Ariely' -e 'Michael LaCour' -e 'David Rosenhan' -e 'Diederik Stapel' -e 'Didier Raoult' -e 'Brian Wansink' -e 'Marc Hauser' -e 'Robert Rosenthal' -e 'J. Hendrik Schön' -e 'Matthew Walker' -e 'Guéguen' -e 'Gueguen' | grep -F --invert-match -e '/doc/economics/experience-curve/2020-kc.pdf' -e '/doc/food/2002-wansink.pdf' -e 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2244801/'; }
    wrap λ "Dishonest or serial fabricators detected as authors? If a fraudulent publication should be annotated anyway, add a warning to the annotation & whitelist it."

     λ(){ find ./ -type f -name "*.page" | grep -F --invert-match '/variable' | grep -F --invert-match '_site' | sort | sed -e 's/\.page$//' -e 's/\.\/\(.*\)/_site\/\1/' | xargs --max-args=500 grep -F --with-filename --color=always -e '{#'; }
     wrap λ "Bad link ID overrides in Markdown."

    λ(){ find ./ -type f -name "*.page" | grep -F --invert-match '_site' | sort | sed -e 's/\.page$//' -e 's/\.\/\(.*\)/_site\/\1/'  | parallel --max-args=500 grep -E --with-filename --color=always -e 'pdf#page[0-9]' -e 'pdf#pg[0-9]' -e '\#[a-z]\+\#[a-z]\+'; }
    wrap λ "Incorrect PDF page links in Markdown."

    λ(){ find ./ -type f -name "*.page" -type f -exec grep -E -e 'cssExtension:' {} \; | \
       grep -F --invert-match -e 'cssExtension: drop-caps-cheshire' -e 'cssExtension: drop-caps-cheshire reader-mode' -e 'cssExtension: drop-caps-de-zs' -e 'cssExtension: drop-caps-goudy' -e 'cssExtension: drop-caps-goudy reader-mode' -e 'cssExtension: drop-caps-kanzlei' -e 'cssExtension: "drop-caps-kanzlei reader-mode"' -e 'cssExtension: drop-caps-yinit'; }
    wrap λ "Incorrect drop caps in Markdown."

    λ(){ find ./ -type f -name "*.page" | grep -F --invert-match '_site' | grep -F --invert-match -e 'lorem-code.page' -e 'ab-test.page' | sort | sed -e 's/\.page$//' -e 's/\.\/\(.*\)/_site\/\1/'  | parallel --max-args=500 "grep --color=always -F --with-filename -- '<span class=\"er\">'"; } # NOTE: filtered out lorem-code.page's deliberate CSS test-case use of it in the syntax-highlighting section
    wrap λ "Broken code in Markdown."

    λ(){ find ./ -type f -name "*.page" | parallel --max-args=500 "grep -F --with-filename -e '<span class=\"supsub\">' -e 'class=\"subsup\"><sup>' --"; }
    wrap λ "Incorrect use of 'supsub' name (should be 'subsup')."

    λ(){ find ./ -type f -name "*.page" | parallel --max-args=500 "grep -F --with-filename -e 'class=\"subsup\"><sup>'"; }
    wrap λ "Incorrect ordering of '<sup>' (the superscript '<sup>' must come second, or else risk Pandoc misinterpreting as footnote while translating HTML↔Markdown)."

    λ(){ ge -e '<div class="admonition .*\?">[^$]' -e 'class="admonition"' -e '"admonition warn"' -e '<div class="epigrah">' -e 'class="epigraph>' $PAGES; }
    wrap λ "Broken admonition paragraph or epigraph in Markdown."

    λ(){ ge -e '^   - '  -e '~~~[[:alnum:]]' $PAGES; }
    wrap λ "Markdown formatting problem: use of 3-space indented sub-list items instead of 4-space?"

    λ(){ ge -e ' a [aei]' $PAGES | grep -F --invert-match -e 'static/build/' -e '/gpt-3' -e '/gpt-2-preference-learning' -e 'sicp/'; }
    wrap λ "Grammar: 'a' → 'an'?"

     λ(){ ge -e '<div class="text-center">$' -e '[A-Za-z]\.\. ' -e '– ' -e  ' –' -- $PAGES; }
     wrap λ "Markdown: miscellaneous regexp errors."

    λ(){ find -L . -type f -size 0  -printf 'Empty file: %p %s\n' | grep -F --invert-match '.git/FETCH_HEAD' -e './.git/modules/static/logs/refs/remotes/'; }
    wrap λ "Empty files somewhere."

    λ(){ find ./_site/ -type f -not -name "*.*" -exec grep --quiet --binary-files=without-match . {} \; -print0 | parallel --null --max-args=500 "grep -F --color=always --with-filename -- '————–'"; }
    wrap λ "Broken tables in HTML."

    λ(){ find ./ -type f -name "*.page" | grep -F --invert-match '_site' | sort | sed -e 's/\.page$//' -e 's/\.\/\(.*\)/_site\/\1/'  | xargs --max-args=500 grep -F --with-filename --color=always -e '](/​image/​' -e '](/image/' -e '](/​images/​' -e '](/images/' -e '<p>[[' -e ' _</span><a ' -e ' _<a ' -e '{.marginnote}' -e '^[]' -e '‘’' -e '``'; }
    wrap λ "Miscellaneous fixed-string errors in compiled HTML."

    λ(){ find ./ -type f -name "*.page" | grep -F --invert-match '_site' | sort | sed -e 's/\.page$//' -e 's/\.\/\(.*\)/_site\/\1/'  | xargs --max-args=500 grep -E --with-filename --color=always -e ' __[A-Z][a-z]' -e 'href="/[a-z0-9-]#fn[0-9]\+"' -e 'href="#fn[0-9]\+"' -e '"></a>' | grep -F -v -e 'tabindex="-1"></a>'; }
    wrap λ "Miscellaneous regexp errors in compiled HTML."

    λ(){ ge -e '^"~/' -e '\$";$' -e '$" "doc' -e '\|' -e '\.\*\.\*' -e '\.\*";' -e '"";$' -e '.\*\$ doc' ./static/redirect/nginx*.conf | grep -F -e 'default "";'; }
    wrap λ "Warning: empty result or caret/tilde-less Nginx redirect rule (dangerous—matches anywhere in URL!)"

    λ(){ ghci -istatic/build/ ./static/build/LinkMetadata.hs -e 'warnParagraphizeYAML "metadata/full.yaml"'; }
    wrap λ "Annotations that need to be rewritten into paragraphs."

    λ(){ runghc -istatic/build/ ./static/build/link-prioritize.hs 20; }
    wrap λ "Links needing annotations by priority:"

    λ(){ ge -e '[a-zA-Z]- ' -e 'PsycInfo Database Record' -e 'https://www.gwern.net' -e '/home/gwern/' -e 'https://goo.gl' -- ./metadata/*.yaml | \
         grep -F --invert-match -e 'https://web.archive.org/web/'; }
    wrap λ "Check possible typo in YAML metadata database."

    λ(){ ge '  - .*[a-z]–[a-Z]' ./metadata/full.yaml ./metadata/half.yaml; }
    wrap λ "Look for en-dash abuse."

    λ(){ gf -e ' ?' ./metadata/full.yaml; }
    wrap λ "Problem with question-marks (perhaps the crossref/Emacs copy-paste problem?)."

    λ(){ grep -F --invert-match -e 'N,N-DMT' -e 'E,Z-nepetalactone' -e 'Z,E-nepetalactone' -e 'N,N-Dimethyltryptamine' -e 'N,N-dimethyltryptamine' -e 'h,s,v' -e ',VGG<sub>' -e 'data-link-icon-type="text,' -e 'data-link-icon-type=\"text,' -e '(R,S)' -e 'R,R-formoterol' -e '(18)F-FDG' -e '<em>N,N</em>' -- ./metadata/full.yaml ./metadata/half.yaml | \
             ge -e ',[A-Za-z]'; }
    wrap λ "Look for run-together commas (but exclude chemical names where that's correct)."

    λ(){ grep -E --invert-match '^- - http' ./metadata/*.yaml | ge '[a-zA-Z0-9>]-$'; }
    wrap λ "Look for YAML line breaking at a hyphen."

    λ(){ grep -E -e '[.,:;-<]</a>' -e '\]</a>' -- ./metadata/*.yaml | grep -F --invert-match -e 'i.i.d.' -e 'sativum</em> L.</a>' -e 'this cloning process.</a>' -e '#' -e '[review]</a>' | ge -e '[.,:;-<]</a>'; }
    wrap λ "Look for punctuation inside links; unless it's a full sentence or a quote or a section link, generally prefer to put punctuation outside."

    λ(){ gf -e '**' -e ' _' -e '_ ' -e '!!' -e '*' -- ./metadata/full.yaml ./metadata/half.yaml | grep -F --invert-match '_search_algorithm'; } # need to exclude 'A* search'
    wrap λ "Look for italics errors."

    λ(){ gf -e 'amp#' -- ./metadata/*.yaml; }
    wrap λ "Unicode/HTML entity encoding error?"

    λ(){ grep -E --color=always -e '^- - /docs/.*' -e '^  -  ' -e "\. '$" -e '[a-zA-Z]\.[0-9]\+ [A-Z]' \
            -e 'href="[a-ce-gi-ln-zA-Z]' -e '>\.\.[a-zA-Z]' -e '\]\([0-9]' \
            -e '[⁰ⁱ⁴⁵⁶⁷⁸⁹⁻⁼⁽⁾ⁿ₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ₐₑₒₓₔₕₖₗₘₙₚₛₜ]' -e '<p>Table [0-9]' -e '<p>Figure [0-9]' \
            -e 'id="[0-9]' -e '</[a-z][a-z]\+\?' -e 'via.*ihub' -e " '$" -e "’’" -e ' a [aei]' -e '</[0-9]\+' \
            -e ' - 20[0-9][0-9]:[0-9][0-9]:[0-9][0-9]' -e '#googl$' -e "#googl$'" -e 'gwtag' -e ' <p><strong>[A-Z][A-Z][A-Z]\+</strong>' \
            -e '&org=.*&org=' -e '[0-9]⁄[0-9]\.[0-9]' -e '[0-9]\.[0-9]⁄[0-9]' -e '\[[Kk]eywords\?: ' -- ./metadata/*.yaml; }
    wrap λ "Check possible syntax errors in YAML metadata database (regexp matches)."

    λ(){ grep -F --color=always -e ']{' -e 'id="cb1"' -e '<dd>' -e '<dl>' \
            -e '&lgt;/a>' -e '</a&gt;' -e '&lgt;/p>' -e '/p&gt;' -e '<i><i' -e '</e>' -e '>>' \
            -e '<abstract' -e '<em<' -e '< em>' -e '<em.' -e '<center' -e '<p/>' -e '</o>' -e '< sub>' -e '< /i>' \
            -e '</i></i>' -e '<i><i>' -e 'font-style:italic' -e '<p><p>' -e '</p></p>' -e 'fnref' \
            -e '<figure class="invertible">' -e '</a<' -e 'href="%5Bhttps' -e '<jats:inline-graphic' \
            -e '<figure-inline' -e '<small></small>' -e '<inline-formula' -e '<inline-graphic' -e '<ahref=' \
            -e '](/' -e '-, ' -e '<abstract abstract-type="' -e '- pdftk' -e 'thumb|' -e ' <span>' -- ./metadata/*.yaml; }
    wrap λ "#1: Check possible syntax errors in YAML metadata database (fixed string matches)."
    λ(){ grep -F --color=always -e '<sec ' -e '<list' -e '</list>' -e '<wb<em>r</em>' -e '<abb<em>' -e '<ext-link' -e '<title>' -e '</title>' \
            -e ' {{' -e '<<' -e '[Formula: see text]' -e '<p><img' -e '<p> <img' -e '- - /./' -e '[Keyword' -e '[KEYWORD' \
            -e '[Key word' -e '<strong>[Keywords:' -e 'href="$"' -e 'en.m.wikipedia.org' -e '<em>Figure' \
            -e '<strongfigure' -e ' ,' -e ' ,' -e 'href="Wikipedia"' -e 'href="W"' -e 'href="(' -e '>/em>' -e '<figure>[' \
            -e '<figcaption></figcaption>' -e '&Ouml;' -e '&uuml;' -e '&amp;gt;' -e '&amp;lt;' -e '&amp;ge;' -e '&amp;le;' \
            -e '<ul class="columns"' -e '<ol class="columns"' -e ',/div>' -e '](https://' -e ' the the ' \
            -e 'Ꜳ' -e 'ꜳ'  -e 'ꬱ' -e 'Ꜵ' -e 'ꜵ' -e 'Ꜷ' -e 'ꜷ' -e 'Ꜹ' -e 'ꜹ' -e 'Ꜻ' -e 'ꜻ' -e 'Ꜽ' -e 'ꜽ' -- ./metadata/*.yaml; }
    wrap λ "#2: Check possible syntax errors in YAML metadata database (fixed string matches)."
    λ(){ grep -F --color=always -e '🙰' -e 'ꭁ' -e 'ﬀ' -e 'ﬃ' -e 'ﬄ' -e 'ﬁ' -e 'ﬂ' -e 'ﬅ' -e 'ﬆ ' -e 'ᵫ' -e 'ꭣ' -e ']9h' -e ']9/' \
            -e ']https' -e 'STRONG>' -e '\1' -e '\2' -e '\3' -e ']($' -e '](₿' -e 'M age' -e '….' -e '((' -e ' %' \
            -e '<h1' -e '</h1>' -e '<h2' -e '</h2>' -e '<h3' -e '</h3>' -e '<h4' -e '</h4>' -e '<h5' -e '</h5>' \
            -e '</strong>::' -e ' bya ' -e '?gi=' -e ' ]' -e 'gwsed' -e 'full.full' -e ',,' \
            -e '"!"' -e '</sub<' -e 'xref>' -e '<xref' -e '<e>' -e '\\$' -e 'title="http' -e '%3Csup%3E' -e 'sup%3E' -e ' et la ' \
            -e '<strong>Abstract' -e ' ]' -e "</a>’s" -e 'title="&#39; ' -e 'collapseAbstract' -e 'utm_' \
            -e ' JEL' -e 'top-k' -e '</p> </p>' -e '</sip>' -e '<sip>' -e ',</a>' -e ' : ' -e " ' " -e '>/>a' -e '</a></a>' -e '(, ' \
            -e '&lt;figcaption' -e '{.' -e ' ?' -e " ’’" -e 'lt;/td&gt;' -e "‘’" -e "’‘" -e "’’" -e '<li></li>' -e '</em<em>' -e '𝑂' \
            -e '</a.>' -e ' . ' -e ' , ' -e ' ; ' -e 'class=”collapse”' -e "‘’" -e " ’" -e '<bold>' -e '</bold>' -e '<jats:bold>' \
            -e  '</jats:bold>' -e 'Ã©' -e '</a>s' -e '/&gt;'  -e '&lt;figcaption'  -e 'aria-hidden=">' -e '&gt;</a>' -e '<A Href' \
            -e '</strong>:,' -e ' et al.' -e '<em>et al</em>' -e '<span class="latex">LaTeX</span>' -e '<div>' -e '>LaTeX</a>' -e '>TeX</a>' -e '<em><em>' \
            -e '</em></em>' -e '<strong><strong>' -e '</strong></strong>' -e 'doi:' -e '\\\' -e 'href"http' \
            -e '… .' -e '... .'  -e '– ' -e  ' –' -e '### Competing' -e '<strong></strong>' -e '<span style="font-weight: 400;">' \
            -e '</p> </figcaption>' -e '</p></figcaption>' -e '<figcaption aria-hidden="true"><p>' -e '<figcaption aria-hidden="true"> <p>' \
            -e '<figcaption><p>' -e '<figcaption> <p>' -e 'Your input seems to be incomplete.' -e 'tercile' -e 'tertile' \
            -e '</strong> [' -- ./metadata/*.yaml | \
             grep -F --invert-match 'popular_shelves';
       }
    wrap λ "#3: Check possible syntax errors in YAML metadata database (fixed string matches)."

    λ(){ grep -E -e ' [0-9]/[0-9]\+ ' -- ./metadata/*.yaml | grep -F --invert-match -e 'Toll-like' -e 'Adam' -e '0/1' -e 'My Little Pony Seasons' -e '9/11'; }
    wrap λ "Possible uses of FRACTION SLASH ⁄ or EN DASH –?"

    λ(){ grep -F -e '""' -- ./metadata/*.yaml | grep -F --invert-match -e ' alt=""' -e 'controls=""' -e 'loop=""'; }
    wrap λ "Doubled double-quotes in YAML, usually an error."

    λ(){ grep -F -e "'''" -- ./metadata/full.yaml ./metadata/half.yaml; }
    wrap λ "Triple quotes in YAML, should be curly quotes for readability/safety."

    λ(){ ge --invert-match '^- - ' -- ./metadata/*.yaml | gf -e ' -- ' -e '---'; }
    wrap λ "Markdown hyphen problems in YAML metadata database"

    λ(){ grep -E -e '^    - _' $PAGES | grep -F --invert-match -e '_Additional Poems_' -e '_Aim for the Top!_' -e '_[Cognitive Surplus](!W)_' \
                                              -e '_Fontemon_' -e '_Forbes_' -e '_[Four Major Plays of Chikamatsu](!W)_' \
                                              -e '_[Neon Genesis Evangelion: Angelic Days](!W)_' -e '_Rebuild_' -e '_[Star Maker](!W)_' \
                                              -e '_[The Battles of Coxinga](!W)_' -e '_[The End of Evangelion](!W)_' -e '_The Fountain_' \
                                              -e '_[The Love Suicides at Sonezaki](!W)_' -e '_[The Pleasures of Japanese Literature](!W)_' \
                                              -e '_The Simple Men_'  -e '_Renaming of the Birds_' -e '_[The Love Suicides at Amijima](!W)_' \
                                              -e '_[The Uprooted Pine](!W)_' -e '_[Travelers of a Hundred Ages](!W)_' \
                                              -e '_[Seeds in the Heart: Japanese Literature from Earliest Times to the Late Sixteenth Century](!W)_' \
                                              -e '_[Neon Genesis Evangelion (manga)](!W)_' -e '_[Dendrocnide moroides](!W)_';
          }
    wrap λ "Markdown files: incorrect list nesting using italics for second-level list instead of smallcaps?"

    λ(){ grep --with-filename --perl-regexp -e "[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]" $PAGES; }
    wrap λ "Markdown files: garbage or control characters detected?"

    λ(){  find metadata/ -type f -name "*.html" -exec grep --with-filename --perl-regexp "[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]" {} \;; }
    wrap λ "Metadata HTML files: garbage or control characters detected?"

    λ(){ ge -e '^- - https://en\.wikipedia\.org/wiki/' -- ./metadata/full.yaml; }
    wrap λ "Wikipedia annotations in YAML metadata database, but will be ignored by popups! Override with non-WP URL?"

    λ(){ ge -e '^- - /[12][0-9][0-9]-[a-z]\.pdf$' -- ./metadata/*.yaml; }
    wrap λ "Wrong filepaths in YAML metadata database—missing prefix?"

    λ(){ ge -e ' [0-9]*[02456789]th' -e ' [0-9]*[3]rd' -e ' [0-9]*[2]nd' -e ' [0-9]*[1]st' -- ./metadata/*.yaml | \
             grep -F --invert-match -e '%' -e '<figure>' -e 'alt="Figure ' -e http -e '- - /' -e "- - ! '" -e 'src=' -e "- - '#"; }
    wrap λ "Missing superscript abbreviations in YAML metadata database"

    λ(){ ge -e 'up>T[Hh]<' -e 'up>R[Dd]<' -e 'up>N[Dd]<' -e 'up>S[Tt]<' -- ./metadata/*.yaml; }
    wrap λ "Superscript abbreviations are weirdly capitalized?"

    λ(){ gf -e ' <sup>' -e ' <sub>' -e ' </sup>' -e ' </sub>' -- ./metadata/*.yaml | gf -e ' <sup>242m</sup>Am' -e ' <sup>60</sup>Co' -e ' <sup>2</sup> This is because of the principle' -e ' <sup>3</sup> There are some who' -e ' <sup>4</sup> Such as setting' -e ' <sup>5</sup> Such as buying gifts' -e ' <sup>31</sup>P-Magnetic' ; }
    wrap λ "Superscripts/subscripts have spaces in front?"

    λ(){ ge -e '<p><img ' -e '<img src="http' -e '<img src="[^h/].*"' ./metadata/*.yaml; }
    wrap λ "Check <figure> vs <img> usage, image hotlinking, non-absolute relative image paths in YAML metadata database"

    λ(){ gf -e ' significant'  ./metadata/full.yaml; }
    wrap λ "Misleading language in full.yaml"

    λ(){ gf -e '/doc/www/'  ./metadata/full.yaml; }
    wrap λ "Generated local archive links showing up in manual annotations."

    λ(){ gf -e 'backlink/' -e 'metadata/annotation/' -e '?gi=' -- ./metadata/backlinks.hs; }
    wrap λ "Bad paths in backlinks databases: metadata paths are being annotated when they should not be!"

    λ(){ ge -e '#[[:alnum:]]\+#' -- ./metadata/*.hs ./metadata/*.yaml; }
    wrap λ "Bad paths in metadata databases: redundant anchors"

    λ(){ find _site/ -type f -name "index" | gf -e '{#'; }
    wrap λ "Broken anchors in directory indexes."

    λ(){ find ./ -type f -name '*gwner*' -or -name '*\.htm'; }
    wrap λ "Malformed filenames: dangerous strings in them?"

    λ(){ find ./ -type f -wholename '*[^-a-zA-Z0-9_./~%#]*' | grep -F --invert-match -e 'cattleya幻想写景' -e '緑華野菜子'; }
    wrap λ "Malformed filenames: dangerous characters in them?"

    λ(){
        set +e;
        IFS=$(echo -en "\n\b");
        OTHERS="$(find metadata/annotation/ -name "*.html"; echo index)"
        for PAGE in $PAGES $OTHERS ./static/404; do
            HTML="${PAGE%.page}"
            TIDY=$(tidy -quiet -errors --doctype html5 ./_site/"$HTML" 2>&1 >/dev/null | \
                       grep -F --invert-match -e '<link> proprietary attribute ' -e 'Warning: trimming empty <span>' \
                             -e "Error: missing quote mark for attribute value" -e 'Warning: <img> proprietary attribute "loading"' \
                             -e 'Warning: <svg> proprietary attribute "alt"' -e 'Warning: <source> proprietary attribute "alt"' \
                             -e 'Warning: missing <!DOCTYPE> declaration' -e 'Warning: inserting implicit <body>' \
                             -e "Warning: inserting missing 'title' element" -e 'Warning: <img> proprietary attribute "decoding"' \
                             -e 'Warning: <a> escaping malformed URI reference' -e 'Warning: <script> proprietary attribute "fetchpriority"' \
                             -e 'Warning: <img> lacks "alt" attribute' )
            if [[ -n $TIDY ]]; then echo -e "\n\e[31m$PAGE\e[0m:\n$TIDY"; fi
        done

        set -e;
    }
    wrap λ "Markdown→HTML pages don't validate as HTML5"

    ## anchor-checker.php doesn't work on HTML fragments, like the metadata annotations, and those rarely ever have within-fragment anchor links anyway, so skip those:
    λ() { for PAGE in $PAGES; do
              ANCHOR=$(./static/build/anchor-checker.php "$PAGE")
              if [[ -n $ANCHOR ]]; then echo -e "\n\e[31m$PAGE\e[0m:\n$ANCHOR"; fi
          done;
          }
    wrap λ "Anchors linked but not defined inside page?"

    λ(){ find . -not -name "*#*" -xtype l -printf 'Broken symbolic link: %p\n'; }
    wrap λ "Broken symbolic links"

    λ(){ gwa | grep -F -- '[]' | grep -F --invert-match -e '/newsletter/' -e '/index#manual-annotation' | sort; } # we exclude future newsletter issues as deliberately untagged to avoid appearing at the top of the newsletter tag # | grep -E --invert-match --perl-regexp '\e\[36ma\e\[0m: '
    wrap λ "Untagged annotations."

    ## Is the Internet up?
    ping -q -c 5 google.com  &>/dev/null

    # Testing complete.
  fi

    # Sync:
    set -e
    ## make sure nginx user can list all directories (x) and read all files (r)
    chmod a+x $(find ./ -type d)
    chmod --recursive a+r ./*
    ## sync to Hetzner server: (`--size-only` because Hakyll rebuilds mean that timestamps will always be different, forcing a slower rsync)
    ## If any links are symbolic links (such as to make the build smaller/faster), we make rsync follow the symbolic link (as if it were a hard link) and copy the file using `--copy-links`.
    ## NOTE: we skip time/size syncs because sometimes the infrastructure changes values but not file size, and it's confusing when JS/CSS doesn't get updated; since the infrastructure is so small (compared to eg. doc/*), just force a hash-based sync every time:
    bold "Syncing static/…"
    rsync --perms --exclude=".*" --exclude "*.hi" --exclude "*.o" --exclude "*.elc" --exclude '#*' --exclude='preprocess-markdown' --exclude 'generateLinkBibliography' --exclude='generateDirectory' --exclude='generateSimilar' --exclude='hakyll' --exclude='guessTag' --exclude='link-extractor' --chmod='a+r' --recursive --checksum --copy-links --verbose --itemize-changes --stats ./static/ gwern@176.9.41.242:"/home/gwern/gwern.net/static"
    ## Likewise, force checks of the Markdown pages but skip symlinks (ie. non-generated files):
    bold "Syncing pages…"
    rsync --perms --exclude=".*" --chmod='a+r' --recursive --checksum --quiet --info=skip0 ./_site/  gwern@176.9.41.242:"/home/gwern/gwern.net"
    ## Randomize sync type—usually, fast, but occasionally do a regular slow hash-based rsync which deletes old files:
    bold "Syncing everything else…"
    SPEED=""; if [ "$SLOW" ]; then if ((RANDOM % 100 < 95)); then SPEED="--size-only"; else SPEED="--delete --checksum"; fi; else SPEED="--size-only"; fi
    rsync --perms --exclude=".*" --chmod='a+r' --recursive $SPEED --copy-links --verbose --itemize-changes --stats ./_site/  gwern@176.9.41.242:"/home/gwern/gwern.net"
    set +e

    bold "Expiring ≤100 updated files…"
    # expire CloudFlare cache to avoid hassle of manual expiration: (if more than 100, we've probably done some sort of major systemic change & better to flush whole cache or otherwise investigate manually)
    # NOTE: 'bot-fighting' CloudFlare settings must be largely disabled, otherwise CF will simply CAPTCHA or block outright the various curl/linkchecker tests as 'bots'.
    EXPIRE="$(find . -type f -mtime -1 -not -wholename "*/\.*/*" -not -wholename "*/_*/*" | grep -F --invert-match -e '/doc/www' -e '/static/build/' -e '/static/template/' -e '/static/include/' -e '/metadata/annotation/backlink/' -e '/metadata/annotation/similar/' | xargs ls -t 2>/dev/null | sed -e 's/\.page$//' -e 's/^\.\/\(.*\)$/https:\/\/gwern\.net\/\1/' | head -50) https://gwern.net/sitemap.xml https://gwern.net/lorem https://gwern.net/ https://gwern.net/index https://gwern.net/metadata/today-quote.html https://gwern.net/metadata/today-annotation.html https://gwern.net/metadata/today-site.html"
    for URL in $EXPIRE; do
        echo -n "Expiring: $URL "
        ( curl --silent --request POST "https://api.cloudflare.com/client/v4/zones/57d8c26bc34c5cfa11749f1226e5da69/purge_cache" \
            --header "X-Auth-Email:gwern@gwern.net" \
            --header "Authorization: Bearer $CLOUDFLARE_CACHE_TOKEN" \
            --header "Content-Type: application/json" \
            --data "{\"files\":[\"$URL\"]}" > /dev/null; ) &
    done
    echo

 if [ "$SLOW" ]; then
    # test a random page modified in the past month for W3 validation & dead-link/anchor errors (HTML tidy misses some, it seems, and the W3 validator is difficult to install locally):
    CHECK_RANDOM_PAGE=$(echo "$PAGES" | grep -F --invert-match -e '/fulltext' -e '/lorem' | sed -e 's/\.page$//' -e 's/^\.\/\(.*\)$/https:\/\/gwern\.net\/\1/' \
                       | shuf | head -1 | xargs urlencode)
    CHECK_RANDOM_ANNOTATION=$(find metadata/annotation/ -maxdepth 1 -name "*.html" -type f -size +2k | \
                                  shuf | head -1 | \
                                  sed -e 's/metadata\/annotation\/\(.*\)/\1/' | \
                                  # once for the on-disk escaping, once for the URL argument to the W3C checker
                                  xargs urlencode | xargs urlencode | \
                                  sed -e 's/^\(.*\)$/https:\/\/gwern\.net\/metadata\/annotation\/\1/')
    ( curl --silent --request POST "https://api.cloudflare.com/client/v4/zones/57d8c26bc34c5cfa11749f1226e5da69/purge_cache" \
            --header "X-Auth-Email:gwern@gwern.net" \
            --header "Authorization: Bearer $CLOUDFLARE_CACHE_TOKEN" \
            --header "Content-Type: application/json" \
            --data "{\"files\":[\"$CHECK_RANDOM_PAGE\", \"$CHECK_RANDOM_ANNOTATION\"]}" > /dev/null; )

    # every other sync, check 4 pages in the W3 link-checker. (The error count has been going down thanks to local-archives, so more convenient to check in larger blocks. We'll shift to 8 checks every 4 syncs at some point, and so on.)
    if ((RANDOM % 100 > 50)); then
        # wait a bit for the CF cache to expire so it can refill with the latest version to be checked:
        (if ((RANDOM % 100 > 90)); then sleep 30s && $X_BROWSER "https://validator.w3.org/nu/?doc=$CHECK_RANDOM_PAGE"; fi;
         sleep 5s; $X_BROWSER "https://validator.w3.org/checklink?uri=$CHECK_RANDOM_PAGE&no_referer=on";
         # sleep 5s; $X_BROWSER "https://validator.w3.org/checklink?uri=$CHECK_RANDOM_PAGE&no_referer=on";
         # sleep 5s; $X_BROWSER "https://validator.w3.org/checklink?uri=$CHECK_RANDOM_ANNOTATION&no_referer=on";
         sleep 5s; $X_BROWSER "https://validator.w3.org/checklink?uri=$CHECK_RANDOM_ANNOTATION&no_referer=on";
         if ((RANDOM % 100 > 90)); then $X_BROWSER "https://pagespeed.web.dev/report?url=$CHECK_RANDOM_PAGE&form_factor=desktop"; fi;
        )
    fi

    sleep 30s; chromium --temp-profile "https://gwern.net/index#footer" &> /dev/null & # check the x-of-the-day in a different & cache-free browser instance

    # once in a while, do a detailed check for accessibility issues using WAVE Web Accessibility Evaluation Tool:
    if ((RANDOM % 100 > 99)); then $X_BROWSER "https://wave.webaim.org/report#/$CHECK_RANDOM_PAGE"; fi

    # some of the live popups have probably broken, since websites keep adding X-FRAME options…
    if ((RANDOM % 100 > 90)); then ghci -istatic/build/ ./static/build/LinkLive.hs -e 'linkLiveTestHeaders'; fi

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
         cr 'https://gwern.net/doc/cs/1955-nash' 'https://gwern.net/doc/cs/cryptography/1955-nash'
         cr 'https://gwern.net/doc/cs/cryptography/1955-nash' 'https://gwern.net/doc/cs/cryptography/1955-nash' # check www.gwern.net → gwern.net redirect

       }
    wrap λ "Check that some redirects go where they should"
    λ() { cm () { [[ "$1" != $(c --write-out '%{content_type}' "$2") ]] && echo "$1" "$2"; }
          ### check key pages:
          ## check every possible extension:
          ## check some random ones:
          cm "application/epub+zip" 'https://gwern.net/doc/anime/eva/2002-takeda-notenkimemoirs.epub'
          cm "application/font-sfnt" 'https://gwern.net/static/font/drop-cap/kanzlei/Kanzlei-Initialen-M.ttf'
          cm "application/javascript" 'https://gwern.net/doc/statistics/order/beanmachine-multistage/script.js'
          cm "application/javascript" 'https://gwern.net/static/js/rewrite.js'
          cm "application/javascript" 'https://gwern.net/static/js/sidenotes.js'
          cm "application/json" 'https://gwern.net/doc/touhou/2013-c84-downloads.json'
          cm "application/msaccess" 'https://gwern.net/doc/touhou/2013-06-08-acircle-tohoarrange.mdb'
          cm "application/msword" 'https://gwern.net/doc/iq/2014-tenijenhuis-supplement.doc'
          cm "application/octet-stream" 'https://gwern.net/doc/zeo/firmware-v2.6.3R-zeo.img'
          cm "application/pdf" 'https://gwern.net/doc/cs/hardware/2010-bates.pdf'
          cm "application/pdf" 'https://gwern.net/doc/history/1694-gregory.pdf'
          cm "application/pdf" 'https://gwern.net/doc/statistics/decision/1994-benter.pdf'
          cm "application/vnd.ms-excel" 'https://gwern.net/doc/dual-n-back/2012-05-30-kundu-dnbrapm.xls'
          cm "application/vnd.oasis.opendocument.spreadsheet" 'https://gwern.net/doc/genetics/heritable/1980-osborne-twinsblackandwhite-appendix.ods'
          cm "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" 'https://gwern.net/doc/cs/2010-nordhaus-nordhaus2007twocenturiesofproductivitygrowthincomputing-appendix.xlsx'
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
          cm "text/html; charset=utf-8" 'https://gwern.net/doc/cs/2012-terencetao-anonymity.html'
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
          cm "text/markdown; charset=utf-8" 'https://gwern.net/2014-spirulina.page'
          cm "text/markdown; charset=utf-8" 'https://gwern.net/dnm-archive.page'
          cm "text/markdown; charset=utf-8" 'https://gwern.net/gpt-3.page'
          cm "text/plain; charset=utf-8" 'https://gwern.net/doc/personal/2009-sleep.txt'
          cm "text/plain; charset=utf-8" 'https://gwern.net/static/redirect/nginx.conf'
          cm "text/x-adobe-acrobat-drm" 'https://gwern.net/doc/dual-n-back/2012-zhong.ebt'
          cm "text/x-haskell; charset=utf-8" 'https://gwern.net/static/build/hakyll.hs'
          cm "text/x-opml; charset=utf-8" 'https://gwern.net/doc/personal/rss-subscriptions.opml'
          cm "text/x-patch; charset=utf-8" 'https://gwern.net/doc/ai/music/2019-12-22-gpt2-preferencelearning-gwern-abcmusic.patch'
          cm "text/x-r; charset=utf-8" 'https://gwern.net/static/build/linkAbstract.R'
          cm "text/plain; charset=utf-8" 'https://gwern.net/static/build/linkArchive.sh'
          cm "text/yaml; charset=utf-8" 'https://gwern.net/metadata/full.yaml'
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
    wrap λ "The live MIME types are incorrect"

    ## known-content check:
    elinks -dump 'https://gwern.net/index'   | grep -F --quiet -e 'This Is The Website' || red "/index content-check failed"
    elinks -dump 'https://gwern.net/zeo/zeo' | grep -F --quiet -e 'lithium orotate'     || red "/zeo/zeo content-check failed"

    ## check that tag-directories have the right thumbnails (ie. *not* the fallback thumbnail):
    λ(){ curl --silent 'https://gwern.net/doc/sociology/index' 'https://gwern.net/doc/psychology/index' 'https://gwern.net/doc/economics/index' | \
             grep -F 'https://gwern.net/static/img/logo/logo-whitebg-large-border.png'; }
    wrap λ "Tag-directories missing their automatically-extracted-from-annotation thumbnails & instead having the site-wide default thumbnail?"

    ## Check that password protection is working:
    λ() { URL="https://gwern.net/private/canary"
          USERNAME="guest"
          PASSWORD="password"
          CANARY_TOKEN_SUBSTRING="c1e1908dac358d40e7e2"

          # Use curl with & without basic authentication to download the webpage:
          RESPONSE=$(curl --silent --user "$USERNAME:$PASSWORD" "$URL"; curl --silent "$URL")

          # Check if the canary token is present in the downloaded content:
          if echo "$RESPONSE" | grep -F --quiet "$CANARY_TOKEN_SUBSTRING"; then
              echo "Error: Canary token found! $RESPONSE"
          fi
    }
    wrap λ "Canary token was downloadable; nginx password-protection security failed?"

    ## did any of the key pages mysteriously vanish from the live version?
    linkchecker --ignore-url='https://www.googletagmanager.com' --threads=5 --check-extern --recursion-level=1 'https://gwern.net/'
    ## - traffic checks/alerts are done in Google Analytics: alerts on <900 pageviews/daily, <40s average session length/daily.
    ## - latency/downtime checks are done in `updown.io` (every 1h, 1s response-time for /index)
    set +e
  fi

    # Cleanup post:
    rm --recursive --force -- ./_cache/ ./_site/ || true

  if [ "$SLOW" ]; then
    # Testing files, post-sync
    bold "Checking for file anomalies…"
    λ(){ fdupes --quiet --sameline --size --nohidden $(find ./ -type d | grep -E --invert-match -e 'static' -e '.git' -e 'gwern/wiki/$' -e 'metadata/annotation/backlink' -e 'metadata/annotation/similar' -e 'metadata/annotation/link-bibliography') | grep -F --invert-match -e 'bytes each' -e 'trimfill.png'; }
    wrap λ "Duplicate file check"

    λ(){ find ./ -type f | grep -F --invert-match -e 'git/' -e 'newsletter/' -e 'doc/rotten.com/' -e 'doc/www/' -e 'metadata/annotation/' -e 'doc/personal/2011-gwern-yourmorals.org/' -e 'index.page' -e 'index.html' -e 'favicon.ico' -e 'generator_config.txt' -e '.gitignore' -e 'static/build/Config/' | xargs --max-procs=0 --max-args=1 basename  | sort | uniq --count | grep -E --invert-match -e '^ +1 ' | sort --numeric-sort; }
    wrap λ "File base names are preferably globally-unique, to avoid issues with duplicate search results and clashing link IDs."

    λ() { find . -perm u=r -path '.git' -prune; }
    wrap λ "Read-only file check" ## check for read-only outside ./.git/ (weird but happened):

    λ(){ gf -e 'RealObjects' -e '404 Not Found Error: No Page' -e ' may refer to:' ./metadata/auto.yaml; }
    wrap λ "Broken links, corrupt authors', or links to Wikipedia disambiguation pages in auto.yaml."

    λ(){ (find . -type f -name "*--*"; find . -type f -name "*~*"; ) | grep -F --invert-match -e metadata/annotation/; }
    wrap λ "No files should have double hyphens or tildes in their names."

    λ(){ grep -F --before-context=1 -e 'Right Nothing' -e 'Just ""' ./metadata/archive.hs; }
    wrap λ "Links failed to archive (broken)."

    λ(){ find . -type f | grep -F --invert-match -e '.'; }
    wrap λ "Every file should have at least one period in them (extension)."

    λ(){ find . -type f -name "*\.*\.page" | grep -F --invert-match -e '404.page'; }
    wrap λ "Markdown files should have exactly one period in them."

    λ(){ find . -type f -name "*.html.html.html"; }
    wrap λ "Found a triple-.html HTML file; weird! Syntax-highlighting gone astray?"

    λ(){ find . -type f -mtime +3 -name "*#*" -or -type f -name "temp[0-9]*"; }
    wrap λ "Stale temporary files?"

    λ(){ find * -type d -empty; }
    wrap λ "Unused or empty directories?"

    bold "Checking for HTML anomalies…"
    λ(){ BROKEN_HTMLS="$(find ./ -type f -name "*.html" | grep -F --invert-match 'static/' | \
                         parallel --max-args=500 "grep -F --ignore-case --files-with-matches \
                         -e '404 Not Found' -e '<title>Sign in - Google Accounts</title' -e 'Download Limit Exceeded' -e 'Access Denied'" | sort)"
         for BROKEN_HTML in $BROKEN_HTMLS; do
             grep --before-context=3 "$BROKEN_HTML" ./metadata/archive.hs | grep -F --invert-match -e 'Right' -e 'Just';
         done; }
    wrap λ "Archives of broken links"

    ## 'file' throws a lot of false negatives on HTML pages, often detecting XML and/or ASCII instead, so we whitelist some:
    λ(){ find ./ -type f -name "*.html" | grep -F --invert-match -e 4a4187fdcd0c848285640ce9842ebdf1bf179369 -e 5fda79427f76747234982154aad027034ddf5309 \
                                                -e f0cab2b23e1929d87f060beee71f339505da5cad -e a9abc8e6fcade0e4c49d531c7d9de11aaea37fe5 \
                                                -e 2015-01-15-outlawmarket-index.html -e ac4f5ed5051405ddbb7deabae2bce48b7f43174c.html \
                                                -e %3FDaicon-videos.html -e 86600697f8fd73d008d8383ff4878c25eda28473.html \
                                                -e '16aacaabe05dfc07c0e966b994d7dd0a727cd90e' -e 'metadata/today-quote.html' -e 'metadata/today-annotation.html' \
                                                -e '023a48cb80d48b1438d2accbceb5dc8ad01e8e02' -e '/Starr_Report/' \
             | parallel --max-args=500 file | grep -F --invert-match -e 'HTML document, ' -e 'ASCII text' -e 'LaTeX document, UTF-8 Unicode text'; }
    wrap λ "Corrupted filetype HTMLs"

    ## having noindex tags causes conflicts with the robots.txt and throws SEO errors; except in the ./doc/www/ mirrors, where we don't want them to be crawled:
    λ(){ find ./ -type f -name "*.html" | grep -F --invert-match -e './doc/www/' -e './static/404' -e './static/template/default.html' -e 'lucky-luciano' | xargs grep -F --files-with-matches 'noindex'; }
    wrap λ "Noindex tags detected in HTML pages."

    bold "Checking for PDF anomalies…"
    λ(){ BROKEN_PDFS="$(find ./ -type f -name "*.pdf" -not -size 0 | sort | parallel --max-args=500 file | \
                                grep --invert-match 'PDF document' | cut -d ':' -f 1)"
         for BROKEN_PDF in $BROKEN_PDFS; do
             echo "$BROKEN_PDF"; grep --before-context=3 "$BROKEN_PDF" ./metadata/archive.hs;
         done; }
    wrap λ "Corrupted or broken PDFs"

    λ(){
        LL="$(curl --silent https://ifconfig.me/ip)"
        export LL # some academic publishers inject spam, but not in an easy-to-grep way; I've noticed they sometimes insert the download IP, however, so we add that dynamically as a search string.
        checkSpamHeader () {
            # extract text from first page, where the junk usually is (some insert at the end, but those are less of an issue & harder to check):
            HEADER=$(pdftotext -f 1 -l 1 "$@" - 2> /dev/null | \
                         grep -F -e 'INFORMATION TO USERS' -e 'Your use of the JSTOR archive indicates your acceptance of JSTOR' \
                               -e 'This PDF document was made available from www.rand.org as a public' -e 'A journal for the publication of original scientific research' \
                               -e 'This is a PDF file of an unedited manuscript that has been accepted for publication.' \
                               -e 'Additional services and information for ' -e 'Access to this document was granted through an Emerald subscription' \
                               -e 'PLEASE SCROLL DOWN FOR ARTICLE' -e 'ZEW Discussion Papers' -e "$LL" -e 'eScholarship.org' \
                  -e 'Full Terms & Conditions of access and use can be found at' -e 'This paper is posted at DigitalCommons' -e 'This paper is included in the Proceedings of the' \
                  -e 'See discussions, stats, and author profiles for this publication at' \
                  -e 'This article appeared in a journal published by Elsevier. The attached' \
                  -e 'The user has requested enhancement of the downloaded file' \
                  -e 'See discussions, stats, and author profiles for this publication at: https://www.researchgate.net' \
                  -e 'This paper has been accepted for publication in ')
            if [ "$HEADER" != "" ]; then echo "Header: $@"; fi;
        }
        export -f checkSpamHeader
        find ./doc/ -type f -name "*.pdf" | grep -F --invert-match -e 'doc/www/' | sort | parallel checkSpamHeader
    }
    wrap λ "Remove academic-publisher wrapper junk from PDFs."

    removeEncryption () { ENCRYPTION=$(exiftool -quiet -quiet -Encryption "$@");
                          if [ "$ENCRYPTION" != "" ]; then
                              echo "$@"
                              TEMP=$(mktemp /tmp/encrypted-XXXX.pdf)
                              pdftk "$@" input_pw output "$TEMP" && mv "$TEMP" "$@";
                          fi; }
    export -f removeEncryption
    find ./ -type f -name "*.pdf" -not -size 0 | sort | parallel removeEncryption

    λ(){ find ./ -type f -name "*.djvu"; }
    wrap λ "Legacy DjVu detected (convert to JBIG2 PDF; see <https://gwern.net/design-graveyard#djvu-files>)."

    bold "Checking for image anomalies…"
    λ(){ find ./doc/ -type f -name "*.jpg" | parallel --max-args=500 file | grep -F --invert-match 'JPEG image data'; }
    wrap λ "Corrupted JPGs"

    λ(){ find ./doc/ -type f -name "*.png" | parallel --max-args=500 file | grep -F --invert-match 'PNG image data'; }
    wrap λ "Corrupted PNGs"

    λ(){  find ./doc/ -name "*.png" | grep -F --invert-match -e '/static/img/' -e '/doc/www/misc/' | sort | xargs identify -format '%F %[opaque]\n' | grep -F ' false'; }
    wrap λ "Partially transparent PNGs (may break in dark mode, convert with 'mogrify -background white -alpha remove -alpha off')"

    λ(){ find ./ -type f -name "*.gif" | grep -F --invert-match -e 'static/img/' -e 'doc/gwern.net-gitstats/' -e 'doc/rotten.com/' -e 'doc/genetics/selection/www.mountimprobable.com/' | parallel --max-args=500 identify | grep -E '\.gif\[[0-9]\] '; }
    wrap λ "Animated GIF is deprecated; GIFs should be converted to WebMs/MP4s."

    λ(){ JPGS_BIG="$(find ./doc/ -type f -name "*.jpg" | parallel --max-args=500 "identify -format '%Q %F\n'" {} | sort --numeric-sort | grep -E -e '^[7-9][0-9] ' -e '^6[6-9]' -e '^100')"
    echo "$JPGS_BIG"
    compressJPG2 $(echo "$JPGS_BIG" | cut --delimiter=' ' --field=2); }
    wrap λ "Compressing high-quality JPGs to ≤65% quality…"

    bold "Compressing new PNGs…"
    png $(find ./doc/ -type f -name "*.png" -mtime -3 | grep -F --invert-match -e './doc/www/misc/')

    ## Find JPGS which are too wide (1600px is an entire screen width on even wide monitors, which is too large for a figure/illustration):
    λ() { for IMAGE in $(find ./doc/ -type f -name "*.jpg" -or -name "*.png" | grep -F --invert-match -e '2020-07-19-oceaninthemiddleofanisland-gpt3-chinesepoetrytranslation.png' -e '2020-05-22-caji9-deviantart-stylegan-ahegao.png' -e '2021-gwern-meme-virginvschad-journalpapervsblogpost.png' -e 'tadne-l4rz-kmeans-k256-n120k-centroidsamples.jpg' -e '2009-august-newtype-rebuildinterview-maayasakamoto-pg090091.jpg' -e 'doc/fiction/science-fiction/batman/' -e 'doc/ai/nn/transformer/gpt/dall-e/2/' -e '2022-09-21-gwern-stablediffusionv14-circulardropcapinitialsamples.png' -e '2022-09-22-gwern-stablediffusionv14-textualinversion-yinit-dropcapsexperiments.png' -e '2022-09-27-gwern-gwernnet-indentjustification2x2abtest.png' -e 'reinforcement-learning/2022-bakhtin' -e 'technology/2021-roberts-figure2' -e '2022-10-02-mollywhite-annotate-latecomersdesktopscreenshot.png' -e '/doc/anime/eva/' -e 'doc/www/misc/' -e '2021-power-poster.png' -e '2002-change-table2-preandposttestscoresultsfrommindmappingshowminimaleffect.png' -e 'genetics/selection/www.mountimprobable.com/assets/images/card.png' -e 'reinforcement-learning/imperfect-information/diplomacy/2022-bakhtin-figure6-successfulcicerohumandialogueexamplesfromtestgames.jpg' -e 'reinforcement-learning/imperfect-information/diplomacy/2022-bakhtin-figure3-differentcicerointentsleadtodifferentdialogues.jpg' -e 'reinforcement-learning/imperfect-information/diplomacy/2022-bakhtin-figure5-theeffectofdialogueoncicerosplanningandintents3possiblescenariosinanegotiationwithengland.jpg' -e 'reinforcement-learning/imperfect-information/diplomacy/2022-bakhtin-figure2-trainingandinferenceofcicerointentcontrolleddialogue.jpg' -e 'reinforcement-learning/imperfect-information/diplomacy/2022-bakhtin-figure1-architectureofcicerodiplomacyagent.jpg' -e '2021-roberts-figure2-manufacturingofhumanbloodbricks.jpg' -e 'gwern-gwernnet' ); do
              SIZE_W=$(identify -format "%w" "$IMAGE")
              if (( SIZE_W > 1600  )); then
                  echo "Too wide image: $IMAGE $SIZE_W; shrinking…";
                  mogrify  -resize 1600x10000 "$IMAGE";
              fi;
          done; }
    wrap λ "Too-wide images (downscale)"

    bold "Running site functionality checks…"
    ## Look for domains that may benefit from link icons or link live status now:
    λ() { ghci -istatic/build/ ./static/build/LinkIcon.hs  -e 'linkIconPrioritize' | grep -F --invert-match -e ' secs,' -e 'it :: [(Int, T.Text)]' -e '[]'; }
    wrap λ "Need link icons?"
    λ() { ghci -istatic/build/ ./static/build/LinkLive.hs  -e 'linkLivePrioritize' | grep -F --invert-match -e ' secs,' -e 'it :: [(Int, T.Text)]' -e '[]'; }
    wrap λ "Need link live whitelist/blacklisting?"

    λ() { find ./metadata/annotation/similar/ -type f -name "*.html" | xargs --max-procs=0 --max-args=5000 grep -F --no-filename -e '<a href="' -- | sort | uniq --count | sort --numeric-sort | grep -E '^ \+[4-9][0-9]\+ \+'; }
    wrap λ "Similar-links: overused links indicate pathological lookups; blacklist links as necessary."

    λ(){ ghci -i/home/gwern/wiki/static/build/ ./static/build/XOfTheDay.hs -e 'sitePrioritize' | \
             grep -F --invert-match -e ' secs,' -e 'it :: [T.Text]' -e '[]' || true; }
    wrap λ "Site-of-the-day: check for recommendations?"

    λ() { (cd ./static/build/ && find ./ -type f -name "*.hs" -exec ghc -fno-code {} \; ) 2>&1 >/dev/null; }
    wrap λ "Test-compilation of all Haskell files in static/build: failure."

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
        λ(){ (find . -name "*.page"; find ./metadata/annotation/ -maxdepth 1 -name "*.html") | shuf | \
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
        for URL in $(find . -type f -name "*.page" | parallel --max-args=500 runghc -istatic/build/ ./static/build/link-extractor.hs | \
                         grep -E -e '^/' | cut --delimiter=' ' --field=1 | sort -u); do
            echo "$URL"
            MIME=$(curl --silent --max-redirs 0 --output /dev/null --write '%{content_type}' "https://gwern.net$URL");
            if [[ "$MIME" == "" ]]; then red "redirect! $URL (MIME: $MIME)"; fi;
        done

        for URL in $(find . -type f -name "*.page" | parallel --max-args=500 runghc -istatic/build/ ./static/build/link-extractor.hs | \
                         grep -E -e '^https://gwern.net' | sort -u); do
            MIME=$(curl --silent --max-redirs 0 --output /dev/null --write '%{content_type}' "$URL");
            if [[ "$MIME" == "" ]]; then red "redirect! $URL"; fi;
        done
    fi
  fi

    rm static/build/generateLinkBibliography static/build/*.hi static/build/*.o &>/dev/null || true

    bold "Sync successful"
fi
