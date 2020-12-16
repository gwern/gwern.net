#!/bin/bash

# sync-gwern.net.sh: shell script which automates a full build and sync of gwern.net. A simple build can be done using 'runhaskell hakyll.hs build', but that is slow, semi-error-prone (did you remember to delete all intermediates?), and does no sanity checks or optimizations like compiling the MathJax to static CSS/fonts (avoiding multi-second JS delays).
#
# This script automates all of that: it cleans up, compiles a hakyll binary for faster compilation, generates a sitemap XML file, optimizes the MathJax use, checks for many kinds of errors, uploads, and cleans up.
#
# Author: Gwern Branwen
# Date: 2016-10-01
# When:  Time-stamp: "2019-09-15 14:38:14 gwern"
# License: CC-0

# key dependencies: GHC, Hakyll, s3cmd, emacs, curl, tidy (HTML5 version), urlencode ('gridsite-clients' package), linkchecker, fdupes, ImageMagick, exiftool, mathjax-node-page (eg `npm i -g mathjax-node-page`), parallel, xargs...

if [[ -n $(command -v ghc) && -n $(command -v git) && -n $(command -v rsync) && -n $(command -v curl) && -n $(command -v ping) && \
          -n $(command -v tidy) && -n $(command -v linkchecker) && -n $(command -v du) && -n $(command -v rm) && -n $(command -v find) && \
          -n $(command -v fdupes) && -n $(command -v urlencode) && -n $(command -v sed) && -n $(command -v parallel) && \
          -n $(command -v file) && -n $(command -v exiftool) && -n $(command -v identify) ]] && \
       [ -z $(pgrep "hakyll") ];
then
    set -e
    cd ~/wiki/ && (git status &)

    ## Update the directory listing pages: there are a number of directories we want to avoid, like the various mirrors or JS projects, or directories just of data like CSVs, or dumps of docs, so we'll use a whitelist of directories which have files which may have decent annotations & be worth browsing:
    DIRS="docs/ docs/ai/ docs/ai/anime/ docs/ai/music/ docs/ai/poetry/ docs/algernon/ docs/anime/ docs/aspirin/ \
          docs/biology/ docs/bitcoin/ docs/borges/ docs/catnip/ docs/co2/ docs/conscientiousness/ \
          docs/creatine/ docs/cs/ docs/culture/ docs/design/ docs/dnb/ docs/economics/ docs/elections/ docs/eva/ docs/fiction/ \
          docs/genetics/ docs/genetics/correlation/ docs/genetics/editing/ docs/genetics/heritable/ docs/genetics/selection/ \
          docs/history/ docs/history/medici/ docs/iodine/ docs/iq/ docs/iq/fullerton/ docs/iq/munich/ docs/iq/roe/ docs/iq/smpy/ \
          docs/japanese/ docs/japanese/zeami/ docs/linkrot/ docs/lithium/ docs/longevity/ docs/longevity/aspirin/ \
          docs/math/ docs/melatonin/ docs/modafinil/ docs/modafinil/blackmarkets/ \
          docs/modafinil/survey/ docs/music-distraction/ docs/nature/ docs/nicotine/ docs/nootropics/ docs/philo/ \
          docs/philo/brethrenofpurity/ docs/predictions/ docs/psychology/ \
          docs/psychology/okcupid/ docs/psychology/writing/ docs/radiance/ docs/rl/ docs/science/ docs/sociology/ docs/spacedrepetition/ \
          docs/sr/ docs/statistics/ docs/statistics/bayes/ docs/statistics/bias/ docs/statistics/causality/ \
          docs/statistics/comparison/ docs/statistics/decision/ docs/statistics/meta-analysis/ docs/statistics/order/ \
          docs/statistics/peerreview/ docs/sunkcosts/ docs/tcs/ docs/tea/ docs/technology/ docs/terrorism/ docs/tominaga/ \
          docs/touhou/ docs/traffic/ docs/transhumanism/ docs/vitamind/ docs/wikipedia/ docs/xrisks/ docs/zeo/"
    generateDirectory () {
                rm "$1/index.page" > /dev/null || true;
                runhaskell -istatic/build/ static/build/generateDirectory.hs "$1" > "$1/index.page"; }
    generateDirectory docs/ai/poetry/ # test run; doing 1 run first also updates auto.yaml as necessary while avoiding race conditions in parallel updates
    export -f generateDirectory
    echo "$DIRS" | tr --squeeze-repeats ' ' '\n' | parallel --jobs 16 --progress generateDirectory

    cd ./static/ && (git status; git pull; git push &)
    cd ./build/
    # Cleanup pre:
    rm --recursive --force -- ~/wiki/_cache/ ~/wiki/_site/ ./static/build/hakyll ./static/build/*.o ./static/build/*.hi || true

    # Build:
    ## gwern.net is big and Hakyll+Pandoc is slow, so it's worth the hassle of compiling an optimized version to build
    ghc -tmpdir /tmp/ -Wall -optl-fuse-ld=gold -rtsopts -threaded -O2 --make hakyll.hs
    ## Parallelization:
    N="$(if [ ${#} == 0 ]; then echo 8; else echo "$1"; fi)"
    cd ../../ # go to site root
    ./static/build/hakyll build +RTS -N"$N" -RTS || exit 1

    ## WARNING: this is a crazy hack to insert a horizontal rule 'in between' the first 3 sections on /index (Newest/Popular/Notable), and the rest (starting with Statistics); the CSS for making the rule a block dividing the two halves just doesn't work in any other way, but Pandoc Markdown doesn't let you write stuff 'in between' sections, either. So... a hack.
    sed -i -e 's/section id=\"statistics\"/hr class="horizontalRule-nth-2"> <section id="statistics"/' ./_site/index

    ## generate a sitemap file for search engines:
    ## possible alternative implementation in hakyll: https://www.rohanjain.in/hakyll-sitemap/
    (echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?> <urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
     ## very static files which rarely change: PDFs, images, site infrastructure:
     find -L _site/docs/ _site/images/ _site/static/ -not -name "*.page" -type f | fgrep --invert-match -e 'docs/www/' -e 'metadata/' | sort | parallel urlencode -m |
         sed -e 's/_site\/\(.*\)/\<url\>\<loc\>https:\/\/www\.gwern\.net\/\1<\/loc><changefreq>never<\/changefreq><\/url>/'
     ## Everything else changes once in a while:
     find -L _site/ -not -name "*.page" -type f | fgrep --invert-match -e 'static/' -e 'docs/' -e 'images/' -e 'Fulltext' | sort | parallel urlencode -m |
         sed -e 's/_site\/\(.*\)/\<url\>\<loc\>https:\/\/www\.gwern\.net\/\1<\/loc><changefreq>monthly<\/changefreq><\/url>/'
     echo "</urlset>") >> ./_site/sitemap.xml

    ## use https://github.com/pkra/mathjax-node-page/ to statically compile the MathJax rendering of the MathML to display math instantly on page load
    ## background: https://joashc.github.io/posts/2015-09-14-prerender-mathjax.html ; installation: `npm install --prefix ~/src/ mathjax-node-page`
    staticCompileMathJax () {
        fgrep --quiet '<span class="math inline"' "$@" && \
            TARGET=$(mktemp /tmp/XXXXXXX.html) && \
            cat "$@" | ~/src/node_modules/mathjax-node-page/bin/mjpage --output CommonHTML | \
                ## WARNING: experimental CSS optimization: can't figure out where MathJax generates its CSS which is compiled,
                ## but it potentially blocks rendering without a 'font-display: swap;' parameter (which is perfectly safe since the user won't see any math early on)
                sed -e 's/^\@font-face {/\@font-face {font-display: swap; /' \
                    >> "$TARGET" && \
                    mv "$TARGET" "$@" && echo "$@ succeeded" # || echo $@ 'failed MathJax compilation!';
    }
    export -f staticCompileMathJax
    find ./ -path ./_site -prune -type f -o -name "*.page" | sort | sed -e 's/\.page//' -e 's/\.\/\(.*\)/_site\/\1/' | parallel staticCompileMathJax || true

    rm -- ./static/build/hakyll ./static/build/*.o ./static/build/*.hi || true

    # Testing compilation results:
    set +e
    ## function to wrap checks and print red-highlighted warning if non-zero output (self-documenting):
    wrap() { OUTPUT=$($1 2>&1)
             WARN="$2"
             if [ -n "$OUTPUT" ]; then
                 echo -e "\e[41m$WARN\e[0m":
                 echo -e "$OUTPUT";
             fi; }

    λ(){ VISIBLE_N=$(cat ./_site/sitemap.xml | wc --lines); [ "$VISIBLE_N" -le 13040 ] && echo "$VISIBLE_N" && exit 1; }
    wrap λ "Sanity-check number-of-public-site-files in sitemap.xml failed"

    λ(){ COMPILED_N="$(find -L ./_site/ -type f | wc --lines)"
         [ "$COMPILED_N" -le 21000 ] && echo "File count: $COMPILED_N" && exit 1;
         COMPILED_BYTES="$(du --summarize --total --dereference --bytes ./_site/ | tail --lines=1 | cut --field=1)"
         [ "$COMPILED_BYTES" -le 41000000000 ] && echo "Total filesize: $COMPILED_BYTES" && exit 1; }
    wrap λ "Sanity-check: number of files & file-size"

    λ(){ find ./ -name "*.page" | fgrep --invert-match '_site' | sort | sed -e 's/\.page//' -e 's/\.\/\(.*\)/_site\/\1/'  | parallel fgrep --with-filename --color=always '!Wikipedia'; }
    wrap λ "Stray interwiki links"

    λ(){ PAGES=$(find ./ -name "*.page" | fgrep --invert-match '_site' | sort | sed -e 's/\.page//' -e 's/\.\/\(.*\)/_site\/\1/')
       for PAGE in $PAGEs; do fgrep --color=always -e '<span class="smallcaps-auto"><span class="smallcaps-auto">' "$PAGE"; done }
    wrap λ "Smallcaps-auto regression"

    λ(){ find ./ -name "*.page" -type f -exec egrep -e 'cssExtension: [a-c,e-z]' {} \; ; }
    wrap λ "Incorrect drop caps"

    λ(){ find -L . -type f -size 0  -printf 'Empty file: %p %s\n' | fgrep -v '.git/FETCH_HEAD'; }
    wrap λ "Empty files"

    λ(){ find ./_site/ -type f -not -name "*.*" -exec grep --quiet --binary-files=without-match . {} \; -print0 | parallel --null --max-args=5000 "fgrep --with-filename -- '————–'"; }
    wrap λ "Broken table"

    λ(){ find ./ -name "*.page" | fgrep --invert-match '_site' | sort | sed -e 's/\.page//' -e 's/\.\/\(.*\)/_site\/\1/'  | parallel --max-args=5000 "fgrep --with-filename -- '<span class=\"er\">'" | fgrep -v '<span class="er">foo!'; } # NOTE: filtered out Lorem.page's deliberate CSS test-case use of it
    wrap λ "Broken code"

    λ(){ egrep -e '/home/gwern/' -e '^- - /doc/.*' -e '^  -  ' -e ']{.smallcaps-auto}' -e ']{.smallcaps}' -- ./metadata/custom.yaml; }
    wrap λ "Check possible typo in custom.yaml"

    λ(){ egrep -e '<img src="http' -e '<img src="[^h/].*"'  ./metadata/custom.yaml | fgrep -v 'wikimedia.org'; }
    wrap λ "Check image hotlinking & non-absolute relative image paths in custom.yaml"

    λ() {
        set +e;
        IFS=$(echo -en "\n\b");
        PAGES="$(find . -type f -name "*.page" | grep -v -e '_site/' -e 'Book-reviews' | sort -u)"
        OTHERS="$(find ./_site/tags/ -type f | sed -e 's/\.\/_site//')"
        for PAGE in $PAGES $OTHERS ./static/404.html; do
            HTML="${PAGE%.page}"
            TIDY=$(tidy -quiet -errors --doctype html5 ./_site/"$HTML" 2>&1 >/dev/null | \
                       fgrep --invert-match -e '<link> proprietary attribute ' -e 'Warning: trimming empty <span>' \
                             -e "Error: missing quote mark for attribute value" -e 'Warning: <img> proprietary attribute "loading"' \
                             -e 'Warning: <svg> proprietary attribute "alt"' )
            if [[ -n $TIDY ]]; then echo -e "\n\e[31m$PAGE\e[0m:\n$TIDY"; fi
        done;
        set -e; }
    wrap λ "Markdown→HTML pages don't validate as HTML5"

    ## Is the Internet up?
    ping -q -c 5 google.com  &> /dev/null

    # Sync:
    ## make sure nginx user can list all directories (x) and read all files (r)
    chmod a+x $(find ~/wiki/ -type d)
    chmod --recursive a+r ~/wiki/*

    λ(){ find . -xtype l -printf 'Broken symbolic link: %p\n'; }
    wrap λ "Broken symbolic links"

    set -e
    ping -q -c5 google.com
    ## sync to Hetzner server: (`--size-only` because Hakyll rebuilds mean that timestamps will always be different, forcing a slower rsync)
    ## If any links are symbolic links (such as to make the build smaller/faster), we make rsync follow the symbolic link (as if it were a hard link) and copy the file using `--copy-links`.
    ## Randomize sync type - usually, fast, but occasionally do a regular slow hash-based rsync which deletes old files:
    SPEED=""; if ((RANDOM % 100 < 99)); then SPEED="--size-only"; else SPEED="--delete"; fi;
    rsync --chmod='a+r' --recursive $SPEED --copy-links --verbose --itemize-changes --stats ./_site/ gwern@78.46.86.149:"/home/gwern/gwern.net"
    set +e

    # expire CloudFlare cache to avoid hassle of manual expiration:
    EXPIRE="$(find . -type f -mtime -1 -not -wholename "*/\.*/*" -not -wholename "*/_*/*" | sed -e 's/\.page//' -e 's/^\.\/\(.*\)$/https:\/\/www\.gwern\.net\/\1/' | sort ) https://www.gwern.net/sitemap.xml https://www.gwern.net/index"
    for URL in $EXPIRE; do
        echo -n "Expiring: $URL "
        curl --silent --request POST "https://api.cloudflare.com/client/v4/zones/57d8c26bc34c5cfa11749f1226e5da69/purge_cache" \
            --header "X-Auth-Email:gwern@gwern.net" \
            --header "Authorization: Bearer $CLOUDFLARE_CACHE_TOKEN" \
            --header "Content-Type: application/json" \
            --data "{\"files\":[\"$URL\"]}" | jq '.success'
        curl --silent "$URL" > /dev/null &
    done

    # test a random page modified in the past month for W3 validation errors (HTML tidy misses some, it seems, and the W3 validator is difficult to install locally):
    CHECK_RANDOM=$(find . -type f -mtime -31 -name "*.page" | sed -e 's/\.page//' -e 's/^\.\/\(.*\)$/https:\/\/www\.gwern\.net\/\1/' \
                       | shuf | head -1 | xargs urlencode)
    $X_BROWSER "https://validator.w3.org/nu/?doc=$CHECK_RANDOM"

    # Testing post-sync:
    c() { curl --compressed --silent --output /dev/null --head "$@"; }
    λ(){ cr() { [[ "$2" != $(c --location --write-out '%{url_effective}' "$1") ]] && echo "$1" "$2"; }
         cr 'https://www.gwern.net/dnm-archives' 'https://www.gwern.net/DNM-archives'
         cr 'https://www.gwern.net/docs/dnb/1978-zimmer.pdf' 'https://www.gwern.net/docs/music-distraction/1978-zimmer.pdf'
         cr 'https://www.gwern.net/AB%20testing' 'https://www.gwern.net/AB-testing'
         cr 'https://www.gwern.net/Archiving%20URLs.html' 'https://www.gwern.net/Archiving-URLs'
         cr 'https://www.gwern.net/docs/ai/2019-10-21-gwern-gpt2-folkrnn-samples.ogg' 'https://www.gwern.net/docs/ai/music/2019-10-21-gwern-gpt2-folkrnn-samples.mp3'; }
    wrap λ "Check that some redirects go where they should"
    λ() { cm() { [[ "$1" != $(c --write-out '%{content_type}' "$2") ]] && echo "$1" "$2"; }
          ### check key pages:
          ## check every possible extension:
          ## check some random ones:
          cm "application/epub+zip" 'https://www.gwern.net/docs/eva/2002-takeda-notenkimemoirs.epub'
          cm "application/font-sfnt" 'https://www.gwern.net/static/font/drop-cap/kanzlei/Kanzlei-Initialen-M.ttf'
          cm "application/javascript" 'https://www.gwern.net/docs/statistics/order/beanmachine-multistage/script.js'
          cm "application/javascript" 'https://www.gwern.net/static/js/rewrite.js'
          cm "application/javascript" 'https://www.gwern.net/static/js/sidenotes.js'
          cm "application/json" 'https://www.gwern.net/docs/touhou/2013-c84-downloads.json'
          cm "application/msaccess" 'https://www.gwern.net/docs/touhou/2013-06-08-acircle-tohoarrange.mdb'
          cm "application/msword" 'https://www.gwern.net/docs/iq/2014-tenijenhuis-supplement.doc'
          cm "application/octet-stream" 'https://www.gwern.net/docs/zeo/firmware-v2.6.3R-zeo.img'
          cm "application/pdf" 'https://www.gwern.net/docs/cs/2010-bates.pdf'
          cm "application/pdf" 'https://www.gwern.net/docs/history/1694-gregory.pdf'
          cm "application/vnd.ms-excel" 'https://www.gwern.net/docs/dnb/2012-05-30-kundu-dnbrapm.xls'
          cm "application/vnd.oasis.opendocument.spreadsheet" 'https://www.gwern.net/docs/genetics/heritable/1980-osborne-twinsblackandwhite-appendix.ods'
          cm "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" 'https://www.gwern.net/docs/cs/2010-nordhaus-nordhaus2007twocenturiesofproductivitygrowthincomputing-appendix.xlsx'
          cm "application/vnd.openxmlformats-officedocument.wordprocessingml.document" 'https://www.gwern.net/docs/genetics/heritable/2015-mosing-supplement.docx'
          cm "application/vnd.rn-realmedia" 'https://www.gwern.net/docs/rotten.com/library/bio/crime/serial-killers/elmer-wayne-henley/areyouguilty.rm'
          cm "application/x-maff" 'https://www.gwern.net/docs/eva/2001-pulpmag-hernandez-2.html.maff'
          cm "application/x-shockwave-flash" 'https://www.gwern.net/docs/rotten.com/library/bio/entertainers/comic/patton-oswalt/patton.swf'
          cm "application/x-tar" 'https://www.gwern.net/docs/dnb/2011-zhong.tar'
          cm "application/x-xz" 'https://www.gwern.net/docs/personal/2013-09-25-gwern-googlealertsemails.tar.xz'
          cm "application/zip" 'https://www.gwern.net/docs/statistics/bayes/2014-tenan-supplement.zip'
          cm "audio/mpeg" 'https://www.gwern.net/docs/history/1969-schirra-apollo11flighttothemoon.mp3'
          cm "audio/wav" 'https://www.gwern.net/docs/rotten.com/library/bio/entertainers/comic/david-letterman/letterman_any_sense.wav'
          cm "image/gif" 'https://www.gwern.net/docs/gwern.net-gitstats/arrow-none.gif'
          cm "image/gif" 'https://www.gwern.net/docs/rotten.com/library/religion/creationism/creationism6.GIF'
          cm "image/jpeg" 'https://www.gwern.net/docs/personal/2011-gwern-yourmorals.org/schwartz_process.php_files/schwartz_graph.jpg'
          cm "image/jpeg" 'https://www.gwern.net/docs/rotten.com/library/bio/pornographers/al-goldstein/goldstein-fuck-you.jpeg'
          cm "image/jpeg" 'https://www.gwern.net/docs/rotten.com/library/religion/heresy/circumcellions/circumcellions-augustine.JPG'
          cm "image/png" 'https://www.gwern.net/docs/statistics/order/beanmachine-multistage/beanmachine-demo.png'
          cm "image/png" 'https://www.gwern.net/static/img/logo/logo.png'
          cm "image/svg+xml" 'https://www.gwern.net/images/spacedrepetition/forgetting-curves.svg'
          cm "image/x-icon" 'https://www.gwern.net/static/img/favicon.ico'
          cm "image/x-ms-bmp" 'https://www.gwern.net/docs/rotten.com/library/bio/hackers/robert-morris/morris.bmp'
          cm "image/x-xcf" 'https://www.gwern.net/docs/personal/businesscard-front-draft.xcf'
          cm "message/rfc822" 'https://www.gwern.net/docs/linkrot/2009-08-20-b3ta-fujitsuhtml.mht'
          cm "text/css" 'https://www.gwern.net/docs/gwern.net-gitstats/gitstats.css'
          cm "text/css" 'https://www.gwern.net/docs/statistics/order/beanmachine-multistage/offsets.css'
          cm "text/css" 'https://www.gwern.net/docs/statistics/order/beanmachine-multistage/style.css'
          cm "text/css" 'https://www.gwern.net/static/css/default.css'
          cm "text/css" 'https://www.gwern.net/static/css/fonts.css'
          cm "text/css" 'https://www.gwern.net/static/css/initial.css'
          cm "text/css" 'https://www.gwern.net/static/css/links.css'
          cm "text/csv; charset=utf-8" 'https://www.gwern.net/docs/statistics/2013-google-index.csv'
          cm "text/html" 'https://www.gwern.net/atom.xml'
          cm "text/html; charset=utf-8" 'https://www.gwern.net/docs/cs/2012-terencetao-anonymity.html'
          cm "text/html; charset=utf-8" 'https://www.gwern.net/docs/sr/2013-06-07-premiumdutch-profile.htm'
          cm "text/html; charset=utf-8" 'https://www.gwern.net/index'
          cm "text/markdown; charset=utf-8" 'https://www.gwern.net/2014-spirulina.page'
          cm "text/plain; charset=utf-8" 'https://www.gwern.net/docs/personal/2009-sleep.txt'
          cm "text/plain; charset=utf-8" 'https://www.gwern.net/static/redirects/nginx.conf'
          cm "text/x-adobe-acrobat-drm" 'https://www.gwern.net/docs/dnb/2012-zhong.ebt'
          cm "text/x-haskell; charset=utf-8" 'https://www.gwern.net/static/build/hakyll.hs'
          cm "text/x-opml; charset=utf-8" 'https://www.gwern.net/docs/personal/rss-subscriptions.opml'
          cm "text/x-patch; charset=utf-8" 'https://www.gwern.net/docs/ai/music/2019-12-22-gpt2-preferencelearning-gwern-abcmusic.patch'
          cm "text/x-r; charset=utf-8" 'https://www.gwern.net/static/build/linkAbstract.R'
          cm "text/x-shellscript; charset=utf-8" 'https://www.gwern.net/static/build/linkArchive.sh'
          cm "text/yaml; charset=utf-8" 'https://www.gwern.net/metadata/custom.yaml'
          cm "video/mp4" 'https://www.gwern.net/images/genetics/selection/2019-coop-illinoislongtermselectionexperiment-responsetoselection-animation.mp4'
          cm "video/webm" 'https://www.gwern.net/images/statistics/2003-murray-humanaccomplishment-region-proportions-bootstrap.webm'
        }
    wrap λ "The live MIME types are incorrect"

    ## known-content check:
    λ(){ curl --silent 'https://www.gwern.net/index' | tr -d '­' | fgrep --quiet 'This is the website</span> of <strong>Gwern Branwen</strong>' || echo "Content-check failed"
         curl --silent 'https://www.gwern.net/Zeo'   | tr -d '­' | fgrep --quiet 'lithium orotate' || echo "Content-check failed"; }
    wrap λ "Known-content check of index/Zeo"

    ## did any of the key pages mysteriously vanish from the live version?
    linkchecker --threads=4 --check-extern --recursion-level=1 'https://www.gwern.net/index'
    ## - traffic checks/alerts are done in Google Analytics: alerts on <900 pageviews/daily, <40s average session length/daily.
    ## - latency/downtime checks are done in `updown.io` (every 1h, 1s response-time for /index)
    set +e

    # Cleanup post:
    rm --recursive --force -- ~/wiki/_cache/ ~/wiki/_site/ || true

    # Testing files, post-sync
    λ(){ fdupes --quiet --sameline --size --nohidden $(find ~/wiki/ -type d | egrep -v -e 'static' -e '.git' -e 'gwern/wiki/$' -e 'docs/www/') | fgrep --invert-match -e 'bytes each' -e 'trimfill.png' ; }
    wrap λ "Duplicate file check"

    λ() { find . -perm u=r -path '.git' -prune; }
    wrap λ "Read-only file check" ## check for read-only outside ./.git/ (weird but happened):

    λ(){ fgrep -e 'RealObjects' -e '404 Not Found Error: No Page' -e ' may refer to:' ./metadata/auto.yaml; }
    wrap λ "Broken links, corrupt authors', or links to Wikipedia disambiguation pages in auto.yaml."

    λ(){ find . -type f -name "*--*"; find . -type f -name "*~*"; }
    wrap λ "No files should have double hyphens or tildes in their names."

    λ(){ BROKEN_HTMLS="$(find ./ -type f -name "*.html" | fgrep --invert-match 'static/' | \
                         parallel --max-args=400 "fgrep --ignore-case --files-with-matches \
                         -e '404 Not Found' -e '<title>Sign in - Google Accounts</title'" | sort)"
         for BROKEN_HTML in $BROKEN_HTMLS;
         do grep --before-context=3 "$BROKEN_HTML" metadata/archive.hs | fgrep --invert-match -e 'Right' -e 'Just' ;
         done; }
    wrap λ "Archives of broken links"

    λ(){ BROKEN_PDFS="$(find ./ -type f -name "*.pdf" | sort | parallel file | grep -v 'PDF document' | cut -d ':' -f 1)"
         for BROKEN_PDF in $BROKEN_PDFS; do
             echo "$BROKEN_PDF"; grep --before-context=3 "$BROKEN_PDF" metadata/archive.hs;
         done; }
    wrap λ "Corrupted or broken PDFs"

    λ(){ find ./ -type f -name "*.jpg" | parallel file | fgrep --invert-match 'JPEG image data'; }
    wrap λ "Corrupted JPGs"

    λ(){ find ./ -type f -name "*.png" | parallel file | fgrep --invert-match 'PNG image data'; }
    wrap λ "Corrupted PNGs"

    ## 'file' throws a lot of false negatives on HTML pages, often detecting XML and/or ASCII instead, so we whitelist some:
    λ(){ find ./ -type f -name "*.html" | fgrep --invert-match -e 4a4187fdcd0c848285640ce9842ebdf1bf179369 -e 5fda79427f76747234982154aad027034ddf5309 \
                                                -e f0cab2b23e1929d87f060beee71f339505da5cad -e a9abc8e6fcade0e4c49d531c7d9de11aaea37fe5 \
                                                -e 2015-01-15-outlawmarket-index.html -e ac4f5ed5051405ddbb7deabae2bce48b7f43174c.html \
             | parallel file | fgrep --invert-match -e 'HTML document, ' -e 'ASCII text'; }
    wrap λ "Corrupted HTMLs"

    λ(){ checkEncryption () { ENCRYPTION=$(exiftool -quiet -quiet -Encryption "$@");
                              if [ "$ENCRYPTION" != "" ]; then echo "Encrypted: $@"; fi; }
         export -f checkEncryption
         find ./ -type f -name "*.pdf" | parallel checkEncryption; }
    wrap λ "'Encrypted' PDFs (fix with pdftk: `pdftk $PDF input_pw output foo.pdf`)" &

    ## DjVu is deprecated (due to SEO - no search engines will crawl DjVu, turns out!):
    λ(){ find ./ -type f -name "*.djvu"; }
    wrap λ "DjVu detected (convert to PDF)"

    ## having noindex tags causes conflicts with the robots.txt and throws SEO errors; except in the ./docs/www/ mirrors, where we don't want them to be crawled:
    λ(){ fgrep --files-with-matches 'noindex' $(find ./ -type f -name "*.html" | fgrep --invert-match -e './docs/www/' -e './static/404.html'); }
    wrap λ "Noindex tags detected in HTML pages"

    λ() { find ./ -type f -name "*.gif" | fgrep --invert-match -e 'static/img/' -e 'docs/gwern.net-gitstats/' -e 'docs/rotten.com/' -e 'docs/genetics/selection/www.mountimprobable.com/' | parallel identify | egrep '\.gif\[[0-9]\] '; }
    wrap λ "Animated GIF is deprecated; GIFs should be converted to WebMs/MP4"

    λ() {  find ./ -type f -name "*.jpg" | parallel --max-args=100 "identify -format '%Q %F\n'" {} | sort --numeric-sort | egrep -e '^[7-9][0-9] ' -e '^6[6-9]'; }
    wrap λ "Compress JPGs to <=65% quality"

    ## Find JPGS which are too wide (1600px is an entire screen width on even widee monitors, which is too large for a figure/illustration):
    λ() { for IMAGE in $(find ./images/ -type f -name "*.jpg" -or -name "*.png" | fgrep --invert-match -e 'images/ai/gpt/2020-07-19-oceaninthemiddleofanisland-gpt3-chinesepoetrytranslation.png' -e 'images/gan/2020-05-22-caji9-deviantart-stylegan-ahegao.png' | sort); do
              SIZE_W=$(identify -format "%w" "$IMAGE")
              if (( $SIZE_W > 1600  )); then echo "Too wide image: $IMAGE $SIZE_W"; fi;
          done; }
    wrap λ "Too-wide JPGs (downscale)"

    # if the first of the month, download all pages and check that they have the right MIME type and are not suspiciously small or redirects.
    if [ $(date +"%d") == "1" ]; then

        PAGES=$(cd ~/wiki/ && find . -type f -name "*.page" | sed -e 's/\.\///' -e 's/\.page$//' | sort)
        c() { curl --compressed --silent --output /dev/null --head "$@"; }
        for PAGE in $PAGES; do
            MIME=$(c --max-redirs 0 --write-out '%{content_type}' "https://www.gwern.net/$PAGE")
            if [ "$MIME" != "text/html; charset=utf-8" ]; then echo "$PAGE: $MIME"; exit 2; fi

            SIZE=$(curl --max-redirs 0 --compressed --silent "https://www.gwern.net/$PAGE" | wc --bytes)
            if [ "$SIZE" -lt 7500 ]; then echo "$PAGE : $SIZE : $MIME" && exit 2; fi
        done
    fi

    echo "Sync successful"
else
    echo "Dependencies missing or Hakyll already running?"
fi
