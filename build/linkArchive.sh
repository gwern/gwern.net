#!/bin/bash

# linkArchive.sh: archive a URL through SingleFile and link locally
# Author: Gwern Branwen
# Date: 2020-02-07
# When:  Time-stamp: "2023-03-09 16:15:46 gwern"
# License: CC-0
#
# Shell script to archive URLs/PDFs via SingleFile for use with LinkArchive.hs:
# extract the location of the static serialized HTML, and move it to the wiki's `./doc/www/$DOMAIN/$SHA1($URL).html`;
# if the MIME type indicates a PDF, we download & host locally.
# For detailed background on how this is used & the overall design, see LinkArchive.hs & <https://gwern.net/archiving#preemptive-local-archiving>.
#
# Example:
# "https://www.framerated.co.uk/the-haunting-1963/" → /home/gwern/wiki/doc/www/www.framerated.co.uk/31900688e194a1ffa443c2895aaab8f8513370f3.html
#
# $ linkArchive.sh 'http://www.jacurutu.com/viewtopic.php?p=101694'
# /home/gwern/wiki/doc/www/www.jacurutu.com/718b0de585ef3dcd778a196fb2b8c842b42c7bc2.html
# $ linkArchive.sh 'http://www.jacurutu.com/viewtopic.php?p=101694#p101694'
# /home/gwern/wiki/doc/www/www.jacurutu.com/718b0de585ef3dcd778a196fb2b8c842b42c7bc2.html
#
# Requires: sha1sum, SingleFile+chromium, timeout, curl, wget, ocrmypdf; pdftk recommended for 'decrypting' PDFs

# set -e
# set -x

USER_AGENT="Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:110.0) Gecko/20100101 Firefox/110.0"

TARGET=""
## NOTE: anchor-handling is tricky. We need to drop everything after '#' because it's technically not part of the
## filepath/URL - it's only relevant to the web browser.
## A web browser visiting a URL like "https://foo.com/bar.html#1" is visiting the file "bar.html", *not* "bar.html#1".
## But we still need to return it as part of the final rewritten path/URL.
HASH="$(echo -n "$1" | sed -e 's/^\///' | cut -d '#' -f 1 | sha1sum - | cut -d ' ' -f 1)"
DOMAIN="$(echo "$1" | awk -F[/:] '{print $4}')"
if [[ -n $(echo "$1" | grep -F '#') ]]; then
    ANCHOR="#$(echo -n "$1" | cut -d '#' -f 2)"
fi

FILE=$(ls -U "doc/www/$DOMAIN/$HASH."* 2> /dev/null) || true
if [[ -n "$FILE" || "$2" == "--check" ]]; then # use of `--check` means that we always skip archiving and return either the path or its failure, an empty string
    echo -n "$FILE$ANCHOR"
else

    URL=$(echo "$@" | sed -e 's/https:\/\/arxiv\.org/https:\/\/export.arxiv.org/') # NOTE: http://export.arxiv.org/help/robots (we do the rewrite here to keep the directories & URLs as expected like `/doc/www/arxiv.org/`).
    ## 404?
    HTTP_STATUS=$(timeout 20s curl --user-agent "$USER_AGENT" \
                          --head --write-out '%{http_code}' --silent -L -o /dev/null "$URL" || echo "Unsuccessful: $1 $HASH" 1>&2 && exit 1)
    if [[ "$HTTP_STATUS" == "404" ]]; then
        echo "Unsuccessful: $1 $HASH" 1>&2
        exit 1
    else
        # Remote HTML, which might actually be a PDF:
        MIME_REMOTE=$(timeout 20s curl --user-agent "$USER_AGENT" \
                          --head --write-out '%{content_type}' --silent -L -o /dev/null "$URL" || echo "Unsuccessful: $1 $HASH" 1>&2 && exit 1)

        if [[ "$MIME_REMOTE" =~ "application/pdf".* || "$URL" =~ .*'.pdf'.* || "$URL" =~ .*'_pdf'.* || "$URL" =~ '#pdf'.* || "$URL" =~ .*'/pdf/'.* ]];
        then

            timeout --kill-after=700s 700s wget --user-agent="$USER_AGENT" \
                    --quiet --output-file=/dev/null "$1" --output-document=/tmp/"$HASH".pdf
            TARGET=/tmp/"$HASH".pdf
            ## sometimes servers lie or a former PDF URL has linkrotted or changed to a HTML landing page, so we need to check
            ## that we actually got a real PDF file:
            MIME_LOCAL=$(file "$TARGET" | grep -F 'PDF document, version ') || true

            if [[ -f "$TARGET" ]] && [[ -n "$MIME_LOCAL" ]] && [[ ! "$MIME_REMOTE" =~ .*"text/html".* ]] || \
                   [[ "$MIME_REMOTE" =~ "application/pdf".*  || "$MIME_REMOTE" =~ "application/octet-stream".* ]] || \
                   [[ ! "$MIME_LOCAL" == "" ]];
            then
                mkdir --parents "./doc/www/$DOMAIN/"
                ## move the PDF into the Gwern.net repo:
                mv "$TARGET" "./doc/www/$DOMAIN/$HASH.pdf"
                echo -n "doc/www/$DOMAIN/$HASH.pdf$ANCHOR"
                ## use my local custom installation of recent ocrmypdf + JBIG2 encoder to OCR & optimize PDFs I'm hosting:
                (source activate fastai && ocrmypdf --skip-text --optimize 3 --jbig2-lossy "./doc/www/$DOMAIN/$HASH.pdf" "./doc/www/$DOMAIN/$HASH.pdf" &)
                $WWW_BROWSER "./doc/www/$DOMAIN/$HASH.pdf"
            else
                echo "Unsuccessful: $1 $HASH" 1>&2
                exit 1
            fi
            # Handle actual remote HTML file:
        else
            TARGET="/tmp/$HASH.html"
            # https://github.com/gildas-lormeau/SingleFile/blob/master/cli/README.MD (ArchiveBox didn't work out)
            # WARNING: for me single-file emits misleading errors about needing to 'npm install' the browser, but
            # apparently you're supposed to `--browser-executable-path` workaround that, which is documented only in a bug report
            # CURRENT CLI from chrome://version: /home/gwern/snap/chromium/common/chromium/Default
            # REGULAR:                           /home/gwern/snap/chromium/common/chromium/Default
            set -x
            timeout --kill-after=240s 240s \
                    ~/src/SingleFile/cli/single-file --browser-executable-path "$(command -v chromium-browser)" --compress-CSS --remove-scripts false \
                    `# --browser-extensions "$(find ~/snap/chromium/common/chromium/Default/Extensions/* -maxdepth 0 -type d | tr '\n' ',')"` \
                    --browser-args "[\"--profile-directory=Default\", \"--user-data-dir=/home/gwern/snap/chromium/common/chromium/\", \"--load-extension=/home/gwern/snap/chromium/common/chromium/Default/Extensions/cjpalhdlnbpafiamejdnhcphjbkeiagm/1.41.4_1/\", \"--load-extension=/home/gwern/snap/chromium/common/chromium/Default/Extensions/dmghijelimhndkbmpgbldicpogfkceaj/0.4.2_0/\", \"--load-extension=/home/gwern/snap/chromium/common/chromium/Default/Extensions/doojmbjmlfjjnbmnoijecmcbfeoakpjm/11.3.3_0/\", \"--load-extension=/home/gwern/snap/chromium/common/chromium/Default/Extensions/kkdpmhnladdopljabkgpacgpliggeeaf/1.12.2_0/\", \"--load-extension=/home/gwern/snap/chromium/common/chromium/Default/Extensions/mpiodijhokgodhhofbcjdecpffjipkle/1.19.30_0/\", \"--load-extension=/home/gwern/snap/chromium/common/chromium/Default/Extensions/nkbihfbeogaeaoehlefnkodbefgpgknn/10.9.3_0/\", \"--load-extension=/home/gwern/snap/chromium/common/chromium/Default/Extensions/oolchklbojaobiipbmcnlgacfgficiig/1.3.4_0/\", \"--load-extension=/home/gwern/snap/chromium/common/chromium/Default/Extensions/padekgcemlokbadohgkifijomclgjgif/2.5.21_0/\", \"--load-extension=/home/gwern/snap/chromium/common/chromium/Default/Extensions/pioclpoplcdbaefihamjohnefbikjilc/7.19.0_0/\"]" \
                    `# --browser-headless=false` \
                    --user-agent "$USER_AGENT" \
                    --browser-load-max-time "120000" \
                    --load-deferred-images-max-idle-time "10000" \
                    --max-resource-size 50 \
                    --browser-wait-until "networkidle2" \
                    --browser-height "10000" \
                    "$1" "$TARGET" 1>&2
            set +x

            if [[ -f "$TARGET" ]]; then
                ## Check for error pages which nevertheless returned validly:
                ERROR_404=$(grep -F -e '404 Not Found' -e 'Download Limit Exceeded' -e 'Access Denied' "$TARGET")
                if [[ -z "$ERROR_404" ]]; then
                    mkdir --parents "./doc/www/$DOMAIN/"
                    mv "$TARGET" "./doc/www/$DOMAIN/$HASH.html"
                    ## everything successful, so return the filepath of the final result to our caller:
                    echo -n "doc/www/$DOMAIN/$HASH.html$ANCHOR"
                    ## open original vs archived in web browser so the user can check that it preserved OK, or if it needs to be handled manually or domain blacklisted:
                    $WWW_BROWSER "./doc/www/$DOMAIN/$HASH.html$ANCHOR" "$1" &
                else
                    rm "$TARGET"
                    echo "Unsuccessful: $1 $HASH" 1>&2
                    exit 1
                fi
            fi
        fi
    fi
fi
