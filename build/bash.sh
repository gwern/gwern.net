#!/bin/bash

# Author: Gwern Branwen
# Date: 2016-10-01
# When:  Time-stamp: "2023-04-01 09:30:00 gwern"
# License: CC-0
#
# Bash helper functions for Gwern.net wiki use.
# Intended to be sourced in `~/.bashrc` like `. ~/wiki/static/build/bash.sh`
#
# Helper functions include: website cache invalidation; PDF metadata query & editing; tab-completion for tag/directory names; querying the newsletters, filenames, annotation database, website, and IRC (in increasing generality); site-wide string search-and-replace (including a HTTP→HTTPS special-case which comes up a lot fixing redirects); functions to rename files & directories; and a `gwtag` CLIcommand which create new annotations (if there is a usable annotation source, such as being an Arxiv link) or add/remove tags from specified files/URLs.
#
# See also: ~/wiki/static/build/upload, gwa, crossref, compressJpg2

set -e

cloudflare-expire () {
    ARGS=$(echo "$@" | sed -e 's/^wiki\///' -e 's/\~\//\/home\/gwern\/wiki\//' -e 's/^\([a-zA-Z0-9].*\)/\/home\/gwern\/wiki\/\1/' -e 's/https:\/\/gwern\.net//g' -e 's/https:\/\/gwern\.net//g' -e 's/^\/doc/\/home\/gwern\/wiki\/doc/')
    for FILE in $(realpath $ARGS); do
        URL="$(echo $FILE | sed -e 's/\.page//' -e 's/\/home\/gwern\/wiki\/\(.*\)/https:\/\/gwern\.net\/\1/g' -e 's/\.page//g' | sort )"
        echo -n "Expiring: $FILE → $URL : "
        curl --silent --request POST "https://api.cloudflare.com/client/v4/zones/$CLOUDFLARE_TARGET/purge_cache" \
            --header "X-Auth-Email:gwern@gwern.net" \
            --header "Authorization: Bearer $CLOUDFLARE_CACHE_TOKEN" \
            --header "Content-Type: application/json" \
            --data "{\"files\":[\"$URL\"]}" | jq '.success'
        (curl --silent "$URL" &) > /dev/null
    done
}
export -f cloudflare-expire
cloudflare-expire-all () {
    (find ~/wiki/ -name "*.page" -type f -not -wholename "*/\.*/*" -not -wholename "*/_*/*" | sort; find ~/wiki/static/ -type f | sort) | \
        sed -e 's/\.page//' -e 's/^\.\/\(.*\)$/https:\/\/gwern\.net\/\1/' | sort | parallel cloudflare-expire;
    }

pdf () {
    for ARG in "$@"; do
        TARGET=$(echo "$ARG" | sed -e 's/^\/doc\//\/home\/gwern\/wiki\/doc\//')
        (evince "$TARGET" &);
    done; }

# delete the first page of the PDF. This is useful to remove the spam in PDFs from JSTOR and many other academic publishers. (Some of them do 2 or even 3 spam pages, but you can just run `pdfcut` repeatedly quickly with <Up> arrow in bash, of course.)
pdfcut () { if [ $# -ne 1 ]; then echo "Too many arguments" && return 1; fi
            ORIGINAL=$(echo "$@" | sed -e 's/^\/doc\//\/home\/gwern\/wiki\/doc\//')
            TARGET=$(mktemp /tmp/XXXXXX.pdf);
            pdftk "$ORIGINAL" cat 2-end  output "$TARGET" &&
            # pdftk by default erases all metadata, so we need to copy it all to the new PDF:
                exiftool -TagsFromFile "$ORIGINAL" "$TARGET" &&
            mv "$TARGET" "$ORIGINAL" || rm "$TARGET";
          (crossref "$ORIGINAL" &);
          }

# crossref: defined in ~/wiki/static/build/crossref
alias cr="crossref"

# PDF cleanup: strip encryption, run through `pdftk` to render them standard & strip out weirdness, reformat titles.
e () { FILE=""
       # did I write '$ e -Title="Foobar" 2000-baz.pdf' or '$ e 2000-baz.pdf -Title="Foobar"'?
       if [[ "${1:0:1}" == "-" ]]; then
           FILE="${*: -1}"
       else
           FILE=$(echo "$1" | sed -e 's/^\/doc/\/home\/gwern\/wiki\/doc/')
           shift;
       fi
       if [[ -a "$FILE" ]]; then
           if [[ "$FILE" =~ .*\.pdf || "$FILE" =~ .*\.jpg || "$FILE" =~ .*\.png ]]; then
               if [[ "$FILE" =~ .*\.pdf ]]; then
                   ENCRYPTION=$(exiftool -q -q -Encryption "$FILE")
                   if [ "$ENCRYPTION" != "" ]; then
                       pdftk "$FILE" input_pw output foo.pdf && mv foo.pdf "$FILE";
                   fi;
                   # Sometimes PDFs are really weird and exiftool will claim to successfully write metadata to them
                   # and yet nothing will happen. I've found that if you pass them through pdftk first, they seem almost identical
                   # but the mysterious unwritable behavior stops. So we convert any PDF by default.
                   TMP="$(mktemp /tmp/XXXXX.pdf)"
                   # pdftk by default erases all metadata, so we need to copy it all to the new PDF:
                   pdftk "$FILE" cat output "$TMP" && \
                       exiftool -TagsFromFile "$FILE" "$TMP" && \
                         mv "$TMP" "$FILE"
               fi;
               echo "$FILE"
               exiftool -m -overwrite_original "$FILE" "$@";
               # getting very tired of these hyphen junk in my titles...
               TITLE1="$(exiftool -printFormat '$Title' -Title "$FILE")"
               TITLE2="$(echo "$TITLE1" | sed -e 's/‐/-/g' -e 's/^ +//g' -e 's/ +$//g' -e 's/\.$//' | tr '_' ':')" # WARNING: tr mangles Unicode, but sed doesn't
               if [[ "$TITLE1" != "$TITLE2" ]]; then exiftool -overwrite_original -Title="$TITLE2" "$FILE"; fi

           else emacsclient "$FILE";
           fi;
       else echo "File does not exist? $FILE"
       fi;
     }
alias E="e"
alias ea="exiftool -All"
alias exiftool="exiftool -overwrite_original"

# Gwern.net searches:
## grep gwern.net specifically:
gw () {
    if [ $# == 0 ]; then echo "Missing search query."; return 2; fi

    QUERY="$*";
    RESULTS=$( (find ~/wiki/ -type f -name "*.page";
         ls ~/.emacs;
         find ~/wiki/metadata/ ~/wiki/haskell/ -name "*.hs" -or -name "*.yaml";
         find ~/wiki/static/ -type f -name "*.js" -or -name "*.css" -or -name "*.hs" -or -name "*.conf" -or -name "*.yaml" -or -name "*.py" -or -name "*.sh";
         find ~/wiki/ -type f -name "*.html" -not -wholename "*/doc/*" ) | \
           grep -F -v -e '.#' -e 'auto.hs' -e doc/link-bibliography/ -e metadata/annotation/ | sort --unique  | \
           xargs grep -F --color=always --ignore-case --with-filename "$QUERY" | cut -c 1-2548);
    if [ -z "$RESULTS" ]; then
        gwl "$@" # fall back to double-checking IRC logs
    else
        echo "$RESULTS"
    fi
}

## file names only
gwf () { (cd ~/wiki/ && find . -type f | grep -F -v -e '.#' | grep --ignore-case "$@" | sed -e 's/^\.\//\//g') | sort; }
## Newsletter only:
gwn () { if [ $# != 1 ]; then QUERY="$*"; else QUERY="$@"; fi
        find ~/wiki/newsletter/ -type f -name "*.page" | \
             grep -F -v -e '.#' |
            sort --unique  | xargs grep -E --ignore-case --color=always --with-filename "$QUERY" | cut --characters 1-2048; }
## Annotations:
# gwa: defined in ~/wiki/static/build/gwa
gwal () { gwa "$@" | less -p "$@"; }

## #lesswrong IRC logs
gwl () { if [ $# != 1 ]; then QUERY="$*"; else QUERY="$@"; fi
         grep --context=1 --text --ignore-case -- "$QUERY" ~/doc/irclogs/*/\#lesswrong.log; }

# Gwern.net modifications:
## gwsed shortcut for the common use case of updating a domain HTTP → HTTPS; typically the URLs are otherwise unchanged, and don't need to be individually updated.
gwhttp () {
    if [ $# != 1 ]; then
        echo "HTTP migration only works for 1 domain, did you try a regular search-and-replace?"
        return 2
    else
        # extract top-level domain to do the search-and-replace on that:
        HTTP="http://$(echo "$1" | sed -e 's/[^/]*\/\/\([^@]*@\)\?\([^:/]*\).*/\2/')"
        HTTPS=${HTTP//http:/https:}
        gwsed "$HTTP" "$HTTPS"
    fi
}
export -f gwhttp

## Move a file and sync it and update all callers:
gwmv () {
    if [ $# != 2 ]; then
        echo "Need two arguments: OLD file and NEW file! Only got: \"$@\""
        return 2
    else
    (
        set -x
        if [[ ! $(pwd) =~ "/home/gwern/wiki/".* ]]; then cd ~/wiki/ ; fi
        OLD=$(echo "$1" | sed -e 's/https:\/\/gwern\.net//g' -e 's/^\///g' | xargs realpath | sed -e 's/\/home\/gwern\/wiki\//\//g' )
        NEW=$(echo "$2" | sed -e 's/https:\/\/gwern\.net//g' -e 's/^\///g' | xargs realpath | sed -e 's/\/home\/gwern\/wiki\//\//g')


        if [ -d "$HOME/wiki$OLD" ] || [ -d "${OLD:1}" ]; then
            echo "The first argument ($1 $OLD) is a directory. Please use 'gwmvdir' to rename entire directories."
            return 3
        elif [ ! -f "$HOME/wiki$OLD" ] && [ ! -f "${OLD:1}" ]; then
            echo "File $OLD not found in current directory or ~/wiki"
            return 3
        fi

        # strip any slash prefix like '/doc/psychology/2021-foo.pdf' → 'doc/psychology/2021-foo.pdf' to turn it into a local relative link

        # eg "gwMv 'doc/psychology/2021-foo.pdf' 'doc/psychology/writing'" as a shortcut for "gwmv 'doc/psychology/2021-foo.pdf' 'doc/psychology/writing/2021-foo.pdf'"
        if [ -d ~/wiki$NEW ]; then NEW=$(echo $NEW/$(basename $OLD) | sed -e 's/\/\//\//'); fi


        cd ~/wiki/
        # to rename a gwern.net file:
        # 1. git mv the old filename to new
        if [[ -a ~/wiki$OLD ]]; then
            touch ~/wiki"$NEW" && rm ~/wiki"$NEW" && git mv ~/wiki"$OLD" ~/wiki"$NEW" || return 3
            rsync --mkpath --chmod='a+r' -q ~/wiki"$NEW" gwern@176.9.41.242:"/home/gwern/gwern.net$NEW" || true > /dev/null &
            # 2. gwsed old new
            gwsed "$OLD" "$NEW"
            # 3. add a redirected old to nw
            echo '"~^'"$OLD"'.*$" "'"$NEW"'";' | tee --append ~/wiki/static/redirect/nginx.conf
            # 4. delete outdated annotations:
            OLD_FILE=$(basename "$OLD")
            rm ~/wiki/metadata/annotation/*"$OLD_FILE"* || true > /dev/null
        else
                echo "File does not exist? $OLD (to be moved to $NEW)" && return 1;
        fi
        )
        set +x
    fi
}
## Move a directory:
gwmvdir () {
    cd ~/wiki/
    OLD="$(readlink --canonicalize "$1" | cut -d '/' -f 5-)"
    ls "$OLD"
    NEW="$(readlink --canonicalize "$2" | cut -d '/' -f 5-)"
    mkdir -p "$NEW"
    git add "$NEW"
    for FILE in $(ls "$OLD"/* | grep -F -v 'index.page'); do
        echo $FILE
        gwmv "$FILE" "$NEW/$(basename $FILE)"
    done
    rm "$OLD/index.page" || true
    rmdir "$OLD"
}

## 'Move' or tag an annotation:
## gwtag URL [tag]
## eg 'gwtag https://foo iq psychology sociology'
alias gwt="gwtag"
gwtag () { (
             cd ~/wiki/ &&
                     # echo "---" && grep -F -- "$1" ./metadata/*.yaml || true
                     timeout 12m runghc -istatic/build/ ./static/build/changeTag.hs "$@"; echo "" # &&
                         # echo "---" && grep -F -- "$1" ./metadata/*.yaml
         ); }

GWERNNET_DIRS_FULL="$(cd ~/ && find wiki/doc/ -type d | grep -F -v -e 'doc/rotten.com' -e 'doc/www/' -e 2000-iapac-norvir -e mountimprobable.com -e personal/2011-gwern-yourmorals.org -e psychology/european-journal-of-parapsychology -e reinforcement-learning/armstrong-controlproblem -e statistics/order/beanmachine-multistage -e gwern.net-gitstats -e link-bibliography | cut -d '/' -f 3- | sort)"
GWERNNET_DIRS_SHORT="$(echo $GWERNNET_DIRS_FULL | tr '/' '\n' | sort -u)"
GWERNNET_DIRS_SUFFIXES="$(echo $GWERNNET_DIRS_FULL | tr ' ' '\n' | egrep -e '[a-z0-9-]\+/[a-z0-9-]\+/[a-z0-9-]\+' | rev | cut --delimiter='/' --fields=1-2 | rev)" # for completing tags which may need to be disambiguated, like 'gpt/nonfiction'
complete -W "$GWERNNET_DIRS_FULL $GWERNNET_DIRS_SHORT $GWERNNET_DIRS_SUFFIXES" -f upload
complete -W "$GWERNNET_DIRS_FULL $GWERNNET_DIRS_SHORT $GWERNNET_DIRS_SUFFIXES" u
complete -W "$GWERNNET_DIRS_FULL $GWERNNET_DIRS_SHORT $GWERNNET_DIRS_SUFFIXES" gwtag
complete -W "$GWERNNET_DIRS_FULL $GWERNNET_DIRS_SHORT $GWERNNET_DIRS_SUFFIXES" gwt
alias u="upload"
# 'upload' moved to ~/wiki/static/build/upload for easier calling from XMonad

# GPT-3-written:
# shortcut for handling link-archiving review:
# Bash shell function named `mvuri` which will take a filename with a URI encoding like `file:///home/gwern/wiki/doc/www/www.patterns.app/d7aaf7b7491492af22c98dae1079fbfa93961b5b.html` and transform that argument into `/home/gwern/wiki/doc/www/www.patterns.app/d7aaf7b7491492af22c98dae1079fbfa93961b5b.html` and then `mv` the URL snapshot to that like normal.
# eg  `$ mvuri file:///home/gwern/wiki/doc/www/www.patterns.app/d7aaf7b7491492af22c98dae1079fbfa93961b5b.html`
mvuri () {
  local ENCODED_PATH="$1"
  local DECODED_PATH="${ENCODED_PATH//\%/\\x}"
  DECODED_PATH="${DECODED_PATH#file://}"
  local DESTINATION="$DECODED_PATH"

  local SOURCE
  SOURCE="$(find ~/ -maxdepth 1 -name "*.html" -print -quit)"
  echo "$SOURCE" "$DESTINATION"
  mv "$SOURCE" "$DESTINATION"
}

# downscale images for mobile viewports; the smallest 530px.jpg version is always 530px, the larger one is either JPG or PNG depending on original file type/. This replicates `imageSrcset` in Image.hs
function resize_image() {
  if [[ $# -ne 1 ]]; then
    echo "Usage: resize_image <image>"
    return 1
  fi

  IMAGE="$1"
  EXT="${IMAGE##*.}"
  BASE="${IMAGE%.*}"

  if ! [[ -f "${BASE}.${EXT}-530px.jpg" ]]; then
    echo "Creating ${BASE}.${EXT}-530px.jpg …"
    convert "${IMAGE}" -resize 530x530 "${BASE}.${EXT}-530px.jpg"
    compressJPG2 "${BASE}.${EXT}-530px.jpg"
  fi

  if ! [[ -f "${BASE}.${EXT}-768px.${EXT}" ]]; then
    echo "Creating ${BASE}.${EXT}-768px.${EXT} …"
    convert "${IMAGE}" -resize 768x768 "${BASE}.${EXT}-768px.${EXT}"
    png "${BASE}.${EXT}-768px.${EXT}"
  fi
}
