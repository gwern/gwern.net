#!/bin/bash

# Author: Gwern Branwen
# Date: 2016-10-01
# When:  Time-stamp: "2024-05-10 20:40:00 gwern"
# License: CC-0
#
# Bash helper functions for Gwern.net wiki use.
# Intended to be sourced in `~/.bashrc` like `. ~/wiki/static/build/bash.sh`
#
# Helper functions include: website cache invalidation; PDF metadata query & editing; tab-completion for tag/directory names; querying the newsletters, filenames, annotation database, website, and IRC (in increasing generality); site-wide string search-and-replace (including a HTTP→HTTPS special-case which comes up a lot fixing redirects); functions to rename files & directories; and a `gwtag` CLIcommand which create new annotations (if there is a usable annotation source, such as being an Arxiv link) or add/remove tags from specified files/URLs.
#
# See also: /static/build/{upload, gwa, crossref, compressJpg2}

source /usr/share/bash-completion/bash_completion || true # useful for better `upload` tab-completion

# Default parallelism:
export N="7"

set -e

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
ge  () { grep -E "$@"; }
gec  () { ge --color=always "$@"; }
gev () { ge --invert-match "$@"; }
gf  () { grep -F "$@"; }
gfc  () { gf --color=always "$@"; }
gfv () { gf --invert-match "$@"; }
export -f bold red wrap ge gec gev gf gfc gfv

file2Path () { echo "$1" | sed -e 's/~\/wiki//' -e 's/\/home\/gwern\/wiki\//\//' -e 's/\.\//\//' -e 's/https:\/\/gwern\.net//g' -e 's/^/\//' -e 's/^\/\//\//'; }
path2File () {
    # This function takes any number of arguments as input paths or URLs
    # and converts them to a file path under the ~/wiki/ directory.

    # Loop through all arguments passed to the function
    for arg in "$@"
    do
        if [ -f "$arg" ]; then echo "$arg"; else
        # Convert the input to the desired file path using sed
        # The following transformations are applied:
        # 1. Strip 'wiki/' prefix if present
        # 2. Replace '~/' with '/home/gwern/wiki/'
        # 3. Prepend '/home/gwern/wiki/' if the path starts with an alphanumeric character
        # 4. Remove 'https://gwern.net' if present to support URLs from gwern.net
        # 5. Special handling for paths starting with '/doc' to ensure they go into the 'doc' subdirectory correctly
        ARG=$(echo "$arg" | sed -e 's/^wiki\///' \
                                 -e 's/\~\//\/home\/gwern\/wiki\//' \
                                 -e 's/^\([a-zA-Z0-9].*\)/\/home\/gwern\/wiki\/\1/' \
                                 -e 's/https:\/\/gwern\.net//g' \
                                 -e 's/^\/doc/\/home\/gwern\/wiki\/doc/')
        echo "$ARG"
        fi
    done
}
export -f path2File

cloudflare-expire () {
    ARGS=$(path2File "$@")
    for FILE in $(realpath $ARGS); do
        URL="$(echo $FILE | sed -e 's/\.md//' -e 's/\/home\/gwern\/wiki\/\(.*\)/https:\/\/gwern\.net\/\1/g' -e 's/\.md//g' | sort )"
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
    (find ~/wiki/ -name "*.md" -type f -not -wholename "*/\.*/*" -not -wholename "*/_*/*" | sort; find ~/wiki/static/ -type f | sort) | \
        sed -e 's/\.md//' -e 's/^\.\/\(.*\)$/https:\/\/gwern\.net\/\1/' | sort | parallel cloudflare-expire;
    }

pdf () {
    for ARG in "$@"; do
        TARGET=$(echo "$ARG" | sed -e 's/^\/doc\//\/home\/gwern\/wiki\/doc\//')
        (evince "$TARGET" &);
    done; }

# delete the first page of the PDF. This is useful to remove the spam in PDFs from JSTOR and many other academic publishers. (Some of them do 2 or even 3 spam pages, but you can just run `pdfcut` repeatedly quickly with <Up> arrow in bash, of course.)
pdfcut () { if [ $# -ne 1 ]; then echo "Too many arguments" && return 1; fi
            ORIGINAL=$(path2File "$@")
            TARGET=$(mktemp /tmp/XXXXXX.pdf);
            pdftk "$ORIGINAL" cat 2-end  output "$TARGET" &&
            # pdftk by default erases all metadata, so we need to copy it all to the new PDF:
                exiftool -TagsFromFile "$ORIGINAL" "$TARGET" &&
            mv "$TARGET" "$ORIGINAL" || rm "$TARGET";
          (crossref "$ORIGINAL" &);
          }
# sometimes we want to keep the first/cover page, but still don't want to actually make it the *first* page (or work around this with the `#p[age=2` trick; so we can just rotate it to the end rather than deleting it entirely.
pdfcut-append () { if [ $# -ne 1 ]; then echo "Wrong number of arguments arguments; 'pdfcut-append' moves the first page to the end. To delete the first page, use 'pdfcut'." && return 1; fi
            ORIGINAL=$(path2File "$@")
            TARGET=$(mktemp /tmp/XXXXXX.pdf);
            pdftk "$ORIGINAL" cat 2-end 1  output "$TARGET" &&
            # pdftk by default erases all metadata, so we need to copy it all to the new PDF:
                exiftool -TagsFromFile "$ORIGINAL" "$TARGET" &&
            mv "$TARGET" "$ORIGINAL" || rm "$TARGET";
          (crossref "$ORIGINAL" &);
          }

# trim whitespace from around JPG/PNG images
crop_one () { if [[ "$@" =~ .*\.(jpg|png) ]]; then
        nice convert $(path2File "$@") -crop "$(nice -n 19 ionice -c 3 convert "$@" -virtual-pixel edge -blur 0x5 -fuzz 1% -trim -format '%wx%h%O' info:)" +repage "$@"; fi }
crop () { export -f crop_one; ls $(path2File "$@") | parallel crop_one; }
# WARNING: if 'export' isn't inside the function call, it breaks 'atd'! no idea why. may be connected to Shellshock.
export -f crop

alias invert="mogrify -negate"

# report to InvertOrNot.com one image URL that incorrectly inverts/invert-nots for dark-mode: <https://invertornot.com/docs#/default/correction_api_correction_post>
# `$ invert-error-report https://gwern.net/doc/math/2024-zhang-figure1-overfittingofmodelfamiliestogsm8k.png`
invert-error-report () { curl --request 'POST' 'https://invertornot.com/api/correction' \
                              --header 'accept: application/json' --header 'Content-Type: application/json' \
                              --data '["'$1'"]'; }

# add white pixels to an image which has been cropped too tightly to look good:
pad () {
    for FILE in "$@"; do
        mogrify -bordercolor white -border 30 "$(path2File "$FILE")"
    done
}
pad-black () {
    for FILE in "$@"; do
        mogrify -bordercolor black -border 30 "$(path2File "$FILE")"
    done
}
crop-pad () { crop "$@" && pad "$@"; }
crop-pad-black () { crop "$@" && pad-black "$@"; }

# function split_image() {     local image_path="$1";     local base_name=$(basename "$image_path" .png);     local height=$(identify -format "%h" "$image_path");     local half_height=$((height / 2))     convert "$image_path" -crop 100%x50%+0+0 "${base_name}-1.png";     convert "$image_path" -crop 100%x50%+0+$half_height "${base_name}-2.png"; }

# convert black background to white:  `mogrify -fuzz 5% -fill white -draw "color 0,0 floodfill"`

# check a PNG to see if it can be turned into a JPG with minimal quality loss (according to the ImageMagick PSNR perceptual loss); for PNGs that should be JPGs, often the JPG will be a third the size or less, which (particularly for large images like sample-grids) makes for more pleasant web browsing.
# (This does not rename or convert as it is still experimental and automatically renaming gwern.net files is dangerous due to the need for global search-and-replace & defining nginx redirects.)
png2JPGQualityCheck () {
    for ARGS in "$@"; do
        ARG=$(path2File "$ARGS")
        QUALITY_THRESHOLD=31 # decibels
        SIZE_REDUCTION_THRESHOLD=30 # %
        TMP_DIR="${TMPDIR:-/tmp}" # Use TMPDIR if set, otherwise default to /tmp
        # JPG_BASENAME="$(basename "${ARG%.png}.jpg")"
        JPG="$(mktemp "$TMP_DIR/XXXXXX.jpg")"

        # Convert PNG to JPG at 15% quality (to ensure any artifacts show up clearly):
        convert "$ARG" -quality 15% "$JPG"

        # Calculate file sizes
        PNG_SIZE=$(stat -c%s "$ARG")
        JPG_SIZE=$(stat -c%s "$JPG")

        # Calculate size reduction in percentage
        SIZE_REDUCTION=$(echo "scale=2; (1 - $JPG_SIZE / $PNG_SIZE) * 100" | bc)

        # Calculate PSNR
        PSNR=$(compare -metric PSNR "$ARG" "$JPG" null: 2>&1)

        # Check both PSNR quality and size reduction
        if (( $(echo "$PSNR > $QUALITY_THRESHOLD" | bc -l) )) && (( $(echo "$SIZE_REDUCTION >= $SIZE_REDUCTION_THRESHOLD" | bc -l) )); then
            echo "$ARG"
        fi

        # Clean up: Remove the temporary JPG file
        rm "$JPG"
    done
}
export -f png2JPGQualityCheck

# crossref: defined in ~/wiki/static/build/crossref
cr() { crossref "$@" & }

# PDF cleanup: strip encryption, run through `pdftk` to render them standard & strip out weirdness, reformat titles.
e () { FILE=""
       # did I write '$ e -Title="Foobar" 2000-baz.pdf' or '$ e 2000-baz.pdf -Title="Foobar"'?
       if [[ "${1:0:1}" == "-" ]]; then
           FILE="${*: -1}"
       else
           FILE=$(path2File "$1")
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
               TITLE2="$(echo "$TITLE1" | sed -e 's/‐/-/g' -e 's/^ \+//g' -e 's/ \+$//g' -e 's/\.$//' | tr '_' ':')" # WARNING: tr mangles Unicode, but sed doesn't
               if [[ "$TITLE1" != "$TITLE2" ]]; then exiftool -overwrite_original -Title="$TITLE2" "$FILE"; fi

           else emacsclient "$FILE";
           fi;
       else echo "File does not exist? $FILE"
       fi;
     }
alias E="e" # typo
alias ea="exiftool -All"
alias exiftool="exiftool -overwrite_original"

# Gwern.net searches:
## fixed-grep gwern.net specifically:
gw () {
    if [ $# == 0 ]; then echo "Missing search query."; return 2; fi

    QUERY="$*";
    RESULTS=$( (find ~/wiki/ -type f -name "*.md";
         ls ~/.emacs;
         find ~/wiki/metadata/ ~/wiki/haskell/ -name "*.hs" -or -name "*.gtx";
         find ~/wiki/static/ -type f -name "*.js" -or -name "*.css" -or -name "*.hs" -or -name "*.conf" -or -name "*.gtx" -or -name "*.py" -or -name "*.sh";
         find ~/wiki/ -type f -name "*.html" -not -wholename "*/doc/*" ) | \
           grep -F -v -e '.#' -e 'auto.hs' -e doc/link-bibliography/ -e metadata/annotation/ -e _site/ -e _cache/ | sort --unique  | \
           xargs grep -F --color=always --ignore-case --with-filename -- "$QUERY" | cut -c 1-2548);
    if [ -z "$RESULTS" ]; then
        gwl "$@" # fall back to double-checking IRC logs
    else
        echo "$RESULTS"
    fi
}

## file names only
gwf () { (cd ~/wiki/ && find . -type f | grep -F -v -e '.#' -e '_cache/' -e '_site/' -e '.git/' | grep --ignore-case "$@" | sed -e 's/^\.\//\//g') | sort; } # path2File?
## Newsletter only:
gwn () { if [ $# != 1 ]; then QUERY="$*"; else QUERY="$@"; fi
        find ~/wiki/newsletter/ -type f -name "*.md" | \
             grep -F -v -e '.#' |
            sort --unique  | xargs grep -E --ignore-case --color=always --with-filename "$QUERY" | cut --characters 1-2048; }
## Annotations:
# gwa: defined in ~/wiki/static/build/gwa
gwal () { gwa "$@" | less -p "$@"; }

## #lesswrong IRC logs
gwl () { if [ $# != 1 ]; then QUERY="$*"; else QUERY="$@"; fi
         grep -E --context=1 --text --ignore-case -- "$QUERY" ~/doc/irclogs/*/\#lesswrong.log; }

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
## TODO: handle pages/essays, not just files? see </static/build/rename.hs> for a sketch.
gwmv () {

    # shortcut: if there's only 1 PNG argument & it's a known wiki file (ie. version-controlled in git), then that means we are converting it to JPG using `png2JPGQualityCheck`, and can skip manually setting the second argument to the `.jpg` extension.
# (The converse currently never happens & so is unsupported.)
    [[ $# -eq 1 && $1 =~ \.png$ && $(cd ~/wiki/ && git ls-files --error-unmatch "$1" 2>/dev/null) ]] && set -- "$1" "${1%.png}.jpg"

    if [ $# != 2 ]; then
        echo "Need two arguments: OLD file and NEW file! Only got: \"$@\""
        return 2
    else

        set -x
        if [[ ! $(pwd) =~ "/home/gwern/wiki/".* ]]; then cd ~/wiki/ ; fi
        OLD=$(echo "$1" | tr -d '  ⁠' | sed -e 's/https:\/\/gwern\.net//g' -e 's/^\///g' | xargs realpath | sed -e 's/\/home\/gwern\/wiki\//\//g' )
        NEW=$(echo "$2" | tr -d ' ⁠ ' | sed -e 's/https:\/\/gwern\.net//g' -e 's/^\///g' | xargs realpath | sed -e 's/\/home\/gwern\/wiki\//\//g')
        # Check if the parent directory of the NEW path exists
        NEW_DIR=$(dirname "$HOME/wiki$NEW")
        if [ ! -d "$NEW_DIR" ]; then
            echo "Target directory $NEW_DIR does not exist. Operation aborted."
            return 7
        fi

        if [ -d "$HOME/wiki$OLD" ] || [ -d "${OLD:1}" ]; then
            echo "The first argument ($1 $OLD) is a directory. Please use 'gwmvdir' to rename entire directories."
            return 3
        elif [ ! -f "$HOME/wiki$OLD" ] && [ ! -f "${OLD:1}" ]; then
            echo "File $OLD not found in current directory or ~/wiki"
            return 4
        fi

        # strip any slash prefix like '/doc/psychology/2021-foo.pdf' → 'doc/psychology/2021-foo.pdf' to turn it into a local relative link

        # eg "gwMv 'doc/psychology/2021-foo.pdf' 'doc/psychology/writing'" as a shortcut for "gwmv 'doc/psychology/2021-foo.pdf' 'doc/psychology/writing/2021-foo.pdf'"
        if [ -d ~/wiki$NEW ]; then NEW=$(echo $NEW/$(basename $OLD) | sed -e 's/\/\//\//'); fi

        cd ~/wiki/
        if [[ -a ~/wiki$NEW ]]; then
            echo "Moved-to target file $NEW exists! Will not move $OLD and overwrite it. If you deliberately want to overwrite $NEW, then explicitly delete it first."
            return 5
        fi

        # Check if OLD is a PNG and NEW is a JPG for conversion
        if [[ "$OLD" =~ \.png$ && "$NEW" =~ \.jpg$ ]]; then
            # preserve the git history by stashing the converted JPG, doing a `git mv` to tell git about the file renaming, and then overwriting the 'JPG' (actually the original PNG) with an actual JPG
            TMP="$(mktemp /tmp/XXXXX.jpg)"
            convert "$HOME/wiki$OLD" "$TMP"
            compressJPG2 "$TMP"
            feh "$HOME/wiki$OLD" "$TMP" || return 6
            wait
            git mv "$HOME/wiki$OLD" "$HOME/wiki${NEW%.jpg}.jpg"
            mv "$TMP" "$HOME/wiki${NEW%.jpg}.jpg"
        elif [[ -a ~/wiki$OLD ]]; then
            touch ~/wiki"$NEW" && rm ~/wiki"$NEW" && git mv ~/wiki"$OLD" ~/wiki"$NEW" || return 3
        else
            echo "File does not exist? $OLD (to be moved to $NEW)" && return 1;
        fi

        ssh gwern@176.9.41.242 "mkdir -p /home/gwern/gwern.net$(dirname $NEW)"  # TODO: replace this ssh line with the `--mkpath` option in rsync when that becomes available:
        rsync --chmod='a+r' -q ~/wiki"$NEW" gwern@176.9.41.242:"/home/gwern/gwern.net$NEW" || echo "gwmv: rsync failed?" > /dev/null &
        gwsed "$OLD" "$NEW"
        echo '"~^'"$OLD"'.*$" "'"$NEW"'";' | tee --append ~/wiki/static/redirect/nginx.conf # 3. add a redirected old to nginx
        # 4. delete outdated annotations:
        OLD_FILE=$(basename "$OLD"); rm "~/wiki/metadata/annotation/*$OLD_FILE*" || true > /dev/null
        wait

    set +x
    fi
}

## Move a directory:
gwmvdir () {
    cd ~/wiki/
    OLD="$1"
    NEW="$2"
    if [[ "$OLD" == /* ]]; then
        OLD="${OLD:1}"
    fi
    if [[ "$NEW" == /* ]]; then
        NEW="${NEW:1}"
    fi

    OLD="$(readlink --canonicalize "$OLD" | cut -d '/' -f 5-)"
    ls "$OLD"
    NEW="$(readlink --canonicalize "$NEW" | cut -d '/' -f 5-)"
    mkdir -p "$NEW"
    git add "$NEW"
    for FILE in $(ls "$OLD"/* | grep -F -v 'index.md'); do
        echo $FILE
        gwmv "$FILE" "$NEW/$(basename $FILE)"
    done
    rm "$OLD/index.md" || true
    rmdir "$OLD"
}

## 'Move' or tag an annotation:
## gwtag URL [tag]
## eg. `gwtag https://foo iq psychology sociology`
alias gwt="gwtag"
alias t="gwtag"
gwtag () { (
             wait; # just in case another tool might be running (eg. gwtag or gwsed)
             cd ~/wiki/ &&
                     # echo "---" && grep -F -- "$1" ./metadata/*.gtx || true
                     timeout 20m nice ./static/build/changeTag "$@"; echo "" # &&
                         # echo "---" && grep -F -- "$1" ./metadata/*.gtx
         ); }

# eg. `"ai ai/anime ai/anime/danbooru ... ai/scaling ai/scaling/economics ... japan/poetry/shotetsu japan/poetry/teika ... technology/digital-antiquarian ... zeo/short-sleeper"`
GWERNNET_DIRS_FULL="$(cd ~/ && find wiki/doc/ -type d | grep -F -v -e 'doc/rotten.com' -e 'doc/www/' \
                         -e 2000-iapac-norvir -e mountimprobable.com -e personal/2011-gwern-yourmorals.org \
                         -e psychology/european-journal-of-parapsychology -e reinforcement-learning/armstrong-controlproblem \
                         -e statistics/order/beanmachine-multistage -e gwern.net-gitstats -e metadata/annotation | \
                         cut --delimiter='/' --fields=3- | sort)"
# eg. `"1 2 2010-crc 2013-cicadas 3 4 5 abandoned-footnotes ab-testing ... www zeami zeo"`
GWERNNET_DIRS_SHORT="$(echo $GWERNNET_DIRS_FULL | tr '/' '\n' | tr ' ' '\n' | sort --unique)"
# for completing tags which may need to be disambiguated, like 'gpt/nonfiction':
# eg. `"1/lsd 2/2010-crc 3/fiction 3/nonfiction ... palm/2 personality/conscientiousness personality/psychopathy ... video/analysis video/generation vision/dream"`
GWERNNET_DIRS_SUFFIXES="$(echo $GWERNNET_DIRS_FULL | tr ' ' '\n' | grep -E -e '[a-z0-9-]+/[a-z0-9-]+/[a-z0-9-]+' | \
                               rev | cut --delimiter='/' --fields=1-2 | rev | sort --unique)"
complete -W "$GWERNNET_DIRS_FULL $GWERNNET_DIRS_SHORT $GWERNNET_DIRS_SUFFIXES" u gwtag gwt t

alias u="upload"
# 'upload' moved to ~/wiki/static/build/upload for easier calling from XMonad
## tab-complete the first argument as the local file, and the second argument as the remote directory:
_upload() {
    local cur cword
    _init_completion || return ## assumes `bash-completion` package is installed & sourced previously

    if [[ $cword -eq 1 ]]; then
        # File completion on first argument
        _filedir
    elif [[ $cword -eq 2 ]]; then
        # Directory completion on second argument
        COMPREPLY=( $(compgen -W "${GWERNNET_DIRS_FULL} ${GWERNNET_DIRS_SHORT} ${GWERNNET_DIRS_SUFFIXES}" -- "$cur") )
    fi
}
complete -F _upload upload

# wait for a file to become quiescent because eg. the web browser is still downloading it:
is_downloading() {
  file="$1"
  current_time=$(date +%s)

  # Check if the file exists
  if [ -f "$file" ]; then
    modified_time=$(stat -c %Y "$file")
    elapsed_time=$((current_time - modified_time))

    # Sleep if last-modified time is not at least 5 seconds ago
    if [ $elapsed_time -lt 5 ]; then
      sleep $((5 - elapsed_time))
    fi
  else
    echo "File not found."
  fi
}

# shortcut for handling link-archiving review:
# `mvuri`: takes a filename with a URI encoding
# like `file:///home/gwern/wiki/doc/www/www.patterns.app/d7aaf7b7491492af22c98dae1079fbfa93961b5b.html`
# and transform that argument into `/home/gwern/wiki/doc/www/www.patterns.app/d7aaf7b7491492af22c98dae1079fbfa93961b5b.html`
# (ignoring any anchor/hash on the original .html file) and then `mv` the URL snapshot to that like normal.
# If the target starts with 'doc/' (perhaps because it has been copied from `/metadata/archive.hs` while manually fixing)
# It is treated as if it had started with `file:///home/gwern/wiki/doc/`
#
# eg. `$ mvuri file:///home/gwern/wiki/doc/www/www.patterns.app/d7aaf7b7491492af22c98dae1079fbfa93961b5b.html`
# `$ mvuri file:///home/gwern/wiki/doc/www/www.patterns.app/d7aaf7b7491492af22c98dae1079fbfa93961b5b.html#toc`
# `$ mvuri doc/www/www.patterns.app/d7aaf7b7491492af22c98dae1079fbfa93961b5b.html`
# are all equivalent.
mvuri () {
    # Check if inotifywait is installed
    if ! command -v inotifywait &> /dev/null; then
        echo "inotifywait could not be found. Please install 'inotify-tools' package."
        exit
    fi

  local ENCODED_PATH="$1"
  local DECODED_PATH="${ENCODED_PATH//\%/\\x}"
  DECODED_PATH="${DECODED_PATH#file://}"
  DECODED_PATH="${DECODED_PATH%%#*}" # ignore anchors like `foo.html#id` by stripping them

  # Check if DECODED_PATH starts with 'doc/', if so prepend with '/home/gwern/wiki/'
  if [[ $DECODED_PATH == doc/* ]]; then
    DECODED_PATH="/home/gwern/wiki/$DECODED_PATH"
  fi

  local DESTINATION="$DECODED_PATH"

  local SOURCE
  # the browser may not have yet begun writing the file, so make sure we wait until it does:
  while true; do
      # we target the oldest .html file, in case others have started being created in the mean time:
    SOURCE="$(find ~/ -maxdepth 1 -name "*.html" -printf '%T+ %p\n' | sort --key=1,1 | cut --delimiter=' ' --fields=2- | head --lines=1)"
    if [ -n "$SOURCE" ]; then
      break
    else
      inotifywait --quiet --event create ~/
    fi
  done
  # the file may not yet be fully written, so we additionally wait until it's (probably) complete:
  is_downloading "$SOURCE"
  echo "$SOURCE" "$(du -ch "$SOURCE")" "$DESTINATION"
  if [[ $(stat -c%s "$SOURCE") -ge 10000000 ]]; then # EXPERIMENTAL: optimize large HTML files (>10MB) by splitting back into separate files to avoid strict downloads
      mv "$SOURCE" "$DESTINATION"
      php ~/wiki/static/build/deconstruct_singlefile.php "$DESTINATION"
  else
      mv "$SOURCE" "$DESTINATION"
  fi

  echo -n -e '\a'; # ring bell because done
}

# everyNDays: returns a boolean true every _N_ whole-number days in a systematic way based on date modulus; can be used in place of `$RANDOM` calls to do an operation 'randomly' every once in a while, by calling `everyNDays N && ...` or `if everyNDays N; then ...; else; ... fi`.
# (Better than actual randomness because it avoids clumping/starvation, or potentially doing all of the operations on the same run by chance; and far simpler than any explicit tracking of state/date.)
# The optional second whole-number argument is an 'offset' to avoid clumping of multiple calls with the same _N_; they can be offset by relatively-prime numbers or just plain incremented.
everyNDays() {
    (( (($(date +%j) + ${2:-0}) % $1) == 0 )) # optional offset to stagger or space out multiple calls of _N_ relative to each other
}
