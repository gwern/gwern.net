#!/bin/bash

# upload: convenience script for uploading PDFs, images, and other files to gwern.net. Handles naming & reformatting.
# Author: Gwern Branwen
# Date: 2021-01-01
# When:  Time-stamp: "2025-03-05 19:30:13 gwern"
# License: CC-0
#
# Upload files to Gwern.net conveniently, either temporary working files or permanent additions.
# Usage:
# $ upload file # temporary documents are uploaded to /doc/www/misc/$file, and deleted after 90 days.
# $ upload file directory-name # uploaded to /doc/$directory-name/$file if that is unique, otherwise the tag rewrite system guesses
# $ upload 1994-benter.pdf statistics/decision # uploads to `/doc/statistics/decision/1994-benter.pdf`
# $ upload benter1994.pdf decision # renames to `1994-benter.pdf` and uploads to `/doc/statistics/decision/1994-benter.pdf`
#
# Files receive standard optimization, reformatting, compression, metadata-scrubbing etc.
# This will rename to be globally-unique (and verify pre-existing extension usage), reformat, run PDFs through `ocrmypdf`
# (via the `compressPdf` wrapper, to JBIG2-compress, OCR, and convert to PDF/A), and `git add` new files (or if too large to be safe to version-control, added to `.gitignore` instead).
# They are then opened in a web browser to verify they uploaded, have permissions, and render.

. ~/wiki/static/build/bash.sh

set -e

if [ ! -f "$1" ] || [ ! -s "$1" ]; then red "l25: '$1' is not a file or is empty‽" && exit 1; fi

# the fundamental function which does all the real work. Jump to the bottom for the actual argument-handling loop of `upload`.
_upload() {
  wait

  (locate "$1" &)

  FILENAME="$1"
  ## Check whether there are any files with the same extension as the upload candidate; if not, it is likely erroneous in some way and we bail out to the user:
  EXT="${FILENAME##*.}"
  ext_lower=$(echo "$EXT" | tr '[:upper:]' '[:lower:]')
  ALLOWED_EXTENSIONS=$(find ~/wiki/ -type f -printf '%f\n' \
                        | awk --field-separator '.' 'NF>1 {print $NF}' \
                        | sort --ignore-case --unique | tr '[:upper:]' '[:lower:]')

  if ! echo "$ALLOWED_EXTENSIONS" | grep --word-regexp --quiet "$ext_lower"; then
    red "Error: Unsupported file extension '.$EXT' in file '$FILENAME'? This extension has never been used before. Please manually add it, to verify that this is a new but valid extension."
    exit 1
  fi
  # we don't want to try to compile random Markdown snippets, so rename to `.txt` which will be treated as a static asset:
  if [[ $FILENAME == *.md ]]; then
    NEW_FILENAME="${FILENAME%.md}.txt"
    mv "$1" "$NEW_FILENAME"
    echo "Renamed: $FILENAME to $NEW_FILENAME"
  fi
  if [[ $FILENAME == *.jpeg ]]; then
    FILENAME="${FILENAME%.jpeg}.jpg"
    mv "$1" "$FILENAME"
    # we avoid WebP as still too exotic; a WebP could be converted to PNG or JPG, depending on what it encoded, but since we check elsewhere for PNGs that should be JPG, we can just default to converting it to PNG to be safe:
  elif [[ $FILENAME == *.webp ]]; then
    PNG_FILENAME="${FILENAME%.webp}.png"
    if convert "$FILENAME" "$PNG_FILENAME"; then
      FILENAME="$PNG_FILENAME"
      rm "$1"  # successful, so remove the original WebP file
      bold "Converted WebP to PNG: $PNG_FILENAME"
    else
      red "Failed to convert WebP to PNG. Proceeding with original WebP file."
    fi
  fi

  # Attempt to make filename globally unique, due to repetition of surnames.
  #
  # eg. I go to do `upload 2023-liu-2.pdf economics`, and it turns out `/doc/psychology/2023-liu-2.pdf` already exists...
  # as do `/doc/biology/2023-liu-3.pdf` and `/doc/technology/2023-liu-4.pdf`. (Liu is an *extremely* common Asian surname.)
  # So this function will try to loop over numeric suffixes 1–9 to rename it to the first workable filename, in this case, `2023-liu-5.pdf`.
  function rename_file() {
    local filename="$1"
    local base_name extension new_filename new_file_path

    base_name="${filename%.*}"
    extension="${filename##*.}"

    new_file_path=$(find ~/wiki/ -type f -name "$(basename $filename)" -print -quit)

    # if filename already exists, try to rename it
    if [[ -n "$new_file_path" ]]; then
      for ((i=2; i<=20; i++)); do
        new_filename="${base_name}-${i}.${extension}"
        # avoid spurious collisions with temporary/working files in the infrastructure repo or the scratch directory:
        new_file_path=$(find ~/wiki/ -type f ! -path "~/wiki/static/*" ! -path "~/wiki/doc/www/*" -name "$new_filename" -print -quit)

        if [[ -z "$new_file_path" ]]; then
          mv "$filename" "$new_filename"
          bold "File '$filename' has been renamed to '$new_filename'"
          filename="$new_filename"
          break
        fi
      done
    fi

    # if filename after possible renaming does not exist, that means we're using a new filename
    if [[ ! -e "$filename" ]]; then
      red "Error: File '$filename' could not be renamed. Please check for possible issues." >&2
      return 1
    fi

    FILENAME="$filename"
    return 0
  }
  rename_file "$FILENAME"

  BROWSER=""
  if [ -n "$(pgrep firefox)" ]; then BROWSER="firefox"; else BROWSER="chromium"; fi

  if [[ $# -eq 1 || "$2" == "" ]]; then
      # convenience function: timestamps are useful for files, but it's annoying to manually add the date. We can't assume that a regular file was created 'today' because it is usually a historical paper or something, but temporary files are almost always just-created, and even if not, it's useful to know *when* it was uploaded.
      if ! [[ "$FILENAME" =~ ^20[2-4][0-9]-[0-9][0-9]-[0-9][0-9] ]]; then
          DIRNAME=$(dirname "$FILENAME")  # Extract the directory path
          BASENAME=$(basename "$FILENAME")  # Extract the filename
          TIMESTAMPED="$(date '+%F')-$BASENAME"  # Prefix the filename with the timestamp
          if [ "$DIRNAME" = "." ]; then
              # If the file is in the current directory, DIRNAME will be '.'
              mv "$FILENAME" "$TIMESTAMPED"
              FILENAME="$TIMESTAMPED"
          else
              # Reconstruct the full path with the timestamped filename
              mv "$FILENAME" "$DIRNAME/$TIMESTAMPED"
              FILENAME="$DIRNAME/$TIMESTAMPED"
          fi
      fi
      TARGET=$(basename "$FILENAME")
      if [[ "$TARGET" =~ .*\.jpg || "$TARGET" =~ .*\.png ]]; then exiftool -overwrite_original -All="" "$TARGET"; fi # strip potentially dangerous metadata from scrap images
      # format Markdown/text files for more readability
      TEMPFILE=$(mktemp /tmp/text.XXXXX)
      if [[ "$TARGET" =~ .*\.md || "$TARGET" =~ .*\.txt ]]; then fold --spaces --width=80 "$TARGET" >> "$TEMPFILE" && mv "$TEMPFILE" "$TARGET"; fi

      mv "$TARGET" ~/wiki/doc/www/misc/
      cd ~/wiki/ || exit
      TARGET2="./doc/www/misc/$TARGET"
      rsync --chmod='a+r' -q "$TARGET2" gwern@176.9.41.242:"/home/gwern/gwern.net/doc/www/misc/" || \
          rsync --chmod='a+r' -v "$TARGET2" gwern@176.9.41.242:"/home/gwern/gwern.net/doc/www/misc/"
      URL="https://gwern.net/doc/www/misc/$TARGET"
      echo "$URL" && "$BROWSER" "$URL" 2> /dev/null &
  else
      TARGET_DIR=""
      TARGET_DIR=doc/"$2"

      if [ ! -d ~/wiki/"$TARGET_DIR"  ]; then
          # try to guess a target:
          GUESS=$(cd ~/wiki/ && ./static/build/guessTag "$2")
          if [ ! -d ~/wiki/doc/"$GUESS"/ ]; then
              # the guess failed too, so bail out entirely:
              ls ~/wiki/"$TARGET_DIR" ~/wiki/doc/"$GUESS"/
              red "$FILENAME; Directory $TARGET_DIR $2 (and fallback guess $GUESS) does not exist?"
              return 2
          else
              # restart with fixed directory
              bold "Retrying as \"upload $FILENAME $GUESS\"…"
              upload "$FILENAME" "$GUESS"
          fi
      else
          if [ -a "$FILENAME" ]; then
              ## automatically rename a file like 'benter1994.pdf' (Libgen) to '1994-benter.pdf' (gwern.net):
              FILE="$FILENAME"
              if [[ "$FILE" =~ ([a-zA-Z]+)([0-9][0-9][0-9][0-9])\.pdf ]];
              then
                  SWAP="${BASH_REMATCH[2]}-${BASH_REMATCH[1]}.pdf"
                  SWAP=$(echo "$SWAP" | tr 'A-Z' 'a-z') ## eg '1979-Svorny.pdf' → '1979-svorny.pdf'

                  mv "$FILE" "$SWAP"
                  FILE="$SWAP"
              fi
              TARGET=$TARGET_DIR/$(basename "$FILE")
              if [ ! -e ~/wiki/"$TARGET" ]; then
                  mv "$FILE" ~/wiki/"$TARGET"
                  cd ~/wiki/ || return 3
                  chmod a+r "$TARGET"
                  if [[ "$TARGET" =~ .*\.pdf ]]; then
                      METADATA=$(crossref "$TARGET") && echo "$METADATA" & # background for speed, but print it out mostly-atomically to avoid being mangled & impeding copy-paste of the annotation metadata
                      compressPdf "$TARGET" || true; # sometimes PDFs error out in `ocrmypdf` and yield a size of 0, so ignore errors
                      chmod a+r "$TARGET";
                  fi
                  # Large file strategy: naively checking in large files like multi-gigabyte `.pkl` files is a recipe for degrading the git repo. However, we do not *really* need to track their histories (as they are usually WORM/archival files), we have plenty of disk space & bandwidth on the dedicated server, and we want to otherwise treat large files identically to smaller ones in terms of hosting on Gwern.net, including in tag-directories, file icons, and so on. So, using git-annex or git-lfs is overkill and doesn't offer any functionality we need. Instead, we simply make heavier use of `.gitignore`: large files are added to the appropriate directory in `/docs/`, and then simply ignored by git. This is toil if we do it by hand, but we add files using `upload`, so we can simply automate the addition of a file-specific ignore line (if that is necessary).
                  # Check file size and add to .gitignore if large.
                  FILESIZE=$(stat -c%s "$TARGET")
                  if [ "$FILESIZE" -gt 262144000 ]; then  # 250MB = 262144000 bytes; TODO: maybe lower this to 100MB to mirror Github's longstanding (and much copied) blob-filesize limit?
                      if ! git check-ignore --quiet "$TARGET"; then # check if ignored by previous rules, such as filetype-level ignores
                          echo "$TARGET" >> ./.gitignore
                      else
                          # We will document every file ignored this way, to look at usage later & make it easy to rollback or switch to an alternative:
                          echo "# Large-file ignored: \"$TARGET\"" >> ./.gitignore
                      fi
                      bold "Added large file $TARGET to .gitignore (size: $(numfmt --to=iec-i --suffix=B $FILESIZE))"
                  else
                      (git add "$TARGET" &)
                  fi
                  (rsync --chmod='a+r' --mkpath -q "$TARGET" gwern@176.9.41.242:"/home/gwern/gwern.net/$TARGET_DIR/" || \
                      rsync --chmod='a+r' --mkpath -v "$TARGET" gwern@176.9.41.242:"/home/gwern/gwern.net/$TARGET_DIR/"
                  URL="https://gwern.net/$TARGET_DIR/$(basename "$FILE")"
                  cloudflare-expire "$TARGET_DIR/$(basename "$FILE")" # expire any possible 404s from previous failure or similar cache staleness
                  curl --head "$URL" > /dev/null # verify it's downloadable
                  echo ""
                  echo "/$TARGET $URL"

                  if [[ "$TARGET" =~ .*\.png ]]; then png2JPGQualityCheck ~/wiki/"$TARGET"; fi

                  "$BROWSER" "$URL" 2> /dev/null) &

              else red "Error: ~/wiki/$TARGET already exists at this exact path & filename! Will not try to automatically rename & upload, as this may be a duplicate: the user must check & rename manually to override."
                   echo
                   crossref "$TARGET"
                   return 4
              fi
          else red "First argument $FILENAME is not a file?"
               return 1
          fi
      fi
  fi
}

# `upload` main loop, calling `upload` as appropriate:
## If last argument is not a file, it's a directory, and we call `_upload` repeatedly with `_upload $file_n $directory`.
## This keeps the logic simpler than trying to handle many variable-length arguments in `_upload`.
if [[ ! -f "${!#}" ]]; then
    dir="${!#}"
    files=("${@:1:$(($#-1))}")
else
    files=("$@")
fi

for file in "${files[@]}"; do
    if [[ -n "$dir" ]]; then
        (_upload "$file" "$dir")
    else
        (_upload "$file")
    fi
done

wait
