#!/usr/bin/env bash

# Author: Gwern Branwen
# Date: 2016-10-01
# When:  Time-stamp: "2026-04-11 00:22:16 gwern"
# License: CC-0
#
# Bash helper functions for Gwern.net wiki use.
# Intended to be sourced in `~/.bashrc` like `. ~/wiki/static/build/bash.sh`
#
# Helper functions include: website cache invalidation; PDF metadata query & editing; tab-completion for tag/directory names; querying the newsletters, filenames, annotation database, website, and IRC (in increasing generality); site-wide string search-and-replace (including a HTTP→HTTPS special-case which comes up a lot fixing redirects); functions to rename files & directories; and a `gwtag` CLIcommand which create new annotations (if there is a usable annotation source, such as being an Arxiv link) or add/remove tags from specified files/URLs.
#
# For a detailed style guide & documentation of our Bash code, see <https://gwern.net/style-guide#bash>.
#
# See also: </static/build/{upload, gwa, crossref, compressJpg2}>

source /usr/share/bash-completion/bash_completion || true # useful for better `upload` tab-completion

length () { awk '{ print length, $0 }' | sort --general-numeric-sort | \
                awk '{$1=""; print $0}' | sed -e 's/^ //'; }

# Default Haskell parallelism:
export N="14"

set -e

bold () { echo -e "\033[1m$@\033[0m"; }
red  () { echo -e "\e[41m$@\e[0m"; }
# Green text (uses foreground color for success messages)
green () {
  # \e[32m sets foreground to green
  # \e[0m resets all attributes
  echo -e "\e[32m$@\e[0m"
}
# Yellow text (uses foreground color for warnings)
yellow () {
  # \e[33m sets foreground to yellow
  # \e[0m resets all attributes
  echo -e "\e[33m$@\e[0m"
}
## function to wrap checks and print red-highlighted warning if non-zero output (self-documenting):
wrap () { local OUTPUT=$($1 2>&1)
          local WARN="$2"
          if [ -n "$OUTPUT" ]; then
              echo -n "Begin: "; red "$WARN";
              echo -e "$OUTPUT";
              echo -n "End: "; red "$WARN";
          fi; }
ge  () { grep --extended-regexp "$@"; }
gec  () { ge --color=always "$@"; }
gev () { ge --invert-match "$@"; }
gf  () { grep --fixed-strings "$@"; }
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
                                 -e 's/#.*$//g' \
                                 -e 's/^\/doc/\/home\/gwern\/wiki\/doc/'\
           -e 's/\/\//\//g') # clean up any double-slashes
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

# delete the first page of the PDF. This is useful to remove the spam in PDFs from JSTOR and many other academic publishers. (Some of them do 2 or even 3 spam pages, but you can just run `pdf-cut` repeatedly quickly with <Up> arrow in bash, of course.)
alias pdfcut="pdf-cut"
alias pdfcut-last="pdf-cut-last"
pdf-cut () { for PDF in "$@"; do
                ORIGINAL=$(path2File "$PDF")
                TARGET=$(mktemp /tmp/XXXXXX.pdf);
                pdftk "$ORIGINAL" cat 2-end  output "$TARGET" &&
                    # pdftk by default erases all metadata, so we need to copy it all to the new PDF:
                    exiftool -TagsFromFile "$ORIGINAL" -- "$TARGET" &&
                    mv -- "$TARGET" "$ORIGINAL" || rm -- "$TARGET";
                (crossref "$ORIGINAL" &);
          done
          }
# delete the last page of the PDF, similar to `pdf-cut`:
pdf-cut-last () {
    if (( $# != 1 )); then
        red "Usage: pdf-cut-last <pdf-file>" >&2
        return 1
    fi

    ORIGINAL=$(path2File "$1")
    TARGET=$(mktemp /tmp/XXXXXX.pdf)

    # Get the total number of pages
    PAGES=$(pdftk "$ORIGINAL" dump_data | grep NumberOfPages | awk '{print $2}')

    # If the PDF has only one page, we can't remove the last page
    if [ "$PAGES" -eq 1 ]; then
        red "Error: The PDF has only one page. Cannot remove the last page." >&2
        return 1
    fi

    # Remove the last page
    pdftk "$ORIGINAL" cat 1-r2 output "$TARGET" &&
    # Copy metadata to the new PDF
    exiftool -TagsFromFile "$ORIGINAL" -- "$TARGET" &&
    mv -- "$TARGET" "$ORIGINAL" || rm -- "$TARGET"

    (crossref "$ORIGINAL" &)
}
# sometimes we want to keep the first/cover page, but still don't want to actually make it the *first* page (or work around this with the `#page=2` trick; so we can just rotate it to the end rather than deleting it entirely.
alias pdfcut-append="pdf-cut-append"
pdf-cut-append () { if (( $# != 1 )); then red "Wrong number of arguments arguments; 'pdf-cut-append' moves the first page to the end. To delete the first page, use 'pdf-cut'." >&2 && return 1; fi
            ORIGINAL=$(path2File "$@")
            TARGET=$(mktemp /tmp/XXXXXX.pdf);
            pdftk "$ORIGINAL" cat 2-end 1  output "$TARGET" &&
            # pdftk by default erases all metadata, so we need to copy it all to the new PDF:
                exiftool -TagsFromFile "$ORIGINAL" -- "$TARGET" &&
            mv -- "$TARGET" "$ORIGINAL" || rm -- "$TARGET";
          (crossref "$ORIGINAL" &);
          }

# concatenate a set of PDFs, and preserve the metadata of the first PDF; this is useful for combining a paper with its supplement or other related documents, while not erasing the metadata the way naive `pdftk` concatenation would:
# This accepts Docx files as well due to their frequency in supplemental files, so `pdf-append foo.pdf supplement-1.doc supplement-2.pdf` is allowed (Docx is converted to PDF by `doc2pdf`).
# (The appended PDFs are soft-deleted by default, by moving them to the used temporary directory, which is not removed afterwards. In case of a rare problem, they can be retrieved from there.)
# (Assuming helper functions red, bold, path2File exist)
# --- pdf-append Function ---

# Concatenates a set of files (PDFs, documents, spreadsheets, images) into a single PDF.
#
# Usage:
#   pdf-append <target_pdf> <input_file_2> [input_file_3 ...]
#
# Arguments:
#   <target_pdf> : The path to the first PDF, which will also be the final output file.
#                  Its metadata will be preserved. If it doesn't exist, it will be created.
#                  If it exists but is not a PDF, the script will attempt to convert it
#                  (if supported type) and use it as the first part, but metadata
#                  preservation might be limited.
#   <input_file_N> : Additional files to append. Supported types:
#                    - PDF (.pdf)
#                    - Documents (.doc, .docx, .odt) via 'doc2pdf' or 'libreoffice'
#                    - Spreadsheets (.xlsx, .xls, .ods, .csv) via 'libreoffice'
#                    - Images (.png, .jpg, .jpeg, .webp, .avif, .gif) via 'img2pdf' or 'convert'
#
# Behavior:
#   - Non-PDF inputs are converted to temporary PDFs.
#   - All processed PDFs are concatenated using 'pdftk'.
#   - Metadata from the original <target_pdf> (first argument) is applied to the final output
#     using 'exiftool'. If the first argument wasn't originally a PDF, metadata might
#     be sourced from the *first successfully processed PDF* in the list.
#   - The original input files (from the 2nd argument onwards) are moved to a temporary
#     directory (`TEMP_DIR`) as a "soft delete" for potential recovery. This directory
#     is reported at the end and is *not* automatically removed.
#   - Temporary files (converted PDFs) within TEMP_DIR are *not* automatically removed.
#
# Dependencies:
#   bash, pdftk, exiftool, realpath (or adjust path2File)
#   Optional (for conversions):
#     libreoffice (for docs/spreadsheets)
#     doc2pdf (alternative for docs, often wraps LibreOffice/other)
#     img2pdf (preferred for images)
#     convert (from ImageMagick, fallback for images)
#
pdf-append () {
    # Enable case-insensitive matching for file extensions during processing
    shopt -s nocasematch

    if (( $# < 2 )); then
        red "Usage: pdf-append <target_pdf> <input_file_2> [input_file_3 ...]" >&2
        shopt -u nocasematch # Restore default matching
        return 1
    fi

    # Convert all arguments to their potentially canonical filepaths
    # Allows non-existent target path using realpath -m or similar in path2File
    local updated_args=()
    local file
    for file in "$@"; do
        local resolved_path
        resolved_path=$(path2File "$file") # Assumes path2File handles non-existent paths gracefully
        if [ -z "$resolved_path" ]; then
             # This might occur if path2File fails unexpectedly even with non-existent handling
             red "Error resolving path for argument: $file" >&2
             shopt -u nocasematch
             return 1
        fi
         updated_args+=( "$resolved_path" )
    done
    # Update the positional parameters ($1, $2, etc.) with resolved paths
    set -- "${updated_args[@]}"

    # The first argument serves as the final destination and primary metadata source
    local TARGET_DEST_PDF="$1"

    # Create temporary storage locations
    local TEMP_TARGET # Temporary file for the combined PDF before final move
    TEMP_TARGET="$(mktemp /tmp/pdf-append-combined-XXXXXX.pdf)"
    local TEMP_DIR # Directory for intermediate files and moved originals
    TEMP_DIR="$(mktemp --directory /tmp/pdf-append-work-XXXXXX)"

    # Ensure temporary files/dirs are cleaned up on script exit/interrupt, unless successful
    trap 'rm --force "$TEMP_TARGET"; rm --recursive --force -- "$TEMP_DIR"' EXIT HUP INT TERM

    # Array to hold paths to all PDFs ready for concatenation (originals + converted)
    local PDF_FILES=()
    # Array to hold original paths of input files (2nd onwards) to be moved at the end
    local FILES_TO_MOVE=("${@:2}") # Capture originals for soft-delete later

    # --- Process and Convert Input Files ---
    local output_pdf # Holds path for converted temporary PDF
    for file in "$@"; do
        # Define potential output path within TEMP_DIR for conversions
        # Using process ID ($$) makes it slightly more robust against unlikely name collisions
        output_pdf="$TEMP_DIR/$(basename "$file" | sed 's/[^a-zA-Z0-9._-]/_/g')-$$.pdf"

        if [[ "$file" =~ \.pdf$ ]]; then
            # Add existing PDFs directly to the list
            PDF_FILES+=("$file")

        elif [[ "$file" =~ \.(doc|docx|odt)$ ]]; then
            bold "Converting Document: $file" >&2
            if command -v doc2pdf &> /dev/null; then
                if doc2pdf "$file" "$output_pdf"; then
                    PDF_FILES+=("$output_pdf")
                else
                    red "doc2pdf conversion failed for: $file" >&2
                    # Continue processing other files
                fi
            elif command -v libreoffice &> /dev/null; then
                 bold " (using LibreOffice as fallback for document)" >&2
                 local LO_USER_PROFILE="file:///tmp/LibO_PDF_Conversion_$(date +%s)_$$"
                 if libreoffice --headless --invisible \
                            -env:UserInstallation="$LO_USER_PROFILE" \
                            --convert-to pdf:writer_pdf_Export \
                            --outdir "$TEMP_DIR" \
                            "$file" &>/dev/null; then
                      # LibreOffice might create a file with a different basename if source had spaces/etc
                      local expected_lo_output="$TEMP_DIR/$(basename "${file%.*}").pdf"
                      if [[ -f "$expected_lo_output" ]]; then
                           # Rename to our predictable name if needed
                           if [[ "$expected_lo_output" != "$output_pdf" ]]; then
                               mv -- "$expected_lo_output" "$output_pdf" || red "Failed to rename LO output"
                           fi
                           PDF_FILES+=("$output_pdf")
                      else
                           red "LibreOffice conversion produced no output for: $file" >&2
                      fi
                      rm --recursive --force -- "${LO_USER_PROFILE#file://}"
                 else
                      red "LibreOffice conversion failed for: $file" >&2
                      rm --recursive --force -- "${LO_USER_PROFILE#file://}"
                 fi
            else
                 red "Skipping document: $file (doc2pdf or libreoffice not found)" >&2
            fi

        elif [[ "$file" =~ \.(xlsx|xls|ods|csv)$ ]]; then
            bold "Converting Spreadsheet: $file" >&2
            if command -v libreoffice &> /dev/null; then
                local LO_USER_PROFILE="file:///tmp/LibO_PDF_Conversion_$(date +%s)_$$"
                if libreoffice --headless --invisible \
                            -env:UserInstallation="$LO_USER_PROFILE" \
                            --convert-to pdf:calc_pdf_Export \
                            --outdir "$TEMP_DIR" \
                            "$file" &>/dev/null; then
                    # Check if conversion was successful and find the output file
                    local expected_lo_output="$TEMP_DIR/$(basename "${file%.*}").pdf"
                    if [[ -f "$expected_lo_output" ]]; then
                          # Rename to our predictable name if needed
                         if [[ "$expected_lo_output" != "$output_pdf" ]]; then
                              mv -- "$expected_lo_output" "$output_pdf" || red "Failed to rename LO output"
                         fi
                        PDF_FILES+=("$output_pdf")
                    else
                        # Sometimes LO might output just ".pdf" if filename is tricky, try finding *any* new PDF
                        local found_pdf=$(find "$TEMP_DIR" -maxdepth 1 -name '*.pdf' -newer "$file" -print -quit)
                         if [[ -n "$found_pdf" ]] && [[ -f "$found_pdf" ]]; then
                            yellow "Warning: LibreOffice created unexpected PDF name '$found_pdf' for '$file'. Using it." >&2
                            mv -- "$found_pdf" "$output_pdf" || red "Failed to rename found LO output"
                            PDF_FILES+=("$output_pdf")
                        else
                            red "LibreOffice conversion produced no verifiable output for: $file" >&2
                        fi
                    fi
                    rm --recursive --force -- "${LO_USER_PROFILE#file://}"
                else
                    red "LibreOffice conversion failed for: $file" >&2
                    rm --recursive --force -- "${LO_USER_PROFILE#file://}"
                fi
            else
                red "Skipping spreadsheet: $file (libreoffice not found)" >&2
            fi

        elif [[ "$file" =~ \.(png|jpg|jpeg|webp|avif|gif)$ ]]; then
             bold "Converting Image: $file" >&2
             if command -v img2pdf &> /dev/null; then
                 # Attempt conversion using img2pdf
                 if img2pdf "$file" -o "$output_pdf"; then
                     PDF_FILES+=("$output_pdf")
                 else
                      red "img2pdf conversion failed for image: $file" >&2
                 fi
             elif command -v convert &> /dev/null; then
                  # Fallback to ImageMagick's convert
                  bold " (using ImageMagick 'convert' as fallback)" >&2
                  if convert "$file" "$output_pdf"; then
                      PDF_FILES+=("$output_pdf")
                  else
                      red "ImageMagick 'convert' failed for image: $file" >&2
                  fi
             else
                 # Neither conversion tool is available
                 red "Skipping image: $file (img2pdf and convert not found)" >&2
             fi

        else
            # Handle files that are not PDF and not of a supported convertible type
            # Special check: If the *first* file is unsupported, we cannot proceed meaningfully.
             if [[ "$file" == "$TARGET_DEST_PDF" ]]; then
                 red "First input file '$file' is not a PDF and is not a supported type for conversion. Cannot proceed." >&2
                 shopt -u nocasematch; trap - EXIT HUP INT TERM; rm --force "$TEMP_TARGET"; rm --recursive --force -- "$TEMP_DIR"; return 1;
            else
                 red "Skipping unsupported file type: $file" >&2
            fi
        fi
    done

    # --- Validation Before Concatenation ---
    if [ ${#PDF_FILES[@]} -lt 1 ]; then
        red "No PDF files available (or created) to concatenate." >&2
        shopt -u nocasematch; trap - EXIT HUP INT TERM; rm --force"$TEMP_TARGET"; rm --recursive --force -- "$TEMP_DIR"; return 1;
    fi
    # Check if only the first file remains (no new files added/valid)
     if [ ${#PDF_FILES[@]} -eq 1 ] && [ "${PDF_FILES[0]}" == "$TARGET_DEST_PDF" ]; then
         bold "Input consists only of the target file '$TARGET_DEST_PDF'. No changes needed." >&2
         shopt -u nocasematch; trap - EXIT HUP INT TERM; rm --force --"$TEMP_TARGET"; rm --recursive --force -- "$TEMP_DIR"; return 0
     fi

    # --- Concatenation ---
    bold "Concatenating ${#PDF_FILES[@]} PDF file(s) into temporary file..." >&2
    if pdftk "${PDF_FILES[@]}" cat output "$TEMP_TARGET"; then
        bold "Concatenation successful." >&2

        # --- Metadata Preservation ---
        local ORIGINAL_METADATA_SOURCE="$TARGET_DEST_PDF" # Default source is the first input file path
        bold "Attempting to preserve metadata from '$ORIGINAL_METADATA_SOURCE'..." >&2

        # Check if the designated source file actually exists (it might have been converted)
        if [ ! -f "$ORIGINAL_METADATA_SOURCE" ]; then
             # If the original first file doesn't exist (was non-PDF and now lives in TEMP_DIR),
             # try using the *first PDF* that was actually processed as the metadata source.
             if [ -f "${PDF_FILES[0]}" ]; then
                 ORIGINAL_METADATA_SOURCE="${PDF_FILES[0]}"
                 yellow "Warning: Original first file path '$TARGET_DEST_PDF' not found. Using metadata from first processed PDF: $ORIGINAL_METADATA_SOURCE" >&2
             else
                 # This case is unlikely if concatenation succeeded, but handle it.
                 yellow "Warning: Cannot find suitable source file for metadata. Skipping preservation." >&2
                 ORIGINAL_METADATA_SOURCE="" # Indicate failure to find source
             fi
        fi

        # Apply metadata if a source was determined
        if [[ -n "$ORIGINAL_METADATA_SOURCE" ]]; then
            if exiftool -q -q -TagsFromFile "$ORIGINAL_METADATA_SOURCE" -all:all --pdf-update:all "$TEMP_TARGET" -overwrite_original >/dev/null; then
                bold "Metadata preserved." >&2
            else
                 # exiftool might leave a backup file on failure, try cleaning it
                 exiftool -delete_original= -- "$TEMP_TARGET" &>/dev/null
                 yellow "Warning: exiftool failed to preserve metadata from '$ORIGINAL_METADATA_SOURCE'. Output PDF may lack original metadata." >&2
            fi
        fi

        # --- Final Output ---
        bold "Moving temporary result to final destination: $TARGET_DEST_PDF" >&2
        if mv -- "$TEMP_TARGET" "$TARGET_DEST_PDF"; then
            bold "Successfully created/updated: $TARGET_DEST_PDF" >&2

            # --- Cleanup: Move Original Input Files (Soft Delete) ---
            # Moves the original files listed in FILES_TO_MOVE (inputs from 2nd onwards)
            # This happens regardless of whether their conversion was successful or if they were used.
            bold "Moving original appended source files to $TEMP_DIR..." >&2
            local file_to_move
            for file_to_move in "${FILES_TO_MOVE[@]}"; do
                # Check if the original file still exists at its path before moving
                if [ -f "$file_to_move" ]; then
                    if ! mv -- "$file_to_move" "$TEMP_DIR/"; then
                        yellow "Warning: Failed to move original file '$file_to_move' to $TEMP_DIR" >&2
                    fi
                # Optional: Could add handling here if the file_to_move is already in TEMP_DIR (e.g., was a converted file path)
                # else
                #    echo "Debug: Original file '$file_to_move' not found at original path, likely already processed/moved." >&2
                fi
            done

            bold "Original appended files and intermediate PDFs are in: $TEMP_DIR" >&2
            # Optional: List contents for user confirmation
            # ls -lh "$TEMP_DIR" >&2

            # --- Post-processing ---
            if command -v crossref &>/dev/null; then
                bold "Running crossref on final PDF..." >&2
                crossref "$TARGET_DEST_PDF"
            fi

            # Success: Disable the cleanup trap so TEMP_DIR persists
            trap - EXIT HUP INT TERM
            shopt -u nocasematch # Restore default matching
            # Explicitly state that TEMP_DIR is kept:
            bold "Temporary directory $TEMP_DIR has been kept for inspection or recovery." >&2
            # To clean up automatically instead, uncomment the next line and remove the bold message above:
            # rm --recursive --force "$TEMP_DIR"
            return 0
        else
            red "Error: Failed to move temporary file '$TEMP_TARGET' to final destination '$TARGET_DEST_PDF'." >&2
            # Let the EXIT trap handle cleanup of TEMP_TARGET and TEMP_DIR
            shopt -u nocasematch
            return 1
        fi
    else
        red "Error: PDF concatenation failed using pdftk." >&2
        # Let the EXIT trap handle cleanup of TEMP_TARGET and TEMP_DIR
        shopt -u nocasematch
        return 1
    fi

    # Should not be reached, but ensure case-insensitivity is turned off
    shopt -u nocasematch
}

doc2pdf () {
    (( $# == 0 )) && { red "Usage: doc2pdf <input_file> [output_file]"; return 1; }
    input="$1"
    output="${2:-${input%.*}.pdf}"

    [[ ! -s "$input" || ! "$input" =~ \.(doc|docx|odt)$ ]] && { red "Error: Invalid or empty file: $input"; return 1; }

    soffice --convert-to pdf "$input" --headless --outdir "$(dirname "$output")"
    [ "$#" -eq 2 ] && mv -- "${input%.*}.pdf" "$output"
}

# trim whitespace from around JPG/PNG images
crop_one () {
    local file="$1"

    if [[ "$file" =~ \.(jpg|png)$ ]]; then

        local w h
        w=$(identify -ping -format '%[w]' -- "$file" 2>/dev/null || echo 0)
        h=$(identify -ping -format '%[h]' -- "$file" 2>/dev/null || echo 0)
        (( w * h > 25000000 )) && \
            red "Warning: Image '$file' is larger than 5,000×5,000 pixels." >&2

        # Use a temporary file for atomic saving
        local tmp_file="${file}.tmp"

        # Only replace the original if the convert command succeeds
        if nice convert "$file" \
            -crop "$(nice --adjustment=19 ionice --class 3 convert "$file" -virtual-pixel edge -blur 0x5 -fuzz 1% -trim -format '%wx%h%O' info:)" \
            +repage "$tmp_file"; then

            mv "$tmp_file" "$file"
        else
            rm -f "$tmp_file" # clean up
            red "Error: Failed to crop '$file'" >&2
        fi
    fi
}
crop () {
    (( $# == 0 )) && { echo "Usage: crop <files...>" >&2; return 1; }

    for f in "$@"; do
        printf '%s\0' "$(path2File "$f")"
    # `--memfree 1G` tells parallel to avoid starting new jobs if free memory <1GB
    done | parallel -0 --memfree 1G crop_one
}
export -f crop crop_one path2File red

alias invert="mogrify -negate"

# report to InvertOrNot.com one image URL that incorrectly inverts/invert-nots for dark-mode: <https://invertornot.com/docs#/default/correction_api_correction_post>
# `$ invert-error-report https://gwern.net/doc/math/2024-zhang-figure1-overfittingofmodelfamiliestogsm8k.png`
invert-error-report () { for URL in "$@"; do
                             curl --request 'POST' 'https://invertornot.com/api/correction' \
                              --header 'accept: application/json' --header 'Content-Type: application/json' \
                              --data '["'"$URL"'"]';
                       done; }

# add white pixels to an image which has been cropped too tightly to look good:
pad () {
    for FILE in "$@"; do
        mogrify -bordercolor white -border 30 "$(path2File "$FILE")"
    done
}
alias pad-white="pad"
pad-black () {
    for FILE in "$@"; do
        # NOTE: the color is not 'black', to match the current Gwern.net dark-mode's black background, which is not full-black but slightly grayed to avoid excessive contrast:
        mogrify -bordercolor "#161616" -border 30 "$(path2File "$FILE")"
    done
}
crop-pad () { crop "$@" && pad "$@"; }
crop-pad-black () { crop "$@" && pad-black "$@"; }

# function split_image () {     local image_path="$1";     local base_name=$(basename "$image_path" .png);     local height=$(identify -ping -format "%h" -- "$image_path");     local half_height=$((height / 2))     convert "$image_path" -crop 100%x50%+0+0 "${base_name}-1.png";     convert "$image_path" -crop 100%x50%+0+$half_height "${base_name}-2.png"; }

# convert black background to white:  `mogrify -fuzz 5% -fill white -draw "color 0,0 floodfill"`

# Image combination utilities: use ImageMagick to stitch together sets of images into grids.
#
# These helpers are how Gwern.net grids, screenshot mosaics, comic strips, and sample galleries
# get assembled. They integrate with 'pad', 'upload', 'compressJPG', etc.: a typical workflow
# might be 'pad *.jpg && cv4 && upload *.jpg' to pad, tile, and publish a batch of images.
#
# DESIGN
# ------
# A single core command, 'combine', takes a layout pattern and a list of images.
# The pattern is a tiny DSL: slot numbers (1-indexed, referring to images within a batch)
# arranged left-to-right, with '-' as a row separator and '_' as an explicit blank cell.
# Slot numbers may appear in any order, so reordering like '4 1 - 3 2' is valid. What is
# disallowed is a sparse set like '1 3': batching is by the highest referenced slot, so that
# would silently consume slot 2. Use '_' for a real blank cell instead.
# The pattern tiles automatically across the input images, consuming the highest referenced
# slot number per batch.
#
#   combine PATTERN -- [FILES...]
#
# The '--' separates pattern from files. If FILES is omitted, all images in cwd are used
# (sorted by version-sort). If '--' is absent entirely, all arguments are treated as files
# and combined into a single horizontal row (legacy/convenience behavior).
#
# Output format is inferred from the first input file's extension. The first file in each
# batch is overwritten with the combined result; the rest are deleted. Mismatched dimensions
# are handled by centering on a black background.
#
# A download guard ('_combine_wait_ready') waits up to 50s for files still being written to
# disk, preventing corruption when images are being saved from a browser concurrently.
#
# PATTERN DSL
# -----------
# Tokens:
#   N    - slot number (1-indexed): which image in the current batch occupies this cell
#   _    - explicit blank cell: black placeholder, does not consume an input image
#   -    - row break: start a new row below
#
# Within a row, slots are joined horizontally (+append). Rows are stacked vertically (-append).
# Slot numbers can appear in any order, enabling reversal/permutation without reordering the
# file arguments themselves.
#
# EXAMPLES
# --------
#   combine -- *.jpg                   # all images in one horizontal row
#   combine 1 2 -- *.jpg               # horizontal pairs, batched
#   combine 1 - 2 -- *.jpg             # vertical pairs, batched
#   combine 1 2 - 3 4 -- *.jpg         # 2×2 grid, batched
#   combine 1 2 - 3 _ -- *.jpg         # 3-image 2×2 grid with blank lower-right
#   combine 4 1 - 3 2 -- *.jpg         # 2×2 grid with batch-local permutation
#   combine 2 1 -- a.jpg b.jpg         # horizontal pair, reversed order
#   combine 1 - 2 - 3 - 4 -- *.jpg     # vertical stack of 4, batched
#   combine a.jpg b.jpg c.jpg          # (no '--') horizontal, all at once
#
# Leftover images that don't fill a complete batch are skipped with a message.
#
# ALIASES
# -------
# Short wrappers for common patterns, for interactive muscle memory:
#   combine2       →  combine 1 2 --              (horizontal pairs)
#   combiner / cbr →  combine, file args reversed (horizontal, reversed)
#   combinev / cv  →  combine 1 - 2 - ... - N --  (vertical, all)
#   combinevr / cvr → combinev, file args reversed
#   combinev2 / cv2 → combine 1 - 2 --            (vertical pairs)
#   combinev2r/cv2r → combinev2, file args reversed
#   combineSquare  →  combine 1 2 - 3 4 --        (2×2 grid)
#   combinev4 / cv4 → smart: per batch, picks 2×2 (or 3+blank) vs 1×4 / 1×3
#
# SEPARATE COMMANDS (not part of the DSL)
# ----------------------------------------
# combine-unstack       - destructive crop: split a 2×2 grid image into 4 tiles, re-stack as 1×4
# combine-unstack-last  - same, but delete the 4th tile (assumed blank) before re-stacking
# These operate on the image file itself (mogrify), not on multiple files.
#
# GIF files are excluded from auto-discovery because they may be animations requiring
# special handling (conversion to MP4, frame extraction, etc.).

# Collect image files in cwd into a named Bash array variable (default: REPLY).
# Usage: get_image_files myarray  →  populates $myarray; get_image_files  →  populates $REPLY.
# Finds .jpg/.jpeg/.png/.webp/.avif (non-recursive, version-sorted). GIF excluded because
# animations need special handling (MP4 conversion, frame extraction). Assumes no newlines in filenames.
get_image_files () {
    local _varname="${1:-REPLY}"
    mapfile -t "$_varname" < <(find . -maxdepth 1 -type f                              \( -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.png" -o -iname "*.webp" -o -iname "*.avif" \)                             | sort --version-sort)
}
get_ext () {
    local f="$1"
    if [[ "$f" != *.* ]]; then
        red "Error: File '$f' has no extension. Cannot determine output format." >&2
        return 1
    fi
    local extension="${f##*.}"
    if [[ -z "$extension" ]]; then
        red "Error: File '$f' has an empty extension." >&2
        return 1
    fi
    echo "$extension"
}

# Wait for a file to exist, be non-empty, and remain untouched for ~3s.
# Second argument is a timeout in seconds, unlike the global 'is_downloading' helper.
_combine_wait_ready () {
    local file="$1"
    local timeout="${2:-50}"
    local deadline=$((SECONDS + timeout))
    local modified elapsed

    while true; do
        if [[ -s "$file" ]]; then
            modified=$(stat -c %Y "$file") || return 1
            elapsed=$(( $(date +%s) - modified ))
            if (( elapsed >= 3 )); then
                return 0
            fi
        fi
        if (( SECONDS >= deadline )); then
            red "combine: timed out waiting for '$file' to finish being written." >&2
            return 1
        fi
        sleep 1
    done
}

# Make a black blank tile matching the reference image dimensions.
_combine_make_blank () {
    local ref="$1"
    local ext="$2"
    local T w h

    read -r w h < <(identify -ping -format "%w %h" -- "$ref" 2>/dev/null)
    if [[ -z "$w" || -z "$h" ]]; then
        red "combine: failed to read dimensions of blank reference '$ref'." >&2
        return 1
    fi

    T=$(mktemp "/tmp/XXXXXXX.$ext") || return 1
    if convert -size "${w}x${h}" xc:black "$T"; then
        echo "$T"
    else
        rm --force -- "$T"
        return 1
    fi
}

# ── Core combiner ──────────────────────────────────────────────────────────
#
# combine [PATTERN...] [-- [FILES...]]
#
# PATTERN tokens: positive integers (slot numbers), '_' (explicit blank cells), and '-'
# (row separator). If '--' is absent, all arguments are treated as files and combined
# horizontally (legacy). If FILES is empty after '--', defaults to all images in cwd.
# If PATTERN is empty (bare 'combine -- ...'), combines all files horizontally.
combine () {
    local -a pattern=() files=() rows=()
    local seen_sep=false
    local arg i token current_row="" max_slot=0 last_token_was_row_break=false
    local -a slot_seen=()

    # ── parse: split on '--' ──
    for arg in "$@"; do
        if [[ "$arg" == "--" ]]; then
            seen_sep=true
            continue
        fi
        if $seen_sep; then
            files+=("$arg")
        else
            pattern+=("$arg")
        fi
    done

    # No '--': legacy mode, all args are files, horizontal combine-all.
    if ! $seen_sep; then
        files=("${pattern[@]}")
        pattern=()
    fi

    # Default files: all images in cwd.
    if (( ${#files[@]} == 0 )); then
        get_image_files files
    fi
    if (( ${#files[@]} == 0 )); then
        red "combine: no images found." >&2
        return 1
    fi
    if (( ${#files[@]} == 1 )); then
        red "combine: ≥2 images required but got 1." >&2
        return 1
    fi

    # Default pattern: single row of all files (horizontal combine-all).
    if (( ${#pattern[@]} == 0 )); then
        for ((i=1; i<=${#files[@]}; i++)); do
            pattern+=("$i")
        done
    fi

    # ── parse pattern into rows and batch_size ──
    for token in "${pattern[@]}"; do
        if [[ "$token" == "-" ]]; then
            if [[ -z "$current_row" ]]; then
                red "combine: empty row in pattern." >&2
                return 1
            fi
            rows+=("$current_row")
            current_row=""
            last_token_was_row_break=true
        elif [[ "$token" == "_" ]]; then
            current_row+="${current_row:+ }_"
            last_token_was_row_break=false
        elif [[ "$token" =~ ^[0-9]+$ ]] && (( token >= 1 )); then
            current_row+="${current_row:+ }$token"
            (( token > max_slot )) && max_slot=$token
            slot_seen[token]=1
            last_token_was_row_break=false
        else
            red "combine: invalid pattern token '$token'." >&2
            return 1
        fi
    done
    if $last_token_was_row_break; then
        red "combine: pattern cannot end with '-'." >&2
        return 1
    fi
    [[ -n "$current_row" ]] && rows+=("$current_row")

    if (( ${#rows[@]} == 0 )); then
        red "combine: empty or invalid pattern." >&2
        return 1
    fi
    if (( max_slot == 0 )); then
        red "combine: pattern references no images; use numeric slots as well as '_' blanks." >&2
        return 1
    fi
    for ((i=1; i<=max_slot; i++)); do
        if [[ -z "${slot_seen[i]:-}" ]]; then
            red "combine: sparse slot set; missing slot ${i} between 1 and ${max_slot}. Reordering is fine, but sparse numbering like '1 3' is ambiguous; use '_' for blanks." >&2
            return 1
        fi
    done

    local batch_size=$max_slot

    # ── download guard ──
    local IMG
    for IMG in "${files[@]}"; do
        _combine_wait_ready "$IMG" 50 || return 2
    done

    # ── process batches ──
    local offset=0
    local processed_any=false
    while (( offset + batch_size <= ${#files[@]} )); do
        local -a batch=("${files[@]:offset:batch_size}")
        local ext
        ext=$(get_ext "${batch[0]}") || return 1

        # Build each row via +append, then stack rows via -append.
        local -a row_temps=()
        local ok=true
        local row row_ref T target
        for row in "${rows[@]}"; do
            local -a slots=() row_files=() row_generated=()
            local slot blank_temp

            read -ra slots <<< "$row"
            row_ref="${batch[0]}"
            for slot in "${slots[@]}"; do
                if [[ "$slot" != "_" ]]; then
                    row_ref="${batch[$((slot - 1))]}"
                    break
                fi
            done

            for slot in "${slots[@]}"; do
                if [[ "$slot" == "_" ]]; then
                    blank_temp=$(_combine_make_blank "$row_ref" "$ext") || {
                        ok=false
                        break
                    }
                    row_files+=("$blank_temp")
                    row_generated+=("$blank_temp")
                else
                    row_files+=("${batch[$((slot - 1))]}")
                fi
            done
            if ! $ok; then
                if (( ${#row_generated[@]} > 0 )); then
                    rm --force -- "${row_generated[@]}"
                fi
                break
            fi

            T=$(mktemp "/tmp/combine-row-XXXXXX.${ext}") || {
                if (( ${#row_generated[@]} > 0 )); then
                    rm --force -- "${row_generated[@]}"
                fi
                return 1
            }
            if (( ${#row_files[@]} == 1 )); then
                cp -- "${row_files[0]}" "$T" || ok=false
            else
                convert -background black -gravity center +append -- "${row_files[@]}" "$T" || ok=false
            fi

            if (( ${#row_generated[@]} > 0 )); then
                rm --force -- "${row_generated[@]}"
            fi

            if $ok; then
                row_temps+=("$T")
            else
                rm --force -- "$T"
                break
            fi
        done

        if ! $ok; then
            if (( ${#row_temps[@]} > 0 )); then
                rm --force -- "${row_temps[@]}"
            fi
            return 3
        fi

        target=$(mktemp "/tmp/combine-target-XXXXXX.${ext}") || {
            if (( ${#row_temps[@]} > 0 )); then
                rm --force -- "${row_temps[@]}"
            fi
            return 1
        }
        if (( ${#row_temps[@]} == 1 )); then
            mv -- "${row_temps[0]}" "$target"
        else
            if ! convert -background black -gravity center -append -- "${row_temps[@]}" "$target"; then
                rm --force -- "$target" "${row_temps[@]}"
                return 3
            fi
            rm --force -- "${row_temps[@]}"
        fi

        if ! mv -- "$target" "${batch[0]}"; then
            rm --force -- "$target"
            return 3
        fi
        if (( ${#batch[@]} > 1 )); then
            rm -- "${batch[@]:1}" || return 3
        fi

        processed_any=true
        offset=$((offset + batch_size))
    done

    # Leftovers
    local leftover=$(( ${#files[@]} - offset ))
    if (( leftover > 0 )); then
        local -a skipped=("${files[@]:offset}")
        echo "(Skipping ${leftover} leftover image(s): ${skipped[*]})"
        if ! $processed_any; then
            return 1
        fi
    fi
}

# ── Wrappers ───────────────────────────────────────────────────────────────

# Build a pattern string dynamically: _combine_all_pattern N "-" → "1 - 2 - ... - N"
#                                      _combine_all_pattern N ""  → "1 2 ... N"
_combine_all_pattern () {
    local n="$1"
    local sep="${2:-}"
    local i
    local pattern=()

    for ((i=1; i<=n; i++)); do
        pattern+=("$i")
        if [[ -n "$sep" ]] && (( i < n )); then
            pattern+=("$sep")
        fi
    done
    echo "${pattern[*]}"
}

# Reverse file args, then delegate.
_combine_reversed () {
    local wrapper="$1"
    shift

    local args=("$@")
    local reversed=()
    local i

    if (( ${#args[@]} == 0 )); then
        get_image_files args
    fi
    for ((i=${#args[@]}-1; i>=0; i--)); do
        reversed+=("${args[i]}")
    done
    "$wrapper" "${reversed[@]}"
}

# Fail loudly when explicitly supplied files cannot fill whole fixed-size batches.
_combine_require_explicit_multiple () {
    local batch_size="$1"
    shift

    if (( $# == 0 )); then
        return 0
    fi
    if (( $# < batch_size || $# % batch_size != 0 )); then
        red "combine: explicit file count $# is not a multiple of ${batch_size}." >&2
        return 1
    fi
}

# combine vertically (all images into one column)
combinev () {
    local args=("$@")
    local pat_str
    local pat=()

    if (( ${#args[@]} == 0 )); then
        get_image_files args
    fi
    pat_str=$(_combine_all_pattern "${#args[@]}" "-")
    read -ra pat <<< "$pat_str"
    combine "${pat[@]}" -- "${args[@]}"
}
alias cv="combinev"

# combine horizontally, reversed
combiner () { _combine_reversed combine "$@"; }
alias cbr="combiner" # we can't use 'cr' because that collides with 'crossref'

# combine vertically, reversed
combinevr () { _combine_reversed combinev "$@"; }
alias cvr="combinevr"

# combine horizontally in pairs
combine2 () {
    _combine_require_explicit_multiple 2 "$@" || return 1
    combine 1 2 -- "$@"
}
alias c2="combine2"

# combine vertically in pairs
combinev2 () {
    _combine_require_explicit_multiple 2 "$@" || return 1
    combine 1 - 2 -- "$@"
}
alias cv2="combinev2"

# combine vertically in pairs, reversed
combinev2r () { _combine_reversed combinev2 "$@"; }
alias cv2r="combinev2r"

# 2×2 grid, batched
combineSquare () {
    _combine_require_explicit_multiple 4 "$@" || return 1
    combine 1 2 - 3 4 -- "$@"
}

# True if every image in the batch is wider than tall.
_combine_batch_is_wide () {
    local img w h
    for img in "$@"; do
        _combine_wait_ready "$img" 50 || return 2
        read -r w h < <(identify -ping -format "%w %h" -- "$img" 2>/dev/null)
        if [[ -z "$w" || -z "$h" ]]; then
            red "combinev4: failed to read dimensions of '$img'." >&2
            return 2
        fi
        if (( w <= h )); then
            return 1
        fi
    done
    return 0
}

_combinev4_next_chunk_size () {
    local remaining="$1"
    case "$remaining" in
        2|3|4)
            echo "$remaining"
            ;;
        *)
            if (( remaining - 4 == 1 )); then
                echo 3
            else
                echo 4
            fi
            ;;
    esac
}

_combinev4_run_batch () {
    local batch=("$@")
    local pattern=()

    case ${#batch[@]} in
        2)
            pattern=(1 - 2)
            ;;
        3)
            if _combine_batch_is_wide "${batch[@]}"; then
                pattern=(1 - 2 - 3)
            else
                pattern=(1 2 - 3 _)
            fi
            ;;
        4)
            if _combine_batch_is_wide "${batch[@]}"; then
                pattern=(1 - 2 - 3 - 4)
            else
                pattern=(1 2 - 3 4)
            fi
            ;;
        *)
            red "combinev4: expected 2-4 images in a batch, got ${#batch[@]}." >&2
            return 1
            ;;
    esac

    combine "${pattern[@]}" -- "${batch[@]}"
}

# Smart 2×2 or 1×4 based on aspect ratio, decided per batch.
combinev4 () {
    local args=("$@")
    local offset=0 remaining chunk_size
    local batch=()

    if (( ${#args[@]} == 0 )); then
        get_image_files args
    fi
    if (( ${#args[@]} < 2 )); then
        red "combinev4: ≥2 images required but got ${#args[@]}." >&2
        return 1
    fi

    while (( offset < ${#args[@]} )); do
        remaining=$(( ${#args[@]} - offset ))
        chunk_size=$(_combinev4_next_chunk_size "$remaining")
        batch=("${args[@]:offset:chunk_size}")
        _combinev4_run_batch "${batch[@]}" || return $?
        offset=$((offset + chunk_size))
    done
}
alias cv4="combinev4"

# ── Unstacking (destructive crops, not compositions) ───────────────────────
# used in feh:
# rotate a 2×2=4 images → 1×4
combine-unstack () {
    local args=("$@")
    if (( ${#args[@]} == 0 )); then get_image_files args; fi
    for IMG in "${args[@]}"; do _combine_wait_ready "$IMG" 50 || return 2; done
    for FILE in "${args[@]}"; do mogrify -crop 50%x50% +repage -append -- "$FILE"; done
}
# rotate, and delete the last square, under the assumption that it is a 4-square with a blank black final square:
combine-unstack-last () {
    local args=("$@")
    if (( ${#args[@]} == 0 )); then get_image_files args; fi
    for IMG in "${args[@]}"; do _combine_wait_ready "$IMG" 50 || return 2; done
    for FILE in "${args[@]}"; do mogrify -crop 50%x50% +repage -delete 3 -append -- "$FILE"; done
}

# convert an animated GIF to MP4; MP4s are preferred on Gwern.net for efficiency and controllability.
# Uses H.264 with web-optimized settings (faststart, yuv420p, CRF 23).
# Skips static GIFs. Reports size savings per file.
# Usage: gifToMp4 [file.gif ...] (defaults to all .gif files in current directory)
gifToMp4() {
    local args=("$@")

    # Find all GIFs in current directory if no arguments provided
    if [[ ${#args[@]} -eq 0 ]]; then
        mapfile -t args < <(find . -maxdepth 1 -type f -iname "*.gif" | sort --version-sort)
        if [[ ${#args[@]} -eq 0 ]]; then
            red "Error: No .gif files found in current directory" >&2
            return 1
        fi
        bold "Processing ${#args[@]} .gif files found in current directory"
    fi

    local input
    for input in "${args[@]}"; do
        if [[ ! "${input,,}" =~ \.gif$ ]]; then
            red "Error: Skipping non-GIF file: $input" >&2
            continue
        fi
        if [[ ! -f "$input" ]]; then
            red "Error: File not found: $input" >&2
            continue
        fi

        # Check if GIF is animated
        local frame_count
        frame_count=$(ffprobe -v error -select_streams v:0 -count_packets \
                             -show_entries stream=nb_read_packets -of csv=p=0 "$input" 2>/dev/null | tr -d '[:space:]')
        if [[ -z "$frame_count" || ! "$frame_count" =~ ^[0-9]+$ || "$frame_count" -le 1 ]]; then
            echo "Skipping static GIF: $input" >&2
            continue
        fi

        is_downloading "$input" 50 || continue

        local output="${input%.*}.mp4"
        bold "Converting: $input → $output ($frame_count frames)"

        # -c:v libx264:         H.264 for broad compatibility
        # -crf 23:              visually lossless for animation; default quality
        # -preset slower:       better compression for archival web hosting
        # -pix_fmt yuv420p:     required for broad player compatibility
        # -movflags +faststart: move moov atom to start for progressive web playback
        # -an:                  strip audio (GIFs have none; avoids empty audio tracks)
        # scale filter:         ensure even dimensions required by yuv420p
        if ffmpeg -y -loglevel warning -i "$input" \
                  -c:v libx264 -crf 23 -preset slower \
                  -pix_fmt yuv420p \
                  -movflags +faststart \
                  -an \
                  -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" \
                  "$output"; then
            local gif_size mp4_size savings
            gif_size=$(stat -c%s "$input")
            mp4_size=$(stat -c%s "$output")
            savings=$(echo "scale=1; (1 - $mp4_size / $gif_size) * 100" | bc)
            green "  $input: $(numfmt --to=iec "$gif_size") → $(numfmt --to=iec "$mp4_size") (${savings}% smaller)"
        else
            red "Error: ffmpeg conversion failed for: $input" >&2
            rm --force -- "$output" # clean up partial output
        fi
    done
}
export -f gifToMp4

# mp4-check-faststart: report which MP4s are missing the faststart moov-atom optimization.
# The moov atom is the MP4 "table of contents"; when it follows mdat (the actual media data),
# browsers must download the entire file before playback can begin.
# Usage: mp4-check-faststart [file.mp4 ...] (defaults to all .mp4 files in current directory)
mp4-check-faststart () {
    local args=("$@")
    if [[ ${#args[@]} -eq 0 ]]; then
        mapfile -t args < <(find . -maxdepth 1 -type f -iname "*.mp4" | sort --version-sort)
        if [[ ${#args[@]} -eq 0 ]]; then
            red "Error: No .mp4 files found in current directory" >&2
            return 1
        fi
    fi

    local slow=0 fast=0 total=${#args[@]}
    local file
    for file in "${args[@]}"; do
        if [[ ! -f "$file" ]]; then
            red "Error: File not found: $file" >&2
            continue
        fi

        # Extract top-level atom order via ffprobe; look for whether moov appears before mdat.
        # The -show_entries format_tags trick doesn't help here; instead we use -show_format
        # and check the actual byte-level atom layout with a simple python one-liner:
        local atom_order
        atom_order=$(python3 -c "
import struct, sys
with open(sys.argv[1], 'rb') as f:
    atoms = []
    while True:
        hdr = f.read(8)
        if len(hdr) < 8:
            break
        size, name = struct.unpack('>I4s', hdr)
        name = name.decode('ascii', errors='replace')
        atoms.append(name)
        if name in ('moov', 'mdat'):
            pass  # keep scanning for both
        if size == 1:  # 64-bit extended size
            ext = f.read(8)
            if len(ext) < 8:
                break
            size = struct.unpack('>Q', ext)[0]
            f.seek(size - 16, 1)
        elif size == 0:  # atom extends to EOF
            break
        else:
            f.seek(size - 8, 1)
    # Report: did moov come before mdat?
    moov_idx = next((i for i, a in enumerate(atoms) if a == 'moov'), None)
    mdat_idx = next((i for i, a in enumerate(atoms) if a == 'mdat'), None)
    if moov_idx is not None and mdat_idx is not None:
        print('fast' if moov_idx < mdat_idx else 'slow')
    elif moov_idx is None:
        print('error:no-moov')
    else:
        print('fast')  # no mdat is unusual but moov-first is fine
" "$file" 2>/dev/null)

        case "$atom_order" in
            slow)
                echo "$file"
                ((slow++))
                ;;
            fast)
                ((fast++))
                ;;
            *)
                yellow "Warning: Could not determine atom order for: $file" >&2
                ;;
        esac
    done

    echo "---" >&2
    echo "$total checked: $slow slow (moov after mdat), $fast already fast" >&2
}
export -f mp4-check-faststart

# mp4-fix-faststart: losslessly remux MP4s to move the moov atom before mdat.
# Only touches files that actually need it. No re-encoding; bitwise identical audio/video streams.
# Usage: mp4-fix-faststart [file.mp4 ...] (defaults to all slow MP4s in current directory)
mp4-fix-faststart () {
    local args=("$@")
    if [[ ${#args[@]} -eq 0 ]]; then
        # Default: find and fix only the slow ones
        mapfile -t args < <(mp4-check-faststart 2>/dev/null)
        if [[ ${#args[@]} -eq 0 ]]; then
            bold "All MP4s in current directory already have faststart." >&2
            return 0
        fi
        bold "Found ${#args[@]} MP4s needing faststart fix"
    fi

    local file
    for file in "${args[@]}"; do
        if [[ ! -f "$file" ]]; then
            red "Error: File not found: $file" >&2
            continue
        fi

        is_downloading "$file" 50 || continue

        local tmp
        tmp=$(mktemp "${file%.mp4}-faststart-XXXXXX.mp4")

        bold "Fixing: $file"
        # -c copy:              stream copy, no re-encoding (lossless, fast)
        # -movflags +faststart: move moov atom to beginning
        # -map_metadata 0:     preserve all metadata from source
        if ffmpeg -y -loglevel warning \
                  -i "$file" \
                  -c copy \
                  -map_metadata 0 \
                  -movflags +faststart \
                  "$tmp"; then
            # Sanity check: output should be roughly the same size (remux adds/removes only bytes of moov relocation)
            local orig_size new_size
            orig_size=$(stat -c%s "$file")
            new_size=$(stat -c%s "$tmp")
            # If the new file is wildly different in size (>5% smaller), something went wrong
            if (( new_size < orig_size * 90 / 100 )); then
                red "  Error: output is suspiciously smaller ($(numfmt --to=iec "$orig_size") → $(numfmt --to=iec "$new_size")); keeping original" >&2
                rm --force -- "$tmp"
                continue
            fi
            mv -- "$tmp" "$file"
            green "  Done: $file ($(numfmt --to=iec "$orig_size"))"
        else
            red "  Error: ffmpeg remux failed for: $file" >&2
            rm --force -- "$tmp"
        fi
    done
}
export -f mp4-fix-faststart

# check a PNG to see if it can be turned into a JPG with minimal quality loss (according to the ImageMagick PSNR perceptual loss); for PNGs that should be JPGs (eg. photos are surprisingly often saved as PNGs), often the JPG will be a third the size or less, which (particularly for large images like sample-grids) makes for more pleasant web browsing.
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
        PSNR=$(compare -metric PSNR "$ARG" "$JPG" null: 2>&1 | cut --delimiter=' ' --fields=1)

        # Check both PSNR quality and size reduction
        if (( $(echo "$PSNR > $QUALITY_THRESHOLD" | bc -l) )) && (( $(echo "$SIZE_REDUCTION >= $SIZE_REDUCTION_THRESHOLD" | bc -l) )); then
            echo "$ARG"
        fi

        # Clean up: Remove the temporary JPG file
        rm -- "$JPG"
    done
}
export -f png2JPGQualityCheck

# crossref: defined in ~/wiki/static/build/crossref
cr () { crossref "$@" & }

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
                         mv -- "$TMP" "$FILE"
               fi;
               echo "$FILE"
               exiftool -m -overwrite_original "$FILE" "$@";
               # getting very tired of these hyphen junk in my titles...
               TITLE1="$(exiftool -printFormat '$Title' -Title -- "$FILE")"
               TITLE2="$(echo "$TITLE1" | sed -e 's/‐/-/g' -e 's/^ \+//g' -e 's/ \+$//g' -e 's/\.$//' | tr '_' ':')" # WARNING: tr mangles Unicode, but sed doesn't
               if [[ "$TITLE1" != "$TITLE2" ]]; then exiftool -overwrite_original -Title="$TITLE2" -- "$FILE"; fi

           else emacsclient "$FILE";
           fi;
       else red "File does not exist? $FILE"
       fi;
     }
alias ea="exiftool -All"
alias exiftool="exiftool -overwrite_original"

# Gwern.net searches:
## fixed-grep gwern.net specifically:
gw () {
    if (( $# == 0 )); then red "Missing search query." >&2 && return 2; fi

    QUERY=$(echo "$*" | tr -d '\n') # remove newlines, which are usually spurious (and also not supported by grep by default?) thanks to browsers injecting newlines everywhere when copy-pasting...
    RESULTS=$( (find ~/wiki/ -type f -name "*.md";
         ls ~/.emacs ~/*.md;
         find ~/wiki/metadata/ ~/wiki/haskell/ -name "*.hs" -or -name "*.gtx" | grep --fixed-strings -v -e 'metadata/listsortedmagic.hs' -e 'metadata/listname.hs';
         find ~/wiki/static/ -type f -name "*.js" -or -name "*.css" -or -name "*.hs" -or -name "*.conf" -or -name "*.gtx" -or -name "*.py" -or -name "*.sh";
         find ~/wiki/ -type f -name "*.html" -not -wholename "*/doc/*" ) | \
           grep --fixed-strings -v -e '.#' -e 'auto.hs' -e doc/link-bibliography/ -e metadata/annotation/ -e _site/ -e _cache/ | sort --unique  | \
           xargs grep --fixed-strings --color=always --ignore-case --with-filename -- "$QUERY" | cut -c 1-2548);
    if [ -z "$RESULTS" ]; then
        gwl "$QUERY" # fall back to double-checking IRC logs
    else
        echo "$RESULTS"
    fi
}

## file names only (include home-directory in case of duplicate or not-yet-uploaded files)
gwf () { (cd ~/wiki/ && (find ~/ -maxdepth 1 -type f; find . -type f) | grep --fixed-strings -v -e '.#' -e '_cache/' -e '_site/' -e '.git/' | grep --ignore-case "$@" | sed -e 's/^\.\//\//g') | sort; } # path2File?
## Newsletter only:
gwn () { if (( $# != 1 )); then QUERY="$*"; else QUERY="$@"; fi
        find ~/wiki/newsletter/ -type f -name "*.md" | \
             grep --fixed-strings -v -e '.#' |
            sort --unique  | xargs grep --extended-regexp --ignore-case --color=always --with-filename "$QUERY" | cut --characters 1-2048; }
## Annotations:
# gwa: defined in ~/wiki/static/build/gwa
gwal () { gwa "$@" | less -p "$@"; }

## #lesswrong IRC logs
gwl () { if (( $# != 1 )); then QUERY="$*"; else QUERY="$@"; fi
         grep --extended-regexp --context=1 --text --ignore-case -- "$QUERY" ~/doc/irclogs/*/\#lesswrong.log; }

# Gwern.net modifications:
## gwsed shortcut for the common use case of updating a domain HTTP → HTTPS; typically the URLs are otherwise unchanged, and don't need to be individually updated.
gwhttp () {
    if (( $# != 1 )); then
        echo "HTTP migration only works for 1 domain, did you try a regular search-and-replace?" >&2
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
if (( $# == 1 )) && [[ $1 =~ \.png$ && $(cd ~/wiki/ && git ls-files --error-unmatch ./"$1" 2>/dev/null) ]];
   then gwmv "$1" "${1%.png}.jpg"
else
    if (( $# != 2 )); then
        red "Need two arguments: OLD file and NEW file! Only got: $@" >&2
        return 2
    else

        set -x
        if [[ ! $(pwd) =~ "/home/gwern/wiki/".* ]]; then cd ~/wiki/ ; fi
        OLD=$(echo "$1" | tr -d '  ⁠' | sed -e 's/https:\/\/gwern\.net//g' -e 's/^\///g' | xargs realpath | sed -e 's/\/home\/gwern\/wiki\//\//g' )
        NEW=$(echo "$2" | tr -d ' ⁠ ' | sed -e 's/https:\/\/gwern\.net//g' -e 's/^\///g' | xargs realpath | sed -e 's/\/home\/gwern\/wiki\//\//g')
        if [[ "$NEW" == "" ]]; then red "Processing arguments failed, exiting immediately!" >&2 ; echo "$OLD" "$NEW" >&2 ; return 8; fi
        # Check if the parent directory of the NEW path exists
        NEW_DIR=$(dirname "$HOME/wiki$NEW")
        if [ ! -d "$NEW_DIR" ]; then
            red "Target directory $NEW_DIR does not exist. Operation aborted." >&2
            return 7
        fi

        if [ -d "$HOME/wiki$OLD" ] || [ -d "${OLD:1}" ]; then
            red "The first argument ($1 $OLD) is a directory. Please use 'gwmvdir' to rename entire directories." >&2
            return 3
        elif [ ! -f "$HOME/wiki$OLD" ] && [ ! -f "${OLD:1}" ]; then
            red "File $OLD not found in current directory or ~/wiki" >&2
            return 4
        fi

        # strip any slash prefix like '/doc/psychology/2021-foo.pdf' → 'doc/psychology/2021-foo.pdf' to turn it into a local relative link

        # eg "gwMv 'doc/psychology/2021-foo.pdf' 'doc/psychology/writing'" as a shortcut for "gwmv 'doc/psychology/2021-foo.pdf' 'doc/psychology/writing/2021-foo.pdf'"
        if [ -d ~/wiki$NEW ]; then NEW=$(echo $NEW/$(basename $OLD) | sed -e 's/\/\//\//'); fi

        cd ~/wiki/
        if [[ -a ~/wiki$NEW ]]; then
            red "Moved-to target file $NEW exists! Will not move $OLD and overwrite it. If you deliberately want to overwrite $NEW, then explicitly delete it first." >&2
            return 5
        fi

        # Check if OLD is a PNG and NEW is a JPG for conversion
        if [[ "$OLD" =~ \.png$ && "$NEW" =~ \.jpg$ ]]; then
            # preserve the git history by stashing the converted JPG, doing a `git mv` to tell git about the file renaming, and then overwriting the 'JPG' (actually the original PNG) with an actual JPG
            TMP="$(mktemp /tmp/XXXXX.jpg)"
            convert "$HOME/wiki$OLD" "$TMP"
            compressJPG "$TMP"
            git mv "$HOME/wiki$OLD" "$HOME/wiki${NEW%.jpg}.jpg"
            mv -- "$TMP" "$HOME/wiki${NEW%.jpg}.jpg"
        elif [[ -a ~/wiki$OLD ]]; then
            touch ~/wiki"$NEW" && rm ~/wiki"$NEW" && git mv ~/wiki"$OLD" ~/wiki"$NEW" || return 3
        else
            red "File does not exist? $OLD (to be moved to $NEW)" >&2 && return 1;
        fi

        rsync --mkpath --chmod='a+r' --quiet -- ~/wiki"$NEW" gwern@176.9.41.242:"/home/gwern/gwern.net$NEW" || red "gwmv: rsync failed?" > /dev/null &
        gwsed "$OLD" "$NEW"
        # there are likely many Nginx redirects pointing from an inbound path to the old original URL as as a target.
        # These look like a single line like '"~^/inbound$" "/old";'
        # Update the target (tricky!):
        stringReplace '"'"$OLD"'";' '"'"$NEW"'";' ~/wiki/static/nginx/redirect/*.conf;
        echo '"~^'"$OLD"'.*$" "'"$NEW"'";' | tee --append ~/wiki/static/nginx/redirect/move.conf # 3. add a redirected old to nginx
        # 4. delete outdated annotations:
        OLD_FILE=$(basename "$OLD"); rm -- "$HOME/wiki/metadata/annotation/*$OLD_FILE*" || true > /dev/null
        wait

    set +x
    fi
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

    OLD="$(readlink --canonicalize "$OLD" | cut --delimiter='/' --fields=5-)"
    ls "$OLD"
    NEW="$(readlink --canonicalize "$NEW" | cut --delimiter='/' --fields=5-)"
    mkdir -p "$NEW"
    git add "$NEW"
    for FILE in $(ls "$OLD"/* | grep --fixed-strings -v 'index.md'); do
        echo "$FILE"
        gwmv "$FILE" "$NEW/$(basename $FILE)"
    done
    rm -- "$OLD/index.md" || true
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
                     # echo "---" && grep --fixed-strings -- "$1" ./metadata/*.gtx || true
                     timeout 20m nice changeTag +RTS -N2 -RTS "$@"; echo "" # &&
                         # echo "---" && grep --fixed-strings -- "$1" ./metadata/*.gtx
         ); }

# eg. `"ai ai/anime ai/anime/danbooru ... ai/scaling ai/scaling/economics ... japan/poetry/shotetsu japan/poetry/teika ... technology/digital-antiquarian ... zeo/short-sleeper"`
GWERNNET_DIRS_FULL="$(cd ~/ && find wiki/doc/ -type d | grep --fixed-strings -v -e 'doc/rotten.com' -e 'doc/www/' \
                         -e 2000-iapac-norvir -e mountimprobable.com -e personal/2011-gwern-yourmorals.org \
                         -e psychology/european-journal-of-parapsychology -e reinforcement-learning/armstrong-controlproblem \
                         -e statistics/order/beanmachine-multistage -e gwern.net-gitstats -e metadata/annotation | \
                         cut --delimiter='/' --fields=3- | sort)"
# eg. `"1 2 2010-crc 2013-cicadas 3 4 5 abandoned-footnotes ab-testing ... www zeami zeo"`
GWERNNET_DIRS_SHORT="$(echo $GWERNNET_DIRS_FULL | tr '/' '\n' | tr ' ' '\n' | sort --unique)"
# for completing tags which may need to be disambiguated, like 'gpt/nonfiction':
# eg. `"1/lsd 2/2010-crc 3/fiction 3/nonfiction ... palm/2 personality/conscientiousness personality/psychopathy ... video/analysis video/generation vision/dream"`
GWERNNET_DIRS_SUFFIXES="$(echo $GWERNNET_DIRS_FULL | tr ' ' '\n' | grep --extended-regexp -e '[a-z0-9-]+/[a-z0-9-]+/[a-z0-9-]+' | \
                               rev | cut --delimiter='/' --fields=1-2 | rev | sort --unique)"
complete -W "$GWERNNET_DIRS_FULL $GWERNNET_DIRS_SHORT $GWERNNET_DIRS_SUFFIXES" u gwtag gwt t

alias u="upload"
# 'upload' moved to ~/wiki/static/build/upload for easier calling from XMonad
## tab-complete the first argument as the local file, and the second argument as the remote directory:
_upload () {
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

# wait for a file (first argument) to become quiescent because eg. the web browser is still downloading it; optional second argument is the minimum size in kilobytes (eg. archived webpages are almost always >10kb these days, even if it's an old-school all-text HTML page):
is_downloading () {
  file="$1"
  min_size_kb="${2:-0}"  # Default to 0 if not provided
  min_size_bytes=$((min_size_kb * 1024))
  current_time=$(date +%s)

  # Check if the file exists and is non-zero in size
  if [ -s "$file" ]; then
    file_size=$(stat -c %s "$file")
    modified_time=$(stat -c %Y "$file")
    elapsed_time=$((current_time - modified_time))

    # Check if file size meets the minimum requirement
    if [ "$file_size" -lt "$min_size_bytes" ]; then
      sleep 3
      is_downloading "$file" "$min_size_kb"
      return
    fi

    # Sleep if last-modified time is not at least 3 seconds ago
    if [ $elapsed_time -lt 3 ]; then
      sleep $((3 - elapsed_time))
      is_downloading "$file" "$min_size_kb"
    fi
  else
    # File doesn't exist or is zero-sized, wait and check again
    sleep 3
    is_downloading "$file" "$min_size_kb"
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
    # URL case: argument begins with 'http'/'https', which means we are doing a manual insertion of a particular URL into the local-archive as a whole, rather than overwriting an existing snapshot, so we need to update the database once we've injected the new one:
    if [[ "$1" =~ ^https?:// ]]; then
        TARGET_PATH=$(linkArchive.sh --dry-run "$1")

        if [[ -e "$TARGET_PATH" ]]; then
            red "Refusing to overwrite existing $TARGET_PATH for $1. Do it manually if intended."
            return 2
        fi

        # Oldest *.html in $HOME (browser just saved)
        NEW=$(find "$HOME" -maxdepth 1 -name '*.html' -printf '%T@ %p\n' \
              | sort --numeric-sort | cut --delimiter=' ' --fields=2- | head --lines=1)

        if [[ -z "$NEW" ]]; then
            red "No freshly-saved HTML file found in \$HOME to move to $TARGET_PATH."
            return 3
        fi

        (cd ~/wiki/
         mkdir $(dirname "$TARGET_PATH") || true
         mv -- "$NEW" "$TARGET_PATH"
         ghci -istatic/build/ ./static/build/LinkArchive.hs \
              -e "LinkArchive.insertLinkIntoDB (Right (Just \"$TARGET_PATH\")) \"$1\""
        )
        return 0
    fi

    # Check if inotifywait is installed
    if ! command -v inotifywait &> /dev/null; then
        red "inotifywait could not be found. Please install 'inotify-tools' package."
        return 1
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
  if [ ! -f "$DESTINATION" ]; then red "WARNING: destination target $DESTINATION does not exist!"; fi

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
  is_downloading "$SOURCE" 10 # most web pages <10kb are broken or error pages
  bold "$SOURCE" "$(du -ch "$SOURCE")" "$DESTINATION"
  if [[ $(stat -c%s "$SOURCE") -ge 10000000 ]]; then # EXPERIMENTAL: optimize large HTML files (>10MB) by splitting back into separate files to avoid strict downloads
      mv -- "$SOURCE" "$DESTINATION"
      php ~/wiki/static/build/deconstruct_singlefile.php "$DESTINATION"
  else
      mv -- "$SOURCE" "$DESTINATION"
  fi

  echo -n -e '\a'; # ring bell because done
}

# convert a static HTML file into an efficient Gwtar (see </gwtar>)
alias gwtar="php ~/wiki/static/build/deconstruct_singlefile.php --create-gwtar"

# everyNDays: returns a boolean true every _N_ whole-number days in a systematic way based on date modulus; can be used in place of `$RANDOM` calls to do an operation 'randomly' every once in a while, by calling `everyNDays N && ...` or `if everyNDays N; then ...; else; ... fi`.
# (Better than actual randomness because it avoids clumping/starvation, or potentially doing all of the operations on the same run by chance; and far simpler than any explicit tracking of state/date.)
# The optional second whole-number argument is an 'offset' to avoid clumping of multiple calls with the same _N_; they can be offset by relatively-prime numbers or just plain incremented.
everyNDays () {
    (( (($(date +%-j) + ${2:-0}) % $1) == 0 )) # optional offset second argument to stagger or space out multiple calls of _N_ relative to each other
}

# sort a list of directories from stdin by their most recently modified file.
# (We ignore any file not tracked by git, because it may be auto-generated or modified spuriously.
# We treating sub-directories as separate directories, so a directory with a modified file doesn't trigger all its parents as well).
#
# This is useful for prioritizing commands, particularly if they may crash on an error:
# errors (eg. broken links or malformed syntax) will usually be in the most-recently-modified files,
# so processing them first reduces latency to fix.
sort_by_lastmodified () {
    local git_files latest_file timestamp
    while IFS= read -r dir; do
        # Change to the directory
        pushd "$dir" >/dev/null || continue
        # Get list of files not ignored by git
        git_files=$(git ls-files --exclude-standard)
        if [ -n "$git_files" ]; then
            # Find the most recently modified file among git-tracked files
            latest_file=$(git ls-files --exclude-standard -z | xargs -0 stat --format='%m %N' 2>/dev/null | sort --reverse --numeric | head --lines=1)
        else
            # If no git-tracked files, find the most recently modified untracked file
            latest_file=$(find . -type f -not -path './.git/*' -printf '%T@ %p\n' 2>/dev/null | sort --reverse --numeric | head --lines=1)
        fi
        if [ -n "$latest_file" ]; then
            # Extract the timestamp and print it with the directory
            timestamp=$(echo "$latest_file" | cut --delimiter=' ' --fields=1)
            echo "$timestamp $dir"
        else
            # If no files found, use the directory's modification time
            timestamp=$(stat --format='%m' . 2>/dev/null || stat -c '%Y' .)
            echo "$timestamp $dir"
        fi
        # Return to the original directory
        popd >/dev/null
    done | sort --reverse --numeric | cut --delimiter=' ' --fields=2-
}

# KNOWN GOOD / GOLD UNIT-TESTING (experimental)
# the </lorem*> pages exercise most site functionality, and many errors or regressions in new patches should show up as changes in the generated HTML.
# So we can simply compare the new HTML to old stored snapshots to inspect any changes (`run_regression_test`), and if the changes are good, update the snapshots (`lorem_update`).
SITE_URL="https://gwern.net"
SNAPSHOT_DIR="./metadata/snapshot"
CONTENT_DIR="$HOME/wiki/"

get_lorem_pages () {
    find "$CONTENT_DIR" -name "lorem*.md" -printf "%f\n" | sed 's/\.md$//' | sort
}

lorem_download () {
    local page=$1
    local output_file="${SNAPSHOT_DIR}/${page}"
    local temp_file; temp_file=$(mktemp)
    # we version the infrastructure files with a 10-digit seconds-level Unix timestamp to bust caches, and those will change every time those are modified, eg. to add a link-icon, and trigger spurious changes. So we used sed to remove those from both downloaded snapshots & downloaded live pages.
    curl --silent "${SITE_URL}/${page}" |
        sed 's/\.\(css\|js\|svg\)?v=[0-9]\{10\}/.\1/g' >> "$temp_file"
    mv -- "$temp_file" "${output_file}"
}

compare_page () {
    local page=$1
    local snapshot_file="${SNAPSHOT_DIR}/${page}"
    local temp_file; temp_file=$(mktemp)

    curl --silent "${SITE_URL}/${page}" | sed 's/\.\(css\|js\|svg\)?v=[0-9]\{10\}/.\1/g' >> "${temp_file}"

    if ! diff --ignore-space-change --brief "${temp_file}" "${snapshot_file}" &>/dev/null; then
        red "Changes detected in \"${page}\":"
        # Git: "If exactly two paths are given and at least one points outside the current repository, git diff will compare the two files / directories."; this works better for diffing HTML than any invocations of `diff` I found.
        git diff --color-words "${snapshot_file}" "${temp_file}"
    fi

    rm -- "${temp_file}"
}

lorem_update () {
    cd ~/wiki/
    bold "Updating snapshots…"
    mkdir -p "${SNAPSHOT_DIR}"
    get_lorem_pages | while IFS= read -r page; do
        lorem_download "${page}"
        bold "Updated snapshot for \"${page}\""
    done
}

run_gold_test () {
    for page in $(get_lorem_pages); do
        compare_page "${page}"
    done
}

# `swap FILE1 FILE2`: rename FILE1 to FILE2, and FILE2 to FILE1
#
# Exchanges the names of two files by renaming through a temporary file in the
# same directory as FILE1. All three operations are same-filesystem renames
# (directory-entry relinks), so no data is ever copied and no extra disk space
# is consumed.
#
# On partial failure (kill between renames, full directory, etc.) mv prints its
# own error and a '.swap.*' tempfile is left in FILE1's directory holding the
# salvageable content. Remove it once you've recovered.
#
# Does not handle:

# - Files on different filesystems (mktemp will be on FILE1's fs; mv of FILE2
#   across filesystems will fail, leaving a .swap.* tempfile — recoverable)
# - Missing files (mv will error clearly)
# - Broken symlinks (-e is false for them)
#
# A more elaborate version could: verify both files are on the same device
# before starting; handle broken symlinks via -L; clean up the tempfile on
# success while preserving it on failure via EXIT trap state; add -v/--dry-run
# flags; or on Linux ≥3.15 use `renameat2(RENAME_EXCHANGE)` for a true atomic
# single-syscall swap with no tempfile or failure window at all. We omit all of
# this in the name of simplicity.
swap() {
    [[ $# -eq 2 ]] || { printf 'Usage: swap <file1> <file2>\n' >&2; return 1; }
    [[ "$1" -ef "$2" ]] && { printf 'swap: same file\n' >&2; return 1; }

    local tmp
    tmp=$(mktemp -d -- "$(dirname -- "$1")/.swap.XXXXXXXXXX") || return 1

    # Safely clean up the directory on exit, but ONLY if it is empty.
    trap 'rmdir -- "$tmp" 2>/dev/null' RETURN

    command mv -- "$1" "$tmp/val" &&
    command mv -- "$2" "$1"       &&
    command mv -- "$tmp/val" "$2"
}
