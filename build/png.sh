#!/bin/bash
set -euo pipefail

# do a mix of lossless then lossy optimization:
pngOne () {
    if [[ "$1" =~ .*\.png ]]; then
        local orig_size new_size
        orig_size=$(stat --printf="%s" "$1") # in bytes
        temp_file=$(mktemp --suffix=.png)
        cp "$1" "$temp_file"
         # WARNING: can't overwrite on Ubuntu as pngnq is buggy and will simply delete the file! so create lossy version and overwrite manually:
        nice -n 19 pngnq -v -s1 "$temp_file"
        optimized_file="${temp_file%.*}"-nq8.png
        if [ -f "$optimized_file" ]; then
            nice -n 19 advpng --shrink-insane --iter 30 --recompress "$optimized_file"
            nice -n 19 optipng -o9 "$optimized_file"
            new_size=$(stat --printf="%s" "$optimized_file")
            # calculate the percentage difference
            diff=$(echo "scale=2; 100*(($orig_size - $new_size)/$orig_size)" | bc)
            # only replace if new file is at least 10% smaller
            if (( $(echo "$diff >= 10" | bc -l) )); then
                mv "$optimized_file" "$1";
            else
                echo "Optimized file of $1 is not at least 10% smaller ($orig_size → $new_size ; reduction: $diff%). Leaving the original file untouched."
            fi
        else
            echo "Optimized file $optimized_file doesn't exist. Leaving the original file untouched."
        fi
        rm -f "$temp_file" "$optimized_file"
    else
        echo "File $1 is not a PNG. Skipping."
    fi
}
export -f pngOne

for item in "$@"; do
    if [ -f "$item" ]; then
        pngOne "$item" &
    elif [ -d "$item" ]; then
        find "$item" -type f -name "*.png" | sort | parallel pngOne
    fi
done
