<?php
# Image analysis script for deciding whether a JPG/PNG image should have an outline in web pages.
# Based on heuristic analysis of corners & solid colors.
# (SVG default to no-outline. Other image types are not yet handled.)
#
# Called by Image.hs on local images where '.outline'/'.outline-not' classes are not set.
#
# Dependencies: GD Library
# Author: Said Achmiz
# Date: 2024-12-18
# License: CC-0

/** Design rationale:
 *
 * Determines whether an image should have an outline, returning "1" (outline) or "0" (no outline).
 *
 * Logic:
 * - For PNG/JPEG, the script checks the 4 corner pixels:
 * - If corners differ in color, return "1".
 * - If corners share a single color but have any transparency, return "1".
 * - If corners share a single fully opaque color other than pure black (#000000) or pure white (#FFFFFF), return "1".
 * - Otherwise, return "0".
 * - SVG images always return "0".
 *   We do not try to support other raster or vector images; see `Image.isImageFilename`: JPG/PNG/SVG cover almost every image displayed on Gwern.net and the rest are too rare to care about.
 *
 * Rationale:
 * Images with varied/transparent corners often benefit from an outline for better contrast.
 * Uniform black or white corners usually blend well with common page backgrounds.
 *
 * Dependencies: GD Library for PNG/JPEG processing.
 * Usage: $ php should_image_have_outline.php <image_path>
 *
 * Exit codes:
 * 0: Success (outputs "0" (False) or "1" (True), no newline)
 * 1: Invalid argument count
 * 2: Unsupported file extension
 * 3: Image loading failed
 * 4: Uppercase file extension
 * 5: File does not exist or size could not be read.
 * 6: File too small.
 * 7: File too large.
 * 8: Image too small.
 * 9: Image too large.
 * 10: Image uses too much RAM.
 * 11: File cannot be read.
 */

# ---------- CONFIGURATION ----------
$MAX_FILESIZE_BYTES  = 100 * 1024 * 1024; // 100 MB maximum
$MIN_FILESIZE_BYTES  = 500;         // 0.5 KB minimum
$MAX_WIDTH           = 32768;  # Common max texture size
$MIN_WIDTH           = 25;
$MAX_HEIGHT          = 32768;
$MIN_HEIGHT          = 25;

# ---------- MAIN SCRIPT ----------

if ($argc != 2) {
    echo "Fatal error: requires exactly 1 image argument.";
    exit(1);
}

$image_path = $argv[1];
$image = null;

// Check file extension is lowercase
if (preg_match('/[A-Z]/', pathinfo($image_path, PATHINFO_EXTENSION))) {
    echo "Fatal error: Image extensions must be lowercase.";
    exit(4);
}

// Check file existence, readability, & min/max size
if (!file_exists($image_path)) {
    echo "Fatal error: File does not exist.";
    exit(5);
}
if (!is_readable($image_path)) {
    echo "Fatal error: Cannot read file.";
    exit(11);
}
$filesize = filesize($image_path);
if ($filesize === false) {
    echo "Fatal error: Could not read file size.";
    exit(5);
}
if ($filesize < $MIN_FILESIZE_BYTES) {
    echo "Fatal error: File is too small (< 0.5 KB).";
    exit(6);
}
if ($filesize > $MAX_FILESIZE_BYTES) {
    echo "Fatal error: File is too large (> 100 MB).";
    exit(7);
}

// Handle images by extension
if (str_ends_with($image_path, ".svg")) {
    // SVG always "no outline"
    echo "0";
    exit(0);
} elseif (str_ends_with($image_path, ".png")) {
    $image = imageCreateFromPng($image_path);
    if ($image === false) {
        echo "Fatal error: Failed to load PNG image.";
        exit(3);
    }
} elseif (str_ends_with($image_path, ".jpg") || str_ends_with($image_path, ".jpeg")) {
    $image = imageCreateFromJpeg($image_path);
    if ($image === false) {
        echo "Fatal error: Failed to load JPEG image.";
        exit(3);
    }
} else {
    echo "Fatal error: Unsupported file extension (use .jpg/.jpeg/.png/.svg).";
    exit(2);
}

// Check dimensions
$width = imagesx($image);
$height = imagesy($image);
if ($width < $MIN_WIDTH || $height < $MIN_HEIGHT) {
    echo "Fatal error: Image is too small (must be at least 25×25).";
    exit(8);
}
if ($width > $MAX_WIDTH || $height > $MAX_HEIGHT) {
    echo "Fatal error: Image dimensions too large (max 32768×32768).";
    exit(9);
}
if ($width * $height > 268435456) { # 16384×16384
    echo "Fatal error: Image requires too much memory.";
    exit(10);
}

// Examine corner pixels
$corner_colors = [
    imageColorsForIndex($image, imageColorAt($image, 0,          0          )),
    imageColorsForIndex($image, imageColorAt($image, $width - 1, 0          )),
    imageColorsForIndex($image, imageColorAt($image, 0,          $height - 1)),
    imageColorsForIndex($image, imageColorAt($image, $width - 1, $height - 1)),
];
// Clean up
imagedestroy($image);

// Simple corner-based heuristic
if (!are_all_same($corner_colors)) {
    echo "1";
} else if (!is_opaque($corner_colors[0])) {
    echo "1";
} else if (is_white($corner_colors[0]) || is_black($corner_colors[0])) {
    echo "0";
} else {
    echo "1";
}
exit(0);

# ---------- HELPER FUNCTIONS ----------

function is_opaque($color) {
    return ($color["alpha"] == 0);
}

function is_white($color) {
    return (is_grey($color) && $color["red"] == 255);
}

function is_black($color) {
    return (is_grey($color) && $color["red"] == 0);
}

function is_grey($color) {
    return ($color["red"] == $color["green"] && $color["red"] == $color["blue"]);
}

function are_all_same($colors) {
    for ($i = 1; $i < count($colors); $i++) {
        if (!are_same($colors[0], $colors[$i])) {
            return false;
        }
    }
    return true;
}

function are_same($color_a, $color_b) {
    return ($color_a["red"]   == $color_b["red"]
         && $color_a["green"] == $color_b["green"]
         && $color_a["blue"]  == $color_b["blue"]
         && $color_a["alpha"] == $color_b["alpha"]);
}
?>
