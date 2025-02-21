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

if ($argc != 2) {
    echo "Fatal error: 'should_image_have_outline.php' requires exactly 1 image argument.";
    exit(1);
}

$image_path = $argv[1];
$image = null;

// Verify lowercase extension
if (preg_match('/[A-Z]/', pathinfo($image_path, PATHINFO_EXTENSION))) {
    echo "Fatal error: Image extensions must be lowercase.";
    exit(4);
}

if (str_ends_with($image_path, ".svg")) {
    echo "0";
    exit(0);
} else if (str_ends_with($image_path, ".png")) {
    // from 'GD Library'; `sudo apt-get install php-gd`:
    $image = imageCreateFromPng($image_path);
    if ($image === false) {
        echo "Fatal error: Failed to load PNG image";
        exit(3);
    }
} else if (str_ends_with($image_path, ".jpg") || str_ends_with($image_path, ".jpeg")) {
    $image = imageCreateFromJpeg($image_path);
    if ($image === false) {
        echo "Fatal error: Failed to load JPEG image";
        exit(3);
    }
} else {
    echo "Fatal error: Argument not recognized as having image JPG/PNG/SVG image extension.";
    exit(2);
}

$width = imagesx($image);
$height = imagesy($image);
$corner_colors = [
    imageColorsForIndex($image, imageColorAt($image, 0,          0          )),
    imageColorsForIndex($image, imageColorAt($image, $width - 1, 0          )),
    imageColorsForIndex($image, imageColorAt($image, 0,          $height - 1)),
    imageColorsForIndex($image, imageColorAt($image, $width - 1, $height - 1)),
];

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

## FUNCTIONS
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
