<?php

if ($argc < 2)
	die;

$image_path = $argv[1];
$image = null;
if (str_ends_with($image_path, ".png"))
	$image = imageCreateFromPng($image_path);
else if (str_ends_with($image_path, ".jpg"))
	$image = imageCreateFromJpeg($image_path);
else
	die;
$width = imagesx($image);
$height = imagesy($image);

$corner_colors = [
	imageColorsForIndex($image, imageColorAt($image, 0,          0          )),
	imageColorsForIndex($image, imageColorAt($image, $width - 1, 0          )),
	imageColorsForIndex($image, imageColorAt($image, 0,          $height - 1)),
	imageColorsForIndex($image, imageColorAt($image, $width - 1, $height - 1)),	
];

$colors_are_all_same = are_all_same($corner_colors);
if (are_all_same($corner_colors) == false) {
	echo "1";
} else if (is_opaque($corner_colors[0]) == false) {
	echo "1";
} else if (   is_white($corner_colors[0])
		   || is_black($corner_colors[0])) {
	echo "0";
} else {
	echo "1";
}

echo "\n";
die;

## FUNCTIONS

function is_opaque($color) {
	return ($color["alpha"] == 0);
}

function is_white($color) {
	return (   is_grey($color)
			&& $color["red"] == 255);
}

function is_black($color) {
	return (   is_grey($color)
			&& $color["red"] == 0);
}

function is_grey($color) {
	return (   $color["red"] == $color["green"]
			&& $color["red"] == $color["blue"]);
}

function are_all_same($colors) {
	for ($i = 1; $i < count($colors); $i++)
		if (are_same($colors[0], $colors[$i]) == false)
			return false;

	return true;
}

function are_same($color_a, $color_b) {
	return (   $color_a["red"]   == $color_b["red"]
			&& $color_a["green"] == $color_b["green"]
			&& $color_a["blue"]  == $color_b["blue"]
			&& $color_a["alpha"] == $color_b["alpha"]);
}

?>