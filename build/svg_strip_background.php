<?php

## Usage:
##   svg_strip_background.php foo.svg bar.svg baz.svg

$svg_tag_regexp = '/<svg ([^<>]* )?width="(.+?)" ([^<>]* )?height="(.+?)"[^<>]*>/';

$file_names = array_slice($argv, 1);
foreach ($file_names as $file_name) {
	echo $file_name . "\n";

	$svg = file_get_contents($file_name);

	preg_match($svg_tag_regexp, $svg, $m);
	$width = $m[2];
	$height = $m[4];

	$path_regexp = '/(<path[^<>]*d="' 
				 . "M-0 -0L{$width} 0L{$width} {$height}L-0 {$height}L0 -0Z" 
				 . '"[^<>]*>)/';
// 	$path_regexp = '/(<path[^<>]*d="'
// 				 . "M0 0h{$width}v{$height}H0z"
// 				 . '"[^<>]*>)/';
	preg_match($path_regexp, $svg, $m);
	
	$svg = preg_replace($path_regexp, '', $svg);

	file_put_contents($file_name, $svg);
}
