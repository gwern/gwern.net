<?php

$static_root = __DIR__ . '/..';
$icon_dir = "{$static_root}/img/icon";
$icon_file_path = "{$icon_dir}/icons.svg";

$inlined_head_file_path = "{$static_root}/include/inlined-head.html";

$iconSpacingFactor = 1.1;

## If the SSI <head> include file isn’t already built, build it.
if (!file_exists($inlined_head_file_path))
	require_once("{$static_root}/build/build_head_includes.php");

##	Delete existing icon file.
if (file_exists($icon_file_path))
	unlink($icon_file_path);

$icon_file_paths = glob("{$icon_dir}/*.svg");

$out = [ '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">' ];

$position = 0;
foreach ($icon_file_paths as $path) {
	$icon = preg_replace('/>\s+</', '><', file_get_contents($path));

	preg_match('/([^\/]+)\.svg$/', $path, $m);
	$icon_file_name = $m[1];

	preg_match('/<svg (.+?)>(.+)<\/svg>/', $icon, $m);
	$svg_attributes = $m[1];
	$svg_contents =  $m[2];

	preg_match_all('/(\S+?)="(.+?)"/', $svg_attributes, $m, PREG_SET_ORDER);
	$viewBox = '';
	$icon_attributes = implode(' ', array_map(function ($match) {
										return "{$match[1]}=\"{$match[2]}\"";
									}, array_filter($m,
													function ($match) use (&$viewBox) { 
														if ($match[1] == 'viewBox')
															$viewBox = $match[2];

												 		return (in_array($match[1], [ 
																			 'viewBox', 
																			 'xmlns', 
																			 'xmlns:xlink'
																		 ]) == false); 
												 	})));

	$viewBox = explode(' ', $viewBox);

	$translateX = $position - floatval($viewBox[0]);
	$transform = "translate({$translateX}, 0)";
	$viewBox[0] = $position;
	$position += floatval($viewBox[2]) * $iconSpacingFactor;

	$viewBox = implode(' ', $viewBox);
	$out[] = "<view id=\"{$icon_file_name}\" viewBox=\"{$viewBox}\" preserveAspectRatio=\"xMinYMin\" />";

	$out[] = "<g" 
		   . ($icon_attributes ? ' ' : '') 
		   . $icon_attributes 
		   . " transform=\"{$transform}\""
		   . '>' 
		   . $svg_contents 
		   . "</g>";
}

$out[] = '</svg>';

## Write out icon sprite file.
file_put_contents($icon_file_path, implode("\n", $out) . "\n");

## Append preload link to <head> includes file.
$icon_file_pathname = '/static/img/icon/icons.svg';
file_put_contents($inlined_head_file_path,
				  trim(preg_replace('/<link rel="preload" href=".+?\/icons.svg" .+?>/', '', file_get_contents($inlined_head_file_path)))
				  . "\n"
				  . "<link rel=\"preload\" href=\"{$icon_file_pathname}\" as=\"image\">"
				  . "\n");

?>
