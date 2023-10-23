<?php

echo "Building versioned font CSS...\n";

require_once(__DIR__ . '/build_paths.php');
require_once(__DIR__ . '/build_variables.php');

global $static_root, $css_dir;

## Files

$font_files = [
	'fonts',
	'initial-fonts'
];

## Action

foreach ($font_files as $file) {
	$font_css = file_get_contents("{$css_dir}/{$file}-GENERATED.css");

	$versioned_font_css = preg_replace_callback('/\'\/static\/(.+?)\'/i', 'VersionAssetURL', $font_css);

	$file_path = "{$css_dir}/{$file}-VERSIONED.css";
	file_put_contents($file_path, $versioned_font_css);
	$updated_files[] = $file_path;
}

## FUNCTIONS

function VersionAssetURL($m) {
	global $static_root;

	$file_path = "{$static_root}/{$m[1]}";
	$file_mod_time = filemtime($file_path);

	return "'/static/{$m[1]}?v={$file_mod_time}'";
}

?>
