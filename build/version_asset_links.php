<?php

echo "Versioning assets links...\n";

require_once(__DIR__ . '/build_paths.php');
require_once(__DIR__ . '/build_variables.php');

global $static_root, $css_dir, $icon_dir;

$asset_file_paths = [
	"{$icon_dir}/icons.svg"
];
$assets = [ ];
foreach ($asset_file_paths as $asset_file_path) {
	$assets[substr($asset_file_path, strlen($static_root))] = filemtime($asset_file_path);
}

$files = [
	"{$css_dir}/head-GENERATED.css",
	"{$css_dir}/style-GENERATED.css"
];
foreach ($files as $file_path) {
	$file = file_get_contents($file_path);

	foreach ($assets as $url_path => $file_mod_time) {
		$file = str_replace($url_path, "{$url_path}?v={$file_mod_time}", $file);
	}

	$versioned_file_path = str_replace('-GENERATED', '-VERSIONED', $file_path);
	file_put_contents($versioned_file_path, $file);
	$updated_files[] = $versioned_file_path;
}

?>
