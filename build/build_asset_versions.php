<?php

echo "Building asset versions...\n";

require_once(__DIR__ . '/build_paths.php');
require_once(__DIR__ . '/build_variables.php');

global $static_root, $icon_dir, $logo_dir, $font_dir, $js_dir;

$asset_file_paths = [
	"{$icon_dir}/icons.svg"
];
$asset_patterns = [
	"{$logo_dir}/*/*-small-*.png",
	"{$logo_dir}/*/*/*-small-*.png",
	"{$logo_dir}/*/*.svg",
	"{$logo_dir}/*/*/*.svg",

	"{$font_dir}/dropcap/*/*-small-*.png",
	"{$font_dir}/dropcap/*/*/*-small-*.png",
	"{$font_dir}/dropcap/*/*.svg",
	"{$font_dir}/dropcap/*/*/*.svg"
];
foreach ($asset_patterns as $pattern) {
	$asset_file_paths = array_merge($asset_file_paths, glob($pattern));
}
$assets = [ ];
foreach ($asset_file_paths as $asset_file_path) {
	$assets[substr($asset_file_path, strlen($static_root))] = filemtime($asset_file_path);
}

$output = "GW.assetVersions = {\n";

$output_lines = [ ];
foreach ($assets as $url_path => $file_mod_time) {
	$output_lines[] = (  "\t"
						. '"'
						. '/static'
						. $url_path
						. '": "'
						. $file_mod_time
						. '"');
}

$output .= implode(",\n", $output_lines);
$output .= "\n};\n";

$asset_versions_file_path = "{$js_dir}/asset-versions-GENERATED.js";
file_put_contents($asset_versions_file_path, $output);
$updated_files[] = $asset_versions_file_path;

?>
