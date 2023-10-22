<?php

echo "Building asset versions...\n";

require_once(__DIR__ . '/build_paths.php');
require_once(__DIR__ . '/build_variables.php');

$asset_file_paths = [
	"{$static_root}/img/icon/icons.svg"
];
$assets = [ ];
foreach ($asset_file_paths as $asset_file_path) {
	$assets[substr($asset_file_path, strlen($static_root))] = filemtime($asset_file_path);
}

$output = "<script>\n"
		. "GW.assetVersions = {\n";

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
$output .= "</script>\n";

$asset_versions_file_path = "{$js_dir}/asset-versions-GENERATED.js";
file_put_contents($asset_versions_file_path, $output);
$updated_files[] = $asset_versions_file_path;

?>
