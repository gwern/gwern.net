<?php

echo "Versioning assets...\n";

$static_root = __DIR__ . "/..";

$include_dir = "{$static_root}/include";
$icon_dir = "{$static_root}/img/icon";

$patterns = [
	"{$icon_dir}/icons.svg"
];

$file_paths = [ ];
foreach ($patterns as $pattern)
	$file_paths = array_merge($file_paths, glob($pattern));

$output = "<script>\n"
		. "GW.assetVersions = {\n";

$assets = [ ];
foreach ($file_paths as $file_path) {
	$assets[substr($file_path, strlen($static_root))] = filemtime($file_path);
}

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

file_put_contents("{$include_dir}/inlined-asset-versions.html", $output);

$files = [
	"{$static_root}/css/head-GENERATED.css",
	"{$static_root}/css/style-GENERATED.css"
];
foreach ($files as $file_path) {
	$file = file_get_contents($file_path);

	foreach ($assets as $url_path => $file_mod_time) {
		$file = str_replace($url_path, "{$url_path}?v={$file_mod_time}", $file);
	}

	file_put_contents(str_replace('-GENERATED', '-VERSIONED', $file_path), $file);
}

?>
