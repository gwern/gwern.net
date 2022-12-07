<?php

$static_root = __DIR__ . "/..";
$include_dir = "{$static_root}/includes";
$include_templates_dir = "{$static_root}/templates/include";

$patterns = [
	"{$include_templates_dir}/*.tmpl",
	"{$include_templates_dir}/templates.json",
];

$paths = [ ];
foreach ($patterns as $pattern)
	$paths = array_merge($paths, glob($pattern));

$output = "<script>\n"
		. "GW.assetVersions = {\n";

$output .= implode(",\n", array_map(function ($path) use ($static_root) {
	return (  "\t"
			. '"'
			. '/static'
			. substr($path, strlen($static_root))
			. '": "'
			. filemtime($path)
			. '"');
}, $paths));

$output .= "\n};\n";
$output .= "</script>\n";

file_put_contents("{$include_dir}/inlined-asset-versions.html", $output);

?>
