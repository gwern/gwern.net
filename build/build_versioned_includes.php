<?php

echo "Building versioned includes...\n";

/*
 Instructions:

 	php build_versioned_includes.php

 In template or page:

 	<!--#include virtual="/static/include/inlined-foot.html"-->
 	<!--#include virtual="/static/include/inlined-fonts.html"-->

 (Pages must be .shtml (not .html) for this to work.)

 In .htaccess (if using Apache):

	Options +Includes
	AddType text/html .shtml
	AddOutputFilter INCLUDES .shtml

 (Configuration for non-Apache servers left as exercise for reader.)
 */

$static_root = __DIR__ . '/..';

$files = [
	'inlined-foot',
	'inlined-fonts'
];

foreach ($files as $file) {
	$template_file_path = "{$static_root}/template/{$file}-template.html";
	$versioned_file_path = "{$static_root}/include/{$file}.html";

	$infile = file_exists($template_file_path)
			  ? file_get_contents($template_file_path)
			  : file_get_contents($versioned_file_path);

	$outfile = preg_replace_callback('/([\'"])\/static\/(.+?)(\.[^\.\/]+?)(\?v=[0-9]+)?\1/i', 'VersionAssetHref', $infile);

	file_put_contents($versioned_file_path, $outfile);
}

## FUNCTIONS

function VersionAssetHref($m) {
	global $static_root;

	$file_name = $m[2];
	$file_extension = $m[3];

	$possible_file_paths = array_map(function ($suffix) use ($static_root, $file_name, $file_extension) {
		return "{$static_root}/{$file_name}{$suffix}{$file_extension}";
	}, [ 'VERSIONED', '-GENERATED', '' ]);
	foreach ($possible_file_paths as $file_path) {
		if (file_exists($file_path)) {
			$file_mod_time = filemtime($file_path);
			return "\"/static/{$file_name}{$file_extension}?v={$file_mod_time}\"";
		}
	}

	die('FILE NOT FOUND: ' . "{$static_root}/{$file_name}{$file_extension}\n");
}

?>
