<?php

echo "Building versioned includes...\n";

require_once(__DIR__ . '/build_functions.php');

/*
 Instructions:

 	php build_versioned_includes.php

 In template or page:

 	<!--#include virtual="/static/include/inlined-foot.html"-->

 In .htaccess (if using Apache):

	Options +Includes
	AddType text/html .shtml
	AddOutputFilter INCLUDES .shtml

 (NOTE: Pages must be .shtml (not .html) for this to work, with Apache.)

 (Configuration for non-Apache servers left as exercise for reader.)
 */

$static_root = __DIR__ . '/..';

$files = [
	'inlined-asset-links'
];

foreach ($files as $file) {
	$template_file_path = "{$static_root}/template/{$file}-template.html";
	$versioned_file_path = "{$static_root}/include/{$file}.html";

	$infile = file_exists($template_file_path)
			  ? file_get_contents($template_file_path)
			  : file_get_contents($versioned_file_path);

	$outfile = preg_replace_callback('/([\'"])\/static\/(.+?)(\.[^\.\/]+?)(\?v=[0-9]+)?\1/i', function ($m) {
		return VersionedAssetHref($m[2], $m[3]);
	}, $infile);

	file_put_contents($versioned_file_path, $outfile);
}

?>
