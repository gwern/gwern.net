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

## Paths

$static_root = __DIR__ . '/..';
$js_dir = "{$static_root}/js";
$css_dir = "{$static_root}/css";
$includes_dir = "{$static_root}/include";

## Includes

$includes = [
	'<link rel="stylesheet" type="text/css" href="/static/css/style.css">',
	'<script src="/static/js/script.js" defer></script>'
];

## Action

$outfile = "";
foreach ($includes as $file) {
	preg_match('/(src|href)="\/static\/(.+?)(\.[^\.\/]+?)"/i', $file, $m);
	$file_path = "{$static_root}/{$m[2]}{$m[3]}";
	$href = VersionedAssetHref($m[2], $m[3]);

	$outfile .= preg_replace('/(src|href)="(.+?)"/', "$1={$href}", $file) . "\n";
}
file_put_contents("{$includes_dir}/inlined-asset-links.html", $outfile);

?>
