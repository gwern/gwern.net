<?php

echo "Building versioned includes...\n";

require_once(__DIR__ . '/build_paths.php');
require_once(__DIR__ . '/build_variables.php');
require_once(__DIR__ . '/build_functions.php');

global $static_root, $include_dir;

/*
 Instructions:

 	php build_versioned_includes.php

 In template or page:

 	<!--#include virtual="/static/include/inlined-asset-links.html"-->

 In .htaccess (if using Apache):

	Options +Includes
	AddType text/html .shtml
	AddOutputFilter INCLUDES .shtml

 (NOTE: Pages must be .shtml (not .html) for this to work, with Apache.)

 (Configuration for non-Apache servers left as exercise for reader.)
 */

## Includes

$includes = [
	'<link rel="stylesheet" href="/static/css/style.css" media="print" onload="this.media=`all`">', 
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

$inlined_asset_links_file_path = "{$include_dir}/inlined-asset-links.html";
file_put_contents($inlined_asset_links_file_path, $outfile);
$updated_files[] = $inlined_asset_links_file_path;

?>
