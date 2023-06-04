<?php

echo "Building <head> includes...\n";

require_once(__DIR__ . '/build_functions.php');

/*
 Instructions:

 	php build_head_includes.php

 In <head> of template or page:

 	<!--#include virtual="/static/include/inlined-head.html"-->

 (Pages must be .shtml (not .html) for this to work.)

 In .htaccess (if using Apache):

	Options +Includes
	AddType text/html .shtml
	AddOutputFilter INCLUDES .shtml

 (Configuration for non-Apache servers left as exercise for reader.)
 */

## Paths

$static_root = __DIR__ . "/..";
$js_dir = "{$static_root}/js";
$css_dir = "{$static_root}/css";
$includes_dir = "{$static_root}/include";

## Includes

$includes = [
	[ 'light-mode-GENERATED.css', 'id="inlined-styles-colors"' ],
	[ 'dark-mode-GENERATED.css', 'id="inlined-styles-colors-dark" media="all and (prefers-color-scheme: dark)"' ],
	[ '<link id="initial-styles" rel="stylesheet" type="text/css" href="/static/css/head.css">' ],
	[ '<script src="/static/js/head.js"></script>' ]
];

## Action

$outfile = "";
foreach ($includes as $include) {
	$file = $include[0];
	$attributes = @$include[1];

	$type = '';
	if (preg_match('/\.js$/', $file))
		$type = 'script';
	else if (preg_match('/\.css$/', $file))
		$type = 'style';
	else if (preg_match('/(src|href)=/', $file))
		$type = 'external';
	else
		continue;

	$file_path;
	switch ($type) {
		case 'script':
			$file_path = "{$js_dir}/{$file}";
			break;
		case 'style':
			$file_path = "{$css_dir}/{$file}";
			break;
		case 'external':
			preg_match('/(src|href)="\/static\/(.+?)"/', $file, $m);
			$file_path = "{$static_root}/{$m[2]}";
			break;
		default:
			break;
	}

	if ($type == 'external') {
		preg_match('/"\/static\/(.+?)(\.[^\.\/]+?)"/i', $file, $m);
		$href = VersionedAssetHref($m[1], $m[2]);
		$outfile .= preg_replace('/(src|href)="(.+?)"/', "$1={$href}", $file) . "\n";
	} else {
		$outfile .= "<{$type}" . ($attributes ? " {$attributes}" : "") . ">\n";
		$outfile .= file_get_contents($file_path);
		$outfile .= "</{$type}>\n";
	}
}
file_put_contents("{$includes_dir}/inlined-head.html", $outfile);

?>
