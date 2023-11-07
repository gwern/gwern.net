<?php

echo "Building standalone page includes...\n";

require_once(__DIR__ . '/build_paths.php');
require_once(__DIR__ . '/build_variables.php');
require_once(__DIR__ . '/build_functions.php');

global $static_root, $css_dir, $include_dir;

/*
 Instructions:

 	php build_standalone_includes.php

 In <head> of template or page:

 	<!--#include virtual="/static/include/inlined-standalone.html"-->

 (Pages must be .shtml (not .html) for this to work.)

 In .htaccess (if using Apache):

	Options +Includes
	AddType text/html .shtml
	AddOutputFilter INCLUDES .shtml

 (Configuration for non-Apache servers left as exercise for reader.)
 */

## Includes

$includes = [
	[ 'light-mode-GENERATED.css', 'id="inlined-styles-colors"' ],

	[ '<link rel="stylesheet" href="/static/css/head.css">' ],
	[ '<link rel="stylesheet" href="/static/css/style.css">' ]
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

$inlined_head_file_path = "{$include_dir}/inlined-standalone.html";
file_put_contents($inlined_head_file_path, $outfile);
$updated_files[] = $inlined_head_file_path;

?>
