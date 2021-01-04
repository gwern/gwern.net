<?php

/*
 Instructions:
 
 	php build_head_includes.php
 
 In <head> of template or page:
 
 	<!--#include virtual="/static/includes/inlined-head.html"-->

 (Pages must be .shtml (not .html) for this to work.)
 
 In .htaccess (if using Apache):
 
	Options +Includes
	AddType text/html .shtml
	AddOutputFilter INCLUDES .shtml

 (Configuration for non-Apache servers left as exercise for reader.)
 */

$includes = [
	[ 'colors.css', 'id="inlined-styles-colors"' ],
	[ 'initial.css', 'id="inlined-styles"' ],
	[ 'dark-mode.css', 'id="inlined-dark-mode-styles" media="all and (prefers-color-scheme: dark)"' ],
	[ 'gw-inline.js' ],
	[ 'darkmode-inline.js' ],
];

$outfile = "";

foreach ($includes as $include) {
	$file_name = $include[0];
	$attributes = @$include[1];

	$type = "";
	if (preg_match('/\.js$/', $file_name))
		$type = "script";
	else if (preg_match('/\.css$/', $file_name))
		$type = "style";
	else
		continue;

	$dir_prefix;
	switch ($type) {
		case "script":
			$dir_prefix = __DIR__ . "/../js/";
			break;
		case "style":
			$dir_prefix = __DIR__ . "/../css/";
			break;
		default:
			break;
	}
	
	$outfile .= "<{$type}" . ($attributes ? " {$attributes}" : "") . ">\n";
	$outfile .= file_get_contents($dir_prefix . $file_name);
	$outfile .= "</{$type}>\n";
}

$includes_dir = __DIR__ . "/../includes";

file_put_contents("{$includes_dir}/inlined-head.html", $outfile);

$outfile = str_replace('$', '$$', $outfile);

file_put_contents("{$includes_dir}/inlined-head-escaped.html", $outfile);

?>
