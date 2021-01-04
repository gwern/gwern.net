<?php

/*
 Instructions:
 
 	php build_versioned_includes.php
 
 In template or page:
 
 	<!--#include virtual="/static/includes/inlined-foot.html"-->

 (Pages must be .shtml (not .html) for this to work.)
 
 In .htaccess (if using Apache):
 
	Options +Includes
	AddType text/html .shtml
	AddOutputFilter INCLUDES .shtml

 (Configuration for non-Apache servers left as exercise for reader.)
 */
 
$static_root = __DIR__ . "/..";

$infile = file_get_contents("{$static_root}/templates/inlined-foot-template.html");

$outfile = preg_replace_callback('/"\/static\/(.+?)"/i', 'VersionAssetHref', $infile);

file_put_contents("{$static_root}/includes/inlined-foot.html", $outfile);

function VersionAssetHref($m) {
	global $static_root;

	$file_path = "{$static_root}/{$m[1]}";
	$file_mod_time = filemtime($file_path);

	return "\"/static/{$m[1]}?v={$file_mod_time}\"";
}

?>
