#!/usr/bin/env php
<?php

$force = @$argv[1] == "--force";

$build_dir = __DIR__;
$static_dir = "{$build_dir}/..";

$css_components = [
	"{$static_dir}/css/include/colors.css",
	"{$static_dir}/css/include/components/dark-mode-adjustments.css"
];
$css_components = implode(" ", $css_components);
if ($force || (`git diff-index --cached HEAD -- {$css_components}`)) {
	require_once("{$build_dir}/build_css.php");
	`git add {$static_dir}/css/.`;
}

$head_includes = [
	"{$static_dir}/css/include/colors.css",
	"{$static_dir}/css/include/dark-mode-GENERATED.css",
	"{$static_dir}/css/include/initial.css",
	"{$static_dir}/js/darkmode-inline.js",
	"{$static_dir}/js/gw-inline.js"
];
$head_includes = implode(" ", $head_includes);
if ($force || (`git diff-index --cached HEAD -- {$head_includes}`)) {
	require_once("{$build_dir}/build_head_includes.php");
	`git add {$static_dir}/includes/.`;
}

$versioned_files = [
	"{$static_dir}/css/default.css",
	"{$static_dir}/css/fonts.css",
	"{$static_dir}/css/links.css",
	"{$static_dir}/js/annotations.js",
	"{$static_dir}/js/collapse.js",
	"{$static_dir}/js/darkmode.js",
	"{$static_dir}/js/extracts.js",
	"{$static_dir}/js/extracts-annotations.js",
	"{$static_dir}/js/extracts-content.js",
	"{$static_dir}/js/extracts-options.js",
	"{$static_dir}/js/image-focus.js",
	"{$static_dir}/js/popins.js",
	"{$static_dir}/js/popups.js",
	"{$static_dir}/js/rewrite.js",
// 	"{$static_dir}/js/sidenotes.js",
	"{$static_dir}/js/tablesorter.js",
	"{$static_dir}/js/typography.js",
	"{$static_dir}/templates/inlined-fonts-template.html",
	"{$static_dir}/templates/inlined-foot-template.html"
];
$versioned_files = implode(" ", $versioned_files);
if ($force || (`git diff-index --cached HEAD -- {$versioned_files}`)) {
	require_once("{$build_dir}/build_versioned_includes.php");
	`git add {$static_dir}/includes/.`;
}

?>
