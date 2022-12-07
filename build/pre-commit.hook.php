#!/usr/bin/env php
<?php

$force = @$argv[1] == "--force";

$build_dir = __DIR__;
$static_dir = "{$build_dir}/..";

## Components of generated/assembled inlined style sheets.
$css_components = [
	"{$static_dir}/css/include/colors.css",
	"{$static_dir}/css/include/components/dark-mode-adjustments.css"
];
$css_components = implode(" ", $css_components);
if ($force || (`git diff-index --cached HEAD -- {$css_components}`)) {
	require_once("{$build_dir}/build_css.php");
	`git add {$static_dir}/css/.`;
}

## Inlined styles and scripts.
$head_includes = [
	"{$static_dir}/css/include/colors.css",
	"{$static_dir}/css/include/dark-mode-GENERATED.css",
	"{$static_dir}/css/include/initial.css",
	"{$static_dir}/js/dark-mode-inline.js",
	"{$static_dir}/js/gw-inline.js",
	"{$static_dir}/js/reader-mode-inline.js",
];
$head_includes = implode(" ", $head_includes);
if ($force || (`git diff-index --cached HEAD -- {$head_includes}`)) {
	require_once("{$build_dir}/build_head_includes.php");
	`git add {$static_dir}/includes/.`;
}

## Font spec.
$font_spec = "{$static_dir}/font/font_spec.php";
if ($force || (`git diff-index --cached HEAD -- {$font_spec}`)) {
	require_once("{$build_dir}/build_font_css.php");
	`git add {$static_dir}/css/.`;
}

## Fonts and font CSS.
$fonts_and_font_css = [
	"{$static_dir}/css/fonts.css"
];
$font_path_patterns = [
	"{$static_dir}/font/*/*.otf",
	"{$static_dir}/font/*/*.ttf",
	"{$static_dir}/font/*/*/*.otf",
	"{$static_dir}/font/*/*/*.ttf"
];
foreach ($font_path_patterns as $pattern) {
	$fonts_and_font_css = array_merge($fonts_and_font_css, glob($pattern));
}
$fonts_and_font_css = implode(" ", $fonts_and_font_css);
if ($force || (`git diff-index --cached HEAD -- {$fonts_and_font_css}`)) {
	require_once("{$build_dir}/build_versioned_font_css.php");
	`touch {$static_dir}/css/fonts.css`;
	`git add {$static_dir}/css/.`;
}

## External styles and scripts.
$versioned_files = [
	"{$static_dir}/css/default.css",
	"{$static_dir}/css/fonts.css",
	"{$static_dir}/css/links.css",
	"{$static_dir}/js/annotations.js",
	"{$static_dir}/js/collapse.js",
	"{$static_dir}/js/content.js",
	"{$static_dir}/js/dark-mode.js",
	"{$static_dir}/js/extracts.js",
	"{$static_dir}/js/extracts-annotations.js",
	"{$static_dir}/js/extracts-content.js",
	"{$static_dir}/js/extracts-options.js",
	"{$static_dir}/js/image-focus.js",
	"{$static_dir}/js/popins.js",
	"{$static_dir}/js/popups.js",
	"{$static_dir}/js/reader-mode.js",
	"{$static_dir}/js/rewrite.js",
	"{$static_dir}/js/sidenotes.js",
	"{$static_dir}/js/tablesorter.js",
	"{$static_dir}/js/transclude.js",
	"{$static_dir}/js/typography.js",
	"{$static_dir}/js/utility.js",
	"{$static_dir}/templates/inlined-fonts-template.html",
	"{$static_dir}/templates/inlined-foot-template.html"
];
$versioned_files = implode(" ", $versioned_files);
if ($force || (`git diff-index --cached HEAD -- {$versioned_files}`)) {
	require_once("{$build_dir}/build_versioned_includes.php");
	`git add {$static_dir}/includes/.`;
}

## Templates and other assets.
$versioned_assets = [ ];
$versioned_asset_patterns = [
	"{$static_dir}/templates/include/*.tmpl",
	"{$static_dir}/templates/include/templates.json",
];
foreach ($versioned_asset_patterns as $pattern)
	$versioned_assets = array_merge($versioned_assets, glob($pattern));
$versioned_assets = implode(" ", $versioned_assets);
if ($force || (`git diff-index --cached HEAD -- {$versioned_assets}`)) {
	require_once("{$build_dir}/build_asset_versions.php");
	`git add {$static_dir}/includes/.`;
}

?>
