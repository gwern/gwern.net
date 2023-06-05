#!/usr/bin/env php
<?php

$force = @$argv[1] == "--force";

$build_dir = __DIR__;
$static_dir = "{$build_dir}/..";

## Fonts and font CSS.
## Build the font CSS from the font spec.
$fonts_and_font_css = [
	"{$static_dir}/font/font_spec.php"
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
	require_once("{$build_dir}/build_font_css.php");
	require_once("{$build_dir}/build_versioned_font_css.php");
	`git add {$static_dir}/css/.`;
}

## Components of color (light/dark) inlined CSS.
## Build the inlined color CSS (for light and dark mode).
$css_components = [
	"{$static_dir}/css/colors.css",
	"{$static_dir}/css/light-mode-adjustments.css",
	"{$static_dir}/css/dark-mode-adjustments.css"
];
$css_components = implode(" ", $css_components);
if ($force || (`git diff-index --cached HEAD -- {$css_components}`)) {
	require_once("{$build_dir}/build_css.php");
	`git add {$static_dir}/css/.`;
}

## Unified assets (JS & CSS).
## Assemble all the .css and .js files into head.css/style.css and 
## head.js/script.js, respectively.
$disparate_assets = [
	'{$static_dir}/js/utility.js',
	'{$static_dir}/js/initial.js',
	'{$static_dir}/js/layout.js',
	'{$static_dir}/js/dark-mode-initial.js',
	'{$static_dir}/js/reader-mode-initial.js',

	'{$static_dir}/css/initial.css',
	'{$static_dir}/css/initial-fonts-VERSIONED.css',
	'{$static_dir}/css/reader-mode-initial.css',

	'{$static_dir}/js/misc.js',
	'{$static_dir}/js/popups.js',
	'{$static_dir}/js/popins.js',
	'{$static_dir}/js/annotations.js',
	'{$static_dir}/js/content.js',
	'{$static_dir}/js/transclude.js',
	'{$static_dir}/js/extracts.js',
	'{$static_dir}/js/extracts-annotations.js',
	'{$static_dir}/js/extracts-content.js',
	'{$static_dir}/js/extracts-options.js',
	'{$static_dir}/js/extracts-load.js',
	'{$static_dir}/js/typography.js',
	'{$static_dir}/js/Hyphenopoly_Loader.js',
	'{$static_dir}/js/rewrite.js',
	'{$static_dir}/js/collapse.js',
	'{$static_dir}/js/sidenotes.js',
	'{$static_dir}/js/image-focus.js',
	'{$static_dir}/js/dark-mode.js',
	'{$static_dir}/js/reader-mode.js',

	'{$static_dir}/css/fonts-VERSIONED.css',
	'{$static_dir}/css/default.css',
	'{$static_dir}/css/links.css'	
];
$disparate_asset_patterns = [
	"{$static_dir}/template/include/*.tmpl",
];
foreach ($disparate_asset_patterns as $pattern)
	$disparate_assets = array_merge($disparate_assets, glob($pattern));
$disparate_assets = implode(" ", $disparate_assets);
if ($force || (`git diff-index --cached HEAD -- {$disparate_assets}`)) {
	require_once("{$build_dir}/build_unified_assets.php");
	`git add {$static_dir}/css/. {$static_dir}/js/.`;
}

## Icons.
## Build the icons.svg sprite file out of individual SVG icons.
$icons = [ ];
$icon_patterns = [
	"{$static_dir}/img/icon/*.svg"
];
foreach ($icon_patterns as $pattern)
	$icons = array_merge($icons, glob($pattern));
$icons = implode(" ", $icons);
if ($force || (`git diff-index --cached HEAD -- {$icons}`)) {
	require_once("{$build_dir}/build_icon_sprite_file.php");
	`git add {$static_dir}/img/icon/. {$static_dir}/include/.`;
}

## Icons, redux.
## Ensure that the CSS files use versioned links to icons.svg.
$versioned_assets = [
	"{$static_dir}/img/icon/icons.svg"
];
$files_with_versioned_asset_links = [
	"{$static_dir}/css/head-GENERATED.css",
	"{$static_dir}/css/style-GENERATED.css",
];
$versioned_assets = array_merge($versioned_assets, $files_with_versioned_asset_links);
$versioned_assets = implode(" ", $versioned_assets);
if ($force || (`git diff-index --cached HEAD -- {$versioned_assets}`)) {
	require_once("{$build_dir}/build_asset_versions.php");
	`git add {$static_dir}/include/. {$static_dir}/css/.`;
}

## Initial styles and scripts.
## Build the SSI-included <head> section, with both inlined styles and blocking
## (versioned) links to head.css and head.js.
$head_includes = [
	"{$static_dir}/css/light-mode-GENERATED.css",
	"{$static_dir}/css/dark-mode-GENERATED.css",
	"{$static_dir}/css/head-VERSIONED.css",
	"{$static_dir}/js/head-GENERATED.js"
];
$head_includes = implode(" ", $head_includes);
if ($force || (`git diff-index --cached HEAD -- {$head_includes}`)) {
	require_once("{$build_dir}/build_head_includes.php");
	`git add {$static_dir}/include/.`;
}

## External styles and scripts.
## Build the SSI-included <body> section, with non-blocking (versioned) links 
## to style.css and script.js.
$versioned_files = [
	"{$static_dir}/css/style-VERSIONED.css",
	"{$static_dir}/js/script-GENERATED.js",
	"{$static_dir}/template/inlined-asset-links-template.html"
];
$versioned_files = implode(" ", $versioned_files);
if ($force || (`git diff-index --cached HEAD -- {$versioned_files}`)) {
	require_once("{$build_dir}/build_versioned_includes.php");
	`git add {$static_dir}/include/.`;
}

?>
