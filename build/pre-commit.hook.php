#!/usr/bin/env php
<?php

$force = @$argv[1] == "--force";

$build_dir = __DIR__;
$static_dir = "{$build_dir}/..";

## Fonts and font CSS.
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
$disparate_assets = [
	'utility.js',
	'initial.js',
	'layout.js',
	'dark-mode-initial.js',
	'reader-mode-initial.js',

	'initial.css',
	'initial-fonts-VERSIONED.css',

	'misc.js',
	'popups.js',
	'popins.js',
	'annotations.js',
	'content.js',
	'transclude-templates-GENERATED.js',
	'transclude.js',
	'extracts.js',
	'extracts-annotations.js',
	'extracts-content.js',
	'extracts-options.js',
	'extracts-load.js',
	'typography.js',
	'Hyphenopoly_Loader.js',
	'rewrite.js',
	'collapse.js',
	'sidenotes.js',
	'image-focus.js',
	'dark-mode.js',
	'reader-mode.js',

	'fonts-VERSIONED.css',
	'default.css',
	'links.css'	
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

## Inlined styles and scripts.
$head_includes = [
	"{$static_dir}/css/light-mode-GENERATED.css",
	"{$static_dir}/css/dark-mode-GENERATED.css",
	"{$static_dir}/css/head.css",
	"{$static_dir}/js/head.js"
];
$head_includes = implode(" ", $head_includes);
if ($force || (`git diff-index --cached HEAD -- {$head_includes}`)) {
	require_once("{$build_dir}/build_head_includes.php");
	`git add {$static_dir}/include/.`;
}

## External styles and scripts.
$versioned_files = [
	"{$static_dir}/css/style.css",
	"{$static_dir}/js/script.js",
	"{$static_dir}/template/inlined-asset-links-template.html"
];
$versioned_files = implode(" ", $versioned_files);
if ($force || (`git diff-index --cached HEAD -- {$versioned_files}`)) {
	require_once("{$build_dir}/build_versioned_includes.php");
	`git add {$static_dir}/include/.`;
}

## Icons.
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

## Assets (icons & templates).
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

?>
