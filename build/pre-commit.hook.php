#!/usr/bin/env php
<?php

require_once(__DIR__ . '/build_paths.php');
require_once(__DIR__ . '/build_variables.php');

$force = @$argv[1] == "--force";


## Process given source files, if updated, with the given script file.
function process_source_files($source_file_paths, $script_file_name) {
	global $force, $build_dir, $updated_files;

	if (count($source_file_paths) == 0)
		return;

	$file_paths_string = implode(" ", $source_file_paths);
	if ($force || (`git diff-index --cached HEAD -- {$file_paths_string}`)) {
		require_once("{$build_dir}/{$script_file_name}");

		## Add updated files and clear updated files array.
		$updated_files = implode(" ", $updated_files);
		`git add {$updated_files}`;
		$updated_files = [ ];
	}
}


## Fonts and font spec.
## Build the font CSS from the font spec.
$fonts_and_font_spec = [
	"{$font_dir}/font_spec.php"
];
$font_path_patterns = [
	"{$font_dir}/*/*.otf",
	"{$font_dir}/*/*.ttf",
	"{$font_dir}/*/*/*.otf",
	"{$font_dir}/*/*/*.ttf"
];
foreach ($font_path_patterns as $pattern) {
	$fonts_and_font_spec = array_merge($fonts_and_font_spec, glob($pattern));
}

process_source_files($fonts_and_font_spec, 'build_font_css.php');


## Font CSS.
## Build the versioned font CSS from the generated font CSS.
$font_css = [
	"{$font_dir}/fonts-GENERATED.css",
	"{$font_dir}/initial-fonts-GENERATED.css"
];

process_source_files($font_css, 'build_versioned_font_css.php');


## Components of color (light/dark) inlined CSS.
## Build the inlined color CSS (for light and dark mode).
$css_components = [
	"{$css_dir}/colors.css",
	"{$css_dir}/light-mode-adjustments.css",
	"{$css_dir}/dark-mode-adjustments.css"
];

process_source_files($css_components, 'build_mode_css.php');


## Icons.
## Build the icons.svg sprite file out of individual SVG icons.
$icons = [ ];
$icon_patterns = [
	"{$icon_dir}/*.svg"
];
foreach ($icon_patterns as $pattern)
	$icons = array_merge($icons, glob($pattern));

process_source_files($icons, 'build_icon_sprite_file.php');


## Asset versions.
## Build asset version database for JS-loaded assets.
$versioned_assets = [
	"{$icon_dir}/icons.svg"
];
$versioned_asset_patterns = [
	"{$logo_dir}/*/*-small-*.png",
	"{$logo_dir}/*/*/*-small-*.png",
	"{$logo_dir}/*/*.svg",
	"{$logo_dir}/*/*/*.svg",

	"{$font_dir}/dropcap/*/*-small-*.png",
	"{$font_dir}/dropcap/*/*/*-small-*.png",
	"{$font_dir}/dropcap/*/*.svg",
	"{$font_dir}/dropcap/*/*/*.svg"
];
foreach ($versioned_asset_patterns as $pattern) {
	$versioned_assets = array_merge($versioned_assets, glob($pattern));
}

process_source_files($versioned_assets, 'build_asset_versions.php');


## Unified assets (JS & CSS).
## Assemble all the .css and .js files into head.css/style.css and 
## head.js/script.js, respectively.
$disparate_assets = [
	"{$js_dir}/utility.js",
	"{$js_dir}/initial.js",
	"{$js_dir}/special-occasions.js",
	"{$js_dir}/layout.js",
	"{$js_dir}/dark-mode-initial.js",
	"{$js_dir}/reader-mode-initial.js",
	"{$js_dir}/asset-versions-GENERATED.js",

	"{$css_dir}/initial.css",
	"{$css_dir}/special-occasions.css",
	"{$css_dir}/initial-fonts-VERSIONED.css",
	"{$css_dir}/reader-mode-initial.css",

	"{$js_dir}/misc.js",
	"{$js_dir}/popups.js",
	"{$js_dir}/popins.js",
	"{$js_dir}/annotations.js",
	"{$js_dir}/content.js",
	"{$js_dir}/transclude.js",
	"{$js_dir}/extracts.js",
	"{$js_dir}/extracts-annotations.js",
	"{$js_dir}/extracts-content.js",
	"{$js_dir}/extracts-options.js",
	"{$js_dir}/extracts-load.js",
	"{$js_dir}/typography.js",
	"{$js_dir}/Hyphenopoly_Loader.js",
	"{$js_dir}/rewrite.js",
	"{$js_dir}/collapse.js",
	"{$js_dir}/sidenotes.js",
	"{$js_dir}/image-focus.js",
	"{$js_dir}/dark-mode.js",
	"{$js_dir}/reader-mode.js",

	"{$css_dir}/fonts-VERSIONED.css",
	"{$css_dir}/default.css",
	"{$css_dir}/links.css"	
];
$disparate_asset_patterns = [
	"{$static_root}/template/include/*.tmpl",
];
foreach ($disparate_asset_patterns as $pattern)
	$disparate_assets = array_merge($disparate_assets, glob($pattern));

process_source_files($disparate_assets, 'build_unified_assets.php');


## Icons, redux.
## Ensure that the CSS files use versioned links to icons.svg.
$files_with_asset_links = [
	"{$css_dir}/head-GENERATED.css",
	"{$css_dir}/style-GENERATED.css",
];

process_source_files($files_with_asset_links, 'version_asset_links.php');


## Initial styles and scripts.
## Build the SSI-included <head> section, with both inlined styles and blocking
## (versioned) links to head.css and head.js.
$head_includes = [
	"{$css_dir}/light-mode-GENERATED.css",
	"{$css_dir}/dark-mode-GENERATED.css",
	"{$css_dir}/head-VERSIONED.css",
	"{$js_dir}/head-GENERATED.js",
	"{$icon_dir}/icons.svg"
];

process_source_files($head_includes, 'build_head_includes.php');


## External styles and scripts.
## Build the SSI-included <body> section, with non-blocking (versioned) links 
## to style.css and script.js.
$body_includes = [
	"{$css_dir}/style-VERSIONED.css",
	"{$js_dir}/script-GENERATED.js"
];

process_source_files($body_includes, 'build_body_includes.php');


## Styles for standalone files.
## Build the SSI-included <head> section, with both inlined styles and blocking
## (versioned) links to head.css and style.css.
$body_includes = [
	"{$css_dir}/light-mode-GENERATED.css",
	"{$css_dir}/head-VERSIONED.css",
	"{$css_dir}/style-VERSIONED.css"
];

process_source_files($body_includes, 'build_standalone_includes.php');


?>
