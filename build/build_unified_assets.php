<?php

echo "Building unified CSS and JS assets...\n";

require_once(__DIR__ . '/build_paths.php');
require_once(__DIR__ . '/build_variables.php');
require_once(__DIR__ . '/build_functions.php');

global $template_dir, $css_dir, $js_dir;

## FILES

$head_css = [
	'initial.css',
	'special-occasions.css',
	'initial-fonts-VERSIONED.css',
	'reader-mode-initial.css'
];

$head_js = [
	'utility.js',
	'initial.js',
	'special-occasions.js',
	'layout.js',
	'dark-mode-initial.js',
	'reader-mode-initial.js',
	'asset-versions-GENERATED.js'
];

$css = [
	'fonts-VERSIONED.css',
	'default.css',
	'links.css'
];

$js = [
	'misc.js',
	'popups.js',
	'popins.js',
	'annotations.js',
	'content.js',
	'transclude.js',
	'transclude-templates-GENERATED.js',
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
	'reader-mode.js'
];

$templates = glob("{$template_dir}/*.tmpl");

## ACTION

## Initial CSS.
$head_css_file = "";
foreach ($head_css as $head_css_component) {
	$head_css_file .= file_get_contents("{$css_dir}/{$head_css_component}");
}

$head_css_file_path = "{$css_dir}/head-GENERATED.css";
file_put_contents($head_css_file_path, $head_css_file);
$updated_files[] = $head_css_file_path;


## Transclude templates.
$templates_file = "Transclude.templates = {\n";
foreach ($templates as $template_path) {
	preg_match('/([^\/]+)\.tmpl$/', $template_path, $m);
	$template_name = $m[1];

	$template = file_get_contents($template_path);
	$template = '`' . str_replace([ '`', '\\' ], [ '\`', '\\\\' ], $template) . '`';

	$templates_file .= "\t\"{$template_name}\": ";
	$templates_file .= $template;
	$templates_file .= ",\n";
}
$templates_file .= "};\n";

$transclude_templates_file_path = "{$js_dir}/transclude-templates-GENERATED.js";
file_put_contents($transclude_templates_file_path, $templates_file);
$updated_files[] = $transclude_templates_file_path;


## Initial JS.
$head_js_file = "";
foreach ($head_js as $head_js_component) {
	$head_js_file .= file_get_contents("{$js_dir}/{$head_js_component}");
}

$head_js_file_path = "{$js_dir}/head-GENERATED.js";
file_put_contents($head_js_file_path, $head_js_file);
$updated_files[] = $head_js_file_path;


## Main CSS.
$css_file = "";
foreach ($css as $css_component) {
	$css_file .= file_get_contents("{$css_dir}/{$css_component}");
}

$css_file_path = "{$css_dir}/style-GENERATED.css";
file_put_contents($css_file_path, $css_file);
$updated_files[] = $css_file_path;


## Main JS.
$js_file = "";
foreach ($js as $js_component) {
	$js_file .= file_get_contents("{$js_dir}/{$js_component}");
}

$js_file_path = "{$js_dir}/script-GENERATED.js";
file_put_contents($js_file_path, $js_file);
$updated_files[] = $js_file_path;

?>
