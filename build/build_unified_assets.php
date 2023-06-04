<?php

echo "Building unified CSS and JS assets...\n";

require_once(__DIR__ . '/build_functions.php');

## PATHS

$static_root = __DIR__ . "/..";
$js_dir = "{$static_root}/js";
$css_dir = "{$static_root}/css";
$template_dir = "{$static_root}/template/include";

## FILES

$head_js = [
	'utility.js',
	'initial.js',
	'layout.js',
	'dark-mode-initial.js',
	'reader-mode-initial.js'
];

$head_css = [
	'initial.css',
	'initial-fonts-VERSIONED.css'
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

$css = [
	'fonts-VERSIONED.css',
	'default.css',
	'links.css'
];

$templates = glob("{$template_dir}/*.tmpl");

## ACTION

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
file_put_contents("{$js_dir}/transclude-templates-GENERATED.js", $templates_file);

## Initial CSS.
$head_css_file = "";
foreach ($head_css as $head_css_component) {
	$head_css_file .= file_get_contents("{$css_dir}/{$head_css_component}");
}
file_put_contents("{$css_dir}/head-GENERATED.css", $head_css_file);

## Initial JS.
$head_js_file = "";
foreach ($head_js as $head_js_component) {
	$head_js_file .= file_get_contents("{$js_dir}/{$head_js_component}");
}
file_put_contents("{$js_dir}/head-GENERATED.js", $head_js_file);

## Main CSS.
$css_file = "";
foreach ($css as $css_component) {
	$css_file .= file_get_contents("{$css_dir}/{$css_component}");
}
file_put_contents("{$css_dir}/style-GENERATED.css", $css_file);

## Main JS.
$js_file = "";
foreach ($js as $js_component) {
	$js_file .= file_get_contents("{$js_dir}/{$js_component}");
}
file_put_contents("{$js_dir}/script-GENERATED.js", $js_file);

?>
