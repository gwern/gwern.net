<?php

echo "Building inlined image variable CSS files...\n";

require_once(__DIR__ . '/build_paths.php');
require_once(__DIR__ . '/build_variables.php');

global $css_dir, $img_dir;

$inlined_images_files_structure = [
	## Images inlined into head.css.
	"inlined-images-initial" => [
		"{$img_dir}/ornament/inlined-initial/*.*",
		"{$img_dir}/pattern/*.*",
	],

	## Images inlined into style.css.
	"inlined-images" => [
		"{$img_dir}/logo/logo-smooth.svg",
		"{$img_dir}/ornament/inlined/*.*"
	]
];

$images_section_heading_comment = "/**********/\n"
								. "/* IMAGES */\n"
								. "/**********/\n\n";

foreach ($inlined_images_files_structure as $prefix => $patterns) {
	$inlined_images = [ ];
	foreach ($patterns as $pattern)
		$inlined_images = array_merge($inlined_images, glob($pattern));

	$inlined_images_file = $images_section_heading_comment;
	$inlined_images_file .= ":root {\n";
	foreach ($inlined_images as $image_file_path) {
		$file_extension = pathinfo($image_file_path, PATHINFO_EXTENSION);
		if ($file_extension == 'svg') {
			$inlined_images_file .= css_variable_line_from_svg_file($image_file_path);
		} else {
			$inlined_images_file .= css_variable_line_from_bitmap_image_file($image_file_path);
		}
	}
	$inlined_images_file .= "}\n\n";

	$inlined_images_file_path = "{$css_dir}/{$prefix}-GENERATED.css";
	file_put_contents($inlined_images_file_path, $inlined_images_file);
	$updated_files[] = $inlined_images_file_path;
}

## FUNCTIONS

function css_variable_line_from_svg_file($svg_file_path) {
	$css_variable_line  = "\t--GW-image-";
	$css_variable_line .= str_replace('.', '-', pathinfo($svg_file_path, PATHINFO_BASENAME));
	$css_variable_line .= ": url('data:image/svg+xml;utf8,";

	$svg_contents = preg_replace([
		'/#/',
		'/>\s+</',
		'/\s+/'
	], [
		'%23',
		'><',
		' '
	], file_get_contents($svg_file_path));
	$css_variable_line .= $svg_contents;

	$css_variable_line .= "');\n";

	return $css_variable_line;
}

function css_variable_line_from_bitmap_image_file($image_file_path) {
	$css_variable_line  = "\t--GW-image-";
	$css_variable_line .= str_replace('.', '-', pathinfo($image_file_path, PATHINFO_BASENAME));
	$css_variable_line .= ": url('data:image/";
	$css_variable_line .= pathinfo($image_file_path, PATHINFO_EXTENSION);
	$css_variable_line .= ";base64,";
	$css_variable_line .= base64_encode(file_get_contents($image_file_path));
	$css_variable_line .= "');\n";

	return $css_variable_line;
}

?>
