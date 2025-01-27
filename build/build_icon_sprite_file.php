<?php

echo "Building icon sprite file...\n";

require_once(__DIR__ . '/build_paths.php');
require_once(__DIR__ . '/build_variables.php');

global $icon_dir;

$icon_file_path = "{$icon_dir}/icons.svg";

$iconSpacingFactor = 1.1;

##	Delete existing icon file.
if (file_exists($icon_file_path))
	unlink($icon_file_path);

$icon_file_paths = glob("{$icon_dir}/*.svg");

$out = [ '<svg xmlns="http://www.w3.org/2000/svg">' ];

$position = 0;
foreach ($icon_file_paths as $path) {
	$icon = preg_replace('/>\s+</', '><', file_get_contents($path));

	preg_match('/([^\/]+)\.svg$/', $path, $m);
	$icon_file_name = $m[1];

	preg_match('/<svg (.+?)>(.+)<\/svg>/', $icon, $m);
	$svg_attributes = $m[1];
	$svg_contents = $m[2];

	preg_match_all('/(\S+?)="(.+?)"/', $svg_attributes, $m, PREG_SET_ORDER);
	$viewBox = '';
	$icon_attributes = array_filter($m,
									function ($match) use (&$viewBox) { 
										if ($match[1] == 'viewBox')
											$viewBox = $match[2];

										return (str_starts_with($match[1], 'xmlns:') == false);
									});
	$icon_attributes = array_combine(array_column($icon_attributes, 1), 
									 array_column($icon_attributes, 2));

	if ($viewBox == '') {
		## If there is no `viewBox` attribute, but we do have both the `width`
		## and `height` attributes set, then we can simply set the viewBox from
		## the width and height (setting the origin to be 0,0).
		if (   isset($icon_attributes['width'])
			&& isset($icon_attributes['height'])) {
			$viewBox = "0 0 {$icon_attributes['width']} {$icon_attributes['height']}";
			unset($icon_attributes['width']);
			unset($icon_attributes['height']);
		} else {
			preg_match('/[^\/]+$/', $path, $m);
			echo "WARNING: Icon `{$m[0]}` has invalid or missing viewBox attribute (and no width+height attributes); skipping.\n";
			continue;
		}
	}

	$viewBox = explode(' ', $viewBox);

	$translateX = $position - floatval($viewBox[0]);
	$transform = "translate({$translateX}, 0)";
	$viewBox[0] = $position;
	$position += floatval($viewBox[2]) * $iconSpacingFactor;

	$viewBox = implode(' ', $viewBox);
	$out[] = "<view id=\"{$icon_file_name}\" viewBox=\"{$viewBox}\" preserveAspectRatio=\"xMinYMin\" />";

	## If the SVG has a `transform` attribute on the <svg> element itself, we
	## must move it to nested a wrapper <g>; otherwise, it will conflict with
	## the transform we must apply to the icon contents to position it within
	## the file.
	if (isset($icon_attributes['transform'])) {
		$svg_contents = "<g transform=\"{$icon_attributes['transform']}\">"
					  + $svg_contents
					  + "</g>";
		unset($icon_attributes['transform']);
	}

	$icon_attributes = implode(' ', array_map(function ($key) use (&$icon_attributes) {
										return "{$key}=\"{$icon_attributes[$key]}\"";
									}, array_keys($icon_attributes)));

	$out[] = "<g" 
		   . ($icon_attributes ? ' ' : '') 
		   . $icon_attributes 
		   . " transform=\"{$transform}\""
		   . '>' 
		   . $svg_contents 
		   . "</g>";
}

$out[] = '</svg>';

## Write out icon sprite file.
file_put_contents($icon_file_path, implode("\n", $out) . "\n");
$updated_files[] = $icon_file_path;

?>
