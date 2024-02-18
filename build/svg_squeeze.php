<?php

## Usage:
##   svg_squeeze 2 foo.svg bar.svg baz.svg
## (The second argument is the squeeze factor; 2 or 3 works best for most images.)

$path_regexp = '/(<path[^<>]*d=")(.+?)("[^<>]*>)/';
$value_regexp = '/(?<![0-9\.])[0-9]+(?:\.[0-9]+)?(?![0-9\.])/';

# Lower factor = squeeze harder.
$squeeze_factor = $argv[1];

$file_names = array_slice($argv, 2);
foreach ($file_names as $file_name) {
	echo $file_name . "\n";

	$svg = file_get_contents($file_name);

	$all_values = [ ];

	preg_match_all($path_regexp, $svg, $paths);
	foreach ($paths[2] as &$path) {
		preg_match_all($value_regexp, $path, $values);
		$all_values[] = $values[0];	
	}

	$all_values = array_merge(...$all_values);

	if (count($all_values) < 2)
		continue;

	sort($all_values, SORT_NUMERIC);
	$scale = $all_values[count($all_values) - 1] - $all_values[0];
	$precision = ((round(log10($scale)) - 1) * -1) + $squeeze_factor;

	$svg = preg_replace_callback($path_regexp, function ($m) use ($precision) {
		global $value_regexp;

		$path = $m[2];

		preg_match_all($value_regexp, $path, $values);

		$path = preg_replace_callback($value_regexp, function ($m) use ($precision) {
			$value = round($m[0], $precision);

			return $value;
		}, $path);

		return $m[1].$path.$m[3];
	}, $svg);

	file_force_contents(getcwd() . '/squeezed/'.$file_name, $svg);
}

function file_force_contents($path, $contents){
	$parts = explode('/', $path);
	$file = array_pop($parts);
	$path = '';
	foreach ($parts as $part)
		if (!is_dir($path .= "{$part}/"))
			mkdir($path);
	file_put_contents("{$path}{$file}", $contents);
}

?>