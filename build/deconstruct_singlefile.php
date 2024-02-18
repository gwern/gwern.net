<?php

@ini_set('memory_limit', "256M");

$input_file_path = $argv[1];
$output_file_suffix = $argv[2] ?? '-deconstructed';

$input_file = file_get_contents($input_file_path);

preg_match('/^(.+)(\.[^\.]+)$/', $input_file_path, $m);
$output_file_path = "{$m[1]}{$output_file_suffix}{$m[2]}";

$asset_type_map = [
	'application/font-woff' => 'woff',
	'font/opentype' => 'otf',
	'image/x-icon' => 'ico',
	'image/jpeg' => 'jpg',
	'image/gif' => 'gif',
	'image/png' => 'png',
];

$asset_count = 0;

// $output_file = '';
// $start_text = 'url(data:';
// $end_text = ')';
// $offset = 0;
// while ($startpos = strpos($input_file, $start_text, $offset)) {
// 	$output_file .= substr($input_file, $offset, $startpos - $offset);
// 	$output_file .= 'url(';
// 
// 	$endpos = strpos($input_file, $end_text, $startpos);
// 
// 	$offset = $startpos + strlen($start_text);
// 	$data_plus_type = substr($input_file, $offset, $endpos - $offset);
// 
// 	preg_match('/([^;]+);base64,(.+)/', $data_plus_type, $m);
// 
// 	$type = $m[1];
// 	$data = $m[2];
// 
// 	global $input_file_path, $asset_type_map;
// 
// 	$asset_suffix = '-asset-' . (++$asset_count);
// 	$asset_extension = $asset_type_map[$type] ?? 'dat';
// 	$asset_path = "{$input_file_path}{$asset_suffix}.{$asset_extension}";
// 
// 	file_put_contents($asset_path, base64_decode($data));
// 
// 	preg_match('/[^\/]+$/', $asset_path, $m);
// 	$asset_name = $m[0];
// 
// 	$output_file .= $asset_name;
// 
// 	$offset = $endpos;
// }
// 
// $output_file .= substr($input_file, $offset);

$output_file = preg_replace_callback('/([\'"]?)data:([^;]+);base64,([^\'"\)\s]+)(\1)/', function ($m) {
	global $input_file_path, $asset_type_map, $asset_count;

	$type = $m[2];
	$data = $m[3];
	$quote = strlen($m[1]) > 0 ? $m[1] : '"';

	$asset_suffix = '-asset-' . (++$asset_count);
	$asset_extension = $asset_type_map[$type] ?? 'dat';
	$asset_path = "{$input_file_path}{$asset_suffix}.{$asset_extension}";

	$encoded_length = strlen($data);
// 	echo "Saving asset #{$asset_count} (encoded length: {$encoded_length} bytes)...\n";

	file_put_contents($asset_path, base64_decode($data));

	preg_match('/[^\/]+$/', $asset_path, $m);
	$asset_name = $m[0];

	return "{$quote}{$asset_name}{$quote}";
}, $input_file);

// echo "Saving output file...\n";

file_put_contents($output_file_path, $output_file);

?>
