<?php

## Usage (default 1024MB memory limit):
##   php deconstruct_singlefile.php foo.html
##   php deconstruct_singlefile.php -m 2048M foo.html
##   php deconstruct_singlefile.php --memory-limit 2048M foo.html

$args = [ ];
for ($i = 1; $i < $argc; $i++) {
	if (str_starts_with($argv[$i], '-')) {
		if (isset($argv[$i + 1])) {
			$args[$argv[$i]] = $argv[++$i];
		} else {
			echo "Invalid argument!\n";
			die;
		}
	} else if (!isset($args['file'])) {
		$args['file'] = $argv[$i];
	}
}

$memory_limit = $args['-m'] ?? $args['--memory-limit'] ?? '1024M';
@ini_set('memory_limit', $memory_limit);

$input_file_path = $args['file'];
$input_file = file_get_contents($input_file_path);

preg_match('/^(.+\/)?([^\/]+)\.html$/', $input_file_path, $m);
$asset_directory = "{$m[1]}{$m[2]}";
$asset_base_name = $m[2];

$asset_type_map = [
	'video/3gpp2'                 => '3g2',
	'video/3gp'                   => '3gp',
	'video/3gpp'                  => '3gp',
	'audio/x-acc'                 => 'aac',
	'audio/ac3'                   => 'ac3',
	'audio/x-aiff'                => 'aif',
	'audio/aiff'                  => 'aif',
	'image/bmp'                   => 'bmp',
	'image/x-bmp'                 => 'bmp',
	'image/x-bitmap'              => 'bmp',
	'image/x-xbitmap'             => 'bmp',
	'image/x-win-bitmap'          => 'bmp',
	'image/x-windows-bmp'         => 'bmp',
	'image/ms-bmp'                => 'bmp',
	'image/x-ms-bmp'              => 'bmp',
	'application/bmp'             => 'bmp',
	'application/x-bmp'           => 'bmp',
	'application/x-win-bitmap'    => 'bmp',
	'video/x-f4v'                 => 'f4v',
	'audio/x-flac'                => 'flac',
	'video/x-flv'                 => 'flv',
	'image/gif'                   => 'gif',
	'image/x-icon'                => 'ico',
	'image/x-ico'                 => 'ico',
	'image/vnd.microsoft.icon'    => 'ico',
	'image/jp2'                   => 'jp2',
	'video/mj2'                   => 'jp2',
	'image/jpx'                   => 'jp2',
	'image/jpm'                   => 'jp2',
	'image/jpeg'                  => 'jpg',
	'image/pjpeg'                 => 'jpg',
	'application/javascript'      => 'js',
	'application/x-javascript'    => 'js',
	'text/javascript'             => 'js',
	'audio/x-m4a'                 => 'm4a',
	'audio/mp4'                   => 'm4a',
	'audio/midi'                  => 'mid',
	'video/quicktime'             => 'mov',
	'audio/mpeg'                  => 'mp3',
	'audio/mpg'                   => 'mp3',
	'audio/mpeg3'                 => 'mp3',
	'audio/mp3'                   => 'mp3',
	'video/mp4'                   => 'mp4',
	'video/mpeg'                  => 'mpeg',
	'audio/ogg'                   => 'ogg',
	'video/ogg'                   => 'ogg',
	'application/ogg'             => 'ogg',
	'font/otf'                    => 'otf',
	'font/opentype'               => 'otf',
	'application/x-font-opentype' => 'otf',
	'image/png'                   => 'png',
	'image/x-png'                 => 'png',
	'audio/x-realaudio'           => 'ra',
	'audio/x-pn-realaudio'        => 'ram',
	'video/vnd.rn-realvideo'      => 'rv',
	'image/svg+xml'               => 'svg',
	'image/tiff'                  => 'tiff',
	'font/ttf'                    => 'ttf',
	'application/x-font-ttf'      => 'ttf',
	'audio/x-wav'                 => 'wav',
	'audio/wave'                  => 'wav',
	'audio/wav'                   => 'wav',
	'video/webm'                  => 'webm',
	'image/webp'                  => 'webp',
	'audio/x-ms-wma'              => 'wma',
	'video/x-ms-wmv'              => 'wmv',
	'video/x-ms-asf'              => 'wmv',
	'font/woff'                   => 'woff',
	'application/font-woff'       => 'woff',
	'font/woff2'                  => 'woff2',
	'application/font-woff2'      => 'woff2',
];

$image_file_extensions = [
	'bmp',
	'gif',
	'icon',
	'jp2',
	'jpg',
	'png',
	'tiff',
	'webp'
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

$output_file = preg_replace_callback('/([\'"]?)data:([a-z0-9-+\.\/]+?);base64,([A-Za-z0-9+\/=]+)(\1)/', function ($m) {
	global $asset_type_map, $image_file_extensions;
	global $input_file_path, $asset_directory, $asset_base_name, $asset_count;

	$type = $m[2];
	$data = $m[3];
	$quote = strlen($m[1]) > 0 ? $m[1] : '"';

	$asset_suffix = '-asset-' . (++$asset_count);
	$asset_extension = $asset_type_map[$type] ?? 'dat';
	$asset_name = "{$asset_base_name}{$asset_suffix}.{$asset_extension}";
	$asset_path = "{$asset_directory}/{$asset_name}";

	file_force_contents($asset_path, base64_decode($data));

	## Check image file integrity with ImageMagick.
	## If file is bad, delete it, and leave the asset as base64.
	if (in_array($asset_extension, $image_file_extensions)) {
		$im_identify_result = `identify $asset_path 2>&1`;
		if (strpos($im_identify_result, 'error') !== false) {
			unlink($asset_path);
			return $m[0];
		}
	}

	return "{$quote}{$asset_base_name}/{$asset_name}{$quote}";
}, $input_file);

$output_file = preg_replace('/<img/', '<img loading="lazy" decoding="async"', $output_file);

`mv "{$input_file_path}" "{$input_file_path}.bak"`;
file_put_contents($input_file_path, $output_file);

## FUNCTIONS

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
