<?php

## DEFINITIONS

/*	Font info:

	Required
	- name
	- base_path
	- format
	- entries
		- weight (key)
		- filename

	Optional
	- italic
	- font-display
	- unicode-range
 */
$bare_fields = [ "name", "base_path", "format" ];
$formats = [
	'ttf' => 'truetype',
	'otf' => 'opentype',
];

## PATHS

$static_root = __DIR__ . "/..";

$css_dir = "{$static_root}/css";
$font_dir = "{$static_root}/font";

## ACTION

//	Read spec file.
ob_start();
require("{$font_dir}/font_spec.php");
$spec_file = ob_get_contents();
ob_end_clean();

$outfile = '@charset "UTF-8";' . "\n";

$spec_blocks = explode("\n\n", trim($spec_file));

foreach ($spec_blocks as $spec_block) {
	$spec_lines = explode("\n", trim($spec_block));

	$spec = [ ];
	$entries = [ ];
	foreach ($spec_lines as $line) {
		if ($line[0] == '`') {
			list($weight, $filename) = kv_tokenize($line);
			$entries[] = [ 'weight' => $weight, 'filename' => $filename ];	
		} else if ($line[0] == "\t") {
			list($key, $value) = kv_tokenize(ltrim($line));
			$entries[array_key_last($entries)][$key] = $value;
		} else {
			list($key, $value) = kv_tokenize($line);
			if ($key == '') {
				foreach ($bare_fields as $field) {
					if (!isset($spec[$field])) {
						$key = $field;
						break;
					}
				}
			}
			$spec[$key] = $value;
		}
	}

	$out = [ ];
	$pad = str_pad('', strlen($spec['name']), '*');
	$out[] = "\n" . "/**" . $pad                      . "**/";
	$out[] =        "/* " . strtoupper($spec['name']) . " */";
	$out[] =        "/**" . $pad                      . "**/" . "\n";
	foreach ($entries as $entry_spec) {
		$entry_spec = array_merge($spec, $entry_spec);
		if (($entry_spec['italic'] ?? null) !== '')
			$out[] = construct_entry($entry_spec);
		if (($entry_spec['italic'] ?? null) !== null)
			$out[] = construct_entry($entry_spec, true);
	}

	$outfile .= implode("\n", $out) . "\n";
}

file_put_contents("{$css_dir}/fonts.css", $outfile);

die;

## FUNCTIONS

function construct_entry($entry_spec, $italic = false) {
	global $formats;
	
	$entry   = [ ];
	$entry[] = "@font-face {";
	$entry[] = "	font-family: '" . $entry_spec['name'] . "';";
	if ($entry_spec['weight'] > '')
		$entry[] = "	font-weight: " . $entry_spec['weight'] . ";";
	if (isset($entry_spec['italic']))
		$entry[] = "	font-style: " . ($italic ? 'italic' : 'normal') . ";";
	$path = $entry_spec['base_path'] . $entry_spec['filename'];
	if ($italic && $entry_spec['italic'] != '') {
		$pattern = $entry_spec['italic'][0];
		$replacement = $entry_spec['italic'][1];
		$path = preg_replace($pattern, $replacement, $path);
	}
	$format = $formats[$entry_spec['format']];
	$entry[] = "	src: url('{$path}.{$entry_spec['format']}') format('{$format}');";
	$additional_keys = [ 'font-display', 'unicode-range' ];
	foreach ($additional_keys as $key)
		if (isset($entry_spec[$key]))
			$entry[] = "	{$key}: {$entry_spec[$key]};";
	$entry[] = "}";

	return implode("\n", $entry);
}

function kv_tokenize($line) {
	$parts = explode("\t", $line);

	if (count($parts) == 1) {
		$key = '';
		$value = $parts[0];
	} else {
		$parts = array_filter($parts);
		$key = $parts[0];
		if ($key[0] == '`')
			$key = substr($key, 1);
		switch ($key) {
			case 'italic':
				$value = array_slice($parts, 1);
				break;
			default:
				$value = count($parts) > 1 ? array_slice($parts, 1)[0] : '';
				break;
		}
	}
	return [ $key, $value ];
}

?>