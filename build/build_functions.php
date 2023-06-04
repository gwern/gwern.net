<?php

function VersionedAssetHref($file_name, $file_extension) {
	global $static_root;

	$possible_file_paths = array_map(function ($suffix) use ($static_root, $file_name, $file_extension) {
		return "{$static_root}/{$file_name}{$suffix}{$file_extension}";
	}, [ '-VERSIONED', '-GENERATED', '' ]);
	foreach ($possible_file_paths as $file_path) {
		if (file_exists($file_path)) {
			$file_mod_time = filemtime($file_path);
			return "\"/static/{$file_name}{$file_extension}?v={$file_mod_time}\"";
		}
	}

	die('FILE NOT FOUND: ' . "{$static_root}/{$file_name}{$file_extension}\n");
}

?>
