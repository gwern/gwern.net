<?php

## NOTE: This script is for use in a development environment only!

$static_dir = __DIR__;

$file_name = $_GET['f'];

$content_type = 'text/plain';
if (str_ends_with($file_name, '.js'))
	$content_type = 'application/javascript';
else if (str_ends_with($file_name, '.css'))
	$content_type = 'text/css';

header ("Content-type: {$content_type}; charset=utf-8");

if (   `git diff`
	&& `git add . ; ./build/pre-commit.hook.php | tee -a asset.log`)
	`rm .git/index.lock`;

echo file_get_contents($file_name);

?>