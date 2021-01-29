#!/usr/bin/env php
<?php

$force = @$argv[1] == "--force";

$build_dir = __DIR__;
$static_dir = "{$build_dir}/..";

if ($force || (`git diff-index --cached HEAD -- {$static_dir}/css/colors.css {$static_dir}/css/dark-mode-adjustments.css`)) {
	require_once("{$build_dir}/build_css.php");
	`git add {$static_dir}/css/.`;
}

if ($force || (`git diff-index --cached HEAD -- {$static_dir}/css/colors.css {$static_dir}/css/initial.css {$static_dir}/css/dark-mode.css {$static_dir}/js/gw-inline.js {$static_dir}/js/darkmode-inline.js`)) {
	require_once("{$build_dir}/build_head_includes.php");
	`git add {$static_dir}/includes/.`;
}

if ($force || (`git diff-index --cached HEAD -- {$static_dir}/css/fonts.css {$static_dir}/css/default.css {$static_dir}/css/links.css {$static_dir}/js/popups.js {$static_dir}/js/extracts.js {$static_dir}/js/extracts-options.js {$static_dir}/js/image-focus.js {$static_dir}/js/tablesorter.js {$static_dir}/js/typography.js {$static_dir}/js/rewrite.js {$static_dir}/js/collapse.js {$static_dir}/js/darkmode.js`)) {
	require_once("{$build_dir}/build_versioned_includes.php");
	`git add {$static_dir}/includes/.`;
}

?>
