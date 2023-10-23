<?php

echo "Building mode CSS...\n";

require_once(__DIR__ . '/build_paths.php');
require_once(__DIR__ . '/build_variables.php');

global $build_dir, $css_dir;

## Generate dark mode colors.
`php {$build_dir}/color-scheme-convert.php {$css_dir}/colors.css 1 "Lab" 0.55 > {$css_dir}/colors-dark-GENERATED.css`;
$updated_files[] = "{$css_dir}/colors-dark-GENERATED.css";

## Generate light mode initial CSS (light colors + mode-specific adjustments).
`cat {$css_dir}/colors.css {$css_dir}/light-mode-adjustments.css > {$css_dir}/light-mode-GENERATED.css`;
$updated_files[] = "{$css_dir}/light-mode-GENERATED.css";

## Generate dark mode initial CSS (dark colors + mode-specific adjustments).
`cat {$css_dir}/colors-dark-GENERATED.css {$css_dir}/dark-mode-adjustments.css > {$css_dir}/dark-mode-GENERATED.css`;
$updated_files[] = "{$css_dir}/dark-mode-GENERATED.css";

?>