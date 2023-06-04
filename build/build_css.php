<?php

echo "Building color CSS...\n";

$static_root = __DIR__ . "/..";
$css_dir = "{$static_root}/css";
$build_dir = "{$static_root}/build";

`php {$build_dir}/color-scheme-convert.php {$css_dir}/colors.css 1 "Lab" 0.55 > {$css_dir}/colors-dark-GENERATED.css`;

`cat {$css_dir}/colors.css {$css_dir}/light-mode-adjustments.css > {$css_dir}/light-mode-GENERATED.css`;
`cat {$css_dir}/colors-dark-GENERATED.css {$css_dir}/dark-mode-adjustments.css > {$css_dir}/dark-mode-GENERATED.css`;

?>