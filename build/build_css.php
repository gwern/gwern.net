<?php

echo "Building color CSS...\n";

$css_include_dir = __DIR__ . "/../css/include";
$build_dir = __DIR__;

`php {$build_dir}/color-scheme-convert.php {$css_include_dir}/colors.css 1 "Lab" 0.55 > {$css_include_dir}/components/colors-dark-GENERATED.css`;
`cat {$css_include_dir}/components/colors-dark-GENERATED.css {$css_include_dir}/components/dark-mode-adjustments.css > {$css_include_dir}/dark-mode-GENERATED.css`;

?>