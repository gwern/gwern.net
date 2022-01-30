<?php

$css_dir = __DIR__ . "/../css";
$build_dir = __DIR__;

`php {$build_dir}/color-scheme-convert.php {$css_dir}/include/colors.css 1 "Lab" 0.55 > {$css_dir}/include/components/colors-dark-GENERATED.css`;
`cat {$css_dir}/include/components/colors-dark-GENERATED.css {$css_dir}/include/components/dark-mode-adjustments.css > {$css_dir}/include/dark-mode-GENERATED.css`;

?>