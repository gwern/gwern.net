<?php

$css_dir = __DIR__ . "/../css";
$build_dir = __DIR__;

`php {$build_dir}/color-scheme-convert.php {$css_dir}/colors.css 1 "Lab" 0.55 > {$css_dir}/colors-dark.css`;
`cat {$css_dir}/colors-dark.css {$css_dir}/dark-mode-adjustments.css > {$css_dir}/dark-mode.css`;

?>