<?php

`php color-scheme-convert.php ../css/colors.css 1 "Lab" 0.55 > ../css/colors-dark.css`;
`cat ../css/colors-dark.css ../css/dark-mode-adjustments.css > ../css/dark-mode.css`;

?>