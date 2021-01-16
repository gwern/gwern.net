<?php

if ($argc < 2)
	die("NO!!!\n");

for ($i = 1; $i < $argc; $i++) {
	$filepath = $argv[$i];

	$page = file_get_contents($filepath);

	preg_match('/<section id="link-bibliography".+?>(.+?)<\/section>/s', $page, $matches);
	$link_bibliography = trim($matches[1])."\n";
	$modified_page = preg_replace('/(<section id="link-bibliography" class="level1) collapse(">).+?(<\/section>)/s', '$1$2$3', $page);

	file_put_contents(preg_replace('/\.html$/', '-link-bibliography.html', $filepath), $link_bibliography);
	file_put_contents($filepath, $modified_page);
}

?>