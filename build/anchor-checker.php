#! /usr/bin/env php
<?php
// anchor-checker.php: Check anchors in HTML files
// Authors: D. Bohdan, Gwern Branwen
// Date: 2024-02-27
// License: choice of CC-0 or MIT-0
//
// This script only checks anchors local to each document. Anchors prefixed
// with a filename are ignored even if they refer to the same file. Anchors
// with no element with the corresponding fragment ID
// are written to stderr prefixed with the filename.
//
// Usage:
// $ anchor-checker.php FILE1 [FILE2]...
// Output:
// dir/file1.html<tab>#foo
// dir/file1.html<tab>#bar
// dir/file2.html<tab>#baz
// ...
//
// Requires: PHP 8.x, HTML5-PHP
// Installation: $ sudo apt install php-masterminds-html5 php-mbstring

error_reporting(E_ALL);

set_include_path("/usr/share/php");
require("Masterminds/HTML5/autoload.php");
use Masterminds\HTML5;

function main($files) {
    $exit_code = 0;

    if (count($files) == 0) { fwrite(STDERR,"Wrong number of file arguments:"); fwrite(STDERR, implode(", ", $files)); exit(1); }

    foreach ($files as $file) {
        $bad_anchors = check_file($file);
        foreach ($bad_anchors as $a) {
            fprintf(STDERR, "%s\t%s\n", $file, $a);
            $exit_code = 1;
        }
    }

    exit($exit_code);
}

function check_file($file) {
    $html = file_get_contents($file);
    if (!$html) { fwrite(STDERR,"Failed to read file:"); fwrite(STDERR, $file); exit(2); }

    if (preg_match("/^\s*$/", $html)) return [];

    $html5 = new Masterminds\HTML5([
        'disable_html_ns' => true,
    ]);
    $dom = $html5->loadHTML($html);

    return check_document($dom);
}

function check_document($dom) {
    $ids = (new DOMXpath($dom))->query("//@id");
    $id_set = ["#" => true, "#top" => true];

    foreach ($ids as $id) {
        $id_set["#" . $id->value] = true;
    }

    $bad_anchors = array();
    $hrefs = (new DOMXpath($dom))->query("//a/@href");

    foreach ($hrefs as $href) {
        $value = urldecode(trim($href->value));

        if (substr($value, 0, 1) !== "#") continue;

        if (!array_key_exists($value, $id_set)) {
            $bad_anchors[] = $value;
        }
    }

    return $bad_anchors;
}

main(array_slice($argv, 1));
