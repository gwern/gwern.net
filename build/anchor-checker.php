#! /usr/bin/env php
<?php
// Check anchors in HTML files. Only checks anchors local to each document.
// Anchors prefixed with a filename are ignored even if they refer to the
// same file. Anchors with no element with the corresponding fragment ID
// are written to stderr prefixed with the filename.
//
// Usage: anchor-checker.php FILE1 [FILE2]...
//
// To the extent possible under law, D. Bohdan has waived all copyright and
// related or neighboring rights to this work.
//
// Date: 2021-08-11.
// Requirements: PHP 7.x with the standard DOM module.

error_reporting(E_ALL);

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

function check_file($file) { // }
    $html = file_get_contents($file);
    if (!$html) { fwrite(STDERR,"Failed to read file:"); fwrite(STDERR, $file); exit(2); }

    // An ugly hack to get around missing HTML5 support tripping up the parser.
    $html = preg_replace("/<wbr>/", "", $html);

    if (preg_match("/^\s*$/", $html)) return [];

    $dom = new DOMDocument();
    $dom->loadHTML($html, LIBXML_NOERROR | LIBXML_NOWARNING);

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
