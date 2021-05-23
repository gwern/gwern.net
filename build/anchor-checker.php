#! /usr/bin/env php
<?php
// Check anchors in HTML files.  Only checks anchors local to each document.
// Anchors prefixed with a filename are ignored even if they refer to the
// same file.  Anchors with no element with the corresponding fragment ID
// are written to stderr prefixed with the filename.
//
// Usage: check-anchors.php [file ...]
//
// To the extent possible under law, D. Bohdan has waived all copyright and
// related or neighboring rights to this work.
//
// Date: 2021-05-24.
// Requirements: PHP 7.x.

error_reporting(E_ALL);

function main($files) {
    $exit_code = 0;

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
    $dom = new DOMDocument();
    $dom->loadHTMLFile($file, LIBXML_NOERROR | LIBXML_NOWARNING);

    return check_document($dom);
}

function check_document($dom) {
    $ids = (new DOMXpath($dom))->query("//@id");
    $id_set = array();

    foreach ($ids as $id) {
        $id_set["#" . $id->value] = true;
    }

    $bad_anchors = array();
    $hrefs = (new DOMXpath($dom))->query("//a/@href");
    foreach ($hrefs as $href) {
        $value = trim($href->value);

        if ($value[0] !== "#") continue;

        if (!array_key_exists($value, $id_set)) {
            $bad_anchors[] = $value;
        }
    }

    return $bad_anchors;
}

main(array_slice($argv, 1));
