#! /usr/bin/env php
<?php
// anchor-checker.php: Check anchors in HTML files
// Authors: D. Bohdan, Gwern Branwen
// Date: 2025-01-26
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
// Requires: PHP 8.1+, HTML5-PHP
// Installation: $ sudo apt install php-masterminds-html5 php-mbstring

declare(strict_types=1);

error_reporting(E_ALL);

set_include_path('/usr/share/php');
require ('Masterminds/HTML5/autoload.php');

use Masterminds\HTML5;

class CheckResult
{
    public function __construct(
        public readonly array $bad_anchors,
        public readonly int $href_count
    ) {}
}

function main(array $files): never
{
    if (empty($files)) {
        fprintf(STDERR, "Fatal error: No file arguments.\n");
        exit(1);
    }

    $exit_code = 0;
    $no_hrefs_files = [];

    foreach ($files as $file) {
        $result = check_file($file);

        if ($result->href_count === 0) {
            $no_hrefs_files[] = $file;
            continue;
        }

        foreach ($result->bad_anchors as $a) {
            fprintf(STDERR, "%s\t%s\n", $file, $a);
            $exit_code = 1;
        }
    }

    if (!empty($no_hrefs_files)) {
        fprintf(STDERR, "Fatal error: No HTML '<a href>' elements found, are you sure this is HTML? Failed to parse files: %s\n", implode(', ', $no_hrefs_files));
        $exit_code = 1;
    }

    exit($exit_code);
}

function check_file(string $file): CheckResult
{
    $html = file_get_contents($file);
    if ($html === false) {
        fprintf(STDERR, "Fatal error: Failed to read file: %s\n", $file);
        exit(2);
    }

    if (preg_match('/^\s*$/', $html)) {
        return new CheckResult([], 0);
    }

    $html5 = new HTML5([
        'disable_html_ns' => true,
    ]);
    $dom = $html5->loadHTML($html);

    return check_document($dom);
}

function check_document(DOMDocument $dom): CheckResult
{
    $xpath = new DOMXPath($dom);
    $ids = $xpath->query('//@id');
    $id_set = ['#' => true, '#top' => true];

    foreach ($ids as $id) {
        $id_set['#' . $id->value] = true;
    }

    $bad_anchors = [];
    $hrefs = $xpath->query('//a/@href');

    foreach ($hrefs as $href) {
        $value = urldecode(trim($href->value));

        if (!str_starts_with($value, '#')) {
            continue;
        }

        if (!isset($id_set[$value])) {
            $bad_anchors[] = $value;
        }
    }

    return new CheckResult($bad_anchors, $hrefs->length);
}

main(array_slice($argv, 1));
