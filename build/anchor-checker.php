#! /usr/bin/env php
<?php
// anchor-checker.php: Check anchors in HTML files
// Authors: D. Bohdan, Gwern Branwen
// Date: Time-stamp: "2025-04-24 11:50:18 gwern"
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
// Requires: PHP 8.1+
// Installation: $ sudo apt install php-cli

declare(strict_types=1);

error_reporting(E_ALL);

class CheckResult
{
    /**
     * @param list<string> $bad_anchors List of invalid local anchors found (e.g., ['#missing', '#also-gone'])
     * @param int $href_count Total number of <a href=...> and <area href=...> attributes found
     */
    public function __construct(
        public readonly array $bad_anchors,
        public readonly int $href_count
    ) {}
}

function main(array $files): never
{
    if (empty($files)) {
        fprintf(STDERR, "Fatal error: No file arguments provided.\n");
        fprintf(STDERR, "Usage: %s FILE1 [FILE2]...\n", basename(__FILE__));
        exit(1);
    }

    $exit_code = 0;
    $no_hrefs_files = [];

    foreach ($files as $file) {
        if (!is_file($file) || !is_readable($file)) {
            fprintf(STDERR, "Warning: Cannot read file, skipping: %s\n", $file);
            continue;
        }
        $result = check_file($file);

        // Check if *any* <a href> or <area href> were found
        if ($result->href_count === 0) {
            // Don't immediately error, collect these files first
            $no_hrefs_files[] = $file;
            // Continue checking other files, maybe only *some* files lack links
            continue;
        }

        if (!empty($result->bad_anchors)) {
            foreach ($result->bad_anchors as $a) {
                fprintf(STDERR, "%s\t%s\n", $file, $a);
            }
            // Only set exit code to 1 if bad anchors are found
            $exit_code = 1;
        }
    }

    // Report files with no links found *after* checking all files
    if (!empty($no_hrefs_files)) {
        // This is now more of a warning unless *all* files had no links
        $all_files_had_no_links = (count($no_hrefs_files) === count($files));
        $level = $all_files_had_no_links ? 'Fatal error' : 'Warning';

        fprintf(STDERR, "%s: No HTML '<a href=...>' or '<area href=...>' elements found in the following file(s). Are they valid HTML with links? %s\n", $level, implode(', ', $no_hrefs_files));
        // Only exit with error if *all* processed files lacked links, otherwise it might be intentional
        if ($all_files_had_no_links) {
            $exit_code = 1;
        }
    }

    exit($exit_code);
}

function check_file(string $file): CheckResult
{
    $html = file_get_contents($file);
    if ($html === false) {
        // This case should ideally not be reached due to the check in main, but good practice
        fprintf(STDERR, "Fatal error: Failed to read file: %s\n", $file);
        exit(2);  // Use a different exit code for file read errors
    }

    // An ugly hack to get around missing HTML5 support tripping up the parser.
    $html = preg_replace('/<wbr>/', '', $html);
    // Allow empty files, they have no bad anchors and no links
    if (trim($html) === '') {
        return new CheckResult([], 0);
    }

    try {
        $dom = new DOMDocument();
        $dom->loadHTML($html, LIBXML_NOERROR | LIBXML_NOWARNING);
    } catch (\Exception $e) {
        fprintf(STDERR, "Fatal error: Failed to parse HTML file '%s': %s\n", $file, $e->getMessage());
        exit(3);  // Use a different exit code for parse errors
    }

    return check_document($dom);
}

function check_document(DOMDocument $dom): CheckResult
{
    $xpath = new DOMXPath($dom);

    // --- Collect all potential anchor targets ---
    // Find all elements with an 'id' attribute
    $ids = $xpath->query('//@id');
    $id_set = ['#' => true, '#top' => true];  // Base valid targets

    foreach ($ids as $idNode) {
        $idValue = trim($idNode->value);
        if ($idValue !== '') {  // Ensure ID is not empty
            $id_set['#' . $idValue] = true;
        }
    }

    // Find all elements with a 'name' attribute (for older compatibility, though less common now)
    // Note: DOMXPath needs the element name for attribute checks like this if not global //*
    $names = $xpath->query('//a[@name] | //map[@name] | //area[@name] | //img[@name]');  // Add other elements if needed
    foreach ($names as $nameNode) {
        $nameValue = trim($nameNode->getAttribute('name'));
        if ($nameValue !== '') {
            // HTML5 doesn't officially use 'name' for anchors anymore, but let's be lenient
            // Treat name same as ID for anchor target purposes
            $id_set['#' . $nameValue] = true;
        }
    }

    $bad_anchors = [];

    // --- Find all links (<a> and <area>) ---
    // Use XPath union '|' to get hrefs from both <a> and <area> tags
    $all_hrefs = $xpath->query('//a/@href | //area/@href');

    // --- Check each link ---
    foreach ($all_hrefs as $href) {
        // Decode URI encoding (e.g., %20) and trim whitespace
        $value = urldecode(trim($href->value));

        // Only check local anchors starting with '#'
        if (!str_starts_with($value, '#')) {
            continue;
        }

        // Check if the anchor target (e.g., '#mysection') exists in our set of IDs/names
        if (!isset($id_set[$value])) {
            $bad_anchors[] = $value;  // Add to list of bad anchors if not found
        }
    }

    // Return the list of bad anchors and the total count of <a href> + <area href> found
    return new CheckResult(array_unique($bad_anchors), $all_hrefs->length);  // Use array_unique for cleaner output
}

// Execute the main function with command line arguments (excluding the script name itself)
main(array_slice($argv, 1));
