<?php

# Author: Said Achmiz
# Date: 2024-02-18
# When:  Time-stamp: "2026-02-01 16:58:24 gwern"
# License: CC-0
#
# `deconstruct_singlefile.php`: Extracts embedded base64 assets from SingleFile HTML archives into
# separate files, and optionally repackages everything into a self-extracting Gwtar (`.gwtar.html`) archive.
# For background on the local archiving workflow this tool was designed for, see <https://gwern.net/archiving#preemptive-local-archiving>.
#
# SingleFile (<https://github.com/nicmad/single-file>) saves complete web pages as monolithic HTML
# files with all assets (images, fonts, scripts) embedded as data URIs. This works but produces
# bloated files that are slow to parse and cannot benefit from incremental loading. This script
# reverses that embedding: it extracts each data URI to a separate file in an asset directory,
# rewrites the HTML to use relative URLs, and applies optional image optimizations.
# (Images/GIFs are recompressed losslessly & lossily, and PNGs are converted to JPEG if minimal PSNR distortion,
# due to the many extremely large photographic PNGs on websites which should be JPEGS.)
#
# The gwtar format (`--create-gwtar=1`) is a self-extracting HTML archive for long-term preservation:
# - Opens as a normal HTML page with embedded JavaScript decoder
# - Contains a ustar tarball appended after the HTML
# - Includes SHA-256 hashes in the asset manifest for integrity verification
# - Optionally includes PAR2 parity data (25% redundancy) for forward error correction
# The result is a single file that browsers can render directly while remaining extractable with
# standard tools (tail + tar) and recoverable from partial corruption.
# (See <https://gwern.net/gwtar> for detailed description and design rationale.)
#
# Usage:
#   php deconstruct_singlefile.php foo.html [options]
#
# Options:
#   --memory-limit=1024M      PHP memory limit (default: 1024M)
#   --backtrack-limit=5000000 PCRE backtrack limit for regex on large files
#   --jpg-quality=80%         JPEG quality when converting from PNG
#   --optimize-images=1       Run 'compressGIF'/'compressPNG'/'compressJPG' if available
#   --save-original=1         Keep original SingleFile with '.bak' extension
#   --create-gwtar=0          Create self-extracting '.gwtar.html' archive
#   --add-fec-data=1          Append PAR2 parity files (requires '--create-gwtar')
#   --keep-original=1         Retain original '.html' file when creating the Gwtar
#   --debug=0                 Print diagnostic messages (PNG conversion decisions, etc.)
#
# (Boolean arguments may also be set to true by passing them without a value.)
#
# Dependencies: `php-cli` (with `pcre`, `json`), ImageMagick (`identify`, `convert`, `compare`), `tar`
# Optional: `par2create` (`par2` package, for `--add-fec-data`), Gwern.net utilities:
# `compressGIF`/`compressPNG`/`compressJPG` (for `--optimize-images`)
# Required files in script directory: `gwtar.js`, `gwtar_noscript.html`.

## Gwtar version:
$gwtar_version_string = 'v1';

## Command line arguments.
$args = [ ];
$boolean_arg_names = [
    '--optimize-images',
    '--save-original',
    '--create-gwtar',
    '--add-fec-data',
    '--keep-original',
    '--debug'
];
for ($i = 1; $i < $argc; $i++) {
    if (str_starts_with($argv[$i], '--')) {
        $parts = explode('=', $argv[$i]);
        if (count($parts) == 2) {
            if (in_array($parts[0], $boolean_arg_names)) {
                $args[$parts[0]] = ($parts[1] != '0');
            } else {
                $args[$parts[0]] = $parts[1];
            }
        } else {
            if (in_array($parts[0], $boolean_arg_names)) {
                $args[$parts[0]] = true;
            } else {
                echo "ERROR: Argument ‘{$args[$parts[0]]}’ must have a value! Exiting.\n";
                die;
            }
        }
    } else if (!isset($args['file'])) {
        $args['file'] = $argv[$i];
    }
}

## Check for file existing.
if (file_exists($args['file']) == false) {
    echo "ERROR: file ‘{$args['file']}’ does not exist! Exiting.\n";
    die;
}

## The JS file that does the client-side processing.
$js_script_path = __DIR__ . '/gwtar.js';
if (file_exists($js_script_path) == false) {
    echo "ERROR: JS script file (‘gwtar.js’) must be in the same directory as this script! Exiting.\n";
    die;
}

## The <noscript> warning.
$noscript_message_path = __DIR__ . '/gwtar_noscript.html';
if (file_exists($noscript_message_path) == false) {
    echo "ERROR: Noscript message file (‘gwtar_noscript.html’) must be in the same directory as this script! Exiting.\n";
    die;
}

## Directory for auxiliary tools.
$build_tool_dir = __DIR__;

## PHP memory limit.
$memory_limit = $args['--memory-limit'] ?? '1024M';
@ini_set('memory_limit', $memory_limit);

## PCRE backtracking limit.
$backtrack_limit = $args['--backtrack-limit'] ?? '5000000';
@ini_set('pcre.backtrack_limit', $backtrack_limit);

## JPEG output quality (for converting PNGs).
$jpg_quality = $args['--jpg-quality'] ?? '80%';

## Optimize images?
$optimize_images = $args['--optimize-images'] ?? true;

## Save original?
$save_original = $args['--save-original'] ?? true;

## Create a .gwtar?
$create_gwtar = $args['--create-gwtar'] ?? false;

## Add FEC data with par2?
$add_fec_data = $args['--add-fec-data'] ?? true;

## Keep original (when creating a .gwtar)?
$keep_original = $args['--keep-original'] ?? true;

## Debug mode?
$debug = $args['--debug'] ?? false;

## Map of content-types to file extensions.
$asset_type_map = [
    'video/3gpp2'                 => '3g2',
    'video/3gp'                   => '3gp',
    'video/3gpp'                  => '3gp',
    'audio/x-acc'                 => 'aac',
    'audio/ac3'                   => 'ac3',
    'audio/x-aiff'                => 'aif',
    'audio/aiff'                  => 'aif',
    'image/bmp'                   => 'bmp',
    'image/x-bmp'                 => 'bmp',
    'image/x-bitmap'              => 'bmp',
    'image/x-xbitmap'             => 'bmp',
    'image/x-win-bitmap'          => 'bmp',
    'image/x-windows-bmp'         => 'bmp',
    'image/ms-bmp'                => 'bmp',
    'image/x-ms-bmp'              => 'bmp',
    'application/bmp'             => 'bmp',
    'application/x-bmp'           => 'bmp',
    'application/x-win-bitmap'    => 'bmp',
    'video/x-f4v'                 => 'f4v',
    'audio/x-flac'                => 'flac',
    'video/x-flv'                 => 'flv',
    'image/gif'                   => 'gif',
    'image/x-icon'                => 'ico',
    'image/x-ico'                 => 'ico',
    'image/vnd.microsoft.icon'    => 'ico',
    'image/jp2'                   => 'jp2',
    'video/mj2'                   => 'jp2',
    'image/jpx'                   => 'jp2',
    'image/jpm'                   => 'jp2',
    'image/jpeg'                  => 'jpg',
    'image/pjpeg'                 => 'jpg',
    'application/javascript'      => 'js',
    'application/x-javascript'    => 'js',
    'text/javascript'             => 'js',
    'audio/x-m4a'                 => 'm4a',
    'audio/mp4'                   => 'm4a',
    'audio/midi'                  => 'mid',
    'video/quicktime'             => 'mov',
    'audio/mpeg'                  => 'mp3',
    'audio/mpg'                   => 'mp3',
    'audio/mpeg3'                 => 'mp3',
    'audio/mp3'                   => 'mp3',
    'video/mp4'                   => 'mp4',
    'video/mpeg'                  => 'mpeg',
    'audio/ogg'                   => 'ogg',
    'video/ogg'                   => 'ogg',
    'application/ogg'             => 'ogg',
    'font/otf'                    => 'otf',
    'font/opentype'               => 'otf',
    'application/x-font-opentype' => 'otf',
    'image/png'                   => 'png',
    'image/x-png'                 => 'png',
    'audio/x-realaudio'           => 'ra',
    'audio/x-pn-realaudio'        => 'ram',
    'video/vnd.rn-realvideo'      => 'rv',
    'image/svg+xml'               => 'svg',
    'image/tiff'                  => 'tiff',
    'font/ttf'                    => 'ttf',
    'application/x-font-ttf'      => 'ttf',
    'audio/x-wav'                 => 'wav',
    'audio/wave'                  => 'wav',
    'audio/wav'                   => 'wav',
    'video/webm'                  => 'webm',
    'image/webp'                  => 'webp',
    'audio/x-ms-wma'              => 'wma',
    'video/x-ms-wmv'              => 'wmv',
    'video/x-ms-asf'              => 'wmv',
    'font/woff'                   => 'woff',
    'application/font-woff'       => 'woff',
    'font/woff2'                  => 'woff2',
    'application/font-woff2'      => 'woff2',
];

## These file types will get special ImageMagick treatment.
$image_file_extensions = [
    'bmp',
    'gif',
    'icon',
    'jp2',
    'jpg',
    'png',
    'tiff',
    'webp'
];

## Get singlefile archive.
$input_file_path = $args['file'];
$input_file = file_get_contents($input_file_path);

## Determine singefile archive basename.
preg_match('/^(.+\/)?([^\/]+)\.html$/', $input_file_path, $m);
$asset_directory = "{$m[1]}{$m[2]}";
$asset_base_name = $m[2];
$html_file_name = "{$asset_base_name}.html";

## Prepare to count assets, and record sizes and content-types.
$assets = [
    [
        'size' => '0',
        'content-type' => 'text/html',
        'basename' => $asset_base_name
    ]
];

$output_file = preg_replace_callback('/([\'"]?)data:([a-z0-9-+\.\/]+?);base64,([A-Za-z0-9+\/=]+)(\1)/', function ($m) {
    global $asset_type_map, $image_file_extensions;
    global $input_file_path, $asset_directory, $asset_base_name;
    global $jpg_quality, $optimize_images, $build_tool_dir;
    global $create_gwtar, $assets;
    global $debug;

    $type = $m[2];
    $data = $m[3];
    $quote = strlen($m[1]) > 0 ? $m[1] : '"';

    $asset_suffix = '-asset-' . (count($assets));
    $asset_extension = $asset_type_map[$type] ?? 'dat';
    $asset_name = "{$asset_base_name}{$asset_suffix}.{$asset_extension}";
    $asset_file_path = "{$asset_directory}/{$asset_name}";

    file_force_contents($asset_file_path, base64_decode($data));

    ## Check image file integrity with ImageMagick.
    ## If file is bad, delete it, and leave the asset as base64.
    if (in_array($asset_extension, $image_file_extensions)) {
        $im_identify_result = `identify "{$asset_file_path}" 2>&1`;
        if (strpos($im_identify_result, 'error') !== false) {
            unlink($asset_file_path);
            return $m[0];
        }
    }

    ## Check a PNG to see if it can be turned into a JPG with minimal quality
    ## loss (according to the ImageMagick PSNR perceptual loss); for PNGs that
    ## should be JPEGs, often the JPEG will be a third the size or less, which
    ## (particularly for large images like sample-grids) makes for more pleasant
    ## web browsing.
    $png_converted_to_jpg = false;
    if ($asset_extension == 'png') {
        ## Criteria.
        $quality_threshold = 31; # decibels
        $size_reduction_threshold = 30; # %

        ## Create the JPEG.
        $alt_asset_extension = 'jpg';
        $alt_asset_name = "{$asset_base_name}{$asset_suffix}.{$alt_asset_extension}";
        $alt_asset_file_path = "{$asset_directory}/{$alt_asset_name}";
        `convert "{$asset_file_path}" -quality 15% "{$alt_asset_file_path}" 2>&1`;

        if (file_exists($alt_asset_file_path)) {
            ## Measure size reduction.
            $png_size = filesize($asset_file_path);
            $jpg_size = filesize($alt_asset_file_path);
            $size_reduction = (1 - ($jpg_size / $png_size)) * 100;

            ## Measure quality.
            $psnr = explode(' ', `compare -metric PSNR "{$asset_file_path}" "{$alt_asset_file_path}" null: 2>&1`)[0];

            ## Check if size reduction and quality measure up.
            if (   $psnr > $quality_threshold
                && $size_reduction > $size_reduction_threshold) {
                $png_converted_to_jpg = true;
                $asset_extension = $alt_asset_extension;

                ## Create the full-quality JPEG.
                `convert "{$asset_file_path}" -quality {$jpg_quality} "{$alt_asset_file_path}" 2>&1`;

                ## Delete the PNG.
                unlink($asset_file_path);

                ## We’ll write out the JPEG’s path to the HTML file.
                $asset_name = $alt_asset_name;

                ## For file size calculation.
                $asset_file_path = $alt_asset_file_path;
            } else {
                if ($debug) {
                    echo "PNG [{$asset_file_path}] unsuitable for JPEG conversion (";
                    $reasons = [ ];
                    if (!($psnr > $quality_threshold))
                        $reasons[] = "PSNR too low ({$psnr})";
                    if (!($size_reduction > $size_reduction_threshold))
                        $reasons[] = "Insufficient size reduction ({$size_reduction}%)";
                    echo implode('; ', $reasons);
                    echo ")\n";
                }

                ## Delete the test JPEG.
                unlink($alt_asset_file_path);
            }
        } else if ($debug) {
            echo "Could not create JPEG from PNG: {$asset_file_path}\n";
        }
    }

    ## Optimize images, if need be.
    if ($optimize_images) {
        if (   $asset_extension == 'gif'
            && file_exists("{$build_tool_dir}/compressGIF")) {
            `{$build_tool_dir}/compressGIF "$asset_file_path"`;
        }
        if (   $asset_extension == 'png'
            && file_exists("{$build_tool_dir}/compressPNG")) {
            `{$build_tool_dir}/compressPNG "$asset_file_path"`;
        }
        if (   $asset_extension == 'jpg'
            && $png_converted_to_jpg == false
            && file_exists("{$build_tool_dir}/compressJPG")) {
            `{$build_tool_dir}/compressJPG "$asset_file_path"`;
        }
    }

    ## Save asset name, size, and content type in asset manifest.
    $assets[$asset_name] = [
        'size'          => filesize($asset_file_path),
        'content-type'  => $type,
        'hash'          => hash_file('sha256', $asset_file_path)
    ];

    ## Construct asset relative URL.
    $asset_relative_url = urlencode($asset_base_name) . '/' . urlencode($asset_name);

    return ($quote . $asset_relative_url . $quote);
}, $input_file);
if (preg_last_error() != PREG_NO_ERROR)
    die;

## Make images lazy loading.
$output_file = preg_replace('/<img/', '<img loading="lazy" decoding="async"', $output_file);
if (preg_last_error() != PREG_NO_ERROR)
    die;

## Remove content security policy.
$output_file = preg_replace('/<meta http-equiv=content-security-policy content=".+?".*>/', '', $output_file);
if (preg_last_error() != PREG_NO_ERROR)
    die;

## Get hash of original singlefile.
if ($create_gwtar)
    $input_file_hash = hash_file('sha256', $input_file_path);

## Save original singlefile, if need be.
if (   $save_original
    || (   $create_gwtar
        && $keep_original))
    `mv "{$input_file_path}" "{$input_file_path}.bak"`;

## Write out slimmed-down HTML file.
file_put_contents($input_file_path, $output_file);

## If we’re making a .gwtar archive...
if ($create_gwtar) {
    ## Get HTML file size.
    $assets[0]['size'] = filesize($input_file_path);

    ## Get HTML file hash.
    $assets[0]['hash'] = hash_file('sha256', $input_file_path);

    ## Sort asset manifest to put JavaScript files first.
    uksort($assets, function ($a, $b) {
        if ($a == 0)
            return -1;
        if (   str_ends_with($a, '.js') == true
            && str_ends_with($b, '.js') == false)
            return -1;
        if (   str_ends_with($a, '.js') == false
            && str_ends_with($b, '.js') == true)
            return 1;
        return 0;
    });

    ## Create tarball.
    $tarball_file_path = "{$asset_directory}/../{$asset_base_name}.tar";
    `tar --create -f "{$tarball_file_path}" --format=ustar --owner=0 --group=0 "{$input_file_path}"`;
    foreach ($assets as $asset_name => $asset_info) {
        if ($asset_name == 0)
            continue;

        $asset_file_path = "{$asset_directory}/{$asset_name}";
        `tar --append -f "{$tarball_file_path}" --format=ustar --owner=0 --group=0 "{$asset_file_path}"`;
    }

    ## Write out asset manifest.
    $asset_manifest_file_path = "{$asset_directory}/../{$asset_base_name}-asset-manifest.js";
    file_put_contents($asset_manifest_file_path, json_encode($assets));

    ## GWTAR archive file path.
    $gwtar_file_path = "{$asset_directory}/../{$asset_base_name}.gwtar.html";

    ## Construct GWTAR archive file.
    $gwtar_file_parts = [
        implode("\n", [ '<html>', "<!-- Gwtar self-extracting HTML archive, {$gwtar_version_string} -->" ]),
        implode("\n", [ '', "<!-- Original SingleFile: {$html_file_name} {$input_file_hash} (SHA-256) -->" ]),
        implode("\n", [ '', '<body>', '<script>', 'let assets = ' ]),
        json_encode($assets, JSON_UNESCAPED_SLASHES|JSON_PRETTY_PRINT),
        implode("\n", [ '', '</script>', '<script>', 'let overhead = parseInt("' ]),
        '000000000000', // 12 digits
        implode("\n", [ '");', 'let totalArchiveSize = parseInt("' ]),
        '000000000000000000000000', // 24 digits
        implode("\n", [ '");', '</script>', '<script>', '' ]),
        file_get_contents($js_script_path),
        implode("\n", [ '', '</script>', '' ]),
        file_get_contents($noscript_message_path),
        implode("\n", [ '', '<script>', 'window.stop();', '</script>' ]),
        implode("\n", [ '', '</body>', '</html>', '<!-- GWTAR END', '' ])
    ];
    $overhead = strlen(implode('', $gwtar_file_parts));
    $gwtar_file_parts[5] = str_pad($overhead, 12, '0', STR_PAD_LEFT);
    $total_gwtar_size = $overhead + filesize($tarball_file_path);
    $gwtar_file_parts[7] = str_pad($total_gwtar_size, 24, '0', STR_PAD_LEFT);
    file_put_contents($gwtar_file_path, implode('', $gwtar_file_parts));
    `cat "{$tarball_file_path}" >> "{$gwtar_file_path}"`;

    ## Make PAR2 files for forward error correction, tarball them up, and
    ## append, if need be.
    if ($add_fec_data) {
        `par2create -r25 "{$gwtar_file_path}"`;
        $par_tarball_file_path = "{$gwtar_file_path}.par2.tar";
        `tar --create -f "{$par_tarball_file_path}" --format=ustar --owner=0 --group=0 "{$gwtar_file_path}.par2"; rm "{$gwtar_file_path}.par2"`;
        foreach (glob("{$gwtar_file_path}.*.par2") as $par_file_path) {
            `tar --append -f "{$par_tarball_file_path}" --format=ustar --owner=0 --group=0 "{$par_file_path}"; rm "{$par_file_path}"`;
        }
        `cat "{$par_tarball_file_path}" >> "{$gwtar_file_path}"; rm "{$par_tarball_file_path}"`;
    }

    ## Remove asset manifest.
    unlink($asset_manifest_file_path);

    ## Remove tarball.
    unlink($tarball_file_path);

    ## Replace original file, if need be; otherwise remove HTML file.
    if ($keep_original) {
        `mv "{$input_file_path}.bak" "{$input_file_path}"`;
    } else {
        unlink($input_file_path);
    }

    ## Remove asset directory.
    `rm -r "{$asset_directory}/"`;
}

## Done.
die;

## FUNCTIONS

function file_force_contents($path, $contents){
    $parts = explode('/', $path);
    $file = array_pop($parts);
    $path = '';
    foreach ($parts as $part)
        if (!is_dir($path .= "{$part}/"))
            mkdir($path);
    file_put_contents("{$path}{$file}", $contents);
}

?>
