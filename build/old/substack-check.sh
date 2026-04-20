#!/usr/bin/env bash
#
# substack-filter.sh — filter a list of URLs to only those redirect to / hosted on Substack
#
# Author: Gwern Branwen
# Date: 2026-02-06
# License: CC-0
#
# Usage:
#   ./substack-filter.sh [INPUT_FILE] [OUTPUT_FILE]
#   cat urls.txt | ./substack-filter.sh
#   ./substack-filter.sh urls.txt substack-urls.txt
#
# This is useful for finding domains to mirror or treat specially.
# One could also use it to analyze your browsing or bookmark history
# to find Substack-using URLs (perhaps to 'like' them en masse),
# eg. `firefox $(cat ~/wiki/doc/meta/2026-02-06-gwern-gwernnet-allsubstackusingurlsgeneratedbysubstackprobesh.txt)`
#
# Approach:
#   1. *.substack.com domains are kept immediately.
#   2. Unknown domains are checked via parallel DNS CNAME checks
#      (Substack custom domains CNAME to target.substack-custom-domains.com).
#   3. Remaining unknowns get a parallel HTTP HEAD check for the
#      'x-served-by: Substack' header (catches apex domains and
#      Cloudflare-proxied setups where CNAME is flattened).
#
# Notes:
# - Bash-only; GNU userland assumed.
# - Fails fast; recoverable network errors are explicitly ignored.
#
#
# Example script to open the results systematically in Chromium:
#
#
## INPUT="/home/gwern/2026-02-06-gwern-gwernnet-allsubstackusingurlsgeneratedbysubstackchecksh.txt"
## BATCH="${2:-20}"
## START="${3:-1}"
##
## tail --lines="+${START}" -- "$INPUT" | while mapfile -t -n "$BATCH" URLS && (( ${#URLS[@]} )); do
##     chromium "${URLS[@]}" &
##     START=$(( START + ${#URLS[@]} ))
##     echo >&2 "Opened ${#URLS[@]} tabs. Next batch: START=${START}. Press Enter to continue, Ctrl-C to stop..."
##     read -r < /dev/tty
## done

set -e

########################################
# Configuration
########################################

INPUT="${1:--}"
OUTPUT="${2:-substack-urls.txt}"
PARALLEL_DNS="${PARALLEL_DNS:-50}"
PARALLEL_HEAD="${PARALLEL_HEAD:-10}"

########################################
# Helpers
########################################

require_cmds() {
    local missing=()
    for cmd in "$@"; do
        command -v "$cmd" >/dev/null 2>&1 || missing+=("$cmd")
    done
    if (( ${#missing[@]} )); then
        echo "Missing required commands: ${missing[*]}" >&2
        exit 1
    fi
}

########################################
# Pre-flight
########################################

require_cmds dig curl xargs sed wc grep

########################################
# Main
########################################

# Read all URLs into memory
mapfile -t URLS < <(cat "$INPUT")

declare -A DOMAIN_IS_SUBSTACK
declare -A DOMAIN_SAMPLE_URL
UNKNOWN_DOMAINS=()

## Phase 1: classify domains by name alone
echo >&2 "Phase 1: classifying domains..."

for url in "${URLS[@]}"; do
    domain=$(echo "$url" | sed --regexp-extended 's|^https?://([^/]+).*|\1|')
    if [[ "$domain" == *.substack.com || "$domain" == substack.com ]]; then
        DOMAIN_IS_SUBSTACK["$domain"]=1
    elif [[ -z "${DOMAIN_IS_SUBSTACK[$domain]+x}" ]]; then
        DOMAIN_IS_SUBSTACK["$domain"]=unknown
        DOMAIN_SAMPLE_URL["$domain"]="$url"
        UNKNOWN_DOMAINS+=("$domain")
    fi
done

echo >&2 "${#DOMAIN_IS_SUBSTACK[@]} unique domains (${#UNKNOWN_DOMAINS[@]} to check)"

## Phase 2: parallel CNAME checks (fast, no HTTP)
echo >&2 "Phase 2: CNAME check (parallelism=${PARALLEL_DNS})..."

CNAME_RESULTS=$(printf '%s\n' "${UNKNOWN_DOMAINS[@]}" \
    | xargs --delimiter='\n' --max-procs="$PARALLEL_DNS" --replace={} sh -c '
        domain="$1"
        base="${domain#www.}"
        # Check the domain as-is (catches blog.*, newsletter.*, etc.)
        if dig +short +time=2 +tries=1 CNAME "$domain" 2>/dev/null \
            | grep --quiet --ignore-case "substack-custom-domains"; then
            echo "$domain"
        # Fall back to www. prefix (catches bare domains whose www. CNAMEs to Substack)
        elif dig +short +time=2 +tries=1 CNAME "www.${base}" 2>/dev/null \
            | grep --quiet --ignore-case "substack-custom-domains"; then
            echo "$domain"
        fi
    ' _ {})

while IFS= read -r domain; do
    [[ -z "$domain" ]] && continue
    DOMAIN_IS_SUBSTACK["$domain"]=1
    echo >&2 "  $domain → Substack (CNAME)"
done <<< "$CNAME_RESULTS"

## Phase 3: parallel HEAD fallback for remaining unknowns
## (catches apex domains which can't CNAME, and Cloudflare-proxied domains
## where the CNAME is flattened into A records)
REMAINING=()
for domain in "${UNKNOWN_DOMAINS[@]}"; do
    [[ "${DOMAIN_IS_SUBSTACK[$domain]}" != "unknown" ]] && continue
    REMAINING+=("$domain")
done

echo >&2 "Phase 3: HEAD checks for ${#REMAINING[@]} remaining domains (parallelism=${PARALLEL_HEAD})..."

HEAD_RESULTS=$(for domain in "${REMAINING[@]}"; do
    echo "$domain ${DOMAIN_SAMPLE_URL[$domain]}"
done | xargs --delimiter='\n' --max-procs="$PARALLEL_HEAD" --max-lines=1 sh -c '
    domain="$1"; url="$2"
    if curl --silent --head --location \
        --max-time 10 \
        --max-redirs 5 \
        "$url" 2>/dev/null \
        | grep --quiet --ignore-case "^x-served-by: Substack"; then
        echo "$domain"
    fi
' _)

while IFS= read -r domain; do
    [[ -z "$domain" ]] && continue
    DOMAIN_IS_SUBSTACK["$domain"]=1
    echo >&2 "  $domain → Substack (HEAD)"
done <<< "$HEAD_RESULTS"

## Phase 4: emit matching URLs
echo >&2 "Phase 4: writing output..."

> "$OUTPUT"
for url in "${URLS[@]}"; do
    domain=$(echo "$url" | sed --regexp-extended 's|^https?://([^/]+).*|\1|')
    if [[ "${DOMAIN_IS_SUBSTACK[$domain]:-0}" == "1" ]]; then
        echo "$url" >> "$OUTPUT"
    fi
done

COUNT=$(wc --lines < "$OUTPUT")
echo >&2 "Done. Wrote $COUNT Substack URLs to $OUTPUT"
