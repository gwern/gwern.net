#!/usr/bin/env bash
# Emit Bash commands using stringReplace OLD NEW FILE...; do not edit files.
# Usage: bash flatten-redirects.sh [broken.conf move.conf ...] > replacements.sh
# Then review replacements.sh and source it where stringReplace is available.
# Requires Bash 4+, curl, awk, sort, mktemp. One request at a time.
# BASE_URL may be overridden for testing; use an origin without a path.
set -euo pipefail
export LC_ALL=C

if [[ ${1-} == --help ]]; then
    sed -n '2,6p' "$0"
    exit 0
fi
(( BASH_VERSINFO[0] >= 4 )) || { echo 'Requires Bash 4+.' >&2; exit 1; }
(( $# )) || set -- broken.conf move.conf
files=()
for file in "$@"; do
    [[ -f $file && -r $file ]] || { printf 'Cannot read: %s\n' "$file" >&2; exit 1; }
    # Absolute filenames also make the generated commands independent of cwd.
    [[ $file == /* ]] || file=$PWD/$file
    files+=("$file")
done
base=${BASE_URL:-https://gwern.net}
base=${base%/}
[[ $base =~ ^https?://[^/?\#]+$ ]] || { echo 'BASE_URL must be an HTTP(S) origin.' >&2; exit 1; }
max_hops=20
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT

# Exclude this site's error sink both in input and at every redirect hop.
is_404() {
    case ${1%%[?#]*} in
        /404|"http://${base#*://}/404"|"https://${base#*://}/404"|"//${base#*://}/404") return 0 ;;
        *) return 1 ;;
    esac
}

# Quoted, one-line nginx map entries. Escapes in the source regex are OK.
# Retain the actual whitespace around the destination for literal replacement.
entry='^[[:space:]]*"([^"\\]|\\.)*"([[:space:]]+)"([^"\\]*)"([[:space:]]*);[[:space:]]*(#.*)?$'
declare -A targets=() tail_target=() tail_prefix=() tail_suffix=() resolved=()
for file in "${files[@]}"; do
    while IFS= read -r line || [[ -n $line ]]; do
        [[ $line =~ $entry ]] || continue
        target=${BASH_REMATCH[3]}
        [[ -n $target ]] || continue
        # /404 fragments are intentional sinks; omit them before deduplication.
        is_404 "$target" && continue
        prefix='"'${BASH_REMATCH[2]}'"'
        suffix='"'${BASH_REMATCH[4]}';'
        tail=$prefix$target$suffix
        targets["$target"]=1
        tail_target["$tail"]=$target
        tail_prefix["$tail"]=$prefix
        tail_suffix["$tail"]=$suffix
    done < "$file"
done
if (( ${#targets[@]} == 0 )); then
    echo 'No eligible literal quoted destinations found.' >&2
    exit 0
fi
printf '%s\n' "${!targets[@]}" | sort -u > "$work/targets"

# Reject nginx variables/captures and characters requiring nginx escaping.
literal_url() {
    [[ $1 != *'$'* && $1 != *'"'* && $1 != *'\'* && ! $1 =~ [[:space:][:cntrl:]] ]]
}

# curl resolves relative Location values through redirect_url. Do not use -L:
# temporary redirects must never become permanent replacements.
probe() {
    local url=$1 result location
    local -a common=(--globoff --path-as-is --silent --show-error
        --connect-timeout 10 --max-time 30 --proto '=http,https'
        --dump-header "$work/headers" --output /dev/null
        --write-out $'%{http_code}\n%{redirect_url}')
    result=''
    if result=$(curl --disable "${common[@]}" --head --url "$url"); then
        status=${result%%$'\n'*}
    else
        status=000
    fi
    # Confirm HEAD errors and unsupported/non-301 statuses with GET.
    if [[ $status != 200 && $status != 301 ]]; then
        result=$(curl --disable "${common[@]}" --url "$url") || return 1
        status=${result%%$'\n'*}
    fi
    next=${result#*$'\n'}
    if [[ $status == 301 && -n $next ]]; then
        location=$(awk '
            /^HTTP\// { location="" }
            tolower($0) ~ /^location:/ {
                sub(/^[^:]*:[ \t]*/, ""); sub(/[ \t\r]+$/, ""); location=$0
            }
            END { printf "%s", location }
        ' "$work/headers")
        # Browsers inherit the old fragment unless Location specifies one.
        # Reading the raw header also preserves an explicitly empty fragment.
        next=${next%%#*}
        if [[ $location == *'#'* ]]; then
            next+=#${location#*#}
        elif [[ $url == *'#'* ]]; then
            next+=#${url#*#}
        fi
    fi
}

resolve() {
    local url=$1 hops=0 key
    local -A seen=()
    final=''
    reason=''
    while :; do
        if is_404 "$url"; then reason="excluded /404 destination: $url"; return 1; fi
        key=${url%%#*}  # Fragments are not sent in HTTP requests.
        if [[ -n ${seen["$key"]-} ]]; then reason="redirect loop at $url"; return 1; fi
        seen["$key"]=1
        if ! probe "$url"; then reason="request failed at $url"; return 1; fi
        case $status in
            200) final=$url; return 0 ;;
            301)
                if (( hops >= max_hops )); then reason='more than 20 redirects'; return 1; fi
                if [[ ! $next =~ ^https?:// ]] || ! literal_url "$next"; then
                    reason="unusable Location at $url"; return 1
                fi
                url=$next
                hops=$((hops + 1))
                ;;
            *) reason="HTTP $status at $url (requires only 301s, then 200)"; return 1 ;;
        esac
    done
}

checked=0 changed=0 skipped=0
while IFS= read -r target; do
    if ! literal_url "$target"; then
        printf 'SKIP %s: variable, whitespace, or escaping\n' "$target" >&2
        skipped=$((skipped + 1)); continue
    fi
    case $target in
        http://*|https://*) url=$target ;;
        # These known local namespaces have a stray leading slash.
        //doc/*|//docs/*) url=$base/${target#//} ;;
        //*) url=${base%%:*}:$target ;;
        /*) url=$base$target ;;
        *) printf 'SKIP %s: not an HTTP(S) URL or rooted path\n' "$target" >&2
           skipped=$((skipped + 1)); continue ;;
    esac
    checked=$((checked + 1))
    printf '[%d/%d] %s\n' "$checked" "${#targets[@]}" "$target" >&2
    if ! resolve "$url"; then
        printf 'SKIP %s: %s\n' "$target" "$reason" >&2
        skipped=$((skipped + 1)); continue
    fi
    [[ $final != "$url" || $target == //doc/* || $target == //docs/* ]] || continue
    # Keep canonical same-origin destinations in Gwern's /path form.
    case $final in "$base"/*) final=${final#"$base"} ;; esac
    resolved["$target"]=$final
    changed=$((changed + 1))
    printf 'UPDATE %s -> %s\n' "$target" "$final" >&2
done < "$work/targets"

printf '# Generated by flatten-redirects.sh; review, then source in Bash.\n'
printf '# stringReplace must accept literal OLD NEW FILE... arguments.\n'
# Match the closing source quote plus the entire destination field, so a
# destination substring cannot accidentally rewrite a source regex or prefix.
printf '%s\n' "${!tail_target[@]}" | sort > "$work/tails"
while IFS= read -r tail; do
    target=${tail_target["$tail"]}
    [[ -n ${resolved["$target"]-} ]] || continue
    replacement=${tail_prefix["$tail"]}${resolved["$target"]}${tail_suffix["$tail"]}
    printf 'stringReplace %q %q' "$tail" "$replacement"
    printf ' %q' "${files[@]}"
    printf ' || return\n'
done < "$work/tails"
printf '%d unique targets; %d changed; %d skipped.\n' "${#targets[@]}" "$changed" "$skipped" >&2
