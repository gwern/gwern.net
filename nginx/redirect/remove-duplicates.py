#!/usr/bin/env python3

# remove-duplicates.py: lint & optionally prune duplicate nginx `map` keys in Gwern.net redirect tables
# Author: GPT-5.2 Pro
# Date: 2026-02-25
# When:  Time-stamp: "2026-02-25 15:33:46 gwern"
# License: CC-0
#
# Usage:
#
#   # Lint (exit 1 if duplicates exist):
#   $ python3 remove-duplicates.py nginx/redirect/move.conf nginx/redirect/broken.conf
#
#   # Lint, but ignore duplicates where the mapping is identical:
#   $ python3 remove-duplicates.py --allow-redundant nginx/redirect/move.conf nginx/redirect/broken.conf
#
#   # Auto-fix (behavior-preserving): delete shadowed duplicates in-place (writes .bak backups by default):
#   $ python3 remove-duplicates.py --prune-shadowed --in-place nginx/redirect/move.conf nginx/redirect/broken.conf
#
#   # Auto-fix without backups:
#   $ python3 remove-duplicates.py --prune-shadowed --in-place --no-backup nginx/redirect/move.conf nginx/redirect/broken.conf
#
# Background:
#
# Gwern.net implements most legacy URL redirects in nginx using a `map` keyed on `$request_uri`:
#
# ~~~
#   map $request_uri $new_uri {
#     include nginx/redirect/move.conf;
#     include nginx/redirect/broken.conf;
#   }
# ~~~
#
# and then redirects when `$new_uri` is non-empty.
#
# This style is compact and fast, but it is order-sensitive: nginx evaluates `map` entries top-to-bottom.
# For a given request, the first matching rule wins.
# As a result, any later rule with the *same key string* is dead code (a "shadowed duplicate").
#
# Duplicates matter here because `/404#NNNNN` is used for debuggability: collapsing many "broken crawler"
# patterns into a single `/404` target makes it hard to determine which pattern caught a given request.
# Shadowed duplicates silently defeat this tagging.
#
# Purpose / intent:
#
# - Provide a lint step suitable for `sync.sh` / CI: fail when duplicate `map` keys appear.
# - Make it cheap to keep redirect tables tidy as they grow to tens of thousands of entries.
#
# What counts as a "duplicate":
# The key is taken literally as the quoted string in a one-line `map` entry:
#
# ~~~
# "~^/foo$" "/bar";
# ~~~
#
# If the exact same key string appears more than once (across any input files, in include order),
# the later occurrences are duplicates and are never reached in nginx.
#
# This script does NOT attempt semantic equivalence (eg, two different regexes matching the same set).
# It only detects exact key-string duplication, because that is unambiguously wrong / unreachable.
#
# How it works:
# - Parse each file line-by-line and extract only "simple" one-line entries of the form:
#     "KEY" "VALUE";
#   ignoring blank lines and comments.
# - Track all occurrences of each KEY in file/include order.
# - Report duplicates as:
#     WINS: first occurrence (effective in nginx)
#     SHADOWED: later occurrences (ignored by nginx)
#   If the values differ, the key is marked "conflicting targets".
#
# Comment parsing:
# nginx comments start with `#`, but `#` may appear inside quoted strings.
# This script strips comments only when `#` occurs outside double quotes and respects backslash escapes.
#
# Optional pruning mode:
# `--prune-shadowed --in-place` deletes SHADOWED duplicate lines, keeping only the first occurrence.
# This is behavior-preserving with respect to nginx today (because the first occurrence already wins),
# and converts "silent shadowing" into "one key → one redirect target".
#
# Caveat: pruning does not fix incorrect precedence.
# If the *wrong* target is currently first, pruning will preserve that behavior and discard later intent.
# Use lint output to manually resolve conflicts when precedence matters.
#
# Exit codes:
#
#   0: no duplicates found (or pruning completed successfully)
#   1: duplicates found (lint failure)
#   2: file/usage error
#
# Limitations:
#
# - Requires Python ≥3.9 (for `set[int]` type-hint syntax and `Path` features).
# - Keys containing literal escaped double-quotes (eg `"~^/foo\"bar$"`) are not supported
#   and will cause a fatal error.  No real redirect keys should contain these.
# - Mixed line endings within a single file are not supported and will cause a fatal error.
#   All files should use Unix (LF) line endings.
#
# See also: <https://gwern.net/style-guide#renaming>  (redirect conventions; fragments; `.redirect-from-id`)

import argparse
import re
import sys
from collections import defaultdict
from pathlib import Path
import tempfile
import os

MAP_ENTRY_RE = re.compile(r'^\s*"([^"]+)"\s+"([^"]*)"\s*;\s*$')

def check_line_endings(path: Path):
    """Fatal error if a file contains mixed line endings (eg both CRLF and bare LF)."""
    raw = path.read_bytes()
    has_crlf = b"\r\n" in raw
    # Check for bare \r or bare \n (ie \n not preceded by \r)
    bare_lf = raw.replace(b"\r\n", b"")  # remove all CRLF
    has_bare_lf = b"\n" in bare_lf
    has_bare_cr = b"\r" in bare_lf
    kinds = sum([has_crlf, has_bare_lf, has_bare_cr])
    if kinds > 1:
        print(
            f"ERROR: {path}: mixed line endings detected "
            f"(CRLF={has_crlf}, bare-LF={has_bare_lf}, bare-CR={has_bare_cr}). "
            f"Normalize to Unix (LF) line endings first.",
            file=sys.stderr,
        )
        raise SystemExit(2)

def strip_comments_nginx(line: str) -> str:
    """
    Strip nginx '#' comments, but only when the # is outside double quotes.
    Also respects backslash escapes inside quoted strings.
    """
    out = []
    in_quote = False
    escape = False
    for ch in line:
        if escape:
            out.append(ch)
            escape = False
            continue
        if ch == "\\":
            out.append(ch)
            escape = True
            continue
        if ch == '"':
            in_quote = not in_quote
            out.append(ch)
            continue
        if ch == "#" and not in_quote:
            break
        out.append(ch)
    return "".join(out)

def parse_map_entry_from_raw_line(raw: str, path: str = "<unknown>", lineno: int = 0):
    """
    Return (key, value) if the line is a map entry, else None.
    Works even if the original raw line has a trailing # comment.
    Raises SystemExit if the line contains backslash-escaped quotes inside a map entry,
    since no real redirect keys should contain these and the simple regex cannot parse them.
    """
    line = strip_comments_nginx(raw).strip()
    if not line:
        return None
    if line.startswith('"') and '\\"' in line:
        print(
            f"ERROR: {path}:{lineno}: backslash-escaped quote in map entry "
            f"(not supported): {line!r}",
            file=sys.stderr,
        )
        raise SystemExit(2)
    m = MAP_ENTRY_RE.match(line)
    if not m:
        return None
    return m.group(1), m.group(2)

def iter_map_entries(path: Path):
    with path.open("r", encoding="utf-8", errors="replace") as f:
        for lineno, raw in enumerate(f, 1):
            parsed = parse_map_entry_from_raw_line(raw, path=str(path), lineno=lineno)
            if not parsed:
                continue
            key, val = parsed
            yield key, val, lineno

def rewrite_file_pruning_lines(path: Path, delete_lines: set[int], backup_suffix: str):
    if not delete_lines:
        return 0

    with path.open("r", encoding="utf-8", errors="replace", newline="") as f:
        lines = f.readlines()

    removed = 0
    fd, tmpname = tempfile.mkstemp(prefix=path.name + ".", suffix=".tmp", dir=str(path.parent))
    os.close(fd)
    tmppath = Path(tmpname)

    try:
        with tmppath.open("w", encoding="utf-8", newline="") as out:
            for idx, raw in enumerate(lines, 1):
                if idx in delete_lines:
                    removed += 1
                    continue
                out.write(raw)

        if backup_suffix is not None:
            backup_path = path.with_name(path.name + backup_suffix)
            if backup_path.exists():
                print(
                    f"ERROR: backup file already exists: {backup_path}\n"
                    f"  Remove it manually or use --no-backup.",
                    file=sys.stderr,
                )
                raise SystemExit(2)
            path.rename(backup_path)

        tmppath.replace(path)
        return removed
    finally:
        if tmppath.exists():
            tmppath.unlink(missing_ok=True)

def main() -> int:
    ap = argparse.ArgumentParser(
        description="Find duplicate nginx map keys in gwern.net redirect tables (and optionally prune shadowed duplicates)."
    )
    ap.add_argument(
        "files",
        nargs="+",
        help="Input files, in nginx include order (order matters).",
    )
    ap.add_argument(
        "--allow-redundant",
        action="store_true",
        help="Ignore duplicates that map to the same target (only report conflicting ones).",
    )

    ap.add_argument(
        "--prune-shadowed",
        action="store_true",
        help="Delete shadowed duplicate keys (keep first occurrence in include order).",
    )
    ap.add_argument(
        "--in-place",
        action="store_true",
        help="Actually rewrite the input files in place (required with --prune-shadowed).",
    )
    ap.add_argument(
        "--no-backup",
        action="store_true",
        help="Skip writing .bak backup files when pruning in place.",
    )

    args = ap.parse_args()

    if args.prune_shadowed and not args.in_place:
        print("ERROR: --prune-shadowed requires --in-place", file=sys.stderr)
        return 2

    # Parse occurrences in include order.
    occ = defaultdict(list)  # key -> list[(file, line, value)] in include order
    total = 0

    for fname in args.files:
        p = Path(fname)
        if not p.exists():
            print(f"ERROR: missing file: {fname}", file=sys.stderr)
            return 2
        check_line_endings(p)
        for key, val, lineno in iter_map_entries(p):
            total += 1
            occ[key].append((fname, lineno, val))

    dups = {k: v for k, v in occ.items() if len(v) > 1}

    def is_conflict(v):
        return len({x[2] for x in v}) > 1

    # Optional pruning mode (behavior-preserving): delete all but the first occurrence.
    if args.prune_shadowed:
        delete_by_file = defaultdict(set)  # file -> set(line_numbers)
        for key, v in dups.items():
            # keep v[0], delete v[1:]
            for (fname, lineno, _val) in v[1:]:
                delete_by_file[fname].add(lineno)

        backup_suffix = None if args.no_backup else ".bak"

        removed_total = 0
        for fname in args.files:
            p = Path(fname)
            removed = rewrite_file_pruning_lines(p, delete_by_file.get(fname, set()), backup_suffix)
            removed_total += removed
            if removed:
                print(f"pruned {removed} shadowed entries from {fname}", file=sys.stderr)

        print(f"done: pruned {removed_total} lines (parsed entries: {total})", file=sys.stderr)
        return 0

    # Lint/report mode (original behavior)
    if args.allow_redundant:
        dups = {k: v for k, v in dups.items() if is_conflict(v)}

    if not dups:
        return 0

    conflicts = sum(1 for v in dups.values() if is_conflict(v))
    print(
        f"duplicate redirect-map keys: {len(dups)} "
        f"(conflicting: {conflicts}) "
        f"(parsed entries: {total})",
        file=sys.stderr,
    )
    print(file=sys.stderr)

    for key in sorted(dups.keys()):
        v = dups[key]
        conflict = is_conflict(v)

        print(key)
        for i, (fname, lineno, val) in enumerate(v):
            tag = "WINS" if i == 0 else "SHADOWED"
            print(f"  {tag}: {fname}:{lineno} -> {val}")
        if conflict:
            print("  NOTE: conflicting targets")
        print()

    return 1

if __name__ == "__main__":
    raise SystemExit(main())
