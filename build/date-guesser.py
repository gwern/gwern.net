#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# date-guesser.py: conservatively extract visible dates from natural language inputs or structured text like URLs.
# Author: Gwern Branwen, GPT-5.4 Pro
# Revised: 2026-04-21
# License: CC-0
#
# Usage:
#   OPENAI_API_KEY="sk-XXX" echo 'https://erikbern.com/2016/04/04/nyc-subway-math' | python date-guesser.py
#
# Output is exactly one of:
#   YYYY
#   YYYY-MM
#   YYYY-MM-DD
#   ""
#
# This script intentionally does not ask the model to remember or infer hidden publication dates.
# For exact manually verified dates that are not visible in the input, add rows to EXAMPLES:
#
#   ("https://proceedings.mlr.press/v139/vicol21a.html", "2021-07-01", False),
#
# Schema:
#
#   ("visible/generalizable input", "date")
#   ("opaque URL/ID/local PDF", "known true date", False)
#
# The final False means: use as an exact-match correction, but do not show it to the LLM prompt.
# This keeps true lookup facts in this file without teaching the model to guess from opaque IDs.

from __future__ import annotations

import json
import os
import re
import sys
from dataclasses import dataclass
from datetime import datetime, timezone
from typing import Iterable
from urllib.parse import unquote, urlparse

from openai import OpenAI

EMPTY = '""'

ExampleRow = tuple[str, str] | tuple[str, str, bool]


def unpack_example(row: ExampleRow) -> tuple[str, str, bool]:
    """
    EXAMPLES rows are either:
      (input, output)        -> exact lookup + show in prompt
      (input, output, False) -> exact lookup only; hide from prompt

    The third form is for manually verified dates that are not visibly extractable.
    They are true overrides, but they should not teach the model to guess.
    """
    if len(row) == 2:
        return row[0], row[1], True
    return row[0], row[1], row[2]


MONTHS = {
    "jan": 1, "january": 1,
    "feb": 2, "february": 2,
    "mar": 3, "march": 3,
    "apr": 4, "april": 4,
    "may": 5,
    "jun": 6, "june": 6,
    "jul": 7, "july": 7,
    "aug": 8, "august": 8,
    "sep": 9, "sept": 9, "september": 9,
    "oct": 10, "october": 10,
    "nov": 11, "november": 11,
    "dec": 12, "december": 12,
}

DATE_CONTEXT_RE = re.compile(
    r"""(?ix)
    \b(
        date|published|posted|created|written|submitted|accepted|released|release|
        updated|modified|timestamp|time-stamp|last\ modified|publication|
        this\ post\ was\ submitted|from\ our|edition
    )\b
    """
)

BAD_URL_HOST_RE = re.compile(
    r"""(?ix)
    (^|\.)(
        wikipedia\.org|
        x\.com|twitter\.com|xcancel\.com|
        news\.ycombinator\.com|
        youtube\.com|youtu\.be|
        github\.com|
        scholar\.google\.com
    )$
    """
)

# Numeric IDs in these URL families are not publication dates by themselves.
UNTRUSTED_DATE_CODE_HOST_RE = re.compile(
    r"""(?ix)
    (^|\.)(
        ncbi\.nlm\.nih\.gov|
        pmc\.ncbi\.nlm\.nih\.gov|
        pubmed\.ncbi\.nlm\.nih\.gov|
        nature\.com|
        link\.springer\.com|
        onlinelibrary\.wiley\.com|
        journals\.plos\.org|
        academic\.oup\.com|
        sciencedirect\.com|
        science\.org|
        pnas\.org|
        liebertpub\.com|
        frontiersin\.org|
        royalsocietypublishing\.org
    )$
    """
)

BLOGISH_SEGMENTS = {
    "blog", "blogs", "post", "posts", "news", "archive", "archives",
    "article", "articles", "entry", "entries", "log", "notes", "updates",
    "technology", "world", "business", "science", "health", "culture",
}


@dataclass(frozen=True)
class Candidate:
    date: str
    source: str


def current_date() -> datetime:
    # UTC avoids day-boundary surprises on remote machines.
    return datetime.now(timezone.utc).replace(tzinfo=None)


def validate_date_format(date_str: str) -> bool:
    return bool(re.match(r"^\d{4}(?:-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12]\d|3[01]))?)?$", date_str))


def validate_date_not_future(date_str: str, today: datetime | None = None) -> bool:
    today = today or current_date()

    try:
        if len(date_str) == 4:
            year = int(date_str)
            return 1000 <= year <= today.year
        if len(date_str) == 7:
            date = datetime.strptime(date_str, "%Y-%m")
            return 1000 <= date.year and (date.year < today.year or (date.year == today.year and date.month <= today.month))
        if len(date_str) == 10:
            date = datetime.strptime(date_str, "%Y-%m-%d")
            return 1000 <= date.year and date <= today
    except ValueError:
        return False

    return False


def valid_ymd(year: int, month: int | None = None, day: int | None = None, today: datetime | None = None) -> str | None:
    today = today or current_date()

    if not (1000 <= year <= today.year):
        return None

    if month is None:
        out = f"{year:04d}"
    elif day is None:
        if not (1 <= month <= 12):
            return None
        out = f"{year:04d}-{month:02d}"
    else:
        if not (1 <= month <= 12):
            return None
        try:
            datetime(year, month, day)
        except ValueError:
            return None
        out = f"{year:04d}-{month:02d}-{day:02d}"

    return out if validate_date_not_future(out, today=today) else None


def year_from_two_digits(two_digits: int, today: datetime | None = None) -> int:
    today = today or current_date()
    pivot = today.year % 100
    return 2000 + two_digits if two_digits <= pivot else 1900 + two_digits


def context_near(text: str, start: int, end: int, width: int = 40) -> bool:
    left = text[max(0, start - width):start]
    right = text[end:min(len(text), end + width)]
    return bool(DATE_CONTEXT_RE.search(left) or DATE_CONTEXT_RE.search(right))


def strip_archive_wrapper(text: str) -> str:
    # Ignore IA snapshot timestamps; examine only the archived original URL.
    # Handles:
    #   https://web.archive.org/web/20141018022010/https://example.com/2014/10/foo
    #   https://web.archive.org/web/20100915000000*/http://example.com
    return re.sub(
        r"https?://web\.archive\.org/web/\d{1,14}\*?/(https?://\S+)",
        lambda m: unquote(m.group(1)),
        text,
        flags=re.IGNORECASE,
    )


def host_matches(host: str, pattern: re.Pattern[str]) -> bool:
    host = host.lower()
    return bool(pattern.search(host))


def first_url(text: str) -> str | None:
    m = re.search(r"https?://[^\s)>\]\"']+", text)
    return m.group(0) if m else None


def all_urls(text: str) -> Iterable[str]:
    return re.findall(r"https?://[^\s)>\]\"']+", text)


def extract_arxiv(text: str, today: datetime) -> Candidate | None:
    # New arXiv IDs: YYMM.NNNNN. The suffix is an ID, not a day.
    for m in re.finditer(r"(?i)(?:arxiv[:./ ]+|arxiv\.org/(?:abs|pdf)/)(\d{2})(0[1-9]|1[0-2])\.\d{4,5}", text):
        yy, mm = int(m.group(1)), int(m.group(2))
        year = year_from_two_digits(yy, today=today)
        date = valid_ymd(year, mm, today=today)
        if date:
            return Candidate(date, "arxiv")

    return None


def extract_unix_timestamp(text: str, today: datetime) -> Candidate | None:
    stripped = text.strip()
    if not re.fullmatch(r"\d{10}", stripped):
        return None

    ts = int(stripped)
    try:
        dt = datetime.fromtimestamp(ts, timezone.utc).replace(tzinfo=None)
    except (OverflowError, OSError, ValueError):
        return None

    if datetime(2000, 1, 1) <= dt <= today:
        return Candidate(dt.strftime("%Y-%m-%d"), "unix timestamp")

    return None


def extract_month_name_dates(text: str, today: datetime) -> Candidate | None:
    # July 31, 2024
    pattern1 = re.compile(
        r"(?i)\b("
        + "|".join(sorted(MONTHS, key=len, reverse=True))
        + r")\.?\s+([0-3]?\d)(?:st|nd|rd|th)?[,]?\s+((?:1|2)\d{3})\b"
    )

    for m in pattern1.finditer(text):
        month = MONTHS[m.group(1).lower()]
        day = int(m.group(2))
        year = int(m.group(3))
        # Accept full natural-language dates. They are explicit dates, not hidden guesses.
        date = valid_ymd(year, month, day, today=today)
        if date:
            return Candidate(date, "month name")

    # 29 Jul 2025 / 10 February 1991 / first of May 2023
    pattern2 = re.compile(
        r"(?i)\b([0-3]?\d|first|second|third)\s+(?:of\s+)?("
        + "|".join(sorted(MONTHS, key=len, reverse=True))
        + r")\.?[,]?\s+((?:1|2)\d{3})\b"
    )

    ordinals = {"first": 1, "second": 2, "third": 3}
    for m in pattern2.finditer(text):
        day_text = m.group(1).lower()
        day = ordinals.get(day_text, int(day_text) if day_text.isdigit() else 0)
        month = MONTHS[m.group(2).lower()]
        year = int(m.group(3))
        date = valid_ymd(year, month, day, today=today)
        if date:
            return Candidate(date, "month name")

    return None


def normalize_numeric_date(year: str, month: str | None = None, day: str | None = None, today: datetime | None = None) -> str | None:
    today = today or current_date()

    y = int(year.lstrip("0") or "0")
    if y < 100:
        y = year_from_two_digits(y, today=today)

    if month is None:
        return valid_ymd(y, today=today)

    m = int(month.lstrip("0") or "0")

    if day is None:
        return valid_ymd(y, m, today=today)

    d = int(day.lstrip("0") or "0")
    return valid_ymd(y, m, d, today=today)


def extract_labeled_numeric_dates(text: str, today: datetime) -> Candidate | None:
    # Full ISO-ish dates, allowing unambiguous extra leading zeros in scraped typos.
    for m in re.finditer(r"(?<!\d)(0?\d{4})[-/.](0{0,2}\d{1,2})[-/.](0{0,2}\d{1,2})(?!\d)", text):
        start, end = m.span()
        candidate = normalize_numeric_date(m.group(1), m.group(2), m.group(3), today=today)

        if not candidate:
            continue

        # Accept if it is clearly metadata or starts the record. Otherwise URL extraction handles path dates.
        if start == 0 or text[max(0, start - 3):start].strip(" ,;(") == "" or context_near(text, start, end):
            return Candidate(candidate, "labeled numeric date")

    # YYYYMMDDHHMMSS / YYYYMMDD with timestamp/date context.
    for m in re.finditer(r"(?<!\d)((?:1|2)\d{3})(0[1-9]|1[0-2])(0[1-9]|[12]\d|3[01])(?:\d{6})?(?!\d)", text):
        start, end = m.span()
        candidate = normalize_numeric_date(m.group(1), m.group(2), m.group(3), today=today)
        if candidate and context_near(text, start, end):
            return Candidate(candidate, "compact labeled date")

    # MM/DD/YY, DD/MM/YY. Only unambiguous cases.
    for m in re.finditer(r"(?<!\d)(\d{1,2})/(\d{1,2})/(\d{2,4})(?!\d)", text):
        start, end = m.span()
        a, b, y = int(m.group(1)), int(m.group(2)), m.group(3)

        if a > 12 and b <= 12:      # DD/MM/YYYY
            day, month = a, b
        elif b > 12 and a <= 12:    # MM/DD/YYYY
            month, day = a, b
        else:
            continue

        year = int(y)
        if year < 100:
            year = year_from_two_digits(year, today=today)

        candidate = valid_ymd(year, month, day, today=today)
        if candidate and context_near(text, start, end):
            return Candidate(candidate, "slash date")

    # 24-01-15 / 95-01-15 with explicit date context.
    for m in re.finditer(r"(?<!\d)(\d{2})[-.](0[1-9]|1[0-2])[-.](0[1-9]|[12]\d|3[01])(?!\d)", text):
        start, end = m.span()
        if not context_near(text, start, end):
            continue

        year = year_from_two_digits(int(m.group(1)), today=today)
        candidate = valid_ymd(year, int(m.group(2)), int(m.group(3)), today=today)
        if candidate:
            return Candidate(candidate, "two-digit year date")

    # Year-month in explicit metadata contexts.
    for m in re.finditer(r"(?<!\d)((?:1|2)\d{3})[-/.](0?[1-9]|1[0-2])(?![-/.\d])", text):
        start, end = m.span()
        if context_near(text, start, end):
            candidate = normalize_numeric_date(m.group(1), m.group(2), today=today)
            if candidate:
                return Candidate(candidate, "labeled year-month")

    # ISO week dates: preserve only the year.
    for m in re.finditer(r"(?<!\d)((?:1|2)\d{3})-W(?:0[1-9]|[1-4]\d|5[0-3])(?!\d)", text):
        start, end = m.span()
        if context_near(text, start, end):
            candidate = valid_ymd(int(m.group(1)), today=today)
            if candidate:
                return Candidate(candidate, "ISO week year")

    # Quarters: only the year is explicit enough.
    for m in re.finditer(r"(?i)\bQ[1-4]\s+((?:1|2)\d{3})\b|\b((?:1|2)\d{3})Q[1-4]\b", text):
        start, end = m.span()
        if context_near(text, start, end):
            year = int(m.group(1) or m.group(2))
            candidate = valid_ymd(year, today=today)
            if candidate:
                return Candidate(candidate, "quarter year")

    # Bare publication year with context.
    for m in re.finditer(r"(?<![\dA-Za-z.])((?:1|2)\d{3})(?:\.)?(?![\dA-Za-z.])", text):
        start, end = m.span()
        if context_near(text, start, end):
            candidate = valid_ymd(int(m.group(1)), today=today)
            if candidate:
                return Candidate(candidate, "labeled year")

    return None


def extract_date_from_url_path(url: str, today: datetime) -> Candidate | None:
    parsed = urlparse(url)
    host = parsed.netloc.lower().split("@")[-1].split(":")[0]

    if not host:
        return None

    if host_matches(host, BAD_URL_HOST_RE):
        return None

    # Do not treat DOI/article accession numerals as dates without an explicit date-coded scheme.
    if host_matches(host, UNTRUSTED_DATE_CODE_HOST_RE):
        # bioRxiv/medRxiv are date-coded DOIs under 10.1101; keep year-month only.
        m = re.search(r"/10\.1101/((?:1|2)\d{3})\.(0[1-9]|1[0-2])\.(0[1-9]|[12]\d|3[01])\.", parsed.path)
        if m and ("biorxiv.org" in host or "medrxiv.org" in host):
            candidate = valid_ymd(int(m.group(1)), int(m.group(2)), today=today)
            return Candidate(candidate, "preprint DOI month") if candidate else None
        return None

    path = unquote(parsed.path)
    segments = [seg for seg in path.split("/") if seg]

    # arXiv-style URL extraction is handled separately.
    if "arxiv.org" in host:
        return None

    # bioRxiv/medRxiv date-coded DOI prefixes. Use only year-month; day may not be publication day.
    if "biorxiv.org" in host or "medrxiv.org" in host:
        m = re.search(r"/10\.1101/((?:1|2)\d{3})\.(0[1-9]|1[0-2])\.(0[1-9]|[12]\d|3[01])\.", parsed.path)
        if m:
            candidate = valid_ymd(int(m.group(1)), int(m.group(2)), today=today)
            return Candidate(candidate, "preprint DOI month") if candidate else None

    # /YYYY/MM/DD/... and /YYYY/MM/...
    for idx in range(len(segments) - 1):
        if re.fullmatch(r"(?:1|2)\d{3}", segments[idx]) and re.fullmatch(r"0?[1-9]|1[0-2]", segments[idx + 1]):
            year = int(segments[idx])
            month = int(segments[idx + 1])

            if idx + 2 < len(segments):
                next_segment = segments[idx + 2]

                if re.fullmatch(r"0?[1-9]|[12]\d|3[01]", next_segment):
                    candidate = valid_ymd(year, month, int(next_segment), today=today)
                    if candidate:
                        return Candidate(candidate, "URL /YYYY/MM/DD/")

                # Some news URLs use /YYYY/MM/YYMMDD-slug.
                m = re.match(r"(\d{2})(0[1-9]|1[0-2])(0[1-9]|[12]\d|3[01])(?:\D|$)", next_segment)
                if m and int(m.group(1)) == year % 100 and int(m.group(2)) == month:
                    candidate = valid_ymd(year, month, int(m.group(3)), today=today)
                    if candidate:
                        return Candidate(candidate, "URL /YYYY/MM/YYMMDD-slug")

            candidate = valid_ymd(year, month, today=today)
            if candidate:
                return Candidate(candidate, "URL /YYYY/MM/")

    # /YYYY-MM-DD-slug or /YYYYMMDD-slug in a blog/news-ish path.
    for idx, seg in enumerate(segments):
        prev = segments[idx - 1].lower() if idx else ""
        is_blogish = prev in BLOGISH_SEGMENTS or any(s.lower() in BLOGISH_SEGMENTS for s in segments[:idx])

        m = re.match(r"((?:1|2)\d{3})[-_](0[1-9]|1[0-2])[-_](0[1-9]|[12]\d|3[01])(?:\D|$)", seg)
        if m and is_blogish:
            candidate = valid_ymd(int(m.group(1)), int(m.group(2)), int(m.group(3)), today=today)
            if candidate:
                return Candidate(candidate, "URL YYYY-MM-DD slug")

        m = re.match(r"((?:1|2)\d{3})(0[1-9]|1[0-2])(0[1-9]|[12]\d|3[01])(?:\D|$)", seg)
        if m and is_blogish:
            candidate = valid_ymd(int(m.group(1)), int(m.group(2)), int(m.group(3)), today=today)
            if candidate:
                return Candidate(candidate, "URL YYYYMMDD slug")

        # /blog/202601-title -> YYYY-MM.
        m = re.match(r"((?:1|2)\d{3})(0[1-9]|1[0-2])(?:\D|$)", seg)
        if m and is_blogish:
            candidate = valid_ymd(int(m.group(1)), int(m.group(2)), today=today)
            if candidate:
                return Candidate(candidate, "URL YYYYMM slug")

        # /blog/260224.html -> YYMMDD, only in blog-like paths.
        m = re.fullmatch(r"(\d{2})(0[1-9]|1[0-2])(0[1-9]|[12]\d|3[01])(?:\.\w+)?", seg)
        if m and is_blogish:
            year = year_from_two_digits(int(m.group(1)), today=today)
            candidate = valid_ymd(year, int(m.group(2)), int(m.group(3)), today=today)
            if candidate:
                return Candidate(candidate, "URL YYMMDD basename")

    # /posts/2024/foo or /blog/2024/foo -> year only.
    for idx, seg in enumerate(segments):
        if re.fullmatch(r"(?:1|2)\d{3}", seg):
            prev = segments[idx - 1].lower() if idx else ""
            if prev in BLOGISH_SEGMENTS:
                candidate = valid_ymd(int(seg), today=today)
                if candidate:
                    return Candidate(candidate, "URL year segment")

    return None


def deterministic_extract(text: str, today: datetime | None = None) -> Candidate | None:
    today = today or current_date()
    original = text.strip()
    if not original:
        return None

    text = strip_archive_wrapper(original)

    # Wikipedia URLs are explicitly excluded.
    for url in all_urls(text):
        parsed = urlparse(url)
        host = parsed.netloc.lower().split("@")[-1].split(":")[0]
        if "wikipedia.org" in host:
            return None

    # Exact machine-readable cases.
    for extractor in (extract_unix_timestamp, extract_arxiv):
        candidate = extractor(text, today)
        if candidate:
            return candidate

    # Explicit metadata in plain text. Prefer full written dates before bare numeric years.
    candidate = extract_month_name_dates(text, today)
    if candidate:
        return candidate

    candidate = extract_labeled_numeric_dates(text, today)
    if candidate:
        return candidate

    # URL path dates. Query strings, fragments, IDs, and archive snapshot dates are ignored.
    for url in all_urls(text):
        candidate = extract_date_from_url_path(url, today)
        if candidate:
            return candidate

    # Bare local file paths such as /doc/cs/2014-deoliveira.pdf can safely give the year only.
    # Do not hallucinate exact publication days from these file names.
    for m in re.finditer(r"(?<!\d)/(?:doc|docs|pdf|papers)/[^\s]*?/((?:1|2)\d{3})-[A-Za-z][^/\s]*\.pdf\b", text):
        candidate = valid_ymd(int(m.group(1)), today=today)
        if candidate:
            return Candidate(candidate, "local PDF year")

    return None


def should_skip_model(target: str) -> bool:
    """
    Return True when there is no deterministic positive date, and the remaining
    evidence is from URL/ID families that should not be guessed from.

    This is the guard that prevents bare PMC/Nature/PLOS/HN/Twitter/GitHub/etc.
    URLs from falling through to the LLM and getting a plausible hallucinated date.
    Exact manually verified exceptions still work because exact_example_lookup()
    runs before this guard.
    """
    original_urls = list(all_urls(target))
    stripped = strip_archive_wrapper(target.strip())
    urls = list(all_urls(stripped)) or original_urls

    if not urls:
        return False

    for url in urls:
        parsed = urlparse(url)
        host = parsed.netloc.lower().split("@")[-1].split(":")[0]

        if not host:
            continue

        if host == "web.archive.org" or host.endswith(".web.archive.org"):
            return True

        if host_matches(host, BAD_URL_HOST_RE) or host_matches(host, UNTRUSTED_DATE_CODE_HOST_RE):
            return True

    return False


def exact_example_lookup(target: str) -> str | None:
    """
    Look up exact examples before regex or model inference.

    Iterate in reverse so later corrective examples override earlier broad/negative examples.
    This matters because a URL may first appear as a conservative prompt example returning "",
    and later as an exact manually verified date.
    """
    keys = {target}

    url = first_url(target)
    if url:
        keys.add(url)

    for row in reversed(EXAMPLES):
        inp, out, _show_in_prompt = unpack_example(row)
        if inp in keys:
            return out

    return None


INSTRUCTIONS = """\
Task: extract one visible date from the input.

Return exactly one line:
- YYYY
- YYYY-MM
- YYYY-MM-DD
- ""

Prefer precision actually supported by the input. If only a year is visible, return YYYY. If year+month are visible, return YYYY-MM. If a full valid date is visible, return YYYY-MM-DD.

Be conservative. Do not make up, remember, browse, infer, or approximate publication dates.

A date is valid only if it is between 1000AD and CURRENT_DATE inclusive.

Good evidence:
- explicit absolute dates after labels such as published, posted, created, submitted, accepted, updated, modified, release date, timestamp;
- clear blog/news URL path dates such as /YYYY/MM/DD/, /YYYY/MM/, YYYY-MM-DD-slug, or YYYYMMDD-slug;
- arXiv IDs of the form YYMM.NNNNN, where only YYMM is a date;
- bioRxiv/medRxiv DOI prefixes of the form 10.1101/YYYY.MM.DD..., but use only YYYY-MM unless the input separately gives an explicit publication day;
- exact calendar dates written in words, such as July 31, 2024.

Bad evidence:
- article IDs, DOI suffixes, PMC/PMID IDs, Hacker News IDs, Twitter/X snowflakes, GitHub issue IDs, YouTube parameters, phone numbers, ISBNs, product numbers, issue numbers, room numbers, accession numbers, model names, software versions, scores, ratios, and hex colors;
- dates in Wikipedia URLs or article titles;
- Internet Archive snapshot timestamps. Ignore the snapshot timestamp and examine only the archived original URL;
- years in titles/topics/events, such as “The 2024 Olympic Games”, “The LessWrong 2022 Review”, “Postcard From 1952”, “What o3 Becomes by 2028”, or “Reflections on 2025”;
- dates that require world knowledge, such as event schedules, anniversaries, release histories, political terms, model-release dates, or publication dates remembered from the web.

If there are several valid dates, return the first explicit publication-like date left-to-right. Skip invalid calendar dates. For ranges, return the earliest endpoint only when the range itself is a publication/copyright/academic-date range; return "" when the range is the article’s subject matter.

If unsure, return "".
Do not add commentary.
"""

# TODO: No self-test runner, which could automate self-testing in the future, perhaps as part of sync.sh, like 'everyNDays 90 && date-guesser.py --test'?. The EXAMPLES list is tailor-made for this — a --test mode that iterates EXAMPLES, runs main-logic on each input, and asserts output matches would catch regressions every time you touch the regexes or prompt. Given the density of corner cases (leap years, pivot-year logic, archive wrappers, URL path dialects), this is the single most valuable addition. Maybe 30 lines.
EXAMPLES: list[ExampleRow] = [
    ("Published: 02-29-2024 | https://example.com/leap-year-article", "2024-02-29"),
    ("Published: 02-29-2023 | https://example.com/another-leap-year-article", EMPTY),
    ("Date: 04/31/2024: Article about calendars", EMPTY),
    ("Created: 2024.02.29", "2024-02-29"),
    ("Last modified: 31/12/2023 23:59:59 UTC", "2023-12-31"),
    ("EST Release Date: 12/13/23", "2023-12-13"),
    ("Published Date: 13/12/23", "2023-12-13"),
    ("Updated 24-01-15", "2024-01-15"),
    ("Updated 95-01-15", "1995-01-15"),
    ("Posted: Yesterday at 3pm", EMPTY),
    ("today is 2025-09-30; posted 8 days ago", "2025-09-22"),
    ("Updated: 2 days ago", EMPTY),
    ("Article accepted 2024-02-30, published 2024-03-01", "2024-03-01"),
    ("Written 2024/04/31: Edited 2024/05/01", "2024-05-01"),
    ("Released between 2023Q4 and 2024Q1", "2023"),
    ("Copyright (c) 2020-2024", "2020"),
    ("Volume 23, Issue 45 (Winter 2023-2024)", "2023"),
    ("Academic Year 2023/24", "2023"),
    ("The 2024 Olympic Games in Paris: A Look Ahead - Sports Illustrated", EMPTY),
    ("The LessWrong 2022 Review", EMPTY),
    ("What o3 Becomes by 2028", EMPTY),
    ("Reflections on 2025", EMPTY),
    ("Postcard From 1952", EMPTY),
    ("SpaceX's Starship: From Prototype to Orbit (2019-2023)", EMPTY),
    ("Apple WWDC 2023 Keynote: iOS 17, macOS 14, and More", EMPTY),
    ("The 240th anniversary of the signing of the Declaration of Independence", EMPTY),
    ("Christmas 2024", EMPTY),
    ("Bitcoin Whitepaper: 15th Anniversary Edition (October 31, 2008)", "2008-10-31"),
    ("this post was submitted on 29 Jul 2025", "2025-07-29"),
    ("Jeffrey Arlo Brown, July 31, 2024", "2024-07-31"),
    ("TimeStamp:20240229123456", "2024-02-29"),
    ("1724256502", "2024-08-21"),
    ("Article 123456789 from 2024-W01", "2024"),
    ("Publication: 2024. Journal of Examples", "2024"),
    ("From the 1990's collection", EMPTY),
    ("Temperature was 2023.5 degrees", EMPTY),
    ("Score was 2024:1", EMPTY),
    ("Published at 2024hrs on Jan 1", EMPTY),
    ("Model: RTX 2080 Ti", EMPTY),
    ("ISBN: 1-234567-890-2024", EMPTY),
    ("Patent №2024-123456", EMPTY),
    ("SKU: 20240123-ABC", EMPTY),
    ("Contact: +1 (202) 555-2024", EMPTY),
    ("hex color #202420", EMPTY),
    ("File:Ganku 岸駒—Tiger and Bamboo—2017-196—Princeton University Art Museum.jpg", EMPTY),
    ("A paper from arXiv:2401.12345", "2024-01"),
    ("DOI: 10.48550/arXiv.2408.08459", "2024-08"),
    ("https://arxiv.org/abs/2408.08459", "2024-08"),
    ("https://www.biorxiv.org/content/10.1101/2024.08.13.607810.full", "2024-08"),
    ("https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.0020100Z", EMPTY),
    ("https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0134152", EMPTY),
    ("https://pmc.ncbi.nlm.nih.gov/articles/PMC8188931/", EMPTY),
    ("https://www.nature.com/articles/s41586-021-03819-2#deepmind", EMPTY),
    ("https://www.nature.com/articles/s41586-025-09761-x#deepmind", EMPTY),
    ("https://doi.org/10.1038/s41586-021-03819-2", EMPTY),
    ("https://www.dwarkeshpatel.com/p/progress-update", EMPTY),
    ("https://if50.substack.com/p/1985-a-mind-forever-voyaging", EMPTY),
    ("https://if50.substack.com/p/2020-scents-and-semiosis", EMPTY),
    ("https://samuelalbanie.substack.com/p/reflections-on-2025", EMPTY),
    ("https://blog.google/products/gemini/gemini-3/", EMPTY),
    ("https://www.anthropic.com/news/claude-opus-4-5", EMPTY),
    ("https://openai.com/index/introducing-gpt-5-2/", EMPTY),
    ("https://github.com/martinarjovsky/WassersteinGAN/issues/2#issuecomment-278710552", EMPTY),
    ("https://x.com/jconorgrogan/status/1820212444016345146", EMPTY),
    ("https://news.ycombinator.com/item?id=36302805", EMPTY),
    ("https://www.youtube.com/watch?v=dQw4w9WgXcQ&t=42s", EMPTY),
    ("https://en.wikipedia.org/wiki/Rams_(2018_film)", EMPTY),
    ("https://en.wikipedia.org/wiki/Revolutions_of_1989", EMPTY),
    ("https://publicdomainreview.org/collection/w-w-denslow-illustrations-wonderful-wizard-of-oz-1900/", EMPTY),
    ("https://notes.stlartsupply.com/the-golden-age-of-japanese-pencils-1952-1967/ The Golden Age of Japanese Pencils, 1952–1967", EMPTY),
    ("https://erikbern.com/2016/04/04/nyc-subway-math", "2016-04-04"),
    ("https://jdstillwater.blogspot.com/2012/05/i-put-toaster-in-dishwasher.html", "2012-05"),
    ("https://3quarksdaily.com/3quarksdaily/2011/06/a-crab-canon-for-douglas-hofstadter.html", "2011-06"),
    ("https://www.bbc.com/future/article/20240201-a-us-engineer-had-a-shocking-plan-to-improve-the-climate-burn-all-coal-on-earth", "2024-02-01"),
    ("https://www.reuters.com/technology/artificial-intelligence/openai-builds-first-chip-with-broadcom-tsmc-scales-back-foundry-ambition-2024-10-29/", "2024-10-29"),
    ("https://www.crunchyroll.com/news/interviews/2024/10/28/thaliarchus-cosmic-warlord-kin-bright-interview", "2024-10-28"),
    ("https://example.com/blog/2022/03/article-slug.html", "2022-03"),
    ("https://smoothbrains.net/posts/2025-04-29-xenon-and-nitrous-oxide.html", "2025-04-29"),
    ("https://lab174.com/blog/202601-yaml-norway/", "2026-01"),
    ("https://lewiscampbell.tech/blog/260224.html Reading English from 1000 AD", "2026-02-24"),
    ("https://willhbr.net/2025/10/20/light-mode-infffffflation/", "2025-10-20"),
    ("https://web.archive.org/web/20110530014638/https://today.msnbc.msn.com/id/43098220/ns/today-today_people/t/after-years-millionaire-misers-heirs-finally-split-m/", EMPTY),
    ("https://web.archive.org/web/20141018022010/https://news.nationalgeographic.com/news/2014/10/141015-better-beef-genetics-science-agriculture-environment-ngfood/", "2014-10-15"),
    ("https://web.archive.org/web/20100915000000*/http://example.com", EMPTY),
    ("https://web.archive.org/web/20191127163535/http://www.aidungeon.io/2019/11/my-orc-band-and-our-quest-for-equal.html", "2019-11"),
    ("/newsletter/2022/index", "2022"),
    ("/doc/cs/2014-deoliveira.pdf", "2014"),
    ("/doc/ai/scaling/2020-bell.pdf#facebook", "2020"),

    # Exact-only verified dates.
    # These are in EXAMPLES so there is no separate metadata file, but they are hidden from the prompt.
    # Use this for opaque IDs, publisher URLs, and local PDFs whose exact dates are known from GTX/Zotero/etc.
    ("https://proceedings.mlr.press/v139/vicol21a.html", "2021-07-01", False),
    ("https://pmc.ncbi.nlm.nih.gov/articles/PMC8188931/", "2021-03-15", False),
    ("https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0134152", "2015-08-12", False),
    ("https://www.nature.com/articles/s41586-021-03819-2#deepmind", "2021-07-15", False),
    ("https://doi.org/10.1038/s41586-021-03819-2", "2021-07-15", False),
    ("/doc/ai/scaling/2020-bell.pdf#facebook", "2020-08-22", False),
    ("/doc/ai/scaling/hardware/2019-roy.pdf", "2019-11-27", False),
    ("/doc/ai/scaling/hardware/2020-jiang.pdf", "2020-11-04", False),
    ("/doc/darknet-market/2020-zhou-2.pdf", "2020-02-01", False),
    ("https://theonion.com/area-man-consults-internet-whenever-possible-1819565463/", "2000-01-26", False),
    ("https://publicdomainreview.org/collection/kreuzigung/ The Language of Form: Lothar Schreyer’s Kreuzigung (1920)", "2025-06-18", False)
]


def build_prompt(target: str, today: datetime) -> str:
    current = today.strftime("%Y-%m-%d")
    rendered_examples = []

    for row in EXAMPLES:
        inp, out, show_in_prompt = unpack_example(row)
        if not show_in_prompt:
            continue

        rendered_examples.append(
            "Input: " + json.dumps(inp, ensure_ascii=False) + "\n"
            "Output: " + out
        )

    return (
        INSTRUCTIONS.replace("CURRENT_DATE", current)
        + "\nExamples:\n\n"
        + "\n\n".join(rendered_examples)
        + "\n\nTask:\n\n"
        + "Input: "
        + json.dumps(target, ensure_ascii=False)
        + "\nOutput:"
    )


def normalize_model_output(raw: str) -> str:
    answer = raw.strip().splitlines()[0].strip() if raw.strip() else EMPTY

    if answer.startswith("Output:"):
        answer = answer[len("Output:"):].strip()

    if answer in {EMPTY, '" "', "''", ""}:
        return EMPTY

    # Repair JSON-quoted non-empty outputs such as "2024-08-21".
    if len(answer) >= 2 and answer[0] == '"' and answer[-1] == '"':
        try:
            decoded = json.loads(answer)
        except json.JSONDecodeError:
            decoded = answer
        if decoded == "":
            return EMPTY
        if isinstance(decoded, str):
            answer = decoded.strip()

    return answer


def ask_model(target: str, today: datetime) -> str:
    client = OpenAI()
    model = os.environ.get("OPENAI_MODEL", "gpt-5-mini")
    prompt = build_prompt(target, today)

    completion = client.chat.completions.create(
        model=model,
        messages=[
            {
                "role": "system",
                "content": "You are a conservative bibliographic metadata extractor. Extract visible dates; do not guess hidden dates.",
            },
            {"role": "user", "content": prompt},
        ],
        temperature=1,  # current OpenAI reasoning models require the default temperature
        top_p=1,
        seed=0,
    )

    return normalize_model_output(completion.choices[0].message.content)


def main() -> None:
    if len(sys.argv) == 1:
        target = sys.stdin.read().strip()
    else:
        target = sys.argv[1].strip()

    today = current_date()

    exact = exact_example_lookup(target)
    if exact is not None:
        if exact == EMPTY:
            print(EMPTY)
            return
        if validate_date_format(exact) and validate_date_not_future(exact, today=today):
            print(exact)
            return
        print(EMPTY)
        return

    candidate = deterministic_extract(target, today=today)
    if candidate:
        print(candidate.date)
        return

    if should_skip_model(target):
        print(EMPTY)
        return

    date_str = ask_model(target, today=today)

    if date_str == EMPTY:
        print(EMPTY)
        return

    if not validate_date_format(date_str) or not validate_date_not_future(date_str, today=today):
        print(EMPTY)
        return

    print(date_str)


if __name__ == "__main__":
    main()
