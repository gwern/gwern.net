#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# latex2unicode.py: Convert a simple inline TeX/LaTeX (aimed at ArXiv abstracts) into Unicode+HTML+CSS, using the OA API.
# Author: Gwern Branwen
# Date: 2023-06-28
# When:  Time-stamp: "2026-06-10 15:32:33 gwern"
# License: CC-0
#
# Usage: $ OPENAI_API_KEY="sk-XXX" xclip -o | python latex2unicode.py
#
# Typesetting TeX/LaTeX for web browsers is typically a heavyweight operation; even if done server-side, display often requires a lot of CSS+fonts. And then the result looks highly unnatural and clearly 'alien', interrupting reading flow. This is worthwhile for complex equations, where browser typesetting is not up to snuff, but for many in-the-wild TeX uses, the use is often as simple as `$X$`, which would look better as `<em>X</em>` & take megabytes less to render. So it is desirable for simple TeX expressions to convert them to 'native' Unicode/HTML (augmented with a bit of custom CSS to handle things like superscripts-over-subscripts which pop up in integrals/summations/binomials/matrices etc).
# Unfortunately, TeX is an irregular macro language which is hard to parse and 'compile' to Unicode: it's easy to do many examples, but there's a long tail of weird variables, formatting commands etc, which means that I wind up defining lots of rewrites by hand, even though they are usually pretty 'obvious'. So, quite tedious and unrewarding.
# However, this is a perfect use-case for GPT models: it is hard to write comprehensive rules for, but is an extremely constrained problem in a domain it knows well which requires processing few tokens, where I can give it many few-shot examples, interrogate it for edge-cases to then write rules/examples for, and the harm of an error is relatively minimal (anyone seriously using an equation will need to read the original anyway, so won't be fooled by a wrong translation).
# So we write down a list of general rules, then a bunch of specific examples, then ask GPT-4 to translate from TeX to Unicode/HTML/CSS.
# The output is post-validated by a lint (tag balance, no digits/operators/punctuation inside `<em>`); on lint failure we retry once, then fail explicitly by emitting the input unchanged & exiting non-zero.
#
# eg.
# $ echo 'a + b = c^2' | python3 latex2unicode.py
# <em>a</em> + <em>b</em> = <em>c</em><sup>2</sup>
#
# Bonus feature: LLMs are smart enough to generalize, so free-form natural language inputs may also work:
#
# $ echo 'x times 2 but raised to 1/3rds' | latex2unicode.py
# <em>x</em> × 2<sup>1⁄3</sup>
# $ echo 'asymptotically square root n' | latex2unicode.py
# 𝒪(√<em>n</em>)
#
# NOTE: this is intended only for using clean TeX and compiling to something usable in HTML/Markdown. For converting from an image or screenshot to TeX, see tools like <https://github.com/lukas-blecher/LaTeX-OCR> or <https://github.com/VikParuchuri/texify> or <https://mathpix.com/snipping-tool> (or prompting a VLM like Claude-3 or GPT-4o-V with an image & request)

import re
import sys

from openai import OpenAI

prompt_preamble = """
Task: Convert LaTeX inline expressions from ArXiv-style TeX math to inline Unicode+HTML+CSS, for easier reading in web browsers.

Task example:

Input to convert: <span class="math inline">\\(H\\gg1\\)</span>
Converted output: <em>H</em> ≫ 1

Details:

- Convert only if the result is unambiguous.
- Note that inputs may be very short, because each LaTeX fragment in an abstract is processed individually. Many inputs will be as short as a single letter (which are variables).
- ITALICIZATION (most important rule): italicize, with `<em>`, *only* individual Latin-letter variables. Never italicize digits, operators (+, −, =, ×, ⋅), punctuation (commas, primes, parentheses, brackets, braces), fraction slashes, or multi-letter function/operator/unit/class names (log, sin, max, sigmoid, Pr, lim, TC, SPACE, g, but note single-letter *variables* are still italicized). Apply `<em>` at the finest possible granularity, tagging each variable letter separately:
    - `x_{t+1}` → `<em>x</em><sub><em>t</em>+1</sub>` (CORRECT)
    - `x_{t+1}` → `<em>x<sub>t+1</sub></em>` (WRONG: italicizes '+' and '1')
    - `f(x)` → `<em>f</em>(<em>x</em>)` (CORRECT)
    - `f(x)` → `<em>f(x)</em>` (WRONG: italicizes the parentheses)
    - `m'` → `<em>m</em>′` (CORRECT)
    - `m'` → `<em>m′</em>` (WRONG: italicizes the prime)
    - `M_a` → `<em>M</em><sub><em>a</em></sub>` (CORRECT)
    - `M_a` → `<em>M<sub>a</sub></em>` (WRONG: do not wrap whole tokens; tag each letter)
  Combining diacritics attached to a variable stay inside its `<em>` (eg. `\\hat r` → `<em>r̂</em>`). The exception is `\\textit{...}`, which is ordinary text emphasis and is converted as a whole (`\\textit{zero-shot}` → `<em>zero-shot</em>`). When in doubt, leave a character upright.
  Adjacent single-letter variables may share one `<em>` (eg. a product `SAT` of variables <em>S</em>, <em>A</em>, <em>T</em> → `<em>SAT</em>`), since the rendering is identical—but only if every character in the run is a variable letter. Distinguish variable products from multi-letter *names* (SAT solver, ReLU, ResNet), which stay upright.
- GREEK LETTERS: render as plain upright Unicode Greek (α, Δ, Λ, θ) with no `<em>`; do not use the Mathematical Italic Greek block (U+1D6C2 etc.), which renders unreliably across fonts. Descriptive/label subscripts (eg. `M_{PBH}`, `x_{max}`) are upright; single-letter variable subscripts are italicized individually.
- PARTICLE PHYSICS: particle symbols and their quark-flavor sub/superscripts are conventionally upright: `\\Lambda_c` → `Λ<sub>c</sub>`, `p\\bar{p}` → `pp̅`, `\\mu^+` → `μ<sup>+</sup>`.
- Assume only default environment settings with no redefinitions or uses like `\\newcommand` or `\\begin`. Skip custom operators.
- Do not modify block-level equations, or complex structures such as diagrams or tables or arrays or matrices (eg `\\begin{bmatrix}`), or illustrations such as drawn by TikZ or `\\draw` , as those require special processing (eg. matrixes must be converted into HTML tables). Do not convert them & simply repeat it if the input is not an inline math expression.
- If a TeX command has no reasonable Unicode equivalent, such as the `\\overrightarrow{AB}`/`\\vec{AB}` or `\\check{a}` or `\\underline`/`\\overline` commands in LaTeX, simply repeat it.
- If a TeX command merely adjusts positioning, size, or margin (such as `\\raisebox`/`\\big`/`\\Big`), always omit it from the conversion (as it is probably unnecessary & would need to be handled specially if it was).
- The TeX/LaTeX special glyphs (`\\TeX` & `\\LaTeX`) are handled elsewhere; do not convert them, but simply repeat it.
- Use Unicode entities, eg. MATHEMATICAL CAPITAL SCRIPT O `𝒪` in place of `\\mathcal{O}`, and likewise for the Fraktur ones (`\\mathfrak`) and bold ones (`\\mathbb`). Convert to the closest Unicode entity that exists. Convert symbols, special symbols, mathematical operators, and Greek letters. Convert even if the Unicode is rare (such as  `𝒪`). If there is no Unicode equivalent (such as because there is not a matching letter in that font family, or no appropriate combining character), then do not convert it.
- If there are multiple reasonable choices, such as  `\\approx` which could be represented as `≈` or `~`, choose the simpler-looking one. Do not choose the complex one unless there is some good specific reason for that.
- For superimposed subscript+superscript, such as binomials, use our predefined CSS <span> class `subsup`, eg. `(\\Delta^0_n)` → `Δ<span class="subsup"><sub><em>n</em></sub><sup>0</sup></span>`; `\\Xi_{cc}^{++} = ccu` → `Ξ<span class="subsup"><sub>cc</sub><sup>++</sup></span> = ccu`; `\\,\\Lambda_c \\Lambda_c \\to \\Xi_{cc}^{++}\\,n\\,` → `&#8201;Λ<sub>c</sub> Λ<sub>c</sub> → Ξ<span class="subsup"><sub>cc</sub><sup>++</sup></span>&#8201;n&#8201;`. This is also useful for summations or integrals, such as `\\int_a^b f(x) dx` → `∫<span class="subsup"><sub><em>a</em></sub><sup><em>b</em></sup></span> <em>f</em>(<em>x</em>) <em>dx</em>`.

    - NOTE: the '<sub>' must come before '<sup>', for Pandoc compatibility. (Hence the name 'subsup' rather than 'supsub'.)
- For small fractions, where both numbers are 3 integer digits or less, use FRACTION SLASH (⁄) to convert (eg. `1/2` or `\\frac{1}{2}` → `1⁄2`). Do not use the Unicode fractions like VULGAR FRACTION ONE HALF `½`.
- For symbolic or large fractions, where one argument is a letter or symbol or >3 integer digits, use U+29F8 BIG SOLIDUS (⧸) instead, like '<em>a</em>⧸<em>b</em>'.
- For complex fractions which use superscripts or subscripts, multiple arguments etc, do not convert them & simply repeat them. eg. do not convert `\\(\\frac{a^{b}}{c^{d}}\\)`, as it is too complex.
- Convert roots such as square or cube roots if that would be unambiguous. For example, `\\sqrt[3]{8}` → `∛8` is good, but not `\\sqrt[3]{ab}` because `∛<em>ab</em>` is ambiguous; do not convert complex roots like `\\sqrt[3]{ab}`.

    For multi-token radicands, convert by wrapping the radicand in parentheses, eg. `\\sqrt{H^3 SAT}` → `√(<em>H</em><sup>3</sup><em>SAT</em>)`; never emit a bare ambiguous `√<em>ab</em>`.
- Color & styling: if necessary, you may use simple CSS inline with a `<span style="">` declaration, such as to color something blue using `<span style="color: blue">`.
- Outlines/boxes: you may use simple inline CSS to draw borders.
- Be careful about dash use: correctly use MINUS SIGN (−) vs EM DASH (—) vs EN DASH (–) vs HYPHEN-MINUS (-).

More rules/examples for edge-cases:

- ' O(1)'
 𝒪(1)
- '<span class="math inline">\\(\\mathsf{TC}^0\\)</span>'
<strong>TC</strong><sup>0</sup>
- '<span class="math inline">\\(\\approx\\)</span>'
~
- '<span class="math inline">\\(1-\\tilde \\Omega(n^{-1/3})\\)</span>'
1 − Ω̃(<em>n</em><sup>−1⁄3</sup>)
- '<span class="math inline">\\(\\mathbf{R}^3\\)</span>'
𝐑<sup>3</sup>
- '<span class="math inline">\\(\\ell_p\\)</span>'
𝓁<sub><em>p</em></sub>
- '\\textcircled{r}'
ⓡ
- '(\\nabla \\log p_t\\)'
∇ log <em>p</em><sub><em>t</em></sub>
- '\\(\\partial_t u = \\Delta u + \\tilde B(u,u)\\)'
∂<sub><em>t</em></sub><em>u</em> = Δ<em>u</em> + <em>B̃</em>(<em>u</em>, <em>u</em>)
- '\\(1 - \\frac{1}{e}\\)'
1 − 1⧸<em>e</em>
- 'O(\\sqrt{T}'
𝒪(√<em>T</em>)
- '<span class="math inline">\\(^\\circ\\)</span>'
°
- '<span class="math inline">\\(^\\bullet\\)</span>'
•
- '6\\times 10^{-6}\\)'
6 × 10<sup>−6</sup>
- '5\\div10'
5 ÷ 10
- '\\Pr(\\text{text} | \\alpha)'
Pr(text | α)
- '<span class="math inline">\\(\\hbar\\)</span>'
ℏ
- '\\frac{1}{2}'
1⁄2
- '\\nabla'
∇
- '<span>\\(r \\to\\infty\\)</span>'
<em>r</em> → ∞
- '\\hat{a}'
<em>â</em>
- '\\textit{zero-shot}'
<em>zero-shot</em>
- '\\(f(x) = x \\cdot \\text{sigmoid}(\\beta x)\\)'
<em>f</em>(<em>x</em>) = <em>x</em> ⋅ sigmoid(β <em>x</em>)
- '\\clubsuit'
♣
- '\\textcolor{red}{x}'
<span style="color: red"><em>x</em></span>
- '\\textcolor{red}{X}'
<span style="color: red"><em>X</em></span>
- '\\textbf{bolding}'
<strong>bolding</strong>
- '\\textit{emphasis}'
<em>emphasis</em>
- 'B'
<em>B</em>
- 'u'
<em>u</em>
- 'X + Y'
<em>X</em> + <em>Y</em>
- '\\,\\Lambda_b \\Lambda_b \\to \\Xi_{bb}\\,N\\,'
&#8201;Λ<sub>b</sub> Λ<sub>b</sub> → Ξ<sub>bb</sub>&#8201;N&#8201;
- 'x \\in (-\\infty, \\infty)'
<em>x</em> ∈ (−∞, ∞)
- 'p\\bar{p} \\to \\mu^+\\mu^-'
pp̅ → μ<sup>+</sup>μ<sup>−</sup>
- '\\alpha\\omega\\epsilon\\S\\o m\\in'
αωε§øm∈
- '^2H ^6Li ^{10}B ^{14}N'
<sup>2</sup>H <sup>6</sup>Li <sup>10</sup>B <sup>14</sup>N
- '\\mathcal{L} \\mathcal{H} \\mathbb{R} \\mathbb{C}'
ℒ ℋ ℝ ℂ
- '\\textrm{M}_\\odot'
M<sub>☉</sub>
- '10^{-16} - 10^{-10} \\sim \\textrm{M}_\\odot'
10<sup>−16</sup>–10<sup>−10</sup> M<sub>☉</sub>
- '200+'
200+
- 'M = M_a \\cup M_b \\subseteq \\mathbb{R}^d'
<em>M</em> = <em>M</em><sub><em>a</em></sub> ∪ <em>M</em><sub><em>b</em></sub> ⊆ ℝ<sup><em>d</em></sup>
- 'f : \\mathbb{R}^d \\to \\mathbb{R}^p'
<em>f</em> : ℝ<sup><em>d</em></sup> → ℝ<sup><em>p</em></sup>
- 'M_a'
<em>M</em><sub><em>a</em></sub>
- 'β_k\\bigl(f(M_i)\\bigr) = 0'
β<sub><em>k</em></sub>(<em>f</em>(<em>M</em><sub><em>i</em></sub>)) = 0
- 'k \\ge 1'
<em>k</em> ≥ 1
- 'β_0\\bigl(f(M_i)\\bigr) = 1'
β<sub>0</sub>(<em>f</em>(<em>M</em><sub><em>i</em></sub>)) = 1
- 'i =a, b'
<em>i</em> = <em>a</em>, <em>b</em>
- '(n,d,\\lambda)'
(<em>n</em>, <em>d</em>, λ)
- '\\Lambda'
Λ
- '\\not\\approx'
≉
- '\\left\\langle A \\middle| B \\right\\rangle'
⟨<em>A</em>|<em>B</em>⟩
# note: <https://en.wikipedia.org/wiki/Blackboard_bold#Encoding>: "In Unicode, a few of the more common blackboard bold characters (ℂ, ℍ, ℕ, ℙ, ℚ, ℝ, and ℤ) are encoded in the Basic Multilingual Plane (BMP) in the Letterlike Symbols (2100–214F) area, named DOUBLE-STRUCK CAPITAL C etc. The rest, however, are encoded outside the BMP, in Mathematical Alphanumeric Symbols (1D400–1D7FF), specifically from 1D538–1D550 (uppercase, excluding those encoded in the BMP), 1D552–1D56B (lowercase) and 1D7D8–1D7E1 (digits). Blackboard bold Arabic letters are encoded in Arabic Mathematical Alphabetic Symbols (1EE00–1EEFF), specifically 1EEA1–1EEBB."
- '\\mathcal{R}'
ℛ
- '\\mathbb{R}'
ℝ
- '\\mathbb{N}'
ℕ
- '\\cancel{x}'
<em>x̸</em>
- '\\left{\\frac{1}{2} \\right}'
\\left{\\frac{1}{2} \\right}
- '\\dot{x}'
<em>x&#775;</em>
- '\\ddot{x}'
<em>x&#776;</em>
- 'x^{y^{z}}'
<em>x</em><sup><em>y</em><sup><em>z</em></sup></sup>
- '\\lim_{x \\to \\infty} f(x)'
lim<span class="subsup"><sub><em>x</em> → ∞</sub></span> <em>f</em>(<em>x</em>)
- '\\boxed{A}'
<span style="display: inline-block; border: 1px solid black; padding: 0 3px; margin: 0 2px; line-height: 1.2;"><em>A</em></span>
- '\\,'
&#8201;
- '\\:'
&#8197;
- '\\;'
&#8198;
- '\\quad'
&#8195;
- '\\qquad'
&#8195;&#8195;
- '\\!'

- En space
&#8194;
- Figure space
&#8199;
- Punctuation space
&#8200;
- 'O(m&#39; \\log^2 m&#39;)'
𝒪(<em>m</em>′ log<sup>2</sup> <em>m</em>′)
- 'n&#39;'
<em>n</em>′
- '$%$'
%
- '%'
%
- "\\(0.90, 0.91, 0.94\\)"
0.90, 0.91, 0.94
- '123/456'
123⁄456
- '123/4567'
123⧸4,567
- '1,234/765'
1,234⧸765
- '5610/987980'
5,610⧸987,980
- '504827'
504,827
- '($(\\frac{202680742}{582771} \\cdot 0.1) \\cdot 100$)'
((202,680,742⧸582,771 ⋅ 0.1) ⋅ 100)
- '740/618'
740⁄618
- '$\\frac{1910}{209} = 9.14$'
1,910⧸209 = 9.14
- '(504827⁄1800) × 1.0 × 100'
(504,827⧸1,800) × 1.0 × 100
- '$n/({\\pi\\over 8}$ lg $n)\\sp{1/2}$'
<em>n</em>⧸(π⧸8 lg <em>n</em>)<sup>1⁄2</sup>
- 'O(\\log n \\operatorname{polyloglog} n)'
𝒪(log <em>n</em> polyloglog <em>n</em>)
- 'r1,... rm'
<em>r</em><sub>1</sub>, ..., <em>r</em><sub><em>m</em></sub>
- '\\(LCSPACE[s,c,e] = CSPACE[\\Theta(s + e \\log c), \\Theta(c)]\\)'
LCSPACE[<em>s</em>, <em>c</em>, <em>e</em>] = CSPACE[Θ(<em>s</em> + <em>e</em> log <em>c</em>), Θ(<em>c</em>)]
- 'M_{PBH} &gt; 1.4 \\times 10^{17} {\\rm g}'
<em>M</em><sub>PBH</sub> &gt; 1.4 × 10<sup>17</sup> g
- '\\(&lt;n\\)'
&lt;<em>n</em>
- '$DyT($x$) = \\tanh(α$x$)$'
DyT(<em>x</em>) = tanh(α<em>x</em>)
- '\\hat r'
<em>r̂</em>
- '$x = \\frac{o \\cdot e - (1 - e)}{o}$'
<em>x</em> = (<em>o</em> ⋅ <em>e</em> − (1 − <em>e</em>))⧸<em>o</em>
- '$\\mathcal{V}$'
𝒱
- '\\(\\sim 10^6 \\mathrm{\\mu Lenat/word}\\)'
~10<sup>6</sup> μLenat⧸word
- '\\322\\'
322
- 'E\\in\\mathbb{R}^{m\\times n}'
<em>E</em> ∈ ℝ<sup><em>m</em> × <em>n</em></sup>
- 'z = \\frac{147.21 - 147.64}{0.145} = -2.96'
<em>z</em> = (147.21 − 147.64)⧸0.145 = −2.96
- '$12,087$'
12,087
- '1,600'
1,600
- '$2*5=10$'
2 ⋅ 5 = 10
- '$32^\\circ\\text{C}$'
32℃
- '$\\binom{m}{2}$'
(<span class="subsup"><sub>2</sub><sup><em>m</em></sup></span>)
- '\\(391,562\\)'
391,562
- 'α=1/(2+θ)'
α = 1⧸(2 + θ)
- 'κ=2-2/(3+θ)'
κ = 2 − 2⧸(3 + θ)
- 'b=(1+c)/2'
<em>b</em> = (1 + <em>c</em>)⧸2
- '$\\tilde{O}(\\sqrt{H^3 SAT})$'
𝒪̃(√(<em>H</em><sup>3</sup> <em>SAT</em>))

Task:

- '"""

# Characters which must never appear inside `<em>`: digits, operators, brackets,
# primes, fraction slashes, multiplication signs, & non-hyphen dashes.
# (HYPHEN-MINUS & comma are deliberately excluded, to permit `\textit` phrases
# like `<em>zero-shot</em>`.)
FORBIDDEN_IN_EM = re.compile(r"[0-9+=()\[\]{}′⁄⧸×⋅−–—]")
# Match an <em> element's contents (no nesting of <em> is legal):
EM_CONTENTS    = re.compile(r"<em>((?:(?!</?em>).)*)</em>", flags=re.DOTALL)
# Numeric/named character references are legitimate inside <em> (eg. combining
# diacritics like `x&#775;`), so strip them before checking for forbidden characters:
ENTITY         = re.compile(r"&#[0-9]+;|&[a-zA-Z]+;")

def lint(html: str) -> list[str]:
    """Return a list of typographic problems in the converted HTML (empty = clean)."""
    problems = []
    for tag in ("em", "sub", "sup", "strong", "span"):
        if html.count("<" + tag) != html.count("</" + tag + ">"):
            problems.append(f"unbalanced <{tag}> tags")
    for m in EM_CONTENTS.finditer(html):
        inner = ENTITY.sub("", m.group(1))
        if FORBIDDEN_IN_EM.search(inner):
            problems.append(f"over-italicization (non-variable character inside <em>): {m.group(0)!r}")
    if re.search(r'<span class="subsup">\s*<sup', html):
        problems.append("<sup> precedes <sub> inside span.subsup (breaks Pandoc compatibility)")
    return problems

def convert(client: OpenAI, target: str) -> str:
    completion = client.chat.completions.create(
        model="gpt-5.4-mini",
        messages=[
            {"role": "system", "content": "You are a skilled mathematician & tasteful typographer, expert in LaTeX."},
            {"role": "user", "content": prompt_preamble + target + "'\n"},
        ],
    )
    return completion.choices[0].message.content.rstrip()

def main() -> None:
    if len(sys.argv) == 1:
        target = sys.stdin.read().strip()
    else:
        target = sys.argv[1]

    client = OpenAI()

    max_attempts = 2
    problems: list[str] = []
    for _attempt in range(max_attempts):
        output = convert(client, target)
        problems = lint(output)
        if not problems:
            print(output, end='') # avoid trailing newline because we might be cleaning inline text & want to avoid injecting newlines
            return

    # Explicit failure: report on stderr, emit input unchanged (the standard
    # 'do not convert; simply repeat' fallback), & exit non-zero so callers can detect it.
    print(f"latex2unicode.py: lint failed after {max_attempts} attempts on input {target!r}:", file=sys.stderr)
    for p in problems:
        print(f"  - {p}", file=sys.stderr)
    print(f"  last output: {output!r}", file=sys.stderr)
    print(target, end='')
    sys.exit(1)

if __name__ == "__main__":
    main()

