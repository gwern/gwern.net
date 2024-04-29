#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# latex2unicode.py: Convert a simple inline TeX/LaTeX (aimed at ArXiv abstracts) into Unicode+HTML+CSS, using the OA API.
# Author: Gwern Branwen
# Date: 2023-06-28
# When:  Time-stamp: "2024-04-28 20:25:46 gwern"
# License: CC-0
#
# Usage: $ OPENAI_API_KEY="sk-XXX" xclip -o | python latex2unicode.py
#
# Typesetting TeX/LaTeX for web browsers is typically a heavyweight operation; even if done server-side, display often requires a lot of CSS+fonts. And then the result looks highly unnatural and clearly 'alien', interrupting reading flow. This is worthwhile for complex equations, where browser typesetting is not up to snuff, but for many in-the-wild TeX uses, the use is often as simple as `$X$`, which would look better as `<em>X</em>` & take megabytes less to render. So it is desirable for simple TeX expressions to convert them to 'native' Unicode/HTML (augmented with a bit of custom CSS to handle things like superscripts-over-subscripts which pop up in integrals/summations/binomials/matrices etc).
# Unfortunately, TeX is an irregular macro language which is hard to parse and 'compile' to Unicode: it's easy to do many examples, but there's a long tail of weird variables, formatting commands etc, which means that I wind up defining lots of rewrites by hand, even though they are usually pretty 'obvious'. So, quite tedious and unrewarding.
# However, this is a perfect use-case for GPT models: it is hard to write comprehensive rules for, but is an extremely constrained problem in a domain it knows well which requires processing few tokens, where I can give it many few-shot examples, interrogate it for edge-cases to then write rules/examples for, and the harm of an error is relatively minimal (anyone seriously using an equation will need to read the original anyway, so won't be fooled by a wrong translation).
# So we write down a list of general rules, then a bunch of specific examples, then ask GPT-4 to translate from TeX to Unicode/HTML/CSS.
#
# eg.
# $ echo 'a + b = c^2' | python3 latex2unicode.py
# <em>a</em> + <em>b</em> = <em>c</em><sup>2</sup>

import sys
from openai import OpenAI
client = OpenAI()

if len(sys.argv) == 1:
    target = sys.stdin.read().strip()
else:
    target = sys.argv[1]

prompt = """
Task: Convert LaTeX inline expressions from ArXiv-style TeX math to inline Unicode+HTML+CSS, for easier reading in web browsers.

Details:

- Convert only if the result is unambiguous.
- Note that inputs may be very short, because each LaTeX fragment in an ArXiv abstract is processed individually. Many inputs will be a short as single letters (which are variables).
- Assume only default environment settings with no redefinitions or uses like `\newcommand` or `\begin`.
- Do not modify block-level equations, or complex structures such as diagrams or tables or arrays or matrices (eg `\begin{bmatrix}`), or illustrations such as drawn by TikZ or `\draw` , as those require special processing. Do not convert them & simply repeat it if the input is not an inline math expression.
- If a TeX command has no reasonable Unicode equivalent, such as the `\overrightarrow{AB}`/`\vec{AB}` or `\check{a}` or `\\underline`/`overline` commands in LaTeX, simply repeat it.
- If a TeX command merely adjusts positioning, size, or margin (such as `\big`/`\raisebox`/`\big`/`\Big`), always omit it from the conversion (as it is probably unnecessary & would need to be handled specially if it was).
- The TeX/LaTeX special glyphs (`\TeX` & `\LaTeX`) are handled elsewhere; do not convert them, but simply repeat it.
- Use Unicode entities, eg. MATHEMATICAL CAPITAL SCRIPT O 𝒪 in place of `\mathcal{O}`, and likewise for the Fraktur ones (`\mathfrak`) and bold ones (`\mathbb`). Convert to the closest Unicode entity that exists. Convert symbols, special symbols, mathematical operators, and Greek letters. Convert even if the Unicode is rare (such as  `𝒪`). If there is no Unicode equivalent (such as because there is not a matching letter in that font family, or no appropriate combining character), then do not convert it.
- If there are multiple reasonable choices, such as  `\approx` which could be represented as `≈` or `~`, choose the simpler-looking one. Do not choose the complex one unless there is some good specific reason for that.
- For superimposed subscript+superscript, use a predefined CSS <span> class `subsup`, eg. `(\Delta^0_n)` → `Δ<span class="subsup"><sup>0</sup><sub><em>n</em></sub></span>`; `\Xi_{cc}^{++} = ccu` → `Ξ<span class="subsup"><sub>cc</sub><sup>++</sup></span> = <em>ccu</em>`; `\,\Lambda_c \Lambda_c \to \Xi_{cc}^{++}\,n\,` → `<em>Λ<sub>c</sub></em> <em>Λ<sub>c</sub></em> → Ξ<span class="subsup"><sub>cc</sub><sup>++</sup></span>,<em>n</em>`. This is also useful for summations or integrals, such as `\int_a^b f(x) dx` → `∫<span class="subsup"><sub><em>a</em></sub><sup><em>b</em></sup></span> <em>f</em>(<em>x</em>) <em>dx</em>`.
- For small fractions, use FRACTION SLASH (⁄) to convert (eg. `1/2` or `\frac{1}{2}` → `1⁄2`). Do not use the Unicode fractions like VULGAR FRACTION ONE HALF `½`.
- For complex fractions which use superscripts or subscripts, multiple arguments etc, do not convert them & simply repeat them. eg. do not convert `\(\frac{a^{b}}{c^{d}}\)`, as it is too complex.
- Convert roots such as square or cube roots if that would be unambiguous. For example, `\sqrt[3]{8}` → `∛8` is good, but not `\sqrt[3]{ab}` because `∛<em>ab</em>` is ambiguous; do not convert complex roots like `\sqrt[3]{ab}`.
- Color & styling: if necessary, you may use very simple CSS inline with a `<span style="">` declaration, such as to color something blue using `<span style="color: blue">`.
- Be careful about dash use: correctly use MINUS SIGN (−) vs EM DASH (—) vs EN DASH (–) vs hyphen (-).
- More examples: ` O(1)` → ` 𝒪(1)`; `<span class="math inline">\(\mathsf{TC}^0\)</span>` → `<strong>TC</strong><sup>0</sup>`; `<span class="math inline">\(\approx\)</span>` → `~`; `<span class="math inline">\(1-\tilde \Omega(n^{-1/3})\)</span>` → `1 − Ω̃(<em>n</em><sup>−1⁄3</sup>)`; `<span class="math inline">\(\mathbf{R}^3\)</span>` → `𝐑<sup>3</sup>`; `<span class="math inline">\(\ell_p\)</span>` → `𝓁<sub>p</sub>`; `\textcircled{r}` → `ⓡ`; `(\nabla \log p_t\)` → `∇ log <em>p<sub>t</sub></em>`; `\(\partial_t u = \Delta u + \tilde B(u,u)\)` → `∂<sub><em>t</em></sub><em>u</em> = Δ<em>u</em> + <em>B̃</em>(<em>u</em>, <em>u</em>)`; `\(1 - \frac{1}{e}\)` → `"1 − 1⁄<em>e</em>`; `O(\sqrt{T}` → `𝒪(√<em>T</em>)`; `<span class="math inline">\(^\circ\)</span>` → `°`; `<span class="math inline">\(^\bullet\)</span>` → `•`; `6\times 10^{-6}\)` → `6×10<sup>−6</sup>`; `5\div10` → `5 ÷ 10`; `\Pr(\text{text} | \alpha)` → `Pr(text | α)`; `<span class="math inline">\(\hbar\)</span>` → `ℏ`; `\frac{1}{2}`→ `1⁄2`; `\nabla` → `∇`; `<span>\(r \to\infty\)</span>` → `<em>r</em> → ∞`; `\hat{a}` → `â`; \textit{zero-shot}` → `<em>zero-shot</em>`; `\(f(x) = x \cdot \text{sigmoid}(\beta x)\)` → `<em>f(x)</em> = <em>x</em> × sigmoid(β <em>x</em>)`; `\clubsuit` → `♣`; `\textcolor{red}{x}` → `<span style="color: red">x</span>`; `\textbf{bolding}` → `<strong>bolding</strong>`; `\textit{emphasis}` → `<em>emphasis</em>`; `B` → `<em>B</em>`; `u` → `<em>u</em>`; `X + Y` → `<em>X</em> + <em>Y</em>`; `\,\Lambda_b \Lambda_b \to \Xi_{bb}\,N\,` → `, <em>Λ<sub>b</sub></em> <em>Λ<sub>b</sub></em> → Ξ<sub><em>bb</em></sub> <em>N</em>,`, `x \in (-\infty, \infty)` → `x ∈ (-∞, ∞)`, `p\bar{p} \to \mu^+\mu^-` → `pp̅ → μ<sup>+</sup>μ<sup>−</sup>`, `\alpha\omega\epsilon\S\om\in` → `αωε§øm∈`, `^2H ^6Li ^{10}B ^{14}N` → `<sup>2</sup>H <sup>6</sup>Li <sup>10</sup>B <sup>14</sup>N`, `\mathcal{L} \mathcal{H} \mathbb{R} \mathbb{C}` → `ℒ ℋ ℝ ℂ`, `\textrm{M}_\odot` → `M<sub>☉</sub`, `10^{-16} - 10^{-10}  \sim \textrm{M}_\odot` → `10<sup>−16</sup>–10<sup>−10</sup>M<sub>☉</sub>`, `200+` → `200+`.

Task example:

Input to convert: <span class="math inline">\(H\gg1\)</span>
Converted output: <em>H</em> ≫ 1

Task:

Input to convert: """ + target + "Converted output: "

completion = client.chat.completions.create(
  model="gpt-4-1106-preview", # we use GPT-4 because the outputs are short, we want the highest accuracy possible, we provide a lot of examples & instructions which may overload dumber models, and reviewing for correctness can be difficult, so we are willing to spend a few pennies to avoid the risk of a lower model
  messages=[
    {"role": "system", "content": "You are a skilled mathematician & tasteful typographer, expert in LaTeX."},
    {"role": "user", "content": prompt }
  ]
)

print(completion.choices[0].message.content)
