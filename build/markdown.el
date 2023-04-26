;;; markdown.el --- Emacs support for editing Gwern.net
;;; Copyright (C) 2009 by Gwern Branwen
;;; License: CC-0
;;; When:  Time-stamp: "2023-04-25 10:52:32 gwern"
;;; Words: GNU Emacs, Markdown, HTML, YAML, Gwern.net, typography
;;;
;;; Commentary:
;;; Helper files for editing Markdown, HTML, and HTML-in-YAML, particularly reformatting & editing annotations in the Gwern.net house style.
;;; Additional functions include error-checking and prettifying confusable characters like dashes.

; since I hardly ever write elisp, and often start writing things in the *scratch* buffer, save time by defaulting to Markdown.
(setq initial-major-mode 'markdown-mode)
(setq initial-scratch-message "")

; I do much of my editing in gwern.net files, so save myself some tab-completion hassle:
(setq default-directory "~/wiki/")

;;we rely on the Github dev version because the 2017 v2.3 stable release packaged everywhere is missing a bugfix (stable breaks on any Markdown file with HTML comments in it); NOTE: still seems to be true on Ubuntu `elpa-markdown-mode` 2.3+210-1 as of 2023-02-11!
(add-to-list 'load-path "~/src/markdown-mode/")
(require 'markdown-mode)

; YAML-mode is most useful for editing my `/metadata/full.yaml` file
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

; (setq major-mode 'markdown-mode) ; needs to be done via 'Customize'?
(setq markdown-command
   "pandoc --mathjax --metadata title='Markdown preview' --to=html5 --standalone --number-sections --toc --reference-links --css=~/wiki/static/css/initial.css --css=~/wiki/static/css/links.css --css=~/wiki/static/css/default.css --css=~/wiki/static/css/dark-mode-adjustments.css --css=~/wiki/static/css/fonts.css --css=~/wiki/static/css/FontAwesome.css --css=~/wiki/static/css/dark-mode.css --css=~/wiki/static/css/colors.css --css=~/wiki/static/css/colors-dark.css -f markdown+smart --template=/home/gwern/bin/bin/pandoc-template-html5-articleedit.html5 -V lang=en-us")
(setq markdown-enable-math t)
(setq markdown-italic-underscore t)

;I like unusual semantic punctuation!
(defun interrobang () (interactive (insert-char ?‽ 1))) ;; interrobang: ‽ for replacing "?!"\"!?"
(defun irony       () (interactive (insert-char ?⸮ 1))) (defalias 'sarcasm 'irony) ;; sarcasm mark: ⸮ (better than '</sarcasm>' or '[!]', anyway)
(defun bitcoin     () (interactive (insert-char ?₿ 1)))
(defun en-dash     () (interactive (insert-char ?– 1)))
(defun em-dash     () (interactive (insert-char ?— 1)))
(defun arrow-right () (interactive (insert-char ?→ 1)))
(defun arrow-left  () (interactive (insert-char ?← 1)))
(defun arrow-both  () (interactive (insert-char ?↔ 1)))
(defun arrow-up    () (interactive (insert-char ?↑ 1)))
(defun arrow-down  () (interactive (insert-char ?↓ 1)))
(defun interpunct  () (interactive (insert-char ?· 1)))

(defun replace-all (original replacement)
  "Regexp search-and-replace all instances of ORIGINAL to REPLACEMENT: define a local equivalent of `replace-string' which won't throw annoying errors about being intended only for interactive use: exact string replacement (case-sensitive)."
  (save-excursion
    (let ((case-fold-search nil))
      (progn (goto-char (point-min))
             (while (re-search-forward original nil t)
               (replace-match replacement nil t))
             ))))

(defun de-unicode ()
  "Replace a subset of Unicode punctuation in the buffer with their ASCII equivalents. Most useful for Markdown mode."
  (interactive
  (save-excursion
    (goto-char (point-min))
    ; (replace-all "−" "\-") ; Pandoc Markdown→HTML does not support an escape for the 'minus sign'/−, so we write it literally & disable de-unicoding
    (replace-all "\\u2013" "--")
    (replace-all "â" "")
    (replace-all "\\u2022\n\n    " "- ")
    (replace-all "\\u2022\n" "- ")
    (replace-all "\\u2018" "‘")
    (replace-all "\\u2019" "’")
    (replace-all "–" "--")
    (replace-all "—" "---")
    (replace-all "-" "-")
    (replace-all "­" "-")
    (replace-all "­" "-")
    (replace-all "‐" "-")
    (replace-all "‘" "'")
    (replace-all "’" "'")
    (replace-all "’" "'")
    (replace-all "’" "'")
    (replace-all "‛" "'")
    (replace-all "" "'")
    (replace-all "“" "\"")
    (replace-all "”" "\"")
    (replace-all ",”" "”,")
    (replace-all "„" "\"")
    (replace-all "ﬂ" "fl")
    (replace-all "ﬁ" "fi")
    (replace-all "…" "...")
    (replace-all "‎" " ")
    (replace-all "​" " ")
    (replace-all "﻿" "")
    (replace-all " " " ")
    (replace-all "•" "-")
    (replace-all "" "-")
    (replace-all "" "-")
    (delete-trailing-whitespace)
    nil)))
(add-hook 'markdown-mode-hook
          (lambda ()
            (when buffer-file-name
              (add-hook 'before-save-hook
                        'de-unicode
                        nil t))))

; do *one* replacement and then quit. This is particularly useful in doing rewrites of hyperlinks: typically, we only want to hyperlink one instance (usually the first) of a word or phrase, and then skip the rest. The default `query-replace` requires us to either manually `n` them all, or `q` to quit. It can be toilsome to go through a lot of this. So we write our own to auto-exit on the first replacement.
; GPT-4-written. (Tried GPT-3.5 for most of it, but kept screwing up on parenthesis-matching. Neither version could remove the highlighting on substitutions.)
; currently primarily used by `getLinkSuggestions
(defun query-replace-once (from-string to-string &optional delimited start end)
  "Replace the first occurrence of FROM-STRING with TO-STRING.
If DELIMITED is non-nil, only match whole words.
START and END specify the region to search."
  (interactive
   (list (read-from-minibuffer "Query replace once (regexp): ")
         (read-from-minibuffer "Query replace once with: ")
         nil
        (when (use-region-p)
           (region-beginning))
         (when (use-region-p)
           (region-end))))
  (query-replace-regexp-once (regexp-quote from-string) to-string delimited start end))
(defun query-replace-regexp-once (regexp to-string &optional delimited start end)
  "Replace the first occurrence of REGEXP with TO-STRING.
If DELIMITED is non-nil, only match whole words.
START and END specify the region to search."
  (interactive
   (list (read-from-minibuffer "Query replace regexp once (regexp): ")
         (read-from-minibuffer "Query replace regexp once with: ")
         nil
         (when (use-region-p)
           (region-beginning))
         (when (use-region-p)
           (region-end))))
  (let ((inhibit-read-only t)
        (case-fold-search nil)
        (search-function (if delimited 're-search-forward-word 're-search-forward))
        (replace-done nil))
    (save-excursion
      (goto-char (or start (point-min)))
      (while (and (not replace-done) (funcall search-function regexp end t))
        (isearch-highlight (match-beginning 0) (match-end 0))
        (let ((response (read-char-choice
                         (concat "Replace this occurrence? (y/n/q): "
                                 (substring-no-properties (match-string 0)))
                         '(?y ?n ?q))))
          (cond ((eq response ?y)
                 (replace-match to-string t nil) ; NOTE: fixed-string replacement, not matched-case. We do not want to mangle URLs and create rewrites like 'Twitter' → '[Twitter](Https://En.Wikipedia.Org/Wiki/Twitter)'!
                 (setq replace-done t))
                ((eq response ?n)
                 (forward-char))
                ((eq response ?q)
                 (setq replace-done t) ; treat as successfully finished and exit politely
                 ))))
      (lazy-highlight-cleanup t))))
(defun re-search-forward-word (regexp &optional bound noerror count)
  "Search forward from point for a whole-word occurrence of REGEXP.
This is a wrapper around `re-search-forward' that ensures word boundaries.
BOUND, NOERROR, and COUNT have the same meaning as in `re-search-forward'."
  (let ((word-regexp (concat "\\b" regexp "\\b")))
    (re-search-forward word-regexp bound noerror count)))

;; TODO Abbreviation ideas:
;; script outputs: https://pastebin.com/rU0TyG5B
;; (!W → (!Wikipedia)
;; !Mar → ^[!Margin: ]
;; bc → because
;; iq → intelligence
;; moda → modafinil
;; s-s → statistically-significant
;; /r/g → https://www.reddit.com/r/gwern/

(defun fmt-range ()
  (interactive
   (let ((begin (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max)))
         )
     (save-excursion
       (goto-char (point-min))
       (query-replace-regexp "from \\([0-9\\.]+\\) to \\([0-9\\.]+\\)" "\\1–\\2" nil begin end)
       (query-replace-regexp "from \\([0-9\\.]+\\) to \\([0-9\\.]+\\)" "\\1 → \\2" nil begin end)
       )
     (save-buffer)
     (kill-buffer nil)
     )
   )
  )
; update gwern.net Markdown files with latest capabilities & conventions:
(defun fmt ()
  (interactive
   (let ((begin (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max)))
         )
     (save-excursion
       (goto-char (point-min))
       (de-unicode)
       (de-unicode)
       (flyspell-buffer)

       (replace-all "﻿" "") ; byte order mark?
       (replace-all "" "fi")
       (replace-all "" "fl")
    (replace-all "\\\u2013" "--")
    (replace-all "â" "")
    (replace-all "â" "“")
    (replace-all "â" "”")
    (replace-all "â" "–")
    (replace-all "â" "−")
    (replace-all "\n    \\\\u2022\n" "\n- ")
    (replace-all "\\\\u2022\n\n    " "- ")
    (replace-all "    \\\\u2022\n\n    " "- ")
    (replace-all "\n\\\\u2022 " "\n- ")
    (replace-all " Âµg" " µg")
    (replace-all "Î¼g" "µg")
    (replace-all "\nâ¢\n" "- ")
    (replace-all "â¢ " "- ")
    (replace-all "Kendall's Ï" "Kendall's <em>τ</em>")
    (replace-all "\\\\u03bc" "μ")
    (replace-all "\\\\u2018" "‘")
    (replace-all "\\\\u2019" "’")
    (replace-all "\u2009" " ")
    (replace-all "\\\\u2013" "–")
    (replace-all "â\\" "'")
    (replace-all "â" "'")
    (replace-all "â\\" "—")
    (replace-all "â" "−")
    (replace-all "\\\\u2014" "—")
    (replace-all "\\\\u201c" "“")
    (replace-all "\\\\u201d" "”")
    (replace-all "\\\\u2009" " ")
    (replace-all "\\\\u2212" "−")
    (replace-all "\\\\u2192" "→")
    (replace-all "\\\\u221e" "𝓁<sub>∞</sub>")
    (replace-all "\\\\u03b5" "𝜀")
    (replace-all "\\\\u223c" "~")
    (replace-all "\\\\u2217" "✱")
    (replace-all "\\\\u2020" "†")
    (replace-all "\\\\u2021" "‡")
    (replace-all "\\\\u2194" "↔")
    (replace-all "\\\\u2248 " "~")
    (replace-all "\\\\u03b1" "α")
    (replace-all "\\\\u03b8i" "θ<sub><em>i</em></sub>")
    (replace-all "\\\\u2265" "≥")
    (replace-all "\\\\u03b8" "θ")
    (replace-all " \\\\u2022 " ", ")
    (replace-all "\\\\u2022" "·")
    (replace-all "\\\\u2264" "≤")
    (replace-all "\\\\U0001d442" "𝒪")
    (replace-all "\\\\U0001d4412" "_N_^2^")
    (replace-all "\\\\u2208" "∈")
    (replace-all "\\\\U0001d45a" "𝑚")
    (replace-all "\\\\u2113" "𝓁")
    (replace-all "â¤" "≤")
    (replace-all "](wiki/" "](/")
    (replace-all "](//doc" "](/doc")
    (replace-all "]]http" "](https")
    (replace-all "]]/" "](/")
       (replace-all "" "=")
       (replace-all "  " ", ")
       (replace-all "T h i s" "This")
       (replace-all "T h e" "The")
       (replace-all "Author links open overlay panel" "")
       (replace-all "et al.," "et al")
       (replace-all "\n---\n" "\n<hr />\n")
       (replace-all "" " = ")
       (replace-all "" " < ")
       (replace-all "\n " "\n")
       (replace-all " = " " = ")
       (replace-all "‐" "-")
       (replace-all "­\n" "")
       (replace-all "­" "")
       (replace-all "–" "--")
       (replace-all "—" "---")
       (replace-all " ‑\n" "") (replace-all "‑\n" "") (replace-all "‑" "-") ; deal with NON-BREAKING HYPHEN which NEJM uses for both line-breaking and regular hyphens, /sigh
       (replace-all "<b>" "**")
       (replace-all "</b>" "**")
       (replace-all "<i>" "<em>")
       (replace-all "</i>" "</em>")
       (replace-all "=  " "= ")
       (replace-all "∼" "~")
       (replace-all "Previous article in issue\nNext article in issue\nKeywords\n" "[**Keywords**: ")
       (replace-all "Previous article in issue\nKeywords\n" "[**Keywords**: ")
       (replace-all "•\n\n    " "- ")
       (replace-all "eta≠analys" "eta-analys") ; odd typo in some PDFs: "meta≠analyses"
       (replace-all "\n•\n" "- ")
       (replace-all "    •\n    " "- ")
       (replace-all "<p> " "<p>")
       (replace-all " </p>" "</p>")
       (replace-all "View ORCID Profile" "")
       (replace-all " gf " " _g~f~_ ")
       (replace-all " gf." " _g~f~_.")
       (replace-all "(gf)" "(_g~f~_)")
       (replace-all "_gf_" "_g~f~_")
       (replace-all "gF" "_g~f~_")
       (replace-all "ﬁ" "fi")
       (replace-all "ﬀ" "ff")
       (replace-all "ﬃ" "ffi")
       (replace-all "ﬂ" "fl")
       (replace-all "￿" "fi")
       (replace-all "Æ" "fi")
       (replace-all "𝑥" "<em>x</em>")
       (replace-all "𝑦" "<em>y</em>")
       (replace-all " opens in new tab." "")
       (replace-all "Author links open overlay" "")
       (replace-all "Get rights and content" "")
       (replace-all "https://doi.org/" "DOI: ")
       (replace-all "e\\. g\\." "eg")
       (replace-all "₁" "<sub>1</sub>")
       (replace-all "₂" "<sub>2</sub>")
       (replace-all "₃" "<sub>3</sub>")
       (replace-all "₄" "<sub>4</sub>")
       (replace-all "¹" "<sup>1</sup>")
       (replace-all "²" "<sup>2</sup>")
       (replace-all "³" "<sup>3</sup>")
       (replace-all "⁴" "<sup>4</sup>")
       (replace-all "*†" "")
       (replace-all "†," ",")
       (replace-all "‡" "")
       (replace-all ",," ",")
       (replace-all ",”" "”,")
       (replace-all "‘‘" "\"")
       (replace-all "’’" "\"")
       (query-replace " ’" " ‘" nil begin end)
       (replace-all " " " ") ; replace THIN SPACE
       (replace-all "](!)" "](!W)")
       (replace-all "˜" "~")
       (replace-all "randomis" "randomiz")
       (replace-all "mendelian random" "Mendelian random")
       (replace-all "behaviour" "behavior")
       (replace-all " utilise" "use")
       (replace-all " utilize" "use")
       (replace-all " utilising" " using")
       (replace-all " utilizing" " using")
       (replace-all " utilisation" " usage")
       (replace-all " utilization" " usage")
       (replace-all "\n• " "\n- ")
       (replace-all " colour" " color")
       (replace-all "](/docs/" "](/doc/")
       (replace-all "href=\"/docs/" "href=\"/doc/")
       (replace-all "href='/docs/" "href='/doc/")
       (replace-all "]/doc" "](/doc")
       (query-replace "]/" "](/" nil begin end)

       (query-replace " · " ", " nil begin end)
       ; remove subtle whitespace problems: double space
       (query-replace-regexp "\\([[:alnum:]]\\)  \\([[:graph:]]\\)" "\\1 \\2" nil begin end)
       (query-replace-regexp "\\([a-z]+\\),\\([a-z]+\\)" "\\1, \\2" nil begin end) ; run-together comma phrases often appear in PDF OCR like 'foo,bar'; outside chemical names, this is highly unusual

       (query-replace ",”" "”," nil begin end)
       (query-replace "'''''" "**_" nil begin end)
       (query-replace "'''" "**" nil begin end)
       (query-replace " " " " nil begin end)
       (query-replace "''" "\"" nil begin end)
       (query-replace "( " "(" nil begin end)
       (query-replace "| " "|" nil begin end)
       (query-replace "|-|" "|–|" nil begin end) ; EN DASH: absolute value ranges
       (query-replace " )" ")" nil begin end)
       (query-replace "?!" "‽" nil begin end)
       (query-replace "!?" "‽" nil begin end)
       (query-replace-regexp "!!+" "!" nil begin end)
       (query-replace " — " "—" nil begin end)
       (query-replace " – " "—" nil begin end)
       (query-replace " − " "—" nil begin end)
       (query-replace " -> " "→" nil begin end)
       (query-replace "->"   "→" nil begin end)
       (query-replace "~>"   "→" nil begin end)
       (query-replace " percent " "% " nil begin end)
       (query-replace " per cent " "% " nil begin end)
       (query-replace "95 %CI" "95% CI" nil begin end)
       (query-replace "95 % confidence interval" "95% confidence interval" nil begin end)
       (query-replace " , " ", " nil begin end)
       (query-replace "http://reddit.com" "https://old.reddit.com" nil begin end)
       (query-replace "https://reddit.com" "https://old.reddit.com" nil begin end)
       (query-replace "http://www.reddit.com" "https://old.reddit.com" nil begin end)
       (query-replace "https://www.reddit.com" "https://old.reddit.com" nil begin end)
       (query-replace "https://mobile.twitter.com" "https://twitter.com" nil begin end)
       ; italicize p-values & sample sizes:
       (query-replace " d=" " _d_ = " nil begin end)
       (query-replace " d = " " _d_ = " nil begin end)
       (query-replace "(d = " "(_d_ = " nil begin end)
       (query-replace "Cohen’s d" "Cohen’s _d_" nil begin end)
       (query-replace "<em>P</em>=" "_p_ = " nil begin end)
       (query-replace "pone-tailed" "<em>p</em><sub><em>one-tailed</em></sub>" nil begin end)
       (query-replace "ptrend" "<em>p</em><sub>trend</sub>" nil begin end)
       (query-replace "pmeta" "<em>p</em><sub>meta</sub>" nil begin end)
       (query-replace "pinteraction" "<em>p</em><sub>interaction</sub>" nil begin end)
       (query-replace "p0" "<em>p</em><sub>0</sub>" nil begin end)
       (query-replace "pgc" "<em>p<sub>gc</sub></em>" nil begin end)
       (query-replace-regexp "\\([[:punct:]]\\)p<" "\\1_p_ <" nil begin end)
       (replace-all " n = " " _n_ = ")
       (replace-all "(n = " "(_n_ = ")
       (replace-all "(N=" "(_n_ = ")
       (replace-all "(n=" "(_n_ = ")
       (query-replace "(N" "(_n_ " nil begin end)
       (query-replace " N)" " _n_)" nil begin end)
       (query-replace "[n = " "[_n_ = " nil begin end)
       (query-replace "[N=" "[_n_ = " nil begin end)
       (query-replace "[n=" "[_n_ = " nil begin end)
       (query-replace "[N" "[_n_ " nil begin end)
       (query-replace " n=" " _n_ = " nil begin end)
       (query-replace " N = " " _n_ = " nil begin end)
       (query-replace " N=" " _n_ = " nil begin end)
       (query-replace " n =" " _n_ = " nil begin end)
       (query-replace " n " " _n_ " nil begin end)
       (query-replace " _n_=" " _n_ = " nil begin end)
       (replace-all "(_n_=" "(_n_ = ")
       (query-replace "n = " "_n_ = " nil begin end)
       (query-replace "N = " "_n_ = " nil begin end)
       (query-replace " n≤" " _n_ ≤ " nil begin end)
       (query-replace " n≥" " _n_ ≥ " nil begin end)
       (query-replace " O(" " 𝒪(" nil begin end)
       (query-replace "( r =" "(_r_ =" nil begin end)
       (query-replace "(r)" "(_r_)" nil begin end)
       (query-replace "(r ≥" "(_r_ ≥" nil begin end)
       (query-replace " r values" " _r_ values" nil begin end)
       (query-replace "(rs =" "(<em>r</em>s =" nil begin end)
       (query-replace " rs =" " <em>r</em>s =" nil begin end)
       (query-replace "Mr = " "_M_~r~ = " nil begin end)
       (query-replace "zMR " "_z_~MR~" nil begin end)
       (query-replace "z score" "_z_ score" nil begin end)
       (query-replace "z-score" "_z_-score" nil begin end)
       (query-replace " z-" " _z_-" nil begin end)
       (query-replace "Mage " "M~age~" nil begin end)
       (query-replace "Mage " "M~age~" nil begin end)
       (query-replace "tmax " "t~max~" nil begin end)
       (query-replace "Cmax" "C~max~" nil begin end)
       (query-replace "AUClast" "AUC~last~" nil begin end)
       (query-replace "AUC∞" "AUC~∞~" nil begin end)
       (query-replace "SDage " "SD~age~" nil begin end)
       (query-replace "RR=" "RR = " nil begin end)
       (query-replace "r =" "_r_ = " nil begin end)
       (query-replace "r=" "_r_ = " nil begin end)
       (query-replace "r = " "_r_ = " nil begin end)
       (query-replace "(r range = " "(_r_ range = " nil begin end)
       (query-replace " r’s " " _r_’s " nil begin end)
       (query-replace "r ≈" "_r_ ≈ " nil begin end)
       (query-replace " rs " " <em>r</em>s " nil begin end)
       (query-replace " r-value" " <em>r</em>-value" nil begin end)
       (query-replace " p = " " _p_ = " nil begin end)
       (query-replace " p=" " _p_ = " nil begin end)
       (query-replace " p =" " _p_ = " nil begin end)
       (query-replace "(p = " "(_p_ = " nil begin end)
       (query-replace "[p = " "[_p_ = " nil begin end)
       (query-replace "(P=" "(_p_ = " nil begin end)
       (query-replace "p-value" "_p_-value" nil begin end)
       (query-replace "p value" "_p_-value" nil begin end)
       (query-replace "p-curve" "_p_-curve" nil begin end)
       (query-replace "(t<" "(_t_ < " nil begin end)
       (query-replace "(t>" "(_t_ > "  nil begin end)
       (query-replace " t-statistic" " _t_-statistic"  nil begin end)
       (query-replace "OR=" "OR = " nil begin end)
       (query-replace "AOR=" "AOR = " nil begin end)
       (query-replace "HR=" "HR = " nil begin end)
       (query-replace "rP=" "_r~p~_ = " nil begin end)
       (query-replace "rP = " "_r~p~_ = " nil begin end)
       (query-replace "rg=" "_r~g~_ = " nil begin end)
       (query-replace "rg = " "_r~g~_ = " nil begin end)
       (query-replace "(rg)" "(_r~g~_)" nil begin end)
       (query-replace "(rg " "(_r~g~_ " nil begin end)
       (query-replace " rg " " _r~g~_ " nil begin end)
       (replace-all "|rg|" "|<em>r<sub>g</sub></em>|")
       (replace-all "| rg |" "|<em>r<sub>g</sub></em>|")
       (replace-all "| _r~g~_ |" "|<em>r<sub>g</sub></em>|")
       (query-replace "ra = " "_r~a~_ = " nil begin end)
       (query-replace "(ra)" "(_r~a~_)" nil begin end)
       (query-replace "(ra " "(_r~a~_ " nil begin end)
       (query-replace " ra " " _r~a~_ " nil begin end)
       (replace-all "|ra|" "|<em>r<sub>a</sub></em>|")
       (replace-all "| ra |" "|<em>r<sub>a</sub></em>|")
       (replace-all "| _r~a~_ |" "|<em>r<sub>a</sub></em>|")
       (query-replace "QM[" "_Q_~M~[" nil begin end)
       (query-replace "QM(" "_Q_~M~(" nil begin end)
       (query-replace "= ." "= 0." nil begin end)
       (query-replace "< ." "< 0." nil begin end)
       (query-replace "> ." "> 0." nil begin end)
       (query-replace "ρ =" "_ρ_ =" nil begin end)
       (query-replace "SE=" "SE = " nil begin end)
       (query-replace "</em>=" "</em> = " nil begin end)
       (query-replace "</em>&gt;" "</em> &gt; " nil begin end)
       (query-replace "</em>&lt;" "</em> &lt; " nil begin end)
       (query-replace "</em><" "</em> < " nil begin end)
       (query-replace "</em>>" "</em> > " nil begin end)
       (query-replace "_<" "_ < " nil begin end)
       (query-replace "_>" "_ > " nil begin end)
       (query-replace "=." " = 0." nil begin end)
       (query-replace "(.0" "(0.0" nil begin end)
       (query-replace " < .0" " < 0.0" nil begin end)
       (query-replace "=1 " " = 1 " nil begin end)
       (query-replace "=2 " " = 2 "  nil begin end)
       (query-replace "(p < 0." "(_p_ < 0." nil begin end)
       (query-replace " p value" " _p_-value" nil begin end)
       (query-replace " p-hack" " _p_-hack" nil begin end)
       (query-replace " p " " _p_ " nil begin end)
       (query-replace " <0." " < 0." nil begin end)
       (query-replace "<=" "≤" nil begin end)
       (query-replace ">=" "≥" nil begin end)
       (query-replace "( N " "(_n_ " nil begin end)
       (query-replace "( _n_" "(_n_ " nil begin end)
       (replace-all " n = " " _n_ = ")
       (replace-all "(n ≈" "(_n_ ≈")
       (replace-all " n ≈" " _n_ ≈")
       (replace-all "(n " "(_n_ ")
       (replace-all "(ie, " "(ie. ")
       (replace-all "(ie " "(ie. ")
       (replace-all "(i\\.e\\.," "(ie.")
       (replace-all "(i\\.e\\." "(ie.")
       (replace-all " e\\.g\\." " eg.")
       (replace-all "(e\\.g\\." "(eg.")
       (replace-all "(eg " "(eg.")
       (replace-all " eg " " eg. ")
       (replace-all "eg\\., " "eg. ")
       (replace-all "e\\.g\\., " "eg. ")
       (replace-all "<sup>+</sup>" "⁺")
       (replace-all "et al.\n" "et al")
       (replace-all " ω 2" " ω<sup>2</sup>")
       (replace-all "τ2" "τ<sup>2</sup>")
       (replace-all "ω2" "ω<sup>2</sup>")
       (replace-all "chi-squared" "<em>χ</em><sup>2</sup>")
       (replace-all " Escherichia coli" " <em>Escherichia coli</em>")
       (replace-all "two-by-two" "2×2")
       (replace-all " B.M.I" " BMI")
       (query-replace "n-gram" "<em>n</em>-gram" nil begin end)
       (query-replace "k-mean" "<em>k</em>-mean" nil begin end)
       (query-replace "partial Î·2" "partial η<sup>2</sup>" nil begin end)
       (query-replace "s9 " "s’ " nil begin end) ; workaround Medrxiv/Biorxiv consistent malformatting of "s'" as "s9" (somehow); can't auto-replace due to 'CRISPR-Cas9' etc
       (query-replace " −." " −0." nil begin end)
       (query-replace "50/50" "50:50" nil begin end)
       (query-replace "C3O2" "C~3~O~2~" nil begin end)
       (query-replace "CO2" "CO~2~" nil begin end)
       (query-replace " CO2" " CO~2~" nil begin end)
       (query-replace "CO2." "CO~2~" nil begin end)
       (query-replace "O2" "O~2~" nil begin end)
       (query-replace "β42" "β~42~" nil begin end)
       (query-replace "βA" "β~A~" nil begin end)
       (query-replace "βC" "β~C~" nil begin end)
       (query-replace "βE" "β~E~" nil begin end)
       (query-replace "CH4" "CH~4~" nil begin end)
       (query-replace "PH3" "PH~3~" nil begin end)
       (query-replace "PM2.5" "PM<sub>2.5</sub>" nil begin end)
       (query-replace "μg/m3" "μg⁄m<sup>3</sup>" nil begin end)
       (query-replace "H2" "H~2~" nil begin end)
       (query-replace "H2O" "H~2~O" nil begin end)
       (query-replace "cm-2" "cm^−2^" nil begin end)
       (query-replace "cm−2" "cm^−2^" nil begin end)
       (query-replace "cm3" "cm^3^" nil begin end)
       (query-replace " m3" " m^3^" nil begin end)
       (query-replace "kg/m2" "kg⁄m^2^" nil begin end)
       (query-replace " m/s" " m⁄s" nil begin end)
       (query-replace " km/s" " km⁄s" nil begin end)
       (query-replace "µm3" "µm^3^" nil begin end)
       (query-replace " ug" " μg" nil begin end)
       (query-replace "ϰ2" "ϰ^2^" nil begin end)
       (query-replace " s-1" " s^−1^" nil begin end)
       (query-replace " s−1" " s^−1^" nil begin end)
       (query-replace "ml−1" "ml<sup>−1</sup>" nil begin end)
       (query-replace "^-1 " "^−1^ " nil begin end)
       (query-replace "dl−1" "dl<sup>−1</sup>" nil begin end)
       (query-replace "kb−1" "kb<sup>−1</sup>" nil begin end)
       (query-replace " g-related" " _g_-related" nil begin end)
       (query-replace "g+ " "_g_^+^ " nil begin end)
       (query-replace "g-factor" "_g_-factor" nil begin end)
       (query-replace "g-load" "_g_-load" nil begin end)
       (query-replace "(g)" "(_g_)" nil begin end)
       (query-replace " g." " _g_." nil begin end)
       (query-replace " g," " _g_," nil begin end)
       (query-replace " g:" " _g_:" nil begin end)
       (query-replace " g " " _g_ " nil begin end)
       (query-replace "(g=" "(_g_ = " nil begin end)
       (query-replace "mg L−1" "mg _L_^−1^" nil begin end)
       (query-replace "tau181" "tau~181~" nil begin end)
       (query-replace "vitamin D3" "vitamin D~3~" nil begin end)
       (query-replace "vitamin D4" "vitamin D~4~" nil begin end)
       (query-replace " \" " " \"" nil begin end)
       (query-replace " \\times " " \cdot " nil begin end)
       (query-replace "''" "\"" nil begin end)
       (query-replace "``" "\"" nil begin end)
       (query-replace "$\alpha$" "α" nil begin end)
       (query-replace "$\beta$" "β" nil begin end)
       (query-replace "$\sigma$" "σ" nil begin end)
       (query-replace "Neff=" "_N_~eff~ = " nil begin end)
       (query-replace "Neff" "_N_~eff~" nil begin end)
       (query-replace " Ne " " <em>N<sub>e</sub></em> " nil begin end)
       (query-replace-regexp " Ne\\\([[:punct:]]\\)" " <em>N<sub>e</sub></em>\\1" nil begin end)
       (query-replace-regexp "\\\([[:punct:]]\\)Ne " "\\1<em>N<sub>e</sub></em> " nil begin end)
       (query-replace "$N_e$" "_N_~_e_~" nil begin end)
       (query-replace "$\frac{n}{2}$" "_n_⁄2" nil begin end)
       (query-replace "$\frac{N}{2}$" "_n_⁄2" nil begin end)
       (query-replace " a2" " _a_^2^" nil begin end)
       (query-replace " c2" " _c_^2^" nil begin end)
       (query-replace " e2" " _e_^2^" nil begin end)
       (query-replace "(a2" "(_a_^2^" nil begin end)
       (query-replace "(c2" "(_c_^2^" nil begin end)
       (query-replace "(e2" "(_e_^2^" nil begin end)
       (query-replace "h2SNP" "<em>h</em><span class=\"subsup\"><sup>2</sup><sub>SNP</sub></span>" nil begin end)
       (query-replace "h2snp" "<em>h</em><span class=\"subsup\"><sup>2</sup><sub>SNP</sub></span>" nil begin end)
       (query-replace-regexp "<em>\\([a-zA-Z]+\\)</em><sup>2</sup><sub>\\([a-zA-Z]+\\)</sub>" "<em>\\1</em><sup>2</sup><sub>\\2</sub>" nil begin end)
       (query-replace "h2 " "_h_^2^ " nil begin end)
       (query-replace " h2" " _h_^2^" nil begin end)
       (query-replace "(h2=" "(_h_^2^ = " nil begin end)
       (query-replace "(h2 = " "(_h_^2^ = " nil begin end)
       (query-replace "h2 = " "_h_^2^ = " nil begin end)
       (query-replace "c 2" "_c_^2^" nil begin end)
       (query-replace "h^2SNP" "<em>h</em><span class=\"subsup\"><sup>2</sup><sub><em>SNP</em></sub></span>" nil begin end)
       (query-replace "_h_^2^ = " "_h_^2^ = " nil begin end)
       (query-replace "Fst" "F~st~" nil begin end)
       (query-replace "rMZ" "_r_~MZ~" nil begin end)
       (query-replace "rDZ" "_r_~DZ~" nil begin end)
       (query-replace "R22" "_R_<span class=\"subsup\"><sup>2</sup><sub>2</sub></span>" nil begin end)
       (query-replace " Wm−1" " Wm<sup>−1</sup>" nil begin end)
       (query-replace " K−1" " K<sup>−1</sup>" nil begin end)
       (query-replace " kg−1" " kg<sup>−1</sup>" nil begin end)
       (query-replace "60Co" "^60^Co" nil begin end)
       (query-replace " I2" " _I_^2^" nil begin end)
       (query-replace "≤p≤" " ≤ _p_ ≤ " nil begin end)
       (query-replace "BF10" "<a href=\"https://en.wikipedia.org/wiki/Bayes_factor\">BF</a><sub>10</sub>" nil begin end)
       (query-replace "BF10" "BF<sub>10</sub>" nil begin end)
       (query-replace ":   " ": " nil begin end)
       (query-replace "(i)" "(1)" nil begin end)
       (query-replace "(ii)" "(2)" nil begin end)
       (query-replace "(iii)" "(3)" nil begin end)
       (query-replace "(iv)" "(4)" nil begin end)
       (query-replace "(v)" "(5)" nil begin end)
       (query-replace "(vi)" "(6)" nil begin end)
       (query-replace "(vii)" "(7)" nil begin end)
       (query-replace "(viii)" "(8)" nil begin end)
       (query-replace "(ix)" "(9)" nil begin end)
       (query-replace "(x)" "(10)" nil begin end)
       (query-replace "(a)" "(1)" nil begin end)
       (query-replace "(b)" "(2)" nil begin end)
       (query-replace "(c)" "(3)" nil begin end)
       (query-replace "(d)" "(4)" nil begin end)
       (query-replace "(e)" "(5)" nil begin end)
       (query-replace "(f)" "(6)" nil begin end)
       (query-replace "(g)" "(7)" nil begin end)
       (query-replace "(h)" "(8)" nil begin end)
       (query-replace "(i)" "(9)" nil begin end)
       (query-replace "(j)" "(10)" nil begin end)
       (query-replace " i)" " (1)" nil begin end)
       (query-replace " ii)" " (2)" nil begin end)
       (query-replace " iii)" " (3)" nil begin end)
       (query-replace " iv)" " (4)" nil begin end)
       (query-replace " v)" " (5)" nil begin end)
       (query-replace " vi)" " (6)" nil begin end)
       (query-replace " vii)" " (7)" nil begin end)
       (query-replace " viii)" " (8)" nil begin end)
       (query-replace " ix)" " (9)" nil begin end)
       (query-replace " x)" " (10)" nil begin end)

       (query-replace "DALL-E" "DALL·E" nil begin end)
       (query-replace "DALL-e" "DALL·E" nil begin end)
       (query-replace "NOVA1" "_NOVA1_" nil begin end)
       (query-replace "x-axis" "_x_-axis" nil begin end)
       (query-replace "x axis" "_x_-axis" nil begin end)
       (query-replace "z axis" "_x_-axis" nil begin end)
       (query-replace "y-axis" "_y_-axis" nil begin end)
       (query-replace "z-axis" "_z_-axis" nil begin end)
       (query-replace "y axis" "_y_-axis" nil begin end)
       (query-replace "x-axes" "_x_-axes" nil begin end)
       (query-replace "x axes" "_x_-axes" nil begin end)
       (query-replace "z-axes" "_z_-axes" nil begin end)
       (query-replace "z axes" "_z_-axes" nil begin end)
       (query-replace "y-axes" "_y_-axes" nil begin end)
       (query-replace "y axes" "_y_-axes" nil begin end)
       (query-replace " x " " _x_ " nil begin end)
       (query-replace " x:" " _x_:" nil begin end)
       (query-replace " y " " _y_ " nil begin end)
       (query-replace " y:" " _y_:" nil begin end)
       (query-replace " z " " _z_ " nil begin end)
       (query-replace " k " " _k_ " nil begin end)
       (query-replace " k=" " _k_ =" nil begin end)
       (query-replace " n " " _n_ " nil begin end)
       (query-replace "(n2)" "(_n_^2^)" nil begin end)
       (query-replace "(n3)" "(_n_^3^)" nil begin end)
       (query-replace "n→" "_n_ → " nil begin end)
       (query-replace "n-back" "_n_-back" nil begin end)
       (query-replace "log 2" "log<sub>2</sub>" nil begin end)
       (query-replace "fuck" "f—k" nil begin end)
       (query-replace "damn" "d—n" nil begin end)
       (query-replace "shit" "s—t" nil begin end)
       (query-replace "95%CI" "95% CI" nil begin end)
       (query-replace "allowlist" "whitelist" nil begin end)
       (query-replace "denylist" "blacklist" nil begin end)
       (query-replace " <sup>" "<sup>" nil begin end) ; can't auto-replace because of instances like isotopic elements with *prefixed* superscripts, eg ' <sup>60</sup>Co'

       ; replace all word-numbers with actual numbers:
       (query-replace " one-hundred" " 100" nil begin end) (query-replace " one hundred" " 100" nil begin end) (query-replace " ninety nine" " 99" nil begin end) (query-replace " ninety-nine" " 99" nil begin end) (query-replace " ninety eight" " 98" nil begin end) (query-replace " ninety-eight" " 98" nil begin end) (query-replace " ninety seven" " 97" nil begin end) (query-replace " ninety-seven" " 97" nil begin end) (query-replace " ninety six" " 96" nil begin end) (query-replace " ninety-six" " 96" nil begin end) (query-replace " ninety five" " 95" nil begin end) (query-replace " ninety-five" " 95" nil begin end) (query-replace " ninety four" " 94" nil begin end) (query-replace " ninety-four" " 94" nil begin end) (query-replace " ninety three" " 93" nil begin end) (query-replace " ninety-three" " 93" nil begin end) (query-replace " ninety two" " 92" nil begin end) (query-replace " ninety-two" " 92" nil begin end) (query-replace " ninety one" " 91" nil begin end) (query-replace " ninety-one" " 91" nil begin end) (query-replace " ninety" " 90" nil begin end) (query-replace " eighty nine" " 89" nil begin end) (query-replace " eighty-nine" " 89" nil begin end) (query-replace " eighty eight" " 88" nil begin end) (query-replace " eighty-eight" " 88" nil begin end) (query-replace " eighty seven" " 87" nil begin end) (query-replace " eighty-seven" " 87" nil begin end) (query-replace " eighty six" " 86" nil begin end) (query-replace " eighty-six" " 86" nil begin end) (query-replace " eighty five" " 85" nil begin end) (query-replace " eighty-five" " 85" nil begin end) (query-replace " eighty four" " 84" nil begin end) (query-replace " eighty-four" " 84" nil begin end) (query-replace " eighty three" " 83" nil begin end) (query-replace " eighty-three" " 83" nil begin end) (query-replace " eighty two" " 82" nil begin end) (query-replace " eighty-two" " 82" nil begin end) (query-replace " eighty one" " 81" nil begin end) (query-replace " eighty-one" " 81" nil begin end) (query-replace " eighty" " 80" nil begin end) (query-replace " seventy nine" " 79" nil begin end) (query-replace " seventy-nine" " 79" nil begin end) (query-replace " seventy eight" " 78" nil begin end) (query-replace " seventy-eight" " 78" nil begin end) (query-replace " seventy seven" " 77" nil begin end) (query-replace " seventy-seven" " 77" nil begin end) (query-replace " seventy six" " 76" nil begin end) (query-replace " seventy-six" " 76" nil begin end) (query-replace " seventy five" " 75" nil begin end) (query-replace " seventy-five" " 75" nil begin end) (query-replace " seventy four" " 74" nil begin end) (query-replace " seventy-four" " 74" nil begin end) (query-replace " seventy three" " 73" nil begin end) (query-replace " seventy-three" " 73" nil begin end) (query-replace " seventy two" " 72" nil begin end) (query-replace " seventy-two" " 72" nil begin end) (query-replace " seventy one" " 71" nil begin end) (query-replace " seventy-one" " 71" nil begin end) (query-replace " seventy" " 70" nil begin end) (query-replace "sixties" "60s" nil begin end) (query-replace " sixty nine" " 69" nil begin end) (query-replace " sixty-nine" " 69" nil begin end) (query-replace " sixty eight" " 68" nil begin end) (query-replace " sixty-eight" " 68" nil begin end) (query-replace " sixty seven" " 67" nil begin end) (query-replace " sixty-seven" " 67" nil begin end) (query-replace " sixty six" " 66" nil begin end) (query-replace " sixty-six" " 66" nil begin end) (query-replace " sixty five" " 65" nil begin end) (query-replace " sixty-five" " 65" nil begin end) (query-replace " sixty four" " 64" nil begin end) (query-replace " sixty-four" " 64" nil begin end) (query-replace " sixty three" " 63" nil begin end) (query-replace " sixty-three" " 63" nil begin end) (query-replace " sixty two" " 62" nil begin end) (query-replace " sixty-two" " 62" nil begin end) (query-replace " sixty one" " 61" nil begin end) (query-replace " sixty-one" " 61" nil begin end) (query-replace " sixty" " 60" nil begin end) (query-replace " fifty nine" " 59" nil begin end) (query-replace " fifty-nine" " 59" nil begin end) (query-replace " fifty eight" " 58" nil begin end) (query-replace " fifty-eight" " 58" nil begin end) (query-replace " fifty seven" " 57" nil begin end) (query-replace " fifty-seven" " 57" nil begin end) (query-replace " fifty six" " 56" nil begin end) (query-replace " fifty-six" " 56" nil begin end) (query-replace " fifty five" " 55" nil begin end) (query-replace " fifty-five" " 55" nil begin end) (query-replace " fifty four" " 54" nil begin end) (query-replace " fifty-four" " 54" nil begin end) (query-replace " fifty three" " 53" nil begin end) (query-replace " fifty-three" " 53" nil begin end) (query-replace " fifty two" " 52" nil begin end) (query-replace " fifty-two" " 52" nil begin end) (query-replace " fifty one" " 51" nil begin end) (query-replace " fifty-one" " 51" nil begin end) (query-replace " fifty" " 50" nil begin end) (query-replace " forty nine" " 49" nil begin end) (query-replace " forty-nine" " 49" nil begin end) (query-replace " forty eight" " 48" nil begin end) (query-replace " forty-eight" " 48" nil begin end) (query-replace " forty seven" " 47" nil begin end) (query-replace " forty-seven" " 47" nil begin end) (query-replace " forty six" " 46" nil begin end) (query-replace " forty-six" " 46" nil begin end) (query-replace " forty five" " 45" nil begin end) (query-replace " forty-five" " 45" nil begin end) (query-replace " forty four" " 44" nil begin end) (query-replace " forty-four" " 44" nil begin end) (query-replace " forty three" " 43" nil begin end) (query-replace " forty-three" " 43" nil begin end) (query-replace " forty two" " 42" nil begin end) (query-replace " forty-two" " 42" nil begin end) (query-replace " forty one" " 41" nil begin end) (query-replace " forty-one" " 41" nil begin end) (query-replace " forty" " 40" nil begin end) (query-replace " thirty nine" " 39" nil begin end) (query-replace " thirty-nine" " 39" nil begin end) (query-replace " thirty eight" " 38" nil begin end) (query-replace " thirty-eight" " 38" nil begin end) (query-replace " thirty seven" " 37" nil begin end) (query-replace " thirty-seven" " 37" nil begin end) (query-replace " thirty six" " 36" nil begin end) (query-replace " thirty-six" " 36" nil begin end) (query-replace " thirty five" " 35" nil begin end) (query-replace " thirty-five" " 35" nil begin end) (query-replace " thirty four" " 34" nil begin end) (query-replace " thirty-four" " 34" nil begin end) (query-replace " thirty three" " 33" nil begin end) (query-replace " thirty-three" " 33" nil begin end) (query-replace " thirty two" " 32" nil begin end) (query-replace " thirty-two" " 32" nil begin end) (query-replace " thirty one" " 31" nil begin end) (query-replace " thirty-one" " 31" nil begin end) (query-replace " thirty" " 30" nil begin end) (query-replace " twenty nine" " 29" nil begin end) (query-replace " twenty-nine" " 29" nil begin end) (query-replace " twenty eight" " 28" nil begin end) (query-replace " twenty-eight" " 28" nil begin end) (query-replace " twenty seven" " 27" nil begin end) (query-replace " twenty-seven" " 27" nil begin end) (query-replace " twenty six" " 26" nil begin end) (query-replace " twenty-six" " 26" nil begin end) (query-replace " twenty five" " 25" nil begin end) (query-replace " twenty-five" " 25" nil begin end) (query-replace " twenty four" " 24" nil begin end) (query-replace " twenty-four" " 24" nil begin end) (query-replace " twenty three" " 23" nil begin end) (query-replace " twenty-three" " 23" nil begin end) (query-replace " twenty two" " 22" nil begin end) (query-replace " twenty-two" " 22" nil begin end) (query-replace " twenty one" " 21" nil begin end) (query-replace " twenty-one" " 21" nil begin end) (query-replace " twenty" " 20" nil begin end) (query-replace " nineteen" " 19" nil begin end) (query-replace " eighteen" " 18" nil begin end) (query-replace " seventeen" " 17" nil begin end) (query-replace " sixteen" " 16" nil begin end) (query-replace " fifteen" " 15" nil begin end) (query-replace " fourteen" " 14" nil begin end) (query-replace " thirteen" " 13" nil begin end) (query-replace " twelve" " 12" nil begin end) (query-replace " eleven" " 11" nil begin end) (query-replace " ten " " 10 " nil begin end) (query-replace " nine" " 9" nil begin end) (query-replace " eight" " 8" nil begin end) (query-replace " seven" " 7" nil begin end) (query-replace " six" " 6" nil begin end) (query-replace " five" " 5" nil begin end) (query-replace " four" " 4" nil begin end) (query-replace " three" " 3" nil begin end) (query-replace "Ten " "10 " nil begin end) (query-replace "Nine " " 9" nil begin end) (query-replace "Eight " "8 " nil begin end) (query-replace "Seven " "7 " nil begin end) (query-replace "Six " "6 " nil begin end) (query-replace "Five " "5 " nil begin end) (query-replace "Four " "4 " nil begin end) (query-replace "Three " "3 " nil begin end)
       ; look for possible screwed-up exponentials
       (query-replace " 1015" " 10^15^" nil begin end) (query-replace " 1014" " 10^14^" nil begin end) (query-replace " 1013" " 10^13^" nil begin end) (query-replace " 1012" " 10^12^" nil begin end) (query-replace " 1011" " 10^11^" nil begin end) (query-replace " 1010" " 10^10^" nil begin end) (query-replace " 109" " 10^9^" nil begin end) (query-replace " 108" " 10^8^" nil begin end) (query-replace " 107" " 10^7^" nil begin end) (query-replace " 106" " 10^6^" nil begin end) (query-replace " 105" " 10^5^" nil begin end) (query-replace " 104" " 10^4^" nil begin end) (query-replace " 103" " 10^3^" nil begin end) (query-replace " 102" " 10^2^" nil begin end)

       ; numbers:
       (query-replace-regexp "\\([023456789]\\)th" "\\1^th^" nil begin end)
       (query-replace-regexp "\\([1]\\)st"        "\\1^st^" nil begin end)
       (query-replace-regexp "\\([3]\\)rd"        "\\1^rd^" nil begin end)
       (query-replace-regexp " one to \\([0-9\\.]+\\)" " \\1–\\2" nil begin end)
       (query-replace-regexp "from \\([0-9\\.]+\\) to \\([0-9\\.]+\\)" "\\1–\\2" nil begin end)
       (query-replace-regexp "\\([0-9\\.]+\\) to \\([0-9\\.]+\\)" "\\1–\\2" nil begin end)
       (query-replace-regexp "from \\([0-9\\.]+\\) to \\([0-9\\.]+\\)" "\\1 → \\2" nil begin end)
       (query-replace-regexp "\\([a-z]+\\)- and \\([a-z]+-[a-z]+\\)" "\\1 & \\2" nil begin end)
       (query-replace-regexp "\\([0-9\\.]+\\) to \\([0-9\\.]+\\)" "\\1 → \\2" nil begin end)
       (query-replace-regexp "between \\([0-9\\.]+\\) and \\([0-9\\.]+\\)" "\\1–\\2" nil begin end)
       (query-replace-regexp " \\([0-9\\.]+\\) or \\([0-9\\.]+\\) " " \\1–\\2 " nil begin end)
       (query-replace "First" "1^st^" nil begin end)
       (query-replace "Second" "2^nd^" nil begin end)
       (query-replace "Third" "3^rd^" nil begin end)
       (query-replace "(four|fif|six|seven|eigh|nin|ten)th"        "\\1^th^" nil begin end)
       (query-replace "(four|fif|six|seven|eigh|nin|ten)th"        "\\1^th^" nil begin end)

       ; format abstract sub-headings with bold if we are writing an abstract and not a Markdown file:
       (unless (buffer-file-name)
         (query-replace " -\n    " "" nil begin end)
         (query-replace " -\n" "" nil begin end)
         (query-replace "-\n    " "" nil begin end)
         (query-replace "-\n" "" nil begin end)
         (query-replace "- \n" "" nil begin end)
         (query-replace "-\n" "-" nil begin end)

         (when (and (equal (buffer-name) "foo")
           (not (save-excursion
                  (goto-char (point-min))
                  (re-search-forward "^[0-9#]" nil t))))
  (markdown-remove-newlines-in-paragraphs)) ; once all the hyphenation is dealt with, remove the hard-newlines which are common in PDF copy-pastes. These hard newlines are a problem because they break many string matches, and they make `langcheck` highlight every line beginning/ending in red as an error.
         (query-replace " -- " "---" nil begin end)
         (query-replace " --- " "---" nil begin end)
         (query-replace "--- " "---" nil begin end)
         (query-replace " ---" "---" nil begin end)
         (query-replace "----" "---" nil begin end)
         (query-replace " -\"" "---\"" nil begin end)
         (query-replace "\"- " "\"---" nil begin end)
         (replace-all "► " "- ")
         (replace-all "Key Points\n" "**Key Points**:\n")
         (query-replace "\nQuestion " "\n**Question**: " nil begin end)
         (query-replace "Question:" "**Question**: " nil begin end)
         (replace-all "\n\nPurpose" "\n\n**Purpose**:")
         (replace-all "\nFindings " "\n**Findings**: ")
         (replace-all "\nMeaning " "\n**Meaning**: ")
         (replace-all "\nImportance " "\n**Importance**: ")
         (replace-all "^Importance " "**Importance**: ")
         (replace-all "\nObjective and Method: " "\n**Objective & Method**: ")
         (replace-all "\nObjective " "\n**Objective**: ")
         (replace-all "\nDesign, Setting, and Participants " "\n**Design, Setting, & Participants**: ")
         (replace-all "\nSearch methods\n\n" "\n\n<strong>Search Methods</strong>: ")
         (replace-all " (Methods)" " (<strong>Methods</strong>)")
         (replace-all "\nSelection criteria\n\n" "\n\n<strong>Selection Criteria</strong>: ")
         (replace-all "\nInterventions " "\n**Interventions**: ")
         (replace-all "\nMain Outcomes and Measures " "\n**Main Outcomes & Measures**: ")
         (replace-all "Measurement and Results:" "**Measurement & Results**:")
         (replace-all "\nResults and conclusion " "\n**Results & Conclusions**: ")
         (replace-all "\nResults " "\n**Results**: ")
         (replace-all "\nConclusions and Relevance " "\n**Conclusions & Relevance**: ")
         (replace-all "\nTrial Registration " "\n**Trial Registration**: ")
         (replace-all "Background\n" "\n**Background**: ")
         (replace-all "\nBackground " "**Background**: ")
         (replace-all "\nAims\n" "\n**Aims**: ")
         (replace-all "\nDesign\n" "\n**Design**: ")
         (replace-all "\nSetting\n" "\n**Setting**: ")
         (replace-all "\nParticipants\n" "\n**Participants**: ")
         (replace-all "\nMeasurements\n" "\n**Measurements**: ")
         (replace-all "\nMaterials and methods\n" "\n\n**Method & Materials**: " )
         (replace-all "\nMethods and materials\n" "\n\n**Method & Materials**: " )
         (replace-all "Methods\n" "\n**Methods**: ")
         (replace-all "\nMethods " "\n**Methods**: ")
         (replace-all "Highlights\n" "\n**Highlights**:\n\n")
         (replace-all "Interpretation\n" "\n**Interpretation**: ")
         (replace-all "Funding\n" "\n**Funding**: ")
         (replace-all "Highlights:" "**Highlights**: ")
         (replace-all "Background\\:" "**Background**: ")
         (replace-all "Background\\. " "**Background**: ")
         (replace-all "Abstract:" "**Abstract**: ")
         (replace-all "Context:" "**Context**: ")
         (replace-all "Purpose:" "**Purpose**: ")
         (replace-all "Rationale: " "**Rationale**: ")
         (replace-all "Rationale\n" "**Rationale**: ")
         (replace-all "Rationale: " "**Rationale**: ")
         (replace-all "Rationale\n\n" "**Rationale**: ")
         (replace-all "Study Objectives\n" "**Objectives**: ")
         (replace-all "Study Objectives:\n" "**Objectives**: ")
         (replace-all "Objective:" "**Objective**: ")
         (replace-all "Objectives:" "**Objectives**: ")
         (replace-all "Objectives\n" "\n**Objectives**: ")
         (replace-all "Aims\n\n" "\n**Aims**: ")
         (query-replace "Description:" "**Description**: " nil begin end)
         (replace-all "Design:" "**Design**: ")
         (replace-all "Design\n\n" "\n**Design**: ")
         (replace-all "Study Design and Setting\n" "**Study Design & Setting**: ")
         (replace-all "Subjects:" "**Subjects**: ")
         (replace-all "Methods:" "**Methods**: ")
         (replace-all "Methods\\. " "**Methods**: ")
         (replace-all "\nMethods and findings\n\n" "\n\n**Methods & Findings**: ")
         (replace-all "\n\nDesign and methods" "\n\n**Design & Methods**: ")
         (replace-all "Method:" "**Method**:")
         (replace-all "Design, Setting, and Participants:" "**Design, Setting, & Participants**: ")
         (replace-all "Participants and Setting:" "**Participants & Setting**: ")
         (replace-all "Setting:" "**Setting**: ")
         (replace-all "Setting\n\n" "\n**Setting**: ")
         (replace-all "Participants:" "**Participants**: ")
         (replace-all "Participants\n\n" "\n**Participants**: ")
         (replace-all "Meaning:" "**Meaning**: ")
         (replace-all "Intervention:" "**Intervention**: ")
         (replace-all "\nInterventions:" "\n**Intervention**: ")
         (replace-all "Data Sources:" "**Data Sources**: ")
         (replace-all "Data sources: " "**Data Sources**: ")
         (replace-all "Data sources\n" "**Data Sources**: ")
         (replace-all "Exposures:" "**Exposures**: ")
         (replace-all "\nExposures " "\n**Exposures**: ")
         (replace-all "Main Outcome Measures:" "**Main Outcome Measures**: ")
         (replace-all "Main outcome measures:\n" "**Main Outcome Measures**: ")
         (replace-all "Main outcome measure:\ " "**Main Outcome Measures**: ")
         (replace-all "Main outcome measures\n" "**Main Outcome Measures**: ")
         (replace-all "\nMain outcome measures " "\n**Main Outcome Measures**: ")
         (replace-all "Measurements:" "**Measurements**: ")
         (replace-all "Measurements\n\n" "\n**Measurements**: ")
         (replace-all "Main Outcome Measures:" "**Outcome Measures**: ")
         (replace-all "Main Outcome Measures\n" "**Outcome Measures**: ")
         (replace-all "Outcome measures:" "**Outcome Measures**: ")
         (replace-all "Outcomes:" "\n**Outcomes**: ")
         (replace-all "Outcomes\n\n" "\n**Outcomes**: ")
         (replace-all "\n\nMethods and Results\n" "\n\n**Methods & Results**: ")
         (replace-all "Results:" "**Results**: ")
         (replace-all "Results\\. " "**Results**: ")
         (replace-all "Measurements and Results\n" "\n**Measurements & Results**: ")
         (replace-all "Results\n" "\n**Results**: ")
         (replace-all "Aims:" "**Aims**: ")
         (replace-all "Aim:\n" "**Aims**: ")
         (replace-all "Aim\n\n" "\n\n**Aim**: ")
         (replace-all "Methodology/Principal Findings\n\n" "\n**Methodology/Principal Results**: ")
         (replace-all "Methodology\n\n" "\n**Methodology**: ")
         (replace-all "Principal Findings\n\n" "\n**Principal Results**: ")
         (replace-all "Methods and Findings\n\n" "\n**Methods & Results**: ")
         (replace-all "Findings\n\n" "\n**Results**: ")
         (replace-all "Findings\n" "\n**Results**: ")
         (replace-all "\nScope of review\n\n" "\n\n**Scope of Review**: ")
         (replace-all "\n\nReview methods " "\n\n**Review Methods**: ")
         (replace-all "\nMajor conclusions\n\n" "\n\n**Major Conclusions**: ")
         (replace-all "\nFindings: " "\n**Results**: ")
         (replace-all "\nFinding: " "\n**Results**: ")
         (replace-all "\nFinding " "\n**Results**: ")
         (replace-all "\nQuestion " "\n**Question**: ")
         (replace-all "^Question " "**Question**: ")
         (replace-all "Significance:" "**Significance**: ")
         (replace-all "Conclusions\n\n" "\n**Conclusions**: ")
         (replace-all "Conclusions\n" "\n**Conclusions**: ")
         (replace-all "Conclusion:" "**Conclusion**: ")
         (replace-all "Conclusions\\. " "**Conclusions**: ")
         (replace-all "Conclusions:" "**Conclusions**: ")
         (replace-all "Conclusions and Relevance:" "**Conclusions & Relevance**: ")
         (replace-all "Conclusion\n" "\n**Conclusion**: ")
         (replace-all "\nConclusions " "\n**Conclusion**: ")
         (replace-all "\n\nConclusion " "\n\n**Conclusion**: ")
         (replace-all "Method\n" "\n**Method**: ")
         (replace-all "Objective\n" "**Objective**: ")
         (replace-all "^Objective " "**Objective**: ")
         (replace-all "OBJECTIVE " "**Objective**: ")
         (replace-all "OBJECTIVE\n\n" "\n**Objective**: ")
         (replace-all "OBJECTIVE: " "\n**Objective**: ")
         (replace-all "\nObjectives " "\n**Objectives**: ")
         (replace-all "INTRODUCTION\n\n" "\n**Background**: ")
         (replace-all "INTRODUCTION\n" "**Background**: ")
         (replace-all "INTRODUCTION: " "**Background**: ")
         (replace-all "Introduction\n\n" "\n**Background**: ")
         (query-replace-regexp "^Introduction\n" "\n**Background**: " nil begin end)
         (query-replace-regexp "^INTRODUCTION " "\n**Background**: " nil begin end)
         (replace-all "RATIONALE\n\n" "\n**Rationale**: ")
         (replace-all "RATIONALE\n" "**Rationale**: ")
         (query-replace-regexp "^RATIONALE " "\n**Rationale**: " nil begin end)
         (replace-all "RESULTS\n\n" "\n**Results**: ")
         (replace-all "\nRESULTS: " "\n**Results**: ")
         (query-replace-regexp "^RESULTS " "\n**Results**: " nil begin end)
         (replace-all "Discussion\n\n" "\n**Discussion**: ")
         (replace-all "\n\nDiscussion\n\n" "\n\n**Discussion**: ")
         (replace-all "\n\nDiscussion" "\n\n**Discussion**: ")
         (replace-all "\n\nDiscussion: " "\n\n**Discussion**: ")
         (replace-all "CONCLUSION\n\n" "\n**Conclusion**: ")
         (replace-all "CONCLUSION\n" "\n**Conclusion**: ")
         (query-replace-regexp "^CONCLUSION " "\n**Conclusion**: " nil begin end)
         (query-replace "BACKGROUND" "**Background**: " nil begin end)
         (replace-all "Background and Aims\n" "**Background & Aims**: ")
         (replace-all "\n\nMETHODS\n" "\n\n**Method**: ")
         (replace-all "METHODS: " "**Method**: ")
         (replace-all "\n\nRESULTS" "\n\n**Results**: ")
         (replace-all "\nMain results\n\n" "\n**Results**: ")
         (replace-all "\n\nCONCLUSIONS" "\n\n**Conclusions**: ")
         (replace-all "CONCLUSIONS\n\n" "\n**Conclusions**: ")
         (replace-all "Conclusions/Significance\n\n" "\n**Conclusion**: ")
         (replace-all "\nAuthors' conclusions\n\n" "\n\n**Conclusions**: ")
         (replace-all "\nPractice implications" "\n**Practice Implications**: ")
         (replace-all "Trial Registration:" "**Trial Registration**: ")
         (replace-all "Trial registration:" "**Trial Registration**: ")
         (replace-all "Trial Registration number:" "**Trial Registration Number**: ")
         (replace-all "Trial Registration " "**Trial Registration**: ")
         (replace-all "Clinical Trial Registration:\n" "**Clinical Trial Registration**: ")
         (replace-all "Clinical trial registration\n" "**Clinical Trial Registration**: ")
         (replace-all "Clinical Translation\n\n" "\n**Clinical Translation**: ")
         (replace-all "\nSystematic review registration " "\n**Systematic Review Registration**: " )
         (replace-all "eLife digest\n\n" "\n**eLife digest**: ")
         (replace-all "\nKEYWORDS:" "[**Keywords**:")
         (replace-all "\nKEY WORDS:" "[**Keywords**:")
         (replace-all "\nkey words" "[**Keywords**:")
         (replace-all "\nkey Words" "[**Keywords**:")
         (replace-all "\\[Keywords:" "[**Keywords**:")
         (replace-all "\\[Keyword:" "[**Keywords**:")
         (replace-all "Key words::" "[**Keywords**:")
         (replace-all "Key Messages\n" "**Key Messages**:")
         (replace-all "\nAuthor summary\n\n" "\n\n**Author Summary**: ")
         (replace-all "\nAuthor Summary\n\n" "\n\n**Author Summary**: ")
         (replace-all "Purpose\n\n" "\n**Purpose**: ")
         (replace-all "Purpose\n" "**Purpose**: ")
         (replace-all "Design/methodology/approach\n\n" "\n**Design/Methodology/Approach**: ")
         (replace-all "RESEARCH DESIGN AND METHODS " "**Research Design & Method**: ")
         (replace-all "RESEARCH DESIGN AND METHODS\n\n" "\n**Research Design & Method**: ")
         (replace-all "METHOD: " "**Method**: ")
         (replace-all "\nDesign " "\n**Design**: ")
         (replace-all "Research limitations/implications\n\n" "\n**Research Limitations/Implications**: ")
         (replace-all "Strengths and Limitations\n\n" "\n**Strengths & Limitations**: ")
         (replace-all "\nLimitations\n" "\n\n**Limitations**: ")
         (replace-all "\nLimitations.\n" "\n**Limitations**: ")
         (replace-all "\nData Sources\n" "\n**Data Sources**: ")
         (replace-all "\nData Sources " "\n**Data Sources**: ")
         (replace-all "\nData sources " "\n**Data Sources**: ")
         (replace-all "\nStudy Selection\n" "\n**Study Selection**: ")
         (replace-all "\nStudy Selection " "\n**Study Selection**: ")
         (replace-all "\nStudy selection: " "\n**Study Selection**: ")
         (replace-all "\nData Extraction\n" "\n**Data Extraction**: ")
         (replace-all "\nData extraction: " "\n**Data Extraction**: ")
         (replace-all "\nData extraction and synthesis " "\n**Data Extraction & Synthesis**: ")
         (replace-all "\nData Extraction and Synthesis " "\n**Data Extraction & Synthesis**: ")
         (replace-all "\nData Synthesis\n" "\n**Data Synthesis**: ")
         (replace-all "\nData synthesis: " "\n**Data Synthesis**: ")
         (replace-all "\nData Synthesis " "\n**Data Synthesis**: ")
         (replace-all "\nData collection and analysis\n\n" "<strong>Data & Analysis</strong>: ")
         (replace-all "\nEligibility criteria for selecting studies " "\n**Eligibility Criteria For Selecting Studies**: ")
         (replace-all "Practical implications\n\n" "\n**Practical Implications**: ")
         (replace-all "Social implications\n\n" "\n**Social Implications**: ")
         (replace-all "Educational Impact and Implications Statement:" "**Educational Impact & Implications Statement**:")
         (replace-all "Originality/value\n\n" "\n**Originality/Value**: ")
         (replace-all "\nKeywords\n" "[**Keywords**: ")
         (replace-all "\nKeywords: " "[**Keywords**: ")
         (replace-all "\nKey Words\n" "\n[**Keywords**: ")
         (replace-all "\nKey Words:\n" "[**Keywords**: ")
         (replace-all "\nKey Words: " "[**Keywords**: ")
         (replace-all "\nKey words\n" "[**Keywords**: ")
         (replace-all "\\[Key Words: " "[**Keywords**: ")
         (replace-all "\\[Key words: " "[**Keywords**: ")
         (replace-all "\\[KEYWORDS: " "[**Keywords**: ")
         (replace-all "\\[K E Y W O R D S: " "[**Keywords**: ")
         (query-replace "\nAbbreviations\n" "[**Abbreviations**: " nil begin end)
         (query-replace ".\n<strong>" ".\n\n**" nil begin end)
         (replace-all "\\[<strong>Keywords</strong>:" "\n\n[**Keywords**:")
       )
       (replace-all ",”" "”,")
       (replace-all ";”" "”;")
       (replace-all ",\"" "\",")
       (replace-all ";\"" "\";")
       (replace-all "\n\n\n" "\n\n")
       (replace-all ":  " ": ")
       (replace-all " ► " "\n- ")
       (replace-all "Previous article in issueNext article in issue" "")
       (replace-all "Previous article in issue\nNext article in issue" "")
       (replace-all "\nAbstract\n" "\n")
       (replace-all "statistical significance" "statistical-significance")
       (replace-all "statistically significant" "statistically-significant")
       (replace-all "Statistically significant" "Statistically-significant")
       (replace-all "clinical significance" "clinical-significance")
       (replace-all "clinically significant" "clinically-significant")
       (replace-all "genome-wide significant" "genome-wide statistically-significant")
       (replace-all "genome-wide significance" "genome-wide statistical-significance")
       (funcall-interactively #'query-replace-regexp " significantly" (query-replace-compile-replacement "\\,(let* ((replacements '((?1 \" statistically-significantly\") (?2 \" importantly\") (?3 \" largely\") (?4 \" substantially\"))) (choice (read-multiple-choice \"Replace: \" replacements))) (second choice))" t) nil begin end)
       (funcall-interactively #'query-replace-regexp " significant" (query-replace-compile-replacement "\\,(let* ((replacements '((?1 \" statistically-significant\") (?2 \" important\") (?3 \" large\") (?4 \" substantial\"))) (choice (read-multiple-choice \"Replace: \" replacements))) (second choice))" t) nil begin end)
       (funcall-interactively #'query-replace-regexp " significance" (query-replace-compile-replacement "\\,(let* ((replacements '((?1 \" statistical-significance\") (?2 \" importance\") (?3 \" large\"))) (choice (read-multiple-choice \"Replace: \" replacements))) (second choice))" t) nil begin end)
       (query-replace "non-significantly" "non-statistically-significantly" nil begin end)
       (query-replace "non-significance" "non-statistical-significance" nil begin end)
       (query-replace "nonsignificant" "non-statistically-significant" nil begin end)
       (query-replace " Homo habilis" " _Homo habilis_" nil begin end)
       (query-replace " Homo erectus" " _Homo erectus_" nil begin end)
       (query-replace " Homo " " _Homo_ " nil begin end)
       (query-replace "https://www.gwern.net/" "https://gwern.net/" nil begin end)
       (query-replace "https://gwern.net/" "/" nil begin end)

       ; shout-out to typography nerds by using the proper logotypes:
       (let ((case-fold-search t) (search-upper-case t)) ; need these to override `query-replace` magic‽
         (query-replace "P > 0" "_p_ > 0" nil begin end)
         (query-replace "p < " "_p_ <" nil begin end)
         (query-replace "P ≤ " "_p_ ≤ " nil begin end)
         (query-replace "p<" "_p_ < "   nil begin end)
         (query-replace "p>" "_p_ > "   nil begin end)
         (query-replace "p < " "_p_ <"   nil begin end)
         (query-replace "P < " "_p_ <" nil begin end)
         (query-replace "P<" "_p_ <"   nil begin end)
         (query-replace "P <" "_p_ <"   nil begin end)
         (query-replace "_P_ <" "_p_ <"   nil begin end)
         (query-replace "_P_<" "_p_ <" nil begin end)
         (query-replace "_p_=." "_p_ = 0." nil begin end)
         (query-replace "_p_<." "_p_ < 0." nil begin end)
         (query-replace "_p_ <." "_p_ < 0." nil begin end)
         (query-replace "( _p_ = " "(_p_ = " nil begin end)
         (query-replace "<0." " < 0." nil begin end)
         (query-replace "p = " "_p_ = " nil begin end)
         (query-replace "( P = " "(_p_ = " nil begin end)
         (query-replace " P < " " _p_ < " nil begin end)
         (query-replace "P-for-interaction" "_p_-for-interaction" nil begin end)
         (query-replace " p = " " _p_ = " nil begin end)
         (query-replace "(ps " "(<em>p</em>s " nil begin end)
         (query-replace "k-NN" "_k_-NN" nil begin end)
         (query-replace "𝜌" "_ρ_" nil begin end)

         (query-replace "ρSDS" "ρ~SDS~" nil begin end)
         (query-replace "tpre-Neolithic" "<em>t</em><sub>pre-Neolithic</sub>" nil begin end)
         (query-replace "tNearEast" "<em>t</em><sub>Near East</sub>" nil begin end)
         (query-replace "tNeolithic" "<em>t</em><sub>Neolithic</sub>" nil begin end)
         (query-replace " b = " " _b_ = " nil begin end)
         (query-replace "(b = " "(_b_ = " nil begin end)
         (query-replace "[b=" "[_b_ =" nil begin end)
         (query-replace "[b = " "[_b_ =" nil begin end)
         (query-replace "b=" "_b_ =" nil begin end)
         (query-replace "_b_=" "_b_ =" nil begin end)
         (query-replace "(b=" "(_b_ =" nil begin end)
         (query-replace " t <" " _t_ <"  nil begin end)
         (query-replace " t =" " _t_ =" nil begin end)
         (query-replace "(t =" "(_t_ =" nil begin end)
         (query-replace " t-test" " _t_-test" nil begin end)
         (query-replace " t test" " _t_-test" nil begin end)
         (query-replace " R2 " " R^2^ " nil begin end)
         (query-replace "(R2 " "(R^2^ " nil begin end)
         (query-replace "R2=" " R^2^ ="  nil begin end)
         (query-replace "R2<" " R^2^ <" nil begin end)
         (query-replace "R2>" " R^2^ <" nil begin end)
         (query-replace "r2" "R^2^" nil begin end)
         (query-replace "χ2" "χ^2^" nil begin end)
         (query-replace "mm 2" "mm^2^" nil begin end)
         (query-replace "m−2" "m^−2^" nil begin end)
         (query-replace "rxy " "<em>r<sub>xy</sub></em>" nil begin end)
         (query-replace "n-of-1" "<em>n</em>-of-1" nil begin end)
         (query-replace " g = " " _g_ = " nil begin end)
         (query-replace " g's" " _g_'s" nil begin end)
         (query-replace "(g = " "(_g_ = " nil begin end)
         (query-replace "(g value" "(_g_ value" nil begin end)
         (query-replace "(m = " "(_m_ =" nil begin end)
         (query-replace "LaTeX" "<span class=\"logotype-latex\">L<span class=\"logotype-latex-a\">a</span>T<span class=\"logotype-latex-e\">e</span>X</span>" nil begin end) ; <span class="logotype-latex">L<span class="logotype-latex-a">a</span>T<span class="logotype-latex-e">e</span>X</span>
         (query-replace "LATEX" "<span class=\"logotype-latex\">L<span class=\"logotype-latex-a\">a</span>T<span class=\"logotype-latex-e\">e</span>X</span>" nil begin end)
         (query-replace "TeX" "<span class=\"logotype-tex\">T<sub>e</sub>X</span>" nil begin end) ; <span class="logotype-tex">T<sub>e</sub>X</span>
         (query-replace "TEX" "<span class=\"logotype-tex\">T<sub>e</sub>X</span>" nil begin end))
       (query-replace "Nepeta cataria" "_Nepeta cataria_" nil begin end)
       (query-replace "MC4R" "_MC4R_" nil begin end)
       (query-replace "two thirds" "2⁄3" nil begin end)
       (query-replace "two-thirds" "2⁄3" nil begin end)
       (query-replace "three-fourths" "3⁄4" nil begin end)
       (query-replace "three-fifths" "3⁄5" nil begin end)
       (query-replace-regexp "\\([0-9]+\\) of \\([0-9]+\\)" "\\1⁄\\2" nil begin end)
       (query-replace-regexp "\\([0-9]+\\) of the \\([0-9]+\\)" "\\1⁄\\2" nil begin end)
       (query-replace-regexp " \\([0-9][0-9]?[0-9]?\\) of \\([0-9][0-9]?[0-9]?\\) " " \\1⁄\\2 " nil begin end)
       (query-replace-regexp "\\([0-9]+\\) out of \\([0-9]+\\)" "\\1⁄\\2" nil begin end)
       (query-replace-regexp "\\([0-9]+\\) in every \\([0-9]+\\)" "\\1⁄\\2" nil begin end) ; eg. "approximately one in every 10 citations across leading psychology journals is inaccurate"
       (query-replace "...." "..." nil begin end)
       (query-replace "....." "..." nil begin end)
       (query-replace-regexp "\n..([A-Za-z])" "\n...\\1" nil begin end) ; replace malformed '...' ellipsis excerpts
       (query-replace-regexp "^\\.\\.\\. " "..." nil begin end)
       (query-replace " = ." " = 0." nil begin end)
       (query-replace "Ss" "Subjects" nil begin end) ; PsycNET APA abbreviation
       (query-replace-regexp " (PsycInfo Database Record (c) [12][0-9]+ APA, all rights reserved)" "" nil begin end) ; PsycNET copyright junk
       (query-replace " • " ", " nil begin end) ; some 'Keywords' sections are always MIDDLE DOT formatted

       (query-replace-regexp "\\([0-9]+\\)·\\([0-9]+\\)" "\\1.\\2" nil begin end) ; mostly for the Lancet

       (query-replace "Figs. " "Figures " nil begin end)
       (query-replace "Fig. " "Figure " nil begin end)
       (query-replace-regexp "Supplementary [fF]ig\\. \\([0-9]+[a-fA-F]*\\)\\." "**Supplementary Figure \\1**."  nil begin end) ; 'Supp Fig. 1. ', 'Fig. 2a)' etc
       (query-replace-regexp "Supplementary [fF]ig\\. \\([0-9]+[a-fA-F]*\\)"    "**Supplementary Figure \\1**"   nil begin end) ; 'Supp Fig. 1,', 'Fig. 2a,' etc
       (query-replace-regexp "Supplementary [fF]igure \\([0-9]+[a-fA-F]*\\)\\." "**Supplementary Figure \\1**."  nil begin end) ; 'Supp Figure 1. The graph' etc
       (query-replace-regexp "Supplementary ([fF]ig\\. \\([0-9]+[a-fA-F]*\\))"  "(**Supplementary Figure \\1**)" nil begin end) ; (Supp Fig. 3b)
       (query-replace-regexp "Supplementary ([fF]igure \\([0-9]+[a-fA-F]*\\))"  "(**Supplementary Figure \\1**)" nil begin end) ; (Supp Figure 3b)
       (query-replace-regexp "Supplementary ([fF]ig\\. \\([0-9]+[a-fA-F]*\\),"  "(**Supplementary Figure \\1**," nil begin end) ; (Supp Fig. 3b,
       (query-replace-regexp "Supplementary [fF]igures \\([0-9]+[a-fA-F]*\\) and \\([0-9]+[a-fA-F]*\\)"  "**Supplementary Figures \\1** & **\\2**" nil begin end)
       (query-replace-regexp "[fF]ig\\.? ?\\(S?\\)\\([0-9\\.]+[a-fA-F]*\\)\\.?" "**Figure \\1\\2**."  nil begin end) ; 'Fig. 1. ', 'Fig. 2a)', 'Figure 1.5' etc
       (query-replace-regexp "[fF]ig\\.? \\(S?\\)\\([0-9\\.]+[a-fA-F]*\\)"    "**Figure \\1\\2**"   nil begin end) ; 'Fig. 1,', 'Fig. 2a,' etc
       (query-replace-regexp "[fF]igure \\(S?\\)\\([0-9\\.]+[a-fA-F]*\\.?\\)" "**Figure \\1\\2**"  nil begin end) ; 'Figure 1. The graph' etc
       (query-replace-regexp "([fF]ig\\.? \\(S?\\)\\([0-9\\.]+[a-fA-F]*\\))"  "(**Figure \\1\\2**)" nil begin end) ; (Fig. 3b)
       (query-replace-regexp "([fF]igure \\(S?\\)\\([0-9\\.]+[a-fA-F]*\\))"  "(**Figure \\1\\2**)" nil begin end) ; (Figure 3b)
       (query-replace-regexp "([fF]ig\\.? \\(S?\\)\\([0-9\\.]+[a-fA-F]*\\),"  "(**Figure \\1\\2**," nil begin end) ; (Fig. S3b,

       (query-replace-regexp "[aA]ppendix.? ?\\([a-zA-Z]?\\)\\([0-9\\.]+[a-fA-F]*\\)"  "**Appendix \\1\\2**" nil begin end) ; 'Appendix A2'

       ; '§ SECTION SIGN' is better than writing out '<strong>Section N</strong>' everywhere. It's much shorter, we already use SECTION SIGN heavily, it reduces overuse of bold, is easier to grep for, and it saves a bit of time formatting annotations (because of the lack of lookahead/lookbehind in these regexp rewrites, 'Section N' will match every time, even if it's already wrapped in <strong></strong>/**bolding**, and I have to waste time skipping them). It would be nice to symbolize Figure/Table/Experiment/Data as well, but there's no widely-understood symbol which could be used, and usually no abbreviation either. (Perhaps 'Supplement.*' could be replaced by just 'S' and 'Figure' by 'Fig.' at some point…)
       (query-replace-regexp "[Ss]ection ?\\([0-9.]+[a-fA-F]*\\)"  "§\\1" nil begin end) ; 'Section 9' → '§9'
       (query-replace-regexp "[Ss]ections ?\\([0-9.]+[a-fA-F]*\\) and \\([0-9.]+[a-fA-F]*\\)"  "§\\1 & §\\2" nil begin end) ; 'Sections 1 and 2' → '§1 & §2'

       (query-replace-regexp "Chapter \\([0-9]+[a-fA-F]*\\)" "**Ch\\1**"  nil begin end) ; 'Chapter 1', 'Chapter 7a' etc

       (query-replace-regexp "Supplementary [Tt]able\\. \\([0-9]+[a-fA-F]*\\)\\." "**Supplementary Table \\1**."  nil begin end) ; 'Table. 1. ', 'Table. 2a)' etc
       (query-replace-regexp "Supplementary [Tt]able\\. \\([0-9]+[a-fA-F]*\\)"    "**Supplementary Table \\1**"   nil begin end) ; 'Table. 1,', 'Table. 2a,' etc
       (query-replace-regexp "Supplementary [Tt]able \\([0-9]+[a-fA-F]*\\)\\." "**Supplementary Table \\1**:"  nil begin end) ; 'Table 1. The graph' etc
       (query-replace-regexp "Supplementary ([Tt]able\\. \\([0-9]+[a-fA-F]*\\))"  "(**Supplementary Table \\1**)" nil begin end) ; (Table. 3b)
       (query-replace-regexp "Supplementary ([Tt]able \\([0-9]+[a-fA-F]*\\))"  "(**Supplementary Table \\1**)" nil begin end) ; (Table 3b)
       (query-replace-regexp "Supplementary ([Tt]able\\. \\([0-9]+[a-fA-F]*\\),"  "(**Supplementary Table \\1**," nil begin end) ; (Table. 3b,
       (query-replace-regexp "Supplementary [Tt]ables \\([0-9]+[a-fA-F]*\\) and \\([0-9]+[a-fA-F]*\\)" "**Supplementary Tables \\1** & **\\2**" nil begin end)

       (query-replace-regexp "[Tt]ables?\\.? \\(S?[0-9]+[a-fA-F]*\\)\\." "**Table \\1**:"  nil begin end) ; 'Table. 1. ', 'Table. 2a)' etc
       (query-replace-regexp "[Tt]ables?\\.? \\(S?[0-9]+[a-fA-F]*\\)"    "**Table \\1**"   nil begin end) ; 'Table. 1,', 'Table. 2a,' etc
       (query-replace-regexp "[Tt]ables?\\.? \\(S?[a-fA-F]+[0-9]+\\)"    "**Table \\1**"   nil begin end) ; 'Table S3'
       (query-replace-regexp "[Tt]ables? \\(S?[0-9]+[a-fA-F]*\\)\\." "**Table \\1**:"  nil begin end) ; 'Table 1. The graph' etc
       (query-replace-regexp "([Tt]ables?\\.? \\(S?[0-9]+[a-fA-F]*\\))"  "(**Table \\1**)" nil begin end) ; (Table. 3b)
       (query-replace-regexp "([Tt]ables? \\(S?[0-9]+[a-fA-F]*\\))"  "(**Table \\1**)" nil begin end) ; (Table 3b)
       (query-replace-regexp "([Tt]ables?\\.? \\(S?[0-9]+[a-fA-F]*\\),"  "(**Table \\1**," nil begin end) ; (Table. 3b,
       (query-replace-regexp "[Tt]ables?\\.? \\(S?[0-9]+[a-fA-F]*\\) and \\([0-9]+[a-fA-F]*\\)"  "**Table \\1** & **\\2**" nil begin end) ; 'Tables 9 and 10'

       (query-replace-regexp "Experiment \\([0-9]+[a-fA-F]*\\)" "**Experiment \\1**"  nil begin end) ; 'Experiment 1', 'Experiment 2a' etc
       (query-replace "Experiments 1 and 2" "**Experiments 1** & **2**" nil begin end)
       (query-replace-regexp "[Ss]tudy \\([0-9]+[a-fA-F]*\\)" "**Study \\1**"  nil begin end)
       (query-replace-regexp "[Ss]tudies \\([0-9]+[a-fA-F]*\\)[-–]+\\([0-9]+[a-fA-F]*\\)" "**Studies \\1--\\2**"  nil begin end)
       (query-replace-regexp "[Ss]tudies \\([0-9]+[a-fA-F]*\\) and \\([0-9]+[a-fA-F]*\\)" "**Studies \\1** & **\\2**"  nil begin end)
       (query-replace-regexp "[Ss]tudies \\([0-9]+[a-fA-F]*\\), \\([0-9]+[a-fA-F]*\\), and \\([0-9]+[a-fA-F]*\\)" "**Studies \\1**, **\\2**, & **\\3**"  nil begin end)
       (query-replace-regexp "[Ss]tudies \\([0-9]+[a-fA-F]*\\), \\([0-9]+[a-fA-F]*\\), \\([0-9]+[a-fA-F]*\\)" "**Studies \\1**, **\\2**, **\\3**"  nil begin end)

       (query-replace-regexp "^\\( *\\)\\([0-9]+\\)\\. " "\\1#. " nil begin end) ; Markdown ordered-list number formatting: see default.css for more discussion, but using '1./2./3.' list numbering in Markdown yields hardwired number formatting rather than plain `<ol>`, which causes styling problems when nested in lists.
       (query-replace-regexp "Supplementary Data \\([0-9]+[a-fA-F]*\\)" "**Supplementary Data \\1**"  nil begin end) ; '(Supplementary Data 7)'
       (query-replace " 0)" " (0)" nil begin end) ; we do the number+paren check after the '(study 1)' check
       (query-replace " 1)" " (1)" nil begin end)
       (query-replace " 2)" " (2)" nil begin end)
       (query-replace " 3)" " (3)" nil begin end)
       (query-replace " 4)" " (4)" nil begin end)
       (query-replace " 5)" " (5)" nil begin end)
       (query-replace " 6)" " (6)" nil begin end)
       (query-replace " 7)" " (7)" nil begin end)
       (query-replace " 8)" " (8)" nil begin end)
       (query-replace " 9)" " (9)" nil begin end)
       (query-replace " 10)" " (10)" nil begin end)
       (check-parens)

       (query-replace-regexp " percent\\([[:punct:]]\\)" "%\\1" nil begin end)
       (query-replace-regexp "\\([[:digit:]]\\)×10−\\([[:digit:]]+\\)" "\\1×10<sup>−\\2</sup>" nil begin end) ; minus sign version
       (query-replace-regexp " --\\([[:digit:]]\\)" " −\\1" nil begin end) ; some minuses are rendered as hyphens, so switch to minus sign
       (query-replace-regexp "\\([[:digit:]]\\)×10-\\([[:digit:]]+\\)" "\\1×10<sup>−\\2</sup>" nil begin end) ; hyphen version
       (query-replace-regexp " [x×] 10-\\([[:digit:]]+\\)" "×10<sup>−\\1</sup>" nil begin end) ; hyphen version
       (query-replace-regexp "[x×] ?10--\\([[:digit:]]+\\)" "× 10<sup>−\\1</sup>" nil begin end)
       (query-replace-regexp "\\([[:digit:]]+\\)[x×]10-\\([[:digit:]]+\\)" "\\1 × 10<sup>−\\2</sup>" nil begin end)
       (query-replace-regexp "\\([[:digit:]\\.]+\\)[Ee]-\\([[:digit:]]+\\)" "\\1 × 10<sup>−\\2</sup>" nil begin end) ; 2.0E-26, 2.0e-26
       ; (query-replace-regexp "[x×] ?10--\\([0-9]+\\)" "× 10<sup>−\\1</sup>" nil begin end)
       (query-replace-regexp "\\([a-zA-Z0-9]\\.\\)\\([[:digit:]]+\\) \\([A-Z]\\)" "\\1<sup>\\2</sup> \\3" nil begin end) ; look for copy-pasted footnotes, like "X works great.13 Therefore"
       (query-replace-regexp "\\([a-zA-Z0-9.]\\”\\)\\([[:digit:]]+\\) \\([A-Z]\\)" "\\1<sup>\\2</sup> \\3" nil begin end)
       (query-replace-regexp "\\([[:punct:]]\\)\\[\\([0-9, -]+\\)\\] " "\\1<sup>\\2</sup> " nil begin end) ; Wikipedia-style referencs: "Foo.[33]" or "is around 75%,[83 but varies"
       ; (query-replace-regexp "\\([[:punct:]]\\)\\([0-9]+\\) " "\\1<sup>\\2</sup> " nil begin end) ; looser
       (query-replace-regexp "\\([[:punct:]]\\)\\([0-9,- ]+\\)$" "\\1<sup>\\2</sup>" nil begin end) ; looser: handle end of line
       (query-replace-regexp " \\[\\([0-9, -]+\\)\\]\\([[:punct:]]\\)" "\\2<sup>\\1</sup> " nil begin end) ; 'contributing to higher energy intake [42].'
       (query-replace-regexp "\\[\\([0-9, -]+\\)\\] " "<sup>\\1</sup> " nil begin end)
       (query-replace-regexp "\\([0-9]+\\)- to \\([0-9]+\\)-" "\\1--\\2-" nil begin end) ; "18- to 20-year-olds" → "18--20-year-olds"
       (query-replace-regexp "\\([0-9]+\\)- and \\([0-9]+\\)-" "\\1 & \\2-" nil begin end) ; "We use 1979- and 1997-cohort National Longitudinal Survey of Youth (NLSY) data" → "We use 1979 & 1997-cohort"

       (query-replace-regexp "\\([[:alnum:]]\\)- " "\\1---" nil begin end)
       (query-replace-regexp "\\([[:alnum:]]\\)\\.\\. " "\\1... " nil begin end)
       (query-replace-regexp "\\([0-9]\\) % " "\\1% " nil begin end)
       (query-replace-regexp "\\([0-9]\\)folds?" "\\1×" nil begin end)
       (query-replace-regexp "\\([0-9]\\)-folds?" "\\1×" nil begin end)
       (query-replace-regexp "\\([0-9]\\) folds?" "\\1×" nil begin end)
       (query-replace "1st" "1^st^" nil begin end)
       (query-replace "2nd" "2^nd^" nil begin end)
       (query-replace "3rd" "3^rd^" nil begin end)
       (query-replace ":.0" ": 0.0" nil begin end)
       (query-replace "−." "−0." nil begin end)
       (query-replace " -." " −0." nil begin end)
       (query-replace "\\([[:digit:]]\\) %" "\\1%" nil begin end)
       (query-replace-regexp "\\([a-zA-Z,]\\) \\.\\([[:digit:]]\\)" "\\1 0.\\2" nil begin end)
       (query-replace-regexp "^\\.\\([[:digit:]]\\)" "0.\\1" nil begin end)
       (query-replace-regexp "\\([a-zA-Z]\\) −\\.\\([[:digit:]]\\)" "\\1 −0.\\2" nil begin end)
       (query-replace-regexp "= −\\.\\([[:digit:]]\\)" "= −0.\\1" nil begin end)
       (query-replace-regexp "= -\\.\\([[:digit:]]\\)" "= −0.\\1" nil begin end)
       (query-replace-regexp ", -\\.\\([[:digit:]]\\)" ", −0.\\1" nil begin end)
       (query-replace-regexp " \\.\\([[:digit:]]+\\)" " 0.\\1" nil begin end) ; eg " .47"
       (query-replace-regexp "and -\\.\\([[:digit:]]\\)" "and −0.\\1" nil begin end)
       (query-replace-regexp "\\([[:digit:]]\\.[[:digit:]]+\\)-\\.\\([[:digit:]]\\)" "\\1--0.\\2" nil begin end)

       (query-replace-regexp "--\\.\\([[:digit:]]+\\)" "--0.\\1" nil begin end)
       (query-replace-regexp " \\+\\.\\([[:digit:]]+\\)" " +0.\\1" nil begin end) ; '_r_ = +.33.'
       (query-replace-regexp "\\[\\.\\([[:digit:]]+\\)" "[0.\\1" nil begin end)
       (query-replace-regexp "10−\\([[:digit:]]+\\)" "10^−\\1^" nil begin end)
       (query-replace-regexp "− \\([[:digit:]]+\\)" "−\\1" nil begin end)
       (query-replace-regexp "\\([[:digit:]]+\\)-)" "\\1--)" nil begin end)
       (query-replace-regexp "\\([[:digit:]]+\\)%−\\([[:digit:]]+\\)%" "\\1%--\\2%" nil begin end) ; MINUS SIGN
       ; de-LaTeX where the power is unnecessary
       (query-replace-regexp "\\$\\([[:alnum:]]\\)\\$" "_\\1_" nil begin end)
       (query-replace-regexp "\\$\\([[:alnum:]]\\)^\\([[:alnum:]]\\)\\$" "_\\1^\\2^_" nil begin end)
       (query-replace-regexp "\\$\\([[:alnum:]]\\)_\\([[:alnum:]]\\)\\$" "_\\1~\\2~_" nil begin end)
       (query-replace-regexp "\\$\\\\sqrt{\\([[:digit:]]+\\)}\\$" "√\\1" nil begin end)
       ; fancy inline fractions using special Unicode slash:
       (query-replace-regexp " \\([[:digit:]]\\)/\\([[:digit:]]+\\)" " \\1⁄\\2" nil begin end)
       (query-replace-regexp "(\\([[:digit:]]+\\)/\\([[:digit:]]+\\)" "(\\1⁄\\2" nil begin end)
       (query-replace-regexp " \\([[:digit:]][[:digit:]]?\\)/\\([[:digit:]][[:digit:]]?\\)" " \\1⁄\\2" nil begin end)
       (query-replace-regexp "\\$\\\\frac{\\([[:digit:]]\\)}{\\([[:digit:]]\\)}\\$" "\\1⁄\\2" nil begin end)
       (query-replace-regexp "\\$\\\\frac{\\([[:digit:]]+\\)}{\\([[:digit:]]+\\)}\\$" "\\1/\\2" nil begin end)
       (query-replace-regexp "\\([[:punct:]]\\)n=" "\\1_n_ = " nil begin end)
       ; use MULTIPLICATION SIGN in multiplier expressions like "10x larger" (MULTIPLICATION X is too large):
       (query-replace-regexp "\\([[:digit:]]+\\)x" "\\1×" nil begin end)
       (query-replace-regexp "\\([[:digit:]]+\\)-times" "\\1×" nil begin end)
       (query-replace-regexp "\\([[:digit:]]+\\) times" "\\1×" nil begin end)
       (query-replace-regexp "\\([[:digit:]]+\\) x \\([[:digit:]]+\\)" "\\1×\\2" nil begin end)
       ; comma formatting:
       (query-replace-regexp "\\([[:digit:]]+\\)\\([[:digit:]][[:digit:]][[:digit:]]\\)\\([[:digit:]][[:digit:]][[:digit:]]\\)\\([[:digit:]][[:digit:]][[:digit:]]\\)" "\\1,\\2,\\3,\\4" nil begin end)
       (query-replace-regexp "\\([[:digit:]]+\\)\\([[:digit:]][[:digit:]][[:digit:]]\\)\\([[:digit:]][[:digit:]][[:digit:]]\\)" "\\1,\\2,\\3" nil begin end)
       (query-replace-regexp "\\([[:digit:]]+\\)\\([[:digit:]][[:digit:]][[:digit:]]\\)" "\\1,\\2" nil begin end)
       ; special currencies:
       (query-replace-regexp "\\([.0-9]*[[:space:]]\\)?btc" "₿\\1" nil begin end)
       ; fancy punctuation:
       (query-replace-regexp "\\([[:graph:]]\\) - +\\([[:graph:]]\\)" "\\1---\\2"     nil begin end) ; em dash for interjections
       (query-replace-regexp "\\([[:digit:]]+\\)-\\([[:digit:]]\\)"   "\\1--\\2"      nil begin end) ; en dash for ranges like "5-10"
       (query-replace-regexp "\\([[:digit:]]\\)\\.\\([[:digit:]]+\\)-\\([[:digit:]]\\)"   "\\1.\\2--\\3"      nil begin end) ; en dash for ranges like "1.5-10"
       (query-replace-regexp " \\([[:digit:]]%\\)-\\([[:digit:]]\\)"   " \\1--\\2"      nil begin end) ; en dash for percentage ranges: eg ' 10%-50%'
       (query-replace-regexp "(\\([[:digit:]]+%\\)-\\([[:digit:]]+\\)"   "(\\1--\\2"      nil begin end) ; en dash for percentage ranges: eg '10%-50%'
       (query-replace-regexp " \\([[:digit:]]+\\) [-–—]+ \\([[:digit:]]+\\)"   " \\1--\\2"      nil begin end) ; en dash for numerical space-separated ranges: eg '1990 - 1995' (whether separated by en/em/hyphen)
       (query-replace-regexp "\\([a-zA-Z]\\)–\\([[a-zA-Z]]\\)"       "\\1-\\2"       nil begin end) ; remove mistakenly used en dashes
       (query-replace-regexp "\\([a-zA-Z]\\)–\\([a-zA-Z]\\)"               "\\1-\\2"       nil begin end) ; check for misplaced en-dashes…
       (query-replace-regexp "\\([a-zA-Z]\\)--\\([a-zA-Z]\\)"               "\\1-\\2"       nil begin end) ; …and the Markdown-escaped version as well [we don't check for em-dash because that's more likely to be correct when between letters, it's just en-dash-between-letters that's usually wrong)
       (query-replace-regexp "\\([ ,(\[]\\)-\\([[:digit:]]\\)"                   "\\1−\\2" nil begin end) ; minus sign instead of hyphen
       (query-replace-regexp " --\\([[:digit:]]\\)"   " −\\1"      nil begin end) ; replace mistaken en-dashes with minus signs
       (query-replace-regexp " -\\[\\$" " −[$" nil begin end) ; minus sign: replace "It saves -[$1]($2021)" → "It saves −[$1]($2021)"
       ; move units outside inflation-adjusted amounts (they keep slipping into annotations when excerpting or rewriting units to be consistent); Inflation.hs handles these but inelegantly & adds a lot of complexity vs a simple compact notation of suffixing. So '[$10k]($2023)' → '[$10]($2023)k', for k/m/b/t:
       (query-replace-regexp "\\[\\$\\([0-9.]+\\)\\([kmbt]\\)\\](\\$\\([12][0-9][0-9][0-9]\\))" "[$\\1]($\\3)\\2" nil begin end)
       (query-replace        "=-"                                                " = −" nil begin end)
       (query-replace-regexp "\\([[:digit:]]\\) \\([[:digit:]][[:digit:]][[:digit:]]\\)" "\\1&thinsp;\\2" nil begin end) ; improve formatting of European-style space numerals
       (query-replace-regexp "\\([[a-zA-Z]]\\) " "\\1 " nil begin end)
       (query-replace-regexp " \\([[a-zA-Z]]\\)" " \\1" nil begin end)
       (query-replace-regexp "\\([[:digit:]]\\) degrees?" "\\1°" nil begin end)

       ; (query-replace-regexp "\\([A-TV-Z]\.\\)\\([A-TV-Z]\.\\)\\([A-RT-Z]\.\\) \\([[:upper:]]\\)" "\\1 \\2 \\3 \\4" nil begin end) ; 'C.A.M. Stirling' -> 'C. A. M. Stirling'
       ; (query-replace-regexp "\\([A-TV-Z]\.\\)\\([A-RT-Z]\.\\) \\([[:upper:]]\\)" "\\1 \\2 \\3" nil begin end) ; 'J.K. Rowling' → 'J. K. Rowling'; but try to skip U.S. initialisms like 'U.S. Army' or 'U.S. Congress'
       (query-replace-regexp "\\([a-zA-Z]\\)- " "\\1" nil begin end)
       (query-replace-regexp "::+" ":" nil begin end)
       (query-replace-regexp "\\*\\*\\*\\*\\(.*\\)" "**_\\1_" nil begin end) ; 4 '****s' may be an error on either side of an intended '**foo**'
       (query-replace-regexp "\\(.*\\)\\*\\*\\*\\*" "**_\\1_" nil begin end) ; 4 '****s' may be an error on either side of an intended '**foo**'
       (query-replace-regexp "\\*\\*\\*\\(.*\\)\\*\\*\\*" "**_\\1_**" nil begin end) ; '***' is a bad way to write bold-italics in Markdown because it's unreliable to read/parse
       (replace-all "<Strong>" "**")
       (replace-all "</Strong>" "**")
       (query-replace-regexp "\\([a-zA-Z]+\\) et al\\.? (\\([[:digit:]]+[a-z]?\\))" "\\1 et al \\2" nil begin end) ; 'Heald et al. (2015a)' → 'Heald et al 2015a'
       (query-replace-regexp "\\([a-zA-Z]+\\) et al\\. \\([[:digit:]]+\\)" "\\1 et al \\2" nil begin end)
       (query-replace-regexp "\\([A-Z][a-zñü]+\\) (\\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]\\))" "\\1 \\2" nil begin end) ; 'Darwin (1875)' → 'Darwin 1875'
       (query-replace-regexp "\\([A-Z][a-zñü]+\\), \\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]\\)" "\\1 \\2" nil begin end) ; 'Fitts, 1954;' → 'Fitts 1954'
       (query-replace-regexp "\\([A-Z][a-zñü]+\\) and \\([A-Z][a-zñ]+\\) (\\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]\\))" "\\1 & \\2 \\3" nil begin end) ; 'Goriely and Neukirch (2006)' → 'Goriely & Neukirch 2006'
       (query-replace-regexp "\\([A-Z][a-zñü]+\\), [A-Z][a-zñü]+, [A-Z][a-zñü]+, [&and]+ [A-Z][a-zñü]+ \\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]\\)" "\\1 et al \\2" nil begin end) ; 'Rowe, Isnard, Gallenmüller, & Speck 2006' → 'Rowe et al 2006' ; 4-fold
       (query-replace-regexp "\\([A-Z][a-zñü]+\\), [A-Z][a-zñü]+, [&and]+ [A-Z][a-zñü]+ \\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]\\)" "\\1 et al \\2" nil begin end) ; 3-fold
       (query-replace-regexp "\\([A-Z][a-zñü]+\\), [A-Z][a-zñü]+, [A-Z][a-zñü]+, [A-Z][a-zñü]+, [&and]+ [A-Z][a-zñü]+ \\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]\\)" "\\1 et al \\2" nil begin end) ; 5-fold
       (query-replace-regexp "\\([A-Z][a-zñü]+\\), [A-Z][a-zñü]+, [A-Z][a-zñü]+, [A-Z][a-zñü]+, [A-Z][a-zñü]+, [&and]+ [A-Z][a-zñü]+ \\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]\\)" "\\1 et al \\2" nil begin end) ; 6-fold
       (query-replace-regexp "\\([A-Z][a-zñü]+\\), [A-Z][a-zñü]+, [A-Z][a-zñü]+, [A-Z][a-zñü]+, [A-Z][a-zñü]+, [A-Z][a-zñü]+, [&and]+ [A-Z][a-zñü]+ \\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]\\)" "\\1 et al \\2" nil begin end) ; 7-fold
       (query-replace-regexp "\\([A-Z][a-z]+\\) and \\([A-Z][a-z]+\\),? \\([0-9]+\\)" "\\1 & \\2 \\3") ; eg 'Lofquist and Dawis 1991' → 'Lofquist & Dawis 1991'
       (query-replace-regexp "\\([A-Z][a-zñü]+\\) [&and]+ \\([A-Z][a-z]+\\), \\([[:digit:]][[:digit:]][[:digit:]][[:digit:]]\\)" "\\1 & \\2 \\3" nil begin end) ; '(Darwin & Darwin, 1880)' → '(Darwin & Darwin 1980)', or '(Darwin and Darwin, 1880)' → '(Darwin & Darwin 1980)'

       ; (query-replace-regexp "’\\([A-Za-qt-z]+\\)" "’ \\1" nil begin end) ; run-together apostrophes from PDFs
       (query-replace-regexp "^  \\([A-Za-z[:punct:]]+.*\\)" "    \\1" nil begin end) ; sometimes we get pseudo-indented text which I expect to get a code block but isn't enough.
       (query-replace-regexp "^\\([0-9]\\)) " "\\1. " nil begin end) ; convert single-parenthesis ordered lists to normal ordered list
       (query-replace-regexp "^ +<" " <" nil begin end) (query-replace-regexp "^ +</" " </" nil begin end) ; fix problems with leading whitespace causing HTML snippets to be treated as 4-space-indented literals

       (query-replace " <sup>" "<sup>" nil begin end)
       (query-replace "*8" "**" nil begin end)
       (query-replace "8*" "**" nil begin end)
       (query-replace "!{" "![" nil begin end)
       (query-replace-regexp " \"'\\(.+\\)', " " \"‘\\1’, " nil begin end) ; avoid downstream YAML errors from titles encoded in tooltips with single straight quotes
       (replace-all "\n\n\n" "\n\n")

       (message "%s %s" begin end)
       )
     (flyspell-buffer)
     (ispell) ; spellcheck
     (message "Getting suggested links…")
     (getLinkSuggestions "~/wiki/metadata/linkSuggestions.el")
     (message "Checking grammar/language…")
     (langtool-check)
     (call-interactively #'langtool-correct-buffer) ; grammar
     (check-parens)
     (message "Remember to collapse appendices, annotate links, add inflation-adjustments to all '$'/'₿'s, add margin notes, 'invert' images, and run `markdown-lint`")
     nil
     )))

; GPT-4
(require 'rx)
(defgroup markdown-newline-removal nil
  "Options for removing newlines within paragraphs in Markdown text."
  :group 'markdown)
(defcustom markdown-excluded-chars (rx (any ?- ?\n ?\d ?# ?* ?>))
  "Characters to exclude when removing newlines within paragraphs in Markdown text."
  :type 'regexp
  :group 'markdown-newline-removal)
(defun markdown-remove-newlines-in-paragraphs (&optional buffer use-region)
  "Replace newlines with spaces within paragraphs of Markdown text in BUFFER.
If BUFFER is nil, use the current buffer. If USE-REGION is non-nil,
operate on the current region instead of the entire buffer.
This assumes you have already removed hyphenation (either by removing the hyphen+newline for linebreaking-only hyphens, or just the newline when the hyphen is in the original word regardless of linebreaking)."
  (interactive "P")
  (with-current-buffer (or buffer (current-buffer))
    ;; Save the current point position and restore it after the operation
    (save-excursion
      ;; Determine the search range based on the use-region argument
      (let ((start (if use-region (region-beginning) (point-min)))
            (end (if use-region (region-end) (point-max))))
        ;; Move the point to the start of the search range
        (goto-char start)
        ;; Define the regex pattern using the excluded characters custom variable
        (let* ((excluded-chars (replace-regexp-in-string "^\\[\\|\\]$" "" markdown-excluded-chars))
               (pattern (concat "\\([^" excluded-chars "]\\)\\(\n\\)\\([^" excluded-chars "]\\)")))
          ;; Search and replace newlines within paragraphs
          (save-match-data
            (while (re-search-forward pattern end t)
              (replace-match "\\1 \\3" nil nil))))))
    ;; Inform the user when the operation is complete
    (message "Newlines removed within paragraphs.")))

(defvar markdown-rewrites '())
(defun buffer-contains-substring (string)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))
(defun getLinkSuggestions (rewriteFile)
  "Query the user for a long list of possible search-and-replaces in a buffer. The list is defined in a file argument REWRITEFILE and generated by an external utility parsing the gwern.net sources.
This works by: read a Lisp file with the 'rewrites' variable; 'rewrites' is a list of string pairs. Each string is a possible anchor text like \"Barack Obama\" and its corresponding URL, \"https://en.wikipedia.org/wiki/Barack_Obama\". Looping over this list of pairs, for each pair, do a 'query-replace' query on the buffer, converting it into a Markdown lint like '[Barack Obama](https://en.wikipedia.org/wiki/Barack_Obama)'.
This external file can be written by hand, but it is intended to be automatically generated by a Haskell tool which parses the full Gwern.net HTML+Markdown sources for all possible links, and turns the full set of links into a list of anchor text/link pairs, which it dumps in Elisp format into the file.
This tool is run automatically by a cron job. So any link on Gwern.net will automatically become a search-and-replace query, and it will be updated based on any manually-added links."
        (progn
          (load-file rewriteFile)

          (let ((begin (if (region-active-p) (region-beginning) (point-min)))
                (end (if (region-active-p) (region-end) (point-max)))
                )
            (save-excursion
              (goto-char (point-min))

              (dolist (pair markdown-rewrites)
                (let ((original (first pair))
                      (replacement (second pair))
                      )
                  ; skip if already done
                  (if (not (buffer-contains-substring replacement))
                      (let ((case-fold-search t) (search-upper-case t) (case-replace nil))
                        (query-replace-once original (concat "[" original "](" replacement ")") t begin end)
                        ))))))))

(defun markdown-annotation-compile ()
  "Turn a Markdown buffer into a HTML5 snippet without newlines and with escaped quotes, suitable for using as a YAML string inside annotated gwern.net links (see full.yaml)."
  (interactive)
  (call-interactively #'fmt)
  (save-window-excursion

    (defvar $pos 1)
    (message "Preprocessing and compiling into HTML…")
    ; Pandoc converts the Markdown to HTML. Then the HTML goes through `preprocess-markdown` which runs additional typographic/formatting rewrites, runs LinkAuto to automatically linkify text, and then runs through GenerateSimilar to provide a list of relevant annotations to curate as the 'see-also' section at the bottom of annotations (if they are approved).
    ; NOTE: because `preprocess-markdown` is calling the OA API via the embedder, $OPENAI_API_KEY must be defined in the Emacs environment, either via `(setenv "OPENAI_API_KEY" "sk-xyz123456789")` or by putting it in `~/.bash_profile`. (Putting it in `.env` or `.bashrc` is not enough, because they won't apply to GUI/X Emacs)
    (let ((markdown-command "cd ~/wiki/ && timeout 2m ~/wiki/static/build/preprocess-markdown | \\
                             pandoc --mathjax --metadata title='Annotation preview' --to=html5 --from=html | \\
                             tidy -indent -wrap 130 --clean yes --merge-divs no --break-before-br yes --logical-emphasis yes --quote-nbsp no -quiet --show-warnings no --show-body-only auto | \\
                             sed 's/^/    /' || true") (visible-bell nil))
      (markdown-kill-ring-save)

      (setq $pos (point-max))
      (goto-char $pos)
      (insert "\n---------------\n")
      (yank)
      (goto-char $pos)
      (html-mode)

      ; (replace-all "\n" " ")
      (let ((begin (if (region-active-p) (region-beginning) (+ $pos 1)))
            (end (if (region-active-p) (region-end) (point-max)))
            )
      (replace-all "<p> " "<p>")
      (replace-all " </p>" "</p>")
      ; (replace-all "</p> <p>" "</p>\n<p>")
      ; (replace-all "</p><p>" "</p>\n<p>")
      (replace-all " id=\"cb1\">" "") ; the Pandoc syntax-highlighting IDs cause ID clashes when substituted into pages, so delete all
      (replace-all " id=\"cb2\">" "")
      (replace-all " id=\"cb3\">" "")
      (replace-all " id=\"cb4\">" "")
      (replace-all "<p><img " "<figure><img ")
      ; (replace-all "\" /></p>" "\" /></figure>")
      (replace-all "’’" "’")
      (replace-all "</a>’s" "’s</a>")
      (replace-all "%3Csup%3Est%3C/sup%3E" "th")
      (replace-all "%3Csup%3End%3C/sup%3E" "nd")
      (replace-all "%3Csup%3Erd%3C/sup%3E" "rd")
      (replace-all "<!-- -->" "")
      ; unescaped single quotation marks will often break the YAML, so they need to either be replaced with the intended Unicode, or double-quoted to 'escape' them
      (query-replace "'" "''" nil begin end)
      (delete-trailing-whitespace)
      (forward-line)
      (ding)
      (message "Done.")
      )
      )
    )
  )
(add-hook 'markdown-mode-hook
          (lambda ()
            (define-key markdown-mode-map "\C-c\ w" 'markdown-annotation-compile)))
(defvar html-mode-map) ; suppress reference-to-free-variable byte-compile warning
(add-hook 'html-mode-hook
          (lambda ()
            (define-key html-mode-map "\C-c\ w" 'markdown-annotation-compile)))

; for the `foo` buffer I do most of my annotation work in, on the first copy-paste of a block of text, detect if it has any paragraph breaks (ie. double newlines), and if it does not, then automatically run paragraphizer.py on it to try to break it up into logical paragraphs.
; (Note/warning: written by GPT-3.5. Curiously, GPT-4 failed when I tried to repeat this exercise in it using the same starting prompt & kind of feedback: because it tries to implement solutions using advice, buffer-local variables, and :properties—which are subtly buggy in their handling of state, and so wind up running paragraphizer.py on every paste.)
(defun markdown-paragraphize ()
  "Automatically paragraphize single-paragraph abstracts. Intended for Markdown mode with double-newlines for newlines; may malfunction if run on other formats like HTML (where `</p><p>` pairs can come in many forms, not to mention other block elements like blockquotes)."
  (interactive)
  (let ((double-newline-found nil))
          (save-excursion
        (goto-char (point-min))
        (unless (search-forward-regexp "\n\n" nil t)
          (message "Paragraphizing abstract…")
          (shell-command-on-region (point-min) (point-max) "~/wiki/static/build/paragraphizer.py" nil t)
          (setq double-newline-found t)))
    (when double-newline-found
      (goto-char (point-max))
      (message "Paragraphizing abstract done."))))
(defun markdown-paragraphize-hook ()
  "Hook function for `markdown-paragraphize`."
  (when (and (equal (buffer-name) "foo")
             (derived-mode-p 'markdown-mode)
             (eq this-command 'yank)
             (>= (buffer-size) 600)) ; ensure that there is enough in the buffer to plausibly be a full copy-pasted abstract, as opposed to a random snippet or line.
    (markdown-paragraphize)))
(add-hook 'post-command-hook #'markdown-paragraphize-hook)

; add new-line / paragraph snippet
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map (kbd "<C-return>")  (lambda () (interactive)
                                                            (if (= ?\s (preceding-char)) (delete-char -1))
                                                            (insert "</p> <p>")
                                                            (if (= ?\s (following-char)) (delete-char 1)))
            )
          ))

(add-hook 'markdown-mode-hook   'visual-fill-column-mode)

;; Markup editing shortcuts for HTML/Markdown/YAML annotation editing.
;; Functions to easily add italics, bold, Wikipedia links, smallcaps, & margin-note syntax.
(defun surround-region-or-word (start-tag end-tag)
  "Surround selected region (or next word if no region) with START-TAG and END-TAG."
  (interactive)
  (let ((begin (if (region-active-p)
                   (region-beginning)
                 (point)))
        (end (if (region-active-p)
                 (region-end)
               (progn
                 (forward-word)
                 (point)))))
    (goto-char end)
    (insert end-tag)
    (goto-char begin)
    (insert start-tag)
    (goto-char (+ end (length start-tag) (length end-tag)))))
;; the wrappers:
(defun html-insert-emphasis ()
  "Surround selected region (or word) with HTML <em> tags for italics/emphasis (also valid in Markdown, which supports `*FOO*`)."
  (interactive)
  (surround-region-or-word "<em>" "</em>"))
(defun markdown-insert-emphasis ()
  "Surround selected region (or word) with Markdown asterisks for italics/emphasis.
Equivalent to `<em>FOO</em>` in HTML.
Gwern.net uses `*` for emphasis, and generally reserves `_` for italics such as book titles (in keeping with Internet conventions
predating Gruber's Markdown mistake of conflating `*`/`_`)."
  (interactive)
  (surround-region-or-word "*" "*"))
(defun html-insert-strong ()
  "Surround selected region (or word) with <strong> bold tags (HTML, equivalent to `**` in Markdown).
Used in abstracts for topics, first-level list emphasis, etc."
  (interactive)
  (surround-region-or-word "<strong>" "</strong>"))
(defun markdown-insert-strong ()
  "Surround selected region (or word) with `**` bold tags (Markdown).
Equivalent to `<strong>FOO</strong>` in HTML.
Used in abstracts for topics, first-level list emphasis, etc."
  (interactive)
  (surround-region-or-word "**" "**"))
(defun html-insert-smallcaps ()
  "Surround selected region (or word) with smallcaps syntax.
Built-in CSS class in HTML & Pandoc Markdown, span syntax is equivalent to `[FOO]{.smallcaps}`.
Smallcaps are used on Gwern.net for second-level emphasis after bold has been used."
  (interactive)
  (surround-region-or-word "<span class=\"smallcaps\">" "</span>"))
(defun markdown-insert-smallcaps ()
  "Surround selected region (or word) with smallcaps syntax (Pandoc Markdown syntax).
Built-in CSS class in HTML & Pandoc Markdown, equivalent to `<span class=\"smallcaps\">FOO</span>`.
Smallcaps are used on Gwern.net for second-level emphasis after bold has been used."
  (interactive)
  (surround-region-or-word "[" "]{.smallcaps}"))
(defun html-insert-wp-link ()
  "Surround selected region (or word) with custom Wikipedia link syntax in HTML.
Compiled by Interwiki.hs to the equivalent (usually) of `<a href=\"https://en.wikipedia.org/wiki/FOO\">FOO</a>`."
  (interactive)
  (surround-region-or-word "<a href=\"!W\">" "</a>"))
(defun markdown-insert-wp-link ()
  "Surround selected region (or word) with custom Wikipedia link syntax in Markdown."
  (interactive)
  (surround-region-or-word "[" "](!W)"))
(defun markdown-insert-margin-note ()
  "Surround selected region FOO BAR (or word FOO) with a 'margin note': `^[!Margin: FOO BAR]`.
This creates marginal glosses (in the left-margin) using custom overloaded footnote syntax.
These margin-notes are used as very abbreviated italicized summaries of the following paragraph
(like very small inlined section headers).
NOTE: no HTML version: margin notes are not supported in annotations because no footnote support.
(Maybe they could be, just always inline+italicized, similar to narrow windows?)"
  (interactive)
  (surround-region-or-word "^[!Margin: " "]"))
;; keybindings:
;;; Markdown:
(add-hook 'markdown-mode-hook (lambda()(define-key markdown-mode-map "\C-c\ \C-e" 'markdown-insert-emphasis)))
(add-hook 'markdown-mode-hook (lambda()(define-key markdown-mode-map "\C-c\ \C-s" 'markdown-insert-strong)))
(add-hook 'markdown-mode-hook (lambda()(define-key markdown-mode-map "\C-c\ s"    'markdown-insert-smallcaps)))
(add-hook 'markdown-mode-hook (lambda()(define-key markdown-mode-map "\C-c\ \C-w" 'markdown-insert-wp-link)))
(add-hook 'markdown-mode-hook (lambda()(define-key markdown-mode-map "\C-c\ \C-m" 'markdown-insert-margin-note)))
;;; HTML:
(add-hook 'html-mode-hook (lambda()(define-key html-mode-map "\C-c\ \C-e" 'html-insert-emphasis)))
(add-hook 'html-mode-hook (lambda()(define-key html-mode-map "\C-c\ \C-s" 'html-insert-strong)))
(add-hook 'html-mode-hook (lambda()(define-key html-mode-map "\C-c\ s"    'html-insert-smallcaps)))
(add-hook 'html-mode-hook (lambda()(define-key html-mode-map "\C-c\ \C-w" 'html-insert-wp-link)))
;;; YAML: (the YAML files store raw HTML snippets, so insert HTML rather than Markdown markup)
(add-hook 'yaml-mode-hook (lambda()(define-key yaml-mode-map "\C-c\ \C-e" 'html-insert-emphasis)))
(add-hook 'yaml-mode-hook (lambda()(define-key yaml-mode-map "\C-c\ \C-s" 'html-insert-strong)))
(add-hook 'yaml-mode-hook (lambda()(define-key yaml-mode-map "\C-c\ s"    'html-insert-smallcaps)))
(add-hook 'yaml-mode-hook (lambda()(define-key yaml-mode-map "\C-c\ \C-w" 'html-insert-wp-link)))

;sp
(add-hook 'markdown-mode-hook 'flyspell)
;for toggling visibility of sections - makes big pages easier to work with
(add-hook 'markdown-mode-hook 'outline-minor-mode)
;In Markdown files, there are few excuses for unbalanced delimiters, and unbalance almost always indicates a link syntax error; in cases where quoted text must contain unbalanced delimiters (eg diffs, or neural-net-generated text or redirects fixing typos), a matching delimiter can be added in a comment like '<!-- () [] -->' to make it add up.
(defun balance-parens () (when buffer-file-name
                           (add-hook 'after-save-hook
                                     'check-parens
                                     nil t)))
(add-hook 'markdown-mode-hook   'balance-parens)
(add-hook 'ledger-mode-hook     'balance-parens)
(add-hook 'emacs-lisp-mode-hook 'balance-parens)
(add-hook 'haskell-mode-hook    'balance-parens)
(add-hook 'css-mode-hook        'balance-parens)
(add-hook 'javascript-mode-hook 'balance-parens)
(add-hook 'html-mode-hook       'balance-parens)
(add-hook 'python-mode-hook     'balance-parens)
; NOTE: I skip YAML mode because syntax-level quoting is kept validated by the database processing, and within-annotation balancing is checked in Hakyll, and using `check-parens` in YAML mode triggers far too many spurious errors.

; ispell: ignore code blocks in Pandoc Markdown
; TODO: add a fix for '#' not being handled in URLs. current hack borrowed from <https://github.com/jrblevin/markdown-mode/issues/420> (also a good example of maintainers being lazy)
(add-to-list 'ispell-skip-region-alist
             '("^~~~" . "^~~~"))
(add-to-list 'ispell-skip-region-alist '("#[a-zA-Z]+" forward-word))

;flycheck mode: meant for code errors, but useful for prose linting & Markdown syntax checking as well
;Supported languages: <https://www.flycheck.org/en/latest/languages.html#flycheck-languages>
;Currently used CLI: 'proselint'/'mdl' for text Markdown writing; 'hlint'/'ghc' for Haskell; 'tidy' for HTML, 'flake8' (`apt-get install python-flake8`) for Python; 'jshint' (`npm install --prefix ~/src/ jshint`, configured in ~/.jshintrc) for JavaScript
; NOTE: aside from the 'flycheck' package (from MELPA), you also need a CLI tool to do the actual checking, either <https://github.com/markdownlint/markdownlint/> (`mdl`, Ruby) or <https://github.com/DavidAnson/markdownlint> + <https://github.com/igorshubovych/markdownlint-cli> (`markdown-lint-cli`, a Node.js clone/fork)
; (load "~/src/flycheck/flycheck.el") ; the MELPA package is out of date and does not have either Markdown or proselint support as of 18 Nov 2019, so we have to load from the Github repo
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
; syntax checkers must be whitelisted/enabled individually, so turn on proselint & mdl
(add-to-list 'flycheck-checkers 'proselint) ; configured in ~/.proselintrc
(add-to-list 'flycheck-checkers 'markdown-mdl) ; configured in ~/.mdlrc ; list & explanation of rules: <https://github.com/markdownlint/markdownlint/blob/master/docs/RULES.md>

; 'langtool': a Java tool for grammar checking: <https://github.com/mhayashi1120/Emacs-langtool> <https://languagetool.org/dev>
(require 'langtool)
(setq langtool-language-tool-jar "~/bin/bin/LanguageTool-4.7/languagetool-commandline.jar")
(setq langtool-default-language "en-US")
; <http://wiki.languagetool.org/command-line-options> <https://community.languagetool.org/rule/list> ; can look up in there or run a command like
; $ java -jar languagetool-commandline.jar -b --line-by-line --language en-US  --disable "EN_QUOTES,MULTIPLICATION_SIGN,WORD_CONTAINS_UNDERSCORE,ET_AL,SENTENCE_WHITESPACE,DASH_RULE,MORFOLOGIK_RULE_EN_US,EN_UNPAIRED_BRACKETS,WHITESPACE_RULE" ~/wiki/research-criticism.page
; to disable specific rules
(setq langtool-user-arguments '("-b" "--line-by-line" "--disable" "EN_QUOTES,MULTIPLICATION_SIGN,WORD_CONTAINS_UNDERSCORE,ET_AL,SENTENCE_WHITESPACE,DASH_RULE,MORFOLOGIK_RULE_EN_US,EN_UNPAIRED_BRACKETS,WHITESPACE_RULE,UNIT_SPACE,TR"))

; mismatched quotes are no good either
; <http://stackoverflow.com/questions/9527593/customizing-check-parens-to-check-double-quotes>
(add-hook 'markdown-mode-hook (lambda () (modify-syntax-entry ?\" "$" markdown-mode-syntax-table)))
(add-hook 'yaml-mode-hook (lambda () (modify-syntax-entry ?{ "(" markdown-mode-syntax-table) (modify-syntax-entry ?} ")" markdown-mode-syntax-table)))
(add-hook 'yaml-mode-hook (lambda () (modify-syntax-entry ?\( "(" markdown-mode-syntax-table) (modify-syntax-entry ?\) ")" markdown-mode-syntax-table)))

; We visually highlight '\[' in Markdown files to emphasize that they are part of editorial insertions (like '[sic]') and *not* the ubiquitous Markdown link syntax. Confusing them can cause problems.
(global-prettify-symbols-mode 1)
(add-hook 'markdown-mode-hook (lambda ()
   (mapc (lambda (pair) (push pair prettify-symbols-alist))
         '(
           ("\\[" . ?〖)
           ("\\]" . ?〗)
           ("–" . ?ˉ) ; highlight en-dashes because they look so much like regular hyphens
           ("—" . ?⸺) ; em-dashes
           ("−" . ?━) ; MINUS SIGN
           ))))
(add-hook 'yaml-mode-hook (lambda ()
   (mapc (lambda (pair) (push pair prettify-symbols-alist))
         '(
           ("\\[" . ?〖)
           ("\\]" . ?〗)
           ("–" . ?ˉ) ; highlight EN DASH because they look so much like regular hyphens
           ("—" . ?⸺) ; EM DASH
           ("−" . ?━) ; MINUS SIGN
           ))))
