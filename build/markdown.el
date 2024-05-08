;; -*- lexical-binding: t -*-
;;; markdown.el --- Emacs support for editing Gwern.net
;;; Copyright (C) 2009 by Gwern Branwen
;;; License: CC-0
;;; When:  Time-stamp: "2024-05-06 10:28:45 gwern"
;;; Words: GNU Emacs, Markdown, HTML, GTX, Gwern.net, typography
;;;
;;; Commentary:
;;; Helper files for editing Markdown, HTML, and HTML-in-GTX, particularly reformatting & editing annotations in the Gwern.net house style.
;;; Additional functions include error-checking and prettifying confusable characters like dashes.

; since I hardly ever write elisp, and often start writing things in the *scratch* buffer, save time by defaulting to Markdown.
(setq initial-major-mode 'markdown-mode)
(setq initial-scratch-message "")

(push '("\\.gtx$" . html-mode) auto-mode-alist)

; I do much of my editing in gwern.net files, so save myself some tab-completion hassle:
(setq default-directory "~/wiki/")

;;we rely on the Github dev version because the 2017 v2.3 stable release packaged everywhere is missing a bugfix (stable breaks on any Markdown file with HTML comments in it); NOTE: still seems to be true on Ubuntu `elpa-markdown-mode` 2.3+210-1 as of 2023-02-11!
(add-to-list 'load-path "~/src/markdown-mode/")
(require 'markdown-mode)

;; ; Metadata files are stored in YAML; but yaml-mode may be too slow to use given how large they have become...
;; (require 'yaml-mode)
;; (defun my/yaml-mode-decision ()
;;   "Activate yaml-mode with conditionally disabled Flycheck."
;;   (let (
;;         (disable-flycheck (or (string-prefix-p (expand-file-name "~/wiki/metadata/") (buffer-file-name))
;;                               (> (nth 7 (file-attributes (buffer-file-name))) 1000000))))
;;     ;; Disable Flycheck if the file is large or in a specific directory
;;     (when disable-flycheck
;;       (flycheck-mode -1))

;;     ;; Activate yaml-mode
;;     (yaml-mode)

;;     ;; Additional settings if Flycheck is disabled
;;     (when disable-flycheck
;;       (font-lock-mode -1)
;;       (message "Custom YAML mode settings applied for large file/specific directory"))))
;; (add-hook 'yaml-mode-hook 'my/yaml-mode-decision)
;; ; (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

; (setq major-mode 'markdown-mode) ; needs to be done via 'Customize'?
(setq markdown-command
   "pandoc --mathjax --metadata title='Markdown-preview' --to=html5 --standalone --number-sections --toc --reference-links --css=https://gwern.net/static/css/default.css -f markdown+smart --template=/home/gwern/wiki/static/template/pandoc/template-html5-articleedit.html5 -V lang=en-us")
(setq markdown-enable-math t)
(setq markdown-italic-underscore t)

; warn on dangerous use of statistical-significance testing language:
(add-hook 'markdown-mode-hook
            (lambda ()
              (font-lock-add-keywords nil '(
                  (" significant" 0 'taylor-special-words-warning t)
                  (" significance" 0 'taylor-special-words-warning t)
              ))
              (setq show-trailing-whitespace t)
              ))
(add-hook 'html-mode-hook
            (lambda ()
              (font-lock-add-keywords nil '(
                  (" significant" 0 'taylor-special-words-warning t)
                  (" significance" 0 'taylor-special-words-warning t)
              ))))
;; (add-hook 'yaml-mode-hook
;;             (lambda ()
;;               (font-lock-add-keywords nil '(
;;                   (" significant" 0 'taylor-special-words-warning t)
;;                   (" significance" 0 'taylor-special-words-warning t)
;;               ))))

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
  "Regexp search-and-replace all instances of ORIGINAL to REPLACEMENT: define a local equivalent of `replace-string'
which won't throw annoying errors about only interactive use: exact string replacement (case-sensitive)."
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

; Easy Unicode insertion mnemonics; uses the unusual X modifier key 'Super'.
; This is not bound by default to a key usually, but on my 102-key US layout, I rebind the useless 'Menu' key to it: `$ modmap -e 'keysym Menu = Super_R'`.
; Then 's-' in `kbd` notation is 'Super-'. (I avoid use of 'Compose' key because I find the shortcuts highly unintuitive: <https://en.wikipedia.org/wiki/Compose_key#Common_compose_combinations>.)
; TODO: for some reason this collides with XMonad keybindings on the Win key, despite that being assigned to 'Super'/mod4Mask and so in theory not being an issue with these Super keys?
(defun super-insert (key char)
  "Bind Super (H-) plus KEY to insert CHAR."
  (global-set-key (kbd (concat "s-" key)) (lambda () (interactive) (insert char)))  )
(super-insert "\"" "‘") ; eg equivalent to `(global-set-key (kbd "s-'") (lambda () (interactive) (insert "‘")))
(super-insert "'" "’")
(super-insert ";" "“")
(super-insert ":" "”")
(super-insert "-" "—") ; EM DASH
(super-insert "_" "–") ; EN DASH
(super-insert "=" "−") ; MINUS SIGN
(super-insert "x" "×") ; MULTIPLICATION SIGN
(super-insert "," "≤") ; LESS-THAN OR EQUAL TO
(super-insert "." "≥") ; GREATER-THAN OR EQUAL TO
(super-insert "/" "⁄") ; FRACTION SLASH
(super-insert "o" "𝒪") ; MATHEMATICAL SCRIPT CAPITAL O

; Abbreviation mode: text shortcuts for common terms, or characters which are hard to input:
(setq-default abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
    ("microgram" "µg" nil 0)
    ("Section" "§" nil 0) ; 'SECTION SIGN'
    ("bc" "because" nil 0)
    ("moda" "modafinil" nil 0)
    ("ss" "statistically-significant" nil 0)
    ("psi" "ψ" nil 0)
    ("arrowright" "→" nil 0)
    ("arrowleft" "←" nil 0)
    ("arrowboth" "↔" nil 0)
    ("arrowup" "↑" nil 0)
    ("arrowdown" "↓" nil 0)
    ("sarcasm?" "⸮" nil 0)
    ("asterisk" "✱" nil 0) ; 'HEAVY AST​ERISK' to avoid confusion with Markdown
    ("notequalto" "≠" nil 0)
    ("bitcoin" "₿" nil 0)
    ("delta" "Δ" nil 0)
    ("lessthanorequalto" "≤" nil 0)
    ("greaterthanorequalto" "≥" nil 0)
    ("poundsterling" "£" nil 0)
    ("poundssterling" "£" nil 0)
    ("poundsign" "£" nil 0)
    ("yen" "¥" nil 0)
    ("euro" "€" nil 0)
    ("degrees" "°" nil 0)
    ("celsius" "℃" nil 0)
    ("degreecelsius" "℃" nil 0)
    ("degreescelsius" "℃" nil 0)
    ("pi" "π" nil 0)
    ("bigo" "𝒪" nil 0)
    ("epsilon" "ε" nil 0)
    ("sigma" "σ" nil 0)
    ))

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

(defun fmt ()
  "Update gwern.net Markdown files & annotations with latest conventions.
Mostly string search-and-replace to enforce house style in terms of format."
  (interactive
   (let ((begin (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max)))
         )
     (save-excursion
       (goto-char (point-min))
       (de-unicode)
       (de-unicode)
       (flyspell-buffer)
       (delete-trailing-whitespace)
       (check-parens)

       (let ; Blind unconditional rewrites:
           ((blind '(("﻿" . "") ; byte order mark?
                     (" " . " ")
                     ("" . "fi")
                     ("" . "fl")
                     ("\\\u2013" . "--")
                     ("â" . "")
                     ("â" . "“")
                     ("ì" . "“")
                     ("â" . "”")
                     ("î" . "”")
                     ("í" . "’")
                     ("â" . "–")
                     ("â" . "−")
                     ("\n    \\\\u2022\n" . "\n- ")
                     ("\\\\u2022\n\n    " . "- ")
                     ("    \\\\u2022\n\n    " . "- ")
                     ("\n\\\\u2022 " . "\n- ")
                     (" Âµg" . " µg")
                     ("Î¼g" . "µg")
                     ("\nâ¢\n" . "- ")
                     ("â¢ " . "- ")
                     ("<br><br>" . "\n\n")
                     ("Kendall's Ï" . "Kendall's <em>τ</em>")
                     ("\\\\u03bc" . "μ")
                     ("\\\\u2018" . "‘")
                     ("\\\\u2019" . "’")
                     ("\u2009" . " ")
                     ("\\\\u2013" . "–")
                     ("â\\" . "'")
                     ("â" . "'")
                     ("â\\" . "—")
                     ("â" . "−")
                     ("\\\\u2014" . "—")
                     ("\\\\u201c" . "“")
                     ("\\\\u201d" . "”")
                     ("\\\\u2009" . " ")
                     ("\\\\u2212" . "−")
                     ("\\\\u2192" . "→")
                     ("\\\\u221e" . "𝓁<sub>∞</sub>")
                     ("\\\\u03b5" . "𝜀")
                     ("\\\\u223c" . "~")
                     ("\\\\u2217" . "✱")
                     ("\\\\u2020" . "†")
                     ("\\\\u2021" . "‡")
                     ("\\\\u2194" . "↔")
                     ("\\\\u2248 " . "~")
                     ("\\\\u03b1" . "α")
                     ("\\\\u03b8i" . "θ<sub><em>i</em></sub>")
                     ("\\\\u2265" . "≥")
                     ("\\\\u03b8" . "θ")
                     (" \\\\u2022 " . ", ")
                     ("\\\\u2022" . "·")
                     ("\\\\u2264" . "≤")
                     ("\\\\U0001d442" . "𝒪")
                     ("\\\\U0001d4412" . "_N_^2^")
                     ("\\\\u2208" . "∈")
                     ("\\\\U0001d45a" . "𝑚")
                     ("\\\\u2113" . "𝓁")
                     ("â¤" . "≤")
                     ("](wiki/" . "](/")
                     ("](//doc" . "](/doc")
                     ("]]http" . "](https")
                     ("]]/" . "](/")
                     (" \\[\" . " " [\"")
                     (" \"](" . "\"](")
                     ("" . "=")
                     ("  " . ", ")
                     ("T h i s" . "This")
                     ("T h e" . "The")
                     ("Author links open overlay panel" . "")
                     ("et al.," . "et al")
                     ("\n---\n" . "\n<hr />\n")
                     ("" . " = ")
                     ("" . " < ")
                     ("\n " . "\n")
                     (" = " . " = ")
                     ("  =" . " =")
                     ("=  " . "= ")
                     ("‐" . "-")
                     ("­\n" . "")
                     ("­" . "")
                     ("–" . "--")
                     ("—" . "---")
                     (" ‑\n" . "")
                     ("‑\n" . "")
                     ("‑" . "-") ; deal with NON-BREAKING HYPHEN which NEJM uses for both line-breaking and regular hyphens, /sigh
                     ("¬ " . "")
                     ("<b>" . "**")
                     ("</b>" . "**")
                     ("<i>" . "<em>")
                     ("</i>" . "</em>")
                     ("=  " . "= ")
                     ("∼" . "~")
                     ("Previous article in issue\nNext article in issue\nKeywords\n" . "[**Keywords**: ")
                     ("Previous article in issue\nKeywords\n" . "[**Keywords**: ")
                     ("•\n\n    " . "- ")
                     ("     ● " . "- ")
                     ("eta≠analys" . "eta-analys") ; odd typo in some PDFs: "meta≠analyses"
                     ("\n•\n" . "- ")
                     ("    •\n    " . "- ")
                     ("<p> " . "<p>")
                     (" </p>" . "</p>")
                     ("View ORCID Profile" . "")
                     (" gf " . " _g~f~_ ")
                     (" gf." . " _g~f~_.")
                     ("(gf)" . "(_g~f~_)")
                     ("_gf_" . "_g~f~_")
                     (" gF" . " _g~f~_")
                     ("Gq" . "_G~q~_")
                     ("Gc" . "_G~c~_")
                     ("Gf" . "_G~f~_")
                     ("5-HT2A" . "5-HT~2A~")
                     ("ﬁ" . "fi")
                     ("" . "ff")
                     ("ﬀ" . "ff")
                     ("ﬃ" . "ffi")
                     ("ﬂ" . "fl")
                     ("￿" . "fi")
                     ("Æ" . "fi")
                     ("𝑥" . "_x_")
                     ("𝑦" . "_y_")
                     (" opens in new tab." . "")
                     ("Author links open overlay" . "")
                     ("Get rights and content" . "")
                     ("https://doi.org/" . "DOI: ")
                     ("e\\. g\\." . "eg")
                     ("₁" . "<sub>1</sub>")
                     ("₂" . "<sub>2</sub>")
                     ("₃" . "<sub>3</sub>")
                     ("₄" . "<sub>4</sub>")
                     ("¹" . "<sup>1</sup>")
                     ("²" . "<sup>2</sup>")
                     ("³" . "<sup>3</sup>")
                     ("⁴" . "<sup>4</sup>")
                     ("*†" . "")
                     ("†," . ",")
                     ("‡" . "")
                     (",," . ",")
                     (",”" . "”,")
                     ("‘‘" . "\"")
                     ("’’" . "\"")
                     (" . " " ") ; replace THIN SPACE
                     ("](!)" . "](!W)")
                     ("˜" . "~")
                     ("randomis" . "randomiz")
                     ("mendelian random" . "Mendelian random")
                     ("behaviour" . "behavior")
                     (" utilise" . "use")
                     (" utilize" . "use")
                     (" utilising" . " using")
                     (" utilizing" . " using")
                     (" utilisation" . " usage")
                     (" utilization" . " usage")
                     ("\n• " . "\n- ")
                     (" colour" . " color")
                     ("](/docs/" . "](/doc/")
                     ("href=\"/docs/" . "href=\"/doc/")
                     ("href='/docs/" . "href='/doc/")
                     ("]/doc" . "](/doc")
                     (" n = " . " _n_ = ")
                     ("(n = " . "(_n_ = ")
                     ("(N=" . "(_n_ = ")
                     ("(n=" . "(_n_ = ")
                     ("(_n_=" . "(_n_ = ")
                     ("|rg|" . "|<em>r<sub>g</sub></em>|")
                     ("| rg |" . "|<em>r<sub>g</sub></em>|")
                     ("| _r~g~_ |" . "|<em>r<sub>g</sub></em>|")
                     ("|ra|" . "|<em>r<sub>a</sub></em>|")
                     ("| ra |" . "|<em>r<sub>a</sub></em>|")
                     ("| _r~a~_ |" . "|<em>r<sub>a</sub></em>|")
                     (" n = " . " _n_ = ")
                     ("(n ≈" . "(_n_ ≈")
                     (" n ≈" . " _n_ ≈")
                     ("(n " . "(_n_ ")
                     ("(ie, " . "(ie ")
                     ("(ie " . "(ie ")
                     ("(i\\.e\\.," . "(ie")
                     ("(i\\.e\\." . "(ie")
                     ("(ie\\." . "(ie")
                     ("e\\.g\\., " . "eg ")
                     (" e\\.g\\." . " eg")
                     ("(e\\.g\\." . "(eg")
                     ("eg\\., " . "eg ")
                     ("(eg\\." . "(eg")
                     ("Na\\+" . "Na⁺")
                     ("K\\+" . "K⁺")
                     ("Ca2+" . "Ca<sup>2</sup>⁺")
                     ("<sup>+</sup>" . "⁺")
                     ("+-" . "±")
                     ("+/−" . "±")
                     ("et al.\n" . "et al")
                     (" ω 2" . " ω<sup>2</sup>")
                     ("τ2" . "τ<sup>2</sup>")
                     ("ω2" . "ω<sup>2</sup>")
                     ("chi-squared" . "<em>χ</em><sup>2</sup>")
                     ("X2(2)" . "<em>χ</em><sup>2</sup>")
                     ("F1-ATPase" . "F<sub>1</sub>-ATPase")
                     ("α3β3" . "α<sub>3</sub>β<sub>3</sub>")
                     (" Escherichia coli" . " <em>Escherichia coli</em>")
                     (" E\\. coli" . " <em>E. coli</em>")
                     (" Saccharomyces cerevisiae" . " <em>Saccharomyces cerevisiae</em>")
                     (" in vivo " . " _in vivo_ ")
                     (" ex vivo " . " _ex vivo_ ")
                     ("two-by-two" . "2×2")
                     (" B\\.M\\.I\\." . " BMI")
                     (" A\\.I\\." . " AI")
                     (" C\\.E\\.O\\." . " CEO")
                     ("controled" . "controlled")
                     ("one-fourth" . "1⁄4")
                     ("one-half" . "1⁄2")
                     ("One-fifth" . "1⁄5<sup>th</sup>")
                     ("one-sixth" . "1⁄6<sup>th</sup>")
                     ("three-quarters" . "3⁄4<sup>ths</sup>")
                     ("two-thirds" . "2⁄3<sup>rds</sup>")
                     ("2 thirds" . "2⁄3<sup>rds</sup>")
                     ("and/ or" . "and/or")
                     ("\\[\\]\\(\\!W\\)" . " ERROR ")
                     ("( <em>" . "(<em>")
                     ("<Sup>" . "<sup>")
                     ("</Sup>" . "</sup>")
                     ("<Sub>" . "<sub>")
                     ("</Sub>" . "</sub>")
                     ("]9/doc" . "](/doc")
                     ("on X (formerly Twitter)" . "on Twitter")
                     ("on X:" . "on Twitter:")
                     (" X (formerly known as Twitter)" . " Twitter")
                     ("Elon Musk’s X" . "Elon Musk’s Twitter")
                     (" target=\"_blank\"" . "")
                     ("\\.," . ". ")
                     ("**: : " . ": ")
                     (">: : " . ">: ")
                     )
                   )
            )
         (dolist (pair blind)
           (replace-all (car pair) (cdr pair)))
         )

       (let ((queries '( ; do non-regexp fixed-string query-replace queries:
                        (" ’" . " ‘")
                        ("]/" . "](/")
                        (" · " . ", ")
                        (",”" . "”,")
                        ("'''''" . "**_")
                        ("'''" . "**")
                        (" . " " ")
                        ("''" . "\"")
                        ("( " . "(")
                        ("| " . "|")
                        ("|-|" . "|–|") ; EN DASH: absolute value ranges
                        (" )" . ")")
                        ("?!" . "‽")
                        ("!?" . "‽")
                        (" ?" . "?")
                        (" — " . "—")
                        (" – " . "—")
                        (" − " . "—")
                        (" -> " . "→")
                        ("->"   . "→")
                        ("~>"   . "→")
                        (" percent " . "% ")
                        (" per cent " . "% ")
                        ("95 %CI" . "95% CI")
                        ("95 % confidence interval" . "95% confidence interval")
                        (" , " . ", ")
                        ("http://reddit.com"   . "https://www.reddit.com")
                        ("https://reddit.com"  . "https://www.reddit.com")
                        ("http://i.reddit.com" . "https://www.reddit.com")
                        ("https://mobile.twitter.com" . "https://twitter.com")
                        (" d=" . " _d_ = ") ; italicize p-values & sample sizes:
                        (" d = " . " _d_ = ")
                        ("(d = " . "(_d_ = ")
                        ("(d < " . "(_d_ < ")
                        ("Cohen’s d" . "Cohen’s _d_")
                        ("<em>P</em>=" . "_p_ = ")
                        ("pone-tailed" . "<em>p</em><sub><em>one-tailed</em></sub>")
                        ("ptrend" . "<em>p</em><sub>trend</sub>")
                        ("pmeta" . "<em>p</em><sub>meta</sub>")
                        ("pinteraction" . "<em>p</em><sub>interaction</sub>")
                        ("p0" . "<em>p</em><sub>0</sub>")
                        ("pgc" . "<em>p<sub>gc</sub></em>")
                        ("(N" . "(_n_ ")
                        ("(n, " . "(_n_, ")
                        (" N)" . " _n_)")
                        ("[n = " . "[_n_ = ")
                        ("[N=" . "[_n_ = ")
                        ("[n=" . "[_n_ = ")
                        ("[N" . "[_n_ ")
                        (" n=" . " _n_ = ")
                        (" N = " . " _n_ = ")
                        (" N=" . " _n_ = ")
                        (" n =" . " _n_ = ")
                        (" n " . " _n_ ")
                        (" _n_=" . " _n_ = ")
                        ("n = " . "_n_ = ")
                        ("N = " . "_n_ = ")
                        (" n≤" . " _n_ ≤ ")
                        (" n≥" . " _n_ ≥ ")
                        ("λN" . "λ<em>n</em>")
                        (" O(" . " 𝒪(")
                        ("( r =" . "(_r_ =")
                        ("(r)" . "(_r_)")
                        ("(r ≥" . "(_r_ ≥")
                        ("≤ r ≤" . "≤ _r_ ≤")
                        ("≤ r " . "≤ _r_ ")
                        (" r ≤" . " _r_ ≤")
                        (" r values" . " _r_ values")
                        ("(rs =" . "(<em>r</em>s =")
                        (" rs =" . " <em>r</em>s =")
                        ("rmeta =" . "<em>r</em><sub>meta</sub> =")
                        ("rUKB =" . "<em>r</em><sub>UKB</sub> =")
                        (" r " . " _r_ ")
                        ("Mr = " . "_M_~r~ = ")
                        ("zMR " . "_z_~MR~")
                        ("z score" . "_z_ score")
                        ("z-score" . "_z_-score")
                        (" z-" . " _z_-")
                        ("Mage=" . "M~age~ = ")
                        ("Mage " . "M~age~")
                        ("Mage " . "M~age~")
                        ("tmax " . "t~max~")
                        ("Cmax" . "C~max~")
                        ("AUClast" . "AUC~last~")
                        ("AUC∞" . "AUC~∞~")
                        ("AUC0–t" . "AUC~0–t~")
                        ("SDage=" . "SD~age~ = ")
                        ("SDage " . "SD~age~")
                        ("mean=" . "mean = ")
                        ("OR=" . "OR = ")
                        ("AOR=" . "AOR = ")
                        ("RR=" . "RR = ")
                        ("r =" . "_r_ = ")
                        ("r=" . "_r_ = ")
                        ("r = " . "_r_ = ")
                        ("r’ =" . "_r′_ = ")
                        ("(r range = " . "(_r_ range = ")
                        (" r’s " . " _r_’s ")
                        ("r ≈" . "_r_ ≈ ")
                        (" rs " . " <em>r</em>s ")
                        (" r-value" . " <em>r</em>-value")
                        (" p = " . " _p_ = ")
                        (" p=" . " _p_ = ")
                        (" p =" . " _p_ = ")
                        ("(p = " . "(_p_ = ")
                        ("[p = " . "[_p_ = ")
                        ("(P=" . "(_p_ = ")
                        ("p-value" . "_p_-value")
                        ("p value" . "_p_-value")
                        ("p-curve" . "_p_-curve")
                        ("Padj" . "_p_<sub>adj</sub>")
                        ("(t<" . "(_t_ < ")
                        ("(t>" . "(_t_ > " )
                        (" t-statistic" . " _t_-statistic" )
                        ("HR=" . "HR = ")
                        ("rP=" . "_r~p~_ = ")
                        ("rP = " . "_r~p~_ = ")
                        ("rg=" . "_r~g~_ = ")
                        ("rg = " . "_r~g~_ = ")
                        ("(rg)" . "(_r~g~_)")
                        ("(rg " . "(_r~g~_ ")
                        (" rg " . " _r~g~_ ")
                        ("ra = " . "_r~a~_ = ")
                        ("(ra)" . "(_r~a~_)")
                        ("(ra " . "(_r~a~_ ")
                        (" ra " . " _r~a~_ ")
                        ("QM[" . "_Q_~M~[")
                        ("QM(" . "_Q_~M~(")
                        ("= ." . "= 0.")
                        ("< ." . "< 0.")
                        (" > ." . "> 0.")
                        ("ρ =" . "_ρ_ =")
                        ("SE=" . "SE = ")
                        ("</em>=" . "</em> = ")
                        ("</em>&gt;" . "</em> &gt; ")
                        ("</em>&lt;" . "</em> &lt; ")
                        ("</em><" . "</em> < ")
                        ("</em>>" . "</em> > ")
                        ("_<" . "_ < ")
                        ("_>" . "_ > ")
                        ("=." . " = 0.")
                        ("(.0" . "(0.0")
                        (" < .0" . " < 0.0")
                        ("=1 " . " = 1 ")
                        ("=2 " . " = 2 " )
                        ("(p < 0." . "(_p_ < 0.")
                        (" p value" . " _p_-value")
                        (" p-hack" . " _p_-hack")
                        (" p " . " _p_ ")
                        (" <0." . " < 0.")
                        ("<=" . "≤")
                        (">=" . "≥")
                        ("( N " . "(_n_ ")
                        ("( _n_" . "(_n_ ")
                        ("n-gram" . "<em>n</em>-gram")
                        ("k-mean" . "<em>k</em>-mean")
                        ("partial Î·2" . "partial η<sup>2</sup>")
                        ("s9 " . "s’ ") ; workaround MedRxiv/BioRxiv consistent malformation of "s'" as "s9" (somehow); can't auto-replace due to 'CRISPR-Cas9' etc
                        (" −." . " −0.")
                        ("50/50" . "50:50")
                        ("¼" . "1⁄4")
                        ("C3O2" . "C~3~O~2~")
                        ("CO2" . "CO~2~")
                        (" CO2" . " CO~2~")
                        ("CO2." . "CO~2~")
                        ("O2" . "O~2~")
                        ("β42" . "β~42~")
                        ("βA" . "β~A~")
                        ("βC" . "β~C~")
                        ("βE" . "β~E~")
                        ("CH4" . "CH~4~")
                        ("PH3" . "PH~3~")
                        ("PM2.5" . "PM<sub>2.5</sub>")
                        ("μg/m3" . "μg⁄m<sup>3</sup>")
                        ("H2" . "H~2~")
                        ("H2O" . "H~2~O")
                        ("cm-2" . "cm^−2^")
                        ("cm−2" . "cm^−2^")
                        ("cm3" . "cm^3^")
                        (" m3" . " m^3^")
                        ("kg/m2" . "kg⁄m^2^")
                        (" m2" . "m^2^")
                        (" m^2 " . "m^2^")
                        ("×1−1×min" . " × 1<sup>−1</sup> × min")
                        ("t1/2" . "t<sub>1⁄2</sub>")
                        (" m/s" . " m⁄s")
                        (" km/s" . " km⁄s")
                        ("µm3" . "µm^3^")
                        (" ug" . " μg")
                        ("ϰ2" . "ϰ^2^")
                        ("min−1" . "min<sup>−1</sup>")
                        ("min-1" . "min<sup>−1</sup>")
                        (" s-1" . " s^−1^")
                        (" s−1" . " s^−1^")
                        ("ml−1" . "ml<sup>−1</sup>")
                        ("^-1 " . "^−1^ ")
                        (" d−1" . " d<sup>−1</sup>")
                        ("dl−1" . "dl<sup>−1</sup>")
                        ("kb−1" . "kb<sup>−1</sup>")
                        (" g-related" . " _g_-related")
                        ("g+ " . "_g_^+^ ")
                        ("g-factor" . "_g_-factor")
                        ("g-load" . "_g_-load")
                        ("(g)" . "(_g_)")
                        (" g." . " _g_.")
                        (" g," . " _g_,")
                        (" g:" . " _g_:")
                        (" g " . " _g_ ")
                        ("(g=" . "(_g_ = ")
                        ("mg L−1" . "mg _L_^−1^")
                        ("tau181" . "tau~181~")
                        ("vitamin D3" . "vitamin D~3~")
                        ("vitamin D4" . "vitamin D~4~")
                        (" \" . " " \"")
                        (" \\times " . " \cdot ")
                        ("''" . "\"")
                        ("``" . "\"")
                        ("$\alpha$" . "α")
                        ("$\beta$" . "β")
                        ("$\sigma$" . "σ")
                        ("Neff=" . "_N_~eff~ = ")
                        ("Neff" . "_N_~eff~")
                        ("Ntotal" . "_N_~total~")
                        (" Ne " . " <em>N<sub>e</sub></em> ")
                        ("$N_e$" . "_N_~_e_~")
                        ("$\frac{n}{2}$" . "_n_⁄2")
                        ("$\frac{N}{2}$" . "_n_⁄2")
                        ("<em>b</em> = " . "β = ")
                        (" a2" . " _a_^2^")
                        (" c2" . " _c_^2^")
                        (" e2" . " _e_^2^")
                        ("(a2" . "(_a_^2^")
                        ("(c2" . "(_c_^2^")
                        ("(e2" . "(_e_^2^")
                        ("h2SNP" . "<em>h</em><span class=\"subsup\"><sup>2</sup><sub>SNP</sub></span>")
                        ("h2snp" . "<em>h</em><span class=\"subsup\"><sup>2</sup><sub>SNP</sub></span>")
                        ("h2 " . "_h_^2^ ")
                        (" h2" . " _h_^2^")
                        ("(h2=" . "(_h_^2^ = ")
                        ("(h2 = " . "(_h_^2^ = ")
                        ("h2 = " . "_h_^2^ = ")
                        ("c 2" . "_c_^2^")
                        ("h^2SNP" . "<em>h</em><span class=\"subsup\"><sup>2</sup><sub><em>SNP</em></sub></span>")
                        ("_h_^2^ = " . "_h_^2^ = ")
                        ("Fst" . "F~st~")
                        ("rMZ" . "_r_~MZ~")
                        ("rDZ" . "_r_~DZ~")
                        ("R22" . "_R_<span class=\"subsup\"><sup>2</sup><sub>2</sub></span>")
                        (" Wm−1" . " Wm<sup>−1</sup>")
                        (" K−1" . " K<sup>−1</sup>")
                        (" kg−1" . " kg<sup>−1</sup>")
                        ("μg-1" . "μg<sup>−1</sup>")
                        ("μg−1" . "μg<sup>−1</sup>")
                        ("l−1" . "l<sup>−1</sup>")
                        ("m s--1" . "m s<sup>−1</sup>")
                        ("cm--3" . "cm<sup>−3</sup>")
                        (" cm2" . " cm<sup>2</sup>")
                        ("m·h−1" . "m×h<sup>−</sup>")
                        (" </sup>" . "</sup>")
                        ("<sup> " . "<sup>")
                        ("60Co" . "^60^Co")
                        (" I2" . " _I_^2^")
                        ("≤p≤" . " ≤ _p_ ≤ ")
                        ("BF10" . "<a href=\"https://en.wikipedia.org/wiki/Bayes_factor\">BF</a><sub>10</sub>")
                        ("BF10" . "BF<sub>10</sub>")
                        ("BF01" . "BF<sub>10</sub>")
                        (":   " . ": ")
                        ("(i)" . "(1)")
                        ("(ii)" . "(2)")
                        ("(iii)" . "(3)")
                        ("(iv)" . "(4)")
                        ("(v)" . "(5)")
                        ("(vi)" . "(6)")
                        ("(vii)" . "(7)")
                        ("(viii)" . "(8)")
                        ("(ix)" . "(9)")
                        ("(x)" . "(10)")
                        ("(a)" . "(1)")
                        ("(b)" . "(2)")
                        ("(c)" . "(3)")
                        ("(d)" . "(4)")
                        ("(e)" . "(5)")
                        ("(f)" . "(6)")
                        ("(g)" . "(7)")
                        ("(h)" . "(8)")
                        ("(i)" . "(9)")
                        ("(j)" . "(10)")
                        (" i)" . " (1)")
                        (" ii)" . " (2)")
                        (" iii)" . " (3)")
                        (" iv)" . " (4)")
                        (" v)" . " (5)")
                        (" vi)" . " (6)")
                        (" vii)" . " (7)")
                        (" viii)" . " (8)")
                        (" ix)" . " (9)")
                        (" x)" . " (10)")
                        ("DALL-E" . "DALL·E")
                        ("DALL-e" . "DALL·E")
                        ("NOVA1" . "_NOVA1_")
                        (" X " . " Twitter ")
                        (" X." . " Twitter.")
                        ("X, formerly known as Twitter," . "Twitter")
                        ("x-axis" . "_x_-axis")
                        ("x axis" . "_x_-axis")
                        ("z axis" . "_x_-axis")
                        ("y-axis" . "_y_-axis")
                        (" y-value" . " _y_-value")
                        (" x-value" . " _x_-value")
                        (" z-value" . " _z_-value")
                        ("z-axis" . "_z_-axis")
                        ("y axis" . "_y_-axis")
                        ("x-axes" . "_x_-axes")
                        ("x axes" . "_x_-axes")
                        ("z-axes" . "_z_-axes")
                        ("z axes" . "_z_-axes")
                        ("y-axes" . "_y_-axes")
                        ("y axes" . "_y_-axes")
                        (" x " . " _x_ ")
                        (" x:" . " _x_:")
                        (" y " . " _y_ ")
                        (" y:" . " _y_:")
                        (" z " . " _z_ ")
                        (" k " . " _k_ ")
                        (" k=" . " _k_ =")
                        (" n " . " _n_ ")
                        ("(n2)" . "(_n_^2^)")
                        ("(n3)" . "(_n_^3^)")
                        ("n→" . "_n_ → ")
                        ("n-back" . "_n_-back")
                        ("log 2" . "log<sub>2</sub>")
                        ("fuck" . "f—k")
                        ("damn" . "d—n")
                        ("shit" . "s—t")
                        ("95%CI" . "95% CI")
                        ("allowlist" . "whitelist")
                        ("denylist" . "blacklist")
                        (" <sup>" . "<sup>") ; can't auto-replace because of instances like isotopic elements with *prefixed* superscripts, eg ' <sup>60</sup>Co'
                                        ; replace all word-numbers with actual numbers:
                        (" one-hundred" . " 100")
                        (" one hundred" . " 100")
                        (" ninety nine" . " 99")
                        (" ninety-nine" . " 99")
                        (" ninety eight" . " 98")
                        (" ninety-eight" . " 98")
                        (" ninety seven" . " 97")
                        (" ninety-seven" . " 97")
                        (" ninety six" . " 96")
                        (" ninety-six" . " 96")
                        (" ninety five" . " 95")
                        (" ninety-five" . " 95")
                        (" ninety four" . " 94")
                        (" ninety-four" . " 94")
                        (" ninety three" . " 93")
                        (" ninety-three" . " 93")
                        (" ninety two" . " 92")
                        (" ninety-two" . " 92")
                        (" ninety one" . " 91")
                        (" ninety-one" . " 91")
                        (" ninety" . " 90")
                        (" eighty nine" . " 89")
                        (" eighty-nine" . " 89")
                        (" eighty eight" . " 88")
                        (" eighty-eight" . " 88")
                        (" eighty seven" . " 87")
                        (" eighty-seven" . " 87")
                        (" eighty six" . " 86")
                        (" eighty-six" . " 86")
                        (" eighty five" . " 85")
                        (" eighty-five" . " 85")
                        (" eighty four" . " 84")
                        (" eighty-four" . " 84")
                        (" eighty three" . " 83")
                        (" eighty-three" . " 83")
                        (" eighty two" . " 82")
                        (" eighty-two" . " 82")
                        (" eighty one" . " 81")
                        (" eighty-one" . " 81")
                        (" eighty" . " 80")
                        (" seventy nine" . " 79")
                        (" seventy-nine" . " 79")
                        (" seventy eight" . " 78")
                        (" seventy-eight" . " 78")
                        (" seventy seven" . " 77")
                        (" seventy-seven" . " 77")
                        (" seventy six" . " 76")
                        (" seventy-six" . " 76")
                        (" seventy five" . " 75")
                        (" seventy-five" . " 75")
                        (" seventy four" . " 74")
                        (" seventy-four" . " 74")
                        (" seventy three" . " 73")
                        (" seventy-three" . " 73")
                        (" seventy two" . " 72")
                        (" seventy-two" . " 72")
                        (" seventy one" . " 71")
                        (" seventy-one" . " 71")
                        (" seventy" . " 70")
                        ("sixties" . "60s")
                        (" sixty nine" . " 69")
                        (" sixty-nine" . " 69")
                        (" sixty eight" . " 68")
                        (" sixty-eight" . " 68")
                        (" sixty seven" . " 67")
                        (" sixty-seven" . " 67")
                        (" sixty six" . " 66")
                        (" sixty-six" . " 66")
                        (" sixty five" . " 65")
                        (" sixty-five" . " 65")
                        (" sixty four" . " 64")
                        (" sixty-four" . " 64")
                        (" sixty three" . " 63")
                        (" sixty-three" . " 63")
                        (" sixty two" . " 62")
                        (" sixty-two" . " 62")
                        (" sixty one" . " 61")
                        (" sixty-one" . " 61")
                        (" sixty" . " 60")
                        (" fifty nine" . " 59")
                        (" fifty-nine" . " 59")
                        (" fifty eight" . " 58")
                        (" fifty-eight" . " 58")
                        (" fifty seven" . " 57")
                        (" fifty-seven" . " 57")
                        (" fifty six" . " 56")
                        (" fifty-six" . " 56")
                        (" fifty five" . " 55")
                        (" fifty-five" . " 55")
                        (" fifty four" . " 54")
                        (" fifty-four" . " 54")
                        (" fifty three" . " 53")
                        (" fifty-three" . " 53")
                        (" fifty two" . " 52")
                        (" fifty-two" . " 52")
                        (" fifty one" . " 51")
                        (" fifty-one" . " 51")
                        (" fifty" . " 50")
                        (" forty nine" . " 49")
                        (" forty-nine" . " 49")
                        (" forty eight" . " 48")
                        (" forty-eight" . " 48")
                        (" forty seven" . " 47")
                        (" forty-seven" . " 47")
                        (" forty six" . " 46")
                        (" forty-six" . " 46")
                        (" forty five" . " 45")
                        (" forty-five" . " 45")
                        (" forty four" . " 44")
                        (" forty-four" . " 44")
                        (" forty three" . " 43")
                        (" forty-three" . " 43")
                        (" forty two" . " 42")
                        (" forty-two" . " 42")
                        (" forty one" . " 41")
                        (" forty-one" . " 41")
                        (" forty" . " 40")
                        (" thirty nine" . " 39")
                        (" thirty-nine" . " 39")
                        (" thirty eight" . " 38")
                        (" thirty-eight" . " 38")
                        (" thirty seven" . " 37")
                        (" thirty-seven" . " 37")
                        (" thirty six" . " 36")
                        (" thirty-six" . " 36")
                        (" thirty five" . " 35")
                        (" thirty-five" . " 35")
                        (" thirty four" . " 34")
                        (" thirty-four" . " 34")
                        (" thirty three" . " 33")
                        (" thirty-three" . " 33")
                        (" thirty two" . " 32")
                        (" thirty-two" . " 32")
                        (" thirty one" . " 31")
                        (" thirty-one" . " 31")
                        (" thirty" . " 30")
                        (" twenty nine" . " 29")
                        (" twenty-nine" . " 29")
                        (" twenty eight" . " 28")
                        (" twenty-eight" . " 28")
                        (" twenty seven" . " 27")
                        (" twenty-seven" . " 27")
                        (" twenty six" . " 26")
                        (" twenty-six" . " 26")
                        (" twenty five" . " 25")
                        (" twenty-five" . " 25")
                        (" twenty four" . " 24")
                        (" twenty-four" . " 24")
                        (" twenty three" . " 23")
                        (" twenty-three" . " 23")
                        (" twenty two" . " 22")
                        (" twenty-two" . " 22")
                        (" twenty one" . " 21")
                        (" twenty-one" . " 21")
                        ("twenty-first century" . "21<sup>st</sup> century")
                        (" twenty" . " 20")
                        (" twentieth" . " 20<sup>th</sup>")
                        (" nineteen" . " 19")
                        ("nineteenth" . " 19<sup>th</sup>")
                        (" eighteen" . " 18")
                        (" seventeen" . " 17")
                        (" sixteen" . " 16")
                        (" fifteen" . " 15")
                        (" fourteen" . " 14")
                        (" thirteen" . " 13")
                        (" twelve" . " 12")
                        (" eleven" . " 11")
                        (" ten " . " 10 ")
                        (" nine" . " 9")
                        (" eight" . " 8")
                        (" seven" . " 7")
                        (" six" . " 6")
                        (" five" . " 5")
                        (" four" . " 4")
                        (" three" . " 3")
                        ("Ten " . "10 ")
                        ("Nine " . " 9")
                        ("Eight " . "8 ")
                        ("Seven " . "7 ")
                        ("Six " . "6 ")
                        ("Five " . "5 ")
                        ("Four " . "4 ")
                        ("Three " . "3 ")
                        ; look for possible screwed-up exponentials
                        (" 1015" . " 10^15^")
                        (" 1014" . " 10^14^")
                        (" 1013" . " 10^13^")
                        (" 1012" . " 10^12^")
                        (" 1011" . " 10^11^")
                        (" 1010" . " 10^10^")
                        (" 109" . " 10^9^")
                        (" 108" . " 10^8^")
                        (" 107" . " 10^7^")
                        (" 106" . " 10^6^")
                        (" 105" . " 10^5^")
                        (" 104" . " 10^4^")
                        (" 103" . " 10^3^")
                        (" 102" . " 10^2^")
                        )
                      )
             )
         (dolist (pair queries)
           (query-replace (car pair) (cdr pair) nil begin end))
         )

       (let ((regexps '(
                        ("!!+" . "!")
                        ("\\([[:alnum:]]\\)  \\([[:graph:]]\\)" . "\\1 \\2") ; remove subtle whitespace problems: double space
                        ("\\([a-z]+\\),\\([a-z]+\\)" . "\\1, \\2") ; run-together comma phrases often appear in PDF OCR like 'foo,bar'; outside chemical names, this is highly unusual.
                        ("\\([0-9]+\\)[  ]\\([0-9]+\\)" . "\\1,\\2") ; '50 000' or '50 000' → '50,000'
                        ("\\([[:punct:]]\\)p<" . "\\1_p_ <")
                        (" Ne\\\([[:punct:]]\\)" . " <em>N<sub>e</sub></em>\\1")
                        ("\\\([[:punct:]]\\)Ne " . "\\1<em>N<sub>e</sub></em> ")
                        ("<em>\\([a-zA-Z]+\\)</em><sup>2</sup><sub>\\([a-zA-Z]+\\)</sub>" . "<em>\\1</em><sup>2</sup><sub>\\2</sub>")
                        ; numbers, superscripts:
                        ("\\([023456789]\\)th" . "\\1^th^")
                        ("\\([1]\\)st"        . "\\1^st^")
                        ("\\([3]\\)rd"        . "\\1^rd^")
                        ("(four|fif|six|seven|eigh|nin|ten)th"        . "\\1^th^")
                        ("(four|fif|six|seven|eigh|nin|ten)th"        . "\\1^th^")
                        ; numbers, ranges or changes:
                        ; NOTE: we deliberately omit EN DASH-ification of ranges involving negative numbers. For example, '−0.3 to −3.7'
                        ; would look confusing if written '−0.3–−3.7'. It's correct & unambiguous because it uses MINUS SIGN & EN DASH
                        ; appropriately, but the glyphs are way too similar-looking. (Sorry, I didn't design English punctuation.)
                        ; And this is also true if any of the numbers have minus signs (eg. '−0.3–3.7' or '0.3–−3.7' would be no better).
                        ("between \\([0-9∞.]+\\) and \\([0-9∞.]+\\)" . "\\1–\\2")
                        (" one to \\([0-9∞.]+\\)"                    . " \\1–\\2")
                        ("from \\([0-9∞.]+\\) to \\([0-9∞.]+\\)"     . "\\1–\\2")
                        ("\\([0-9∞\\.]+\\) to \\([0-9∞\\.]+\\)"      . "\\1–\\2")
                        ("\\([0-9∞\\.]+\\) through \\([0-9∞\\.]+\\)" . "\\1–\\2")
                        ("from \\([0-9∞.]+|one|two|three\\) to \\([0-9∞.]+\\)"     . "\\1 → \\2")
                        ("\\([a-z]+\\)- and \\([a-z]+-[a-z]+\\)"     . "\\1 & \\2")
                        ("\\([0-9∞.]++|one|two|three\\) to \\([0-9∞.]+\\)"          . "\\1 → \\2")
                        ("between \\([0-9∞.]+\\) and \\([0-9∞.]+\\)" . "\\1–\\2") ; "range between 2 and 10" → "range 2–10"
                        (" \\([0-9∞.]+\\) or \\([0-9∞.]+\\) "        . " \\1–\\2 ")
                        ("\\([0-9∞.]+\\)- to \\([0-9∞.]+\\)-"          . "\\1–\\2-") ; "18- to 20-year-olds" → "18--20-year-olds"
                        ("95% CI = \\([0-9]\\.[0-9]+\\), \\([0-9]\\.[0-9]+\\)" . "95% CI = \\1–\\2")
                        ("CI:\\([0-9]\\)" . "CI: \\1") ; "95% CI:0.01-0.99"
                        )
                      ))
         (dolist (pair regexps)
           (query-replace-regexp (car pair) (cdr pair) nil begin end))
         )

       ; format abstract sub-headings with bold if we are writing an abstract and not a Markdown file:
       (unless (buffer-file-name)
         (query-replace " -\n    " "" nil begin end)
         (query-replace " -\n" "" nil begin end)
         (query-replace "-\n    " "" nil begin end)
         (query-replace "-\n" "" nil begin end)
         (query-replace "- \n" "" nil begin end)
         (query-replace "-\n" "-" nil begin end)

         (query-replace " -- " "---" nil begin end)
         (query-replace " --- " "---" nil begin end)
         (query-replace "--- " "---" nil begin end)
         (query-replace " ---" "---" nil begin end)
         (query-replace "----" "---" nil begin end)
         (query-replace " -\"" "---\"" nil begin end)
         (query-replace "\"- " "\"---" nil begin end)

         (query-replace "\nQuestion " "\n**Question**: " nil begin end)
         (query-replace "Question:" "**Question**: " nil begin end)
         (replace-all "\n\nPurpose" "\n\n**Background**:")
         (replace-all "\nFindings " "\n**Findings**: ")
         (replace-all "\nRecent Findings " "\n**Findings**: ")
         (replace-all "\nMeaning " "\n**Meaning**: ")
         (replace-all "\nImportance " "\n**Importance**: ")
         (replace-all "^Importance " "**Importance**: ")
         (replace-all "\nObjective and Method: " "\n**Objective & Method**: ")
         (replace-all "\nObjective " "\n**Objective**: ")
         (replace-all "\nDesign, Setting, and Participants " "\n**Design, Setting, & Participants**: ")
         (replace-all "\nSearch methods\n\n" "\n\n<strong>Search Method</strong>: ")
         (replace-all " (Methods)" " (<strong>Method</strong>)")
         (replace-all "\nSelection criteria\n\n" "\n\n<strong>Selection Criteria</strong>: ")
         (replace-all "\nInterventions " "\n**Interventions**: ")
         (replace-all "\nMain Outcomes and Measures " "\n**Main Outcomes & Measures**: ")
         (replace-all "Measurement and Results:" "**Measurement & Results**:")
         (replace-all "\nResults and conclusion " "\n**Result & Conclusion**: ")
         (replace-all "\nResults " "\n**Results**: ")
         (replace-all "\nConclusions and Relevance " "\n**Conclusion & Relevance**: ")
         (replace-all "\nTrial Registration " "\n**Trial Registration**: ")
         (replace-all "Background\n" "\n**Background**: ")
         (replace-all "\nBackground " "**Background**: ")
         (replace-all "Introduction: " "\n**Introduction**: ")
         (replace-all "\nIntroduction. " "\n**Introduction**: ")
         (replace-all "\nAims\n" "\n**Aims**: ")
         (replace-all "\nStudy Design\n" "\n**Design**: ")
         (replace-all "\nDesign\n" "\n**Design**: ")
         (replace-all "\nSetting\n" "\n**Setting**: ")
         (replace-all "\nParticipants\n" "\n**Participants**: ")
         (replace-all "\nMeasurements\n" "\n**Measurements**: ")
         (replace-all "\nMaterials and methods\n" "\n\n**Method & Materials**: " )
         (replace-all "\nMethods and materials\n" "\n\n**Method & Materials**: " )
         (replace-all "Methods\n" "\n**Method**: ")
         (replace-all "\nMethods " "\n**Method**: ")
         (replace-all "\nMethod. " "\n**Method**: ")
         (replace-all "Highlights\n" "\n**Highlights**:\n\n")
         (replace-all "Interpretation\n" "\n**Interpretation**: ")
         (replace-all "Funding\n" "\n**Funding**: ")
         (replace-all "Highlights:" "**Highlights**: ")
         (replace-all "Background\\:" "**Background**: ")
         (replace-all "Background\\. " "**Background**: ")
         (replace-all "Abstract:" "**Abstract**: ")
         (replace-all "Context:" "**Context**: ")
         (replace-all "Purpose:" "**Background**: ")
         (replace-all "Rationale: " "**Background**: ")
         (replace-all "Rationale\n" "**Background**: ")
         (replace-all "Rationale: " "**Background**: ")
         (replace-all "Rationale\n\n" "**Background**: ")
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
         (replace-all "Methods:" "**Method**: ")
         (replace-all "Methods\\. " "**Method**: ")
         (replace-all "\nMethods and findings\n\n" "\n\n**Method & Findings**: ")
         (replace-all "\n\nDesign and methods" "\n\n**Design & Method**: ")
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
         (replace-all "\n\nMethods and Results\n" "\n\n**Method & Results**: ")
         (replace-all "Results:" "**Results**: ")
         (replace-all "Results\\. " "**Results**: ")
         (replace-all "\nResult:" "\n**Results**: ")
         (replace-all "\nResult\\. " "\n**Results**: ")
         (replace-all "Measurements and Results\n" "\n**Measurements & Results**: ")
         (replace-all "Results\n" "\n**Results**: ")
         (replace-all "Aims:" "**Aims**: ")
         (replace-all "Aim:\n" "**Aims**: ")
         (replace-all "Aim\n\n" "\n\n**Aim**: ")
         (replace-all "Methodology/Principal Findings\n\n" "\n**Methodology/Principal Results**: ")
         (replace-all "Methodology\n\n" "\n**Methodology**: ")
         (replace-all "Principal Findings\n\n" "\n**Principal Results**: ")
         (replace-all "Methods and Findings\n\n" "\n**Method & Results**: ")
         (replace-all "Findings\n\n" "\n**Results**: ")
         (replace-all "Findings\n" "\n**Results**: ")
         (replace-all "\nScope of review\n\n" "\n\n**Scope of Review**: ")
         (replace-all "\n\nReview methods " "\n\n**Review Method**: ")
         (replace-all "\nMajor conclusions\n\n" "\n\n**Conclusion**: ")
         (replace-all "\nFindings: " "\n**Results**: ")
         (replace-all "\nFinding: " "\n**Results**: ")
         (replace-all "\nFinding " "\n**Results**: ")
         (replace-all "\nQuestion " "\n**Question**: ")
         (replace-all "^Question " "**Question**: ")
         (replace-all "Significance:" "**Significance**: ")
         (replace-all "Conclusions\n\n" "\n**Conclusion**: ")
         (replace-all "Conclusions\n" "\n**Conclusion**: ")
         (replace-all "Conclusion:" "**Conclusion**: ")
         (replace-all "Conclusions\\. " "**Conclusion**: ")
         (replace-all "Conclusions:" "**Conclusion**: ")
         (replace-all "Conclusions and Relevance:" "**Conclusion & Relevance**: ")
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
         (replace-all "\nContext " "\n**Background**: ")
         (replace-all "\nBackground and objective " "\n**Background**: ")
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
         (replace-all "\nMETHODS " "\n**Method**: ")
         (replace-all "\n\nRESULTS" "\n\n**Results**: ")
         (replace-all "\nMain results\n\n" "\n**Results**: ")
         (replace-all "\n\nCONCLUSIONS" "\n\n**Conclusion**: ")
         (replace-all "CONCLUSIONS\n\n" "\n**Conclusion**: ")
         (replace-all "Conclusions/Significance\n\n" "\n**Conclusion**: ")
         (replace-all "\nAuthors' conclusions\n\n" "\n\n**Conclusion**: ")
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
         (replace-all "Purpose\n\n" "\n**Background**: ")
         (replace-all "Purpose\n" "**Background**: ")
         (replace-all "Purpose of Review\n" "**Background**: ")
         (replace-all "Design/methodology/approach\n\n" "\n**Method**: ")
         (replace-all "RESEARCH DESIGN AND METHODS " "**Method**: ")
         (replace-all "RESEARCH DESIGN AND METHODS\n\n" "\n**Method**: ")
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
       (replace-all "\n► " "\n- ")
       (replace-all "Previous article in issueNext article in issue" "")
       (replace-all "Previous article in issue\nNext article in issue" "")
       (replace-all "\nAbstract\n" "\n")
       (replace-all "statistical significance" "statistical-significance")
       (replace-all "statistically significant" "statistically-significant")
       (replace-all "Statistically significant" "Statistically-significant")
       (replace-all "clinical significance" "clinical-significance")
       (replace-all "clinically significant" "clinically-significant")
       (replace-all "statistically different" "statistically-significantly different")
       (replace-all "statistical difference" "statistically-significant difference")
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
       (query-replace "\nhat " "\nWhat" nil begin end)

       ; shout-out to typography nerds by using the proper logotypes:
       (let ((case-fold-search t) (search-upper-case t)) ; need these to override `query-replace` magic‽
         (query-replace "P > 0" "_p_ > 0" nil begin end)
         (query-replace "p < " "_p_ <" nil begin end)
         (query-replace "P ≤ " "_p_ ≤ " nil begin end)
         (query-replace "p<" "_p_ < "   nil begin end)
         (query-replace-regexp "[^u]p>" "_p_ > "   nil begin end) ; avoid '<sup>'
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
         (query-replace "Sect\\. " "Section " nil begin end)

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
         (query-replace "(t="  "(_t_ = " nil begin end)
         (query-replace " t-test" " _t_-test" nil begin end)
         (query-replace " t test" " _t_-test" nil begin end)
         (query-replace "(t test" "(_t_-test" nil begin end)
         (query-replace " R2 " " R^2^ " nil begin end)
         (query-replace "(R2 " "(R^2^ " nil begin end)
         (query-replace "R2=" " R^2^ ="  nil begin end)
         (query-replace "R2<" " R^2^ <" nil begin end)
         (query-replace "R2>" " R^2^ <" nil begin end)
         (query-replace "r2" "R^2^" nil begin end)
         (query-replace "χ2" "χ^2^" nil begin end)
         (query-replace "mm 2" "mm^2^" nil begin end)
         (query-replace "age2" "age^2^" nil begin end)
         (query-replace "m−2" "m^−2^" nil begin end)
         (query-replace "rxy " "<em>r<sub>xy</sub></em>" nil begin end)
         (query-replace "n-of-1" "<em>n</em>-of-1" nil begin end)
         (query-replace " g = " " _g_ = " nil begin end)
         (query-replace " g's" " _g_'s" nil begin end)
         (query-replace "(g " "(_g_ " nil begin end)
         (query-replace " g)" " _g_)" nil begin end)
         (query-replace "(m = " "(_m_ =" nil begin end)
         (query-replace "LaTeX" "<span class=\"logotype-latex\">L<span class=\"logotype-latex-a\">a</span>T<span class=\"logotype-latex-e\">e</span>X</span>" nil begin end) ; <span class="logotype-latex">L<span class="logotype-latex-a">a</span>T<span class="logotype-latex-e">e</span>X</span>
         (query-replace "LATEX" "<span class=\"logotype-latex\">L<span class=\"logotype-latex-a\">a</span>T<span class=\"logotype-latex-e\">e</span>X</span>" nil begin end)
         (query-replace "TeX" "<span class=\"logotype-tex\">T<sub>e</sub>X</span>" nil begin end) ; <span class="logotype-tex">T<sub>e</sub>X</span>
         (query-replace "TEX" "<span class=\"logotype-tex\">T<sub>e</sub>X</span>" nil begin end)
         (query-replace-regexp "^\\.\\.\\.he" "...The" nil begin end) ; very common to truncate in copy-paste
         ) ; end case-fold let (should create strict case-matching?)
       (query-replace "Nepeta cataria" "_Nepeta cataria_" nil begin end)
       (query-replace "MC4R" "_MC4R_" nil begin end)
       (query-replace "GPT2" "GPT-2" nil begin end) (query-replace "GPT3" "GPT-3" nil begin end) (query-replace "GPT4" "GPT-4" nil begin end)
       (query-replace "two thirds" "2⁄3" nil begin end)
       (query-replace "two-thirds" "2⁄3" nil begin end)
       (query-replace "three-fourths" "3⁄4" nil begin end)
       (query-replace "3-fourths" "3⁄4" nil begin end)
       (query-replace "three-fifths" "3⁄5" nil begin end)
       (query-replace-regexp "\\([0-9]+\\) of \\([0-9]+\\)" "\\1⁄\\2" nil begin end)
       (query-replace-regexp "\\([0-9]+\\) of the \\([0-9][0-9]?[0-9]?\\)" "\\1⁄\\2" nil begin end)
       (query-replace-regexp " \\([0-9][0-9]?[0-9]?\\) of \\([0-9][0-9]?[0-9]?\\) " " \\1⁄\\2 " nil begin end)
       (query-replace-regexp "\\([0-9]+\\) out of \\([0-9][0-9]?[0-9]?\\)" "\\1⁄\\2" nil begin end)
       (query-replace-regexp "\\([0-9]+\\) out of the \\([0-9][0-9]?[0-9]?\\)" "\\1⁄\\2" nil begin end)
       (query-replace-regexp "\\([0-9]+\\) in every \\([0-9]+\\)" "\\1⁄\\2" nil begin end) ; eg. "approximately one in every 10 citations across leading psychology journals is inaccurate"
       (query-replace-regexp "≈ \\([0-9]+\\)%" "≈\\1%" nil begin end) ; "to ≈ 15% for heroin"

       (query-replace "...." "..." nil begin end)
       (query-replace "....." "..." nil begin end)
       (query-replace-regexp "\n\\.\\.\\([A-Za-z]\\)" "\n...\\1" nil begin end) ; replace malformed '...' ellipsis excerpts
       (query-replace-regexp "^\\.\\.\\. " "..." nil begin end)
       (query-replace " = ." " = 0." nil begin end)
       (query-replace "Ss" "Subjects" nil begin end) ; PsycNET APA abbreviations
       (query-replace-regexp "\\([Ee]\\)xp\\(s?\\) " "\\1xperiment\\2 " nil begin end)
       (query-replace-regexp " (PsycInfo Database Record (c) [12][0-9]+ APA, all rights reserved)" "" nil begin end) ; PsycNET copyright junk
       (query-replace " • " ", " nil begin end) ; some 'Keywords' sections are always MIDDLE DOT formatted

       (query-replace-regexp "\\([0-9]+\\)·\\([0-9]+\\)" "\\1.\\2" nil begin end) ; mostly for the Lancet

       (query-replace "Figs. " "Figures " nil begin end)
       (query-replace "Fig. " "Figure " nil begin end)
       (query-replace-regexp "Supplementary [fF]ig\\. \\(S?[0-9]+[a-hA-H]*\\)\\." "**Supplementary Figure \\1**."  nil begin end) ; 'Supp Fig. 1. ', 'Fig. 2a)' etc
       (query-replace-regexp "Supplementary [fF]ig\\. \\(S?[0-9]+[a-hA-H]*\\)"    "**Supplementary Figure \\1**"   nil begin end) ; 'Supp Fig. 1,', 'Fig. 2a,' etc
       (query-replace-regexp "Supplementary [fF]igure \\(S?[0-9]+[a-hA-H]*\\)\\." "**Supplementary Figure \\1**."  nil begin end) ; 'Supp Figure 1. The graph' etc
       (query-replace-regexp "Supplementary ([fF]ig\\. \\(S?[0-9]+[a-hA-H]*\\))"  "(**Supplementary Figure \\1**)" nil begin end) ; (Supp Fig. 3b)
       (query-replace-regexp "Supplementary ([fF]igure \\(S?[0-9]+[a-hA-H]*\\))"  "(**Supplementary Figure \\1**)" nil begin end) ; (Supp Figure 3b)
       (query-replace-regexp "Supplementary ([fF]ig\\. \\(S?[0-9]+[a-hA-H]*\\),"  "(**Supplementary Figure \\1**," nil begin end) ; (Supp Fig. 3b,
       (query-replace-regexp "Supplementary [fF]igures \\(S?[0-9]+[a-hA-H]*\\) and \\([0-9]+[a-hA-H]*\\)"  "**Supplementary Figures \\1** & **\\2**" nil begin end)
       (query-replace-regexp "[fF]ig\\.? ?\\(S?\\)\\([0-9\\.]+[a-hA-H]*\\)\\.?" "**Figure \\1\\2**."  nil begin end) ; 'Fig. 1. ', 'Fig. 2a)', 'Figure 1.5' etc
       (query-replace-regexp "[fF]ig\\.? \\(S?\\)\\([0-9\\.]+[a-hA-H]*\\)"    "**Figure \\1\\2**"   nil begin end) ; 'Fig. 1,', 'Fig. 2a,' etc
       (query-replace-regexp "[fF]igure \\(S?\\)\\([0-9\\.]+[a-hA-H]*\\.?\\)" "**Figure \\1\\2**"  nil begin end) ; 'Figure 1. The graph' etc
       (query-replace-regexp "([fF]ig\\.? \\(S?\\)\\([0-9\\.]+[a-hA-H]*\\))"  "(**Figure \\1\\2**)" nil begin end) ; (Fig. 3b)
       (query-replace-regexp "([fF]igure \\(S?\\)\\([0-9\\.]+[a-hA-H]*\\))"  "(**Figure \\1\\2**)" nil begin end) ; (Figure 3b)
       (query-replace-regexp "([fF]ig\\.? \\(S?\\)\\([0-9\\.]+[a-hA-H]*\\),"  "(**Figure \\1\\2**," nil begin end) ; (Fig. S3b,

       (query-replace-regexp "[aA]ppendix.? ?\\([a-zA-Z]?\\)\\([0-9\\.]+[a-hA-H]*\\)"  "**Appendix \\1\\2**" nil begin end) ; 'Appendix A2'

       ; '§ SECTION SIGN' is better than writing out '<strong>Section N</strong>' everywhere. It's much shorter, we already use SECTION SIGN heavily, it reduces overuse of bold, is easier to grep for, and it saves a bit of time formatting annotations (because of the lack of lookahead/lookbehind in these regexp rewrites, 'Section N' will match every time, even if it's already wrapped in <strong></strong>/**bolding**, and I have to waste time skipping them). It would be nice to symbolize Figure/Table/Experiment/Data as well, but there's no widely-understood symbol which could be used, and usually no abbreviation either. (Perhaps 'Supplement.*' could be replaced by just 'S' and 'Figure' by 'Fig.' at some point…)
       (query-replace-regexp "[Ss]ection ?\\([0-9.]+[a-hA-H]*\\)"  "§\\1" nil begin end) ; 'Section 9' → '§9', 'section 9' → '§9'
       (query-replace-regexp "Part ?\\([0-9.]+[a-hA-H]*\\)"  "§\\1" nil begin end) ; 'Part 9' → '§9'
       (query-replace-regexp "[Ss]ections ?\\([0-9.]+[a-hA-H]*\\) and \\([0-9.]+[a-hA-H]*\\)"  "§\\1 & §\\2" nil begin end) ; 'Sections 1 and 2' → '§1 & §2'

       (query-replace-regexp "Chapter \\([0-9]+[a-hA-H]*\\)" "**Ch\\1**"  nil begin end) ; 'Chapter 1', 'Chapter 7a' etc

       (query-replace-regexp "Supplementary [Tt]able\\.? \\([0-9]+[a-hA-H]*\\)\\." "**Supplementary Table \\1**."  nil begin end) ; 'Table. 1. ', 'Table. 2a)' etc
       (query-replace-regexp "Supplementary [Tt]able\\.? \\([0-9]+[a-hA-H]*\\)"    "**Supplementary Table \\1**"   nil begin end) ; 'Table. 1,', 'Table. 2a,' etc
       (query-replace-regexp "Supplementary [Tt]able \\([0-9]+[a-hA-H]*\\)\\." "**Supplementary Table \\1**:"  nil begin end) ; 'Table 1. The graph' etc
       (query-replace-regexp "Supplementary ([Tt]able\\. \\([0-9]+[a-hA-H]*\\))"  "(**Supplementary Table \\1**)" nil begin end) ; (Table. 3b)
       (query-replace-regexp "Supplementary ([Tt]able \\([0-9]+[a-hA-H]*\\))"  "(**Supplementary Table \\1**)" nil begin end) ; (Table 3b)
       (query-replace-regexp "Supplementary ([Tt]able\\. \\([0-9]+[a-hA-H]*\\),"  "(**Supplementary Table \\1**," nil begin end) ; (Table. 3b,
       (query-replace-regexp "Supplementary [Tt]ables \\([0-9]+[a-hA-H]*\\) and \\([0-9]+[a-hA-H]*\\)" "**Supplementary Tables \\1** & **\\2**" nil begin end)

       (query-replace-regexp "[Tt]ables?\\.? \\(S?[0-9]+[a-hA-H]*\\)\\." "**Table \\1**:"  nil begin end) ; 'Table. 1. ', 'Table. 2a)' etc
       (query-replace-regexp "[Tt]ables?\\.? \\(S?[0-9]+[a-hA-H]*\\)"    "**Table \\1**"   nil begin end) ; 'Table. 1,', 'Table. 2a,' etc
       (query-replace-regexp "[Tt]ables?\\.? \\(S?[a-hA-H]+[0-9]+\\)"    "**Table \\1**"   nil begin end) ; 'Table S3'
       (query-replace-regexp "[Tt]ables? \\(S?[0-9]+[a-hA-H]*\\)\\." "**Table \\1**:"  nil begin end) ; 'Table 1. The graph' etc
       (query-replace-regexp "([Tt]ables?\\.? \\(S?[0-9]+[a-hA-H]*\\))"  "(**Table \\1**)" nil begin end) ; (Table. 3b)
       (query-replace-regexp "([Tt]ables? \\(S?[0-9]+[a-hA-H]*\\))"  "(**Table \\1**)" nil begin end) ; (Table 3b)
       (query-replace-regexp "([Tt]ables?\\.? \\(S?[0-9]+[a-hA-H]*\\),"  "(**Table \\1**," nil begin end) ; (Table. 3b,
       (query-replace-regexp "[Tt]ables?\\.? \\(S?[0-9]+[a-hA-H]*\\) and \\([0-9]+[a-hA-H]*\\)"  "**Table \\1** & **\\2**" nil begin end) ; 'Tables 9 and 10'

       (query-replace-regexp "Experiment \\([0-9]+[a-hA-H]*\\)" "**Experiment \\1**"  nil begin end) ; 'Experiment 1', 'Experiment 2a' etc
       (query-replace "Experiments 1 and 2" "**Experiments 1** & **2**" nil begin end)
       (query-replace-regexp "[Ss]tudy \\([0-9]+[a-hA-H]*\\)" "**Study \\1**"  nil begin end)
       (query-replace-regexp "[Ss]tudies \\([0-9]+[a-hA-H]*\\)[-–]+\\([0-9]+[a-hA-H]*\\)" "**Studies \\1--\\2**"  nil begin end)
       (query-replace-regexp "[Ss]tudies \\([0-9]+[a-hA-H]*\\) and \\([0-9]+[a-hA-H]*\\)" "**Studies \\1** & **\\2**"  nil begin end)
       (query-replace-regexp "[Ss]tudies \\([0-9]+[a-hA-H]*\\), \\([0-9]+[a-hA-H]*\\), and \\([0-9]+[a-hA-H]*\\)" "**Studies \\1**, **\\2**, & **\\3**"  nil begin end)
       (query-replace-regexp "[Ss]tudies \\([0-9]+[a-hA-H]*\\), \\([0-9]+[a-hA-H]*\\), \\([0-9]+[a-hA-H]*\\)" "**Studies \\1**, **\\2**, **\\3**"  nil begin end)

       (query-replace-regexp "^\\( *\\)\\([0-9]+\\)\\. " "\\1#. " nil begin end) ; Markdown ordered-list number formatting: see default.css for more discussion, but using '1./2./3.' list numbering in Markdown yields hardwired number formatting rather than plain `<ol>`, which causes styling problems when nested in lists.
       (query-replace-regexp "Supplementary Data \\([0-9]+[a-hA-H]*\\)" "**Supplementary Data \\1**"  nil begin end) ; '(Supplementary Data 7)'
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
       (query-replace-regexp "\\^-\\([[:digit:]]+\\)\\^" "<sup>−\\1</sup>" nil begin end) ; 'foo^-1^' → 'foo<sup>−1</sup>'
       (query-replace-regexp "\\([[:digit:]\\.]+\\)[Ee]-\\([[:digit:]]+\\)" "\\1 × 10<sup>−\\2</sup>" nil begin end) ; 2.0E-26, 2.0e-26
       ; (query-replace-regexp "[x×] ?10--\\([0-9]+\\)" "× 10<sup>−\\1</sup>" nil begin end)
       (query-replace-regexp "\\([a-zA-Z0-9][,;\\.]\\)\\([[:digit:]]+\\) \\([A-Za-z]\\)" "\\1<sup>\\2</sup> \\3" nil begin end) ; look for copy-pasted footnotes, like "X works great.13 Therefore" or "possibly biological reasons,4 but"
       (query-replace-regexp "Footnote\\([0-9]+\\)" "<sup>\\1</sup>" nil begin end) ; cambridge.org has footnotes that copypaste like 'Footnote10'
       (query-replace-regexp "\\([a-zA-Z0-9.]\\”\\)\\([[:digit:]]+\\) \\([A-Z]\\)" "\\1<sup>\\2</sup> \\3" nil begin end)
       (query-replace-regexp "\\([[:punct:]]\\)\\[\\([0-9, -]+\\)\\] " "\\1<sup>\\2</sup> " nil begin end) ; Wikipedia-style referencs: "Foo.[33]" or "is around 75%,[83 but varies"
       ; (query-replace-regexp "\\([[:punct:]]\\)\\([0-9]+\\) " "\\1<sup>\\2</sup> " nil begin end) ; looser
       (query-replace-regexp "\\([[:punct:]]\\)\\([0-9,- ]+\\)$" "\\1<sup>\\2</sup>" nil begin end) ; looser: handle end of line
       (query-replace-regexp " \\[\\([0-9, -]+\\)\\]\\([[:punct:]]\\)" "\\2<sup>\\1</sup> " nil begin end) ; 'contributing to higher energy intake [42].'
       (query-replace-regexp "\\[\\([0-9, -]+\\)\\] " "<sup>\\1</sup> " nil begin end)
       (query-replace-regexp "\\([0-9]+\\)- and \\([0-9]+\\)-" "\\1 & \\2-" nil begin end) ; "We use 1979- and 1997-cohort National Longitudinal Survey of Youth (NLSY) data" → "We use 1979 & 1997-cohort"
       (query-replace-regexp "pg?\\. [0-9]" "pg\\1") ; page citations: 'p. 5', 'pg. 5' → 'pg5'

       (query-replace-regexp "\\([[:alnum:]]\\)- " "\\1---" nil begin end)
       (query-replace-regexp "\\([[:alnum:]]\\)\\.\\. " "\\1... " nil begin end)
       (query-replace-regexp "\\([0-9]\\) %" "\\1%" nil begin end)
       (query-replace-regexp "\\([0-9]\\)folds?" "\\1×" nil begin end)
       (query-replace-regexp "\\([0-9]\\)-folds?" "\\1×" nil begin end)
       (query-replace-regexp "\\([0-9]\\) folds?" "\\1×" nil begin end)
       (query-replace "1st" "1^st^" nil begin end)
       (query-replace "2nd" "2^nd^" nil begin end)
       (query-replace "3rd" "3^rd^" nil begin end)
       (query-replace ":.0" ": 0.0" nil begin end)
       (query-replace "−." "−0." nil begin end)
       (query-replace " -." " −0." nil begin end)
       (query-replace "[-." "[−0." nil begin end)
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
       (query-replace-regexp "10[-−]\\([[:digit:]]+\\)" "10^−\\1^" nil begin end) ; eg '10-10 mol/liter'
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
       (query-replace-regexp "\\([[:digit:]]+\\) x \\([[:digit:]]+\\)" "\\1 × \\2" nil begin end)
       (query-replace-regexp "\\([[:digit:]]+\\) \\* \\([[:digit:]]+\\)" "\\1 × \\2" nil begin end)
       ; comma formatting: eg. '100000000000' -> '100,000,000,000' - but we need to skip URLs where such numbers are ubiquitous & time-wasting. Avoid possible years which start with 1/2.
       (my-markdown-or-html-query-replace-regexp "\\([[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]+\\)" #'comma-format-number begin end) ; 5+ digit numbers
       (my-markdown-or-html-query-replace-regexp "\\([3-9][[:digit:]][[:digit:]][[:digit:]]\\)" #'comma-format-number begin end) ; 4-digit numbers, which might be years/dates, like '5000', but skipping ones starting in 1/2, which might be a year like '2023' etc
       (my-markdown-or-html-query-replace-regexp "[[:punct:]] \\([3-9][[:digit:]][[:digit:]][[:digit:]]\\)" #'comma-format-number begin end) ; 4-digit numbers, which might be years/dates: if preceded by punctuation, this is *usually* a number, because it'd be odd to write the year at the stard of a phrase. No one says 'Foo bar. 2023 is the date, June the month.' It'd always be 'Foo bar. In June 2023' etc.
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
       (query-replace-regexp "\\([a-zA-Z]\\)--\\([a-zA-Z]\\)"               "\\1---\\2"       nil begin end) ; if it's not a single hyphen after all, then it may be an EM DASH that's intended.
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
       (query-replace-regexp "^ +<" " <" nil begin end)
       (query-replace-regexp "^ +</" " </" nil begin end) ; fix problems with leading whitespace causing HTML snippets to be treated as 4-space-indented literals

       (query-replace " <sup>" "<sup>" nil begin end)
       (query-replace "*8" "**" nil begin end)
       (query-replace "8*" "**" nil begin end)
       (query-replace "!{" "![" nil begin end)
       (query-replace "].(" ".](" nil begin end)
       ; (query-replace-regexp " \"'\\(.+?\\)', " " \"‘\\1’, " nil begin end) ; avoid downstream YAML errors from titles encoded in tooltips with single straight quotes
       (replace-all "\n\n\n" "\n\n")

       ; do this at the end to minimize errors from things that would've been fixed
       (when (and (equal (buffer-name) "foo")
                  ; avoid breaking numbered-lists & headers
                  (not (save-excursion
                         (goto-char (point-min))
                         (re-search-forward "^[0-9#]" nil t)))
                  ; we must skip annotations with `.interview` markup: the indentation is critical, and simply wrapping breaks the interview formatting unless it is correctly done, which we can't do because it requires parsing Markdown lists & depths to understand exactly how many spaces to indent. So since removing newlines-in-paragraphs is nice but not critical, we just skip it for interviews.
                  (not (save-excursion
                  (goto-char (point-min))
                  (re-search-forward "<div class=\"interview\">" nil t))))
         (markdown-remove-newlines-in-paragraphs) ; once all the hyphenation is dealt with, remove the hard-newlines which are common in PDF copy-pastes. These hard newlines are a problem because they break many string matches, and they make `langcheck` highlight every line beginning/ending in red as an error.
         )

       (message "%s %s" begin end)
       )
     (flyspell-buffer)
     (query-inflation-adjust)
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

(defun query-inflation-adjust ()
  "Interactively inflation-adjust all dollar amounts in the buffer.
This replaces all dollar amounts in the buffer with a Pandoc Markdown
 Span element in Gwern.net inflation-adjuster format.

The function checks first if there are any dollar amounts in the buffer.
If there are, it prompts the user for a year, defaulting to the current year,
and then replaces all dollar amounts with the dollar amount wrapped in a
Pandoc Markdown Span element tagged with the year, following the format
'[$Y]($YEAR)'.

For example, '$20' would be replaced with '[$20]($2023)' if the year 2023
was input. If there are no dollar amounts in the buffer, the function exits
without prompting the user.

The function handles negative dollar amounts, and dollar amounts followed
immediately by punctuation. It does not handle dollar amounts in
scientific notation or with currency symbols, or dollar amounts with unusual
formatting following some European conventions.

The function checks if the year input by the user is a numeric string.
If it is not, the function outputs an error message and does not perform
the replacement."
  (interactive)
  (let ((from "\\([-−]?\\$\\([0-9,.]+\\)\\([kmbt]?\\)\\)\\>"))
    (if (not (string-match-p from (buffer-string)))
        (message "query-inflation-adjust: No dollar amounts to adjust; exiting.")
      (let* ((year (read-string "Year: " (format-time-string "%Y"))))
        (if (string-match-p "\\`[0-9]+\\'" year)
            (let ((to (concat "[$\\2]($" year ")\\3")))
              (query-replace-regexp from to nil (point-min) (point-max)))
          (message "Invalid year"))))))

;; (defun number-with-commas (num)
;;   "Format a number `NUM` with commas."
;;   (let ((num-str (number-to-string num))
;;         (pos 0)
;;         (result ""))
;;     (dotimes (i (length num-str))
;;       (when (and (> pos 0) (= 0 (mod pos 3)))
;;         (setq result (concat "," result)))
;;       (setq pos (1+ pos)
;;             result (concat (substring num-str (- (length num-str) pos) (- (length num-str) (1- pos))) result)))
;;     result))
(defun comma-format-number (num-str)
  "Insert commas into a number string `NUM-STR`."
  (let ((num (string-to-number num-str)))
    (number-with-commas num)))

(defun number-with-commas (num)
  "Format a number `NUM` with commas."
  (let ((num-str (reverse (number-to-string num)))  ; Reverse the string to process from the least significant digit
        (result ""))
    (dotimes (i (length num-str))
      (when (and (> i 0) (= 0 (mod i 3)))
        (setq result (concat result ",")))  ; Insert comma before every group of 3 digits
      (setq result (concat result (char-to-string (elt num-str i)))))  ; Append current digit
    (reverse result)))  ; Reverse the result to correct the order

; implement a URL-skipping `query-replace-regexp`, named `my-markdown-or-html-query-replace-regexp`. Many rewrites need to skip URLs but there's no good way to do it hitherto. This function also takes arbitrary functions to do the rewrite, which can be useful too for more complex rewrites.
; GPT-4:
(require 'pulse)
(defcustom my-markdown-or-html-query-replace-skip-urls t
  "Non-nil means `my-markdown-or-html-query-replace-regexp` should skip link URLs."
  :type 'boolean
  :group 'my-markdown-or-html)
(defun my-markdown-or-html-inside-link-p ()
  "Return non-nil if point is inside a Markdown or HTML link."
  (let ((faces (list (get-text-property (point) 'face)
                     ; The point might be at the beginning or end of the URL, which might not have the expected face property.
                     ; To fix this issue, we check for the face property not only at the point but also at the previous and next characters.
                     ; This way, we ensure the function returns non-nil if the point is at the beginning or end of a URL.
                     (get-text-property (1- (point)) 'face)
                     (get-text-property (1+ (point)) 'face))))
    (cl-some (lambda (face) (or (eq face 'markdown-url-face)
                                (eq face 'html-attr-value-face)))
             faces)))
(defun my-markdown-or-html-query-replace-args ()
  "Read the arguments for `my-markdown-or-html-query-replace-regexp`."
  (query-replace-read-args (concat "Query replace"
                                   (if current-prefix-arg " word" "")
                                   " regexp"
                                   (if (and transient-mark-mode mark-active) " in region" ""))
                           t))

(defun my-markdown-or-html-query-replace-confirm (from to)
  "Prompt the user to confirm replacing `FROM` with `TO`.
Highlight the matched text during the query. Accept `y` for yes, `n` for no, and `q` to quit."
  (pulse-momentary-highlight-region (match-beginning 0) (match-end 0))
  (let ((response nil))
    (while (not (member response '(?y ?n ?q)))
      (setq response (read-char-choice (format "Replace `%s' with `%s'? (y/n/q) " from to) '(?y ?n ?q))))
    (cond ((eq response ?y) t)
          ((eq response ?n) nil)
          ((eq response ?q) 'quit)
          )
    )
  )

(defun my-markdown-or-html-query-replace-regexp (regexp replace-fn &optional start end)
  "Interactively query replace `REGEXP` with the result of `REPLACE-FN`.
This version continues searching after a `q` quit signal but skips replacements.

This function skips over Markdown or HTML link URLs when performing the
replacements if `my-markdown-or-html-query-replace-skip-urls` is non-nil. It operates in the
region between `START` and `END` if both are provided; otherwise, it operates on
the entire buffer.

`REPLACE-FN` should be a function that accepts a string (the matched text) and
returns the replacement string.

WARNING: This function  handles user input to `quit` (via `q`) not by stopping the search entirely,
but by continuing to search through the remainder of the document without performing
 any further replacements. This hack works around a problem where quitting a replacement operation
would unexpectedly kill the calling command (`fmt`).
By setting a `skip-replacement` flag upon a `quit` signal, somehow it works?

\(fn REGEXP REPLACE-FN &optional START END)"
  (interactive)
  (let ((replacements 0)
        (skip-replacement nil)) ; New flag to control skipping of replacements
    (save-excursion
      (goto-char (or start (point-min)))
      (save-match-data
        (while (re-search-forward regexp end t)
          (unless (or skip-replacement (my-markdown-or-html-inside-link-p))
            (let* ((from (match-string 0))
                   (to (funcall replace-fn from))
                   (confirmation (my-markdown-or-html-query-replace-confirm from to)))
              (cond ((eq confirmation t)
                     (replace-match to t t)
                     (setq replacements (1+ replacements)))
                    ((eq confirmation 'quit)
                     (setq skip-replacement t))))))) ; Continue searching but skip replacements
    (message "Query replace finished, %d replacements made" replacements)))
  )


; GPT-4
(require 'rx)
(defgroup markdown-newline-removal nil
  "Options for removing newlines within paragraphs in Markdown text."
  :group 'markdown)
(defcustom markdown-excluded-chars (rx (any ?- ?\n ?\d ?# ?* ?> ?. ?? ?|))
  "Characters to exclude when removing newlines within paragraphs in Markdown text."
  :type 'regexp
  :group 'markdown-newline-removal)
(defun markdown-remove-newlines-in-paragraphs (&optional buffer use-region)
  "Replace newlines with spaces within paragraphs of Markdown text in BUFFER.
It then adds a newline between each sentence.

If BUFFER is nil, use the current buffer. If USE-REGION is non-nil,
operate on the current region instead of the entire buffer.
This assumes you have already removed hyphenation (either by removing the
hyphen+newline for line-breaking-only hyphens, or just the newline when the
hyphen is in the original word regardless of line-breaking)."
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
              (replace-match "\\1 \\3" nil nil)))))
      ;; Insert a newline at the end of each sentence
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\([.!?]\\)\\([[:space:]]\\)" nil t)
          (replace-match "\\1\n"))))

    ;; Inform the user when the operation is complete
    (message "Newlines removed within paragraphs.")))

(defvar markdown-rewrites '())
(defun buffer-contains-substring (string)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))
(defun getLinkSuggestions (rewriteFile)
  "Query the user for a long list of possible search-and-replaces in a buffer.
The list is defined in a file argument REWRITEFILE and generated by an external utility parsing the gwern.net sources.
This works by: read a Lisp file with the `rewrites` variable; `rewrites` is a list of string pairs.
Each string is a possible anchor text like \"Barack Obama\" and its corresponding URL, \"https://en.wikipedia.org/wiki/Barack_Obama\".
Looping over this list of pairs, for each pair, do a `query-replace` query on the buffer,
converting it into a Markdown lint like `[Barack Obama](https://en.wikipedia.org/wiki/Barack_Obama)`.
This external file can be written by hand, but it is intended to be automatically generated by a Haskell tool
which parses the full Gwern.net HTML+Markdown sources for all possible links,
and turns the full set of links into a list of anchor text/link pairs, which it dumps in Elisp format into the file.
This tool is run automatically by a cron job.
So any link on Gwern.net will automatically become a search-and-replace query,
and it will be updated based on any manually-added links."
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
  "Turn a Markdown buffer into a HTML5 snippet without newlines and with escaped quotes,
suitable for using as a GTX string inside annotated gwern.net links (see `full.gtx`)."
  (interactive)
  (call-interactively #'fmt)
  (save-window-excursion

    (defvar $pos 1)
    (message "Preprocessing and compiling into HTML…")
    ; Pandoc converts the Markdown to HTML. Then the HTML goes through `preprocess-markdown` which runs additional typographic/formatting rewrites, runs LinkAuto to automatically linkify text, and then runs through GenerateSimilar to provide a list of relevant annotations to curate as the 'see-also' section at the bottom of annotations (if they are approved).
    ; NOTE: because `preprocess-markdown` is calling the OA API via the embedder, $OPENAI_API_KEY must be defined in the Emacs environment, either via `(setenv "OPENAI_API_KEY" "sk-xyz123456789")` or by putting it in `~/.bash_profile`. (Putting it in `.env` or `.bashrc` is not enough, because they won't apply to GUI/X Emacs)
    (let ((markdown-command "~/wiki/static/build/preprocess-annotation.sh") (visible-bell nil))
      (markdown-kill-ring-save)

      (setq $pos (point-max))
      (goto-char $pos)
      (insert "\n---------------\n")
      (yank)
      (goto-char $pos)

      ; (replace-all "\n" " ")
      (let ( ; (begin (if (region-active-p) (region-beginning) (+ $pos 1)))
             ; (end (if (region-active-p) (region-end) (point-max)))
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
      ; (query-replace "'" "''" nil begin end)
      (delete-trailing-whitespace)
      (forward-line)
      (html-mode)
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
  "Automatically paragraphize single-paragraph abstracts.
Intended for Markdown mode with double-newlines for newlines;
may malfunction if run on other formats like HTML
(where `</p><p>` pairs can come in many forms, not to mention other block elements like blockquotes)."
  (interactive)
  (delete-trailing-whitespace)
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
             (>= (buffer-size) 500)) ; ensure that there is enough in the buffer to plausibly be a full copy-pasted abstract, as opposed to a random snippet or line.
    (markdown-paragraphize)))
(add-hook 'post-command-hook #'markdown-paragraphize-hook)

; add new-line / paragraph snippet
(add-hook 'html-mode-hook
          (lambda ()
            (define-key html-mode-map (kbd "<C-return>")  (lambda () (interactive)
                                                            (if (= ?\s (preceding-char)) (delete-char -1))
                                                            (insert "</p>\n<p>")
                                                            (if (= ?\s (following-char)) (delete-char 1)))
            )
          ))

(add-hook 'markdown-mode-hook   'visual-fill-column-mode)

;; Markup editing shortcuts for HTML/Markdown/GTX annotation editing.
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
  "Surround selected region (or word) with HTML <em> tags for italics/emphasis (also Markdown, which supports `*FOO*`)."
  (interactive)
  (surround-region-or-word "<em>" "</em>"))
(defun markdown-insert-emphasis ()
  "Surround selected region (or word) with Markdown asterisks for italics/emphasis.
Equivalent to `<em>FOO</em>` in HTML.
Gwern.net uses `*` for emphasis, and generally reserves `_` for italics such as book titles
(in keeping with Internet conventions predating Gruber's Markdown mistake of conflating `*`/`_`)."
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
Built-in CSS class in HTML & Pandoc Markdown, span syntax is equivalent to
`[FOO]{.smallcaps}`.
Smallcaps are used on Gwern.net for second-level emphasis after bold has been used."
  (interactive)
  (surround-region-or-word "<span class=\"smallcaps\">" "</span>"))
(defun markdown-insert-smallcaps ()
  "Surround selected region (or word) with smallcaps syntax (Pandoc Markdown).
Built-in CSS class in HTML & Pandoc Markdown, equivalent to
`<span class=\"smallcaps\">FOO</span>`.
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
  "Surround selected region FOO BAR (or word FOO) with a `margin-note`.
\(Implemented as a special `<span>` class.\)
This creates marginal glosses (in the left margin) as counterparts to sidenotes.
These margin-notes are used as very abbreviated italicized summaries of the
 paragraph \(like very small inlined section headers\)."
  (interactive)
  (surround-region-or-word "[" "]{.marginnote}"))
(defun html-insert-margin-note ()
  "Surround selected region FOO BAR (or word FOO) with a `margin-note`.
\(Implemented as a special `<span>` class.\)
This creates marginal glosses (in the left margin) as counterparts to sidenotes.
These margin-notes are used as very abbreviated italicized summaries of the
 paragraph \(like very small inlined section headers\).
When inserting margin-notes into HTML snippets, that usually means an annotation
and the margin-note is an editorial insertion, which are denoted by paired `[]` brackets.
To save effort, we add those as well."
  (interactive)
  (surround-region-or-word "<span class=\"marginnote\">[" "]</span>"))
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
(add-hook 'html-mode-hook (lambda()(define-key html-mode-map "\C-c\ \C-m" 'html-insert-margin-note)))
;; ;;; YAML: (the YAML files store raw HTML snippets, so insert HTML rather than Markdown markup)
;; (add-hook 'yaml-mode-hook (lambda()(define-key yaml-mode-map "\C-c\ \C-e" 'html-insert-emphasis)))
;; (add-hook 'yaml-mode-hook (lambda()(define-key yaml-mode-map "\C-c\ \C-s" 'html-insert-strong)))
;; (add-hook 'yaml-mode-hook (lambda()(define-key yaml-mode-map "\C-c\ s"    'html-insert-smallcaps)))
;; (add-hook 'yaml-mode-hook (lambda()(define-key yaml-mode-map "\C-c\ \C-w" 'html-insert-wp-link)))
;; (add-hook 'yaml-mode-hook (lambda()(define-key yaml-mode-map "\C-c\ \C-m" 'html-insert-margin-note)))

;sp
; (add-hook 'markdown-mode-hook 'flyspell)
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
(defun my/flycheck-disable-for-gtx-files ()
  "Disable flycheck for files ending with '.gtx'."
  (let ((filename (buffer-file-name)))
    (when (and filename (string-match "\\.gtx\\'" filename))
      (flycheck-mode -1))))

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'flycheck-before-syntax-check-hook #'my/flycheck-disable-for-gtx-files)
; syntax checkers must be whitelisted/enabled individually, so turn on proselint & mdl
(add-to-list 'flycheck-checkers 'proselint) ; configured in ~/.proselintrc
(add-to-list 'flycheck-checkers 'markdown-mdl) ; configured in ~/.mdlrc ; list & explanation of rules: <https://github.com/markdownlint/markdownlint/blob/master/docs/RULES.md>

; 'langtool': a Java tool for grammar checking: <https://github.com/mhayashi1120/Emacs-langtool> <https://languagetool.org/dev>
(when (require 'langtool nil 'noerror)
  (setq langtool-language-tool-jar "~/bin/bin/LanguageTool-4.7/languagetool-commandline.jar")
  (setq langtool-default-language "en-US")
                                        ; <http://wiki.languagetool.org/command-line-options> <https://community.languagetool.org/rule/list> ; can look up in there or run a command like
                                        ; $ java -jar languagetool-commandline.jar -b --line-by-line --language en-US  --disable "EN_QUOTES,MULTIPLICATION_SIGN,WORD_CONTAINS_UNDERSCORE,ET_AL,SENTENCE_WHITESPACE,DASH_RULE,MORFOLOGIK_RULE_EN_US,EN_UNPAIRED_BRACKETS,WHITESPACE_RULE" ~/wiki/research-criticism.md
                                        ; to disable specific rules
  (setq langtool-user-arguments '("-b" "--line-by-line" "--disable" "EN_QUOTES,MULTIPLICATION_SIGN,WORD_CONTAINS_UNDERSCORE,ET_AL,SENTENCE_WHITESPACE,DASH_RULE,MORFOLOGIK_RULE_EN_US,EN_UNPAIRED_BRACKETS,WHITESPACE_RULE,UNIT_SPACE,TR"))
)

; mismatched quotes are no good either
; <http://stackoverflow.com/questions/9527593/customizing-check-parens-to-check-double-quotes>
(add-hook 'markdown-mode-hook (lambda () (modify-syntax-entry ?\" "$" markdown-mode-syntax-table)))
;; (add-hook 'yaml-mode-hook (lambda () (modify-syntax-entry ?{  "(" markdown-mode-syntax-table) (modify-syntax-entry ?} ")" markdown-mode-syntax-table)))
;; (add-hook 'yaml-mode-hook (lambda () (modify-syntax-entry ?\( "(" markdown-mode-syntax-table) (modify-syntax-entry ?\) ")" markdown-mode-syntax-table)))

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
;; (add-hook 'yaml-mode-hook (lambda ()
;;    (mapc (lambda (pair) (push pair prettify-symbols-alist))
;;          '(
;;            ("\\[" . ?〖)
;;            ("\\]" . ?〗)
;;            ("–" . ?ˉ) ; highlight EN DASH because they look so much like regular hyphens
;;            ("—" . ?⸺) ; EM DASH
;;            ("−" . ?━) ; MINUS SIGN
;;            ))))

;; for line-by-line incrementing IDs; useful for popup links.
;; (defun add-html-spans (start end)
;;   (interactive "r")
;;   (goto-char start)
;;   (let ((id 1))
;;     (while (and (< (point) end)
;;                 (re-search-forward "^>.*$" nil t))
;;       (let ((line (match-string 0)))
;;         (unless (string-match "^>[ \t]*$" line)
;;           (replace-match (format "> <span id=\"line-%d\">%s</span>" id
;;                                  (substring line 2))
;;                          t nil nil 0)
;;           (setq id (1+ id)))))))
