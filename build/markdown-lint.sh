#!/bin/bash
# When:  Time-stamp: "2024-04-29 18:38:27 gwern"
# see https://gwern.net/about#markdown-checker

set +x

# function to wrap checks and print highlighted warning if non-zero output (self-documenting):
wrap() { OUTPUT=$($1 2>&1)
         WARN="$2"
         if [ -n "$OUTPUT" ]; then
             echo -e "\e[41m$WARN\e[0m":
             echo -e "$OUTPUT";
         fi; }
fgp () { grep -F --context=1 --line-number --color=always "$@"; }
egp () { grep -E --ignore-case --context=1 --line-number --color=always "$@"; }

for PAGE in "$@"
do
    if [[ $PAGE == *.md ]]; then

        λ(){ file "$PAGE" | fgp --invert-match 'text'; }
        wrap λ "Not text, perhaps due to bad copy-paste"

        λ(){ egp '[^[:print:]]' "$PAGE"; }
        wrap λ "File contains unprintable characters"

        λ(){ fgp -e 'http://dl.dropbox' -e '.wiley.com/doi/abs/'  \
                 -e 'www.tandfonline.com/doi/abs/' -e 'jstor.org' -e 'springer.com' -e 'springerlink.com' \
                 -e 'www.mendeley.com' -e 'academia.edu' -e 'researchgate.net' -e 'pdf.yt' \
                 -e 'photobucket' -e 'imgur.com' -e 'hathitrust.org' -e 'emilkirkegaard.dk' -e 'arthurjensen.net' \
                 -e 'humanvarieties.org' -e 'libgen.io/' -e 'gen.lib.rus.ec/' -e 'sci-hub.bz/' -e '](http://www.scilogs.com/' \
                 -e 'sci-hub.cc/' -e "papers.nber.org/" -e '](!wikipedia' -e '](!wikipedia)'"'s" -e 'https://wwww.' -e 'http://wwww.' \
                 -e 'http://33bits.org' -e 'https://gwern.net' -e 'https://gwern.net' -e 'web.archive.org/web/2' \
                 -e 'webarchive.org.uk/wayback/' -e 'webcitation.org' -e 'plus.google.com' -e 'www.deepdotweb.com' -e 'raikoth.net' \
                 -e 'drive.google.com/file' -e 'ssrn.com' -e 'ardenm.us' -e 'gnxp.nofe.me' -e 'psycnet.apa.org' \
                 -e 'wellcomelibrary.org/item/' -e 'dlcs.io/pdf/' -e 'secure.wikimedia.org' \
                 -e 'https://biorxiv.org' \
                 -e 'fbclid=' -e '?gid=' -e 'twitter.com/#!' -e 'pay.reddit.com' -e 'europepmc.org' -e 'drugcite.com' \
                 -e 'guardian.co.uk' -e 'mlp.wikia.com' -e '฿' -e '!Wikipedia ""' -e 'medium.com' -e 'temcauley.staff.shef.ac.uk' \
                 -e 'yahoo.com' -e 'bloomberg.com' -e '.wsj.com' -e 'extremelongevity.net' -e 'blog.openai.com' \
                 -e 'https://ww.gwern.net' -e 'https://w.gwern.net' -e 'www.heretical.com' -e 'books.google.ca' \
                 -e 'lesserwrong.com' -e 'au.news.yahoo.com' -e 'northjersey.com' -e 'tribune.com.pk' -e 'idsnews.com' \
                 -e 'catsensebook.com' -e 'whec.com' -e 'www.mercurynews.com' -e 'meetup.com' \
                 -e 'dlcs.io/' -e 'centerforcollegeaffordability.org' -e 'quora.com' -e 'times-news.com' -e 'www.cebp.nl' \
                 -e '#filmtv' -e 'nybooks.com' -e '<div id="columns">' -e 'annualreviews.org' \
                 -e 'dspace.mit.edu' -e 'shirky.com' -e '](http://www.nzherald.co.nz)' -e 'https://www.arxiv.org' \
                  -e 'goodreads.com/review/show' -e 'myanimelist.net/reviews.php?id='  \
                 -e 'cloudfront.net' -e 'https://www.amazon.com/s?ie=UTF8&field-isbn=&page=1&rh=i:stripbooks' -e 'http://ltimmelduchamp.com' \
                 -e 'thiswaifudoesnotexist.net)' -e 'thiswaifudoesnotexist.net"' -e 'www.wikilivres.ca' -e 'worldtracker.org' \
                 -e 'meaningness.wordpress.com' -e 'ibooksonline.com' -e 'tinypic.com' -e 'isteve.com' -e 'j-bradford-delong.net' -- "$PAGE";
           egp -e 'https://arxiv.org/abs/[0-9]\{4\}\.[0-9]+v[0-9]' -- "$PAGE";}
        wrap λ "find bad URLS, unacceptable/unreliable/risky domains, malformed syntax, unmatched apostrophes"

        λ(){ fgp  -e 'http://news.ycombinator.com' -e 'http://github.com' -e 'http://www.ncbi.nlm.nih.gov/pubmed/' \
                  -e 'http://www.coursera.org' -e 'http://en.wikipedia.org' -e 'http://biorxiv.org' -e 'http://www.biorxiv.org' \
                  -e 'http://arxiv.org' -e 'http://slatestarcodex.com' -e 'http://arxiv.org' -e 'http://www.arxiv.org' \
                  -e 'http://myanimelist.net' -e 'http://www.bmj.com' -e 'http://www.youtube.com' -e 'http://youtu.be' -e "http://www.nature.com/" \
                  -e "http://www.sciencedirect.com" -e "http://journals.plos.org" -e "http://www.pnas.org" -e "http://www.wsj.com" \
                  -e "http://link.springer.com" -e "http://www.bbc.com" -e "http://genomebiology.biomedcentral.com" -e "http://www.npr.org" \
                  -e "http://www.ipscell.com" -e "http://www.newyorker.com" -e "http://www.nytimes.com" -e 'http://ask.metafilter.com' \
                  -e 'http://www.metafilter.com' -e 'http://www.vanityfair.com' -e  'http://econlog.econlib.org' -e 'http://www.overcomingbias.com' \
                  -e 'http://www.economist.com' -e 'http://www.theverge.com' -- "$PAGE"; }
        wrap λ "HTTP → HTTPS URLs"

        ## ban articles written by John Hewitt; he endorses the pig-human pseudoscience, lies about research (eg claiming platypus genome proven to be a bird hybrid), and makes bad arguments (eg. his criticism of senolytics because senescent cells do not have a single unique universal signature):
        λ(){ grep -F -e 'phys.org' -- "$PAGE" | fgp -v -e '2019-07-cat-science.html' -e '2017-08-cavemen-genetic-checkup.html' -e'2019-12-mouse-pups-born-eggs-derived.html'; }
        wrap λ "Phys.org link detected: make sure John Hewitt didn't write it"

        λ(){ ~/wiki/static/build/Columns.hs "$PAGE"; }
        wrap λ "Add columns wrapper?"

        λ(){ runghc -i/home/gwern/wiki/static/build/ ~/wiki/static/build/link-extractor.hs "$PAGE" | egp --only-matching -e '^http://.*archive\.org/.*\.pdf$'; }
        wrap λ "check for aggregator-hosted PDFs and host them on Gwern.net to make them visible to Google Scholar/provide backups"

        λ(){ egp -e 'http://www.pnas.org/content/.*/.*/.*.abstract' -e '[^\.]t\.test\(' -e '^\~\~\{\.' \
                 -e 'ncbi.nlm.nih.gov/pubmed/[[:digit:]][[:digit:]]*' \
                 -e 'biorxiv.org/content/biorxiv/.*\.pdf ' -e '(https://www.biorxiv.org/content/biorxiv/.*\.pdf)' \
                 -e 'arxiv.org/pdf/.*\.pdf)' -e 'arxiv.org/pdf/.*\.pdf "'  -- "$PAGE"; }
        wrap λ "if I am not linking a specific page on Arxiv or BioRxiv, why am I linking to the PDF rather than the landing page?"
        λ(){ fgp -e ".pdf#subsection" -e ".pdf#Appendix" -- "$PAGE"; }
        wrap λ "Section PDF links break when archived locally due to PDF/A restrictions (thereby breaking the local-PDF popups), so avoid unusual anchors in favor of 'page=N' anchor links"

        λ() { egp -e '<div id="abstract"' -e '<div id="collapseSummary"' -e '^</div$' -e ' n=[[:digit:]]' -e ' n = [[:digit:]]' \
                  -e ']\(/.*#fn[[:digit:]]' -e '[0-9]\.[0-9]*⁄' -e '^\.>' -e ' "\)[ );,$]' \
                  -e 'cssExtension: [a-c,e-z]' -e '^R> ' -e '^#+ Comments$' -- "$PAGE";
              fgp -e '(www' -e ')www' -e '![](' -e ']()' -e ' )' -e '](//' -e '](/wiki/' -e '](wiki/' -e '——–' -e '——' -e '————–' -e ' --- ' \
                  -e ' percent ' -e "    Pearson'" -e '~~~{.sh}' -e 'library("' -e ' +-' -e ' -+' -e '"collapse Summary"' -e '"CollapseSummary"' -e 'collapseSumary' -e '<!_-' -e ' bu ' \
                  -e ']{.dropcaps}' -e '{,smallcaps}' -e '[.smallcaps}' -e '[PMC]{.smallcaps}' -e 'nsheppard' -e '<div class-' \
                  -e '^ > [a-Z]' -e '^  > [a-Z]' -e '^   > [a-Z]' -e '^  - [a-Z]' -e '^   - [a-Z]' \
                  -e '<p class="drop-cap' -e 'class="drop-caps-' -e ' n_=' -e '~~~{.collape}' -e '~~~~' -e '{.fullwidth}' -e 'Wikiepdia' -e 'Wikipdia' -e '/doc/genetic/' \
                  -e '" ](' -e '!Marin:' -e '](image/' -e '](images/'  -e '](/image/' -e '](/images/' -e '\Mathcal{' -e "''" -e '``' -e ' " ' -e '\mathcal{O}(log' -e 'preload="metadata"' \
                  -e '#close' -e '#page=page' -e '.pdf#section' -e '.pdf#subsection' -e '^<sup>' -e '<sup>^' -e '^</sup>' -e '</sup>^' -e ' : ' -e ']^[' -- "$PAGE"; }
        wrap λ "look for broken syntax in original Markdown: (NOTE: footnotes should not be linked to because they are unstable; they should either be sections/appendices, or given a stable permanent span ID)"

        λ() { grep -P -e '[\x{0590}-\x{05FF}]|[\x{0600}-\x{06FF}]'  -- "$PAGE"; }
        wrap λ "Check that bidirectional scripts (Hebrew, Arabic) are not displayed; can cause Firefox Mac rendering bugs page-wide"

        λ(){ grep -F '~~~{.' -- "$PAGE" | tr -d '{}~' | tr ' ' '\n' | \
                 grep -F -v -e '.R' -e '.collapse' -e '.Haskell' -e '.Bash' -e '.Diff' -e '.Javascript' -e '.numberLines' \
                       -e '.Python' -e '.C ' -e '.CPO' -e '.SQL' -e '.Bibtex' -e '.HTML' -e '.CSS'; }
        wrap λ "look for potentially broken syntax-highlighting classes"

        λ(){ grep -E --invert-match '[[:space:]]*>' -- "$PAGE" | fgp -e ' significant ' -e ' significantly ' -e ' obvious' -e 'basically' -e ' the the ' -e 'reproducibility crisis' -e 'replicability crisis'; } # WARNING: can't use 'egp' for some reason
        wrap λ "look for personal uses of illegitimate statistics & weasel words, but filter out blockquotes"

        λ(){ fgp -e ' feet' -e ' foot ' -e ' pound ' -e ' mile ' -e ' miles ' -e ' inch' -- "$PAGE";
             egp -e '[0-9][0-9]* \(January\|February\|March\|April\|May\|June\|July\|August\|September\|October\|November\|December\) [0-9][0-9][0-9][0-9]' -- "$PAGE"; }
        wrap λ "look for English/imperial units as a reminder to switch to metric as much as possible"

        λ(){ grep -E --only-matching '^\[\^.*\]: ' -- "$PAGE" | sort | uniq --count | \
                 grep -F --invert-match '      1 [^'; }
        wrap λ "check for duplicate footnote IDs (force no highlighting, because the terminal escape codes trigger bracket-matching)"

        λ(){ egp --only-matching '\!\[.*\]\(wiki/.*\)' -- "$PAGE"; }
        wrap λ "indicates broken copy-paste of image location"

        λ(){ grep --perl-regexp --null --only-matching -e '\!\[.*\]\(.*\)\n\!\[.*\]\(.*\)' -- "$PAGE"; }
        wrap λ "look for images used without newline in between them; in some situations, this leads to odd distortions of aspect ratio/zooming or something (first discovered in /correlation in blockquotes)"

        λ(){ egp '^[^$]* [^\"]\$[^$]*$' -- "$PAGE"; }
        wrap λ "look for unescaped single dollar-signs (risk of future breakage)"

        λ(){ egp -e '[a-zA-Z]- '  -- "$PAGE"; }
        wrap λ "Write out shortcuts"

        λ(){ fgp -e '= ~' -- "$PAGE" | fgp --invert-match ' mods'; }
        wrap λ "Unicodify: instead of writing 'x = ~y', unicode as '≈'"
        λ(){ fgp -e '?!' -e '!?' -e '<->' -e '~>' -- "$PAGE"; }
        wrap λ "Unicodify: misc"
        λ(){ egp -e '[[:alnum:]]≠[[:alnum:]]' -- "$PAGE"; }
        wrap λ "Unicodify: != renders better with spaces around it"
        λ(){ egp -e '\$[[:alnum:]]\$' -e '\$\\sqrt{[[:digit:]]}\$' -e '\$[[:alnum:]]^[[:alnum:]]\$' -e '\$[[:alnum:]]+ \cdot [[:alnum:]]+\$' -e '\$[[:alnum:]]\$' \
                 -e '\$\\sqrt{[[:digit:]]}\$' -e '\$log_2\$' -e '\$log_10\$' -e '\$\\mathcal{N}' -- "$PAGE"; }
        wrap λ "LaTeX: simplify to Unicode/Markdown"
        λ(){ egp -e '\$\\frac{[0-9]+}{[0-9]+}' -- "$PAGE"; }
        wrap λ "Unicodify: LaTeX for simple numerical fractions is overkill; use '⁄' FRACTION SLASH instead"
        λ(){ grep -F ' \\times ' -- "$PAGE"; }
        wrap λ "LaTeX: \\cdot is nicer"
        λ(){ grep -F '$$E(' -- "$PAGE"; }
        wrap λ "LaTeX: use \\mathbb for expectations"
        λ(){ egp -e '[a-zA-Z]→[a-zA-Z]' -e '[a-zA-Z]←[a-zA-Z]' -e '[a-zA-Z]↔[a-zA-Z]' -- "$PAGE"; }
        wrap λ "Add spaces to arrows: more legible, fewer odd interactions (like Hyphenator)"

        λ(){ fgp -i -e '<div class="admonition-warning">' -e '<div class="admonition-note">' -e '<div class="admonition-error">' \
                 -e '**Warn' -e '**Note' -e '**Error' -- "$PAGE"; }
        wrap λ "Reminder to use formal admonitions instead of just bolding"

        λ() { egp -e '^[0-9]\. \*[^\*]' -e '^- \*[^\*][a-Z]' -e '^- \[.*\]\{\.smallcaps\}' -- "$PAGE"; }
        wrap λ "Reminder to use bold as the top level of emphasis in lists rather than italics or smallcaps"

        [ "$(grep -E '^title: '       "$PAGE" | wc --char)" -le 10 ] && echo -e '\e[41mWARNING\e[0m: "title:" metadata too short.'
        [ "$(grep -E '^title: '       "$PAGE" | wc --char)" -ge 60 ] && echo -e '\e[41mWARNING\e[0m: "title:" metadata too long.'
        [ "$(grep -E '^description: ' "$PAGE" | wc --char)" -le 90 ] && echo -e '\e[41mWARNING\e[0m: "description:" metadata too short.'
        [ "$(grep -E '^description: ' "$PAGE" | wc --char)" -ge 320 ] && echo -e '\e[41mWARNING\e[0m: "description:" metadata too long.'
        [ "$(grep -E '^next: '        "$PAGE" | wc --char)" -eq  0 ] && echo -e '\e[41mWARNING\e[0m: "next:" metadata is missing.'
        [ "$(grep -E '^previous: '    "$PAGE" | wc --char)" -eq  0 ] && echo -e '\e[41mWARNING\e[0m: "previous:" metadata is missing.'
        [ "$(grep -E '^thumbnail: '   "$PAGE" | wc --char)" -le 20 ] && echo -e '\e[41mWARNING\e[0m: No thumbnail/illustration defined.'

        # skip on newsletters since their URLs are always being modified:
        [[ ! $PAGE =~ "newsletter/" ]] && [ "$(grep -E '^modified: 20'  "$PAGE" | wc --char)" -eq  0 ] && echo -e '\e[41mWARNING\e[0m: "modified:" metadata is missing.'

        λ() { markdown-length-checker.hs "$PAGE";}
        wrap λ "Source code line lengths"

        λ() { markdown-footnote-length.hs "$PAGE"; }
        wrap λ "Footnote lengths"

        # proselint configuration is done in ~/.proselintrc; for Markdown, currently disabled: "archaism.misc" "cursing.nfl" "links.broken" "misc.but" "misc.chat speak" "misc.phrasal_adjectives" "sexism.misc" "typography.diacritical_marks" "typography.exclamation" "typography.symbols" "typography.symbols.ellipsis" "typography.symbols.curly_quotes"
        λ() { proselint "$PAGE"; }
        wrap λ "Proselint nitpicks"

        ## reused later as well:
        HTML=$(mktemp  --suffix=".html")
        cat "$PAGE" | pandoc --metadata lang=en --metadata title="Test" --mathml --to=html5 --standalone --number-sections --toc --reference-links --css=https://gwern.net/static/css/default.css -f markdown+smart --template=/home/gwern/bin/bin/pandoc-template-html5-articleedit.html5 - --output="$HTML"

        λ() { tidy -quiet -errors --doctype html5 "$HTML" 2>&1 >/dev/null | grep -F -v -e 'Warning: <img> proprietary attribute "loading"'; }
        wrap λ "HTML validation problems"

        λ() { cat "$HTML" | elinks -dump --force-html | grep -E 'file:///dev/stdin#.*\..*'; }
        wrap λ "Header problem: period present (valid HTML but breaks CSS/JS!). Override default Pandoc link fragment with '{#header-without-period}'"

        λ() {  COLLAPSED=$(cat "$HTML" | grep -E --after-context=3 '<h[0-7] class="collapse"')
               COLLAPSED_SECTION_COUNT=$(echo "$COLLAPSED" | grep -E '<h[0-7] class="collapse"' | wc --lines)
               COLLAPSED_SUMMARY_COUNT=$(echo "$COLLAPSED" | grep -F '<div class="abstract-collapse">' | wc --lines)
               MISSING=$(( COLLAPSED_SECTION_COUNT - COLLAPSED_SUMMARY_COUNT ))
               if [[ $MISSING != 0 ]];
               then echo "Missing collapsed section summaries?"
                    echo "Sections: $COLLAPSED_SECTION_COUNT ; summaries: $COLLAPSED_SUMMARY_COUNT"
                    echo "Hits: $COLLAPSED"
               fi; }
        wrap λ "Missing collapse summaries"

        λ() {  fgp -e "<""del"">" "$HTML";
               elinks -dump --force-html "$HTML" \
                   | fgp -e '\frac' -e '\times' -e '(http' -e ')http' -e '[http' -e ']http'  \
                        -e ' _ ' -e '[^' -e '^]' -e '/* ' -e ' */' -e '<!--' -e '-->' -e '<-- ' -e '<—' -e '—>' \
                        -e '$title$' -e '<del>' -e '.smallcaps' -e '</q<' -e '<q>' -e '</q>' \
                        -e '$description$' -e '$author$' -e '$tags$' -e '$category$' \
                        -e '(!Wikipedia' -e '(!Hoogle' -e 'http://www.gwern.net' -e 'http://gwern.net' -e 'https://www.gwern.net' -e 'smallcaps}' \
                        -e '**'; }
        wrap λ "look for syntax errors making it to the final HTML output"

        λ(){ runghc -i/home/gwern/wiki/static/build/ ~/wiki/static/build/link-extractor.hs "$PAGE" | grep -E -v -e "^http" -e '^!Wikipedia' -e '^#' -e '^/' -e '^\!' -e  '^\$'; }
        wrap λ "special syntax shouldn't make it to the compiled HTML"

        λ() { runghc -i/home/gwern/wiki/static/build/ ~/wiki/static/build/link-extractor.hs "$PAGE" | grep -E -v -e '^\!' -e  '^\$' | sort | uniq --count | sort --numeric-sort | grep -E -v -e '.* 1 '; }
        wrap λ "Duplicate links"

        λ(){ egp --only-matching '\!\[.*\]\(http://.*\)' -- "$PAGE"; }
        wrap λ "image hotlinking deprecated; impolite, and slows page loads & site compiles"

        # Note links which need to be annotated (probably most of them...)
        λ() { runghc -i/home/gwern/wiki/static/build/ ~/wiki/static/build/link-extractor.hs "$PAGE" | grep -E -v -e '^\!' -e '^\$' -e '^/doc/.*txt' -e '.xz$' -e '^#' -e '.patch$' -e '.jpg$' -e '.png$' -e '.mp4' -e '.mp3' -e 'news.ycombinator.com' -e 'old.reddit.com' -e 'youtube.com' -e 'youtu.be/' -e 'amazon.com' -e 'bandcamp.com' -e 'dropbox.com' -e 'vocadb.net' -e 'twitter.com' -e '#link-bibliography' -e 'https://en.wikipedia.org/wiki' | runghc -istatic/build/ static/build/link-prioritize.hs; }
        wrap λ "Link annotations required"

        # we use link annotations on URLs to warn readers about PDFs; if a URL ends in 'pdf', it gets a PDF icon. What about URLs which redirect to or serve PDF?
        # we must manually annotate them with a '#pdf'. Check URLs in a page for URLs which serve a PDF MIME type but do not mention PDF in their URL:
        λ() { checkPDF() {
                  MIME=$(timeout 20s curl --insecure --write-out '%{content_type}' --silent -o /dev/null "$@" | grep -F -i -e "application/pdf" -e "application/octet-stream")
                  if [ ${#MIME} -gt 5 ]; then
                      if [[ ! $@ =~ .*pdf.* ]] && [[ ! $@ =~ .*PDF.* ]]; then
                          echo "UNWARNED NON-LOCAL PDF: " "$@" "$MIME"
                      fi
                      echo "NON-LOCAL PDF: $@"
                  fi; }
              export -f checkPDF
              # examples: no/no/yes
              ## checkPDF 'http://www.nytimes.com/2009/11/15/business/economy/15view.html ' # no
              ## checkPDF 'http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.208.2314&rep=rep1&type=pdf' # yes
              ## checkPDF 'https://files.osf.io/v1/resources/np2jd/providers/osfstorage/59614dec594d9002288271b6?action=download&version=1&direct' # yes
              runghc -i/home/gwern/wiki/static/build/ ~/wiki/static/build/link-extractor.hs "$PAGE" | grep -E "^http" | grep -F -v -e 'https://gwern.net' -e arxiv.org -e pnas.org | sort -u | shuf | parallel -n 1 checkPDF; }
        wrap λ "Non-icon/warned PDF links"

        λ() { for PDF in $(runghc -i/home/gwern/wiki/static/build/ ~/wiki/static/build/link-extractor.hs "$PAGE" | grep -E -e '^/doc/' -e 'https:\/\/gwern\.net\/' | \
                               grep -E '\.pdf$' | sed -e 's/\/doc/doc/' -e 's/https:\/\/gwern\.net//' ); do

                  TITLE=$(exiftool -printFormat '$Title' -Title ~/wiki/"$PDF")
                  AUTHOR=$(exiftool -printFormat '$Author' -Author ~/wiki/"$PDF")
                  DATE=$(exiftool -printFormat '$Date' -Date ~/wiki/"$PDF")
                  DOI=$(exiftool -printFormat '$DOI' -DOI ~/wiki/"$PDF")
                  TEXT=$(pdftotext ~/wiki/"$PDF" - 2>/dev/null)
                  TEXT_LENGTH=$(echo "$TEXT" | wc --chars)
                  PREPRINT=$(echo "$TEXT" | grep -F -e 'This is a PDF file of an unedited manuscript that has been accepted for publication.' \
                                                  -e 'use of the JSTOR database indicates your acceptance' \
                                                  -e 'you may not download an entire issue of a journal' \
                                                  -e 'This article appeared in a journal published by Elsevier.' \
                                                  -e 'institutional repository. Authors requiring further information' \
                                                  -e 'This article has been accepted for publication and undergone full peer review' \
                                                  -e 'the copyediting, typesetting, pagination and proofreading process' \
                                                  -e 'Changes resulting from the publishing process, including peer review,' \
                                                  -e 'may lead to differences between this version and the Version of Record.' )
                  if [[ -n "$PREPRINT" ]]; then
                      echo "$HOME/wiki/$PDF: preprint or front-page garbage ($PREPRINT)"
                  fi

                  if (( $TEXT_LENGTH < 1024 )); then
                      echo "$HOME/wiki/$PDF OCR text length: $TEXT_LENGTH"
                  fi
                  if [[ -z $TITLE || -z $AUTHOR || -z $DATE || -z $DOI ]]; then
                      exiftool -Title -Author -Date -DOI ~/wiki/"$PDF"
                  fi
              done; }
        wrap λ "Linked PDFs have missing OCR or missing metadata fields"

        # Finally, check for broken external links; ignore local URLs which are usually either relative links to
        # other pages in the repo or may be interwiki links like '!Wikipedia'.
        λ() { linkchecker --no-status --check-extern --threads=1 --timeout=20 -r1 --ignore='file://.*' "$HTML"; }
        wrap λ "Broken links"
    fi
    if [[ $PAGE == *.sh ]]; then
        shellcheck "$PAGE"
    fi
    if [[ $PAGE == *.hs ]]; then
        hlint "$PAGE"
    fi
    if [[ $PAGE == *.html ]]; then
        tidy -quiet -errors --doctype html5 "$PAGE"
    fi
done
