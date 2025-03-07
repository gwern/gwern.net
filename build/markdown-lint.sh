#!/bin/bash
# When:  Time-stamp: "2025-03-06 20:33:58 gwern"
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

        λ(){ fgp -e 'http://dl.dropbox' -e '.wiley.com/doi/abs/'  \
                 -e 'www.tandfonline.com/doi/abs/' -e 'jstor.org' -e 'springer.com' -e 'springerlink.com' \
                 -e 'www.mendeley.com' -e 'academia.edu' -e 'researchgate.net' -e 'pdf.yt' \
                 -e 'photobucket' -e 'imgur.com' -e 'hathitrust.org' -e 'emilkirkegaard.dk' -e 'arthurjensen.net' \
                 -e 'humanvarieties.org' -e 'libgen.io/' -e 'gen.lib.rus.ec/' -e 'sci-hub.bz/' -e '](http://www.scilogs.com/' \
                 -e 'sci-hub.cc/' -e "papers.nber.org/" -e '](!wikipedia' -e '](!wikipedia)'"'s" -e 'https://wwww.' -e 'http://wwww.' \
                 -e 'http://33bits.org' -e 'https://gwern.net' -e 'https://gwern.net' -e 'web.archive.org/web/2' \
                 -e 'webarchive.org.uk/wayback/' -e 'webcitation.org' -e 'plus.google.com' -e 'www.deepdotweb.com' -e 'raikoth.net' \
                 -e 'drive.google.com/' -e 'ssrn.com' -e 'ardenm.us' -e 'gnxp.nofe.me' -e 'psycnet.apa.org' \
                 -e 'wellcomelibrary.org/item/' -e 'dlcs.io/pdf/' -e 'secure.wikimedia.org' \
                 -e 'https://biorxiv.org' \
                 -e 'fbclid=' -e '?gid=' -e 'x.com/#!' -e 'pay.reddit.com' -e 'europepmc.org' -e 'drugcite.com' \
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

        λ(){ egp -e 'http://www.pnas.org/content/.*/.*/.*.abstract' -e '[^\.]t\.test\(' -e '^\~\~\{\.' \
                 -e 'ncbi.nlm.nih.gov/pubmed/[[:digit:]][[:digit:]]*' \
                 -e 'biorxiv.org/content/biorxiv/.*\.pdf ' -e '(https://www.biorxiv.org/content/biorxiv/.*\.pdf)' \
                 -e 'arxiv.org/pdf/.*\.pdf)' -e 'arxiv.org/pdf/.*\.pdf "'  -- "$PAGE"; }
        wrap λ "if I am not linking a specific page on Arxiv or BioRxiv, why am I linking to the PDF rather than the landing page?"

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

        λ(){ grep -F '~~~{.' -- "$PAGE" | tr -d '{}~' | tr ' ' '\n' | \
                 grep -F -v -e '.R' -e '.collapse' -e '.Haskell' -e '.Bash' -e '.Diff' -e '.Javascript' -e '.numberLines' \
                       -e '.Python' -e '.C ' -e '.CPO' -e '.SQL' -e '.Bibtex' -e '.HTML' -e '.CSS'; }
        wrap λ "look for potentially broken syntax-highlighting classes"

        λ(){ grep -E --invert-match '[[:space:]]*>' -- "$PAGE" | fgp -e ' significant ' -e ' significantly ' -e ' obvious' -e 'basically' -e ' the the ' -e 'reproducibility crisis' -e 'replicability crisis'; } # WARNING: can't use 'egp' for some reason
        wrap λ "look for personal uses of illegitimate statistics & weasel words, but filter out blockquotes"

        λ(){ fgp -e ' feet' -e ' foot ' -e ' pound ' -e ' mile ' -e ' miles ' -e ' inch' -- "$PAGE";
             egp -e '[0-9][0-9]* \(January\|February\|March\|April\|May\|June\|July\|August\|September\|October\|November\|December\) [0-9][0-9][0-9][0-9]' -- "$PAGE"; }
        wrap λ "look for English/imperial units as a reminder to switch to metric as much as possible"

        λ(){ egp '^[^$]* [^\"]\$[^$]*$' -- "$PAGE"; }
        wrap λ "look for unescaped single dollar-signs (risk of future breakage)"

        λ(){ egp -e '[a-zA-Z]- '  -- "$PAGE"; }
        wrap λ "Write out shortcuts"

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

        ## reused later as well:
        HTML=$(mktemp  --suffix=".html")
        cat "$PAGE" | pandoc --metadata lang=en --metadata title="Test" --mathml --to=html5 --standalone --number-sections --toc --reference-links --css=https://gwern.net/static/css/default.css -f markdown+smart --template=/home/gwern/bin/bin/pandoc-template-html5-articleedit.html5 - --output="$HTML"

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

    fi

done
