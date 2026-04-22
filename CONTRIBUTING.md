External contributor rules for the Gwern.net source code repo.
(Contributions to the website content are not covered by this repo or CONTRIBUTING, and should be sent to Gwern Branwen directly.)

1. **Security disclosure**: Security issues should not be filed publicly; for security issues or contributions, please [contact Gwern Branwen directly](https://gwern.net/me#contact).
2. **Licensing**: all contributions must be licensed under the same terms as the file they modify; generally, [CC-0](https://creativecommons.org/public-domain/cc0/ "‘CC-0: Creative Commons public domain license’, Commons 2002"), or MIT where CC-0 is legally problematic

    By opening a PR, you affirm you have the right to license the contribution under the appropriate license, including any rights that might be claimed by an employer or AI tool provider. If you are unsure about the applicable license, open an issue first.
3. **Issue-first**: substantial PRs should have an issue filed first for discussion; trivial fixes (typos, obvious bugs) can go straight to PR.
4. **Clean style code** (see [the Gwern.net Manual of Style](https://gwern.net/style-guide) in general for source code guidelines):

    - *no runtime warnings*: default compilation/execution should never print warnings/errors (eg. new CSS & JS should not trigger browser console warnings in the latest Chromium/Firefox, PHP/R code should not print warnings when run on Gwern's Ubuntu Linux LTS OS)
    - no *compile-time warnings*: further, all source code must be `-Wall`-clean or explicitly whitelisted as relevant for each language:

        - `Haskell`: should be `ghc -Wall`-clean and [hlint-clean](https://github.com/ndmitchell/hlint) (some hlint rules are disabled in `.hlint.yaml`; all remaining rules should be valid)
        - `Bash`: should be [shellcheck-clean](https://github.com/koalaman/shellcheck)
        - `Elisp`: byte-compile without warnings
        - `HTML`: [HTML Tidy](https://www.html-tidy.org/) & [W3 Validator](https://validator.w3.org/) (but only a subset)
        <!-- TODO: linters for CSS, JS, R, and Python -->
    - *no editing generated files* directly: all `*-VERSIONED.ext`, `*-GENERATED.ext` etc. files should be touched only by the PHP scripts which generate them via the pre-commit hook.
5. **House style**: all contributions should follow language-specific guidelines (see again the MoS). Currently:

    - Markdown/HTML
    - [Bash](https://gwern.net/style-guide#bash)
    - [Haskell](https://gwern.net/style-guide#haskell)
6. **Document the verification**: all external contributions must mention if and how a contribution has been tested and validated

    (An acceptable answer would look like "The Haskell compiled without warnings and [`Test.testAll`](https://gwern.net/static/build/Test.hs) reported no errors anywhere, including the new unit tests." or "the new JS file with this test HTML page ran successfully on my laptop Firefox-149.0.2 on Ubuntu 24.04.4 LTS." "Tested locally" is inadequate; did you evaluate a JS function call in a browser console? Fully compile and run the site on a remote web server and verified by browsing?)
7. **AI contribution requirements**: AI-generated contributions are accepted; but:

    - *metadata*: the tools & version used must be mentioned
    - *highest quality AI*: contributions must have been reviewed by a frontier model from a major lab (Anthropic, OpenAI, Google DeepMind, etc.) at roughly the highest capability of their flagship offering
    - *human-operated*: there must be an ultimately responsible human who endorses contributions and accepts blame; 'fully' autonomous contributions are disallowed. (Agent-generated PRs with no evidence of human reading of the diff will be closed without review.)
8. **Human accountability**: the ultimately responsible human is fully responsible for any contributions (AI or otherwise)

    They may be banned from the repo if a contribution has serious errors (eg. fabricated citations, inserted malware/exfiltration, license violations, silent data deletion, repeated submissions of unreviewed AI output...), or just at my sole discretion.
    Bans are not appealable.
9. **PR Rejection Policy**: We get to things when we get to them. We will explicitly reject PRs which are unacceptable. Pinging after 30 days is fine.

