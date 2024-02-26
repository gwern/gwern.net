/*************************************/
/*  Provides link decoration services.

    To add new linkicon associations (i.e., to assign a new or existing icon to
    a new category of links), modify one of the arrays in the latter part of
    this file (either Links.targetLinkTypes, for assigning a linkicon to links
    by domain / URL pattern, or Links.fileLinkTypes, for assigning a linkicon to
    links by file extension).
 */
Links = {
    /**********************/
    /*  Core functionality.
     */

    /*  Applies suitable linkicon classes to all links within the given
        container.
     */
    //  Called by: rewrite.js
    decorateLinksWithin: (doc) => {
        /*  Track which graphical linkicons we’re making use of in this
            container (so we can add them to the style block later).
         */
        let newLinkIcons = [ ];

        doc.querySelectorAll("a").forEach(link => {
            if (link.classList.contains("icon-not"))
                return;

            let iconInfo = Links.linkIconInfo(link);
            if (   iconInfo.type
                && iconInfo.icon) {
                link.dataset.linkIconType = iconInfo.type;
                link.dataset.linkIcon = iconInfo.icon;

                if (iconInfo.type == "svg")
                    newLinkIcons.push(iconInfo.icon);
            }
        });

        /*  If we’re using any graphical linkicons in this container, add them
            to the style block now.
         */
        if (newLinkIcons > [])
            Links.addGraphicalLinkIcons(newLinkIcons);
    },

    /*  All the graphical linkicons used on the page so far. Each entry in this
        array is a linkicon name. (See the entries of type ‘svg’ in the
        Links.fileLinkTypes and Links.targetLinkTypes arrays, below, for what
        values these linkicon names can have.
     */
    graphicalLinkIcons: [ ],

    /*  The style block that maps SVG linkicon names (which are associated with
        link elements by means of the `data-link-icon` attribute) with SVG icon
        file URLs, by means of a CSS variable (`--link-icon-url`) scoped to
        links with the icon name as the value of their `data-link-icon`
        attribute. In other words, the style block contains a series of lines
        such as the following (the example is for an SVG linkicon with the name
        ‘worddoc’):

        a[data-link-icon='worddoc'] { --link-icon-url: url('/static/img/icon/worddoc.svg'); }

        And thus for every SVG linkicon used on the page.

        (Note that this would not be necessary if the CSS3 `attr()` function
         were usable for all properties (i.e. https://caniuse.com/css3-attr );
         in such a case we could do for SVG linkicons what we already do for
         textual linkicons, i.e. refer directly to the `data-link-icon`
         attribute in the CSS. Alas, currently there is no browser support for
         this feature...)
     */
    graphicalLinkIconsStyleBlock: null,

    /*  Adds the given graphical linkicons to the style block. The newLinkIcons
        argument is an array of SVG linkicon names. (This function also injects
        the style block, if not already present.)
     */
    //  Called by: Links.decorateLinksWithin
    addGraphicalLinkIcons: (newLinkIcons) => {
        /*  Only do anything to the style block if we have actually added any
            linkicons (i.e. do a union of the existing linkicon array with the
            new addition, and check for a changed count); otherwise, no need
            to touch the style block and cause unnecessary re-rendering.
         */
        let oldCount = Links.graphicalLinkIcons.length;
        Links.graphicalLinkIcons = [...new Set([...(Links.graphicalLinkIcons), ...newLinkIcons])];
        if (Links.graphicalLinkIcons.length == oldCount)
            return;

        //  Inject the style block, if need be.
        if (Links.graphicalLinkIconsStyleBlock == null) {
            document.body.insertAdjacentHTML("beforeend", `<style id="graphical-link-icons"></style>`);
            Links.graphicalLinkIconsStyleBlock = document.querySelector("#graphical-link-icons");
        }

        /*  Generate the CSS for all the linkicons, and inject it into the style
            block.
         */
        Links.graphicalLinkIconsStyleBlock.innerHTML = Links.graphicalLinkIcons.map(icon =>
            `a[data-link-icon='${icon}'] { --link-icon-url: url('/static/img/icon/${icon}.svg'); }`
        ).join("\n");
    },

    /*  Returns an info object that specifies linkicon and linkicon type for
        the given link. The info object has fields ‘type’ and ‘icon’, both of
        which have string values. (See the Links.fileLinkTypes and
        Links.targetLinkTypes arrays for details on what the values of these
        fields can be, for what sorts of links.)

        Note that this function first checks whether the link is matched by one
        of the defined target type patterns (see the Links.targetLinkTypes
        array). Only if the link does not match any of those patterns does this
        function then check whether the link is to one of the defined file types
        (see the Links.fileLinkTypes array). (See explanation of the gwern.net
        linkicon philosophy at https://gwern.net/lorem#link-icons for more
        information on linkicon precedence.)
     */
    //  Called by: Links.decorateLinksWithin
    linkIconInfo: (link) => {
        let linkTargetType = Links.linkTargetType(link);
        if (linkTargetType)
            return linkTargetType;

        let linkFileType = Links.linkFileType(link);
        if (linkFileType)
            return linkFileType;

        return { };
    },

    /*********************************************/
    /*  Icons for links to certain sorts of files.
     */

    /*  Returns true iff the given link points to a file with the given
        extension (if `ext` is a string) or any of the given extensions (if
        `ext` is an array of strings). (In other words, returns true if the
        link’s pathname ends with a period followed by the given string / one
        of the given strings.)
     */
    //  Called by: Links.linkFileType
    //  Called by: some of the functions in Links.specialTargetTestFunctions
    linkFileExtensionMatches: (link, ext) => {
        return typeof ext == "string"
               ? link.pathname.toLowerCase().endsWith(`.${ext}`)
               : link.pathname.toLowerCase().endsWithAnyOf(ext.map(xt => `.${xt}`));
    },

    /*  Returns an info object that specifies link icon and link icon type for
        the given link, if the link is a link to one of the recognized file
        types. (Returns null otherwise.) The info object has fields ‘type’ and
        ‘icon’, both of which have string values. (See the Links.fileLinkTypes
        array for details on what the values of these fields can be.)
     */
    //  Called by: Links.linkIconInfo
    linkFileType: (link) => {
        /*  Various defined file types, classified by file extensions.
         */
        for (fileTypeDefinition of Links.fileLinkTypes) {
            [ fileType, iconType, extensions ] = fileTypeDefinition;
            if (Links.linkFileExtensionMatches(link, extensions.split(" ")))
                return { icon: fileType, type: iconType };
        }

        /*  PDF links, decorated by server code with a ‘link-pdf’ class.
         */
        if (link.classList.contains("link-pdf"))
            return { icon: "pdf", type: "svg" };

        /*  PHP files, but only if hosted locally (remote links with the ‘.php’
            extension are overwhelmingly likely to be dynamic web pages, rather
            than statically served PHP files).
         */
        if (   link.hostname == location.hostname
            && Links.linkFileExtensionMatches(link, "php"))
            return { icon: "code", type: "svg" };

        /*  HTML documents, but only if hosted locally _and_ located in the
            /static/ directory (remote links ending in ‘.html’ will be web pages
            all but a vanishingly tiny percentage of the time; likewise, local
            URLs ending in ‘.html’ that are outside the /static/ path are simply
            local pages.
         */
        if (   link.hostname == location.hostname
            && link.pathname.startsWith("/static/")
            && Links.linkFileExtensionMatches(link, "html"))
            return { icon: "code", type: "svg" };

        /*  Google Docs.
         */
        if (link.hostname == "docs.google.com")
            return { icon: "worddoc", type: "svg" };

        /*  Imgur links (except links to the Imgur front page itself).
         */
        if (   link.hostname == "imgur.com"
            && link.pathname > "")
            return { icon: "image", type: "svg" };

        return null;
    },

    /******************************************/
    /*  Icons for links to specific sites, etc.
     */

    /*  Returns true iff the given link matches the given pattern by hostname.

        Note that the hostname we classify links by is not necessarily hostname
        of the URL, per se, but the hostname of the site from which the content
        has been sourced (these two things will differ in the case of locally
        archived pages from other sites).

        The `pattern` argument may be a string or a RegExp. If a string, then
        an exact string match is checked for. If a RegExp, then a full pattern
        match is checked for (and capture groups, etc., are ignored).
     */
    //  Called by: Links.linkTargetType
    //  Called by: some of the functions in Links.specialTargetTestFunctions
    linkHostnameMatchesPattern: (link, pattern) => {
        //  Get the true hostname of the link’s content.
        let hostname = link.hostname;
        if (   link.hostname == location.hostname
            && link.pathname.startsWith("/doc/www/"))
            hostname = /\/doc\/www\/([^\/]+?)(\/.+)?$/.exec(link.pathname)[1];

        //  Hostname matched by regular expression.
        if (   pattern instanceof RegExp
            && pattern.test(hostname) == true)
            return true;

        //  Hostname exactly matched by string.
        if (hostname == pattern)
            return true;

        return false;
    },

    /*  Returns an info object that specifies link icon and link icon type for
        the given link, if the link matches one of the defined URL pattersn.
        (Returns null otherwise.) The info object has fields ‘type’ and ‘icon’,
        both of which have string values. (See the Links.fileLinkTypes array for
        details on what the values of these fields can be.)
     */
    //  Called by: Links.linkIconInfo
    linkTargetType: (link) => {
        for (targetTypeDefinition of Links.targetLinkTypes) {
            [ icon, type, urlPattern ] = targetTypeDefinition;

            if (   typeof urlPattern == "string"
                && urlPattern.startsWith("$")) {
                let testFunction = Links.specialTargetTestFunctions[urlPattern.substr(1)];
                if (   testFunction
                    && testFunction(link))
                    return { icon: icon, type: type };
            } else if (Links.linkHostnameMatchesPattern(link, urlPattern)) {
                return { icon: icon, type: type };
            }
        }

        return null;
    }
};

/************************************/
/*  LINKED FILE TYPE ICON DEFINITIONS

    Each row of this array defines or specifies an icon, specifies how the icon
    should be rendered, and lists a set of file extensions, to links to which
    the icon should be applied.

    Links are checked against each row’s file extension group in sequence; when
    a  match is found, the icon specified by that row is assigned to the link
    (and no more rows are checked). (If a match is not found in this array, then
    no linkicon data attributes are applied to the link.)

    Fields in each row are:

        [ ICON_NAME, ICON_TYPE, FILE_EXTENSIONS ]

    (All fields are required for each entry.)

    See comment on the Links.targetLinkTypes array, below, for information about
    the ICON_NAME and ICON_TYPE fields.

    FILE EXTENSIONS field values are strings, containing a space-separated list
    of file extensions (without the dot, i.e. `txt` and not `.txt`). A link
    whose pathname ends with a dot followed by one of the listed file extensions
    will be assigned the named icon. (File extensions should always be given in
    lowercase.)
 */
Links.fileLinkTypes = [
    //  Textfiles.
    [ "txt",            "svg",      "opml txt xml json jsonl md"            ],

    //  Code, scripts, etc.
    [ "code",           "svg",      "css hs js conf sh r patch diff"        ],

    //  Word (& compatible) documents.
    [ "worddoc",        "svg",      "doc docx"                              ],

    //  Excel (& compatible) documents.
    [ "spreadsheet",    "svg",      "xls xlsx ods"                          ],

    //  CSV files.
    [ "csv",            "svg",      "csv"                                   ],

    //  Images.
    [ "image",          "svg",      "gif bmp ico jpg jpeg png svg xcf"      ],

    //  Archive files.
    [ "audio",          "svg",      "mp3 wav flac ogg rm"                   ],

    //  Video files.
    [ "file-video",     "svg",      "swf mp4 mkv webm"                      ],

    //  Archive files.
    [ "archive",        "svg",      "tar zip xz img bin pkl onnx pt"        ],

    //  Miscellaneous files (for which there is no specific icon).
    [ "misc",           "svg",      "ebt mdb mht ttf"                       ],

    //  EPUB files.
    [ "EPUB",           "text,sans,quad",       "epub"                      ],
];

/*******************************/
/*  LINK TARGET ICON DEFINITIONS

    Each row of this array defines or specifies an icon, specifies how the icon
    should be rendered, and provides a link target matching pattern that assigns
    the icon to a specific category of links (usually, links to websites/pages
    on a certain domain).

    Links are checked against each row’s matching pattern in sequence; when a
    match is found, the icon specified by that row is assigned to the link (and
    no more rows are checked). (If a match is not found in this array, icon
    assignment falls back to the file type array (Links.fileLinkTypes).)

    Fields in each row are:

        [ ICON_NAME, ICON_TYPE, URL_PATTERN ]

    (All fields are required for each entry.)

    ICON NAMES are strings; they specify the characters of a text icon, or the
    name of an SVG icon.

    ICON TYPES are comma-separated lists of type tags. These specify how the
    icon should be rendered. Available type tags include the following:

        text, bold, italic, quad, sans, overline, svg

    One of either ‘text’ or ‘svg’ must be present, otherwise the icon will not
    be rendered.

    (Note that not all type tag combinations are valid; for instance, an icon
     type of "text,svg" is invalid, as an icon may be a text icon or an SVG
     icon, but not both. Specifying an invalid combination of type tags results
     in undefined behavior.)

    URL PATTERNS may be one of three things:

    1. A string specifying a domain name (in which case exact string match
       against the link’s hostname is checked for).

    2. A RegExp (which is tested for full-pattern match against the link’s
       hostname; capture groups, etc., are ignored).

    3. A key into the Links.specialTargetTestFunctions dictionary (designated by
       a string starting with ‘$’; the actual key is the string with the ‘$’
       prefix omitted), in which case the link is passed to the specified
       function, which returns true or false.

    The first way (string) is preferred if a single, exact domain can be
    specified, because string comparison is faster than either regular
    expression parsing and matching or (most likely) a function call.
 */
Links.targetLinkTypes = [
    /***********************************************/
    /*  High-priority URL patterns get listed first.
     */

    //  DeepMind; match articles or anchors about DM too.
    //  primary user: deepmind.com
    [ "deepmind",           "svg",                  "$DEEPMIND"                         ],

    /*  Microsoft: I don’t think
        https://en.wikipedia.org/wiki/File:Microsoft_logo_(2012).svg
        is all that recognizable, so make a logotype more like
        https://en.wikipedia.org/wiki/File:Microsoft_logo_(1987).svg :
        an italic sans "MS".
     */
    [ "MS",             "text,sans,italic",         "$MICROSOFT"                        ],

    //  Nvidia: https://en.wikipedia.org/wiki/Nvidia#cite_note-2 yeah no
    [ "N",              "text",                     "$NVIDIA"                           ],

    //  OpenAI; match articles or anchors about OA too.
    //  primary user: openai.com
    [ "openai",             "svg",                  "$OPEN_AI"                          ],

    //  Facebook.
    [ "facebook",           "svg",                  "$FACEBOOK"                         ],

    //  Google.
    [ "google",             "svg",                  "$GOOGLE"                           ],

    /**************************/
    /*  Unicode trickery icons.
     */

    //  (ψ) GREEK SMALL LETTER PSI
    [ "\u{03C8}",       "text",                     "psyarxiv.com"                      ],

    //  SSC’s book
    /*  (ℵ) ALEF SYMBOL (We use the math symbol instead of the Hebrew
        deliberately to avoid triggering bizarre Hebrew bidirectional
        text-related layout bugs on Mac Firefox.)
     */
    [ "\u{2135}",       "text",                     "unsongbook.com"                    ],

    /*  Favicon is a little normal distribution/histogram
        (▅▇▃) LOWER FIVE EIGHTHS BLOCK, LOWER SEVEN EIGHTHS BLOCK,
              LOWER THREE EIGHTHS BLOCK
     */
    [ "\u{2585}\u{2587}\u{2583}",   "text",         "andrewgelman.com"                  ],
    [ "\u{2585}\u{2587}\u{2583}",   "text",         "statmodeling.stat.columbia.edu"    ],

    //  Kevin Simler’s Melting Asphalt blog
    //  (▲) BLACK UP-POINTING TRIANGLE
    [ "\u{25B2}",       "text",                     "meltingasphalt.com"                ],

    //  (♘) WHITE CHESS KNIGHT
    [ "\u{2658}",       "text",                     "link.springer.com"                 ],

    /*  TinyLetter’s icon, without color, isn’t memorable enough; throw in the
        other email services
        (✉) ENVELOPE
     */
    [ "\u{2709}",       "text",                     "www.tinyletter.com"                ],
    [ "\u{2709}",       "text",                     "groups.google.com"                 ],
    [ "\u{2709}",       "text",                     "groups.yahoo.com"                  ],
    [ "\u{2709}",       "text",                     "www.mail-archive.com"              ],

    //  Bloomberg: no usable logo, just an inset-B
    //  (𝐁) MATHEMATICAL BOLD CAPITAL B
    [ "\u{1D401}",      "text",                     /(.+\.)?bloomberg.com$/             ],

    //  Medium: cheaper to abuse Unicode
    //  (𝐌) MATHEMATICAL BOLD CAPITAL M
    [ "\u{1D40C}",      "text",                     "medium.com"                        ],

    //  MR: cheaper to abuse Unicode
    //  (𝐑) MATHEMATICAL BOLD CAPITAL R
    [ "M\u{1D411}",     "text",                     "marginalrevolution.com"            ],

    /*  Haskell: simplify logo; the double-lambda is too busy when used for link
        icons
        (𝛌) MATHEMATICAL BOLD SMALL LAMDA
        primary user: hackage.haskell.org
     */
    [ "\u{1D6CC}",      "text",                     "$HASKELL_DOT_ORG"                  ],

    /*  ArXiv: Their skull+smiley logo is too bizarre & off-putting to use, in
        addition to not working as a tiny monochrome image
        (𝛘) MATHEMATICAL BOLD SMALL CHI (bold makes it show up better when tiny)
     */
    [ "\u{1D6D8}",      "text",                     "arxiv.org"                         ],
    [ "\u{1D6D8}",      "text",                     "browse.arxiv.org"                  ],

    /******************************************************/
    /*  Icons made of one or more regular ASCII characters.
     */

    //  The Atlantic: replicate sloping by italics
    [ "A",              "text,italic",              /(.+\.)?theatlantic.com$/           ],

    [ "AF",             "text",                     "$ALIGNMENT_FORUM"                  ],

    [ "ANN",            "text,sans",                /(.+\.)?animenewsnetwork.com$/      ],

    //  Ars is an orange box, not usable
    [ "ars",            "text,sans",                "arstechnica.com"                   ],

    //  BBC: no usable logo
    [ "BBC",            "text,sans",                /(.+\.)?bbc.com$/                   ],
    [ "BBC",            "text,sans",                /(.+\.)?bbc.co.uk$/                 ],

    //  British Medical Journal or just ‘bmj’
    [ "bmj",            "text,sans",                /(.+\.)?bmj.com$/                   ],

    [ "CDC",            "text",                     "www.cdc.gov"                       ],

    //  US federal Department of Justice
    [ "DoJ",            "text",                     /(.+\.)?justice.gov$/               ],

    [ "E",              "text,italic",              "www.edge.org"                      ],

    //  Economist: logo is just ‘Economist’...
    [ "E",              "text,sans",                /(.+\.)?economist.com$/             ],

    //  Elsevier/Sciencedirect.com: also an ‘E’
    [ "E",              "text",                     /(.+\.)?sciencedirect.com$/         ],

    //  Evangelion: we’ll split this into EGF-related and other NGE sites
    //  Primary user: eva.onegeek.org
    [ "EG",             "text",                     /(.+\.)?evageeks.org$/              ],
    [ "EG",             "text",                     /(.+\.)?evamonkey.com$/             ],

    //  Filfre.net/The Digital Antiquarian has no logo or usable substitute...
    [ "F",              "text",                     /(.+\.)?filfre.net$/                ],

    //  U.S. Food & Drug Administration
    [ "FDA",            "text,sans",                /(.+\.)?fda.gov$/                   ],

    /*  The FF.net logo is pretty crazy, and I don’t think anyone would
        recognize it in monochrome
     */
    [ "FF",             "text",                     "www.fanfiction.net"                ],

    //  GoodReads: logo doesn’t make sense as a grayscale
    [ "GR",             "text",                     /(.+\.)?goodreads.com$/             ],

    //  The Harney & Sons logo is too fancy to scale down reasonably
    [ "H",              "text",                     /(.+\.)?harney.com$/                ],

    //  Kevin Kelly
    [ "KK",             "text,sans",                "kk.org"                            ],

    //  LW logo is just a colored ‘LW’, so no point in converting
    //  other user: wiki.lesswrong.com
    [ "LW",             "text",                     /(.+\.)?lesswrong.com$/             ],
    [ "LW",             "text",                     "www.greaterwrong.com"              ],

    //  MAL: the blue of their logo doesn’t work, so just text
    [ "MAL",            "text,sans",                "myanimelist.net"                   ],

    [ "MJ",             "text,sans",                "www.motherjones.com"               ],

    //  Nature
    [ "n",              "text",                     /(.+\.)?nature.com$/                ],

    //  Primary user: forum.evageeks.org wiki.evageeks.org
    [ "NGE",            "text",                     /(.+\.)?onegeek.org$/               ],
    [ "NGE",            "text",                     "eva-fan.com"                       ],
    [ "NGE",            "text",                     "evaotaku.com"                      ],
    [ "NGE",            "text",                     "khara.co.jp"                       ],
    [ "NGE",            "text",                     "gainax.co.jp"                      ],
    [ "NGE",            "text",                     "17th-angel.tumblr.com"             ],

    //  OB logo too bad to use
    [ "OB",             "text",                     "www.overcomingbias.com"            ],

    //  Oxford Academic Journals / OUP
    [ "OUP",            "text",                     "academic.oup.com"                  ],

    [ "P@D",            "text",                     "poniesatdawn.bandcamp.com"         ],

    //  The Paris Review: not even going to try to make their weird bird logo work
    [ "PR",             "text",                     /(.+\.)?theparisreview.org$/        ],

    /*  R: at this point R Studio has taken over a lot of control of the R
        ecosystem, so might as well treat them as official too...
        primary user: cran.r-project.org
     */
    [ "R",              "text",                     /(.+\.)?r-project.org$/             ],
    [ "R",              "text",                     /(.+\.)?rstudio.com$/               ],

    //  Science is just typeset in red
    [ "S",              "text",                     /(.+\.)?science.org$/               ],
    [ "S",              "text",                     /(.+\.)?sciencemag.org$/            ],

    [ "S",              "text,sans",                "slate.com"                         ],

    [ "s",              "text",                     /(.+\.)?salon.com$/                 ],

    //  Avoid the unfortunate connotations of ‘SS’
    [ "Ss",             "text",                     "scholars-stage.org"                ],

    //  SSC logo too bad to use
    //  primary user: slatestarcodex.com
    [ "SSC",            "text",                     "$SLATE_STAR_CODEX"                 ],

    /*  Technology Review (their logo has a little slash in it which you
        probably can’t see at low-res) but is otherwise just a ‘T’ so meh
     */
    [ "T",              "text,sans",                /(.+\.)?technologyreview.com$/      ],

    //  TV Tropes: their lampshade icon is unrecognizable & hard to see small
    [ "TV",             "text",                     "tvtropes.org"                      ],

    //  Gene Wolfe mailing list; no logo
    //  primary user: lists.urth.net
    [ "U",              "text",                     /(.+\.)?urth.net$/                  ],

    [ "VF",             "text",                     "www.vanityfair.com"                ],

    [ "Vox",            "text,italic",              "www.vox.com"                       ],

    /*  Wiley & Sons’s ‘W’ unfortunately overlaps with the WP ‘W’ but if we sans
        it, maybe that’ll help
        primary user: onlinelibrary.wiley.com
     */
    [ "W",              "text,sans",                /(.+\.)?wiley.com$/                 ],

    //  The Wall Street Journal
    [ "WSJ",            "text",                     /(.+\.)?wsj.com$/                   ],

    //  Long Now Foundation projects
    [ "X",              "text,overline",            /(.+\.)?longbets.org$/              ],
    [ "X",              "text,overline",            /(.+\.)?longnow.org$/               ],
    [ "X",              "text,overline",            /(.+\.)?rosettaproject.org$/        ],
    [ "X",              "text,overline",            /(.+\.)?theinterval.org$/           ],

    [ "ys",             "text",                     "yunnansourcing.com"                ],

    //  PB logo is confusing. A purple question mark...?
    [ "?",              "text,sans,bold",           "predictionbook.com"                ],

    /****************************/
    /*  Quad-letter-square icons.
     */

    //  Cell: their logo is unrecognizable (and dumb)
    [ "CELL",           "text,quad,sans",           "www.cell.com"                      ],

    [ "MLPW",           "text,quad,sans,italic",    "mlp.fandom.com"                    ],

    [ "NBER",           "text,quad",                "$NBER"                             ],

    /*  PNAS: they don’t have a real logo, but their favicon does a nice little
        compact square (white text on blue background), and we can replicate
        that in CSS (but just as black text on white background,
        per our monochrome theme)
     */
    [ "PNAS",           "text,quad",                /(.+\.)?pnas.org$/                  ],

    [ "RAND",           "text,quad,sans",           /(.+\.)?rand.org$/                  ],

    //  Sage Journals’s logo is a circled S... but would anyone recognize it?
    //  primary user: journals.sagepub.com
    [ "SAGE",           "text,quad,sans",           /(.+\.)?sagepub.com$/               ],

    [ "TPDR",           "text,quad",                "publicdomainreview.org"            ],

    [ "XKCD",           "text,quad,sans",           /(.+\.)?xkcd.com$/                  ],

    /*************/
    /*  SVG icons.
     */

    //  Amazon.
    [ "amazon",             "svg",                  /(.+\.)?amazon.com$/                ],
    [ "amazon",             "svg",                  /amazon.co.[^\.]+$/                 ],

    //  Bitcoin.
    //  primary user: en.bitcoin.it
    [ "bitcoin",            "svg",                  /(.+\.)?bitcoin.it$/                ],
    [ "bitcoin",            "svg",                  "bitcointalk.org"                   ],

    //  BioRxiv (custom icon: italic Chi with DNA cross-strands).
    [ "chi-dna",            "svg",                  /(.+\.)?biorxiv.org$/               ],
    [ "chi-dna",            "svg",                  /(.+\.)?medrxiv.org$/               ],

    //  Distill ML journal.
    [ "distillpub",         "svg",                  "distill.pub"                       ],

    /*  Dropbox: old file-host, deprecated since they’ve started killing
        inactive accounts
        primary user: dl.dropboxusercontent.com
     */
    [ "dropbox",            "svg",                  /(.+\.)?dropbox.com$/               ],
    [ "dropbox",            "svg",                  /(.+\.)?dropboxusercontent.com$/    ],

    //  Erowid.
    [ "erowid",             "svg",                  /(.+\.)?erowid.org$/                ],

    //  Github; I exclude github.io because that’s blogs.
    [ "github",             "svg",                  /(.+\.)?github.com$/                ],

    //  Google Scholar.
    [ "google-scholar",     "svg",                  "scholar.google.com"                ],

    //  PG/HN/YC (shared logo).
    //  primary user: news.ycombinator.com
    [ "hn",                 "svg",                  /(.+\.)?paulgraham.com$/            ],
    [ "hn",                 "svg",                  /(.+\.)?ycombinator.com$/           ],

    //  Web archives.
    //  primary user: web.archive.org
    //  secondary user: replay.waybackmachine.org
    [ "internetarchive",    "svg",                  /(.+\.)?webcitation.org$/           ],
    [ "internetarchive",    "svg",                  /(.+\.)?mementoweb.org$/            ],
    [ "internetarchive",    "svg",                  /(.+\.)?archive.org$/               ],
    [ "internetarchive",    "svg",                  /(.+\.)?archive-it.org$/            ],
    [ "internetarchive",    "svg",                  /(.+\.)?archiveteam.org$/           ],
    [ "internetarchive",    "svg",                  /(.+\.)?waybackmachine.org$/        ],
    [ "internetarchive",    "svg",                  "$LOCAL_ARCHIVE"                    ],

    //  MegaUpload/Mega: filesharing (used for big files).
    [ "mega",               "svg",                  "mega.nz"                           ],

    //  MIRI/intelligence.org.
    [ "miri",               "svg",                  "intelligence.org"                  ],

    //  The New York Times: reduction of full SVG logo to just the ‘T’ they use as an icon.
    [ "newyorktimes",       "svg",                  /(.+\.)?nytimes.com$/               ],

    /*  NCBI/Pubmed: simplification of their logo
        (https://upload.wikimedia.org/wikipedia/commons/0/07/US-NLM-NCBI-Logo.svg).
        primary user: ncbi.nlm.nih.gov
     */
    [ "nlm-ncbi",           "svg",                  /(.+\.)?nlm.nih.gov$/               ],

    /*  Patreon. (Used the old one
        (https://upload.wikimedia.org/wikipedia/commons/9/94/Patreon_logo.svg)
        because I don’t like the new one.)
     */
    [ "patreon",            "svg",                  /(.+\.)?patreon.com$/               ],

    //  PLOS ONE in all their domain permutations...
    //  primary user: journals.plos.org
    [ "plos",               "svg",                  /(.+\.)?plos.org$/                  ],
    [ "plos",               "svg",                  /(.+\.)?plosone.org$/               ],
    [ "plos",               "svg",                  /(.+\.)?plosbiology.org$/           ],
    [ "plos",               "svg",                  /(.+\.)?plosmedicine.org$/          ],

    //  Reddit.
    //  primary user: old.reddit.com
    [ "reddit",             "svg",                  /(.+\.)?reddit.com$/                ],

    //  The *Exchange/*Overflow family of websites.
    [ "stackexchange",      "svg",                  /(.+\.?)?overflow.net$/             ],
    [ "stackexchange",      "svg",                  /(.+\.?)?overflow.com$/             ],
    [ "stackexchange",      "svg",                  /(.+\.)?stackexchange.com$/         ],

    //  Substack.
    //  primary user: gwern.substack.com
    [ "substack",           "svg",                  /(.+\.)?substack.com$/              ],

    //  El Grauniad.
    [ "theguardian",        "svg",                  /(.+\.)?theguardian.com$/           ],
    [ "theguardian",        "svg",                  "www.guardian.co.uk"                ],

    //  The New Yorker: the Dandy SVG, simplified & rotated more vertically.
    [ "thenewyorker",       "svg",                  /(.+\.)?newyorker.com$/             ],

    //  Tumblr.
    [ "tumblr",             "svg",                  /(.+\.)?tumblr.com$/                ],

    //  Twitter.
    [ "twitter",            "svg",                  /(.+\.)?twitter.com$/               ],
    [ "twitter",            "svg",                  "nitter.net"                  ],

    //  Upton Tea.
    [ "uptontea",           "svg",                  /(.+\.)?uptontea.com$/              ],

    //  Bandcamp/SoundCloud links.
    [ "video",              "svg",                  /(.+\.)?soundcloud.com$/            ],
    [ "video",              "svg",                  /(.+\.)?bandcamp.com$/              ],

    //  The Washington Post: truncated their blackletter to ‘WP’.
    [ "washingtonpost",     "svg",                  /(.+\.)?washingtonpost.com$/        ],

    //  Wikipedia.
    //  primary user: en.wikipedia.org
    [ "wikipedia",          "svg",                  /(.+\.)?wikipedia.org$/             ],
    //  primary user: meta.wikimedia.org
    [ "wikipedia",          "svg",                  /(.+\.)?wikimedia.org$/             ],
    //  primary user: en.wiktionary.org
    [ "wikipedia",          "svg",                  /(.+\.)?wiktionary.org$/            ],
    //  primary user: en.wikisource.org
    [ "wikipedia",          "svg",                  /(.+\.)?wikisource.org$/            ],
    [ "wikipedia",          "svg",                  /(.+\.)?wikimediafoundation.org$/   ],

    //  Wired.
    [ "wired",              "svg",                  /(.+\.)?wired.com$/                 ],

    //  YouTube links.
    [ "youtube",            "svg",                  "www.youtube.com"                   ],
    [ "youtube",            "svg",                  "www.youtu.be"                      ],

//  [ "ICON_NAME",      "ICON_TYPE",                "URL_PATTERN"                       ],
];

/*  Some domains / sites require have special conditions for link icon
    assignment.
 */
Links.specialTargetTestFunctions = {
    //  Links to haskell.org, NOT including links to .hs code files
    "HASKELL_DOT_ORG": (link) => (   Links.linkHostnameMatchesPattern(link, /(.+\.)?haskell.org$/)
                                  && Links.linkFileExtensionMatches(link, "hs") == false),

    //  Either the Alignment Forum itself, or the AF view on GreaterWrong
    "ALIGNMENT_FORUM": (link) => (   Links.linkHostnameMatchesPattern(link, /(.+\.)?alignmentforum.org$/)
                                  || (   Links.linkHostnameMatchesPattern(link, "www.greaterwrong.com")
                                      && link.searchParams.get("view") == "alignment-forum")),

    //  SPEAK THE DEVIL’S NAME AND HIS LINKICON SHALL APPEAR
    "MICROSOFT": (link) => (/microsoft/i).test(link.href),

    //  A lesser demon, but the same principle applies
    "NVIDIA": (link) => (/nvidia/i).test(link.href),

    //  Slate Star Codex & co.
    "SLATE_STAR_CODEX": (link) => (   (   Links.linkHostnameMatchesPattern(link, /(.+\.)?slatestarcodex.com$/)
                                       && !(link.classList.contains("link-pdf")))
                                   || (/slatestarcodex/i).test(link.href)
                                   || (/yvain/i).test(link.href)
                                   || Links.linkHostnameMatchesPattern(link, "slatestarscratchpad.tumblr.com")
                                   || Links.linkHostnameMatchesPattern(link, "www.astralcodexten.com")),

    //  Links to NBER, NOT including PDF file links
    "NBER": (link) => (   Links.linkHostnameMatchesPattern(link, "www.nber.org")
                       && !(link.classList.contains("link-pdf"))),

    //  All that gives voice to Deepmind is Deepmind
    "DEEPMIND": (link) => (/deepmind/i).test(link.href),

    //  Whosoever speaks the name of OpenAI shall make manifest its linkicon
    "OPEN_AI": (link) => (/openai/i).test(link.href),

    //  Locally archived documents / pages (but not the ones in PDF format)
    "LOCAL_ARCHIVE": (link) => (    link.classList.contains("local-archive-link")
                                && !link.classList.contains("link-pdf")),

    //  The Enemy wears many faces
    //  ... enough to fill a whole book, one might say
    "FACEBOOK": (link) => (   Links.linkHostnameMatchesPattern(link, /(.+\.)?facebook.com$/)
                           || link.hash.toLowerCase() == "#facebook"
                           || link.hash.toLowerCase().includes("org=facebook")),

    //  They say Don’t Be Evil... they wouldn’t lie, would they...?
    /*  Google searches, other tools.
        Note that there are many Google subdomains, which we may wish to iconify
        differently, so we narrow down with just ‘www’.

        Google Brain doesn’t have any consistent or recognizable logo, don’t
        bother trying to replicate one of the dots (no one will recognize it);
        use ‘GB’ would not be a bad idea, but I suspect that would also confuse
        people. So reusing the ‘G’ is the least bad option.
     */
    "GOOGLE": (link) => (   Links.linkHostnameMatchesPattern(link, "www.google.com")
                         || link.hash.toLowerCase() == "#google"
                         || link.hash.toLowerCase().includes("org=google")),

};
