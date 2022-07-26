ReaderMode = {
    active: false,

    styles: `
        ${(_π("body.reader-mode-active ",
              [ "#sidebar-links",
                "#page-metadata",
                "#TOC ul li::before",
                "#footer",
                "#footer-logo",
                "#navigation",
                "#sidenote-column-left",
                "#sidenote-column-right",
                ".inflationAdjusted .supsub",
                ".footnote-ref"
                ]
              ).join(",\n"))} {
            display: none;
        }
        body.reader-mode-active #logo {
            border-color: transparent;
        }
        @media only screen and (max-width: 649px) {
            body.reader-mode-active #TOC {
                margin-top: 1em;
            }
        }
        body.reader-mode-active #TOC ul li {
            padding-left: 0.125em;
        }
        ${(_π("body.reader-mode-active .spawns-",
              [ "popup", "popin" ]
              ).join(",\n"))} {
            margin: 0;
        }
        ${(_π("body.reader-mode-active .spawns-",
              [ "popup", "popin" ],
              " .indicator-hook"
              ).join(",\n"))} {
            padding-left: 0;
        }
        ${(_π("body.reader-mode-active .spawns-",
              [ "popup", "popin" ],
              " .indicator-hook::before"
              ).join(",\n"))} {
            left: -0.3em;
            box-shadow:
                -0.17em 0.05em 0 0 var(--GW-reader-mode-masked-link-bracket-background-color),
                -0.17em -0.05em 0 0 var(--GW-reader-mode-masked-link-bracket-background-color),
                -0.17em 0 0 0 var(--GW-reader-mode-masked-link-bracket-background-color);
        }
        ${(_π("body.reader-mode-active.masked-links-hidden .spawns-",
              [ "popup", "popin" ],
              " .indicator-hook"
              ).join(",\n"))},
        ${(_π("body.reader-mode-active #markdownBody ",
              [ "p", "li", "figcaption" ],
              " a::after"
              ).join(",\n"))} {
            display: none;
        }
        ${(_π("body.reader-mode-active.masked-links-hidden #markdownBody ",
              [ "p", "li", "figcaption" ],
              " a:not(.popup-open)",
              [ "", ":visited", ":hover" ]
              ).join(",\n"))} {
            color: inherit;
            background: none;
            cursor: text;
        }
        body.reader-mode-active.masked-links-hidden a:link,
        body.reader-mode-active.masked-links-hidden a:link * {
            text-shadow: none;
        }
        @media only screen and (max-width: 649px) {
            body.reader-mode-active #sidebar {
                position: relative;
                margin: 1em 0 0 0;
            }
            body.reader-mode-active #sidebar #logo {
                width: 100%;
                justify-content: center;
                margin: 0;
                padding: 0;
            }
            body.reader-mode-active #sidebar #logo svg {
                width: 1.5rem;
                background-color: var(--GW-body-background-color);
                padding: 0 10px 0 11px;
            }
            body.reader-mode-active #sidebar #logo::before {
                content: "";
                position: absolute;
                width: 100%;
                height: 0;
                border-bottom: 1px dotted var(--GW-index-page-bottom-ornament-line-color);
                left: 0;
                top: 50%;
                z-index: -1;
            }
            body.reader-mode-active header h1 {
                margin: 0.625em 0 0.375em 0;
            }
        }
    `,

    /*  Inject a style block with the given content and element ID.
     */
    //  Called by: this file (immediately on load)
    injectStyleBlock: (styleBlockContent, styleBlockID) => {
        document.querySelector("head").insertAdjacentHTML("beforeend", `<style id='${styleBlockID}'>${styleBlockContent}</style>`);
    },

    /*  Activate or deactivate reader mode, as determined by the current setting
        and the selected mode.
     */
    //  Called by: this file (doWhenBodyExists)
    //  Called by: ReaderMode.modeSelectButtonClicked (reader-mode.js)
    setMode: (selectedMode = ReaderMode.currentMode()) => {
        GWLog("ReaderMode.setMode", "reader-mode.js", 1);

        //  Activate (if needed).
        if (ReaderMode.enabled() == true)
            ReaderMode.activate();

		//	Fire event.
		GW.notificationCenter.fireEvent("ReaderMode.didSetMode");
    },

    /*  Returns true if reader mode is set to be enabled for the current page,
        false otherwise.
     */
    enabled: () => {
        let currentMode = ReaderMode.currentMode();
        return (   currentMode == "on"
                || (   currentMode == "auto"
                    && document.body.classList.contains("reader-mode")))
    },

    /*  Returns current (saved) mode (on, off, or auto).
     */
    currentMode: () => {
        return (localStorage.getItem("reader-mode-setting") || "auto");
    },

    /*  Masks links and hide other elements, as appropriate. This will hide
        linkicons and pop-frame indicators, and will thus cause reflow.
     */
    //  Called by: ReaderMode.setMode
    activate: () => {
        GWLog("ReaderMode.activate", "reader-mode.js", 1);

        ReaderMode.active = true;

        //  Add body classes.
        document.body.classList.add("reader-mode-active", "masked-links-hidden");
    },
};

//  Inject initial style block.
ReaderMode.injectStyleBlock(ReaderMode.styles, "reader-mode-styles");

//  Activate saved mode, once the <body> element is loaded (and classes known).
doWhenBodyExists(ReaderMode.setMode);
