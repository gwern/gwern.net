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
                ".inflation-adjusted .subsup",
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
        ${(_π("body.reader-mode-active #markdownBody .spawns-",
              [ "popup", "popin" ]
              ).join(",\n"))} {
            margin: 0;
            padding: 0;
        }
        ${(_π("body.reader-mode-active #markdownBody .spawns-",
              [ "popup", "popin" ],
              " .indicator-hook::before"
              ).join(",\n"))} {
            padding-left: 0.3em;
            box-shadow:
                -0.17em 0.05em 0 0 var(--GW-reader-mode-masked-link-bracket-background-color),
                -0.17em -0.05em 0 0 var(--GW-reader-mode-masked-link-bracket-background-color),
                -0.17em 0 0 0 var(--GW-reader-mode-masked-link-bracket-background-color);
			background-image:
				linear-gradient(var(--GW-link-underline-background-color),
								var(--GW-link-underline-background-color)),
				linear-gradient(var(--GW-link-underline-gradient-line-color),
								var(--GW-link-underline-gradient-line-color)),
				linear-gradient(var(--GW-link-underline-gradient-line-color),
								var(--GW-link-underline-gradient-line-color));
			background-size:
       			1px  0.5em,
				100% 1px,
				1px  calc(0.75em - 0.1em);
			background-position:
				0 0.1em,
				0 calc(100% - 0.1em),
				0 calc(100% - 0.1em);
        }
        ${(_π("body.reader-mode-active.masked-links-hidden #markdownBody .spawns-",
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
        body.reader-mode-active.masked-links-hidden #markdownBody a:link,
        body.reader-mode-active.masked-links-hidden #markdownBody a:link * {
            text-shadow: none;
        }
        body.reader-mode-active.masked-links-hidden #markdownBody a code {
            border-bottom-width: 1px;
        }
        /*	Citations.
         */
		body.reader-mode-active .cite-joiner {
			display: initial;
		}
		body.reader-mode-active .cite-author + .cite-date::before {
			content: " ";
		}
		body.reader-mode-active .cite-author-plural::after {
			content: none;
		}
		body.reader-mode-active .cite-date {
			vertical-align: unset;
			font-size: unset;
			line-height: unset;
			font-variant-numeric: unset;
			margin-left: unset;
		}
        /*	Mobile layout adjustments.
         */
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
                margin: auto;
            }
            body.reader-mode-active #sidebar #logo::before {
                content: "";
                position: absolute;
                width: 100%;
                height: 0;
                border-bottom: 1px dotted var(--GW-bottom-ornament-line-color);
                left: 0;
                top: 50%;
                z-index: -1;
            }
            body.reader-mode-active header h1 {
                margin: 0.625em 0 0.375em 0;
            }
        }
    `,

	readerModeTitleNote: " (reader mode)",

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

		//	Update document title.
		document.title += ReaderMode.readerModeTitleNote;
    },
};

//  Inject initial style block.
ReaderMode.injectStyleBlock(ReaderMode.styles, "reader-mode-styles");

//  Activate saved mode, once the <body> element is loaded (and classes known).
doWhenBodyExists(ReaderMode.setMode);
