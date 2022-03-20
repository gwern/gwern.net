ReaderMode = {
	active: false,

	styles: 
		_π("body.reader-mode-active ", [ 
		   "#sidebar-links",
		   "#page-metadata",
		   "#TOC ul li::before",
		   "#footer",
		   "#navigation",
		   "#sidenote-column-left",
		   "#sidenote-column-right",
		   ".inflationAdjusted .supsub"
		]).join(",\n") + ` {
			display: none;
		}` + `
		body.reader-mode-active #logo {
			border-color: transparent;
		}
		body.reader-mode-active #TOC {
			margin-top: 1em;
		}
		body.reader-mode-active #TOC ul li {
			padding-left: 0.125em;
		}
		` + _π("body.reader-mode-active .spawns-", [ "popup", "popin" ]).join(",\n") + ` {
			margin: 0;
		}
		` + _π("body.reader-mode-active .spawns-", [ "popup", "popin" ], " .indicator-hook").join(",\n") + ` {
			padding-left: 0;
		}
		` + _π("body.reader-mode-active .spawns-", [ "popup", "popin" ], " .indicator-hook::before").join(",\n") + ` {
			left: -0.3em;
			box-shadow:
				-0.17em 0.05em 0 0 var(--GW-reader-mode-masked-link-bracket-background-color),
				-0.17em -0.05em 0 0 var(--GW-reader-mode-masked-link-bracket-background-color),
				-0.17em 0 0 0 var(--GW-reader-mode-masked-link-bracket-background-color);
		}
		` + _π("body.reader-mode-active.masked-links-hidden .spawns-", [ "popup", "popin" ], " .indicator-hook").join(",\n") + ",\n" + 
			_π("body.reader-mode-active #markdownBody ", [ "p", "li" ], " a::after").join(",\n") + ` {
			display: none;
		}
		` + _π("body.reader-mode-active.masked-links-hidden #markdownBody ", [ "p", "li" ], " a:not(.popup-open)", [ "", ":visited", ":hover" ]).join(",\n") + ` {
			color: inherit;
			background: none;
			cursor: text;
		}
	`,

	/*	Inject a style block with the given content and element ID.
	 */
	//	Called by: this file (immediately on load)
	injectStyleBlock: (styleBlockContent, styleBlockID) => {
		document.querySelector("head").insertAdjacentHTML("beforeend", `<style id='${styleBlockID}'>${styleBlockContent}</style>`);
	},

	/*	Activate or deactivate reader mode, as determined by the current setting
		and the selected mode.
	 */
	//	Called by: this file (doWhenBodyExists)
	//	Called by: ReaderMode.modeSelectButtonClicked
	setMode: (selectedMode = ReaderMode.currentMode()) => {
		GWLog("ReaderMode.setMode", "reader-mode.js", 1);

		//	Activate (if needed).
		if (ReaderMode.enabled() == true)
			ReaderMode.activate();
	},

	/*	Returns true if reader mode is set to be enabled for the current page,
		false otherwise.
	 */
    enabled: () => {
		let currentMode = ReaderMode.currentMode();
        return (   currentMode == "on"
        		|| (   currentMode == "auto"
        			&& document.body.classList.contains("reader-mode")))
    },

	/*	Returns current (saved) mode (on, off, or auto).
	 */
	currentMode: () => {
		return (localStorage.getItem("reader-mode-setting") || "auto");
	},

	/*	Masks links and hide other elements, as appropriate. This will hide
		linkicons and pop-frame indicators, and will thus cause reflow.
	 */
	//	Called by: ReaderMode.setMode
	activate: () => {
		GWLog("ReaderMode.activate", "reader-mode.js", 1);

		ReaderMode.active = true;

		//	Add body classes.
		document.body.classList.add("reader-mode-active", "masked-links-hidden");
	},
};

//	Inject initial style block.
ReaderMode.injectStyleBlock(ReaderMode.styles, "reader-mode-styles");

//	Activate saved mode, once the <body> element is loaded (and classes known).
doWhenBodyExists(ReaderMode.setMode);
