ReaderMode = {
    active: false,

    readerModeTitleNote: " (reader mode)",

	/*	Overridable default mode.
	 */
	defaultMode: "auto",

    /*  Returns current (saved) mode (on, off, or auto).
     */
    currentMode: () => {
        return (localStorage.getItem("reader-mode-setting") ?? ReaderMode.defaultMode);
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

    /*  Masks links and hide other elements, as appropriate. This will hide
        linkicons and pop-frame indicators, and will thus cause reflow.
     */
    //  Called by: ReaderMode.setMode
    activate: () => {
        GWLog("ReaderMode.activate", "reader-mode-initial.js", 1);

        //  Add body classes.
        document.body.classList.add("reader-mode-active", "masked-links-hidden");

        //  Update document title.
        document.title += ReaderMode.readerModeTitleNote;
    }
};

//  Activate saved mode, once the <body> element is loaded (and classes known).
doWhenBodyExists(() => {
	//  Activate (if needed).
	if (ReaderMode.enabled() == true)
		ReaderMode.activate();
});

//	Once .setMode() is available, set the mode.
GW.notificationCenter.addHandlerForEvent("ReaderMode.didLoad", (eventInfo) => {
	ReaderMode.setMode();
}, { once: true });
