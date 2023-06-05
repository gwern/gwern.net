ReaderMode = {
    active: false,

    readerModeTitleNote: " (reader mode)",

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

        //  Fire event.
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

        //  Update document title.
        document.title += ReaderMode.readerModeTitleNote;
    },
};

//  Activate saved mode, once the <body> element is loaded (and classes known).
doWhenBodyExists(ReaderMode.setMode);
