/*	This code is part of dark-mode.js by Said Achmiz.
	See the file `dark-mode.js` for license and more information.
 */

/*	Dark mode: before anything else loads, check browser localStorage for dark 
	mode preference and immediately toggle sets of CSS color variables/classes 
	to avoid any ‘flash of white’ or delayed loading. Note: CSS falls back to 
	the media-query browser/OS variable preference, so still works if JS is 
	blocked! (The JS is only necessary for the theme switcher widget allowing 
	‘force light’/‘force dark’ options. If users block JS, set the dark mode 
	preference, and are unhappy when they get dark mode, well, they made their 
	bed and must lie in it.)
 */

DarkMode = {
	/*	Dark mode elements, switched on or off (or to match system setting) by
		changing the value of their ‘media’ attribute.
	 */
	switchedElementsSelector: [
		"#inlined-styles-colors-dark",
		"#favicon-dark",
		"#favicon-apple-touch-dark"
	].join(", "),

	/*	The ‘media’ attribute values for dark mode elements, for each mode.
	 */
	mediaAttributeValues: {
		"auto":  "all and (prefers-color-scheme: dark)",
		"dark":  "all",
		"light": "not all"
	},

	/*	Page default mode. (This is not a configuration parameter!)
	 */
	pageDefaultMode: null,

    /*  Returns current mode (light, dark, or auto).
     */
    currentMode: () => {
		let switchedElement = document.querySelector(DarkMode.switchedElementsSelector);

		return Object.keys(DarkMode.mediaAttributeValues).find(key => switchedElement.media == DarkMode.mediaAttributeValues[key]);
    },

	/*	Returns saved mode (light, dark, or auto).
	 */
	savedMode: () => {
		return (localStorage.getItem("dark-mode-setting") ?? "auto");
	},

	//	Called by: DarkMode.setMode
	saveMode: (newMode) => {
		GWLog("DarkMode.saveMode", "dark-mode-initial.js", 1);

		if (newMode == "auto")
			localStorage.removeItem("dark-mode-setting");
		else
			localStorage.setItem("dark-mode-setting", newMode);
	},

	/*  Set specified color mode (auto, light, dark).

		Called by: this file (immediately upon load)
		Called by: DarkMode.modeSelectButtonClicked (dark-mode.js)
	 */
	setMode: (selectedMode, save = false) => {
		GWLog("DarkMode.setMode", "dark-mode-initial.js", 1);

		//	Don’t try to set an undefined mode.
		if (selectedMode == undefined)
			return;

		//	Remember previous mode.
		let previousMode = DarkMode.currentMode();

		//	If we’re trying to set the already-set mode, do nothing.
		if (selectedMode == previousMode)
			return;

		//	Set ‘media’ attribute of dark mode elements to match requested mode.
		document.querySelectorAll(DarkMode.switchedElementsSelector).forEach(element => {
			element.media = DarkMode.mediaAttributeValues[selectedMode ?? "auto"];
		});

		//	Save, if needed.
		if (save == true)
			DarkMode.saveMode(selectedMode);

		//	Fire event.
		GW.notificationCenter.fireEvent("DarkMode.didSetMode", { previousMode: previousMode });
	},

	/*	If called with no arguments, returns currently active color mode (light 
		or dark), based on saved selector mode, plus system setting (if selected
		mode is ‘auto’).

		Can instead be called to determine computed mode for an arbitrary
		combination of selected mode and a true/false value for whether the 
		system dark mode is active.
	 */
	computedMode: (modeSetting = DarkMode.currentMode(), systemDarkModeActive = GW.mediaQueries.systemDarkModeActive.matches) => {
		return ((   modeSetting == "dark" 
				|| (   modeSetting == "auto" 
					&& systemDarkModeActive == true))
				? "dark"
				: "light");
	}
};

//	Activate saved mode.
DarkMode.setMode(DarkMode.savedMode());

//  Once the <body> element is loaded (and classes known), set specified mode.
doWhenBodyExists(() => {
	if (document.body.classList.contains("dark-mode"))
		DarkMode.pageDefaultMode = "dark";

	//	Page default mode takes effect if the user hasn’t picked a mode.
	if (   DarkMode.currentMode() == "auto"
		&& DarkMode.pageDefaultMode != null)
		DarkMode.setMode(DarkMode.pageDefaultMode);
});

//	Set up mode change events.
GW.notificationCenter.addHandlerForEvent("DarkMode.didSetMode", (eventInfo) => {
	let previousComputedMode = DarkMode.computedMode(eventInfo.previousMode, GW.mediaQueries.systemDarkModeActive.matches);
	if (previousComputedMode != DarkMode.computedMode())	
		GW.notificationCenter.fireEvent("DarkMode.computedModeDidChange");
}, { name: "DarkMode.fireComputedModeDidChangeEventIfNeededOnSetMode" });
doWhenMatchMedia(GW.mediaQueries.systemDarkModeActive, {
	name: "DarkMode.fireComputedModeDidChangeEventForSystemDarkModeChange",
	ifMatchesOrAlwaysDo: (mediaQuery) => {
		let previousComputedMode = DarkMode.computedMode(DarkMode.currentMode(), !(mediaQuery.matches));
		if (previousComputedMode != DarkMode.computedMode())
			GW.notificationCenter.fireEvent("DarkMode.computedModeDidChange");
	},
	callWhenAdd: false
});
