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

	/*	Overridable default mode. (This is not a configuration parameter!)
	 */
	defaultMode: "auto",

    /*  Returns current mode (light, dark, or auto).
     */
    currentMode: () => {
		let switchedElement = document.querySelector(DarkMode.switchedElementsSelector);

		return Object.keys(DarkMode.mediaAttributeValues).find(key => switchedElement.media == DarkMode.mediaAttributeValues[key]);
    },

	//	Called by: DarkMode.setMode
	saveMode: (newMode = DarkMode.currentMode()) => {
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
	setMode: (selectedMode = DarkMode.defaultMode) => {
		GWLog("DarkMode.setMode", "dark-mode-initial.js", 1);

		//	Remember previous mode.
		let previousMode = DarkMode.currentMode();

		//	Save the new setting.
		DarkMode.saveMode(selectedMode);

		//	Set ‘media’ attribute of dark mode elements to match requested mode.
		document.querySelectorAll(DarkMode.switchedElementsSelector).forEach(element => {
			element.media = DarkMode.mediaAttributeValues[selectedMode];
		});

		//	Fire event.
		GW.notificationCenter.fireEvent("DarkMode.didSetMode", { previousMode: previousMode });
	},

	/*	Returns currently active color mode (light or dark).
		Based on saved selector mode, plus system setting (if selected mode is
		‘auto’).
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
DarkMode.setMode();

//  Once the <body> element is loaded (and classes known), set specified mode.
doWhenBodyExists(() => {
	if (document.body.classList.contains("dark-mode"))
		DarkMode.defaultMode = "dark";

	DarkMode.setMode();
});

//	Set up mode change events.
GW.notificationCenter.addHandlerForEvent("DarkMode.didSetMode", (info) => {
	let previousComputedMode = DarkMode.computedMode(info.previousMode, GW.mediaQueries.systemDarkModeActive.matches);
	if (previousComputedMode != DarkMode.computedMode())	
		GW.notificationCenter.fireEvent("DarkMode.computedModeDidChange");
});
doWhenMatchMedia(GW.mediaQueries.systemDarkModeActive, {
	name: "DarkMode.fireComputedModeDidChangeEventForSystemDarkModeChange",
	ifMatchesOrAlwaysDo: (mediaQuery) => {
		let previousComputedMode = DarkMode.computedMode(DarkMode.currentMode(), !(mediaQuery.matches));
		if (previousComputedMode != DarkMode.computedMode())
			GW.notificationCenter.fireEvent("DarkMode.computedModeDidChange");
	},
	callWhenAdd: true
});
