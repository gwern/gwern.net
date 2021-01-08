/*	This code is part of darkmode.js by Said Achmiz.
	See the file `darkmode.js` for license and more information.
	*/

/*	Dark mode: before anything else loads, check browser localStorage for dark 
	mode preference and immediately toggle sets of CSS color variables/classes 
	to avoid any 'flash of white' or delayed loading. Note: CSS falls back to 
	the media-query browser/OS variable preference, so still works if JS is 
	blocked! (The JS is only necessary for the theme switcher widget allowing 
	'force light'/'force dark' options. If users block JS, set the dark mode 
	preference, and are unhappy when they get dark mode, well, they made their 
	bed and must lie in it.)
	*/

/*  Set specified color mode (auto, light, dark).
    */
function setMode(modeOption) {
    GWLog("setMode", "darkmode.js", 1);

    // Inject the appropriate styles.
    let darkModeStyles = document.querySelector("#inlined-dark-mode-styles, #dark-mode-styles");
    if (darkModeStyles == null) return;
    if (modeOption == 'auto') {
        darkModeStyles.media = "all and (prefers-color-scheme: dark)";
    } else if (modeOption == 'dark') {
        darkModeStyles.media = "all";
    } else {
        darkModeStyles.media = "not all";
    }

    // Update selector state.
    if (window.updateModeSelectorState)
	    updateModeSelectorState();
}

// Get saved mode setting (or default).
let currentMode = localStorage.getItem("selected-mode") || 'auto';

// Activate saved mode.
setMode(currentMode);
