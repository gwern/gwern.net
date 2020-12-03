/*	This code is part of darkmode.js by Said Achmiz.
	See the file `darkmode.js` for license and more information.
	*/

/*	Create global 'GW' object, if need be.
	*/
if (typeof window.GW == "undefined")
        window.GW = { };
GW.temp = { };

/*	Debug output.
	*/
function GWLog (string) {
    if (GW.loggingEnabled || localStorage.getItem("logging-enabled") == "true")
        console.log(string);
}

/*  Set specified color mode (auto, light, dark).
    */
function setMode(modeOption) {
    GWLog("setMode");

    // Inject the appropriate styles.
    let darkModeStyles = document.querySelector("#inlined-dark-mode-styles");
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
