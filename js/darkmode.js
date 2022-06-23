// darkmode.js: Javascript library for controlling page appearance, toggling between regular white and ‘dark mode’
// Author: Said Achmiz
// Date: 2020-03-20
// When: Time-stamp: "2022-01-05 11:31:32 gwern"
// license: PD

/*	Experimental ‘dark mode’: Mac OS (Safari) lets users specify via an OS 
	widget ‘dark’/‘light’ to make everything appear bright-white or darker (e.g. 
	for darker at evening to avoid straining eyes & disrupting circadian 
	rhyhms); this then is exposed by Safari as a CSS variable which can be 
	selected on. This is also currently supported by Firefox weakly as an 
	about:config variable. Hypothetically, iOS in the future might use its 
	camera or the clock to set ‘dark mode’ automatically. 

	https://drafts.csswg.org/mediaqueries-5/#prefers-color-scheme
	https://webkit.org/blog/8718/new-webkit-features-in-safari-12-1/
	https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme

	Images are handled specially: images are *not* inverted/negated by default; 
	images with a special class, `.invertible-auto` (set on images by automated 
	tools like ImageMagick scripts counting colors) or `.invertible` 
	(set manually), will be inverted. (This is intended to allow inversion of 
	images which would invert well, like statistical graphs or charts, which are
	typically black-on-white, and are much more pleasant to read in dark mode
	when inverted to white-on-black.) Inversion is removed on image hover or 
	image-focus.js click-to-zoom.

	Because many users do not have access to a browser/OS which explicitly 
	supports dark mode, cannot modify the browser/OS setting without undesired 
	side-effects, wish to opt in only for specific websites, or simply forget 
	that they turned on dark mode & dislike it, we make dark mode controllable 
	by providing a widget at the top of the page.
 */

GW.darkMode = { };

/***********/
/* OPTIONS */
/***********/

GW.darkMode.modeOptions = [
    [ 'auto', 'Auto', 'Set light or dark mode automatically, according to system-wide setting (Win: Start → Personalization → Colors; Mac: Apple → System-Preferences → General → Appearance; iOS: Settings → Display-and-Brightness; Android: Settings → Display)' ],
    [ 'light', 'Light', 'Light mode at all times (black-on-white)' ],
    [ 'dark', 'Dark', 'Dark mode at all times (inverted: white-on-black)' ]
];

/******************/
/* MODE SELECTION */
/******************/

/*	Called on page load (doWhenPageLoaded, this file).
 */
function injectModeSelector() {
    GWLog("injectModeSelector", "darkmode.js", 1);

    //	Get saved mode setting (or default).
    let currentMode = localStorage.getItem("selected-mode") || 'auto';

    //	Inject the mode selector widget.
    GW.darkMode.modeSelector = addUIElement(
        "<div id='mode-selector'>" +
        String.prototype.concat.apply("", GW.darkMode.modeOptions.map(modeOption => {
            let [ name, label, desc ] = modeOption;
            let selected = (name == currentMode ? ' selected' : '');
            let disabled = (name == currentMode ? ' disabled' : '');
            return `<button type='button' class='select-mode-${name}${selected}'${disabled} tabindex='-1' data-name='${name}' title='${desc}'>${label}</button>`})) +
        "</div>");

	//	Activate mode selector widget buttons.
    GW.darkMode.modeSelector.querySelectorAll("button").forEach(button => {
        button.addActivateEvent(GW.darkMode.modeSelectButtonClicked = (event) => {
            GWLog("GW.darkMode.modeSelectButtonClicked", "darkmode.js", 2);

            // Determine which setting was chosen (ie. which button was clicked).
            let selectedMode = event.target.dataset.name;

            // Save the new setting.
            if (selectedMode == "auto")
            	localStorage.removeItem("selected-mode");
            else
            	localStorage.setItem("selected-mode", selectedMode);

            // Actually change the mode.
            setMode(selectedMode);
        });
    });

	/*	Add listeners to update mode selector visibility on scroll and on hover.
	 */
    addScrollListener(updateModeSelectorVisibility, "GW.darkMode.updateModeSelectorVisibilityScrollListener", true, false);
    GW.darkMode.modeSelector.addEventListener("mouseenter", () => { showModeSelector(); });

	/*	Add active media query to update mode selector state when system dark
		mode setting changes. (This is relevant only for the ‘auto’ setting.)
	 */
    doWhenMatchMedia(GW.mediaQueries.systemDarkModeActive, "GW.darkMode.updateModeSelectorStateForSystemDarkMode", () => { updateModeSelectorState(); });
}

/*  Show/hide the mode selector in response to scrolling.

    Called by the ‘GW.darkMode.updateModeSelectorVisibilityScrollListener’ 
    scroll listener.
 */
function updateModeSelectorVisibility(event) {
    GWLog("updateModeSelectorVisibility", "darkmode.js", 3);

    /*	Hide mode selector when scrolling a full page down (or one PgDn’s
    	worth of scroll distance, anyhow).
     */
    if (GW.scrollState.unbrokenDownScrollDistance > (0.8 * window.innerHeight)) {
        hideModeSelector();
    }

    /*	On desktop, show mode selector when scrolling to top of page, or a full
    	page up (or one PgUp’s worth of scroll distance).
    	On mobile, show mode selector on ANY scroll up.
     */
    if (GW.isMobile()) {
        if (   GW.scrollState.unbrokenUpScrollDistance > 0 
        	|| GW.scrollState.lastScrollTop <= 0)
            showModeSelector();
    } else if (   GW.scrollState.unbrokenUpScrollDistance > (0.8 * window.innerHeight)
               || GW.scrollState.lastScrollTop == 0) {
        showModeSelector();
    }
}

/*	Called by: updateModeSelectorVisibility()
 */
function hideModeSelector() {
    GWLog("hideModeSelector", "darkmode.js", 3);

    GW.darkMode.modeSelector.classList.toggle("hidden", true);
}

/*	Called by: mode selector ‘mouseenter’ event handler
	Called by: updateModeSelectorVisibility()
 */
function showModeSelector() {
    GWLog("showModeSelector", "darkmode.js", 3);

    GW.darkMode.modeSelector.classList.toggle("hidden", false);
}

/*  Update the states of the mode selector buttons.

	Called by: setMode() (darkmode-inline.js)
	Called by: updateModeSelectorStateForSystemDarkMode active media query
 */
function updateModeSelectorState() {
    GWLog("updateModeSelectorState", "darkmode.js", 2);

	/*	If the mode selector has not yet been injected (i.e. if we’re calling
		this function on initial page load), then do nothing.
	 */
    if (GW.darkMode.modeSelector == null)
    	return;

    //	Get saved mode setting (or default).
    let currentMode = localStorage.getItem("selected-mode") || 'auto';

    //	Clear current buttons state.
    GW.darkMode.modeSelector.querySelectorAll("button").forEach(button => {
        button.classList.remove("active", "selected");
        button.disabled = false;
    });

    //	Set the correct button to be selected.
    GW.darkMode.modeSelector.querySelectorAll(`.select-mode-${currentMode}`).forEach(button => {
        button.classList.add("selected");
        button.disabled = true;
    });

    /*	Ensure the right button (light or dark) has the “currently active” 
    	indicator, if the current mode is ‘auto’.
     */
    if (currentMode == "auto") {
        if (GW.mediaQueries.systemDarkModeActive.matches)
            GW.darkMode.modeSelector.querySelector(".select-mode-dark").classList.add("active");
        else
            GW.darkMode.modeSelector.querySelector(".select-mode-light").classList.add("active");
    }
}

/******************/
/* INITIALIZATION */
/******************/

doWhenPageLoaded(() => {
    injectModeSelector();
});
