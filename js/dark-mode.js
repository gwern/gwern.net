// dark-mode.js: Javascript library for controlling page appearance, toggling between regular white and ‘dark mode’
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
	images with a special class, `.invert-auto` (set on images by automated 
	tools like ImageMagick scripts counting colors) or `.invert` 
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

DarkMode = { ...DarkMode, 
	/*****************/
	/*	Configuration.
	 */
	modeOptions: [
		[ "auto", "Auto", "Auto Light/Dark", "Auto Light/Dark", "Set light or dark mode automatically, according to system-wide setting (Win: Start → Personalization → Colors; Mac: Apple → System-Preferences → Appearance; iOS: Settings → Display-and-Brightness; Android: Settings → Display)", "adjust-solid" ],
		[ "light", "Dark", "Light Mode", "Light Mode", "Light mode at all times (black-on-white)", "sun-solid" ],
		[ "dark", "Light", "Dark Mode", "Dark Mode", "Dark mode at all times (inverted: white-on-black)", "moon-solid" ]
	],

	selectedModeOptionNote: " [This option is currently selected.]",

	/******************/
	/*	Infrastructure.
	 */

	modeSelector: null,
	modeSelectorInteractable: true,

	/*************/
	/*	Functions.
	 */

	/*	Set up UI.
	 */
	setup: () => {
		GWLog("DarkMode.setup", "dark-mode.js", 1);

		//	Inject mode selector(s).
		DarkMode.injectModeSelector();
		document.querySelectorAll(".dark-mode-selector-inline").forEach(element => {
			DarkMode.injectModeSelector(element);
		});

		//	Update saved setting.
		DarkMode.saveMode();
	},

	/******************/
	/*	Mode selection.
	 */

	//	Called by: DarkMode.injectModeSelector
	modeSelectorHTML: (inline = false) => {
		//	Get saved mode setting (or default).
		let currentMode = DarkMode.currentMode();

		let modeSelectorInnerHTML = DarkMode.modeOptions.map(modeOption => {
			let [ name, shortLabel, unselectedLabel, selectedLabel, desc, iconName ] = modeOption;
			let selected = (name == currentMode ? " selected" : " selectable");
			let disabled = (name == currentMode ? " disabled" : "");
			let active = (   currentMode == "auto"
						  && name == DarkMode.computedMode())
						  ? " active"
						  : "";
			if (name == currentMode)
				desc += DarkMode.selectedModeOptionNote;
			let label = inline
						? shortLabel
						: (name == currentMode
						   ? selectedLabel 
						   : unselectedLabel);
			return `<button
					 type="button"
					 class="select-mode-${name}${selected}${active}"
					 ${disabled}
					 tabindex="-1"
					 data-name="${name}"
					 title="${desc}"
					 >`
						+ `<span class="icon">${(GW.svg(iconName))}</span>`
						+ `<span 
							class="label"
							data-selected-label="${selectedLabel}"
							data-unselected-label="${unselectedLabel}"
							>${label}</span>`
				 + `</button>`;
		  }).join("");

		let selectorTag = (inline ? "span" : "div");
		let selectorId = (inline ? "" : "dark-mode-selector");
		let selectorClass = ("dark-mode-selector mode-selector" + (inline ? " mode-selector-inline" : ""));

		return `<${selectorTag} id="${selectorId}" class="${selectorClass}">${modeSelectorInnerHTML}</${selectorTag}>`;
	},

	modeSelectButtonClicked: (event) => {
		GWLog("DarkMode.modeSelectButtonClicked", "dark-mode.js", 2);

		let button = event.target.closest("button");

		//	Determine which setting was chosen (ie. which button was clicked).
		let selectedMode = button.dataset.name;

		/*	We don’t want clicks to go through if the transition 
			between modes has not completed yet, so we disable the 
			button temporarily while we’re transitioning between 
			modes.
		 */
		doIfAllowed(() => {
			//	Actually change the mode.
			DarkMode.setMode(selectedMode);
		}, DarkMode, "modeSelectorInteractable");
	},

	//	Called by: DarkMode.setup
	injectModeSelector: (replacedElement = null) => {
		GWLog("DarkMode.injectModeSelector", "dark-mode.js", 1);

		//	Inject the mode selector widget.
		let modeSelector;
		if (replacedElement) {
			modeSelector = elementFromHTML(DarkMode.modeSelectorHTML(true));
			replacedElement.replaceWith(modeSelector);
		} else {
			modeSelector = DarkMode.modeSelector = GW.pageToolbar.addWidget(DarkMode.modeSelectorHTML());
		}

		//	Activate mode selector widget buttons.
		modeSelector.querySelectorAll("button").forEach(button => {
			button.addActivateEvent(DarkMode.modeSelectButtonClicked);
		});

		//	Register event handler to update mode selector state.
		GW.notificationCenter.addHandlerForEvent("DarkMode.didSetMode", (info) => {
			DarkMode.updateModeSelectorState(modeSelector);
		});

		/*	Add active media query to update mode selector state when system dark
			mode setting changes. (This is relevant only for the ‘auto’ setting.)
		 */
		doWhenMatchMedia(GW.mediaQueries.systemDarkModeActive, "DarkMode.updateModeSelectorStateForSystemDarkMode", () => { 
			DarkMode.updateModeSelectorState(modeSelector);
		});
	},

	//	Called by: DarkMode.didSetMode event handler
	//	Called by: DarkMode.updateModeSelectorStateForSystemDarkMode active media query
	updateModeSelectorState: (modeSelector = DarkMode.modeSelector) => {
		GWLog("DarkMode.updateModeSelectorState", "dark-mode.js", 2);

		/*	If the mode selector has not yet been injected, then do nothing.
		 */
		if (modeSelector == null)
			return;

		//	Get saved mode setting (or default).
		let currentMode = DarkMode.currentMode();

		//	Clear current buttons state.
		modeSelector.querySelectorAll("button").forEach(button => {
			button.classList.remove("active");
			button.swapClasses([ "selectable", "selected" ], 0);
			button.disabled = false;
			if (button.title.endsWith(DarkMode.selectedModeOptionNote))
				button.title = button.title.slice(0, (-1 * DarkMode.selectedModeOptionNote.length));

			if (modeSelector.classList.contains("mode-selector-inline") == false) {
				//	Reset label text to unselected state.
				let label = button.querySelector(".label");
				label.innerHTML = label.dataset.unselectedLabel;
			}
		});

		//	Set the correct button to be selected.
		modeSelector.querySelectorAll(`.select-mode-${currentMode}`).forEach(button => {
			button.swapClasses([ "selectable", "selected" ], 1);
			button.disabled = true;
			button.title += DarkMode.selectedModeOptionNote;

			if (modeSelector.classList.contains("mode-selector-inline") == false) {
				//	Set label text to selected state.
				let label = button.querySelector(".label");
				label.innerHTML = label.dataset.selectedLabel;
			}
		});

		/*	Ensure the right button (light or dark) has the “currently active” 
			indicator, if the current mode is ‘auto’.
		 */
		if (currentMode == "auto") {
			let activeMode = GW.mediaQueries.systemDarkModeActive.matches 
							 ? "dark" 
							 : "light";
			modeSelector.querySelector(`.select-mode-${activeMode}`).classList.add("active");
		}
	}
};

GW.notificationCenter.fireEvent("DarkMode.didLoad");

doWhenPageLoaded(() => {
    DarkMode.setup();
});
