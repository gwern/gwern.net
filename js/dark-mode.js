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
		[ 'auto', 'Auto', 'Set light or dark mode automatically, according to system-wide setting (Win: Start → Personalization → Colors; Mac: Apple → System-Preferences → General → Appearance; iOS: Settings → Display-and-Brightness; Android: Settings → Display)' ],
		[ 'light', 'Light', 'Light mode at all times (black-on-white)' ],
		[ 'dark', 'Dark', 'Dark mode at all times (inverted: white-on-black)' ]
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
	},

	/******************/
	/*	Mode selection.
	 */

	//	Called by: DarkMode.modeSelectButtonClicked
	saveMode: (newMode) => {
		GWLog("DarkMode.saveMode", "dark-mode.js", 1);

		if (newMode == "auto")
			localStorage.removeItem("dark-mode-setting");
		else
			localStorage.setItem("dark-mode-setting", newMode);
	},

	//	Called by: DarkMode.injectModeSelector
	modeSelectorHTML: (inline = false) => {
		let selectorTagName = (inline ? "span" : "div");
		let selectorId = (inline ? "" : " id='dark-mode-selector'");
		let selectorClass = (" class='dark-mode-selector mode-selector" + (inline ? " mode-selector-inline" : "") + "'");

		//	Get saved mode setting (or default).
		let currentMode = DarkMode.currentMode();

		return `<${selectorTagName}${selectorId}${selectorClass}>`
			+ DarkMode.modeOptions.map(modeOption => {
				let [ name, label, desc ] = modeOption;
				let selected = (name == currentMode ? " selected" : "");
				let disabled = (name == currentMode ? " disabled" : "");
				let active = ((   currentMode == "auto"
							   && name == (GW.mediaQueries.systemDarkModeActive.matches ? "dark" : "light"))
							  ? " active"
							  : "");
				if (name == currentMode)
					desc += DarkMode.selectedModeOptionNote;
				return `<button
							type="button"
							class="select-mode-${name}${selected}${active}"
							${disabled}
							tabindex="-1"
							data-name="${name}"
							title="${desc}"
								>${label}</button>`;
			  }).join("")
			+ `</${selectorTagName}>`;
	},

	modeSelectButtonClicked: (event) => {
		GWLog("DarkMode.modeSelectButtonClicked", "dark-mode.js", 2);

		/*	We don’t want clicks to go through if the transition 
			between modes has not completed yet, so we disable the 
			button temporarily while we’re transitioning between 
			modes.
		 */
		doIfAllowed(() => {
			// Determine which setting was chosen (ie. which button was clicked).
			let selectedMode = event.target.dataset.name;

			// Save the new setting.
			DarkMode.saveMode(selectedMode);

			// Actually change the mode.
			DarkMode.setMode(selectedMode);
		}, DarkMode, "modeSelectorInteractable");
	},

	//	Called by: DarkMode.setup
	injectModeSelector: (replacedElement = null) => {
		GWLog("DarkMode.injectModeSelector", "dark-mode.js", 1);

		//	Inject the mode selector widget.
		let modeSelector;
		if (replacedElement) {
			replacedElement.innerHTML = DarkMode.modeSelectorHTML(true);
			modeSelector = replacedElement.firstElementChild;
			unwrap(replacedElement);
		} else {
			modeSelector = DarkMode.modeSelector = addUIElement(DarkMode.modeSelectorHTML());
		}

		//  Add event listeners.
		requestAnimationFrame(() => {
			//	Activate mode selector widget buttons.
			modeSelector.querySelectorAll("button").forEach(button => {
				button.addActivateEvent(DarkMode.modeSelectButtonClicked);
			});

			if (modeSelector == DarkMode.modeSelector) {
				//	Show the button on hover (if it’s hid via scroll-down).
				DarkMode.modeSelector.addEventListener("mouseenter", (event) => {
					//	Fire event.
					GW.notificationCenter.fireEvent("GW.modeSelectorMouseEnter");
				});
				GW.notificationCenter.addHandlerForEvent("GW.modeSelectorMouseEnter", (info) => {
					DarkMode.showModeSelector();
				});
			}
		});

		//	Register event handler to update mode selector state.
		GW.notificationCenter.addHandlerForEvent("DarkMode.didSetMode", (info) => {
			DarkMode.updateModeSelectorState(modeSelector);
		});

		if (modeSelector == DarkMode.modeSelector) {
			//	Show/hide the main selector on scroll up/down.
			addScrollListener(DarkMode.updateModeSelectorVisibility,
				"DarkMode.updateModeSelectorVisibilityScrollListener", { defer: true });
		}

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
			button.classList.remove("active", "selected");
			button.disabled = false;
			if (button.title.endsWith(DarkMode.selectedModeOptionNote))
				button.title = button.title.slice(0, (-1 * DarkMode.selectedModeOptionNote.length));
		});

		//	Set the correct button to be selected.
		modeSelector.querySelectorAll(`.select-mode-${currentMode}`).forEach(button => {
			button.classList.add("selected");
			button.disabled = true;
			button.title += DarkMode.selectedModeOptionNote;
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
	},

	//	Called by: DarkMode.updateModeSelectorVisibilityScrollListener
	updateModeSelectorVisibility: (event) => {
		GWLog("DarkMode.updateModeSelectorVisibility", "dark-mode.js", 3);

		if (DarkMode.modeSelector == null)
			return;

		//	One PgDn’s worth of scroll distance, approximately.
		let onePageScrollDistance = (0.8 * window.innerHeight);

		/*	Hide mode selector when scrolling a full page down.
		 */
		if (GW.scrollState.unbrokenDownScrollDistance > onePageScrollDistance) {
			DarkMode.hideModeSelector();
		}

		/*	On desktop, show mode selector when scrolling to top of page, or a full
			page up.
			On mobile, show mode selector on ANY scroll up.
		 */
		if (GW.isMobile()) {
			if (   GW.scrollState.unbrokenUpScrollDistance > 0 
				|| GW.scrollState.lastScrollTop <= 0)
				DarkMode.showModeSelector();
		} else if (   GW.scrollState.unbrokenUpScrollDistance > onePageScrollDistance
				   || GW.scrollState.lastScrollTop <= 0) {
			DarkMode.showModeSelector();
		}
	},

	//	Called by: DarkMode.updateModeSelectorVisibility
	//	Called by: dark mode selector ‘mouseenter’ event handler
	showModeSelector: () => {
		GWLog("DarkMode.showModeSelector", "dark-mode.js", 3);

		DarkMode.modeSelector.classList.remove("hidden");
	},

	//	Called by: DarkMode.updateModeSelectorVisibility
	hideModeSelector: () => {
		GWLog("DarkMode.hideModeSelector", "dark-mode.js", 3);

		DarkMode.modeSelector.classList.add("hidden");
	}
};

GW.notificationCenter.fireEvent("DarkMode.didLoad");

doWhenPageLoaded(() => {
    DarkMode.setup();
});
