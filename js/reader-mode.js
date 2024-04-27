ReaderMode = { ...ReaderMode, 
	/*****************/
	/*	Configuration.
	 */
	maskedLinksSelector: "p a, li a",

	deactivateTriggerElementSelector: "#reader-mode-disable-when-here, #see-also, #external-links, #appendix, #appendices, #navigation, #footer, #footer-decoration-container",

	showMaskedLinksDelay: 250,

	adjustedPopupTriggerDelay: 2400,

	modeOptions: [
		[ "auto", "Auto", "Auto Reader Mode", "Auto Reader Mode", "Reader mode enabled automatically on certain pages. (When enabled, hold Alt key to show links in text.)", "book-with-gear" ],
		[ "on", "On", "Enable Reader Mode", "Reader Mode Enabled", "Enable reader mode on all pages. (Hold Alt key to show links in text.)", "book-open-solid" ],
		[ "off", "Off", "Disable Reader Mode", "Reader-Mode Disabled", "Disable reader mode on all pages.", "book-open-regular" ]
	],

	selectedModeOptionNote: " [This option is currently selected.]",

	/******************/
	/*	Infrastructure.
	 */
	markdownBody: document.querySelector("#markdownBody"),

	maskedLinksKeyToggleInfoAlert: null,

	modeSelector: null,
	modeSelectorInteractable: true,

	deactivateOnScrollDownObserver: null,

	state: {
		hoveringOverLink: false,
		altKeyPressed: false
	},

	/*************/
	/*	Functions.
	 */

	/*	Set up reader mode UI and interactions.
	 */
	setup: () => {
		GWLog("ReaderMode.setup", "reader-mode.js", 1);

		//	Fully activate.
		if (ReaderMode.active)
			ReaderMode.activate();

		//	Inject mode selector(s).
		ReaderMode.injectModeSelector();
		document.querySelectorAll(".reader-mode-selector-inline").forEach(element => {
			ReaderMode.injectModeSelector(element);
		});
	},

	/******************/
	/*	Mode selection.
	 */

	//	Called by: ReaderMode.modeSelectButtonClicked
	saveMode: (newMode) => {
		GWLog("ReaderMode.saveMode", "reader-mode.js", 1);

		if (newMode == "auto")
			localStorage.removeItem("reader-mode-setting");
		else
			localStorage.setItem("reader-mode-setting", newMode);
	},

	/*	Activate or deactivate reader mode, as determined by the current setting
		and the selected mode.
	 */
	//	Called by: ReaderMode.modeSelectButtonClicked
	setMode: (selectedMode = ReaderMode.currentMode()) => {
		GWLog("ReaderMode.setMode", "reader-mode.js", 1);

		//	Activate or deactivate, as (and if) needed.
		if (   ReaderMode.active == true
			&& ReaderMode.enabled() == false) {
			ReaderMode.deactivate();
		} else if (   ReaderMode.active == false
				   && ReaderMode.enabled() == true) {
			ReaderMode.activate();
		}

		/*	Kill the intersection observer, if switching away from "auto" mode.
			Or, spawn the intersection observer, if switching to "auto" mode.
		 */
		if (   selectedMode != "auto"
			&& ReaderMode.deactivateOnScrollDownObserver != null) {
			ReaderMode.despawnObserver();
		} else if (   selectedMode == "auto"
				   && ReaderMode.active == true
				   && ReaderMode.deactivateOnScrollDownObserver == null) {
			ReaderMode.spawnObserver();
		}

		//	Fire event.
		GW.notificationCenter.fireEvent("ReaderMode.didSetMode");
	},

	//	Called by: ReaderMode.injectModeSelector
	modeSelectorHTML: (inline = false) => {
		//	Get saved mode setting (or default).
		let currentMode = ReaderMode.currentMode();

		let modeSelectorInnerHTML = ReaderMode.modeOptions.map(modeOption => {
			let [ name, shortLabel, unselectedLabel, selectedLabel, desc, iconName ] = modeOption;
			let selected = (name == currentMode ? " selected" : " selectable");
			let disabled = (name == currentMode ? " disabled" : "");
			let active = ((   currentMode == "auto"
						   && name == (ReaderMode.active ? "on" : "off"))
						  ? " active"
						  : "");
			if (name == currentMode)
				desc += ReaderMode.selectedModeOptionNote;
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
		let selectorId = (inline ? "" : "reader-mode-selector");
		let selectorClass = ("reader-mode-selector mode-selector" + (inline ? " mode-selector-inline" : ""));

		return `<${selectorTag} id="${selectorId}" class="${selectorClass}">${modeSelectorInnerHTML}</${selectorTag}>`;
	},

	modeSelectButtonClicked: (event) => {
		GWLog("ReaderMode.modeSelectButtonClicked", "reader-mode.js", 2);

		let button = event.target.closest("button");

		// Determine which setting was chosen (ie. which button was clicked).
		let selectedMode = button.dataset.name;

		/*	We don’t want clicks to go through if the transition 
			between modes has not completed yet, so we disable the 
			button temporarily while we’re transitioning between 
			modes.
		 */
		doIfAllowed(() => {
			// Save the new setting.
			ReaderMode.saveMode(selectedMode);

			// Actually change the mode.
			ReaderMode.setMode(selectedMode);
		}, ReaderMode, "modeSelectorInteractable");
	},

	//	Called by: ReaderMode.setup
	injectModeSelector: (replacedElement = null) => {
		GWLog("ReaderMode.injectModeSelector", "reader-mode.js", 1);

		//	Inject the mode selector widget.
		let modeSelector;
		if (replacedElement) {
			modeSelector = elementFromHTML(ReaderMode.modeSelectorHTML(true));
			replacedElement.replaceWith(modeSelector);
		} else {
			modeSelector = ReaderMode.modeSelector = GW.pageToolbar.addWidget(ReaderMode.modeSelectorHTML());
		}

		//	Activate mode selector widget buttons.
		modeSelector.querySelectorAll("button").forEach(button => {
			button.addActivateEvent(ReaderMode.modeSelectButtonClicked);
		});

		//	Register event handler to update mode selector state.
		GW.notificationCenter.addHandlerForEvent("ReaderMode.didSetMode", (info) => {
			ReaderMode.updateModeSelectorState(modeSelector);
		});
	},

	//	Called by: ReaderMode.didSetMode event handler
	//	Called by: ReaderMode.deactivateOnScrollDownObserver callback
	updateModeSelectorState: (modeSelector = ReaderMode.modeSelector) => {
		GWLog("ReaderMode.updateModeSelectorState", "reader-mode.js", 2);

		/*	If the mode selector has not yet been injected, then do nothing.
		 */
		if (modeSelector == null)
			return;

		//	Get saved mode setting (or default).
		let currentMode = ReaderMode.currentMode();

		//	Clear current buttons state.
		modeSelector.querySelectorAll("button").forEach(button => {
			button.classList.remove("active");
			button.swapClasses([ "selectable", "selected" ], 0);
			button.disabled = false;
			if (button.title.endsWith(ReaderMode.selectedModeOptionNote))
				button.title = button.title.slice(0, (-1 * ReaderMode.selectedModeOptionNote.length));

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
			button.title += ReaderMode.selectedModeOptionNote;

			if (modeSelector.classList.contains("mode-selector-inline") == false) {
				//	Set label text to selected state.
				let label = button.querySelector(".label");
				label.innerHTML = label.dataset.selectedLabel;
			}
		});

		/*	Ensure the right button (on or off) has the “currently active”
			indicator, if the current mode is ‘auto’.
		 */
		if (currentMode == "auto") {
			let activeMode = ReaderMode.active 
							 ? "on" 
							 : "off";
			modeSelector.querySelector(`.select-mode-${activeMode}`).classList.add("active");
		}
	},

	/***************************************************/
	/*	Activation / deactivation. (Core functionality.)
	 */

	/*	Masks links and hide other elements, as appropriate. This will hide
		linkicons and pop-frame indicators, and will thus cause reflow.
	 */
	//	Called by: ReaderMode.setMode
	activate: () => {
		GWLog("ReaderMode.activate", "reader-mode.js", 1);

		ReaderMode.active = true;

		//	Add body classes.
		document.body.classList.add("reader-mode-active", "masked-links-hidden");

		//	Get a list of all the links that are to be masked.
		ReaderMode.maskedLinks = ReaderMode.markdownBody.querySelectorAll(ReaderMode.maskedLinksSelector);

		//	Mask links.
		ReaderMode.maskedLinks.forEach(link => {
			if (GW.isMobile() == false) {
				/*	Add `mouseenter` / `mouseleave` listeners to show/hide masked
					links on hover.
				 */
				link.removeMouseEnterEvent = onEventAfterDelayDo(link, "mouseenter", ReaderMode.showMaskedLinksDelay, ReaderMode.updateState, {
					cancelOnEvents: [ "mouseleave" ]
				});
				link.removeMouseLeaveEvent = onEventAfterDelayDo(link, "mouseleave", 0, ReaderMode.updateState);

				//	Add custom popup trigger delay.
				link.specialPopupTriggerDelay = () => {
					return (ReaderMode.maskedLinksVisible() == false
							? ReaderMode.adjustedPopupTriggerDelay
							: Popups.popupTriggerDelay);
				};
			}

			/*	Add custom link click behavior
				(Save the existing handler, if any. Required for popin support.)
			 */
			link.savedOnClick = link.onclick;
			link.onclick = (event) => { return (ReaderMode.maskedLinksVisible() == true); };
		});

		if (GW.isMobile() == false) {
			//	Inject info alert.
			ReaderMode.maskedLinksKeyToggleInfoAlert = addUIElement(`<div id="masked-links-key-toggle-info-alert">`
				+ `<p>`
					+ `<span class="icon">`
						+ GW.svg("book-open-solid")
					+ `</span>`
					+ `Hold <span class="key">alt</span> / <span class="key">option</span> key to show links</p>`
				+ `</div>`);

			//	Add key down/up listeners, to show/hide masked links with Alt key.
			document.addEventListener("keydown", ReaderMode.altKeyDownOrUp = (event) => {
				if (event.key != "Alt")
					return;

				ReaderMode.updateState(event);
			});
			document.addEventListener("keyup", ReaderMode.altKeyDownOrUp);
		}

		/*	Create intersection observer to automatically unmask links when
			page is scrolled down to a specified location (element).
		 */
		if (ReaderMode.currentMode() == "auto")
			ReaderMode.spawnObserver();

		//	Update visual state.
		ReaderMode.updateVisibility({ maskedLinksVisible: false, maskedLinksKeyToggleInfoAlertVisible: false });

		//	Update document title.
		if (document.title.endsWith(ReaderMode.readerModeTitleNote) == false)
			document.title += ReaderMode.readerModeTitleNote;
	},

	//	Called by: ReaderMode.activate
	//	Called by: ReaderMode.setMode
	spawnObserver: () => {
		GWLog("ReaderMode.spawnObserver", "reader-mode.js", 2);

		//	Create the observer.
		ReaderMode.deactivateOnScrollDownObserver = new IntersectionObserver((entries, observer) => {
			entries.forEach(entry => {
				if (entry.isIntersecting == false)
					return;

				ReaderMode.deactivate();
				ReaderMode.updateModeSelectorState();
				ReaderMode.despawnObserver();
			});
		}, { threshold: 1.0 });

		//	Commence observation.
		ReaderMode.deactivateOnScrollDownObserver.observe(document.querySelector(ReaderMode.deactivateTriggerElementSelector));
	},

	//	Called by: ReaderMode.setMode
	despawnObserver: () => {
		GWLog("ReaderMode.despawnObserver", "reader-mode.js", 2);

		ReaderMode.deactivateOnScrollDownObserver.disconnect();
		ReaderMode.deactivateOnScrollDownObserver = null;
	},

	/*	Unmasks links and reveal other elements, as appropriate. This will
		un-hide linkicons and pop-frame indicators, and will thus cause reflow.
	 */
	//	Called by: ReaderMode.setMode
	//	Called by: ReaderMode.deactivateOnScrollDownObserver callback
	deactivate: () => {
		GWLog("ReaderMode.deactivate", "reader-mode.js", 1);

		ReaderMode.active = false;

		//	Update document title.
		if (document.title.endsWith(ReaderMode.readerModeTitleNote))
			document.title = document.title.slice(0, (-1 * ReaderMode.readerModeTitleNote.length));

		//	Remove body classes.
		document.body.classList.remove("reader-mode-active", "masked-links-hidden");

		//	Remove info alert.
		if (ReaderMode.maskedLinksKeyToggleInfoAlert != null)
			ReaderMode.maskedLinksKeyToggleInfoAlert.remove();

		/*	Unmask every masked link. (Note that ReaderMode.maskedLinks is a
			NodeList, returned by a querySelectorAll call in
			ReaderMode.activate. If that function has never been called, then
			ReaderMode.maskedLinks will be null).
		 */
		(ReaderMode.maskedLinks || [ ]).forEach(link => {
			//	Extract hooks.
// 			link.querySelectorAll(".icon-hook").forEach(hook => { hook.remove() });

			if (GW.isMobile() == false) {
				//	Remove `mouseenter` / `mouseleave` listeners from the link.
				link.removeMouseEnterEvent();
				link.removeMouseLeaveEvent();
				link.removeMouseEnterEvent = null;
				link.removeMouseLeaveEvent = null;

				//	Remove custom popup trigger delay.
				link.specialPopupTriggerDelay = null;
			}

			//	Re-enable normal link click behavior.
			link.onclick = link.savedOnClick;
			link.savedOnClick = null;
		});

		//	Re-layout sidenotes.
		if (window.Sidenotes)
			Sidenotes.updateSidenotePositions();

		if (GW.isMobile() == false) {
			//	Remove key down/up listeners (for the Alt key toggle).
			document.removeEventListener("keydown", ReaderMode.altKeyDownOrUp);
			document.removeEventListener("keyup", ReaderMode.altKeyDownOrUp);
			ReaderMode.altKeyDownOrUp = null;
		}
	},

	/****************/
	/*	Link masking.
	 */

	/*	Returns true if masked links (if any) are currently visible, false
		otherwise.
	 */
	maskedLinksVisible: () => {
		return (document.body.classList.contains("masked-links-hidden") == false);
	},

	/***********************************************/
	/*	Interaction-based state/visibility updating.
	 */

	/*	Update state after an event that might cause a visibility change.
	 */
	//	Called by: masked link `mouseenter`/`mouseleave` event handlers
	//	Called by: document `keydown`/`keyup` event handlers (for Alt key)
	updateState: (event) => {
		GWLog("ReaderMode.updateState", "reader-mode.js", 3);

		//	Update tracked state.
		switch (event.type) {
			case "mouseenter":
				ReaderMode.state.hoveringOverLink = true;
				break;
			case "mouseleave":
				ReaderMode.state.hoveringOverLink = false;
				break;
			case "keydown":
				ReaderMode.state.altKeyPressed = true;
				break;
			case "keyup":
				ReaderMode.state.altKeyPressed = false;
				break;
			default:
				break;
		}

		/*	Determine whether we should show or hide masked links and other
			elements.
		 */
		let shouldShowMaskedLinks = (ReaderMode.state.hoveringOverLink || ReaderMode.state.altKeyPressed);
		let shouldShowMaskedLinksKeyToggleInfoAlert = (ReaderMode.state.hoveringOverLink && !ReaderMode.state.altKeyPressed);

		//	Request the desired visibility update.
		ReaderMode.updateVisibility({
			maskedLinksVisible: shouldShowMaskedLinks,
			maskedLinksKeyToggleInfoAlertVisible: shouldShowMaskedLinksKeyToggleInfoAlert
		});
	},

	/*	Update visibility, based on desired visibility (the `update` argument)
		and the current visibility. (Applies to: masked links, masked links key
		toggle info alert panel.)
	 */
	//	Called by: ReaderMode.activate
	//	Called by: ReaderMode.updateState
	updateVisibility: (update) => {
		GWLog("ReaderMode.updateVisibility", "reader-mode.js", 3);

		/*	Show or hide masked links, depending on what visibility update has
			been requested, and whether it is necessary (i.e., whether or not
			things already are as they should be).
		 */
		if (   update.maskedLinksVisible == true
			&& ReaderMode.maskedLinksVisible() == false) {
			//	Show.
			document.body.classList.remove("masked-links-hidden");
		} else if (   update.maskedLinksVisible == false
				   && ReaderMode.maskedLinksVisible() == true) {
			//	Hide.
			document.body.classList.add("masked-links-hidden");
		}

		if (ReaderMode.maskedLinksKeyToggleInfoAlert != null) {
			//	Likewise, show or hide the key toggle info alert panel, as needed.
			if (update.maskedLinksKeyToggleInfoAlertVisible) {
				//	Show.
				ReaderMode.maskedLinksKeyToggleInfoAlert.classList.remove("hidden");
			} else {
				//	Hide.
				ReaderMode.maskedLinksKeyToggleInfoAlert.classList.add("hidden");
			}
		}
	},
};

GW.notificationCenter.fireEvent("ReaderMode.didLoad");

/*	Ensure that we run setup only after Extracts have completed their setups. 
	(This is so that the onclick handlers and so on are already in place.)
 */
if (window.Extracts) {
    ReaderMode.setup();
} else {
    GW.notificationCenter.addHandlerForEvent("Extracts.didLoad", (info) => {
        ReaderMode.setup();
    }, { once: true });
}
