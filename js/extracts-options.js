Extracts = { ...Extracts, 
	/*****************/
	/*	Configuration.
	 */
	modeOptions: [
		[ "on", "On", `Enable link pop-frames.`, "message-lines-solid" ],
		[ "off", "Off", `Disable link pop-frames.`, "message-slash-solid" ],
	],

	selectedModeOptionNote: " [This option is currently selected.]",

	popFramesDisableDespawnDelay: 2000,
	popFramesDisableAutoToggleDelay: 250,

	/******************/
	/*	Infrastructure.
	 */

	modeSelector: null,
	modeSelectorInteractable: true,

	/*************/
	/*	Functions.
	 */

	/******************/
	/*	Mode selection.
	 */

	//	Called by: Extracts.injectModeSelector
	modeSelectorHTML: (inline = false) => {
		//	Get saved mode setting (or default).
		let currentMode = Extracts.extractPopFramesEnabled() ? "on" : "off";

		let modeSelectorInnerHTML = Extracts.modeOptions.map(modeOption => {
			let [ name, label, desc, icon ] = modeOption;
			let selected = (name == currentMode ? " selected" : " selectable");
			let disabled = (name == currentMode ? " disabled" : "");
			desc = desc.replace("pop-frame", Extracts.popFrameTypeText());
			if (name == currentMode)
				desc += Extracts.selectedModeOptionNote;
			return `<button
						type="button"
						class="select-mode-${name}${selected}"
						${disabled}
						tabindex="-1"
						data-name="${name}"
						title="${desc}"
							>`
						+ `<span class="icon">${(GW.svg(icon))}</span>`
						+ `<span class="label">${label}</span>`
					 + `</button>`;
		  }).join("");

		let selectorTag = (inline ? "span" : "div");
		let selectorId = (inline ? "" : "extracts-mode-selector");
		let selectorClass = ("extracts-mode-selector mode-selector" + (inline ? " mode-selector-inline" : ""));

		return `<${selectorTag} id="${selectorId}" class="${selectorClass}">${modeSelectorInnerHTML}</${selectorTag}>`;
	},

	modeSelectButtonClicked: (event) => {
		GWLog("Extracts.modeSelectButtonClicked", "extracts-options.js", 2);

		let button = event.target.closest("button");

		// Determine which setting was chosen (ie. which button was clicked).
		let selectedMode = button.dataset.name;

		/*	We don’t want clicks to go through if the transition 
			between modes has not completed yet, so we disable the 
			button temporarily while we’re transitioning between 
			modes.
		 */
		doIfAllowed(() => {
			if (selectedMode == "on")
				Extracts.enableExtractPopFrames();
			else
				Extracts.disableExtractPopFrames();
		}, Extracts, "modeSelectorInteractable");
	},

	//	Called by: Extracts.setup (extracts.js)
	injectModeSelector: (replacedElement = null) => {
		GWLog("Extracts.injectModeSelector", "extracts-options.js", 1);

		//	Inject the mode selector widget.
		let modeSelector;
		if (replacedElement) {
			modeSelector = elementFromHTML(Extracts.modeSelectorHTML(true));
			replacedElement.replaceWith(modeSelector);
		} else {
			modeSelector = Extracts.modeSelector = GW.pageToolbar.addWidget(Extracts.modeSelectorHTML());
		}

		//	Activate mode selector widget buttons.
		modeSelector.querySelectorAll("button").forEach(button => {
			button.addActivateEvent(Extracts.modeSelectButtonClicked);
		});

		//	Register event handler to update mode selector state.
		GW.notificationCenter.addHandlerForEvent("Extracts.didSetMode", (info) => {
			Extracts.updateModeSelectorState(modeSelector);
		});
	},

	//	Called by: Extracts.didSetMode event handler
	updateModeSelectorState: (modeSelector = Extracts.modeSelector) => {
		GWLog("Extracts.updateModeSelectorState", "extracts-options.js", 2);

		/*	If the mode selector has not yet been injected, then do nothing.
		 */
		if (modeSelector == null)
			return;

		//	Get saved mode setting (or default).
		let currentMode = Extracts.extractPopFramesEnabled() ? "on" : "off";

		//	Clear current buttons state.
		modeSelector.querySelectorAll("button").forEach(button => {
			button.classList.remove("active");
			button.swapClasses([ "selectable", "selected" ], 0);
			button.disabled = false;
			if (button.title.endsWith(Extracts.selectedModeOptionNote))
				button.title = button.title.slice(0, (-1 * Extracts.selectedModeOptionNote.length));
		});

		//	Set the correct button to be selected.
		modeSelector.querySelectorAll(`.select-mode-${currentMode}`).forEach(button => {
			button.swapClasses([ "selectable", "selected" ], 1);
			button.disabled = true;
			button.title += Extracts.selectedModeOptionNote;
		});
	},

	//	Called by: extracts.js
	disableExtractPopFramesPopFrameTitleBarButton: () => {
		let button = Extracts.popFrameProvider.titleBarComponents.genericButton();

		button.title = `Disable link ${(Extracts.popFrameTypeText())}s [currently enabled]`;
		button.innerHTML = GW.svg("message-lines-regular");
		button.classList.add("extracts-disable-button");

		button.addActivateEvent((event) => {
			event.stopPropagation();

			button.innerHTML = GW.svg("message-slash-regular");
			button.classList.add("disabled");

			GW.pageToolbar.toggleCollapseState(false);

			setTimeout(() => {
				Extracts.popFrameProvider.cleanup();

				setTimeout(() => {
					GW.pageToolbar.flashWidget("extracts-mode-selector");
					setTimeout(() => {
						Extracts.disableExtractPopFrames();

						setTimeout(() => {
							GW.pageToolbar.toggleCollapseState(true);
						}, GW.pageToolbar.demoCollapseDelay + GW.pageToolbar.widgetFlashStayDuration);
					}, GW.pageToolbar.widgetFlashRiseDuration);
				}, Extracts.popFramesDisableAutoToggleDelay);
			}, Extracts.popFramesDisableDespawnDelay);
		});

		return button;
	},

	extractPopFramesDisabledLocalStorageItemKey: () => {
		return (Extracts.popFrameProvider == Popups
				? Extracts.popupsDisabledLocalStorageItemKey
				: Extracts.popinsDisabledLocalStorageItemKey);
	},

	extractPopFramesEnabled: () => {
		return (localStorage.getItem(Extracts.extractPopFramesDisabledLocalStorageItemKey()) != "true");
	},

	disableExtractPopFrames: () => {
		GWLog("Extracts.disableExtractPopFrames", "extracts-options.js", 1);

		//	Save setting.
		localStorage.setItem(Extracts.extractPopFramesDisabledLocalStorageItemKey(), "true");

		//	Fire event.
		GW.notificationCenter.fireEvent("Extracts.didSetMode");

		//	Run cleanup.
		Extracts.cleanup();
	},

	enableExtractPopFrames: () => {
		GWLog("Extracts.enableExtractPopFrames", "extracts-options.js", 1);

		//	Clear saved setting.
		localStorage.removeItem(Extracts.extractPopFramesDisabledLocalStorageItemKey());

		//	Fire event.
		GW.notificationCenter.fireEvent("Extracts.didSetMode");

		//  Run setup.
		Extracts.setup();

		/*  Since the main document has already loaded, we must trigger the
			processing of targets manually.
		 */
		Extracts.processTargetsInContainer(document.body);
	},
};
