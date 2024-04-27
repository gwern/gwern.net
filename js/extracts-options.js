Extracts = { ...Extracts,
	/*****************/
	/*	Configuration.
	 */
	modeOptions: [
		[ "on", "Enable Pop-frames", "Pop-frames Enabled", `Enable link pop-frames.`, "message-lines-solid" ],
		[ "off", "Disable Pop-frames", "Pop-frames Disabled", `Disable link pop-frames.`, "message-slash-solid" ],
	],

	selectedModeOptionNote: " [This option is currently selected.]",

	popFramesDisableDespawnDelay: 1000,
	popFramesDisableWidgetFlashStayDuration: 3000,
	popFramesDisableAutoToggleDelay: 1000,

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
			let [ name, unselectedLabel, selectedLabel, desc, iconName ] = modeOption;
			let selected = (name == currentMode ? " selected" : " selectable");
			let disabled = (name == currentMode ? " disabled" : "");
			unselectedLabel = unselectedLabel.replace("-frame", Extracts.popFrameTypeSuffix());
			selectedLabel = selectedLabel.replace("-frame", Extracts.popFrameTypeSuffix());
			desc = desc.replace("-frame", Extracts.popFrameTypeSuffix());
			if (name == currentMode)
				desc += Extracts.selectedModeOptionNote;
			let label = (name == currentMode) ? selectedLabel : unselectedLabel;
			return `<button
					 type="button"
					 class="select-mode-${name}${selected}"
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

			//	Reset label text to unselected state.
			let label = button.querySelector(".label");
			label.innerHTML = label.dataset.unselectedLabel;
		});

		//	Set the correct button to be selected.
		modeSelector.querySelectorAll(`.select-mode-${currentMode}`).forEach(button => {
			button.swapClasses([ "selectable", "selected" ], 1);
			button.disabled = true;
			button.title += Extracts.selectedModeOptionNote;

			//	Set label text to selected state.
			let label = button.querySelector(".label");
			label.innerHTML = label.dataset.selectedLabel;
		});
	},

	//	Called by: extracts.js
	disableExtractPopFramesPopFrameTitleBarButton: () => {
		let button = Extracts.popFrameProvider.titleBarComponents.genericButton();

		button.title = `Disable link pop${(Extracts.popFrameTypeSuffix())}s [currently enabled]`;
		button.innerHTML = Extracts.popFrameProvider == Popups
						   ? GW.svg("eye-slash-solid")
						   : GW.svg("eye-slash-regular");
		button.classList.add("extracts-disable-button");

		button.addActivateEvent((event) => {
			event.stopPropagation();

			button.classList.add("disabled");

			GW.pageToolbar.toggleCollapseState(false);

			setTimeout(() => {
				Extracts.popFrameProvider.cleanup();

				GW.pageToolbar.flashWidget("extracts-mode-selector", {
					flashStayDuration: Extracts.popFramesDisableWidgetFlashStayDuration,
					showSelectedButtonLabel: true
				});
				setTimeout(() => {
					Extracts.disableExtractPopFrames();

					//	Temporarily highlight newly selected option.
					GW.pageToolbar.getWidget("extracts-mode-selector").classList.add("highlight-selected-button-label");
					setTimeout(() => {
						GW.pageToolbar.getWidget("extracts-mode-selector").classList.remove("highlight-selected-button-label");
					}, Extracts.popFramesDisableWidgetFlashStayDuration
					 - Extracts.popFramesDisableAutoToggleDelay
					 + GW.pageToolbar.widgetFlashFallDuration);

					GW.pageToolbar.toggleCollapseState(true, {
														   delay: GW.pageToolbar.demoCollapseDelay
																+ Extracts.popFramesDisableWidgetFlashStayDuration
																- Extracts.popFramesDisableAutoToggleDelay
																+ GW.pageToolbar.widgetFlashFallDuration
													   });
				}, GW.pageToolbar.widgetFlashRiseDuration + Extracts.popFramesDisableAutoToggleDelay);
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
		Extracts.processTargetsInContainer(Extracts.rootDocument);
	},
};
