Extracts = { ...Extracts, 
	showOptionsDialogButton: null,
	optionsDialog: null,

	//	Called by: extracts.js
	showExtractsOptionsDialogPopFrameTitleBarButton: () => {
		let button = Extracts.popFrameProvider.titleBarComponents.optionsButton();

		button.title = `Show ${(Extracts.popFrameTypeText())} options (enable/disable ${(Extracts.popFrameTypeText())}s)`;
		button.innerHTML = GW.svg("eye-slash-solid");
		button.classList.add("show-extracts-options-dialog");

		button.addActivateEvent((event) => {
			event.stopPropagation();

			Extracts.showOptionsDialog();
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

		//	Run cleanup.
		Extracts.cleanup();
	},

	enableExtractPopFrames: () => {
		GWLog("Extracts.enableExtractPopFrames", "extracts-options.js", 1);

		//	Clear saved setting.
		localStorage.removeItem(Extracts.extractPopFramesDisabledLocalStorageItemKey());

		//  Run setup.
		Extracts.setup();

		/*  Since the main document has already loaded, we must trigger the
			processing of targets manually.
		 */
		Extracts.processTargetsInContainer(document.body);
	},

	popFrameTypeText: () => {
		return (Extracts.popFrameProvider == Popups
				? "popup"
				: "popin");
	},

	showOptionsDialog: () => {
		GWLog("Extracts.showOptionsDialog", "extracts-options.js", 1);

		if (Extracts.popFrameProvider == Popups)
			Popups.hidePopupContainer();

		//  Create the options dialog, if needed.
		if (Extracts.optionsDialog == null) {
			let enabledRadioButtonChecked = Extracts.extractPopFramesEnabled() ? `checked=""` : ``;
			let disabledRadioButtonChecked = Extracts.extractPopFramesEnabled() ? `` : `checked=""`;

			let actionDescription = Extracts.popFrameProvider == Popups
									? "hovering over"
									: "tapping on";

			Extracts.optionsDialog = addUIElement(`<div id="${(Extracts.popFrameTypeText())}-options-dialog" class="extracts-options-dialog" style="display: none;"><div>` + 
				`<div class="extracts-options-dialog-title-bar">` + 
					`<h1>${(Extracts.popFrameTypeText().capitalizeWords())}s</h1>` + 
				`</div>` + 
				`<div class="controls">` + 
					`<form class="option-buttons">
						<label>
							<input class="extracts-enable" name="extracts-enable-status" ${enabledRadioButtonChecked} value="enabled" type="radio">
							<span class="button-text">
								<span class="label">Enable</span>
								<span class="explanation">Show ${(Extracts.popFrameTypeText())}s when ${actionDescription} annotated links.</span>
							</span>
						</label>
						<label>
							<input class="extracts-disable" name="extracts-enable-status" ${disabledRadioButtonChecked} value="disabled" type="radio">
							<span class="button-text">
								<span class="label">Disable</span>
								<span class="explanation">Don’t show ${(Extracts.popFrameTypeText())}s.</span>
							</span>
						</label>
					</form>` +
				`</div>` +
				`<div class="controls-aux">` + 
					`<button type="button" class="cancel-button">Cancel</button>` + 
					`<button type="button" class="save-button default-button">Save</button>` + 
				`</div>` +
			`</div></div>`);

			//  Add event listeners.
			Extracts.optionsDialog.addEventListener("click", Extracts.optionsDialogBackdropClicked = (event) => {
				GWLog("Extracts.optionsDialogBackdropClicked", "extracts.js", 2);

				event.stopPropagation();
				Extracts.fadeOptionsDialog();
			});
			Extracts.optionsDialog.firstElementChild.addEventListener("click", Extracts.optionsDialogClicked = (event) => {
				GWLog("Extracts.optionsDialogClicked", "extracts.js", 3);

				event.stopPropagation();
			});
			Extracts.optionsDialog.querySelector("button.cancel-button").addActivateEvent(Extracts.optionsDialogCancelButtonClicked = (event) => {
				GWLog("Extracts.optionsDialogCancelButtonClicked", "extracts.js", 2);

				Extracts.fadeOptionsDialog();
			});
			Extracts.optionsDialog.querySelector("button.save-button").addActivateEvent(Extracts.optionsDialogSaveButtonClicked = (event) => {
				GWLog("Extracts.optionsDialogSaveButtonClicked", "extracts.js", 2);

				Extracts.saveOptions();
				Extracts.fadeOptionsDialog();
			});
			document.addEventListener("keyup", Extracts.optionsDialogKeyUp = (event) => {
				GWLog("Extracts.optionsDialogKeyUp", "extracts.js", 3);

				let allowedKeys = [ "Escape", "Esc", "Enter", "Return" ];
				if (!allowedKeys.includes(event.key) || Extracts.optionsDialog.style.display == "none")
					return;

				event.preventDefault();

				switch (event.key) {
				case "Enter":
				case "Return":
					Extracts.saveOptions();
					break;
				}

				Extracts.fadeOptionsDialog();
			});
		} else {
			Extracts.optionsDialog.querySelector(Extracts.extractPopFramesEnabled() ? "input.extracts-enable" : "input.extracts-disable").checked = true;
		}

		//  Un-hide the options dialog.
		Extracts.optionsDialog.style.display = "";
	},

	fadeOptionsDialog: () => {
		GWLog("Extracts.fadeOptionsDialog", "extracts-options.js", 1);

		Extracts.optionsDialog.classList.toggle("fading", true);
		setTimeout(Extracts.hideOptionsDialog, 150);
	},

	hideOptionsDialog: () => {
		GWLog("Extracts.hideOptionsDialog", "extracts-options.js", 1);

		//	Update toggle button appearance.
		Extracts.updateShowOptionsDialogButton();

		if (Extracts.popFrameProvider == Popups)
			Popups.unhidePopupContainer();

		if (Extracts.optionsDialog != null) {
			Extracts.optionsDialog.style.display = "none";
			Extracts.optionsDialog.classList.toggle("fading", false);
		}
	},

	saveOptions: () => {
		GWLog("Extracts.saveOptions", "extracts-options.js", 1);

		if (Extracts.optionsDialog.querySelector("input.extracts-enable").checked)
			Extracts.enableExtractPopFrames();
		else
			Extracts.disableExtractPopFrames();
	},

	injectShowOptionsDialogButton: () => {
		GWLog("Extracts.injectShowOptionsDialogButton", "extracts-options.js", 1);

		//  Create and inject the button.
		Extracts.showOptionsDialogButton = GW.pageToolbar.addWidget(`<div id="show-extracts-options-dialog-button">` 
				+ `<button type="button" tabindex="-1">`
					+ `<span class="icon"></span>`
					+ `<span class="label">${(Extracts.popFrameTypeText().capitalizeWords())}s…</span>`
				+ `</button>` 
			+ `</div>`);

		//	Update appearance.
		Extracts.updateShowOptionsDialogButton();

		//  Add event listeners.
		Extracts.showOptionsDialogButton.querySelector("button").addActivateEvent(Extracts.showOptionsDialogButtonClicked = (event) => {
			GWLog("Extracts.showOptionsDialogButtonClicked", "extracts.js", 2);

			event.stopPropagation();

			Extracts.showOptionsDialog();
		});
	},

	updateShowOptionsDialogButton: () => {
		GWLog("Extracts.updateShowOptionsDialogButton", "extracts-options.js", 2);

		//	Update button tooltip.
		let stateText = Extracts.extractPopFramesEnabled() ? "enabled" : "disabled";
		Extracts.showOptionsDialogButton.querySelector("button").title = `Show options for link ${(Extracts.popFrameTypeText())}s. `
																	   + `(${(Extracts.popFrameTypeText().capitalizeWords())}s are currently ${stateText}.)`;

		//	Update icon.											  
		Extracts.showOptionsDialogButton.querySelector(".icon").innerHTML = Extracts.extractPopFramesEnabled()
																			? GW.svg("message-lines-solid")
																			: GW.svg("message-slash-solid");
	},
};

//  Inject “show options” icon/button into page toolbar.
Extracts.injectShowOptionsDialogButton();
