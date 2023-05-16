Extracts = { ...Extracts, 
	/**********/
	/*	Popins.
	 */

// 	popinOptionsEnabled: true,

	//	Called by: extracts.js
	showPopinOptionsDialogPopinTitleBarButton: () => {
		let button = Popins.titleBarComponents.optionsButton();

		button.addActivateEvent((event) => {
			event.stopPropagation();

			Extracts.showPopinOptionsDialog();
		});

		button.title = "Show popin options (enable/disable popins)";
		button.classList.add("show-popin-options-dialog");

		return button;
	},

	disableExtractPopins: () => {
		GWLog("Extracts.disableExtractPopins", "extracts-options.js", 1);

		localStorage.setItem("extract-popins-disabled", "true");
		Extracts.cleanup();
// 		Extracts.injectPopupsDisabledShowPopupOptionsDialogButton();
	},

	enableExtractPopins: () => {
		GWLog("Extracts.enableExtractPopins", "extracts-options.js", 1);

		localStorage.removeItem("extract-popins-disabled");

		//  Run setup.
		Extracts.setup();

		/*  Since the main document has already loaded, we must trigger the
			processing of targets manually.
		 */
		Extracts.processTargetsInContainer(document.body);

		//  Remove the icon/button.
// 		Extracts.removePopupsDisabledShowPopupOptionsDialogButton();
	},

	/**********/
	/*	Popups.
	 */

	showPopupOptionsDialogButton: null,
	popupOptionsDialog: null,

	//	Called by: extracts.js
	showPopupOptionsDialogPopupTitleBarButton: () => {
		let button = Popups.titleBarComponents.optionsButton();

		button.addActivateEvent((event) => {
			event.stopPropagation();

			Extracts.showPopupOptionsDialog();
		});
		button.title = "Show popup options (enable/disable popups)";
		button.classList.add("show-popup-options-dialog");

		return button;
	},

	//	Called by: Extracts.savePopupOptions
	disableExtractPopups: () => {
		GWLog("Extracts.disableExtractPopups", "extracts-options.js", 1);

		localStorage.setItem("extract-popups-disabled", "true");

		//	Run cleanup.
		Extracts.cleanup();
	},

	//	Called by: Extracts.savePopupOptions
	enableExtractPopups: () => {
		GWLog("Extracts.enableExtractPopups", "extracts-options.js", 1);

		localStorage.removeItem("extract-popups-disabled");

		//  Run setup.
		Extracts.setup();

		/*  Since the main document has already loaded, we must trigger the
			processing of targets manually.
		 */
		Extracts.processTargetsInContainer(document.body);
	},

	//	Called by: Extracts.injectShowPopupOptionsDialogButton
	showPopupOptionsDialog: () => {
		GWLog("Extracts.showPopupOptionsDialog", "extracts-options.js", 1);

		Popups.hidePopupContainer();

		//  Create the options dialog, if needed.
		if (Extracts.popupOptionsDialog == null) {
			let enabledRadioButtonChecked = Extracts.popupsEnabled() ? `checked=""` : ``;
			let disabledRadioButtonChecked = Extracts.popupsEnabled() ? `` : `checked=""`;

			Extracts.popupOptionsDialog = addUIElement(`<div id="popup-options-dialog" style="display: none;"><div>` + 
				`<div class="popup-options-dialog-title-bar">` + 
					`<button type="button" class="close-button">${(GW.svg("xmark-regular"))}</button>` + 
					`<h1>Popups</h1>` + 
					`<button type="button" class="save-button">Save</button>` + 
				`</div>` + 
				`<div class="controls">` + 
					`<form class="option-buttons">
						<label>
							<input class="popups-enable" name="popups-enable-status" ${enabledRadioButtonChecked} value="enabled" type="radio">
							<span class="button-text">
								<span class="label">Enable</span>
								<span class="explanation">Show popups when hovering over annotated links.</span>
							</span>
						</label>
						<label>
							<input class="popups-disable" name="popups-enable-status" ${disabledRadioButtonChecked} value="disabled" type="radio">
							<span class="button-text">
								<span class="label">Disable</span>
								<span class="explanation">Don’t show popups.</span>
							</span>
						</label>
					</form>` +
				`</div>` +
			`</div></div>`);

			//  Add event listeners.
			Extracts.popupOptionsDialog.addEventListener("click", Extracts.popupOptionsDialogBackdropClicked = (event) => {
				GWLog("Extracts.popupOptionsDialogBackdropClicked", "extracts.js", 2);

				event.stopPropagation();
				Extracts.fadePopupOptionsDialog();
			});
			Extracts.popupOptionsDialog.firstElementChild.addEventListener("click", Extracts.popupOptionsDialogClicked = (event) => {
				GWLog("Extracts.popupOptionsDialogClicked", "extracts.js", 3);

				event.stopPropagation();
			});
			Extracts.popupOptionsDialog.querySelector("button.close-button").addActivateEvent(Extracts.popupOptionsDialogCloseButtonClicked = (event) => {
				GWLog("Extracts.popupOptionsDialogCloseButtonClicked", "extracts.js", 2);

				Extracts.fadePopupOptionsDialog();
			});
			Extracts.popupOptionsDialog.querySelector("button.save-button").addActivateEvent(Extracts.popupOptionsDialogSaveButtonClicked = (event) => {
				GWLog("Extracts.popupOptionsDialogSaveButtonClicked", "extracts.js", 2);

				Extracts.savePopupOptions();
				Extracts.fadePopupOptionsDialog();
			});
			document.addEventListener("keyup", Extracts.popupOptionsDialogKeyUp = (event) => {
				GWLog("Extracts.popupOptionsDialogKeyUp", "extracts.js", 3);

				let allowedKeys = [ "Escape", "Esc" ];
				if (!allowedKeys.includes(event.key) || Extracts.popupOptionsDialog.style.display == "none")
					return;

				event.preventDefault();
				Extracts.fadePopupOptionsDialog();
			});
		} else {
			Extracts.popupOptionsDialog.querySelector(Extracts.popupsEnabled() ? "input.popups-enable" : "input.popups-disable").checked = true;
		}

		//  Un-hide the options dialog.
		Extracts.popupOptionsDialog.style.display = "";
	},

	//	Called by: Extracts.showPopupOptionsDialog
	fadePopupOptionsDialog: () => {
		GWLog("Extracts.fadePopupOptionsDialog", "extracts-options.js", 1);

		Extracts.popupOptionsDialog.classList.toggle("fading", true);
		setTimeout(Extracts.hidePopupOptionsDialog, 150);
	},

	//	Called by: Extracts.fadePopupOptionsDialog
	hidePopupOptionsDialog: () => {
		GWLog("Extracts.hidePopupOptionsDialog", "extracts-options.js", 1);

		//	Update toggle button appearance.
		Extracts.updateShowPopupOptionsDialogButton();

		Popups.unhidePopupContainer();

		if (Extracts.popupOptionsDialog != null) {
			Extracts.popupOptionsDialog.style.display = "none";
			Extracts.popupOptionsDialog.classList.toggle("fading", false);
		}
	},

	//	Called by: Extracts.showPopupOptionsDialog
	savePopupOptions: () => {
		GWLog("Extracts.savePopupOptions", "extracts-options.js", 1);

		if (Extracts.popupOptionsDialog.querySelector("input.popups-enable").checked)
			Extracts.enableExtractPopups();
		else
			Extracts.disableExtractPopups();
	},

	//	Called by: extracts-options.js (at end of file)
	injectShowPopupOptionsDialogButton: () => {
		GWLog("Extracts.injectShowPopupOptionsDialogButton", "extracts-options.js", 1);

		//  Create and inject the button.
		Extracts.showPopupOptionsDialogButton = GW.pageToolbar.addWidget(`<div id="show-popup-options-dialog-button">` 
				+ `<button type="button" tabindex="-1">`
					+ `<span class="icon"></span>`
					+ `<span class="label">Popups…</span>`
				+ `</button>` 
			+ `</div>`);

		//	Update appearance.
		Extracts.updateShowPopupOptionsDialogButton();

		//  Add event listeners.
		Extracts.showPopupOptionsDialogButton.querySelector("button").addActivateEvent(Extracts.showPopupOptionsDialogButtonClicked = (event) => {
			GWLog("Extracts.showPopupOptionsDialogButtonClicked", "extracts.js", 2);

			event.stopPropagation();

			Extracts.showPopupOptionsDialog();
		});
	},

	updateShowPopupOptionsDialogButton: () => {
		GWLog("Extracts.updateShowPopupOptionsDialogButton", "extracts-options.js", 2);

		//	Update button tooltip.
		let stateText = Extracts.popupsEnabled() ? "enabled" : "disabled";
		Extracts.showPopupOptionsDialogButton.querySelector("button").title = `Show options for link popups. (Popups are currently ${stateText}.)`;

		//	Update icon.											  
		Extracts.showPopupOptionsDialogButton.querySelector(".icon").innerHTML = Extracts.popupsEnabled()
																				 ? GW.svg("message-lines-solid")
																				 : GW.svg("message-slash-solid");
	},
};

//  Inject “popups options” or “popins options” icon/button.
if (Extracts.popFrameProvider == Popups)
	Extracts.injectShowPopupOptionsDialogButton();
