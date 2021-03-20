if (window.Extracts) {
	/*	Popins.
		*/

// 	Extracts.popinOptionsEnabled = true;

	Extracts.showPopinOptionsDialogPopinTitleBarButton = () => {
		let button = Popins.titleBarComponents.optionsButton();

		button.addActivateEvent((event) => {
			event.stopPropagation();

			Extracts.showPopinOptionsDialog();
		});

		button.title = "Show popin options (enable/disable popins)";
		button.classList.add("show-popin-options-dialog");

		return button;
	};

	/*	Popups.
		*/

	Extracts.popupOptionsEnabled = true;

	Extracts.popupsDisabledShowPopupOptionsDialogButton = null;
	Extracts.popupOptionsDialog = null;

	Extracts.showPopupOptionsDialogPopupTitleBarButton = () => {
		let button = Popups.titleBarComponents.optionsButton();

		button.addActivateEvent((event) => {
			event.stopPropagation();

			Extracts.showPopupOptionsDialog();
		});
		button.title = "Show popup options (enable/disable popups)";
		button.classList.add("show-popup-options-dialog");

		return button;
	};

	Extracts.disableExtractPopups = () => {
		GWLog("Extracts.disableExtractPopups", "extracts.js", 1);

		localStorage.setItem("extract-popups-disabled", "true");
		Extracts.cleanup();
		Extracts.injectPopupsDisabledShowPopupOptionsDialogButton();
	};

	Extracts.enableExtractPopups = () => {
		GWLog("Extracts.enableExtractPopups", "extracts.js", 1);

		localStorage.removeItem("extract-popups-disabled");

		//  Run setup.
		Extracts.setup();

		/*  Since the main document has already loaded, we must trigger the
			processing of targets (and add event handlers) manually.
			*/
		document.querySelectorAll(Extracts.contentContainersSelector).forEach(container => {
			Extracts.addTargetsWithin(container);
			Extracts.setUpAnnotationLoadEventWithin(container);
		});
		GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", Extracts.signalAnnotationLoaded);
		GW.notificationCenter.addHandlerForEvent("GW.contentLoadDidFail", Extracts.signalAnnotationLoadFailed);

		//  Remove the icon/button.
		Extracts.removePopupsDisabledShowPopupOptionsDialogButton();
	};

	Extracts.showPopupOptionsDialog = () => {
		GWLog("Extracts.showPopupOptionsDialog", "extracts.js", 1);

		Popups.hidePopupContainer();

		//  Create the options dialog, if needed.
		if (Extracts.popupOptionsDialog == null) {
			let enabledRadioButtonChecked = Extracts.popupsEnabled() ? `checked=""` : ``;
			let disabledRadioButtonChecked = Extracts.popupsEnabled() ? `` : `checked=""`;
			Extracts.popupOptionsDialog = addUIElement(`<div id='popup-options-dialog' style='display: none;'><div>` + 
				`<h1>Popups</h1>` + 
				`<form class="option-buttons">
					<label>
						<input class="popups-enable" name="popups-enable-status" ${enabledRadioButtonChecked} value="enabled" type="radio">
						<span class='button-text'>
							<span class='label'>Enable</span>
							<span class='explanation'>Show popups when hovering over annotated links.</span>
						</span>
					</label>
					<label>
						<input class="popups-disable" name="popups-enable-status" ${disabledRadioButtonChecked} value="disabled" type="radio">
						<span class='button-text'>
							<span class='label'>Disable</span>
							<span class='explanation'>Don’t show popups.</span>
						</span>
					</label>
				</form>` +
				`<button type='button' class='close-button'><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 512"><path d="M193.94 256L296.5 153.44l21.15-21.15c3.12-3.12 3.12-8.19 0-11.31l-22.63-22.63c-3.12-3.12-8.19-3.12-11.31 0L160 222.06 36.29 98.34c-3.12-3.12-8.19-3.12-11.31 0L2.34 120.97c-3.12 3.12-3.12 8.19 0 11.31L126.06 256 2.34 379.71c-3.12 3.12-3.12 8.19 0 11.31l22.63 22.63c3.12 3.12 8.19 3.12 11.31 0L160 289.94 262.56 392.5l21.15 21.15c3.12 3.12 8.19 3.12 11.31 0l22.63-22.63c3.12-3.12 3.12-8.19 0-11.31L193.94 256z"/></svg></button>` + 
				`<button type='button' class='save-button'>Save</button>` + 
				`</div></div>`);
			//  Add event listeners.
			requestAnimationFrame(() => {
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
			});
		} else {
			Extracts.popupOptionsDialog.querySelector(Extracts.popupsEnabled() ? "input.popups-enable" : "input.popups-disable").checked = true;
		}

		//  Un-hide the options dialog.
		Extracts.popupOptionsDialog.style.display = "";
	};

	Extracts.fadePopupOptionsDialog = () => {
		GWLog("Extracts.fadePopupOptionsDialog", "extracts.js", 1);

		Extracts.popupOptionsDialog.classList.toggle("fading", true);
		setTimeout(Extracts.hidePopupOptionsDialog, 150);
	};

	Extracts.hidePopupOptionsDialog = () => {
		GWLog("Extracts.hidePopupOptionsDialog", "extracts.js", 1);

		Popups.unhidePopupContainer();

		if (Extracts.popupOptionsDialog != null) {
			Extracts.popupOptionsDialog.style.display = "none";
			Extracts.popupOptionsDialog.classList.toggle("fading", false);
		}
	};

	Extracts.savePopupOptions = () => {
		GWLog("Extracts.savePopupOptions", "extracts.js", 1);

		if (Extracts.popupOptionsDialog.querySelector("input.popups-enable").checked)
			Extracts.enableExtractPopups();
		else
			Extracts.disableExtractPopups();
	};

	Extracts.injectPopupsDisabledShowPopupOptionsDialogButton = () => {
		GWLog("Extracts.injectPopupsDisabledShowPopupOptionsDialogButton", "extracts.js", 1);

		if (Extracts.popupsDisabledShowPopupOptionsDialogButton != null)
			return;

		//  Create and inject the button.
		Extracts.popupsDisabledShowPopupOptionsDialogButton = addUIElement(`<div id="popups-disabled-show-popup-options-dialog-button">` + 
			`<button type="button" title="Show options for link popups. (Popups are currently disabled.)"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 640 512"><path d="M64 352c0 35.3 28.7 64 64 64h96v84c0 9.8 11.2 15.5 19.1 9.7L368 416h2L64 179.5V352zm569.8 106.1l-77.6-60c12.1-11.6 19.8-28 19.8-46.1V64c0-35.3-28.7-64-64-64H128c-21.5 0-40.4 10.7-52 27L45.5 3.4C38.5-2 28.5-.8 23 6.2L3.4 31.4c-5.4 7-4.2 17 2.8 22.4l588.4 454.7c7 5.4 17 4.2 22.5-2.8l19.6-25.3c5.4-6.8 4.1-16.9-2.9-22.3z"/></svg></button>` + `</div>`);

		//  Add event listener.
		requestAnimationFrame(() => {
			Extracts.popupsDisabledShowPopupOptionsDialogButton.querySelector("button").addActivateEvent(Extracts.popupsDisabledShowPopupOptionsDialogButtonClicked = (event) => {
				GWLog("Extracts.popupsDisabledShowPopupOptionsDialogButtonClicked", "extracts.js", 2);

				event.stopPropagation();

				Extracts.showPopupOptionsDialog();
			});
		});

		//	Show/hide the button on scroll up/down.
		addScrollListener(Extracts.updatePopupsDisabledShowPopupOptionsDialogButtonVisibility, 
			"updatePopupsDisabledShowPopupOptionsDialogButtonVisibilityScrollListener");
	};

	Extracts.updatePopupsDisabledShowPopupOptionsDialogButtonVisibility = (event) => {
		GWLog("updatePopupsDisabledShowPopupOptionsDialogButtonVisibility", "rewrite.js", 3);

		if (Extracts.popupsDisabledShowPopupOptionsDialogButton == null)
			return;

		// Hide button when scrolling a full page down.
		if (GW.scrollState.unbrokenDownScrollDistance > window.innerHeight)
			Extracts.popupsDisabledShowPopupOptionsDialogButton.classList.toggle("hidden", true);

		// Show back-to-top link on ANY scroll up.
		if (GW.scrollState.unbrokenUpScrollDistance > window.innerHeight || GW.scrollState.lastScrollTop <= 0)
			Extracts.popupsDisabledShowPopupOptionsDialogButton.classList.toggle("hidden", false);
	};

	Extracts.removePopupsDisabledShowPopupOptionsDialogButton = () => {
		GWLog("Extracts.removePopupsDisabledShowPopupOptionsDialogButton", "extracts.js", 1);

		if (Extracts.popupsDisabledShowPopupOptionsDialogButton == null)
			return;

		Extracts.popupsDisabledShowPopupOptionsDialogButton.remove();
		Extracts.popupsDisabledShowPopupOptionsDialogButton = null;
	};
}

//  Inject “popups disabled” icon/button, if need be.
if (!Extracts.popupsEnabled())
	Extracts.injectPopupsDisabledShowPopupOptionsDialogButton();
