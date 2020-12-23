if (window.Extracts) {
	Extracts.popupOptionsDialog = null;

	Extracts.disableExtractPopups = () => {
		GWLog("Extracts.disableExtractPopups", "extracts.js", 1);

		localStorage.setItem("extract-popups-disabled", "true");
		Extracts.cleanup();
		Extracts.injectPopupsDisabledShowPopupOptionsDialogButton();
	};

	Extracts.enableExtractPopups = () => {
		GWLog("Extracts.enableExtractPopups", "extracts.js", 1);

		localStorage.removeItem("extract-popups-disabled");
		Extracts.setup();
		Extracts.removePopupsDisabledShowPopupOptionsDialogButton();
	};

	Extracts.showPopupOptionsDialog = () => {
		GWLog("Extracts.showPopupOptionsDialog", "extracts.js", 1);

		//  Create the options dialog, if needed.
		if (Extracts.popupOptionsDialog == null) {
			let popupsEnabled = localStorage.getItem("extract-popups-disabled") != "true";
			let enabledRadioButtonChecked = popupsEnabled ? `checked=""` : ``;
			let disabledRadioButtonChecked = popupsEnabled ? `` : `checked=""`;
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
}
