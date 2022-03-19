ReaderMode = {
	/*****************/
	/*	Configuration.
	 */
	maskedLinksSelector: "p a, li a",
	maskedLinksParentBlockSelector: "p, li",

	deactivateTriggerElementSelector: "#reader-mode-disable-when-here, #see-also, #external-links, #appendix, #appendices, #navigation, #footer",

	showMaskedLinksDelay: 250,

	adjustedPopupTriggerDelay: 2400,

	modeOptions: [
		[ 'auto', 'Auto', 'Reader mode enabled automatically on certain pages; otherwise, disabled. (When enabled, hold Alt key to show links in text.)' ],
		[ 'on', 'On', 'Reader mode enabled on all pages. (Hold Alt key to show links in text.)' ],
		[ 'off', 'Off', 'Reader mode disabled on all pages.' ]
	],

	styles: `
		body.reader-mode-active #page-metadata,
		body.reader-mode-active #sidebar-links,
		body.reader-mode-active #TOC ul li::before,
        body.reader-mode-active #footer,
        body.reader-mode-active #navigation,
        body.reader-mode-active .inflationAdjusted .supsub {
			display: none;
		}
		body.reader-mode-active #TOC ul li {
			padding-left: 0.125em;
		}
		.markdownBody .masked-link {
			margin: 0;
		}
		.markdownBody .masked-link .indicator-hook {
			padding-left: 0;
		}
		.markdownBody .masked-link .indicator-hook::before {
			left: -0.3em;
			box-shadow:
				-0.17em 0.05em 0 0 var(--GW-link-underline-background-color),
				-0.17em -0.05em 0 0 var(--GW-link-underline-background-color),
				-0.17em 0 0 0 var(--GW-link-underline-background-color);
		}
		.markdownBody.masked-links-hidden .masked-link .indicator-hook,
		.markdownBody .masked-link::after {
			display: none;
		}
		.markdownBody.masked-links-hidden a.masked-link:not(.popup-open),
		.markdownBody.masked-links-hidden a.masked-link:not(.popup-open):visited,
		.markdownBody.masked-links-hidden a.masked-link:not(.popup-open):hover {
			color: inherit;
			background: none;
			cursor: text;
		}
		#masked-links-key-toggle-info-alert {
			position: absolute;
			background-color: rgba(0, 0, 0, 0.6);
			color: #fff;
			text-shadow:
				0 0 1px #000,
				0 0 3px #000,
				0 0 5px #000;
			padding: 0.5em 1em;
			left: 2px;
			bottom: 1.5em;
			font-family: var(--GW-sans-serif-font-stack);
			font-weight: 700;
			pointer-events: none;
		}
		#masked-links-key-toggle-info-alert.hidden {
			visibility: hidden;
			opacity: 0;
			transition:
				visibility 0.15s ease,
				opacity 0.15s ease;
		}
		#masked-links-key-toggle-info-alert img {
			display: inline-block;
			width: 1.25em;
			filter: invert(1) drop-shadow(0 0 3px var(--GW-body-text-color));
			vertical-align: text-bottom;
			margin: 0 0.625em 0 0;
			top: -0.05em;
			position: relative;
		}
		#masked-links-key-toggle-info-alert .key {
			border: 1px solid #bbb;
			padding: 0.05em 0.375em 0.125em 0.375em;
			display: inline-block;
			border-radius: 4px;
			margin: 0 0.1875em 0 0.125em;
			background-color: #444;
			box-shadow:
				1px 1px 3px 0 #000;
			font-feature-settings: 'smcp';
		}
		#reader-mode-selector {
			position: absolute;
			right: 11em;
			display: flex;
			background-color: var(--GW-mode-selector-background-color);
			padding: 0.1em 0.25em 0.3em 0.25em;
			border: 3px solid transparent;
			opacity: 0.3;
			transition:
				opacity 2s ease;
		}
		#reader-mode-selector.hidden {
			opacity: 0;
		}
		#reader-mode-selector:hover {
			transition: none;
			opacity: 1.0;
			border: 3px double var(--GW-mode-selector-border-hover-color);
		}
		#reader-mode-selector button {
			-moz-appearance: none;
			appearance: none;
			border: none;
			background-color: transparent;
			padding: 0.5em;
			margin: 0 0 0 1.875em;
			line-height: 1;
			font-family: var(--GW-sans-serif-font-stack);
			font-size: 0.75rem;
			text-align: center;
			color: var(--GW-mode-selector-button-text-color);
			position: relative;
			display: flex;
		}
		#reader-mode-selector button::before {
			width: calc(7/5 * 1em);
			height: calc(7/5 * 1em);
			position: absolute;
			left: calc(1px + -4/3 * 1em);
			opacity: 0.4;
			top: 0.5em;
		}
		#reader-mode-selector button.select-mode-auto::before {
			content: url('/static/img/icons/book-with-gear.svg');
			top: 0.4em;
		}
		#reader-mode-selector button.select-mode-on::before {
			content: url('/static/img/icons/book-open-solid.svg');
		}
		#reader-mode-selector button.select-mode-off::before {
			content: url('/static/img/icons/book-open.svg');
		}
		#reader-mode-selector button:not(.selected):hover::before {
			opacity: 1.0;
		}
		#reader-mode-selector button:hover,
		#reader-mode-selector button.selected {
			box-shadow:
				0 2px 0 6px var(--GW-mode-selector-background-color) inset,
				0 1px 0 6px currentColor inset;
		}
		#reader-mode-selector button:not(:disabled):hover {
			color: var(--GW-mode-selector-button-hover-text-color);
			cursor: pointer;
		}
		#reader-mode-selector button:not(:disabled):active {
			transform: translateY(2px);
			box-shadow:
				0 0px 0 6px var(--GW-mode-selector-background-color) inset,
				0 -1px 0 6px currentColor inset;
		}
		#reader-mode-selector button.active:not(:hover)::after {
			content: "";
			position: absolute;
			bottom: 0.25em;
			left: 0;
			right: 0;
			border-bottom: 1px dotted currentColor;
			width: calc(100% - 12px);
			margin: auto;
		}
		@media only screen and (max-width: 1800px) {
			#reader-mode-selector {
				right: 0;
				top: 2em;
			}
		}
		@media only screen and (max-width: 1535px) {
			#reader-mode-selector {
				flex-flow: column;
				padding: 0.1em 0.1em 0.2em 0.15em;
				align-items: flex-start;
				top: 7em;
			}
			#reader-mode-selector button + button {
				margin-top: 0.25em;
			}
		}
		@media only screen and (max-width: 1279px) {
			#reader-mode-selector {
				padding: 0.1em 0.1em 0.25em 0.15em;
			}
			#reader-mode-selector button[class^='select-mode-'] {
				color: transparent;
				margin: 0;
				padding: 0;
				width: 2em;
				height: 2em;
				align-items: center;
				box-shadow: none;
				opacity: 0.55;
			}
			#reader-mode-selector  button[class^='select-mode-'] + button {
				margin-top: 0.5em;
			}
			#reader-mode-selector button[class^='select-mode-']::before {
				left: 12.5%;
				width: 75%;
				height: 75%;
			}
			#reader-mode-selector button:not(:disabled):hover {
				color: transparent;
			}
			#reader-mode-selector button:hover,
			#reader-mode-selector button.selected {
				opacity: 1.0;
			}
		}
		@media only screen and (max-width: 649px) {
			#reader-mode-selector button[class^='select-mode-'] {
				width: 2.25em;
				height: 2.25em;
			}
		}
	`,

	/******************/
	/*	Infrastructure.
	 */
	markdownBody: document.querySelector("#markdownBody"),

	maskedLinksKeyToggleInfoAlert: null,

	active: false,

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

	/*	Prepare to activate reader mode.
	 */
	setup: () => {
		GWLog("ReaderMode.setup", "reader-mode.js", 1);

		//	Inject style block.
		document.querySelector("head").insertAdjacentHTML("beforeend", `<style id='reader-mode-styles'>${ReaderMode.styles}</style>`);

		//	Inject mode selector.
		ReaderMode.injectModeSelector();

		//	Get saved mode setting (or default).
		let currentMode = ReaderMode.currentMode();

		//	Activate saved mode.
		ReaderMode.setMode(currentMode);
	},

	/******************/
	/*	Mode selection.
	 */

	/*	Activate or deactivate reader mode, as determined by the current setting
		and the selected mode.
	 */
	//	Called by: ReaderMode.setup
	//	Called by: ReaderMode.modeSelectButtonClicked
	setMode: (selectedMode) => {
		GWLog("ReaderMode.setMode", "reader-mode.js", 1);

		//	Activate or deactivate, as (and if) needed.
		if (   ReaderMode.active == true
			&& ReaderMode.enabled() == false) {
			ReaderMode.deactivate();
		} else if (   ReaderMode.active == false
				   && ReaderMode.enabled() == true) {
			ReaderMode.activate();
		}

		//	Kill the intersection observer, if switching away from "auto" mode.
		if (   selectedMode != "auto"
			&& ReaderMode.deactivateOnScrollDownObserver != null) {
			ReaderMode.deactivateOnScrollDownObserver.disconnect();
			ReaderMode.deactivateOnScrollDownObserver = null;
		}

		//	Update mode selector state.
		ReaderMode.updateModeSelectorState();
	},

	/*	Returns true if reader mode is set to be enabled for the current page,
		false otherwise.
	 */
    enabled: () => {
		let currentMode = ReaderMode.currentMode();
        return (   currentMode == "on"
        		|| (   currentMode == "auto"
        			&& document.body.classList.contains("reader-mode")))
    },

	/*	Returns current (saved) mode (on, off, or auto).
	 */
	currentMode: () => {
		return (localStorage.getItem("reader-mode-setting") || 'auto');
	},

	//	Called by: ReaderMode.setup
	injectModeSelector: () => {
		GWLog("ReaderMode.injectModeSelector", "reader-mode.js", 1);

		//	Get saved mode setting (or default).
		let currentMode = ReaderMode.currentMode();

		//	Inject the mode selector widget.
		ReaderMode.modeSelector = addUIElement(
			"<div id='reader-mode-selector'>" +
			String.prototype.concat.apply("", ReaderMode.modeOptions.map(modeOption => {
				let [ name, label, desc ] = modeOption;
				let selected = (name == currentMode ? ' selected' : '');
				let disabled = (name == currentMode ? ' disabled' : '');
				return `<button type='button' class='select-mode-${name}${selected}'${disabled} tabindex='-1' data-name='${name}' title='${desc}'>${label}</button>`})) +
			"</div>");

		//  Add event listeners and update state.
		requestAnimationFrame(() => {
			//	Activate mode selector widget buttons.
			ReaderMode.modeSelector.querySelectorAll("button").forEach(button => {
				button.addActivateEvent(ReaderMode.modeSelectButtonClicked = (event) => {
					GWLog("ReaderMode.modeSelectButtonClicked", "reader-mode.js", 2);

					/*	We don’t want clicks to go through if the transition between
						modes has not completed yet.
					 */
					if (ReaderMode.modeSelectorInteractable == false)
						return;

					/*	Disable the button temporarily while we’re transitioning
						between modes.
					 */
					ReaderMode.modeSelectorInteractable = false;

					// Determine which setting was chosen (ie. which button was clicked).
					let selectedMode = event.target.dataset.name;

					// Save the new setting.
					if (selectedMode == "auto")
						localStorage.removeItem("reader-mode-setting");
					else
						localStorage.setItem("reader-mode-setting", selectedMode);

					// Actually change the mode.
					ReaderMode.setMode(selectedMode);

					//	Re-enable button after the mode switch is complete.
					requestAnimationFrame(() => {
						ReaderMode.modeSelectorInteractable = true;
					});
				});
			});

			//	Show the button on hover (if it’s hid via scroll-down).
			ReaderMode.modeSelector.addEventListener("mouseenter", () => { ReaderMode.showModeSelector(); });
		});

		//	Show/hide the button on scroll up/down.
		addScrollListener(ReaderMode.updateModeSelectorVisibility,
			"ReaderMode.updateModeSelectorVisibilityScrollListener");
	},

	//	Called by: ReaderMode.setMode
	//	Called by: ReaderMode.deactivateOnScrollDownObserver callback
	updateModeSelectorState: () => {
		GWLog("ReaderMode.updateModeSelectorState", "reader-mode.js", 2);

		/*	If the mode selector has not yet been injected, then do nothing.
		 */
		if (ReaderMode.modeSelector == null)
			return;

		//	Get saved mode setting (or default).
		let currentMode = ReaderMode.currentMode();

		//	Clear current buttons state.
		ReaderMode.modeSelector.querySelectorAll("button").forEach(button => {
			button.classList.remove("active", "selected");
			button.disabled = false;
		});

		//	Set the correct button to be selected.
		ReaderMode.modeSelector.querySelectorAll(`.select-mode-${currentMode}`).forEach(button => {
			button.classList.add("selected");
			button.disabled = true;
		});

		/*	Ensure the right button (on or off) has the “currently active”
			indicator, if the current mode is ‘auto’.
		 */
		if (currentMode == "auto")
			ReaderMode.modeSelector.querySelector(`.select-mode-${(ReaderMode.active ? "on" : "off")}`).classList.add("active");
	},

	//	Called by: ReaderMode.updateModeSelectorVisibilityScrollListener
	updateModeSelectorVisibility: () => {
		GWLog("ReaderMode.updateModeSelectorVisibility", "reader-mode.js", 3);

		if (ReaderMode.modeSelector == null)
			return;

	    //	Hide mode selector when scrolling a full page down.
		if (GW.scrollState.unbrokenDownScrollDistance > window.innerHeight)
			ReaderMode.hideModeSelector();

		/*	On desktop, show mode selector when scrolling to top of page, or a
			full page up.
			On mobile, show mode selector on ANY scroll up.
		 */
		if (GW.isMobile()) {
			if (GW.scrollState.unbrokenUpScrollDistance > 0 || GW.scrollState.lastScrollTop <= 0)
				ReaderMode.showModeSelector();
		} else if (   GW.scrollState.unbrokenUpScrollDistance > window.innerHeight
				   || GW.scrollState.lastScrollTop == 0) {
			ReaderMode.showModeSelector();
		}
	},

	//	Called by: ReaderMode.updateModeSelectorVisibility
	//	Called by: reader mode selector ‘mouseenter’ event handler
	showModeSelector: () => {
		GWLog("ReaderMode.showModeSelector", "reader-mode.js", 3);

		ReaderMode.modeSelector.classList.remove("hidden");
	},

	//	Called by: ReaderMode.updateModeSelectorVisibility
	hideModeSelector: () => {
		GWLog("ReaderMode.showModeSelector", "reader-mode.js", 3);

		ReaderMode.modeSelector.classList.add("hidden");
	},

	/***************************************************/
	/*	Activation / deactivation. (Core functionality.)
	 */

	/*	Masks links and hide other elements, as appropriate. This will hide
		linkicons and pop-frame  indicators, and will thus cause reflow.
	 */
	//	Called by: ReaderMode.setMode
	activate: () => {
		GWLog("ReaderMode.activate", "reader-mode.js", 1);

		ReaderMode.active = true;

		//	Add body class.
		document.body.classList.add("reader-mode-active");

		//	Get a list of all the links that are to be masked.
		ReaderMode.maskedLinks = ReaderMode.markdownBody.querySelectorAll(ReaderMode.maskedLinksSelector);

		//	Mask links.
		ReaderMode.maskedLinks.forEach(link => {
			//	Annotate each link with a class.
			link.classList.add("masked-link");

			//	Insert hooks for linkicons.
			link.insertAdjacentHTML("beforeend", `<span class='icon-hook'><span></span></span>`);

			/*	Add `mouseenter` / `mouseleave` listeners to show/hide masked
				links on hover.
			 */
			link.removeMouseEnterEvent = onEventAfterDelayDo(link, "mouseenter", ReaderMode.showMaskedLinksDelay, ReaderMode.updateState, "mouseleave");
			link.removeMouseLeaveEvent = onEventAfterDelayDo(link, "mouseleave", 0, ReaderMode.updateState);

			//	Add custom popup trigger delay.
			link.specialPopupTriggerDelay = () => {
				return (ReaderMode.maskedLinksVisible() == false
						? ReaderMode.adjustedPopupTriggerDelay
						: Popups.popupTriggerDelay);
			};

			//	Add custom link click behavior.
			link.onclick = (event) => { return (ReaderMode.maskedLinksVisible() == true); };
		});

		//	Inject info alert.
		ReaderMode.maskedLinksKeyToggleInfoAlert = addUIElement(`<div id='masked-links-key-toggle-info-alert'>`
			+ `<p>`
			+ `<img src='/static/img/icons/book-open-solid.svg'>`
			+ `Hold <span class="key">alt</span> / <span class="key">option</span> key to show links</p>`
			+ `</div>`);

		//	Add key down/up listeners, to show/hide masked links with Alt key.
		document.addEventListener("keydown", ReaderMode.altKeyDownOrUp = (event) => {
			if (event.key != "Alt")
				return;

			ReaderMode.updateState(event);
		});
		document.addEventListener("keyup", ReaderMode.altKeyDownOrUp);

		/*	Create intersection observer to automatically unmask links when
			page is scrolled down to a specified location (element).
		 */
		if (ReaderMode.currentMode() == "auto") {
			ReaderMode.deactivateOnScrollDownObserver = new IntersectionObserver((entries, observer) => {
				entries.forEach(entry => {
					if (entry.isIntersecting == false)
						return;

					ReaderMode.deactivate();
					ReaderMode.updateModeSelectorState();
					ReaderMode.deactivateOnScrollDownObserver.disconnect();
					ReaderMode.deactivateOnScrollDownObserver = null;
				});
			}, { threshold: 1.0 });
			ReaderMode.deactivateOnScrollDownObserver.observe(document.querySelector(ReaderMode.deactivateTriggerElementSelector));
		}

		//	Update visual state.
		ReaderMode.updateVisibility({ maskedLinksVisible: false, maskedLinksKeyToggleInfoAlertVisible: false });
	},

	/*	Unmasks links and reveal other elements, as appropriate. This will
		un-hide linkicons and pop-frame indicators, and will thus cause reflow.
	 */
	//	Called by: ReaderMode.setMode
	//	Called by: ReaderMode.deactivateOnScrollDownObserver callback
	deactivate: () => {
		GWLog("ReaderMode.deactivate", "reader-mode.js", 1);

		ReaderMode.active = false;

		//	Remove body class.
		document.body.classList.remove("reader-mode-active");

		//	Show masked links.
		ReaderMode.showMaskedLinks();

		//	Remove info alert.
		document.querySelectorAll("#masked-links-key-toggle-info-alert").forEach(alert => { alert.remove() });

		/*	Unmask every masked link. (Note that ReaderMode.maskedLinks is a
			NodeList, returned by a querySelectorAll call in
			ReaderMode.maskLinks. If that function has never been called, then
			ReaderMode.maskedLinks will be null).
		 */
		ReaderMode.maskedLinks.forEach(link => {
			link.classList.remove("masked-link");

			//	Extract hooks.
			link.querySelectorAll(".icon-hook").forEach(hook => { hook.remove() });

			//	Remove `mouseenter` / `mouseleave` listeners from the link.
			link.removeMouseEnterEvent();
			link.removeMouseLeaveEvent();
			link.removeMouseEnterEvent = null;
			link.removeMouseLeaveEvent = null;

			//	Remove custom popup trigger delay.
			link.specialPopupTriggerDelay = null;

			//	Re-enable normal link click behavior.
			link.onclick = null;
		});

		//	Remove key down/up listeners (for the Alt key toggle).
		document.removeEventListener("keydown", ReaderMode.altKeyDownOrUp);
		document.removeEventListener("keyup", ReaderMode.altKeyDownOrUp);
		ReaderMode.altKeyDownOrUp = null;
	},

	/****************/
	/*	Link masking.
	 */

	/*	Returns true if masked links (if any) are currently visible, false
		otherwise.
	 */
	maskedLinksVisible: () => {
		return (ReaderMode.markdownBody.classList.contains("masked-links-hidden") == false);
	},

	/*	Hides masked links (if any).
	 */
	hideMaskedLinks: () => {
		GWLog("ReaderMode.hideMaskedLinks", "reader-mode.js", 2);

		/*	Hide masked links. (Because linkicons and pop-frame indicators are
			already hidden, this causes no reflow).
		 */
		ReaderMode.markdownBody.classList.add("masked-links-hidden");
	},

	/*	Unhides masked links (if any).
	 */
	showMaskedLinks: () => {
		GWLog("ReaderMode.showMaskedLinks", "reader-mode.js", 2);

		/*	Unhide masked links. (This does not reveal linkicons or pop-frame
			indicators, and thus causes no reflow).
		 */
		ReaderMode.markdownBody.classList.remove("masked-links-hidden");
	},

	/*******************************/
	/*	Key toggle info alert panel.
	 */

	/*	Hide key toggle info alert.
	 */
	hideKeyToggleInfoAlert: () => {
		GWLog("ReaderMode.hideKeyToggleInfoAlert", "reader-mode.js", 2);

		ReaderMode.maskedLinksKeyToggleInfoAlert.classList.add("hidden");
	},

	/*	Show key toggle info alert.
	 */
	showKeyToggleInfoAlert: () => {
		GWLog("ReaderMode.showKeyToggleInfoAlert", "reader-mode.js", 2);

		ReaderMode.maskedLinksKeyToggleInfoAlert.classList.remove("hidden");
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
			case 'mouseenter':
				ReaderMode.state.hoveringOverLink = true;
				break;
			case 'mouseleave':
				ReaderMode.state.hoveringOverLink = false;
				break;
			case 'keydown':
				ReaderMode.state.altKeyPressed = true;
				break;
			case 'keyup':
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
			ReaderMode.showMaskedLinks();
		} else if (   update.maskedLinksVisible == false
				   && ReaderMode.maskedLinksVisible() == true) {
			ReaderMode.hideMaskedLinks();
		}

		//	Likewise, show or hide the key toggle info alert panel, as needed.
		if (update.maskedLinksKeyToggleInfoAlertVisible) {
			ReaderMode.showKeyToggleInfoAlert();
		} else {
			ReaderMode.hideKeyToggleInfoAlert();
		}
	},
};

ReaderMode.setup();
