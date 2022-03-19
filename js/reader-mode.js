ReaderMode = {
	/*****************/
	/*	Configuration.
	 */
	maskedLinksSelector: "p a, li a",
	maskedLinksParentBlockSelector: "p, li",

	deactivateTriggerElementSelector: "#reader-mode-disable-when-here, #see-also, #external-links, #appendix, #appendices, #navigation, #footer",

	showMaskedLinksDelay: 250,

	adjustedPopupTriggerDelay: 2400,

	styles: `
		body.reader-mode-active #page-metadata,
		body.reader-mode-active #sidebar-links,
		body.reader-mode-active #TOC ul li::before,
        body.reader-mode-active #footer,
        body.reader-mode-active #navigation { {
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
		#reader-mode-toggle-button {
			position: fixed;
			top: calc(0.5em + 3px);
			right: 12em;
			z-index: 1;
			opacity: 0.25;
			visibility: visible;
			transition:
				opacity 2s ease;
		}
		@media only screen and (max-width: 1760px) {
			#reader-mode-toggle-button {
				top: 7em;
				right: 0.6em;
			}
		}
		#reader-mode-toggle-button.hidden {
			opacity: 0;
		}
		#reader-mode-toggle-button:hover {
			opacity: 1.0;
			transition: none;
		}
		#reader-mode-toggle-button button {
			-moz-appearance: none;
			appearance: none;
			border: none;
			font-family: inherit;
			font-size: inherit;
			background: inherit;
			color: var(--GW-popups-show-popup-options-dialog-button-color);
			fill: currentColor;
			opacity: 0.47;
			padding: 0;
			line-height: 1;
			display: block;
			width: 1.125em;
		}
		#reader-mode-toggle-button button:hover {
			cursor: pointer;
			opacity: 1.0;
		}
		#reader-mode-toggle-button button:active {
			transform: scale(0.95);
		}
	`,

	/******************/
	/*	Infrastructure.
	 */
	markdownBody: document.querySelector("#markdownBody"),

	maskedLinksKeyToggleInfoAlert: null,

	active: false,

	toggleButton: null,
	toggleButtonInteractable: true,

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

		//	Inject toggle button.
		ReaderMode.injectToggleButton();

		//	Inject style block.
		document.querySelector("head").insertAdjacentHTML("beforeend", `<style id='reader-mode-styles'>${ReaderMode.styles}</style>`);

		//	Activate immediately, if need be.
		if (ReaderMode.enabled())
			ReaderMode.activate();
	},

	/*	Returns true if reader mode is set to be enabled for the current page,
		false otherwise.
	 */
    enabled: () => {
        return (   localStorage.getItem("reader-mode-enabled") == "true"
        		|| (   document.body.classList.contains("reader-mode")
        			&& localStorage.getItem("reader-mode-enabled") != "false"))
    },

	//	Called by: ReaderMode.setup
	injectToggleButton: () => {
		GWLog("ReaderMode.injectToggleButton", "reader-mode.js", 1);

		//  Create and inject the button.
		ReaderMode.toggleButton = addUIElement(`<div id="reader-mode-toggle-button">`
			+ `<button type="button"><img></button>`
			+ `</div>`);

		//  Add event listeners and update state.
		requestAnimationFrame(() => {
			//	Set initial toggle button state properly.
			ReaderMode.updateToggleButtonState();

			//	Button’s click handler.
			ReaderMode.toggleButton.querySelector("button").addActivateEvent(ReaderMode.toggleButtonClicked = (event) => {
				GWLog("ReaderMode.toggleButtonClicked", "reader-mode.js", 2);

				event.stopPropagation();

				/*	We don’t want clicks to go through if the transition between
					modes has not completed yet.
				 */
				if (ReaderMode.toggleButtonInteractable == false)
					return;

				/*	Disable the button temporarily while we’re transitioning
					between modes.
				 */
				ReaderMode.toggleButtonInteractable = false;

				//	Switch modes; activate if inactive, deactivate otherwise.
				if (ReaderMode.active) {
					ReaderMode.deactivate();
				} else {
					ReaderMode.activate();
				}

				//	Re-enable button after the mode switch is complete.
				requestAnimationFrame(() => {
					ReaderMode.toggleButtonInteractable = true;
				});
			});

			//	Show the button on hover (if it’s hid via scroll-down).
			ReaderMode.toggleButton.addEventListener("mouseenter", () => { ReaderMode.showToggleButton(); });
		});

		//	Show/hide the button on scroll up/down.
		addScrollListener(ReaderMode.updateToggleButtonVisibility,
			"ReaderMode.updateToggleButtonVisibilityScrollListener");
	},

	//	Called by: ReaderMode.injectToggleButton
	//	Called by: ReaderMode.toggleButtonClicked
	updateToggleButtonState: () => {
		let button = ReaderMode.toggleButton.querySelector("button");
		button.title = ReaderMode.active
					   ? "Disable reader mode"
					   : "Enable reader mode";

		let buttonImage = button.firstElementChild;
		buttonImage.src = ReaderMode.active
						  ? "/static/img/icons/book-open-solid.svg"
						  : "/static/img/icons/book-open.svg";
	},

	//	Called by: ReaderMode.updateToggleButtonVisibilityScrollListener
	updateToggleButtonVisibility: () => {
		GWLog("ReaderMode.updateToggleButtonVisibility", "reader-mode.js", 3);

		if (ReaderMode.toggleButton == null)
			return;

		// Hide button when scrolling a full page down.
		if (GW.scrollState.unbrokenDownScrollDistance > window.innerHeight)
			ReaderMode.hideToggleButton();

		// Show back-to-top link on ANY scroll up.
		if (GW.scrollState.unbrokenUpScrollDistance > window.innerHeight || GW.scrollState.lastScrollTop <= 0)
			ReaderMode.showToggleButton();
	},

	//	Called by: ReaderMode.updateToggleButtonVisibility
	//	Called by: reader mode toggle button ‘mouseenter’ event handler
	showToggleButton: () => {
		ReaderMode.toggleButton.classList.remove("hidden");
	},

	//	Called by: ReaderMode.updateToggleButtonVisibility
	hideToggleButton: () => {
		ReaderMode.toggleButton.classList.add("hidden");
	},

	/*	Masks links and hide other elements, as appropriate. This will hide
		linkicons and pop-frame  indicators, and will thus cause reflow.
	 */
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

		//	Add info alert.
		ReaderMode.maskedLinksKeyToggleInfoAlert = addUIElement(`<div id='masked-links-key-toggle-info-alert'><p>Hold <span class="key">alt</span> / <span class="key">option</span> key to show links</p></div>`);

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
		let observer = new IntersectionObserver((entries, observer) => {
			entries.forEach(entry => {
				if (entry.isIntersecting == false)
					return;

				ReaderMode.deactivate();
				observer.disconnect();
			});
		}, { threshold: 1.0 });
		observer.observe(document.querySelector(ReaderMode.deactivateTriggerElementSelector));

		//	Update visual state.
		ReaderMode.updateVisibility({ maskedLinksVisible: false, maskedLinksKeyToggleInfoAlertVisible: false });

		//	Update toggle button state.
		ReaderMode.updateToggleButtonState();
	},

	/*	Unmasks links and reveal other elements, as appropriate. This will
		un-hide linkicons and pop-frame indicators, and will thus cause reflow.
	 */
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

		//	Update toggle button state.
		ReaderMode.updateToggleButtonState();
	},

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

	/*	Update state after an event that might cause a visibility change.
	 */
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
