ReaderMode = {
	/*****************/
	/*	Configuration.
	 */
	maskedLinksSelector: "p a, li a",
	maskedLinksParentBlockSelector: "p, li",

	unmaskLinksTriggerElementSelector: "#reader-mode-disable-when-here, #see-also, #external-links, #appendix, #appendices, #navigation, #footer",

	showMaskedLinksDelay: 250,

	adjustedPopupTriggerDelay: 2400,

	/******************/
	/*	Infrastructure.
	 */
	markdownBody: document.querySelector("#markdownBody"),

	maskedLinksStyleBlock: null,

	maskedLinksKeyToggleInfoAlert: null,

	state: {
		hoveringOverLink: false,
		altKeyPressed: false
	},

	/*************/
	/*	Functions.
	 */

	/*	Enables reader mode.
	 */
	setup: () => {
		ReaderMode.maskLinks();
		ReaderMode.updateVisibility({ maskedLinksVisible: false, maskedLinksKeyToggleInfoAlertVisible: false });
	},

	/*	Masks links, as appropriate. This will hide linkicons and pop-frame 
		indicators, and will thus cause reflow.
	 */
	maskLinks: () => {
		/*	Get a list of all the links that are to be masked.
		 */
		ReaderMode.maskedLinks = ReaderMode.markdownBody.querySelectorAll(ReaderMode.maskedLinksSelector);

		/*	Mask links.
		 */
		ReaderMode.maskedLinks.forEach(link => {
			/*	Annotate each link with a class.
			 */
			link.classList.add("masked-link");

			/*	Insert hooks for linkicons.
			 */
			link.insertAdjacentHTML("beforeend", `<span class='icon-hook'><span></span></span>`);

			/*	Add `mouseenter` / `mouseleave` listeners to show/hide masked 
				links on hover.
			 */
			link.removeMouseEnterEvent = onEventAfterDelayDo(link, "mouseenter", ReaderMode.showMaskedLinksDelay, ReaderMode.updateState, "mouseleave");
			link.removeMouseLeaveEvent = onEventAfterDelayDo(link, "mouseleave", 0, ReaderMode.updateState);

			/*	Add custom popup trigger delay.
			 */
			link.specialPopupTriggerDelay = () => {
				return (ReaderMode.maskedLinksVisible() == false 
						? ReaderMode.adjustedPopupTriggerDelay 
						: Popups.popupTriggerDelay);
			};

			/*	Add custom link click behavior.
			 */
			link.onclick = (event) => { return (ReaderMode.maskedLinksVisible() == true); };
		});

		/*	Inject style block.
		 */
		document.querySelector("head").insertAdjacentHTML("beforeend", `<style id='masked-links-styles'>
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
			.markdownBody .masked-link.masked-links-hidden .indicator-hook,
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
				bottom: 1em;
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
		</style>`);

		/*	Add info alert.
		 */
		ReaderMode.maskedLinksKeyToggleInfoAlert = addUIElement(`<div id='masked-links-key-toggle-info-alert'><p>Hold <span class="key">alt</span> / <span class="key">option</span> key to show links</p></div>`);

		/*	Add key down/up listeners, to show/hide masked links with Alt key.
		 */
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

				ReaderMode.unmaskLinks();
				observer.disconnect();
			});
		}, { threshold: 1.0 });
		observer.observe(document.querySelector(ReaderMode.unmaskLinksTriggerElementSelector));
	},

	/*	Unmasks links. This will un-hide linkicons and pop-frame indicators, and
		will thus cause reflow.
	 */
	unmaskLinks: () => {
		ReaderMode.showMaskedLinks();

		/*	Remove style block (if present).
		 */
		document.querySelectorAll("#masked-links-styles").forEach(styles => { styles.remove() });

		/*	Remove info alert.
		 */
		document.querySelectorAll("#masked-links-key-toggle-info-alert").forEach(alert => { alert.remove() });

		/*	Unmask every masked link. (Note that ReaderMode.maskedLinks is a 
			NodeList, returned by a querySelectorAll call in 
			ReaderMode.maskLinks. If that function has never been called, then 
			ReaderMode.maskedLinks will be null).
		 */
		ReaderMode.maskedLinks.forEach(link => {
			link.classList.remove("masked-link");

			/*	Extract hooks.
			 */
			link.querySelectorAll(".icon-hook").forEach(hook => { hook.remove() });

			/*	Remove `mouseenter` / `mouseleave` listeners from the link.
			 */
			link.removeMouseEnterEvent();
			link.removeMouseLeaveEvent();
			link.removeMouseEnterEvent = null;
			link.removeMouseLeaveEvent = null;

			/*	Remove custom popup trigger delay.
			 */
			link.specialPopupTriggerDelay = null;

			/*	Re-enable normal link click behavior.
			 */
			link.onclick = null;
		});

		/*	Remove key down/up listeners (for the Alt key toggle).
		 */
		document.removeEventListener("keydown", ReaderMode.altKeyDownOrUp);
		document.removeEventListener("keyup", ReaderMode.altKeyDownOrUp);
		ReaderMode.altKeyDownOrUp = null;
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
		/*	Hide masked links. (Because linkicons and pop-frame indicators are
			already hidden, this causes no reflow).
		 */
		ReaderMode.markdownBody.classList.add("masked-links-hidden");
	},

	/*	Unhides masked links (if any).
	 */
	showMaskedLinks: () => {
		/*	Unhide masked links. (This does not reveal linkicons or pop-frame
			indicators, and thus causes no reflow).
		 */
		ReaderMode.markdownBody.classList.remove("masked-links-hidden");
	},

	/*	Hide key toggle info alert.
	 */
	hideKeyToggleInfoAlert: () => {
		ReaderMode.maskedLinksKeyToggleInfoAlert.classList.add("hidden");
	},

	/*	Show key toggle info alert.
	 */
	showKeyToggleInfoAlert: () => {
		ReaderMode.maskedLinksKeyToggleInfoAlert.classList.remove("hidden");
	},

	/*	Update state after an event that might cause a visibility change.
	 */
	updateState: (event) => {
		/*	Update tracked state.
		 */
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

		/*	Request the desired visibility update.
		 */
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

		/*	Likewise, show or hide the key toggle info alert panel, as needed.
		 */
		if (update.maskedLinksKeyToggleInfoAlertVisible) {
			ReaderMode.showKeyToggleInfoAlert();
		} else {
			ReaderMode.hideKeyToggleInfoAlert();
		}
	},
};

ReaderMode.setup();
