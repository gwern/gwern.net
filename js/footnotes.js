/*	Popup/floating footnotes to avoid readers needing to scroll to the end of
	the page to see any footnotes; see
	http://ignorethecode.net/blog/2010/04/20/footnotes/ for details.
Original author:  Lukas Mathis (2010-04-20)
License: public domain ("And some people have asked me about a license for this piece of code. I think it’s far too short to get its own license, so I’m relinquishing any copyright claims. Consider the code to be public domain. No attribution is necessary.")
	*/
Popups = {
	/**********/
	/*	Config.
		*/
	popupContainerSelector: "#markdownBody",

	minFootnoteWidth: 520,

	/******************/
	/*	Implementation.
		*/
	popupContainer: null,

	isMobile: () => {
		/*  We consider a client to be mobile if one of two conditions obtain:
		    1. JavaScript detects touch capability, AND viewport is narrow; or,
		    2. CSS does NOT detect hover capability.
		    */
		return (   (   ('ontouchstart' in document.documentElement)
					&& GW.mediaQueries.mobileWidth.matches)
				|| !GW.mediaQueries.hoverAvailable.matches);
	},
	setup: () => {
		Popups.popupContainer = document.querySelector(Popups.popupContainerSelector);
	},
	newPopup: (id = "popup") => {
		let popup = document.createElement('div');
		popup.id = id;
		return popup;
	},
	spawnPopup: (popup, target) => {
		//	Inject the popup into the page.
		Popups.injectPopup(popup);

		//  Position the popup appropriately with respect to the target.
		Popups.positionPopup(popup, target);
	},
	injectPopup: (popup) => {
		Popups.popupContainer.appendChild(popup);
	},
	positionPopup: (popup, target) => {
		let targetViewportRect = target.getBoundingClientRect();
		let bodyAbsoluteRect = document.body.getBoundingClientRect();
		var targetOriginInPopupContainer = {
			left: (targetViewportRect.left - bodyAbsoluteRect.left),
			top: (targetViewportRect.top - bodyAbsoluteRect.top)
		};

		/*	How much "breathing room" to give the footnote reference (i.e.,
			offset of the footnote popup).
			*/
		var popupBreathingRoom = {
			x:	(Math.round(targetViewportRect.width) * 1.5),
			y:	Math.round(targetViewportRect.height) + (Math.round(targetViewportRect.width) * 0.5)
		};

		/*	Set the horizontal position first; this causes the popup to be laid
			out, and the layout engine calculates the height for us.
			*/
		var footnotePopupLeft = targetOriginInPopupContainer.left + popupBreathingRoom.x;
		if (footnotePopupLeft + Popups.minFootnoteWidth > window.innerWidth)
			footnotePopupLeft = window.innerWidth - Popups.minFootnoteWidth;
		popup.style.left = footnotePopupLeft + "px";
		//	Correct for various positioning aberrations.
		if (popup.getBoundingClientRect().right > window.innerWidth)
			popup.style.maxWidth = (popup.clientWidth - (popup.getBoundingClientRect().right - window.innerWidth) - parseInt(getComputedStyle(popup.firstElementChild).paddingRight)) + "px";
		else if (targetOriginInPopupContainer.left + popupBreathingRoom.x + popup.clientWidth < window.innerWidth)
			popup.style.left = (targetOriginInPopupContainer.left + popupBreathingRoom.x) + "px";
		else if (targetOriginInPopupContainer.left - (popupBreathingRoom.x + popup.clientWidth) > popup.getBoundingClientRect().left)
			popup.style.left = (targetOriginInPopupContainer.left - popupBreathingRoom.x - popup.clientWidth) + "px";

		//	Now we know how tall the popup is...
		var provisionalFootnotePopupHeight = popup.clientHeight;

		//	Determining vertical position is full of edge cases.
		var footnotePopupTop = targetOriginInPopupContainer.top + popupBreathingRoom.y;
		if (footnotePopupTop + provisionalFootnotePopupHeight > window.innerHeight + window.scrollY) {
			footnotePopupTop -= (provisionalFootnotePopupHeight + popupBreathingRoom.y);
		}
		if (top + provisionalFootnotePopupHeight > window.innerHeight + window.scrollY ||
			provisionalFootnotePopupHeight == window.innerHeight ||
			footnotePopupTop < window.scrollY) {
			footnotePopupTop = window.scrollY;
		}
		if (footnotePopupTop + provisionalFootnotePopupHeight + 120 < targetOriginInPopupContainer.top) {
			footnotePopupTop = targetOriginInPopupContainer.top - provisionalFootnotePopupHeight;
		} else if (top > targetOriginInPopupContainer.top) {
			footnotePopupTop -= 90;
		}
		if (footnotePopupTop < 0) {
			footnotePopupTop = 0;
		}
		popup.style.top = footnotePopupTop + "px";
	},
    despawnPopup: (popup) => {
		GWLog("Popups.despawnPopup", "footnotes.js", 2);

		if (popup == null)
			return;

	    popup.classList.remove("fading");
        popup.remove();
        document.activeElement.blur();
    }
};

doWhenPageLoaded(() => {
	GW.notificationCenter.fireEvent("Popups.loaded");

	Popups.setup();
});

Footnotes = {
	/**********/
	/*	Config.
		*/
    targetElementsSelector: ".footnote-ref",
    excludedElementsSelector: null,
    excludedContainerElementsSelector: null,

    popupTriggerDelay: 200,
    popupFadeoutDelay: 50,
    popupFadeoutDuration: 250,

	/******************/
	/*	Implementation.
		*/
	popupFadeTimer: false,
	popupDespawnTimer: false,
	popupSpawnTimer: false,
	popup: null,

	unbind: () => {
		GWLog("Footnotes.unbind", "footnotes.js", 1);

		document.querySelectorAll(Footnotes.targetElementsSelector).forEach(target => {
			if (   target.closest(Footnotes.excludedElementsSelector) == target
				|| target.closest(Footnotes.excludedContainerElementsSelector) != null)
				return;

			//	Unbind existing mouseenter/mouseleave events, if any.
			target.removeEventListener("mouseenter", Footnotes.targetMouseenter);
			target.removeEventListener("mouseleave", Footnotes.targetMouseleave);
		});

		GW.notificationCenter.fireEvent("Footnotes.eventsUnbound");
	},
	setup: () => {
		GWLog("Footnotes.setup", "footnotes.js", 1);

		Footnotes.unbind();

        if (Popups.isMobile()) {
            GWLog("Mobile client detected. Exiting.", "footnotes.js", 1);
            return;
        } else {
            GWLog("Non-mobile client detected. Setting up.", "footnotes.js", 1);
        }

		//	Get all targets.
		document.querySelectorAll(Footnotes.targetElementsSelector).forEach(target => {
			if (   target.closest(Footnotes.excludedElementsSelector) == target
				|| target.closest(Footnotes.excludedContainerElementsSelector) != null)
				return;

			//	Bind mouseenter/mouseleave events.
			target.addEventListener("mouseenter", Footnotes.targetMouseenter);
			target.addEventListener("mouseleave", Footnotes.targetMouseleave);
		});

		GW.notificationCenter.fireEvent("Footnotes.setupComplete");
	},
	fillPopup: (popup, target) => {
		if (!target.hash)
			return false;

		let targetFootnoteId = target.hash.substr(1);
		let targetFootnote = document.querySelector("#" + targetFootnoteId);
		if (!targetFootnote)
			return false;

		popup.innerHTML = '<div>' + targetFootnote.innerHTML + '</div>';
		popup.dataset.footnoteReference = targetFootnoteId;
		return true;
	},
	preparePopup: (popup) => {
		//	Add event listeners.
		popup.addEventListener("mouseup", Footnotes.popupMouseup);
		popup.addEventListener("mouseenter", Footnotes.popupMouseenter);
		popup.addEventListener("mouseleave", Footnotes.popupMouseleave);
	},
	//	The mouseenter event.
	targetMouseenter: (event) => {
		GWLog("Footnotes.targetMouseenter", "footnotes.js", 2);

        //  Get the target.
        let target = event.target.closest(Footnotes.targetElementsSelector);

		//	Stop the countdown to un-pop the popup.
		Footnotes.clearPopupTimers();

		Footnotes.popupSpawnTimer = setTimeout(() => {
			GWLog("Footnotes.popupSpawnTimer fired", "footnotes.js", 2);

			//  Despawn existing popup, if any.
			Popups.despawnPopup(Footnotes.popup);

            //  Create the new popup.
			Footnotes.popup = Popups.newPopup("footnotediv");

			//	Inject the contents of the footnote into the popup.
			if (Footnotes.fillPopup(Footnotes.popup, target) == false)
				return;

			// Prepare the newly created and filled popup for spawning.
			Footnotes.preparePopup(Footnotes.popup);

			// Spawn the prepared popup.
			Popups.spawnPopup(Footnotes.popup, target);
		}, Footnotes.popupTriggerDelay);
	},
	//	The mouseleave event.
	targetMouseleave: (event) => {
		GWLog("Footnotes.targetMouseleave", "footnotes.js", 2);

		Footnotes.clearPopupTimers();

		if (!Footnotes.popup) return;

		Footnotes.setPopupFadeTimer();
	},
    //	The “user moved mouse out of popup” mouseleave event.
	popupMouseleave: (event) => {
		GWLog("Footnotes.popupMouseleave", "footnotes.js", 2);

		Footnotes.clearPopupTimers();
		
		Footnotes.setPopupFadeTimer();
	},
	//	The “user moved mouse back into popup” mouseenter event.
	popupMouseenter: (event) => {
		GWLog("Footnotes.popupMouseenter", "footnotes.js", 2);

		Footnotes.clearPopupTimers();
		Footnotes.popup.classList.remove("fading");
	},
    popupMouseup: (event) => {
		GWLog("Footnotes.popupMouseup", "footnotes.js", 2);

		event.stopPropagation();

		Footnotes.clearPopupTimers();
		Popups.despawnPopup(Footnotes.popup);
    },
    clearPopupTimers: () => {
	    GWLog("Footnotes.clearPopupTimers", "footnotes.js", 2);

        clearTimeout(Footnotes.popupFadeTimer);
        clearTimeout(Footnotes.popupDespawnTimer);
        clearTimeout(Footnotes.popupSpawnTimer);
    },
    setPopupFadeTimer: () => {
		GWLog("Footnotes.setPopupFadeTimer", "footnotes.js", 2);

        Footnotes.popupFadeTimer = setTimeout(() => {
			GWLog("Footnotes.popupFadeTimer fired", "footnotes.js", 2);

			Footnotes.setPopupDespawnTimer();
        }, Footnotes.popupFadeoutDelay);
    },
    setPopupDespawnTimer: () => {
		GWLog("Footnotes.setPopupDespawnTimer", "footnotes.js", 2);

		Footnotes.popup.classList.add("fading");
		Footnotes.popupDespawnTimer = setTimeout(() => {
			GWLog("Footnotes.popupDespawnTimer fired", "footnotes.js", 2);

			Popups.despawnPopup(Footnotes.popup);
		}, Footnotes.popupFadeoutDuration);
    }
};

doWhenPageLoaded(() => {
	GW.notificationCenter.fireEvent("Footnotes.loaded");

	Footnotes.setup();
});
