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
    stylesID: "popups-styles",
    popupContainerID: "popup-container",
    popupContainerParentSelector: "html",
    popupContainerZIndex: "1000",

    popupBreathingRoomX: 24.0,
    popupBreathingRoomY: 16.0,

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
	cleanup: () => {
		GWLog("Popups.cleanup", "footnotes.js", 1);

        //  Remove popups container and injected styles.
        document.querySelectorAll(`#${Popups.stylesID}, #${Popups.popupContainerID}`).forEach(element => element.remove());
	},
	setup: () => {
		GWLog("Popups.setup", "footnotes.js", 1);

        //  Run cleanup.
        Popups.cleanup();

        //  Inject styles.
        document.querySelector("head").insertAdjacentHTML("beforeend", Popups.stylesHTML);

        //  Inject popups container.
        let popupContainerParent = document.querySelector(Popups.popupContainerParentSelector);
        if (!popupContainerParent) {
            GWLog("Popup container parent element not found. Exiting.", "footnotes.js", 1);
            return;
        }
        popupContainerParent.insertAdjacentHTML("beforeend", `<div id='${Popups.popupContainerID}' style='z-index: ${Popups.popupContainerZIndex};'></div>`);
        requestAnimationFrame(() => {
            Popups.popupContainer = document.querySelector(`#${Popups.popupContainerID}`);
        });
	},
	newPopup: (id = "popup") => {
		let popup = document.createElement('div');
		popup.id = id;
		popup.classList.toggle('popupdiv', true);
		return popup;
	},
	spawnPopup: (popup, target, event) => {
		//	Inject the popup into the page.
		Popups.injectPopup(popup);

		//  Position the popup appropriately with respect to the target.
		Popups.positionPopup(popup, target, event);
	},
	injectPopup: (popup) => {
		Popups.popupContainer.appendChild(popup);
	},
	positionPopup: (popup, target, event) => {
		let popupContainerViewportRect = Popups.popupContainer.getBoundingClientRect();
		let targetViewportRect = target.getBoundingClientRect();
		let targetOriginInPopupContainer = {
			x: (targetViewportRect.left - popupContainerViewportRect.left),
			y: (targetViewportRect.top - popupContainerViewportRect.top)
		};
		let mouseEnterEventPositionInPopupContainer = {
			x: (event.clientX - popupContainerViewportRect.left),
			y: (event.clientY - popupContainerViewportRect.top)
		};

		//  Wait for the "naive" layout to be completed, and then...
		requestAnimationFrame(() => {
			/*  How much "breathing room" to give the target (i.e., offset of
				the popup).
				*/
			var popupBreathingRoom = {
				x: Popups.popupBreathingRoomX,
				y: Popups.popupBreathingRoomY
			};

			/*  This is the width and height of the popup, as already determined
				by the layout system, and taking into account the popup's content,
				and the max-width, min-width, etc., CSS properties.
				*/
			var popupIntrinsicWidth = popup.clientWidth;
			var popupIntrinsicHeight = popup.clientHeight;

			var provisionalPopupXPosition;
			var provisionalPopupYPosition;

			var tocLink = target.closest("#TOC");
			if (tocLink) {
				provisionalPopupXPosition = document.querySelector("#TOC").getBoundingClientRect().right + 1.0 - popupContainerViewportRect.left;
				provisionalPopupYPosition = mouseEnterEventPositionInPopupContainer.y - ((event.clientY / window.innerHeight) * popupIntrinsicHeight);
			} else {
				var offToTheSide = false;

				/*  Can the popup fit above the target? If so, put it there.
					Failing that, can it fit below the target? If so, put it there.
					*/
				var popupSpawnYOriginForSpawnAbove = Math.min(mouseEnterEventPositionInPopupContainer.y - popupBreathingRoom.y,
															  targetOriginInPopupContainer.y + targetViewportRect.height - (popupBreathingRoom.y * 2.0));
				var popupSpawnYOriginForSpawnBelow = Math.max(mouseEnterEventPositionInPopupContainer.y + popupBreathingRoom.y,
															  targetOriginInPopupContainer.y + (popupBreathingRoom.y * 2.0));
				if (  popupSpawnYOriginForSpawnAbove - popupIntrinsicHeight >= popupContainerViewportRect.y * -1) {
					//  Above.
					provisionalPopupYPosition = popupSpawnYOriginForSpawnAbove - popupIntrinsicHeight;
				} else if (popupSpawnYOriginForSpawnBelow + popupIntrinsicHeight <= (popupContainerViewportRect.y * -1) + window.innerHeight) {
					//  Below.
					provisionalPopupYPosition = popupSpawnYOriginForSpawnBelow;
				} else {
					/*  The popup does not fit above or below! We will have to
						put it off to the left or right.
						*/
					offToTheSide = true;
				}

				if (offToTheSide) {
					popupBreathingRoom.x *= 2.0;
					provisionalPopupYPosition = mouseEnterEventPositionInPopupContainer.y - ((event.clientY / window.innerHeight) * popupIntrinsicHeight);
					if (provisionalPopupYPosition - popupContainerViewportRect.y < 0)
						provisionalPopupYPosition = 0.0;

					//  Determine whether to put the popup off to the right, or left.
					if (  mouseEnterEventPositionInPopupContainer.x
						+ popupBreathingRoom.x
						+ popupIntrinsicWidth
						  <=
						  popupContainerViewportRect.x * -1
						+ window.innerWidth) {
						//  Off to the right.
						provisionalPopupXPosition = mouseEnterEventPositionInPopupContainer.x + popupBreathingRoom.x;
					} else if (  mouseEnterEventPositionInPopupContainer.x
							   - popupBreathingRoom.x
							   - popupIntrinsicWidth
								 >=
								 popupContainerViewportRect.x * -1) {
						//  Off to the left.
						provisionalPopupXPosition = mouseEnterEventPositionInPopupContainer.x - popupIntrinsicWidth - popupBreathingRoom.x;
					}
				} else {
					/*  Place popup off to the right (and either above or below),
						as per the previous block of code.
						*/
					provisionalPopupXPosition = mouseEnterEventPositionInPopupContainer.x + popupBreathingRoom.x;
				}
			}

			/*  Does the popup extend past the right edge of the container?
				If so, move it left, until its right edge is flush with
				the container's right edge.
				*/
			if (provisionalPopupXPosition + popupIntrinsicWidth > popupContainerViewportRect.width) {
				provisionalPopupXPosition -= provisionalPopupXPosition + popupIntrinsicWidth - popupContainerViewportRect.width;
			}

			/*  Now (after having nudged the popup left, if need be),
				does the popup extend past the *left* edge of the container?
				Make its left edge flush with the container's left edge.
				*/
			if (provisionalPopupXPosition < 0) {
				provisionalPopupXPosition = 0;
			}

			popup.style.left = `${provisionalPopupXPosition}px`;
			popup.style.top = `${provisionalPopupYPosition}px`;

			document.activeElement.blur();
		});
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

/********************/
/*	Essential styles.
	*/
Popups.stylesHTML = `<style id='${Popups.stylesID}'>
#${Popups.popupContainerID} {
    position: absolute;
    left: 0;
    top: 0;
    width: 100%;
    pointer-events: none;
}
#${Popups.popupContainerID} > * {
    pointer-events: auto;
}
#popupdiv {
    position: absolute;
    opacity: 1.0;
    transition: none;
}
#popupdiv.fading {
    opacity: 0.0;
    transition:
        opacity 0.25s ease-in 0.1s;
}
#popupdiv > div {
    overflow: auto;
    overscroll-behavior: none;
}
</style>`;

doWhenPageLoaded(() => {
	GW.notificationCenter.fireEvent("Popups.loaded");

	Popups.setup();
});

Footnotes = {
	/**********/
	/*	Config.
		*/
    stylesID: "footnotes-styles",

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
    cleanup: () => {
		GWLog("Footnotes.cleanup", "footnotes.js", 1);

        //  Unbind event listeners.
        Footnotes.unbind();

        //  Remove popups container and injected styles.
        document.querySelectorAll(`#${Footnotes.stylesID}`).forEach(element => element.remove());
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

        //  Inject styles.
        document.querySelector("head").insertAdjacentHTML("beforeend", Footnotes.stylesHTML);

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
			Popups.spawnPopup(Footnotes.popup, target, event);
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

/********************/
/*	Essential styles.
	*/
Footnotes.stylesHTML = `<style id='${Popups.stylesID}'>
</style>`;

doWhenPageLoaded(() => {
	GW.notificationCenter.fireEvent("Footnotes.loaded");

	Footnotes.setup();
});
