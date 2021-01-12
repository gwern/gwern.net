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
    popupContainerZIndex: "10001",

    popupBreathingRoomX: 12.0,
    popupBreathingRoomY: 8.0,

    popupTriggerDelay: 200,
    popupFadeoutDelay: 50,
    popupFadeoutDuration: 250,

	/******************/
	/*	Implementation.
		*/
	popupFadeTimer: false,
	popupDespawnTimer: false,
	popupSpawnTimer: false,
	popupContainer: null,

	cleanup: () => {
		GWLog("Popups.cleanup", "popups.js", 1);

        //  Remove popups container and injected styles.
        document.querySelectorAll(`#${Popups.stylesID}, #${Popups.popupContainerID}`).forEach(element => element.remove());

		//  Remove Escape key event listener.
		document.removeEventListener("keyup", Popups.keyUp);
	},
	setup: () => {
		GWLog("Popups.setup", "popups.js", 1);

        //  Run cleanup.
        Popups.cleanup();

        //  Inject styles.
        document.querySelector("head").insertAdjacentHTML("beforeend", Popups.stylesHTML);

        //  Inject popups container.
        let popupContainerParent = document.querySelector(Popups.popupContainerParentSelector);
        if (!popupContainerParent) {
            GWLog("Popup container parent element not found. Exiting.", "popups.js", 1);
            return;
        }
        popupContainerParent.insertAdjacentHTML("beforeend", `<div id='${Popups.popupContainerID}'></div>`);
        requestAnimationFrame(() => {
            Popups.popupContainer = document.querySelector(`#${Popups.popupContainerID}`);
        });

		//  Add Escape key event listener.
		document.addEventListener("keyup", Popups.keyUp = (event) => {
			GWLog("Popups.keyUp", "popups.js", 3);
			let allowedKeys = [ "Escape", "Esc" ];
			if (!allowedKeys.includes(event.key) || Popups.popupContainer.childElementCount == 0)
				return;

			event.preventDefault();

			[...Popups.popupContainer.children].forEach(popup => {
				Popups.despawnPopup(popup);
			});
		});

		GW.notificationCenter.fireEvent("Popups.setupDidComplete");
	},
	addTargetsWithin: (contentContainer, targets, prepareFunction, targetPrepareFunction = null) => {
		if (typeof contentContainer == "string")
			contentContainer = document.querySelector(contentContainer);

		if (contentContainer == null)
			return;

		//	Get all targets.
		contentContainer.querySelectorAll(targets.targetElementsSelector).forEach(target => {
			if (   target.closest(targets.excludedElementsSelector) == target
				|| target.closest(targets.excludedContainerElementsSelector) != null) {
				target.classList.toggle("no-popup", true);
				return;
			}

			if (!targets.testTarget(target)) {
				target.classList.toggle("no-popup", true);
				return;
			}

			//	Bind mouseenter/mouseleave events.
			target.addEventListener("mouseenter", Popups.targetMouseenter);
			target.addEventListener("mouseleave", Popups.targetMouseleave);

			//  Set prepare function.
			target.popupPrepareFunction = prepareFunction;

			//  Run any custom processing.
			if (targetPrepareFunction)
				targetPrepareFunction(target);

			//  Mark target as spawning a popup.
			target.classList.toggle("spawns-popup", true);
		});
	},
	addTargets: (targets, prepareFunction, targetPrepareFunction = null) => {
		GWLog("Popups.addTargets", "popups.js", 1);

		Popups.addTargetsWithin(document, targets, prepareFunction, targetPrepareFunction);
	},
	removeTargetsWithin: (contentContainer, targets, targetRestoreFunction = null) => {
		if (typeof contentContainer == "string")
			contentContainer = document.querySelector(contentContainer);

		if (contentContainer == null)
			return;

		contentContainer.querySelectorAll(targets.targetElementsSelector).forEach(target => {
			if (   target.closest(targets.excludedElementsSelector) == target
				|| target.closest(targets.excludedContainerElementsSelector) != null) {
				target.classList.toggle("no-popup", false);
				return;
			}

			if (!targets.testTarget(target)) {
				target.classList.toggle("no-popup", false);
				return;
			}

			//	Unbind existing mouseenter/mouseleave events, if any.
			target.removeEventListener("mouseenter", Popups.targetMouseenter);
			target.removeEventListener("mouseleave", Popups.targetMouseleave);

			//  Clear timers for target.
			Popups.clearPopupTimers(target);

			//  Remove spawned popup for target, if any.
			if (target.popup)
				Popups.despawnPopup(target.popup);

			//  Unset popup prepare function.
			target.popupPrepareFunction = null;

			//  Un-mark target as spawning a popup.
			target.classList.toggle("spawns-popup", false);

			//  Run any custom processing.
			if (targetRestoreFunction)
				targetRestoreFunction(target);
		});
	},
	removeTargets: (targets, targetRestoreFunction = null) => {
		GWLog("Popups.removeTargets", "popups.js", 1);

		Popups.removeTargetsWithin(document, targets, targetRestoreFunction);
	},

	newPopup: () => {
		GWLog("Popups.newPopup", "popups.js", 2);

		let popup = document.createElement("div");
		popup.classList.add("popupdiv");
		return popup;
	},
	spawnPopup: (popup, target, event) => {
		GWLog("Popups.spawnPopup", "popups.js", 2);

		//	Inject the popup into the page.
		Popups.injectPopup(popup);

		//  Position the popup appropriately with respect to the target.
		Popups.positionPopup(popup, target, event);

		//  Mark target as having an active popup associated with it.
		target.classList.add("popup-open");

		GW.notificationCenter.fireEvent("Popups.popupDidSpawn", { popup: popup });
	},
	injectPopup: (popup) => {
		GWLog("Popups.injectPopup", "popups.js", 2);

		Popups.popupContainer.appendChild(popup);

		//	Add event listeners.
		popup.addEventListener("click", Popups.popupClicked);
		popup.addEventListener("mouseenter", Popups.popupMouseenter);
		popup.addEventListener("mouseleave", Popups.popupMouseleave);
	},
	positionPopup: (popup, target, event) => {
		GWLog("Popups.positionPopup", "popups.js", 2);

		let popupContainerViewportRect = Popups.popupContainer.getBoundingClientRect();

		let targetViewportRect = target.getBoundingClientRect();
		var targetRectInPopupContainer = {
			x: (targetViewportRect.left - popupContainerViewportRect.left),
			y: (targetViewportRect.top - popupContainerViewportRect.top)
		};
		targetRectInPopupContainer = {
			x: 		targetRectInPopupContainer.x,
			y: 		targetRectInPopupContainer.y,
			width: 	targetViewportRect.width,
			height: targetViewportRect.height,
			left: 	targetRectInPopupContainer.x,
			top: 	targetRectInPopupContainer.y,
			right: 	targetRectInPopupContainer.x + targetViewportRect.width,
			bottom: targetRectInPopupContainer.y + targetViewportRect.height
		};

		let mouseEnterEventPositionInPopupContainer = {
			x: (event.clientX - popupContainerViewportRect.left),
			y: (event.clientY - popupContainerViewportRect.top)
		};

		//	Prevent popup cycling in Chromium.
		popup.style.visibility = "hidden";

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
			var popupIntrinsicWidth = popup.offsetWidth;
			var popupIntrinsicHeight = popup.offsetHeight;

			var provisionalPopupXPosition;
			var provisionalPopupYPosition;

			if (target.popupSpecialPositioningFunction) {
				[ provisionalPopupXPosition, provisionalPopupYPosition ] = target.popupSpecialPositioningFunction(popup, target, event);
			} else {
				var offToTheSide = false;

				/*  Can the popup fit above the target? If so, put it there.
					Failing that, can it fit below the target? If so, put it there.
					*/
				var popupSpawnYOriginForSpawnAbove = targetRectInPopupContainer.top - popupBreathingRoom.y;
				var popupSpawnYOriginForSpawnBelow = targetRectInPopupContainer.bottom + popupBreathingRoom.y;
				if (target.closest(".popupdiv")) {
					/*  The popup is a nested popup. We prefer to put it off to 
						the left or right.
						*/
					offToTheSide = true;
				} else if ((popupSpawnYOriginForSpawnAbove - popupIntrinsicHeight) >= (popupContainerViewportRect.y * -1)) {
					//  Above.
					provisionalPopupYPosition = popupSpawnYOriginForSpawnAbove - popupIntrinsicHeight;
				} else if ((popupSpawnYOriginForSpawnBelow + popupIntrinsicHeight) <= ((popupContainerViewportRect.y * -1) + window.innerHeight)) {
					//  Below.
					provisionalPopupYPosition = popupSpawnYOriginForSpawnBelow;
				} else {
					/*  The popup does not fit above or below! We will have to
						put it off to the left or right.
						*/
					offToTheSide = true;
				}

				if (offToTheSide) {
					provisionalPopupYPosition = mouseEnterEventPositionInPopupContainer.y - ((event.clientY / window.innerHeight) * popupIntrinsicHeight);
					if (provisionalPopupYPosition - popupContainerViewportRect.y < 0)
						provisionalPopupYPosition = 0.0;

					//  Determine whether to put the popup off to the right, or left.
					if (  targetRectInPopupContainer.right
						+ popupBreathingRoom.x
						+ popupIntrinsicWidth
						  <=
						  popupContainerViewportRect.x * -1
						+ window.innerWidth) {
						//  Off to the right.
						provisionalPopupXPosition = targetRectInPopupContainer.right + popupBreathingRoom.x;
					} else if (  targetRectInPopupContainer.left
							   - popupBreathingRoom.x
							   - popupIntrinsicWidth
								 >=
								 popupContainerViewportRect.x * -1) {
						//  Off to the left.
						provisionalPopupXPosition = targetRectInPopupContainer.left - popupIntrinsicWidth - popupBreathingRoom.x;
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
				the container’s right edge.
				*/
			if (provisionalPopupXPosition + popupIntrinsicWidth > popupContainerViewportRect.width) {
				//  We add 1.0 here to prevent wrapping due to rounding.
				provisionalPopupXPosition -= (provisionalPopupXPosition + popupIntrinsicWidth - popupContainerViewportRect.width + 1.0);
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

			//	Prevent popup cycling in Chromium.
			popup.style.visibility = "visible";

			document.activeElement.blur();
		});
	},
    despawnPopup: (popup) => {
		GWLog("Popups.despawnPopup", "popups.js", 2);

		GW.notificationCenter.fireEvent("Popups.popupWillDespawn", { popup: popup });

	    popup.classList.remove("fading");
        popup.remove();
        popup.popupTarget.classList.remove("popup-open");
        popup.popupTarget.popup = null;
        document.activeElement.blur();
    },

	getPopupAncestorStack: (popup) => {
		var popupAndAncestors = [ ];
		for (popupInStack = popup; popupInStack != null; popupInStack = popupInStack.popupTarget.closest(".popupdiv"))
			popupAndAncestors.splice(0, 0, popupInStack);
		return popupAndAncestors;
	},

    clearPopupTimers: (target) => {
	    GWLog("Popups.clearPopupTimers", "popups.js", 3);

		if (target.popup)
			target.popup.classList.remove("fading");

        clearTimeout(target.popupFadeTimer);
        clearTimeout(target.popupDespawnTimer);
        clearTimeout(target.popupSpawnTimer);
    },
	setPopupSpawnTimer: (target, event, prepareFunction) => {
		GWLog("Popups.setPopupSpawnTimer", "popups.js", 2);

		target.popupSpawnTimer = setTimeout(() => {
			GWLog("Popups.popupSpawnTimer fired", "popups.js", 2);

			//  Despawn existing popup, if any.
			if (target.popup)
				Popups.despawnPopup(target.popup);

			//  Create the new popup.
			target.popup = Popups.newPopup();

			//  Give the popup a reference to the target.
			target.popup.popupTarget = target;

			// Prepare the newly created popup for spawning.
			if (prepareFunction(target.popup, target) == false)
				return;

			// Spawn the prepared popup.
			Popups.spawnPopup(target.popup, target, event);
		}, Popups.popupTriggerDelay);
	},
    setPopupFadeTimer: (target) => {
		GWLog("Popups.setPopupFadeTimer", "popups.js", 2);

        target.popupFadeTimer = setTimeout(() => {
			GWLog("popupFadeTimer fired", "popups.js", 2);

			Popups.setPopupDespawnTimer(target);
        }, Popups.popupFadeoutDelay);
    },
    setPopupDespawnTimer: (target) => {
		GWLog("Popups.setPopupDespawnTimer", "popups.js", 2);

		target.popup.classList.add("fading");
		target.popupDespawnTimer = setTimeout(() => {
			GWLog("popupDespawnTimer fired", "popups.js", 2);

			Popups.despawnPopup(target.popup);
		}, Popups.popupFadeoutDuration);
    },

    //	The “user moved mouse out of popup” mouseleave event.
	popupMouseleave: (event) => {
		GWLog("Popups.popupMouseleave", "popups.js", 2);

		Popups.getPopupAncestorStack(event.target).reverse().forEach(popupInStack => {
			Popups.clearPopupTimers(popupInStack.popupTarget);
			Popups.setPopupFadeTimer(popupInStack.popupTarget);
		});
	},
	//	The “user moved mouse back into popup” mouseenter event.
	popupMouseenter: (event) => {
		GWLog("Popups.popupMouseenter", "popups.js", 2);

		Popups.getPopupAncestorStack(event.target).forEach(popupInStack => {
			Popups.clearPopupTimers(popupInStack.popupTarget);
		});
	},
    popupClicked: (event) => {
		GWLog("Popups.popupClicked", "popups.js", 2);

		let popup = event.target.closest(".popupdiv");

		event.stopPropagation();
		Popups.clearPopupTimers(popup.popupTarget);
		Popups.despawnPopup(popup);
    },
	//	The mouseenter event.
	targetMouseenter: (event) => {
		GWLog("Popups.targetMouseenter", "popups.js", 2);

		//	Stop the countdown to un-pop the popup.
		Popups.clearPopupTimers(event.target);

		//  Start the countdown to pop up the popup.
		Popups.setPopupSpawnTimer(event.target, event, event.target.popupPrepareFunction);
	},
	//	The mouseleave event.
	targetMouseleave: (event) => {
		GWLog("Popups.targetMouseleave", "popups.js", 2);

		Popups.clearPopupTimers(event.target);

		if (event.target.popup)
			Popups.setPopupFadeTimer(event.target);
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
    z-index: ${Popups.popupContainerZIndex};
}
#${Popups.popupContainerID} > * {
    pointer-events: auto;
}
.popupdiv {
    position: absolute;
    overflow: visible;
    overscroll-behavior: none;
}
</style>`;

GW.notificationCenter.fireEvent("Popups.didLoad");

/******************/
/*	Initialization.
	*/
doWhenPageLoaded(() => {
	Popups.setup();
});
