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
    popupContainerID: "popup-container",
    popupContainerParentSelector: "html",
    popupContainerZIndex: "10001",

    popupBreathingRoomX: 12.0,
    popupBreathingRoomY: 8.0,

    popupTriggerDelay: 600,
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
        document.querySelectorAll(`#${Popups.popupContainerID}`).forEach(element => element.remove());

		//  Remove Escape key event listener.
		document.removeEventListener("keyup", Popups.keyUp);
	},

	setup: () => {
		GWLog("Popups.setup", "popups.js", 1);

        //  Run cleanup.
        Popups.cleanup();

        //  Inject popups container.
        let popupContainerParent = document.querySelector(Popups.popupContainerParentSelector);
        if (!popupContainerParent) {
            GWLog("Popup container parent element not found. Exiting.", "popups.js", 1);
            return;
        }
        popupContainerParent.insertAdjacentHTML("beforeend", `<div
        	id="${Popups.popupContainerID}"
        	class="popup-container"
        	style="z-index: ${Popups.popupContainerZIndex};"
        		></div>`);
        requestAnimationFrame(() => {
            Popups.popupContainer = document.querySelector(`#${Popups.popupContainerID}`);
        });

		//  Add Escape key event listener.
		document.addEventListener("keyup", Popups.keyUp);

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

			//  Apply the test function to the target.
			if (!targets.testTarget(target)) {
				target.classList.toggle("no-popup", true);
				return;
			}

			//	Bind mouseenter/mouseleave events.
			target.addEventListener("mouseenter", Popups.targetMouseEnter);
			target.addEventListener("mouseleave", Popups.targetMouseLeave);

			//  Set prepare function.
			target.preparePopup = prepareFunction;

			//  Run any custom processing.
			if (targetPrepareFunction)
				targetPrepareFunction(target);

			//  Mark target as spawning a popup.
			target.classList.toggle("spawns-popup", true);
		});
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

			//  Apply the test function to the target.
			if (!targets.testTarget(target)) {
				target.classList.toggle("no-popup", false);
				return;
			}

			//	Unbind existing mouseenter/mouseleave events, if any.
			target.removeEventListener("mouseenter", Popups.targetMouseEnter);
			target.removeEventListener("mouseleave", Popups.targetMouseLeave);

			//  Clear timers for target.
			Popups.clearPopupTimers(target);

			//  Remove spawned popup for target, if any.
			if (target.popup)
				Popups.despawnPopup(target.popup);

			//  Unset popup prepare function.
			target.preparePopup = null;

			//  Un-mark target as spawning a popup.
			target.classList.toggle("spawns-popup", false);

			//  Run any custom processing.
			if (targetRestoreFunction)
				targetRestoreFunction(target);
		});
	},

	/*******************/
	/*  General helpers.
		*/

	hidePopupContainer: () => {
		GWLog("Popups.hidePopupContainer", "popups.js", 3);

		Popups.popupContainer.style.visibility = "hidden";
	},

	unhidePopupContainer: () => {
		GWLog("Popups.unhidePopupContainer", "popups.js", 3);

		Popups.popupContainer.style.visibility = "";
	},

	updatePageScrollState: () => {
		GWLog("Popups.updatePageScrollState", "popups.js", 2);

		if (Popups.allSpawnedPopups().findIndex(popup => Popups.popupIsMaximized(popup)) == -1)
			togglePageScrolling(true);
		else
			togglePageScrolling(false);
	},

	allSpawnedPopups: () => {
		return Array.from(Popups.popupContainer.children).filter(popup => !popup.classList.contains("fading"));
	},

	/****************************************/
	/*  Visibility of elements within popups.
		*/

	//	Returns true if the given element is currently visible.
	isVisible: (element) => {
		let containingPopup = element.closest(".popup");
		return (containingPopup ? isWithinRect(element, containingPopup.getBoundingClientRect()) : isOnScreen(element));
	},

	scrollElementIntoViewInPopFrame: (element) => {
		let popup = element.closest(".popup");
		popup.scrollView.scrollTop = element.getBoundingClientRect().top - popup.contentView.getBoundingClientRect().top;
	},

	/*******************************/
	/*  Popup spawning & despawning.
		*/

	newPopup: (target) => {
		GWLog("Popups.newPopup", "popups.js", 2);

		let popup = document.createElement("div");
		popup.classList.add("popup", "popframe");
		popup.innerHTML = `<div class="popframe-scroll-view"><div class="popframe-content-view"></div></div>`;
		popup.scrollView = popup.querySelector(".popframe-scroll-view");
		popup.contentView = popup.querySelector(".popframe-content-view");
		popup.contentView.popup = popup.scrollView.popup = popup;
		popup.titleBarContents = [ ];

		//  Give the popup a reference to the target.
		popup.spawningTarget = target;

		return popup;
	},

	setPopFrameContent: (popup, contentHTML) => {
		popup.contentView.innerHTML = contentHTML;
		return (contentHTML > "");
	},

	spawnPopup: (target, spawnPoint) => {
		GWLog("Popups.spawnPopup", "popups.js", 2);

		//  Prevent spawn attempts before setup complete.
		if (Popups.popupContainer == null)
			return;

		//  Despawn existing popup, if any.
		if (target.popup)
			Popups.despawnPopup(target.popup);

		//  Create the new popup.
		target.popFrame = target.popup = Popups.newPopup(target);

		//  Prepare the newly created popup for spawning.
		if (!(target.popFrame = target.popup = target.preparePopup(target.popup)))
			return;

		//  If title bar contents are provided, add a title bar (if needed).
		if (  !target.popup.titleBar
			&& target.popup.titleBarContents.length > 0)
			Popups.addTitleBarToPopup(target.popup);

		if (target.popup.parentElement == Popups.popupContainer) {
			//  If the popup is an existing popup, just bring it to the front.
			Popups.bringPopupToFront(target.popup);
		} else {
			//	Otherwise, inject the popup into the page.
			Popups.injectPopup(target.popup);
		}

		//  Position the popup appropriately with respect to the target.
		Popups.positionPopup(target.popup, spawnPoint);

		//  Mark target as having an active popup associated with it.
		target.classList.add("popup-open");

		//  Fire notification event.
		GW.notificationCenter.fireEvent("Popups.popupDidSpawn", { popup: target.popup });
	},

	injectPopup: (popup) => {
		GWLog("Popups.injectPopup", "popups.js", 2);

		//  Add popup to a popup stack.
		if (popup.popupStack == null) {
			let parentPopup = popup.spawningTarget.closest(".popup");
			popup.popupStack = parentPopup ? parentPopup.popupStack : [ ];
		} else {
			popup.popupStack.remove(popup);
		}
		popup.popupStack.push(popup);

		//  Inject popup into page.
		Popups.popupContainer.appendChild(popup);

		//  Bring popup to front.
		Popups.bringPopupToFront(popup);

		//  Cache border width.
		popup.borderWidth = parseFloat(getComputedStyle(popup).borderLeftWidth);

		//	Add event listeners.
		popup.addEventListener("click", Popups.popupClicked);
		popup.addEventListener("mouseenter", Popups.popupMouseEnter);
		popup.addEventListener("mouseleave", Popups.popupMouseLeave);
		popup.addEventListener("mouseout", Popups.popupMouseOut);
		popup.addEventListener("mousedown", Popups.popupMouseDown);

		//  We define the mousemove listener here in order to capture `popup`.
		popup.addEventListener("mousemove", Popups.popupMouseMove = (event) => {
			GWLog("Popups.popupMouseMove", "popups.js", 3);

			if (   event.target == popup
				&& window.popupBeingDragged == null
				&& Popups.popupIsResizeable(popup)) {
				//  Mouse position is relative to the popup’s coordinate system.
				let edgeOrCorner = Popups.edgeOrCorner(popup, {
					x: event.clientX - popup.viewportRect.left,
					y: event.clientY - popup.viewportRect.top
				});

				//  Set cursor.
				document.documentElement.style.cursor = Popups.cursorForPopupBorder(edgeOrCorner);
			}
		});
	},

	attachPopupToTarget: (popup) => {
		GWLog("Popups.attachPopupToTarget", "popups.js", 2);

		Popups.clearPopupTimers(popup.spawningTarget);

        popup.spawningTarget.classList.add("popup-open");
        popup.spawningTarget.popup = popup;
        popup.spawningTarget.popFrame = popup;
	},

	detachPopupFromTarget: (popup) => {
		GWLog("Popups.detachPopupFromTarget", "popups.js", 2);

		Popups.clearPopupTimers(popup.spawningTarget);

        popup.spawningTarget.classList.remove("popup-open");
        popup.spawningTarget.popup = null;
        popup.spawningTarget.popFrame = null;
	},

    despawnPopup: (popup) => {
		GWLog("Popups.despawnPopup", "popups.js", 2);

		GW.notificationCenter.fireEvent("Popups.popupWillDespawn", { popup: popup });

		//  Detach popup from its spawning target.
        Popups.detachPopupFromTarget(popup);

		//  Remove popup from the page.
        popup.remove();

		//  Remove popup from its popup stack.
        popup.popupStack.remove(popup);
        popup.popupStack = null;

		//  Update z-indexes of all popups.
		Popups.updatePopupsZOrder();

		//  Enable/disable main document scrolling.
		Popups.updatePageScrollState();

        document.activeElement.blur();
    },

	getPopupAncestorStack: (popup) => {
		let indexOfPopup = popup.popupStack.indexOf(popup);
		if (indexOfPopup != -1) {
			return popup.popupStack.slice(0, indexOfPopup + 1);
		} else {
			let parentPopup = popup.spawningTarget.closest(".popup");
			return (parentPopup && parentPopup.popupStack) ? Popups.getPopupAncestorStack(parentPopup) : [ ];
		}
	},

	/********************/
	/*  Popup collapsing.
		*/
	popupIsCollapsed: (popup) => {
		return popup.classList.contains("collapsed");
	},

	collapseOrUncollapsePopup: (popup) => {
		GWLog("Popups.collapseOrUncollapsePopup", "popups.js", 2);

		if (Popups.popupIsCollapsed(popup)) {
			Popups.uncollapsePopup(popup);
		} else {
			Popups.collapsePopup(popup);
		}
	},

	collapsePopup: (popup) => {
		GWLog("Popups.collapsePopup", "popups.js", 3);

		//  Update class.
		popup.classList.toggle("collapsed", true);

		//  Save and unset height, if need be.
		if (popup.style.height) {
			popup.dataset.previousHeight = popup.style.height;
			popup.style.height = "";
		}

		//  Remove popup from its stack and detach from spawning target.
		popup.popupStack.remove(popup);
		Popups.detachPopupFromTarget(popup);

		//  Clear timers.
		Popups.clearPopupTimers(popup.spawningTarget);

		//  Update title bar buttons states (if any).
		if (popup.titleBar)
			popup.titleBar.updateState();
	},

	uncollapsePopup: (popup) => {
		GWLog("Popups.uncollapsePopup", "popups.js", 3);

		//  Update class.
		popup.classList.toggle("collapsed", false);

		//  Restore height, if need be.
		if (popup.dataset.previousHeight) {
			if (!Popups.popupIsEphemeral(popup))
				popup.style.height = popup.dataset.previousHeight;

			//  Delete saved height.
			delete popup.dataset["previousHeight"];
		}

		/*  Re-add popup to its stack and re-attach it to its spawning target
			(unless it’s pinned).
			*/
		if (!(Popups.popupIsPinned(popup))) {
			popup.popupStack.push(popup);
			Popups.attachPopupToTarget(popup);
		}

		//  Clear timers.
		Popups.clearPopupTimers(popup.spawningTarget);

		//  Update title bar buttons states (if any).
		if (popup.titleBar)
			popup.titleBar.updateState();
	},

	/********************************************************/
	/*  Popup pinning/unpinning, zooming/tiling, & restoring.
		*/

	//  Popup tiling control keys.
	popupTilingControlKeys: (localStorage.getItem("popup-tiling-control-keys") || ""),
	setPopupTilingControlKeys: (keystring) => {
		GWLog("Popups.setPopupTilingControlKeys", "popups.js", 1);

		Popups.popupTilingControlKeys = keystring || "aswdqexzfcv";
		localStorage.setItem("popup-tiling-control-keys", Popups.popupTilingControlKeys);
	},

	popupIsEphemeral: (popup) => {
		return !(Popups.popupIsPinned(popup) || Popups.popupIsZoomed(popup) || Popups.popupIsCollapsed(popup));
	},

	popupIsResizeable: (popup) => {
		return (Popups.popupIsPinned(popup) || Popups.popupIsZoomed(popup));
	},

	popupIsZoomed: (popup) => {
		return popup.classList.contains("zoomed");
	},

	popupIsMaximized: (popup) => {
		return (popup.classList.contains("zoomed") && popup.classList.contains("full"));
	},

	popupWasRestored: (popup) => {
		return popup.classList.contains("restored");
	},

	popupIsPinned: (popup) => {
		return popup.classList.contains("pinned");
	},

	popupWasUnpinned: (popup) => {
		return popup.classList.contains("unpinned");
	},

	zoomPopup: (popup, place) => {
		GWLog("Popups.zoomPopup", "popups.js", 2);

		//  If popup isn’t already zoomed, save position.
		if (!(Popups.popupIsZoomed(popup))) {
			popup.dataset.previousXPosition = popup.viewportRect.left;
			popup.dataset.previousYPosition = popup.viewportRect.top;
		}

		//  If the popup is collapsed, expand it.
		if (Popups.popupIsCollapsed(popup))
			Popups.uncollapsePopup(popup);

		//  Update classes.
		popup.swapClasses([ "zoomed", "restored" ], 0);
		popup.classList.remove(...(Popups.titleBarComponents.popupPlaces));
		popup.classList.add(place);

		//  Viewport width must account for vertical scroll bar.
		let viewportWidth = document.documentElement.offsetWidth;
		let viewportHeight = window.innerHeight;
		switch (place) {
			case "top-left":
				popup.zoomToX = 0.0;
				popup.zoomToY = 0.0;
				break;
			case "top":
				popup.zoomToX = 0.0;
				popup.zoomToY = 0.0;
				break;
			case "top-right":
				popup.zoomToX = viewportWidth / 2.0;
				popup.zoomToY = 0.0;
				break;
			case "left":
				popup.zoomToX = 0.0;
				popup.zoomToY = 0.0;
				break;
			case "full":
				popup.zoomToX = 0.0;
				popup.zoomToY = 0.0;
				break;
			case "right":
				popup.zoomToX = viewportWidth / 2.0;
				popup.zoomToY = 0.0;
				break;
			case "bottom-left":
				popup.zoomToX = 0.0;
				popup.zoomToY = viewportHeight / 2.0;
				break;
			case "bottom":
				popup.zoomToX = 0.0;
				popup.zoomToY = viewportHeight / 2.0;
				break;
			case "bottom-right":
				popup.zoomToX = viewportWidth / 2.0;
				popup.zoomToY = viewportHeight / 2.0;
				break;
		}

		//  Update popup position.
		Popups.positionPopup(popup);

		//  Update popup size.
		popup.style.maxWidth = "unset";
		popup.style.maxHeight = "unset";
		switch (place) {
			case "full":
				popup.style.width = "100%";
				popup.style.height = "100vh";
				break;
			case "left":
			case "right":
				popup.style.width = "50%";
				popup.style.height = "100vh";
				break;
			case "top":
			case "bottom":
				popup.style.width = "100%";
				popup.style.height = "50vh";
				break;
			case "top-left":
			case "top-right":
			case "bottom-left":
			case "bottom-right":
				popup.style.width = "50%";
				popup.style.height = "50vh";
				break;
		}
		popup.scrollView.style.maxHeight = "calc(100% - var(--popup-title-bar-height))";

		//  Remove popup from its stack and detach from spawning target.
		popup.popupStack.remove(popup);
		Popups.detachPopupFromTarget(popup);

		//  Clear timers.
		Popups.clearPopupTimers(popup.spawningTarget);

		//  Enable/disable main document scrolling.
		Popups.updatePageScrollState();

		//  Update title bar buttons states (if any).
		if (popup.titleBar)
			popup.titleBar.updateState();
	},

	restorePopup: (popup) => {
		GWLog("Popups.restorePopup", "popups.js", 2);

		//  Update classes.
		popup.swapClasses([ "zoomed", "restored" ], 1);
		popup.classList.remove(...(Popups.titleBarComponents.popupPlaces));
		popup.classList.remove("resized");

		//  Update popup size.
		popup.style.width = "";
		popup.style.height = "";
		popup.style.maxWidth = "";
		popup.style.maxHeight = "";
		popup.scrollView.style.maxHeight = "";

		//  Update popup position.
		Popups.positionPopup(popup);

		/*  Re-add popup to its stack and re-attach it to its spawning target
			(unless it’s pinned).
			*/
		if (!(Popups.popupIsPinned(popup))) {
			popup.popupStack.push(popup);
			Popups.attachPopupToTarget(popup);
		}

		//  Clear timers.
		Popups.clearPopupTimers(popup.spawningTarget);

		//  Enable/disable main document scrolling.
		Popups.updatePageScrollState();

		//  Update title bar buttons states (if any).
		if (popup.titleBar)
			popup.titleBar.updateState();
	},

	pinOrUnpinPopup: (popup) => {
		GWLog("Popups.pinOrUnpinPopup", "popups.js", 2);

		if (Popups.popupIsPinned(popup)) {
			Popups.unpinPopup(popup);
		} else {
			Popups.pinPopup(popup);
		}
	},

	pinPopup: (popup) => {
		GWLog("Popups.pinPopup", "popups.js", 2);

		popup.swapClasses([ "pinned", "unpinned" ], 0);
		Popups.positionPopup(popup);
		popup.popupStack.remove(popup);
		Popups.detachPopupFromTarget(popup);

		popup.titleBar.updateState();
	},

	unpinPopup: (popup) => {
		GWLog("Popups.unpinPopup", "popups.js", 2);

		popup.swapClasses([ "pinned", "unpinned" ], 1);
		Popups.positionPopup(popup);
		popup.popupStack.push(popup);
		Popups.attachPopupToTarget(popup);

		popup.titleBar.updateState();
	},

	/******************/
	/*  Popup resizing.
		*/

	popupWasResized: (popup) => {
		return popup.classList.contains("resized");
	},

	edgeOrCorner: (popup, relativeMousePos) => {
		//  Make corner drag areas big enough to make a decent mouse target.
		let cornerHandleSize = Math.min(20.0, (Math.min(popup.viewportRect.width, popup.viewportRect.height) / 3.0));

			   if (   relativeMousePos.x < cornerHandleSize
				   && relativeMousePos.y < cornerHandleSize) {
			return "corner-top-left";
		} else if (   relativeMousePos.x > popup.viewportRect.width - cornerHandleSize
				   && relativeMousePos.y > popup.viewportRect.height - cornerHandleSize) {
			return "corner-bottom-right";
		} else if (   relativeMousePos.x < cornerHandleSize
				   && relativeMousePos.y > popup.viewportRect.height - cornerHandleSize) {
			return "corner-bottom-left";
		} else if (   relativeMousePos.x > popup.viewportRect.width - cornerHandleSize
				   && relativeMousePos.y < cornerHandleSize) {
			return "corner-top-right";
		} else if (relativeMousePos.x < cornerHandleSize) {
			return "edge-left";
		} else if (relativeMousePos.x > popup.viewportRect.width - cornerHandleSize) {
			return "edge-right";
		} else if (relativeMousePos.y < cornerHandleSize) {
			return "edge-top";
		} else if (relativeMousePos.y > popup.viewportRect.height - cornerHandleSize) {
			return "edge-bottom";
		} else {
			return "";
		}
	},

	cursorForPopupBorder: (edgeOrCorner) => {
		switch (edgeOrCorner) {
			case "edge-top":
			case "edge-bottom":
				return "row-resize";
			case "edge-left":
			case "edge-right":
				return "col-resize";
			case "corner-top-left":
			case "corner-bottom-right":
				return "nwse-resize";
			case "corner-top-right":
			case "corner-bottom-left":
				return "nesw-resize";
		}
	},

	/*******************/
	/*  Popup title bar.
		*/

	//  Add title bar to a popup which has a populated .titleBarContents.
	addTitleBarToPopup: (popup) => {
		GWLog("Popups.addTitleBarToPopup", "popups.js", 2);

		//  Set class ‘has-title-bar’ on the popup.
		popup.classList.add("has-title-bar");

		//  Create and inject the title bar element.
		popup.titleBar = document.createElement("div");
		popup.titleBar.classList.add("popframe-title-bar");
		popup.titleBar.title = "Drag popup by title bar to reposition; double-click title bar to collapse";
		popup.insertBefore(popup.titleBar, popup.firstElementChild);

		//  Add the provided title bar contents (buttons, title, etc.).
		popup.titleBarContents.forEach(elementOrHTML => {
			if (typeof elementOrHTML == "string") {
				popup.titleBar.insertAdjacentHTML("beforeend", elementOrHTML);
			} else {
				popup.titleBar.appendChild(elementOrHTML);
			}
			let newlyAddedElement = popup.titleBar.lastElementChild;

			if (newlyAddedElement.buttonAction)
				newlyAddedElement.addActivateEvent(newlyAddedElement.buttonAction);

			//  Add popup-positioning submenu to zoom button.
			if (   newlyAddedElement.classList.contains("zoom-button")
				&& newlyAddedElement.submenuEnabled)
				Popups.titleBarComponents.addSubmenuToButton(newlyAddedElement, "zoom-button-submenu", Popups.titleBarComponents.popupZoomButtons());
		});

		//  Add state-updating function.
		popup.titleBar.updateState = () => {
			popup.titleBar.querySelectorAll("button").forEach(button => {
				if (button.updateState)
					button.updateState();
			});
		};

		//  Add event listeners for dragging the popup by the title bar.
		popup.titleBar.addEventListener("mousedown", Popups.popupTitleBarMouseDown);
		popup.titleBar.addEventListener("mouseup", Popups.popupTitleBarMouseUp);

		//  Add double-click event listener for collapsing/uncollapsing the popup.
		popup.titleBar.addEventListener("dblclick", Popups.popupTitleBarDoubleClicked);
	},

	//  Elements and methods related to popup title bars.
	titleBarComponents: {
		//  The standard positions for a popup to zoom to.
		popupPlaces: [ "top-left", "top", "top-right", "left", "full", "right", "bottom-left", "bottom", "bottom-right" ],

		//  Icons for various popup title bar buttons.
		buttonIcons: {
			"close": `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="M325.8 193.8L263.6 256l62.2 62.2c4.7 4.7 4.7 12.3 0 17l-22.6 22.6c-4.7 4.7-12.3 4.7-17 0L224 295.6l-62.2 62.2c-4.7 4.7-12.3 4.7-17 0l-22.6-22.6c-4.7-4.7-4.7-12.3 0-17l62.2-62.2-62.2-62.2c-4.7-4.7-4.7-12.3 0-17l22.6-22.6c4.7-4.7 12.3-4.7 17 0l62.2 62.2 62.2-62.2c4.7-4.7 12.3-4.7 17 0l22.6 22.6c4.7 4.7 4.7 12.3 0 17zM448 80v352c0 26.5-21.5 48-48 48H48c-26.5 0-48-21.5-48-48V80c0-26.5 21.5-48 48-48h352c26.5 0 48 21.5 48 48zm-48 346V86c0-3.3-2.7-6-6-6H54c-3.3 0-6 2.7-6 6v340c0 3.3 2.7 6 6 6h340c3.3 0 6-2.7 6-6z"/></svg>`,
			"zoom": `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path fill="currentColor" d="M0 180V56c0-13.3 10.7-24 24-24h124c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12H64v84c0 6.6-5.4 12-12 12H12c-6.6 0-12-5.4-12-12zM288 44v40c0 6.6 5.4 12 12 12h84v84c0 6.6 5.4 12 12 12h40c6.6 0 12-5.4 12-12V56c0-13.3-10.7-24-24-24H300c-6.6 0-12 5.4-12 12zm148 276h-40c-6.6 0-12 5.4-12 12v84h-84c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h124c13.3 0 24-10.7 24-24V332c0-6.6-5.4-12-12-12zM160 468v-40c0-6.6-5.4-12-12-12H64v-84c0-6.6-5.4-12-12-12H12c-6.6 0-12 5.4-12 12v124c0 13.3 10.7 24 24 24h124c6.6 0 12-5.4 12-12z"></path></svg>`,
			"restore": `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path fill="currentColor" d="M436 192H312c-13.3 0-24-10.7-24-24V44c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v84h84c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12zm-276-24V44c0-6.6-5.4-12-12-12h-40c-6.6 0-12 5.4-12 12v84H12c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h124c13.3 0 24-10.7 24-24zm0 300V344c0-13.3-10.7-24-24-24H12c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h84v84c0 6.6 5.4 12 12 12h40c6.6 0 12-5.4 12-12zm192 0v-84h84c6.6 0 12-5.4 12-12v-40c0-6.6-5.4-12-12-12H312c-13.3 0-24 10.7-24 24v124c0 6.6 5.4 12 12 12h40c6.6 0 12-5.4 12-12z"></path></svg>`,
			"pin": `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 384 512"><path d="M306.5 186.6l-5.7-42.6H328c13.2 0 24-10.8 24-24V24c0-13.2-10.8-24-24-24H56C42.8 0 32 10.8 32 24v96c0 13.2 10.8 24 24 24h27.2l-5.7 42.6C29.6 219.4 0 270.7 0 328c0 13.2 10.8 24 24 24h144v104c0 .9.1 1.7.4 2.5l16 48c2.4 7.3 12.8 7.3 15.2 0l16-48c.3-.8.4-1.7.4-2.5V352h144c13.2 0 24-10.8 24-24 0-57.3-29.6-108.6-77.5-141.4zM50.5 304c8.3-38.5 35.6-70 71.5-87.8L138 96H80V48h224v48h-58l16 120.2c35.8 17.8 63.2 49.4 71.5 87.8z"/></svg>`,
			"unpin": `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 384 512"><path d="M298.028 214.267L285.793 96H328c13.255 0 24-10.745 24-24V24c0-13.255-10.745-24-24-24H56C42.745 0 32 10.745 32 24v48c0 13.255 10.745 24 24 24h42.207L85.972 214.267C37.465 236.82 0 277.261 0 328c0 13.255 10.745 24 24 24h136v104.007c0 1.242.289 2.467.845 3.578l24 48c2.941 5.882 11.364 5.893 14.311 0l24-48a8.008 8.008 0 0 0 .845-3.578V352h136c13.255 0 24-10.745 24-24-.001-51.183-37.983-91.42-85.973-113.733z"/></svg>`,
			"options": `<svg enable-background="new 0 0 32 32" id="Glyph" version="1.1" viewBox="0 0 32 32" xml:space="preserve" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><path d="M20.722,24.964c0.096,0.096,0.057,0.264-0.073,0.306c-7.7,2.466-16.032-1.503-18.594-8.942  c-0.072-0.21-0.072-0.444,0-0.655c0.743-2.157,1.99-4.047,3.588-5.573c0.061-0.058,0.158-0.056,0.217,0.003l4.302,4.302  c0.03,0.03,0.041,0.072,0.031,0.113c-1.116,4.345,2.948,8.395,7.276,7.294c0.049-0.013,0.095-0.004,0.131,0.032  C17.958,22.201,20.045,24.287,20.722,24.964z" id="XMLID_323_"/><path d="M24.68,23.266c2.406-1.692,4.281-4.079,5.266-6.941c0.072-0.21,0.072-0.44,0-0.65  C27.954,9.888,22.35,6,16,6c-2.479,0-4.841,0.597-6.921,1.665L3.707,2.293c-0.391-0.391-1.023-0.391-1.414,0s-0.391,1.023,0,1.414  l26,26c0.391,0.391,1.023,0.391,1.414,0c0.391-0.391,0.391-1.023,0-1.414L24.68,23.266z M16,10c3.309,0,6,2.691,6,6  c0,1.294-0.416,2.49-1.115,3.471l-8.356-8.356C13.51,10.416,14.706,10,16,10z" id="XMLID_325_"/></svg>`,
			"zoom-top-left": `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="M 0,180 V 56 C 0,42.7 10.7,32 24,32 h 124 c 6.6,0 12,5.4 12,12 v 40 c 0,6.6 -5.4,12 -12,12 H 64 v 84 c 0,6.6 -5.4,12 -12,12 H 12 C 5.4,192 0,186.6 0,180 Z" /></svg>`,
			"zoom-top": `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="M 0,180 V 56 C 0,42.7 10.7,32 24,32 h 124 c 6.6,0 12,5.4 12,12 v 40 c 0,6.6 -5.4,12 -12,12 H 64 v 84 c 0,6.6 -5.4,12 -12,12 H 12 C 5.4,192 0,186.6 0,180 Z M 288,44 v 40 c 0,6.6 5.4,12 12,12 h 84 v 84 c 0,6.6 5.4,12 12,12 h 40 c 6.6,0 12,-5.4 12,-12 V 56 C 448,42.7 437.3,32 424,32 H 300 c -6.6,0 -12,5.4 -12,12 z" /></svg>`,
			"zoom-top-right": `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="m 288,44 v 40 c 0,6.6 5.4,12 12,12 h 84 v 84 c 0,6.6 5.4,12 12,12 h 40 c 6.6,0 12,-5.4 12,-12 V 56 C 448,42.7 437.3,32 424,32 H 300 c -6.6,0 -12,5.4 -12,12 z" /></svg>`,
			"zoom-left": `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="M 0,180 V 56 C 0,42.7 10.7,32 24,32 h 124 c 6.6,0 12,5.4 12,12 v 40 c 0,6.6 -5.4,12 -12,12 H 64 v 84 c 0,6.6 -5.4,12 -12,12 H 12 C 5.4,192 0,186.6 0,180 Z m 160,288 v -40 c 0,-6.6 -5.4,-12 -12,-12 H 64 v -84 c 0,-6.6 -5.4,-12 -12,-12 H 12 c -6.6,0 -12,5.4 -12,12 v 124 c 0,13.3 10.7,24 24,24 h 124 c 6.6,0 12,-5.4 12,-12 z" /></svg>`,
			"zoom-full": `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path fill="currentColor" d="M0 180V56c0-13.3 10.7-24 24-24h124c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12H64v84c0 6.6-5.4 12-12 12H12c-6.6 0-12-5.4-12-12zM288 44v40c0 6.6 5.4 12 12 12h84v84c0 6.6 5.4 12 12 12h40c6.6 0 12-5.4 12-12V56c0-13.3-10.7-24-24-24H300c-6.6 0-12 5.4-12 12zm148 276h-40c-6.6 0-12 5.4-12 12v84h-84c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h124c13.3 0 24-10.7 24-24V332c0-6.6-5.4-12-12-12zM160 468v-40c0-6.6-5.4-12-12-12H64v-84c0-6.6-5.4-12-12-12H12c-6.6 0-12 5.4-12 12v124c0 13.3 10.7 24 24 24h124c6.6 0 12-5.4 12-12z"></path></svg>`,
			"zoom-right": `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="m 288,44 v 40 c 0,6.6 5.4,12 12,12 h 84 v 84 c 0,6.6 5.4,12 12,12 h 40 c 6.6,0 12,-5.4 12,-12 V 56 C 448,42.7 437.3,32 424,32 H 300 c -6.6,0 -12,5.4 -12,12 z m 148,276 h -40 c -6.6,0 -12,5.4 -12,12 v 84 h -84 c -6.6,0 -12,5.4 -12,12 v 40 c 0,6.6 5.4,12 12,12 h 124 c 13.3,0 24,-10.7 24,-24 V 332 c 0,-6.6 -5.4,-12 -12,-12 z" /></svg>`,
			"zoom-bottom-left": `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="m 160,468 v -40 c 0,-6.6 -5.4,-12 -12,-12 H 64 v -84 c 0,-6.6 -5.4,-12 -12,-12 H 12 c -6.6,0 -12,5.4 -12,12 v 124 c 0,13.3 10.7,24 24,24 h 124 c 6.6,0 12,-5.4 12,-12 z" /></svg>`,
			"zoom-bottom": `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="m 436,320 h -40 c -6.6,0 -12,5.4 -12,12 v 84 h -84 c -6.6,0 -12,5.4 -12,12 v 40 c 0,6.6 5.4,12 12,12 h 124 c 13.3,0 24,-10.7 24,-24 V 332 c 0,-6.6 -5.4,-12 -12,-12 z M 160,468 v -40 c 0,-6.6 -5.4,-12 -12,-12 H 64 v -84 c 0,-6.6 -5.4,-12 -12,-12 H 12 c -6.6,0 -12,5.4 -12,12 v 124 c 0,13.3 10.7,24 24,24 h 124 c 6.6,0 12,-5.4 12,-12 z" /></svg>`,
			"zoom-bottom-right": `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="m 436,320 h -40 c -6.6,0 -12,5.4 -12,12 v 84 h -84 c -6.6,0 -12,5.4 -12,12 v 40 c 0,6.6 5.4,12 12,12 h 124 c 13.3,0 24,-10.7 24,-24 V 332 c 0,-6.6 -5.4,-12 -12,-12 z" /></svg>`
		},

		//  Tooltip text for various popup title bar icons.
		buttonTitles: {
			"close": "Close this popup",
			"zoom": "Maximize this popup",
			"restore": "Restore this popup to normal size and position",
			"pin": "Pin this popup to the screen",
			"unpin": "Un-pin this popup from the screen",
			"options": "Show options",
			"zoom-top-left": "Place this popup in the top-left quarter of the screen",
			"zoom-top": "Place this popup on the top half of the screen",
			"zoom-top-right": "Place this popup in the top-right quarter of the screen",
			"zoom-left": "Place this popup on the left half of the screen",
			"zoom-right": "Place this popup on the right half of the screen",
			"zoom-full": "Expand this popup to fill the screen",
			"zoom-bottom-left": "Place this popup in the bottom-left quarter of the screen",
			"zoom-bottom": "Place this popup on the bottom half of the screen",
			"zoom-bottom-right": "Place this popup in the bottom-right quarter of the screen"
		},

		//  A generic button, with no icon or tooltip text.
		genericButton: () => {
			let button = document.createElement("BUTTON");
			button.classList.add("popframe-title-bar-button");

			button.buttonAction = (event) => { event.stopPropagation(); };

			return button;
		},

		//  Close button.
		closeButton: () => {
			let button = Popups.titleBarComponents.genericButton();
			button.classList.add("close-button");

			button.innerHTML = Popups.titleBarComponents.buttonIcons["close"];
			button.title = Popups.titleBarComponents.buttonTitles["close"];

			button.buttonAction = (event) => {
				event.stopPropagation();

				Popups.despawnPopup(event.target.closest(".popup"));
			};

			return button;
		},

		//  Zoom button (with submenu).
		zoomButton: () => {
			let button = Popups.titleBarComponents.genericButton();
			button.classList.add("zoom-button", "zoom");

			button.defaultHTML = Popups.titleBarComponents.buttonIcons["zoom"];
			button.alternateHTML = Popups.titleBarComponents.buttonIcons["restore"];
			button.innerHTML = button.defaultHTML;

			button.defaultTitle = Popups.titleBarComponents.buttonTitles["zoom"];
			button.alternateTitle = Popups.titleBarComponents.buttonTitles["restore"];
			button.title = button.defaultTitle;

			button.buttonAction = (event) => {
				event.stopPropagation();

				let popup = button.closest(".popup");

				if (button.classList.contains("zoom")) {
					Popups.zoomPopup(popup, "full");
				} else {
					Popups.restorePopup(popup);
				}
			};

			button.updateState = () => {
				let popup = button.closest(".popup");

				let alternateStateEnabled = (Popups.popupIsZoomed(popup) || Popups.popupWasResized(popup));

				button.innerHTML = alternateStateEnabled ? button.alternateHTML : button.defaultHTML;
				button.title = alternateStateEnabled ? button.alternateTitle : button.defaultTitle;

				button.swapClasses([ "zoom", "restore" ], (alternateStateEnabled ? 1 : 0));
			};

			button.enableSubmenu = () => {
				button.submenuEnabled = true;
				return button;
			};

			return button;
		},

		//  Zoom buttons (to be put into zoom button submenu).
		popupZoomButtons: () => {
			return Popups.titleBarComponents.popupPlaces.map(place => {
				let button = Popups.titleBarComponents.genericButton();
				button.classList.add("submenu-button", "zoom-button", place);

				button.innerHTML = Popups.titleBarComponents.buttonIcons[`zoom-${place}`];
				button.title = Popups.titleBarComponents.buttonTitles[`zoom-${place}`];

				button.buttonAction = (event) => {
					event.stopPropagation();

					let popup = button.closest(".popup");

					Popups.zoomPopup(popup, place);
				};

				return button;
			});
		},

		//  Pin button.
		pinButton: () => {
			let button = Popups.titleBarComponents.genericButton();
			button.classList.add("pin-button", "pin");

			button.defaultHTML = Popups.titleBarComponents.buttonIcons["pin"];
			button.alternateHTML = Popups.titleBarComponents.buttonIcons["unpin"];
			button.innerHTML = button.defaultHTML;

			button.defaultTitle = Popups.titleBarComponents.buttonTitles["pin"];
			button.alternateTitle = Popups.titleBarComponents.buttonTitles["unpin"];
			button.title = button.defaultTitle;

			button.buttonAction = (event) => {
				event.stopPropagation();

				Popups.pinOrUnpinPopup(button.closest(".popup"));
			};

			button.updateState = () => {
				let popup = button.closest(".popup");

				button.innerHTML = Popups.popupIsEphemeral(popup) ? button.defaultHTML : button.alternateHTML;
				button.title = Popups.popupIsPinned(popup) ? button.alternateTitle : button.defaultTitle;

				button.swapClasses([ "pin", "unpin" ], (Popups.popupIsPinned(popup) ? 1 : 0));

				button.disabled = !(Popups.popupIsEphemeral(popup))
							   && !(Popups.popupIsPinned(popup));
			};

			return button;
		},

		//  Options button (does nothing by default).
		optionsButton: () => {
			let button = Popups.titleBarComponents.genericButton();
			button.classList.add("options-button");

			button.innerHTML = Popups.titleBarComponents.buttonIcons["options"];
			button.title = Popups.titleBarComponents.buttonTitles["options"];

			return button;
		},

		//  Add a submenu of the given class and with given buttons to a button.
		addSubmenuToButton: (button, submenuClass, submenuButtons) => {
			let popup = button.closest(".popup");

			button.classList.add("has-submenu");

			button.submenu = document.createElement("div");
			button.submenu.classList.add("submenu", submenuClass);

			popup.titleBar.insertBefore(button.submenu, button.nextElementSibling);

			submenuButtons.forEach(submenuButton => {
				button.submenu.appendChild(submenuButton);
				if (submenuButton.buttonAction)
					submenuButton.addActivateEvent(submenuButton.buttonAction);
			});
		},
	},

	/*********************/
	/*	Popups z-ordering.
		*/

	updatePopupsZOrder: () => {
		GWLog("Popups.updatePopupsZOrder", "popups.js", 3);

		let allPopups = Popups.allSpawnedPopups();
		allPopups.sort((a, b) => parseInt(a.style.zIndex) - parseInt(b.style.zIndex));
		for (let i = 0; i < allPopups.length; i++)
			allPopups[i].style.zIndex = i + 1;

		//  Focus the front-most popup.
		Popups.focusPopup(Popups.frontmostPopup());
	},

	popupIsFrontmost: (popup) => {
		return (parseInt(popup.style.zIndex) == Popups.allSpawnedPopups().length);
	},

	frontmostPopup: () => {
		let allPopups = Popups.allSpawnedPopups();
		return allPopups.find(popup => parseInt(popup.style.zIndex) == allPopups.length);
	},

	bringPopupToFront: (popup) => {
		GWLog("Popups.bringPopupToFront", "popups.js", 3);

		//  If it’s already at the front, do nothing.
		if (Popups.popupIsFrontmost(popup))
			return;

		//  Set z-index.
		popup.style.zIndex = (Popups.allSpawnedPopups().length + 1);

		//  Update z-indexes of all popups.
		Popups.updatePopupsZOrder();
	},

	/******************/
	/*  Popup focusing.
		*/

	popupIsFocused: (popup) => {
		return popup.classList.contains("focused");
	},

	focusedPopup: () => {
		return Popups.allSpawnedPopups().find(popup => Popups.popupIsFocused(popup));
	},

	focusPopup: (popup) => {
		GWLog("Popups.focusPopup", "popups.js", 3);

		//  Un-focus any focused popups.
		Popups.allSpawnedPopups().forEach(spawnedPopup => {
			spawnedPopup.classList.toggle("focused", false);
		});

		//  Focus the given popup.
		if (popup)
			popup.classList.toggle("focused", true);
	},

	/*********************/
	/*  Popup positioning.
		*/

	preferSidePositioning: (target) => {
		return target.preferSidePositioning ? target.preferSidePositioning() : false;
	},

	positionPopup: (popup, spawnPoint) => {
		GWLog("Popups.positionPopup", "popups.js", 2);

		let target = popup.spawningTarget;
		if (spawnPoint) target.lastMouseEnterLocation = spawnPoint;
		else spawnPoint = target.lastMouseEnterLocation;

		let targetViewportRect = target.getBoundingClientRect();

		//	Prevent popup cycling in Chromium.
		popup.style.visibility = "hidden";

		//  Wait for the “naive” layout to be completed, and then...
		requestAnimationFrame(() => {
			/*  This is the width and height of the popup, as already determined
				by the layout system, and taking into account the popup’s content,
				and the max-width, min-width, etc., CSS properties.
				*/
			let popupIntrinsicWidth = popup.offsetWidth;
			let popupIntrinsicHeight = popup.offsetHeight;

			let provisionalPopupXPosition = 0.0;
			let provisionalPopupYPosition = 0.0;

			let offToTheSide = false;
			let popupSpawnYOriginForSpawnAbove = targetViewportRect.top - Popups.popupBreathingRoomY;
			let popupSpawnYOriginForSpawnBelow = targetViewportRect.bottom + Popups.popupBreathingRoomY;
			if (target.closest(".popup") || Popups.preferSidePositioning(target)) {
				/*  The popup is a nested popup, or the target specifies that it
					prefers to have popups spawned to the side; we try to put
					the popup off to the left or right.
					*/
				offToTheSide = true;
			}

			provisionalPopupYPosition = spawnPoint.y - ((spawnPoint.y / window.innerHeight) * popupIntrinsicHeight);
			if (provisionalPopupYPosition < 0.0)
				provisionalPopupYPosition = 0.0;

			//  Determine whether to put the popup off to the right, or left.
			if (  targetViewportRect.right
				+ Popups.popupBreathingRoomX
				+ popupIntrinsicWidth
				  <= document.documentElement.offsetWidth) {
				//  Off to the right.
				provisionalPopupXPosition = targetViewportRect.right + Popups.popupBreathingRoomX;
			} else if (  targetViewportRect.left
					   - Popups.popupBreathingRoomX
					   - popupIntrinsicWidth
						 >= 0) {
				//  Off to the left.
				provisionalPopupXPosition = targetViewportRect.left - popupIntrinsicWidth - Popups.popupBreathingRoomX;
			} else {
				//  Not off to either side, in fact.
				offToTheSide = false;
			}

			/*  Can the popup fit above the target? If so, put it there.
				Failing that, can it fit below the target? If so, put it there.
				*/
			if (!offToTheSide) {
				if (  popupSpawnYOriginForSpawnAbove
					- popupIntrinsicHeight
					  >= 0) {
					//  Above.
					provisionalPopupYPosition = popupSpawnYOriginForSpawnAbove - popupIntrinsicHeight;
				} else if (  popupSpawnYOriginForSpawnBelow
						   + popupIntrinsicHeight
						     <= window.innerHeight) {
					//  Below.
					provisionalPopupYPosition = popupSpawnYOriginForSpawnBelow;
				} else {
					/*  The popup does not fit above or below! We will have to
						put it off to to the right after all...
						*/
					offToTheSide = true;
				}
			}

			if (!offToTheSide) {
				/*  Place popup off to the right (and either above or below),
					as per the previous block of code.
					*/
				provisionalPopupXPosition = spawnPoint.x + Popups.popupBreathingRoomX;
			}

			/*  Does the popup extend past the right edge of the container?
				If so, move it left, until its right edge is flush with
				the container’s right edge.
				*/
			if (  provisionalPopupXPosition
				+ popupIntrinsicWidth
				  > document.documentElement.offsetWidth) {
				//  We add 1.0 here to prevent wrapping due to rounding.
				provisionalPopupXPosition -= (provisionalPopupXPosition + popupIntrinsicWidth - document.documentElement.offsetWidth + 1.0);
			}

			/*  Now (after having nudged the popup left, if need be),
				does the popup extend past the *left* edge of the container?
				Make its left edge flush with the container's left edge.
				*/
			if (provisionalPopupXPosition < 0) {
				provisionalPopupXPosition = 0;
			}

			//  Special cases for maximizing/restoring and pinning/unpinning.
			let getPositionToRestore = (popup) => {
				xPos = parseFloat(popup.dataset.previousXPosition);
				yPos = parseFloat(popup.dataset.previousYPosition);

				//  Clear saved position.
				delete popup.dataset.previousXPosition;
				delete popup.dataset.previousYPosition;

				popup.classList.toggle("restored", false);

				return [ xPos, yPos ];
			};
			if (Popups.popupIsZoomed(popup)) {
				provisionalPopupXPosition = popup.zoomToX;
				provisionalPopupYPosition = popup.zoomToY;
			} else if (Popups.popupIsPinned(popup)) {
				if (Popups.popupWasRestored(popup)) {
					[ provisionalPopupXPosition, provisionalPopupYPosition ] = getPositionToRestore(popup);
				} else {
					provisionalPopupXPosition = popup.viewportRect.left;
					provisionalPopupYPosition = popup.viewportRect.top;
				}
			} else {
				if (Popups.popupWasUnpinned(popup)) {
					provisionalPopupXPosition = popup.viewportRect.left;
					provisionalPopupYPosition = popup.viewportRect.top;

					popup.classList.toggle("unpinned", false);
				} else if (Popups.popupWasRestored(popup)) {
					[ provisionalPopupXPosition, provisionalPopupYPosition ] = getPositionToRestore(popup);
				}
			}

			//  Set only position, not size.
			Popups.setPopupViewportRect(popup, new DOMRect(provisionalPopupXPosition, provisionalPopupYPosition, 0, 0));

			//  Cache the viewport rect.
			popup.viewportRect = popup.getBoundingClientRect();

			//	Prevent popup cycling in Chromium.
			popup.style.visibility = "";

			document.activeElement.blur();
		});
	},

	setPopupViewportRect: (popup, rect) => {
		GWLog("Popups.setPopupViewportRect", "popups.js", 3);

		if (Popups.popupIsEphemeral(popup)) {
            let popupContainerViewportRect = Popups.popupContainer.getBoundingClientRect();
			rect.x -= popupContainerViewportRect.left;
			rect.y -= popupContainerViewportRect.top;
		}

		popup.style.position = Popups.popupIsEphemeral(popup) ? "" : "fixed";

		popup.style.left = `${rect.x}px`;
		popup.style.top = `${rect.y}px`;

		if (rect.width > 0 && rect.height > 0) {
			popup.style.maxWidth = "unset";
			popup.style.maxHeight = "unset";

			popup.style.width = `${rect.width}px`;
			popup.style.height = `${rect.height}px`;

			popup.scrollView.style.maxHeight = "calc(100% - var(--popup-title-bar-height))";
		}
	},

	/****************/
	/*	Popup timers.
		*/

    clearPopupTimers: (target) => {
	    GWLog("Popups.clearPopupTimers", "popups.js", 3);

		if (target.popup)
			target.popup.classList.remove("fading");

        clearTimeout(target.popupFadeTimer);
        clearTimeout(target.popupDespawnTimer);
        clearTimeout(target.popupSpawnTimer);
    },

	setPopupSpawnTimer: (target, event) => {
		GWLog("Popups.setPopupSpawnTimer", "popups.js", 2);

		target.popupSpawnTimer = setTimeout(() => {
			GWLog("Popups.popupSpawnTimer fired", "popups.js", 2);

			// Spawn the popup.
			Popups.spawnPopup(target, { x: event.clientX, y: event.clientY });
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

	/*******************/
	/*  Event listeners.
		*/

    //	The “user moved mouse out of popup” mouseleave event.
	popupMouseLeave: (event) => {
		GWLog("Popups.popupMouseLeave", "popups.js", 2);

		if (window.popupBeingDragged)
			return;

		Popups.getPopupAncestorStack(event.target).reverse().forEach(popupInStack => {
			Popups.clearPopupTimers(popupInStack.spawningTarget);
			Popups.setPopupFadeTimer(popupInStack.spawningTarget);
		});
	},

	//	The “user moved mouse back into popup” mouseenter event.
	popupMouseEnter: (event) => {
		GWLog("Popups.popupMouseEnter", "popups.js", 2);

		Popups.getPopupAncestorStack(event.target).forEach(popupInStack => {
			Popups.clearPopupTimers(popupInStack.spawningTarget);
		});
	},

	//  The “user clicked in body of popup” event.
    popupClicked: (event) => {
		GWLog("Popups.popupClicked", "popups.js", 2);

		let popup = event.target.closest(".popup");

		if (!(Popups.popupIsFrontmost(popup))) {
			Popups.bringPopupToFront(popup);
			return;
		}

		if (!(Popups.popupIsEphemeral(popup)))
			return;

		event.stopPropagation();
		Popups.clearPopupTimers(popup.spawningTarget);
// 		Popups.despawnPopup(popup);
    },

	//  The popup mouse down event (for resizing by dragging an edge/corner).
	popupMouseDown: (event) => {
		GWLog("Popups.popupMouseDown", "popups.js", 2);

		//  Prevent other events from triggering.
		event.stopPropagation();

		//  Get the containing popup.
		let popup = event.target.closest(".popup");

		/*  Make sure we’re clicking on the popup (ie. its edge) and not
			on any of the popup’s contained elements; that this is a
			left-click; and that the popup is pinned or zoomed.
			*/
		if (   event.target != popup
			|| event.button != 0
			|| !(Popups.popupIsResizeable(popup)))
			return;

		//  Bring the popup to the front.
		Popups.bringPopupToFront(popup);

		//  Prevent clicks from doing anything other than what we want.
		event.preventDefault();

		//  Mark popup as currently being resized.
		popup.classList.toggle("resizing", true);

		//  Save position, if need be.
		if (!("previousXPosition" in popup.dataset) && !("previousYPosition" in popup.dataset)) {
			popup.dataset.previousXPosition = popup.viewportRect.left;
			popup.dataset.previousYPosition = popup.viewportRect.top;
		}

		//  Determine direction of resizing.
		let edgeOrCorner = Popups.edgeOrCorner(popup, {
			x: event.clientX - popup.viewportRect.left,
			y: event.clientY - popup.viewportRect.top
		});

		//  Point where the drag began.
		let dragStartMouseCoordX = event.clientX;
		let dragStartMouseCoordY = event.clientY;

		//  Popup initial rect.
		let newPopupViewportRect = DOMRect.fromRect(popup.viewportRect);

		/*  Add the mouse up event listener (to window, not the popup, because
			the drag might end anywhere, due to animation lag).
			*/
		window.addEventListener("mouseup", Popups.popupResizeMouseUp);

		//  Viewport width must account for vertical scroll bar.
		let viewportWidth = document.documentElement.offsetWidth;
		let viewportHeight = window.innerHeight;

		//  Popup minimum width/height.
		let popupMinWidth = parseFloat(getComputedStyle(popup).minWidth);
		let popupMinHeight = parseFloat(getComputedStyle(popup).minHeight);

		//  The mousemove event that triggers the continuous resizing.
		window.onmousemove = (event) => {
			window.popupBeingResized = popup;

			popup.classList.toggle("resized", true);

			let deltaX = event.clientX - dragStartMouseCoordX;
			let deltaY = event.clientY - dragStartMouseCoordY;

			let resizeTop = () => {
				newPopupViewportRect.y = valMinMax(popup.viewportRect.y + deltaY, 0, popup.viewportRect.bottom - popupMinHeight);
				newPopupViewportRect.height = popup.viewportRect.bottom - newPopupViewportRect.y;
			};
			let resizeBottom = () => {
				newPopupViewportRect.height = valMinMax(popup.viewportRect.height + deltaY, popupMinHeight, viewportHeight - popup.viewportRect.y);
			};
			let resizeLeft = () => {
				newPopupViewportRect.x = valMinMax(popup.viewportRect.x + deltaX, 0, popup.viewportRect.right - popupMinWidth);
				newPopupViewportRect.width = popup.viewportRect.right - newPopupViewportRect.x;
			};
			let resizeRight = () => {
				newPopupViewportRect.width = valMinMax(popup.viewportRect.width + deltaX, popupMinWidth, viewportWidth - popup.viewportRect.x);
			};

			switch (edgeOrCorner) {
				case "edge-top":
					resizeTop();
					break;
				case "edge-bottom":
					resizeBottom();
					break;
				case "edge-left":
					resizeLeft();
					break;
				case "edge-right":
					resizeRight();
					break;
				case "corner-top-left":
					resizeTop();
					resizeLeft();
					break;
				case "corner-bottom-right":
					resizeBottom();
					resizeRight();
					break;
				case "corner-top-right":
					resizeTop();
					resizeRight();
					break;
				case "corner-bottom-left":
					resizeBottom();
					resizeLeft();
					break;
			}

			Popups.setPopupViewportRect(popup, newPopupViewportRect);
		};
	},

	//  The resize-end mouseup event.
	popupResizeMouseUp: (event) => {
		GWLog("Popups.popupResizeMouseUp", "popups.js", 2);

		event.stopPropagation();

		window.onmousemove = null;

		//  Reset cursor to normal.
		document.documentElement.style.cursor = "";

		let popup = window.popupBeingResized;
		if (popup) {
			popup.classList.toggle("resizing", false);

			if (Popups.popupWasResized(popup))
				popup.titleBar.updateState();

			//  Cache the viewport rect.
			popup.viewportRect = popup.getBoundingClientRect();
		}
		window.popupBeingResized = null;

		window.removeEventListener("mouseup", Popups.popupResizeMouseUp);
	},

	//  The popup mouseout event.
	popupMouseOut: (event) => {
		GWLog("Popups.popupMouseOut", "popups.js", 3);

		//  Reset cursor.
		if (window.popupBeingDragged == null)
			document.documentElement.style.cursor = "";
	},

	//  The popup title bar mouseup event.
	popupTitleBarMouseDown: (event) => {
		GWLog("Popups.popupTitleBarMouseDown", "popups.js", 2);

		//  Prevent other events from triggering.
		event.stopPropagation();

		//  Get the containing popup.
		let popup = event.target.closest(".popup");

		//  Bring the popup to the front.
		Popups.bringPopupToFront(popup);

		//  We only want to do anything on left-clicks.
		if (event.button != 0)
			return;

		//  Also do nothing if the click is on a title bar button.
		if (event.target.closest(".popframe-title-bar-button"))
			return;

		//  Prevent clicks from doing anything other than what we want.
		event.preventDefault();

		//  Mark popup as grabbed.
		popup.classList.toggle("grabbed", true);

		//  Change cursor to “grabbing hand”.
		document.documentElement.style.cursor = "grabbing";

		/*  If the mouse-down event is on the popup title (and the title
			is a link).
			*/
		popup.linkDragTarget = event.target.closest("a");

		/*  Deal with edge case where drag to screen bottom ends up
			with the mouse-up event happening in the popup body.
			*/
		popup.removeEventListener("click", Popups.popupClicked);

		//  Point where the drag began.
		let dragStartMouseCoordX = event.clientX;
		let dragStartMouseCoordY = event.clientY;

		//  Popup initial position.
		let newPopupViewportRect = DOMRect.fromRect(popup.viewportRect);
		//  Do not change popup size.
		newPopupViewportRect.width = 0;
		newPopupViewportRect.height = 0;

		//  Add the drag-end mouseup listener.
		window.addEventListener("mouseup", Popups.popupDragMouseUp);

		//  Viewport width must account for vertical scroll bar.
		let viewportWidth = document.documentElement.offsetWidth;
		let viewportHeight = window.innerHeight;

		//  We define the mousemove listener here to capture variables.
		window.onmousemove = (event) => {
			window.popupBeingDragged = popup;

			//  Mark popup as being dragged.
			popup.classList.toggle("dragging", true);

			//  If dragging by the title, disable its normal click handler.
			if (popup.linkDragTarget)
				popup.linkDragTarget.onclick = (event) => { return false; };

			//  Current drag vector relative to mouse starting position.
			let deltaX = event.clientX - dragStartMouseCoordX;
			let deltaY = event.clientY - dragStartMouseCoordY;

			//  Apply the vector to popup starting position; clamp to screen.
			newPopupViewportRect.x = valMinMax(popup.viewportRect.left + deltaX,
											   0,
											   viewportWidth - popup.viewportRect.width);
			newPopupViewportRect.y = valMinMax(popup.viewportRect.top + deltaY,
											   0,
											   viewportHeight - popup.viewportRect.height);

			//  Set the new popup rect.
			Popups.setPopupViewportRect(popup, newPopupViewportRect);
		};
	},

	//  The mouseup event that ends a popup drag-to-move.
	popupDragMouseUp: (event) => {
		GWLog("Popups.popupDragMouseUp", "popups.js", 2);

		//  Prevent other events from triggering.
		event.stopPropagation();

		//  Remove the mousemove handler.
		window.onmousemove = null;

		//  Reset cursor to normal.
		document.documentElement.style.cursor = "";

		let popup = window.popupBeingDragged;
		if (popup) {
			popup.classList.toggle("grabbed", false);
			popup.classList.toggle("dragging", false);

			//  Re-enable clicking on the title.
			if (popup.linkDragTarget) {
				requestAnimationFrame(() => {
					popup.linkDragTarget.onclick = null;
					popup.linkDragTarget = null;
				});
			}

			//  Cache the viewport rect.
			popup.viewportRect = popup.getBoundingClientRect();

			//  Ensure that the click listener isn’t fired at once.
			requestAnimationFrame(() => {
				popup.addEventListener("click", Popups.popupClicked);
			});

			/*  If the drag of a non-pinned popup ended outside the
				popup (possibly outside the viewport), treat this
				as mousing out of the popup.
				*/
			if ((  !event.target.closest
				 || event.target.closest(".popup") == null)
				&&  Popups.popupIsEphemeral(popup)) {
				Popups.getPopupAncestorStack(popup).reverse().forEach(popupInStack => {
					Popups.clearPopupTimers(popupInStack.spawningTarget);
					Popups.setPopupFadeTimer(popupInStack.spawningTarget);
				});
			}
		}
		window.popupBeingDragged = null;

		//  Remove the listener (ie. we only want this fired once).
		window.removeEventListener("mouseup", Popups.popupDragMouseUp);
	},

	//  The popup title bar mouseup event.
	popupTitleBarMouseUp: (event) => {
		GWLog("Popups.popupTitleBarMouseUp", "popups.js", 2);

		event.target.closest(".popup").classList.toggle("grabbed", false);
	},

	//  The popup title bar double-click event.
	popupTitleBarDoubleClicked: (event) => {
		GWLog("Popups.popupTitleBarDoubleClicked", "popups.js", 2);

		let popup = event.target.closest(".popup");
		if (Popups.popupIsCollapsed(popup)) {
			Popups.uncollapsePopup(popup);
		} else {
			Popups.collapsePopup(popup);
		}
	},

	//	The target mouseenter event.
	targetMouseEnter: (event) => {
		GWLog("Popups.targetMouseEnter", "popups.js", 2);

		if (window.popupBeingDragged)
			return;

		//	Stop the countdown to un-pop the popup.
		Popups.clearPopupTimers(event.target);

		if (event.target.popup == null) {
			//  Start the countdown to pop up the popup (if not already spawned).
			Popups.setPopupSpawnTimer(event.target, event);
		} else {
			/*  If already spawned, just bring the popup to the front and
				re-position it.
				*/
			Popups.bringPopupToFront(event.target.popup);
			Popups.positionPopup(event.target.popup, { x: event.clientX, y: event.clientY });
		}
	},

	//	The target mouseleave event.
	targetMouseLeave: (event) => {
		GWLog("Popups.targetMouseLeave", "popups.js", 2);

		event.target.lastMouseEnterEvent = null;

		Popups.clearPopupTimers(event.target);

		if (event.target.popup)
			Popups.setPopupFadeTimer(event.target);
	},

	//  The keyup event.
	keyUp: (event) => {
		GWLog("Popups.keyUp", "popups.js", 3);
		let allowedKeys = [ "Escape", "Esc", ...(Popups.popupTilingControlKeys.split("")) ];
		if (!allowedKeys.includes(event.key) || Popups.allSpawnedPopups().length == 0)
			return;

		event.preventDefault();

		switch(event.key) {
			case "Escape":
			case "Esc":
				if (Popups.allSpawnedPopups().length > 0)
					Popups.despawnPopup(Popups.focusedPopup());
				break;
			case Popups.popupTilingControlKeys.substr(0,1):
				Popups.zoomPopup(Popups.focusedPopup(), "left");
				break;
			case Popups.popupTilingControlKeys.substr(1,1):
				Popups.zoomPopup(Popups.focusedPopup(), "bottom");
				break;
			case Popups.popupTilingControlKeys.substr(2,1):
				Popups.zoomPopup(Popups.focusedPopup(), "top");
				break;
			case Popups.popupTilingControlKeys.substr(3,1):
				Popups.zoomPopup(Popups.focusedPopup(), "right");
				break;
			case Popups.popupTilingControlKeys.substr(4,1):
				Popups.zoomPopup(Popups.focusedPopup(), "top-left");
				break;
			case Popups.popupTilingControlKeys.substr(5,1):
				Popups.zoomPopup(Popups.focusedPopup(), "top-right");
				break;
			case Popups.popupTilingControlKeys.substr(6,1):
				Popups.zoomPopup(Popups.focusedPopup(), "bottom-right");
				break;
			case Popups.popupTilingControlKeys.substr(7,1):
				Popups.zoomPopup(Popups.focusedPopup(), "bottom-left");
				break;
			case Popups.popupTilingControlKeys.substr(8,1):
				Popups.zoomPopup(Popups.focusedPopup(), "full");
				break;
			case Popups.popupTilingControlKeys.substr(9,1):
				Popups.pinOrUnpinPopup(Popups.focusedPopup());
				break;
			case Popups.popupTilingControlKeys.substr(10,1):
				Popups.collapseOrUncollapsePopup(Popups.focusedPopup());
				break;
			default:
				break;
		}
	}
};

GW.notificationCenter.fireEvent("Popups.didLoad");

/******************/
/*	Initialization.
	*/
doWhenPageLoaded(() => {
	Popups.setup();
});
