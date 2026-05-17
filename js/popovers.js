/*	Popup/floating footnotes to avoid readers needing to scroll to the end of
	the page to see any footnotes; see
	https://ignorethecode.net/blog/2010/04/20/footnotes/ for details.

	Original author:  Lukas Mathis (2010-04-20)
	License: public domain (“And some people have asked me about a license for 
	this piece of code. I think it’s far too short to get its own license, so 
	I’m relinquishing any copyright claims. Consider the code to be public 
	domain. No attribution is necessary.")
 */

/*	Because the ‘popover’ property is already taken by the ‘popover’ built-in
	Element attribute:
	https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Global_attributes/popover
 */
Node.prototype.getPopover = function () {
	return this["_gw_popover"];
};
Node.prototype.setPopover = function (popover) {
	this["_gw_popover"] = popover;
};

Popovers = {
	/*****************/
	/*	Configuration.
		*/

	windowTopPopoverPositionMargin: 0.0,
	windowBottomPopoverPositionMargin: 0.0,

	/******************/
	/*	Implementation.
		*/

	//	Used in: Popovers.containingDocumentForTarget
	rootDocument: document,

	spawnedPopovers: [ ],

	//	Called by: Popovers.setup
	cleanup: () => {
		GWLog("Popovers.cleanup", "popovers.js", 1);

		//  Remove all remnant popovers.
		Popovers.removeAllPopovers();

		//  Remove Escape key event listener.
		document.removeEventListener("keyup", Popovers.keyUp);

		//	Remove history state change event listener.
		window.removeEventListener("popstate", Popovers.popState);

		//	Fire event.
		GW.notificationCenter.fireEvent("Popovers.cleanupDidComplete");
	},

	//	Called by: popovers.js (doWhenPageLoaded)
	setup: () => {
		GWLog("Popovers.setup", "popovers.js", 1);

        //  Run cleanup.
        Popovers.cleanup();

		//  Add Escape key event listener.
		document.addEventListener("keyup", Popovers.keyUp);

		//	Add history state change event listener.
		window.addEventListener("popstate", Popovers.popState);

		//	Fire event.
		GW.notificationCenter.fireEvent("Popovers.setupDidComplete");
	},

	//	Called by: extracts.js
	addTarget: (target, prepareFunction) => {
		//  Bind activate event.
		target.onclick = Popovers.targetClicked;

		//  Set prepare function.
		target.preparePopover = prepareFunction;

		//  Mark target as spawning a popover.
		target.classList.toggle("spawns-popover", true);
	},

	//	Called by: extracts.js
	removeTarget: (target) => {
		//  Remove the popover (if any).
		if (target.getPopover())
			Popovers.removePopover(target.getPopover());

		//  Unbind existing activate events, if any.
		target.onclick = null;

		//  Unset popover prepare function.
		target.preparePopover = null;

		//  Un-mark target as spawning a popover.
		target.classList.toggle("spawns-popover", false);
	},

	/***********/
	/*	Helpers.
		*/

	//	Called by: extracts.js
	scrollElementIntoViewInPopFrame: (element, alwaysRevealTopEdge = false) => {
		let popover = Popovers.containingPopFrame(element);

		let elementRect = element.getBoundingClientRect();
		let popoverBodyRect = popover.body.getBoundingClientRect();
		let popoverScrollViewRect = popover.scrollView.getBoundingClientRect();

		let bottomBound = alwaysRevealTopEdge ? elementRect.top : elementRect.bottom;
		if (   popover.scrollView.scrollTop                                >= elementRect.top - popoverBodyRect.top
			&& popover.scrollView.scrollTop + popoverScrollViewRect.height <= bottomBound     - popoverBodyRect.top)
			return;

		popover.scrollView.scrollTop = elementRect.top - popoverBodyRect.top;
	},

	//	Called by: Popovers.injectPopoverForTarget
	containingDocumentForTarget: (target) => {
		return (Popovers.containingPopFrame(target)?.document ?? Popovers.rootDocument);
	},

	//	Called by: Popovers.keyUp
	getTopPopover: () => {
		return Popovers.spawnedPopovers.first;
	},

	allSpawnedPopFrames: () => {
		return Popovers.allSpawnedPopovers();
	},

	//	Called by: Popovers.targetClicked (event handler)
	//	Called by: Popovers.cleanup
	//	Called by: extracts.js
	allSpawnedPopovers: () => {
		return Popovers.spawnedPopovers;
	},

	//	Called by: Popovers.addTitleBarToPopover
	popoverStackNumber: (popover) => {
		//  If there’s another popover in the ‘stack’ below this one…
		let popoverBelow = (   popover.nextElementSibling
							&& popover.nextElementSibling.classList.contains("popover"))
							? popover.nextElementSibling
							: null;
		if (popoverBelow)
			return parseInt(popoverBelow.titleBar.stackCounter.textContent) + 1;
		else
			return 1;
	},

	//	Called by: extracts.js
	//	Called by: Popovers.containingDocumentForTarget
	//	Called by: Popovers.scrollElementIntoViewInPopFrame
	containingPopFrame: (element) => {
		let shadowBody = element.closest(".shadow-body");
		if (shadowBody)
			return shadowBody.getPopover();

		return element.closest(".popover");
	},

	//	Called by: many functions in many places
	addClassesToPopFrame: (popover, ...args) => {
		popover.classList.add(...args);
		popover.body.classList.add(...args);
	},

	//	Called by: many functions in many places
	removeClassesFromPopFrame: (popover, ...args) => {
		popover.classList.remove(...args);
		popover.body.classList.remove(...args);
	},

	/**********************/
	/*	Popover title bars.
		*/

	/*  Add title bar to a popover which has a populated .titleBarContents.
		*/
	//	Called by: Popovers.injectPopoverForTarget
	addTitleBarToPopover: (popover) => {
		//  Set class ‘has-title-bar’ on the popover.
		popover.classList.add("has-title-bar");

		//  Create and inject the title bar element.
		popover.titleBar = newElement("DIV", { class: "popframe-title-bar" });
		popover.insertBefore(popover.titleBar, popover.firstElementChild);

		//  Add popover stack counter.
		popover.titleBar.stackCounter = newElement("SPAN", { class: "popover-stack-counter" });
		requestAnimationFrame(() => {
			let popoverStackNumber = Popovers.popoverStackNumber(popover);
			popover.titleBar.stackCounter.textContent = popoverStackNumber;
			if (popoverStackNumber == 1)
				popover.titleBar.stackCounter.style.display = "none";
		});
		popover.titleBar.appendChild(popover.titleBar.stackCounter);

		//  Add the provided title bar contents (buttons, title, etc.).
		popover.titleBarContents.forEach(element => {
			popover.titleBar.appendChild(element);

			if (element.buttonAction)
				element.addActivateEvent(element.buttonAction);
		});

		//	Bind auxiliary title-link click event.
		popover.titleBar.querySelectorAll("a").forEach(link => {
			link.addActivateEvent(Popovers.popoverTitleBarLinkClicked);
		});
	},

	/*  Add secondary title-link to a popover which has a title-link.
		*/
	//	Called by: Popovers.injectPopoverForTarget
	addFooterBarToPopover: (popover) => {
		let popoverTitleLink = popover.querySelector(".popframe-title-link");
		if (!popoverTitleLink)
			return;

		//  Set class ‘has-footer-bar’ on the popover.
		popover.classList.add("has-footer-bar");

		//	Inject popover footer bar.
		popover.footerBar = newElement("DIV", { class: "popover-footer-bar" });
		popover.insertBefore(popover.footerBar, null);

		//	Inject footer title-link.
		popover.footerBar.appendChild(newElement("A", {
			href: popoverTitleLink.href,
			class: "popframe-title-link",
			title: `Open ${popoverTitleLink.href} in a new tab.`,
			target: "_blank"
		}, {
			innerHTML: `<span class="bracket">[</span>`
					 + `Open in new tab`
					 + `<span class="bracket">]</span>`
		}));
	},

	/*  Elements and methods related to popover title bars.
		*/
	titleBarComponents: {
		//  Icons for various popup title bar buttons.
		buttonIcons: {
			"close": "times-square-light",
			"options": "gear-solid"
		},

		//  Tooltip text for various popup title bar icons.
		buttonTitles: {
			"close": "Close this popover",
			"options": "Show options"
		},

		//  A generic button, with no icon or tooltip text.
		genericButton: () => {
			let button = newElement("BUTTON", { class: "popframe-title-bar-button" });

			button.buttonAction = (event) => { event.stopPropagation(); };

			return button;
		},

		//  Close button.
		closeButton: () => {
			let button = Popovers.titleBarComponents.genericButton();

			button.classList.add("close-button");
			button.innerHTML = GW.svg(Popovers.titleBarComponents.buttonIcons["close"]);
			button.title = Popovers.titleBarComponents.buttonTitles["close"];
			button.buttonAction = (event) => {
				event.stopPropagation();

				Popovers.removePopover(Popovers.containingPopFrame(event.target));
			};

			return button;
		},

		//  Options button (does nothing by default).
		optionsButton: () => {
			let button = Popovers.titleBarComponents.genericButton();

			button.classList.add("options-button");
			button.innerHTML = GW.svg(Popovers.titleBarComponents.buttonIcons["options"]);
			button.title = Popovers.titleBarComponents.buttonTitles["options"];

			return button;
		}
	},

	/******************/
	/*	Optional parts.
	 */

	addPartToPopFrame: (popover, part) => {
		popover.append(part);
	},

	/********************/
	/*	Popover spawning.
		*/

	//	Called by: Popovers.injectPopoverForTarget
	newPopover: (target) => {
		GWLog("Popovers.newPopover", "popovers.js", 2);

		//	Create popover, scroll view, content view, shadow root, shadow body.
		let popover = newElement("DIV", { class: "popover popframe" }, { spawningTarget: target });
		popover.scrollView = popover.appendChild(newElement("DIV", { class: "popframe-scroll-view" }));
		popover.contentView = popover.scrollView.appendChild(newElement("DIV", { class: "popframe-content-view" }));
		popover.document = popover.contentView.attachShadow({ mode: "open" });
		popover.document.body = popover.body = popover.shadowBody = popover.document.appendChild(newElement("DIV", {
			class: "popframe-body popover-body shadow-body"
		}));

		//	Set reverse references.
		[ popover.body,
		  popover.document,
		  popover.contentView,
		  popover.scrollView
		  ].forEach(node => {
			node.setPopover(popover);
		});

		//	Inject style reset.
		popover.document.insertBefore(newElement("STYLE", null, { innerHTML: `.shadow-body { all: initial; }` }), popover.body);

		//	Default empty title bar.
		popover.titleBarContents = [ ];

		//	Loading spinner and “loading failed” message views.
		popover.loadingSpinnerView = popover.appendChild(newElement("DIV", { class: "popframe-loading-spinner-view" }));
		popover.loadingFailedMessageView = popover.appendChild(newElement("DIV", { class: "popframe-loading-failed-message-view" }));

		return popover;
	},

	//	Called by: extracts.js
	//	Called by: extracts-content.js
	setPopFrameContent: (popover, content) => {
		if (content) {
			popover.body.replaceChildren(content);

			return true;
		} else {
			return false;
		}
	},

	//	Called by: Popovers.injectPopoverForTarget
	updateLocationForSpawnedPopovers: (newState = true) => {
		let newHash = location.hash > "" 
					  ? location.hash.split(";").first 
					  : "#";

		let popoverIDStrings = null;
		if (Popovers.spawnedPopovers.length > 0) {
			popoverIDStrings = Popovers.spawnedPopovers.map(x => {
				return (x.spawningTarget.id > "" 
						? x.spawningTarget.id 
						: fixedEncodeURIComponent(x.spawningTarget.getAttribute("href"))
						);
			}).reverse();
			newHash += ";" + popoverIDStrings.join(":");
		}
		if (newState) {
			history.pushState({ popovers: popoverIDStrings }, null, newHash);
		} else {
			history.replaceState({ popovers: popoverIDStrings }, null, newHash);
		}

		cleanLocationHash();
	},

	//	Called by: Popovers.targetClicked (event handler)
	injectPopoverForTarget: (target, options) => {
		GWLog("Popovers.injectPopoverForTarget", "popovers.js", 2);

		options = Object.assign({
			inheritInitialHeight: true
		}, options);

		//  Create the new popover.
		let popover = Popovers.newPopover(target);

		// Prepare the newly created popover for injection.
		if (popover = target.preparePopover(popover)) {
			//	Attach popover to target.
			Popovers.attachPopoverToTarget(popover, target);
		} else {
			//	Preparation failed.
			return;
		}

		/*  If title bar contents are provided, create and inject the popover
			title bar, and set class `has-title-bar` on the popover.
			*/
		if (popover.titleBarContents.length > 0) {
			Popovers.addTitleBarToPopover(popover);

			if (popover.classList.contains("no-footer-bar") == false)
				Popovers.addFooterBarToPopover(popover);
		}

		//	Add listener to enable tapping on the backdrop to dismiss the popover.
		popover.addEventListener("click", Popovers.popoverClicked);

		//  Get containing document (for popovers spawned from targets in popovers).
		let containingDocument = Popovers.containingDocumentForTarget(target);
		if (containingDocument.getPopover()) {
			/*  Save the parent popover’s scroll state when pushing it down the
				‘stack’.
				*/
			containingDocument.getPopover().lastScrollTop = containingDocument.getPopover().scrollView.scrollTop;

			/*	If popover is still loading (or has failed to load), and the
				`inheritInitialHeight` option is enabled, then set the new 
				popover’s initial height to the height of the parent popover (to
				be adjusted after the new popover finishes loading, if ever).
			 */
			if (   options.inheritInitialHeight
				&& (   Popovers.popFrameStateLoading(popover)
					|| Popovers.popFrameStateLoadingFailed(popover)))
				popover.style.height = Math.round(containingDocument.getPopover().clientHeight) + "px";

			containingDocument.getPopover().parentElement.insertBefore(popover, containingDocument.getPopover());
		} else {
			/*	Locate insertion point. (There are certain elements within which 
				we ought not insert a popover, such as tables, or anything else
				which might scroll horizontally; or other things, perhaps. We
				therefore find the nearest ancestor which is *not* contained in
				one of these “shouldn’t insert popover within this” containers,
				and insert the popover directly after that element.)
			 */
			let insertionTarget, insertWhere;
			let cannotInsertIntoTheseThingsSelector = [
				".table-wrapper"
			].join(", ");
			do {
				insertionTarget = insertWhere ?? target;
				insertWhere = insertionTarget.parentElement;
			} while (insertWhere.closest(cannotInsertIntoTheseThingsSelector));

			//	Inject.
			insertWhere.insertBefore(popover, insertionTarget.nextSibling);
		}

		//	Push popover onto spawned popovers stack.
		Popovers.spawnedPopovers.unshift(popover);

		//	Update location.
		Popovers.updateLocationForSpawnedPopovers();

		//	Designate ancestors.
		let ancestor = popover.parentElement;
		do { ancestor.classList.add("popover-ancestor"); }
		while (   (ancestor = ancestor.parentElement) 
			   && [ "MAIN", "ARTICLE" ].includes(ancestor.tagName) == false);

		//  Mark target as having an open popover associated with it.
		target.classList.add("popover-open", "highlighted");
		//	Fire event.
		GW.notificationCenter.fireEvent("Popovers.popoverDidInject", { popover: popover });

		//	Post-inject adjustments.
		requestAnimationFrame(() => {
			if (target.getPopover() == null)
				return;

			//	Adjust popover position.
			if (target.adjustPopoverWidth)
				target.adjustPopoverWidth(popover);

			//  Scroll page so that entire popover is visible, if need be.
			requestAnimationFrame(() => {
				Popovers.scrollPopoverIntoView(popover);
			});
		});
	},

	/*	Returns full viewport rect for popover and all auxiliary elements
		(title bar, footers, etc.).
	 */
	getPopoverViewportRect: (popover) => {
		return rectUnion(popover.getBoundingClientRect(), ...(Array.from(popover.children).map(x => x.getBoundingClientRect())));
	},

	//	Called by: extracts.js
	popFrameStateLoading: (popover) => {
		return popover.classList.contains("loading");
	},

	//	Called by: extracts.js
	popFrameStateLoadingFailed: (popover) => {
		return popover.classList.contains("loading-failed");
	},

	//	Called by: extracts.js
	setPopFrameStateLoading: (popover) => {
		Popovers.removeClassesFromPopFrame(popover, "loading-failed");
		Popovers.addClassesToPopFrame(popover, "loading");
	},

	//	Called by: extracts.js
	setPopFrameStateLoadingFailed: (popover) => {
		Popovers.removeClassesFromPopFrame(popover, "loading");
		Popovers.addClassesToPopFrame(popover, "loading-failed");
	},

	//	Called by: extracts.js
	clearPopFrameState: (popover) => {
		Popovers.removeClassesFromPopFrame(popover, "loading", "loading-failed");

		//	Clear provisional popover height (inherited from parent popover).
		popover.style.height = "";
	},

	//	Called by: Popovers.injectPopoverForTarget
	//	Called by: extracts.js
	scrollPopoverIntoView: (popover) => {
		let popoverViewportRect = Popovers.getPopoverViewportRect(popover);

		if (popover.closest(".markdownBody") == null) {
			popover.style.top = "0";
		} else {
			let windowScrollOffsetForThisPopover = parseInt(popover.dataset.windowScrollOffset ?? '0');

			let scrollWindowBy = 0;
			if (popoverViewportRect.bottom > window.innerHeight - Popovers.windowBottomPopoverPositionMargin) {
				scrollWindowBy = Math.round(  window.innerHeight * -0.95 
											+ Popovers.windowBottomPopoverPositionMargin 
											+ popoverViewportRect.bottom);
			} else if (popoverViewportRect.top < 0 + Popovers.windowTopPopoverPositionMargin) {
				scrollWindowBy = Math.round(  window.innerHeight * -0.10 
											- Popovers.windowTopPopoverPositionMargin 
											+ popoverViewportRect.top);
			}

			if (scrollWindowBy > 0) {
				window.scrollBy(0, scrollWindowBy);
				popover.dataset.windowScrollOffset = windowScrollOffsetForThisPopover + scrollWindowBy;
			}
		}

		//	Set scroll view height.
		popover.body.style.setProperty("--popframe-scroll-view-height", popover.scrollView.clientHeight + "px");
	},

	//	Called by: Popovers.cleanup
	removeAllPopovers: () => {
		while (Popovers.getTopPopover())
			Popovers.removePopover(Popovers.getTopPopover());
	},

	//	Called by: extracts.js
	cleanPopoversFromContainer: (container) => {
		GWLog("Popovers.cleanPopoversFromContainer", "popovers.js", 2);

		container.querySelectorAll(".popover").forEach(popover => {
			popover.remove();
		});
		container.querySelectorAll(".popover-ancestor").forEach(popoverAncestor => {
			popoverAncestor.classList.remove("popover-ancestor");
		});
		container.querySelectorAll(".popover-open").forEach(popoverSpawningTarget => {
			popoverSpawningTarget.classList.remove("popover-open", "highlighted");
		});
	},

	//	Called by: Popovers.cleanup
	//	Called by: Popovers.targetClicked (event handler)
	//	Called by: Popovers.removeTarget
	//	Called by: Popovers.titleBarComponents.closeButton
	//	Called by: Popovers.injectPopoverForTarget
	removePopover: (popover, remove = false) => {
		GWLog("Popovers.removePopover", "popovers.js", 2);

		//	Proper interaction with history state.
		if (remove == false) {
			history.back();
			return;
		}

		//  If there’s another popover in the ‘stack’ below this one…
		let popoverBelow = popover.nextElementSibling?.classList.contains("popover")
						 ? popover.nextElementSibling
						 : null;

		//	Save place.
		let ancestor = popover.parentElement;

		//	Fire event.
		GW.notificationCenter.fireEvent("Popovers.popoverWillDespawn", { popover: popover });

		//  Detach popover from its spawning target.
		Popovers.detachPopoverFromTarget(popover);

		//  Remove popover from page.
		popover.remove();

		//	Remove from spawned popovers stack.
		Popovers.spawnedPopovers.remove(popover);

		//  … restore its scroll state.
		if (popoverBelow) {
			popoverBelow.scrollView.scrollTop = popoverBelow.lastScrollTop;
		} else {
			do { ancestor.classList.remove("popover-ancestor"); }
			while (ancestor = ancestor.parentElement);
		}

		//	Restore the window’s scroll state to before the popover was injected.
		window.scrollBy(0, -1 * parseInt(popover.dataset.windowScrollOffset ?? '0'));
	},

	//	Called by: Popovers.injectPopoverForTarget
	attachPopoverToTarget: (popover, target) => {
		GWLog("Popovers.attachPopoverToTarget", "popups.js", 2);

		target = target ?? popover.spawningTarget;

        target.classList.add("popover-open");
        target.setPopover(popover);
        target.popFrame = popover;

		popover.spawningTarget = target;
	},

	//	Called by: Popovers.removePopover
	detachPopoverFromTarget: (popover, target) => {
		GWLog("Popovers.detachPopoverFromTarget", "popovers.js", 2);

		target = target ?? popover.spawningTarget;

		target.setPopover(null);
		target.popFrame = null;
		target.classList.remove("popover-open", "highlighted");
	},

	isSpawned: (popover) => {
		return (   popover != null
				&& popover.parentElement != null);
	},

	/*******************/
	/*	Event listeners.
		*/

	//	Added by: Popovers.setup
	popState: (event) => {
		let popoverIDStringsInNewHistoryState = (event.state?.popovers ?? [ ]);
		if (popoverIDStringsInNewHistoryState.length < Popovers.spawnedPopovers.length) {
			while (popoverIDStringsInNewHistoryState.length < Popovers.spawnedPopovers.length) {
				Popovers.removePopover(Popovers.getTopPopover(), true);
			}
		} else if (popoverIDStringsInNewHistoryState.length > Popovers.spawnedPopovers.length) {
			Popovers.updateLocationForSpawnedPopovers(false);
		}
	},

	//	Added by: Popovers.addTarget
	targetClicked: (event) => {
		GWLog("Popovers.targetClicked", "popovers.js", 2);

		//	Only unmodified click events should trigger popover spawn.
		if (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey)
			return;

		event.preventDefault();

		Popovers.injectPopoverForTarget(event.target.closest(".spawns-popover"));

		document.activeElement.blur();
	},

	//	A click (tap) on the popover (which will actually be the popover backdrop).
	popoverClicked: (event) => {
		GWLog("Popovers.popoverClicked", "popovers.js", 2);

		/*	If this isn’t a tap directly on the popover itself (i.e., if the 
			event has bubbled up from a descendant element), we do nothing.
		 */
		if (event.target.classList.contains("popover") == false)
			return;

		event.stopPropagation();

		Popovers.removePopover(event.target);
	},

	//	A click (tap) on a popover title-link.
	popoverTitleBarLinkClicked: (event) => {
		GWLog("Popovers.popoverClicked", "popovers.js", 2);

		let link = event.target.closest("a");
		if (   link.hostname == location.hostname
			&& link.pathname == location.pathname
			&& link.target == "_self")
			Popovers.removePopover(Popovers.containingPopFrame(link));
	},

	/*  The keyup event.
		*/
	//	Added by: Popovers.setup
	keyUp: (event) => {
		GWLog("Popovers.keyUp", "popovers.js", 3);
		let allowedKeys = [ "Escape", "Esc" ];
		if (!allowedKeys.includes(event.key))
			return;

		event.preventDefault();

		switch(event.key) {
			case "Escape":
			case "Esc":
				let popover = Popovers.getTopPopover();
				if (popover)
					Popovers.removePopover(popover);
				break;
			default:
				break;
		}
	}
};

GW.notificationCenter.fireEvent("Popovers.didLoad");
