/*	Popup/floating footnotes to avoid readers needing to scroll to the end of
	the page to see any footnotes; see
	https://ignorethecode.net/blog/2010/04/20/footnotes/ for details.

	Original author:  Lukas Mathis (2010-04-20)
	License: public domain (“And some people have asked me about a license for 
	this piece of code. I think it’s far too short to get its own license, so 
	I’m relinquishing any copyright claims. Consider the code to be public 
	domain. No attribution is necessary.")
 */

Popins = {
	/******************/
	/*	Implementation.
		*/

	//	Used in: Popins.containingDocumentForTarget
	rootDocument: document,

	spawnedPopins: [ ],

	//	Called by: Popins.setup
	cleanup: () => {
		GWLog("Popins.cleanup", "popins.js", 1);

		//  Remove all remnant popins.
		Popins.removeAllPopins();

		//  Remove Escape key event listener.
		document.removeEventListener("keyup", Popins.keyUp);

		//	Fire event.
		GW.notificationCenter.fireEvent("Popins.cleanupDidComplete");
	},

	//	Called by: popins.js (doWhenPageLoaded)
	setup: () => {
		GWLog("Popins.setup", "popins.js", 1);

        //  Run cleanup.
        Popins.cleanup();

		//  Add Escape key event listener.
		document.addEventListener("keyup", Popins.keyUp);

		//	Fire event.
		GW.notificationCenter.fireEvent("Popins.setupDidComplete");
	},

	//	Called by: extracts.js
	addTarget: (target, prepareFunction) => {
		//  Bind activate event.
		target.onclick = Popins.targetClicked;

		//  Set prepare function.
		target.preparePopin = prepareFunction;

		//  Mark target as spawning a popin.
		target.classList.toggle("spawns-popin", true);
	},

	//	Called by: extracts.js
	removeTarget: (target) => {
		//  Remove the popin (if any).
		if (target.popin)
			Popins.removePopin(target.popin);

		//  Unbind existing activate events, if any.
		target.onclick = null;

		//  Unset popin prepare function.
		target.preparePopin = null;

		//  Un-mark target as spawning a popin.
		target.classList.toggle("spawns-popin", false);
	},

	/***********/
	/*	Helpers.
		*/

	//	Called by: extracts.js
	scrollElementIntoViewInPopFrame: (element, alwaysRevealTopEdge = false) => {
		let popin = Popins.containingPopFrame(element);

		let elementRect = element.getBoundingClientRect();
		let popinBodyRect = popin.body.getBoundingClientRect();
		let popinScrollViewRect = popin.scrollView.getBoundingClientRect();

		let bottomBound = alwaysRevealTopEdge ? elementRect.top : elementRect.bottom;
		if (   popin.scrollView.scrollTop                              >= elementRect.top    - popinBodyRect.top
			&& popin.scrollView.scrollTop + popinScrollViewRect.height <= bottomBound - popinBodyRect.top)
			return;

		popin.scrollView.scrollTop = elementRect.top - popinBodyRect.top;
	},

	//	Called by: Popins.injectPopinForTarget
	containingDocumentForTarget: (target) => {
		return (Popins.containingPopFrame(target)?.document ?? Popins.rootDocument);
	},

	//	Called by: Popins.keyUp
	getTopPopin: () => {
		return Popins.spawnedPopins.first;
	},

	allSpawnedPopFrames: () => {
		return Popins.allSpawnedPopins();
	},

	//	Called by: Popins.targetClicked (event handler)
	//	Called by: Popins.cleanup
	//	Called by: extracts.js
	allSpawnedPopins: () => {
		return Popins.spawnedPopins;
	},

	//	Called by: Popins.addTitleBarToPopin
	popinStackNumber: (popin) => {
		//  If there’s another popin in the ‘stack’ below this one…
		let popinBelow = (   popin.nextElementSibling
						  && popin.nextElementSibling.classList.contains("popin"))
						 ? popin.nextElementSibling
						 : null;
		if (popinBelow)
			return parseInt(popinBelow.titleBar.stackCounter.textContent) + 1;
		else
			return 1;
	},

	//	Called by: extracts.js
	//	Called by: Popins.containingDocumentForTarget
	//	Called by: Popins.scrollElementIntoViewInPopFrame
	containingPopFrame: (element) => {
		let shadowBody = element.closest(".shadow-body");
		if (shadowBody)
			return shadowBody.popin;

		return element.closest(".popin");
	},

	//	Called by: many functions in many places
	addClassesToPopFrame: (popin, ...args) => {
		popin.classList.add(...args);
		popin.body.classList.add(...args);
	},

	//	Called by: many functions in many places
	removeClassesFromPopFrame: (popin, ...args) => {
		popin.classList.remove(...args);
		popin.body.classList.remove(...args);
	},

	/********************/
	/*	Popin title bars.
		*/

	/*  Add title bar to a popin which has a populated .titleBarContents.
		*/
	//	Called by: Popins.injectPopinForTarget
	addTitleBarToPopin: (popin) => {
		//  Set class ‘has-title-bar’ on the popin.
		popin.classList.add("has-title-bar");

		//  Create and inject the title bar element.
		popin.titleBar = newElement("DIV", { class: "popframe-title-bar" });
		popin.insertBefore(popin.titleBar, popin.firstElementChild);

		//  Add popin stack counter.
		popin.titleBar.stackCounter = newElement("SPAN", { class: "popin-stack-counter" });
		requestAnimationFrame(() => {
			let popinStackNumber = Popins.popinStackNumber(popin);
			popin.titleBar.stackCounter.textContent = popinStackNumber;
			if (popinStackNumber == 1)
				popin.titleBar.stackCounter.style.display = "none";
		});
		popin.titleBar.appendChild(popin.titleBar.stackCounter);

		//  Add the provided title bar contents (buttons, title, etc.).
		popin.titleBarContents.forEach(element => {
			popin.titleBar.appendChild(element);

			if (element.buttonAction)
				element.addActivateEvent(element.buttonAction);
		});
	},

	/*  Add secondary title-link to a popin which has a title-link.
		*/
	//	Called by: Popins.injectPopinForTarget
	addFooterBarToPopin: (popin) => {
		let popinTitleLink = popin.querySelector(".popframe-title-link");
		if (!popinTitleLink)
			return;

		//  Set class ‘has-footer-bar’ on the popin.
		popin.classList.add("has-footer-bar");

		//	Inject popin footer bar.
		popin.footerBar = newElement("DIV", { class: "popin-footer-bar" });
		popin.insertBefore(popin.footerBar, null);

		//	Inject footer title-link.
		popin.footerBar.appendChild(newElement("A", {
			href: popinTitleLink.href,
			class: "popframe-title-link",
			title: `Open ${popinTitleLink.href} in a new tab.`,
			target: "_blank"
		}, {
			innerHTML: `<span class="bracket">[</span>`
					 + `Open in new tab`
					 + `<span class="bracket">]</span>`
		}));
	},

	/*  Elements and methods related to popin title bars.
		*/
	titleBarComponents: {
		//  Icons for various popup title bar buttons.
		buttonIcons: {
			"close": "times-square-light",
			"options": "gear-solid"
		},

		//  Tooltip text for various popup title bar icons.
		buttonTitles: {
			"close": "Close this popin",
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
			let button = Popins.titleBarComponents.genericButton();

			button.classList.add("close-button");
			button.innerHTML = GW.svg(Popins.titleBarComponents.buttonIcons["close"]);
			button.title = Popins.titleBarComponents.buttonTitles["close"];
			button.buttonAction = (event) => {
				event.stopPropagation();

				Popins.removePopin(Popins.containingPopFrame(event.target));
			};

			return button;
		},

		//  Options button (does nothing by default).
		optionsButton: () => {
			let button = Popins.titleBarComponents.genericButton();

			button.classList.add("options-button");
			button.innerHTML = GW.svg(Popins.titleBarComponents.buttonIcons["options"]);
			button.title = Popins.titleBarComponents.buttonTitles["options"];

			return button;
		}
	},

	/******************/
	/*	Optional parts.
	 */

	addPartToPopFrame: (popin, part) => {
		popin.append(part);
	},

	/******************/
	/*	Popin spawning.
		*/

	//	Called by: Popins.injectPopinForTarget
	newPopin: (target) => {
		GWLog("Popins.newPopin", "popins.js", 2);

		//	Create popin, scroll view, content view, shadow root, shadow body.
		let popin = newElement("DIV", { class: "popin popframe" }, { spawningTarget: target });
		popin.scrollView = popin.appendChild(newElement("DIV", { class: "popframe-scroll-view" }));
		popin.contentView = popin.scrollView.appendChild(newElement("DIV", { class: "popframe-content-view" }));
		popin.document = popin.contentView.attachShadow({ mode: "open" });
		popin.document.body = popin.body = popin.shadowBody = popin.document.appendChild(newElement("DIV", {
			class: "popframe-body popin-body shadow-body"
		}));

		//	Set reverse references.
		popin.document.popin = popin.body.popin = popin.contentView.popin = popin.scrollView.popin = popin;

		//	Inject style reset.
		popin.document.insertBefore(newElement("STYLE", null, { innerHTML: `.shadow-body { all: initial; }` }), popin.body);

		//	Default empty title bar.
		popin.titleBarContents = [ ];

		//	Loading spinner and “loading failed” message views.
		popin.loadingSpinnerView = popin.appendChild(newElement("DIV", { class: "popframe-loading-spinner-view" }));
		popin.loadingFailedMessageView = popin.appendChild(newElement("DIV", { class: "popframe-loading-failed-message-view" }));

		return popin;
	},

	//	Called by: extracts.js
	//	Called by: extracts-content.js
	setPopFrameContent: (popin, content) => {
		if (content) {
			popin.body.replaceChildren(content);

			return true;
		} else {
			return false;
		}
	},

	//	Called by: Popins.targetClicked (event handler)
	injectPopinForTarget: (target, options) => {
		GWLog("Popins.injectPopinForTarget", "popins.js", 2);

		options = Object.assign({
			inheritInitialHeight: true
		}, options);

		//  Create the new popin.
		let popin = Popins.newPopin(target);

		// Prepare the newly created popin for injection.
		if (popin = target.preparePopin(popin)) {
			//	Attach popin to target.
			Popins.attachPopinToTarget(popin, target);
		} else {
			//	Preparation failed.
			return;
		}

		/*  If title bar contents are provided, create and inject the popin
			title bar, and set class `has-title-bar` on the popin.
			*/
		if (popin.titleBarContents.length > 0) {
			Popins.addTitleBarToPopin(popin);

			if (popin.classList.contains("no-footer-bar") == false)
				Popins.addFooterBarToPopin(popin);
		}

		//	Add listener to enable tapping on the backdrop to dismiss the popin.
		popin.addEventListener("click", Popins.popinClicked);

		//  Get containing document (for popins spawned from targets in popins).
		let containingDocument = Popins.containingDocumentForTarget(target);
		if (containingDocument.popin) {
			/*  Save the parent popin’s scroll state when pushing it down the
				‘stack’.
				*/
			containingDocument.popin.lastScrollTop = containingDocument.popin.scrollView.scrollTop;

			/*	If popin is still loading (or has failed to load), and the
				`inheritInitialHeight` option is enabled, then set the new 
				popin’s initial height to the height of the parent popin (to be 
				adjusted after the new popin finishes loading, if ever).
			 */
			if (   options.inheritInitialHeight
				&& (   Popins.popFrameStateLoading(popin)
					|| Popins.popFrameStateLoadingFailed(popin)))
				popin.style.height = Math.round(containingDocument.popin.clientHeight) + "px";

			containingDocument.popin.parentElement.insertBefore(popin, containingDocument.popin);
		} else {
			target.parentElement.insertBefore(popin, target.nextSibling);
		}

		//	Push popin onto spawned popins stack.
		Popins.spawnedPopins.unshift(popin);

		//	Designate ancestors.
		let ancestor = popin.parentElement;
		do { ancestor.classList.add("popin-ancestor"); }
		while (   (ancestor = ancestor.parentElement) 
			   && [ "MAIN", "ARTICLE" ].includes(ancestor.tagName) == false);

		//  Mark target as having an open popin associated with it.
		target.classList.add("popin-open", "highlighted");

		//	Fire event.
		GW.notificationCenter.fireEvent("Popins.popinDidInject", { popin: popin });

		//	Post-inject adjustments.
		requestAnimationFrame(() => {
			if (target.popin == null)
				return;

			//	Adjust popin position.
			if (target.adjustPopinWidth)
				target.adjustPopinWidth(popin);

			//  Scroll page so that entire popin is visible, if need be.
			requestAnimationFrame(() => {
				Popins.scrollPopinIntoView(popin);
			});
		});
	},

	/*	Returns full viewport rect for popin and all auxiliary elements
		(title bar, footers, etc.).
	 */
	getPopinViewportRect: (popin) => {
		return rectUnion(popin.getBoundingClientRect(), ...(Array.from(popin.children).map(x => x.getBoundingClientRect())));
	},

	//	Called by: extracts.js
	popFrameStateLoading: (popin) => {
		return popin.classList.contains("loading");
	},

	//	Called by: extracts.js
	popFrameStateLoadingFailed: (popin) => {
		return popin.classList.contains("loading-failed");
	},

	//	Called by: extracts.js
	setPopFrameStateLoading: (popin) => {
		Popins.removeClassesFromPopFrame(popin, "loading-failed");
		Popins.addClassesToPopFrame(popin, "loading");
	},

	//	Called by: extracts.js
	setPopFrameStateLoadingFailed: (popin) => {
		Popins.removeClassesFromPopFrame(popin, "loading");
		Popins.addClassesToPopFrame(popin, "loading-failed");
	},

	//	Called by: extracts.js
	clearPopFrameState: (popin) => {
		Popins.removeClassesFromPopFrame(popin, "loading", "loading-failed");

		//	Clear provisional popin height (inherited from parent popin).
		popin.style.height = "";
	},

	//	Called by: Popins.injectPopinForTarget
	//	Called by: extracts.js
	scrollPopinIntoView: (popin) => {
		let popinViewportRect = Popins.getPopinViewportRect(popin);

		if (popin.closest(".markdownBody") == null) {
			popin.style.top = "0";
		} else {
			let windowScrollOffsetForThisPopin = parseInt(popin.dataset.windowScrollOffset ?? '0');

			let scrollWindowBy = 0;
			if (popinViewportRect.bottom > window.innerHeight) {
				scrollWindowBy = Math.round((window.innerHeight * 0.05) + popinViewportRect.bottom - window.innerHeight);
			} else if (popinViewportRect.top < 0) {
				scrollWindowBy = Math.round((window.innerHeight * -0.1) + popinViewportRect.top);
			}

			if (scrollWindowBy > 0) {
				window.scrollBy(0, scrollWindowBy);
				popin.dataset.windowScrollOffset = windowScrollOffsetForThisPopin + scrollWindowBy;
			}
		}

		//	Set scroll view height.
		popin.body.style.setProperty("--popframe-scroll-view-height", popin.scrollView.clientHeight + "px");
	},

	//	Called by: Popins.cleanup
	removeAllPopins: () => {
		while (Popins.getTopPopin())
			Popins.removePopin(Popins.getTopPopin());
	},

	//	Called by: extracts.js
	cleanPopinsFromContainer: (container) => {
		GWLog("Popins.cleanPopinsFromContainer", "popins.js", 2);

		container.querySelectorAll(".popin").forEach(popin => {
			popin.remove();
		});
		container.querySelectorAll(".popin-ancestor").forEach(popinAncestor => {
			popinAncestor.classList.remove("popin-ancestor");
		});
		container.querySelectorAll(".popin-open").forEach(popinSpawningTarget => {
			popinSpawningTarget.classList.remove("popin-open", "highlighted");
		});
	},

	//	Called by: Popins.cleanup
	//	Called by: Popins.targetClicked (event handler)
	//	Called by: Popins.removeTarget
	//	Called by: Popins.titleBarComponents.closeButton
	//	Called by: Popins.injectPopinForTarget
	removePopin: (popin) => {
		GWLog("Popins.removePopin", "popins.js", 2);

		//  If there’s another popin in the ‘stack’ below this one…
		let popinBelow = popin.nextElementSibling?.classList.contains("popin")
						 ? popin.nextElementSibling
						 : null;

		//	Save place.
		let ancestor = popin.parentElement;

		//	Fire event.
		GW.notificationCenter.fireEvent("Popins.popinWillDespawn", { popin: popin });

		//  Detach popin from its spawning target.
		Popins.detachPopinFromTarget(popin);

		//  Remove popin from page.
		popin.remove();

		//	Remove from spawned popins stack.
		Popins.spawnedPopins.remove(popin);

		//  … restore its scroll state.
		if (popinBelow) {
			popinBelow.scrollView.scrollTop = popinBelow.lastScrollTop;
		} else {
			do { ancestor.classList.remove("popin-ancestor"); }
			while (ancestor = ancestor.parentElement);
		}

		//	Restore the window’s scroll state to before the popin was injected.
		window.scrollBy(0, -1 * parseInt(popin.dataset.windowScrollOffset ?? '0'));
	},

	//	Called by: Popins.injectPopinForTarget
	attachPopinToTarget: (popin, target) => {
		GWLog("Popins.attachPopinToTarget", "popups.js", 2);

		target = target ?? popin.spawningTarget;

        target.classList.add("popin-open");
        target.popin = popin;
        target.popFrame = popin;

		popin.spawningTarget = target;
	},

	//	Called by: Popins.removePopin
	detachPopinFromTarget: (popin, target) => {
		GWLog("Popins.detachPopinFromTarget", "popins.js", 2);

		target = target ?? popin.spawningTarget;

		target.popin = null;
		target.popFrame = null;
		target.classList.remove("popin-open", "highlighted");
	},

	isSpawned: (popin) => {
		return (   popin != null
				&& popin.parentElement != null);
	},

	/*******************/
	/*	Event listeners.
		*/

	//	Added by: Popins.addTarget
	targetClicked: (event) => {
		GWLog("Popins.targetClicked", "popins.js", 2);

		//	Only unmodified click events should trigger popin spawn.
		if (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey)
			return;

		event.preventDefault();

		Popins.injectPopinForTarget(event.target.closest(".spawns-popin"));

		document.activeElement.blur();
	},

	//	A click (tap) on the popin (which will actually be the popin backdrop).
	popinClicked: (event) => {
		GWLog("Popins.popinClicked", "popins.js", 2);

		/*	If this isn’t a tap directly on the popin itself (i.e., if the event
			has bubbled up from a descendant element), we do nothing.
		 */
		if (event.target.classList.contains("popin") == false)
			return;

		event.stopPropagation();

		Popins.removePopin(event.target);
	},

	/*  The keyup event.
		*/
	//	Added by: Popins.setup
	keyUp: (event) => {
		GWLog("Popins.keyUp", "popins.js", 3);
		let allowedKeys = [ "Escape", "Esc" ];
		if (!allowedKeys.includes(event.key))
			return;

		event.preventDefault();

		switch(event.key) {
			case "Escape":
			case "Esc":
				let popin = Popins.getTopPopin();
				if (popin)
					Popins.removePopin(popin);
				break;
			default:
				break;
		}
	}
};

GW.notificationCenter.fireEvent("Popins.didLoad");
