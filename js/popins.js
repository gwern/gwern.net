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
		while (Popins.getTopPopin())
			Popins.removePopin(Popins.getTopPopin());

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
		let containingPopin = Popins.containingPopFrame(target);
		return (containingPopin ? containingPopin.document : Popins.rootDocument);
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
		popin.titleBar = newElement("DIV");
		popin.titleBar.classList.add("popframe-title-bar");
		popin.insertBefore(popin.titleBar, popin.firstElementChild);

		//  Add popin stack counter.
		popin.titleBar.stackCounter = newElement("SPAN");
		popin.titleBar.stackCounter.classList.add("popin-stack-counter");
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
		popin.footerBar = newElement("DIV");
		popin.footerBar.classList.add("popin-footer-bar");
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
			let button = newElement("BUTTON");
			button.classList.add("popframe-title-bar-button");

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

	/************************/
	/*	Optional UI elements.
	 */

	addUIElementsToPopFrame: (popin, ...args) => {
		popin.uiElementsContainer.append(...args);
	},

	/******************/
	/*	Popin spawning.
		*/

	//	Called by: Popins.injectPopinForTarget
	newPopin: (target) => {
		GWLog("Popins.newPopin", "popins.js", 2);

		let popin = newElement("DIV");
		popin.classList.add("popin", "popframe");
		popin.innerHTML = `<div class="popframe-scroll-view"><div class="popframe-content-view"></div></div>`;
		popin.scrollView = popin.querySelector(".popframe-scroll-view");
		popin.contentView = popin.querySelector(".popframe-content-view");

		popin.contentView.attachShadow({ mode: "open" });
		popin.document = popin.contentView.shadowRoot;
		popin.document.appendChild(newElement("DIV"));
		popin.document.body = popin.body = popin.shadowBody = popin.document.firstElementChild;
		popin.body.classList.add("popframe-body", "popin-body", "shadow-body");

		let styleReset = newElement("STYLE");
		styleReset.innerHTML = `.shadow-body { all: initial; }`;
		popin.document.insertBefore(styleReset, popin.body);

		popin.document.popin = popin;

		popin.body.popin = popin.contentView.popin = popin.scrollView.popin = popin;

		popin.titleBarContents = [ ];

		//  Give the popin a reference to the target.
		popin.spawningTarget = target;

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
	injectPopinForTarget: (target) => {
		GWLog("Popins.injectPopinForTarget", "popins.js", 2);

		//  Create the new popin.
		target.popFrame = target.popin = Popins.newPopin(target);

		// Prepare the newly created popin for injection.
		if (!(target.popFrame = target.popin = target.preparePopin(target.popin)))
			return;

		/*  If title bar contents are provided, create and inject the popin
			title bar, and set class `has-title-bar` on the popin.
			*/
		if (target.popin.titleBarContents.length > 0) {
			Popins.addTitleBarToPopin(target.popin);

			if (target.popin.classList.contains("no-footer-bar") == false)
				Popins.addFooterBarToPopin(target.popin);
		}

		//	Add listener to enable tapping on the backdrop to dismiss the popin.
		target.popin.addEventListener("click", Popins.popinClicked);

		//  Get containing document.
		let containingDocument = Popins.containingDocumentForTarget(target);

		//  Remove (other) existing popins on this level.
		containingDocument.querySelectorAll(".popin").forEach(existingPopin => {
			if (existingPopin != target.popin)
				Popins.removePopin(existingPopin);
		});

		//	Set rendering progress indicator (spinner).
		Popins.addClassesToPopFrame(target.popin, "rendering");

		//  Inject the popin.
		if (containingDocument.popin) {
			/*  Save the parent popin’s scroll state when pushing it down the
				‘stack’.
				*/
			containingDocument.popin.lastScrollTop = containingDocument.popin.scrollView.scrollTop;

			containingDocument.popin.parentElement.insertBefore(target.popin, containingDocument.popin);
		} else {
			target.parentElement.insertBefore(target.popin, target.nextSibling);
		}

		//	Push popin onto spawned popins stack.
		Popins.spawnedPopins.unshift(target.popin);

		//	Designate ancestors.
		let ancestor = target.popin.parentElement;
		do { ancestor.classList.add("popin-ancestor"); }
		while (   (ancestor = ancestor.parentElement) 
			   && [ "MAIN", "ARTICLE" ].includes(ancestor.tagName) == false);

		//  Mark target as having an open popin associated with it.
		target.classList.add("popin-open", "highlighted");

		//	Fire event.
		GW.notificationCenter.fireEvent("Popins.popinDidInject", { popin: target.popin });

		//	Post-inject adjustments.
		requestAnimationFrame(() => {
			if (target.popin == null)
				return;

			//	Adjust popin position.
			if (target.adjustPopinWidth)
				target.adjustPopinWidth(target.popin);

			//	Disable rendering progress indicator (spinner).
			Popins.removeClassesFromPopFrame(target.popin, "rendering");

			//	Set scroll view height.
			target.popin.body.style.setProperty("--popframe-scroll-view-height", target.popin.scrollView.clientHeight + "px");

			//  Scroll page so that entire popin is visible, if need be.
			requestAnimationFrame(() => {
				Popins.scrollPopinIntoView(target.popin);
			});
		});
	},

	/*	Returns full viewport rect for popin and all auxiliary elements
		(title bar, footers, etc.).
	 */
	getPopinViewportRect: (popin) => {
		return rectUnion(popin.getBoundingClientRect(), ...(Array.from(popin.children).map(x => x.getBoundingClientRect())));
	},

	//	Called by: Popins.injectPopinForTarget
	//	Called by: extracts.js
	scrollPopinIntoView: (popin) => {
		let popinViewportRect = Popins.getPopinViewportRect(popin);

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
	},

	//	Called by: extracts.js
	removeAllPopinsInContainer: (container) => {
		GWLog("Popins.removeAllPopinsInContainer", "popins.js", 2);

		container.querySelectorAll(".popin").forEach(popin => {
			Popins.removePopin(popin);
		});
	},

	//	Called by: Popins.cleanup
	//	Called by: Popins.targetClicked (event handler)
	//	Called by: Popins.removeTargetsWithin
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
		Popins.spawnedPopins.shift();

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

	//	Called by: Popins.removePopin
	detachPopinFromTarget: (popin) => {
		GWLog("Popins.detachPopinFromTarget", "popins.js", 2);

		if (popin.spawningTarget == null)
			return;

		popin.spawningTarget.popin = null;
		popin.spawningTarget.popFrame = null;
		popin.spawningTarget.classList.remove("popin-open", "highlighted");
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

		let target = event.target.closest(".spawns-popin");

		if (target.classList.contains("popin-open")) {
			Popins.allSpawnedPopins().forEach(popin => {
				Popins.removePopin(popin);
			});
		} else {
			Popins.injectPopinForTarget(target);
		}

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
