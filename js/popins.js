/*	Popup/floating footnotes to avoid readers needing to scroll to the end of
	the page to see any footnotes; see
	http://ignorethecode.net/blog/2010/04/20/footnotes/ for details.
Original author:  Lukas Mathis (2010-04-20)
License: public domain ("And some people have asked me about a license for this piece of code. I think it’s far too short to get its own license, so I’m relinquishing any copyright claims. Consider the code to be public domain. No attribution is necessary.")
	*/
Popins = {
	/******************/
	/*	Implementation.
		*/

	rootDocument: document.firstElementChild,

	cleanup: () => {
		GWLog("Popins.cleanup", "popins.js", 1);

		//  Remove all remnant popins.
		Popins.allSpawnedPopins().forEach(popin => {
			Popins.removePopin(popin);
		});
	},

	setup: () => {
		GWLog("Popins.setup", "popins.js", 1);

        //  Run cleanup.
        Popins.cleanup();

		GW.notificationCenter.fireEvent("Popins.setupDidComplete");
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
				target.classList.toggle("no-popin", true);
				return;
			}

			if (!targets.testTarget(target)) {
				target.classList.toggle("no-popin", true);
				return;
			}

			//  Bind activate event.
			target.onclick = Popins.targetClicked;

			//  Set prepare function.
			target.preparePopin = prepareFunction;

			//  Run any custom processing.
			if (targetPrepareFunction)
				targetPrepareFunction(target);

			//  Mark target as spawning a popin.
			target.classList.toggle("spawns-popin", true);
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
				target.classList.toggle("no-popin", false);
				return;
			}

			if (!targets.testTarget(target)) {
				target.classList.toggle("no-popin", false);
				return;
			}

			//  Unbind existing activate events, if any.
			target.onclick = null;

			//  Remove the popin (if any).
			if (target.popin)
				Popins.removePopin(target.popin);

			//  Unset popin prepare function.
			target.preparePopin = null;

			//  Un-mark target as spawning a popin.
			target.classList.toggle("spawns-popin", false);

			//  Run any custom processing.
			if (targetRestoreFunction)
				targetRestoreFunction(target);
		});
	},

	/***********/
	/*	Helpers.
		*/

	scrollElementIntoViewInPopFrame: (element) => {
		let popin = element.closest(".popin");
		popin.scrollView.scrollTop = element.getBoundingClientRect().top - popin.contentView.getBoundingClientRect().top;
	},

	containingDocumentForTarget: (target) => {
		let containingPopin = target.closest(".popin");
		return (containingPopin ? containingPopin.contentView : Popins.rootDocument);
	},

	allSpawnedPopins: () => {
		//  TODO: make this more efficient! (keep a running array or somesuch)
		return Array.from(document.querySelectorAll(".popin"));
	},

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

	/********************/
	/*	Popin title bars.
		*/

	//  Add title bar to a popin which has a populated .titleBarContents.
	addTitleBarToPopin: (popin) => {
		//  Set class ‘has-title-bar’ on the popin.
		popin.classList.add("has-title-bar");

		//  Create and inject the title bar element.
		popin.titleBar = document.createElement("div");
		popin.titleBar.classList.add("popframe-title-bar");
		popin.insertBefore(popin.titleBar, popin.firstElementChild);

		//  Add popin stack counter.
		popin.titleBar.stackCounter = document.createElement("span");
		popin.titleBar.stackCounter.classList.add("popin-stack-counter");
		requestAnimationFrame(() => {
			let popinStackNumber = Popins.popinStackNumber(popin);
			popin.titleBar.stackCounter.textContent = popinStackNumber;
			if (popinStackNumber == 1)
				popin.titleBar.stackCounter.style.display = "none";
		});
		popin.titleBar.appendChild(popin.titleBar.stackCounter);

		//  Add the provided title bar contents (buttons, title, etc.).
		popin.titleBarContents.forEach(elementOrHTML => {
			if (typeof elementOrHTML == "string") {
				popin.titleBar.insertAdjacentHTML("beforeend", elementOrHTML);
			} else {
				popin.titleBar.appendChild(elementOrHTML);
			}
			let newlyAddedElement = popin.titleBar.lastElementChild;

			if (newlyAddedElement.buttonAction)
				newlyAddedElement.addActivateEvent(newlyAddedElement.buttonAction);
		});
	},

	//  Elements and methods related to popin title bars.
	titleBarComponents: {
		genericButton: () => {
			let button = document.createElement("BUTTON");
			button.classList.add("popframe-title-bar-button");
			button.buttonAction = (event) => {
				event.stopPropagation();
			};
			return button;
		},

		closeButton: () => {
			let button = Popins.titleBarComponents.genericButton();
			button.innerHTML = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 512"><path d="M193.94 256L296.5 153.44l21.15-21.15c3.12-3.12 3.12-8.19 0-11.31l-22.63-22.63c-3.12-3.12-8.19-3.12-11.31 0L160 222.06 36.29 98.34c-3.12-3.12-8.19-3.12-11.31 0L2.34 120.97c-3.12 3.12-3.12 8.19 0 11.31L126.06 256 2.34 379.71c-3.12 3.12-3.12 8.19 0 11.31l22.63 22.63c3.12 3.12 8.19 3.12 11.31 0L160 289.94 262.56 392.5l21.15 21.15c3.12 3.12 8.19 3.12 11.31 0l22.63-22.63c3.12-3.12 3.12-8.19 0-11.31L193.94 256z"/></svg>`;
			button.title = "Close this popin";
			button.classList.add("close-button");
			button.buttonAction = (event) => {
				event.stopPropagation();

				let popin = event.target.closest(".popin");
				if (popin) {
					Popins.removePopin(popin);
				}
			};
			return button;
		},

		optionsButton: () => {
			let button = Popins.titleBarComponents.genericButton();
			button.innerHTML = `<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 20 20"><g transform="translate(10 10)"><path id="a" d="M1.5-10h-3l-1 6.5h5m0 7h-5l1 6.5h3"/><use transform="rotate(45)" xlink:href="#a"/><use transform="rotate(90)" xlink:href="#a"/><use transform="rotate(135)" xlink:href="#a"/></g><path d="M10 2.5a7.5 7.5 0 000 15 7.5 7.5 0 000-15v4a3.5 3.5 0 010 7 3.5 3.5 0 010-7"/></svg>`;
			return button;
		}
	},

	/******************/
	/*	Popin spawning.
		*/

	newPopin: (target) => {
		GWLog("Popins.newPopin", "popins.js", 2);

		let popin = document.createElement("div");
		popin.classList.add("popin", "popframe");
		popin.innerHTML = `<div class="popframe-scroll-view"><div class="popframe-content-view"></div></div>`;
		popin.scrollView = popin.querySelector(".popframe-scroll-view");
		popin.contentView = popin.querySelector(".popframe-content-view");
		popin.contentView.popin = popin.scrollView.popin = popin;
		popin.titleBarContents = [ ];

		//  Give the popin a reference to the target.
		popin.spawningTarget = target;

		return popin;
	},

	setPopFrameContent: (popin, contentHTML) => {
		popin.contentView.innerHTML = contentHTML;
		return (contentHTML > "");
	},

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
		if (target.popin.titleBarContents.length > 0)
			Popins.addTitleBarToPopin(target.popin);

		//  Get containing document.
		let containingDocument = Popins.containingDocumentForTarget(target);

		//  Remove (other) existing popins on this level.
		containingDocument.querySelectorAll(".popin").forEach(existingPopin => {
			if (existingPopin != target.popin)
				Popins.removePopin(existingPopin);
		});

		//  Inject the popin.
		if (containingDocument.popin) {
			/*  Save the parent popup’s scroll state when pushing it down the 
				‘stack’.
				*/
			containingDocument.popin.lastScrollTop = containingDocument.popin.scrollView.scrollTop;

			containingDocument.popin.parentElement.insertBefore(target.popin, containingDocument.popin);
		} else {
			target.parentElement.insertBefore(target.popin, target.nextSibling);
		}

		//  Mark target as having an open popin associated with it.
		target.classList.add("popin-open", "highlighted");

		//  Scroll page so that entire popin is visible, if need be.
		requestAnimationFrame(() => {
			Popins.scrollPopinIntoView(target.popin);
		});

		GW.notificationCenter.fireEvent("Popins.popinDidInject", { popin: target.popin });
	},

	scrollPopinIntoView: (popin) => {
		let popinViewportRect = popin.getBoundingClientRect();

		if (popinViewportRect.bottom > window.innerHeight) {
			window.scrollBy(0, (window.innerHeight * 0.1) + popinViewportRect.bottom - window.innerHeight);
		} else if (popinViewportRect.top < 0) {
			window.scrollBy(0, (window.innerHeight * -0.1) + popinViewportRect.top);
		}
	},

	removePopin: (popin) => {
		GWLog("Popins.removePopin", "popins.js", 2);

		//  If there’s another popin in the ‘stack’ below this one…
		let popinBelow = (   popin.nextElementSibling 
						  && popin.nextElementSibling.classList.contains("popin")) 
						 ? popin.nextElementSibling 
						 : null;

		//  Remove popin from page.
		popin.remove();

		//  … restore its scroll state.
		if (popinBelow)
			popinBelow.scrollView.scrollTop = popinBelow.lastScrollTop;

		//  Detach popin from its spawning target.
		Popins.detachPopinFromTarget(popin);
	},

	detachPopinFromTarget: (popin) => {
		GWLog("Popins.detachPopinFromTarget", "popins.js", 2);

		popin.spawningTarget.popin = null;
		popin.spawningTarget.popFrame = null;
		popin.spawningTarget.classList.remove("popin-open", "highlighted");
	},

	/*******************/
	/*	Event listeners.
		*/

	targetClicked: (event) => {
		GWLog("Popins.targetClicked", "popins.js", 2);

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
};

GW.notificationCenter.fireEvent("Popins.didLoad");

/******************/
/*	Initialization.
	*/
doWhenPageLoaded(() => {
	Popins.setup();
});
