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
	cleanup: () => {
		GWLog("Popins.cleanup", "popins.js", 1);

		//  Remove all remnant popins.
		//  TODO: this
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
	addTargets: (targets, prepareFunction, targetPrepareFunction = null) => {
		GWLog("Popins.addTargets", "popins.js", 1);

		Popins.addTargetsWithin(document, targets, prepareFunction, targetPrepareFunction);
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
			Popins.removePopin(target);

			//  Unset popin prepare function.
			target.preparePopin = null;

			//  Un-mark target as spawning a popin.
			target.classList.toggle("spawns-popin", false);

			//  Run any custom processing.
			if (targetRestoreFunction)
				targetRestoreFunction(target);
		});
	},
	removeTargets: (targets, targetRestoreFunction = null) => {
		GWLog("Popins.removeTargets", "popins.js", 1);

		Popins.removeTargetsWithin(document, targets, targetRestoreFunction);
	},

	scrollElementIntoViewInPopFrame: (element) => {
		let popin = element.closest(".popin");
		popin.scrollView.scrollTop = element.getBoundingClientRect().top - popin.scrollView.getBoundingClientRect().top;
	},

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
			button.innerHTML = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="M325.8 193.8L263.6 256l62.2 62.2c4.7 4.7 4.7 12.3 0 17l-22.6 22.6c-4.7 4.7-12.3 4.7-17 0L224 295.6l-62.2 62.2c-4.7 4.7-12.3 4.7-17 0l-22.6-22.6c-4.7-4.7-4.7-12.3 0-17l62.2-62.2-62.2-62.2c-4.7-4.7-4.7-12.3 0-17l22.6-22.6c4.7-4.7 12.3-4.7 17 0l62.2 62.2 62.2-62.2c4.7-4.7 12.3-4.7 17 0l22.6 22.6c4.7 4.7 4.7 12.3 0 17zM448 80v352c0 26.5-21.5 48-48 48H48c-26.5 0-48-21.5-48-48V80c0-26.5 21.5-48 48-48h352c26.5 0 48 21.5 48 48zm-48 346V86c0-3.3-2.7-6-6-6H54c-3.3 0-6 2.7-6 6v340c0 3.3 2.7 6 6 6h340c3.3 0 6-2.7 6-6z"/></svg>`;
			button.title = "Close this popin";
			button.classList.add("close-button");
			button.buttonAction = (event) => {
				event.stopPropagation();

				let popin = event.target.closest(".popin");
				if (popin)
					Popins.removePopin(popin);
			};
			return button;
		},
		maximizeButton: () => {
			let button = Popins.titleBarComponents.genericButton();
			button.defaultHTML = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path fill="currentColor" d="M0 180V56c0-13.3 10.7-24 24-24h124c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12H64v84c0 6.6-5.4 12-12 12H12c-6.6 0-12-5.4-12-12zM288 44v40c0 6.6 5.4 12 12 12h84v84c0 6.6 5.4 12 12 12h40c6.6 0 12-5.4 12-12V56c0-13.3-10.7-24-24-24H300c-6.6 0-12 5.4-12 12zm148 276h-40c-6.6 0-12 5.4-12 12v84h-84c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h124c13.3 0 24-10.7 24-24V332c0-6.6-5.4-12-12-12zM160 468v-40c0-6.6-5.4-12-12-12H64v-84c0-6.6-5.4-12-12-12H12c-6.6 0-12 5.4-12 12v124c0 13.3 10.7 24 24 24h124c6.6 0 12-5.4 12-12z"></path></svg>`;
			button.alternateHTML = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path fill="currentColor" d="M436 192H312c-13.3 0-24-10.7-24-24V44c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v84h84c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12zm-276-24V44c0-6.6-5.4-12-12-12h-40c-6.6 0-12 5.4-12 12v84H12c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h124c13.3 0 24-10.7 24-24zm0 300V344c0-13.3-10.7-24-24-24H12c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h84v84c0 6.6 5.4 12 12 12h40c6.6 0 12-5.4 12-12zm192 0v-84h84c6.6 0 12-5.4 12-12v-40c0-6.6-5.4-12-12-12H312c-13.3 0-24 10.7-24 24v124c0 6.6 5.4 12 12 12h40c6.6 0 12-5.4 12-12z"></path></svg>`;
			button.defaultTitle = "Maximize this popin";
			button.alternateTitle = "Restore this popin to normal size";
			button.innerHTML = button.defaultHTML;
			button.title = button.defaultTitle;
			button.classList.add("maximize-button", "maximize");
			button.buttonAction = (event) => {
				event.stopPropagation();

				let popin = button.closest(".popin");
				if (popin) {
					Popins.zoomPopin(popin);
					popin.titleBar.querySelectorAll("button.maximize-button").forEach(titleBarButton => {
						titleBarButton.updateState();
					});
				}
			};
			button.updateState = () => {
				let popin = button.closest(".popin");
				if (!popin)
					return;

				button.innerHTML = Popins.popinIsMaximized(popin) ? button.alternateHTML : button.defaultHTML;
				button.title = Popins.popinIsMaximized(popin) ? button.alternateTitle : button.defaultTitle;

				button.swapClasses([ "maximize", "restore" ], (Popins.popinIsMaximized(popin) ? 1 : 0));
			};
			return button;
		},
		optionsButton: () => {
			let button = Popins.titleBarComponents.genericButton();
			button.innerHTML = `<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 20 20"><g transform="translate(10 10)"><path id="a" d="M1.5-10h-3l-1 6.5h5m0 7h-5l1 6.5h3"/><use transform="rotate(45)" xlink:href="#a"/><use transform="rotate(90)" xlink:href="#a"/><use transform="rotate(135)" xlink:href="#a"/></g><path d="M10 2.5a7.5 7.5 0 000 15 7.5 7.5 0 000-15v4a3.5 3.5 0 010 7 3.5 3.5 0 010-7"/></svg>`;
			return button;
		}
	},

	targetClicked: (event) => {
		event.preventDefault();

		let target = event.target.closest(".spawns-popin");

		if (target.classList.contains("popin-open")) {
			Popins.removePopin(target);
		} else {
			Popins.injectPopin(target);
		}

		document.activeElement.blur();
	},

	newPopin: () => {
		GWLog("Popins.newPopin", "popins.js", 2);

		let popin = document.createElement("div");
		popin.classList.add("popin", "popframe");
		popin.innerHTML = `<div class="popframe-scroll-view"><div class="popframe-content-view"></div></div>`;
		popin.scrollView = popin.querySelector(".popframe-scroll-view");
		popin.contentView = popin.querySelector(".popframe-content-view");
		popin.titleBarContents = [ ];
		return popin;
	},
	setPopFrameContent: (popin, contentHTML) => {
		popin.querySelector(".popframe-content-view").innerHTML = contentHTML;
		return (contentHTML > "");
	},
	injectPopin: (target) => {
		GWLog("Popins.injectPopin", "popins.js", 2);

		//  Remove existing popin, if any.
		if (target.popin)
			Popins.removePopin(target);

		//  Create the new popin.
		target.popin = Popins.newPopin();
		target.popFrame = target.popin;

		//  Give the popin a reference to the target.
		target.popin.spawningTarget = target;

		// Prepare the newly created popin for injection.
		if (!(target.popin = target.preparePopin(target.popin)))
			return;

		/*  If title bar contents are provided, create and inject the popin
			title bar, and set class `has-title-bar` on the popin.
			*/
		if (target.popin.titleBarContents.length > 0) {
			target.popin.classList.add("has-title-bar");

			target.popin.titleBar = document.createElement("div");
			target.popin.titleBar.classList.add("popframe-title-bar");
			target.popin.insertBefore(target.popin.titleBar, target.popin.firstElementChild);

			target.popin.titleBarContents.forEach(elementOrHTML => {
				if (typeof elementOrHTML == "string") {
					target.popin.titleBar.insertAdjacentHTML("beforeend", elementOrHTML);
				} else {
					target.popin.titleBar.appendChild(elementOrHTML);
				}
				let newlyAddedElement = target.popin.titleBar.lastElementChild;
				if (newlyAddedElement.buttonAction)
					newlyAddedElement.addActivateEvent(newlyAddedElement.buttonAction);
			});
		}

		//  Inject the popin.
		target.parentElement.insertBefore(target.popin, target.nextSibling);

		//  Mark target as having an open popin associated with it.
		target.classList.add("popin-open");

		GW.notificationCenter.fireEvent("Popins.popinDidInject", { popin: target.popin });
	},
	removePopin: (target) => {
		GWLog("Popins.removePopin", "popins.js", 2);

		if (target.popin)
			target.popin.remove();
		target.popin = null;
		target.popFrame = null;
		target.classList.toggle("popin-open", false);
	}
};

GW.notificationCenter.fireEvent("Popins.didLoad");

/******************/
/*	Initialization.
	*/
doWhenPageLoaded(() => {
	Popins.setup();
});
