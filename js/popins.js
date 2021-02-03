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

	targetClicked: (event) => {
		event.preventDefault();

		if (event.target.classList.contains("popin-open")) {
			Popins.removePopin(event.target);
		} else {
			Popins.injectPopin(event.target);
		}

		document.activeElement.blur();
	},

	newPopin: () => {
		GWLog("Popins.newPopin", "popins.js", 2);

		let popin = document.createElement("div");
		popin.classList.add("popin", "popframe");
		popin.innerHTML = `<div class="popin-scroll-view"><div class="popin-content-view"></div></div>`;
		popin.scrollView = popin.querySelector(".popin-scroll-view");
		popin.contentView = popin.querySelector(".popin-content-view");
		popin.titleBarContents = [ ];
		return popin;
	},
	setPopFrameContent: (popin, contentHTML) => {
		popin.querySelector(".popin-content-view").innerHTML = contentHTML;
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

		/*  If title bar contents are provided, create and inject the popup
			title bar, and set class `has-title-bar` on the popup.
			*/
		//  TODO: this

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
