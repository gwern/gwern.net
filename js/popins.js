/*	Popup/floating footnotes to avoid readers needing to scroll to the end of
	the page to see any footnotes; see
	http://ignorethecode.net/blog/2010/04/20/footnotes/ for details.
Original author:  Lukas Mathis (2010-04-20)
License: public domain ("And some people have asked me about a license for this piece of code. I think it’s far too short to get its own license, so I’m relinquishing any copyright claims. Consider the code to be public domain. No attribution is necessary.")
	*/
Popins = {
	/**********/
	/*	Config.
		*/
    stylesID: "popins-styles",

	/******************/
	/*	Implementation.
		*/
	cleanup: () => {
		GWLog("Popins.cleanup", "popins.js", 1);

        //  Remove injected styles.
        document.querySelectorAll(`#${Popins.stylesID}`).forEach(element => element.remove());

		//  Remove all toggles and remnant popins.
		//  TODO: this
	},
	setup: () => {
		GWLog("Popins.setup", "popins.js", 1);

        //  Run cleanup.
        Popins.cleanup();

        //  Inject styles.
        document.querySelector("head").insertAdjacentHTML("beforeend", Popins.stylesHTML);

		GW.notificationCenter.fireEvent("Popins.setupDidComplete");
	},
	addTargetsWithin: (contentContainer, targetSelectors, prepareFunction, targetPrepareFunction = null) => {
		if (typeof contentContainer == "string")
			contentContainer = document.querySelector(contentContainer);

		if (contentContainer == null)
			return;

		//	Get all targets.
		contentContainer.querySelectorAll(targetSelectors.targetElementsSelector).forEach(target => {
			if (   target.closest(targetSelectors.excludedElementsSelector) == target
				|| target.closest(targetSelectors.excludedContainerElementsSelector) != null)
				return;

			//  Run any custom processing.
			if (targetPrepareFunction)
				targetPrepareFunction(target);

			//  Inject the popin.
			Popins.injectPopin(target, prepareFunction);
		});
	},
	addTargets: (targetSelectors, prepareFunction, targetPrepareFunction = null) => {
		GWLog("Popins.addTargets", "popins.js", 1);

		Popins.addTargetsWithin(document, targetSelectors, prepareFunction, targetPrepareFunction);
	},
	removeTargetsWithin: (contentContainer, targetSelectors, targetRestoreFunction = null) => {
		if (typeof contentContainer == "string")
			contentContainer = document.querySelector(contentContainer);

		if (contentContainer == null)
			return;

		contentContainer.querySelectorAll(targetSelectors.targetElementsSelector).forEach(target => {
			if (   target.closest(targetSelectors.excludedElementsSelector) == target
				|| target.closest(targetSelectors.excludedContainerElementsSelector) != null)
				return;

			//  Remove the popin.
			Popins.removePopin(target);

			//  Run any custom processing.
			if (targetRestoreFunction)
				targetRestoreFunction(target);
		});
	},
	removeTargets: (targetSelectors, targetRestoreFunction = null) => {
		GWLog("Popins.removeTargets", "popins.js", 1);

		Popins.removeTargetsWithin(document, targetSelectors, targetRestoreFunction);
	},

	injectPopin: (target, prepareFunction) => {
		GWLog("Popins.injectPopin", "popins.js", 2);

		//  Create the new popin.
		target.popin = document.createElement("div");
		target.popin.classList.add("popindiv");

		//  Give the popin a reference to the target.
		target.popin.popinTarget = target;

		// Prepare the newly created popin for injection.
		if (prepareFunction(target.popin, target) == false)
			return;

		//  Inject the popin.
		target.onclick = (event) => {
			event.preventDefault();
			if (target.popinPreShowFunction)
				target.popinPreShowFunction(target.popin);
			target.classList.toggle("popin-open");
			document.activeElement.blur();
		};
		target.parentElement.insertBefore(target.popin, target.nextSibling);
		target.classList.toggle("spawns-popin", true);

		GW.notificationCenter.fireEvent("Popins.popinDidInject", { popin: target.popin });
	},
	removePopin: (target) => {
		GWLog("Popins.removePopin", "popins.js", 2);

		target.classList.toggle("popin-open", false);
		target.classList.toggle("spawns-popin", false);
		if (target.popin)
			target.popin.remove();
		target.popin = null;
	}
};

/********************/
/*	Essential styles.
	*/
Popins.stylesHTML = `<style id='${Popins.stylesID}'>

.popindiv {
    display: none;

    float: left;
    border-width: 3px;
    border-style: double;
    margin: 1em 0;
    padding: 0.625em 1em 0.75em 1em;
    font-size: 0.9em;
    width: 100%;
    box-sizing: border-box;
}
.spawns-popin.popin-open + .popindiv {
    display: block;
}
</style>`;

/******************/
/*	Initialization.
	*/
doWhenPageLoaded(() => {
	GW.notificationCenter.fireEvent("Popins.didLoad");

	Popins.setup();
});
