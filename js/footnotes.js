/*	Popup/floating footnotes to avoid readers needing to scroll to the end of
	the page to see any footnotes; see
	http://ignorethecode.net/blog/2010/04/20/footnotes/ for details.
Original author:  Lukas Mathis (2010-04-20)
License: public domain ("And some people have asked me about a license for this piece of code. I think it’s far too short to get its own license, so I’m relinquishing any copyright claims. Consider the code to be public domain. No attribution is necessary.")
	*/
Footnotes = {
	/**********/
	/*	Config.
		*/
    stylesID: "footnotes-styles",

	targets: {
		targetElementsSelector: ".footnote-ref",
		excludedElementsSelector: null,
		excludedContainerElementsSelector: null,
	},

	/******************/
	/*	Implementation.
		*/
	unbind: () => {
		GWLog("Footnotes.unbind", "footnotes.js", 1);

		Popups.removeTargets(Footnotes.targets);

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

        //  Run cleanup.
		Footnotes.cleanup();

        if (Popups.isMobile()) {
            GWLog("Mobile client detected. Exiting.", "footnotes.js", 1);
            return;
        } else {
            GWLog("Non-mobile client detected. Setting up.", "footnotes.js", 1);
        }

        //  Inject styles.
        document.querySelector("head").insertAdjacentHTML("beforeend", Footnotes.stylesHTML);

		//  Set up targets.
		Popups.addTargets(Footnotes.targets, Footnotes.preparePopup);

		GW.notificationCenter.fireEvent("Footnotes.setupComplete");
	},
	fillPopup: (popup, target) => {
		GWLog("Footnotes.fillPopup", "footnotes.js", 2);

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
	preparePopup: (popup, target) => {
		GWLog("Footnotes.preparePopup", "footnotes.js", 2);

		popup.id = "footnotediv";

		//	Inject the contents of the footnote into the popup.
		if (Footnotes.fillPopup(popup, target) == false)
			return false;

		return true;
	}
};

/********************/
/*	Essential styles.
	*/
Footnotes.stylesHTML = `<style id='${Popups.stylesID}'>
</style>`;

/******************/
/*	Initialization.
	*/
doWhenPageLoaded(() => {
	GW.notificationCenter.fireEvent("Footnotes.loaded");

	if (window.Popups)
		Footnotes.setup();
	else
		GW.notificationCenter.addHandlerForEvent("Popups.setupComplete", () => {
			Footnotes.setup();
		}, { once: true });
});
