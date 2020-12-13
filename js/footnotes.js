/*	Popup/floating footnotes to avoid readers needing to scroll to the end of
	the page to see any footnotes; see
	http://ignorethecode.net/blog/2010/04/20/footnotes/ for details.
Original author:  Lukas Mathis (2010-04-20)
License: public domain ("And some people have asked me about a license for this piece of code. I think it’s far too short to get its own license, so I’m relinquishing any copyright claims. Consider the code to be public domain. No attribution is necessary.")
	*/
Footnotes = {
	contentContainerSelector: "#markdownBody",
	minFootnoteWidth: 520,

    popupTriggerDelay: 200,
    popupFadeoutDelay: 50,
    popupFadeoutDuration: 250,

	popupFadeTimer: false,
	popupDespawnTimer: false,
	popupSpawnTimer: false,
	footnotePopup: null,

	unbind: () => {
		GWLog("Footnotes.unbind", "footnotes.js", 1);

		document.querySelectorAll(".footnote-ref").forEach(fnref => {
			//	Unbind existing mouseenter/mouseleave events, if any.
			fnref.removeEventListener("mouseenter", Footnotes.targetMouseenter);
			fnref.removeEventListener("mouseleave", Footnotes.targetMouseleave);
		});

		GW.notificationCenter.fireEvent("Footnotes.eventsUnbound");
	},
	setup: () => {
		GWLog("Footnotes.setup", "footnotes.js", 1);

		Footnotes.unbind();
		//	Get all footnote links.
		document.querySelectorAll(".footnote-ref").forEach(fnref => {
			//	Bind mousemover/mouseleave events.
			fnref.addEventListener("mouseenter", Footnotes.targetMouseenter);
			fnref.addEventListener("mouseleave", Footnotes.targetMouseleave);
		});

		GW.notificationCenter.fireEvent("Footnotes.setupComplete");
	},
	//	The mouseenter event.
	targetMouseenter: (event) => {
		GWLog("Footnotes.targetMouseenter", "footnotes.js", 2);

        //  Get the target.
        let target = event.target;

		//	Stop the countdown to un-pop the popup.
		Footnotes.clearPopupTimers();

		Footnotes.popupSpawnTimer = setTimeout(() => {
			GWLog("Footnotes.popupSpawnTimer fired", "footnotes.js", 2);

			//	Get the citation and its target footnote ID.
			var citationAbsoluteRect = target.getBoundingClientRect();
			let bodyAbsoluteRect = document.body.getBoundingClientRect();
			var citationPosition = {
				left: (citationAbsoluteRect.left - bodyAbsoluteRect.left),
				top: (citationAbsoluteRect.top - bodyAbsoluteRect.top)
			};

			if (!target.hash) return;
			let targetFootnoteId = target.hash.substr(1);

			//	Get, or create, the footnote popup.
			Footnotes.footnotePopup = document.querySelector("#footnotediv");
			if (Footnotes.footnotePopup) {
				Footnotes.footnotePopup.classList.remove("fading");
				Footnotes.footnotePopup.remove();
			} else {
				Footnotes.footnotePopup = document.createElement('div');
				Footnotes.footnotePopup.id = "footnotediv";
			}

			//	Inject the contents of the footnote into the popup, if needed.
			if (Footnotes.footnotePopup.dataset.footnoteReference != targetFootnoteId) {
				var targetFootnote = document.querySelector("#" + targetFootnoteId);
				Footnotes.footnotePopup.innerHTML = '<div>' + targetFootnote.innerHTML + '</div>';
				Footnotes.footnotePopup.dataset.footnoteReference = targetFootnoteId;
			}

			//	Inject the popup into the page.
			document.querySelector(Footnotes.contentContainerSelector).appendChild(Footnotes.footnotePopup);

			//	Add event listeners.
			Footnotes.footnotePopup.addEventListener("mouseenter", Footnotes.popupMouseenter);
			Footnotes.footnotePopup.addEventListener("mouseleave", Footnotes.popupMouseleave);

			/*	How much "breathing room" to give the footnote reference (i.e.,
				offset of the footnote popup).
				*/
			var footnotePopupBreathingRoom = {
				x:	(Math.round(citationAbsoluteRect.width) * 1.5),
				y:	Math.round(citationAbsoluteRect.height) + (Math.round(citationAbsoluteRect.width) * 0.5)
			};

			/*	Set the horizontal position first; this causes the popup to be laid
				out, and the layout engine calculates the height for us.
				*/
			var footnotePopupLeft = citationPosition.left + footnotePopupBreathingRoom.x;
			if (footnotePopupLeft + Footnotes.minFootnoteWidth > window.innerWidth)
				footnotePopupLeft = window.innerWidth - Footnotes.minFootnoteWidth;
			Footnotes.footnotePopup.style.left = footnotePopupLeft + "px";
			//	Correct for various positioning aberrations.
			if (Footnotes.footnotePopup.getBoundingClientRect().right > window.innerWidth)
				Footnotes.footnotePopup.style.maxWidth = (Footnotes.footnotePopup.clientWidth - (Footnotes.footnotePopup.getBoundingClientRect().right - window.innerWidth) - parseInt(getComputedStyle(Footnotes.footnotePopup.firstElementChild).paddingRight)) + "px";
			else if (citationPosition.left + footnotePopupBreathingRoom.x + Footnotes.footnotePopup.clientWidth < window.innerWidth)
				Footnotes.footnotePopup.style.left = (citationPosition.left + footnotePopupBreathingRoom.x) + "px";
			else if (citationPosition.left - (footnotePopupBreathingRoom.x + Footnotes.footnotePopup.clientWidth) > Footnotes.footnotePopup.getBoundingClientRect().left)
				Footnotes.footnotePopup.style.left = (citationPosition.left - footnotePopupBreathingRoom.x - Footnotes.footnotePopup.clientWidth) + "px";

			//	Now we know how tall the popup is...
			var provisionalFootnotePopupHeight = Footnotes.footnotePopup.clientHeight;

			//	Determining vertical position is full of edge cases.
			var footnotePopupTop = citationPosition.top + footnotePopupBreathingRoom.y;
			if (footnotePopupTop + provisionalFootnotePopupHeight > window.innerHeight + window.scrollY) {
				footnotePopupTop -= (provisionalFootnotePopupHeight + footnotePopupBreathingRoom.y);
			}
			if (top + provisionalFootnotePopupHeight > window.innerHeight + window.scrollY ||
				provisionalFootnotePopupHeight == window.innerHeight ||
				footnotePopupTop < window.scrollY) {
				footnotePopupTop = window.scrollY;
			}
			if (footnotePopupTop + provisionalFootnotePopupHeight + 120 < citationPosition.top) {
				footnotePopupTop = citationPosition.top - provisionalFootnotePopupHeight;
			} else if (top > citationPosition.top) {
				footnotePopupTop -= 90;
			}
			if (footnotePopupTop < 0) {
				footnotePopupTop = 0;
			}
			Footnotes.footnotePopup.style.top = footnotePopupTop + "px";
		}, Footnotes.popupTriggerDelay);
	},
	//	The mouseleave event.
	targetMouseleave: (event) => {
		GWLog("Footnotes.targetMouseleave", "footnotes.js", 2);

		Footnotes.clearPopupTimers();

		if (!Footnotes.footnotePopup) return;

		Footnotes.setPopupFadeTimer();
	},
    //	The “user moved mouse out of popup” mouseleave event.
	popupMouseleave: (event) => {
		GWLog("Footnotes.popupMouseleave", "footnotes.js", 2);

		Footnotes.clearPopupTimers();
		
		Footnotes.setPopupFadeTimer();
	},
	//	The “user moved mouse back into popup” mouseenter event.
	popupMouseenter: (event) => {
		GWLog("Footnotes.popupMouseenter", "footnotes.js", 2);

		Footnotes.clearPopupTimers();
		Footnotes.footnotePopup.classList.remove("fading");
	},
    clearPopupTimers: () => {
	    GWLog("Footnotes.clearPopupTimers", "footnotes.js", 2);

        clearTimeout(Footnotes.popupFadeTimer);
        clearTimeout(Footnotes.popupDespawnTimer);
        clearTimeout(Footnotes.popupSpawnTimer);
    },
    setPopupFadeTimer: () => {
		GWLog("Footnotes.setPopupFadeTimer", "footnotes.js", 2);

        Footnotes.popupFadeTimer = setTimeout(() => {
			GWLog("Footnotes.popupFadeTimer fired", "footnotes.js", 2);

			Footnotes.footnotePopup.classList.add("fading");
			Footnotes.setPopupDespawnTimer();
        }, Footnotes.popupFadeoutDelay);
    },
    setPopupDespawnTimer: () => {
		GWLog("Footnotes.setPopupDespawnTimer", "footnotes.js", 2);

		Footnotes.popupDespawnTimer = setTimeout(() => {
			GWLog("Footnotes.popupDespawnTimer fired", "footnotes.js", 2);

	        Footnotes.footnotePopup.classList.remove("fading");
			Footnotes.despawnPopup();
		}, Footnotes.popupFadeoutDuration);
    },
    despawnPopup: () => {
		GWLog("Footnotes.despawnPopup", "footnotes.js", 2);

		if (Footnotes.footnotePopup == null)
			return;

        Footnotes.footnotePopup.remove();
        document.activeElement.blur();
        Footnotes.footnotePopup.innerHTML = "";
//         Extracts.popupContainer.classList.remove("popup-visible");
    }
}

doWhenPageLoaded(() => {
	GW.notificationCenter.fireEvent("Footnotes.loaded");

	Footnotes.setup();
});
