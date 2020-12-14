/*	Popup/floating footnotes to avoid readers needing to scroll to the end of
	the page to see any footnotes; see
	http://ignorethecode.net/blog/2010/04/20/footnotes/ for details.
Original author:  Lukas Mathis (2010-04-20)
License: public domain ("And some people have asked me about a license for this piece of code. I think it’s far too short to get its own license, so I’m relinquishing any copyright claims. Consider the code to be public domain. No attribution is necessary.")
	*/
Footnotes = {
	contentContainerSelector: "#markdownBody",

    targetElementsSelector: ".footnote-ref",
    excludedElementsSelector: null,
    excludedContainerElementsSelector: null,

	minFootnoteWidth: 520,

    popupTriggerDelay: 200,
    popupFadeoutDelay: 50,
    popupFadeoutDuration: 250,

	popupFadeTimer: false,
	popupDespawnTimer: false,
	popupSpawnTimer: false,
	footnotePopup: null,

	isMobile: () => {
		/*  We consider a client to be mobile if one of two conditions obtain:
		    1. JavaScript detects touch capability, AND viewport is narrow; or,
		    2. CSS does NOT detect hover capability.
		    */
		return (   (   ('ontouchstart' in document.documentElement)
					&& GW.mediaQueries.mobileWidth.matches)
				|| !GW.mediaQueries.hoverAvailable.matches);
	},
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

        if (Footnotes.isMobile()) {
            GWLog("Mobile client detected. Exiting.", "footnotes.js", 1);
            return;
        } else {
            GWLog("Non-mobile client detected. Setting up.", "footnotes.js", 1);
        }

		//	Get all targets.
		document.querySelectorAll(Footnotes.targetElementsSelector).forEach(target => {
			if (   target.closest(Footnotes.excludedElementsSelector) == target
				|| target.closest(Footnotes.excludedContainerElementsSelector) != null)
				return;

			//	Bind mouseenter/mouseleave events.
			target.addEventListener("mouseenter", Footnotes.targetMouseenter);
			target.addEventListener("mouseleave", Footnotes.targetMouseleave);
		});

		GW.notificationCenter.fireEvent("Footnotes.setupComplete");
	},
	//	The mouseenter event.
	targetMouseenter: (event) => {
		GWLog("Footnotes.targetMouseenter", "footnotes.js", 2);

        //  Get the target.
        let target = event.target.closest(Footnotes.targetElementsSelector);

		//	Stop the countdown to un-pop the popup.
		Footnotes.clearPopupTimers();

		Footnotes.popupSpawnTimer = setTimeout(() => {
			GWLog("Footnotes.popupSpawnTimer fired", "footnotes.js", 2);

			if (!target.hash) return;
			let targetFootnoteId = target.hash.substr(1);

            let targetViewportRect = target.getBoundingClientRect();
			let bodyAbsoluteRect = document.body.getBoundingClientRect();
			var citationPosition = {
				left: (targetViewportRect.left - bodyAbsoluteRect.left),
				top: (targetViewportRect.top - bodyAbsoluteRect.top)
			};

			//  Remove existing popup, if any.
			Footnotes.despawnPopup();
			Footnotes.popup = null;

            //  Create the popup.
			Footnotes.popup = document.createElement('div');
			Footnotes.popup.id = "footnotediv";

			//	Inject the contents of the footnote into the popup, if needed.
			if (Footnotes.popup.dataset.footnoteReference != targetFootnoteId) {
				var targetFootnote = document.querySelector("#" + targetFootnoteId);
				Footnotes.popup.innerHTML = '<div>' + targetFootnote.innerHTML + '</div>';
				Footnotes.popup.dataset.footnoteReference = targetFootnoteId;
			}

			//	Inject the popup into the page.
			document.querySelector(Footnotes.contentContainerSelector).appendChild(Footnotes.popup);

			//	Add event listeners.
            Footnotes.popup.addEventListener("mouseup", Footnotes.popupMouseup);
			Footnotes.popup.addEventListener("mouseenter", Footnotes.popupMouseenter);
			Footnotes.popup.addEventListener("mouseleave", Footnotes.popupMouseleave);

			/*	How much "breathing room" to give the footnote reference (i.e.,
				offset of the footnote popup).
				*/
			var footnotePopupBreathingRoom = {
				x:	(Math.round(targetViewportRect.width) * 1.5),
				y:	Math.round(targetViewportRect.height) + (Math.round(targetViewportRect.width) * 0.5)
			};

			/*	Set the horizontal position first; this causes the popup to be laid
				out, and the layout engine calculates the height for us.
				*/
			var footnotePopupLeft = citationPosition.left + footnotePopupBreathingRoom.x;
			if (footnotePopupLeft + Footnotes.minFootnoteWidth > window.innerWidth)
				footnotePopupLeft = window.innerWidth - Footnotes.minFootnoteWidth;
			Footnotes.popup.style.left = footnotePopupLeft + "px";
			//	Correct for various positioning aberrations.
			if (Footnotes.popup.getBoundingClientRect().right > window.innerWidth)
				Footnotes.popup.style.maxWidth = (Footnotes.popup.clientWidth - (Footnotes.popup.getBoundingClientRect().right - window.innerWidth) - parseInt(getComputedStyle(Footnotes.popup.firstElementChild).paddingRight)) + "px";
			else if (citationPosition.left + footnotePopupBreathingRoom.x + Footnotes.popup.clientWidth < window.innerWidth)
				Footnotes.popup.style.left = (citationPosition.left + footnotePopupBreathingRoom.x) + "px";
			else if (citationPosition.left - (footnotePopupBreathingRoom.x + Footnotes.popup.clientWidth) > Footnotes.popup.getBoundingClientRect().left)
				Footnotes.popup.style.left = (citationPosition.left - footnotePopupBreathingRoom.x - Footnotes.popup.clientWidth) + "px";

			//	Now we know how tall the popup is...
			var provisionalFootnotePopupHeight = Footnotes.popup.clientHeight;

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
			Footnotes.popup.style.top = footnotePopupTop + "px";
		}, Footnotes.popupTriggerDelay);
	},
	//	The mouseleave event.
	targetMouseleave: (event) => {
		GWLog("Footnotes.targetMouseleave", "footnotes.js", 2);

		Footnotes.clearPopupTimers();

		if (!Footnotes.popup) return;

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
		Footnotes.popup.classList.remove("fading");
	},
    popupMouseup: (event) => {
		GWLog("Footnotes.popupMouseup", "footnotes.js", 2);

		event.stopPropagation();

		Footnotes.clearPopupTimers();
		Footnotes.despawnPopup();
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

			Footnotes.setPopupDespawnTimer();
        }, Footnotes.popupFadeoutDelay);
    },
    setPopupDespawnTimer: () => {
		GWLog("Footnotes.setPopupDespawnTimer", "footnotes.js", 2);

		Footnotes.popup.classList.add("fading");
		Footnotes.popupDespawnTimer = setTimeout(() => {
			GWLog("Footnotes.popupDespawnTimer fired", "footnotes.js", 2);

			Footnotes.despawnPopup();
		}, Footnotes.popupFadeoutDuration);
    },
    despawnPopup: () => {
		GWLog("Footnotes.despawnPopup", "footnotes.js", 2);

		if (Footnotes.popup == null)
			return;

	        Footnotes.popup.classList.remove("fading");
        Footnotes.popup.remove();
        document.activeElement.blur();
    }
};

doWhenPageLoaded(() => {
	GW.notificationCenter.fireEvent("Footnotes.loaded");

	Footnotes.setup();
});
