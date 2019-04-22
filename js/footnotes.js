/*	Popup/floating footnotes to avoid readers needing to scroll to the end of
	the page to see any footnotes; see
	http://ignorethecode.net/blog/2010/04/20/footnotes/ for details.
	*/
Footnotes = {
	contentContainerSelector: "#markdownBody",
	minFootnoteWidth: 520,
	footnotefadetimeout: false,
	footnotekilltimeout: false,
	footnotepopuptimeout: false,
	footnotePopup: null,
	unbind: function() {
		document.querySelectorAll(".footnote-ref").forEach(fnref => {
			//	Unbind existing mouseover/mouseout events, if any.
			fnref.removeEventListener("mouseover", Footnotes.footnoteover);
			fnref.removeEventListener("mouseout", Footnotes.footnoteoout);
		});
	},
	setup: function() {
		Footnotes.unbind();
		//	Get all footnote links.
		document.querySelectorAll(".footnote-ref").forEach(fnref => {
			//	Bind mousemover/mouseout events.
			fnref.addEventListener("mouseover", Footnotes.footnoteover);
			fnref.addEventListener("mouseout", Footnotes.footnoteoout);
		});
	},
	//	The mouseover event.
	footnoteover: (event) => {
		//	Stop the countdown to un-pop the popup.
		clearTimeout(Footnotes.footnotefadetimeout);
		clearTimeout(Footnotes.footnotekilltimeout);
		clearTimeout(Footnotes.footnotepopuptimeout);

		Footnotes.footnotepopuptimeout = setTimeout(() => {
			//	Get the citation and its target footnote ID.
			var citationAbsoluteRect = event.target.getBoundingClientRect();
			let bodyAbsoluteRect = document.body.getBoundingClientRect();
			var citationPosition = {
				left: (citationAbsoluteRect.left - bodyAbsoluteRect.left),
				top: (citationAbsoluteRect.top - bodyAbsoluteRect.top)
			};

			if (!event.target.hash) return;
			var targetFootnoteId = event.target.hash.substr(1);

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
			Footnotes.footnotePopup.addEventListener("mouseover", Footnotes.divover);
			Footnotes.footnotePopup.addEventListener("mouseout", Footnotes.footnoteoout);

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
		}, 50);
	},
	//	The mouseout event.
	footnoteoout: (event) => {
		clearTimeout(Footnotes.footnotefadetimeout);
		clearTimeout(Footnotes.footnotekilltimeout);
		clearTimeout(Footnotes.footnotepopuptimeout);

		if (!Footnotes.footnotePopup) return;

		Footnotes.footnotefadetimeout = setTimeout(() => {
			Footnotes.footnotePopup.classList.add("fading");
			Footnotes.footnotekilltimeout = setTimeout(() => {
				Footnotes.footnotePopup.classList.remove("fading");
				Footnotes.footnotePopup.remove();
			}, 750);
		}, 100);
	},
	//	The "user moved mouse back into popup" mouseover event.
	divover: (event) => {
		clearTimeout(Footnotes.footnotefadetimeout);
		clearTimeout(Footnotes.footnotekilltimeout);
		clearTimeout(Footnotes.footnotepopuptimeout);
		Footnotes.footnotePopup.classList.remove("fading");
	}
}

if (document.readyState == "complete") {
	Footnotes.setup();
} else {
	window.addEventListener("load", Footnotes.setup);
}
