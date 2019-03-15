/*	Popup/floating footnotes to avoid readers needing to scroll to the end of
	the page to see any footnotes; see
	http://ignorethecode.net/blog/2010/04/20/footnotes/ for details.
	*/
Footnotes = {
	contentContainerSelector: "#markdownBody",
	minFootnoteWidth: 520,
	footnotefadetimeout: false,
	footnotekilltimeout: false,
	footnotePopup: null,
	setup: function() {
		//	Get all footnote links.
		document.querySelectorAll(".footnote-ref").forEach(fnref => {
			//	Unbind existing mouseover/mouseout events, if any.
			fnref.removeEventListener("mouseover", Footnotes.footnoteover);
			fnref.removeEventListener("mouseout", Footnotes.footnoteoout);

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

		var targetFootnoteId = event.target.hash.substr(1);
		var citationPosition = { left: event.target.offsetLeft, top: event.target.offsetTop };
		var citationAbsoluteRect = event.target.getBoundingClientRect();

		//	Get, or create, the footnote popup.
		Footnotes.footnotePopup = document.querySelector("#footnotediv");
		if (!Footnotes.footnotePopup || Footnotes.footnotePopup.dataset.footnoteReference != targetFootnoteId) {
			Footnotes.footnotePopup = document.createElement('div');
			Footnotes.footnotePopup.id = "footnotediv";
			Footnotes.footnotePopup.dataset.footnoteReference = targetFootnoteId;
			Footnotes.footnotePopup.addEventListener("mouseover", Footnotes.divover);
			Footnotes.footnotePopup.addEventListener("mouseout", Footnotes.footnoteoout);
		}
		Footnotes.footnotePopup.classList.remove("fading");

		//	Inject the contents of the footnote into the popup.
		var targetFootnote = document.querySelector("#" + targetFootnoteId);
		Footnotes.footnotePopup.innerHTML = '<div>' + targetFootnote.innerHTML + '</div>';

		//	Inject the popup into the page.
		document.querySelector(Footnotes.contentContainerSelector).appendChild(Footnotes.footnotePopup);

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
		if (footnotePopupLeft + Footnotes.minFootnoteWidth > window.innerWidth + window.scrollX)
			footnotePopupLeft = window.innerWidth - Footnotes.minFootnoteWidth + window.scrollX;
		Footnotes.footnotePopup.style.left = footnotePopupLeft + "px";

		//	Now we know how tall the popup is...
		var provisionalFootnotePopupRect = Footnotes.footnotePopup.getBoundingClientRect();

		//	Determining vertical position is full of edge cases.
		var footnotePopupTop = citationPosition.top + footnotePopupBreathingRoom.y;
		if (footnotePopupTop + provisionalFootnotePopupRect.height > window.innerHeight + window.scrollY) {
			footnotePopupTop -= (provisionalFootnotePopupRect.height + footnotePopupBreathingRoom.y);
		}
		if (top + provisionalFootnotePopupRect.height > window.innerHeight + window.scrollY ||
			provisionalFootnotePopupRect.height == window.innerHeight ||
			footnotePopupTop < window.scrollY) {
			footnotePopupTop = window.scrollY;
		}
		if (footnotePopupTop + provisionalFootnotePopupRect.height + 120 < citationPosition.top) {
			footnotePopupTop = citationPosition.top - provisionalFootnotePopupRect.height;
		} else if (top > citationPosition.top) {
			footnotePopupTop -= 90;
		}
		if (footnotePopupTop < 0) {
			footnotePopupTop = 0;
		}
		Footnotes.footnotePopup.style.top = footnotePopupTop + "px";
	},
	//	The mouseout event.
	footnoteoout: (event) => {
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
		Footnotes.footnotePopup.classList.remove("fading");
	}
}

if (document.readyState == "complete") {
	Footnotes.setup();
} else {
	window.addEventListener("load", Footnotes.setup);
}
