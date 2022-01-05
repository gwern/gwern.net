/* sidenotes.js: standalone JS library for parsing HTML documents with Pandoc-style footnotes and dynamically repositioning them into the left/right margins, when browser windows are wide enough.
Sidenotes (see https://www.gwern.net/Sidenotes ) are superior to footnotes where possible because they enable the reader to immediately look at them without requiring user action to 'go to' or 'pop up' the footnotes; even floating footnotes require effort by the reader.
sidenotes.js is inspired by the Tufte-CSS sidenotes (https://edwardtufte.github.io/tufte-css/#sidenotes), but where Tufte-CSS uses static footnotes inlined into the body of the page (requiring modifications to Pandoc's compilation), which doesn't always work well for particularly long or frequent sidenotes, sidenotes.js will rearrange sidenotes to fit as best as possible, and will respond to window changes.
Particularly long sidenotes are also partially 'collapsed'.
Styling (especially for oversized-sidenotes which must scroll) is done in /static/css/default.css "SIDENOTES" section.

Author: Said Achmiz
2019-03-11
license: MIT (derivative of footnotes.js, which is PD)
*/

Sidenotes = {
	/*  The `sidenoteSpacing` constant defines the minimum vertical space that
		is permitted between adjacent sidenotes; any less, and they are
		considered to be overlapping.
		*/
	sidenoteSpacing: 60.0,

	/*	This includes the border width.
		*/
	sidenotePadding: 13.0,

	/*	Maximum height of a sidenote.
		*/
	sidenoteMaxHeight: 600.0,

	/*	Elements which occupy (partially or fully) the sidenote columns, and
		which can thus collide with sidenotes.
		*/
	potentiallyOverlappingElementsSelector: ".full-width img, .full-width video, .full-width table, .full-width pre, .marginnote",

	/*  Media query objects (for checking and attaching listeners).
		*/
	mediaQueries: {
		viewportWidthBreakpoint: matchMedia("(max-width: 1760px)")
	},

	/*****************/
	/* Infrastructure.
		*/
	sidenoteDivs: null,
	citations: null,

	sidenoteColumnLeft: null,
	sidenoteColumnRight: null,

	noteNumberFromHash: () => {
		if (location.hash.match(/#[sf]n[0-9]/))
			return location.hash.substr(3);
		else if (location.hash.match(/#fnref[0-9]/))
			return location.hash.substr(6);
		else
			return "";
	},

	/*	Bind event listeners to highlight citation when sidenote is hovered over.
		*/
	bindSidenoteMouseEvents: () => {
		GWLog("Sidenotes.bindSidenoteMouseEvents", "sidenotes.js", 1);

		for (var i = 0; i < Sidenotes.citations.length; i++) {
			let citation = Sidenotes.citations[i];
			let sidenote = Sidenotes.sidenoteDivs[i];

			sidenote.addEventListener("mouseenter", sidenote.sidenoteover = (event) => {
				citation.classList.toggle("highlighted", true);
			});
			sidenote.addEventListener("mouseleave", sidenote.sidenoteout = (event) => {
				citation.classList.toggle("highlighted", false);
			});
		}
	},

	/*  The “target counterpart” is the element associated with the target, i.e.:
		if the URL hash targets a footnote reference, its counterpart is the
		sidenote for that citation; and vice-versa, if the hash targets a sidenote,
		its counterpart is the in-text citation. We want a target counterpart to be
		highlighted along with the target itself; therefore we apply a special
		‘targeted’ class to the target counterpart.
		*/
	updateTargetCounterpart: () => {
		GWLog("Sidenotes.updateTargetCounterpart", "sidenotes.js", 1);

		/*  Clear existing targeting.
			*/
		document.querySelectorAll(".targeted").forEach(element => {
			element.classList.remove("targeted");
		});

		/*  Identify new target counterpart, if any.
			*/
		var counterpart;
		if (location.hash.match(/#sn[0-9]/)) {
			counterpart = document.querySelector("#fnref" + Sidenotes.noteNumberFromHash());
		} else if (location.hash.match(/#fnref[0-9]/) && Sidenotes.mediaQueries.viewportWidthBreakpoint.matches == false) {
			counterpart = document.querySelector("#sn" + Sidenotes.noteNumberFromHash());
		}
		/*  If a target counterpart exists, mark it as such.
			*/
		if (counterpart)
			counterpart.classList.toggle("targeted", true);
	},

	/*  Move sidenotes within currently-collapsed collapse blocks to the hidden
		sidenote storage container (#hidden-sidenote-storage). Conversely, move
		sidenotes within currently-expanded collapse blocks from the hidden sidenote
		storage container to the appropriate sidenote column.
		*/
	updateSidenotesInCollapseBlocks: () => {
		GWLog("Sidenotes.updateSidenotesInCollapseBlocks", "sidenotes.js", 1);

		for (var i = 0; i < Sidenotes.citations.length; i++) {
			let citation = Sidenotes.citations[i];
			let sidenote = Sidenotes.sidenoteDivs[i];

			sidenote.classList.toggle("hidden", isWithinCollapsedBlock(citation));
		}
	},

	getNextVisibleSidenote: (sidenote) => {
		var nextSidenoteNumber;
		for (nextSidenoteNumber = sidenote.id.substr(2) + 2;
			 (nextSidenoteNumber <= Sidenotes.citations.length && Sidenotes.sidenoteDivs[nextSidenoteNumber - 1].classList.contains("hidden"));
			 nextSidenoteNumber += 2)
			 ;
		return nextSidenoteNumber <= Sidenotes.citations.length
			   ? Sidenotes.sidenoteDivs[nextSidenoteNumber - 1]
			   : null;
	},

	/*  This function actually calculates and sets the positions of all sidenotes.
		*/
	updateSidenotePositions: () => {
		GWLog("Sidenotes.updateSidenotePositions", "sidenotes.js", 1);

		/*  If we’re in footnotes mode (ie. the viewport is too narrow), then
			don’t do anything.
			*/
		if (Sidenotes.mediaQueries.viewportWidthBreakpoint.matches)
			return;

		//  Update the disposition of sidenotes within collapse blocks.
		Sidenotes.updateSidenotesInCollapseBlocks();

		/*  Initial layout (to force browser layout engine to compute sidenotes’
			height for us).
			*/
		for (var i = 0; i < Sidenotes.citations.length; i++) {
			let sidenote = Sidenotes.sidenoteDivs[i];

			/*  Check whether the sidenote is in the hidden sidenote storage (ie.
				within a currently-collapsed collapse block. If so, skip it.
				*/
			if (sidenote.classList.contains("hidden"))
				continue;

			//  What side is this sidenote on?
			let side = (i % 2) ? Sidenotes.sidenoteColumnLeft : Sidenotes.sidenoteColumnRight;

			//  Maximum height.
			sidenote.firstElementChild.style.maxHeight = `${Sidenotes.sidenoteMaxHeight}px`;

			//  Default position (vertically aligned with the footnote reference).
			sidenote.style.top = Math.round(((Sidenotes.citations[i].getBoundingClientRect().top) - side.getBoundingClientRect().top) + 4) + "px";

			/*  Mark sidenotes which are cut off vertically.
				*/
			let sidenoteOuterWrapper = sidenote.firstElementChild;
			sidenote.classList.toggle("cut-off", (sidenoteOuterWrapper.scrollHeight > sidenoteOuterWrapper.offsetHeight + 2));
		}

		/*  Determine proscribed vertical ranges (ie. bands of the page from which
			sidenotes are excluded, by the presence of, eg. a full-width table).
			NOTE: We assume that proscribed vertical ranges do NOT overlap.
			*/
		var proscribedVerticalRangesLeft = [ ];
		var proscribedVerticalRangesRight = [ ];
		let leftColumnBoundingRect = Sidenotes.sidenoteColumnLeft.getBoundingClientRect();
		let rightColumnBoundingRect = Sidenotes.sidenoteColumnRight.getBoundingClientRect();

		/*  Examine all potentially overlapping elements (ie. non-sidenote
			elements that may appear in, or extend into, the side columns).
			*/
		document.querySelectorAll(Sidenotes.potentiallyOverlappingElementsSelector).forEach(potentiallyOverlappingElement => {
			let elementBoundingRect = potentiallyOverlappingElement.getBoundingClientRect();

			if (!(elementBoundingRect.left > leftColumnBoundingRect.right || elementBoundingRect.right < leftColumnBoundingRect.left))
				proscribedVerticalRangesLeft.push({ top: elementBoundingRect.top - leftColumnBoundingRect.top,
													bottom: elementBoundingRect.bottom - leftColumnBoundingRect.top });

			if (!(elementBoundingRect.left > rightColumnBoundingRect.right || elementBoundingRect.right < rightColumnBoundingRect.left))
				proscribedVerticalRangesRight.push({ top: elementBoundingRect.top - rightColumnBoundingRect.top,
													 bottom: elementBoundingRect.bottom - rightColumnBoundingRect.top });
		});

		/*  The bottom edges of each column are also “proscribed vertical ranges”.
			*/
		proscribedVerticalRangesLeft.push({
			top:    Sidenotes.sidenoteColumnLeft.clientHeight,
			bottom: Sidenotes.sidenoteColumnLeft.clientHeight
		});
		proscribedVerticalRangesRight.push({
			top:    Sidenotes.sidenoteColumnRight.clientHeight,
			bottom: Sidenotes.sidenoteColumnRight.clientHeight
		});

		/*  Correct for overlap (both between sidenotes, and of sidenotes with
			proscribed vertical ranges, such as those associated with full-width
			tables).
			*/
		for (var i = 0; i < Sidenotes.citations.length; i++) {
			let sidenote = Sidenotes.sidenoteDivs[i];
			let sidenoteNumber = sidenote.id.substr(2);

			let nextSidenote = Sidenotes.getNextVisibleSidenote(sidenote);
			let nextSidenoteNumber = nextSidenote ? nextSidenote.id.substr(2) : "";

			/*  Is this sidenote even displayed? Or is it hidden (ie. within
				a currently-collapsed collapse block)? If so, skip it.
				*/
			if (sidenote.classList.contains("hidden")) continue;

			//  What side is this sidenote on?
			let side = (i % 2) ? Sidenotes.sidenoteColumnLeft : Sidenotes.sidenoteColumnRight;

			/*  What points bound the vertical region within which this sidenote may
				be placed?
				*/
			let room = {
				ceiling:    0,
				floor:      side.clientHeight
			};
			let sidenoteFootprint = {
				top:    sidenote.offsetTop - Sidenotes.sidenoteSpacing,
				bottom: sidenote.offsetTop + sidenote.offsetHeight + Sidenotes.sidenoteSpacing
			};
			let sidenoteFootprintHalfwayPoint = (sidenoteFootprint.top + sidenoteFootprint.bottom) / 2;

			let proscribedVerticalRanges = [...((i % 2) ? proscribedVerticalRangesLeft : proscribedVerticalRangesRight)];
			proscribedVerticalRanges.sort((a, b) => {
				if (a.bottom < b.bottom) return -1;
				if (a.bottom > b.bottom) return 1;
				return 0;
			});

			/*  Simultaneously traverse the array of proscribed ranges up and down,
				narrowing down the room we have to work with (in which to place this
				sidenote) from both sides.
				*/
			var nextProscribedRangeAfterSidenote = -1;
			for (var j = 0; j < proscribedVerticalRanges.length; j++) {
				let rangeCountingUp = {
					top:            proscribedVerticalRanges[j].top,
					bottom:         proscribedVerticalRanges[j].bottom,
				};
				rangeCountingUp.halfwayPoint = (rangeCountingUp.top + rangeCountingUp.bottom) / 2;
				if (rangeCountingUp.halfwayPoint < sidenoteFootprintHalfwayPoint)
					room.ceiling = rangeCountingUp.bottom;

				let indexCountingDown = proscribedVerticalRanges.length - j - 1;
				let rangeCountingDown = {
					top:    proscribedVerticalRanges[indexCountingDown].top,
					bottom: proscribedVerticalRanges[indexCountingDown].bottom
				};
				rangeCountingDown.halfwayPoint = (rangeCountingDown.top + rangeCountingDown.bottom) / 2;
				if (rangeCountingDown.halfwayPoint > sidenoteFootprintHalfwayPoint) {
					room.floor = rangeCountingDown.top;
					nextProscribedRangeAfterSidenote = indexCountingDown;
				}
			}
			if (i > 1) {
				let previousSidenoteBottom = Sidenotes.sidenoteDivs[i - 2].offsetTop + Sidenotes.sidenoteDivs[i - 2].offsetHeight;
				if (previousSidenoteBottom > room.ceiling)
					room.ceiling = previousSidenoteBottom;
			}
			GWLog(`Sidenote ${i + 1}’s room is: (${room.ceiling}, ${room.floor}).`, "sidenotes.js", 2);

			//  Is this sidenote capable of fitting within the room it now occupies?
			if (sidenoteFootprint.bottom - sidenoteFootprint.top > room.floor - room.ceiling) {
				/*  If this is not caused by bumping into the top of a proscribed
					range, then it could only be because the sidenote is either too
					long for the entire page itself, or it’s longer than the entire
					footnotes section (and comes very late in the document).
					In that case, just give up.
					*/
				if (nextProscribedRangeAfterSidenote == -1) {
					GWLog("TOO MUCH SIDENOTES. GIVING UP. :(", "sidenotes.js");
					return;
				}

				/*  Otherwise, move the sidenote down to the next free space, and
					try laying it out again.
					*/
				sidenote.style.top = (proscribedVerticalRanges[nextProscribedRangeAfterSidenote].bottom + Sidenotes.sidenoteSpacing) + "px";
				i--;
				continue;
			}
			/*  At this point, we are guaranteed that the sidenote can fit within
				its room. We do not have to worry that it will overlap its floor if
				we move it right up against its ceiling (or vice versa).
				*/

			/*  Does this sidenote overlap its room’s ceiling? In such a case, we
				will have to move it down, regardless of whether there’s a next
				sidenote that would be overlapped.
				*/
			var overlapWithCeiling = room.ceiling - sidenoteFootprint.top;
			if (overlapWithCeiling > 0) {
				GWLog(`Sidenote ${sidenoteNumber} overlaps its ceiling!`, "sidenotes.js", 2);

				sidenote.style.top = (parseInt(sidenote.style.top) + overlapWithCeiling) + "px";
				sidenoteFootprint.top += overlapWithCeiling;
				sidenoteFootprint.bottom += overlapWithCeiling;
			}

			//  Does this sidenote overlap its room’s floor?
			var overlapWithFloor = sidenoteFootprint.bottom - room.floor;
			if (overlapWithFloor > 0)
				GWLog(`Sidenote ${sidenoteNumber} overlaps its floor by ${overlapWithFloor} pixels!`, "sidenotes.js", 2);

			/*  Is there a next sidenote, and if so, is there any overlap between
				it and this one?
				*/
			var overlapWithNextSidenote = nextSidenote ?
										  (sidenoteFootprint.bottom - nextSidenote.offsetTop) :
										  -1;
			if (overlapWithNextSidenote > 0)
				GWLog(`Sidenote ${sidenoteNumber} overlaps sidenote ${nextSidenoteNumber} by ${overlapWithNextSidenote} pixels!`, "sidenotes.js", 2);

			/*  If the sidenote overlaps the next sidenote AND its room’s floor,
				we want to know what it overlaps more.
				*/
			var overlapBelow = Math.max(overlapWithNextSidenote, overlapWithFloor);

			/*  If there’s no overlap with the room’s floor, and there’s no overlap
				with the next sidenote (or there is no next sidenote), then the
				current sidenote’s position needs no further adjustment.
				*/
			if (overlapBelow <= 0) continue;

			/*  Figure out how much vertical space above we have; if there’s enough
				“headroom”, we can simply move the current sidenote up.
				*/
			let previousSidenote = sidenote.previousElementSibling;
			let maxHeadroom = sidenoteFootprint.top - room.ceiling;
			let headroom = previousSidenote ?
						   Math.min(maxHeadroom, (sidenoteFootprint.top - (previousSidenote.offsetTop + previousSidenote.offsetHeight))) :
						   maxHeadroom;
			GWLog(`We have ${headroom}px of headroom.`, "sidenotes.js", 2);

			//  If we have enough headroom, simply move the sidenote up.
			if (headroom >= overlapBelow) {
				GWLog(`There is enough headroom. Moving sidenote ${sidenoteNumber} up.`, "sidenotes.js", 2);
				sidenote.style.top = (parseInt(sidenote.style.top) - overlapBelow) + "px";
				continue;
			} else {
				//  We don’t have enough headroom!
				GWLog(`There is not enough headroom to move sidenote ${sidenoteNumber} all the way up!`, "sidenotes.js", 2);

				/*  If there’s overlap with the room’s floor, and the headroom is
					insufficient to clear that overlap, then we will have to move
					the current sidenote to the next open space, and try laying it
					out again.
					*/
				if (headroom < overlapWithFloor) {
					sidenote.style.top = (proscribedVerticalRanges[nextProscribedRangeAfterSidenote].bottom + Sidenotes.sidenoteSpacing) + "px";
					i--;
					continue;
				}

				/*  If the headroom is enough to clear the sidenote’s overlap with
					the room’s floor (if any), then it must be insufficient to clear
					the overlap with the next sidenote. Before we try moving the
					current sidenote up, we check to see whether the *next* sidenote
					will fit in the remaining space of the current room. If not,
					then that next sidenote will need to be moved to the next open
					space, and the current sidenote need not be disturbed...
					*/
				if ((sidenoteFootprint.bottom + nextSidenote.offsetHeight + Sidenotes.sidenoteSpacing - headroom) >
					proscribedVerticalRanges[nextProscribedRangeAfterSidenote].top)
					continue;

				//  Move the sidenote up as much as we can...
				GWLog(`Moving sidenote ${sidenoteNumber} up by ${headroom} pixels...`, "sidenotes.js", 2);
				sidenote.style.top = (parseInt(sidenote.style.top) - headroom) + "px";
				//  Recompute overlap...
				overlapWithNextSidenote -= headroom;
				/*  And move the next sidenote down - possibly causing overlap.
					(But this will be handled when we process the next sidenote.)
					*/
				GWLog(`... and moving sidenote ${nextSidenoteNumber} down by ${overlapWithNextSidenote} pixels.`, "sidenotes.js", 2);
				nextSidenote.style.top = (parseInt(nextSidenote.style.top) + overlapWithNextSidenote) + "px";
			}
		}

		//  Show the sidenote columns.
		Sidenotes.sidenoteColumnLeft.style.visibility = "";
		Sidenotes.sidenoteColumnRight.style.visibility = "";

		GW.notificationCenter.fireEvent("Sidenotes.sidenotePositionsDidUpdate");
	},

	/*  Destroys the HTML structure of the sidenotes.
		*/
	deconstructSidenotes: () => {
		GWLog("Sidenotes.deconstructSidenotes", "sidenotes.js", 1);

		Sidenotes.sidenoteDivs = null;
		Sidenotes.citations = null;

		Sidenotes.sidenoteColumnLeft.remove();
		Sidenotes.sidenoteColumnLeft = null;
		Sidenotes.sidenoteColumnRight.remove();
		Sidenotes.sidenoteColumnRight = null;
	},

	/*  Constructs the HTML structure, and associated listeners and auxiliaries,
		of the sidenotes.
		*/
	constructSidenotes: () => {
		GWLog("Sidenotes.constructSidenotes", "sidenotes.js", 1);

		/*  Do nothing if sidenotes.js somehow gets run extremely early in the page
			load process.
			*/
		let markdownBody = document.querySelector("#markdownBody");
		if (!markdownBody) return;

		/*  Add the sidenote columns (removing them first if they already exist).
			*/
		if (Sidenotes.sidenoteColumnLeft) Sidenotes.sidenoteColumnLeft.remove();
		if (Sidenotes.sidenoteColumnRight) Sidenotes.sidenoteColumnRight.remove();
		markdownBody.insertAdjacentHTML("beforeend",
			"<div id='sidenote-column-left' class='footnotes' style='visibility:hidden'></div>" +
			"<div id='sidenote-column-right' class='footnotes' style='visibility:hidden'></div>");
		Sidenotes.sidenoteColumnLeft = document.querySelector("#sidenote-column-left");
		Sidenotes.sidenoteColumnRight = document.querySelector("#sidenote-column-right");

		/*  Create and inject the sidenotes.
			*/
		Sidenotes.sidenoteDivs = [ ];
		//  The footnote references (citations).
		Sidenotes.citations = Array.from(document.querySelectorAll("a.footnote-ref"));
		for (var i = 0; i < Sidenotes.citations.length; i++) {
			//  Create the sidenote outer containing block...
			let sidenote = document.createElement("div");
			sidenote.classList.add("sidenote");
			let sidenoteNumber = "" + (i + 1);
			sidenote.id = "sn" + sidenoteNumber;
			//  Wrap the contents of the footnote in two wrapper divs...
			let referencedFootnote = document.querySelector("#fn" + sidenoteNumber);
			sidenote.innerHTML = `<div class="sidenote-outer-wrapper"><div class="sidenote-inner-wrapper">` +
								 (referencedFootnote ? referencedFootnote.innerHTML : "Loading sidenote contents, please wait…")
								 + `</div></div>`;
			//  Add the sidenote to the sidenotes array...
			Sidenotes.sidenoteDivs.push(sidenote);
			//  On which side should the sidenote go? Odd - right; even - left.
			let side = (i % 2) ? Sidenotes.sidenoteColumnLeft : Sidenotes.sidenoteColumnRight;
			//  Inject the sidenote into the page.
			side.appendChild(sidenote);
		}

		/*  Create & inject the sidenote self-links (ie. boxed sidenote numbers).
			*/
		for (var i = 0; i < Sidenotes.citations.length; i++) {
			let sidenoteSelfLink = document.createElement("a");
			sidenoteSelfLink.classList.add("sidenote-self-link");
			sidenoteSelfLink.href = "#sn" + (i + 1);
			sidenoteSelfLink.textContent = (i + 1);
			Sidenotes.sidenoteDivs[i].appendChild(sidenoteSelfLink);
		}

		GW.notificationCenter.fireEvent("Sidenotes.sidenotesDidConstruct");
	},

	cleanup: () => {
		GWLog("Sidenotes.cleanup", "sidenotes.js", 1);

		/*	Deactivate active media queries.
			*/
		cancelDoWhenMatchMedia("Sidenotes.rewriteHashForCurrentMode");
		cancelDoWhenMatchMedia("Sidenotes.rewriteCitationTargetsForCurrentMode");
		cancelDoWhenMatchMedia("Sidenotes.bindOrUnbindEventListenersForCurrentMode");
		cancelDoWhenMatchMedia("Sidenotes.addOrRemoveEventHandlersForCurrentMode");

		/*	Remove sidenotes & auxiliaries from HTML.
			*/
		Sidenotes.deconstructSidenotes();

		GW.notificationCenter.fireEvent("Sidenotes.sidenotesDidDeconstruct");
	},

	/*  Q:  Why is this setup function so long and complex?
		A:  In order to properly handle all of the following:

		1.  The two different modes (footnote popups vs. sidenotes)
		2.  The interactions between sidenotes and collapse blocks
		3.  Linking to footnotes/sidenotes
		4.  Loading a URL that links to a footnote/sidenote
		5.  Changes in the viewport width dynamically altering all of the above

		… and, of course, correct layout of the sidenotes, even in tricky cases
		where the citations are densely packed and the sidenotes are long.
		*/
	setup: () => {
		GWLog("Sidenotes.setup", "sidenotes.js", 1);

		/*  If the page was loaded with a hash that points to a footnote, but
			sidenotes are enabled (or vice-versa), rewrite the hash in accordance
			with the current mode (this will also cause the page to end up scrolled
			to the appropriate element - footnote or sidenote). Add an active media
			query to rewrite the hash whenever the viewport width media query changes.
			*/
		doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "Sidenotes.rewriteHashForCurrentMode", (mediaQuery) => {
			let regex = new RegExp(mediaQuery.matches ? "#sn[0-9]" : "#fn[0-9]");
			let prefix = (mediaQuery.matches ? "#fn" : "#sn");

			if (location.hash.match(regex)) {
				GW.hashRealignValue = prefix + Sidenotes.noteNumberFromHash();

				if (document.readyState == "complete") {
					history.replaceState(null, null, GW.hashRealignValue);
					GW.hashRealignValue = null;
				}
			}
		}, null, (mediaQuery) => {
			if (location.hash.match(/#sn[0-9]/)) {
				GW.hashRealignValue = "#fn" + Sidenotes.noteNumberFromHash();

				if (document.readyState == "complete") {
					history.replaceState(null, null, GW.hashRealignValue);
					GW.hashRealignValue = null;
				}
			}
		});

		/*	We do not bother to construct sidenotes on mobile clients, and so
			the rest of this is also irrelevant.
			*/
		if (GW.isMobile())
			return;

		/*  In footnote mode (ie. on viewports too narrow to support sidenotes),
			footnote reference links (ie. citations) should point down to footnotes
			(this is the default state).
			But in sidenote mode, footnote reference links should point to sidenotes.
			This function rewrites all footnote reference links appropriately to the
			current mode (based on viewport width).
			*/
		GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", () => {
			doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "Sidenotes.rewriteCitationTargetsForCurrentMode", (mediaQuery) => {
				for (var i = 0; i < Sidenotes.citations.length; i++)
					Sidenotes.citations[i].href = (mediaQuery.matches ? "#fn" : "#sn") + (i + 1);
			}, null, (mediaQuery) => {
				for (var i = 0; i < Sidenotes.citations.length; i++)
					Sidenotes.citations[i].href = "#fn" + (i + 1);
			});
		}, { once: true });

		/*  Bind sidenote mouse-hover events.
			*/
		GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", () => {
			Sidenotes.bindSidenoteMouseEvents();
		}, { once: true });

		GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", () => {
			doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "Sidenotes.addOrRemoveEventHandlersForCurrentMode", (mediaQuery) => {
				if (!mediaQuery.matches) {
					/*  After the hash updates, properly highlight everything, if needed.
						Also, if the hash points to a sidenote whose citation is in a
						collapse block, expand it and all collapse blocks enclosing it.
						*/
					GW.notificationCenter.addHandlerForEvent("Collapse.targetDidRevealOnHashUpdate", Sidenotes.updateStateAfterTargetDidRevealOnHashUpdate = (info) => {
						if (location.hash.match(/#sn[0-9]/)) {
							revealElement(document.querySelector("#fnref" + Sidenotes.noteNumberFromHash()), false);
							scrollElementIntoView(getHashTargetedElement(), (-1 * Sidenotes.sidenotePadding));
						}

						Sidenotes.updateTargetCounterpart();
					});

					/*	Add event handler to (asynchronously) recompute sidenote positioning
						when full-width media lazy-loads.
						*/
					GW.notificationCenter.addHandlerForEvent("Rewrite.fullWidthMediaDidLoad", Sidenotes.updateSidenotePositionsAfterFullWidthMediaDidLoad = () => {
						requestAnimationFrame(Sidenotes.updateSidenotePositions);
					});

					/*	Add event handler to (asynchronously) recompute sidenote positioning
						when collapse blocks are expanded/collapsed.
						*/
					GW.notificationCenter.addHandlerForEvent("Collapse.collapseStateDidChange", Sidenotes.updateSidenotePositionsAfterCollapseStateDidChange = () => {
						requestAnimationFrame(Sidenotes.updateSidenotePositions);
					});
				} else {
					/*	Deactivate notification handlers.
						*/
					GW.notificationCenter.removeHandlerForEvent("Rewrite.fullWidthMediaDidLoad", Sidenotes.updateSidenotePositionsAfterFullWidthMediaDidLoad);
					GW.notificationCenter.removeHandlerForEvent("Collapse.collapseStateDidChange", Sidenotes.updateSidenotePositionsAfterCollapseStateDidChange);
					GW.notificationCenter.removeHandlerForEvent("Collapse.targetDidRevealOnHashUpdate", Sidenotes.updateStateAfterTargetDidRevealOnHashUpdate);
				}
			}, null, (mediaQuery) => {
				/*	Deactivate notification handlers.
					*/
				GW.notificationCenter.removeHandlerForEvent("Rewrite.fullWidthMediaDidLoad", Sidenotes.updateSidenotePositionsAfterFullWidthMediaDidLoad);
				GW.notificationCenter.removeHandlerForEvent("Collapse.collapseStateDidChange", Sidenotes.updateSidenotePositionsAfterCollapseStateDidChange);
				GW.notificationCenter.removeHandlerForEvent("Collapse.targetDidRevealOnHashUpdate", Sidenotes.updateStateAfterTargetDidRevealOnHashUpdate);
			});
		}, { once: true });

		/*	Once the sidenotes are constructed, lay them out.
			*/
		GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", () => {
			/*  Lay out the sidenotes as soon as the document is loaded.
				*/
			Sidenotes.updateSidenotePositions();

			/*	If layout remains to be done, queue up another reposition for
				when all layout is complete.
				*/
			if (!GW.pageLayoutComplete) {
				GW.notificationCenter.addHandlerForEvent("Rewrite.pageLayoutDidComplete", () => {
					requestAnimationFrame(Sidenotes.updateSidenotePositions);
				}, { once: true });
			}

			/*  Add a resize listener so that sidenote positions are recalculated when
				the window is resized.
				*/
			window.addEventListener("resize", Sidenotes.windowResized = (event) => {
				GWLog("Sidenotes.windowResized", "sidenotes.js", 2);

				requestAnimationFrame(() => {
					Sidenotes.updateSidenotePositions();
				});
			});
		}, { once: true });

		/*  Construct the sidenotes as soon as the HTML content is fully loaded.
			*/
		GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", Sidenotes.constructSidenotesWhenMainContentLoads = (info) => {
			Sidenotes.constructSidenotes();
		}, { phase: "<eventListeners", once: true, condition: (info) => info.isMainDocument });

		GW.notificationCenter.fireEvent("Sidenotes.setupDidComplete");
	}
};

GW.notificationCenter.fireEvent("Sidenotes.didLoad");

/*	Update the margin note style, and add event listener to re-update it
	when the viewport width changes.
	*/
doWhenPageLoaded (() => {
	doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "Sidenotes.updateMarginNoteStyleForCurrentMode", (mediaQuery) => {
		document.querySelectorAll(".marginnote").forEach(marginNote => {
			marginNote.swapClasses([ "inline", "sidenote" ], (mediaQuery.matches ? 0 : 1));
		});
	});
});

//  LET... THERE... BE... SIDENOTES!!!
Sidenotes.setup();
