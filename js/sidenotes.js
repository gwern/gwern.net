/* sidenotes.js: standalone JS library for parsing HTML documents with Pandoc-style footnotes and dynamically repositioning them into the left/right margins, when browser windows are wide enough.
Sidenotes (see https://www.gwern.net/Sidenotes ) are superior to footnotes where possible because they enable the reader to immediately look at them without requiring user action to 'go to' or 'pop up' the footnotes; even floating footnotes require effort by the reader.
sidenotes.js is inspired by the Tufte-CSS sidenotes (https://edwardtufte.github.io/tufte-css/#sidenotes), but where Tufte-CSS uses static footnotes inlined into the body of the page (requiring modifications to Pandoc's compilation), which doesn't always work well for particularly long or frequent sidenotes, sidenotes.js will rearrange sidenotes to fit as best as possible, and will respond to window changes.
Particularly long sidenotes are also partially 'collapsed'.
Styling (especially for oversized-sidenotes which must scroll) is done in /static/css/default.css "SIDENOTES" section.

Author: Said Achmiz
2019-03-11
license: MIT (derivative of footnotes.js, which is PD)
*/

/********************************/
/*	Events fired by sidenotes.js:

	Sidenotes.didLoad
		Fired when the Sidenotes object has loaded.

	Sidenotes.setupDidComplete
		Fired just before the ‘setup’ function returns.

	Sidenotes.cleanupDidComplete
		Fired just before the ‘cleanup’ function returns.

	Sidenotes.sidenotesDidConstruct
		Fired after HTML structure of sidenotes has been created and injected
		into the page, and event listeners have been attached.

	Sidenotes.sidenotePositionsDidUpdate
		Fired after sidenote positions have been calculated (or re-calculated),
		either after the initial page load or after, e.g., some change in
		viewport dimensions.
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

	/*	Elements which occupy (partially or fully) the sidenote columns, and
		which can thus collide with sidenotes.
		*/
	potentiallyOverlappingElementsSelectors: [
		".width-full img",
		".width-full video",
		".width-full .caption-wrapper",
		".width-full.table-wrapper",
		".full-width-code-block-wrapper .width-full",
		".marginnote"
	],

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

		for (let i = 0; i < Sidenotes.citations.length; i++) {
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
		let counterpart;
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

	/*  Hide sidenotes within currently-collapsed collapse blocks. Show
		sidenotes not within currently-collapsed collapse blocks.
		*/
	updateSidenotesInCollapseBlocks: () => {
		GWLog("Sidenotes.updateSidenotesInCollapseBlocks", "sidenotes.js", 1);

		for (let i = 0; i < Sidenotes.citations.length; i++) {
			let citation = Sidenotes.citations[i];
			let sidenote = Sidenotes.sidenoteDivs[i];

			sidenote.classList.toggle("hidden", isWithinCollapsedBlock(citation));
		}
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

		//	Check for cut-off sidenotes.
		Sidenotes.sidenoteDivs.forEach(sidenote => {
			/*  Check whether the sidenote is in the hidden sidenote storage (ie.
				within a currently-collapsed collapse block. If so, skip it.
				*/
			if (sidenote.classList.contains("hidden"))
				return;

			/*  Mark sidenotes which are cut off vertically.
				*/
			let sidenoteOuterWrapper = sidenote.firstElementChild;
			sidenote.classList.toggle("cut-off", (sidenoteOuterWrapper.scrollHeight > sidenoteOuterWrapper.offsetHeight + 2));
		});

		/*  Determine proscribed vertical ranges (ie. bands of the page from which
			sidenotes are excluded, by the presence of, eg. a full-width table).
			*/
		let leftColumnBoundingRect = Sidenotes.sidenoteColumnLeft.getBoundingClientRect();
		let rightColumnBoundingRect = Sidenotes.sidenoteColumnRight.getBoundingClientRect();

		/*  Examine all potentially overlapping elements (ie. non-sidenote
			elements that may appear in, or extend into, the side columns).
			*/
		let proscribedVerticalRangesLeft = [ ];
		let proscribedVerticalRangesRight = [ ];
		document.querySelectorAll(Sidenotes.potentiallyOverlappingElementsSelectors.join(", ")).forEach(potentiallyOverlappingElement => {
			if (isWithinCollapsedBlock(potentiallyOverlappingElement))
				return;

			let elementBoundingRect = potentiallyOverlappingElement.getBoundingClientRect();

			if (!(   elementBoundingRect.left > leftColumnBoundingRect.right 
				  || elementBoundingRect.right < leftColumnBoundingRect.left))
				proscribedVerticalRangesLeft.push({ top: (elementBoundingRect.top - Sidenotes.sidenoteSpacing) - leftColumnBoundingRect.top,
													bottom: (elementBoundingRect.bottom + Sidenotes.sidenoteSpacing) - leftColumnBoundingRect.top,
													element: potentiallyOverlappingElement });

			if (!(   elementBoundingRect.left > rightColumnBoundingRect.right 
				  || elementBoundingRect.right < rightColumnBoundingRect.left))
				proscribedVerticalRangesRight.push({ top: (elementBoundingRect.top - Sidenotes.sidenoteSpacing) - rightColumnBoundingRect.top,
													 bottom: (elementBoundingRect.bottom + Sidenotes.sidenoteSpacing) - rightColumnBoundingRect.top,
													 element: potentiallyOverlappingElement });
		});

		//  The bottom edges of each column are also “proscribed vertical ranges”.
		proscribedVerticalRangesLeft.push({
			top:    Sidenotes.sidenoteColumnLeft.clientHeight,
			bottom: Sidenotes.sidenoteColumnLeft.clientHeight
		});
		proscribedVerticalRangesRight.push({
			top:    Sidenotes.sidenoteColumnRight.clientHeight,
			bottom: Sidenotes.sidenoteColumnRight.clientHeight
		});

		//	Sort and merge.
		[ proscribedVerticalRangesLeft, proscribedVerticalRangesRight ].forEach(ranges => {
			ranges.sort((rangeA, rangeB) => {
				return (rangeA.top - rangeB.top);
			});

			for (let i = 0; i < ranges.length - 1; i++) {
				let thisRange = ranges[i];
				let nextRange = ranges[i + 1];

				if (nextRange.top <= thisRange.bottom) {
					thisRange.bottom = nextRange.bottom;
					ranges.splice(i + 1, 1);
					i++;
				}
			}
		});

		/*	Remove sidenotes from page, so that we can set their positions
			without causing reflow.
		 */
		Sidenotes.sidenoteDivs.forEach(sidenote => {
			sidenote.lastKnownHeight = sidenote.offsetHeight;
			sidenote.remove();
		});

		//	Clean up old layout cells, if any.
		[ Sidenotes.sidenoteColumnLeft, Sidenotes.sidenoteColumnRight ].forEach(column => {
			column.querySelectorAll(".sidenote-layout-cell").forEach(cell => cell.remove());
		});

		//	Construct new layout cells.
		let layoutCells = [ ];
		[ [ Sidenotes.sidenoteColumnLeft, leftColumnBoundingRect, proscribedVerticalRangesLeft ], 
		  [ Sidenotes.sidenoteColumnRight, rightColumnBoundingRect, proscribedVerticalRangesRight ]
		  ].forEach(side => {
		  	let [ column, rect, ranges ] = side;
			let prevRangeBottom = 0;

			ranges.forEach(range => {
				let cell = newElement("DIV", {
					"class": "sidenote-layout-cell"
				});
				cell.sidenotes = [ ];
				cell.container = column;
				cell.room = (range.top - prevRangeBottom);
				cell.style.top = prevRangeBottom + "px";
				cell.style.height = cell.room + "px";

				column.append(cell);
				cell.rect = cell.getBoundingClientRect();
				layoutCells.push(cell);

				prevRangeBottom = range.bottom;
			});
		});

		/*	Default position for a sidenote within a layout cell is vertically
			aligned with the footnote reference, or else at the top of the 
			cell, whichever is lower.
		 */
		let defaultNotePosInCellForCitation = (cell, citation) => {
			return Math.max(0, Math.round((citation.getBoundingClientRect().top - cell.rect.top) + 4));
		};

		//	Assign sidenotes to layout cells.
		for (let i = 0; i < Sidenotes.citations.length; i++) {
			let citation = Sidenotes.citations[i];
			let citationBoundingRect = citation.getBoundingClientRect();

			let sidenote = Sidenotes.sidenoteDivs[i];

			/*  Is this sidenote even displayed? Or is it hidden (i.e., its
				citation is within a currently-collapsed collapse block)? If so,
				skip it.
				*/
			if (sidenote.classList.contains("hidden"))
				continue;

			//	Get all the cells that the sidenote can fit into.
			let fittingLayoutCells = layoutCells.filter(cell => cell.room >= sidenote.lastKnownHeight);
			if (fittingLayoutCells.length == 0) {
				GWLog("TOO MUCH SIDENOTES. GIVING UP. :(", "sidenotes.js");
				Sidenotes.sidenoteDivs.forEach(sidenote => {
					sidenote.remove();
				});
				return;
			}

			/*	These functions are used to sort layout cells by best fit for 
				placing the current sidenote.
			*/
			let vDistanceToCell = (cell) => {
				if (   citationBoundingRect.top > cell.rect.top 
					&& citationBoundingRect.top < cell.rect.bottom)
					return 0;
				return (citationBoundingRect.top < cell.rect.top
						? Math.abs(citationBoundingRect.top - cell.rect.top)
						: Math.abs(citationBoundingRect.top - cell.rect.bottom));
			};
			let hDistanceToCell = (cell) => {
				return Math.abs(citationBoundingRect.left - (cell.left + (cell.width / 2)));
			};
			let overlapWithNote = (cell, note) => {
				let notePosInCell = defaultNotePosInCellForCitation(cell, citation);

				let otherNoteCitation = Sidenotes.citations[parseInt(note.id.substr(2)) - 1];
				let otherNotePosInCell = defaultNotePosInCellForCitation(cell, otherNoteCitation);

				return (   otherNotePosInCell > notePosInCell + sidenote.lastKnownHeight + Sidenotes.sidenoteSpacing
						|| notePosInCell      > otherNotePosInCell + note.lastKnownHeight + Sidenotes.sidenoteSpacing)
					   ? 0
					   : Math.max(notePosInCell + sidenote.lastKnownHeight + Sidenotes.sidenoteSpacing - otherNotePosInCell,
					   			  otherNotePosInCell + note.lastKnownHeight + Sidenotes.sidenoteSpacing - notePosInCell);
			};
			let cellCrowdedness = (cell) => {
				return cell.sidenotes.reduce((totalOverlap, note) => { return (totalOverlap + overlapWithNote(cell, note)); }, 0);
			};

			/*	We sort the fitting cells by vertical distance from the sidenote
				and crowdedness at the sidenote’s default location within the
				cell, and secondarily by horizontal distance from the sidenote.
			 */
			fittingLayoutCells.sort((cellA, cellB) => {
				return (   (  (vDistanceToCell(cellA) + cellCrowdedness(cellA)) 
							- (vDistanceToCell(cellB) + cellCrowdedness(cellB)))
						|| (hDistanceToCell(cellA) - hDistanceToCell(cellB)));
			});
			let closestFittingLayoutCell = fittingLayoutCells[0];

			//	Add the sidenote to the selected cell.
			closestFittingLayoutCell.room -= (sidenote.lastKnownHeight + Sidenotes.sidenoteSpacing);
			closestFittingLayoutCell.sidenotes.push(sidenote);
		};

		//	Function to compute distance between two successive sidenotes.
		let getDistance = (noteA, noteB) => {
			return (noteB.posInCell - (noteA.posInCell + noteA.lastKnownHeight + Sidenotes.sidenoteSpacing));
		};

		//	Position sidenotes within layout cells.
		layoutCells.forEach(cell => {
			if (cell.sidenotes.length == 0)
				return;

			//	Set all of the cell’s sidenotes to default positions.
			cell.sidenotes.forEach(sidenote => {
				let citation = Sidenotes.citations[(parseInt(sidenote.id.substr(2)) - 1)];
				sidenote.posInCell = defaultNotePosInCellForCitation(cell, citation);
			});

			//	Sort the cell’s sidenotes vertically (secondarily by number).
			cell.sidenotes.sort((noteA, noteB) => {
				return (   (noteA.posInCell - noteB.posInCell)
						|| (parseInt(noteA.id.substr(2)) - parseInt(noteB.id.substr(2))));
			});

			//	Called in pushNotesUp().
			let shiftNotesUp = (noteIndexes, shiftUpDistance) => {
				noteIndexes.forEach(idx => {
					cell.sidenotes[idx].posInCell -= shiftUpDistance;
				});
			};

			//	Called immediately below.
			let pushNotesUp = (pushUpWhich, pushUpForce, bruteStrength = false) => {
				let roomToPush = pushUpWhich.first == 0
								 ? cell.sidenotes[pushUpWhich.first].posInCell
								 : Math.max(0, getDistance(cell.sidenotes[pushUpWhich.first - 1], cell.sidenotes[pushUpWhich.first]));

				let pushUpDistance = bruteStrength 
									 ? pushUpForce 
									 : Math.floor(pushUpForce / pushUpWhich.length);
				if (pushUpDistance <= roomToPush) {
					shiftNotesUp(pushUpWhich, pushUpDistance);
					return (pushUpForce - pushUpDistance);
				} else {
					shiftNotesUp(pushUpWhich, roomToPush);
					if (pushUpWhich.first == 0)
						return (pushUpForce - roomToPush);					

					pushUpWhich.splice(0, 0, pushUpWhich.first - 1);
					return pushNotesUp(pushUpWhich, (pushUpForce - roomToPush), bruteStrength);
				}
			};

			/*	Check each sidenote after the first for overlap with the one
				above it; if it overlaps, try pushing the sidenote(s) above it
				upward, and also shift the note itself downward.
			 */
			for (let i = 1; i < cell.sidenotes.length; i++) {
				let prevNote = cell.sidenotes[i - 1];
				let thisNote = cell.sidenotes[i];
				let nextNote = (i == cell.sidenotes.length - 1)
							   ? null
							   : cell.sidenotes[i + 1];

				let overlapAbove = Math.max(0, (-1 * getDistance(prevNote, thisNote)));
				if (overlapAbove == 0)
					continue;

				let pushUpForce = Math.round(overlapAbove / 2);
				thisNote.posInCell += ((overlapAbove - pushUpForce) + pushNotesUp([ (i - 1) ], pushUpForce));
			}

			/*	Check whether the lowest sidenote overlaps the cell’s bottom;
				if so, push it (and any sidenotes above it that it bumps into)
				upward.
			 */
			let overlapOfBottom = Math.max(0, (cell.sidenotes.last.posInCell + cell.sidenotes.last.lastKnownHeight) - parseInt(cell.style.height));
			if (overlapOfBottom > 0)
				pushNotesUp([ (cell.sidenotes.length - 1) ], overlapOfBottom, true);

			cell.sidenotes.forEach(sidenote => {
				sidenote.style.top = Math.round(sidenote.posInCell) + "px";
			});

			cell.append(...cell.sidenotes);
		});

		//  Show the sidenote columns.
		Sidenotes.sidenoteColumnLeft.style.visibility = "";
		Sidenotes.sidenoteColumnRight.style.visibility = "";

		GW.notificationCenter.fireEvent("Sidenotes.sidenotePositionsDidUpdate");

		return;
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

		/*  Do nothing if constructSidenotes() somehow gets run extremely early 
			in the page load process.
			*/
		let markdownBody = document.querySelector("#markdownBody");
		if (!markdownBody)
			return;

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
		for (let i = 1; i <= Sidenotes.citations.length; i++) {
			//  Create the sidenote outer containing block...
			let sidenote = newElement("DIV", { "class": "sidenote", "id": `sn${i}` });

			//  Wrap the contents of the footnote in two wrapper divs...
			let referencedFootnote = document.querySelector(`#fn${i}`);
			sidenote.innerHTML = `<div class="sidenote-outer-wrapper"><div class="sidenote-inner-wrapper">` 
							   + (referencedFootnote 
							   	  ? referencedFootnote.innerHTML 
							   	  : "Loading sidenote contents, please wait…")
							   + `</div></div>`;

			/*  Create & inject the sidenote self-links (ie. boxed sidenote 
				numbers).
				*/
			let sidenoteSelfLink = newElement("A", { "class": "sidenote-self-link", "href": `#sn${i}` });
			sidenoteSelfLink.textContent = i;
			sidenote.appendChild(sidenoteSelfLink);

			//  Add the sidenote to the sidenotes array...
			Sidenotes.sidenoteDivs.push(sidenote);

			//  On which side should the sidenote go? Odd - right; even - left.
			let side = (i % 2) ? Sidenotes.sidenoteColumnRight : Sidenotes.sidenoteColumnLeft;

			//  Inject the sidenote into the page.
			side.appendChild(sidenote);
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

		GW.notificationCenter.fireEvent("Sidenotes.cleanupDidComplete");
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
		GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", (info) => {
			doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "Sidenotes.rewriteCitationTargetsForCurrentMode", (mediaQuery) => {
				for (let i = 0; i < Sidenotes.citations.length; i++)
					Sidenotes.citations[i].href = (mediaQuery.matches ? "#fn" : "#sn") + (i + 1);
			}, null, (mediaQuery) => {
				for (let i = 0; i < Sidenotes.citations.length; i++)
					Sidenotes.citations[i].href = "#fn" + (i + 1);
			});
		}, { once: true });

		/*  Bind sidenote mouse-hover events.
			*/
		GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", (info) => {
			Sidenotes.bindSidenoteMouseEvents();
		}, { once: true });

		GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", (info) => {
			doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "Sidenotes.addOrRemoveEventHandlersForCurrentMode", (mediaQuery) => {
				/*	Deactivate notification handlers.
					*/
				GW.notificationCenter.removeHandlerForEvent("Rewrite.contentDidChange", Sidenotes.updateSidenotePositionsAfterFullWidthMediaDidLoad);
				GW.notificationCenter.removeHandlerForEvent("Rewrite.fullWidthMediaDidLoad", Sidenotes.updateSidenotePositionsAfterFullWidthMediaDidLoad);
				GW.notificationCenter.removeHandlerForEvent("Collapse.collapseStateDidChange", Sidenotes.updateSidenotePositionsAfterCollapseStateDidChange);
				GW.notificationCenter.removeHandlerForEvent("Collapse.targetDidRevealOnHashUpdate", Sidenotes.updateStateAfterTargetDidRevealOnHashUpdate);
			}, (mediaQuery) => {
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
				GW.notificationCenter.addHandlerForEvent("Rewrite.fullWidthMediaDidLoad", Sidenotes.updateSidenotePositionsAfterFullWidthMediaDidLoad = (info) => {
					if (isWithinCollapsedBlock(info.mediaElement))
						return;

					requestAnimationFrame(Sidenotes.updateSidenotePositions);
				});

				/*	Add event handler to (asynchronously) recompute sidenote positioning
					when collapse blocks are expanded/collapsed.
					*/
				GW.notificationCenter.addHandlerForEvent("Collapse.collapseStateDidChange", Sidenotes.updateSidenotePositionsAfterCollapseStateDidChange = (info) => {
					requestAnimationFrame(Sidenotes.updateSidenotePositions);
				});

				/*	Add event handler to (asynchronously) recompute sidenote positioning
					when new content is loaded (e.g. via transclusion).
					*/
				GW.notificationCenter.addHandlerForEvent("Rewrite.contentDidChange", (info) => {
					requestAnimationFrame(Sidenotes.updateSidenotePositions);
				}, { condition: (info) => (info.document == document) });
			}, (mediaQuery) => {
				/*	Deactivate notification handlers.
					*/
				GW.notificationCenter.removeHandlerForEvent("Rewrite.contentDidChange", Sidenotes.updateSidenotePositionsAfterFullWidthMediaDidLoad);
				GW.notificationCenter.removeHandlerForEvent("Rewrite.fullWidthMediaDidLoad", Sidenotes.updateSidenotePositionsAfterFullWidthMediaDidLoad);
				GW.notificationCenter.removeHandlerForEvent("Collapse.collapseStateDidChange", Sidenotes.updateSidenotePositionsAfterCollapseStateDidChange);
				GW.notificationCenter.removeHandlerForEvent("Collapse.targetDidRevealOnHashUpdate", Sidenotes.updateStateAfterTargetDidRevealOnHashUpdate);
			});
		}, { once: true });

		/*	Once the sidenotes are constructed, lay them out.
			*/
		GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", (info) => {
			/*  Lay out the sidenotes as soon as the document is loaded.
				*/
			Sidenotes.updateSidenotePositions();

			/*	If layout remains to be done, queue up another reposition for
				when all layout is complete.
				*/
			if (!GW.pageLayoutComplete) {
				GW.notificationCenter.addHandlerForEvent("Rewrite.pageLayoutDidComplete", (info) => {
					requestAnimationFrame(Sidenotes.updateSidenotePositions);
				}, { once: true });
			}

			/*  Add a resize listener so that sidenote positions are recalculated when
				the window is resized.
				*/
			window.addEventListener("resize", Sidenotes.windowResized = (event) => {
				GWLog("Sidenotes.windowResized", "sidenotes.js", 2);

				requestAnimationFrame(Sidenotes.updateSidenotePositions);
			});
		}, { once: true });

		/*  Construct the sidenotes as soon as the HTML content is fully loaded.
			*/
		GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", Sidenotes.constructSidenotesWhenMainContentLoads = (info) => {
			Sidenotes.constructSidenotes();
		}, { 
			phase: "<eventListeners", 
			once: true, 
			condition: (info) => info.isMainDocument 
		});

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
