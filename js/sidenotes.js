/* sidenotes.js: standalone JS library for parsing HTML documents with Pandoc-style footnotes and dynamically repositioning them into the left/right margins, when browser windows are wide enough.
Sidenotes (see https://www.gwern.net/Sidenotes ) are superior to footnotes where possible because they enable the reader to immediately look at them without requiring user action to 'go to' or 'pop up' the footnotes; even floating footnotes require effort by the reader.
sidenotes.js is inspired by the Tufte-CSS sidenotes (https://edwardtufte.github.io/tufte-css/#sidenotes), but where Tufte-CSS uses static footnotes inlined into the body of the page (requiring modifications to Pandoc's compilation), which doesn't always work well for particularly long or frequent sidenotes, sidenotes.js will rearrange sidenotes to fit as best as possible, and will respond to window changes.
Particularly long sidenotes are also partially 'collapsed'.
Styling (especially for oversized-sidenotes which must scroll) is done in /static/css/default.css "SIDENOTES" section.

Author: Said Achmiz
2019-03-11
license: MIT (derivative of footnotes.js, which is PD)
*/

/*****************/
/*	Configuration.
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

	/*	The greatest width (in CSS dimensions) at which sidenotes will _not_ be
		shown. If the viewport is wider than this, then sidenotes are enabled.
	 */
	sidenotesIfViewportWiderThan: "1760px",

	useLeftColumn: () => false,
	useRightColumn: () => true
};

/******************/
/*	Implementation.
 */
Sidenotes = { ...Sidenotes,
	/*  Media query objects (for checking and attaching listeners).
		*/
	mediaQueries: {
		viewportWidthBreakpoint: matchMedia(`(max-width: ${Sidenotes.sidenotesIfViewportWiderThan})`)
	},

	/*****************/
	/* Infrastructure.
	 */
	sidenotes: null,
	citations: null,

	sidenoteColumnLeft: null,
	sidenoteColumnRight: null,

	hiddenSidenoteStorage: null,

	sidenoteOfNumber: (number) => {
		return (Sidenotes.sidenotes.find(sidenote => Notes.noteNumberFromHash(sidenote.id) == number) ?? null);
	},

	citationOfNumber: (number) => {
		return (Sidenotes.citations.find(citation => Notes.noteNumberFromHash(citation.id) == number) ?? null);
	},

	/*	The sidenote of the same number as the given citation; 
		or, the citation of the same number as the given sidenote.
	 */
	counterpart: (element) => {
		let number = Notes.noteNumberFromHash(element.id);
		return (element.classList.contains("sidenote")
			    ? Sidenotes.citationOfNumber(number)
			    : Sidenotes.sidenoteOfNumber(number));
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
		let target;
		if (location.hash.match(/#sn[0-9]/)) {
			target = document.querySelector("#sn" + Notes.noteNumberFromHash());
		} else if (location.hash.match(/#fnref[0-9]/) && Sidenotes.mediaQueries.viewportWidthBreakpoint.matches == false) {
			target = document.querySelector("#fnref" + Notes.noteNumberFromHash());
		} else {
			return;
		}
		let counterpart = Sidenotes.counterpart(target);

		/*  Mark the target and the counterpart, if any.
			*/
		if (target)
			target.classList.add("targeted");
		if (counterpart)
			counterpart.classList.add("targeted");
	},

	/*  Hide sidenotes within currently-collapsed collapse blocks. Show
		sidenotes not within currently-collapsed collapse blocks.
		*/
	updateSidenotesInCollapseBlocks: () => {
		GWLog("Sidenotes.updateSidenotesInCollapseBlocks", "sidenotes.js", 1);

		Sidenotes.sidenotes.forEach(sidenote => {
			let citation = Sidenotes.counterpart(sidenote);
			sidenote.classList.toggle("hidden", isWithinCollapsedBlock(citation));
		});
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
		Sidenotes.sidenotes.forEach(sidenote => {
			/*  Check whether the sidenote is currently hidden (i.e., within a 
				currently-collapsed collapse block or similar). If so, skip it.
				*/
			if (sidenote.classList.contains("hidden"))
				return;

			//  On which side should the sidenote go? Odd - right; even - left.
			let sidenoteNumber = Notes.noteNumberFromHash(sidenote.id);
			let side = (sidenoteNumber % 2) ? Sidenotes.sidenoteColumnRight : Sidenotes.sidenoteColumnLeft;

			//  Inject the sidenote into the column (provisionally).
			side.appendChild(sidenote);

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
			without causing reflow. Store their layout heights (which cannot
			be retrieved in the normal way while the sidenotes aren’t part of
			the DOM).
		 */
		Sidenotes.sidenotes.forEach(sidenote => {
			sidenote.lastKnownHeight = sidenote.offsetHeight;
			sidenote.remove();
		});

		//	Clean up old layout cells, if any.
		[ Sidenotes.sidenoteColumnLeft, Sidenotes.sidenoteColumnRight ].forEach(column => {
			column.querySelectorAll(".sidenote-layout-cell").forEach(cell => cell.remove());
		});

		//	Construct new layout cells.
		let layoutCells = [ ];
		let sides = [ ];
		if (Sidenotes.useLeftColumn())
			sides.push([ Sidenotes.sidenoteColumnLeft, leftColumnBoundingRect, proscribedVerticalRangesLeft ]);
		if (Sidenotes.useRightColumn())
			sides.push([ Sidenotes.sidenoteColumnRight, rightColumnBoundingRect, proscribedVerticalRangesRight ]);
		sides.forEach(side => {
			let [ column, rect, ranges ] = side;
			let prevRangeBottom = 0;

			ranges.forEach(range => {
				let cell = newElement("DIV", {
					"class": "sidenote-layout-cell"
				}, {
					"sidenotes": [ ],
					"container": column,
					"room": (range.top - prevRangeBottom),
					"style": `top: ${prevRangeBottom + "px"}; height: ${(range.top - prevRangeBottom) + "px"}`
				});

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
		for (citation of Sidenotes.citations) {
			let citationBoundingRect = citation.getBoundingClientRect();

			let sidenote = Sidenotes.counterpart(citation);

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
				Sidenotes.sidenotes.forEach(sidenote => {
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

				let otherNoteCitation = Sidenotes.counterpart(note);
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
				let citation = Sidenotes.counterpart(sidenote);
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

			//	Set the sidenote positions via inline styles.
			cell.sidenotes.forEach(sidenote => {
				sidenote.style.top = Math.round(sidenote.posInCell) + "px";
			});

			//	Re-inject the sidenotes into the page.
			cell.append(...cell.sidenotes);
		});

		//  Un-hide the sidenote columns.
		Sidenotes.sidenoteColumnLeft.style.visibility = "";
		Sidenotes.sidenoteColumnRight.style.visibility = "";

		//	Fire event.
		GW.notificationCenter.fireEvent("Sidenotes.sidenotePositionsDidUpdate");
	},

	/*  Destroys the HTML structure of the sidenotes.
		*/
	deconstructSidenotes: () => {
		GWLog("Sidenotes.deconstructSidenotes", "sidenotes.js", 1);

		Sidenotes.sidenotes = null;
		Sidenotes.citations = null;

		if (Sidenotes.sidenoteColumnLeft)
			Sidenotes.sidenoteColumnLeft.remove();
		Sidenotes.sidenoteColumnLeft = null;

		if (Sidenotes.sidenoteColumnRight)
			Sidenotes.sidenoteColumnRight.remove();
		Sidenotes.sidenoteColumnRight = null;

		if (Sidenotes.hiddenSidenoteStorage)
			Sidenotes.hiddenSidenoteStorage.remove();
		Sidenotes.hiddenSidenoteStorage = null;
	},

	/*  Constructs the HTML structure, and associated listeners and auxiliaries,
		of the sidenotes.
		*/
	constructSidenotes: (loadEventInfo) => {
		GWLog("Sidenotes.constructSidenotes", "sidenotes.js", 1);

		/*  Do nothing if constructSidenotes() somehow gets run extremely early 
			in the page load process.
			*/
		let markdownBody = document.querySelector("#markdownBody");
		if (markdownBody == null)
			return;

		//	Destroy before creating.
		Sidenotes.deconstructSidenotes();

		//  Add the sidenote columns.
		Sidenotes.sidenoteColumnLeft = newElement("DIV", { "id": "sidenote-column-left" });
		Sidenotes.sidenoteColumnRight = newElement("DIV", { "id": "sidenote-column-right" });
		[ Sidenotes.sidenoteColumnLeft, Sidenotes.sidenoteColumnRight ].forEach(column => {
			column.classList.add("footnotes");
			column.style.visibility = "hidden";
			markdownBody.append(column);
		});

		//	Add the hidden sidenote storage.
		markdownBody.append(Sidenotes.hiddenSidenoteStorage = newElement("DIV", {
			"id": "hidden-sidenote-storage", 
			"class": "footnotes",
			"style": "display:none" 
		}));

		/*  Create and inject the sidenotes.
			*/
		Sidenotes.sidenotes = [ ];
		//  The footnote references (citations).
		Sidenotes.citations = Array.from(document.querySelectorAll("a.footnote-ref"));
		Sidenotes.citations.forEach(citation => {
			let noteNumber = Notes.noteNumberFromHash(citation.hash);

			//  Create the sidenote outer containing block...
			let sidenote = newElement("DIV", { "class": "sidenote", "id": `sn${noteNumber}` });

			//  Wrap the contents of the footnote in two wrapper divs...
			let referencedFootnote = document.querySelector(`#fn${noteNumber}`);
			sidenote.innerHTML = `<div class="sidenote-outer-wrapper"><div class="sidenote-inner-wrapper">` 
							   + (referencedFootnote 
							   	  ? referencedFootnote.innerHTML 
							   	  : "Loading sidenote contents, please wait…")
							   + `</div></div>`;
			sidenote.outerWrapper = sidenote.querySelector(".sidenote-outer-wrapper");
			sidenote.innerWrapper = sidenote.querySelector(".sidenote-inner-wrapper");

			/*  Create & inject the sidenote self-links (ie. boxed sidenote 
				numbers).
				*/
			sidenote.appendChild(newElement("A", { 
				"class": "sidenote-self-link",
				"href": `#sn${noteNumber}` 
			}, { 
				"textContent": noteNumber 
			}));

			//  Add the sidenote to the sidenotes array...
			Sidenotes.sidenotes.push(sidenote);

			//	Inject the sidenote into the page.
			Sidenotes.hiddenSidenoteStorage.append(sidenote);
		});

		/*  Bind sidenote mouse-hover events.
			*/
		Sidenotes.citations.forEach(citation => {
			let sidenote = Sidenotes.counterpart(citation);

			//	Unbind existing events, if any.
			if (sidenote.onSidenoteMouseEnterHighlightCitation)
				sidenote.removeEventListener("mouseenter", sidenote.onSidenoteMouseEnterHighlightCitation);
			if (sidenote.onSidenoteMouseLeaveUnhighlightCitation)
				sidenote.removeEventListener("mouseleave", sidenote.onSidenoteMouseLeaveUnhighlightCitation);

			if (citation.onCitationMouseEnterSlideSidenote)
				citation.removeEventListener("mouseenter", citation.onCitationMouseEnterSlideSidenote);
			if (sidenote.onSidenoteMouseEnterSlideSidenote)
				sidenote.removeEventListener("mouseenter", sidenote.onSidenoteMouseEnterSlideSidenote);
			if (sidenote.onSidenoteMouseLeaveUnslideSidenote)
				sidenote.removeEventListener("mouseleave", sidenote.onSidenoteMouseLeaveUnslideSidenote);

			if (sidenote.scrollListener)
				sidenote.outerWrapper.removeEventListener("scroll", sidenote.scrollListener);

			//	Bind new events.
			sidenote.addEventListener("mouseenter", sidenote.onSidenoteMouseEnterHighlightCitation = (event) => {
				citation.classList.toggle("highlighted", true);
			});
			sidenote.addEventListener("mouseleave", sidenote.onSidenoteMouseLeaveUnhighlightCitation = (event) => {
				citation.classList.toggle("highlighted", false);
			});

			citation.addEventListener("mouseenter", citation.onCitationMouseEnterSlideSidenote = (event) => {
				Sidenotes.putAllSidenotesBack(sidenote);
				Sidenotes.slideSidenoteIntoView(sidenote, true);
			});
			sidenote.addEventListener("mouseenter", sidenote.onSidenoteMouseEnterSlideSidenote = (event) => {
				Sidenotes.putAllSidenotesBack(sidenote);
				Sidenotes.slideSidenoteIntoView(sidenote, false);
			});
			sidenote.addEventListener("mouseleave", sidenote.onSidenoteMouseLeaveUnslideSidenote = (event) => {
				Sidenotes.putSidenoteBack(sidenote);
			});

			sidenote.scrollListener = addScrollListener((event) => {
				sidenote.classList.toggle("hide-more-indicator", sidenote.outerWrapper.scrollTop + sidenote.outerWrapper.clientHeight == sidenote.outerWrapper.scrollHeight);
			}, null, { }, sidenote.outerWrapper);
		});

		GW.notificationCenter.fireEvent("Sidenotes.sidenotesDidConstruct");

		//	Fire events.
		GW.notificationCenter.fireEvent("GW.contentDidLoad", {
			source: "Sidenotes.constructSidenotes",
			container: Sidenotes.hiddenSidenoteStorage,
			document: document,
			loadLocation: loadEventInfo.loadLocation
		});
		GW.notificationCenter.fireEvent("GW.contentDidInject", {
			source: "Sidenotes.constructSidenotes",
			container: Sidenotes.hiddenSidenoteStorage,
			document: document,
			loadLocation: loadEventInfo.loadLocation,
			flags: 0
		});
	},

	cleanup: () => {
		GWLog("Sidenotes.cleanup", "sidenotes.js", 1);

		/*	Deactivate active media queries.
			*/
		cancelDoWhenMatchMedia("Sidenotes.rewriteHashForCurrentMode");
		cancelDoWhenMatchMedia("Sidenotes.rewriteCitationTargetsForCurrentMode");
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
			sidenotes are enabled (or vice-versa), rewrite the hash in 
			accordance with the current mode (this will also cause the page to 
			end up scrolled to the appropriate element - footnote or sidenote). 
			Add an active media query to rewrite the hash whenever the viewport 
			width media query changes.
			*/
		doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "Sidenotes.rewriteHashForCurrentMode", (mediaQuery) => {
			let regex = new RegExp(mediaQuery.matches ? "^#sn[0-9]" : "^#fn[0-9]");
			let prefix = (mediaQuery.matches ? "#fn" : "#sn");

			if (location.hash.match(regex))
				relocate(prefix + Notes.noteNumberFromHash());
		}, null, (mediaQuery) => {
			if (location.hash.match(/^#sn[0-9]/))
				relocate("#fn" + Notes.noteNumberFromHash());
		});

		/*	We do not bother to construct sidenotes on mobile clients, and so
			the rest of this is also irrelevant.
			*/
		if (GW.isMobile())
			return;

		/*	Update the margin note style, and add event listener to re-update it
			when the viewport width changes. Also add event handler to update
			margin note style in transcluded content and pop-frames.
			*/
		GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", (info) => {
			doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "Sidenotes.updateMarginNoteStyleForCurrentMode", (mediaQuery) => {
				document.querySelectorAll(".marginnote").forEach(marginNote => {
					marginNote.swapClasses([ "inline", "sidenote" ], (Sidenotes.mediaQueries.viewportWidthBreakpoint.matches ? 0 : 1));
				});
			});
		}, { 
			phase: "rewrite",
			condition: (info) => (info.container == document.body),
			once: true
		});
		GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
			info.container.querySelectorAll(".marginnote").forEach(marginNote => {
				marginNote.swapClasses([ "inline", "sidenote" ], ((Sidenotes.mediaQueries.viewportWidthBreakpoint.matches || info.document != document) ? 0 : 1));
			});
		});

		/*  In footnote mode (ie. on viewports too narrow to support sidenotes),
			footnote reference links (i.e., citations) should point down to 
			footnotes (this is the default state). But in sidenote mode, 
			footnote reference links should point to sidenotes.

			We therefore rewrite all footnote reference links appropriately to
			the current mode (based on viewport width).

			We also add an active media query to rewrite the links if a change
			in viewport width results in switching modes, as well as an event
			handler to rewrite footnote reference links in transcluded content.
			*/
		GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", (info) => {
			doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "Sidenotes.rewriteCitationTargetsForCurrentMode", (mediaQuery) => {
				Sidenotes.citations.forEach(citation => {
					citation.href = (mediaQuery.matches ? "#fn" : "#sn") + Notes.noteNumberFromHash(citation.hash);
				});
			}, null, (mediaQuery) => {
				Sidenotes.citations.forEach(citation => {
					citation.href = "#fn" + Notes.noteNumberFromHash(citation.hash);
				});
			});

			GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", (info) => {
				info.document.querySelectorAll("a.footnote-ref").forEach(citation => {
					if (citation.pathname == location.pathname)
						citation.href = (Sidenotes.mediaQueries.viewportWidthBreakpoint.matches ? "#fn" : "#sn") 
										+ Notes.noteNumberFromHash(citation.hash);
				});
			}, { 
				phase: "rewrite",
				condition: (info) => (info.document == document) 
			});
		}, { once: true });

		/*	What happens if the page loads with a URL hash that points to a 
			sidenote or footnote or citation? We need to scroll appropriately,
			and do other adjustments, just as we do when the hash updates.
		 */
		GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotePositionsDidUpdate", Sidenotes.updateStateAfterHashChange = (info) => {
			if (location.hash.match(/#sn[0-9]/)) {
				let citation = document.querySelector("#fnref" + Notes.noteNumberFromHash());
				let sidenote = Sidenotes.counterpart(citation);

				revealElement(citation, false);

				Sidenotes.slideLockSidenote(sidenote);

				requestAnimationFrame(() => {
					scrollElementIntoView(sidenote, (-1 * Sidenotes.sidenotePadding));

					Sidenotes.unSlideLockSidenote(sidenote);
				});
			} else if (location.hash.match(/#fnref[0-9]/)) {
				let citation = getHashTargetedElement();
				let sidenote = Sidenotes.counterpart(citation);

				Sidenotes.slideLockSidenote(sidenote);

				requestAnimationFrame(() => {
					let sidenoteRect = sidenote.getBoundingClientRect();
					let citationRect = citation.getBoundingClientRect();
					if (   sidenoteRect.top < Sidenotes.sidenotePadding
						&& citationRect.bottom + (-1 * (sidenoteRect.top - Sidenotes.sidenotePadding)) < window.innerHeight)
						scrollElementIntoView(sidenote, (-1 * Sidenotes.sidenotePadding));

					Sidenotes.unSlideLockSidenote(sidenote);
				});
			}

			/*	Hide mode selectors, as they would otherwise overlap a 
				sidenote that’s on the top-right.
			 */
			if (Notes.noteNumberFromHash() > "")
				Sidenotes.hideInterferingUIElements();

			//	Update highlighted state of sidenote and citation, if need be.
			requestAnimationFrame(() => {
				Sidenotes.updateTargetCounterpart();
			});
		}, { once: true });

		GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", (info) => {
			doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "Sidenotes.addOrRemoveEventHandlersForCurrentMode", (mediaQuery) => {
				/*	Deactivate event handlers.
					*/
				GW.notificationCenter.removeHandlerForEvent("GW.hashDidChange", Sidenotes.updateStateAfterHashChange);
				GW.notificationCenter.removeHandlerForEvent("Rewrite.contentDidChange", Sidenotes.updateSidenotePositionsAfterContentDidChange);
				GW.notificationCenter.removeHandlerForEvent("Rewrite.fullWidthMediaDidLoad", Sidenotes.updateSidenotePositionsAfterFullWidthMediaDidLoad);
				GW.notificationCenter.removeHandlerForEvent("Collapse.collapseStateDidChange", Sidenotes.updateSidenotePositionsAfterCollapseStateDidChange);
				window.removeEventListener("resize", Sidenotes.windowResized);
				GW.notificationCenter.removeHandlerForEvent("GW.contentDidInject", Sidenotes.bindAdditionalSidenoteSlideEvents);
				removeScrollListener("Sidenotes.unSlideSidenotesOnScroll");
			}, (mediaQuery) => {
				/*  After the hash updates, properly highlight everything, if needed.
					Also, if the hash points to a sidenote whose citation is in a
					collapse block, expand it and all collapse blocks enclosing it.
					*/
				GW.notificationCenter.addHandlerForEvent("GW.hashDidChange", Sidenotes.updateStateAfterHashChange);

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
				GW.notificationCenter.addHandlerForEvent("Rewrite.contentDidChange", Sidenotes.updateSidenotePositionsAfterContentDidChange = (info) => {
					requestAnimationFrame(Sidenotes.updateSidenotePositions);
				}, { condition: (info) => (info.document == document) });

				/*  Add a resize listener so that sidenote positions are recalculated when
					the window is resized.
					*/
				window.addEventListener("resize", Sidenotes.windowResized = (event) => {
					GWLog("Sidenotes.windowResized", "sidenotes.js", 2);

					requestAnimationFrame(Sidenotes.updateSidenotePositions);
				});

				/*	Add handler to bind more sidenote-slide events if more 
					citations are injected (e.g., in a popup).
				 */
				GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", Sidenotes.bindAdditionalSidenoteSlideEvents = (info) => {
					info.container.querySelectorAll("a.footnote-ref").forEach(citation => {
						if (citation.pathname != location.pathname)
							return;

						let sidenote = Sidenotes.counterpart(citation);
						citation.addEventListener("mouseenter", citation.onCitationMouseEnterSlideSidenote = (event) => {
							Sidenotes.putAllSidenotesBack(sidenote);
							Sidenotes.slideSidenoteIntoView(sidenote, false);
						});
					});
				}, { condition: (info) => (info.document != document) });

				/*	Add a scroll listener to un-slide all sidenotes on scroll.
				 */
				addScrollListener((event) => {
					Sidenotes.putAllSidenotesBack();
				}, "Sidenotes.unSlideSidenotesOnScroll", { defer: true });
			}, (mediaQuery) => {
				/*	Deactivate event handlers.
					*/
				GW.notificationCenter.removeHandlerForEvent("GW.hashDidChange", Sidenotes.updateStateAfterHashChange);
				GW.notificationCenter.removeHandlerForEvent("Rewrite.contentDidChange", Sidenotes.updateSidenotePositionsAfterContentDidChange);
				GW.notificationCenter.removeHandlerForEvent("Rewrite.fullWidthMediaDidLoad", Sidenotes.updateSidenotePositionsAfterFullWidthMediaDidLoad);
				GW.notificationCenter.removeHandlerForEvent("Collapse.collapseStateDidChange", Sidenotes.updateSidenotePositionsAfterCollapseStateDidChange);
				window.removeEventListener("resize", Sidenotes.windowResized);
				GW.notificationCenter.removeHandlerForEvent("GW.contentDidInject", Sidenotes.bindAdditionalSidenoteSlideEvents);
				removeScrollListener("Sidenotes.unSlideSidenotesOnScroll");
			});
		}, { once: true });

		/*	Once the sidenotes are constructed, lay them out.
			*/
		GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", (info) => {
			//	Lay out sidenotes once page layout is complete.
			doWhenPageLayoutComplete(Sidenotes.updateSidenotePositions);
		}, { once: true });

		/*  Construct the sidenotes whenever content is injected into the main
			page (including the initial page load).
			*/
		GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", Sidenotes.constructSidenotesWhenMainPageContentDidInject = (info) => {
			GWLog("Sidenotes.constructSidenotesWhenMainPageContentDidInject", "sidenotes.js", 1);

			Sidenotes.constructSidenotes(info);
		}, { 
			condition: (info) => (   info.document == document
								  && info.source != "Sidenotes.constructSidenotes")
		});

		GW.notificationCenter.fireEvent("Sidenotes.setupDidComplete");
	},

	hideInterferingUIElements: () => {
		requestAnimationFrame(() => {
			setTimeout(() => {
				DarkMode.hideModeSelector();
				ReaderMode.hideModeSelector();

				GW.backToTop.classList.toggle("hidden", true)
			}, 25);
		});
	},

	/**************/
	/*	Slidenotes.
	 */

	displacedSidenotes: [ ],

	/*	If the sidenote is offscreen, slide it onto the screen.
	 */
	slideSidenoteIntoView: (sidenote, toCitation) => {
		GWLog("Sidenotes.slideSidenoteIntoView", "sidenotes.js", 3);

		Sidenotes.hideInterferingUIElements();

		if (sidenote.style.transform == "none")
			return;

		let sidenoteRect = sidenote.getBoundingClientRect();
		if (   sidenoteRect.top >= Sidenotes.sidenotePadding
			&& sidenoteRect.bottom <= window.innerHeight - Sidenotes.sidenotePadding)
			return;

		let newSidenoteTop = sidenoteRect.top;
		if (toCitation) {
			let citationRect = Sidenotes.counterpart(sidenote).getBoundingClientRect()
			newSidenoteTop = Math.max(sidenoteRect.top, Sidenotes.sidenotePadding);
			if (newSidenoteTop + sidenoteRect.height < citationRect.bottom)
				newSidenoteTop = citationRect.top;
			newSidenoteTop = Math.min(citationRect.top + sidenoteRect.height, 
									  Math.min(newSidenoteTop + sidenoteRect.height, 
									  		   window.innerHeight - Sidenotes.sidenotePadding)) 
						   - sidenoteRect.height;
		} else {
			newSidenoteTop = Math.max(Sidenotes.sidenotePadding, sidenoteRect.top);
			newSidenoteTop = Math.min(newSidenoteTop + sidenoteRect.height, 
									  window.innerHeight - Sidenotes.sidenotePadding) 
						   - sidenoteRect.height;		
		}

		let delta = newSidenoteTop - sidenoteRect.top;
		if (delta) {
			sidenote.style.transform = `translateY(${delta}px)`;
			sidenote.classList.toggle("displaced", true);
			if (Sidenotes.displacedSidenotes.includes(sidenote) == false)
				Sidenotes.displacedSidenotes.push(sidenote);
		}
	},

	/*	Un-slide a slid-onto-the-screen sidenote.
	 */
	putSidenoteBack: (sidenote) => {
		GWLog("Sidenotes.putSidenoteBack", "sidenotes.js", 3);

		if (sidenote.style.transform == "none")
			return;

		sidenote.style.transform = "";
		sidenote.classList.toggle("displaced", false);
	},

	/*	Un-slide all sidenotes (possibly except one).
	 */
	putAllSidenotesBack: (exceptOne = null) => {
		GWLog("Sidenotes.putAllSidenotesBack", "sidenotes.js", 3);

		Sidenotes.displacedSidenotes.forEach(sidenote => {
			if (sidenote == exceptOne)
				return;

			Sidenotes.putSidenoteBack(sidenote);
		});
		Sidenotes.displacedSidenotes = exceptOne ? [ exceptOne ] : [ ];
	},

	/*	Instantly un-slide sidenote and make it un-slidable.
	 */
	slideLockSidenote: (sidenote) => {
		GWLog("Sidenotes.slideLockSidenote", "sidenotes.js", 3);

		sidenote.style.transition = "none";
		sidenote.style.transform = "none";
		sidenote.classList.toggle("displaced", false);
	},

	/*	Instantly un-slide sidenote and make it slidable.
	 */
	unSlideLockSidenote: (sidenote) => {
		GWLog("Sidenotes.unSlideLockSidenote", "sidenotes.js", 3);

		sidenote.style.transform = "";
		sidenote.style.transition = "";
		sidenote.classList.toggle("displaced", false);
	},
};

GW.notificationCenter.fireEvent("Sidenotes.didLoad");

//  LET... THERE... BE... SIDENOTES!!!
Sidenotes.setup();
