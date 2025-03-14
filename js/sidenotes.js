/*	sidenotes.js: standalone JS library for parsing HTML documents with
	Pandoc-style footnotes and dynamically repositioning them into the
	left/right margins, when browser windows are wide enough.

	Sidenotes (see https://gwern.net/sidenote ) are superior to footnotes where
	possible because they enable the reader to immediately look at them without
	requiring user action to “go to” or “pop up” the footnotes; even floating
	footnotes require effort by the reader.

	sidenotes.js is inspired by the Tufte-CSS sidenotes
	(https://edwardtufte.github.io/tufte-css/#sidenotes), but where Tufte-CSS
	uses static footnotes inlined into the body of the page (requiring
	modifications to Pandoc’s compilation), which doesn’t always work well for
	particularly long or frequent sidenotes, sidenotes.js will rearrange
	sidenotes to fit as best as possible, and will respond to window changes.

	Particularly long sidenotes are also partially “collapsed”. Styling
	(especially for oversized-sidenotes which must scroll) is done in
	/static/css/default.css “SIDENOTES” section.

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
		".width-full table",
		".width-full pre",
		".marginnote"
	],

	constrainMarginNotesWithinSelectors: [
		".backlink-context",
		".margin-notes-block",
		".footnote",
		".sidenote > *"
	],

	/*	The smallest width (in CSS dimensions) at which sidenotes will be shown.
		If the viewport is narrower than this, then sidenotes are disabled.
	 */
	minimumViewportWidthForSidenotes: "1761px",

	/*	The smallest width (in CSS dimensions) at which margin notes will be
		shown as sidenotes. If the viewport is narrower than this, then margin
		notes will be inlined.
	 */
	minimumViewportWidthForSidenoteMarginNotes: "1497px",

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
		viewportWidthBreakpoint: matchMedia(`(min-width: ${Sidenotes.minimumViewportWidthForSidenotes})`),
		marginNoteViewportWidthBreakpoint: matchMedia(`(min-width: ${Sidenotes.minimumViewportWidthForSidenoteMarginNotes})`)
	},

	/*****************/
	/* Infrastructure.
	 */
	sidenotes: null,
	citations: null,

	sidenoteColumnLeft: null,
	sidenoteColumnRight: null,

	positionUpdateQueued: false,

	sidenoteOfNumber: (number) => {
		return (Sidenotes.sidenotes?.find(sidenote => Notes.noteNumber(sidenote) == number) ?? null);
	},

	citationOfNumber: (number) => {
		return (Sidenotes.citations?.find(citation => Notes.noteNumber(citation) == number) ?? null);
	},

	/*	The sidenote of the same number as the given citation;
		or, the citation of the same number as the given sidenote.
	 */
	counterpart: (element) => {
		if (element == null)
			return null;

		let number = Notes.noteNumber(element);
		let counterpart = (element.classList.contains("sidenote")
						   ? Sidenotes.citationOfNumber(number)
						   : Sidenotes.sidenoteOfNumber(number));
		if (counterpart == null)
			GWLog(`Counterpart of ${element.tagName}#${element.id}.${(Array.from(element.classList).join("."))} not found!`, "sidenotes.js", 0);

		return counterpart;
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

		if (Sidenotes.mediaQueries.viewportWidthBreakpoint.matches == false)
			return;

		//  Clear existing targeting.
		let targetedElementSelector = [
			".footnote-ref",
			".footnote",
			".sidenote"
		].map(x => x + ".targeted").join(", ");
		document.querySelectorAll(targetedElementSelector).forEach(element => {
			element.classList.remove("targeted");
		});

		//  Identify target and counterpart, if any.
		let target = location.hash.match(/^#(sn|fnref)[0-9]+$/)
					 ? getHashTargetedElement()
					 : null;
		let counterpart = Sidenotes.counterpart(target);

		//  Mark the target and the counterpart, if any.
		if (target)
			target.classList.add("targeted");
		if (counterpart)
			counterpart.classList.add("targeted");
	},

	updateFootnoteBackArrowOrientationForSidenote: (sidenote, offset = 0) => {
		if (sidenote.classList.contains("hidden"))
			return;

		sidenote.querySelectorAll(".footnote-back").forEach(footnoteBackLink => {
			let arrow = footnoteBackLink.querySelector(".footnote-back svg");
			let citation = Notes.hashMatchesCitation(footnoteBackLink.hash)
						   ? Sidenotes.counterpart(sidenote)
						   : document.querySelector(selectorFromHash(footnoteBackLink.hash));

			if (   arrow == null
				|| citation == null)
				return;

			let sidenoteRect = sidenote.getBoundingClientRect();
			let citationRect = Array.from(citation.getClientRects()).first;
			let x = (citationRect.x + citationRect.width * 0.5) - (sidenoteRect.x + sidenoteRect.width * 0.5);
			let y = (sidenoteRect.y + sidenoteRect.height * 0.5) + offset - (citationRect.y + citationRect.height * 0.5);
			let rotationAngle = -1 * (modulo(Math.atan2(y, x) * (180 / Math.PI), 360) - 90);
			arrow.style.transform = `rotate(${rotationAngle}deg)`;
		});
	},

	/*	Queues a sidenote position update on the next available animation frame,
		if an update is not already queued.
	 */
	updateSidenotePositionsIfNeeded: () => {
		if (   Sidenotes.sidenoteColumnLeft  == null
			&& Sidenotes.sidenoteColumnRight == null)
			return;

		if (Sidenotes.positionUpdateQueued)
			return;

		Sidenotes.positionUpdateQueued = true;
		requestIdleCallback(() => {
			Sidenotes.positionUpdateQueued = false;

			if (Sidenotes.sidenotesNeedConstructing)
				return;

			Sidenotes.updateSidenotePositions();
		});
	},

	updateStateAfterHashChange: () => {
		GWLog("Sidenotes.updateStateAfterHashChange", "sidenotes.js", 1);

		//	Update highlighted state of sidenote and citation, if need be.
		Sidenotes.updateTargetCounterpart();

		/*	If hash targets a sidenote, reveal corresponding citation; and
			vice-versa. Scroll everything into view properly.
		 */
		if (Notes.hashMatchesSidenote()) {
			let citation = document.querySelector("#" + Notes.citationIdForNumber(Notes.noteNumberFromHash()));
			if (citation == null)
				return;

			revealElement(citation, {
				scrollIntoView: false
			});

			let sidenote = Sidenotes.counterpart(citation);
			if (sidenote == null)
				return;

			Sidenotes.slideLockSidenote(sidenote);

			requestAnimationFrame(() => {
				scrollElementIntoView(sidenote, {
					offset: (-1 * (Sidenotes.sidenotePadding + 1))
				});

				Sidenotes.unSlideLockSidenote(sidenote);
			});
		} else if (Notes.hashMatchesCitation()) {
			let citation = getHashTargetedElement();
			if (citation == null)
				return;

			let sidenote = Sidenotes.counterpart(citation);
			if (sidenote == null)
				return;

			Sidenotes.slideLockSidenote(sidenote);

			requestAnimationFrame(() => {
				let sidenoteRect = sidenote.getBoundingClientRect();
				let citationRect = citation.getBoundingClientRect();
				if (   sidenoteRect.top < Sidenotes.sidenotePadding + 1
					&& citationRect.bottom + (-1 * (sidenoteRect.top - Sidenotes.sidenotePadding)) < window.innerHeight)
					scrollElementIntoView(sidenote, {
						offset: (-1 * (Sidenotes.sidenotePadding + 1))
					});

				Sidenotes.unSlideLockSidenote(sidenote);
			});
		}

		/*	Hide mode selectors, as they would otherwise overlap a
			sidenote that’s on the top-right.
		 */
		if (Notes.noteNumberFromHash() > "")
			Sidenotes.hideInterferingUIElements();
	},

	/*	Place all sidenotes in the appropriate column; hide or un-hide each
		sidenote as appropriate.
	 */
	updateSidenoteDispositions: () => {
		for (let [ index, sidenote ] of Sidenotes.sidenotes.entries()) {
			/*  Hide sidenotes within currently-collapsed collapse blocks. Show
				sidenotes not within currently-collapsed collapse blocks.
			 */
			sidenote.classList.toggle("hidden", isWithinCollapsedBlock(Sidenotes.citations[index]));

			//  On which side should the sidenote go?
			let sidenoteNumber = Notes.noteNumber(sidenote);
			let side = null;
			       if (   Sidenotes.useLeftColumn()  == true
					   && Sidenotes.useRightColumn() == false) {
				//	Left.
				side = Sidenotes.sidenoteColumnLeft;
			} else if (   Sidenotes.useLeftColumn()  == false
					   && Sidenotes.useRightColumn() == true) {
				//	Right.
				side = Sidenotes.sidenoteColumnRight;
			} else if (   Sidenotes.useLeftColumn()  == true
					   && Sidenotes.useRightColumn() == true) {
				//	Odd - right; even - left.
				side = (sidenoteNumber % 2
						? Sidenotes.sidenoteColumnLeft
						: Sidenotes.sidenoteColumnRight);
			}

			//  Inject the sidenote into the column, if need be.
			if (sidenote.parentElement != side)
				side.append(sidenote);
		}
	},

	/*  This function actually calculates and sets the positions of all sidenotes.
	 */
	updateSidenotePositions: () => {
		GWLog("Sidenotes.updateSidenotePositions", "sidenotes.js", 1);

		/*  If we’re in footnotes mode (ie. the viewport is too narrow), then
			don’t do anything.
		 */
		if (Sidenotes.mediaQueries.viewportWidthBreakpoint.matches == false)
			return;

		//	Update the disposition of sidenotes.
		Sidenotes.updateSidenoteDispositions();

		/*  Determine proscribed vertical ranges (ie. bands of the page from which
			sidenotes are excluded, by the presence of, eg. a full-width table).
		 */
		let leftColumnBoundingRect  = Sidenotes.sidenoteColumnLeft.getBoundingClientRect();
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

			if (!(   elementBoundingRect.left  > leftColumnBoundingRect.right
				  || elementBoundingRect.right < leftColumnBoundingRect.left))
				proscribedVerticalRangesLeft.push({ top:    (elementBoundingRect.top    - Sidenotes.sidenoteSpacing) - leftColumnBoundingRect.top,
													bottom: (elementBoundingRect.bottom + Sidenotes.sidenoteSpacing) - leftColumnBoundingRect.top,
													element: potentiallyOverlappingElement });

			if (!(   elementBoundingRect.left  > rightColumnBoundingRect.right
				  || elementBoundingRect.right < rightColumnBoundingRect.left))
				proscribedVerticalRangesRight.push({ top:    (elementBoundingRect.top    - Sidenotes.sidenoteSpacing) - rightColumnBoundingRect.top,
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

				if (nextRange.top   <= thisRange.bottom) {
					thisRange.bottom = nextRange.bottom;
					ranges.splice(i + 1, 1);
					i++;
				}
			}
		});

		//	Store their layout heights of sidenotes.
		Sidenotes.sidenotes.forEach(sidenote => {
			//  Mark sidenotes which are cut off vertically.
			let sidenoteOuterWrapper = sidenote.firstElementChild;
			sidenote.classList.toggle("cut-off", (sidenoteOuterWrapper.scrollHeight > sidenoteOuterWrapper.offsetHeight + 2));
		});

		//	Construct new layout cells.
		let layoutCells = [ ];
		let columnSpecs = [ ];
		if (Sidenotes.useLeftColumn())
			columnSpecs.push([ Sidenotes.sidenoteColumnLeft,  leftColumnBoundingRect,  proscribedVerticalRangesLeft  ]);
		if (Sidenotes.useRightColumn())
			columnSpecs.push([ Sidenotes.sidenoteColumnRight, rightColumnBoundingRect, proscribedVerticalRangesRight ]);
		columnSpecs.forEach(columnSpec => {
			let [ column, columnRect, proscribedVerticalRanges ] = columnSpec;
			let prevRangeBottom = 0;

			proscribedVerticalRanges.forEach(range => {
				layoutCells.push({
					sidenotes: [ ],
					column: column,
					columnRect: columnRect,
					left: columnRect.left,
					right: columnRect.right,
					width: columnRect.width,
					top: (columnRect.top + prevRangeBottom),
					bottom: (columnRect.top + range.top),
					height: (range.top - prevRangeBottom),
					room: (range.top - prevRangeBottom),
				});

				prevRangeBottom = range.bottom;
			});
		});

		/*	Default position for a sidenote within a layout cell is vertically
			aligned with the footnote reference, or else at the top of the
			cell, whichever is lower.
		 */
		let defaultNotePosInCellForCitation = (cell, citation) => {
			return Math.max(0, Math.round((citation.getBoundingClientRect().top - cell.top) + 4));
		};

		//	Assign sidenotes to layout cells.
		for (let [ index, citation ] of Sidenotes.citations.entries()) {
			let sidenote = Sidenotes.sidenotes[index];

			/*  Is this sidenote even displayed? Or is it hidden (i.e., its
				citation is within a currently-collapsed collapse block)?
				If so, skip it.
			 */
			if (sidenote.classList.contains("hidden"))
				continue;

			//	Get all the cells that the sidenote can fit into.
			let fittingLayoutCells = layoutCells.filter(cell => cell.room >= sidenote.offsetHeight);
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
			let citationBoundingRect = citation.getBoundingClientRect();
			let vDistanceToCell = (cell) => {
				if (   citationBoundingRect.top > cell.top
					&& citationBoundingRect.top < cell.bottom)
					return 0;
				return (citationBoundingRect.top < cell.top
						? Math.abs(citationBoundingRect.top - cell.top)
						: Math.abs(citationBoundingRect.top - cell.bottom));
			};
			let hDistanceToCell = (cell) => {
				return Math.abs(citationBoundingRect.left - (cell.left + (cell.width / 2)));
			};
			let overlapWithNote = (cell, note) => {
				let notePosInCell = defaultNotePosInCellForCitation(cell, citation);

				let otherNoteCitation = Sidenotes.counterpart(note);
				let otherNotePosInCell = defaultNotePosInCellForCitation(cell, otherNoteCitation);

				return (   otherNotePosInCell > notePosInCell +  sidenote.offsetHeight + Sidenotes.sidenoteSpacing
						|| notePosInCell      > otherNotePosInCell + note.offsetHeight + Sidenotes.sidenoteSpacing)
					   ? 0
					   : Math.max(notePosInCell +  sidenote.offsetHeight + Sidenotes.sidenoteSpacing - otherNotePosInCell,
					   			  otherNotePosInCell + note.offsetHeight + Sidenotes.sidenoteSpacing - notePosInCell);
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
			let closestFittingLayoutCell = fittingLayoutCells.first;

			//	Add the sidenote to the selected cell.
			closestFittingLayoutCell.room -= (sidenote.offsetHeight + Sidenotes.sidenoteSpacing);
			closestFittingLayoutCell.sidenotes.push(sidenote);
		}

		//	Function to compute distance between two successive sidenotes.
		let getDistance = (noteA, noteB) => {
			return (noteB.posInCell - (noteA.posInCell + noteA.offsetHeight + Sidenotes.sidenoteSpacing));
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
						|| (parseInt(noteA.id.slice(2)) - parseInt(noteB.id.slice(2))));
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
			let overlapOfBottom = Math.max(0, (cell.sidenotes.last.posInCell + cell.sidenotes.last.offsetHeight) - cell.height);
			if (overlapOfBottom > 0)
				pushNotesUp([ (cell.sidenotes.length - 1) ], overlapOfBottom, true);

			//	Set the sidenote positions via inline styles.
			cell.sidenotes.forEach(sidenote => {
				sidenote.style.top = (Math.round(sidenote.posInCell) + (cell.top - cell.columnRect.top)) + "px";
			});
		});

		//	Update footnote-back arrow orientation in sidenotes.
		Sidenotes.sidenotes.forEach(Sidenotes.updateFootnoteBackArrowOrientationForSidenote);

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
	},

	/*  Constructs the HTML structure, and associated listeners and auxiliaries,
		of the sidenotes.
	 */
	constructSidenotes: (injectEventInfo) => {
		GWLog("Sidenotes.constructSidenotes", "sidenotes.js", 1);

		//	Ensure that infrastructure is constructed if need be.
		if (   Sidenotes.sidenoteColumnLeft  == null
			&& Sidenotes.sidenoteColumnRight == null) {
			let markdownBody = document.querySelector("#markdownBody");

			//  Add the sidenote columns.
			Sidenotes.sidenoteColumnLeft  = newElement("DIV", { "id": "sidenote-column-left" });
			Sidenotes.sidenoteColumnRight = newElement("DIV", { "id": "sidenote-column-right" });
			[ Sidenotes.sidenoteColumnLeft, Sidenotes.sidenoteColumnRight ].forEach(column => {
				column.classList.add("footnotes", "sidenote-column");
				column.style.visibility = "hidden";
				markdownBody.append(column);
			});

			Sidenotes.sidenotes = [ ];
			Sidenotes.citations = [ ];
		}

		let modifiedFootnote = injectEventInfo.container.closest("li.footnote");
		if (modifiedFootnote) {
			let noteNumber = Notes.noteNumber(modifiedFootnote);

			let sidenote = Sidenotes.sidenoteOfNumber(noteNumber);
			if (sidenote == null)
				return;

			let citation = Sidenotes.citationOfNumber(noteNumber);

			//	Inject the sidenote contents into the sidenote.
			let includeLink = synthesizeIncludeLink(citation, {
				"class": "include-strict include-unwrap",
				"data-include-selector-not": ".footnote-self-link"
			});
			includeLink.hash = "#" + Notes.footnoteIdForNumber(noteNumber);
			sidenote.querySelector(".sidenote-inner-wrapper").replaceChildren(includeLink);

			//	Trigger transclude.
			Transclude.triggerTransclude(includeLink, {
				container: sidenote,
				document: document,
				source: "Sidenotes.constructSidenotes"
			});

			//	Fire event.
			GW.notificationCenter.fireEvent("Sidenotes.sidenotesDidConstruct");

			return;
		}

		/*	Get citations in the newly injected content. (Skip citations of a
			number matching existing citations; also, deduplicate, keeping only
			the first instance of multiple citations with the same number.)
		 */
		let newCitations = Array.from(injectEventInfo.container.querySelectorAll("a.footnote-ref")).filter(citation => {
			return (Sidenotes.citationOfNumber(Notes.noteNumber(citation)) == null);
		}).filter((citation, index, array) => {
			return (array.findIndex(otherCitation => (Notes.noteNumber(otherCitation) == Notes.noteNumber(citation))) == index);
		});
		if (newCitations.length == 0)
			return;

		//  The footnote references (citations).
		Sidenotes.citations.push(...newCitations);

		//	Sort citations array by citation number.
		Sidenotes.citations.sort((citationA, citationB) => (Notes.noteNumber(citationA) - Notes.noteNumber(citationB)));

		//	Remove existing sidenotes.
		Sidenotes.sidenotes.forEach(sidenote => {
			sidenote.remove();
		});
		Sidenotes.sidenotes = [ ];

		//  Create and inject the sidenotes.
		Sidenotes.citations.forEach(citation => {
			let noteNumber = Notes.noteNumber(citation);

			//  Create the sidenote outer containing block...
			let sidenote = newElement("DIV", {
				class: "sidenote",
				id: Notes.sidenoteIdForNumber(noteNumber),
				style: "visibility: hidden"
			});

			//  Wrap the contents of the footnote in two wrapper divs...
			sidenote.appendChild(sidenote.outerWrapper = newElement("DIV", {
				class: "sidenote-outer-wrapper"
			})).appendChild(sidenote.innerWrapper = newElement("DIV", {
				class: "sidenote-inner-wrapper"
			}));

			/*  Create & inject the sidenote self-link (ie. boxed sidenote
				number).
			 */
			sidenote.append(newElement("A", {
				"class": "sidenote-self-link",
				"href": "#" + Notes.sidenoteIdForNumber(noteNumber)
			}, {
				"textContent": noteNumber
			}));

			//	Inject the sidenote contents into the sidenote.
			let includeLink = synthesizeIncludeLink(citation, {
				"class": "include-unwrap",
				"data-include-selector-not": ".footnote-self-link",
				"data-page-section-id": "footnotes"
			});
			includeLink.hash = "#" + Notes.footnoteIdForNumber(noteNumber);
			includeLink.classList.remove("footnote-ref");
			sidenote.querySelector(".sidenote-inner-wrapper").append(includeLink);

			//  Add the sidenote to the sidenotes array...
			Sidenotes.sidenotes.push(sidenote);
		});

		//	Inject the sidenotes into the page.
		Sidenotes.updateSidenoteDispositions();

		/*  Bind sidenote mouse-hover events.
		 */
		for (let [ index, citation ] of Sidenotes.citations.entries()) {
			let sidenote = Sidenotes.sidenotes[index];

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

			//	Highlight both sidenote and citation on sidenote hover.
			sidenote.addEventListener("mouseenter", sidenote.onSidenoteMouseEnterHighlightCitation = (event) => {
				citation.classList.toggle("highlighted", true);
				sidenote.classList.toggle("hovering", true);
			});
			sidenote.addEventListener("mouseleave", sidenote.onSidenoteMouseLeaveUnhighlightCitation = (event) => {
				citation.classList.toggle("highlighted", false);
				sidenote.classList.toggle("hovering", false);
			});

			/*	NOTE: The absence of event bindings here to highlight the 
				citation and the sidenote when the *citation* is hovered over 
				is not an oversight. Those events are bound elsewhere (see
				rewrite.js).
			 */

			/*	Slide sidenote into view when hovering over either sidenote or
				citation.
			 */
			citation.addEventListener("mouseenter", citation.onCitationMouseEnterSlideSidenote = (event) => {
				Sidenotes.putAllSidenotesBack(sidenote);
				requestAnimationFrame(() => {
					Sidenotes.slideSidenoteIntoView(sidenote, true);
				});
			});
			sidenote.addEventListener("mouseenter", sidenote.onSidenoteMouseEnterSlideSidenote = (event) => {
				Sidenotes.putAllSidenotesBack(sidenote);
				requestAnimationFrame(() => {
					Sidenotes.slideSidenoteIntoView(sidenote, false);
				});
			});
			sidenote.addEventListener("mouseleave", sidenote.onSidenoteMouseLeaveUnslideSidenote = (event) => {
				Sidenotes.putSidenoteBack(sidenote);
			});

			/*	Hide the “there is more hidden content below the fold”
				(ellipsis) indicator when client scrolls to the bottom of a 
				cut-off sidenote; show it otherwise.
			 */
			sidenote.scrollListener = addScrollListener(sidenote.onSidenoteScrollToggleHideMoreIndicator = (event) => {
				sidenote.classList.toggle("hide-more-indicator", sidenote.outerWrapper.scrollTop + sidenote.outerWrapper.clientHeight == sidenote.outerWrapper.scrollHeight);
			}, {
				target: sidenote.outerWrapper
			});
		}

		//	Invalidate cached notes.
		Notes.invalidateCachedNotesForPathname(injectEventInfo.loadLocation.pathname);

		//	Trigger transcludes.
		let columns = [ ];
		if (Sidenotes.useLeftColumn())
			columns.push(Sidenotes.sidenoteColumnLeft);
		if (Sidenotes.useRightColumn())
			columns.push(Sidenotes.sidenoteColumnRight);
		columns.forEach(column => {
			Transclude.triggerTranscludesInContainer(column, {
				container: column,
				document: document,
				source: "Sidenotes.constructSidenotes"
			}, {
				immediately: false,
				doWhenDidInject: (info) => {
					info.container.closest(".sidenote").style.visibility = "";
				}
			});
		});

		//	Fire event.
		GW.notificationCenter.fireEvent("Sidenotes.sidenotesDidConstruct");
	},

	cleanup: () => {
		GWLog("Sidenotes.cleanup", "sidenotes.js", 1);

		/*	Deactivate active media queries.
		 */
		cancelDoWhenMatchMedia("Sidenotes.rewriteHashForCurrentMode");
		cancelDoWhenMatchMedia("Sidenotes.updateMarginNoteStyleForCurrentMode");
		cancelDoWhenMatchMedia("Sidenotes.rewriteCitationTargetsForCurrentMode");
		cancelDoWhenMatchMedia("Sidenotes.addOrRemoveEventHandlersForCurrentMode");

		/*	Remove sidenotes & auxiliaries from HTML.
		 */
		Sidenotes.deconstructSidenotes(true);

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
			if (   Notes.hashMatchesFootnote()
				|| Notes.hashMatchesSidenote()) {
				relocate("#" + (mediaQuery.matches 
								? Notes.sidenoteIdForNumber(Notes.noteNumberFromHash()) 
								: Notes.footnoteIdForNumber(Notes.noteNumberFromHash())));

				//	Update targeting.
				if (mediaQuery.matches)
					Sidenotes.updateTargetCounterpart();
				else
					updateFootnoteTargeting();
			}
		}, null, (mediaQuery) => {
			if (Notes.hashMatchesSidenote()) {
				relocate("#" + Notes.footnoteIdForNumber(Notes.noteNumberFromHash()));

				//	Update targeting.
				updateFootnoteTargeting();
			}
		});

		/*	We do not bother to construct sidenotes on mobile clients, and so
			the rest of this is also irrelevant.
		 */
		if (GW.isMobile())
			return;

		/*	Add event handler to update margin note style in transcluded content
			and pop-frames.
		 */
		addContentInjectHandler(GW.contentInjectHandlers.setMarginNoteStyle = (eventInfo) => {
			GWLog("setMarginNoteStyle", "sidenotes.js", 1);

			/*	Set margin notes to ‘inline’ or ‘sidenote’ style, depending on 
				what mode the page is in (based on viewport width), whether each
				margin note is in a constrained block, and whether it’s on the 
				main page or in something like a pop-frame.
			 */
			eventInfo.container.querySelectorAll(".marginnote").forEach(marginNote => {
				let inline = (   marginNote.closest(Sidenotes.constrainMarginNotesWithinSelectors.join(", "))
							  || Sidenotes.mediaQueries.marginNoteViewportWidthBreakpoint.matches == false
							  || eventInfo.document != document);
				marginNote.swapClasses([ "inline", "sidenote" ], (inline ? 0 : 1));
			});
		}, ">rewrite");

		/*	When the main content loads, update the margin note style; and add 
			event listener to re-update it when the viewport width changes.
		 */
		addContentLoadHandler(GW.contentLoadHandlers.addUpdateMarginNoteStyleForCurrentModeActiveMediaQuery = (eventInfo) => {
			GWLog("addUpdateMarginNoteStyleForCurrentModeActiveMediaQuery", "sidenotes.js", 1);

			doWhenMatchMedia(Sidenotes.mediaQueries.marginNoteViewportWidthBreakpoint, "Sidenotes.updateMarginNoteStyleForCurrentMode", (mediaQuery) => {
				GW.contentInjectHandlers.setMarginNoteStyle(eventInfo);
			});
		}, "rewrite", (info) => info.container == document.main, true);

		/*	When an anchor link is clicked that sets the hash to its existing
			value, weird things happen. In particular, weird things happen with
			citations and sidenotes. We must prevent that, by updating state
			properly when that happens. (No ‘hashchange’ event is fired in this
			case, so we cannot depend on the ‘GW.hashDidChange’ event handler.)
		 */
		addContentInjectHandler(Sidenotes.addFauxHashChangeEventsToNoteMetaLinks = (eventInfo) => {
			GWLog("addFauxHashChangeEventsToNoteMetaLinks", "sidenotes.js", 1);

			let selector = [
				"a.footnote-ref",
				"a.sidenote-self-link",
				".sidenote a.footnote-back"
			].join(", ");

			eventInfo.container.querySelectorAll(selector).forEach(link => {
				link.addActivateEvent((event) => {
					if (link.hash == location.hash)
						Sidenotes.updateStateAfterHashChange();
				});
			});
		}, "eventListeners", (info) => info.document == document);

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
		doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "Sidenotes.rewriteCitationTargetsForCurrentMode", (mediaQuery) => {
			document.querySelectorAll("a.footnote-ref").forEach(citation => {
				if (citation.pathname == location.pathname)
					citation.hash = "#" + (mediaQuery.matches 
										   ? Notes.sidenoteIdForNumber(Notes.noteNumber(citation))
										   : Notes.footnoteIdForNumber(Notes.noteNumber(citation)));
			});
		}, null, (mediaQuery) => {
			document.querySelectorAll("a.footnote-ref").forEach(citation => {
				if (citation.pathname == location.pathname)
					citation.hash = "#" + Notes.footnoteIdForNumber(Notes.noteNumber(citation));
			});
		});

		addContentLoadHandler(Sidenotes.rewriteCitationTargetsInLoadedContent = (eventInfo) => {
			GWLog("rewriteCitationTargetsInLoadedContent", "sidenotes.js", 1);

			document.querySelectorAll("a.footnote-ref").forEach(citation => {
				if (citation.pathname == location.pathname)
					citation.hash = "#" + (Sidenotes.mediaQueries.viewportWidthBreakpoint.matches 
										   ? Notes.sidenoteIdForNumber(Notes.noteNumber(citation))
										   : Notes.footnoteIdForNumber(Notes.noteNumber(citation)));
			});
		}, "rewrite", (info) => info.document == document);

		/*	What happens if the page loads with a URL hash that points to a
			sidenote or footnote or citation? We need to scroll appropriately,
			and do other adjustments, just as we do when the hash updates.
		 */
		GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", Sidenotes.updateHashTargetedElementStateAfterSidenotesDidConstruct = (eventInfo) => {
			GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotePositionsDidUpdate", (eventInfo) => {
				Sidenotes.updateStateAfterHashChange();
			}, { once: true });
		});

		//	Add listener to update sidenote positions when media loads.
		addContentInjectHandler(GW.contentInjectHandlers.addMediaElementLoadEventsInSidenotes = (eventInfo) => {
			GWLog("addMediaElementLoadEventsInSidenotes", "sidenotes.js", 1);

			eventInfo.container.querySelectorAll("figure img, figure video").forEach(mediaElement => {
				mediaElement.addEventListener("load", (event) => {
					doWhenPageLayoutComplete(Sidenotes.updateSidenotePositionsIfNeeded);
				}, { once: true });
			});
		}, "eventListeners", (info) => (info.container.closest(".sidenote") != null));

		//	Add event listeners, and the switch between modes.
		doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "Sidenotes.addOrRemoveEventHandlersForCurrentMode", (mediaQuery) => {
			doWhenPageLayoutComplete(Sidenotes.updateSidenotePositionsIfNeeded);

			/*  After the hash updates, properly highlight everything, if needed.
				Also, if the hash points to a sidenote whose citation is in a
				collapse block, expand it and all collapse blocks enclosing it.
			 */
			GW.notificationCenter.addHandlerForEvent("GW.hashDidChange", Sidenotes.updateStateAfterHashChange);

			/*	Add event handler to (asynchronously) recompute sidenote positioning
				when full-width media lazy-loads.
			 */
			GW.notificationCenter.addHandlerForEvent("Rewrite.fullWidthMediaDidLoad", Sidenotes.updateSidenotePositionsAfterFullWidthMediaDidLoad = (eventInfo) => {
				if (isWithinCollapsedBlock(eventInfo.mediaElement))
					return;

				doWhenPageLayoutComplete(Sidenotes.updateSidenotePositionsIfNeeded);
			});

			/*	Add event handler to (asynchronously) recompute sidenote positioning
				when collapse blocks are expanded/collapsed.
			 */
			GW.notificationCenter.addHandlerForEvent("Collapse.collapseStateDidChange", Sidenotes.updateSidenotePositionsAfterCollapseStateDidChange = (eventInfo) => {
				let sidenote = eventInfo.collapseBlock.closest(".sidenote");
				if (sidenote?.classList.contains("hovering")) {
					sidenote.addEventListener("mouseleave", (event) => {
						doWhenPageLayoutComplete(Sidenotes.updateSidenotePositionsIfNeeded);
					}, { once: true });
				} else {
					doWhenPageLayoutComplete(Sidenotes.updateSidenotePositionsIfNeeded);
				}
			}, {
				condition: (info) => (info.collapseBlock.closest("#markdownBody") != null)
			});

			/*	Add event handler to (asynchronously) recompute sidenote positioning
				when new content is loaded (e.g. via transclusion).
			 */
			GW.notificationCenter.addHandlerForEvent("Rewrite.contentDidChange", Sidenotes.updateSidenotePositionsAfterContentDidChange = (eventInfo) => {
				let sidenote = eventInfo.where.closest(".sidenote");
				if (sidenote?.classList.contains("hovering")) {
					sidenote.addEventListener("mouseleave", (event) => {
						doWhenPageLayoutComplete(Sidenotes.updateSidenotePositionsIfNeeded);
					}, { once: true });
				} else {
					doWhenPageLayoutComplete(Sidenotes.updateSidenotePositionsIfNeeded);
				}
			}, {
				condition: (info) => (   info.document == document
									  && info.source == "transclude")
			});

			/*  Add a resize listener so that sidenote positions are recalculated when
				the window is resized.
			 */
			addWindowResizeListener(Sidenotes.windowResized = (event) => {
				GWLog("Sidenotes.windowResized", "sidenotes.js", 3);

				doWhenPageLayoutComplete(Sidenotes.updateSidenotePositionsIfNeeded);
			}, {
				name: "Sidenotes.updateSidenotePositionsOnWindowResizeListener"
			});

			/*	Add handler to bind more sidenote-slide events if more
				citations are injected (e.g., in a popup).
			 */
			addContentInjectHandler(Sidenotes.bindAdditionalSidenoteSlideEvents = (eventInfo) => {
				GWLog("bindAdditionalSidenoteSlideEvents", "sidenotes.js", 3);

				eventInfo.container.querySelectorAll("a.footnote-ref").forEach(citation => {
					let sidenote = Sidenotes.counterpart(citation);
					if (sidenote == null)
						return;

					citation.addEventListener("mouseenter", citation.onCitationMouseEnterSlideSidenote = (event) => {
						Sidenotes.putAllSidenotesBack(sidenote);
						requestAnimationFrame(() => {
							Sidenotes.slideSidenoteIntoView(sidenote, true);
						});
					});
				});
			}, "eventListeners", (info) => info.document != document);

			/*	Add a scroll listener to un-slide all sidenotes on scroll.
			 */
			addScrollListener((event) => {
				Sidenotes.putAllSidenotesBack();
			}, {
				name: "Sidenotes.unSlideSidenotesOnScrollListener",
				defer: true
			});
		}, (mediaQuery) => {
			/*	Deactivate event handlers.
			 */
			GW.notificationCenter.removeHandlerForEvent("GW.hashDidChange", Sidenotes.updateStateAfterHashChange);
			GW.notificationCenter.removeHandlerForEvent("Rewrite.contentDidChange", Sidenotes.updateSidenotePositionsAfterContentDidChange);
			GW.notificationCenter.removeHandlerForEvent("Rewrite.fullWidthMediaDidLoad", Sidenotes.updateSidenotePositionsAfterFullWidthMediaDidLoad);
			GW.notificationCenter.removeHandlerForEvent("Collapse.collapseStateDidChange", Sidenotes.updateSidenotePositionsAfterCollapseStateDidChange);
			GW.notificationCenter.removeHandlerForEvent("GW.contentDidInject", Sidenotes.bindAdditionalSidenoteSlideEvents);
			removeScrollListener("Sidenotes.unSlideSidenotesOnScroll");
			removeWindowResizeListener("Sidenotes.recalculateSidenotePositionsOnWindowResize");
		}, (mediaQuery) => {
			/*	Deactivate event handlers.
			 */
			GW.notificationCenter.removeHandlerForEvent("GW.hashDidChange", Sidenotes.updateStateAfterHashChange);
			GW.notificationCenter.removeHandlerForEvent("Rewrite.contentDidChange", Sidenotes.updateSidenotePositionsAfterContentDidChange);
			GW.notificationCenter.removeHandlerForEvent("Rewrite.fullWidthMediaDidLoad", Sidenotes.updateSidenotePositionsAfterFullWidthMediaDidLoad);
			GW.notificationCenter.removeHandlerForEvent("Collapse.collapseStateDidChange", Sidenotes.updateSidenotePositionsAfterCollapseStateDidChange);
			GW.notificationCenter.removeHandlerForEvent("GW.contentDidInject", Sidenotes.bindAdditionalSidenoteSlideEvents);
			removeScrollListener("Sidenotes.unSlideSidenotesOnScroll");
			removeWindowResizeListener("Sidenotes.recalculateSidenotePositionsOnWindowResize");
		});

		//	Once the sidenotes are constructed, lay them out.
		GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", (eventInfo) => {
			//	Lay out sidenotes once page layout is complete.
			doWhenPageLayoutComplete(() => {
				Sidenotes.updateSidenotePositions();

				//	Add listener to lay out sidenotes when they are re-constructed.
				GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", (eventInfo) => {
					//	Update highlighted state of sidenote and citation, if need be.
					Sidenotes.updateTargetCounterpart();

					//	Update sidenote positions.
					Sidenotes.updateSidenotePositionsIfNeeded();
				});

				/*	Add listener to lay out sidenotes when additional layout is
					done in the main document.
				 */
				GW.notificationCenter.addHandlerForEvent("Layout.layoutProcessorDidComplete", (eventInfo) => {
					//	Update sidenote positions.
					Sidenotes.updateSidenotePositionsIfNeeded();
				}, {
					condition: (info) => (   info.processorName == "applyBlockSpacingInContainer"
										  && info.container == document.main
										  && info.blockContainer.closest(".sidenote-column") == null)
				});
			});
		}, { once: true });

		/*  Construct the sidenotes whenever content is injected into the main
			page (including the initial page load).
		 */
		addContentInjectHandler(GW.contentInjectHandlers.constructSidenotesWhenMainPageContentDidInject = (eventInfo) => {
			GWLog("constructSidenotesWhenMainPageContentDidInject", "sidenotes.js", 1);

			if (eventInfo.willUpdateFootnotes == true) {
				GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (footnotesInjectEventInfo) => {
					Sidenotes.constructSidenotes(eventInfo);
				}, {
					once: true,
					condition: (info) => (   info.source == "transclude.footnotes"
										  && info.document == document
										  && (        info.loadLocation.pathname +      info.loadLocation.hash
										  	  == eventInfo.loadLocation.pathname + eventInfo.loadLocation.hash))
				});
			} else {
				Sidenotes.constructSidenotes(eventInfo);
			}
		}, ">rewrite", (info) => (   info.document == document
								  && info.container.closest(".sidenote") == null
								  && (   (   info.localize == true
								 		  && info.container.querySelector("a.footnote-ref") != null)
								 	  || info.container.closest("li.footnote") != null)));

		/*	Invalidate cached notes for the base location pathname of the 
			injected content when a sidenote loads.
		 */
		addContentInjectHandler(GW.contentInjectHandlers.invalidateNotesForCitationWhenSidenoteDidInject = (eventInfo) => {
			GWLog("invalidateNotesForCitationWhenSidenoteDidInject", "sidenotes.js", 1);

			if (eventInfo.container.querySelector(".footnote-back") != null)	
				Notes.invalidateCachedNotesForPathname(eventInfo.loadLocation.pathname);
		}, "<rewrite", (info) => (   info.document == document
								  && info.container.closest(".sidenote") != null));

		//	Fire event.
		GW.notificationCenter.fireEvent("Sidenotes.setupDidComplete");
	},

	hideInterferingUIElements: () => {
		requestAnimationFrame(() => {
			setTimeout(() => {
				//	Page toolbar.
				GW.pageToolbar?.toggleCollapseState(true);
				GW.pageToolbar?.fade();

				//	Back-to-top link.
				GW.backToTop?.classList.toggle("hidden", true)
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

		let minDistanceFromScreenEdge = Sidenotes.sidenotePadding + 1.0;

		let sidenoteRect = sidenote.getBoundingClientRect();
		if (   sidenoteRect.top >= minDistanceFromScreenEdge
			&& sidenoteRect.bottom <= window.innerHeight - minDistanceFromScreenEdge)
			return;

		let newSidenoteTop = sidenoteRect.top;
		if (toCitation) {
			let citationRect = Sidenotes.counterpart(sidenote).getBoundingClientRect()

			//	Down to citation.
			newSidenoteTop = Math.max(sidenoteRect.top, minDistanceFromScreenEdge, citationRect.top);

			//	Up to citation.
			newSidenoteTop = Math.min(newSidenoteTop + sidenoteRect.height,
									  window.innerHeight - minDistanceFromScreenEdge,
									  citationRect.top + sidenoteRect.height)
						   - sidenoteRect.height;

			//	Down to viewport top.
			newSidenoteTop = Math.max(newSidenoteTop, minDistanceFromScreenEdge);
		} else {
			//	Down to viewport top.
			newSidenoteTop = Math.max(sidenoteRect.top, minDistanceFromScreenEdge);

			//	Up to viewport bottom.
			newSidenoteTop = Math.min(newSidenoteTop + sidenoteRect.height,
									  window.innerHeight - minDistanceFromScreenEdge)
						   - sidenoteRect.height;
		}

		let delta = Math.round(newSidenoteTop - sidenoteRect.top);
		if (delta) {
			sidenote.style.transform = `translateY(${delta}px)`;
			sidenote.classList.toggle("displaced", true);
			if (Sidenotes.displacedSidenotes.includes(sidenote) == false)
				Sidenotes.displacedSidenotes.push(sidenote);
		}

		Sidenotes.updateFootnoteBackArrowOrientationForSidenote(sidenote, delta);
	},

	/*	Un-slide a slid-onto-the-screen sidenote.
	 */
	putSidenoteBack: (sidenote) => {
		GWLog("Sidenotes.putSidenoteBack", "sidenotes.js", 3);

		if (sidenote.style.transform == "none")
			return;

		let delta = sidenote.style.transform > ""
					? -1 * parseInt(sidenote.style.transform.match(/translateY\((.+?)\)/)[1])
					: 0;

		sidenote.style.transform = "";
		sidenote.classList.toggle("displaced", false);

		Sidenotes.updateFootnoteBackArrowOrientationForSidenote(sidenote, delta);
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
