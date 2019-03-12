if (typeof window.GW == "undefined")
	window.GW = { };

/********************/
/* DEBUGGING OUTPUT */
/********************/

function GWLog (string) {
	if (GW.loggingEnabled || localStorage.getItem("logging-enabled") == "true")
		console.log(string);
}
GW.enableLogging = (permanently = false) => {
	if (permanently)
		localStorage.setItem("logging-enabled", "true");
	else
		GW.loggingEnabled = true;
};
GW.disableLogging = (permanently = false) => {
	if (permanently)
		localStorage.removeItem("logging-enabled");
	else
		GW.loggingEnabled = false;
};

/***********/
/* HELPERS */
/***********/

/*	The "target counterpart" is the element associated with the target, i.e.:
	if the URL hash targets a footnote reference, its counterpart is the
	sidenote for that citation; and vice-versa, if the hash targets a sidenote,
	its counterpart is the in-text citation. We want a target counterpart to be
	highlighted along with the target itself; therefore we apply a special
	"targeted" class to the target counterpart.
	*/
function updateTargetCounterpart() {
	GWLog("updateTargetCounterpart");

	/*	Clear existing targeting.
		*/
	document.querySelectorAll(".targeted").forEach(element => {
		element.classList.remove("targeted");
	});

	/*	Identify new target counterpart, if any.
		*/
	var counterpart;
	if (location.hash.match(/#sn[0-9]/)) {
		counterpart = document.querySelector("#fnref" + location.hash.substr(3));
	} else if (location.hash.match(/#fnref[0-9]/) && window.matchMedia(GW.sidenotes.viewportWidthBreakpointMediaQuery).matches == false) {
		counterpart = document.querySelector("#sn" + location.hash.substr(6));
	}
	/*	If a target counterpart exists, mark it as such.
		*/
	if (counterpart)
		counterpart.classList.toggle("targeted", true);
}

/*	Returns true if the given element intersects the viewport, false otherwise.
	*/
function isOnScreen(element) {
	let rect = element.getBoundingClientRect();
	return (rect.top < window.innerHeight &&
			rect.bottom > 0 &&
			rect.left < window.innerWidth &&
			rect.right > 0);
}

/*	This is necessary to defeat a bug where if the page is loaded with the URL
	hash targeting some element, the element does not match the :target CSS
	pseudo-class.
	*/
function realignHashIfNeeded() {
	GWLog("realignHashIfNeeded");

	if (location.hash.match(/#sn[0-9]/) || location.hash.match(/#fnref[0-9]/))
		realignHash();
}
function realignHash() {
	GWLog("realignHash");

	var hash = location.hash;
	history.replaceState(null, null, "#");
	location.hash = hash;
}

/*	Firefox.
	*/
function ridiculousWorkaroundsForBrowsersFromBizarroWorld() {
	GWLog("ridiculousWorkaroundsForBrowsersFromBizarroWorld");

	GW.isFirefox = navigator.userAgent.toLowerCase().indexOf('firefox') > -1;
	if (!GW.isFirefox) {
		GW.sidenotes.viewportWidthBreakpointMediaQuery = `(max-width: 176ch)`;
	} else {
		let widthOfCharacterUnit = parseInt(getComputedStyle(document.body).maxWidth) / GW.maxBodyWidthInCharacterUnits;
		let viewportWidthBreakpointInPixels = 176 * widthOfCharacterUnit;

		GW.sidenotes.viewportWidthBreakpointMediaQuery = `(max-width: ${viewportWidthBreakpointInPixels}px)`
		document.querySelector("head").insertAdjacentHTML("beforeend", "<style>" + `
			@-moz-document url-prefix() {
				@media only screen and (max-width: ${viewportWidthBreakpointInPixels}px) {
					#sidenote-column-left,
					#sidenote-column-right {
						display: none;
					}
				}
				@media only screen and (min-width: ${viewportWidthBreakpointInPixels + 1}px) {
					main {
						position: relative;
						right: 4ch;
					}
				}
				@media only screen and (max-width: ${viewportWidthBreakpointInPixels}px) {
					.footnote-ref:target {
						background-color: inherit;
						box-shadow: none;
					}
				}
			}
		` + "</style>");
	}
}

/*******************/
/* COLLAPSE BLOCKS */
/*******************/

/*	Returns true if the given collapse block is currently collapsed.
	NOTE: This does not count targeted collapse blocks as expanded unless
	their disclosure button is also engaged (i.e., in the checked state).
	This is deliberate! (Because we use the disclosure button state to
	determine whether we need to recompute layout.)
	*/
function isCollapsed(collapseBlock) {
	let collapseCheckbox = collapseBlock.querySelector(".disclosure-button");
	return (collapseCheckbox.checked == false);
}

/*	Returns true if the given element is within a currently-collapsed collapse
	block.
	*/
function isWithinCollapsedBlock(element) {
	/*	If the element is not within a collapse block at all, it obviously can't
		be within a *currently-collapsed* collapse block.
		*/
	let collapseParent = element.closest(".collapse");
	if (!collapseParent) return false;

	/*	If the element is within a collapse block and that collapse block is
		currently collapsed, then the condition is satisfied...
		*/
	if (isCollapsed(collapseParent)) return true;

	/*	BUT the collapse block that the element is in, even if *it* is not
		itself collapsed, could be *within* another collapse block!
		*/
	return isWithinCollapsedBlock(collapseParent.parentElement);
}

/*	This function expands all collapse blocks containing the given element, if
	any (including the element itself, if it is a collapse block). Returns true
	if any such expansion occurred.
	*/
function expandCollapseBlocksToReveal(element) {
	GWLog("expandCollapseBlocksToReveal");

	/*	If the given element is not within any collapse block, there is nothing
		to do.
		*/
	if (!isWithinCollapsedBlock(element)) return false;

	//	Expand the nearest collapse block.
	let collapseParent = element.closest(".collapse");
	let disclosureButton = collapseParent.querySelector(".disclosure-button");
	let expansionOccurred = (disclosureButton.checked == false);
	disclosureButton.checked = true;

	//	Expand any higher-level collapse blocks!
	/*	Update sidenote positions only if we do NOT have to do any further
		expansion (otherwise we'll do redundant layout).
		*/
	if (!expandCollapseBlocksToReveal(collapseParent.parentElement) && expansionOccurred)
		setTimeout(updateSidenotePositions);

	//	Report whether we had to expand a collapse block.
	return expansionOccurred;
}

/*	This function expands all necessary collapse blocks to reveal the element
	targeted by the URL hash. (This includes expanding collapse blocks to
	reveal a footnote reference associated with a targeted sidenote).
	*/
function expandCollapseBlocksToRevealTarget() {
	GWLog("expandCollapseBlocksToRevealTarget");

	if (!location.hash) return;

	let target = location.hash.match(/#sn[0-9]/) ?
				 document.querySelector("#fnref" + location.hash.substr(3)) :
				 document.querySelector(decodeURIComponent(location.hash));
	if (!target) return;

	expandCollapseBlocksToReveal(target);
	if (!isOnScreen(target))
		target.scrollIntoView();
}

/*	Move sidenotes within currently-collapsed collapse blocks to the hidden
	sidenote storage container (#hidden-sidenote-storage). Conversely, move
	sidenotes within currently-expanded collapse blocks from the hidden sidenote
	storage container to the appropriate sidenote column.
	*/
function updateSidenotesInCollapseBlocks() {
	GWLog("updateSidenotesInCollapseBlocks");

	for (var i = 0; i < GW.sidenotes.footnoteRefs.length; i++) {
		let fnref = GW.sidenotes.footnoteRefs[i];
		let sidenote = GW.sidenotes.sidenoteDivs[i];

		//	If the enclosing collapse block is currently collapsed...
		if (isWithinCollapsedBlock(fnref)) {
			//	Move the sidenote to the hidden sidenote storage.
			GW.sidenotes.hiddenSidenoteStorage.appendChild(sidenote);
			continue;
		}

		//	Otherwise, move the sidenote back into the correct sidenote column.
		let side = (i % 2) ? GW.sidenoteColumnLeft : GW.sidenoteColumnRight;
		//	What's the next sidenote?
		var nextSidenoteIndex = i + 2;
		while (nextSidenoteIndex < GW.sidenotes.footnoteRefs.length &&
			   GW.sidenotes.sidenoteDivs[nextSidenoteIndex].parentElement == GW.sidenotes.hiddenSidenoteStorage)
			   nextSidenoteIndex += 2;
		if (nextSidenoteIndex >= GW.sidenotes.footnoteRefs.length) {
		/*	If no subsequent sidenote is displayed, append the current sidenote
			to the column.
			*/
			side.appendChild(sidenote);
		} else {
		/*	Otherwise, insert it before the next displayed sidenote.
			*/
			side.insertBefore(sidenote, GW.sidenotes.sidenoteDivs[nextSidenoteIndex]);
		}
	}
}

/***************************/
/* FOOTNOTES VS. SIDENOTES */
/***************************/

/*	In footnote mode (i.e., on viewports too narrow to support sidenotes),
	footnote reference links (i.e., citations) should point down to footnotes.
	But in sidenote mode, footnote reference links should point to sidenotes.
	This function rewrites all footnote reference links appropriately to the
	current mode (based on viewport width).
	*/
function updateFootnoteReferenceLinks() {
	GWLog("updateFootnoteReferenceLinks");

	for (var i = 0; i < GW.sidenotes.footnoteRefs.length; i++) {
		let fnref = GW.sidenotes.footnoteRefs[i];
		if (window.matchMedia(GW.sidenotes.viewportWidthBreakpointMediaQuery).matches == false) {
			fnref.href = "#sn" + (i + 1);
		} else {
			fnref.href = "#fn" + (i + 1);
		}
	}
}

/*	Bind event listeners for the footnote popups or the sidenotes, as
	appropriate for the current viewport width; unbind the others.
	*/
function updateFootnoteEventListeners() {
	GWLog("updateFootnoteEventListeners");

	/*	Determine whether we are in sidenote mode or footnote mode.
		*/
	var sidenotes = (window.matchMedia(GW.sidenotes.viewportWidthBreakpointMediaQuery).matches == false);

	/*	Get all footnote links.
		(We use jQuery for this because footnotes.js is written with jQuery and
		otherwise bind()/unbind() don't work.)
		*/
	var footnotelinks = jQuery('.footnote-ref')

	if (sidenotes) {
		//	Unbind footnote mouse events.
		footnotelinks.unbind('mouseover', Footnotes.footnoteover);
		footnotelinks.unbind('mouseout', Footnotes.footnoteoout);
		//	Bind sidenote mouse events.
		for (var i = 0; i < GW.sidenotes.footnoteRefs.length; i++) {
			let fnref = GW.sidenotes.footnoteRefs[i];
			let sidenote = GW.sidenotes.sidenoteDivs[i];

			fnref.addEventListener("mouseover", GW.sidenotes.footnoteover = () => {
				sidenote.classList.toggle("highlighted", true);
			});
			fnref.addEventListener("mouseout", GW.sidenotes.footnoteout = () => {
				sidenote.classList.remove("highlighted");
			});
			sidenote.addEventListener("mouseover", GW.sidenotes.sidenoteover = () => {
				fnref.classList.toggle("highlighted", true);
			});
			sidenote.addEventListener("mouseout", GW.sidenotes.sidenoteout = () => {
				fnref.classList.remove("highlighted");
			});
		}
	} else {
		//	Unbind sidenote mouse events.
		for (var i = 0; i < GW.sidenotes.footnoteRefs.length; i++) {
			let fnref = GW.sidenotes.footnoteRefs[i];
			let sidenote = GW.sidenotes.sidenoteDivs[i];

			fnref.removeEventListener("mouseover", GW.sidenotes.footnoteover);
			fnref.removeEventListener("mouseout", GW.sidenotes.footnoteout);
			sidenote.removeEventListener("mouseover", GW.sidenotes.sidenoteover);
			sidenote.removeEventListener("mouseout", GW.sidenotes.sidenoteout);
		}
		//	Bind footnote mouse events.
		footnotelinks.bind('mouseover', Footnotes.footnoteover);
		footnotelinks.bind('mouseout', Footnotes.footnoteoout);
	}
}

/*	In some rare cases, we might switch to sidenote mode while a footnote popup
	is on the screen. Since we remove footnote popup event listeners during the
	switch, that popup will remain there forever... unless we clean it up.
	*/
function clearFootnotePopups() {
	GWLog("clearFootnotePopups");

	document.querySelectorAll("#footnotediv").forEach(footnotePopup => { footnotePopup.remove(); });
}

/**********/
/* LAYOUT */
/**********/

/*	This function actually calculates and sets the positions of all sidenotes.
	*/
function updateSidenotePositions() {
	GWLog("updateSidenotePositions");

	/*	If we're in footnotes mode (i.e., the viewport is too narrow), then
		don't do anything.
		*/
	if (window.matchMedia(GW.sidenotes.viewportWidthBreakpointMediaQuery).matches == true)
		return;

	//	Update the disposition of sidenotes within collapse blocks.
	updateSidenotesInCollapseBlocks();

	for (var i = 0; i < GW.sidenotes.footnoteRefs.length; i++) {
		let sidenote = GW.sidenotes.sidenoteDivs[i];

		/*	Check whether the sidenote is in the hidden sidenote storage (i.e.,
			within a currently-collapsed collapse block. If so, skip it.
			*/
		if (sidenote.parentElement == GW.sidenotes.hiddenSidenoteStorage)
			continue;

		//	What side is this sidenote on?
		let side = (i % 2) ? GW.sidenoteColumnLeft : GW.sidenoteColumnRight;

		//	Default position (vertically aligned with the footnote reference).
		sidenote.style.top = Math.round(((GW.sidenotes.footnoteRefs[i].getBoundingClientRect().top) - side.getBoundingClientRect().top) + 4) + "px";

		/*	Mark sidenotes which are cut off vertically.
			*/
		let sidenoteOuterWrapper = sidenote.firstElementChild;
		sidenote.classList.toggle("cut-off", (sidenoteOuterWrapper.scrollHeight > sidenoteOuterWrapper.clientHeight + 2));
	}

	/*	Correct for overlap.
		*/
	for (var i = 0; i < GW.sidenotes.footnoteRefs.length; i++) {
		let sidenote = GW.sidenotes.sidenoteDivs[i];
		let nextSidenote = sidenote.nextElementSibling;

		/*	Is this sidenote even displayed? Or is it hidden (i.e., within
			a currently-collapsed collapse block)? If so, skip it.
			*/
		if (sidenote.parentElement == GW.sidenotes.hiddenSidenoteStorage) continue;

		//	Is there a sidenote below this one?
		if (!nextSidenote) continue;

		//	Is there any overlap between them?
		var overlap = (sidenote.getBoundingClientRect().top + sidenote.clientHeight + GW.sidenotes.sidenoteSpacing) - nextSidenote.getBoundingClientRect().top;
		/*	If the sidenote is cut off (due to being too tall), compensate
			(to maintain visual consistency).
			*/
		/*	If there's no overlap, then this sidenote's position needs no
			adjustment.
			*/
		if (overlap <= 0) continue;

		//	At this point, we know there is overlap.
		GWLog(`Sidenote ${sidenote.id.substr(3)} overlaps sidenote ${nextSidenote.id.substr(3)}!`);

		/*	Figure out how much vertical space above we have; if there's enough
			"headroom", we can simply move the current sidenote up.
			*/
		let previousSidenote = sidenote.previousElementSibling;
		let headroom = previousSidenote ?
					   sidenote.getBoundingClientRect().top - (previousSidenote.getBoundingClientRect().top + previousSidenote.clientHeight + GW.sidenotes.sidenoteSpacing) :
					   sidenote.getBoundingClientRect().top - sidenote.parentElement.getBoundingClientRect().top;
		GWLog(`We have ${headroom}px of headroom.`);

		//	If we have enough headroom, simply move the sidenote up.
		if (headroom > overlap) {
			GWLog(`There is enough headroom. Moving sidenote ${sidenote.id.substr(3)} up.`);
			sidenote.style.top = (parseInt(sidenote.style.top) - overlap) + "px";
			continue;
		} else {
			//	We don't have enough headroom!
			GWLog(`There is not enough headroom to move sidenote ${sidenote.id.substr(3)} all the way up!`);
			//	Move the sidenote up as much as we can...
			GWLog(`Moving sidenote ${sidenote.id.substr(3)} up by ${headroom} pixels...`);
			sidenote.style.top = (parseInt(sidenote.style.top) - headroom) + "px";
			//	Recompute overlap...
			overlap -= headroom;
			/*	And move the next sidenote down - possibly causing overlap.
				(But this will be handled when we process the next sidenote.)
				*/
			GWLog(`... and moving sidenote ${nextSidenote.id.substr(3)} down by ${overlap} pixels.`);
			nextSidenote.style.top = (parseInt(nextSidenote.style.top) + overlap) + "px";
		}
	}
}

/******************/
/* INITIALIZATION */
/******************/

/*	Q:	Why is this setup function so long and complex?
	A:	In order to properly handle all of the following:

	1.	The two different modes (footnote popups vs. sidenotes)
	2.	The interactions between sidenotes and collapse blocks
	3.	Linking to footnotes/sidenotes
	4.	Loading a URL that links to a footnote/sidenote
	5.	Disclosing too-long sidenotes (and otherwise interacting with sidenotes)
	6.	Changes in the viewport width dynamically altering all of the above

	… and, of course, correct layout of the sidenotes, even in tricky cases
	where the citations are densely packed and the sidenotes are long.
	*/
function sidenotesSetup() {
	GWLog("sidenotesSetup");

	/*	The `sidenoteSpacing` constant defines the minimum vertical space that
		is permitted between adjacent sidenotes; any less, and they are
		considered to be overlapping.
		*/
	GW.sidenotes = {
		sidenoteSpacing:	60
	};
	/*	This should match the "max-width" property of the "body" element.
		*/
	GW.maxBodyWidthInCharacterUnits = 112;

	//	Compensate for Firefox nonsense.
	ridiculousWorkaroundsForBrowsersFromBizarroWorld();

	/*	Add the sidenote columns.
		*/
	document.querySelector("#markdownBody").insertAdjacentHTML("beforeend",
		"<div id='sidenote-column-left' class='footnotes'></div>" +
		"<div id='sidenote-column-right' class='footnotes'></div>");
	GW.sidenoteColumnLeft = document.querySelector("#sidenote-column-left");
	GW.sidenoteColumnRight = document.querySelector("#sidenote-column-right");

	/*	Position left sidenote column so top is flush with top of first
		full-width block (i.e., one that is not pushed right by the TOC).

		NOTE: This doesn't quite do what it says (due to overflow), but that's
		fine; nothing really breaks as a result...
		*/
	let markdownBody = document.querySelector("#markdownBody");
	var firstFullWidthBlock;
	for (var block of markdownBody.children) {
		if (block.clientWidth == markdownBody.clientWidth) {
			firstFullWidthBlock = block;
			break;
		}
	}
	let offset = firstFullWidthBlock.offsetTop;
	if (GW.sidenoteColumnLeft.offsetTop < firstFullWidthBlock.offsetTop) {
		GW.sidenoteColumnLeft.style.top = offset + "px";
		GW.sidenoteColumnLeft.style.height = `calc(100% - ${offset}px)`;
	}

	/*	Create and inject the sidenotes.
		*/
	GW.sidenotes.sidenoteDivs = [ ];
	//	The footnote references (citations).
	GW.sidenotes.footnoteRefs = Array.from(document.querySelectorAll("a.footnote-ref"));
	for (var i = 0; i < GW.sidenotes.footnoteRefs.length; i++) {
		//	Create the sidenote outer containing block...
		let sidenote = document.createElement("div");
		sidenote.classList.add("sidenote");
		sidenote.id = "sn" + (i + 1);
		//	Wrap the contents of the footnote in two wrapper divs...
		sidenote.innerHTML = "<div class='sidenote-outer-wrapper'><div class='sidenote-inner-wrapper'>" + document.querySelector(GW.sidenotes.footnoteRefs[i].hash).innerHTML + "</div></div>";
		//	Add the sidenote to the sidenotes array...
		GW.sidenotes.sidenoteDivs.push(sidenote);
		//	On which side should the sidenote go? Odd - right; even - left.
		let side = (i % 2) ? GW.sidenoteColumnLeft : GW.sidenoteColumnRight;
		//	Inject the sidenote into the page.
		side.appendChild(sidenote);
	}

	/*	Create & inject the sidenote self-links (i.e., boxed sidenote numbers).
		*/
	for (var i = 0; i < GW.sidenotes.footnoteRefs.length; i++) {
		let sidenoteSelfLink = document.createElement("a");
		sidenoteSelfLink.classList.add("sidenote-self-link");
		sidenoteSelfLink.href = "#sn" + (i + 1);
		sidenoteSelfLink.textContent = (i + 1);
		GW.sidenotes.sidenoteDivs[i].appendChild(sidenoteSelfLink);
	}

	/*	Create & inject the hidden sidenote storage (for sidenotes within
		currently-collapsed collapse blocks).
		*/
	GW.sidenotes.hiddenSidenoteStorage = document.createElement("div");
	GW.sidenotes.hiddenSidenoteStorage.id = "hidden-sidenote-storage";
	GW.sidenotes.hiddenSidenoteStorage.style.display = "none";
	markdownBody.appendChild(GW.sidenotes.hiddenSidenoteStorage);

	/*	Add listeners to target a sidenote when clicked.
		*/
	for (var i = 0; i < GW.sidenotes.footnoteRefs.length; i++) {
		let sidenote = GW.sidenotes.sidenoteDivs[i];
		sidenote.addEventListener("click", GW.sidenotes.sidenoteClicked = (event) => {
			GWLog("GW.sidenotes.sidenoteClicked");

			if (decodeURIComponent(location.hash) == sidenote.id) return;

			// Make sure clicking a sidenote does not cause scrolling.
			let scrollPositionBeforeNavigate = window.scrollY;
			location.hash = encodeURIComponent(sidenote.id);
			window.scrollTo(0, scrollPositionBeforeNavigate);
		});
	}

	/*	Insert zero-width spaces after problematic characters in sidenotes.
		(This is to mitigate justification/wrapping problems.)
		*/
	GW.sidenotes.problematicCharacters = '/=≠';
	GW.sidenotes.sidenoteDivs.forEach(sidenote => {
		sidenote.querySelectorAll("*").forEach(element => {
			if (element.closest(".sourceCode")) return;
			element.childNodes.forEach(node => {
				if (node.childNodes.length > 0) return;
				node.textContent = node.textContent.replace(new RegExp("(\\w[" + GW.sidenotes.problematicCharacters + "])(\\w)", 'g'), "$1\u{200B}$2");
			});
		});
	});

	/*	Add a resize listener so that sidenote positions are recalculated when
		the window is resized.
		*/
	window.addEventListener('resize', GW.sidenotes.windowResized = (event) => {
		GWLog("GW.sidenotes.windowResized");

		updateSidenotePositions();
	});
	/*	Lay out the sidenotes as soon as the document is loaded.
		*/
	if (document.readyState == "complete") {
		updateSidenotePositions();
	} else {
		window.addEventListener("load", updateSidenotePositions);
	}

	/*	Listen for changes to whether the viewport width media query is matched;
		if such a change occurs (i.e., if the viewport becomes, or stops being,
		wide enough to support sidenotes), switch modes from footnote popups to
		sidenotes or vice/versa, as appropriate.
		(This listener may also be fired if the dev tools pane is opened, etc.)
		*/
	window.matchMedia(GW.sidenotes.viewportWidthBreakpointMediaQuery).addListener(GW.sidenotes.viewportWidthBreakpointChanged = () => {
		GWLog("GW.sidenotes.viewportWidthBreakpointChanged");

		updateFootnoteEventListeners();
		updateFootnoteReferenceLinks();
		clearFootnotePopups();
	});
	/*	Immediately set the correct mode (footnote popups or sidenotes), and
		rewrite the citation (footnote reference) links to point to footnotes
		or to sidenotes, as appropriate.
		*/
	window.addEventListener("load", () => {
		updateFootnoteEventListeners();
		updateFootnoteReferenceLinks();
	});

	/*	If the page was loaded with a hash that points to a footnote, but
		sidenotes are enabled (or vice-versa), rewrite the hash in accordance
		with the current mode (this will also cause the page to end up scrolled
		to the appropriate element - footnote or sidenote).
		*/
	if (location.hash.match(/#sn[0-9]/) &&
		window.matchMedia(GW.sidenotes.viewportWidthBreakpointMediaQuery).matches == true) {
		location.hash = "#fn" + location.hash.substr(3);
	} else if (location.hash.match(/#fn[0-9]/) &&
		window.matchMedia(GW.sidenotes.viewportWidthBreakpointMediaQuery).matches == false) {
		location.hash = "#sn" + location.hash.substr(3);
	} else {
		/*	Otherwise, make sure that if a sidenote is targeted by the hash, it
			indeed ends up looking highlighted (this defeats a weird bug).
			*/
		requestIdleCallback(realignHashIfNeeded);
	}

	/*	Having updated the hash, now properly highlight everything, if needed,
		and add a listener to update the target counterpart if the hash changes
		later.

		Also, if the hash points to a collapse block, or to an element within a
		collapse block, expand it and all collapse blocks enclosing it.
		*/
	window.addEventListener("hashchange", GW.sidenotes.hashChanged = () => {
		GWLog("GW.sidenotes.hashChanged");

		expandCollapseBlocksToRevealTarget();
		updateTargetCounterpart();
	});
	window.addEventListener("load", () => {
		expandCollapseBlocksToRevealTarget();
		updateTargetCounterpart();
	});

	/*	Add event listeners to (asynchronously) recompute sidenote positioning
		when a collapse block is manually collapsed or expanded.
		*/
	document.querySelectorAll(".disclosure-button").forEach(collapseCheckbox => {
		collapseCheckbox.addEventListener("change", GW.sidenotes.disclosureButtonValueChanged = (event) => {
			GWLog("GW.sidenotes.disclosureButtonValueChanged");

			setTimeout(updateSidenotePositions);
		});
	});
}

//	LET... THERE... BE... SIDENOTES!!!
sidenotesSetup();
