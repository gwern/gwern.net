/*************************/
/*	Configuration / state.
 */
GW.collapse = {
	/*	Visibility of block collapse labels depends on how many times the user 
		has used them already.
	 */
	alwaysShowCollapseInteractionHints: (getSavedCount("clicked-to-expand-collapse-block-count") < (GW.isMobile() ? 6 : 3)),
	showCollapseInteractionHintsOnHover: (   GW.isMobile() == false 
										  && getSavedCount("clicked-to-expand-collapse-block-count") < 6),

	/*	Hover events (see below).
	 */
	hoverEventsEnabled: (GW.isMobile() == false),
	hoverEventsActive: (GW.isMobile() == false)
};

/****************************************************************************/
/*	On desktop, disable hover events on scroll; re-enable them on mouse move.
 */
if (GW.collapse.hoverEventsEnabled) {
	//	Disable on scroll.
	addScrollListener(GW.collapse.disableCollapseHoverEventsOnScroll = (event) => {
		GW.collapse.hoverEventsActive = false;
	}, "disableCollapseHoverEventsOnScrollListener");

	/*	Add event handler to add scroll listener to spawned popups, to
		disable hover events when scrolling within a popup.
	 */
	GW.notificationCenter.addHandlerForEvent("Popups.popupDidSpawn", GW.collapse.addDisableHoverEventsOnScrollListenerOnPopupSpawned = (info) => {
		addScrollListener(GW.collapse.disableCollapseHoverEventsOnScroll, null, null, info.popup.scrollView);
	});

	//	Enable on mousemove.
	window.addEventListener("mousemove", GW.collapse.windowMouseMove = (event) => {
		GW.collapse.hoverEventsActive = true;
	});
}

/*******************************************************************************/
/*  This function expands all collapse blocks containing the given node, if
    any (including the node itself, if it is a collapse block). Returns true
    if any such expansion occurred. Fires Collapse.collapseStateDidChange event
    after all (possibly recursive) expansion is completed. (Only one event fired
    per non-recursive call to expandCollapseBlocksToReveal(), even if recursive
    expansion occurred.)
 */
function expandCollapseBlocksToReveal(node, fireStateChangedEvent = true) {
    GWLog("expandCollapseBlocksToReveal", "collapse.js", 2);

	if (!node)
		return;

    // If the node is not an element (e.g. a text node), get its parent element.
    let element = node instanceof HTMLElement ? node : node.parentElement;

    /*  If the given element is not within any collapsed block, there is nothing
        to do.
     */
    if (!isWithinCollapsedBlock(element))
    	return false;

    //  Determine if nearest collapse block needs expanding.
    let collapseBlock = element.closest(".collapse");
    let expand = isCollapsed(collapseBlock);

    /*  Expand any higher-level collapse blocks.
		Fire state change event only if we will not have to expand this block
		(otherwise we’ll do redundant layout).
     */
	let expandedAncestor = expandCollapseBlocksToReveal(collapseBlock.parentElement, expand == false);

    if (expand) {
		//	Expand nearest collapse block.
		toggleCollapseBlockState(collapseBlock, expand);

		/*	Fire state change event only if we will not have to do any more 
			expansion (otherwise we’ll do redundant layout).
		 */
		if (fireStateChangedEvent) {
			GW.notificationCenter.fireEvent("Collapse.collapseStateDidChange", {
				source: "expandCollapseBlocksToReveal",
				collapseBlock: collapseBlock
			});
		}
	}

    //  Report whether we had to expand a collapse block.
    return (expand || expandedAncestor);
}

/*******************************************************************************/
/*	This function collapses the specified collapse block and all collapse blocks
	nested within it, if any. Fires Collapse.collapseStateDidChange event after
	all (possibly recursive) collapsing is completed. (Only one event fired per
	non-recursive call to collapseCollapseBlock(), even if recursive collapsing 
	occurred.)
 */
function collapseCollapseBlock(collapseBlock, fireEvent = true) {
    GWLog("collapseCollapseBlock", "collapse.js", 2);

	if (isCollapsed(collapseBlock))
		return;

	/*	Collapse any nested collapse blocks. Fire no state change events when
		doing so; we will fire a single event, once we’ve collapsed the 
		specified collapse block, after all of its nested collapse blocks are 
		collapsed.
	 */
	collapseBlock.querySelectorAll(".collapse").forEach(nestedCollapseBlock => {
		collapseCollapseBlock(nestedCollapseBlock, false);
	});

	//	Collapse block.
	toggleCollapseBlockState(collapseBlock, false);

	//	Fire event, if need be.
	if (fireEvent) {
    	GW.notificationCenter.fireEvent("Collapse.collapseStateDidChange", {
    		source: "collapseCollapseBlock",
    		collapseBlock: collapseBlock
    	});
	}
}

/*******************************************************************/
/*  Returns true if the given collapse block is currently collapsed.
 */
function isCollapsed(collapseBlock) {
	if (Array.from(collapseBlock.children).findIndex(child => child.classList.contains("collapse-content-wrapper")) === -1)
		return false;

    return (collapseBlock.classList.contains("expanded-not"));
}

/*****************************************************************************/
/*  Returns true if the given element is within a currently-collapsed collapse
    block.
 */
function isWithinCollapsedBlock(element) {
    /*  If the element is not within a collapse block at all, it obviously can't
        be within a *currently-collapsed* collapse block.
     */
    let collapseParent = element.closest(".collapse");
    if (!collapseParent)
    	return false;

    /*  If the element is within a collapse block and that collapse block is
        currently collapsed, then the condition is satisfied...
     */
    if (isCollapsed(collapseParent))
    	return true;

    /*  BUT the collapse block that the element is in, even if *it* is not
        itself collapsed, could be *within* another collapse block!
     */
    return isWithinCollapsedBlock(collapseParent.parentElement);
}

/************************************************************************/
/*	Returns true iff element’s immediate children include any block-level 
	elements.
 */
function containsBlockChildren(element) {
	for (child of element.children) {
		if ([ "DIV", "P", "UL", "LI", "SECTION", "BLOCKQUOTE", "FIGURE" ].includes(child.tagName))
			return true;
		if (   child.tagName == "A"
			&& Transclude.isIncludeLink(child))
			return true;
	}

	return false
}

/***************************************************************************/
/*	Constructs and returns a disclosure button (for block-level collapses by
	default; to get an inline button, pass `false`).
 */
function newDisclosureButton(block = true, start = true) {
	let className = "disclosure-button" + (block ? "" : (" " + (start ? "start" : "end")));
	let disclosureButtonHTML = `<button type="button" class="${className}" tabindex="-1" aria-label="Open/close collapsed section">`;
	if (block) {
		disclosureButtonHTML += `<span class="part top">`
								 + `<span class="label"></span>`
								 + `<span class="icon">`
									+ GW.svg("chevron-left-solid")
								 + `</span>`
							  + `</span>`
							  + `<span class="part bottom">`
							  	 + `<span class="label"></span>`
								 + `<span class="icon">`
									+ GW.svg("chevron-left-solid")
								 + `</span>`
							  + `</span>`;
	} else {
		disclosureButtonHTML += `<span class="icon">`
							  + (start
								 ? GW.svg("bracket-square-left-sharp-light")
								 : (  GW.svg("angle-right-regular")
									+ GW.svg("bracket-square-right-sharp-light")))
							  + `</span>`;
	}
	disclosureButtonHTML += `</button>`;

	return elementFromHTML(disclosureButtonHTML);
}

/***********************************************************************/
/*  Inject disclosure buttons and otherwise prepare the collapse blocks.
 */
addContentLoadHandler(GW.contentLoadHandlers.prepareCollapseBlocks = (eventInfo) => {
	GWLog("prepareCollapseBlocks", "collapse.js", 1);

	//  Construct all collapse blocks (in correct final state).
	eventInfo.container.querySelectorAll(".collapse").forEach(collapseBlock => {
		//	Compensate for Pandoc putting .collapse class on headings.
		if ([ "H1", "H2", "H3", "H4", "H5", "H6" ].includes(collapseBlock.tagName)) {
			collapseBlock.classList.remove("collapse");
			if (collapseBlock.className == "")
				collapseBlock.removeAttribute("class");

			return;
		}

		let startExpanded = (collapseBlock.contains(getHashTargetedElement()) == true);

		if (GW.collapse.hoverEventsEnabled)
			collapseBlock.classList.add("expand-on-hover");

		let collapseWrapper;
		if ([ "DIV", "SECTION", "SPAN" ].includes(collapseBlock.tagName)) {
			//	No additional wrapper needed for these tag types.
			collapseWrapper = collapseBlock;

			/*	Rewrap spans that are NOT inline collapses (i.e., those that
				are, for some reason, wrapping block-level content).
			 */
			if (   collapseWrapper.tagName == "SPAN"
				&& containsBlockChildren(collapseWrapper))
				collapseWrapper = rewrapContents(collapseWrapper, null, "DIV", true, true);

			//	Designate collapse type (block or inline).
			if ([ "SPAN" ].includes(collapseWrapper.tagName))
				collapseWrapper.classList.add("collapse-inline");
			else
				collapseWrapper.classList.add("collapse-block");

			/*	Abstracts (the .abstract class) can end up in collapses
				without this being known in advance, so may not have the
				.abstract-collapse class, as they should.
			 */
			let collapseAbstract = collapseWrapper.querySelector(".collapse > .abstract");
			if (collapseAbstract?.closest(".collapse") == collapseWrapper)
				collapseAbstract.classList.add("abstract-collapse");

			//	Ensure correct structure and classes of abstracts.
			collapseAbstract = collapseWrapper.querySelector(".collapse > .abstract-collapse");
			if (collapseAbstract?.closest(".collapse") == collapseWrapper) {
				//	Mark those collapse blocks that have abstracts.
				collapseWrapper.classList.add("has-abstract");

				//	Wrap bare text nodes and inline elements in <p> elements.
				if (collapseWrapper.classList.contains("collapse-block"))
					paragraphizeTextNodesOfElement(collapseAbstract);

				//	Make sure “real” abstracts are marked as such.
				if (   collapseWrapper.classList.contains("collapse-block")
					&& collapseAbstract.firstElementChild?.tagName == "BLOCKQUOTE")
					collapseAbstract.classList.add("abstract");
			} else {
				//	Mark those collapse blocks that have no abstracts.
				collapseWrapper.classList.add("no-abstract");
			}

			//	Designate “bare content” collapse blocks.
			if (collapseWrapper.classList.contains("collapse-block")) {
				let bareContentTags = [ "P", "UL", "OL" ];
				if (   bareContentTags.includes(collapseWrapper.firstElementChild.tagName)
					|| (   collapseWrapper.classList.contains("has-abstract")
						&& bareContentTags.includes(collapseWrapper.firstElementChild.firstElementChild.tagName)))
					collapseWrapper.classList.add("bare-content");
			}
		} else {
			//	Additional wrapper is required for most tag types.
			collapseWrapper = wrapElement(collapseBlock, null, "DIV", true, [ "collapse", "expand-on-hover" ]);

			//	This is a block collapse.
			collapseWrapper.classList.add("collapse-block");

			//	Collapse blocks of this type never have abstracts.
			collapseWrapper.classList.add("no-abstract");
		}

		//	Mark as expanded, if need be.
		collapseWrapper.swapClasses([ "expanded", "expanded-not" ], startExpanded ? 0 : 1)

		//  Inject the disclosure button.
		if (collapseWrapper.classList.contains("collapse-inline")) {
			//	Button at start.
			collapseWrapper.insertBefore(newDisclosureButton(false), collapseWrapper.firstChild);

			//	Button at end.
			collapseWrapper.insertBefore(newDisclosureButton(false, false), null);
		} else if ([ "SECTION" ].includes(collapseWrapper.tagName)) {
			collapseWrapper.insertBefore(newDisclosureButton(), collapseWrapper.firstElementChild.nextElementSibling);
		} else {
			collapseWrapper.insertBefore(newDisclosureButton(), collapseWrapper.firstChild);
		}

		//	Slight HTML structure rectification.
		if (   collapseWrapper.parentElement
			&& [ "P" ].includes(collapseWrapper.parentElement.tagName) == true
			&& [ "SPAN" ].includes(collapseWrapper.tagName) == false
			&& isOnlyChild(collapseWrapper))
			unwrap(collapseWrapper.parentElement);

		//	Construct collapse content wrapper.
		let collapseContentWrapperTagName = collapseWrapper.tagName == "SPAN" ? "SPAN" : "DIV";
		let collapseContentWrapper = newElement(collapseContentWrapperTagName, { "class": "collapse-content-wrapper" });
		let childNodesArray = Array.from(collapseWrapper.childNodes);
		collapseContentWrapper.append(...childNodesArray.slice(childNodesArray.findLastIndex(node => {
			return (   node instanceof Element 
					&& node.matches(".disclosure-button:not(.end), .abstract-collapse"));
		}) + 1));
		collapseWrapper.append(collapseContentWrapper);
		/*	Move the auxiliary (closing) disclosure button of an inline 
			collapse back to its proper place.
		 */
		if ([ "SPAN" ].includes(collapseWrapper.tagName))
			collapseWrapper.append(collapseContentWrapper.lastElementChild);

		//	Fire event.
		if (startExpanded) {
			GW.notificationCenter.fireEvent("Collapse.collapseStateDidChange", {
				source: "prepareCollapseBlocks",
				collapseBlock: collapseWrapper
			});
		}
	});
}, "rewrite");

/*****************************************************************************/
/*	Ensure that top part of disclosure button (including chevron icon) matches
	height of section heading text, for section collapses.
 */
addContentInjectHandler(GW.contentInjectHandlers.rectifySectionCollapseLayout = (eventInfo) => {
	GWLog("rectifySectionCollapseLayout", "collapse.js", 1);

	eventInfo.container.querySelectorAll("section.collapse").forEach(section => {
		section.style.removeProperty("--collapse-toggle-top-height");
		section.style.removeProperty("--collapse-toggle-top-icon-size");

		requestIdleCallback(() => {
			let rects = Array.from(section.firstElementChild.querySelector("a").getClientRects());
			let oneLineHeight = rects.first?.height ?? 0;
			let totalHeight = rects.reduce((h, r) => h + r.height, 0);
			if (   oneLineHeight == 0
				|| totalHeight == 0)
				return;

			section.style.setProperty("--collapse-toggle-top-height", Math.round(totalHeight + oneLineHeight * 0.15) + "px");
			section.style.setProperty("--collapse-toggle-top-icon-size", Math.round(oneLineHeight * 1.15) + "px");
		});
	});
}, ">rewrite");

/******************************************************************************/
/*  Collapse all expanded collapse blocks. (Mostly relevant when popping up
	sections of an already-displayed full page, which may have collapses in it,
	which have already been expanded, but which we do not want to be expanded
	when the sections containing them appear in a new context.)
 */
addContentInjectHandler(GW.contentInjectHandlers.collapseExpandedCollapseBlocks = (eventInfo) => {
	GWLog("collapseExpandedCollapseBlocks", "collapse.js", 1);

	eventInfo.container.querySelectorAll(".collapse.expanded").forEach(collapseCollapseBlock);
}, "<eventListeners");

/********************************************************/
/*	Updates disclosure button label for current UI state.
 */
function updateDisclosureButtonState(collapseBlock, showLabels) {
	GWLog("updateDisclosureButtonState", "collapse.js", 2);

	let action = GW.isMobile() ? "Tap" : "Click";
	let labelHTML = isCollapsed(collapseBlock)
					? `${action} to expand`
					: `${action} to collapse`;

	if (collapseBlock.classList.contains("collapse-block")) {
		let disclosureButton = collapseBlock.querySelector(".disclosure-button");

		disclosureButton.querySelectorAll(".part .label").forEach(label => {
			label.innerHTML = labelHTML;
		});

		disclosureButton.classList.toggle("labels-visible", showLabels || GW.collapse.alwaysShowCollapseInteractionHints);
	} else {
		[ collapseBlock.firstElementChild, collapseBlock.lastElementChild ].forEach(disclosureButton => {
			disclosureButton.title = labelHTML;
		});
	}
}

/***************************************/
/*	Expand or collapse a collapse block.
 */
function toggleCollapseBlockState(collapseBlock, expanding) {
	//	Set proper classes.
	collapseBlock.swapClasses([ "expanded", "expanded-not" ], expanding ? 0 : 1);

	//	Update label text and other HTML-based UI state.
	updateDisclosureButtonState(collapseBlock, GW.collapse.showCollapseInteractionHintsOnHover);

	/*	Compensate for block indentation due to nesting (e.g., lists).

		(Don’t do this for full-width collapses, as the full-width code will
		 already apply suitable side margins.)

		(Also don’t do this for collapses in blockquotes, which get treated
		 specially.)
	 */
	if (   collapseBlock.classList.contains("collapse-block")
		&& collapseBlock.closest("blockquote") == null
		&& collapseBlock.querySelector(".collapse-content-wrapper").classList.contains("width-full") == false) {
		if (expanding) {
			let contentRect = collapseBlock.querySelector(".collapse-content-wrapper").getBoundingClientRect();
			let enclosingContentRect = collapseBlock.closest(".markdownBody").getBoundingClientRect();
			let offset = getComputedStyle(collapseBlock).getPropertyValue("--collapse-left-offset");

			collapseBlock.style.marginLeft = `calc(${(enclosingContentRect.x - contentRect.x)}px - ${offset})`;
		} else { // if (collapsing)
			collapseBlock.style.marginLeft = "";
		}
	}
}

/*************************************************/
/*  Add event listeners to the disclosure buttons.
 */
addContentInjectHandler(GW.contentInjectHandlers.activateCollapseBlockDisclosureButtons = (eventInfo) => {
	GWLog("activateCollapseBlockDisclosureButtons", "collapse.js", 1);

    //  Add listeners to collapse block disclosure buttons.
	eventInfo.container.querySelectorAll(".disclosure-button").forEach(disclosureButton => {
		if (disclosureButton.actionHandler)
			return;

		let collapseBlock = disclosureButton.closest(".collapse");

		updateDisclosureButtonState(collapseBlock);

		disclosureButton.addActivateEvent(disclosureButton.actionHandler = (event) => {
			GWLog("Collapse.collapseBlockDisclosureButtonActivated", "collapse.js", 2);

			//	Nullify accidental late clicks in block collapses.
			if (   collapseBlock.classList.contains("collapse-block")
				&& collapseBlock.classList.contains("just-auto-expanded"))
				return;

			//	Expanding? Collapsing? (For readability and consistency.)
			let expanding = isCollapsed(collapseBlock);
			let collapsing = (expanding == false);

			//	Keep count of clicks to uncollapse.
			if (   expanding
				&& collapseBlock.classList.contains("collapse-block")
				&& event.type == "click")
				incrementSavedCount("clicked-to-expand-collapse-block-count");

			//	Expand or collapse.
			toggleCollapseBlockState(collapseBlock, expanding);

			/*	If a collapse block was collapsed from the bottom, it might now
				be up off the screen. Scroll it into view.
			 */
			if (   collapsing
				&& isOnScreen(collapseBlock) == false)
				scrollElementIntoView(collapseBlock);
			/*	If a collapse block was expanded from the bottom, the top of the
				collapse block might be up off the screen. Scroll it into view.
			 */
			else if (   expanding
					 && collapseBlock.getBoundingClientRect().top < 0)
				scrollElementIntoView(collapseBlock);

			//	Update temporary state.
			if (   collapseBlock.classList.contains("expand-on-hover")
				&& GW.collapse.hoverEventsEnabled) {
				let tempClass = null;
				switch (event.type) {
				case "click":
					tempClass = "just-clicked"; break;
				case "mouseenter":
					tempClass = "just-auto-expanded"; break;
				}
				if (tempClass) {
					collapseBlock.classList.add(tempClass);
					collapseBlock.addEventListener("mouseleave", (event) => {
						collapseBlock.classList.remove(tempClass);
					}, { once: true });
				}
			}

			GW.notificationCenter.fireEvent("Collapse.collapseStateDidChange", {
				source: "Collapse.collapseBlockDisclosureButtonStateChanged",
				collapseBlock: collapseBlock
			});
		});

		//	Collapse block expand-on-hover.
		if (   collapseBlock.classList.contains("expand-on-hover")
			&& GW.collapse.hoverEventsEnabled) {
			collapseBlock.addEventListener("mouseenter", (event) => {
				if (GW.collapse.hoverEventsActive == false) {
					collapseBlock.classList.add("hover-not");
				} else {
					collapseBlock.classList.remove("hover-not");
				}
			});
			onEventAfterDelayDo(collapseBlock, "mouseenter", 1000, (event) => {
				if (GW.collapse.hoverEventsActive == false)
					return;

				if (isCollapsed(collapseBlock) == false)
					return;

				if (collapseBlock.classList.contains("just-clicked"))
					return;

				disclosureButton.actionHandler(event);
			}, [ "mouseleave", "mousedown" ]);
		}

		//	On-hover state changes.
		if (GW.collapse.hoverEventsEnabled) {
			//	Add listener to show labels on hover, if need be.
			if (   collapseBlock.classList.contains("collapse-block")
				&& GW.collapse.showCollapseInteractionHintsOnHover == true
				&& GW.collapse.alwaysShowCollapseInteractionHints == false) {
				disclosureButton.addEventListener("mouseenter", (event) => {
					if (GW.collapse.hoverEventsActive == false)
						return;

					updateDisclosureButtonState(collapseBlock, true);
				});
				disclosureButton.addEventListener("mouseleave", (event) => {
					if (GW.collapse.hoverEventsActive == false)
						return;

					updateDisclosureButtonState(collapseBlock);
				});
			}

			//	Add listeners to highlight counterpart at other end.
			if (   collapseBlock.classList.contains("collapse-inline")
				&& disclosureButton.classList.containsAnyOf([ "start", "end" ])) {
				let counterpart = disclosureButton.classList.contains("end")
								  ? collapseBlock.querySelector(".disclosure-button")
								  : collapseBlock.querySelector(".collapse-content-wrapper").nextElementSibling;
				disclosureButton.addEventListener("mouseenter", (event) => {
					if (GW.collapse.hoverEventsActive == false)
						return;

					counterpart.classList.add("hover");
				});
				disclosureButton.addEventListener("mouseleave", (event) => {
					if (GW.collapse.hoverEventsActive == false)
						return;

					counterpart.classList.remove("hover");
				});
			}
		}
	});
}, "eventListeners");

/************************************************************************/
/*	Permanently expand a collapse block and remove its disclosure button.
 */
function expandLockCollapseBlock(collapseBlock) {
	//	Remove disclosure button.
	collapseBlock.querySelector(".disclosure-button").remove();

	//	Expand.
	let wasCollapsed = isCollapsed(collapseBlock);

	collapseBlock.classList.remove("collapse", "collapse-block", "collapse-inline", "expanded", "expanded-not", "expand-on-hover");
	if (collapseBlock.className == "")
		collapseBlock.removeAttribute("class");

	Array.from(collapseBlock.children).filter(x => x.matches(".collapse-content-wrapper, .abstract-collapse:not(.abstract)")).forEach(unwrap);
	
	if (   collapseBlock.tagName == "DIV"
		&& collapseBlock.className == ""
		&& isOnlyChild(collapseBlock.firstElementChild)) {
		unwrap(collapseBlock);
	}

	//	Fire event.
	if (wasCollapsed) {
		GW.notificationCenter.fireEvent("Collapse.collapseStateDidChange", {
			source: "Collapse.expandLockCollapseBlocks",
			collapseBlock: collapseBlock
		});
	}
}

/**********************************************************/
/*	Removes disclosure buttons and expands collapse blocks.
 */
addContentInjectHandler(GW.contentInjectHandlers.expandLockCollapseBlocks = (eventInfo) => {
	GWLog("expandLockCollapseBlocks", "collapse.js", 2);

	//  Permanently expand collapse blocks (by making them into regular blocks).
	eventInfo.container.querySelectorAll(".collapse").forEach(expandLockCollapseBlock);
}, "<rewrite", (info) => info.stripCollapses);

/*******************************************************************************/
/*	Ensure that the given element is scrolled into view when layout is complete.

	NOTE: Offset is ignored if element is inside a pop-frame.
 */
function scrollElementIntoView(element, offset = 0) {
    GWLog("scrollElementIntoView", "collapse.js", 2);

	if (   Extracts 
		&& Extracts.popFrameProvider
		&& Extracts.popFrameProvider.containingPopFrame(element)) {
		Extracts.popFrameProvider.scrollElementIntoViewInPopFrame(element);
	} else {	
		doWhenPageLayoutComplete(() => {
			element.scrollIntoView();
			if (offset != 0)
				window.scrollBy(0, offset);
			updateScrollState();
		});
	}
}

/*******************************************************************************/
/*	Expand collapse blocks to reveal the given element, and scroll it into view.
 */
function revealElement(element, scrollIntoView = true) {
    GWLog("revealElement", "collapse.js", 2);

	let didExpandCollapseBlocks = expandCollapseBlocksToReveal(element);

	if (scrollIntoView) {
		if (didExpandCollapseBlocks) {
			requestAnimationFrame(() => {
				scrollElementIntoView(element);		
			});
		} else {
			scrollElementIntoView(element);
		}
	}

	return didExpandCollapseBlocks;
}

/***********************************************/
/*  Reveal the element targeted by the URL hash.
 */
function revealTarget() {
    GWLog("revealTarget", "collapse.js", 1);

    let target = getHashTargetedElement();
    if (!target)
    	return;

	let didReveal = revealElement(target);

	//	Fire notification event.
	if (didReveal)
		GW.notificationCenter.fireEvent("Collapse.targetDidReveal");
}

/***************************************************************/
/*	On load and on hash change, reveal element targeted by hash.
 */
GW.notificationCenter.addHandlerForEvent("GW.hashHandlingSetupDidComplete", GW.revealTargetOnPageLayoutComplete = (info) => {
    GWLog("GW.revealTargetOnPageLayoutComplete", "collapse.js", 1);

	revealTarget();

	GW.notificationCenter.addHandlerForEvent("GW.hashDidChange", GW.revealTargetOnHashChange = (info) => {
 		GWLog("GW.revealTargetOnHashChange", "collapse.js", 1);

		revealTarget();
	});
});

/*******************************************************************************/
/*	What happens when a user C-fs on a page and there is a hit *inside* a
	collapse block? Just navigating to the collapsed section is not useful,
	especially when there may be multiple collapses inside a frame. So we must
	specially handle searches and pop open collapse sections with matches. We do
	this by watching for selection changes. (We don’t bother checking for window
	focus/blur because that is unreliable and in any case doesn’t work for
	“Search Again” key command.)
 */
document.addEventListener("selectionchange", GW.selectionChangedRevealElement = (event) => {
	GWLog("GW.selectionChangedRevealElement", "collapse.js", 3);

	let newSelection = document.getSelection();
	if (   newSelection
		&& newSelection.rangeCount > 0
		&& newSelection.getRangeAt(0).toString().length > 0) {
		let element = (newSelection.anchorNode.nodeType === Node.ELEMENT_NODE
					   ? newSelection.anchorNode
					   : newSelection.anchorNode.parentElement);
		if (isWithinCollapsedBlock(element))
			revealElement(element);
	}
});
