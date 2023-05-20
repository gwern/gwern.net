/*****************************************************************************/
/*	Visibility of block collapse labels depends on how many times the user has
	used them already.
 */
GW.collapse = {
	alwaysShowCollapseInteractionHints: (getSavedCount("clicked-to-expand-collapse-block-count") < (GW.isMobile() ? 6 : 3)),
	showCollapseInteractionHintsOnHover: (   GW.isMobile() == false 
										  && getSavedCount("clicked-to-expand-collapse-block-count") < 6)
};

/*******************************************************************************/
/*  This function expands all collapse blocks containing the given node, if
    any (including the node itself, if it is a collapse block). Returns true
    if any such expansion occurred. Fires Collapse.collapseStateDidChange event
    after all (possibly recursive) expansion is completed. (Only one event fired
    per non-recursive call to expandCollapseBlocksToReveal(), even if recursive
    expansion occurred.)
 */
function expandCollapseBlocksToReveal(node) {
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

    //  Expand the nearest collapse block.
    let collapseBlock = element.closest(".collapse");
    let expanded = isCollapsed(collapseBlock);
    toggleCollapseBlockState(collapseBlock, expanded);

    /*  Expand any higher-level collapse blocks!
        Fire state change event only if we did NOT have to do any further
        expansion (otherwise we’ll do redundant layout).
     */
    if (!expandCollapseBlocksToReveal(collapseBlock.parentElement) && expanded)
    	GW.notificationCenter.fireEvent("Collapse.collapseStateDidChange", {
    		source: "expandCollapseBlocksToReveal",
    		collapseBlock: collapseBlock
    	});

    //  Report whether we had to expand a collapse block.
    return expanded;
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
		disclosureButtonHTML += `<span class="icon"></span>`;
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

		if (GW.isMobile() == false)
			collapseBlock.classList.add("expand-on-hover");

		let collapseWrapper;
		if ([ "DIV", "SECTION", "SPAN" ].includes(collapseBlock.tagName)) {
			/*	Rewrap spans that are NOT inline collapses (i.e., those that
				are, for some reason, wrapping block-level content).
			 */
			if (   collapseBlock.tagName == "SPAN"
				&& containsBlockChildren(collapseBlock))
				collapseBlock = rewrapContents(collapseBlock, null, "DIV", true, true);

			//	Designate collapse type (block or inline).
			if ([ "SPAN" ].includes(collapseBlock.tagName))
				collapseBlock.classList.add("collapse-inline");
			else
				collapseBlock.classList.add("collapse-block");

			//	No additional wrapper needed for these tag types.
			collapseWrapper = collapseBlock;

			//	Ensure correct structure and classes of abstracts.
			collapseWrapper.querySelectorAll(".collapse > .abstract").forEach(collapseAbstract => {
				/*	Abstracts (the .abstract class) can end up in collapses
					without this being known in advance, so may not have the
					.abstract-collapse class, as they should.
				 */
				collapseAbstract.classList.add("abstract-collapse");
			});
			collapseWrapper.querySelectorAll(".collapse > .abstract-collapse").forEach(collapseAbstract => {
				//	Mark those collapse blocks that have abstracts.
				collapseAbstract.closest(".collapse").classList.add("has-abstract");

				if (collapseWrapper.classList.contains("collapse-block")) {
					if (   collapseAbstract.children.length == 0
						&& collapseAbstract.childNodes.length > 0) {
						//	Wrap bare text nodes.
						collapseAbstract.innerHTML = `<p>${(collapseAbstract.innerHTML.trim())}</p>`;
					} else if (   collapseAbstract.firstElementChild
							   && collapseAbstract.firstElementChild.tagName == "BLOCKQUOTE") {
						//	Make sure “real” abstracts are marked as such.
						collapseAbstract.classList.add("abstract");
					}
				}
			});

			//	Designate “bare content” collapse blocks.
			if (collapseWrapper.classList.contains("collapse-block")) {
				let bareContentTags = [ "P", "UL", "OL" ];
				if (   bareContentTags.includes(collapseBlock.firstElementChild.tagName)
					|| (   collapseWrapper.classList.contains("has-abstract")
						&& bareContentTags.includes(collapseBlock.firstElementChild.firstElementChild.tagName)))
					collapseWrapper.classList.add("bare-content");
			}
		} else {
			//	Additional wrapper is required for most tag types.
			collapseWrapper = wrapElement(collapseBlock, null, "DIV", true, [ "collapse", "expand-on-hover" ]);

			//	This is a block collapse.
			collapseWrapper.classList.add("collapse-block");
		}

		//	Mark as expanded, if need be.
		collapseWrapper.swapClasses([ "expanded", "expanded-not" ], startExpanded ? 0 : 1)

		//  Inject the disclosure button.
		if (collapseWrapper.classList.contains("collapse-inline")) {
			//	Button at start.
			if (collapseBlock.firstElementChild.classList.contains("abstract-collapse"))
				collapseWrapper.insertBefore(newDisclosureButton(false), collapseBlock.firstElementChild.nextSibling);
			else
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

		//	Designate abstract-less collapse blocks.
		if (collapseContentWrapper.previousElementSibling.classList.contains("abstract-collapse") == false)
			collapseWrapper.classList.add("no-abstract");

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

	let baseFontSize = parseInt(getComputedStyle(eventInfo.container).getPropertyValue("--GW-base-font-size"));

	eventInfo.container.querySelectorAll("section.collapse").forEach(section => {
		section.style.removeProperty("--collapse-toggle-top-height");
		section.style.removeProperty("--collapse-toggle-top-icon-size");

		requestAnimationFrame(() => {
			let minHeight = baseFontSize * parseFloat(getComputedStyle(section).getPropertyValue("--collapse-toggle-top-height"));
			let headingTextRects = Array.from(section.firstElementChild.querySelector("a").getClientRects());
			let oneLineHeight = headingTextRects.first.height;
			let totalHeight = headingTextRects.map(rect => rect.height).reduce((acc, val) => acc + val);
			if (totalHeight <= minHeight)
				return;

			section.style.setProperty("--collapse-toggle-top-height", Math.round(totalHeight + oneLineHeight * 0.25) + "px");
			section.style.setProperty("--collapse-toggle-top-icon-size", Math.round(oneLineHeight * 1.25) + "px");
		});
	});
}, "rewrite");

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

	let disclosureButton = collapseBlock.querySelector(".disclosure-button");

	let action = GW.isMobile() ? "Tap" : "Click";
	let labelHTML = isCollapsed(collapseBlock)
					? `${action} to expand`
					: `${action} to collapse`;

	disclosureButton.querySelectorAll(".part .label").forEach(label => {
		label.innerHTML = labelHTML;
	});

	if (collapseBlock.classList.contains("collapse-block")) {
		disclosureButton.classList.toggle("labels-visible", showLabels || GW.collapse.alwaysShowCollapseInteractionHints);
	} else {
		if (isCollapsed(collapseBlock)) {
			disclosureButton.querySelector(".icon").innerHTML = GW.svg("bracket-square-left-sharp-light") 
															  + GW.svg("angle-right-regular")
															  + GW.svg("bracket-square-right-sharp-light");
		} else {
			let collapseContentWrapper = collapseBlock.querySelector(".collapse-content-wrapper");
			collapseContentWrapper.previousElementSibling.querySelector(".icon").innerHTML = GW.svg("bracket-square-left-sharp-light");
			collapseContentWrapper.nextElementSibling.querySelector(".icon").innerHTML = GW.svg("bracket-square-right-sharp-light");
		}
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

	//	Compensate for block indentation due to nesting (e.g., lists).
	if (collapseBlock.classList.contains("collapse-block")) {
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
				&& GW.isMobile() == false) {
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
			&& GW.isMobile() == false) {
			onEventAfterDelayDo(collapseBlock, "mouseenter", 1000, (event) => {
				if (isCollapsed(collapseBlock) == false)
					return;

				if (collapseBlock.classList.contains("just-clicked"))
					return;

				disclosureButton.actionHandler(event);
			}, [ "mouseleave", "mousedown" ]);
		}

		//	On-hover state changes.
		if (GW.isMobile() == false) {
			//	Add listener to show labels on hover, if need be.
			if (   collapseBlock.classList.contains("collapse-block")
				&& GW.collapse.showCollapseInteractionHintsOnHover == true
				&& GW.collapse.alwaysShowCollapseInteractionHints == false) {
				disclosureButton.addEventListener("mouseenter", (event) => {
					updateDisclosureButtonState(collapseBlock, true);
				});
				disclosureButton.addEventListener("mouseleave", (event) => {
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
					counterpart.classList.add("hover");
				});
				disclosureButton.addEventListener("mouseleave", (event) => {
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
