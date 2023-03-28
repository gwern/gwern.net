GW.assets.collapseChevron = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 512"><path d="M34.52 239.03L228.87 44.69c9.37-9.37 24.57-9.37 33.94 0l22.67 22.67c9.36 9.36 9.37 24.52.04 33.9L131.49 256l154.02 154.75c9.34 9.38 9.32 24.54-.04 33.9l-22.67 22.67c-9.37 9.37-24.57 9.37-33.94 0L34.52 272.97c-9.37-9.37-9.37-24.57 0-33.94z" fill="%23CCC"/></svg>`;

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
    let expansionOccurred = isCollapsed(collapseBlock);
    collapseBlock.classList.toggle("expanded", true);
	updateDisclosureButtonState(collapseBlock);

    /*  Expand any higher-level collapse blocks!
        Fire state change event only if we did NOT have to do any further
        expansion (otherwise we’ll do redundant layout).
     */
    if (!expandCollapseBlocksToReveal(collapseBlock.parentElement) && expansionOccurred)
    	GW.notificationCenter.fireEvent("Collapse.collapseStateDidChange", { source: "expandCollapseBlocksToReveal" });

    //  Report whether we had to expand a collapse block.
    return expansionOccurred;
}

/*******************************************************************/
/*  Returns true if the given collapse block is currently collapsed.
 */
function isCollapsed(collapseBlock) {
    return (collapseBlock.classList.contains("expanded") == false);
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

/***********************************************************************/
/*  Inject disclosure buttons and otherwise prepare the collapse blocks.
 */
addContentLoadHandler(GW.contentLoadHandlers.prepareCollapseBlocks = (eventInfo) => {
	GWLog("prepareCollapseBlocks", "collapse.js", 1);

	let aBlockDidStartExpanded = false;

	//  Construct all collapse blocks (in correct final state).
	eventInfo.container.querySelectorAll(".collapse").forEach(collapseBlock => {
		let startExpanded = (collapseBlock.contains(getHashTargetedElement()) == true);
		let disclosureButtonHTML = `<button type="button" class="disclosure-button" aria-label="Open/close collapsed section">`
									 + `<span class="part top">`
										 + `<span class="label"></span>`
										 + `<span class="icon">`
										 	+ GW.assets.collapseChevron
										 + `</span>`
									 + `</span>`
									 + `<span class="part bottom">`
										 + `<span class="label"></span>`
										 + `<span class="icon"></span>`
									 + `</span>`
								 + `</button>`;

		if (startExpanded)
			aBlockDidStartExpanded = true;

		if ([ "H1", "H2", "H3", "H4", "H5", "H6" ].includes(collapseBlock.tagName)) {
			collapseBlock.classList.remove("collapse");
			if (collapseBlock.className == "")
				collapseBlock.removeAttribute("class");
		} else {
			collapseBlock.classList.add("expand-on-hover");

			let collapseWrapper;
			if ([ "DIV", "SECTION" ].includes(collapseBlock.tagName)) {
				collapseWrapper = collapseBlock;

				//	Ensure correct structure and classes of abstracts.
				collapseBlock.querySelectorAll(".collapse > .abstract").forEach(collapseAbstract => {
					/*	Abstracts (the .abstract class) can end up in collapses
						without this being known in advance, so may not have the
						.abstract-collapse class, as they should.
					 */
					collapseAbstract.classList.add("abstract-collapse");
				});
				collapseBlock.querySelectorAll(".collapse > .abstract-collapse").forEach(collapseAbstract => {
					//	Mark those collapse blocks that have abstracts.
					collapseAbstract.closest(".collapse").classList.add("has-abstract");

					//	Make sure “real” abstracts are marked as such.
					if (collapseAbstract.firstElementChild.tagName == "BLOCKQUOTE")
						collapseAbstract.classList.add("abstract");
						
				});
			} else {
				collapseWrapper = wrapElement(collapseBlock, null, "DIV", true, [ "collapse", "expand-on-hover" ]);
			}

			//	Mark as expanded, if need be.
			if (startExpanded)
				collapseWrapper.classList.add("expanded");

			//  Inject the disclosure button.
			if ([ "SECTION" ].includes(collapseBlock.tagName)) {
				collapseWrapper.firstElementChild.insertAdjacentHTML("afterend", disclosureButtonHTML);
			} else {
				collapseWrapper.insertAdjacentHTML("afterbegin", disclosureButtonHTML);
			}

			//	Construct collapse content wrapper.
			let collapseContentWrapper = newElement("DIV", { "class": "collapse-content-wrapper" });
			let childNodesArray = Array.from(collapseWrapper.childNodes);
			collapseContentWrapper.append(...childNodesArray.slice(childNodesArray.findLastIndex(node => {
				return (   node instanceof Element 
						&& node.classList.containsAnyOf([ "disclosure-button", "abstract-collapse" ]));
			}) + 1));
			collapseWrapper.append(collapseContentWrapper);
		}
	});

	if (aBlockDidStartExpanded)
		GW.notificationCenter.fireEvent("Collapse.collapseStateDidChange", { source: "prepareCollapseBlocks" });
}, "rewrite");

/********************************************************/
/*	Updates disclosure button label for current UI state.
 */
function updateDisclosureButtonState(collapseBlock) {
	GWLog("updateDisclosureButtonState", "collapse.js", 2);

	let disclosureButton = collapseBlock.querySelector(".disclosure-button");

	let labelHTML = isCollapsed(collapseBlock)
					? `Click to expand`
					: `Click to collapse`;

	disclosureButton.querySelector(".part.top .label").innerHTML = labelHTML;
	disclosureButton.querySelector(".part.bottom .label").innerHTML = labelHTML;
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

			collapseBlock.classList.toggle("expanded");

			updateDisclosureButtonState(collapseBlock);

			//	“Scroll into view” in main document vs. pop-frames.
			let scrollCollapseBlockIntoView = (collapseBlock) => {
				if (Extracts.popFrameProvider.containingPopFrame(collapseBlock))
					Extracts.popFrameProvider.scrollElementIntoViewInPopFrame(collapseBlock);
				else
					scrollElementIntoView(collapseBlock);
			};

			/*	If a collapse block was collapsed from the bottom, it might now
				be up off the screen. Scroll it into view.
			 */
			if (   isCollapsed(collapseBlock)
				&& isOnScreen(collapseBlock) == false)
				scrollCollapseBlockIntoView(collapseBlock);
			/*	If a collapse block was expanded from the bottom, the top of the
				collapse block might be up off the screen. Scroll it into view.
			 */
			else if (   isCollapsed(collapseBlock) == false
					 && collapseBlock.getBoundingClientRect().top < 0)
				scrollCollapseBlockIntoView(collapseBlock);

			GW.notificationCenter.fireEvent("Collapse.collapseStateDidChange", { source: "Collapse.collapseBlockDisclosureButtonStateChanged" });
		});

		/*	Collapse block expand-on-hover. Clicking within the block while it
			is temporarily expanded causes it to stay expanded permanently.
		 */
		if (   collapseBlock.classList.contains("expand-on-hover")
			&& GW.isMobile() == false) {
			let expandOnHoverDelay = 800;
			let collapseOnUnhoverDelay = 300;

			onEventAfterDelayDo(collapseBlock, "mouseenter", expandOnHoverDelay, (event) => {
				if (isCollapsed(collapseBlock) == false)
					return;

				disclosureButton.actionHandler(event);

				collapseBlock.classList.add("expanded-temp");

				let removeUnhoverHandler;
				let collapseBlockMouseleaveHandler = (event) => {
					if (collapseBlock.classList.contains("expanded-temp") == false)
						return;

					disclosureButton.actionHandler(event);

					collapseBlock.classList.remove("expanded-temp");

					removeUnhoverHandler();
				};
				removeUnhoverHandler = onEventAfterDelayDo(collapseBlock, "mouseleave", collapseOnUnhoverDelay, (event) => {
					collapseBlockMouseleaveHandler(event);
				}, "mouseenter");

				let collapseBlockClickHandler = (event) => {
					collapseBlock.classList.remove("expanded-temp");

					removeUnhoverHandler();
					collapseBlock.removeEventListener("click", collapseBlockClickHandler);
				};
				collapseBlock.addEventListener("click", collapseBlockClickHandler);
			}, "mouseleave");
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

	collapseBlock.classList.remove("collapse", "expanded", "expand-on-hover");
	if (collapseBlock.className == "")
		collapseBlock.removeAttribute("class");

	if (collapseBlock.firstElementChild.classList.contains("collapse-content-wrapper")) {
		unwrap(collapseBlock.firstElementChild);
	}
	
	if (   collapseBlock.tagName == "DIV"
		&& collapseBlock.className == ""
		&& isOnlyChild(collapseBlock.firstElementChild)) {
		unwrap(collapseBlock);
	}

	//	Fire event.
	if (wasCollapsed)
		GW.notificationCenter.fireEvent("Collapse.collapseStateDidChange", { source: "Collapse.expandLockCollapseBlocks" });
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
 */
function scrollElementIntoView(element, offset = 0) {
    GWLog("scrollElementIntoView", "collapse.js", 2);

	doWhenPageLayoutComplete(() => {
		element.scrollIntoView();
		if (offset != 0)
			window.scrollBy(0, offset);
	});
}

/*******************************************************************************/
/*	Expand collapse blocks to reveal the given element, and scroll it into view.
 */
function revealElement(element, scrollIntoView = true) {
    GWLog("revealElement", "collapse.js", 2);

	let didExpandCollapseBlocks = expandCollapseBlocksToReveal(element);

	if (   scrollIntoView
		&& isOnScreen(element) == false)
		scrollElementIntoView(element);

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
		revealElement(element);
	}
});
