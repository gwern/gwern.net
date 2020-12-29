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
    if (!isWithinCollapsedBlock(element)) return false;

    //  Expand the nearest collapse block.
    let collapseParent = element.closest(".collapse");
    let disclosureButton = collapseParent.querySelector(".disclosure-button");
    let expansionOccurred = (disclosureButton.checked == false);
    disclosureButton.checked = true;
    collapseParent.classList.toggle("expanded", disclosureButton.checked);

    /*  Expand any higher-level collapse blocks!
        Fire state change event only if we did NOT have to do any further
        expansion (otherwise we’ll do redundant layout).
        */
    if (!expandCollapseBlocksToReveal(collapseParent.parentElement) && expansionOccurred)
    	GW.notificationCenter.fireEvent("Collapse.collapseStateDidChange");

    //  Report whether we had to expand a collapse block.
    return expansionOccurred;
}

/*  Returns true if the given collapse block is currently collapsed.
    */
function isCollapsed(collapseBlock) {
    return !collapseBlock.classList.contains("expanded");
}

/*  Returns true if the given element is within a currently-collapsed collapse
    block.
    */
function isWithinCollapsedBlock(element) {
    /*  If the element is not within a collapse block at all, it obviously can't
        be within a *currently-collapsed* collapse block.
        */
    let collapseParent = element.closest(".collapse");
    if (!collapseParent) return false;

    /*  If the element is within a collapse block and that collapse block is
        currently collapsed, then the condition is satisfied...
        */
    if (isCollapsed(collapseParent)) return true;

    /*  BUT the collapse block that the element is in, even if *it* is not
        itself collapsed, could be *within* another collapse block!
        */
    return isWithinCollapsedBlock(collapseParent.parentElement);
}

/*  Inject disclosure buttons and otherwise prepare the collapse blocks.
    */
function prepareCollapseBlocks() {
	GWLog("prepareCollapseBlocks", "collapse.js", 1);

	let hashTarget = getHashTargetedElement();
	document.querySelectorAll(".collapse").forEach(collapseBlock => {
		let checked = collapseBlock.contains(hashTarget) ? " checked='checked'" : "";
		let disclosureButtonHTML = `<input type='checkbox' title='This is a collapsed region; mouse click to expand it. Collapsed text can be sections, code, text samples, or long digressions which most users will not read, and interested readers can opt into.' class='disclosure-button' aria-label='Open/close collapsed section'${checked}>`;
		if (collapseBlock.tagName == "SECTION") {
			//  Inject the disclosure button.
			collapseBlock.children[0].insertAdjacentHTML("afterend", disclosureButtonHTML);
			if (checked > "")
				collapseBlock.classList.add("expanded");
		} else if ([ "H1", "H2", "H3", "H4", "H5", "H6" ].includes(collapseBlock.tagName)) {
			//  Remove the ‘collapse’ class and do nothing else.
			collapseBlock.classList.remove("collapse");
		} else if (collapseBlock.parentElement.tagName == "DIV" && collapseBlock.parentElement.children.length == 1) {
			//  Use parent div as collapse block wrapper.
			let realCollapseBlock = collapseBlock.parentElement;
			realCollapseBlock.classList.add("collapse");
			realCollapseBlock.insertAdjacentHTML("afterbegin", disclosureButtonHTML);
			if (checked > "")
				realCollapseBlock.classList.add("expanded");
			collapseBlock.classList.remove("collapse");
		} else {
			//  Construct collapse block wrapper and inject the disclosure button.
			let realCollapseBlock = document.createElement("div");
			realCollapseBlock.classList.add("collapse");
			realCollapseBlock.insertAdjacentHTML("afterbegin", disclosureButtonHTML);
			if (checked > "")
				realCollapseBlock.classList.add("expanded");
			//  Move block-to-be-collapsed into wrapper.
			collapseBlock.parentElement.insertBefore(realCollapseBlock, collapseBlock);
			collapseBlock.classList.remove("collapse");
			realCollapseBlock.appendChild(collapseBlock);
		}
	});

    /*  Add listeners to toggle ‘expanded’ class of collapse blocks.
		*/
	document.querySelectorAll(".disclosure-button").forEach(disclosureButton => {
		let collapseBlock = disclosureButton.closest(".collapse");
		disclosureButton.addEventListener("change", (event) => {
			collapseBlock.classList.toggle("expanded", disclosureButton.checked);

	    	GW.notificationCenter.fireEvent("Collapse.collapseStateDidChange");
		});
	});
}
doWhenDOMContentLoaded(prepareCollapseBlocks);

/*	Expand collapse blocks to reveal the given element, and scroll it into view.
	*/
function revealElement(element, scrollIntoView = true) {
    GWLog("revealElement", "collapse.js", 2);

	let didExpandCollapseBlocks = expandCollapseBlocksToReveal(element);
	if (scrollIntoView) {
		doWhenPageLoaded(() => {
			requestAnimationFrame(() => {
				element.scrollIntoView();
			});
		});
	}
	return didExpandCollapseBlocks;
}

/*  Return the element targeted by the URL hash, or null.
    */
function getHashTargetedElement() {
	return (location.hash.length > 1)
			? document.querySelector(decodeURIComponent(location.hash))
			: null;
}

/*  Reveal the element targeted by the URL hash. Do the same on hash change.
    */
function revealTarget() {
    GWLog("revealTarget", "collapse.js", 1);

    if (!location.hash)
    	return;

    let target = getHashTargetedElement();
    if (!target)
    	return;

	revealElement(target);

	/*	Fire notification event. Pass handlers the revealElement() function,
		so that they can reveal other elements, if desired.
		*/
	GW.notificationCenter.fireEvent("Collapse.targetDidRevealOnHashUpdate", { 
		revealTargetFunction: revealTarget,
		revealElementFunction: revealElement
	});
}
/*	We don’t need to do this unconditionally (e.g. on DOMContentLoaded) because
	the hashchange event will be triggered by the realignHash() function in
	rewrite.js (and in any case we inject the collapse disclosure buttons in the
	correct state to begin with). (We do still want realignHash() to cause the
	hashchange event to fire, so that Collapse.targetDidRevealOnHashUpdate fires
	if need be and triggers any auxiliary element reveals.)
	*/
window.addEventListener("hashchange", GW.hashUpdated = () => {
	GWLog("GW.hashUpdated", "collapse.js", 1);

	revealTarget();
});

/*	What happens when a user C-fs on a page and there is a hit *inside* a collapse block? Just navigating to the collapsed section is not useful, especially when there may be multiple collapses inside a frame. So we must specially handle searches and pop open collapse sections with matches. We do this by watching for selection changes. (We don’t bother checking for window focus/blur because that is unreliable and in any case doesn’t work for “Search Again” key command.
	*/
document.addEventListener("selectionchange", GW.selectionChanged = (event) => {
	GWLog("GW.selectionChanged", "rewrite.js", 3);

	let newSelection = document.getSelection();
	if (newSelection && newSelection.getRangeAt(0).toString().length > 0)
		expandCollapseBlocksToReveal(newSelection.anchorNode);
});
