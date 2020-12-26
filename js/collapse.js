/*  Reveals the given node by expanding all containing collapse blocks.
    */
function expandAllAncestorsOfNode(node) {
	GWLog("expandAllAncestorsOfNode", "collapse.js", 2);

    // If the node is not an element (e.g. a text node), get its parent element.
    let element = node instanceof HTMLElement ? node : node.parentElement;

    // Get the closest containing collapse block. If none such, return.
    let enclosingCollapseBlock = element.closest(".collapse");
    if (!enclosingCollapseBlock) return;

    // Expand the collapse block by checking the disclosure-button checkbox.
    enclosingCollapseBlock.querySelector(`#${enclosingCollapseBlock.id} > .disclosure-button`).checked = true;

    // Recursively expand all ancestors of the collapse block.
    expandAllAncestorsOfNode(enclosingCollapseBlock.parentElement);
}

/*  This function expands all collapse blocks containing the given node, if
    any (including the node itself, if it is a collapse block). Returns true
    if any such expansion occurred.
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
        expansion (otherwise we'll do redundant layout).
        */
    if (!expandCollapseBlocksToReveal(collapseParent.parentElement) && expansionOccurred)
    	GW.notificationCenter.fireEvent("Collapse.collapseStateDidChange");

    //  Report whether we had to expand a collapse block.
    return expansionOccurred;
}

/*  Returns true if the given collapse block is currently collapsed.
    NOTE: This does not count targeted collapse blocks as expanded unless
    their disclosure button is also engaged (i.e., in the checked state).
    This is deliberate! (Because we use the disclosure button state to
    determine whether we need to recompute layout.)
    */
function isCollapsed(collapseBlock) {
    let collapseCheckbox = collapseBlock.querySelector(".disclosure-button");
    return (collapseCheckbox.checked == false);
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

/*  This function expands all necessary collapse blocks to reveal the element
    targeted by the URL hash. (This includes expanding collapse blocks to
    reveal a footnote reference associated with a targeted sidenote). It also
    scrolls the targeted element into view.
    */
function revealTarget() {
    GWLog("revealTarget", "collapse.js", 1);

    if (!location.hash) return;

    let target = document.querySelector(decodeURIComponent(location.hash));
    if (!target) return;

    /*  What needs to be revealed is not necessarily the targeted element
        itself; if the target is a sidenote, expand collapsed blocks to reveal
        the citation reference.
        */
    let targetInText = location.hash.match(/#sn[0-9]/) 
    				   ? document.querySelector("#fnref" + location.hash.substr(3)) 
    				   : target;
    expandCollapseBlocksToReveal(targetInText);

    //  Scroll the target into view.
    target.scrollIntoView();
}

/*  Inject disclosure buttons and otherwise prepare the collapse blocks.
    */
function prepareCollapseBlocks() {
	GWLog("prepareCollapseBlocks", "collapse.js", 1);

	document.querySelectorAll(".collapse").forEach(collapseBlock => {
		let disclosureButtonHTML = "<input type='checkbox' title='This is a collapsed region; mouse click to expand it. Collapsed text can be sections, code, text samples, or long digressions which most users will not read, and interested readers can opt into.' class='disclosure-button' aria-label='Open/close collapsed section'>";
		if (collapseBlock.tagName == "SECTION") {
			//  Inject the disclosure button.
			collapseBlock.children[0].insertAdjacentHTML("afterend", disclosureButtonHTML);
		} else if ([ "H1", "H2", "H3", "H4", "H5", "H6" ].includes(collapseBlock.tagName)) {
			// Remove the `collapse` class and do nothing else.
			collapseBlock.classList.remove("collapse");
		} else {
			//  Construct collapse block wrapper and inject the disclosure button.
			let realCollapseBlock = document.createElement("div");
			realCollapseBlock.classList.add("collapse");
			realCollapseBlock.insertAdjacentHTML("afterbegin", disclosureButtonHTML);
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

			//  If it's a code block, adjust its height.
			if (collapseBlock.lastElementChild.tagName == "PRE") {
				let codeBlock = collapseBlock.lastElementChild.lastElementChild;
				if (codeBlock.tagName != "CODE") return;

				codeBlock.style.height = "";
				requestAnimationFrame(() => {
					rectifyCodeBlockHeight(codeBlock);
				});
			}

	    	GW.notificationCenter.fireEvent("Collapse.collapseStateDidChange");
		});
	});
}
prepareCollapseBlocks();
