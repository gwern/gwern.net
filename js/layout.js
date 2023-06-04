/********/
/* MISC */
/********/

/************************************************************/
/*	Inflict the indentation AB test upon the given container.
 */
function inflictABTestClassesUponContainer(container, conditions) {
	if (container.closest(GW.layout.blockLayoutExclusionSelector))
		return;

	let justifyClassNames = [ "justified-not", "justified" ];
	let indentClassNames = [ "indented-not", "indented" ];

	conditions = conditions ?? {
		justify: document.body.classList.contains(justifyClassNames[0]) ? 0 : 1,
		indent: document.body.classList.contains(indentClassNames[0]) ? 0 : 1
	};

	container.querySelectorAll(".markdownBody").forEach(markdownBody => {
		if (markdownBody.closest(GW.layout.blockLayoutExclusionSelector))
			return;

		markdownBody.swapClasses(justifyClassNames, conditions.justify);
		markdownBody.swapClasses(indentClassNames, conditions.indent);
	});
	if (container.matches?.(".markdownBody")) {
		container.swapClasses(justifyClassNames, conditions.justify);
		container.swapClasses(indentClassNames, conditions.indent);
	}
};

/***********************************/
/*	Inflict the indentation AB test.
 */
addContentInjectHandler(GW.contentInjectHandlers.inflictABTestClasses = (eventInfo) => {
    GWLog("inflictABTestClasses", "rewrite.js", 1);

	inflictABTestClassesUponContainer(eventInfo.container)
}, ">rewrite");


/**********/
/* LAYOUT */
/**********/

GW.layout = {
	containersNeedingLayout: [ ],

	layoutProcessors: [ ],

	//	Block containers 
	blockContainers: [
		".markdownBody",
		"section",
		".collapse",
		"blockquote",
		".epigraph",
		".admonition",
		".sidenote"
	],

	//	Block elements get layout classes applied to them.
	blockElements: [
		"section",
		".collapse",
		"blockquote",
		".epigraph",
		"p",
		".columns",
		".footnote",
		"figure",
		"hr",
		".sourceCode",
		".table-wrapper",
		".math.block",
		".admonition",
		".annotation"
	],

	//	Wrappers are transparent at the top and bottom.
	wrapperElements: [
		"div",
		"span",
		".list",
		"li"
	],

	//	Half-wrappers are transparent at the bottom only, not the top.
	halfWrapperElements: [
		"section"
	],

	//	Elements which do not participate in block layout.
	skipElements: [
		".empty",
		".float",
		"noscript",
		"button"
	],

	//	Elements which always participate in block layout, even when empty.
	nonEmptyElements: [
		"hr"
	],

	//	Do not apply block layout classes within these containers.
	blockLayoutExclusionSelector: [
		"#page-metadata",
		".TOC"
	].join(", "),

	blockSpacing: [
		[ "body.index .abstract > p.first-block",	 7, false ],
		[ "body.index section",						 7, false ],
		[ "body.index section li p + p",			 0, false ],

		[ ".float.first-block",			 2, false ],
		[ ".first-block",				 0, false ],

		[ ".heading + section",			 5, false ],
		[ ".heading + *",				 4, false ],

		[ "p.footnote-back-block",		 1, false ],
		[ "p.first-graf",				10 ],
		[ "p.list-heading",				10 ],
		[ "p",							(block) => (indentModeActive(block) ? 0 : 10) ],

		[ ".collapse-block",			10 ],

		[ "section.level1",				15 ],
		[ "section.level2",				13 ],
		[ "section.level3",				11 ],
		[ "section.level4",				10 ],
		[ "section.level5",				 9 ],
		[ "section.level6",				 8 ],

		[ "section.footnotes",			14 ],
		[ ".footnote",					 6 ],

		[ "hr",							10 ],

		[ ".columns",					 6 ],

		[ "figure.outline-not",			 9 ],
		[ "figure",						10 ],

		[ "blockquote",					10 ],

		[ ".epigraph",					 6 ],

		[ "div.table-wrapper",			10 ],

		[ "div.sourceCode",				10 ],

		[ ".math.block",				10 ],

		[ ".admonition",				10 ],

		[ ".annotation",				10 ]
	],

	blockSpacingAdjustments: [
		[ "p + p",							(bsm, block) => bsm - 6 ],

		[ [ "p + :not(p)",
			":not(p) + p"
			],								(bsm, block) => bsm - 2 ],

		[ [ "p + blockquote",
			"blockquote + p",
			"blockquote + blockquote"
			],								(bsm, block) => bsm - 2 ],

		[ [	"p + .math.block",
			".math.block + p",
			".math.block + .math.block"
			],								(bsm, block) => bsm - 2 ],

		[ [ ".in-list + .in-list",
			".list-heading + .in-list"
			],								(bsm, block) => bsm - 2 ],

		[ ".list-heading + .aux-links-list p",
											(bsm, block) => bsm - 2 ],
	]
};

/**********************************************************************/
/*	Registers a layout processor function, which will be applied to all 
	rendered content as part of the dynamic layout process.
 */
function addLayoutProcessor(processor, options = { }) {
	GW.layout.layoutProcessors.push([ processor, options ]);
}

/****************************************************/
/*	Activates dynamic layout for the given container.
 */
function startDynamicLayoutInContainer(container) {
	let blockContainersSelector = selectorize(GW.layout.blockContainers);

	let observer = new MutationObserver((mutationsList, observer) => {
		let baseDocumentLocation = baseLocationForDocument(mutationsList.first?.target?.getRootNode());

		//	Construct list of all block containers affected by these mutations.
		let affectedContainers = [ ];

		for (mutationRecord of mutationsList) {
			//	Find block container in which the mutated element is contained.
			let nearestContainer = mutationRecord.target.closest(blockContainersSelector);

			//	Avoid adding a container twice, and apply exclusions.
			if (   nearestContainer
				&& affectedContainers.includes(nearestContainer) == false
				&& nearestContainer.closest(GW.layout.blockLayoutExclusionSelector) == null)
				affectedContainers.push(nearestContainer);
		}

		/*	Exclude containers that are contained within other containers in
			the list, to prevent redundant processing.
		 */
		affectedContainers = affectedContainers.filter(c => affectedContainers.findIndex(x => 
			(c.compareDocumentPosition(x) & Node.DOCUMENT_POSITION_CONTAINS)
		) == -1);

		/*	Add containers to list of containers needing layout processing, if
			they are not there already.
		 */
		affectedContainers.forEach(affectedContainer => {
			if (GW.layout.containersNeedingLayout.includes(affectedContainer) == false)
				GW.layout.containersNeedingLayout.push(affectedContainer);
		});
		requestAnimationFrame(() => {
			//	Do layout in all waiting containers.
			while (GW.layout.containersNeedingLayout.length > 0) {
				let nextContainer = GW.layout.containersNeedingLayout.shift();
				GW.layout.layoutProcessors.forEach(layoutProcessor => {
					let [ processor, options ] = layoutProcessor;

					let info = {
						container: nextContainer,
						baseLocation: baseDocumentLocation
					};
					if (options.condition?.(info) == false)
						return;

					processor(nextContainer);
				});
			}
		});
	});

	observer.observe(container, { subtree: true, childList: true });
}

/*************************************************/
/*	Activate dynamic layout for the main document.
 */
doWhenBodyExists(() => {
	startDynamicLayoutInContainer(document.body);
});

/***************************************************************************/
/*	Returns true if indent mode is active for the block’s container, 
	(or for the main page content, if block not specified), false otherwise.
 */
function indentModeActive(block) {
	block = block ?? document.querySelector("#markdownBody");

	return (block.closest(".markdownBody")?.matches(".indented") == true);
}

/*****************************************************************************/
/*	Process layout options object, so that it contains all the appropriate
	defaults (from GW.layout). (This must be done before the options object is
	read or used in any way except being passed to another function.)
 */
function processLayoutOptions(options) {
	[	"blockContainers",
		"blockElements",
		"skipElements",
		"nonEmptyElements",
		"wrapperElements",
		"halfWrapperElements"
	].forEach(optionKey => {
		let selectorKey = optionKey + "Selector";
		if (options[selectorKey] != null)
			return;

		if (options[optionKey] == null) {
			options[optionKey] = GW.layout[optionKey];

			let capitalizedOptionKey = optionKey.slice(0, 1).toUpperCase() + optionKey.slice(1);
			let alsoOptionKey = "also" + capitalizedOptionKey;
			let notOptionKey = "not" + capitalizedOptionKey;

			if (options[alsoOptionKey] != null)
				options[optionKey] = [ ...options[optionKey], options[alsoOptionKey] ];

			if (options[notOptionKey] != null)
				options[optionKey] = options[optionKey].filter(x => options[notOptionKey].includes(x) == false);
		}

		options[selectorKey] = options[optionKey].join(", ");
	});
}

/***************************************************************************/
/*	Returns true if element is a wrapper of the given type, false otherwise.

	Types: upOut, downOut, upIn, downIn
 */
function isWrapper(element, wrapperType, options = { }) {
	processLayoutOptions(options);

	let filter = [ "downOut", "upIn" ].includes(wrapperType)
				 ? (x => [ ...options.wrapperElements, ...options.halfWrapperElements ].includes(x) == false)
				 : (x => options.wrapperElements.includes(x) == false)
	let blockElementsSelector = options.blockElements.filter(filter).join(", ");
	let blockContainersSelector = options.blockContainers.filter(filter).join(", ");
	let wrappersSelector = [ "downOut", "upIn" ].includes(wrapperType)
						   ? [ ...options.wrapperElements, ...options.halfWrapperElements ].join(", ")
						   : options.wrapperElements.join(", ");

	return (   element?.matches(wrappersSelector) == true
			&& element?.matches(blockElementsSelector) != true
			&& element?.matches(blockContainersSelector) != true);
}

/*****************************************************************/
/*	Returns true if element is a skipped element, false otherwise.
 */
function isSkipped(element, options = { }) {
	processLayoutOptions(options);

	return (element?.matches(options.skipElementsSelector) == true);
}

/**************************************************************/
/*	Returns true if element is a layout block, false otherwise.
 */
function isBlock(element, options = { }) {
	processLayoutOptions(options);

	return (element?.matches(options.blockElementsSelector) == true);
}

/***************************************************************************/
/*	Returns true if element is an always-not-empty element, false otherwise.
 */
function isNonEmpty(element, options = { }) {
	processLayoutOptions(options);

	return (element?.matches(options.nonEmptyElementsSelector) == true);
}

/******************************************************************/
/*	Returns nearest enclosing block container of the given element.
	(Might be null.)
 */
function blockContainerOf(element, options = { }) {
	if (element == null)
		return null;

	processLayoutOptions(options);

	return element.parentElement?.closest(options.blockContainersSelector);
}

/**************************************************************************/
/*	Returns layout block sequential (next or previous) to the given element
	in the block flow.
 */
function sequentialBlockOf(element, direction, options = { }) {
	if (element == null)
		return null;

	processLayoutOptions(options);

	let siblingKey = direction + "ElementSibling";
	let wrapperDirection = (direction == "next" ? "down" : "up");
	let wrapperInType = wrapperDirection + "In";
	let wrapperOutType = wrapperDirection + "Out";
	let terminus = (direction == "next" ? "first" : "last");

	//	Look inside “transparent” wrappers (that don’t affect layout).
	if (isWrapper(element[siblingKey], wrapperInType, options))
		return terminalBlockOf(element[siblingKey], terminus, options);
	
	//	Skip elements that don’t participate in block flow.
	if (   isSkipped(element[siblingKey], options)
		|| (   isNodeEmpty(element[siblingKey]) == true
			&& isNonEmpty(element[siblingKey], options) == false))
		return sequentialBlockOf(element[siblingKey], direction, options);

	//	An actual block element (the base case).
	if (isBlock(element[siblingKey], options))
		return element[siblingKey];

	/*	If we’re asked for the sequential block of the terminal child of a 
		transparent wrapper, we return the sequential block of that wrapper 
		(recursively, of course).
	 */
	if (isWrapper(element.parentElement, wrapperOutType, options))
		return sequentialBlockOf(element.parentElement, direction, options);

	return null;
}

/************************************************************************/
/*	Returns layout block previous to the given element in the block flow.
	(Might be null.)
 */
function previousBlockOf(element, options = { }) {
	return sequentialBlockOf(element, "previous", options);
}

/************************************************************************/
/*	Returns layout block next from the given element in the block flow.
	(Might be null.)
 */
function nextBlockOf(element, options = { }) {
	return sequentialBlockOf(element, "next", options);
}

/***************************************************************/
/*	Returns terminal (first or last) layout block of an element.
	(Might be the element itself, or null.)
 */
function terminalBlockOf(element, terminus, options = { }) {
	if (element == null)
		return null;

	processLayoutOptions(options);

	let wrapperType = (terminus == "first" ? "down" : "up") + "In";

	//	Look inside wrappers.
	if (isWrapper(element, wrapperType, options)) {
		for (let i  = (terminus == "first" ? 0                       : element.children.length - 1); 
				 i != (terminus == "first" ? element.children.length : -1); 
			     i += (terminus == "first" ? 1                       : -1)) {
			let terminalBlock = terminalBlockOf(element.children[i], terminus, options);
			if (terminalBlock)
				return terminalBlock;
		}
	}

	//	The element itself is a layout block.
	if (isBlock(element, options))
		return element;

	return null;
}

/*******************************************/
/*	Returns last layout block of an element.
	(Might be the element itself, or null.)
 */
function lastBlockOf(element, options = { }) {
	return terminalBlockOf(element, "last", options);
}

/********************************************/
/*	Returns first layout block of an element.
	(Might be the element itself, or null.)
 */
function firstBlockOf(element, options = { }) {
	return terminalBlockOf(element, "first", options);
}

/***************************************************************************/
/*	Returns all “child” blocks of an element (blocks that are descended from 
	the given element with no other blocks in the chain of descent; wrappers 
	don’t count).
 */
function childBlocksOf(element, options = { }) {
	if (element == null)
		return null;

	processLayoutOptions(options);

	let childBlocks = Array.from(element.children);

	for (let i = 0; i < childBlocks.length; i++) {
		if (isWrapper(childBlocks[i], "downIn", options)) {
			childBlocks.splice(i, 1, ...childBlocks[i].children);
			i--;
		}
	}

	return childBlocks;
}

/**************************************************************************/
/*	Returns assembled and appropriately prefixed selector from given parts.
 */
function selectorize(parts) {
	return parts.map(part => (part == ".markdownBody" ? part : `.markdownBody ${part}`)).join(", ");
}

/***************************************************************/
/*	Returns tag_name#id.class1.class2.class3 of a given element.
 */
function elementSummaryString(element) {
	return (  element.tagName.toLowerCase()
			+ (element.id ? `#${element.id}` : ``)
			+ (Array.from(element.classList).map(x => `.${x}`).join("")));
}

/********************************************************/
/*	Returns block spacing multiplier for the given block.
 */
function getBlockSpacingMultiplier(block) {
	let predicateFromSelector = (selector) => {
		let parts = selector.match(/^(.+) \+ (.+)$/);
		if (parts) {
			/*	Headings do not normally count as layout blocks, but they do 
				here, except headings of collapsible sections.
			 */
			return (block) => (   previousBlockOf(block, { alsoBlockElements: [ "section:not(.collapse) > .heading" ] })?.matches(parts[1]) 
							   && block.matches(parts[2]));
		} else {
			return (block) => block.matches(selector);
		}
	};

	let predicateMatches = (predicate, block) => {
		if (typeof predicate == "string")
			predicate = predicateFromSelector(predicate);

		if (   typeof predicate == "object"
			&& predicate instanceof Array) {
			let predicateArray = predicate;
			predicate = (block) => {
				return (predicateArray.findIndex(x => predicateMatches(x, block)) != -1);
			};
		}

		return (predicate(block) == true);
	};

	for (let [ predicate, result, adjustable = true ] of GW.layout.blockSpacing) {
		if (predicateMatches(predicate, block)) {
			let bsm = (typeof result == "function")
					  ? result(block)
					  : result;

			if (adjustable) {
				for (let [ predicate, transform ] of GW.layout.blockSpacingAdjustments)
					if (predicateMatches(predicate, block))
						bsm = Math.max(0, transform(bsm, block));
			}

			return bsm;
		}
	}

	return undefined;
}


/*********************/
/* LAYOUT PROCESSORS */
/*********************/

/*************************************************************************/
/*	Apply block layout classes to appropriate elements in given container.
 */
addLayoutProcessor(GW.layout.applyBlockLayoutClassesInContainer = (container) => {
	//	Designate headings.
	container.querySelectorAll(selectorize(range(1, 6).map(x => `h${x}`))).forEach(heading => {
		heading.classList.add("heading");
	});

	//	Designate floats.
	container.querySelectorAll(selectorize([
		".float-left",
		".float-right"
	])).forEach(floatBlock => {
		floatBlock.classList.add("float");
	});

	//	Designate lists.
	container.querySelectorAll(selectorize([
		"ul",
		"ol"
	])).forEach(list => {
		list.classList.add("list");
	});

	//	Designate float-containing lists.
	container.querySelectorAll(".markdownBody li .float").forEach(floatBlock => {
		let options = { alsoBlockContainers: [ ".list" ] };
		let container = blockContainerOf(floatBlock, options);
		while (container?.matches(".list")) {
			container.classList.add("has-floats");
			container = blockContainerOf(container, options);
		}
	});

	//	Designate “big lists”.
	/*	If any of a list’s list items have multiple non-list children, then 
		it is a “big list” (with consequences for block spacing).
	 */
	let isBigList = (list) => {
		if (list.matches(".list") != true)
			return false;

		for (let listItem of list.children) {
			if (childBlocksOf(listItem, { notWrapperElements: [ ".list" ] }).filter(x => x.matches(".list") != true).length > 1)
				return true;
		}

		return false;
	};
	container.querySelectorAll(".list").forEach(list => {
		let bigList = isBigList(list);

		/*	If this is a sub-list, and any other sub-lists on the same level as 
			this one are “big lists”, then this is also a “big list” (because
			the designation of “bigness” is applied to *list levels within a
			list tree*, not to individual lists).
		 */
		 let container = blockContainerOf(list, { alsoBlockContainers: [ "li" ] });
		 if (container?.matches("li")) {
		 	for (let listItem of container.parentElement.children) {
				if (childBlocksOf(listItem, { notWrapperElements: [ ".list" ] }).findIndex(x => isBigList(x)) != -1) {
					bigList = true;
					break;
				}
		 	}
		 }

		list.classList.toggle("big-list", bigList);
	});

	//	Apply special block sequence classes.
	container.querySelectorAll(selectorize(GW.layout.blockElements)).forEach(block => {
		if (block.closest(GW.layout.blockLayoutExclusionSelector))
			return;

		/*	Designate blocks preceded by nothing (not counting floats and other 
			elements that do not participate in block flow) in their block 
			container (the .first-block class).

			(Headings do not normally count as layout blocks, but they do here,
			 except headings of collapsible sections.)
		 */
		block.classList.toggle("first-block", previousBlockOf(block, {
			alsoBlockElements: [ "section:not(.collapse) > .heading" ]
		}) == null);

		//	Apply special paragraph classes.
		if (block.matches("p") == true) {
			//	Empty paragraphs (the .empty-graf class; not displayed).
			let empty = isNodeEmpty(block);
			block.classList.toggle("empty-graf", empty);
			if (empty)
				return;

			/*	Paragraphs preceded directly by other paragraphs (not in lists)
				(the .first-graf class).
			 */
			let strictPreviousBlock = previousBlockOf(block, { 
				notWrapperElements: [ "li", ".list" ], 
				notHalfWrapperElements: [ "section" ] 
			});
			if (   strictPreviousBlock?.matches("p") != true
				|| block.matches(".text-center") == true
				|| strictPreviousBlock?.matches(".text-center") == true
				|| strictPreviousBlock?.matches(".page-description-annotation") == true) {
				block.classList.toggle("first-graf", true);
			}
			
			/*	Colon-terminated paragraphs followed by lists 
				(the .list-heading class).
			 */
			let strictNextBlock = nextBlockOf(block, { 
				alsoBlockElements: [ ".list" ], 
				notWrapperElements: [ ".list" ]
			});
			if (   strictNextBlock?.matches(".list")
				&& block.textContent.trim().endsWith(":")) {
				block.classList.toggle("list-heading", true);
			}

			/*	Introductory paragraphs of documents or self-contained parts
				of documents (the .intro-graf class).
			 */
			if (   block.matches(".text-center") != true
				&& block.closest("#footer") == null
				&& block.firstElementChild?.matches("span.smallcaps") != true
				&& block.firstChild instanceof HTMLAnchorElement != true) {
				let isFirstWithin = (block, containerSelector, options) => {
					return (   blockContainerOf(block, options)?.matches(containerSelector) == true
							&& previousBlockOf(block, options) == null);
				}
				let options = { alsoSkipElements: [ ".epigraph" ], alsoBlockContainers: [ "li" ] };
				if (   isFirstWithin(block, "#markdownBody", options)
					|| (   isFirstWithin(block, "section", options)
						&& isFirstWithin(blockContainerOf(block), "#markdownBody"))
					|| previousBlockOf(block, options)?.matches(".abstract blockquote, #page-metadata"))
					block.classList.toggle("intro-graf", true);
			}
		}

		//	Designate blocks in lists (the .in-list class).
		block.classList.toggle("in-list", blockContainerOf(block, { alsoBlockContainers: [ "li" ] })?.matches("li") == true);
	});

	//	Designate those list items which need more spacing (when not indenting).
	if (indentModeActive(container) == false) {
		container.querySelectorAll(selectorize([ "li:not(.footnote)" ])).forEach(listItem => {
			if (listItem.closest(GW.layout.blockLayoutExclusionSelector))
				return;

			if (   listItem.closest(".list").matches(".big-list")
				&& previousBlockOf(firstBlockOf(listItem))?.matches("p, blockquote") == true)
				listItem.dataset.bsmMod = "2";
		});
	}
});

/**********************************************/
/*	Apply block spacing in the given container.
 */
addLayoutProcessor(GW.layout.applyBlockSpacingInContainer = (container) => {
	//	Apply block spacing.
	container.querySelectorAll(selectorize(GW.layout.blockElements)).forEach(block => {
		if (block.closest(GW.layout.blockLayoutExclusionSelector))
			return;

		let bsm = getBlockSpacingMultiplier(block);
		if (bsm != undefined) {
			block.classList.add("block");
			block.style.setProperty("--bsm", `${bsm}`);
		} else {
			block.classList.remove("block");
			block.style.removeProperty("--bsm");
		}
	});

	//	Lists require special treatment.
	container.querySelectorAll(selectorize([ "li:not(.footnote)" ])).forEach(listItem => {
		if (listItem.closest(GW.layout.blockLayoutExclusionSelector))
			return;

		let firstBlockWithin = firstBlockOf(listItem);
		let bsm = firstBlockWithin?.style.getPropertyValue("--bsm") ?? 0;
		if (bsm) {
			if (listItem.dataset.bsmMod) {
				bsm = "" + (parseInt(bsm) + parseInt(listItem.dataset.bsmMod));
				delete listItem.dataset.bsmMod;
			}

			/*	We must propagate the spacing of the first block within the 
				list item to the list item itself.
			 */
			listItem.classList.add("block");
			listItem.style.setProperty("--bsm", bsm);
			firstBlockWithin.style.setProperty("--bsm", 0);
		}
	});
});

