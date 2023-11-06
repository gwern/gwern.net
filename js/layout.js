/**********/
/* LAYOUT */
/**********/

GW.layout = {
	optionsCache: { },

	blockContainersNeedingLayout: [ ],

	layoutProcessors: [ ],

	//	Block containers 
	blockContainers: [
		".markdownBody",
		"section",
		".collapse",
		"blockquote",
		".epigraph",
		".admonition",
		".sidenote",
		"#x-of-the-day"
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
		".TOC",
		".interview .exchange",
		".interview .utterance"
	],

	//	Wrappers are transparent at the top and bottom.
	wrapperElements: [
		"div",
		"span",
		".list",
		"li",
		".parsed-raw-block"
	],

	//	Half-wrappers are transparent at the bottom only, not the top.
	halfWrapperElements: [
		"section"
	],

	//	Elements which do not participate in block layout.
	skipElements: [
		".empty",
		".empty-graf",
		".hidden",
		".float",
		"noscript",
		"button",
		"a:empty",
		".heading.collapse"
	],

	//	Elements which always participate in block layout, even when empty.
	nonEmptyElements: [
		"hr"
	],

	//	Do not apply block layout classes within these containers.
	blockLayoutExclusionSelector: [
		"#page-metadata",
		".TOC > *"
	].join(", "),

	blockSpacing: [
		[ "body.page-index .abstract > p.first-block",	 7, false ],
		[ "body.page-index section",					 7, false ],
		[ "body.page-index section li p + p",			 0, false ],

		[ ".float.first-block",			 2, false ],
		[ ".first-block",				 0, false ],

		[ ".heading + section",			 5, false ],
		[ ".heading + *",				 4, false ],

		[ ".annotation p.data-field.title + p.data-field",	
										 1, false ],
		[ ".annotation p.data-field.title + .data-field.annotation-abstract",
										 3, false ],

		[ ".aux-links-append + .aux-links-append",
										 0, false ],
		[ ".collapse.expanded-not p.aux-links-list-label + *",
										 0, false ],

		[ ".interview .exchange",		 4, false ],
		[ ".interview .utterance",		 2, false ],

		[ ".truncated + p", 			 4, false ],

		[ "p.footnote-back-block",		 1, false ],
		[ "p.first-graf",				10 ],
		[ "p.list-heading",				10 ],
		[ "p",							 0 ],

		[ ".TOC",						10 ],

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

		[ ".aux-links-append .columns",	 4 ],
		[ ".columns",					 6 ],

		[ "figure.outline-not",			 9 ],
		[ "figure",						10 ],

		[ "blockquote",					10 ],

		[ ".epigraph",					 6 ],

		[ "div.table-wrapper",			10 ],

		[ "div.sourceCode",				10 ],

		[ ".math.block",				10 ],

		[ ".admonition",				10 ],
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

		[ ".aux-links-append p + .aux-links-append p.list-heading",
											(bsm, block) => bsm + 2 ],

		[ "figcaption *",					(bsm, block) => bsm - 2 ],

		[ ".TOC + .collapse-block",		 	(bsm, block) => bsm - 4 ],
	]
};

//	Add support for .desktop-not and .mobile-not classes.
GW.layout.skipElements.push(GW.mediaQueries.mobileWidth.matches ? ".mobile-not" : ".desktop-not");

GW.layout.defaultOptions = processLayoutOptions({
	blockContainers: GW.layout.blockContainers,
	blockElements: GW.layout.blockElements,
	skipElements: GW.layout.skipElements,
	nonEmptyElements: GW.layout.nonEmptyElements,
	wrapperElements: GW.layout.wrapperElements,
	halfWrapperElements: GW.layout.halfWrapperElements
});

/**********************************************************************/
/*	Registers a layout processor function, which will be applied to all 
	rendered content as part of the dynamic layout process.
 */
function addLayoutProcessor(name, processor, options = { }) {
	//	Reference for easy direct calling.
	GW.layout[name] = processor;

	//	Add to layout processor list.
	GW.layout.layoutProcessors.push([ name, processor, options ]);
}

/****************************************************/
/*	Activates dynamic layout for the given container.
 */
function startDynamicLayoutInContainer(container) {
	let containingDocument = container.getRootNode();

	let blockContainersSelector = selectorize(GW.layout.blockContainers);

	let observer = new MutationObserver((mutationsList, observer) => {
		let baseDocumentLocation = baseLocationForDocument(mutationsList.first?.target?.getRootNode());

		//	Construct list of all block containers affected by these mutations.
		let affectedBlockContainers = [ ];

		for (mutationRecord of mutationsList) {
			//	Find block container in which the mutated element is contained.
			let nearestBlockContainer = mutationRecord.target.closest(blockContainersSelector);

			//	Avoid adding a container twice, and apply exclusions.
			if (   nearestBlockContainer
				&& affectedBlockContainers.includes(nearestBlockContainer) == false
				&& nearestBlockContainer.closest(GW.layout.blockLayoutExclusionSelector) == null)
				affectedBlockContainers.push(nearestBlockContainer);
		}

		/*	Exclude block containers that are contained within other block 
			containers in the list, to prevent redundant processing.
		 */
		affectedBlockContainers = affectedBlockContainers.filter(c => affectedBlockContainers.findIndex(x => 
			(c.compareDocumentPosition(x) & Node.DOCUMENT_POSITION_CONTAINS)
		) == -1);

		/*	Add containers to list of containers needing layout processing, if
			they are not there already.
		 */
		affectedBlockContainers.forEach(affectedBlockContainer => {
			if (GW.layout.blockContainersNeedingLayout.includes(affectedBlockContainer) == false)
				GW.layout.blockContainersNeedingLayout.push(affectedBlockContainer);
		});
		requestAnimationFrame(() => {
			GW.layout.currentPassBegin = performance.now();

			//	Do layout in all waiting block containers.
			while (GW.layout.blockContainersNeedingLayout.length > 0) {
				let nextBlockContainer = GW.layout.blockContainersNeedingLayout.shift();
				GW.layout.layoutProcessors.forEach(layoutProcessor => {
					let [ name, processor, options ] = layoutProcessor;

					let info = {
						container: nextBlockContainer,
						baseLocation: baseDocumentLocation
					};
					if (options.condition?.(info) == false)
						return;

					processor(nextBlockContainer);

					GW.notificationCenter.fireEvent("Layout.layoutProcessorDidComplete", {
						document: containingDocument,
						container: container,
						processorName: name,
						processorOptions: options,
						blockContainer: nextBlockContainer
					});
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

/*****************************************************************************/
/*	Process layout options object, so that it contains all the appropriate
	defaults (from GW.layout). (This must be done before the options object is
	read or used in any way except being passed to another function.)
 */
function processLayoutOptions(options) {
	if (options == null)
		return GW.layout.defaultOptions;

	if (options["blockElementsSelector"] != null)
		return options;

	let cacheKey = options.cacheKey;
	if (cacheKey == null) {
		cacheKey = "";
		for (let [ key, value ] of Object.entries(options))
			cacheKey += `| ${key}: ` + value.join(", ");
		options.cacheKey = cacheKey;
	}
	if (GW.layout.optionsCache[cacheKey])
		return GW.layout.optionsCache[cacheKey];

	[	"blockContainers",
		"blockElements",
		"skipElements",
		"nonEmptyElements",
		"wrapperElements",
		"halfWrapperElements"
	].forEach(optionKey => {
		let option = options[optionKey];
		if (option == null) {
			option = GW.layout[optionKey];

			let capitalizedOptionKey = optionKey.slice(0, 1).toUpperCase() + optionKey.slice(1);

			let alsoOption = options["also" + capitalizedOptionKey];
			if (alsoOption != null)
				option = option.concat(alsoOption);

			let notOption = options["not" + capitalizedOptionKey];
			if (notOption != null)
				option = option.filter(x => notOption.includes(x) == false);

			options[optionKey] = option;
		}

		if ([ "wrapperElements", "halfWrapperElements" ].includes(optionKey) == false)
			options[optionKey + "Selector"] = option.join(", ");
	});

	options.wrapperOptions = { };

	let topFilter = (x => [ ...options.wrapperElements, ...options.halfWrapperElements ].includes(x) == false);
	options.wrapperOptions["downOut"] = {
		blockElementsSelector: options.blockElements.filter(topFilter).join(", "),
		blockContainersSelector: options.blockContainers.filter(topFilter).join(", "),
		wrappersSelector: [ ...options.wrapperElements, ...options.halfWrapperElements ].join(", ")
	};
	options.wrapperOptions["upIn"] = options.wrapperOptions["downOut"];

	let bottomFilter = (x => options.wrapperElements.includes(x) == false);
	options.wrapperOptions["downIn"] = {
		blockElementsSelector: options.blockElements.filter(bottomFilter).join(", "),
		blockContainersSelector: options.blockContainers.filter(bottomFilter).join(", "),
		wrappersSelector: options.wrapperElements.join(", ")
	};
	options.wrapperOptions["upOut"] = options.wrapperOptions["downIn"];

	GW.layout.optionsCache[cacheKey] = options;

	return options;
}

/******************************************************************/
/*	Generate element layout cache key for given action and options.
	(Or, just use provided cache key, if any.)
 */
function generateCacheKey(action, options) {
	return `${action} ${options.cacheKey}`;
}

/***************************************************************************/
/*	Retrieve desired result from element’s layout cache, or calculate it and 
	store in element’s layout cache; and, in any case, return.
 */
function useLayoutCache(element, uniqueKey, options, f) {
	options = processLayoutOptions(options);

	let cacheKey = generateCacheKey(uniqueKey, options);

	if (  (element.layoutCache?.time ?? 0) < GW.layout.currentPassBegin
		|| element.layoutCache[cacheKey] == null) {
		if ((element.layoutCache?.time ?? 0) < GW.layout.currentPassBegin)
			element.layoutCache = { time: GW.layout.currentPassBegin };

		element.layoutCache[cacheKey] = f(element, options);
	}

	return element.layoutCache[cacheKey];
}

/***************************************************************************/
/*	Returns true if element is a wrapper of the given type, false otherwise.

	Types: upOut, downOut, upIn, downIn
 */
function isWrapper(element, wrapperType, options) {
	if (element == null)
		return null;

	return useLayoutCache(element, "isWrapper", options, (element, options) => {
		return (   element?.matches(options.wrapperOptions[wrapperType].wrappersSelector) == true
				&& element?.matches(options.wrapperOptions[wrapperType].blockElementsSelector) != true
				&& element?.matches(options.wrapperOptions[wrapperType].blockContainersSelector) != true);
	});
}

/*****************************************************************/
/*	Returns true if element is a skipped element, false otherwise.
 */
function isSkipped(element, options) {
	if (element == null)
		return null;

	return useLayoutCache(element, "isSkipped", options, (element, options) => {
		return (element?.matches(options.skipElementsSelector) == true);
	});
}

/**************************************************************/
/*	Returns true if element is a layout block, false otherwise.
 */
function isBlock(element, options) {
	if (element == null)
		return null;

	return useLayoutCache(element, "isBlock", options, (element, options) => {
		return (element?.matches(options.blockElementsSelector) == true);
	});
}

/***************************************************************************/
/*	Returns true if element is an always-not-empty element, false otherwise.
 */
function isNonEmpty(element, options) {
	if (element == null)
		return null;

	return useLayoutCache(element, "isNonEmpty", options, (element, options) => {
		return (element?.matches(options.nonEmptyElementsSelector) == true);
	});
}

/******************************************************************/
/*	Returns nearest enclosing block container of the given element.
	(Might be null.)
 */
function blockContainerOf(element, options) {
	if (element == null)
		return null;

	return useLayoutCache(element, "blockContainer", options, (element, options) => {
		return element.parentElement?.closest(options.blockContainersSelector);
	});
}

/**************************************************************************/
/*	Returns layout block sequential (next or previous) to the given element
	in the block flow.
 */
function sequentialBlockOf(element, direction, options) {
	if (element == null)
		return null;

	options = processLayoutOptions(options);

	let siblingKey = direction + "ElementSibling";
	let wrapperDirection = (direction == "next" ? "down" : "up");
	let wrapperInType = wrapperDirection + "In";
	let wrapperOutType = wrapperDirection + "Out";
	let terminus = (direction == "next" ? "first" : "last");

	//	Look inside “transparent” wrappers (that don’t affect layout).
	if (isWrapper(element[siblingKey], wrapperInType, options)) {
		let terminalBlock = terminalBlockOf(element[siblingKey], terminus, options);
		if (terminalBlock)
			return terminalBlock;
	}
	
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
function previousBlockOf(element, options) {
	if (element == null)
		return null;

	return useLayoutCache(element, "previousBlock", options, (element, options) => {
		return sequentialBlockOf(element, "previous", options);
	});
}

/************************************************************************/
/*	Returns layout block next from the given element in the block flow.
	(Might be null.)
 */
function nextBlockOf(element, options) {
	if (element == null)
		return null;

	return useLayoutCache(element, "nextBlock", options, (element, options) => {
		return sequentialBlockOf(element, "next", options);
	});
}

/***************************************************************/
/*	Returns terminal (first or last) layout block of an element.
	(Might be the element itself, or null.)
 */
function terminalBlockOf(element, terminus, options, strictDescent = false) {
	if (element == null)
		return null;

	options = processLayoutOptions(options);

	let wrapperType = (terminus == "first" ? "down" : "up") + "In";

	//	Look inside wrappers (or any block, if strictDescent is specified).
	if (   strictDescent == true
		|| isWrapper(element, wrapperType, options)) {
		let childBlocks = childBlocksOf(element);
		for (let i  = (terminus == "first" ? 0                  : childBlocks.length - 1); 
				 i != (terminus == "first" ? childBlocks.length : -1); 
			     i += (terminus == "first" ? 1                  : -1)) {
			let terminalBlock = terminalBlockOf(childBlocks[i], terminus, options);
			if (   terminalBlock
				&& isSkipped(terminalBlock, options) == false
				&& (   isNodeEmpty(terminalBlock) == false
					|| isNonEmpty(terminalBlock, options) == true))
				return terminalBlock;
		}
	}

	//	The element itself is a layout block (only if no strictDescent).
	if (   strictDescent == false
		&& isBlock(element, options))
		return element;

	return null;
}

/*******************************************/
/*	Returns last layout block of an element.
	(Might be the element itself, or null.)
 */
function lastBlockOf(element, options, strictDescent) {
	if (element == null)
		return null;

	if (strictDescent) {
		return terminalBlockOf(element, "last", options, true);
	} else {
		return useLayoutCache(element, "lastBlock", options, (element, options) => {
			return terminalBlockOf(element, "last", options);
		});
	}
}

/********************************************/
/*	Returns first layout block of an element.
	(Might be the element itself, or null.)
 */
function firstBlockOf(element, options, strictDescent) {
	if (element == null)
		return null;

	if (strictDescent) {
		return terminalBlockOf(element, "first", options, true);
	} else {
		return useLayoutCache(element, "firstBlock", options, (element, options) => {
			return terminalBlockOf(element, "first", options);
		});
	}
}

/***************************************************************************/
/*	Returns all “child” blocks of an element (blocks that are descended from 
	the given element with no other blocks in the chain of descent; wrappers 
	don’t count).
 */
function childBlocksOf(element, options) {
	if (element == null)
		return null;

	return useLayoutCache(element, "childBlocks", options, (element, options) => {
		options = processLayoutOptions(options);

		let childBlocks = Array.from(element.children);

		for (let i = 0; i < childBlocks.length; i++) {
			if (isWrapper(childBlocks[i], "downIn", options)) {
				childBlocks.splice(i, 1, ...childBlocks[i].children);
				i--;
			} else if (isBlock(childBlocks[i], options) == false) {
				childBlocks.splice(i, 1);
				i--;
			}
		}

		return childBlocks;
	});
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
function getBlockSpacingMultiplier(block, debug = false) {
	let predicateFromSelector = (selector) => {
		let parts = selector.match(/^(.+) \+ (.+)$/);
		if (parts) {
			/*	Headings do not normally count as layout blocks, but they do 
				here (unless it’s a heading of a collapse section, in which case
				it still doesn’t count as a layout block).
			 */
			return (block) => (   previousBlockOf(block, {
									alsoBlockElements: [ "section:not(.collapse) > .heading" ],
									cacheKey: "alsoBlocks_nonCollapseSectionHeadings"
								  })?.matches(parts[1]) 
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

	if (debug)
		console.log(block);
	for (let [ predicate, result, adjustable = true ] of GW.layout.blockSpacing) {
		if (predicateMatches(predicate, block)) {
			if (debug)
				console.log(predicate);
			let bsm = (typeof result == "function")
					  ? result(block)
					  : result;

			if (adjustable) {
				for (let [ predicate, transform ] of GW.layout.blockSpacingAdjustments)
					if (predicateMatches(predicate, block)) {
						if (debug)
							console.log(predicate);
						bsm = Math.max(0, transform(bsm, block));
					}
			}

			return bsm;
		}
	}

	return undefined;
}

/*****************************************************************************/
/*	Returns a block’s drop cap type (‘goudy’, ‘yinit’, etc.), or null if none.
 */
function dropCapTypeOf(block) {
	return Array.from(block.classList).find(cssClass => /^drop-caps?-/.test(cssClass))?.replace("-caps-", "-cap-")?.slice("drop-cap-".length);
}

/******************************************************************************/
/*	Adds a drop cap class to a block. Drop caps may be ‘kanzlei’, ‘de-zs’, etc.
	(See default.css for the list.)
 */
function addDropCapClassTo(block, dropCapType) {
	if (block.classList.contains("force-drop-cap"))
		return;

	stripDropCapClassesFrom(block);

	block.classList.add(`drop-cap-${dropCapType}`);
}

/*************************************/
/*	Strip drop cap classes from block.
 */
function stripDropCapClassesFrom(block) {
	if (block.classList.contains("force-drop-cap"))
		return;

	block.classList.remove(...(Array.from(block.classList).filter(className => className.startsWith("drop-cap-"))));
}


/*********************/
/* LAYOUT PROCESSORS */
/*********************/

/*************************************************************************/
/*	Apply block layout classes to appropriate elements in given container.
 */
addLayoutProcessor("applyBlockLayoutClassesInContainer", (container) => {
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
		let options = {
			alsoBlockContainers: [ ".list" ],
			cacheKey: "alsoBlockContainers_lists"
		};
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
			if (childBlocksOf(listItem, {
					notWrapperElements: [ ".list" ],
					cacheKey: "notWrappers_lists"
				}).filter(x => x.matches(".list") != true).length > 1)
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
		 let container = blockContainerOf(list, {
		 	alsoBlockContainers: [ "li" ],
		 	cacheKey: "alsoBlockContainers_listItems"
		 });
		 if (container?.matches("li")) {
		 	for (let listItem of container.parentElement.children) {
				if (childBlocksOf(listItem, {
						notWrapperElements: [ ".list" ],
						cacheKey: "notWrappers_lists"
					}).findIndex(x => isBigList(x)) != -1) {
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

			Headings do not normally count as layout blocks, but they do here 
			(unless it’s a heading of a collapse section, in which case it still
			 doesn’t count as a layout block).
		 */
		block.classList.toggle("first-block", previousBlockOf(block, {
			alsoBlockElements: [ "section:not(.collapse) > .heading" ],
			cacheKey: "alsoBlocks_nonCollapseSectionHeadings"
		}) == null);

		//	Designate blocks in lists (the .in-list class).
		block.classList.toggle("in-list", blockContainerOf(block, {
			alsoBlockContainers: [ "li" ],
			cacheKey: "alsoBlockContainers_listItems"
		})?.matches("li") == true);

		//	Apply special paragraph classes.
		if (block.matches("p") == true) {
			//	Empty paragraphs (the .empty-graf class; not displayed).
			let emptyGraf = isNodeEmpty(block);
			block.classList.toggle("empty-graf", emptyGraf);
			if (emptyGraf)
				return;

			/*	Paragraphs not preceded directly by other paragraphs 
				(not in lists) (the .first-graf class).
			 */
			let firstGraf = false;
			let previousBlockSelector = [
				":not(p)",
				".text-center",
				".margin-notes-block",
				".page-description-annotation",
				".annotation .data-field"
			].join(", ");
			let strictPreviousBlock = previousBlockOf(block, { 
				alsoBlockElements: [ ".list" ],
				notWrapperElements: [ "li", ".list" ], 
				notHalfWrapperElements: [ "section" ],
				cacheKey: "alsoBlocks_lists_notWrappers_listsAndListItems_notHalfWrappers_sections"
			});
			if (   strictPreviousBlock == null
				|| strictPreviousBlock.matches(previousBlockSelector) == true)
				firstGraf = true;
			block.classList.toggle("first-graf", firstGraf);
			
			/*	Colon-terminated paragraphs followed by lists 
				(the .list-heading class).
			 */
			let listHeading = false;
			let strictNextBlock = nextBlockOf(block, { 
				alsoBlockElements: [ ".list" ], 
				notWrapperElements: [ ".list" ],
				cacheKey: "alsoBlocks_lists_notWrappers_lists"
			});
			if (   strictNextBlock?.matches(".list")
				&& block.textContent.trim().endsWith(":"))
				listHeading = true;
			block.classList.toggle("list-heading", listHeading);

			/*	Introductory paragraphs of documents or self-contained parts
				of documents (the .intro-graf class).
			 */
			let introGraf = false;
			if (   block.matches(".text-center, .margin-notes-block") != true
				&& block.matches(".in-list") != true
				&& block.closest("#footer, figcaption") == null
				&& block.firstElementChild?.matches("span.smallcaps") != true) {
				let isFirstWithin = (block, containerSelector, options) => {
					return (   blockContainerOf(block, options)?.matches(containerSelector) == true
							&& previousBlockOf(block, options) == null);
				}
				let options = {
					alsoSkipElements: [ ".epigraph", ".margin-notes-block" ],
					alsoBlockContainers: [ "li" ],
					cacheKey: "alsoSkip_epigraphs_alsoBlockContainers_listItems"
				};

				let previousBlock = previousBlockOf(block, options);
				if (   isFirstWithin(block, "#markdownBody", options)
					|| (   isFirstWithin(block, "section", options)
						&& isFirstWithin(blockContainerOf(block), "#markdownBody"))
					|| previousBlock?.matches(".abstract blockquote, #page-metadata"))
					introGraf = true;

				//	The .intro-graf class also implies .first-graf.
				if (introGraf)
					block.classList.add("first-graf");

				/*  Add drop cap class. This could be set globally, or 
					overridden by a .abstract; the latter could be 
					`drop-cap-not` (which nullifies any page-global drop-cap 
					class for the given block).
				 */
				let dropCapType = null;
				if (introGraf) {
					dropCapType = (previousBlock?.matches(".abstract blockquote")
								   ? dropCapTypeOf(previousBlock)
								   : null) ?? dropCapTypeOf(document.body);
				} else if (block.parentElement?.tagName == "DIV") {
					dropCapType = dropCapTypeOf(block.parentElement);
					if (dropCapType && dropCapType != "not")
						block.classList.add("first-graf");
				}
				if (dropCapType && dropCapType != "not") {
					addDropCapClassTo(block, dropCapType);
				} else {
					stripDropCapClassesFrom(block);
				}
			}
			block.classList.toggle("intro-graf", introGraf);
		}
	});
});

/*******************************************************************************/
/*	Run given callback on given container immediately and also at any later
	time when block layout classes are updated in that container (e.g., <body>).
 */
function processContainerNowAndAfterBlockLayout(container, callback) {
	//	Run immediately...
	callback(container);

	//	... and also add event listener for if block layout classes are updated.
	GW.notificationCenter.addHandlerForEvent("Layout.layoutProcessorDidComplete", (layoutEventInfo) => {
		callback(container);
	}, {
		condition: (layoutEventInfo) => (   layoutEventInfo.container == container
										 && layoutEventInfo.processorName == "applyBlockLayoutClassesInContainer")
	});
}

/**********************************************/
/*	Apply block spacing in the given container.
 */
addLayoutProcessor("applyBlockSpacingInContainer", (container) => {
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

	//	Apply list spacing.
	container.querySelectorAll(selectorize([ "li:not(.footnote)" ])).forEach(listItem => {
		if (listItem.closest(GW.layout.blockLayoutExclusionSelector))
			return;

		let firstBlockWithin = firstBlockOf(listItem);
		let bsm = firstBlockWithin?.style.getPropertyValue("--bsm");

		//	Apply list item BSM modifier.
		if (   bsm > "" 
			&& listItem.dataset.bsmMod > "")
			bsm = "" + (parseInt(bsm) + parseInt(listItem.dataset.bsmMod));

		//	Apply BSM.
		if (bsm > "") {
			/*	We must propagate the spacing of the first block within the 
				list item to the list item itself.
			 */
			listItem.classList.add("block");
			listItem.style.setProperty("--bsm", bsm);

			//	Avoid double-counting.
			if (   firstBlockWithin != null 
				&& firstBlockWithin != listItem)
				firstBlockWithin.style.setProperty("--bsm", 0);
		}

		//	Delete now-extraneous data attribute.
		if (listItem.dataset.bsmMod)
			delete listItem.dataset.bsmMod;
	});

	//	Floats require special treatment.
	container.querySelectorAll(selectorize([ ".float" ])).forEach(floatBlock => {
		if (floatBlock.closest(GW.layout.blockLayoutExclusionSelector))
			return;

		//	Floating elements take on the top margin of the next block.
		let nextBlock = nextBlockOf(floatBlock, {
			alsoBlockElements: [ "li" ],
			cacheKey: "alsoBlocks_listItems"
		});
		let nextBlockBSM = nextBlock?.style?.getPropertyValue("--bsm");
		if (nextBlockBSM) {
			/*	If the next block (in the strict sense, i.e. not counting list 
				items!) is a paragraph, then adjust margin.
			 */
			let strictNextBlock = nextBlockOf(floatBlock);
			if (strictNextBlock.matches("p"))
				nextBlockBSM = "" + (parseInt(nextBlockBSM) + 2);

			floatBlock.style.setProperty("--bsm", nextBlockBSM);
		}
	});
});

/****************************************************************************/
/*	Apply block layout classes to a document fragment, to make them available
	to any other load handlers (rewrite functions).
 */
addContentLoadHandler(GW.contentLoadHandlers.applyBlockLayoutClassesInDocumentFragment = (eventInfo) => {
    GWLog("applyBlockLayoutClassesInDocumentFragment", "layout.js", 1);

	GW.layout.applyBlockLayoutClassesInContainer(eventInfo.container);
}, "<rewrite", (info) => (info.container instanceof DocumentFragment));

/*********************************************************************/
/*	Apply block spacing in collapse block when collapse state changes.
 */
GW.notificationCenter.addHandlerForEvent("Collapse.collapseStateDidChange", (eventInfo) => {
	GWLog("applyBlockSpacingInCollapseBlockOnStateChange", "layout.js", 2);

	GW.layout.applyBlockSpacingInContainer(eventInfo.collapseBlock);
});