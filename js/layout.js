/**********/
/* LAYOUT */
/**********/

GW.layout = {
	initialPageLayoutComplete: false,

	optionsCache: { },

	blockContainersNeedingLayout: [ ],

	layoutProcessors: [ ],

	//	Block containers
	blockContainers: [
		".markdownBody",
		"section",
		".collapse-block",
		"blockquote",
		".epigraph",
		".admonition",
		".sidenote",
		"th",
		"td",
		"#x-of-the-day"
	],

	//	Block elements get layout classes applied to them.
	blockElements: [
		"section",
		".collapse-block",
		"blockquote",
		".epigraph",
		"p",
		".columns",
		".footnote",
		"figure",
		"iframe",
		"hr",
		"div.sourceCode",
		".table-wrapper",
		".math.block",
		".admonition",
		".TOC",
		".interview .exchange",
		".interview .utterance"
	],

	//	Complex block elements can contain other blocks.
	complexBlockElementSelector: [
		"section",
		".collapse-block",
		"blockquote",
		".epigraph",
		".columns",
		".footnote",
		"figure",
		".admonition",
		".interview .exchange",
		".interview .utterance"
	].join(", "),

	//	These block types, if they precede a <p>, do not make it not-first.
	firstGrafAllowablePreviousBlockSelector: [
		":not(p)",
		".text-center",
		".text-right",
		".section-metadata",
		".margin-notes-block",
		".page-description-annotation",
		".data-field",
		".admonition-title > p"
	].join(", "),

	//	Wrappers are transparent at the top and bottom.
	wrapperElements: [
		"div",
		"span",
		".list",
		"li",
		"figcaption",
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
		".heading.collapse",
		".abstract-collapse-only"
	],

	//	Elements which always participate in block layout, even when empty.
	nonEmptyElements: [
		"hr"
	],

	//	Do not apply block layout classes within these elements.
	blockLayoutExclusionSelector: [
		"#page-metadata",
		".TOC > *",
		".popframe"
	].join(", "),

	//	Isolate layout within these elements.
	layoutIsolationSelector: [
		".sidenote-column"
	].join(", "),

	emptyNodeExclusionPredicate: (node) => {
		if (node.nodeType != Node.ELEMENT_NODE)
			return false;

		/*	Exclude elements that have any classes (discounting 
			classes added by the layout system).
		 */
		let layoutClasses = [
			"block",
			"first-block",
			"last-block",
			"empty-graf",
			"first-graf",
			"intro-graf",
			"list",
			"in-list",
			"float",
			"has-floats",
			"overlap-not",
			"force-dropcap",
			"heading"
		];
		for (let cssClass of node.classList)
			if (   layoutClasses.includes(cssClass) == false
				&& /^dropcaps?-/.test(cssClass) == false)
				return true;

		//	Exclude elements that have any data attributes.
		if (isIterableEmpty(node.dataset) == false)
			return true;

		return false;
	},

	siblingCombinatorRegExp: /^(.+) \+ (.+)$/,

	blockSpacing: [
		[ "body.page-index section",	 9, false ],
		[ "body.page-index section li p + p",
										 0, false ],

		[ "body.page-404 .display-entry .float + .epigraph",
										 0, false ],
		[ "body.page-404 section#other-options", 
										16, false ],
		[ "body.page-404 #new-popular-notable section",
										13, false ],
		[ "body.page-404 #new-popular-notable section li p + p",
										 0, false ],

		[ "section#see-also.first-block",
										 4, false ],

		[ "#link-bibliography-section", 15, false ],

		[ "body.page-blog-index p.data-field.title:not(.first-block)",	
										10, false ],
		[ "body.blog-page p.data-field + .data-field.annotation-abstract p", 
										 7, false ],
		[ "body.blog-page .annotation.blog-post > .annotation-abstract > .aux-links-append + .aux-links-append",
										 4, false ],
		[ "body.blog-page .annotation.blog-post > .annotation-abstract > .aux-links-append + .file-include-collapse",
										 4, false ],

		[ ".float.first-block",			 2, false ],
		[ ".first-block",				 0, false ],

		[ ".heading + p.data-field.title",
										 2, false ],
		[ ".heading + section",			 5, false ],
		[ ".heading + *",				 4, false ],

		[ ".tweet .tweet-content",		 3, false ],
		[ ".tweet .tweet-content p",	 3, false ],
		[ ".tweet figure",				 8, false ],

		[ "p.data-field.title + p.data-field",
										 1, false ],
		[ "p.data-field.title + .data-field",
										 3, false ],
		[ "p.data-field.title + .file-include-collapse",
										 3, false ],

		[ ".annotation .data-field.file-includes .collapse + annotation .collapse",
										 5, false ],
		[ ".annotation .data-field + .annotation .collapse",
										 5, false ],
		[ ".annotation .collapse.bare-content + annotation .collapse.bare-content",
										 4, false ],
		[ ".annotation * + annotation .collapse.bare-content",
										 6, false ],

		[ ".section-backlinks-container + .section-backlinks-container",
										 4, false ],
		[ ".aux-links-append + .aux-links-append",
										 0, false ],
		[ ".collapse.expanded-not p.aux-links-list-label + *",
										 0, false ],
		[ ".aux-links-append + .file-include-collapse",
										 0, false ],

		[ ".interview .exchange",		 4, false ],
		[ ".interview .utterance",		 2, false ],

		[ ".admonition-title > p + p",   1, false ],

		[ "p.footnote-back-block",		 1, false ],
		[ "p.in-list + p.data-field",	10 ],
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

		[ "hr.horizontal-rule-small",    6 ],
		[ "hr",							10 ],

		[ ".aux-links-append .columns",	 4 ],
		[ ".columns",					 6 ],

		[ "figure.outline-not",			 9 ],
		[ "figure",						10 ],

		[ "iframe",						10 ],

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

		[ "hr.horizontal-rule-small + *",	(bsm, block) => bsm - 4 ],

		[ ".TOC + .collapse-block",		 	(bsm, block) => bsm - 4 ],
	]
};

//	Preprocess selectors.
function blockMatchesSelector(block, selector) {
	if (typeof selector == "string")
		return block.matches(selector);
	else
		return selector.matches(block);
}
function preprocessSelector(selector) {
	let parts = selector.match(GW.layout.siblingCombinatorRegExp);
	if (parts) {
		let selectorParts = parts.slice(1, 3).map(part => preprocessSelector(part));
		return {
			isGWSelector: true,
			parts: selectorParts,
			matches: (block) => {
				if (blockMatchesSelector(block, selectorParts[1]) == false)
					return false;

				/*	We want .foo + .bar to match in all of the following cases:
				
						<div class="foo">foo</div>
						<div class="bar">bar</div>

					and:

						<div class="foo float-right">foo</div>
						<div class="bar">bar</div>

					and:

						<div class="foo">foo</div>
						<div class="baz float-right">baz</div>
						<div class="bar">bar</div>

					Therefore we want floats to both count as blocks, and also
					to NOT count as blocks, at the same time. To be specific,
					if a float itself matches the preceding-block selector, then
					we count it as a match; but if it doesn’t, then we do not
					declare the match to have failed, but keep looking for other
					potentially matching blocks which might come before it.
				 */

				let previousBlock = block;
				do {
					previousBlock = previousBlockOf(previousBlock, {
										alsoBlockElements: [ "section:not(.collapse) > .heading" ],
										notSkipElements: [ ".float" ],
										cacheKey: "alsoBlocks_nonCollapseSectionHeadings_notSkip_floats"
									});

					if (previousBlock == null)
						return false;

					if (blockMatchesSelector(previousBlock, selectorParts[0]))
						return true;

					if (blockMatchesSelector(previousBlock, ".float") == false)
						return false;
				} while (true);
			}
		};
	} else {
		return selector;
	}
}
GW.layout.blockSpacing.forEach(entry => {
	let [ selector, result, adjustable = true ] = entry;
	entry.splice(0, 1, preprocessSelector(selector));
});
GW.layout.blockSpacingAdjustments.forEach(entry => {
	let [ selectorOrSelectorArray, adjustmentFunction ] = entry;
	if (typeof selectorOrSelectorArray == "string") {
		entry.splice(0, 1, preprocessSelector(selectorOrSelectorArray));
	} else {
		entry.splice(0, 1, selectorOrSelectorArray.map(selector => preprocessSelector(selector)));
	}
});

//	Add support for .desktop-not and .mobile-not classes.
GW.layout.skipElements.push(GW.mediaQueries.mobileWidth.matches ? ".mobile-not" : ".desktop-not");

//	Skip non-layout-containing blocks if they themselves appear in block flow.
GW.layout.skipElements.push(...(GW.layout.blockLayoutExclusionSelector.split(", ")));

//	Default block sequence function options.
GW.layout.defaultOptions = processLayoutOptions({
	blockContainers: GW.layout.blockContainers,
	blockElements: GW.layout.blockElements,
	skipElements: GW.layout.skipElements,
	nonEmptyElements: GW.layout.nonEmptyElements,
	wrapperElements: GW.layout.wrapperElements,
	halfWrapperElements: GW.layout.halfWrapperElements
});

//	Needed so that predicates (like isBlock()) can be called prior to layout.
GW.layout.currentPassBegin = 1;

/**********************************************************************/
/*	Registers a layout processor function, which will be applied to all
	rendered content as part of the dynamic layout process.

	Available option fields:

	blockLayout (boolean)
		If set to true (the default), then this will be treated as a block
		layout processor (and block layout exclusions will be applied to it).
		Otherwise, block layout exclusions will be ignored.

	condition (function)
		If provided, will be tested against an info object (which is the same
		as that passed to the ‘Layout.layoutProcessorDidComplete’ event) for
		each block container to which the layout processor might be applied;
		if false returned, the layout processor is skipped for that block.
 */
function addLayoutProcessor(name, processor, options) {
	options = Object.assign({
		blockLayout: true
	}, options);

	//	Reference for easy direct calling.
	GW.layout[name] = processor;

	//	Add to layout processor list.
	GW.layout.layoutProcessors.push({
		name: name,
		processor: processor,
		options: options
	});
}

/******************************************************************************/
/*	Applies given layout processor to given block container within the given
	container.

	Fires didComplete event for each time a layout processor fires.
 */
function applyLayoutProcessorToBlockContainer(processorSpec, blockContainer, container) {
	let containingDocument = container.getRootNode();

	if (processorSpec.options.condition?.({
			document: containingDocument,
			container: container,
			processorName: processorSpec.name,
			blockContainer: blockContainer
		}) == false)
		return;

	processorSpec.processor(blockContainer);

	GW.notificationCenter.fireEvent("Layout.layoutProcessorDidComplete", {
		document: container.getRootNode(),
		container: container,
		processorName: processorSpec.name,
		blockContainer: blockContainer
	});
}

/****************************************************/
/*	Activates dynamic layout for the given container.
 */
function startDynamicLayoutInContainer(container) {
	let blockContainersSelector = GW.layout.blockContainers.map(
		part => (part == ".markdownBody") ? part : (".markdownBody " + part)
	).join(", ");

	let observer = new MutationObserver((mutationsList, observer) => {
		//	Construct list of all block containers affected by these mutations.
		let affectedBlockContainers = [ ];
		for (let mutationRecord of mutationsList) {
			//	Find nearest ancestor block container.
			let nearestBlockContainer = mutationRecord.target.closest(blockContainersSelector);
			if (nearestBlockContainer == null)
				continue;

			/*	Track whether block layout should be done in this block 
				container, or only non-block rewrites.
			 */
			let blockContainerEntry = {
				blockContainer: nearestBlockContainer,
				doBlockLayout: true
			};

			//	Enforce layout isolation zones.
			let isolationZone = mutationRecord.target.parentElement?.closest(GW.layout.layoutIsolationSelector);
			if (isolationZone?.closest(blockContainersSelector) == nearestBlockContainer) {
				isolationZone.querySelectorAll(blockContainersSelector).forEach(isolatedBlockContainer => {
					//	Avoid adding a block container twice.
					if (affectedBlockContainers.findIndex(x => x.blockContainer == isolatedBlockContainer) == -1) {
						affectedBlockContainers.push({
							blockContainer: isolatedBlockContainer,
							doBlockLayout: true
						});
					}
				});
			} else {
				//	Enforce block layout exclusion zones.
				if (mutationRecord.target.closest(GW.layout.blockLayoutExclusionSelector) != null)
					blockContainerEntry.doBlockLayout = false;

				//	Avoid adding a block container twice.
				if (affectedBlockContainers.findIndex(x => x.blockContainer == blockContainerEntry.blockContainer) == -1)
					affectedBlockContainers.push(blockContainerEntry);
			}			
		}

		/*	Exclude block containers that are contained within other block
			containers in the list, to prevent redundant processing.
		 */
		affectedBlockContainers = affectedBlockContainers.filter(c => affectedBlockContainers.findIndex(x => {
			//	This means “x contains c”.
			return (c.blockContainer.compareDocumentPosition(x.blockContainer) & Node.DOCUMENT_POSITION_CONTAINS);
		}) == -1);

		/*	Add containers to list of containers needing layout processing, if
			they are not there already.
		 */
		affectedBlockContainers.forEach(blockContainerEntry => {
			if (GW.layout.blockContainersNeedingLayout.includes(blockContainerEntry) == false)
				GW.layout.blockContainersNeedingLayout.push(blockContainerEntry);
		});
		requestAnimationFrame(() => {
			GW.layout.currentPassBegin = performance.now();

			//	Do layout in all waiting block containers.
			while (GW.layout.blockContainersNeedingLayout.length > 0) {
				let nextBlockContainerEntry = GW.layout.blockContainersNeedingLayout.shift();
				GW.layout.layoutProcessors.forEach(processorSpec => {
					if (   processorSpec.options.blockLayout == false
						|| nextBlockContainerEntry.doBlockLayout == true)
						applyLayoutProcessorToBlockContainer(processorSpec, nextBlockContainerEntry.blockContainer, container);
				});
			}
		});
	});

	observer.observe(container, { subtree: true, childList: true });
}

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

/***************************************************************************/
/*	Retrieve desired result from element’s layout cache, or calculate it and
	store in element’s layout cache; and, in any case, return.
 */
function useLayoutCache(element, action, options, f) {
	if (element == null)
		return null;

	options = processLayoutOptions(options);

	let cacheKey = action;
	if (options.cacheKey)
		cacheKey += (" " + options.cacheKey);

	let layoutCache = element.layoutCache;
	let cacheInvalid = false;
	if (layoutCache == null) {
		layoutCache = element.layoutCache = new Map();
		cacheInvalid = true;
	}

	if (   cacheInvalid
		|| (layoutCache.get("time") ?? 0) < GW.layout.currentPassBegin) {
		layoutCache.clear();
		layoutCache.set("time", GW.layout.currentPassBegin);
		cacheInvalid = true;
	}

	if (   cacheInvalid
		|| layoutCache.has(cacheKey) == false) {
		layoutCache.set(cacheKey, f(element, options));
	}

	return layoutCache.get(cacheKey);
}

/***************************************************************************/
/*	Returns true if element is a wrapper of the given type, false otherwise.

	Types: upOut, downOut, upIn, downIn
 */
function isWrapper(element, wrapperType, options) {
	return useLayoutCache(element, "isWrapper_" + wrapperType, options, (element, options) => {
		return (   element?.matches(options.wrapperOptions[wrapperType].wrappersSelector) == true
				&& element?.matches(options.wrapperOptions[wrapperType].blockElementsSelector) != true
				&& element?.matches(options.wrapperOptions[wrapperType].blockContainersSelector) != true);
	});
}

/*****************************************************************/
/*	Returns true if element is a skipped element, false otherwise.
 */
function isSkipped(element, options) {
	return useLayoutCache(element, "isSkipped", options, (element, options) => {
		return (element?.matches(options.skipElementsSelector) == true);
	});
}

/**************************************************************/
/*	Returns true if element is a layout block, false otherwise.
 */
function isBlock(element, options) {
	return useLayoutCache(element, "isBlock", options, (element, options) => {
		return (element?.matches(options.blockElementsSelector) == true);
	});
}

/******************************************************************/
/*	Returns true if the element is empty (accounting for metadata).
 */
function isEmpty(element, options) {
	return useLayoutCache(element, "isEmpty", options, (element, options) => {
		return isNodeEmpty_metadataAware(element);
	});
}

/***************************************************************************/
/*	Returns true if element is an always-not-empty element, false otherwise.
 */
function isNonEmpty(element, options) {
	return useLayoutCache(element, "isNonEmpty", options, (element, options) => {
		return (element?.matches(options.nonEmptyElementsSelector) == true);
	});
}

/******************************************************************/
/*	Returns nearest enclosing block container of the given element.
	(Might be null.)
 */
function blockContainerOf(element, options) {
	return useLayoutCache(element, "blockContainer", options, (element, options) => {
		return element.parentElement?.closest(options.blockContainersSelector);
	});
}

/**************************************************************************/
/*	Returns layout block sequential (next or previous) to the given element
	in the block flow.
 */
function sequentialBlockOf(element, direction, options) {
	let _sequentialBlockOf = (element, direction, options) => {
		let elementSibling = direction == "next"
							 ? element.nextElementSibling
							 : element.previousElementSibling;
		let wrapperDirection = (direction == "next" ? "down" : "up");
		let wrapperInType = wrapperDirection + "In";
		let wrapperOutType = wrapperDirection + "Out";
		let terminus = (direction == "next" ? "first" : "last");

		//	Skip elements that don’t participate in block flow.
		if (isSkipped(elementSibling, options))
			return sequentialBlockOf(elementSibling, direction, options);

		//	Look inside “transparent” wrappers (that don’t affect layout).
		if (isWrapper(elementSibling, wrapperInType, options)) {
			let terminalBlock = terminalBlockOf(elementSibling, terminus, options);
			if (terminalBlock)
				return terminalBlock;
		}

		//	Skip empty elements.
		if (   isEmpty(elementSibling, options) == true
			&& isNonEmpty(elementSibling, options) == false)
			return sequentialBlockOf(elementSibling, direction, options);

		//	An actual block element (the base case).
		if (isBlock(elementSibling, options))
			return elementSibling;

		/*	If we’re asked for the sequential block of the terminal child of a
			transparent wrapper, we return the sequential block of that wrapper
			(recursively, of course).
		 */
		if (isWrapper(element.parentElement, wrapperOutType, options))
			return sequentialBlockOf(element.parentElement, direction, options);

		return null;
	};

	return useLayoutCache(element, "sequentialBlock_" + direction, options, (element, options) => {
		return _sequentialBlockOf(element, direction, options);
	});
}

/************************************************************************/
/*	Returns layout block previous to the given element in the block flow.
	(Might be null.)
 */
function previousBlockOf(element, options) {
	return useLayoutCache(element, "previousBlock", options, (element, options) => {
		return sequentialBlockOf(element, "previous", options);
	});
}

/************************************************************************/
/*	Returns layout block next from the given element in the block flow.
	(Might be null.)
 */
function nextBlockOf(element, options) {
	return useLayoutCache(element, "nextBlock", options, (element, options) => {
		return sequentialBlockOf(element, "next", options);
	});
}

/***************************************************************/
/*	Returns terminal (first or last) layout block of an element.
	(Might be the element itself, or null.)
 */
function terminalBlockOf(element, terminus, options, strictDescent = false) {
	let _terminalBlockOf = (element, terminus, options, strictDescent) => {
		//	Look inside wrappers (or any block, if strictDescent is specified).
		let wrapperType = (terminus == "first" ? "down" : "up") + "In";
		if (   strictDescent == true
			|| isWrapper(element, wrapperType, options)) {
			let childBlocks = childBlocksOf(element, options);
			for (let i  = (terminus == "first" ? 0                  : childBlocks.length - 1);
					 i != (terminus == "first" ? childBlocks.length : -1);
					 i += (terminus == "first" ? 1                  : -1)) {
				let terminalBlock = terminalBlockOf(childBlocks[i], terminus, options);
				if (   terminalBlock
					&& isSkipped(terminalBlock, options) == false
					&& (   isEmpty(terminalBlock, options) == false
						|| isNonEmpty(terminalBlock, options) == true))
					return terminalBlock;
			}
		}

		//	The element itself is a layout block (only if no strictDescent).
		if (   strictDescent == false
			&& isBlock(element, options))
			return element;

		return null;
	};

	if (strictDescent) {
		return _terminalBlockOf(element, terminus, options, true);
	} else {
		return useLayoutCache(element, "terminalBlock_" + terminus, options, (element, options) => {
			return _terminalBlockOf(element, terminus, options, false);
		});
	}
}

/*******************************************/
/*	Returns last layout block of an element.
	(Might be the element itself, or null.)
 */
function lastBlockOf(element, options, strictDescent) {
	if (strictDescent) {
		return terminalBlockOf(element, "last", options, strictDescent);
	} else {
		return useLayoutCache(element, "lastBlock", options, (element, options) => {
			return terminalBlockOf(element, "last", options, strictDescent);
		});
	}
}

/********************************************/
/*	Returns first layout block of an element.
	(Might be the element itself, or null.)
 */
function firstBlockOf(element, options, strictDescent) {
	if (strictDescent) {
		return terminalBlockOf(element, "first", options, strictDescent);
	} else {
		return useLayoutCache(element, "firstBlock", options, (element, options) => {
			return terminalBlockOf(element, "first", options, strictDescent);
		});
	}
}

/***************************************************************************/
/*	Returns all “child” blocks of an element (blocks that are descended from
	the given element with no other blocks in the chain of descent; wrappers
	don’t count).
 */
function childBlocksOf(element, options) {
	return useLayoutCache(element, "childBlocks", options, (element, options) => {
		let childBlocks = [ ];
		for (let block of element.children) {
			if (isWrapper(block, "downIn", options)) {
				childBlocks.push(...(childBlocksOf(block)));
			} else if (isBlock(block, options)) {
				childBlocks.push(block);
			}
		}

		return childBlocks;
	});
}

/****************************************************************************/
/*	Returns true if the element is a “bare wrapper”, i.e. a <div> or <span> 
	with no classes (or, in the <div> case, possibly just the ‘block’ class); 
	false otherwise.
 */
function isBareWrapper(element) {
	return (   (   element.tagName == "DIV"
				&& (   element.className.trim() == ""
					|| element.className.trim() == "block"))
			|| (   element.tagName == "SPAN"
				&& element.className.trim() == ""));
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
	let predicateMatches = (selectorOrSelectorArray, block) => {
		let predicate;
		if (   typeof selectorOrSelectorArray == "string"
			|| selectorOrSelectorArray.isGWSelector == true) {
			predicate = (block) => (blockMatchesSelector(block, selectorOrSelectorArray));
		} else {
			predicate = (block) => (selectorOrSelectorArray.findIndex(x => predicateMatches(x, block)) != -1);
		}

		return (predicate(block) == true);
	};

	if (debug)
		console.log(block);
	for (let [ selector, result, adjustable = true ] of GW.layout.blockSpacing) {
		if (predicateMatches(selector, block)) {
			if (debug)
				console.log(selector);
			let bsm = (typeof result == "function")
					  ? result(block)
					  : result;

			if (adjustable) {
				for (let [ selectorOrSelectorArray, transform ] of GW.layout.blockSpacingAdjustments)
					if (predicateMatches(selectorOrSelectorArray, block)) {
						if (debug)
							console.log(selectorOrSelectorArray);
						bsm = Math.max(0, transform(bsm, block));
					}
			}

			return bsm;
		}
	}

	return undefined;
}

/*****************************************************************************/
/*	Returns a block’s dropcap type (‘goudy’, ‘yinit’, etc.), or null if none.
 */
function dropcapTypeOf(block) {
	if (block == null)
		return null;

	return Array.from(block.classList).find(cssClass => /^dropcaps?-/.test(cssClass))?.replace(/^dropcaps-/, "dropcap-")?.slice("dropcap-".length);
}

/******************************************************************************/
/*	Adds a dropcap class to a block. Dropcaps may be ‘kanzlei’, ‘de-zs’, etc.
	(See default.css for the list.)
 */
function addDropcapClassTo(block, dropcapType) {
	if (block.classList.contains("force-dropcap"))
		return;

	stripDropcapClassesFrom(block);

	block.classList.add(`dropcap-${dropcapType}`);
}

/*************************************/
/*	Strip dropcap classes from block.
 */
function stripDropcapClassesFrom(block) {
	if (block.classList.contains("force-dropcap"))
		return;

	block.classList.remove(...(Array.from(block.classList).filter(className => className.startsWith("dropcap-"))));
}

/**************************************************************************/
/*	Like paragraphizeTextNodesOfElement, but retains elements with metadata
	(an ID, non-layout classes, or any data attributes), as well as links,
	<br> elements, and lists.
 */
function paragraphizeTextNodesOfElementRetainingMetadata(element) {
	paragraphizeTextNodesOfElement(element, {
		excludeSelector: [
			".graf-content-not"		
			].join(", "),
		nodeOmissionOptions: {
			alsoExcludePredicate: GW.layout.emptyNodeExclusionPredicate,
			alsoExcludeSelector: "a, br, ul, ol", 
			excludeIdentifiedElements: true
		}
	});
}

/****************************************************************************/
/*	Given a set of nodes all of which are children of the same parent node, 
	and conditional on the node before the first node of the set being a text
	node that ends in a left parenthesis and the node after the last node of 
	the set being a text node that begins with a right parenthesis, wraps the
	nodes in the set, plus the preceding and following nodes, in a <span>
	with the class `parenthesized-set` plus any other classes given by an 
	optional class string.

	If the conditions are not met, does nothing.
 */
function wrapParenthesizedNodes(className = null, ...args) {
	let parentNode = args.first.parentNode;
	if (parentNode == null)
		return;

	for (let node of args)
		if (node.parentNode != parentNode)
			return;

	if (   args.first.previousSibling?.nodeType == Node.TEXT_NODE
		&& args.first.previousSibling.nodeValue.endsWith("(")
		&& args.last.nextSibling?.nodeType == Node.TEXT_NODE
		&& args.last.nextSibling.nodeValue.startsWith(")")) {
		args.first.previousSibling.nodeValue = args.first.previousSibling.nodeValue.slice(0, -1);
		args.last.nextSibling.nodeValue = args.last.nextSibling.nodeValue.slice(1);
		let nextNode = args.last.nextSibling;
		let wrapper = newElement("SPAN", { class: `parenthesized-set${(className ? (" " + className) : "")}` });
		wrapper.append(document.createTextNode("("), ...args, document.createTextNode(")"));
		parentNode.insertBefore(wrapper, nextNode);
	}
}

/*****************************************************************************/
/*	Like isNodeEmpty, but does not count elements with metadata as being empty
	(i.e., if they have an ID, or non-layout classes, or any data attributes).
 */
function isNodeEmpty_metadataAware(node) {
	return isNodeEmpty(node, {
		alsoExcludePredicate: GW.layout.emptyNodeExclusionPredicate,
		excludeIdentifiedElements: true
	});
}

/******************************************************************************/
/*	Returns the text content of a node, collapsing/stripping (as appropriate)
	whitespace that occurs between HTML tags, with special handling for rows 
	and sections of a table.
 */
function textContentOf(node) {
	if (isNodeEmpty(node) == true)
		return (node.textContent.includes("\n") 
				? "\n"
				: (node.textContent > ""
				   ? " "
				   : ""));

	if (node.nodeType == Node.TEXT_NODE)
		return node.textContent;

	if (   node.nodeType == Node.ELEMENT_NODE
		&& [ "THEAD", "TBODY", "TFOOT" ].includes(node.tagName))
		return Array.from(node.children
				).filter(childNode => childNode.nodeType == Node.ELEMENT_NODE
				).map(childNode => textContentOf(childNode)
				).join("\n");

	if (   node.nodeType == Node.ELEMENT_NODE
		&& node.tagName  == "TR")
		return Array.from(node.children
				).filter(childNode => childNode.nodeType == Node.ELEMENT_NODE
				).map(childNode => textContentOf(childNode)
				).join("\t");

	return Array.from(node.childNodes).reduce((textContent, childNode) => (textContent + textContentOf(childNode)), "");
}


/*********************/
/* LAYOUT PROCESSORS */
/*********************/

/*******************************************************************************/
/*	Run given callback on given container immediately and also at any later
	time when block layout classes are updated in that container (e.g., <main>).
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

/*******************************************************************************/
/*	Apply block layout classes to appropriate elements in given block container.
 */
addLayoutProcessor("applyBlockLayoutClassesInContainer", (blockContainer) => {
    GWLog("applyBlockLayoutClassesInContainer", "layout.js", 2);

	let containingDocument = blockContainer.getRootNode();

	//	Exclusion predicate.
	let blockContainersSelector = GW.layout.blockContainers.join(", ");
	let exclude = (block) => {
		if (block.parentElement?.closest(GW.layout.blockLayoutExclusionSelector) != null)
			return true;

		return false;
	};

	//	Designate headings.
	blockContainer.querySelectorAll(range(1, 6).map(x => `h${x}`).join(", ")).forEach(heading => {
		if (exclude(heading))
			return;

		heading.classList.add("heading");
	});

	//	Designate floats (on non-mobile layouts).
	let floatClasses = [
		".float-left",
		".float-right"
	];
	if (GW.mediaQueries.mobileWidth.matches == false) {
		blockContainer.querySelectorAll(floatClasses.join(", ")).forEach(floatBlock => {
			if (exclude(floatBlock))
				return;

			floatBlock.classList.add("float");
		});
	} else {
		blockContainer.querySelectorAll(floatClasses.join(", ")).forEach(floatBlock => {
			if (exclude(floatBlock))
				return;

			floatBlock.classList.remove("float");
		});
	}

	//	Designate lists.
	blockContainer.querySelectorAll([
		"ul",
		"ol"
	].join(", ")).forEach(list => {
		list.classList.add("list");
	});

	//	Designate float-containing lists.
	blockContainer.querySelectorAll(".markdownBody li .float").forEach(floatBlock => {
		if (exclude(floatBlock))
			return;

		let options = {
			alsoBlockContainers: [ ".list" ],
			cacheKey: "alsoBlockContainers_lists"
		};
		let ancestorBlockContainer = blockContainerOf(floatBlock, options);
		while (ancestorBlockContainer?.matches(".list")) {
			ancestorBlockContainer.classList.add("has-floats");
			ancestorBlockContainer = blockContainerOf(ancestorBlockContainer, options);
		}
	});

	//	Designate “big lists”.
	/*	If any of a list’s list items have multiple non-list children, then
		it is a “big list” (with consequences for block spacing).
	 */
	let listItemChildBlocksOptions = {
		notWrapperElements: [ ".list" ],
		cacheKey: "notWrappers_lists"
	};
	let isBigList = (list) => {
		if (list.matches(".list") != true)
			return false;

		for (let listItem of list.children) {
			if (childBlocksOf(listItem, listItemChildBlocksOptions).filter(x => x.matches(".list") != true).length > 1)
				return true;
		}

		return false;
	};
	blockContainer.querySelectorAll(".list").forEach(list => {
		if (exclude(list))
			return;

		let bigList = isBigList(list);

		/*	If this is a sub-list, and any other sub-lists on the same level as
			this one are “big lists”, then this is also a “big list” (because
			the designation of “bigness” is applied to *list levels within a
			list tree*, not to individual lists).
		 */
		 let ancestorBlockContainer = blockContainerOf(list, {
		 	alsoBlockContainers: [ "li" ],
		 	cacheKey: "alsoBlockContainers_listItems"
		 });
		 if (ancestorBlockContainer?.matches("li")) {
		 	for (let listItem of ancestorBlockContainer.parentElement.children) {
				if (childBlocksOf(listItem, listItemChildBlocksOptions).findIndex(x => isBigList(x)) != -1) {
					bigList = true;
					break;
				}
		 	}
		 }

		list.classList.toggle("big-list", bigList);
	});

	//	Disable triptychs on mobile layouts.
	blockContainer.querySelectorAll(".triptych").forEach(triptych => {
		if (exclude(triptych))
			return;

		/*	Why “aptych”? Because on mobile it is laid out in one column
			instead of three, making it “un-folded”:
			https://old.reddit.com/r/AncientGreek/comments/ypts2o/polyptychs_help_with_a_word/
		 */
		triptych.classList.toggle("aptych", GW.mediaQueries.mobileWidth.matches);
	});

	//	Apply special block sequence classes.
	let applyBlockSequenceClassesToBlock = (block) => {
		if (exclude(block))
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

		/*	Designate blocks followed by nothing (not counting floats and other
			elements that do not participate in block flow) in their block
			container (the .last-block class).
		 */
		block.classList.toggle("last-block", nextBlockOf(block) == null);

		//	Designate blocks in lists (the .in-list class).
		block.classList.toggle("in-list", blockContainerOf(block, {
			alsoBlockContainers: [ "li" ],
			cacheKey: "alsoBlockContainers_listItems"
		})?.matches("li") == true);

		//	Apply special paragraph classes.
		if (block.matches("p") == true) {
			//	Empty paragraphs (the .empty-graf class; not displayed).
			let emptyGraf = isEmpty(block);
			block.classList.toggle("empty-graf", emptyGraf);
			if (emptyGraf)
				return;

			/*	Paragraphs not preceded directly by other paragraphs
				(not in lists) (the .first-graf class).
			 */
			let firstGraf = false;
			let strictPreviousBlock = previousBlockOf(block, {
				alsoBlockElements: [ ".list" ],
				notWrapperElements: [ "li", ".list" ],
				notHalfWrapperElements: [ "section" ],
				cacheKey: "alsoBlocks_lists_notWrappers_listsAndListItems_notHalfWrappers_sections"
			});
			if (   strictPreviousBlock == null
				|| strictPreviousBlock.matches(GW.layout.firstGrafAllowablePreviousBlockSelector) == true)
				firstGraf = true;
			//	Centered text must be vertically spaced in all cases.
			if (block.matches(".text-center"))
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
			if (   strictNextBlock?.matches(".list, .columns")
				&& block.textContent.trim().endsWith(":")
				&& block.textContent.includes(".") == false)
				listHeading = true;
			block.classList.toggle("list-heading", listHeading);

			/*	Introductory paragraphs of documents or self-contained parts
				of documents (the .intro-graf class).
			 */
			let introGraf = false;
			if (   block.matches(".text-center, .margin-notes-block, .data-field") != true
				&& block.matches(".in-list") != true
				&& block.parentElement?.closest("#footer, figcaption, table") == null
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

				/*  Add dropcap class. This could be set globally, or
					overridden by a .abstract; the latter could be
					`dropcap-not` (which nullifies any page-global dropcap
					class for the given block).
				 */
				let dropcapType = null;
				if (introGraf) {
					dropcapType = (previousBlock?.matches(".abstract blockquote")
								   ? dropcapTypeOf(previousBlock)
								   : null) ?? dropcapTypeOf(containingDocument.body);
				} else {
					let dropcapContainerOptions = {
						alsoBlockContainers: [ "div[class*='dropcap-']" ],
						cacheKey: "alsoBlockContainers_dropcapDivs"
					};
					let dropcapContainer = blockContainerOf(block, dropcapContainerOptions);
					if (   dropcapContainer?.matches("div")
						&& previousBlockOf(block, dropcapContainerOptions) == null) {
						dropcapType = dropcapTypeOf(dropcapContainer);
						block.classList.add("first-graf");
					}
				}
				if (   dropcapType 
					&& dropcapType != "not") {
					addDropcapClassTo(block, dropcapType);
				} else {
					stripDropcapClassesFrom(block);

					/*	If resetDropcapInBlock() has not been defined, then it 
						is also guaranteed to be unnecessary, as that means that
						the functions that *add* dropcaps to blocks have also
						not been defined and thus cannot have been called, so
						nothing needs to be reset.
					 */
					if (window.resetDropcapInBlock)
						resetDropcapInBlock(block);
				}
			}
			block.classList.toggle("intro-graf", introGraf);

			let footnoteBackLink = block.querySelector(".footnote-back");
			if (isOnlyChild(footnoteBackLink))
				block.classList.add("footnote-back-block");
		}

		if (block.matches(GW.layout.complexBlockElementSelector))
			childBlocksOf(block).forEach(applyBlockSequenceClassesToBlock);

		//	Tables need special handling.
		if (block.matches(".table-wrapper")) {
			block.querySelectorAll("th, td").forEach(cell => {
				childBlocksOf(cell).forEach(applyBlockSequenceClassesToBlock);
			});
		}
	};
	childBlocksOf(blockContainer).forEach(applyBlockSequenceClassesToBlock);
});

/**********************************************/
/*	Apply block spacing in the given container.
 */
addLayoutProcessor("applyBlockSpacingInContainer", (blockContainer) => {
    GWLog("applyBlockSpacingInContainer", "layout.js", 2);

	//	Exclusion predicate.
	let exclude = (block) => {
		if (block.parentElement?.closest(GW.layout.blockLayoutExclusionSelector) != null)
			return true;

		return false;
	};

	//	Eliminate false positives (due to transjection, e.g.).
	blockContainer.querySelectorAll(".block").forEach(allegedBlock => {
		if (   isBlock(allegedBlock) == false
			|| exclude(allegedBlock) == true) {
			allegedBlock.classList.remove("block");
			allegedBlock.style.removeProperty("--bsm");
		}
	});

	//	Set block spacing multipliers.
	let applyBSMToBlock = (block) => {
		let bsm = getBlockSpacingMultiplier(block);
		if (bsm == undefined) {
			//	Remove block spacing metadata from what shouldn’t have it.
			block.classList.remove("block");
			block.style.removeProperty("--bsm");
		} else {
			//	Apply block spacing.
			block.classList.add("block");
			block.style.setProperty("--bsm", `${bsm}`);
		}

		if (block.matches(GW.layout.complexBlockElementSelector))
			childBlocksOf(block).forEach(applyBSMToBlock);

		//	Tables need special handling.
		if (block.matches(".table-wrapper")) {
			block.querySelectorAll("th, td").forEach(cell => {
				childBlocksOf(cell).forEach(applyBSMToBlock);
			});
		}
	};
	childBlocksOf(blockContainer).forEach(applyBSMToBlock);

	//	Triptychs require special treatment.
	blockContainer.querySelectorAll(".triptych").forEach(triptych => {
		if (exclude(triptych))
			return;

		if (triptych.classList.contains("aptych")) {
			triptych.style.removeProperty("--bsm");
			triptych.classList.remove("block");
		} else {
			//	Set triptych BSM to that of first panel.
			let triptychBSM = parseInt(triptych.firstElementChild.style.getPropertyValue("--bsm"));;

			//	Set all panels’ BSM to 0.
			for (let panel of triptych.children)
				panel.style.setProperty("--bsm", 0);

			triptych.style.setProperty("--bsm", triptychBSM);
			triptych.classList.add("block");
		}
	});

	//	Lists require special treatment.
	blockContainer.querySelectorAll("li:not(.footnote)").forEach(listItem => {
		if (exclude(listItem))
			return;

		let firstBlockWithin = firstBlockOf(listItem);
		let bsm = firstBlockWithin?.style.getPropertyValue("--bsm");

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
	});

	//	Floats require special treatment on non-mobile layouts.
	blockContainer.querySelectorAll(".float").forEach(floatBlock => {
		if (exclude(floatBlock))
			return;

		//	Floating elements take on the top margin of the next block.
		let nextBlock = nextBlockOf(floatBlock, {
			alsoBlockElements: [ "li" ],
			cacheKey: "alsoBlocks_listItems"
		});
		let nextBlockBSM = nextBlock?.style?.getPropertyValue("--bsm");
		if (nextBlockBSM) {
			/*	If the next block (in the strict sense, i.e. not counting list
				items!) is a paragraph or non-collapse section heading, then 
				adjust margin.
			 */
			let strictNextBlock = nextBlockOf(floatBlock, {
				alsoBlockElements: [ "section:not(.collapse) > .heading" ],
				cacheKey: "alsoBlocks_nonCollapseSectionHeadings"
			});
			if (strictNextBlock?.matches("p, .heading"))
				nextBlockBSM = "" + (parseInt(nextBlockBSM) + 2);

			floatBlock.style.setProperty("--bsm", nextBlockBSM);
		}
	});
});

/****************************************************************************/
/*	Apply block layout classes to the main document, prior to rewrites. (This
	is necessary in browsers that delay MutationObserver firing until after
	DOMContentLoaded.)
 */
addContentLoadHandler(GW.contentLoadHandlers.applyBlockLayoutClassesInMainDocument = (eventInfo) => {
    GWLog("applyBlockLayoutClassesInMainDocument", "layout.js", 1);

	eventInfo.container.querySelectorAll(".markdownBody").forEach(blockContainer => {
		GW.layout.applyBlockLayoutClassesInContainer(blockContainer);
	});
}, "<rewrite", (info) => (info.container == document.main));

/**************************************************************************/
/*	Fire “page layout complete event” on the next animation frame after the 
	end of all rewrites triggered directly by the DOMContentLoaded event.
 */
addContentInjectHandler(GW.contentInjectHandlers.completePageLayout = (eventInfo) => {
    GWLog("completePageLayout", "layout.js", 1);

	requestAnimationFrame(() => {
		GW.layout.initialPageLayoutComplete = true;
		GW.notificationCenter.fireEvent("Layout.initialPageLayoutDidComplete");
	});
}, "<eventListeners", (info) => (info.container == document.main));

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

/*************************************************/
/*	Activate dynamic layout for the main document.
 */
doWhenMainExists(() => {
	startDynamicLayoutInContainer(document.main);

	//	Add listener to redo layout when orientation changes.
	doWhenMatchMedia(GW.mediaQueries.portraitOrientation, {
		name: "Layout.updateLayoutWhenOrientationChanges",
		ifMatchesOrAlwaysDo: (mediaQuery) => {
			document.main.querySelectorAll(".markdownBody").forEach(blockContainer => {
				GW.layout.layoutProcessors.forEach(processorSpec => {
					applyLayoutProcessorToBlockContainer(processorSpec, blockContainer, document.main);
				});
			});
		},
		callWhenAdd: false
	});
});

/******************************************************************************/
/*  Run the given function immediately if initial page layout has completed, or
	add an event handler to run it as soon as initial page layout completes.
 */
function doWhenPageLayoutComplete(f) {
    if (GW.layout.initialPageLayoutComplete == true)
        f();
    else
        GW.notificationCenter.addHandlerForEvent("Layout.initialPageLayoutDidComplete", (info) => {
            f();
        }, { once: true });
}
