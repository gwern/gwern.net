/* Miscellaneous JS functions which run after the page loads to rewrite or adjust parts of the page. */
/* author: Said Achmiz */
/* license: MIT */

/******************************/
/*	Events fired by rewrite.js:

	Rewrite.fullWidthMediaDidLoad
		Fired when a full-width image or video is loaded. This event is only
		fired after the initial page load completes (i.e., it is triggered by
		lazy-loading of media elements).

	Rewrite.pageLayoutWillComplete
		Fired just before the page layout process completes. The value of the
		`GW.pageLayoutComplete` flag is false at this point.

	Rewrite.pageLayoutDidComplete
		Fired just after the page layout process completes. The value of the
		`GW.pageLayoutComplete` flag is true at this point.
 */

/***************************************/
/*	NOTE on the GW.contentDidLoad event:

	This event is fired whenever any new local page content is loaded (whether
	this means “loaded via a network request”, “loaded from cache”,
	“constructed from existing page elements”, or any other process by which
	a new unit of page content is created). This includes the initial page
	load, but also such things as pop-frames being spawned, annotations being
	lazy-loaded, etc.

	Many event handlers are attached to this event, because a great deal of
	processing must take place before newly-loaded page content is ready for
	presentation to the user. Typography rectification must take place; the HTML
	structure of certain page elements (such as tables, figures, etc.) must be
	reconfigured; CSS classes must be added; various event listeners attached;
	etc. Most of this file (rewrite.js) consists of exactly such “content load
	handlers”, a.k.a. “rewrite functions”. (Additional content load handlers are
	defined elsewhere in the code, as appropriate; e.g. the handler that
	attaches event listeners to annotated links to load annotations when the
	user mouses over such links, which is found in extracts-annotations.js.)

	The GW.contentDidLoad event has the following named handler phases (see
	gw-inline.js for details on what this means):

		[ "rewrite", "eventListeners" ]

	The GW.contentDidLoad event should have the following keys and values in its
	event info dictionary (see gw-inline.js for details on event info
	dictionaries):

		‘source’ (key)
			String that indicates function (or event name, if fired from a
			browser event listener) from which the event is fired (such as
			‘Annotation.loadAnnotation’).

		‘document’ (key)
			DOM object containing the loaded content. (For the GW.contentDidLoad
			event fired on the initial page load, the value of this key is
			`document.firstElementChild`, i.e. the <html> element of the page.
			For pop-frames, this is the documentElement of the pop-frame.)

		‘location’ (key)
			URL object (https://developer.mozilla.org/en-US/docs/Web/API/URL)
			which specifies the URL from which the loaded content was loaded.
			For the main page, the represented URL will be `location.href`. For
			pop-frames and the like, the represented URL will be the `href`
			attribute of the spawning target. For local annotations, this will
			be the URL of the annotation resource on the local server (NOT the
			URL of the annotated link!). For Wikipedia annotations, this will be
			the URL of the API request to retrieve the annotation data.

		‘flags’ (key)
			Bit field containing various flags (combined via bitwise OR). The
			values of the flags are defined in GW.contentDidLoadEventFlags.

			(Note that event handlers for the ‘GW.contentDidLoad’ event can
			 access the values of these flags directly via property access on
			 the event, e.g. the following two expressions are equivalent:

			   eventInfo.flags & GW.contentDidLoadEventFlags.needsRewrite != 0

			   eventInfo.needsRewrite

			 It is recommended that the latter form be used.)

			The flags are:

			‘isMainDocument’
				Specifies whether the loaded content is the main page itself, or
				some content loaded within the main page (which might be a
				content fragment like an annotation, or an entire other local
				page loaded in a pop-frame, or something else).

				The value of this key should be true only once per page session,
				when the initial page content is loaded (and the
				GW.contentDidLoad event is called from a listener for the
				DOMContentLoaded browser event).

			‘needsRewrite’
				Specifies whether the loaded content needs to be processed
				through a rewrite pass (i.e., to have its typography rectified,
				HTML structure adjusted, etc.). If this value is false, then the
				loaded content has already had rewriting performed (this will be
				the case when the content is being loaded from a local cache),
				and only needs to have event listeners attached, positioning
				adjusted, and other such treatment that doesn’t modify the
				content.

			‘clickable’
				Currently unused. Reserved for future use.

			‘collapseAllowed’
				Specifies whether the loaded content is permitted to have
				collapsed sections. Generally false for all but the main page
				(because collapsing/un-collapsing interactions are awkward and
				confusing in pop-frames and similar embedded content elements).
				If the value of this key is false, then any collapse blocks in
				the loaded content will be automatically expanded (if present)
				or simply not enabled in the first place, and all content in
				collapsible sections will be visible at all times.

			‘isCollapseBlock’
				Currently unused. Reserved for future use.

			‘isFullPage’
				Specifies whether the loaded content is a whole local page. True
				for the main page load, and also for loads of whole other local
				pages (for embedding into a pop-frame); false in all other cases
				(fragments of a page, e.g. footnotes; annotations; etc.)

				If true, the loaded content will contain a main content element
				(`#markdownBody`) as well as a page metadata block
				(`#page-metadata`).

			‘fullWidthPossible’
				Specifies whether full-width elements are permitted in the
				loaded content. Generally true only for the main page load. If
				false, elements marked as full-width will be laid out as if for
				a mobile (narrow) viewport, regardless of the actual dimensions
				of the loaded content’s container (i.e. they will not actually
				be “full-width”).
 */

GW.rewriteFunctions = { };


/***********/
/* HELPERS */
/***********/

/******************************************************************************/
/*	Create and return a new element with the specified tag name and attributes.
 */
function newElement(tagName, attributes = { }) {
	let element = document.createElement(tagName);
	for (const attrName in attributes)
		if (attributes.hasOwnProperty(attrName))
			element.setAttribute(attrName, attributes[attrName]);
	return element;
}

/*****************************************************/
/*	Wrap an element in a wrapper element.
 */
function wrapElement(element, wrapClass, wrapTagName = "DIV", useExistingWrapper = false, moveClasses = false) {
	if (   useExistingWrapper
		&& element.parentElement
		&& element.parentElement.tagName == wrapTagName
		&& element.parentElement.children.length == 1) {
		if (wrapClass > "")
			element.parentElement.classList.add(...(wrapClass.split(" ")));
	} else {
		let wrapper = document.createElement(wrapTagName);
		if (wrapClass > "")
			wrapper.classList.add(...(wrapClass.split(" ")));
		element.parentElement.insertBefore(wrapper, element);
		wrapper.appendChild(element);
	}

	if (moveClasses === false)
		return;

	if (moveClasses === true) {
		element.parentElement.classList.add(...(element.classList));
		element.removeAttribute("class");
		return;
	}

	if (!(moveClasses instanceof Array))
		return;

	moveClasses.forEach(cssClass => {
		if (element.classList.contains(cssClass)) {
			element.classList.remove(cssClass);
			element.parentElement.classList.add(cssClass);
		}
	});
}

/*****************************************************/
/*	Wrap all elements specified by the given selector.
 */
function wrapAll(selector, wrapClassOrFunction, wrapTagName = "DIV", root = document, useExistingWrappers = false, moveClasses = false) {
	let wrapperFunction;
	if (typeof wrapClassOrFunction == "string") {
		wrapperFunction = (element) => {
			wrapElement(element, wrapClassOrFunction, wrapTagName, useExistingWrappers, moveClasses);
		};
	} else {
		wrapperFunction = wrapClassOrFunction;
	}

	root.querySelectorAll(selector).forEach(wrapperFunction);
}

/****************************************/
/*	Replace an element with its contents.
 */
function unwrap(wrapper) {
	if (wrapper.parentElement == null)
		return;

	while (wrapper.childNodes.length > 0)
		wrapper.parentElement.insertBefore(wrapper.firstChild, wrapper);
	wrapper.remove();
}

/*******************************************************/
/*	Unwrap all elements specified by the given selector.
 */
function unwrapAll(selector, root = document) {
	root.querySelectorAll(selector).forEach(element => {
		unwrap(element);
	});
}


/**********/
/* TABLES */
/**********/

/****************************************************************/
/*  Wrap each table in a div.table-wrapper (for layout purposes).
 */
function wrapTables(loadEventInfo) {
    GWLog("wrapTables", "rewrite.js", 1);

	wrapAll("table", "table-wrapper", "DIV", loadEventInfo.document, true)
}

/*****************************************************************************/
/*  Wrap each full-width table in a div.full-width-table-inner-wrapper, and
	also move the .collapse class (if any) from the outer wrapper to the table
	(for consistency).
 */
function wrapFullWidthTables(loadEventInfo) {
    GWLog("wrapFullWidthTables", "rewrite.js", 1);

	wrapAll(".table-wrapper.width-full > table", "full-width-table-inner-wrapper", "DIV", loadEventInfo.document, false);

	//	Move ‘collapse’ class from wrappers to tables.
	loadEventInfo.document.querySelectorAll(".table-wrapper.width-full.collapse table").forEach(table => {
		table.closest(".table-wrapper").classList.remove("collapse");
		table.classList.add("collapse");
	});
}

/**********************************************/
/*  Add content load handler to process tables.
 */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processTables = (info) => {
    GWLog("GW.rewriteFunctions.processTables", "rewrite.js", 2);

    wrapTables(info);
    if (info.fullWidthPossible)
        wrapFullWidthTables(info);
}, {
	phase: "rewrite",
	condition: (info) => info.needsRewrite
});


/***********/
/* FIGURES */
/***********/

/*******************************/
/*  Wrap bare images in figures.
 */
function wrapImages(loadEventInfo) {
    GWLog("wrapImages", "rewrite.js", 1);

	loadEventInfo.document.querySelectorAll("p > img:only-child").forEach(image => {
		unwrap(image.parentElement);
	});

	let exclusionSelector = ".footnote-back, .mwe-math-element";
	wrapAll("img", (image) => {
		if (image.closest(exclusionSelector))
			return;

		let figure = image.closest("figure");
		if (   figure
			&& figure.querySelector("figcaption") != null)
			return;

		wrapElement(image, null, "FIGURE", true,
			[ "float-left", "float-right", "outline-not", "image-focus-not" ]);
	}, null, loadEventInfo.document);
}

/********************************/
/*  Inject wrappers into figures.
 */
function wrapFigures(loadEventInfo) {
    GWLog("wrapFigures", "rewrite.js", 1);

    loadEventInfo.document.querySelectorAll("figure").forEach(figure => {
        let media = figure.querySelector("img, audio, video");
        let caption = figure.querySelector("figcaption");

        if (!(media && caption))
            return;

        //  Create an inner wrapper for the figure contents.
        let innerWrapper = document.createElement("SPAN");
        innerWrapper.classList.add("figure-inner-wrapper");
        figure.appendChild(innerWrapper);

        //  Wrap the caption in the wrapper span.
        let wrapper = document.createElement("SPAN");
        wrapper.classList.add("caption-wrapper");
        wrapper.appendChild(caption);

        //  Get the media, or (if any) the image wrapper.
        let mediaBlock = media.closest(".image-wrapper") || media;

        //  Re-insert the (possibly wrapped) media and the wrapped caption into
        //  the figure.
        innerWrapper.appendChild(mediaBlock);
        innerWrapper.appendChild(wrapper);

        // Tag the figure with the image’s float class.
        if (media.classList.contains("float-left"))
            media.closest("figure").classList.add("float-left");
        if (media.classList.contains("float-right"))
            media.closest("figure").classList.add("float-right");
    });
}

/********************************************************************/
/*  Designate full-width figures as such (with a ‘width-full’ class).
 */
function markFullWidthFigures(loadEventInfo) {
    GWLog("markFullWidthFigures", "rewrite.js", 1);

    let fullWidthClass = "width-full";

    let allFullWidthMedia = loadEventInfo.document.querySelectorAll(`img.${fullWidthClass}, video.${fullWidthClass}`);
    allFullWidthMedia.forEach(fullWidthMedia => {
        fullWidthMedia.closest("figure").classList.toggle(fullWidthClass, true);
    });

    /*  Add ‘load’ listener for lazy-loaded media (as it might cause re-layout
        of eg. sidenotes). Do this only after page loads, to avoid spurious
        re-layout at initial page load.
     */
    doWhenPageLoaded(() => {
        allFullWidthMedia.forEach(fullWidthMedia => {
            fullWidthMedia.addEventListener("load", (event) => {
                GW.notificationCenter.fireEvent("Rewrite.fullWidthMediaDidLoad");
            });
        });
    });
}

/***********************************************/
/*  Add content load handler to process figures.
 */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processFigures = (info) => {
    GWLog("GW.rewriteFunctions.processFigures", "rewrite.js", 2);

	wrapImages(info);
    wrapFigures(info);
    if (info.fullWidthPossible)
        markFullWidthFigures(info);
}, {
	phase: "rewrite",
	condition: (info) => info.needsRewrite
});


/***************/
/* CODE BLOCKS */
/***************/

/***********************************************************/
/*  Wrap each pre.width-full in a div.width-full and a
    div.full-width-code-block-wrapper (for layout purposes).
 */
function wrapFullWidthPreBlocks(loadEventInfo) {
    GWLog("wrapFullWidthPreBlocks", "rewrite.js", 1);

	wrapAll("pre.width-full", (fullWidthPre) => {
		wrapElement(fullWidthPre, "width-full", "DIV", true);
		wrapElement(fullWidthPre.parentElement, "full-width-code-block-wrapper", "DIV", false);
	}, null, loadEventInfo.document);
}

/***************************************************/
/*  Add content load handler to process code blocks.
 */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processCodeBlocks = (info) => {
    GWLog("GW.rewriteFunctions.processCodeBlocks", "rewrite.js", 2);

    if (info.fullWidthPossible)
        wrapFullWidthPreBlocks(info);
}, {
	phase: "rewrite",
	condition: (info) => info.needsRewrite
});


/**************/
/* TYPOGRAPHY */
/**************/

/*****************************************/
/*  Returns the current selection as HTML.
 */
//	NOTE: This function appears to currently be unused. —SA, 2022-01-31
// function getSelectionHTML() {
//     let container = document.createElement("DIV");
//     container.appendChild(window.getSelection().getRangeAt(0).cloneContents());
//     return container.innerHTML;
// }

/******************************************************************/
/*	Configure Hyphenopoly.

    Requires Hyphenopoly_Loader.js to be loaded prior to this file.
 */
Hyphenopoly.config({
	require: {
		"en-us": "FORCEHYPHENOPOLY"
	},
	setup: {
		hide: "none",
		keepAlive: true,
		safeCopy: true
	}
});

/******************************************************************/
/*	Hyphenate with Hyphenopoly.

    Requires Hyphenopoly_Loader.js to be loaded prior to this file.
 */
function hyphenate(loadEventInfo) {
    GWLog("hyphenate", "rewrite.js", 1);

	if (GW.isX11())
		return;

	let selector = ".markdownBody p";

	if (Hyphenopoly.hyphenators)
		Hyphenopoly.hyphenators.HTML.then((hyphenate) => {
			loadEventInfo.document.querySelectorAll(selector).forEach(block => {
				hyphenate(block);
			});
		});
}

/*******************************************************************************/
/*	Set line height of element to its computed line height, rounded to the 
	nearest pixel.

	(Actually, the value of the CSS `line-height` property is set to a unitless 
	 value computed to result in the rendered line height being an integer pixel
	 value.)

	Returns the CSS property value as a string.

	If the `returnOnly` argument is TRUE, then the property is not actually set;
	the value to be set is only returned.
 */
function rectifyLineHeight(elementOrSelector, root = document, returnOnly = false) {
	if (typeof elementOrSelector == "string")
		elementOrSelector = root.querySelector(elementOrSelector);
	
	if (elementOrSelector == null)
		return null;
	
	let e = elementOrSelector;
	let style = getComputedStyle(e);
	let fontSize = parseFloat(style.fontSize);
	let lineHeight = Math.round(parseFloat(style.lineHeight));
	if (isNaN(fontSize) || isNaN(lineHeight))
		return null;
	let lineHeightPropertyValue = `calc(${lineHeight}/${fontSize})`;
	if (!returnOnly)
		e.style.lineHeight = lineHeightPropertyValue;
	return lineHeightPropertyValue;
}

/**********************************************************************/
/*	Rectify line heights of elements matching a given set of selectors.
 */
function rectifyLineHeights(loadEventInfo) {
    GWLog("rectifyLineHeights", "rewrite.js", 1);

	/*	Note: these selectors should be “all block elements on which the font 
		size is adjusted”. It seems possible but code-complex and 
		runtime-expensive to find all such elements dynamically; using a fixed
		selector list is simpler and faster but, of course, has the downside of
		many likely false negatives. (False positives can also occur but should
		be mostly harmless, or, at any rate, no more harmful than true 
		positives. Possible sources of harm from true or false positives include
		render time penalty [minor] and failure to recalculate line heights in 
		response to changes in computed values caused by, e.g., viewport shifts
		[major]. The latter could perhaps be addressed by calling this function
		from a window resize listener [inefficient] or from some sort of 
		mutation observer or some other low-overhead observer/listener that 
		watches for changes in CSS in response to various transformations and
		events.) The selector list may also become stale as changes to HTML 
		structure and to the CSS codebase are not reliably propagated to parts 
		of the code such as this.

		The current implementation should thus be considered a prototype, and
		revisited at a later date.
			—SA 2022-06-23
	 */
	let selectors = [
		"#page-description",
		"blockquote", 
		".sidenote"
	];

	/*	On the first pass, compute the adjusted values for line-height of all 
		affected elements; on the second pass, actually set the values. (This is
		done to prevent cascades of deviations from ideal [fractional] values 
		due to rounding, in nested line-height-adjusted elements.)
	 */
	let elements = [ ];
	loadEventInfo.document.querySelectorAll(selectors.join(", ")).forEach(element => {
		if (element.style.lineHeight > "")
			return;
		let lineHeight = rectifyLineHeight(element, loadEventInfo.document, true);
		elements.push({ 
			element: 	element, 
			lineHeight:	lineHeight 
		});
	});
	elements.forEach(e => {
		if (   null != e.element 
			&& null != e.lineHeight)
			e.element.style.lineHeight = e.lineHeight;
	});
}

/********************************************/
/*  Add content load handlers for typography.
 */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.typographyRewrite = (info) => {
    GWLog("GW.rewriteFunctions.typographyRewrite", "rewrite.js", 2);

	hyphenate(info);
}, {
	phase: "rewrite",
	condition: (info) => (   info.needsRewrite
						  && (   info.isMainDocument == false
						      || GW.isMobile()))
});
GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", GW.rewriteFunctions.typographyStyles = (info) => {
    GWLog("GW.rewriteFunctions.typographyStyles", "rewrite.js", 2);

	rectifyLineHeights(info);
});

/*********************/
/* FULL-WIDTH BLOCKS */
/*********************/

/*******************************************************************************/
/*  Expands all tables (& other blocks) whose wrapper block is marked with class
    ‘width-full’, and all figures marked with class ‘width-full’, to span the
    viewport (minus a specified margin on both sides).
 */
function createFullWidthBlockLayoutStyles() {
    GWLog("createFullWidthBlockLayoutStyles", "rewrite.js", 1);

    /*  Configuration and dynamic value storage.
     */
    GW.fullWidthBlockLayout = {
        sideMargin: 25,
        pageWidth: 0,
        leftAdjustment: 0
    };

    /*  Pre-query key elements, to save performance on resize.
     */
    let rootElement = document.querySelector("html");
    let markdownBody = document.querySelector("#markdownBody");

    /*  Inject styles block to hold dynamically updated layout variables.
     */
    document.querySelector("head").insertAdjacentHTML("beforeend", `<style id="full-width-block-layout-styles"></style>`);
    let fullWidthBlockLayoutStyles = document.querySelector("#full-width-block-layout-styles");

    /*  Function to update layout variables (called immediately and on resize).
     */
    let updateFullWidthBlockLayoutStyles = () => {
        GWLog("updateFullWidthBlockLayoutStyles", "rewrite.js", 2);

        GW.fullWidthBlockLayout.pageWidth = rootElement.offsetWidth;

        let markdownBodyRect = markdownBody.getBoundingClientRect();
        let markdownBodyRightMargin = GW.fullWidthBlockLayout.pageWidth - markdownBodyRect.right;
        GW.fullWidthBlockLayout.leftAdjustment = markdownBodyRect.left - markdownBodyRightMargin;

        fullWidthBlockLayoutStyles.innerHTML = `:root {
            --GW-full-width-block-layout-side-margin: ${GW.fullWidthBlockLayout.sideMargin}px;
            --GW-full-width-block-layout-page-width: ${GW.fullWidthBlockLayout.pageWidth}px;
            --GW-full-width-block-layout-left-adjustment: ${GW.fullWidthBlockLayout.leftAdjustment}px;
        }`;
    };
    updateFullWidthBlockLayoutStyles();

    //  Add listener to update layout variables on window resize.
    window.addEventListener("resize", updateFullWidthBlockLayoutStyles);
}
doWhenPageLoaded(createFullWidthBlockLayoutStyles);

/************************************/
/*  Set margins of full-width blocks.
 */
function setMarginsOnFullWidthBlocks(loadEventInfo) {
    GWLog("setMarginsOnFullWidthBlocks", "rewrite.js", 1);

    //  Get all full-width blocks in the given document.
    let allFullWidthBlocks = loadEventInfo.document.querySelectorAll("div.width-full, figure.width-full");

    let removeFullWidthBlockMargins = () => {
        allFullWidthBlocks.forEach(fullWidthBlock => {
            fullWidthBlock.style.marginLeft = "";
            fullWidthBlock.style.marginRight = "";
        });
    };

    if (!loadEventInfo.fullWidthPossible) {
        removeFullWidthBlockMargins();
        return;
    }

    //  Un-expand when mobile width, expand otherwise.
    doWhenMatchMedia(GW.mediaQueries.mobileWidth, "updateFullWidthBlockExpansionForCurrentWidthClass", () => {
        removeFullWidthBlockMargins();
    }, () => {
        allFullWidthBlocks.forEach(fullWidthBlock => {
            fullWidthBlock.style.marginLeft = `calc(
                                                    (-1 * (var(--GW-full-width-block-layout-left-adjustment) / 2.0))
                                                  + (var(--GW-full-width-block-layout-side-margin))
                                                  - ((var(--GW-full-width-block-layout-page-width) - 100%) / 2.0)
                                                )`;
            fullWidthBlock.style.marginRight = `calc(
                                                     (var(--GW-full-width-block-layout-left-adjustment) / 2.0)
                                                   + (var(--GW-full-width-block-layout-side-margin))
                                                   - ((var(--GW-full-width-block-layout-page-width) - 100%) / 2.0)
                                                )`;
        });
    });
}

/*********************************************************/
/*  Add content load handler to process full-width blocks.
 */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processFullWidthBlocks = (info) => {
    GWLog("GW.rewriteFunctions.processFullWidthBlocks", "rewrite.js", 2);

    setMarginsOnFullWidthBlocks(info);
}, { phase: ">rewrite" });


/***************/
/* ANNOTATIONS */
/***************/

/*******************************************************************************/
/*  Apply various typographic fixes (educate quotes, inject <wbr> elements after
    certain problematic characters, etc.).

    Requires typography.js to be loaded prior to this file.
 */
function rectifyTypographyInAnnotation(loadEventInfo) {
    GWLog("rectifyTypographyInAnnotation", "rewrite.js", 1);

    Typography.processElement(loadEventInfo.document,
        (  Typography.replacementTypes.QUOTES
         | Typography.replacementTypes.WORDBREAKS
         | Typography.replacementTypes.ELLIPSES),
    	true);

    //  Educate quotes in image alt-text.
    loadEventInfo.document.querySelectorAll("img").forEach(image => {
        image.alt = Typography.processString(image.alt, Typography.replacementTypes.QUOTES);
    });
}

/*****************************************************************************/
/*  Sets, in CSS, the image dimensions that are specified in HTML.
    (This is to ensure no reflow when annotation popups are spawned.)
 */
function setImageDimensionsInAnnotation(loadEventInfo) {
    GWLog("setImageDimensionsInAnnotation", "rewrite.js", 1);

    loadEventInfo.document.querySelectorAll("figure img[width]").forEach(image => {
        image.style.width = image.getAttribute("width") + "px";
    });
}

/**************************************************************************/
/*  Add content load handler for processing a loaded annotation (fragment).
 */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processAnnotation = (info) => {
    GWLog("GW.rewriteFunctions.processAnnotation", "rewrite.js", 2);

    rectifyTypographyInAnnotation(info);
    setImageDimensionsInAnnotation(info);
}, {
	phase: "rewrite",
	condition: (info) => (   info.needsRewrite
						  && info.source == "Extracts.annotationForTarget")
});

/******************************************************************************/
/*  Bind mouse hover events to, when hovering over an annotated link, highlight
	that annotation (as viewed in a tags directory, for instance).
 */
function bindSectionHighlightEventsToAnnotatedLinks(loadEventInfo) {
    GWLog("bindSectionHighlightEventsToAnnotatedLinks", "rewrite.js", 1);

    loadEventInfo.document.querySelectorAll(".link-annotated").forEach(annotatedLink => {
        //  Unbind existing events, if any.
        if (annotatedLink.annotatedLinkMouseEnter)
        	annotatedLink.removeEventListener("mouseenter", annotatedLink.annotatedLinkMouseEnter);
        if (annotatedLink.annotatedLinkMouseLeave)
        	annotatedLink.removeEventListener("mouseleave", annotatedLink.annotatedLinkMouseLeave);

        //  Bind events.
        let linkURL = fixedEncodeURIComponent(annotatedLink.href);
        let targetAnalogueInLinkBibliography = document.querySelector(`a[id^='linkBibliography'][href='${linkURL}']`);
        if (targetAnalogueInLinkBibliography) {
        	let containingSection = targetAnalogueInLinkBibliography.closest("section");
        	if (containingSection) {
				annotatedLink.addEventListener("mouseenter", annotatedLink.annotatedLinkMouseEnter = (event) => {
					clearTimeout(containingSection.highlightFadeTimer);
					containingSection.classList.toggle("highlight-fading", false);
					containingSection.classList.toggle("highlighted", true);
				});
				annotatedLink.addEventListener("mouseleave", annotatedLink.annotatedLinkMouseLeave = (event) => {
					containingSection.classList.toggle("highlight-fading", true);
					containingSection.highlightFadeTimer = setTimeout(() => {
						containingSection.classList.toggle("highlight-fading", false);
						containingSection.classList.toggle("highlighted", false);
					}, 150);
				});
        	}
        }
    });
}

/***********************************************************/
/*  Add content load handler for processing annotated links.
 */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processAnnotatedLinks = (info) => {
    GWLog("GW.rewriteFunctions.processAnnotatedLinks", "rewrite.js", 2);

    bindSectionHighlightEventsToAnnotatedLinks(info);
}, { phase: "eventListeners" });


/*********************/
/* TABLE OF CONTENTS */
/*********************/

/***************************************************************************/
/*	Strip spurious <span> tags (unavoidably added by Pandoc) from TOC links.
 */
function stripTOCLinkSpans(loadEventInfo) {
    GWLog("stripTOCLinkSpans", "rewrite.js", 1);

	unwrapAll(".TOC li a > span:not([class])", loadEventInfo.document);
}

/*************************************************************/
/*  Add content load handler for processing table of contents.
 */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processTOC = (info) => {
    GWLog("GW.rewriteFunctions.processTOC", "rewrite.js", 2);

    stripTOCLinkSpans(info);
}, {
	phase: "rewrite",
	condition: (info) => (info.needsRewrite)
});


/*************/
/* FOOTNOTES */
/*************/

/*****************************************/
/*	Add footnote class to footnote blocks.
 */
function addFootnoteClassToFootnotes(loadEventInfo) {
    GWLog("addFootnoteClassToFootnotes", "rewrite.js", 1);
    let footnotesSection = loadEventInfo.document.querySelector("#footnotes");
    if (!footnotesSection)
        return;

    let footnotes = Array.from(footnotesSection.querySelector("#footnotes > ol").children);

	footnotes.forEach(footnote => {
		footnote.classList.add("footnote");
	});
}

/******************************/
/*  Inject footnote self-links.
 */
function injectFootnoteSelfLinks(loadEventInfo) {
    GWLog("injectFootnoteSelfLinks", "rewrite.js", 1);

    let footnotesSection = loadEventInfo.document.querySelector("#footnotes");
    if (!footnotesSection)
        return;

    let footnotes = Array.from(footnotesSection.querySelector("#footnotes > ol").children);

    for (let i = 0; i < footnotes.length; i++)
        footnotes[i].insertAdjacentHTML("afterbegin",
        	`<a
        		href="#fn${(i + 1)}"
        		title="Link to footnote ${(i + 1)}"
        		class="footnote-self-link"
        			>&nbsp;</a>`);
}

/******************************************/
/*	Highlight footnote self-links on hover.
 */
function bindHighlightEventsToFootnoteSelfLinks(loadEventInfo) {
    GWLog("bindHighlightEventsToFootnoteSelfLinks", "rewrite.js", 1);

    //  Highlight footnote on hover over self-link.
    loadEventInfo.document.querySelectorAll(".footnote-self-link").forEach(footnoteSelfLink => {
        footnoteSelfLink.addEventListener("mouseenter", (event) => {
            footnoteSelfLink.parentElement.classList.toggle("highlighted", true);
        });
        footnoteSelfLink.addEventListener("mouseleave", (event) => {
            footnoteSelfLink.parentElement.classList.toggle("highlighted", false);
        });
    });
}

/*****************************************************************/
/*	Rewrite footnote back-to-citation links (generated by Pandoc).
 */
function rewriteFootnoteBackLinks(loadEventInfo) {
    GWLog("rewriteFootnoteBackLinks", "rewrite.js", 1);

    let footnotesSection = loadEventInfo.document.querySelector("#footnotes");
    if (!footnotesSection)
        return;

    let footnotes = Array.from(footnotesSection.querySelector("#footnotes > ol").children);

	/*	Base font size (1rem) is 20px at this time, making a good default.
		That value might change later, but this’ll still be a fine default;
		the width/height get adjusted below, anyway, so no big deal if the
		default is not the final value. We mostly care about having _a_ value
		for the width/height for page load performance reasons.
	 */
	let defaultSize = 20;
	footnotes.forEach(footnote => {
		let backlink = footnote.querySelector(".footnote-back");
		backlink.textContent = "";
		backlink.appendChild(newElement("IMG", {
			width: defaultSize,
			height: defaultSize,
			alt: "↩ Right arrow curving left [footnote return link] arrow",
			src: "/static/img/icons/arrow-hook-left.svg"
		}));
	});
}

/***********************************************************************/
/*	Set size properly, after setting default value in previous function.
 */
function rectifyFootnoteBackLinkArrowSize(loadEventInfo) {
    GWLog("rectifyFootnoteBackLinkArrowSize", "rewrite.js", 1);

    let footnotesSection = loadEventInfo.document.querySelector("#footnotes");
    if (!footnotesSection)
        return;

    let footnotes = Array.from(footnotesSection.querySelector("#footnotes > ol").children);

	requestIdleCallback(() => {
		let size = parseInt(getComputedStyle(footnotesSection).fontSize);
		if (!size)
			return;
		footnotes.forEach(footnote => {
			let arrow = footnote.querySelector(".footnote-back img");
			arrow.width = size;
			arrow.height = size;
		});
	});
}

/*****************************************************/
/*  Inject self-link for the footnotes section itself.
 */
function injectFootnoteSectionSelfLink(loadEventInfo) {
    GWLog("injectFootnoteSectionSelfLink", "rewrite.js", 1);

    let footnotesSection = loadEventInfo.document.querySelector("#footnotes");
    if (!footnotesSection)
        return;

    let footnotesSectionSelfLink = document.createElement("A");
    footnotesSectionSelfLink.href = "#footnotes";
    footnotesSectionSelfLink.title = "Link to section: § ‘Footnotes’";
    footnotesSectionSelfLink.classList.add("section-self-link");

    footnotesSection.insertBefore(footnotesSectionSelfLink, footnotesSection.firstElementChild.nextElementSibling);

    //  Highlight on hover.
    footnotesSectionSelfLink.addEventListener("mouseenter", (event) => {
        footnotesSectionSelfLink.previousElementSibling.classList.toggle("highlighted", true);
    });
    footnotesSectionSelfLink.addEventListener("mouseleave", (event) => {
        footnotesSectionSelfLink.previousElementSibling.classList.toggle("highlighted", false);
    });
}

/**************************************************************************/
/*  Return all {side|foot}note elements associated with the given citation.
 */
function allNotesForCitation(citation) {
    if (!citation.classList.contains("footnote-ref"))
        return null;

    let citationNumber = citation.id.substr(5);
    /*  We must check to ensure that the note in question is from the same
        page as the citation (to distinguish between main document and any
        full-page embeds that may be spawned).
     */
    return Array.from(document.querySelectorAll(`#fn${citationNumber}, #sn${citationNumber}`)).filter(note => note.querySelector(".footnote-back").pathname == citation.pathname);
}

/***************************************************************************/
/*  Bind mouse hover events to, when hovering over a citation, highlight all
    {side|foot}notes associated with that citation.
 */
function bindNoteHighlightEventsToCitations(loadEventInfo) {
    GWLog("bindNoteHighlightEventsToCitations", "rewrite.js", 1);

    loadEventInfo.document.querySelectorAll(".footnote-ref").forEach(citation => {
        //  Unbind existing events, if any.
        if (citation.citationMouseEnter)
        	citation.removeEventListener("mouseenter", citation.citationMouseEnter);
        if (citation.citationMouseLeave)
        	citation.removeEventListener("mouseleave", citation.citationMouseLeave);

        //  Bind events.
        let notesForCitation = allNotesForCitation(citation);
        citation.addEventListener("mouseenter", citation.citationMouseEnter = (event) => {
            notesForCitation.forEach(note => {
                note.classList.toggle("highlighted", true);
            });
        });
        citation.addEventListener("mouseleave", citation.citationMouseLeave = (event) => {
            notesForCitation.forEach(note => {
                note.classList.toggle("highlighted", false);
            });
        });
    });
}

/*******************************************/
/*  Add a TOC link to the footnotes section.
 */
function injectFootnotesTOCLink(loadEventInfo) {
    GWLog("injectFootnotesTOCLink", "rewrite.js", 1);

    let footnotesSection = loadEventInfo.document.querySelector("#footnotes");
    let TOCList = loadEventInfo.document.querySelector("#TOC > ul");
    if (   TOCList
    	&& footnotesSection)
        TOCList.insertAdjacentHTML("beforeend", `<li><a href="#footnotes">Footnotes</a></li>\n`);
}

/**************************************************************/
/*  Add content load handlers for processing footnotes section.
 */

GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processFootnotes = (info) => {
    GWLog("GW.rewriteFunctions.processFootnotes", "rewrite.js", 2);

    addFootnoteClassToFootnotes(info);
    injectFootnoteSectionSelfLink(info);
    injectFootnoteSelfLinks(info);
    rewriteFootnoteBackLinks(info);
}, {
	phase: "rewrite",
	condition: (info) => (   info.needsRewrite
						  && info.isFullPage)
});

GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.injectFootnotesTOCLink = (info) => {
    GWLog("GW.rewriteFunctions.injectFootnotesTOCLink", "rewrite.js", 2);

    injectFootnotesTOCLink(info);
}, {
	phase: "rewrite",
	condition: (info) => info.isMainDocument
});

GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processCitations = (info) => {
    GWLog("GW.rewriteFunctions.processCitations", "rewrite.js", 2);

	rectifyFootnoteBackLinkArrowSize(info);
	bindHighlightEventsToFootnoteSelfLinks(info);
    bindNoteHighlightEventsToCitations(info);
}, { phase: "eventListeners" });


/*********/
/* LINKS */
/*********/

/********************************************************************/
/*  Designate self-links (a.k.a. anchorlinks) and local links (a.k.a.
    within-site links) as such.
 */
function addSpecialLinkClasses(loadEventInfo) {
    GWLog("addSpecialLinkClasses", "rewrite.js", 1);

    loadEventInfo.document.querySelectorAll(".markdownBody a[href]").forEach(link => {
        if (   link.hostname != location.hostname
            || link.closest("h1, h2, h3, h4, h5, h6")
            || link.closest(".section-self-link, .footnote-ref, .footnote-back, .footnote-self-link, .sidenote-self-link"))
            return;

        if (   loadEventInfo.location
            && link.pathname == loadEventInfo.location.pathname
            && (   loadEventInfo.isFullPage
            	|| null != loadEventInfo.document.querySelector(selectorFromHash(link.hash)))) {
            link.swapClasses([ "link-self", "link-local" ], 0);
        } else if (link.pathname.slice(1).match(/[\.]/) == null) {
            link.swapClasses([ "link-self", "link-local" ], 1);
        }
    });
}

/************************************************************************/
/*	Assign proper link icons to self-links (directional or otherwise) and
	local links.
 */
function designateSpecialLinkIcons(loadEventInfo) {
    GWLog("designateSpecialLinkIcons", "rewrite.js", 1);

	//	Self-links (anchorlinks to the current page).
	loadEventInfo.document.querySelectorAll(".link-self").forEach(link => {
		link.dataset.linkIconType = "text";
		link.dataset.linkIcon = "\u{00B6}"; // ¶

		/*  Directional navigation links on self-links: for each self-link like
			“see [later](#later-identifier)”, find the linked identifier,
			whether it’s before or after, and if it is before/previously,
			annotate the self-link with ‘↑’ and if after/later, ‘↓’. This helps
			the reader know if it’s a backwards link to an identifier already
			read, or an unread identifier.
		 */
        let target = loadEventInfo.document.querySelector(selectorFromHash(link.hash));
        if (!target)
        	return;

        link.dataset.linkIconType = "svg";
        link.dataset.linkIcon =
        	(link.compareDocumentPosition(target) == Node.DOCUMENT_POSITION_FOLLOWING
             ? 'arrow-down'
             : 'arrow-up');
	});

	//	Local links (to other pages on the site).
	loadEventInfo.document.querySelectorAll(".link-local").forEach(link => {
		if (link.dataset.linkIcon)
			return;

		link.dataset.linkIconType = "text";
		link.dataset.linkIcon = "\u{1D50A}"; // 𝔊
	});
}

/************************************************/
/*  Add content load handler for link processing.
 */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processLinks = (info) => {
    GWLog("GW.rewriteFunctions.processLinks", "rewrite.js", 2);

    addSpecialLinkClasses(info);
    designateSpecialLinkIcons(info);
}, { phase: "rewrite" });


/*********/
/* MISC. */
/*********/

/***************************************************************************/
/*  Clean up image alt-text. (Shouldn’t matter, because all image URLs work,
    right? Yeah, right...)
 */
function cleanUpImageAltText(loadEventInfo) {
    GWLog("cleanUpImageAltText", "rewrite.js", 1);

    loadEventInfo.document.querySelectorAll("img[alt]").forEach(image => {
        image.alt = decodeURIComponent(image.alt.replace(/%(?![A-Fa-f0-9]{2})/g, "%25"));
    });
}

/************************************************************************/
/*	Prevent line breaks immediately before citations (which “orphans” the
	citation on the next line, and looks ugly).
 */
function noBreakForCitations(loadEventInfo) {
    GWLog("noBreakForCitations", "rewrite.js", 1);

	loadEventInfo.document.querySelectorAll(".footnote-ref").forEach(citation => {
		citation.insertAdjacentHTML("beforebegin", "&NoBreak;");
		let textNode = citation.querySelector("sup").firstTextNode;
		textNode.textContent = "\u{2060}" + textNode.textContent;
	});
}

/**************************************************************/
/*  Add content load handler for doing miscellaneous rewriting.
 */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processMiscellaneousRewrites = (info) => {
    GWLog("GW.rewriteFunctions.processMiscellaneousRewrites", "rewrite.js", 2);

    cleanUpImageAltText(info);
    noBreakForCitations(info);
}, {
	phase: "rewrite",
	condition: (info) => info.needsRewrite
});

/***************************************************************************/
/*	Call the given function when the element specified by the given selector
	intersects the viewport.
	NOTE: This function is currently unused. (It was used to load Disqus.)
		—SA 2022-04-21
 */
function lazyLoadObserver(f, selector) {
	let observer = new IntersectionObserver((entries) => {
		if (entries[0].intersectionRatio <= 0)
			return;
		f();
		observer.disconnect();
	});

	let target = document.querySelector(selector);
	if (target)
		observer.observe(target);
}


/*************/
/* DROP CAPS */
/*************/

/*******************************************************/
/*  Apply classes to blocks that should have a drop cap.
 */
function applyDropCapsClasses(loadEventInfo) {
    GWLog("applyDropCapsClasses", "rewrite.js", 1);

	let loadedDocBody = loadEventInfo.document.querySelector("body");

    //  Add ‘drop-cap-’ class to requisite blocks.
    let dropCapBlocksSelector = [
    	".markdownBody > p:first-child",
    	".markdownBody > .epigraph:first-child + p",
    	".markdownBody .abstract:not(.scrape-abstract-not) + p"
    ].join(", ");
    let dropCapClass = Array.from(loadedDocBody.classList).find(cssClass => cssClass.startsWith("drop-caps-"));
    if (dropCapClass)
        dropCapClass = dropCapClass.replace("-caps-", "-cap-");

	loadEventInfo.document.querySelectorAll(dropCapBlocksSelector).forEach(dropCapBlock => {
		/*  Drop cap class could be set globally, or overridden by a .abstract;
			the latter could be `drop-cap-no` (which nullifies any page-global
			drop-cap class for the given block).
		 */
		let precedingAbstract = (   dropCapBlock.previousElementSibling
								 && dropCapBlock.previousElementSibling.classList.contains("abstract"))
								? dropCapBlock.previousElementSibling
								: null;
		dropCapClass = (precedingAbstract
						? Array.from(precedingAbstract.classList).find(cssClass => cssClass.startsWith("drop-cap-"))
						: null) || dropCapClass;
		if (   dropCapClass
			&& dropCapClass != "drop-cap-no")
			dropCapBlock.classList.add(dropCapClass);
	});
}

/******************************************/
/*  Add content load handler for drop caps.
 */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processDropCaps = (info) => {
    GWLog("GW.rewriteFunctions.processDropCaps", "rewrite.js", 2);

    applyDropCapsClasses(info);
}, {
	phase: "rewrite",
	condition: (info) => info.isMainDocument
});


/********/
/* MATH */
/********/

/*****************************************************************************/
/*	Makes it so that copying a rendered equation or other math element copies
	the LaTeX source, instead of the useless gibberish that is the contents of
	the text nodes of the HTML representation of the equation.
 */
function addCopyListenersToMathBlocks(loadEventInfo) {
    GWLog("addCopyListenersToMathBlocks", "rewrite.js", 1);

	loadEventInfo.document.querySelectorAll(".mjx-chtml").forEach(mathBlock => {
		mathBlock.addEventListener("copy", (event) => {
			event.preventDefault();
			let latexSource = event.target.closest(".mjx-math").getAttribute("aria-label");
			event.clipboardData.setData("text/plain", latexSource);
			event.clipboardData.setData("text/html", latexSource);
		});
	});
}

/******************************************************************************/
/*	Makes double-clicking on a math element select the entire math element.
	(This actually makes no difference to the behavior of the copy listener
	 [see `addCopyListenersToMathBlocks`], which copies the entire LaTeX source
	 of the full equation no matter how much of said equation is selected when
	 the copy command is sent; however, it ensures that the UI communicates the
	 actual behavior in a more accurate and understandable way.)
 */
function addDoubleClickListenersToMathBlocks(loadEventInfo) {
    GWLog("addDoubleClickListenersToMathBlocks", "rewrite.js", 1);

	loadEventInfo.document.querySelectorAll(".mjpage").forEach(mathBlock => {
		mathBlock.addEventListener("dblclick", (event) => {
			document.getSelection().selectAllChildren(mathBlock.querySelector(".mjx-chtml"));
		});
		mathBlock.title = mathBlock.classList.contains("mjpage__block")
						  ? "Double-click to select equation, then copy, to get LaTeX source (or, just click the Copy button in the top-right of the equation area)"
						  : "Double-click to select equation; copy to get LaTeX source";
	});
}

/****************************************************************/
/*	Add block buttons (copy) to block (not inline) math elements.
 */
function addBlockButtonsToMathBlocks(loadEventInfo) {
    GWLog("addBlockButtonsToMathBlocks", "rewrite.js", 1);

	loadEventInfo.document.querySelectorAll(".mjpage__block").forEach(mathBlock => {
		//	Inject button bar.
		mathBlock.insertAdjacentHTML("beforeend",
			  `<span class="block-button-bar">`
			+ `<button type="button" class="copy" tabindex="-1" title="Copy LaTeX source of this equation to clipboard">`
			+ `<img src="/static/img/icons/copy.svg">`
			+ `</button>`
			+ `<span class="scratchpad"></span>`
			+ `</span>`);

		//	Activate buttons.
		requestAnimationFrame(() => {
			//	Copy button (copies LaTeX source);
			let latexSource = mathBlock.querySelector(".mjx-math").getAttribute("aria-label");
			let scratchpad = mathBlock.querySelector(".scratchpad");
			mathBlock.querySelector("button.copy").addActivateEvent((event) => {
				GWLog("mathBlockCopyButtonClicked", "rewrite.js", 3);

				//	Perform copy operation.
				scratchpad.innerText = latexSource;
				selectElementContents(scratchpad);
				document.execCommand("copy");
				scratchpad.innerText = "";

				//	Flash math block, for visual feedback of copy operation.
				mathBlock.classList.add("flash");
				setTimeout(() => { mathBlock.classList.remove("flash"); }, 150);
			});
		});
	});
}

/***********************************************/
/*  Add content load handlers for math elements.
 */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processMathElements = (info) => {
    GWLog("GW.rewriteFunctions.processMathElements", "rewrite.js", 2);

    addCopyListenersToMathBlocks(info);
    addDoubleClickListenersToMathBlocks(info);
}, {
	phase: "eventListeners"
});

GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.rewriteMathElements = (info) => {
    GWLog("GW.rewriteFunctions.processMathElements", "rewrite.js", 2);

    addBlockButtonsToMathBlocks(info);
}, {
	phase: "rewrite",
	condition: (info) => info.needsRewrite
});


/********************/
/* BACK TO TOP LINK */
/********************/

/***********************************************************************/
/*  Injects the “back to top” link. (Called only for the main document.)
 */
function injectBackToTopLink(loadEventInfo) {
    GWLog("injectBackToTopLink", "rewrite.js", 1);

    GW.backToTop = addUIElement(`<div id="back-to-top"><a href="#top" tabindex="-1" title="Back to top">` +
        `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="M6.1 422.3l209.4-209.4c4.7-4.7 12.3-4.7 17 0l209.4 209.4c4.7 4.7 4.7 12.3 0 17l-19.8 19.8c-4.7 4.7-12.3 4.7-17 0L224 278.4 42.9 459.1c-4.7 4.7-12.3 4.7-17 0L6.1 439.3c-4.7-4.7-4.7-12.3 0-17zm0-143l19.8 19.8c4.7 4.7 12.3 4.7 17 0L224 118.4l181.1 180.7c4.7 4.7 12.3 4.7 17 0l19.8-19.8c4.7-4.7 4.7-12.3 0-17L232.5 52.9c-4.7-4.7-12.3-4.7-17 0L6.1 262.3c-4.7 4.7-4.7 12.3 0 17z"/></svg>`
        + `</a></div>`);

	//  Show/hide the back-to-top link on scroll up/down.
	addScrollListener(updateBackToTopLinkVisibility, "updateBackToTopLinkVisibilityScrollListener", true);
}

/***********************************************************/
/*  Add content load handler to inject the back-to-top link.
 */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.injectBackToTopLink = (info) => {
    GWLog("GW.rewriteFunctions.injectBackToTopLink", "rewrite.js", 2);

    injectBackToTopLink(info);
}, {
	phase: "rewrite",
	condition: (info) => info.isMainDocument
});

/*******************************************************************************/
/*  Show/hide the back-to-top link in response to scrolling.

    Called by the ‘updateBackToTopLinkVisibilityScrollListener’ scroll listener.
 */
function updateBackToTopLinkVisibility(event) {
    GWLog("updateBackToTopLinkVisibility", "rewrite.js", 0);

    /*  Show back-to-top link on ANY scroll up, or when scrolling a full page
        down from the top.
     */
    if (   GW.scrollState.unbrokenUpScrollDistance > 0
    	|| GW.scrollState.unbrokenDownScrollDistance > window.innerHeight)
        GW.backToTop.classList.toggle("hidden", false);

    //  Hide back-to-top link when scrolling to top.
    if (GW.scrollState.lastScrollTop <= 0)
        GW.backToTop.classList.toggle("hidden", true);
}


/*****************/
/* END OF LAYOUT */
/*****************/

doWhenPageLoaded(() => {
    GW.notificationCenter.fireEvent("Rewrite.pageLayoutWillComplete");
    requestAnimationFrame(() => {
        GW.pageLayoutComplete = true;
        GW.notificationCenter.fireEvent("Rewrite.pageLayoutDidComplete");
    });
}, { once: true });


/**************************/
/* BROKEN ANCHOR CHECKING */
/**************************/
/*	If a reader loads a page and the anchor ID/hash does not exist inside the page,
	fire off a request to the 404 page, whose logs are reviewed manually,
	with the offending page+anchor ID, for correction (either fixing an outdated
	link somewhere on gwern.net, or adding a span/div manually to the page to
	make old inbound links go where they ought to).

    Such broken anchors can reflect out of date cross-page references, or reflect
	incoming URLs from elsewhere on the Internet which are broken/outdated.
	(Within-page anchor links are checked statically at compile-time, and those
	 errors should never exist.)
 */

function reportBrokenAnchorLink(link) {
	GWLog("reportBrokenAnchorLink", "rewrite.js", 1);

	if (link.hash == "")
		return;

	GWServerLogError(fixedEncodeURIComponent(link.pathname) + "--" + fixedEncodeURIComponent(link.hash.substr(1)), "broken hash-anchor");
}

function brokenAnchorCheck() {
	GWLog("brokenAnchorCheck", "rewrite.js", 1);

	if (   location.hash > ""
		&& /^#if_slide_([0-9]+)/.test(location.hash) == false
		&& /^#:~:/.test(location.hash) == false
		&& document.querySelector(selectorFromHash(location.hash)) == null)
		reportBrokenAnchorLink(location);
}
doWhenPageLoaded(brokenAnchorCheck);


/********************/
/* HASH REALIGNMENT */
/********************/

/*  This is necessary to defeat a bug where if the page is loaded with the URL
    hash targeting some element, the element does not match the :target CSS
    pseudo-class.
 */
function realignHash() {
    GWLog("realignHash", "rewrite.js", 1);

    //  Chrome’s fancy new “scroll to text fragment”. Deal with it in Firefox.
    if (GW.isFirefox()) {
        if (location.hash.startsWith("#:~:")) {
            GW.hashRealignValue = (GW.hashRealignValue || "#");
        } else if (location.hash.includes(":~:")) {
            GW.hashRealignValue = (GW.hashRealignValue || location.hash.replace(/:~:.*$/, ""));
        }
    }

    let hash = (GW.hashRealignValue || location.hash);
    if (hash > "") {
        //  Strip hash.
        history.replaceState(null, null, "#");

        //  Reset hash.
        location.hash = hash;

        //  Prevent redundant realignment.
        GW.hashRealignValue = null;
    }
}
doWhenDOMContentLoaded(realignHash);
doWhenPageLoaded(() => {
    requestAnimationFrame(realignHash);
});


/*****************************************************************************************/
/*! instant.page v5.1.0 - (C) 2019-2020 Alexandre Dieulot - https://instant.page/license */
/* Settings: 'prefetch' (loads HTML of target) after 1600ms hover (desktop) or mouse-down-click (mobile); TODO: left in logging for testing during experiment */
let pls="a:not(.has-content)";let t,e;const n=new Set,o=document.createElement("link"),z=o.relList&&o.relList.supports&&o.relList.supports("prefetch")&&window.IntersectionObserver&&"isIntersecting"in IntersectionObserverEntry.prototype,s="instantAllowQueryString"in document.body.dataset,a=true,r="instantWhitelist"in document.body.dataset,c="instantMousedownShortcut"in document.body.dataset,d=1111;let l=1600,u=!1,f=!1,m=!1;if("instantIntensity"in document.body.dataset){const t=document.body.dataset.instantIntensity;if("mousedown"==t.substr(0,"mousedown".length))u=!0,"mousedown-only"==t&&(f=!0);else if("viewport"==t.substr(0,"viewport".length))navigator.connection&&(navigator.connection.saveData||navigator.connection.effectiveType&&navigator.connection.effectiveType.includes("2g"))||("viewport"==t?document.documentElement.clientWidth*document.documentElement.clientHeight<45e4&&(m=!0):"viewport-all"==t&&(m=!0));else{const e=parseInt(t);isNaN(e)||(l=e)}}if(z){const n={capture:!0,passive:!0};if(f||document.addEventListener("touchstart",function(t){e=performance.now();const n=t.target.closest(pls);console.log(n);if(!h(n))return;v(n.href)},n),u?c||document.addEventListener("mousedown",function(t){const e=t.target.closest(pls);if(!h(e))return;v(e.href)},n):document.addEventListener("mouseover",function(n){if(performance.now()-e<d)return;const o=n.target.closest(pls);if(!h(o))return;o.addEventListener("mouseout",p,{passive:!0}),t=setTimeout(()=>{v(o.href),t=void 0},l)},n),c&&document.addEventListener("mousedown",function(t){if(performance.now()-e<d)return;const n=t.target.closest("a");if(t.which>1||t.metaKey||t.ctrlKey)return;if(!n)return;n.addEventListener("click",function(t){1337!=t.detail&&t.preventDefault()},{capture:!0,passive:!1,once:!0});const o=new MouseEvent("click",{view:window,bubbles:!0,cancelable:!1,detail:1337});n.dispatchEvent(o)},n),m){let t;(t=window.requestIdleCallback?t=>{requestIdleCallback(t,{timeout:1500})}:t=>{t()})(()=>{const t=new IntersectionObserver(e=>{e.forEach(e=>{if(e.isIntersecting){const n=e.target;t.unobserve(n),v(n.href)}})});document.querySelectorAll("a").forEach(e=>{h(e)&&t.observe(e)})})}}function p(e){e.relatedTarget&&e.target.closest("a")==e.relatedTarget.closest("a")||t&&(clearTimeout(t),t=void 0)}function h(t){if(t&&t.href&&(!r||"instant"in t.dataset)&&(a||t.origin==location.origin||"instant"in t.dataset)&&["http:","https:"].includes(t.protocol)&&("http:"!=t.protocol||"https:"!=location.protocol)&&(s||!t.search||"instant"in t.dataset)&&!(t.hash&&t.pathname+t.search==location.pathname+location.search||"noInstant"in t.dataset))return!0}function v(t){if(n.has(t))return;const e=document.createElement("link");console.log("Prefetched: "+t);e.rel="prefetch",e.href=t,document.head.appendChild(e),n.add(t)};
