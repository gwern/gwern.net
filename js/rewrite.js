/* Miscellaneous JS functions which run after the page loads to rewrite or adjust parts of the page. */
/* author: Said Achmiz */
/* license: MIT */

/*******************************************************************************/
/*  NOTE on the GW.contentDidLoad and GW.contentDidInject events:

    These events are fired whenever any new local page content is loaded and
    injected into the page, respectively. (Here “loaded” may mean “loaded via a
    network request”, “constructed from a template”, or any other process by 
    which a new unit of page content is created. This includes the initial page 
    load, but also such things as annotations being lazy-loaded, etc. Likewise,
    “injected” may mean “injected into the base page”, “injected into a 
    pop-frame shadow-root”, “injected into a DocumentFragment in cache”, etc.)

    Many event handlers are attached to these, because a great deal of
    processing must take place before newly-loaded page content is ready for
    presentation to the user. Typography rectification must take place; the HTML
    structure of certain page elements (such as tables, figures, etc.) must be
    reconfigured; CSS classes must be added; various event listeners attached;
    etc. Most of this file (rewrite.js) consists of exactly such “content load
    handlers” and “content inject handlers”, a.k.a. “rewrite functions”. 
    (Additional content load and inject handlers are defined elsewhere in the 
    code, as appropriate; e.g. the handler that attaches event listeners to 
    annotated links to load annotations when the user mouses over such links, 
    which is found in extracts-annotations.js.)

    The GW.contentDidLoad event has the following named handler phases (see
    gw-inline.js for details on what this means):

        [ "transclude", "rewrite" ]

    The GW.contentDidInject event has the following named handler phases:

        [ "rewrite", "eventListeners" ]

    The GW.contentDidLoad and GW.contentDidInject events should have the 
    following keys and values in their event info dictionary (see gw-inline.js 
    for details on event info dictionaries):

        ‘source’ (key) (required)
            String that indicates function (or event name, if fired from a
            browser event listener) from which the event is fired (such as
            ‘Annotation.load’).

		‘container’ (key) (required)
            DOM object containing the loaded content. (For the GW.contentDidLoad
            event fired on the initial page load, the value of this key is
            `document`, i.e. the root document of the page. For pop-frames, this
            may be the `document` property of the pop-frame, or a
            DocumentFragment containing the embedded page elements.) The 
            container will contain nothing but the newly-loaded content.
            (This key can be thought of as “what has been loaded?”.)

        ‘document’ (key) (required)
            Document into which the content was loaded. May or may not be 
            identical with the value of the ‘container’ key (in those cases when
            the loaded content is a whole document itself). The value of this 
            key is necessarily either a Document (i.e., the root document of the
            page) or a DocumentFragment. (This key can be thought of as “into 
            where has the loaded content been loaded?”.)

		‘contentType’ (key)
			String that indicates content type of the loaded content. Might be
			null (which indicates the default content type: local page content).
			Otherwise may be `annotation` or something else.

        ‘loadLocation’ (key)
            URL object (https://developer.mozilla.org/en-US/docs/Web/API/URL)
            which specifies the URL from which the loaded content was loaded.
            For the main page, the represented URL will be the value of
            `location.href`. For pop-frames, transcludes, etc., the represented
            URL will be that of the page in which the content resides. (If the
            loaded/injected content is not sourced from any page, this key will
            have a null value.)

    The GW.contentDidInject event should additionally have a value for the 
    following key:

        ‘flags’ (key) (required)
            Bit field containing various flags (combined via bitwise OR). The
            values of the flags are defined in GW.contentDidInjectEventFlags.

            (Note that event handlers for the ‘GW.contentDidInject’ event can
             access the values of these flags directly via property access on
             the event info, e.g. the following two expressions are equivalent:

               eventInfo.flags & GW.contentDidInjectEventFlags.clickable != 0

               eventInfo.clickable

             It is recommended that the latter form be used.)

            The flags are:

            ‘clickable’
                Currently unused. Reserved for future use.

            ‘stripCollapses’
                Specifies whether the loaded content is permitted to have
                collapsed sections. Generally false. If the value of this key is 
                true, then any collapse blocks in the loaded content will be 
                automatically expanded and stripped, and all content in
                collapsible sections will be visible at all times.

            ‘fullWidthPossible’
                Specifies whether full-width elements are permitted in the
                loaded content. Generally true only for the main page load. If
                false, elements marked as full-width will be laid out as if for
                a mobile (narrow) viewport, regardless of the actual dimensions
                of the loaded content’s container (i.e. they will not actually
                be “full-width”).
 */


/***********/
/* HELPERS */
/***********/

/*****************************************************************************/
/*	Return fully qualified, versioned (if possible) URL for asset at the given
	path.
 */
function versionedAssetURL(pathname) {
	let version = GW.assetVersions[pathname];
	let versionString = (version ? `?v=${version}` : ``);
	return new URL(  "https://"
				   + location.hostname
				   + pathname
				   + versionString);
}

/********************************/
/*	Used in nearestBlockElement()
 */
GW.defaultBlockElementSelectors = [
	[	".footnote",
		".sidenote"
		].join(", "),
	".aux-links-append",
	"li",
	"p",
	[	"section",
		".markdownBody > *",
		".include-wrapper-block"
		].join(", ")
];

/********************************************************************/
/*	Return the nearest block element that contains the given element.
 */
function nearestBlockElement(element, blockElementSelectors = GW.defaultBlockElementSelectors) {
	if (typeof blockElementSelectors == "string")
		blockElementSelectors = [ blockElementSelectors ];

	let block = null;
	for (selector of blockElementSelectors)
		if (block = element.closest(selector))
			break;

	return block;
}

/****************************************************************************/
/*	Return the element, in the target document, pointed to by the hash of the
	given link (which may be a URL object or an <a> Element).
 */
function targetElementInDocument(link, doc) {
	let element = doc.querySelector(selectorFromHash(link.hash));
	if (element)
		return element;

	if (link instanceof Element) {
		let backlinksList = link.closest(".backlinks-list");
		if (backlinksList) {
			let escapedURL = CSS.escape(backlinksList.dataset.targetUrl);
			element = doc.querySelector(`a.link-page[id][href*='${escapedURL}']`);
		}
	}

	if (element == null)
		reportBrokenAnchorLink(link);

	return element;
}

/******************************************************************************/
/*	Return original URL for a link. (Equal to the link’s URL itself for all but
	locally archived links.)
 */
function originalURLForLink(link) {
	if (   link.dataset.urlOriginal == null
		|| link.dataset.urlOriginal == "")
		return new URL(link.href);

	let originalURL = new URL(link.dataset.urlOriginal);

	/*  Special cases where the original URL of the target does not
		match the target’s proper identifier (possibly due to outgoing
		link rewriting).
	 */
	if (originalURL.hostname == "ar5iv.labs.arxiv.org") {
		originalURL.hostname = "arxiv.org";
		originalURL.pathname = originalURL.pathname.replace("/html/", "/abs/");
		/*	Erase the ?fallback=original query parameter necessary to 
			make it redirect if no Ar5iv version is available.
		 */
		originalURL.search = ""; 
	}

	return originalURL;
}

/*****************************************************************************/
/*	Construct synthetic include-link. The optional ‘link’ argument may be 
	a string, a URL object, or an <a> Element, in which case it, or its .href 
	property, is used as the ‘href’ attribute of the synthesized include-link.

	If the ‘link’ argument is an <a> Element and has a ‘data-url-original’ 
	attribute, then the same attribute is assigned the same value on the
	synthesized include-link.
 */
function synthesizeIncludeLink(link, attributes, properties) {
	let includeLink = newElement("A", attributes, properties);

	if (link == null)
		return includeLink;

	if (typeof link == "string")
		includeLink.href = link;
	else if (   link instanceof Element
			 || link instanceof URL)
		includeLink.href = link.href;

	if (   link instanceof Element
		&& link.dataset.urlOriginal)
		includeLink.dataset.urlOriginal = link.dataset.urlOriginal;

	return includeLink;
}

/******************************************************************************/
/*	Returns the heading level of a <section> element. (Given by a class of the
	form ‘levelX’ where X is a positive integer. Defaults to 1 if no such class
	is present.)
 */
function sectionLevel(section) {
	if (  !section
		|| section.tagName != "SECTION")
		return null;

	//	Note: ‘m’ is a regexp matches array.
	let m = Array.from(section.classList).map(c => c.match(/^level([0-9]*)$/)).find(m => m);
	return (m ? parseInt(m[1]) : 1);
}

/*****************************************************************************/
/*  Add content load handler (i.e., an event handler for the GW.contentDidLoad
    event).
 */
function addContentLoadHandler(handler, phase, condition = null) {
    let options = { phase: phase };
    if (condition)
        options.condition = condition;
    GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", handler, options);
}

GW.contentLoadHandlers = { };

/*************************************************************/
/*  Add content inject handler (i.e., an event handler for the 
	GW.contentDidInject event).
 */
function addContentInjectHandler(handler, phase, condition = null) {
    let options = { phase: phase };
    if (condition)
        options.condition = condition;
    GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", handler, options);
}

GW.contentInjectHandlers = { };

/*****************************************************************************/
/*  Adds the given copy processor, appending it to the existing array thereof.

    Each copy processor should take two arguments: the copy event, and the
    DocumentFragment which holds the selection as it is being processed by each
    successive copy processor.

    A copy processor should return true if processing should continue after it’s
    done, false otherwise (e.g. if it has entirely replaced the contents of the
    selection object with what the final clipboard contents should be).
 */
function addCopyProcessor(processor) {
    if (GW.copyProcessors == null)
        GW.copyProcessors = [ ];

    GW.copyProcessors.push(processor);
}


/*************/
/* AUX-LINKS */
/*************/

AuxLinks = {
    auxLinksLinkTypes: {
        "/metadata/annotations/backlinks/":          "backlinks",
        "/metadata/annotations/similars/":           "similars",
        "/metadata/annotations/link-bibliography/":  "link-bibliography"
    },

    auxLinksLinkType: (link) => {
        for ([ pathnamePrefix, linkType ] of Object.entries(AuxLinks.auxLinksLinkTypes))
            if (link.pathname.startsWith(pathnamePrefix))
                return linkType;

        return null;
    },

    /*  Page or document for whom the aux-links are.
     */
    targetOfAuxLinksLink: (link) => {
        for ([ pathnamePrefix, linkType ] of Object.entries(AuxLinks.auxLinksLinkTypes)) {
            if (link.pathname.startsWith(pathnamePrefix)) {
                if (link.pathname.endsWith(".html")) {
                    let start = pathnamePrefix.length;
                    let end = (link.pathname.length - ".html".length);
                    return decodeURIComponent(decodeURIComponent(link.pathname.slice(start, end)));
                } else {
                    let start = (pathnamePrefix.length - 1);
                    return link.pathname.slice(start);
                }
            }
        }

        return null;
    }
};


/*********/
/* NOTES */
/*********/

Notes = {
	/*	Get the (side|foot)note number from the URL hash (which might point to a 
		footnote, a sidenote, or a citation).
	 */
	noteNumberFromHash: (hash = location.hash) => {
		if (hash.startsWith("#") == false)
			hash = "#" + hash;

		if (hash.match(/#[sf]n[0-9]/))
			return hash.substr(3);
		else if (hash.match(/#fnref[0-9]/))
			return hash.substr(6);
		else
			return "";
	},

	citationSelectorMatching: (element) => {
		return ("#" + Notes.idForCitationNumber(Notes.noteNumberFromHash(element.hash)));
	},

	footnoteSelectorMatching: (element) => {
		return ("#" + Notes.idForFootnoteNumber(Notes.noteNumberFromHash(element.hash)));
	},

	sidenoteSelectorMatching: (element) => {
		return ("#" + Notes.idForSidenoteNumber(Notes.noteNumberFromHash(element.hash)));
	},

	idForCitationNumber: (number) => {
		return `fnref${number}`;
	},

	idForFootnoteNumber: (number) => {
		return `fn${number}`;
	},

	idForSidenoteNumber: (number) => {
		return `sn${number}`;
	},

	setCitationNumber: (citation, number) => {
        //  #fnN
        citation.hash = citation.hash.slice(0, 3) + number;

        //  fnrefN
        citation.id = citation.id.slice(0, 5) + number;

        //  Link text.
        citation.firstElementChild.textContent = number;
	},

	setFootnoteNumber: (footnote, number) => {
        //  fnN
        footnote.id = footnote.id.slice(0, 2) + number;

        //  #fnrefN
        let footnoteBackLink = footnote.querySelector("a.footnote-back");
        footnoteBackLink.hash = footnoteBackLink.hash.slice(0, 6) + number;

		//	#fnN
		let footnoteSelfLink = footnote.querySelector("a.footnote-self-link");
		footnoteSelfLink.hash = footnoteSelfLink.hash.slice(0, 3) + number;
		footnoteSelfLink.title = "Link to footnote " + number;
	},

	/**************************************************************************/
	/*  Return all {side|foot}note elements associated with the given citation.
	 */
	allNotesForCitation: (citation) => {
		if (!citation.classList.contains("footnote-ref"))
			return null;

		let citationNumber = citation.id.substr(5);
		/*  We must check to ensure that the note in question is from the same
			page as the citation (to distinguish between main document and any
			full-page embeds that may be spawned).
		 */
		let selector = `#fn${citationNumber}, #sn${citationNumber}`;
		let allNotes = Array.from(document.querySelectorAll(selector)).concat(Array.from(citation.getRootNode().querySelectorAll(selector)));
		return allNotes.filter(note => {
			let footnoteBackLink = note.querySelector(".footnote-back");
			return (   footnoteBackLink != null
					&& footnoteBackLink.pathname == citation.pathname);
		});
	}
};

/*************/
/* CLIPBOARD */
/*************/

/****************************************************************************/
/*  Set up the copy processor system by registering a ‘copy’ event handler to
    call copy processors.
 */
addContentInjectHandler(GW.contentInjectHandlers.registerCopyProcessors = (eventInfo) => {
    GWLog("registerCopyProcessors", "rewrite.js", 1);

    eventInfo.document.addEventListener("copy", (event) => {
        event.preventDefault();
        event.stopPropagation();

        let selection = getSelectionAsDocument(eventInfo.document);

        let i = 0;
        while (   i < GW.copyProcessors.length
               && GW.copyProcessors[i++](event, selection));

        event.clipboardData.setData("text/plain", selection.textContent);
        event.clipboardData.setData("text/html", selection.innerHTML);
    });
}, "eventListeners", (info) => (info.container == info.document.body));


/**********/
/* TABLES */
/**********/

/****************************************************************/
/*  Wrap each table in a div.table-wrapper (for layout purposes).
 */
addContentLoadHandler(GW.contentLoadHandlers.wrapTables = (eventInfo) => {
    GWLog("wrapTables", "rewrite.js", 1);

    wrapAll("table", "table-wrapper", "DIV", eventInfo.container, true)
}, "rewrite");

/*****************************************************************************/
/*  Wrap each full-width table in a div.full-width-table-inner-wrapper, and
    also move the .collapse class (if any) from the outer wrapper to the table
    (for consistency).
 */
addContentInjectHandler(GW.contentInjectHandlers.wrapFullWidthTables = (eventInfo) => {
    GWLog("wrapFullWidthTables", "rewrite.js", 1);

    wrapAll(".table-wrapper.width-full > table", "full-width-table-inner-wrapper", "DIV", eventInfo.container, false);

    //  Move ‘collapse’ class from wrappers to tables.
    eventInfo.container.querySelectorAll(".table-wrapper.width-full.collapse table").forEach(table => {
        table.closest(".table-wrapper").classList.remove("collapse");
        table.classList.add("collapse");
    });
}, "rewrite", (info) => info.fullWidthPossible);


/***********/
/* FIGURES */
/***********/

/*******************************/
/*  Wrap bare images in figures.
 */
addContentLoadHandler(GW.contentLoadHandlers.wrapImages = (eventInfo) => {
    GWLog("wrapImages", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("p > img:only-child").forEach(image => {
        unwrap(image.parentElement);
    });

    let exclusionSelector = ".footnote-back, td, th";
    wrapAll("img", (image) => {
        if (   image.classList.contains("figure-not")
        	|| image.closest(exclusionSelector))
            return;

        let figure = image.closest("figure");
        if (   figure
        	&& figure.querySelector("figcaption") != null)
            return;

        wrapElement(image, null, "FIGURE", true,
            [ "float-left", "float-right", "outline-not", "image-focus-not" ]);
    }, null, eventInfo.container);
}, "rewrite");

/*****************************************************************************/
/*  Sets, in CSS, the image dimensions that are specified in HTML.
    (This is to ensure no reflow.)
 */
addContentLoadHandler(GW.contentLoadHandlers.setImageDimensions = (eventInfo) => {
    GWLog("setImageDimensions", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("figure img[width][height]").forEach(image => {
		let width = image.getAttribute("width");
		let height = image.getAttribute("height");

    	image.style.aspectRatio = `${width} / ${height}`;

		if (eventInfo.contentType == "annotation")
			image.style.width = `${width}px`;
    });
}, "rewrite");

/********************************/
/*  Inject wrappers into figures.
 */
addContentLoadHandler(GW.contentLoadHandlers.wrapFigures = (eventInfo) => {
    GWLog("wrapFigures", "rewrite.js", 1);

	let mediaSelector = "img, audio, video";

    eventInfo.container.querySelectorAll("figure").forEach(figure => {
        let media = figure.querySelector(mediaSelector);
        let caption = figure.querySelector("figcaption");

        if (!(media && caption))
            return;

        //  Create an inner wrapper for the figure contents.
        let innerWrapper = newElement("SPAN", { "class": "figure-inner-wrapper" });
        figure.appendChild(innerWrapper);

        //  Re-insert the (possibly wrapped) media into the figure.
        figure.querySelectorAll(mediaSelector).forEach(mediaElement => {
        	let mediaBlock = mediaElement.closest(".image-wrapper") || mediaElement;
        	innerWrapper.appendChild(mediaBlock);
        });

        //  Wrap the caption in the wrapper span.
        let captionWrapper = newElement("SPAN", { "class": "caption-wrapper" });
        captionWrapper.appendChild(caption);

        //  Re-insert the wrapped caption into the figure.
        innerWrapper.appendChild(captionWrapper);

        //	Tag the figure with the first (or only) media element’s float class.
        if (media.classList.contains("float-left"))
            media.closest("figure").classList.add("float-left");
        if (media.classList.contains("float-right"))
            media.closest("figure").classList.add("float-right");
    });
}, "rewrite");

/********************************************************************/
/*  Designate full-width figures as such (with a ‘width-full’ class).
 */
addContentInjectHandler(GW.contentInjectHandlers.markFullWidthFigures = (eventInfo) => {
    GWLog("markFullWidthFigures", "rewrite.js", 1);

    let fullWidthClass = "width-full";

    let allFullWidthMedia = eventInfo.container.querySelectorAll(`img.${fullWidthClass}, video.${fullWidthClass}`);
    allFullWidthMedia.forEach(fullWidthMedia => {
        fullWidthMedia.closest("figure").classList.toggle(fullWidthClass, true);
    });

    /*  Add ‘load’ listener for lazy-loaded media (as it might cause re-layout
        of e.g. sidenotes). Do this only after page layout is complete, to avoid
        spurious re-layout at initial page load.
     */
    doWhenPageLayoutComplete(() => {
        allFullWidthMedia.forEach(fullWidthMedia => {
            fullWidthMedia.addEventListener("load", (event) => {
                GW.notificationCenter.fireEvent("Rewrite.fullWidthMediaDidLoad", {
                    mediaElement: fullWidthMedia
                });
            });
        });
    });
}, "rewrite", (info) => info.fullWidthPossible);

/*****************************************************************/
/*  Allow for floated figures at the start of annotation abstracts
	(only on sufficiently wide viewports).
 */
addContentLoadHandler(GW.contentLoadHandlers.relocateThumbnailInAnnotation = (eventInfo) => {
    GWLog("relocateThumbnailInAnnotation", "rewrite.js", 1);

	if (GW.mediaQueries.mobileWidth.matches)
		return;

	let annotationAbstract = eventInfo.container.querySelector(".annotation-abstract");
	if (   annotationAbstract == null
		|| annotationAbstract.tagName == "BLOCKQUOTE")
		return;

	let container = annotationAbstract.closest(".annotation");
	if (   container == null
		|| container == annotationAbstract)
		return;

	let initialFigure = annotationAbstract.querySelector(".annotation-abstract > figure.float-right:first-child");
	if (initialFigure == null) {
		let pageThumbnailImage = annotationAbstract.querySelector("img.page-thumbnail");
		if (pageThumbnailImage)
			initialFigure = pageThumbnailImage.closest("figure");
	}
    if (initialFigure)
		container.insertBefore(initialFigure, container.firstElementChild);
}, "rewrite");


/***************/
/* CODE BLOCKS */
/***************/

/***********************************************************/
/*  Wrap each pre.width-full in a div.width-full and a
    div.full-width-code-block-wrapper (for layout purposes).
 */
addContentInjectHandler(GW.contentInjectHandlers.wrapFullWidthPreBlocks = (eventInfo) => {
    GWLog("wrapFullWidthPreBlocks", "rewrite.js", 1);

    wrapAll("pre.width-full", (fullWidthPre) => {
        wrapElement(fullWidthPre, "width-full", "DIV", true);
        wrapElement(fullWidthPre.parentElement, "full-width-code-block-wrapper", "DIV", false);
    }, null, eventInfo.container);
}, "rewrite", (info) => info.fullWidthPossible);


/***********/
/* COLUMNS */
/***********/

/*****************************************/
/*  Disable columns if only one list item.
 */
addContentLoadHandler(GW.contentLoadHandlers.disableSingleItemColumnBlocks = (eventInfo) => {
    GWLog("disableSingleItemColumnBlocks", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".columns > ul").forEach(columnList => {
        if (columnList.children.length == 1)
            columnList.parentElement.classList.remove("columns");
    });
}, "rewrite");


/****************/
/* MARGIN NOTES */
/****************/

/*************************************************************/
/*  Wrap the contents of all margin notes in an inner wrapper.
 */
addContentLoadHandler(GW.contentLoadHandlers.wrapMarginNotes = (eventInfo) => {
    GWLog("wrapFullWidthPreBlocks", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".marginnote").forEach(marginnote => {
        let innerWrapper = newElement("SPAN", { "class": "marginnote-inner-wrapper" });
        innerWrapper.append(...marginnote.childNodes);
        marginnote.append(innerWrapper);
    });
}, "rewrite");


/**************/
/* TYPOGRAPHY */
/**************/

/******************************************************************************/
/*  Remove extraneous whitespace-only text nodes from between the element parts
    of a .cite (citation element).
 */
addContentLoadHandler(GW.contentLoadHandlers.removeExtraneousWhitespaceFromCitations = (eventInfo) => {
    GWLog("removeExtraneousWhitespaceFromCitations", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".cite").forEach(citation => {
        Array.from(citation.children).forEach(citationPart => {
            if (   citationPart.nextSibling
                && citationPart.nextSibling.nodeType == Node.TEXT_NODE
                && isNodeEmpty(citationPart.nextSibling))
                citationPart.nextSibling.remove();
        });
    });
}, "rewrite");

/******************************************************************/
/*  Configure Hyphenopoly.

    Requires Hyphenopoly_Loader.js to be loaded prior to this file.
 */
Hyphenopoly.config({
    require: {
        "en-us": "FORCEHYPHENOPOLY"
    },
    setup: {
        hide: "none",
        keepAlive: true,
        safeCopy: false
    }
});

/******************************************************************/
/*  Hyphenate with Hyphenopoly.

    Requires Hyphenopoly_Loader.js to be loaded prior to this file.
 */
function hyphenate(eventInfo) {
    GWLog("hyphenate", "rewrite.js", 1);

    if (!(Hyphenopoly.hyphenators))
        return;

    if (GW.isX11())
        return;

    let doHyphenation = (selector) => {
        Hyphenopoly.hyphenators.HTML.then((hyphenate) => {
            eventInfo.container.querySelectorAll(selector).forEach(block => {
                hyphenate(block);
            });
        });
    };

    if (   GW.isMobile()
        || eventInfo.document != document) {
        doHyphenation((eventInfo.document == document) ? ".markdownBody p" : "p");
    } else {
        doHyphenation(".sidenote p, .abstract blockquote p");
    }
}

/*******************************************************************************/
/*  Set line height of element to its computed line height, rounded to the
    nearest pixel.

    (Actually, the value of the CSS `line-height` property is set to a unitless
     value computed to result in the rendered line height being an integer pixel
     value.)

    Returns the CSS property value as a string.

    If the `returnOnly` argument is TRUE, then the property is not actually set;
    the value to be set is only returned.
 */
function rectifyLineHeight(elementOrSelector, container = document.body, returnOnly = false) {
    if (typeof elementOrSelector == "string")
        elementOrSelector = container.querySelector(elementOrSelector);

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
/*  Rectify line heights of elements matching a given set of selectors.
 */
function rectifyLineHeights(eventInfo) {
    GWLog("rectifyLineHeights", "rewrite.js", 1);

    /*  Note: these selectors should be “all block elements on which the font
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

    /*  On the first pass, compute the adjusted values for line-height of all
        affected elements; on the second pass, actually set the values. (This is
        done to prevent cascades of deviations from ideal [fractional] values
        due to rounding, in nested line-height-adjusted elements.)
     */
    let elements = [ ];
    eventInfo.container.querySelectorAll(selectors.join(", ")).forEach(element => {
        if (element.style.lineHeight > "")
            return;
        let lineHeight = rectifyLineHeight(element, eventInfo.container, true);
        elements.push({
            element:    element,
            lineHeight: lineHeight
        });
    });
    elements.forEach(e => {
        if (   null != e.element
            && null != e.lineHeight)
            e.element.style.lineHeight = e.lineHeight;
    });
}

/**********************************************/
/*  Add content inject handlers for typography.
 */
addContentInjectHandler(GW.contentInjectHandlers.rectifyTypography = (info) => {
    hyphenate(info);
    rectifyLineHeights(info);
}, "rewrite");

/****************************************/
/*  Remove soft hyphens from copied text.
 */
addCopyProcessor((event, selection) => {
    //  Passing `true` for the third argument also removes zero-width spaces.
    Typography.processElement(selection, Typography.replacementTypes.SOFTHYPHENS, true);

    return true;
});

/*****************************************************************************/
/*  Makes it so that copying an author-date citation (e.g. `Foo et al 2001`)
    interact properly with copy-paste when rendered with pseudo-element ellipses
    (`Foo...2001`).
 */
addCopyProcessor((event, selection) => {
    /*  Set `display` of all `span.cite-joiner` to `initial` (overriding the
        default of `none`) so that their contents are included in the
        content properties of the selection).
     */
    selection.querySelectorAll(".cite-joiner").forEach(citeJoiner => {
        citeJoiner.style.display = "initial";
        citeJoiner.innerHTML = ` ${citeJoiner.innerHTML} `;
    });

    return true;
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

GW.notificationCenter.addHandlerForEvent("GW.pageLayoutWillComplete", (info) => {
    createFullWidthBlockLayoutStyles();
});

/************************************/
/*  Set margins of full-width blocks.
 */
addContentInjectHandler(GW.contentInjectHandlers.setMarginsOnFullWidthBlocks = (eventInfo) => {
    GWLog("setMarginsOnFullWidthBlocks", "rewrite.js", 1);

    //  Get all full-width blocks in the given document.
    let allFullWidthBlocks = eventInfo.container.querySelectorAll("div.width-full, figure.width-full");

    let removeFullWidthBlockMargins = () => {
        allFullWidthBlocks.forEach(fullWidthBlock => {
            fullWidthBlock.style.marginLeft = "";
            fullWidthBlock.style.marginRight = "";
        });
    };

    if (eventInfo.fullWidthPossible == false) {
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
}, ">rewrite");


/***************/
/* ANNOTATIONS */
/***************/

/******************************************************************************/
/*	Make the page thumbnail (or just the first figure, if no thumbnail present)
	in an annotation load eagerly instead of lazily.
 */
addContentLoadHandler(GW.contentLoadHandlers.setEagerLoadingForAnnotationImages = (eventInfo) => {
    GWLog("setEagerLoadingForAnnotationImages", "rewrite.js", 1);

	let firstImage = (   eventInfo.container.querySelector(".page-thumbnail")
					  || eventInfo.container.querySelector("figure img"))
	if (firstImage) {
		firstImage.loading = "eager";
		firstImage.decoding = "sync";
	}
}, "rewrite", (info) => (info.contentType == "annotation"));

/*****************************************************************/
/*	Partial annotations, defined inline (in directories and such).
 */
addContentLoadHandler(GW.contentLoadHandlers.rewritePartialAnnotations = (eventInfo) => {
    GWLog("rewritePartialAnnotations", "rewrite.js", 1);

	eventInfo.container.querySelectorAll(".annotation-partial").forEach(partialAnnotation => {
		if (partialAnnotation.firstElementChild.classList.contains("data-field"))
			return;

		//	Designate reference link, for annotations.js to identify it.
		let referenceLink = partialAnnotation.querySelector("a");
		referenceLink.classList.add("link-annotated-partial");

		//	Load data into Annotations.
		Annotations.cacheAPIResponseForIdentifier(newDocument(partialAnnotation), 
												  Annotations.targetIdentifier(referenceLink));

		//	Replace reference block contents with synthetic include-link.
		partialAnnotation.replaceChildren(synthesizeIncludeLink(referenceLink, {
			"class": "include-annotation include-replace-container link-annotated-partial",
			"data-template-fields": "annotationClassSuffix:$",
			"data-annotation-class-suffix": "-partial"
		}));

		//	Fire GW.contentDidLoadEvent (to trigger transclude).
		GW.notificationCenter.fireEvent("GW.contentDidLoad", {
			source: "rewritePartialAnnotations",
			container: partialAnnotation,
			document: eventInfo.document
		});
	});
}, "rewrite");

/***************************************************************************/
/*	Because annotations transclude aux-links, we make the aux-links links in
	the metadata line of annotations scroll down to the appended aux-links
	blocks.
 */
addContentInjectHandler(GW.contentInjectHandlers.rewriteAuxLinksLinksInTranscludedAnnotations = (eventInfo) => {
    GWLog("rewriteAuxLinksLinksInTranscludedAnnotations", "rewrite.js", 1);

	let annotation = eventInfo.container.querySelector(".annotation");
	if (annotation == null)
		return;

	let inPopFrame = (annotation.closest(".popframe-body") != null);

	annotation.querySelectorAll(".data-field.aux-links a.aux-links").forEach(auxLinksLink => {
		let auxLinksLinkType = AuxLinks.auxLinksLinkType(auxLinksLink);
		let includedAuxLinksBlock = annotation.querySelector(`.${auxLinksLinkType}-append`);
		if (includedAuxLinksBlock) {
			auxLinksLink.onclick = () => { return false; };
			auxLinksLink.addActivateEvent((event) => {
				if (inPopFrame) {
					Extracts.popFrameProvider.scrollElementIntoViewInPopFrame(includedAuxLinksBlock);
				} else {
					revealElement(includedAuxLinksBlock, true);
				}
				return false;
			});
		}
	});
}, "eventListeners", (info) => (info.contentType == "annotation"));

/*******************************************************************************/
/*  Apply various typographic fixes (educate quotes, inject <wbr> elements after
    certain problematic characters, etc.).

    Requires typography.js to be loaded prior to this file.
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyTypographyInAnnotation = (eventInfo) => {
    GWLog("rectifyTypographyInAnnotation", "rewrite.js", 1);

    Typography.processElement(eventInfo.container,
        (  Typography.replacementTypes.QUOTES
         | Typography.replacementTypes.WORDBREAKS
         | Typography.replacementTypes.ELLIPSES),
        true);

    //  Educate quotes in image alt-text.
    eventInfo.container.querySelectorAll("img").forEach(image => {
        image.alt = Typography.processString(image.alt, Typography.replacementTypes.QUOTES);
    });
}, "rewrite", (info) => (info.contentType == "annotation"));

/******************************************************************************/
/*  Bind mouse hover events to, when hovering over an annotated link, highlight
    that annotation (as viewed in a tags directory, for instance).
 */
addContentInjectHandler(GW.contentInjectHandlers.bindSectionHighlightEventsToAnnotatedLinks = (eventInfo) => {
    GWLog("bindSectionHighlightEventsToAnnotatedLinks", "rewrite.js", 1);

    Annotations.allAnnotatedLinksInContainer(eventInfo.container).forEach(annotatedLink => {
        //  Unbind existing events, if any.
        if (annotatedLink.annotatedLinkMouseEnter)
            annotatedLink.removeEventListener("mouseenter", annotatedLink.annotatedLinkMouseEnter);
        if (annotatedLink.annotatedLinkMouseLeave)
            annotatedLink.removeEventListener("mouseleave", annotatedLink.annotatedLinkMouseLeave);

        //  Bind events.
        let escapedLinkURL = CSS.escape(decodeURIComponent(annotatedLink.href));
        let targetAnalogueInLinkBibliography = document.querySelector(`a[id^='link-bibliography'][href='${escapedLinkURL}']`);
        if (   targetAnalogueInLinkBibliography
            && targetAnalogueInLinkBibliography != annotatedLink) {
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
}, "eventListeners");


/*********************/
/* LINK BIBLIOGRAPHY */
/*********************/

/*********************************************************************/
/*	Remove the “Link Bibliography:” bold text when transcluding a link
	bibliography into a page’s Link Bibliography section.
 */
addContentInjectHandler(GW.contentInjectHandlers.removeSubheadingFromLinkBibliography = (eventInfo) => {
    GWLog("removeSubheadingFromLinkBibliography", "rewrite.js", 1);

	if (eventInfo.container.closest("section#link-bibliography")) {
		let subheading = eventInfo.container.querySelector("div#link-bibliography-link-footer-transclusion > p:first-child");
		if (subheading)
			subheading.remove();
	}
}, "rewrite", (info) => (info.source == "transclude"));

/*****************************************************************************/
/*	Apply a class to those link-bibs that should use the more compact styling.
 */
addContentInjectHandler(GW.contentInjectHandlers.applyLinkBibliographyStylingClass = (eventInfo) => {
    GWLog("applyLinkBibliographyStylingClass", "rewrite.js", 1);

	eventInfo.container.querySelectorAll(".link-bibliography-list").forEach(linkBibList => {
		if (linkBibList.closest("li, .link-bibliography-append, .popframe-body.link-bibliography"))
			linkBibList.classList.add("link-bibliography-list-compact");
	});
}, "rewrite");


/*********************/
/* TABLE OF CONTENTS */
/*********************/

/***************************************************************************/
/*  Strip spurious <span> tags (unavoidably added by Pandoc) from TOC links.
 */
addContentLoadHandler(GW.contentLoadHandlers.stripTOCLinkSpans = (eventInfo) => {
    GWLog("stripTOCLinkSpans", "rewrite.js", 1);

    unwrapAll(".TOC li a > span:not([class])", eventInfo.container);
}, "rewrite");

/*******************************************************************************/
/*  Updates the page TOC with any sections within the given container that don’t
    already have TOC entries.
 */
//	Called by: updateMainPageTOC
//  Called by: includeContent (transclude.js)
function updatePageTOC(newContent, needsProcessing = false) {
    GWLog("updatePageTOC", "transclude.js", 2);

    let TOC = document.querySelector("#TOC");
    if (!TOC)
        return;

	//	Don’t nest TOC entries any deeper than this.
	let maxNestingDepth = 4;

    //  Find where to insert the new TOC entries.
    let parentSection = newContent.closest("section") ?? document.querySelector("#markdownBody");
    let nextSection = Array.from(parentSection.children).filter(child =>
    	   child.tagName == "SECTION"
    	&& child.compareDocumentPosition(newContent) == Node.DOCUMENT_POSITION_PRECEDING
    ).first;

    //  Any already-existing <section> should have a TOC entry.
    let parentTOCElement = parentSection.id == "markdownBody"
                           ? TOC
                           : TOC.querySelector(`#toc-${CSS.escape(parentSection.id)}`).parentElement;
    let followingTOCElement = nextSection
    						  ? parentTOCElement.querySelector(`#toc-${CSS.escape(nextSection.id)}`).parentElement
    						  : null;

    //  TOC entry insertion function, called recursively.
    function addToPageTOC(newContent, parentTOCElement, followingTOCElement) {
        let addedEntries = [ ];

        newContent.querySelectorAll("section").forEach(section => {
            /*  We may have already added this section in a recursive call from
                a previous section.
             */
            if (parentTOCElement.querySelector(`a[href$='#${fixedEncodeURIComponent(section.id)}']`) != null)
                return;

			/*	If this section is too deeply nested, do not add it.
			 */
			if (sectionLevel(section) > maxNestingDepth)
				return;

            //  Construct entry.
            let entry = newElement("LI");
            let entryText = section.id == "footnotes"
                            ? "Footnotes"
                            : section.firstElementChild.textContent;
            entry.innerHTML = `<a id='toc-${section.id}' href='#${fixedEncodeURIComponent(section.id)}'>${entryText}</a>`;

            //  Get or construct the <ul> element.
            let subList = Array.from(parentTOCElement.childNodes).find(child => child.tagName == "UL");
            if (!subList) {
                subList = newElement("UL");
                parentTOCElement.appendChild(subList);
            }

            subList.insertBefore(entry, followingTOCElement);
            addedEntries.push(entry);

            //  Recursive call, to added sections nested within this one.
            addToPageTOC(section, entry, null);
        });

        return addedEntries;
    }

    //  Add the new entries.
    let newEntries = addToPageTOC(newContent, parentTOCElement, followingTOCElement);

    if (needsProcessing) {
        //  Process the new entries to activate pop-frame spawning.
        newEntries.forEach(Extracts.addTargetsWithin);
    }
}

/****************************************************************************/
/*  Update main page TOC within any sections within the initially loaded page
    that don’t already have TOC entries.
 */
addContentLoadHandler(GW.contentLoadHandlers.updateMainPageTOC = (eventInfo) => {
    GWLog("updateMainPageTOC", "rewrite.js", 1);

    updatePageTOC(eventInfo.container.querySelector("#markdownBody"));
}, "rewrite", (info) => (info.container == document.body));

/**********************************************************/
/*	Relocate and clean up TOC on tag directory index pages.
 */
addContentLoadHandler(GW.contentLoadHandlers.rewriteDirectoryIndexTOC = (eventInfo) => {
    GWLog("rewriteDirectoryIndexTOC", "rewrite.js", 1);

	let TOC = eventInfo.container.querySelector("#TOC");
	let seeAlsoSection = eventInfo.container.querySelector("#see-also");

	if (   TOC == null
		|| seeAlsoSection == null)
		return;

	/*	Place the TOC after the “See Also” section (which also places it after
		the page abstract, if such exists, because that comes before the
		“See Also” section).
	 */
	seeAlsoSection.parentElement.insertBefore(TOC, seeAlsoSection.nextElementSibling);

	//	The “See Also” section no longer needs a TOC entry.
	TOC.querySelector("#toc-see-also").closest("li").remove();

	/*	If “Links” is the only remaining section, then it does not itself need
		a TOC entry; shift its children up one TOC level.
	 */
	let linksTOCEntry = TOC.querySelector("#toc-links");
	if (isOnlyChild(linksTOCEntry.closest("li"))) {
		let outerTOCList = TOC.querySelector("ul");
		let innerTOCList = TOC.querySelector("#toc-links + ul");

		TOC.insertBefore(innerTOCList, null);
		outerTOCList.remove();

		//	Mark with special class, for styling purposes.
		TOC.classList.add("TOC-links-only");
	}
}, "rewrite", (info) => (   info.container == document.body
						 && info.loadLocation
						 && /^\/docs\/.+\/index$/.test(info.loadLocation.pathname)));

/**************************************************************************/
/*	If the table of contents has but one entry (or none at all), remove it.
 */
addContentLoadHandler(GW.contentLoadHandlers.removeTOCIfSingleEntry = (eventInfo) => {
    GWLog("removeTOCIfSingleEntry", "rewrite.js", 1);

	let TOC = eventInfo.container.querySelector(".TOC");
	if (TOC == null)
		return;

	let numEntries = TOC.querySelectorAll("li").length;
	if (   (   TOC.id == "TOC"
		    && numEntries <= 1)
		|| numEntries == 0)
		TOC.remove();
}, "rewrite");


/*************/
/* FOOTNOTES */
/*************/

/*****************************************************/
/*  Inject self-link for the footnotes section itself.
 */
addContentLoadHandler(GW.contentLoadHandlers.injectFootnoteSectionSelfLink = (eventInfo) => {
    GWLog("injectFootnoteSectionSelfLink", "rewrite.js", 1);

    let footnotesSection = eventInfo.container.querySelector("#footnotes");
    if (!footnotesSection)
        return;

    let footnotesSectionSelfLink = newElement("A", {
        "class": "section-self-link",
        "href": "#footnotes",
        "title": "Link to section: § ‘Footnotes’"
    });

    footnotesSection.insertBefore(footnotesSectionSelfLink, footnotesSection.firstElementChild.nextElementSibling);

    //  Highlight on hover.
    footnotesSectionSelfLink.addEventListener("mouseenter", (event) => {
        footnotesSectionSelfLink.previousElementSibling.classList.toggle("highlighted", true);
    });
    footnotesSectionSelfLink.addEventListener("mouseleave", (event) => {
        footnotesSectionSelfLink.previousElementSibling.classList.toggle("highlighted", false);
    });
}, "rewrite");

/*****************************************/
/*  Add footnote class to footnote blocks.
 */
addContentLoadHandler(GW.contentLoadHandlers.addFootnoteClassToFootnotes = (eventInfo) => {
    GWLog("addFootnoteClassToFootnotes", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("#footnotes > ol > li").forEach(footnote => {
        footnote.classList.add("footnote");
    });
}, "rewrite");

/******************************/
/*  Inject footnote self-links.
 */
addContentLoadHandler(GW.contentLoadHandlers.injectFootnoteSelfLinks = (eventInfo) => {
    GWLog("injectFootnoteSelfLinks", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("#footnotes > ol > li").forEach(footnote => {
        if (footnote.querySelector(".footnote-self-link"))
            return;

        let footnoteNumber = footnote.id.slice(2);
        footnote.insertAdjacentHTML("afterbegin",
            `<a
                href="#fn${footnoteNumber}"
                title="Link to footnote ${footnoteNumber}"
                class="footnote-self-link"
                    >&nbsp;</a>`);
    });
}, "rewrite");

/*****************************************************************/
/*  Rewrite footnote back-to-citation links (generated by Pandoc).
 */
addContentLoadHandler(GW.contentLoadHandlers.rewriteFootnoteBackLinks = (eventInfo) => {
    GWLog("rewriteFootnoteBackLinks", "rewrite.js", 1);

    /*  Base font size (1rem) is 20px at this time, making a good default.
        That value might change later, but this’ll still be a fine default;
        the width/height get adjusted below, anyway, so no big deal if the
        default is not the final value. We mostly care about having _a_ value
        for the width/height for page load performance reasons.
     */
    let defaultSize = 20;
    eventInfo.container.querySelectorAll("#footnotes > ol > li").forEach(footnote => {
        let backlink = footnote.querySelector(".footnote-back");
        if (backlink.querySelector("img"))
            return;

        backlink.textContent = "";
        backlink.appendChild(newElement("IMG", {
            width: defaultSize,
            height: defaultSize,
            alt: "↩ Right arrow curving left [footnote return link] arrow",
            src: "/static/img/icons/arrow-hook-left.svg"
        }));
    });
}, "rewrite");

/******************************************************************************/
/*  Set size properly, after setting default value in rewriteFootnoteBackLinks.
 */
addContentInjectHandler(GW.contentInjectHandlers.rectifyFootnoteBackLinkArrowSize = (eventInfo) => {
    GWLog("rectifyFootnoteBackLinkArrowSize", "rewrite.js", 1);

    let footnotesList = eventInfo.container.querySelector("#footnotes > ol");
    if (!footnotesList)
        return;

    requestIdleCallback(() => {
        let size = parseInt(getComputedStyle(footnotesList).fontSize);
        if (!size)
            return;

        eventInfo.container.querySelectorAll("#footnotes > ol > li").forEach(footnote => {
            let arrow = footnote.querySelector(".footnote-back img");
            if (!arrow)
                return;

            arrow.width = size;
            arrow.height = size;
        });
    });
}, "rewrite");

/***************************************************************************/
/*  Bind mouse hover events to, when hovering over a citation, highlight all
    {side|foot}notes associated with that citation.
 */
addContentInjectHandler(GW.contentInjectHandlers.bindHighlightEventsToFootnoteSelfLinks = (eventInfo) => {
    GWLog("bindNoteHighlightEventsToCitations", "rewrite.js", 1);

    let allCitations = eventInfo.container.querySelectorAll(".footnote-ref");

    let bindEventsToCitation = (citation) => {
        //  Unbind existing events, if any.
        if (citation.citationMouseEnter)
            citation.removeEventListener("mouseenter", citation.citationMouseEnter);
        if (citation.citationMouseLeave)
            citation.removeEventListener("mouseleave", citation.citationMouseLeave);

        //  Bind events.
        let notesForCitation = Notes.allNotesForCitation(citation);
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
    };

    //  Bind events.
    allCitations.forEach(bindEventsToCitation);

	if (allCitations.length > 0) {
		//  Add handler to re-bind events if more notes are injected.
		GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
			allCitations.forEach(bindEventsToCitation);
		}, { condition: (info) => (   info.document == document
								   || info.document == eventInfo.document)
		});
    }
}, "eventListeners");

/******************************************/
/*  Highlight footnote self-links on hover.
 */
addContentInjectHandler(GW.contentInjectHandlers.bindNoteHighlightEventsToCitations = (eventInfo) => {
    GWLog("bindHighlightEventsToFootnoteSelfLinks", "rewrite.js", 1);

    //  Highlight footnote on hover over self-link.
    eventInfo.container.querySelectorAll(".footnote-self-link").forEach(footnoteSelfLink => {
        footnoteSelfLink.addEventListener("mouseenter", (event) => {
            footnoteSelfLink.parentElement.classList.toggle("highlighted", true);
        });
        footnoteSelfLink.addEventListener("mouseleave", (event) => {
            footnoteSelfLink.parentElement.classList.toggle("highlighted", false);
        });
    });
}, "eventListeners");


/*********/
/* LINKS */
/*********/

/********************************************************/
/*	Return the location (URL) associated with a document.
	(Document|DocumentFragment) => URL
 */
function baseLocationForDocument(doc) {
	if (doc == document) {
		return new URL(location.href);
	} else if (   doc.body instanceof Element
			   && doc.body.classList.contains("popframe-body")) {
		let spawningTarget = (Extracts.popFrameProvider == Popups
							  ? doc.body.popup.spawningTarget
							  : doc.body.popin.spawningTarget);
		return new URL(spawningTarget.href);
	} else if (doc.baseLocation) {
		return new URL(doc.baseLocation.href);
	} else {
		return null;
	}
}

/**********************************************************************/
/*  Qualify anchorlinks in loaded content by rewriting their `pathname`
    attributes.
 */
addContentInjectHandler(GW.contentInjectHandlers.qualifyAnchorLinks = (eventInfo) => {
    GWLog("qualifyAnchorLinks", "rewrite.js", 1);

	let baseLocation = baseLocationForDocument(eventInfo.document);
	if (baseLocation == null)
		return;

	let loadLocation = (eventInfo.loadLocation ?? baseLocation);

    eventInfo.container.querySelectorAll("a[href]").forEach(link => {
        if (   (   link.getAttribute("href").startsWith("#")
                || link.pathname == loadLocation.pathname)
                // if initial base page load
            && (   eventInfo.container == document.body
            	// if the link refers to an element also in the loaded content
                || eventInfo.container.querySelector(selectorFromHash(link.hash)) != null
                // if the link refers to the loaded content container itself
                || (   eventInfo.container instanceof Element
                    && eventInfo.container == eventInfo.container.closest(selectorFromHash(link.hash)))
                || (   eventInfo.document.querySelector("#page-metadata") != null
			        		// if we’re transcluding a citation (because we merge footnotes)
                	&& (   (   eventInfo.source == "transclude"
							&& link.classList.contains("footnote-ref"))
							// if we’re merging a footnote for transcluded content
						|| (   eventInfo.source == "transclude.footnotes"
							&& link.classList.contains("footnote-back")))))) {
            link.pathname = baseLocation.pathname;
        } else if (link.getAttribute("href").startsWith("#")) {
            link.pathname = loadLocation.pathname;
        }
    });
}, "rewrite");

/********************************************************************/
/*  Designate self-links (a.k.a. anchorlinks) and local links (a.k.a.
    within-site links) as such, via CSS classes.
 */
addContentInjectHandler(GW.contentInjectHandlers.addSpecialLinkClasses = (eventInfo) => {
    GWLog("addSpecialLinkClasses", "rewrite.js", 1);

	let baseLocation = baseLocationForDocument(eventInfo.document);
	if (baseLocation == null)
		return;

    eventInfo.container.querySelectorAll(".markdownBody a[href]").forEach(link => {
        if (   link.hostname != location.hostname
            || link.closest("h1, h2, h3, h4, h5, h6")
            || link.closest(".section-self-link, .footnote-ref, .footnote-back, .footnote-self-link, .sidenote-self-link"))
            return;

        if (   link.pathname == baseLocation.pathname
                // if initial base page load
            && (   eventInfo.container == document.body
            	// if the link refers to an element also in the loaded content
                || eventInfo.container.querySelector(selectorFromHash(link.hash)) != null
               // if the link refers to the loaded content container itself
                || (   eventInfo.container instanceof Element
                    && eventInfo.container == eventInfo.container.closest(selectorFromHash(link.hash))))) {
            link.swapClasses([ "link-self", "link-page" ], 0);
        } else if (link.pathname.slice(1).match(/[\.]/) == null) {
            link.swapClasses([ "link-self", "link-page" ], 1);
        }
    });
}, "rewrite");

/************************************************************************/
/*  Assign proper link icons to self-links (directional or otherwise) and
    local links.
 */
addContentInjectHandler(GW.contentInjectHandlers.designateSpecialLinkIcons = (eventInfo) => {
    GWLog("designateSpecialLinkIcons", "rewrite.js", 1);

	//	Internal links on the home page need no decoration.
	if (   eventInfo.container == document.body
		&& eventInfo.container.classList.contains("index"))
		return;

    //  Self-links (anchorlinks to the current page).
    eventInfo.container.querySelectorAll(".link-self:not(.icon-not)").forEach(link => {
        link.dataset.linkIconType = "text";
        link.dataset.linkIcon = "\u{00B6}"; // ¶

        /*  Directional navigation links on self-links: for each self-link like
            “see [later](#later-identifier)”, find the linked identifier,
            whether it’s before or after, and if it is before/previously,
            annotate the self-link with ‘↑’ and if after/later, ‘↓’. This helps
            the reader know if it’s a backwards link to an identifier already
            read, or an unread identifier.
         */
        let target = eventInfo.container.querySelector(selectorFromHash(link.hash));
        if (!target)
            return;

        link.dataset.linkIconType = "svg";
        link.dataset.linkIcon =
            (link.compareDocumentPosition(target) == Node.DOCUMENT_POSITION_FOLLOWING
             ? "arrow-down"
             : "arrow-up");
    });

    //  Local links (to other pages on the site).
    eventInfo.container.querySelectorAll(".link-page:not(.icon-not)").forEach(link => {
        if (link.dataset.linkIcon)
            return;

        link.dataset.linkIconType = "text";
        link.dataset.linkIcon = "\u{1D50A}"; // 𝔊
    });
}, "rewrite");


/*********/
/* MISC. */
/*********/

/***************************************************************/
/*	Mark the link lists on the index page, for styling purposes.
 */
addContentLoadHandler(GW.contentLoadHandlers.designateIndexPageSectionLinkLists = (eventInfo) => {
    GWLog("designateIndexPageSectionLinkLists", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("section > ul").forEach(sectionLinkList => {
		sectionLinkList.classList.add("section-link-list");
	});
}, "rewrite", (info) => (   info.container == document.body 
						 && (   info.loadLocation.pathname == "/"
						 	 || info.loadLocation.pathname == "/index")));

/***************************************************************************/
/*  Clean up image alt-text. (Shouldn’t matter, because all image URLs work,
    right? Yeah, right...)
 */
addContentLoadHandler(GW.contentLoadHandlers.cleanUpImageAltText = (eventInfo) => {
    GWLog("cleanUpImageAltText", "rewrite.js", 1);

    /*  If an image has no alt text, use the value of the ‘title’ attribute,
        if present; otherwise, a default string (“Image”).
     */
    eventInfo.container.querySelectorAll("img:not([alt])").forEach(image => {
        image.alt = (image.title || "Image");
    });

    //  URL-encode ‘%’ signs in image alt text.
    eventInfo.container.querySelectorAll("img[alt]").forEach(image => {
        image.alt = decodeURIComponent(image.alt.replace(/%(?![A-Fa-f0-9]{2})/g, "%25"));
    });
}, "rewrite");

/************************************************************************/
/*  Prevent line breaks immediately before citations (which “orphans” the
    citation on the next line, and looks ugly).
 */
addContentLoadHandler(GW.contentLoadHandlers.noBreakForCitations = (eventInfo) => {
    GWLog("noBreakForCitations", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".footnote-ref").forEach(citation => {
        citation.insertAdjacentHTML("beforebegin", "&NoBreak;");
        let textNode = citation.querySelector("sup").firstTextNode;
        textNode.textContent = "\u{2060}" + textNode.textContent;
    });
}, "rewrite");


/*************/
/* DROP CAPS */
/*************/

/*******************************************************/
/*  Apply classes to blocks that should have a drop cap.
 */
addContentLoadHandler(GW.contentLoadHandlers.applyDropCapsClasses = (eventInfo) => {
    GWLog("applyDropCapsClasses", "rewrite.js", 1);

    //  Add ‘drop-cap-’ class to requisite blocks.
    let dropCapBlocksSelector = [
        ".markdownBody > p:first-child",
        ".markdownBody > section:first-of-type > p:nth-child(2)",
        ".markdownBody > .epigraph:first-child + p",
        ".markdownBody > section:first-of-type > .epigraph:nth-child(2) + p",
        ".markdownBody .abstract:not(.scrape-abstract-not) + p"
    ].join(", ");
    let dropCapClass = Array.from(eventInfo.container.classList).find(cssClass => cssClass.startsWith("drop-caps-"));
    if (dropCapClass)
        dropCapClass = dropCapClass.replace("-caps-", "-cap-");

    eventInfo.container.querySelectorAll(dropCapBlocksSelector).forEach(dropCapBlock => {
        /*  Drop cap class could be set globally, or overridden by a .abstract;
            the latter could be `drop-cap-not` (which nullifies any page-global
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
            && dropCapClass != "drop-cap-not")
            dropCapBlock.classList.add(dropCapClass);
    });
}, "rewrite", (info) => (info.container == document.body));

addContentInjectHandler(GW.contentInjectHandlers.preventDropCapsOverlap = (eventInfo) => {
    GWLog("preventDropCapsOverlap", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("[class*='drop-cap-']").forEach(dropCapBlock => {
		if (dropCapBlock.nextElementSibling) {
			if (   dropCapBlock.nextElementSibling.classList.containsAnyOf([ "columns" ])
				|| [ "OL", "UL" ].includes(dropCapBlock.nextElementSibling.tagName)
				|| [ "0", "0px" ].includes(getComputedStyle(dropCapBlock.nextElementSibling).borderWidth) == false)
				dropCapBlock.classList.add("overlap-not");
		}
	});
}, "rewrite", (info) => (info.document == document))

/********/
/* MATH */
/********/

/*****************************************************************************/
/*  Makes it so that copying a rendered equation or other math element copies
    the LaTeX source, instead of the useless gibberish that is the contents of
    the text nodes of the HTML representation of the equation.
 */
addCopyProcessor((event, selection) => {
    if (event.target.closest(".mjx-math")) {
        selection.replaceChildren(event.target.closest(".mjx-math").getAttribute("aria-label"));

        return false;
    }

    selection.querySelectorAll(".mjx-chtml").forEach(mathBlock => {
        mathBlock.innerHTML = mathBlock.querySelector(".mjx-math").getAttribute("aria-label");
    });

    return true;
});

/******************************************************************************/
/*  Makes double-clicking on a math element select the entire math element.
    (This actually makes no difference to the behavior of the copy listener
     [see the `addCopyProcessor` call above], which copies the entire LaTeX
     source of the full equation no matter how much of said equation is selected
     when the copy command is sent; however, it ensures that the UI communicates
     the actual behavior in a more accurate and understandable way.)
 */
addContentInjectHandler(GW.contentInjectHandlers.addDoubleClickListenersToMathBlocks = (eventInfo) => {
    GWLog("addDoubleClickListenersToMathBlocks", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".mjpage").forEach(mathBlock => {
        mathBlock.addEventListener("dblclick", (event) => {
            document.getSelection().selectAllChildren(mathBlock.querySelector(".mjx-chtml"));
        });
        mathBlock.title = mathBlock.classList.contains("mjpage__block")
                          ? "Double-click to select equation, then copy, to get LaTeX source (or, just click the Copy button in the top-right of the equation area)"
                          : "Double-click to select equation; copy to get LaTeX source";
    });
}, "eventListeners");

/****************************************************************/
/*  Add block buttons (copy) to block (not inline) math elements.
 */
addContentLoadHandler(GW.contentLoadHandlers.addBlockButtonsToMathBlocks = (eventInfo) => {
    GWLog("addBlockButtonsToMathBlocks", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".mjpage__block").forEach(mathBlock => {
        //  Inject button bar.
        mathBlock.insertAdjacentHTML("beforeend",
              `<span class="block-button-bar">`
            + `<button type="button" class="copy" tabindex="-1" title="Copy LaTeX source of this equation to clipboard">`
            + `<img src="/static/img/icons/copy.svg">`
            + `</button>`
            + `<span class="scratchpad"></span>`
            + `</span>`);
    });
}, "rewrite");

/************************************************/
/*	Activate copy buttons of math block elements.
 */
addContentInjectHandler(GW.contentInjectHandlers.activateMathBlockButtons = (eventInfo) => {
    GWLog("activateMathBlockButtons", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".mjpage__block").forEach(mathBlock => {
		//  Copy button (copies LaTeX source).
		let latexSource = mathBlock.querySelector(".mjx-math").getAttribute("aria-label");
		let scratchpad = mathBlock.querySelector(".scratchpad");
		mathBlock.querySelector("button.copy").addActivateEvent((event) => {
			GWLog("mathBlockCopyButtonClicked", "rewrite.js", 3);

			//  Perform copy operation.
			scratchpad.innerText = latexSource;
			selectElementContents(scratchpad);
			document.execCommand("copy");
			scratchpad.innerText = "";

			//  Flash math block, for visual feedback of copy operation.
			mathBlock.classList.add("flash");
			setTimeout(() => { mathBlock.classList.remove("flash"); }, 150);
		});
    });
}, "eventListeners");


/********************/
/* BACK TO TOP LINK */
/********************/

/***************************************************************************/
/*  On mobile, update the scroll position indicator in the back-to-top link.

    Called by the ‘updateBackToTopLinkScrollListener’ scroll listener.
 */
function updateBackToTopLinkScrollPositionIndicator(event) {
    GWLog("updateBackToTopLinkScrollPositionIndicator", "rewrite.js", 3);

    GW.backToTop.dataset.scrollPosition = Math.round(100 * (window.pageYOffset / (document.documentElement.offsetHeight - window.innerHeight)));
    GW.backToTop.style.backgroundSize = `100% ${GW.backToTop.dataset.scrollPosition}%`;
}

/*********************************************************************/
/*  Show/hide the back-to-top link in response to scrolling.

    Called by the ‘updateBackToTopLinkScrollListener’ scroll listener.
 */
function updateBackToTopLinkVisibility(event) {
    GWLog("updateBackToTopLinkVisibility", "rewrite.js", 3);

    //  One PgDn’s worth of scroll distance, approximately.
    let onePageScrollDistance = (0.8 * window.innerHeight);

    if (GW.isMobile()) {
        //  Show back-to-top link on ANY scroll down.
        if (GW.scrollState.unbrokenDownScrollDistance > 0)
            GW.backToTop.classList.toggle("hidden", false);
    } else {
        //  Show back-to-top link when scrolling a full page down from the top.
        if (GW.scrollState.unbrokenDownScrollDistance > onePageScrollDistance)
            GW.backToTop.classList.toggle("hidden", false);
    }

    //  Hide back-to-top link when scrolling to top.
    if (GW.scrollState.lastScrollTop <= 0)
        GW.backToTop.classList.toggle("hidden", true);
}

/***********************************************************************/
/*  Injects the “back to top” link. (Called only for the main document.)
 */
addContentLoadHandler(GW.contentLoadHandlers.injectBackToTopLink = (eventInfo) => {
    GWLog("injectBackToTopLink", "rewrite.js", 1);

    GW.backToTop = addUIElement(`<div id="back-to-top"><a href="#top" tabindex="-1" title="Back to top">` +
        `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="M6.1 422.3l209.4-209.4c4.7-4.7 12.3-4.7 17 0l209.4 209.4c4.7 4.7 4.7 12.3 0 17l-19.8 19.8c-4.7 4.7-12.3 4.7-17 0L224 278.4 42.9 459.1c-4.7 4.7-12.3 4.7-17 0L6.1 439.3c-4.7-4.7-4.7-12.3 0-17zm0-143l19.8 19.8c4.7 4.7 12.3 4.7 17 0L224 118.4l181.1 180.7c4.7 4.7 12.3 4.7 17 0l19.8-19.8c4.7-4.7 4.7-12.3 0-17L232.5 52.9c-4.7-4.7-12.3-4.7-17 0L6.1 262.3c-4.7 4.7-4.7 12.3 0 17z"/></svg>`
        + `</a></div>`);

    //  Show/hide the back-to-top link on scroll up/down.
    addScrollListener((event) => {
        updateBackToTopLinkVisibility(event);

        if (GW.isMobile())
            updateBackToTopLinkScrollPositionIndicator(event);
    }, "updateBackToTopLinkScrollListener", { defer: true, ifDeferCallWhenAdd: true });

    //  Show the back-to-top link on mouseover.
    GW.backToTop.addEventListener("mouseenter", (event) => {
        GW.backToTop.style.transition = "none";
        GW.backToTop.classList.toggle("hidden", false);
    });
    GW.backToTop.addEventListener("mouseleave", (event) => {
        GW.backToTop.style.transition = "";
    });
    GW.backToTop.addEventListener("click", (event) => {
        GW.backToTop.style.transition = "";
    });
}, "rewrite", (info) => info.container == document.body);


/*****************/
/* END OF LAYOUT */
/*****************/

/*  Run the given function immediately if page layout has completed, or add an
    event handler to run it as soon as page layout completes.
 */
function doWhenPageLayoutComplete(f) {
    if (GW.pageLayoutComplete == true)
        f();
    else
        GW.notificationCenter.addHandlerForEvent("GW.pageLayoutDidComplete", (info) => {
            f();
        }, { once: true });
}

doWhenPageLoaded(() => {
    GW.notificationCenter.fireEvent("GW.pageLayoutWillComplete");
    requestAnimationFrame(() => {
        GW.pageLayoutComplete = true;
        GW.notificationCenter.fireEvent("GW.pageLayoutDidComplete");
    });
});


/***********/
/* CONSOLE */
/***********/

function $ (f) {
	try {
		let result = typeof f == "function"
					 ? f()
					 : eval(f);
		if (result != undefined)
			GW.console.print(result);
	} catch (e) {
		GW.console.print(e);
	}
}

GW.console = {
	outputBuffer: newDocument(),

	flushBuffer: () => {
		if (GW.console.view == false)
			return;

		GW.console.view.contentView.append(GW.console.outputBuffer);
		GW.console.updateHeight();
		GW.console.scrollToBottom();
	},

	scrollToBottom: () => {
		GW.console.view.scrollView.scrollTop = GW.console.view.contentView.clientHeight 
											 - GW.console.view.scrollView.clientHeight;
	},

	clearOutput: () => {
		GW.console.view.contentView.replaceChildren();
		GW.console.updateHeight();
		GW.console.scrollToBottom();
	},

	print: (entity, flush = true) => {
		let style = "";
		if (   entity == undefined
			|| entity == null)
			style = "color: #777;"
		if (entity instanceof Error)
			style = "color: #f00;"

		let output;
		if (entity instanceof Error) {
			output = entity.stack;
			console.error(entity);
		} else if (typeof entity == "string") {
			output = entity.replace(/</g, "&lt;").replace(/>/g, "&gt;");
			console.log(entity);
		} else {
			if (entity) {
				let jsonString = JSON.stringify(entity, null, "\n"
					 				  ).replace(/\\n/g, "\n"
					 				  ).replace(/\\t/g, "\t"
					 				  ).replace(/\\"/g, "\"")
				console.log(jsonString);
				jsonString = JSON.stringify(entity, null, "\n"
					 			  ).replace(/\\t/g, "    "
					 			  ).replace(/\\"/g, "&quot;"
					 			  ).replace(/</g, "&lt;"
					 			  ).replace(/>/g, "&gt;"
					 			  ).replace(/\n+/g, "\n"
					 			  ).replace(/\\n/g, "<br>")
				output = jsonString;
			} else {
				console.log(entity);
				output = entity;
			}
		}

		GW.console.outputBuffer.appendChild(newElement("P", { style: style }, { innerHTML: output }));

		if (flush)
			GW.console.flushBuffer();
	},

	setInputCursorPosition: (pos) => {
		GW.console.view.input.setSelectionRange(pos, pos);
	},

	show: () => {
		GW.console.scrollToBottom();
		GW.console.view.classList.toggle("hidden", false);
		GW.console.view.input.focus();
	},

	hide: () => {
		GW.console.view.input.blur();
		GW.console.view.classList.toggle("hidden", true);
	},

	isVisible: () => {
		return (GW.console.view.classList.contains("hidden") == false);
	},

	updateHeight: () => {
		GW.console.view.style.setProperty("--GW-console-view-height", GW.console.view.offsetHeight + "px");
	},

	setPrompt: (string) => {
		GW.console.view.prompt.innerHTML = string;
	},

	clearCommandLine: () => {
		GW.console.view.input.value = "";
	},

	keyDown: (event) => {
		if (GW.console.isVisible() == false)
			return;

		let allowedKeys = [ "Enter", "ArrowUp", "ArrowDown" ];
		if (allowedKeys.includes(event.key) == false)
			return;

		if (document.activeElement != GW.console.view.input)
			return;

		switch (event.key) {
			case "Enter":
				GW.console.commandLineCommandReceived();
				break;

			case "ArrowUp":
				event.preventDefault();
				if (GW.console.commandLog_pointer == GW.console.commandLog.length)
					GW.console.commandLog_currentCommandLine = GW.console.view.input.value;
				let prevLine = GW.console.commandLog_prevEntry();
				if (prevLine != null) {
					GW.console.view.input.value = prevLine;
					GW.console.setInputCursorPosition(GW.console.view.input.value.length);
				}
				break;

			case "ArrowDown":
				event.preventDefault();
				let nextLine = GW.console.commandLog_nextEntry();
				if (nextLine != null) {
					GW.console.view.input.value = nextLine;
					GW.console.setInputCursorPosition(GW.console.view.input.value.length);
				}
				break;
		}
	},

	keyUp: (event) => {
		let allowedKeys = [ "`", "Esc", "Escape" ];
		if (allowedKeys.includes(event.key) == false)
			return;

		switch (event.key) {
			case "`":
				if (GW.console.isVisible() == false)
					GW.console.show();
				break;
			case "Esc":
			case "Escape":
				if (GW.console.isVisible() == true)
					GW.console.hide();
				break;
		};
	},

	commandLineInputReceived: (event) => {
		//	Nothing… yet.
	},

	commandLog: [ ],
	commandLog_currentCommandLine: "",
	commandLog_pointer: 0,
	commandLog_prevEntry: () => {
		if (GW.console.commandLog_pointer == 0)
			return null;

		return GW.console.commandLog_entryAtIndex(--(GW.console.commandLog_pointer));
	},
	commandLog_nextEntry: () => {
		if (GW.console.commandLog_pointer == GW.console.commandLog.length)
			return null;

		return GW.console.commandLog_entryAtIndex(++(GW.console.commandLog_pointer));
	},
	commandLog_entryAtIndex: (index) => {
		return (index == GW.console.commandLog.length
				? GW.console.commandLog_currentCommandLine
				: GW.console.commandLog[index]);
	},

	commandLineCommandReceived: () => {
		let inputLine = event.target.value;

		GW.console.print("> " + inputLine);
		GW.console.clearCommandLine();

		GW.console.commandLog.push(inputLine);
		GW.console.commandLog_currentCommandLine = "";
		GW.console.commandLog_pointer = GW.console.commandLog.length;

		if (/^`.*`$/.test(inputLine))
			GW.console.jsExecLine(inputLine.slice(1, -1));
		else
			GW.console.execLine(inputLine);
	},

	execLine: (line) => {
		let command = line;
		switch (command.toLowerCase()) {
			case "clear":
				GW.console.clearOutput();
				break;
			default:
				GW.console.print(`gwrnsh: ${line}: command not found.`);
				break;
		}
	},

	jsExecLine: (line) => {
		$(line);
	}
};

//	Dump temporary buffer.
if (GW.consoleTempBuffer > "") {
	GW.consoleTempBuffer.split("\n").forEach(line => {
		GW.console.print(line, false);
	});
	GW.consoleTempBuffer = null;
}

doWhenBodyExists(() => {
	//	Construct views.
	GW.console.view = addUIElement(`<div id="console" class="hidden">
		<div class="console-scroll-view">
			<div class="console-content-view"></div>
		</div>
		<div class="console-command-line">
			<div class="console-command-line-prompt">
				<span></span>
			</div>
			<div class="console-command-line-entry-field">
				<input name="console-command" type="text" autocomplete="off"></input>
			</div>
		</div>
	</div>`);

	//	Convenience references.
	GW.console.view.scrollView = GW.console.view.querySelector(".console-scroll-view");
	GW.console.view.contentView = GW.console.view.querySelector(".console-content-view");
	GW.console.view.prompt = GW.console.view.querySelector(".console-command-line-prompt span");
	GW.console.view.input  = GW.console.view.querySelector(".console-command-line-entry-field input");

	//	Set prompt.
	GW.console.setPrompt(location.pathname);

	//	Flush output buffer.
	GW.console.flushBuffer();

	//	Update height.
	GW.console.updateHeight();

	//	Add event listeners, if console enabled.
	if (   getQueryVariable("console") == "1"
		|| getQueryVariable("console") == "2"
		|| localStorage.getItem("console-enabled") == "true") {
		//	Add show/hide key event listener.
		document.addEventListener("keyup", GW.console.keyUp);

		//	Add command line “Enter” key event listener.
		document.addEventListener("keydown", GW.console.keyDown);

		//	Add command line input (text entry) event listener.
		GW.console.view.input.addEventListener("input", GW.console.commandLineInputReceived);
	}

	//	Show console, if auto-show enabled.
	if (getQueryVariable("console") == "2")
		GW.console.show();
});


/**************************/
/* BROKEN ANCHOR CHECKING */
/**************************/
/*  If a reader loads a page and the anchor ID/hash does not exist inside the page,
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

/*  Check for broken anchor (location hash not pointing to any element on the
    page) both at page load time and whenever the hash changes.
 */
GW.notificationCenter.addHandlerForEvent("GW.hashHandlingSetupDidComplete", GW.brokenAnchorCheck = (info) => {
    GWLog("GW.brokenAnchorCheck", "rewrite.js", 1);

    if (   location.hash > ""
        && /^#if_slide/.test(location.hash) == false
        && /^#:~:/.test(location.hash) == false
        && document.querySelector(selectorFromHash(location.hash)) == null)
        reportBrokenAnchorLink(location);
}, { once: true });
GW.notificationCenter.addHandlerForEvent("GW.hashDidChange", GW.brokenAnchorCheck);


/**************************/
/* LOCATION HASH HANDLING */
/**************************/

function cleanLocationHash() {
    GWLog("cleanLocationHash", "rewrite.js", 2);

    if (   location.hash == "#top"
        || (   location.hash == ""
            && location.href.endsWith("#"))) {
        relocate(location.pathname);
    }
}

GW.notificationCenter.addHandlerForEvent("GW.pageLayoutDidComplete", GW.pageLayoutCompleteHashHandlingSetup = (info) => {
    GWLog("GW.pageLayoutCompleteHashHandlingSetup", "rewrite.js", 1);

    //  Chrome’s fancy new “scroll to text fragment”. Deal with it in Firefox.
    if (GW.isFirefox()) {
        if (location.hash.startsWith("#:~:")) {
            relocate(location.pathname);
        } else if (location.hash.includes(":~:")) {
            relocate(location.hash.replace(/:~:.*$/, ""));
        }
    }

    //  Clean location hash.
    cleanLocationHash();

    //  Save hash, for change tracking.
    GW.locationHash = location.hash;

    /*  Remove “#top” or “#” from the URL hash (e.g. after user clicks on the
        back-to-top link).
     */
    window.addEventListener("hashchange", GW.handleBrowserHashChangeEvent = () => {
        GWLog("GW.handleBrowserHashChangeEvent", "rewrite.js", 1);

        //  Clean location hash.
        cleanLocationHash();

        //  If hash really changed, update saved hash and fire event.
        if (GW.locationHash != location.hash) {
            GW.notificationCenter.fireEvent("GW.hashDidChange", { oldHash: GW.locationHash });
            GW.locationHash = location.hash;
        }
    });

    GW.notificationCenter.fireEvent("GW.hashHandlingSetupDidComplete");
}, { once: true });


/************/
/* PRINTING */
/************/

/*********************************************************************/
/*	Trigger transcludes and expand-lock collapse blocks when printing.
 */
window.addEventListener("beforeprint", (event) => {
	GWLog("Print command received.", "rewrite.js", 1);

	function expand(container) {
		if (   container instanceof Element
			&& container.closest("#link-bibliography"))
			return;

		Transclude.triggerTranscludesInContainer(container);
		expandLockCollapseBlocks({ container: container });
	}

	GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", GW.expandAllContentWhenLoadingPrintView = (info) => {
		expand(info.container);
	});

	expand(document);
});
window.addEventListener("afterprint", (event) => {
	GWLog("Print command completed.", "rewrite.js", 1);

	GW.notificationCenter.removeHandlerForEvent("GW.contentDidInject", GW.expandAllContentWhenLoadingPrintView);
});


/*****************************************************************************************/
/*! instant.page v5.1.0 - (C) 2019-2020 Alexandre Dieulot - https://instant.page/license */
/* Settings: 'prefetch' (loads HTML of target) after 1600ms hover (desktop) or mouse-down-click (mobile); TODO: left in logging for testing during experiment */
let pls="a:not(.has-content)";let t,e;const n=new Set,o=document.createElement("link"),z=o.relList&&o.relList.supports&&o.relList.supports("prefetch")&&window.IntersectionObserver&&"isIntersecting"in IntersectionObserverEntry.prototype,s="instantAllowQueryString"in document.body.dataset,a=true,r="instantWhitelist"in document.body.dataset,c="instantMousedownShortcut"in document.body.dataset,d=1111;let l=1600,u=!1,f=!1,m=!1;if("instantIntensity"in document.body.dataset){const t=document.body.dataset.instantIntensity;if("mousedown"==t.substr(0,"mousedown".length))u=!0,"mousedown-only"==t&&(f=!0);else if("viewport"==t.substr(0,"viewport".length))navigator.connection&&(navigator.connection.saveData||navigator.connection.effectiveType&&navigator.connection.effectiveType.includes("2g"))||("viewport"==t?document.documentElement.clientWidth*document.documentElement.clientHeight<45e4&&(m=!0):"viewport-all"==t&&(m=!0));else{const e=parseInt(t);isNaN(e)||(l=e)}}if(z){const n={capture:!0,passive:!0};if(f||document.addEventListener("touchstart",function(t){e=performance.now();const n=t.target.closest(pls);if(!h(n))return;v(n.href)},n),u?c||document.addEventListener("mousedown",function(t){const e=t.target.closest(pls);if(!h(e))return;v(e.href)},n):document.addEventListener("mouseover",function(n){if(performance.now()-e<d)return;const o=n.target.closest(pls);if(!h(o))return;o.addEventListener("mouseout",p,{passive:!0}),t=setTimeout(()=>{v(o.href),t=void 0},l)},n),c&&document.addEventListener("mousedown",function(t){if(performance.now()-e<d)return;const n=t.target.closest("a");if(t.which>1||t.metaKey||t.ctrlKey)return;if(!n)return;n.addEventListener("click",function(t){1337!=t.detail&&t.preventDefault()},{capture:!0,passive:!1,once:!0});const o=new MouseEvent("click",{view:window,bubbles:!0,cancelable:!1,detail:1337});n.dispatchEvent(o)},n),m){let t;(t=window.requestIdleCallback?t=>{requestIdleCallback(t,{timeout:1500})}:t=>{t()})(()=>{const t=new IntersectionObserver(e=>{e.forEach(e=>{if(e.isIntersecting){const n=e.target;t.unobserve(n),v(n.href)}})});document.querySelectorAll("a").forEach(e=>{h(e)&&t.observe(e)})})}}function p(e){e.relatedTarget&&e.target.closest("a")==e.relatedTarget.closest("a")||t&&(clearTimeout(t),t=void 0)}function h(t){if(t&&t.href&&(!r||"instant"in t.dataset)&&(a||t.origin==location.origin||"instant"in t.dataset)&&["http:","https:"].includes(t.protocol)&&("http:"!=t.protocol||"https:"!=location.protocol)&&(s||!t.search||"instant"in t.dataset)&&!(t.hash&&t.pathname+t.search==location.pathname+location.search||"noInstant"in t.dataset))return!0}function v(t){if(n.has(t))return;const e=document.createElement("link");console.log("Prefetched: "+t);e.rel="prefetch",e.href=t,document.head.appendChild(e),n.add(t)};
