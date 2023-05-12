/**********/
/* ASSETS */
/**********/

GW.assets = { };


/******************/
/* ASSET VERSIONS */
/******************/

GW.assetVersions = (GW.assetVersions ?? { });

/*****************************************************************************/
/*  Return fully qualified, versioned (if possible) URL for asset at the given
    path.
 */
function versionedAssetURL(pathname) {
    let version = GW.assetVersions[pathname];
    let versionString = (version ? `?v=${version}` : ``);
    return new URL(  location.origin
                   + pathname
                   + versionString);
}


/*************/
/* DOCUMENTS */
/*************/

/********************************************************/
/*  Return the location (URL) associated with a document.
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


/*********/
/* LINKS */
/*********/

/******************************************************************************/
/*	Returns true if the link is an annotated link, OR if it is an include-link
	which transclude.js treats  as an annotation transclude. (This is relevant
	because in either case, the link hash should be ignored, when deciding what
	to do with a link on the basis of it having or not having a link hash.)
 */
function isAnnotationLink(link) {
	return (Annotations.isAnnotatedLink(link) || Transclude.isAnnotationTransclude(link));
}

/****************************************************************************/
/*  Return the element, in the target document, pointed to by the hash of the
    given link (which may be a URL object or an HTMLAnchorElement).
 */
function targetElementInDocument(link, doc) {
    if (isAnchorLink(link) == false)
        return null;

	let anchor = anchorsForLink(link)[0];
    let element = null;

    if (anchor.startsWith("#"))
        element = doc.querySelector(selectorFromHash(anchor));

	if (   element == null
		&& link instanceof HTMLAnchorElement
		&& link.dataset.backlinkTargetUrl > "") {
		//	HAX. (Remove when link IDs are fixed. —SA 2023-03-22)
		/*	Disabling this hack, hopefully it’s no longer needed.
			(See also line below.) —SA 2023-04-29
		 */
// 		let exactBacklinkSelector = null;
// 		if (anchor.startsWith("#gwern")) {
// 			let targetID = "#" + anchor.slice(("#gwern" + link.dataset.backlinkTargetUrl.slice(1).replace("/", "-") + "-").length);
// 			if (targetID > "")
// 				exactBacklinkSelector = `a[href*='${CSS.escape(link.dataset.backlinkTargetUrl + targetID)}']`;
// 		}

		let backlinkSelector = [
			`a[href*='${CSS.escape(link.dataset.backlinkTargetUrl)}']:not(.backlink-not)`,
			`a[data-url-original='${(link.dataset.backlinkTargetUrl)}']:not(.backlink-not)`
		].join(", ");
		let exclusionSelector = [
			"#page-metadata a",
			".aux-links-list a"
		].join(", ");
		/*	Disabling this hack, hopefully it’s no longer needed.
			(See also lines above.) —SA 2023-04-29
		 */
        element = /* doc.querySelector(exactBacklinkSelector) ?? */ (Array.from(doc.querySelectorAll(backlinkSelector)).filter(backlink => {
            return (   (link.dataset.backlinkTargetUrl.startsWith("/")
            			? backlink.pathname == link.dataset.backlinkTargetUrl
            			: (   backlink.href == link.dataset.backlinkTargetUrl
            			   || backlink.dataset.urlOriginal == link.dataset.backlinkTargetUrl))
                    && backlink.closest(exclusionSelector) == null);
        }).first);
    }

    return element;
}

/*****************************************************************************/
/*  Returns true if the given link (a URL or an HTMLAnchorElement) points to a
    specific element within a page, rather than to a whole page. (This is
    usually because the link has a URL hash, but may also be because the link
    is a backlink, in which case it implicitly points to that link in the
    target page which points back at the target page for the backlink; or it
    may be because the link is a link with a value for the `data-target-id`
    or `data-backlink-target-url` attributes.)
 */
function isAnchorLink(link) {
    return (anchorsForLink(link).length == 1);
}

/***********************************************/
/*  Removes all anchor data from the given link.
 */
function stripAnchorsFromLink(link) {
    if (link instanceof HTMLAnchorElement) {
        link.removeAttribute("data-target-id");
        link.removeAttribute("data-backlink-target-url");
    }

    link.hash = "";
}

/****************************************************************************/
/*  Returns an array of anchors for the given link. This array may have zero,
    one, or two elements.
 */
function anchorsForLink(link) {
	if (link instanceof HTMLAnchorElement) {
		if (link.dataset.targetId > "") {
			return link.dataset.targetId.split(" ").map(x => `#${x}`);
		} else if (   isAnnotationLink(link) == false
				   && link.hash > "") {
			return link.hash.match(/#[^#]*/g);
		} else if (   isAnnotationLink(link) == false
				   && link.dataset.backlinkTargetUrl > "") {
			return [ link.dataset.backlinkTargetUrl ];
		} else {
			return [ ];
		}
	} else {
		 return link.hash.match(/#[^#]*/g) ?? [ ];
	}
}

/******************************************************************************/
/*  Return original URL for a link. (Equal to the link’s URL itself for all but
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
        /*  Erase the ?fallback=original query parameter necessary to
            make it redirect if no Ar5iv version is available.
         */
        originalURL.search = "";
    }

    return originalURL;
}


/************/
/* SECTIONS */
/************/

/******************************************************************************/
/*  Returns the heading level of a <section> element. (Given by a class of the
    form ‘levelX’ where X is a positive integer. Defaults to 1 if no such class
    is present.)
 */
function sectionLevel(section) {
    if (  !section
        || section.tagName != "SECTION")
        return null;

    //  Note: ‘m’ is a regexp matches array.
    let m = Array.from(section.classList).map(c => c.match(/^level([0-9]*)$/)).find(m => m);
    return (m ? parseInt(m[1]) : 1);
}


/*************/
/* CLIPBOARD */
/*************/

/*******************************************/
/*	Copy the provided text to the clipboard.
 */
function copyTextToClipboard(text) {
	let scratchpad = document.querySelector("#scratchpad");

	//  Perform copy operation.
	scratchpad.innerText = text;
	selectElementContents(scratchpad);
	document.execCommand("copy");
	scratchpad.innerText = "";
}

/***************************************************/
/*	Create scratchpad for synthetic copy operations.
 */
doWhenDOMContentLoaded(() => {
	document.body.append(newElement("SPAN", { "id": "scratchpad" }));
});

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

/******************************************************************************/
/*  Set up the copy processor system by registering a ‘copy’ event handler to
    call copy processors. (Must be set up for the main document, and separately
    for any shadow roots.)
 */
function registerCopyProcessorsForDocument(doc) {
    GWLog("registerCopyProcessorsForDocument", "rewrite.js", 1);

    doc.addEventListener("copy", (event) => {
		if (   GW.copyProcessors == null
			|| GW.copyProcessors.length == 0)
			return;

        event.preventDefault();
        event.stopPropagation();

        let selection = getSelectionAsDocument(doc);

        let i = 0;
        while (   i < GW.copyProcessors.length
               && GW.copyProcessors[i++](event, selection));

        event.clipboardData.setData("text/plain", selection.textContent);
        event.clipboardData.setData("text/html", selection.innerHTML);
    });
}


/*************/
/* AUX-LINKS */
/*************/

AuxLinks = {
    auxLinksLinkTypes: {
        "/metadata/annotation/backlink/":           "backlinks",
        "/metadata/annotation/similar/":            "similars",
        "/metadata/annotation/link-bibliography/":  "link-bibliography"
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
    /*  Get the (side|foot)note number from the URL hash (which might point to a
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

	noteNumber: (element) => {
		return Notes.noteNumberFromHash(element.hash ?? element.id);
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
        if (footnoteBackLink) {
	        footnoteBackLink.hash = footnoteBackLink.hash.slice(0, 6) + number;
	    }

        //  #fnN
        let footnoteSelfLink = footnote.querySelector("a.footnote-self-link");
        if (footnoteSelfLink) {
			footnoteSelfLink.hash = footnoteSelfLink.hash.slice(0, 3) + number;
			footnoteSelfLink.title = "Link to footnote " + number;
		}

		//	Footnote backlinks.
		let backlinksListLabelLink = footnote.querySelector(".section-backlinks .backlinks-list-label a");
		if (backlinksListLabelLink) {
			//  #fnN
			backlinksListLabelLink.hash = backlinksListLabelLink.hash.slice(0, 3) + number;

			//	N
			backlinksListLabelLink.querySelector("span.footnote-number").innerText = number;
		}
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


/*********************/
/* TABLE OF CONTENTS */
/*********************/

/*******************************************************************************/
/*  Updates the page TOC with any sections within the given container that don’t
    already have TOC entries.
 */
//  Called by: updateMainPageTOC (rewrite.js)
//  Called by: includeContent (transclude.js)
function updatePageTOC(newContent, needsProcessing = false) {
    GWLog("updatePageTOC", "transclude.js", 2);

    let TOC = document.querySelector("#TOC");
    if (!TOC)
        return;

    //  Don’t nest TOC entries any deeper than this.
    let maxNestingDepth = 4;

    /*  Find where to insert the new TOC entries.
    	Any already-existing <section> should have a TOC entry.
    	(Unless the TOC entry has been removed or is missing for some reason,
    	 in which case use the entry for the section after that, and so on.)
     */
    let parentSection = newContent.closest("section") ?? document.querySelector("#markdownBody");
    let parentTOCElement = parentSection.id == "markdownBody"
                           ? TOC
                           : TOC.querySelector(`#toc-${(CSS.escape(parentSection.id))}`).parentElement;

    let currentSection = newContent;
    let nextSection = null;
    let nextSectionTOCLink = null;
    do {
    	nextSection = Array.from(parentSection.children).filter(child =>
			   child.tagName == "SECTION"
			&& child.compareDocumentPosition(currentSection) == Node.DOCUMENT_POSITION_PRECEDING
		).first;
		currentSection = nextSection;
		nextSectionTOCLink = nextSection ? parentTOCElement.querySelector(`#toc-${(CSS.escape(nextSection.id))}`) : null;
	} while (nextSection && nextSectionTOCLink == null);
    let followingTOCElement = nextSectionTOCLink
                              ? nextSectionTOCLink.parentElement
                              : null;

    //  TOC entry insertion function, called recursively.
    function addToPageTOC(newContent, parentTOCElement, followingTOCElement) {
        let addedEntries = [ ];

        newContent.querySelectorAll("section").forEach(section => {
            /*  We may have already added this section in a recursive call from
                a previous section.
             */
            if (parentTOCElement.querySelector(`a[href$='#${(CSS.escape(fixedEncodeURIComponent(section.id)))}']`) != null)
                return;

            /*  If this section is too deeply nested, do not add it.
             */
            if (sectionLevel(section) > maxNestingDepth)
                return;

            //  Construct entry.
            let entry = newElement("LI");
            let entryText = section.id == "footnotes"
                            ? "Footnotes"
                            : section.firstElementChild.querySelector("a").innerHTML;
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

		//	Rectify typography in new entries.
        newEntries.forEach(entry => {
	        Typography.processElement(entry, Typography.replacementTypes.WORDBREAKS, true);
        });
    }
}


/*************/
/* FOOTNOTES */
/*************/

/*****************************************************************************/
/*	Mark hash-targeted footnote with ‘targeted’ class.
 */
function updateFootnoteTargeting() {
	GWLog("updateFootnoteTargeting", "rewrite.js", 1);

	if (   Sidenotes
		&& Sidenotes.mediaQueries.viewportWidthBreakpoint.matches)
		return;

	//	Clear any existing targeting.
	let targetedElementSelector = [
		".footnote-ref",
		".footnote"
	].map(x => x + ".targeted").join(", ");
	document.querySelectorAll(targetedElementSelector).forEach(element => {
		element.classList.remove("targeted");
	});

	//  Identify and mark target footnote.
	let target = location.hash.match(/^#(fn|fnref)[0-9]+$/)
				 ? getHashTargetedElement()
				 : null;
	if (target)
		target.classList.add("targeted");
}


/******************************/
/* GENERAL ACTIVITY INDICATOR */
/******************************/

GW.activities = [ ];

function beginActivity() {
	GW.activities.push({ });

	if (GW.activityIndicator)
		GW.activityIndicator.classList.add("on");
}

function endActivity() {
	GW.activities.shift();

	if (   GW.activityIndicator
		&& GW.activities.length == 0)
		GW.activityIndicator.classList.remove("on");
}


/********/
/* MISC */
/********/

/****************************************************************************/
/*	Returns relevant scroll container for the given element. Null is returned
	for elements whose scroll container is just the viewport.
 */
function scrollContainerOf(element) {
	if (   Extracts
		&& Extracts.popFrameProvider) {
		let containingPopFrame = Extracts.popFrameProvider.containingPopFrame(element);
		if (containingPopFrame)
			return containingPopFrame.scrollView;
	}

	return null;
}

/*********************************************************/
/*	Returns page scroll position, as integer (percentage).
 */
function getPageScrollPosition() {
	return Math.round(100 * (window.pageYOffset / (document.documentElement.offsetHeight - window.innerHeight)));
}

/*********************************************************************/
/*	Returns a saved (in local storage) integer, or 0 if nothing saved.
 */
function getSavedCount(key) {
	return parseInt(localStorage.getItem(key) || "0");
}

/*****************************************************************************/
/*	Add 1 to a saved (in local storage) integer, or set it to 1 if none saved.
 */
function incrementSavedCount(key) {
	localStorage.setItem(key, getSavedCount(key) + 1);
}


/***********/
/* PAGE UI */
/***********/

/*************************************************************************/
/*  Adds given element (first creating it from HTML, if necessary) to
    #ui-elements-container (creating the latter if it does not exist), and
    returns the added element.
 */
function addUIElement(element) {
    let uiElementsContainer = (   document.querySelector("#ui-elements-container")
    						   ?? document.querySelector("body").appendChild(newElement("DIV", { id: "ui-elements-container" })));

	if (typeof element == "string")
		element = elementFromHTML(element);

    return uiElementsContainer.appendChild(element);
}


/****************/
/* PAGE TOOLBAR */
/****************/

GW.pageToolbar = {
	maxDemos: 1,

	toolbar: null,

	/*	Adds and returns page toolbar. (If page toolbar already exists, returns
		existing page toolbar.)

		NOTE: This function may run before GW.pageToolbar.setup().
	 */
	getToolbar: () => {
		return (   GW.pageToolbar.toolbar
				?? addUIElement(  `<div id="page-toolbar"`
								+ (GW.isMobile() ? ` class="mobile"` : ``)
								+ `><div class="buttons"></div></div>`));
	},

	/*	Adds provided button (first creating it from HTML, if necessary) to the
		page toolbar, and returns the added button.

		NOTE: This function may run before GW.pageToolbar.setup().
	 */
	addButton: (button) => {
		if (typeof button == "string")
			button = elementFromHTML(button);

		return GW.pageToolbar.getToolbar().querySelector(".buttons").appendChild(button);
	},

	/*	Adds a button group containing the provided buttons (first creating
		them from HTML, if necessary) to the page toolbar, and returns the
		added button group.

		NOTE: This function may run before GW.pageToolbar.setup().
	 */
	addButtonGroup: (buttons, buttonGroupProperties) => {
		if (typeof buttons == "string")
			buttons = Array.from(newDocument(buttons).children);

		let buttonGroup = newElement("DIV", buttonGroupProperties);
		buttonGroup.classList.add("button-group");
		buttonGroup.append(...buttons);

		return GW.pageToolbar.getToolbar().querySelector(".buttons").appendChild(buttonGroup);
	},

	toggleCollapseState: () => {
		if (GW.pageToolbar.toolbar.classList.contains("collapsed")) {
			GW.pageToolbar.uncollapse();
		} else {
			GW.pageToolbar.collapse();
		}
	},

	collapse: (slowly = false) => {
		clearTimeout(GW.pageToolbar.toolbar.collapseTimer);

		GW.pageToolbar.toolbar.classList.add("collapsed");

		if (slowly) {
			GW.pageToolbar.toolbar.classList.add("collapsed-slowly");
			GW.pageToolbar.toolbar.collapseTimer = setTimeout(GW.pageToolbar.collapse, 750);
		} else {
			GW.pageToolbar.toolbar.classList.remove("collapsed-slowly");
		}
	},

	uncollapse: () => {
		clearTimeout(GW.pageToolbar.toolbar.collapseTimer);

		GW.pageToolbar.toolbar.classList.remove("collapsed", "collapsed-slowly");
	},

	fade: () => {
		GW.pageToolbar.toolbar.classList.add("faded");
	},

	unfade: () => {
		GW.pageToolbar.toolbar.classList.remove("faded");
	},

	updateState: (event) => {
		//	Collapse on scroll.
		let thresholdScrollDistance = (0.2 * window.innerHeight);
		if (   GW.scrollState.unbrokenUpScrollDistance   > (0.2 * window.innerHeight)
			|| GW.scrollState.unbrokenDownScrollDistance > (0.2 * window.innerHeight))
			GW.pageToolbar.collapse();

		//	Fade on scroll; unfade when scrolling to top.
		let pageScrollPosition = getPageScrollPosition();
		if (   pageScrollPosition == 0
			|| pageScrollPosition == 100
			|| GW.scrollState.unbrokenUpScrollDistance   > (0.8 * window.innerHeight)) {
			GW.pageToolbar.unfade();
		} else if (GW.scrollState.unbrokenDownScrollDistance > (0.8 * window.innerHeight)) {
			GW.pageToolbar.fade();
		}
	},

	setup: () => {
		if (GW.isMobile()) {
			GW.pageToolbar.toolbar = GW.pageToolbar.getToolbar();

			let startCollapsed = getSavedCount("page-toolbar-demos-count") >= GW.pageToolbar.maxDemos;
			if (startCollapsed) {
				GW.pageToolbar.collapse();
			} else {
				incrementSavedCount("page-toolbar-demos-count");
			}

			GW.pageToolbar.toolbar.append(
				elementFromHTML(
					`<button type="button" title="Toggle button for dark mode & reader mode controls." class="toggle-button main-toggle-button" tabindex="-1">`
				  + `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path d="M495.9 166.6c3.2 8.7 .5 18.4-6.4 24.6l-43.3 39.4c1.1 8.3 1.7 16.8 1.7 25.4s-.6 17.1-1.7 25.4l43.3 39.4c6.9 6.2 9.6 15.9 6.4 24.6c-4.4 11.9-9.7 23.3-15.8 34.3l-4.7 8.1c-6.6 11-14 21.4-22.1 31.2c-5.9 7.2-15.7 9.6-24.5 6.8l-55.7-17.7c-13.4 10.3-28.2 18.9-44 25.4l-12.5 57.1c-2 9.1-9 16.3-18.2 17.8c-13.8 2.3-28 3.5-42.5 3.5s-28.7-1.2-42.5-3.5c-9.2-1.5-16.2-8.7-18.2-17.8l-12.5-57.1c-15.8-6.5-30.6-15.1-44-25.4L83.1 425.9c-8.8 2.8-18.6 .3-24.5-6.8c-8.1-9.8-15.5-20.2-22.1-31.2l-4.7-8.1c-6.1-11-11.4-22.4-15.8-34.3c-3.2-8.7-.5-18.4 6.4-24.6l43.3-39.4C64.6 273.1 64 264.6 64 256s.6-17.1 1.7-25.4L22.4 191.2c-6.9-6.2-9.6-15.9-6.4-24.6c4.4-11.9 9.7-23.3 15.8-34.3l4.7-8.1c6.6-11 14-21.4 22.1-31.2c5.9-7.2 15.7-9.6 24.5-6.8l55.7 17.7c13.4-10.3 28.2-18.9 44-25.4l12.5-57.1c2-9.1 9-16.3 18.2-17.8C227.3 1.2 241.5 0 256 0s28.7 1.2 42.5 3.5c9.2 1.5 16.2 8.7 18.2 17.8l12.5 57.1c15.8 6.5 30.6 15.1 44 25.4l55.7-17.7c8.8-2.8 18.6-.3 24.5 6.8c8.1 9.8 15.5 20.2 22.1 31.2l4.7 8.1c6.1 11 11.4 22.4 15.8 34.3zM256 336a80 80 0 1 0 0-160 80 80 0 1 0 0 160z"/></svg>`
				  + `</button>`
				),
				elementFromHTML(
					`<button type="button" title="Collapse button for dark mode & reader mode controls." class="toggle-button collapse-button" tabindex="-1">`
				  + `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="M441.9 167.3l-19.8-19.8c-4.7-4.7-12.3-4.7-17 0L224 328.2 42.9 147.5c-4.7-4.7-12.3-4.7-17 0L6.1 167.3c-4.7 4.7-4.7 12.3 0 17l209.4 209.4c4.7 4.7 12.3 4.7 17 0l209.4-209.4c4.7-4.7 4.7-12.3 0-17z"/></svg>`
				  + `</button>`
				)
			);

			//	Activate buttons.
			GW.pageToolbar.toolbar.querySelectorAll("button.toggle-button").forEach(button => {
				button.addActivateEvent(event => { GW.pageToolbar.toggleCollapseState(); });

				//	Unfade main toggle button on click/tap.
				if (button.classList.contains("main-toggle-button"))
					button.addEventListener("mousedown", (event) => {
						GW.pageToolbar.unfade();
					});
			});

			doWhenPageLoaded(() => {
				/*	Slowly collapse toolbar shortly after page load (if it’s not
					already collapsed).
				 */
				if (startCollapsed == false)
					setTimeout(GW.pageToolbar.collapse, 2500, true);

				//	Update toolbar state on scroll.
				addScrollListener(GW.pageToolbar.updateState, "updatePageToolbarStateScrollListener", { defer: true });
			});
		} else {
			//	TODO: Desktop version
		}
	},
};

doWhenBodyExists(GW.pageToolbar.setup);


/********************/
/* BACK TO TOP LINK */
/********************/

/*********************************************************************/
/*  Show/hide the back-to-top link in response to scrolling.

    Called by the ‘updateBackToTopLinkScrollListener’ scroll listener.
 */
function updateBackToTopLinkVisibility(event) {
    GWLog("updateBackToTopLinkVisibility", "rewrite.js", 3);

    //  One PgDn’s worth of scroll distance, approximately.
    let onePageScrollDistance = (0.8 * window.innerHeight);

	let pageScrollPosition = getPageScrollPosition();

    //  Hide back-to-top link when scrolling to top.
    if (pageScrollPosition == 0)
        GW.backToTop.classList.toggle("hidden", true);
    //	Show back-to-top link when scrolling to bottom.
    else if (pageScrollPosition == 100)
    	GW.backToTop.classList.toggle("hidden", false);
    //  Show back-to-top link when scrolling a full page down from the top.
    else if (GW.scrollState.unbrokenDownScrollDistance > onePageScrollDistance * 2.0)
        GW.backToTop.classList.toggle("hidden", false);
    //  Hide back-to-top link on half a page’s worth of scroll up.
    else if (GW.scrollState.unbrokenUpScrollDistance > onePageScrollDistance * 0.5)
        GW.backToTop.classList.toggle("hidden", true);
}

/**********************************/
/*  Injects the “back to top” link.
 */
if (GW.isMobile() == false) doWhenPageLoaded(() => {
    GWLog("injectBackToTopLink", "rewrite.js", 1);

    GW.backToTop = addUIElement(`<div id="back-to-top"><a href="#top" tabindex="-1" title="Back to top">`
        + `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="M6.1 422.3l209.4-209.4c4.7-4.7 12.3-4.7 17 0l209.4 209.4c4.7 4.7 4.7 12.3 0 17l-19.8 19.8c-4.7 4.7-12.3 4.7-17 0L224 278.4 42.9 459.1c-4.7 4.7-12.3 4.7-17 0L6.1 439.3c-4.7-4.7-4.7-12.3 0-17zm0-143l19.8 19.8c4.7 4.7 12.3 4.7 17 0L224 118.4l181.1 180.7c4.7 4.7 12.3 4.7 17 0l19.8-19.8c4.7-4.7 4.7-12.3 0-17L232.5 52.9c-4.7-4.7-12.3-4.7-17 0L6.1 262.3c-4.7 4.7-4.7 12.3 0 17z"/></svg>`
        + `</a></div>`);

    //  Show/hide the back-to-top link on scroll up/down.
    addScrollListener(updateBackToTopLinkVisibility, "updateBackToTopLinkScrollListener", { defer: true, ifDeferCallWhenAdd: true });

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
});


/**************************/
/* MOBILE FLOATING HEADER */
/**************************/

if (GW.isMobile()) GW.floatingHeader = {
    minimumYOffset: 0,

    maxChainLength: 3,

    maxHeaderHeight: 60,

    chainLinkClasses: {
        "…": "ellipsis",
        "header": "page-title"
    },

    currentTrail: [ ],

    /*  Scroll down enough to make whatever’s under the header visible.
     */
    adjustScrollTop: () => {
        if (GW.floatingHeader.header == null)
            return;
        let previousHash = GW.locationHash;
        requestAnimationFrame(() => {
            if (location.hash > "") {
                if (previousHash == GW.locationHash)
                    window.scrollBy(0, -1 * GW.floatingHeader.header.offsetHeight);
                else
                    GW.floatingHeader.adjustScrollTop();
            }
        });
    },

    /*  Show/hide the floating header, and update state, in response to
        scroll event.
        (Called by the ‘updateFloatingHeaderScrollListener’ scroll listener.)
     */
    updateState: (event, maxChainLength = GW.floatingHeader.maxChainLength) => {
        GWLog("updateFloatingHeaderState", "rewrite.js", 3);

        //  Show/hide the entire header.
        GW.floatingHeader.header.classList.toggle("hidden",
            window.pageYOffset < GW.floatingHeader.minimumYOffset);

        //  Update scroll indicator bar.
        GW.floatingHeader.scrollIndicator.dataset.scrollPosition = getPageScrollPosition();
        GW.floatingHeader.scrollIndicator.style.backgroundSize = `${GW.floatingHeader.scrollIndicator.dataset.scrollPosition}% 100%`;

        //  Update breadcrumb display.
        let trail = GW.floatingHeader.getTrail();
        if (   trail.join("/") != GW.floatingHeader.currentTrail.join("/")
            || maxChainLength < GW.floatingHeader.maxChainLength) {
            GW.floatingHeader.linkChain.classList.toggle("truncate-page-title", trail.length > maxChainLength);
            let chainLinks = GW.floatingHeader.constructLinkChain(trail, maxChainLength);
            GW.floatingHeader.linkChain.replaceChildren(...chainLinks);
            chainLinks.forEach(link => { link.addActivateEvent(GW.floatingHeader.linkInChainClicked); });

            //  Constrain header height.
            if (   GW.floatingHeader.header.offsetHeight > GW.floatingHeader.maxHeaderHeight
                && maxChainLength > 1)
                GW.floatingHeader.updateState(event, maxChainLength - 1);
            else
                GW.floatingHeader.currentTrail = trail;
        }
    },

    getTrail: (offset = 0) => {
        let element = document.elementFromPoint(window.innerWidth / 2,
                                                GW.floatingHeader.minimumYOffset + offset);
        if (GW.floatingHeader.firstSection.compareDocumentPosition(element) & Node.DOCUMENT_POSITION_PRECEDING)
            return [ "header" ];

        if (   GW.floatingHeader.markdownBody.contains(element) == false
            && GW.floatingHeader.pageMainElement.contains(element) == true)
            return GW.floatingHeader.currentTrail;

        if (element.tagName == "SECTION")
            return (GW.floatingHeader.currentTrail.length == 0
                    ? GW.floatingHeader.getTrail(offset - 10)
                    : GW.floatingHeader.currentTrail);

        let trail = [ ];
        while (element = element.closest("section")) {
            trail.push(`#${element.id}`);
            element = element.parentElement;
        }

        if (trail.length == 0)
            return GW.floatingHeader.currentTrail;

        trail.push("header");
        trail.reverse();

        return trail;
    },

    constructLinkChain: (trail, maxChainLength) => {
        let deleteCount = Math.max(0, trail.length - maxChainLength);
        if (deleteCount > 0) {
            trail = trail.slice();
            trail.splice(0, deleteCount - 1, "…");
        }

        let chain = trail.map(x => newElement("A", {
            href: (x.startsWith("#") ? x : "#top"),
            class: (GW.floatingHeader.chainLinkClasses[x] ?? "")
        }, {
            innerHTML: (x.startsWith("#")
                        ? (x == "#footnotes"
                           ? "Footnotes"
                           : document.querySelector(`#${(CSS.escape(x.slice(1)))}`).firstElementChild.textContent)
                        : (x == "…"
                           ? "…"
                           : GW.floatingHeader.pageHeader.textContent)).trim()
        }));

        if (chain[0].innerHTML == "…") {
            chain[0].href = chain[1].href;
            chain.splice(1, 1);
        }

        return chain;
    },

    linkInChainClicked: (event) => {
        if (event.target.hash == location.hash)
            GW.floatingHeader.adjustScrollTop();
    },

	setup: () => {
		GWLog("GW.floatingHeader.setup", "rewrite.js", 1);

		if (GW.isMobile() == false)
			return;

		GW.floatingHeader.header = addUIElement(  `<div id="floating-header" class="hidden">`
												+ `<div class="link-chain"></div>`
												+ `<div class="scroll-indicator"></div>`
												+ `</div>`);

		//  Pre-query elements, so as not to waste cycles on each scroll event.
		GW.floatingHeader.linkChain = GW.floatingHeader.header.querySelector(".link-chain");
		GW.floatingHeader.scrollIndicator = GW.floatingHeader.header.querySelector(".scroll-indicator");
		GW.floatingHeader.pageHeader = document.querySelector("header");
		GW.floatingHeader.pageMainElement = document.querySelector("main");
		GW.floatingHeader.markdownBody = document.querySelector("#markdownBody");
		GW.floatingHeader.firstSection = document.querySelector("section");

		//  Calculate minimum Y offset.
		let thresholdElement = getComputedStyle(GW.floatingHeader.pageHeader).display != "none"
							   ? GW.floatingHeader.pageHeader
							   : document.querySelector("#sidebar");
		GW.floatingHeader.minimumYOffset = thresholdElement.getBoundingClientRect().top 
										 + window.pageYOffset 
										 + thresholdElement.offsetHeight;

		//  Show/hide the back-to-top link on scroll up/down.
		addScrollListener(GW.floatingHeader.updateState, "updateFloatingHeaderScrollListener",
			{ defer: true, ifDeferCallWhenAdd: true });

		//  Adjust initial scroll offset.
		doWhenPageLayoutComplete(GW.floatingHeader.adjustScrollTop);
	}
};

if (GW.isMobile())
	doWhenPageLoaded(GW.floatingHeader.setup);


/******************************/
/* GENERAL ACTIVITY INDICATOR */
/******************************/

doWhenBodyExists(() => {
    GW.activityIndicator = addUIElement(`<div id="general-activity-indicator" class="on">`
        + `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path d="M288 32c0 17.673-14.327 32-32 32s-32-14.327-32-32 14.327-32 32-32 32 14.327 32 32zm-32 416c-17.673 0-32 14.327-32 32s14.327 32 32 32 32-14.327 32-32-14.327-32-32-32zm256-192c0-17.673-14.327-32-32-32s-32 14.327-32 32 14.327 32 32 32 32-14.327 32-32zm-448 0c0-17.673-14.327-32-32-32S0 238.327 0 256s14.327 32 32 32 32-14.327 32-32zm33.608 126.392c-17.673 0-32 14.327-32 32s14.327 32 32 32 32-14.327 32-32-14.327-32-32-32zm316.784 0c-17.673 0-32 14.327-32 32s14.327 32 32 32 32-14.327 32-32-14.327-32-32-32zM97.608 65.608c-17.673 0-32 14.327-32 32 0 17.673 14.327 32 32 32s32-14.327 32-32c0-17.673-14.327-32-32-32z"/></svg>`
        + `</div>`);
});

doWhenPageLayoutComplete(() => {
    endActivity();
});


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

function realignHash() {
    requestIdleCallback(() => {
        location.hash = GW.locationHash;
    });
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

    //  Correct for Firefox hash / scroll position bug.
    if (GW.isFirefox())
        realignHash();

    /*  Remove “#top” or “#” from the URL hash (e.g. after user clicks on the
        back-to-top link).
     */
    window.addEventListener("hashchange", GW.handleBrowserHashChangeEvent = () => {
        GWLog("GW.handleBrowserHashChangeEvent", "rewrite.js", 1);

        //  Clean location hash.
        cleanLocationHash();

        //  Compensate for floating header.
        if (GW.floatingHeader)
            GW.floatingHeader.adjustScrollTop();

        //  If hash really changed, update saved hash and fire event.
        if (GW.locationHash != location.hash) {
            GW.notificationCenter.fireEvent("GW.hashDidChange", { oldHash: GW.locationHash });
            GW.locationHash = location.hash;
        }
    });

    GW.notificationCenter.fireEvent("GW.hashHandlingSetupDidComplete");
}, { once: true });


