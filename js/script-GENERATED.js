/*******************/
/* INJECT TRIGGERS */
/*******************/

GW.elementInjectTriggers = { };

/****************************************************************************/
/*	Register element inject trigger for the given uuid. (In other words, when 
	element with `data-uuid` attribute with value equal to the given uuid is 
	injected into the document, run the given function on the element.)

	Returns the uuid.

	(If null is passed for the uuid, one will be generated automatically.)

	Each entry thus added triggers only once per uuid, then deletes itself.
 */
function onInject(uuid, f) {
	uuid = uuid ?? crypto.randomUUID();

	GW.elementInjectTriggers[uuid] = f;

	return uuid;
}

/***********************************************************************/
/*	Watch for element injections in the given document. Process injected 
	elements through registered inject triggers.
 */
function observeInjectedElementsInDocument(doc) {
	let observer = new MutationObserver((mutationsList, observer) => {
		if (Object.entries(GW.elementInjectTriggers).length == 0)
			return;

		let doTrigger = (node, f) => {
			delete GW.elementInjectTriggers[node.dataset.uuid];
			f(node);
			node.dataset.uuid = null;
		};

		for (mutationRecord of mutationsList) {
			for (let [ uuid, f ] of Object.entries(GW.elementInjectTriggers)) {
				for (node of mutationRecord.addedNodes) {
					if (node instanceof HTMLElement) {
						if (node.dataset.uuid == uuid) {
							doTrigger(node, f);
							break;
						} else {
							let nestedNode = node.querySelector(`[data-uuid='${uuid}']`);
							if (nestedNode) {
								doTrigger(nestedNode, f);
								break;
							}
						}
					}
				}
			}
		}
	});

	observer.observe(doc, { subtree: true, childList: true });
}

observeInjectedElementsInDocument(document);

/******************************************************************************/
/*	Returns a placeholder element that, when injected, replaces itself with the
	return value of the provided replacement function (to which the placeholder
	is passed).

	If an optional wrapper function is given, replacement is done within an
	anonymous closure which is passed to the wrapper function. (This can be 
	used to, e.g., delay replacement, by passing a suitable doWhen function
	as the wrapper.)
 */
function placeholder(replaceFunction, wrapperFunction = null) {
	let transform;
	if (wrapperFunction) {
		transform = (element) => {
			wrapperFunction(() => {
				element.replaceWith(replaceFunction(element));
			});
		};
	} else {
		transform = (element) => {
			element.replaceWith(replaceFunction(element));
		};
	}

	let uuid = onInject(null, transform);

	return `<span class="placeholder" data-uuid="${uuid}"></span>`;
}


/**********/
/* ASSETS */
/**********/

doAjax({
	location: versionedAssetURL("/static/img/icon/icons.svg"),
	onSuccess: (event) => {
		GW.svgIconFile = newDocument(event.target.response);

		GW.notificationCenter.fireEvent("GW.SVGIconsLoaded");
	}
});

function doWhenSVGIconsLoaded(f) {
    if (GW.svgIconFile != null)
        f();
    else
        GW.notificationCenter.addHandlerForEvent("GW.SVGIconsLoaded", (info) => {
            f();
        }, { once: true });
}

GW.svg = (icon) => {
	if (GW.svgIconFile == null)
		return placeholder(element => elementFromHTML(GW.svg(icon)), doWhenSVGIconsLoaded);

	let iconView = GW.svgIconFile.querySelector(`#${icon}`);
	if (iconView == null)
		return null;

	let viewBox = iconView.getAttribute("viewBox").split(" ").map(x => parseFloat(x));
	let g = iconView.nextElementSibling;
	let xOffset = parseFloat(g.getAttribute("transform").match(/translate\((.+?), .+\)/)[1]);
	viewBox[0] -= xOffset;
	viewBox = viewBox.join(" ");

	return (  `<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="${viewBox}">`
			+ g.innerHTML
			+ `</svg>`);
};


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
    return URLFromString(pathname + versionString);
}

/*****************************************************************************/
/*	Return a random alternate asset pathname (not versioned), given a pathname
	with ‘%R’ where a number should be, e.g.:

		/static/img/logo/christmas/light/logo-christmas-light-%R-small-1x.png

	will return

		/static/img/logo/christmas/light/logo-christmas-light-1-small-1x.png

	(or -2, -3, etc., selecting randomly from available numbered alternates).

	Specified assets must be listed in the versioned asset database.
 */
function randomAsset(assetPathname) {
	let assetPathnameRegExp = new RegExp(assetPathname.replace("%R", "[0-9]+"));
	let alternateAssetPathnames = [ ];
	for (versionedAssetPathname of Object.keys(GW.assetVersions)) {
		if (assetPathnameRegExp.test(versionedAssetPathname))
			alternateAssetPathnames.push(versionedAssetPathname);
	}

	return alternateAssetPathnames[rollDie(alternateAssetPathnames.length) - 1];
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
	return (Annotations.isAnnotatedLinkFull(link) || Transclude.isAnnotationTransclude(link));
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

		let backlinkSelector = `a[href*='${CSS.escape(link.dataset.backlinkTargetUrl)}']:not(.backlink-not)`;
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
            			: backlink.href == link.dataset.backlinkTargetUrl)
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

        let citationNumber = Notes.noteNumber(citation);
        let selector = `#fn${citationNumber}, #sn${citationNumber}`;

        let allNotes = Array.from(document.querySelectorAll(selector)
        			   ).concat(Array.from(citation.getRootNode().querySelectorAll(selector))
        			   ).concat(Extracts.popFrameProvider.allSpawnedPopFrames().flatMap(popFrame => 
									Array.from(popFrame.body.querySelectorAll(selector)))
        			   ).unique();
        /*  We must check to ensure that the note in question is from the same
            page as the citation (to distinguish between main document and any
            full-page embeds that may be spawned).
         */
        return allNotes.filter(note => (note.querySelector(".footnote-back")?.pathname == citation.pathname));
    }
};


/****************/
/* MARGIN NOTES */
/****************/

GW.marginNotes = {
	//	Don’t show margin notes block if there are fewer notes than this.
	minimumAggregatedNotesCount: 3,

	aggregationNeededInDocuments: [ ]
};

/****************************************************************************/
/*	Aggregate margin notes, on the next animation frame, if not already done.
 */
function aggregateMarginNotesIfNeeded(eventInfo) {
	if (GW.marginNotes.aggregationNeededInDocuments.includes(eventInfo.document) == false)
		GW.marginNotes.aggregationNeededInDocuments.push(eventInfo.document);

	requestAnimationFrame(() => {
		if (GW.marginNotes.aggregationNeededInDocuments.includes(eventInfo.document) == false)
			return;

		GW.marginNotes.aggregationNeededInDocuments.remove(eventInfo.document);

		aggregateMarginNotes(eventInfo);
	});
}

/**************************/
/*	Aggregate margin notes.
 */
function aggregateMarginNotes(eventInfo) {
    GWLog("aggregateMarginNotes", "misc.js", 2);

	let marginNotesBlockClass = "margin-notes-block";

	eventInfo.document.querySelectorAll(".marginnote").forEach(marginNote => {
		if (marginNote.textContent.trim() == "☞")
			return;

		let section = marginNote.closest("section, .markdownBody");
		if (section == null)
			return;

		let marginNotesBlock = section.querySelector(`#${(CSS.escape(section.id))}-${marginNotesBlockClass}`);
		if (marginNotesBlock == null) {
			/*	Construct the margin notes block. It should go after any 
				abstract and/or epigraph that opens the section.
			 */
			let firstBlock = firstBlockOf(section, {
				alsoSkipElements: [ 
					".abstract blockquote", 
					".epigraph", 
					"p.data-field"
				]
			}, true);

			let marginNoteBlockContainerElementsSelector = [ 
				"section",
				".markdownBody",
				".abstract-collapse:not(.abstract)",
				".collapse-content-wrapper",
				".annotation-abstract"
			].join(", ");
			while (firstBlock.parentElement.matches(marginNoteBlockContainerElementsSelector) == false)
				firstBlock = firstBlock.parentElement;

			//	Inject the margin notes block.
			marginNotesBlock = newElement("P", {
				class: marginNotesBlockClass,
				id: `${section.id}-${marginNotesBlockClass}`
			});
			firstBlock.parentElement.insertBefore(marginNotesBlock, firstBlock);
		}

		//	Prevent duplication.
		if (Array.from(marginNotesBlock.children).findIndex(child => {
				let trimmedNoteContent = marginNote.textContent.trim()
				return (trimmedNoteContent.endsWith(".")
						? child.textContent.trim() == trimmedNoteContent.slice(0, -1)
						: child.textContent.trim() == trimmedNoteContent);
			}) != -1)
			return;

		//	Clone the note.
		let clonedNote = marginNote.cloneNode(true);

		//	Set margin note type class.
		clonedNote.swapClasses([ "inline", "sidenote" ], 0);

		//	Unwrap the inner wrapper (unneeded here).
		unwrap(clonedNote.querySelector(".marginnote-inner-wrapper"));

		//	Trim whitespace.
		clonedNote.innerHTML = clonedNote.innerHTML.trim();

		//	Strip trailing period.
		if (clonedNote.textContent.endsWith("."))
			clonedNote.lastTextNode.nodeValue = clonedNote.lastTextNode.nodeValue.slice(0, -1);

		//	Append.
		marginNotesBlock.append(clonedNote);
	});

	//	Update visibility of margin note blocks.
	eventInfo.document.querySelectorAll(`.${marginNotesBlockClass}`).forEach(marginNotesBlock => {
		marginNotesBlock.classList.toggle("hidden", marginNotesBlock.children.length < GW.marginNotes.minimumAggregatedNotesCount);			
	});
}


/*********************/
/* TABLE OF CONTENTS */
/*********************/

GW.TOC = { };

/*********************************************************************/
/*	Update page TOC, on the next animation frame, if not already done.
 */
function updatePageTOCIfNeeded(eventInfo) {
	GW.TOC.needsUpdate = true;

	requestAnimationFrame(() => {
		if (GW.TOC.needsUpdate == false)
			return;

		GW.TOC.needsUpdate = false;

		updatePageTOC(eventInfo);
	});
}

/*****************************************************************************/
/*  Updates the page TOC with any sections in the page that don’t already have 
	TOC entries.
 */
//  Called by: updateMainPageTOC (rewrite.js)
//  Called by: includeContent (transclude.js)
function updatePageTOC(eventInfo) {
    GWLog("updatePageTOC", "misc.js", 2);

    let TOC = document.querySelector("#TOC");
    if (!TOC)
        return;

    //  Don’t nest TOC entries any deeper than this.
    let maxNestingDepth = 4;

	//	Collect new entries, for later processing (if need be).
	let newEntries = [ ];

	document.querySelectorAll("#markdownBody section").forEach(section => {
		//	If this section already has a TOC entry, return.
		if (TOC.querySelector(`a[href$='#${(CSS.escape(fixedEncodeURIComponent(section.id)))}']`) != null)
			return;

		//  If this section is too deeply nested, do not add it.
		if (sectionLevel(section) > maxNestingDepth)
			return;

		/*  Find where to insert the new TOC entry.
			Any already-existing <section> should have a TOC entry.
			(Unless the TOC entry has been removed or is missing for some reason,
			 in which case use the entry for the section after that, and so on.)
		 */
		let parentSection = section.parentElement.closest("section") ?? document.querySelector("#markdownBody");
		let parentTOCElement = parentSection.id == "markdownBody"
							   ? TOC
							   : TOC.querySelector(`#toc-${(CSS.escape(parentSection.id))}`).closest("li");

		let nextSection = null;
		let nextSectionTOCLink = null;
		let followingSections = childBlocksOf(parentSection).filter(child => 
			   child.tagName == "SECTION"
			&& child.compareDocumentPosition(section) == Node.DOCUMENT_POSITION_PRECEDING
		);
		do {
			nextSection = followingSections.shift();
			nextSectionTOCLink = nextSection 
								 ? parentTOCElement.querySelector(`#toc-${(CSS.escape(nextSection.id))}`) 
								 : null;
		} while (   nextSection 
				 && nextSectionTOCLink == null);
		let followingTOCElement = nextSectionTOCLink
								  ? nextSectionTOCLink.closest("li")
								  : null;

		//  Construct entry.
		let entry = newElement("LI");
		let entryText = section.id == "footnotes"
						? "Footnotes"
						: section.firstElementChild.querySelector("a").innerHTML;
		entry.innerHTML = `<a 
							class='link-self decorate-not'
							id='toc-${section.id}' 
							href='#${fixedEncodeURIComponent(section.id)}'
								>${entryText}</a>`;

		//  Get or construct the <ul> element.
		let subList = (   Array.from(parentTOCElement.childNodes).find(child => child.tagName == "UL") 
					   ?? parentTOCElement.appendChild(newElement("UL")));

		//	Insert and store.
		subList.insertBefore(entry, followingTOCElement);
		newEntries.push(entry);
	});

	//  Process the new entries to activate pop-frame spawning.
	newEntries.forEach(Extracts.addTargetsWithin);

	//	Rectify typography in new entries.
	newEntries.forEach(entry => {
		Typography.processElement(entry, Typography.replacementTypes.WORDBREAKS, true);
	});

	//	Update visibility.
	updateTOCVisibility(TOC);
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


/*************/
/* DROP CAPS */
/*************/

GW.graphicalDropCaps = {
	dropCapTypes: [
		"dropcat"
	]
};

/***************************************************************************/
/*	Returns URL of a random graphical drop-cap of the given type and letter,
	appropriate for the current mode and the viewport’s device pixel ratio.
 */
function randomDropCapURL(dropCapType, letter) {
	let mode = DarkMode.computedMode();
	let scale = valMinMax(Math.ceil(window.devicePixelRatio), 1, 2);

	let dropCapPathname = randomAsset(`/static/font/drop-cap/${dropCapType}/${mode}/${letter}-%R-small-${scale}x.png`);
	let dropCapURL = versionedAssetURL(dropCapPathname);

	return dropCapURL;
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
function addUIElement(element, options = { }) {
    let uiElementsContainer = (   document.querySelector("#ui-elements-container")
    						   ?? document.querySelector("body").appendChild(newElement("DIV", { id: "ui-elements-container" })));

	if (typeof element == "string")
		element = elementFromHTML(element);

	if (options.raiseOnHover == true) {
		element.addEventListener("mouseenter", (event) => {
			uiElementsContainer.classList.add("hover");
		});
		element.addEventListener("mouseleave", (event) => {
			uiElementsContainer.classList.remove("hover");
		});
	}

    return uiElementsContainer.appendChild(element);
}


/****************/
/* PAGE TOOLBAR */
/****************/

GW.pageToolbar = {
	maxDemos: 1,

	hoverUncollapseDelay: 400,
	unhoverCollapseDelay: 2500,
	demoCollapseDelay: 5000,

	/*	These values must be synced with CSS. Do not modify them in isolation!
		(Listed variables that correspond to each parameter are in default.css.
		 Divide these values by 1000 and specify them in seconds, e.g. a value
		 of 250 becomes a CSS value of `0.25s`.)
	 */
	collapseDuration: 250, // --GW-page-toolbar-collapse-duration
	demoCollapseDuration: 1000, // --GW-page-toolbar-slow-collapse-duration
	fadeAfterCollapseDuration: 250, // --GW-page-toolbar-fade-after-collapse-duration

	//	Do not modify these two values without updating CSS also!
	widgetFlashRiseDuration: 750, // --GW-page-toolbar-widget-flash-rise-duration
	widgetFlashFallDuration: 750, // --GW-page-toolbar-widget-flash-fall-duration
	widgetFlashStayDuration: 1000,

	toolbar: null,

	setupComplete: false,

	/*	Adds and returns page toolbar. (If page toolbar already exists, returns
		existing page toolbar.)

		NOTE: This function may run before GW.pageToolbar.setup().
	 */
	getToolbar: () => {
		return (    GW.pageToolbar.toolbar
				?? (GW.pageToolbar.toolbar = addUIElement(`<div id="page-toolbar"><div class="widgets"></div></div>`,
														  { raiseOnHover: true })));
	},

	/*	Adds a widget (which may contain buttons or whatever else) (first 
		creating it from HTML, if necessary) to the page toolbar, and returns 
		the added widget.

		NOTE: This function may run before GW.pageToolbar.setup().
	 */
	addWidget: (widget) => {
		if (typeof widget == "string")
			widget = elementFromHTML(widget);

		widget.classList.add("widget");

		//	If setup has run, update state after adding widget.
		if (GW.pageToolbar.setupComplete)
			GW.pageToolbar.updateState();

		return GW.pageToolbar.getToolbar().querySelector(".widgets").appendChild(widget);
	},

	/*	Removes a widget with the given ID and returns it.

		NOTE: This function may run before GW.pageToolbar.setup().
	 */
	removeWidget: (widgetID) => {
		let widget = GW.pageToolbar.getToolbar().querySelector(`.widget#${widgetID}`);
		if (widget == null)
			return null;

		widget.remove();

		//	If setup has run, update state after removing widget.
		if (GW.pageToolbar.setupComplete)
			GW.pageToolbar.updateState();

		return widget;
	},

	flashWidget: (widgetID, quickly = false) => {
		let widget = GW.pageToolbar.getToolbar().querySelector(`.widget#${widgetID}`);
		if (widget == null)
			return null;

		let durationFactor = quickly ? 0.25 : 1.0;

		widget.classList.add("flashing");
		setTimeout(() => {
			widget.swapClasses([ "flashing", "flashing-fade" ], 1);
			setTimeout(() => {
				widget.classList.remove("flashing-fade");
			}, GW.pageToolbar.widgetFlashFallDuration);
		}, GW.pageToolbar.widgetFlashRiseDuration + (GW.pageToolbar.widgetFlashStayDuration * durationFactor));
	},

	isCollapsed: () => {
		return GW.pageToolbar.toolbar.classList.contains("collapsed");
	},

	isTempExpanded: () => {
		return GW.pageToolbar.toolbar.classList.contains("expanded-temp");
	},

	/*	Collapse or uncollapse toolbar. (The second argument uncollapses 
		temporarily or collapses slowly. By default, uncollapse permanently and
		collapse quickly.)

		NOTE: Use only this method to collapse or uncollapse toolbar; the
		.collapse() and .uncollapse() methods are for internal use only.
	 */
	toggleCollapseState: (collapse, tempOrSlowly = false, delay) => {
		if (collapse && delay) {
			GW.pageToolbar.toolbar.collapseTimer = setTimeout(GW.pageToolbar.toggleCollapseState, delay, collapse, tempOrSlowly);
			return;
		}

		GW.pageToolbar.toolbar.classList.remove("expanded-temp");

		if (collapse == undefined) {
			if (GW.pageToolbar.isCollapsed()) {
				GW.pageToolbar.uncollapse();
			} else {
				GW.pageToolbar.collapse();
			}
		} else if (collapse == true) {
			GW.pageToolbar.collapse(tempOrSlowly);
		} else {
			GW.pageToolbar.uncollapse();
			if (tempOrSlowly)
				GW.pageToolbar.toolbar.classList.add("expanded-temp");
		}
	},

	/*	Collapse toolbar.

		(For internal use only; do not call except from .toggleCollapseState().)
	 */
	collapse: (slowly = false) => {
		clearTimeout(GW.pageToolbar.toolbar.collapseTimer);

		GW.pageToolbar.toolbar.classList.add("collapsed");

		if (slowly) {
			GW.pageToolbar.addToolbarClassesTemporarily("animating", "collapsed-slowly", 
				GW.pageToolbar.demoCollapseDuration + GW.pageToolbar.fadeAfterCollapseDuration);
		} else {
			GW.pageToolbar.addToolbarClassesTemporarily("animating", 
				GW.pageToolbar.collapseDuration + GW.pageToolbar.fadeAfterCollapseDuration);
		}
	},

	/*	Uncollapse toolbar.

		(For internal use only; do not call except from .toggleCollapseState().)
	 */
	uncollapse: () => {
		clearTimeout(GW.pageToolbar.toolbar.collapseTimer);

		GW.pageToolbar.addToolbarClassesTemporarily("animating", 
			GW.pageToolbar.collapseDuration + GW.pageToolbar.fadeAfterCollapseDuration);

		GW.pageToolbar.toolbar.classList.remove("collapsed", "collapsed-slowly");
	},

	/*	Fade toolbar to full transparency.
	 */
	fade: () => {
		GW.pageToolbar.toolbar.classList.add("faded");
	},

	/*	Un-fade toolbar from full transparency.
	 */
	unfade: () => {
		GW.pageToolbar.toolbar.classList.remove("faded");
	},

	/*	Temporarily add one or more classes to the toolbar. Takes 2 or more 
		arguments; the 1st through n-1’th argument are strings (class names),
		while the last argument is a number (the time duration after which
		the added classes shall be removed).
	 */
	addToolbarClassesTemporarily: (...args) => {
		clearTimeout(GW.pageToolbar.toolbar.tempClassTimer);

		let duration = args.last;

		GW.pageToolbar.toolbar.classList.add(...(args.slice(0, -1)));
		GW.pageToolbar.toolbar.tempClassTimer = setTimeout(() => {
			GW.pageToolbar.toolbar.classList.remove(...(args.slice(0, -1)));
		}, duration);
	},

	/*	Update layout, position, and collapse state of toolbar.
		(Called when window is scrolled or resized, and also when widgets are
		 added or removed.)
	 */
	updateState: (event) => {
		if (   event 
			&& event.type == "scroll"
			&& GW.pageToolbar.toolbar.matches(":hover") == false) {
			//	Collapse on scroll.
			let thresholdScrollDistance = (0.2 * window.innerHeight);
			if (   GW.scrollState.unbrokenUpScrollDistance   > (0.2 * window.innerHeight)
				|| GW.scrollState.unbrokenDownScrollDistance > (0.2 * window.innerHeight))
				GW.pageToolbar.toggleCollapseState(true);

			//	Fade on scroll; unfade when scrolling to top.
			let pageScrollPosition = getPageScrollPosition();
			if (   pageScrollPosition == 0
				|| pageScrollPosition == 100
				|| GW.scrollState.unbrokenUpScrollDistance       > (0.8 * window.innerHeight)) {
				GW.pageToolbar.unfade();
			} else if (GW.scrollState.unbrokenDownScrollDistance > (0.8 * window.innerHeight)) {
				GW.pageToolbar.fade();
			}
		} else {
			if (GW.isMobile()) {
				GW.pageToolbar.toolbar.classList.add("mobile", "button-labels-not");
			} else {
				GW.pageToolbar.toolbar.classList.add("desktop");
				GW.pageToolbar.toolbar.classList.remove("vertical", "horizontal", "button-labels-not");

				GW.pageToolbar.toolbar.classList.add("vertical");
			}
		}
	},

	setup: () => {
		GW.pageToolbar.toolbar = GW.pageToolbar.getToolbar();

		let startCollapsed = getSavedCount("page-toolbar-demos-count") >= GW.pageToolbar.maxDemos;
		if (startCollapsed) {
			//	Don’t collapse if hovering.
			if (GW.pageToolbar.toolbar.matches(":hover") == false)
				GW.pageToolbar.toggleCollapseState(true);
		} else {
			incrementSavedCount("page-toolbar-demos-count");
		}

		GW.pageToolbar.toolbar.append(
			newElement("BUTTON", {
				type: "button",
				title: "Collapse/expand controls",
				class: "toggle-button main-toggle-button",
				tabindex: "-1"
			}, {
				innerHTML: GW.svg("gear-solid")
			}),
			newElement("BUTTON", {
				type: "button",
				title: "Collapse controls",
				class: "toggle-button collapse-button",
				tabindex: "-1"
			}, {
				innerHTML: GW.svg("chevron-down-regular")
			})
		);

		//	Activate buttons.
		GW.pageToolbar.toolbar.querySelectorAll("button.toggle-button").forEach(button => {
			//	Toggle collapse state on click/tap.
			button.addEventListener("click", (event) => {
				//	Left-click only.
				if (event.button != 0)
					return;

				if (GW.pageToolbar.isTempExpanded()) {
					/*	Do not re-collapse if temp-expanded; instead,
						permanentize expanded state (expand-lock).
					 */
					GW.pageToolbar.toggleCollapseState(false);
				} else {
					//	Expand or collapse.
					GW.pageToolbar.toggleCollapseState();
				}
			});

			if (button.classList.contains("main-toggle-button")) {
				if (GW.isMobile()) {
					//	Unfade on tap.
					button.addEventListener("mousedown", (event) => {
						GW.pageToolbar.unfade();
					});
				} else {
					//	Unfade on hover.
					GW.pageToolbar.toolbar.addEventListener("mouseenter", (event) => {
						GW.pageToolbar.unfade();
					});

					//	Uncollapse on hover.
					onEventAfterDelayDo(button, "mouseenter", GW.pageToolbar.hoverUncollapseDelay, (event) => {
						if (GW.pageToolbar.isCollapsed())
							GW.pageToolbar.toggleCollapseState(false, true);
					}, [ "mouseleave", "mousedown" ]);

					//	Collapse on unhover.
					onEventAfterDelayDo(GW.pageToolbar.toolbar, "mouseleave", GW.pageToolbar.unhoverCollapseDelay, (event) => {
						if (GW.pageToolbar.isTempExpanded())
							GW.pageToolbar.toggleCollapseState(true);
					}, "mouseenter");
				}
			}
		});

		//	Set initial state.
		GW.pageToolbar.updateState();

		doWhenPageLoaded(() => {
			/*	Slowly collapse toolbar shortly after page load (if it’s not
				already collapsed).
			 */
			if (startCollapsed == false) {
				requestAnimationFrame(() => {
					Array.from(GW.pageToolbar.getToolbar().querySelector(".widgets").children).forEach(widget => {
						let order = parseInt(getComputedStyle(widget).order);
						setTimeout(GW.pageToolbar.flashWidget, order * GW.pageToolbar.widgetFlashRiseDuration * 4/3, widget.id, true);
					});

					//	Don’t collapse if hovering.
					if (GW.pageToolbar.toolbar.matches(":hover") == false)
						GW.pageToolbar.toggleCollapseState(true, true, GW.pageToolbar.demoCollapseDelay);
				});
			}

			//	Update toolbar state on scroll.
			addScrollListener(GW.pageToolbar.updateState, "updatePageToolbarStateListener", { defer: true });

			//	Update toolbar state on window resize.
			addWindowResizeListener(GW.pageToolbar.updateState, "updatePageToolbarStateListener", { defer: true });
		});

		GW.pageToolbar.setupComplete = true;
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
        + GW.svg("arrow-up-to-line-light")
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


/*******************/
/* FLOATING HEADER */
/*******************/

GW.floatingHeader = {
    minimumYOffset: 0,

    maxChainLength: GW.isMobile() ? 3 : 6,

    maxHeaderHeight: GW.isMobile() ? 60 : 360,

    chainLinkClasses: {
        "…": "ellipsis",
        "header": "page-title"
    },

    currentTrail: [ ],

    /*  Scroll down enough to make whatever’s under the header visible.
     */
    adjustScrollTop: () => {
    	if (GW.isMobile() == false)
    		return;

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
            let index = 0;
            chainLinks.forEach(link => {
            	link.addActivateEvent(GW.floatingHeader.linkInChainClicked);
            	let span = wrapElement(link, "link", "SPAN", false, true);
            	span.style.setProperty("--link-index", index++);
            });

			if (GW.isMobile() == false)
				Extracts.addTargetsWithin(GW.floatingHeader.linkChain);

            //  Constrain header height.
            if (   GW.floatingHeader.header.offsetHeight > GW.floatingHeader.maxHeaderHeight
                && maxChainLength > 1)
                GW.floatingHeader.updateState(event, maxChainLength - 1);
            else
                GW.floatingHeader.currentTrail = trail;
        }
    },

    getTrail: () => {
    	let headerOffset = GW.isMobile() ? GW.floatingHeader.header.offsetHeight : 10;
        let element = document.elementFromPoint(window.innerWidth / 2, headerOffset + 10);

        if (   element.tagName == "SECTION"
        	|| element == GW.floatingHeader.markdownBody)
            return (GW.floatingHeader.currentTrail.length == 0
                    ? [ "header" ]
                    : GW.floatingHeader.currentTrail);

		if (GW.floatingHeader.firstSection == null)
			return [ "header" ];

        if (GW.floatingHeader.firstSection.compareDocumentPosition(element) & Node.DOCUMENT_POSITION_PRECEDING)
            return [ "header" ];

        if (   GW.floatingHeader.markdownBody.contains(element) == false
            && GW.floatingHeader.pageMainElement.contains(element) == true)
            return GW.floatingHeader.currentTrail;

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

		//	No floating header on desktop /index.
		if (   GW.isMobile() == false
			&& /\/(index)?$/.test(location.pathname))
			return;

		//	Inject header.
		GW.floatingHeader.header = addUIElement(  `<div id="floating-header" class="hidden">`
												+ `<div class="link-chain"></div>`
												+ `<div class="scroll-indicator"></div>`
												+ `</div>`);

		//	Designate desktop version of header.
		if (GW.isMobile() == false)
			GW.floatingHeader.header.classList.add("desktop");

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

doWhenPageLoaded(GW.floatingHeader.setup);


/******************************/
/* GENERAL ACTIVITY INDICATOR */
/******************************/

doWhenBodyExists(() => {
    GW.activityIndicator = addUIElement(`<div id="general-activity-indicator" class="on">`
        + GW.svg("spinner-regular")
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


/*	Popup/floating footnotes to avoid readers needing to scroll to the end of
	the page to see any footnotes; see
	http://ignorethecode.net/blog/2010/04/20/footnotes/ for details.

	Original author:  Lukas Mathis (2010-04-20)
	License: public domain ("And some people have asked me about a license for
	this piece of code. I think it’s far too short to get its own license, so
	I’m relinquishing any copyright claims. Consider the code to be public
	domain. No attribution is necessary.")
 */

Popups = {
	/**********/
	/*	Config.
	 */
    popupContainerID: "popup-container",
    popupContainerParentSelector: "html",
    popupContainerZIndex: "10000",

    popupBreathingRoomX: 12.0,
    popupBreathingRoomY: 8.0,
    popupBreathingRoomYTight: -4.0,

    popupTriggerDelay: 750,
    popupFadeoutDelay: 100,
    popupFadeoutDuration: 250,

	/******************/
	/*	Implementation.
	 */

	//	Used in: Popups.containingDocumentForTarget
	rootDocument: document,

	popupFadeTimer: false,
	popupDespawnTimer: false,
	popupSpawnTimer: false,
	popupContainer: null,

	popupBeingDragged: null,

	hoverEventsActive: true,

	cleanup: () => {
		GWLog("Popups.cleanup", "popups.js", 1);

        //  Remove popups container.
        document.querySelectorAll(`#${Popups.popupContainerID}`).forEach(element => element.remove());
		Popups.popupContainer = null;

		//  Remove Escape key event listener.
		document.removeEventListener("keyup", Popups.keyUp);

		//	Remove scroll listener.
		removeScrollListener("updatePopupsEventStateScrollListener");
		//	Remove popup-spawn event handler.
		GW.notificationCenter.removeHandlerForEvent("Popups.popupDidSpawn", Popups.addDisableHoverEventsOnScrollListenerOnPopupSpawned);
		//	Remove mousemove listener.
		window.removeEventListener("mousemove", Popups.windowMouseMove);
	},

	setup: () => {
		GWLog("Popups.setup", "popups.js", 1);

        //  Run cleanup.
        Popups.cleanup();

        //  Inject popups container.
        let popupContainerParent = document.querySelector(Popups.popupContainerParentSelector);
        if (popupContainerParent == null) {
            GWLog("Popup container parent element not found. Exiting.", "popups.js", 1);
            return;
        }
        Popups.popupContainer = popupContainerParent.appendChild(newElement("DIV", {
        	id: Popups.popupContainerID,
        	class: "popup-container",
        	style: `z-index: ${Popups.popupContainerZIndex};`
        }));

		//	Add window resize listener, to reposition pinned popups.
		addWindowResizeListener(Popups.repositionPopupsOnWindowResize = (event) => {
			Popups.allSpawnedPopups().forEach(popup => {
				Popups.setPopupViewportRect(popup, popup.viewportRect, { clampPositionToScreen: true });
			});
		}, "repositionPopupsOnWindowResizeListener", { defer: true });

		//  Add Escape key event listener.
		document.addEventListener("keyup", Popups.keyUp);

		//	Add scroll listener, to disable hover on scroll.
		addScrollListener(Popups.disableHoverEventsOnScroll = (event) => {
			Popups.hoverEventsActive = false;
		}, "disablePopupHoverEventsOnScrollListener");
		/*	Add event handler to add scroll listener to spawned popups, to
			disable hover events when scrolling within a popup.
		 */
		GW.notificationCenter.addHandlerForEvent("Popups.popupDidSpawn", Popups.addDisableHoverEventsOnScrollListenerOnPopupSpawned = (info) => {
			addScrollListener(Popups.disableHoverEventsOnScroll, null, null, info.popup.scrollView);
		});
		//	Add mousemove listener, to enable hover on mouse move.
		window.addEventListener("mousemove", Popups.windowMouseMove = (event) => {
			Popups.hoverEventsActive = true;
		});

		GW.notificationCenter.fireEvent("Popups.setupDidComplete");
	},

	//	Called by: extracts.js
	addTargetsWithin: (contentContainer, targets, prepareFunction, targetPrepareFunction, targetRestoreFunction) => {
		GWLog("Popups.addTargetsWithin", "popups.js", 1);

		if (typeof contentContainer == "string")
			contentContainer = document.querySelector(contentContainer);

		if (contentContainer == null)
			return;

		//	Get all targets.
		contentContainer.querySelectorAll(targets.targetElementsSelector).forEach(target => {
			if (   target.matches(targets.excludedElementsSelector)
				|| target.closest(targets.excludedContainerElementsSelector) != null) {
				target.classList.toggle("no-popup", true);
				return;
			}

			//  Apply the test function to the target.
			if (targets.testTarget(target) == false) {
				target.classList.toggle("no-popup", true);
				targetRestoreFunction(target);
				return;
			}

			//	Bind mouseenter/mouseleave/mousedown events.
			target.addEventListener("mouseenter", Popups.targetMouseEnter);
			target.addEventListener("mouseleave", Popups.targetMouseLeave);
			target.addEventListener("mousedown", Popups.targetMouseDown);

			//  Set prepare function.
			target.preparePopup = prepareFunction;

			//	Set target restore function.
			target.restoreTarget = targetRestoreFunction;

			//  Run any custom processing.
			if (targetPrepareFunction)
				targetPrepareFunction(target);

			//  Mark target as spawning a popup.
			target.classList.toggle("spawns-popup", true);
		});
	},

	//	Called by: extracts.js
	removeTargetsWithin: (contentContainer, targets, targetRestoreFunction = null) => {
		GWLog("Popups.removeTargetsWithin", "popups.js", 1);

		if (typeof contentContainer == "string")
			contentContainer = document.querySelector(contentContainer);

		if (contentContainer == null)
			return;

		contentContainer.querySelectorAll(targets.targetElementsSelector).forEach(target => {
			if (   target.matches(targets.excludedElementsSelector)
				|| target.closest(targets.excludedContainerElementsSelector) != null) {
				target.classList.toggle("no-popup", false);
				return;
			}

			//  Apply the test function to the target.
			if (targets.testTarget(target) == false) {
				target.classList.toggle("no-popup", false);
				return;
			}

			//	Unbind existing mouseenter/mouseleave/mousedown events, if any.
			target.removeEventListener("mouseenter", Popups.targetMouseEnter);
			target.removeEventListener("mouseleave", Popups.targetMouseLeave);
			target.removeEventListener("mousedown", Popups.targetMouseDown);

			//  Clear timers for target.
			Popups.clearPopupTimers(target);

			//  Remove spawned popup for target, if any.
			if (target.popup)
				Popups.despawnPopup(target.popup);

			//  Unset popup prepare function.
			target.preparePopup = null;

			//  Un-mark target as spawning a popup.
			target.classList.toggle("spawns-popup", false);

			//  Run any custom processing.
			targetRestoreFunction = targetRestoreFunction ?? target.restoreTarget;
			if (targetRestoreFunction)
				targetRestoreFunction(target);
		});
	},

	/*******************/
	/*  General helpers.
	 */

	popupContainerIsVisible: () => {
		return (Popups.popupContainer.style.visibility != "hidden");
	},

	//	Called by: extracts-options.js
	hidePopupContainer: () => {
		GWLog("Popups.hidePopupContainer", "popups.js", 3);

		if (Popups.popupContainer) {
			Popups.popupContainer.style.visibility = "hidden";
			Popups.allSpawnedPopups().forEach(popup => {
				Popups.addClassesToPopFrame(popup, "hidden");
			});
		} else {
			GW.notificationCenter.addHandlerForEvent("Popups.setDidComplete", (info) => {
				Popups.hidePopupContainer();
			});
		}
	},

	//	Called by: extracts-options.js
	unhidePopupContainer: () => {
		GWLog("Popups.unhidePopupContainer", "popups.js", 3);

		if (Popups.popupContainer) {
			Popups.popupContainer.style.visibility = "";
			Popups.allSpawnedPopups().forEach(popup => {
				Popups.removeClassesFromPopFrame(popup, "hidden");
			});
		} else {
			GW.notificationCenter.addHandlerForEvent("Popups.setDidComplete", (info) => {
				Popups.unhidePopupContainer();
			});
		}
	},

	updatePageScrollState: () => {
		GWLog("Popups.updatePageScrollState", "popups.js", 2);

		if (Popups.allSpawnedPopups().findIndex(popup => Popups.popupIsMaximized(popup)) == -1)
			togglePageScrolling(true);
		else
			togglePageScrolling(false);
	},

	containingDocumentForTarget: (target) => {
		let containingPopup = Popups.containingPopFrame(target);
		return (containingPopup ? containingPopup.document : Popups.rootDocument);
	},

	allSpawnedPopFrames: () => {
		return Popups.allSpawnedPopups();
	},

	//	Called by: extracts.js
	allSpawnedPopups: () => {
		if (Popups.popupContainer == null)
			return [ ];

		return Array.from(Popups.popupContainer.children).filter(popup => (popup.classList.contains("fading") == false));
	},

	//	Called by: extracts.js
	containingPopFrame: (element) => {
		let shadowBody = element.closest(".shadow-body");
		if (shadowBody)
			return shadowBody.popup;

		return element.closest(".popup");
	},

	addClassesToPopFrame: (popup, ...args) => {
		popup.classList.add(...args);
		popup.body.classList.add(...args);
	},

	removeClassesFromPopFrame: (popup, ...args) => {
		popup.classList.remove(...args);
		popup.body.classList.remove(...args);
	},

	/****************************************/
	/*  Visibility of elements within popups.
	 */

	/*	Returns true if the given element is currently visible.
	 */
	//	Called by: extracts-content.js
	isVisible: (element) => {
		let containingPopup = Popups.containingPopFrame(element);
		return (containingPopup ? isWithinRect(element, containingPopup.getBoundingClientRect()) : isOnScreen(element));
	},

	//	Called by: extracts.js
	scrollElementIntoViewInPopFrame: (element, alwaysRevealTopEdge = false) => {
		let popup = Popups.containingPopFrame(element);

		let elementRect = element.getBoundingClientRect();
		let popupBodyRect = popup.body.getBoundingClientRect();
		let popupScrollViewRect = popup.scrollView.getBoundingClientRect();

		let bottomBound = alwaysRevealTopEdge ? elementRect.top : elementRect.bottom;
		if (   popup.scrollView.scrollTop                              >= elementRect.top    - popupBodyRect.top
			&& popup.scrollView.scrollTop + popupScrollViewRect.height <= bottomBound - popupBodyRect.top)
			return;

		popup.scrollView.scrollTop = elementRect.top - popupBodyRect.top;
	},

	/*******************************/
	/*  Popup spawning & despawning.
	 */

	newPopup: (target) => {
		GWLog("Popups.newPopup", "popups.js", 2);

		let popup = newElement("DIV");
		popup.classList.add("popup", "popframe");
		popup.innerHTML = `<div class="popframe-scroll-view"><div class="popframe-content-view"></div></div>`;
		popup.scrollView = popup.querySelector(".popframe-scroll-view");
		popup.contentView = popup.querySelector(".popframe-content-view");

		popup.contentView.attachShadow({ mode: "open" });
		popup.document = popup.contentView.shadowRoot;
		popup.document.appendChild(newElement("DIV"));
		popup.document.body = popup.body = popup.shadowBody = popup.document.firstElementChild;
		popup.body.classList.add("popframe-body", "popup-body", "shadow-body");

		let styleReset = newElement("STYLE");
		styleReset.innerHTML = `.shadow-body { all: initial; }`;
		popup.document.insertBefore(styleReset, popup.body);

		popup.body.popup = popup.contentView.popup = popup.scrollView.popup = popup;

		popup.titleBarContents = [ ];

		popup.uiElementsContainer = popup.appendChild(newElement("DIV", { "class": "popframe-ui-elements-container" }));

		//  Give the popup a reference to the target.
		popup.spawningTarget = target;

		return popup;
	},

	//	Called by: extracts.js
	//	Called by: extracts-content.js
	setPopFrameContent: (popup, content) => {
		if (content) {
			popup.body.replaceChildren(content);

			return true;
		} else {
			return false;
		}
	},

	//	Called by: extracts.js
	//	Called by: extracts-annotations.js
	spawnPopup: (target, spawnPoint) => {
		GWLog("Popups.spawnPopup", "popups.js", 2);

		//  Prevent spawn attempts before setup complete.
		if (Popups.popupContainer == null)
			return;

		//	Set wait cursor.
		Popups.setWaitCursorForTarget(target);

		//  Despawn existing popup, if any.
		if (target.popup)
			Popups.despawnPopup(target.popup);

		/*	Once this popup is spawned, despawn all non-pinned popups not in this
			popup’s stack.
		 */
		GW.notificationCenter.addHandlerForEvent("Popups.popupDidSpawn", (info) => {
			Popups.allSpawnedPopups().forEach(spawnedPopup => {
				if (   Popups.popupIsPinned(spawnedPopup) == false
					&& target.popup.popupStack.indexOf(spawnedPopup) == -1)
					Popups.despawnPopup(spawnedPopup);
			});
		}, {
			once: true,
			condition: (info) => (info.popup == target.popup)
		});

		//  Create the new popup.
		target.popFrame = target.popup = Popups.newPopup(target);

		//  Prepare the newly created popup for spawning.
		if ((target.popFrame = target.popup = target.preparePopup(target.popup)) == null) {
			//	Reset cursor to normal.
			Popups.clearWaitCursorForTarget(target);

			return;
		}

		//  If title bar contents are provided, add a title bar (if needed).
		if (   target.popup.titleBar == null
			&& target.popup.titleBarContents.length > 0)
			Popups.addTitleBarToPopup(target.popup);

		if (target.popup.parentElement == Popups.popupContainer) {
			//  If the popup is an existing popup, just bring it to the front.
			Popups.bringPopupToFront(target.popup);
		} else {
			//	Otherwise, inject the popup into the page.
			Popups.injectPopup(target.popup);
		}

		//  Position the popup appropriately with respect to the target.
		Popups.positionPopup(target.popup, spawnPoint);

		//  Mark target as having an active popup associated with it.
		target.classList.add("popup-open");

		//  Fire notification event.
		GW.notificationCenter.fireEvent("Popups.popupDidSpawn", { popup: target.popup });

		requestAnimationFrame(() => {
			//	Disable rendering progress indicator (spinner).
			if (target.popup)
				Popups.removeClassesFromPopFrame(target.popup, "rendering");

			//	Reset cursor to normal.
			Popups.clearWaitCursorForTarget(target);
		});
	},

	injectPopup: (popup) => {
		GWLog("Popups.injectPopup", "popups.js", 2);

		//  Add popup to a popup stack.
		if (popup.popupStack == null) {
			let parentPopup = Popups.containingPopFrame(popup.spawningTarget);
			popup.popupStack = parentPopup ? parentPopup.popupStack : [ ];
		} else {
			popup.popupStack.remove(popup);
		}
		popup.popupStack.push(popup);

		//	Set rendering progress indicator (spinner).
		Popups.addClassesToPopFrame(popup, "rendering");

		//  Inject popup into page.
		Popups.popupContainer.appendChild(popup);

		//  Bring popup to front.
		Popups.bringPopupToFront(popup);

		//  Cache border width.
		popup.borderWidth = parseFloat(getComputedStyle(popup).borderLeftWidth);

		//	Add event listeners.
		popup.addEventListener("click", Popups.popupClicked);
		popup.addEventListener("mouseenter", Popups.popupMouseEnter);
		popup.addEventListener("mouseleave", Popups.popupMouseLeave);
		popup.addEventListener("mouseout", Popups.popupMouseOut);
		popup.addEventListener("mousedown", Popups.popupMouseDown);

		//  We define the mousemove listener here in order to capture `popup`.
		popup.addEventListener("mousemove", Popups.popupMouseMove = (event) => {
			GWLog("Popups.popupMouseMove", "popups.js", 3);

			if (   event.target == popup
				&& Popups.popupBeingDragged == null
				&& Popups.popupIsResizeable(popup)) {
				//  Mouse position is relative to the popup’s coordinate system.
				let edgeOrCorner = Popups.edgeOrCorner(popup, {
					x: event.clientX - popup.viewportRect.left,
					y: event.clientY - popup.viewportRect.top
				});

				//  Set cursor.
				document.documentElement.style.cursor = Popups.cursorForPopupBorder(edgeOrCorner);
			}
		});
	},

	attachPopupToTarget: (popup) => {
		GWLog("Popups.attachPopupToTarget", "popups.js", 2);

		Popups.clearPopupTimers(popup.spawningTarget);

        popup.spawningTarget.classList.add("popup-open");
        popup.spawningTarget.popup = popup;
        popup.spawningTarget.popFrame = popup;
	},

	//	Called by: extracts.js
	detachPopupFromTarget: (popup) => {
		GWLog("Popups.detachPopupFromTarget", "popups.js", 2);

		Popups.clearPopupTimers(popup.spawningTarget);

        popup.spawningTarget.classList.remove("popup-open");
        popup.spawningTarget.popup = null;
        popup.spawningTarget.popFrame = null;
	},

    despawnPopup: (popup) => {
		GWLog("Popups.despawnPopup", "popups.js", 2);

		if (popup.isDespawned)
			return;

		GW.notificationCenter.fireEvent("Popups.popupWillDespawn", { popup: popup });

		//  Detach popup from its spawning target.
		Popups.detachPopupFromTarget(popup);

		//  Remove popup from the page.
		popup.remove();

		//  Remove popup from its popup stack.
		popup.popupStack.remove(popup);
		popup.popupStack = null;

		//	Mark popup as despawned.
		popup.isDespawned = true;

		//  Update z-indexes of all popups.
		Popups.updatePopupsZOrder();

		//  Enable/disable main document scrolling.
		Popups.updatePageScrollState();

		//	Reset cursor to normal.
		requestAnimationFrame(() => {
			Popups.clearWaitCursorForTarget(popup.spawningTarget);
		});

        document.activeElement.blur();
    },

	getPopupAncestorStack: (popup) => {
		let indexOfPopup = popup.popupStack.indexOf(popup);
		if (indexOfPopup != -1) {
			return popup.popupStack.slice(0, indexOfPopup + 1);
		} else {
			let parentPopup = Popups.containingPopFrame(popup.spawningTarget);
			return ((parentPopup && parentPopup.popupStack)
				    ? Popups.getPopupAncestorStack(parentPopup)
				    : [ ]);
		}
	},

	isSpawned: (popup) => {
		return (   popup
				&& popup.parentElement
				&& popup.classList.contains("fading") == false);
	},

	/********************/
	/*  Popup collapsing.
	 */
	popupIsCollapsed: (popup) => {
		return popup.classList.contains("collapsed");
	},

	collapseOrUncollapsePopup: (popup) => {
		GWLog("Popups.collapseOrUncollapsePopup", "popups.js", 2);

		if (Popups.popupIsCollapsed(popup)) {
			Popups.uncollapsePopup(popup);
		} else {
			Popups.collapsePopup(popup);
		}

		//  Cache the viewport rect.
		popup.viewportRect = popup.getBoundingClientRect();
	},

	collapsePopup: (popup) => {
		GWLog("Popups.collapsePopup", "popups.js", 3);

		//  Update class.
		Popups.addClassesToPopFrame(popup, "collapsed");

		//  Save and unset height, if need be.
		if (popup.style.height) {
			popup.dataset.previousHeight = popup.style.height;
			popup.style.height = "";
		}

		//  Pin popup.
		Popups.pinPopup(popup);

		//  Clear timers.
		Popups.clearPopupTimers(popup.spawningTarget);

		//  Update title bar buttons states (if any).
		if (popup.titleBar)
			popup.titleBar.updateState();
	},

	uncollapsePopup: (popup) => {
		GWLog("Popups.uncollapsePopup", "popups.js", 3);

		//  Update class.
		Popups.removeClassesFromPopFrame(popup, "collapsed");

		//  Restore height, if need be.
		if (popup.dataset.previousHeight) {
			if (Popups.popupIsPinned(popup) == true)
				popup.style.height = popup.dataset.previousHeight;

			//  Delete saved height.
			delete popup.dataset["previousHeight"];
		}

		//  Clear timers.
		Popups.clearPopupTimers(popup.spawningTarget);

		//  Update title bar buttons states (if any).
		if (popup.titleBar)
			popup.titleBar.updateState();
	},

	/********************************************************/
	/*  Popup pinning/unpinning, zooming/tiling, & restoring.
	 */

	/*  Popup tiling control keys.
	 */
	popupTilingControlKeys: (localStorage.getItem("popup-tiling-control-keys") || ""),
	//	This function is currently unused (but should be used in the future).
	//		—SA, 2022-02-01
	setPopupTilingControlKeys: (keystring) => {
		GWLog("Popups.setPopupTilingControlKeys", "popups.js", 1);

		Popups.popupTilingControlKeys = keystring || "aswdqexzfcv";
		localStorage.setItem("popup-tiling-control-keys", Popups.popupTilingControlKeys);
	},

	popupIsResizeable: (popup) => {
		return (   Popups.popupIsPinned(popup)
				&& (   Popups.popupAllowsHorizontalResize(popup)
					|| Popups.popupAllowsVerticalResize(popup)));
	},

	popupAllowsHorizontalResize: (popup) => {
		return (popup.classList.contains("no-resize-width") == false);
	},

	popupAllowsVerticalResize: (popup) => {
		return (   popup.classList.contains("no-resize-height") == false
				&& Popups.popupIsCollapsed(popup) == false);
	},

	popupIsZoomed: (popup) => {
		return popup.classList.contains("zoomed");
	},

	popupIsZoomedToPlace: (popup, place) => {
		return (   popup.classList.contains("zoomed")
				&& popup.classList.contains(place));
	},

	popupIsMaximized: (popup) => {
		return (popup.classList.contains("zoomed") && popup.classList.contains("full"));
	},

	popupWasRestored: (popup) => {
		return popup.classList.contains("restored");
	},

	popupIsPinned: (popup) => {
		return popup.classList.contains("pinned");
	},

	popupWasUnpinned: (popup) => {
		return popup.classList.contains("unpinned");
	},

	zoomPopup: (popup, place) => {
		GWLog("Popups.zoomPopup", "popups.js", 2);

		//  If popup isn’t already zoomed, save position.
		if (Popups.popupIsZoomed(popup) == false) {
			popup.dataset.previousXPosition = popup.viewportRect.left;
			popup.dataset.previousYPosition = popup.viewportRect.top;
		}

		//  If the popup is collapsed, expand it.
		if (Popups.popupIsCollapsed(popup))
			Popups.uncollapsePopup(popup);

		//  Update classes.
		Popups.removeClassesFromPopFrame(popup, "restored", ...(Popups.titleBarComponents.popupPlaces));
		Popups.addClassesToPopFrame(popup, "zoomed", place);

		//  Viewport width must account for vertical scroll bar.
		let viewportWidth = document.documentElement.offsetWidth;
		let viewportHeight = window.innerHeight;
		switch (place) {
			case "top-left":
				popup.zoomToX = 0.0;
				popup.zoomToY = 0.0;
				break;
			case "top":
				popup.zoomToX = 0.0;
				popup.zoomToY = 0.0;
				break;
			case "top-right":
				popup.zoomToX = viewportWidth / 2.0;
				popup.zoomToY = 0.0;
				break;
			case "left":
				popup.zoomToX = 0.0;
				popup.zoomToY = 0.0;
				break;
			case "full":
				popup.zoomToX = 0.0;
				popup.zoomToY = 0.0;
				break;
			case "right":
				popup.zoomToX = viewportWidth / 2.0;
				popup.zoomToY = 0.0;
				break;
			case "bottom-left":
				popup.zoomToX = 0.0;
				popup.zoomToY = viewportHeight / 2.0;
				break;
			case "bottom":
				popup.zoomToX = 0.0;
				popup.zoomToY = viewportHeight / 2.0;
				break;
			case "bottom-right":
				popup.zoomToX = viewportWidth / 2.0;
				popup.zoomToY = viewportHeight / 2.0;
				break;
		}

		//  Update popup position.
		Popups.positionPopup(popup);

		//  Update popup size.
		popup.style.maxWidth = "unset";
		popup.style.maxHeight = "unset";
		switch (place) {
			case "full":
				popup.style.width = "100%";
				popup.style.height = "100vh";
				break;
			case "left":
			case "right":
				popup.style.width = "50%";
				popup.style.height = "100vh";
				break;
			case "top":
			case "bottom":
				popup.style.width = "100%";
				popup.style.height = "50vh";
				break;
			case "top-left":
			case "top-right":
			case "bottom-left":
			case "bottom-right":
				popup.style.width = "50%";
				popup.style.height = "50vh";
				break;
		}
		popup.scrollView.style.maxHeight = "calc(100% - var(--popup-title-bar-height))";

		//  Pin popup.
		Popups.pinPopup(popup);

		//  Clear timers.
		Popups.clearPopupTimers(popup.spawningTarget);

		//  Enable/disable main document scrolling.
		Popups.updatePageScrollState();

		//  Update title bar buttons states (if any).
		if (popup.titleBar)
			popup.titleBar.updateState();
	},

	restorePopup: (popup) => {
		GWLog("Popups.restorePopup", "popups.js", 2);

		//  Update classes.
		Popups.removeClassesFromPopFrame(popup, "zoomed", "resized", ...(Popups.titleBarComponents.popupPlaces));
		Popups.addClassesToPopFrame(popup, "restored");

		//  Update popup size.
		popup.style.width = "";
		popup.style.height = "";
		popup.style.maxWidth = "";
		popup.style.maxHeight = "";
		popup.scrollView.style.maxHeight = "";

		//  Update popup position.
		Popups.positionPopup(popup);

		//  Clear timers.
		Popups.clearPopupTimers(popup.spawningTarget);

		//  Enable/disable main document scrolling.
		Popups.updatePageScrollState();

		//  Update title bar buttons states (if any).
		if (popup.titleBar)
			popup.titleBar.updateState();
	},

	pinOrUnpinPopup: (popup) => {
		GWLog("Popups.pinOrUnpinPopup", "popups.js", 2);

		if (Popups.popupIsPinned(popup) == true) {
			Popups.unpinPopup(popup);
		} else {
			Popups.pinPopup(popup);
		}
	},

	pinPopup: (popup) => {
		GWLog("Popups.pinPopup", "popups.js", 2);

		popup.swapClasses([ "pinned", "unpinned" ], 0);
		Popups.positionPopup(popup);
		popup.popupStack.remove(popup);
		Popups.detachPopupFromTarget(popup);

		popup.titleBar.updateState();
	},

	unpinPopup: (popup) => {
		GWLog("Popups.unpinPopup", "popups.js", 2);

		popup.swapClasses([ "pinned", "unpinned" ], 1);
		Popups.positionPopup(popup);
		popup.popupStack.push(popup);
		Popups.attachPopupToTarget(popup);

		popup.titleBar.updateState();
	},

	/******************/
	/*  Popup resizing.
	 */

	popupWasResized: (popup) => {
		return popup.classList.contains("resized");
	},

	edgeOrCorner: (popup, relativeMousePos) => {
		if (Popups.popupAllowsHorizontalResize(popup) == false) {
			let cornerHandleSize = popup.borderWidth;

			       if (relativeMousePos.y < cornerHandleSize) {
				return "edge-top";
			} else if (relativeMousePos.y > popup.viewportRect.height - cornerHandleSize) {
				return "edge-bottom";
			} else {
				return "";
			}
		} else if (Popups.popupAllowsVerticalResize(popup) == false) {
			let cornerHandleSize = popup.borderWidth;

			       if (relativeMousePos.x < cornerHandleSize) {
				return "edge-left";
			} else if (relativeMousePos.x > popup.viewportRect.width - cornerHandleSize) {
				return "edge-right";
			} else {
				return "";
			}
		} else {
			//  Make corner drag areas big enough to make a decent mouse target.
			let cornerHandleSize = Math.min(20.0, (Math.min(popup.viewportRect.width, popup.viewportRect.height) / 3.0));

				   if (   relativeMousePos.x < cornerHandleSize
					   && relativeMousePos.y < cornerHandleSize) {
				return "corner-top-left";
			} else if (   relativeMousePos.x > popup.viewportRect.width - cornerHandleSize
					   && relativeMousePos.y > popup.viewportRect.height - cornerHandleSize) {
				return "corner-bottom-right";
			} else if (   relativeMousePos.x < cornerHandleSize
					   && relativeMousePos.y > popup.viewportRect.height - cornerHandleSize) {
				return "corner-bottom-left";
			} else if (   relativeMousePos.x > popup.viewportRect.width - cornerHandleSize
					   && relativeMousePos.y < cornerHandleSize) {
				return "corner-top-right";
			} else if (relativeMousePos.x < cornerHandleSize) {
				return "edge-left";
			} else if (relativeMousePos.x > popup.viewportRect.width - cornerHandleSize) {
				return "edge-right";
			} else if (relativeMousePos.y < cornerHandleSize) {
				return "edge-top";
			} else if (relativeMousePos.y > popup.viewportRect.height - cornerHandleSize) {
				return "edge-bottom";
			} else {
				return "";
			}
		}
	},

	cursorForPopupBorder: (edgeOrCorner) => {
		switch (edgeOrCorner) {
		case "edge-top":
		case "edge-bottom":
			return "row-resize";
		case "edge-left":
		case "edge-right":
			return "col-resize";
		case "corner-top-left":
		case "corner-bottom-right":
			return "nwse-resize";
		case "corner-top-right":
		case "corner-bottom-left":
			return "nesw-resize";
		default:
			return "";
		}
	},

	/*******************/
	/*  Popup title bar.
	 */

	/*  Add title bar to a popup which has a populated .titleBarContents.
	 */
	addTitleBarToPopup: (popup) => {
		GWLog("Popups.addTitleBarToPopup", "popups.js", 2);

		//  Set class ‘has-title-bar’ on the popup.
		popup.classList.add("has-title-bar");

		//  Create and inject the title bar element.
		popup.titleBar = newElement("DIV");
		popup.titleBar.classList.add("popframe-title-bar");
		popup.titleBar.title = "Drag popup by title bar to reposition; double-click title bar to collapse (hold Option/Alt to collapse all)";
		popup.insertBefore(popup.titleBar, popup.firstElementChild);

		//  Add the provided title bar contents (buttons, title, etc.).
		popup.titleBarContents.forEach(element => {
			popup.titleBar.appendChild(element);

			if (element.buttonAction)
				element.addActivateEvent(element.buttonAction);

			//  Add popup-positioning submenu to zoom button.
			if (   element.classList.contains("zoom-button")
				&& element.submenuEnabled)
				Popups.titleBarComponents.addSubmenuToButton(element, "zoom-button-submenu", Popups.titleBarComponents.popupZoomButtons());
		});

		//  Add state-updating function.
		popup.titleBar.updateState = () => {
			popup.titleBar.querySelectorAll("button").forEach(button => {
				if (button.updateState)
					button.updateState();
			});
		};

		//  Add event listeners for dragging the popup by the title bar.
		popup.titleBar.addEventListener("mousedown", Popups.popupTitleBarMouseDown);
		popup.titleBar.addEventListener("mouseup", Popups.popupTitleBarMouseUp);

		//  Add double-click event listener for collapsing/uncollapsing the popup.
		popup.titleBar.addEventListener("dblclick", Popups.popupTitleBarDoubleClicked);
	},

	/*  Elements and methods related to popup title bars.
	 */
	titleBarComponents: {
		//  The standard positions for a popup to zoom to.
		popupPlaces: [ "top-left", "top", "top-right", "left", "full", "right", "bottom-left", "bottom", "bottom-right" ],

		getButtonIcon: (buttonType) => {
			let icon = Popups.titleBarComponents.buttonIcons[buttonType];
			return icon.startsWith("<") ? icon : GW.svg(icon);
// 			return GW.svg(Popups.titleBarComponents.buttonIcons[buttonType]);
		},

		/*  Icons for various popup title bar buttons.
			(Values are keys for GW.svg().)
		 */
		buttonIcons: {
			"close": "times-square-regular",
			"zoom": "arrows-maximize-solid",
			"restore": "compress-solid",
			"pin": "thumbtack-regular",
			"unpin": "thumbtack-solid",
			"options": "gear-solid",
			"zoom-top-left": "expand-arrows-up-left",
			"zoom-top": "expand-arrows-up",
			"zoom-top-right": "expand-arrows-up-right",
			"zoom-left": "expand-arrows-left",
			"zoom-full": "arrows-maximize-solid",
			"zoom-right": "expand-arrows-right",
			"zoom-bottom-left": "expand-arrows-down-left",
			"zoom-bottom": "expand-arrows-down",
			"zoom-bottom-right": "expand-arrows-down-right"
		},

		//  Tooltip text for various popup title bar icons.
		buttonTitles: {
			"close": "Close this popup (hold Option/Alt to close all)",
			"zoom": "Maximize this popup",
			"restore": "Restore this popup to normal size and position",
			"pin": "Pin this popup to the screen (hold Option/Alt to pin all)",
			"unpin": "Un-pin this popup from the screen (hold Option/Alt to pin all)",
			"options": "Show options",
			"zoom-top-left": "Place this popup in the top-left quarter of the screen",
			"zoom-top": "Place this popup on the top half of the screen",
			"zoom-top-right": "Place this popup in the top-right quarter of the screen",
			"zoom-left": "Place this popup on the left half of the screen",
			"zoom-right": "Place this popup on the right half of the screen",
			"zoom-full": "Expand this popup to fill the screen",
			"zoom-bottom-left": "Place this popup in the bottom-left quarter of the screen",
			"zoom-bottom": "Place this popup on the bottom half of the screen",
			"zoom-bottom-right": "Place this popup in the bottom-right quarter of the screen"
		},

		//  A generic button, with no icon or tooltip text.
		genericButton: () => {
			let button = newElement("BUTTON");
			button.classList.add("popframe-title-bar-button");

			button.buttonAction = (event) => { event.stopPropagation(); };

			return button;
		},

		//  Close button.
		closeButton: () => {
			let button = Popups.titleBarComponents.genericButton();
			button.classList.add("close-button");

			button.innerHTML = Popups.titleBarComponents.getButtonIcon("close");
			button.title = Popups.titleBarComponents.buttonTitles["close"];

			button.buttonAction = (event) => {
				event.stopPropagation();

				if (event.altKey == true) {
					Popups.allSpawnedPopups().forEach(popup => {
						Popups.despawnPopup(popup);
					});
				} else {
					Popups.despawnPopup(Popups.containingPopFrame(event.target));
				}
			};

			return button;
		},

		//  Zoom button (with submenu).
		zoomButton: () => {
			let button = Popups.titleBarComponents.genericButton();
			button.classList.add("zoom-button", "zoom");

			button.defaultHTML = Popups.titleBarComponents.getButtonIcon("zoom");
			button.alternateHTML = Popups.titleBarComponents.getButtonIcon("restore");
			button.innerHTML = button.defaultHTML;

			button.defaultTitle = Popups.titleBarComponents.buttonTitles["zoom"];
			button.alternateTitle = Popups.titleBarComponents.buttonTitles["restore"];
			button.title = button.defaultTitle;

			button.buttonAction = (event) => {
				event.stopPropagation();

				let popup = Popups.containingPopFrame(button);

				if (button.classList.contains("zoom")) {
					Popups.zoomPopup(popup, "full");
				} else {
					Popups.restorePopup(popup);
				}
			};

			button.updateState = () => {
				let popup = Popups.containingPopFrame(button);

				let alternateStateEnabled = (Popups.popupIsZoomed(popup) || Popups.popupWasResized(popup));

				button.innerHTML = alternateStateEnabled ? button.alternateHTML : button.defaultHTML;
				button.title = alternateStateEnabled ? button.alternateTitle : button.defaultTitle;

				button.swapClasses([ "zoom", "restore" ], (alternateStateEnabled ? 1 : 0));

				if (button.submenuEnabled == true) {
					button.submenu.querySelectorAll(".submenu-button").forEach(submenuButton => {
						submenuButton.updateState();
					});
				}
			};

			button.enableSubmenu = () => {
				button.submenuEnabled = true;
				return button;
			};

			return button;
		},

		//  Zoom buttons (to be put into zoom button submenu).
		popupZoomButtons: () => {
			return Popups.titleBarComponents.popupPlaces.map(place => {
				let button = Popups.titleBarComponents.genericButton();

				button.classList.add("submenu-button", "zoom-button", place);

				button.defaultHTML = Popups.titleBarComponents.getButtonIcon(`zoom-${place}`);
				button.alternateHTML = Popups.titleBarComponents.getButtonIcon("restore");
				button.innerHTML = button.defaultHTML;

				button.defaultTitle = Popups.titleBarComponents.buttonTitles[`zoom-${place}`];
				button.alternateTitle = Popups.titleBarComponents.buttonTitles["restore"];
				button.title = button.defaultTitle;

				button.buttonAction = (event) => {
					event.stopPropagation();

					let popup = Popups.containingPopFrame(button);

					if (button.classList.contains(`zoom-${place}`)) {
						Popups.zoomPopup(popup, place);
					} else {
						Popups.restorePopup(popup);
					}
				};

				button.updateState = () => {
					let popup = Popups.containingPopFrame(button);

					let alternateStateEnabled = Popups.popupIsZoomedToPlace(popup, place);

					button.innerHTML = alternateStateEnabled ? button.alternateHTML : button.defaultHTML;
					button.title = alternateStateEnabled ? button.alternateTitle : button.defaultTitle;

					button.swapClasses([ `zoom-${place}`, "restore" ], (alternateStateEnabled ? 1 : 0));
				};

				return button;
			});
		},

		//  Pin button.
		pinButton: () => {
			let button = Popups.titleBarComponents.genericButton();
			button.classList.add("pin-button", "pin");

			button.defaultHTML = Popups.titleBarComponents.getButtonIcon("pin");
			button.alternateHTML = Popups.titleBarComponents.getButtonIcon("unpin");
			button.innerHTML = button.defaultHTML;

			button.defaultTitle = Popups.titleBarComponents.buttonTitles["pin"];
			button.alternateTitle = Popups.titleBarComponents.buttonTitles["unpin"];
			button.title = button.defaultTitle;

			button.buttonAction = (event) => {
				event.stopPropagation();

				let popup = Popups.containingPopFrame(button);

				if (event.altKey == true) {
					let action = Popups.popupIsPinned(popup) ? "unpinPopup" : "pinPopup";
					Popups.allSpawnedPopups().forEach(popup => {
						Popups[action](popup);
					});
				} else {
					Popups.pinOrUnpinPopup(popup);
				}
			};

			button.updateState = () => {
				let popup = Popups.containingPopFrame(button);

				button.innerHTML = Popups.popupIsPinned(popup) ? button.alternateHTML : button.defaultHTML;
				button.title = Popups.popupIsPinned(popup) ? button.alternateTitle : button.defaultTitle;

				button.swapClasses([ "pin", "unpin" ], (Popups.popupIsPinned(popup) ? 1 : 0));
			};

			return button;
		},

		//  Options button (does nothing by default).
		optionsButton: () => {
			let button = Popups.titleBarComponents.genericButton();
			button.classList.add("options-button");

			button.innerHTML = Popups.titleBarComponents.getButtonIcon("options");
			button.title = Popups.titleBarComponents.buttonTitles["options"];

			return button;
		},

		/*  Add a submenu of the given class and with given buttons to a button.
		 */
		addSubmenuToButton: (button, submenuClass, submenuButtons) => {
			let popup = Popups.containingPopFrame(button);

			button.classList.add("has-submenu");

			button.submenu = newElement("DIV");
			button.submenu.classList.add("submenu", submenuClass);

			popup.titleBar.insertBefore(button.submenu, button.nextElementSibling);

			submenuButtons.forEach(submenuButton => {
				button.submenu.appendChild(submenuButton);
				if (submenuButton.buttonAction)
					submenuButton.addActivateEvent(submenuButton.buttonAction);
			});
		},
	},

	/******************/
	/*	Optional parts.
	 */

	addPartToPopFrame: (popup, part) => {
		popup.append(part);
	},

	/************************/
	/*	Optional UI elements.
	 */

	addUIElementsToPopFrame: (popup, ...args) => {
		popup.uiElementsContainer.append(...args);
	},

	/*********************/
	/*	Popups z-ordering.
	 */

	updatePopupsZOrder: () => {
		GWLog("Popups.updatePopupsZOrder", "popups.js", 3);

		let allPopups = Popups.allSpawnedPopups();
		allPopups.sort((a, b) => parseInt(a.style.zIndex) - parseInt(b.style.zIndex));
		for (let i = 0; i < allPopups.length; i++)
			allPopups[i].style.zIndex = i + 1;

		//  Focus the front-most popup.
		Popups.focusPopup(Popups.frontmostPopup());
	},

	popupIsFrontmost: (popup) => {
		return (parseInt(popup.style.zIndex) == Popups.allSpawnedPopups().length);
	},

	frontmostPopup: () => {
		let allPopups = Popups.allSpawnedPopups();
		return allPopups.find(popup => parseInt(popup.style.zIndex) == allPopups.length);
	},

	bringPopupToFront: (popup) => {
		GWLog("Popups.bringPopupToFront", "popups.js", 3);

		//  If it’s already at the front, do nothing.
		if (Popups.popupIsFrontmost(popup))
			return;

		//  Set z-index.
		popup.style.zIndex = (Popups.allSpawnedPopups().length + 1);

		//  Update z-indexes of all popups.
		Popups.updatePopupsZOrder();
	},

	/******************/
	/*  Popup focusing.
	 */

	popupIsFocused: (popup) => {
		return popup.classList.contains("focused");
	},

	focusedPopup: () => {
		return Popups.allSpawnedPopups().find(popup => Popups.popupIsFocused(popup));
	},

	focusPopup: (popup) => {
		GWLog("Popups.focusPopup", "popups.js", 3);

		//  Un-focus any focused popups.
		Popups.allSpawnedPopups().forEach(spawnedPopup => {
			Popups.removeClassesFromPopFrame(spawnedPopup, "focused");
		});

		//  Focus the given popup.
		if (popup)
			Popups.addClassesToPopFrame(popup, "focused");
	},

	/*********************/
	/*  Popup positioning.
	 */

	/*	Returns full viewport rect for popup and all auxiliary elements
		(footers, etc.).
	 */
	getPopupViewportRect: (popup) => {
		return rectUnion(popup.getBoundingClientRect(), ...(Array.from(popup.children).map(x => x.getBoundingClientRect())));
	},

	//	See also: extracts.js
	preferSidePositioning: (target) => {
		return target.preferSidePositioning ? target.preferSidePositioning() : false;
	},

	/*	Returns current popup position. (Usable only after popup is positioned.)
	 */
	popupPosition: (popup) => {
		return {
			x: parseInt(popup.style.left),
			y: parseInt(popup.style.top)
		};
	},

	positionPopup: (popup, spawnPoint, tight = false) => {
		GWLog("Popups.positionPopup", "popups.js", 2);

		let target = popup.spawningTarget;
		if (spawnPoint)
			target.lastMouseEnterLocation = spawnPoint;
		else if (target.lastMouseEnterLocation)
			spawnPoint = target.lastMouseEnterLocation;
		else
			return;

		/*	When the target’s bounding rect is composed of multiple client rects
			(as when the target is a link that wraps across a line break), we
			must select the right rect, to prevent the popup from spawning far
			away from the cursor.
		 */
		let targetViewportRect =    Array.from(target.getClientRects()).find(rect => pointWithinRect(spawnPoint, rect))
								 || target.getBoundingClientRect();

		//  Wait for the “naive” layout to be completed, and then...
		requestAnimationFrame(() => {
			//	Clear popup position.
			popup.style.left = "";
			popup.style.top = "";

			/*  This is the width and height of the popup, as already determined
				by the layout system, and taking into account the popup’s content,
				and the max-width, min-width, etc., CSS properties.
			 */
			let popupIntrinsicRect = Popups.getPopupViewportRect(popup);
			let popupIntrinsicWidth = popupIntrinsicRect.width;
			let popupIntrinsicHeight = popupIntrinsicRect.height;

			let provisionalPopupXPosition = 0.0;
			let provisionalPopupYPosition = 0.0;

			let offToTheSide = false;
			let popupSpawnYOriginForSpawnAbove = targetViewportRect.top
											   - (tight ? Popups.popupBreathingRoomYTight : Popups.popupBreathingRoomY);
			let popupSpawnYOriginForSpawnBelow = targetViewportRect.bottom
											   + (tight ? Popups.popupBreathingRoomYTight : Popups.popupBreathingRoomY);
			if (   Popups.containingPopFrame(target)
				|| Popups.preferSidePositioning(target)) {
				/*  The popup is a nested popup, or the target specifies that it
					prefers to have popups spawned to the side; we try to put
					the popup off to the left or right.
				 */
				offToTheSide = true;
			}

			provisionalPopupYPosition = spawnPoint.y - ((spawnPoint.y / window.innerHeight) * popupIntrinsicHeight);
			if (provisionalPopupYPosition < 0.0)
				provisionalPopupYPosition = 0.0;

			//  Determine whether to put the popup off to the right, or left.
			if (  targetViewportRect.right
				+ Popups.popupBreathingRoomX
				+ popupIntrinsicWidth
				  <= document.documentElement.offsetWidth) {
				//  Off to the right.
				provisionalPopupXPosition = targetViewportRect.right + Popups.popupBreathingRoomX;
			} else if (  targetViewportRect.left
					   - Popups.popupBreathingRoomX
					   - popupIntrinsicWidth
						 >= 0) {
				//  Off to the left.
				provisionalPopupXPosition = targetViewportRect.left - popupIntrinsicWidth - Popups.popupBreathingRoomX;
			} else {
				//  Not off to either side, in fact.
				offToTheSide = false;
			}

			/*  Can the popup fit above the target? If so, put it there.
				Failing that, can it fit below the target? If so, put it there.
			 */
			if (offToTheSide == false) {
				if (  popupSpawnYOriginForSpawnAbove
					- popupIntrinsicHeight
					  >= 0) {
					//  Above.
					provisionalPopupYPosition = popupSpawnYOriginForSpawnAbove - popupIntrinsicHeight;
				} else if (  popupSpawnYOriginForSpawnBelow
						   + popupIntrinsicHeight
						     <= window.innerHeight) {
					//  Below.
					provisionalPopupYPosition = popupSpawnYOriginForSpawnBelow;
				} else {
					//  The popup does not fit above or below!
					if (tight == false) {
						//	Let’s try and pack it in more tightly...
						Popups.positionPopup(popup, null, true);
						return;
					} else {
						/*	... or, failing that, we will have to put it off to
							the right after all.
						 */
						offToTheSide = true;
					}
				}
			}

			if (offToTheSide == false) {
				/*  Place popup off to the right (and either above or below),
					as per the previous block of code.
				 */
				provisionalPopupXPosition = spawnPoint.x + Popups.popupBreathingRoomX;
			}

			/*  Does the popup extend past the right edge of the container?
				If so, move it left, until its right edge is flush with
				the container’s right edge.
			 */
			if (  provisionalPopupXPosition
				+ popupIntrinsicWidth
				  > document.documentElement.offsetWidth) {
				//  We add 1.0 here to prevent wrapping due to rounding.
				provisionalPopupXPosition -= (provisionalPopupXPosition + popupIntrinsicWidth - document.documentElement.offsetWidth + 1.0);
			}

			/*  Now (after having nudged the popup left, if need be),
				does the popup extend past the *left* edge of the container?
				Make its left edge flush with the container's left edge.
			 */
			if (provisionalPopupXPosition < 0)
				provisionalPopupXPosition = 0;

			//  Special cases for maximizing/restoring and pinning/unpinning.
			let getPositionToRestore = (popup) => {
				xPos = parseFloat(popup.dataset.previousXPosition);
				yPos = parseFloat(popup.dataset.previousYPosition);

				//  Clear saved position.
				delete popup.dataset.previousXPosition;
				delete popup.dataset.previousYPosition;

				Popups.removeClassesFromPopFrame(popup, "restored");

				return [ xPos, yPos ];
			};
			if (Popups.popupIsZoomed(popup)) {
				provisionalPopupXPosition = popup.zoomToX;
				provisionalPopupYPosition = popup.zoomToY;
			} else if (Popups.popupIsPinned(popup) == true) {
				if (Popups.popupWasRestored(popup)) {
					[ provisionalPopupXPosition, provisionalPopupYPosition ] = getPositionToRestore(popup);
				} else {
					provisionalPopupXPosition = popup.viewportRect.left;
					provisionalPopupYPosition = popup.viewportRect.top;
				}
			} else {
				if (Popups.popupWasUnpinned(popup)) {
					provisionalPopupXPosition = popup.viewportRect.left;
					provisionalPopupYPosition = popup.viewportRect.top;

					Popups.removeClassesFromPopFrame(popup, "unpinned");
				} else if (Popups.popupWasRestored(popup)) {
					[ provisionalPopupXPosition, provisionalPopupYPosition ] = getPositionToRestore(popup);
				}
			}

			//  Set only position, not size.
			Popups.setPopupViewportRect(popup, new DOMRect(provisionalPopupXPosition, provisionalPopupYPosition, 0, 0));

			//  Cache the viewport rect.
			popup.viewportRect = popup.getBoundingClientRect();

			document.activeElement.blur();
		});
	},

	setPopupViewportRect: (popup, rect, options = { }) => {
		GWLog("Popups.setPopupViewportRect", "popups.js", 3);

		if (options.clampPositionToScreen) {
			//  Viewport width must account for vertical scroll bar.
			let viewportWidth = document.documentElement.offsetWidth;
			let viewportHeight = window.innerHeight;

			//	Clamp position to screen, keeping size constant.
			rect.x = valMinMax(rect.x,
							   0,
							   viewportWidth - (rect.width || popup.viewportRect.width));
			rect.y = valMinMax(rect.y,
							   0,
							   viewportHeight - (rect.height || popup.viewportRect.height));
		}

		if (Popups.popupIsPinned(popup) == false) {
            let popupContainerViewportRect = Popups.popupContainer.getBoundingClientRect();
			rect.x -= popupContainerViewportRect.left;
			rect.y -= popupContainerViewportRect.top;
		}

		popup.style.position = Popups.popupIsPinned(popup) ? "fixed" : "";

		popup.style.left = `${(Math.round(rect.x))}px`;
		popup.style.top = `${(Math.round(rect.y))}px`;

		if (   rect.width > 0
			&& rect.height > 0) {
			popup.style.maxWidth = "unset";
			popup.style.maxHeight = "unset";

			popup.style.width = `${(Math.round(rect.width))}px`;
			popup.style.height = `${(Math.round(rect.height))}px`;

			popup.scrollView.style.maxHeight = "calc(100% - var(--popup-title-bar-height))";
		}
	},

	/****************/
	/*	Popup timers.
	 */

    clearPopupTimers: (target) => {
	    GWLog("Popups.clearPopupTimers", "popups.js", 3);

		if (target.popup)
			Popups.removeClassesFromPopFrame(target.popup, "fading");

        clearTimeout(target.popupFadeTimer);
        clearTimeout(target.popupDespawnTimer);
        clearTimeout(target.popupSpawnTimer);
    },

	setPopupSpawnTimer: (target, event) => {
		GWLog("Popups.setPopupSpawnTimer", "popups.js", 2);

		let popupTriggerDelay = target.specialPopupTriggerDelay != null
								? (typeof target.specialPopupTriggerDelay == "function"
								   ? target.specialPopupTriggerDelay()
								   : target.specialPopupTriggerDelay)
								: Popups.popupTriggerDelay;
		target.popupSpawnTimer = setTimeout(() => {
			GWLog("Popups.popupSpawnTimer fired", "popups.js", 2);

			//	Spawn the popup.
			Popups.spawnPopup(target, { x: event.clientX, y: event.clientY });
		}, popupTriggerDelay);
	},

    setPopupFadeTimer: (target) => {
		GWLog("Popups.setPopupFadeTimer", "popups.js", 2);

        target.popupFadeTimer = setTimeout(() => {
			GWLog("popupFadeTimer fired", "popups.js", 2);

			Popups.setPopupDespawnTimer(target);
        }, Popups.popupFadeoutDelay);
    },

    setPopupDespawnTimer: (target) => {
		GWLog("Popups.setPopupDespawnTimer", "popups.js", 2);

		Popups.addClassesToPopFrame(target.popup, "fading");
		target.popupDespawnTimer = setTimeout(() => {
			GWLog("popupDespawnTimer fired", "popups.js", 2);

			Popups.despawnPopup(target.popup);
		}, Popups.popupFadeoutDuration);
    },

	/********************************/
	/*	Popup progress UI indicators.
	 */

	setWaitCursorForTarget: (target) => {
		GWLog("Popups.setWaitCursorForTarget", "popups.js", 2);

		document.documentElement.style.cursor = "progress";
		target.style.cursor = "progress";
		if (target.popup)
			target.popup.style.cursor = "progress";
	},

	clearWaitCursorForTarget: (target) => {
		GWLog("Popups.clearWaitCursorForTarget", "popups.js", 3);

		document.documentElement.style.cursor = "";
		target.style.cursor = "";
		if (target.popup)
			target.popup.style.cursor = "";
	},

	/*******************/
	/*  Event listeners.
	 */

   /*	The “user moved mouse out of popup” mouseleave event.
    */
    //	Added by: Popups.injectPopup
	popupMouseLeave: (event) => {
		GWLog("Popups.popupMouseLeave", "popups.js", 2);

		if (Popups.popupBeingDragged)
			return;

		if (Popups.popupContainerIsVisible() == false)
			return;

		Popups.getPopupAncestorStack(event.target).reverse().forEach(popupInStack => {
			Popups.clearPopupTimers(popupInStack.spawningTarget);
			Popups.setPopupFadeTimer(popupInStack.spawningTarget);
		});
	},

	/*	The “user moved mouse back into popup” mouseenter event.
	 */
	//	Added by: Popups.injectPopup
	popupMouseEnter: (event) => {
		GWLog("Popups.popupMouseEnter", "popups.js", 2);

		Popups.getPopupAncestorStack(event.target).forEach(popupInStack => {
			Popups.clearPopupTimers(popupInStack.spawningTarget);
		});
	},

	/*  The “user clicked in body of popup” event.
	 */
	//	Added by: Popups.injectPopup
    popupClicked: (event) => {
		GWLog("Popups.popupClicked", "popups.js", 2);

		let popup = Popups.containingPopFrame(event.target);

		if (   Popups.popupIsFrontmost(popup) == false
			&& event.metaKey == false)
			Popups.bringPopupToFront(popup);

		event.stopPropagation();

		Popups.clearPopupTimers(popup.spawningTarget);
    },

	/*  The popup mouse down event (for resizing by dragging an edge/corner).
	 */
	//	Added by: Popups.injectPopup
	popupMouseDown: (event) => {
		GWLog("Popups.popupMouseDown", "popups.js", 2);

		//  Prevent other events from triggering.
		event.stopPropagation();

		//  Get the containing popup.
		let popup = Popups.containingPopFrame(event.target);

		/*  Make sure we’re clicking on the popup (ie. its edge) and not
			on any of the popup’s contained elements; that this is a
			left-click; and that the popup is pinned or zoomed.
		 */
		if (   event.target != popup
			|| event.button != 0
			|| Popups.popupIsResizeable(popup) == false)
			return;

		//  Bring the popup to the front.
		if (event.metaKey == false)
			Popups.bringPopupToFront(popup);

		//  Prevent clicks from doing anything other than what we want.
		event.preventDefault();

		//  Determine direction of resizing.
		let edgeOrCorner = Popups.edgeOrCorner(popup, {
			x: event.clientX - popup.viewportRect.left,
			y: event.clientY - popup.viewportRect.top
		});

		//	Perhaps we cannot resize in this direction?
		if (edgeOrCorner == "")
			return;

		//  Mark popup as currently being resized.
		Popups.addClassesToPopFrame(popup, "resizing");

		//  Save position, if need be.
		if (   !("previousXPosition" in popup.dataset)
			&& !("previousYPosition" in popup.dataset)) {
			popup.dataset.previousXPosition = popup.viewportRect.left;
			popup.dataset.previousYPosition = popup.viewportRect.top;
		}

		//  Point where the drag began.
		let dragStartMouseCoordX = event.clientX;
		let dragStartMouseCoordY = event.clientY;

		//  Popup initial rect.
		let newPopupViewportRect = DOMRect.fromRect(popup.viewportRect);

		/*  Add the mouse up event listener (to window, not the popup, because
			the drag might end anywhere, due to animation lag).
		 */
		window.addEventListener("mouseup", Popups.popupResizeMouseUp);

		//  Viewport width must account for vertical scroll bar.
		let viewportWidth = document.documentElement.offsetWidth;
		let viewportHeight = window.innerHeight;

		//  Popup minimum width/height.
		let popupMinWidth = parseFloat(getComputedStyle(popup).minWidth);
		let popupMinHeight = parseFloat(getComputedStyle(popup).minHeight);

		//  The mousemove event that triggers the continuous resizing.
		window.onmousemove = (event) => {
			window.popupBeingResized = popup;

			Popups.removeClassesFromPopFrame(popup, ...(Popups.titleBarComponents.popupPlaces));
			Popups.addClassesToPopFrame(popup, "resized");

			let deltaX = event.clientX - dragStartMouseCoordX;
			let deltaY = event.clientY - dragStartMouseCoordY;

			let resizeTop = () => {
				newPopupViewportRect.y = valMinMax(popup.viewportRect.y + deltaY, 0, popup.viewportRect.bottom - popupMinHeight);
				newPopupViewportRect.height = popup.viewportRect.bottom - newPopupViewportRect.y;
			};
			let resizeBottom = () => {
				newPopupViewportRect.height = valMinMax(popup.viewportRect.height + deltaY, popupMinHeight, viewportHeight - popup.viewportRect.y);
			};
			let resizeLeft = () => {
				newPopupViewportRect.x = valMinMax(popup.viewportRect.x + deltaX, 0, popup.viewportRect.right - popupMinWidth);
				newPopupViewportRect.width = popup.viewportRect.right - newPopupViewportRect.x;
			};
			let resizeRight = () => {
				newPopupViewportRect.width = valMinMax(popup.viewportRect.width + deltaX, popupMinWidth, viewportWidth - popup.viewportRect.x);
			};

			switch (edgeOrCorner) {
				case "edge-top":
					resizeTop();
					break;
				case "edge-bottom":
					resizeBottom();
					break;
				case "edge-left":
					resizeLeft();
					break;
				case "edge-right":
					resizeRight();
					break;
				case "corner-top-left":
					resizeTop();
					resizeLeft();
					break;
				case "corner-bottom-right":
					resizeBottom();
					resizeRight();
					break;
				case "corner-top-right":
					resizeTop();
					resizeRight();
					break;
				case "corner-bottom-left":
					resizeBottom();
					resizeLeft();
					break;
			}

			Popups.setPopupViewportRect(popup, newPopupViewportRect);
		};
	},

	/*  The resize-end mouseup event.
	 */
	//	Added by: Popups.popupMouseDown
	popupResizeMouseUp: (event) => {
		GWLog("Popups.popupResizeMouseUp", "popups.js", 2);

		event.stopPropagation();

		window.onmousemove = null;

		//  Reset cursor to normal.
		document.documentElement.style.cursor = "";

		let popup = window.popupBeingResized;
		if (popup) {
			Popups.removeClassesFromPopFrame(popup, "resizing");

			if (Popups.popupWasResized(popup))
				popup.titleBar.updateState();

			//  Cache the viewport rect.
			popup.viewportRect = popup.getBoundingClientRect();
		}
		window.popupBeingResized = null;

		window.removeEventListener("mouseup", Popups.popupResizeMouseUp);
	},

	/*  The popup mouseout event.
	 */
	//	Added by: Popups.injectPopup
	popupMouseOut: (event) => {
		GWLog("Popups.popupMouseOut", "popups.js", 3);

		//  Reset cursor.
		if (   Popups.popupBeingDragged == null
			&& event.target.style.cursor == "")
			document.documentElement.style.cursor = "";
	},

	/*  The popup title bar mouseup event.
	 */
	//	Added by: Popups.addTitleBarToPopup
	popupTitleBarMouseDown: (event) => {
		GWLog("Popups.popupTitleBarMouseDown", "popups.js", 2);

		//  Prevent other events from triggering.
		event.stopPropagation();

		//  Get the containing popup.
		let popup = Popups.containingPopFrame(event.target);

		//  Bring the popup to the front.
		if (event.metaKey == false)
			Popups.bringPopupToFront(popup);

		//  We only want to do anything on left-clicks.
		if (event.button != 0)
			return;

		//  Also do nothing if the click is on a title bar button.
		if (event.target.closest(".popframe-title-bar-button"))
			return;

		//  Prevent clicks from doing anything other than what we want.
		event.preventDefault();

		//  Mark popup as grabbed.
		Popups.addClassesToPopFrame(popup, "grabbed");

		//  Change cursor to “grabbing hand”.
		document.documentElement.style.cursor = "grabbing";

		/*  If the mouse-down event is on the popup title (and the title
			is a link).
		 */
		popup.linkDragTarget = event.target.closest("a");

		/*  Deal with edge case where drag to screen bottom ends up
			with the mouse-up event happening in the popup body.
		 */
		popup.removeEventListener("click", Popups.popupClicked);

		//  Point where the drag began.
		let dragStartMouseCoordX = event.clientX;
		let dragStartMouseCoordY = event.clientY;

		//  Popup initial position.
		let newPopupViewportRect = DOMRect.fromRect(popup.viewportRect);
		//  Do not change popup size.
		newPopupViewportRect.width = 0;
		newPopupViewportRect.height = 0;

		//  Add the drag-end mouseup listener.
		window.addEventListener("mouseup", Popups.popupDragMouseUp);

		//  We define the mousemove listener here to capture variables.
		window.onmousemove = (event) => {
			Popups.popupBeingDragged = popup;

			//  Mark popup as being dragged.
			Popups.addClassesToPopFrame(popup, "dragging");

			//  If dragging by the title, disable its normal click handler.
			if (popup.linkDragTarget)
				popup.linkDragTarget.onclick = (event) => { return false; };

			//  Current drag vector relative to mouse starting position.
			newPopupViewportRect.x = popup.viewportRect.x + (event.clientX - dragStartMouseCoordX);
			newPopupViewportRect.y = popup.viewportRect.y + (event.clientY - dragStartMouseCoordY);

			//  Set new viewport rect; clamp to screen.
			Popups.setPopupViewportRect(popup, newPopupViewportRect, { clampPositionToScreen: true });
		};
	},

	/*  The mouseup event that ends a popup drag-to-move.
	 */
	//	Added by: Popups.popupTitleBarMouseDown
	popupDragMouseUp: (event) => {
		GWLog("Popups.popupDragMouseUp", "popups.js", 2);

		//  Prevent other events from triggering.
		event.stopPropagation();

		//  Remove the mousemove handler.
		window.onmousemove = null;

		//  Reset cursor to normal.
		document.documentElement.style.cursor = "";

		let popup = Popups.popupBeingDragged;
		if (popup) {
			Popups.removeClassesFromPopFrame(popup, "grabbed", "dragging");

			//  Re-enable clicking on the title.
			if (popup.linkDragTarget) {
				requestAnimationFrame(() => {
					popup.linkDragTarget.onclick = null;
					popup.linkDragTarget = null;
				});
			}

			//  Cache the viewport rect.
			popup.viewportRect = popup.getBoundingClientRect();

			//  Ensure that the click listener isn’t fired at once.
			requestAnimationFrame(() => {
				popup.addEventListener("click", Popups.popupClicked);
			});

			/*  If the drag of a non-pinned popup ended outside the
				popup (possibly outside the viewport), treat this
				as mousing out of the popup.
			 */
			if (   (   event.target.closest == null
				    || Popups.containingPopFrame(event.target) == null)
				&& Popups.popupIsPinned(popup) == false) {
				Popups.getPopupAncestorStack(popup).reverse().forEach(popupInStack => {
					Popups.clearPopupTimers(popupInStack.spawningTarget);
					Popups.setPopupFadeTimer(popupInStack.spawningTarget);
				});
			}

			//  Pin popup.
			Popups.pinPopup(popup);
		}
		Popups.popupBeingDragged = null;

		//  Remove the listener (ie. we only want this fired once).
		window.removeEventListener("mouseup", Popups.popupDragMouseUp);
	},

	/*  The popup title bar mouseup event.
	 */
	//	Added by: Popups.addTitleBarToPopup
	popupTitleBarMouseUp: (event) => {
		GWLog("Popups.popupTitleBarMouseUp", "popups.js", 2);

		Popups.containingPopFrame(event.target).classList.toggle("grabbed", false);
	},

	/*  The popup title bar double-click event.
	 */
	//	Added by: Popups.addTitleBarToPopup
	popupTitleBarDoubleClicked: (event) => {
		GWLog("Popups.popupTitleBarDoubleClicked", "popups.js", 2);

		let popup = Popups.containingPopFrame(event.target);

		if (event.altKey == true) {
			let expand = Popups.popupIsCollapsed(Popups.containingPopFrame(event.target));
			Popups.allSpawnedPopups().forEach(popup => {
				if (expand)
					Popups.uncollapsePopup(popup);
				else
					Popups.collapsePopup(popup);
			});
		} else {
			Popups.collapseOrUncollapsePopup(popup);
		}
	},

	/*	The target mouseenter event.
	 */
	//	Added by: Popups.addTargetsWithin
	targetMouseEnter: (event) => {
		GWLog("Popups.targetMouseEnter", "popups.js", 2);

		if (Popups.popupBeingDragged)
			return;

		if (Popups.hoverEventsActive == false)
			return;

		//	Stop the countdown to un-pop the popup.
		Popups.clearPopupTimers(event.target);

		if (event.target.popup == null) {
			//  Start the countdown to pop up the popup (if not already spawned).
			Popups.setPopupSpawnTimer(event.target, event);
		} else {
			/*  If already spawned, just bring the popup to the front and
				re-position it.
			 */
			Popups.bringPopupToFront(event.target.popup);
			Popups.positionPopup(event.target.popup, { x: event.clientX, y: event.clientY });
		}
	},

	/*	The target mouseleave event.
	 */
	//	Added by: Popups.addTargetsWithin
	targetMouseLeave: (event) => {
		GWLog("Popups.targetMouseLeave", "popups.js", 2);

		event.target.lastMouseEnterEvent = null;

		Popups.clearPopupTimers(event.target);

		if (event.target.popup)
			Popups.setPopupFadeTimer(event.target);
	},

    /*	The “user (left- or right-) clicked target” mousedown event.
     */
    //	Added by: Popups.addTargetsWithin
	targetMouseDown: (event) => {
		GWLog("Popups.targetMouseDown", "popups.js", 2);

		if (Popups.popupBeingDragged)
			return;

		if (   event.target.closest(".popframe-ui-elements-container")
			&& event.button == 0)
			return;

		/*	Unlike ‘mouseenter’ and ‘mouseleave’, ‘mousedown’ behaves like
			‘mouseover’/‘mouseout’ in that it attaches to the innermost element,
			which might not be our spawning target (but instead some descendant
			element); we must find the actual spawning target.
		 */
		let target = event.target.closest(".spawns-popup");

		//	Cancel spawning of popups from the target.
		Popups.clearPopupTimers(target);

		//	Despawn any (non-pinned) popup already spawned from the target.
		if (target.popup)
			Popups.despawnPopup(target.popup);
	},

	/*  The keyup event.
	 */
	//	Added by: Popups.setup
	keyUp: (event) => {
		GWLog("Popups.keyUp", "popups.js", 3);
		let allowedKeys = [ "Escape", "Esc", ...(Popups.popupTilingControlKeys.split("")) ];
		if (   allowedKeys.includes(event.key) == false
			|| Popups.allSpawnedPopups().length == 0)
			return;

		event.preventDefault();

		switch(event.key) {
			case "Escape":
			case "Esc":
				if (   Popups.popupContainerIsVisible()
					&& Popups.allSpawnedPopups().length > 0)
					Popups.despawnPopup(Popups.focusedPopup());
				break;
			case Popups.popupTilingControlKeys.substr(0,1):
				Popups.zoomPopup(Popups.focusedPopup(), "left");
				break;
			case Popups.popupTilingControlKeys.substr(1,1):
				Popups.zoomPopup(Popups.focusedPopup(), "bottom");
				break;
			case Popups.popupTilingControlKeys.substr(2,1):
				Popups.zoomPopup(Popups.focusedPopup(), "top");
				break;
			case Popups.popupTilingControlKeys.substr(3,1):
				Popups.zoomPopup(Popups.focusedPopup(), "right");
				break;
			case Popups.popupTilingControlKeys.substr(4,1):
				Popups.zoomPopup(Popups.focusedPopup(), "top-left");
				break;
			case Popups.popupTilingControlKeys.substr(5,1):
				Popups.zoomPopup(Popups.focusedPopup(), "top-right");
				break;
			case Popups.popupTilingControlKeys.substr(6,1):
				Popups.zoomPopup(Popups.focusedPopup(), "bottom-right");
				break;
			case Popups.popupTilingControlKeys.substr(7,1):
				Popups.zoomPopup(Popups.focusedPopup(), "bottom-left");
				break;
			case Popups.popupTilingControlKeys.substr(8,1):
				Popups.zoomPopup(Popups.focusedPopup(), "full");
				break;
			case Popups.popupTilingControlKeys.substr(9,1):
				Popups.pinOrUnpinPopup(Popups.focusedPopup());
				break;
			case Popups.popupTilingControlKeys.substr(10,1):
				Popups.collapseOrUncollapsePopup(Popups.focusedPopup());
				break;
			default:
				break;
		}
	}
};

GW.notificationCenter.fireEvent("Popups.didLoad");
/*	Popup/floating footnotes to avoid readers needing to scroll to the end of
	the page to see any footnotes; see
	https://ignorethecode.net/blog/2010/04/20/footnotes/ for details.
Original author:  Lukas Mathis (2010-04-20)
License: public domain ("And some people have asked me about a license for this piece of code. I think it’s far too short to get its own license, so I’m relinquishing any copyright claims. Consider the code to be public domain. No attribution is necessary.")
	*/

Popins = {
	/******************/
	/*	Implementation.
		*/

	//	Used in: Popins.containingDocumentForTarget
	rootDocument: document,

	spawnedPopins: [ ],

	//	Called by: Popins.setup
	cleanup: () => {
		GWLog("Popins.cleanup", "popins.js", 1);

		//  Remove all remnant popins.
		Popins.allSpawnedPopins().forEach(popin => {
			Popins.removePopin(popin);
		});

		//  Remove Escape key event listener.
		document.removeEventListener("keyup", Popins.keyUp);
	},

	//	Called by: popins.js (doWhenPageLoaded)
	setup: () => {
		GWLog("Popins.setup", "popins.js", 1);

        //  Run cleanup.
        Popins.cleanup();

		//  Add Escape key event listener.
		document.addEventListener("keyup", Popins.keyUp);

		GW.notificationCenter.fireEvent("Popins.setupDidComplete");
	},

	//	Called by: extracts.js
	addTargetsWithin: (contentContainer, targets, prepareFunction, targetPrepareFunction, targetRestoreFunction) => {
		if (typeof contentContainer == "string")
			contentContainer = document.querySelector(contentContainer);

		if (contentContainer == null)
			return;

		//	Get all targets.
		contentContainer.querySelectorAll(targets.targetElementsSelector).forEach(target => {
			if (   target.matches(targets.excludedElementsSelector)
				|| target.closest(targets.excludedContainerElementsSelector) != null) {
				target.classList.toggle("no-popin", true);
				return;
			}

			if (!targets.testTarget(target)) {
				target.classList.toggle("no-popin", true);
				targetRestoreFunction(target);
				return;
			}

			//  Bind activate event.
			target.onclick = Popins.targetClicked;

			//  Set prepare function.
			target.preparePopin = prepareFunction;

			//	Set target restore function.
			target.restoreTarget = targetRestoreFunction;

			//  Run any custom processing.
			if (targetPrepareFunction)
				targetPrepareFunction(target);

			//  Mark target as spawning a popin.
			target.classList.toggle("spawns-popin", true);
		});
	},

	//	Called by: extracts.js
	removeTargetsWithin: (contentContainer, targets, targetRestoreFunction = null) => {
		if (typeof contentContainer == "string")
			contentContainer = document.querySelector(contentContainer);

		if (contentContainer == null)
			return;

		contentContainer.querySelectorAll(targets.targetElementsSelector).forEach(target => {
			if (   target.matches(targets.excludedElementsSelector)
				|| target.closest(targets.excludedContainerElementsSelector) != null) {
				target.classList.toggle("no-popin", false);
				return;
			}

			if (!targets.testTarget(target)) {
				target.classList.toggle("no-popin", false);
				return;
			}

			//  Unbind existing activate events, if any.
			target.onclick = null;

			//  Remove the popin (if any).
			if (target.popin)
				Popins.removePopin(target.popin);

			//  Unset popin prepare function.
			target.preparePopin = null;

			//  Un-mark target as spawning a popin.
			target.classList.toggle("spawns-popin", false);

			//  Run any custom processing.
			targetRestoreFunction = targetRestoreFunction ?? target.restoreTarget;
			if (targetRestoreFunction)
				targetRestoreFunction(target);
		});
	},

	/***********/
	/*	Helpers.
		*/

	//	Called by: extracts.js
	scrollElementIntoViewInPopFrame: (element, alwaysRevealTopEdge = false) => {
		let popin = Popins.containingPopFrame(element);

		let elementRect = element.getBoundingClientRect();
		let popinBodyRect = popin.body.getBoundingClientRect();
		let popinScrollViewRect = popin.scrollView.getBoundingClientRect();

		let bottomBound = alwaysRevealTopEdge ? elementRect.top : elementRect.bottom;
		if (   popin.scrollView.scrollTop                              >= elementRect.top    - popinBodyRect.top
			&& popin.scrollView.scrollTop + popinScrollViewRect.height <= bottomBound - popinBodyRect.top)
			return;

		popin.scrollView.scrollTop = elementRect.top - popinBodyRect.top;
	},

	//	Called by: Popins.injectPopinForTarget
	containingDocumentForTarget: (target) => {
		let containingPopin = Popins.containingPopFrame(target);
		return (containingPopin ? containingPopin.document : Popins.rootDocument);
	},

	//	Called by: Popins.keyUp
	getTopPopin: () => {
		return document.querySelector(".popin");
	},

	allSpawnedPopFrames: () => {
		return Popins.allSpawnedPopins();
	},

	//	Called by: Popins.targetClicked (event handler)
	//	Called by: Popins.cleanup
	//	Called by: extracts.js
	allSpawnedPopins: () => {
		return Popins.spawnedPopins;
	},

	//	Called by: Popins.addTitleBarToPopin
	popinStackNumber: (popin) => {
		//  If there’s another popin in the ‘stack’ below this one…
		let popinBelow = (   popin.nextElementSibling
						  && popin.nextElementSibling.classList.contains("popin"))
						 ? popin.nextElementSibling
						 : null;
		if (popinBelow)
			return parseInt(popinBelow.titleBar.stackCounter.textContent) + 1;
		else
			return 1;
	},

	//	Called by: extracts.js
	//	Called by: Popins.containingDocumentForTarget
	//	Called by: Popins.scrollElementIntoViewInPopFrame
	containingPopFrame: (element) => {
		let shadowBody = element.closest(".shadow-body");
		if (shadowBody)
			return shadowBody.popin;

		return element.closest(".popin");
	},

	//	Called by: many functions in many places
	addClassesToPopFrame: (popin, ...args) => {
		popin.classList.add(...args);
		popin.body.classList.add(...args);
	},

	//	Called by: many functions in many places
	removeClassesFromPopFrame: (popin, ...args) => {
		popin.classList.remove(...args);
		popin.body.classList.remove(...args);
	},

	/********************/
	/*	Popin title bars.
		*/

	/*  Add title bar to a popin which has a populated .titleBarContents.
		*/
	//	Called by: Popins.injectPopinForTarget
	addTitleBarToPopin: (popin) => {
		//  Set class ‘has-title-bar’ on the popin.
		popin.classList.add("has-title-bar");

		//  Create and inject the title bar element.
		popin.titleBar = newElement("DIV");
		popin.titleBar.classList.add("popframe-title-bar");
		popin.insertBefore(popin.titleBar, popin.firstElementChild);

		//  Add popin stack counter.
		popin.titleBar.stackCounter = newElement("SPAN");
		popin.titleBar.stackCounter.classList.add("popin-stack-counter");
		requestAnimationFrame(() => {
			let popinStackNumber = Popins.popinStackNumber(popin);
			popin.titleBar.stackCounter.textContent = popinStackNumber;
			if (popinStackNumber == 1)
				popin.titleBar.stackCounter.style.display = "none";
		});
		popin.titleBar.appendChild(popin.titleBar.stackCounter);

		//  Add the provided title bar contents (buttons, title, etc.).
		popin.titleBarContents.forEach(element => {
			popin.titleBar.appendChild(element);

			if (element.buttonAction)
				element.addActivateEvent(element.buttonAction);
		});
	},

	/*  Add secondary title-link to a popin which has a title-link.
		*/
	//	Called by: Popins.injectPopinForTarget
	addFooterBarToPopin: (popin) => {
		let popinTitleLink = popin.querySelector(".popframe-title-link");
		if (!popinTitleLink)
			return;

		//  Set class ‘has-footer-bar’ on the popin.
		popin.classList.add("has-footer-bar");

		//	Inject popin footer bar.
		popin.footerBar = newElement("DIV");
		popin.footerBar.classList.add("popin-footer-bar");
		popin.insertBefore(popin.footerBar, null);

		//	Inject footer title-link.
		let footerTitleLink = newElement("A");
		footerTitleLink.classList.add("popframe-title-link");
		footerTitleLink.href = popinTitleLink.href;
		footerTitleLink.title = `Open ${footerTitleLink.href} in a new tab.`;
		footerTitleLink.target = "_blank";
		footerTitleLink.textContent = "Open in new tab…";
		popin.footerBar.appendChild(footerTitleLink);
	},

	/*  Elements and methods related to popin title bars.
		*/
	titleBarComponents: {
		//  Icons for various popup title bar buttons.
		buttonIcons: {
			"close": "xmark-regular",
			"options": "gear-solid"
		},

		//  Tooltip text for various popup title bar icons.
		buttonTitles: {
			"close": "Close this popin",
			"options": "Show options"
		},

		//  A generic button, with no icon or tooltip text.
		genericButton: () => {
			let button = newElement("BUTTON");
			button.classList.add("popframe-title-bar-button");

			button.buttonAction = (event) => { event.stopPropagation(); };

			return button;
		},

		//  Close button.
		closeButton: () => {
			let button = Popins.titleBarComponents.genericButton();
			button.classList.add("close-button");

			button.innerHTML = GW.svg(Popins.titleBarComponents.buttonIcons["close"]);
			button.title = Popins.titleBarComponents.buttonTitles["close"];

			button.buttonAction = (event) => {
				event.stopPropagation();

				Popins.removePopin(Popins.containingPopFrame(event.target));
			};

			return button;
		},

		//  Options button (does nothing by default).
		optionsButton: () => {
			let button = Popins.titleBarComponents.genericButton();
			button.classList.add("options-button");

			button.innerHTML = GW.svg(Popins.titleBarComponents.buttonIcons["options"]);
			button.title = Popins.titleBarComponents.buttonTitles["options"];

			return button;
		}
	},

	/******************/
	/*	Optional parts.
	 */

	addPartToPopFrame: (popin, part) => {
		popin.append(part);
	},

	/************************/
	/*	Optional UI elements.
	 */

	addUIElementsToPopFrame: (popin, ...args) => {
		popin.uiElementsContainer.append(...args);
	},

	/******************/
	/*	Popin spawning.
		*/

	//	Called by: Popins.injectPopinForTarget
	newPopin: (target) => {
		GWLog("Popins.newPopin", "popins.js", 2);

		let popin = newElement("DIV");
		popin.classList.add("popin", "popframe");
		popin.innerHTML = `<div class="popframe-scroll-view"><div class="popframe-content-view"></div></div>`;
		popin.scrollView = popin.querySelector(".popframe-scroll-view");
		popin.contentView = popin.querySelector(".popframe-content-view");

		popin.contentView.attachShadow({ mode: "open" });
		popin.document = popin.contentView.shadowRoot;
		popin.document.appendChild(newElement("DIV"));
		popin.document.body = popin.body = popin.shadowBody = popin.document.firstElementChild;
		popin.body.classList.add("popframe-body", "popin-body", "shadow-body");

		let styleReset = newElement("STYLE");
		styleReset.innerHTML = `.shadow-body { all: initial; }`;
		popin.document.insertBefore(styleReset, popin.body);

		popin.body.popin = popin.contentView.popin = popin.scrollView.popin = popin;

		popin.titleBarContents = [ ];

		//  Give the popin a reference to the target.
		popin.spawningTarget = target;

		return popin;
	},

	//	Called by: extracts.js
	//	Called by: extracts-content.js
	setPopFrameContent: (popin, content) => {
		if (content) {
			popin.body.replaceChildren(content);

			return true;
		} else {
			return false;
		}
	},

	//	Called by: Popins.targetClicked (event handler)
	injectPopinForTarget: (target) => {
		GWLog("Popins.injectPopinForTarget", "popins.js", 2);

		//  Create the new popin.
		target.popFrame = target.popin = Popins.newPopin(target);

		// Prepare the newly created popin for injection.
		if (!(target.popFrame = target.popin = target.preparePopin(target.popin)))
			return;

		/*  If title bar contents are provided, create and inject the popin
			title bar, and set class `has-title-bar` on the popin.
			*/
		if (target.popin.titleBarContents.length > 0)
			Popins.addTitleBarToPopin(target.popin);
			Popins.addFooterBarToPopin(target.popin);

		//  Get containing document.
		let containingDocument = Popins.containingDocumentForTarget(target);

		//  Remove (other) existing popins on this level.
		containingDocument.querySelectorAll(".popin").forEach(existingPopin => {
			if (existingPopin != target.popin)
				Popins.removePopin(existingPopin);
		});

		//	Set rendering progress indicator (spinner).
		Popins.addClassesToPopFrame(target.popin, "rendering");

		//  Inject the popin.
		if (containingDocument.popin) {
			/*  Save the parent popin’s scroll state when pushing it down the
				‘stack’.
				*/
			containingDocument.popin.lastScrollTop = containingDocument.popin.scrollView.scrollTop;

			containingDocument.popin.parentElement.insertBefore(target.popin, containingDocument.popin);
		} else {
			target.parentElement.insertBefore(target.popin, target.nextSibling);
		}

		//	Push popin onto spawned popins stack.
		Popins.spawnedPopins.unshift(target.popin);

		//	Designate ancestors.
		let ancestor = target.popin.parentElement;
		do { ancestor.classList.add("popin-ancestor"); }
		while (   (ancestor = ancestor.parentElement) 
			   && [ "MAIN", "ARTICLE" ].includes(ancestor.tagName) == false);

		//  Mark target as having an open popin associated with it.
		target.classList.add("popin-open", "highlighted");

		//	Adjust popin position.
		requestAnimationFrame(() => {
			if (target.adjustPopinWidth)
				target.adjustPopinWidth(target.popin);

			//  Scroll page so that entire popin is visible, if need be.
			requestAnimationFrame(() => {
				Popins.scrollPopinIntoView(target.popin);
			});
		});

		GW.notificationCenter.fireEvent("Popins.popinDidInject", { popin: target.popin });

		requestAnimationFrame(() => {
			//	Disable rendering progress indicator (spinner).
			if (target.popin)
				Popins.removeClassesFromPopFrame(target.popin, "rendering");
		});
	},

	/*	Returns full viewport rect for popin and all auxiliary elements
		(title bar, footers, etc.).
	 */
	getPopinViewportRect: (popin) => {
		return rectUnion(popin.getBoundingClientRect(), ...(Array.from(popin.children).map(x => x.getBoundingClientRect())));
	},

	//	Called by: Popins.injectPopinForTarget
	//	Called by: extracts.js
	scrollPopinIntoView: (popin) => {
		let popinViewportRect = Popins.getPopinViewportRect(popin);

		let windowScrollOffsetForThisPopin = parseInt(popin.dataset.windowScrollOffset ?? '0');

		let scrollWindowBy = 0;
		if (popinViewportRect.bottom > window.innerHeight) {
			scrollWindowBy = Math.round((window.innerHeight * 0.05) + popinViewportRect.bottom - window.innerHeight);
		} else if (popinViewportRect.top < 0) {
			scrollWindowBy = Math.round((window.innerHeight * -0.1) + popinViewportRect.top);
		}

		if (scrollWindowBy > 0) {
			window.scrollBy(0, scrollWindowBy);
			popin.dataset.windowScrollOffset = windowScrollOffsetForThisPopin + scrollWindowBy;
		}
	},

	//	Called by: extracts.js
	removeAllPopinsInContainer: (container) => {
		GWLog("Popins.removeAllPopinsInContainer", "popins.js", 2);

		container.querySelectorAll(".popin").forEach(popin => {
			Popins.removePopin(popin);
		});
	},

	//	Called by: Popins.cleanup
	//	Called by: Popins.targetClicked (event handler)
	//	Called by: Popins.removeTargetsWithin
	//	Called by: Popins.titleBarComponents.closeButton
	//	Called by: Popins.injectPopinForTarget
	removePopin: (popin) => {
		GWLog("Popins.removePopin", "popins.js", 2);

		//  If there’s another popin in the ‘stack’ below this one…
		let popinBelow = (   popin.nextElementSibling
						  && popin.nextElementSibling.classList.contains("popin"))
						 ? popin.nextElementSibling
						 : null;

		//	Save place.
		let ancestor = popin.parentElement;

		//	Remove from spawned popins stack.
		Popins.spawnedPopins.shift();

		//  Remove popin from page.
		popin.remove();

		//  … restore its scroll state.
		if (popinBelow) {
			popinBelow.scrollView.scrollTop = popinBelow.lastScrollTop;
		} else {
			do { ancestor.classList.remove("popin-ancestor"); }
			while (ancestor = ancestor.parentElement);
		}

		//  Detach popin from its spawning target.
		Popins.detachPopinFromTarget(popin);

		//	Restore the window’s scroll state to before the popin was injected.
		window.scrollBy(0, -1 * parseInt(popin.dataset.windowScrollOffset ?? '0'));
	},

	//	Called by: Popins.removePopin
	detachPopinFromTarget: (popin) => {
		GWLog("Popins.detachPopinFromTarget", "popins.js", 2);

		if (popin.spawningTarget == null)
			return;

		popin.spawningTarget.popin = null;
		popin.spawningTarget.popFrame = null;
		popin.spawningTarget.classList.remove("popin-open", "highlighted");
	},

	isSpawned: (popin) => {
		return (   popin
				&& popin.parentElement);
	},

	/*******************/
	/*	Event listeners.
		*/

	//	Added by: Popins.addTargetsWithin
	targetClicked: (event) => {
		GWLog("Popins.targetClicked", "popins.js", 2);

		//	Only unmodified click events should trigger popin spawn.
		if (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey)
			return;

		event.preventDefault();

		let target = event.target.closest(".spawns-popin");

		if (target.classList.contains("popin-open")) {
			Popins.allSpawnedPopins().forEach(popin => {
				Popins.removePopin(popin);
			});
		} else {
			Popins.injectPopinForTarget(target);
		}

		document.activeElement.blur();
	},

	/*  The keyup event.
		*/
	//	Added by: Popins.setup
	keyUp: (event) => {
		GWLog("Popins.keyUp", "popins.js", 3);
		let allowedKeys = [ "Escape", "Esc" ];
		if (!allowedKeys.includes(event.key))
			return;

		event.preventDefault();

		switch(event.key) {
			case "Escape":
			case "Esc":
				let popin = Popins.getTopPopin();
				if (popin)
					Popins.removePopin(popin);
				break;
			default:
				break;
		}
	}
};

GW.notificationCenter.fireEvent("Popins.didLoad");
Annotations = {
	annotatedLinkFullClass: "link-annotated",
	annotatedLinkPartialClass: "link-annotated-partial"
};

Annotations = { ...Annotations,
    /***********/
    /*  General.
     */

	isAnnotatedLink: (link) => {
		return link.classList.containsAnyOf([ Annotations.annotatedLinkFullClass,  Annotations.annotatedLinkPartialClass ]);
	},

	isAnnotatedLinkFull: (link) => {
		return link.classList.contains(Annotations.annotatedLinkFullClass);
	},

	isAnnotatedLinkPartial: (link) => {
		return link.classList.contains(Annotations.annotatedLinkPartialClass);
	},

    allAnnotatedLinksInContainer: (container) => {
        return Array.from(container.querySelectorAll("a[class*='link-annotated']")).filter(link => Annotations.isAnnotatedLink(link));
    },

    /*  Returns the target identifier: the relative url (for local links), 
    	or the full URL (for foreign links).

        Used for loading annotations, and caching reference data.
     */
	targetIdentifier: (target) => {
		return (target.hostname == location.hostname
			   ? target.pathname + target.hash
			   : target.href);
	},

	/***************************/
	/*	Caching (API responses).
	 */

	//	Convenience method.
	cachedDocumentForLink: (link) => {
		/*	If the data source for this link delegates its functionality to
			a different data source, request the cached document from the
			delegate’s provider object.
		 */
		let dataSource = Annotations.dataSourceForLink(link);
		if (   dataSource
			&& dataSource.delegateDataProvider)
			return dataSource.delegateDataProvider.cachedDocumentForLink(link);

		let cachedAPIResponse = Annotations.cachedAPIResponseForLink(link);

		if (   cachedAPIResponse
			&& cachedAPIResponse != "LOADING_FAILED"
			&& cachedAPIResponse instanceof DocumentFragment) {
			return cachedAPIResponse;
		} else {
			return null;
		}
	},

    /*  Returns true iff a cached API response exists for the given link.
     */
    //	Called by: Extracts.setUpAnnotationLoadEventsWithin (extracts-annotations.js)
    cachedDataExists: (link) => {
		/*	If the data source for this link delegates its functionality to
			a different data source, request the cached data from the
			delegate’s provider object.
		 */
		let dataSource = Annotations.dataSourceForLink(link);
		if (   dataSource
			&& dataSource.delegateDataProvider)
			return dataSource.delegateDataProvider.cachedDataExists(link);

        let cachedAPIResponse = Annotations.cachedAPIResponseForLink(link);
        return (   cachedAPIResponse != null
        		&& cachedAPIResponse != "LOADING_FAILED");
    },

	//	Used by: Annotations.cachedAPIResponseForLink
	//	Used by: Annotations.cacheAPIResponseForLink
	cachedAPIResponses: { },

	responseCacheKeyForLink: (link) => {
		return Annotations.sourceURLForLink(link).href;
	},

	//	Called by: Annotations.load
	cachedAPIResponseForLink: (link) => {
		return Annotations.cachedAPIResponses[Annotations.responseCacheKeyForLink(link)];
	},

	//	Called by: Annotations.load
	cacheAPIResponseForLink: (response, link) => {
		Annotations.cachedAPIResponses[Annotations.responseCacheKeyForLink(link)] = response;
	},

	/****************************/
	/*	Caching (reference data).
	 */

    /*  Storage for retrieved and cached annotations.
     */
    cachedReferenceData: { },

	referenceDataCacheKeyForLink: (link) => {
		return Annotations.targetIdentifier(link);
	},

	cachedReferenceDataForLink: (link) => {
		return Annotations.cachedReferenceData[Annotations.referenceDataCacheKeyForLink(link)];
	},

	cacheReferenceDataForLink: (referenceData, link) => {
		Annotations.cachedReferenceData[Annotations.referenceDataCacheKeyForLink(link)] = referenceData;
	},

    /*  Returns cached annotation reference data for a given link, or else 
    	either “LOADING_FAILED” (if loading the annotation was attempted but 
    	failed) or null (if the annotation has not been loaded).
     */
    referenceDataForLink: (link) => {
		/*	If the data source for this link delegates its functionality to
			a different data source, request the cached data from the
			delegate’s provider object.
		 */
		let dataSource = Annotations.dataSourceForLink(link);
		if (   dataSource
			&& dataSource.delegateDataProvider)
			return dataSource.delegateDataProvider.referenceDataForLink(link);

    	let referenceData = Annotations.cachedReferenceDataForLink(link);
		if (   referenceData == null
			&& Annotations.cachedDataExists(link)) {
			/*	Perhaps we’ve got an API response cached, but we haven’t 
				actually constructed reference data from it yet. (Maybe because 
				the API response was acquired otherwise than by the usual load 
				process. Or because the API response is the same as that for a 
				different link, so we don’t need to load it again.)
			 */
			//	Get parsed API response.
			let cachedAPIResponse = Annotations.cachedAPIResponseForLink(link);

			//	Attempt to construct reference data from API response.
			referenceData = Annotations.referenceDataFromParsedAPIResponse(cachedAPIResponse, link) ?? "LOADING_FAILED";
			if (referenceData == "LOADING_FAILED")
				//	Send request to record failure in server logs.
				GWServerLogError(Annotations.sourceURLForLink(link).href + `--could-not-process`, "problematic annotation");

			//	Cache reference data (successfully constructed or not).
			Annotations.cacheReferenceDataForLink(referenceData, link);
		}

        return referenceData;
    },

	/***********/
	/*	Loading.
	 */

	//	Called by: Annotations.sourceURLForLink
	//	Called by: Annotations.processedAPIResponseForLink
	//	Called by: Annotations.referenceDataFromParsedAPIResponse
	dataSourceForLink: (link) => {
		for ([ sourceName, dataSource ] of Object.entries(Annotations.dataSources)) {
			if (sourceName == "local")
				continue;

			if (   (   dataSource.matches
					&& dataSource.matches(link))
				|| (   dataSource.delegateDataSource
					&& dataSource.delegateDataSource.matches
					&& dataSource.delegateDataSource.matches(link)))
				return dataSource;
		}

		return Annotations.dataSources.local;
	},

	//	Called by: Annotations.load
	processedAPIResponseForLink: (response, link) => {
		return Annotations.dataSourceForLink(link).processAPIResponse(response);
	},

	/*	Returns the URL of the annotation resource for the given link.
	 */
	//	Called by: Annotations.load
	//	Called by: Annotations.cachedAPIResponseForLink
	//	Called by: Annotations.cacheAPIResponseForLink
	sourceURLForLink: (link) => {
		return Annotations.dataSourceForLink(link).sourceURLForLink(link);
	},

	//	Called by: extracts.annotationForTarget (extracts-annotations.js)
	waitForDataLoad: (link, loadHandler = null, loadFailHandler = null) => {
		/*	If the data source for this link delegates its functionality to
			a different data source, the delegate’s provider object should
			handle loading.
		 */
		let dataSource = Annotations.dataSourceForLink(link);
		if (   dataSource
			&& dataSource.delegateDataProvider) {
			dataSource.delegateDataProvider.waitForDataLoad(link, loadHandler, loadFailHandler);
			return;
		}

		if (Annotations.cachedAPIResponseForLink(link) == "LOADING_FAILED") {
            if (loadFailHandler)
            	loadFailHandler(link);

			return;
		} else if (Annotations.cachedAPIResponseForLink(link)) {
			if (Annotations.referenceDataForLink(link) == "LOADING_FAILED") {
				if (loadFailHandler)
					loadFailHandler(link);
			} else {
				if (loadHandler)
					loadHandler(link);
			}

			return;
		}

		let didLoadHandler = (info) => {
            if (loadHandler)
            	loadHandler(link);

			GW.notificationCenter.removeHandlerForEvent("Annotations.annotationLoadDidFail", loadDidFailHandler);
        };
        let loadDidFailHandler = (info) => {
            if (loadFailHandler)
            	loadFailHandler(link);

			GW.notificationCenter.removeHandlerForEvent("Annotations.annotationDidLoad", didLoadHandler);
        };
		let options = { 
        	once: true, 
        	condition: (info) => info.link == link
        };

        GW.notificationCenter.addHandlerForEvent("Annotations.annotationDidLoad", didLoadHandler, options);
        GW.notificationCenter.addHandlerForEvent("Annotations.annotationLoadDidFail", loadDidFailHandler, options);
	},

    /*  Load and process the annotation for the given link.
     */
    //	Called by: Extracts.setUpAnnotationLoadEventsWithin (extracts-annotations.js)
    load: (link, loadHandler = null, loadFailHandler = null) => {
        GWLog("Annotations.load", "annotations.js", 2);

		/*	If the data source for this link delegates its functionality to
			a different data source, the delegate’s provider object should
			handle loading.
		 */
		let dataSource = Annotations.dataSourceForLink(link);
		if (   dataSource
			&& dataSource.delegateDataProvider) {
			dataSource.delegateDataProvider.load(link, loadHandler, loadFailHandler);
			return;
		}

		/*	Get URL of the annotation resource.
		 */
        let sourceURL = Annotations.sourceURLForLink(link);

		/*	Depending on the data source, `response` could be HTML,
			JSON, or other. We construct and cache a reference data object,
			then fire the appropriate event.
		 */
		let processResponse = (response) => {
			let referenceData = Annotations.referenceDataFromParsedAPIResponse(response, link);

			if (referenceData) {
				Annotations.cacheReferenceDataForLink(referenceData, link);

				GW.notificationCenter.fireEvent("Annotations.annotationDidLoad", { 
					link: link 
				});
			} else {
				Annotations.cacheReferenceDataForLink("LOADING_FAILED", link);

				GW.notificationCenter.fireEvent("Annotations.annotationLoadDidFail", { 
					link: link 
				});

				//	Send request to record failure in server logs.
				GWServerLogError(sourceURL.href + `--could-not-process`, "problematic annotation");
			}
		};

		/*	Retrieve, parse, and cache the annotation resource; or use an
			already-cached API response.
		 */
		let response = Annotations.cachedAPIResponseForLink(link);
		if (response) {
			processResponse(response);
		} else {
			doAjax({
				location: sourceURL.href,
				onSuccess: (event) => {
					let response = Annotations.processedAPIResponseForLink(event.target.responseText, link);

					Annotations.cacheAPIResponseForLink(response, link);

					processResponse(response);
				},
				onFailure: (event) => {
					Annotations.cacheAPIResponseForLink("LOADING_FAILED", link);
					Annotations.cacheReferenceDataForLink("LOADING_FAILED", link);

					GW.notificationCenter.fireEvent("Annotations.annotationLoadDidFail", { link: link });

					//	Send request to record failure in server logs.
					GWServerLogError(sourceURL.href, "missing annotation");
				},
				headers: Annotations.dataSourceForLink(link).additionalAPIRequestHeaders
			});
		}

		//	Call any provided handlers, if/when appropriate.
		if (loadHandler || loadFailHandler)
			Annotations.waitForDataLoad(link, loadHandler, loadFailHandler);
    },

	//	Called by: Annotations.load
	referenceDataFromParsedAPIResponse: (response, link) => {
		if (response == "LOADING_FAILED")
			return null;

		return Annotations.dataSourceForLink(link).referenceDataFromParsedAPIResponse(response, link);
	},

	/***************************/
	/* ANNOTATION DATA SOURCES */
	/***************************/
	/*	Note on annotation data sources:

		More data sources may be added. Any data source object must have these
		four properties, each a function with the given signature:

		.matches(URL|Element) => boolean
		.sourceURLForLink(URL|Element) => URL
		.processAPIResponse(string) => object
		.referenceDataFromParsedAPIResponse(object, URL|Element) => object

		(Most data source objects also have additional properties, functions,
		 etc., as necessary to implement the above functionality.)

		Examine implementation of these functions in .dataSources.local to
		understand their purpose.
	 */

	dataSources: {
		/********************************************************/
		/*	Annotations generated server-side and hosted locally.
		 */

		local: {
			/*	There could be a local annotation for any link. As this returns
				true for all links, it is the fallback data source in the event 
				that no other data sources match a link.
			 */
			matches: (link) => {
				return true;
			},

			//	Called by: Annotations.processedAPIResponseForLink
			//	Called by: Annotations.sourceURLForLink
			sourceURLForLink: (link) => {
				return URLFromString(  Annotations.dataSources.local.basePathname
									 + fixedEncodeURIComponent(fixedEncodeURIComponent(Annotations.targetIdentifier(link)))
									 + ".html");
			},

			//	Called by: Annotations.processedAPIResponseForLink
			processAPIResponse: (response) => {
				let responseDoc = newDocument(response);

				//	Request the image, to cache it.
				let thumbnail = responseDoc.querySelector(".page-thumbnail");
				if (thumbnail)
					doAjax({ location: URLFromString(thumbnail.src) });

				return responseDoc;
			},

			//	Called by: Annotations.referenceDataFromParsedAPIResponse
			referenceDataFromParsedAPIResponse: (response, link = null) => {
				let referenceElement = response.querySelector(Annotations.dataSources.local.referenceElementSelector);

				let titleHTML = referenceElement.innerHTML;
				let titleText = referenceElement.textContent;

				//	On mobile, use mobile-specific link href, if provided.
				let titleLinkHref = (   referenceElement.dataset.hrefMobile 
									 && GW.isMobile())
									? referenceElement.dataset.hrefMobile
									: referenceElement.href;

				let titleLinkClass = "title-link";
				//  Import certain link classes.
				/*  Just ‘link-live’ for now, but the inclusion rule is: any class that
					is used to test whether a link is of a certain type - see e.g.
					Extracts.isForeignSiteLink() in extracts-content.js - for which link
					type there can be annotations (so not, e.g., ‘footnote-ref’, because
					there’s no such thing as an annotated footnote link). This way, the
					title-link of the popup will correctly test as the *un-annotated*
					link type of the original target.
					—SA 2022-06-13
				 */
				[ "link-live" ].forEach(targetClass => {
					if (referenceElement.classList.contains(targetClass))
						titleLinkClass += ` ${targetClass}`;
				});

				//	Special handling for links with separate ‘HTML’ URLs.
				if (   referenceElement.dataset.urlHtml
					&& titleLinkClass.includes("link-live") == false)
					titleLinkClass += " link-live";

				//	Link icon for the title link.
				let titleLinkIconMetadata;
				if (referenceElement.dataset.linkIcon) {
					titleLinkIconMetadata = `data-link-icon-type="${(referenceElement.dataset.linkIconType)}"`
										  + `data-link-icon="${(referenceElement.dataset.linkIcon)}"`;
				} else if (link && link.dataset.linkIcon) {
					titleLinkIconMetadata = `data-link-icon-type="${(link.dataset.linkIconType)}"`
										  + `data-link-icon="${(link.dataset.linkIcon)}"`;
				}

				//	Special data attributes for the title link.
				let titleLinkDataAttributes = [ 
					"urlHtml", 
					"urlArchive"
				].map(attr => 
					referenceElement.dataset[attr] 
					? `data-${(attr.replace(/([a-z])([A-Z])/g, "$1-$2").toLowerCase())}="${referenceElement.dataset[attr]}"` 
					: null
				).filter(Boolean).join(" ");
				if (titleLinkDataAttributes == "")
					titleLinkDataAttributes = null;

				//	Archive URL.
				let archiveURL = referenceElement.dataset.urlArchive ?? null;
				let archiveURLText = archiveURL
									  ? "archive"
									  : null;

				//  Author list.
				let authorElement = response.querySelector(".author");
				//	Generate comma-separated author list; truncate with “…” abbreviation for ‘et al’ @ > 3.
				let authorList;
				if (authorElement) {
					authorList = authorElement.innerHTML.split(", ").slice(0, 3).join(", ");
					if (authorList.length < authorElement.innerHTML.length)
						authorList += "…";
				}
				let author = authorElement 
							 ? `<span class="data-field author cite-author">${authorList}</span>` 
							 : null;

				//  Date.
				let dateElement = response.querySelector(".date");
				let date = dateElement 
						   ? (  `<span class="data-field cite-date" title="${dateElement.textContent}">` 
						      + dateElement.textContent.replace(/-[0-9][0-9]-[0-9][0-9]$/, "") 
						      + `</span>`) 
						   : null;

				// Link Tags
				let tagsElement = response.querySelector(".link-tags");
				let tags = tagsElement
						   ? `<span class="data-field link-tags">${tagsElement.innerHTML}</span>`
						   : null;

				//	The backlinks link (if exists).
				let backlinksElement = response.querySelector(".backlinks");
				let backlinks = backlinksElement
								? `<span 
									class="data-field aux-links backlinks" 
									>${backlinksElement.innerHTML}</span>`
								: null;

				//	The similar-links link (if exists).
				let similarsElement = response.querySelector(".similars");
				let similars = similarsElement
							   ? `<span 
							       class="data-field aux-links similars"
							       >${similarsElement.innerHTML}</span>`
							   : null;

                //	The link-link-bibliography link (if exists).
				let linkbibElement = response.querySelector(".link-bibliography");
				let linkbib = linkbibElement
							  ? `<span 
							  	  class="data-field aux-links link-bibliography"
							  	  >${linkbibElement.innerHTML}</span>`
							  : null;

				//	All the aux-links (tags, backlinks, similars, link link-bib).
				let auxLinks = ([ tags, backlinks, similars, linkbib ].filter(x => x).join("; ") || null);
				if (auxLinks)
					auxLinks = ` (${auxLinks})`;

				//  Combined author, date, & aux-links.
				let authorDateAux = ([ author, date, auxLinks ].filter(x => x).join("") || null);

				//	Abstract (if exists).
				let abstractElement = response.querySelector("blockquote");
				let abstractHTML = null;
				if (abstractElement) {
					let abstractDocument = newDocument(abstractElement.childNodes);
					Annotations.dataSources.local.postProcessReferenceEntry(abstractDocument, link);
					abstractHTML = abstractDocument.innerHTML;
				}

				//	Pop-frame title text.
				let popFrameTitle = referenceElement.cloneNode(true);
				//	Trim quotes.
				let [ first, last ] = [ popFrameTitle.firstTextNode, popFrameTitle.lastTextNode ];
				if (   /^['"‘“]/.test(first.textContent) == true
					&& /['"’”]$/.test(last.textContent)  == true) {
					first.textContent = first.textContent.slice(1);
					last.textContent = last.textContent.slice(0, -1);
				}
				let popFrameTitleText = popFrameTitle.innerHTML;

				return {
					content: {
						titleHTML:                titleHTML,
						fullTitleHTML:            titleHTML,
						titleText:                titleText,
						titleLinkHref:            titleLinkHref,
						titleLinkClass:           titleLinkClass,
						titleLinkIconMetadata:    titleLinkIconMetadata,
						titleLinkDataAttributes:  titleLinkDataAttributes,
						archiveURL:               archiveURL,
						archiveURLText:           archiveURLText,
						author:                   author,
						date:                     date,
						auxLinks:                 auxLinks,
						authorDateAux:            authorDateAux,
						abstract:                 abstractHTML,
					},
					template:                       "annotation-blockquote-inside",
					linkTarget:                     (GW.isMobile() ? "_self" : "_blank"),
					whichTab:                       (GW.isMobile() ? "current" : "new"),
					tabOrWindow:                    (GW.isMobile() ? "tab" : "window"),
					popFrameTitleText:              popFrameTitleText,
					popFrameTitleLinkHref:          titleLinkHref,
					popFrameTitleArchiveLinkHref:   archiveURL
				};
			},

			/*  Post-process an already-constructed local annotation 
				(do HTML cleanup, etc.).
			 */
			//	Called by: Annotations.dataSources.local.referenceDataFromParsedAPIResponse
			postProcessReferenceEntry: (referenceEntry, link = null) => {
				//	Unwrap extraneous <div>s, if present.
				if (   referenceEntry.firstElementChild == referenceEntry.lastElementChild
					&& referenceEntry.firstElementChild.tagName == "DIV")
					unwrap(referenceEntry.firstElementChild);

				//	If there’s a “See Also” section, rectify its classes.
				let seeAlsoList = referenceEntry.querySelector(_π(".see-also-append", " ", [ "ul", "ol" ]).join(", "));
				if (seeAlsoList) {
					seeAlsoList.classList.add("aux-links-list", "see-also-list");

					let listLabel = previousBlockOf(seeAlsoList, { notBlockElements: [ ".columns" ] });
					if (listLabel)
						listLabel.classList.add("aux-links-list-label", "see-also-list-label");
				}

				//	Unwrap more extraneous <div>s, if present.
				let pageDescriptionClass = "page-description-annotation";
				let pageDescription = referenceEntry.querySelector(`div.${pageDescriptionClass}`);
				if (pageDescription)
					unwrap(pageDescription, {
						moveClasses: true,
						classesToMove: [ pageDescriptionClass ]
					});
			},

			basePathname: "/metadata/annotation/",
			referenceElementSelector: [ Annotations.annotatedLinkFullClass,  Annotations.annotatedLinkPartialClass ].map(className => `a.${className}`).join(", ")
		}
	}
};


/**********/
/*	Tweets.
 */
Annotations.dataSources.twitter = {
	get delegateDataProvider() { return Content; },
	get delegateDataSource() { return Content.contentTypes.tweet; }
};


/**************************************************/
/*  Wikipedia entries (page summaries or sections).
 */
Annotations.dataSources.wikipedia = {
	/*	The Wikipedia API only gives usable responses for most, not all,
		Wikipedia URLs.
	 */
	matches: (link) => {
		return (   /(.+?)\.wikipedia\.org/.test(link.hostname)
				&& link.pathname.startsWith("/wiki/")
				&& link.pathname.startsWithAnyOf(_π("/wiki/", [ "File:", "Category:", "Special:" ])) == false);
	},

	//	Called by: Annotations.processedAPIResponseForLink
	//	Called by: Annotations.sourceURLForLink
	sourceURLForLink: (link) => {
		annotationURL = URLFromString(link.href);

		let wikiPageName = fixedEncodeURIComponent(/\/wiki\/(.+?)$/.exec(decodeURIComponent(annotationURL.pathname))[1]);
		annotationURL.pathname = `/api/rest_v1/page/html/${wikiPageName}`;
		annotationURL.hash = "";

		return annotationURL;
	},

	//	Called by: Annotations.processedAPIResponseForLink
	processAPIResponse: (response) => {
		return newDocument(response);
	},

	//	Called by: Annotations.referenceDataFromParsedAPIResponse
	referenceDataFromParsedAPIResponse: (response, articleLink) => {
		let titleLinkHref = articleLink.href;

		let responseHTML, titleHTML, fullTitleHTML, secondaryTitleLinksHTML;
		if (articleLink.hash > "") {
			let targetHeading = response.querySelector(selectorFromHash(articleLink.hash));

			/*	Check whether we have tried to load a page section which does
				not exist on the requested wiki page.
			 */
			if (!targetHeading)
				return null;

			//	The id is on the heading, so the section is its parent.
			let targetSection = targetHeading.parentElement.cloneNode(true);

			//	Excise heading.
			targetHeading = targetSection.firstElementChild;
			targetHeading.remove();

			//	Content sans heading.
			responseHTML = targetSection.innerHTML;

			//	Unwrap or delete links, but save them for inclusion in the template.
			secondaryTitleLinksHTML = "";
			//	First link is the section title itself.
			targetHeading.querySelectorAll("a:first-of-type").forEach(link => {
				//  Process link, save HTML, unwrap.
				Annotations.dataSources.wikipedia.qualifyWikipediaLink(link, articleLink);
				Annotations.dataSources.wikipedia.designateWikiLink(link);
				secondaryTitleLinksHTML += link.outerHTML;
				unwrap(link);
			});
			//	Additional links are other things, who knows what.
			targetHeading.querySelectorAll("a").forEach(link => {
				//  Process link, save HTML, delete.
				Annotations.dataSources.wikipedia.qualifyWikipediaLink(link, articleLink);
				Annotations.dataSources.wikipedia.designateWikiLink(link);
				secondaryTitleLinksHTML += link.outerHTML;
				link.remove();
			});
			if (secondaryTitleLinksHTML > "")
				secondaryTitleLinksHTML = ` (${secondaryTitleLinksHTML})`;

			//	Cleaned section title.
			titleHTML = targetHeading.innerHTML;
			fullTitleHTML = `${titleHTML} (${(response.querySelector("title").innerHTML)})`;
		} else {
			responseHTML = response.querySelector("[data-mw-section-id='0']").innerHTML;
			titleHTML = unescapeHTML(response.querySelector("title").innerHTML);
			fullTitleHTML = titleHTML;

			//	Build TOC.
			let sections = Array.from(response.querySelectorAll("section")).slice(1);
			if (   sections 
				&& sections.length > 0) {
				responseHTML += `<div class="TOC columns"><ul>`;
				let headingLevel = 2;
				for (let i = 0; i < sections.length; i++) {
					let section = sections[i];
					let headingElement = section.firstElementChild;
					let newHeadingLevel = parseInt(headingElement.tagName.slice(1));
					if (newHeadingLevel > headingLevel)
						responseHTML += `<ul>`;

					if (   i > 0 
						&& newHeadingLevel <= headingLevel)
						responseHTML += `</li>`;

					if (newHeadingLevel < headingLevel)
						responseHTML += `</ul>`;

					//	We must encode, because the anchor might contain quotes.
					let urlEncodedAnchor = fixedEncodeURIComponent(headingElement.id);

					//	Get heading, parse as HTML, and unwrap links.
					let heading = headingElement.cloneNode(true);
					heading.querySelectorAll("a").forEach(link => { unwrap(link); });

					//	Construct TOC entry.
					responseHTML += `<li><a href='${articleLink}#${urlEncodedAnchor}'>${(heading.innerHTML)}</a>`;

					headingLevel = newHeadingLevel;
				}
				responseHTML += `</li></ul></div>`;
			}
		}

		let referenceEntry = newDocument(responseHTML);
		Annotations.dataSources.wikipedia.postProcessReferenceEntry(referenceEntry, articleLink);
		let abstractHTML = referenceEntry.innerHTML;

		let titleText = newElement("SPAN", null, { innerHTML: titleHTML }).textContent;

		//	Pop-frame title text. Mark sections with ‘§’ symbol.
		let popFrameTitleHTML = (articleLink.hash > ""
								 ? `${(response.querySelector("title").innerHTML)} &#x00a7; ${titleHTML}`
								 : titleHTML);
		let popFrameTitleText = newElement("SPAN", null, { innerHTML: popFrameTitleHTML }).textContent;

		return {
			content: {
				titleHTML:                titleHTML,
				fullTitleHTML:            fullTitleHTML,
				secondaryTitleLinksHTML:  (secondaryTitleLinksHTML ?? null),
				titleText:                titleText,
				titleLinkHref:            titleLinkHref,
				titleLinkClass:           `title-link link-live`,
				titleLinkIconMetadata:    `data-link-icon-type="svg" data-link-icon="wikipedia"`,
				abstract: 		          abstractHTML,
				dataSourceClass:          "wikipedia-entry",
			},
			template:               "annotation-blockquote-inside",
			linkTarget:             (GW.isMobile() ? "_self" : "_blank"),
			whichTab:               (GW.isMobile() ? "current" : "new"),
			tabOrWindow:            (GW.isMobile() ? "tab" : "window"),
			popFrameTitleText:      popFrameTitleText,
			popFrameTitleLinkHref:  titleLinkHref
		};
	},

	additionalAPIRequestHeaders: {
		"Accept": 'text/html; charset=utf-8; profile="https://www.mediawiki.org/wiki/Specs/HTML/2.1.0"'
	},

	/*	Qualify a link in a Wikipedia article.
	 */
	qualifyWikipediaLink: (link, hostArticleLink) => {
		if (link.getAttribute("href") == null)
			return;

		//  Qualify link.
		if (link.getAttribute("rel") == "mw:WikiLink")
			link.pathname = "/wiki" + link.getAttribute("href").slice(1);
		if (link.getAttribute("href").startsWith("#"))
			link.pathname = hostArticleLink.pathname;
		if (link.hostname == location.hostname)
			link.hostname = hostArticleLink.hostname;
	},

	/*	Mark a wiki-link appropriately, as annotated, or live, or neither.
	 */
	designateWikiLink: (link) => {
		if (/(.+?)\.wikipedia\.org/.test(link.hostname)) {
			if (Annotations.dataSources.wikipedia.matches(link)) {
				link.classList.add(Annotations.annotatedLinkFullClass);
			} else {
				if (!(   link.pathname.startsWithAnyOf(_π("/wiki/", [ "Special:" ]))
					  || link.pathname == "/w/index.php"))
					link.classList.add("link-live");
			}
		}
	},

	/*  Elements to excise from a Wikipedia entry.
	 */
	//	Used in: Annotations.dataSources.wikipedia.postProcessReferenceEntry
	extraneousElementSelectors: [
		"style",
		".mw-ref",
		".shortdescription",
		"td hr",
		".hatnote",
		".portal",
		".penicon",
		".reference",
		".Template-Fact",
		".error",
		".mwe-math-mathml-inline",
        ".sidebar",
        ".ambox",
		".unicode.haudio"
	],

	/*  CSS properties to preserve when stripping inline styles.
	 */
	//	Used in: Annotations.dataSources.wikipedia.postProcessReferenceEntry
	preservedInlineStyleProperties: [
		"display",
		"position",
		"top",
		"left",
		"bottom",
		"right",
		"width",
		"height",
		"word-break"
	],

	/*  Post-process an already-constructed annotation created from a Wikipedia
		entry (do HTML cleanup, etc.).
	 */
	//	Called by: Annotations.dataSources.wikipedia.referenceDataFromParsedAPIResponse
	postProcessReferenceEntry: (referenceEntry, articleLink) => {
		//  Remove unwanted elements.
		referenceEntry.querySelectorAll(Annotations.dataSources.wikipedia.extraneousElementSelectors.join(", ")).forEach(element => {
			element.remove();
		});

		//  Remove location maps (they don’t work right).
		referenceEntry.querySelectorAll(".locmap").forEach(locmap => {
			(locmap.closest("tr") ?? locmap).remove();
		});

		//	Remove other maps.
		referenceEntry.querySelectorAll("img").forEach(image => {
			let imageSourceURL = URLFromString(image.src);
			if (imageSourceURL.hostname == "maps.wikimedia.org")
				image.remove();
		});

		//  Remove empty paragraphs.
		referenceEntry.querySelectorAll("p:empty").forEach(emptyGraf => {
			emptyGraf.remove();
		});

		//	Remove edit-links.
		referenceEntry.querySelectorAll("a[title^='Edit this on Wiki'], a[title^='Edit this at Wiki']").forEach(editLink => {
			editLink.remove();
		});

		//  Process links.
		referenceEntry.querySelectorAll("a").forEach(link => {
			//	De-linkify non-anchor self-links.
			if (   link.hash     == ""
				&& link.pathname == articleLink.pathname) {
				unwrap(link);
				return;
			}

			//  Qualify links.
			Annotations.dataSources.wikipedia.qualifyWikipediaLink(link, articleLink);

			//  Mark other Wikipedia links as also being annotated.
			Annotations.dataSources.wikipedia.designateWikiLink(link);

			//  Mark self-links (anchorlinks within the same article).
			if (link.pathname == articleLink.pathname)
				link.classList.add("link-self");
		});

		//	Strip inline styles and some related attributes.
		let tableElementsSelector = "table, thead, tfoot, tbody, tr, th, td";
		referenceEntry.querySelectorAll("[style]").forEach(styledElement => {
			//	Skip table elements; we handle those specially.
			if (styledElement.matches(tableElementsSelector))
				return;

			if (styledElement.style.display != "none")
				stripStyles(styledElement, null, Annotations.dataSources.wikipedia.preservedInlineStyleProperties);
		});
		//	Special handling for table elements.
		referenceEntry.querySelectorAll(tableElementsSelector).forEach(tableElement => {
			if (tableElement.style.display != "none") {
				if (tableElement.style.position == "relative")
					stripStyles(tableElement, null, [ "text-align", "position", "width", "height" ]);
				else
					stripStyles(tableElement, null, [ "text-align" ]);
			}

			[ "width", "height", "align" ].forEach(attribute => {
				tableElement.removeAttribute(attribute);
			});
		});

		//  Rectify table classes.
		referenceEntry.querySelectorAll("table.sidebar").forEach(table => {
			table.classList.toggle("infobox", true);
		});

		//  Normalize table cell types.
		referenceEntry.querySelectorAll("th:not(:only-child)").forEach(cell => {
			let rowSpan = (cell.rowSpan > 1) ? ` rowspan="${cell.rowSpan}"` : ``;
			let colSpan = (cell.colSpan > 1) ? ` colspan="${cell.colSpan}"` : ``;
			cell.outerHTML = `<td${rowSpan}${colSpan}>${cell.innerHTML}</td>`;
		});

		//  Un-linkify images.
		referenceEntry.querySelectorAll("a img").forEach(imageLink => {
			imageLink.parentElement.outerHTML = imageLink.outerHTML;
		});

		//	Fix chemical formulas.
		referenceEntry.querySelectorAll(".chemf br").forEach(br => {
			br.remove();
		});

		//	Rectify quoteboxes.
		referenceEntry.querySelectorAll("div.quotebox").forEach(quotebox => {
			let blockquote = quotebox.querySelector("blockquote");
			blockquote.classList.add("quotebox");
			
			let title = quotebox.querySelector(".quotebox-title");
			if (title) {
				blockquote.insertBefore(title, blockquote.firstElementChild);
			}

			let cite = quotebox.querySelector("blockquote + p");
			if (cite) {
				blockquote.insertBefore(cite, null);
				cite.classList.add("quotebox-citation");
			}

			unwrap(quotebox);
		});

		//  Separate out the thumbnail and float it.
		let thumbnail = referenceEntry.querySelector("img");
		let thumbnailContainer;
		if (thumbnail)
			thumbnailContainer = thumbnail.closest(".infobox-image, .thumb");
		if (   thumbnail
			&& thumbnailContainer
			&& thumbnailContainer.closest(".gallery") == null) {
			while ([ "TR", "TD", "TH" ].includes(thumbnailContainer.tagName))
				thumbnailContainer = thumbnailContainer.parentElement;

			//	Save references to thumbnails’ parent elements.
			let thumbnailParents = [ ];

			//  Create the figure and move the thumbnail(s) into it.
			let figure = newElement("FIGURE", { "class": "float-right" });
			thumbnailContainer.querySelectorAll(".infobox-image img, .thumb img").forEach(image => {
				if (image.closest("figure") == figure)
					return;

				thumbnailParents.push(image.parentElement);

				let closestRow = image.parentElement;
				while (   closestRow != null
					   && !(   closestRow.tagName == "TR" 
					   		|| closestRow.style.display == "table-row")) {
					closestRow = closestRow.parentElement.closest("tr, [style*='display']");
				}
				if (closestRow == null)
					return;

				let allImagesInRow = closestRow.querySelectorAll("img");
				if (allImagesInRow.length > 1) {
					let rowWrapper = newElement("SPAN", { "class": "image-wrapper image-row-wrapper" });
					rowWrapper.append(...allImagesInRow);
					figure.append(rowWrapper);
				} else {
					figure.append(allImagesInRow[0]);
				}
			});

			//  Create the caption, if need be.
			let caption = referenceEntry.querySelector(".mw-default-size + div, .infobox-caption");
			if (   caption
				&& caption.textContent > "")
				figure.appendChild(newElement("FIGCAPTION", null, { "innerHTML": caption.innerHTML }));

			//  Insert the figure as the first child of the annotation.
			referenceEntry.insertBefore(figure, referenceEntry.firstElementChild);

			//  Rectify classes.
			thumbnailParents.first?.closest("table")?.classList.toggle("infobox", true);

			//  Remove the whole row where each thumbnail was.
			thumbnailParents.forEach(thumbnailParent => {
				thumbnailParent.closest("tr")?.remove();
			});
		} else if (   thumbnail
				   && thumbnail.closest("figure")) {
			let figure = thumbnail.closest("figure");

			//  Insert the figure as the first child of the annotation.
			referenceEntry.insertBefore(figure, referenceEntry.firstElementChild);
			figure.classList.add("float-right");

			let caption = figure.querySelector("figcaption");
			if (caption.textContent == "")
				caption.remove();
		}

		//	Rewrite other figures.
		referenceEntry.querySelectorAll("div.thumb").forEach(figureBlock => {
			let figure = newElement("FIGURE");
			figureBlock.querySelectorAll("img").forEach(image => {
				figure.appendChild(image);
			});
			figure.appendChild(newElement("FIGCAPTION", null, { "innerHTML": figureBlock.querySelector(".thumbcaption")?.innerHTML }));
			figureBlock.parentNode.insertBefore(figure, figureBlock);
			figureBlock.remove();
		});

		//	Mark certain images as not to be wrapped in figures.
		let noFigureImagesSelector = [
			".mwe-math-element",
			".mw-default-size",
			".sister-logo",
			".side-box-image",
			"p"
		].map(selector => `${selector} img`).join(", ");
		referenceEntry.querySelectorAll(noFigureImagesSelector).forEach(image => {
			image.classList.add("figure-not");
		});
	}
};

//	Fire load event.
GW.notificationCenter.fireEvent("Annotations.didLoad");
Content = {
	/*******************/
	/*	Content caching.
	 */

	cachedContent: { },

	contentCacheKeyForLink: (link) => {
		return Content.sourceURLsForLink(link).first.href;
	},

	cacheContentForLink: (content, link) => {
		Content.cachedContent[Content.contentCacheKeyForLink(link)] = content;
	},

	cachedContentForLink: (link) => {
		//	Special case for the link being to the current page.
		if (link.pathname == location.pathname)
			Content.load(link);

		return Content.cachedContent[Content.contentCacheKeyForLink(link)];
	},

	cachedDocumentForLink: (link) => {
		let content = Content.cachedContentForLink(link);
		return (content && content != "LOADING_FAILED"
				? content.document 
				: null);
	},

	cachedDataExists: (link) => {
		let cachedContent = Content.cachedContentForLink(link);
        return (   cachedContent != null
        		&& cachedContent != "LOADING_FAILED");
	},

	updateCachedContent: (link, updateFunction) => {
		if (Content.cachedDataExists(link) == false)
			return;

		let content = Content.cachedContentForLink(link);

		switch (Content.contentTypeForLink(link)) {
			case Content.contentTypes.localPage:
				updateFunction(content.document);
				break;
			default:
				break;
		}
	},

	/*******************/
	/*	Content loading.
	 */

	sourceURLsForLink: (link) => {
		return Content.contentTypeForLink(link).sourceURLsForLink(link);
	},

	//	Called by: Extracts.handleIncompleteReferenceData (extracts.js)
	waitForDataLoad: (link, loadHandler = null, loadFailHandler = null) => {
		if (Content.cachedContentForLink(link) == "LOADING_FAILED") {
            if (loadFailHandler)
            	loadFailHandler(link);

			return;
		} else if (Content.cachedContentForLink(link)) {
            if (loadHandler)
            	loadHandler(link);

			return;
		}

		let didLoadHandler = (info) => {
            if (loadHandler)
            	loadHandler(link);

			GW.notificationCenter.removeHandlerForEvent("Content.contentLoadDidFail", loadDidFailHandler);
        };
        let loadDidFailHandler = (info) => {
            if (loadFailHandler)
            	loadFailHandler(link);

			GW.notificationCenter.removeHandlerForEvent("Content.contentDidLoad", didLoadHandler);
        };
		let options = {
        	once: true,
        	condition: (info) => (info.link == link)
        };

        GW.notificationCenter.addHandlerForEvent("Content.contentDidLoad", didLoadHandler, options);
        GW.notificationCenter.addHandlerForEvent("Content.contentLoadDidFail", loadDidFailHandler, options);
	},

	load: (link, loadHandler = null, loadFailHandler = null, sourceURLsRemaining = null) => {
        GWLog("Content.load", "content.js", 2);

		sourceURLsRemaining = sourceURLsRemaining ?? Content.sourceURLsForLink(link);
		let sourceURL = sourceURLsRemaining.shift();

		let processResponse = (response) => {
			let content = Content.contentFromResponse(response, link, sourceURL);

			if (content) {
				Content.cacheContentForLink(content, link);

				GW.notificationCenter.fireEvent("Content.contentDidLoad", {
					link: link
				});
			} else {
				Content.cacheContentForLink("LOADING_FAILED", link);

				GW.notificationCenter.fireEvent("Content.contentLoadDidFail", {
					link: link
				});

				//	Send request to record failure in server logs.
				GWServerLogError(sourceURL + `--could-not-process`, "problematic content");
			}
		};

		if (sourceURL.pathname == location.pathname) {
			processResponse();
		} else {
			doAjax({
				location: sourceURL.href,
				onSuccess: (event) => {
					let contentType = Content.contentTypeForLink(link);
					let httpContentTypeHeader = event.target.getResponseHeader("Content-Type");
					if (   contentType.permittedContentTypes
						&& (   httpContentTypeHeader == null
							|| contentType.permittedContentTypes.includes(httpContentTypeHeader.match(/(.+?)(?:;|$)/)[1]) == false)) {
						//	Send request to record failure in server logs.
						GWServerLogError(includeLink.href + `--bad-content-type`, "bad content type");

						return;
                    }

					processResponse(event.target.responseText);
				},
				onFailure: (event) => {
					if (sourceURLsRemaining.length > 0) {
						Content.load(link, null, null, sourceURLsRemaining);
						return;
					}

					Content.cacheContentForLink("LOADING_FAILED", link);

					GW.notificationCenter.fireEvent("Content.contentLoadDidFail", {
						link: link
					});

					//	Send request to record failure in server logs.
					GWServerLogError(sourceURL, "missing content");
				}
			});
		}

		//	Call any provided handlers, if/when appropriate.
		if (loadHandler || loadFailHandler)
			Content.waitForDataLoad(link, loadHandler, loadFailHandler);
	},

	contentFromResponse: (response, link, loadURL) => {
		return Content.contentTypeForLink(link).contentFromResponse(response, link, loadURL);
	},

	/****************************/
	/*	Reference data retrieval.
	 */

	referenceDataForLink: (link) => {
		let content = Content.cachedContentForLink(link);
		if (   content == null
			|| content == "LOADING_FAILED") {
			return content;
		} else {
			return Content.referenceDataFromContent(content, link);
		}
	},

	referenceDataFromContent: (content, link) => {
		return Content.contentTypeForLink(link).referenceDataFromContent(content, link);
	},

	/**************************************************************/
	/*	CONTENT TYPES

		Each has four necessary members:

		.matches(URL|Element) => boolean
		.sourceURLsForLink(URL|Element) => [ URL ]
		.contentFromResponse(string, URL|Element, URL) => object
		.referenceDataFromContent(object, URL|Element) => object
	 */

	contentTypeForLink: (link) => {
		for ([ typeName, contentType ] of Object.entries(Content.contentTypes))
			if (contentType.matches(link))
				return contentType;

		return null;
	},

	contentTypes: {
		tweet: {
			matches: (link) => {
				return (   [ "twitter.com", "x.com" ].includes(link.hostname)
						&& link.pathname.match(/\/.+?\/status\/[0-9]+$/) != null);
			},

			sourceURLsForLink: (link) => {
				let urls = [ ];

				if (link.dataset.urlArchive)
					urls.push(URLFromString(link.dataset.urlArchive));

				if (link.dataset.urlHtml)
					urls.push(URLFromString(link.dataset.urlHtml));

				return urls.concat(Content.contentTypes.tweet.liveNitterHosts.map(nitterHost => {
					let nitterURL = URLFromString(link.href);
					nitterURL.hostname = nitterHost;
					return nitterURL;
				}));
			},

			contentFromResponse: (response, link = null, loadURL) => {
				return {
					document: newDocument(response)
				};
			},

			referenceDataFromContent: (tweetPage, link) => {
				//	Link metadata for title-links.
				let titleLinkClass = "title-link link-live content-transform-not";
				let titleLinkIconMetadata = `data-link-icon-type="svg" data-link-icon="twitter"`;

				let nitterHost = Content.contentTypes.tweet.getNitterHost();

				//	URL for link to user’s page.
				let titleLinkURL = URLFromString(tweetPage.document.querySelector(".main-tweet a.username").href);
				titleLinkURL.hostname = nitterHost;
				let titleLinkHref = titleLinkURL.href;

				//	Avatar.
				let avatarImgElement = tweetPage.document.querySelector(".main-tweet img.avatar").cloneNode(true);
				avatarImgElement.setAttribute("style", avatarImgElement.getAttribute("style") 
													   + ";" 
													   + tweetPage.document.querySelector("style").innerHTML.match(/:root\{(.+?)\}/)[1]);
				let avatarImgSrcVar = avatarImgElement.style.getPropertyValue("background-image").match(/var\((.+?)\)/)[1];
				let avatarImgSrc = avatarImgElement.style.getPropertyValue(avatarImgSrcVar).match(/url\("(.+?)"\)/)[1];
				let avatarImg = newElement("IMG", { src: avatarImgSrc, class: "avatar figure-not" });

				//	Text of link to user’s page.
				let titleParts = tweetPage.document.querySelector("title").textContent.match(/^(.+?) \((@.+?)\):/);
				let titleText = `“${titleParts[1]}” (${titleParts[2]})`;
				let titleHTML = `${avatarImg.outerHTML}“${titleParts[1]}” (<code>${titleParts[2]}</code>)`;

				//	Link to tweet.
				let tweetDate = new Date(Date.parse(tweetPage.document.querySelector(".main-tweet .tweet-date").textContent));
				let tweetDateString = `${tweetDate.getFullYear()}-${tweetDate.getMonth()}-${tweetDate.getDate()}`;
				let tweetLinkURL = URLFromString(link.href);
				tweetLinkURL.hostname = nitterHost;
				tweetLinkURL.hash = "m";

				//	Secondary title links.
				let secondaryTitleLinksHTML = ` on <a href="${tweetLinkURL.href}" class="${titleLinkClass}" ${titleLinkIconMetadata}>${tweetDateString}</a>:`;

				//	Tweet content itself.
				let tweetContent = tweetPage.document.querySelector(".main-tweet .tweet-content").innerHTML.split("\n\n").map(graf => `<p>${graf}</p>`).join("\n");

				//	Attached media (video or images).
				tweetContent += Content.contentTypes.tweet.mediaEmbedHTML(tweetPage.document);

				//	Pop-frame title text.
				let popFrameTitleText = `${titleHTML} on ${tweetDateString}`;

				return {
					content: {
						titleHTML:                titleHTML,
						fullTitleHTML:            titleHTML,
						secondaryTitleLinksHTML:  secondaryTitleLinksHTML,
						titleText:                titleText,
						titleLinkHref:            titleLinkHref,
						titleLinkClass:           titleLinkClass,
						titleLinkIconMetadata:    titleLinkIconMetadata,
						abstract: 		          tweetContent,
						dataSourceClass:          "tweet",
					},
					template:                       "annotation-blockquote-outside",
					linkTarget:                     (GW.isMobile() ? "_self" : "_blank"),
					whichTab:                       (GW.isMobile() ? "current" : "new"),
					tabOrWindow:                    (GW.isMobile() ? "tab" : "window"),
					popFrameTitleText:              popFrameTitleText,
					popFrameTitleLinkHref:          tweetLinkURL.href
				};
			},

			mediaURLFromMetaTag: (mediaMetaTag, nitterHost) => {
				let mediaURL = URLFromString(mediaMetaTag.content);
				mediaURL.hostname = nitterHost;
				return mediaURL;
			},

			mediaEmbedHTML: (tweetDoc) => {
				let attachments = tweetDoc.querySelector(".main-tweet .attachments");
				if (attachments) {
					let mediaHTML = ``;
					attachments.querySelectorAll("img, video").forEach(mediaElement => {
						mediaHTML += `<figure>${mediaElement.outerHTML}</figure>`;
					});

					return mediaHTML;
				} else {
					return "";
				}
			},

			liveNitterHosts: [
				"nitter.net"
			],

			getNitterHost: () => {
				let hosts = Content.contentTypes.tweet.liveNitterHosts;
				return hosts[rollDie(hosts.length) - 1];
			}
		},

		localCodeFile: {
			matches: (link) => {
				//	Maybe it’s a foreign link?
				if (link.hostname != location.hostname)
					return false;

				//	Maybe it’s an annotated link?
				if (Annotations.isAnnotatedLinkFull(link))
					return false;

				//	Maybe it’s an aux-links link?
				if (link.pathname.startsWith("/metadata/"))
					return false;

				//	Maybe it’s a local document link?
				if (   link.pathname.startsWith("/doc/www/")
                	|| (   link.pathname.startsWith("/doc/")
                		&& link.pathname.match(/\.(html|pdf)$/i) != null))
                	return false;

				let codeFileURLRegExp = new RegExp(
					  '\\.('
					+ Content.contentTypes.localCodeFile.codeFileExtensions.join("|")
					+ ')$'
				, 'i');
				return codeFileURLRegExp.test(link.pathname);
			},

			/*  We first try to retrieve a syntax-highlighted version of the
				given code file, stored on the server as an HTML fragment. If
				present, we embed that. If there’s no such fragment, then we
				just embed the contents of the actual code file, in a
				<pre>-wrapped <code> element.
			 */
			sourceURLsForLink: (link) => {
				let codeFileURL = URLFromString(link.href);
				codeFileURL.hash = "";
				codeFileURL.search = "";

				let syntaxHighlightedCodeFileURL = URLFromString(codeFileURL.href);
				syntaxHighlightedCodeFileURL.pathname += ".html";

				return [ syntaxHighlightedCodeFileURL, codeFileURL ];
			},

			contentFromResponse: (response, link = null, loadURL) => {
				let codeDocument;

				//	Parse (encoding and wrapping first, if need be).
				if (   response.slice(0, 1) == "<"
					&& link.pathname.endsWithAnyOf([ ".html", ".xml", ".svg" ]) == false) {
					//	Syntax-highlighted code (already HTML-encoded).
					codeDocument = newDocument(response);

					//	We want <body> contents only, no metadata and such.
					let nodes = Array.from(codeDocument.childNodes);
					let codeWrapper = codeDocument.querySelector("div.sourceCode");
					codeDocument.replaceChildren(...(nodes.slice(nodes.indexOf(codeWrapper))));

					//	Mark truncated syntax-highlighted code files.
					if (codeWrapper.nextElementSibling?.tagName == "P")
						codeWrapper.classList.add("truncated");
				} else {
					//	“Raw” code.
					let htmlEncodedResponse = response.replace(
						/[<>]/g,
						c => ('&#' + c.charCodeAt(0) + ';')
					);
					codeDocument = newDocument(  `<pre class="raw-code"><code>`
											   + htmlEncodedResponse
											   + `</code></pre>`);
				}

				//	Inject line spans.
				let codeBlock = codeDocument.querySelector("code");
				codeBlock.innerHTML = codeBlock.innerHTML.split("\n").map(
					line => (`<span class="line">${(line || "&nbsp;")}</span>`)
				).join("\n");

				return {
					document: codeDocument
				};
			},

			referenceDataFromContent: (codePage, link = null) => {
				return {
					content: codePage.document
				};
			},

			codeFileExtensions: [
				//	Truncated at 1000 lines for preview.
				"bash", "c", "conf", "css", "csv", "diff", "hs", "html", "js",
				"json", "jsonl", "opml", "page", "patch", "php", "py", "R",
				"sh", "xml", "yaml",
				//	Non-syntax highlighted (due to lack of known format), but truncated:
				"txt"
			]
		},

		localFragment: {
			matches: (link) => {
				//	Maybe it’s a foreign link?
				if (link.hostname != location.hostname)
					return false;

				//	Maybe it’s an annotated link?
				if (Annotations.isAnnotatedLinkFull(link))
					return false;

				return (   link.pathname.startsWith("/metadata/")
						&& link.pathname.endsWith(".html"));
			},

			sourceURLsForLink: (link) => {
				let url = URLFromString(link.href);
				url.hash = "";
				url.search = "";

				return [ url ];
			},

			contentFromResponse: (response, link = null, loadURL) => {
				let fragment = newDocument(response);

				let auxLinksLinkType = AuxLinks.auxLinksLinkType(loadURL);
				if (auxLinksLinkType) {
					let auxLinksList = fragment.querySelector("ul, ol");
					if (auxLinksList) {
						auxLinksList.classList.add("aux-links-list", auxLinksLinkType + "-list");
						auxLinksList.previousElementSibling.classList.add("aux-links-list-label", auxLinksLinkType + "-list-label");

						if (auxLinksLinkType == "backlinks") {
							auxLinksList.querySelectorAll("blockquote").forEach(blockquote => {
								blockquote.classList.add("backlink-context");
							});
							auxLinksList.querySelectorAll("li > p").forEach(p => {
								p.classList.add("backlink-source");
							});
							auxLinksList.querySelectorAll(".backlink-source a:nth-of-type(2), .backlink-context a").forEach(auxLink => {
								auxLink.dataset.backlinkTargetUrl = AuxLinks.targetOfAuxLinksLink(loadURL);
							});
						}
					}
				}

				//  Fire contentDidLoad event, if need be.
				GW.notificationCenter.fireEvent("GW.contentDidLoad", {
					source: "Content.contentTypes.localFragment.load",
					container: fragment,
					document: fragment,
					loadLocation: link
				});

				return {
					document: fragment
				};
			},

			referenceDataFromContent: (fragment, link = null) => {
				return {
					content: fragment.document
				};
			},

		    permittedContentTypes: [ "text/html" ]
		},

		localPage: {
			matches: (link) => {
				//	Maybe it’s a foreign link?
				if (link.hostname != location.hostname)
					return false;

				//	Maybe it’s an annotated link?
				if (   Annotations.isAnnotatedLinkFull(link) == true
					&& Transclude.isContentTransclude(link) == false)
					return false;

				/*  If it has a period in it, it’s probably not a page, but is 
					something else, like a file of some sort, or a locally 
					archived document. Still, we allow for explicit overrides.
				 */
				return (   link.pathname.match(/\./) == null
						|| link.pathname.endsWith("/index")
						|| link.classList.contains("link-page"));
			},

			sourceURLsForLink: (link) => {
				let url = URLFromString(link.href);
				url.hash = "";
				url.search = "";

				return [ url ];
			},

			contentFromResponse: (response, link = null, loadURL) => {
				let page = response
						   ? newDocument(response)
						   : document;

				if (response)
					page.baseLocation = loadURL;

				//	Get the body classes.
				let pageBodyClasses = page.querySelector("meta[name='page-body-classes']").getAttribute("content").trim().split(" ");

				//  Get the page title.
				let pageTitle = page.querySelector("title").innerHTML.match(Content.contentTypes.localPage.pageTitleRegexp)[1];

				//	Get the page thumbnail URL and metadata.
				let pageThumbnailHTML;
				let pageThumbnailMetaTag = page.querySelector("meta[property='og:image']");
				if (pageThumbnailMetaTag) {
					let pageThumbnailURL = URLFromString(pageThumbnailMetaTag.getAttribute("content"));

					//	Alt text, if provided.
					let pageThumbnailAltMetaTag = page.querySelector("meta[property='og:image:alt']");
					let pageThumbnailAltText = (pageThumbnailAltMetaTag
												? pageThumbnailAltMetaTag.getAttribute("content")
												: `Thumbnail image for “${pageTitle}”`
												).replace(/"/g, "&quot;");

					//	Image dimensions.
					let pageThumbnailWidth = page.querySelector("meta[property='og:image:width']").getAttribute("content");
					let pageThumbnailHeight = page.querySelector("meta[property='og:image:height']").getAttribute("content");

					//	Construct and save the <img> tag.
					if (pageThumbnailURL.pathname.startsWith(Content.contentTypes.localPage.defaultPageThumbnailPathnamePrefix) == false)
						pageThumbnailHTML = `<img
							src="${pageThumbnailURL.href}"
							title="${pageThumbnailAltText}"
							width="${pageThumbnailWidth}"
							height="${pageThumbnailHeight}"
							style="width: ${pageThumbnailWidth}px; height: auto;"
								>`;

					//	Request the image, to cache it.
					doAjax({ location: pageThumbnailURL.href });
				}

				if (response) {
					//  Fire contentDidLoad event, if need be.
					GW.notificationCenter.fireEvent("GW.contentDidLoad", {
						source: "Content.contentTypes.localPage.load",
						container: page,
						document: page,
						loadLocation: loadURL
					});
				}

				return {
					title:          pageTitle,
					bodyClasses:    pageBodyClasses,
					thumbnailHTML:  pageThumbnailHTML,
					document:       page
				};
			},

			referenceDataFromContent: (page, link) => {
				//  The page content is the page body plus the metadata block.
				let pageContent = newDocument();
				//	Add the page metadata block.
				let pageMetadataBlock = page.document.querySelector("#page-metadata");
				if (pageMetadataBlock) {
					pageContent.append(newDocument(pageMetadataBlock));

					pageMetadataBlock = pageContent.querySelector("#page-metadata");
					pageMetadataBlock.classList.remove("markdownBody");
					if (pageMetadataBlock.className == "")
						pageMetadataBlock.removeAttribute("class");
				}
				//	Add the page main content block.
				pageContent.append(newDocument(page.document.querySelector("#markdownBody").childNodes));

				//	Find the target element and/or containing block, if any.
				let element = targetElementInDocument(link, pageContent);

				//	Pop-frame title text.
				let popFrameTitleTextParts = [ ];
				if (link.pathname != location.pathname)
					popFrameTitleTextParts.push(page.title);

				//	Section title or block id.
				if (element) {
					let nearestSection = element.closest("section");
					let nearestFootnote = element.closest("li.footnote");
					if (nearestFootnote) {
						popFrameTitleTextParts.push("Footnote", Notes.noteNumber(nearestFootnote));
						let identifyingSpan = nearestFootnote.querySelector("span[id]:empty");
						if (identifyingSpan)
							popFrameTitleTextParts.push(`(#${(identifyingSpan.id)})`);
					} else if (nearestSection) {
						//	Section mark (§) for sections.
						popFrameTitleTextParts.push("&#x00a7;");
						if (nearestSection.id == "footnotes") {
							popFrameTitleTextParts.push("Footnotes");
						} else {
							popFrameTitleTextParts.push(nearestSection.firstElementChild.textContent);
						}
					} else {
						popFrameTitleTextParts.push(link.hash);
					}
				}

				return {
					content:                 pageContent,
					pageTitle:               page.title,
					pageBodyClasses:         page.bodyClasses,
					pageThumbnailHTML:       page.thumbnailHTML,
					popFrameTitleLinkHref:   link.href,
					popFrameTitleText:       popFrameTitleTextParts.join(" "),
					popFrameTitleTextShort:  popFrameTitleTextParts.first
				}
			},

		    permittedContentTypes: [ "text/html" ],
			pageTitleRegexp: /^(.+?) · Gwern\.net( \(reader mode\))?$/,
			defaultPageThumbnailPathnamePrefix: "/static/img/logo/logo-"
		}
	}
};
/* author: Said Achmiz */
/* license: MIT */

/****************/
/* TRANSCLUSION */
/****************/

/*  Transclusion is dynamic insertion, into a document, of part or all of
    a different document.


    I. BASICS
    =========

    Put an include-link into the page, and at load time, the link will be
    replaced by the content it specifies.

    An include-link is a link (<a> tag) which has the `include` class, e.g.:

        <a class="include" href="/Sidenotes#comparisons"></a>

    At load time, this tag will be replaced with the `#comparisons` section of
    the /Sidenotes page.

    If the include-link’s URL (i.e., the value of its `href` attribute) has no
    hash (a.k.a. fragment identifier), then the entire page content will be
    transcluded. (If the page contains an element with the `markdownBody` ID,
    then only the contents of that element will be transcluded; otherwise, the
    contents of the `body` element will be transcluded; if neither element is
    present, then the complete contents of the page will be transcluded.)

    If the include-link’s URL has a hash, and the page content contains an
    element with an ID matching the hash, then only that element (or that
    element’s contents; see the `include-unwrap` option, below) will be
    transcluded. (If the URL has a hash but the hash does not identify any
    element contained in the page content, nothing is transcluded.)

    (See the ADVANCED section, below, for other ways to use an include-link’s
     URL hash to specify parts of a page to transclude.)


    II. OPTIONS
    ===========

    Several optional classes modify the behavior of include-links:

    include-annotation
    include-content
        If the include-link is an annotated link, then instead of transcluding 
        the linked content, the annotation for the linked content may be 
        transcluded.

        The default behavior is set via the
        Transclude.transcludeAnnotationsByDefault property. If this is set to
        `true`, then fully (not partially!) annotated links transclude the 
        annotation unless the `include-content` class is set (in which case they 
        transclude their linked content). If it is set to `false`, then fully
        annotated links transclude the annotation only if the 
        `include-annotation` class is set (otherwise they transclude their 
        linked content).

		Note that merely partially annotated links always default to 
		transcluding content, unless the `include-annotation` class is set.
		(See also the `include-annotation-partial` alias class.)

    include-strict
        By default, include-linked are lazy-loaded. A lazy-loaded include-link
        will not trigger (i.e., transclude its content) immediately at load
        time. Instead, it will wait until the user scrolls down to the part of
        the page where the link is located, or pops up a popup that contains
        that part of the page, or otherwise “looks” at the include-link’s
        surrounding context. Only then will the transclusion take place.
        A strict include-link, on the other hand, triggers immediately at
        load time.

        `include-strict` implies `include-even-when-collapsed`, because
        otherwise odd behavior can result (eg. a 'strict' transclusion in the
        first line or two of a collapse will be visibly untranscluded; and
        collapses blocking strict transclusion can lead to unpredictable 
        breakage when the contents of the transclusion are depended upon by the 
        rest of the page, and collapses are added/removed by editors).

    include-even-when-collapsed
        Normally, an include-link that is inside a collapsed block will not
        trigger at load time; instead, it will trigger only when it is revealed
        by expansion of its containing collapse block(s). The 
        `include-even-when-collapsed` class disables this delay, forcing the 
        include-link to trigger when revealed by scrolling (if it is not marked 
        as `include-strict`; otherwise, `include-strict` will force the 
        include-link to trigger at load time, regardless of anything to do with
        collapses) even if, at such time, it is within a collapsed block.

        Note that the `include-strict` and `include-even-when-collapsed` options 
        are not mutually exclusive, and do not do the same thing.

    include-unwrap
        Normally, when an include-link’s URL specifies an element ID to
        transclude, the element with that ID is transcluded in its entirety.
        When the `include-unwrap` option is used, the element itself is
        discarded, and only the element’s contents are transcluded.

        (This option has no effect unless the include-link’s URL hash specifies
         a single element ID to transclude.)

	include-block-context
	data-block-context-options
		Normally, when an include-link’s URL specifies an element ID to
		transclude, only (at most; see `include-unwrap`) that element is
		transcluded. When the `include-block-context` option is used, not only
		the identified element itself, but also its containing block element
		(and everything within) will be included. (What “block element” means
		in this context is not the same as what the HTML spec means by the
		term. Determination of what counts as a block element is done in a
		content-aware way.)

		If `include-unwrap` is used as well as `include-block-context`, then the
		identified element’s containing block will be unwrapped, and the
		included content will be all the child nodes of the identified element’s
		containing block.

        (This option has no effect unless the include-link’s URL hash specifies
         a single element ID to transclude.)

		The `data-block-context-options` attribute allows various options to be 
		specified for how block context should be determined and handled. The
		value of this attribute is a pipe (`|`) separated list of option fields.
		The following options may be specified:

		expanded
			Expanded block context mode omits paragraphs (the <p> element) from
			consideration as containing blocks.

    include-replace-container
        Normally, when transclusion occurs, the transcluded content replaces the
        include-link in the page, leaving any surrounding elements untouched.
        When the `include-replace-container` option is used, the include-link’s
        parent element, instead of just the include-link itself, is replaced by
        the transcluded content. (This means that any other contents of the
        include-link’s parent element are also discarded.)

    include-identify-not
        Normally, if the include-link has a nonempty ‘id’ attribute, and that
        ID does not occur in the transcluded content (after any unwrapping; see
        ‘include-unwrap’, above, for details), the content will be wrapped in a
        DIV element, which will be given the ID of the include-link. When the
        `include-identify-not` option is used, this will not be done.

	include-spinner
    include-spinner-not
        Shows or hides the “loading spinner” that is shown at the site of the
        include-link while content to be transcluded is being retrieved. In the
        absence of either of these classes, the spinner will be shown or not,
        depending on context. Using either class causes the spinner to be shown
        or not shown (respectively), unconditionally.


    III. ADVANCED
    =============

	1. Transclude range syntax
	--------------------------

    The transclusion feature supports PmWiki-style transclude range syntax,
    very similar to the one described here:
    https://www.pmwiki.org/wiki/PmWiki/IncludeOtherPages#includeanchor

    To use transclude range syntax, an include-link’s URL should have a “double”
    hash, i.e. a hash consisting of two ‘#’-prefixed parts:

        <a class="include" href="/Sidenotes#tufte-css#tables"></a>

    This will include all parts of the "/Sidenotes" page’s content starting from
    the element with ID `tufte-css`, all the way up to (but *not* including!) 
    the element with ID `tables`.

    Either the first or the second identifier (the parts after the ‘#’) may
    instead be empty. The possibilities are:

    #foo#bar
        Include everything starting from element `#foo` up to (but not
        including) element `#bar`.

    ##bar
        Include everything from the start of the page content up to (but not
        including) element `#bar`.

    #foo#
        Include everything starting from element `#foo` to the end of the page.

    ##
        Include the entire page content (same as not having a hash at all).

    In all cases, only the page content is considered, not any “page furniture”
    (i.e., only the contents of `#markdownBody`, if present; or only the
     contents of `<body>`, if present; or the whole page, otherwise).

    If an element of one of the specified IDs is not found in the page, the
    transclusion fails.

    If both elements are present, but the end element does not follow the start
    element in the page order (i.e., if the start element comes after the end
    element, or if they are the same), then the transcluded content is empty.

	2. Include template
	-------------------

	The `data-include-template` attribute allows selection of include template
	to use. (Note that some include data sources specify a template by default;
	the `data-include-template` attribute overrides the default in such cases.)
	If a template is specified, the included content is treated as a template
	data source, rather than being included directly. (See comment for the
	templateDataFromHTML() function for information about how template data
	is specified in HTML. Note that some data sources provide template data in
	pre-constructed object form, which bypasses the need to extract it from
	HTML source.)

	3. Selector-based inclusion/exclusion
	-------------------------------------

	The `data-include-selector` and `data-include-selector-not` attributes allow
	the use of CSS selectors to specify parts of the included DOM subtree to 
	include or omit. (If both attributes are present, 
	`data-include-selector-not` is applied first.)

	(NOTE: `data-include-selector` may be seen as a generalization of the 
	 `include-block-context` option, described above. Note, however, that both
	 `include-block-context` and either or both of `data-include-selector` / 
	 `data-include-selector-not` may be used simultaneously. The effects of the 
	 data attributes are applied last, after all `include-*` options have been
	 applied.)
 */

/******************************************************************************/
/*	Extract template data from an HTML string or DOM object by looking for
	elements with either the `data-template-field` or the
	`data-template-fields` attribute.

	If the `data-template-fields` attribute is not present but the
	`data-template-field` attribute is present, then the value of the latter
	attribute is treated as the data field name; the .innerHTML of the
	element is the field value.

	If the `data-template-fields` attribute is present, then the attribute
	value is treated as a comma-separated list of
	`fieldName:fieldValueIdentifier` pairs. For each pair, the part before the
	colon (the fieldName) is the data field name. The part after the colon
	(the fieldValueIdentifier) can be interpreted in one of two ways:

	If the fieldValueIdentifier begins with a dollar sign (the ‘$’ character),
	then the rest of the identifier (after the dollar sign) is treated as the
	name of the attribute of the given element which holds the field value.

	If the fieldValueIdentifier is _only_ the ‘$’ character, then the field
	value will be the value of the data attribute that corresponds to the
	field name (i.e., if the field is `fooBar`, then the field value will be
	taken from attribute `data-foo-bar`).

	If the fieldValueIdentifier begins with a period (the ‘.’ character), then
	the rest of the identifier (after the period) is treated as the name of the
	DOM object property of the given element which holds the field value.

	If the fieldValueIdentifier is _only_ the ‘.’ character, then the field
	value will be the value of the element property matching the field name
	(i.e., if the field name is `fooBar`, then the field value will be the
	value of the element’s .fooBar property).

	Examples:

		<span data-template-field="foo">Bar</span>

	This element defines a data field with name `foo` and value `Bar`.

		<span data-template-field="foo:$title" title="Bar"></span>

	This element defines a data field with name `foo` and value `Bar`.

		<span data-template-field="foo:$title, bar:.tagName" title="Baz"></span>

	This element defines two data fields: one with name `foo` and value `Baz`,
	and one with name `bar` and value `SPAN`.

		<span data-template-field="foo:title" title="Bar"></span>

	This element defines no data fields.
 */
//	(string|Document|DocumentFragment|Element) => object
function templateDataFromHTML(html) {
	let dataObject = { };

	if ((   html instanceof Document
		 || html instanceof DocumentFragment) == false)
		html = newDocument(html);

	html.querySelectorAll("[data-template-field], [data-template-fields]").forEach(element => {
		if (element.dataset.templateFields) {
			element.dataset.templateFields.split(",").forEach(templateField => {
				let [ beforeColon, afterColon ] = templateField.trim().split(":");
				let fieldName = beforeColon.trim();
				let fieldValueIdentifier = afterColon.trim();

				if (fieldValueIdentifier.startsWith(".")) {
					dataObject[fieldName] = fieldValueIdentifier == "."
											? element[fieldName]
											: element[fieldValueIdentifier.slice(1)];
				} else if (fieldValueIdentifier.startsWith("$")) {
					dataObject[fieldName] = fieldValueIdentifier == "$"
											? element.dataset[fieldName]
											: element.getAttribute(fieldValueIdentifier.slice(1));
				}
			});
		} else {
			dataObject[element.dataset.templateField] = element.innerHTML;
		}
	});

	return dataObject;
}

/***************************************************************************/
/*	Returns true or false, based on the value of defined template expression
	constants. (Returns false for unknown constant name.)
 */
function evaluateTemplateExpressionConstant(constant) {
	if (constant == "_TRUE_")
		return true;

	if (constant == "_FALSE_")
		return false;

	return false;
}

/************************************************************************/
/*	Return either true or false, having evaluated the template expression
	(used in conditionals, e.g. `<[IF !foo & bar]>baz<[IFEND]>`).
 */
function evaluateTemplateExpression(expr, valueFunction = (() => null)) {
	if (expr == "_TRUE_")
		return true;

	if (expr == "_FALSE_")
		return false;

	return evaluateTemplateExpressionConstant(expr.replace(
		//	Brackets.
		/\s*\[\s*(.+?)\s*\]\s*/g,
		(match, bracketedExpr) =>
		(evaluateTemplateExpression(bracketedExpr, valueFunction)
		 ? "_TRUE_"
		 : "_FALSE_")
	).replace(
		//	Boolean AND, OR.
		/^\s*([^&|]+?)\s*([&|])\s*(.+?)\s*$/,
		(match, leftOperand, operator, rightOperand) => {
			let leftOperandTrue = evaluateTemplateExpression(leftOperand, valueFunction);
			let rightOperandTrue = evaluateTemplateExpression(rightOperand, valueFunction);
			let expressionTrue = operator == "&"
								 ? (leftOperandTrue && rightOperandTrue)
								 : (leftOperandTrue || rightOperandTrue);
			return expressionTrue ? "_TRUE_" : "_FALSE_";
		}
	).replace(
		//	Boolean NOT.
		/^\s*!\s*(.+?)\s*$/,
		(match, operand) =>
		(evaluateTemplateExpression(operand, valueFunction)
		 ? "_FALSE_"
		 : "_TRUE_")
	).replace(/^\s*(.+)\s*$/g,
		(match, fieldName) =>
		(/^_(.*)_$/.test(fieldName)
		 ? fieldName
		 : (valueFunction(fieldName) == null
			? "_FALSE_"
			: "_TRUE_"))
	));
}

/******************************************************************************/
/*	Fill a template with provided reference data (supplemented by an optional
	context object).

	Reference data may be a data object, or else an HTML string (in which case
	the templateDataFromHTML() function is used to extract data from the HTML).

	If no ‘data’ argument is provided, then the template itself will be parsed
	to extract reference data (again, using the templateDataFromHTML()
	function).

	(Context argument must be an object, not a string.)

	Available options (defaults):

		preserveSurroundingWhitespaceInConditionals (false)
			If true, `<[IF foo]> bar <[IFEND]>` becomes ` bar `;
			if false, `bar`.

		fireContentLoadEvent (false)
			If true, a GW.contentDidLoad event is fired on the filled template.
 */
//	(string, string|object, object, object) => DocumentFragment
function fillTemplate(template, data = null, context = null, options = { }) {
	if (   template == null
		|| template == "LOADING_FAILED")
		return null;

	//	If no data source is provided, use the template itself as data source.
	if (   data == null
		|| data == "LOADING_FAILED")
		data = template;

	/*	If the data source is a string, assume it to be HTML and extract data;
		likewise, if the data source is a DocumentFragment, extract data.
	 */
	if (   typeof data == "string"
		|| data instanceof DocumentFragment)
		data = templateDataFromHTML(data);

	/*	Data variables specified in the provided context argument (if any)
		take precedence over the reference data.
	 */
	let valueFunction = (fieldName) => {
		return (context && context[fieldName]
				? context[fieldName]
				: (data ? data[fieldName] : null));
	};

	//	Line continuations.
	template = template.replace(
		/>\\\n\s*</gs,
		(match) => "><"
	);

	//	Comments.
	template = template.replace(
		/<\(.+?\)>/gs,
		(match) => ""
	);

	//	Escapes.
	template = template.replace(
		/\\(.)/gs,
		(match, escaped) => "<[:" + escaped.codePointAt(0) + ":]>"
	);

	/*	Conditionals. JavaScript’s regexps do not support recursion, so we
		keep running the replacement until no conditionals remain.
	 */
	let didReplace;
	do {
		didReplace = false;
		template = template.replace(
			/<\[IF([0-9]*)\s+(.+?)\]>(.+?)(?:<\[ELSE\1\]>(.+?))?<\[IF\1END\]>/gs,
			(match, nestLevel, expr, ifValue, elseValue) => {
				didReplace = true;
				let returnValue = evaluateTemplateExpression(expr, valueFunction)
								  ? (ifValue ?? "")
								  : (elseValue ?? "");
				return options.preserveSurroundingWhitespaceInConditionals
					   ? returnValue
					   : returnValue.trim();
			});
	} while (didReplace);

	//	Data variable substitution.
	template = template.replace(
		/<\{(.+?)\}>/g,
		(match, fieldName) => (valueFunction(fieldName) ?? "")
	);

	//	Escapes, redux.
	template = template.replace(
		/<\[:(.+?):\]>/gs,
		(match, codePointSequence) => String.fromCodePoint(...(codePointSequence.split("/").map(x => parseInt(x))))
	);

	//	Construct DOM tree from filled template.
	let outputDocument = newDocument(template);

	//	Fire GW.contentDidLoad event, if need be.
	if (options.fireContentLoadEvent) {
		let loadEventInfo = {
            container: outputDocument,
            document: outputDocument
        };

		if (options.loadEventInfo)
			for ([key, value] of Object.entries(options.loadEventInfo))
				if ([ "container", "document" ].includes(key) == false)
					loadEventInfo[key] = value;

		GW.notificationCenter.fireEvent("GW.contentDidLoad", loadEventInfo);
	}

	return outputDocument;
}

/*****************************************************************************/
/*	Construct synthetic include-link. The optional ‘link’ argument may be
	a string, a URL object, or an HTMLAnchorElement, in which case it, or its
	.href property, is used as the ‘href’ attribute of the synthesized
	include-link.
 */
function synthesizeIncludeLink(link, attributes, properties) {
	let includeLink = newElement("A", attributes, properties);

	if (link == null)
		return includeLink;

	if (typeof link == "string")
		includeLink.href = link;
	else if (   link instanceof HTMLAnchorElement
			 || link instanceof URL)
		includeLink.href = link.href;

	if (   link instanceof HTMLAnchorElement
		&& link.dataset.backlinkTargetUrl)
		includeLink.dataset.backlinkTargetUrl = link.dataset.backlinkTargetUrl;

	//	In case no include classes have been added yet...
	if (Transclude.isIncludeLink(includeLink) == false)
		includeLink.classList.add("include");

	return includeLink;
}

/*************************************************************************/
/*	Return appropriate loadLocation for given include-link. (May be null.)
 */
function loadLocationForIncludeLink(includeLink) {
    if (Transclude.isAnnotationTransclude(includeLink) == false) {
    	contentSourceURLs = Content.sourceURLsForLink(includeLink);
    	return contentSourceURLs
			   ? contentSourceURLs.first
			   : includeLink.eventInfo.loadLocation;
    } else {
    	return null;
    }
}

/***********************************************************************/
/*  Replace an include-link with the given content (a DocumentFragment).
 */
//  Called by: Transclude.transclude
function includeContent(includeLink, content) {
    GWLog("includeContent", "transclude.js", 2);

	/*  We skip include-links for which a transclude operation is already in
		progress or has completed (which might happen if we’re given an
		include-link to process, but that link has already been replaced by its
		transcluded content and has been removed from the document).
	 */
	if (includeLink.classList.containsAnyOf([
		"include-in-progress",
		"include-complete"
	])) return;

	//	Where to inject?
    let replaceContainer = (   includeLink.parentElement != null
                            && includeLink.classList.contains("include-replace-container"));
    let insertWhere = replaceContainer
                      ? includeLink.parentElement
                      : includeLink;

    /*  Just in case, do nothing if the element-to-be-replaced (either the
    	include-link itself, or its container, as appropriate) isn’t attached
    	to anything.
     */
    if (insertWhere.parentNode == null)
        return;

    //  Prevent race condition, part I.
    includeLink.classList.add("include-in-progress");

    //  Document into which the transclusion is being done.
    let containingDocument = includeLink.eventInfo.document;
    let transcludingIntoFullPage = (containingDocument.querySelector("#page-metadata") != null);

    //  Save reference for potential removal later.
    let includeLinkParentElement = includeLink.parentElement;

	//	WITHIN-WRAPPER MODIFICATIONS BEGIN

    //  Wrap (unwrapping first, if need be).
    let wrapper = newElement("SPAN", { "class": "include-wrapper" });
    if (   includeLink.classList.contains("include-unwrap")
        && isAnchorLink(includeLink)
        && content.childElementCount == 1) {
		wrapper.id = content.firstElementChild.id;
		wrapper.append(...content.firstElementChild.childNodes);
    } else {
        wrapper.append(content);
    }

    //  Inject wrapper.
    insertWhere.parentNode.insertBefore(wrapper, insertWhere);

    /*  When transcluding into a full page, delete various “metadata” sections
    	such as page-metadata, footnotes, etc. (Save references to some.)
     */
	let newContentFootnotesSection = wrapper.querySelector("#footnotes");
    if (transcludingIntoFullPage) {
    	let metadataSectionsSelector = [
    		"#page-metadata",
    		"#footnotes",
    		"#further-reading",
    		"#similars-section",
    		"#link-bibliography-section"
    	].join(", ");
    	wrapper.querySelectorAll(metadataSectionsSelector).forEach(section => {
    		section.remove();
    	});
    }

    //  ID transplantation.
    if (   includeLink.id > ""
        && includeLink.classList.contains("include-identify-not") == false
        && wrapper.querySelector(`#${(CSS.escape(includeLink.id))}`) == null) {
        let idBearerBlock = newElement("DIV", { "id": includeLink.id, "class": "include-wrapper-block" });
        idBearerBlock.append(...wrapper.childNodes);
        wrapper.append(idBearerBlock);
    }

	//	Intelligent rectification of contained HTML structure.
	if (wrapper.closest("#footnotes > ol") == null) {
		wrapper.querySelectorAll(".footnote-self-link").forEach(link => {
			link.remove();
		});
		if (wrapper.querySelector("#footnotes > ol") == null) {
			wrapper.querySelectorAll(".footnote-back").forEach(link => {
				link.remove();
			});
		}
	}

	//	Clear loading state of all include-links.
	Transclude.allIncludeLinksInContainer(wrapper).forEach(Transclude.clearLinkState);

    //  Fire GW.contentDidInject event.
	let flags = GW.contentDidInjectEventFlags.clickable;
	if (containingDocument == document)
		flags |= GW.contentDidInjectEventFlags.fullWidthPossible;
	let contentType = null;
	if (   Transclude.isAnnotationTransclude(includeLink)
		|| (   Content.contentTypes.localFragment.matches(includeLink)
			&& /^\/metadata\/annotation\/[^\/]+$/.test(includeLink.pathname)))
		contentType = "annotation";
	GW.notificationCenter.fireEvent("GW.contentDidInject", {
		source: "transclude",
		contentType: contentType,
		context: includeLink.eventInfo.context,
		container: wrapper,
		document: containingDocument,
		loadLocation: loadLocationForIncludeLink(includeLink),
		flags: flags,
		includeLink: includeLink
	});

	//	WITHIN-WRAPPER MODIFICATIONS END; OTHER MODIFICATIONS BEGIN

	//	Distribute backlinks, if need be.
	if (   transcludingIntoFullPage
		&& AuxLinks.auxLinksLinkType(includeLink) == "backlinks"
		&& wrapper.closest("#backlinks-section") != null)
		distributeSectionBacklinks(includeLink, wrapper);

    //  Update footnotes, if need be, when transcluding into a full page.
    if (   transcludingIntoFullPage
    	&& Transclude.isAnnotationTransclude(includeLink) == false)
        updateFootnotesAfterInclusion(includeLink, wrapper, newContentFootnotesSection);

	//  Update TOC, if need be, when transcluding into the base page.
    if (   containingDocument == document
    	&& Transclude.isAnnotationTransclude(includeLink) == false)
        updatePageTOCIfNeeded(includeLink.eventInfo);

	//	Aggregate margin notes.
	aggregateMarginNotesIfNeeded(includeLink.eventInfo);

	//	Import style sheets, if need be.
	if (   containingDocument == document
		|| containingDocument instanceof ShadowRoot)
		importStylesAfterTransclusion(includeLink, wrapper);

    //  Remove extraneous text node after link, if any.
    if (   replaceContainer == false
        && includeLink.nextSibling
        && includeLink.nextSibling.nodeType == Node.TEXT_NODE) {
        let cleanedNodeContents = Typography.processString(includeLink.nextSibling.textContent, Typography.replacementTypes.CLEAN);
        if (   cleanedNodeContents.match(/\S/) == null
        	|| cleanedNodeContents == ".")
	        includeLink.parentNode.removeChild(includeLink.nextSibling);
    }

    //  Remove link.
    if (replaceContainer == false)
        includeLink.remove();

    //  Intelligent rectification of surrounding HTML structure.
    if (   Transclude.isAnnotationTransclude(includeLink)
        && replaceContainer == false) {
        let allowedParentTags = [ "SECTION", "DIV" ];

        //  Special handling for annotation transcludes in link bibliographies.
		/*	NOTE: Provisionally disabling this conditional to determine whether
			it can be removed, and "LI" added to the allowedParentTags array
			unconditionally.
			—SA 2023-04-18
		 */
//         if (   wrapper.parentElement != null
//         	&& wrapper.parentElement.closest(".link-bibliography-list") != null)
            allowedParentTags.push("LI");

        while (   wrapper.parentElement != null
               && wrapper.parentElement.parentElement != null
        	   && false == allowedParentTags.includes(wrapper.parentElement.tagName)) {
            let nextNode = wrapper.nextSibling;

            wrapper.parentElement.parentElement.insertBefore(wrapper, wrapper.parentElement.nextSibling);

            if (isNodeEmpty(wrapper.previousSibling)) {
                wrapper.previousSibling.remove();
                continue;
            }

            if (nextNode == null)
                continue;

            let firstPart = wrapper.previousSibling;
            let secondPart = newElement(firstPart.tagName);
            if (firstPart.className > "")
                secondPart.className = firstPart.className;
            while (nextNode) {
                let thisNode = nextNode;
                nextNode = nextNode.nextSibling;
                secondPart.appendChild(thisNode);
            }

            if (isNodeEmpty(firstPart) == true)
                firstPart.remove();

            if (isNodeEmpty(secondPart) == false)
                wrapper.parentElement.insertBefore(secondPart, wrapper.nextSibling);
        }
    }

	//	Retain reference to nodes.
	let addedNodes = Array.from(wrapper.childNodes);

    //  Unwrap.
    unwrap(wrapper);

    //  Remove include-link’s container, if specified.
    if (replaceContainer)
        includeLinkParentElement.remove();

	//	OTHER MODIFICATIONS END

    //  Prevent race condition, part II.
    includeLink.classList.add("include-complete");
    includeLink.classList.remove("include-in-progress");

    //  Fire event, if need be.
    if (includeLink.delayed) {
        GW.notificationCenter.fireEvent("Rewrite.contentDidChange", {
            source: "transclude",
            document: containingDocument,
            nodes: addedNodes
        });
    }

	//	Activity ends.
	endActivity();
}

/*****************************************************************************/
/*	Distributes, to each section of the page, all backlinks that point to that
	section specifically.
 */
function distributeSectionBacklinks(includeLink, mainBacklinksBlockWrapper) {
	let containingDocument = includeLink.eventInfo.document;

	let prefix = `gwern-${(includeLink.eventInfo.loadLocation.pathname.slice(1))}-`;

	mainBacklinksBlockWrapper.querySelectorAll(".backlink-context a[data-target-id]").forEach(backlinkContextLink => {
		let id = backlinkContextLink.dataset.targetId.slice(prefix.length);
		if (id == "")
			return;

		let targetElement = containingDocument.querySelector(`#${(CSS.escape(id))}`);
		if (targetElement == null)
			return;

		let targetBlock = targetElement.closest("section, li.footnote");
		if (targetBlock == null)
			return;

		let backlinksBlock = targetBlock.querySelector(".section-backlinks");
		if (backlinksBlock == null) {
			//	Backlinks block.
			backlinksBlock = newElement("DIV", { "class": "section-backlinks", "id": `${id}-backlinks` });

			//	Label.
			backlinksBlock.append(mainBacklinksBlockWrapper.querySelector("#backlinks").firstElementChild.cloneNode(true));
			let sectionLabelLinkTarget = baseLocationForDocument(containingDocument).pathname + "#" + targetBlock.id;
			let sectionLabelHTML = targetBlock.tagName == "SECTION"
								   ? `“${(targetBlock.firstElementChild.textContent)}”`
								   : `footnote <span class="footnote-number">${(Notes.noteNumberFromHash(targetBlock.id))}</span>`;
			backlinksBlock.querySelector("p strong").innerHTML = `Backlinks for <a href="${sectionLabelLinkTarget}" class="link-page">${sectionLabelHTML}</a>:`;

			//	List.
			backlinksBlock.append(newElement("UL", { "class": "aux-links-list backlinks-list" }));

			//	Collapse wrapper.
			let collapseWrapper = newElement("DIV", { "class": "collapse aux-links-append section-backlinks-container" });
			collapseWrapper.append(backlinksBlock);

			//	Include wrapper.
			let includeWrapper = newElement("DIV", { "class": "include-wrapper section-backlinks-include-wrapper" });
			includeWrapper.append(collapseWrapper);
			let container = targetBlock.classList.contains("collapse")
							? (targetBlock.querySelector(".collapse-content-wrapper") ?? targetBlock)
							: targetBlock;
			container.append(includeWrapper);
		}

		backlinksBlock.querySelector(".backlinks-list").append(backlinkContextLink.closest("li").cloneNode(true));
	});

	containingDocument.querySelectorAll(".section-backlinks-include-wrapper").forEach(includeWrapper => {
		//	Clear loading state of all include-links.
		Transclude.allIncludeLinksInContainer(includeWrapper).forEach(Transclude.clearLinkState);

		//	Fire load event.
		GW.notificationCenter.fireEvent("GW.contentDidLoad", {
			source: "transclude.section-backlinks",
			container: includeWrapper,
			document: containingDocument,
			loadLocation: loadLocationForIncludeLink(includeLink)
		});

		//	Fire inject event.
		let flags = GW.contentDidInjectEventFlags.clickable;
		if (containingDocument == document)
			flags |= GW.contentDidInjectEventFlags.fullWidthPossible;
		GW.notificationCenter.fireEvent("GW.contentDidInject", {
			source: "transclude.section-backlinks",
			container: includeWrapper,
			document: containingDocument,
			loadLocation: loadLocationForIncludeLink(includeLink),
			flags: flags
		});

		unwrap(includeWrapper);
	});
}

/*****************************************************************************/
/*	Returns true iff a given document contains a style sheet identified by the
	given selector.
 */
function documentHasStyleSheet(doc, selector) {
	if (doc == document)
		return (doc.head.querySelector(selector) != null);
	else if (doc instanceof ShadowRoot)
		return (doc.body.querySelector(selector) != null);
	else
		return false;
}

/*****************************************************************************/
/*	Imports needed styles (<style> and/or <link> elements) after transclusion.
 */
function importStylesAfterTransclusion(includeLink, wrapper) {
	let containingDocument = includeLink.eventInfo.document;
	let newContentSourceDocument = Transclude.dataProviderForLink(includeLink).cachedDocumentForLink(includeLink);

	if (newContentSourceDocument == null)
		return;

	let styleDefs = [
		[ "#mathjax-styles", ".mjpage" ]
	];

	styleDefs.forEach(styleDef => {
		let [ styleSheetSelector, elementSelector ] = styleDef;
		let stylesheet = newContentSourceDocument.querySelector(styleSheetSelector);
		if (   stylesheet
			&& (elementSelector 
				? containingDocument.querySelector(elementSelector) != null
				: true)) {
			/*	Add stylesheet to root document in all cases, if need be.
				(If this is not done, then fonts will not be loaded.)
			 */
			if (documentHasStyleSheet(document, styleSheetSelector) == false)
				document.head.append(stylesheet.cloneNode(true));

			/*	If containing document is a shadow root, give it a copy of the
				style sheet also.
			 */
			if (containingDocument instanceof ShadowRoot)
				containingDocument.insertBefore(stylesheet.cloneNode(true), containingDocument.body);
		}
	});
}

/**************************************************************************/
/*  Updates footnotes section after transclusion.

    Returns wrapper element of the added footnotes, if any; null otherwise.
 */
//  Called by: includeContent
function updateFootnotesAfterInclusion(includeLink, newContent, newContentFootnotesSection) {
    GWLog("updateFootnotesAfterInclusion", "transclude.js", 2);

	/*	If the transcluded content didn’t include the footnotes section of the
		source page, attempt to get the footnotes section from the cached full
		document that the new content was sliced from.
	 */
    if (   newContentFootnotesSection == null
    	&& Transclude.isAnnotationTransclude(includeLink) == false) {
    	let newContentSourceDocument = Content.cachedDocumentForLink(includeLink);
    	if (newContentSourceDocument)
    		newContentFootnotesSection = newContentSourceDocument.querySelector("#footnotes");
    }

    let citationsInNewContent = newContent.querySelectorAll(".footnote-ref");
    if (   citationsInNewContent.length == 0
        || newContentFootnotesSection == null)
        return;

    let containingDocument = includeLink.eventInfo.document;

	//	If the host page doesn’t have a footnotes section, construct one.
    let footnotesSection = containingDocument.querySelector(".markdownBody > #footnotes");
    if (!footnotesSection) {
        //  Construct footnotes section.
        footnotesSection = newElement("SECTION", { "id": "footnotes", "class": "footnotes", "role": "doc-endnotes" });
        footnotesSection.append(newElement("HR"));
        footnotesSection.append(newElement("OL"));

        //  Wrap.
        let footnotesSectionWrapper = newElement("SPAN", { "class": "include-wrapper" });
        footnotesSectionWrapper.append(footnotesSection);

        //  Inject.
        let markdownBody = (containingDocument.querySelector("#markdownBody") ?? containingDocument.querySelector(".markdownBody"));
        markdownBody.append(footnotesSectionWrapper);

        //  Fire events.
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "transclude.footnotesSection",
            container: footnotesSectionWrapper,
            document: containingDocument
        });
		GW.notificationCenter.fireEvent("GW.contentDidInject", {
			source: "transclude.footnotesSection",
			container: footnotesSectionWrapper,
			document: containingDocument,
            flags: 0
		});

        //  Update page TOC to add footnotes section entry.
        updatePageTOCIfNeeded(includeLink.eventInfo);

        //  Unwrap.
        unwrap(footnotesSectionWrapper);
    }

	//	Construct wrapper.
    let newFootnotesWrapper = newElement("OL", { "class": "include-wrapper" });

	//	Add new footnotes to wrapper.
    citationsInNewContent.forEach(citation => {
        //  Original footnote (in source content/document).
        let footnote = newContentFootnotesSection.querySelector(Notes.footnoteSelectorMatching(citation));

		//	Determine footnote’s source page, and its note number on that page.
		let sourcePagePathname = (footnote.dataset.sourcePagePathname ?? loadLocationForIncludeLink(includeLink).pathname);
		let originalNoteNumber = (footnote.dataset.originalNoteNumber ?? Notes.noteNumber(citation));

		//	Check for already added copy of this footnote.
		let alreadyAddedFootnote = footnotesSection.querySelector(`li.footnote`
								 + `[data-source-page-pathname='${(CSS.escape(sourcePagePathname))}']`
								 + `[data-original-note-number='${originalNoteNumber}']`);

        //  Copy the footnote, or keep a pointer to it.
        citation.footnote = (alreadyAddedFootnote ?? newFootnotesWrapper.appendChild(document.importNode(footnote, true)));

		if (alreadyAddedFootnote == null) {
			//	Record source page and original number.
			citation.footnote.dataset.sourcePagePathname = sourcePagePathname;
			citation.footnote.dataset.originalNoteNumber = originalNoteNumber;
		}
    });

	//	Inject wrapper.
    footnotesSection.appendChild(newFootnotesWrapper);

	//	Fire GW.contentDidLoad event.
	GW.notificationCenter.fireEvent("GW.contentDidLoad", {
		source: "transclude.footnotes",
		container: newFootnotesWrapper,
		document: containingDocument,
		loadLocation: loadLocationForIncludeLink(includeLink)
	});

	//	Parent element of footnotes.
	let footnotesList = footnotesSection.querySelector("ol");

	//	Merge and unwrap.
	footnotesList.append(...(newFootnotesWrapper.children));

	//	Re-number citations/footnotes, and re-order footnotes.
	let footnoteNumber = 1;
	containingDocument.querySelectorAll(".footnote-ref").forEach(citation => {
		if (citation.closest(".sidenote"))
			return;

		let footnote = citation.footnote ?? footnotesSection.querySelector(Notes.footnoteSelectorMatching(citation));

		if (footnote.parentElement == newFootnotesWrapper) {
			Notes.setCitationNumber(citation, Notes.noteNumber(footnote));
		} else {
			Notes.setCitationNumber(citation, footnoteNumber);
			Notes.setFootnoteNumber(footnote, footnoteNumber);

			newFootnotesWrapper.appendChild(footnote);

			footnoteNumber++;
		}
	});

	//	Fire inject event.
	let flags = GW.contentDidInjectEventFlags.clickable;
	if (containingDocument == document)
		flags |= GW.contentDidInjectEventFlags.fullWidthPossible;
	GW.notificationCenter.fireEvent("GW.contentDidInject", {
		source: "transclude.footnotes",
		container: newFootnotesWrapper,
		document: containingDocument,
		loadLocation: loadLocationForIncludeLink(includeLink),
		flags: flags
	});

	//	Merge and unwrap (redux).
	footnotesList.append(...(newFootnotesWrapper.children));

	//	Discard wrapper.
	newFootnotesWrapper.remove();
}

/***********************************************************************/
/*  Handles interactions between include-links and content at locations.
 */
Transclude = {
    /*****************/
    /*  Configuration.
     */

    permittedClassNames: [
        "include",
        "include-annotation",
        "include-content",
        "include-strict",
        "include-even-when-collapsed",
        "include-unwrap",
        "include-block-context",
        "include-replace-container",
        "include-identify-not"
    ],

    transcludeAnnotationsByDefault: true,

    lazyLoadViewportMargin: "100%",

    /******************************/
    /*  Detection of include-links.
     */

    isIncludeLink: (link) => {
        return link.classList.containsAnyOf(Transclude.permittedClassNames);
    },

    allIncludeLinksInContainer: (container) => {
        return Array.from(container.querySelectorAll("a[class*='include']")).filter(link => Transclude.isIncludeLink(link));
    },

	isContentTransclude: (link) => {
		if (Transclude.isIncludeLink(link) == false)
			return false;

        if ((   Transclude.hasFullAnnotation(link) 
        	 || link.classList.contains("include-annotation")
        	 ) == false)
            return true;

		return ((   Transclude.transcludeAnnotationsByDefault
				 && Transclude.hasFullAnnotation(link))
				? link.classList.contains("include-content") == true
				: link.classList.contains("include-annotation") == false);
	},

    isAnnotationTransclude: (link) => {
		if (Transclude.isIncludeLink(link) == false)
			return false;

        if ((   Transclude.hasFullAnnotation(link) 
        	 || link.classList.contains("include-annotation")
        	 ) == false)
            return false;

        return ((   Transclude.transcludeAnnotationsByDefault
        		 && Transclude.hasFullAnnotation(link))
                ? link.classList.contains("include-content") == false
                : link.classList.contains("include-annotation") == true);
    },

	hasAnnotation: (link) => {
		return (Annotations.isAnnotatedLink(link));
	},

	hasFullAnnotation: (link) => {
		return (Annotations.isAnnotatedLinkFull(link));
	},

    /**************/
    /*  Templating.
     */

	templates: { },

	doWhenTemplateLoaded: (templateName, loadHandler, loadFailHandler = null) => {
		let template = Transclude.templates[templateName];
		if (template == "LOADING_FAILED") {
			if (loadFailHandler)
				loadFailHandler();
		} else if (template) {
			loadHandler(template);
		} else {
			let loadOrFailHandler = (info) => {
				if (info.eventName == "Transclude.templateDidLoad") {
					loadHandler(Transclude.templates[templateName], true);

					GW.notificationCenter.removeHandlerForEvent("Transclude.templateLoadDidFail", loadOrFailHandler);
				} else {
					if (loadFailHandler)
						loadFailHandler(null, true);

					GW.notificationCenter.removeHandlerForEvent("Transclude.templateDidLoad", loadOrFailHandler);
				}
			};
			GW.notificationCenter.addHandlerForEvent("Transclude.templateDidLoad", loadOrFailHandler, {
				once: true,
				condition: (info) => info.templateName == templateName
			});
			GW.notificationCenter.addHandlerForEvent("Transclude.templateLoadDidFail", loadOrFailHandler, {
				once: true,
				condition: (info) => info.templateName == templateName
			});
		}
	},

	//	(string, string|object, object) => DocumentFragment
	fillTemplateNamed: (templateName, data, context, options) => {
		return fillTemplate(Transclude.templates[templateName], data, context, options);
	},

    /********************************/
    /*  Retrieved content processing.
     */

	//	Used in: Transclude.blockContext
	specificBlockElementSelectors: [
		[	".footnote",
			".sidenote"
			].join(", "),
		".aux-links-append"
	],

	generalBlockElementSelectors: [
		"figure",
		"li",
		"p",
		"blockquote",
		[	"section",
			".markdownBody > *",
// 			".include-wrapper-block",
			].join(", ")
	],

	generalBlockContextMinimumLength: 200,

	//	Called by: Transclude.sliceContentFromDocument
	blockContext: (element, includeLink) => {
		let block = null;

		let selectors = [ ...Transclude.specificBlockElementSelectors, ...Transclude.generalBlockElementSelectors ];

		/*	Parse and process block context options (if any) specified by the 
			include-link. (See documentation for the .include-block-context
			class for details.)
		 */
		if (includeLink.dataset.blockContextOptions) {
			let options = includeLink.dataset.blockContextOptions.split("|");

			//	Expanded mode.
			if (options.includes("expanded"))
				selectors.remove("p");
		}

		for (selector of selectors)
			if (block = element.closest(selector) ?? block)
// 				if (   Transclude.specificBlockElementSelectors.includes(selector)
// 					|| block.textContent.length > Transclude.generalBlockContextMinimumLength
// 					|| (   block.parentNode == null
// 						|| block.parentNode instanceof Element == false))
					break;

		return ([ "BLOCKQUOTE", "LI" ].includes(block.tagName)
				? block.childNodes
				: block);
	},

    //  Called by: Transclude.transclude
    sliceContentFromDocument: (sourceDocument, includeLink) => {
        //  If it’s a full page, extract just the page content.
        let pageContent = sourceDocument.querySelector("#markdownBody") ?? sourceDocument.querySelector("body");
        let content = pageContent ? newDocument(pageContent.childNodes) : newDocument(sourceDocument);

        //  If the link’s anchor(s) specify part of the page, extract that.
        let anchors = anchorsForLink(includeLink);
        if (anchors.length == 2) {
            //  PmWiki-like transclude range syntax.

			//	Start element.
			let startElement = null;
			if (anchors[0].length > 1) {
				startElement = content.querySelector(selectorFromHash(anchors[0]));

				//	If specified but missing, transclude nothing.
				if (startElement == null)
					return newDocument();
			}

			//	End element.
			let endElement = null;
			if (anchors[1].length > 1) {
				endElement = content.querySelector(selectorFromHash(anchors[1]));

				//	If specified but missing, transclude nothing.
				if (endElement == null)
					return newDocument();
			}

            /*  If both ends of the range are unspecified, we return the entire
                content.
             */
            if (   startElement == null
                && endElement == null)
                return content;

            /*  If both ends of the range exist, but the end element
                doesn’t follow the start element, we return nothing.
             */
            if (   startElement
                && endElement
                && (   startElement == endElement
                    || startElement.compareDocumentPosition(endElement) & Node.DOCUMENT_POSITION_PRECEDING))
                return newDocument();

            //  Slice.
            let slicedContent = newDocument();

            if (startElement == null) {
                //  From start to id.
                slicedContent.appendChild(content);

                let currentNode = endElement;
                while (currentNode != slicedContent) {
                    while (currentNode.nextSibling) {
                        currentNode.nextSibling.remove();
                    }
                    currentNode = currentNode.parentNode;
                }
                endElement.remove();
            } else if (endElement == null) {
                //  From id to end.
                let nodesToAppend = [ startElement ];

                let currentNode = startElement;
                while (currentNode.parentNode) {
                    while (currentNode.nextSibling) {
                        nodesToAppend.push(currentNode.nextSibling);
                        currentNode = currentNode.nextSibling;
                    }
                    currentNode = currentNode.parentNode;
                }

                nodesToAppend.forEach(node => { slicedContent.appendChild(node); });
            } else {
                //  From id to id.
                let nodesToAppend = [ ];

                /*  Node which contains both start and end elements
                    (which might be the root DocumentFragment).
                 */
                let sharedAncestor = startElement.parentNode;
                while (!sharedAncestor.contains(endElement))
                    sharedAncestor = sharedAncestor.parentNode;

                let currentNode = startElement;

                /*  The branch of the tree containing the start element
                    (if it does not also contain the end element).
                 */
                while (currentNode.parentNode != sharedAncestor) {
                    while (currentNode.nextSibling) {
                        nodesToAppend.push(currentNode.nextSibling);
                        currentNode = currentNode.nextSibling;
                    }
                    currentNode = currentNode.parentNode;
                }

                //  There might be intervening branches.
                if (!currentNode.contains(endElement)) {
                    while (!currentNode.nextSibling.contains(endElement)) {
                        currentNode = currentNode.nextSibling;
                        nodesToAppend.push(currentNode);
                    }
                    currentNode = currentNode.nextSibling;
                }

                //  The branch of the tree containing the end element.
                if (currentNode != endElement) {
                    let endBranchOrigin = currentNode;
                    currentNode = endElement;
                    while (currentNode != endBranchOrigin) {
                        while (currentNode.nextSibling) {
                            currentNode.nextSibling.remove();
                        }
                        currentNode = currentNode.parentNode;
                    }
                    endElement.remove();
                    nodesToAppend.push(endBranchOrigin);
                }

                //  Insert the start element, if not there already.
                if (!nodesToAppend.last.contains(startElement))
                    nodesToAppend.splice(0, 0, startElement);

                //  Assemble.
                nodesToAppend.forEach(node => { slicedContent.appendChild(node); });
            }

            content = slicedContent;
        } else if (isAnchorLink(includeLink)) {
            //  Simple element tranclude.
            let targetElement = targetElementInDocument(includeLink, content);
            if (targetElement) {
				//	Optional block context.
            	/*	Check for whether the target element is *itself* an
            		include-link which will bring in a content block. If so,
            		do not include any *additional* block context, even if
            		the include-link we’re currently processing requests it!
            	 */
				let isBlockTranscludeLink = (   Transclude.isIncludeLink(targetElement)
											 && (   targetElement.classList.contains("include-block-context")
												 || (   targetElement.id > ""
													 && targetElement.classList.contains("include-identify-not") == false)));

				/*	We do not want to transclude annotations within backlink
					context. So, we will transform an annotation include link 
					in such a case into a normal link, and include its block
					context normally.
				 */
				if (   isBlockTranscludeLink
					&& Transclude.isAnnotationTransclude(targetElement)
					&& includeLink.closest(".backlink-context") != null) {
					Transclude.clearLinkState(targetElement);
					targetElement.classList.remove(...Transclude.permittedClassNames, "include-spinner", "include-spinner-not");
					isBlockTranscludeLink = false;
				}

				if (   includeLink.classList.contains("include-block-context")
					&& isBlockTranscludeLink == false) {
					let blockContext = Transclude.blockContext(targetElement, includeLink);
					if (blockContext) {
						content = newDocument(blockContext);

						//	Mark targeted element, for styling purposes.
						targetElement = targetElementInDocument(includeLink, content);
						if (targetElement)
							targetElement.classList.add("block-context-highlighted");
					} else {
						content = newDocument(targetElement);
					}
				} else {
					content = newDocument(targetElement);
				}
            } else {
            	content = newDocument();

            	reportBrokenAnchorLink(includeLink);
            }
        }

		//	Apply `data-include-selector-not` attribute.
		if (includeLink.dataset.includeSelectorNot) {
			content.querySelectorAll(includeLink.dataset.includeSelectorNot).forEach(element => {
				element.remove();
			});
		}

		//	Apply `data-include-selector` attribute.
		if (includeLink.dataset.includeSelector) {
			let nodesToInclude = [ ];
			content.querySelectorAll(includeLink.dataset.includeSelector).forEach(element => {
				if (nodesToInclude.findIndex(x => x.contains(element)) === -1)
					nodesToInclude.push(element);
			});
			content.replaceChildren(...nodesToInclude);
		}

        return content;
    },

    /*************************/
    /*  Include-link handling.
     */

	dataProviderNameForLink: (includeLink) => {
		return (Transclude.isAnnotationTransclude(includeLink)
				? "Annotations"
				: "Content");
	},

	dataProviderForLink: (includeLink) => {
		return window[Transclude.dataProviderNameForLink(includeLink)];
	},

	doWhenDataProviderLoaded: (includeLink, loadHandler) => {
		GW.notificationCenter.addHandlerForEvent(`${(Transclude.dataProviderNameForLink(includeLink))}.didLoad`, 
												 loadHandler, 
												 { once: true });
	},

	//  Enable alias classes for various forms of includes.
	includeLinkAliasTransforms: [ ],

	addIncludeLinkAliasClass: (aliasClass, linkTransform) => {
		Transclude.permittedClassNames.push(aliasClass);
		Transclude.includeLinkAliasTransforms.push([ aliasClass, linkTransform ]);
	},

	resolveIncludeLinkAliasClasses: (includeLink) => {
		Transclude.includeLinkAliasTransforms.forEach(alias => {
			let [ aliasClass, linkTransform ] = alias;
			if (   includeLink.classList.contains(aliasClass)
				&& linkTransform(includeLink))
				includeLink.classList.remove(aliasClass);
		});
	},

    //  Called by: Transclude.transclude
    //  Called by: Transclude.triggerTranscludesInContainer
    //  Called by: handleTranscludes (rewrite function)
    transclude: (includeLink, now = false) => {
        GWLog("Transclude.transclude", "transclude.js", 2);

		//	Resolve alias classes.
		Transclude.resolveIncludeLinkAliasClasses(includeLink);

		/*  We don’t retry failed loads, nor do we replicate ongoing loads.
         */
        if (   now == false
        	&& includeLink.classList.containsAnyOf([
        	"include-loading",
            "include-loading-failed"
        ])) return;

		/*  We exclude cross-origin transclusion for security reasons, but from
			a technical standpoint there’s no reason it shouldn’t work. Simply
			comment out the block below to enable cross-origin transcludes.
			—SA 2022-08-18
		 */
        if (   includeLink.hostname != location.hostname
            && Transclude.isAnnotationTransclude(includeLink) == false) {
            Transclude.setLinkStateLoadingFailed(includeLink);
            return;
        }

		/*	We do not attempt to transclude annotation transclude links which 
			do not (according to their set-by-the-server designation) actually 
			have any annotation.
		 */
		if (   Transclude.isAnnotationTransclude(includeLink)
			&& Transclude.hasAnnotation(includeLink) == false)
			return;

        /*  By default, includes within collapse blocks only get transcluded
            if/when the collapse block is expanded.
         */
        if (   now == false
            && isWithinCollapsedBlock(includeLink)
            && includeLink.classList.contains("include-strict") == false
            && includeLink.classList.contains("include-even-when-collapsed") == false) {
            includeLink.delayed = true;
            GW.notificationCenter.addHandlerForEvent("Collapse.collapseStateDidChange", (info) => {
                Transclude.transclude(includeLink);
            }, {
            	once: true,
            	condition: (info) => (isWithinCollapsedBlock(includeLink) == false)
            });

            return;
        }

        //  Set loading state.
        Transclude.setLinkStateLoading(includeLink);

        //  Transclusion is lazy by default.
        if (   now == false
            && includeLink.classList.contains("include-strict") == false) {
            includeLink.delayed = true;
            requestIdleCallback(() => {
                lazyLoadObserver(() => {
                    Transclude.transclude(includeLink, true);
                }, includeLink, {
                	root: scrollContainerOf(includeLink),
                	rootMargin: Transclude.lazyLoadViewportMargin
                });
            });

            return;
        }

		//	Get data provider.
		let dataProvider = Transclude.dataProviderForLink(includeLink);
        if (dataProvider == null) {
			/*  If data provider is not loaded, wait until it loads to attempt 
				transclusion.
			 */
			includeLink.delayed = true;
			Transclude.doWhenDataProviderLoaded(includeLink, (info) => {
				Transclude.transclude(includeLink, true);
			});

			return;
        }

		//	Activity begins.
		beginActivity();

		//	Request data load, if need be.
		if (dataProvider.cachedDataExists(includeLink) == false) {
			dataProvider.load(includeLink);
	        includeLink.delayed = true;
		}

		//	When data loads (or if it is already loaded), transclude.
		let processData = (template) => {
			//	Reference data.
			let referenceData = dataProvider.referenceDataForLink(includeLink);
			let templateData = referenceData.content;

			let content = null;
			if (template) {
				//	Template fill context.
				let context = Object.assign({ }, referenceData, templateDataFromHTML(includeLink));

				//	Template fill options.
				let options = {
					fireContentLoadEvent: true,
					loadEventInfo: {
						source: "transclude",
						contentType: (Transclude.isAnnotationTransclude(includeLink) ? "annotation" : null),
						includeLink: includeLink
					}
				};

				//	Fill template.
				content = fillTemplate(template, templateData, context, options);
			} else if (referenceData.content instanceof DocumentFragment) {
				content = referenceData.content;
			}

			//	Slice and include, or else handle failure.
			if (content) {
				includeContent(includeLink, Transclude.sliceContentFromDocument(content, includeLink));
			} else {
				Transclude.setLinkStateLoadingFailed(includeLink);

				//	Send request to record failure in server logs.
				GWServerLogError(includeLink.href + `--transclude-template-fill-failed`,
								 "failed transclude template fill");
			}
		};
		dataProvider.waitForDataLoad(includeLink, (link) => {
		   	//	Load success handler.

			/*	If a template is specified by name, then we’ll need to make sure
				that it’s loaded before we can fill it with data.
			 */
			let templateName = includeLink.dataset.template || dataProvider.referenceDataForLink(includeLink).template;
			if (templateName) {
				Transclude.doWhenTemplateLoaded(templateName, (template, delayed) => {
					if (delayed)
						includeLink.delayed = true;

					processData(template);
				}, (delayed) => {
					Transclude.setLinkStateLoadingFailed(includeLink);

					//	Send request to record failure in server logs.
					GWServerLogError(templateName + `--include-template-load-failed`,
									 "failed include template load");
				});
			} else {
				processData();
			}
		}, (link) => {
		   	//	Load fail handler.
		   	endActivity();

			/*  If we’ve already tried and failed to load the content, we
				will not try loading again, and just show a “loading failed”
				message.
			 */
			Transclude.setLinkStateLoadingFailed(includeLink);

			//  Send request to record failure in server logs.
			GWServerLogError(includeLink.href + `--transclude-failed`,
							 "failed transclude");
		});
    },

    /*****************/
    /*  Misc. helpers.
     */

    //  Called by: "beforeprint" listener (rewrite.js)
    triggerTranscludesInContainer: (container, eventInfo) => {
        Transclude.allIncludeLinksInContainer(container).forEach(includeLink => {
        	if (eventInfo)
        		includeLink.eventInfo = eventInfo;

            Transclude.transclude(includeLink, true);
        });
    },

    /********************/
    /*  Loading spinners.
     */

    //  Called by: Transclude.transclude
    setLinkStateLoading: (link) => {
        if (Transclude.isIncludeLink(link) == false)
            return;

		//	Designate loading state.
        link.classList.add("include-loading");

		//	Intelligently add loading spinner, unless override class set.
		if (link.classList.containsAnyOf([ "include-spinner", "include-spinner-not" ]) == false) {
			/*	Add loading spinner for link bibliography entries and also any
				include-link not within a collapsed block.
			 */
			if (isWithinCollapsedBlock(link) == false) {
				link.classList.add("include-spinner");
			} else {
				let containingAuxLinksBlock = link.closest(".aux-links-list, .aux-links-append");
				if (   containingAuxLinksBlock
					&& containingAuxLinksBlock.classList.contains("link-bibliography-list")) {
					link.classList.add("include-spinner");
				}
			}
		}

		//	Disable link icon, if loading spinner present.
        if (   link.classList.contains("include-spinner")
        	&& link.textContent > "")
            link.classList.add("icon-not");

		//	Disable normal link functionality.
        link.onclick = () => { return false; };

		//	Set temporary tooltip.
        link.savedTitle = link.title ?? "";
        link.title = "Content is loading. Please wait.";
    },

    //  Called by: Transclude.transclude
    setLinkStateLoadingFailed: (link) => {
        if (Transclude.isIncludeLink(link) == false)
            return;

		//	Record load failure.
        link.swapClasses([ "include-loading", "include-loading-failed" ], 1);

		//	Revert to normal link functionality.
		Transclude.resetLinkBehavior(link);

        //  Fire event, if need be.
        if (link.delayed) {
            GW.notificationCenter.fireEvent("Rewrite.contentDidChange", {
                source: "transclude.loadingFailed",
                document: link.eventInfo.document,
                nodes: [ link ]
            });
        }
    },

    //  Called by: includeContent
	clearLinkState: (link) => {
        if (Transclude.isIncludeLink(link) == false)
            return;

		//	Clear classes.
		link.classList.remove("include-loading", "include-loading-failed");

		//	Revert to normal link functionality.
		Transclude.resetLinkBehavior(link);
	},

	//	Called by: Transclude.setLinkStateLoadingFailed
	//	Called by: Transclude.clearLinkState
	resetLinkBehavior: (link) => {
		//	Re-enable link icon.
        if (link.textContent > "")
            link.classList.remove("icon-not");

		//	Re-enable normal link behavior.
        link.onclick = null;

		//	Replace normal tooltip.
        if (link.savedTitle != null) {
            link.title = link.savedTitle;
            link.savedTitle = null;
        }
	}
};

/****************************/
/*  Process transclude-links.
 */
addContentLoadHandler(GW.contentLoadHandlers.handleTranscludes = (eventInfo) => {
    GWLog("handleTranscludes", "transclude.js", 1);

    Transclude.allIncludeLinksInContainer(eventInfo.container).forEach(includeLink => {
		//	Store a reference to the load event info.
		includeLink.eventInfo = eventInfo;

        //  Transclude now or maybe later.
        Transclude.transclude(includeLink);
    });
}, "transclude");

/*************************************************************/
/*	Re-process when injecting. (Necessary for cloned content.)
 */
addContentInjectHandler(GW.contentInjectHandlers.handleTranscludes = GW.contentLoadHandlers.handleTranscludes, "rewrite");

/******************************************/
/*	Add various include-link alias classes.
 */

/*========================================================*/
/*	.include-annotation-partial
		`class="include-annotation"`
		`data-include-selector-not=".annotation-abstract"`
		`data-template-fields="annotationClassSuffix:$"`
		`data-annotation-class-suffix="-partial"`
 */
Transclude.addIncludeLinkAliasClass("include-annotation-partial", (includeLink) => {
	includeLink.classList.add("include-annotation");
	includeLink.dataset.includeSelectorNot = ".annotation-abstract";
	includeLink.dataset.templateFields = [
		...((includeLink.dataset.templateFields ?? "").split(",").filter(x => x)),
		"annotationClassSuffix:$"
	].join(",");
	includeLink.dataset.annotationClassSuffix = "-partial";

	return true;
});

/*====================================================*/
/*	.include-annotation.include-omit-metadata
		`data-include-selector=".annotation-abstract"`
 */
Transclude.addIncludeLinkAliasClass("include-omit-metadata", (includeLink) => {
	if (Transclude.isAnnotatedLinkFull(includeLink) == false)
		return false;

	includeLink.dataset.includeSelector = ".annotation-abstract";

	return true;
});

/*=============================================*/
/*	.include-block-context-expanded
		`class="include-block-context"`
		`data-block-context-options="expanded"`
 */
Transclude.addIncludeLinkAliasClass("include-block-context-expanded", (includeLink) => {
	includeLink.classList.add("include-block-context");
	includeLink.dataset.blockContextOptions = "expanded";

	return true;
});

/*==========================================================*/
/*	.include-content-no-header
		`class="include-unwrap"`
		`data-include-selector-not="h1, h2, h3, h4, h5, h6"`
 */
Transclude.addIncludeLinkAliasClass("include-content-no-header", (includeLink) => {
	includeLink.classList.add("include-unwrap");
	includeLink.dataset.includeSelectorNot = "h1, h2, h3, h4, h5, h6";

	return true;
});
Transclude.templates = {
	"annotation-blockquote-inside": `<div class="annotation<{annotationClassSuffix}> <{dataSourceClass}>">
	<p class="data-field title <[IF authorDateAux]>author-date-aux<[IFEND]>">
		<a 
		   class="<{titleLinkClass}>"
		   title="Open <{titleLinkHref}> in <{whichTab}> <{tabOrWindow}>"
		   href="<{titleLinkHref}>"
		   <[IF linkTarget]>target="<{linkTarget}>"<[IFEND]>
		   <[IF titleLinkDataAttributes]><{titleLinkDataAttributes}><[IFEND]>
		   <{titleLinkIconMetadata}>
			   ><{fullTitleHTML}></a>\\
		<[IF secondaryTitleLinksHTML]><span class="secondary-title-links"><{secondaryTitleLinksHTML}></span><[IFEND]>\\
		<[IF abstract & ![ archiveURL | authorDateAux ] ]>:<[IFEND]>\\

		<[IF archiveURL]>
		<span class="archiveURL">[<a
			 title="Link to local archive for <{titleText}>"
			 href="<{archiveURL}>"
			 <[IF2 linkTarget]>target="<{linkTarget}>"<[IF2END]>
			 alt="Locally archived version of this URL"
				 ><{archiveURLText}></a>]</span>
		<[IFEND]>\\

		<[IF authorDateAux]><[IF2 author | date]>,\\ <[IF2END]><{authorDateAux}><[IF2 abstract]>:<[IF2END]><[IFEND]>
	</p>
	<[IF abstract]>
	<blockquote class="data-field annotation-abstract"><{abstract}></blockquote>
	<[IFEND]>
</div>`,
	"annotation-blockquote-not": `<div class="annotation<{annotationClassSuffix}> <{dataSourceClass}>">
	<p class="data-field title">
		<a 
		   class="<{titleLinkClass}>"
		   title="Open <{titleLinkHref}> in <{whichTab}> <{tabOrWindow}>"
		   href="<{titleLinkHref}>"
		   <[IF linkTarget]>target="<{linkTarget}>"<[IFEND]>
		   <[IF titleLinkDataAttributes]><{titleLinkDataAttributes}><[IFEND]>
		   <{titleLinkIconMetadata}>
			   ><{titleHTML}></a>\\
		<[IF secondaryTitleLinksHTML]><span class="secondary-title-links"><{secondaryTitleLinksHTML}></span><[IFEND]>\\

		<[IF archiveURL]>
		<span class="archiveURL">[<a
			 title="Link to local archive for <{titleText}>"
			 href="<{archiveURL}>"
			 <[IF2 linkTarget]>target="<{linkTarget}>"<[IF2END]>
			 alt="Locally archived version of this URL"
				 ><{archiveURLText}></a>]</span>
		<[IFEND]>
	</p>
	<[IF authorDateAux]>
	<p class="data-field author-date-aux"><{authorDateAux}></p>
	<[IFEND]>
	<[IF abstract]>
	<div class="data-field annotation-abstract"><{abstract}></div>
	<[IFEND]>
</div>`,
	"annotation-blockquote-outside": `<blockquote class="annotation<{annotationClassSuffix}> <{dataSourceClass}>">
	<p class="data-field title">
		<a 
		   class="<{titleLinkClass}>"
		   title="Open <{titleLinkHref}> in <{whichTab}> <{tabOrWindow}>"
		   href="<{titleLinkHref}>"
		   <[IF linkTarget]>target="<{linkTarget}>"<[IFEND]>
		   <[IF titleLinkDataAttributes]><{titleLinkDataAttributes}><[IFEND]>
		   <{titleLinkIconMetadata}>
			   ><{titleHTML}></a>\\
		<[IF secondaryTitleLinksHTML]><span class="secondary-title-links"><{secondaryTitleLinksHTML}></span><[IFEND]>\\

		<[IF archiveURL]>
		<span class="archiveURL">[<a
			 title="Link to local archive for <{titleText}>"
			 href="<{archiveURL}>"
			 <[IF2 linkTarget]>target="<{linkTarget}>"<[IF2END]>
			 alt="Locally archived version of this URL"
				 ><{archiveURLText}></a
		>]</span>
		<[IFEND]>
	</p>
	<[IF authorDateAux]>
	<p class="data-field author-date-aux"><{authorDateAux}></p>
	<[IFEND]>
	<[IF abstract]>
	<div class="data-field annotation-abstract"><{abstract}></div>
	<[IFEND]>
</blockquote>`,
	"pop-frame-title-annotation": `<[IF popFrameTitleArchiveLinkHref]>
<a
    class="popframe-title-link"
    title="Open <{popFrameTitleArchiveLinkHref}> in <{whichTab}> <{tabOrWindow}>."
    href="<{popFrameTitleArchiveLinkHref}>"
    target="<{linkTarget}>"
        ><{popFrameTitleText}></a>
<[ELSE]>
<a
    class="popframe-title-link"
    href="<{popFrameTitleLinkHref}>"
    title="Open <{popFrameTitleLinkHref}> in <{whichTab}> <{tabOrWindow}>."
    target="<{linkTarget}>"
        ><{popFrameTitleText}></a>
<[IFEND]>
`,
	"pop-frame-title-standard": `<a
	class="popframe-title-link"
	href="<{popFrameTitleLinkHref}>"
	title="Open <{popFrameTitleLinkHref}> in <{whichTab}> <{tabOrWindow}>."
	target="<{linkTarget}>"
		><{popFrameTitleText}></a>`,
};
// popups.js: standalone Javascript library for creating 'popups' which display link metadata (typically, title/author/date/summary), for extremely convenient reference/abstract reading.
// Author: Said Achmiz, Shawn Presser (mobile & Youtube support)
// Date: 2019-09-12
// When:
// license: MIT (derivative of footnotes.js, which is PD)

// Popups are inspired by Wikipedia's augmented tooltips (originally implemented as editor-built extensions, now available to all readers via https://www.mediawiki.org/wiki/Page_Previews ). Whenever any such link is mouse-overed by the user, popups.js will pop up a large tooltip-like square with the contents of the attributes. This is particularly intended for references, where it is extremely convenient to autopopulate links such as to Arxiv.org/Biorxiv.org/Wikipedia with the link's title/author/date/abstract, so the reader can see it instantly. Links to 'reverse citations' are provided as much as possible: links with DOIs go to a Semantic Scholar search engine query for that DOI, which prioritizes meta-analyses & systematic reviews to provide context for any given paper (particularly whether it has failed to replicate or otherwise been debunked); for URLs ending in 'PDF' which probably have Semantic Scholar entries, they go to a title search; and for all other URLs, a Google search using the obscure `link:` operator is provided.. For more details, see `LinkMetadata.hs`.

// On mobile, clicking on links (as opposed to hovering over links on desktop) will bring up the annotation or video; another click on it or the popup will then go to it. A click outside it de-activates it.

// For an example of a Hakyll library which generates annotations for Wikipedia/Biorxiv/Arxiv/PDFs/arbitrarily-defined links, see https://gwern.net/static/build/LinkMetadata.hs ; for a live demonstration, see the links in https://gwern.net/newsletter/2019/07

Extracts = {
    /*  Target containers.
     */
    contentContainersSelector: [
    	".markdownBody",
    	"#TOC",
    	"#page-metadata",
    	"#sidebar"
    ].join(", "),

	/*	Don’t display indicator hooks on links in these containers.
	 */
	hooklessLinksContainersSelector: [
		"body.index #markdownBody",
		"#sidebar",
		".TOC",
		".floating-header"
	].join(", "),

    /*  Targets.
     */
    targets: {
        targetElementsSelector: "a[href]",
        excludedElementsSelector: [
            ".section-self-link",
            ".footnote-self-link",
            ".sidenote-self-link",
            "[aria-hidden='true']",
            "[href$='#top']",
            ".extract-not"
        ].join(", "),
        excludedContainerElementsSelector: "h1, h2, h3, h4, h5, h6",
        //  See comment at Extracts.isLocalPageLink for info on this function.
        //  Called by: pop-frame providers (popins.js or popups.js).
        testTarget: (target) => {
            let targetTypeInfo = Extracts.targetTypeInfo(target);
            if (targetTypeInfo) {
                let specialTestFunction = Extracts[`testTarget_${targetTypeInfo.typeName}`]
                if (   specialTestFunction
                	&& specialTestFunction(target) == false)
                    return false;

                //  Do not allow pop-frames to spawn themselves.
                let containingPopFrame = Extracts.popFrameProvider.containingPopFrame(target);
                if (   containingPopFrame
                	&& Extracts.targetsMatch(containingPopFrame.spawningTarget, target))
                    return false;

				//	Don’t spawn duplicate popins.
				if (Extracts.popFrameProvider == Popins) {
					let popinStack = Popins.allSpawnedPopins();
					if (popinStack.findIndex(popin => Extracts.targetsMatch(popin.spawningTarget, target)) !== -1)
						return false;
				}

                //  Added specified classes to the target.
                if (targetTypeInfo.targetClasses) {
                	if (typeof targetTypeInfo.targetClasses == "string")
	                    target.classList.add(...(targetTypeInfo.targetClasses.split(" ")));
	                else if (typeof targetTypeInfo.targetClasses == "function")
	                	target.classList.add(...(targetTypeInfo.targetClasses(target).split(" ")));
                }

                return true;
            }

            return false;
        }
    },

    /*  Misc. configuration.
     */
    server404PageTitles: [
        "404 Not Found"
    ],

    rootDocument: document,

    /******************/
    /*  Infrastructure.
     */

    //  Can be ‘Popups’ or ‘Popins’, currently.
    popFrameProviderName: null,
    //  Can be the Popups or Popins object, currently.
    popFrameProvider: null,

    /***********/
    /*  General.
     */

	//	Called by: popups.js and popins.js when removing a target
	//	(see Extracts.removeTargetsWithin)
	restoreTarget: (target) => {
		//  Restore title attribute, if any.
		if (target.dataset.attributeTitle) {
			target.title = target.dataset.attributeTitle;
			target.removeAttribute("data-attribute-title");
		}

		target.classList.remove("has-content", "has-annotation", "has-annotation-partial");
	},

    //  Called by: Extracts.cleanup
    removeTargetsWithin: (container) => {
        GWLog("Extracts.removeTargetsWithin", "extracts.js", 1);

        Extracts.popFrameProvider.removeTargetsWithin(container, Extracts.targets, Extracts.restoreTarget);
    },

    //  Called by: extracts-options.js
    cleanup: () => {
        GWLog("Extracts.cleanup", "extracts.js", 1);

		//	Remove pop-frame indicator hooks.
		document.querySelectorAll(".has-content").forEach(link => {
			link.querySelector(".indicator-hook").remove();
			link.classList.remove("has-indicator-hook");
		});

        //  Unbind event listeners and restore targets.
        document.querySelectorAll(Extracts.contentContainersSelector).forEach(container => {
            Extracts.removeTargetsWithin(container);
        });

        //  Remove content inject event handler.
    	GW.notificationCenter.removeHandlerForEvent("GW.contentDidInject", Extracts.processTargetsOnContentInject);

		//	Remove phantom popin cleaning handler.
		if (Extracts.popFrameProvider == Popins)
			GW.notificationCenter.removeHandlerForEvent("GW.contentDidInject", Extracts.cleanPopinsFromInjectedContent);

		//	Remove pop-frames & containers.
		Extracts.popFrameProvider.cleanup();

        //  Fire cleanup-complete event.
        GW.notificationCenter.fireEvent("Extracts.cleanupDidComplete");
    },

    //  Called by: Extracts.processTargetsInContainer
    //  Called by: extracts-options.js
    addTargetsWithin: (container) => {
        GWLog("Extracts.addTargetsWithin", "extracts.js", 1);

        if (Extracts.popFrameProvider == Popups) {
            Popups.addTargetsWithin(container, Extracts.targets, Extracts.preparePopup, Extracts.preparePopupTarget, Extracts.restoreTarget);
        } else if (Extracts.popFrameProvider == Popins) {
            Popins.addTargetsWithin(container, Extracts.targets, Extracts.preparePopin, Extracts.preparePopinTarget, Extracts.restoreTarget);
        }

		Extracts.setUpAnnotationLoadEventsWithin(container);
		Extracts.setUpContentLoadEventsWithin(container);
    },

    //  Called by: extracts.js (doSetup)
    //  Called by: extracts-options.js
    setup: () => {
        GWLog("Extracts.setup", "extracts.js", 1);

		//  Set pop-frame type (mode) - popups or popins.
		let mobileMode = (   localStorage.getItem("extracts-force-popins") == "true"
						  || GW.isMobile() 
						  || matchMedia("(max-width: 1279px) and (max-height: 959px)").matches);
		Extracts.popFrameProviderName = mobileMode ? "Popins" : "Popups";
		GWLog(`${(mobileMode ? "Mobile" : "Non-mobile")} client detected. Activating ${(mobileMode ? "popins" : "popups")}.`, "extracts.js", 1);

		//  Prevent null references.
		Popups = window["Popups"] || { };
		Popins = window["Popins"] || { };

		//	If provider not loaded yet, defer setup until it is.
		if (window[Extracts.popFrameProviderName] == null) {
			GW.notificationCenter.addHandlerForEvent(Extracts.popFrameProviderName + ".didLoad", (info) => {
				Extracts.setup();
			}, { once: true });

			return;
		}

        //  Set service provider object.
        Extracts.popFrameProvider = window[Extracts.popFrameProviderName];

		//	Inject mode selectors, if need be.
		if (Extracts.modeSelector == null) {
			Extracts.injectModeSelector();
			document.querySelectorAll(".extracts-mode-selector-inline").forEach(element => {
				Extracts.injectModeSelector(element);
			});
		}

		//	Do not proceed if disabled.
        if (Extracts.popFrameProvider == Popups) {
            GWLog("Setting up for popups.", "extracts.js", 1);

            if (Extracts.popupsEnabled() == false)
                return;

            GWLog("Activating popups.", "extracts.js", 1);
        } else {
            GWLog("Setting up for popins.", "extracts.js", 1);

			if (Extracts.popinsEnabled() == false)
				return;

            GWLog("Activating popins.", "extracts.js", 1);
        }

		//	Run provider setup.
		Extracts.popFrameProvider.setup();

        /*  Add handler to set up targets in loaded content (including
            newly-spawned pop-frames; this allows for recursion), and to
            add hover/click event listeners to annotated targets, to load
            annotations (fragments).
         */
        addContentInjectHandler(Extracts.processTargetsOnContentInject = (eventInfo) => {
            GWLog("Extracts.processTargetsOnContentInject", "extracts.js", 2);

            Extracts.processTargetsInContainer(eventInfo.container);
        }, "eventListeners");

		//	Add handler to prevent “phantom” popins.
		if (Extracts.popFrameProvider == Popins) {
			addContentInjectHandler((eventInfo) => {
				//	Clean any existing popins.
				Popins.removeAllPopinsInContainer(eventInfo.container);
			}, "rewrite");
		}

        //  Fire setup-complete event.
        GW.notificationCenter.fireEvent("Extracts.setupDidComplete");
    },

    //  Called by: Extracts.setup
    processTargetsInContainer: (container) => {
        GWLog("Extracts.processTargetsInContainer", "extracts.js", 2);

		if (   container instanceof DocumentFragment
			|| (   container instanceof Element 
			    && container.closest(Extracts.contentContainersSelector))) {
			Extracts.addTargetsWithin(container);
		} else {
            container.querySelectorAll(Extracts.contentContainersSelector).forEach(contentContainer => {
                Extracts.addTargetsWithin(contentContainer);
            });
        }

		/*	Add pop-frame indicator hooks, if need be.
			(See links.css for how these are used.)
		 */
		container.querySelectorAll(".has-content").forEach(link => {
			link.classList.toggle("has-indicator-hook", 
								  (link.closest(Extracts.hooklessLinksContainersSelector) == null));
				
			if (link.querySelector(".indicator-hook") != null)
				return;

			link.insertBefore(newElement("SPAN", { class: "indicator-hook" }), link.firstChild);

			/*	Inject U+2060 WORD JOINER at start of first text node of the
				link. (It _must_ be injected as a Unicode character into the
				existing text node; injecting it within the .indicator-hook
				span, or as an HTML escape code into the text node, or in
				any other fashion, creates a separate text node, which
				causes all sorts of problems - text shadow artifacts, etc.)
			 */
			let linkFirstTextNode = link.firstTextNode;
			if (linkFirstTextNode)
				linkFirstTextNode.textContent = "\u{2060}" + linkFirstTextNode.textContent;
		});
    },

    /***********/
    /*  Content.
     */

	/*  This array defines the types of ‘targets’ (ie. annotated links,
		links pointing to available content such as images or code files,
		citations, etc.) that Extracts supports.
		The fields in each entry are:
			1. Type name
			2. Type predicate function (of the Extracts object) for identifying
			   targets of the type; returns true iff target is of that type
			3. Class(es) to be added to targets of the type (these are added
			   during initial processing)
			4. Fill function (of the Extracts object); called to fill a
			   pop-frame for a target of that type with content
			5. Class(es) to be added to a pop-frame for targets of that type
	 */
	targetTypeDefinitions: [ ],

    /*  Returns full type info for the given target (in other words, the data
        from the appropriate row of the targetTypeDefinitions array), or null
        if the target is not matched by the predicate function of any known type.
     */
    //  Called by: many functions, all in extracts.js
    targetTypeInfo: (target) => {
        let info = { };
        for (definition of Extracts.targetTypeDefinitions) {
            [   info.typeName,
                info.predicateFunctionName,
                info.targetClasses,
                info.popFrameFillFunctionName,
                info.popFrameClasses
            ] = definition;
            if (Extracts[info.predicateFunctionName](target))
                return info;
        }

        return null;
    },

    //  Called by: Extracts.targetsMatch
    //  Called by: Extracts.fillPopFrame
    //  Called by: extracts-annotations.js
    targetIdentifier: (target) => {
    	return Extracts.isAnnotatedLink(target)
    		   ? Annotations.targetIdentifier(target)
    		   : (target.hostname == location.hostname
                  ? target.pathname + target.hash
                  : target.href);
    },

    /*  Returns true if the two targets will spawn identical popups
        (that is, if they are of the same type, and have the same identifiers).
     */
    //  Called by: Extracts.targets.testTarget
    //  Called by: Extracts.spawnedPopupMatchingTarget
    targetsMatch: (targetA, targetB) => {
        return    Extracts.targetIdentifier(targetA) == Extracts.targetIdentifier(targetB)
               && Extracts.targetTypeInfo(targetA).typeName == Extracts.targetTypeInfo(targetB).typeName;
    },

    /*  This function fills a pop-frame for a given target with content. It
        returns true if the pop-frame successfully filled, false otherwise.
     */
    //  Called by: Extracts.preparePopFrame
    //  Called by: Extracts.refreshPopFrameAfterLocalPageLoads
    //  Called by: extracts-annotations.js
    fillPopFrame: (popFrame) => {
        GWLog("Extracts.fillPopFrame", "extracts.js", 2);

        let didFill = false;
        let target = popFrame.spawningTarget;
        let targetTypeInfo = Extracts.targetTypeInfo(target);
        if (   targetTypeInfo
        	&& targetTypeInfo.popFrameFillFunctionName) {
            didFill = Extracts.popFrameProvider.setPopFrameContent(popFrame, Extracts[targetTypeInfo.popFrameFillFunctionName](target));
            if (targetTypeInfo.popFrameClasses)
            	Extracts.popFrameProvider.addClassesToPopFrame(popFrame, ...(targetTypeInfo.popFrameClasses.split(" ")));
        }

        if (didFill) {
            return true;
        } else {
            GWLog(`Unable to fill pop-frame (${Extracts.targetIdentifier(target)} [${(targetTypeInfo ? targetTypeInfo.typeName : "UNDEFINED")}])!`, "extracts.js", 1);
            return false;
        }
    },

    //  Called by: Extracts.targetDocument
    //  Called by: Extracts.preparePopup
    //  Called by: Extracts.preparePopin
    //  Called by: extracts-annotations.js
    popFrameHasLoaded: (popFrame) => {
        return !(popFrame.classList.contains("loading") || popFrame.classList.contains("loading-failed"));
    },

    //  Called by: Extracts.titleForPopFrame
    //  Called by: Extracts.titleForPopFrame_LOCAL_PAGE
    //  Called by: extracts-annotations.js
    //  Called by: extracts-content.js
    standardPopFrameTitleElementForTarget: (target, titleText) => {
        if (typeof titleText == "undefined") {
            titleText = (target.hostname == location.hostname)
                        ? target.pathname + target.hash
                        : target.href;
            titleText = `<code>${titleText}</code>`;
    	}

        /*  Because tab-handling is bad on mobile, readers expect the original
            remote URL to open up in-tab, as readers will be single-threaded;
            on desktop, we can open up in a tab for poweruser-browsing of
            tab-explosions.
         */
		return Transclude.fillTemplateNamed("pop-frame-title-standard", {
			popFrameTitleLinkHref:  target.href,
			popFrameTitleText:      titleText
		}, Extracts.getStandardPopFrameTitleTemplateFillContext());
    },

	getStandardPopFrameTitleTemplateFillContext: () => {
		return {
			linkTarget:   ((Extracts.popFrameProvider == Popins) ? "_self" : "_blank"),
			whichTab:     ((Extracts.popFrameProvider == Popins) ? "current" : "new"),
			tabOrWindow:  ((Extracts.popFrameProvider == Popins) ? "tab" : "window")
		};
	},

    /*  Returns the contents of the title element for a pop-frame.
     */
    //  Called by: Extracts.preparePopup
    //  Called by: Extracts.preparePopin
    //  Called by: Extracts.rewritePopinContent
    titleForPopFrame: (popFrame, titleText) => {
        let target = popFrame.spawningTarget;

        //  Special handling for certain popup types.
        let targetTypeName = Extracts.targetTypeInfo(target).typeName;
        let specialTitleFunction = (Extracts.popFrameProvider == Popups
                                    ? Extracts[`titleForPopup_${targetTypeName}`]
                                    : Extracts[`titleForPopin_${targetTypeName}`])
                                || Extracts[`titleForPopFrame_${targetTypeName}`];
        if (specialTitleFunction)
            return specialTitleFunction(popFrame, titleText);
        else
            return Extracts.standardPopFrameTitleElementForTarget(target, titleText);
    },

	//	Called by: Extracts.rewritePopinContent
	//	Called by: Extracts.rewritePopFrameContent_LOCAL_PAGE
	updatePopFrameTitle: (popFrame, titleText) => {
        GWLog("Extracts.updatePopFrameTitle", "extracts.js", 2);

		if (popFrame.titleBar) {
			popFrame.titleBar.querySelector(".popframe-title").replaceChildren(Extracts.titleForPopFrame(popFrame, titleText));
		} else if (popFrame.titleBarContents) {
			popFrame.titleBarContents.find(x => x.classList.contains("popframe-title")).replaceChildren(Extracts.titleForPopFrame(popFrame, titleText));
		}
	},

    /*  This function’s purpose is to allow for the transclusion of entire pages
        on the same website (displayed to the user in popups, or injected in
        block flow as popins), and the (almost-)seamless handling of local links
        in such transcluded content in the same way that they’re handled in the
        root document (ie. the actual page loaded in the browser window). This
        permits us to have truly recursive popups with unlimited recursion depth
        and no loss of functionality.

        For any given target element, targetDocument() asks: to what local
        document does the link refer?

        This may be either the root document, or an entire other page that was
        transcluded wholesale and embedded as a pop-frame (of class
        ‘full-page’).
     */
    //  Called by: Extracts.localPageForTarget
    //  Called by: Extracts.titleForPopFrame_LOCAL_PAGE
    //  Called by: extracts-content.js
    targetDocument: (target) => {
        if (target.hostname != location.hostname)
            return null;

        if (target.pathname == location.pathname)
            return Extracts.rootDocument;

        if (Extracts.popFrameProvider == Popups) {
            let popupForTargetDocument = Popups.allSpawnedPopups().find(popup => (   popup.classList.contains("full-page")
                                                                                  && popup.spawningTarget.pathname == target.pathname));
            return popupForTargetDocument ? popupForTargetDocument.document : null;
        } else if (Extracts.popFrameProvider == Popins) {
            let popinForTargetDocument = Popins.allSpawnedPopins().find(popin => (   popin.classList.contains("full-page")
                                                                                  && popin.spawningTarget.pathname == target.pathname)
                                                                                  && Extracts.popFrameHasLoaded(popin));
            return popinForTargetDocument ? popinForTargetDocument.document : null;
        }
    },

    /*  Activate loading spinner for an object pop-frame.
     */
    //  Called by: extracts-content.js
    setLoadingSpinner: (popFrame) => {
        let target = popFrame.spawningTarget;

        popFrame.classList.toggle("loading", true);

        let objectOfSomeSort = popFrame.document.querySelector("iframe, object, img");
		if (objectOfSomeSort == null)
			return;

        //  When loading ends (in success or failure)...
        if ([ "IFRAME" ].includes(objectOfSomeSort.tagName)) {
            //  Iframes do not fire ‘error’ on server error.
            objectOfSomeSort.onload = (event) => {
                Extracts.postRefreshUpdatePopFrameForTarget(target, true);

                /*  We do this for local documents only. Cross-origin
                    protections prevent us from accessing the content of
                    an iframe with a foreign site, so we do nothing special
                    and simply let the foreign site’s server show its usual
                    404 page (or whatever) if the linked page is not found.
                 */
                if (   target.hostname == location.hostname
                    && Extracts.server404PageTitles.includes(objectOfSomeSort.contentDocument.title)) {
					Extracts.postRefreshUpdatePopFrameForTarget(target, false);
                }
            };
        } else if ([ "OBJECT", "IMG" ].includes(objectOfSomeSort.tagName)) {
            //  Objects & images fire ‘error’ on server error or load fail.
            objectOfSomeSort.onload = (event) => {
                Extracts.postRefreshUpdatePopFrameForTarget(target, true);
            };
        }

        /*  We set an ‘error’ handler for *all* types of entity, even
            iframes, just in case.
         */
        if ([ "IFRAME", "OBJECT", "IMG" ].includes(objectOfSomeSort.tagName)) {
			objectOfSomeSort.onerror = (event) => {
				Extracts.popFrameProvider.removeClassesFromPopFrame(popFrame, "loading");
				Extracts.popFrameProvider.addClassesToPopFrame(popFrame, "loading-failed");
			};
        }
    },

	/*	If the reference data is not yet available, we either queue the 
		refresh-pop-frame callbacks for when it does load or fail (in the case 
		where the network request hasn’t come back yet), or mark the pop-frame 
		as “loading failed” and do nothing (if the load has failed).
	 */
	//	Called by: Extracts.localPageForTarget
	//	Called by: Extracts.localCodeFileForTarget
	//	Called by: Extracts.annotationForTarget
	handleIncompleteReferenceData: (target, referenceData, dataProvider) => {
        if (referenceData == null) {
            /*  If the content has yet to be loaded, we’ll trust that it has
            	been asked to load, and meanwhile wait, and do nothing yet.
             */
			target.popFrame.classList.toggle("loading", true);

			dataProvider.waitForDataLoad(dataProvider.targetIdentifier(target), 
			   (identifier) => {
				Extracts.postRefreshSuccessUpdatePopFrameForTarget(target);
			}, (identifier) => {
				Extracts.postRefreshFailureUpdatePopFrameForTarget(target);
			});
        } else if (referenceData == "LOADING_FAILED") {
            /*  If we’ve already tried and failed to load the content, we
                will not try loading again, and just show the “loading failed”
                message.
             */
            target.popFrame.classList.add("loading-failed");
        }
	},

	//	Called by: Extracts.setLoadingSpinner
	postRefreshUpdatePopFrameForTarget: (target, success) => {
        GWLog("Extracts.postRefreshUpdatePopFrameForTarget", "extracts.js", 2);

		let popFrame = target.popFrame;

		if (Extracts.popFrameProvider.isSpawned(popFrame) == false)
			return;

		Extracts.popFrameProvider.removeClassesFromPopFrame(popFrame, "loading");

		if (!success)
			Extracts.popFrameProvider.addClassesToPopFrame(popFrame, "loading-failed");

		//  Re-spawn, or fill and rewrite, the pop-frame.
		if (Extracts.popFrameProvider == Popups)
			Popups.positionPopup(popFrame);
		else if (Extracts.popFrameProvider == Popins)
			Popins.scrollPopinIntoView(popFrame);
	},

	//	Called by: Extracts.rewritePopFrameContent_LOCAL_PAGE
	//	Called by: Extracts.rewritePopupContent_CITATION_BACK_LINK
    scrollToTargetedElementInPopFrame: (popFrame) => {
        GWLog("Extracts.scrollToTargetedElementInPopFrame", "extracts.js", 3);

        let target = popFrame.spawningTarget;

        if (isAnchorLink(target)) {
            requestAnimationFrame(() => {
            	let element = null;
                if (   popFrame
                    && (element = targetElementInDocument(target, popFrame.document)))
                    revealElement(element, true);
            });
        }
    },

    //  Make anchorlinks scroll pop-frame instead of opening normally.
	constrainLinkClickBehaviorInPopFrame: (popFrame, extraCondition = (link => true)) => {
        let target = popFrame.spawningTarget;

        popFrame.document.querySelectorAll("a").forEach(link => {
            if (   link.hostname == target.hostname
                && link.pathname == target.pathname
                && link.hash > ""
                && extraCondition(link)) {
                link.onclick = () => { return false; };
                link.addActivateEvent((event) => {
                    let hashTarget = targetElementInDocument(link, popFrame.document);
                    if (hashTarget) {
                        Extracts.popFrameProvider.scrollElementIntoViewInPopFrame(hashTarget, true);
                        return false;
                    } else {
                        return true;
                    }
                });
            }
        });
	},

    /***************************/
    /*  Pop-frames (in general).
     */

	popFrameTypeText: () => {
		return (Extracts.popFrameProvider == Popups
				? "popup"
				: "popin");
	},

    //  Called by: Extracts.preparePopup
    //  Called by: Extracts.preparePopin
    preparePopFrame: (popFrame) => {
        GWLog("Extracts.preparePopFrame", "extracts.js", 2);

        let target = popFrame.spawningTarget;

        //  Import the class(es) of the target.
        popFrame.classList.add(...target.classList);
        //  We then remove some of the imported classes.
        popFrame.classList.remove("has-annotation", "has-annotation-partial", 
        	"has-content", "link-self", "link-annotated", "link-page", 
        	"has-icon", "has-indicator-hook", "uri", "arrow-up", "arrow-down", 
        	"spawns-popup", "spawns-popin");

        //  Attempt to fill the popup.
        if (Extracts.fillPopFrame(popFrame) == false)
            return null;

        //  Add ‘markdownBody’ class.
        popFrame.body.classList.add("markdownBody");

		//	Inject styles.
		let inlinedStyleIDs = [ 
			"inlined-styles-colors", 
			"inlined-styles-colors-dark", 
			"mathjax-styles"
		];
		Array.from(document.styleSheets).filter(styleSheet => 
			(   styleSheet.ownerNode.tagName == "LINK" 
			 || inlinedStyleIDs.includes(styleSheet.ownerNode.id))
		).forEach(styleSheet => {
			let styleBlock = elementFromHTML("<style>"
				+ Array.from(styleSheet.cssRules).map(rule => rule.cssText).join("\n")
				+ "</style>");
			[ "id", "media" ].forEach(attribute => {
				if (styleSheet.ownerNode.hasAttribute(attribute))
					styleBlock.setAttribute(attribute, styleSheet.ownerNode.getAttribute(attribute));
			});
			popFrame.document.insertBefore(styleBlock, popFrame.body);
		});

		//	Add handler to update popup position when content changes.
		GW.notificationCenter.addHandlerForEvent("Rewrite.contentDidChange", (info) => {
			if (popFrame == null)
				return;

			if (Extracts.popFrameProvider == Popups)
				Popups.positionPopup(popFrame);
			else // if (Extracts.popFrameProvider == Popins)
				Popins.scrollPopinIntoView(popFrame);
		}, { 
			condition: (info) => (info.document == popFrame.document) 
		});

        return popFrame;
    },

	additionalRewrites: [ ],

    /**********/
    /*  Popins.
     */

	popinsDisabledLocalStorageItemKey: "extract-popins-disabled",

    //  Called by: Extracts.setup
    popinsEnabled: () => {
        return (localStorage.getItem(Extracts.popinsDisabledLocalStorageItemKey) != "true");
    },

    /*  Called by popins.js when adding a target.
     */
    //  (See Extracts.addTargetsWithin)
	preparePopinTarget: (target) => {
		target.adjustPopinWidth = (popin) => {
			let leftMargin, rightMargin;
			let popinRect = popin.getBoundingClientRect();
			if (GW.mediaQueries.mobileWidth.matches) {
				//	Make popin take up entire content column width.
				let bodyRect = document.body.getBoundingClientRect();
				leftMargin = (bodyRect.left - popinRect.left);
				rightMargin = (popinRect.right - bodyRect.right);
			} else {
				let containerSelector = [ 
					".abstract blockquote",
					".markdownBody" 
				].join(", ");
				let containerRect = popin.closest(containerSelector).getBoundingClientRect();
				leftMargin = (containerRect.left - popinRect.left);
				rightMargin = (popinRect.right - containerRect.right);
			}
			popin.style = `margin-left: ${leftMargin}px; `
						+ `margin-right: ${rightMargin}px; `
						+ `width: calc(${popinRect.width}px + ${(-1 * (leftMargin + rightMargin))}px)`;
		};
	},

    /*  Called by popins.js just before injecting the popin. This is our chance
        to fill the popin with content, and rewrite that content in whatever
        ways necessary. After this function exits, the popin will appear on the
        screen.
     */
    //  Called by: popins.js
    preparePopin: (popin) => {
        GWLog("Extracts.preparePopin", "extracts.js", 2);

        let target = popin.spawningTarget;

		//	Activate dynamic layout for the popin.
		startDynamicLayoutInContainer(popin.body);

        /*  Call generic pop-frame prepare function (which will attempt to fill
            the popin).
         */
        if ((popin = Extracts.preparePopFrame(popin)) == null)
            return null;

        //  Add popin title bar contents.
        let popinTitle = Extracts.titleForPopFrame(popin);
        if (popinTitle) {
            popin.titleBarContents = [
            	Extracts.disableExtractPopFramesPopFrameTitleBarButton(),
            	newElement("SPAN", { "class": "popframe-title" }, { "innerHTML": popinTitle.innerHTML }),
                Popins.titleBarComponents.closeButton()
            ];
        }

        //  Special handling for certain popin types.
        let targetTypeName = Extracts.targetTypeInfo(target).typeName;
        let specialPrepareFunction = Extracts[`preparePopin_${targetTypeName}`] || Extracts[`preparePopFrame_${targetTypeName}`];
        if (specialPrepareFunction)
            if ((popin = specialPrepareFunction(popin)) == null)
                return null;

        /*  If we’re waiting for content to be loaded into the popin
            asynchronously, then there’s no need to do rewrites for now.
         */
        if (Extracts.popFrameHasLoaded(popin))
            Extracts.rewritePopinContent(popin);

        return popin;
    },

    //	Called by: Extracts.preparePopin
    //  Called by: Extracts.postRefreshSuccessUpdatePopFrameForTarget
    rewritePopinContent: (popin) => {
        GWLog("Extracts.rewritePopinContent", "extracts.js", 2);

        let target = popin.spawningTarget;

        //  Update the title.
        Extracts.updatePopFrameTitle(popin);

        //  Special handling for certain popin types.
        let targetTypeName = Extracts.targetTypeInfo(target).typeName;
        let specialRewriteFunction = Extracts[`rewritePopinContent_${targetTypeName}`] || Extracts[`rewritePopFrameContent_${targetTypeName}`];
        if (specialRewriteFunction)
            specialRewriteFunction(popin);

		//	Additional rewrites.
		Extracts.additionalRewrites.forEach(rewriteFunction => {
			rewriteFunction(popin);
		});

		//	Register copy processors in popin.
		registerCopyProcessorsForDocument(popin.document);

        //  For object popins, scroll popin into view once object loads.
        let objectOfSomeSort = popin.document.querySelector("iframe, object, img, video");
        if (objectOfSomeSort) {
            objectOfSomeSort.addEventListener("load", (event) => {
                requestAnimationFrame(() => {
                    Popins.scrollPopinIntoView(popin);
                });
            });
        }
    },

    /**********/
    /*  Popups.
     */

	popupsDisabledLocalStorageItemKey: "extract-popups-disabled",
	
    //  Called by: Extracts.setup
    //  Called by: extracts-options.js
    popupsEnabled: () => {
        return (localStorage.getItem(Extracts.popupsDisabledLocalStorageItemKey) != "true");
    },

    //  Called by: Extracts.preparePopup
    spawnedPopupMatchingTarget: (target) => {
        return Popups.allSpawnedPopups().find(popup =>
                   Extracts.targetsMatch(target, popup.spawningTarget)
                && Popups.popupIsPinned(popup) == false);
    },

    /*  Called by popups.js when adding a target.
     */
    //  (See Extracts.addTargetsWithin)
    preparePopupTarget: (target) => {
        //  Remove the title attribute (saving it first);
        if (target.title) {
            target.dataset.attributeTitle = target.title;
            target.removeAttribute("title");
        }

        //  For special positioning by Popups.js.
        target.preferSidePositioning = () => {
            return (   target.closest("li") != null
                    && target.closest(".columns") == null);
        };
    },

    /*  Called by popups.js just before spawning (injecting and positioning) the
        popup. This is our chance to fill the popup with content, and rewrite
        that content in whatever ways necessary. After this function exits, the
        popup will appear on the screen.
     */
    //  (See also Extracts.addTargetsWithin)
    preparePopup: (popup) => {
        GWLog("Extracts.preparePopup", "extracts.js", 2);

        let target = popup.spawningTarget;

        /*  If a popup already exists that matches the target, do not spawn a
            new popup; just use the existing popup.
         */
        let existingPopup = Extracts.spawnedPopupMatchingTarget(target);
        if (existingPopup) {
            Popups.detachPopupFromTarget(existingPopup);
            existingPopup.spawningTarget = target;
            return existingPopup;
        }

		//	Activate dynamic layout for the popup.
		startDynamicLayoutInContainer(popup.body);

        /*  Call generic pop-frame prepare function (which will attempt to fill
            the popup).
         */
        if ((popup = Extracts.preparePopFrame(popup)) == null)
            return null;

        //  Add popup title bar contents.
        let popupTitle = Extracts.titleForPopFrame(popup);
        if (popupTitle) {
            popup.titleBarContents = [
                Popups.titleBarComponents.closeButton(),
                Popups.titleBarComponents.zoomButton().enableSubmenu(),
                Popups.titleBarComponents.pinButton(),
                newElement("SPAN", { "class": "popframe-title" }, { "innerHTML": popupTitle.innerHTML }),
                Extracts.disableExtractPopFramesPopFrameTitleBarButton()
            ];
        }

        //  Special handling for certain popup types.
        let targetTypeName = Extracts.targetTypeInfo(target).typeName;
        let specialPrepareFunction = Extracts[`preparePopup_${targetTypeName}`] || Extracts[`preparePopFrame_${targetTypeName}`];
        if (specialPrepareFunction)
            if ((popup = specialPrepareFunction(popup)) == null)
                return null;

        /*  If we’re waiting for content to be loaded into the popup
            asynchronously, then there’s no need to do rewrites for now.
         */
        if (Extracts.popFrameHasLoaded(popup))
            Extracts.rewritePopupContent(popup);

        return popup;
    },

    //	Called by: Extracts.preparePopup
    //  Called by: Extracts.postRefreshSuccessUpdatePopFrameForTarget
    rewritePopupContent: (popup) => {
        GWLog("Extracts.rewritePopupContent", "extracts.js", 2);

        let target = popup.spawningTarget;

        //  Special handling for certain popup types.
        let targetTypeName = Extracts.targetTypeInfo(target).typeName;
        let specialRewriteFunction = Extracts[`rewritePopupContent_${targetTypeName}`] || Extracts[`rewritePopFrameContent_${targetTypeName}`];
        if (specialRewriteFunction)
            specialRewriteFunction(popup);

		//	Additional rewrites.
		Extracts.additionalRewrites.forEach(rewriteFunction => {
			rewriteFunction(popup);
		});

		//	Register copy processors in popup.
		registerCopyProcessorsForDocument(popup.document);
    }
};

/*	Browser native image lazy loading does not seem to work in pop-frames (due 
	to the shadow root or the nested scroll container or some combination 
	thereof), so we have to implement it ourselves.
 */
Extracts.additionalRewrites.push(Extracts.lazyLoadImages = (popFrame) => {
    GWLog("Extracts.lazyLoadImages", "extracts.js", 2);

	popFrame.body.querySelectorAll("img[loading='lazy']").forEach(image => {
		lazyLoadObserver(() => {
			image.loading = "eager";
			image.decoding = "sync";
		}, image, {
			root: scrollContainerOf(image),
			rootMargin: window.innerHeight + "px"
		});
	});
});
/*=-------------=*/
/*= ANNOTATIONS =*/
/*=-------------=*/

Extracts.targetTypeDefinitions.push([
    "ANNOTATION",               // Type name
    "isAnnotatedLink",          // Type predicate function
    "has-annotation",           // Target classes to add
    "annotationForTarget",      // Pop-frame fill function
    "annotation"                // Pop-frame classes
]);

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    //  Called by: extracts.js
    //  Called by: extracts-content.js
    isAnnotatedLink: (target) => {
        return Annotations.isAnnotatedLinkFull(target);
    },

    /*  This “special testing function” is used to exclude certain targets which
        have already been categorized as (in this case) `ANNOTATION` targets. It
        returns false if the target is to be excluded, true otherwise. Excluded
        targets will not spawn pop-frames.
     */
    //  Called by: Extracts.targets.testTarget (as `testTarget_${targetTypeInfo.typeName}`)
    testTarget_ANNOTATION: (target) => {
        return (!(   Extracts.popFrameProvider == Popins
                  && (   Extracts.isTOCLink(target)
                      || Extracts.isSidebarLink(target))));
    },

    /*  An annotation for a link.
        */
    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    annotationForTarget: (target) => {
        GWLog("Extracts.annotationForTarget", "extracts-annotations.js", 2);

		return newDocument(synthesizeIncludeLink(target, {
			"class": "link-annotated include-annotation",
			"data-template": "annotation-blockquote-not"
		}));
    },

    //  Called by: extracts.js (as `titleForPopFrame_${targetTypeName}`)
    titleForPopFrame_ANNOTATION: (popFrame) => {
        GWLog("Extracts.titleForPopFrame_ANNOTATION", "extracts-annotations.js", 2);

        let target = popFrame.spawningTarget;
		let referenceData = Annotations.referenceDataForLink(target);
		if (referenceData == null) {
			referenceData = {
				popFrameTitleLinkHref:          target.href,
				popFrameTitleArchiveLinkHref:   (target.dataset.urlArchive ?? null),
				popFrameTitleText:              (target.hostname == location.hostname
												 ? target.pathname + target.hash
												 : target.href)
			};
			referenceData.popFrameTitleText = `<code>${referenceData.popFrameTitleText}</code>`;
		}

		return Transclude.fillTemplateNamed("pop-frame-title-annotation", referenceData, Extracts.getStandardPopFrameTitleTemplateFillContext());
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_ANNOTATION: (popup) => {
        let target = popup.spawningTarget;

        /*  Do not spawn annotation popup if the annotation is already visible
            on screen. (This may occur if the target is in a popup that was
            spawned from a backlinks popup for this same annotation as viewed on
            a tag index page, for example.)
         */
        let escapedLinkURL = CSS.escape(decodeURIComponent(target.href));
        let targetAnalogueInLinkBibliography = document.querySelector(`a[id^='link-bibliography'][href='${escapedLinkURL}']`);
        if (targetAnalogueInLinkBibliography) {
            let containingSection = targetAnalogueInLinkBibliography.closest("section");
            if (   containingSection
                && containingSection.querySelector("blockquote")
                && Popups.isVisible(containingSection)) {
                return null;
            }
        }

        return popup;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_ANNOTATION: (popFrame, injectEventInfo = null) => {
        GWLog("Extracts.rewritePopFrameContent_ANNOTATION", "extracts-annotations.js", 2);

		if (injectEventInfo == null) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopFrameContent_ANNOTATION(popFrame, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popFrame.body, {
				source: "Extracts.rewritePopFrameContent_ANNOTATION",
				container: popFrame.body,
				document: popFrame.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE

        let target = popFrame.spawningTarget;

        //  Mark annotations from non-local data sources.
		let referenceData = Annotations.referenceDataForLink(target);
        if (referenceData.content.dataSourceClass)
            Extracts.popFrameProvider.addClassesToPopFrame(popFrame, ...(referenceData.content.dataSourceClass.split(" ")));

        //  Update the title.
        Extracts.updatePopFrameTitle(popFrame);
    }
};

/*=-----------------------=*/
/*= ANNOTATIONS (PARTIAL) =*/
/*=-----------------------=*/

Extracts.targetTypeDefinitions.push([
    "ANNOTATION_PARTIAL",            // Type name
    "isPartialAnnotationLink",       // Type predicate function
    "has-annotation-partial",        // Target classes to add
    "partialAnnotationForTarget",    // Pop-frame fill function
    "annotation annotation-partial"  // Pop-frame classes
]);

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    //  Called by: extracts.js
    //  Called by: extracts-content.js
    isPartialAnnotationLink: (target) => {
        return Annotations.isAnnotatedLinkPartial(target);
    },

    /*  This “special testing function” is used to exclude certain targets which
        have already been categorized as (in this case) `ANNOTATION` targets. It
        returns false if the target is to be excluded, true otherwise. Excluded
        targets will not spawn pop-frames.
     */
    //  Called by: Extracts.targets.testTarget (as `testTarget_${targetTypeInfo.typeName}`)
    testTarget_ANNOTATION_PARTIAL: (target) => {
    	return Extracts.testTarget_ANNOTATION(target);
    },

    /*  A partial annotation for a link.
        */
    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    partialAnnotationForTarget: (target) => {
        GWLog("Extracts.partialAnnotationForTarget", "extracts-annotations.js", 2);

		return newDocument(synthesizeIncludeLink(target, {
			"class": "link-annotated-partial include-annotation-partial",
			"data-template": "annotation-blockquote-not"
		}));
    },

    //  Called by: extracts.js (as `titleForPopFrame_${targetTypeName}`)
    titleForPopFrame_ANNOTATION_PARTIAL: (popFrame) => {
        GWLog("Extracts.titleForPopFrame_ANNOTATION_PARTIAL", "extracts-annotations.js", 2);

		return Extracts.titleForPopFrame_ANNOTATION(popFrame);
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_ANNOTATION_PARTIAL: (popup) => {
    	return Extracts.preparePopup_ANNOTATION(popup);
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_ANNOTATION_PARTIAL: (popFrame) => {
        GWLog("Extracts.rewritePopFrameContent_ANNOTATION_PARTIAL", "extracts-annotations.js", 2);

		Extracts.rewritePopFrameContent_ANNOTATION(popFrame);
    }
};

/*	Inject partial-annotation metadata into a popup that is not already a
	partial annotation.
 */
Extracts.additionalRewrites.push(Extracts.injectPartialAnnotationMetadata = (popFrame) => {
    GWLog("Extracts.injectPartialAnnotationMetadata", "extracts.js", 2);

	let target = popFrame.spawningTarget;
	if (   Annotations.isAnnotatedLinkPartial(target) == false
		|| Extracts.targetTypeInfo(target).typeName == "ANNOTATION_PARTIAL")
		return;

	//	Construct container and synthesized include-link.
	let partialAnnotationAppendContainer = newElement("DIV", {
		"class": [ "partial-annotation-append-container",
				   "markdownBody",
				   "popframe-body",
				   (Extracts.popFrameProvider == Popups ? "popup-body" : "popin-body")
				   ].join(" ")
	});
	partialAnnotationAppendContainer.appendChild(synthesizeIncludeLink(target.href, {
		"class": "link-annotated-partial include-annotation-partial include-strict",
		"data-template": "annotation-blockquote-not"
	}));

	//	Add the whole thing to the pop-frame.
	Extracts.popFrameProvider.addPartToPopFrame(popFrame, partialAnnotationAppendContainer);
	Extracts.popFrameProvider.addClassesToPopFrame(popFrame, "has-footer");

	//	Trigger transclude of the partial annotation.
	Transclude.triggerTranscludesInContainer(partialAnnotationAppendContainer, {
		source: "Extracts.injectPartialAnnotationMetadata",
		container: partialAnnotationAppendContainer,
		document: partialAnnotationAppendContainer
	});
});

/*=----------------------=*/
/*= ANNOTATIONS: HELPERS =*/
/*=----------------------=*/

Extracts = { ...Extracts,
    annotationLoadHoverDelay: 25,

    //  Called by: extracts.js
    setUpAnnotationLoadEventsWithin: (container) => {
        GWLog("Extracts.setUpAnnotationLoadEventsWithin", "extracts-annotations.js", 1);

        //  Get all the annotated targets in the container.
        let allAnnotatedTargetsInContainer = Annotations.allAnnotatedLinksInContainer(container);

        if (Extracts.popFrameProvider == Popups) {
            //  Add hover event listeners to all the annotated targets.
            allAnnotatedTargetsInContainer.forEach(annotatedTarget => {
                annotatedTarget.removeAnnotationLoadEvents = onEventAfterDelayDo(annotatedTarget, "mouseenter", Extracts.annotationLoadHoverDelay, (event) => {
                    //  Do nothing if the annotation is already loaded.
                    if (Annotations.cachedDataExists(annotatedTarget) == false)
                        Annotations.load(annotatedTarget);
                }, "mouseleave");
            });

			if (allAnnotatedTargetsInContainer.length > 0) {
				/*  Set up handler to remove hover event listeners from all
					the annotated targets in the document.
					*/
				GW.notificationCenter.addHandlerForEvent("Extracts.cleanupDidComplete", (info) => {
					allAnnotatedTargetsInContainer.forEach(annotatedTarget => {
						if (annotatedTarget.removeAnnotationLoadEvents) {
							annotatedTarget.removeAnnotationLoadEvents();
							annotatedTarget.removeAnnotationLoadEvents = null;
						}
					});
				}, { once: true });
            }
        } else { // if (Extracts.popFrameProvider == Popins)
            //  Add click event listeners to all the annotated targets.
            allAnnotatedTargetsInContainer.forEach(annotatedTarget => {
                annotatedTarget.addEventListener("click", annotatedTarget.annotationLoad_click = (event) => {
                    //  Do nothing if the annotation is already loaded.
                    if (Annotations.cachedDataExists(annotatedTarget) == false)
                        Annotations.load(annotatedTarget);
                });
            });

            /*  Set up handler to remove click event listeners from all
                the annotated targets in the document.
                */
            GW.notificationCenter.addHandlerForEvent("Extracts.cleanupDidComplete", (info) => {
                allAnnotatedTargetsInContainer.forEach(annotatedTarget => {
                    annotatedTarget.removeEventListener("click", annotatedTarget.annotationLoad_click);
                });
            }, { once: true });
        }
    }
};
/***************************************************************************/
/*  The target-testing and pop-frame-filling functions in this section
	come in sets, which define and implement classes of pop-frames
	(whether those be popups, or popins, etc.). (These classes are things
	like “a link that has a statically generated extract provided for it”,
	“a link to a locally archived web page”, “an anchorlink to a section of
	the current page”, and so on.)

	Each set contains a testing function, which is called by
	testTarget() to determine if the target (link, etc.) is eligible for
	processing, and is also called by fillPopFrame() to find the
	appropriate filling function for a pop-frame spawned by a given
	target. The testing function takes a target element and examines its
	href or other properties, and returns true if the target is a member of
	that class of targets, false otherwise.

	NOTE: These testing (a.k.a. “type predicate”) functions SHOULD NOT be used
	directly, but only via Extracts.targetTypeInfo()!

	Each set also contains the corresponding filling function, which
	is called by fillPopFrame() (chosen on the basis of the return values
	of the testing functions, and the specified order in which they’re
	called). The filling function takes a target element and returns a
	string which comprises the HTML contents that should be injected into
	the pop-frame spawned by the given target.
 */

Extracts.targetTypeDefinitions.insertBefore([
	"LOCAL_PAGE",          // Type name
	"isLocalPageLink",     // Type predicate function
	"has-content",         // Target classes to add
	"localPageForTarget",  // Pop-frame fill function
	"local-page"           // Pop-frame classes
], (def => def[0] == "ANNOTATION_PARTIAL"));

/*=-------------=*/
/*= LOCAL PAGES =*/
/*=-------------=*/

Extracts = { ...Extracts,
    /*  Local links (to sections of the current page, or other site pages).
     */
    //  Called by: Extracts.targetTypeInfo (as `predicateFunctionName`)
    isLocalPageLink: (target) => {
        return (   Content.contentTypes.localPage.matches(target)
				&& (   isAnchorLink(target)
					|| target.pathname != location.pathname));
    },

    /*  TOC links.
     */
    //  Called by: Extracts.testTarget_LOCAL_PAGE
    //  Called by: Extracts.preparePopup_LOCAL_PAGE
    isTOCLink: (target) => {
        return (target.closest("#TOC") != null);
    },

    /*  Links in the sidebar.
     */
    //  Called by: Extracts.testTarget_LOCAL_PAGE
    isSidebarLink: (target) => {
        return (target.closest("#sidebar") != null);
    },

	/*	“Full context” links in backlinks lists.
	 */
	isFullBacklinkContextLink: (target) => {
		return (   target.closest(".backlink-source") != null
				&& target.classList.contains("link-page"));
	},

    /*  This “special testing function” is used to exclude certain targets which
        have already been categorized as (in this case) `LOCAL_PAGE` targets. It
        returns false if the target is to be excluded, true otherwise. Excluded
        targets will not spawn pop-frames.
     */
    //  Called by: Extracts.targets.testTarget (as `testTarget_${targetTypeInfo.typeName}`)
    testTarget_LOCAL_PAGE: (target) => {
        return (!(   Extracts.popFrameProvider == Popins
        		  && (   Extracts.isTOCLink(target)
        			  || Extracts.isSidebarLink(target))));
    },

    //  Called by: Extracts.fillPopFrame (as `popFrameFillFunctionName`)
    //	Called by: Extracts.citationForTarget (extracts-content.js)
    //	Called by: Extracts.citationBackLinkForTarget (extracts-content.js)
    localPageForTarget: (target, forceNarrow) => {
        GWLog("Extracts.localPageForTarget", "extracts.js", 2);

		/*  Check to see if the target location matches an already-displayed
			page (which can be the root page of the window).

			If the entire linked page is already displayed, and if the
			target points to an anchor in that page, display the linked
			section or element.

			Also display just the linked block if we’re spawning this
			pop-frame from an in-pop-frame TOC.

			Otherwise, display the entire linked page.
		 */
		let fullPage = !(   isAnchorLink(target)
        				 && (   forceNarrow
        					 || target.closest(".TOC")
        					 || Extracts.targetDocument(target)));
        if (fullPage) {
            /*  Note that we might end up here because there is yet no
                pop-frame with the full linked document, OR because there is
                such a pop-frame but it’s a pinned popup or something (and thus
                we didn’t want to despawn it and respawn it at this target’s
                location).
            */
			/*  Mark the pop-frame as a full page embed, and give it suitable
				identifying classes.
			 */
			Extracts.popFrameProvider.addClassesToPopFrame(target.popFrame, "full-page");
        }

		//	Designate “full context” pop-frames for backlinks.
		if (Extracts.isFullBacklinkContextLink(target))
			Extracts.popFrameProvider.addClassesToPopFrame(target.popFrame, "full-backlink-context");

		//	Synthesize include-link (with or without hash, as appropriate).
		let includeLink = synthesizeIncludeLink(target, { class: "include-block-context-expanded" });
		if (fullPage) {
			stripAnchorsFromLink(includeLink);
		} else if (   Extracts.isFullBacklinkContextLink(target)
				   && target.pathname == location.pathname) {
			/*	Since “full” context is just the base page, which we don’t want 
				to pop up/in, we instead show the containing section or
				footnote.
			 */
			let targetElement = targetElementInDocument(target, Extracts.rootDocument);
			let nearestSection = targetElement.closest("section, li.footnote");
			if (nearestSection)
				includeLink.hash = "#" + nearestSection.id;
		}

		return newDocument(includeLink);
    },

    //  Called by: Extracts.titleForPopFrame (as `titleForPopFrame_${targetTypeName}`)
    titleForPopFrame_LOCAL_PAGE: (popFrame) => {
        GWLog("Extracts.titleForPopFrame_LOCAL_PAGE", "extracts.js", 2);

        let target = popFrame.spawningTarget;
        let referenceData = Content.referenceDataForLink(target);

		let popFrameTitleText, popFrameTitleLinkHref;
		if (referenceData == null) {
			popFrameTitleText = "";
			if (target.pathname != location.pathname)
				popFrameTitleText += target.pathname;
			if (popFrame.classList.contains("full-page") == false)
				popFrameTitleText += target.hash;
			popFrameTitleText = `<code>${popFrameTitleText}</code>`;

			popFrameTitleLinkHref = target.href;
		} else {
			popFrameTitleText = popFrame.classList.contains("full-page")
								? referenceData.popFrameTitleTextShort
								: referenceData.popFrameTitleText;
			popFrameTitleLinkHref = referenceData.popFrameTitleLinkHref;
		}

		if (popFrame.classList.contains("backlinks")) {
			popFrameTitleText += " (Backlinks)";
		}

		return Transclude.fillTemplateNamed("pop-frame-title-standard", {
			popFrameTitleLinkHref:  popFrameTitleLinkHref,
			popFrameTitleText:      popFrameTitleText
		}, Extracts.getStandardPopFrameTitleTemplateFillContext());
    },

	preparePopFrame_LOCAL_PAGE: (popFrame) => {
        GWLog("Extracts.preparePopFrame_LOCAL_PAGE", "extracts.js", 2);

        let target = popFrame.spawningTarget;

		/*	For local content embed pop-frames, add handler to trigger
			transcludes in source content when they trigger in the pop-frame.
		 */
		if (Content.cachedDataExists(target)) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Content.updateCachedContent(target, (content) => {
					Transclude.allIncludeLinksInContainer(content).filter(includeLink =>
						includeLink.href == info.includeLink.href
					).forEach(includeLink => {
						Transclude.transclude(includeLink, true);
					});
				});
			}, { condition: (info) => (   info.source == "transclude"
									   && info.document == popFrame.document) });
		}

		return popFrame;
	},

    //  Called by: Extracts.preparePopup (as `preparePopup_${targetTypeName}`)
    preparePopup_LOCAL_PAGE: (popup) => {
        GWLog("Extracts.preparePopup_LOCAL_PAGE", "extracts.js", 2);

        let target = popup.spawningTarget;

		popup = Extracts.preparePopFrame_LOCAL_PAGE(popup);

		//  Do not spawn “full context” popup if the link is visible.
 		if (   Extracts.isFullBacklinkContextLink(target)
 			&& popup.classList.contains("full-page") == false
 			&& Popups.isVisible(targetElementInDocument(target, Extracts.rootDocument)))
			return null;

       /*  Designate popups spawned from section links in the the TOC (for
            special styling).
         */
        if (Extracts.isTOCLink(target))
        	Extracts.popFrameProvider.addClassesToPopFrame(popup, "toc-section");

        return popup;
    },

    //  Called by: Extracts.rewritePopinContent_LOCAL_PAGE
    //  Called by: Extracts.rewritePopupContent_LOCAL_PAGE
    //  Called by: Extracts.rewritePopinContent (as `rewritePopFrameContent_${targetTypeName}`)
    //  Called by: Extracts.rewritePopupContent (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_PAGE: (popFrame, injectEventInfo = null) => {
        GWLog("Extracts.rewritePopFrameContent_LOCAL_PAGE", "extracts.js", 2);

		let target = popFrame.spawningTarget;

		if (injectEventInfo == null) {
			//	Preliminary rewrites.
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				//	Add page body classes.
				let referenceData = Content.referenceDataForLink(target);
				Extracts.popFrameProvider.addClassesToPopFrame(popFrame, ...(referenceData.pageBodyClasses));

				//	Update pop-frame title.
				Extracts.updatePopFrameTitle(popFrame);
			}, {
				phase: "<",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Main rewrites.
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopFrameContent_LOCAL_PAGE(popFrame, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popFrame.body, {
				source: "Extracts.rewritePopFrameContent_LOCAL_PAGE",
				container: popFrame.body,
				document: popFrame.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE

		//	Provider-specific rewrites.
		if (Extracts.popFrameProvider == Popups)
			Extracts.rewritePopupContent_LOCAL_PAGE(popFrame, injectEventInfo);
		else // if (Extracts.popFrameProvider == Popins)
			Extracts.rewritePopinContent_LOCAL_PAGE(popFrame, injectEventInfo);

		//	Something failed somehow.
		if (isNodeEmpty(injectEventInfo.container)) {
			Extracts.popFrameProvider.addClassesToPopFrame(popFrame, "loading-failed");
			return;
		}

		//	Make first image load eagerly.
		let firstImage = (   popFrame.body.querySelector(".page-thumbnail")
						  || popFrame.body.querySelector("figure img"))
		if (firstImage) {
			firstImage.loading = "eager";
			firstImage.decoding = "sync";
		}

		//	Strip a single collapse block encompassing the top level content.
		if (   isOnlyChild(injectEventInfo.container.firstElementChild)
			&& injectEventInfo.container.firstElementChild.classList.contains("collapse"))
			expandLockCollapseBlock(injectEventInfo.container.firstElementChild);

		//	Designate section backlinks popups as such.
		if (injectEventInfo.container.firstElementChild.classList.containsAnyOf([ "section-backlinks", "section-backlinks-container" ]))
			Extracts.popFrameProvider.addClassesToPopFrame(popFrame, "aux-links", "backlinks");

		/*	In the case where the spawning link points to a specific element
			within the transcluded content, but we’re transcluding the full
			page and not just the block context of the targeted element,
			transclude.js has not marked the targeted element for us already.
			So we must do it here.
		 */
		if (   isAnchorLink(target)
			&& popFrame.classList.containsAnyOf([ "full-page", "full-backlink-context" ]))
			targetElementInDocument(target, popFrame.document).classList.add("block-context-highlighted");

		//  Scroll to the target.
		Extracts.scrollToTargetedElementInPopFrame(popFrame);

		//	Lazy-loading of adjacent sections.
		//	WARNING: Experimental code!
// 		if (target.hash > "") {
// 			requestAnimationFrame(() => {
// 				Extracts.loadAdjacentSections(popFrame, "next,prev");
// 			});
// 		}
    },

    //  Called by: Extracts.rewritePopupContent (as `rewritePopupContent_${targetTypeName}`)
    rewritePopupContent_LOCAL_PAGE: (popup, injectEventInfo = null) => {
        GWLog("Extracts.rewritePopupContent_LOCAL_PAGE", "extracts.js", 2);

		if (injectEventInfo == null) {
			Extracts.rewritePopFrameContent_LOCAL_PAGE(popup);
			return;
		}

        let target = popup.spawningTarget;

		let referenceData = Content.referenceDataForLink(target);
		if (referenceData) {
			//	Insert page thumbnail into page abstract.
			if (   referenceData.pageThumbnailHTML
				&& popup.document.querySelector("img.page-thumbnail") == null) {
				let pageAbstract = popup.document.querySelector("#page-metadata + .abstract blockquote");
				if (pageAbstract)
					pageAbstract.insertBefore(newElement("FIGURE", {
						class: "float-right"
					}, {
						innerHTML: referenceData.pageThumbnailHTML
					}), pageAbstract.firstChild);
			}
		}

        //  Make anchorlinks scroll popup instead of opening normally.
		Extracts.constrainLinkClickBehaviorInPopFrame(popup);
    },

    //  Called by: Extracts.rewritePopinContent (as `rewritePopinContent_${targetTypeName}`)
    rewritePopinContent_LOCAL_PAGE: (popin, injectEventInfo = null) => {
        GWLog("Extracts.rewritePopinContent_LOCAL_PAGE", "extracts.js", 2);

		if (injectEventInfo == null) {
			Extracts.rewritePopFrameContent_LOCAL_PAGE(popin);
			return;
		}

        /*  Make anchorlinks scroll popin instead of opening normally
        	(but only for non-popin-spawning anchorlinks).
         */
		Extracts.constrainLinkClickBehaviorInPopFrame(popin, (link => link.classList.contains("no-popin")));
    },

	loadAdjacentSections: (popFrame, which) => {
        GWLog("Extracts.loadAdjacentSections", "extracts.js", 2);

		which = which.split(",");
		let next = which.includes("next");
		let prev = which.includes("prev");

		let target = popFrame.spawningTarget;
		let sourceDocument = Extracts.cachedPages[target.pathname] || Extracts.rootDocument;

		popFrame.firstSection = popFrame.firstSection || targetElementInDocument(target, sourceDocument);
		popFrame.lastSection = popFrame.lastSection || popFrame.firstSection;

		if (!(next || prev))
			return;

		if (targetElementInDocument(target, popFrame.document) == null) {
			let sectionWrapper = newElement("SECTION", {
				"id": popFrame.firstSection.id,
				"class": [ ...(popFrame.firstSection.classList) ].join(" ")
			});
			sectionWrapper.replaceChildren(...(popFrame.body.children));
			popFrame.body.appendChild(sectionWrapper);

			//  Fire a contentDidInject event.
			GW.notificationCenter.fireEvent("GW.contentDidInject", {
				source: "Extracts.loadAdjacentSections",
				container: popFrame.body.firstElementChild,
				document: popFrame.document,
				loadLocation: URLFromString(target.href)
			});
		}

		let prevSection = popFrame.firstSection.previousElementSibling;
		if (prev && prevSection) {
			popFrame.body.insertBefore(newDocument(prevSection), popFrame.body.firstElementChild);

			//  Fire a contentDidInject event.
			GW.notificationCenter.fireEvent("GW.contentDidInject", {
				source: "Extracts.loadAdjacentSections",
				container: popFrame.body.firstElementChild,
				document: popFrame.document,
				loadLocation: URLFromString(target.href)
			});

			popFrame.firstSection = prevSection;
		}

		let nextSection = popFrame.lastSection.nextElementSibling;
		if (next && nextSection) {
			popFrame.body.insertBefore(newDocument(nextSection), null);

			//  Fire a contentDidInject event.
			GW.notificationCenter.fireEvent("GW.contentDidInject", {
				source: "Extracts.loadAdjacentSections",
				container: popFrame.body.lastElementChild,
				document: popFrame.document,
				loadLocation: URLFromString(target.href)
			});

			popFrame.lastSection = nextSection;
		}
	}
};

/*=-----------------=*/
/*= AUXILIARY LINKS =*/
/*=-----------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "AUX_LINKS_LINK",       // Type name
    "isAuxLinksLink",       // Type predicate function
    "has-content",          // Target classes to add
    "auxLinksForTarget",    // Pop-frame fill function
    "aux-links"             // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: Extracts.isLocalCodeFileLink
    //  Called by: extracts.js (as `predicateFunctionName`)
    isAuxLinksLink: (target) => {
        let auxLinksLinkType = AuxLinks.auxLinksLinkType(target);
        return (   auxLinksLinkType != null
                && target.classList.contains(auxLinksLinkType));
    },

    /*  This “special testing function” is used to exclude certain targets which
        have already been categorized as (in this case) `AUX_LINKS_LINK` targets.
        It returns false if the target is to be excluded, true otherwise.
        Excluded targets will not spawn pop-frames.
     */
    //  Called by: Extracts.targets.testTarget (as `testTarget_${targetTypeInfo.typeName}`)
    testTarget_AUX_LINKS_LINK: (target) => {
        let exclude = false;
        let auxLinksType = AuxLinks.auxLinksLinkType(target);
        let containingAnnotation = target.closest(".annotation");
        if (containingAnnotation) {
            let includedAuxLinksBlock = containingAnnotation.querySelector(`.${auxLinksType}-append`);
            if (includedAuxLinksBlock)
                exclude = true;
        }

        return (!(   Extracts.popFrameProvider == Popins
                  && exclude == true));
    },

    /*  Backlinks, similar-links, etc.
     */
    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    auxLinksForTarget: (target) => {
        GWLog("Extracts.auxLinksForTarget", "extracts-content.js", 2);

		return newDocument(synthesizeIncludeLink(target, { class: AuxLinks.auxLinksLinkType(target) }));
    },

    //  Called by: Extracts.preparePopFrame (as `preparePopFrame_${targetTypeName}`)
    preparePopFrame_AUX_LINKS_LINK: (popFrame) => {
        GWLog("Extracts.preparePopFrame_AUX_LINKS_LINK", "extracts-content.js", 2);

        let auxLinksLinkType = AuxLinks.auxLinksLinkType(popFrame.spawningTarget);
        if (auxLinksLinkType > "")
            Extracts.popFrameProvider.addClassesToPopFrame(popFrame, auxLinksLinkType);

        return popFrame;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_AUX_LINKS_LINK: (popFrame, injectEventInfo = null) => {
        let target = popFrame.spawningTarget;

		if (injectEventInfo == null) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopFrameContent_AUX_LINKS_LINK(popFrame, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popFrame.body, {
				source: "Extracts.rewritePopFrameContent_AUX_LINKS_LINK",
				container: popFrame.body,
				document: popFrame.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE

		if (Extracts.popFrameProvider == Popups) {
			popFrame.document.querySelectorAll(".backlink-source a:nth-of-type(2)").forEach(fullContextLink => {
				let targetDocument = Extracts.targetDocument(fullContextLink);
				if (targetDocument) {
					let targetElement = targetElementInDocument(fullContextLink, targetDocument);
					fullContextLink.addEventListener("mouseenter", (event) => {
						targetElement.classList.toggle("block-context-highlighted-temp", true);
					});
					fullContextLink.addEventListener("mouseleave", (event) => {
						targetElement.classList.toggle("block-context-highlighted-temp", false);
					});
					GW.notificationCenter.addHandlerForEvent("Popups.popupWillDespawn", (info) => {
						targetElement.classList.toggle("block-context-highlighted-temp", false);
					}, {
						once: true,
						condition: (info) => (info.popup == popFrame)
					});
				}
			});
		}
    },

    //  Called by: extracts.js (as `titleForPopFrame_${targetTypeName}`)
    titleForPopFrame_AUX_LINKS_LINK: (popFrame) => {
        let target = popFrame.spawningTarget;
        let targetPage = AuxLinks.targetOfAuxLinksLink(target);
        let auxLinksLinkType = AuxLinks.auxLinksLinkType(target);
        switch (auxLinksLinkType) {
		case "backlinks":
			return newDocument(`<code>${targetPage}</code><span> (Backlinks)</span>`);
		case "similars":
			return newDocument(`<code>${targetPage}</code><span> (Similar links)</span>`);
		case "link-bibliography":
			return newDocument(`<code>${targetPage}</code><span> (Link bibliography)</span>`);
		default:
			return newDocument(`<code>${targetPage}</code>`);
        }
    },
};

/*=----------------=*/
/*= DROP-CAP LINKS =*/
/*=----------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "DROP_CAP_LINK",     // Type name
    "isDropCapLink",     // Type predicate function
    null,                // Target classes to add
    "dropCapForTarget",  // Pop-frame fill function
    "drop-cap"           // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    isDropCapLink: (target) => {
        return target.classList.contains("link-drop-cap");
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    dropCapForTarget: (target) => {
        GWLog("Extracts.dropCapForTarget", "extracts-content.js", 2);

		let letter = target.dataset.letter;
		let dropCapType = target.dataset.dropCapType;

		return newDocument(
			  `<p>A capital letter <strong>${letter}</strong> drop-cap initial, from the `
			+ `<a class="link-page" href="/dropcap#${dropCapType}"><strong>${dropCapType}</strong></a>`
			+ ` drop-cap font.</p>`
		)
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_DROP_CAP_LINK: (popup) => {
        let target = popup.spawningTarget;

        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_DROP_CAP_LINK: (popFrame) => {
        GWLog("Extracts.rewritePopFrameContent_DROP_CAP_LINK", "extracts.js", 2);

		//	Determine load location.
        let target = popFrame.spawningTarget;
		let containingPopFrame = Extracts.popFrameProvider.containingPopFrame(target);
		let loadLocation = containingPopFrame
						   ? containingPopFrame.spawningTarget
						   : location;
		
		//	Fire events.
		GW.notificationCenter.fireEvent("GW.contentDidLoad", {
			source: "Extracts.rewritePopFrameContent_DROP_CAP_LINK",
			container: popFrame.body,
			document: popFrame.document,
			loadLocation: new URL(loadLocation.href)
		});
		GW.notificationCenter.fireEvent("GW.contentDidInject", {
			source: "Extracts.rewritePopFrameContent_DROP_CAP_LINK",
			container: popFrame.body,
			document: popFrame.document,
			loadLocation: new URL(loadLocation.href),
			flags: GW.contentDidInjectEventFlags.clickable
		});
    },
};

/*=-----------=*/
/*= CITATIONS =*/
/*=-----------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "CITATION",             // Type name
    "isCitation",           // Type predicate function
    null,                   // Target classes to add
    "citationForTarget",    // Pop-frame fill function
    "footnote"              // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    isCitation: (target) => {
        return target.classList.contains("footnote-ref");
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    citationForTarget: (target) => {
        GWLog("Extracts.citationForTarget", "extracts-content.js", 2);

		return Extracts.localPageForTarget(target, true);
    },

    //  Called by: extracts.js (as `titleForPopFrame_${targetTypeName}`)
    titleForPopFrame_CITATION: (popFrame) => {
        let target = popFrame.spawningTarget;
        let footnoteNumber = target.querySelector("sup").textContent;
        let popFrameTitleText = `Footnote #${footnoteNumber}`;

        return Extracts.standardPopFrameTitleElementForTarget(target, popFrameTitleText);
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_CITATION: (popup) => {
        let target = popup.spawningTarget;

        /*  Do not spawn footnote popup if the {side|foot}note it points to is
            visible.
         */
        if (Array.from(Notes.allNotesForCitation(target)).findIndex(note => Popups.isVisible(note)) != -1)
            return null;

        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        /*  Add event listeners to highlight citation when its footnote
            popup is hovered over.
         */
        popup.addEventListener("mouseenter", (event) => {
            target.classList.toggle("highlighted", true);
        });
        popup.addEventListener("mouseleave", (event) => {
            target.classList.toggle("highlighted", false);
        });
        GW.notificationCenter.addHandlerForEvent("Popups.popupWillDespawn", (info) => {
            target.classList.toggle("highlighted", false);
        }, {
			once: true,
			condition: (info) => (info.popup == popup)
		});

        return popup;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_CITATION: (popFrame, injectEventInfo = null) => {
        GWLog("Extracts.rewritePopFrameContent_CITATION", "extracts.js", 2);

		if (injectEventInfo == null) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopFrameContent_CITATION(popFrame, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popFrame.body, {
				source: "Extracts.rewritePopFrameContent_CITATION",
				container: popFrame.body,
				document: popFrame.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE

		/*	Unwrap sidenote. (Corrects for edge case where a popup for a section
			of the current page which is currently within a collapsed section, 
			contains a footnote reference. Hovering over the citation will spawn
			a popup instead of sliding up the sidenote, as the latter is hidden.
			The sidenote, once transcluded, must then be unwrapped specially.)
		 */
		if (injectEventInfo.container.firstElementChild.classList.contains("sidenote"))
			unwrap(injectEventInfo.container.querySelector(".sidenote-inner-wrapper"));
    },
};

/*=---------------------=*/
/*= CITATIONS BACKLINKS =*/
/*=---------------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "CITATION_BACK_LINK",               // Type name
    "isCitationBackLink",               // Type predicate function
    null,                               // Target classes to add
    "citationBackLinkForTarget",        // Pop-frame fill function
    "citation-context"                  // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    isCitationBackLink: (target) => {
        return target.classList.contains("footnote-back");
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    citationBackLinkForTarget: (target) => {
        GWLog("Extracts.citationBackLinkForTarget", "extracts-content.js", 2);

        return Extracts.localPageForTarget(target, true);
    },

    /*  This “special testing function” is used to exclude certain targets which
        have already been categorized as (in this case) `CITATION_BACK_LINK`
        targets. It returns false if the target is to be excluded, true
        otherwise. Excluded targets will not spawn pop-frames.
     */
    //  Called by: extracts.js (as `testTarget_${targetTypeInfo.typeName}`)
    testTarget_CITATION_BACK_LINK: (target) => {
        return (Extracts.popFrameProvider != Popins);
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_CITATION_BACK_LINK: (popup) => {
        let target = popup.spawningTarget;

        //  Do not spawn citation context popup if citation is visible.
        let targetDocument = Extracts.targetDocument(target);
        if (targetDocument) {
        	let targetElement = targetElementInDocument(target, targetDocument);
        	if (   targetElement
        		&& Popups.isVisible(targetElement))
        		return null;
        }

        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    },

    //  Called by: extracts.js (as `rewritePopupContent_${targetTypeName}`)
    rewritePopupContent_CITATION_BACK_LINK: (popup, injectEventInfo = null) => {
        let target = popup.spawningTarget;

		if (injectEventInfo == null) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopupContent_CITATION_BACK_LINK(popup, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popup.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popup.body, {
				source: "Extracts.rewritePopupContent_CITATION_BACK_LINK",
				container: popup.body,
				document: popup.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE

        //  Highlight citation in popup.
        /*  Remove the .targeted class from a targeted citation (if any)
            inside the popup (to prevent confusion with the citation that
            the spawning link points to, which will be highlighted).
         */
        popup.document.querySelectorAll(".footnote-ref.targeted").forEach(targetedCitation => {
            targetedCitation.classList.remove("targeted");
        });
        //  In the popup, the citation for which context is being shown.
        let citationInPopup = targetElementInDocument(target, popup.document);
        //  Highlight the citation.
        citationInPopup.classList.add("targeted");
        //	Remove class that would interfere with styling.
        citationInPopup.classList.remove("block-context-highlighted");

        //  Scroll to the citation.
        Extracts.scrollToTargetedElementInPopFrame(popup);
    }
};

/*=---------------=*/
/*= REMOTE VIDEOS =*/
/*=---------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "VIDEO",                // Type name
    "isVideoLink",          // Type predicate function
    "has-content",          // Target classes to add
    "videoForTarget",       // Pop-frame fill function
    "video object"          // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    // Called by: Extracts.isVideoLink
    // Called by: Extracts.videoForTarget
    youtubeId: (url) => {
        let match = url.href.match(/^.*(youtu.be\/|v\/|u\/\w\/|embed\/|watch\?v=|\&v=)([^#\&\?]*).*/);
        if (   match
			&& match.length == 3
            && match[2].length == 11) {
            return match[2];
        } else {
            return null;
        }
    },

    // Called by: Extracts.isVideoLink
    // Called by: Extracts.videoForTarget
	vimeoId: (url) => {
		let match = url.pathname.match(/^\/([0-9]+)$/);
		if (   match
			&& match.length == 2) {
			return match[1];
		} else {
			return null;
		}
	},

    //  Called by: extracts.js (as `predicateFunctionName`)
    isVideoLink: (target) => {
        if ([ "www.youtube.com", "youtube.com", "youtu.be" ].includes(target.hostname)) {
            return (Extracts.youtubeId(target) != null);
        } else if ([ "vimeo.com" ].includes(target.hostname)) {
        	return (Extracts.vimeoId(target) != null);
        } else {
            return false;
        }
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    videoForTarget: (target) => {
        GWLog("Extracts.videoForTarget", "extracts-content.js", 2);

        if ([ "www.youtube.com", "youtube.com", "youtu.be" ].includes(target.hostname)) {
			let srcdocStyles =
				  `<style>`
				+ `* { padding: 0; margin: 0; overflow: hidden; } `
				+ `html, body { height: 100%; } `
				+ `img, span { position: absolute; width: 100%; top: 0; bottom: 0; margin: auto; } `
				+ `span { height: 1.5em; text-align: center; font: 48px/1.5 sans-serif; color: white; text-shadow: 0 0 0.5em black; }`
				+ `</style>`;

			let videoId = Extracts.youtubeId(target);
			let videoEmbedURL = URLFromString(`https://www.youtube.com/embed/${videoId}`);
			let placeholderImgSrc = `https://img.youtube.com/vi/${videoId}/hqdefault.jpg`;
			let playButtonHTML = `<span class='video-embed-play-button'>&#x25BA;</span>`;
			let srcdocHTML = `<a href='${videoEmbedURL.href}?autoplay=1'><img src='${placeholderImgSrc}'>${playButtonHTML}</a>`;

			//  `allow-same-origin` only for EXTERNAL videos, NOT local videos!
			return newDocument(Extracts.objectHTMLForURL(videoEmbedURL,
				`srcdoc="${srcdocStyles}${srcdocHTML}" sandbox="allow-scripts allow-same-origin" allowfullscreen`));
        } else if ([ "vimeo.com" ].includes(target.hostname)) {
			let videoId = Extracts.vimeoId(target);
			let videoEmbedURL = URLFromString(`https://player.vimeo.com/video/${videoId}`);
        	return newDocument(Extracts.objectHTMLForURL(videoEmbedURL,
        		`allow="autoplay; fullscreen; picture-in-picture" allowfullscreen`));
		}
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_VIDEO: (popup) => {
		let target = popup.spawningTarget;

		if ([ "www.youtube.com", "youtube.com", "youtu.be" ].includes(target.hostname)) {
			Extracts.popFrameProvider.addClassesToPopFrame(popup, "youtube");
		} else if ([ "vimeo.com" ].includes(target.hostname)) {
			Extracts.popFrameProvider.addClassesToPopFrame(popup, "vimeo");
		}

        return popup;
    },
};

/*=-----------------------=*/
/*= LOCALLY HOSTED VIDEOS =*/
/*=-----------------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "LOCAL_VIDEO",              // Type name
    "isLocalVideoLink",         // Type predicate function
    "has-content",              // Target classes to add
    "localVideoForTarget",      // Pop-frame fill function
    "video object"              // Pop-frame class
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Used in: Extracts.isLocalVideoLink
    videoFileExtensions: [ "mp4", "webm" ],

    //  Called by: extracts.js (as `predicateFunctionName`)
    isLocalVideoLink: (target) => {
        if (target.hostname != location.hostname)
            return false;

        let videoFileURLRegExp = new RegExp(
              '('
            + Extracts.videoFileExtensions.map(ext => `\\.${ext}`).join("|")
            + ')$'
        , 'i');
        return (target.pathname.match(videoFileURLRegExp) != null);
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    localVideoForTarget: (target) => {
        GWLog("Extracts.localVideoForTarget", "extracts-content.js", 2);

        return newDocument(
              `<figure>`
            + `<video controls="controls" preload="none">`
            + `<source src="${target.href}">`
            + `</video></figure>`);
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_LOCAL_VIDEO: (popup) => {
        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_VIDEO: (popFrame) => {
    	let video = popFrame.document.querySelector("video");
    	let source = video.querySelector("source");

		Extracts.popFrameProvider.addClassesToPopFrame(popFrame, "loading");

		doAjax({
			location: source.src,
			method: "HEAD",
			onSuccess: (event) => {
				Extracts.postRefreshUpdatePopFrameForTarget(popFrame.spawningTarget, true);
			},
			onFailure: (event) => {
                Extracts.postRefreshUpdatePopFrameForTarget(popFrame.spawningTarget, false);
			}
		});
    }
};

/*=----------------------------=*/
/*= LOCALLY HOSTED AUDIO FILES =*/
/*=----------------------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "LOCAL_AUDIO",              // Type name
    "isLocalAudioLink",         // Type predicate function
    "has-content",              // Target classes to add
    "localAudioForTarget",      // Pop-frame fill function
    "audio object"              // Pop-frame class
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Used in: Extracts.isLocalVideoLink
    audioFileExtensions: [ "mp3" ],

    //  Called by: extracts.js (as `predicateFunctionName`)
    isLocalAudioLink: (target) => {
        if (target.hostname != location.hostname)
            return false;

        let audioFileURLRegExp = new RegExp(
              '('
            + Extracts.audioFileExtensions.map(ext => `\\.${ext}`).join("|")
            + ')$'
        , 'i');
        return (target.pathname.match(audioFileURLRegExp) != null);
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    localAudioForTarget: (target) => {
        GWLog("Extracts.localAudioForTarget", "extracts-content.js", 2);

        return newDocument(
        	  `<figure>`
            + `<audio controls="controls" preload="none">`
            + `<source src="${target.href}">`
            + `</audio></figure>`);
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_LOCAL_AUDIO: (popup) => {
        //  Mini title bar.
        popup.classList.add("mini-title-bar");

		//	Audio elements can’t get taller.
        popup.classList.add("no-resize-height");

        return popup;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_AUDIO: (popFrame) => {
    	let audio = popFrame.document.querySelector("audio");
    	let source = audio.querySelector("source");

		Extracts.popFrameProvider.addClassesToPopFrame(popFrame, "loading");

		doAjax({
			location: source.src,
			method: "HEAD",
			onSuccess: (event) => {
				Extracts.postRefreshUpdatePopFrameForTarget(popFrame.spawningTarget, true);
			},
			onFailure: (event) => {
				Extracts.postRefreshUpdatePopFrameForTarget(popFrame.spawningTarget, false);
			}
		});
    }
};

/*=-----------------------=*/
/*= LOCALLY HOSTED IMAGES =*/
/*=-----------------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "LOCAL_IMAGE",              // Type name
    "isLocalImageLink",         // Type predicate function
    "has-content",              // Target classes to add
    "localImageForTarget",      // Pop-frame fill function
    "image object"              // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Used in: Extracts.isLocalImageLink
    imageFileExtensions: [ "bmp", "gif", "ico", "jpeg", "jpg", "png", "svg" ],

    //  Used in: Extracts.localImageForTarget
    imageMaxWidth: 634.0,
    imageMaxHeight: 453.0,

    //  Called by: extracts.js (as `predicateFunctionName`)
    isLocalImageLink: (target) => {
        if (target.hostname != location.hostname)
            return false;

        let imageFileURLRegExp = new RegExp(
              '('
            + Extracts.imageFileExtensions.map(ext => `\\.${ext}`).join("|")
            + ')$'
        , 'i');
        return (target.pathname.match(imageFileURLRegExp) != null);
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    localImageForTarget: (target) => {
        GWLog("Extracts.localImageForTarget", "extracts-content.js", 2);

        let width = target.dataset.imageWidth ?? 0;
        let height = target.dataset.imageHeight ?? 0;

		//	Constrain dimensions, shrinking proportionally.
        if (width > Extracts.imageMaxWidth) {
            height *= Extracts.imageMaxWidth / width;
            width = Extracts.imageMaxWidth;
        }
        if (height > Extracts.imageMaxHeight) {
            width *= Extracts.imageMaxHeight / height;
            height = Extracts.imageMaxHeight;
        }

		//	Specify dimensions in HTML and CSS.
        let styles = ``;
        if (   width > 0
            && height > 0)
            styles = `width="${(target.dataset.imageWidth)}" `
            	   + `height="${(target.dataset.imageHeight)}" `
            	   + `style="width: ${width}px; height: ${height}px; aspect-ratio: ${width} / ${height}"`;

		//	Special handling for SVGs.
		if (target.pathname.endsWith(".svg"))
			styles = `style="width: 100%; height: 100%"`;

        /*  Note that we pass in the original image-link’s classes; this is 
        	good for classes like ‘invert’.
         */
        return newDocument(`<figure><img
                                ${styles}
                                class="${target.classList}"
                                src="${target.href}"
                                loading="eager"
                                decoding="sync"
                                    ></figure>`);
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_LOCAL_IMAGE: (popup) => {
        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    },

    //  Called by: Extracts.rewritePopinContent_LOCAL_IMAGE
    //  Called by: Extracts.rewritePopupContent_LOCAL_IMAGE
    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_IMAGE: (popFrame) => {
        //  Remove extraneous classes from images in image pop-frames.
        popFrame.document.querySelector("img").classList.remove("link-page", "link-self",
            "has-annotation", "has-annotation-partial", "has-content");

		//	Loading spinner.
		Extracts.setLoadingSpinner(popFrame);

		//	We don’t need the full content inject handling, just ImageFocus.
		ImageFocus.processImagesWithin(popFrame.body);
    },

    //  Called by: extracts.js (as `rewritePopupContent_${targetTypeName}`)
    rewritePopinContent_LOCAL_IMAGE: (popin) => {
        Extracts.rewritePopFrameContent_LOCAL_IMAGE(popin);

        //  Remove extraneous classes from images in image popins.
        popin.document.querySelector("img").classList.remove("spawns-popin");
    },

    //  Called by: extracts.js (as `rewritePopinContent_${targetTypeName}`)
    rewritePopupContent_LOCAL_IMAGE: (popup) => {
        Extracts.rewritePopFrameContent_LOCAL_IMAGE(popup);

        //  Remove extraneous classes from images in image popups.
        popup.document.querySelector("img").classList.remove("spawns-popup");

        if (popup.document.querySelector("img[width][height]"))
        	Extracts.popFrameProvider.addClassesToPopFrame(popup, "dimensions-specified");
    },
};

/*=--------------------------=*/
/*= LOCALLY HOSTED DOCUMENTS =*/
/*=--------------------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "LOCAL_DOCUMENT",               // Type name
    "isLocalDocumentLink",          // Type predicate function
    "has-content",                  // Target classes to add
    "localDocumentForTarget",       // Pop-frame fill function
    "local-document object"         // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    isLocalDocumentLink: (target) => {
        if (target.hostname != location.hostname)
            return false;

        return (   target.pathname.startsWith("/doc/www/")
                || (   target.pathname.startsWith("/doc/")
                    && target.pathname.match(/\.(html|pdf)$/i) != null));
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    localDocumentForTarget: (target) => {
        GWLog("Extracts.localDocumentForTarget", "extracts-content.js", 2);

        return newDocument(Extracts.objectHTMLForURL(target,
            `sandbox="allow-same-origin" referrerpolicy="same-origin"`));
    },

    /*  This “special testing function” is used to exclude certain targets which
        have already been categorized as (in this case) `LOCAL_DOCUMENT`
        targets. It returns false if the target is to be excluded, true
        otherwise. Excluded targets will not spawn pop-frames.
     */
    //  Called by: extracts.js (as `testTarget_${targetTypeInfo.typeName}`)
    testTarget_LOCAL_DOCUMENT: (target) => {
    	/*	Mobile browsers have no in-browser PDF viewer, so a popin would be
    		pointless, since the file will download anyway.
    	 */
    	if (   Extracts.popFrameProvider == Popins
            && target.href.match(/\.pdf(#|$)/) != null)
            return false;

        return true;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_DOCUMENT: (popFrame) => {
        let iframe = popFrame.document.querySelector("iframe");
        if (iframe) {
        	/*	All of this `srcURL` stuff is necessary as a workaround for a 
        		Chrome bug that scrolls the parent page when an iframe popup
        		has a `src` attribute with a hash and that hash points to an
        		old-style anchor (`<a name="foo">`).
        	 */
			let srcURL = URLFromString(iframe.src);
			if (   srcURL.pathname.endsWith(".html")
				&& srcURL.hash > "") {
				srcURL.savedHash = srcURL.hash;
				srcURL.hash = "";
				iframe.src = srcURL.href;
			}

            iframe.addEventListener("load", (event) => {
				if (srcURL.savedHash) {
					let selector = selectorFromHash(srcURL.savedHash);
					let element = iframe.contentDocument.querySelector(`${selector}, [name='${(selector.slice(1))}']`);
					if (element)
						iframe.contentWindow.scrollTo(0, element.getBoundingClientRect().y);
				}

				//  Set title of popup from page title.
				Extracts.updatePopFrameTitle(popFrame, iframe.contentDocument.title);
            });
        }

        //  Loading spinner.
        Extracts.setLoadingSpinner(popFrame);
    }
};

/*=---------------------------=*/
/*= LOCALLY HOSTED CODE FILES =*/
/*=---------------------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "LOCAL_CODE_FILE",              // Type name
    "isLocalCodeFileLink",          // Type predicate function
    "has-content",                  // Target classes to add
    "localCodeFileForTarget",       // Pop-frame fill function
    "local-code-file"               // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    isLocalCodeFileLink: (target) => {
    	return Content.contentTypes.localCodeFile.matches(target);
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    localCodeFileForTarget: (target) => {
        GWLog("Extracts.localCodeFileForTarget", "extracts-content.js", 2);

        return newDocument(synthesizeIncludeLink(target));
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_CODE_FILE: (popFrame, injectEventInfo = null) => {
        let target = popFrame.spawningTarget;

		if (injectEventInfo == null) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopFrameContent_LOCAL_CODE_FILE(popFrame, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popFrame.body, {
				source: "Extracts.rewritePopFrameContent_LOCAL_CODE_FILE",
				container: popFrame.body,
				document: popFrame.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE
    },
};

/*=----------------=*/
/*= OTHER WEBSITES =*/
/*=----------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "FOREIGN_SITE",             // Type name
    "isForeignSiteLink",        // Type predicate function
    "has-content",              // Target classes to add
    "foreignSiteForTarget",     // Pop-frame fill function
    "foreign-site object"       // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    isForeignSiteLink: (target) => {
        if (target.hostname == location.hostname)
            return false;

        return target.classList.contains("link-live");
    },

    //  Used in: Extracts.foreignSiteForTarget
    foreignSiteEmbedURLTransforms: [
        //	Wikimedia commons
        [	(url) => (   url.hostname == "commons.wikimedia.org" 
        			  && url.pathname.startsWith("/wiki/File:")),
        	(url) => {
        		url.hostname = "api.wikimedia.org";
        		url.pathname = "/core/v1/commons/file/" + url.pathname.match(/\/(File:.+)$/)[1];
        	},
        	(url, target) => {
				doAjax({
					location: url.href,
					responseType: "json",
					onSuccess: (event) => {
						if (Extracts.popFrameProvider.isSpawned(target.popFrame) == false)
							return;

						Extracts.popFrameProvider.setPopFrameContent(target.popFrame, 
							newDocument(Extracts.objectHTMLForURL(event.target.response.original.url, "sandbox")));
						Extracts.setLoadingSpinner(target.popFrame);
					},
					onFailure: (event) => {
						Extracts.postRefreshUpdatePopFrameForTarget(target, false);
					}
				});

				return newDocument();
			} ]
    ],

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    foreignSiteForTarget: (target) => {
        GWLog("Extracts.foreignSiteForTarget", "extracts-content.js", 2);

		let url = URLFromString(target.dataset.urlHtml ?? target.href);

        //  WARNING: EXPERIMENTAL FEATURE!
        if (localStorage.getItem("enable-embed-proxy") == "true") {
            let proxyURL = URLFromString("https://api.obormot.net/embed.php");

            doAjax({
                location: proxyURL.href,
                params: { url: url.href },
                onSuccess: (event) => {
                    if (Extracts.popFrameProvider.isSpawned(target.popFrame) == false)
                        return;

                    let doc = newElement("DIV", null, { "innerHTML": event.target.responseText });
                    doc.querySelectorAll("[href], [src]").forEach(element => {
                        if (element.href) {
                            let elementURL = URLFromString(element.href);
                            if (   elementURL.host == location.host
                                && !element.getAttribute("href").startsWith("#")) {
                                elementURL.host = url.host;
                                element.href = elementURL.href;
                            }
                        } else if (element.src) {
                            let elementURL = URLFromString(element.src);
                            if (elementURL.host == location.host) {
                                elementURL.host = url.host;
                                element.src = elementURL.href;
                            }
                        }
                    });

                    if (event.target.getResponseHeader("content-type").startsWith("text/plain"))
                        doc.innerHTML = `<pre>${doc.innerHTML}</pre>`;

                    target.popFrame.document.querySelector("iframe").srcdoc = doc.innerHTML;

                    Extracts.postRefreshUpdatePopFrameForTarget(target, true);
                },
                onFailure: (event) => {
                    if (Extracts.popFrameProvider.isSpawned(target.popFrame) == false)
                        return;

                    Extracts.postRefreshUpdatePopFrameForTarget(target, false);
                }
            });

            return newDocument(`<iframe frameborder="0" sandbox="allow-scripts allow-popups"></iframe>`);
        }
        //  END EXPERIMENTAL SECTION

		/*	If a special ‘HTML’ URL is specified, use that, sans transformation.
        	Otherwise, transform URL for embedding.
         */
        if (target.dataset.urlHtml == null) {
			for ([ test, transform, special ] of Extracts.foreignSiteEmbedURLTransforms) {
				if (test(url)) {
					if (transform) {
						transform(url);
					}
					if (special) {
						let retval = special(url, target);
						if (retval)
							return retval;
					}
					break;
				}
			}
		}

        return newDocument(Extracts.objectHTMLForURL(url, "sandbox"));
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_FOREIGN_SITE: (popFrame) => {
        //  Loading spinner.
        Extracts.setLoadingSpinner(popFrame);
    }
};

/*=------------------=*/
/*= CONTENT: HELPERS =*/
/*=------------------=*/

Extracts = { ...Extracts,
    //  Called by: Extracts.videoForTarget
    //  Called by: Extracts.localDocumentForTarget
    //  Called by: Extracts.foreignSiteForTarget
    objectHTMLForURL: (url, additionalAttributes = null) => {
		if (typeof url == "string")
			url = URLFromString(url);

        if (url.href.match(/\.pdf(#|$)/) != null) {
            let data = url.href + (url.hash ? "&" : "#") + "view=FitH";
            return `<object
                        data="${data}"
                            ></object>`;
        } else {
            return `<iframe
                        src="${url.href}"
                        frameborder="0"
                        ${(additionalAttributes ? (" " + additionalAttributes) : "")}
                            ></iframe>`;
        }
    },

	//	Used in: Extracts.setUpContentLoadEventsWithin
	contentLoadHoverDelay: 25,

    //  Called by: extracts.js
    setUpContentLoadEventsWithin: (container) => {
        GWLog("Extracts.setUpContentLoadEventsWithin", "extracts.js", 1);

        /*  Get all targets in the container that use Content as a data loading
        	provider. (Currently that is local page links, local fragment links,
        	and local code file links.)
         */
        let allTargetsInContainer = Array.from(container.querySelectorAll("a[class*='has-content']")).filter(link =>
        	Content.contentTypeForLink(link) != null
        );

        if (Extracts.popFrameProvider == Popups) {
            //  Add hover event listeners to all the chosen targets.
            allTargetsInContainer.forEach(target => {
                target.removeContentLoadEvents = onEventAfterDelayDo(target, "mouseenter", Extracts.contentLoadHoverDelay, (event) => {
                    //  Do nothing if the content is already loaded.
                    if (Content.cachedDataExists(target) == false)
                        Content.load(target);
                }, "mouseleave");
            });

			if (allTargetsInContainer.length > 0) {
				/*  Set up handler to remove hover event listeners from all
					the chosen targets in the document.
					*/
				GW.notificationCenter.addHandlerForEvent("Extracts.cleanupDidComplete", (info) => {
					allTargetsInContainer.forEach(target => {
						if (target.removeContentLoadEvents) {
							target.removeContentLoadEvents();
							target.removeContentLoadEvents = null;
						}
					});
				}, { once: true });
            }
        } else { // if (Extracts.popFrameProvider == Popins)
            //  Add click event listeners to all the chosen targets.
            allTargetsInContainer.forEach(target => {
                target.addEventListener("click", target.contentLoad_click = (event) => {
                    //  Do nothing if the content is already loaded.
                    if (Content.cachedDataExists(target) == false)
                        Content.load(target);
                });
            });

            /*  Set up handler to remove click event listeners from all
                the annotated targets in the document.
                */
            GW.notificationCenter.addHandlerForEvent("Extracts.cleanupDidComplete", (info) => {
                allTargetsInContainer.forEach(target => {
                    target.removeEventListener("click", target.contentLoad_click);
                });
            }, { once: true });
        }
    },
};
Extracts = { ...Extracts, 
	/*****************/
	/*	Configuration.
	 */
	modeOptions: [
		[ "on", "On", `Enable link pop-frames.`, "message-lines-solid" ],
		[ "off", "Off", `Disable link pop-frames.`, "message-slash-solid" ],
	],

	selectedModeOptionNote: " [This option is currently selected.]",

	popFramesDisableDespawnDelay: 2000,
	popFramesDisableAutoToggleDelay: 250,

	/******************/
	/*	Infrastructure.
	 */

	modeSelector: null,
	modeSelectorInteractable: true,

	/*************/
	/*	Functions.
	 */

	/******************/
	/*	Mode selection.
	 */

	//	Called by: Extracts.injectModeSelector
	modeSelectorHTML: (inline = false) => {
		//	Get saved mode setting (or default).
		let currentMode = Extracts.extractPopFramesEnabled() ? "on" : "off";

		let modeSelectorInnerHTML = Extracts.modeOptions.map(modeOption => {
			let [ name, label, desc, icon ] = modeOption;
			let selected = (name == currentMode ? " selected" : " selectable");
			let disabled = (name == currentMode ? " disabled" : "");
			desc = desc.replace("pop-frame", Extracts.popFrameTypeText());
			if (name == currentMode)
				desc += Extracts.selectedModeOptionNote;
			return `<button
						type="button"
						class="select-mode-${name}${selected}"
						${disabled}
						tabindex="-1"
						data-name="${name}"
						title="${desc}"
							>`
						+ `<span class="icon">${(GW.svg(icon))}</span>`
						+ `<span class="label">${label}</span>`
					 + `</button>`;
		  }).join("");

		let selectorTag = (inline ? "span" : "div");
		let selectorId = (inline ? "" : "extracts-mode-selector");
		let selectorClass = ("extracts-mode-selector mode-selector" + (inline ? " mode-selector-inline" : ""));

		return `<${selectorTag} id="${selectorId}" class="${selectorClass}">${modeSelectorInnerHTML}</${selectorTag}>`;
	},

	modeSelectButtonClicked: (event) => {
		GWLog("Extracts.modeSelectButtonClicked", "extracts-options.js", 2);

		let button = event.target.closest("button");

		// Determine which setting was chosen (ie. which button was clicked).
		let selectedMode = button.dataset.name;

		/*	We don’t want clicks to go through if the transition 
			between modes has not completed yet, so we disable the 
			button temporarily while we’re transitioning between 
			modes.
		 */
		doIfAllowed(() => {
			if (selectedMode == "on")
				Extracts.enableExtractPopFrames();
			else
				Extracts.disableExtractPopFrames();
		}, Extracts, "modeSelectorInteractable");
	},

	//	Called by: Extracts.setup (extracts.js)
	injectModeSelector: (replacedElement = null) => {
		GWLog("Extracts.injectModeSelector", "extracts-options.js", 1);

		//	Inject the mode selector widget.
		let modeSelector;
		if (replacedElement) {
			modeSelector = elementFromHTML(Extracts.modeSelectorHTML(true));
			replacedElement.replaceWith(modeSelector);
		} else {
			modeSelector = Extracts.modeSelector = GW.pageToolbar.addWidget(Extracts.modeSelectorHTML());
		}

		//	Activate mode selector widget buttons.
		modeSelector.querySelectorAll("button").forEach(button => {
			button.addActivateEvent(Extracts.modeSelectButtonClicked);
		});

		//	Register event handler to update mode selector state.
		GW.notificationCenter.addHandlerForEvent("Extracts.didSetMode", (info) => {
			Extracts.updateModeSelectorState(modeSelector);
		});
	},

	//	Called by: Extracts.didSetMode event handler
	updateModeSelectorState: (modeSelector = Extracts.modeSelector) => {
		GWLog("Extracts.updateModeSelectorState", "extracts-options.js", 2);

		/*	If the mode selector has not yet been injected, then do nothing.
		 */
		if (modeSelector == null)
			return;

		//	Get saved mode setting (or default).
		let currentMode = Extracts.extractPopFramesEnabled() ? "on" : "off";

		//	Clear current buttons state.
		modeSelector.querySelectorAll("button").forEach(button => {
			button.classList.remove("active");
			button.swapClasses([ "selectable", "selected" ], 0);
			button.disabled = false;
			if (button.title.endsWith(Extracts.selectedModeOptionNote))
				button.title = button.title.slice(0, (-1 * Extracts.selectedModeOptionNote.length));
		});

		//	Set the correct button to be selected.
		modeSelector.querySelectorAll(`.select-mode-${currentMode}`).forEach(button => {
			button.swapClasses([ "selectable", "selected" ], 1);
			button.disabled = true;
			button.title += Extracts.selectedModeOptionNote;
		});
	},

	//	Called by: extracts.js
	disableExtractPopFramesPopFrameTitleBarButton: () => {
		let button = Extracts.popFrameProvider.titleBarComponents.genericButton();

		button.title = `Disable link ${(Extracts.popFrameTypeText())}s [currently enabled]`;
		button.innerHTML = GW.svg("message-lines-regular");
		button.classList.add("extracts-disable-button");

		button.addActivateEvent((event) => {
			event.stopPropagation();

			button.innerHTML = GW.svg("message-slash-regular");
			button.classList.add("disabled");

			GW.pageToolbar.toggleCollapseState(false);

			setTimeout(() => {
				Extracts.popFrameProvider.cleanup();

				setTimeout(() => {
					GW.pageToolbar.flashWidget("extracts-mode-selector");
					setTimeout(() => {
						Extracts.disableExtractPopFrames();

						GW.pageToolbar.toggleCollapseState(true, true, GW.pageToolbar.demoCollapseDelay + GW.pageToolbar.widgetFlashStayDuration);
					}, GW.pageToolbar.widgetFlashRiseDuration);
				}, Extracts.popFramesDisableAutoToggleDelay);
			}, Extracts.popFramesDisableDespawnDelay);
		});

		return button;
	},

	extractPopFramesDisabledLocalStorageItemKey: () => {
		return (Extracts.popFrameProvider == Popups
				? Extracts.popupsDisabledLocalStorageItemKey
				: Extracts.popinsDisabledLocalStorageItemKey);
	},

	extractPopFramesEnabled: () => {
		return (localStorage.getItem(Extracts.extractPopFramesDisabledLocalStorageItemKey()) != "true");
	},

	disableExtractPopFrames: () => {
		GWLog("Extracts.disableExtractPopFrames", "extracts-options.js", 1);

		//	Save setting.
		localStorage.setItem(Extracts.extractPopFramesDisabledLocalStorageItemKey(), "true");

		//	Fire event.
		GW.notificationCenter.fireEvent("Extracts.didSetMode");

		//	Run cleanup.
		Extracts.cleanup();
	},

	enableExtractPopFrames: () => {
		GWLog("Extracts.enableExtractPopFrames", "extracts-options.js", 1);

		//	Clear saved setting.
		localStorage.removeItem(Extracts.extractPopFramesDisabledLocalStorageItemKey());

		//	Fire event.
		GW.notificationCenter.fireEvent("Extracts.didSetMode");

		//  Run setup.
		Extracts.setup();

		/*  Since the main document has already loaded, we must trigger the
			processing of targets manually.
		 */
		Extracts.processTargetsInContainer(document.body);
	},
};
/*	This file should be loaded after all other extracts*.js files.
 */

GW.notificationCenter.fireEvent("Extracts.didLoad");

Extracts.setup();
/*	Typography.js
	(Copyright 2020 Said Achmiz)
	MIT License

	is based on

	https://github.com/kellym/smartquotes.js
	(Copyright 2013 Kelly Martin)
	MIT License

	and

	https://github.com/kronusaturn/lw2-viewer
	(Copyright 2018 kronusaturn)
	MIT License
	*/

Typography = {
	replacements: (types) => {
		let specifiedReplacements = [ ];
		let replacementTypeDefinitions = [
			[ Typography.replacementTypes.QUOTES,		Typography.replacementDefinitionGroups.quotes		],
			[ Typography.replacementTypes.HYPHENS,		Typography.replacementDefinitionGroups.hyphens		],
			[ Typography.replacementTypes.ELLIPSES,		Typography.replacementDefinitionGroups.ellipses		],
			[ Typography.replacementTypes.ARROWS,		Typography.replacementDefinitionGroups.arrows		],
			[ Typography.replacementTypes.WORDBREAKS,	Typography.replacementDefinitionGroups.wordbreaks	],
			[ Typography.replacementTypes.MISC,			Typography.replacementDefinitionGroups.misc			],
			[ Typography.replacementTypes.SOFTHYPHENS,	Typography.replacementDefinitionGroups.softHyphens	],
			[ Typography.replacementTypes.JOINERS,		Typography.replacementDefinitionGroups.joiners		],
			[ Typography.replacementTypes.SEPARATORS,	Typography.replacementDefinitionGroups.separators	]
		];
		for ([ replacementTypeCode, replacementGroup ] of replacementTypeDefinitions) {
			if (types & replacementTypeCode)
				for (replacement of replacementGroup)
					specifiedReplacements.push(replacement);
		}
		return specifiedReplacements;
	},
	replacementTypes: {
		NONE:			0x0000,
		QUOTES:			0x0001,
		HYPHENS:		0x0002,
		ELLIPSES:		0x0004,
		ARROWS:			0x0008,
		WORDBREAKS:		0x0010,
		MISC:			0x0020,
		SOFTHYPHENS:	0x0040,
		JOINERS:		0x0080,
		SEPARATORS:		0x0100,
		CLEAN: 			(0x0040 + 0x0080 + 0x0100)
	},
	replacementDefinitionGroups: {
		quotes: [
			// triple prime
			[ /'''/, '\u2034' ],
			// beginning "
			[ /(?<=[\s([]|^)"(?=[^\s?!.,;\/)])/, '\u201c' ],
			// ending "
			[ /(?<=\u201c[^"]*)"(?=[^"]*$|[^\u201c"]*(?=\u201c))/, '\u201d' ],
			// remaining " at end of word
			[ /(?<=[^0-9])"/, '\u201d' ],
			// double quotes
			[ /"(.+?)"/, '\u201c$1\u201d' ],
			// double prime as two single quotes
			[ /''/, '\u2033' ],
			// beginning '
			[ /(?<=\W|^)'(?=\S)/, '\u2018' ],
			// conjunction's possession
			[ /(?<=[a-z0-9])'(?=[a-z])/i, '\u2019' ],
			// abbrev. years like '93
			[ /\u2018(?=(?:[0-9]{2}[^\u2019]*)(?:\u2018(?:[^0-9]|$)|$|\u2019[a-z]))/i, '\u2019' ],
			// ending '
			[ /(?<=(\u2018[^']*)|[a-z])'(?=[^0-9]|$)/i, '\u2019' ],
			// backwards apostrophe
			[ /(?<=\B|^)\u2018(?=([^\u2018\u2019]*\u2019\b)*([^\u2018\u2019]*\B\W[\u2018\u2019]\b|[^\u2018\u2019]*$))/i, '\u2019' ],
			// double prime
			[ /"/, '\u2033' ],
			// prime
			[ /'/, '\u2032' ]
		],
		hyphens: [
			// turn a hyphen surrounded by spaces, between words, into an em-dash
			[ /(?<=[a-z\u201d]) (-) (?=[a-z\u201c])/i, '\u2014' ],
			// turn a hyphen between a space and a quote, into an em-dash
			[ /(?<=[a-z]) (-)(?=\u201d)/i, '\u2014' ],
			[ /(?<=\u201c)(-) (?=[a-z])/i, '\u2014' ],
			// turn a double or triple hyphen, optionally surrounded by spaces, between words, or at the start of a line, into an em-dash
			[ /(?<=[a-z"'“”‘’]|\n) ?(---?) ?(?=[a-z"'“”‘’])/i, '\u2014' ],
			// turn a hyphen surrounded by spaces, between decimal digits, into an en-dash
			[ /(?<=[0-9]) (-) (?=[0-9])/, '\u2013' ]
		],
		ellipses: [
			// Ellipsis rectification.
			[ /(?<=^|\s)\.\.\./, '…' ],
			[ /\.\.\.(?=\s|$)/, '…' ]
		],
		arrows: [
			// Arrows
			[ /(?<=\s)->(?=\s)/, '\u2192' ],
			[ /(?<=\s)<-(?=\s)/, '\u2190' ],
			[ /(?<=\s)=>(?=\s)/, '\u21d2' ],
			[ /(?<=\s)<=(?=\s)/, '\u21d0' ],
			[ /(?<=\s)<=>(?=\s)/, '\u21d4' ]
		],
		wordbreaks: [
			// Word-breaks after slashes (for long URLs etc.).
			[ /(?<=.)\/+(?!\u200b)/, '$&\u200b' ],
		],
		misc: [
			// Convert nbsp to regular space.
			[ /\xa0/, ' ' ],
			// Two spaces after a period is INCORRECT.
			[ /(?<=\w[\.\?\!])[ \u00a0]{2}(?=\w)/, ' ' ],
			// Hyphen followed by a numeral (with an optional space first), becomes an actual minus sign.
			[ /(?<=\s)-( ?)(?=[0-9])/, '\u2212$1' ]
		],
		softHyphens: [
			// Strip soft hyphens.
			[ /\u00ad/, '' ]
		],
		joiners: [
			// Strip joiners.
			[ /\u2060/, '' ]
		],
		separators: [
			// Strip zero-width spaces.
			[ /\u200b|&ZeroWidthSpace;/, '' ],
			// Strip hair spaces.
			[ /\u200a|&hairsp;/, '' ],
		]
	},
	processString: (str, replacementTypes = Typography.replacementTypes.NONE, segments = null) => {
		if (segments == null)
			segments = [ str.length ];

		function segmentIndexAtOffset(segments, offset) {
			let currentSegmentStart = 0;
			for (let i = 0; i < segments.length; i++) {
				if (   offset >= currentSegmentStart
					&& offset < currentSegmentStart + segments[i])
					return i;

				currentSegmentStart += segments[i];
			}
			return -1;
		}

		Typography.replacements(replacementTypes).forEach(replacement => {
			let [ pattern, template ] = replacement;

			let globalPattern = new RegExp(pattern.source, pattern.flags + "g");
			let match = null;
			while (match = globalPattern.exec(str)) {
				let newStr = str.replace(pattern, template);
				let lengthChange = newStr.length - str.length;

				if (lengthChange == 0)
					continue;

				let segmentAtMatchStart = segmentIndexAtOffset(segments, match.index);
				let segmentAtMatchEnd = segmentIndexAtOffset(segments, match.index + match[0].length - 1);
				if (segmentAtMatchStart == segmentAtMatchEnd) {
					segments[segmentAtMatchStart] += lengthChange;
				} else {
					//	TODO: THIS!
				}

				str = newStr;
			}
		});

		return str;
	},
	excludedTags: [ 'CODE', 'PRE', 'SCRIPT', 'STYLE', 'NOSCRIPT' ],
	processElement: (element, replacementTypes = Typography.replacementTypes.NONE, rectifyWordBreaks = true) => {
		if (Typography.excludedTags.includes(element.nodeName))
			return null;

		function decomposeElement(element) {
			let text = "";
			let textNodes = [ ];

			if (Typography.excludedTags.includes(element.nodeName))
				return [ text, textNodes ];

			for (node of element.childNodes) {
				if (node.nodeType === Node.TEXT_NODE) {
					textNodes.push(node);
					text += node.nodeValue;
				} else if (node.childNodes.length > 0) {
					let [ subtext, subnodes ] = decomposeElement(node);
					text += subtext;
					textNodes.splice(textNodes.length, 0, ...subnodes);
				}
			}

			return [ text, textNodes ];
		}

		let [ text, textNodes ] = decomposeElement(element);
		let segments = textNodes.map(node => node.nodeValue.length);
		text = Typography.processString(text, replacementTypes, segments);
		let currentSegmentStart = 0;
		for (let i = 0; i < textNodes.length; i++) {
			textNodes[i].nodeValue = text.slice(currentSegmentStart, currentSegmentStart + segments[i]);
			currentSegmentStart += segments[i];
		}

		//  Transform separators into <wbr> tags.
		if (rectifyWordBreaks)
			Typography.rectifyWordBreaks(element);

		return text;
	},
	rectifyWordBreaks: (element) => {
		let replacements = [ ];
		for (node of element.childNodes) {
			if (node.nodeType === Node.ELEMENT_NODE) {
				Typography.rectifyWordBreaks(node);
			} else if (node.nodeType === Node.TEXT_NODE) {
				let sepRegExp = new RegExp(Typography.replacementDefinitionGroups.separators.map(x => x[0].source).join("|"), "g");
				let parts = [ ];
				let start = 0;
				let match = null;
				while (match = sepRegExp.exec(node.textContent)) {
					parts.push([ start, match.index ]);
					start = match.index + match[0].length;
				}
				if (parts.length > 0) {
					let replacementNodes = [ ];
					parts.forEach(part => {
						if (part[1] > part[0])
							replacementNodes.push(document.createTextNode(node.textContent.slice(...part)));
						replacementNodes.push(newElement("WBR"));
					});
					replacementNodes.push(document.createTextNode(node.textContent.slice(start)));
					replacements.push([ node, replacementNodes ]);
				}
			}
		}
		if (replacements.length > 0) {
			//	Replace.
			replacements.forEach(replacement => {
				let [ replacedNode, replacementNodes ] = replacement;
				replacedNode.parentNode.replaceChild(newDocument(replacementNodes), replacedNode);
			});

			//	Remove all but one of each set of consecutive <wbr> tags.
			function isWBR(node) {
				return (   node.nodeType === Node.ELEMENT_NODE
						&& node.tagName == "WBR");
			}

			function isEmptyTextNode(node) {
				return (   node.nodeType === Node.TEXT_NODE
						&& isNodeEmpty(node) == true);
			}

			let prevNodeIsWBR = false;
			for (let i = 0; i < element.childNodes.length; i++) {
				let node = element.childNodes[i];
				if (isWBR(node) && prevNodeIsWBR == false) {
					prevNodeIsWBR = true;
				} else if (prevNodeIsWBR) {
					if (   isWBR(node) 
						|| isEmptyTextNode(node)) {
						node.remove();
						i--;
					} else {
						prevNodeIsWBR = false;
					}
				}
			}
		}
	}
};
/**
 * @license Hyphenopoly_Loader 5.0.0-beta.4 - client side hyphenation
 * ©2022  Mathias Nater, Güttingen (mathiasnater at gmail dot com)
 * https://github.com/mnater/Hyphenopoly
 *
 * Released under the MIT license
 * https://github.com/mnater/Hyphenopoly/blob/master/LICENSE
 */
/* globals Hyphenopoly:readonly */
window.Hyphenopoly = {};

((w, d, H, o) => {
    "use strict";

    /**
     * Shortcut for new Map
     * @param {any} init - initialiser for new Map
     * @returns {Map}
     */
    const mp = (init) => {
        return new Map(init);
    };

    const scriptName = "Hyphenopoly_Loader.js";
    const thisScript = d.currentScript.src;
    const store = sessionStorage;
    let mainScriptLoaded = false;

    /**
     * The main function runs the feature test and loads Hyphenopoly if
     * necessary.
     */
    const main = (() => {
        const shortcuts = {
            "ac": "appendChild",
            "ce": "createElement",
            "ct": "createTextNode"
        };

        /**
         * Create deferred Promise
         *
         * From http://lea.verou.me/2016/12/resolve-promises-externally-with-
         * this-one-weird-trick/
         * @return {promise}
         */
        const defProm = () => {
            let res = null;
            let rej = null;
            const promise = new Promise((resolve, reject) => {
                res = resolve;
                rej = reject;
            });
            promise.resolve = res;
            promise.reject = rej;
            return promise;
        };

        let stylesNode = null;

        /**
         * Define function H.hide.
         * This function hides (state = 1) or unhides (state = 0)
         * the whole document (mode == 0) or
         * each selected element (mode == 1) or
         * text of each selected element (mode == 2) or
         * nothing (mode == -1)
         * @param {integer} state - State
         * @param {integer} mode  - Mode
         */
        H.hide = (state, mode) => {
            if (state === 0) {
                if (stylesNode) {
                    stylesNode.remove();
                }
            } else {
                let vis = "{visibility:hidden!important}";
                stylesNode = d[shortcuts.ce]("style");
                let myStyle = "";
                if (mode === 0) {
                    myStyle = "html" + vis;
                } else if (mode !== -1) {
                    if (mode === 2) {
                        vis = "{color:transparent!important}";
                    }
                    o.keys(H.s.selectors).forEach((sel) => {
                        myStyle += sel + vis;
                    });
                }
                stylesNode[shortcuts.ac](d[shortcuts.ct](myStyle));
                d.head[shortcuts.ac](stylesNode);
            }
        };

        const tester = (() => {
            let fakeBody = null;
            return {

                /**
                 * Append fakeBody with tests to document
                 * @returns {Object|null} The body element or null, if no tests
                 */
                "ap": () => {
                    if (fakeBody) {
                        d.documentElement[shortcuts.ac](fakeBody);
                        return fakeBody;
                    }
                    return null;
                },

                /**
                 * Remove fakeBody
                 * @returns {undefined}
                 */
                "cl": () => {
                    if (fakeBody) {
                        fakeBody.remove();
                    }
                },

                /**
                 * Create and append div with CSS-hyphenated word
                 * @param {string} lang Language
                 * @returns {undefined}
                 */
                "cr": (lang) => {
                    if (H.cf.langs.has(lang)) {
                        return;
                    }
                    fakeBody = fakeBody || d[shortcuts.ce]("body");
                    const testDiv = d[shortcuts.ce]("div");
                    const ha = "hyphens:auto";
                    testDiv.lang = lang;
                    testDiv.style.cssText = `visibility:hidden;-webkit-${ha};-ms-${ha};${ha};width:48px;font-size:12px;line-height:12px;border:none;padding:0;word-wrap:normal`;
                    testDiv[shortcuts.ac](
                        d[shortcuts.ct](H.lrq.get(lang).wo.toLowerCase())
                    );
                    fakeBody[shortcuts.ac](testDiv);
                }
            };
        })();

        /**
         * Checks if hyphens (ev.prefixed) is set to auto for the element.
         * @param {Object} elm - the element
         * @returns {Boolean} result of the check
         */
        const checkCSSHyphensSupport = (elmStyle) => {
            const h = elmStyle.hyphens ||
                elmStyle.webkitHyphens ||
                elmStyle.msHyphens;
            return (h === "auto");
        };

        H.res = {
            "he": mp()
        };

        /**
         * Load hyphenEngines to H.res.he
         *
         * Make sure each .wasm is loaded exactly once, even for fallbacks
         * Store a list of languages to by hyphenated with each .wasm
         * @param {string} lang The language
         * @returns {undefined}
         */
        const loadhyphenEngine = (lang) => {
            const fn = H.lrq.get(lang).fn;
            H.cf.pf = true;
            H.cf.langs.set(lang, "H9Y");
            if (H.res.he.has(fn)) {
                H.res.he.get(fn).l.push(lang);
            } else {
                H.res.he.set(
                    fn,
                    {
                        "l": [lang],
                        "w": w.fetch(H.paths.patterndir + fn + ".wasm", {"credentials": H.s.CORScredentials})
                    }
                );
            }
        };
        H.lrq.forEach((value, lang) => {
            if (value.wo === "FORCEHYPHENOPOLY" || H.cf.langs.get(lang) === "H9Y") {
                loadhyphenEngine(lang);
            } else {
                tester.cr(lang);
            }
        });
        const testContainer = tester.ap();
        if (testContainer) {
            testContainer.querySelectorAll("div").forEach((n) => {
                if (checkCSSHyphensSupport(n.style) && n.offsetHeight > 12) {
                    H.cf.langs.set(n.lang, "CSS");
                } else {
                    loadhyphenEngine(n.lang);
                }
            });
            tester.cl();
        }
        const hev = H.hev;
        if (H.cf.pf) {
            H.res.DOM = new Promise((res) => {
                if (d.readyState === "loading") {
                    d.addEventListener(
                        "DOMContentLoaded",
                        res,
                        {
                            "once": true,
                            "passive": true
                        }
                    );
                } else {
                    res();
                }
            });
            H.hide(1, H.s.hide);
            H.timeOutHandler = w.setTimeout(() => {
                H.hide(0, null);
                // eslint-disable-next-line no-console
                console.info(scriptName + " timed out.");
            }, H.s.timeout);
            if (mainScriptLoaded) {
                H.main();
            } else {
                // Load main script
                const script = d[shortcuts.ce]("script");
                script.src = H.paths.maindir + "Hyphenopoly.js";
                d.head[shortcuts.ac](script);
                mainScriptLoaded = true;
            }
            H.hy6ors = mp();
            H.cf.langs.forEach((langDef, lang) => {
                if (langDef === "H9Y") {
                    H.hy6ors.set(lang, defProm());
                }
            });
            H.hy6ors.set("HTML", defProm());
            H.hyphenators = new Proxy(H.hy6ors, {
                "get": (target, key) => {
                    return target.get(key);
                },
                "set": () => {
                    // Inhibit setting of hyphenators
                    return true;
                }
            });
            (() => {
                if (hev && hev.polyfill) {
                    hev.polyfill();
                }
            })();
        } else {
            (() => {
                if (hev && hev.tearDown) {
                    hev.tearDown();
                }
                w.Hyphenopoly = null;
            })();
        }
        (() => {
            if (H.cft) {
                store.setItem(scriptName, JSON.stringify(
                    {
                        "langs": [...H.cf.langs.entries()],
                        "pf": H.cf.pf
                    }
                ));
            }
        })();
    });

    H.config = (c) => {
        /**
         * Sets default properties for an Object
         * @param {object} obj - The object to set defaults to
         * @param {object} defaults - The defaults to set
         * @returns {object}
         */
        const setDefaults = (obj, defaults) => {
            if (obj) {
                o.entries(defaults).forEach(([k, v]) => {
                    // eslint-disable-next-line security/detect-object-injection
                    obj[k] = obj[k] || v;
                });
                return obj;
            }
            return defaults;
        };

        H.cft = Boolean(c.cacheFeatureTests);
        if (H.cft && store.getItem(scriptName)) {
            H.cf = JSON.parse(store.getItem(scriptName));
            H.cf.langs = mp(H.cf.langs);
        } else {
            H.cf = {
                "langs": mp(),
                "pf": false
            };
        }

        const maindir = thisScript.slice(0, (thisScript.lastIndexOf("/") + 1));
        const patterndir = maindir + "patterns/";
        H.paths = setDefaults(c.paths, {
            maindir,
            patterndir
        });
        H.s = setDefaults(c.setup, {
            "CORScredentials": "include",
            "hide": "all",
            "selectors": {".hyphenate": {}},
            "timeout": 1000
        });
        // Change mode string to mode int
        H.s.hide = ["all", "element", "text"].indexOf(H.s.hide);
        if (c.handleEvent) {
            H.hev = c.handleEvent;
        }

        const fallbacks = mp(o.entries(c.fallbacks || {}));
        H.lrq = mp();
        o.entries(c.require).forEach(([lang, wo]) => {
            H.lrq.set(lang.toLowerCase(), {
                "fn": fallbacks.get(lang) || lang,
                wo
            });
        });

        main();
    };
})(window, document, Hyphenopoly, Object);
/* Miscellaneous JS functions which run after the page loads to rewrite or adjust parts of the page. */
/* author: Said Achmiz */
/* license: MIT */

/*************/
/* CLIPBOARD */
/*************/

/*******************************************/
/*  Set up copy processors in main document.
 */
doWhenDOMContentLoaded(() => {
    registerCopyProcessorsForDocument(document);
});


/*************/
/* AUX-LINKS */
/*************/

/*************************************************************************/
/*  Add “backlinks” link to start of section popups, when that section has
    a backlinks block.
 */
addContentInjectHandler(GW.contentInjectHandlers.injectBacklinksLinkIntoLocalSectionPopFrame = (eventInfo) => {
    GWLog("injectBacklinksLinkIntoLocalSectionPopFrame", "rewrite.js", 1);

    let containingPopFrame = Extracts.popFrameProvider.containingPopFrame(eventInfo.container);
    if (   containingPopFrame.classList.contains("local-page") == true
        && containingPopFrame.classList.contains("full-page") == false) {
        let section = eventInfo.container.querySelector("section");
        if (section == null)
            return;

        let backlinksBlock = eventInfo.container.querySelector(`#${(CSS.escape(section.id))}-backlinks`);
        if (backlinksBlock == null)
            return;

        //  Construct link and enclosing block.
        let backlinksLink = newElement("A", {
            "class": "aux-links backlinks",
            "href": "#" + backlinksBlock.id
        }, {
            "innerHTML": "backlinks"
        });
        let sectionMetadataBlock = newElement("P", {
            "class": "section-metadata"
        });
        sectionMetadataBlock.append(backlinksLink);
        section.insertBefore(sectionMetadataBlock, section.children[1]);

        //  Make a click on the link uncollapse the backlinks block.
        backlinksLink.addActivateEvent((event) => {
            if (isWithinCollapsedBlock(backlinksBlock)) {
                GW.notificationCenter.addHandlerForEvent("Collapse.collapseStateDidChange", (info) => {
                    revealElement(backlinksBlock);
                }, {
                    once: true,
                    condition: (isWithinCollapsedBlock(backlinksBlock) == false)
                });
            } else {
                requestAnimationFrame(() => {
                    revealElement(backlinksBlock);
                });
            }
        });
    }
}, "rewrite", (info) => (info.context == "popFrame"));


/*********/
/* LISTS */
/*********/

GW.layout.orderedListTypes = [
	"decimal",
	"lower-alpha",
	"upper-alpha",
	"lower-roman",
	"upper-roman"
];

/*****************************************************************************/
/*	Returns the type (CSS `list-item` counter value type) of an <ol> element.
 */
function orderedListType(list) {
	if (list?.tagName != "OL")
		return null;

	for (let type of GW.layout.orderedListTypes)
		if (list.classList.contains(`list-type-${type}`))
			return type;

	return null;
}

/************************************************************************/
/*	Sets the type (CSS `list-item` counter value type) of an <ol> element.
 */
function setOrderedListType(list, type) {
	if (list?.tagName != "OL")
		return;

	for (let type of GW.layout.orderedListTypes)
		list.classList.remove(`list-type-${type}`);

	list.classList.add(`list-type-${type}`);
}

/*******************************************************************/
/*	Returns the nesting level (an integer in [1,listCyclePeriod]) of 
	a <ul> element.
 */
function unorderedListLevel(list) {
	if (list?.tagName != "UL")
		return 0;

	let prefix = "list-level-";

	return (parseInt(Array.from(list.classList).find(c => c.startsWith(prefix))?.slice(prefix.length)) || 1);
}

/***********************************************************/
/*	Sets CSS class matching nesting level of a <ul> element.
 */
function setUnorderedListLevel(list, level) {
	if (list?.tagName != "UL")
		return;

	let prefix = "list-level-";

	list.swapClasses([ Array.from(list.classList).find(c => c.startsWith(prefix)), `${prefix}${level}` ], 1);
}

/***********************************/
/*  Designate list type via a class.
 */
addContentInjectHandler(GW.contentInjectHandlers.designateListTypes = (eventInfo) => {
    GWLog("designateListTypes", "rewrite.js", 1);

    //	Workaround for case-insensitivity of CSS selectors.
    eventInfo.container.querySelectorAll("ol[type]").forEach(list => {
        switch (list.type) {
        case '1':
            setOrderedListType(list, "decimal");
            break;
        case 'a':
            setOrderedListType(list, "lower-alpha");
            break;
        case 'A':
            setOrderedListType(list, "upper-alpha");
            break;
        case 'i':
            setOrderedListType(list, "lower-roman");
            break;
        case 'I':
            setOrderedListType(list, "upper-roman");
            break;
        default:
            break;
        }
    });

	//	If not explicitly specified, cycle between these three list types.
    eventInfo.container.querySelectorAll("ol:not([type])").forEach(list => {
		let enclosingList = list.parentElement?.closest("ol");
		let enclosingListType = enclosingList?.parentElement?.matches("section#footnotes")
								? null
								: orderedListType(enclosingList);

    	switch (enclosingListType) {
		case "decimal":
			setOrderedListType(list, "upper-roman");
			break;
		case "upper-roman":
			setOrderedListType(list, "lower-alpha");
			break;
		case "lower-alpha":
		default:
			setOrderedListType(list, "decimal");
			break;
    	}
    });

	//	Set list levels.
	let listCyclePeriod = 3;
	eventInfo.container.querySelectorAll("ul").forEach(list => {
		setUnorderedListLevel(list, (unorderedListLevel(list.parentElement?.closest("ul")) % listCyclePeriod) + 1);
	});
}, ">rewrite");

/*****************************************************************/
/*	Wrap text nodes and inline elements in list items in <p> tags.
 */
addContentLoadHandler(GW.contentLoadHandlers.paragraphizeListTextNodes = (eventInfo) => {
    GWLog("paragraphizeListTextNodes", "rewrite.js", 1);

	eventInfo.container.querySelectorAll(selectorize([ "li" ])).forEach(paragraphizeTextNodesOfElement);
}, "rewrite");

/**********************************************/
/*  Rectify styling/structure of list headings.
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyListHeadings = (eventInfo) => {
    GWLog("rectifyListHeadings", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("p > strong:only-child").forEach(boldElement => {
        if (   boldElement.parentElement.childNodes.length == 2
            && boldElement.parentElement.firstChild == boldElement
            && boldElement.parentElement.lastChild.nodeType == Node.TEXT_NODE
            && boldElement.parentElement.lastChild.nodeValue == ":") {
            boldElement.parentElement.lastChild.remove();
            boldElement.lastTextNode.nodeValue += ":";
        }

        if (   boldElement.parentElement.childNodes.length == 1
            && boldElement.parentElement.tagName == "P"
            && boldElement.parentElement.nextElementSibling
            && boldElement.closest("LI") == null
            && (   [ "UL", "OL" ].includes(boldElement.parentElement.nextElementSibling.tagName)
                || boldElement.parentElement.nextElementSibling.classList.contains("columns")))
            boldElement.parentElement.classList.add("list-heading");
    });
}, "rewrite");


/***************/
/* BLOCKQUOTES */
/***************/

/****************************************/
/*	Rectify HTML structure of interviews.
 */
addContentLoadHandler(GW.contentLoadHandlers.rewriteInterviews = (eventInfo) => {
    GWLog("designateBlockquoteLevels", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("div.interview, div.interview > div.collapse").forEach(interviewWrapper => {
		if (interviewWrapper.firstElementChild.tagName != "UL")
			return;

		let interview = interviewWrapper.firstElementChild;
		interview.classList.add("interview");

		for (let exchange of interview.children) {
			exchange.classList.add("exchange");

			for (let utterance of exchange.firstElementChild.children) {
				utterance.classList.add("utterance");

				let speaker = utterance.querySelector("strong");

				//	If the speaker is wrapped, find the outermost wrapper.
				let nextNode;
				while (   speaker.parentElement
					   && speaker.parentElement.tagName != "P")
					speaker = speaker.parentElement;
				nextNode = speaker.nextSibling;
				speaker.classList.add("speaker");
				speaker.querySelector("speaker")?.classList.remove("speaker");

				//	Move colon.
				(speaker.querySelector("strong") ?? speaker).innerHTML += nextNode.textContent.slice(0, 1) + " ";
				nextNode.textContent = nextNode.textContent.slice(1).trimStart();
			}
		}

		unwrap(interviewWrapper);
	});
}, "rewrite");

/*************************************************************************/
/*	Returns the nesting level (an integer in [1,blockquoteCyclePeriod]) of 
	a <blockquote> element.
 */
function blockquoteLevel(blockquote) {
	if (blockquote?.tagName != "BLOCKQUOTE")
		return 0;

	let prefix = "blockquote-level-";

	return (parseInt(Array.from(blockquote.classList).find(c => c.startsWith(prefix))?.slice(prefix.length)) || 1);
}

/*******************************************************************/
/*	Sets CSS class matching nesting level of a <blockquote> element.
 */
function setBlockquoteLevel(blockquote, level) {
	if (blockquote?.tagName != "BLOCKQUOTE")
		return;

	let prefix = "blockquote-level-";

	blockquote.swapClasses([ Array.from(blockquote.classList).find(c => c.startsWith(prefix)), `${prefix}${level}` ], 1);
}

/******************************************/
/*  Designate blockquote level via a class.
 */
addContentInjectHandler(GW.contentInjectHandlers.designateBlockquoteLevels = (eventInfo) => {
    GWLog("designateBlockquoteLevels", "rewrite.js", 1);

	let blockquoteCyclePeriod = 6;
	eventInfo.container.querySelectorAll("blockquote").forEach(blockquote => {
		setBlockquoteLevel(blockquote, (blockquoteLevel(blockquote.parentElement?.closest("blockquote")) % blockquoteCyclePeriod) + 1);
	});
}, ">rewrite");


/**********/
/* TABLES */
/**********/

/**************************************************************************/
/*  If there are tables, import tablesorter.js (if need be) and make tables
    sortable.
 */
addContentInjectHandler(GW.contentInjectHandlers.makeTablesSortable = (eventInfo) => {
    GWLog("makeTablesSortable", "rewrite.js", 1);

    if (eventInfo.container.querySelector("table") == null)
        return;

    //  Import tablesorter.js, if need be.
    let scriptTag = document.querySelector("script[src*='/static/js/tablesorter.js']");
    if (scriptTag == null) {
        scriptTag = newElement("SCRIPT", {
            "type": "text/javascript",
            "src": "/static/js/tablesorter.js"
        });
        document.body.appendChild(scriptTag);
    }

    let sortTables = (eventInfo) => {
        jQuery("table", eventInfo.document).tablesorter();
    };

    if (window["jQuery"]) {
        sortTables(eventInfo);
    } else {
        GW.notificationCenter.addHandlerForEvent("Tablesorter.didLoad", (info) => {
            sortTables(eventInfo);
        }, { once: true });
    }
});

/************************************************************************/
/*  Wrap each table in a div.table-wrapper and a div.table-scroll-wrapper
    (for layout purposes).
 */
addContentLoadHandler(GW.contentLoadHandlers.wrapTables = (eventInfo) => {
    GWLog("wrapTables", "rewrite.js", 1);

    wrapAll("table", "table-wrapper", "DIV", eventInfo.container, true);
    wrapAll("table", "table-scroll-wrapper", "DIV", eventInfo.container, false);

    eventInfo.container.querySelectorAll(".table-scroll-wrapper").forEach(tableScrollWrapper => {
        transferClasses(tableScrollWrapper.closest(".table-wrapper"), tableScrollWrapper, [ "width-full" ]);
    });
}, "rewrite");

/********************************************************************/
/*  Rectify wrapper structure of full-width tables:

    div.table-wrapper.table.width-full
        div.table-scroll-wrapper
            table

    or

    div.table-wrapper.collapse
        div.collapse-content-wrapper.table.width-full
            div.table-scroll-wrapper
                table
 */
addContentInjectHandler(GW.contentInjectHandlers.wrapFullWidthTables = (eventInfo) => {
    GWLog("wrapFullWidthTables", "rewrite.js", 1);

    wrapAll(".table-wrapper .width-full", "table width-full", "DIV", eventInfo.container, true, [ "width-full" ]);
}, "rewrite", (info) => info.fullWidthPossible);


/***********/
/* FIGURES */
/***********/

/******************************************************************/
/*	Wrap text nodes and inline elements in figcaptions in <p> tags.
 */
addContentLoadHandler(GW.contentLoadHandlers.paragraphizeFigcaptionTextNodes = (eventInfo) => {
    GWLog("paragraphizeFigcaptionTextNodes", "rewrite.js", 1);

	eventInfo.container.querySelectorAll(selectorize([ "figcaption" ])).forEach(paragraphizeTextNodesOfElement);
}, "rewrite");

/***************************************************************************/
/*  Make sure that the figcaption, alt-text, and title are, collectively, as
    useful as possible (i.e., ensure that neither the alt-text nor the title
    duplicate the contents of the figcaption).
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyImageAuxText = (eventInfo) => {
    GWLog("rectifyImageAuxText", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("figure img").forEach(image => {
        let figcaption = image.closest("figure").querySelector("figcaption");
        if (figcaption == null)
            return;

        let [ captionText, titleText, altText ] = [
            figcaption.cloneNode(true),
            newElement("SPAN", null, { "innerHTML": image.getAttribute("title") }),
            newElement("SPAN", null, { "innerHTML": image.getAttribute("alt") }),
        ].map(element => {
            if (element)
                Typography.processElement(element, Typography.replacementTypes.CLEAN|Typography.replacementTypes.QUOTES);

            return element.textContent.trim();
        });

        if (titleText == captionText)
            image.title = altText;

        if (altText == captionText)
            image.alt = titleText;
    });
}, "rewrite");

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
 */
function setImageDimensions(image, fixWidth = false, fixHeight = false) {
    let width = image.getAttribute("width");
    let height = image.getAttribute("height");

    image.style.aspectRatio = `${width} / ${height}`;

	if (image.maxHeight == null) {
		//	This should match `1rem`.
		let baseFontSize = GW.isMobile() ? "18" : "20";

		/*	This should match the `max-height` property value for all images in
			figures (the `figure img` selector; see initial.css).
		 */
		image.maxHeight = window.innerHeight - (8 * baseFontSize);
	}

    if (image.maxHeight)
        width = Math.round(Math.min(width, image.maxHeight * (width/height)));

    if (fixWidth) {
        image.style.width = `${width}px`;
    }
    if (fixHeight) {
        //  Nothing, for now.
    }
}

/**********************************************************/
/*  Prevent reflow in annotations, reduce reflow elsewhere.
 */
addContentLoadHandler(GW.contentLoadHandlers.setImageDimensions = (eventInfo) => {
    GWLog("setImageDimensions", "rewrite.js", 1);

	//	Do not set image dimensions in sidenotes.
	if (eventInfo.container == Sidenotes.hiddenSidenoteStorage)
		return;

    eventInfo.container.querySelectorAll("figure img[width][height]").forEach(image => {
        let fixWidth = (   eventInfo.contentType == "annotation"
                        && (   image.classList.containsAnyOf([ "float-left", "float-right" ])
                        	|| image.closest("figure")?.classList.containsAnyOf([ "float-left", "float-right" ])));
        setImageDimensions(image, fixWidth);
    });

    //  Also ensure that SVGs get rendered as big as possible.
    eventInfo.container.querySelectorAll("figure img[src$='.svg']").forEach(svg => {
        svg.style.width = "100vw";
        svg.style.aspectRatio = svg.dataset.aspectRatio;
    });
}, "rewrite");

/********************************************/
/*  Prevent reflow due to lazy-loaded images.
 */
addContentInjectHandler(GW.contentInjectHandlers.updateImageDimensions = (eventInfo) => {
    GWLog("updateImageDimensions", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("figure img[width][height][loading='lazy']").forEach(image => {
        setImageDimensions(image, true);
    });
}, "rewrite");

/*********************************************************/
/*  Ensure image dimensions update when device is rotated.
 */
addContentInjectHandler(GW.contentInjectHandlers.addOrientationChangeImageDimensionUpdateEvents = (eventInfo) => {
    GWLog("addOrientationChangeImageDimensionUpdateEvents", "rewrite.js", 1);

	let images = eventInfo.container.querySelectorAll("figure img[width][height]");

	GW.mediaQueries.portraitOrientation.addListener((event) => { 
		requestAnimationFrame(() => {
			images.forEach(image => {
				image.style.width = "";
				setImageDimensions(image);
			});
		});
	});
}, "eventListeners");

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

        //  Tag the figure with the first (or only) media element’s float class.
        [ "float-left", "float-right" ].forEach(floatClass => {
            if (media.classList.contains(floatClass))
                figure.classList.add(floatClass);
                media.classList.remove(floatClass);
        });
    });
}, "rewrite");

/********************************/
/*  Don’t float solitary figures.
 */
addContentInjectHandler(GW.contentInjectHandlers.deFloatSolitaryFigures = (eventInfo) => {
    GWLog("deFloatSolitaryFigures", "rewrite.js", 1);

    let floatClasses = [ "float-left", "float-right" ];
    eventInfo.container.querySelectorAll(floatClasses.map(x => `figure.${x}:only-child`).join(", ")).forEach(figure => {
        if (isOnlyChild(figure))
            figure.classList.remove(...floatClasses);
    });
}, "rewrite");

/********************************************************************/
/*  Designate full-width figures as such (with a ‘width-full’ class).
 */
addContentInjectHandler(GW.contentInjectHandlers.prepareFullWidthFigures = (eventInfo) => {
    GWLog("prepareFullWidthFigures", "rewrite.js", 1);

    let fullWidthClass = "width-full";

    let allFullWidthMedia = eventInfo.container.querySelectorAll(`img.${fullWidthClass}, video.${fullWidthClass}`);
    allFullWidthMedia.forEach(fullWidthMedia => {
        fullWidthMedia.closest("figure").classList.toggle(fullWidthClass, true);
    });

    //  Constrain caption width to width of media element.
    let constrainCaptionWidth = (fullWidthMedia) => {
        let caption = fullWidthMedia.closest("figure").querySelector(".caption-wrapper");
        if (caption)
            caption.style.maxWidth = fullWidthMedia.offsetWidth > 0
                                     ? fullWidthMedia.offsetWidth + "px"
                                     : fullWidthMedia.closest(".markdownBody").offsetWidth + "px";
    };

    //  Add ‘load’ listener for lazy-loaded media.
    allFullWidthMedia.forEach(fullWidthMedia => {
        fullWidthMedia.addEventListener("load", fullWidthMedia.loadListener = (event) => {
            constrainCaptionWidth(fullWidthMedia);
            fullWidthMedia.loadListener = null;
        }, { once: true });
    });

    /*  Re-add ‘load’ listener for lazy-loaded media (as it might cause
        re-layout of e.g. sidenotes). Do this only after page layout is
        complete, to avoid spurious re-layout at initial page load.
     */
    doWhenPageLayoutComplete(() => {
        allFullWidthMedia.forEach(fullWidthMedia => {
            constrainCaptionWidth(fullWidthMedia);
            if (fullWidthMedia.loadListener) {
                fullWidthMedia.removeEventListener("load", fullWidthMedia.loadListener);
                fullWidthMedia.addEventListener("load", (event) => {
                    constrainCaptionWidth(fullWidthMedia);
                    GW.notificationCenter.fireEvent("Rewrite.fullWidthMediaDidLoad", {
                        mediaElement: fullWidthMedia
                    });
                }, { once: true });
            }
        });
        //  Add listener to update caption max-width when window resizes.
        addWindowResizeListener(event => {
            allFullWidthMedia.forEach(constrainCaptionWidth);
        }, "constrainFullWidthMediaCaptionWidthResizeListener");
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

/****************************************************************/
/*  Account for interaction between image-focus.js and popups.js.
 */
GW.notificationCenter.addHandlerForEvent("ImageFocus.imageOverlayDidAppear", (info) => {
    if (Extracts.popFrameProvider == Popups)
        Popups.hidePopupContainer();
});
GW.notificationCenter.addHandlerForEvent("ImageFocus.imageOverlayDidDisappear", (info) => {
    if (Extracts.popFrameProvider == Popups)
        Popups.unhidePopupContainer();
});



/***************/
/* CODE BLOCKS */
/***************/

/*************************************************************/
/*	Wrap each <pre> in a div.sourceCode (for layout purposes).
 */
addContentLoadHandler(GW.contentLoadHandlers.wrapPreBlocks = (eventInfo) => {
    GWLog("wrapPreBlocks", "rewrite.js", 1);

	wrapAll("pre", "sourceCode", "DIV", eventInfo.container, true, false);
}, "rewrite");

/**********************************************************************/
/*  Wrap each pre.width-full in a div.width-full (for layout purposes).
 */
addContentInjectHandler(GW.contentInjectHandlers.wrapFullWidthPreBlocks = (eventInfo) => {
    GWLog("wrapFullWidthPreBlocks", "rewrite.js", 1);

    wrapAll("pre.width-full", "width-full", "DIV", eventInfo.container, true, false);
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

/**************************/
/*	Aggregate margin notes.
 */
addContentLoadHandler(GW.contentLoadHandlers.aggregateMarginNotes = (eventInfo) => {
    GWLog("aggregateMarginNotes", "rewrite.js", 1);

	aggregateMarginNotesIfNeeded(eventInfo);
}, "rewrite");


/**************/
/* TYPOGRAPHY */
/**************/

/***********************************/
/*	Rectify typography in body text.

	NOTE: This should be temporary. Word breaks after slashes should be added
	in body text on the back end, at content build time. But that is currently
	not working, hence this temporary client-side solution.
	—SA 2023-09-13
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyTypographyInBodyText = (eventInfo) => {
    GWLog("rectifyTypographyInBodyText", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("p").forEach(graf => {
		Typography.processElement(graf, Typography.replacementTypes.WORDBREAKS);
	});
}, "rewrite");

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

/**********************************************/
/*  Hyphenate with Hyphenopoly.

    Requires Hyphenopoly_Loader.js to be loaded prior to this file.
 */
addContentInjectHandler(GW.contentInjectHandlers.hyphenate = (eventInfo) => {
    GWLog("hyphenate", "rewrite.js", 1);

    if (!(Hyphenopoly.hyphenators))
        return;

    if (GW.isX11())
        return;

    let selector = (GW.isMobile()
                    ? ".markdownBody p"
                    : (eventInfo.document == document
                       ? ".sidenote p, .abstract blockquote p"
                       : "p"));
    let blocks = eventInfo.container.querySelectorAll(selector);
    Hyphenopoly.hyphenators.HTML.then((hyphenate) => {
        blocks.forEach(block => {
            hyphenate(block);
            Typography.processElement(block, Typography.replacementTypes.NONE, true);
        });
    });
}, "rewrite");

/************************************************************************/
/*  Remove soft hyphens and other extraneous characters from copied text.
 */
addCopyProcessor((event, selection) => {
    Typography.processElement(selection, Typography.replacementTypes.CLEAN);

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
        content properties of the selection); inject surrounding spaces.
     */
    selection.querySelectorAll(".cite-joiner").forEach(citeJoiner => {
        citeJoiner.style.display = "initial";
        citeJoiner.innerHTML = ` ${citeJoiner.innerHTML} `;
    });

	/*	Inject preceding space when a span.cite-date follows immediately after
		a span.cite-author (i.e., there is no span.cite-joiner, because there
		are no more than two authors).
	 */
    selection.querySelectorAll(".cite-author + .cite-date").forEach(citeDateAfterAuthor => {
    	citeDateAfterAuthor.innerHTML = ` ${citeDateAfterAuthor.innerHTML}`;
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
    let fullWidthBlockLayoutStyles = document.querySelector("head").appendChild(newElement("STYLE", { id: "full-width-block-layout-styles" }));

    /*  Function to update layout variables (called immediately and on resize).
     */
    let updateFullWidthBlockLayoutStyles = (event) => {
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
    addWindowResizeListener(updateFullWidthBlockLayoutStyles, "updateFullWidthBlockLayoutStylesResizeListener");
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
/*  Transform title-link of truncated annotations (i.e., full annotations
    transcluded as partial annotations) to allow access to the full annotation.
 */
addContentLoadHandler(GW.contentLoadHandlers.rewriteTruncatedAnnotations = (eventInfo) => {
    GWLog("rewriteTruncatedAnnotations", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".annotation-partial").forEach(partialAnnotation => {
        //  Check to see whether the abstract exists.
        if (Annotations.referenceDataForLink(eventInfo.includeLink).content.abstract == null)
            return;

		//	Remove colon.
		partialAnnotation.querySelector(".data-field.author-date-aux").lastTextNode.nodeValue = ")";

        //  Rewrite title-link.
        let titleLink = partialAnnotation.querySelector("a.title-link");
        titleLink.classList.add(Annotations.annotatedLinkFullClass);
    });
}, "<rewrite", (info) => (   info.source == "transclude"
                          && info.contentType == "annotation"));

/*****************************************************************/
/*  Partial annotations, defined inline (in directories and such).
 */
addContentLoadHandler(GW.contentLoadHandlers.rewritePartialAnnotations = (eventInfo) => {
    GWLog("rewritePartialAnnotations", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".annotation-partial").forEach(partialAnnotation => {
        //  If already done, do not redo.
        if (partialAnnotation.firstElementChild.classList.contains("data-field"))
            return;

        //  Identify reference link.
        let referenceLink = partialAnnotation.querySelector("a");

        //  If already in progress, do not interfere.
        if (Transclude.isIncludeLink(referenceLink))
            return;

        //  Designate reference link, for annotations.js to identify it.
        referenceLink.classList.add("link-annotated-partial");

        //  Load data into Annotations.
        Annotations.cacheAPIResponseForLink(newDocument(partialAnnotation),
                                            referenceLink);

        //  Replace reference block contents with synthetic include-link.
        partialAnnotation.replaceChildren(synthesizeIncludeLink(referenceLink, {
            "class": "include-annotation include-replace-container link-annotated-partial",
            "data-template-fields": "annotationClassSuffix:$",
            "data-annotation-class-suffix": "-partial"
        }));

        //  Fire GW.contentDidLoadEvent (to trigger transclude).
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "rewritePartialAnnotations",
            container: partialAnnotation,
            document: eventInfo.document,
            loadLocation: eventInfo.loadLocation
        });
    });
}, "rewrite");

/***************************************************************************/
/*  Make the page thumbnail in an annotation load eagerly instead of lazily.
 */
addContentLoadHandler(GW.contentLoadHandlers.setEagerLoadingForAnnotationImages = (eventInfo) => {
    GWLog("setEagerLoadingForAnnotationImages", "rewrite.js", 1);

    let firstImage = (eventInfo.container.querySelector(".page-thumbnail"))
    if (firstImage) {
        firstImage.loading = "eager";
        firstImage.decoding = "sync";
    }
}, "rewrite", (info) => (info.contentType == "annotation"));

/***************************************************************************/
/*  Because annotations transclude aux-links, we make the aux-links links in
    the metadata line of annotations scroll down to the appended aux-links
    blocks.
 */
addContentInjectHandler(GW.contentInjectHandlers.rewriteAuxLinksLinksInTranscludedAnnotations = (eventInfo) => {
    GWLog("rewriteAuxLinksLinksInTranscludedAnnotations", "rewrite.js", 1);

    let annotation = eventInfo.container.querySelector(".annotation");
    if (annotation == null)
        return;

    let inPopFrame = (Extracts.popFrameProvider.containingPopFrame(annotation) != null);

    annotation.querySelectorAll(".data-field.aux-links a.aux-links").forEach(auxLinksLink => {
        let auxLinksLinkType = AuxLinks.auxLinksLinkType(auxLinksLink);
        let includedAuxLinksBlock = annotation.querySelector(`.${auxLinksLinkType}-append`);
        if (includedAuxLinksBlock) {
            auxLinksLink.onclick = () => { return false; };
            auxLinksLink.addActivateEvent((event) => {
                if (includedAuxLinksBlock.querySelector("ul, ol") == null) {
                    GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
                        revealElement(includedAuxLinksBlock);
                    }, { once: true });
                }

                revealElement(includedAuxLinksBlock);

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
/*  Remove the “Link Bibliography:” bold text when transcluding a link
    bibliography into a page’s Link Bibliography section.
 */
addContentInjectHandler(GW.contentInjectHandlers.removeSubheadingFromLinkBibliography = (eventInfo) => {
    GWLog("removeSubheadingFromLinkBibliography", "rewrite.js", 1);

    if (eventInfo.container.closest("section#link-bibliography-section")) {
        let subheading = eventInfo.container.querySelector("#link-bibliography > .aux-links-list-label");
        if (subheading)
            subheading.remove();
    }
}, "rewrite", (info) => (info.source == "transclude"));

/*****************************************************************************/
/*  Apply a class to those link-bibs that should use the more compact styling.
 */
addContentInjectHandler(GW.contentInjectHandlers.applyLinkBibliographyCompactStylingClass = (eventInfo) => {
    GWLog("applyLinkBibliographyCompactStylingClass", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".link-bibliography-list").forEach(linkBibList => {
        if (linkBibList.closest("li, .link-bibliography-append, .popframe-body.link-bibliography"))
            linkBibList.classList.add("link-bibliography-list-compact");
    });
}, "rewrite");


/*********************/
/* TABLE OF CONTENTS */
/*********************/

/******************************************************************/
/*  Sets TOC collapse state and updates the collapse toggle button.
 */
function setTOCCollapseState(collapsed = false) {
    let TOC = document.querySelector("#TOC");
    if (!TOC)
        return;

    TOC.classList.toggle("collapsed", collapsed);

    let button = TOC.querySelector(".toc-collapse-toggle-button");
    if (!button)
        return;

    button.title = collapsed ? "Expand table of contents" : "Collapse table of contents";
}

/*******************************************************/
/*  Add the collapse toggle button to the main page TOC.
 */
addContentLoadHandler(GW.contentLoadHandlers.injectTOCMinimizeButton = (eventInfo) => {
    GWLog("injectTOCMinimizeButton", "rewrite.js", 1);

    let TOC = document.querySelector("#TOC");
    if (!TOC)
        return;

    let button = newElement("BUTTON", {
        "class": "toc-collapse-toggle-button",
        "title": "Collapse table of contents",
        "tabindex": "-1"
    }, {
        "innerHTML": `<span>${(GW.svg("chevron-left-solid"))}</span>`
    });
    TOC.appendChild(button);

    let defaultTOCCollapseState = "false";
    setTOCCollapseState((localStorage.getItem("toc-collapsed") ?? defaultTOCCollapseState) == "true");

    button.addActivateEvent((event) => {
        setTOCCollapseState(TOC.classList.contains("collapsed") == false);
        localStorage.setItem("toc-collapsed", TOC.classList.contains("collapsed"));
    });
}, "rewrite", (info) => (info.container == document.body));

/***************************************************************************/
/*  Strip spurious <span> tags (unavoidably added by Pandoc) from TOC links.
 */
addContentLoadHandler(GW.contentLoadHandlers.stripTOCLinkSpans = (eventInfo) => {
    GWLog("stripTOCLinkSpans", "rewrite.js", 1);

    unwrapAll(".TOC li a > span:not([class])", eventInfo.container);
}, "rewrite");

/**************************************************************************/
/*  Update main page TOC with any sections within the initially loaded page
    that don’t already have TOC entries.
 */
addContentLoadHandler(GW.contentLoadHandlers.updateMainPageTOC = (eventInfo) => {
    GWLog("updateMainPageTOC", "rewrite.js", 1);

    updatePageTOCIfNeeded(eventInfo);
}, "rewrite", (info) => (info.container == document.body));

/*************************************************/
/*  Apply typography rectification to TOC entries.
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyTypographyInTOC = (eventInfo) => {
    GWLog("rectifyTypographyInTOC", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".TOC").forEach(TOC => {
        Typography.processElement(TOC, Typography.replacementTypes.WORDBREAKS, true);
    });
}, "rewrite");

/**********************************************************/
/*  Disable link decoration (underlining) on all TOC links.
 */
addContentLoadHandler(GW.contentLoadHandlers.disableTOCLinkDecoration = (eventInfo) => {
    GWLog("disableTOCLinkDecoration", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".TOC a").forEach(link => {
        link.classList.add("decorate-not");
    });
}, "rewrite");

/**********************************************************/
/*  Relocate and clean up TOC on tag directory index pages.
 */
addContentLoadHandler(GW.contentLoadHandlers.rewriteDirectoryIndexTOC = (eventInfo) => {
    GWLog("rewriteDirectoryIndexTOC", "rewrite.js", 1);

    let TOC = eventInfo.container.querySelector("#TOC");
    let seeAlsoSection = eventInfo.container.querySelector("#see-also");

    if (   TOC == null
        || seeAlsoSection == null)
        return;

    /*  Place the TOC after the “See Also” section (which also places it after
        the page abstract, if such exists, because that comes before the
        “See Also” section).
     */
    seeAlsoSection.parentElement.insertBefore(TOC, seeAlsoSection.nextElementSibling);

    //  The “See Also” section no longer needs a TOC entry.
    TOC.querySelector("#toc-see-also").closest("li").remove();

    /*  If “Links” is the only remaining section, then it does not itself need
        a TOC entry; shift its children up one TOC level.
     */
    let linksTOCEntry = TOC.querySelector("#toc-links");
    if (   linksTOCEntry
        && isOnlyChild(linksTOCEntry.closest("li"))) {
        let outerTOCList = TOC.querySelector("ul");
        let innerTOCList = TOC.querySelector("#toc-links + ul");

        TOC.insertBefore(innerTOCList, null);
        outerTOCList.remove();

        //  Mark with special class, for styling purposes.
        TOC.classList.add("TOC-links-only");
    }

	//	Update visibility.
	updateTOCVisibility(TOC);
}, "rewrite", (info) => (   info.container == document.body
                         && /\/(index)?$/.test(location.pathname)));

/*******************************************************************************/
/*	Update visibility of a TOC. (Hide if no entries; if main page TOC, also hide
	if one entry.)
 */
function updateTOCVisibility(TOC) {
    let numEntries = TOC.querySelectorAll("li").length;
    if (   (   TOC.id == "TOC"
            && numEntries <= 1)
        || numEntries == 0) {
        TOC.classList.toggle("hidden", true);
    } else {
        TOC.classList.toggle("hidden", false);
    }
}

/************************************************************************/
/*  If the table of contents has but one entry (or none at all), hide it.
 */
addContentLoadHandler(GW.contentLoadHandlers.updateTOCVisibility = (eventInfo) => {
    GWLog("removeTOCIfSingleEntry", "rewrite.js", 1);

    let TOC = eventInfo.container.querySelector(".TOC");
    if (TOC == null)
        return;

	updateTOCVisibility(TOC);
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

/*****************************************************************************/
/*  Mark hash-targeted footnote with ‘targeted’ class on page load, and update
    when hash changes.
 */
addContentInjectHandler(GW.contentInjectHandlers.markTargetedFootnote = (eventInfo) => {
    GWLog("markTargetedFootnote", "rewrite.js", 1);

    //  Mark target footnote, if any.
    updateFootnoteTargeting();

    //  Add event handler to update targeting again on hash change.
    GW.notificationCenter.addHandlerForEvent("GW.hashDidChange", (info) => {
        updateFootnoteTargeting();
    });
}, "rewrite", (info) => info.container == document.body);

/******************************/
/*  Inject footnote self-links.
 */
addContentLoadHandler(GW.contentLoadHandlers.injectFootnoteSelfLinks = (eventInfo) => {
    GWLog("injectFootnoteSelfLinks", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("#footnotes > ol > li").forEach(footnote => {
        if (footnote.querySelector(".footnote-self-link"))
            return;

        let footnoteNumber = Notes.noteNumber(footnote);
        footnote.insertBefore(newElement("A", {
        	href: `#fn${footnoteNumber}`,
        	title: `Link to footnote ${footnoteNumber}`,
        	class: "footnote-self-link"
        }, {
        	innerHTML: "&nbsp;"
        }), footnote.firstChild);
    });
}, "rewrite");

/*****************************************************************/
/*  Rewrite footnote back-to-citation links (generated by Pandoc).
 */
addContentLoadHandler(GW.contentLoadHandlers.rewriteFootnoteBackLinks = (eventInfo) => {
    GWLog("rewriteFootnoteBackLinks", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("#footnotes > ol > li").forEach(footnote => {
        let backlink = footnote.querySelector(".footnote-back");

		if (isOnlyChild(backlink))
			backlink.parentElement.classList.add("footnote-back-block");

        if (backlink.querySelector("svg, .placeholder"))
            return;

        backlink.innerHTML = GW.svg("arrow-hook-left");
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
        addContentInjectHandler(GW.contentInjectHandlers.rebindHighlightEventsToFootnoteSelfLinks = (eventInfo) => {
            allCitations.forEach(bindEventsToCitation);
        }, "eventListeners", (info) => (   info.document == document
        								|| info.document == eventInfo.document));
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

    let exclusionSelector = [
        ".backlink-source"
    ].join(", ");

    eventInfo.container.querySelectorAll("a[href]").forEach(link => {
        if (link.closest(exclusionSelector) != null)
            return;

        if (   (   link.getAttribute("href").startsWith("#")
                || link.pathname == loadLocation.pathname)
                // if initial base page load
            && (   eventInfo.container == document.body
                // if the link refers to an element also in the loaded content
                || eventInfo.container.querySelector(selectorFromHash(link.hash)) != null
                // if the link refers to the loaded content container itself
                || (   eventInfo.container instanceof Element
                    && eventInfo.container.matches(selectorFromHash(link.hash)))
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

    let exclusionSelector = [
        "h1, h2, h3, h4, h5, h6",
        ".section-self-link",
        ".footnote-ref",
        ".footnote-back",
        ".footnote-self-link",
        ".sidenote-self-link",
        ".backlink-context"
    ].join(", ");

    eventInfo.container.querySelectorAll(".markdownBody a[href]").forEach(link => {
        if (   link.hostname != location.hostname
            || link.closest(exclusionSelector))
            return;

        if (   link.pathname == baseLocation.pathname
                // if initial base page load
            && (   eventInfo.container == document.body
                // if the link refers to an element also in the loaded content
                || eventInfo.container.querySelector(selectorFromHash(link.hash)) != null
               // if the link refers to the loaded content container itself
                || (   eventInfo.container instanceof Element
                    && eventInfo.container.matches(selectorFromHash(link.hash))))) {
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
            (link.compareDocumentPosition(target) & Node.DOCUMENT_POSITION_FOLLOWING
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

/*****************************************/
/*  Removes link icons that should not be.
 */
addContentInjectHandler(GW.contentInjectHandlers.cleanSpuriousLinkIcons = (eventInfo) => {
    GWLog("cleanSpuriousLinkIcons", "rewrite.js", 1);

    let excludedLinkSelector = [
        /*  Index page, and embeds thereof, do not need the G icon.

            NOTE: we do not use the usual method of suppressing G icons
            (`.icon-not` class), because /index and /static/404 are *so* long
            and routinely modified/expanded, so doing it ‘manually’ would risk
            occasional omissions or syntax errors.
         */
        "body.page-index",
        "body.page-static-404",
        ".popframe-body.page-index",
        ".popframe-body.page-static-404",

        /*  TOC links should never have link icons under any circumstances.
         */
        ".TOC"
    ].map(x => x + " a[data-link-icon]").join(", ");

    eventInfo.container.querySelectorAll(excludedLinkSelector).forEach(link => {
        link.removeAttribute("data-link-icon-type");
        link.removeAttribute("data-link-icon");
    });
}, "rewrite");

/****************************************************************************/
/*  Adds HTML and CSS to a link, enabling display of its specified link icon.
 */
function enableLinkIcon(link) {
    if (link.classList.contains("has-icon"))
        return;

    //  Add hook.
    link.appendChild(newElement("SPAN", { class: "link-icon-hook" }, { innerHTML: "\u{2060}" }));

    //  Set CSS variable.
    if (link.dataset.linkIconType.includes("text")) {
        link.style.setProperty("--link-icon", `"${(link.dataset.linkIcon)}"`);
    } else if (link.dataset.linkIconType.includes("svg")) {
		let iconFileURL = versionedAssetURL("/static/img/icon/icons.svg");
        link.style.setProperty("--link-icon-url",
            `url("${iconFileURL.pathname}${iconFileURL.search}#${(link.dataset.linkIcon)}")`);
    }

    //  Set class.
    link.classList.add("has-icon");
}

/****************************************************************************/
/*  Disable display of a link’s link icon by removing requisite HTML and CSS.
 */
function disableLinkIcon(link) {
    if (link.classList.contains("has-icon") == false)
        return;

    //  Remove hook.
    link.querySelector(".link-icon-hook").remove();

    //  Clear CSS variables.
    link.style.removeProperty("--link-icon");
    link.style.removeProperty("--link-icon-url");

    //  Unset class.
    link.classList.remove("has-icon");
}

/*************************************************************************/
/*  Enable or disable display of link icons, as appropriate for each link.
 */
addContentInjectHandler(GW.contentInjectHandlers.setLinkIconStates = (eventInfo) => {
    GWLog("setLinkIconStates", "rewrite.js", 1);

    /*  Enable display of link icons for all links that have specified icons.
     */
    eventInfo.container.querySelectorAll("a[data-link-icon]").forEach(link => {
        enableLinkIcon(link);
    });

    /*  Disable display of link icons for links that have had it enabled, but
        actually should not display icons (which may happen if, e.g.,
        a .link-page becomes a .link-self due to transclusion / pop-frame
        embedding, and has no anchor).
     */
    let iconlessLinkSelector = [
        "a:not([data-link-icon])",
        "a[data-link-icon='']"
    ].map(x => x + ".has-icon").join(", ");
    eventInfo.container.querySelectorAll(iconlessLinkSelector).forEach(link => {
        disableLinkIcon(link);
    });
}, "rewrite");


/*********/
/* MISC. */
/*********/

GW.currencyFormatter = new Intl.NumberFormat('en-US', {
	style: 'currency',
	currency: 'USD',
	minimumFractionDigits: 2
});
GW.currentYear = new Date().getFullYear();

/*************************************************************************/
/*	Return prettified version of a string representing an amount of money.
 */
function prettifyCurrencyString(amount, compact = false, forceRound = false) {
	let currency = amount[0];

	let number = Number(amount.replace(/[^0-9.−-]+/g, ""));
	if (   number >= 100
		|| forceRound)
		number = Math.round(number);

	amount = GW.currencyFormatter.format(number);

	//	Remove trailing zeroes.
	amount = amount.replace(/\.00?$/, '');

	//	Reset currency unit.
	amount = currency + amount.slice(1);

	if (compact) {
		amount = amount.replace(/,000,000,000$/, 'b');
		amount = amount.replace(/,000,000$/, 'm');
		amount = amount.replace(/,000$/, 'k');
	}

	return amount;
}

/**************************************************************************/
/*	Rewrite inflation-adjustment elements to make the currency amounts more
	useful and readable.
 */
addContentLoadHandler(GW.contentLoadHandlers.rewriteInflationAdjusters = (eventInfo) => {
    GWLog("rewriteInflationAdjusters", "rewrite.js", 1);

	eventInfo.container.querySelectorAll(".inflation-adjusted").forEach(infAdj => {
		let unadjusted = infAdj.querySelector("sup");
		let adjusted = infAdj.firstChild;

		unadjusted.textContent = prettifyCurrencyString(unadjusted.textContent, true);

		/*	Always round adjusted amount if unadjusted amount has no fractional
			component and adjusted amount has more than one whole digit.
		 */
		let forceRound = (   unadjusted.textContent.includes(".") == false
						  && adjusted.textContent.match(/([0-9]+)(\.|$)/)[1].length > 1);
		adjusted.textContent = prettifyCurrencyString(adjusted.textContent, false, forceRound);
	});
}, "rewrite");

/***************************************************************************/
/*  Makes it so that copying an inflation-adjusted currency amount interacts 
	properly with copy-paste.
 */
addCopyProcessor((event, selection) => {
    /*  Rewrite inflation-adjuster elements into a simple inline typographical 
    	format, e.g. “$0.10 (1990; $1.30 in 2023)”.
     */
    selection.querySelectorAll(".inflation-adjusted").forEach(infAdj => {
		let adjustedText = infAdj.firstChild.textContent;
		let unadjustedText = infAdj.querySelector("sup").textContent;
		let yearText = infAdj.querySelector("sub").textContent;

		//	Un-abbreviate powers of 1,000 in unadjusted amount.
		unadjustedText = unadjustedText.replace("k", ",000");
		unadjustedText = unadjustedText.replace("m", ",000,000");
		unadjustedText = unadjustedText.replace("b", ",000,000,000");

        infAdj.innerHTML = `${unadjustedText} [${yearText}; ${adjustedText} in ${GW.currentYear}]`;
    });

    return true;
});

/******************************************************************************/
/*  Makes double-clicking on an inflation adjuster select the entire element.
	(This is so that the copy processor, above, can reliably work as intended.)
 */
addContentInjectHandler(GW.contentInjectHandlers.addDoubleClickListenersToInflationAdjusters = (eventInfo) => {
    GWLog("addDoubleClickListenersToInflationAdjusters", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".inflation-adjusted").forEach(infAdj => {
        infAdj.addEventListener("dblclick", (event) => {
            document.getSelection().selectNode(infAdj);
        });
    });
}, "eventListeners");

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
    citation on the next line, and looks ugly) and immediately after citations
    (which causes punctuation following a citation to be orphaned, and also
    looks ugly).
 */
addContentLoadHandler(GW.contentLoadHandlers.noBreakForCitations = (eventInfo) => {
    GWLog("noBreakForCitations", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".footnote-ref").forEach(citation => {
    	citation.parentElement.insertBefore(document.createTextNode("\u{2060}"), citation);
        let textNode = citation.querySelector("sup").firstTextNode;
        textNode.textContent = "\u{2060}" + textNode.textContent + "\u{2060}";
    });
}, "rewrite");

/****************************************************************************/
/*	Designate containers wherein colors (e.g. link colors) should be inverted
	(because the container has a dark background).
 */
addContentLoadHandler(GW.contentLoadHandlers.designatedColorInvertedContainers = (eventInfo) => {
    GWLog("designatedColorInvertedContainers", "rewrite.js", 1);

	let selector = [
		".admonition.warning",
		".admonition.error"
	].join(", ");

	eventInfo.container.querySelectorAll(selector).forEach(container => {
		container.classList.add("colors-invert");
	});
}, "rewrite");

/******************************************************************/
/*	Wrap text nodes and inline elements in admonitions in <p> tags.
 */
addContentLoadHandler(GW.contentLoadHandlers.paragraphizeAdmonitionTextNodes = (eventInfo) => {
    GWLog("paragraphizeAdmonitionTextNodes", "rewrite.js", 1);

	eventInfo.container.querySelectorAll(selectorize([ ".admonition", ".admonition-title" ])).forEach(paragraphizeTextNodesOfElement);
}, "rewrite");

/*********************************************/
/*	Fix incorrect text block tag types.

	- .text-center are <div> but should be <p>
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifySpecialTextBlockTagTypes = (eventInfo) => {
    GWLog("rectifySpecialTextBlockTagTypes", "rewrite.js", 1);

	eventInfo.container.querySelectorAll(".text-center").forEach(div => {
		rewrapContents(div, null, "P", true, true);
	});
}, "rewrite");

/*******************************************************/
/*	Designate ordinal superscripts (1st, 2nd, 3rd, nth).
 */
addContentLoadHandler(GW.contentLoadHandlers.designateOrdinals = (eventInfo) => {
    GWLog("designateOrdinals", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("sup").forEach(sup => {
		if ([ "st", "nd", "rd", "th" ].includes(sup.textContent.toLowerCase()))
			sup.classList.add("ordinal");
	});
}, "rewrite");


/*************/
/* DROP CAPS */
/*************/

/****************************************************************************/
/*	Wrap non-graphical (textual) drop-caps in a span, for ease of processing.
 */
addContentInjectHandler(GW.contentInjectHandlers.separateNonGraphicalDropCaps = (eventInfo) => {
    GWLog("separateNonGraphicalDropCaps", "rewrite.js", 1);

	processContainerNowAndAfterBlockLayout(eventInfo.container, (container) => {
		container.querySelectorAll("p[class*='drop-cap-']").forEach(dropCapBlock => {
			//	If this drop-cap has already been processed, do nothing.
			if (dropCapBlock.querySelector(".drop-cap"))
				return;

			//	Determine drop-cap type.
			let dropCapType = dropCapTypeOf(dropCapBlock);

			//	If this is a graphical drop-cap, do nothing.
			if (GW.graphicalDropCaps.dropCapTypes.includes(dropCapType) == true)
				return;

			//	Determine initial letter.
			let firstLetter = dropCapBlock.firstTextNode.textContent.slice(0, 1);

			//	Separate first letter from rest of text content.
			dropCapBlock.firstTextNode.textContent = dropCapBlock.firstTextNode.textContent.slice(1);

			//	Inject the drop-cap.
			let dropCap = newElement("SPAN", {
				class: "drop-cap"
			}, {
				innerHTML: firstLetter
			});
			dropCapBlock.insertBefore(dropCap, dropCapBlock.firstChild);
		});
	});
}, "rewrite", (info) => (   info.document == document
						 && GW.mediaQueries.mobileWidth.matches == false));

/*************************************************************/
/*	Graphical drop-caps (only on sufficiently wide viewports).
 */
addContentInjectHandler(GW.contentInjectHandlers.enableGraphicalDropCaps = (eventInfo) => {
    GWLog("enableGraphicalDropCaps", "rewrite.js", 1);

	processContainerNowAndAfterBlockLayout(eventInfo.container, (container) => {
		container.querySelectorAll("p[class*='drop-cap-']").forEach(dropCapBlock => {
			//	If this block has already been processed, no need to do anything.
			if (dropCapBlock.querySelector(".drop-cap"))
				return;

			//	Determine drop-cap type.
			let dropCapType = dropCapTypeOf(dropCapBlock);

			//	Is this a recognized graphical drop-cap type?
			if (GW.graphicalDropCaps.dropCapTypes.includes(dropCapType) == false)
				return;

			//	Designate as graphical drop-cap.
			dropCapBlock.classList.add("graphical-drop-cap");

			//	Determine initial letter.
			let firstLetter = dropCapBlock.firstTextNode.textContent.slice(0, 1);

			//	Separate first letter from rest of text content.
			dropCapBlock.firstTextNode.textContent = dropCapBlock.firstTextNode.textContent.slice(1);

			//	Inject a hidden span to hold the first letter as text.
			dropCapBlock.insertBefore(newElement("SPAN", {
				class: "hidden-first-letter",
			}, {
				innerHTML: firstLetter
			}), dropCapBlock.firstChild);

			//	Select a drop-cap.
			let dropCapURL = randomDropCapURL(dropCapType, firstLetter);

			//	Inject the drop-cap image element.
			let dropCapImage = newElement("IMG", {
				class: "drop-cap figure-not",
				src: dropCapURL.pathname + dropCapURL.search
			});
			dropCapBlock.insertBefore(dropCapImage, dropCapBlock.firstChild);
		});
	});
}, "rewrite", (info) => (   info.document == document
						 && GW.mediaQueries.mobileWidth.matches == false));

/***********************************************************/
/*	Activate mode-based dynamic graphical drop-cap swapping.
 */
addContentInjectHandler(GW.contentInjectHandlers.activateDynamicGraphicalDropCaps = (eventInfo) => {
    GWLog("activateDynamicGraphicalDropCaps", "rewrite.js", 1);

	processContainerNowAndAfterBlockLayout(eventInfo.container, (container) => {
		container.querySelectorAll("p[class*='drop-cap-']").forEach(dropCapBlock => {
			//	Determine drop-cap type.
			let dropCapType = dropCapTypeOf(dropCapBlock);

			//	Is this a recognized graphical drop-cap type?
			if (GW.graphicalDropCaps.dropCapTypes.includes(dropCapType) == false)
				return;

			//	Get the drop-cap image element.
			let dropCapImage = dropCapBlock.querySelector("img.drop-cap");
			if (dropCapImage == null)
				return;

			//	If the handler already exists, do nothing.
			if (dropCapImage.modeChangeHandler)
				return;

			//	Get the initial letter.
			let firstLetter = dropCapBlock.querySelector(".hidden-first-letter")?.textContent;
			if (firstLetter == null)
				return;

			//	Add event handler to switch image when mode changes.
			GW.notificationCenter.addHandlerForEvent(dropCapImage.modeChangeHandler = "DarkMode.computedModeDidChange", (info) => {
				let dropCapUrl = randomDropCapURL(dropCapType, firstLetter);
				dropCapImage.src = dropCapUrl.pathname + dropCapUrl.search;
			});
		});
	});
}, "eventListeners", (info) => (   info.document == document
								&& GW.mediaQueries.mobileWidth.matches == false));

/*********************/
/*	Linkify drop-caps.
 */
addContentInjectHandler(GW.contentInjectHandlers.linkifyDropCaps = (eventInfo) => {
    GWLog("linkifyDropCaps", "rewrite.js", 1);

	processContainerNowAndAfterBlockLayout(eventInfo.container, (container) => {
		container.querySelectorAll("p[class*='drop-cap-']").forEach(dropCapBlock => {
			//	If this drop-cap has already been linkified, do nothing.
			if (dropCapBlock.querySelector(".link-drop-cap"))
				return;

			//	Determine drop-cap type.
			let dropCapType = dropCapTypeOf(dropCapBlock);

			//	Determine initial letter.
			let firstLetter = (   dropCapBlock.querySelector("span.drop-cap")
							   ?? dropCapBlock.querySelector(".hidden-first-letter")).textContent;

			//	Get the drop-cap (textual or graphical).
			let dropCap = dropCapBlock.querySelector(".drop-cap");

			//	Wrap the drop-cap (textual or graphical) in a link.
			let dropCapLink = newElement("A", {
				class: "link-page link-drop-cap",
				href: "/dropcap#" + dropCapType,
				"data-letter": firstLetter,
				"data-drop-cap-type": dropCapType
			});
			let dropCapLinkWrapper = newElement("SPAN");
			dropCapLinkWrapper.append(dropCapLink);
			dropCapLink.append(dropCap);
			dropCapBlock.insertBefore(dropCapLinkWrapper, dropCapBlock.firstChild);

			//	Process the link to enable extract pop-frames.
			Extracts.addTargetsWithin(dropCapLinkWrapper);

			//	Unwrap temporary wrapper.
			unwrap(dropCapLinkWrapper);
		});
	});
}, "rewrite", (info) => (   info.document == document
						 && GW.mediaQueries.mobileWidth.matches == false));

/***********************************************************************/
/*	Prevent blocks with drop caps from overlapping the block below them.
 */
addContentInjectHandler(GW.contentInjectHandlers.preventDropCapsOverlap = (eventInfo) => {
    GWLog("preventDropCapsOverlap", "rewrite.js", 1);

	processContainerNowAndAfterBlockLayout(eventInfo.container, (container) => {
		container.querySelectorAll("p[class*='drop-cap-']").forEach(dropCapBlock => {
			let nextBlock = nextBlockOf(dropCapBlock, { alsoBlockElements: [ ".list" ] });
			if (   nextBlock == null
				|| nextBlock.matches("section, blockquote, .list, .collapse, .list-heading"))
				dropCapBlock.classList.add("overlap-not");
		});
	});
}, ">rewrite", (info) => (   info.document == document
						  && GW.mediaQueries.mobileWidth.matches == false));


/********/
/* MATH */
/********/

/**************************************/
/*	Unwrap <p> wrappers of math blocks.
 */
addContentLoadHandler(GW.contentLoadHandlers.unwrapMathBlocks = (eventInfo) => {
    GWLog("unwrapMathBlocks", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".mjpage__block").forEach(mathBlock => {
        mathBlock = mathBlock.closest(".math");
        mathBlock.classList.add("block");

		if (   mathBlock.parentElement?.matches("p")
			&& isOnlyChild(mathBlock))
			unwrap(mathBlock.parentElement);
	});
}, "rewrite");

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

    selection.querySelectorAll(".mjx-chtml").forEach(mathElement => {
        mathElement.innerHTML = " " + mathElement.querySelector(".mjx-math").getAttribute("aria-label") + " ";
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

    eventInfo.container.querySelectorAll(".mjpage").forEach(mathElement => {
        mathElement.addEventListener("dblclick", (event) => {
            document.getSelection().selectAllChildren(mathElement.querySelector(".mjx-chtml"));
        });
        mathElement.title = mathElement.classList.contains("mjpage__block")
        					? "Double-click to select equation, then copy, to get LaTeX source (or, just click the Copy button in the top-right of the equation area)"
        					: "Double-click to select equation; copy to get LaTeX source";
    });
}, "eventListeners");

/****************************************************************/
/*  Add block buttons (copy) to block (not inline) math elements.
 */
addContentLoadHandler(GW.contentLoadHandlers.addBlockButtonsToMathBlocks = (eventInfo) => {
    GWLog("addBlockButtonsToMathBlocks", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".math.block").forEach(mathBlock => {
        //  Inject button bar.
        mathBlock.appendChild(newElement("SPAN", { class: "block-button-bar" })).append(
        	newElement("BUTTON", {
				type: "button",
				class: "copy",
				tabindex: "-1",
				title: "Copy LaTeX source of this equation to clipboard"
			}, {
				innerHTML: GW.svg("copy-regular")
			}), 
			newElement("SPAN", {
				class: "scratchpad"
			})
		);
    });
}, "rewrite");

/************************************************/
/*  Activate copy buttons of math block elements.
 */
addContentInjectHandler(GW.contentInjectHandlers.activateMathBlockButtons = (eventInfo) => {
    GWLog("activateMathBlockButtons", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".math.block").forEach(mathBlock => {
        //  LaTeX source.
        let latexSource = mathBlock.querySelector(".mjx-math").getAttribute("aria-label");

        //  Copy button (copies LaTeX source).
        mathBlock.querySelector("button.copy").addActivateEvent((event) => {
            GWLog("mathBlockCopyButtonClicked", "rewrite.js", 3);

            copyTextToClipboard(latexSource);

            //  Flash math block, for visual feedback of copy operation.
            let innerMathBlock = mathBlock.querySelector(".MJXc-display");
            innerMathBlock.classList.add("flash");
            setTimeout(() => { innerMathBlock.classList.remove("flash"); }, 150);
        });
    });
}, "eventListeners");


/**********************************/
/* BROKEN HTML STRUCTURE CHECKING */
/**********************************/

/*  Check for #footnotes outside of #markdownBody, which indicates a prematurely
    closed div#markdownBody (probably due to some error in the page source).
 */
doWhenPageLoaded(() => {
    let footnotesSection = document.querySelector("#footnotes");
    if (   footnotesSection
        && footnotesSection.closest("#markdownBody") == null)
        GWServerLogError(location.href + "--broken-html-structure");
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
GW.notificationCenter.addHandlerForEvent("GW.hashHandlingSetupDidComplete", GW.brokenAnchorCheck = (eventInfo) => {
    GWLog("GW.brokenAnchorCheck", "rewrite.js", 1);

    if (   location.hash > ""
        && /^#if_slide/.test(location.hash) == false
        && /^#:~:/.test(location.hash) == false
        && document.querySelector(selectorFromHash(location.hash)) == null)
        reportBrokenAnchorLink(location);
}, { once: true });
GW.notificationCenter.addHandlerForEvent("GW.hashDidChange", GW.brokenAnchorCheck);


/************/
/* PRINTING */
/************/

/*********************************************************************/
/*  Trigger transcludes and expand-lock collapse blocks when printing.
 */
window.addEventListener("beforeprint", (event) => {
    GWLog("Print command received.", "rewrite.js", 1);

    function expand(container) {
        if (   container instanceof Element
            && container.closest("#link-bibliography, .aux-links-append"))
            return;

        Transclude.triggerTranscludesInContainer(container);
        container.querySelectorAll(".collapse").forEach(expandLockCollapseBlock);
    }

    GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", GW.expandAllContentWhenLoadingPrintView = (eventInfo) => {
        expand(eventInfo.container);
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
/*************************/
/*	Configuration / state.
 */
GW.collapse = {
	/*	Visibility of block collapse labels depends on how many times the user 
		has used them already.
	 */
	alwaysShowCollapseInteractionHints: (getSavedCount("clicked-to-expand-collapse-block-count") < (GW.isMobile() ? 6 : 3)),
	showCollapseInteractionHintsOnHover: (   GW.isMobile() == false 
										  && getSavedCount("clicked-to-expand-collapse-block-count") < 6),

	/*	Hover events (see below).
	 */
	hoverEventsEnabled: (GW.isMobile() == false),
	hoverEventsActive: (GW.isMobile() == false)
};

/****************************************************************************/
/*	On desktop, disable hover events on scroll; re-enable them on mouse move.
 */
if (GW.collapse.hoverEventsEnabled) {
	//	Disable on scroll.
	addScrollListener(GW.collapse.disableCollapseHoverEventsOnScroll = (event) => {
		GW.collapse.hoverEventsActive = false;
	}, "disableCollapseHoverEventsOnScrollListener");

	/*	Add event handler to add scroll listener to spawned popups, to
		disable hover events when scrolling within a popup.
	 */
	GW.notificationCenter.addHandlerForEvent("Popups.popupDidSpawn", GW.collapse.addDisableHoverEventsOnScrollListenerOnPopupSpawned = (info) => {
		addScrollListener(GW.collapse.disableCollapseHoverEventsOnScroll, null, null, info.popup.scrollView);
	});

	//	Enable on mousemove.
	window.addEventListener("mousemove", GW.collapse.windowMouseMove = (event) => {
		GW.collapse.hoverEventsActive = true;
	});
}

/*******************************************************************************/
/*  This function expands all collapse blocks containing the given node, if
    any (including the node itself, if it is a collapse block). Returns true
    if any such expansion occurred. Fires Collapse.collapseStateDidChange event
    after all (possibly recursive) expansion is completed. (Only one event fired
    per non-recursive call to expandCollapseBlocksToReveal(), even if recursive
    expansion occurred.)
 */
function expandCollapseBlocksToReveal(node, fireStateChangedEvent = true) {
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

    //  Determine if nearest collapse block needs expanding.
    let collapseBlock = element.closest(".collapse");
    let expand = isCollapsed(collapseBlock);

    /*  Expand any higher-level collapse blocks.
		Fire state change event only if we will not have to expand this block
		(otherwise we’ll do redundant layout).
     */
	let expandedAncestor = expandCollapseBlocksToReveal(collapseBlock.parentElement, expand == false);

    if (expand) {
		//	Expand nearest collapse block.
		toggleCollapseBlockState(collapseBlock, expand);

		/*	Fire state change event only if we will not have to do any more 
			expansion (otherwise we’ll do redundant layout).
		 */
		if (fireStateChangedEvent) {
			GW.notificationCenter.fireEvent("Collapse.collapseStateDidChange", {
				source: "expandCollapseBlocksToReveal",
				collapseBlock: collapseBlock
			});
		}
	}

    //  Report whether we had to expand a collapse block.
    return (expand || expandedAncestor);
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
		disclosureButtonHTML += `<span class="icon">`
							  + (start
								 ? GW.svg("bracket-square-left-sharp-light")
								 : (  GW.svg("angle-right-regular")
									+ GW.svg("bracket-square-right-sharp-light")))
							  + `</span>`;
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

		if (GW.collapse.hoverEventsEnabled)
			collapseBlock.classList.add("expand-on-hover");

		let collapseWrapper;
		if ([ "DIV", "SECTION", "SPAN" ].includes(collapseBlock.tagName)) {
			//	No additional wrapper needed for these tag types.
			collapseWrapper = collapseBlock;

			/*	Rewrap spans that are NOT inline collapses (i.e., those that
				are, for some reason, wrapping block-level content).
			 */
			if (   collapseWrapper.tagName == "SPAN"
				&& containsBlockChildren(collapseWrapper))
				collapseWrapper = rewrapContents(collapseWrapper, null, "DIV", true, true);

			//	Designate collapse type (block or inline).
			if ([ "SPAN" ].includes(collapseWrapper.tagName))
				collapseWrapper.classList.add("collapse-inline");
			else
				collapseWrapper.classList.add("collapse-block");

			/*	Abstracts (the .abstract class) can end up in collapses
				without this being known in advance, so may not have the
				.abstract-collapse class, as they should.
			 */
			let collapseAbstract = collapseWrapper.querySelector(".collapse > .abstract");
			if (collapseAbstract?.closest(".collapse") == collapseWrapper)
				collapseAbstract.classList.add("abstract-collapse");

			//	Ensure correct structure and classes of abstracts.
			collapseAbstract = collapseWrapper.querySelector(".collapse > .abstract-collapse");
			if (collapseAbstract?.closest(".collapse") == collapseWrapper) {
				//	Mark those collapse blocks that have abstracts.
				collapseWrapper.classList.add("has-abstract");

				//	Wrap bare text nodes and inline elements in <p> elements.
				if (collapseWrapper.classList.contains("collapse-block"))
					paragraphizeTextNodesOfElement(collapseAbstract);

				//	Make sure “real” abstracts are marked as such.
				if (   collapseWrapper.classList.contains("collapse-block")
					&& collapseAbstract.firstElementChild?.tagName == "BLOCKQUOTE")
					collapseAbstract.classList.add("abstract");
			} else {
				//	Mark those collapse blocks that have no abstracts.
				collapseWrapper.classList.add("no-abstract");
			}

			//	Designate “bare content” collapse blocks.
			if (collapseWrapper.classList.contains("collapse-block")) {
				let bareContentTags = [ "P", "UL", "OL" ];
				if (   bareContentTags.includes(collapseWrapper.firstElementChild.tagName)
					|| (   collapseWrapper.classList.contains("has-abstract")
						&& bareContentTags.includes(collapseWrapper.firstElementChild.firstElementChild.tagName)))
					collapseWrapper.classList.add("bare-content");
			}
		} else {
			//	Additional wrapper is required for most tag types.
			collapseWrapper = wrapElement(collapseBlock, null, "DIV", true, [ "collapse", "expand-on-hover" ]);

			//	This is a block collapse.
			collapseWrapper.classList.add("collapse-block");

			//	Collapse blocks of this type never have abstracts.
			collapseWrapper.classList.add("no-abstract");
		}

		//	Mark as expanded, if need be.
		collapseWrapper.swapClasses([ "expanded", "expanded-not" ], startExpanded ? 0 : 1)

		//  Inject the disclosure button.
		if (collapseWrapper.classList.contains("collapse-inline")) {
			//	Button at start.
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

	eventInfo.container.querySelectorAll("section.collapse").forEach(section => {
		section.style.removeProperty("--collapse-toggle-top-height");
		section.style.removeProperty("--collapse-toggle-top-icon-size");

		requestIdleCallback(() => {
			let rects = Array.from(section.firstElementChild.querySelector("a").getClientRects());
			let oneLineHeight = rects.first?.height ?? 0;
			let totalHeight = rects.reduce((h, r) => h + r.height, 0);
			if (   oneLineHeight == 0
				|| totalHeight == 0)
				return;

			section.style.setProperty("--collapse-toggle-top-height", Math.round(totalHeight + oneLineHeight * 0.15) + "px");
			section.style.setProperty("--collapse-toggle-top-icon-size", Math.round(oneLineHeight * 1.15) + "px");
		});
	});
}, ">rewrite");

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

	let action = GW.isMobile() ? "Tap" : "Click";
	let labelHTML = isCollapsed(collapseBlock)
					? `${action} to expand`
					: `${action} to collapse`;

	if (collapseBlock.classList.contains("collapse-block")) {
		let disclosureButton = collapseBlock.querySelector(".disclosure-button");

		disclosureButton.querySelectorAll(".part .label").forEach(label => {
			label.innerHTML = labelHTML;
		});

		disclosureButton.classList.toggle("labels-visible", showLabels || GW.collapse.alwaysShowCollapseInteractionHints);
	} else {
		[ collapseBlock.firstElementChild, collapseBlock.lastElementChild ].forEach(disclosureButton => {
			disclosureButton.title = labelHTML;
		});
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

	/*	Compensate for block indentation due to nesting (e.g., lists).

		(Don’t do this for full-width collapses, as the full-width code will
		 already apply suitable side margins.)

		(Also don’t do this for collapses in blockquotes, which get treated
		 specially.)
	 */
	if (   collapseBlock.classList.contains("collapse-block")
		&& collapseBlock.closest("blockquote") == null
		&& collapseBlock.querySelector(".collapse-content-wrapper").classList.contains("width-full") == false) {
		if (expanding) {
			let collapseContentWrapper = collapseBlock.querySelector(".collapse-content-wrapper");
			let markdownBody = collapseBlock.closest(".markdownBody");

			let contentRect = collapseContentWrapper.getBoundingClientRect();
			let enclosingContentRect = markdownBody.getBoundingClientRect();
			let collapseLeftOffsetPx = getComputedStyle(collapseBlock).getPropertyValue("--collapse-left-offset");
			let floatOffset = 0;

			//	Compensate for TOC.
			if (markdownBody.id == "markdownBody") {
				let TOC = document.querySelector("#TOC");
				if (TOC) {
					let TOCRect = TOC.getBoundingClientRect();
					if (TOCRect.bottom > contentRect.top) {
						floatOffset = Math.round(  TOCRect.width 
												 + parseInt(getComputedStyle(TOC).marginRight)
												 + parseInt(getComputedStyle(collapseBlock).paddingLeft));
					}
				}
			}

			collapseBlock.style.marginLeft = `calc(${(enclosingContentRect.x - contentRect.x)}px - ${collapseLeftOffsetPx} + ${floatOffset}px)`;
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
				&& GW.collapse.hoverEventsEnabled) {
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
			&& GW.collapse.hoverEventsEnabled) {
			collapseBlock.addEventListener("mouseenter", (event) => {
				if (GW.collapse.hoverEventsActive == false) {
					collapseBlock.classList.add("hover-not");
				} else {
					collapseBlock.classList.remove("hover-not");
				}
			});
			onEventAfterDelayDo(collapseBlock, "mouseenter", 1000, (event) => {
				if (GW.collapse.hoverEventsActive == false)
					return;

				if (isCollapsed(collapseBlock) == false)
					return;

				if (collapseBlock.classList.contains("just-clicked"))
					return;

				disclosureButton.actionHandler(event);
			}, [ "mouseleave", "mousedown" ]);
		}

		//	On-hover state changes.
		if (GW.collapse.hoverEventsEnabled) {
			//	Add listener to show labels on hover, if need be.
			if (   collapseBlock.classList.contains("collapse-block")
				&& GW.collapse.showCollapseInteractionHintsOnHover == true
				&& GW.collapse.alwaysShowCollapseInteractionHints == false) {
				disclosureButton.addEventListener("mouseenter", (event) => {
					if (GW.collapse.hoverEventsActive == false)
						return;

					updateDisclosureButtonState(collapseBlock, true);
				});
				disclosureButton.addEventListener("mouseleave", (event) => {
					if (GW.collapse.hoverEventsActive == false)
						return;

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
					if (GW.collapse.hoverEventsActive == false)
						return;

					counterpart.classList.add("hover");
				});
				disclosureButton.addEventListener("mouseleave", (event) => {
					if (GW.collapse.hoverEventsActive == false)
						return;

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
/* sidenotes.js: standalone JS library for parsing HTML documents with Pandoc-style footnotes and dynamically repositioning them into the left/right margins, when browser windows are wide enough.
Sidenotes (see https://gwern.net/Sidenotes ) are superior to footnotes where possible because they enable the reader to immediately look at them without requiring user action to 'go to' or 'pop up' the footnotes; even floating footnotes require effort by the reader.
sidenotes.js is inspired by the Tufte-CSS sidenotes (https://edwardtufte.github.io/tufte-css/#sidenotes), but where Tufte-CSS uses static footnotes inlined into the body of the page (requiring modifications to Pandoc's compilation), which doesn't always work well for particularly long or frequent sidenotes, sidenotes.js will rearrange sidenotes to fit as best as possible, and will respond to window changes.
Particularly long sidenotes are also partially 'collapsed'.
Styling (especially for oversized-sidenotes which must scroll) is done in /static/css/default.css "SIDENOTES" section.

Author: Said Achmiz
2019-03-11
license: MIT (derivative of footnotes.js, which is PD)
*/

/*****************/
/*	Configuration.
 */
Sidenotes = {
	/*  The `sidenoteSpacing` constant defines the minimum vertical space that
		is permitted between adjacent sidenotes; any less, and they are
		considered to be overlapping.
		*/
	sidenoteSpacing: 60.0,

	/*	This includes the border width.
		*/
	sidenotePadding: 13.0,

	/*	Elements which occupy (partially or fully) the sidenote columns, and
		which can thus collide with sidenotes.
		*/
	potentiallyOverlappingElementsSelectors: [
		".width-full img",
		".width-full video",
		".width-full .caption-wrapper",
		".width-full table",
		".width-full pre",
		".marginnote"
	],

	constrainMarginNotesWithinSelectors: [
		".backlink-context",
		".margin-notes-block"
	],

	/*	The smallest width (in CSS dimensions) at which sidenotes will be shown.
		If the viewport is narrower than this, then sidenotes are disabled.
	 */
	minimumViewportWidthForSidenotes: "1761px",

	useLeftColumn: () => false,
	useRightColumn: () => true
};

/******************/
/*	Implementation.
 */
Sidenotes = { ...Sidenotes,
	/*  Media query objects (for checking and attaching listeners).
		*/
	mediaQueries: {
		viewportWidthBreakpoint: matchMedia(`(min-width: ${Sidenotes.minimumViewportWidthForSidenotes})`)
	},

	/*****************/
	/* Infrastructure.
	 */
	sidenotes: null,
	citations: null,

	sidenoteColumnLeft: null,
	sidenoteColumnRight: null,

	hiddenSidenoteStorage: null,

	positionUpdateQueued: false,

	sidenoteOfNumber: (number) => {
		return (Sidenotes.sidenotes.find(sidenote => Notes.noteNumberFromHash(sidenote.id) == number) ?? null);
	},

	citationOfNumber: (number) => {
		return (Sidenotes.citations.find(citation => Notes.noteNumberFromHash(citation.id) == number) ?? null);
	},

	/*	The sidenote of the same number as the given citation; 
		or, the citation of the same number as the given sidenote.
	 */
	counterpart: (element) => {
		let number = Notes.noteNumberFromHash(element.id);
		let counterpart = (element.classList.contains("sidenote")
						   ? Sidenotes.citationOfNumber(number)
						   : Sidenotes.sidenoteOfNumber(number));
		if (counterpart == null)
			GWLog(`Counterpart of ${element.tagName}#${element.id}.${(Array.from(element.classList).join("."))} not found!`, "sidenotes.js", 0);
		return counterpart;
	},

	/*  The “target counterpart” is the element associated with the target, i.e.:
		if the URL hash targets a footnote reference, its counterpart is the
		sidenote for that citation; and vice-versa, if the hash targets a sidenote,
		its counterpart is the in-text citation. We want a target counterpart to be
		highlighted along with the target itself; therefore we apply a special
		‘targeted’ class to the target counterpart.
		*/
	updateTargetCounterpart: () => {
		GWLog("Sidenotes.updateTargetCounterpart", "sidenotes.js", 1);

		if (Sidenotes.mediaQueries.viewportWidthBreakpoint.matches == false)
			return;

		//  Clear existing targeting.
		let targetedElementSelector = [
			".footnote-ref", 
			".footnote",
			".sidenote"
		].map(x => x + ".targeted").join(", ");
		document.querySelectorAll(targetedElementSelector).forEach(element => {
			element.classList.remove("targeted");
		});

		//  Identify target and counterpart, if any.
		let target = location.hash.match(/^#(sn|fnref)[0-9]+$/)
					 ? getHashTargetedElement()
					 : null;

		if (target == null)
			return;

		let counterpart = Sidenotes.counterpart(target);

		//  Mark the target and the counterpart, if any.
		if (target)
			target.classList.add("targeted");
		if (counterpart)
			counterpart.classList.add("targeted");
	},

	/*	Set margin notes to ‘inline’ or ‘sidenote’ style, depending on what mode
		the page is in (based on viewport width), whether each margin note is
		in a constrained block, and whether it’s on the main page or in 
		something like a pop-frame.

		(This function should be called from a load or inject event handler,
		 and the event info passed to it as argument.)
	 */
	setMarginNoteStyle: (eventInfo) => {
		GWLog("Sidenotes.setMarginNoteStyle", "sidenotes.js", 1);

		eventInfo.container.querySelectorAll(".marginnote").forEach(marginNote => {
			let inline = (   marginNote.closest(Sidenotes.constrainMarginNotesWithinSelectors.join(", "))
						  || Sidenotes.mediaQueries.viewportWidthBreakpoint.matches == false
						  || eventInfo.document != document);
			marginNote.swapClasses([ "inline", "sidenote" ], (inline ? 0 : 1));
		});
	},

	/*  Hide sidenotes within currently-collapsed collapse blocks. Show
		sidenotes not within currently-collapsed collapse blocks.
		*/
	updateSidenotesInCollapseBlocks: () => {
		GWLog("Sidenotes.updateSidenotesInCollapseBlocks", "sidenotes.js", 1);

		Sidenotes.sidenotes.forEach(sidenote => {
			let citation = Sidenotes.counterpart(sidenote);
			sidenote.classList.toggle("hidden", isWithinCollapsedBlock(citation));
		});
	},

	/*	Queues a sidenote position update on the next available animation frame,
		if an update is not already queued.
	 */
	updateSidenotePositionsIfNeeded: () => {
		if (Sidenotes.positionUpdateQueued)
			return;

		Sidenotes.positionUpdateQueued = true;
		requestAnimationFrame(() => {
			Sidenotes.positionUpdateQueued = false;
			Sidenotes.updateSidenotePositions();
		});
	},

	/*  This function actually calculates and sets the positions of all sidenotes.
		*/
	updateSidenotePositions: () => {
		GWLog("Sidenotes.updateSidenotePositions", "sidenotes.js", 1);

		/*  If we’re in footnotes mode (ie. the viewport is too narrow), then
			don’t do anything.
			*/
		if (Sidenotes.mediaQueries.viewportWidthBreakpoint.matches == false)
			return;

		//  Update the disposition of sidenotes within collapse blocks.
		Sidenotes.updateSidenotesInCollapseBlocks();

		//	Check for cut-off sidenotes.
		Sidenotes.sidenotes.forEach(sidenote => {
			/*  Check whether the sidenote is currently hidden (i.e., within a 
				currently-collapsed collapse block or similar). If so, skip it.
				*/
			if (sidenote.classList.contains("hidden")) {
				Sidenotes.hiddenSidenoteStorage.append(sidenote);
				return;
			}

			//  On which side should the sidenote go?
			let sidenoteNumber = Notes.noteNumberFromHash(sidenote.id);
			let side = null;
			       if (   Sidenotes.useLeftColumn()  == true
					   && Sidenotes.useRightColumn() == false) {
				//	Left.
				side = Sidenotes.sidenoteColumnLeft;
			} else if (   Sidenotes.useLeftColumn()  == false
					   && Sidenotes.useRightColumn() == true) {
				//	Right.
				side = Sidenotes.sidenoteColumnRight;
			} else if (   Sidenotes.useLeftColumn()  == true
					   && Sidenotes.useRightColumn() == true) {
				//	Odd - right; even - left.
				side = (sidenoteNumber % 2
						? Sidenotes.sidenoteColumnLeft
						: Sidenotes.sidenoteColumnRight);
			}

			//  Inject the sidenote into the column (provisionally).
			side.append(sidenote);

			/*  Mark sidenotes which are cut off vertically.
				*/
			let sidenoteOuterWrapper = sidenote.firstElementChild;
			sidenote.classList.toggle("cut-off", (sidenoteOuterWrapper.scrollHeight > sidenoteOuterWrapper.offsetHeight + 2));
		});

		/*  Determine proscribed vertical ranges (ie. bands of the page from which
			sidenotes are excluded, by the presence of, eg. a full-width table).
			*/
		let leftColumnBoundingRect = Sidenotes.sidenoteColumnLeft.getBoundingClientRect();
		let rightColumnBoundingRect = Sidenotes.sidenoteColumnRight.getBoundingClientRect();

		/*  Examine all potentially overlapping elements (ie. non-sidenote
			elements that may appear in, or extend into, the side columns).
			*/
		let proscribedVerticalRangesLeft = [ ];
		let proscribedVerticalRangesRight = [ ];
		document.querySelectorAll(Sidenotes.potentiallyOverlappingElementsSelectors.join(", ")).forEach(potentiallyOverlappingElement => {
			if (isWithinCollapsedBlock(potentiallyOverlappingElement))
				return;

			let elementBoundingRect = potentiallyOverlappingElement.getBoundingClientRect();

			if (!(   elementBoundingRect.left > leftColumnBoundingRect.right 
				  || elementBoundingRect.right < leftColumnBoundingRect.left))
				proscribedVerticalRangesLeft.push({ top: (elementBoundingRect.top - Sidenotes.sidenoteSpacing) - leftColumnBoundingRect.top,
													bottom: (elementBoundingRect.bottom + Sidenotes.sidenoteSpacing) - leftColumnBoundingRect.top,
													element: potentiallyOverlappingElement });

			if (!(   elementBoundingRect.left > rightColumnBoundingRect.right 
				  || elementBoundingRect.right < rightColumnBoundingRect.left))
				proscribedVerticalRangesRight.push({ top: (elementBoundingRect.top - Sidenotes.sidenoteSpacing) - rightColumnBoundingRect.top,
													 bottom: (elementBoundingRect.bottom + Sidenotes.sidenoteSpacing) - rightColumnBoundingRect.top,
													 element: potentiallyOverlappingElement });
		});

		//  The bottom edges of each column are also “proscribed vertical ranges”.
		proscribedVerticalRangesLeft.push({
			top:    Sidenotes.sidenoteColumnLeft.clientHeight,
			bottom: Sidenotes.sidenoteColumnLeft.clientHeight
		});
		proscribedVerticalRangesRight.push({
			top:    Sidenotes.sidenoteColumnRight.clientHeight,
			bottom: Sidenotes.sidenoteColumnRight.clientHeight
		});

		//	Sort and merge.
		[ proscribedVerticalRangesLeft, proscribedVerticalRangesRight ].forEach(ranges => {
			ranges.sort((rangeA, rangeB) => {
				return (rangeA.top - rangeB.top);
			});

			for (let i = 0; i < ranges.length - 1; i++) {
				let thisRange = ranges[i];
				let nextRange = ranges[i + 1];

				if (nextRange.top <= thisRange.bottom) {
					thisRange.bottom = nextRange.bottom;
					ranges.splice(i + 1, 1);
					i++;
				}
			}
		});

		/*	Remove sidenotes from page, so that we can set their positions
			without causing reflow. Store their layout heights (which cannot
			be retrieved in the normal way while the sidenotes aren’t part of
			the DOM).
		 */
		Sidenotes.sidenotes.forEach(sidenote => {
			sidenote.lastKnownHeight = sidenote.offsetHeight;
			sidenote.remove();
		});

		//	Clean up old layout cells, if any.
		[ Sidenotes.sidenoteColumnLeft, Sidenotes.sidenoteColumnRight ].forEach(column => {
			column.querySelectorAll(".sidenote-layout-cell").forEach(cell => cell.remove());
		});

		//	Construct new layout cells.
		let layoutCells = [ ];
		let sides = [ ];
		if (Sidenotes.useLeftColumn())
			sides.push([ Sidenotes.sidenoteColumnLeft, leftColumnBoundingRect, proscribedVerticalRangesLeft ]);
		if (Sidenotes.useRightColumn())
			sides.push([ Sidenotes.sidenoteColumnRight, rightColumnBoundingRect, proscribedVerticalRangesRight ]);
		sides.forEach(side => {
			let [ column, rect, ranges ] = side;
			let prevRangeBottom = 0;

			ranges.forEach(range => {
				let cell = newElement("DIV", {
					"class": "sidenote-layout-cell"
				}, {
					"sidenotes": [ ],
					"container": column,
					"room": (range.top - prevRangeBottom),
					"style": `top: ${prevRangeBottom + "px"}; height: ${(range.top - prevRangeBottom) + "px"}`
				});

				column.append(cell);
				cell.rect = cell.getBoundingClientRect();
				layoutCells.push(cell);

				prevRangeBottom = range.bottom;
			});
		});

		/*	Default position for a sidenote within a layout cell is vertically
			aligned with the footnote reference, or else at the top of the 
			cell, whichever is lower.
		 */
		let defaultNotePosInCellForCitation = (cell, citation) => {
			return Math.max(0, Math.round((citation.getBoundingClientRect().top - cell.rect.top) + 4));
		};

		//	Assign sidenotes to layout cells.
		for (citation of Sidenotes.citations) {
			let citationBoundingRect = citation.getBoundingClientRect();

			let sidenote = Sidenotes.counterpart(citation);

			/*  Is this sidenote even displayed? Or is it hidden (i.e., its
				citation is within a currently-collapsed collapse block)? If so,
				skip it.
				*/
			if (sidenote.classList.contains("hidden")) {
				Sidenotes.hiddenSidenoteStorage.append(sidenote);
				continue;
			}

			//	Get all the cells that the sidenote can fit into.
			let fittingLayoutCells = layoutCells.filter(cell => cell.room >= sidenote.lastKnownHeight);
			if (fittingLayoutCells.length == 0) {
				GWLog("TOO MUCH SIDENOTES. GIVING UP. :(", "sidenotes.js");
				Sidenotes.sidenotes.forEach(sidenote => {
					sidenote.remove();
				});
				return;
			}

			/*	These functions are used to sort layout cells by best fit for 
				placing the current sidenote.
			*/
			let vDistanceToCell = (cell) => {
				if (   citationBoundingRect.top > cell.rect.top 
					&& citationBoundingRect.top < cell.rect.bottom)
					return 0;
				return (citationBoundingRect.top < cell.rect.top
						? Math.abs(citationBoundingRect.top - cell.rect.top)
						: Math.abs(citationBoundingRect.top - cell.rect.bottom));
			};
			let hDistanceToCell = (cell) => {
				return Math.abs(citationBoundingRect.left - (cell.left + (cell.width / 2)));
			};
			let overlapWithNote = (cell, note) => {
				let notePosInCell = defaultNotePosInCellForCitation(cell, citation);

				let otherNoteCitation = Sidenotes.counterpart(note);
				let otherNotePosInCell = defaultNotePosInCellForCitation(cell, otherNoteCitation);

				return (   otherNotePosInCell > notePosInCell + sidenote.lastKnownHeight + Sidenotes.sidenoteSpacing
						|| notePosInCell      > otherNotePosInCell + note.lastKnownHeight + Sidenotes.sidenoteSpacing)
					   ? 0
					   : Math.max(notePosInCell + sidenote.lastKnownHeight + Sidenotes.sidenoteSpacing - otherNotePosInCell,
					   			  otherNotePosInCell + note.lastKnownHeight + Sidenotes.sidenoteSpacing - notePosInCell);
			};
			let cellCrowdedness = (cell) => {
				return cell.sidenotes.reduce((totalOverlap, note) => { return (totalOverlap + overlapWithNote(cell, note)); }, 0);
			};

			/*	We sort the fitting cells by vertical distance from the sidenote
				and crowdedness at the sidenote’s default location within the
				cell, and secondarily by horizontal distance from the sidenote.
			 */
			fittingLayoutCells.sort((cellA, cellB) => {
				return (   (  (vDistanceToCell(cellA) + cellCrowdedness(cellA)) 
							- (vDistanceToCell(cellB) + cellCrowdedness(cellB)))
						|| (hDistanceToCell(cellA) - hDistanceToCell(cellB)));
			});
			let closestFittingLayoutCell = fittingLayoutCells[0];

			//	Add the sidenote to the selected cell.
			closestFittingLayoutCell.room -= (sidenote.lastKnownHeight + Sidenotes.sidenoteSpacing);
			closestFittingLayoutCell.sidenotes.push(sidenote);
		};

		//	Function to compute distance between two successive sidenotes.
		let getDistance = (noteA, noteB) => {
			return (noteB.posInCell - (noteA.posInCell + noteA.lastKnownHeight + Sidenotes.sidenoteSpacing));
		};

		//	Position sidenotes within layout cells.
		layoutCells.forEach(cell => {
			if (cell.sidenotes.length == 0)
				return;

			//	Set all of the cell’s sidenotes to default positions.
			cell.sidenotes.forEach(sidenote => {
				let citation = Sidenotes.counterpart(sidenote);
				sidenote.posInCell = defaultNotePosInCellForCitation(cell, citation);
			});

			//	Sort the cell’s sidenotes vertically (secondarily by number).
			cell.sidenotes.sort((noteA, noteB) => {
				return (   (noteA.posInCell - noteB.posInCell)
						|| (parseInt(noteA.id.substr(2)) - parseInt(noteB.id.substr(2))));
			});

			//	Called in pushNotesUp().
			let shiftNotesUp = (noteIndexes, shiftUpDistance) => {
				noteIndexes.forEach(idx => {
					cell.sidenotes[idx].posInCell -= shiftUpDistance;
				});
			};

			//	Called immediately below.
			let pushNotesUp = (pushUpWhich, pushUpForce, bruteStrength = false) => {
				let roomToPush = pushUpWhich.first == 0
								 ? cell.sidenotes[pushUpWhich.first].posInCell
								 : Math.max(0, getDistance(cell.sidenotes[pushUpWhich.first - 1], cell.sidenotes[pushUpWhich.first]));

				let pushUpDistance = bruteStrength 
									 ? pushUpForce 
									 : Math.floor(pushUpForce / pushUpWhich.length);
				if (pushUpDistance <= roomToPush) {
					shiftNotesUp(pushUpWhich, pushUpDistance);
					return (pushUpForce - pushUpDistance);
				} else {
					shiftNotesUp(pushUpWhich, roomToPush);
					if (pushUpWhich.first == 0)
						return (pushUpForce - roomToPush);					

					pushUpWhich.splice(0, 0, pushUpWhich.first - 1);
					return pushNotesUp(pushUpWhich, (pushUpForce - roomToPush), bruteStrength);
				}
			};

			/*	Check each sidenote after the first for overlap with the one
				above it; if it overlaps, try pushing the sidenote(s) above it
				upward, and also shift the note itself downward.
			 */
			for (let i = 1; i < cell.sidenotes.length; i++) {
				let prevNote = cell.sidenotes[i - 1];
				let thisNote = cell.sidenotes[i];
				let nextNote = (i == cell.sidenotes.length - 1)
							   ? null
							   : cell.sidenotes[i + 1];

				let overlapAbove = Math.max(0, (-1 * getDistance(prevNote, thisNote)));
				if (overlapAbove == 0)
					continue;

				let pushUpForce = Math.round(overlapAbove / 2);
				thisNote.posInCell += ((overlapAbove - pushUpForce) + pushNotesUp([ (i - 1) ], pushUpForce));
			}

			/*	Check whether the lowest sidenote overlaps the cell’s bottom;
				if so, push it (and any sidenotes above it that it bumps into)
				upward.
			 */
			let overlapOfBottom = Math.max(0, (cell.sidenotes.last.posInCell + cell.sidenotes.last.lastKnownHeight) - parseInt(cell.style.height));
			if (overlapOfBottom > 0)
				pushNotesUp([ (cell.sidenotes.length - 1) ], overlapOfBottom, true);

			//	Set the sidenote positions via inline styles.
			cell.sidenotes.forEach(sidenote => {
				sidenote.style.top = Math.round(sidenote.posInCell) + "px";
			});

			//	Re-inject the sidenotes into the page.
			cell.append(...cell.sidenotes);
		});

		//  Un-hide the sidenote columns.
		Sidenotes.sidenoteColumnLeft.style.visibility = "";
		Sidenotes.sidenoteColumnRight.style.visibility = "";

		//	Fire event.
		GW.notificationCenter.fireEvent("Sidenotes.sidenotePositionsDidUpdate");
	},

	/*  Destroys the HTML structure of the sidenotes.
		*/
	deconstructSidenotes: () => {
		GWLog("Sidenotes.deconstructSidenotes", "sidenotes.js", 1);

		Sidenotes.sidenotes = null;
		Sidenotes.citations = null;

		if (Sidenotes.sidenoteColumnLeft)
			Sidenotes.sidenoteColumnLeft.remove();
		Sidenotes.sidenoteColumnLeft = null;

		if (Sidenotes.sidenoteColumnRight)
			Sidenotes.sidenoteColumnRight.remove();
		Sidenotes.sidenoteColumnRight = null;

		if (Sidenotes.hiddenSidenoteStorage)
			Sidenotes.hiddenSidenoteStorage.remove();
		Sidenotes.hiddenSidenoteStorage = null;
	},

	/*  Constructs the HTML structure, and associated listeners and auxiliaries,
		of the sidenotes.
		*/
	constructSidenotes: () => {
		GWLog("Sidenotes.constructSidenotes", "sidenotes.js", 1);

		/*  Do nothing if constructSidenotes() somehow gets run extremely early 
			in the page load process.
			*/
		let markdownBody = document.querySelector("#markdownBody");
		if (markdownBody == null)
			return;

		//	Destroy before creating.
		Sidenotes.deconstructSidenotes();

		//  Add the sidenote columns.
		Sidenotes.sidenoteColumnLeft = newElement("DIV", { "id": "sidenote-column-left" });
		Sidenotes.sidenoteColumnRight = newElement("DIV", { "id": "sidenote-column-right" });
		[ Sidenotes.sidenoteColumnLeft, Sidenotes.sidenoteColumnRight ].forEach(column => {
			column.classList.add("footnotes");
			column.style.visibility = "hidden";
			markdownBody.append(column);
		});

		//	Add the hidden sidenote storage.
		markdownBody.append(Sidenotes.hiddenSidenoteStorage = newElement("DIV", {
			"id": "hidden-sidenote-storage", 
			"class": "footnotes",
			"style": "display:none" 
		}));

		/*  Create and inject the sidenotes.
			*/
		Sidenotes.sidenotes = [ ];
		//  The footnote references (citations).
		Sidenotes.citations = Array.from(document.querySelectorAll("a.footnote-ref"));

		//	If there are no footnotes, we’re done.
		if (Sidenotes.citations.length == 0)
			return;

		Sidenotes.citations.forEach(citation => {
			let noteNumber = Notes.noteNumberFromHash(citation.hash);

			//  Create the sidenote outer containing block...
			let sidenote = newElement("DIV", { "class": "sidenote", "id": `sn${noteNumber}` });

			//  Wrap the contents of the footnote in two wrapper divs...
			let referencedFootnote = document.querySelector(`#fn${noteNumber}`);
			sidenote.innerHTML = `<div class="sidenote-outer-wrapper"><div class="sidenote-inner-wrapper">` 
							   + (referencedFootnote 
							   	  ? referencedFootnote.innerHTML 
							   	  : "Loading sidenote contents, please wait…")
							   + `</div></div>`;
			sidenote.outerWrapper = sidenote.querySelector(".sidenote-outer-wrapper");
			sidenote.innerWrapper = sidenote.querySelector(".sidenote-inner-wrapper");

			/*  Create & inject the sidenote self-links (ie. boxed sidenote 
				numbers).
				*/
			sidenote.append(newElement("A", { 
				"class": "sidenote-self-link",
				"href": `#sn${noteNumber}` 
			}, { 
				"textContent": noteNumber 
			}));

			//	Remove footnote self-link.
			sidenote.querySelector(".footnote-self-link")?.remove();

			//	Add listener to update sidenote positions when media loads.
			sidenote.querySelectorAll("figure img, figure video").forEach(mediaElement => {
				mediaElement.addEventListener("load", (event) => {
					Sidenotes.updateSidenotePositionsIfNeeded();
				}, { once: true });
			});

			//  Add the sidenote to the sidenotes array...
			Sidenotes.sidenotes.push(sidenote);

			//	Inject the sidenote into the page.
			Sidenotes.hiddenSidenoteStorage.append(sidenote);
		});

		/*  Bind sidenote mouse-hover events.
			*/
		Sidenotes.citations.forEach(citation => {
			let sidenote = Sidenotes.counterpart(citation);

			//	Unbind existing events, if any.
			if (sidenote.onSidenoteMouseEnterHighlightCitation)
				sidenote.removeEventListener("mouseenter", sidenote.onSidenoteMouseEnterHighlightCitation);
			if (sidenote.onSidenoteMouseLeaveUnhighlightCitation)
				sidenote.removeEventListener("mouseleave", sidenote.onSidenoteMouseLeaveUnhighlightCitation);

			if (citation.onCitationMouseEnterSlideSidenote)
				citation.removeEventListener("mouseenter", citation.onCitationMouseEnterSlideSidenote);
			if (sidenote.onSidenoteMouseEnterSlideSidenote)
				sidenote.removeEventListener("mouseenter", sidenote.onSidenoteMouseEnterSlideSidenote);
			if (sidenote.onSidenoteMouseLeaveUnslideSidenote)
				sidenote.removeEventListener("mouseleave", sidenote.onSidenoteMouseLeaveUnslideSidenote);

			if (sidenote.scrollListener)
				sidenote.outerWrapper.removeEventListener("scroll", sidenote.scrollListener);

			//	Bind new events.
			sidenote.addEventListener("mouseenter", sidenote.onSidenoteMouseEnterHighlightCitation = (event) => {
				citation.classList.toggle("highlighted", true);
			});
			sidenote.addEventListener("mouseleave", sidenote.onSidenoteMouseLeaveUnhighlightCitation = (event) => {
				citation.classList.toggle("highlighted", false);
			});

			citation.addEventListener("mouseenter", citation.onCitationMouseEnterSlideSidenote = (event) => {
				Sidenotes.putAllSidenotesBack(sidenote);
				requestAnimationFrame(() => {
					Sidenotes.slideSidenoteIntoView(sidenote, true);
				});
			});
			sidenote.addEventListener("mouseenter", sidenote.onSidenoteMouseEnterSlideSidenote = (event) => {
				Sidenotes.putAllSidenotesBack(sidenote);
				requestAnimationFrame(() => {
					Sidenotes.slideSidenoteIntoView(sidenote, false);
				});
			});
			sidenote.addEventListener("mouseleave", sidenote.onSidenoteMouseLeaveUnslideSidenote = (event) => {
				Sidenotes.putSidenoteBack(sidenote);
			});

			sidenote.scrollListener = addScrollListener((event) => {
				sidenote.classList.toggle("hide-more-indicator", sidenote.outerWrapper.scrollTop + sidenote.outerWrapper.clientHeight == sidenote.outerWrapper.scrollHeight);
			}, null, { }, sidenote.outerWrapper);
		});

		GW.notificationCenter.fireEvent("Sidenotes.sidenotesDidConstruct");

		//	Fire events.
		GW.notificationCenter.fireEvent("GW.contentDidLoad", {
			source: "Sidenotes.constructSidenotes",
			container: Sidenotes.hiddenSidenoteStorage,
			document: document,
			loadLocation: location
		});
		GW.notificationCenter.fireEvent("GW.contentDidInject", {
			source: "Sidenotes.constructSidenotes",
			container: Sidenotes.hiddenSidenoteStorage,
			document: document,
			loadLocation: location,
			flags: 0
		});
	},

	cleanup: () => {
		GWLog("Sidenotes.cleanup", "sidenotes.js", 1);

		/*	Deactivate active media queries.
			*/
		cancelDoWhenMatchMedia("Sidenotes.rewriteHashForCurrentMode");
		cancelDoWhenMatchMedia("Sidenotes.rewriteCitationTargetsForCurrentMode");
		cancelDoWhenMatchMedia("Sidenotes.addOrRemoveEventHandlersForCurrentMode");

		/*	Remove sidenotes & auxiliaries from HTML.
			*/
		Sidenotes.deconstructSidenotes();

		GW.notificationCenter.fireEvent("Sidenotes.cleanupDidComplete");
	},

	/*  Q:  Why is this setup function so long and complex?
		A:  In order to properly handle all of the following:

		1.  The two different modes (footnote popups vs. sidenotes)
		2.  The interactions between sidenotes and collapse blocks
		3.  Linking to footnotes/sidenotes
		4.  Loading a URL that links to a footnote/sidenote
		5.  Changes in the viewport width dynamically altering all of the above

		… and, of course, correct layout of the sidenotes, even in tricky cases
		where the citations are densely packed and the sidenotes are long.
		*/
	setup: () => {
		GWLog("Sidenotes.setup", "sidenotes.js", 1);

		/*  If the page was loaded with a hash that points to a footnote, but
			sidenotes are enabled (or vice-versa), rewrite the hash in 
			accordance with the current mode (this will also cause the page to 
			end up scrolled to the appropriate element - footnote or sidenote). 
			Add an active media query to rewrite the hash whenever the viewport 
			width media query changes.
			*/
		doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "Sidenotes.rewriteHashForCurrentMode", (mediaQuery) => {
			let regex = new RegExp(mediaQuery.matches ? "^#fn[0-9]+$" : "^#sn[0-9]+$");
			let prefix = (mediaQuery.matches ? "#sn" : "#fn");

			if (location.hash.match(regex)) {
				relocate(prefix + Notes.noteNumberFromHash());

				//	Update targeting.
				if (mediaQuery.matches)
					Sidenotes.updateTargetCounterpart();
				else
					updateFootnoteTargeting();
			}
		}, null, (mediaQuery) => {
			if (location.hash.match(/^#sn[0-9]/)) {
				relocate("#fn" + Notes.noteNumberFromHash());

				//	Update targeting.
				updateFootnoteTargeting();
			}
		});

		/*	We do not bother to construct sidenotes on mobile clients, and so
			the rest of this is also irrelevant.
			*/
		if (GW.isMobile())
			return;

		/*	Update the margin note style, and add event listener to re-update it
			when the viewport width changes. Also add event handler to update
			margin note style in transcluded content and pop-frames.
			*/
		addContentLoadHandler((info) => {
			doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "Sidenotes.updateMarginNoteStyleForCurrentMode", (mediaQuery) => {
				Sidenotes.setMarginNoteStyle(info);
			});
		}, "rewrite", (info) => info.container == document.body, true);
		addContentInjectHandler(Sidenotes.setMarginNoteStyle, ">rewrite");

		/*	When an anchor link is clicked that sets the hash to its existing
			value, weird things happen. In particular, weird things happen with
			citations and sidenotes. We must prevent that, by updating state
			properly when that happens. (No ‘hashchange’ event is fired in this
			case, so we cannot depend on the ‘GW.hashDidChange’ event handler.)
		 */
		addContentInjectHandler(Sidenotes.addFauxHashChangeEventsToNoteMetaLinks = (eventInfo) => {
			let selector = [
				"a.footnote-ref",
				"a.sidenote-self-link",
				".sidenote a.footnote-back"
			].join(", ");

			eventInfo.container.querySelectorAll(selector).forEach(link => {
				link.addActivateEvent((event) => {
					if (link.hash == location.hash)
						Sidenotes.updateStateAfterHashChange();
				});
			});
		}, "eventListeners", (info) => info.document == document);

		/*  In footnote mode (ie. on viewports too narrow to support sidenotes),
			footnote reference links (i.e., citations) should point down to 
			footnotes (this is the default state). But in sidenote mode, 
			footnote reference links should point to sidenotes.

			We therefore rewrite all footnote reference links appropriately to
			the current mode (based on viewport width).

			We also add an active media query to rewrite the links if a change
			in viewport width results in switching modes, as well as an event
			handler to rewrite footnote reference links in transcluded content.
			*/
		doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "Sidenotes.rewriteCitationTargetsForCurrentMode", (mediaQuery) => {
			document.querySelectorAll("a.footnote-ref").forEach(citation => {
				citation.href = (mediaQuery.matches ? "#sn" : "#fn") + Notes.noteNumberFromHash(citation.hash);
			});
		}, null, (mediaQuery) => {
			document.querySelectorAll("a.footnote-ref").forEach(citation => {
				citation.href = "#fn" + Notes.noteNumberFromHash(citation.hash);
			});
		});

		addContentLoadHandler(Sidenotes.rewriteCitationTargetsInLoadedContent = (eventInfo) => {
			document.querySelectorAll("a.footnote-ref").forEach(citation => {
				if (citation.pathname == location.pathname)
					citation.href = (Sidenotes.mediaQueries.viewportWidthBreakpoint.matches ? "#sn" : "#fn") 
									+ Notes.noteNumberFromHash(citation.hash);
			});
		}, "rewrite", (info) => info.document == document);

		/*	What happens if the page loads with a URL hash that points to a 
			sidenote or footnote or citation? We need to scroll appropriately,
			and do other adjustments, just as we do when the hash updates.
		 */
		GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotePositionsDidUpdate", Sidenotes.updateStateAfterHashChange = (info) => {
			GWLog("Sidenotes.updateStateAfterHashChange", "sidenotes.js", 1);

			//	Update highlighted state of sidenote and citation, if need be.
			Sidenotes.updateTargetCounterpart();

			/*	If hash targets a sidenote, reveal corresponding citation; and
				vice-versa. Scroll everything into view properly.
			 */
			if (location.hash.match(/#sn[0-9]/)) {
				let citation = document.querySelector("#fnref" + Notes.noteNumberFromHash());
				if (citation == null)
					return;

				let sidenote = Sidenotes.counterpart(citation);

				revealElement(citation, false);

				Sidenotes.slideLockSidenote(sidenote);

				requestAnimationFrame(() => {
					scrollElementIntoView(sidenote, ((-1 * Sidenotes.sidenotePadding) - 1));

					Sidenotes.unSlideLockSidenote(sidenote);
				});
			} else if (location.hash.match(/#fnref[0-9]/)) {
				let citation = getHashTargetedElement();
				let sidenote = Sidenotes.counterpart(citation);

				Sidenotes.slideLockSidenote(sidenote);

				requestAnimationFrame(() => {
					let sidenoteRect = sidenote.getBoundingClientRect();
					let citationRect = citation.getBoundingClientRect();
					if (   sidenoteRect.top < Sidenotes.sidenotePadding + 1
						&& citationRect.bottom + (-1 * (sidenoteRect.top - Sidenotes.sidenotePadding)) < window.innerHeight)
						scrollElementIntoView(sidenote, ((-1 * Sidenotes.sidenotePadding) - 1));

					Sidenotes.unSlideLockSidenote(sidenote);
				});
			}

			/*	Hide mode selectors, as they would otherwise overlap a 
				sidenote that’s on the top-right.
			 */
			if (Notes.noteNumberFromHash() > "")
				Sidenotes.hideInterferingUIElements();
		}, { once: true });

		//	Add event listeners, and the switch between modes.
		doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "Sidenotes.addOrRemoveEventHandlersForCurrentMode", (mediaQuery) => {
			doWhenPageLayoutComplete(Sidenotes.updateSidenotePositionsIfNeeded);

			/*  After the hash updates, properly highlight everything, if needed.
				Also, if the hash points to a sidenote whose citation is in a
				collapse block, expand it and all collapse blocks enclosing it.
				*/
			GW.notificationCenter.addHandlerForEvent("GW.hashDidChange", Sidenotes.updateStateAfterHashChange);

			/*	Add event handler to (asynchronously) recompute sidenote positioning
				when full-width media lazy-loads.
				*/
			GW.notificationCenter.addHandlerForEvent("Rewrite.fullWidthMediaDidLoad", Sidenotes.updateSidenotePositionsAfterFullWidthMediaDidLoad = (info) => {
				if (isWithinCollapsedBlock(info.mediaElement))
					return;

				doWhenPageLayoutComplete(Sidenotes.updateSidenotePositionsIfNeeded);
			});

			/*	Add event handler to (asynchronously) recompute sidenote positioning
				when collapse blocks are expanded/collapsed.
				*/
			GW.notificationCenter.addHandlerForEvent("Collapse.collapseStateDidChange", Sidenotes.updateSidenotePositionsAfterCollapseStateDidChange = (info) => {
				doWhenPageLayoutComplete(Sidenotes.updateSidenotePositionsIfNeeded);
			}, { condition: (info) => (info.collapseBlock.closest("#markdownBody") != null) });

			/*	Add event handler to (asynchronously) recompute sidenote positioning
				when new content is loaded (e.g. via transclusion).
				*/
			GW.notificationCenter.addHandlerForEvent("Rewrite.contentDidChange", Sidenotes.updateSidenotePositionsAfterContentDidChange = (info) => {
				doWhenPageLayoutComplete(Sidenotes.updateSidenotePositionsIfNeeded);
			}, { condition: (info) => (info.document == document) });

			/*  Add a resize listener so that sidenote positions are recalculated when
				the window is resized.
				*/
			addWindowResizeListener(Sidenotes.windowResized = (event) => {
				GWLog("Sidenotes.windowResized", "sidenotes.js", 2);

				doWhenPageLayoutComplete(Sidenotes.updateSidenotePositionsIfNeeded);
			}, "Sidenotes.recalculateSidenotePositionsOnWindowResize");

			/*	Add handler to bind more sidenote-slide events if more 
				citations are injected (e.g., in a popup).
			 */
			addContentInjectHandler(Sidenotes.bindAdditionalSidenoteSlideEvents = (eventInfo) => {
				eventInfo.container.querySelectorAll("a.footnote-ref").forEach(citation => {
					if (citation.pathname != location.pathname)
						return;

					let sidenote = Sidenotes.counterpart(citation);
					citation.addEventListener("mouseenter", citation.onCitationMouseEnterSlideSidenote = (event) => {
						Sidenotes.putAllSidenotesBack(sidenote);
						requestAnimationFrame(() => {
							Sidenotes.slideSidenoteIntoView(sidenote, true);
						});
					});
				});
			}, "eventListeners", (info) => info.document != document);

			/*	Add a scroll listener to un-slide all sidenotes on scroll.
			 */
			addScrollListener((event) => {
				Sidenotes.putAllSidenotesBack();
			}, "Sidenotes.unSlideSidenotesOnScroll", { defer: true });
		}, (mediaQuery) => {
			/*	Deactivate event handlers.
				*/
			GW.notificationCenter.removeHandlerForEvent("GW.hashDidChange", Sidenotes.updateStateAfterHashChange);
			GW.notificationCenter.removeHandlerForEvent("Rewrite.contentDidChange", Sidenotes.updateSidenotePositionsAfterContentDidChange);
			GW.notificationCenter.removeHandlerForEvent("Rewrite.fullWidthMediaDidLoad", Sidenotes.updateSidenotePositionsAfterFullWidthMediaDidLoad);
			GW.notificationCenter.removeHandlerForEvent("Collapse.collapseStateDidChange", Sidenotes.updateSidenotePositionsAfterCollapseStateDidChange);
			GW.notificationCenter.removeHandlerForEvent("GW.contentDidInject", Sidenotes.bindAdditionalSidenoteSlideEvents);
			removeScrollListener("Sidenotes.unSlideSidenotesOnScroll");
			removeWindowResizeListener("Sidenotes.recalculateSidenotePositionsOnWindowResize");
		}, (mediaQuery) => {
			/*	Deactivate event handlers.
				*/
			GW.notificationCenter.removeHandlerForEvent("GW.hashDidChange", Sidenotes.updateStateAfterHashChange);
			GW.notificationCenter.removeHandlerForEvent("Rewrite.contentDidChange", Sidenotes.updateSidenotePositionsAfterContentDidChange);
			GW.notificationCenter.removeHandlerForEvent("Rewrite.fullWidthMediaDidLoad", Sidenotes.updateSidenotePositionsAfterFullWidthMediaDidLoad);
			GW.notificationCenter.removeHandlerForEvent("Collapse.collapseStateDidChange", Sidenotes.updateSidenotePositionsAfterCollapseStateDidChange);
			GW.notificationCenter.removeHandlerForEvent("GW.contentDidInject", Sidenotes.bindAdditionalSidenoteSlideEvents);
			removeScrollListener("Sidenotes.unSlideSidenotesOnScroll");
			removeWindowResizeListener("Sidenotes.recalculateSidenotePositionsOnWindowResize");
		});

		//	Once the sidenotes are constructed, lay them out.
		GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", (info) => {
			//	Lay out sidenotes once page layout is complete.
			doWhenPageLayoutComplete(() => {
				Sidenotes.updateSidenotePositionsIfNeeded();

				//	Add listener to lay out sidenotes when they are re-constructed.
				GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", (info) => {
					//	Update highlighted state of sidenote and citation, if need be.
					Sidenotes.updateTargetCounterpart();

					//	Update sidenote positions.
					Sidenotes.updateSidenotePositionsIfNeeded();
				});
			});
		}, { once: true });

		/*  Construct the sidenotes whenever content is injected into the main
			page (including the initial page load).
			*/
		addContentInjectHandler(Sidenotes.constructSidenotesWhenMainPageContentDidInject = (eventInfo) => {
			GWLog("Sidenotes.constructSidenotesWhenMainPageContentDidInject", "sidenotes.js", 1);

			if (eventInfo.container == document.body) {
				Sidenotes.constructSidenotes();
			} else {
				Sidenotes.sidenotesNeedConstructing = true;
				requestIdleCallback(() => {
					if (Sidenotes.sidenotesNeedConstructing == true) {
						Sidenotes.constructSidenotes();
						Sidenotes.sidenotesNeedConstructing = false;
					}
				});
			}
		}, "rewrite", (info) => (   info.document == document
								 && info.source != "Sidenotes.constructSidenotes"));

		GW.notificationCenter.fireEvent("Sidenotes.setupDidComplete");
	},

	hideInterferingUIElements: () => {
		requestAnimationFrame(() => {
			setTimeout(() => {
				//	Page toolbar.
				GW.pageToolbar.toggleCollapseState(true);
				GW.pageToolbar.fade();

				//	Back-to-top link.
				GW.backToTop.classList.toggle("hidden", true)
			}, 25);
		});
	},

	/**************/
	/*	Slidenotes.
	 */

	displacedSidenotes: [ ],

	/*	If the sidenote is offscreen, slide it onto the screen.
	 */
	slideSidenoteIntoView: (sidenote, toCitation) => {
		GWLog("Sidenotes.slideSidenoteIntoView", "sidenotes.js", 3);

		Sidenotes.hideInterferingUIElements();

		if (sidenote.style.transform == "none")
			return;

		let minDistanceFromScreenEdge = Sidenotes.sidenotePadding + 1.0;

		let sidenoteRect = sidenote.getBoundingClientRect();
		if (   sidenoteRect.top >= minDistanceFromScreenEdge
			&& sidenoteRect.bottom <= window.innerHeight - minDistanceFromScreenEdge)
			return;

		let newSidenoteTop = sidenoteRect.top;
		if (toCitation) {
			let citationRect = Sidenotes.counterpart(sidenote).getBoundingClientRect()

			//	Down to citation.
			newSidenoteTop = Math.max(sidenoteRect.top, minDistanceFromScreenEdge, citationRect.top);

			//	Up to citation.
			newSidenoteTop = Math.min(newSidenoteTop + sidenoteRect.height,
									  window.innerHeight - minDistanceFromScreenEdge,
									  citationRect.top + sidenoteRect.height)
						   - sidenoteRect.height;

			//	Down to viewport top.
			newSidenoteTop = Math.max(newSidenoteTop, minDistanceFromScreenEdge);
		} else {
			//	Down to viewport top.
			newSidenoteTop = Math.max(sidenoteRect.top, minDistanceFromScreenEdge);

			//	Up to viewport bottom.
			newSidenoteTop = Math.min(newSidenoteTop + sidenoteRect.height, 
									  window.innerHeight - minDistanceFromScreenEdge) 
						   - sidenoteRect.height;		
		}

		let delta = Math.round(newSidenoteTop - sidenoteRect.top);
		if (delta) {
			sidenote.style.transform = `translateY(${delta}px)`;
			sidenote.classList.toggle("displaced", true);
			if (Sidenotes.displacedSidenotes.includes(sidenote) == false)
				Sidenotes.displacedSidenotes.push(sidenote);
		}
	},

	/*	Un-slide a slid-onto-the-screen sidenote.
	 */
	putSidenoteBack: (sidenote) => {
		GWLog("Sidenotes.putSidenoteBack", "sidenotes.js", 3);

		if (sidenote.style.transform == "none")
			return;

		sidenote.style.transform = "";
		sidenote.classList.toggle("displaced", false);
	},

	/*	Un-slide all sidenotes (possibly except one).
	 */
	putAllSidenotesBack: (exceptOne = null) => {
		GWLog("Sidenotes.putAllSidenotesBack", "sidenotes.js", 3);

		Sidenotes.displacedSidenotes.forEach(sidenote => {
			if (sidenote == exceptOne)
				return;

			Sidenotes.putSidenoteBack(sidenote);
		});
		Sidenotes.displacedSidenotes = exceptOne ? [ exceptOne ] : [ ];
	},

	/*	Instantly un-slide sidenote and make it un-slidable.
	 */
	slideLockSidenote: (sidenote) => {
		GWLog("Sidenotes.slideLockSidenote", "sidenotes.js", 3);

		sidenote.style.transition = "none";
		sidenote.style.transform = "none";
		sidenote.classList.toggle("displaced", false);
	},

	/*	Instantly un-slide sidenote and make it slidable.
	 */
	unSlideLockSidenote: (sidenote) => {
		GWLog("Sidenotes.unSlideLockSidenote", "sidenotes.js", 3);

		sidenote.style.transform = "";
		sidenote.style.transition = "";
		sidenote.classList.toggle("displaced", false);
	},
};

GW.notificationCenter.fireEvent("Sidenotes.didLoad");

//  LET... THERE... BE... SIDENOTES!!!
Sidenotes.setup();
/* Image-focus.js */
/* Written by Obormot, 15 February 2019 */
/* License: GPL (derivative work of https://www.pmwiki.org/wiki/Cookbook/ImgFocus ) */
/* Lightweight dependency-free JavaScript library for "click to focus/zoom" images in HTML web pages. Originally coded for Obormot.net / GreaterWrong.com. */

ImageFocus = {
	/****************/
	/* Configuration.
	 ****************/

	contentImagesSelector: [
		".markdownBody figure img"
	].join(", "),

	excludedContainerElementsSelector: [
		"a",
		"button",
		"figure.image-focus-not"
	].join(", "),

	imageGalleryInclusionTest: (image) => {
		return (   image.closest("#markdownBody") != null
				&& image.closest(".footnotes") == null
				&& image.classList.contains("page-thumbnail") == false);
	},

	shrinkRatio: 0.975,

	hideUITimerDuration: 1500,

	dropShadowFilterForImages: " drop-shadow(10px 10px 10px #000) drop-shadow(0 0 10px #444)",

	/*****************/
	/* Infrastructure.
	 *****************/

	imageFocusUIElementsSelector: [
		".slideshow-button",
		".help-overlay",
		".image-number",
		".caption"
	].join(", "),

	focusableImagesSelector: null,
	focusedImageSelector: null,
	galleryImagesSelector: null,

	hideUITimer: null,

	overlay: null,

	mouseLastMovedAt: 0,

	currentlyFocusedImage: null,
	
	imageInFocus: null,

	/************/
	/* Functions.
	 ************/

	setup: () => {
		GWLog("ImageFocus.setup", "image-focus.js", 1);

		//  Create the image focus overlay.
		ImageFocus.overlay = addUIElement(`<div id="image-focus-overlay">
			<div class="help-overlay">
				<p class="slideshow-help-text"><strong>Arrow keys:</strong> Next/previous image</p>
				<p><strong>Escape</strong> or <strong>click</strong>: Hide zoomed image</p>
				<p><strong>Space bar:</strong> Reset image size & position</p>
				<p><strong>Scroll</strong> to zoom in/out</p>
				<p>(When zoomed in, <strong>drag</strong> to pan;<br /><strong>double-click</strong> to reset size & position)</p>
			</div>
			<div class="image-number"></div>
			<div class="slideshow-buttons">
				<button type="button" class="slideshow-button previous" tabindex="-1" title="Previous image">
					${(GW.svg("chevron-left-solid"))}
				</button>
				<button type="button" class="slideshow-button next" tabindex="-1" title="Next image">
					${(GW.svg("chevron-right-solid"))}
				</button>
			</div>
			<div class="caption"></div>
			<div class="loading-spinner">
				${(GW.svg("circle-notch-light"))}
			</div>
		</div>`);

		//  On orientation change, reset the size & position.
		GW.mediaQueries.portraitOrientation.addListener((event) => { requestAnimationFrame(ImageFocus.resetFocusedImagePosition); });

		//  Add click listeners to the buttons.
		ImageFocus.overlay.querySelectorAll(".slideshow-button").forEach(button => {
			button.addActivateEvent(ImageFocus.slideshowButtonClicked = (event) => {
				GWLog("ImageFocus.slideshowButtonClicked", "image-focus.js", 2);

				ImageFocus.focusNextImage(event.target.classList.contains("next"));
				ImageFocus.cancelImageFocusHideUITimer();
				event.target.blur();
			});
		});

		//	Add listeners to help overlay.
		let helpOverlay = ImageFocus.overlay.querySelector(".help-overlay");
		if (GW.isMobile()) {
			helpOverlay.addEventListener("click", (event) => {
				helpOverlay.classList.toggle("open");
			});
		} else {
			helpOverlay.addEventListener("mouseenter", (event) => {
				helpOverlay.classList.add("open");
			});
			helpOverlay.addEventListener("mouseleave", (event) => {
				helpOverlay.classList.remove("open");
			});
		}

		//  UI starts out hidden.
		ImageFocus.hideImageFocusUI();

		//	Selector-suffixing function.
		function suffixedSelector(selector, suffix) {
			return selector.split(", ").map(part => part + suffix).join(", ");
		}

		/*	Create auxiliary selectors by suffixing provided content images
			selector with appropriate classes.
		 */
		ImageFocus.focusableImagesSelector = suffixedSelector(ImageFocus.contentImagesSelector, ".focusable");
		ImageFocus.focusedImageSelector = suffixedSelector(ImageFocus.contentImagesSelector, ".focused");
		ImageFocus.galleryImagesSelector = suffixedSelector(ImageFocus.contentImagesSelector, ".gallery-image");

        //  Add handler to set up events for images in injected content.
        addContentInjectHandler(ImageFocus.processImagesOnContentInject = (eventInfo) => {
            GWLog("ImageFocus.processImagesOnContentInject", "image-focus.js", 2);

            ImageFocus.processImagesWithin(eventInfo.container);

			//	If this content is (or is being loaded into) the main page...
			if (eventInfo.document == document) {
				//  Count how many images there are in the page, and set the “… of X” label to that.
				ImageFocus.overlay.querySelector(".image-number").dataset.numberOfImages = document.querySelectorAll(ImageFocus.galleryImagesSelector).length;

				//  Accesskey-L starts the slideshow.
				(document.querySelector(ImageFocus.galleryImagesSelector)||{}).accessKey = "l";
			}

			//	Fire targets-processed event.
			GW.notificationCenter.fireEvent("ImageFocus.imagesDidProcessOnContentInject", {
				source: "ImageFocus.processImagesOnContentInject",
				container: eventInfo.container,
				document: eventInfo.document
			});
        }, "eventListeners");

		//	Add handler to focus image on hashchange event.
		GW.notificationCenter.addHandlerForEvent("GW.hashDidChange", (info) => {
			ImageFocus.focusImageSpecifiedByURL();
		});

        //  Fire setup-complete event.
		GW.notificationCenter.fireEvent("ImageFocus.setupDidComplete");
	},

	processImagesWithin: (container) => {
		GWLog("ImageFocus.processImagesWithin", "image-focus.js", 1);

		/*	Add ‘focusable’ class to all focusable images; add ‘gallery-image’
			class to all focusable images that are to be included in the main
			image gallery.
		 */
		container.querySelectorAll(ImageFocus.contentImagesSelector).forEach(image => {
			if (image.closest(ImageFocus.excludedContainerElementsSelector))
				return;

			image.classList.add("focusable");

			if (ImageFocus.imageGalleryInclusionTest(image))
				image.classList.add("gallery-image");
		});

		//  Add the listener to all focusable images.
		container.querySelectorAll(ImageFocus.focusableImagesSelector).forEach(image => {
			image.addEventListener("click", ImageFocus.imageClickedToFocus);
		});

		//  Wrap all focusable images in a span.
		container.querySelectorAll(ImageFocus.focusableImagesSelector).forEach(image => {
			wrapElement(image, "image-wrapper focusable", "SPAN");
		});
	},

	preloadImage: (image) => {
		if (image.naturalWidth > 0)
			return;

		image.loading = "eager";
		image.decoding = "sync";
	},

	focusImage: (imageToFocus, scrollToImage = true) => {
		GWLog("ImageFocus.focusImage", "image-focus.js", 1);

		//	Show overlay.
		ImageFocus.enterImageFocus();

		//	Show UI.
		ImageFocus.unhideImageFocusUI();

		//	Unfocus currently focused image, if any.
		ImageFocus.unfocusImage();

		//	Focus new image.
		imageToFocus.classList.toggle("focused", true);

		/*	If the new image is part of the main image gallery (i.e., if we are
			in gallery mode, rather than single-image mode)...
		 */
		if (imageToFocus.classList.contains("gallery-image")) {
			//	Update slideshow state.
			let lastFocusedImage = document.querySelector("img.last-focused");
			if (lastFocusedImage) {
				lastFocusedImage.classList.remove("last-focused");
				lastFocusedImage.removeAttribute("accesskey");
			}

			//  Set state of next/previous buttons.
			let images = document.querySelectorAll(ImageFocus.galleryImagesSelector);
			let indexOfFocusedImage = ImageFocus.getIndexOfFocusedImage();
			ImageFocus.overlay.querySelector(".slideshow-button.previous").disabled = (indexOfFocusedImage == 0);
			ImageFocus.overlay.querySelector(".slideshow-button.next").disabled = (indexOfFocusedImage == images.length - 1);

			//  Set the image number.
			ImageFocus.overlay.querySelector(".image-number").textContent = (indexOfFocusedImage + 1);

			//  Replace the hash.
			if (!location.hash.startsWith("#if_slide_"))
				ImageFocus.savedHash = location.hash;
			relocate("#if_slide_" + (indexOfFocusedImage + 1));

			//	Also preload the next and previous images.
			if (indexOfFocusedImage > 0)
				ImageFocus.preloadImage(images[indexOfFocusedImage - 1]);
			if (indexOfFocusedImage < images.length - 1)
				ImageFocus.preloadImage(images[indexOfFocusedImage + 1]);
		}

		//	Save reference to newly focused image.
		ImageFocus.currentlyFocusedImage = imageToFocus;

		//	Scroll to focused image, if need be.
		if (scrollToImage)
			revealElement(ImageFocus.currentlyFocusedImage);

		//  Create the focused version of the image.
		ImageFocus.imageInFocus = imageToFocus.cloneNode(true);
		ImageFocus.imageInFocus.loading = "eager";
		ImageFocus.imageInFocus.decoding = "sync";
		ImageFocus.imageInFocus.style = "";
		ImageFocus.imageInFocus.style.filter = imageToFocus.style.filter + ImageFocus.dropShadowFilterForImages;
		ImageFocus.imageInFocus.removeAttribute("title");

		//	Allow for styling based on loading state.
		ImageFocus.imageInFocus.classList.add("loading");
		ImageFocus.imageInFocus.addEventListener("load", (event) => {
			event.target.classList.remove("loading");
		}, { once: true });

		//  Add the image to the overlay.
		ImageFocus.overlay.insertBefore(ImageFocus.imageInFocus, ImageFocus.overlay.querySelector(".loading-spinner"));

		//  Set image to default size and position.
		ImageFocus.resetFocusedImagePosition(true);

		//  If image is bigger than viewport, it’s draggable.
		ImageFocus.imageInFocus.addEventListener("mousedown", ImageFocus.imageMouseDown);

		//  If image is bigger than viewport, double-click resets size/position.
		ImageFocus.imageInFocus.addEventListener("dblclick", ImageFocus.doubleClick);

		/*  If this image is part of the main gallery, then mark the overlay as 
			being in slide show mode (to show buttons/count). Otherwise, the
			overlay should be in single-image mode.
		 */
		ImageFocus.overlay.classList.toggle("slideshow", imageToFocus.classList.contains("gallery-image"));

		//  Set the caption.
		ImageFocus.setImageFocusCaption();

		//	Fire event.
		GW.notificationCenter.fireEvent("ImageFocus.imageDidFocus", { image: imageToFocus });
	},

	resetFocusedImagePosition: (updateOnLoad = false) => {
		GWLog("ImageFocus.resetFocusedImagePosition", "image-focus.js", 2);

		if (ImageFocus.imageInFocus == null)
			return;

		//  Make sure that initially, the image fits into the viewport.
		let imageWidth, imageHeight;
		if ((URLFromString(ImageFocus.imageInFocus.src)).pathname.endsWith(".svg")) {
			//	Special handling for SVGs, which have no intrinsic size.
			if (ImageFocus.imageInFocus.dataset.aspectRatio > "") {
				ImageFocus.imageInFocus.style.aspectRatio = ImageFocus.imageInFocus.dataset.aspectRatio;

				let parts = ImageFocus.imageInFocus.dataset.aspectRatio.match(/([0-9]+) \/ ([0-9]+)/);
				let aspectRatio = parseInt(parts[1]) / parseInt(parts[2]);
				imageWidth = window.innerHeight * aspectRatio;
				imageHeight = window.innerHeight;
			} else {
				imageWidth = imageHeight = Math.min(window.innerWidth, window.innerHeight);
			}
		} else {
			//	Non-SVGs have intrinsic size.
			imageWidth = ImageFocus.imageInFocus.naturalWidth || ImageFocus.imageInFocus.getAttribute("width");
			imageHeight = ImageFocus.imageInFocus.naturalHeight || ImageFocus.imageInFocus.getAttribute("height");

			if (imageWidth * imageHeight == 0) {
				if (updateOnLoad == true) {
					//	Reset on load.
					ImageFocus.imageInFocus.addEventListener("load", (event) => {
						ImageFocus.resetFocusedImagePosition(false);
					}, { once: true });

					return;
				} else {
					//	This shouldn’t happen. Display an error?
					return;
				}
			}
		}

		//	Constrain dimensions proportionally.
		let constrainedWidth = Math.min(imageWidth, window.innerWidth * ImageFocus.shrinkRatio);
		let widthShrinkRatio = constrainedWidth / imageWidth;
		let constrainedHeight = Math.min(imageHeight, window.innerHeight * ImageFocus.shrinkRatio);
		let heightShrinkRatio = constrainedHeight / imageHeight;
		let shrinkRatio = Math.min(widthShrinkRatio, heightShrinkRatio);

		//	Set dimensions via CSS.
		ImageFocus.imageInFocus.style.width = Math.round(imageWidth * shrinkRatio) + "px";
		ImageFocus.imageInFocus.style.height = Math.round(imageHeight * shrinkRatio) + "px";

		//  Remove modifications to position.
		ImageFocus.imageInFocus.style.left = "";
		ImageFocus.imageInFocus.style.top = "";

		//  Set the cursor appropriately.
		ImageFocus.setFocusedImageCursor();
	},

	setFocusedImageCursor: () => {
		GWLog("ImageFocus.setFocusedImageCursor", "image-focus.js", 2);

		if (ImageFocus.imageInFocus == null)
			return;

		ImageFocus.imageInFocus.style.cursor = (   ImageFocus.imageInFocus.height >= window.innerHeight
												|| ImageFocus.imageInFocus.width >= window.innerWidth)
											   ? "move"
											   : "";
	},

	unfocusImage: () => {
		GWLog("ImageFocus.unfocusImage", "image-focus.js", 1);

		//  Remove image from overlay.
		if (ImageFocus.imageInFocus) {
			ImageFocus.imageInFocus.remove();
			ImageFocus.imageInFocus = null;
		}

		//	Update currently focused image in page.
		if (ImageFocus.currentlyFocusedImage) {
			//	Save reference to image-to-be-unfocused.
			let unfocusedImage = ImageFocus.currentlyFocusedImage;

			ImageFocus.currentlyFocusedImage.classList.remove("focused");
			ImageFocus.currentlyFocusedImage = null;

			//	Fire event.
			GW.notificationCenter.fireEvent("ImageFocus.imageDidUnfocus", { image: unfocusedImage });
		}
	},

	enterImageFocus: () => {
		GWLog("ImageFocus.enterImageFocus", "image-focus.js", 1);

		if (ImageFocus.overlay.classList.contains("engaged"))
			return;

		//	Show overlay.
		ImageFocus.overlay.classList.add("engaged");

		//  Add listener to zoom image with scroll wheel.
		window.addEventListener("wheel", ImageFocus.scrollEvent, { passive: false });

		//  Escape key unfocuses, spacebar resets.
		document.addEventListener("keyup", ImageFocus.keyUp);

		//  Prevent spacebar or arrow keys from scrolling page when image focused.
		requestAnimationFrame(() => {
			togglePageScrolling(false);
		});

		//  Moving mouse unhides image focus UI.
		window.addEventListener("mousemove", ImageFocus.mouseMoved);

		//	Drag-end event; also, click to unfocus.
		window.addEventListener("mouseup", ImageFocus.mouseUp);

		//	Fire event.
		GW.notificationCenter.fireEvent("ImageFocus.imageOverlayDidAppear");
	},

	exitImageFocus: () => {
		GWLog("ImageFocus.exitImageFocus", "image-focus.js", 1);

		/*	If currently focused image is part of the main image gallery, 
			preserve state.
		 */
		if (   ImageFocus.currentlyFocusedImage
			&& ImageFocus.currentlyFocusedImage.classList.contains("gallery-image")) {
			//	Update classes.
			ImageFocus.currentlyFocusedImage.classList.remove("focused");

			if (ImageFocus.currentlyFocusedImage.classList.contains("gallery-image")) {
				ImageFocus.currentlyFocusedImage.classList.add("last-focused");

				//  Set accesskey of currently focused image, to re-focus it.
				ImageFocus.currentlyFocusedImage.accessKey = "l";
			}

			//  Reset the hash, if needed.
			if (location.hash.startsWith("#if_slide_")) {
				let previousURL = URLFromString(location.href);
				previousURL.hash = ImageFocus.savedHash ?? "";
				relocate(previousURL.href);

				ImageFocus.savedHash = null;
			}
		}

		//	Unfocus currently focused image.
		ImageFocus.unfocusImage();

		//  Remove event listeners.
		window.removeEventListener("wheel", ImageFocus.scrollEvent);
		window.removeEventListener("mousemove", ImageFocus.mouseMoved);
		window.removeEventListener("mouseup", ImageFocus.mouseUp);
		document.removeEventListener("keyup", ImageFocus.keyUp);

		//  Hide overlay.
		ImageFocus.overlay.classList.remove("engaged");

		requestAnimationFrame(() => {
			//  Re-enable page scrolling.
			togglePageScrolling(true);
		});

		//	Fire event.
		GW.notificationCenter.fireEvent("ImageFocus.imageOverlayDidDisappear");
	},

	getIndexOfFocusedImage: () => {
		let images = document.querySelectorAll(ImageFocus.galleryImagesSelector);
		let indexOfFocusedImage = -1;
		for (i = 0; i < images.length; i++) {
			if (images[i].classList.contains("focused")) {
				indexOfFocusedImage = i;
				break;
			}
		}
		return indexOfFocusedImage;
	},

	focusNextImage: (next = true) => {
		GWLog("ImageFocus.focusNextImage", "image-focus.js", 1);

		//	Find next image to focus.
		let images = document.querySelectorAll(ImageFocus.galleryImagesSelector);
		let indexOfFocusedImage = ImageFocus.getIndexOfFocusedImage();

		//	This shouldn’t happen, but...
		if (next ? (++indexOfFocusedImage == images.length) : (--indexOfFocusedImage == -1))
			return;

		//	Focus new image.
		ImageFocus.focusImage(images[indexOfFocusedImage]);
	},

	setImageFocusCaption: () => {
		GWLog("ImageFocus.setImageFocusCaption", "image-focus.js", 2);

		//	Used in comparison below.
		function textContentOf(node) {
			return node.textContent.trim().replace(/\n\n/g, " ");
		}

		/*	Get the figure caption, the ‘title’ attribute of the image, and the 
			‘alt’ attribute of the image. Clean each of typographic invisibles
			and educate quotes. Discard duplicate strings. Wrap all remaining 
			(unique) strings in <p> tags, and inject into caption container.
		 */
		let figcaption = ImageFocus.currentlyFocusedImage.closest("figure").querySelector("figcaption");
		ImageFocus.overlay.querySelector(".caption").replaceChildren(newDocument(`<div class="caption-text-wrapper">` 
		  + [ ...[
				(figcaption ? figcaption.cloneNode(true) : null),
				newElement("SPAN", null, { "innerHTML": ImageFocus.currentlyFocusedImage.getAttribute("title") }),
				newElement("SPAN", null, { "innerHTML": ImageFocus.currentlyFocusedImage.getAttribute("alt") }),
			].map(element => {
				if (element)
					Typography.processElement(element, Typography.replacementTypes.CLEAN|Typography.replacementTypes.QUOTES);

				if (element?.tagName == "FIGCAPTION")
					element.innerHTML = Array.from(element.children).map(p => p.innerHTML).join("<br>\n<br>\n");

				return element;
			}).filter((element, index, array) => (
					element != null
				 && isNodeEmpty(element) == false
				 && array.findIndex(otherElement => (
				 		otherElement != null
					 && textContentOf(otherElement) == textContentOf(element))
					) == index)
			).map(element => 
				`<p>${(element.innerHTML.trim())}</p>`
			)].join("") 
		  + `</div>`
		  + `<p class="image-url" title="Click to copy image URL to clipboard">`
			  + `<code class="url">`
				  + ImageFocus.currentlyFocusedImage.src
			  + `</code>`
			  + `<span class="icon-container">`
				  + `<span class="icon normal">`
					  + GW.svg("copy-regular")
				  + `</span>`
				  + `<span class="icon copied">`
					  + GW.svg("circle-check-solid")
				  + `</span>`
			  + `</span>`
		  + `</p>`));

		//	Activate click-to-copy on image URL.
		let imageURLContainer = ImageFocus.overlay.querySelector(".caption .image-url");
		imageURLContainer.addActivateEvent((event) => {
			copyTextToClipboard(imageURLContainer.querySelector(".url").textContent);

			//	Update icon.
			imageURLContainer.classList.add("copied");

            //  Flash URL, for visual feedback of copy operation.
            imageURLContainer.classList.add("flash");
            setTimeout(() => { imageURLContainer.classList.remove("flash"); }, 150);
		});
		imageURLContainer.addEventListener("mouseleave", (event) => {
			//	Reset icon.
			imageURLContainer.classList.remove("copied");
		});
	},

	focusImageSpecifiedByURL: () => {
		GWLog("ImageFocus.focusImageSpecifiedByURL", "image-focus.js", 1);

		if (location.hash.startsWith("#if_slide_")) {
			doWhenPageLoaded(() => {
				let images = document.querySelectorAll(ImageFocus.galleryImagesSelector);
				let imageToFocus = (/#if_slide_([0-9]+)/.exec(location.hash)||{})[1];
				if (   imageToFocus > 0
					&& imageToFocus <= images.length) {
					ImageFocus.focusImage(images[imageToFocus - 1]);
				}
			});
		}
	},

	/************************************/
	/* Image gallery UI showing / hiding.
	 ************************************/

	hideImageFocusUI: () => {
		GWLog("ImageFocus.hideImageFocusUI", "image-focus.js", 3);

		ImageFocus.overlay.querySelectorAll(ImageFocus.imageFocusUIElementsSelector).forEach(element => {
			element.classList.toggle("hidden", true);
		});
	},

	hideUITimerExpired: () => {
		GWLog("ImageFocus.hideUITimerExpired", "image-focus.js", 3);

		let timeSinceLastMouseMove = (new Date()) - ImageFocus.mouseLastMovedAt;
		if (timeSinceLastMouseMove < ImageFocus.hideUITimerDuration) {
			ImageFocus.hideUITimer = setTimeout(ImageFocus.hideUITimerExpired, (ImageFocus.hideUITimerDuration - timeSinceLastMouseMove));
		} else {
			ImageFocus.hideImageFocusUI();
			ImageFocus.cancelImageFocusHideUITimer();
		}
	},

	unhideImageFocusUI: () => {
		GWLog("ImageFocus.unhideImageFocusUI", "image-focus.js", 3);

		ImageFocus.overlay.querySelectorAll(ImageFocus.imageFocusUIElementsSelector).forEach(element => {
			element.classList.toggle("hidden", false);
		});

		if (GW.isMobile() == false)
			ImageFocus.hideUITimer = setTimeout(ImageFocus.hideUITimerExpired, ImageFocus.hideUITimerDuration);
	},

	cancelImageFocusHideUITimer: () => {
		GWLog("ImageFocus.cancelImageFocusHideUITimer", "image-focus.js", 3);

		clearTimeout(ImageFocus.hideUITimer);
		ImageFocus.hideUITimer = null;
	},

	/*********/
	/* Events.
	 *********/

	//  Event listener for clicking on images to focus them.
	imageClickedToFocus: (event) => {
		GWLog("ImageFocus.imageClickedToFocus", "image-focus.js", 2);

		//	Focus the clicked image, but don’t scroll to it.
		ImageFocus.focusImage(event.target, false);
	},

	scrollEvent: (event) => {
		GWLog("ImageFocus.scrollEvent", "image-focus.js", 3);

		event.preventDefault();

		let image = ImageFocus.imageInFocus;

		//  Remove the filter.
		image.savedFilter = image.style.filter;
		image.style.filter = "none";

		//  Get bounding box of the image within the viewport.
		let imageBoundingBox = image.getBoundingClientRect();

		//  Calculate resize factor.
		let factor = ((image.height > 10 && image.width > 10) || event.deltaY < 0)
					 ? 1 + Math.sqrt(Math.abs(event.deltaY))/100.0
					 : 1;

		//  Resize.
		image.style.width = (event.deltaY < 0 ?
							(image.clientWidth * factor) :
							(image.clientWidth / factor))
							+ "px";
		image.style.height = "auto";

		//  Designate zoom origin.
		let zoomOrigin;

		//  Zoom from cursor if we’re zoomed in to where image exceeds screen, AND
		//  the cursor is over the image.
		let imageSizeExceedsWindowBounds = (   image.getBoundingClientRect().width > window.innerWidth
											|| image.getBoundingClientRect().height > window.innerHeight);
		let zoomingFromCursor =    imageSizeExceedsWindowBounds
								&& (   imageBoundingBox.left <= event.clientX
									&& event.clientX <= imageBoundingBox.right
									&& imageBoundingBox.top <= event.clientY
									&& event.clientY <= imageBoundingBox.bottom);

		//  Otherwise, if we’re zooming OUT, zoom from window center; if we’re
		//  zooming IN, zoom from image center.
		let zoomingFromWindowCenter = event.deltaY > 0;
		if (zoomingFromCursor)
			zoomOrigin = { x: event.clientX,
						   y: event.clientY };
		else if (zoomingFromWindowCenter)
			zoomOrigin = { x: window.innerWidth / 2,
						   y: window.innerHeight / 2 };
		else
			zoomOrigin = { x: imageBoundingBox.x + imageBoundingBox.width / 2,
						   y: imageBoundingBox.y + imageBoundingBox.height / 2 };

		//  Calculate offset from zoom origin.
		let offsetOfImageFromZoomOrigin = {
			x: imageBoundingBox.x - zoomOrigin.x,
			y: imageBoundingBox.y - zoomOrigin.y
		}

		//  Calculate delta from centered zoom.
		let deltaFromCenteredZoom = {
			x: image.getBoundingClientRect().x - (zoomOrigin.x + (event.deltaY < 0 ? offsetOfImageFromZoomOrigin.x * factor : offsetOfImageFromZoomOrigin.x / factor)),
			y: image.getBoundingClientRect().y - (zoomOrigin.y + (event.deltaY < 0 ? offsetOfImageFromZoomOrigin.y * factor : offsetOfImageFromZoomOrigin.y / factor))
		}

		//  Adjust image position appropriately.
		image.style.left = parseInt(getComputedStyle(image).left) - deltaFromCenteredZoom.x + "px";
		image.style.top = parseInt(getComputedStyle(image).top) - deltaFromCenteredZoom.y + "px";

		//  Gradually re-center image, if it’s smaller than the window.
		if (!imageSizeExceedsWindowBounds) {
			let imageCenter = { x: image.getBoundingClientRect().x + image.getBoundingClientRect().width / 2,
								y: image.getBoundingClientRect().y + image.getBoundingClientRect().height / 2 }
			let windowCenter = { x: window.innerWidth / 2,
								 y: window.innerHeight / 2 }
			let imageOffsetFromCenter = { x: windowCenter.x - imageCenter.x,
										  y: windowCenter.y - imageCenter.y }

			//  Divide the offset by 10 because we’re nudging the image toward center,
			//  not jumping it there.
			image.style.left = parseInt(getComputedStyle(image).left) + imageOffsetFromCenter.x / 10 + "px";
			image.style.top = parseInt(getComputedStyle(image).top) + imageOffsetFromCenter.y / 10 + "px";
		}

		//  Put the filter back.
		image.style.filter = image.savedFilter;

		//  Set the cursor appropriately.
		ImageFocus.setFocusedImageCursor();
	},

	mouseUp: (event) => {
		GWLog("ImageFocus.mouseUp", "image-focus.js", 2);

		//	Different handling for drag-end events than clicks.
		let imageWasBeingDragged = (window.onmousemove != null);

		//	Do this regardless of where the mouse-up is.
		if (   ImageFocus.imageInFocus.height >= window.innerHeight
			|| ImageFocus.imageInFocus.width >= window.innerWidth) {
			window.onmousemove = "";

			//  Put the filter back.
			ImageFocus.imageInFocus.style.filter = ImageFocus.imageInFocus.savedFilter;
		}

		//	Do nothing more if click is on a UI element.
		if (event.target.closest(ImageFocus.imageFocusUIElementsSelector))
			return;

		//  We only want to do anything on left-clicks.
		if (event.button != 0)
			return;

		if (   (   ImageFocus.imageInFocus.height < window.innerHeight
				&& ImageFocus.imageInFocus.width < window.innerWidth)
			|| (   imageWasBeingDragged == false
				&& event.target != ImageFocus.imageInFocus))
			ImageFocus.exitImageFocus();
	},

	imageMouseDown: (event) => {
		GWLog("ImageFocus.imageMouseDown", "image-focus.js", 2);

		//  We only want to do anything on left-clicks.
		if (event.button != 0)
			return;

		//	Prevent browser/system drag-and-drop initiate.
		event.preventDefault();

		if (   ImageFocus.imageInFocus.height >= window.innerHeight
			|| ImageFocus.imageInFocus.width >= window.innerWidth) {
			let mouseCoordX = event.clientX;
			let mouseCoordY = event.clientY;

			let imageCoordX = parseInt(getComputedStyle(ImageFocus.imageInFocus).left);
			let imageCoordY = parseInt(getComputedStyle(ImageFocus.imageInFocus).top);

			//  Save the filter.
			ImageFocus.imageInFocus.savedFilter = ImageFocus.imageInFocus.style.filter;

			window.onmousemove = (event) => {
				//  Remove the filter.
				ImageFocus.imageInFocus.style.filter = "none";
				ImageFocus.imageInFocus.style.left = imageCoordX + event.clientX - mouseCoordX + "px";
				ImageFocus.imageInFocus.style.top = imageCoordY + event.clientY - mouseCoordY + "px";
			};
			return false;
		}
	},

	doubleClick: (event) => {
		GWLog("ImageFocus.doubleClick", "image-focus.js", 2);

		if (   ImageFocus.imageInFocus.height >= window.innerHeight
			|| ImageFocus.imageInFocus.width >= window.innerWidth)
			ImageFocus.resetFocusedImagePosition();
	},

	keyUp: (event) => {
		GWLog("ImageFocus.keyUp", "image-focus.js", 3);

		let allowedKeys = [ " ", "Spacebar", "Escape", "Esc", "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight", "Up", "Down", "Left", "Right" ];
		if (   !allowedKeys.includes(event.key)
			|| getComputedStyle(ImageFocus.overlay).display == "none")
			return;

		event.preventDefault();

		switch (event.key) {
		case "Escape":
		case "Esc":
			ImageFocus.exitImageFocus();
			break;
		case " ":
		case "Spacebar":
			ImageFocus.resetFocusedImagePosition();
			break;
		case "ArrowDown":
		case "Down":
		case "ArrowRight":
		case "Right":
			if (   ImageFocus.currentlyFocusedImage
				&& ImageFocus.currentlyFocusedImage.classList.contains("gallery-image"))
				ImageFocus.focusNextImage(true);
			break;
		case "ArrowUp":
		case "Up":
		case "ArrowLeft":
		case "Left":
			if (   ImageFocus.currentlyFocusedImage
				&& ImageFocus.currentlyFocusedImage.classList.contains("gallery-image"))
				ImageFocus.focusNextImage(false);
			break;
		}
	},

	mouseMoved: (event) => {
		GWLog("ImageFocus.mouseMoved", "image-focus.js", 3);

		let currentDateTime = new Date();

		if ([ ImageFocus.imageInFocus, 
			  ImageFocus.overlay, 
			  document.documentElement 
			 ].includes(event.target)) {
			if (ImageFocus.hideUITimer == null)
				ImageFocus.unhideImageFocusUI();

			ImageFocus.mouseLastMovedAt = currentDateTime;
		} else {
			ImageFocus.cancelImageFocusHideUITimer();
		}
	}
};

GW.notificationCenter.fireEvent("ImageFocus.didLoad");

ImageFocus.setup();

//	If the URL specifies an image, focus it after the page has loaded.
ImageFocus.focusImageSpecifiedByURL();
// dark-mode.js: Javascript library for controlling page appearance, toggling between regular white and ‘dark mode’
// Author: Said Achmiz
// Date: 2020-03-20
// When: Time-stamp: "2022-01-05 11:31:32 gwern"
// license: PD

/*	Experimental ‘dark mode’: Mac OS (Safari) lets users specify via an OS 
	widget ‘dark’/‘light’ to make everything appear bright-white or darker (e.g. 
	for darker at evening to avoid straining eyes & disrupting circadian 
	rhyhms); this then is exposed by Safari as a CSS variable which can be 
	selected on. This is also currently supported by Firefox weakly as an 
	about:config variable. Hypothetically, iOS in the future might use its 
	camera or the clock to set ‘dark mode’ automatically. 

	https://drafts.csswg.org/mediaqueries-5/#prefers-color-scheme
	https://webkit.org/blog/8718/new-webkit-features-in-safari-12-1/
	https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme

	Images are handled specially: images are *not* inverted/negated by default; 
	images with a special class, `.invert-auto` (set on images by automated 
	tools like ImageMagick scripts counting colors) or `.invert` 
	(set manually), will be inverted. (This is intended to allow inversion of 
	images which would invert well, like statistical graphs or charts, which are
	typically black-on-white, and are much more pleasant to read in dark mode
	when inverted to white-on-black.) Inversion is removed on image hover or 
	image-focus.js click-to-zoom.

	Because many users do not have access to a browser/OS which explicitly 
	supports dark mode, cannot modify the browser/OS setting without undesired 
	side-effects, wish to opt in only for specific websites, or simply forget 
	that they turned on dark mode & dislike it, we make dark mode controllable 
	by providing a widget at the top of the page.
 */

DarkMode = { ...DarkMode, 
	/*****************/
	/*	Configuration.
	 */
	modeOptions: [
		[ "auto", "Auto", "Set light or dark mode automatically, according to system-wide setting (Win: Start → Personalization → Colors; Mac: Apple → System-Preferences → General → Appearance; iOS: Settings → Display-and-Brightness; Android: Settings → Display)", "adjust-solid" ],
		[ "light", "Light", "Light mode at all times (black-on-white)", "sun-solid" ],
		[ "dark", "Dark", "Dark mode at all times (inverted: white-on-black)", "moon-solid" ]
	],

	selectedModeOptionNote: " [This option is currently selected.]",

	/******************/
	/*	Infrastructure.
	 */

	modeSelector: null,
	modeSelectorInteractable: true,

	/*************/
	/*	Functions.
	 */

	/*	Set up UI.
	 */
	setup: () => {
		GWLog("DarkMode.setup", "dark-mode.js", 1);

		//	Inject mode selector(s).
		DarkMode.injectModeSelector();
		document.querySelectorAll(".dark-mode-selector-inline").forEach(element => {
			DarkMode.injectModeSelector(element);
		});

		//	Update saved setting.
		DarkMode.saveMode();
	},

	/******************/
	/*	Mode selection.
	 */

	//	Called by: DarkMode.injectModeSelector
	modeSelectorHTML: (inline = false) => {
		//	Get saved mode setting (or default).
		let currentMode = DarkMode.currentMode();

		let modeSelectorInnerHTML = DarkMode.modeOptions.map(modeOption => {
			let [ name, label, desc, icon ] = modeOption;
			let selected = (name == currentMode ? " selected" : " selectable");
			let disabled = (name == currentMode ? " disabled" : "");
			let active = (   currentMode == "auto"
						  && name == DarkMode.computedMode())
						  ? " active"
						  : "";
			if (name == currentMode)
				desc += DarkMode.selectedModeOptionNote;
			return `<button
						type="button"
						class="select-mode-${name}${selected}${active}"
						${disabled}
						tabindex="-1"
						data-name="${name}"
						title="${desc}"
							>`
						+ `<span class="icon">${(GW.svg(icon))}</span>`
						+ `<span class="label">${label}</span>`
					 + `</button>`;
		  }).join("");

		let selectorTag = (inline ? "span" : "div");
		let selectorId = (inline ? "" : "dark-mode-selector");
		let selectorClass = ("dark-mode-selector mode-selector" + (inline ? " mode-selector-inline" : ""));

		return `<${selectorTag} id="${selectorId}" class="${selectorClass}">${modeSelectorInnerHTML}</${selectorTag}>`;
	},

	modeSelectButtonClicked: (event) => {
		GWLog("DarkMode.modeSelectButtonClicked", "dark-mode.js", 2);

		let button = event.target.closest("button");

		//	Determine which setting was chosen (ie. which button was clicked).
		let selectedMode = button.dataset.name;

		/*	We don’t want clicks to go through if the transition 
			between modes has not completed yet, so we disable the 
			button temporarily while we’re transitioning between 
			modes.
		 */
		doIfAllowed(() => {
			//	Actually change the mode.
			DarkMode.setMode(selectedMode);
		}, DarkMode, "modeSelectorInteractable");
	},

	//	Called by: DarkMode.setup
	injectModeSelector: (replacedElement = null) => {
		GWLog("DarkMode.injectModeSelector", "dark-mode.js", 1);

		//	Inject the mode selector widget.
		let modeSelector;
		if (replacedElement) {
			modeSelector = elementFromHTML(DarkMode.modeSelectorHTML(true));
			replacedElement.replaceWith(modeSelector);
		} else {
			modeSelector = DarkMode.modeSelector = GW.pageToolbar.addWidget(DarkMode.modeSelectorHTML());
		}

		//	Activate mode selector widget buttons.
		modeSelector.querySelectorAll("button").forEach(button => {
			button.addActivateEvent(DarkMode.modeSelectButtonClicked);
		});

		//	Register event handler to update mode selector state.
		GW.notificationCenter.addHandlerForEvent("DarkMode.didSetMode", (info) => {
			DarkMode.updateModeSelectorState(modeSelector);
		});

		/*	Add active media query to update mode selector state when system dark
			mode setting changes. (This is relevant only for the ‘auto’ setting.)
		 */
		doWhenMatchMedia(GW.mediaQueries.systemDarkModeActive, "DarkMode.updateModeSelectorStateForSystemDarkMode", () => { 
			DarkMode.updateModeSelectorState(modeSelector);
		});
	},

	//	Called by: DarkMode.didSetMode event handler
	//	Called by: DarkMode.updateModeSelectorStateForSystemDarkMode active media query
	updateModeSelectorState: (modeSelector = DarkMode.modeSelector) => {
		GWLog("DarkMode.updateModeSelectorState", "dark-mode.js", 2);

		/*	If the mode selector has not yet been injected, then do nothing.
		 */
		if (modeSelector == null)
			return;

		//	Get saved mode setting (or default).
		let currentMode = DarkMode.currentMode();

		//	Clear current buttons state.
		modeSelector.querySelectorAll("button").forEach(button => {
			button.classList.remove("active");
			button.swapClasses([ "selectable", "selected" ], 0);
			button.disabled = false;
			if (button.title.endsWith(DarkMode.selectedModeOptionNote))
				button.title = button.title.slice(0, (-1 * DarkMode.selectedModeOptionNote.length));
		});

		//	Set the correct button to be selected.
		modeSelector.querySelectorAll(`.select-mode-${currentMode}`).forEach(button => {
			button.swapClasses([ "selectable", "selected" ], 1);
			button.disabled = true;
			button.title += DarkMode.selectedModeOptionNote;
		});

		/*	Ensure the right button (light or dark) has the “currently active” 
			indicator, if the current mode is ‘auto’.
		 */
		if (currentMode == "auto") {
			let activeMode = GW.mediaQueries.systemDarkModeActive.matches 
							 ? "dark" 
							 : "light";
			modeSelector.querySelector(`.select-mode-${activeMode}`).classList.add("active");
		}
	}
};

GW.notificationCenter.fireEvent("DarkMode.didLoad");

doWhenPageLoaded(() => {
    DarkMode.setup();
});
ReaderMode = { ...ReaderMode, 
	/*****************/
	/*	Configuration.
	 */
	maskedLinksSelector: "p a, li a",

	deactivateTriggerElementSelector: "#reader-mode-disable-when-here, #see-also, #external-links, #appendix, #appendices, #navigation, #footer, #footer-decoration-container",

	showMaskedLinksDelay: 250,

	adjustedPopupTriggerDelay: 2400,

	modeOptions: [
		[ "auto", "Auto", "Reader mode enabled automatically on certain pages. (When enabled, hold Alt key to show links in text.)", "book-with-gear" ],
		[ "on", "On", "Enable reader mode on all pages. (Hold Alt key to show links in text.)", "book-open-solid" ],
		[ "off", "Off", "Disable reader mode on all pages.", "book-open" ]
	],

	selectedModeOptionNote: " [This option is currently selected.]",

	/******************/
	/*	Infrastructure.
	 */
	markdownBody: document.querySelector("#markdownBody"),

	maskedLinksKeyToggleInfoAlert: null,

	modeSelector: null,
	modeSelectorInteractable: true,

	deactivateOnScrollDownObserver: null,

	state: {
		hoveringOverLink: false,
		altKeyPressed: false
	},

	/*************/
	/*	Functions.
	 */

	/*	Set up reader mode UI and interactions.
	 */
	setup: () => {
		GWLog("ReaderMode.setup", "reader-mode.js", 1);

		//	Fully activate.
		if (ReaderMode.active)
			ReaderMode.activate();

		//	Inject mode selector(s).
		ReaderMode.injectModeSelector();
		document.querySelectorAll(".reader-mode-selector-inline").forEach(element => {
			ReaderMode.injectModeSelector(element);
		});
	},

	/******************/
	/*	Mode selection.
	 */

	//	Called by: ReaderMode.modeSelectButtonClicked
	saveMode: (newMode) => {
		GWLog("ReaderMode.saveMode", "reader-mode.js", 1);

		if (newMode == "auto")
			localStorage.removeItem("reader-mode-setting");
		else
			localStorage.setItem("reader-mode-setting", newMode);
	},

	/*	Activate or deactivate reader mode, as determined by the current setting
		and the selected mode.
	 */
	//	Called by: ReaderMode.modeSelectButtonClicked
	setMode: (selectedMode = ReaderMode.currentMode()) => {
		GWLog("ReaderMode.setMode", "reader-mode.js", 1);

		//	Activate or deactivate, as (and if) needed.
		if (   ReaderMode.active == true
			&& ReaderMode.enabled() == false) {
			ReaderMode.deactivate();
		} else if (   ReaderMode.active == false
				   && ReaderMode.enabled() == true) {
			ReaderMode.activate();
		}

		/*	Kill the intersection observer, if switching away from "auto" mode.
			Or, spawn the intersection observer, if switching to "auto" mode.
		 */
		if (   selectedMode != "auto"
			&& ReaderMode.deactivateOnScrollDownObserver != null) {
			ReaderMode.despawnObserver();
		} else if (   selectedMode == "auto"
				   && ReaderMode.active == true
				   && ReaderMode.deactivateOnScrollDownObserver == null) {
			ReaderMode.spawnObserver();
		}

		//	Fire event.
		GW.notificationCenter.fireEvent("ReaderMode.didSetMode");
	},

	//	Called by: ReaderMode.injectModeSelector
	modeSelectorHTML: (inline = false) => {
		//	Get saved mode setting (or default).
		let currentMode = ReaderMode.currentMode();

		let modeSelectorInnerHTML = ReaderMode.modeOptions.map(modeOption => {
			let [ name, label, desc, icon ] = modeOption;
			let selected = (name == currentMode ? " selected" : " selectable");
			let disabled = (name == currentMode ? " disabled" : "");
			let active = ((   currentMode == "auto"
						   && name == (ReaderMode.active ? "on" : "off"))
						  ? " active"
						  : "");
			if (name == currentMode)
				desc += ReaderMode.selectedModeOptionNote;
			return `<button
						type="button"
						class="select-mode-${name}${selected}${active}"
						${disabled}
						tabindex="-1"
						data-name="${name}"
						title="${desc}"
							>`
						+ `<span class="icon">${(GW.svg(icon))}</span>`
						+ `<span class="label">${label}</span>`
					 + `</button>`;
		  }).join("");

		let selectorTag = (inline ? "span" : "div");
		let selectorId = (inline ? "" : "reader-mode-selector");
		let selectorClass = ("reader-mode-selector mode-selector" + (inline ? " mode-selector-inline" : ""));

		return `<${selectorTag} id="${selectorId}" class="${selectorClass}">${modeSelectorInnerHTML}</${selectorTag}>`;
	},

	modeSelectButtonClicked: (event) => {
		GWLog("ReaderMode.modeSelectButtonClicked", "reader-mode.js", 2);

		let button = event.target.closest("button");

		// Determine which setting was chosen (ie. which button was clicked).
		let selectedMode = button.dataset.name;

		/*	We don’t want clicks to go through if the transition 
			between modes has not completed yet, so we disable the 
			button temporarily while we’re transitioning between 
			modes.
		 */
		doIfAllowed(() => {
			// Save the new setting.
			ReaderMode.saveMode(selectedMode);

			// Actually change the mode.
			ReaderMode.setMode(selectedMode);
		}, ReaderMode, "modeSelectorInteractable");
	},

	//	Called by: ReaderMode.setup
	injectModeSelector: (replacedElement = null) => {
		GWLog("ReaderMode.injectModeSelector", "reader-mode.js", 1);

		//	Inject the mode selector widget.
		let modeSelector;
		if (replacedElement) {
			modeSelector = elementFromHTML(ReaderMode.modeSelectorHTML(true));
			replacedElement.replaceWith(modeSelector);
		} else {
			modeSelector = ReaderMode.modeSelector = GW.pageToolbar.addWidget(ReaderMode.modeSelectorHTML());
		}

		//	Activate mode selector widget buttons.
		modeSelector.querySelectorAll("button").forEach(button => {
			button.addActivateEvent(ReaderMode.modeSelectButtonClicked);
		});

		//	Register event handler to update mode selector state.
		GW.notificationCenter.addHandlerForEvent("ReaderMode.didSetMode", (info) => {
			ReaderMode.updateModeSelectorState(modeSelector);
		});
	},

	//	Called by: ReaderMode.didSetMode event handler
	//	Called by: ReaderMode.deactivateOnScrollDownObserver callback
	updateModeSelectorState: (modeSelector = ReaderMode.modeSelector) => {
		GWLog("ReaderMode.updateModeSelectorState", "reader-mode.js", 2);

		/*	If the mode selector has not yet been injected, then do nothing.
		 */
		if (modeSelector == null)
			return;

		//	Get saved mode setting (or default).
		let currentMode = ReaderMode.currentMode();

		//	Clear current buttons state.
		modeSelector.querySelectorAll("button").forEach(button => {
			button.classList.remove("active");
			button.swapClasses([ "selectable", "selected" ], 0);
			button.disabled = false;
			if (button.title.endsWith(ReaderMode.selectedModeOptionNote))
				button.title = button.title.slice(0, (-1 * ReaderMode.selectedModeOptionNote.length));
		});

		//	Set the correct button to be selected.
		modeSelector.querySelectorAll(`.select-mode-${currentMode}`).forEach(button => {
			button.swapClasses([ "selectable", "selected" ], 1);
			button.disabled = true;
			button.title += ReaderMode.selectedModeOptionNote;
		});

		/*	Ensure the right button (on or off) has the “currently active”
			indicator, if the current mode is ‘auto’.
		 */
		if (currentMode == "auto") {
			let activeMode = ReaderMode.active 
							 ? "on" 
							 : "off";
			modeSelector.querySelector(`.select-mode-${activeMode}`).classList.add("active");
		}
	},

	/***************************************************/
	/*	Activation / deactivation. (Core functionality.)
	 */

	/*	Masks links and hide other elements, as appropriate. This will hide
		linkicons and pop-frame indicators, and will thus cause reflow.
	 */
	//	Called by: ReaderMode.setMode
	activate: () => {
		GWLog("ReaderMode.activate", "reader-mode.js", 1);

		ReaderMode.active = true;

		//	Add body classes.
		document.body.classList.add("reader-mode-active", "masked-links-hidden");

		//	Get a list of all the links that are to be masked.
		ReaderMode.maskedLinks = ReaderMode.markdownBody.querySelectorAll(ReaderMode.maskedLinksSelector);

		//	Mask links.
		ReaderMode.maskedLinks.forEach(link => {
			if (GW.isMobile() == false) {
				/*	Add `mouseenter` / `mouseleave` listeners to show/hide masked
					links on hover.
				 */
				link.removeMouseEnterEvent = onEventAfterDelayDo(link, "mouseenter", ReaderMode.showMaskedLinksDelay, ReaderMode.updateState, "mouseleave");
				link.removeMouseLeaveEvent = onEventAfterDelayDo(link, "mouseleave", 0, ReaderMode.updateState);

				//	Add custom popup trigger delay.
				link.specialPopupTriggerDelay = () => {
					return (ReaderMode.maskedLinksVisible() == false
							? ReaderMode.adjustedPopupTriggerDelay
							: Popups.popupTriggerDelay);
				};
			}

			/*	Add custom link click behavior
				(Save the existing handler, if any. Required for popin support.)
			 */
			link.savedOnClick = link.onclick;
			link.onclick = (event) => { return (ReaderMode.maskedLinksVisible() == true); };
		});

		if (GW.isMobile() == false) {
			//	Inject info alert.
			ReaderMode.maskedLinksKeyToggleInfoAlert = addUIElement(`<div id="masked-links-key-toggle-info-alert">`
				+ `<p>`
					+ `<span class="icon">`
						+ GW.svg("book-open-solid")
					+ `</span>`
					+ `Hold <span class="key">alt</span> / <span class="key">option</span> key to show links</p>`
				+ `</div>`);

			//	Add key down/up listeners, to show/hide masked links with Alt key.
			document.addEventListener("keydown", ReaderMode.altKeyDownOrUp = (event) => {
				if (event.key != "Alt")
					return;

				ReaderMode.updateState(event);
			});
			document.addEventListener("keyup", ReaderMode.altKeyDownOrUp);
		}

		/*	Create intersection observer to automatically unmask links when
			page is scrolled down to a specified location (element).
		 */
		if (ReaderMode.currentMode() == "auto")
			ReaderMode.spawnObserver();

		//	Update visual state.
		ReaderMode.updateVisibility({ maskedLinksVisible: false, maskedLinksKeyToggleInfoAlertVisible: false });

		//	Update document title.
		if (document.title.endsWith(ReaderMode.readerModeTitleNote) == false)
			document.title += ReaderMode.readerModeTitleNote;
	},

	//	Called by: ReaderMode.activate
	//	Called by: ReaderMode.setMode
	spawnObserver: () => {
		GWLog("ReaderMode.spawnObserver", "reader-mode.js", 2);

		//	Create the observer.
		ReaderMode.deactivateOnScrollDownObserver = new IntersectionObserver((entries, observer) => {
			entries.forEach(entry => {
				if (entry.isIntersecting == false)
					return;

				ReaderMode.deactivate();
				ReaderMode.updateModeSelectorState();
				ReaderMode.despawnObserver();
			});
		}, { threshold: 1.0 });

		//	Commence observation.
		ReaderMode.deactivateOnScrollDownObserver.observe(document.querySelector(ReaderMode.deactivateTriggerElementSelector));
	},

	//	Called by: ReaderMode.setMode
	despawnObserver: () => {
		GWLog("ReaderMode.despawnObserver", "reader-mode.js", 2);

		ReaderMode.deactivateOnScrollDownObserver.disconnect();
		ReaderMode.deactivateOnScrollDownObserver = null;
	},

	/*	Unmasks links and reveal other elements, as appropriate. This will
		un-hide linkicons and pop-frame indicators, and will thus cause reflow.
	 */
	//	Called by: ReaderMode.setMode
	//	Called by: ReaderMode.deactivateOnScrollDownObserver callback
	deactivate: () => {
		GWLog("ReaderMode.deactivate", "reader-mode.js", 1);

		ReaderMode.active = false;

		//	Update document title.
		if (document.title.endsWith(ReaderMode.readerModeTitleNote))
			document.title = document.title.slice(0, (-1 * ReaderMode.readerModeTitleNote.length));

		//	Remove body classes.
		document.body.classList.remove("reader-mode-active", "masked-links-hidden");

		//	Remove info alert.
		if (ReaderMode.maskedLinksKeyToggleInfoAlert != null)
			ReaderMode.maskedLinksKeyToggleInfoAlert.remove();

		/*	Unmask every masked link. (Note that ReaderMode.maskedLinks is a
			NodeList, returned by a querySelectorAll call in
			ReaderMode.activate. If that function has never been called, then
			ReaderMode.maskedLinks will be null).
		 */
		(ReaderMode.maskedLinks || [ ]).forEach(link => {
			//	Extract hooks.
// 			link.querySelectorAll(".icon-hook").forEach(hook => { hook.remove() });

			if (GW.isMobile() == false) {
				//	Remove `mouseenter` / `mouseleave` listeners from the link.
				link.removeMouseEnterEvent();
				link.removeMouseLeaveEvent();
				link.removeMouseEnterEvent = null;
				link.removeMouseLeaveEvent = null;

				//	Remove custom popup trigger delay.
				link.specialPopupTriggerDelay = null;
			}

			//	Re-enable normal link click behavior.
			link.onclick = link.savedOnClick;
			link.savedOnClick = null;
		});

		//	Re-layout sidenotes.
		if (window.Sidenotes)
			Sidenotes.updateSidenotePositions();

		if (GW.isMobile() == false) {
			//	Remove key down/up listeners (for the Alt key toggle).
			document.removeEventListener("keydown", ReaderMode.altKeyDownOrUp);
			document.removeEventListener("keyup", ReaderMode.altKeyDownOrUp);
			ReaderMode.altKeyDownOrUp = null;
		}
	},

	/****************/
	/*	Link masking.
	 */

	/*	Returns true if masked links (if any) are currently visible, false
		otherwise.
	 */
	maskedLinksVisible: () => {
		return (document.body.classList.contains("masked-links-hidden") == false);
	},

	/***********************************************/
	/*	Interaction-based state/visibility updating.
	 */

	/*	Update state after an event that might cause a visibility change.
	 */
	//	Called by: masked link `mouseenter`/`mouseleave` event handlers
	//	Called by: document `keydown`/`keyup` event handlers (for Alt key)
	updateState: (event) => {
		GWLog("ReaderMode.updateState", "reader-mode.js", 3);

		//	Update tracked state.
		switch (event.type) {
			case "mouseenter":
				ReaderMode.state.hoveringOverLink = true;
				break;
			case "mouseleave":
				ReaderMode.state.hoveringOverLink = false;
				break;
			case "keydown":
				ReaderMode.state.altKeyPressed = true;
				break;
			case "keyup":
				ReaderMode.state.altKeyPressed = false;
				break;
			default:
				break;
		}

		/*	Determine whether we should show or hide masked links and other
			elements.
		 */
		let shouldShowMaskedLinks = (ReaderMode.state.hoveringOverLink || ReaderMode.state.altKeyPressed);
		let shouldShowMaskedLinksKeyToggleInfoAlert = (ReaderMode.state.hoveringOverLink && !ReaderMode.state.altKeyPressed);

		//	Request the desired visibility update.
		ReaderMode.updateVisibility({
			maskedLinksVisible: shouldShowMaskedLinks,
			maskedLinksKeyToggleInfoAlertVisible: shouldShowMaskedLinksKeyToggleInfoAlert
		});
	},

	/*	Update visibility, based on desired visibility (the `update` argument)
		and the current visibility. (Applies to: masked links, masked links key
		toggle info alert panel.)
	 */
	//	Called by: ReaderMode.activate
	//	Called by: ReaderMode.updateState
	updateVisibility: (update) => {
		GWLog("ReaderMode.updateVisibility", "reader-mode.js", 3);

		/*	Show or hide masked links, depending on what visibility update has
			been requested, and whether it is necessary (i.e., whether or not
			things already are as they should be).
		 */
		if (   update.maskedLinksVisible == true
			&& ReaderMode.maskedLinksVisible() == false) {
			//	Show.
			document.body.classList.remove("masked-links-hidden");
		} else if (   update.maskedLinksVisible == false
				   && ReaderMode.maskedLinksVisible() == true) {
			//	Hide.
			document.body.classList.add("masked-links-hidden");
		}

		if (ReaderMode.maskedLinksKeyToggleInfoAlert != null) {
			//	Likewise, show or hide the key toggle info alert panel, as needed.
			if (update.maskedLinksKeyToggleInfoAlertVisible) {
				//	Show.
				ReaderMode.maskedLinksKeyToggleInfoAlert.classList.remove("hidden");
			} else {
				//	Hide.
				ReaderMode.maskedLinksKeyToggleInfoAlert.classList.add("hidden");
			}
		}
	},
};

GW.notificationCenter.fireEvent("ReaderMode.didLoad");

/*	Ensure that we run setup only after Extracts have completed their setups. 
	(This is so that the onclick handlers and so on are already in place.)
 */
if (window.Extracts) {
    ReaderMode.setup();
} else {
    GW.notificationCenter.addHandlerForEvent("Extracts.didLoad", (info) => {
        ReaderMode.setup();
    }, { once: true });
}
