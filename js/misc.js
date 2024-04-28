/*******************/
/* INJECT TRIGGERS */
/*******************/

GW.elementInjectTriggers = { };
GW.defunctElementInjectTriggers = { };

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

		let doTrigger = (element, f) => {
			GW.defunctElementInjectTriggers[element.dataset.uuid] = f;
			delete GW.elementInjectTriggers[element.dataset.uuid];
			f(element);
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
function placeholder(replaceFunction, wrapperFunction) {
	let transform = wrapperFunction
					? (element) => { wrapperFunction(() => { element.replaceWith(replaceFunction(element)); }); }
					: (element) => { element.replaceWith(replaceFunction(element)); }

	let uuid = onInject(null, transform);

	return `<span class="placeholder" data-uuid="${uuid}"></span>`;
}

/*****************************************************************************/
/*	Generate new UUIDs for any placeholder elements in the given container. 
	(Necessary when using a DocumentFragment to make a copy of a subtree; 
	 otherwise - since inject triggers are deleted after triggering once - 
	 any placeholders in the copied subtree will never get replaced.)
 */
function regeneratePlaceholderIds(container) {
	container.querySelectorAll(".placeholder").forEach(placeholder => {
		placeholder.dataset.uuid = onInject(null, (   GW.elementInjectTriggers[placeholder.dataset.uuid] 
												   ?? GW.defunctElementInjectTriggers[placeholder.dataset.uuid]));
	});
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
function randomAsset(assetPathnamePattern) {
	let assetPathnameRegExp = new RegExp(assetPathnamePattern);
	let alternateAssetPathnames = [ ];
	for (versionedAssetPathname of Object.keys(GW.assetVersions)) {
		if (assetPathnameRegExp.test(versionedAssetPathname))
			alternateAssetPathnames.push(versionedAssetPathname);
	}

	return (alternateAssetPathnames[rollDie(alternateAssetPathnames.length) - 1] ?? null);
}


/*******************/
/* IMAGE INVERSION */
/*******************/

GW.invertOrNot = { };
GW.invertOrNotAPIEndpoint = "https://invertornot.com/api/url";

/*******************************************************************/
/*	Returns true if the given image should be inverted in dark mode.
 */
function shouldInvertImageInDarkMode(image) {
	return (GW.invertOrNot[image.src].invert == true);
}

/*****************************************************************************/
/*	Sends request to InvertOrNot for judgments about whether the images in the
	given container ought to be inverted.
 */
function requestImageInversionDataForImagesInContainer(container) {
	let imageURLs = Array.from(container.querySelectorAll("figure img")).map(image => 
		(   URLFromString(image.src).pathname.match(/\.(png|jpe?g$)/i) 
		 && GW.invertOrNot[image.src] == null)
		? image.src
		: null 
	).filter(x => x);
	if (imageURLs.length == 0)
		return;

	doAjax({
		location: GW.invertOrNotAPIEndpoint,
		method: "POST",
		serialization: "JSON",
		responseType: "json",
		params: imageURLs,
		onSuccess: (event) => {
			event.target.response.forEach(imageInfo => {
				GW.invertOrNot[imageInfo.url] = {
					invert: (imageInfo.invert == 1)
				};
			});
		},
		onFailure: (event) => {
			console.log(event);
		}
	});
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
        for (let [ pathnamePrefix, linkType ] of Object.entries(AuxLinks.auxLinksLinkTypes))
            if (link.pathname.startsWith(pathnamePrefix))
                return linkType;

        return null;
    },

    /*  Page or document for whom the aux-links are.
     */
    targetOfAuxLinksLink: (link) => {
        for (let [ pathnamePrefix, linkType ] of Object.entries(AuxLinks.auxLinksLinkTypes)) {
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
									Array.from(popFrame.document.querySelectorAll(selector)))
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
function aggregateMarginNotesIfNeededInDocument(doc) {
	if (GW.marginNotes.aggregationNeededInDocuments.includes(doc) == false)
		GW.marginNotes.aggregationNeededInDocuments.push(doc);

	requestAnimationFrame(() => {
		if (GW.marginNotes.aggregationNeededInDocuments.includes(doc) == false)
			return;

		GW.marginNotes.aggregationNeededInDocuments.remove(doc);

		aggregateMarginNotesInDocument(doc);
	});
}

/**************************/
/*	Aggregate margin notes.
 */
function aggregateMarginNotesInDocument(doc) {
    GWLog("aggregateMarginNotesInDocument", "misc.js", 2);

	let marginNotesBlockClass = "margin-notes-block";

	doc.querySelectorAll(".marginnote").forEach(marginNote => {
		if (marginNote.textContent.trim() == "☞")
			return;

		let section = marginNote.closest("section, .markdownBody, .annotation-abstract");
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

		//	Clone the note.
		let clonedNote = marginNote.cloneNode(true);

		//	Set margin note type class.
		clonedNote.swapClasses([ "inline", "sidenote" ], 0);

		//	Unwrap the inner wrapper (unneeded here).
		unwrap(clonedNote.querySelector(".marginnote-inner-wrapper"));

		//	Remove dropcap, if any.
		resetDropcapInBlock(clonedNote);

		//	Trim whitespace.
		clonedNote.innerHTML = clonedNote.innerHTML.trim();

		//	Strip brackets.
        /*	Reason: we use brackets for editorial insertions & commentary, 
        	particularly in annotations where the reader assumes the text is 
        	written by the original authors.
        		Sometimes in long annotations where we wish to add ‘sections’ 
        	(because the original didn’t have them or they were inappropriate, 
        	eg. long journalistic essays where the material is scattered rather 
        	than organized by topic as necessary for a convenient annotation), 
        	we use margin-notes as a substitute for original sections.
        	Such editorializing of course must be marked by brackets to avoid 
        	misleading the reader; but then, when aggregated at the beginning 
        	of the annotation like all margin notes, it looks bad: 
        	‘[Foo] · [Bar] · [Baz] · [Quux]’.
        		So, although it risks misleading readers who do not read down 
        	to the actual margin-note usage & see that it’s an editorial 
        	insertion, we remove the brackets when aggregated.
        		(If it is necessary to override this feature & force brackets 
        	displayed in aggregates - perhaps because the margin-note is some 
        	exotic chemical name that starts with a bracket - one can use 
        	alternate Unicode bracket-pairs, or possibly some sort of 
        	non-printing non-whitespace character to block the match. 
        	Although, since the match requires the text to both start *and* end 
        	with a bracket, this should be an extremely rare edge-case not 
        	worth thinking about further.)
         */
		if (   clonedNote.textContent.startsWith("[")
			&& clonedNote.textContent.endsWith("]")) {
			clonedNote.firstTextNode.nodeValue = clonedNote.firstTextNode.nodeValue.slice(1);
			clonedNote.lastTextNode.nodeValue = clonedNote.lastTextNode.nodeValue.slice(0, -1);
		}

		//	Strip trailing period.
		if (clonedNote.textContent.endsWith("."))
			clonedNote.lastTextNode.nodeValue = clonedNote.lastTextNode.nodeValue.slice(0, -1);

		//	Prevent duplication.
		if (Array.from(marginNotesBlock.children).findIndex(child => {
				return clonedNote.textContent == child.textContent;
			}) != -1)
			return;

		//	Append.
		marginNotesBlock.append(clonedNote);

		//	Process the new entries to activate pop-frame spawning.
		Extracts.addTargetsWithin(marginNotesBlock);
	});

	//	Update visibility of margin note blocks.
	doc.querySelectorAll(`.${marginNotesBlockClass}`).forEach(marginNotesBlock => {
		marginNotesBlock.classList.toggle("hidden", marginNotesBlock.children.length < GW.marginNotes.minimumAggregatedNotesCount);
	});
}

/***************************************************************************/
/*	Child nodes of a paragraph, excluding any margin notes in sidenote mode.
 */
function nodesOfGraf(graf) {
	return Array.from(graf.childNodes).filter(node => ((node instanceof Element && node.matches(".marginnote.sidenote")) == false));
}

/*****************************************************************************/
/*	Text content of a paragraph, excluding the contents of any margin notes in
	sidenote mode.
 */
function textContentOfGraf(graf) {
	return nodesOfGraf(graf).map(node => node.textContent).join("");
}

/******************************************************************************/
/*	First text node of a paragraph, skipping any margin notes in sidenote mode.
 */
function firstTextNodeOfGraf(graf) {
	return nodesOfGraf(graf).first.firstTextNode;
}


/*********************/
/* TABLE OF CONTENTS */
/*********************/

GW.TOC = {
	containersToUpdate: [ ]
};

/*********************************************************************/
/*	Update page TOC, on the next animation frame, if not already done.
 */
function updatePageTOCIfNeeded(container = document) {
	if (container == document) {
		GW.TOC.containersToUpdate = [ document ];
	} else if (GW.TOC.containersToUpdate.includes(container) == false) {
		GW.TOC.containersToUpdate.push(container);
	}

	requestAnimationFrame(() => {
		while (GW.TOC.containersToUpdate.length > 0)
			updatePageTOC(GW.TOC.containersToUpdate.shift());
	});
}

/*****************************************************************************/
/*  Updates the page TOC with any sections in the page that don’t already have
	TOC entries.
 */
//  Called by: updateMainPageTOC (rewrite.js)
//  Called by: includeContent (transclude.js)
function updatePageTOC(container = document) {
    GWLog("updatePageTOC", "misc.js", 2);

    let TOC = document.querySelector("#TOC");
    if (!TOC)
        return;

    //  Don’t nest TOC entries any deeper than this.
    let maxNestingDepth = 4;

	//	Collect new entries, for later processing (if need be).
	let newEntries = [ ];

	container.querySelectorAll("#markdownBody section").forEach(section => {
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
		Typography.processElement(entry, Typography.replacementTypes.WORDBREAKS);
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
/* DROPCAPS */
/*************/

GW.dropcaps = {
	dropcapBlockSelector: "p[class*='dropcap-']:not(.dropcap-not)",

	graphicalDropcapTypes: [
		"dropcat",
		"gene-wolfe",
		"ninit"
	]
};

/***************************************************************************/
/*	Returns URL of a random graphical dropcap of the given type and letter,
	appropriate for the current mode and the viewport’s device pixel ratio.
 */
function randomDropcapURL(dropcapType, letter) {
	let mode = DarkMode.computedMode();
	let scale = valMinMax(Math.ceil(window.devicePixelRatio), 1, 2);

	let dropcapPathname = randomAsset(`/static/font/dropcap/${dropcapType}/(${mode}/)?${letter.toUpperCase()}(-.+)?-[0-9]+(\\.svg|-small-${scale}x\\.png)$`);
	if (dropcapPathname == null)
		return null;

	return versionedAssetURL(dropcapPathname);
}

/*****************************************************************************/
/*	Reset dropcap in the given block to initial state (as it was prior to the
	handlers in this section being run, i.e. not implemented, only marked for
	implementation).

	This function is also used to strip dropcaps from blocks that shouldn’t
	have them in the first place.
 */
function resetDropcapInBlock(block) {
	let dropcapLink = block.querySelector(".link-dropcap");
	if (dropcapLink == null)
		return;

	unwrap(dropcapLink);

	//	If this is a graphical dropcap block...
	let dropcapImage = block.querySelector("img.dropcap");
	if (dropcapImage) {
		//	Remove mode change handler.
		GW.notificationCenter.removeHandlerForEvent(dropcapImage.modeChangeHandler, "DarkMode.computedModeDidChange");

		//	Remove graphical dropcap.
		dropcapImage.remove();
	}

	//	Text node surgery: reattach letter.
	let letterSpan = block.querySelector("span.dropcap, span.hidden-initial-letter");
	letterSpan.nextSibling.textContent = letterSpan.textContent + letterSpan.nextSibling.textContent;
	letterSpan.remove();

	//	Text node surgery: reattach preceding punctuation (if any).
	let precedingPunctuation = block.querySelector("span.initial-preceding-punctuation");
	if (precedingPunctuation) {
		precedingPunctuation.nextSibling.textContent = precedingPunctuation.textContent + precedingPunctuation.nextSibling.textContent;
		precedingPunctuation.remove();
	}
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

	Available option fields:

	raiseOnHover (boolean)
		When the added UI element is hovered over, it gains a `hover` class.
 */
function addUIElement(element, options) {
	options = Object.assign({
		raiseOnHover: false
	}, options);

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
	widgetFlashRiseDuration: 1000, // --GW-page-toolbar-widget-flash-rise-duration
	widgetFlashFallDuration: 1000, // --GW-page-toolbar-widget-flash-fall-duration
	widgetFlashStayDuration: 500,

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

		//	Add widget.
		GW.pageToolbar.getToolbar().querySelector(".widgets").appendChild(widget);

		//	If setup has run, update state after adding widget.
		if (GW.pageToolbar.setupComplete)
			GW.pageToolbar.updateState();

		return widget;
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

	/*	Returns the widget with the given ID; or null, if no such widget ID.
	 */
	getWidget: (widgetID) => {
		return GW.pageToolbar.getToolbar().querySelector(`.widget#${widgetID}`);
	},

	flashWidget: (widgetID, options = { }) => {
		let widget = GW.pageToolbar.getToolbar().querySelector(`.widget#${widgetID}`);
		if (widget == null)
			return null;

		widget.classList.add("flashing");
		if (options.showSelectedButtonLabel)
			setTimeout(() => { widget.classList.add("show-selected-button-label"); }, GW.pageToolbar.widgetFlashRiseDuration * 0.5);
		setTimeout(() => {
			widget.swapClasses([ "flashing", "flashing-fade" ], 1);
			setTimeout(() => {
				widget.classList.remove("flashing-fade");
			}, GW.pageToolbar.widgetFlashFallDuration);
			if (options.showSelectedButtonLabel)
				setTimeout(() => { widget.classList.remove("show-selected-button-label"); }, GW.pageToolbar.widgetFlashFallDuration * 0.5);
		}, GW.pageToolbar.widgetFlashRiseDuration + (options.flashStayDuration ?? GW.pageToolbar.widgetFlashStayDuration));
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

		Available option fields:

		delay (integer)
			Collapse or uncollapse after a delay, instead of immediately.

		temp (boolean)
			If un-collapsing, do it only temporarily (re-collapse on un-hover).

		slow (boolean)
			If collapsing, do it slowly.
	 */
	toggleCollapseState: (collapse, options) => {
		options = Object.assign({
			delay: 0,
			temp: false,
			slow: false
		}, options);

		if (   collapse 
			&& options.delay > 0) {
			GW.pageToolbar.toolbar.collapseTimer = setTimeout(GW.pageToolbar.toggleCollapseState, 
															  options.delay, 
															  collapse, {
																  temp: options.temp,
																  slow: options.slow
															  });
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
			GW.pageToolbar.collapse(options.slow);
		} else {
			GW.pageToolbar.uncollapse(options.temp);
		}
	},

	/*	Collapse toolbar.

		(For internal use only; do not call except from .toggleCollapseState().)
	 */
	collapse: (slow = false) => {
		clearTimeout(GW.pageToolbar.toolbar.collapseTimer);

		GW.pageToolbar.toolbar.classList.add("collapsed");

		if (slow) {
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
	uncollapse: (temp = false) => {
		clearTimeout(GW.pageToolbar.toolbar.collapseTimer);

		GW.pageToolbar.addToolbarClassesTemporarily("animating",
			GW.pageToolbar.collapseDuration + GW.pageToolbar.fadeAfterCollapseDuration);

		GW.pageToolbar.toolbar.classList.remove("collapsed", "collapsed-slowly");

		if (temp)
			GW.pageToolbar.toolbar.classList.add("expanded-temp");
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
							GW.pageToolbar.toggleCollapseState(false, { temp: true });
					}, {
						cancelOnEvents: [ "mouseleave", "mousedown" ]
					});

					//	Collapse on unhover.
					onEventAfterDelayDo(GW.pageToolbar.toolbar, "mouseleave", GW.pageToolbar.unhoverCollapseDelay, (event) => {
						if (GW.pageToolbar.isTempExpanded())
							GW.pageToolbar.toggleCollapseState(true);
					}, {
						cancelOnEvents: [ "mouseenter" ]
					});
				}
			}
		});

		//	Set initial state.
		GW.pageToolbar.updateState();

		doWhenPageLoaded(() => {
			/*	Slowly collapse toolbar shortly after page load (if it’s not
				already collapsed).
			 */
			let startCollapsed = getSavedCount("page-toolbar-demos-count") >= GW.pageToolbar.maxDemos;
			if (startCollapsed == false) {
				requestAnimationFrame(() => {
					Array.from(GW.pageToolbar.getToolbar().querySelector(".widgets").children).forEach(widget => {
						let order = parseInt(getComputedStyle(widget).order);
						setTimeout(GW.pageToolbar.flashWidget, 
								   order * GW.pageToolbar.widgetFlashRiseDuration * 4/3, 
								   widget.id, { 
									   showSelectedButtonLabel: true 
								   });
					});

					//	Don’t collapse if hovering.
					if (GW.pageToolbar.toolbar.matches(":hover") == false)
						GW.pageToolbar.toggleCollapseState(true, { 
															  slow: true, 
															  delay: GW.pageToolbar.demoCollapseDelay
														   });
				});
			} else {
				incrementSavedCount("page-toolbar-demos-count");
			}

			//	Update toolbar state on scroll.
			addScrollListener(GW.pageToolbar.updateState, {
				name: "updatePageToolbarStateOnScrollListener", 
				defer: true
			});

			//	Update toolbar state on window resize.
			addWindowResizeListener(GW.pageToolbar.updateState, {
				name: "updatePageToolbarStateOnWindowResizeListener", 
				defer: true
			});
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
    addScrollListener(updateBackToTopLinkVisibility, {
    	name: "updateBackToTopLinkScrollListener", 
    	defer: true, 
    	ifDeferCallWhenAdd: true
    });

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
            	let span = wrapElement(link, "span.link", { moveClasses: true });
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
                           : document.querySelector(`#${(CSS.escape(x.slice(1)))} .heading a`).innerHTML)
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

		if (Extracts.popFrameProvider == Popins)
			Popins.removeAllPopins();
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
		addScrollListener(GW.floatingHeader.updateState, {
			name: "updateFloatingHeaderScrollListener",
			defer: true, 
			ifDeferCallWhenAdd: true
		});

		//  Adjust initial scroll offset.
		doWhenPageLayoutComplete(GW.floatingHeader.adjustScrollTop);
	}
};

doWhenPageLoaded(GW.floatingHeader.setup);


/**********/
/* SEARCH */
/**********/

GW.search = {
	searchWidgetId: "gcse-search",

	keyCommandSpawnSearchWidgetFlashStayDuration: 3000,

	searchWidget: null,
	searchWidgetLink: null,

	pinSearchPopup: (options) => {
		options = Object.assign({
			focus: false,
			popup: GW.search.searchWidgetLink?.popup
		}, options);

		if (options.popup) {
			Popups.pinPopup(options.popup);

			if (options.focus) {
				requestAnimationFrame(() => {
					options.popup.document.querySelector("iframe")?.contentDocument?.querySelector("input")?.focus();
				});
			}
		}
	},

	setup: () => {
		//	Add search widget to page toolbar.
		GW.search.searchWidget = GW.pageToolbar.addWidget(  `<div id="${GW.search.searchWidgetId}">`
														  + `<a 
														  	   class="search"
														  	   href="/static/search.html" 
														  	   data-link-content-type="local-document"
														  	   >`
														  + `<span class="icon">`
														  + GW.svg("magnifying-glass")
														  + `</span>`
														  + `<span class="label">Search</span>`
														  + `</a></div>`);

		//	Disable normal link functionality.
		GW.search.searchWidgetLink = GW.search.searchWidget.querySelector("a");
		GW.search.searchWidgetLink.onclick = () => false;

		//	Activate pop-frames.
		Extracts.config.hooklessLinksContainersSelector += `, #${GW.search.searchWidgetId}`;
		Extracts.addTargetsWithin(GW.search.searchWidget);

		//	Event handler for popup spawn / popin inject.
		let popFrameSpawnEventHandler = (eventInfo) => {
			let popFrame = (eventInfo.popup ?? eventInfo.popin);

			let iframe = popFrame.document.querySelector("iframe");
			iframe.addEventListener("load", (event) => {
				let inputBox = iframe.contentDocument.querySelector("input");

				//	Focus search box on load.
				inputBox.focus();

				let observer = new MutationObserver((mutationsList, observer) => {
					//	Pin popup if a search is done.
					GW.search.pinSearchPopup();

					//	Show/hide placeholder text as appropriate.
					if (iframe.contentWindow.location.hash.includes("gsc.q")) {
						Extracts.popFrameProvider.addClassesToPopFrame(popFrame, "search-results");
						iframe.contentDocument.querySelector(".search-results-placeholder").style.display = "none";
					} else {
						Extracts.popFrameProvider.removeClassesFromPopFrame(popFrame, "search-results");
						iframe.contentDocument.querySelector(".search-results-placeholder").style.display = "";
					}
				});

				observer.observe(iframe.contentDocument.body, {
					subtree: true,
					childList: true,
					characterData: true
				});

				if (Extracts.popFrameProvider == Popups) {
					inputBox.addEventListener("blur", (event) => {
						inputBox.justLostFocus = true;
					});
					iframe.contentDocument.addEventListener("keyup", (event) => {
						let allowedKeys = [ "Escape", "Esc", "/" ];
						if (allowedKeys.includes(event.key) == false)
							return;

						event.preventDefault();

						switch(event.key) {
							case "Escape":
							case "Esc": {
								if (inputBox.justLostFocus) {
									inputBox.justLostFocus = false;
								} else if (Popups.popupIsPinned(popFrame)) {
									Popups.unpinPopup(popFrame);
								} else {
									Popups.despawnPopup(popFrame);
								}

								break;
							}
							case "/": {
								GW.search.pinSearchPopup({ popup: popFrame, focus: true });

								break;
							}
							default: {
								break;
							}
						}
					});
				}
			});
		};

		//	Configure pop-frame behavior.
		if (Extracts.popFrameProvider == Popups) {
			//	Configure popup positioning and click response.
			GW.search.searchWidgetLink.preferSidePositioning = () => true;
			GW.search.searchWidgetLink.cancelPopupOnClick = () => false;

			//	Pin popup and focus search box if widget is clicked.
			GW.search.searchWidgetLink.addActivateEvent((event) => {
				GW.search.pinSearchPopup({ focus: true });
			});

			//	Add popup spawn event handler.
			GW.notificationCenter.addHandlerForEvent("Popups.popupDidSpawn", popFrameSpawnEventHandler, {
				condition: (info) => (info.popup.spawningTarget == GW.search.searchWidgetLink)
			});
		} else {
			//	Add popin inject event handler.
			GW.notificationCenter.addHandlerForEvent("Popins.popinDidInject", popFrameSpawnEventHandler, {
				condition: (info) => (info.popin.spawningTarget == GW.search.searchWidgetLink)
			});
		}
	}
};

doWhenPageLoaded(GW.search.setup);


/****************/
/* KEY COMMANDS */
/****************/
/*	Miscellaneous key commands.
 */

GW.keyCommands = { };

GW.keyCommands.keyUp = (event) => {
	GWLog("GW.keyCommands.keyUp", "popups.js", 3);
	let allowedKeys = [ "/" ];
	if (allowedKeys.includes(event.key) == false)
		return;

	event.preventDefault();

	switch(event.key) {
		case "/": {
			//	Expand page toolbar and flash search widget.
			GW.pageToolbar.toggleCollapseState(false);
			GW.pageToolbar.flashWidget(GW.search.searchWidgetId, {
				flashStayDuration: GW.search.keyCommandSpawnSearchWidgetFlashStayDuration
			});

			//	When the popup spawns, pin it and focus the search box.
			GW.notificationCenter.addHandlerForEvent("Popups.popupDidSpawn", (info) => {
				requestAnimationFrame(() => {
					GW.search.pinSearchPopup({ focus: true });
				});
			}, {
				once: true,
				condition: (info) => (info.popup.spawningTarget == GW.search.searchWidgetLink)
			});

			//	Spawn popup.
			Popups.spawnPopup(document.querySelector(`#${GW.search.searchWidgetId} a`));

			break;
		}
		default: {
			break;
		}
	}
};

doWhenPageLoaded(() => {
	if (Extracts.popFrameProvider == Popups)
		document.addEventListener("keyup", GW.keyCommands.keyUp);
});


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
