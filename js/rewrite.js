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

	eventInfo.container.querySelectorAll("li").forEach(listItem => {
		if (listItem.closest(".TOC"))
			return;

		paragraphizeTextNodesOfElement(listItem);
	});
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
    });
}, "rewrite");


/***************/
/* BLOCKQUOTES */
/***************/

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

/**********************************************/
/*	Remove Pandoc-inserted <colgroup> elements.
 */
addContentLoadHandler(GW.contentLoadHandlers.deleteColgroups = (eventInfo) => {
    GWLog("deleteColgroups", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("colgroup").forEach(colgroup => {
		colgroup.remove();
	});
}, "rewrite");

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
        jQuery(".table:not(.table-sort-not) table", eventInfo.document).tablesorter();
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

    wrapAll("table", ".table-wrapper", {
    	useExistingWrapper: true,
    	root: eventInfo.container
    });
    wrapAll("table", ".table-scroll-wrapper", { 
    	useExistingWrapper: false,
    	root: eventInfo.container
    });

	/*	Move .width-full class from the outer .table-wrapper down to the inner
		.table-scroll-wrapper. (This is done so that the `wrapFullWidthTables`
		content inject handler may work properly.)
	 */
    eventInfo.container.querySelectorAll(".table-scroll-wrapper").forEach(tableScrollWrapper => {
    	let tableWrapper = tableScrollWrapper.closest(".table-wrapper");
        transferClasses(tableWrapper, tableScrollWrapper, [ "width-full" ]);
    });
}, "rewrite");

/****************************************************/
/*  Rectify full-width table wrapper class structure:

    div.table-wrapper.table.width-full
        div.table-scroll-wrapper
            table

    or

    div.table-wrapper.collapse
        div.collapse-content-wrapper.table.width-full
            div.table-scroll-wrapper
                table
 */
addContentInjectHandler(GW.contentInjectHandlers.rectifyFullWidthTableWrapperStructure = (eventInfo) => {
    GWLog("rectifyFullWidthTableWrapperStructure", "rewrite.js", 1);

	wrapAll(".table-scroll-wrapper.width-full", ".table", {
		useExistingWrapper: true,
		moveClasses: [ "width-full" ],
		root: eventInfo.container
	});
}, "rewrite", (info) => info.fullWidthPossible);


/***********/
/* FIGURES */
/***********/

/****************************************************************************/
/*	Request image inversion data for images in the loaded content. (We omit
	from this load handler those GW.contentDidLoad events which are fired when 
	we construct templated content from already extract reference data, as by 
	then it is already too late; there is no time to send an invertOrNot API 
	request and receive a response. Instead, requesting inversion data for 
	images in templated content is handled by the data source object for that
	content (either Content, in content.js, or Annotations, in annotations.js).
 */
addContentLoadHandler(GW.contentLoadHandlers.requestImageInversionData = (eventInfo) => {
    GWLog("requestImageInversionData", "rewrite.js", 1);

	//	Request image inversion judgments from invertornot.
	requestImageInversionDataForImagesInContainer(eventInfo.container);
}, ">rewrite", (info) => (info.source != "transclude"));

/****************************************************************************/
/*	Apply image inversion data to images in the loaded content, if available.
 */
addContentInjectHandler(GW.contentInjectHandlers.applyImageInversionData = (eventInfo) => {
    GWLog("requestImageInversionData", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("figure img").forEach(image => {
		if (   image.classList.containsAnyOf([ "invert", "invert-auto", "invert-not" ]) == false
			&& GW.invertOrNot[image.src] != null)
			image.classList.add(GW.invertOrNot[image.src].invert ? "invert-auto" : "invert-not");
	});
}, "rewrite");

/******************************************************************/
/*	Wrap text nodes and inline elements in figcaptions in <p> tags.
 */
addContentLoadHandler(GW.contentLoadHandlers.paragraphizeFigcaptionTextNodes = (eventInfo) => {
    GWLog("paragraphizeFigcaptionTextNodes", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("figcaption").forEach(paragraphizeTextNodesOfElement);
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

		/*	If the ‘title’ attribute merely duplicates the caption, but the 
			‘alt’ attribute has something different (and nonempty), then copy
			the ‘alt’ to the ‘title’.
		 */
        if (   titleText == captionText
        	&& altText != captionText
        	&& altText > "")
            image.title = altText;

		/*	As above, but vice-versa (copy ‘title’ to ‘alt’, if appropriate).
		 */
        if (   altText == captionText
        	&& titleText != captionText
        	&& titleText > "")
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

    let exclusionSelector = [
    	"td",
    	"th",
    	".footnote-back"
    ].join(", ");
    wrapAll("img", (image) => {
        if (   image.classList.contains("figure-not")
            || image.closest(exclusionSelector) != null
            || image.closest("figure") != null)
            return;

        wrapElement(image, "figure");
    }, {
    	root: eventInfo.container
    });
}, "rewrite");

/******************************************************************************/
/*  Set, in CSS, the media (image/video) dimensions that are specified in HTML.
 */
function setMediaElementDimensions(mediaElement, fixWidth = false, fixHeight = false) {
    let width = mediaElement.getAttribute("width");
    let height = mediaElement.getAttribute("height");

    mediaElement.style.aspectRatio = mediaElement.dataset.aspectRatio ?? `${width} / ${height}`;

	if (mediaElement.maxHeight == null) {
		//	This should match `1rem`.
		let baseFontSize = GW.isMobile() ? "18" : "20";

		/*	This should match the `max-height` property value for all images in
			figures (the `figure img` selector; see initial.css).
		 */
		mediaElement.maxHeight = window.innerHeight - (8 * baseFontSize);
	}

    if (mediaElement.maxHeight)
        width = Math.round(Math.min(width, mediaElement.maxHeight * (width/height)));

    if (fixWidth) {
        mediaElement.style.width = `${width}px`;
    }
    if (fixHeight) {
        //  Nothing, for now.
    }
}

GW.dimensionSpecifiedMediaElementSelector = [
	"img[width][height]:not([src$='.svg'])",
	"video[width][height]"
].map(x => `figure ${x}`).join(", ");

/**************************************************************/
/*  Prevent reflow for floats, reduce reflow for other figures.
 */
addContentLoadHandler(GW.contentLoadHandlers.setMediaElementDimensions = (eventInfo) => {
    GWLog("setMediaElementDimensions", "rewrite.js", 1);

	//	Do not set image dimensions in sidenotes.
	if (eventInfo.container == Sidenotes.hiddenSidenoteStorage)
		return;

	//	Set specified dimensions in CSS.
    eventInfo.container.querySelectorAll(GW.dimensionSpecifiedMediaElementSelector).forEach(mediaElement => {
        let fixWidth = (   mediaElement.classList.containsAnyOf([ "float-left", "float-right" ])
                        || mediaElement.closest("figure")?.classList.containsAnyOf([ "float-left", "float-right" ]));
        setMediaElementDimensions(mediaElement, fixWidth);
    });

    //  Also ensure that SVGs get rendered as big as possible.
    eventInfo.container.querySelectorAll("figure img[src$='.svg']").forEach(svg => {
        svg.style.width = "100vw";
        svg.style.aspectRatio = svg.dataset.aspectRatio;
    });
}, "rewrite");

/************************************************************/
/*  Prevent reflow due to lazy-loaded media (images, videos).
 */
addContentInjectHandler(GW.contentInjectHandlers.updateMediaElementDimensions = (eventInfo) => {
    GWLog("updateMediaElementDimensions", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(GW.dimensionSpecifiedMediaElementSelector).forEach(mediaElement => {
        setMediaElementDimensions(mediaElement, true);
    });
}, "rewrite");

/************************************************************************/
/*	Set image dimensions from inline-specified image data (e.g., base64).
 */
addContentInjectHandler(GW.contentInjectHandlers.setImageDimensionsFromImageData = (eventInfo) => {
    GWLog("setImageDimensionsFromImageData", "rewrite.js", 1);

	/*	If an image doesn’t have dimensions set, but image data is already 
		available (because the source is a data: URI), we can determine 
		dimensions once the image “loads” (i.e., ‘load’ event fires, when 
		browser parses the data: attribute).
	 */
	eventInfo.container.querySelectorAll("figure img:not([width])").forEach(image => {
		if (image.loadHandler)
			return;

		image.addEventListener("load", image.loadHandler = (event) => {
			image.setAttribute("width", image.naturalWidth);
			image.setAttribute("height", image.naturalHeight);
			image.setAttribute("data-aspect-ratio", `${image.naturalWidth} / ${image.naturalHeight}`);

			setMediaElementDimensions(image);

			//	Ensure proper interaction with image-focus.
			if (image.classList.contains("focusable"))
				ImageFocus.designateSmallImageIfNeeded(image);
		});
	});
}, "eventListeners");

/************************************************************************/
/*  Ensure media (image, video) dimensions update when device is rotated.
 */
addContentInjectHandler(GW.contentInjectHandlers.addOrientationChangeMediaElementDimensionUpdateEvents = (eventInfo) => {
    GWLog("addOrientationChangeMediaElementDimensionUpdateEvents", "rewrite.js", 1);

	let mediaElements = eventInfo.container.querySelectorAll(GW.dimensionSpecifiedMediaElementSelector);

	doWhenMatchMedia(GW.mediaQueries.portraitOrientation, "Rewrite.updateMediaElementDimensionsWhenOrientationChanges", (mediaQuery) => {
		mediaElements.forEach(mediaElement => {
			mediaElement.maxHeight = null;
		});
		requestAnimationFrame(() => {
			mediaElements.forEach(mediaElement => {
				mediaElement.style.width = "";
				setMediaElementDimensions(mediaElement, true);
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

        if (   media   == null
        	|| caption == null)
            return;

        //  Create an inner wrapper for the figure contents.
        let innerWrapper = newElement("SPAN", { "class": "figure-inner-wrapper" });
        figure.appendChild(innerWrapper);

        //  Re-insert the (possibly wrapped) media into the figure.
        figure.querySelectorAll(mediaSelector).forEach(mediaElement => {
            let mediaBlock = (   mediaElement.closest(".image-row-wrapper") 
            				  ?? mediaElement.closest(".image-wrapper") 
            				  ?? mediaElement);
            innerWrapper.appendChild(mediaBlock);
        });

        //  Wrap the caption in the wrapper span.
        let captionWrapper = newElement("SPAN", { "class": "caption-wrapper" });
        captionWrapper.appendChild(caption);

        //  Re-insert the wrapped caption into the figure.
        innerWrapper.appendChild(captionWrapper);
    });
}, "rewrite");

/******************************************************************************/
/*	Figure captions might be empty if they are generated by including the 
	annotation abstract of an annotated media include link, but the abstract is
	actually empty (because it’s a partial annotation).
 */
addContentLoadHandler(GW.contentLoadHandlers.removeEmptyFigureCaptions = (eventInfo) => {
    GWLog("removeEmptyFigureCaptions", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("figcaption").forEach(figcaption => {
		if (isNodeEmpty(figcaption, { alsoExcludeSelector: "a" }))
			figcaption.remove();
	});
}, "rewrite");

/*****************************************************************************/
/*	Allow for specifying figure classes by setting classes on a media element.
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyFigureClasses = (eventInfo) => {
    GWLog("rectifyFigureClasses", "rewrite.js", 1);

    let mediaSelector = "img, audio, video";

    eventInfo.container.querySelectorAll("figure").forEach(figure => {
        let media = figure.querySelector(mediaSelector);

        //  Tag the figure with the first (or only) media element’s classes.
        [ "float-left", "float-right", "outline-not", "image-focus-not" ].forEach(imgClass => {
            if (media.classList.contains(imgClass)) {
                figure.classList.add(imgClass);
				media.classList.remove(imgClass);
			}
        });

		media.classList.remove("float");
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

/***********************************************************************/
/*  Prepare full-width (class `width-full`) figures; add listeners, etc.
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

    doWhenPageLayoutComplete(() => {
		/*  Update ‘load’ listener for any lazy-loaded media which has not 
			already loaded (as it might cause re-layout of e.g. sidenotes). Do 
			this only after page layout is complete, to avoid spurious re-layout
			at initial page load.
		 */
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
        }, {
        	name: "constrainFullWidthMediaCaptionWidthOnWindowResizeListener"
        });
    });
}, "rewrite", (info) => info.fullWidthPossible);

/******************************************************************************/
/*	There is no browser native lazy loading for <video> tag `poster` attribute,
	so we implement it ourselves.
 */
addContentInjectHandler(GW.contentInjectHandlers.lazyLoadVideoPosters = (eventInfo) => {
    GWLog("lazyLoadVideoPosters", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("video:not([poster])").forEach(video => {
		lazyLoadObserver(() => {
			video.poster = video.dataset.videoPoster;
		}, video, {
			root: scrollContainerOf(video),
			rootMargin: "100%"
		});
	});
}, "eventListeners");

/******************************************************************************/
/*	Enable clicking anywhere on a video (that has not yet loaded and started to
	play) to load it and start playing it. (Otherwise, only clicking the ‘play’
	button causes the video to load and play.)
 */
addContentInjectHandler(GW.contentInjectHandlers.enableVideoClickToPlay = (eventInfo) => {
    GWLog("enableVideoClickToPlay", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("video").forEach(video => {
		video.addEventListener("click", video.clickToPlayEvent = (event) => {
			video.play();
			video.removeEventListener("click", video.clickToPlayEvent);
			video.clickToPlayEvent = null;
		});
	});
}, "eventListeners");

/****************************************************************/
/*  Account for interaction between image-focus.js and popups.js.
 */
if (Extracts.popFrameProvider == Popups) {
	GW.notificationCenter.addHandlerForEvent("ImageFocus.imageOverlayDidAppear", (info) => {
		Popups.hidePopupContainer();
	});
	GW.notificationCenter.addHandlerForEvent("ImageFocus.imageOverlayDidDisappear", (info) => {
		Popups.unhidePopupContainer();
	});
	GW.notificationCenter.addHandlerForEvent("ImageFocus.imageDidFocus", (info) => {
		/*	Pin a popup when clicking to image-focus an image within it
			(unless it’s a popup that contains *only* the image, and nothing 
			 else - no metadata, no other content, nothing - in which case,
			 pinning is unnecessary).
		 */
		let popup = Popups.containingPopFrame(info.image);
		if (   popup
			&& (   popup.classList.contains("object")
				&& Annotations.isAnnotatedLink(popup.spawningTarget) == false) == false)
			Popups.pinPopup(popup);
	});
}


/***************/
/* CODE BLOCKS */
/***************/

/*************************************************************/
/*	Wrap each <pre> in a div.sourceCode (for layout purposes).
 */
addContentLoadHandler(GW.contentLoadHandlers.wrapPreBlocks = (eventInfo) => {
    GWLog("wrapPreBlocks", "rewrite.js", 1);

	wrapAll("pre", ".sourceCode", {
		useExistingWrapper: true,
		root: eventInfo.container
	});
}, "rewrite");

/********************************************************/
/*	EXPERIMENTAL: Highlight-on-hover for all code blocks.
 */
addContentLoadHandler(GW.contentLoadHandlers.addCodeBlockLineClasses = (eventInfo) => {
    GWLog("addCodeBlockLineClasses", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("code.sourceCode > span:not(.line)").forEach(lineSpan => {
		lineSpan.classList.add("line");
		if (lineSpan.innerHTML.length == 0)
			lineSpan.innerHTML = "&nbsp;";
	});

	eventInfo.container.querySelectorAll("pre code:not(.sourceCode)").forEach(codeBlock => {
		codeBlock.innerHTML = codeBlock.innerHTML.split("\n").map(
			line => `<span class="line">${(line || "&nbsp;")}</span>`
		).join("\n");
	});
}, "rewrite");

/*****************************************************************************/
/*	Allow for specifying code block classes by setting classes on the <pre>.
	(Workaround for a Pandoc peculiarity where classes set on a code block
	 are applied to the <pre> element and not on the div.sourceCode wrapper.)
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyCodeBlockClasses = (eventInfo) => {
    GWLog("rectifyCodeBlockClasses", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("pre").forEach(preBlock => {
        let wrapper = preBlock.closest("div.sourceCode");

        //  Tag the wrapper with the <pre>’s classes.
        [ "float-left", "float-right" ].forEach(preClass => {
            if (preBlock.classList.contains(preClass)) {
                wrapper.classList.add(preClass);
				preBlock.classList.remove(preClass);
			}
        });

		preBlock.classList.remove("float");
    });
}, "rewrite");

/**********************************************************************/
/*  Wrap each pre.width-full in a div.width-full (for layout purposes).
 */
addContentInjectHandler(GW.contentInjectHandlers.wrapFullWidthPreBlocks = (eventInfo) => {
    GWLog("wrapFullWidthPreBlocks", "rewrite.js", 1);

    wrapAll("pre.width-full", ".width-full", {
    	useExistingWrapper: true,
		root: eventInfo.container
	});
}, "rewrite", (info) => info.fullWidthPossible);


/**********/
/* EMBEDS */
/**********/

/******************************************************************************/
/*	There’s no way to tell whether an <iframe> has loaded, except to listen for 
	the `load` event. So, we implement our own checkable load flag, with a 
	class.
 */
addContentInjectHandler(GW.contentInjectHandlers.markLoadedEmbeds = (eventInfo) => {
    GWLog("markLoadedEmbeds", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("iframe.loaded-not").forEach(embed => {
		embed.addEventListener("load", (event) => {
			embed.classList.remove("loaded-not");
		});
	});
}, "eventListeners");

/**************************************************************************/
/*	Workaround for a Chrome bug that scrolls the parent page when an iframe 
	popup has a `src` attribute with a hash and that hash points to an 
	old-style anchor (`<a name="foo">`).
 */
addContentInjectHandler(GW.contentInjectHandlers.applyIframeScrollFix = (eventInfo) => {
    GWLog("applyIframeScrollFix", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("iframe.loaded-not").forEach(iframe => {
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
		}, { once: true });
	});
}, "eventListeners");


/***********/
/* COLUMNS */
/***********/

/*****************************************/
/*  Disable columns if only one list item.
 */
addContentLoadHandler(GW.contentLoadHandlers.disableSingleItemColumnBlocks = (eventInfo) => {
    GWLog("disableSingleItemColumnBlocks", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".columns > ul").forEach(columnList => {
        if (columnList.children.length == 1) {
            columnList.parentElement.classList.remove("columns");

	        if (columnList.parentElement.className == "")
	        	unwrap(columnList.parentElement);
		}
    });
}, "rewrite");


/**************/
/* INTERVIEWS */
/**************/

/****************************************/
/*	Rectify HTML structure of interviews.
 */
addContentLoadHandler(GW.contentLoadHandlers.rewriteInterviews = (eventInfo) => {
    GWLog("rewriteInterviews", "rewrite.js", 1);

	eventInfo.container.querySelectorAll(".interview, .interview > .collapse").forEach(interviewWrapper => {
		if (interviewWrapper.firstElementChild.tagName != "UL")
			return;

		let interview = newElement("UL", { class: `list ${interviewWrapper.className}` });

		for (let child of Array.from(interviewWrapper.children)) {
			if (child.tagName != "UL")
				continue;

			let exchange = interview.appendChild(newElement("LI", { class: "exchange" }));
			exchange.append(child.cloneNode(true));

			for (let utterance of exchange.firstElementChild.children) {
				utterance.classList.add("utterance");

				let speaker = utterance.querySelector("strong");

				//	If the speaker is wrapped, find the outermost wrapper.
				while (   speaker.parentElement
					   && speaker.parentElement.tagName != "P"
					   && speaker.nextSibling?.textContent.startsWith(":") != true)
					speaker = speaker.parentElement;
				speaker.classList.add("speaker");

				//	Move colon.
				(speaker.querySelector("strong") ?? speaker).innerHTML += ":";
				speaker.nextSibling.textContent = speaker.nextSibling.textContent.slice(1).trimStart();
			}
		}

		interviewWrapper.replaceWith(interview);
	});
}, "rewrite");


/****************/
/* MARGIN NOTES */
/****************/

/*************************************************************/
/*  Wrap the contents of all margin notes in an inner wrapper.
 */
addContentLoadHandler(GW.contentLoadHandlers.wrapMarginNotes = (eventInfo) => {
    GWLog("wrapMarginNotes", "rewrite.js", 1);

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

	aggregateMarginNotesInDocument(eventInfo.document);
}, "rewrite");


/**************/
/* TYPOGRAPHY */
/**************/

/*******************************************************************************/
/*  Apply various typographic fixes (educate quotes, inject <wbr> elements after
    certain problematic characters, etc.) in content transforms.

    Requires typography.js to be loaded prior to this file.
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyTypographyInContentTransforms = (eventInfo) => {
    GWLog("rectifyTypographyInContentTransforms", "rewrite.js", 1);

    Typography.processElement(eventInfo.container,
        (  Typography.replacementTypes.QUOTES
         | Typography.replacementTypes.WORDBREAKS
         | Typography.replacementTypes.ELLIPSES));

    //  Educate quotes in image alt-text.
    eventInfo.container.querySelectorAll("img").forEach(image => {
        image.alt = Typography.processString(image.alt, Typography.replacementTypes.QUOTES);
    });
}, "rewrite", (info) => (   info.contentType == "wikipediaEntry"
						 || info.contentType == "tweet"));

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

    if (Hyphenopoly.hyphenators == null)
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
    addWindowResizeListener(updateFullWidthBlockLayoutStyles, {
    	name: "updateFullWidthBlockLayoutStylesOnWindowResizeListener"
    });
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
			//	Compensate for block indentation due to nesting (e.g., lists).
        	let additionalLeftAdjustmentPx = "0px";
        	let enclosingListItem = fullWidthBlock.closest("li");
        	if (enclosingListItem) {
				let fullContentRect = fullWidthBlock.closest(".markdownBody").getBoundingClientRect();
				let listContentRect = enclosingListItem.firstElementChild.getBoundingClientRect();
				additionalLeftAdjustmentPx = (fullContentRect.x - listContentRect.x) + "px";
        	}

            fullWidthBlock.style.marginLeft = `calc(
                                                    (-1 * (var(--GW-full-width-block-layout-left-adjustment) / 2.0))
                                                  + (var(--GW-full-width-block-layout-side-margin))
                                                  - ((var(--GW-full-width-block-layout-page-width) - 100%) / 2.0)
                                                  + (${additionalLeftAdjustmentPx} / 2.0)
                                                )`;
            fullWidthBlock.style.marginRight = `calc(
                                                     (var(--GW-full-width-block-layout-left-adjustment) / 2.0)
                                                   + (var(--GW-full-width-block-layout-side-margin))
                                                   - ((var(--GW-full-width-block-layout-page-width) - 100%) / 2.0)
                                                   - (${additionalLeftAdjustmentPx} / 2.0)
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

        //  Rewrite title-link.
        partialAnnotation.querySelector("a.title-link").classList.add(Annotations.annotatedLinkFullClass);
    });
}, "<rewrite", (info) => (   info.source == "transclude"
                          && info.contentType == "annotation"));

/***************************************************************************/
/*	Apply proper classes to inline file-include collapses, both on directory 
	index pages and in annotations.
 */
addContentInjectHandler(GW.contentInjectHandlers.rectifyFileAppendClasses = (eventInfo) => {
    GWLog("rectifyFileAppendClasses", "rewrite.js", 1);

	eventInfo.container.querySelectorAll(".aux-links-transclude-file, .file-includes").forEach(fileIncludesBlock => {
		//	The file-include block itself may be a collapse! If so, wrap it.
		if (fileIncludesBlock.matches(".collapse"))
			fileIncludesBlock = wrapElement(fileIncludesBlock, "div.file-includes", { moveClasses: [ "data-field", "file-includes" ] });
		//	Rectify class.
		fileIncludesBlock.swapClasses([ "aux-links-transclude-file", "file-includes" ], 1);
		//	Apply standard class to all collapses within the includes block.
		fileIncludesBlock.querySelectorAll(".collapse").forEach(fileIncludeCollapse => {
			fileIncludeCollapse.swapClasses([ "aux-links-transclude-file", "file-include-collapse" ], 1);
			fileIncludeCollapse.swapClasses([ "bare-content", "bare-content-not" ], 1);
		});
	});
}, "rewrite");

/******************************************************************************/
/*	Properly handle file includes in annotations when their include-link fires.
 */
addContentInjectHandler(GW.contentInjectHandlers.handleFileIncludeUncollapseInAnnotations = (eventInfo) => {
    GWLog("handleFileIncludeUncollapseInAnnotations", "rewrite.js", 1);

	eventInfo.container.querySelectorAll(".file-include-collapse").forEach(fileIncludeCollapse => {
		let includeLink = fileIncludeCollapse.querySelector("a");
		GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (embedInjectEventInfo) => {
			/*	Don’t scroll to an embed in the main document if there are 
				popups on screen.
			 */
			if (   embedInjectEventInfo.document == document
				&& Extracts.popFrameProvider == Popups
				&& Popups.allSpawnedPopups().length > 0)
				return;

			let embed = embedInjectEventInfo.container.firstElementChild;

			//	Scroll into view (but not if it’s off-screen).
			if (isOnScreen(embed))
				scrollElementIntoView(embed);
			if (   embed.tagName == "IFRAME"
				&& Extracts.popFrameProvider.containingPopFrame(embed) != null)
				embed.addEventListener("load", (event) => {
					if (isOnScreen(embed))
						scrollElementIntoView(embed);
				});

			//	Designate now-last collapse for styling.
			let previousBlock = previousBlockOf(embed);
			if (   embed.closest(".collapse") == null
				&& previousBlock.classList.contains("collapse-block"))
				previousBlock.classList.add("last-collapse");
		}, {
			once: true,
			condition: (info) => (info.includeLink == includeLink)
		});
	});
}, "eventListeners", (info) => (info.contentType == "annotation"));

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
/* DIRECTORY INDEXES */
/*********************/

/******************************************************************************/
/*	On directory index pages, remove invalid include-links in file-append 
	sections; if no valid includes remain, delete the entire file-append block.
 */
addContentLoadHandler(GW.contentLoadHandlers.stripInvalidFileAppends = (eventInfo) => {
    GWLog("stripInvalidFileAppends", "rewrite.js", 1);

	eventInfo.container.querySelectorAll(".aux-links-transclude-file").forEach(fileAppendBlock => {
		/*	Remove any file embed links that lack a valid content type (e.g., 
			foreign-site links that have not been whitelisted for embedding).
		 */
		Transclude.allIncludeLinksInContainer(fileAppendBlock).forEach(includeLink => {
			if (Content.contentTypeForLink(includeLink) == null)
				includeLink.remove();
		});

		//	If no valid include-links remain, delete the whole block.
		if (isNodeEmpty(fileAppendBlock)) {
			//	Delete colon.
			if (fileAppendBlock.previousElementSibling.lastTextNode.nodeValue == ":")
				fileAppendBlock.previousElementSibling.lastTextNode.remove();

			fileAppendBlock.remove();
		}
	});
}, "rewrite", (info) => (   info.container == document.body
                         && /\/(index)?$/.test(location.pathname)));


/*********************/
/* LINK BIBLIOGRAPHY */
/*********************/

/*********************************************************************/
/*  Remove the “Link Bibliography:” bold text when transcluding a link
    bibliography into a page’s Link Bibliography section.
 */
addContentInjectHandler(GW.contentInjectHandlers.removeSubheadingsFromLinkBibliographies = (eventInfo) => {
    GWLog("removeSubheadingsFromLinkBibliographies", "rewrite.js", 1);

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
    if (TOC == null)
        return;

    TOC.classList.toggle("collapsed", collapsed);

    let button = TOC.querySelector(".toc-collapse-toggle-button");
    if (button == null)
        return;

    button.title = collapsed ? "Expand table of contents" : "Collapse table of contents";
}

/*******************************************************/
/*  Add the collapse toggle button to the main page TOC.
 */
addContentLoadHandler(GW.contentLoadHandlers.injectTOCMinimizeButton = (eventInfo) => {
    GWLog("injectTOCMinimizeButton", "rewrite.js", 1);

    let TOC = document.querySelector("#TOC");
    if (TOC == null)
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
/*  Strip spurious <span> tags (unavoidably added by Pandoc) from TOC links
	(only in the page-level TOC).
 */
addContentLoadHandler(GW.contentLoadHandlers.stripTOCLinkSpans = (eventInfo) => {
    GWLog("stripTOCLinkSpans", "rewrite.js", 1);

    unwrapAll(".TOC li a > span:not([class])", {
    	root: eventInfo.container
    });
}, "rewrite", (info) => (info.container == document.body));

/**************************************************************************/
/*  Update main page TOC with any sections within the initially loaded page
    that don’t already have TOC entries.
 */
addContentLoadHandler(GW.contentLoadHandlers.updateMainPageTOC = (eventInfo) => {
    GWLog("updateMainPageTOC", "rewrite.js", 1);

    updatePageTOC();
}, "rewrite", (info) => (info.container == document.body));

/*************************************************/
/*  Apply typography rectification to TOC entries.
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyTypographyInTOC = (eventInfo) => {
    GWLog("rectifyTypographyInTOC", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".TOC").forEach(TOC => {
        Typography.processElement(TOC, Typography.replacementTypes.WORDBREAKS);
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
    GWLog("updateTOCVisibility", "rewrite.js", 1);

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
    if (footnotesSection == null)
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
    GWLog("bindHighlightEventsToFootnoteSelfLinks", "rewrite.js", 1);

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
    GWLog("bindNoteHighlightEventsToCitations", "rewrite.js", 1);

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

    eventInfo.container.querySelectorAll("a[href]").forEach(link => {
        if (   eventInfo.localize == true
            && (   link.getAttribute("href").startsWith("#")
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
        if (target == null)
            return;

        link.dataset.linkIconType = "svg";
        link.dataset.linkIcon =
            (link.compareDocumentPosition(target) & Node.DOCUMENT_POSITION_FOLLOWING
             ? "arrow-down"
             : "arrow-up");
    });

    //  Local links (to other pages on the site).
    eventInfo.container.querySelectorAll(".link-page:not(.icon-not)").forEach(link => {
        if (   link.dataset.linkIcon
        	&& [ "arrow-down", "arrow-up" ].includes(link.dataset.linkIcon) == false)
            return;

        link.dataset.linkIconType = "text";
        link.dataset.linkIcon = [ "arrow-down", "arrow-up" ].includes(link.dataset.linkIcon)
        						? "\u{00B6}" // ¶
        						: "\u{1D50A}"; // 𝔊
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

        //  TOC links should never have link icons under any circumstances.
        ".TOC",

		//	No link icons in table headers.
		"thead"
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

/**************************************************************************/
/*	Enable special list icons for list items that contain recently modified 
	links.
 */
addContentInjectHandler(GW.contentInjectHandlers.enableRecentlyModifiedLinkListIcons = (eventInfo) => {
    GWLog("enableRecentlyModifiedLinkListIcons", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("li a.link-modified-recently").forEach(link => {
		let containingGraf = link.closest("p");
		if (containingGraf?.matches("li > p:only-of-type")) {
			containingGraf.parentElement.classList.add("link-modified-recently-list-item");
			link.classList.add("in-list");
		}
	});
}, "rewrite");

/****************************************************************************/
/*	Enable special icons for recently modified links (that are not in lists).
 */
addContentInjectHandler(GW.contentInjectHandlers.enableRecentlyModifiedLinkIcons = (eventInfo) => {
    GWLog("enableRecentlyModifiedLinkIcons", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("a.link-modified-recently:not(.in-list)").forEach(link => {
		if (link.querySelector(".recently-modified-icon-hook") != null)
			return;

		//	Inject indicator hook span.
		link.insertBefore(newElement("SPAN", { class: "recently-modified-icon-hook" }), link.firstChild);

		/*	Inject U+2060 WORD JOINER at start of first text node of the
			link. (It _must_ be injected as a Unicode character into the
			existing text node; injecting it within the .indicator-hook
			span, or as an HTML escape code into the text node, or in
			any other fashion, creates a separate text node, which
			causes all sorts of problems - text shadow artifacts, etc.)
		 */
		let linkFirstTextNode = link.firstTextNode;
		if (   linkFirstTextNode
			&& linkFirstTextNode.textContent.startsWith("\u{2060}") == false)
			linkFirstTextNode.textContent = "\u{2060}" + linkFirstTextNode.textContent;

		link.classList.add("has-recently-modified-icon");
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

GW.defaultImageAuxText = "[Image]";

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
        image.alt = (image.title || GW.defaultImageAuxText);
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
addContentLoadHandler(GW.contentLoadHandlers.designateColorInvertedContainers = (eventInfo) => {
    GWLog("designateColorInvertedContainers", "rewrite.js", 1);

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

	eventInfo.container.querySelectorAll(".admonition", ".admonition-title").forEach(paragraphizeTextNodesOfElement);
}, "rewrite");

/*********************************************/
/*	Fix incorrect text block tag types.

	- .text-center are <div> but should be <p>
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifySpecialTextBlockTagTypes = (eventInfo) => {
    GWLog("rectifySpecialTextBlockTagTypes", "rewrite.js", 1);

	eventInfo.container.querySelectorAll(".text-center").forEach(centerDiv => {
		unwrap(centerDiv, { moveClasses: true });
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


/************/
/* DROPCAPS */
/************/

/***************************************************/
/*	Dropcaps (only on sufficiently wide viewports).
 */
addContentInjectHandler(GW.contentInjectHandlers.rewriteDropcaps = (eventInfo) => {
    GWLog("rewriteDropcaps", "rewrite.js", 1);

	//	Reset dropcaps when margin note mode changes.
	doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "GW.dropcaps.resetDropcapsWhenMarginNoteModeChanges", (mediaQuery) => {
		eventInfo.container.querySelectorAll(GW.dropcaps.dropcapBlockSelector).forEach(resetDropcapInBlock);
	});

	//	A letter (capital or lowercase), optionally preceded by an opening quotation mark.
	let initialRegexp = new RegExp(/^(\s*[“‘]?)?([a-zA-Z])/);

	processContainerNowAndAfterBlockLayout(eventInfo.container, (container) => {
		container.querySelectorAll(GW.dropcaps.dropcapBlockSelector).forEach(dropcapBlock => {
			//	If this dropcap has already been processed, do nothing.
			if (dropcapBlock.querySelector(".dropcap"))
				return;

			//	Make sure the graf begins properly and determine initial letter.
			let initial = initialRegexp.exec(textContentOfGraf(dropcapBlock));
			if (initial == null) {
				addDropcapClassTo(dropcapBlock, "not");
				return;
			}
			let [ fullInitial, precedingPunctuation, initialLetter ] = initial;

			//	Locate insertion point.
			let firstNode = firstTextNodeOfGraf(dropcapBlock);
			let firstNodeParent = firstNode.parentElement;

			//	Separate first letter from rest of text content.
			firstNode.textContent = firstNode.textContent.slice(fullInitial.length);

			//	Determine dropcap type.
			let dropcapType = dropcapTypeOf(dropcapBlock);

			//	Is this is a graphical dropcap?
			if (GW.dropcaps.graphicalDropcapTypes.includes(dropcapType)) {
				//	Designate as graphical dropcap block.
				dropcapBlock.classList.add("graphical-dropcap");

				//	Inject a hidden span to hold the first letter as text.
				firstNodeParent.insertBefore(newElement("SPAN", {
					class: "hidden-initial-letter",
				}, {
					innerHTML: initialLetter
				}), firstNode);

				//	Construct the dropcap image element.
				let dropcapImage = newElement("IMG", {
					class: "dropcap figure-not",
					loading: "lazy"
				});

				//	Select a dropcap.
				let dropcapURL = randomDropcapURL(dropcapType, initialLetter);
				if (dropcapURL == null) {
					//	If no available dropcap image, set disabled flag.
					dropcapBlock.classList.add("disable-dropcap");
				} else {
					//	Specify image URL.
					dropcapImage.src = dropcapURL.pathname + dropcapURL.search;

					//	Add image file format class.
					dropcapImage.classList.add(dropcapURL?.pathname.slice(-3));

					/*	Dropcap should be inverted if it’s designed for a mode 
						opposite to the current mode (rather than being designed 
						either for the current mode or for either mode); in such a
						case it will have the opposite mode in the file name.
					 */
					let shouldInvert = dropcapURL.pathname.includes("-" + (DarkMode.computedMode() == "light" ? "dark" : "light"));
					if (shouldInvert)
						dropcapImage.classList.add("invert");
				}

				//	Inject the dropcap image element.
				firstNodeParent.insertBefore(dropcapImage, firstNode.previousSibling);
			} else {
				//	Inject the dropcap.
				firstNodeParent.insertBefore(newElement("SPAN", {
					class: "dropcap"
				}, {
					innerHTML: initialLetter.toUpperCase()
				}), firstNode);
			}

			//	If there’s punctuation before the initial letter, inject it.
			if (precedingPunctuation) {
				firstNodeParent.insertBefore(newElement("SPAN", {
					class: "initial-preceding-punctuation"
				}, {
					innerHTML: precedingPunctuation
				}), firstNodeParent.querySelector(".dropcap"));
			}
		});
	});
}, "rewrite", (info) => (   info.document == document
						 && GW.mediaQueries.mobileWidth.matches == false
						 && GW.isMobile() == false));

/***********************************************************/
/*	Activate mode-based dynamic graphical dropcap swapping.
 */
addContentInjectHandler(GW.contentInjectHandlers.activateDynamicGraphicalDropcaps = (eventInfo) => {
    GWLog("activateDynamicGraphicalDropcaps", "rewrite.js", 1);

	processContainerNowAndAfterBlockLayout(eventInfo.container, (container) => {
		container.querySelectorAll(GW.dropcaps.dropcapBlockSelector).forEach(dropcapBlock => {
			//	Determine dropcap type.
			let dropcapType = dropcapTypeOf(dropcapBlock);

			//	Is this a recognized graphical dropcap type?
			if (GW.dropcaps.graphicalDropcapTypes.includes(dropcapType) == false)
				return;

			//	Get the dropcap image element.
			let dropcapImage = dropcapBlock.querySelector("img.dropcap");
			if (dropcapImage == null)
				return;

			//	Get the initial letter.
			let initialLetter = dropcapBlock.querySelector(".hidden-initial-letter")?.textContent;
			if (initialLetter == null)
				return;

			//	If the handler already exists, do nothing.
			if (dropcapImage.modeChangeHandler)
				return;

			//	Add event handler to switch image when mode changes.
			GW.notificationCenter.addHandlerForEvent(dropcapImage.modeChangeHandler = "DarkMode.computedModeDidChange", (info) => {
				//	Clear disabled flag, if any.
				dropcapBlock.classList.remove("disable-dropcap");

				//	Get new dropcap URL.
				let dropcapURL = randomDropcapURL(dropcapType, initialLetter);
				if (dropcapURL == null) {
					//	If no available dropcap image, set disabled flag.
					dropcapBlock.classList.add("disable-dropcap");
					return;
				}

				//	Update image URL.
				dropcapImage.src = dropcapURL.pathname + dropcapURL.search;

				//	Update inversion.
				dropcapImage.classList.toggle("invert", dropcapURL.pathname.includes("-" + (DarkMode.computedMode() == "light" ? "dark" : "light")));

				//	Update image file format class.
				dropcapImage.classList.remove("png", "svg");
				dropcapImage.classList.add(dropcapURL.pathname.slice(-3));
			});
		});
	});
}, "eventListeners", (info) => (   info.document == document
								&& GW.mediaQueries.mobileWidth.matches == false
								&& GW.isMobile() == false));

/*********************/
/*	Linkify dropcaps.
 */
addContentInjectHandler(GW.contentInjectHandlers.linkifyDropcaps = (eventInfo) => {
    GWLog("linkifyDropcaps", "rewrite.js", 1);

	processContainerNowAndAfterBlockLayout(eventInfo.container, (container) => {
		container.querySelectorAll(GW.dropcaps.dropcapBlockSelector).forEach(dropcapBlock => {
			//	If this dropcap has already been linkified, do nothing.
			if (dropcapBlock.querySelector(".link-dropcap"))
				return;

			//	Determine dropcap type.
			let dropcapType = dropcapTypeOf(dropcapBlock);

			//	Determine initial letter.
			let initialLetter = (   dropcapBlock.querySelector("span.dropcap")
								 ?? dropcapBlock.querySelector(".hidden-initial-letter")).textContent;

			//	Get the dropcap (textual or graphical).
			let dropcap = dropcapBlock.querySelector(".dropcap");

			//	Wrap the dropcap (textual or graphical) in a link.
			let dropcapLink = newElement("A", {
				class: "link-page link-dropcap",
				href: "/dropcap#" + dropcapType,
				"data-letter": initialLetter,
				"data-dropcap-type": dropcapType
			});
			let dropcapLinkWrapper = newElement("SPAN");
			dropcapLinkWrapper.append(dropcapLink);
			dropcapLink.append(dropcap);

			//	Locate insertion point.
			let firstNode = firstTextNodeOfGraf(dropcapBlock);
			let firstNodeParent = firstNode.parentElement;
			if (firstNodeParent.matches(".initial-preceding-punctuation")) {
				firstNode = firstNodeParent.nextSibling;
				firstNodeParent = firstNodeParent.parentElement;
			} else if (firstNodeParent.matches(".hidden-initial-letter")) {
				firstNode = firstNodeParent;
				firstNodeParent = firstNodeParent.parentElement;
			}

			//	Inject the link-wrapped dropcap back into the block.
			firstNodeParent.insertBefore(dropcapLinkWrapper, firstNode);

			//	Process the link to enable extract pop-frames.
			Extracts.addTargetsWithin(dropcapLinkWrapper);

			//	Unwrap temporary wrapper.
			unwrap(dropcapLinkWrapper);
		});
	});
}, "rewrite", (info) => (   info.document == document
						 && GW.mediaQueries.mobileWidth.matches == false
						 && GW.isMobile() == false));

/***********************************************************************/
/*	Prevent blocks with dropcaps from overlapping the block below them.
 */
addContentInjectHandler(GW.contentInjectHandlers.preventDropcapsOverlap = (eventInfo) => {
    GWLog("preventDropcapsOverlap", "rewrite.js", 1);

	let blocksNotToBeOverlappedSelector = [
		"p[class*='dropcap-']",
		"section",
		"blockquote",
		".collapse",
		".list-heading",
		".in-list",
		"div.sourceCode"
	].join(", ");

	processContainerNowAndAfterBlockLayout(eventInfo.container, (container) => {
		container.querySelectorAll("p[class*='dropcap-']").forEach(dropcapBlock => {
			let nextBlock = nextBlockOf(dropcapBlock);
			if (   nextBlock == null
				|| nextBlock.matches(blocksNotToBeOverlappedSelector))
				dropcapBlock.classList.add("overlap-not");
		});
	});
}, ">rewrite", (info) => (   info.document == document
						  && GW.mediaQueries.mobileWidth.matches == false
						  && GW.isMobile() == false));


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
window.addEventListener("beforeprint", GW.beforePrintHandler = (event) => {
    GWLog("Print command received.", "rewrite.js", 1);

    function expand(container) {
        Transclude.allIncludeLinksInContainer(container).forEach(includeLink => {
			if (includeLink.closest("#link-bibliography, .link-bibliography-append"))
				return;

            Transclude.transclude(includeLink, true);
        });

        container.querySelectorAll(".collapse").forEach(expandLockCollapseBlock);
    }

    GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", GW.expandAllContentWhenLoadingPrintView = (eventInfo) => {
        expand(eventInfo.container);
    }, {
    	condition: (info) => (info.document == document)
    });

    expand(document);
});
window.addEventListener("afterprint", GW.afterPrintHandler = (event) => {
    GWLog("Print command completed.", "rewrite.js", 1);

    GW.notificationCenter.removeHandlerForEvent("GW.contentDidInject", GW.expandAllContentWhenLoadingPrintView);
});


/*****************************************************************************************/
/*! instant.page v5.1.0 - (C) 2019-2020 Alexandre Dieulot - https://instant.page/license */
/* Settings: 'prefetch' (loads HTML of target) after 1600ms hover (desktop) or mouse-down-click (mobile); TODO: left in logging for testing during experiment */
let pls="a:not(.has-content)";let t,e;const n=new Set,o=document.createElement("link"),z=o.relList&&o.relList.supports&&o.relList.supports("prefetch")&&window.IntersectionObserver&&"isIntersecting"in IntersectionObserverEntry.prototype,s="instantAllowQueryString"in document.body.dataset,a=true,r="instantWhitelist"in document.body.dataset,c="instantMousedownShortcut"in document.body.dataset,d=1111;let l=1600,u=!1,f=!1,m=!1;if("instantIntensity"in document.body.dataset){const t=document.body.dataset.instantIntensity;if("mousedown"==t.substr(0,"mousedown".length))u=!0,"mousedown-only"==t&&(f=!0);else if("viewport"==t.substr(0,"viewport".length))navigator.connection&&(navigator.connection.saveData||navigator.connection.effectiveType&&navigator.connection.effectiveType.includes("2g"))||("viewport"==t?document.documentElement.clientWidth*document.documentElement.clientHeight<45e4&&(m=!0):"viewport-all"==t&&(m=!0));else{const e=parseInt(t);isNaN(e)||(l=e)}}if(z){const n={capture:!0,passive:!0};if(f||document.addEventListener("touchstart",function(t){e=performance.now();const n=t.target.closest(pls);if(!h(n))return;v(n.href)},n),u?c||document.addEventListener("mousedown",function(t){const e=t.target.closest(pls);if(!h(e))return;v(e.href)},n):document.addEventListener("mouseover",function(n){if(performance.now()-e<d)return;const o=n.target.closest(pls);if(!h(o))return;o.addEventListener("mouseout",p,{passive:!0}),t=setTimeout(()=>{v(o.href),t=void 0},l)},n),c&&document.addEventListener("mousedown",function(t){if(performance.now()-e<d)return;const n=t.target.closest("a");if(t.which>1||t.metaKey||t.ctrlKey)return;if(!n)return;n.addEventListener("click",function(t){1337!=t.detail&&t.preventDefault()},{capture:!0,passive:!1,once:!0});const o=new MouseEvent("click",{view:window,bubbles:!0,cancelable:!1,detail:1337});n.dispatchEvent(o)},n),m){let t;(t=window.requestIdleCallback?t=>{requestIdleCallback(t,{timeout:1500})}:t=>{t()})(()=>{const t=new IntersectionObserver(e=>{e.forEach(e=>{if(e.isIntersecting){const n=e.target;t.unobserve(n),v(n.href)}})});document.querySelectorAll("a").forEach(e=>{h(e)&&t.observe(e)})})}}function p(e){e.relatedTarget&&e.target.closest("a")==e.relatedTarget.closest("a")||t&&(clearTimeout(t),t=void 0)}function h(t){if(t&&t.href&&(!r||"instant"in t.dataset)&&(a||t.origin==location.origin||"instant"in t.dataset)&&["http:","https:"].includes(t.protocol)&&("http:"!=t.protocol||"https:"!=location.protocol)&&(s||!t.search||"instant"in t.dataset)&&!(t.hash&&t.pathname+t.search==location.pathname+location.search||"noInstant"in t.dataset))return!0}function v(t){if(n.has(t))return;const e=document.createElement("link");console.log("Prefetched: "+t);e.rel="prefetch",e.href=t,document.head.appendChild(e),n.add(t)};
