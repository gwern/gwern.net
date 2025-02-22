/*******************/
/* INJECT TRIGGERS */
/*******************/

GW.elementInjectTriggers = { };
GW.defunctElementInjectTriggers = { };

/****************************************************************************/
/*  Register element inject trigger for the given uuid. (In other words, when
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
/*  Watch for element injections in the given document. Process injected
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
/*  Returns a placeholder element that, when injected, replaces itself with the
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
/*  Generate new UUIDs for any placeholder elements in the given container.
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

    return (  `<svg
    			xmlns="http://www.w3.org/2000/svg"
    			viewBox="${viewBox}"
    			>`
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

/***************************************************************************/
/*	Convenience function for shared code between uses of getAssetPathname().
 */
function processAssetSequenceOptions(options, metaOptions) {
	metaOptions = Object.assign({
		currentAssetURL: null,
		assetSavedIndexKey: null
	}, metaOptions);

	let sequenceIndex, sequenceCurrent;
	if (GW.allowedAssetSequencingModes.includes(options.sequence) == false) {
		sequenceIndex = null;
		sequenceCurrent = null;
	} else if (options.sequence.endsWith("Current")) {
		for (let prefix of [ "next", "previous" ])
			if (options.sequence.startsWith(prefix))
				sequenceIndex = prefix;

		sequenceCurrent = metaOptions.currentAssetURL.pathname;
	} else {
		let savedIndexKey = metaOptions.assetSavedIndexKey;
		let savedIndex = localStorage.getItem(savedIndexKey);
		if (   savedIndex == null
			&& options.randomize) {
			sequenceIndex = rollDie(1E6);
			localStorage.setItem(savedIndexKey, sequenceIndex);
		} else if (options.sequence.startsWith("next")) {
			sequenceIndex = savedIndex == null
							? 1
							: parseInt(savedIndex) + 1;
			localStorage.setItem(savedIndexKey, sequenceIndex);
		} else {
			sequenceIndex = savedIndex == null
							? 0
							: parseInt(savedIndex) - 1;
			localStorage.setItem(savedIndexKey, sequenceIndex);
		}

		sequenceCurrent = null;
	}

	return { sequenceIndex, sequenceCurrent };
}

/*****************************************************************************/
/*  Return an asset pathname (not versioned), given a pathname regular
	expression pattern (in string form, not a RegExp object), with ‘%R’ where
	a number should be, e.g.:

        /static/img/logo/christmas/light/logo-christmas-light-%R(\\.svg|-small-1x\\.(png|jpg|webp))

    will return files with pathnames like:

        /static/img/logo/christmas/light/logo-christmas-light-1-small-1x.png
        /static/img/logo/christmas/light/logo-christmas-light-1-small-1x.jpg
        /static/img/logo/christmas/light/logo-christmas-light-1-small-1x.webp
		/static/img/logo/christmas/light/logo-christmas-light-1.svg

    (Or -2, -3, etc.)

    Specified assets must be listed in the versioned asset database.

	By default, selects uniform-randomly from all available asset pathnames
	matching the provided pattern. (But see option fields, below.)

	Available option fields:

	sequenceIndex (integer)
	sequenceIndex (string)
		If this field is set to an integer value, then, instead of returning a
		random asset pathname out of the asset pathnames matching the provided
		pattern, selects the i’th one, where i is equal to (sequenceIndex - 1)
		modulo the number of matching asset pathnames.

		If this field is set to a string value, then it must be either “next”
		or “previous”, and the `sequenceCurrent` field must also be set; if
		these conditions are not met, null is returned. (See the
		`sequenceCurrent` field, below, for details on this option.)

	sequenceCurrent (string)
		If the `sequenceIndex` field is not set to a string value of either
		“next” or “previous”, this field is ignored.

		If `sequenceIndex` is set to “next”, and the value of this field is
		equal to a value of one of the asset pathnames that match the provided
		pattern, then the next pattern in the set of matching patterns is
		returned (wrapping around to the first value after the last one).

		If `sequenceIndex` is set to “previous”, and the value of this field
		is equal to a value of one of the asset pathnames that match the
		provided pattern, then the previous pattern in the set of matching
		patterns is returned (wrapping around to the last value after the
		first).

		If the value of this field does not match any of the asset pathnames
		that match the provided pattern (including if it is null), then, if
		`sequenceIndex` is set to “next”, it behaves as if `sequenceIndex` had
		been set to 1; and if `sequenceIndex` is set to “previous”, it behaves
		as if `sequenceIndex` had been set to 0 (i.e., the first or the last
		pattern in the set of matching patterns is returned).
 */
function getAssetPathname(assetPathnamePattern, options) {
	options = Object.assign({
		sequenceIndex: null,
		sequenceCurrent: null
	}, options);

    let assetPathnameRegExp = new RegExp(assetPathnamePattern.replace("%R", "[0-9]+"));
    let matchingAssetPathnames = [ ];
    for (versionedAssetPathname of Object.keys(GW.assetVersions)) {
        if (assetPathnameRegExp.test(versionedAssetPathname))
            matchingAssetPathnames.push(versionedAssetPathname);
    }

	if (matchingAssetPathnames.length == 0) {
		return null;
	} else if (options.sequenceIndex == null) {
		return matchingAssetPathnames[rollDie(matchingAssetPathnames.length) - 1];
	} else if (typeof options.sequenceIndex == "number") {
		return matchingAssetPathnames[modulo(options.sequenceIndex - 1, matchingAssetPathnames.length)];
	} else if (typeof options.sequenceIndex == "string") {
		if ([ "next", "previous" ].includes(options.sequenceIndex) == false)
			return null;

		let currentIndex = matchingAssetPathnames.indexOf(options.sequenceCurrent);
		if (currentIndex == -1) {
			return (options.sequenceIndex == "next"
					? matchingAssetPathnames.first
					: matchingAssetPathnames.last);
		} else {
			return (options.sequenceIndex == "next"
					? matchingAssetPathnames[modulo(currentIndex + 1, matchingAssetPathnames.length)]
					: matchingAssetPathnames[modulo(currentIndex - 1, matchingAssetPathnames.length)]);
		}
	} else {
		return null;
	}
}


/*******************/
/* IMAGE OUTLINING */
/*******************/

GW.outlineOrNot = { };
GW.outlineOrNotAPIEndpoint = "https://api.obormot.net/outlineornot/url";

/******************************************************************************/
/*	Returns true if the given image’s outlining status has been set (i.e., if
	it has one of the classes [ "outline", "outline-auto", "outline-not",
	"outline-not-auto" ]), false otherwise.
 */
function outliningJudgmentHasBeenAppliedToImage(image) {
	return (image.classList.containsAnyOf([ "outline", "outline-auto", "outline-not", "outline-not-auto" ]) == true);
}

/*****************************************************************************/
/*  Returns true if the given image should be outlined (i.e., the outlineOrNot
	API has judged this image to be outline-requiring), false if the image
	should not be outlined (i.e., the outlineOrNot API has judged this image
	to be non-outline-requiring, null if no judgment is available.
 */
function outliningJudgmentForImage(image) {
    return (GW.outlineOrNot[Images.smallestAvailableImageSizeURLForImage(image).href]?.outline ?? null);
}

/*****************************************************************************/
/*	Applies available (i.e., requested and received from the outlineOrNot API)
	image outlining judgment data to the given image, and returns true if this
	was done successfully. If no such data is available for the given image,
	does nothing (and returns false). Likewise does nothing (and returns null)
	for images which already have their outlining status specified.
 */
function applyImageOutliningJudgment(image) {
	if (outliningJudgmentHasBeenAppliedToImage(image))
		return null;

	let outliningJudgment = outliningJudgmentForImage(image);
	if (outliningJudgment != null) {
		image.classList.add(outliningJudgment == true ? "outline-auto" : "outline-not-auto");
		return true;
	} else {
		return false;
	}
}

/*****************************************************************************/
/*  Sends request to outlineOrNot for judgments about whether the images in the
    given container ought to be outlined.
 */
function requestImageOutliningJudgmentsForImagesInContainer(container) {
	/*	Disable, for now.
			—SA 2024-12-18
	 */
	return;

    let imageURLs = Array.from(container.querySelectorAll("figure img")).map(image => {
    	let imageURL = Images.smallestAvailableImageSizeURLForImage(image);
        return (   imageURL.pathname.match(/\.(png|jpe?g$)/i)
        		&& GW.invertOrNot[imageURL.href] == null)
        	   ? imageURL.href
        	   : null;
    }).filter(x => x);
    if (imageURLs.length == 0)
        return;

    doAjax({
        location: GW.outlineOrNotAPIEndpoint,
        method: "POST",
        serialization: "JSON",
        responseType: "json",
        params: imageURLs,
        onSuccess: (event) => {
            event.target.response.forEach(imageInfo => {
                GW.outlineOrNot[imageInfo.url] = {
                    outline: (imageInfo.outline == 1)
                };
            });

			GW.notificationCenter.fireEvent("GW.imageOutliningJudgmentsAvailable", { judgments: event.target.response });
        },
        onFailure: (event) => {
            console.log(event);
        }
    });
}


/*******************/
/* IMAGE INVERSION */
/*******************/

GW.invertOrNot = { };
GW.invertOrNotAPIEndpoint = "https://invertornot.com/api/url";

/******************************************************************************/
/*	Returns true if the given image’s inversion status has been set (i.e., if
	it has one of the classes [ "invert", "invert-auto", "invert-not",
	"invert-not-auto" ]), false otherwise.
 */
function inversionJudgmentHasBeenAppliedToImage(image) {
	return (image.classList.containsAnyOf([ "invert", "invert-auto", "invert-not", "invert-not-auto" ]) == true);
}

/****************************************************************************/
/*  Returns true if the given image should be inverted in dark mode (i.e.,
	the invertOrNot API has judged this image to be invertible), false if the
	image should not be inverted (i.e., the invertOrNot API has judged this
	image to be non-invertible, null if no judgment is available.
 */
function inversionJudgmentForImage(image) {
    return (GW.invertOrNot[Images.smallestAvailableImageSizeURLForImage(image).href]?.invert ?? null);
}

/*****************************************************************************/
/*	Applies available (i.e., requested and received from the invertOrNot API)
	image inversion judgment data to the given image, and returns true if this
	was done successfully. If no such data is available for the given image,
	does nothing (and returns false). Likewise does nothing (and returns null)
	for images which already have their inversion status specified.
 */
function applyImageInversionJudgment(image) {
	if (inversionJudgmentHasBeenAppliedToImage(image))
		return null;

	let inversionJudgment = inversionJudgmentForImage(image);
	if (inversionJudgment != null) {
		image.classList.add(inversionJudgment == true ? "invert-auto" : "invert-not-auto");
		return true;
	} else {
		return false;
	}
}

/*****************************************************************************/
/*  Sends request to invertOrNot for judgments about whether the images in the
    given container ought to be inverted.
 */
function requestImageInversionJudgmentsForImagesInContainer(container) {
    let imageURLs = Array.from(container.querySelectorAll("figure img")).map(image => {
    	let imageURL = Images.smallestAvailableImageSizeURLForImage(image);
        return (   imageURL.pathname.match(/\.(png|jpe?g$)/i)
        		&& GW.invertOrNot[imageURL.href] == null)
        	   ? imageURL.href
        	   : null;
    }).filter(x => x);
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

			GW.notificationCenter.fireEvent("GW.imageInversionJudgmentsAvailable", { judgments: event.target.response });
        },
        onFailure: (event) => {
            console.log(event);
        }
    });
}


/**********/
/* IMAGES */
/**********/

Images = {
    thumbnailBasePath: "/metadata/thumbnail/",

    thumbnailDefaultSize: "256",

	thumbnailSizeFromURL: (url) => {
		if (typeof url == "string")
			url = URLFromString(url);

		return parseInt(url.pathname.slice(Images.thumbnailBasePath.length).split("/")[0]);
	},

	smallestAvailableImageSizeURLForImage: (image) => {
		return (Images.thumbnailURLForImage(image) ?? Images.fullSizeURLForImage(image));
	},

	fullSizeURLForImage: (image) => {
		return URLFromString(image.dataset.srcSizeFull ?? image.src);
	},

    thumbnailURLForImageURL: (imageSrcURL, size = Images.thumbnailDefaultSize) => {
        if (imageSrcURL.hostname != location.hostname)
            return null;

        return URLFromString(  Images.thumbnailBasePath
                             + size + "px/"
                             + fixedEncodeURIComponent(fixedEncodeURIComponent(imageSrcURL.pathname)));
    },

    thumbnailURLForImage: (image, size = Images.thumbnailDefaultSize) => {
		if (Images.isSVG(image))
			return null;

        return (Images.isThumbnail(image)
        		? URLFromString(image.src)
        		: Images.thumbnailURLForImageURL(URLFromString(image.src)));
    },

    thumbnailifyImage: (image) => {
		if (Images.isSVG(image))
			return;

    	if (Images.isThumbnail(image))
    		return;

        let thumbnailURL = Images.thumbnailURLForImage(image);
        if (thumbnailURL) {
            image.dataset.srcSizeFull = image.src;
            image.src = thumbnailURL.href;
        }
    },

	isSVG: (image) => {
		return (URLFromString(image.src).pathname.toLowerCase().endsWith(".svg"));
	},

	isThumbnail: (image) => {
		return (image.dataset.srcSizeFull > "");
	},

	unthumbnailifyImage: (image) => {
		if (Images.isThumbnail(image)) {
			image.src = image.dataset.srcSizeFull;
			delete image.dataset.srcSizeFull;
		}
	}
};


/***********************/
/* PROGRESS INDICATORS */
/***********************/

/**************************************************************************/
/*	Returns SVG source for a progress-indicator SVG icon, given a specified
	progress percentage (in [0,100]).
 */
function arcSVGForProgressPercent (percent) {
	let svgOpeningTagSrc = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512">`;
	let svgClosingTagSrc = `</svg>`;

	let strokeWidth = GW.isMobile() ? 64.0 : 56.0;
	let boxRadius = 256.0;
	let radius = boxRadius - (strokeWidth * 0.5);

	let backdropCircleGray = (GW.isMobile() ? 110.0 : 170.0) + (percent * 0.64);
	let backdropCircleColor = Color.hexStringFromRGB({
		red: backdropCircleGray,
		green: backdropCircleGray,
		blue: backdropCircleGray
	});
	let backdropCircleSrc = `<circle cx="${boxRadius}" cy="${boxRadius}" r="${radius}"`
						  + ` stroke-width="${strokeWidth}" stroke="${backdropCircleColor}" fill="none"/>`;

	let arcAttributesSrc = `fill="none" stroke="#000" stroke-width="${strokeWidth}" stroke-linecap="round"`;
	let arcSrc;
	if (percent == 100) {
		arcSrc = `<circle cx="${boxRadius}" cy="${boxRadius}" r="${radius}" ${arcAttributesSrc}/>`;
	} else {
		let angle = 2.0 * Math.PI * ((percent / 100.0) - 0.25);
		let y = (radius * Math.sin(angle)) + boxRadius;
		let x = (radius * Math.cos(angle)) + boxRadius;
		let largeArc = percent > 50 ? "1" : "0";
		arcSrc = `<path
				   d="M ${boxRadius} ${strokeWidth * 0.5} A ${radius} ${radius} 0 ${largeArc} 1 ${x} ${y}"
				   ${arcAttributesSrc}/>`;
	}

	return (svgOpeningTagSrc + backdropCircleSrc + arcSrc + svgClosingTagSrc);
}

/*****************************************************************************/
/*	Given an element with a `data-progress-percentage` attribute, injects an
	inline icon displaying the specified progress percentage. (The icon will
	automatically be further processed for display by the inline icon system.)
 */
function renderProgressPercentageIcon(progressIndicator) {
	let svgSrc = arcSVGForProgressPercent(parseInt(progressIndicator.dataset.progressPercentage));
	progressIndicator.querySelector(".progress-indicator-icon")?.remove();
	progressIndicator.appendChild(newElement("SPAN", {
		class: "progress-indicator-icon icon-special",
		style: `--icon-url: url("data:image/svg+xml;utf8,${encodeURIComponent(svgSrc)}")`
	}));
}


/*************/
/* DOCUMENTS */
/*************/

/*  Return the location (URL) associated with a document.
    (Document|DocumentFragment) => URL
 */
function baseLocationForDocument(doc) {
	if (doc == null) {
		return null;
	} else if (doc == document) {
        return URLFromString(location.href);
    } else if (doc.baseLocation) {
        return URLFromString(doc.baseLocation.href);
    } else {
        return null;
    }
}


/*********/
/* LINKS */
/*********/

/******************************************************************************/
/*  Returns true if the link is an annotated link, OR if it is an include-link
    which transclude.js treats as an annotation transclude. (This is relevant
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
        //  HAX. (Remove when link IDs are fixed. —SA 2023-03-22)
        /*  Disabling this hack, hopefully it’s no longer needed.
            (See also line below.) —SA 2023-04-29
         */
//      let exactBacklinkSelector = null;
//      if (anchor.startsWith("#gwern")) {
//          let targetID = "#" + anchor.slice(("#gwern" + link.dataset.backlinkTargetUrl.slice(1).replace("/", "-") + "-").length);
//          if (targetID > "")
//              exactBacklinkSelector = `a[href*='${CSS.escape(link.dataset.backlinkTargetUrl + targetID)}']`;
//      }

        let backlinkSelector = `a[href*='${CSS.escape(link.dataset.backlinkTargetUrl)}']:not(.backlink-not)`;
        let exclusionSelector = [
            "#page-metadata a",
            ".aux-links-list a"
        ].join(", ");
        /*  Disabling this hack, hopefully it’s no longer needed.
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
/*  Copy the provided text to the clipboard.
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
/*  Create scratchpad for synthetic copy operations.
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
    GWLog("registerCopyProcessorsForDocument", "misc.js", 1);

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
	hashForCitationRegexp: new RegExp("^#fnref[0-9]+$"),

	hashMatchesCitation: (hash = location.hash) => {
		return Notes.hashForCitationRegexp.test(hash);
	},

	hashForFootnoteRegexp: new RegExp("^#fn[0-9]+$"),

	hashMatchesFootnote: (hash = location.hash) => {
		return Notes.hashForFootnoteRegexp.test(hash);
	},

	hashForSidenoteRegexp: new RegExp("^#sn[0-9]+$"),

	hashMatchesSidenote: (hash = location.hash) => {
		return Notes.hashForSidenoteRegexp.test(hash);
	},

    /*  Get the (side|foot)note number from a URL hash (which might point to a
        footnote, a sidenote, or a citation).
     */
    noteNumberFromHash: (hash = location.hash) => {
        if (   Notes.hashMatchesFootnote(hash)
        	|| Notes.hashMatchesSidenote(hash))
            return hash.substr(3);
        else if (Notes.hashMatchesCitation(hash))
            return hash.substr(6);
        else
            return "";
    },

    noteNumber: (element) => {
        return Notes.noteNumberFromHash(element.hash ?? ("#" + element.id));
    },

    citationIdForNumber: (number) => {
        return `fnref${number}`;
    },

    footnoteIdForNumber: (number) => {
        return `fn${number}`;
    },

    sidenoteIdForNumber: (number) => {
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

        //  Footnote backlinks.
        let backlinksListLabelLink = footnote.querySelector(".section-backlinks .backlinks-list-label a");
        if (backlinksListLabelLink) {
            //  #fnN
            backlinksListLabelLink.hash = backlinksListLabelLink.hash.slice(0, 3) + number;

            //  N
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
    //  Don’t show margin notes block if there are fewer notes than this.
    minimumAggregatedNotesCount: 3,

    aggregationNeededInDocuments: [ ]
};

/****************************************************************************/
/*  Aggregate margin notes, on the next animation frame, if not already done.
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
/*  Aggregate margin notes.
 */
function aggregateMarginNotesInDocument(doc) {
    GWLog("aggregateMarginNotesInDocument", "misc.js", 2);

    let marginNotesBlockClass = "margin-notes-block";

    doc.querySelectorAll(".marginnote").forEach(marginNote => {
        if (marginNote.classList.contains("only-icon"))
            return;

        let section = marginNote.closest("section, .markdownBody, .annotation-abstract");
        if (section == null)
            return;

        let marginNotesBlock = section.querySelector(`#${(CSS.escape(section.id))}-${marginNotesBlockClass}`);
        if (marginNotesBlock == null) {
            /*  Construct the margin notes block. It should go after any
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

            //  Inject the margin notes block.
            marginNotesBlock = newElement("P", {
                class: marginNotesBlockClass,
                id: `${section.id}-${marginNotesBlockClass}`
            });
            firstBlock.parentElement.insertBefore(marginNotesBlock, firstBlock);
        }

        //  Clone the note.
        let clonedNote = marginNote.cloneNode(true);

        //  Set margin note type class.
        clonedNote.swapClasses([ "inline", "sidenote" ], 0);

        //  Unwrap the inner wrapper (unneeded here).
        unwrap(clonedNote.querySelector(".marginnote-inner-wrapper"));

        //  Remove dropcap, if any.
        resetDropcapInBlock(clonedNote);

        //  Trim whitespace.
        clonedNote.innerHTML = clonedNote.innerHTML.trim();

        //  Strip brackets.
        /*  Reason: we use brackets for editorial insertions & commentary,
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

        //  Strip trailing period.
        if (clonedNote.textContent.endsWith("."))
            clonedNote.lastTextNode.nodeValue = clonedNote.lastTextNode.nodeValue.slice(0, -1);

        //  Prevent duplication.
        if (Array.from(marginNotesBlock.children).findIndex(child => {
                return clonedNote.textContent == child.textContent;
            }) != -1)
            return;

        //  Append.
        marginNotesBlock.append(clonedNote);

        //  Process the new entries to activate pop-frame spawning.
        Extracts.addTargetsWithin(marginNotesBlock);
    });

    //  Update visibility of margin note blocks.
    doc.querySelectorAll(`.${marginNotesBlockClass}`).forEach(marginNotesBlock => {
        marginNotesBlock.classList.toggle("hidden", marginNotesBlock.children.length < GW.marginNotes.minimumAggregatedNotesCount);
    });
}

/***************************************************************************/
/*  Child nodes of a paragraph, excluding any margin notes in sidenote mode.
 */
function nodesOfGraf(graf) {
    return Array.from(graf.childNodes).filter(node => ((node instanceof Element && node.matches(".marginnote.sidenote")) == false));
}

/*****************************************************************************/
/*  Text content of a paragraph, excluding the contents of any margin notes in
    sidenote mode.
 */
function textContentOfGraf(graf) {
    return nodesOfGraf(graf).map(node => node.textContent).join("");
}

/******************************************************************************/
/*  First text node of a paragraph, skipping any margin notes in sidenote mode.
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
/*  Update page TOC, on the next animation frame, if not already done.
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

    //  Collect new entries, for later processing (if need be).
    let newEntries = [ ];

    container.querySelectorAll("#markdownBody section").forEach(section => {
        //  If this section already has a TOC entry, return.
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
                            class='decorate-not'
                            id='toc-${section.id}'
                            href='#${fixedEncodeURIComponent(section.id)}'
                                >${entryText}</a>`;

        //  Get or construct the <ul> element.
        let subList = (   Array.from(parentTOCElement.childNodes).find(child => child.tagName == "UL")
                       ?? parentTOCElement.appendChild(newElement("UL")));

        //  Insert and store.
        subList.insertBefore(entry, followingTOCElement);
        newEntries.push(entry);
    });

    //  Process the new entries to activate pop-frame spawning.
    newEntries.forEach(Extracts.addTargetsWithin);

    //  Rectify typography in new entries.
    newEntries.forEach(entry => {
        Typography.processElement(entry, Typography.replacementTypes.WORDBREAKS);
    });

    //  Update visibility.
    updateTOCVisibility(TOC);
}


/*************/
/* FOOTNOTES */
/*************/

/*****************************************************************************/
/*  Mark hash-targeted footnote with ‘targeted’ class.
 */
function updateFootnoteTargeting() {
    GWLog("updateFootnoteTargeting", "misc.js", 1);

    if (   Sidenotes
        && Sidenotes.mediaQueries.viewportWidthBreakpoint.matches)
        return;

    //  Clear any existing targeting.
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
/*  Returns URL of a graphical dropcap of the given type and letter,
    appropriate for the current mode and the viewport’s device pixel ratio.

	For an explanation of the available option fields, see the
	`injectSpecialPageLogo()` function in special-occasions.js.
 */
function getDropcapURL(dropcapType, letter, options) {
	options = Object.assign({
		mode: DarkMode.computedMode(),
		identifier: null,
		randomize: true,
		sequence: null
	}, options);

	//	Identifier string (empty, or hyphen plus a number, or “-%R”).
    let dropcapIdentifierRegexpString = ``;
    if (options.identifier) {
    	dropcapIdentifierRegexpString = `-${options.identifier}`;
    } else if (   options.randomize == true
    		   || GW.allowedAssetSequencingModes.includes(options.sequence)) {
    	dropcapIdentifierRegexpString = `-%R`;
    }

	/*	Bitmap files come in several scales (for different pixel densities of
		display); SVGs are singular.
	 */
    let scale = valMinMax(Math.ceil(window.devicePixelRatio), 1, 2);
    let fileFormatRegexpSuffix = `(\\.svg|-small-${scale}x\\.(png|jpg|webp))$`;

	/*	File name pattern further depends on whether we have separate light
		and dark dropcaps of this sort.
	 */
	let dropcapPathnamePattern = `/static/font/dropcap/${dropcapType}/`
							   + (options.mode
							      ? `(${options.mode}/)?`
							      : ``)
							   + letter.toUpperCase()
							   + `(-.+)?`
							   + dropcapIdentifierRegexpString
							   + fileFormatRegexpSuffix;
    let dropcapPathname = getAssetPathname(dropcapPathnamePattern, processAssetSequenceOptions(options, {
    	assetSavedIndexKey: `dropcap-sequence-index-${dropcapType}`
    }));
    if (dropcapPathname == null)
        return null;

    return versionedAssetURL(dropcapPathname);
}

/*****************************************************************************/
/*  Reset dropcap in the given block to initial state (as it was prior to the
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

    //  If this is a graphical dropcap block...
    let dropcapImage = block.querySelector("img.dropcap");
    if (dropcapImage) {
        //  Remove mode change handler.
        GW.notificationCenter.removeHandlerForEvent(dropcapImage.modeChangeHandler, "DarkMode.computedModeDidChange");

        //  Remove graphical dropcap.
        dropcapImage.remove();
    }

    //  Text node surgery: reattach letter.
    let letterSpan = block.querySelector("span.dropcap, span.hidden-initial-letter");
    letterSpan.nextSibling.textContent = letterSpan.textContent + letterSpan.nextSibling.textContent;
    letterSpan.remove();

    //  Text node surgery: reattach preceding punctuation (if any).
    let precedingPunctuation = block.querySelector("span.initial-preceding-punctuation");
    if (precedingPunctuation) {
        precedingPunctuation.nextSibling.textContent = precedingPunctuation.textContent + precedingPunctuation.nextSibling.textContent;
        precedingPunctuation.remove();
    }
}


/**************/
/* TYPOGRAPHY */
/**************/

/****************************************************************************/
/*	Strips all special HTML structure within date range elements in the given
	block.
 */
function stripDateRangeMetadataInBlock(block) {
	block.querySelectorAll(".date-range").forEach(dateRange => {
		//	Remove subscripts.
		dateRange.querySelectorAll("sub").forEach(sub => {
			sub.remove();
		});

		//	Unwrap superscript and sub+sup span wrapper.
		unwrap(dateRange.querySelector(".subsup"));
		unwrap(dateRange.querySelector("sup"));

		//	Remove ‘title’ attribute.
		dateRange.removeAttribute("title");
	});
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
/*  Returns relevant scroll container for the given element. Null is returned
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
/*  Returns page scroll position, as integer (percentage).
 */
function getPageScrollPosition() {
    return Math.round(100 * (window.pageYOffset / (document.documentElement.offsetHeight - window.innerHeight)));
}

/*********************************************************************/
/*  Returns a saved (in local storage) integer, or 0 if nothing saved.
 */
function getSavedCount(key) {
    return parseInt(localStorage.getItem(key) ?? "0");
}

/*****************************************************************************/
/*  Add 1 to a saved (in local storage) integer, or set it to 1 if none saved.
 */
function incrementSavedCount(key) {
    localStorage.setItem(key, getSavedCount(key) + 1);
}

/*****************************************************/
/*	Reset (delete) a saved (in local storage) integer.
 */
function resetSavedCount(key) {
	localStorage.removeItem(key);
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
    demoCollapseDelay: 9000,

    /*  These values must be synced with CSS. Do not modify them in isolation!
        (Listed variables that correspond to each parameter are in default.css.
         Divide these values by 1000 and specify them in seconds, e.g. a value
         of 250 becomes a CSS value of `0.25s`.)
     */
    collapseDuration: 250, // --GW-page-toolbar-collapse-duration
    demoCollapseDuration: 1000, // --GW-page-toolbar-slow-collapse-duration
    fadeAfterCollapseDuration: 250, // --GW-page-toolbar-fade-after-collapse-duration

    //  Do not modify these two values without updating CSS also!
    widgetFlashRiseDuration: 1000, // --GW-page-toolbar-widget-flash-rise-duration
    widgetFlashFallDuration: 1000, // --GW-page-toolbar-widget-flash-fall-duration
    widgetFlashStayDuration: 500,

    toolbar: null,

    setupComplete: false,

    /*  Adds and returns page toolbar. (If page toolbar already exists, returns
        existing page toolbar.)

        NOTE: This function may run before GW.pageToolbar.setup().
     */
    getToolbar: () => {
        return (    GW.pageToolbar.toolbar
                ?? (GW.pageToolbar.toolbar = addUIElement(`<div id="page-toolbar"><div class="widgets"></div></div>`,
                                                          { raiseOnHover: true })));
    },

    /*  Adds a widget (which may contain buttons or whatever else) (first
        creating it from HTML, if necessary) to the page toolbar, and returns
        the added widget.

        NOTE: This function may run before GW.pageToolbar.setup().
     */
    addWidget: (widget) => {
        if (typeof widget == "string")
            widget = elementFromHTML(widget);

        widget.classList.add("widget");
		widget.querySelectorAll("button").forEach(button => {
			button.classList.add("widget-button");
		});

        //  Add widget.
        GW.pageToolbar.getToolbar().querySelector(".widgets").appendChild(widget);

        //  If setup has run, update state after adding widget.
        if (GW.pageToolbar.setupComplete)
            GW.pageToolbar.updateState();

        return widget;
    },

    /*  Removes a widget with the given ID and returns it.

        NOTE: This function may run before GW.pageToolbar.setup().
     */
    removeWidget: (widgetID) => {
        let widget = GW.pageToolbar.getToolbar().querySelector(`.widget#${widgetID}`);
        if (widget == null)
            return null;

        widget.remove();

        //  If setup has run, update state after removing widget.
        if (GW.pageToolbar.setupComplete)
            GW.pageToolbar.updateState();

        return widget;
    },

    /*  Returns the widget with the given ID; or null, if no such widget ID.
     */
    getWidget: (widgetID) => {
        return GW.pageToolbar.getToolbar().querySelector(`.widget#${widgetID}`);
    },

    flashWidget: (widgetID, options) => {
		options = Object.assign({
			flashStayDuration: null,
			showSelectedButtonLabel: false,
			highlightSelectedButtonLabelAfterDelay: null
		}, options);

        let widget = GW.pageToolbar.getToolbar().querySelector(`.widget#${widgetID}`);
        if (widget == null)
            return null;

        widget.classList.add("flashing");
        if (options.showSelectedButtonLabel) {
            setTimeout(() => { widget.classList.add("show-selected-button-label"); },
            		   GW.pageToolbar.widgetFlashRiseDuration * 0.5);

			if (options.highlightSelectedButtonLabelAfterDelay != null)
				setTimeout(() => { widget.classList.add("highlight-selected-button-label"); },
						   GW.pageToolbar.widgetFlashRiseDuration + options.highlightSelectedButtonLabelAfterDelay);
        }
        setTimeout(() => {
            widget.swapClasses([ "flashing", "flashing-fade" ], 1);
            setTimeout(() => {
                widget.classList.remove("flashing-fade");
            }, GW.pageToolbar.widgetFlashFallDuration);
            if (options.showSelectedButtonLabel) {
                setTimeout(() => { widget.classList.remove("show-selected-button-label"); },
                		   GW.pageToolbar.widgetFlashFallDuration * 0.5);

			if (options.highlightSelectedButtonLabelAfterDelay != null)
				setTimeout(() => { widget.classList.remove("highlight-selected-button-label"); },
						   GW.pageToolbar.widgetFlashFallDuration);
            }
        }, GW.pageToolbar.widgetFlashRiseDuration + (options.flashStayDuration ?? GW.pageToolbar.widgetFlashStayDuration));
    },

    isCollapsed: () => {
        return GW.pageToolbar.toolbar.classList.contains("collapsed");
    },

    isTempExpanded: () => {
        return GW.pageToolbar.toolbar.classList.contains("expanded-temp");
    },

    /*  Collapse or uncollapse toolbar. (The second argument uncollapses
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

		let isCollapsed = GW.pageToolbar.isCollapsed();

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

		//	Fire event, if need be.
		if (isCollapsed != GW.pageToolbar.isCollapsed()) {
			GW.notificationCenter.fireEvent("GW.pageToolbarCollapseStateDidChange", {
				collapse: collapse,
				collapseOptions: options
			});
		}
    },

    /*  Collapse toolbar.

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

    /*  Uncollapse toolbar.

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

    /*  Fade toolbar to full transparency.
     */
    fade: () => {
        GW.pageToolbar.toolbar.classList.add("faded");
    },

    /*  Un-fade toolbar from full transparency.
     */
    unfade: () => {
        GW.pageToolbar.toolbar.classList.remove("faded");
    },

    /*  Temporarily add one or more classes to the toolbar. Takes 2 or more
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

    /*  Update layout, position, and collapse state of toolbar.
        (Called when window is scrolled or resized, and also when widgets are
         added or removed.)
     */
    updateState: (event) => {
        if (   event
            && event.type == "scroll"
            && (   GW.isMobile()
            	|| GW.pageToolbar.toolbar.matches(":hover") == false)) {
            //  Collapse on scroll.
            let thresholdScrollDistance = (0.2 * window.innerHeight);
            if (   GW.scrollState.unbrokenUpScrollDistance   > (0.2 * window.innerHeight)
                || GW.scrollState.unbrokenDownScrollDistance > (0.2 * window.innerHeight))
                GW.pageToolbar.toggleCollapseState(true);

            //  Fade on scroll; unfade when scrolling to top.
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

	setPositionOffset: (offset) => {
		if (GW.pageToolbar.toolbar == null)
			return;

		GW.pageToolbar.toolbar.style.setProperty("--toolbar-offset-x", offset.x + "px");
		GW.pageToolbar.toolbar.style.setProperty("--toolbar-offset-y", offset.y + "px");
	},

	shouldStartCollapsed: () => {
		return (   GW.isTorBrowser()
        		|| getSavedCount("page-toolbar-demos-count") >= GW.pageToolbar.maxDemos);
	},

    setup: () => {
        GW.pageToolbar.toolbar = GW.pageToolbar.getToolbar();

        let startCollapsed = GW.pageToolbar.shouldStartCollapsed();
        if (startCollapsed) {
            //  Don’t collapse if hovering.
            if (GW.pageToolbar.toolbar.matches(":hover") == false)
                GW.pageToolbar.toggleCollapseState(true);
        }

        GW.pageToolbar.toolbar.append(
            newElement("BUTTON", {
                type: "button",
                title: "Collapse/expand controls",
                class: "toggle-button main-toggle-button",
                tabindex: "-1",
                accessKey: "t"
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

		//	Toolbar toggle button click event handler.
		let buttonClickHandler = (event) => {
			//  Left-click only.
			if (event.button != 0)
				return;

			if (GW.pageToolbar.isTempExpanded()) {
				/*  Do not re-collapse if temp-expanded; instead,
					permanentize expanded state (expand-lock).
				 */
				GW.pageToolbar.toggleCollapseState(false);
			} else {
				//  Expand or collapse.
				GW.pageToolbar.toggleCollapseState();
			}
		};

		/*	Inject inline mode widgets in already-loaded content, and add
			rewrite processor to inject any inline widgets in subsequently
			loaded content.
		 */
		processMainContentAndAddRewriteProcessor("addInlineToolbarToggleWidgetsInContainer", (container) => {
			container.querySelectorAll(".toolbar-mode-selector-inline").forEach(element => {
				let widgetHTML = `<span class="toolbar-toggle-widget mode-selector mode-selector-inline`
							   + (startCollapsed ? " toolbar-collapsed" : "")
							   + `">`
							   + `<button
							   	   type="button"
							   	   class="toggle-button"
							   	   title="Collapse/expand controls"
							   	   tabindex="-1">`
							   + `<span class="icon">`
							   + GW.svg("gear-solid")
							   + `</span>`
							   + `</button>`
							   + `</span>`;
				let widget = elementFromHTML(widgetHTML);
				element.replaceWith(widget);
				widget.querySelector("button").addEventListener("click", buttonClickHandler);
				wrapParenthesizedNodes("inline-mode-selector", widget);

				GW.notificationCenter.addHandlerForEvent("GW.pageToolbarCollapseStateDidChange", (eventInfo) => {
					widget.classList.toggle("toolbar-collapsed", eventInfo.collapse);
				});
			});
		});

        //  Activate buttons.
        GW.pageToolbar.toolbar.querySelectorAll("button.toggle-button").forEach(button => {
            //  Toggle collapse state on click/tap.
            button.addEventListener("click", buttonClickHandler);

            if (button.classList.contains("main-toggle-button")) {
                if (GW.isMobile()) {
                    //  Unfade on tap.
                    button.addEventListener("mousedown", (event) => {
                        GW.pageToolbar.unfade();
                    });
                } else {
                    //  Unfade on hover.
                    GW.pageToolbar.toolbar.addEventListener("mouseenter", (event) => {
                        GW.pageToolbar.unfade();
                    });

                    //  Uncollapse on hover.
                    onEventAfterDelayDo(button, "mouseenter", GW.pageToolbar.hoverUncollapseDelay, (event) => {
                        if (GW.pageToolbar.isCollapsed())
                            GW.pageToolbar.toggleCollapseState(false, { temp: true });
                    }, {
                        cancelOnEvents: [ "mouseleave", "mousedown" ]
                    });

                    //  Collapse on unhover.
                    onEventAfterDelayDo(GW.pageToolbar.toolbar, "mouseleave", GW.pageToolbar.unhoverCollapseDelay, (event) => {
                        if (GW.pageToolbar.isTempExpanded())
                            GW.pageToolbar.toggleCollapseState(true);
                    }, {
                        cancelOnEvents: [ "mouseenter" ]
                    });
                }
            }
        });

        //  Set initial state.
        GW.pageToolbar.updateState();

        doWhenPageLoaded(() => {
            /*  Slowly collapse toolbar shortly after page load (if it’s not
                already collapsed).
             */
            let startCollapsed = GW.pageToolbar.shouldStartCollapsed();
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

                    //  Don’t collapse if hovering.
                    if (GW.pageToolbar.toolbar.matches(":hover") == false)
                        GW.pageToolbar.toggleCollapseState(true, {
                                                              slow: true,
                                                              delay: GW.pageToolbar.demoCollapseDelay
                                                           });
                });

                incrementSavedCount("page-toolbar-demos-count");
            }

            //  Update toolbar state on scroll.
            addScrollListener(GW.pageToolbar.updateState, {
                name: "updatePageToolbarStateOnScrollListener",
                defer: true
            });

            //  Update toolbar state on window resize.
            addWindowResizeListener(GW.pageToolbar.updateState, {
                name: "updatePageToolbarStateOnWindowResizeListener",
                defer: true
            });
        });

        GW.pageToolbar.setupComplete = true;
    },

	expandToolbarFlashWidgetDoThing: (widgetId, doThing, options) => {
		options = Object.assign({
			widgetFlashStayDuration: 3000,
			doThingDelay: 250
		}, options);

		//	Expand toolbar.
		GW.pageToolbar.toggleCollapseState(false);

		setTimeout(() => {
			GW.pageToolbar.flashWidget(widgetId, {
				flashStayDuration: options.widgetFlashStayDuration,
				showSelectedButtonLabel: true,
				highlightSelectedButtonLabelAfterDelay: options.doThingDelay
			});
			setTimeout(() => {
				doThing();

				//	Collapse toolbar, after a delay.
				GW.pageToolbar.toggleCollapseState(true, {
													   delay: GW.pageToolbar.demoCollapseDelay
															+ options.widgetFlashStayDuration
															+ GW.pageToolbar.widgetFlashFallDuration
												   });
			}, GW.pageToolbar.widgetFlashRiseDuration + options.doThingDelay);
		}, GW.pageToolbar.collapseDuration);
	}
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
    GWLog("updateBackToTopLinkVisibility", "misc.js", 3);

    //  One PgDn’s worth of scroll distance, approximately.
    let onePageScrollDistance = (0.8 * window.innerHeight);

    let pageScrollPosition = getPageScrollPosition();

    //  Hide back-to-top link when scrolling to top.
    if (pageScrollPosition == 0)
        GW.backToTop.classList.toggle("hidden", true);
    //  Show back-to-top link when scrolling to bottom.
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
    GWLog("injectBackToTopLink", "misc.js", 1);

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

/***********************************************************/
/*	Rewrite footer logo link to also link to #top on /index.
 */
addContentLoadHandler(GW.contentLoadHandlers.rewriteIndexFooterLogoLinkHref = (eventInfo) => {
    GWLog("rewriteIndexFooterLogoLinkHref", "misc.js", 1);

    eventInfo.container.querySelectorAll("#footer-decoration-container .footer-logo").forEach(footerLogo => {
        footerLogo.href = "#top";
    });
}, "rewrite", (info) => (   info.container == document.main
                         && /\/(index)?$/.test(location.pathname)));


/*******************/
/* FLOATING HEADER */
/*******************/

GW.floatingHeader = {
    minimumYOffset: 0,

    maxChainLength: 6,

	//	Mobile only.
    maxHeaderHeight: 60,

    chainLinkClasses: {
        "…": "ellipsis",
        "header": "page-title"
    },

    currentTrail: [ ],

	isHidden: () => {
		return GW.floatingHeader.header?.classList.contains("hidden");
	},

    /*  Show/hide the floating header, and update state, in response to
        scroll event.

        (Called by the ‘updateFloatingHeaderScrollListener’ scroll listener.)
     */
    updateState: (event, maxChainLength = GW.floatingHeader.maxChainLength) => {
        GWLog("updateFloatingHeaderState", "misc.js", 3);

        //  Show/hide the entire header.
        GW.floatingHeader.header.classList.toggle("hidden",
            window.pageYOffset < GW.floatingHeader.minimumYOffset);

        //  Update scroll indicator bar.
        GW.floatingHeader.scrollIndicator.dataset.scrollPosition = getPageScrollPosition();
        GW.floatingHeader.scrollIndicator.style.backgroundSize = `${GW.floatingHeader.scrollIndicator.dataset.scrollPosition}% 100%`;

        //  Update breadcrumb display.
        let trail = GW.floatingHeader.getTrail();
		if (GW.isMobile()) {
			/*	We must update the display if either the current position in the
				page has changed (i.e., we’ve scrolled), or if we are having to
				re-compute the state due to having to reduce the header height
				from what it would be if we were displaying the entire current
				trail (i.e. if this is a recursive call).
			 */
			if (   trail.join("/") != GW.floatingHeader.currentTrail.join("/")
				|| maxChainLength < GW.floatingHeader.maxChainLength) {
				GW.floatingHeader.linkChain.classList.toggle("truncate-page-title", trail.length > maxChainLength);

				let chainLinks = GW.floatingHeader.constructLinkChain(trail, maxChainLength);
				GW.floatingHeader.linkChain.replaceChildren(...chainLinks);
				chainLinks.forEach(link => {
					let span = wrapElement(link, "span.link", { moveClasses: true });

					//	Enable special link click behavior.
					link.addActivateEvent(GW.floatingHeader.linkInChainClicked);
				});

				//  Constrain header height.
				if (   GW.floatingHeader.header.offsetHeight > GW.floatingHeader.maxHeaderHeight
					&& maxChainLength > 1) {
					GW.floatingHeader.updateState(event, maxChainLength - 1);
					return;
				}

				//	Update current trail.
				GW.floatingHeader.currentTrail = trail;
			}

			/*	Update page toolbar position offset, so that the header does not
				block the page toolbar toggle button.
			 */
			GW.pageToolbar.setPositionOffset(new DOMPoint(0, GW.floatingHeader.isHidden() == false
															 ? -1 * GW.floatingHeader.header.offsetHeight
															 : 0));
		} else {
        	/*	We must update the display if the current position in the page
        		has changed (i.e., we’ve scrolled).
        	 */
			if (trail.join("/") != GW.floatingHeader.currentTrail.join("/")) {
				let chainLinks = GW.floatingHeader.constructLinkChain(trail, maxChainLength);
				GW.floatingHeader.linkChain.replaceChildren(...chainLinks);
				let index = 0;
				chainLinks.forEach(link => {
					let span = wrapElement(link, "span.link", { moveClasses: true });

					//	Enable layout based on nesting level.
					span.style.setProperty("--link-index", index++);
				});

				//	Chain links should spawn section popups.
				Extracts.addTargetsWithin(GW.floatingHeader.linkChain);

				//	Update current trail.
				GW.floatingHeader.currentTrail = trail;
			}
		}
    },

    getTrail: () => {
        let element = document.elementFromPoint(window.innerWidth / 2, 20);

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
        if (Extracts.popFrameProvider == Popins)
            Popins.removeAllPopins();
    },

    setup: () => {
        GWLog("GW.floatingHeader.setup", "misc.js", 1);

        //  No floating header on desktop /index.
        if (   GW.isMobile() == false
            && /\/(index)?$/.test(location.pathname))
            return;

        //  Inject header.
        if (GW.isMobile()) {
			GW.floatingHeader.header = addUIElement(  `<div id="floating-header" class="hidden position-bottom">`
													+ `<div class="scroll-indicator"></div>`
													+ `<div class="link-chain"></div>`
													+ `</div>`);

        } else {
			GW.floatingHeader.header = addUIElement(  `<div id="floating-header" class="hidden position-top">`
													+ `<div class="link-chain"></div>`
													+ `<div class="scroll-indicator"></div>`
													+ `</div>`);
        }

        //  Designate desktop version of header.
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

		//	Ensure that popin positioning takes header height into account.
		if (GW.isMobile()) {
			if (window["Popins"] == null) {
				GW.notificationCenter.addHandlerForEvent("Popins.didLoad", (info) => {
					Popins.windowBottomPopinPositionMargin = GW.floatingHeader.maxHeaderHeight;
				}, { once: true });
			} else {
				Popins.windowBottomPopinPositionMargin = GW.floatingHeader.maxHeaderHeight;
			}
		}
    }
};

doWhenPageLoaded(GW.floatingHeader.setup);


/***************************/
/* POP-FRAME SPAWN WIDGETS */
/***************************/

GW.popFrameSpawnWidgets = {
	widgetTypes: {
		template: {
			//	Configuration.
			linkHref: null,
			linkAdditionalAttributes: null,
			iconName: null,
			onPopupPinDo: null,
			addToolbarWidget: false,
			toolbarWidgetLabel: null,
			inlineWidgetReplacedElementSelector: null,
			keyCommand: null,
			additionalSetup: null,
			additionalWidgetActivation: null,

			//	Defaults.
			keyCommandSpawnWidgetFlashStayDuration: 3000,

			//	Infrastructure.
			toolbarWidget: null,
			virtualWidget: null
		}
	},

	addWidgetType: (widgetTypeName, widgetTypeSpec) => {
		return (GW.popFrameSpawnWidgets.widgetTypes[widgetTypeName] = Object.assign({ },
			GW.popFrameSpawnWidgets.widgetTypes.template,
			widgetTypeSpec,
			{ name: widgetTypeName }));
	},

	pinPopup: (popup) => {
		if (popup == null)
			return;

		let widgetType = GW.popFrameSpawnWidgets.widgetTypes[popup.spawningTarget.closest(".link-widget").dataset.widgetType];

		Popups.pinPopup(popup);
		Popups.bringPopupToFront(popup);

		if (widgetType.onPopupPinDo != null)
			requestAnimationFrame(() => { widgetType.onPopupPinDo(popup); });
	},

	activateWidget: (widget) => {
		let widgetType = GW.popFrameSpawnWidgets.widgetTypes[widget.dataset.widgetType];

		widget.widgetLink.onclick = () => false;

        //  Activate pop-frames.
        Extracts.addTargetsWithin(widget);

        //  Configure pop-frame behavior.
        if (Extracts.popFrameProvider == Popups) {
            //  Configure popup positioning and click response.
            widget.widgetLink.cancelPopupOnClick = () => false;
			widget.widgetLink.keepPopupAttachedOnPin = () => true;
            if (   widget == widgetType.toolbarWidget
            	|| widget == widgetType.virtualWidget)
	            widget.widgetLink.preferPopupSidePositioning = () => true;

            //  Pin popup if widget is clicked.
            widget.widgetLink.addActivateEvent((event) => {
				if (widget.widgetLink.popup == null) {
					//  When the popup spawns, pin it.
					GW.notificationCenter.addHandlerForEvent("Popups.popupDidSpawn", (info) => {
						requestAnimationFrame(() => {
							GW.popFrameSpawnWidgets.pinPopup(info.popup);
						});
					}, {
						once: true,
						condition: (info) => (info.popup.spawningTarget == widget.widgetLink)
					});

					//  Spawn popup.
					Popups.spawnPopup(widget.widgetLink);
				} else {
					GW.popFrameSpawnWidgets.pinPopup(widget.widgetLink.popup);
				}
            });
        }

		//	Run any additional activation code.
		if (widgetType.additionalWidgetActivation != null)
			widgetType.additionalWidgetActivation(widget);
	},

	setup: (widgetType) => {
		if (widgetType.addToolbarWidget == true) {
			//	Add.
			let widgetHTML = `<div
							   id="${widgetType.name}-widget"
							   class="link-widget"
							   data-widget-type="${widgetType.name}">`
						   + `<a
						   	   class="${widgetType.name} widget-button no-footer-bar"
						   	   href="${widgetType.linkHref}" `
						   + (Object.entries(widgetType.linkAdditionalAttributes ?? { }).map(
						   		([ attrName, attrValue ]) => `${attrName}="${attrValue}"`
						   	  ).join(" "))
						   + `>`
						   + `<span class="icon">`
						   + GW.svg(widgetType.iconName)
						   + `</span>`
						   + `<span class="label">${widgetType.toolbarWidgetLabel}</span>`
						   + `</a></div>`;
			widgetType.toolbarWidget = GW.pageToolbar.addWidget(widgetHTML);
			widgetType.toolbarWidget.widgetLink = widgetType.toolbarWidget.querySelector("a");

			//	Activate.
			GW.popFrameSpawnWidgets.activateWidget(widgetType.toolbarWidget);
		} else if (widgetType.keyCommand != null) {
			//	Create “virtual widget”.
			let widgetHTML = `<div
							   id="${widgetType.name}-widget"
							   class="link-widget"
							   data-widget-type="${widgetType.name}">`
						   + `<a class="${widgetType.name} no-footer-bar"
								 href="${widgetType.linkHref}" `
						   + (Object.entries(widgetType.linkAdditionalAttributes ?? { }).map(
						   		([ attrName, attrValue ]) => `${attrName}="${attrValue}"`
						   	  ).join(" "))
						   + `></a></div>`;
			widgetType.virtualWidget = GW.popFrameSpawnWidgets.virtualWidgetContainer.appendChild(elementFromHTML(widgetHTML));
			widgetType.virtualWidget.widgetLink = widgetType.virtualWidget.querySelector("a");

			//	Activate.
			GW.popFrameSpawnWidgets.activateWidget(widgetType.virtualWidget);
		}

		if (widgetType.inlineWidgetReplacedElementSelector != null) {
			/*	Inject inline mode widgets in already-loaded content, and add
				rewrite processor to inject any inline widgets in subsequently
				loaded content.
			 */
			processMainContentAndAddRewriteProcessor("addInline_" + widgetType.name + "_widgetsInContainer", (container) => {
				container.querySelectorAll(widgetType.inlineWidgetReplacedElementSelector).forEach(element => {
					let widgetHTML = `<span class="link-widget" data-widget-type="${widgetType.name}">`
								   + `<a class="${widgetType.name} no-footer-bar"
								   		 href="${widgetType.linkHref}" `
								   + (Object.entries(widgetType.linkAdditionalAttributes ?? { }).map(
								   		([ attrName, attrValue ]) => `${attrName}="${attrValue}"`
								   	  ).join(" "))
								   + `>`
								   + `<span class="icon-${widgetType.iconName}"></span>`
								   + `</a>`;
					let widget = elementFromHTML(widgetHTML);
					widget.widgetLink = widget.querySelector("a");
					element.replaceWith(widget);
					wrapParenthesizedNodes("inline-mode-selector", widget);

					//	Activate.
					GW.popFrameSpawnWidgets.activateWidget(widget);
				});
			});
		}

		if (widgetType.additionalSetup != null)
			widgetType.additionalSetup(widgetType);
	},
};

doWhenPageLoaded(() => {
	//	Add virtual widget container.
	GW.popFrameSpawnWidgets.virtualWidgetContainer = document.body.appendChild(newElement("DIV", { id: "virtual-widget-container" }));

	//	Add event handler for widget key commands.
	GW.notificationCenter.addHandlerForEvent("GW.keyWasPressed", GW.popFrameSpawnWidgets.keyPressEventHandler = (eventInfo) => {
		eventInfo.keyUpEvent.preventDefault();

		let widgetType = Object.values(GW.popFrameSpawnWidgets.widgetTypes).find(widgetType => widgetType.keyCommand == eventInfo.key);

		if (widgetType.toolbarWidget != null) {
			//  Expand page toolbar.
			GW.pageToolbar.toggleCollapseState(false, {
				temp: (GW.pageToolbar.isCollapsed() || GW.pageToolbar.isTempExpanded())
			});

			//	Flash widget.
			GW.pageToolbar.flashWidget(widgetType.toolbarWidget.id, {
				flashStayDuration: widgetType.keyCommandSpawnWidgetFlashStayDuration
			});
		}

		let mainWidgetLink = (widgetType.toolbarWidget ?? widgetType.virtualWidget).widgetLink;
		if (mainWidgetLink.popup == null) {
			//  When the popup spawns, pin it.
			GW.notificationCenter.addHandlerForEvent("Popups.popupDidSpawn", (info) => {
				requestAnimationFrame(() => {
					GW.popFrameSpawnWidgets.pinPopup(mainWidgetLink.popup);
				});
			}, {
				once: true,
				condition: (info) => (info.popup.spawningTarget == mainWidgetLink)
			});

			//  Spawn popup.
			Popups.spawnPopup(mainWidgetLink);
		} else {
			GW.popFrameSpawnWidgets.pinPopup(mainWidgetLink.popup);
		}
	}, {
		condition: (info) => (Object.values(GW.popFrameSpawnWidgets.widgetTypes).map(
			widgetType => widgetType.keyCommand
		).filter(x => x).includes(info.key))
	});

	//	Set up help widget(s).
	GW.popFrameSpawnWidgets.setup(GW.popFrameSpawnWidgets.addWidgetType("help", {
		linkHref: "/help",
		linkAdditionalAttributes: null,
		iconName: "question-solid",
		onPopupPinDo: null,
		addToolbarWidget: true,
		toolbarWidgetLabel: "Help",
		inlineWidgetReplacedElementSelector: ".help-mode-selector-inline",
		keyCommand: "?"
	}));

	//	Set up search widget(s).
	GW.popFrameSpawnWidgets.setup(GW.popFrameSpawnWidgets.addWidgetType("search", {
		linkHref: "/static/google-search.html",
		linkAdditionalAttributes: { "aria-label": "Search site with Google Search",
									"data-link-content-type": "local-document" },
		iconName: "magnifying-glass",
		onPopupPinDo: (popup) => { popup.document.querySelector("iframe")?.contentDocument?.querySelector("input")?.focus(); },
		addToolbarWidget: true,
		toolbarWidgetLabel: "Search",
		inlineWidgetReplacedElementSelector: ".search-mode-selector-inline",
		keyCommand: "/",
		additionalSetup: (widgetType) => {
			//	Add DNS-prefetch tag.
			//	See https://developer.mozilla.org/en-US/docs/Web/Performance/dns-prefetch
			document.head.appendChild(elementFromHTML(`<link rel="dns-prefetch" href="https://www.google.com/search" />`));
		},
		additionalWidgetActivation: (widget) => {
			//	Function to set the proper mode (auto, light, dark) in the iframe.
			let updateSearchIframeMode = (iframe) => {
				iframe.contentDocument.querySelector("#search-styles-dark").media = DarkMode.mediaAttributeValues[DarkMode.currentMode()];
			};

			//  Event handler for popup spawn / popin inject.
			let popFrameSpawnEventHandler = (eventInfo) => {
				let popFrame = (eventInfo.popup ?? eventInfo.popin);
				let iframe = popFrame.document.querySelector("iframe");
				iframe.addEventListener("load", (event) => {
					//	Set proper mode.
					updateSearchIframeMode(iframe);

					//	Add handler to update search pop-frame when switching modes.
					GW.notificationCenter.addHandlerForEvent("DarkMode.didSetMode", iframe.darkModeDidSetModeHandler = (info) => {
						updateSearchIframeMode(iframe)
					});

					let inputBox = iframe.contentDocument.querySelector("input.search");

					//  Focus search box on load.
					inputBox.focus();
					inputBox.addEventListener("blur", (event) => {
						inputBox.focus();
					});

					if (Extracts.popFrameProvider == Popups) {
						//	Pin popup if text is entered.
						inputBox.addEventListener("input", (event) => {
							Popups.pinPopup(popFrame);
						});

						//	Enable normal popup Esc-key behavior.
						iframe.contentDocument.addEventListener("keyup", (event) => {
							let allowedKeys = [ "Escape", "Esc" ];
							if (allowedKeys.includes(event.key) == false)
								return;

							event.preventDefault();

							if (Popups.popupIsPinned(popFrame)) {
								Popups.unpinPopup(popFrame);
							} else {
								Popups.despawnPopup(popFrame);
							}
						});
					}

					//	Enable “search where” functionality.
					let searchWhereSelector = iframe.contentDocument.querySelector("#search-where-selector");
					searchWhereSelector.querySelectorAll("input").forEach(radioButton => {
						radioButton.addEventListener("change", (event) => {
							searchWhereSelector.querySelectorAll("input").forEach(otherRadioButton => {
								otherRadioButton.removeAttribute("checked");
							});
							radioButton.setAttribute("checked", "");
						});
					});

					//	Enable submit override (to make site search work).
					iframe.contentDocument.querySelector(".searchform").addEventListener("submit", (event) => {
						event.preventDefault();

						let form = event.target;
						form.querySelector("input.query").value = searchWhereSelector.querySelector("input[checked]").value
																+ " "
																+ form.querySelector("input.search").value;
						form.submit();
					});
				});
			};

			//	Event handler for popup despawn.
			let popFrameDespawnEventHandler = (eventInfo) => {
				let popFrame = (eventInfo.popup ?? eventInfo.popin);
				GW.notificationCenter.removeHandlerForEvent("DarkMode.didSetMode", popFrame.document.querySelector("iframe").darkModeDidSetModeHandler);
			};

			//  Add pop-frame spawn/despawn event handlers.
			if (Extracts.popFrameProvider == Popups) {
				GW.notificationCenter.addHandlerForEvent("Popups.popupDidSpawn", popFrameSpawnEventHandler, {
					condition: (info) => (info.popup.spawningTarget == widget.widgetLink)
				});
				GW.notificationCenter.addHandlerForEvent("Popups.popupWillDespawn", popFrameDespawnEventHandler, {
					condition: (info) => (info.popup.spawningTarget == widget.widgetLink)
				});
			} else {
				GW.notificationCenter.addHandlerForEvent("Popins.popinDidInject", popFrameSpawnEventHandler, {
					condition: (info) => (info.popin.spawningTarget == widget.widgetLink)
				});
				GW.notificationCenter.addHandlerForEvent("Popins.popinWillDespawn", popFrameDespawnEventHandler, {
					condition: (info) => (info.popin.spawningTarget == widget.widgetLink)
				});
			}
		}
	}));
});


/****************/
/* KEY COMMANDS */
/****************/
/*  Proper keypress support (such that keypress sequences work properly for
	modified keypresses).
 */

GW.keyCommands = {
	keysPressed: { },

	keyDown: (event) => {
		GWLog("GW.keyCommands.keyDown", "misc.js", 3);

		GW.keyCommands.keysPressed[event.keyCode] = {
			key: event.key,
			altKey: event.altKey,
			ctrlKey: event.altKey,
			metaKey: event.altKey,
			shiftKey: event.altKey,
		};
	},

	keyUp: (event) => {
	    GWLog("GW.keyCommands.keyUp", "misc.js", 3);

		let keyDownEventInfo = GW.keyCommands.keysPressed[event.keyCode];
		if (keyDownEventInfo == null)
			return;

		GW.notificationCenter.fireEvent("GW.keyWasPressed", {
			key: keyDownEventInfo.key,
			altKey: keyDownEventInfo.altKey,
			ctrlKey: keyDownEventInfo.altKey,
			metaKey: keyDownEventInfo.altKey,
			shiftKey: keyDownEventInfo.altKey,
			keyUpEvent: event
		});

		GW.keyCommands.keysPressed[event.keyCode] = null;
	}
};

doWhenPageLoaded(() => {
	document.addEventListener("keydown", GW.keyCommands.keyDown);
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


/**************************/
/* LOCATION HASH HANDLING */
/**************************/

function cleanLocationHash() {
    GWLog("cleanLocationHash", "misc.js", 2);

    if (   location.hash == "#top"
        || (   location.hash == ""
            && location.href.endsWith("#"))) {
        relocate(location.pathname);
    }
}

doWhenPageLayoutComplete(GW.pageLayoutCompleteHashHandlingSetup = (info) => {
    GWLog("GW.pageLayoutCompleteHashHandlingSetup", "misc.js", 1);

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
        GWLog("GW.handleBrowserHashChangeEvent", "misc.js", 1);

        //  Clean location hash.
        cleanLocationHash();

        //  If hash really changed, update saved hash and fire event.
        if (GW.locationHash != location.hash) {
            GW.notificationCenter.fireEvent("GW.hashDidChange", { oldHash: GW.locationHash });
            GW.locationHash = location.hash;
        }
    });

    GW.notificationCenter.fireEvent("GW.hashHandlingSetupDidComplete");
});
