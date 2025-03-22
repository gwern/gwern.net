/**********************/
/* REWRITE PROCESSORS */
/**********************/
/*	“Non-block layout” a.k.a. “rewrite” processors. Like rewrites, but faster.
 */

/******************************************************************************/
/*	Add a “non-block” layout processor, a.k.a. a “rewrite processor”.

	(See `addLayoutProcessor`, in layout.js, for explanation of option fields.)
 */
function addRewriteProcessor(processorName, processor, options = null) {
	options = Object.assign({
		blockLayout: false
	}, options);

	addLayoutProcessor(processorName, processor, options);
}

/*****************************************************************************/
/*	Run rewrite processor in already-loaded main page content, and add rewrite
	processor to process any subsequently loaded content.
 */
function processMainContentAndAddRewriteProcessor(processorName, processor) {
	processor(document.main);
	addRewriteProcessor(processorName, processor);
}


/****************/
/* INLINE ICONS */
/****************/

/**********************************************/
/*	Enable inline icons in the given container.
 */
addRewriteProcessor("processInlineIconsInContainer", (blockContainer) => {
    GWLog("processInlineIconsInContainer", "rewrite-initial.js", 2);

	blockContainer.querySelectorAll("span[class*='icon-']").forEach(inlineIcon => {
		if (inlineIcon.classList.contains("icon-not"))
			return;

		//	Some icons are special.
		let specialIconsSelector = [
			".icon-single-white-star-on-black-circle"
		].join(", ");
		if (inlineIcon.matches(specialIconsSelector))
			inlineIcon.classList.add("icon-special");

		/*	“Special” icons will have their `--icon-url` CSS variable set
			elsewhere (in other runtime code, or in CSS).
		 */
		if (inlineIcon.classList.contains("icon-special") == false) {
			let iconName = Array.from(inlineIcon.classList).find(className => className.startsWith("icon-"))?.slice("icon-".length);
			if (iconName == null)
				return;

			inlineIcon.style.setProperty("--icon-url", `url('/static/img/icon/icons.svg#${iconName}')`);
		}

		inlineIcon.classList.add("inline-icon", "dark-mode-invert");
	});
});


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
function renderProgressPercentageIcon(progressIndicatorElement) {
	let progressPercentage = parseInt(progressIndicatorElement.dataset.progressPercentage);
	if (isNaN(progressPercentage))
		return;
	let svgSrc = arcSVGForProgressPercent(progressPercentage);
	progressIndicatorElement.querySelector(".progress-indicator-icon")?.remove();
	progressIndicatorElement.appendChild(newElement("SPAN", {
		class: "progress-indicator-icon icon-special",
		style: `--icon-url: url("data:image/svg+xml;utf8,${encodeURIComponent(svgSrc)}")`
	}));
	progressIndicatorElement.classList.add("progress-percentage-rendered");
}

/***********************************************************************/
/*	Render progress percentage icon for the given element, if it has not 
	already been rendered.
 */
function renderProgressPercentageIconIfNeeded(progressIndicatorElement) {
	if (isProgressPercentageIconRendered(progressIndicatorElement) == true)
		return;

	renderProgressPercentageIcon(progressIndicatorElement);
}

/***************************************************************************/
/*	Returns true if progress percentage icon has been rendered for the given 
	element.
 */
function isProgressPercentageIconRendered(progressIndicatorElement) {
	return (progressIndicatorElement.classList.contains("progress-percentage-rendered"));
}

/*************************************************************************/
/*	Mark the given element as not having a valid progress percentage icon.
 */
function invalidateProgressPercentageIconForElement(progressIndicatorElement) {
	progressIndicatorElement.classList.remove("progress-percentage-rendered");
}

/**********************************************************/
/*	Inject progress indicator icons into any element with a
	data-progress-percentage attribute.
 */
addRewriteProcessor("injectProgressIcons", (blockContainer) => {
    GWLog("injectProgressIcons", "rewrite-initial.js", 1);

	blockContainer.querySelectorAll("[data-progress-percentage]").forEach(renderProgressPercentageIconIfNeeded);
}, "rewrite");


/***************************/
/* RECENTLY-MODIFIED ICONS */
/***************************/

/**********************************************************************/
/*	Adds recently-modified icon (white star on black circle) to a link.
 */
function addRecentlyModifiedIconToLink(link) {
	if (link.classList.contains("has-recently-modified-icon") == true)
		return;

	//  Inject indicator hook span.
	link.insertBefore(newElement("SPAN", { class: "recently-modified-icon-hook" }), link.firstChild);

	if (link.classList.contains("has-indicator-hook")) {
		/*	If the link has an indicator hook, we must inject a text node
			containing a U+2060 WORD JOINER between the two hooks. This ensures
			that the two link styling elements are arranged properly, and do not
			span a line break.
		 */
		 link.insertBefore(document.createTextNode("\u{2060}"), link.querySelector(".indicator-hook"));
	} else {
		/*  Inject U+2060 WORD JOINER at start of first text node of the
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
	}

	link.classList.add("has-recently-modified-icon");
}

/***************************************************************************/
/*	Removes recently-modified icon (white star on black circle) from a link.
 */
function removeRecentlyModifiedIconFromLink(link) {
	if (link.classList.contains("has-recently-modified-icon") == false)
		return;

	let iconHook = link.querySelector(".recently-modified-icon-hook");
	if (iconHook.nextSibling.firstTextNode.textContent.startsWith("\u{2060}"))
		iconHook.nextSibling.firstTextNode.textContent = iconHook.nextSibling.firstTextNode.textContent.slice(1);
	iconHook.remove();

	link.classList.remove("has-recently-modified-icon");

	/*	If this link has an indicator hook, then we must remove the text node
		containing U+2060 WORD JOINER between the two hooks.
	 */
	if (   link.classList.contains("has-indicator-hook")
		&& link.firstTextNode.textContent == "\u{2060}")
		link.firstTextNode.remove();
}

/****************************************************************************/
/*  Enable special icons for recently modified links (that are not in lists).
 */
addRewriteProcessor("enableRecentlyModifiedLinkIcons", (blockContainer) => {
    GWLog("enableRecentlyModifiedLinkIcons", "rewrite-initial.js", 2);

    blockContainer.querySelectorAll("a.link-modified-recently:not(.in-list)").forEach(addRecentlyModifiedIconToLink);
});

/**************************************************************************/
/*  Enable special list icons for list items that contain recently modified
    links.
 */
addRewriteProcessor("enableRecentlyModifiedLinkListIcons", (blockContainer) => {
    GWLog("enableRecentlyModifiedLinkListIcons", "rewrite-initial.js", 2);

    blockContainer.querySelectorAll("li a.link-modified-recently").forEach(link => {
        let inList = false;
        let containingGraf = link.closest("p");
        if (containingGraf?.matches("li > p:only-of-type")) {
            inList = true;
        } else if (containingGraf?.matches(".data-field")) {
            /*  This handles cases such as those where we’re transcluding an
                annotation into a list, and each annotation has its own list
                item (thus the .link-modified-recently class would be on the
                title-link of the annotation).
             */
            let ancestor = containingGraf.parentElement;
            while (   ancestor.matches("li") == false
                   && (   ancestor.parentElement.children.length == 1
                       || (   ancestor.parentElement.children.length == 2
                           && ancestor.matches(".include-wrapper"))))
                   ancestor = ancestor.parentElement;
            if (ancestor.matches("li"))
                inList = true;
        }
        if (inList) {
            link.closest("li").classList.add("link-modified-recently-list-item");
            link.classList.add("in-list");

			//	Remove existing icon, if any.
			if (link.classList.contains("has-recently-modified-icon"))
	            removeRecentlyModifiedIconFromLink(link);
        }
    });
});


/*********/
/* LISTS */
/*********/

/*************************************************************/
/*	Add certain style classes to certain lists and list items.
 */
addRewriteProcessor("designateListStyles", (blockContainer) => {
    GWLog("designateListStyles", "rewrite-initial.js", 2);

	blockContainer.querySelectorAll("ul > li").forEach(listItem => {
		if (listItem.closest(".TOC") == null)
			listItem.classList.add("dark-mode-invert");
	});
});


/********************/
/* HORIZONTAL RULES */
/********************/

/*************************************************/
/*	Add certain style classes to horizontal rules.
 */
addRewriteProcessor("designateHorizontalRuleStyles", (blockContainer) => {
    GWLog("designateHorizontalRuleStyles", "rewrite-initial.js", 2);

	let hrTypeClassPrefix = "horizontal-rule-";

	blockContainer.querySelectorAll("hr").forEach(hr => {
		hr.classList.add("dark-mode-invert");

		//	If there is no type class, just add the default one and return.
		let classBearerBlock = hr.closest("[class*='horizontal-rule-']");
		if (classBearerBlock == null) {
			hr.classList.add(hrTypeClassPrefix + "nth-1");
			return;
		}

		//	If the type class is on a containing div, unwrap, moving attributes.
		if (classBearerBlock != hr)
			unwrap(classBearerBlock, { moveID: true, moveClasses: true });

		//	If the type class is a sequence designator, do nothing.
		let specialHRTypeClasses = [
			"horizontal-rule-small"
		];
		let hrTypeClass = Array.from(hr.classList).find(cssClass => 
			(   cssClass.startsWith(hrTypeClassPrefix)  == true
			 && specialHRTypeClasses.includes(cssClass) == false)
		)?.slice(hrTypeClassPrefix.length);
		if (hrTypeClass.startsWith("nth-"))
			return;

		//	Type class specifies a custom image; set the CSS property.
		hr.style.setProperty("--icon-image", `var(--GW-image-${hrTypeClass})`);
	});
});


/*************************/
/* INLINE MODE SELECTORS */
/*************************/

/********************************************/
/*	Wrap parenthesized inline mode selectors.
 */
addRewriteProcessor("wrapParenthesizedInlineModeSelectors", (blockContainer) => {
    GWLog("wrapParenthesizedInlineModeSelectors", "rewrite-initial.js", 2);

	let inlineModeSelectorsSelector = [
		"dark",
		"reader",
		"extracts",
		"search",
		"help",
		"toolbar"
	].map(x => `.${x}-mode-selector-inline`).join(", ");

	blockContainer.querySelectorAll(inlineModeSelectorsSelector).forEach(modeSelector => {
		if (modeSelector.closest(".inline-mode-selector") != null)
			return;

		wrapParenthesizedNodes("inline-mode-selector", modeSelector);
	});
});


/************************/
/* PAGE THUMBNAIL IMAGE */
/************************/

/******************************************************************************/
/*	Given a full document (HTML page), return an object with the attributes for
	an <img> element of the page thumbnail image.
 */
function pageThumbnailAttributesFromDocument(doc) {
	//  Get the page thumbnail URL and metadata.
	let pageThumbnailAttributes;
	let pageThumbnailMetaTag = doc.querySelector("meta[property='og:image']");
	if (pageThumbnailMetaTag) {
		let pageThumbnailURL = URLFromString(pageThumbnailMetaTag.getAttribute("content"));

		//  Alt text, if provided.
		let pageThumbnailAltMetaTag = doc.querySelector("meta[property='og:image:alt']");
		let pageThumbnailAltText = (pageThumbnailAltMetaTag
									? pageThumbnailAltMetaTag.getAttribute("content")
									: `Thumbnail image for “${(doc.querySelector("meta[property='og:title']").getAttribute("content"))}”`
									).replace(/"/g, "&quot;");

		//  Image dimensions.
		let pageThumbnailWidth = doc.querySelector("meta[property='og:image:width']").getAttribute("content");
		let pageThumbnailHeight = doc.querySelector("meta[property='og:image:height']").getAttribute("content");

		return {
			src: pageThumbnailURL.href,
			title: pageThumbnailAltText,
			width: pageThumbnailWidth,
			height: pageThumbnailHeight,
			style: "width: ${pageThumbnailWidth}px; height: auto;"
		};
	} else {
		return null;
	}
}

/*****************************************************************************/
/*	Inject the page thumbnail image into the page abstract (or the abstract of
	a full-page pop-frame). (Returns the thumbnail image element.)

	Available option fields:

	atEnd (boolean)
		If true (the default), thumbnail figure placed at end, after all other
		content in the abstract. If false, placed at start, before all else.

	floatClass (string)
		Float-related class to set on the figure. Default is ‘float-not’. Other
		options are ‘float-left’ or ‘float-right’.
 */
function injectThumbnailIntoPageAbstract(pageAbstract, pageThumbnailAttributes, options) {
	options = Object.assign({
		atEnd: true,
		floatClass: "float-not"
	}, options);

	//	Check if the page thumbnail has already been injected.
	if (pageAbstract.querySelector(".page-thumbnail-figure") != null)
		return null;

	//	Except logo.
	if (URLFromString(pageThumbnailAttributes.src).pathname.startsWith("/static/img/logo/"))
		return null;

	//	Construct.
	let pageThumbnail = newElement("IMG", pageThumbnailAttributes);
	let pageThumbnailWrapper = newElement("SPAN", {
		class: "image-wrapper img"
	});
	let pageThumbnailFigure = newElement("FIGURE", {
		class: "page-thumbnail-figure " + options.floatClass
	});
	pageThumbnailFigure.appendChild(pageThumbnailWrapper).appendChild(pageThumbnail);

	//	Inject.
	pageAbstract.insertBefore(pageThumbnailFigure, (options.atEnd ? null : pageAbstract.firstElementChild));

	return pageThumbnail;
}

/************************************************************************/
/*	Inject the page thumbnail into the page abstract, when such is found.
 */
if (location.pathname.endsWithAnyOf([ "/", "/index" ]) == false) {
	let pageThumbnailAttributes;
	doWhenBodyExists(() => {
		pageThumbnailAttributes = pageThumbnailAttributesFromDocument(document);
		doAjax({ location: URLFromString(pageThumbnailAttributes.src) });
	});
	doWhenElementExists((firstAfterAbstract) => {
		//	Get page abstract.
		let pageAbstract = firstAfterAbstract.previousElementSibling.firstElementChild;

		//	Designate page abstract.
		pageAbstract.classList.add("page-abstract");

		//	Inject page thumbnail into page abstract.
		injectThumbnailIntoPageAbstract(pageAbstract, pageThumbnailAttributes, { atEnd: true });
	}, "#markdownBody > .abstract:first-child + *");
}


/***************************/
/* ADDITIONAL EARLY LAYOUT */
/***************************/

/*************************************************/
/*	Placeholder page for ID-based content loading.
 */
doWhenMainExists(() => {
	if (location.pathname.startsWith("/ref/")) {
		document.querySelectorAll("title, header h1").forEach(element => {
			element.replaceChildren();
		});
	}
});

/**********************************/
/*	Designate /blog/ pages as such.
 */
doWhenBodyExists(() => {
	if (location.pathname.startsWith("/blog/"))
		document.body.classList.add("blog-page");
});

/**************************************************************************/
/*  Update visibility of a TOC. (Hide if no entries; if main page TOC, also 
	hide if one entry.)
 */
function updateTOCVisibility(TOC) {
	if (TOC == null)
		return;

    let numEntries = TOC.querySelectorAll("li").length;
    if (   (   TOC.id == "TOC"
            && numEntries <= 1)
        || numEntries == 0) {
        TOC.classList.toggle("hidden", true);
    } else {
        TOC.classList.toggle("hidden", false);
    }
}

doWhenElementExists(updateTOCVisibility, "#TOC");

