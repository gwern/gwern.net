/**********************/
/* REWRITE PROCESSORS */
/**********************/
/*	“Non-block layout” a.k.a. “rewrite” processors. Like rewrites, but faster.
 */

/*****************************************************************************/
/*	Run rewrite processor in already-loaded main page content, and add rewrite
	processor to process any subsequently loaded content.
 */
function processMainContentAndAddRewriteProcessor(processorName, processor) {
	processor(document.main);
	addLayoutProcessor(processorName, processor, { blockLayout: false });
}

/**********************************************/
/*	Enable inline icons in the given container.
 */
addLayoutProcessor("processInlineIconsInContainer", (blockContainer) => {
    GWLog("processInlineIconsInContainer", "layout.js", 2);

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
}, { blockLayout: false });

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
addLayoutProcessor("enableRecentlyModifiedLinkIcons", (blockContainer) => {
    GWLog("enableRecentlyModifiedLinkIcons", "layout.js", 2);

    blockContainer.querySelectorAll("a.link-modified-recently:not(.in-list)").forEach(addRecentlyModifiedIconToLink);
}, { blockLayout: false });

/**************************************************************************/
/*  Enable special list icons for list items that contain recently modified
    links.
 */
addLayoutProcessor("enableRecentlyModifiedLinkListIcons", (blockContainer) => {
    GWLog("enableRecentlyModifiedLinkListIcons", "layout.js", 2);

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
}, { blockLayout: false });

/*************************************************************/
/*	Add certain style classes to certain lists and list items.
 */
addLayoutProcessor("designateListStyles", (blockContainer) => {
    GWLog("designateListStyles", "layout.js", 2);

	blockContainer.querySelectorAll("ul > li").forEach(listItem => {
		if (listItem.closest(".TOC") == null)
			listItem.classList.add("dark-mode-invert");
	});
}, { blockLayout: false });

/*************************************************/
/*	Add certain style classes to horizontal rules.
 */
addLayoutProcessor("designateHorizontalRuleStyles", (blockContainer) => {
    GWLog("designateHorizontalRuleStyles", "layout.js", 2);

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
}, { blockLayout: false });

/********************************************/
/*	Wrap parenthesized inline mode selectors.
 */
addLayoutProcessor("wrapParenthesizedInlineModeSelectors", (blockContainer) => {
    GWLog("wrapParenthesizedInlineModeSelectors", "layout.js", 2);

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
}, { blockLayout: false });


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
