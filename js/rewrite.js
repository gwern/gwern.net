/* Miscellaneous JS functions which run after the page loads to rewrite or adjust parts of the page. */
/* author: Said Achmiz */
/* license: MIT (derivative of footnotes.js, which is PD) */

/**********/
/* TABLES */
/**********/

/*  Wrap each table in a div.table-wrapper (for layout purposes).
    */
function wrapTables() {
	GWLog("wrapTables", "rewrite.js", 1);

	let wrapperClass = "table-wrapper";
	document.querySelectorAll("table").forEach(table => {
		if (table.parentElement.tagName == "DIV" && table.parentElement.children.length == 1)
			table.parentElement.classList.toggle(wrapperClass, true);
		else
			table.outerHTML = `<div class="${wrapperClass}">` + table.outerHTML + `</div>`;
	});
}
doWhenDOMContentLoaded(wrapTables);

/*	Wrap each full-width table in a div.full-width-table-wrapper, and also move
	the .collapse class (if any) from the outer wrapper to the table (for
	consistency).
	*/
function wrapFullWidthTables() {
	let fullWidthClass = "full-width";
	let fullWidthInnerWrapperClass = "full-width-table-inner-wrapper";
	document.querySelectorAll(`.table-wrapper.${fullWidthClass}`).forEach(fullWidthTableWrapper => {
		if (fullWidthTableWrapper.classList.contains("collapse")) {
			fullWidthTableWrapper.classList.remove("collapse");
			fullWidthTableWrapper.firstElementChild.classList.add("collapse");
		}

		fullWidthTableWrapper.innerHTML = `<div class="${fullWidthInnerWrapperClass}">` + fullWidthTableWrapper.innerHTML + `</div>`;
	});
}
doWhenDOMContentLoaded(wrapFullWidthTables);

/***********/
/* FIGURES */
/***********/

/*  Inject wrappers into figures.
    */
function wrapFigures() {
	GWLog("wrapFigures", "rewrite.js", 1);

	document.querySelectorAll("figure").forEach(figure => {
		let media = figure.querySelector("img, audio, video");
		let caption = figure.querySelector("figcaption");

		if (!(media && caption))
			return;

		//  Create an inner wrapper for the figure contents.
		let innerWrapper = document.createElement("span");
		innerWrapper.classList.add("figure-inner-wrapper");
		figure.appendChild(innerWrapper);

		//  Wrap the caption in the wrapper span.
		let wrapper = document.createElement("span");
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
doWhenDOMContentLoaded(wrapFigures);

/*	Designate full-width figures as such (with a ‘full-width’ class).
	*/
function markFullWidthFigures() {
	let fullWidthClass = "full-width";

	let allFullWidthMedia = document.querySelectorAll(`img.${fullWidthClass}, video.${fullWidthClass}`);
    allFullWidthMedia.forEach(fullWidthMedia => {
        fullWidthMedia.closest("figure").classList.toggle(fullWidthClass, true);
    });

	/*  Add ‘load’ listener for lazy-loaded media (as it might cause re-layout
		of e.g. sidenotes). Do this only after page loads, to avoid spurious
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
doWhenDOMContentLoaded(markFullWidthFigures);

/***************/
/* CODE BLOCKS */
/***************/

/*  Wrap each pre.full-width in a div.full-width and a 
	div.full-width-code-block-wrapper (for layout purposes).
    */
function wrapFullWidthPreBlocks() {
	GWLog("wrapFullWidthPreBlocks", "rewrite.js", 1);

	let fullWidthClass = "full-width";
	let fullWidthInnerWrapperClass = "full-width-code-block-wrapper";
	document.querySelectorAll(`pre.${fullWidthClass}`).forEach(fullWidthPre => {
		if (fullWidthPre.parentElement.tagName == "DIV" && fullWidthPre.parentElement.children.length == 1)
			fullWidthPre.parentElement.classList.toggle(fullWidthClass, true);
		else
			fullWidthPre.outerHTML = `<div class="${fullWidthClass}">` + fullWidthPre.outerHTML + `</div>`;

		fullWidthPre.parentElement.innerHTML = `<div class="${fullWidthInnerWrapperClass}">` + fullWidthPre.parentElement.innerHTML + `</div>`;
	});
}
doWhenDOMContentLoaded(wrapFullWidthPreBlocks);

/**************/
/* TYPOGRAPHY */
/**************/

/*	HYPHENS
	Add copy listener to strip soft hyphens from copy-pasted text (inserted by compile-time hyphenator).
	*/
function getSelectionHTML() {
    var container = document.createElement("div");
    container.appendChild(window.getSelection().getRangeAt(0).cloneContents());
    return container.innerHTML;
}
window.addEventListener("copy", GW.textCopied = (event) => {
	GWLog("GW.textCopied", "rewrite.js", 2);

    if (event.target.matches("input, textarea")) return;
    event.preventDefault();
    const selectedHTML = getSelectionHTML();
    const selectedText = getSelection().toString();
    event.clipboardData.setData("text/plain", selectedText.replace(/\u00AD|\u200b/g, ""));
    event.clipboardData.setData("text/html",  selectedHTML.replace(/\u00AD|\u200b/g, ""));
});

/*********************/
/* FULL-WIDTH BLOCKS */
/*********************/

/*  Expands all tables (& other blocks) whose wrapper block is marked with class
    ‘full-width’, and all figures marked with class ‘full-width’, to span the 
    viewport (minus a specified margin on both sides).
    */
function expandFullWidthBlocks() {
	GWLog("expandFullWidthBlocks", "rewrite.js", 1);

	//  TODO: active media queries to switch between mobile and non-mobile

	/*	On narrow (“mobile”) viewports, do no layout (figures on mobile are
		already as “full-width” as they’re going to get).
		*/
	if (GW.mediaQueries.mobileWidth.matches) return;

	/*	Configuration and dynamic value storage.
		*/
	GW.fullWidthBlockLayout = {
		sideMargin: 25,
		pageWidth: 0,
		leftAdjustment: 0
	};

	/*	Pre-query key elements, to save performance on resize.
		*/
	let rootElement = document.querySelector("html");
	let markdownBody = document.querySelector("#markdownBody");

	/*	Inject styles block to hold dynamically updated layout variables.
		*/
	document.querySelector("head").insertAdjacentHTML("beforeend", `<style id="full-width-block-layout-styles"></style>`);
	let fullWidthBlockLayoutStyles = document.querySelector("#full-width-block-layout-styles");

	/*	Function to update layout variables (called immediately and on resize).
		*/
	let updateFullWidthBlockLayoutStyles = () => {
		GWLog("updateFullWidthBlockLayoutStyles", "rewrite.js", 2);

		if (GW.mediaQueries.mobileWidth.matches) {
			document.querySelectorAll("div.full-width, figure.full-width").forEach(fullWidthBlock => {
				fullWidthBlock.style.marginLeft = "";
				fullWidthBlock.style.marginRight = "";
			});
			return;
		}

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

	/*	Set margins of full-width blocks.
		*/
    document.querySelectorAll("div.full-width, figure.full-width").forEach(fullWidthBlock => {
		fullWidthBlock.style.marginLeft = `calc((-1 * (var(--GW-full-width-block-layout-left-adjustment) / 2.0)) + var(--GW-full-width-block-layout-side-margin) - (var(--GW-full-width-block-layout-page-width) - 100%)/2)`;
		fullWidthBlock.style.marginRight = `calc((var(--GW-full-width-block-layout-left-adjustment) / 2.0) + var(--GW-full-width-block-layout-side-margin) - (var(--GW-full-width-block-layout-page-width) - 100%)/2)`;
    });

	/*	Add listener to update layout variables on window resize.
		*/
	window.addEventListener("resize", updateFullWidthBlockLayoutStyles);

	GW.notificationCenter.fireEvent("Rewrite.didExpandFullWidthBlocks");
}
GW.notificationCenter.addHandlerForEvent("Rewrite.didExpandFullWidthBlocks", () => {
	GW.notificationCenter.fireEvent("Rewrite.pageLayoutWillComplete");
	requestAnimationFrame(() => {
		GW.pageLayoutComplete = true;
		GW.notificationCenter.fireEvent("Rewrite.pageLayoutDidComplete");
	});
}, { once: true });
doWhenPageLoaded(expandFullWidthBlocks);

/*********/
/* MISC. */
/*********/

/*	Clean up image alt-text. (Shouldn’t matter, because all image URLs work,
	right? Yeah, right...)
	*/
function cleanUpImageAltText() {
	GWLog("cleanUpImageAltText", "rewrite.js", 1);

	document.querySelectorAll("img[alt]").forEach(image => {
		image.alt = decodeURIComponent(image.alt);
	});
}
doWhenDOMContentLoaded(cleanUpImageAltText);

/*	Directional navigation links on self-links: for each self-link like “see [later](#later-identifier)”, find the linked identifier, whether it’s before or after, and if it is before/previously, annotate the self-link with ‘↑’ and if after/later, ‘↓’. This helps the reader know if it’s a backwards link to a identifier already read, or an unread identifier.
	*/
function directionalizeAnchorLinks() {
	GWLog("directionalizeAnchorLinks", "rewrite.js", 1);

	document.body.querySelectorAll("#markdownBody a[href^='#']").forEach(identifierLink => {
		if (   identifierLink.closest("h1, h2, h3, h4, h5, h6")
			|| identifierLink.closest(".footnote-ref, .footnote-back, .sidenote-self-link"))
			return;
		target = document.body.querySelector(identifierLink.getAttribute("href"));
		if (!target) return;
		identifierLink.classList.add((identifierLink.compareDocumentPosition(target) == Node.DOCUMENT_POSITION_FOLLOWING) ? 'identifier-link-down' : 'identifier-link-up');
	});
}
doWhenDOMContentLoaded(directionalizeAnchorLinks);

/*	The footnotes section has no ID because Pandoc is weird. Give it one.
	*/
function identifyFootnotesSection() {
	GWLog("identifyFootnotesSection", "rewrite.js", 1);

	let footnotesSection = document.querySelector("section.footnotes");
	if (footnotesSection)
		footnotesSection.id = "footnotes";
}
doWhenDOMContentLoaded(identifyFootnotesSection);

/*	Enable hovering over a link bibliography entry number to link to it, much
	like the self-links on section headings.
	*/
function injectLinkBibliographyItemSelfLinks() {
	GWLog("injectLinkBibliographyItemSelfLinks", "rewrite.js", 1);

	let linkBibliographyListItems = Array.from(document.querySelector("#link-bibliography > ol").children);

	for (var i = 0; i < linkBibliographyListItems.length; i++) {
		let id = `link-bibliography-entry-${i + 1}`;
		linkBibliographyListItems[i].id = id;
		linkBibliographyListItems[i].insertAdjacentHTML("afterbegin", `<a href="#${id}" class="link-bibliography-item-self-link">&nbsp;</a>`);
	}
}
doWhenDOMContentLoaded(injectLinkBibliographyItemSelfLinks);

/*	Requires typography.js to be loaded prior to this file.
	*/
function rectifyTypographyInLinkBibliographyEntries() {
	GWLog("rectifyTypographyInLinkBibliographyEntries", "rewrite.js", 1);

	document.querySelectorAll("#link-bibliography > ol > li > blockquote").forEach(linkBibliographyEntryContent => {
		Typography.processElement(linkBibliographyEntryContent, Typography.replacementTypes.QUOTES|Typography.replacementTypes.WORDBREAKS);

		//	Educate quotes in image alt-text.
		linkBibliographyEntryContent.querySelectorAll("img").forEach(image => {
			image.alt = Typography.processString(image.alt, Typography.replacementTypes.QUOTES);
		});
	});
}
doWhenDOMContentLoaded(rectifyTypographyInLinkBibliographyEntries);

/*	Hash realignment.
	*/
function realignHash() {
	GWLog("realignHash", "rewrite.js", 1);

	//  Chrome’s fancy new “scroll to text fragment”. Deal with it in Firefox.
	if (GW.isFirefox()) {
		if (location.hash.startsWith("#:~:")) {
			GW.hashRealignValue = GW.hashRealignValue || "#";
		} else if (location.hash.includes(":~:")) {
			GW.hashRealignValue = GW.hashRealignValue || location.hash.replace(/:~:.*$/, "");
		}
	}

	let hash = GW.hashRealignValue || location.hash;
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
/*  This is necessary to defeat a bug where if the page is loaded with the URL
	hash targeting some element, the element does not match the :target CSS
	pseudo-class.
	*/
doWhenPageLoaded(() => {
	requestAnimationFrame(realignHash);
});

/*! instant.page v5.1.0 - (C) 2019-2020 Alexandre Dieulot - https://instant.page/license */
/* Settings: 'prefetch' (loads HTML of target) after 800ms hover (desktop) or mouse-down-click (mobile); TODO: left in logging for testing during experiment */
let t,e;const n=new Set,o=document.createElement("link"),z=o.relList&&o.relList.supports&&o.relList.supports("prefetch")&&window.IntersectionObserver&&"isIntersecting"in IntersectionObserverEntry.prototype,s="instantAllowQueryString"in document.body.dataset,a=true,r="instantWhitelist"in document.body.dataset,c="instantMousedownShortcut"in document.body.dataset,d=1111;let l=800,u=!1,f=!1,m=!1;if("instantIntensity"in document.body.dataset){const t=document.body.dataset.instantIntensity;if("mousedown"==t.substr(0,"mousedown".length))u=!0,"mousedown-only"==t&&(f=!0);else if("viewport"==t.substr(0,"viewport".length))navigator.connection&&(navigator.connection.saveData||navigator.connection.effectiveType&&navigator.connection.effectiveType.includes("2g"))||("viewport"==t?document.documentElement.clientWidth*document.documentElement.clientHeight<45e4&&(m=!0):"viewport-all"==t&&(m=!0));else{const e=parseInt(t);isNaN(e)||(l=e)}}if(z){const n={capture:!0,passive:!0};if(f||document.addEventListener("touchstart",function(t){e=performance.now();const n=t.target.closest("a");if(!h(n))return;v(n.href)},n),u?c||document.addEventListener("mousedown",function(t){const e=t.target.closest("a");if(!h(e))return;v(e.href)},n):document.addEventListener("mouseover",function(n){if(performance.now()-e<d)return;const o=n.target.closest("a");if(!h(o))return;o.addEventListener("mouseout",p,{passive:!0}),t=setTimeout(()=>{v(o.href),t=void 0},l)},n),c&&document.addEventListener("mousedown",function(t){if(performance.now()-e<d)return;const n=t.target.closest("a");if(t.which>1||t.metaKey||t.ctrlKey)return;if(!n)return;n.addEventListener("click",function(t){1337!=t.detail&&t.preventDefault()},{capture:!0,passive:!1,once:!0});const o=new MouseEvent("click",{view:window,bubbles:!0,cancelable:!1,detail:1337});n.dispatchEvent(o)},n),m){let t;(t=window.requestIdleCallback?t=>{requestIdleCallback(t,{timeout:1500})}:t=>{t()})(()=>{const t=new IntersectionObserver(e=>{e.forEach(e=>{if(e.isIntersecting){const n=e.target;t.unobserve(n),v(n.href)}})});document.querySelectorAll("a").forEach(e=>{h(e)&&t.observe(e)})})}}function p(e){e.relatedTarget&&e.target.closest("a")==e.relatedTarget.closest("a")||t&&(clearTimeout(t),t=void 0)}function h(t){if(t&&t.href&&(!r||"instant"in t.dataset)&&(a||t.origin==location.origin||"instant"in t.dataset)&&["http:","https:"].includes(t.protocol)&&("http:"!=t.protocol||"https:"!=location.protocol)&&(s||!t.search||"instant"in t.dataset)&&!(t.hash&&t.pathname+t.search==location.pathname+location.search||"noInstant"in t.dataset))return!0}function v(t){if(n.has(t))return;const e=document.createElement("link");console.log("Prefetched: "+t);e.rel="prefetch",e.href=t,document.head.appendChild(e),n.add(t)};

/**************/
/* DEPRECATED */
/**************/

// For X11 Linux, middle-click somehow manages to bypass the copy-paste listener
// function getTextNodes(node) {
//  var allTextNodes = [ ];
//  let walk = document.createTreeWalker(node, NodeFilter.SHOW_TEXT, null, false);
//  while (node = walk.nextNode())
//      allTextNodes.push(node);
//  return allTextNodes;
// }
// if (navigator.userAgent.match(/X11|Ubuntu/)) {
//  getTextNodes(document.querySelector("#markdownBody")).forEach(textNode => {
//      textNode.textContent = textNode.textContent.replace(/\u00ad/g,"");
//  });
// }
