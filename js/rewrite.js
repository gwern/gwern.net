/* Miscellaneous JS functions which run after the page loads to rewrite or adjust parts of the page. */

/***********/
/* HELPERS */
/***********/

/*	Run the given function immediately if the page is already loaded, or add
	a listener to run it as soon as the page loads.
	*/
function doWhenPageLoaded(f) {
	if (document.readyState == "complete")
		f();
	else
		window.addEventListener("load", f);
}

/*=--------=*/
/*= Tables =*/
/*=--------=*/

/*	Expands all tables whose wrapper block is marked with class "full-width"
	to span the viewport (minus a specified margin on both sides).
	*/
function expandFullWidthTables() {
	let fullWidthTableMargin = "2.5ch";
	let pageWidth = document.querySelector("html").clientWidth;

	/*	Find all full-width tables; set their position and size.
		*/
	document.querySelectorAll(".tableWrapper.full-width").forEach(fullWidthTable => {
		fullWidthTable.removeAttribute("style");
		fullWidthTable.style.left = `calc(${(fullWidthTable.getBoundingClientRect().left * -1) + "px"} + ${fullWidthTableMargin})`;
		fullWidthTable.style.width = `calc(${pageWidth + "px"} - (2 * ${fullWidthTableMargin}))`;
	});

	/*	If sidenotes exist, update sidenote positions.
		*/
	requestAnimationFrame(() => {
		if (typeof window.GW == "undefined" ||
			typeof GW.sidenotes == "undefined" ||
			GW.sidenotes.mediaQueries.viewportWidthBreakpoint.matches == true ||
			GW.sidenotes.sidenoteDivs.length == 0)
			return;

			updateSidenotePositions();
	});
}

/*=-----------------=*/
/*= Figure captions =*/
/*=-----------------=*/

/*	Returns an array of objects each with two properties: 'media' (which
	is the image or video element) and 'caption' (which is the figcaption
	element). These are all the captioned media elements on the page.
	*/
function getAllCaptionedMedia() {
	return Array.prototype.map.call(document.querySelectorAll("figure"), figure => {
		let media = figure.querySelector("img") || figure.querySelector("video");
		let caption = figure.querySelector("figcaption");
		return { media: media, caption: caption };
	}).filter(captionedMedia => captionedMedia.media && captionedMedia.caption);
}

/*	Sets minimum width of all captions to the width of their associated
	media (image or video) element.
	*/
function setCaptionsMinimumWidth() {
	getAllCaptionedMedia().forEach(captionedMedia => {
		//	Get the caption wrapper span.
		let wrapper = captionedMedia.caption.closest(".caption-wrapper");
		//	Set wrapper minimum width to width of media element.
		wrapper.style.minWidth = captionedMedia.media.clientWidth + "px";
	});
}

/*=-------------=*/
/*= Code blocks =*/
/*=-------------=*/

/*	Rounds the height of all code blocks to the nearest integer (i.e., the
	nearest pixel), to fix a weird bug that cuts off the bottom border.
	*/
function rectifyCodeBlockHeight(codeBlock) {
	codeBlock.style.height = parseInt(getComputedStyle(codeBlock).height) + "px";
}

/*********/
/* SETUP */
/*********/

/*	Wrap each table in a div.tableWrapper (for layout purposes).
	*/
document.querySelectorAll("table").forEach(table => {
	if (table.parentElement.tagName == "DIV" && table.parentElement.children.length == 1)
		table.parentElement.classList.toggle("tableWrapper", true);
	else
		table.outerHTML = "<div class='tableWrapper'>" + table.outerHTML + "</div>";
});

/*	Expand full-width tables, and add a listener to recompute their size and
	position upon window resize.
	*/
doWhenPageLoaded(expandFullWidthTables);
window.addEventListener("resize", expandFullWidthTables);

/*	Unwrap pre.sourceCode blocks from their extraneous containing divs.
	*/
document.querySelectorAll("div.sourceCode").forEach(scd => {
	scd.outerHTML = scd.innerHTML;
});

/*	Rectify heights of all code blocks.
	*/
doWhenPageLoaded(() => {
	document.querySelectorAll("pre code").forEach(codeBlock => {
		rectifyCodeBlockHeight(codeBlock);
	});
});

/*	Inject disclosure buttons and otherwise prepare the collapse blocks.
	*/
document.querySelectorAll(".collapse").forEach(collapseBlock => {
	let disclosureButtonHTML = "<input type='checkbox' class='disclosure-button'>";
	if (collapseBlock.tagName == "SECTION") {
		//  Inject the disclosure button.
		collapseBlock.children[0].insertAdjacentHTML("afterend", disclosureButtonHTML);
	} else {
		//  Construct collapse block wrapper and inject the disclosure button.
		let realCollapseBlock = document.createElement("div");
		realCollapseBlock.classList.add("collapse");
		realCollapseBlock.insertAdjacentHTML("afterbegin", disclosureButtonHTML);
		//  Move block-to-be-collapsed into wrapper.
		collapseBlock.parentElement.insertBefore(realCollapseBlock, collapseBlock);
		collapseBlock.classList.remove("collapse");
		realCollapseBlock.appendChild(collapseBlock);
	}
});
/*	Add listeners to toggle ‘expanded’ class of collapse blocks.
	*/
document.querySelectorAll(".disclosure-button").forEach(disclosureButton => {
	let collapseBlock = disclosureButton.closest(".collapse");
	disclosureButton.addEventListener("change", (event) => {
		collapseBlock.classList.toggle("expanded", disclosureButton.checked);

		//	If it’s a code block, adjust its height.
		if (collapseBlock.lastElementChild.tagName == "PRE") {
			let codeBlock = collapseBlock.lastElementChild.lastElementChild;
			if (codeBlock.tagName != "CODE") return;

			codeBlock.style.height = "";
			requestAnimationFrame(() => {
				rectifyCodeBlockHeight(codeBlock);
			});
		}
	});
});

/*	Inject spans to contain curly quotes within <q> elements.
	*/
document.querySelectorAll("q").forEach(q => {
	let openQuote = `<span class='quote-mark open'>${q.innerHTML.substring(0, 1)}</span>`;
	let closeQuote = `<span class='quote-mark close'>${q.innerHTML.substring(q.innerHTML.length - 1)}</span>`;
	q.innerHTML = q.innerHTML.substring(1, q.innerHTML.length - 1);
	if (q.parentElement.tagName == "A" && q.parentElement.childNodes.length == 1) {
		q.parentElement.outerHTML = `${openQuote}${q.parentElement.outerHTML}${closeQuote}`;
	} else {
		q.outerHTML = `${openQuote}${q.outerHTML}${closeQuote}`;
	}
});

/*	Wrap all captions in figures in a span.
	*/
getAllCaptionedMedia().forEach(captionedMedia => {
	//	Wrap the caption in the wrapper span.
	let wrapper = document.createElement("span");
	wrapper.classList.add("caption-wrapper");
	wrapper.appendChild(captionedMedia.caption);

	//	Re-insert the wrapped caption into the figure.
	let figure = captionedMedia.media.closest("figure");
	figure.appendChild(wrapper);
});

/*  Set minimum caption box width, and add listener to recalculate on
	window resize.
	*/
doWhenPageLoaded(setCaptionsMinimumWidth);
window.addEventListener('resize', setCaptionsMinimumWidth);

/*	Insert zero-width spaces after problematic characters in links.
	(This is to mitigate justification/wrapping problems.)
	*/
let problematicCharacters = '/';
document.querySelectorAll("p a, p a *").forEach(element => {
	element.childNodes.forEach(node => {
	   if (node.childNodes.length > 0) return;
	   node.textContent = node.textContent.replace(new RegExp("(\\w[" + problematicCharacters + "])(\\w)", 'g'), "$1\u{200B}$2");
	});
});

/*	Rectify straight quotes / apostrophes / etc. in the header and the page metadata.
	TODO: this should be doable in Hakyll at compile-time
	*/
let textCleanRegexps = [
	// beginning "
	[/([^A-Za-z0-9_\)]|^)"(\S)/, '$1\u201c$2'],
	// ending "
	[/(\u201c[^"]*)"([^"]*$|[^\u201c"]*\u201c)/, '$1\u201d$2'],
	// remaining " at end of word
	[/([^0-9])"/, '$1\u201d'],
	// double quotes
	[/"(.+?)"/, '\u201c$1\u201d'],
	// beginning '
	[/(\W|^)'(\S)/, '$1\u2018$2'],
	// conjunction's possession
	[/([a-z])'([a-z])/, '$1\u2019$2'],
	// abbrev. years like '93
	[/(\u2018)([0-9]{2}[^\u2019]*)(\u2018([^0-9]|$)|$|\u2019[a-z])/, '\u2019$2$3'],
	// ending '
	[/((\u2018[^']*)|[a-z])'([^0-9]|$)/, '$1\u2019$3'],
	// backwards apostrophe
	[/(\B|^)\u2018(?=([^\u2018\u2019]*\u2019\b)*([^\u2018\u2019]*\B\W[\u2018\u2019]\b|[^\u2018\u2019]*$))/, '$1\u2019'],
];
document.querySelectorAll("header *, #page-metadata *").forEach(element => {
	element.childNodes.forEach(node => {
		if (node.childNodes.length > 0) return;
		textCleanRegexps.forEach(tcr => {
			node.textContent = node.textContent.replace(new RegExp(tcr[0], 'ig'), tcr[1]);
		});
	});
});

/*	Guard against emojification of footnote backlinks.
	*/
document.querySelectorAll(".footnote-back").forEach(backlink => {
	backlink.textContent += "\u{FE0E}";
});
