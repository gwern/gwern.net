// popups.js: standalone Javascript library for creating 'popups' which display link metadata (typically, title/author/date/summary), for extremely convenient reference/abstract reading.
// Author: Said Achmiz, Shawn Presser (mobile & Youtube support)
// Date: 2019-09-12
// When:
// license: MIT (derivative of footnotes.js, which is PD)

// popups.js parses a HTML document and looks for <a> links which have the 'docMetadata' attribute class, and the attributes 'data-popup-title', 'data-popup-author', 'data-popup-date', 'data-popup-doi', 'data-popup-abstract'.
// (These attributes are expected to be populated already by the HTML document's compiler, however, they can also be done dynamically. See 'https://share.obormot.net/misc/gwern/wikipedia-popups.js' for an example of a library which does Wikipedia-only dynamically on page loads.)

// Popups are inspired by Wikipedia's augmented tooltips (originally implemented as editor-built extensions, now available to all readers via https://www.mediawiki.org/wiki/Page_Previews ). Whenever any such link is mouse-overed by the user, popups.js will pop up a large tooltip-like square with the contents of the attributes. This is particularly intended for references, where it is extremely convenient to autopopulate links such as to Arxiv.org/Biorxiv.org/Wikipedia with the link's title/author/date/abstract, so the reader can see it instantly. Links to 'reverse citations' are provided as much as possible: links with DOIs go to a Semantic Scholar search engine query for that DOI, which prioritizes meta-analyses & systematic reviews to provide context for any given paper (particularly whether it has failed to replicate or otherwise been debunked); for URLs ending in 'PDF' which probably have Semantic Scholar entries, they go to a title search; and for all other URLs, a Google search using the obscure `link:` operator is provided.. For more details, see `LinkMetadata.hs`.

// On mobile, clicking on links (as opposed to hovering over links on desktop) will bring up the annotation or video; another click on it or the popup will then go to it. A click outside it de-activates it.

// For an example of a Hakyll library which generates annotations for Wikipedia/Biorxiv/Arxiv/PDFs/arbitrarily-defined links, see https://www.gwern.net/LinkMetadata.hs ; for a live demonstration, see the links in https://www.gwern.net/newsletter/2019/07

Extracts = {
	/**********/
	/*	Config.
		*/
	contentContainersSelector: "#markdownBody, #TOC",
    // WARNING: selectors must not contain periods; Pandoc will generate section headers which contain periods in them, which will break the query selector; see https://github.com/jgm/pandoc/issues/6553
    targets: {
		targetElementsSelector: "a.docMetadata, a[href^='./images/'], a[href^='../images/'], a[href^='/images/'], a[href^='https://www.gwern.net/images/'], a[href*='youtube.com'], a[href*='youtu.be'], #TOC a, a[href^='#'], a.footnote-back, span.defnMetadata",
		excludedElementsSelector: ".footnote-ref",
		excludedContainerElementsSelector: "h1, h2, h3, h4, h5, h6"
    },

	/******************/
	/*	Implementation.
		*/
    extractForTarget: (target) => {
		GWLog("Extracts.extractForTarget", "extracts.js", 2);

		//  Title and abstract are mandatory.
		if (!target.dataset.popupTitleHtml || !target.dataset.popupAbstract)
			return null;

		//  Link to original URL (for archive links) or link to archive (for live links).
        var archiveOrOriginalLink = "";
        if (   target.dataset.urlOriginal != undefined 
        	&& target.dataset.urlOriginal != target.href) {
            archiveOrOriginalLink = (`<span class="originalURL"><code>` + "[" + 
            		   `<a href="${target.dataset.urlOriginal}" 
                       		title="Link to original URL for ‘${target.dataset.popupTitle}’" 
                       		alt="Original URL for this archived link; may be broken.">` + 
                       "URL" + `</a>` + "]" + `</code></span>`);
        } else if (!target.href.startsWithAnyOf([ "https://www.gwern.net", "https://en.wikipedia.org", "https://archive.org", "https://www.biorxiv.org", "https://arxiv.org" ])) {
			archiveOrOriginalLink = (`<span class="iaMirror">` +
					   `<a title="Search Internet Archive via Memento for mirrors of URL: <${target.href}> (for ‘${target.dataset.popupTitle}’)" 
					   		href="http://timetravel.mementoweb.org/list/20100101000000/${target.href}">` +
					   `</a></span>`);
        }

		//  Extract title/link.
		let titleLink = `<a class="title-link" target="_new" href="${target.href}" title="Open ${target.href} in a new window">${target.dataset.popupTitleHtml}</a>`;

		//	Author.
		let author = `<span class="data-field author">${(target.dataset.popupAuthor || "")}</span>`;

		//  Link to citations on Google Scholar, or link to search for links on Google.
        var citationsOrLinks = "";
        if (target.dataset.popupDoi != undefined) {
            citationsOrLinks = `; <a href="https://scholar.google.com/scholar?q=%22${target.dataset.popupDoi}%22+OR+%22${target.dataset.popupTitle}%22" target="_new" title="Reverse citations of this paper (‘${target.dataset.popupTitle}’), with DOI ‘${target.dataset.popupDoi}’, in Google Scholar">` + "cites" + `</a>`;
        } else if (target.href.includesAnyOf([ "pdf", "https://arxiv.org", "https://openreview.net", "ieee.org", "rand.org", "dspace.mit.edu", "thegradient.pub", "inkandswitch.com", "nature.com", "sciencemag.org" ])) {
            /* Not all scholarly papers come with DOIs; eg it's the policy of Arxiv to *not* provide DOIs. ;_; */
            citationsOrLinks = `; <a href="https://scholar.google.com/scholar?q=%22${target.dataset.popupTitle}%22" target='_new' title="Reverse citations of this paper (‘${target.dataset.popupTitle}’) in Google Scholar">` + "cites" + `</a>`;
        } else if (!target.href.startsWith("https://en.wikipedia.org")) {
            citationsOrLinks = `; <a href="https://www.google.com/search?num=100&q=link%3A%22${target.href}%22+OR+%22${target.dataset.popupTitle}%22" target="_new" title="Links to this page (‘${target.dataset.popupTitle}’) in Google">` + "links" + `</a>`;
        }

		//	Date; citations/links.
		let dateAndCitationsOrLinks = (target.dataset.popupDate ? ` (${target.dataset.popupDate}${citationsOrLinks})` : ``);

		//  The fully constructed extract popup contents.
        return `<div class="popup-extract">` +
                   `<p class="data-field title">${archiveOrOriginalLink}${titleLink}</p>` +
                   `<p class="data-field author-plus-date">${author}${dateAndCitationsOrLinks}</p>` +
                   `<div class="data-field popupAbstract">${target.dataset.popupAbstract}</div>` +
               `</div>`;
    },
    definitionForTarget: (target) => {
		GWLog("Extracts.definitionForTarget", "extracts.js", 2);

		//  Title and abstract are mandatory.
		if (!target.dataset.popupTitleHtml || !target.dataset.popupAbstract)
			return null;

		let author = `<span class="data-field author">${(target.dataset.popupAuthor || "")}</span>`
		let date = (target.dataset.popupDate ? ` (${target.dataset.popupDate})` : ``);

        return `<div class="popup-extract">` +
        		   `<p class="data-field title">${target.dataset.popupTitleHtml}</p>` +
        		   `<p class="data-field author-plus-date">${author}${date}</p>` +
        		   `<div class="data-field popupAbstract">${target.dataset.popupAbstract}</div>` +
        	   `</div>`;
    },
    youtubeId: (url) => {
        let match = url.match(/^.*(youtu.be\/|v\/|u\/\w\/|embed\/|watch\?v=|\&v=)([^#\&\?]*).*/);
        if (match && match[2].length == 11) {
            return match[2];
        } else {
            return '';
        }
    },
    videoForTarget: (target, videoId) => {
		GWLog("Extracts.videoForTarget", "extracts.js", 2);

        return `<div class="popup-video"><iframe src="//www.youtube.com/embed/${videoId}" frameborder="0" allowfullscreen></iframe></div>`;
    },
    nearestBlockElement: (element) => {
    	return element.closest("address, aside, blockquote, dd, dt, figure, footer, h1, h2, h3, h4, h5, h6, header, p, pre, section, table, tfoot, ol, ul");
    },
    sectionEmbedForTarget: (target) => {
		GWLog("Extracts.sectionEmbedForTarget", "extracts.js", 2);

        let targetElement = document.querySelector(target.getAttribute('href'));
        if (targetElement.tagName != "SECTION")
	        targetElement = Extracts.nearestBlockElement(targetElement);
		let sectionEmbedHTML = (targetElement.tagName == "SECTION") ? targetElement.innerHTML : targetElement.outerHTML;

        return `<div class='popup-section-embed'>${sectionEmbedHTML}</div>`;
    },
    citationContextForTarget: (target) => {
		GWLog("Extracts.citationContextForTarget", "extracts.js", 2);

        let targetCitation = document.querySelector(target.getAttribute('href'));
        let citationContextBlockElement = Extracts.nearestBlockElement(targetCitation);
		let citationContextHTML = (citationContextBlockElement.tagName == "SECTION") ? citationContextBlockElement.innerHTML : citationContextBlockElement.outerHTML;

        return `<div class='popup-citation-context'>${citationContextHTML}</div>`;
    },
    localImageForTarget: (target) => {
		GWLog("Extracts.localImageForTarget", "extracts.js", 2);

        // note that we pass in the original image-link's classes - this is good for classes like 'invertible'.
        return `<div class='popup-local-image'><img class='${target.classList}' width='${Extracts.maxPopupWidth}' src='${target.href}'></div>`;
    },

    cleanup: () => {
		GWLog("Extracts.cleanup", "extracts.js", 1);

        //  Unbind event listeners.
        document.querySelectorAll(Extracts.contentContainersSelector).forEach(container => {
        	Popups.removeTargetsWithin(container, Extracts.targets);
        });

        //  Remove popups.
        document.querySelectorAll(`#${Popups.popupContainerID} .extract-popup`).forEach(element => element.remove());
    },
    setup: () => {
		GWLog("Extracts.setup", "extracts.js", 1);

        //  Run cleanup.
        Extracts.cleanup();

        if (Popups.isMobile()) {
            GWLog("Mobile client detected. Exiting.", "extracts.js", 1);
            return;
        } else {
            GWLog("Non-mobile client detected. Setting up.", "extracts.js", 1);
        }

		//  Set up targets.
		let prepareTarget = (target) => {
            //  Remove the title attribute.
            target.removeAttribute("title");
		};
		document.querySelectorAll(Extracts.contentContainersSelector).forEach(container => {
			Popups.addTargetsWithin(container, Extracts.targets, Extracts.preparePopup, prepareTarget);
		});

		//  Recursively set up targets within newly-spawned popups as well.
		GW.notificationCenter.addHandlerForEvent("Popups.popupSpawned", (info) => {
			Popups.addTargetsWithin(info.popup, Extracts.targets, Extracts.preparePopup, prepareTarget);
		});
 
		GW.notificationCenter.fireEvent("Extracts.setupComplete");
    },
    fillPopup: (popup, target) => {
		GWLog("Extracts.fillPopup", "extracts.js", 2);

		//  Inject the contents of the popup into the popup div.
		let videoId = (target.tagName == "A") ? Extracts.youtubeId(target.href) : null;
		if (videoId) {
			//  Videos (both local and remote).
			popup.innerHTML = Extracts.videoForTarget(target, videoId);
		} else if (target.classList.contains("footnote-back")) {
			//  Context surrounding a citation (displayed on footnote-back links).
			popup.innerHTML = Extracts.citationContextForTarget(target);
		} else if (target.tagName == "A" && target.getAttribute("href").startsWith("#")) {
			//  Identified sections of the current page.
			popup.innerHTML = Extracts.sectionEmbedForTarget(target);
		} else if (target.tagName == "A" && target.href.startsWith("https://www.gwern.net/images/")) {
			//  Locally hosted images.
			popup.innerHTML = Extracts.localImageForTarget(target);
		} else if (target.classList.contains("docMetadata")) {
			//  Summaries of links to elsewhere.
			popup.innerHTML = Extracts.extractForTarget(target);
		} else if (target.classList.contains("defnMetadata")) {
			//  Definitions.
			popup.innerHTML = Extracts.definitionForTarget(target);
		}

		return (popup.childElementCount != 0);
    },
    preparePopup: (popup, target) => {
		GWLog("Extracts.preparePopup", "extracts.js", 2);

		//  Import the class(es) of the target, and add some others.
		popup.classList.add(...target.classList, "extract-popup", "markdownBody");

		//  Special handling for section links spawned by the TOC.
		if (target.closest("#TOC")) {
			popup.classList.add("toc-section-popup");
		}

		//	Inject the extract for the target into the popup.
		if (Extracts.fillPopup(popup, target) == false)
			return false;

		//  Fix full-width figures.
		popup.querySelectorAll(".caption-wrapper").forEach(captionWrapper => {
			captionWrapper.style.minWidth = "";
		});
		popup.querySelectorAll(".full-width").forEach(fullWidthBlock => {
			fullWidthBlock.style.marginLeft = "";
			fullWidthBlock.style.marginRight = "";
		});

		//  Expand collapsed code blocks and then re-rectify heights.
		popup.querySelectorAll("pre code").forEach(codeBlock => {
			codeBlock.style.height = "";
			requestAnimationFrame(() => {
				rectifyCodeBlockHeight(codeBlock);
			});
		});

		//  Rectify margin note style.
		popup.querySelectorAll(".marginnote").forEach(marginNote => {
			marginNote.classList.add("inline");
			marginNote.classList.remove("sidenote");
		});

		//  Qualify internal links in extracts.
		let targetHref = target.getAttribute("href");
		if (popup.classList.contains("docMetadata") && targetHref.startsWith("/")) {
			popup.querySelectorAll("a[href^='#']").forEach(anchorLink => {
				let savedHash = anchorLink.hash;
				anchorLink.setAttribute("href", targetHref);
				anchorLink.hash = savedHash;
			});
		}

		return true;
    }
};

doWhenPageLoaded(() => {
	GW.notificationCenter.fireEvent("Extracts.loaded");

	if (window.Popups)
		Extracts.setup();
	else
		GW.notificationCenter.addHandlerForEvent("Popups.setupComplete", () => {
			Extracts.setup();
		}, { once: true });
});
