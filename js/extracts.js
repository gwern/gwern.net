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

        var doi = "";
        var archive = "";
        if (target.dataset.urlOriginal != undefined && target.dataset.urlOriginal != target.href) {
            archive = (`<span class="originalURL"><code>[` +
                       `<a href="${target.dataset.urlOriginal}" ` +
                       `title="Link to original URL: '${target.dataset.urlOriginal}' (for '${target.dataset.popupTitle}')" ` +
                       `alt="Original URL for this archived link; may be broken.">URL</a>` +
                       `]</code></span>`); }
        else {
            if (   !target.href.startsWith("https://www.gwern.net")
           	&& !target.href.startsWith("https://en.wikipedia.org")
           	&& !target.href.startsWith("https://archive.org")
           	&& !target.href.startsWith("https://www.biorxiv.org")
           	&& !target.href.startsWith("https://arxiv.org")
           	) {
                archive = (`<span class="iaMirror">` +
                           `<a title="Search Internet Archive via Memento for mirrors of URL: '${target.href}' (for '${target.dataset.popupTitle}')" ` +
                           `href="http://timetravel.mementoweb.org/list/20100101000000/${target.href}">` +
                           `</a></span>`);
            }
        }

        if (target.dataset.popupDoi != undefined) {
            doi = `; <a href="https://scholar.google.com/scholar?q=%22${target.dataset.popupDoi}%22+OR+%22${target.dataset.popupTitle}%22" target='_new' title="Reverse citations of the paper '${target.dataset.popupTitle}' with DOI '${target.dataset.popupDoi}' in Google Scholar">cites</a>`;
        } else if (target.href.includes("pdf") ||
                   /* Not all scholarly papers come with DOIs; eg it's the policy of Arxiv to *not* provide DOIs. ;_; */
                   target.href.includes("https://arxiv.org") ||
                   target.href.includes("https://openreview.net") ||
                   target.href.includes("ieee.org") ||
                   target.href.includes("rand.org") ||
                   target.href.includes("dspace.mit.edu") ||
                   target.href.includes("thegradient.pub") ||
                   target.href.includes("inkandswitch.com") ||
                   target.href.includes("nature.com") ||
                   target.href.includes("sciencemag.org") ) {
            doi = `; <a href="https://scholar.google.com/scholar?q=%22${target.dataset.popupTitle}%22" target='_new' title="Reverse citations of the paper '${target.dataset.popupTitle}' in Google Scholar">cites</a>`;
        } else if (!target.href.startsWith("https://en.wikipedia.org")) {
            doi = `; ` +
                  `<a href="https://www.google.com/search?num=100&q=link%3A%22${target.href}%22+OR+%22${target.dataset.popupTitle}%22" ` +
                  `target='_new' ` +
                  `title="Links to this page '${target.dataset.popupTitle}' in Google">` +
                  `links</a>`;
        }
        var icon = "";
        if (   !target.href.startsWith("https://www.gwern.net")
       	&& !target.href.startsWith("/")
       	&& !target.href.startsWith(".")
       	) {
            icon = `<a
                        class='icon'
                        target='_new'
                        href='${target.href}'
                        title='Open this reference in a new window'
                    ></a>`; }
        return `<div class='popup-extract'>` +
                    `<p class='data-field title'>` +
                         archive +
                         `<a
                            class='title-link'
                            target='_new'
                            href='${target.href}'
                            title='${target.href}'
                                >${target.dataset.popupTitleHtml || ""}</a>` +
                         icon +
                    `</p>` +
                    `<p class='data-field author-plus-date'>` +
                        `<span class='data-field author'>${target.dataset.popupAuthor || ""}</span>${target.dataset.popupDate ? (" (" + target.dataset.popupDate + doi + ")") : ""}` +
                    `</p>` +
                    `<div class='data-field popupAbstract'>` +
                        `${target.dataset.popupAbstract || ""}` +
                    `</div>` +
                `</div>`;
    },
    definitionForTarget: (target) => {
		GWLog("Extracts.definitionForTarget", "extracts.js", 2);

        return `<div class='popup-extract'>` +
                    `<p class='data-field title'>` +
                        `${target.dataset.popupTitleHtml || ""}` +
                    `</p>` +
                    `<p class='data-field author-plus-date'>` +
                        `<span class='data-field author'>${target.dataset.popupAuthor || ""}</span>${target.dataset.popupDate ? (" (" + target.dataset.popupDate + ")") : ""}` +
                    `</p>` +
                    `<div class='data-field popupAbstract'>` +
                        `${target.dataset.popupAbstract || ""}` +
                    `</div>` +
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

        return `<div class='popup-video'>` +
            `<iframe src="//www.youtube.com/embed/${videoId}" frameborder="0" allowfullscreen></iframe>` + 
            `</div>`;
    },
    sectionEmbedForTarget: (target) => {
		GWLog("Extracts.sectionEmbedForTarget", "extracts.js", 2);

        let targetSectionHTML = document.querySelector(target.getAttribute('href')).innerHTML;
        if (targetSectionHTML.length < 2) { // not a section but a random <div> target
            return Extracts.citationContextForTarget(target);
        } else {
            return Extracts.citationContextForTarget(target);
        }
        // NOTE: experiment for supporting link ID popups; testing the length breaks link popups because they're too short
        // but if we present *everything* as a citation-with-context, the '.closest' selector including '<section>' seems to work acceptably?
            // return `<div class='popup-section-embed'>${targetSectionHTML}</div>`;
    },
    citationContextForTarget: (target) => {
		GWLog("Extracts.citationContextForTarget", "extracts.js", 2);

        let citationContextHTML = document.querySelector(target.getAttribute('href')).closest("address, aside, blockquote, dd, dt, figure, footer, h1, h2, h3, h4, h5, h6, header, p, pre, section, table, tfoot, ol, ul").innerHTML;
        return `<div class='popup-citation-context'>${citationContextHTML}</div>`; // ellipses added via CSS
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
			popup.innerHTML = Extracts.videoForTarget(target, videoId);
		} else if (target.classList.contains("footnote-back")) {
			popup.innerHTML = Extracts.citationContextForTarget(target);
		} else if (target.tagName == "A" && target.getAttribute("href").startsWith("#")) {
			popup.innerHTML = Extracts.sectionEmbedForTarget(target);
		} else if (target.tagName == "A" && target.href.startsWith("https://www.gwern.net/images/")) {
			popup.innerHTML = Extracts.localImageForTarget(target);
		} else if (target.classList.contains("docMetadata")) {
			popup.innerHTML = Extracts.extractForTarget(target);
		} else if (target.classList.contains("defnMetadata")) {
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
