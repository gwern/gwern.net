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
    stylesID: "extracts-styles",

    // WARNING: selectors must not contain periods; Pandoc will generate section headers which contain periods in them, which will break the query selector; see https://github.com/jgm/pandoc/issues/6553
    targetElementsSelector: "#markdownBody a.docMetadata, #markdownBody a[href^='./images/'], #markdownBody a[href^='../images/'], #markdownBody a[href^='/images/'], #markdownBody a[href^='https://www.gwern.net/images/'], #markdownBody a[href*='youtube.com'], #markdownBody a[href*='youtu.be'], #TOC a, #markdownBody a[href^='#'], #markdownBody a.footnote-back, span.defnMetadata",
    excludedElementsSelector: ".footnote-ref",
    excludedContainerElementsSelector: "h1, h2, h3, h4, h5, h6",

    minPopupWidth: 360,
    maxPopupWidth: 640,
    popupBorderWidth: 3,
    videoPopupWidth: 495,
    videoPopupHeight: 310,

    popupTriggerDelay: 200,
    popupFadeoutDelay: 50,
    popupFadeoutDuration: 250,

	/******************/
	/*	Implementation.
		*/
    popupFadeTimer: false,
    popupDespawnTimer: false,
    popupSpawnTimer: false,
    popupContainer: null,
    popup: null,

    extractForTarget: (target) => {
		GWLog("Extracts.extractForTarget", "popups.js", 2);

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
		GWLog("Extracts.definitionForTarget", "popups.js", 2);

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
		GWLog("Extracts.videoForTarget", "popups.js", 2);

        return `<div class='popup-screenshot'>` +
            `<iframe width="${Extracts.videoPopupWidth}px" height="${Extracts.videoPopupHeight}px"` +
            `src="//www.youtube.com/embed/${videoId}" frameborder="0" allowfullscreen>` +
            `</iframe></div>`;
    },
    sectionEmbedForTarget: (target) => {
		GWLog("Extracts.sectionEmbedForTarget", "popups.js", 2);

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
		GWLog("Extracts.citationContextForTarget", "popups.js", 2);

        let citationContextHTML = document.querySelector(target.getAttribute('href')).closest("address, aside, blockquote, dd, dt, figure, footer, h1, h2, h3, h4, h5, h6, header, p, pre, section, table, tfoot, ol, ul").innerHTML;
        return `<div class='popup-citation-context'>${citationContextHTML}</div>`; // ellipses added via CSS
    },
    localImageForTarget: (target) => {
		GWLog("Extracts.localImageForTarget", "popups.js", 2);

        // note that we pass in the original image-link's classes - this is good for classes like 'invertible'.
        return `<div class='popup-local-image'><img class='${target.classList}' width='${Extracts.maxPopupWidth}' src='${target.href}'></div>`;
    },

	unbind: () => {
		GWLog("Extracts.unbind", "popups.js", 1);

        document.querySelectorAll(Extracts.targetElementsSelector).forEach(target => {
			if (   target.closest(Extracts.excludedElementsSelector) == target
				|| target.closest(Extracts.excludedContainerElementsSelector) != null)
				return;
 
			//  Unbind existing mouseenter/mouseleave events, if any.
            target.removeEventListener("mouseenter", Extracts.targetMouseenter);
            target.removeEventListener("mouseleave", Extracts.targetMouseleave);
        });

		GW.notificationCenter.fireEvent("Extracts.eventsUnbound");
    },
    cleanup: () => {
		GWLog("Extracts.cleanup", "popups.js", 1);

        //  Unbind event listeners.
        Extracts.unbind();

        //  Remove injected styles.
        document.querySelectorAll(`#${Extracts.stylesID}`).forEach(element => element.remove());
    },
    setup: () => {
		GWLog("Extracts.setup", "popups.js", 1);

        //  Run cleanup.
        Extracts.cleanup();

        if (Popups.isMobile()) {
            GWLog("Mobile client detected. Exiting.", "popups.js", 1);
            return;
        } else {
            GWLog("Non-mobile client detected. Setting up.", "popups.js", 1);
        }

        //  Inject styles.
        document.querySelector("head").insertAdjacentHTML("beforeend", Extracts.stylesHTML);

        //  Get all targets.
        document.querySelectorAll(Extracts.targetElementsSelector).forEach(target => {
			if (   target.closest(Extracts.excludedElementsSelector) == target
				|| target.closest(Extracts.excludedContainerElementsSelector) != null)
				return;

            //  Bind mouseenter/mouseleave events.
            target.addEventListener("mouseenter", Extracts.targetMouseenter);
            target.addEventListener("mouseleave", Extracts.targetMouseleave);

            //  Remove the title attribute.
            target.removeAttribute("title");
        });
    },
    fillPopup: (popup, target) => {
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
		//  Import the class(es) of the target.
		popup.classList.add(...target.classList);

		//  Add event listeners.
		popup.addEventListener("mouseup", Extracts.popupMouseup);
		popup.addEventListener("mouseenter", Extracts.popupMouseenter);
		popup.addEventListener("mouseleave", Extracts.popupMouseleave);

		if (popup.firstElementChild.tagName == 'DIV') {
			let innerDiv = popup.firstElementChild;

			innerDiv.style.minWidth = `${Extracts.minPopupWidth}px`;
			innerDiv.style.maxWidth = `${Extracts.maxPopupWidth}px`;

			if (target.tagName == "A" && target.getAttribute("href").startsWith("#") && target.closest("#TOC") == null) {
				// Section embed elsewhere but the TOC.
				innerDiv.style.maxHeight = `calc(${Extracts.maxPopupWidth}px * 0.75)`;
			} else {
				innerDiv.style.maxHeight = `calc(100vh - 2 * ${Extracts.popupBorderWidth}px - 26px)`;
			}
		}
    },
	//  The mouseenter event.
    targetMouseenter: (event) => {
		GWLog("Extracts.targetMouseenter", "popups.js", 2);

        //  Get the target.
        let target = event.target.closest(Extracts.targetElementsSelector);

        //  Stop the countdown to un-pop the popup.
		Extracts.clearPopupTimers();

        Extracts.popupSpawnTimer = setTimeout(() => {
			GWLog("Extracts.popupSpawnTimer fired", "popups.js", 2);

			//  Despawn existing popup, if any.
			Popups.despawnPopup(Extracts.popup);

            //  Create the popup.
			Extracts.popup = Popups.newPopup("popupdiv");

			//	Inject the extract for the target into the popup.
			if (Extracts.fillPopup(Extracts.popup, target) == false)
				return;

			// Prepare the newly created and filled popup for spawning.
			Extracts.preparePopup(Extracts.popup, target);

			// Spawn the prepared popup.
			Popups.spawnPopup(Extracts.popup, target, event);
        }, Extracts.popupTriggerDelay);
    },
    //  The mouseleave event.
    targetMouseleave: (event) => {
		GWLog("Extracts.targetMouseleave", "popups.js", 2);

		Extracts.clearPopupTimers();

        if (!Extracts.popup) return;

		Extracts.setPopupFadeTimer();
    },
    //	The “user moved mouse out of popup” mouseleave event.
    popupMouseleave: (event) => {
		GWLog("Extracts.popupMouseleave", "popups.js", 2);

		Extracts.clearPopupTimers();
		
		Extracts.setPopupFadeTimer();
    },
    //  The “user moved mouse back into popup” mouseenter event.
    popupMouseenter: (event) => {
		GWLog("Extracts.popupMouseenter", "popups.js", 2);

		Extracts.clearPopupTimers();
        Extracts.popup.classList.remove("fading");
    },
    popupMouseup: (event) => {
		GWLog("Extracts.popupMouseup", "popups.js", 2);

		event.stopPropagation();

		Extracts.clearPopupTimers();
		Popups.despawnPopup(Extracts.popup);
    },
    clearPopupTimers: () => {
	    GWLog("Extracts.clearPopupTimers", "popups.js", 2);

        clearTimeout(Extracts.popupFadeTimer);
        clearTimeout(Extracts.popupDespawnTimer);
        clearTimeout(Extracts.popupSpawnTimer);
    },
    setPopupFadeTimer: () => {
		GWLog("Extracts.setPopupFadeTimer", "popups.js", 2);

        Extracts.popupFadeTimer = setTimeout(() => {
			GWLog("Extracts.popupFadeTimer fired", "popups.js", 2);

			Extracts.setPopupDespawnTimer();
        }, Extracts.popupFadeoutDelay);
    },
    setPopupDespawnTimer: () => {
		GWLog("Extracts.setPopupDespawnTimer", "popups.js", 2);

		Extracts.popup.classList.add("fading");
		Extracts.popupDespawnTimer = setTimeout(() => {
			GWLog("Extracts.popupDespawnTimer fired", "popups.js", 2);

			Popups.despawnPopup(Extracts.popup);
		}, Extracts.popupFadeoutDuration);
    }
};

/********************/
/*	Essential styles.
	*/
Extracts.stylesHTML = `<style id='${Extracts.stylesID}'>
#popupdiv img {
	width: 100%;
}
#popupdiv a {
    position: relative;
    z-index: 0;
}
#popupdiv > div .data-field {
    text-align: left;
    text-indent: 0;
}
#popupdiv > div .data-field + .data-field {
    margin-top: 0.25em;
}
#popupdiv > div .data-field:empty {
    display: none;
}
#popupdiv > div.popup-screenshot {
    padding: 0;
    max-width: unset;
}
#popupdiv > div.popup-screenshot img {
    display: block;
}
#popupdiv > div.popup-screenshot a::after {
    content: none;
}

#popupdiv > div.popup-section-embed,
#popupdiv > div.popup-citation-context {
    height: 100%;
    overflow-x: hidden;
}
.popup-citation-context:first-child::before,
.popup-citation-context:last-child::after {
	content: "…";
}

#popupdiv > div.popup-section-embed > h1:first-child,
#popupdiv > div.popup-section-embed > h2:first-child,
#popupdiv > div.popup-section-embed > h3:first-child,
#popupdiv > div.popup-section-embed > h4:first-child,
#popupdiv > div.popup-citation-context > h1:first-child,
#popupdiv > div.popup-citation-context > h2:first-child,
#popupdiv > div.popup-citation-context > h3:first-child,
#popupdiv > div.popup-citation-context > h4:first-child {
    margin-top: 0;
}
#popupdiv > div .icon {
    background-image: none !important;
    position: relative;
}
</style>`;

doWhenPageLoaded(() => {
	GW.notificationCenter.fireEvent("Extracts.loaded");

	Extracts.setup();
});
