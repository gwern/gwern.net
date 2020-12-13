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
    popupStylesID: "popups-styles",
    popupContainerID: "popup-container",
    popupContainerParentSelector: "html",
    popupContainerZIndex: "1000",

    // WARNING: selectors must not contain periods; Pandoc will generate section headers which contain periods in them, which will break the query selector; see https://github.com/jgm/pandoc/issues/6553
    targetElementsSelector: "#markdownBody a.docMetadata, #markdownBody a[href^='./images/'], #markdownBody a[href^='../images/'], #markdownBody a[href^='/images/'], #markdownBody a[href^='https://www.gwern.net/images/'], #markdownBody a[href*='youtube.com'], #markdownBody a[href*='youtu.be'], #TOC a, #markdownBody a[href^='#'], #markdownBody a.footnote-back, span.defnMetadata",
    excludedElementsSelector: ".footnote-ref",
    excludedContainerElementsSelector: "h1, h2, h3, h4, h5, h6",
    minPopupWidth: 360,
    maxPopupWidth: 640,
    popupBorderWidth: 3,

    popupBreathingRoomX: 24.0,
    popupBreathingRoomY: 16.0,

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
    },
    cleanup: () => {
		GWLog("Extracts.cleanup", "popups.js", 1);

        //  Unbind event listeners.
        Extracts.unbind();

        //  Remove popups container and injected styles.
        document.querySelectorAll(`#${Extracts.popupStylesID}, #${Extracts.popupStylesID}-default, #${Extracts.popupContainerID}`).forEach(element => element.remove());
    },
    setup: () => {
		GWLog("Extracts.setup", "popups.js", 1);

        //  Run cleanup.
        Extracts.cleanup();

		/*  We consider a client to be mobile if one of two conditions obtain:
		    1. JavaScript detects touch capability, AND viewport is narrow; or,
		    2. CSS does NOT detect hover capability.
		    */
        if (   (   ('ontouchstart' in document.documentElement)
        		&& GW.mediaQueries.mobileWidth.matches)
        	|| !GW.mediaQueries.hoverAvailable.matches) {
            GWLog("Mobile client detected. Exiting.", "popups.js", 1);
            return;
        } else {
            GWLog("Non-mobile client detected. Setting up.", "popups.js", 1);
        }

        //  Inject styles.
        document.querySelector("head").insertAdjacentHTML("beforeend", Extracts.popupStylesHTML);

        //  Inject popups container.
        let popupContainerParent = document.querySelector(Extracts.popupContainerParentSelector);
        if (!popupContainerParent) {
            GWLog("Popup container parent element not found. Exiting.", "popups.js", 1);
            return;
        }
        popupContainerParent.insertAdjacentHTML("beforeend", `<div id='${Extracts.popupContainerID}' style='z-index: ${Extracts.popupContainerZIndex};'></div>`);

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
    //  The mouseenter event.
    targetMouseenter: (event) => {
		GWLog("Extracts.targetMouseenter", "popups.js", 2);

        //  Get the target.
        let target = event.target.closest(Extracts.targetElementsSelector);

        //  Stop the countdown to un-pop the popup.
		Extracts.clearPopupTimers();

        Extracts.popupSpawnTimer = setTimeout(() => {
			GWLog("Extracts.popupSpawnTimer fired", "popups.js", 2);

            let popupContainerViewportRect = Extracts.popupContainer.getBoundingClientRect();
            let targetViewportRect = target.getBoundingClientRect();
            let targetOriginInPopupContainer = {
                x: (targetViewportRect.left - popupContainerViewportRect.left),
                y: (targetViewportRect.top - popupContainerViewportRect.top)
            };
            let mouseEnterEventPositionInPopupContainer = {
                x: (event.clientX - popupContainerViewportRect.left),
                y: (event.clientY - popupContainerViewportRect.top)
            };

			//  Remove existing popup, if any.
            Extracts.popup = Extracts.popupContainer.querySelector("#popupdiv");
			Extracts.despawnPopup();
			Extracts.popup = null;

            //  Create the popup.
			Extracts.popup = document.createElement('div');
			Extracts.popup.id = "popupdiv";
			Extracts.popup.className = target.className;

            //  Inject the contents of the popup into the popup div.
            Extracts.popup.removeAttribute("style");

			let videoId = (target.tagName == "A") ? Extracts.youtubeId(target.href) : null;
			if (videoId) {
				Extracts.popup.innerHTML = Extracts.videoForTarget(target, videoId);
			} else if (target.classList.contains("footnote-back")) {
				Extracts.popup.innerHTML = Extracts.citationContextForTarget(target);
			} else if (target.tagName == "A" && target.getAttribute("href").startsWith("#")) {
				Extracts.popup.innerHTML = Extracts.sectionEmbedForTarget(target);
			} else if (target.tagName == "A" && target.href.startsWith("https://www.gwern.net/images/")) {
				Extracts.popup.innerHTML = Extracts.localImageForTarget(target);
			} else if (target.classList.contains("docMetadata")) {
				Extracts.popup.innerHTML = Extracts.extractForTarget(target);
			} else if (target.classList.contains("defnMetadata")) {
				Extracts.popup.innerHTML = Extracts.definitionForTarget(target);
			}

			if (Extracts.popup.childElementCount == 0)
				return;

            if (Extracts.popup.firstElementChild.tagName == 'DIV') {
            	let innerDiv = Extracts.popup.firstElementChild;

            	innerDiv.style.minWidth = `${Extracts.minPopupWidth}px`;
            	innerDiv.style.maxWidth = `${Extracts.maxPopupWidth}px`;

            	if (target.tagName == "A" && target.getAttribute("href").startsWith("#") && target.closest("#TOC") == null) {
            		// Section embed elsewhere but the TOC.
                    innerDiv.style.maxHeight = `calc(${Extracts.maxPopupWidth}px * 0.75)`;
                } else {
	            	innerDiv.style.maxHeight = `calc(100vh - 2 * ${Extracts.popupBorderWidth}px - 26px)`;
            	}
            }

            //  Inject the popup into the page.
            Extracts.popup.style.visibility = "hidden";
            Extracts.popup.style.left = "0px";
            Extracts.popup.style.top = "0px";
            document.querySelector(`#${Extracts.popupContainerID}`).appendChild(Extracts.popup);

            //  Add event listeners.
            Extracts.popup.addEventListener("mouseup", Extracts.popupMouseup);
            Extracts.popup.addEventListener("mouseenter", Extracts.popupMouseenter);
            Extracts.popup.addEventListener("mouseleave", Extracts.popupMouseleave);

            //  Wait for the "naive" layout to be completed, and then...
            requestAnimationFrame(() => {
                /*  How much "breathing room" to give the target (i.e., offset of
                    the popup).
                    */
                var popupBreathingRoom = {
                    x: Extracts.popupBreathingRoomX,
                    y: Extracts.popupBreathingRoomY
                };

                /*  This is the width and height of the popup, as already determined
                    by the layout system, and taking into account the popup's content,
                    and the max-width, min-width, etc., CSS properties.
                    */
                var popupIntrinsicWidth = Extracts.popup.clientWidth;
                var popupIntrinsicHeight = Extracts.popup.clientHeight;

                var provisionalPopupXPosition;
                var provisionalPopupYPosition;

                var tocLink = target.closest("#TOC");
                if (tocLink) {
                    provisionalPopupXPosition = document.querySelector("#TOC").getBoundingClientRect().right + 1.0 - popupContainerViewportRect.left;
                    provisionalPopupYPosition = mouseEnterEventPositionInPopupContainer.y - ((event.clientY / window.innerHeight) * popupIntrinsicHeight);
                } else {
	                var offToTheSide = false;

					/*  Can the popup fit above the target? If so, put it there.
						Failing that, can it fit below the target? If so, put it there.
						*/
					var popupSpawnYOriginForSpawnAbove = Math.min(mouseEnterEventPositionInPopupContainer.y - popupBreathingRoom.y,
																  targetOriginInPopupContainer.y + targetViewportRect.height - (popupBreathingRoom.y * 2.0));
					var popupSpawnYOriginForSpawnBelow = Math.max(mouseEnterEventPositionInPopupContainer.y + popupBreathingRoom.y,
																  targetOriginInPopupContainer.y + (popupBreathingRoom.y * 2.0));
					if (  popupSpawnYOriginForSpawnAbove - popupIntrinsicHeight >= popupContainerViewportRect.y * -1) {
						//  Above.
						provisionalPopupYPosition = popupSpawnYOriginForSpawnAbove - popupIntrinsicHeight;
					} else if (popupSpawnYOriginForSpawnBelow + popupIntrinsicHeight <= (popupContainerViewportRect.y * -1) + window.innerHeight) {
						//  Below.
						provisionalPopupYPosition = popupSpawnYOriginForSpawnBelow;
					} else {
						/*  The popup does not fit above or below! We will have to
							put it off to the left or right.
							*/
						offToTheSide = true;
					}

					if (offToTheSide) {
						popupBreathingRoom.x *= 2.0;
						provisionalPopupYPosition = mouseEnterEventPositionInPopupContainer.y - ((event.clientY / window.innerHeight) * popupIntrinsicHeight);
						if (provisionalPopupYPosition - popupContainerViewportRect.y < 0)
							provisionalPopupYPosition = 0.0;

						//  Determine whether to put the popup off to the right, or left.
						if (  mouseEnterEventPositionInPopupContainer.x
							+ popupBreathingRoom.x
							+ popupIntrinsicWidth
							  <=
							  popupContainerViewportRect.x * -1
							+ window.innerWidth) {
							//  Off to the right.
							provisionalPopupXPosition = mouseEnterEventPositionInPopupContainer.x + popupBreathingRoom.x;
						} else if (  mouseEnterEventPositionInPopupContainer.x
								   - popupBreathingRoom.x
								   - popupIntrinsicWidth
									 >=
									 popupContainerViewportRect.x * -1) {
							//  Off to the left.
							provisionalPopupXPosition = mouseEnterEventPositionInPopupContainer.x - popupIntrinsicWidth - popupBreathingRoom.x;
						}
					} else {
						/*  Place popup off to the right (and either above or below),
							as per the previous block of code.
							*/
						provisionalPopupXPosition = mouseEnterEventPositionInPopupContainer.x + popupBreathingRoom.x;
					}
				}

                /*  Does the popup extend past the right edge of the container?
                    If so, move it left, until its right edge is flush with
                    the container's right edge.
                    */
                if (provisionalPopupXPosition + popupIntrinsicWidth > popupContainerViewportRect.width) {
                    provisionalPopupXPosition -= provisionalPopupXPosition + popupIntrinsicWidth - popupContainerViewportRect.width;
                }

                /*  Now (after having nudged the popup left, if need be),
                    does the popup extend past the *left* edge of the container?
                    Make its left edge flush with the container's left edge.
                    */
                if (provisionalPopupXPosition < 0) {
                    provisionalPopupXPosition = 0;
                }

                Extracts.popup.style.left = `${provisionalPopupXPosition}px`;
                Extracts.popup.style.top = `${provisionalPopupYPosition}px`;

                Extracts.popup.style.visibility = "";
                document.activeElement.blur();
            });
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
		Extracts.despawnPopup();
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

            Extracts.popup.classList.add("fading");
			Extracts.setPopupDespawnTimer();
        }, Extracts.popupFadeoutDelay);
    },
    setPopupDespawnTimer: () => {
		GWLog("Extracts.setPopupDespawnTimer", "popups.js", 2);

		Extracts.popupDespawnTimer = setTimeout(() => {
			GWLog("Extracts.popupDespawnTimer fired", "popups.js", 2);

	        Extracts.popup.classList.remove("fading");
			Extracts.despawnPopup();
		}, Extracts.popupFadeoutDuration);
    },
    despawnPopup: () => {
		GWLog("Extracts.despawnPopup", "popups.js", 2);

		if (Extracts.popup == null)
			return;

        Extracts.popup.remove();
        document.activeElement.blur();
        Extracts.popup.innerHTML = "";
    }
};

/********************/
/*	Essential styles.
	*/
Extracts.popupStylesHTML = `<style id='${Extracts.popupStylesID}'>
#${Extracts.popupContainerID} {
    position: absolute;
    left: 0;
    top: 0;
    width: 100%;
    pointer-events: none;
}
#${Extracts.popupContainerID} > * {
    pointer-events: auto;
}

#popupdiv {
    position: absolute;
    opacity: 1.0;
    transition: none;
}
#popupdiv.fading {
    opacity: 0.0;
    transition:
        opacity 0.25s ease-in 0.1s;
}
#popupdiv > div {
    overflow: auto;
    overscroll-behavior: none;
}
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
