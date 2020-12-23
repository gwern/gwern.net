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
    imageFileExtensions: [ "bmp", "gif", "ico", "jpeg", "jpg", "png", "svg" ],
    codeFileExtensions: [ "R", "css", "hs", "js", "patch", "sh", "php", "conf" ],
};

Extracts = {
	/**********/
	/*	Config.
		*/
	contentContainersSelector: "#markdownBody, #TOC",
    // WARNING: selectors must not contain periods; Pandoc will generate section headers which contain periods in them, which will break the query selector; see https://github.com/jgm/pandoc/issues/6553
    imageFileExtensions: Extracts.imageFileExtensions,
    codeFileExtensions: Extracts.codeFileExtensions,
    targets: {
		targetElementsSelector: [
			"a.docMetadata, a[href*='youtube.com'], a[href*='youtu.be'], #TOC a, a[href^='#'], a[href^='/'][href*='#'], a[href^='/docs/www/'], a.footnote-back, span.defnMetadata", 
			Extracts.imageFileExtensions.map(ext => `a[href^='/'][href$='${ext}'], a[href^='https://www.gwern.net/'][href$='${ext}']`).join(", "),
			Extracts.codeFileExtensions.map(ext => `a[href^='/'][href$='${ext}'], a[href^='https://www.gwern.net/'][href$='${ext}']`).join(", ")
			].join(", "),
		excludedElementsSelector: ".external-section-embed-popup .footnote-ref, .external-section-embed-popup .footnote-back",
		excludedContainerElementsSelector: "h1, h2, h3, h4, h5, h6"
    },

	/***********/
	/*	General.
		*/
	isMobile: () => {
		/*  We consider a client to be mobile if one of two conditions obtain:
		    1. JavaScript detects touch capability, AND viewport is narrow; or,
		    2. CSS does NOT detect hover capability.
		    */
// 		return true;
		return (   (   ('ontouchstart' in document.documentElement)
					&& GW.mediaQueries.mobileWidth.matches)
				|| !GW.mediaQueries.hoverAvailable.matches);
	},
    cleanup: () => {
		GWLog("Extracts.cleanup", "extracts.js", 1);

		//  Target restore function (same for mobile and non-mobile).
		let restoreTarget = (target) => {
			if (target.dataset.attributeTitle) {
				//  Restore the title attribute, saved in `data-attribute-title`.
				target.title = target.dataset.attributeTitle;
				//  Remove the data attribute.
				target.removeAttribute("data-attribute-title");
			}

			target.classList.toggle("has-content", false);
			target.classList.toggle("has-annotation", false);
		};

		if (Extracts.isMobile()) {
			//  TEMPORARY!!
			return;

			//  Restore targets and remove popins.
			document.querySelectorAll(Extracts.contentContainersSelector).forEach(container => {
				Popins.removeTargetsWithin(container, Extracts.targets, restoreTarget);
			});

			//  Remove event handler for newly-spawned popups.
			GW.notificationCenter.removeHandlerForEvent("Popups.popinDidInject", Extracts.popinInjectHandler);
		} else {
			//  Remove “popups disabled” icon/button, if present.
			Extracts.removePopupsDisabledShowPopupOptionsDialogButton();

			//  Unbind event listeners, restore targets, and remove popups.
			document.querySelectorAll(Extracts.contentContainersSelector).forEach(container => {
				Popups.removeTargetsWithin(container, Extracts.targets, restoreTarget);
			});

			//  Remove event handler for newly-spawned popups.
			GW.notificationCenter.removeHandlerForEvent("Popups.popupDidSpawn", Extracts.popupSpawnHandler);
		}
    },
    setup: () => {
		GWLog("Extracts.setup", "extracts.js", 1);

		//  Shared target prepare function (for both mobile and non-mobile).
		let sharedPrepareTarget = (target) => {
			if (target.title) {
				//  Preserve the title attribute, for possible restoration later.
				target.dataset.attributeTitle = target.title;
			}

			if (   Extracts.isVideoLink(target)
				|| Extracts.isLocalImageLink(target)
				|| Extracts.isLocalDocumentLink(target)
				|| Extracts.isLocalCodeFileLink(target)
				|| Extracts.isExternalSectionLink(target)) {
				target.classList.toggle("has-content", true);
			}
		};

        if (Extracts.isMobile()) {
			//  TEMPORARY!!
			return;

            GWLog("Mobile client detected. Injecting pop-ins.", "extracts.js", 1);

			//  Target prepare function.
			Extracts.prepareTargetForPopins = (target) => {
				sharedPrepareTarget(target);

				//  Alter the title attribute.
				target.title = "Click to reveal";
			};

			//  Prepare to recursively inject popins within newly-injected popins.
			GW.notificationCenter.addHandlerForEvent("Popins.popinDidInject", Extracts.popinInjectHandler = (info) => {
				Popins.addTargetsWithin(info.popin, Extracts.targets, Extracts.preparePopin, Extracts.prepareTargetForPopins);
			});

			//  Inject popins.
			document.querySelectorAll(Extracts.contentContainersSelector).forEach(container => {
				Popins.addTargetsWithin(container, Extracts.targets, Extracts.preparePopin, Extracts.prepareTargetForPopins);
			});
        } else {
            GWLog("Non-mobile client detected. Activating popups.", "extracts.js", 1);

			if (localStorage.getItem("extract-popups-disabled") == "true") {
				//  Inject “popups disabled” icon/button.
				Extracts.injectPopupsDisabledShowPopupOptionsDialogButton();
				return;
			}

			//  Target prepare function.
			Extracts.prepareTargetForPopups = (target) => {
				sharedPrepareTarget(target);

				//  Remove the title attribute.
				target.removeAttribute("title");
			};

			//  Set up targets.
			document.querySelectorAll(Extracts.contentContainersSelector).forEach(container => {
				Popups.addTargetsWithin(container, Extracts.targets, Extracts.preparePopup, Extracts.prepareTargetForPopups);
			});

			//  Recursively set up targets within newly-spawned popups as well.
			GW.notificationCenter.addHandlerForEvent("Popups.popupDidSpawn", Extracts.popupSpawnHandler = (info) => {
				Popups.addTargetsWithin(info.popup, Extracts.targets, Extracts.preparePopup, Extracts.prepareTargetForPopups);

				//  Remove click listener from code popups, to allow selection.
				if (info.popup.classList.contains("local-code-file-popup"))
					info.popup.removeEventListener("click", Popups.popupClicked);
			});
        }

		GW.notificationCenter.fireEvent("Extracts.setupDidComplete");
    },

	/***********/
	/*	Content.
		*/

    qualifyLinksInPopContent: (popX, target) => {
		let targetHref = target.getAttribute("href");
		popX.querySelectorAll("a[href^='#']").forEach(anchorLink => {
			anchorLink.setAttribute("href", targetHref.match(/^([^#]+)/)[1] + anchorLink.hash);
		});
    },

	//  Summaries of links to elsewhere.
	isExtractLink: (target) => {
		return target.classList.contains("docMetadata");
	},
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
            		   `<a href="${target.dataset.urlOriginal}" target="_new" 
                       		title="Link to original URL for ‘${target.dataset.popupTitle}’" 
                       		alt="Original URL for this archived link; may be broken.">` + 
                       "URL" + `</a>` + "]" + `</code></span>`);
        } else if (!target.href.startsWithAnyOf([ "https://www.gwern.net", "https://en.wikipedia.org", "https://archive.org", "https://www.biorxiv.org", "https://arxiv.org" ])) {
			archiveOrOriginalLink = (`<span class="iaMirror">` +
					   `<a title="Search Internet Archive via Memento for mirrors of URL: <${target.href}> (for ‘${target.dataset.popupTitle}’)" 
					   		href="http://timetravel.mementoweb.org/list/20100101000000/${target.href}" target="_new">` +
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
            citationsOrLinks = `; <a class="cites" href="https://www.google.com/search?num=100&q=link%3A%22${target.href}%22+OR+%22${target.dataset.popupTitle}%22" target="_new" title="Links to this page (‘${target.dataset.popupTitle}’) in Google">` + "links" + `</a>`;
        }

		//	Date; citations/links.
		let dateAndCitationsOrLinks = (target.dataset.popupDate ? ` <span class="date-plus-cites">(<span class="data-field date">${target.dataset.popupDate}</span>${citationsOrLinks})</span>` : ``);

		//  The fully constructed extract popup contents.
        return `<div>` +
                   `<p class="data-field title">${archiveOrOriginalLink}${titleLink}</p>` +
                   `<p class="data-field author-plus-date">${author}${dateAndCitationsOrLinks}</p>` +
                   `<div class="data-field popupAbstract">${target.dataset.popupAbstract}</div>` +
               `</div>`;
    },

	//  Definitions.
    isDefinitionLink: (target) => {
		return target.classList.contains("defnMetadata");
	},
    definitionForTarget: (target) => {
		GWLog("Extracts.definitionForTarget", "extracts.js", 2);

		//  Title and abstract are mandatory.
		if (!target.dataset.popupTitleHtml || !target.dataset.popupAbstract)
			return null;

		let author = `<span class="data-field author">${(target.dataset.popupAuthor || "")}</span>`
		let date = (target.dataset.popupDate ? ` (${target.dataset.popupDate})` : ``);

        return `<div>` +
        		   `<p class="data-field title">${target.dataset.popupTitleHtml}</p>` +
        		   `<p class="data-field author-plus-date">${author}${date}</p>` +
        		   `<div class="data-field popupAbstract">${target.dataset.popupAbstract}</div>` +
        	   `</div>`;
    },

	//  Videos (both local and remote).
    youtubeId: (url) => {
        let match = url.match(/^.*(youtu.be\/|v\/|u\/\w\/|embed\/|watch\?v=|\&v=)([^#\&\?]*).*/);
        if (match && match[2].length == 11) {
            return match[2];
        } else {
            return null;
        }
    },
    isVideoLink: (target) => {
		let videoId = (target.tagName == "A") ? Extracts.youtubeId(target.href) : null;
		return (videoId != null);
    },
    videoForTarget: (target) => {
		GWLog("Extracts.videoForTarget", "extracts.js", 2);

		let videoId = Extracts.youtubeId(target.href);
		let videoEmbedURL = `https://www.youtube.com/embed/${videoId}`;
		let placeholderImgSrc = `https://img.youtube.com/vi/${videoId}/hqdefault.jpg`;
		let srcdocStyles = `<style>` + 
			`* { padding: 0; margin: 0; overflow: hidden; }` + 
			`html, body { height: 100%; } ` + 
			`img, span { position: absolute; width: 100%; top: 0; bottom: 0; margin: auto; } ` + 
			`span { height: 1.5em; text-align: center; font: 48px/1.5 sans-serif; color: white; text-shadow: 0 0 0.5em black; }` + 
			`</style>`;
		let playButtonHTML = `<span class='video-embed-play-button'>&#x25BA;</span>`;
		let srcdocHTML = `<a href='${videoEmbedURL}?autoplay=1'><img src='${placeholderImgSrc}'>${playButtonHTML}</a>`;

		//  `allow-same-origin` only for EXTERNAL videos, NOT local videos!
        return `<div><iframe src="${videoEmbedURL}" srcdoc="${srcdocStyles}${srcdocHTML}" frameborder="0" allowfullscreen sandbox="allow-scripts allow-same-origin"></iframe></div>`;
    },
    nearestBlockElement: (element) => {
    	return element.closest("address, aside, blockquote, dd, dt, figure, footer, h1, h2, h3, h4, h5, h6, header, p, pre, section, table, tfoot, ol, ul");
    },

	//  Identified sections of the current page.
    isInternalSectionLink: (target) => {
   		return (target.tagName == "A" && target.getAttribute("href").startsWith("#"));
	},
    sectionEmbedForTarget: (target) => {
		GWLog("Extracts.sectionEmbedForTarget", "extracts.js", 2);

        let targetElement = document.querySelector(target.getAttribute('href'));
        if (targetElement.tagName != "SECTION")
	        targetElement = Extracts.nearestBlockElement(targetElement);
		let sectionEmbedHTML = (targetElement.tagName == "SECTION") ? targetElement.innerHTML : targetElement.outerHTML;

        return `<div>${sectionEmbedHTML}</div>`;
    },

	//  TOC links.
	isTOCLink: (target) => {
		return (target.closest("#TOC") != null);
	},

	//  Identified sections of another page on gwern.net.
    isExternalSectionLink: (target) => {
    	if (target.tagName != "A")
    		return false;

		let targetHref = target.getAttribute("href");
		return targetHref.match(/^\/[^\.]+?#.+$/) != null;
    },
    cachedPages: { },
    externalSectionEmbedForTarget: (target) => {
		GWLog("Extracts.externalSectionEmbedForTarget", "extracts.js", 2);

		let fillPopup = (markdownBody) => {
			GWLog("Filling popup...", "extracts.js", 2);

			target.popup.innerHTML = `<div>${markdownBody.innerHTML}</div>`;

			//  Give the popup inner div an identifying class.
			target.popup.firstElementChild.classList.toggle("page-" + target.pathname.substring(1), true);

			//  First, qualify internal links in the popup.
			Extracts.qualifyLinksInPopContent(target.popup, target);

			/*  Because the Popups.popupDidSpawn event has already fired,
				we must process the newly-constructed popup manually,
				to enable recursive popups within.
				*/
			Popups.addTargetsWithin(target.popup, Extracts.targets, Extracts.preparePopup, Extracts.prepareTargetForPopups);

			//  Scroll to the target.
			target.popup.scrollTop = target.popup.querySelector(target.hash).getBoundingClientRect().top - target.popup.getBoundingClientRect().top;
		};

		if (Extracts.cachedPages[target.pathname]) {
			requestAnimationFrame(() => {
				fillPopup(Extracts.cachedPages[target.pathname]);
			});
		} else {
			target.popup.classList.toggle("loading", true);
			doAjax({
				location: target.href,
				onSuccess: (event) => {
					if (!target.popup)
						return;

					target.popup.classList.toggle("loading", false);

					target.popup.innerHTML = `<div>${event.target.responseText}</div>`;
					Extracts.cachedPages[target.pathname] = target.popup.querySelector("#markdownBody");
					fillPopup(Extracts.cachedPages[target.pathname]);
				},
				onFailure: (event) => {
					target.popup.classList.toggle("loading", false);

					//  TODO: Inject some sort of "not found" message
				}
			});
		}

		return `<div></div>`;
    },

	//  Context surrounding a citation (displayed on footnote-back links).
    isCitationBackLink: (target) => {
	    return target.classList.contains("footnote-back");
    },
    citationContextForTarget: (target) => {
		GWLog("Extracts.citationContextForTarget", "extracts.js", 2);

        let targetCitation = document.querySelector(target.getAttribute('href'));
        let citationContextBlockElement = Extracts.nearestBlockElement(targetCitation);
		let citationContextHTML = (citationContextBlockElement.tagName == "SECTION") ? citationContextBlockElement.innerHTML : citationContextBlockElement.outerHTML;

        return `<div>${citationContextHTML}</div>`;
    },

	//  Locally hosted images.
    isLocalImageLink: (target) => {
    	if (target.tagName != "A")
    		return false;

		let targetHref = target.getAttribute("href");
		let imageFileURLRegExp = new RegExp('^\\/.*(' + Extracts.imageFileExtensions.map(ext => `\\.${ext}`).join("|") + ')', 'i');

		return targetHref.startsWith("/images/") || (targetHref.match(imageFileURLRegExp) != null);
    },
    localImageForTarget: (target) => {
		GWLog("Extracts.localImageForTarget", "extracts.js", 2);

        //  Note that we pass in the original image-link’s classes - this is good for classes like ‘invertible’.
        return `<div><img class="${target.classList}" src="${target.href}" loading="lazy"></div>`;
    },

	//  Locally hosted documents (html, pdf, etc.).
    isLocalDocumentLink: (target) => {
	    return (target.tagName == "A" && target.getAttribute("href").startsWith("/docs/www"));
    },
    localDocumentForTarget: (target) => {
		GWLog("Extracts.localDocumentForTarget", "extracts.js", 2);

		if (target.href.match(/\.pdf(#|$)/) != null) {
			return `<div><object data="${target.href}"></object></div>`;
		} else {
			return `<div><iframe src="${target.href}" frameborder="0" allowfullscreen sandbox></iframe></div>`;
		}
    },

	//  Locally hosted code files (css, js, hs, etc.).
    isLocalCodeFileLink: (target) => {
    	if (target.tagName != "A")
    		return false;

		let targetHref = target.getAttribute("href");
		let codeFileURLRegExp = new RegExp('^\\/.*(' + Extracts.codeFileExtensions.map(ext => `\\.${ext}`).join("|") + ')', 'i');

		return targetHref.match(codeFileURLRegExp) != null;
    },
    localCodeFileForTarget: (target) => {
		GWLog("Extracts.localCodeFileForTarget", "extracts.js", 2);

		target.popup.classList.toggle("loading", true);
		doAjax({
			location: target.href + ".html",
			onSuccess: (event) => {
				if (!target.popup)
					return;

				target.popup.classList.toggle("loading", false);
				target.popup.innerHTML = event.target.responseText;
			},
			onFailure: (event) => {
				doAjax({
					location: target.href,
					onSuccess: (event) => {
						if (!target.popup)
							return;

						target.popup.classList.toggle("loading", false);
						let htmlEncodedResponse = event.target.responseText.replace(/[<>]/g, c => ('&#' + c.charCodeAt(0) + ';'));
						target.popup.innerHTML = `<pre><code>${htmlEncodedResponse}</code></pre>`;
					},
					onFailure: (event) => {
						target.popup.classList.toggle("loading", false);
						//  TODO: Inject some sort of "not found" message
					}
				});
			}
		});

		return `<div></div>`;
    },

	//  Citations.
    isCitation: (target) => {
		return target.classList.contains("footnote-ref");
	},
    noteAssociatedWithTarget: (target) => {
		if (!target.hash)
			return null;

		//  This could be a footnote, or a sidenote!
		let targetNoteId = target.hash.substr(1);
		return document.querySelector("#" + targetNoteId);
    },
    noteForTarget: (target) => {
		let targetNote = Extracts.noteAssociatedWithTarget(target);
		if (!targetNote)
			return null;

		return `<div>${targetNote.innerHTML}</div>`;
    },

	/**********/
	/*	Popins.
		*/
    fillPopin: (popin, target) => {
		GWLog("Extracts.fillPopin", "extracts.js", 2);

		//  Inject the contents of the popin into the popin div.
		if (Extracts.isVideoLink(target)) {
			//  Videos (both local and remote).
			popin.innerHTML = Extracts.videoForTarget(target);
			popin.classList.add("video-popin", "object-popin");
		} else if (target.classList.contains("footnote-ref")) {
			//  Citations.
			if (!target.hash)
				return false;

			//  This could be a footnote, or a sidenote!
			let targetNoteId = target.hash.substr(1);
			let targetNote = document.querySelector("#" + targetNoteId);
			if (!targetNote)
				return false;

			popin.innerHTML = `<div>${targetNote.innerHTML}</div>`;
			popin.targetNote = targetNote;
			popin.classList.add("footnote-popin");
		} else if (target.classList.contains("footnote-back")) {
			//  There are no citation-context popins.
			return false;
		} else if (target.tagName == "A" && target.getAttribute("href").startsWith("#")) {
			//  There are no section embed popins.
			return false;
		} else if (Extracts.isExternalSectionLink(target)) {
			//  Identified sections of another page on gwern.net.
			popin.innerHTML = Extracts.externalSectionEmbedForTarget(target);
			popin.classList.add("external-section-embed-popin");
		} else if (Extracts.isLocalImageLink(target)) {
			//  Locally hosted images.
			popin.innerHTML = Extracts.localImageForTarget(target);
			popin.classList.add("image-popin", "object-popin");
		} else if (target.classList.contains("docMetadata")) {
			//  Summaries of links to elsewhere.
			popin.innerHTML = Extracts.extractForTarget(target);
		} else if (target.classList.contains("defnMetadata")) {
			//  Definitions.
			popin.innerHTML = Extracts.definitionForTarget(target);
			popin.classList.add("definition-popin");
		} else if (Extracts.isLocalDocumentLink(target)) {
			//  Locally hosted documents (html, pdf, etc.).
			popin.innerHTML = Extracts.localDocumentForTarget(target);
			popin.classList.add("local-document-popin", "object-popin");
		} else if (Extracts.isLocalCodeFileLink(target)) {
			//  Locally hosted code files (css, js, hs, etc.).
			popin.innerHTML = Extracts.localCodeFileForTarget(target);
			popin.classList.add("local-code-file-popin", "object-popin");
		}

		if (popin.childElementCount != 0) {
			return true;
		} else {
			GWLog("Unable to fill popin!", "extracts.js", 1);
			return false;
		}
    },
    preparePopin: (popin, target) => {
		GWLog("Extracts.preparePopin", "extracts.js", 2);

		//  Import the class(es) of the target, and add some others.
		popin.classList.add(...target.classList, "extract-popin");

		//	Inject the extract for the target into the popin.
		if (Extracts.fillPopin(popin, target) == false)
			return false;

		//  Ensure no reflow due to figures.
		popup.querySelectorAll("img[width]").forEach(img => {
			img.style.width = img.width + "px";
		});

		//  Qualify internal links in extracts.
		if (popin.classList.contains("docMetadata") && target.getAttribute("href").startsWith("/"))
			Extracts.qualifyLinksInPopContent(popin, target);

		return true;
    },

	/**********/
	/*	Popups.
		*/
	popupsDisabledShowPopupOptionsDialogButton: null,
	injectPopupsDisabledShowPopupOptionsDialogButton: () => {
		GWLog("Extracts.injectPopupsDisabledShowPopupOptionsDialogButton", "extracts.js", 1);

		if (Extracts.popupsDisabledShowPopupOptionsDialogButton != null)
			return;

		//  Create and inject the button.
		Extracts.popupsDisabledShowPopupOptionsDialogButton = addUIElement(`<div id="popups-disabled-show-popup-options-dialog-button">` + 
			`<button type="button" title="Show options for link popups. (Popups are currently disabled.)"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 640 512"><path d="M64 352c0 35.3 28.7 64 64 64h96v84c0 9.8 11.2 15.5 19.1 9.7L368 416h2L64 179.5V352zm569.8 106.1l-77.6-60c12.1-11.6 19.8-28 19.8-46.1V64c0-35.3-28.7-64-64-64H128c-21.5 0-40.4 10.7-52 27L45.5 3.4C38.5-2 28.5-.8 23 6.2L3.4 31.4c-5.4 7-4.2 17 2.8 22.4l588.4 454.7c7 5.4 17 4.2 22.5-2.8l19.6-25.3c5.4-6.8 4.1-16.9-2.9-22.3z"/></svg></button>` + `</div>`);

		//  Add event listener.
		requestAnimationFrame(() => {
			Extracts.popupsDisabledShowPopupOptionsDialogButton.addActivateEvent(Extracts.popupsDisabledShowPopupOptionsDialogButtonClicked = (event) => {
				GWLog("Extracts.popupsDisabledShowPopupOptionsDialogButtonClicked", "extracts.js", 2);

				event.stopPropagation();
				if (Extracts.showPopupOptionsDialog) {
					Extracts.showPopupOptionsDialog();
				} else {
					GWLog("Script not loaded: extracts-options.js", "extracts.js", 1);
				}
			});
		});
	},
	removePopupsDisabledShowPopupOptionsDialogButton: () => {
		GWLog("Extracts.removePopupsDisabledShowPopupOptionsDialogButton", "extracts.js", 1);

		if (Extracts.popupsDisabledShowPopupOptionsDialogButton == null)
			return;

		Extracts.popupsDisabledShowPopupOptionsDialogButton.remove();
		Extracts.popupsDisabledShowPopupOptionsDialogButton = null;
	},
    fillPopup: (popup, target) => {
		GWLog("Extracts.fillPopup", "extracts.js", 2);

		let possiblePopupTypes = [
			[ "isVideoLink", 			"videoForTarget", 					"video-popup object-popup" 				],
			[ "isCitation", 			"noteForTarget", 					"footnote-popup" 						],
			[ "isCitationBackLink", 	"citationContextForTarget", 		"citation-context-popup" 				],
			[ "isInternalSectionLink",	"sectionEmbedForTarget", 			"external-section-embed-popup" 			],
			[ "isExternalSectionLink", 	"externalSectionEmbedForTarget", 	"external-section-embed-popup"			],
			[ "isLocalImageLink", 		"localImageForTarget", 				"image-popup object-popup" 				],
			[ "isExtractLink", 			"extractForTarget", 				"" 										],
			[ "isDefinitionLink", 		"definitionForTarget", 				"definition-popup" 						],
			[ "isLocalDocumentLink", 	"localDocumentForTarget", 			"local-document-popup object-popup" 	],
			[ "isLocalCodeFileLink", 	"localCodeFileForTarget", 			"local-code-file-popup object-popup" 	]
		];

		for ([ testMethodName, fillMethodName, classes ] of possiblePopupTypes) {
			if (   Extracts[fillMethodName] != null
				&& Extracts[testMethodName](target)) {
				popup.innerHTML = Extracts[fillMethodName](target);
				if (classes > "")
					popup.classList.add(...(classes.split(" ")));
				break;
			}
		}

		if (popup.childElementCount != 0) {
			return true;
		} else {
			GWLog("Unable to fill popup!", "extracts.js", 1);
			return false;
		}
    },
    preparePopup: (popup, target) => {
		GWLog("Extracts.preparePopup", "extracts.js", 2);

		//  Import the class(es) of the target, and add some others.
		popup.classList.add(...target.classList, "extract-popup", "markdownBody");
		//  We then remove some of the imported classes.
		popup.classList.remove("has-annotation", "has-content", "spawns-popup");

		//	Inject the extract for the target into the popup.
		if (Extracts.fillPopup(popup, target) == false)
			return false;

		if (Extracts.isCitation(target)) {
			//  Do not spawn footnote popup if sidenote is visible.
			if (   GW.sidenotes != null
				&& !GW.sidenotes.mediaQueries.viewportWidthBreakpoint.matches
				&& isOnScreen(Extracts.noteAssociatedWithTarget(target)))
				return false;

			/*  Add event listeners to highlight citation when its footnote
				popup is spawned.
				*/
			popup.addEventListener("mouseenter", (event) => {
				target.classList.toggle("highlighted", true);
			});
			popup.addEventListener("mouseleave", (event) => {
				target.classList.toggle("highlighted", false);
			});
			GW.notificationCenter.addHandlerForEvent("Popups.popupWillDespawn", Extracts.footnotePopupDespawnHandler = (info) => {
				target.classList.toggle("highlighted", false);
			});
		} else if (Extracts.isCitationBackLink(target)) {
			//  Do not spawn citation context popup if citation is visible.
			if (isOnScreen(document.querySelector("#markdownBody " + target.getAttribute("href"))))
				return false;

			//  Remove the .targeted class from a targeted citation (if any).
			popup.querySelectorAll(".footnote-ref.targeted").forEach(targetedCitation => {
				targetedCitation.classList.remove("targeted");
			});

			//  Highlight citation in a citation context popup.
			popup.querySelector(target.getAttribute("href")).classList.add("highlighted");
		} else if (Extracts.isTOCLink(target)) {
			popup.classList.add("toc-section-popup");

			//  Special positioning for section links spawned by the TOC.
			target.popupSpecialPositioningFunction = (preparedPopup, popupTarget, mouseEvent) => {
				let popupContainerViewportRect = Popups.popupContainer.getBoundingClientRect();
				let mouseEnterEventPositionInPopupContainer = {
					x: (mouseEvent.clientX - popupContainerViewportRect.left),
					y: (mouseEvent.clientY - popupContainerViewportRect.top)
				};

				var popupIntrinsicHeight = preparedPopup.offsetHeight;

				let provisionalPopupXPosition = document.querySelector("#TOC").getBoundingClientRect().right + 1.0 - popupContainerViewportRect.left;
				let provisionalPopupYPosition = mouseEnterEventPositionInPopupContainer.y - ((mouseEvent.clientY / window.innerHeight) * popupIntrinsicHeight);

				return [ provisionalPopupXPosition, provisionalPopupYPosition ];
			}
		}

		//  Fix full-width figures.
		popup.querySelectorAll(".caption-wrapper").forEach(captionWrapper => {
			captionWrapper.style.minWidth = "";
		});
		popup.querySelectorAll(".full-width").forEach(fullWidthBlock => {
			fullWidthBlock.style.marginLeft = "";
			fullWidthBlock.style.marginRight = "";
		});

		//  Ensure no reflow due to figures.
		popup.querySelectorAll("img[width]").forEach(img => {
			img.style.width = img.width + "px";
		});

		//  Allow for floated figures at the start of abstract.
		if (   Extracts.isExtractLink(target)
			|| Extracts.isDefinitionLink(target)) {
			let initialFigure = popup.querySelector(".popupAbstract > figure.float-right:first-child");
			if (initialFigure) {
				popup.querySelector(".data-field.title").style.paddingRight = "50%";
			}
		}

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
		if (   Extracts.isExtractLink(target) 
			&& target.getAttribute("href").startsWith("/")) {
			Extracts.qualifyLinksInPopContent(popup, target);
		}

		//  Loading spinners.
		if (Extracts.isLocalDocumentLink(target)) {
			popup.classList.toggle("loading", true);
			popup.querySelector("iframe, object").onload = (event) => {
				popup.classList.toggle("loading", false);
			};
			popup.querySelector("iframe, object").onerror = (event) => {
				popup.classList.toggle("loading", false);
				//  TODO: do some sort of "loading failed" message
			};
		}
		if (Extracts.isLocalImageLink(target)) {
			popup.classList.toggle("loading", true);
			popup.querySelector("img").onload = (event) => {
				popup.classList.toggle("loading", false);
			};
			popup.querySelector("img").onerror = (event) => {
				popup.classList.toggle("loading", false);
				//  TODO: do some sort of "loading failed" message
			};
		}

		return true;
    }
};

doWhenPageLoaded(() => {
	GW.notificationCenter.fireEvent("Extracts.didLoad");

	let serviceProviderObjectName = Extracts.isMobile() ? "Popins" : "Popups";

	if (window[serviceProviderObjectName])
		Extracts.setup();
	else
		GW.notificationCenter.addHandlerForEvent(serviceProviderObjectName + ".setupDidComplete", () => {
			Extracts.setup();
		}, { once: true });
});
