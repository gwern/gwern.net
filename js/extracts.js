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
	/*****************/
	/*	Configuration.
		*/

	/*	Target containers.
		*/
	contentContainersSelector: "#markdownBody, #TOC",

	/*	Targets.
		*/
    targets: {
		targetElementsSelector: "a[href], span.defnMetadata", 
		excludedElementsSelector: ".sidenote-self-link, .external-section-embed-popup .footnote-ref, .external-section-embed-popup .footnote-back",
		excludedContainerElementsSelector: "h1, h2, h3, h4, h5, h6",
		testTarget: (target) => {
			let linkTypes = [
				[ "isExtractLink", 			"has-annotation" 	],
				[ "isDefinitionLink", 		"has-annotation" 	],
				[ "isVideoLink", 			"has-content" 		],
				[ "isLocalImageLink", 		"has-content"		],
				[ "isLocalCodeFileLink", 	"has-content"		],
				[ "isLocalDocumentLink", 	"has-content"		],
				[ "isExternalSectionLink", 	"has-content" 		],
				[ "isForeignSiteLink",	 	"has-content"		],
				[ "isCitation", 			null 				],
				[ "isCitationBackLink", 	null 				],
				[ "isInternalSectionLink",	null				]
			];

			for ([ testMethodName, classes ] of linkTypes) {
				if (Extracts[testMethodName](target)) {
					if (classes) target.classList.add(...(classes.split(" ")));
					return true;
				}
			}

			return false;
		}
    },

	/*	Misc. configuration.
		*/
    imageFileExtensions: [ "bmp", "gif", "ico", "jpeg", "jpg", "png", "svg" ],
    codeFileExtensions: [ "R", "css", "hs", "js", "patch", "sh", "php", "conf", "html" ],
    qualifyingForeignDomains: [ 
    	"www.greaterwrong.com", 
    	"greaterwrong.com", 
    	"www.lesswrong.com",
    	"lesswrong.com" 
    	],

	/*	Infrastructure.
		*/
	referenceElementContainerSelector: "#link-bibliography",
	referenceElementEntrySelectorPrefix: "#link-bibliography > ol > li > p:first-child",
	annotatedTargetSelectors: [ "a.docMetadata", "span.defnMetadata" ],

	/***********/
	/*	General.
		*/
    cleanup: () => {
		GWLog("Extracts.cleanup", "extracts.js", 1);

		//  Target restore function (same for mobile and non-mobile).
		let restoreTarget = (target) => {
			target.classList.remove("has-content", "has-annotation");
		};

		if (GW.isMobile()) {
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

		Extracts.referenceElementContainer = document.querySelector(Extracts.referenceElementContainerSelector);

		/*  Do not provide extracts for annotated links that are link
			bibliography entries, as their annotations are right there below the
			link itself.
			*/
		Extracts.targets.excludedElementsSelector = [ 
			Extracts.targets.excludedElementsSelector, 
			Extracts.annotatedTargetSelectors.map(selector => `${Extracts.referenceElementEntrySelectorPrefix} ${selector}`).join(", ")
		].join(", ");

        if (GW.isMobile()) {
			//  TEMPORARY!!
			return;

            GWLog("Mobile client detected. Injecting pop-ins.", "extracts.js", 1);

			//  Target prepare function.
			Extracts.prepareTargetForPopins = (target) => {
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

	//  Helper methods.
    qualifyLinksInPopContent: (popX, target) => {
		let targetHref = target.getAttribute("href");
		popX.querySelectorAll("a[href^='#']").forEach(anchorLink => {
			anchorLink.setAttribute("href", targetHref.match(/^([^#]+)/)[1] + anchorLink.hash);
		});
    },
    nearestBlockElement: (element) => {
    	return element.closest("address, aside, blockquote, dd, div, dt, figure, footer, h1, h2, h3, h4, h5, h6, header, li, p, pre, section, table, tfoot, ol, ul");
    },
	fillPopElement: (popElement, target, possiblePopTypes) => {
		GWLog("Extracts.fillPopElement", "extracts.js", 2);

		for ([ testMethodName, fillMethodName, classes ] of possiblePopTypes) {
			if (   Extracts[fillMethodName] != null
				&& Extracts[testMethodName](target)) {
				popElement.innerHTML = Extracts[fillMethodName](target);
				if (classes)
					popElement.classList.add(...(classes.split(" ")));
				break;
			}
		}

		if (popElement.childElementCount != 0) {
			return true;
		} else {
			GWLog("Unable to fill popup ( " + (target.href || target.dataset.originalDefinitionId) + " )!", "extracts.js", 1);
			return false;
		}
	},
	urlForTarget: (target) => {
		return (target.tagName == "A" ? new URL(target.href) : null);
	},

	//  Reference links, for extracts and definitions.
	referenceElementContainer: null,

	//  Summaries of links to elsewhere.
	isExtractLink: (target) => {
		return target.classList.contains("docMetadata");
	},
    extractForTarget: (target) => {
		GWLog("Extracts.extractForTarget", "extracts.js", 2);

		let referenceElement = Extracts.referenceElementContainer.querySelector(`${Extracts.referenceElementEntrySelectorPrefix} ` + 
								`a[href='${target.getAttribute("href")}']`);
		let referenceListEntry = referenceElement.closest("li");

		let titleHTML = referenceElement.innerHTML.trimQuotes();
		let titleText = referenceElement.textContent;
		let abstractHTML = referenceListEntry.querySelector("blockquote").innerHTML;

		//  Link to original URL (for archive links) or link to archive (for live links).
        var archiveOrOriginalLinkHTML = "";
        if (   referenceElement.dataset.urlOriginal != undefined 
        	&& referenceElement.dataset.urlOriginal != target.href) {
            archiveOrOriginalLinkHTML = (`<span class="originalURL"><code>` + "[" + 
            		   `<a href="${referenceElement.dataset.urlOriginal}" target="_new" 
                       		title="Link to original URL for ‘${titleText}’" 
                       		alt="Original URL for this archived link; may be broken.">` + 
                       "URL" + `</a>` + "]" + `</code></span>`);
        } else if (!target.href.startsWithAnyOf([ "https://www.gwern.net", "https://en.wikipedia.org", "https://archive.org", "https://www.biorxiv.org", "https://arxiv.org" ])) {
			archiveOrOriginalLinkHTML = (`<span class="iaMirror">` +
					   `<a title="Search Internet Archive via Memento for mirrors of URL: <${target.href}> (for ‘${titleText}’)" 
					   		href="http://timetravel.mementoweb.org/list/20100101000000/${target.href}" target="_new">` +
					   `</a></span>`);
        }

		//  Extract title/link.
		let titleLinkHTML = `<a class="title-link" target="_new" href="${target.href}" title="Open ${target.href} in a new window">${titleHTML}</a>`;

		//	Author.
		let authorElement = referenceListEntry.querySelector(".author");
		let authorHTML = (authorElement ? `<span class="data-field author">${(authorElement.textContent || "")}</span>` : ``);

		//  Link to citations on Google Scholar, or link to search for links on Google.
        var citationsOrLinks = "";
        if (referenceElement.dataset.doi != undefined) {
            citationsOrLinks = `; <a href="https://scholar.google.com/scholar?q=%22${referenceElement.dataset.doi}%22+OR+%22${titleText}%22" target="_new" title="Reverse citations of this paper (‘${titleText}’), with DOI ‘${referenceElement.dataset.doi}’, in Google Scholar">` + "cites" + `</a>`;
        } else if (target.href.includesAnyOf([ "pdf", "https://arxiv.org", "https://openreview.net", "ieee.org", "rand.org", "dspace.mit.edu", "thegradient.pub", "inkandswitch.com", "nature.com", "sciencemag.org" ])) {
            /* Not all scholarly papers come with DOIs; eg it's the policy of Arxiv to *not* provide DOIs. ;_; */
            citationsOrLinks = `; <a href="https://scholar.google.com/scholar?q=%22${titleText}%22" target='_new' title="Reverse citations of this paper (‘${titleText}’) in Google Scholar">` + "cites" + `</a>`;
        } else if (!target.href.startsWith("https://en.wikipedia.org")) {
            citationsOrLinks = `; <a class="cites" href="https://www.google.com/search?num=100&q=link%3A%22${target.href}%22+OR+%22${titleText}%22" target="_new" title="Links to this page (‘${titleText}’) in Google">` + "links" + `</a>`;
        }

		//	Date; citations/links.
		let dateElement = referenceListEntry.querySelector(".date");
		let dateAndCitationsOrLinksHTML = (dateElement ? ` <span class="date-plus-cites">(<span class="data-field date">${dateElement.textContent}</span>${citationsOrLinks})</span>` : ``);

		//  The fully constructed extract popup contents.
        return `<div>` +
                   `<p class="data-field title">${archiveOrOriginalLinkHTML}${titleLinkHTML}</p>` +
                   `<p class="data-field author-plus-date">${authorHTML}${dateAndCitationsOrLinksHTML}</p>` +
                   `<div class="data-field popupAbstract">${abstractHTML}</div>` +
               `</div>`;
    },

	//  Definitions.
    isDefinitionLink: (target) => {
		return target.classList.contains("defnMetadata");
	},
    definitionForTarget: (target) => {
		GWLog("Extracts.definitionForTarget", "extracts.js", 2);

		let referenceElement = Extracts.referenceElementContainer.querySelector(`${Extracts.referenceElementEntrySelectorPrefix} ` + 
								`span[data-original-definition-id='${target.dataset.originalDefinitionId}']`);
		let referenceListEntry = referenceElement.closest("li");

		let titleHTML = referenceElement.innerHTML.trimQuotes();
		let titleText = referenceElement.textContent;
		let abstractHTML = referenceListEntry.querySelector("blockquote").innerHTML;

		let authorElement = referenceListEntry.querySelector(".author");
		let authorHTML = (authorElement ? `<span class="data-field author">${(authorElement.textContent || "")}</span>` : ``);

		let dateElement = referenceListEntry.querySelector(".date");
		let dateHTML = (dateElement ? ` (${dateElement.textContent})` : ``);

        return `<div>` +
        		   `<p class="data-field title">${titleHTML}</p>` +
        		   `<p class="data-field author-plus-date">${authorHTML}${dateHTML}</p>` +
        		   `<div class="data-field popupAbstract">${abstractHTML}</div>` +
        	   `</div>`;
    },

	//  Videos (both local and remote).
    youtubeId: (href) => {
        let match = href.match(/^.*(youtu.be\/|v\/|u\/\w\/|embed\/|watch\?v=|\&v=)([^#\&\?]*).*/);
        if (match && match[2].length == 11) {
            return match[2];
        } else {
            return null;
        }
    },
    isVideoLink: (target) => {
		let url = Extracts.urlForTarget(target);
		if (!url) return false;

		if ([ "www.youtube.com", "youtube.com", "youtu.be" ].includes(url.hostname)) {
			return (Extracts.youtubeId(target.href) != null);
		} else {
			return false;
		}
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

	//  Citations.
    isCitation: (target) => {
		return target.classList.contains("footnote-ref");
	},

	//  Context surrounding a citation (displayed on footnote-back links).
    isCitationBackLink: (target) => {
	    return target.classList.contains("footnote-back");
    },

	//  Identified sections of the current page.
    isInternalSectionLink: (target) => {
		let url = Extracts.urlForTarget(target);
		if (!url) return false;

		return (   url.hostname == location.hostname
				&& url.pathname == location.pathname
				&& url.hash > "");
	},
    sectionEmbedForTarget: (target) => {
		GWLog("Extracts.sectionEmbedForTarget", "extracts.js", 2);

        let targetElement = document.querySelector(target.getAttribute('href'));
        let nearestBlockElement = Extracts.nearestBlockElement(targetElement);

		//  Unwrap sections and {foot|side}notes from their containers.
		let sectionEmbedHTML = (nearestBlockElement.tagName == "SECTION" || target.classList.contains("footnote-ref")) 
								? nearestBlockElement.innerHTML 
								: nearestBlockElement.outerHTML;

        return `<div>${sectionEmbedHTML}</div>`;
    },

	//  TOC links.
	isTOCLink: (target) => {
		return (target.closest("#TOC") != null);
	},

	//  Identified sections of another page on gwern.net.
    isExternalSectionLink: (target) => {
		let url = Extracts.urlForTarget(target);
		if (  !url
			|| url.hostname != location.hostname
			|| url.pathname == location.pathname)
			return false;

		return true;
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
			if (target.hash > "")
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

	//  Other websites.
	isForeignSiteLink: (target) => {
		let url = Extracts.urlForTarget(target);
		if (!url) return false;

		return Extracts.qualifyingForeignDomains.includes(url.hostname);
	},
	foreignSiteForTarget: (target) => {
		let url = Extracts.urlForTarget(target);

		if ([ "www.lesswrong.com", "lesswrong.com", "www.greaterwrong.com", "greaterwrong.com" ].includes(url.hostname)) {
			url.hostname = "www.greaterwrong.com";
			url.search = "format=preview&theme=classic";
		}

		return `<div><iframe src="${url.href}" frameborder="0" allowfullscreen sandbox></iframe></div>`;
	},

	//  Locally hosted images.
    isLocalImageLink: (target) => {
		let url = Extracts.urlForTarget(target);
		if (  !url
			|| url.hostname != location.hostname)
			return false;

		let imageFileURLRegExp = new RegExp(
			  '(' 
			+ Extracts.imageFileExtensions.map(ext => `\\.${ext}`).join("|") 
			+ ')$'
		, 'i');
		return (url.pathname.match(imageFileURLRegExp) != null);
    },
    localImageForTarget: (target) => {
		GWLog("Extracts.localImageForTarget", "extracts.js", 2);

        //  Note that we pass in the original image-link’s classes - this is good for classes like ‘invertible’.
        return `<div><img class="${target.classList}" src="${target.href}" loading="lazy"></div>`;
    },

	//  Locally hosted documents (html, pdf, etc.).
    isLocalDocumentLink: (target) => {
		let url = Extracts.urlForTarget(target);
		if (  !url
			|| url.hostname != location.hostname)
			return false;

	    return (   url.pathname.startsWith("/docs/www/")
	    		|| (   url.pathname.startsWith("/docs/")
	    			&& url.pathname.match(/\.(html|pdf)$/i) != null));
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
		let url = Extracts.urlForTarget(target);
		if (  !url
			|| url.hostname != location.hostname)
			return false;

		let codeFileURLRegExp = new RegExp(
			  '(' 
			+ Extracts.codeFileExtensions.map(ext => `\\.${ext}`).join("|") 
			+ ')$'
		, 'i');
		return (url.pathname.match(codeFileURLRegExp) != null);
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
						target.popup.innerHTML = `<div><pre><code>${htmlEncodedResponse}</code></pre></div>`;
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

	/**********/
	/*	Popins.
		*/
    preparePopin: (popin, target) => {
		GWLog("Extracts.preparePopin", "extracts.js", 2);

		//  Import the class(es) of the target, and add some others.
		popin.classList.add(...target.classList, "extract-popin");

		//	Inject the extract for the target into the popin.
		if (Extracts.fillPopElement(popin, target, [
			[ "isExtractLink", 			"extractForTarget", 				null 										],
			[ "isDefinitionLink", 		"definitionForTarget", 				"definition-popin" 						],
			[ "isVideoLink", 			"videoForTarget", 					"video-popin object-popin" 				],
			[ "isLocalImageLink", 		"localImageForTarget", 				"image-popin object-popin" 				],
			[ "isLocalCodeFileLink", 	"localCodeFileForTarget", 			"local-code-file-popin" 				],
			[ "isLocalDocumentLink", 	"localDocumentForTarget", 			"local-document-popin object-popin" 	],
			[ "isExternalSectionLink", 	"externalSectionEmbedForTarget", 	"external-section-embed-popin"			],
			[ "isForeignSiteLink",	 	"foreignSiteForTarget", 			"foreign-site-popin object-popin" 							],
			[ "isCitation", 			"sectionEmbedForTarget", 			"footnote-popin" 						],
			[ "isCitationBackLink", 	null, 								null					 				],
			[ "isInternalSectionLink",	null,					 			null				 					]
			]) == false)
			return false;

		//  Ensure no reflow due to figures.
		popup.querySelectorAll("img[width]").forEach(img => {
			img.style.width = img.width + "px";
		});

		//  Qualify internal links in extracts.
		if (   Extracts.isExtractLink(target) 
			&& target.getAttribute("href").startsWith("/"))
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
    preparePopup: (popup, target) => {
		GWLog("Extracts.preparePopup", "extracts.js", 2);

		//  Import the class(es) of the target, and add some others.
		popup.classList.add(...target.classList, "extract-popup", "markdownBody");
		//  We then remove some of the imported classes.
		popup.classList.remove("has-annotation", "has-content", "spawns-popup");

		//	Inject the extract for the target into the popup.
		if (Extracts.fillPopElement(popup, target, [
			[ "isExtractLink", 			"extractForTarget", 				null 										],
			[ "isDefinitionLink", 		"definitionForTarget", 				"definition-popup" 						],
			[ "isVideoLink", 			"videoForTarget", 					"video-popup object-popup" 				],
			[ "isLocalImageLink", 		"localImageForTarget", 				"image-popup object-popup" 				],
			[ "isLocalCodeFileLink", 	"localCodeFileForTarget", 			"local-code-file-popup" 				],
			[ "isLocalDocumentLink", 	"localDocumentForTarget", 			"local-document-popup object-popup" 	],
			[ "isExternalSectionLink", 	"externalSectionEmbedForTarget", 	"external-section-embed-popup"			],
			[ "isForeignSiteLink",	 	"foreignSiteForTarget", 			"foreign-site-popup object-popup" 							],
			[ "isCitation", 			"sectionEmbedForTarget", 			"footnote-popup" 						],
			[ "isCitationBackLink", 	"sectionEmbedForTarget", 			"citation-context-popup" 				],
			[ "isInternalSectionLink",	"sectionEmbedForTarget", 			"section-embed-popup" 					]
			]) == false)
			return false;

		if (Extracts.isCitation(target)) {
			//  Do not spawn footnote popup if sidenote is visible.
			if (isOnScreen(document.querySelector(target.hash)))
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
				let popupdiv = popup.firstElementChild;
				popupdiv.insertBefore(initialFigure, popupdiv.firstElementChild);
			}
		}

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
		if (   Extracts.isLocalDocumentLink(target)
			|| Extracts.isForeignSiteLink(target)
			) {
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

GW.notificationCenter.fireEvent("Extracts.didLoad");

doWhenPageLoaded(() => {
	let serviceProviderObjectName = GW.isMobile() ? "Popins" : "Popups";

	if (window[serviceProviderObjectName])
		Extracts.setup();
	else
		GW.notificationCenter.addHandlerForEvent(serviceProviderObjectName + ".setupDidComplete", () => {
			Extracts.setup();
		}, { once: true });
});
