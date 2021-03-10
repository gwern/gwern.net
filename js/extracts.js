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
    /*  Configuration.
        */

	annotatedTargetSelectors: [ "a.docMetadata", "span.defnMetadata" ],
	annotationLoadHoverDelay: 25,

	/*	Target containers.
		*/
	contentContainersSelector: ".markdownBody, #TOC, #page-metadata, #sidebar",

	/*	Targets.
		*/
    targets: {
		targetElementsSelector: "a[href], span.defnMetadata", 
		excludedElementsSelector: [
			".section-self-link",
			".footnote-self-link",
			".sidenote-self-link"
		].join(", "),
		excludedContainerElementsSelector: "h1, h2, h3, h4, h5, h6",
		testTarget: (target) => {
			let targetTypeInfo = Extracts.targetTypeInfo(target);
			if (targetTypeInfo) {
				//  Not all types of popups are available as popins also.
				if (   Extracts.popFrameProvider == Popins
					&& (   Extracts.isCitationBackLink(target)
						|| Extracts.isTOCLink(target)))
					return false;

				//  Do not allow pop-frames to spawn themselves.
				let containingPopFrame = target.closest(".popframe");
				if (containingPopFrame && Extracts.targetsMatch(containingPopFrame.spawningTarget, target))
					return false;

				if (targetTypeInfo.targetClasses)
					target.classList.add(...(targetTypeInfo.targetClasses.split(" ")));
				return true;
			}

			return false;
		}
    },

	/*	Misc. configuration.
		*/
    imageFileExtensions: [ "bmp", "gif", "ico", "jpeg", "jpg", "png", "svg" ],
    videoFileExtensions: [ "mp4" ],
    codeFileExtensions: [ "R", "css", "hs", "js", "patch", "sh", "php", "conf", "html" ],
    qualifyingForeignDomains: [ 
    	"www.greaterwrong.com", 
    	"greaterwrong.com", 
    	"www.lesswrong.com",
    	"lesswrong.com",
    	/(.+?)\.wikipedia\.org/
    ],
    blacklistedForeignDomains: [
    ],

	imageMaxWidth: 634.0,
	imageMaxHeight: 474.0,
	videoMaxWidth: 634.0,
	videoMaxHeight: 474.0,

	server404PageTitles: [
		"404 Not Found"
	],

    pageTitleRegexp: /^(.+?) · Gwern\.net$/,

	rootDocument: document.firstElementChild,

	/******************/
	/*	Infrastructure.
		*/

	popFrameProviderName: null,
	popFrameProvider: null,

	/***********/
	/*	General.
		*/

    removeTargetsWithin: (container) => {
 		GWLog("Extracts.removeTargetsWithin", "extracts.js", 1);

		//  Target restore function (same for popups and popins).
		let restoreTarget = (target) => {
			//  Restore title attribute, if any.
			if (target.dataset.attributeTitle) {
				target.title = target.dataset.attributeTitle;
				target.removeAttribute("data-attribute-title");
			}

			target.classList.remove("has-content", "has-annotation");
		};

		Extracts.popFrameProvider.removeTargetsWithin(container, Extracts.targets, restoreTarget);
    },

    cleanup: () => {
		GWLog("Extracts.cleanup", "extracts.js", 1);

		//  Unbind event listeners, restore targets, and remove popups.
		document.querySelectorAll(Extracts.contentContainersSelector).forEach(container => {
			Extracts.removeTargetsWithin(container);
		});

		//  Remove content load event handlers.
		[ Extracts.processTargetsOnContentLoad,
		  Extracts.setUpAnnotationLoadEvent,
		  ].forEach(handler => GW.notificationCenter.removeHandlerForEvent("GW.contentDidLoad", handler));

		if (Extracts.popFrameProvider == Popups) {
			//  Remove “popups disabled” icon/button, if present.
			if (Extracts.popupOptionsEnabled) {
				Extracts.removePopupsDisabledShowPopupOptionsDialogButton();
			}
		} else {
		}

		//  Fire cleanup-complete event.
		GW.notificationCenter.fireEvent("Extracts.cleanupDidComplete");
    },

    addTargetsWithin: (container) => {
 		GWLog("Extracts.addTargetsWithin", "extracts.js", 1);

		if (Extracts.popFrameProvider == Popups) {
    		Popups.addTargetsWithin(container, Extracts.targets, Extracts.preparePopup, Extracts.preparePopupTarget);
    	} else if (Extracts.popFrameProvider == Popins) {
    		Popins.addTargetsWithin(container, Extracts.targets, Extracts.preparePopin);
    	}
    },

    setUpAnnotationLoadEventWithin: (container) => {
		GWLog("Extracts.setUpAnnotationLoadEventWithin", "extracts.js", 1);

		//  Get all the annotated targets in the container.
		let allAnnotatedTargetsInContainer = Array.from(container.querySelectorAll(Extracts.annotatedTargetSelectors.join(", ")));

		if (Extracts.popFrameProvider == Popups) {
			//  Add hover event listeners to all the annotated targets.
			allAnnotatedTargetsInContainer.forEach(annotatedTarget => {
				annotatedTarget.addEventListener("mouseenter", annotatedTarget.annotationLoad_mouseEnter = (event) => {
					//  Get the unique identifier of the annotation for the target.
					let annotationIdentifier = Extracts.targetIdentifier(annotatedTarget);

					//  Do nothing if the annotation is already loaded.
					if (Annotations.cachedAnnotationExists(annotationIdentifier))
						return;

					/*  On hover, start a timer, duration of one-half the 
						popup trigger delay...
						*/
					annotatedTarget.annotationLoadTimer = setTimeout(() => {
						/*  ... to load the annotation.
							*/
						Annotations.loadAnnotation(annotationIdentifier);
					}, (Extracts.annotationLoadHoverDelay));
				});
				annotatedTarget.addEventListener("mouseleave", annotatedTarget.annotationLoad_mouseLeave = (event) => {
					/*  Cancel timer on mouseout (no need to commence a load
						on a merely transient hover).
						*/
					clearTimeout(annotatedTarget.annotationLoadTimer);
				});
			});

			/*  Set up handler to remove hover event listeners from all
				the annotated targets in the document.
				*/
			GW.notificationCenter.addHandlerForEvent("Extracts.cleanupDidComplete", () => {
				allAnnotatedTargetsInContainer.forEach(annotatedTarget => {
					annotatedTarget.removeEventListener("mouseenter", annotatedTarget.annotationLoad_mouseEnter);
					annotatedTarget.removeEventListener("mouseleave", annotatedTarget.annotationLoad_mouseLeave);
				});
			}, { once: true });
		} else {
		
		}
    },

    setup: () => {
		GWLog("Extracts.setup", "extracts.js", 1);

		//  TEMPORARY!!
		if (GW.isMobile())
			return;

		//  Set service provider object.
		Extracts.popFrameProvider = window[Extracts.popFrameProviderName];

        if (Extracts.popFrameProvider == Popups) {
            GWLog("Setting up for popups.", "extracts.js", 1);

			if (!Extracts.popupsEnabled()) {
				if (Extracts.popupOptionsEnabled) {
					//  Inject “popups disabled” icon/button.
					Extracts.injectPopupsDisabledShowPopupOptionsDialogButton();
				}
				return;
			}

            GWLog("Activating popups.", "extracts.js", 1);
        } else {
            GWLog("Setting up for popins.", "extracts.js", 1);

            GWLog("Activating popins.", "extracts.js", 1);
        }

		/*  Add handler to set up targets in loaded content (including 
			newly-spawned pop-frames; this allows for recursion), and to
			add hover/click event listeners to annotated targets, to load 
			annotations (fragments).
			*/
		GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", Extracts.processTargetsOnContentLoad = (info) => {
			GWLog("Extracts.processTargetsOnContentLoad", "extracts.js", 2);

			if (info.document.closest(Extracts.contentContainersSelector)) {
				Extracts.addTargetsWithin(info.document);
				Extracts.setUpAnnotationLoadEventWithin(info.document);
			} else {
				info.document.querySelectorAll(Extracts.contentContainersSelector).forEach(container => {
					Extracts.addTargetsWithin(container);
					Extracts.setUpAnnotationLoadEventWithin(container);
				});
			}
		}, { phase: "eventListeners" });

		//  Fire setup-complete event.
		GW.notificationCenter.fireEvent("Extracts.setupDidComplete");
    },

	/***********/
	/*	Content.
		*/

	/*	This array defines the types of ‘targets’ (i.e., annotated links,
		definitions, links pointing to available content such as images or code
		files, citations, etc.) that Extracts supports.
		*/
	targetTypeDefinitions: [
		[ "EXTRACT",  			"isExtractLink",		"has-annotation", 	"annotationForTarget", 			"extract annotation"    ],
		[ "DEFINITION",  		"isDefinition",			"has-annotation",	"annotationForTarget", 			"definition annotation" ],
		[ "CITATION",  			"isCitation", 			null, 				"localTranscludeForTarget", 	"footnote"              ],
		[ "CITATION_BACK_LINK",	"isCitationBackLink", 	null, 				"localTranscludeForTarget", 	"citation-context"      ],
		[ "VIDEO",  			"isVideoLink", 			"has-content", 		"videoForTarget", 				"video object"          ],
		[ "LOCAL_VIDEO", 		"isLocalVideoLink", 	"has-content", 		"localVideoForTarget", 			"video object"          ],
		[ "LOCAL_IMAGE", 		"isLocalImageLink", 	"has-content", 		"localImageForTarget", 			"image object"          ],
		[ "LOCAL_DOCUMENT", 	"isLocalDocumentLink", 	"has-content", 		"localDocumentForTarget", 		"local-document object" ],
		[ "LOCAL_CODE_FILE", 	"isLocalCodeFileLink", 	"has-content", 		"localCodeFileForTarget", 		"local-code-file"       ],
		[ "LOCAL_PAGE",  		"isLocalPageLink", 		"has-content",		"localTranscludeForTarget", 	"local-transclude"      ],
		[ "FOREIGN_SITE", 		"isForeignSiteLink", 	"has-content",	 	"foreignSiteForTarget", 		"foreign-site object"   ]
	],
			
	/*	Returns full type info for the given target. This contains the target 
		type name, the name of the predicate function for identifying targets of
		that type (e.g., isExtractLink), classes which should be applied to 
		targets of that type during initial processing, the fill functions to
		fill popups and popins of that type, and the classes which should be 
		applied to pop-frames of that type.
		*/
	targetTypeInfo: (target) => {
		let info = { };
		for (definition of Extracts.targetTypeDefinitions) {
			[ 	info.typeName, 
				info.predicateFunctionName, 
				info.targetClasses,
				info.popFrameFillFunctionName,
				info.popFrameClasses
			] = definition;
			if (Extracts[info.predicateFunctionName](target))
				return info;
		}

		return null;
	},

	/*	Returns the target identifier: the definition ID (for definitions), or
		the original URL (for locally archived pages), or the relative url
		(for local links), or the full URL (for foreign links).
		*/
	targetIdentifier: (target) => {
		if (!target.href)
			return target.dataset.originalDefinitionId;

		return    target.dataset.urlOriginal 
			   || (target.hostname == location.hostname
			   	   ? target.pathname + target.hash
			   	   : target.href);
	},

	/*	Returns true if the two targets will spawn identical popups
		(that is, if they are of the same type, and have the same identifiers).
		*/
	targetsMatch: (targetA, targetB) => {
		return    Extracts.targetIdentifier(targetA) == Extracts.targetIdentifier(targetB)
			   && Extracts.targetTypeInfo(targetA).typeName == Extracts.targetTypeInfo(targetB).typeName;
	},

	/*	This function qualifies anchorlinks in transcluded content (i.e., other
		pages on the site, as well as extracts describing other pages on the 
		site), by rewriting their href attributes to include the path of the 
		target (link) that spawned the pop-frame that contains the transcluded 
		content.
		*/
    qualifyLinksInPopFrame: (popFrame) => {
		popFrame.querySelectorAll("a[href^='#']").forEach(anchorLink => {
		    anchorLink.pathname = popFrame.spawningTarget.pathname;
		});
    },

    nearestBlockElement: (element) => {
    	return element.closest("address, aside, blockquote, dd, div, dt, figure, footer, h1, h2, h3, h4, h5, h6, header, li, p, pre, section, table, tfoot, ol, ul");
    },

	/*	This function fills a pop-frame for a given target with content. It 
		returns true if the pop-frame successfully filled, false otherwise.
		*/
	fillPopFrame: (popFrame) => {
		GWLog("Extracts.fillPopFrame", "extracts.js", 2);

		let didFill = false;
		let target = popFrame.spawningTarget;
		let targetTypeInfo = Extracts.targetTypeInfo(target);
		if (targetTypeInfo && targetTypeInfo.popFrameFillFunctionName) {
			didFill = Extracts.popFrameProvider.setPopFrameContent(popFrame, Extracts[targetTypeInfo.popFrameFillFunctionName](target));
			if (targetTypeInfo.popFrameClasses)
				popFrame.classList.add(...(targetTypeInfo.popFrameClasses.split(" ")));
		}

		if (didFill) {
			return true;
		} else {
			GWLog(`Unable to fill pop-frame (${Extracts.targetIdentifier(target)} [${(targetTypeInfo ? targetTypeInfo.typeName : "UNDEFINED")}])!`, "extracts.js", 1);
			return false;
		}
	},

	popFrameHasLoaded: (popFrame) => {
		return !(popFrame.classList.contains("loading") || popFrame.classList.contains("loading-failed"));
	},

	//  Returns the contents of the title element for a pop-frame.
	titleForPopFrame: (popFrame) => {
		let target = popFrame.spawningTarget;

		let popFrameTitle;

		if (Extracts.isDefinition(target)) {
			popFrameTitle = Extracts.popFrameHasLoaded(popFrame)
							? `<span>${popFrame.querySelector(".data-field.title").textContent}</span>`
							: `<span>${target.dataset.originalDefinitionId}</span>`;
		} else {
			let popFrameTitleText;
			if (Extracts.isExtractLink(target)) {
					popFrameTitleText = Extracts.popFrameHasLoaded(popFrame)
										? popFrame.querySelector(".data-field.title").textContent
										: popFrameTitleText = target.pathname + target.hash;
			} else if (target.hostname == location.hostname) {
				if (target.dataset.urlOriginal) {
					popFrameTitleText = target.dataset.urlOriginal;
				} else if (target.pathname == location.pathname) {
					let nearestBlockElement = Extracts.nearestBlockElement(document.querySelector(decodeURIComponent(target.hash)));
					popFrameTitleText = nearestBlockElement.tagName == "SECTION"
										? nearestBlockElement.firstElementChild.textContent
										: target.hash;
				} else {
					popFrameTitleText = popFrame.classList.contains("external-page-embed")
										? target.pathname
										: (target.pathname + target.hash);
				}
			} else {
				popFrameTitleText = target.href;
			}
			popFrameTitleText = decodeURIComponent(popFrameTitleText);

			if (target.hash > "")
				popFrameTitleText = "&#x00a7; " + popFrameTitleText;

			//  For local-archive links, include archive link with original.
			if (target.dataset.urlOriginal) {
				popFrameTitle = `<a 
						class="popframe-title-link-archived"
						href="${target.href}"
						title="Open ${target.href} in a new window"
						target="_blank"
							>[ARCHIVED]</a>` +
					`<span class="separator">·</span>` +
					`<a 
						class="popframe-title-link"
						href="${target.dataset.urlOriginal}"
						title="Open ${target.dataset.urlOriginal} in a new window"
						target="_blank"
							>${popFrameTitleText}</a>`;
			} else {
				popFrameTitle = `<a 
					class="popframe-title-link"
					href="${target.href}"
					title="Open ${target.href} in a new window"
					target="_blank"
						>${popFrameTitleText}</a>`;
			}
		}

		return popFrameTitle;
	},

	/*	This function’s purpose is to allow for the transclusion of entire pages
		on the same website (displayed to the user in popups, or injected in 
		block flow as popins), and the (almost-)seamless handling of local links
		in such transcluded content in the same way that they’re handled in the 
		root document (i.e., the actual page loaded in the browser window). This
		permits us to have truly recursive popups with unlimited recursion depth
		and no loss of functionality.

		For any given target element, targetDocument() asks: to what local 
		document does the link refer?

		This may be either the root document, or an entire other page that was
		transcluded wholesale and embedded as a pop-frame (of class 
		‘external-page-embed’).
		*/
	targetDocument: (target) => {
		if (target.hostname != location.hostname)
			return null;

		if (target.pathname == location.pathname)
			return Extracts.rootDocument;

		if (Extracts.popFrameProvider == Popups) {
			let containingPopup = target.closest(".popup");
			if (!containingPopup)
				return null;

			let popupForTargetDocument = containingPopup.popupStack.find(popup => (   popup.classList.contains("external-page-embed") 
																				   && popup.spawningTarget.pathname == target.pathname));
			return popupForTargetDocument ? popupForTargetDocument.contentView : null;
		} else {
		
		}
	},

	/*	Returns the location (a URL object) of the document for a given target.
		*/
	locationForTarget: (target) => {
		return (target.hostname == location.hostname) ? new URL(location.href) : null;
	},

	/******************************************************************/
	/*  Helpers for targets with annotations (extracts or definitions).
		*/

	/*	Refresh (respawn or reload) a pop-frame for an annotated target after 
		its annotation (fragment) loads.
		*/
	refreshPopFrameAfterAnnotationLoads: (target) => {
		GWLog("Extracts.refreshPopFrameAfterAnnotationLoads", "extracts.js", 2);

		target.popFrame.classList.toggle("loading", true);

		/*	We set up an event handler for when the fragment loads, and respawn 
			the popup / re-inject the popin, after it spawns (if it 
			hasn’t de-spawned already, e.g. if the user moused out of the 
			target).
			*/
		GW.notificationCenter.addHandlerForEvent("Annotations.annotationDidLoad", target.refreshPopFrameWhenFragmentLoaded = (info) => {
			GWLog("refreshPopFrameWhenFragmentLoaded", "extracts.js", 2);

			//  If the pop-frame has despawned, don’t respawn it.
			if (!target.popFrame)
				return;

			if (Extracts.popFrameProvider == Popups) {
				Popups.spawnPopup(target);
			} else if (Extracts.popFrameProvider == Popins) {
				Extracts.fillPopFrame(target.popin);
				target.popin.classList.toggle("loading", false);

				Extracts.rewritePopinContent(target.popin);
			}
		}, { once: true, condition: (info) => info.identifier == Extracts.targetIdentifier(target) });

		//  Add handler for if the fragment load fails.
		GW.notificationCenter.addHandlerForEvent("Annotations.annotationLoadDidFail", target.updatePopFrameWhenFragmentLoadFails = (info) => {
			GWLog("updatePopFrameWhenFragmentLoadFails", "extracts.js", 2);

			//  If the pop-frame has despawned, don’t respawn it.
			if (!target.popFrame)
				return;

			target.popFrame.swapClasses([ "loading", "loading-failed" ], 1);
		}, { once: true, condition: (info) => info.identifier == Extracts.targetIdentifier(target) });
	},

	/***************************************************************************/
	/*  The target-testing and pop-frame-filling functions in this section
		come in sets, which define and implement classes of pop-frames 
		(whether those be popups, or popins, etc.). (These classes are things 
		like “a link that has a statically generated extract provided for it”,
		“a link to a locally archived web page”, “an anchorlink to a section of
		the current page”, and so on.)

		Each set contains a testing function, which is called by 
		testTarget() to determine if the target (link, etc.) is eligible for 
		processing, and is also called by fillPopFrame() to find the 
		appropriate filling function for a pop-frame spawned by a given 
		target. The testing function takes a target element and examines its
		href or other properties, and returns true if the target is a member of
		that class of targets, false otherwise.

		Each set also contains the corresponding filling function, which
		is called by fillPopFrame() (chosen on the basis of the return values 
		of the testing functions, and the specified order in which they’re 
		called). The filling function takes a target element and returns a 
		string which comprises the HTML contents that should be injected into
		the pop-frame spawned by the given target.
		*/

	//  Summaries of links to elsewhere.
	isExtractLink: (target) => {
		return target.classList.contains("docMetadata");
	},

    //  Definitions.
    isDefinition: (target) => {
        return target.classList.contains("defnMetadata");
    },

	//  Either an extract or a definition.
	annotationForTarget: (target) => {
		GWLog("Extracts.annotationForTarget", "extracts.js", 2);

		let annotationIdentifier = Extracts.targetIdentifier(target);

		if (Annotations.annotationForIdentifier(annotationIdentifier) == null) {
			Extracts.refreshPopFrameAfterAnnotationLoads(target);
			return `&nbsp;`;
		} else if (Annotations.annotationForIdentifier(annotationIdentifier) == "LOADING_FAILED") {
			target.popFrame.classList.add("loading-failed");
			return `&nbsp;`;
		}

		let referenceData = Annotations.referenceDataForAnnotationIdentifier(annotationIdentifier);

		if (Extracts.isDefinition(target)) {
			//  The fully constructed definition pop-frame contents.
			return `<p class="data-field title">${referenceData.titleHTML}</p>` 
				 + `<p class="data-field author-plus-date">${referenceData.authorHTML}${referenceData.dateHTML}</p>` 
				 + `<div class="data-field annotation-abstract">${referenceData.abstractHTML}</div>`;
		} else { // if (Extracts.isExtractLink(target))
			//  Link to original URL (for archive links).
			let originalLinkHTML = "";
			if (   referenceData.element.dataset.urlOriginal != undefined 
				&& referenceData.element.dataset.urlOriginal != target.href) {
				originalLinkHTML = `<span class="originalURL">[<a 
								title="Link to original URL for ‘${referenceData.titleText}’" 
								href="${referenceData.element.dataset.urlOriginal}"
								target="_new" 
								alt="Original URL for this archived link; may be broken."
									>original</a>]</span>`;
			}

			//  Extract title/link.
			let titleLinkClass = (originalLinkHTML > "" ? `title-link local-archive-link` : `title-link`);
			let titleLinkHTML = `<a 
									class="${titleLinkClass}" 
									target="_new" 
									href="${target.href}" 
									title="Open ${target.href} in a new window"
										>${referenceData.titleHTML}</a>`;

			//  The fully constructed extract pop-frame contents.
			let abstractSpecialClass = ``;
			if (Annotations.isWikipediaLink(annotationIdentifier))
				abstractSpecialClass = "wikipedia-entry";
			return `<p class="data-field title">${originalLinkHTML}${titleLinkHTML}</p>` 
				 + `<p class="data-field author-plus-date">${referenceData.authorHTML}${referenceData.dateHTML}</p>` 
				 + `<div class="data-field annotation-abstract ${abstractSpecialClass}">${referenceData.abstractHTML}</div>`;
		}
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
        if (!target.href) return false;

        if ([ "www.youtube.com", "youtube.com", "youtu.be" ].includes(target.hostname)) {
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
        return `<iframe src="${videoEmbedURL}" srcdoc="${srcdocStyles}${srcdocHTML}" frameborder="0" allowfullscreen sandbox="allow-scripts allow-same-origin"></iframe>`;
    },

    //  Citations.
    isCitation: (target) => {
		return target.classList.contains("footnote-ref");
	},

	//  Context surrounding a citation (displayed on footnote-back links).
    isCitationBackLink: (target) => {
	    return target.classList.contains("footnote-back");
    },

	//  Local links (to sections of the current page, or other site pages).
    isLocalPageLink: (target) => {
		if (  !target.href
			|| Extracts.isExtractLink(target))
			return false;

		if (target.hostname != location.hostname)
			return false;

		/*  If it has a period in it, it’s not a page, but is something else,
			like a file of some sort, or a locally archived document (accounted
			for in the other test functions, if need be).
			*/
		if (target.pathname.match(/\./))
			return false;

		return (   target.pathname != location.pathname
				|| target.hash > "");
	},

    localTranscludeForTarget: (target) => {
		GWLog("Extracts.localTranscludeForTarget", "extracts.js", 2);

		/*	Check to see if the target location matches an already-displayed 
			page (which can be the root page of the window).
			*/
		let fullTargetDocument = Extracts.targetDocument(target);
		if (fullTargetDocument) {
			/*  If it does, display the section. (We know it must be an 
				anchorlink because if it were not, the target would not be
				active.)
				*/
			return Extracts.sectionEmbedForTarget(target, fullTargetDocument);
		} else {
			//  Otherwise, display the entire linked page.
			target.popFrame.classList.add("external-page-embed");
			return Extracts.externalPageEmbedForTarget(target);
		}
	},

	//  Sections of the current page.
    sectionEmbedForTarget: (target, fullTargetDocument) => {
		GWLog("Extracts.sectionEmbedForTarget", "extracts.js", 2);

        let nearestBlockElement = Extracts.nearestBlockElement(fullTargetDocument.querySelector(decodeURIComponent(target.hash)));

		//  Unwrap sections and {foot|side}notes from their containers.
		if (nearestBlockElement.tagName == "SECTION") {
			return nearestBlockElement.innerHTML;
		} else if (Extracts.isCitation(target)) {
			if (target.hash.startsWith("#sn")) {
				return nearestBlockElement.querySelector(".sidenote-inner-wrapper").innerHTML;
			} else {
				return nearestBlockElement.innerHTML;
			}
		} else {
			return nearestBlockElement.outerHTML;
		}
    },

	//  TOC links.
	isTOCLink: (target) => {
		return (target.closest("#TOC") != null);
	},

	//  Other site pages.
    cachedPages: { },
    cachedPageTitles: { },
    externalPageEmbedForTarget: (target) => {
		GWLog("Extracts.externalPageEmbedForTarget", "extracts.js", 2);

		let fillPopFrame = (markdownBody) => {
			GWLog("Filling pop-frame...", "extracts.js", 2);

			Extracts.popFrameProvider.setPopFrameContent(target.popFrame, markdownBody.innerHTML);

			//  Give the pop-frame an identifying class.
			target.popFrame.classList.toggle("page-" + target.pathname.substring(1), true);

			//  Set the pop-frame title.
			if (target.popFrame.titleBar)
				target.popFrame.titleBar.querySelector(".popframe-title-link").innerHTML = Extracts.cachedPageTitles[target.pathname];

			//  First, qualify internal links in the pop-frame.
			Extracts.qualifyLinksInPopFrame(target.popFrame);

			/*  Then, trigger the rewrite pass by firing the requisite event.
				(This will also activate spawning targets in the embedded page.)
				*/
			GW.notificationCenter.fireEvent("GW.contentDidLoad", {
				source: "Extracts.externalPageEmbedForTarget",
				document: target.popFrame.contentView, 
				isMainDocument: false,
				needsRewrite: true, 
				clickable: false, 
				collapseAllowed: false, 
				isCollapseBlock: false,
				isFullPage: true,
				location: new URL(target.href),
				fullWidthPossible: false
			});

			//  Scroll to the target.
			if (target.hash > "")
				Extracts.popFrameProvider.scrollElementIntoViewInPopFrame(target.popFrame.querySelector(decodeURIComponent(target.hash)));
		};

		if (Extracts.cachedPages[target.pathname]) {
			target.popFrame.classList.toggle("loading", true);
			requestAnimationFrame(() => {
				target.popFrame.classList.toggle("loading", false);

				fillPopFrame(Extracts.cachedPages[target.pathname]);
			});
		} else {
			target.popFrame.classList.toggle("loading", true);
			doAjax({
				location: target.href,
				onSuccess: (event) => {
					if (!target.popFrame)
						return;

					target.popFrame.classList.toggle("loading", false);

					//  Inject the whole page into the pop-frame at first.
					Extracts.popFrameProvider.setPopFrameContent(target.popFrame, event.target.responseText);

					//  The content is the page body plus the metadata block.
					Extracts.cachedPages[target.pathname] = target.popFrame.querySelector("#markdownBody");
					let pageMetadata = target.popFrame.querySelector("#page-metadata");
					if (pageMetadata)
						Extracts.cachedPages[target.pathname].insertBefore(pageMetadata, Extracts.cachedPages[target.pathname].firstElementChild);

					//  Get the page title.
					Extracts.cachedPageTitles[target.pathname] = target.popFrame.querySelector("title").innerHTML.match(Extracts.pageTitleRegexp)[1];

					//  Inject the content into the pop-frame.
					fillPopFrame(Extracts.cachedPages[target.pathname]);
				},
				onFailure: (event) => {
					if (!target.popFrame)
						return;

					target.popFrame.swapClasses([ "loading", "loading-failed" ], 1);
				}
			});
		}

		return `&nbsp;`;
    },

	//  Other websites.
	isForeignSiteLink: (target) => {
		if (  !target.href
			|| Extracts.isExtractLink(target)) return false;

		return  (   Extracts.qualifyingForeignDomains.includes(target.hostname)
				 || Extracts.qualifyingForeignDomains.findIndex(domainPattern => (domainPattern instanceof RegExp && domainPattern.test(target.hostname) == true)) != -1)
			&& !Extracts.blacklistedForeignDomains.includes(target.hostname);
	},
	foreignSiteForTarget: (target) => {
		let url = new URL(target.href);

		if ([ "www.lesswrong.com", "lesswrong.com", "www.greaterwrong.com", "greaterwrong.com" ].includes(url.hostname)) {
			url.protocol = "https:";
			url.hostname = "www.greaterwrong.com";
			url.search = "format=preview&theme=classic";
		} else if (/(.+?)\.wikipedia\.org/.test(url.hostname) == true) {
			url.protocol = "https:";
			url.hostname = url.hostname.replace(/(.+?)(?:\.m)?\.wikipedia\.org/, "$1.m.wikipedia.org");
			if (!url.hash)
				url.hash = "#bodyContent";
		} else {
			url.protocol = "https:";
		}

		return `<iframe src="${url.href}" frameborder="0" sandbox></iframe>`;
	},

	//  Locally hosted videos.
	isLocalVideoLink: (target) => {
		if (  !target.href
			|| target.hostname != location.hostname)
			return false;

		let videoFileURLRegExp = new RegExp(
			  '(' 
			+ Extracts.videoFileExtensions.map(ext => `\\.${ext}`).join("|") 
			+ ')$'
		, 'i');
		return (target.pathname.match(videoFileURLRegExp) != null);
	},
    localVideoForTarget: (target) => {
		GWLog("Extracts.localVideoForTarget", "extracts.js", 2);

// 		let width = target.dataset.imageWidth || 0;
// 		let height = target.dataset.imageHeight || 0;
// 
// 		if (width > Extracts.imageMaxWidth) {
// 			height *= Extracts.imageMaxWidth / width;
// 			width = Extracts.imageMaxWidth;
// 		}
// 		if (height > Extracts.imageMaxHeight) {
// 			width *= Extracts.imageMaxHeight / height;
// 			height = Extracts.imageMaxHeight;
// 		}
// 
// 		let styles = ``;
// 		if (width > 0 && height > 0) {
// 			styles = `width="${width}" height="${height}" style="width: ${width}px; height: ${height}px;"`;
// 		}

        //  Note that we pass in the original image-link’s classes - this is good for classes like ‘invertible’.
//         return `<img ${styles} class="${target.classList}" src="${target.href}" loading="lazy">`;
        return `<video controls="controls" preload="none">` + 
        	`<source src="${target.href}">` + 
			`</video>`;
    },

	//  Locally hosted images.
    isLocalImageLink: (target) => {
		if (  !target.href
			|| target.hostname != location.hostname)
			return false;

		let imageFileURLRegExp = new RegExp(
			  '(' 
			+ Extracts.imageFileExtensions.map(ext => `\\.${ext}`).join("|") 
			+ ')$'
		, 'i');
		return (target.pathname.match(imageFileURLRegExp) != null);
    },
    localImageForTarget: (target) => {
		GWLog("Extracts.localImageForTarget", "extracts.js", 2);

		let width = target.dataset.imageWidth || 0;
		let height = target.dataset.imageHeight || 0;

		if (width > Extracts.imageMaxWidth) {
			height *= Extracts.imageMaxWidth / width;
			width = Extracts.imageMaxWidth;
		}
		if (height > Extracts.imageMaxHeight) {
			width *= Extracts.imageMaxHeight / height;
			height = Extracts.imageMaxHeight;
		}

		let styles = ``;
		if (width > 0 && height > 0)
			styles = `width="${width}" height="${height}" style="width: ${width}px; height: ${height}px;"`;

        //  Note that we pass in the original image-link’s classes - this is good for classes like ‘invertible’.
        return `<img ${styles} class="${target.classList}" src="${target.href}" loading="lazy">`;
    },

	//  Locally hosted documents (html, pdf, etc.).
    isLocalDocumentLink: (target) => {
		if (  !target.href
			|| target.hostname != location.hostname
			|| Extracts.isExtractLink(target))
			return false;

	    return (   target.pathname.startsWith("/docs/www/")
	            || (   target.pathname.startsWith("/docs/")
	                && target.pathname.match(/\.(html|pdf)$/i) != null));
    },
    localDocumentForTarget: (target) => {
		GWLog("Extracts.localDocumentForTarget", "extracts.js", 2);

		if (target.href.match(/\.pdf(#|$)/) != null) {
			let data = target.href + (target.href.includes("#") ? "&" : "#") + "view=FitH";
			return `<object data="${data}"></object>`;
		} else {
			return `<iframe src="${target.href}" frameborder="0" sandbox="allow-same-origin" referrerpolicy="same-origin"></iframe>`;
		}
    },

	//  Locally hosted code files (css, js, hs, etc.).
    isLocalCodeFileLink: (target) => {
		if (  !target.href
			|| target.hostname != location.hostname
			|| Extracts.isExtractLink(target))
			return false;

		let codeFileURLRegExp = new RegExp(
			  '(' 
			+ Extracts.codeFileExtensions.map(ext => `\\.${ext}`).join("|") 
			+ ')$'
		, 'i');
		return (target.pathname.match(codeFileURLRegExp) != null);
    },
	/*  We first try to retrieve a syntax-highlighted version of the given code 
		file, stored on the server as an HTML fragment. If present, we embed 
		that. If there’s no such fragment, then we just embed the contents of 
		the actual code file, in a <pre>-wrapped <code> element.
		*/
    localCodeFileForTarget: (target) => {
		GWLog("Extracts.localCodeFileForTarget", "extracts.js", 2);

		let setPopFrameContent = Popups.setPopFrameContent;

		target.popFrame.classList.toggle("loading", true);
		doAjax({
			location: target.href + ".html",
			onSuccess: (event) => {
				if (!target.popFrame)
					return;

				target.popFrame.classList.toggle("loading", false);
				setPopFrameContent(target.popFrame, event.target.responseText);
			},
			onFailure: (event) => {
				doAjax({
					location: target.href,
					onSuccess: (event) => {
						if (!target.popFrame)
							return;

						target.popFrame.classList.toggle("loading", false);

						let htmlEncodedResponse = event.target.responseText.replace(/[<>]/g, c => ('&#' + c.charCodeAt(0) + ';'));
						let lines = htmlEncodedResponse.split("\n");
						htmlEncodedResponse = lines.map(line => `<span class="line">${(line || "&nbsp;")}</span>`).join("\n");

						setPopFrameContent(target.popFrame, `<pre class="raw-code"><code>${htmlEncodedResponse}</code></pre>`);
					},
					onFailure: (event) => {
						if (!target.popFrame)
							return;

						target.popFrame.swapClasses([ "loading", "loading-failed" ], 1);
					}
				});
			}
		});

		return `&nbsp;`;
    },	

	/**********/
	/*	Popins.
		*/

	/*	Called by popins.js just before injecting the popin. This is our chance 
		to fill the popin with content, and rewrite that content in whatever 
		ways necessary. After this function exits, the popin will appear on the 
		screen.
		*/
    preparePopin: (popin) => {
		GWLog("Extracts.preparePopin", "extracts.js", 2);

		let target = popin.spawningTarget;

		//  Import the class(es) of the target.
		popin.classList.add(...target.classList);
		//  We then remove some of the imported classes.
		popin.classList.remove("has-annotation", "has-content", "link-self", "link-local", "spawns-popin");

		//  Add ‘markdownBody’ class.
		popin.contentView.classList.add("markdownBody");

		//  Attempt to load annotation, if need be.
		if (   Extracts.isExtractLink(target)
			|| Extracts.isDefinition(target)) {
			let annotationIdentifier = Extracts.targetIdentifier(target);
			if (!Annotations.cachedAnnotationExists(annotationIdentifier))
				Annotations.loadAnnotation(annotationIdentifier);
		}

		//  Attempt to fill the popin.
		if (Extracts.fillPopFrame(popin) == false)
			return null;

		//  Add popin title bar contents.
		let popinTitle = Extracts.titleForPopFrame(popin);
		if (popinTitle) {
			//  Add the title.
			popinTitle = `<span class="popframe-title">${popinTitle}</span>`;
			popin.titleBarContents.push(popinTitle);

			//  Add the close button.
			popin.titleBarContents.push(Popins.titleBarComponents.closeButton());

			//  Add the options button.
			if (Extracts.popinOptionsEnabled) {
				let showPopinOptionsDialogButton = Popins.titleBarComponents.optionsButton();
				showPopinOptionsDialogButton.addActivateEvent((event) => {
					event.stopPropagation();

					Extracts.showPopinOptionsDialog();
				});
				showPopinOptionsDialogButton.title = "Show popin options (enable/disable popins)";
				showPopinOptionsDialogButton.classList.add("show-popin-options-dialog");
				popup.titleBarContents.push(showPopinOptionsDialogButton);
			}
		}

		/*  If we’re waiting for content to be loaded into the popin 
			asynchronously, then there’s no need to do rewrites for now.
			*/
		if (Extracts.popFrameHasLoaded(popin))
			Extracts.rewritePopinContent(popin);

		return popin;
    },
     
    rewritePopinContent: (popin) => {
		GWLog("Extracts.rewritePopinContent", "extracts.js", 2);

		let target = popin.spawningTarget;

		//  Update the title.
		if (popin.titleBar)
			popin.titleBar.querySelector(".popframe-title").innerHTML = Extracts.titleForPopFrame(popin);

		//	Mark Wikipedia entries.
		if (   Extracts.isExtractLink(target)
			&& popin.querySelector(".annotation-abstract").classList.contains("wikipedia-entry"))
			popin.contentView.classList.add("wikipedia-entry");

		//  Special handling for image popins.
		if (Extracts.isLocalImageLink(target)) {
			let image = popin.querySelector("img");

			//  Remove extraneous classes from images in image popins.
			image.classList.remove("has-annotation", "has-content", "link-self", "link-local", "spawns-popin");
		}

		//  Rectify margin note style.
		popin.querySelectorAll(".marginnote").forEach(marginNote => {
			marginNote.swapClasses([ "inline", "sidenote" ], 0);
		});

		//  Qualify internal links in extracts.
		if (   Extracts.isExtractLink(target) 
			&& target.hostname == location.hostname) {
			Extracts.qualifyLinksInPopFrame(target.popFrame);
		}

		/*  If the popin is of a type that contains local HTML content of some
			sort, then fire a contentDidLoad event to trigger any necessary
			rewrites.
			*/
		if (   Extracts.isExtractLink(target)
			|| Extracts.isDefinition(target)
			|| Extracts.isLocalPageLink(target)
			|| Extracts.isCitation(target)
			) {
			GW.notificationCenter.fireEvent("GW.contentDidLoad", {
				source: "Extracts.preparePopin",
				document: popin.contentView,
				isMainDocument: false,
				needsRewrite: false, 
				clickable: false, 
				collapseAllowed: false, 
				isCollapseBlock: false,
				isFullPage: false,
				location: Extracts.locationForTarget(target),
				fullWidthPossible: false
			});
		}

		//  Loading spinners.
		if (   Extracts.isLocalDocumentLink(target)
			|| Extracts.isForeignSiteLink(target)
			|| Extracts.isLocalImageLink(target)
			|| Extracts.isLocalVideoLink(target)
			) {
			popin.classList.toggle("loading", true);

			//  When loading ends (in success or failure)...
			let objectOfSomeSort = popin.querySelector("iframe, object, img, video");
			if (objectOfSomeSort.tagName == "IFRAME") {
				//  Iframes do not fire ‘error’ on server error.
				objectOfSomeSort.onload = (event) => {
					popin.classList.toggle("loading", false);

					/*	We do this for local documents only. Cross-origin 
						protections prevent us from accessing the content of
						an iframe with a foreign site, so we do nothing special
						and simply let the foreign site’s server show its usual
						404 page (or whatever) if the linked page is not found.
						*/
					if (   target.hostname == location.hostname
						&& Extracts.server404PageTitles.includes(objectOfSomeSort.contentDocument.title))
						popin.classList.toggle("loading-failed", true);
				};
			} else {
				//  Objects & images fire ‘error’ on server error or load fail.
				objectOfSomeSort.onload = (event) => {
					popin.classList.toggle("loading", false);
				};
			}
			/*  We set an ‘error’ handler for *all* types of entity, even 
				iframes, just in case.
				*/
			objectOfSomeSort.onerror = (event) => {
				popin.swapClasses([ "loading", "loading-failed" ], 1);
			};
		}
    },
   
	/**********/
	/*	Popups.
		*/

	popupsEnabled: () => {
		return (localStorage.getItem("extract-popups-disabled") != "true");
	},

	spawnedPopupMatchingTarget: (target) => {
		let parentPopup = target.closest(".popup");
		return Popups.allSpawnedPopups().find(popup => 
				   Extracts.targetsMatch(target, popup.spawningTarget) 
				&& Popups.popupIsEphemeral(popup)
		);
	},

	//  Called by popups.js when adding a target.
	preparePopupTarget: (target) => {
		//  Remove the title attribute (saving it first);
		if (target.title) {
			target.dataset.attributeTitle = target.title;
			target.removeAttribute("title");
		}

		//  For special positioning by Popups.js.
		target.preferSidePositioning = () => {
			return (   target.closest("#sidebar, li") != null
					&& target.closest(".columns") == null);
		};
	},

	/*	Called by popups.js just before spawning (injecting and positioning) the
		popup. This is our chance to fill the popup with content, and rewrite
		that content in whatever ways necessary. After this function exits, the
		popup will appear on the screen.
		*/
    preparePopup: (popup) => {
		GWLog("Extracts.preparePopup", "extracts.js", 2);

		let target = popup.spawningTarget;

		/*  If a popup already exists that matches the target, do not spawn a
			new popup; just use the existing popup.
			*/
		let existingPopup = Extracts.spawnedPopupMatchingTarget(target);
		if (existingPopup) {
			Popups.detachPopupFromTarget(existingPopup);
			existingPopup.spawningTarget = target;
			return existingPopup;
		}

		//  Import the class(es) of the target.
		popup.classList.add(...target.classList);
		//  We then remove some of the imported classes.
		popup.classList.remove("has-annotation", "has-content", "link-self", "link-local", "spawns-popup");

		//  Add ‘markdownBody’ class.
		popup.contentView.classList.add("markdownBody");

		/*  Situationally prevent spawning of citation and citation-context 
			links: do not spawn footnote popup if the {side|foot}note it points 
			to is visible, and do not spawn citation context popup if citation 
			is visible.
			*/
		if (   (   Extracts.isCitation(target) 
				&& Array.from(allNotesForCitation(target)).findIndex(note => Popups.isVisible(note)) != -1)
			|| (   Extracts.isCitationBackLink(target) 
				&& Popups.isVisible(Extracts.targetDocument(target).querySelector(decodeURIComponent(target.hash)))))
			return null;

		//  Various special handling.
		if (Extracts.isTOCLink(target)) {
			//  Designate section links spawned by the TOC (for special styling).
			popup.classList.add("toc-section");
		} else if (Extracts.isCitation(target)) {
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
		}

		//  Attempt to fill the popup.
		if (Extracts.fillPopFrame(popup) == false)
			return null;

		//  Add popup title bar contents.
		let popupTitle = Extracts.titleForPopFrame(popup);
		if (popupTitle) {
			//  Zero out existing title bar contents.
			popup.titleBarContents = [ ];

			//  Add the close, zoom, and pin buttons.
			popup.titleBarContents.push(
				Popups.titleBarComponents.closeButton(),
				Popups.titleBarComponents.zoomButton().enableSubmenu(),
				Popups.titleBarComponents.pinButton()
			);

			//  Add the title.
			popupTitle = `<span class="popframe-title">${popupTitle}</span>`;
			popup.titleBarContents.push(popupTitle);

			//  Add the options button.
			if (Extracts.popupOptionsEnabled) {
				let showPopupOptionsDialogButton = Popups.titleBarComponents.optionsButton();

				showPopupOptionsDialogButton.addActivateEvent((event) => {
					event.stopPropagation();

					Extracts.showPopupOptionsDialog();
				});
				showPopupOptionsDialogButton.title = "Show popup options (enable/disable popups)";
				showPopupOptionsDialogButton.classList.add("show-popup-options-dialog");

				popup.titleBarContents.push(showPopupOptionsDialogButton);
			}
		}

		//  Some kinds of popups get an alternate form of title bar.
		if (   Extracts.isLocalImageLink(target)
			|| Extracts.isLocalVideoLink(target)
			|| Extracts.isCitation(target)
			|| Extracts.isCitationBackLink(target))
			popup.classList.add("mini-title-bar");	

		/*  If we’re waiting for content to be loaded into the popup 
			asynchronously, then there’s no need to do rewrites for now.
			*/
		if (Extracts.popFrameHasLoaded(popup)) 
			Extracts.rewritePopupContent(popup);

		return popup;
    },
    
    rewritePopupContent: (popup) => {
		GWLog("Extracts.rewritePopupContent", "extracts.js", 2);

		let target = popup.spawningTarget;

		//	Mark Wikipedia entries.
		if (   Extracts.isExtractLink(target)
			&& popup.querySelector(".annotation-abstract").classList.contains("wikipedia-entry"))
			popup.contentView.classList.add("wikipedia-entry");

		//  Highlight citation in popup.
		if (Extracts.isCitationBackLink(target)) {
			/*  Remove the .targeted class from a targeted citation (if any)
				inside the popup (to prevent confusion with the citation that
				the spawning link points to, which will be highlighted).
				*/
			popup.querySelectorAll(".footnote-ref.targeted").forEach(targetedCitation => {
				targetedCitation.classList.remove("targeted");
			});

			//  In the popup, the citation for which context is being shown.
			let citationInPopup = popup.querySelector(decodeURIComponent(target.hash));

			//  Highlight the citation.
			citationInPopup.classList.add("targeted");

			//  Scroll to the citation.
			requestAnimationFrame(() => {
				Extracts.popFrameProvider.scrollElementIntoViewInPopFrame(citationInPopup, popup);
			});
		}

		//  Special handling for image popups.
		if (Extracts.isLocalImageLink(target)) {
			//  Remove extraneous classes from images in image popups.
			popup.querySelector("img").classList.remove("has-annotation", "has-content", "link-self", "link-local", "spawns-popup");

			if (popup.querySelector("img[width][height]"))
				popup.classList.add("dimensions-specified");
		}

		//  Ensure no reflow due to figures.
		popup.querySelectorAll("figure[class^='float-'] img[width]").forEach(img => {
			if (img.style.width <= "") {
				img.style.width = img.getAttribute("width") + "px";
				img.style.maxHeight = "unset";
			}
		});

		//  Allow for floated figures at the start of abstract.
		if (   Extracts.isExtractLink(target)
			|| Extracts.isDefinition(target)) {
			let initialFigure = popup.querySelector(".annotation-abstract > figure.float-right:first-child");
			if (initialFigure) {
				popup.contentView.insertBefore(initialFigure, popup.contentView.firstElementChild);
			}
		}

		//  Rectify margin note style.
		popup.querySelectorAll(".marginnote").forEach(marginNote => {
			marginNote.swapClasses([ "inline", "sidenote" ], 0);
		});

		//  Qualify internal links in extracts.
		if (   Extracts.isExtractLink(target) 
			&& target.hostname == location.hostname) {
			Extracts.qualifyLinksInPopFrame(target.popFrame);
		}

		/*  If the popup is of a type that contains local HTML content of some
			sort, then fire a contentDidLoad event to trigger any necessary
			rewrites.
			*/
		if (   Extracts.isExtractLink(target)
			|| Extracts.isDefinition(target)
			|| Extracts.isLocalPageLink(target)
			|| Extracts.isCitation(target)
			|| Extracts.isCitationBackLink(target)
			) {
			GW.notificationCenter.fireEvent("GW.contentDidLoad", {
				source: "Extracts.preparePopup",
				document: popup.contentView,
				isMainDocument: false,
				needsRewrite: false, 
				clickable: false, 
				collapseAllowed: false, 
				isCollapseBlock: false,
				isFullPage: false,
				location: Extracts.locationForTarget(target),
				fullWidthPossible: false
			});
		}

		//  For locally archived web pages, set title of popup from page title.
		if (Extracts.isLocalDocumentLink(target)) {
			let iframe = popup.querySelector("iframe");
			if (iframe) {
				iframe.addEventListener("load", (event) => {
					popup.titleBar.querySelector(".popframe-title-link").innerHTML = iframe.contentDocument.title;
				});
			}
		}

		//  Loading spinners.
		if (   Extracts.isLocalDocumentLink(target)
			|| Extracts.isForeignSiteLink(target)
			|| Extracts.isLocalImageLink(target)
			|| Extracts.isLocalVideoLink(target)
			) {
			popup.classList.toggle("loading", true);

			//  When loading ends (in success or failure)...
			let objectOfSomeSort = popup.querySelector("iframe, object, img, video");
			if (objectOfSomeSort.tagName == "IFRAME") {
				//  Iframes do not fire ‘error’ on server error.
				objectOfSomeSort.onload = (event) => {
					popup.classList.toggle("loading", false);

					/*	We do this for local documents only. Cross-origin 
						protections prevent us from accessing the content of
						an iframe with a foreign site, so we do nothing special
						and simply let the foreign site’s server show its usual
						404 page (or whatever) if the linked page is not found.
						*/
					if (   target.hostname == location.hostname
						&& Extracts.server404PageTitles.includes(objectOfSomeSort.contentDocument.title)) {
						popup.classList.toggle("loading-failed", true);
					}
				};
			} else {
				//  Objects & images fire ‘error’ on server error or load fail.
				objectOfSomeSort.onload = (event) => {
					popup.classList.toggle("loading", false);
				};
			}
			/*  We set an ‘error’ handler for *all* types of entity, even 
				iframes, just in case.
				*/
			objectOfSomeSort.onerror = (event) => {
				popup.swapClasses([ "loading", "loading-failed" ], 1);
			};
		}
    }
};

GW.notificationCenter.fireEvent("Extracts.didLoad");

//  Set pop-frame type (mode) - popups or popins.
let mobileMode = (localStorage.getItem("extracts-force-popins") == "true") || GW.isMobile();
Extracts.popFrameProviderName = mobileMode ? "Popins" : "Popups";
GWLog(`${(mobileMode ? "Mobile" : "Non-mobile")} client detected. Activating ${(mobileMode ? "popins" : "popups")}.`, "extracts.js", 1);

doSetup = () => {
	//  Prevent null references.
	Popups = window["Popups"] || { };
	Popins = window["Popins"] || { };

	Extracts.setup();
};
if (window[Extracts.popFrameProviderName]) {
	doSetup();
} else {
	GW.notificationCenter.addHandlerForEvent(Extracts.popFrameProviderName + ".didLoad", () => {
		doSetup();
	}, { once: true });
}
