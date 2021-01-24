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
	annotationsBasePathname: "/metadata/annotations/",

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
			let targetTypeInfo = Extracts.typeInfo(Extracts.targetType(target));
			if (targetTypeInfo) {
				if (targetTypeInfo.classes)
					target.classList.add(...(targetTypeInfo.classes.split(" ")));
				return true;
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

	imageMaxWidth: 634.0,
	imageMaxHeight: 474.0,

	server404PageTitles: [
		"404 Not Found"
	],

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
			return;
		} else {
			//  Remove “popups disabled” icon/button, if present.
			if (Extracts.popupOptionsEnabled) {
				Extracts.removePopupsDisabledShowPopupOptionsDialogButton();
			}

			//  Unbind event listeners, restore targets, and remove popups.
			document.querySelectorAll(Extracts.contentContainersSelector).forEach(container => {
				Popups.removeTargetsWithin(container, Extracts.targets, restoreTarget);
			});

			//  Remove content load event handlers.
			GW.notificationCenter.removeHandlerForEvent("GW.contentDidLoad", Extracts.processTargetsOnContentLoad);
			GW.notificationCenter.removeHandlerForEvent("GW.contentDidLoad", Extracts.setUpAnnotationLoadEvent);
			GW.notificationCenter.removeHandlerForEvent("GW.contentDidLoad", Extracts.signalAnnotationLoaded);
			GW.notificationCenter.removeHandlerForEvent("GW.contentDidLoad", Extracts.signalAnnotationLoadFailed);
		}

		GW.notificationCenter.fireEvent("Extracts.cleanupDidComplete");
    },
    addTargetsWithin: (container) => {
 		GWLog("Extracts.addTargetsWithin", "extracts.js", 1);

		if (GW.isMobile()) {
    		return;
    	} else {
			//  Target prepare function.
			let prepareTarget = (target) => {
				//  Remove the title attribute.
				target.removeAttribute("title");

				if (Extracts.isTOCLink(target))
					target.classList.remove("has-content");

				//  For special positioning by Popups.js.
				target.preferSidePositioning = () => {
					return (   target.closest("#sidebar, li") != null
							&& target.closest(".columns") == null);
				};
			};

    		Popups.addTargetsWithin(container, Extracts.targets, Extracts.preparePopup, prepareTarget);
    	}
    },
    setup: () => {
		GWLog("Extracts.setup", "extracts.js", 1);

        if (GW.isMobile()) {
			return;
        } else {
            GWLog("Non-mobile client detected. Activating popups.", "extracts.js", 1);

			if (Extracts.popupOptionsEnabled) {
				if (localStorage.getItem("extract-popups-disabled") == "true") {
					//  Inject “popups disabled” icon/button.
					Extracts.injectPopupsDisabledShowPopupOptionsDialogButton();
					return;
				}
			}

			/*  Add handler to set up targets in loaded content (including 
				newly-spawned popups; this allows for popup recursion).
				*/
			GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", Extracts.processTargetsOnContentLoad = (info) => {
				GWLog("Extracts.processTargetsOnContentLoad", "extracts.js", 2);

				if (info.document.closest(Extracts.contentContainersSelector)) {
					Extracts.addTargetsWithin(info.document);
				} else {
					info.document.querySelectorAll(Extracts.contentContainersSelector).forEach(container => {
						Extracts.addTargetsWithin(container);
					});
				}
			}, { phase: "eventListeners" });

			//  Inject the staging area for annotations.
			document.body.insertAdjacentHTML("beforeend", `<div id="annotations-workspace" style="display:none;"></div>`);
			let annotationsWorkspace = document.querySelector("#annotations-workspace");

			/*  Add handler to add hover event listeners to annotated targets,
				to load annotations (fragments).
				*/
			GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", Extracts.setUpAnnotationLoadEvent = (info) => {
				GWLog("Extracts.setUpAnnotationLoadEvent", "extracts.js", 2);

				//  Get all the annotated targets in the document.
				let allAnnotatedTargetsInDocument = Array.from(info.document.querySelectorAll(Extracts.annotatedTargetSelectors.join(", ")));

				//  Add hover event listeners to all the annotated targets.
				allAnnotatedTargetsInDocument.forEach(annotatedTarget => {
					annotatedTarget.addEventListener("mouseenter", annotatedTarget.annotationLoad_mouseEnter = (event) => {
						/*  Do nothing if the annotation for the target is 
							already loaded.
							*/
						let annotationIdentifier = Extracts.targetIdentifier(annotatedTarget);

						let cachedAnnotation = Extracts.cachedAnnotationReferenceEntries[annotationIdentifier];
						if (cachedAnnotation && cachedAnnotation != "LOADING_FAILED") return;

						let annotationURL = new URL("https://" + location.hostname + Extracts.annotationsBasePathname 
													+ fixedEncodeURIComponent(fixedEncodeURIComponent(annotationIdentifier)) + ".html");

						/*  On hover, start a timer, duration of one-half the 
							popup trigger delay...
							*/
						annotatedTarget.annotationLoadTimer = setTimeout(Extracts.loadAnnotation = () => {
							GWLog("Extracts.loadAnnotation", "extracts.js", 2);

							/*  ... to load the annotation.
								*/
							doAjax({
								location: annotationURL.href,
								onSuccess: (event) => {
									document.querySelector("#annotations-workspace").insertAdjacentHTML("beforeend", 
										`<div class="annotation">${event.target.responseText}</div>`);
									GW.notificationCenter.fireEvent("GW.contentDidLoad", { 
										source: "Extracts.loadAnnotation",
										document: annotationsWorkspace.lastElementChild, 
										identifier: annotationIdentifier,
										isMainDocument: false,
										needsRewrite: true, 
										clickable: false, 
										collapseAllowed: false, 
										isCollapseBlock: false,
										isFullPage: false,
										location: annotationURL,
										fullWidthPossible: false
									});
								},
								onFailure: (event) => {
									GW.notificationCenter.fireEvent("GW.contentLoadDidFail", {
										source: "Extracts.loadAnnotation",
										document: annotationsWorkspace, 
										identifier: annotationIdentifier,
										location: annotationURL
									});
								}
							});
						}, (Popups.popupTriggerDelay / 2.0));
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
					allAnnotatedTargetsInDocument.forEach(annotatedTarget => {
						annotatedTarget.removeEventListener("mouseenter", annotatedTarget.annotationLoad_mouseEnter);
						annotatedTarget.removeEventListener("mouseleave", annotatedTarget.annotationLoad_mouseLeave);
					});
				}, { once: true });
			}, { phase: "eventListeners" });

			//	Add handler for if an annotation loads.
			GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", Extracts.signalAnnotationLoaded = (info) => {
				GWLog("Extracts.signalAnnotationLoaded", "extracts.js", 2);

				/*  If this is an annotation that’s loaded, we cache it, remove 
					it from the staging element, and fire the annotationDidLoad
					event.
					*/
				Extracts.cachedAnnotationReferenceEntries[info.identifier] = info.document;
				info.document.remove();

				GW.notificationCenter.fireEvent("GW.annotationDidLoad", { identifier: info.identifier });
			}, {
				phase: ">rewrite",
				condition: (info) => (info.document.parentElement && info.document.parentElement.id == "annotations-workspace")
			});

			//	Add handler for if loading an annotation failed.
			GW.notificationCenter.addHandlerForEvent("GW.contentLoadDidFail", Extracts.signalAnnotationLoadFailed = (info) => {
				GWLog("Extracts.signalAnnotationLoadFailed", "extracts.js", 2);

				/*	If this is an annotation that’s failed to load, then we set
					the cache value to indicate this, and fire the 
					annotationLoadDidFail event.
					*/
				Extracts.cachedAnnotationReferenceEntries[info.identifier] = "LOADING_FAILED";

				GW.notificationCenter.fireEvent("GW.annotationLoadDidFail", { identifier: info.identifier });
			}, { condition: (info) => info.document.id == "annotations-workspace" });
        }

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
		[ "EXTRACT",  			"isExtractLink",		"has-annotation", 	"annotationForTarget", 			"extract annotation"	],
		[ "DEFINITION",  		"isDefinition",			"has-annotation",	"annotationForTarget", 			"definition annotation"	],
		[ "CITATION",  			"isCitation", 			null, 				"sectionEmbedForTarget", 		"footnote"				],
		[ "CITATION_BACK_LINK",	"isCitationBackLink", 	null, 				"sectionEmbedForTarget", 		"citation-context"				],
		[ "VIDEO",  			"isVideoLink", 			"has-content", 		"videoForTarget", 				"video object"			],
		[ "LOCAL_IMAGE", 		"isLocalImageLink", 	"has-content", 		"localImageForTarget", 			"image object"			],
		[ "LOCAL_DOCUMENT", 	"isLocalDocumentLink", 	"has-content", 		"localDocumentForTarget", 		"local-document object"		],
		[ "LOCAL_CODE_FILE", 	"isLocalCodeFileLink", 	"has-content", 		"localCodeFileForTarget", 		"local-code-file"		],
		[ "LOCAL_PAGE",  		"isLocalPageLink", 		"has-content",		"localTranscludeForTarget", 	"local-transclude"	 	],
		[ "FOREIGN_SITE", 		"isForeignSiteLink", 	"has-content",	 	"foreignSiteForTarget", 		"foreign-site object"	]
	],
			
	/*	Returns the type of a target (a string). (Target types are defined in
		the Extracts.targetTypeDefinitions array.)
		*/
	targetType: (target) => {
		for ([ targetType, predicateFunctionName, ...rest ] of Extracts.targetTypeDefinitions)
			if (Extracts[predicateFunctionName](target))
				return targetType;

		return "";
	},

	/*	Returns full type info for the given target type. This contains the 
		name of the predicate function for identifying targets of that type 
		(i.e., isExtractLink), classes which should be applied to targets of 
		that type during initial processing, the fill functions to fill popups
		and popins of that type, and the classes which should be applied to 
		pop-frames of that type.
		*/
	typeInfo: (targetType) => {
		let definition = Extracts.targetTypeDefinitions.find(definition => definition[0] == targetType);
		if (!definition) return null;
		let info = { };
		[ 	info.typeName, 
			info.predicateFunctionName, 
			info.targetClasses,
			info.popupFillFunctionName,
			info.popFrameClasses
		] = definition;
		return info;
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
			   && Extracts.targetType(targetA) == Extracts.targetType(targetB);
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

	/*	This function fills a pop-frame for a given target with content. To do
		so, it uses a provided array of testing/filling function pairs. The
		testing functions are called in order until a match is found, at which 
		point the filling function of the pair is called. Provided classes, if 
		any, are then added to the pop-frame. (Details follow.)

		In addition to the pop-frame and the target, fillPopFrame() takes a
		‘possiblePopTypes’ array, which must have the following structure:

		[ [ testMethodName1, fillMethodName1|null, classString1|null ],
		  [ testMethodName2, fillMethodName2|null, classString2|null ],
		  … ]

		NOTES:

		- fillPopFrame() looks for methods of the given names in the Extracts 
		  object
		- classString must be space-delimited

		The entries will be processed IN ARRAY ORDER. For each entry:

		1. If a fill method of the given name exists, the test method of the 
		   given name will be called with the target as argument.
		2. If the test method returns true, then the pop-frame will be filled 
		   by the given fill method.
		3. If there’s a non-empty class string, the classes in the string will 
		   be added to the pop-frame.

		No further array entries will be processed once a match (i.e., test 
		method that returns true for the given target) is found.

		The function then returns true if a filling method was found (i.e., the
		pop-frame successfully filled), false otherwise.
		*/
	fillPopFrame: (popFrame) => {
		GWLog("Extracts.fillPopFrame", "extracts.js", 2);

		let target = popFrame.spawningTarget;

		let didFill = false;
		let setPopFrameContent = Popups.setPopFrameContent;
		let targetTypeInfo = Extracts.typeInfo(Extracts.targetType(target));
		if (targetTypeInfo && targetTypeInfo.popupFillFunctionName) {
			didFill = setPopFrameContent(popFrame, Extracts[targetTypeInfo.popupFillFunctionName](target));
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

	/*	Refresh (respawn or reload) a pop-frame for an annotated target after 
		its annotation (fragment) loads.
		*/
	refreshPopFrameAfterFragmentLoads: (target) => {
		GWLog("Extracts.refreshPopFrameAfterFragmentLoads", "extracts.js", 2);

		target.popFrame.classList.toggle("loading", true);

		/*	We set up an event handler for when the fragment loads, and respawn 
			the popup / re-inject the popin, after it spawns (if it 
			hasn’t de-spawned already, e.g. if the user moused out of the 
			target).
			*/
		GW.notificationCenter.addHandlerForEvent("GW.annotationDidLoad", target.refreshPopFrameWhenFragmentLoaded = (info) => {
			GWLog("refreshPopFrameWhenFragmentLoaded", "extracts.js", 2);

			//  If the pop-frame has despawned, don’t respawn it.
			if (!target.popFrame)
				return;

			//  TODO: generalize this for popins!
			Popups.spawnPopup(target);
		}, { once: true, condition: (info) => info.identifier == Extracts.targetIdentifier(target) });

		//  Add handler for if the fragment load fails.
		GW.notificationCenter.addHandlerForEvent("GW.annotationLoadDidFail", target.updatePopFrameWhenFragmentLoadFails = (info) => {
			GWLog("updatePopFrameWhenFragmentLoadFails", "extracts.js", 2);

			//  If the pop-frame has despawned, don’t respawn it.
			if (!target.popFrame)
				return;

			target.popFrame.swapClasses([ "loading", "loading-failed" ], 1);
		}, { once: true, condition: (info) => info.identifier == Extracts.targetIdentifier(target) });
	},

	/*	This function’s purpose is to allow for the transclusion of entire pages
		on the same website (displayed to the user in popups, or injected in 
		block flow as popins), and the (almost-)seamless handling of links (or
		other target elements) in such transcluded content in the same way that
		they’re handled in the root document (i.e., the actual page loaded in
		the browser window). This permits us to have truly recursive popups
		with unlimited recursion depth and no loss of functionality.

		For any given target element, originatingDocumentForTarget() asks: where
		(that is, in what *page*, a.k.a. ‘document’) was this element *defined*?

		Was it defined in the root document, or in some extract for a target in 
		the root document, at some remove (even if the target is actually within 
		an extract popup spawned by a section embed popup spawned by a footnote 
		popup)? If so, the root element of the root document (i.e., the <html> 
		element of the page) is returned.

		OR, was the target defined in an entire other page that was transcluded 
		wholesale and embedded as a pop-frame? In that case, the pop-frame 
		where said other page is embedded is returns (again, even if the given
		target is actually in a popup several levels down from that full-page
		embed popup, having been spawned by a definition which spawned a section
		link which spawned… etc.).
		*/
	originatingDocumentForTarget: (target) => {
		return target.originatingDocument || document.firstElementChild;
	},

	/*	Returns the location (a URL object) of the originating document for
		the given target.
		*/
	originatingDocumentLocationForTarget: (target) => {
		return new URL((Extracts.originatingDocumentForTarget(target) == document.firstElementChild)
					   ? location.href
					   : target.originatingDocument.closest(".popframe").spawningTarget.href);
	},

	/*	Returns true if the target location matches an already-displayed page 
		(which can be the root page of the window).
		*/
	documentIsDisplayed: (target) => {
		if (target.pathname == location.pathname)
			return true;

		if (GW.isMobile()) {
			return false;
		} else {
			return (Array.from(Popups.popupContainer.children).findIndex(popup => (
						   popup.classList.contains("external-page-embed") 
						&& popup.spawningTarget.pathname == target.pathname
						)) != -1);
		}
	},

	cachedAnnotationReferenceEntries: { },

	/*	Used to generate extract and definition pop-frames.
		*/
	referenceDataForTarget: (target) => {
		let referenceEntry = Extracts.cachedAnnotationReferenceEntries[Extracts.targetIdentifier(target)];
		let referenceElement = referenceEntry.querySelector(Extracts.annotatedTargetSelectors.map(selector => 
			`.annotation > p ${selector}`
		).join(", "));

		//  Author list.
		let authorElement = referenceEntry.querySelector(".author");
		let authorList;
		if (authorElement) {
			authorList = authorElement.textContent.split(", ").slice(0, 3).join(", ");
			if (authorList.length < authorElement.textContent.length)
				authorList += " et al";
		}

		//  Date.
		let dateElement = referenceEntry.querySelector(".date");

		return {
			element: 		referenceElement,
			titleText: 		referenceElement.textContent,
			titleHTML: 		referenceElement.innerHTML.trimQuotes(),
			authorHTML:		(authorElement ? `<span class="data-field author">${authorList}</span>` : ``),
			dateHTML:		(dateElement ? ` (<span class="data-field date">${dateElement.textContent}</span>)` : ``),
			abstractHTML:	referenceEntry.querySelector("blockquote").innerHTML
		};
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

	//  Either an extract or a definition.
	annotationForTarget: (target) => {
		GWLog("Extracts.annotationForTarget", "extracts.js", 2);

		let annotationIdentifier = Extracts.targetIdentifier(target);
		if (Extracts.cachedAnnotationReferenceEntries[annotationIdentifier] == null) {
			Extracts.refreshPopFrameAfterFragmentLoads(target);
			return `&nbsp;`;
		} else if (Extracts.cachedAnnotationReferenceEntries[annotationIdentifier] == "LOADING_FAILED") {
			target.popFrame.classList.add("loading-failed");
			return `&nbsp;`;
		}

		if (Extracts.isExtractLink(target)) {
			return Extracts.extractForTarget(target);
		} else if (Extracts.isDefinition(target)) {
			return Extracts.definitionForTarget(target);
		} else {
			return ``;
		}
	},

	//  Summaries of links to elsewhere.
	isExtractLink: (target) => {
		return target.classList.contains("docMetadata");
	},
    extractForTarget: (target) => {
		GWLog("Extracts.extractForTarget", "extracts.js", 2);

		let referenceData = Extracts.referenceDataForTarget(target);

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
        return `<p class="data-field title">${originalLinkHTML}${titleLinkHTML}</p>` 
        	 + `<p class="data-field author-plus-date">${referenceData.authorHTML}${referenceData.dateHTML}</p>` 
        	 + `<div class="data-field annotation-abstract">${referenceData.abstractHTML}</div>`;
    },

    //  Definitions.
    isDefinition: (target) => {
        return target.classList.contains("defnMetadata");
    },
    definitionForTarget: (target) => {
        GWLog("Extracts.definitionForTarget", "extracts.js", 2);

        let referenceData = Extracts.referenceDataForTarget(target);

        return `<p class="data-field title">${referenceData.titleHTML}</p>` 
        	 + `<p class="data-field author-plus-date">${referenceData.authorHTML}${referenceData.dateHTML}</p>` 
        	 + `<div class="data-field annotation-abstract">${referenceData.abstractHTML}</div>`;
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

		/*  If it has a period in it, it’s not a page, but is something else,
			like a file of some sort, or a locally archived document (accounted
			for in the other test functions, if need be).
			*/
		if (target.pathname.match(/\./))
			return false;

		return (target.hostname == location.hostname);
	},

    localTranscludeForTarget: (target) => {
		GWLog("Extracts.localTranscludeForTarget", "extracts.js", 2);

		/*	Check to see if the target location matches an already-displayed 
			page (which can be the root page of the window).
			*/
		if (Extracts.documentIsDisplayed(target)) {
			//  If it does, display the section (if an anchorlink) or nothing.
			return (target.hash > "" ? Extracts.sectionEmbedForTarget(target) : null);
		} else {
			//  Otherwise, display the entire linked page.
			target.popFrame.classList.add("external-page-embed");
			return Extracts.externalPageEmbedForTarget(target);
		}
	},

	//  Sections of the current page.
    sectionEmbedForTarget: (target) => {
		GWLog("Extracts.sectionEmbedForTarget", "extracts.js", 2);

        let nearestBlockElement = Extracts.nearestBlockElement(Extracts.originatingDocumentForTarget(target).querySelector(decodeURIComponent(target.hash)));

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
    externalPageEmbedForTarget: (target) => {
		GWLog("Extracts.externalPageEmbedForTarget", "extracts.js", 2);

		let setPopFrameContent = Popups.setPopFrameContent;

		let fillPopFrame = (markdownBody) => {
			GWLog("Filling pop-frame...", "extracts.js", 2);

			setPopFrameContent(target.popFrame, markdownBody.innerHTML);

			//  Give the pop-frame an identifying class.
			target.popFrame.classList.toggle("page-" + target.pathname.substring(1), true);

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
			if (target.hash > "") {
				let scrollElementIntoViewInPopFrame = Popups.scrollElementIntoViewInPopup;
				scrollElementIntoViewInPopFrame(target.popFrame.querySelector(decodeURIComponent(target.hash)));
			}
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

					setPopFrameContent(target.popFrame, event.target.responseText);
					Extracts.cachedPages[target.pathname] = target.popFrame.querySelector("#markdownBody");
					fillPopFrame(Extracts.cachedPages[target.pathname]);
				},
				onFailure: (event) => {
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

		return Extracts.qualifyingForeignDomains.includes(target.hostname);
	},
	foreignSiteForTarget: (target) => {
		let url = new URL(target.href);

		if ([ "www.lesswrong.com", "lesswrong.com", "www.greaterwrong.com", "greaterwrong.com" ].includes(url.hostname)) {
			url.protocol = "https:";
			url.hostname = "www.greaterwrong.com";
			url.search = "format=preview&theme=classic";
		}

		return `<iframe src="${url.href}" frameborder="0" sandbox></iframe>`;
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
		if (width > 0 && height > 0) {
			styles = `width="${width}" height="${height}" style="width: ${width}px; height: ${height}px;"`;
		}

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
			|| target.hostname != location.hostname)
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
						target.popFrame.swapClasses([ "loading", "loading-failed" ], 1);
					}
				});
			}
		});

		return `&nbsp;`;
    },	

	/**********/
	/*	Popups.
		*/

	spawnedPopupMatchingTarget: (target) => {
		return (Popups.allSpawnedPopups().find(popup => Extracts.targetsMatch(target, popup.spawningTarget)) || null);
	},

	/*	Called by popups.js just before spawning (injecting and positioning) the
		popup. This is our chance to fill the popup with content, and rewrite
		that content in whatever ways necessary. After this function exits, the
		popup will appear on the screen.
		*/
    preparePopup: (popup) => {
		GWLog("Extracts.preparePopup", "extracts.js", 2);

		let target = popup.spawningTarget;

		let existingPopup = Extracts.spawnedPopupMatchingTarget(target);
		if (existingPopup) {
			Popups.detachPopupFromTarget(existingPopup);
			existingPopup.spawningTarget = target;
			return existingPopup;
		}

		//  Import the class(es) of the target.
		popup.classList.add(...target.classList);
		//  We then remove some of the imported classes.
		popup.classList.remove("has-annotation", "has-content", "link-local", "spawns-popup");

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
				&& Popups.isVisible(Extracts.originatingDocumentForTarget(target).querySelector(decodeURIComponent(target.hash)))))
			return null;

		//  Various special handling.
		if (Extracts.isTOCLink(target)) {
			//  Designate section links spawned by the TOC (for special styling).
			popup.classList.add("toc-section");
		} else if (Extracts.isLocalCodeFileLink(target)) {
			//  Remove click listener from code popups, to allow selection.
			GW.notificationCenter.addHandlerForEvent("Popups.popupDidSpawn", (info) => {
				popup.removeEventListener("click", Popups.popupClicked);
			}, { once: true });
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

		if (Extracts.fillPopFrame(popup) == false)
			return null;

		//  Add popup title bar contents.
// 		let popupTitle;
// 		if (Extracts.isDefinition(target)) {
// 			//  TODO: account for contents possibly not being loaded yet!
// 			popupTitle = `<span class="popup-title">${popup.querySelector(".data-field.title").textContent}</span>`;
// 		} else if (!Extracts.isLocalImageLink(target)) {
// 			popupTitle = `<a 
// 				class="popup-title"
// 				href="${target.href}"
// 				title="Open ${target.href} in a new window"
// 				target="_blank"
// 					>${(target.href || "")}</a>`
// 		}
		//  NOTE: TEMPORARILY DISABLED!
// 		if (popupTitle) popup.titleBarContents.push(popupTitle);

		/*  If we’re waiting for content to be loaded into the popup 
			asynchronously, then there’s no need to do rewrites for now.
			*/
		if (!popup.classList.contains("loading")) 
			Extracts.rewritePopupContent(popup);

		return popup;
    },
    
    rewritePopupContent: (popup) => {
		GWLog("Extracts.rewritePopupContent", "extracts.js", 2);

		let target = popup.spawningTarget;

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
				let scrollElementIntoViewInPopFrame = Popups.scrollElementIntoViewInPopup;
				scrollElementIntoViewInPopFrame(citationInPopup, popup);
			});
		}

		//  Special handling for image popups.
		if (Extracts.isLocalImageLink(target)) {
			//  Remove extraneous classes from images in image popups.
			popup.querySelector("img").classList.remove("has-annotation", "has-content", "spawns-popup");

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
			//  If the target is itself in a popup, set originating document.
			let containingPopup = target.closest(".popup");
			if (containingPopup) {
				target.originatingDocument = (containingPopup.classList.contains("external-page-embed"))
											 ? containingPopup.contentView
											 : Extracts.originatingDocumentForTarget(containingPopup.spawningTarget);
			}

			GW.notificationCenter.fireEvent("GW.contentDidLoad", {
				source: "Extracts.preparePopup",
				document: popup.contentView,
				isMainDocument: false,
				needsRewrite: false, 
				clickable: false, 
				collapseAllowed: false, 
				isCollapseBlock: false,
				isFullPage: false,
				location: Extracts.originatingDocumentLocationForTarget(target),
				fullWidthPossible: false
			});
		}

		//  Loading spinners.
		if (   Extracts.isLocalDocumentLink(target)
			|| Extracts.isForeignSiteLink(target)
			|| Extracts.isLocalImageLink(target)
			) {
			popup.classList.toggle("loading", true);

			//  When loading ends (in success or failure)...
			let objectOfSomeSort = popup.querySelector("iframe, object, img");
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
						&& Extracts.server404PageTitles.includes(objectOfSomeSort.contentDocument.title))
						popup.classList.toggle("loading-failed", true);
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

let serviceProviderObjectName = GW.isMobile() ? "Popins" : "Popups";
if (window[serviceProviderObjectName]) {
	Extracts.setup();
} else {
	GW.notificationCenter.addHandlerForEvent(serviceProviderObjectName + ".didLoad", () => {
		Extracts.setup();
	}, { once: true });
}
