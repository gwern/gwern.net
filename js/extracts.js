// popups.js: standalone Javascript library for creating 'popups' which display link metadata (typically, title/author/date/summary), for extremely convenient reference/abstract reading.
// Author: Said Achmiz, Shawn Presser (mobile & Youtube support)
// Date: 2019-09-12
// When:
// license: MIT (derivative of footnotes.js, which is PD)

// popups.js parses a HTML document and looks for <a> links which have the 'link-annotated' attribute class, and the attributes 'data-popup-title', 'data-popup-author', 'data-popup-date', 'data-popup-doi', 'data-popup-abstract'.
// (These attributes are expected to be populated already by the HTML document's compiler, however, they can also be done dynamically. See '/static/js/old/wikipedia-popups.js' for an example of a library which does Wikipedia-only dynamically on page loads.)

// Popups are inspired by Wikipedia's augmented tooltips (originally implemented as editor-built extensions, now available to all readers via https://www.mediawiki.org/wiki/Page_Previews ). Whenever any such link is mouse-overed by the user, popups.js will pop up a large tooltip-like square with the contents of the attributes. This is particularly intended for references, where it is extremely convenient to autopopulate links such as to Arxiv.org/Biorxiv.org/Wikipedia with the link's title/author/date/abstract, so the reader can see it instantly. Links to 'reverse citations' are provided as much as possible: links with DOIs go to a Semantic Scholar search engine query for that DOI, which prioritizes meta-analyses & systematic reviews to provide context for any given paper (particularly whether it has failed to replicate or otherwise been debunked); for URLs ending in 'PDF' which probably have Semantic Scholar entries, they go to a title search; and for all other URLs, a Google search using the obscure `link:` operator is provided.. For more details, see `LinkMetadata.hs`.

// On mobile, clicking on links (as opposed to hovering over links on desktop) will bring up the annotation or video; another click on it or the popup will then go to it. A click outside it de-activates it.

// For an example of a Hakyll library which generates annotations for Wikipedia/Biorxiv/Arxiv/PDFs/arbitrarily-defined links, see https://www.gwern.net/static/build/LinkMetadata.hs ; for a live demonstration, see the links in https://www.gwern.net/newsletter/2019/07

/*******************************/
/*  Events fired by extracts.js:

    Extracts.didLoad
        Fired when the Extracts object has loaded.

    Extracts.setupDidComplete
        Fired just before the ‘setup’ function returns.

    Extracts.cleanupDidComplete
        Fired just before the ‘cleanup’ function returns.

	Extracts.targetsDidProcessOnContentLoad {
			source: "Extracts.processTargetsOnContentLoad"
			document:
				The `document` property of the GW.contentDidLoad event that
				triggered the Extracts.processTargetsOnContentLoad handler.
            loadLocation:
                The `loadLocation` property of the GW.contentDidLoad event that
				triggered the Extracts.processTargetsOnContentLoad handler.
            baseLocation:
                The `baseLocation` property of the GW.contentDidLoad event that
				triggered the Extracts.processTargetsOnContentLoad handler.
			flags:
				The `flags` property of the GW.contentDidLoad event that
				triggered the Extracts.processTargetsOnContentLoad handler.
		}
		Fired after targets in a document have been processed (classes applied,
		event listeners attached, etc.).

    GW.contentDidLoad {
            source: "Extracts.rewritePopFrameContent_LOCAL_PAGE"
            document:
                The `document` property of the local transclude pop-frame.
            loadLocation:
                URL of the local page (including anchor, if any).
            baseLocation:
            	Same as loadLocation.
            flags:
                0 (no flags set)
        }
        Fired at the last stage of preparing a local page embed pop-frame for
        spawning (after the pop-frame’s content has been loaded from the cache
        of local pages).

        (See rewrite.js for more information about the keys and values of the
         GW.contentDidLoad event.)

    GW.contentDidLoad {
            source: "Extracts.refreshPopFrameAfterLocalPageLoads"
            document:
                A DocumentFragment containing the embedded page elements.
            loadLocation:
                URL of the local page (including anchor, if any).
            baseLocation:
            	Same as loadLocation.
            flags: GW.contentDidLoadEventFlags.needsRewrite
        }
        Fired at the last stage of preparing a local page embed pop-frame for
        spawning (after the pop-frame’s content has been freshly loaded via
        a network request).

        (See rewrite.js for more information about the keys and values of the
         GW.contentDidLoad event.)
 */

Extracts = {
    /*  Target containers.
     */
    contentContainersSelector: ".markdownBody, #TOC, #page-metadata, #sidebar",

    /*  Targets.
     */
    targets: {
        targetElementsSelector: "a[href]",
        excludedElementsSelector: [
            ".section-self-link",
            ".footnote-self-link",
            ".sidenote-self-link",
            "[aria-hidden='true']"
        ].join(", "),
        excludedContainerElementsSelector: "h1, h2, h3, h4, h5, h6",
        //  See comment at Extracts.isLocalPageLink for info on this function.
        //  Called by: pop-frame providers (popins.js or popups.js).
        testTarget: (target) => {
            let targetTypeInfo = Extracts.targetTypeInfo(target);
            if (targetTypeInfo) {
                let specialTestFunction = Extracts[`testTarget_${targetTypeInfo.typeName}`]
                if (   specialTestFunction
                	&& specialTestFunction(target) == false)
                    return false;

                //  Do not allow pop-frames to spawn themselves.
                //	TODO: verify!
                let containingPopFrame = Extracts.popFrameProvider.containingPopFrame(target);
                if (   containingPopFrame
                	&& Extracts.targetsMatch(containingPopFrame.spawningTarget, target))
                    return false;

                //  Added specified classes to the target.
                if (targetTypeInfo.targetClasses) {
                	if (typeof targetTypeInfo.targetClasses == "string")
	                    target.classList.add(...(targetTypeInfo.targetClasses.split(" ")));
	                else if (typeof targetTypeInfo.targetClasses == "function")
	                	target.classList.add(...(targetTypeInfo.targetClasses(target).split(" ")));
                }

                return true;
            }

            return false;
        }
    },

    /*  Misc. configuration.
     */
    server404PageTitles: [
        "404 Not Found"
    ],

    pageTitleRegexp: /^(.+?) · Gwern\.net$/,

    rootDocument: document,

    /******************/
    /*  Infrastructure.
     */

    //  Can be ‘Popups’ or ‘Popins’, currently.
    popFrameProviderName: null,
    //  Can be the Popups or Popins object, currently.
    popFrameProvider: null,

    /***********/
    /*  General.
     */

    //  Called by: Extracts.cleanup
    removeTargetsWithin: (container) => {
        GWLog("Extracts.removeTargetsWithin", "extracts.js", 1);

        //  Target restore function (same for popups and popins).
        let restoreTarget = (target) => {
            //  Restore title attribute, if any.
            if (target.dataset.attributeTitle) {
                target.title = target.dataset.attributeTitle;
                target.removeAttribute("data-attribute-title");
            }

            target.classList.remove("has-content", "has-annotation", "has-annotation-partial");
        };

        Extracts.popFrameProvider.removeTargetsWithin(container, Extracts.targets, restoreTarget);
    },

    //  Called by: extracts-options.js
    cleanup: () => {
        GWLog("Extracts.cleanup", "extracts.js", 1);

		//	Remove pop-frame indicator hooks.
		document.querySelectorAll(".has-content").forEach(link => {
			link.querySelector(".indicator-hook").remove();
		});

        //  Unbind event listeners and restore targets.
        document.querySelectorAll(Extracts.contentContainersSelector).forEach(container => {
            Extracts.removeTargetsWithin(container);
        });

		//	Remove pop-frames & containers.
		Extracts.popFrameProvider.cleanup();

        //  Remove content load event handlers.
        [ Extracts.processTargetsOnContentLoad,
          Extracts.setUpAnnotationLoadEvent,
          ].forEach(handler => GW.notificationCenter.removeHandlerForEvent("GW.contentDidLoad", handler));

        if (Extracts.popFrameProvider == Popups) {
            //  Remove “popups disabled” icon/button, if present.
            if (Extracts.popupOptionsEnabled)
                Extracts.removePopupsDisabledShowPopupOptionsDialogButton();
        } else {
            //  TODO: this!
        }

        //  Fire cleanup-complete event.
        GW.notificationCenter.fireEvent("Extracts.cleanupDidComplete");
    },

    //  Called by: Extracts.processTargetsInDocument
    //  Called by: extracts-options.js
    addTargetsWithin: (container) => {
        GWLog("Extracts.addTargetsWithin", "extracts.js", 1);

        if (Extracts.popFrameProvider == Popups) {
            Popups.addTargetsWithin(container, Extracts.targets, Extracts.preparePopup, Extracts.preparePopupTarget);
        } else if (Extracts.popFrameProvider == Popins) {
            Popins.addTargetsWithin(container, Extracts.targets, Extracts.preparePopin, Extracts.preparePopinTarget);
        }

		Extracts.setUpAnnotationLoadEventWithin(container);
    },

    //  Called by: extracts.js (doSetup)
    //  Called by: extracts-options.js
    setup: () => {
        GWLog("Extracts.setup", "extracts.js", 1);

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

			if (!Extracts.popinsEnabled()) {
				return;
			}

            GWLog("Activating popins.", "extracts.js", 1);
        }

        /*  Add handler to set up targets in loaded content (including
            newly-spawned pop-frames; this allows for recursion), and to
            add hover/click event listeners to annotated targets, to load
            annotations (fragments).
         */
        GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", Extracts.processTargetsOnContentLoad = (info) => {
            GWLog("Extracts.processTargetsOnContentLoad", "extracts.js", 2);

            Extracts.processTargetsInDocument(info.document, info.needsRewrite);

			//	Fire targets-processed event.
			GW.notificationCenter.fireEvent("Extracts.targetsDidProcessOnContentLoad", {
				source: "Extracts.processTargetsOnContentLoad",
				loadLocation: info.loadLocation,
				baseLocation: info.baseLocation,
				document: info.document,
				flags: info.flags
			});
        }, { phase: "eventListeners" });

        //  Fire setup-complete event.
        GW.notificationCenter.fireEvent("Extracts.setupDidComplete");
    },

    //  Called by: Extracts.setup
    processTargetsInDocument: (doc = Extracts.rootDocument, addHooks = true) => {
        GWLog("Extracts.processTargetsInDocument", "extracts.js", 2);

		if (   doc instanceof DocumentFragment
			|| (   doc instanceof Element 
			    && doc.closest(Extracts.contentContainersSelector))) {
			Extracts.addTargetsWithin(doc);
		} else {
            doc.querySelectorAll(Extracts.contentContainersSelector).forEach(container => {
                Extracts.addTargetsWithin(container);
            });
        }

		/*	Add pop-frame indicator hooks, if need be.
			(See links.css for how these are used.)
		 */
		if (addHooks) {
			doc.querySelectorAll(".has-content").forEach(link => {
				link.insertAdjacentHTML("afterbegin", `<span class='indicator-hook'></span>`);

				/*	Inject U+2060 WORD JOINER at start of first text node of the
					link. (It _must_ be injected as a Unicode character into the
					existing text node; injecting it within the .indicator-hook
					span, or as an HTML escape code into the text node, or in
					any other fashion, creates a separate text node, which
					causes all sorts of problems - text shadow artifacts, etc.)
				 */
				let linkFirstTextNode = link.firstTextNode;
				if (linkFirstTextNode)
					linkFirstTextNode.textContent = "\u{2060}" + linkFirstTextNode.textContent;
			});
		}
    },

    /***********/
    /*  Content.
     */

    /*  This array defines the types of ‘targets’ (ie. annotated links,
        links pointing to available content such as images or code files,
        citations, etc.) that Extracts supports.
        The fields in each entry are:
            1. Type name
            2. Type predicate function (of the Extracts object) for identifying
               targets of the type; returns true iff target is of that type
            3. Class(es) to be added to targets of the type (these are added
               during initial processing)
            4. Fill function (of the Extracts object); called to fill a
               pop-frame for a target of that type with content
            5. Class(es) to be added to a pop-frame for targets of that type
     */
    targetTypeDefinitions: [
        [ "LOCAL_PAGE",         "isLocalPageLink",      "has-content",      "localTranscludeForTarget",     "local-transclude"      ],
    ],

    /*  Returns full type info for the given target (in other words, the data
        from the appropriate row of the targetTypeDefinitions array), or null
        if the target is not matched by the predicate function of any known type.
     */
    //  Called by: many functions, all in extracts.js
    targetTypeInfo: (target) => {
        let info = { };
        for (definition of Extracts.targetTypeDefinitions) {
            [   info.typeName,
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

    /*  Returns the target identifier: the original URL (for locally archived
        pages), or the relative url (for local links), or the full URL (for
        foreign links).
     */
    //  Called by: Extracts.targetsMatch
    //  Called by: Extracts.fillPopFrame
    //  Called by: extracts-annotations.js
    targetIdentifier: (target) => {
        if (target.dataset.urlOriginal) {
            let originalURL = new URL(target.dataset.urlOriginal);

            /*  Special cases where the original URL of the target does not
                match the target’s proper identifier (possibly due to outgoing
                link rewriting).
             */
            if (originalURL.hostname == "ar5iv.labs.arxiv.org") {
                originalURL.hostname = "arxiv.org";
                originalURL.pathname = originalURL.pathname.replace("/html/", "/abs/");
                /*	Erase the ?fallback=original query parameter necessary to 
                	make it redirect if no Ar5iv version is available.
                 */
                originalURL.search = ""; 
            }

            return originalURL.href;
        } else {
            return (target.hostname == location.hostname
                   ? target.pathname + target.hash
                   : target.href);
        }
    },

    /*  Returns true if the two targets will spawn identical popups
        (that is, if they are of the same type, and have the same identifiers).
     */
    //  Called by: Extracts.targets.testTarget
    //  Called by: Extracts.spawnedPopupMatchingTarget
    targetsMatch: (targetA, targetB) => {
        return    Extracts.targetIdentifier(targetA) == Extracts.targetIdentifier(targetB)
               && Extracts.targetTypeInfo(targetA).typeName == Extracts.targetTypeInfo(targetB).typeName;
    },

    //  Called by: Extracts.localTranscludeForTarget
    //  Called by: Extracts.titleForPopFrame_LOCAL_PAGE
    nearestBlockElement: (element) => {
        return (   element.closest("section, .footnote, .sidenote, .markdownBody > *")
        		|| element.closest("p"))
    },

    /*  This function fills a pop-frame for a given target with content. It
        returns true if the pop-frame successfully filled, false otherwise.
     */
    //  Called by: Extracts.preparePopFrame
    //  Called by: Extracts.refreshPopFrameAfterLocalPageLoads
    //  Called by: extracts-annotations.js
    fillPopFrame: (popFrame) => {
        GWLog("Extracts.fillPopFrame", "extracts.js", 2);

        let didFill = false;
        let target = popFrame.spawningTarget;
        let targetTypeInfo = Extracts.targetTypeInfo(target);
        if (   targetTypeInfo
        	&& targetTypeInfo.popFrameFillFunctionName) {
            didFill = Extracts.popFrameProvider.setPopFrameContent(popFrame, Extracts[targetTypeInfo.popFrameFillFunctionName](target));
            if (targetTypeInfo.popFrameClasses)
            	Extracts.popFrameProvider.addClassesToPopFrame(popFrame, ...(targetTypeInfo.popFrameClasses.split(" ")));
        }

        if (didFill) {
            return true;
        } else {
            GWLog(`Unable to fill pop-frame (${Extracts.targetIdentifier(target)} [${(targetTypeInfo ? targetTypeInfo.typeName : "UNDEFINED")}])!`, "extracts.js", 1);
            return false;
        }
    },

    //  Called by: Extracts.targetDocument
    //  Called by: Extracts.preparePopup
    //  Called by: Extracts.preparePopin
    //  Called by: extracts-annotations.js
    popFrameHasLoaded: (popFrame) => {
        return !(popFrame.classList.contains("loading") || popFrame.classList.contains("loading-failed"));
    },

    //  Called by: Extracts.titleForPopFrame
    //  Called by: Extracts.titleForPopFrame_LOCAL_PAGE
    //  Called by: extracts-annotations.js
    //  Called by: extracts-content.js
    standardPopFrameTitleElementForTarget: (target, titleText) => {
        if (typeof titleText == "undefined")
            titleText = (target.hostname == location.hostname)
                        ? target.pathname + target.hash
                        : target.href;

        /*  Because tab-handling is bad on mobile, readers expect the original
            remote URL to open up in-tab, as readers will be single-threaded;
            on desktop, we can open up in a tab for poweruser-browsing of
            tab-explosions.
         */
        let whichWindow = (Extracts.popFrameProvider == Popins) ? "current" : "new";
        let linkTarget = (Extracts.popFrameProvider == Popins) ? "_self" : "_blank";
        return `<a
            class="popframe-title-link"
            href="${target.href}"
            title="Open ${target.href} in ${whichWindow} window."
            target="${linkTarget}"
                >${titleText}</a>`;
    },

    /*  Returns the contents of the title element for a pop-frame.
     */
    //  Called by: Extracts.preparePopup
    //  Called by: Extracts.preparePopin
    //  Called by: Extracts.rewritePopinContent
    titleForPopFrame: (popFrame) => {
        let target = popFrame.spawningTarget;

        //  Special handling for certain popup types.
        let targetTypeName = Extracts.targetTypeInfo(target).typeName;
        let specialTitleFunction = (Extracts.popFrameProvider == Popups
                                    ? Extracts[`titleForPopup_${targetTypeName}`]
                                    : Extracts[`titleForPopin_${targetTypeName}`])
                                || Extracts[`titleForPopFrame_${targetTypeName}`];
        if (specialTitleFunction)
            return specialTitleFunction(popFrame);
        else
            return Extracts.standardPopFrameTitleElementForTarget(target);
    },

    /*  This function’s purpose is to allow for the transclusion of entire pages
        on the same website (displayed to the user in popups, or injected in
        block flow as popins), and the (almost-)seamless handling of local links
        in such transcluded content in the same way that they’re handled in the
        root document (ie. the actual page loaded in the browser window). This
        permits us to have truly recursive popups with unlimited recursion depth
        and no loss of functionality.

        For any given target element, targetDocument() asks: to what local
        document does the link refer?

        This may be either the root document, or an entire other page that was
        transcluded wholesale and embedded as a pop-frame (of class
        ‘external-page-embed’).
     */
    //  Called by: Extracts.localTranscludeForTarget
    //  Called by: Extracts.titleForPopFrame_LOCAL_PAGE
    //  Called by: extracts-content.js
    targetDocument: (target) => {
        if (target.hostname != location.hostname)
            return null;

        if (target.pathname == location.pathname)
            return Extracts.rootDocument;

        if (Extracts.popFrameProvider == Popups) {
            let popupForTargetDocument = Popups.allSpawnedPopups().find(popup => (   popup.classList.contains("external-page-embed")
                                                                                  && popup.spawningTarget.pathname == target.pathname));
            return popupForTargetDocument ? popupForTargetDocument.body : null;
        } else if (Extracts.popFrameProvider == Popins) {
            let popinForTargetDocument = Popins.allSpawnedPopins().find(popin => (   popin.classList.contains("external-page-embed")
                                                                                  && popin.spawningTarget.pathname == target.pathname)
                                                                                  && Extracts.popFrameHasLoaded(popin));
            return popinForTargetDocument ? popinForTargetDocument.body : null;
        }
    },

    /*  Returns the location (a URL object) of the document for a given target.
     */
    //  Called by: Extracts.rewritePopFrameContent_LOCAL_PAGE
    //  Called by: Extracts.refreshPopFrameAfterLocalPageLoads
    //  Called by: extracts-annotations.js
    //  Called by: extracts-content.js
    locationForTarget: (target) => {
        return new URL(target.href);
    },

    /*  Activate loading spinner for an object pop-frame.
     */
    //  Called by: extracts-content.js
    setLoadingSpinner: (popFrame) => {
        let target = popFrame.spawningTarget;

        popFrame.classList.toggle("loading", true);

        //  When loading ends (in success or failure)...
        let objectOfSomeSort = popFrame.body.querySelector("iframe, object, img, video");
        if (objectOfSomeSort.tagName == "IFRAME") {
            //  Iframes do not fire ‘error’ on server error.
            objectOfSomeSort.onload = (event) => {
                popFrame.classList.toggle("loading", false);

                /*  We do this for local documents only. Cross-origin
                    protections prevent us from accessing the content of
                    an iframe with a foreign site, so we do nothing special
                    and simply let the foreign site’s server show its usual
                    404 page (or whatever) if the linked page is not found.
                 */
                if (   target.hostname == location.hostname
                    && Extracts.server404PageTitles.includes(objectOfSomeSort.contentDocument.title)) {
                    popFrame.classList.toggle("loading-failed", true);
                }
            };
        } else {
            //  Objects & images fire ‘error’ on server error or load fail.
            objectOfSomeSort.onload = (event) => {
                popFrame.classList.toggle("loading", false);
            };
        }
        /*  We set an ‘error’ handler for *all* types of entity, even
            iframes, just in case.
         */
        objectOfSomeSort.onerror = (event) => {
            popFrame.swapClasses([ "loading", "loading-failed" ], 1);
        };
    },

	//	Called by: Extracts.refreshPopFrameAfterLocalPageLoads
	//	Called by: Extracts.refreshPopFrameAfterCodeFileLoads
	//	Called by: Extracts.refreshPopFrameAfterAuxLinksLoad
	postRefreshSuccessUpdatePopFrameForTarget: (target) => {
        GWLog("Extracts.postRefreshSuccessUpdatePopFrameForTarget", "extracts.js", 2);

		if (Extracts.popFrameProvider.isSpawned(target.popFrame) == false)
			return;

		//  Re-spawn, or fill and rewrite, the pop-frame.
		if (Extracts.popFrameProvider == Popups) {
			Popups.spawnPopup(target);
		} else if (Extracts.popFrameProvider == Popins) {
			Extracts.fillPopFrame(target.popin);
			target.popin.classList.toggle("loading", false);

			Extracts.rewritePopinContent(target.popin);

			requestAnimationFrame(() => {
				Popins.scrollPopinIntoView(target.popin);
			});
		}
	},

	//	Called by: Extracts.refreshPopFrameAfterLocalPageLoads
	//	Called by: Extracts.refreshPopFrameAfterCodeFileLoads
	//	Called by: Extracts.refreshPopFrameAfterAuxLinksLoad
	postRefreshFailureUpdatePopFrameForTarget: (target) => {
        GWLog("Extracts.postRefreshFailureUpdatePopFrameForTarget", "extracts.js", 2);

		if (Extracts.popFrameProvider.isSpawned(target.popFrame) == false)
			return;

		target.popFrame.swapClasses([ "loading", "loading-failed" ], 1);
	},

	//	Called by: Extracts.rewritePopFrameContent_LOCAL_PAGE
	//	Called by: Extracts.rewritePopupContent_CITATION_BACK_LINK
    scrollToTargetedElementInPopFrame: (target, popFrame) => {
        GWLog("Extracts.scrollToTargetedElementInPopFrame", "extracts.js", 3);

        if (target.hash > "") {
            requestAnimationFrame(() => {
            	let element = null;
                if (   popFrame
                	&& (element = popFrame.body.querySelector(selectorFromHash(target.hash))))
                	requestAnimationFrame(() => {
	                    Extracts.popFrameProvider.scrollElementIntoViewInPopFrame(element);
	                });
            });
        }
    },

    //  Make anchorlinks scroll pop-frame instead of opening normally.
	constrainLinkClickBehaviorInPopFrame: (popFrame, extraCondition = (link => true)) => {
        let target = popFrame.spawningTarget;

        popFrame.body.querySelectorAll("a").forEach(link => {
            if (   link.hostname == target.hostname
                && link.pathname == target.pathname
                && link.hash > ""
                && extraCondition(link)) {
                link.onclick = () => { return false; };
                link.addActivateEvent((event) => {
                    let hashTarget = popFrame.body.querySelector(selectorFromHash(link.hash));
                    if (hashTarget) {
                        Extracts.popFrameProvider.scrollElementIntoViewInPopFrame(hashTarget);
                        return false;
                    } else {
                        return true;
                    }
                });
            }
        });
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

    /*  Local links (to sections of the current page, or other site pages).
     */
    //  Called by: Extracts.targetTypeInfo (as `predicateFunctionName`)
    isLocalPageLink: (target) => {
        if (   target.hostname != location.hostname
            || Extracts.isAnnotatedLink(target))
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

    //  Called by: Extracts.fillPopFrame (as `popFrameFillFunctionName`)
    //	Called by: Extracts.citationForTarget (extracts-content.js)
    //	Called by: Extracts.citationBackLinkForTarget (extracts-content.js)
    localTranscludeForTarget: (target, unwrapFunction, forceNarrow) => {
        GWLog("Extracts.localTranscludeForTarget", "extracts.js", 2);

		unwrapFunction = unwrapFunction ?? (blockElement => blockElement);

        /*  Check to see if the target location matches an already-displayed
            page (which can be the root page of the window).

            If there already is a pop-frame that displays the entire linked
			page, and if the link points to an anchor, display the linked
			section or element.

			Also display just the linked block if we’re spawning this pop-frame
			from an in-pop-frame TOC.
         */
        let fullTargetDocument = Extracts.targetDocument(target);
        if (   target.hash > ""
        	&& (   fullTargetDocument
        		|| forceNarrow
        		|| target.closest(".TOC"))) {
        	/*	If the loaded and cached full page exists but is not displayed
        		in a pop-frame, we get the linked block from there instead
        		(we still only display the linked block, not the full page).
        	 */
            if (fullTargetDocument == null)
            	fullTargetDocument = Extracts.cachedPages[target.pathname];

            if (fullTargetDocument) {
            	let linkedElement = fullTargetDocument.querySelector(selectorFromHash(target.hash));
				let nearestBlock = Extracts.nearestBlockElement(linkedElement);

				return newDocument(unwrapFunction(nearestBlock));
			} else {
				/*	If the page hasn’t been loaded yet, load it; upon the 
					reload, we’ll return here but take the previous (true) 
					branch of this conditional.
				 */
				Extracts.refreshPopFrameAfterLocalPageLoads(target);

				return newDocument();
			}
        } else {
            /*  Otherwise, display the entire linked page.

                (Note that we might end up here because there is yet no
                 pop-frame with the full linked document, OR because there is
                 such a pop-frame but it’s a pinned popup or something (and thus
                 we didn’t want to despawn it and respawn it at this target’s
                 location).
             */
			//  Mark the pop-frame as an external page embed.
			Extracts.popFrameProvider.addClassesToPopFrame(target.popFrame, "external-page-embed");

			if (Extracts.cachedPages[target.pathname]) {
				//  Give the pop-frame an identifying class.
				Extracts.popFrameProvider.addClassesToPopFrame(target.popFrame, "page-" + target.pathname.slice(1));

				return newDocument(Extracts.cachedPages[target.pathname]);
			} else {
				Extracts.refreshPopFrameAfterLocalPageLoads(target);

				return newDocument();
			}
        }
    },

    /*  TOC links.
     */
    //  Called by: Extracts.testTarget_LOCAL_PAGE
    //  Called by: Extracts.preparePopup_LOCAL_PAGE
    isTOCLink: (target) => {
        return (target.closest("#TOC") != null);
    },

    /*  Links in the sidebar.
     */
    //  Called by: Extracts.testTarget_LOCAL_PAGE
    isSidebarLink: (target) => {
        return (target.closest("#sidebar") != null);
    },

    /*  This “special testing function” is used to exclude certain targets which
        have already been categorized as (in this case) `LOCAL_PAGE` targets. It
        returns false if the target is to be excluded, true otherwise. Excluded
        targets will not spawn pop-frames.
     */
    //  Called by: Extracts.targets.testTarget (as `testTarget_${targetTypeInfo.typeName}`)
    testTarget_LOCAL_PAGE: (target) => {
        return (!(   Extracts.popFrameProvider == Popins
                  && (   Extracts.isTOCLink(target)
                      || Extracts.isSidebarLink(target))));
    },

    //  Called by: Extracts.preparePopFrame (as `preparePopFrame_${targetTypeName}`)
    //	Called by: Extracts.preparePopup_LOCAL_PAGE
	preparePopFrame_LOCAL_PAGE: (popFrame) => {
        GWLog("Extracts.preparePopFrame_LOCAL_PAGE", "extracts.js", 2);

		//	Add to a full-page pop-frame the body classes of the page.
		if (   popFrame.classList.contains("external-page-embed")
			&& Extracts.cachedPageBodyClasses[popFrame.spawningTarget.pathname] > null)
			Extracts.popFrameProvider.addClassesToPopFrame(popFrame, ...Extracts.cachedPageBodyClasses[popFrame.spawningTarget.pathname]);

		return popFrame;
	},

    //  Called by: Extracts.preparePopup (as `preparePopup_${targetTypeName}`)
    preparePopup_LOCAL_PAGE: (popup) => {
        GWLog("Extracts.preparePopup_LOCAL_PAGE", "extracts.js", 2);

        let target = popup.spawningTarget;

		popup = Extracts.preparePopFrame_LOCAL_PAGE(popup);

        /*  Designate popups spawned from section links in the the TOC (for
            special styling).
         */
        if (Extracts.isTOCLink(target))
        	Extracts.popFrameProvider.addClassesToPopFrame(popup, "toc-section");

        return popup;
    },

    //  Called by: Extracts.titleForPopFrame (as `titleForPopFrame_${targetTypeName}`)
    titleForPopFrame_LOCAL_PAGE: (popFrame) => {
        GWLog("Extracts.titleForPopFrame_LOCAL_PAGE", "extracts.js", 2);

        let target = popFrame.spawningTarget;
		let targetDocument = Extracts.targetDocument(target) || Extracts.cachedPages[target.pathname];
		let nearestBlockElement = ((targetDocument != null) && (target.hash > "")
								   ? Extracts.nearestBlockElement(targetDocument.querySelector(selectorFromHash(target.hash)))
								   : null);

        let popFrameTitleText = ((() => {
        	//	Designate section embeds with a section mark (§).
        	return ((   target.hash > ""
            		 && nearestBlockElement
           			 && nearestBlockElement.tagName == "SECTION"
        			 && !popFrame.classList.contains("external-page-embed"))
           			? "&#x00a7; "
           			: "");
        })() + (() => {
			if (target.pathname == location.pathname) {
				//  Sections of the current page.
				if (nearestBlockElement.id == "footnotes") {
					/*	Special case for the Footnotes section, which has no heading 
						associated with it, and thus no text to use as a section
						title.
					 */
					return "Footnotes";
				} else {
					return (nearestBlockElement.tagName == "SECTION"
							? nearestBlockElement.firstElementChild.textContent
							: target.hash);
				}
			} else {
				//	Other pages (whole pages or sections).
				if (popFrame.classList.contains("external-page-embed")) {
					//  Entire other pages.
					return (Extracts.cachedPageTitles[target.pathname] || target.pathname);
				} else {
					//  Sections of other pages.
					if (targetDocument) {
						let pageTitleOrPath = Extracts.cachedPageTitles[target.pathname] || target.pathname;
						if (nearestBlockElement.id == "footnotes") {
							/*	Special case for the Footnotes section, which has no
								heading associated with it, and thus no text to use 
								as a section title.
							 */
							return `Footnotes (${pageTitleOrPath})`;
						} else {
							return (nearestBlockElement.tagName == "SECTION"
									? `${nearestBlockElement.firstElementChild.textContent} (${pageTitleOrPath})`
									: `${target.hash} (${pageTitleOrPath})`);
						}
					} else {
						return (target.pathname + target.hash);
					}
				}
			}
        })());

        return Extracts.standardPopFrameTitleElementForTarget(target, popFrameTitleText);
    },

    //  Called by: Extracts.rewritePopinContent_LOCAL_PAGE
    //  Called by: Extracts.rewritePopupContent_LOCAL_PAGE
    //  Called by: Extracts.rewritePopinContent (as `rewritePopFrameContent_${targetTypeName}`)
    //  Called by: Extracts.rewritePopupContent (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_PAGE: (popFrame) => {
        GWLog("Extracts.rewritePopFrameContent_LOCAL_PAGE", "extracts.js", 2);

        let target = popFrame.spawningTarget;

        //  Fire a contentDidLoad event.
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "Extracts.rewritePopFrameContent_LOCAL_PAGE",
            document: popFrame.document,
            loadLocation: Extracts.locationForTarget(target),
            baseLocation: Extracts.locationForTarget(target),
            flags: 0
        });

		//	Lazy-loading of adjacent sections.
		//	WARNING: Experimental code!
// 		if (target.hash > "") {
// 			requestAnimationFrame(() => {
// 				Extracts.loadAdjacentSections(popFrame, "next,prev");
// 			});
// 		}

        //  Scroll to the target.
        Extracts.scrollToTargetedElementInPopFrame(target, popFrame);
    },

	loadAdjacentSections: (popFrame, which) => {
        GWLog("Extracts.loadAdjacentSections", "extracts.js", 2);

		which = which.split(",");
		let next = which.includes("next");
		let prev = which.includes("prev");

		let target = popFrame.spawningTarget;
		let sourceDocument = Extracts.cachedPages[target.pathname] || Extracts.rootDocument;

		popFrame.firstSection = popFrame.firstSection || sourceDocument.querySelector(selectorFromHash(target.hash));
		popFrame.lastSection = popFrame.lastSection || popFrame.firstSection;

		if (!(next || prev))
			return;

		if (popFrame.body.querySelector(selectorFromHash(target.hash)) == null) {
			let sectionWrapper = document.createElement("SECTION");
			sectionWrapper.id = popFrame.firstSection.id;
			sectionWrapper.classList.add(...(popFrame.firstSection.classList));
			sectionWrapper.replaceChildren(...(popFrame.body.children));
			popFrame.body.appendChild(sectionWrapper);

			//  Fire a contentDidLoad event.
			GW.notificationCenter.fireEvent("GW.contentDidLoad", {
				source: "Extracts.loadAdjacentSections",
				document: popFrame.body.firstElementChild,
				loadLocation: Extracts.locationForTarget(target),
				baseLocation: Extracts.locationForTarget(target),
				flags: 0
			});
		}

		let prevSection = popFrame.firstSection.previousElementSibling;
		if (prev && prevSection) {
			popFrame.body.insertBefore(newDocument(prevSection), popFrame.body.firstElementChild);

			//  Fire a contentDidLoad event.
			GW.notificationCenter.fireEvent("GW.contentDidLoad", {
				source: "Extracts.loadAdjacentSections",
				document: popFrame.body.firstElementChild,
				loadLocation: Extracts.locationForTarget(target),
				baseLocation: Extracts.locationForTarget(target),
				flags: 0
			});

			popFrame.firstSection = prevSection;
		}

		let nextSection = popFrame.lastSection.nextElementSibling;
		if (next && nextSection) {
			popFrame.body.insertBefore(newDocument(nextSection), null);

			//  Fire a contentDidLoad event.
			GW.notificationCenter.fireEvent("GW.contentDidLoad", {
				source: "Extracts.loadAdjacentSections",
				document: popFrame.body.lastElementChild,
				loadLocation: Extracts.locationForTarget(target),
				baseLocation: Extracts.locationForTarget(target),
				flags: 0
			});

			popFrame.lastSection = nextSection;
		}
	},

    //  Called by: Extracts.rewritePopinContent (as `rewritePopinContent_${targetTypeName}`)
    rewritePopinContent_LOCAL_PAGE: (popin) => {
        GWLog("Extracts.rewritePopinContent_LOCAL_PAGE", "extracts.js", 2);

        Extracts.rewritePopFrameContent_LOCAL_PAGE(popin);

        /*  Make anchorlinks scroll popin instead of opening normally
        	(but only for non-popin-spawning anchorlinks).
         */
		Extracts.constrainLinkClickBehaviorInPopFrame(popin, (link => link.classList.contains("no-popin")));
    },

    //  Called by: Extracts.rewritePopupContent (as `rewritePopupContent_${targetTypeName}`)
    rewritePopupContent_LOCAL_PAGE: (popup) => {
        GWLog("Extracts.rewritePopupContent_LOCAL_PAGE", "extracts.js", 2);

        Extracts.rewritePopFrameContent_LOCAL_PAGE(popup);

        let target = popup.spawningTarget;

		//	Insert page thumbnail into page abstract.
		if (Extracts.cachedPageThumbnailImageTags[target.pathname]) {
			let pageAbstract = popup.body.querySelector("#page-metadata + .abstract blockquote");
			if (pageAbstract)
				pageAbstract.insertAdjacentHTML("afterbegin", `<figure>${Extracts.cachedPageThumbnailImageTags[target.pathname]}</figure>`);
		}

        //  Make anchorlinks scroll popup instead of opening normally.
		Extracts.constrainLinkClickBehaviorInPopFrame(popup);
    },

    //  Other site pages.
    cachedPages: { },
    cachedPageTitles: { },
    cachedPageBodyClasses: { },
    cachedPageThumbnailImageTags: { },

    //  Called by: Extracts.externalPageEmbedForTarget
    refreshPopFrameAfterLocalPageLoads: (target) => {
        GWLog("Extracts.refreshPopFrameAfterLocalPageLoads", "extracts.js", 2);

        target.popFrame.classList.toggle("loading", true);

        doAjax({
            location: target.href,
            onSuccess: (event) => {
				let page = newDocument(event.target.responseText);

				//	Get the page thumbnail URL and metadata.
				let pageThumbnailMetaTag = page.querySelector("meta[property='og:image']");
				if (pageThumbnailMetaTag) {
					let pageThumbnailURL = new URL(pageThumbnailMetaTag.getAttribute("content"));

					//	Alt text, if provided.
					let pageThumbnailAltMetaTag = page.querySelector("meta[property='og:image:alt']");
					let pageThumbnailAltText = pageThumbnailAltMetaTag
											   ? pageThumbnailAltMetaTag.getAttribute("content")
											   : `Thumbnail image for “${Extracts.cachedPageTitles[target.pathname]}”`;

					//	Image dimensions.
					let pageThumbnailWidth = page.querySelector("meta[property='og:image:width']").getAttribute("content");
					let pageThumbnailHeight = page.querySelector("meta[property='og:image:height']").getAttribute("content");

					//	Construct and save the <img> tag.
					if (pageThumbnailURL.pathname != "/static/img/logo/logo-whitebg-large-border.png")
						Extracts.cachedPageThumbnailImageTags[target.pathname] = `<img
							src='${pageThumbnailURL.href}'
							alt='${pageThumbnailAltText}'
							width='${pageThumbnailWidth}'
							height='${pageThumbnailHeight}'
							style='width: ${pageThumbnailWidth}px; height: auto;'
								>`;

					//	Request the image, to cache it.
					doAjax({ location: pageThumbnailURL.href });
				}

				//	Get the body classes.
				Extracts.cachedPageBodyClasses[target.pathname] = page.querySelector("meta[name='page-body-classes']").getAttribute("content").split(" ");

                //  Get the page title.
                Extracts.cachedPageTitles[target.pathname] = page.querySelector("title").innerHTML.match(Extracts.pageTitleRegexp)[1];

                //  The content is the page body plus the metadata block.
                let pageContent = page.querySelector("#markdownBody");

				//	Add the page metadata block.
                let pageMetadata = page.querySelector("#page-metadata");
                if (pageMetadata) {
                	pageMetadata.classList.remove("markdownBody");
                	if (pageMetadata.className == "")
                		pageMetadata.removeAttribute("class");
                    pageContent.insertBefore(pageMetadata, pageContent.firstElementChild);
                }

				//	Discard extraneous DOM structure.
				page.replaceChildren(...pageContent.children);

				//	Cache the constructed page (document fragment).
				Extracts.cachedPages[target.pathname] = page;

                /*  Trigger the rewrite pass by firing the requisite event.
                 */
                GW.notificationCenter.fireEvent("GW.contentDidLoad", {
                    source: "Extracts.refreshPopFrameAfterLocalPageLoads",
                    document: Extracts.cachedPages[target.pathname],
					loadLocation: Extracts.locationForTarget(target),
					baseLocation: Extracts.locationForTarget(target),
                    flags: GW.contentDidLoadEventFlags.needsRewrite
                });

				Extracts.postRefreshSuccessUpdatePopFrameForTarget(target);
            },
            onFailure: (event) => {
                Extracts.postRefreshFailureUpdatePopFrameForTarget(target);
            }
        });
    },

    /***************************/
    /*  Pop-frames (in general).
     */

    //  Called by: Extracts.preparePopup
    //  Called by: Extracts.preparePopin
    preparePopFrame: (popFrame) => {
        GWLog("Extracts.preparePopFrame", "extracts.js", 2);

        let target = popFrame.spawningTarget;

        //  Import the class(es) of the target.
        popFrame.classList.add(...target.classList);
        //  We then remove some of the imported classes.
        popFrame.classList.remove("has-annotation", "has-annotation-partial", 
        	"has-content", "link-self", "link-local", 
        	"spawns-popup", "spawns-popin", "uri");

        //  Attempt to fill the popup.
        if (Extracts.fillPopFrame(popFrame) == false)
            return null;

        //  Add ‘markdownBody’ class.
        popFrame.body.classList.add("markdownBody");

		//	Inject styles.
		[ "inlined-styles-colors", 
		  "inlined-styles", 
		  "inlined-dark-mode-styles", 
		  "inlined-fonts", 
		  "graphical-link-icons",
		  "mathjax-styles" ].forEach(styleBlockID => {
		  	let existingStyleBlock = document.getElementById(styleBlockID);
		  	if (existingStyleBlock == null)
		  		return;

			let styleBlock = existingStyleBlock.cloneNode(true);
			popFrame.document.insertBefore(styleBlock, popFrame.body);
		});
		document.querySelectorAll("link[rel='stylesheet']").forEach(cssLink => {
			popFrame.document.insertBefore(cssLink.cloneNode(true), popFrame.body);
		});

		/*	For local content embed pop-frames, add handler to trigger 
			transcludes in source document when they trigger in the pop-frame.
		 */
		if (Extracts.isLocalPageLink(target)) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", (info) => {
				let identifiedAncestor = info.document.parentElement;
				while (   identifiedAncestor
					   && identifiedAncestor.id == ""
					   && identifiedAncestor.parentElement != null) {
					identifiedAncestor = identifiedAncestor.parentElement;
				}
				let containerSelector = null;
				if (identifiedAncestor.id) {
					containerSelector = "#" + CSS.escape(decodeURIComponent(identifiedAncestor.id));
				} else if (popFrame.titleBar) {
					let popFrameTitleLink = popFrame.titleBar.querySelector(".popframe-title-link");
					if (popFrameTitleLink)
						containerSelector = selectorFromHash(popFrameTitleLink.hash);
				}
				let sourceDocument = Extracts.cachedPages[target.pathname] || Extracts.rootDocument;
				let containerInSourceDocument = containerSelector 
												? sourceDocument.querySelector(containerSelector)
												: sourceDocument;

				//	Trigger transcludes.
				Transclude.triggerTranscludesInContainer(containerInSourceDocument);
			}, { 
				condition: (info) => (   info.source == "transclude" 
									  && info.document.getRootNode() == popFrame.document)
			});
		}

		//	Add handler to update popup position when content changes.
		GW.notificationCenter.addHandlerForEvent("Rewrite.contentDidChange", (info) => {
			if (Extracts.popFrameProvider == Popups)
				Popups.positionPopup(popFrame);
		}, { condition: (info) => (info.document == popFrame.document) });

        return popFrame;
    },

    /**********/
    /*  Popins.
     */

    //  Called by: Extracts.setup
    popinsEnabled: () => {
        return (localStorage.getItem("extract-popins-disabled") != "true");
    },

    /*  Called by popins.js when adding a target.
     */
    //  (See Extracts.addTargetsWithin)
	preparePopinTarget: (target) => {
		target.adjustPopinWidth = (popin) => {
			let leftMargin, rightMargin;
			let popinRect = popin.getBoundingClientRect();
			if (GW.mediaQueries.mobileWidth.matches) {
				//	Make popin take up entire content column width.
				let bodyRect = document.body.getBoundingClientRect();
				leftMargin = (bodyRect.left - popinRect.left);
				rightMargin = (popinRect.right - bodyRect.right);
			} else {
				let markdownBodyRect = document.querySelector("#markdownBody").getBoundingClientRect();
				leftMargin = (markdownBodyRect.left - popinRect.left);
				rightMargin = (popinRect.right - markdownBodyRect.right);
			}
			popin.style = `margin-left: ${leftMargin}px; `
						+ `margin-right: ${rightMargin}px; `
						+ `width: calc(100% + ${(-1 * (leftMargin + rightMargin))}px)`;
		};
	},

    /*  Called by popins.js just before injecting the popin. This is our chance
        to fill the popin with content, and rewrite that content in whatever
        ways necessary. After this function exits, the popin will appear on the
        screen.
     */
    //  Called by: popins.js
    preparePopin: (popin) => {
        GWLog("Extracts.preparePopin", "extracts.js", 2);

        let target = popin.spawningTarget;

        /*  Call generic pop-frame prepare function (which will attempt to fill
            the popin).
         */
        if ((popin = Extracts.preparePopFrame(popin)) == null)
            return null;

        //  Add popin title bar contents.
        let popinTitle = Extracts.titleForPopFrame(popin);
        if (popinTitle) {
            popin.titleBarContents = [
                `<span class="popframe-title">${popinTitle}</span>`,
                Popins.titleBarComponents.closeButton()
            ];

            //  Add the options button.
            if (Extracts.popinOptionsEnabled)
                popup.titleBarContents.push(Extracts.showPopinOptionsDialogPopinTitleBarButton());
        }

        //  Special handling for certain popin types.
        let targetTypeName = Extracts.targetTypeInfo(target).typeName;
        let specialPrepareFunction = Extracts[`preparePopin_${targetTypeName}`] || Extracts[`preparePopFrame_${targetTypeName}`];
        if (specialPrepareFunction)
            if ((popin = specialPrepareFunction(popin)) == null)
                return null;

        /*  If we’re waiting for content to be loaded into the popin
            asynchronously, then there’s no need to do rewrites for now.
         */
        if (Extracts.popFrameHasLoaded(popin))
            Extracts.rewritePopinContent(popin);

        return popin;
    },

    //  Called by: Extracts.refreshPopFrameAfterLocalPageLoads
    //  Called by: extracts-annotations.js
    //  Called by: extracts-content.js
    rewritePopinContent: (popin) => {
        GWLog("Extracts.rewritePopinContent", "extracts.js", 2);

        let target = popin.spawningTarget;

        //  Update the title.
        if (popin.titleBar)
            popin.titleBar.querySelector(".popframe-title").innerHTML = Extracts.titleForPopFrame(popin);

        //  Special handling for certain popin types.
        let targetTypeName = Extracts.targetTypeInfo(target).typeName;
        let specialRewriteFunction = Extracts[`rewritePopinContent_${targetTypeName}`] || Extracts[`rewritePopFrameContent_${targetTypeName}`];
        if (specialRewriteFunction)
            specialRewriteFunction(popin);

        //  For object popins, scroll popin into view once object loads.
        let objectOfSomeSort = popin.body.querySelector("iframe, object, img, video");
        if (objectOfSomeSort) {
            objectOfSomeSort.addEventListener("load", (event) => {
                requestAnimationFrame(() => {
                    Popins.scrollPopinIntoView(popin);
                });
            });
        }
    },

    /**********/
    /*  Popups.
     */

    //  Called by: Extracts.setup
    //  Called by: extracts-options.js
    popupsEnabled: () => {
        return (localStorage.getItem("extract-popups-disabled") != "true");
    },

    //  Called by: Extracts.preparePopup
    spawnedPopupMatchingTarget: (target) => {
        return Popups.allSpawnedPopups().find(popup =>
                   Extracts.targetsMatch(target, popup.spawningTarget)
                && Popups.popupIsEphemeral(popup));
    },

    /*  Called by popups.js when adding a target.
     */
    //  (See Extracts.addTargetsWithin)
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

    /*  Called by popups.js just before spawning (injecting and positioning) the
        popup. This is our chance to fill the popup with content, and rewrite
        that content in whatever ways necessary. After this function exits, the
        popup will appear on the screen.
     */
    //  (See also Extracts.addTargetsWithin)
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

        /*  Call generic pop-frame prepare function (which will attempt to fill
            the popup).
         */
        if ((popup = Extracts.preparePopFrame(popup)) == null)
            return null;

        //  Add popup title bar contents.
        let popupTitle = Extracts.titleForPopFrame(popup);
        if (popupTitle) {
            popup.titleBarContents = [
                Popups.titleBarComponents.closeButton(),
                Popups.titleBarComponents.zoomButton().enableSubmenu(),
                Popups.titleBarComponents.pinButton(),
                `<span class="popframe-title">${popupTitle}</span>`
            ];

            //  Add the options button.
            if (Extracts.popupOptionsEnabled)
                popup.titleBarContents.push(Extracts.showPopupOptionsDialogPopupTitleBarButton());
        }

        //  Special handling for certain popup types.
        let targetTypeName = Extracts.targetTypeInfo(target).typeName;
        let specialPrepareFunction = Extracts[`preparePopup_${targetTypeName}`] || Extracts[`preparePopFrame_${targetTypeName}`];
        if (specialPrepareFunction)
            if ((popup = specialPrepareFunction(popup)) == null)
                return null;

        /*  If we’re waiting for content to be loaded into the popup
            asynchronously, then there’s no need to do rewrites for now.
         */
        if (Extracts.popFrameHasLoaded(popup))
            Extracts.rewritePopupContent(popup);

        return popup;
    },

    //  Called by: Extracts.preparePopup
    //  Called by: extracts-content.js
    rewritePopupContent: (popup) => {
        GWLog("Extracts.rewritePopupContent", "extracts.js", 2);

        let target = popup.spawningTarget;

        //  Special handling for certain popup types.
        let targetTypeName = Extracts.targetTypeInfo(target).typeName;
        let specialRewriteFunction = Extracts[`rewritePopupContent_${targetTypeName}`] || Extracts[`rewritePopFrameContent_${targetTypeName}`];
        if (specialRewriteFunction)
            specialRewriteFunction(popup);

        //  Ensure no reflow due to figures.
        popup.body.querySelectorAll("figure[class^='float-'] img[width]").forEach(img => {
            if (img.style.width <= "") {
                img.style.width = img.getAttribute("width") + "px";
                img.style.maxHeight = "unset";
            }
        });
    }
};

GW.notificationCenter.fireEvent("Extracts.didLoad");

//  Set pop-frame type (mode) - popups or popins.
let mobileMode =    (localStorage.getItem("extracts-force-popins") == "true") 
				 || GW.isMobile() 
				 || matchMedia("(max-width: 1279px) and (max-height: 959px)").matches;
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
    GW.notificationCenter.addHandlerForEvent(Extracts.popFrameProviderName + ".didLoad", (info) => {
        doSetup();
    }, { once: true });
}
