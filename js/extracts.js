// popups.js: standalone Javascript library for creating 'popups' which display link metadata (typically, title/author/date/summary), for extremely convenient reference/abstract reading.
// Author: Said Achmiz, Shawn Presser (mobile & Youtube support)
// Date: 2019-09-12
// When:
// license: MIT (derivative of footnotes.js, which is PD)

// Popups are inspired by Wikipedia's augmented tooltips (originally implemented as editor-built extensions, now available to all readers via https://www.mediawiki.org/wiki/Page_Previews ). Whenever any such link is mouse-overed by the user, popups.js will pop up a large tooltip-like square with the contents of the attributes. This is particularly intended for references, where it is extremely convenient to autopopulate links such as to Arxiv.org/Biorxiv.org/Wikipedia with the link's title/author/date/abstract, so the reader can see it instantly. Links to 'reverse citations' are provided as much as possible: links with DOIs go to a Semantic Scholar search engine query for that DOI, which prioritizes meta-analyses & systematic reviews to provide context for any given paper (particularly whether it has failed to replicate or otherwise been debunked); for URLs ending in 'PDF' which probably have Semantic Scholar entries, they go to a title search; and for all other URLs, a Google search using the obscure `link:` operator is provided.. For more details, see `LinkMetadata.hs`.

// On mobile, clicking on links (as opposed to hovering over links on desktop) will bring up the annotation or video; another click on it or the popup will then go to it. A click outside it de-activates it.

// For an example of a Hakyll library which generates annotations for Wikipedia/Biorxiv/Arxiv/PDFs/arbitrarily-defined links, see https://www.gwern.net/static/build/LinkMetadata.hs ; for a live demonstration, see the links in https://www.gwern.net/newsletter/2019/07

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

        //  Remove content inject event handler.
    	GW.notificationCenter.removeHandlerForEvent("GW.contentDidInject", Extracts.processTargetsOnContentInject);

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

    //  Called by: Extracts.processTargetsInContainer
    //  Called by: extracts-options.js
    addTargetsWithin: (container) => {
        GWLog("Extracts.addTargetsWithin", "extracts.js", 1);

        if (Extracts.popFrameProvider == Popups) {
            Popups.addTargetsWithin(container, Extracts.targets, Extracts.preparePopup, Extracts.preparePopupTarget);
        } else if (Extracts.popFrameProvider == Popins) {
            Popins.addTargetsWithin(container, Extracts.targets, Extracts.preparePopin, Extracts.preparePopinTarget);
        }

		Extracts.setUpAnnotationLoadEventsWithin(container);
		Extracts.setUpContentLoadEventsWithin(container);
    },

    //  Called by: extracts.js (doSetup)
    //  Called by: extracts-options.js
    setup: () => {
        GWLog("Extracts.setup", "extracts.js", 1);

		//  Set pop-frame type (mode) - popups or popins.
		let mobileMode = (    localStorage.getItem("extracts-force-popins") == "true"
						  || GW.isMobile() 
						  || matchMedia("(max-width: 1279px) and (max-height: 959px)").matches);
		Extracts.popFrameProviderName = mobileMode ? "Popins" : "Popups";
		GWLog(`${(mobileMode ? "Mobile" : "Non-mobile")} client detected. Activating ${(mobileMode ? "popins" : "popups")}.`, "extracts.js", 1);

		//  Prevent null references.
		Popups = window["Popups"] || { };
		Popins = window["Popins"] || { };

		//	If provider not loaded yet, defer setup until it is.
		if (window[Extracts.popFrameProviderName] == null) {
			GW.notificationCenter.addHandlerForEvent(Extracts.popFrameProviderName + ".didLoad", (info) => {
				Extracts.setup();
			}, { once: true });

			return;
		}

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
        GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", Extracts.processTargetsOnContentInject = (info) => {
            GWLog("Extracts.processTargetsOnContentInject", "extracts.js", 2);

            Extracts.processTargetsInContainer(info.container);

			//	Fire targets-processed event.
			GW.notificationCenter.fireEvent("Extracts.targetsDidProcessOnContentInject", {
				source: "extractsProcessTargetsOnContentLoad",
				loadLocation: info.loadLocation,
				container: info.container,
				document: info.document,
				flags: info.flags
			});
        }, { phase: "eventListeners" });

        //  Fire setup-complete event.
        GW.notificationCenter.fireEvent("Extracts.setupDidComplete");
    },

    //  Called by: Extracts.setup
    processTargetsInContainer: (container) => {
        GWLog("Extracts.processTargetsInContainer", "extracts.js", 2);

		if (   container instanceof DocumentFragment
			|| (   container instanceof Element 
			    && container.closest(Extracts.contentContainersSelector))) {
			Extracts.addTargetsWithin(container);
		} else {
            container.querySelectorAll(Extracts.contentContainersSelector).forEach(contentContainer => {
                Extracts.addTargetsWithin(contentContainer);
            });
        }

		/*	Add pop-frame indicator hooks, if need be.
			(See links.css for how these are used.)
		 */
		container.querySelectorAll(".has-content").forEach(link => {
			if (link.querySelector(".indicator-hook") != null)
				return;

			link.insertAdjacentHTML("afterbegin", `<span class="indicator-hook"></span>`);

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
	targetTypeDefinitions: [ ],

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

    //  Called by: Extracts.targetsMatch
    //  Called by: Extracts.fillPopFrame
    //  Called by: extracts-annotations.js
    targetIdentifier: (target) => {
    	return Extracts.isAnnotatedLink(target)
    		   ? Annotations.targetIdentifier(target)
    		   : (target.hostname == location.hostname
                  ? target.pathname + target.hash
                  : target.href);
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
		return Transclude.fillTemplateNamed("pop-frame-title-standard", {
			titleLinkHref:      target.href,
			popFrameTitleText:  titleText
		}, {
			linkTarget:   ((Extracts.popFrameProvider == Popins) ? "_self" : "_blank"),
			whichTab:     ((Extracts.popFrameProvider == Popins) ? "current" : "new"),
			tabOrWindow:  (GW.isMobile() ? "tab" : "window")
		}).innerHTML;
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
        ‘full-page’).
     */
    //  Called by: Extracts.localPageForTarget
    //  Called by: Extracts.titleForPopFrame_LOCAL_PAGE
    //  Called by: extracts-content.js
    targetDocument: (target) => {
        if (target.hostname != location.hostname)
            return null;

        if (target.pathname == location.pathname)
            return Extracts.rootDocument;

        if (Extracts.popFrameProvider == Popups) {
            let popupForTargetDocument = Popups.allSpawnedPopups().find(popup => (   popup.classList.contains("full-page")
                                                                                  && popup.spawningTarget.pathname == target.pathname));
            return popupForTargetDocument ? popupForTargetDocument.body : null;
        } else if (Extracts.popFrameProvider == Popins) {
            let popinForTargetDocument = Popins.allSpawnedPopins().find(popin => (   popin.classList.contains("full-page")
                                                                                  && popin.spawningTarget.pathname == target.pathname)
                                                                                  && Extracts.popFrameHasLoaded(popin));
            return popinForTargetDocument ? popinForTargetDocument.body : null;
        }
    },

    /*  Activate loading spinner for an object pop-frame.
     */
    //  Called by: extracts-content.js
    setLoadingSpinner: (popFrame) => {
        let target = popFrame.spawningTarget;

        popFrame.classList.toggle("loading", true);

        //  When loading ends (in success or failure)...
        let objectOfSomeSort = popFrame.document.querySelector("iframe, object, img, video");
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

	/*	If the reference data is not yet available, we either queue the 
		refresh-pop-frame callbacks for when it does load or fail (in the case 
		where the network request hasn’t come back yet), or mark the pop-frame 
		as “loading failed” and do nothing (if the load has failed).
	 */
	//	Called by: Extracts.localPageForTarget
	//	Called by: Extracts.localCodeFileForTarget
	//	Called by: Extracts.annotationForTarget
	handleIncompleteReferenceData: (target, referenceData, dataProvider) => {
        if (referenceData == null) {
            /*  If the content has yet to be loaded, we’ll trust that it has
            	been asked to load, and meanwhile wait, and do nothing yet.
             */
			target.popFrame.classList.toggle("loading", true);

			dataProvider.waitForDataLoad(dataProvider.targetIdentifier(target), 
			   (identifier) => {
				Extracts.postRefreshSuccessUpdatePopFrameForTarget(target);
			}, (identifier) => {
				Extracts.postRefreshFailureUpdatePopFrameForTarget(target);
			});
        } else if (referenceData == "LOADING_FAILED") {
            /*  If we’ve already tried and failed to load the content, we
                will not try loading again, and just show the “loading failed”
                message.
             */
            target.popFrame.classList.add("loading-failed");
        }
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
                    && (element = targetElementInDocument(target, popFrame.document)))
                	requestAnimationFrame(() => {
	                    Extracts.popFrameProvider.scrollElementIntoViewInPopFrame(element);
	                });
            });
        }
    },

    //  Make anchorlinks scroll pop-frame instead of opening normally.
	constrainLinkClickBehaviorInPopFrame: (popFrame, extraCondition = (link => true)) => {
        let target = popFrame.spawningTarget;

        popFrame.document.querySelectorAll("a").forEach(link => {
            if (   link.hostname == target.hostname
                && link.pathname == target.pathname
                && link.hash > ""
                && extraCondition(link)) {
                link.onclick = () => { return false; };
                link.addActivateEvent((event) => {
                    let hashTarget = targetElementInDocument(link, popFrame.document);
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
        	"has-content", "link-self", "link-page", 
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

		//	Add handler to update popup position when content changes.
		GW.notificationCenter.addHandlerForEvent("Rewrite.contentDidChange", (info) => {
			if (Extracts.popFrameProvider == Popups)
				Popups.positionPopup(popFrame);
			else // if (Extracts.popFrameProvider == Popins)
				Popins.scrollPopinIntoView(popFrame);
		}, { 
			condition: (info) => (info.document == popFrame.document) 
		});

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
				let containerSelector = [ 
					".abstract blockquote",
					".markdownBody" 
				].join(", ");
				let containerRect = popin.closest(containerSelector).getBoundingClientRect();
				leftMargin = (containerRect.left - popinRect.left);
				rightMargin = (popinRect.right - containerRect.right);
			}
			popin.style = `margin-left: ${leftMargin}px; `
						+ `margin-right: ${rightMargin}px; `
						+ `width: calc(${popinRect.width}px + ${(-1 * (leftMargin + rightMargin))}px)`;
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

    //	Called by: Extracts.preparePopin
    //  Called by: Extracts.postRefreshSuccessUpdatePopFrameForTarget
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
        let objectOfSomeSort = popin.document.querySelector("iframe, object, img, video");
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

    //	Called by: Extracts.preparePopup
    //  Called by: Extracts.postRefreshSuccessUpdatePopFrameForTarget
    rewritePopupContent: (popup) => {
        GWLog("Extracts.rewritePopupContent", "extracts.js", 2);

        let target = popup.spawningTarget;

        //  Special handling for certain popup types.
        let targetTypeName = Extracts.targetTypeInfo(target).typeName;
        let specialRewriteFunction = Extracts[`rewritePopupContent_${targetTypeName}`] || Extracts[`rewritePopFrameContent_${targetTypeName}`];
        if (specialRewriteFunction)
            specialRewriteFunction(popup);

        //  Ensure no reflow due to figures.
        popup.document.querySelectorAll("figure[class^='float-'] img[width]").forEach(img => {
            if (img.style.width <= "") {
                img.style.width = img.getAttribute("width") + "px";
                img.style.maxHeight = "unset";
            }
        });
    }
};

GW.notificationCenter.fireEvent("Extracts.didLoad");

Extracts.setup();
