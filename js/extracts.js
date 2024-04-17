// popups.js: standalone Javascript library for creating 'popups' which display link metadata (typically, title/author/date/summary), for extremely convenient reference/abstract reading.
// Author: Said Achmiz, Shawn Presser (mobile & Youtube support)
// Date: 2019-09-12
// When:
// license: MIT (derivative of footnotes.js, which is PD)

// Popups are inspired by Wikipedia's augmented tooltips (originally implemented as editor-built extensions, now available to all readers via https://www.mediawiki.org/wiki/Page_Previews ). Whenever any such link is mouse-overed by the user, popups.js will pop up a large tooltip-like square with the contents of the attributes. This is particularly intended for references, where it is extremely convenient to autopopulate links such as to Arxiv.org/Biorxiv.org/Wikipedia with the link's title/author/date/abstract, so the reader can see it instantly. Links to 'reverse citations' are provided as much as possible: links with DOIs go to a Semantic Scholar search engine query for that DOI, which prioritizes meta-analyses & systematic reviews to provide context for any given paper (particularly whether it has failed to replicate or otherwise been debunked); for URLs ending in 'PDF' which probably have Semantic Scholar entries, they go to a title search; and for all other URLs, a Google search using the obscure `link:` operator is provided.. For more details, see `LinkMetadata.hs`.

// On mobile, clicking on links (as opposed to hovering over links on desktop) will bring up the annotation or video; another click on it or the popup will then go to it. A click outside it de-activates it.

// For an example of a Hakyll library which generates annotations for Wikipedia/Biorxiv/Arxiv/PDFs/arbitrarily-defined links, see <https://gwern.net/static/build/LinkMetadata.hs>; for examples, see the links in <https://gwern.net/lorem-links>

Extracts = {
    /******************/
    /*  Infrastructure.
     */

    rootDocument: document,

    //  Can be ‘Popups’ or ‘Popins’, currently.
    popFrameProviderName: null,
    //  Can be the Popups or Popins object, currently.
    popFrameProvider: null,

    /***********/
    /*  General.
     */

	//	Called by: Extracts.removeTargetsWithin
	restoreTarget: (target) => {
		//  Restore title attribute, if any.
		if (target.dataset.attributeTitle) {
			target.title = target.dataset.attributeTitle;
			target.removeAttribute("data-attribute-title");
		}

		target.classList.remove("has-content", "has-annotation", "has-annotation-partial");
	},

    //  Called by: Extracts.cleanup
    removeTargetsWithin: (container) => {
        GWLog("Extracts.removeTargetsWithin", "extracts.js", 1);

		container.querySelectorAll(Extracts.config.targetElementsSelector).forEach(target => {
			if (   target.matches(Extracts.config.excludedElementsSelector)
				|| target.closest(Extracts.config.excludedContainerElementsSelector) != null)
				return;

			if (Extracts.testTarget(target) == false)
				return;

			Extracts.restoreTarget(target);

			Extracts.popFrameProvider.removeTarget(target);
		});
    },

    //  Called by: extracts-options.js
    cleanup: () => {
        GWLog("Extracts.cleanup", "extracts.js", 1);

		//	Remove pop-frame indicator hooks.
		Extracts.rootDocument.querySelectorAll(".has-indicator-hook").forEach(link => {
			link.querySelector(".indicator-hook").remove();
			link.classList.remove("has-indicator-hook");
		});

        //  Unbind event listeners and restore targets.
        Extracts.rootDocument.querySelectorAll(Extracts.config.contentContainersSelector).forEach(container => {
            Extracts.removeTargetsWithin(container);
        });

        //  Remove content inject event handler.
    	GW.notificationCenter.removeHandlerForEvent("GW.contentDidInject", Extracts.processTargetsOnContentInject);

		//	Remove phantom popin cleaning handler.
		if (Extracts.popFrameProvider == Popins)
			GW.notificationCenter.removeHandlerForEvent("GW.contentDidInject", Extracts.cleanPopinsFromInjectedContent);

		//	Remove pop-frames & containers.
		Extracts.popFrameProvider.cleanup();

        //  Fire cleanup-complete event.
        GW.notificationCenter.fireEvent("Extracts.cleanupDidComplete");
    },

    //  Called by: Extracts.processTargetsInContainer
    //  Called by: extracts-options.js
    addTargetsWithin: (container) => {
        GWLog("Extracts.addTargetsWithin", "extracts.js", 1);

		container.querySelectorAll(Extracts.config.targetElementsSelector).forEach(target => {
			if (   target.matches(Extracts.config.excludedElementsSelector)
				|| target.closest(Extracts.config.excludedContainerElementsSelector) != null)
				return;

			if (Extracts.testTarget(target) == false)
				return;

			if (Extracts.popFrameProvider == Popups)
				Extracts.preparePopupTarget(target);
			else // if (Extracts.popFrameProvider == Popins)
				Extracts.preparePopinTarget(target);

			let popFramePrepareFunction = (Extracts.popFrameProvider == Popups
										   ? Extracts.preparePopup
										   : Extracts.preparePopin);
			Extracts.popFrameProvider.addTarget(target, popFramePrepareFunction);
		});

		/*	Add pop-frame indicator hooks, if need be.
			(See links.css for how these are used.)
		 */
		container.querySelectorAll(".has-content").forEach(link => {
			if (link.closest(Extracts.config.hooklessLinksContainersSelector) != null)
				return;

			if (link.querySelector(".indicator-hook") != null)
				return;

			/*	Inject indicator hook span.
				(If the link already has a recently-modified icon hook, we must,
				 firstly, inject the indicator hook after the recently-modified
				 icon hook, and secondly, inject a text node containing a 
				 U+2060 WORD JOINER between the two hooks. This ensures that the
				 two link styling elements are arranged properly, and do not 
				 span a line break.)
			 */
			let recentlyModifiedIconHook = link.querySelector(".recently-modified-icon-hook");
			link.insertBefore(newElement("SPAN", { class: "indicator-hook" }), 
							  recentlyModifiedIconHook?.nextSibling ?? link.firstChild);
			if (recentlyModifiedIconHook)
				link.insertBefore(document.createTextNode("\u{2060}"), recentlyModifiedIconHook.nextSibling);

			/*	Inject U+2060 WORD JOINER at start of first text node of the
				link. (It _must_ be injected as a Unicode character into the
				existing text node; injecting it within the .indicator-hook
				span, or as an HTML escape code into the text node, or in
				any other fashion, creates a separate text node, which
				causes all sorts of problems - text shadow artifacts, etc.)
			 */
			let linkFirstTextNode = link.firstTextNode;
			if (   linkFirstTextNode
				&& linkFirstTextNode.textContent.startsWith("\u{2060}") == false)
				linkFirstTextNode.textContent = "\u{2060}" + linkFirstTextNode.textContent;

			link.classList.add("has-indicator-hook");
		});

		Extracts.setUpAnnotationLoadEventsWithin(container);
		Extracts.setUpContentLoadEventsWithin(container);
    },

    //  Called by: extracts.js (doSetup)
    //  Called by: extracts-options.js
    setup: () => {
        GWLog("Extracts.setup", "extracts.js", 1);

		//  Set pop-frame type (mode) - popups or popins.
		let mobileMode = (   localStorage.getItem("extracts-force-popins") == "true"
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

		//	Inject mode selectors, if need be.
		if (Extracts.modeSelector == null) {
			Extracts.injectModeSelector();
			Extracts.rootDocument.querySelectorAll(".extracts-mode-selector-inline").forEach(element => {
				Extracts.injectModeSelector(element);
			});
		}

		//	Do not proceed if disabled.
        if (Extracts.popFrameProvider == Popups) {
            GWLog("Setting up for popups.", "extracts.js", 1);

            if (Extracts.popupsEnabled() == false)
                return;

            GWLog("Activating popups.", "extracts.js", 1);
        } else {
            GWLog("Setting up for popins.", "extracts.js", 1);

			if (Extracts.popinsEnabled() == false)
				return;

            GWLog("Activating popins.", "extracts.js", 1);
        }

		//	Run provider setup.
		Extracts.popFrameProvider.setup();

        /*  Add handler to set up targets in loaded content (including
            newly-spawned pop-frames; this allows for recursion), and to
            add hover/click event listeners to annotated targets, to load
            annotations (fragments).
         */
        addContentInjectHandler(Extracts.processTargetsOnContentInject = (eventInfo) => {
            GWLog("Extracts.processTargetsOnContentInject", "extracts.js", 2);

            Extracts.processTargetsInContainer(eventInfo.container);
        }, "eventListeners");

		//	Add handler to prevent “phantom” popins.
		if (Extracts.popFrameProvider == Popins) {
			addContentInjectHandler((eventInfo) => {
				//	Clean any existing popins.
				Popins.cleanPopinsFromContainer(eventInfo.container);
			}, "rewrite");
		}

        //  Fire setup-complete event.
        GW.notificationCenter.fireEvent("Extracts.setupDidComplete");
    },

    //  Called by: Extracts.setup
    processTargetsInContainer: (container) => {
        GWLog("Extracts.processTargetsInContainer", "extracts.js", 2);

		if (   container instanceof DocumentFragment
			|| (   container instanceof Element
			    && container.closest(Extracts.config.contentContainersSelector))) {
			Extracts.addTargetsWithin(container);
		} else {
            container.querySelectorAll(Extracts.config.contentContainersSelector).forEach(contentContainer => {
                Extracts.addTargetsWithin(contentContainer);
            });
        }
    },

    /***********/
    /*  Targets.
     */

	//  See comment at Extracts.isLocalPageLink for info on this function.
	//  Called by: Extracts.addTargetsWithin
	testTarget: (target) => {
		let targetTypeInfo = Extracts.targetTypeInfo(target);
		if (targetTypeInfo) {
			let specialTestFunction = Extracts[`testTarget_${targetTypeInfo.typeName}`]
			if (   specialTestFunction
				&& specialTestFunction(target) == false)
				return false;

			//  Do not allow pop-frames to spawn themselves.
			let containingPopFrame = Extracts.popFrameProvider.containingPopFrame(target);
			if (   containingPopFrame
				&& Extracts.targetsMatch(containingPopFrame.spawningTarget, target))
				return false;

			//	Don’t spawn duplicate popins.
			if (Extracts.popFrameProvider == Popins) {
				let popinStack = Popins.allSpawnedPopins();
				if (popinStack.findIndex(popin => Extracts.targetsMatch(popin.spawningTarget, target)) !== -1)
					return false;
			}

			//  Add specified classes to the target.
			if (targetTypeInfo.targetClasses) {
				if (typeof targetTypeInfo.targetClasses == "string")
					target.classList.add(...(targetTypeInfo.targetClasses.split(" ")));
				else if (typeof targetTypeInfo.targetClasses == "function")
					target.classList.add(...(targetTypeInfo.targetClasses(target).split(" ")));
			}

			return true;
		}

		return false;
	},

	/*  This array defines the types of ‘targets’ (ie. annotated links,
		links pointing to available content such as images or code files,
		citations, etc.) that Extracts supports.
		The fields in each entry are:
			1. Type name
			2. Type predicate function (of the Extracts object) for identifying
			   targets of the type; returns true iff target is of that type
			3. Class(es) to be added to targets of the type (these are added
			   during initial processing) (may be a function on the target)
			4. Fill function (of the Extracts object); called to fill a
			   pop-frame for a target of that type with content
			5. Class(es) to be added to a pop-frame for targets of that type
			   (may be a function on the pop-frame)
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
    targetIdentifier: (target) => {
    	return Extracts.isAnnotatedLink(target)
    		   ? Annotations.targetIdentifier(target)
    		   : (target.hostname == location.hostname
                  ? target.pathname + target.hash
                  : (target instanceof HTMLAnchorElement
			  		 ? target.getAttribute("href")
			  		 : target.href));
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
            return popupForTargetDocument ? popupForTargetDocument.document : null;
        } else if (Extracts.popFrameProvider == Popins) {
            let popinForTargetDocument = Popins.allSpawnedPopins().find(popin => (   popin.classList.contains("full-page")
                                                                                  && popin.spawningTarget.pathname == target.pathname)
                                                                                  && Extracts.popFrameHasLoaded(popin));
            return popinForTargetDocument ? popinForTargetDocument.document : null;
        }
    },

	//	Called by: extracts-content.js
	addPopFrameClassesToLink: (link, ...classes) => {
		link.dataset.popFrameClasses = [ ...(link.dataset.popFrameClasses?.split(" ") ?? [ ]), ...classes ].join(" ");
	},

    /***************************/
    /*  Pop-frames (in general).
     */

	popFrameTypeSuffix: () => {
		return (Extracts.popFrameProvider == Popups
				? "up"
				: "in");
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
            if (targetTypeInfo.popFrameClasses) {
				if (typeof targetTypeInfo.popFrameClasses == "string")
					Extracts.popFrameProvider.addClassesToPopFrame(popFrame, ...(targetTypeInfo.popFrameClasses.split(" ")));
				else if (typeof targetTypeInfo.popFrameClasses == "function")
					Extracts.popFrameProvider.addClassesToPopFrame(popFrame, ...(targetTypeInfo.popFrameClasses(popFrame).split(" ")));
			}
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
        return ((   Extracts.popFrameProvider.popFrameStateLoading(popFrame) 
        		 || Extracts.popFrameProvider.popFrameStateLoadingFailed(popFrame)) == false);
    },

    //  Called by: Extracts.titleForPopFrame
    //  Called by: Extracts.titleForPopFrame_LOCAL_PAGE
    //  Called by: extracts-annotations.js
    //  Called by: extracts-content.js
    standardPopFrameTitleElementForTarget: (target, titleText) => {
        if (typeof titleText == "undefined") {
            titleText = (target.hostname == location.hostname)
                        ? target.pathname + target.hash
                        : target.href;
            titleText = `<code>${titleText}</code>`;
    	}

		return Transclude.fillTemplateNamed("pop-frame-title-standard", {
			popFrameTitleLinkHref:  target.href,
			popFrameTitleText:      titleText
		}, Extracts.getStandardPopFrameTitleTemplateFillContext());
    },

	getStandardPopFrameTitleTemplateFillContext: () => {
        /*  Because tab-handling is bad on mobile, readers expect the original
            remote URL to open up in-tab, as readers will be single-threaded;
            on desktop, we can open up in a tab for poweruser-browsing of
            tab-explosions.
         */
		return {
			linkTarget:   ((Extracts.popFrameProvider == Popins) ? "_self" : "_blank"),
			whichTab:     ((Extracts.popFrameProvider == Popins) ? "current" : "new"),
			tabOrWindow:  ((Extracts.popFrameProvider == Popins) ? "tab" : "window")
		};
	},

    /*  Returns the contents of the title element for a pop-frame.
     */
    //  Called by: Extracts.preparePopup
    //  Called by: Extracts.preparePopin
    //  Called by: Extracts.rewritePopinContent
    titleForPopFrame: (popFrame, titleText) => {
        let target = popFrame.spawningTarget;

        //  Special handling for certain popup types.
        let targetTypeName = Extracts.targetTypeInfo(target).typeName;
        let suffix = Extracts.popFrameTypeSuffix();
        let specialTitleFunction = (   Extracts[`titleForPop${suffix}_${targetTypeName}`]
        							?? Extracts[`titleForPopFrame_${targetTypeName}`]);
        if (specialTitleFunction)
            return specialTitleFunction(popFrame, titleText);
        else
            return Extracts.standardPopFrameTitleElementForTarget(target, titleText);
    },

	//	Called by: Extracts.rewritePopinContent
	//	Called by: Extracts.rewritePopFrameContent_LOCAL_PAGE
	updatePopFrameTitle: (popFrame, titleText) => {
        GWLog("Extracts.updatePopFrameTitle", "extracts.js", 2);

		if (popFrame.titleBar) {
			popFrame.titleBar.querySelector(".popframe-title").replaceChildren(Extracts.titleForPopFrame(popFrame, titleText));
		} else if (popFrame.titleBarContents) {
			popFrame.titleBarContents.find(x => x.classList.contains("popframe-title")).replaceChildren(Extracts.titleForPopFrame(popFrame, titleText));
		}
	},

	//	Called by: Extracts.setLoadingSpinner
	postRefreshUpdatePopFrame: (popFrame, success) => {
        GWLog("Extracts.postRefreshUpdatePopFrame", "extracts.js", 2);

		if (success)
			Extracts.popFrameProvider.clearPopFrameState(popFrame);
		else
			Extracts.popFrameProvider.setPopFrameStateLoadingFailed(popFrame);

		if (Extracts.popFrameProvider.isSpawned(popFrame)) {
			//  Update pop-frame position.
			if (Extracts.popFrameProvider == Popups)
				Popups.positionPopup(popFrame, { reset: true });
			else if (Extracts.popFrameProvider == Popins)
				Popins.scrollPopinIntoView(popFrame);
		}
	},

    //  Called by: Extracts.rewritePopFrameContent
    setLoadingSpinner: (popFrame, useObject = false) => {
        Extracts.popFrameProvider.setPopFrameStateLoading(popFrame);

		if (useObject == false)
			return;
        let objectOfSomeSort = popFrame.document.querySelector("iframe, img, video, audio");
		if (objectOfSomeSort == null)
			return;

		let url = [ "IMG", "IFRAME" ].includes(objectOfSomeSort.tagName)
				  ? URLFromString(objectOfSomeSort.src)
				  : URLFromString(objectOfSomeSort.querySelector("source").src);

		/*	The HTTP HEAD trick does not work with foreign-site pop-frames,
			due to CORS. So, we use load/error events (which are less reliable).
		 */
		if (url.hostname != location.hostname) {
			objectOfSomeSort.onload = (event) => {
				Extracts.postRefreshUpdatePopFrame(popFrame, true);
			};
			//	Note that iframes do not fire ‘error’ on HTTP error.
			objectOfSomeSort.onerror = (event) => {
				Extracts.postRefreshUpdatePopFrame(popFrame, false);
			};
		} else {
			doAjax({
				location: url.href,
				method: "HEAD",
				onSuccess: (event) => {
					Extracts.postRefreshUpdatePopFrame(popFrame, true);
				},
				onFailure: (event) => {
					Extracts.postRefreshUpdatePopFrame(popFrame, false);
				}
			});
		}
    },

	//	Called by: Extracts.rewritePopFrameContent_LOCAL_PAGE
	//	Called by: Extracts.rewritePopupContent_CITATION_BACK_LINK
    scrollToTargetedElementInPopFrame: (popFrame) => {
        GWLog("Extracts.scrollToTargetedElementInPopFrame", "extracts.js", 3);

        let target = popFrame.spawningTarget;

        if (isAnchorLink(target)) {
            requestAnimationFrame(() => {
            	let element = null;
                if (   popFrame
                    && (element = targetElementInDocument(target, popFrame.document))) {
					//	Scroll to element immediately...
                    revealElement(element);

					//	... and also after the first layout pass completes.
					GW.notificationCenter.addHandlerForEvent("Layout.layoutProcessorDidComplete", (layoutEventInfo) => {
						revealElement(element);
					}, {
						condition: (layoutEventInfo) => (   layoutEventInfo.container == popFrame.body
														 && layoutEventInfo.processorName == "applyBlockSpacingInContainer"),
						once: true
					});
                }
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
                    if (hashTarget)
                        revealElement(hashTarget);
                });
            }
        });
	},

    //  Called by: Extracts.preparePopup
    //  Called by: Extracts.preparePopin
    preparePopFrame: (popFrame) => {
        GWLog("Extracts.preparePopFrame", "extracts.js", 2);

        //  Attempt to fill the popup.
        if (Extracts.fillPopFrame(popFrame) == false)
            return null;

		//  Turn loading spinner on.
		Extracts.setLoadingSpinner(popFrame);

        //  Import the class(es) of the target.
        Extracts.popFrameProvider.addClassesToPopFrame(popFrame, ...(popFrame.spawningTarget.classList));
        //  We then remove some of the imported classes.
        Extracts.popFrameProvider.removeClassesFromPopFrame(popFrame, 
        	"uri", "has-annotation", "has-annotation-partial", "has-content", 
        	"link-self", "link-annotated", "link-page", "link-tag", "icon-not", 
        	"has-icon", "has-indicator-hook", "decorate-not",
        	"spawns-popup", "spawns-popin");

		//	Import classes from include-link.
		if (popFrame.body.firstElementChild.dataset.popFrameClasses > "")
			Extracts.popFrameProvider.addClassesToPopFrame(popFrame, ...(popFrame.body.firstElementChild.dataset.popFrameClasses.split(" ")));

		//	Determine pop-frame type.
        let suffix = Extracts.popFrameTypeSuffix();

        //  Add pop-frame title bar contents.
		popFrame.titleBarContents = Extracts[`pop${suffix}TitleBarContents`](popFrame);

        //  Add ‘markdownBody’ class.
        popFrame.body.classList.add("markdownBody");

        //  Special handling for certain pop-frame types.
        let targetTypeName = Extracts.targetTypeInfo(popFrame.spawningTarget).typeName;
        let specialPrepareFunction = (   Extracts[`preparePop${suffix}_${targetTypeName}`] 
        							  ?? Extracts[`preparePopFrame_${targetTypeName}`]);
        if (specialPrepareFunction)
            if ((popFrame = specialPrepareFunction(popFrame)) == null)
                return null;

		//	Inject styles.
		let inlinedStyleIDs = [
			"inlined-styles-colors",
			"inlined-styles-colors-dark",
			"mathjax-styles"
		];
		Array.from(document.styleSheets).filter(styleSheet =>
			(   styleSheet.ownerNode.tagName == "LINK"
			 || inlinedStyleIDs.includes(styleSheet.ownerNode.id))
		).forEach(styleSheet => {
			let styleBlock = elementFromHTML("<style>"
				+ Array.from(styleSheet.cssRules).map(rule => rule.cssText).join("\n")
				+ "</style>");
			[ "id", "media" ].forEach(attribute => {
				if (styleSheet.ownerNode.hasAttribute(attribute))
					styleBlock.setAttribute(attribute, styleSheet.ownerNode.getAttribute(attribute));
			});
			popFrame.document.insertBefore(styleBlock, popFrame.body);
		});

		//	Activate dynamic layout for the pop-frame.
		startDynamicLayoutInContainer(popFrame.body);

		//	Register copy processors in pop-frame.
		registerCopyProcessorsForDocument(popFrame.document);

		//	Add handler to update pop-frame position when content changes.
		GW.notificationCenter.addHandlerForEvent("Rewrite.contentDidChange", popFrame.contentDidChangeHandler = (info) => {
			if (   Transclude.isIncludeLink(popFrame.body.firstElementChild)
				&& popFrame.body.firstElementChild.classList.contains("include-loading-failed")) {
				Extracts.postRefreshUpdatePopFrame(popFrame, false);
			} else {
				Extracts.postRefreshUpdatePopFrame(popFrame, true);
			}
		}, {
			condition: (info) => (info.document == popFrame.document)
		});
		//	Add handler to remove the above handler when pop-frame despawns.
		GW.notificationCenter.addHandlerForEvent(`Pop${suffix}s.pop${suffix}WillDespawn`, (info) => {
			GW.notificationCenter.removeHandlerForEvent("Rewrite.contentDidChange", popFrame.contentDidChangeHandler);
		}, {
			once: true,
			condition: (info) => (info[`pop${suffix}`] == popFrame)
		});

		//	Update pop-frame when content is injected.
		GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
			//	Refresh (turning loading spinner off).
			Extracts.postRefreshUpdatePopFrame(popFrame, true);

			//	Type-specific updates.
			(   Extracts[`updatePop${suffix}_${targetTypeName}`] 
			 ?? Extracts[`updatePopFrame_${targetTypeName}`]
			 )?.(popFrame);
		}, {
			phase: "<",
			condition: (info) => (   info.source == "transclude"
								  && info.document == popFrame.document),
			once: true
		});

		//	Rewrite pop-frame content when it’s injected.
		GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
			//  Type-specific rewrites.
			(   Extracts[`rewritePop${suffix}Content_${targetTypeName}`] 
			 ?? Extracts[`rewritePopFrameContent_${targetTypeName}`]
			 )?.(popFrame, info.container);

			//	Additional rewrites.
			Extracts.additionalRewrites.forEach(rewriteFunction => {
				rewriteFunction(popFrame);
			});

			//  Turn loading spinner back on, if need be.
			if (popFrame.classList.contains("object"))
				Extracts.setLoadingSpinner(popFrame, true);
		}, {
			phase: "rewrite",
			condition: (info) => (   info.source == "transclude"
								  && info.document == popFrame.document),
			once: true
		});

		//	Trigger transclude.
		Transclude.triggerTranscludesInContainer(popFrame.body, {
			source: "Extracts.preparePopFrame",
			container: popFrame.body,
			document: popFrame.document,
			context: "popFrame"
		});

        return popFrame;
    },

	//	Functions added to this array should take one argument (the pop-frame).
	additionalRewrites: [ ],

    /**********/
    /*  Popins.
     */

	popinsDisabledLocalStorageItemKey: "extract-popins-disabled",

    //  Called by: Extracts.setup
    popinsEnabled: () => {
        return (localStorage.getItem(Extracts.popinsDisabledLocalStorageItemKey) != "true");
    },

    //  Called by: Extracts.addTargetsWithin
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
			popin.style.marginLeft = `${leftMargin}px`;
			popin.style.marginRight = `${rightMargin}px`;
			popin.style.width = `calc(${popinRect.width}px + ${(-1 * (leftMargin + rightMargin))}px)`;
		};
	},

	//	Called by: Extracts.preparePopFrame (as Extracts[`pop${suffix}TitleBarContents`])
	popinTitleBarContents: (popin) => {
        let popinTitle = Extracts.titleForPopFrame(popin) ?? { };
		return [
			Extracts.disableExtractPopFramesPopFrameTitleBarButton(),
			newElement("SPAN", { "class": "popframe-title" }, { "innerHTML": popinTitle.innerHTML }),
			Popins.titleBarComponents.closeButton()
		];
	},

    /*  Called by popins.js just before injecting the popin. This is our chance
        to fill the popin with content, and rewrite that content in whatever
        ways necessary. After this function exits, the popin will appear on the
        screen.
     */
    //  Called by: popins.js
    preparePopin: (popin) => {
        GWLog("Extracts.preparePopin", "extracts.js", 2);

        /*  Call generic pop-frame prepare function (which will attempt to fill
            the popin).
         */
        return Extracts.preparePopFrame(popin);
    },

    /**********/
    /*  Popups.
     */

	popupsDisabledLocalStorageItemKey: "extract-popups-disabled",

    //  Called by: Extracts.setup
    //  Called by: extracts-options.js
    popupsEnabled: () => {
        return (localStorage.getItem(Extracts.popupsDisabledLocalStorageItemKey) != "true");
    },

    //  Called by: Extracts.addTargetsWithin
    preparePopupTarget: (target) => {
        //  Remove the title attribute (saving it first);
        if (target.title) {
            target.dataset.attributeTitle = target.title;
            target.removeAttribute("title");
        }

        //  For special positioning by Popups.js.
        target.preferSidePositioning = () => {
            return (   target.closest("li") != null
                    && target.closest(".columns") == null);
        };
    },

	//	Called by: Extracts.preparePopFrame (as Extracts[`pop${suffix}TitleBarContents`])
	popupTitleBarContents: (popup) => {
        let popupTitle = Extracts.titleForPopFrame(popup) ?? { };
		return [
			Popups.titleBarComponents.closeButton(),
			Popups.titleBarComponents.zoomButton().enableSubmenu(),
			Popups.titleBarComponents.pinButton(),
			newElement("SPAN", { "class": "popframe-title" }, { "innerHTML": popupTitle.innerHTML }),
			Extracts.disableExtractPopFramesPopFrameTitleBarButton()
		];
	},

    //  Called by: Extracts.preparePopup
    spawnedPopupMatchingTarget: (target) => {
        return Popups.allSpawnedPopups().find(popup =>
                   Extracts.targetsMatch(target, popup.spawningTarget)
                && Popups.popupIsPinned(popup) == false);
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
        if (existingPopup)
            return existingPopup;

        /*  Call generic pop-frame prepare function (which will attempt to fill
            the popup).
         */
        return Extracts.preparePopFrame(popup);
    }
};

/*****************************************************************************/
/*	Browser native image lazy loading does not seem to work in pop-frames (due
	to the shadow root or the nested scroll container or some combination
	thereof), so we have to implement it ourselves.
 */
Extracts.additionalRewrites.push(Extracts.lazyLoadImages = (popFrame) => {
    GWLog("Extracts.lazyLoadImages", "extracts.js", 2);

	popFrame.document.querySelectorAll("img[loading='lazy']").forEach(image => {
		lazyLoadObserver(() => {
			image.loading = "eager";
			image.decoding = "sync";
		}, image, {
			root: scrollContainerOf(image),
			rootMargin: window.innerHeight + "px"
		});
	});
});
