// popups.js: standalone Javascript library for creating 'popups' which display link metadata (typically, title/author/date/summary), for extremely convenient reference/abstract reading.
// Author: Said Achmiz, Shawn Presser (mobile & Youtube support)
// Date: 2019-09-12
// When:
// license: MIT (derivative of footnotes.js, which is PD)

// popups.js parses a HTML document and looks for <a> links which have the 'docMetadata' attribute class, and the attributes 'data-popup-title', 'data-popup-author', 'data-popup-date', 'data-popup-doi', 'data-popup-abstract'.
// (These attributes are expected to be populated already by the HTML document's compiler, however, they can also be done dynamically. See '/static/js/wikipedia-popups.js' for an example of a library which does Wikipedia-only dynamically on page loads.)

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
            ".sidenote-self-link"
        ].join(", "),
        excludedContainerElementsSelector: "h1, h2, h3, h4, h5, h6",
        testTarget: (target) => {
            let targetTypeInfo = Extracts.targetTypeInfo(target);
            if (targetTypeInfo) {
                let specialTestFunction = Extracts[`testTarget_${targetTypeInfo.typeName}`]
                if (specialTestFunction && specialTestFunction(target) == false)
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

    /*  Misc. configuration.
        */
    server404PageTitles: [
        "404 Not Found"
    ],

    pageTitleRegexp: /^(.+?) · Gwern\.net$/,

    rootDocument: document.firstElementChild,

    /******************/
    /*  Infrastructure.
        */

    popFrameProviderName: null,
    popFrameProvider: null,

    /***********/
    /*  General.
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
            if (Extracts.popupOptionsEnabled)
                Extracts.removePopupsDisabledShowPopupOptionsDialogButton();
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

            GWLog("Activating popins.", "extracts.js", 1);
        }

        /*  Add handler to set up targets in loaded content (including
            newly-spawned pop-frames; this allows for recursion), and to
            add hover/click event listeners to annotated targets, to load
            annotations (fragments).
            */
        GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", Extracts.processTargetsOnContentLoad = (info) => {
            GWLog("Extracts.processTargetsOnContentLoad", "extracts.js", 2);

            Extracts.processTargetsInDocument(info.document);
        }, { phase: "eventListeners" });

        //  Fire setup-complete event.
        GW.notificationCenter.fireEvent("Extracts.setupDidComplete");
    },

    processTargetsInDocument: (doc = Extracts.rootDocument) => {
        GWLog("Extracts.processTargetsInDocument", "extracts.js", 2);

        if (doc.closest(Extracts.contentContainersSelector)) {
            Extracts.addTargetsWithin(doc);
            Extracts.setUpAnnotationLoadEventWithin(doc);
        } else {
            doc.querySelectorAll(Extracts.contentContainersSelector).forEach(container => {
                Extracts.addTargetsWithin(container);
                Extracts.setUpAnnotationLoadEventWithin(container);
            });
        }
    },

    /***********/
    /*  Content.
        */

    /*  This array defines the types of ‘targets’ (ie. annotated links,
        links pointing to available content such as images or code files,
        citations, etc.) that Extracts supports.
        */
    targetTypeDefinitions: [
        [ "LOCAL_PAGE",         "isLocalPageLink",      "has-content",      "localTranscludeForTarget",     "local-transclude"      ],
    ],

    /*  Returns full type info for the given target. This contains the target
        type name, the name of the predicate function for identifying targets of
        that type (eg. isAnnotatedLink), classes which should be applied to
        targets of that type during initial processing, the fill functions to
        fill popups and popins of that type, and the classes which should be
        applied to pop-frames of that type.
        */
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
    targetIdentifier: (target) => {
        return    target.dataset.urlOriginal
               || (target.hostname == location.hostname
                   ? target.pathname + target.hash
                   : target.href);
    },

    /*  Returns true if the two targets will spawn identical popups
        (that is, if they are of the same type, and have the same identifiers).
        */
    targetsMatch: (targetA, targetB) => {
        return    Extracts.targetIdentifier(targetA) == Extracts.targetIdentifier(targetB)
               && Extracts.targetTypeInfo(targetA).typeName == Extracts.targetTypeInfo(targetB).typeName;
    },

    /*  This function qualifies anchorlinks in transcluded content (ie. other
        pages on the site, as well as annotations describing other pages on the
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

    /*  This function fills a pop-frame for a given target with content. It
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

    standardPopFrameTitleElementForTarget: (target, titleText) => {
        if (typeof titleText == "undefined")
            titleText = (target.hostname == location.hostname)
                        ? target.pathname + target.hash
                        : target.href;

        /*	Because tab-handling is bad on mobile, readers expect the original
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

    //  Returns the contents of the title element for a pop-frame.
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
    targetDocument: (target) => {
        if (target.hostname != location.hostname)
            return null;

        if (target.pathname == location.pathname)
            return Extracts.rootDocument;

        if (Extracts.popFrameProvider == Popups) {
            let popupForTargetDocument = Popups.allSpawnedPopups().find(popup => (   popup.classList.contains("external-page-embed")
                                                                                  && popup.spawningTarget.pathname == target.pathname));
            return popupForTargetDocument ? popupForTargetDocument.contentView : null;
        } else if (Extracts.popFrameProvider == Popins) {
            let popinForTargetDocument = Popins.allSpawnedPopins().find(popin => (   popin.classList.contains("external-page-embed")
                                                                                  && popin.spawningTarget.pathname == target.pathname)
                                                                                  && Extracts.popFrameHasLoaded(popin));
            return popinForTargetDocument ? popinForTargetDocument.contentView : null;
        }
    },

    /*  Returns the location (a URL object) of the document for a given target.
        */
    locationForTarget: (target) => {
        return new URL(target.href);
    },

    /*  Activate loading spinner for an object pop-frame.
        */
    setLoadingSpinner: (popFrame) => {
        let target = popFrame.spawningTarget;

        popFrame.classList.toggle("loading", true);

        //  When loading ends (in success or failure)...
        let objectOfSomeSort = popFrame.querySelector("iframe, object, img, video");
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

    //  Local links (to sections of the current page, or other site pages).
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

    localTranscludeForTarget: (target, unwrapFunction) => {
        GWLog("Extracts.localTranscludeForTarget", "extracts.js", 2);

        if (unwrapFunction == null)
            unwrapFunction = (blockElement) => {
                if (blockElement.tagName == "SECTION") {
                    return blockElement.innerHTML;
                } else {
                    return blockElement.outerHTML;
                }
            };

        /*  Check to see if the target location matches an already-displayed
            page (which can be the root page of the window).
            */
        let fullTargetDocument = Extracts.targetDocument(target);
        if (fullTargetDocument) {
            /*  If it does, display the section. (We know it must be an
                anchorlink because if it were not, the target would not be
                active.)
                */
            return unwrapFunction(Extracts.nearestBlockElement(fullTargetDocument.querySelector(decodeURIComponent(target.hash))));
        } else {
            //  Otherwise, display the entire linked page.
            return Extracts.externalPageEmbedForTarget(target);
        }
    },

    //  TOC links.
    isTOCLink: (target) => {
        return (target.closest("#TOC") != null);
    },

    //  Links in the sidebar.
    isSidebarLink: (target) => {
        return (target.closest("#sidebar") != null);
    },

    testTarget_LOCAL_PAGE: (target) => {
        return (!(   Extracts.popFrameProvider == Popins
                  && (   Extracts.isTOCLink(target)
                      || Extracts.isSidebarLink(target))));
    },

    preparePopup_LOCAL_PAGE: (popup) => {
        let target = popup.spawningTarget;

        //  Designate section links spawned by the TOC (for special styling).
        if (Extracts.isTOCLink(target))
            popup.classList.add("toc-section");

        return popup;
    },

    titleForPopFrame_LOCAL_PAGE: (popFrame) => {
        let target = popFrame.spawningTarget;

        let popFrameTitleText;
        if (target.pathname == location.pathname) {
            //  Sections of the current page.
            let nearestBlockElement = Extracts.nearestBlockElement(document.querySelector(decodeURIComponent(target.hash)));
            popFrameTitleText = nearestBlockElement.tagName == "SECTION"
                                ? nearestBlockElement.firstElementChild.textContent
                                : target.hash;
        } else {
            if (popFrame.classList.contains("external-page-embed")) {
                //  Entire other pages.
                popFrameTitleText = Extracts.cachedPageTitles[target.pathname] || target.pathname;
            } else {
                //  Sections of other pages.
                let nearestBlockElement = Extracts.nearestBlockElement(Extracts.targetDocument(target).querySelector(decodeURIComponent(target.hash)));
                popFrameTitleText = nearestBlockElement.tagName == "SECTION"
                                    ? (nearestBlockElement.firstElementChild.textContent + ` (${Extracts.cachedPageTitles[target.pathname] || target.pathname})`)
                                    : (target.pathname + target.hash);
            }
        }

        //  Mark sections with ‘§’ symbol.
        if (target.hash > "" && !popFrame.classList.contains("external-page-embed" &&
            // links with an org notation for link icons (eg. 'https://arxiv.org/abs/2006.07159#google') should not get a section mark
            !["alibaba", "allen", "amazon", "baidu", "deepmind", "eleutherai", "facebook", "google", "googlebrain", "lighton", "microsoft", "miri", "nvidia", "openai", "pdf", "salesforce", "tencent", "tensorfork", "uber", "yandex"].includes(target.hash)))
            popFrameTitleText = "&#x00a7; " + popFrameTitleText;

        return Extracts.standardPopFrameTitleElementForTarget(target, popFrameTitleText);
    },

    rewritePopFrameContent_LOCAL_PAGE: (popFrame) => {
        let target = popFrame.spawningTarget;

        //  Qualify internal links in the pop-frame.
        Extracts.qualifyLinksInPopFrame(target.popFrame);

        //  Rectify margin note style.
        popFrame.querySelectorAll(".marginnote").forEach(marginNote => {
            marginNote.swapClasses([ "inline", "sidenote" ], 0);
        });

        //  Fire a contentDidLoad event.
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "Extracts.rewritePopFrameContent_LOCAL_PAGE",
            document: popFrame.contentView,
            isMainDocument: false,
            needsRewrite: false,
            clickable: false,
            collapseAllowed: false,
            isCollapseBlock: false,
            isFullPage: false,
            location: Extracts.locationForTarget(target),
            fullWidthPossible: false
        });

        //  Scroll to the target.
        if (target.hash > "" && popFrame.classList.contains("external-page-embed"))
            requestAnimationFrame(() => {
                if (popFrame)
                    Extracts.popFrameProvider.scrollElementIntoViewInPopFrame(popFrame.querySelector(decodeURIComponent(target.hash)));
            });
    },

    rewritePopinContent_LOCAL_PAGE: (popin) => {
        Extracts.rewritePopFrameContent_LOCAL_PAGE(popin);

        let target = popin.spawningTarget;

        /*  Make non-popin-spawning anchorlinks scroll popin instead of opening
            normally.
            */
        popin.querySelectorAll("a").forEach(link => {
            if (   link.hostname == target.hostname
                && link.pathname == target.pathname
                && link.hash > ""
                && link.classList.contains("no-popin")) {
                link.onclick = () => { return false; };
                link.addActivateEvent((event) => {
                    let hashTarget = popin.querySelector(decodeURIComponent(link.hash));
                    if (hashTarget) {
                        Popins.scrollElementIntoViewInPopFrame(hashTarget);
                        return false;
                    } else {
                        return true;
                    }
                });
            }
        });
    },

    rewritePopupContent_LOCAL_PAGE: (popup) => {
        Extracts.rewritePopFrameContent_LOCAL_PAGE(popup);

        let target = popup.spawningTarget;

        //  Make anchorlinks scroll popup instead of opening normally.
        popup.querySelectorAll("a").forEach(link => {
            if (   link.hostname == target.hostname
                && link.pathname == target.pathname
                && link.hash > "") {
                link.onclick = () => { return false; };
                link.addActivateEvent((event) => {
                    let hashTarget = popup.querySelector(decodeURIComponent(link.hash));
                    if (hashTarget) {
                        Popups.scrollElementIntoViewInPopFrame(hashTarget);
                        return false;
                    } else {
                        return true;
                    }
                });
            }
        });
    },

    //  Other site pages.
    cachedPages: { },
    cachedPageTitles: { },
    refreshPopFrameAfterLocalPageLoads: (target) => {
        GWLog("Extracts.refreshPopFrameAfterLocalPageLoads", "extracts.js", 2);

        target.popFrame.classList.toggle("loading", true);

        doAjax({
            location: target.href,
            onSuccess: (event) => {
                if (!target.popFrame)
                    return;

                //  Inject the whole page into the pop-frame at first.
                Extracts.popFrameProvider.setPopFrameContent(target.popFrame, event.target.responseText);

                //  The content is the page body plus the metadata block.
                Extracts.cachedPages[target.pathname] = target.popFrame.querySelector("#markdownBody");
                let pageMetadata = target.popFrame.querySelector("#page-metadata");
                if (pageMetadata)
                    Extracts.cachedPages[target.pathname].insertBefore(pageMetadata, Extracts.cachedPages[target.pathname].firstElementChild);

                //  Get the page title.
                Extracts.cachedPageTitles[target.pathname] = target.popFrame.querySelector("title").innerHTML.match(Extracts.pageTitleRegexp)[1];

                /*  Trigger the rewrite pass by firing the requisite event.
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
                    location: Extracts.locationForTarget(target),
                    fullWidthPossible: false
                });

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
            onFailure: (event) => {
                if (!target.popFrame)
                    return;

                target.popFrame.swapClasses([ "loading", "loading-failed" ], 1);
            }
        });
    },
    externalPageEmbedForTarget: (target) => {
        GWLog("Extracts.externalPageEmbedForTarget", "extracts.js", 2);

        //  Mark the pop-frame as an external page embed.
        target.popFrame.classList.add("external-page-embed");

        if (Extracts.cachedPages[target.pathname]) {
            //  Give the pop-frame an identifying class.
            target.popFrame.classList.toggle("external-page-embed", "page-" + target.pathname.substring(1), true);

            return Extracts.cachedPages[target.pathname].innerHTML;
        } else {
            Extracts.refreshPopFrameAfterLocalPageLoads(target);

            return `&nbsp;`;
        }
    },

    /***************************/
    /*  Pop-frames (in general).
        */

    preparePopFrame: (popFrame) => {
        GWLog("Extracts.preparePopFrame", "extracts.js", 2);

        let target = popFrame.spawningTarget;

        //  Import the class(es) of the target.
        popFrame.classList.add(...target.classList);
        //  We then remove some of the imported classes.
        popFrame.classList.remove("has-annotation", "has-content", "link-self", "link-local", "spawns-popup", "spawns-popin");

        //  Add ‘markdownBody’ class.
        popFrame.contentView.classList.add("markdownBody");

        //  Attempt to fill the popup.
        if (Extracts.fillPopFrame(popFrame) == false)
            return null;

        return popFrame;
    },

    /**********/
    /*  Popins.
        */

    /*  Called by popins.js just before injecting the popin. This is our chance
        to fill the popin with content, and rewrite that content in whatever
        ways necessary. After this function exits, the popin will appear on the
        screen.
        */
    preparePopin: (popin) => {
        GWLog("Extracts.preparePopin", "extracts.js", 2);

        let target = popin.spawningTarget;

        //  Call generic prepare function.
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
        let objectOfSomeSort = popin.querySelector("iframe, object, img, video");
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

    popupsEnabled: () => {
        return (localStorage.getItem("extract-popups-disabled") != "true");
    },

    spawnedPopupMatchingTarget: (target) => {
        return Popups.allSpawnedPopups().find(popup =>
                   Extracts.targetsMatch(target, popup.spawningTarget)
                && Popups.popupIsEphemeral(popup));
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

    /*  Called by popups.js just before spawning (injecting and positioning) the
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

        //  Call generic prepare function.
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

    rewritePopupContent: (popup) => {
        GWLog("Extracts.rewritePopupContent", "extracts.js", 2);

        let target = popup.spawningTarget;

        //  Special handling for certain popup types.
        let targetTypeName = Extracts.targetTypeInfo(target).typeName;
        let specialRewriteFunction = Extracts[`rewritePopupContent_${targetTypeName}`] || Extracts[`rewritePopFrameContent_${targetTypeName}`];
        if (specialRewriteFunction)
            specialRewriteFunction(popup);

        //  Ensure no reflow due to figures.
        popup.querySelectorAll("figure[class^='float-'] img[width]").forEach(img => {
            if (img.style.width <= "") {
                img.style.width = img.getAttribute("width") + "px";
                img.style.maxHeight = "unset";
            }
        });
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
