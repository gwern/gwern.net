/*******************************************/
/*  Events fired by extracts-annotations.js:

    GW.contentDidLoad {
            source: "Extracts.annotationForTarget"
            document:
                A DocumentFragment containing the constructed annotation.
            location:
                URL of the annotated target (NOT the URL of the annotation
                resource!).
            flags:
                GW.contentDidLoadEventFlags.needsRewrite
        }
        Fired when the content of the annotation pop-frame (i.e., the
        annotation) has been constructed, but not yet injected into a pop-frame.

        (See rewrite.js for more information about the keys and values of the
         GW.contentDidLoad event.)

    GW.contentDidLoad {
            source: "Extracts.rewritePopFrameContent_ANNOTATION"
            document:
                The documentElement of the annotation pop-frame.
            location:
                URL of the annotated target (NOT the URL of the annotation
                resource!).
            flags:
                0 (no flags set)
        }
        Fired when an annotation pop-frame has been filled with content (i.e.,
        the annotation), at the last stage of preparing the pop-frame for
        spawning (being injected into the page and positioned).

        (See rewrite.js for more information about the keys and values of the
         GW.contentDidLoad event.)
 */

/*=-------------=*/
/*= ANNOTATIONS =*/
/*=-------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "ANNOTATION",               // Type name
    "isAnnotatedLink",          // Type predicate function
    (target) =>           		// Target classes to add
    	((   target.classList.contains("link-annotated-partial")
    	  && !(Annotations.isWikipediaArticleLink(Extracts.targetIdentifier(target))))
    	 ? "has-annotation-partial"
    	 : "has-annotation"),
    "annotationForTarget",      // Pop-frame fill function
    "annotation"                // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts, ...{
    //  Used in: Extracts.setUpAnnotationLoadEventWithin
    annotatedTargetSelectors: [ "a.link-annotated" ],

    //  Constructed annotations.
    cachedAnnotations: { },

    //  Called by: extracts.js (as `predicateFunctionName`)
    //  Called by: extracts.js
    //  Called by: extracts-content.js
    isAnnotatedLink: (target) => {
        return target.classList.contains("link-annotated");
    },

    /*  This “special testing function” is used to exclude certain targets which
        have already been categorized as (in this case) `LOCAL_PAGE` targets. It
        returns false if the target is to be excluded, true otherwise. Excluded
        targets will not spawn pop-frames.
     */
    //  Called by: Extracts.targets.testTarget (as `testTarget_${targetTypeInfo.typeName}`)
    testTarget_ANNOTATION: (target) => {
        return (!(   Extracts.popFrameProvider == Popins
                  && (   Extracts.isTOCLink(target)
                      || Extracts.isSidebarLink(target))));
    },

    /*  An annotation for a link.
        */
    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    annotationForTarget: (target) => {
        GWLog("Extracts.annotationForTarget", "extracts-annotations.js", 2);

        let annotationIdentifier = Extracts.targetIdentifier(target);

        //  Use cached constructed annotation, if available.
        if (Extracts.cachedAnnotations[annotationIdentifier])
            return Extracts.newDocument(Extracts.cachedAnnotations[annotationIdentifier]);

        //  Get annotation reference data (if available).
        let referenceData = Annotations.referenceDataForAnnotationIdentifier(annotationIdentifier);

        //  Wikipedia (external) annotations get special treatment.
        let isWikipediaLink = Annotations.isWikipediaArticleLink(annotationIdentifier);

        //  Check whether the annotation reference data is loaded.
        if (referenceData == null) {
            /*  If the annotation has yet to be loaded, we’ll ask for it to load,
                and meanwhile wait, and do nothing yet.
             */
            Extracts.refreshPopFrameAfterAnnotationDataLoads(target);

            return Extracts.newDocument();
        } else if (referenceData == "LOADING_FAILED") {
            /*  If we’ve already tried and failed to load the annotation, we
                will not try loading again, and just show the “loading failed”
                message.
             */
            target.popFrame.classList.add("loading-failed");

            return Extracts.newDocument();
        }

        //  Open link in same window on mobile, new window on desktop.
        let linkTarget = (Extracts.popFrameProvider == Popins) ? "_self" : "_blank";

        //  Link to original URL (for archive links).
        let originalLinkHTML = "";
        if (   referenceData.element
            && referenceData.element.dataset.urlOriginal != undefined
            && referenceData.element.dataset.urlOriginal != target.href) {
            let originalURLText = referenceData.element.dataset.urlOriginal.includes("ar5iv") ? `<span class="smallcaps">HTML</span>` : "live";
            originalLinkHTML = ` <span class="originalURL">[<a
                            title="Link to original URL for ${referenceData.element.textContent}"
                            href="${referenceData.element.dataset.urlOriginal}"
                            target="${linkTarget}"
                            alt="Original URL for this archived link; may be broken."
                                >` + originalURLText + `</a>]</span>`;
        }

        //  Extract title/link.
        let titleLinkClass = (originalLinkHTML > ""
                              ? `title-link local-archive-link`
                              : (isWikipediaLink
                                 ? `title-link link-live`
                                 : `title-link`));
        //	Import certain link classes from target.
        /*	Just ‘link-live’ for now, but the inclusion rule is: any class that
        	is used to test whether a link is of a certain type - see e.g.
        	Extracts.isForeignSiteLink() in extracts-content.js - for which link
        	type there can be annotations (so not, e.g., ‘footnote-ref’, because
        	there’s no such thing as an annotated footnote link). This way, the
        	title-link of the popup will correctly test as the *un-annotated*
        	link type of the original target.
        	—SA 2022-06-13
         */
        [ "link-live" ].forEach(targetClass => {
        	if (target.classList.contains(targetClass))
        		titleLinkClass += ` ${targetClass}`;
        });
        let titleLinkIconMetadata = (isWikipediaLink
                                     ? `data-link-icon-type="svg" data-link-icon="wikipedia"`
                                     : ``);
        let titleLinkHTML = `<a
                                class="${titleLinkClass}"
                                title="Open ${target.href} in a new window"
                                href="${target.href}"
                                target="${linkTarget}"
                                ${titleLinkIconMetadata}
                                    >${referenceData.titleHTML}</a>`;

        //  Similars, backlinks, tags.
        let auxLinks = [ ];
        if (referenceData.backlinksHTML == ``) {
            if (referenceData.tagsHTML > ``)
                auxLinks.push(`<span class="data-field link-tags">${referenceData.tagsHTML}</span>`);
        } else {
            if (referenceData.tagsHTML > ``)
                auxLinks.push(referenceData.tagsHTML);

            auxLinks.push(referenceData.backlinksHTML);
        }
        if (referenceData.similarHTML) {
			auxLinks.push(referenceData.similarHTML);
		}
		auxLinks = auxLinks.join("; ");

		//	Special class for the abstract for certain annotation sources.
        let abstractSpecialClass = ``;
        if (isWikipediaLink)
            abstractSpecialClass = "wikipedia-entry";

        //  The fully constructed annotation pop-frame contents.
        let authorDateAuxSeparator = (referenceData.authorHTML || referenceData.dateHTML) ? "; " : "";
        let constructedAnnotation = Extracts.newDocument(
              `<p class="data-field title">${titleLinkHTML}${originalLinkHTML}</p>`
            /*  Suppress the author block in WP popups; we have nothing more
                useful to say than ‘Wikipedia’ (even if we grabbed the
                last-revision-time from WP, that’s usually just a trivial bot
                edit and isn’t a ‘real’ author date), and the fact that it’s WP
                is already denoted by the dotted underline, ‘W’ icon on the
                title link, lack of the standard metadata block which non-WP
                annotations have (author/date/tag/backlinks/similar-links), and
                the encyclopedic topic & tone. Putting ‘Wikipedia’ on an entire
                line by itself is just a waste of precious popup vertical space.
             */
            + (isWikipediaLink
               ? ``
               : `<p class="data-field author-date-aux">${referenceData.authorHTML}${referenceData.dateHTML}${authorDateAuxSeparator}${auxLinks}</p>`)
            + `<div class="data-field annotation-abstract ${abstractSpecialClass}"></div>`);
        if (referenceData.abstract)
	        constructedAnnotation.querySelector(".annotation-abstract").appendChild(referenceData.abstract);

        //  Fire contentDidLoad event.
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "Extracts.annotationForTarget",
            document: constructedAnnotation,
            location: Extracts.locationForTarget(target),
            flags: GW.contentDidLoadEventFlags.needsRewrite
        });

        //  Cache constructed and processed annotation.
        Extracts.cachedAnnotations[annotationIdentifier] = constructedAnnotation;

        return Extracts.newDocument(constructedAnnotation);
    },

    //  Called by: extracts.js (as `titleForPopFrame_${targetTypeName}`)
    titleForPopFrame_ANNOTATION: (popFrame) => {
        GWLog("Extracts.titleForPopFrame_ANNOTATION", "extracts-annotations.js", 2);

        let target = popFrame.spawningTarget;

        let popFrameTitleText = Extracts.popFrameHasLoaded(popFrame)
                                ? popFrame.body.querySelector(".data-field.title").textContent
                                : (Annotations.isWikipediaArticleLink(Extracts.targetIdentifier(target))
                                   ? target.href
                                   : target.pathname + target.hash);

        //  Sections.
        if (   target.hash > ""
            //  For sections of local pages, mark with ‘§’ symbol.
            && (    target.hostname == location.hostname
                /*  Annotations for local archive links with an org notation
                    for link icons (eg. 'https://arxiv.org/abs/2006.07159#google')
                    should not get a section mark.
                 */
                    && !(["adobe", "alibaba", "allen", "amazon", "anthropic", "apple", "baidu", "bair", "bytedance",
                          "cerebras", "deepmind", "eleutherai", "elementai", "facebook", "flickr",
                          "github", "google", "googledeepmind", "google-graphcore", "graphcore", "huawei", "intel", "jd", "kako", "laion",
                          "lighton", "microsoft", "microsoftnvidia", "miri", "naver",
                          "nvidia", "openai", "pdf", "salesforce", "sberbank", "sensetime",
                          "snapchat", "spotify", "tencent", "tensorfork", "uber", "yandex"
                      ].includes(target.hash.slice(1))))) {
            popFrameTitleText = "&#x00a7; " + popFrameTitleText;
        } else if (   target.hash > ""
                   && Annotations.isWikipediaArticleLink(Extracts.targetIdentifier(target))
                   && Extracts.popFrameHasLoaded(popFrame)) {
            /*  For Wikipedia, show the page title and the section title,
                separated by the ‘§’ symbol.
             */
            let referenceData = Annotations.referenceDataForAnnotationIdentifier(Extracts.targetIdentifier(popFrame.spawningTarget));
            popFrameTitleText =   referenceData.articleTitle
                                + " &#x00a7; "
                                + popFrameTitleText;
        }


        if (target.dataset.urlOriginal) {
            //  Open link in same window on mobile, new window on desktop.
            let linkTarget = (Extracts.popFrameProvider == Popins) ? "_self" : "_blank";

            //  For local-archive links, include archive link with original.
            return `[<a
                    class="popframe-title-link-archived"
                    title="Open ${target.href} in a new window (desktop) or current (mobile)"
                    href="${target.href}"
                    target="${linkTarget}"
                        >ARCHIVED</a>]` +
                `<span class="separator">·</span>` +
                `<a
                    class="popframe-title-link"
                    title="Open ${target.dataset.urlOriginal} in a new window (desktop) or current (mobile)"
                    href="${target.dataset.urlOriginal}"
                    target="${linkTarget}"
                        >${popFrameTitleText.replace(/^\[original\]/, "")}</a>`;
        } else {
            return Extracts.standardPopFrameTitleElementForTarget(target, popFrameTitleText);
        }
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_ANNOTATION: (popup) => {
        let target = popup.spawningTarget;

        /*  Do not spawn annotation popup if the annotation is already visible
            on screen. (This may occur if the target is in a popup that was
            spawned from a backlinks popup for this same annotation as viewed on
            a tag index page, for example.)
         */
        let targetAnalogueInLinkBibliography = document.querySelector(`a[id^='linkBibliography'][href='` + CSS.escape(target.href) + `']`);
        if (targetAnalogueInLinkBibliography) {
            let containingSection = targetAnalogueInLinkBibliography.closest("section");
            if (   containingSection
                && Popups.isVisible(containingSection)) {
                return null;
            }
        }

        return popup;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_ANNOTATION: (popFrame) => {
        GWLog("Extracts.rewritePopFrameContent_ANNOTATION", "extracts-annotations.js", 2);

        let target = popFrame.spawningTarget;

        //  Mark Wikipedia entries.
        if (popFrame.body.querySelector(".annotation-abstract").classList.contains("wikipedia-entry"))
            Extracts.popFrameProvider.addClassesToPopFrame(popFrame, "wikipedia-entry");

        /*  Allow for floated figures at the start of abstract
            (only on sufficiently wide viewports).
            */
        if (!(GW.mediaQueries.mobileWidth.matches)) {
            let initialFigure = popFrame.body.querySelector(".annotation-abstract > figure.float-right:first-child");
            if (initialFigure)
                popFrame.body.insertBefore(initialFigure, popFrame.body.firstElementChild);
        }

        //  Fire contentDidLoad event.
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "Extracts.rewritePopFrameContent_ANNOTATION",
            document: popFrame.documentElement,
            location: Extracts.locationForTarget(target),
            flags: 0
        });
    },

    /*=----------------------=*/
    /*= ANNOTATIONS: HELPERS =*/
    /*=----------------------=*/

    annotationLoadHoverDelay: 25,

    //  Called by: extracts.js
    //  Called by: extracts-options.js
    setUpAnnotationLoadEventWithin: (container) => {
        GWLog("Extracts.setUpAnnotationLoadEventWithin", "extracts-annotations.js", 1);

        //  Get all the annotated targets in the container.
        let allAnnotatedTargetsInContainer = Array.from(container.querySelectorAll(Extracts.annotatedTargetSelectors.join(", ")));

        if (Extracts.popFrameProvider == Popups) {
            //  Add hover event listeners to all the annotated targets.
            allAnnotatedTargetsInContainer.forEach(annotatedTarget => {
                annotatedTarget.removeAnnotationLoadEvents = onEventAfterDelayDo(annotatedTarget, "mouseenter", Extracts.annotationLoadHoverDelay, (event) => {
                    //  Get the unique identifier of the annotation for the target.
                    let annotationIdentifier = Extracts.targetIdentifier(annotatedTarget);

                    //  Do nothing if the annotation is already loaded.
                    if (Annotations.cachedAnnotationExists(annotationIdentifier))
                        return;

                    //  Otherwise, load the annotation.
                    Annotations.loadAnnotation(annotationIdentifier);
                }, "mouseleave");
            });

            /*  Set up handler to remove hover event listeners from all
                the annotated targets in the document.
                */
            GW.notificationCenter.addHandlerForEvent("Extracts.cleanupDidComplete", (info) => {
                allAnnotatedTargetsInContainer.forEach(annotatedTarget => {
                    annotatedTarget.removeAnnotationLoadEvents();
                    annotatedTarget.removeAnnotationLoadEvents = null;
                });
            }, { once: true });
        } else { // if (Extracts.popFrameProvider == Popins)
            //  Add click event listeners to all the annotated targets.
            allAnnotatedTargetsInContainer.forEach(annotatedTarget => {
                annotatedTarget.addEventListener("click", annotatedTarget.annotationLoad_click = (event) => {
                    //  Get the unique identifier of the annotation for the target.
                    let annotationIdentifier = Extracts.targetIdentifier(annotatedTarget);

                    //  Do nothing if the annotation is already loaded.
                    if (!Annotations.cachedAnnotationExists(annotationIdentifier))
                        Annotations.loadAnnotation(annotationIdentifier);
                });
            });

            /*  Set up handler to remove click event listeners from all
                the annotated targets in the document.
                */
            GW.notificationCenter.addHandlerForEvent("Extracts.cleanupDidComplete", (info) => {
                allAnnotatedTargetsInContainer.forEach(annotatedTarget => {
                    annotatedTarget.removeEventListener("click", annotatedTarget.annotationLoad_click);
                });
            }, { once: true });
        }
    },

    /*  Refresh (respawn or reload) a pop-frame for an annotated target after
        its annotation loads.
        */
    //  Called by: Extracts.annotationForTarget
    refreshPopFrameAfterAnnotationDataLoads: (target) => {
        GWLog("Extracts.refreshPopFrameAfterAnnotationDataLoads", "extracts-annotations.js", 2);

        target.popFrame.classList.toggle("loading", true);

        /*  We set up an event handler for when the annotation loads, and
            respawn the popup / re-inject the popin, after it spawns (if it
            hasn’t de-spawned already, eg. if the user moused out of the
            target).
            */
        GW.notificationCenter.addHandlerForEvent("Annotations.annotationDidLoad", target.refreshPopFrameWhenAnnotationDidLoad = (info) => {
            GWLog("refreshPopFrameWhenAnnotationDidLoad", "extracts.js", 2);

            //  If the pop-frame has despawned, don’t respawn it.
            if (Extracts.popFrameProvider.isSpawned(target.popFrame) == false)
                return;

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
        }, { once: true, condition: (info) => info.identifier == Extracts.targetIdentifier(target) });

        //  Add handler for if the annotation load fails.
        GW.notificationCenter.addHandlerForEvent("Annotations.annotationLoadDidFail", target.updatePopFrameWhenAnnotationLoadDidFail = (info) => {
            GWLog("updatePopFrameWhenAnnotationLoadDidFail", "extracts.js", 2);

            //  If the pop-frame has despawned, don’t respawn it.
            if (Extracts.popFrameProvider.isSpawned(target.popFrame) == false)
                return;

            target.popFrame.swapClasses([ "loading", "loading-failed" ], 1);
        }, { once: true, condition: (info) => info.identifier == Extracts.targetIdentifier(target) });
    }
}};
