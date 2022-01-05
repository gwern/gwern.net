if (window.Extracts) {
    /*=-------------=*/
    /*= ANNOTATIONS =*/
    /*=-------------=*/

    Extracts.targetTypeDefinitions.insertBefore([
        "ANNOTATION",
        "isAnnotatedLink",
        "has-annotation",
        "annotationForTarget",
        "annotation"
    ], (def => def[0] == "LOCAL_PAGE"));

    Extracts.annotatedTargetSelectors = [ "a.docMetadata" ];

    Extracts.isAnnotatedLink = (target) => {
        return target.classList.contains("docMetadata");
    };

    //  An annotation for a link.
    Extracts.annotationForTarget = (target) => {
        GWLog("Extracts.annotationForTarget", "extracts-annotations.js", 2);

        let annotationIdentifier = Extracts.targetIdentifier(target);

        if (Annotations.annotationForIdentifier(annotationIdentifier) == null) {
            Extracts.refreshPopFrameAfterAnnotationLoads(target);
            return `&nbsp;`;
        } else if (Annotations.annotationForIdentifier(annotationIdentifier) == "LOADING_FAILED") {
            target.popFrame.classList.add("loading-failed");
            return `&nbsp;`;
        }

        let referenceData = Annotations.referenceDataForAnnotationIdentifier(annotationIdentifier);

        //  Open link in same window on mobile, new window on desktop.
        let linkTarget = (Extracts.popFrameProvider == Popins) ? "_self" : "_blank";

        //  Link to original URL (for archive links).
        let originalLinkHTML = "";
        if (   referenceData.element.dataset.urlOriginal != undefined
            && referenceData.element.dataset.urlOriginal != target.href) {
            originalLinkHTML = `<span class="originalURL">[<a
                            title="Link to original URL for ‘${referenceData.element.textContent}’"
                            href="${referenceData.element.dataset.urlOriginal}"
                            target="${linkTarget}"
                            alt="Original URL for this archived link; may be broken."
                                >live</a>]</span>`;
        }

        //  Extract title/link.
        let titleLinkClass = (originalLinkHTML > "" ? `title-link local-archive-link` : `title-link`);
        let titleLinkHTML = `<a
                                class="${titleLinkClass}"
                                title="Open ${target.href} in a new window"
                                href="${target.href}"
                                target="${linkTarget}"
                                    >${referenceData.titleHTML}</a>`;

        let similarLinksHtml = referenceData.similarHTML == `` ? `` : `; ${referenceData.similarHTML}`;

        let tagBacklinks = `${similarLinksHtml}</p>`;
        if (referenceData.tagsHTML == `` && referenceData.backlinksHTML == ``) { tagBacklinks = `${similarLinksHtml}</p>`; } else {
            if (referenceData.tagsHTML != `` && referenceData.backlinksHTML == ``) { tagBacklinks = `; <span class="data-field link-tags">${referenceData.tagsHTML}${similarLinksHtml}</p>`; } else {
                if (referenceData.tagsHTML == `` && referenceData.backlinksHTML != ``) { tagBacklinks = `; ${referenceData.backlinksHTML}${similarLinksHtml}</p>`; } else {
                    if (referenceData.tagsHTML != `` && referenceData.backlinksHTML != ``) { tagBacklinks = `; ${referenceData.tagsHTML}; ${referenceData.backlinksHTML}${similarLinksHtml}</p>`; }
                }
            }
        }

        //  The fully constructed annotation pop-frame contents.
        let abstractSpecialClass = ``;
        if (Annotations.isWikipediaLink(annotationIdentifier))
            abstractSpecialClass = "wikipedia-entry";
        return `<p class="data-field title">${titleLinkHTML}${originalLinkHTML}</p>`
            + `<p class="data-field author-plus-date">${referenceData.authorHTML}${referenceData.dateHTML}`
            + tagBacklinks
            + `<div class="data-field annotation-abstract ${abstractSpecialClass}">${referenceData.abstractHTML}</div>`;
    };

    Extracts.titleForPopFrame_ANNOTATION = (popFrame) => {
        let target = popFrame.spawningTarget;

        let popFrameTitleText = Extracts.popFrameHasLoaded(popFrame)
                                ? popFrame.querySelector(".data-field.title").textContent
                                : (Annotations.isWikipediaLink(Extracts.targetIdentifier(target))
                                   ? target.href
                                   : target.pathname + target.hash);

        //  For sections of local pages, and Wikipedia, mark with ‘§’ symbol.
        if (   target.hash > ""
            && (   (   target.hostname == location.hostname
                       // annotations for local archive links with an org notation for link icons (eg. ‘https://www.gwern.net/docs/ai/2020-bell.pdf#facebook') should not get a section mark
                    && !([ "alibaba", "allen", "amazon", "baidu", "deepmind", "eleutherai", "facebook", "google", "googlebrain", "lighton", "microsoft", "miri", "nvidia", "openai", "pdf", "salesforce", "tencent", "tensorfork", "uber", "yandex"
                           ].includes(target.hash)))
                || Annotations.isWikipediaLink(Extracts.targetIdentifier(target))))
            popFrameTitleText = "&#x00a7; " + popFrameTitleText;

        if (target.dataset.urlOriginal) {
            //  Open link in same window on mobile, new window on desktop.
            let linkTarget = (Extracts.popFrameProvider == Popins) ? "_self" : "_blank";

            //  For local-archive links, include archive link with original.
            return `<a
                    class="popframe-title-link-archived"
                    title="Open ${target.href} in a new window (desktop) or current (mobile)"
                    href="${target.href}"
                    target="${linkTarget}"
                        >[ARCHIVED]</a>` +
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
    };

    Extracts.rewritePopFrameContent_ANNOTATION = (popFrame) => {
        let target = popFrame.spawningTarget;

        //  Mark Wikipedia entries.
        if (popFrame.querySelector(".annotation-abstract").classList.contains("wikipedia-entry"))
            popFrame.contentView.classList.add("wikipedia-entry");

        //  Qualify internal links.
        if (target.hostname == location.hostname)
            Extracts.qualifyLinksInPopFrame(popFrame);

        /*  Allow for floated figures at the start of abstract
            (only on sufficiently wide viewports).
            */
        if (!(GW.mediaQueries.mobileWidth.matches)) {
            let initialFigure = popFrame.querySelector(".annotation-abstract > figure.float-right:first-child");
            if (initialFigure)
                popFrame.contentView.insertBefore(initialFigure, popFrame.contentView.firstElementChild);
        }

        //  Fire contentDidLoad event.
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "Extracts.rewritePopFrameContent_ANNOTATION",
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
    };

    /*=----------------------=*/
    /*= ANNOTATIONS: HELPERS =*/
    /*=----------------------=*/

    Extracts.annotationLoadHoverDelay = 25;

    Extracts.setUpAnnotationLoadEventWithin = (container) => {
        GWLog("Extracts.setUpAnnotationLoadEventWithin", "extracts-annotations.js", 1);

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
            GW.notificationCenter.addHandlerForEvent("Extracts.cleanupDidComplete", () => {
                allAnnotatedTargetsInContainer.forEach(annotatedTarget => {
                    annotatedTarget.removeEventListener("click", annotatedTarget.annotationLoad_click);
                });
            }, { once: true });
        }
    };

    /*  Refresh (respawn or reload) a pop-frame for an annotated target after
        its annotation (fragment) loads.
        */
    Extracts.refreshPopFrameAfterAnnotationLoads = (target) => {
        GWLog("Extracts.refreshPopFrameAfterAnnotationLoads", "extracts-annotations.js", 2);

        target.popFrame.classList.toggle("loading", true);

        /*  We set up an event handler for when the fragment loads, and respawn
            the popup / re-inject the popin, after it spawns (if it
            hasn’t de-spawned already, eg. if the user moused out of the
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

                requestAnimationFrame(() => {
                    Popins.scrollPopinIntoView(target.popin);
                });
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
    };
}
