/*=-------------=*/
/*= ANNOTATIONS =*/
/*=-------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "ANNOTATION",               // Type name
    "isAnnotatedLink",          // Type predicate function
    (target) =>                 // Target classes to add
        ((   Annotations.isAnnotatedLinkPartial(target)
          && Annotations.dataSourceForTarget(target) == Annotations.dataSources.local)
         ? "has-annotation-partial"
         : "has-annotation"),
    "annotationForTarget",      // Pop-frame fill function
    "annotation"                // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    //  Called by: extracts.js
    //  Called by: extracts-content.js
    isAnnotatedLink: (target) => {
        return Annotations.isAnnotatedLink(target);
    },

    /*  This “special testing function” is used to exclude certain targets which
        have already been categorized as (in this case) `ANNOTATION` targets. It
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

		return newDocument(synthesizeIncludeLink(target, {
			"class": "link-annotated include-annotation include-strict",
			"data-template": "annotation-blockquote-not",
			"data-template-fields": "linkTarget:$",
			"data-link-target": ((Extracts.popFrameProvider == Popins) ? "_self" : "_blank")
		}));
    },

    //  Called by: extracts.js (as `titleForPopFrame_${targetTypeName}`)
    titleForPopFrame_ANNOTATION: (popFrame) => {
        GWLog("Extracts.titleForPopFrame_ANNOTATION", "extracts-annotations.js", 2);

        let target = popFrame.spawningTarget;
		let referenceData = Annotations.referenceDataForTarget(target);
		if (referenceData == null) {
			referenceData = {
				titleLinkHref:     target.href,
				originalURL:       (target.dataset.urlOriginal ?? null),
				popFrameTitleText: (target.hostname == location.hostname
									? target.pathname + target.hash
									: target.href)
			};
		}

		return Transclude.fillTemplateNamed("pop-frame-title-annotation", referenceData, {
			linkTarget:   ((Extracts.popFrameProvider == Popins) ? "_self" : "_blank"),
			whichTab:     ((Extracts.popFrameProvider == Popins) ? "current" : "new"),
			tabOrWindow:  (GW.isMobile() ? "tab" : "window")
		}).innerHTML;
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_ANNOTATION: (popup) => {
        let target = popup.spawningTarget;

        /*  Do not spawn annotation popup if the annotation is already visible
            on screen. (This may occur if the target is in a popup that was
            spawned from a backlinks popup for this same annotation as viewed on
            a tag index page, for example.)
         */
        let escapedLinkURL = CSS.escape(decodeURIComponent(target.href));
        let targetAnalogueInLinkBibliography = document.querySelector(`a[id^='link-bibliography'][href='${escapedLinkURL}']`);
        if (targetAnalogueInLinkBibliography) {
            let containingSection = targetAnalogueInLinkBibliography.closest("section");
            if (   containingSection
                && containingSection.querySelector("blockquote")
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
		let referenceData = Annotations.referenceDataForTarget(target)

        //  Mark annotations from non-local data sources.
        if (   referenceData
        	&& referenceData.dataSourceClass)
            Extracts.popFrameProvider.addClassesToPopFrame(popFrame, referenceData.dataSourceClass.split(" "));

        //  Fire contentDidLoad event (to trigger transclude).
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "Extracts.rewritePopFrameContent_ANNOTATION",
            container: popFrame.body,
            document: popFrame.document
        });
    },

    /*=----------------------=*/
    /*= ANNOTATIONS: HELPERS =*/
    /*=----------------------=*/

    annotationLoadHoverDelay: 25,

    //  Called by: extracts.js
    setUpAnnotationLoadEventsWithin: (container) => {
        GWLog("Extracts.setUpAnnotationLoadEventsWithin", "extracts-annotations.js", 1);

        //  Get all the annotated targets in the container.
        let allAnnotatedTargetsInContainer = Annotations.allAnnotatedLinksInContainer(container);

        if (Extracts.popFrameProvider == Popups) {
            //  Add hover event listeners to all the annotated targets.
            allAnnotatedTargetsInContainer.forEach(annotatedTarget => {
                annotatedTarget.removeAnnotationLoadEvents = onEventAfterDelayDo(annotatedTarget, "mouseenter", Extracts.annotationLoadHoverDelay, (event) => {
                    //  Get the unique identifier of the annotation for the target.
                    let annotationIdentifier = Annotations.targetIdentifier(annotatedTarget);

                    //  Do nothing if the annotation is already loaded.
                    if (Annotations.cachedDataExists(annotationIdentifier) == false)
                        Annotations.load(annotationIdentifier);
                }, "mouseleave");
            });

			if (allAnnotatedTargetsInContainer.length > 0) {
				/*  Set up handler to remove hover event listeners from all
					the annotated targets in the document.
					*/
				GW.notificationCenter.addHandlerForEvent("Extracts.cleanupDidComplete", (info) => {
					allAnnotatedTargetsInContainer.forEach(annotatedTarget => {
						annotatedTarget.removeAnnotationLoadEvents();
						annotatedTarget.removeAnnotationLoadEvents = null;
					});
				}, { once: true });
            }
        } else { // if (Extracts.popFrameProvider == Popins)
            //  Add click event listeners to all the annotated targets.
            allAnnotatedTargetsInContainer.forEach(annotatedTarget => {
                annotatedTarget.addEventListener("click", annotatedTarget.annotationLoad_click = (event) => {
                    //  Get the unique identifier of the annotation for the target.
                    let annotationIdentifier = Annotations.targetIdentifier(annotatedTarget);

                    //  Do nothing if the annotation is already loaded.
                    if (Annotations.cachedDataExists(annotationIdentifier) == false)
                        Annotations.load(annotationIdentifier);
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
    }
};
