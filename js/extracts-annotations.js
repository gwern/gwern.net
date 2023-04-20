/*=-------------=*/
/*= ANNOTATIONS =*/
/*=-------------=*/

Extracts.targetTypeDefinitions.push([
    "ANNOTATION",               // Type name
    "isAnnotatedLink",          // Type predicate function
    "has-annotation",           // Target classes to add
    "annotationForTarget",      // Pop-frame fill function
    "annotation"                // Pop-frame classes
]);

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    //  Called by: extracts.js
    //  Called by: extracts-content.js
    isAnnotatedLink: (target) => {
        return Annotations.isAnnotatedLinkFull(target);
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
			"class": "link-annotated include-annotation",
			"data-template": "annotation-blockquote-not",
			"data-template-fields": "linkTarget:$,whichTab:$,tabOrWindow:$",
			"data-link-target": ((Extracts.popFrameProvider == Popins) ? "_self" : "_blank"),
			"data-which-tab": ((Extracts.popFrameProvider == Popins) ? "current" : "new"),
			"data-tab-or-window": ((Extracts.popFrameProvider == Popins) ? "tab" : "window")
		}));
    },

    //  Called by: extracts.js (as `titleForPopFrame_${targetTypeName}`)
    titleForPopFrame_ANNOTATION: (popFrame) => {
        GWLog("Extracts.titleForPopFrame_ANNOTATION", "extracts-annotations.js", 2);

        let target = popFrame.spawningTarget;
		let referenceData = Annotations.referenceDataForLink(target);
		if (referenceData == null) {
			referenceData = {
				popFrameTitleLinkHref:          target.href,
				popFrameTitleOriginalLinkHref:  (target.dataset.urlOriginal ?? null),
				popFrameTitleText:              (target.hostname == location.hostname
												 ? target.pathname + target.hash
												 : target.href)
			};
		}

		return Transclude.fillTemplateNamed("pop-frame-title-annotation", referenceData, {
			linkTarget:   ((Extracts.popFrameProvider == Popins) ? "_self" : "_blank"),
			whichTab:     ((Extracts.popFrameProvider == Popins) ? "current" : "new"),
			tabOrWindow:  ((Extracts.popFrameProvider == Popins) ? "tab" : "window")
		});
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
		let referenceData = Annotations.referenceDataForLink(target)

        //  Mark annotations from non-local data sources.
        if (   referenceData
        	&& referenceData.dataSourceClass)
            Extracts.popFrameProvider.addClassesToPopFrame(popFrame, referenceData.dataSourceClass.split(" "));

        //  Trigger transclude.
        Transclude.triggerTranscludesInContainer(popFrame.body, {
            source: "Extracts.rewritePopFrameContent_ANNOTATION",
            container: popFrame.body,
            document: popFrame.document,
            context: "popFrame"
        });
    }
};

/*=-----------------------=*/
/*= ANNOTATIONS (PARTIAL) =*/
/*=-----------------------=*/

Extracts.targetTypeDefinitions.push([
    "ANNOTATION_PARTIAL",            // Type name
    "isPartialAnnotationLink",       // Type predicate function
    "has-annotation-partial",        // Target classes to add
    "partialAnnotationForTarget",    // Pop-frame fill function
    "annotation annotation-partial"  // Pop-frame classes
]);

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    //  Called by: extracts.js
    //  Called by: extracts-content.js
    isPartialAnnotationLink: (target) => {
        return Annotations.isAnnotatedLinkPartial(target);
    },

    /*  This “special testing function” is used to exclude certain targets which
        have already been categorized as (in this case) `ANNOTATION` targets. It
        returns false if the target is to be excluded, true otherwise. Excluded
        targets will not spawn pop-frames.
     */
    //  Called by: Extracts.targets.testTarget (as `testTarget_${targetTypeInfo.typeName}`)
    testTarget_ANNOTATION_PARTIAL: (target) => {
    	return Extracts.testTarget_ANNOTATION(target);
    },

    /*  A partial annotation for a link.
        */
    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    partialAnnotationForTarget: (target) => {
        GWLog("Extracts.partialAnnotationForTarget", "extracts-annotations.js", 2);

		return newDocument(synthesizeIncludeLink(target, {
			"class": "link-annotated-partial include-annotation-partial",
			"data-template": "annotation-blockquote-not",
			"data-template-fields": "linkTarget:$,whichTab:$,tabOrWindow:$",
			"data-link-target": ((Extracts.popFrameProvider == Popins) ? "_self" : "_blank"),
			"data-which-tab": ((Extracts.popFrameProvider == Popins) ? "current" : "new"),
			"data-tab-or-window": ((Extracts.popFrameProvider == Popins) ? "tab" : "window")
		}));
    },

    //  Called by: extracts.js (as `titleForPopFrame_${targetTypeName}`)
    titleForPopFrame_ANNOTATION_PARTIAL: (popFrame) => {
        GWLog("Extracts.titleForPopFrame_ANNOTATION_PARTIAL", "extracts-annotations.js", 2);

		return Extracts.titleForPopFrame_ANNOTATION(popFrame);
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_ANNOTATION_PARTIAL: (popup) => {
    	return Extracts.preparePopup_ANNOTATION(popup);
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_ANNOTATION_PARTIAL: (popFrame) => {
        GWLog("Extracts.rewritePopFrameContent_ANNOTATION_PARTIAL", "extracts-annotations.js", 2);

		Extracts.rewritePopFrameContent_ANNOTATION(popFrame);
    }
};

/*	Inject partial-annotation metadata into a popup that is not already a
	partial annotation.
 */
Extracts.additionalRewrites.push(Extracts.injectPartialAnnotationMetadata = (popFrame) => {
    GWLog("Extracts.injectPartialAnnotationMetadata", "extracts.js", 2);

	let target = popFrame.spawningTarget;
	if (Annotations.isAnnotatedLinkPartial(target) == false)
		return;

    let targetTypeName = Extracts.targetTypeInfo(target).typeName;
	if ([ "ANNOTATION_PARTIAL", "LOCAL_PAGE" ].includes(targetTypeName))
		return;

	//	Construct container and synthesized include-link.
	let partialAnnotationAppendContainer = newElement("DIV", {
		"class": [ "partial-annotation-append-container",
				   "markdownBody",
				   "popframe-body",
				   (Extracts.popFrameProvider == Popups ? "popup-body" : "popin-body")
				   ].join(" ")
	});
	partialAnnotationAppendContainer.appendChild(synthesizeIncludeLink((target.dataset.urlOriginal ?? target.href), {
		"class": "link-annotated-partial include-annotation-partial include-strict",
		"data-template": "annotation-blockquote-not",
		"data-template-fields": "linkTarget:$,whichTab:$,tabOrWindow:$",
		"data-link-target": ((Extracts.popFrameProvider == Popins) ? "_self" : "_blank"),
		"data-which-tab": ((Extracts.popFrameProvider == Popins) ? "current" : "new"),
		"data-tab-or-window": ((Extracts.popFrameProvider == Popins) ? "tab" : "window")
	}));

	//	Add the whole thing to the pop-frame.
	Extracts.popFrameProvider.addPartToPopFrame(popFrame, partialAnnotationAppendContainer);
	Extracts.popFrameProvider.addClassesToPopFrame(popFrame, "has-footer");

	//	Trigger transclude of the partial annotation.
	Transclude.triggerTranscludesInContainer(partialAnnotationAppendContainer, {
		source: "Extracts.injectPartialAnnotationMetadata",
		container: partialAnnotationAppendContainer,
		document: partialAnnotationAppendContainer
	});
});

/*=----------------------=*/
/*= ANNOTATIONS: HELPERS =*/
/*=----------------------=*/

Extracts = { ...Extracts,
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
                    //  Do nothing if the annotation is already loaded.
                    if (Annotations.cachedDataExists(annotatedTarget) == false)
                        Annotations.load(annotatedTarget);
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
                    //  Do nothing if the annotation is already loaded.
                    if (Annotations.cachedDataExists(annotatedTarget) == false)
                        Annotations.load(annotatedTarget);
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
