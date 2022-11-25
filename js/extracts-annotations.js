/*******************************************/
/*  Events fired by extracts-annotations.js:

    GW.contentDidLoad {
            source: "Extracts.annotationForTarget",
            contentType: "annotation",
            document:
                A DocumentFragment containing the constructed annotation.
            loadLocation:
            	URL of the annotation resource.
            baseLocation:
                URL of the annotated target.
            flags:
                GW.contentDidLoadEventFlags.needsRewrite
        }
        Fired when the content of the annotation pop-frame (i.e., the
        annotation) has been constructed, but not yet injected into a pop-frame.

        (See rewrite.js for more information about the keys and values of the
         GW.contentDidLoad event.)

    GW.contentDidLoad {
            source: "Extracts.rewritePopFrameContent_ANNOTATION"
            contentType: "annotation",
            document:
                The `document` property of the annotation pop-frame.
            loadLocation:
            	URL of the annotation resource.
            baseLocation:
                URL of the annotated target.
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
    (target) =>                 // Target classes to add
        ((   Annotations.isAnnotatedLinkPartial(target)
          && Annotations.dataSourceForTarget(target) == Annotations.dataSources.local)
         ? "has-annotation-partial"
         : "has-annotation"),
    "annotationForTarget",      // Pop-frame fill function
    "annotation"                // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Constructed annotations.
    cachedAnnotations: { },

	cachedAnnotationForTarget: (target) => {
		return Extracts.cachedAnnotations[Extracts.targetIdentifier(target)];
	},

	setCachedAnnotationForTarget: (annotation, target) => {
		Extracts.cachedAnnotations[Extracts.targetIdentifier(target)] = annotation;
	},

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

        //  Use cached constructed annotation, if available.
        if (Extracts.cachedAnnotationForTarget(target))
            return newDocument(Extracts.cachedAnnotationForTarget(target));

        //  Get annotation reference data (if it’s been loaded).
        let referenceData = Annotations.referenceDataForTarget(target);
        if (referenceData == null) {
            /*  If the annotation has yet to be loaded, we’ll trust that it has
            	been asked to load, and meanwhile wait, and do nothing yet.
             */
			target.popFrame.classList.toggle("loading", true);

			Annotations.waitForAnnotationLoad(Annotations.targetIdentifier(target), 
			   (identifier) => {
				Extracts.postRefreshSuccessUpdatePopFrameForTarget(target);
			}, (identifier) => {
				Extracts.postRefreshFailureUpdatePopFrameForTarget(target);
			});

            return newDocument();
        } else if (referenceData == "LOADING_FAILED") {
            /*  If we’ve already tried and failed to load the annotation, we
                will not try loading again, and just show the “loading failed”
                message.
             */
            target.popFrame.classList.add("loading-failed");

            return newDocument();
        }

		//	Construct annotation by filling template with reference data.
		let constructedAnnotation = Transclude.fillTemplateNamed("annotation-blockquote-not", referenceData, {
			linkTarget:  ((Extracts.popFrameProvider == Popins) ? "_self" : "_blank")
		});

		//	Make aux-links-append include-links lazy.
		Array.from(constructedAnnotation.querySelectorAll(".include-strict")).filter(link => 
			Extracts.auxLinksLinkType(link) != null
		).forEach(link => {
			link.swapClasses([ "include", "include-strict" ], 0);
			link.classList.add("include-when-collapsed");
		});

        //  Fire contentDidLoad event.
        let targetLocation = new URL(target.href);
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "Extracts.annotationForTarget",
            contentType: "annotation",
            document: constructedAnnotation,
            loadLocation: Annotations.sourceURLForTarget(target),
            baseLocation: targetLocation,
            flags: (  GW.contentDidLoadEventFlags.needsRewrite
            		| GW.contentDidLoadEventFlags.collapseAllowed)
        });

        //  Cache constructed and processed annotation.
        Extracts.setCachedAnnotationForTarget(constructedAnnotation, target);

        return newDocument(constructedAnnotation);
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

        //  Fire contentDidLoad event.
        let targetLocation = new URL(target.href);
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "Extracts.rewritePopFrameContent_ANNOTATION",
            contentType: "annotation",
            document: popFrame.document,
            loadLocation: Annotations.sourceURLForTarget(target),
            baseLocation: targetLocation,
            flags: GW.contentDidLoadEventFlags.collapseAllowed
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
        let allAnnotatedTargetsInContainer = Annotations.allAnnotatedLinksInContainer(container);

        if (Extracts.popFrameProvider == Popups) {
            //  Add hover event listeners to all the annotated targets.
            allAnnotatedTargetsInContainer.forEach(annotatedTarget => {
                annotatedTarget.removeAnnotationLoadEvents = onEventAfterDelayDo(annotatedTarget, "mouseenter", Extracts.annotationLoadHoverDelay, (event) => {
                    //  Get the unique identifier of the annotation for the target.
                    let annotationIdentifier = Annotations.targetIdentifier(annotatedTarget);

                    //  Do nothing if the annotation is already loaded.
                    if (Annotations.cachedAnnotationExists(annotationIdentifier))
                        return;

                    //  Otherwise, load the annotation.
                    Annotations.loadAnnotation(annotationIdentifier);
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
                    if (Annotations.cachedAnnotationExists(annotationIdentifier) == false)
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
    }
};
