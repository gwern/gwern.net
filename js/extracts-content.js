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

	NOTE: These testing (a.k.a. “type predicate”) functions SHOULD NOT be used
	directly, but only via Extracts.targetTypeInfo()!

	Each set also contains the corresponding filling function, which
	is called by fillPopFrame() (chosen on the basis of the return values
	of the testing functions, and the specified order in which they’re
	called). The filling function takes a target element and returns a
	string which comprises the HTML contents that should be injected into
	the pop-frame spawned by the given target.
 */

Extracts.targetTypeDefinitions.insertBefore([
	"LOCAL_PAGE",          // Type name
	"isLocalPageLink",     // Type predicate function
	"has-content",         // Target classes to add
	"localPageForTarget",  // Pop-frame fill function
	"local-page"           // Pop-frame classes
], (def => def[0] == "ANNOTATION_PARTIAL"));

/*=-------------=*/
/*= LOCAL PAGES =*/
/*=-------------=*/

Extracts = { ...Extracts,
    /*  Local links (to sections of the current page, or other site pages).
     */
    //  Called by: Extracts.targetTypeInfo (as `predicateFunctionName`)
    isLocalPageLink: (target) => {
        return (   Content.contentTypes.localPage.matches(target)
				&& (   isAnchorLink(target)
					|| target.pathname != location.pathname));
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

	/*	“Full context” links in backlinks lists.
	 */
	isFullBacklinkContextLink: (target) => {
		return (   target.closest(".backlink-source") != null
				&& target.classList.contains("link-page"));
	},

	/*	Annotation title-links on mobile.
	 */
	isMobileAnnotationTitleLink: (target) => {
		return (   GW.isMobile()
				&& target.matches(".data-field.title a.title-link"));
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
        			  || Extracts.isSidebarLink(target)
        			  || Extracts.isMobileAnnotationTitleLink(target))));
    },

    //  Called by: Extracts.fillPopFrame (as `popFrameFillFunctionName`)
    //	Called by: Extracts.citationForTarget (extracts-content.js)
    //	Called by: Extracts.citationBackLinkForTarget (extracts-content.js)
    localPageForTarget: (target, forceNarrow) => {
        GWLog("Extracts.localPageForTarget", "extracts.js", 2);

		/*  Check to see if the target location matches an already-displayed
			page (which can be the root page of the window).

			If the entire linked page is already displayed, and if the
			target points to an anchor in that page, display the linked
			section or element.

			Also display just the linked block if we’re spawning this
			pop-frame from an in-pop-frame TOC.

			Otherwise, display the entire linked page.
		 */
		let fullPage = !(   isAnchorLink(target)
        				 && (   forceNarrow
        					 || target.closest(".TOC")
        					 || Extracts.targetDocument(target)));
        if (fullPage) {
            /*  Note that we might end up here because there is yet no
                pop-frame with the full linked document, OR because there is
                such a pop-frame but it’s a pinned popup or something (and thus
                we didn’t want to despawn it and respawn it at this target’s
                location).
            */
			/*  Mark the pop-frame as a full page embed, and give it suitable
				identifying classes.
			 */
			Extracts.popFrameProvider.addClassesToPopFrame(target.popFrame, "full-page");
        }

		//	Designate “full context” pop-frames for backlinks.
		if (Extracts.isFullBacklinkContextLink(target))
			Extracts.popFrameProvider.addClassesToPopFrame(target.popFrame, "full-backlink-context");

		//	Synthesize include-link (with or without hash, as appropriate).
		let includeLink = synthesizeIncludeLink(target, { class: "include-block-context-expanded" });
		if (fullPage) {
			stripAnchorsFromLink(includeLink);
		} else if (   Extracts.isFullBacklinkContextLink(target)
				   && target.pathname == location.pathname) {
			/*	Since “full” context is just the base page, which we don’t want 
				to pop up/in, we instead show the containing section or
				footnote.
			 */
			let targetElement = targetElementInDocument(target, Extracts.rootDocument);
			let nearestSection = targetElement.closest("section, li.footnote");
			if (nearestSection)
				includeLink.hash = "#" + nearestSection.id;
		}

		return newDocument(includeLink);
    },

    //  Called by: Extracts.titleForPopFrame (as `titleForPopFrame_${targetTypeName}`)
    titleForPopFrame_LOCAL_PAGE: (popFrame) => {
        GWLog("Extracts.titleForPopFrame_LOCAL_PAGE", "extracts.js", 2);

        let target = popFrame.spawningTarget;
        let referenceData = Content.referenceDataForLink(target);

		let popFrameTitleText, popFrameTitleLinkHref;
		if (referenceData == null) {
			popFrameTitleText = "";
			if (target.pathname != location.pathname)
				popFrameTitleText += target.pathname;
			if (popFrame.classList.contains("full-page") == false)
				popFrameTitleText += target.hash;
			popFrameTitleText = `<code>${popFrameTitleText}</code>`;

			popFrameTitleLinkHref = target.href;
		} else {
			popFrameTitleText = popFrame.classList.contains("full-page")
								? referenceData.popFrameTitleTextShort
								: referenceData.popFrameTitleText;
			popFrameTitleLinkHref = referenceData.popFrameTitleLinkHref;
		}

		if (popFrame.classList.contains("backlinks")) {
			popFrameTitleText += " (Backlinks)";
		}

		return Transclude.fillTemplateNamed("pop-frame-title-standard", {
			popFrameTitleLinkHref:  popFrameTitleLinkHref,
			popFrameTitleText:      popFrameTitleText
		}, Extracts.getStandardPopFrameTitleTemplateFillContext());
    },

	preparePopFrame_LOCAL_PAGE: (popFrame) => {
        GWLog("Extracts.preparePopFrame_LOCAL_PAGE", "extracts.js", 2);

        let target = popFrame.spawningTarget;

		/*	For local content embed pop-frames, add handler to trigger
			transcludes in source content when they trigger in the pop-frame.
		 */
		if (Content.cachedDataExists(target)) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Content.updateCachedContent(target, (content) => {
					Transclude.allIncludeLinksInContainer(content).filter(includeLink =>
						includeLink.href == info.includeLink.href
					).forEach(includeLink => {
						Transclude.transclude(includeLink, true);
					});
				});
			}, { condition: (info) => (   info.source == "transclude"
									   && info.document == popFrame.document) });
		}

		return popFrame;
	},

    //  Called by: Extracts.preparePopup (as `preparePopup_${targetTypeName}`)
    preparePopup_LOCAL_PAGE: (popup) => {
        GWLog("Extracts.preparePopup_LOCAL_PAGE", "extracts.js", 2);

        let target = popup.spawningTarget;

		popup = Extracts.preparePopFrame_LOCAL_PAGE(popup);

		//  Do not spawn “full context” popup if the link is visible.
 		if (   Extracts.isFullBacklinkContextLink(target)
 			&& popup.classList.contains("full-page") == false
 			&& Popups.isVisible(targetElementInDocument(target, Extracts.rootDocument)))
			return null;

       /*  Designate popups spawned from section links in the the TOC (for
            special styling).
         */
        if (Extracts.isTOCLink(target))
        	Extracts.popFrameProvider.addClassesToPopFrame(popup, "toc-section");

        return popup;
    },

    //  Called by: Extracts.rewritePopinContent_LOCAL_PAGE
    //  Called by: Extracts.rewritePopupContent_LOCAL_PAGE
    //  Called by: Extracts.rewritePopinContent (as `rewritePopFrameContent_${targetTypeName}`)
    //  Called by: Extracts.rewritePopupContent (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_PAGE: (popFrame, injectEventInfo = null) => {
        GWLog("Extracts.rewritePopFrameContent_LOCAL_PAGE", "extracts.js", 2);

		let target = popFrame.spawningTarget;

		if (injectEventInfo == null) {
			//	Preliminary rewrites.
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				//	Add page body classes.
				let referenceData = Content.referenceDataForLink(target);
				Extracts.popFrameProvider.addClassesToPopFrame(popFrame, ...(referenceData.pageBodyClasses));

				//	Update pop-frame title.
				Extracts.updatePopFrameTitle(popFrame);
			}, {
				phase: "<",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Main rewrites.
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopFrameContent_LOCAL_PAGE(popFrame, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popFrame.body, {
				source: "Extracts.rewritePopFrameContent_LOCAL_PAGE",
				container: popFrame.body,
				document: popFrame.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE

		//	Provider-specific rewrites.
		if (Extracts.popFrameProvider == Popups)
			Extracts.rewritePopupContent_LOCAL_PAGE(popFrame, injectEventInfo);
		else // if (Extracts.popFrameProvider == Popins)
			Extracts.rewritePopinContent_LOCAL_PAGE(popFrame, injectEventInfo);

		//	Something failed somehow.
		if (isNodeEmpty(injectEventInfo.container)) {
			Extracts.popFrameProvider.addClassesToPopFrame(popFrame, "loading-failed");
			return;
		}

		//	Make first image load eagerly.
		let firstImage = (   popFrame.body.querySelector(".page-thumbnail")
						  || popFrame.body.querySelector("figure img"))
		if (firstImage) {
			firstImage.loading = "eager";
			firstImage.decoding = "sync";
		}

		//	Strip a single collapse block encompassing the top level content.
		if (   isOnlyChild(injectEventInfo.container.firstElementChild)
			&& injectEventInfo.container.firstElementChild.classList.contains("collapse"))
			expandLockCollapseBlock(injectEventInfo.container.firstElementChild);

		//	Designate section backlinks popups as such.
		if (injectEventInfo.container.firstElementChild.classList.containsAnyOf([ "section-backlinks", "section-backlinks-container" ]))
			Extracts.popFrameProvider.addClassesToPopFrame(popFrame, "aux-links", "backlinks");

		/*	In the case where the spawning link points to a specific element
			within the transcluded content, but we’re transcluding the full
			page and not just the block context of the targeted element,
			transclude.js has not marked the targeted element for us already.
			So we must do it here.
		 */
		if (   isAnchorLink(target)
			&& popFrame.classList.containsAnyOf([ "full-page", "full-backlink-context" ]))
			targetElementInDocument(target, popFrame.document).classList.add("block-context-highlighted");

		//  Scroll to the target.
		Extracts.scrollToTargetedElementInPopFrame(popFrame);

		//	Lazy-loading of adjacent sections.
		//	WARNING: Experimental code!
// 		if (target.hash > "") {
// 			requestAnimationFrame(() => {
// 				Extracts.loadAdjacentSections(popFrame, "next,prev");
// 			});
// 		}
    },

    //  Called by: Extracts.rewritePopupContent (as `rewritePopupContent_${targetTypeName}`)
    rewritePopupContent_LOCAL_PAGE: (popup, injectEventInfo = null) => {
        GWLog("Extracts.rewritePopupContent_LOCAL_PAGE", "extracts.js", 2);

		if (injectEventInfo == null) {
			Extracts.rewritePopFrameContent_LOCAL_PAGE(popup);
			return;
		}

        let target = popup.spawningTarget;

		let referenceData = Content.referenceDataForLink(target);
		if (referenceData) {
			//	Insert page thumbnail into page abstract.
			if (   referenceData.pageThumbnailHTML
				&& popup.document.querySelector("img.page-thumbnail") == null) {
				let pageAbstract = popup.document.querySelector("#page-metadata + .abstract blockquote");
				if (pageAbstract)
					pageAbstract.insertBefore(newElement("FIGURE", {
						class: "float-right"
					}, {
						innerHTML: referenceData.pageThumbnailHTML
					}), pageAbstract.firstChild);
			}
		}

        //  Make anchorlinks scroll popup instead of opening normally.
		Extracts.constrainLinkClickBehaviorInPopFrame(popup);
    },

    //  Called by: Extracts.rewritePopinContent (as `rewritePopinContent_${targetTypeName}`)
    rewritePopinContent_LOCAL_PAGE: (popin, injectEventInfo = null) => {
        GWLog("Extracts.rewritePopinContent_LOCAL_PAGE", "extracts.js", 2);

		if (injectEventInfo == null) {
			Extracts.rewritePopFrameContent_LOCAL_PAGE(popin);
			return;
		}

        /*  Make anchorlinks scroll popin instead of opening normally
        	(but only for non-popin-spawning anchorlinks).
         */
		Extracts.constrainLinkClickBehaviorInPopFrame(popin, (link => link.classList.contains("no-popin")));
    },

	loadAdjacentSections: (popFrame, which) => {
        GWLog("Extracts.loadAdjacentSections", "extracts.js", 2);

		which = which.split(",");
		let next = which.includes("next");
		let prev = which.includes("prev");

		let target = popFrame.spawningTarget;
		let sourceDocument = Extracts.cachedPages[target.pathname] || Extracts.rootDocument;

		popFrame.firstSection = popFrame.firstSection || targetElementInDocument(target, sourceDocument);
		popFrame.lastSection = popFrame.lastSection || popFrame.firstSection;

		if (!(next || prev))
			return;

		if (targetElementInDocument(target, popFrame.document) == null) {
			let sectionWrapper = newElement("SECTION", {
				"id": popFrame.firstSection.id,
				"class": [ ...(popFrame.firstSection.classList) ].join(" ")
			});
			sectionWrapper.replaceChildren(...(popFrame.body.children));
			popFrame.body.appendChild(sectionWrapper);

			//  Fire a contentDidInject event.
			GW.notificationCenter.fireEvent("GW.contentDidInject", {
				source: "Extracts.loadAdjacentSections",
				container: popFrame.body.firstElementChild,
				document: popFrame.document,
				loadLocation: URLFromString(target.href)
			});
		}

		let prevSection = popFrame.firstSection.previousElementSibling;
		if (prev && prevSection) {
			popFrame.body.insertBefore(newDocument(prevSection), popFrame.body.firstElementChild);

			//  Fire a contentDidInject event.
			GW.notificationCenter.fireEvent("GW.contentDidInject", {
				source: "Extracts.loadAdjacentSections",
				container: popFrame.body.firstElementChild,
				document: popFrame.document,
				loadLocation: URLFromString(target.href)
			});

			popFrame.firstSection = prevSection;
		}

		let nextSection = popFrame.lastSection.nextElementSibling;
		if (next && nextSection) {
			popFrame.body.insertBefore(newDocument(nextSection), null);

			//  Fire a contentDidInject event.
			GW.notificationCenter.fireEvent("GW.contentDidInject", {
				source: "Extracts.loadAdjacentSections",
				container: popFrame.body.lastElementChild,
				document: popFrame.document,
				loadLocation: URLFromString(target.href)
			});

			popFrame.lastSection = nextSection;
		}
	}
};

/*=-----------------=*/
/*= AUXILIARY LINKS =*/
/*=-----------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "AUX_LINKS_LINK",       // Type name
    "isAuxLinksLink",       // Type predicate function
    "has-content",          // Target classes to add
    "auxLinksForTarget",    // Pop-frame fill function
    "aux-links"             // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: Extracts.isLocalCodeFileLink
    //  Called by: extracts.js (as `predicateFunctionName`)
    isAuxLinksLink: (target) => {
        let auxLinksLinkType = AuxLinks.auxLinksLinkType(target);
        return (   auxLinksLinkType != null
                && target.classList.contains(auxLinksLinkType));
    },

    /*  This “special testing function” is used to exclude certain targets which
        have already been categorized as (in this case) `AUX_LINKS_LINK` targets.
        It returns false if the target is to be excluded, true otherwise.
        Excluded targets will not spawn pop-frames.
     */
    //  Called by: Extracts.targets.testTarget (as `testTarget_${targetTypeInfo.typeName}`)
    testTarget_AUX_LINKS_LINK: (target) => {
        let exclude = false;
        let auxLinksType = AuxLinks.auxLinksLinkType(target);
        let containingAnnotation = target.closest(".annotation");
        if (containingAnnotation) {
            let includedAuxLinksBlock = containingAnnotation.querySelector(`.${auxLinksType}-append`);
            if (includedAuxLinksBlock)
                exclude = true;
        }

        return (!(   Extracts.popFrameProvider == Popins
                  && exclude == true));
    },

    /*  Backlinks, similar-links, etc.
     */
    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    auxLinksForTarget: (target) => {
        GWLog("Extracts.auxLinksForTarget", "extracts-content.js", 2);

		return newDocument(synthesizeIncludeLink(target, { class: AuxLinks.auxLinksLinkType(target) }));
    },

    //  Called by: Extracts.preparePopFrame (as `preparePopFrame_${targetTypeName}`)
    preparePopFrame_AUX_LINKS_LINK: (popFrame) => {
        GWLog("Extracts.preparePopFrame_AUX_LINKS_LINK", "extracts-content.js", 2);

        let auxLinksLinkType = AuxLinks.auxLinksLinkType(popFrame.spawningTarget);
        if (auxLinksLinkType > "")
            Extracts.popFrameProvider.addClassesToPopFrame(popFrame, auxLinksLinkType);

        return popFrame;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_AUX_LINKS_LINK: (popFrame, injectEventInfo = null) => {
		if (injectEventInfo == null) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopFrameContent_AUX_LINKS_LINK(popFrame, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popFrame.body, {
				source: "Extracts.rewritePopFrameContent_AUX_LINKS_LINK",
				container: popFrame.body,
				document: popFrame.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE

		if (Extracts.popFrameProvider == Popups) {
			popFrame.document.querySelectorAll(".backlink-source a:nth-of-type(2)").forEach(fullContextLink => {
				let targetDocument = Extracts.targetDocument(fullContextLink);
				if (targetDocument) {
					let targetElement = targetElementInDocument(fullContextLink, targetDocument);
					fullContextLink.addEventListener("mouseenter", (event) => {
						targetElement.classList.toggle("block-context-highlighted-temp", true);
					});
					fullContextLink.addEventListener("mouseleave", (event) => {
						targetElement.classList.toggle("block-context-highlighted-temp", false);
					});
					GW.notificationCenter.addHandlerForEvent("Popups.popupWillDespawn", (info) => {
						targetElement.classList.toggle("block-context-highlighted-temp", false);
					}, {
						once: true,
						condition: (info) => (info.popup == popFrame)
					});
				}
			});
		}
    },

    //  Called by: extracts.js (as `titleForPopFrame_${targetTypeName}`)
    titleForPopFrame_AUX_LINKS_LINK: (popFrame) => {
        let target = popFrame.spawningTarget;
        let targetPage = AuxLinks.targetOfAuxLinksLink(target);
        let auxLinksLinkType = AuxLinks.auxLinksLinkType(target);
        switch (auxLinksLinkType) {
		case "backlinks":
			return newDocument(`<code>${targetPage}</code><span> (Backlinks)</span>`);
		case "similars":
			return newDocument(`<code>${targetPage}</code><span> (Similar links)</span>`);
		case "link-bibliography":
			return newDocument(`<code>${targetPage}</code><span> (Link bibliography)</span>`);
		default:
			return newDocument(`<code>${targetPage}</code>`);
        }
    },
};

/*=----------------=*/
/*= DROPCAP LINKS =*/
/*=----------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "DROPCAP_LINK",      // Type name
    "isDropcapLink",     // Type predicate function
    null,                // Target classes to add
    "dropcapForTarget",  // Pop-frame fill function
    "dropcap"            // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    isDropcapLink: (target) => {
        return target.classList.contains("link-dropcap");
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    dropcapForTarget: (target) => {
        GWLog("Extracts.dropcapForTarget", "extracts-content.js", 2);

		let letter = target.dataset.letter;
		let dropcapType = target.dataset.dropcapType;

		return newDocument(
			  `<p>A capital letter <strong>${letter}</strong> dropcap initial, from the `
			+ `<a class="link-page" href="/dropcap#${dropcapType}"><strong>${dropcapType}</strong></a>`
			+ ` dropcap font.</p>`
		)
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_DROPCAP_LINK: (popup) => {
        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    },

    //  Called by: extracts.js (as `preparePopin_${targetTypeName}`)
    preparePopin_DROPCAP_LINK: (popin) => {
        //  No footer bar.
        popin.classList.add("no-footer-bar");

        return popin;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_DROPCAP_LINK: (popFrame) => {
        GWLog("Extracts.rewritePopFrameContent_DROPCAP_LINK", "extracts.js", 2);

		//	Determine load location.
        let target = popFrame.spawningTarget;
		let containingPopFrame = Extracts.popFrameProvider.containingPopFrame(target);
		let loadLocation = containingPopFrame
						   ? containingPopFrame.spawningTarget
						   : location;
		
		//	Fire events.
		GW.notificationCenter.fireEvent("GW.contentDidLoad", {
			source: "Extracts.rewritePopFrameContent_DROPCAP_LINK",
			container: popFrame.body,
			document: popFrame.document,
			loadLocation: new URL(loadLocation.href)
		});
		GW.notificationCenter.fireEvent("GW.contentDidInject", {
			source: "Extracts.rewritePopFrameContent_DROPCAP_LINK",
			container: popFrame.body,
			document: popFrame.document,
			loadLocation: new URL(loadLocation.href),
			flags: GW.contentDidInjectEventFlags.clickable
		});
    },
};

/*=-----------=*/
/*= CITATIONS =*/
/*=-----------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "CITATION",             // Type name
    "isCitation",           // Type predicate function
    null,                   // Target classes to add
    "citationForTarget",    // Pop-frame fill function
    "footnote"              // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    isCitation: (target) => {
        return target.classList.contains("footnote-ref");
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    citationForTarget: (target) => {
        GWLog("Extracts.citationForTarget", "extracts-content.js", 2);

		return Extracts.localPageForTarget(target, true);
    },

    //  Called by: extracts.js (as `titleForPopFrame_${targetTypeName}`)
    titleForPopFrame_CITATION: (popFrame) => {
        let target = popFrame.spawningTarget;
        let footnoteNumber = target.querySelector("sup").textContent;
        let popFrameTitleText = `Footnote #${footnoteNumber}`;

        return Extracts.standardPopFrameTitleElementForTarget(target, popFrameTitleText);
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_CITATION: (popup) => {
        let target = popup.spawningTarget;

        /*  Do not spawn footnote popup if the {side|foot}note it points to is
            visible.
         */
        if (Array.from(Notes.allNotesForCitation(target)).findIndex(note => Popups.isVisible(note)) != -1)
            return null;

        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        /*  Add event listeners to highlight citation when its footnote
            popup is hovered over.
         */
        popup.addEventListener("mouseenter", (event) => {
            target.classList.toggle("highlighted", true);
        });
        popup.addEventListener("mouseleave", (event) => {
            target.classList.toggle("highlighted", false);
        });
        GW.notificationCenter.addHandlerForEvent("Popups.popupWillDespawn", (info) => {
            target.classList.toggle("highlighted", false);
        }, {
			once: true,
			condition: (info) => (info.popup == popup)
		});

        return popup;
    },

    //  Called by: extracts.js (as `preparePopin_${targetTypeName}`)
    preparePopin_CITATION: (popin) => {
        //  No footer bar.
        popin.classList.add("no-footer-bar");

        return popin;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_CITATION: (popFrame, injectEventInfo = null) => {
        GWLog("Extracts.rewritePopFrameContent_CITATION", "extracts.js", 2);

		if (injectEventInfo == null) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopFrameContent_CITATION(popFrame, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popFrame.body, {
				source: "Extracts.rewritePopFrameContent_CITATION",
				container: popFrame.body,
				document: popFrame.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE

		/*	Unwrap sidenote. (Corrects for edge case where a popup for a section
			of the current page which is currently within a collapsed section, 
			contains a footnote reference. Hovering over the citation will spawn
			a popup instead of sliding up the sidenote, as the latter is hidden.
			The sidenote, once transcluded, must then be unwrapped specially.)
		 */
		if (injectEventInfo.container.firstElementChild.classList.contains("sidenote"))
			unwrap(injectEventInfo.container.querySelector(".sidenote-inner-wrapper"));
    },
};

/*=---------------------=*/
/*= CITATIONS BACKLINKS =*/
/*=---------------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "CITATION_BACK_LINK",               // Type name
    "isCitationBackLink",               // Type predicate function
    null,                               // Target classes to add
    "citationBackLinkForTarget",        // Pop-frame fill function
    "citation-context"                  // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    isCitationBackLink: (target) => {
        return target.classList.contains("footnote-back");
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    citationBackLinkForTarget: (target) => {
        GWLog("Extracts.citationBackLinkForTarget", "extracts-content.js", 2);

        return Extracts.localPageForTarget(target, true);
    },

    /*  This “special testing function” is used to exclude certain targets which
        have already been categorized as (in this case) `CITATION_BACK_LINK`
        targets. It returns false if the target is to be excluded, true
        otherwise. Excluded targets will not spawn pop-frames.
     */
    //  Called by: extracts.js (as `testTarget_${targetTypeInfo.typeName}`)
    testTarget_CITATION_BACK_LINK: (target) => {
        return (Extracts.popFrameProvider != Popins);
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_CITATION_BACK_LINK: (popup) => {
        let target = popup.spawningTarget;

        //  Do not spawn citation context popup if citation is visible.
        let targetDocument = Extracts.targetDocument(target);
        if (targetDocument) {
        	let targetElement = targetElementInDocument(target, targetDocument);
        	if (   targetElement
        		&& Popups.isVisible(targetElement))
        		return null;
        }

        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    },

    //  Called by: extracts.js (as `preparePopin_${targetTypeName}`)
    preparePopin_CITATION_BACK_LINK: (popin) => {
        //  No footer bar.
        popin.classList.add("no-footer-bar");

        return popin;
    },

    //  Called by: extracts.js (as `rewritePopupContent_${targetTypeName}`)
    rewritePopupContent_CITATION_BACK_LINK: (popup, injectEventInfo = null) => {
        let target = popup.spawningTarget;

		if (injectEventInfo == null) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopupContent_CITATION_BACK_LINK(popup, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popup.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popup.body, {
				source: "Extracts.rewritePopupContent_CITATION_BACK_LINK",
				container: popup.body,
				document: popup.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE

        //  Highlight citation in popup.
        /*  Remove the .targeted class from a targeted citation (if any)
            inside the popup (to prevent confusion with the citation that
            the spawning link points to, which will be highlighted).
         */
        popup.document.querySelectorAll(".footnote-ref.targeted").forEach(targetedCitation => {
            targetedCitation.classList.remove("targeted");
        });
        //  In the popup, the citation for which context is being shown.
        let citationInPopup = targetElementInDocument(target, popup.document);
        //  Highlight the citation.
        citationInPopup.classList.add("targeted");
        //	Remove class that would interfere with styling.
        citationInPopup.classList.remove("block-context-highlighted");

        //  Scroll to the citation.
        Extracts.scrollToTargetedElementInPopFrame(popup);
    }
};

/*=---------------=*/
/*= REMOTE VIDEOS =*/
/*=---------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "REMOTE_VIDEO",          // Type name
    "isRemoteVideoLink",     // Type predicate function
    "has-content",           // Target classes to add
    "remoteVideoForTarget",  // Pop-frame fill function
    "video object"           // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    isRemoteVideoLink: (target) => {
        return Content.contentTypes.remoteVideo.matches(target);
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    remoteVideoForTarget: (target) => {
        GWLog("Extracts.remoteVideoForTarget", "extracts-content.js", 2);

		return newDocument(synthesizeIncludeLink(target));
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_REMOTE_VIDEO: (popup) => {
		let target = popup.spawningTarget;

		if (Content.contentTypes.remoteVideo.isYoutubeLink(target)) {
			Extracts.popFrameProvider.addClassesToPopFrame(popup, "youtube");
		} else if (Content.contentTypes.remoteVideo.isVimeoLink(target)) {
			Extracts.popFrameProvider.addClassesToPopFrame(popup, "vimeo");
		}

        return popup;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_REMOTE_VIDEO: (popFrame, injectEventInfo = null) => {
		if (injectEventInfo == null) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopFrameContent_REMOTE_VIDEO(popFrame, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popFrame.body, {
				source: "Extracts.rewritePopFrameContent_REMOTE_VIDEO",
				container: popFrame.body,
				document: popFrame.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE

        //  Loading spinner.
        Extracts.setLoadingSpinner(popFrame);
    }
};

/*=-----------------------=*/
/*= LOCALLY HOSTED VIDEOS =*/
/*=-----------------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "LOCAL_VIDEO",              // Type name
    "isLocalVideoLink",         // Type predicate function
    "has-content",              // Target classes to add
    "localVideoForTarget",      // Pop-frame fill function
    "video object"              // Pop-frame class
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    isLocalVideoLink: (target) => {
        return Content.contentTypes.localVideo.matches(target);
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    localVideoForTarget: (target) => {
        GWLog("Extracts.localVideoForTarget", "extracts-content.js", 2);

        return newDocument(synthesizeIncludeLink(target));
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_LOCAL_VIDEO: (popup) => {
        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    },

    //  Called by: extracts.js (as `preparePopin_${targetTypeName}`)
    preparePopin_LOCAL_VIDEO: (popin) => {
        //  No footer bar.
        popin.classList.add("no-footer-bar");

        return popin;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_VIDEO: (popFrame, injectEventInfo = null) => {
		if (injectEventInfo == null) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopFrameContent_LOCAL_VIDEO(popFrame, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popFrame.body, {
				source: "Extracts.rewritePopFrameContent_LOCAL_VIDEO",
				container: popFrame.body,
				document: popFrame.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE

        //  Loading spinner.
		Extracts.popFrameProvider.addClassesToPopFrame(popFrame, "loading");

    	let video = popFrame.document.querySelector("video");
    	let source = video.querySelector("source");

		doAjax({
			location: source.src,
			method: "HEAD",
			onSuccess: (event) => {
				Extracts.postRefreshUpdatePopFrameForTarget(popFrame.spawningTarget, true);
			},
			onFailure: (event) => {
                Extracts.postRefreshUpdatePopFrameForTarget(popFrame.spawningTarget, false);
			}
		});
    }
};

/*=----------------------------=*/
/*= LOCALLY HOSTED AUDIO FILES =*/
/*=----------------------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "LOCAL_AUDIO",              // Type name
    "isLocalAudioLink",         // Type predicate function
    "has-content",              // Target classes to add
    "localAudioForTarget",      // Pop-frame fill function
    "audio object"              // Pop-frame class
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    isLocalAudioLink: (target) => {
        return Content.contentTypes.localAudio.matches(target);
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    localAudioForTarget: (target) => {
        GWLog("Extracts.localAudioForTarget", "extracts-content.js", 2);

        return newDocument(synthesizeIncludeLink(target));
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_LOCAL_AUDIO: (popup) => {
        //  Mini title bar.
        popup.classList.add("mini-title-bar");

		//	Audio elements can’t get taller.
        popup.classList.add("no-resize-height");

        return popup;
    },

    //  Called by: extracts.js (as `preparePopin_${targetTypeName}`)
    preparePopin_LOCAL_AUDIO: (popin) => {
        //  No footer bar.
        popin.classList.add("no-footer-bar");

        return popin;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_AUDIO: (popFrame, injectEventInfo = null) => {
		if (injectEventInfo == null) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopFrameContent_LOCAL_AUDIO(popFrame, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popFrame.body, {
				source: "Extracts.rewritePopFrameContent_LOCAL_AUDIO",
				container: popFrame.body,
				document: popFrame.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE

        //  Loading spinner.
		Extracts.popFrameProvider.addClassesToPopFrame(popFrame, "loading");

    	let audio = popFrame.document.querySelector("audio");
    	let source = audio.querySelector("source");

		doAjax({
			location: source.src,
			method: "HEAD",
			onSuccess: (event) => {
				Extracts.postRefreshUpdatePopFrameForTarget(popFrame.spawningTarget, true);
			},
			onFailure: (event) => {
				Extracts.postRefreshUpdatePopFrameForTarget(popFrame.spawningTarget, false);
			}
		});
    }
};

/*=-----------------------=*/
/*= LOCALLY HOSTED IMAGES =*/
/*=-----------------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "LOCAL_IMAGE",              // Type name
    "isLocalImageLink",         // Type predicate function
    "has-content",              // Target classes to add
    "localImageForTarget",      // Pop-frame fill function
    "image object"              // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    isLocalImageLink: (target) => {
        return Content.contentTypes.localImage.matches(target);
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    localImageForTarget: (target) => {
        GWLog("Extracts.localImageForTarget", "extracts-content.js", 2);

        return newDocument(synthesizeIncludeLink(target));
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_LOCAL_IMAGE: (popup) => {
        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    },

    //  Called by: extracts.js (as `preparePopin_${targetTypeName}`)
    preparePopin_LOCAL_IMAGE: (popin) => {
		//	No footer bar.
        popin.classList.add("no-footer-bar");

        return popin;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_IMAGE: (popFrame, injectEventInfo = null) => {
		if (injectEventInfo == null) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopFrameContent_LOCAL_IMAGE(popFrame, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popFrame.body, {
				source: "Extracts.rewritePopFrameContent_LOCAL_IMAGE",
				container: popFrame.body,
				document: popFrame.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE

		//	Loading spinner.
		Extracts.setLoadingSpinner(popFrame);

        if (popFrame.document.querySelector("img[width][height]"))
        	Extracts.popFrameProvider.addClassesToPopFrame(popFrame, "dimensions-specified");
    },
};

/*=--------------------------=*/
/*= LOCALLY HOSTED DOCUMENTS =*/
/*=--------------------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "LOCAL_DOCUMENT",               // Type name
    "isLocalDocumentLink",          // Type predicate function
    "has-content",                  // Target classes to add
    "localDocumentForTarget",       // Pop-frame fill function
    "local-document object"         // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    isLocalDocumentLink: (target) => {
    	return Content.contentTypes.localDocument.matches(target);
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    localDocumentForTarget: (target) => {
        GWLog("Extracts.localDocumentForTarget", "extracts-content.js", 2);

        return newDocument(synthesizeIncludeLink(target));
    },

    /*  This “special testing function” is used to exclude certain targets which
        have already been categorized as (in this case) `LOCAL_DOCUMENT`
        targets. It returns false if the target is to be excluded, true
        otherwise. Excluded targets will not spawn pop-frames.
     */
    //  Called by: extracts.js (as `testTarget_${targetTypeInfo.typeName}`)
    testTarget_LOCAL_DOCUMENT: (target) => {
    	/*	Mobile browsers have no in-browser PDF viewer, so a popin would be
    		pointless, since the file will download anyway.
    	 */
    	if (   Extracts.popFrameProvider == Popins
            && target.pathname.endsWith(".pdf"))
            return false;

        return true;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_DOCUMENT: (popFrame, injectEventInfo = null) => {
		if (injectEventInfo == null) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopFrameContent_LOCAL_DOCUMENT(popFrame, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popFrame.body, {
				source: "Extracts.rewritePopFrameContent_LOCAL_DOCUMENT",
				container: popFrame.body,
				document: popFrame.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE

        let iframe = popFrame.document.querySelector("iframe");
        if (iframe) {
            iframe.addEventListener("load", (event) => {
				//  Set title of popup from page title.
				Extracts.updatePopFrameTitle(popFrame, iframe.contentDocument.title);
            });
        }

        //  Loading spinner.
		Extracts.popFrameProvider.addClassesToPopFrame(popFrame, "loading");

		doAjax({
			location: popFrame.document.querySelector("iframe").src,
			method: "HEAD",
			onSuccess: (event) => {
				Extracts.postRefreshUpdatePopFrameForTarget(popFrame.spawningTarget, true);
			},
			onFailure: (event) => {
                Extracts.postRefreshUpdatePopFrameForTarget(popFrame.spawningTarget, false);
			}
		});
    }
};

/*=---------------------------=*/
/*= LOCALLY HOSTED CODE FILES =*/
/*=---------------------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "LOCAL_CODE_FILE",              // Type name
    "isLocalCodeFileLink",          // Type predicate function
    "has-content",                  // Target classes to add
    "localCodeFileForTarget",       // Pop-frame fill function
    "local-code-file"               // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    isLocalCodeFileLink: (target) => {
    	return Content.contentTypes.localCodeFile.matches(target);
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    localCodeFileForTarget: (target) => {
        GWLog("Extracts.localCodeFileForTarget", "extracts-content.js", 2);

        return newDocument(synthesizeIncludeLink(target));
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_CODE_FILE: (popFrame, injectEventInfo = null) => {
		if (injectEventInfo == null) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopFrameContent_LOCAL_CODE_FILE(popFrame, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popFrame.body, {
				source: "Extracts.rewritePopFrameContent_LOCAL_CODE_FILE",
				container: popFrame.body,
				document: popFrame.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE
    },
};

/*=----------------=*/
/*= OTHER WEBSITES =*/
/*=----------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "FOREIGN_SITE",             // Type name
    "isForeignSiteLink",        // Type predicate function
    "has-content",              // Target classes to add
    "foreignSiteForTarget",     // Pop-frame fill function
    "foreign-site object"       // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    //  Called by: extracts.js (as `predicateFunctionName`)
    isForeignSiteLink: (target) => {
        return Content.contentTypes.foreignSite.matches(target);
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    foreignSiteForTarget: (target) => {
        GWLog("Extracts.foreignSiteForTarget", "extracts-content.js", 2);

        return newDocument(synthesizeIncludeLink(target));
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_FOREIGN_SITE: (popFrame, injectEventInfo = null) => {
		if (injectEventInfo == null) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Extracts.rewritePopFrameContent_FOREIGN_SITE(popFrame, info);
			}, {
				phase: "rewrite",
				condition: (info) => (   info.source == "transclude"
									  && info.document == popFrame.document),
				once: true
			});

			//	Trigger transcludes.
			Transclude.triggerTranscludesInContainer(popFrame.body, {
				source: "Extracts.rewritePopFrameContent_FOREIGN_SITE",
				container: popFrame.body,
				document: popFrame.document,
				context: "popFrame"
			});

			return;
		}

		//	REAL REWRITES BEGIN HERE

        //  Loading spinner.
        Extracts.setLoadingSpinner(popFrame);
    }
};

/*=------------------=*/
/*= CONTENT: HELPERS =*/
/*=------------------=*/

Extracts = { ...Extracts,
	//	Used in: Extracts.setUpContentLoadEventsWithin
	contentLoadHoverDelay: 25,

    //  Called by: extracts.js
    setUpContentLoadEventsWithin: (container) => {
        GWLog("Extracts.setUpContentLoadEventsWithin", "extracts.js", 1);

        /*  Get all targets in the container that use Content as a data loading
        	provider. (Currently that is local page links, local fragment links,
        	and local code file links.)
         */
        let allTargetsInContainer = Array.from(container.querySelectorAll("a[class*='has-content']")).filter(link =>
        	Content.contentTypeForLink(link) != null
        );

        if (Extracts.popFrameProvider == Popups) {
            //  Add hover event listeners to all the chosen targets.
            allTargetsInContainer.forEach(target => {
                target.removeContentLoadEvents = onEventAfterDelayDo(target, "mouseenter", Extracts.contentLoadHoverDelay, (event) => {
                    //  Do nothing if the content is already loaded.
                    if (Content.cachedDataExists(target) == false)
                        Content.load(target);
                }, "mouseleave");
            });

			if (allTargetsInContainer.length > 0) {
				/*  Set up handler to remove hover event listeners from all
					the chosen targets in the document.
					*/
				GW.notificationCenter.addHandlerForEvent("Extracts.cleanupDidComplete", (info) => {
					allTargetsInContainer.forEach(target => {
						if (target.removeContentLoadEvents) {
							target.removeContentLoadEvents();
							target.removeContentLoadEvents = null;
						}
					});
				}, { once: true });
            }
        } else { // if (Extracts.popFrameProvider == Popins)
            //  Add click event listeners to all the chosen targets.
            allTargetsInContainer.forEach(target => {
                target.addEventListener("click", target.contentLoad_click = (event) => {
                    //  Do nothing if the content is already loaded.
                    if (Content.cachedDataExists(target) == false)
                        Content.load(target);
                });
            });

            /*  Set up handler to remove click event listeners from all
                the annotated targets in the document.
                */
            GW.notificationCenter.addHandlerForEvent("Extracts.cleanupDidComplete", (info) => {
                allTargetsInContainer.forEach(target => {
                    target.removeEventListener("click", target.contentLoad_click);
                });
            }, { once: true });
        }
    },
};
