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

Extracts.targetTypeDefinitions.push([
	"LOCAL_PAGE",          // Type name
	"isLocalPageLink",     // Type predicate function
	"has-content",         // Target classes to add
	"localPageForTarget",  // Pop-frame fill function
	"local-page"           // Pop-frame classes
]);

/*=-------------=*/
/*= LOCAL PAGES =*/
/*=-------------=*/

Extracts = { ...Extracts, 
    /*  Local links (to sections of the current page, or other site pages).
     */
    //  Called by: Extracts.targetTypeInfo (as `predicateFunctionName`)
    isLocalPageLink: (target) => {
        return Content.contentTypes.localPage.matchesLink(target);
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
		let fullPage = !(   target.hash > ""
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
			Extracts.popFrameProvider.addClassesToPopFrame(target.popFrame, 
														   "full-page", 
														   "page-" + target.pathname.slice(1));
        }

		//	Ask for a content load, if need be.
		let contentIdentifier = Content.targetIdentifier(target);
		if (Content.cachedDataExists(contentIdentifier) == false)
			Content.load(contentIdentifier);

        //  Get content reference data (if it’s been loaded).
        let referenceData = Content.referenceDataForTarget(target);
        if (   referenceData == null
        	|| referenceData == "LOADING_FAILED") {
        	//	Handle if not loaded yet, or load failed.
	        Extracts.handleIncompleteReferenceData(target, referenceData, Content);

            return newDocument();
        }

        if (fullPage) {
			Extracts.popFrameProvider.addClassesToPopFrame(target.popFrame, ...referenceData.pageBodyClasses.split(" "));
			return newDocument(referenceData.content);
        } else {
//         	return newDocument(referenceData.targetBlock);
			let isBlockTranscludeLink = (   Transclude.isIncludeLink(referenceData.targetElement)
										 && (   referenceData.targetElement.classList.contains("include-block-context")
										 	 || (   referenceData.targetElement.id > ""
												 && referenceData.targetElement.classList.contains("include-identify-not") == false)));
			return (isBlockTranscludeLink
					? newDocument(referenceData.targetElement)
					: newDocument(referenceData.targetBlock));
        }
    },

    //  Called by: Extracts.titleForPopFrame (as `titleForPopFrame_${targetTypeName}`)
    titleForPopFrame_LOCAL_PAGE: (popFrame) => {
        GWLog("Extracts.titleForPopFrame_LOCAL_PAGE", "extracts.js", 2);

        let target = popFrame.spawningTarget;
        let referenceData = Content.referenceDataForTarget(target);

		let popFrameTitleText, titleLinkHref;
		if (referenceData == null) {
			popFrameTitleText = "";
			if (target.pathname != location.pathname)
				popFrameTitleText += target.pathname;
			if (popFrame.classList.contains("full-page") == false)
				popFrameTitleText += target.hash;

			titleLinkHref = target.href;
		} else {
			popFrameTitleText = popFrame.classList.contains("full-page")
								? referenceData.popFrameTitleTextShort
								: referenceData.popFrameTitleText;
			titleLinkHref = referenceData.titleLinkHref;
		}

		return Transclude.fillTemplateNamed("pop-frame-title-standard", {
			titleLinkHref:      titleLinkHref,
			popFrameTitleText:  popFrameTitleText
		}, {
			linkTarget:   ((Extracts.popFrameProvider == Popins) ? "_self" : "_blank"),
			whichTab:     ((Extracts.popFrameProvider == Popins) ? "current" : "new"),
			tabOrWindow:  (GW.isMobile() ? "tab" : "window")
		}).innerHTML;
    },

	preparePopFrame_LOCAL_PAGE: (popFrame) => {
        GWLog("Extracts.preparePopFrame_LOCAL_PAGE", "extracts.js", 2);

        let target = popFrame.spawningTarget;

		/*	For local content embed pop-frames, add handler to trigger 
			transcludes in source content when they trigger in the pop-frame.
		 */
		let identifier = Content.targetIdentifier(target);
		if (Content.cachedDataExists(identifier)) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				Content.updateCachedContent(identifier, (content) => {
					Transclude.allIncludeLinksInContainer(content).filter(includeLink => 
						includeLink.href == info.includeLink.href
					).forEach(includeLink => {
						Transclude.transclude(includeLink, true);
					});
				});

				/*	If the transcluded content makes up the entirety of the 
					pop-frame’s content, refresh the pop-frame after the load.
				 */
// 				if (   info.container.parentElement == popFrame.body
// 					&& info.container.parentElement.children.length == 2)
// 					Extracts.postRefreshSuccessUpdatePopFrameForTarget(target);				
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
    rewritePopFrameContent_LOCAL_PAGE: (popFrame) => {
        GWLog("Extracts.rewritePopFrameContent_LOCAL_PAGE", "extracts.js", 2);

        let target = popFrame.spawningTarget;

		//	Make collapse blocks expand on hover.
		popFrame.document.querySelectorAll(".collapse").forEach(collapseBlock => {
			collapseBlock.classList.add("expand-on-hover");
			updateDisclosureButtonTitleForCollapseBlock(collapseBlock);
		});

		//	Make first image load eagerly.
		let firstImage = (   popFrame.body.querySelector(".page-thumbnail")
						  || popFrame.body.querySelector("figure img"))
		if (firstImage) {
			firstImage.loading = "eager";
			firstImage.decoding = "sync";
		}

		//	Lazy-loading of adjacent sections.
		//	WARNING: Experimental code!
// 		if (target.hash > "") {
// 			requestAnimationFrame(() => {
// 				Extracts.loadAdjacentSections(popFrame, "next,prev");
// 			});
// 		}

        //  Fire a contentDidInject event.
        GW.notificationCenter.fireEvent("GW.contentDidInject", {
            source: "Extracts.rewritePopFrameContent_LOCAL_PAGE",
            container: popFrame.body,
            document: popFrame.document,
            loadLocation: new URL(target.href)
        });

        //  Scroll to the target.
        Extracts.scrollToTargetedElementInPopFrame(target, popFrame);
    },

    //  Called by: Extracts.rewritePopupContent (as `rewritePopupContent_${targetTypeName}`)
    rewritePopupContent_LOCAL_PAGE: (popup) => {
        GWLog("Extracts.rewritePopupContent_LOCAL_PAGE", "extracts.js", 2);

        let target = popup.spawningTarget;

		let referenceData = Content.referenceDataForTarget(target);
		if (referenceData) {
			//	Insert page thumbnail into page abstract.
			if (   referenceData.pageThumbnailHTML
				&& popup.document.querySelector("img.page-thumbnail") == null) {
				let pageAbstract = popup.document.querySelector("#page-metadata + .abstract blockquote");
				if (pageAbstract)
					pageAbstract.insertAdjacentHTML("afterbegin", `<figure>${referenceData.pageThumbnailHTML}</figure>`);
			}
		}

        //  Make anchorlinks scroll popup instead of opening normally.
		Extracts.constrainLinkClickBehaviorInPopFrame(popup);

        Extracts.rewritePopFrameContent_LOCAL_PAGE(popup);
    },

    //  Called by: Extracts.rewritePopinContent (as `rewritePopinContent_${targetTypeName}`)
    rewritePopinContent_LOCAL_PAGE: (popin) => {
        GWLog("Extracts.rewritePopinContent_LOCAL_PAGE", "extracts.js", 2);

        /*  Make anchorlinks scroll popin instead of opening normally
        	(but only for non-popin-spawning anchorlinks).
         */
		Extracts.constrainLinkClickBehaviorInPopFrame(popin, (link => link.classList.contains("no-popin")));

        Extracts.rewritePopFrameContent_LOCAL_PAGE(popin);
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
				loadLocation: new URL(target.href)
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
				loadLocation: new URL(target.href)
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
				loadLocation: new URL(target.href)
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

        let auxLinksLinkType = AuxLinks.auxLinksLinkType(target);

        return newDocument(`<a href="${target.href}" class="${auxLinksLinkType} include-strict"></a>`);
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
    rewritePopFrameContent_AUX_LINKS_LINK: (popFrame) => {
        let target = popFrame.spawningTarget;

        //  Fire a contentDidLoad event (to trigger transclude).
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "Extracts.rewritePopFrameContent_AUX_LINKS_LINK",
            container: popFrame.body,
            document: popFrame.document
        });
    },

    //  Called by: extracts.js (as `titleForPopFrame_${targetTypeName}`)
    titleForPopFrame_AUX_LINKS_LINK: (popFrame) => {
        let target = popFrame.spawningTarget;
        let targetPage = AuxLinks.targetOfAuxLinksLink(target);
        let auxLinksLinkType = AuxLinks.auxLinksLinkType(target);
        switch (auxLinksLinkType) {
            case "backlinks":
                return `${targetPage} (Backlinks)`;
            case "similars":
                return `${targetPage} (Similar links)`;
            case "link-bibliography":
                return `${targetPage} (Link bibliography)`;
            default:
                return `${targetPage}`;
        }
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

		let content = Extracts.localPageForTarget(target, true);

		//	Remove extraneous elements.
		content.querySelectorAll(".footnote-self-link, .footnote-back").forEach(element => {
			element.remove();
		});

		//	Fully unwrap, returning only footnote content.
		return newDocument(content.querySelector("li.footnote, .sidenote-inner-wrapper").childNodes);
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
        GW.notificationCenter.addHandlerForEvent("Popups.popupWillDespawn", Extracts.footnotePopupDespawnHandler = (info) => {
            target.classList.toggle("highlighted", false);
        });

        return popup;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_CITATION: (popFrame) => {
        let target = popFrame.spawningTarget;

        //  Fire a contentDidInject event.
        GW.notificationCenter.fireEvent("GW.contentDidInject", {
            source: "Extracts.rewritePopFrameContent_CITATION",
            container: popFrame.body,
            document: popFrame.document
        });
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
        if (   targetDocument
            && Popups.isVisible(targetElementInDocument(target, targetDocument)))
            return null;

        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    },

    //  Called by: extracts.js (as `rewritePopupContent_${targetTypeName}`)
    rewritePopupContent_CITATION_BACK_LINK: (popup) => {
        let target = popup.spawningTarget;

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

        //  Fire a contentDidInject event.
        GW.notificationCenter.fireEvent("GW.contentDidInject", {
            source: "Extracts.rewritePopupContent_CITATION_BACK_LINK",
            container: popup.body,
            document: popup.document
        });

        //  Scroll to the citation.
        Extracts.scrollToTargetedElementInPopFrame(target, popup);
    }
};

/*=---------------=*/
/*= REMOTE VIDEOS =*/
/*=---------------=*/

Extracts.targetTypeDefinitions.insertBefore([
    "VIDEO",                // Type name
    "isVideoLink",          // Type predicate function
    "has-content",          // Target classes to add
    "videoForTarget",       // Pop-frame fill function
    "video object"          // Pop-frame classes
], (def => def[0] == "LOCAL_PAGE"));

Extracts = { ...Extracts,
    // Called by: Extracts.isVideoLink
    // Called by: Extracts.videoForTarget
    youtubeId: (href) => {
        let match = href.match(/^.*(youtu.be\/|v\/|u\/\w\/|embed\/|watch\?v=|\&v=)([^#\&\?]*).*/);
        if (   match
            && match[2].length == 11) {
            return match[2];
        } else {
            return null;
        }
    },

    //  Called by: extracts.js (as `predicateFunctionName`)
    isVideoLink: (target) => {
        if (Extracts.isAnnotatedLink(target))
            return false;

        if ([ "www.youtube.com", "youtube.com", "youtu.be" ].includes(target.hostname)) {
            return (Extracts.youtubeId(target.href) != null);
        } else {
            return false;
        }
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    videoForTarget: (target) => {
        GWLog("Extracts.videoForTarget", "extracts-content.js", 2);

        let srcdocStyles =
              `<style>`
            + `* { padding: 0; margin: 0; overflow: hidden; } `
            + `html, body { height: 100%; } `
            + `img, span { position: absolute; width: 100%; top: 0; bottom: 0; margin: auto; } `
            + `span { height: 1.5em; text-align: center; font: 48px/1.5 sans-serif; color: white; text-shadow: 0 0 0.5em black; }`
            + `</style>`;

        let videoId = Extracts.youtubeId(target.href);
        let videoEmbedURL = new URL(`https://www.youtube.com/embed/${videoId}`);
        let placeholderImgSrc = `https://img.youtube.com/vi/${videoId}/hqdefault.jpg`;
        let playButtonHTML = `<span class='video-embed-play-button'>&#x25BA;</span>`;
        let srcdocHTML = `<a href='${videoEmbedURL.href}?autoplay=1'><img src='${placeholderImgSrc}'>${playButtonHTML}</a>`;

        //  `allow-same-origin` only for EXTERNAL videos, NOT local videos!
        return newDocument(Extracts.objectHTMLForURL(videoEmbedURL,
            `srcdoc="${srcdocStyles}${srcdocHTML}" sandbox="allow-scripts allow-same-origin" allowfullscreen`));
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
    //  Used in: Extracts.isLocalVideoLink
    videoFileExtensions: [ "mp4" ],

    //  Called by: extracts.js (as `predicateFunctionName`)
    isLocalVideoLink: (target) => {
        if (   target.hostname != location.hostname
            || Extracts.isAnnotatedLink(target))
            return false;

        let videoFileURLRegExp = new RegExp(
              '('
            + Extracts.videoFileExtensions.map(ext => `\\.${ext}`).join("|")
            + ')$'
        , 'i');
        return (target.pathname.match(videoFileURLRegExp) != null);
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    localVideoForTarget: (target) => {
        GWLog("Extracts.localVideoForTarget", "extracts-content.js", 2);

        return newDocument(
              `<video controls="controls" preload="none">`
            + `<source src="${target.href}">`
            + `</video>`);
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_LOCAL_VIDEO: (popup) => {
        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_VIDEO: (popFrame) => {
        //  Loading spinner.
        Extracts.setLoadingSpinner(popFrame);
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
    //  Used in: Extracts.isLocalImageLink
    imageFileExtensions: [ "bmp", "gif", "ico", "jpeg", "jpg", "png", "svg" ],

    //  Used in: Extracts.localImageForTarget
    imageMaxWidth: 634.0,
    imageMaxHeight: 474.0,

    //  Called by: extracts.js (as `predicateFunctionName`)
    isLocalImageLink: (target) => {
        if (   target.hostname != location.hostname
            || Extracts.isAnnotatedLink(target))
            return false;

        let imageFileURLRegExp = new RegExp(
              '('
            + Extracts.imageFileExtensions.map(ext => `\\.${ext}`).join("|")
            + ')$'
        , 'i');
        return (target.pathname.match(imageFileURLRegExp) != null);
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    localImageForTarget: (target) => {
        GWLog("Extracts.localImageForTarget", "extracts-content.js", 2);

        let width = target.dataset.imageWidth || 0;
        let height = target.dataset.imageHeight || 0;

        if (width > Extracts.imageMaxWidth) {
            height *= Extracts.imageMaxWidth / width;
            width = Extracts.imageMaxWidth;
        }
        if (height > Extracts.imageMaxHeight) {
            width *= Extracts.imageMaxHeight / height;
            height = Extracts.imageMaxHeight;
        }

        let styles = ``;
        if (   width > 0
            && height > 0)
            styles = `width="${width}" height="${height}" style="width: ${width}px; height: ${height}px;"`;

        //  Note that we pass in the original image-link’s classes - this is good for classes like ‘invert’.
        return newDocument(`<img
                                ${styles}
                                class="${target.classList}"
                                src="${target.href}"
                                loading="eager"
                                decoding="sync"
                                    >`);
    },

    //  Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    preparePopup_LOCAL_IMAGE: (popup) => {
        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    },

    //  Called by: Extracts.rewritePopinContent_LOCAL_IMAGE
    //  Called by: Extracts.rewritePopupContent_LOCAL_IMAGE
    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_IMAGE: (popFrame) => {
        //  Remove extraneous classes from images in image pop-frames.
        popFrame.document.querySelector("img").classList.remove("link-page", "link-self",
            "has-annotation", "has-annotation-partial", "has-content");

        //  Loading spinner.
        Extracts.setLoadingSpinner(popFrame);
    },

    //  Called by: extracts.js (as `rewritePopupContent_${targetTypeName}`)
    rewritePopinContent_LOCAL_IMAGE: (popin) => {
        Extracts.rewritePopFrameContent_LOCAL_IMAGE(popin);

        //  Remove extraneous classes from images in image popins.
        popin.document.querySelector("img").classList.remove("spawns-popin");
    },

    //  Called by: extracts.js (as `rewritePopinContent_${targetTypeName}`)
    rewritePopupContent_LOCAL_IMAGE: (popup) => {
        Extracts.rewritePopFrameContent_LOCAL_IMAGE(popup);

        //  Remove extraneous classes from images in image popups.
        popup.document.querySelector("img").classList.remove("spawns-popup");

        if (popup.document.querySelector("img[width][height]"))
            popup.classList.add("dimensions-specified");
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
        if (   target.hostname != location.hostname
            || Extracts.isAnnotatedLink(target))
            return false;

        return (   target.pathname.startsWith("/docs/www/")
                || (   target.pathname.startsWith("/docs/")
                    && target.pathname.match(/\.(html|pdf)$/i) != null));
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    localDocumentForTarget: (target) => {
        GWLog("Extracts.localDocumentForTarget", "extracts-content.js", 2);

        return newDocument(Extracts.objectHTMLForURL(target,
            `sandbox="allow-same-origin" referrerpolicy="same-origin"`));
    },

    /*  This “special testing function” is used to exclude certain targets which
        have already been categorized as (in this case) `LOCAL_DOCUMENT`
        targets. It returns false if the target is to be excluded, true
        otherwise. Excluded targets will not spawn pop-frames.
     */
    //  Called by: extracts.js (as `testTarget_${targetTypeInfo.typeName}`)
    testTarget_LOCAL_DOCUMENT: (target) => {
        return (!(   Extracts.popFrameProvider == Popins
                  && target.href.match(/\.pdf(#|$)/) != null));
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_DOCUMENT: (popFrame) => {
        //  Set title of popup from page title.
        let iframe = popFrame.document.querySelector("iframe");
        if (iframe) {
            iframe.addEventListener("load", (event) => {
                popFrame.titleBar.querySelector(".popframe-title-link").innerHTML = iframe.contentDocument.title;
            });
        }

        //  Loading spinner.
        Extracts.setLoadingSpinner(popFrame);
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
    	return Content.contentTypes.localCodeFile.matchesLink(target);
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    localCodeFileForTarget: (target) => {
        GWLog("Extracts.localCodeFileForTarget", "extracts-content.js", 2);

		//	Ask for a content load, if need be.
		let contentIdentifier = Content.targetIdentifier(target);
		if (Content.cachedDataExists(contentIdentifier) == false)
			Content.load(contentIdentifier);

        //  Get content reference data (if it’s been loaded).
        let referenceData = Content.referenceDataForTarget(target);
        if (   referenceData == null
        	|| referenceData == "LOADING_FAILED") {
        	//	Handle if not loaded yet, or load failed.
	        Extracts.handleIncompleteReferenceData(target, referenceData, Content);

            return newDocument();
        }

		return newDocument(referenceData.content);
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_LOCAL_CODE_FILE: (popFrame) => {
        //  Mark truncated code blocks, so layout can be adjusted properly.
        if (popFrame.body.lastElementChild.tagName == "P")
            popFrame.body.firstElementChild.classList.add("truncated");
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
        if (   target.hostname == location.hostname
            || Extracts.isAnnotatedLink(target))
            return false;

        return target.classList.contains("link-live");
    },

    //  Used in: Extracts.foreignSiteForTarget
    foreignSiteEmbedURLTransforms: [
        //  Less Wrong
        [   (url) => [ "www.lesswrong.com", "lesswrong.com", "www.greaterwrong.com", "greaterwrong.com" ].includes(url.hostname),
            (url) => { Extracts.foreignSiteEmbedURLTransform_GreaterWrong(url, "www"); }
            ],
        //  Alignment Forum
        [   (url) => (   [ "www.alignmentforum.org", "alignmentforum.org" ].includes(url.hostname)
                      || (   [ "www.greaterwrong.com", "greaterwrong.com" ].includes(url.hostname)
                          && url.searchParams.get("view") == "alignment-forum")),
            (url) => { Extracts.foreignSiteEmbedURLTransform_GreaterWrong(url, "www", "view=alignment-forum"); }
            ],
        //  EA Forum
        [   (url) => [ "forum.effectivealtruism.org", "ea.greaterwrong.com" ].includes(url.hostname),
            (url) => { Extracts.foreignSiteEmbedURLTransform_GreaterWrong(url, "ea"); }
            ],
        //  Arbital
        [   (url) => [ "arbital.com", "arbital.greaterwrong.com" ].includes(url.hostname),
            (url) => { Extracts.foreignSiteEmbedURLTransform_GreaterWrong(url, "arbital"); }
            ],
        //  Wikipedia
        [   (url) => /(.+?)\.wikipedia\.org/.test(url.hostname) == true,
            (url) => {
                url.hostname = url.hostname.replace(/(.+?)(?:\.m)?\.wikipedia\.org/, "$1.m.wikipedia.org");
                if (!url.hash)
                    url.hash = "#bodyContent";
            } ]
    ],

    //  Used in: Extracts.foreignSiteEmbedURLTransforms
    foreignSiteEmbedURLTransform_GreaterWrong: (url, subdomain = "www", searchString = null) => {
        url.hostname = `${subdomain}.greaterwrong.com`;
        url.search = (searchString
                      ? `${searchString}&`
                      : ``) +
                     "format=preview&theme=classic";
    },

    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    foreignSiteForTarget: (target) => {
        GWLog("Extracts.foreignSiteForTarget", "extracts-content.js", 2);

        let url = new URL(target.href);

        //  WARNING: EXPERIMENTAL FEATURE!
        if (localStorage.getItem("enable-embed-proxy") == "true") {
            let proxyURL = new URL("https://api.obormot.net/embed.php");

            doAjax({
                location: proxyURL.href,
                params: { url: url.href },
                onSuccess: (event) => {
                    if (Extracts.popFrameProvider.isSpawned(target.popFrame) == false)
                        return;

                    let doc = newElement("DIV", null, { "innerHTML": event.target.responseText });
                    doc.querySelectorAll("[href], [src]").forEach(element => {
                        if (element.href) {
                            let elementURL = new URL(element.href);
                            if (   elementURL.host == location.host
                                && !element.getAttribute("href").startsWith("#")) {
                                elementURL.host = url.host;
                                element.href = elementURL.href;
                            }
                        } else if (element.src) {
                            let elementURL = new URL(element.src);
                            if (elementURL.host == location.host) {
                                elementURL.host = url.host;
                                element.src = elementURL.href;
                            }
                        }
                    });

                    if (event.target.getResponseHeader("content-type").startsWith("text/plain"))
                        doc.innerHTML = `<pre>${doc.innerHTML}</pre>`;

                    target.popFrame.document.querySelector("iframe").srcdoc = doc.innerHTML;

                    target.popFrame.classList.toggle("loading", false);
                },
                onFailure: (event) => {
                    if (Extracts.popFrameProvider.isSpawned(target.popFrame) == false)
                        return;

                    target.popFrame.swapClasses([ "loading", "loading-failed" ], 1);
                }
            });

            return newDocument(`<iframe frameborder="0" sandbox="allow-scripts allow-popups"></iframe>`);
        }
        //  END EXPERIMENTAL SECTION

        //  Transform URL for embedding.
        /*  NOTE: the protocol *must* be https, not http; attempting to load
            http URLs from a page loaded over https, even in a shadow-root, will
            fail with a “Mixed Content” error. This way, we force https, in the
            hopes that the foreign site supports TLS, despite that the URL we’ve
            got is http. Unfortunately, some sites do not in fact support TLS;
            those sites will fail to load. This is unavoidable, and means that
            such sites cannot be live-embedded.
         */
        url.protocol = "https:";
        for ([ test, transform ] of Extracts.foreignSiteEmbedURLTransforms) {
            if (test(url)) {
                transform(url);
                break;
            }
        }

        return newDocument(Extracts.objectHTMLForURL(url, "sandbox"));
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_FOREIGN_SITE: (popFrame) => {
        //  Loading spinner.
        Extracts.setLoadingSpinner(popFrame);
    }
};

/*=------------------=*/
/*= CONTENT: HELPERS =*/
/*=------------------=*/

Extracts = { ...Extracts, 
    //  Called by: Extracts.videoForTarget
    //  Called by: Extracts.localDocumentForTarget
    //  Called by: Extracts.foreignSiteForTarget
    objectHTMLForURL: (url, additionalAttributes = null) => {
        if (url.href.match(/\.pdf(#|$)/) != null) {
            let data = url.href + (url.hash ? "&" : "#") + "view=FitH";
            return `<object
                        data="${data}"
                            ></object>`;
        } else {
            return `<iframe
                        src="${url.href}"
                        frameborder="0"
                      + ${(additionalAttributes ? (" " + additionalAttributes) : "")}
                            ></iframe>`;
        }
    },

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
        	Content.contentTypeForTarget(link) != null
        );

        if (Extracts.popFrameProvider == Popups) {
            //  Add hover event listeners to all the chosen targets.
            allTargetsInContainer.forEach(target => {
                target.removeContentLoadEvents = onEventAfterDelayDo(target, "mouseenter", Extracts.contentLoadHoverDelay, (event) => {
                    //  Get the unique identifier of the content for the target.
                    let contentIdentifier = Content.targetIdentifier(target);

                    //  Do nothing if the content is already loaded.
                    if (Content.cachedDataExists(contentIdentifier) == false)
                        Content.load(contentIdentifier);
                }, "mouseleave");
            });

			if (allTargetsInContainer.length > 0) {
				/*  Set up handler to remove hover event listeners from all
					the chosen targets in the document.
					*/
				GW.notificationCenter.addHandlerForEvent("Extracts.cleanupDidComplete", (info) => {
					allTargetsInContainer.forEach(target => {
						target.removeContentLoadEvents();
						target.removeContentLoadEvents = null;
					});
				}, { once: true });
            }
        } else { // if (Extracts.popFrameProvider == Popins)
            //  Add click event listeners to all the chosen targets.
            allTargetsInContainer.forEach(target => {
                target.addEventListener("click", target.contentLoad_click = (event) => {
                    //  Get the unique identifier of the content for the target.
                    let contentIdentifier = Content.targetIdentifier(target);

                    //  Do nothing if the content is already loaded.
                    if (Content.cachedDataExists(contentIdentifier) == false)
                        Content.load(contentIdentifier);
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
