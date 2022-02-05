/***************************************/
/*	Events fired by extracts-content.js:

	GW.contentDidLoad {
			source: "Extracts.rewritePopFrameContent_CITATION"
            document: 
            	The contentView of the pop-frame.
            location: 
            	URL of (i.e., anchor-link to) the footnote (or sidenote; this 
            	depends on whether the page in which the citation appears - 
            	which may not necessarily be the main page, as citations may 
            	also occur in embedded pages - is currently in sidenotes mode 
            	or not).
            isMainDocument: false
            needsRewrite: false
            clickable: false
            collapseAllowed: false
            isCollapseBlock: false
            isFullPage: false
            fullWidthPossible: false
		}
		Fired when a citation (i.e., footnote) pop-frame has been filled with 
		content (i.e., the footnote), at the last stage of preparing the 
		pop-frame for spawning (being injected into the page and positioned).

		(See rewrite.js for more information about the keys and values of the
		 GW.contentDidLoad event.)

	GW.contentDidLoad {
			source: "Extracts.rewritePopupContent_CITATION_BACK_LINK"
            document: 
            	The contentView of the popup (not “pop-frame”, but “popup”; 
            	citation back-links can only spawn popups; they act as simple 
            	anchor-links in popins mode).
            location: 
            	URL of (i.e., anchor-link to) the citation which references the
            	footnote/sidenote which spawned the popup. (If there are 
            	multiple instances of the citation on the page, this will be the
            	URL of the first one, and that is what the popup will contain.)
            isMainDocument: false
            needsRewrite: false
            clickable: false
            collapseAllowed: false
            isCollapseBlock: false
            isFullPage: false
            fullWidthPossible: false
		}
		Fired when a citation back-link pop-frame has been filled with content
		(i.e., the text surrounding the reference which links to the footnote), 
		at the last stage of preparing the pop-frame for spawning (being 
		injected into the page and positioned).

		(See rewrite.js for more information about the keys and values of the
		 GW.contentDidLoad event.)

	GW.contentDidLoad {
            source: "Extracts.rewritePopFrameContent_BACKLINKS_LINK"
            document: 
            	The contentView of the pop-frame.
            location: 
            	URL of the backlinks source file.
            isMainDocument: false
            needsRewrite: false
            clickable: false
            collapseAllowed: false
            isCollapseBlock: false
            isFullPage: false
            fullWidthPossible: false
        }
        Fired at the last stage of preparing a backlinks pop-frame for spawning 
        (after the pop-frame’s content has been loaded from the local backlinks
        frame cache).

		(See rewrite.js for more information about the keys and values of the
		 GW.contentDidLoad event.)

 	GW.contentDidLoad {
            source: "Extracts.refreshPopFrameAfterBacklinksLoad"
            document: 
            	The contentView of the pop-frame.
            location: 
            	URL of the backlinks source file.
            isMainDocument: false
            needsRewrite: true
            clickable: false
            collapseAllowed: false
            isCollapseBlock: false
            isFullPage: false
            fullWidthPossible: false
        }
        Fired at the last stage of preparing a backlinks pop-frame for spawning 
        (after the pop-frame’s content has been freshly loaded via a network 
        request).

		(See rewrite.js for more information about the keys and values of the
		 GW.contentDidLoad event.)
*/

if (window.Extracts) {
    /*=-----------------=*/
    /*= BACKLINKS LINKS =*/
    /*=-----------------=*/

    Extracts.targetTypeDefinitions.insertBefore([
        "BACKLINKS_LINK",				// Type name
        "isBacklinksLink",				// Type predicate function
        "has-content",					// Target classes to add
        "backlinksForTarget",			// Pop-frame fill function
        "backlinks"						// Pop-frame classes
    ], (def => def[0] == "LOCAL_PAGE"));

	Extracts.backlinksCache = { }

	//	Called by: extracts.js (as `predicateFunctionName`)
    Extracts.isBacklinksLink = (target) => {
        return (   target.classList.contains("backlink") 
        		&& target.pathname.startsWith("/metadata/annotations/backlinks/"));
    };

    /*  Backlinks for a page (from the ‘backlinks’ link in #page-metadata).
     */
    //	Called by: extracts.js (as `popFrameFillFunctionName`)
    Extracts.backlinksForTarget = (target) => {
        GWLog("Extracts.backlinksForTarget", "extracts-content.js", 2);

		let targetPage = Extracts.targetPageForBacklinksLink(target);
		
		if (Extracts.backlinksCache[targetPage]) {
			return Extracts.backlinksCache[targetPage].innerHTML;
		} else {
			Extracts.refreshPopFrameAfterBacklinksLoad(target);

			return `&nbsp;`;
		}
    };

	/*	Page whose backlinks a backlinks link contains.
	 */
	//	Called by: Extracts.backlinksForTarget
	Extracts.targetPageForBacklinksLink = (target) => {
		return decodeURIComponent(decodeURIComponent(/\/metadata\/annotations\/backlinks\/(.+?)\.html$/.exec(target.pathname)[1]));
	};

	//	Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    Extracts.rewritePopFrameContent_BACKLINKS_LINK = (popFrame) => {
        let target = popFrame.spawningTarget;

        //  Fire a contentDidLoad event.
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "Extracts.rewritePopFrameContent_BACKLINKS_LINK",
            document: popFrame.contentView,
            location: Extracts.locationForTarget(target),
            isMainDocument: false,
            needsRewrite: false,
            clickable: false,
            collapseAllowed: false,
            isCollapseBlock: false,
            isFullPage: false,
            fullWidthPossible: false
        });
    };

	//	Called by: extracts.js (as `titleForPopFrame_${targetTypeName}`)
    Extracts.titleForPopFrame_BACKLINKS_LINK = (popFrame) => {
        let target = popFrame.spawningTarget;
        let targetPage = Extracts.targetPageForBacklinksLink(target);
        return `${targetPage} (Backlinks)`;
    };

    /*  Refresh (respawn or reload) a pop-frame for a backlinks link after the
        backlinks source loads.
     */
    //	Called by: Extracts.backlinksForTarget
    Extracts.refreshPopFrameAfterBacklinksLoad = (target) => {
        GWLog("Extracts.refreshPopFrameAfterBacklinksLoad", "extracts-content.js", 2);

        target.popFrame.classList.toggle("loading", true);

        doAjax({
            location: target.href,
            onSuccess: (event) => {
                if (!target.popFrame)
                    return;

                //	Inject the backlinks source into the pop-frame.
                Extracts.popFrameProvider.setPopFrameContent(target.popFrame, event.target.responseText);

				//	Cache the backlinks source.
				let targetPage = Extracts.targetPageForBacklinksLink(target);
				Extracts.backlinksCache[targetPage] = target.popFrame.contentView;

                /*  Trigger the rewrite pass by firing the requisite event.
                    */
                GW.notificationCenter.fireEvent("GW.contentDidLoad", {
                    source: "Extracts.refreshPopFrameAfterBacklinksLoad",
                    document: target.popFrame.contentView,
                    location: Extracts.locationForTarget(target),
                    isMainDocument: false,
                    needsRewrite: true,
                    clickable: false,
                    collapseAllowed: false,
                    isCollapseBlock: false,
                    isFullPage: false,
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
    };

    /*=-----------=*/
    /*= CITATIONS =*/
    /*=-----------=*/

    Extracts.targetTypeDefinitions.insertBefore([
        "CITATION",				// Type name
        "isCitation",			// Type predicate function
        null,					// Target classes to add
        "citationForTarget",	// Pop-frame fill function
        "footnote"				// Pop-frame classes
    ], (def => def[0] == "LOCAL_PAGE"));

	//	Called by: extracts.js (as `predicateFunctionName`)
    Extracts.isCitation = (target) => {
        return target.classList.contains("footnote-ref");
    };

    //	Called by: extracts.js (as `popFrameFillFunctionName`)
    Extracts.citationForTarget = (target) => {
        GWLog("Extracts.citationForTarget", "extracts-content.js", 2);

        return Extracts.localTranscludeForTarget(target, (blockElement) => {
            return target.hash.startsWith("#sn")
                   ? blockElement.querySelector(".sidenote-inner-wrapper").innerHTML
                   : blockElement.innerHTML;
        });
    };

	//	Called by: extracts.js (as `titleForPopFrame_${targetTypeName}`)
    Extracts.titleForPopFrame_CITATION = (popFrame) => {
        let target = popFrame.spawningTarget;
        let footnoteNumber = target.querySelector("sup").textContent;
        let popFrameTitleText = `Footnote #${footnoteNumber}`;

        return Extracts.standardPopFrameTitleElementForTarget(target, popFrameTitleText);
    };

	//	Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    Extracts.preparePopup_CITATION = (popup) => {
        let target = popup.spawningTarget;

        /*  Do not spawn footnote popup if the {side|foot}note it points to is
            visible.
         */
        if (Array.from(allNotesForCitation(target)).findIndex(note => Popups.isVisible(note)) != -1)
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
    };

	//	Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    Extracts.rewritePopFrameContent_CITATION = (popFrame) => {
        let target = popFrame.spawningTarget;

        //  Fire a contentDidLoad event.
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "Extracts.rewritePopFrameContent_CITATION",
            document: popFrame.contentView,
            location: Extracts.locationForTarget(target),
            isMainDocument: false,
            needsRewrite: false,
            clickable: false,
            collapseAllowed: false,
            isCollapseBlock: false,
            isFullPage: false,
            fullWidthPossible: false
        });
    };

    /*=---------------------=*/
    /*= CITATIONS BACKLINKS =*/
    /*=---------------------=*/

    Extracts.targetTypeDefinitions.insertBefore([
        "CITATION_BACK_LINK",				// Type name
        "isCitationBackLink",				// Type predicate function
        null,								// Target classes to add
        "citationBackLinkForTarget",		// Pop-frame fill function
        "citation-context"					// Pop-frame classes
    ], (def => def[0] == "LOCAL_PAGE"));

	//	Called by: extracts.js (as `predicateFunctionName`)
    Extracts.isCitationBackLink = (target) => {
        return target.classList.contains("footnote-back");
    };

    //	Called by: extracts.js (as `popFrameFillFunctionName`)
    Extracts.citationBackLinkForTarget = (target) => {
        GWLog("Extracts.citationBackLinkForTarget", "extracts-content.js", 2);

        return Extracts.localTranscludeForTarget(target);
    };

	//	Called by: extracts.js (as `testTarget_${targetTypeInfo.typeName}`)
    Extracts.testTarget_CITATION_BACK_LINK = (target) => {
        return (Extracts.popFrameProvider != Popins);
    };

	//	Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    Extracts.preparePopup_CITATION_BACK_LINK = (popup) => {
        let target = popup.spawningTarget;

        //  Do not spawn citation context popup if citation is visible.
        if (Popups.isVisible(Extracts.targetDocument(target).querySelector(decodeURIComponent(target.hash))))
            return null;

        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    };

	//	Called by: extracts.js (as `rewritePopupContent_${targetTypeName}`)
    Extracts.rewritePopupContent_CITATION_BACK_LINK = (popup) => {
        let target = popup.spawningTarget;

        //  Highlight citation in popup.
        /*  Remove the .targeted class from a targeted citation (if any)
            inside the popup (to prevent confusion with the citation that
            the spawning link points to, which will be highlighted).
         */
        popup.querySelectorAll(".footnote-ref.targeted").forEach(targetedCitation => {
            targetedCitation.classList.remove("targeted");
        });
        //  In the popup, the citation for which context is being shown.
        let citationInPopup = popup.querySelector(decodeURIComponent(target.hash));
        //  Highlight the citation.
        citationInPopup.classList.add("targeted");
        //  Scroll to the citation.
        requestAnimationFrame(() => {
            Extracts.popFrameProvider.scrollElementIntoViewInPopFrame(citationInPopup, popup);
        });

        //  Fire a contentDidLoad event.
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "Extracts.rewritePopupContent_CITATION_BACK_LINK",
            document: popup.contentView,
            location: Extracts.locationForTarget(target),
            isMainDocument: false,
            needsRewrite: false,
            clickable: false,
            collapseAllowed: false,
            isCollapseBlock: false,
            isFullPage: false,
            fullWidthPossible: false
        });
    }

    /*=---------------=*/
    /*= REMOTE VIDEOS =*/
    /*=---------------=*/

    Extracts.targetTypeDefinitions.insertBefore([
        "VIDEO",				// Type name
        "isVideoLink",			// Type predicate function
        "has-content",			// Target classes to add
        "videoForTarget",		// Pop-frame fill function
        "video object"			// Pop-frame classes
    ], (def => def[0] == "LOCAL_PAGE"));

	// Called by: Extracts.isVideoLink
	// Called by: Extracts.videoForTarget
    Extracts.youtubeId = (href) => {
        let match = href.match(/^.*(youtu.be\/|v\/|u\/\w\/|embed\/|watch\?v=|\&v=)([^#\&\?]*).*/);
        if (match && match[2].length == 11) {
            return match[2];
        } else {
            return null;
        }
    };

	//	Called by: extracts.js (as `predicateFunctionName`)
    Extracts.isVideoLink = (target) => {
        if (Extracts.isAnnotatedLink(target))
            return false;

        if ([ "www.youtube.com", "youtube.com", "youtu.be" ].includes(target.hostname)) {
            return (Extracts.youtubeId(target.href) != null);
        } else {
            return false;
        }
    };

    //	Called by: extracts.js (as `popFrameFillFunctionName`)
    Extracts.videoForTarget = (target) => {
        GWLog("Extracts.videoForTarget", "extracts-content.js", 2);

        let videoId = Extracts.youtubeId(target.href);
        let videoEmbedURL = `https://www.youtube.com/embed/${videoId}`;
        let placeholderImgSrc = `https://img.youtube.com/vi/${videoId}/hqdefault.jpg`;
        let srcdocStyles = `<style>` +
            `* { padding: 0; margin: 0; overflow: hidden; }` +
            `html, body { height: 100%; } ` +
            `img, span { position: absolute; width: 100%; top: 0; bottom: 0; margin: auto; } ` +
            `span { height: 1.5em; text-align: center; font: 48px/1.5 sans-serif; color: white; text-shadow: 0 0 0.5em black; }` +
            `</style>`;
        let playButtonHTML = `<span class='video-embed-play-button'>&#x25BA;</span>`;
        let srcdocHTML = `<a href='${videoEmbedURL}?autoplay=1'><img src='${placeholderImgSrc}'>${playButtonHTML}</a>`;

        //  `allow-same-origin` only for EXTERNAL videos, NOT local videos!
        return `<iframe src="${videoEmbedURL}" srcdoc="${srcdocStyles}${srcdocHTML}" frameborder="0" allowfullscreen sandbox="allow-scripts allow-same-origin"></iframe>`;
    };

    /*=-----------------------=*/
    /*= LOCALLY HOSTED VIDEOS =*/
    /*=-----------------------=*/

    Extracts.targetTypeDefinitions.insertBefore([
        "LOCAL_VIDEO",				// Type name
        "isLocalVideoLink",			// Type predicate function
        "has-content",				// Target classes to add
        "localVideoForTarget",		// Pop-frame fill function
        "video object"				// Pop-frame class
    ], (def => def[0] == "LOCAL_PAGE"));

	//	Used in: Extracts.isLocalVideoLink
    Extracts.videoFileExtensions = [ "mp4" ];

	// These variables appear to currently be unused. —SA, 2022-01-31
//	Extracts.videoMaxWidth = 634.0;
//	Extracts.videoMaxHeight = 474.0;

	//	Called by: extracts.js (as `predicateFunctionName`)
    Extracts.isLocalVideoLink = (target) => {
        if (   target.hostname != location.hostname
            || Extracts.isAnnotatedLink(target))
            return false;

        let videoFileURLRegExp = new RegExp(
              '('
            + Extracts.videoFileExtensions.map(ext => `\\.${ext}`).join("|")
            + ')$'
        , 'i');
        return (target.pathname.match(videoFileURLRegExp) != null);
    };

    //	Called by: extracts.js (as `popFrameFillFunctionName`)
    Extracts.localVideoForTarget = (target) => {
        GWLog("Extracts.localVideoForTarget", "extracts-content.js", 2);

//      let width = target.dataset.imageWidth || 0;
//      let height = target.dataset.imageHeight || 0;
//
//      if (width > Extracts.imageMaxWidth) {
//          height *= Extracts.imageMaxWidth / width;
//          width = Extracts.imageMaxWidth;
//      }
//      if (height > Extracts.imageMaxHeight) {
//          width *= Extracts.imageMaxHeight / height;
//          height = Extracts.imageMaxHeight;
//      }
//
//      let styles = ``;
//      if (width > 0 && height > 0) {
//          styles = `width="${width}" height="${height}" style="width: ${width}px; height: ${height}px;"`;
//      }

        //  Note that we pass in the original image-link’s classes - this is good for classes like ‘invertible’.
//         return `<img ${styles} class="${target.classList}" src="${target.href}" loading="lazy">`;
        return `<video controls="controls" preload="none">` +
            `<source src="${target.href}">` +
            `</video>`;
    };

	//	Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    Extracts.preparePopup_LOCAL_VIDEO = (popup) => {
        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    };

	//	Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    Extracts.rewritePopFrameContent_LOCAL_VIDEO = (popFrame) => {
        //  Loading spinner.
        Extracts.setLoadingSpinner(popFrame);
    };

    /*=-----------------------=*/
    /*= LOCALLY HOSTED IMAGES =*/
    /*=-----------------------=*/

    Extracts.targetTypeDefinitions.insertBefore([
        "LOCAL_IMAGE",				// Type name
        "isLocalImageLink",			// Type predicate function
        "has-content",				// Target classes to add
        "localImageForTarget",		// Pop-frame fill function
        "image object"				// Pop-frame classes
    ], (def => def[0] == "LOCAL_PAGE"));

	//	Used in: Extracts.isLocalImageLink
    Extracts.imageFileExtensions = [ "bmp", "gif", "ico", "jpeg", "jpg", "png", "svg" ];

	//	Used in: Extracts.localImageForTarget
    Extracts.imageMaxWidth = 634.0;
    Extracts.imageMaxHeight = 474.0;

	//	Called by: extracts.js (as `predicateFunctionName`)
    Extracts.isLocalImageLink = (target) => {
        if (   target.hostname != location.hostname
            || Extracts.isAnnotatedLink(target))
            return false;

        let imageFileURLRegExp = new RegExp(
              '('
            + Extracts.imageFileExtensions.map(ext => `\\.${ext}`).join("|")
            + ')$'
        , 'i');
        return (target.pathname.match(imageFileURLRegExp) != null);
    };

    //	Called by: extracts.js (as `popFrameFillFunctionName`)
    Extracts.localImageForTarget = (target) => {
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
        if (width > 0 && height > 0)
            styles = `width="${width}" height="${height}" style="width: ${width}px; height: ${height}px;"`;

        //  Note that we pass in the original image-link’s classes - this is good for classes like ‘invertible’.
        return `<img ${styles} class="${target.classList}" src="${target.href}" loading="lazy">`;
    };

	//	Called by: extracts.js (as `preparePopup_${targetTypeName}`)
    Extracts.preparePopup_LOCAL_IMAGE = (popup) => {
        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    };

	//	Called by: Extracts.rewritePopinContent_LOCAL_IMAGE
	//	Called by: Extracts.rewritePopupContent_LOCAL_IMAGE
	//	Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    Extracts.rewritePopFrameContent_LOCAL_IMAGE = (popFrame) => {
        //  Remove extraneous classes from images in image pop-frames.
        popFrame.querySelector("img").classList.remove("has-annotation", "has-content", "link-self", "link-local");

        //  Loading spinner.
        Extracts.setLoadingSpinner(popFrame);
    };

	//	Called by: extracts.js (as `rewritePopupContent_${targetTypeName}`)
    Extracts.rewritePopinContent_LOCAL_IMAGE = (popin) => {
        Extracts.rewritePopFrameContent_LOCAL_IMAGE(popin);

        //  Remove extraneous classes from images in image popins.
        popin.querySelector("img").classList.remove("spawns-popin");
    };

	//	Called by: extracts.js (as `rewritePopinContent_${targetTypeName}`)
    Extracts.rewritePopupContent_LOCAL_IMAGE = (popup) => {
        Extracts.rewritePopFrameContent_LOCAL_IMAGE(popup);

        //  Remove extraneous classes from images in image popups.
        popup.querySelector("img").classList.remove("spawns-popup");

        if (popup.querySelector("img[width][height]"))
            popup.classList.add("dimensions-specified");
    };

    /*=--------------------------=*/
    /*= LOCALLY HOSTED DOCUMENTS =*/
    /*=--------------------------=*/

    Extracts.targetTypeDefinitions.insertBefore([
        "LOCAL_DOCUMENT",				// Type name
        "isLocalDocumentLink",			// Type predicate function
        "has-content",					// Target classes to add
        "localDocumentForTarget",		// Pop-frame fill function
        "local-document object"			// Pop-frame classes
    ], (def => def[0] == "LOCAL_PAGE"));

	//	Called by: extracts.js (as `predicateFunctionName`)
    Extracts.isLocalDocumentLink = (target) => {
        if (   target.hostname != location.hostname
            || Extracts.isAnnotatedLink(target))
            return false;

        return (   target.pathname.startsWith("/docs/www/")
                || (   target.pathname.startsWith("/docs/")
                    && target.pathname.match(/\.(html|pdf)$/i) != null));
    };

    //	Called by: extracts.js (as `popFrameFillFunctionName`)
    Extracts.localDocumentForTarget = (target) => {
        GWLog("Extracts.localDocumentForTarget", "extracts-content.js", 2);

        if (target.href.match(/\.pdf(#|$)/) != null) {
            let data = target.href + (target.href.includes("#") ? "&" : "#") + "view=FitH";
            return `<object data="${data}"></object>`;
        } else {
            return `<iframe src="${target.href}" frameborder="0" sandbox="allow-same-origin" referrerpolicy="same-origin"></iframe>`;
        }
    };

	//	Called by: extracts.js (as `testTarget_${targetTypeInfo.typeName}`)
    Extracts.testTarget_LOCAL_DOCUMENT = (target) => {
        return (!(   Extracts.popFrameProvider == Popins
                  && target.href.match(/\.pdf(#|$)/) != null));
    };

	//	Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    Extracts.rewritePopFrameContent_LOCAL_DOCUMENT = (popFrame) => {
        //  Set title of popup from page title.
        let iframe = popFrame.querySelector("iframe");
        if (iframe) {
            iframe.addEventListener("load", (event) => {
                popFrame.titleBar.querySelector(".popframe-title-link").innerHTML = iframe.contentDocument.title;
            });
        }

        //  Loading spinner.
        Extracts.setLoadingSpinner(popFrame);
    };

    /*=---------------------------=*/
    /*= LOCALLY HOSTED CODE FILES =*/
    /*=---------------------------=*/

    Extracts.targetTypeDefinitions.insertBefore([
        "LOCAL_CODE_FILE",				// Type name
        "isLocalCodeFileLink",			// Type predicate function
        "has-content",					// Target classes to add
        "localCodeFileForTarget",		// Pop-frame fill function
        "local-code-file"				// Pop-frame classes
    ], (def => def[0] == "LOCAL_PAGE"));

	//	Used in: Extracts.isLocalCodeFileLink
    Extracts.codeFileExtensions = [ 
    	"R", "css", "hs", "js", "patch", "sh", "php", "conf", "html", "opml", "xml",
		// Non-syntax highlighted (due to lack of known format or potential size):
		"txt", "json", "jsonl", "csv" 
	];

	//	Called by: extracts.js (as `predicateFunctionName`)
    Extracts.isLocalCodeFileLink = (target) => {
        if (   target.hostname != location.hostname
            || Extracts.isAnnotatedLink(target))
            return false;

        if (Extracts.isBacklinksLink(target))
        	return false;

        let codeFileURLRegExp = new RegExp(
              '('
            + Extracts.codeFileExtensions.map(ext => `\\.${ext}`).join("|")
            + ')$'
        , 'i');
        return (target.pathname.match(codeFileURLRegExp) != null);
    };

    /*  We first try to retrieve a syntax-highlighted version of the given code
        file, stored on the server as an HTML fragment. If present, we embed
        that. If there’s no such fragment, then we just embed the contents of
        the actual code file, in a <pre>-wrapped <code> element.
     */
    //	Called by: extracts.js (as `popFrameFillFunctionName`)
    Extracts.localCodeFileForTarget = (target) => {
        GWLog("Extracts.localCodeFileForTarget", "extracts-content.js", 2);

        let setPopFrameContent = Extracts.popFrameProvider.setPopFrameContent;

        target.popFrame.classList.toggle("loading", true);
        doAjax({
            location: target.href + ".html",
            onSuccess: (event) => {
                if (!target.popFrame)
                    return;

                target.popFrame.classList.toggle("loading", false);
                setPopFrameContent(target.popFrame, event.target.responseText);

                //  Do additional rewriting, if any.
                if (Extracts.popFrameProvider == Popups)
                    Extracts.rewritePopupContent(target.popup);
                else // if (Extracts.popFrameProvider == Popins)
                    Extracts.rewritePopinContent(target.popin);
            },
            onFailure: (event) => {
                doAjax({
                    location: target.href,
                    onSuccess: (event) => {
                        if (!target.popFrame)
                            return;

                        target.popFrame.classList.toggle("loading", false);

                        let htmlEncodedResponse = event.target.responseText.replace(/[<>]/g, c => ('&#' + c.charCodeAt(0) + ';'));
                        let lines = htmlEncodedResponse.split("\n");
                        htmlEncodedResponse = lines.map(line => `<span class="line">${(line || "&nbsp;")}</span>`).join("\n");

                        setPopFrameContent(target.popFrame, `<pre class="raw-code"><code>${htmlEncodedResponse}</code></pre>`);

                        //  Do additional rewriting, if any.
                        if (Extracts.popFrameProvider == Popups)
                            Extracts.rewritePopupContent(target.popup);
                        else // if (Extracts.popFrameProvider == Popins)
                            Extracts.rewritePopinContent(target.popin);
                    },
                    onFailure: (event) => {
                        if (!target.popFrame)
                            return;

                        target.popFrame.swapClasses([ "loading", "loading-failed" ], 1);
                    }
                });
            }
        });

        return `&nbsp;`;
    };

    /*=----------------=*/
    /*= OTHER WEBSITES =*/
    /*=----------------=*/

    Extracts.targetTypeDefinitions.insertBefore([
        "FOREIGN_SITE",				// Type name
        "isForeignSiteLink",		// Type predicate function
        "has-content",				// Target classes to add
        "foreignSiteForTarget",		// Pop-frame fill function
        "foreign-site object"		// Pop-frame classes
    ], (def => def[0] == "LOCAL_PAGE"));

    /* Domains which *would* be useful to live-popup, but set X-Frame-Options/Content-Security-Policy HTTP headers which mean browsers will refuse to load the popup (eg. https://support.mozilla.org/en-US/kb/xframe-neterror-page ):
       old.reddit.com, arxiv.org, www.biorxiv.org, www.medrxiv.org, github.com, github.io, news.ycombinator.com */

	//	Used in: Extracts.isForeignSiteLink
    Extracts.qualifyingForeignDomains = [
        "www.greaterwrong.com",
        "greaterwrong.com",
        "www.lesswrong.com",
        "lesswrong.com",
        /(.+?)\.wikipedia\.org/,
        "nitter.hu",
        /(.+?)\.eleuther\.ai/,
        "bmk.sh"
    ];

	//	Used in: Extracts.isForeignSiteLink
    Extracts.blacklistedForeignDomains = [
    ];

	//	Called by: extracts.js (as `predicateFunctionName`)
    Extracts.isForeignSiteLink = (target) => {
        if (   target.hostname == location.hostname
            || Extracts.isAnnotatedLink(target))
            return false;

        return (   Extracts.qualifyingForeignDomains.includes(target.hostname)
                || Extracts.qualifyingForeignDomains.findIndex(domainPattern => (domainPattern instanceof RegExp && domainPattern.test(target.hostname) == true)) != -1)
            && !Extracts.blacklistedForeignDomains.includes(target.hostname);
    };

    //	Called by: extracts.js (as `popFrameFillFunctionName`)
    Extracts.foreignSiteForTarget = (target) => {
        GWLog("Extracts.foreignSiteForTarget", "extracts-content.js", 2);

        let url = new URL(target.href);

        if ([ "www.lesswrong.com", "lesswrong.com", "www.greaterwrong.com", "greaterwrong.com" ].includes(url.hostname)) {
            url.protocol = "https:";
            url.hostname = "www.greaterwrong.com";
            url.search = "format=preview&theme=classic";
        } else if (/(.+?)\.wikipedia\.org/.test(url.hostname) == true) {
            url.protocol = "https:";
            url.hostname = url.hostname.replace(/(.+?)(?:\.m)?\.wikipedia\.org/, "$1.m.wikipedia.org");
            if (!url.hash)
                url.hash = "#bodyContent";
        } else {
            url.protocol = "https:";
        }

        return `<iframe src="${url.href}" frameborder="0" sandbox></iframe>`;
    };

	//	Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    Extracts.rewritePopFrameContent_FOREIGN_SITE = (popFrame) => {
        //  Loading spinner.
        Extracts.setLoadingSpinner(popFrame);
    };
}
