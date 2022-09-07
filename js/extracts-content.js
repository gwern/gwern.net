/***************************************/
/*  Events fired by extracts-content.js:

    GW.contentDidLoad {
            source: "Extracts.rewritePopFrameContent_CITATION"
            document:
                The `document` property of the citation pop-frame.
            loadLocation:
                URL of (i.e., anchor-link to) the footnote (or sidenote; this
                depends on whether the page in which the citation appears -
                which may not necessarily be the main page, as citations may
                also occur in embedded pages - is currently in sidenotes mode
                or not).
            baseLocation:
            	Same as loadLocation.
            flags:
                0 (no flags set)
        }
        Fired when a citation (i.e., footnote) pop-frame has been filled with
        content (i.e., the footnote), at the last stage of preparing the
        pop-frame for spawning (being injected into the page and positioned).

        (See rewrite.js for more information about the keys and values of the
         GW.contentDidLoad event.)

    GW.contentDidLoad {
            source: "Extracts.rewritePopupContent_CITATION_BACK_LINK"
            document:
                The `document` property of the citation back-link popup.
            loadLocation:
                URL of (i.e., anchor-link to) the citation which references the
                footnote/sidenote which spawned the popup. (If there are
                multiple instances of the citation on the page, this will be the
                URL of the first one, and that is what the popup will contain.)
            baseLocation:
            	Same as loadLocation.
            flags:
                0 (no flags set)
        }
        Fired when a citation back-link popup has been filled with content
        (i.e., the text surrounding the reference which links to the footnote),
        at the last stage of preparing the popup for spawning (being
        injected into the page and positioned).

        (See rewrite.js for more information about the keys and values of the
         GW.contentDidLoad event.)

    GW.contentDidLoad {
            source: "Extracts.rewritePopFrameContent_AUX_LINKS_LINK"
            document:
                The `document` property of the aux-links pop-frame.
            loadLocation:
                URL of the aux-links source file.
            baseLocation:
            	Same as loadLocation.
            flags:
                0 (no flags set)
        }
        Fired at the last stage of preparing an aux-links pop-frame for spawning
        (after the pop-frame’s content has been loaded from the local aux-links
        frame cache).

        (See rewrite.js for more information about the keys and values of the
         GW.contentDidLoad event.)

    GW.contentDidLoad {
            source: "Extracts.refreshPopFrameAfterAuxLinksLoad"
            document:
                A DocumentFragment containing the aux-links elements.
            loadLocation:
                URL of the aux-links source file.
            baseLocation:
            	Same as loadLocation.
            flags: GW.contentDidLoadEventFlags.needsRewrite
        }
        Fired when the content of the aux-links pop-frame has been constructed,
        but not yet injected into a pop-frame.

        (See rewrite.js for more information about the keys and values of the
         GW.contentDidLoad event.)
*/

Extracts = { ...Extracts, 
	//	Called by: Extracts.videoForTarget
	//	Called by: Extracts.localDocumentForTarget
	//	Called by: Extracts.foreignSiteForTarget
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
    auxLinksCache: { },

    //  Called by: Extracts.isAuxLinksLink
    //  Called by: Extracts.titleForPopFrame_AUX_LINKS_LINK
    auxLinksLinkType: (target) => {
        if (target.pathname.startsWith("/metadata/annotations/") == false)
            return null;

        return /^\/metadata\/annotations\/([^\/]+)/.exec(target.pathname)[1];
    },

    //  Called by: Extracts.isLocalCodeFileLink
    //  Called by: extracts.js (as `predicateFunctionName`)
    isAuxLinksLink: (target) => {
        let auxLinksLinkType = Extracts.auxLinksLinkType(target);
        return (auxLinksLinkType && target.classList.contains(auxLinksLinkType));
    },

    /*  Backlinks, similar-links, etc.
     */
    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    auxLinksForTarget: (target) => {
        GWLog("Extracts.auxLinksForTarget", "extracts-content.js", 2);

        if (Extracts.auxLinksCache[target.pathname]) {
            return newDocument(Extracts.auxLinksCache[target.pathname]);
        } else {
            Extracts.refreshPopFrameAfterAuxLinksLoad(target);

            return newDocument();
        }
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_AUX_LINKS_LINK: (popFrame) => {
        let target = popFrame.spawningTarget;

        //  Fire a contentDidLoad event.
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "Extracts.rewritePopFrameContent_AUX_LINKS_LINK",
            document: popFrame.document,
            loadLocation: Extracts.locationForTarget(target),
            baseLocation: Extracts.locationForTarget(target),
            flags: 0
        });
    },

    /*  Page or document for whom the aux-links are.
     */
    //  Called by: Extracts.titleForPopFrame_AUX_LINKS_LINK
    targetOfAuxLinksLink: (target) => {
        return decodeURIComponent(decodeURIComponent(/\/metadata\/annotations\/[^\/]+?\/(.+?)\.html$/.exec(target.pathname)[1]));
    },

    //  Called by: extracts.js (as `titleForPopFrame_${targetTypeName}`)
    titleForPopFrame_AUX_LINKS_LINK: (popFrame) => {
        let target = popFrame.spawningTarget;
        let targetPage = Extracts.targetOfAuxLinksLink(target);
        let auxLinksLinkType = Extracts.auxLinksLinkType(target);
        switch (auxLinksLinkType) {
            case "backlinks":
                return `${targetPage} (Backlinks)`;
            case "similars":
                return `${targetPage} (Similar)`;
            default:
                return `${targetPage}`;
        }
    },

    /*  Refresh (respawn or reload) a pop-frame for an aux-link link after the
        aux-links source loads.
     */
    //  Called by: Extracts.auxLinksForTarget
    refreshPopFrameAfterAuxLinksLoad: (target) => {
        GWLog("Extracts.refreshPopFrameAfterAuxLinksLoad", "extracts-content.js", 2);

        target.popFrame.classList.toggle("loading", true);

        doAjax({
            location: target.href,
            onSuccess: (event) => {
                //  Cache the aux-links source.
                Extracts.auxLinksCache[target.pathname] = newDocument(event.target.responseText);

                /*  Trigger the rewrite pass by firing the requisite event.
                    */
                GW.notificationCenter.fireEvent("GW.contentDidLoad", {
                    source: "Extracts.refreshPopFrameAfterAuxLinksLoad",
                    document: Extracts.auxLinksCache[target.pathname],
					loadLocation: Extracts.locationForTarget(target),
					baseLocation: Extracts.locationForTarget(target),
                    flags: GW.contentDidLoadEventFlags.needsRewrite
                });

				Extracts.postRefreshSuccessUpdatePopFrameForTarget(target);
            },
            onFailure: (event) => {
                Extracts.postRefreshFailureUpdatePopFrameForTarget(target);
            }
        });
    }
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

        return Extracts.localTranscludeForTarget(target, (blockElement) => {
            return target.hash.startsWith("#sn")
                   ? blockElement.querySelector(".sidenote-inner-wrapper").childNodes
                   : blockElement.childNodes;
        }, true);
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
    },

    //  Called by: extracts.js (as `rewritePopFrameContent_${targetTypeName}`)
    rewritePopFrameContent_CITATION: (popFrame) => {
        let target = popFrame.spawningTarget;

		//	Remove back-link and self-link.
		popFrame.body.querySelector(".footnote-self-link").remove();
		popFrame.body.querySelector(".footnote-back").remove();

        //  Fire a contentDidLoad event.
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "Extracts.rewritePopFrameContent_CITATION",
            document: popFrame.document,
            loadLocation: Extracts.locationForTarget(target),
            baseLocation: Extracts.locationForTarget(target),
            flags: 0
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

        return Extracts.localTranscludeForTarget(target, null, true);
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
        	&& Popups.isVisible(targetDocument.querySelector(selectorFromHash(target.hash))))
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
        popup.body.querySelectorAll(".footnote-ref.targeted").forEach(targetedCitation => {
            targetedCitation.classList.remove("targeted");
        });
        //  In the popup, the citation for which context is being shown.
        let citationInPopup = popup.body.querySelector(selectorFromHash(target.hash));
        //  Highlight the citation.
        citationInPopup.classList.add("targeted");

        //  Fire a contentDidLoad event.
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "Extracts.rewritePopupContent_CITATION_BACK_LINK",
            document: popup.document,
            loadLocation: Extracts.locationForTarget(target),
            baseLocation: Extracts.locationForTarget(target),
            flags: 0
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

    // These variables appear to currently be unused. —SA, 2022-01-31
//  Extracts.videoMaxWidth = 634.0;
//  Extracts.videoMaxHeight = 474.0;

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
								loading="lazy"
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
        popFrame.body.querySelector("img").classList.remove("link-local", "link-self", 
        	"has-annotation", "has-annotation-partial", "has-content");

        //  Loading spinner.
        Extracts.setLoadingSpinner(popFrame);
    },

    //  Called by: extracts.js (as `rewritePopupContent_${targetTypeName}`)
    rewritePopinContent_LOCAL_IMAGE: (popin) => {
        Extracts.rewritePopFrameContent_LOCAL_IMAGE(popin);

        //  Remove extraneous classes from images in image popins.
        popin.body.querySelector("img").classList.remove("spawns-popin");
    },

    //  Called by: extracts.js (as `rewritePopinContent_${targetTypeName}`)
    rewritePopupContent_LOCAL_IMAGE: (popup) => {
        Extracts.rewritePopFrameContent_LOCAL_IMAGE(popup);

        //  Remove extraneous classes from images in image popups.
        popup.body.querySelector("img").classList.remove("spawns-popup");

        if (popup.body.querySelector("img[width][height]"))
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
        let iframe = popFrame.body.querySelector("iframe");
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
	codeFilesCache: { },

    //  Used in: Extracts.isLocalCodeFileLink
    codeFileExtensions: [
        // truncated at 1000 lines for preview
        "bash", "c", "conf", "css", "csv", "diff", "hs", "html", "js", "json", "jsonl", "opml",
        "page", "patch", "php", "py", "R", "sh", "xml", "yaml",
        // Non-syntax highlighted (due to lack of known format), but truncated:
        "txt"
    ],

    //  Called by: extracts.js (as `predicateFunctionName`)
    isLocalCodeFileLink: (target) => {
        if (   target.hostname != location.hostname
            || Extracts.isAnnotatedLink(target))
            return false;

        if (Extracts.isAuxLinksLink(target))
            return false;

        let codeFileURLRegExp = new RegExp(
              '\\.('
            + Extracts.codeFileExtensions.join("|")
            + ')$'
        , 'i');
        return (target.pathname.match(codeFileURLRegExp) != null);
    },

    /*  We first try to retrieve a syntax-highlighted version of the given code
        file, stored on the server as an HTML fragment. If present, we embed
        that. If there’s no such fragment, then we just embed the contents of
        the actual code file, in a <pre>-wrapped <code> element.
     */
    //  Called by: extracts.js (as `popFrameFillFunctionName`)
    localCodeFileForTarget: (target) => {
        GWLog("Extracts.localCodeFileForTarget", "extracts-content.js", 2);

		if (Extracts.codeFilesCache[target.pathname]) {
			return newDocument(Extracts.codeFilesCache[target.pathname]);
		} else {
			Extracts.refreshPopFrameAfterCodeFileLoads(target);

            return newDocument();
		}
	},

    /*  Refresh (respawn or reload) a pop-frame for a local code file after the
        code file loads.
     */
    //  Called by: Extracts.localCodeFileForTarget
    refreshPopFrameAfterCodeFileLoads: (target) => {
        GWLog("Extracts.refreshPopFrameAfterCodeFileLoads", "extracts-content.js", 2);

        target.popFrame.classList.toggle("loading", true);

        doAjax({
            location: target.href + ".html",
            onSuccess: (event) => {
				//	Cache the code file.
				Extracts.codeFilesCache[target.pathname] = newDocument(event.target.responseText);

				Extracts.postRefreshSuccessUpdatePopFrameForTarget(target);
            },
            onFailure: (event) => {
                doAjax({
                    location: target.href,
                    onSuccess: (event) => {
                        let htmlEncodedResponse = event.target.responseText.replace(/[<>]/g, c => ('&#' + c.charCodeAt(0) + ';'));
                        let lines = htmlEncodedResponse.split("\n");
                        htmlEncodedResponse = lines.map(line => `<span class="line">${(line || "&nbsp;")}</span>`).join("\n");
                        let codeBlock = `<pre class="raw-code"><code>${htmlEncodedResponse}</code></pre>`;

						//	Cache the code file.
						Extracts.codeFilesCache[target.pathname] = newDocument(codeBlock);

						Extracts.postRefreshSuccessUpdatePopFrameForTarget(target);
                    },
                    onFailure: (event) => {
						Extracts.postRefreshFailureUpdatePopFrameForTarget(target);
                    }
                });
            }
        });

        return newDocument();
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

	//	Used in: Extracts.foreignSiteForTarget
	foreignSiteEmbedURLTransforms: [
		//  Less Wrong
		[	(url) => [ "www.lesswrong.com", "lesswrong.com", "www.greaterwrong.com", "greaterwrong.com" ].includes(url.hostname),
			(url) => { Extracts.foreignSiteEmbedURLTransform_GreaterWrong(url, "www"); } 
			],
		//  Alignment Forum
		[	(url) => (   [ "www.alignmentforum.org", "alignmentforum.org" ].includes(url.hostname)
					  || (   [ "www.greaterwrong.com", "greaterwrong.com" ].includes(url.hostname)
						  && url.searchParams.get("view") == "alignment-forum")),
			(url) => { Extracts.foreignSiteEmbedURLTransform_GreaterWrong(url, "www", "view=alignment-forum"); }
			],
		//  EA Forum
		[	(url) => [ "forum.effectivealtruism.org", "ea.greaterwrong.com" ].includes(url.hostname),
			(url) => { Extracts.foreignSiteEmbedURLTransform_GreaterWrong(url, "ea"); } 
			],
		//  Arbital
		[	(url) => [ "arbital.com", "arbital.greaterwrong.com" ].includes(url.hostname),
			(url) => { Extracts.foreignSiteEmbedURLTransform_GreaterWrong(url, "arbital"); } 
			],
		//  Wikipedia
		[	(url) => /(.+?)\.wikipedia\.org/.test(url.hostname) == true,
			(url) => {
				url.hostname = url.hostname.replace(/(.+?)(?:\.m)?\.wikipedia\.org/, "$1.m.wikipedia.org");
				if (!url.hash)
					url.hash = "#bodyContent";
			} ]
	],

	//	Used in: Extracts.foreignSiteEmbedURLTransforms
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

                    let doc = document.createElement("DIV");
                    doc.innerHTML = event.target.responseText;
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

                    target.popFrame.body.querySelector("iframe").srcdoc = doc.innerHTML;

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

		//	Transform URL for embedding.
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
