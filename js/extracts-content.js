if (window.Extracts) {
    /*=-----------=*/
    /*= CITATIONS =*/
    /*=-----------=*/

    Extracts.targetTypeDefinitions.insertBefore([
        "CITATION",
        "isCitation",
        null,
        "citationForTarget",
        "footnote"
    ], (def => def[0] == "LOCAL_PAGE"));

    Extracts.isCitation = (target) => {
        return target.classList.contains("footnote-ref");
    };

    Extracts.citationForTarget = (target) => {
        GWLog("Extracts.citationForTarget", "extracts-content.js", 2);

        return Extracts.localTranscludeForTarget(target, (blockElement) => {
            return target.hash.startsWith("#sn")
                   ? blockElement.querySelector(".sidenote-inner-wrapper").innerHTML
                   : blockElement.innerHTML;
        });
    };

    Extracts.titleForPopFrame_CITATION = (popFrame) => {
        let target = popFrame.spawningTarget;
        let footnoteNumber = target.querySelector("sup").textContent;
        let popFrameTitleText = `Footnote #${footnoteNumber}`;

        return Extracts.standardPopFrameTitleElementForTarget(target, popFrameTitleText);
    };

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

    Extracts.rewritePopFrameContent_CITATION = (popFrame) => {
        let target = popFrame.spawningTarget;

        //  Fire a contentDidLoad event.
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "Extracts.rewritePopFrameContent_CITATION",
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

    /*=---------------------=*/
    /*= CITATIONS BACKLINKS =*/
    /*=---------------------=*/

    Extracts.targetTypeDefinitions.insertBefore([
        "CITATION_BACK_LINK",
        "isCitationBackLink",
        null,
        "citationBackLinkForTarget",
        "citation-context"
    ], (def => def[0] == "LOCAL_PAGE"));

    Extracts.isCitationBackLink = (target) => {
        return target.classList.contains("footnote-back");
    };

    Extracts.citationBackLinkForTarget = (target) => {
        GWLog("Extracts.citationBackLinkForTarget", "extracts-content.js", 2);

        return Extracts.localTranscludeForTarget(target);
    };

    Extracts.testTarget_CITATION_BACK_LINK = (target) => {
        return (Extracts.popFrameProvider != Popins);
    };

    Extracts.preparePopup_CITATION_BACK_LINK = (popup) => {
        let target = popup.spawningTarget;

        //  Do not spawn citation context popup if citation is visible.
        if (Popups.isVisible(Extracts.targetDocument(target).querySelector(decodeURIComponent(target.hash))))
            return null;

        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    };

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
            isMainDocument: false,
            needsRewrite: false,
            clickable: false,
            collapseAllowed: false,
            isCollapseBlock: false,
            isFullPage: false,
            location: Extracts.locationForTarget(target),
            fullWidthPossible: false
        });
    }

    /*=---------------=*/
    /*= REMOTE VIDEOS =*/
    /*=---------------=*/

    Extracts.targetTypeDefinitions.insertBefore([
        "VIDEO",
        "isVideoLink",
        "has-content",
        "videoForTarget",
        "video object"
    ], (def => def[0] == "LOCAL_PAGE"));

    Extracts.youtubeId = (href) => {
        let match = href.match(/^.*(youtu.be\/|v\/|u\/\w\/|embed\/|watch\?v=|\&v=)([^#\&\?]*).*/);
        if (match && match[2].length == 11) {
            return match[2];
        } else {
            return null;
        }
    };

    Extracts.isVideoLink = (target) => {
        if (Extracts.isAnnotatedLink(target))
            return false;

        if ([ "www.youtube.com", "youtube.com", "youtu.be" ].includes(target.hostname)) {
            return (Extracts.youtubeId(target.href) != null);
        } else {
            return false;
        }
    };

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
        "LOCAL_VIDEO",
        "isLocalVideoLink",
        "has-content",
        "localVideoForTarget",
        "video object"
    ], (def => def[0] == "LOCAL_PAGE"));

    Extracts.videoFileExtensions = [ "mp4" ];

    Extracts.videoMaxWidth = 634.0;
    Extracts.videoMaxHeight = 474.0;

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

    Extracts.preparePopup_LOCAL_VIDEO = (popup) => {
        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    };

    Extracts.rewritePopFrameContent_LOCAL_VIDEO = (popFrame) => {
        //  Loading spinner.
        Extracts.setLoadingSpinner(popFrame);
    };

    /*=-----------------------=*/
    /*= LOCALLY HOSTED IMAGES =*/
    /*=-----------------------=*/

    Extracts.targetTypeDefinitions.insertBefore([
        "LOCAL_IMAGE",
        "isLocalImageLink",
        "has-content",
        "localImageForTarget",
        "image object"
    ], (def => def[0] == "LOCAL_PAGE"));

    Extracts.imageFileExtensions = [ "bmp", "gif", "ico", "jpeg", "jpg", "png", "svg" ];

    Extracts.imageMaxWidth = 634.0;
    Extracts.imageMaxHeight = 474.0;

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

    Extracts.preparePopup_LOCAL_IMAGE = (popup) => {
        //  Mini title bar.
        popup.classList.add("mini-title-bar");

        return popup;
    };

    Extracts.rewritePopFrameContent_LOCAL_IMAGE = (popFrame) => {
        //  Remove extraneous classes from images in image pop-frames.
        popFrame.querySelector("img").classList.remove("has-annotation", "has-content", "link-self", "link-local");

        //  Loading spinner.
        Extracts.setLoadingSpinner(popFrame);
    };

    Extracts.rewritePopinContent_LOCAL_IMAGE = (popin) => {
        Extracts.rewritePopFrameContent_LOCAL_IMAGE(popin);

        //  Remove extraneous classes from images in image popins.
        popin.querySelector("img").classList.remove("spawns-popin");
    };

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
        "LOCAL_DOCUMENT",
        "isLocalDocumentLink",
        "has-content",
        "localDocumentForTarget",
        "local-document object"
    ], (def => def[0] == "LOCAL_PAGE"));

    Extracts.isLocalDocumentLink = (target) => {
        if (   target.hostname != location.hostname
            || Extracts.isAnnotatedLink(target))
            return false;

        return (   target.pathname.startsWith("/docs/www/")
                || (   target.pathname.startsWith("/docs/")
                    && target.pathname.match(/\.(html|pdf)$/i) != null));
    };

    Extracts.localDocumentForTarget = (target) => {
        GWLog("Extracts.localDocumentForTarget", "extracts-content.js", 2);

        if (target.href.match(/\.pdf(#|$)/) != null) {
            let data = target.href + (target.href.includes("#") ? "&" : "#") + "view=FitH";
            return `<object data="${data}"></object>`;
        } else {
            return `<iframe src="${target.href}" frameborder="0" sandbox="allow-same-origin" referrerpolicy="same-origin"></iframe>`;
        }
    };

    Extracts.testTarget_LOCAL_DOCUMENT = (target) => {
        return (!(   Extracts.popFrameProvider == Popins
                  && target.href.match(/\.pdf(#|$)/) != null));
    };

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
        "LOCAL_CODE_FILE",
        "isLocalCodeFileLink",
        "has-content",
        "localCodeFileForTarget",
        "local-code-file"
    ], (def => def[0] == "LOCAL_PAGE"));

    Extracts.codeFileExtensions = [ "R", "css", "hs", "js", "patch", "sh", "php", "conf", "html",
                                    "opml", "xml",
                                    /* Non-syntax highlighted (due to lack of known format or potential size): */
                                    "txt", "json", "jsonl", "csv" ];

    Extracts.isLocalCodeFileLink = (target) => {
        if (   target.hostname != location.hostname
            || Extracts.isAnnotatedLink(target))
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
    Extracts.localCodeFileForTarget = (target) => {
        GWLog("Extracts.localCodeFileForTarget", "extracts-content.js", 2);

        let setPopFrameContent = Popups.setPopFrameContent;

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
        "FOREIGN_SITE",
        "isForeignSiteLink",
        "has-content",
        "foreignSiteForTarget",
        "foreign-site object"
    ], (def => def[0] == "LOCAL_PAGE"));

    Extracts.qualifyingForeignDomains = [
        "www.greaterwrong.com",
        "greaterwrong.com",
        "www.lesswrong.com",
        "lesswrong.com",
        /(.+?)\.wikipedia\.org/
    ];

    Extracts.blacklistedForeignDomains = [
    ];

    Extracts.isForeignSiteLink = (target) => {
        if (   target.hostname == location.hostname
            || Extracts.isAnnotatedLink(target))
            return false;

        return (   Extracts.qualifyingForeignDomains.includes(target.hostname)
                || Extracts.qualifyingForeignDomains.findIndex(domainPattern => (domainPattern instanceof RegExp && domainPattern.test(target.hostname) == true)) != -1)
            && !Extracts.blacklistedForeignDomains.includes(target.hostname);
    };

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

    Extracts.rewritePopFrameContent_FOREIGN_SITE = (popFrame) => {
        //  Loading spinner.
        Extracts.setLoadingSpinner(popFrame);
    };
}
