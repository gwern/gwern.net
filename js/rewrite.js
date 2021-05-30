/* Miscellaneous JS functions which run after the page loads to rewrite or adjust parts of the page. */
/* author: Said Achmiz */
/* license: MIT */

GW.rewriteFunctions = { };

/**********/
/* TABLES */
/**********/

/****************************************************************/
/*  Wrap each table in a div.table-wrapper (for layout purposes).
    */
function wrapTables(loadEventInfo) {
    GWLog("wrapTables", "rewrite.js", 1);

    let wrapperClass = "table-wrapper";
    loadEventInfo.document.querySelectorAll("table").forEach(table => {
        if (table.parentElement.tagName == "DIV" && table.parentElement.children.length == 1)
            table.parentElement.classList.toggle(wrapperClass, true);
        else
            table.outerHTML = `<div class="${wrapperClass}">` + table.outerHTML + `</div>`;
    });
}

/******************************************************************************/
/*  Wrap each full-width table in a div.full-width-table-wrapper, and also move
    the .collapse class (if any) from the outer wrapper to the table (for
    consistency).
    */
function wrapFullWidthTables(loadEventInfo) {
    GWLog("wrapFullWidthTables", "rewrite.js", 1);

    let fullWidthClass = "full-width";
    let fullWidthInnerWrapperClass = "full-width-table-inner-wrapper";
    loadEventInfo.document.querySelectorAll(`.table-wrapper.${fullWidthClass}`).forEach(fullWidthTableWrapper => {
        if (fullWidthTableWrapper.classList.contains("collapse")) {
            fullWidthTableWrapper.classList.remove("collapse");
            fullWidthTableWrapper.firstElementChild.classList.add("collapse");
        }

        fullWidthTableWrapper.innerHTML = `<div class="${fullWidthInnerWrapperClass}">` + fullWidthTableWrapper.innerHTML + `</div>`;
    });
}

/**********************************************/
/*  Add content load handler to process tables.
    */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processTables = (info) => {
    GWLog("GW.rewriteFunctions.processTables", "rewrite.js", 2);

    wrapTables(info);
    if (info.fullWidthPossible)
        wrapFullWidthTables(info);
}, { phase: "rewrite", condition: (info) => info.needsRewrite });

/***********/
/* FIGURES */
/***********/

/********************************/
/*  Inject wrappers into figures.
    */
function wrapFigures(loadEventInfo) {
    GWLog("wrapFigures", "rewrite.js", 1);

    loadEventInfo.document.querySelectorAll("figure").forEach(figure => {
        let media = figure.querySelector("img, audio, video");
        let caption = figure.querySelector("figcaption");

        if (!(media && caption))
            return;

        //  Create an inner wrapper for the figure contents.
        let innerWrapper = document.createElement("span");
        innerWrapper.classList.add("figure-inner-wrapper");
        figure.appendChild(innerWrapper);

        //  Wrap the caption in the wrapper span.
        let wrapper = document.createElement("span");
        wrapper.classList.add("caption-wrapper");
        wrapper.appendChild(caption);

        //  Get the media, or (if any) the image wrapper.
        let mediaBlock = media.closest(".image-wrapper") || media;

        //  Re-insert the (possibly wrapped) media and the wrapped caption into
        //  the figure.
        innerWrapper.appendChild(mediaBlock);
        innerWrapper.appendChild(wrapper);

        // Tag the figure with the image’s float class.
        if (media.classList.contains("float-left"))
            media.closest("figure").classList.add("float-left");
        if (media.classList.contains("float-right"))
            media.closest("figure").classList.add("float-right");
    });
}

/********************************************************************/
/*  Designate full-width figures as such (with a ‘full-width’ class).
    */
function markFullWidthFigures(loadEventInfo) {
    GWLog("markFullWidthFigures", "rewrite.js", 1);

    let fullWidthClass = "full-width";

    let allFullWidthMedia = loadEventInfo.document.querySelectorAll(`img.${fullWidthClass}, video.${fullWidthClass}`);
    allFullWidthMedia.forEach(fullWidthMedia => {
        fullWidthMedia.closest("figure").classList.toggle(fullWidthClass, true);
    });

    /*  Add ‘load’ listener for lazy-loaded media (as it might cause re-layout
        of e.g. sidenotes). Do this only after page loads, to avoid spurious
        re-layout at initial page load.
        */
    doWhenPageLoaded(() => {
        allFullWidthMedia.forEach(fullWidthMedia => {
            fullWidthMedia.addEventListener("load", (event) => {
                GW.notificationCenter.fireEvent("Rewrite.fullWidthMediaDidLoad");
            });
        });
    });
}

/***********************************************/
/*  Add content load handler to process figures.
    */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processFigures = (info) => {
    GWLog("GW.rewriteFunctions.processFigures", "rewrite.js", 2);

    wrapFigures(info);
    if (info.fullWidthPossible)
        markFullWidthFigures(info);
}, { phase: "rewrite", condition: (info) => info.needsRewrite });

/***************/
/* CODE BLOCKS */
/***************/

/***********************************************************/
/*  Wrap each pre.full-width in a div.full-width and a
    div.full-width-code-block-wrapper (for layout purposes).
    */
function wrapFullWidthPreBlocks(loadEventInfo) {
    GWLog("wrapFullWidthPreBlocks", "rewrite.js", 1);

    let fullWidthClass = "full-width";
    let fullWidthInnerWrapperClass = "full-width-code-block-wrapper";
    loadEventInfo.document.querySelectorAll(`pre.${fullWidthClass}`).forEach(fullWidthPre => {
        if (fullWidthPre.parentElement.tagName == "DIV" && fullWidthPre.parentElement.children.length == 1) {
            fullWidthPre.parentElement.classList.toggle(fullWidthClass, true);
            fullWidthPre.parentElement.innerHTML = `<div class="${fullWidthInnerWrapperClass}">` + fullWidthPre.parentElement.innerHTML + `</div>`;
        } else {
            fullWidthPre.parentElement.innerHTML =
                  `<div class="${fullWidthInnerWrapperClass}">`
                + `<div class="${fullWidthClass}">`
                + fullWidthPre.parentElement.innerHTML
                + `</div>`
                + `</div>`;
        }
    });
}

/***************************************************/
/*  Add content load handler to process code blocks.
    */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processCodeBlocks = (info) => {
    GWLog("GW.rewriteFunctions.processCodeBlocks", "rewrite.js", 2);

    if (info.fullWidthPossible)
        wrapFullWidthPreBlocks(info);
}, { phase: "rewrite", condition: (info) => info.needsRewrite });

/**************/
/* TYPOGRAPHY */
/**************/

/*****************************************/
/*  Returns the current selection as HTML.
    */
function getSelectionHTML() {
    let container = document.createElement("div");
    container.appendChild(window.getSelection().getRangeAt(0).cloneContents());
    return container.innerHTML;
}

/*********************/
/* FULL-WIDTH BLOCKS */
/*********************/

/*******************************************************************************/
/*  Expands all tables (& other blocks) whose wrapper block is marked with class
    ‘full-width’, and all figures marked with class ‘full-width’, to span the
    viewport (minus a specified margin on both sides).
    */
function createFullWidthBlockLayoutStyles() {
    GWLog("createFullWidthBlockLayoutStyles", "rewrite.js", 1);

    /*  Configuration and dynamic value storage.
        */
    GW.fullWidthBlockLayout = {
        sideMargin: 25,
        pageWidth: 0,
        leftAdjustment: 0
    };

    /*  Pre-query key elements, to save performance on resize.
        */
    let rootElement = document.querySelector("html");
    let markdownBody = document.querySelector("#markdownBody");

    /*  Inject styles block to hold dynamically updated layout variables.
        */
    document.querySelector("head").insertAdjacentHTML("beforeend", `<style id="full-width-block-layout-styles"></style>`);
    let fullWidthBlockLayoutStyles = document.querySelector("#full-width-block-layout-styles");

    /*  Function to update layout variables (called immediately and on resize).
        */
    let updateFullWidthBlockLayoutStyles = () => {
        GWLog("updateFullWidthBlockLayoutStyles", "rewrite.js", 2);

        GW.fullWidthBlockLayout.pageWidth = rootElement.offsetWidth;

        let markdownBodyRect = markdownBody.getBoundingClientRect();
        let markdownBodyRightMargin = GW.fullWidthBlockLayout.pageWidth - markdownBodyRect.right;
        GW.fullWidthBlockLayout.leftAdjustment = markdownBodyRect.left - markdownBodyRightMargin;

        fullWidthBlockLayoutStyles.innerHTML = `:root {
            --GW-full-width-block-layout-side-margin: ${GW.fullWidthBlockLayout.sideMargin}px;
            --GW-full-width-block-layout-page-width: ${GW.fullWidthBlockLayout.pageWidth}px;
            --GW-full-width-block-layout-left-adjustment: ${GW.fullWidthBlockLayout.leftAdjustment}px;
        }`;
    };
    updateFullWidthBlockLayoutStyles();

    //  Add listener to update layout variables on window resize.
    window.addEventListener("resize", updateFullWidthBlockLayoutStyles);
}
doWhenPageLoaded(createFullWidthBlockLayoutStyles);

/************************************/
/*  Set margins of full-width blocks.
    */
function setMarginsOnFullWidthBlocks(loadEventInfo) {
    GWLog("setMarginsOnFullWidthBlocks", "rewrite.js", 1);

    //  Get all full-width blocks in the given document.
    let allFullWidthBlocks = loadEventInfo.document.querySelectorAll("div.full-width, figure.full-width");

    let removeFullWidthBlockMargins = () => {
        allFullWidthBlocks.forEach(fullWidthBlock => {
            fullWidthBlock.style.marginLeft = "";
            fullWidthBlock.style.marginRight = "";
        });
    };

    if (!loadEventInfo.fullWidthPossible) {
        removeFullWidthBlockMargins();
        return;
    }

    //  Un-expand when mobile width, expand otherwise.
    doWhenMatchMedia(GW.mediaQueries.mobileWidth, "updateFullWidthBlockExpansionForCurrentWidthClass", () => {
        removeFullWidthBlockMargins();
    }, () => {
        allFullWidthBlocks.forEach(fullWidthBlock => {
            fullWidthBlock.style.marginLeft = `calc(
                                                    (-1 * (var(--GW-full-width-block-layout-left-adjustment) / 2.0))
                                                  + (var(--GW-full-width-block-layout-side-margin))
                                                  - ((var(--GW-full-width-block-layout-page-width) - 100%) / 2.0)
                                                )`;
            fullWidthBlock.style.marginRight = `calc(
                                                     (var(--GW-full-width-block-layout-left-adjustment) / 2.0)
                                                   + (var(--GW-full-width-block-layout-side-margin))
                                                   - ((var(--GW-full-width-block-layout-page-width) - 100%) / 2.0)
                                                )`;
        });
    });
}

/*********************************************************/
/*  Add content load handler to process full-width blocks.
    */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processFullWidthBlocks = (info) => {
    GWLog("GW.rewriteFunctions.processFullWidthBlocks", "rewrite.js", 2);

    setMarginsOnFullWidthBlocks(info);
}, { phase: ">rewrite" });

/***************************/
/* ANNOTATIONS (FRAGMENTS) */
/***************************/

/*******************************************************************************/
/*  Apply various typographic fixes (educate quotes, inject <wbr> elements after
    certain problematic characters, etc.).

    Requires typography.js to be loaded prior to this file.
    */
function rectifyTypographyInAnnotation(loadEventInfo) {
    GWLog("rectifyTypographyInAnnotation", "rewrite.js", 1);

    Typography.processElement(loadEventInfo.document,
          Typography.replacementTypes.QUOTES
        | Typography.replacementTypes.WORDBREAKS
        | Typography.replacementTypes.ELLIPSES
    );

    //  Educate quotes in image alt-text.
    loadEventInfo.document.querySelectorAll("img").forEach(image => {
        image.alt = Typography.processString(image.alt, Typography.replacementTypes.QUOTES);
    });
}

/*****************************************************************************/
/*  Sets, in CSS, the image dimensions that are specified in HTML.
    (This is to ensure no reflow when annotation popups are spawned.)
    */
function setImageDimensionsInAnnotation(loadEventInfo) {
    GWLog("setImageDimensionsInAnnotation", "rewrite.js", 1);

    loadEventInfo.document.querySelectorAll("figure img[width]").forEach(image => {
        image.style.width = image.getAttribute("width") + "px";
    });
}

/**************************************************************************/
/*  Add content load handler for processing a loaded annotation (fragment).
    */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processAnnotation = (info) => {
    GWLog("GW.rewriteFunctions.processAnnotation", "rewrite.js", 2);

    rectifyTypographyInAnnotation(info);
    setImageDimensionsInAnnotation(info);
}, { phase: "rewrite", condition: (info) => (info.isMainDocument == false && info.document.parentElement.id == "annotations-workspace") });

/*************/
/* FOOTNOTES */
/*************/

/************************************************************************/
/*  The footnotes section has no ID because Pandoc is weird. Give it one.
    */
function identifyFootnotesSection(loadEventInfo) {
    GWLog("identifyFootnotesSection", "rewrite.js", 1);

    let footnotesSection = loadEventInfo.document.querySelector("section.footnotes");
    if (footnotesSection)
        footnotesSection.id = "footnotes";
}

/******************************/
/*  Inject footnote self-links.
    */
function injectFootnoteSelfLinks(loadEventInfo) {
    GWLog("injectFootnoteSelfLinks", "rewrite.js", 1);

    let footnotesSection = loadEventInfo.document.querySelector("#footnotes");
    if (!footnotesSection)
        return;

    let footnotes = Array.from(footnotesSection.querySelector("#footnotes > ol").children);

    for (let i = 0; i < footnotes.length; i++)
        footnotes[i].insertAdjacentHTML("afterbegin", `<a href="#fn${(i + 1)}" title="Link to footnote ${(i + 1)}" class="footnote-self-link">&nbsp;</a>`);

    //  Highlight footnote on hover over self-link.
    document.querySelectorAll(".footnote-self-link").forEach(footnoteSelfLink => {
        footnoteSelfLink.addEventListener("mouseenter", (event) => {
            footnoteSelfLink.parentElement.classList.toggle("highlighted", true);
        });
        footnoteSelfLink.addEventListener("mouseleave", (event) => {
            footnoteSelfLink.parentElement.classList.toggle("highlighted", false);
        });
    });
}

/*****************************************************/
/*  Inject self-link for the footnotes section itself.
    */
function injectFootnoteSectionSelfLink(loadEventInfo) {
    GWLog("injectFootnoteSectionSelfLink", "rewrite.js", 1);

    let footnotesSection = loadEventInfo.document.querySelector("#footnotes");
    if (!footnotesSection)
        return;

    let footnotesSectionSelfLink = document.createElement("A");
    footnotesSectionSelfLink.href = "#footnotes";
    footnotesSectionSelfLink.title = "Link to section: § ‘Footnotes’";
    footnotesSectionSelfLink.classList.add("section-self-link");

    footnotesSection.insertBefore(footnotesSectionSelfLink, footnotesSection.firstElementChild.nextElementSibling);

    //  Highlight on hover.
    footnotesSectionSelfLink.addEventListener("mouseenter", (event) => {
        footnotesSectionSelfLink.previousElementSibling.classList.toggle("highlighted", true);
    });
    footnotesSectionSelfLink.addEventListener("mouseleave", (event) => {
        footnotesSectionSelfLink.previousElementSibling.classList.toggle("highlighted", false);
    });
}

/**************************************************************************/
/*  Return all {side|foot}note elements associated with the given citation.
    */
function allNotesForCitation(citation) {
    if (!citation.classList.contains("footnote-ref"))
        return null;

    let citationNumber = citation.id.substr(5);
    /*  We must check to ensure that the note in question is from the same
        page as the citation (to distinguish between main document and any
        full-page embeds that may be spawned).
        */
    return Array.from(document.querySelectorAll(`#fn${citationNumber}, #sn${citationNumber}`)).filter(note => note.querySelector(".footnote-back").pathname == citation.pathname);
}

/***************************************************************************/
/*  Bind mouse hover events to, when hovering over a citation, highlight all
    {side|foot}notes associated with that citation.
    */
function bindNoteHighlightEventsToCitations(loadEventInfo) {
    GWLog("bindNoteHighlightEventsToCitations", "rewrite.js", 1);

    loadEventInfo.document.querySelectorAll(".footnote-ref").forEach(citation => {
        //  Unbind existing events, if any.
        if (citation.citationMouseEnter) citation.removeEventListener("mouseenter", citation.citationMouseEnter);
        if (citation.citationMouseLeave) citation.removeEventListener("mouseleave", citation.citationMouseLeave);

        //  Bind events.
        let notesForCitation = allNotesForCitation(citation);
        citation.addEventListener("mouseenter", citation.citationMouseEnter = (event) => {
            notesForCitation.forEach(note => {
                note.classList.toggle("highlighted", true);
            });
        });
        citation.addEventListener("mouseleave", citation.citationMouseLeave = (event) => {
            notesForCitation.forEach(note => {
                note.classList.toggle("highlighted", false);
            });
        });
    });
}

/*******************************************/
/*  Add a TOC link to the footnotes section.
    */
function injectFootnotesTOCLink(loadEventInfo) {
    GWLog("injectFootnotesTOCLink", "rewrite.js", 1);

    let TOCList = loadEventInfo.document.querySelector("#TOC > ul");
    if (TOCList)
        TOCList.insertAdjacentHTML("beforeend", `<li><a href="#footnotes"><span>Footnotes</span></a></li>\n`);
}

/**************************************************************/
/*  Add content load handlers for processing footnotes section.
    */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processFootnotes = (info) => {
    GWLog("GW.rewriteFunctions.processFootnotes", "rewrite.js", 2);

    identifyFootnotesSection(info);
    injectFootnoteSectionSelfLink(info);
    injectFootnoteSelfLinks(info);
}, { phase: "rewrite", condition: (info) => (info.needsRewrite && info.isFullPage) });
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.injectFootnotesTOCLink = (info) => {
    GWLog("GW.rewriteFunctions.injectFootnotesTOCLink", "rewrite.js", 2);

    injectFootnotesTOCLink(info);
}, { phase: "rewrite", condition: (info) => info.isMainDocument });
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processCitations = (info) => {
    GWLog("GW.rewriteFunctions.processCitations", "rewrite.js", 2);

    bindNoteHighlightEventsToCitations(info);
}, { phase: "eventListeners" });

/*********/
/* LINKS */
/*********/

/********************************************************************/
/*  Designate self-links (a.k.a. anchorlinks) and local links (a.k.a.
    within-site links) as such.
    */
function addSpecialLinkClasses(loadEventInfo) {
    GWLog("addSpecialLinkClasses", "rewrite.js", 1);

    loadEventInfo.document.querySelectorAll(".markdownBody a[href]").forEach(link => {
        if (   link.hostname != location.hostname
            || link.closest("h1, h2, h3, h4, h5, h6")
            || link.closest(".section-self-link, .footnote-ref, .footnote-back, .footnote-self-link, .sidenote-self-link"))
            return;

        if (   loadEventInfo.location
            && link.pathname == loadEventInfo.location.pathname
            && (   loadEventInfo.document == Extracts.rootDocument
                || loadEventInfo.document.closest(".popframe").classList.contains("local-transclude"))) {
            link.swapClasses([ "link-self", "link-local" ], 0);
        } else if (link.pathname.substr(1).match(/[\.]/) == null) {
            link.swapClasses([ "link-self", "link-local" ], 1);
        }
    });
}

/*****************************************************************************/
/*  Directional navigation links on self-links: for each self-link like
    “see [later](#later-identifier)”, find the linked identifier, whether it’s
    before or after, and if it is before/previously, annotate the self-link
    with ‘↑’ and if after/later, ‘↓’. This helps the reader know if it’s a
    backwards link to a identifier already read, or an unread identifier.
    */
function directionalizeAnchorLinks(loadEventInfo) {
    GWLog("directionalizeAnchorLinks", "rewrite.js", 1);

    loadEventInfo.document.querySelectorAll("a.link-self").forEach(identifierLink => {
        if (!identifierLink.hash) return;

        target = loadEventInfo.document.querySelector(decodeURIComponent(identifierLink.hash));
        if (!target) return;

        identifierLink.classList.add(
            identifierLink.compareDocumentPosition(target) == Node.DOCUMENT_POSITION_FOLLOWING
            ? 'identifier-link-down'
            : 'identifier-link-up'
        );
    });
}

/************************************************/
/*  Add content load handler for link processing.
    */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processLinks = (info) => {
    GWLog("GW.rewriteFunctions.processLinks", "rewrite.js", 2);

    addSpecialLinkClasses(info);

    if (info.needsRewrite) {
        directionalizeAnchorLinks(info);
    }
}, { phase: "rewrite" });

/*****************/
/* PAGE METADATA */
/*****************/

/********************************************************************/
/*  Add ‘markdownBody’ class to #page-metadata, for styling purposes.
    */
function fixPageMetadataClass(loadEventInfo) {
    GWLog("fixPageMetadataClass", "rewrite.js", 1);

    let pageMetadataSection = document.querySelector("#page-metadata");
    if (pageMetadataSection)
        pageMetadataSection.classList.add("markdownBody");
}

/*****************************************************************/
/*  Add content load handler for processing page metadata section.
    */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processPageMetadata = (info) => {
    GWLog("GW.rewriteFunctions.processPageMetadata", "rewrite.js", 2);

    fixPageMetadataClass(info);
}, { phase: ">rewrite", condition: (info) => info.isMainDocument });

/*********/
/* MISC. */
/*********/

/***************************************************************************/
/*  Clean up image alt-text. (Shouldn’t matter, because all image URLs work,
    right? Yeah, right...)
    */
function cleanUpImageAltText(loadEventInfo) {
    GWLog("cleanUpImageAltText", "rewrite.js", 1);

    loadEventInfo.document.querySelectorAll("img[alt]").forEach(image => {
        image.alt = decodeURIComponent(image.alt.replace(/%(?![A-Fa-f0-9]{2})/g, "%25"));
    });
}

/**************************************************************/
/*  Add content load handler for doing miscellaneous rewriting.
    */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processMiscellaneousRewrites = (info) => {
    GWLog("GW.rewriteFunctions.processMiscellaneousRewrites", "rewrite.js", 2);

    cleanUpImageAltText(info);
}, { phase: "rewrite", condition: (info) => info.needsRewrite });

/*************/
/* DROP CAPS */
/*************/

/*******************************************************/
/*  Apply classes to blocks that should have a drop cap.
    */
function applyDropCapsClasses(loadEventInfo) {
    GWLog("applyDropCapsClasses", "rewrite.js", 1);

    //  Add ‘drop-cap-’ class to requisite blocks.
    let dropCapBlocksSelector = ".markdownBody > p:first-child, .markdownBody > .epigraph:first-child + p, .markdownBody .abstract + p";
    let dropCapClass = Array.from(loadEventInfo.document.querySelector("body").classList).find(cssClass => cssClass.startsWith("drop-caps-"));
    if (dropCapClass) {
        dropCapClass = dropCapClass.replace("-caps-", "-cap-");
        loadEventInfo.document.querySelectorAll(dropCapBlocksSelector).forEach(dropCapBlock => {
            /*  Only add page-global drop cap class to blocks that don’t
                already have a drop cap class of their own.
                */
            if (Array.from(dropCapBlock.classList).findIndex(cssClass => cssClass.startsWith("drop-cap-")) == -1)
                dropCapBlock.classList.add(dropCapClass);
        });
    }
}

/******************************************/
/*  Add content load handler for drop caps.
    */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.processDropCaps = (info) => {
    GWLog("GW.rewriteFunctions.processDropCaps", "rewrite.js", 2);

    applyDropCapsClasses(info);
}, { phase: "rewrite", condition: (info) => (info.needsRewrite && info.isMainDocument) });

/********************/
/* BACK TO TOP LINK */
/********************/

/***********************************************************************/
/*  Injects the “back to top” link. (Called only for the main document.)
    */
function injectBackToTopLink(loadEventInfo) {
    GWLog("injectBackToTopLink", "rewrite.js", 1);

    GW.backToTop = addUIElement(`<div id="back-to-top"><a href="#top" tabindex="-1" title="Back to top">` +
        `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="M6.1 422.3l209.4-209.4c4.7-4.7 12.3-4.7 17 0l209.4 209.4c4.7 4.7 4.7 12.3 0 17l-19.8 19.8c-4.7 4.7-12.3 4.7-17 0L224 278.4 42.9 459.1c-4.7 4.7-12.3 4.7-17 0L6.1 439.3c-4.7-4.7-4.7-12.3 0-17zm0-143l19.8 19.8c4.7 4.7 12.3 4.7 17 0L224 118.4l181.1 180.7c4.7 4.7 12.3 4.7 17 0l19.8-19.8c4.7-4.7 4.7-12.3 0-17L232.5 52.9c-4.7-4.7-12.3-4.7-17 0L6.1 262.3c-4.7 4.7-4.7 12.3 0 17z"/></svg>`
        + `</a></div>`);

    //  On mobile, show/hide the back-to-top link on scroll up/down.
    addScrollListener(updateBackToTopLinkVisibility, "updateBackToTopLinkVisibilityScrollListener");

    //  Update state immediately.
    updateBackToTopLinkVisibility();
}

/***********************************************************/
/*  Add content load handler to inject the back-to-top link.
    */
GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", GW.rewriteFunctions.injectBackToTopLink = (info) => {
    GWLog("GW.rewriteFunctions.injectBackToTopLink", "rewrite.js", 2);

    injectBackToTopLink(info);
}, { phase: "rewrite", condition: (info) => info.isMainDocument });

/*******************************************************************************/
/*  Show/hide the back-to-top link in response to scrolling.

    Called by the ‘updateBackToTopLinkVisibilityScrollListener’ scroll listener.
    */
function updateBackToTopLinkVisibility(event) {
    GWLog("updateBackToTopLinkVisibility", "rewrite.js", 3);

    /*  Show back-to-top link on ANY scroll up, or when scrolling a full page
        down from the top.
        */
    if (GW.scrollState.unbrokenUpScrollDistance > 0 || GW.scrollState.unbrokenDownScrollDistance > window.innerHeight)
        GW.backToTop.classList.toggle("hidden", false);

    //  Hide back-to-top link when scrolling to top.
    if (GW.scrollState.lastScrollTop <= 0)
        GW.backToTop.classList.toggle("hidden", true);
}

/*****************/
/* END OF LAYOUT */
/*****************/

doWhenPageLoaded(() => {
    GW.notificationCenter.fireEvent("Rewrite.pageLayoutWillComplete");
    requestAnimationFrame(() => {
        GW.pageLayoutComplete = true;
        GW.notificationCenter.fireEvent("Rewrite.pageLayoutDidComplete");
    });
}, { once: true });

/********************/
/* HASH REALIGNMENT */
/********************/

/*  This is necessary to defeat a bug where if the page is loaded with the URL
    hash targeting some element, the element does not match the :target CSS
    pseudo-class.
    */
function realignHash() {
    GWLog("realignHash", "rewrite.js", 1);

    //  Chrome’s fancy new “scroll to text fragment”. Deal with it in Firefox.
    if (GW.isFirefox()) {
        if (location.hash.startsWith("#:~:")) {
            GW.hashRealignValue = GW.hashRealignValue || "#";
        } else if (location.hash.includes(":~:")) {
            GW.hashRealignValue = GW.hashRealignValue || location.hash.replace(/:~:.*$/, "");
        }
    }

    let hash = GW.hashRealignValue || location.hash;
    if (hash > "") {
        //  Strip hash.
        history.replaceState(null, null, "#");

        //  Reset hash.
        location.hash = hash;

        //  Prevent redundant realignment.
        GW.hashRealignValue = null;
    }
}
doWhenDOMContentLoaded(realignHash);
doWhenPageLoaded(() => {
    requestAnimationFrame(realignHash);
});

/*! instant.page v5.1.0 - (C) 2019-2020 Alexandre Dieulot - https://instant.page/license */
/* Settings: 'prefetch' (loads HTML of target) after 800ms hover (desktop) or mouse-down-click (mobile); TODO: left in logging for testing during experiment */
let t,e;const n=new Set,o=document.createElement("link"),z=o.relList&&o.relList.supports&&o.relList.supports("prefetch")&&window.IntersectionObserver&&"isIntersecting"in IntersectionObserverEntry.prototype,s="instantAllowQueryString"in document.body.dataset,a=true,r="instantWhitelist"in document.body.dataset,c="instantMousedownShortcut"in document.body.dataset,d=1111;let l=800,u=!1,f=!1,m=!1;if("instantIntensity"in document.body.dataset){const t=document.body.dataset.instantIntensity;if("mousedown"==t.substr(0,"mousedown".length))u=!0,"mousedown-only"==t&&(f=!0);else if("viewport"==t.substr(0,"viewport".length))navigator.connection&&(navigator.connection.saveData||navigator.connection.effectiveType&&navigator.connection.effectiveType.includes("2g"))||("viewport"==t?document.documentElement.clientWidth*document.documentElement.clientHeight<45e4&&(m=!0):"viewport-all"==t&&(m=!0));else{const e=parseInt(t);isNaN(e)||(l=e)}}if(z){const n={capture:!0,passive:!0};if(f||document.addEventListener("touchstart",function(t){e=performance.now();const n=t.target.closest("a");if(!h(n))return;v(n.href)},n),u?c||document.addEventListener("mousedown",function(t){const e=t.target.closest("a");if(!h(e))return;v(e.href)},n):document.addEventListener("mouseover",function(n){if(performance.now()-e<d)return;const o=n.target.closest("a");if(!h(o))return;o.addEventListener("mouseout",p,{passive:!0}),t=setTimeout(()=>{v(o.href),t=void 0},l)},n),c&&document.addEventListener("mousedown",function(t){if(performance.now()-e<d)return;const n=t.target.closest("a");if(t.which>1||t.metaKey||t.ctrlKey)return;if(!n)return;n.addEventListener("click",function(t){1337!=t.detail&&t.preventDefault()},{capture:!0,passive:!1,once:!0});const o=new MouseEvent("click",{view:window,bubbles:!0,cancelable:!1,detail:1337});n.dispatchEvent(o)},n),m){let t;(t=window.requestIdleCallback?t=>{requestIdleCallback(t,{timeout:1500})}:t=>{t()})(()=>{const t=new IntersectionObserver(e=>{e.forEach(e=>{if(e.isIntersecting){const n=e.target;t.unobserve(n),v(n.href)}})});document.querySelectorAll("a").forEach(e=>{h(e)&&t.observe(e)})})}}function p(e){e.relatedTarget&&e.target.closest("a")==e.relatedTarget.closest("a")||t&&(clearTimeout(t),t=void 0)}function h(t){if(t&&t.href&&(!r||"instant"in t.dataset)&&(a||t.origin==location.origin||"instant"in t.dataset)&&["http:","https:"].includes(t.protocol)&&("http:"!=t.protocol||"https:"!=location.protocol)&&(s||!t.search||"instant"in t.dataset)&&!(t.hash&&t.pathname+t.search==location.pathname+location.search||"noInstant"in t.dataset))return!0}function v(t){if(n.has(t))return;const e=document.createElement("link");console.log("Prefetched: "+t);e.rel="prefetch",e.href=t,document.head.appendChild(e),n.add(t)};

/* Broken Anchor Checking */
/* If a reader loads a page and the anchor ID/hash does not exist inside the page, fire off a request to the 404 page, whose logs are reviewed manually, with the offending page+anchor ID, for correction (either fixing an outdated link somewhere on gwern.net, or adding a span/div manually to the page to make old inbound links go where they ought to). */
/* Such broken anchors can reflect out of date cross-page references, or reflect incoming URLs from elsewhere on the Internet which are broken/outdated. (Within-page anchor links are checked statically at compile-time, and those errors should never exist.) */
if (location.hash != "") {
 if (null == document.getElementById(location.hash.split('#')[1]))
  {  brokenHashLog = new URL("https://" + location.hostname + "/static/404.html" +
                                                        "-error-" +
                                                        fixedEncodeURIComponent(location.pathname) +
                                                        "--" +
                                                        fixedEncodeURIComponent(location.hash.split('#')[1]));
     doAjax({ location: brokenHashLog,  onSuccess: (e) => {}, onFailure: (e) => {} });
     console.log("Reporting broken hash-anchor: " + brokenHashLog);
     }
     }
/* Loop over internal page self-links and check that their targets exist; if they do not, report it via 404 like the broken anchors. */
selfLinks = document.querySelectorAll(".markdownBody a[href^='#']");
selfLinks.forEach(function(anchor) {
    anchorExists = document.getElementById(anchor.hash.split('#')[1]);
    if (null == anchorExists) {
     brokenHashLog = new URL("https://" + location.hostname + "/static/404.html" +
                                                        "-error-" +
                                                        fixedEncodeURIComponent(anchor.pathname) +
                                                        "--" +
                                                        fixedEncodeURIComponent(anchor.hash.split('#')[1]));
     doAjax({ location: brokenHashLog,  onSuccess: (e) => {}, onFailure: (e) => {} });
     console.log("Reporting broken hash-anchor: " + brokenHashLog);
    }
});
