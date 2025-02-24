/* Miscellaneous JS functions which run after the page loads to rewrite or adjust parts of the page. */
/* author: Said Achmiz */
/* license: MIT */

/*************/
/* CLIPBOARD */
/*************/

/*******************************************/
/*  Set up copy processors in main document.
 */
doWhenDOMContentLoaded(() => {
    registerCopyProcessorsForDocument(document);
});


/********************/
/* ID-BASED LOADING */
/********************/

/***************************************************************************/
/*	If the URL pathname is in /ref/, load content indicated by the id (i.e., 
	the rest of the path).
 */
addContentLoadHandler(GW.contentLoadHandlers.loadReferencedIdentifier = (eventInfo) => {
    GWLog("loadReferencedIdentifier", "rewrite.js", 1);

	let pageContentContainer = eventInfo.container.querySelector("#markdownBody") ?? eventInfo.container;

	/********************/
	/*	Helper functions.
	 */

	let urlForMappingFile = (basename) => {
		return URLFromString(  "/metadata/annotation/id/"
							 + basename 
							 + ".json?v="
							 + GW.refMappingFileVersion);
	};

	let updatePageTitleElements = (newTitleText) => {
		eventInfo.document.querySelectorAll("title, header h1").forEach(element => {
			element.innerHTML = newTitleText;
		});
	};

	let injectHelpfulErrorMessage = (errorMessageHTML) => {
		pageContentContainer.appendChild(elementFromHTML(`<div class="smallcaps-not"><p>${errorMessageHTML}</p></div>`));
	};

	let activateIncludeLinks = () => {
		GW.contentInjectHandlers.handleTranscludes({
			source: "loadReferencedIdentifier",
			container: pageContentContainer,
			document: eventInfo.document
		});
	};

	/*	The `message` argument may optionally be a 2-element array of strings 
		(the first element being the singular-case message, to be used if there 
		is only one result; the second being the plural-case message, to be 
		used if there are multiple results). Otherwise, it should be a string.
	 */
	let injectIdPrefixMatches = (message, mapping, ref) => {
		let idPrefixMatches = Object.entries(mapping).filter(entry => 
			   entry[0].startsWith(ref)
			&& entry[0] != ref
		);
		if (idPrefixMatches.length > 0) {
			if (typeof message == "object")
				message = idPrefixMatches.length == 1 ? message[0] : message[1];
			injectHelpfulErrorMessage(message);
			let includeLinkClass = idPrefixMatches.length == 1 
								   ? "include-annotation" 
								   : "include-annotation-partial";
			pageContentContainer.appendChild(elementFromHTML(
				  `<ul>`
				+ idPrefixMatches.map(entry => (
					  `<li><p>`
					+ `<a href="/ref/${entry[0]}">${entry[0]}</a>: `
					+ synthesizeIncludeLink(entry[1], {
						"class": "link-annotated ${includeLinkClass}",
						"data-include-selector-not": ".data-field.date, .aux-links-field-container"
					  }, {
					  	innerHTML: `<code>${entry[1]}</code>`
					  }).outerHTML
					+ `</p></li>`
				  )).join("")
				+ `</ul>`));
			activateIncludeLinks();
		}
	};

	let injectUrlPrefixMatches = (matches) => {
		injectHelpfulErrorMessage(`${matches.length} matches found:`);
		pageContentContainer.appendChild(elementFromHTML(
			  `<ul>`
			+ matches.map(entry => (
				  `<li><p>`
				+ synthesizeIncludeLink(entry[0], {
					"class": "link-annotated include-annotation"
				  }, {
					innerHTML: `<code>${entry[0]}</code>`
				  }).outerHTML
				+ `</p></li>`
			  )).join("")
			+ `</ul>`));
		activateIncludeLinks();
	};

	let injectHelpfulSuggestion = (url) => {
		pageContentContainer.appendChild(elementFromHTML("<hr>"));
		pageContentContainer.appendChild(elementFromHTML(
			  `<p>`
			+ `You can try browsing <a 
									 href="/doc/index" 
									 class="link-annotated link-page backlink-not icon-not" 
									 title="‘Essays’, Gwern 2009"
									 >documents by <strong>tag</strong></a>, `
			+ `or <a 
				   href="/index" 
				   class="link-annotated link-page backlink-not icon-not" 
				   title="'Essays', Gwern 2009"
				   >return to the <strong>main page</strong></a>, `
			+ `or search the site:`
			+ `</p>`));

		//	Synthesize and inject search page include-link.
		let searchPageIncludeLink = pageContentContainer.appendChild(synthesizeIncludeLink("/static/google-search.html", {
			"data-link-content-type": "local-document"
		}));

		//	Add inject handler.
		GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (contentDidInjectEventInfo) => {
			//	Function to set the proper mode (auto, light, dark) in the iframe.
			let updateSearchIframeMode = (iframe) => {
				iframe.contentDocument.querySelector("#search-styles-dark").media = DarkMode.mediaAttributeValues[DarkMode.currentMode()];
			};

			let iframe = contentDidInjectEventInfo.container.querySelector("iframe");
			iframe.classList.add("search");
			iframe.addEventListener("load", (event) => {
				let searchField = iframe.contentDocument.querySelector("input.search");

				//	Pre-fill the search field with the URL (if given).
				if (url)
					searchField.value = url;

				//	Set proper mode.
				updateSearchIframeMode(iframe);

				//	Add handler to update search iframe when switching modes.
				GW.notificationCenter.addHandlerForEvent("DarkMode.didSetMode", iframe.darkModeDidSetModeHandler = (info) => {
					updateSearchIframeMode(iframe)
				});

				//	Enable “search where” functionality.
				let searchWhereSelector = iframe.contentDocument.querySelector("#search-where-selector");
				searchWhereSelector.querySelectorAll("input").forEach(radioButton => {
					radioButton.addEventListener("change", (event) => {
						searchWhereSelector.querySelectorAll("input").forEach(otherRadioButton => {
							otherRadioButton.removeAttribute("checked");
						});
						radioButton.setAttribute("checked", "");
					});
				});

				//	Enable submit override (to make site search work).
				iframe.contentDocument.querySelector(".searchform").addEventListener("submit", (event) => {
					event.preventDefault();

					let form = event.target;
					form.querySelector("input.query").value = searchWhereSelector.querySelector("input[checked]").value
															+ " "
															+ form.querySelector("input.search").value;
					form.submit();
				});
			}, { once: true });
		}, {
			condition: (info) => (info.includeLink = searchPageIncludeLink),
			once: true
		});

		//	Trigger include-link.
		Transclude.triggerTransclude(searchPageIncludeLink, {
			source: "loadReferencedIdentifier",
			container: pageContentContainer,
			document: eventInfo.document
		});
	};

	/***************************/
	/*	Main /ref/ logic begins.
	 */

	let ref = decodeURIComponent(eventInfo.loadLocation.pathname.slice("/ref/".length));
	if (ref.length == 0) {
		injectHelpfulErrorMessage("No URL or ID specified.");
		injectHelpfulSuggestion();
	} else if (ref.startsWithAnyOf([ "http://", "https://", "/"])) {
		//	Strip origin from local URLs.
		let url = URLFromString(ref);
		if (url.hostname == location.hostname)
			ref = url.pathname + url.hash;

		//	Retrieve the big URL-to-id mapping file.
		doAjax({
			location: urlForMappingFile("all"),
			responseType: "json",
			onSuccess: (event) => {
				//	Get all prefix matches.
				let urlPrefixMatches = Object.entries(event.target.response).filter(entry => entry[0].startsWith(ref));
				if (urlPrefixMatches.length > 1) {
					/*	If multiple matches, list them all, transcluding 
						annotations where available (attempt in all cases, and
						those that fail will just become regular links).
					 */
					updatePageTitleElements("Unknown Reference");
					injectUrlPrefixMatches(urlPrefixMatches);
					injectHelpfulSuggestion(ref);
				} else if (urlPrefixMatches.length == 1) {
					//	If only one match, redirect to the matching /ref/ page.
					document.head.appendChild(elementFromHTML(`<link rel="canonical" href="${URLFromString(urlPrefixMatches.first[1]).href}">`));
					location = URLFromString("/ref/" + urlPrefixMatches.first[1]);				
				} else {
					//	If no matches at all...
					updatePageTitleElements("Invalid Query");
					injectHelpfulErrorMessage(`No annotation exists for URL <code>${ref}</code>.`);
					injectHelpfulSuggestion(ref);
				}
			}
		});
	} else {
		//	Normalize to lowercase, and update URL bar, if need be.
		let normalizedRef = ref.toLowerCase();
		if (normalizedRef != ref)
			relocate("/ref/" + normalizedRef);

		//	Retrieve id-to-URL mapping file (sliced by initial character).
		let mappingFileBasename = /^[a-zA-Z0-9_-]$/.test(normalizedRef.slice(0, 1)) 
								  ? normalizedRef.slice(0, 1) 
								  : "-";
		doAjax({
			location: urlForMappingFile(mappingFileBasename),
			responseType: "json",
			onSuccess: (event) => {
				let urlString = event.target.response[normalizedRef];
				if (urlString == null) {
					updatePageTitleElements("Invalid Query");
					injectHelpfulErrorMessage(`ID <code>${normalizedRef}</code> does not exist.`);
					injectIdPrefixMatches([ "Perhaps you want this:", "Perhaps you want one of these:" ], event.target.response, normalizedRef);
					injectHelpfulSuggestion(normalizedRef.replace(/-/g, " ").replace(" et al", "").split(" ").filter(x => /^([0-9]{1,3}|[0-9]{5,})$/.test(x) == false).join(" "));
				} else {
					//	Synthesize and inject include-link.
					let annotationIncludeLink = pageContentContainer.appendChild(synthesizeIncludeLink(event.target.response[normalizedRef], {
						class: "link-annotated"
					}));

					//	Add include-link load fail handler.
					GW.notificationCenter.addHandlerForEvent("Rewrite.contentDidChange", (contentDidChangeEventInfo) => {
						annotationIncludeLink.remove();
						updatePageTitleElements("Invalid Query");
						injectHelpfulErrorMessage(  `No annotation exists for ID <code>${normalizedRef}</code>`
												  + ` (<a href="${urlString}"><code>${URLFromString(urlString).href}</code></a>).`);
						injectIdPrefixMatches([ "Perhaps you want this instead:", "Perhaps you want one of these instead:" ], event.target.response, normalizedRef);
						injectHelpfulSuggestion(urlString);
					}, {
						condition: (info) => (   info.source == "transclude.loadingFailed"
											  && info.includeLink == annotationIncludeLink),
						once: true
					});

					//	Trigger include-link.
					Transclude.triggerTransclude(annotationIncludeLink, {
						container: pageContentContainer,
						document: eventInfo.document
					}, {
						doWhenDidLoad: (info) => {
							updatePageTitleElements(Annotations.referenceDataForLink(info.includeLink).popFrameTitle);
							document.head.appendChild(elementFromHTML(`<link rel="canonical" href="${URLFromString(urlString).href}">`));
						}
					});
				}
			}
		});
	}
}, "transclude", (info) => (info.loadLocation?.pathname.startsWith("/ref/") == true));


/*************/
/* AUX-LINKS */
/*************/

/***************************************************/
/*	Strip IDs from links in backlink context blocks.
 */
addContentInjectHandler(GW.contentInjectHandlers.anonymizeLinksInBacklinkContextBlocks = (eventInfo) => {
    GWLog("anonymizeLinksInBacklinkContextBlocks", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("a[id]").forEach(link => {
		link.id = "";
	});
}, "rewrite", (info => (   info.container.closest(".backlink-context") != null
						|| info.container.matches(".section-backlinks-include-wrapper"))));

/******************************************************************************/
/*	Returns the backlinks block for a section or a footnote (creating and 
	injecting the backlinks block if one does not already exist). (Note that,
	in the latter case, a GW.contentDidInject event will need to be fired for
	the backlinks block, once all modifications to it are complete; and its 
	wrapper, a div.section-backlinks-include-wrapper, will need to be unwrapped.)
 */
function getBacklinksBlockForSectionOrFootnote(targetBlock, containingDocument) {
	let backlinksBlock = targetBlock.querySelector(".section-backlinks");
	if (backlinksBlock == null) {
		//	Backlinks block.
		backlinksBlock = newElement("DIV", { "class": "section-backlinks", "id": `${targetBlock.id}-backlinks` });

		//	Label.
		let sectionLabelLinkTarget = baseLocationForDocument(containingDocument).pathname + "#" + targetBlock.id;
		let sectionLabelHTML = targetBlock.tagName == "SECTION"
							   ? `“${(targetBlock.firstElementChild.textContent)}”`
							   : `footnote <span class="footnote-number">${(Notes.noteNumber(targetBlock))}</span>`;
		backlinksBlock.append(elementFromHTML(  `<p class="aux-links-list-label backlinks-list-label">`
											  + `<strong>`
											  + `<a
											  	  href="/design#backlink"
											  	  class="icon-special link-annotated"
											  	  data-link-icon="arrows-pointing-inwards-to-dot"
											  	  data-link-icon-type="svg"
											  	  >Backlinks (<span class="backlink-count">0</span>)</a> for `
											  + `<a 
											  	  href="${sectionLabelLinkTarget}" 
											  	  class="link-page"
											  	  >${sectionLabelHTML}</a>:`
											  + `</strong></p>`));

		//	List.
		backlinksBlock.append(newElement("UL", { "class": "aux-links-list backlinks-list" }));

		//	Collapse wrapper.
		let backlinksBlockCollapseWrapper = newElement("DIV", { "class": "collapse aux-links-append section-backlinks-container" });
		backlinksBlockCollapseWrapper.append(backlinksBlock);

		//	Include wrapper.
		let backlinksBlockIncludeWrapper = newElement("DIV", { "class": "include-wrapper section-backlinks-include-wrapper" });
		backlinksBlockIncludeWrapper.append(backlinksBlockCollapseWrapper);

		//	Inject.
		let targetParentElement = targetBlock.classList.contains("collapse")
								  ? (targetBlock.querySelector(".collapse-content-wrapper") ?? targetBlock)
								  : targetBlock;
		let targetNextSiblingElement = null;
		if (targetBlock.tagName == "SECTION")
			targetNextSiblingElement = targetBlock.querySelector("section");
		targetParentElement.insertBefore(backlinksBlockIncludeWrapper, targetNextSiblingElement);
	}

	return backlinksBlock;
}

/**************************************************************************/
/*	Update the parenthesized count of backlink entries, display in the list 
	label graf of a backlinks block.
 */
function updateBacklinksCountDisplay(backlinksBlock) {
	let countDisplay = backlinksBlock.querySelector(".backlink-count");
	if (countDisplay == null)
		return;

	countDisplay.innerHTML = backlinksBlock.querySelectorAll(".backlinks-list > li").length;
}

/*************************************/
/*	Add within-page section backlinks.
 */
addContentInjectHandler(GW.contentInjectHandlers.addWithinPageBacklinksToSectionBacklinksBlocks = (eventInfo) => {
    GWLog("addWithinPageBacklinksToSectionBacklinksBlocks", "rewrite.js", 1);

	let excludedContainersSelector = [
		"#hidden-sidenote-storage",
		".sidenote-column",
		".aux-links-list"
	].join(", ");
	if (eventInfo.container.closest(excludedContainersSelector) != null)
		return;

	let excludedLinkContainersSelector = [
		"#page-metadata",
		".aux-links-append"
	].join(", ");
	let excludedTargetContainersSelector = [
		"#backlinks-section",
		"#similars-section",
		"#link-bibliography-section"
	].join(", ");

	let backlinksBySectionId = { };
	let mainContentContainer = eventInfo.document.querySelector("#markdownBody") ?? eventInfo.document.querySelector(".markdownBody");
	mainContentContainer.querySelectorAll("a.link-self").forEach(link => {
		if (link.closest(excludedLinkContainersSelector) != null)
			return;

		let targetBlock = mainContentContainer.querySelector(selectorFromHash(link.hash))?.closest("section, li.footnote");
		if (   targetBlock != null
			&& targetBlock.matches(excludedTargetContainersSelector) == false) {
			if (backlinksBySectionId[targetBlock.id] == null)
				backlinksBySectionId[targetBlock.id] = [ targetBlock, [ ] ];

			backlinksBySectionId[targetBlock.id][1].push(link);
		}
	});

	let pageTitle = Content.referenceDataForLink(eventInfo.loadLocation).pageTitle;
	for (let [ targetBlock, linksToTargetBlock ] of Object.values(backlinksBySectionId)) {
		let sectionBacklinksBlock = getBacklinksBlockForSectionOrFootnote(targetBlock, eventInfo.document);
		let sectionBacklinksBlockIncludeWrapper = sectionBacklinksBlock.closest(".section-backlinks-include-wrapper");

		//	Inject the backlink entries...
		for (let link of linksToTargetBlock) {
			let backlinkEntry = elementFromHTML(  `<li><p class="backlink-source">`
												+ `<a 
													href="${link.pathname}" 
													class="backlink-not link-self link-annotated"
													>${pageTitle}</a> (`
												+ `<a 
													href="#${link.id}"
													class="backlink-not link-self extract-not"
													>context</a>`
												+ `):</p>`
												+ `<blockquote class="backlink-context"><p>`
												+ `<a
													href="${link.pathname}"
													class="backlink-not include-block-context-expanded collapsible"
													data-target-id="${link.id}"
													>[backlink context]</a>`
												+ `</p></blockquote>`
												+ `</li>`);

			/*	If we are injecting into an existing section backlinks block, 
				then a separate inject event must be fired for the created 
				backlink.
			 */
			if (sectionBacklinksBlockIncludeWrapper == null) {
				let backlinkEntryIncludeWrapper = newElement("DIV", { "class": "include-wrapper" });
				backlinkEntryIncludeWrapper.append(backlinkEntry);
				sectionBacklinksBlock.querySelector(".backlinks-list").append(backlinkEntryIncludeWrapper);

				//	Clear loading state of all include-links.
				Transclude.allIncludeLinksInContainer(backlinkEntryIncludeWrapper).forEach(Transclude.clearLinkState);

				//	Fire inject event.
				let flags = GW.contentDidInjectEventFlags.clickable;
				if (eventInfo.document == document)
					flags |= GW.contentDidInjectEventFlags.fullWidthPossible;
				GW.notificationCenter.fireEvent("GW.contentDidInject", {
					source: "transclude.section-backlinks",
					contentType: "backlinks",
					container: backlinkEntryIncludeWrapper,
					document: eventInfo.document,
					loadLocation: eventInfo.loadLocation,
					flags: flags
				});

				unwrap(backlinkEntryIncludeWrapper);
			} else {
				sectionBacklinksBlock.querySelector(".backlinks-list").append(backlinkEntry);
			}
		}

		//	Update displayed count.
		updateBacklinksCountDisplay(sectionBacklinksBlock);

		if (sectionBacklinksBlockIncludeWrapper != null) {
			//	Fire load event.
			GW.notificationCenter.fireEvent("GW.contentDidLoad", {
				source: "transclude.section-backlinks",
				contentType: "backlinks",
				container: sectionBacklinksBlockIncludeWrapper,
				document: eventInfo.document,
				loadLocation: eventInfo.loadLocation
			});

			//	Fire inject event.
			let flags = GW.contentDidInjectEventFlags.clickable;
			if (eventInfo.document == document)
				flags |= GW.contentDidInjectEventFlags.fullWidthPossible;
			GW.notificationCenter.fireEvent("GW.contentDidInject", {
				source: "transclude.section-backlinks",
				contentType: "backlinks",
				container: sectionBacklinksBlockIncludeWrapper,
				document: eventInfo.document,
				loadLocation: eventInfo.loadLocation,
				flags: flags
			});

			unwrap(sectionBacklinksBlockIncludeWrapper);
		}
	}

	if (eventInfo.document == document)
		Content.invalidateCachedContent(eventInfo.loadLocation);
}, "rewrite", (info) => (   info.document == document
						 && info.contentType == "localPage"
						 && location.pathname.endsWithAnyOf([ "/index", "/" ]) == false));

/****************************************************************************/
/*	When an annotation is transcluded into a page, and some of the backlinks 
	for the annotated page are from the page into which the annotation is
	transcluded, the “full context” links become pointless, and should become
	just “context” (as in synthesized within-page backlinks), and likewise 
	should not spawn pop-frames.
 */
addContentInjectHandler(GW.contentInjectHandlers.rectifyLocalizedBacklinkContextLinks = (eventInfo) => {
    GWLog("rectifyLocalizedBacklinkContextLinks", "rewrite.js", 1);

	eventInfo.container.querySelectorAll(".backlink-source .link-self:not(.link-annotated)").forEach(backlinkContextLink => {
		backlinkContextLink.innerHTML = "context";
		backlinkContextLink.classList.add("extract-not");
	});
}, "rewrite", (info => (   info.document == document
						&& info.contentType == "backlinks"
						&& info.source != "transclude.section-backlinks")));

/*************************************************************************/
/*  Add “backlinks” link to start of section popups, when that section has
    a backlinks block.
 */
addContentInjectHandler(GW.contentInjectHandlers.injectBacklinksLinkIntoLocalSectionPopFrame = (eventInfo) => {
    GWLog("injectBacklinksLinkIntoLocalSectionPopFrame", "rewrite.js", 1);

    let containingPopFrame = Extracts.popFrameProvider.containingPopFrame(eventInfo.container);
    if (   containingPopFrame.classList.contains("local-page") == true
        && containingPopFrame.classList.contains("full-page") == false) {
        let section = eventInfo.container.querySelector("section");
        if (section == null)
            return;

        let backlinksBlock = eventInfo.container.querySelector(`#${(CSS.escape(section.id))}-backlinks`);
        if (backlinksBlock == null)
            return;

        //  Construct link and enclosing block.
        let backlinksLink = newElement("A", {
            "class": "aux-links backlinks",
            "href": "#" + backlinksBlock.id,
            "data-link-icon": "arrows-pointing-inwards-to-dot",
            "data-link-icon-type": "svg"
        }, {
            "innerHTML": "backlinks"
        });
        let sectionMetadataBlock = newElement("P", {
            "class": "section-metadata"
        });
        sectionMetadataBlock.append(backlinksLink);
        section.insertBefore(sectionMetadataBlock, section.children[1]);

        //  Make a click on the link uncollapse the backlinks block.
        backlinksLink.addActivateEvent((event) => {
            if (isWithinCollapsedBlock(backlinksBlock)) {
                GW.notificationCenter.addHandlerForEvent("Collapse.collapseStateDidChange", (info) => {
                    revealElement(backlinksBlock);
                }, {
                    once: true,
                    condition: (isWithinCollapsedBlock(backlinksBlock) == false)
                });
            } else {
                requestAnimationFrame(() => {
                    revealElement(backlinksBlock);
                });
            }
        });
    }
}, "rewrite", (info) => (info.context == "popFrame"));

/**************************************************************************/
/*  Remove aux-links list labels when transcluding aux-links lists into the
	aux-links sections of a page (Backlinks, Similars, Bibliography).
 */
addContentInjectHandler(GW.contentInjectHandlers.removeAuxLinksListLabelsInAuxLinksSections = (eventInfo) => {
    GWLog("removeAuxLinksListLabelsInAuxLinksSections", "rewrite.js", 1);

	let auxLinksTypes = [
		"backlinks",
		"similars",
		"link-bibliography"
	];
	let auxLinksListLabelSelector = auxLinksTypes.map(auxLinksType =>
		`#${auxLinksType} > .aux-links-list-label, #${auxLinksType} > .columns > .aux-links-list-label`
	).join(", ");

	let auxLinksListLabel = eventInfo.container.querySelector(auxLinksListLabelSelector);
	if (auxLinksListLabel)
		auxLinksListLabel.remove();
}, "rewrite", (info) => (info.source == "transclude"));


/*********/
/* LISTS */
/*********/

GW.layout.orderedListTypes = [
    "decimal",
    "lower-alpha",
    "upper-alpha",
    "lower-roman",
    "upper-roman",
    "lower-greek"
];

/*****************************************************************************/
/*  Returns the type (CSS `list-item` counter value type) of an <ol> element.
 */
function orderedListType(list) {
    if (list?.tagName != "OL")
        return null;

    for (let type of GW.layout.orderedListTypes)
        if (list.classList.contains(`list-type-${type}`))
            return type;

    return null;
}

/************************************************************************/
/*  Sets the type (CSS `list-item` counter value type) of an <ol> element.
 */
function setOrderedListType(list, type) {
    if (list?.tagName != "OL")
        return;

    for (let type of GW.layout.orderedListTypes)
        list.classList.remove(`list-type-${type}`);

    list.classList.add(`list-type-${type}`);
}

/*******************************************************************/
/*  Returns the nesting level (an integer in [1,listCyclePeriod]) of
    a <ul> element.
 */
function unorderedListLevel(list) {
    if (list?.tagName != "UL")
        return 0;

    let prefix = "list-level-";

    return (parseInt(Array.from(list.classList).find(c => c.startsWith(prefix))?.slice(prefix.length)) || 1);
}

/***********************************************************/
/*  Sets CSS class matching nesting level of a <ul> element.
 */
function setUnorderedListLevel(list, level) {
    if (list?.tagName != "UL")
        return;

    let prefix = "list-level-";

    list.swapClasses([ Array.from(list.classList).find(c => c.startsWith(prefix)), `${prefix}${level}` ], 1);
}

/***********************************/
/*  Designate list type via a class.
 */
addContentInjectHandler(GW.contentInjectHandlers.designateListTypes = (eventInfo) => {
    GWLog("designateListTypes", "rewrite.js", 1);

    //  Workaround for case-insensitivity of CSS selectors.
    eventInfo.container.querySelectorAll("ol[type]").forEach(list => {
        switch (list.type) {
        case '1':
            setOrderedListType(list, "decimal");
            break;
        case 'a':
            setOrderedListType(list, "lower-alpha");
            break;
        case 'A':
            setOrderedListType(list, "upper-alpha");
            break;
        case 'i':
            setOrderedListType(list, "lower-roman");
            break;
        case 'I':
            setOrderedListType(list, "upper-roman");
            break;
        case 'α':
            setOrderedListType(list, "lower-greek");
            break;
        default:
            break;
        }
    });

    //  If not explicitly specified, cycle between these three list types.
    eventInfo.container.querySelectorAll("ol:not([type])").forEach(list => {
        let enclosingList = list.parentElement?.closest("ol");
        let enclosingListType = enclosingList?.parentElement?.matches("section#footnotes")
                                ? null
                                : orderedListType(enclosingList);

        switch (enclosingListType) {
        case "decimal":
            setOrderedListType(list, "upper-roman");
            break;
        case "upper-roman":
            setOrderedListType(list, "lower-alpha");
            break;
        case "lower-alpha":
        default:
            setOrderedListType(list, "decimal");
            break;
        }
    });

    //  Set list levels.
    let listCyclePeriod = 3;
    eventInfo.container.querySelectorAll("ul").forEach(list => {
        setUnorderedListLevel(list, (unorderedListLevel(list.parentElement?.closest("ul")) % listCyclePeriod) + 1);
    });
}, ">rewrite");

/*****************************************************************/
/*  Wrap text nodes and inline elements in list items in <p> tags.
 */
addContentLoadHandler(GW.contentLoadHandlers.paragraphizeListTextNodes = (eventInfo) => {
    GWLog("paragraphizeListTextNodes", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("li").forEach(listItem => {
        if (listItem.closest(".TOC"))
            return;

        paragraphizeTextNodesOfElementRetainingMetadata(listItem);
    });
}, "rewrite");

/**********************************************/
/*  Rectify styling/structure of list headings.
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyListHeadings = (eventInfo) => {
    GWLog("rectifyListHeadings", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("p > strong:only-child").forEach(boldElement => {
        if (   boldElement.parentElement.childNodes.length == 2
            && boldElement.parentElement.firstChild == boldElement
            && boldElement.parentElement.lastChild.nodeType == Node.TEXT_NODE
            && boldElement.parentElement.lastChild.nodeValue == ":") {
            boldElement.parentElement.lastChild.remove();
            boldElement.lastTextNode.nodeValue += ":";
        }
    });
}, "rewrite");


/***************/
/* BLOCKQUOTES */
/***************/

/*************************************************************************/
/*  Returns the nesting level (an integer in [1,blockquoteCyclePeriod]) of
    a <blockquote> element.
 */
function blockquoteLevel(blockquote) {
    if (blockquote?.tagName != "BLOCKQUOTE")
        return 0;

    let prefix = "blockquote-level-";

    return (parseInt(Array.from(blockquote.classList).find(c => c.startsWith(prefix))?.slice(prefix.length)) || 1);
}

/*******************************************************************/
/*  Sets CSS class matching nesting level of a <blockquote> element.
 */
function setBlockquoteLevel(blockquote, level) {
    if (blockquote?.tagName != "BLOCKQUOTE")
        return;

    let prefix = "blockquote-level-";

    blockquote.swapClasses([ Array.from(blockquote.classList).find(c => c.startsWith(prefix)), `${prefix}${level}` ], 1);
}

/******************************************/
/*  Designate blockquote level via a class.
 */
addContentInjectHandler(GW.contentInjectHandlers.designateBlockquoteLevels = (eventInfo) => {
    GWLog("designateBlockquoteLevels", "rewrite.js", 1);

    let blockquoteCyclePeriod = 6;
    eventInfo.container.querySelectorAll("blockquote").forEach(blockquote => {
        setBlockquoteLevel(blockquote, (blockquoteLevel(blockquote.parentElement?.closest("blockquote")) % blockquoteCyclePeriod) + 1);
    });
}, ">rewrite");


/**********/
/* TABLES */
/**********/

/**********************************************/
/*  Remove Pandoc-inserted <colgroup> elements.
 */
addContentLoadHandler(GW.contentLoadHandlers.deleteColgroups = (eventInfo) => {
    GWLog("deleteColgroups", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("colgroup").forEach(colgroup => {
        colgroup.remove();
    });
}, "rewrite");

/**************************************************************************/
/*  If there are tables, import tablesorter.js (if need be) and make tables
    sortable.
 */
addContentInjectHandler(GW.contentInjectHandlers.makeTablesSortable = (eventInfo) => {
    GWLog("makeTablesSortable", "rewrite.js", 1);

    if (eventInfo.container.querySelector("table") == null)
        return;

    //  Import tablesorter.js, if need be.
    let scriptTag = document.querySelector("script[src*='/static/js/tablesorter.js']");
    if (scriptTag == null) {
        scriptTag = newElement("SCRIPT", {
            "type": "text/javascript",
            "src": "/static/js/tablesorter.js"
        });
        document.body.appendChild(scriptTag);
    }

    let sortTables = (eventInfo) => {
        jQuery(".table:not(.table-sort-not) table", eventInfo.document).tablesorter();
    };

    if (window["jQuery"]) {
        sortTables(eventInfo);
    } else {
        GW.notificationCenter.addHandlerForEvent("Tablesorter.didLoad", (info) => {
            sortTables(eventInfo);
        }, { once: true });
    }
}, ">rewrite");

/************************************************************************/
/*  Wrap each table in a div.table-wrapper and a div.table-scroll-wrapper
    (for layout purposes).
 */
addContentLoadHandler(GW.contentLoadHandlers.wrapTables = (eventInfo) => {
    GWLog("wrapTables", "rewrite.js", 1);

    wrapAll("table", ".table-wrapper", {
        useExistingWrapper: true,
        root: eventInfo.container
    });
    wrapAll("table", ".table-scroll-wrapper", {
        useExistingWrapper: false,
        root: eventInfo.container
    });

    /*  Move .width-full class from the outer .table-wrapper down to the inner
        .table-scroll-wrapper. (This is done so that the `wrapFullWidthTables`
        content inject handler may work properly.)
     */
    eventInfo.container.querySelectorAll(".table-scroll-wrapper").forEach(tableScrollWrapper => {
        let tableWrapper = tableScrollWrapper.closest(".table-wrapper");
        transferClasses(tableWrapper, tableScrollWrapper, [ "width-full" ]);
    });
}, "rewrite");

/****************************************************/
/*  Rectify full-width table wrapper class structure:

    div.table-wrapper.table.width-full
        div.table-scroll-wrapper
            table

    or

    div.table-wrapper.collapse
        div.collapse-content-wrapper.table.width-full
            div.table-scroll-wrapper
                table
 */
addContentInjectHandler(GW.contentInjectHandlers.rectifyFullWidthTableWrapperStructure = (eventInfo) => {
    GWLog("rectifyFullWidthTableWrapperStructure", "rewrite.js", 1);

    wrapAll(".table-scroll-wrapper.width-full", ".table", {
        useExistingWrapper: true,
        moveClasses: [ "width-full" ],
        root: eventInfo.container
    });
}, "rewrite", (info) => info.fullWidthPossible);


/***********/
/* FIGURES */
/***********/

/******************************************************************************/
/*  Add observers to transform thumbnails into full-sized images if page layout
    demands it.
 */
addContentInjectHandler(GW.contentInjectHandlers.addSwapOutThumbnailEvents = (eventInfo) => {
    GWLog("addSwapOutThumbnailEvents", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("img[data-src-size-full]").forEach(image => {
        let thumbnailSize = Images.thumbnailSizeFromURL(image.src);

        lazyLoadObserver(() => {
            resizeObserver(() => {
                if (thumbnailSize < image.clientWidth * window.devicePixelRatio) {
                    Images.unthumbnailifyImage(image);
                    return false;
                } else if (Images.isThumbnail(image) == false) {
                    return false;
                }
            }, image);
        }, image, {
            root: scrollContainerOf(image),
            rootMargin: "100%"
        });
    });
}, "eventListeners");

/******************************************************************************/
/*  Request image inversion and outlining judgments for images in the loaded 
	content. (We omit from this load handler those GW.contentDidLoad events 
	which are fired when we construct templated content from already extracted 
	reference data, as by then it is already too late; there is no time to send 
	an invertOrNot / outlineOrNot API request and receive a response, before 
	the image must be displayed. Instead, requesting inversion and outlining 
	judgments for images in templated content is handled by the data source 
	object for that content (either Content, in content.js, or Annotations, in 
	annotations.js).)
 */
addContentLoadHandler(GW.contentLoadHandlers.requestImageInversionJudgments = (eventInfo) => {
    GWLog("requestImageInversionJudgments", "rewrite.js", 1);

    //  Request image inversion judgments from invertOrNot.
    requestImageInversionJudgmentsForImagesInContainer(eventInfo.container);

    //  Request image outlining judgments from outlineOrNot.
    requestImageOutliningJudgmentsForImagesInContainer(eventInfo.container);
}, ">rewrite", (info) => (info.source != "transclude"));

/*****************************************************************************/
/*	Apply image inversion judgment, if one is available, to the given image;
	otherwise, add a handler to apply a judgment that becomes available later.
 */
function applyImageInversionJudgmentNowOrLater(image) {
	if (   applyImageInversionJudgment(image) == false
		&& image.inversionJudgmentAvailabilityHandler == null) {
		/*	If no inversion judgment has been applied, there may yet be hope
			for this image; add another listener to wait for additional 
			image inversion judgments to become available in the future.
		 */
		GW.notificationCenter.addHandlerForEvent("GW.imageInversionJudgmentsAvailable", image.inversionJudgmentAvailabilityHandler = (info) => {
			if (applyImageInversionJudgment(image)) {
				GW.notificationCenter.removeHandlerForEvent("GW.imageInversionJudgmentsAvailable", image.inversionJudgmentAvailabilityHandler);
				image.inversionJudgmentAvailabilityHandler = null;
			}
		});
	}
}

/*****************************************************************************/
/*	Apply image outlining judgment, if one is available, to the given image;
	otherwise, add a handler to apply a judgment that becomes available later.
 */
function applyImageOutliningJudgmentNowOrLater(image) {
	let propagateClassesToFigure = (image) => {
		image.closest("figure").swapClasses([ "outline-not", "outline" ], outliningJudgmentForImage(image) ? 1 : 0);
	};

	if (applyImageOutliningJudgment(image)) {
		propagateClassesToFigure(image);
	} else if (   outliningJudgmentHasBeenAppliedToImage(image) == false
			   && image.outliningJudgmentAvailabilityHandler == null) {
		/*	If no outlining judgment has been applied, there may yet be hope
			for this image; add another listener to wait for additional 
			image outlining judgments to become available in the future.
		 */
		GW.notificationCenter.addHandlerForEvent("GW.imageOutliningJudgmentsAvailable", image.outliningJudgmentAvailabilityHandler = (info) => {
			if (applyImageOutliningJudgment(image)) {
				propagateClassesToFigure(image);
				GW.notificationCenter.removeHandlerForEvent("GW.imageOutliningJudgmentsAvailable", image.outliningJudgmentAvailabilityHandler);
				image.outliningJudgmentAvailabilityHandler = null;
			}
		});
	}
}

/***************************************************************************/
/*  Apply image inversion judgments (received from the invertOrNot API) and
	image outlining judgments (received from the outlineOrNot API) to images 
	in the loaded content, if available.
 */
addContentInjectHandler(GW.contentInjectHandlers.applyImageInversionAndOutliningJudgments = (eventInfo) => {
    GWLog("applyImageInversionAndOutliningJudgments", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("figure img").forEach(applyImageInversionJudgmentNowOrLater);
    eventInfo.container.querySelectorAll("figure img").forEach(applyImageOutliningJudgmentNowOrLater);
}, "rewrite");

/******************************************************************/
/*  Wrap text nodes and inline elements in figcaptions in <p> tags.
 */
addContentLoadHandler(GW.contentLoadHandlers.paragraphizeFigcaptionTextNodes = (eventInfo) => {
    GWLog("paragraphizeFigcaptionTextNodes", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("figcaption").forEach(paragraphizeTextNodesOfElementRetainingMetadata);
}, "rewrite");

/***************************************************************************/
/*  Make sure that the figcaption, alt-text, and title are, collectively, as
    useful as possible (i.e., ensure that neither the alt-text nor the title
    duplicate the contents of the figcaption).
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyImageAuxText = (eventInfo) => {
    GWLog("rectifyImageAuxText", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("figure img").forEach(image => {
        let figcaption = image.closest("figure").querySelector("figcaption");
        if (figcaption == null)
            return;

        let [ captionText, titleText, altText ] = [
            figcaption.cloneNode(true),
            newElement("SPAN", null, { "innerHTML": image.getAttribute("title") }),
            newElement("SPAN", null, { "innerHTML": image.getAttribute("alt") }),
        ].map(element => {
            if (element)
                Typography.processElement(element, Typography.replacementTypes.CLEAN|Typography.replacementTypes.QUOTES);

            return element.textContent.trim();
        });

        /*  If the ‘title’ attribute merely duplicates the caption, but the
            ‘alt’ attribute has something different (and nonempty), then copy
            the ‘alt’ to the ‘title’.
         */
        if (   titleText == captionText
            && altText != captionText
            && altText > "")
            image.title = altText;

        /*  As above, but vice-versa (copy ‘title’ to ‘alt’, if appropriate).
         */
        if (   altText == captionText
            && titleText != captionText
            && titleText > "")
            image.alt = titleText;
    });
}, "rewrite");

/*******************************/
/*  Wrap bare images in figures.
 */
addContentLoadHandler(GW.contentLoadHandlers.wrapImages = (eventInfo) => {
    GWLog("wrapImages", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("p > img:only-child").forEach(image => {
        unwrap(image.parentElement);
    });

    let exclusionSelector = [
        "td",
        "th",
        ".footnote-back"
    ].join(", ");
    wrapAll("img", (image) => {
        if (   image.classList.contains("figure-not")
            || image.closest(exclusionSelector) != null
            || image.closest("figure") != null)
            return;

        wrapElement(image, "figure");
    }, {
        root: eventInfo.container
    });
}, "rewrite");

/*****************************************************************************/
/*	Inject the page thumbnail image into the page abstract (or the abstract of
	a full-page pop-frame.
 */
addContentInjectHandler(GW.contentInjectHandlers.injectThumbnailIntoPageAbstract = (eventInfo) => {
    GWLog("injectThumbnailIntoPageAbstract", "rewrite.js", 1);

	let pageAbstract = eventInfo.container.querySelector(".abstract blockquote");
	if (   pageAbstract == null
		|| previousBlockOf(pageAbstract) != null)
		return;

	//	Designate page abstract.
	pageAbstract.classList.add("page-abstract");

	if (pageAbstract.querySelector(".page-thumbnail-figure") != null)
		return;

	//	Insert page thumbnail into page abstract.
	let referenceData = Content.referenceDataForLink(eventInfo.loadLocation);
	if (referenceData.pageThumbnailHTML != null) {
		let pageThumbnailFigure = pageAbstract.insertBefore(newElement("FIGURE", {
			class: "page-thumbnail-figure " + (eventInfo.context == "popFrame" ? "float-right" : "float-not")
		}, {
			innerHTML: referenceData.pageThumbnailHTML
		}), (eventInfo.context == "popFrame"
			 ? pageAbstract.firstElementChild
			 : null));
		let pageThumbnail = pageThumbnailFigure.querySelector("img");
		wrapElement(pageThumbnail, "span.image-wrapper.img");
		if (eventInfo.context == "popFrame")
			Images.thumbnailifyImage(pageThumbnail);

		//	Invert, or not.
		applyImageInversionJudgmentNowOrLater(pageThumbnail);
	}

	if (eventInfo.container == document.main)
		Content.invalidateCachedContent(eventInfo.loadLocation);
}, "rewrite", (info) => (   info.container == document.main
						 || (   info.context == "popFrame"
						 	 && Extracts.popFrameProvider == Popups
						 	 && Extracts.popFrameProvider.containingPopFrame(info.container).classList.contains("full-page"))));

/******************************************************************************/
/*  Set, in CSS, the media (image/video) dimensions that are specified in HTML.
 */
function setMediaElementDimensions(mediaElement, fixWidth = false, fixHeight = false) {
    let width = mediaElement.getAttribute("width");
    let height = mediaElement.getAttribute("height");

    mediaElement.style.aspectRatio = mediaElement.dataset.aspectRatio ?? `${width} / ${height}`;

    if (mediaElement.maxHeight == null) {
        //  This should match `1rem`.
        let baseFontSize = GW.isMobile() ? "18" : "20";

        /*  This should match the `max-height` property value for all images in
            figures (the `figure img` selector; see initial.css).
         */
        mediaElement.maxHeight = window.innerHeight - (8 * baseFontSize);
    }

    if (mediaElement.maxHeight)
        width = Math.round(Math.min(width, mediaElement.maxHeight * (width/height)));

    if (fixWidth) {
        mediaElement.style.width = `${width}px`;
    }
    if (fixHeight) {
        //  Nothing, for now.
    }
}

GW.dimensionSpecifiedMediaElementSelector = [
    "img[width][height]:not([src$='.svg'])",
    "video[width][height]"
].map(x => `figure ${x}`).join(", ");

/**************************************************************/
/*  Prevent reflow for floats, reduce reflow for other figures.
 */
addContentLoadHandler(GW.contentLoadHandlers.setMediaElementDimensions = (eventInfo) => {
    GWLog("setMediaElementDimensions", "rewrite.js", 1);

    //  Set specified dimensions in CSS.
    eventInfo.container.querySelectorAll(GW.dimensionSpecifiedMediaElementSelector).forEach(mediaElement => {
        let fixWidth = (   mediaElement.classList.containsAnyOf([ "float-left", "float-right" ])
                        || mediaElement.closest("figure")?.classList.containsAnyOf([ "float-left", "float-right" ]));
        setMediaElementDimensions(mediaElement, fixWidth);
    });

    //  Also ensure that SVGs get rendered as big as possible.
    eventInfo.container.querySelectorAll("figure img[src$='.svg']").forEach(svg => {
        svg.style.width = "100vw";
        svg.style.aspectRatio = svg.dataset.aspectRatio;
    });
}, "rewrite");

/************************************************************/
/*  Prevent reflow due to lazy-loaded media (images, videos).
 */
addContentInjectHandler(GW.contentInjectHandlers.updateMediaElementDimensions = (eventInfo) => {
    GWLog("updateMediaElementDimensions", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(GW.dimensionSpecifiedMediaElementSelector).forEach(mediaElement => {
        setMediaElementDimensions(mediaElement, true);
    });
}, "rewrite", (info) => (   info.context == "popFrame"
						 && Extracts.popFrameProvider.containingPopFrame(info.container).classList.contains("object")) == false);

/************************************************************************/
/*  Set image dimensions from inline-specified image data (e.g., base64).
 */
addContentInjectHandler(GW.contentInjectHandlers.setImageDimensionsFromImageData = (eventInfo) => {
    GWLog("setImageDimensionsFromImageData", "rewrite.js", 1);

    /*  If an image doesn’t have dimensions set, but image data is already
        available (because the source is a data: URI), we can determine
        dimensions once the image “loads” (i.e., ‘load’ event fires, when
        browser parses the data: attribute).
     */
    eventInfo.container.querySelectorAll("figure img:not([width])").forEach(image => {
        if (image.loadHandler)
            return;

        image.addEventListener("load", image.loadHandler = (event) => {
            image.setAttribute("width", image.naturalWidth);
            image.setAttribute("height", image.naturalHeight);
            image.setAttribute("data-aspect-ratio", `${image.naturalWidth} / ${image.naturalHeight}`);

            setMediaElementDimensions(image);

            //  Ensure proper interaction with image-focus.
            if (image.classList.contains("focusable"))
                ImageFocus.designateSmallImageIfNeeded(image);
        }, { once: true });
    });
}, "eventListeners");

/************************************************************************/
/*  Ensure media (image, video) dimensions update when device is rotated.
 */
addContentInjectHandler(GW.contentInjectHandlers.addOrientationChangeMediaElementDimensionUpdateEvents = (eventInfo) => {
    GWLog("addOrientationChangeMediaElementDimensionUpdateEvents", "rewrite.js", 1);

    let mediaElements = eventInfo.container.querySelectorAll(GW.dimensionSpecifiedMediaElementSelector);

    doWhenMatchMedia(GW.mediaQueries.portraitOrientation, "Rewrite.updateMediaElementDimensionsWhenOrientationChanges", (mediaQuery) => {
        mediaElements.forEach(mediaElement => {
            mediaElement.maxHeight = null;
        });
        requestAnimationFrame(() => {
            mediaElements.forEach(mediaElement => {
                mediaElement.style.width = "";
                setMediaElementDimensions(mediaElement, true);
            });
        });
    });
}, "eventListeners", (info) => (   info.context == "popFrame"
								&& Extracts.popFrameProvider.containingPopFrame(info.container).classList.contains("object")) == false);

/********************************/
/*  Inject wrappers into figures.
 */
addContentLoadHandler(GW.contentLoadHandlers.wrapFigures = (eventInfo) => {
    GWLog("wrapFigures", "rewrite.js", 1);

    let mediaSelector = "img, audio, video";

    eventInfo.container.querySelectorAll("figure").forEach(figure => {
        let media = figure.querySelector(mediaSelector);
        if (media == null)
            return;

        //  Create a wrapper for the figure contents (media plus caption).
        let outerWrapper = figure.appendChild(newElement("SPAN", { "class": "figure-outer-wrapper" }));

        //  Re-insert the (possibly wrapped) media into the figure.
        figure.querySelectorAll(mediaSelector).forEach(mediaElement => {
            let mediaBlock = (   mediaElement.closest(".image-row-wrapper")
                              ?? mediaElement.closest(".image-wrapper")
                              ?? mediaElement);
            outerWrapper.appendChild(mediaBlock);

			//	Ensure proper wrapping.
            if (   mediaBlock == mediaElement
            	|| (   mediaBlock.matches(".image-wrapper") == false
            		&& mediaElement.closest(".image-wrapper") == null))
            	mediaBlock = wrapElement(mediaElement, "span.image-wrapper." + mediaElement.tagName.toLowerCase());
        });

        //  Wrap the caption (if any) in a caption wrapper.
        let caption = figure.querySelector("figcaption");
        if (caption)
	        outerWrapper.appendChild(newElement("SPAN", { "class": "caption-wrapper" })).appendChild(caption);
    });
}, "rewrite");

/***************************************************************************/
/*	Designate whether the media element backdrop should be inverted (back to
	a light color) in dark mode.
 */
addContentInjectHandler(GW.contentInjectHandlers.designateImageBackdropInversionStatus = (eventInfo) => {
    GWLog("designateImageBackdropInversionStatus", "rewrite.js", 1);

    let mediaSelector = _π("figure", " ", [ "img", "audio", "video" ]).join(", ");

	eventInfo.container.querySelectorAll(mediaSelector).forEach(mediaElement => {
		if (mediaElement.matches("audio")) {
			mediaElement.classList.add("dark-mode-invert");		
		} else {
			let wrapper = mediaElement.closest(".image-wrapper");
			if (mediaElement.classList.containsAnyOf([ "invert", "invert-auto" ]) == false)
				wrapper.classList.add("dark-mode-invert");
		}
	});
}, ">rewrite");

/******************************************************************************/
/*  Figure captions might be empty if they are generated by including the
    annotation abstract of an annotated media include link, but the abstract is
    actually empty (because it’s a partial annotation).
 */
addContentLoadHandler(GW.contentLoadHandlers.removeEmptyFigureCaptions = (eventInfo) => {
    GWLog("removeEmptyFigureCaptions", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("figcaption").forEach(figcaption => {
        if (isNodeEmpty(figcaption, { alsoExcludeSelector: "a" }))
            figcaption.remove();
    });
}, "rewrite");

/*****************************************************************************/
/*  Allow for specifying figure classes by setting classes on a media element.
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyFigureClasses = (eventInfo) => {
    GWLog("rectifyFigureClasses", "rewrite.js", 1);

    let mediaSelector = "img, audio, video";

    eventInfo.container.querySelectorAll("figure").forEach(figure => {
        let media = figure.querySelector(mediaSelector);
        if (media == null)
            return;

        //  Tag the figure with the first (or only) media element’s classes.
        [ "float-left", "float-right", "outline-not", "image-focus-not" ].forEach(imgClass => {
            if (media.classList.contains(imgClass)) {
                figure.classList.add(imgClass);
                media.classList.remove(imgClass);
            }
        });

        media.classList.remove("float");
    });
}, "rewrite");

/********************************/
/*  Don’t float solitary figures.
 */
addContentInjectHandler(GW.contentInjectHandlers.deFloatSolitaryFigures = (eventInfo) => {
    GWLog("deFloatSolitaryFigures", "rewrite.js", 1);

    let floatClasses = [ "float-left", "float-right" ];
    eventInfo.container.querySelectorAll(floatClasses.map(x => `figure.${x}:only-child`).join(", ")).forEach(figure => {
        if (isOnlyChild(figure))
            figure.classList.remove(...floatClasses);
    });
}, "rewrite");

/***********************************************************************/
/*  Prepare full-width (class `width-full`) figures; add listeners, etc.
 */
addContentInjectHandler(GW.contentInjectHandlers.prepareFullWidthFigures = (eventInfo) => {
    GWLog("prepareFullWidthFigures", "rewrite.js", 1);

    let fullWidthClass = "width-full";

    let allFullWidthMedia = eventInfo.container.querySelectorAll(`figure img.${fullWidthClass}, figure video.${fullWidthClass}`);
    allFullWidthMedia.forEach(fullWidthMedia => {
        fullWidthMedia.closest("figure").classList.toggle(fullWidthClass, true);
    });

    //  Constrain caption width to width of media element.
    let constrainCaptionWidth = (fullWidthMedia) => {
        let caption = fullWidthMedia.closest("figure").querySelector(".caption-wrapper");
        if (caption)
            caption.style.maxWidth = fullWidthMedia.offsetWidth > 0
                                     ? fullWidthMedia.offsetWidth + "px"
                                     : fullWidthMedia.closest(".markdownBody").offsetWidth + "px";
    };

    //  Add ‘load’ listener for lazy-loaded media.
    allFullWidthMedia.forEach(fullWidthMedia => {
        fullWidthMedia.addEventListener("load", fullWidthMedia.loadListener = (event) => {
            constrainCaptionWidth(fullWidthMedia);
            fullWidthMedia.loadListener = null;
        }, { once: true });
    });

    doWhenPageLayoutComplete(() => {
        /*  Update ‘load’ listener for any lazy-loaded media which has not
            already loaded (as it might cause re-layout of e.g. sidenotes). Do
            this only after page layout is complete, to avoid spurious re-layout
            at initial page load.
         */
        allFullWidthMedia.forEach(fullWidthMedia => {
            constrainCaptionWidth(fullWidthMedia);
            if (fullWidthMedia.loadListener) {
                fullWidthMedia.removeEventListener("load", fullWidthMedia.loadListener);
                fullWidthMedia.addEventListener("load", (event) => {
                    constrainCaptionWidth(fullWidthMedia);
                    GW.notificationCenter.fireEvent("Rewrite.fullWidthMediaDidLoad", {
                        mediaElement: fullWidthMedia
                    });
                }, { once: true });
            }
        });

        //  Add listener to update caption max-width when window resizes.
        addWindowResizeListener(event => {
            allFullWidthMedia.forEach(constrainCaptionWidth);
        }, {
            name: "constrainFullWidthMediaCaptionWidthOnWindowResizeListener"
        });
    });
}, "rewrite", (info) => info.fullWidthPossible);

/******************************************************************************/
/*  There is no browser native lazy loading for <video> tag `poster` attribute,
    so we implement it ourselves.
 */
addContentInjectHandler(GW.contentInjectHandlers.lazyLoadVideoPosters = (eventInfo) => {
    GWLog("lazyLoadVideoPosters", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("video:not([poster])").forEach(video => {
    	let videoURL = URLFromString(video.querySelector("source").src);
    	if (videoURL.hostname == location.hostname)
    		video.dataset.videoPoster = videoURL.pathname + "-poster.jpg";
    	if (video.dataset.videoPoster > "") {
			lazyLoadObserver(() => {
				video.poster = video.dataset.videoPoster;
			}, video, {
				root: scrollContainerOf(video),
				rootMargin: "100%"
			});
		}
    });
}, "eventListeners");

/******************************************************************************/
/*  Enable clicking anywhere on a video (that has not yet loaded and started to
    play) to load it and start playing it. (Otherwise, only clicking the ‘play’
    button causes the video to load and play.)
 */
addContentInjectHandler(GW.contentInjectHandlers.enableVideoClickToPlay = (eventInfo) => {
    GWLog("enableVideoClickToPlay", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("video").forEach(video => {
        video.addEventListener("click", video.clickToPlayEvent = (event) => {
            video.play();
            video.removeEventListener("click", video.clickToPlayEvent);
            video.clickToPlayEvent = null;
        });
    });
}, "eventListeners");

/****************************************************************/
/*  Account for interaction between image-focus.js and popups.js.
 */
if (Extracts.popFrameProvider == Popups) {
    GW.notificationCenter.addHandlerForEvent("ImageFocus.imageOverlayDidAppear", (info) => {
        Popups.hidePopupContainer();
    });
    GW.notificationCenter.addHandlerForEvent("ImageFocus.imageOverlayDidDisappear", (info) => {
        Popups.unhidePopupContainer();
    });
    GW.notificationCenter.addHandlerForEvent("ImageFocus.imageDidFocus", (info) => {
        /*  Pin a popup when clicking to image-focus an image within it
            (unless it’s a popup that contains *only* the image, and nothing
             else - no metadata, no other content, nothing - in which case,
             pinning is unnecessary).
         */
        let popup = Popups.containingPopFrame(info.image);
        if (   popup
            && (   popup.classList.contains("object")
                && Annotations.isAnnotatedLink(popup.spawningTarget) == false) == false)
            Popups.pinPopup(popup);
    });
}


/***************/
/* CODE BLOCKS */
/***************/

/*************************************************************/
/*  Wrap each <pre> in a div.sourceCode (for layout purposes).
 */
addContentLoadHandler(GW.contentLoadHandlers.wrapPreBlocks = (eventInfo) => {
    GWLog("wrapPreBlocks", "rewrite.js", 1);

    wrapAll("pre", ".sourceCode", {
        useExistingWrapper: true,
        root: eventInfo.container
    });
}, "rewrite");

/********************************************************/
/*  EXPERIMENTAL: Highlight-on-hover for all code blocks.
 */
addContentLoadHandler(GW.contentLoadHandlers.addCodeBlockLineClasses = (eventInfo) => {
    GWLog("addCodeBlockLineClasses", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("code.sourceCode > span:not(.line)").forEach(lineSpan => {
        lineSpan.classList.add("line");
        if (lineSpan.innerHTML.length == 0)
            lineSpan.innerHTML = "&nbsp;";
    });

    eventInfo.container.querySelectorAll("pre code:not(.sourceCode)").forEach(codeBlock => {
        codeBlock.innerHTML = codeBlock.innerHTML.split("\n").map(
            line => `<span class="line">${(line || "&nbsp;")}</span>`
        ).join("\n");
    });
}, "rewrite");

/*****************************************************************************/
/*  Allow for specifying code block classes by setting classes on the <pre>.
    (Workaround for a Pandoc peculiarity where classes set on a code block
     are applied to the <pre> element and not on the div.sourceCode wrapper.)
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyCodeBlockClasses = (eventInfo) => {
    GWLog("rectifyCodeBlockClasses", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("pre").forEach(preBlock => {
        let wrapper = preBlock.closest("div.sourceCode");

        //  Tag the wrapper with the <pre>’s classes.
        [ "float-left", "float-right" ].forEach(preClass => {
            if (preBlock.classList.contains(preClass)) {
                wrapper.classList.add(preClass);
                preBlock.classList.remove(preClass);
            }
        });

        preBlock.classList.remove("float");
    });
}, "rewrite");

/**********************************************************************/
/*  Wrap each pre.width-full in a div.width-full (for layout purposes).
 */
addContentInjectHandler(GW.contentInjectHandlers.wrapFullWidthPreBlocks = (eventInfo) => {
    GWLog("wrapFullWidthPreBlocks", "rewrite.js", 1);

    wrapAll("pre.width-full", ".width-full", {
        useExistingWrapper: true,
        root: eventInfo.container
    });
}, "rewrite", (info) => info.fullWidthPossible);


/**********/
/* EMBEDS */
/**********/

/******************************************************************************/
/*  There’s no way to tell whether an <iframe> has loaded, except to listen for
    the `load` event. So, we implement our own checkable load flag, with a
    class.
 */
addContentInjectHandler(GW.contentInjectHandlers.markLoadedEmbeds = (eventInfo) => {
    GWLog("markLoadedEmbeds", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("iframe.loaded-not").forEach(embed => {
        embed.addEventListener("load", (event) => {
            embed.classList.remove("loaded-not");
        }, { once: true });
    });
}, "eventListeners");

/**************************************************************************/
/*  Workaround for a Chrome bug that scrolls the parent page when an iframe
    popup has a `src` attribute with a hash and that hash points to an
    old-style anchor (`<a name="foo">`).
 */
addContentInjectHandler(GW.contentInjectHandlers.applyIframeScrollFix = (eventInfo) => {
    GWLog("applyIframeScrollFix", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("iframe.loaded-not").forEach(iframe => {
        let srcURL = URLFromString(iframe.src);
        if (   srcURL.pathname.endsWith(".html")
            && srcURL.hash > "") {
            srcURL.savedHash = srcURL.hash;
            srcURL.hash = "";
            iframe.src = srcURL.href;
        }

        iframe.addEventListener("load", (event) => {
            if (srcURL.savedHash) {
                let selector = selectorFromHash(srcURL.savedHash);
                let element = iframe.contentDocument.querySelector(`${selector}, [name='${(selector.slice(1))}']`);
                if (element)
                    iframe.contentWindow.scrollTo(0, element.getBoundingClientRect().y);
            }
        }, { once: true });
    });
}, "eventListeners");


/************/
/* HEADINGS */
/************/

/**********************************************************************/
/*	On main page, inject into section headings buttons that copy to the 
	clipboard the link to that section.
 */
addContentInjectHandler(GW.contentInjectHandlers.injectCopySectionLinkButtons = (eventInfo) => {
    GWLog("injectCopySectionLinkButtons", "rewrite.js", 1);

	let sectionHeadingSelector = _π("section", " > ", [ "h1", "h2", "h3", "h4", "h5", "h6" ], ":first-child").join(", ");

	eventInfo.container.querySelectorAll(sectionHeadingSelector).forEach(heading => {
		if (heading.querySelector(".copy-section-link-button") != null)
			return;

		let button = heading.appendChild(newElement("BUTTON", {
			type: "button",
			class: "copy-section-link-button",
			title: "Copy section link to clipboard",
			tabindex: "-1"
		}, {
			innerHTML: GW.svg("link-simple-solid")	
		}));

		button.addEventListener("mouseup", (event) => {
			button.classList.add("clicked");
		});
		button.addActivateEvent((event) => {
			copyTextToClipboard(heading.querySelector("a").href);

			if (button.clickTimer)
				clearTimeout(button.clickTimer);

			button.clickTimer = setTimeout(() => {
				button.classList.remove("clicked");
			}, 150);
		});
	});
}, ">rewrite", (info) => (info.container == document.main));


/***********/
/* COLUMNS */
/***********/

/*****************************************/
/*  Disable columns if only one list item.
 */
addContentLoadHandler(GW.contentLoadHandlers.disableSingleItemColumnBlocks = (eventInfo) => {
    GWLog("disableSingleItemColumnBlocks", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".columns > ul").forEach(columnList => {
        if (columnList.children.length == 1) {
            columnList.parentElement.classList.remove("columns");

            if (columnList.parentElement.className == "")
                unwrap(columnList.parentElement);
        }
    });
}, "rewrite");


/**************/
/* INTERVIEWS */
/**************/

/****************************************/
/*  Rectify HTML structure of interviews.
 */
addContentLoadHandler(GW.contentLoadHandlers.rewriteInterviews = (eventInfo) => {
    GWLog("rewriteInterviews", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".interview, .interview > .collapse").forEach(interviewWrapper => {
        if (interviewWrapper.firstElementChild.tagName != "UL")
            return;

        let interview = newElement("UL", { class: `list ${interviewWrapper.className}` });

        for (let child of Array.from(interviewWrapper.children)) {
            if (child.tagName != "UL")
                continue;

            let exchange = interview.appendChild(newElement("LI", { class: "exchange" }));
            exchange.append(child.cloneNode(true));

            for (let utterance of exchange.firstElementChild.children) {
                utterance.classList.add("utterance");

                let speaker = utterance.querySelector("strong");

                //  If the speaker is wrapped, find the outermost wrapper.
                while (   speaker.parentElement
                       && speaker.parentElement.tagName != "P"
                       && speaker.nextSibling?.textContent.startsWith(":") != true)
                    speaker = speaker.parentElement;
                speaker.classList.add("speaker");

                //  Move colon.
                (speaker.querySelector("strong") ?? speaker).innerHTML += ": ";
                speaker.nextSibling.textContent = speaker.nextSibling.textContent.slice(1).trimStart();
            }
        }

        interviewWrapper.replaceWith(interview);
    });
}, "rewrite");


/****************/
/* MARGIN NOTES */
/****************/

/*************************************************************/
/*  Wrap the contents of all margin notes in an inner wrapper.
 */
addContentLoadHandler(GW.contentLoadHandlers.wrapMarginNotes = (eventInfo) => {
    GWLog("wrapMarginNotes", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".marginnote").forEach(marginnote => {
        let innerWrapper = newElement("SPAN", { "class": "marginnote-inner-wrapper" });
        innerWrapper.append(...marginnote.childNodes);
        marginnote.append(innerWrapper);

		/*	Designate those margin notes which consist of just an icon (e.g. 
			manicule).
		 */
		if (innerWrapper.textContent.trim().length <= 1)
			marginnote.classList.add("only-icon");
    });
}, "rewrite");

/**************************/
/*  Aggregate margin notes.
 */
addContentLoadHandler(GW.contentLoadHandlers.aggregateMarginNotes = (eventInfo) => {
    GWLog("aggregateMarginNotes", "rewrite.js", 1);

    aggregateMarginNotesInDocument(eventInfo.document);
}, "rewrite");


/**************/
/* TYPOGRAPHY */
/**************/

/*******************************************************************************/
/*  Apply various typographic fixes (educate quotes, inject <wbr> elements after
    certain problematic characters, etc.) in content transforms.

    Requires typography.js to be loaded prior to this file.
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyTypographyInContentTransforms = (eventInfo) => {
    GWLog("rectifyTypographyInContentTransforms", "rewrite.js", 1);

    Typography.processElement(eventInfo.container,
        (  Typography.replacementTypes.QUOTES
         | Typography.replacementTypes.WORDBREAKS
         | Typography.replacementTypes.ELLIPSES));

    //  Educate quotes in image alt-text.
    eventInfo.container.querySelectorAll("img").forEach(image => {
        image.alt = Typography.processString(image.alt, Typography.replacementTypes.QUOTES);
    });
}, "rewrite", (info) => (   info.contentType == "wikipediaEntry"
                         || info.contentType == "tweet"));

/***********************************/
/*  Rectify typography in body text.

    NOTE: This should be temporary. Word breaks after slashes should be added
    in body text on the back end, at content build time. But that is currently
    not working, hence this temporary client-side solution.
    —SA 2023-09-13
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyTypographyInBodyText = (eventInfo) => {
    GWLog("rectifyTypographyInBodyText", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("p").forEach(graf => {
        Typography.processElement(graf, Typography.replacementTypes.WORDBREAKS);
    });
}, "rewrite");

/******************************************************************************/
/*  Remove extraneous whitespace-only text nodes from between the element parts
    of a .cite (citation element).
 */
addContentLoadHandler(GW.contentLoadHandlers.removeExtraneousWhitespaceFromCitations = (eventInfo) => {
    GWLog("removeExtraneousWhitespaceFromCitations", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".cite").forEach(citation => {
        Array.from(citation.children).forEach(citationPart => {
            if (   citationPart.nextSibling
                && citationPart.nextSibling.nodeType == Node.TEXT_NODE
                && isNodeEmpty(citationPart.nextSibling))
                citationPart.nextSibling.remove();
        });
    });
}, "rewrite");

/**********************************************************/
/*	Convert Unicode “icon” glyphs into proper inline icons.
 */
addContentLoadHandler(GW.contentLoadHandlers.iconifyUnicodeIconGlyphs = (eventInfo) => {
    GWLog("iconifyUnicodeIconGlyphs", "rewrite.js", 1);

	let glyphIconMapping = {
		"☞": "icon-manicule-right"  // U+261E WHITE RIGHT POINTING INDEX
	};

	let processElement = (element) => {
		let replacements = [ ];
		let replacedGlyphs = [ ];

		for (let node of element.childNodes) {
			if (node.nodeType === Node.ELEMENT_NODE) {
				let replacedGlyphsInNode = processElement(node);

				if (   replacedGlyphsInNode.length > 0
					&& node.classList.containsAnyOf(replacedGlyphsInNode.map(g => glyphIconMapping[g])))
					replacements.push([ node, node.childNodes ]);

				replacedGlyphs.push(...replacedGlyphsInNode);
			} else if (node.nodeType === Node.TEXT_NODE) {
				let glyphRegExp = new RegExp(Object.keys(glyphIconMapping).join("|"), "g");
				let parts = [ ];
				let start = 0;
				let match = null;
				while (match = glyphRegExp.exec(node.textContent)) {
					replacedGlyphs.push(match[0]);
					parts.push([ match[0], start, match.index ]);
					start = match.index + match[0].length;
				}
				if (parts.length > 0) {
					let replacementNodes = [ ];
					parts.forEach(part => {
						if (part[1] > part[0])
							replacementNodes.push(document.createTextNode(node.textContent.slice(...(part.slice(1,2)))));
						replacementNodes.push(newElement("SPAN", { "class": glyphIconMapping[part[0]] }));
					});
					if (node.textContent.length > start)
						replacementNodes.push(document.createTextNode(node.textContent.slice(start)));
					replacements.push([ node, replacementNodes ]);
				}
			}
		}

		if (replacements.length > 0) {
			//	Replace.
			replacements.forEach(replacement => {
				let [ replacedNode, replacementNodes ] = replacement;
				replacedNode.parentNode.replaceChild(newDocument(replacementNodes), replacedNode);
			});
		}

		return replacedGlyphs;
	}

    eventInfo.container.querySelectorAll("p").forEach(graf => {
    	processElement(graf);
    });
}, "rewrite");

/******************************************************/
/*	Inject full fraction markup into .fraction <span>s.
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyFractionMarkup = (eventInfo) => {
    GWLog("rectifyFractionMarkup", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("span.fraction").forEach(fraction => {
		fraction.innerHTML = fraction.innerHTML.replace(/^(.+?)\u2044(.+?)$/, (match, num, denom) => {
			return `<span class="num">${num}</span><span class="frasl">&#x2044;</span><span class="denom">${denom}</span>`;
		});
	});
}, "rewrite");

/******************************************************************/
/*  Configure Hyphenopoly.

    Requires Hyphenopoly_Loader.js to be loaded prior to this file.
 */
Hyphenopoly.config({
    require: {
        "en-us": "FORCEHYPHENOPOLY"
    },
    setup: {
        hide: "none",
        keepAlive: true,
        safeCopy: false
    }
});

/**********************************************/
/*  Hyphenate with Hyphenopoly.

    Requires Hyphenopoly_Loader.js to be loaded prior to this file.
 */
addContentInjectHandler(GW.contentInjectHandlers.hyphenate = (eventInfo) => {
    GWLog("hyphenate", "rewrite.js", 1);

    if (Hyphenopoly.hyphenators == null)
        return;

    if (GW.isX11())
        return;

    let selector = (GW.isMobile()
                    ? ".markdownBody p"
                    : (eventInfo.document == document
                       ? ".sidenote p, .abstract blockquote p"
                       : "p"));
    let blocks = eventInfo.container.querySelectorAll(selector);
    Hyphenopoly.hyphenators.HTML.then((hyphenate) => {
        blocks.forEach(block => {
            hyphenate(block);
            Typography.processElement(block, Typography.replacementTypes.NONE, true);
        });
    });
}, "rewrite");

/************************************************************************/
/*  Remove soft hyphens and other extraneous characters from copied text.
 */
addCopyProcessor((event, selection) => {
    Typography.processElement(selection, Typography.replacementTypes.CLEAN);

    return true;
});

/*****************************************************************************/
/*  Makes it so that copying an author-date citation (e.g. `Foo et al 2001`)
    interact properly with copy-paste when rendered with pseudo-element ellipses
    (`Foo...2001`).
 */
addCopyProcessor((event, selection) => {
    /*  Set `display` of all `span.cite-joiner` to `initial` (overriding the
        default of `none`) so that their contents are included in the
        content properties of the selection); inject surrounding spaces.
     */
    selection.querySelectorAll(".cite-joiner").forEach(citeJoiner => {
        citeJoiner.style.display = "initial";
        citeJoiner.innerHTML = ` ${citeJoiner.innerHTML} `;
    });

    /*  Inject preceding space when a span.cite-date follows immediately after
        a span.cite-author (i.e., there is no span.cite-joiner, because there
        are no more than two authors).
     */
    selection.querySelectorAll(".cite-author + .cite-date").forEach(citeDateAfterAuthor => {
        citeDateAfterAuthor.innerHTML = ` ${citeDateAfterAuthor.innerHTML}`;
    });

    return true;
});

/****************************************************************************/
/*  Normalize symbols (e.g. U+2731 HEAVY ASTERISK ‘✱’ => normal asterisk ‘*’)
 */
addCopyProcessor((event, selection) => {
    Typography.processElement(selection, Typography.replacementTypes.SYMBOLS);

    return true;
});


/*********************/
/* FULL-WIDTH BLOCKS */
/*********************/

/*******************************************************************************/
/*  Expands all tables (& other blocks) whose wrapper block is marked with class
    ‘width-full’, and all figures marked with class ‘width-full’, to span the
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
    let fullWidthBlockLayoutStyles = document.querySelector("head").appendChild(newElement("STYLE", { id: "full-width-block-layout-styles" }));

    /*  Function to update layout variables (called immediately and on resize).
     */
    let updateFullWidthBlockLayoutStyles = (event) => {
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
    addWindowResizeListener(updateFullWidthBlockLayoutStyles, {
        name: "updateFullWidthBlockLayoutStylesOnWindowResizeListener"
    });
}

doWhenPageLoaded(createFullWidthBlockLayoutStyles);

/************************************/
/*  Set margins of full-width blocks.
 */
addContentInjectHandler(GW.contentInjectHandlers.setMarginsOnFullWidthBlocks = (eventInfo) => {
    GWLog("setMarginsOnFullWidthBlocks", "rewrite.js", 1);

    //  Get all full-width blocks in the given document.
    let allFullWidthBlocks = eventInfo.container.querySelectorAll("div.width-full, figure.width-full");

    let removeFullWidthBlockMargins = () => {
        allFullWidthBlocks.forEach(fullWidthBlock => {
            fullWidthBlock.style.marginLeft = "";
            fullWidthBlock.style.marginRight = "";
        });
    };

    if (eventInfo.fullWidthPossible == false) {
        removeFullWidthBlockMargins();
        return;
    }

    //  Un-expand when mobile width, expand otherwise.
    doWhenMatchMedia(GW.mediaQueries.mobileWidth, "updateFullWidthBlockExpansionForCurrentWidthClass", () => {
        removeFullWidthBlockMargins();
    }, () => {
        allFullWidthBlocks.forEach(fullWidthBlock => {
            //  Compensate for block indentation due to nesting (e.g., lists).
            let additionalLeftAdjustmentPx = "0px";
            let enclosingListItem = fullWidthBlock.closest("li");
            if (enclosingListItem) {
                let fullContentRect = fullWidthBlock.closest(".markdownBody").getBoundingClientRect();
                let listContentRect = enclosingListItem.firstElementChild.getBoundingClientRect();
                additionalLeftAdjustmentPx = (fullContentRect.x - listContentRect.x) + "px";
            }

            fullWidthBlock.style.marginLeft = `calc(
                                                    (-1 * (var(--GW-full-width-block-layout-left-adjustment) / 2.0))
                                                  + (var(--GW-full-width-block-layout-side-margin))
                                                  - ((var(--GW-full-width-block-layout-page-width) - 100%) / 2.0)
                                                  + (${additionalLeftAdjustmentPx} / 2.0)
                                                )`;
            fullWidthBlock.style.marginRight = `calc(
                                                     (var(--GW-full-width-block-layout-left-adjustment) / 2.0)
                                                   + (var(--GW-full-width-block-layout-side-margin))
                                                   - ((var(--GW-full-width-block-layout-page-width) - 100%) / 2.0)
                                                   - (${additionalLeftAdjustmentPx} / 2.0)
                                                )`;
        });
    });
}, ">rewrite");


/***************/
/* ANNOTATIONS */
/***************/

/******************************************************************************/
/*  Transform title-link of truncated annotations (i.e., full annotations
    transcluded as partial annotations) to allow access to the full annotation.
 */
addContentLoadHandler(GW.contentLoadHandlers.rewriteTruncatedAnnotations = (eventInfo) => {
    GWLog("rewriteTruncatedAnnotations", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".annotation-partial").forEach(partialAnnotation => {
        //  Check to see whether the abstract exists.
        if (Annotations.referenceDataForLink(eventInfo.includeLink).content.abstract == null)
            return;

        //  Rewrite title-link.
        partialAnnotation.querySelector("a.title-link").classList.add(Annotations.annotatedLinkFullClass);
    });
}, "<rewrite", (info) => (   info.source == "transclude"
                          && info.contentType == "annotation"));

/**********************************************************/
/*	Strip quotes from title-links in annotation pop-frames.
 */
addContentInjectHandler(GW.contentInjectHandlers.rewriteAnnotationTitleLinksInPopFrames = (eventInfo) => {
    GWLog("rewriteAnnotationTitleLinksInPopFrames", "rewrite.js", 1);

	eventInfo.container.querySelector(".data-field.title .title-link")?.trimQuotes();
}, "rewrite", (info) => (   info.source == "transclude"
						 && info.contentType == "annotation"
						 && info.context == "popFrame"));

/***************************************************************************/
/*  Apply proper classes to inline file-include collapses, both on directory
    index pages and in annotations.
 */
addContentInjectHandler(GW.contentInjectHandlers.rectifyFileAppendClasses = (eventInfo) => {
    GWLog("rectifyFileAppendClasses", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".aux-links-transclude-file, .file-includes").forEach(fileIncludesBlock => {
        //  The file-include block itself may be a collapse! If so, wrap it.
        if (fileIncludesBlock.matches(".collapse"))
            fileIncludesBlock = wrapElement(fileIncludesBlock, "div.file-includes", { moveClasses: [ "data-field", "file-includes" ] });
        //  Rectify class.
        fileIncludesBlock.swapClasses([ "aux-links-transclude-file", "file-includes" ], 1);
        //  Apply standard class to all collapses within the includes block.
        fileIncludesBlock.querySelectorAll(".collapse").forEach(fileIncludeCollapse => {
            fileIncludeCollapse.swapClasses([ "aux-links-transclude-file", "file-include-collapse" ], 1);
            fileIncludeCollapse.swapClasses([ "bare-content", "bare-content-not" ], 1);
        });
    });
}, "rewrite");

/******************************************************************************/
/*  Properly handle file includes in annotations when their include-link fires.
 */
addContentInjectHandler(GW.contentInjectHandlers.handleFileIncludeUncollapseInAnnotations = (eventInfo) => {
    GWLog("handleFileIncludeUncollapseInAnnotations", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".file-include-collapse").forEach(fileIncludeCollapse => {
        let includeLink = fileIncludeCollapse.querySelector("a");
        GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (embedInjectEventInfo) => {
            /*  Don’t scroll to an embed in the main document if there are
                popups on screen.
             */
            if (   embedInjectEventInfo.document == document
                && Extracts.popFrameProvider == Popups
                && Popups.allSpawnedPopups().length > 0)
                return;

            let embed = embedInjectEventInfo.container.firstElementChild;

            //  Scroll into view (but not if it’s off-screen).
            if (isOnScreen(embed))
                scrollElementIntoView(embed);
            if (   embed.tagName == "IFRAME"
                && Extracts.popFrameProvider.containingPopFrame(embed) != null)
                embed.addEventListener("load", (event) => {
                    if (isOnScreen(embed))
                        scrollElementIntoView(embed);
                }, { once: true });

            //  Designate now-last collapse for styling.
            let previousBlock = previousBlockOf(embed);
            if (   embed.closest(".collapse") == null
                && previousBlock?.classList.contains("collapse-block"))
                previousBlock.classList.add("last-collapse");
        }, {
            once: true,
            condition: (info) => (info.includeLink == includeLink)
        });
    });
}, "eventListeners", (info) => (info.contentType == "annotation"));

/***************************************************************************/
/*  Because annotations transclude aux-links, we make the aux-links links in
    the metadata line of annotations scroll down to the appended aux-links
    blocks.
 */
addContentInjectHandler(GW.contentInjectHandlers.rewriteAuxLinksLinksInTranscludedAnnotations = (eventInfo) => {
    GWLog("rewriteAuxLinksLinksInTranscludedAnnotations", "rewrite.js", 1);

    let annotation = eventInfo.container.querySelector(".annotation");
    if (annotation == null)
        return;

    let inPopFrame = (Extracts.popFrameProvider.containingPopFrame(annotation) != null);

    annotation.querySelectorAll(".data-field.aux-links a.aux-links").forEach(auxLinksLink => {
        let auxLinksLinkType = AuxLinks.auxLinksLinkType(auxLinksLink);
        let includedAuxLinksBlock = annotation.querySelector(`.${auxLinksLinkType}-append`);
        if (includedAuxLinksBlock) {
            auxLinksLink.onclick = () => { return false; };
            auxLinksLink.addActivateEvent((event) => {
                if (includedAuxLinksBlock.querySelector("ul, ol") == null) {
                    GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
                        revealElement(includedAuxLinksBlock);
                    }, { once: true });
                }

                revealElement(includedAuxLinksBlock);

                return false;
            });
        }
    });
}, "eventListeners", (info) => (info.contentType == "annotation"));

/******************************************************************************/
/*  Bind mouse hover events to, when hovering over an annotated link, highlight
    that annotation (as viewed in a tags directory, for instance).
 */
addContentInjectHandler(GW.contentInjectHandlers.bindSectionHighlightEventsToAnnotatedLinks = (eventInfo) => {
    GWLog("bindSectionHighlightEventsToAnnotatedLinks", "rewrite.js", 1);

    Annotations.allAnnotatedLinksInContainer(eventInfo.container).forEach(annotatedLink => {
        //  Unbind existing events, if any.
        if (annotatedLink.annotatedLinkMouseEnter)
            annotatedLink.removeEventListener("mouseenter", annotatedLink.annotatedLinkMouseEnter);
        if (annotatedLink.annotatedLinkMouseLeave)
            annotatedLink.removeEventListener("mouseleave", annotatedLink.annotatedLinkMouseLeave);

        //  Bind events.
        let escapedLinkURL = CSS.escape(decodeURIComponent(annotatedLink.href));
        let targetAnalogueInLinkBibliography = document.querySelector(`a[id^='link-bibliography'][href='${escapedLinkURL}']`);
        if (   targetAnalogueInLinkBibliography
            && targetAnalogueInLinkBibliography != annotatedLink) {
            let containingSection = targetAnalogueInLinkBibliography.closest("section");
            if (containingSection) {
                annotatedLink.addEventListener("mouseenter", annotatedLink.annotatedLinkMouseEnter = (event) => {
                    clearTimeout(containingSection.highlightFadeTimer);
                    containingSection.classList.toggle("highlight-fading", false);
                    containingSection.classList.toggle("highlighted", true);
                });
                annotatedLink.addEventListener("mouseleave", annotatedLink.annotatedLinkMouseLeave = (event) => {
                    containingSection.classList.toggle("highlight-fading", true);
                    containingSection.highlightFadeTimer = setTimeout(() => {
                        containingSection.classList.toggle("highlight-fading", false);
                        containingSection.classList.toggle("highlighted", false);
                    }, 150);
                });
            }
        }
    });
}, "eventListeners");


/*********************/
/* DIRECTORY INDEXES */
/*********************/

/******************************************************************************/
/*  On directory index pages, remove invalid include-links in file-append
    sections; if no valid includes remain, delete the entire file-append block.
 */
addContentLoadHandler(GW.contentLoadHandlers.stripInvalidFileAppends = (eventInfo) => {
    GWLog("stripInvalidFileAppends", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".aux-links-transclude-file").forEach(fileAppendBlock => {
        /*  Remove any file embed links that lack a valid content type (e.g.,
            foreign-site links that have not been whitelisted for embedding; or
            a PDF embed, on a mobile client, which is considered invalid because
            mobile browsers do not support PDF embedding).
         */
        Transclude.allIncludeLinksInContainer(fileAppendBlock).forEach(includeLink => {
            if (Content.contentTypeForLink(includeLink) == null)
                includeLink.remove();
        });

        //  If no valid include-links remain, delete the whole block.
        if (isNodeEmpty(fileAppendBlock)) {
            //  Delete colon.
            if (fileAppendBlock.previousElementSibling.lastTextNode.nodeValue == ":")
                fileAppendBlock.previousElementSibling.lastTextNode.remove();

            fileAppendBlock.remove();
        }
    });
}, "rewrite", (info) => (   info.container == document.main
                         && /\/(index)?$/.test(location.pathname)));


/*********************/
/* LINK BIBLIOGRAPHY */
/*********************/

/*****************************************************************************/
/*  Apply a class to those link-bibs that should use the more compact styling.
 */
addContentInjectHandler(GW.contentInjectHandlers.applyLinkBibliographyCompactStylingClass = (eventInfo) => {
    GWLog("applyLinkBibliographyCompactStylingClass", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".link-bibliography-list").forEach(linkBibList => {
        if (linkBibList.closest("li, .link-bibliography-append, .popframe-body.link-bibliography"))
            linkBibList.classList.add("link-bibliography-list-compact");
    });
}, "rewrite");

/****************************************************/
/*	Adjust layout of link bibliography context links.
 */
addContentInjectHandler(GW.contentInjectHandlers.rectifyLinkBibliographyContextLinks = (eventInfo) => {
    GWLog("rectifyLinkBibliographyContextLinks", "rewrite.js", 1);

	eventInfo.container.querySelectorAll(".link-bibliography-context").forEach(link => {
		//	Inject context links into annotations, once those load.
		let linkBibEntryIncludeLink = link.closest("li").querySelector("a:not(.link-bibliography-context)");
		if (Transclude.isAnnotationTransclude(linkBibEntryIncludeLink)) {
			GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", (info) => {
				let annotationTitleLine = info.container.querySelector(".data-field.title");
				annotationTitleLine.insertBefore(document.createTextNode(" "), annotationTitleLine.firstChild);
				annotationTitleLine.insertBefore(link, annotationTitleLine.firstChild);
			}, {
				condition: (info) => (info.includeLink == linkBibEntryIncludeLink),
				once: true
			});
		}
	});
}, "<rewrite", (info) => (   info.source == "transclude"
						  && info.loadLocation?.pathname.startsWith("/metadata/annotation/link-bibliography/")));


/*********************/
/* TABLE OF CONTENTS */
/*********************/

/******************************************************************/
/*  Sets TOC collapse state and updates the collapse toggle button.
 */
function setTOCCollapseState(collapsed = false) {
    let TOC = document.querySelector("#TOC");
    if (TOC == null)
        return;

    TOC.classList.toggle("collapsed", collapsed);

    let button = TOC.querySelector(".toc-collapse-toggle-button");
    if (button == null)
        return;

    button.title = collapsed ? "Expand table of contents" : "Collapse table of contents";
}

/*******************************************************/
/*  Add the collapse toggle button to the main page TOC.
 */
addContentLoadHandler(GW.contentLoadHandlers.injectTOCCollapseToggleButton = (eventInfo) => {
    GWLog("injectTOCCollapseToggleButton", "rewrite.js", 1);

    let TOC = document.querySelector("#TOC");
    if (TOC == null)
        return;

    let button = newElement("BUTTON", {
        "class": "toc-collapse-toggle-button",
        "title": "Collapse table of contents",
        "tabindex": "-1"
    }, {
        "innerHTML": `<span>${(GW.svg("chevron-left-solid"))}</span>`
    });
    TOC.appendChild(button);

    let defaultTOCCollapseState = "false";
    setTOCCollapseState((localStorage.getItem("toc-collapsed") ?? defaultTOCCollapseState) == "true");

    button.addActivateEvent((event) => {
        setTOCCollapseState(TOC.classList.contains("collapsed") == false);
        localStorage.setItem("toc-collapsed", TOC.classList.contains("collapsed"));
    });
}, "rewrite", (info) => (info.container == document.main));

/***************************************************************************/
/*  Strip spurious <span> tags (unavoidably added by Pandoc) from TOC links
    (only in the page-level TOC).
 */
addContentLoadHandler(GW.contentLoadHandlers.stripTOCLinkSpans = (eventInfo) => {
    GWLog("stripTOCLinkSpans", "rewrite.js", 1);

    unwrapAll(".TOC li a > span:not([class])", {
        root: eventInfo.container
    });
}, "rewrite", (info) => (info.container == document.main));

/**************************************************************************/
/*  Update main page TOC with any sections within the initially loaded page
    that don’t already have TOC entries.
 */
addContentLoadHandler(GW.contentLoadHandlers.updateMainPageTOC = (eventInfo) => {
    GWLog("updateMainPageTOC", "rewrite.js", 1);

    updatePageTOC();
}, "rewrite", (info) => (info.container == document.main));

/*************************************************/
/*  Apply typography rectification to TOC entries.
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyTypographyInTOC = (eventInfo) => {
    GWLog("rectifyTypographyInTOC", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".TOC").forEach(TOC => {
        Typography.processElement(TOC, Typography.replacementTypes.WORDBREAKS);
    });
}, "rewrite");

/**********************************************************/
/*  Disable link decoration (underlining) on all TOC links.
 */
addContentLoadHandler(GW.contentLoadHandlers.disableTOCLinkDecoration = (eventInfo) => {
    GWLog("disableTOCLinkDecoration", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".TOC a").forEach(link => {
        link.classList.add("decorate-not");
    });
}, "rewrite");

/**********************************************************/
/*  Relocate and clean up TOC on tag directory index pages.
 */
addContentLoadHandler(GW.contentLoadHandlers.rewriteDirectoryIndexTOC = (eventInfo) => {
    GWLog("rewriteDirectoryIndexTOC", "rewrite.js", 1);

    let TOC = document.querySelector("#TOC");
    let seeAlsoSection = document.querySelector("#see-also");

    if (   TOC == null
        || seeAlsoSection == null)
        return;

    /*  Place the TOC after the “See Also” section (which also places it after
        the page abstract, if such exists, because that comes before the
        “See Also” section).
     */
    seeAlsoSection.parentElement.insertBefore(TOC, seeAlsoSection.nextElementSibling);

    //  The “See Also” section no longer needs a TOC entry.
    TOC.querySelector("#toc-see-also").closest("li").remove();

    /*  If “Links” is the only remaining section, then it does not itself need
        a TOC entry; shift its children up one TOC level.
     */
    let linksTOCEntry = TOC.querySelector("#toc-links");
    if (   linksTOCEntry
        && isOnlyChild(linksTOCEntry.closest("li"))) {
        let outerTOCList = TOC.querySelector("ul");
        let innerTOCList = TOC.querySelector("#toc-links + ul");

        TOC.insertBefore(innerTOCList, null);
        outerTOCList.remove();

        //  Mark with special class, for styling purposes.
        TOC.classList.add("TOC-links-only");
    }

    //  Update visibility.
    updateTOCVisibility(TOC);
}, "rewrite", (info) => (   info.container == document.main
                         && /\/(index)?$/.test(location.pathname)));

/***************************************************************************/
/*  Add recently-modified link icons in page TOC, to indicate recently added
	page sections.
 */
addContentLoadHandler(GW.contentLoadHandlers.addRecentlyModifiedDecorationsToPageTOC = (eventInfo) => {
    GWLog("addRecentlyModifiedDecorationsToPageTOC", "rewrite.js", 1);

	let excludedPaths = [
		"/blog/",
		"/ref/"
	];
	if (location.pathname.startsWithAnyOf(excludedPaths))
		return;

	let TOC = document.querySelector("#TOC");
	if (TOC == null)
		return;

	/*	Create document fragment with synthetic include-link for annotation
		of the current page.
	 */
    let annotationDoc = newDocument(synthesizeIncludeLink(location.pathname, { class: "link-annotated include-annotation" }));
	let annotationIncludeLink = annotationDoc.firstElementChild;

	//	Trigger include-link.
	Transclude.triggerTransclude(annotationIncludeLink, {
		source: "addRecentlyModifiedDecorationsToPageTOC",
		container: annotationDoc,
		document: annotationDoc
	}, {
		doWhenDidInject: (info) => {
			/*	Copy `link-modified-recently` class from entries in annotation 
				TOC to corresponding entries in main page TOC.
			 */
			annotationDoc.querySelectorAll(".TOC .link-modified-recently").forEach(recentlyModifiedTOCLink => {
				TOC.querySelector("#" + CSS.escape(recentlyModifiedTOCLink.id)).classList.add("link-modified-recently");
			});
			GW.contentInjectHandlers.enableRecentlyModifiedLinkIcons({ container: TOC });
		}
	});
}, "rewrite", (info) => (info.container == document.main));

/************************************************************************/
/*  If the table of contents has but one entry (or none at all), hide it.
 */
addContentLoadHandler(GW.contentLoadHandlers.updateTOCVisibility = (eventInfo) => {
    GWLog("updateTOCVisibility", "rewrite.js", 1);

    let TOC = eventInfo.container.querySelector(".TOC");
    if (TOC == null)
        return;

    updateTOCVisibility(TOC);
}, "rewrite");


/*************/
/* FOOTNOTES */
/*************/

/*****************************************************/
/*  Inject self-link for the footnotes section itself.
 */
addContentLoadHandler(GW.contentLoadHandlers.injectFootnoteSectionSelfLink = (eventInfo) => {
    GWLog("injectFootnoteSectionSelfLink", "rewrite.js", 1);

    let footnotesSection = eventInfo.container.querySelector("#footnotes");
    if (footnotesSection == null)
        return;

    let footnotesSectionSelfLink = newElement("A", {
        "class": "section-self-link graf-content-not",
        "href": "#footnotes",
        "title": "Link to section: § ‘Footnotes’"
    });

    footnotesSection.insertBefore(footnotesSectionSelfLink, footnotesSection.firstElementChild.nextElementSibling);

    //  Highlight on hover.
    footnotesSectionSelfLink.addEventListener("mouseenter", (event) => {
        footnotesSectionSelfLink.previousElementSibling.classList.toggle("highlighted", true);
    });
    footnotesSectionSelfLink.addEventListener("mouseleave", (event) => {
        footnotesSectionSelfLink.previousElementSibling.classList.toggle("highlighted", false);
    });
}, "rewrite");

/*****************************************/
/*  Add footnote class to footnote blocks.
 */
addContentLoadHandler(GW.contentLoadHandlers.addFootnoteClassToFootnotes = (eventInfo) => {
    GWLog("addFootnoteClassToFootnotes", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("#footnotes > ol > li").forEach(footnote => {
        footnote.classList.add("footnote");
    });
}, "rewrite");

/*****************************************************************************/
/*  Mark hash-targeted footnote with ‘targeted’ class on page load, and update
    when hash changes.
 */
addContentInjectHandler(GW.contentInjectHandlers.markTargetedFootnote = (eventInfo) => {
    GWLog("markTargetedFootnote", "rewrite.js", 1);

    //  Mark target footnote, if any.
    updateFootnoteTargeting();

    //  Add event handler to update targeting again on hash change.
    GW.notificationCenter.addHandlerForEvent("GW.hashDidChange", (info) => {
        updateFootnoteTargeting();
    });
}, "rewrite", (info) => info.container == document.main);

/******************************/
/*  Inject footnote self-links.
 */
addContentLoadHandler(GW.contentLoadHandlers.injectFootnoteSelfLinks = (eventInfo) => {
    GWLog("injectFootnoteSelfLinks", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("#footnotes > ol > li").forEach(footnote => {
        if (footnote.querySelector(".footnote-self-link"))
            return;

        let footnoteNumber = Notes.noteNumber(footnote);
        footnote.insertBefore(newElement("A", {
            href: `#fn${footnoteNumber}`,
            title: `Link to footnote ${footnoteNumber}`,
            class: "footnote-self-link graf-content-not"
        }, {
            innerHTML: "&nbsp;"
        }), footnote.firstChild);
    });
}, "rewrite");

/*****************************************************************/
/*  Rewrite footnote back-to-citation links (generated by Pandoc).
 */
addContentLoadHandler(GW.contentLoadHandlers.rewriteFootnoteBackLinks = (eventInfo) => {
    GWLog("rewriteFootnoteBackLinks", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("#footnotes > ol > li").forEach(footnote => {
        let backlink = footnote.querySelector(".footnote-back");

        if (backlink.querySelector("svg, .placeholder"))
            return;

        backlink.innerHTML = GW.svg("arrow-hook-left");
    });
}, "rewrite");

/***************************************************************************/
/*  Bind mouse hover events to, when hovering over a citation, highlight all
    {side|foot}notes associated with that citation.
 */
addContentInjectHandler(GW.contentInjectHandlers.bindNoteHighlightEventsToCitations = (eventInfo) => {
    GWLog("bindNoteHighlightEventsToCitations", "rewrite.js", 1);

    let allCitations = eventInfo.container.querySelectorAll(".footnote-ref");

    let bindEventsToCitation = (citation) => {
        //  Unbind existing events, if any.
        if (citation.citationMouseEnter)
            citation.removeEventListener("mouseenter", citation.citationMouseEnter);
        if (citation.citationMouseLeave)
            citation.removeEventListener("mouseleave", citation.citationMouseLeave);

        //  Bind events.
        let notesForCitation = Notes.allNotesForCitation(citation);
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
    };

    //  Bind events.
    allCitations.forEach(bindEventsToCitation);

    if (allCitations.length > 0) {
        //  Add handler to re-bind events if more notes are injected.
        addContentInjectHandler(GW.contentInjectHandlers.rebindNoteHighlightEventsToCitations = (eventInfo) => {
            allCitations.forEach(bindEventsToCitation);
        }, "eventListeners", (info) => (   info.document == document
                                        || info.document == eventInfo.document));
    }
}, "eventListeners");

/******************************************/
/*  Highlight footnote self-links on hover.
 */
addContentInjectHandler(GW.contentInjectHandlers.bindHighlightEventsToFootnoteSelfLinks = (eventInfo) => {
    GWLog("bindHighlightEventsToFootnoteSelfLinks", "rewrite.js", 1);

    //  Highlight footnote on hover over self-link.
    eventInfo.container.querySelectorAll(".footnote-self-link").forEach(footnoteSelfLink => {
        footnoteSelfLink.addEventListener("mouseenter", (event) => {
            footnoteSelfLink.parentElement.classList.toggle("highlighted", true);
        });
        footnoteSelfLink.addEventListener("mouseleave", (event) => {
            footnoteSelfLink.parentElement.classList.toggle("highlighted", false);
        });
    });
}, "eventListeners");


/*********/
/* LINKS */
/*********/

/****************************************************************************/
/*  For links with a `data-url-original` attribute, save the `href` attribute
    value in `data-url-archive`, set the `href` to the value of
    `data-url-original`, and delete `data-url-original`.
 */
addContentLoadHandler(GW.contentLoadHandlers.reverseArchivedLinkPolarity = (eventInfo) => {
    GWLog("reverseArchivedLinkPolarity", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("a[data-url-original]").forEach(archivedLink => {
        archivedLink.dataset.urlArchive = archivedLink.href;
        archivedLink.href = archivedLink.dataset.urlOriginal;
        delete archivedLink.dataset.urlOriginal;
    });
}, "<transclude");

/**********************************************************************/
/*  Qualify anchorlinks in loaded content by rewriting their `pathname`
    attributes.
 */
addContentInjectHandler(GW.contentInjectHandlers.qualifyAnchorLinks = (eventInfo) => {
    GWLog("qualifyAnchorLinks", "rewrite.js", 1);

    let baseLocation = baseLocationForDocument(eventInfo.document);
    if (baseLocation == null)
        return;

    let injectingIntoFullPage = (eventInfo.document.querySelector(".markdownBody > #page-metadata, #page-metadata.markdownBody") != null);

    eventInfo.container.querySelectorAll("a[href]").forEach(link => {
        if (   eventInfo.localize == true
            && (   link.getAttribute("href").startsWith("#")
                || link.pathname == eventInfo.loadLocation.pathname)
                   // if the link refers to an element also in the loaded content
            && (   eventInfo.container.querySelector(selectorFromHash(link.hash)) != null
                   //  if the link refers to the loaded content container itself
                || (   eventInfo.container instanceof Element
                    && eventInfo.container.matches(selectorFromHash(link.hash)))
                   //  if we’re injecting into a full page (base page or pop-frame)
                || (   injectingIntoFullPage
                           //  if we’re transcluding a citation (because we merge footnotes)
                    && (   (   eventInfo.source == "transclude"
                            && link.classList.contains("footnote-ref"))
                           //  if we’re merging a footnote for transcluded content
                        || (   eventInfo.source == "transclude.footnotes"
                            && link.classList.contains("footnote-back"))
                        )
                    )
                )
            ) {
            link.pathname = baseLocation.pathname;
        } else if (   eventInfo.loadLocation != null
        		   && link.getAttribute("href").startsWith("#")) {
			link.pathname = eventInfo.loadLocation.pathname;
        }
    });
}, "<rewrite");

/********************************************************************/
/*  Designate self-links (a.k.a. anchorlinks) and local links (a.k.a.
    within-site links) as such, via CSS classes.
 */
addContentInjectHandler(GW.contentInjectHandlers.addSpecialLinkClasses = (eventInfo) => {
    GWLog("addSpecialLinkClasses", "rewrite.js", 1);

    let baseLocation = baseLocationForDocument(eventInfo.document);
    if (baseLocation == null)
        return;

    let exclusionSelector = [
        "h1, h2, h3, h4, h5, h6",
        ".section-self-link",
        ".footnote-ref",
        ".footnote-back",
        ".footnote-self-link",
        ".sidenote-self-link",
        ".backlink-context"
    ].join(", ");

    eventInfo.container.querySelectorAll(".markdownBody a[href]").forEach(link => {
        if (   link.hostname != location.hostname
            || link.closest(exclusionSelector))
            return;

        if (link.pathname == baseLocation.pathname) {
        	link.swapClasses([ "link-self", "link-page" ], 0);
        } else if (link.pathname.slice(1).match(/[\.]/) == null) {
            link.swapClasses([ "link-self", "link-page" ], 1);
        }
    });
}, "<rewrite");

/****************************************/
/*	Add IDs to un-ID’d within-page links.
 */
addContentInjectHandler(GW.contentInjectHandlers.identifyAnchorLinks = (eventInfo) => {
    GWLog("identifyAnchorLinks", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("a.link-self").forEach(link => {
		if (link.id == "")
			link.id = "gwern-" + (link.href + link.textContent).hashCode();
	});
}, "<rewrite");

/******************************************************************************/
/*  Assign local navigation link icons: directional in-page links, generic
	(non-directional) self-links, and local page links. (These should be
	applied only within body text, including pop-frames but excluding page
	metadata sections; and should not be applied to links that already have a
	special link icon, e.g. one assigned on the back-end; nor to links that are
	specifically marked as needing no icon at all.)
 */
addContentInjectHandler(GW.contentInjectHandlers.designateLocalNavigationLinkIcons = (eventInfo) => {
    GWLog("designateLocalNavigationLinkIcons", "rewrite.js", 1);

	/*	Do not display special link icons in these containers and for these
		elements.
	 */
	let exclusionSelector = [
		".icon-not",
		".icon-special",
		"#sidebar",
		"#page-metadata",
		"#footer",
		".aux-links"
	].join(", ");

    //  Self-links (anchorlinks to the current page).
    eventInfo.container.querySelectorAll(".link-self").forEach(link => {
		if (link.closest(exclusionSelector))
			return;

        link.dataset.linkIconType = "text";
        link.dataset.linkIcon = link.hash > ""
        						? "\u{00B6}"   // ‘¶’ PILCROW SIGN
        						: "\u{1D50A}"; // ‘𝔊’ MATHEMATICAL FRAKTUR CAPITAL G [gwern.net logo]

        /*  Directional navigation links on self-links: for each self-link like
            “see [later](#later-identifier)”, find the linked identifier,
            whether it's before or after, and if it is before/previously,
            annotate the self-link with ‘↑’ (UPWARDS ARROW) and if after/later,
            ‘↓’ (DOWNWARDS ARROW).

            This helps the reader know if it’s a backwards link to an identifier
            already read, or an unread identifier, enabling a mental map and
            reducing the cognitive overhead of constantly choosing whether to
            follow a reference.

            This was implemented statically pre-transclusion as an optimization,
            but given that dynamism forces runtime checking of relative status
            for all new fragments (popups or transclude), that has been removed
            in favor of this JS hook, to simplify code & ensure a single source
            of truth.
         */
        let target = eventInfo.document.querySelector(selectorFromHash(link.hash));
        if (target == null)
        	return;

        link.dataset.linkIconType = "svg";
        link.dataset.linkIcon =
            (link.compareDocumentPosition(target) & Node.DOCUMENT_POSITION_FOLLOWING
             ? "arrow-down"
             : "arrow-up");
    });

    //  Local links (to other pages on the site).
    eventInfo.container.querySelectorAll(".link-page").forEach(link => {
		if (link.closest(exclusionSelector))
			return;

        if (   link.dataset.linkIcon
        	&& [ "arrow-down", "arrow-up" ].includes(link.dataset.linkIcon) == false)
            return;

        link.dataset.linkIconType = "text";
        link.dataset.linkIcon = [ "arrow-down", "arrow-up" ].includes(link.dataset.linkIcon)
                                ? "\u{00B6}"   // ‘¶’
                                : "\u{1D50A}"; // ‘𝔊’
    });
}, "rewrite");

/*****************************************/
/*  Removes link icons that should not be.
 */
addContentInjectHandler(GW.contentInjectHandlers.cleanSpuriousLinkIcons = (eventInfo) => {
    GWLog("cleanSpuriousLinkIcons", "rewrite.js", 1);

    let excludedLinkSelector = [
        /*  Index page, and embeds thereof, do not need the G icon.

            NOTE: we do not use the usual method of suppressing G icons
            (`.icon-not` class), because /index and /404 are *so* long
            and routinely modified/expanded, so doing it ‘manually’ would risk
            occasional omissions or syntax errors.
         */
        "body.page-index #markdownBody",
        "body.page-404 #markdownBody",
        ".popframe-body.page-index",
        ".popframe-body.page-404",

        //  TOC links should never have link icons under any circumstances.
        ".TOC",

        //  No link icons in table headers.
        "thead"
    ].map(x => x + " a[data-link-icon]").join(", ");

    eventInfo.container.querySelectorAll(excludedLinkSelector).forEach(link => {
        link.removeAttribute("data-link-icon-type");
        link.removeAttribute("data-link-icon");
    });
}, "rewrite");

/****************************************************************************/
/*  Adds HTML and CSS to a link, enabling display of its specified link icon.
 */
function enableLinkIcon(link) {
    if (link.classList.contains("has-icon"))
        return;

    //  Add hook.
    link.appendChild(newElement("SPAN", { class: "link-icon-hook dark-mode-invert" }, { innerHTML: "\u{2060}" }));

    //  Set CSS variable (link icon).
    if (link.dataset.linkIconType.includes("text")) {
		let linkIcon = link.dataset.linkIcon;

		//	Inject newline into quad link icons.
		if (link.dataset.linkIconType.includes("quad"))
			linkIcon = linkIcon.slice(0, 2) + "\\a " + linkIcon.slice(2);

        link.style.setProperty("--link-icon", `"${linkIcon}"`);
    } else if (link.dataset.linkIconType.includes("svg")) {
        let iconFileURL = versionedAssetURL("/static/img/icon/icons.svg");
        link.style.setProperty("--link-icon-url",
            `url("${iconFileURL.pathname}${iconFileURL.search}#${(link.dataset.linkIcon)}")`);
    }

    //  Set class.
    link.classList.add("has-icon");
}

/*****************************************************************************/
/*  Disables display of a link’s link icon by removing requisite HTML and CSS.
 */
function disableLinkIcon(link) {
    if (link.classList.contains("has-icon") == false)
        return;

    //  Remove hook.
    link.querySelector(".link-icon-hook").remove();

    //  Clear CSS variables.
    link.style.removeProperty("--link-icon");
    link.style.removeProperty("--link-icon-url");

    //  Unset class.
    link.classList.remove("has-icon");
}

/*************************************************************************/
/*  Enable or disable display of link icons, as appropriate for each link.
 */
addContentInjectHandler(GW.contentInjectHandlers.setLinkIconStates = (eventInfo) => {
    GWLog("setLinkIconStates", "rewrite.js", 1);

	//	Disable display of all link icons.
	eventInfo.container.querySelectorAll("a.has-icon").forEach(link => {
		disableLinkIcon(link);
	});

    //  Enable display of link icons for all links that have specified icons.
    eventInfo.container.querySelectorAll("a[data-link-icon]").forEach(link => {
		if (link.dataset.linkIcon > "")
	        enableLinkIcon(link);
    });
}, "rewrite");

/***************************************************************************/
/*  Adds HTML and CSS to a link, enabling colorization of the link icon (and
	the link underlining) on hover. (Requires color.js to be loaded.)
 */
function enableLinkIconColor(link) {
	if (   link.dataset.linkIconColor == null
		|| link.dataset.linkIconColor == "")
		return;

	/*	The transformation colorizes a base color (the text color) to match a
		reference color (the specified link icon color), while maintaining
		relative perceptual lightness.
	 */
	let transformColor = (colorCode) => {
		return Color.processColorValue(colorCode, [ {
			type: Color.ColorTransform.COLORIZE,
			referenceColor: link.dataset.linkIconColor
		} ]);
	};

	//	Set CSS variable (link icon hover color).
	link.style.setProperty("--link-icon-color-hover", transformColor("#000"));

	/*	If the link has an SVG link icon, colorize the SVG, and set the colored
		icon (via a data URI) as the link icon to display on hover.
	 */
	if (link.dataset.linkIconType?.includes("svg")) {
		doWhenSVGIconsLoaded(() => {
			let svg = elementFromHTML(GW.svg(link.dataset.linkIcon).replace(/(?<!href=)"(#[0-9A-Fa-f]+)"/g, 
				(match, colorCode) => {
					return `"${(transformColor(colorCode))}"`;
				}));
			svg.setAttribute("fill", transformColor("#000"));
			link.style.setProperty("--link-icon-url-hover", `url("data:image/svg+xml;utf8,${encodeURIComponent(svg.outerHTML)}")`);
		});
	}
}

/******************************************/
/*	Disables hover colorization for a link.
 */
function disableLinkIconColor(link) {
	link.style.removeProperty("--link-icon-color-hover");
	link.style.removeProperty("--link-icon-url-hover");
}

/*********************************************************************/
/*	Enable link hover colorization, for those links which have a color 
	specified via the data-link-icon-color attribute.
 */
addContentInjectHandler(GW.contentInjectHandlers.setLinkHoverColors = (eventInfo) => {
    GWLog("setLinkIconStates", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("a[data-link-icon-color]").forEach(enableLinkIconColor);
}, "rewrite");

/**********************************************************************/
/*	Adds recently-modified icon (white star on black circle) to a link.
 */
function addRecentlyModifiedIconToLink(link) {
	if (link.classList.contains("has-recently-modified-icon") == true)
		return;

	//  Inject indicator hook span.
	link.insertBefore(newElement("SPAN", { class: "recently-modified-icon-hook" }), link.firstChild);

	if (link.classList.contains("has-indicator-hook")) {
		/*	If the link has an indicator hook, we must inject a text node 
			containing a U+2060 WORD JOINER between the two hooks. This ensures 
			that the two link styling elements are arranged properly, and do not 
			span a line break.
		 */
		 link.insertBefore(document.createTextNode("\u{2060}"), link.querySelector(".indicator-hook"));
	} else {
		/*  Inject U+2060 WORD JOINER at start of first text node of the
			link. (It _must_ be injected as a Unicode character into the
			existing text node; injecting it within the .indicator-hook
			span, or as an HTML escape code into the text node, or in
			any other fashion, creates a separate text node, which
			causes all sorts of problems - text shadow artifacts, etc.)
		 */
		let linkFirstTextNode = link.firstTextNode;
		if (   linkFirstTextNode
			&& linkFirstTextNode.textContent.startsWith("\u{2060}") == false)
			linkFirstTextNode.textContent = "\u{2060}" + linkFirstTextNode.textContent;
	}

	link.classList.add("has-recently-modified-icon");
}

/***************************************************************************/
/*	Removes recently-modified icon (white star on black circle) from a link.
 */
function removeRecentlyModifiedIconFromLink(link) {
	if (link.classList.contains("has-recently-modified-icon") == false)
		return;

	let iconHook = link.querySelector(".recently-modified-icon-hook");
	if (iconHook.nextSibling.firstTextNode.textContent.startsWith("\u{2060}"))
		iconHook.nextSibling.firstTextNode.textContent = iconHook.nextSibling.firstTextNode.textContent.slice(1);
	iconHook.remove();

	link.classList.remove("has-recently-modified-icon");

	/*	If this link has an indicator hook, then we must remove the text node 
		containing U+2060 WORD JOINER between the two hooks.
	 */
	if (   link.classList.contains("has-indicator-hook")
		&& link.firstTextNode.textContent == "\u{2060}")
		link.firstTextNode.remove();
}

/****************************************************************************/
/*  Enable special icons for recently modified links (that are not in lists).
 */
addContentInjectHandler(GW.contentInjectHandlers.enableRecentlyModifiedLinkIcons = (eventInfo) => {
    GWLog("enableRecentlyModifiedLinkIcons", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("a.link-modified-recently:not(.in-list)").forEach(addRecentlyModifiedIconToLink);
}, "rewrite");


/***************/
/* DATE RANGES */
/***************/

/****************************************************************************/
/*  Makes it so that copying a date range interacts properly with copy-paste.
 */
addCopyProcessor((event, selection) => {
    stripDateRangeMetadataInBlock(selection);

    return true;
});


/************************/
/* INFLATION ADJUSTMENT */
/************************/

GW.currencyFormatter = new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD',
    minimumFractionDigits: 2
});
GW.currentYear = new Date().getFullYear();

/*************************************************************************/
/*  Return prettified version of a string representing an amount of money.
 */
function prettifyCurrencyString(amount, compact = false, forceRound = false) {
    let currency = amount[0];

    let number = Number(amount.replace(/[^0-9.−-]+/g, ""));
    if (   number >= 100
        || forceRound)
        number = Math.round(number);

    amount = GW.currencyFormatter.format(number);

    //  Remove trailing zeroes.
    amount = amount.replace(/\.00?$/, '');

    //  Reset currency unit.
    amount = currency + amount.slice(1);

    if (compact) {
        amount = amount.replace(/,000,000,000$/, 'b');
        amount = amount.replace(/,000,000$/, 'm');
        amount = amount.replace(/,000$/, 'k');
    }

    return amount;
}

/**************************************************************************/
/*  Rewrite inflation-adjustment elements to make the currency amounts more
    useful and readable.
 */
addContentLoadHandler(GW.contentLoadHandlers.rewriteInflationAdjusters = (eventInfo) => {
    GWLog("rewriteInflationAdjusters", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".inflation-adjusted").forEach(infAdj => {
        let unadjusted = infAdj.querySelector("sup");
        let adjusted = infAdj.firstChild;

        unadjusted.textContent = prettifyCurrencyString(unadjusted.textContent, true);

        /*  Always round adjusted amount if unadjusted amount has no fractional
            component and adjusted amount has more than one whole digit.
         */
        let forceRound = (   unadjusted.textContent.includes(".") == false
                          && adjusted.textContent.match(/([0-9]+)(\.|$)/)[1].length > 1);
        adjusted.textContent = prettifyCurrencyString(adjusted.textContent, false, forceRound);
    });
}, "rewrite");

/***************************************************************************/
/*  Makes it so that copying an inflation-adjusted currency amount interacts
    properly with copy-paste.
 */
addCopyProcessor((event, selection) => {
    /*  Rewrite inflation-adjuster elements into a simple inline typographical
        format, e.g. “$0.10 (1990; $1.30 in 2023)”.
     */
    selection.querySelectorAll(".inflation-adjusted").forEach(infAdj => {
        let adjustedText = infAdj.firstChild.textContent;
        let unadjustedText = infAdj.querySelector("sup").textContent;
        let yearText = infAdj.querySelector("sub").textContent;

        //  Un-abbreviate powers of 1,000 in unadjusted amount.
        unadjustedText = unadjustedText.replace("k", ",000");
        unadjustedText = unadjustedText.replace("m", ",000,000");
        unadjustedText = unadjustedText.replace("b", ",000,000,000");

        infAdj.innerHTML = `${unadjustedText} [${yearText}; ${adjustedText} in ${GW.currentYear}]`;
    });

    return true;
});

/******************************************************************************/
/*  Makes double-clicking on an inflation adjuster select the entire element.
    (This is so that the copy processor, above, can reliably work as intended.)
 */
addContentInjectHandler(GW.contentInjectHandlers.addDoubleClickListenersToInflationAdjusters = (eventInfo) => {
    GWLog("addDoubleClickListenersToInflationAdjusters", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".inflation-adjusted").forEach(infAdj => {
        infAdj.addEventListener("dblclick", (event) => {
            document.getSelection().selectNode(infAdj);
        });
    });
}, "eventListeners");


/*********/
/* MISC. */
/*********/

/******************************************************************************/
/*	Resolve random element selectors (i.e., containers with a class like
	“display-random-one”) by uniform-randomly selecting the requisite number of
	child elements and making them visible.
 */
addContentInjectHandler(GW.contentInjectHandlers.resolveRandomElementSelectors = (eventInfo) => {
    GWLog("resolveRandomElementSelectors", "rewrite.js", 1);

	let wordsToNumbersMapping = {
		"one":    1,
		"two":    2,
		"three":  3,
		"four":   4,
		"five":   5,
		"six":    6,
		"seven":  7,
		"eight":  8,
		"nine":   9,
		"ten":   10,
	};

	eventInfo.container.querySelectorAll("[class*='display-random-']:not(.visible)").forEach(randomSelectorContainer => {
		//	Determine how many elements to display.
		let howMany = Array.from(randomSelectorContainer.classList).find(cssClass => /^display-random-/.test(cssClass))?.slice("display-random-".length);
		howMany = wordsToNumbersMapping[howMany];

		/*	Select elements to display, until as many as needed are displayed,
			or else none remain to display.
		 */
		let childElements = Array.from(randomSelectorContainer.children);
		while (   howMany > 0
			   && childElements.length > 0) {
			let selectedChildElement = childElements[rollDie(childElements.length) - 1];
			selectedChildElement.classList.add("visible");
			childElements.remove(selectedChildElement);
			howMany--;
		}

		//	Make the container visible.
		randomSelectorContainer.classList.add("visible");
	});
}, "rewrite");

/*********************************************************/
/*	Regenerate placeholder IDs. (See misc.js for details.)
 */
addContentInjectHandler(GW.contentInjectHandlers.regeneratePlaceholderIds = (eventInfo) => {
    GWLog("removeNoscriptTags", "rewrite.js", 1);

	regeneratePlaceholderIds(eventInfo.container);
}, "rewrite");

/*****************************************************************************/
/*	For obvious reasons, <noscript> tags are completely useless in any content
	loaded by this code, and they sometimes interfere with stuff.
 */
addContentLoadHandler(GW.contentLoadHandlers.removeNoscriptTags = (eventInfo) => {
    GWLog("removeNoscriptTags", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("noscript").forEach(noscript => {
		noscript.remove();
	});
}, "<rewrite");

GW.defaultImageAuxText = "[Image]";

/***************************************************************************/
/*  Clean up image alt-text. (Shouldn’t matter, because all image URLs work,
    right? Yeah, right...)
 */
addContentLoadHandler(GW.contentLoadHandlers.cleanUpImageAltText = (eventInfo) => {
    GWLog("cleanUpImageAltText", "rewrite.js", 1);

    /*  If an image has no alt text, use the value of the ‘title’ attribute,
        if present; otherwise, a default string (“Image”).
     */
    eventInfo.container.querySelectorAll("img:not([alt])").forEach(image => {
        image.alt = (image.title || GW.defaultImageAuxText);
    });

    //  URL-encode ‘%’ signs in image alt text.
    eventInfo.container.querySelectorAll("img[alt]").forEach(image => {
        image.alt = decodeURIComponent(image.alt.replace(/%(?![A-Fa-f0-9]{2})/g, "%25"));
    });
}, "rewrite");

/************************************************************************/
/*  Prevent line breaks immediately before citations (which “orphans” the
    citation on the next line, and looks ugly) and immediately after citations
    (which causes punctuation following a citation to be orphaned, and also
    looks ugly).
 */
addContentLoadHandler(GW.contentLoadHandlers.noBreakForCitations = (eventInfo) => {
    GWLog("noBreakForCitations", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".footnote-ref").forEach(citation => {
        citation.parentElement.insertBefore(document.createTextNode("\u{2060}"), citation);
        let textNode = citation.querySelector("sup").firstTextNode;
        textNode.textContent = "\u{2060}" + textNode.textContent + "\u{2060}";
    });
}, "rewrite");

/****************************************************************************/
/*  Designate containers wherein colors (e.g. link colors) should be inverted
    (because the container has a dark background).
 */
addContentLoadHandler(GW.contentLoadHandlers.designateColorInvertedContainers = (eventInfo) => {
    GWLog("designateColorInvertedContainers", "rewrite.js", 1);

    let selector = [
        ".admonition.warning",
        ".admonition.error"
    ].join(", ");

    eventInfo.container.querySelectorAll(selector).forEach(container => {
        container.classList.add("colors-invert");
    });
}, "rewrite");

/******************************************************************/
/*  Wrap text nodes and inline elements in admonitions in <p> tags.
 */
addContentLoadHandler(GW.contentLoadHandlers.paragraphizeAdmonitionTextNodes = (eventInfo) => {
    GWLog("paragraphizeAdmonitionTextNodes", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".admonition", ".admonition-title").forEach(paragraphizeTextNodesOfElementRetainingMetadata);
}, "rewrite");

/*********************************************/
/*  Fix incorrect text block tag types.
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifySpecialTextBlockTagTypes = (eventInfo) => {
    GWLog("rectifySpecialTextBlockTagTypes", "rewrite.js", 1);

	//	Classes which are on <div> but should be on <p>.
	let problematicBlockSelector = [
		"text-center",
		"smallcaps"
	].map(className => `div.${className}`).join(", ");

    eventInfo.container.querySelectorAll(problematicBlockSelector).forEach(div => {
        unwrap(div, {
        	moveID: true,
        	moveClasses: true
        });
    });
}, "rewrite");

/*******************************************************/
/*  Designate ordinal superscripts (1st, 2nd, 3rd, nth).
 */
addContentLoadHandler(GW.contentLoadHandlers.designateOrdinals = (eventInfo) => {
    GWLog("designateOrdinals", "rewrite.js", 1);

    eventInfo.container.querySelectorAll("sup").forEach(sup => {
        if ([ "st", "nd", "rd", "th" ].includes(sup.textContent.toLowerCase()))
            sup.classList.add("ordinal");
    });
}, "rewrite");

/**********************************************************/
/*	Inject progress indicator icons into any element with a 
	data-progress-percentage attribute.
 */
addContentLoadHandler(GW.contentLoadHandlers.injectProgressIcons = (eventInfo) => {
    GWLog("injectProgressIcons", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("[data-progress-percentage]").forEach(renderProgressPercentageIcon);
}, "rewrite");

/*********************************************************************/
/*	Fix a minor appearance glitch in some fields in the page metadata.
 */
addContentLoadHandler(GW.contentLoadHandlers.rectifyPageMetadataFieldLinkAppearance = (eventInfo) => {
    GWLog("rectifyPageMetadataFieldLinkAppearance", "rewrite.js", 1);

	eventInfo.container.querySelectorAll("#page-metadata a").forEach(pageMetadataLink => {
		let nextNode = pageMetadataLink.nextSibling;
		if (   nextNode?.nodeType == Node.TEXT_NODE
			&& nextNode?.nodeValue.startsWith(":")) {
			nextNode.remove();
			pageMetadataLink.parentElement.insertBefore(newElement("SPAN", null, { innerHTML: nextNode.nodeValue }), pageMetadataLink.nextSibling);
		}
	});
}, "rewrite");

/***************************************************************************/
/*	Make blocks that are next to the TOC clear the TOC if they are too long.
 */
addContentInjectHandler(GW.contentInjectHandlers.rectifyTOCAdjacentBlockLayout = (eventInfo) => {
    GWLog("rectifyTOCAdjacentBlockLayout", "rewrite.js", 1);

	let markdownBody = document.querySelector("#markdownBody");
	let TOC = document.querySelector("#TOC");

	GW.layout.TOCAdjacentBlockLayoutNeedsRectification = false;

	let rectifyTOCAdjacentBlockLayoutIfNeeded = () => {
		if (GW.layout.TOCAdjacentBlockLayoutNeedsRectification == false)
			return;

		GW.layout.TOCAdjacentBlockLayoutNeedsRectification = false;

		let TOCRect = TOC.getBoundingClientRect();

		let blockOptions = {
			notBlockElements: [ "section" ],
			alsoWrapperElements: [ "section" ]
		};
		let block = firstBlockOf(markdownBody, blockOptions, true);
		while (block = nextBlockOf(block, blockOptions)) {
			if (   block.classList.contains("collapse")
				&& (   isCollapsed(block) == false
					|| block.style.clear > ""))
				continue;

			block.style.removeProperty("clear");
			block.closest("section")?.style.removeProperty("clear");

			let blockRect = block.getBoundingClientRect();
			if (   blockRect.top <= TOCRect.bottom
				&& blockRect.bottom - TOCRect.bottom > window.innerHeight * 0.5) {
				if (previousBlockOf(block) == null) {
					block.closest("section").style.clear = "left";
				} else {
					block.style.clear = "left";
				}

				TOC.style.marginBottom = "2.5rem";
			} else if (   blockRect.top > TOCRect.bottom
					   && block.style.clear == ""
					   && (block.closest("section")?.style.clear > "") == false) {
				break;
			}
		}
	};

	GW.notificationCenter.addHandlerForEvent("Layout.layoutProcessorDidComplete", (layoutEventInfo) => {
		GW.layout.TOCAdjacentBlockLayoutNeedsRectification = true;

		requestAnimationFrame(rectifyTOCAdjacentBlockLayoutIfNeeded);
	}, {
		condition: (layoutEventInfo) => (   layoutEventInfo.container == document.main
										 && layoutEventInfo.processorName == "applyBlockSpacingInContainer")
	});
}, "rewrite", (info) => info.container == document.main);

/****************************************************************************/
/*	Remove from copied content anything that is hidden on the current type of
	client (i.e., via the .mobile-not or .desktop-not classes).
 */
addCopyProcessor((event, selection) => {
	selection.querySelectorAll(GW.isMobile() ? ".mobile-not" : ".desktop-not").forEach(element => { element.remove(); });

	return true;
});

/****************************************************************************/
/*	Ensure that inline mode selectors have reasonable textual representations
	in copied content.
 */
addCopyProcessor((event, selection) => {
	selection.querySelectorAll(".mode-selector-inline button, .link-widget a").forEach(button => {
		let label = button.dataset.name ?? button.getAttribute("aria-label") ?? (button.getAttribute("title") || button.getAttribute("href"))
		if (button.classList.contains("selected"))
			label = label.toUpperCase();
		button.replaceWith(document.createTextNode("[" + label + "]"));
	});

	return true;
});


/************/
/* DROPCAPS */
/************/

/***************************************************/
/*  Dropcaps (only on sufficiently wide viewports).
 */
addContentInjectHandler(GW.contentInjectHandlers.rewriteDropcaps = (eventInfo) => {
    GWLog("rewriteDropcaps", "rewrite.js", 1);

    //  Reset dropcaps when margin note mode changes.
    doWhenMatchMedia(Sidenotes.mediaQueries.viewportWidthBreakpoint, "GW.dropcaps.resetDropcapsWhenMarginNoteModeChanges", (mediaQuery) => {
        eventInfo.container.querySelectorAll(GW.dropcaps.dropcapBlockSelector).forEach(resetDropcapInBlock);
    });

    //  A letter (capital or lowercase), optionally preceded by an opening quotation mark.
    let initialRegexp = new RegExp(/^(\s*[“‘]?)?([a-zA-Z])/);

    processContainerNowAndAfterBlockLayout(eventInfo.container, (container) => {
        container.querySelectorAll(GW.dropcaps.dropcapBlockSelector).forEach(dropcapBlock => {
            //  If this dropcap has already been processed, do nothing.
            if (dropcapBlock.querySelector(".dropcap"))
                return;

            //  Make sure the graf begins properly and determine initial letter.
            let initial = initialRegexp.exec(textContentOfGraf(dropcapBlock));
            if (initial == null) {
                addDropcapClassTo(dropcapBlock, "not");
                return;
            }
            let [ fullInitial, precedingPunctuation, initialLetter ] = initial;

            //  Locate insertion point.
            let firstNode = firstTextNodeOfGraf(dropcapBlock);
            let firstNodeParent = firstNode.parentElement;

            //  Separate first letter from rest of text content.
            firstNode.textContent = firstNode.textContent.slice(fullInitial.length);

            //  Determine dropcap type.
            let dropcapType = dropcapTypeOf(dropcapBlock);

            //  Is this is a graphical dropcap?
            if (GW.dropcaps.graphicalDropcapTypes.includes(dropcapType)) {
                //  Designate as graphical dropcap block.
                dropcapBlock.classList.add("graphical-dropcap");

                //  Inject a hidden span to hold the first letter as text.
                firstNodeParent.insertBefore(newElement("SPAN", {
                    class: "hidden-initial-letter",
                }, {
                    innerHTML: initialLetter
                }), firstNode);

                //  Construct the dropcap image element.
                let dropcapImage = newElement("IMG", {
                    class: "dropcap figure-not",
                    loading: "lazy"
                });

                //  Select a dropcap.
                let dropcapURL = getDropcapURL(dropcapType, initialLetter);
                if (dropcapURL == null) {
                    //  If no available dropcap image, set disabled flag.
                    dropcapBlock.classList.add("disable-dropcap");
                } else {
                    //  Specify image URL.
                    dropcapImage.src = dropcapURL.pathname + dropcapURL.search;

                    //  Add image file format class.
                    dropcapImage.classList.add(dropcapURL?.pathname.slice(-3));

                    /*  Dropcap should be inverted if it’s designed for a mode
                        opposite to the current mode (rather than being designed
                        either for the current mode or for either mode); in such a
                        case it will have the opposite mode in the file name.
                     */
                    let shouldInvert = dropcapURL.pathname.includes("-" + (DarkMode.computedMode() == "light" ? "dark" : "light"));
                    if (shouldInvert)
                        dropcapImage.classList.add("invert");
                }

                //  Inject the dropcap image element.
                firstNodeParent.insertBefore(dropcapImage, firstNode.previousSibling);
            } else {
                //  Inject the dropcap.
                firstNodeParent.insertBefore(newElement("SPAN", {
                    class: "dropcap"
                }, {
                    innerHTML: initialLetter.toUpperCase()
                }), firstNode);
            }

            //  If there’s punctuation before the initial letter, inject it.
            if (precedingPunctuation) {
                firstNodeParent.insertBefore(newElement("SPAN", {
                    class: "initial-preceding-punctuation"
                }, {
                    innerHTML: precedingPunctuation
                }), firstNodeParent.querySelector(".dropcap"));
            }
        });
    });
}, "rewrite", (info) => (   info.document == document
                         && GW.mediaQueries.mobileWidth.matches == false
                         && GW.isMobile() == false));

/***********************************************************/
/*  Activate mode-based dynamic graphical dropcap swapping.
 */
addContentInjectHandler(GW.contentInjectHandlers.activateDynamicGraphicalDropcaps = (eventInfo) => {
    GWLog("activateDynamicGraphicalDropcaps", "rewrite.js", 1);

    processContainerNowAndAfterBlockLayout(eventInfo.container, (container) => {
        container.querySelectorAll(GW.dropcaps.dropcapBlockSelector).forEach(dropcapBlock => {
            //  Determine dropcap type.
            let dropcapType = dropcapTypeOf(dropcapBlock);

            //  Is this a recognized graphical dropcap type?
            if (GW.dropcaps.graphicalDropcapTypes.includes(dropcapType) == false)
                return;

            //  Get the dropcap image element.
            let dropcapImage = dropcapBlock.querySelector("img.dropcap");
            if (dropcapImage == null)
                return;

            //  Get the initial letter.
            let initialLetter = dropcapBlock.querySelector(".hidden-initial-letter")?.textContent;
            if (initialLetter == null)
                return;

            //  If the handler already exists, do nothing.
            if (dropcapImage.modeChangeHandler)
                return;

            //  Add event handler to switch image when mode changes.
            GW.notificationCenter.addHandlerForEvent(dropcapImage.modeChangeHandler = "DarkMode.computedModeDidChange", (info) => {
                //  Clear disabled flag, if any.
                dropcapBlock.classList.remove("disable-dropcap");

                //  Get new dropcap URL.
                let dropcapURL = getDropcapURL(dropcapType, initialLetter);
                if (dropcapURL == null) {
                    //  If no available dropcap image, set disabled flag.
                    dropcapBlock.classList.add("disable-dropcap");
                    return;
                }

                //  Update image URL.
                dropcapImage.src = dropcapURL.pathname + dropcapURL.search;

                //  Update inversion.
                dropcapImage.classList.toggle("invert", dropcapURL.pathname.includes("-" + (DarkMode.computedMode() == "light" ? "dark" : "light")));

                //  Update image file format class.
                dropcapImage.classList.remove("png", "svg");
                dropcapImage.classList.add(dropcapURL.pathname.slice(-3));
            });
        });
    });
}, "eventListeners", (info) => (   info.document == document
                                && GW.mediaQueries.mobileWidth.matches == false
                                && GW.isMobile() == false));

/*********************/
/*  Linkify dropcaps.
 */
addContentInjectHandler(GW.contentInjectHandlers.linkifyDropcaps = (eventInfo) => {
    GWLog("linkifyDropcaps", "rewrite.js", 1);

    processContainerNowAndAfterBlockLayout(eventInfo.container, (container) => {
        container.querySelectorAll(GW.dropcaps.dropcapBlockSelector).forEach(dropcapBlock => {
            //  If this dropcap has already been linkified, do nothing.
            if (dropcapBlock.querySelector(".link-dropcap"))
                return;

            //  Determine dropcap type.
            let dropcapType = dropcapTypeOf(dropcapBlock);

            //  Determine initial letter.
            let initialLetter = (   dropcapBlock.querySelector("span.dropcap")
                                 ?? dropcapBlock.querySelector(".hidden-initial-letter")).textContent;

            //  Get the dropcap (textual or graphical).
            let dropcap = dropcapBlock.querySelector(".dropcap");

            //  Wrap the dropcap (textual or graphical) in a link.
            let dropcapLink = newElement("A", {
                class: "link-page link-dropcap",
                href: "/dropcap#" + dropcapType,
                "data-letter": initialLetter,
                "data-dropcap-type": dropcapType
            });
            let dropcapLinkWrapper = newElement("SPAN");
            dropcapLinkWrapper.append(dropcapLink);
            dropcapLink.append(dropcap);

            //  Locate insertion point.
            let firstNode = firstTextNodeOfGraf(dropcapBlock);
            let firstNodeParent = firstNode.parentElement;
            if (firstNodeParent.matches(".initial-preceding-punctuation")) {
                firstNode = firstNodeParent.nextSibling;
                firstNodeParent = firstNodeParent.parentElement;
            } else if (firstNodeParent.matches(".hidden-initial-letter")) {
                firstNode = firstNodeParent;
                firstNodeParent = firstNodeParent.parentElement;
            }

            //  Inject the link-wrapped dropcap back into the block.
            firstNodeParent.insertBefore(dropcapLinkWrapper, firstNode);

            //  Process the link to enable extract pop-frames.
            Extracts.addTargetsWithin(dropcapLinkWrapper);

            //  Unwrap temporary wrapper.
            unwrap(dropcapLinkWrapper);
        });
    });
}, "rewrite", (info) => (   info.document == document
                         && GW.mediaQueries.mobileWidth.matches == false
                         && GW.isMobile() == false));

/***********************************************************************/
/*  Prevent blocks with dropcaps from overlapping the block below them.
 */
addContentInjectHandler(GW.contentInjectHandlers.preventDropcapsOverlap = (eventInfo) => {
    GWLog("preventDropcapsOverlap", "rewrite.js", 1);

    let blocksNotToBeOverlappedSelector = [
        "p[class*='dropcap-']",
        "section",
        "blockquote",
        ".collapse",
        ".list-heading",
        ".in-list",
        "div.sourceCode"
    ].join(", ");

    processContainerNowAndAfterBlockLayout(eventInfo.container, (container) => {
        container.querySelectorAll("p[class*='dropcap-']:not(.dropcap-not)").forEach(dropcapBlock => {
            let nextBlock = nextBlockOf(dropcapBlock);
            if (   nextBlock == null
                || nextBlock.matches(blocksNotToBeOverlappedSelector))
                dropcapBlock.classList.add("overlap-not");
        });
    });
}, ">rewrite", (info) => (   info.document == document
                          && GW.mediaQueries.mobileWidth.matches == false
                          && GW.isMobile() == false));


/********/
/* MATH */
/********/

/**************************************/
/*  Unwrap <p> wrappers of math blocks.
 */
addContentLoadHandler(GW.contentLoadHandlers.unwrapMathBlocks = (eventInfo) => {
    GWLog("unwrapMathBlocks", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".mjpage__block").forEach(mathBlock => {
        mathBlock = mathBlock.closest(".math");
        mathBlock.classList.add("block");

        if (   mathBlock.parentElement?.matches("p")
            && isOnlyChild(mathBlock))
            unwrap(mathBlock.parentElement);
    });
}, "rewrite");

/*****************************************************************************/
/*  Makes it so that copying a rendered equation or other math element copies
    the LaTeX source, instead of the useless gibberish that is the contents of
    the text nodes of the HTML representation of the equation.
 */
addCopyProcessor((event, selection) => {
    if (event.target.closest(".mjx-math")) {
        selection.replaceChildren(event.target.closest(".mjx-math").getAttribute("aria-label"));

        return false;
    }

    selection.querySelectorAll(".mjx-chtml").forEach(mathElement => {
        mathElement.innerHTML = " " + mathElement.querySelector(".mjx-math").getAttribute("aria-label") + " ";
    });

    return true;
});

/*****************************************************************************/
/*  Make copying text from Wikipedia articles with math elements properly copy
    the LaTeX source of the math fallback images, rather than omitting them.
 */
addCopyProcessor((event, selection) => {
    selection.querySelectorAll(".wikipedia-math-wrapper img").forEach(mathImage => {
        let mathText = mathImage.alt.slice(1, -1).replace("\\displaystyle", "");

        let mathWrapper = mathImage.closest(".wikipedia-math-wrapper");
        if (   mathWrapper.previousSibling
            && mathWrapper.previousSibling.textContent.endsWith(" "))
            mathText = mathText.trim();

        mathWrapper.replaceChildren(document.createTextNode(mathText));
    });

    return true;
});

/******************************************************************************/
/*  Makes double-clicking on a math element select the entire math element.
    (This actually makes no difference to the behavior of the copy listener
     [see the `addCopyProcessor` call above], which copies the entire LaTeX
     source of the full equation no matter how much of said equation is selected
     when the copy command is sent; however, it ensures that the UI communicates
     the actual behavior in a more accurate and understandable way.)
 */
addContentInjectHandler(GW.contentInjectHandlers.addDoubleClickListenersToMathBlocks = (eventInfo) => {
    GWLog("addDoubleClickListenersToMathBlocks", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".mjpage").forEach(mathElement => {
        mathElement.addEventListener("dblclick", (event) => {
            document.getSelection().selectAllChildren(mathElement.querySelector(".mjx-chtml"));
        });
        mathElement.title = mathElement.classList.contains("mjpage__block")
                            ? "Double-click to select equation, then copy, to get LaTeX source (or, just click the Copy button in the top-right of the equation area)"
                            : "Double-click to select equation; copy to get LaTeX source";
    	mathElement.title += ": " + mathElement.querySelector(".mjx-math").getAttribute("aria-label");
    });
}, "eventListeners");

/****************************************************************/
/*  Add block buttons (copy) to block (not inline) math elements.
 */
addContentLoadHandler(GW.contentLoadHandlers.addBlockButtonsToMathBlocks = (eventInfo) => {
    GWLog("addBlockButtonsToMathBlocks", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".math.block").forEach(mathBlock => {
        //  Inject button bar.
        mathBlock.appendChild(newElement("SPAN", { class: "block-button-bar" })).append(
            newElement("BUTTON", {
                type: "button",
                class: "copy",
                tabindex: "-1",
                title: (  "Copy LaTeX source of this equation to clipboard"
                		+ ": " 
                		+ mathBlock.querySelector(".mjx-math").getAttribute("aria-label"))
            }, {
                innerHTML: GW.svg("copy-regular")
            }),
            newElement("SPAN", {
                class: "scratchpad"
            })
        );
    });
}, "rewrite");

/************************************************/
/*  Activate copy buttons of math block elements.
 */
addContentInjectHandler(GW.contentInjectHandlers.activateMathBlockButtons = (eventInfo) => {
    GWLog("activateMathBlockButtons", "rewrite.js", 1);

    eventInfo.container.querySelectorAll(".math.block").forEach(mathBlock => {
        //  LaTeX source.
        let latexSource = mathBlock.querySelector(".mjx-math").getAttribute("aria-label");

        //  Copy button (copies LaTeX source).
        mathBlock.querySelector("button.copy").addActivateEvent((event) => {
            GWLog("mathBlockCopyButtonClicked", "rewrite.js", 3);

            copyTextToClipboard(latexSource);

            //  Flash math block, for visual feedback of copy operation.
            let innerMathBlock = mathBlock.querySelector(".MJXc-display");
            innerMathBlock.classList.add("flash");
            setTimeout(() => { innerMathBlock.classList.remove("flash"); }, 150);
        });
    });
}, "eventListeners");


/**********************************/
/* BROKEN HTML STRUCTURE CHECKING */
/**********************************/

/*  Check for #footnotes outside of #markdownBody, which indicates a prematurely
    closed div#markdownBody (probably due to some error in the page source).
 */
doWhenPageLoaded(() => {
    let footnotesSection = document.querySelector("#footnotes");
    if (   footnotesSection
        && footnotesSection.closest("#markdownBody") == null)
        GWServerLogError(location.href + "--broken-html-structure");
});


/**************************/
/* BROKEN ANCHOR CHECKING */
/**************************/
/*  If a reader loads a page and the anchor ID/hash does not exist inside the page,
    fire off a request to the 404 page, whose logs are reviewed manually,
    with the offending page+anchor ID, for correction (either fixing an outdated
    link somewhere on gwern.net, or adding a span/div manually to the page to
    make old inbound links go where they ought to).

    Such broken anchors can reflect out of date cross-page references, or reflect
    incoming URLs from elsewhere on the Internet which are broken/outdated.
    (Within-page anchor links are checked statically at compile-time, and those
     errors should never exist.)
 */

function reportBrokenAnchorLink(link) {
    GWLog("reportBrokenAnchorLink", "rewrite.js", 1);

    if (link.hash == "")
        return;

    GWServerLogError(fixedEncodeURIComponent(link.pathname) + "--" + fixedEncodeURIComponent(link.hash.substr(1)), "broken hash-anchor");
}

/*  Check for broken anchor (location hash not pointing to any element on the
    page) both at page load time and whenever the hash changes.
 */
GW.notificationCenter.addHandlerForEvent("GW.hashHandlingSetupDidComplete", GW.brokenAnchorCheck = (eventInfo) => {
    GWLog("GW.brokenAnchorCheck", "rewrite.js", 1);

    if (   location.hash > ""
        && /^#if_slide/.test(location.hash) == false
        && /^#:~:/.test(location.hash) == false
        && document.querySelector(selectorFromHash(location.hash)) == null)
        reportBrokenAnchorLink(location);
}, { once: true });
GW.notificationCenter.addHandlerForEvent("GW.hashDidChange", GW.brokenAnchorCheck);


/************/
/* PRINTING */
/************/

/*********************************************************************/
/*  Trigger transcludes and expand-lock collapse blocks when printing.
 */
window.addEventListener("beforeprint", GW.beforePrintHandler = (event) => {
    GWLog("Print command received.", "rewrite.js", 1);

    function expand(container) {
        Transclude.allIncludeLinksInContainer(container).forEach(includeLink => {
            if (includeLink.closest("#link-bibliography, .link-bibliography-append"))
                return;

            Transclude.transclude(includeLink, true);
        });

        container.querySelectorAll(".collapse").forEach(expandLockCollapseBlock);
    }

    GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", GW.expandAllContentWhenLoadingPrintView = (eventInfo) => {
        expand(eventInfo.container);
    }, {
        condition: (info) => (info.document == document)
    });

    expand(document);
});
window.addEventListener("afterprint", GW.afterPrintHandler = (event) => {
    GWLog("Print command completed.", "rewrite.js", 1);

    GW.notificationCenter.removeHandlerForEvent("GW.contentDidInject", GW.expandAllContentWhenLoadingPrintView);
});


/*****************************************************************************************/
/*! instant.page v5.1.0 - (C) 2019-2020 Alexandre Dieulot - https://instant.page/license */
/* Settings: 'prefetch' (loads HTML of target) after 1600ms hover (desktop) or mouse-down-click (mobile); TODO: left in logging for testing during experiment */
let pls="a:not(.has-content)";let t,e;const n=new Set,o=document.createElement("link"),z=o.relList&&o.relList.supports&&o.relList.supports("prefetch")&&window.IntersectionObserver&&"isIntersecting"in IntersectionObserverEntry.prototype,s="instantAllowQueryString"in document.body.dataset,a=true,r="instantWhitelist"in document.body.dataset,c="instantMousedownShortcut"in document.body.dataset,d=1111;let l=1600,u=!1,f=!1,m=!1;if("instantIntensity"in document.body.dataset){const t=document.body.dataset.instantIntensity;if("mousedown"==t.substr(0,"mousedown".length))u=!0,"mousedown-only"==t&&(f=!0);else if("viewport"==t.substr(0,"viewport".length))navigator.connection&&(navigator.connection.saveData||navigator.connection.effectiveType&&navigator.connection.effectiveType.includes("2g"))||("viewport"==t?document.documentElement.clientWidth*document.documentElement.clientHeight<45e4&&(m=!0):"viewport-all"==t&&(m=!0));else{const e=parseInt(t);isNaN(e)||(l=e)}}if(z){const n={capture:!0,passive:!0};if(f||document.addEventListener("touchstart",function(t){e=performance.now();const n=t.target.closest(pls);if(!h(n))return;v(n.href)},n),u?c||document.addEventListener("mousedown",function(t){const e=t.target.closest(pls);if(!h(e))return;v(e.href)},n):document.addEventListener("mouseover",function(n){if(performance.now()-e<d)return;const o=n.target.closest(pls);if(!h(o))return;o.addEventListener("mouseout",p,{passive:!0}),t=setTimeout(()=>{v(o.href),t=void 0},l)},n),c&&document.addEventListener("mousedown",function(t){if(performance.now()-e<d)return;const n=t.target.closest("a");if(t.which>1||t.metaKey||t.ctrlKey)return;if(!n)return;n.addEventListener("click",function(t){1337!=t.detail&&t.preventDefault()},{capture:!0,passive:!1,once:!0});const o=new MouseEvent("click",{view:window,bubbles:!0,cancelable:!1,detail:1337});n.dispatchEvent(o)},n),m){let t;(t=window.requestIdleCallback?t=>{requestIdleCallback(t,{timeout:1500})}:t=>{t()})(()=>{const t=new IntersectionObserver(e=>{e.forEach(e=>{if(e.isIntersecting){const n=e.target;t.unobserve(n),v(n.href)}})});document.querySelectorAll("a").forEach(e=>{h(e)&&t.observe(e)})})}}function p(e){e.relatedTarget&&e.target.closest("a")==e.relatedTarget.closest("a")||t&&(clearTimeout(t),t=void 0)}function h(t){if(t&&t.href&&(!r||"instant"in t.dataset)&&(a||t.origin==location.origin||"instant"in t.dataset)&&["http:","https:"].includes(t.protocol)&&("http:"!=t.protocol||"https:"!=location.protocol)&&(s||!t.search||"instant"in t.dataset)&&!(t.hash&&t.pathname+t.search==location.pathname+location.search||"noInstant"in t.dataset))return!0}function v(t){if(n.has(t))return;const e=document.createElement("link");console.log("Prefetched: "+t);e.rel="prefetch",e.href=t,document.head.appendChild(e),n.add(t)};
