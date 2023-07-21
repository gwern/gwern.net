Annotations = {
	annotatedLinkFullClass: "link-annotated",
	annotatedLinkPartialClass: "link-annotated-partial"
};

Annotations = { ...Annotations,
    /***********/
    /*  General.
        */

	isAnnotatedLink: (link) => {
		return link.classList.containsAnyOf([ Annotations.annotatedLinkFullClass,  Annotations.annotatedLinkPartialClass ]);
	},

	isAnnotatedLinkFull: (link) => {
		return link.classList.contains(Annotations.annotatedLinkFullClass);
	},

	isAnnotatedLinkPartial: (link) => {
		return link.classList.contains(Annotations.annotatedLinkPartialClass);
	},

    allAnnotatedLinksInContainer: (container) => {
        return Array.from(container.querySelectorAll("a[class*='link-annotated']")).filter(link => Annotations.isAnnotatedLink(link));
    },

    /*  Returns the target identifier: the original URL (for locally archived
        pages), or the relative url (for local links), or the full URL (for
        foreign links).

        Used for loading annotations, and caching reference data.
     */
	targetIdentifier: (target) => {
        if (target.dataset.urlOriginal) {
            return originalURLForLink(target).href;
        } else {
            return (target.hostname == location.hostname
                   ? target.pathname + target.hash
                   : target.href);
        }
	},

	/***************************/
	/*	Caching (API responses).
	 */

	//	Convenience method.
	cachedDocumentForLink: (link) => {
		let cachedAPIResponse = Annotations.cachedAPIResponseForLink(link);

		if (   cachedAPIResponse
			&& cachedAPIResponse != "LOADING_FAILED"
			&& cachedAPIResponse instanceof DocumentFragment)
			return cachedAPIResponse;
	},

    /*  Returns true iff a cached API response exists for the given link.
        */
    //	Called by: Extracts.setUpAnnotationLoadEventsWithin (extracts-annotations.js)
    cachedDataExists: (link) => {
        let cachedAPIResponse = Annotations.cachedAPIResponseForLink(link);
        return (   cachedAPIResponse != null
        		&& cachedAPIResponse != "LOADING_FAILED");
    },

	//	Used by: Annotations.cachedAPIResponseForLink
	//	Used by: Annotations.cacheAPIResponseForLink
	cachedAPIResponses: { },

	responseCacheKeyForLink: (link) => {
		return Annotations.sourceURLForLink(link).href;
	},

	//	Called by: Annotations.load
	cachedAPIResponseForLink: (link) => {
		return Annotations.cachedAPIResponses[Annotations.responseCacheKeyForLink(link)];
	},

	//	Called by: Annotations.load
	cacheAPIResponseForLink: (response, link) => {
		Annotations.cachedAPIResponses[Annotations.responseCacheKeyForLink(link)] = response;
	},

	/****************************/
	/*	Caching (reference data).
	 */

    /*  Storage for retrieved and cached annotations.
        */
    cachedReferenceData: { },

	referenceDataCacheKeyForLink: (link) => {
		return Annotations.targetIdentifier(link);
	},

	cachedReferenceDataForLink: (link) => {
		return Annotations.cachedReferenceData[Annotations.referenceDataCacheKeyForLink(link)];
	},

	cacheReferenceDataForlink: (referenceData, link) => {
		Annotations.cachedReferenceData[Annotations.referenceDataCacheKeyForLink(link)] = referenceData;
	},

    /*  Returns cached annotation reference data for a given link, or else 
    	either “LOADING_FAILED” (if loading the annotation was attempted but 
    	failed) or null (if the annotation has not been loaded).
        */
    referenceDataForLink: (link) => {
    	let referenceData = Annotations.cachedReferenceDataForLink(link);
		if (   referenceData == null
			&& Annotations.cachedDataExists(link)) {
			/*	Perhaps we’ve got an API response cached, but we haven’t 
				actually constructed reference data from it yet. (Maybe because 
				the API response was acquired otherwise than by the usual load 
				process. Or because the API response is the same as that for a 
				different link, so we don’t need to load it again.)
			 */
			//	Get parsed API response.
			let cachedAPIResponse = Annotations.cachedAPIResponseForLink(link);

			//	Attempt to construct reference data from API response.
			referenceData = Annotations.referenceDataFromParsedAPIResponse(cachedAPIResponse, link) ?? "LOADING_FAILED";
			if (referenceData == "LOADING_FAILED")
				//	Send request to record failure in server logs.
				GWServerLogError(Annotations.sourceURLForLink(link).href + `--could-not-process`, "problematic annotation");

			//	Cache reference data (successfully constructed or not).
			Annotations.cacheReferenceDataForlink(referenceData, link);
		}

        return referenceData;
    },

	/***********/
	/*	Loading.
	 */

	//	Called by: Annotations.sourceURLForLink
	//	Called by: Annotations.processedAPIResponseForLink
	//	Called by: Annotations.referenceDataFromParsedAPIResponse
	dataSourceForLink: (link) => {
		for ([ sourceName, dataSource ] of Object.entries(Annotations.dataSources))
			if (   sourceName != "local"
				&& dataSource.matches(link))
				return dataSource;

		return Annotations.dataSources.local;
	},

	//	Called by: Annotations.load
	processedAPIResponseForLink: (response, link) => {
		return Annotations.dataSourceForLink(link).processAPIResponse(response);
	},

	/*	Returns the URL of the annotation resource for the given link.
	 */
	//	Called by: Annotations.load
	//	Called by: Annotations.cachedAPIResponseForLink
	//	Called by: Annotations.cacheAPIResponseForLink
	sourceURLForLink: (link) => {
		return Annotations.dataSourceForLink(link).sourceURLForLink(link);
	},

	//	Called by: extracts.annotationForTarget (extracts-annotations.js)
	waitForDataLoad: (link, loadHandler = null, loadFailHandler = null) => {
		if (Annotations.cachedAPIResponseForLink(link) == "LOADING_FAILED") {
            if (loadFailHandler)
            	loadFailHandler(link);

			return;
		} else if (Annotations.cachedAPIResponseForLink(link)) {
			if (Annotations.referenceDataForLink(link) == "LOADING_FAILED") {
				if (loadFailHandler)
					loadFailHandler(link);
			} else {
				if (loadHandler)
					loadHandler(link);
			}

			return;
		}

		let didLoadHandler = (info) => {
            if (loadHandler)
            	loadHandler(link);

			GW.notificationCenter.removeHandlerForEvent("Annotations.annotationLoadDidFail", loadDidFailHandler);
        };
        let loadDidFailHandler = (info) => {
            if (loadFailHandler)
            	loadFailHandler(link);

			GW.notificationCenter.removeHandlerForEvent("Annotations.annotationDidLoad", didLoadHandler);
        };
		let options = { 
        	once: true, 
        	condition: (info) => info.link == link
        };

        GW.notificationCenter.addHandlerForEvent("Annotations.annotationDidLoad", didLoadHandler, options);
        GW.notificationCenter.addHandlerForEvent("Annotations.annotationLoadDidFail", loadDidFailHandler, options);
	},

    /*  Load and process the annotation for the given link.
        */
    //	Called by: Extracts.setUpAnnotationLoadEventsWithin (extracts-annotations.js)
    load: (link, loadHandler = null, loadFailHandler = null) => {
        GWLog("Annotations.load", "annotations.js", 2);

		/*	Get URL of the annotation resource.
		 */
        let sourceURL = Annotations.sourceURLForLink(link);

		/*	Depending on the data source, `response` could be HTML,
			JSON, or other. We construct and cache a reference data object,
			then fire the appropriate event.
		 */
		let processResponse = (response) => {
			let referenceData = Annotations.referenceDataFromParsedAPIResponse(response, link);

			if (referenceData) {
				Annotations.cacheReferenceDataForlink(referenceData, link);

				GW.notificationCenter.fireEvent("Annotations.annotationDidLoad", { 
					link: link 
				});
			} else {
				Annotations.cacheReferenceDataForlink("LOADING_FAILED", link);

				GW.notificationCenter.fireEvent("Annotations.annotationLoadDidFail", { 
					link: link 
				});

				//	Send request to record failure in server logs.
				GWServerLogError(sourceURL.href + `--could-not-process`, "problematic annotation");
			}
		};

		/*	Retrieve, parse, and cache the annotation resource; or use an
			already-cached API response.
		 */
		let response = Annotations.cachedAPIResponseForLink(link);
		if (response) {
			processResponse(response);
		} else {
			doAjax({
				location: sourceURL.href,
				onSuccess: (event) => {
					let response = Annotations.processedAPIResponseForLink(event.target.responseText, link);

					Annotations.cacheAPIResponseForLink(response, link);

					processResponse(response);
				},
				onFailure: (event) => {
					Annotations.cacheAPIResponseForLink("LOADING_FAILED", link);
					Annotations.cacheReferenceDataForlink("LOADING_FAILED", link);

					GW.notificationCenter.fireEvent("Annotations.annotationLoadDidFail", { link: link });

					//	Send request to record failure in server logs.
					GWServerLogError(sourceURL.href, "missing annotation");
				}
			});
		}

		//	Call any provided handlers, if/when appropriate.
		if (loadHandler || loadFailHandler)
			Annotations.waitForDataLoad(link, loadHandler, loadFailHandler);
    },

	//	Called by: Annotations.load
	referenceDataFromParsedAPIResponse: (response, link) => {
		if (response == "LOADING_FAILED")
			return null;

		return Annotations.dataSourceForLink(link).referenceDataFromParsedAPIResponse(response, link);
	},

	/***************************/
	/* ANNOTATION DATA SOURCES */
	/***************************/
	/*	Note on annotation data sources:

		More data sources may be added. Any data source object must have these
		four properties, each a function with the given signature:

		.matches(URL|Element) => boolean
		.sourceURLForLink(URL|Element) => URL
		.processAPIResponse(string) => object
		.referenceDataFromParsedAPIResponse(object, URL|Element) => object

		(Most data source objects also have additional properties, functions,
		 etc., as necessary to implement the above functionality.)

		Examine implementation of these functions in .dataSources.local to
		understand their purpose.
	 */

	dataSources: {
		/********************************************************/
		/*	Annotations generated server-side and hosted locally.
		 */

		local: {
			/*	There could be a local annotation for any link. As this returns
				true for all links, it is the fallback data source in the event 
				that no other data sources match a link.
			 */
			matches: (link) => {
				return true;
			},

			//	Called by: Annotations.processedAPIResponseForLink
			//	Called by: Annotations.sourceURLForLink
			sourceURLForLink: (link) => {
				return new URL(  location.origin 
							   + Annotations.dataSources.local.basePathname
							   + fixedEncodeURIComponent(fixedEncodeURIComponent(Annotations.targetIdentifier(link)))
							   + ".html");
			},

			//	Called by: Annotations.processedAPIResponseForLink
			processAPIResponse: (response) => {
				let responseDoc = newDocument(response);

				//	Request the image, to cache it.
				let thumbnail = responseDoc.querySelector(".page-thumbnail");
				if (thumbnail)
					doAjax({ location: new URL(thumbnail.src) });

				return responseDoc;
			},

			//	Called by: Annotations.referenceDataFromParsedAPIResponse
			referenceDataFromParsedAPIResponse: (response, link = null) => {
				let referenceElement = response.querySelector(Annotations.dataSources.local.referenceElementSelector);

				let titleHTML = referenceElement.innerHTML;
				let titleText = referenceElement.textContent;
				let titleLinkHref = referenceElement.href;

				let titleLinkClass = "title-link";
				//  Import certain link classes.
				/*  Just ‘link-live’ for now, but the inclusion rule is: any class that
					is used to test whether a link is of a certain type - see e.g.
					Extracts.isForeignSiteLink() in extracts-content.js - for which link
					type there can be annotations (so not, e.g., ‘footnote-ref’, because
					there’s no such thing as an annotated footnote link). This way, the
					title-link of the popup will correctly test as the *un-annotated*
					link type of the original target.
					—SA 2022-06-13
				 */
				[ "link-live" ].forEach(targetClass => {
					if (referenceElement.classList.contains(targetClass))
						titleLinkClass += ` ${targetClass}`;
				});

				//	Link icon for the title link.
				let titleLinkIconMetadata;
				if (referenceElement.dataset.linkIcon) {
					titleLinkIconMetadata = `data-link-icon-type="${(referenceElement.dataset.linkIconType)}"`
										  + `data-link-icon="${(referenceElement.dataset.linkIcon)}"`;
				} else if (link && link.dataset.linkIcon) {
					titleLinkIconMetadata = `data-link-icon-type="${(link.dataset.linkIconType)}"`
										  + `data-link-icon="${(link.dataset.linkIcon)}"`;
				}

				//	Original URL.
				let originalURL = referenceElement.dataset.urlOriginal ?? null;
				let originalURLText = originalURL
									  ? (originalURL.includes("ar5iv") 
									     ? `<span class="smallcaps">HTML</span>` 
									     : "live")
									  : null;

				//  Author list.
				let authorElement = response.querySelector(".author");
				//	Generate comma-separated author list; truncate with “…” abbreviation for ‘et al’ @ > 3.
				let authorList;
				if (authorElement) {
					authorList = authorElement.innerHTML.split(", ").slice(0, 3).join(", ");
					if (authorList.length < authorElement.innerHTML.length)
						authorList += "…";
				}
				let author = authorElement 
							 ? `<span class="data-field author cite-author">${authorList}</span>` 
							 : null;

				//  Date.
				let dateElement = response.querySelector(".date");
				let date = dateElement 
						   ? (  `<span class="data-field cite-date" title="${dateElement.textContent}">` 
						      + dateElement.textContent.replace(/-[0-9][0-9]-[0-9][0-9]$/, "") 
						      + `</span>`) 
						   : null;

				// Link Tags
				let tagsElement = response.querySelector(".link-tags");
				let tags = tagsElement
						   ? `<span class="data-field link-tags">${tagsElement.innerHTML}</span>`
						   : null;

				//	The backlinks link (if exists).
				let backlinksElement = response.querySelector(".backlinks");
				let backlinks = backlinksElement
								? `<span 
									class="data-field aux-links backlinks" 
									>${backlinksElement.innerHTML}</span>`
								: null;

				//	The similar-links link (if exists).
				let similarsElement = response.querySelector(".similars");
				let similars = similarsElement
							   ? `<span 
							       class="data-field aux-links similars"
							       >${similarsElement.innerHTML}</span>`
							   : null;

                //	The link-link-bibliography link (if exists).
				let linkbibElement = response.querySelector(".link-bibliography");
				let linkbib = linkbibElement
							  ? `<span 
							  	  class="data-field aux-links link-bibliography"
							  	  >${linkbibElement.innerHTML}</span>`
							  : null;

				//	All the aux-links (tags, backlinks, similars, link link-bib).
				let auxLinks = ([ tags, backlinks, similars, linkbib ].filter(x => x).join("; ") || null);
				if (auxLinks)
					auxLinks = ` (${auxLinks})`;

				//  Combined author, date, & aux-links.
				let authorDateAux = ([ author, date, auxLinks ].filter(x => x).join("") || null);

				//	Abstract (if exists).
				let abstractElement = response.querySelector("blockquote");
				let abstractHTML = null;
				if (abstractElement) {
					let abstractDocument = newDocument(abstractElement.childNodes);
					Annotations.dataSources.local.postProcessReferenceEntry(abstractDocument, link);
					abstractHTML = abstractDocument.innerHTML;
				}

				//	Pop-frame title text.
				let popFrameTitle = referenceElement.cloneNode(true);
				//	Trim quotes.
				let [ first, last ] = [ popFrameTitle.firstTextNode, popFrameTitle.lastTextNode ];
				if (   /^['"‘“]/.test(first.textContent) == true
					&& /['"’”]$/.test(last.textContent)  == true) {
					first.textContent = first.textContent.slice(1);
					last.textContent = last.textContent.slice(0, -1);
				}
				let popFrameTitleText = popFrameTitle.innerHTML;

				return {
					content: {
						originalURL:            originalURL,
						originalURLText:        originalURLText,
						titleHTML:              titleHTML,
						fullTitleHTML:          titleHTML,
						titleText:              titleText,
						titleLinkHref:          titleLinkHref,
						titleLinkClass:         titleLinkClass,
						titleLinkIconMetadata:  titleLinkIconMetadata,
						author:                 author,
						date:                   date,
						auxLinks:               auxLinks,
						authorDateAux:          authorDateAux,
						abstract:               abstractHTML,
					},
					template:                       "annotation-blockquote-inside",
					linkTarget:                     (GW.isMobile() ? "_self" : "_blank"),
					whichTab:                       (GW.isMobile() ? "current" : "new"),
					tabOrWindow:                    (GW.isMobile() ? "tab" : "window"),
					popFrameTitleText:              popFrameTitleText,
					popFrameTitleLinkHref:          titleLinkHref,
					popFrameTitleOriginalLinkHref:  originalURL
				};
			},

			/*  Post-process an already-constructed local annotation 
				(do HTML cleanup, etc.).
				*/
			//	Called by: Annotations.dataSources.local.referenceDataFromParsedAPIResponse
			postProcessReferenceEntry: (referenceEntry, link = null) => {
				//	Unwrap extraneous <div>s, if present.
				if (   referenceEntry.firstElementChild == referenceEntry.lastElementChild
					&& referenceEntry.firstElementChild.tagName == "DIV")
					unwrap(referenceEntry.firstElementChild);

				//	Unwrap more extraneous <div>s, if present.
				let pageDescriptionClass = "page-description-annotation";
				let pageDescription = referenceEntry.querySelector(`div.${pageDescriptionClass}`);
				if (pageDescription)
					unwrap(pageDescription, {
						moveClasses: true,
						classesToMove: [ pageDescriptionClass ]
					});
			},

			basePathname: "/metadata/annotation/",
			referenceElementSelector: [ Annotations.annotatedLinkFullClass,  Annotations.annotatedLinkPartialClass ].map(className => `a.${className}`).join(", ")
		}
	}
};

/**************************************************/
/*  Wikipedia entries (page summaries or sections).
	*/
Annotations.dataSources.wikipedia = {
	/*	The Wikipedia API only gives usable responses for most, not all,
		Wikipedia URLs.
	 */
	matches: (link) => {
		return (   /(.+?)\.wikipedia\.org/.test(link.hostname)
				&& link.pathname.startsWith("/wiki/")
				&& link.pathname.startsWithAnyOf(_π("/wiki/", [ "File:", "Category:", "Special:" ])) == false);
	},

	//	Called by: Annotations.processedAPIResponseForLink
	//	Called by: Annotations.sourceURLForLink
	sourceURLForLink: (link) => {
		annotationURL = new URL(link.href);

		let wikiPageName = fixedEncodeURIComponent(/\/wiki\/(.+?)$/.exec(decodeURIComponent(annotationURL.pathname))[1]);
		annotationURL.pathname = `/api/rest_v1/page/html/${wikiPageName}`;
		annotationURL.hash = "";

		return annotationURL;
	},

	//	Called by: Annotations.processedAPIResponseForLink
	processAPIResponse: (response) => {
		return newDocument(response);
	},

	//	Called by: Annotations.referenceDataFromParsedAPIResponse
	referenceDataFromParsedAPIResponse: (response, articleLink) => {
		let titleLinkHref = articleLink.href;

		let responseHTML, titleHTML, fullTitleHTML, secondaryTitleLinksHTML;
		if (articleLink.hash > "") {
			let targetHeading = response.querySelector(selectorFromHash(articleLink.hash));

			/*	Check whether we have tried to load a page section which does
				not exist on the requested wiki page.
			 */
			if (!targetHeading)
				return null;

			//	The id is on the heading, so the section is its parent.
			let targetSection = targetHeading.parentElement.cloneNode(true);

			//	Excise heading.
			targetHeading = targetSection.firstElementChild;
			targetHeading.remove();

			//	Content sans heading.
			responseHTML = targetSection.innerHTML;

			//	Unwrap or delete links, but save them for inclusion in the template.
			secondaryTitleLinksHTML = "";
			//	First link is the section title itself.
			targetHeading.querySelectorAll("a:first-of-type").forEach(link => {
				//  Process link, save HTML, unwrap.
				Annotations.dataSources.wikipedia.qualifyWikipediaLink(link, articleLink);
				Annotations.dataSources.wikipedia.designateWikiLink(link);
				secondaryTitleLinksHTML += link.outerHTML;
				unwrap(link);
			});
			//	Additional links are other things, who knows what.
			targetHeading.querySelectorAll("a").forEach(link => {
				//  Process link, save HTML, delete.
				Annotations.dataSources.wikipedia.qualifyWikipediaLink(link, articleLink);
				Annotations.dataSources.wikipedia.designateWikiLink(link);
				secondaryTitleLinksHTML += link.outerHTML;
				link.remove();
			});
			if (secondaryTitleLinksHTML > "")
				secondaryTitleLinksHTML = ` (${secondaryTitleLinksHTML})`;

			//	Cleaned section title.
			titleHTML = targetHeading.innerHTML;
			fullTitleHTML = `${titleHTML} (${(response.querySelector("title").innerHTML)})`;
		} else {
			responseHTML = response.querySelector("[data-mw-section-id='0']").innerHTML;
			titleHTML = unescapeHTML(response.querySelector("title").innerHTML);
			fullTitleHTML = titleHTML;

			//	Build TOC.
			let sections = Array.from(response.querySelectorAll("section")).slice(1);
			if (   sections 
				&& sections.length > 0) {
				responseHTML += `<div class="TOC columns"><ul>`;
				let headingLevel = 2;
				for (let i = 0; i < sections.length; i++) {
					let section = sections[i];
					let headingElement = section.firstElementChild;
					let newHeadingLevel = parseInt(headingElement.tagName.slice(1));
					if (newHeadingLevel > headingLevel)
						responseHTML += `<ul>`;

					if (   i > 0 
						&& newHeadingLevel <= headingLevel)
						responseHTML += `</li>`;

					if (newHeadingLevel < headingLevel)
						responseHTML += `</ul>`;

					//	We must encode, because the anchor might contain quotes.
					let urlEncodedAnchor = fixedEncodeURIComponent(headingElement.id);

					//	Get heading, parse as HTML, and unwrap links.
					let heading = headingElement.cloneNode(true);
					heading.querySelectorAll("a").forEach(link => { unwrap(link); });

					//	Construct TOC entry.
					responseHTML += `<li><a href='${articleLink}#${urlEncodedAnchor}'>${(heading.innerHTML)}</a>`;

					headingLevel = newHeadingLevel;
				}
				responseHTML += `</li></ul></div>`;
			}
		}

		let referenceEntry = newDocument(responseHTML);
		Annotations.dataSources.wikipedia.postProcessReferenceEntry(referenceEntry, articleLink);
		let abstractHTML = referenceEntry.innerHTML;

		let titleText = newElement("SPAN", null, { innerHTML: titleHTML }).textContent;

		//	Pop-frame title text. Mark sections with ‘§’ symbol.
		let popFrameTitleHTML = (articleLink.hash > ""
								 ? `${(response.querySelector("title").innerHTML)} &#x00a7; ${titleHTML}`
								 : titleHTML);
		let popFrameTitleText = newElement("SPAN", null, { innerHTML: popFrameTitleHTML }).textContent;

		return {
			content: {
				titleHTML:                titleHTML,
				fullTitleHTML:            fullTitleHTML,
				secondaryTitleLinksHTML:  (secondaryTitleLinksHTML ?? null),
				titleText:                titleText,
				titleLinkHref:            titleLinkHref,
				titleLinkClass:           `title-link link-live`,
				titleLinkIconMetadata:    `data-link-icon-type="svg" data-link-icon="wikipedia"`,
				abstract: 		          abstractHTML,
				dataSourceClass:          "wikipedia-entry",
			},
			template:               "annotation-blockquote-inside",
			linkTarget:             (GW.isMobile() ? "_self" : "_blank"),
			whichTab:               (GW.isMobile() ? "current" : "new"),
			tabOrWindow:            (GW.isMobile() ? "tab" : "window"),
			popFrameTitleText:      popFrameTitleText,
			popFrameTitleLinkHref:  titleLinkHref
		};
	},

	/*	Qualify a link in a Wikipedia article.
	 */
	qualifyWikipediaLink: (link, hostArticleLink) => {
		if (link.getAttribute("href") == null)
			return;

		//  Qualify link.
		if (link.getAttribute("rel") == "mw:WikiLink")
			link.pathname = "/wiki" + link.getAttribute("href").slice(1);
		if (link.getAttribute("href").startsWith("#"))
			link.pathname = hostArticleLink.pathname;
		if (link.hostname == location.hostname)
			link.hostname = hostArticleLink.hostname;
	},

	/*	Mark a wiki-link appropriately, as annotated, or live, or neither.
	 */
	designateWikiLink: (link) => {
		if (/(.+?)\.wikipedia\.org/.test(link.hostname)) {
			if (Annotations.dataSources.wikipedia.matches(link)) {
				link.classList.add(Annotations.annotatedLinkFullClass);
			} else {
				if (!(   link.pathname.startsWithAnyOf(_π("/wiki/", [ "Special:" ]))
					  || link.pathname == "/w/index.php"))
					link.classList.add("link-live");
			}
		}
	},

	/*  Elements to excise from a Wikipedia entry.
		*/
	//	Used in: Annotations.dataSources.wikipedia.postProcessReferenceEntry
	extraneousElementSelectors: [
		"style",
		".mw-ref",
		".shortdescription",
		"td hr",
		".hatnote",
		".portal",
		".penicon",
		".reference",
		".Template-Fact",
		".error",
		".mwe-math-mathml-inline",
        ".sidebar",
        ".ambox",
		".unicode.haudio"
	],

	/*  CSS properties to preserve when stripping inline styles.
		*/
	//	Used in: Annotations.dataSources.wikipedia.postProcessReferenceEntry
	preservedInlineStyleProperties: [
		"display",
		"position",
		"top",
		"left",
		"bottom",
		"right",
		"width",
		"height",
		"word-break"
	],

	/*  Post-process an already-constructed annotation created from a Wikipedia
		entry (do HTML cleanup, etc.).
		*/
	//	Called by: Annotations.dataSources.wikipedia.referenceDataFromParsedAPIResponse
	postProcessReferenceEntry: (referenceEntry, articleLink) => {
		//  Remove unwanted elements.
		referenceEntry.querySelectorAll(Annotations.dataSources.wikipedia.extraneousElementSelectors.join(", ")).forEach(element => {
			element.remove();
		});

		//  Remove location maps (they don’t work right).
		referenceEntry.querySelectorAll(".locmap").forEach(locmap => {
			(locmap.closest("tr") ?? locmap).remove();
		});

		//	Remove other maps.
		referenceEntry.querySelectorAll("img").forEach(image => {
			let imageSourceURL = new URL(image.src);
			if (imageSourceURL.hostname == "maps.wikimedia.org")
				image.remove();
		});

		//  Remove empty paragraphs.
		referenceEntry.querySelectorAll("p:empty").forEach(emptyGraf => {
			emptyGraf.remove();
		});

		//	Remove edit-links.
		referenceEntry.querySelectorAll("a[title^='Edit this on Wiki'], a[title^='Edit this at Wiki']").forEach(editLink => {
			editLink.remove();
		});

		//  Process links.
		referenceEntry.querySelectorAll("a").forEach(link => {
			//	De-linkify non-anchor self-links.
			if (   link.hash     == ""
				&& link.pathname == articleLink.pathname) {
				unwrap(link);
				return;
			}

			//  Qualify links.
			Annotations.dataSources.wikipedia.qualifyWikipediaLink(link, articleLink);

			//  Mark other Wikipedia links as also being annotated.
			Annotations.dataSources.wikipedia.designateWikiLink(link);

			//  Mark self-links (anchorlinks within the same article).
			if (link.pathname == articleLink.pathname)
				link.classList.add("link-self");
		});

		//	Strip inline styles and some related attributes.
		let tableElementsSelector = "table, thead, tfoot, tbody, tr, th, td";
		referenceEntry.querySelectorAll("[style]").forEach(styledElement => {
			//	Skip table elements; we handle those specially.
			if (styledElement.matches(tableElementsSelector))
				return;

			if (styledElement.style.display != "none")
				stripStyles(styledElement, null, Annotations.dataSources.wikipedia.preservedInlineStyleProperties);
		});
		//	Special handling for table elements.
		referenceEntry.querySelectorAll(tableElementsSelector).forEach(tableElement => {
			if (tableElement.style.display != "none") {
				if (tableElement.style.position == "relative")
					stripStyles(tableElement, null, [ "text-align", "position", "width", "height" ]);
				else
					stripStyles(tableElement, null, [ "text-align" ]);
			}

			[ "width", "height", "align" ].forEach(attribute => {
				tableElement.removeAttribute(attribute);
			});
		});

		//  Rectify table classes.
		referenceEntry.querySelectorAll("table.sidebar").forEach(table => {
			table.classList.toggle("infobox", true);
		});

		//  Normalize table cell types.
		referenceEntry.querySelectorAll("th:not(:only-child)").forEach(cell => {
			let rowSpan = (cell.rowSpan > 1) ? ` rowspan="${cell.rowSpan}"` : ``;
			let colSpan = (cell.colSpan > 1) ? ` colspan="${cell.colSpan}"` : ``;
			cell.outerHTML = `<td${rowSpan}${colSpan}>${cell.innerHTML}</td>`;
		});

		//  Un-linkify images.
		referenceEntry.querySelectorAll("a img").forEach(imageLink => {
			imageLink.parentElement.outerHTML = imageLink.outerHTML;
		});

		//	Fix chemical formulas.
		referenceEntry.querySelectorAll(".chemf br").forEach(br => {
			br.remove();
		});

		//	Rectify quoteboxes.
		referenceEntry.querySelectorAll("div.quotebox").forEach(quotebox => {
			let blockquote = quotebox.querySelector("blockquote");
			blockquote.classList.add("quotebox");
			
			let title = quotebox.querySelector(".quotebox-title");
			if (title) {
				blockquote.insertBefore(title, blockquote.firstElementChild);
			}

			let cite = quotebox.querySelector("blockquote + p");
			if (cite) {
				blockquote.insertBefore(cite, null);
				cite.classList.add("quotebox-citation");
			}

			unwrap(quotebox);
		});

		//  Separate out the thumbnail and float it.
		let thumbnail = referenceEntry.querySelector("img");
		let thumbnailContainer;
		if (thumbnail)
			thumbnailContainer = thumbnail.closest(".infobox-image, .thumb");
		if (   thumbnail
			&& thumbnailContainer
			&& thumbnailContainer.closest(".gallery") == null) {
			while ([ "TR", "TD", "TH" ].includes(thumbnailContainer.tagName))
				thumbnailContainer = thumbnailContainer.parentElement;

			//	Save references to thumbnails’ parent elements.
			let thumbnailParents = [ ];

			//  Create the figure and move the thumbnail(s) into it.
			let figure = newElement("FIGURE", { "class": "float-right" });
			thumbnailContainer.querySelectorAll(".infobox-image img, .thumb img").forEach(image => {
				if (image.closest("figure") == figure)
					return;

				thumbnailParents.push(image.parentElement);

				let closestRow = image.parentElement;
				while (   closestRow != null
					   && !(   closestRow.tagName == "TR" 
					   		|| closestRow.style.display == "table-row")) {
					closestRow = closestRow.parentElement.closest("tr, [style*='display']");
				}
				if (closestRow == null)
					return;

				let allImagesInRow = closestRow.querySelectorAll("img");
				if (allImagesInRow.length > 1) {
					let rowWrapper = newElement("SPAN", { "class": "image-wrapper image-row-wrapper" });
					rowWrapper.append(...allImagesInRow);
					figure.append(rowWrapper);
				} else {
					figure.append(allImagesInRow[0]);
				}
			});

			//  Create the caption, if need be.
			let caption = referenceEntry.querySelector(".mw-default-size + div, .infobox-caption");
			if (   caption
				&& caption.textContent > "")
				figure.appendChild(newElement("FIGCAPTION", null, { "innerHTML": caption.innerHTML }));

			//  Insert the figure as the first child of the annotation.
			referenceEntry.insertBefore(figure, referenceEntry.firstElementChild);

			//  Rectify classes.
			thumbnailParents.first?.closest("table")?.classList.toggle("infobox", true);

			//  Remove the whole row where each thumbnail was.
			thumbnailParents.forEach(thumbnailParent => {
				thumbnailParent.closest("tr")?.remove();
			});
		} else if (   thumbnail
				   && thumbnail.closest("figure")) {
			let figure = thumbnail.closest("figure");

			//  Insert the figure as the first child of the annotation.
			referenceEntry.insertBefore(figure, referenceEntry.firstElementChild);
			figure.classList.add("float-right");

			let caption = figure.querySelector("figcaption");
			if (caption.textContent == "")
				caption.remove();
		}

		//	Rewrite other figures.
		referenceEntry.querySelectorAll("div.thumb").forEach(figureBlock => {
			let figure = newElement("FIGURE");
			figureBlock.querySelectorAll("img").forEach(image => {
				figure.appendChild(image);
			});
			figure.appendChild(newElement("FIGCAPTION", null, { "innerHTML": figureBlock.querySelector(".thumbcaption")?.innerHTML }));
			figureBlock.parentNode.insertBefore(figure, figureBlock);
			figureBlock.remove();
		});

		//	Mark certain images as not to be wrapped in figures.
		let noFigureImagesSelector = [
			".mwe-math-element",
			".mw-default-size",
			".sister-logo",
			".side-box-image",
			"p"
		].map(selector => `${selector} img`).join(", ");
		referenceEntry.querySelectorAll(noFigureImagesSelector).forEach(image => {
			image.classList.add("figure-not");
		});
	}
};

GW.notificationCenter.fireEvent("Annotations.didLoad");
