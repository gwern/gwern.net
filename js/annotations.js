Annotations = {
	basePathname: "/metadata/annotation/",

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

    /*  Returns the target identifier: the relative url (for local links), 
    	or the full URL (for foreign links).

        Used for loading annotations, and caching reference data.
     */
	targetIdentifier: (target) => {
		return (target.hostname == location.hostname
			   ? target.pathname + target.hash
			   : (target instanceof HTMLAnchorElement
			   	  ? target.getAttribute("href")
			   	  : target.href));
	},

	/***************************/
	/*	Caching (API responses).
	 */

	//	Convenience method.
	cachedDocumentForLink: (link) => {
		let cachedAPIResponse = Annotations.cachedAPIResponseForLink(link);
		if (   cachedAPIResponse
			&& cachedAPIResponse != "LOADING_FAILED"
			&& cachedAPIResponse instanceof DocumentFragment) {
			return cachedAPIResponse;
		} else {
			return null;
		}
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

	cacheReferenceDataForLink: (referenceData, link) => {
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
			Annotations.cacheReferenceDataForLink(referenceData, link);
		}

        return referenceData;
    },

	/***********/
	/*	Loading.
	 */

	/*	Returns the URL of the annotation resource for the given link.
	 */
	//	Called by: Annotations.load
	//	Called by: Annotations.cachedAPIResponseForLink
	//	Called by: Annotations.cacheAPIResponseForLink
	sourceURLForLink: (link) => {
		return URLFromString(  Annotations.basePathname
							 + fixedEncodeURIComponent(fixedEncodeURIComponent(Annotations.targetIdentifier(link)))
							 + ".html");
	},

	//	Called by: Annotations.load
	processedAPIResponseForLink: (response, link) => {
		let responseDoc = newDocument(response);

		//	Request the image, to cache it.
		let thumbnail = responseDoc.querySelector(".page-thumbnail");
		if (thumbnail)
			doAjax({ location: URLFromString(thumbnail.src) });

		return responseDoc;
	},

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

		/*	For local annotations, the (already processed) `response` is a 
			DocumentFragment. We construct and cache a reference data object, 
			then fire the appropriate event.
		 */
		let processResponse = (response) => {
			let referenceData = Annotations.referenceDataFromParsedAPIResponse(response, link);

			if (referenceData) {
				Annotations.cacheReferenceDataForLink(referenceData, link);

				GW.notificationCenter.fireEvent("Annotations.annotationDidLoad", { 
					link: link 
				});
			} else {
				Annotations.cacheReferenceDataForLink("LOADING_FAILED", link);

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
					Annotations.cacheReferenceDataForLink("LOADING_FAILED", link);

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

		let referenceElement = response.querySelector([ Annotations.annotatedLinkFullClass, 
														Annotations.annotatedLinkPartialClass 
														].map(className => `a.${className}`).join(", "));

		let titleHTML = referenceElement.innerHTML;
		let titleText = referenceElement.textContent;

		//	On mobile, use mobile-specific link href, if provided.
		let titleLinkHref = (   referenceElement.dataset.hrefMobile 
							 && GW.isMobile())
							? referenceElement.dataset.hrefMobile
							: referenceElement.href;

		//	Construct title link class.
		let titleLinkClass = "title-link";

		//  Import link classes (excluding certain ones).
		Array.from(referenceElement.classList).filter(referenceElementClass => [
			"link-annotated",
			"link-annotated-partial"
		].includes(referenceElementClass) == false).forEach(referenceElementClass => {
			titleLinkClass += ` ${referenceElementClass}`;
		});

		//	Special handling for links with separate ‘HTML’ URLs.
		if (   referenceElement.dataset.urlHtml
			&& titleLinkClass.includes("link-live") == false)
			titleLinkClass += " link-live";

		//	Special data attributes for the title link.
		let titleLinkDataAttributes = [ 
			"urlHtml", 
			"urlArchive",
			"imageWidth",
			"imageHeight",
			"aspectRatio"
		].map(attr => 
			referenceElement.dataset[attr] 
			? `data-${(attr.replace(/([a-z])([A-Z])/g, "$1-$2").toLowerCase())}="${referenceElement.dataset[attr]}"` 
			: null
		).filter(Boolean);

		//	Link icon for the title link.
		if (referenceElement.dataset.linkIcon) {
			titleLinkDataAttributes.push(`data-link-icon-type="${(referenceElement.dataset.linkIconType)}"`);
			titleLinkDataAttributes.push(`data-link-icon="${(referenceElement.dataset.linkIcon)}"`);
		} else if (   link 
				   && link.dataset.linkIcon) {
			titleLinkDataAttributes.push(`data-link-icon-type="${(link.dataset.linkIconType)}"`)
			titleLinkDataAttributes.push(`data-link-icon="${(link.dataset.linkIcon)}"`);
		}

		//	Stringify data attributes.
		titleLinkDataAttributes = (titleLinkDataAttributes.length > 0
								   ? titleLinkDataAttributes.join(" ")
								   : null);

		//  Author list.
		let authorHTML = null;
		let authorElement = response.querySelector(".author");
		if (authorElement) {
			let authorListClass = [ "data-field", ...(authorElement.classList) ].join(" ");
			authorHTML = `<span class="${authorListClass}">${authorElement.innerHTML}</span>`
		}

		//  Date.
		let dateHTML = null;
		let dateElement = response.querySelector(".date");
		if (dateElement) {
			let dateClass = [ "data-field", ...(dateElement.classList) ].join(" ");
			dateHTML = `<span class="${dateClass}" title="${dateElement.textContent}">` 
					 + dateElement.textContent.replace(/-[0-9][0-9]-[0-9][0-9]$/, "") 
					 + `</span>`;
		}

		//	Link tags.
		let tagsHTML = null;
		let tagsElement = response.querySelector(".link-tags");
		if (tagsElement) {
			let tagsListClass = [ "data-field", ...(tagsElement.classList) ].join(" ");
			tagsHTML = `<span class="${tagsListClass}">${tagsElement.innerHTML}</span>`;
		}

		//	Archive URL (if exists).
		let archiveLinkHref = referenceElement.dataset.urlArchive ?? null;
		let linkTarget = (GW.isMobile() ? "_self" : "_blank");
		let archiveLinkHTML = archiveLinkHref
							  ? `<span 
								  class="data-field archiveURL"
								  ><a
									title="Link to local archive for ${titleText}"
									href="${archiveLinkHref}"
									target="${linkTarget}"
									alt="Locally archived version of this URL"
									>archive</a></span>`
							  : null;

		//	The backlinks link (if exists).
		let backlinksElement = response.querySelector(".backlinks");
		let backlinksHTML = backlinksElement
							? `<span 
								class="data-field aux-links backlinks" 
								>${backlinksElement.innerHTML}</span>`
							: null;

		//	The similar-links link (if exists).
		let similarsElement = response.querySelector(".similars");
		let similarsHTML = similarsElement
						   ? `<span 
							   class="data-field aux-links similars"
							   >${similarsElement.innerHTML}</span>`
						   : null;

		//	The link-link-bibliography link (if exists).
		let linkbibElement = response.querySelector(".link-bibliography");
		let linkbibHTML = linkbibElement
						  ? `<span 
							  class="data-field aux-links link-bibliography"
							  >${linkbibElement.innerHTML}</span>`
						  : null;

		//	All the aux-links (tags, archive, backlinks, similars, link link-bib).
		let auxLinksHTML = ([ archiveLinkHTML, backlinksHTML, similarsHTML, linkbibHTML ].filter(x => x).join(", ") || null);
		if (auxLinksHTML || tagsHTML)
			auxLinksHTML = ` (${([ tagsHTML, auxLinksHTML ].filter(x => x).join("; ") || null)})`;

		//  Combined author, date, & aux-links.
		let authorDateAuxHTML = ([ authorHTML, dateHTML, auxLinksHTML ].filter(x => x).join("") || null);

		//	Abstract (if exists).
		let abstractElement = response.querySelector("blockquote");
		let abstractHTML = null;
		let thumbnailFigureHTML = null;
		if (abstractElement) {
			let abstractDocument = newDocument(abstractElement.childNodes);
			Annotations.postProcessReferenceEntry(abstractDocument, link);

			//	Request image inversion judgments from invertornot.
			requestImageInversionDataForImagesInContainer(abstractDocument);

			//	Thumbnail figure.
			let pageThumbnailImage = abstractDocument.querySelector("img.page-thumbnail");
			if (pageThumbnailImage) {
				//	Make thumbnail image load eagerly instead of lazily.
				pageThumbnailImage.loading = "eager";
				pageThumbnailImage.decoding = "sync";

				/*	On sufficiently wide viewports, pull out thumbnail for 
					proper floating.
				 */
				if (GW.mediaQueries.mobileWidth.matches == false) {
					let pageThumbnailFigure = pageThumbnailImage.closest("figure");
					thumbnailFigureHTML = pageThumbnailFigure.outerHTML;
					pageThumbnailFigure.remove();
				}
			}

			abstractHTML = abstractDocument.innerHTML;
		}

		//	File includes (if any).
		let fileIncludesElement = response.querySelector(".aux-links-transclude-file");
		let fileIncludesHTML = null;
		if (fileIncludesElement) {
			/*	Remove any file embed links that lack a valid content 
				type (e.g., foreign-site links that have not been 
				whitelisted for embedding).
			 */
			Transclude.allIncludeLinksInContainer(fileIncludesElement).forEach(includeLink => {
				if (Content.contentTypeForLink(includeLink) == null)
					includeLink.remove();
			});

			/*	Do not include the file includes section if no valid
				include-links remain.
			 */
			if (isNodeEmpty(fileIncludesElement) == false)
				fileIncludesHTML = fileIncludesElement.innerHTML;
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
				title:                    titleHTML,
				titleLinkHref:            titleLinkHref,
				titleLinkClass:           titleLinkClass,
				titleLinkDataAttributes:  titleLinkDataAttributes,
				author:                   authorHTML,
				date:                     dateHTML,
				auxLinks:                 auxLinksHTML,
				authorDateAux:            authorDateAuxHTML,
				abstract:                 abstractHTML,
				thumbnailFigure:          thumbnailFigureHTML,
				fileIncludes:             fileIncludesHTML
			},
			template:                       "annotation-blockquote-inside",
			linkTarget:                     (GW.isMobile() ? "_self" : "_blank"),
			whichTab:                       (GW.isMobile() ? "current" : "new"),
			tabOrWindow:                    (GW.isMobile() ? "tab" : "window"),
			popFrameTemplate:               "annotation-blockquote-not",
			popFrameTitleText:              popFrameTitleText,
			popFrameTitleLinkHref:          titleLinkHref,
			popFrameTitleArchiveLinkHref:   archiveLinkHref
		};
	},

	/*  Post-process an already-constructed local annotation 
		(do HTML cleanup, etc.).
	 */
	postProcessReferenceEntry: (referenceEntry, link = null) => {
		//	Unwrap extraneous <div>s, if present.
		if (   referenceEntry.firstElementChild == referenceEntry.lastElementChild
			&& referenceEntry.firstElementChild.tagName == "DIV")
			unwrap(referenceEntry.firstElementChild);

		//	If there’s a “See Also” section, rectify its classes.
		let seeAlsoList = referenceEntry.querySelector(_π(".see-also-append", " ", [ "ul", "ol" ]).join(", "));
		if (seeAlsoList) {
			seeAlsoList.classList.add("aux-links-list", "see-also-list");

			let listLabel = previousBlockOf(seeAlsoList, { notBlockElements: [ ".columns" ] });
			if (listLabel)
				listLabel.classList.add("aux-links-list-label", "see-also-list-label");
		}

		//	Prevent erroneous collapse class.
		referenceEntry.querySelectorAll(".aux-links-append.collapse").forEach(auxLinksAppendCollapse => {
			auxLinksAppendCollapse.classList.add("bare-content-not");
		});

		//	Unwrap more extraneous <div>s, if present.
		let pageDescriptionClass = "page-description-annotation";
		let pageDescription = referenceEntry.querySelector(`div.${pageDescriptionClass}`);
		if (pageDescription)
			unwrap(pageDescription, { moveClasses: [ pageDescriptionClass ] });
	},
};

//	Fire load event.
GW.notificationCenter.fireEvent("Annotations.didLoad");
