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
			   ? (target.pathname.endsWith("/") 
			   	  ? target.pathname + "index" 
			   	  : target.pathname) + target.hash
			   : (target instanceof HTMLAnchorElement
			   	  ? target.getAttribute("href")
			   	  : target.href));
	},

	shouldLocalizeContentFromLink: (link) => {
		return false;
	},

	/***********/
	/*	Caching.
	 */

	//	Convenience method.
	cachedDocumentForLink: (link) => {
		return (Annotations.cachedReferenceDataForLink(link)?.document ?? null);
	},

	loadingFailedString: "LOADING_FAILED",

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

    /*  Returns true iff cached reference data exists for the given link.
     */
    //	Called by: Extracts.setUpAnnotationLoadEventsWithin (extracts-annotations.js)
    cachedDataExists: (link) => {
    	let referenceData = Annotations.cachedReferenceDataForLink(link);
		return (   referenceData != null
				&& referenceData != Annotations.loadingFailedString);
    },

    /*  Returns cached annotation reference data for a given link, or else
    	either “LOADING_FAILED” (if loading the annotation was attempted but
    	failed) or null (if the annotation has not been loaded).
     */
    referenceDataForLink: (link) => {
    	return Annotations.cachedReferenceDataForLink(link);
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

	waitForDataLoad: (link, loadHandler = null, loadFailHandler = null) => {
		let referenceData = Annotations.referenceDataForLink(link);
		if (referenceData != null) {
			if (referenceData == Annotations.loadingFailedString) {
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

		//	Get URL of the annotation resource.
        let sourceURL = Annotations.sourceURLForLink(link);

		//	Retrieve, parse, process, and cache the annotation data.
		doAjax({
			location: sourceURL.href,
			onSuccess: (event) => {
				let responseDocument = newDocument(event.target.responseText);

				//	Request the page image thumbnail, to cache it.
				let pageImage = responseDocument.querySelector(".page-thumbnail");
				if (   pageImage != null 
					&& Images.isSVG(pageImage) == false)
					doAjax({ location: Images.thumbnailURLForImage(pageImage) });

				/*	Construct and cache a reference data object, then fire the
					appropriate event.
				 */
				Annotations.cacheReferenceDataForLink(Annotations.referenceDataFromParsedAPIResponse(responseDocument, link), link);

				GW.notificationCenter.fireEvent("Annotations.annotationDidLoad", {
					link: link
				});
			},
			onFailure: (event) => {
				Annotations.cacheReferenceDataForLink(Annotations.loadingFailedString, link);

				GW.notificationCenter.fireEvent("Annotations.annotationLoadDidFail", { link: link });

				//	Send request to record failure in server logs.
				GWServerLogError(sourceURL.href + `--${event.target.status}`, "missing annotation");
			}
		});

		//	Call any provided handlers, if/when appropriate.
		if (loadHandler || loadFailHandler)
			Annotations.waitForDataLoad(link, loadHandler, loadFailHandler);
    },

	//	Called by: Annotations.load
	referenceDataFromParsedAPIResponse: (response, link) => {
		let titleLink = response.querySelector([ Annotations.annotatedLinkFullClass,
												 Annotations.annotatedLinkPartialClass
												 ].map(className => `a.${className}`).join(", "));

		//	Strip date ranges (if any).
		stripDateRangeMetadataInBlock(titleLink);

		//	On mobile, use mobile-specific link href, if provided.
		let titleLinkHref = (   titleLink.dataset.hrefMobile
							 && GW.isMobile())
							? titleLink.dataset.hrefMobile
							: titleLink.href;

		//	Construct title link class.
		let titleLinkClasses = [ "title-link" ];

		/*  Import link classes (excluding the ones that designate annotated 
			links, lest we have infinite recursion of annotation popups).
		 */
		titleLinkClasses.push(...(Array.from(titleLink.classList).filter(titleLinkClass => [
			"link-annotated",
			"link-annotated-partial"
		].includes(titleLinkClass) == false)));

		//	Special handling for links with separate ‘iframe’ URLs.
		if (titleLink.dataset.urlIframe)
			titleLinkClasses.push("link-live");

		//	Data attributes for the title link.
		let titleLinkDataAttributes = [ ];
		for (let [ attrName, attrValue ] of Object.entries(titleLink.dataset))
			titleLinkDataAttributes.push(`data-${(attrName.camelCaseToKebabCase())}="${attrValue}"`);

		/*	Import link icon data attributes from the annotated link itself 
			(but do not replace ones already specified by the annotation 
			 title-link).
		 */
		for (let [ attrName, attrValue ] of Object.entries(link.dataset))
			if (   attrName.startsWith("linkIcon")
				&& titleLink.dataset[attrName] == null)
				titleLinkDataAttributes.push(`data-${(attrName.camelCaseToKebabCase())}="${attrValue}"`);

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

		//	All the aux-links (tags, backlinks, similars, link link-bib).
		let auxLinksHTML = ([ backlinksHTML, similarsHTML, linkbibHTML ].filter(x => x).join(", ") || null);
		if (auxLinksHTML || tagsHTML)
			auxLinksHTML = `<span class="aux-links-field-container"> (${([ tagsHTML, auxLinksHTML ].filter(x => x).join("; ") || null)})</span>`;

		//  Combined author, date, & aux-links.
		let authorDateAuxHTML = ([ authorHTML, dateHTML, auxLinksHTML ].filter(x => x).join("") || null);

		//	Abstract (if exists).
		let abstractElement = response.querySelector("blockquote");
		let abstractHTML = null;
		let thumbnailFigureHTML = null;
		if (abstractElement) {
			let abstractDocument = newDocument(abstractElement.childNodes);

			//	Request image inversion judgments from invertOrNot.
			requestImageInversionJudgmentsForImagesInContainer(abstractDocument);

			//	Request image outlining judgments from outlineOrNot.
			requestImageOutliningJudgmentsForImagesInContainer(abstractDocument);

			//	Post-process abstract.
			Annotations.postProcessAnnotationAbstract(abstractDocument, link);

			//	Retrieve thumbnail HTML (if set).
			thumbnailFigureHTML = abstractDocument.thumbnailFigureHTML;

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

			/*	Set special template for file includes of content transforms.
			 */
			Transclude.allIncludeLinksInContainer(fileIncludesElement).forEach(includeLink => {
				if (   Content.isContentTransformLink(includeLink)
					&& includeLink.dataset.includeTemplate == null)
					includeLink.dataset.includeTemplate = "$annotationFileIncludeTemplate";
			});

			/*	Do not include the file includes section if no valid
				include-links remain.
			 */
			if (isNodeEmpty(fileIncludesElement) == false)
				fileIncludesHTML = fileIncludesElement.innerHTML;
		}

		//	TItle bar link should go to /ref/ page for the annotation.
		let popFrameTitleLinkHref = "/ref/" + (link.id || titleLink.id.slice("link-bibliography-".length));

		return {
			document: response,
			content: {
				title:                    titleLink.innerHTML,
				titleLinkHref:            titleLinkHref,
				titleLinkClass:           titleLinkClasses.join(" "),
				titleLinkDataAttributes:  titleLinkDataAttributes,
				author:                   authorHTML,
				date:                     dateHTML,
				auxLinks:                 auxLinksHTML,
				authorDateAux:            authorDateAuxHTML,
				abstract:                 abstractHTML,
				thumbnailFigure:          thumbnailFigureHTML,
				fileIncludes:             fileIncludesHTML
			},
			template:                     "annotation-blockquote-inside",
			popFrameTemplate:             "annotation-blockquote-not",
			popFrameTitle:                titleLink.cloneNode(true).trimQuotes().innerHTML,
			popFrameTitleLinkHref:        popFrameTitleLinkHref
		};
	},

	/*  Post-process an already-constructed local annotation
		(do HTML cleanup, etc.).
	 */
	postProcessAnnotationAbstract: (abstractDocument, link = null) => {
		//	Unwrap extraneous <div>s, if present.
		if (   abstractDocument.firstElementChild == abstractDocument.lastElementChild
			&& abstractDocument.firstElementChild.tagName == "DIV")
			unwrap(abstractDocument.firstElementChild);

		//	If there’s a “See Also” section, rectify its classes.
		let seeAlsoList = abstractDocument.querySelector(_π(".see-also-append", " ", [ "ul", "ol" ]).join(", "));
		if (seeAlsoList) {
			seeAlsoList.classList.add("aux-links-list", "see-also-list");

			let listLabel = previousBlockOf(seeAlsoList, { notBlockElements: [ ".columns" ] });
			if (listLabel)
				listLabel.classList.add("aux-links-list-label", "see-also-list-label");
		}

		//	Prevent erroneous collapse class.
		abstractDocument.querySelectorAll(".aux-links-append.collapse").forEach(auxLinksAppendCollapse => {
			auxLinksAppendCollapse.classList.add("bare-content-not");
		});

		//	Unwrap more extraneous <div>s, if present.
		let pageDescriptionClass = "page-description-annotation";
		let pageDescription = abstractDocument.querySelector(`div.${pageDescriptionClass}`);
		if (pageDescription)
			pageDescription = unwrap(pageDescription, { moveClasses: [ pageDescriptionClass ] });

		//	Page thumbnail.
		let pageThumbnail = abstractDocument.querySelector("img.page-thumbnail");
		if (pageThumbnail) {
			//	Replace full-size page image with thumbnail.
			Images.thumbnailifyImage(pageThumbnail);

			//	Make page image thumbnail load eagerly instead of lazily.
			pageThumbnail.loading = "eager";
			pageThumbnail.decoding = "sync";

			/*	On sufficiently wide viewports, pull out thumbnail figure
				for proper floating.
			 */
			let pageThumbnailFigure = pageThumbnail.closest("figure");
			if (GW.mediaQueries.mobileWidth.matches == false) {
				abstractDocument.thumbnailFigureHTML = pageThumbnailFigure.outerHTML;
				pageThumbnailFigure.remove();
			} else if (pageDescription) {
				abstractDocument.insertBefore(pageThumbnailFigure, pageDescription.last.nextElementSibling);
			}
		}
	},
};

//	Fire load event.
GW.notificationCenter.fireEvent("Annotations.didLoad");
