Content = {
	targetIdentifier: (target) => {
		return target.pathname + target.hash;
	},

	referenceDataForIdentifier: (identifier) => {
		let content = Content.cachedContentForIdentifier(identifier);
		if (   content == null
			|| content == "LOADING_FAILED") {
			return content;
		} else {
			return Content.referenceDataFromContent(content, identifier);
		}
	},

	referenceDataForTarget: (target) => {
		return Content.referenceDataForIdentifier(Content.targetIdentifier(target));
	},

	sourceURLForIdentifier: (identifier) => {
		let url = new URL(  "https://"
						  + location.hostname
						  + identifier);
		url.hash = "";

		return url;
	},

	sourceURLForTarget: (target) => {
		return Content.sourceURLForIdentifier(Content.targetIdentifier(target));
	},

	contentTypeForIdentifier: (identifier) => {
		return "localPage";
	},

	contentTypeForTarget: (target) => {
		return Content.contentTypeForIdentifier(Content.targetIdentifier(target));
	},

	cachedContent: { },

	cachedContentExists: (identifier) => {
		let cachedContent = Content.cachedContentForIdentifier(identifier);
        return (   cachedContent != null 
        		&& cachedContent != "LOADING_FAILED");
	},

	cachedContentForIdentifier: (identifier) => {
		let sourceURL = Content.sourceURLForIdentifier(identifier);
		return Content.cachedContent[sourceURL];
	},

	cacheContentForIdentifier: (content, identifier) => {
		Content.cachedContent[Content.sourceURLForIdentifier(identifier)] = content;
	},

	//	Called by: extracts.localTranscludeForTarget (extracts-annotations.js)
	waitForContentLoad: (identifier, loadHandler = null, loadFailHandler = null) => {
		if (Content.cachedContentForIdentifier(identifier) == "LOADING_FAILED") {
            if (loadFailHandler)
            	loadFailHandler(identifier);

			return;
		} else if (Content.cachedContentForIdentifier(identifier)) {
            if (loadHandler)
            	loadHandler(identifier);

			return;
		}

		let didLoadHandler = (info) => {
            if (loadHandler)
            	loadHandler(identifier);

			GW.notificationCenter.removeHandlerForEvent("Content.contentLoadDidFail", loadDidFailHandler);
        };
        let loadDidFailHandler = (info) => {
            if (loadFailHandler)
            	loadFailHandler(identifier);

			GW.notificationCenter.removeHandlerForEvent("Content.contentDidLoad", didLoadHandler);
        };
		let options = { 
        	once: true, 
        	condition: (info) => info.sourceURL.href == Content.sourceURLForIdentifier(identifier).href
        };

        GW.notificationCenter.addHandlerForEvent("Content.contentDidLoad", didLoadHandler, options);
        GW.notificationCenter.addHandlerForEvent("Content.contentLoadDidFail", loadDidFailHandler, options);
	},

	loadContent: (identifier, loadHandler = null, loadFailHandler = null) => {
        GWLog("Content.loadContent", "content.js", 2);

		let sourceURL = Content.sourceURLForIdentifier(identifier);

		let processResponse = (response) => {
			let content = Content.contentFromResponseForIdentifier(response, identifier);

			if (content) {
				Content.cacheContentForIdentifier(content, identifier);
			
				GW.notificationCenter.fireEvent("Content.contentDidLoad", {
					sourceURL: sourceURL
				});
			} else {
				Content.cacheContentForIdentifier("LOADING_FAILED", identifier);
			
				GW.notificationCenter.fireEvent("Content.contentLoadDidFail", { 
					sourceURL: sourceURL 
				});

				//	Send request to record failure in server logs.
				GWServerLogError(sourceURL + `--could-not-process`, "problematic content");
			}

		};

		if (sourceURL.pathname == location.pathname) {
			processResponse();
		} else {
			doAjax({
				location: sourceURL.href,
				onSuccess: (event) => {
					processResponse(event.target.responseText);
				},
				onFailure: (event) => {
					Content.cacheContentForIdentifier("LOADING_FAILED", identifier);

					GW.notificationCenter.fireEvent("Content.contentLoadDidFail", { 
						identifier: identifier 
					});

					//	Send request to record failure in server logs.
					GWServerLogError(sourceURL, "missing content");
				}
			});
		}

		//	Call any provided handlers, if/when appropriate.
		Content.waitForContentLoad(identifier, loadHandler, loadFailHandler);
	},

	contentFromResponseForIdentifier: (response, identifier) => {
		return Content.contentTypes.localPage.contentFromResponseForIdentifier(response, identifier);
	},

	referenceDataFromContent: (content, identifier) => {
		return Content.contentTypes.localPage.referenceDataFromContent(content, identifier);
	},

	updateCachedContent: (identifier, updateFunction) => {
		if (Content.cachedContentExists(identifier) == false)
			return;

		let content = Content.cachedContentForIdentifier(identifier);

		switch (Content.contentTypeForIdentifier(identifier)) {
			case "localPage":
				updateFunction(content.document);
				break;
			default:
				break;
		}
	},

	/****************/
	/*	Page content.
	 */

	contentTypes: {
		localPage: {
			contentFromResponseForIdentifier: (response, identifier) => {
				let page = response
						   ? newDocument(response)
						   : document;

				//	Get the body classes.
				let pageBodyClasses = page.querySelector("meta[name='page-body-classes']").getAttribute("content");

				//  Get the page title.
				let pageTitle = page.querySelector("title").innerHTML.match(Content.contentTypes.localPage.pageTitleRegexp)[1];

				//	Get the page thumbnail URL and metadata.
				let pageThumbnailHTML;
				let pageThumbnailMetaTag = page.querySelector("meta[property='og:image']");
				if (pageThumbnailMetaTag) {
					let pageThumbnailURL = new URL(pageThumbnailMetaTag.getAttribute("content"));

					//	Alt text, if provided.
					let pageThumbnailAltMetaTag = page.querySelector("meta[property='og:image:alt']");
					let pageThumbnailAltText = pageThumbnailAltMetaTag
											   ? pageThumbnailAltMetaTag.getAttribute("content")
											   : `Thumbnail image for “${pageTitle}”`;

					//	Image dimensions.
					let pageThumbnailWidth = page.querySelector("meta[property='og:image:width']").getAttribute("content");
					let pageThumbnailHeight = page.querySelector("meta[property='og:image:height']").getAttribute("content");

					//	Construct and save the <img> tag.
					if (pageThumbnailURL.pathname.startsWith(Content.contentTypes.localPage.defaultPageThumbnailPathnamePrefix) == false)
						pageThumbnailHTML = `<img
							src='${pageThumbnailURL.href}'
							alt='${pageThumbnailAltText}'
							width='${pageThumbnailWidth}'
							height='${pageThumbnailHeight}'
							style='width: ${pageThumbnailWidth}px; height: auto;'
								>`;

					//	Request the image, to cache it.
					doAjax({ location: pageThumbnailURL.href });
				}

				if (response) {
					//  Fire contentDidLoad event, if need be.
					GW.notificationCenter.fireEvent("GW.contentDidLoad", {
						source: "Content.referenceDataFromPage",
						document: page,
						loadLocation: Content.sourceURLForIdentifier(identifier),
						baseLocation: Content.sourceURLForIdentifier(identifier),
						flags: (  GW.contentDidLoadEventFlags.needsRewrite
								| GW.contentDidLoadEventFlags.collapseAllowed)
					});
				}

				return {
					title:          pageTitle,
					bodyClasses:    pageBodyClasses,
					thumbnailHTML:  pageThumbnailHTML,
					document:       page
				};
			},

			referenceDataFromContent: (page, identifier = null) => {
				//  The page content is the page body plus the metadata block.
				let pageContent = newDocument();
				//	Add the page metadata block.
				let pageMetadataBlock = page.document.querySelector("#page-metadata");
				if (pageMetadataBlock) {
					pageContent.append(newDocument(pageMetadataBlock));

					pageMetadataBlock = pageContent.querySelector("#page-metadata");
					pageMetadataBlock.classList.remove("markdownBody");
					if (pageMetadataBlock.className == "")
						pageMetadataBlock.removeAttribute("class");
				}
				//	Add the page main content block.
				pageContent.append(newDocument(page.document.querySelector("#markdownBody").childNodes));

				//	Find the target element and/or containing block, if any.
				let url = new URL(  "https://"
								  + location.hostname
								  + identifier);
				let element = url.hash
							  ? targetElementInDocument(url, pageContent)
							  : null;
				let block = element
							? Content.contentTypes.localPage.nearestBlockElement(element)
							: null;

				//	Pop-frame title text.
				let popFrameTitleTextParts = [ ];
				if (url.pathname != location.pathname)
					popFrameTitleTextParts.push(page.title);

				//	Section title or block id.
				let nearestSection = block
									 ? block.closest("section")
									 : null;
				if (nearestSection) {
					//	Section mark (§) for sections.
					popFrameTitleTextParts.push("&#x00a7;");
					if (nearestSection.id == "footnotes") {
						popFrameTitleTextParts.push("Footnotes");
					} else {
						popFrameTitleTextParts.push(nearestSection.firstElementChild.textContent);
					}
				} else if (element) {
					popFrameTitleTextParts.push(url.hash);
				}

				return {
					pageTitle:               page.title,
					pageBodyClasses:         page.bodyClasses,
					pageThumbnailHTML:       page.thumbnailHTML,
					pageContent:             pageContent,
					pageContentHTML:         pageContent.innerHTML,
					targetElement:           element,
					targetElementHTML:       element ? element.innerHTML : null,
					targetBlock:             block,
					targetBlockHTML:         block ? block.innerHTML : null,
					titleLinkHref:           url.href,
					popFrameTitleText:       popFrameTitleTextParts.join(" "),
					popFrameTitleTextShort:  popFrameTitleTextParts.first
				}
			},

			pageTitleRegexp: /^(.+?) · Gwern\.net$/,
			defaultPageThumbnailPathnamePrefix: "/static/img/logo/logo-",

			nearestBlockElement: (element) => {
				let blockElementSelector = [
					"section",
					".footnote",
					".sidenote",
					".aux-links-append",
					".markdownBody > *",
					".include-wrapper-block"
				].join(", ");
				return (   element.closest(blockElementSelector)
						|| element.closest("p"))
			}
		}
	}
};
