Content = {
	targetIdentifier: (target) => {
		return target.pathname + target.hash;
	},

	/*******************/
	/*	Content caching.
	 */

	cachedContent: { },

	cachedDataExists: (identifier) => {
		let cachedContent = Content.cachedContentForIdentifier(identifier);
        return (   cachedContent != null 
        		&& cachedContent != "LOADING_FAILED");
	},

	//	Convenience method.
	cachedDocumentForLink: (link) => {
		let identifier = Content.targetIdentifier(link);
		let cachedContent = Content.cachedContentForIdentifier(identifier);
		return (cachedContent
				? cachedContent.document
				: null);
	},

	contentCacheKeyForIdentifier: (identifier) => {
		let url = new URL(  "https://"
						  + location.hostname
						  + identifier);
		return url.pathname;
	},

	cachedContentForIdentifier: (identifier) => {
		return Content.cachedContent[Content.contentCacheKeyForIdentifier(identifier)];
	},

	cacheContentForIdentifier: (content, identifier) => {
		Content.cachedContent[Content.contentCacheKeyForIdentifier(identifier)] = content;
	},

	updateCachedContent: (identifier, updateFunction) => {
		if (Content.cachedDataExists(identifier) == false)
			return;

		let content = Content.cachedContentForIdentifier(identifier);

		switch (Content.contentTypeForIdentifier(identifier)) {
			case Content.contentTypes.localPage:
				updateFunction(content.document);
				break;
			default:
				break;
		}
	},

	/*******************/
	/*	Content loading.
	 */

	sourceURLsForIdentifier: (identifier) => {
		return Content.contentTypeForIdentifier(identifier).sourceURLsForIdentifier(identifier);
	},

	sourceURLsForTarget: (target) => {
		return Content.sourceURLsForIdentifier(Content.targetIdentifier(target));
	},

	//	Called by: Extracts.handleIncompleteReferenceData (extracts.js)
	waitForDataLoad: (identifier, loadHandler = null, loadFailHandler = null) => {
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
        	condition: (info) => (info.identifier == identifier)
        };

        GW.notificationCenter.addHandlerForEvent("Content.contentDidLoad", didLoadHandler, options);
        GW.notificationCenter.addHandlerForEvent("Content.contentLoadDidFail", loadDidFailHandler, options);
	},

	load: (identifier, loadHandler = null, loadFailHandler = null, sourceURLsRemaining = null) => {
        GWLog("Content.load", "content.js", 2);

		sourceURLsRemaining = sourceURLsRemaining ?? Content.sourceURLsForIdentifier(identifier);
		let sourceURL = sourceURLsRemaining.shift();

		let processResponse = (response) => {
			let content = Content.contentFromResponse(response, identifier, sourceURL);

			if (content) {
				Content.cacheContentForIdentifier(content, identifier);
			
				GW.notificationCenter.fireEvent("Content.contentDidLoad", {
					identifier: identifier
				});
			} else {
				Content.cacheContentForIdentifier("LOADING_FAILED", identifier);
			
				GW.notificationCenter.fireEvent("Content.contentLoadDidFail", { 
					identifier: identifier 
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
                    let httpContentType = event.target.getResponseHeader("Content-Type").match(/(.+?)(?:;|$)/)[1];
                    let contentType = Content.contentTypeForIdentifier(identifier);
                    if (   contentType.permittedContentTypes
                    	&& contentType.permittedContentTypes.includes(httpContentType) == false) {
	                	//	Send request to record failure in server logs.
                        GWServerLogError(includeLink.href + `--bad-content-type`, "bad content type");

                        return;
                    }

					processResponse(event.target.responseText);
				},
				onFailure: (event) => {
					if (sourceURLsRemaining.length > 0) {
						Content.load(identifier, null, null, sourceURLsRemaining);
						return;
					}

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
		if (loadHandler || loadFailHandler)
			Content.waitForDataLoad(identifier, loadHandler, loadFailHandler);
	},

	contentFromResponse: (response, identifier, loadURL) => {
		let contentType = Content.contentTypeForIdentifier(identifier);
		return contentType.contentFromResponse(response, identifier, loadURL);
	},

	/****************************/
	/*	Reference data retrieval.
	 */

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

	referenceDataFromContent: (content, identifier) => {
		let contentType = Content.contentTypeForIdentifier(identifier);
		return contentType.referenceDataFromContent(content, identifier);
	},

	/**************************************************************/
	/*	CONTENT TYPES

		Each has five necessary members:

		.matches(string) => boolean
		.matchesLink(URL|Element) => boolean
		.sourceURLsForIdentifier(string) => [ URL ]
		.contentFromResponse(string, string, URL) => object
		.referenceDataFromContent(object, string) => object
	 */

	contentTypeForIdentifier: (identifier) => {
		for ([ typeName, contentType ] of Object.entries(Content.contentTypes))
			if (contentType.matches(identifier))
				return contentType;

		return null;
	},

	contentTypeForTarget: (target) => {
		for ([ typeName, contentType ] of Object.entries(Content.contentTypes))
			if (contentType.matchesLink(target))
				return contentType;

		return null;
	},

	contentTypes: {
		localCodeFile: {
			matches: (identifier) => {
				let url = new URL(  "https://"
								  + location.hostname
								  + identifier);

				//	Maybe it’s an aux-links link?
				if (url.pathname.startsWith("/metadata/"))
					return false;

				//	Maybe it’s a local document link?
				if (   url.pathname.startsWith("/docs/www/")
                	|| (   url.pathname.startsWith("/docs/")
                		&& url.pathname.match(/\.(html|pdf)$/i) != null))
                	return false;

				let codeFileURLRegExp = new RegExp(
					  '\\.('
					+ Content.contentTypes.localCodeFile.codeFileExtensions.join("|")
					+ ')$'
				, 'i');
				return (url.pathname.match(codeFileURLRegExp) != null);
			},

			matchesLink: (link) => {
				//	Maybe it’s a foreign link?
				if (link.hostname != location.hostname)
					return false;

				//	Maybe it’s an annotated link?
				if (Annotations.isAnnotatedLink(link))
					return false

				let identifier = Content.targetIdentifier(link);
				return Content.contentTypes.localCodeFile.matches(identifier);
			},

			/*  We first try to retrieve a syntax-highlighted version of the 
				given code file, stored on the server as an HTML fragment. If 
				present, we embed that. If there’s no such fragment, then we 
				just embed the contents of the actual code file, in a 
				<pre>-wrapped <code> element.
			 */
			sourceURLsForIdentifier: (identifier) => {
				let url = urlSansHash(  "https://"
									  + location.hostname
									  + identifier);

				return [ new URL(url.href + ".html"), url ];
			},

			contentFromResponse: (response, identifier, loadURL) => {
				let codeBlock;
				if (response.slice(0, 1) == "<") {
					codeBlock = newDocument(response);
				} else {
					let htmlEncodedResponse = response.replace(
						/[<>]/g, 
						c => ('&#' + c.charCodeAt(0) + ';')
					).split("\n").map(
						line => (`<span class="line">${(line || "&nbsp;")}</span>`)
					).join("\n");
					codeBlock = newDocument(  `<pre class="raw-code"><code>`
											+ htmlEncodedResponse
											+ `</code></pre>`);
				}

				return {
					document: codeBlock
				};
			},

			referenceDataFromContent: (codePage, identifier = null) => {
				return {
					content: codePage.document
				};
			},

			codeFileExtensions: [
				//	Truncated at 1000 lines for preview.
				"bash", "c", "conf", "css", "csv", "diff", "hs", "html", "js", 
				"json", "jsonl", "opml", "page", "patch", "php", "py", "R", 
				"sh", "xml", "yaml",
				//	Non-syntax highlighted (due to lack of known format), but truncated:
				"txt"
			]
		},

		localFragment: {
			matches: (identifier) => {
				let url = new URL(  "https://"
								  + location.hostname
								  + identifier);

				return (   url.pathname.startsWith("/metadata/")
						&& url.pathname.endsWith(".html"));
			},

			matchesLink: (link) => {
				//	Maybe it’s a foreign link?
				if (link.hostname != location.hostname)
					return false;

				//	Maybe it’s an annotated link?
				if (Annotations.isAnnotatedLink(link))
					return false

				let identifier = Content.targetIdentifier(link);
				return Content.contentTypes.localFragment.matches(identifier);
			},

			sourceURLsForIdentifier: (identifier) => {
				let url = urlSansHash(  "https://"
									  + location.hostname
									  + identifier);

				return [ url ];
			},

			contentFromResponse: (response, identifier, loadURL) => {
				let fragment = newDocument(response);

				let auxLinksLinkType = AuxLinks.auxLinksLinkType(loadURL);
				if (auxLinksLinkType) {
					let auxLinksList = fragment.querySelector("ul, ol");
					if (auxLinksList) {
						auxLinksList.classList.add("aux-links-list", auxLinksLinkType + "-list");

						if (auxLinksLinkType == "backlinks")
							auxLinksList.dataset.targetUrl = AuxLinks.targetOfAuxLinksLink(loadURL);
					}
				}

				//  Fire contentDidLoad event, if need be.
				GW.notificationCenter.fireEvent("GW.contentDidLoad", {
					source: "Content.contentTypes.localFragment.load",
					container: fragment,
					document: fragment
				});

				return {
					document: fragment
				};
			},

			referenceDataFromContent: (fragment, identifier) => {
				return {
					content: fragment.document
				};
			},

		    permittedContentTypes: [ "text/html" ]
		},

		localPage: {
			matches: (identifier) => {
				let url = new URL(  "https://"
								  + location.hostname
								  + identifier);

				/*  If it has a period in it, it’s not a page, but is something 
					else, like a file of some sort, or a locally archived 
					document.
				 */
				return (url.pathname.match(/\./) == null
						&& (   url.pathname != location.pathname
							|| url.hash > ""));
			},

			matchesLink: (link) => {
				//	Maybe it’s a foreign link?
				if (link.hostname != location.hostname)
					return false;

				//	Maybe it’s an annotated link?
				if (Annotations.isAnnotatedLink(link))
					return false

				let identifier = Content.targetIdentifier(link);
				return Content.contentTypes.localPage.matches(identifier);
			},

			sourceURLsForIdentifier: (identifier) => {
				let url = urlSansHash(  "https://"
									  + location.hostname
									  + identifier);

				return [ url ];
			},

			contentFromResponse: (response, identifier, loadURL) => {
				let page = response
						   ? newDocument(response)
						   : document;

				if (response)
					page.baseLocation = loadURL;

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
						source: "Content.contentTypes.localPage.load",
						container: page,
						document: page,
						loadLocation: loadURL
					});
				}

				return {
					title:          pageTitle,
					bodyClasses:    pageBodyClasses,
					thumbnailHTML:  pageThumbnailHTML,
					document:       page
				};
			},

			referenceDataFromContent: (page, identifier) => {
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
							? nearestBlockElement(element)
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
					content:                 pageContent,
					contentHTML:             pageContent.innerHTML,
					targetElement:           element,
					targetElementHTML:       element ? element.innerHTML : null,
					targetBlock:             block,
					targetBlockHTML:         block ? block.innerHTML : null,
					titleLinkHref:           url.href,
					popFrameTitleText:       popFrameTitleTextParts.join(" "),
					popFrameTitleTextShort:  popFrameTitleTextParts.first
				}
			},

		    permittedContentTypes: [ "text/html" ],
			pageTitleRegexp: /^(.+?) · Gwern\.net$/,
			defaultPageThumbnailPathnamePrefix: "/static/img/logo/logo-"
		}
	}
};
