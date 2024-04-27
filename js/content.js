Content = {
    /*******************/
    /*  Content caching.
     */

    cachedContent: { },

    contentCacheKeyForLink: (link) => {
    	return (   Content.contentTypeForLink(link)?.contentCacheKeyForLink?.(link) 
    			?? (Content.sourceURLsForLink(link)?.first ?? link).href);
    },

    cacheContentForLink: (content, link) => {
        Content.cachedContent[Content.contentCacheKeyForLink(link)] = content;
    },

    cachedContentForLink: (link) => {
        //  Special case for the link being to the current page.
        if (   link.pathname == location.pathname
            && Content.cachedContent[Content.contentCacheKeyForLink(link)] == null)
            Content.load(link);

        return Content.cachedContent[Content.contentCacheKeyForLink(link)];
    },

    cachedDocumentForLink: (link) => {
        let content = Content.cachedContentForLink(link);
        return (content && content != "LOADING_FAILED"
                ? content.document
                : null);
    },

    cachedDataExists: (link) => {
        let cachedContent = Content.cachedContentForLink(link);
        return (   cachedContent != null
                && cachedContent != "LOADING_FAILED");
    },

    updateCachedContent: (link, updateFunction) => {
        if (Content.cachedDataExists(link) == false)
            return;

        let content = Content.cachedContentForLink(link);

        switch (Content.contentTypeForLink(link)) {
            case Content.contentTypes.localPage:
                updateFunction(content.document);
                break;
            default:
                break;
        }
    },

    /*******************/
    /*  Content loading.
     */

    sourceURLsForLink: (link) => {
        return Content.contentTypeForLink(link)?.sourceURLsForLink?.(link);
    },

    waitForDataLoad: (link, loadHandler = null, loadFailHandler = null) => {
        if (Content.cachedContentForLink(link) == "LOADING_FAILED") {
            if (loadFailHandler)
                loadFailHandler(link);

            return;
        } else if (Content.cachedContentForLink(link)) {
            if (loadHandler)
                loadHandler(link);

            return;
        }

        let didLoadHandler = (info) => {
            if (loadHandler)
                loadHandler(link);

            GW.notificationCenter.removeHandlerForEvent("Content.contentLoadDidFail", loadDidFailHandler);
        };
        let loadDidFailHandler = (info) => {
            if (loadFailHandler)
                loadFailHandler(link);

            GW.notificationCenter.removeHandlerForEvent("Content.contentDidLoad", didLoadHandler);
        };
        let options = {
            once: true,
            condition: (info) => (info.link == link)
        };

        GW.notificationCenter.addHandlerForEvent("Content.contentDidLoad", didLoadHandler, options);
        GW.notificationCenter.addHandlerForEvent("Content.contentLoadDidFail", loadDidFailHandler, options);
    },

    load: (link, loadHandler = null, loadFailHandler = null, sourceURLsRemaining = null) => {
        GWLog("Content.load", "content.js", 2);

        sourceURLsRemaining = sourceURLsRemaining ?? Content.sourceURLsForLink(link);
        let sourceURL = sourceURLsRemaining?.shift();

        let processResponse = (response) => {
            let content = Content.contentFromLink?.(link) ?? Content.contentFromResponse?.(response, link, sourceURL);
            if (content?.document) {
                Content.cacheContentForLink(content, link);

                GW.notificationCenter.fireEvent("Content.contentDidLoad", {
                    link: link
                });
            } else if (content?.loadURLs) {
            	sourceURLsRemaining = sourceURLsRemaining ?? [ ];
            	sourceURLsRemaining.unshift(...(content.loadURLs));

				Content.load(link, null, null, sourceURLsRemaining);
				return;
            } else {
                Content.cacheContentForLink("LOADING_FAILED", link);

                GW.notificationCenter.fireEvent("Content.contentLoadDidFail", {
                    link: link
                });

                //  Send request to record failure in server logs.
                GWServerLogError(link.href + `--could-not-process`, "problematic content");
            }
        };

        if (   sourceURL == null
            || sourceURL.pathname == location.pathname) {
            processResponse();
        } else {
            doAjax({
                location: sourceURL.href,
                onSuccess: (event) => {
                	let permittedContentTypes = Content.contentTypeForLink(link).permittedContentTypes;
                	let httpContentType = event.target.getResponseHeader("Content-Type")?.match(/(.+?)(?:;|$)/)[1];
                	if (permittedContentTypes?.includes(httpContentType) == false) {
                        //  Send request to record failure in server logs.
                        GWServerLogError(link.href + `--bad-content-type`, "bad content type");

                        return;
                	}

                    processResponse(event.target.responseText);
                },
                onFailure: (event) => {
                    if (sourceURLsRemaining.length > 0) {
                        Content.load(link, null, null, sourceURLsRemaining);
                        return;
                    }

                    Content.cacheContentForLink("LOADING_FAILED", link);

                    GW.notificationCenter.fireEvent("Content.contentLoadDidFail", {
                        link: link
                    });

                    //  Send request to record failure in server logs.
                    GWServerLogError(link.href + `--missing-content`, "missing content");
                },
				headers: Content.contentTypeForLink(link).additionalAPIRequestHeaders
            });
        }

        //  Call any provided handlers, if/when appropriate.
        if (loadHandler || loadFailHandler)
            Content.waitForDataLoad(link, loadHandler, loadFailHandler);
    },

    contentFromLink: (link) => {
        return Content.contentTypeForLink(link)?.contentFromLink?.(link);
    },

    contentFromResponse: (response, link, sourceURL) => {
        return Content.contentTypeForLink(link)?.contentFromResponse?.(response, link, sourceURL);
    },

    /**************************************/
    /*  Reference data retrieval & caching.
     */

    cachedReferenceData: { },

	referenceDataCacheKeyForLink: (link) => {
		return (Content.contentTypeForLink(link)?.referenceDataCacheKeyForLink?.(link) ?? null);
	},

	cachedReferenceDataForLink: (link) => {
		let cacheKey = Content.referenceDataCacheKeyForLink(link);
		if (cacheKey)
			return Content.cachedReferenceData[cacheKey];

		return null;
	},

	cacheReferenceDataForLink: (referenceData, link) => {
		let cacheKey = Content.referenceDataCacheKeyForLink(link);
		if (cacheKey)
			Content.cachedReferenceData[cacheKey] = referenceData;
	},

    referenceDataForLink: (link) => {
        let content = Content.cachedContentForLink(link);
        if (   content == null
            || content == "LOADING_FAILED") {
            return content;
        } else {
			let referenceData = Content.cachedReferenceDataForLink(link);
			if (referenceData == null) {
				referenceData = Content.referenceDataFromContent(content, link);
				Content.cacheReferenceDataForLink(referenceData, link);
			}

			return referenceData;
        }
    },

    referenceDataFromContent: (content, link) => {
    	let contentType = Content.contentTypeForLink(link);
    	return (contentType.referenceDataFromContent
    			? contentType.referenceDataFromContent(content, link)
    			: { content: content.document });
    },

    /***********/
    /*  Helpers.
     */

	isContentTransformLink: (link) => {
		return ([ "tweet",
        		  "wikipediaEntry"
        		  ].findIndex(x => Content.contentTypes[x].matches(link)) !== -1);
	},

	shouldLocalizeContentFromLink: (link) => {
		let shouldLocalize = Content.referenceDataForLink(link)?.shouldLocalize;
		if (   shouldLocalize == true
			|| shouldLocalize == false)
			return shouldLocalize;

		if (Content.contentTypeForLink(link) == Content.contentTypes.localPage)
			return true;

		return false;
	},

    objectHTMLForURL: (url, options = { }) => {
        if (typeof url == "string")
            url = URLFromString(url);

        /*	PDF optional settings to embed more cleanly: fit width, and disable 
        	‘bookmarks’ & ‘thumbnails’ (just not enough space).

        	<https://gwern.net/doc/cs/css/2007-adobe-parametersforopeningpdffiles.pdf#page=6>
        	<https://github.com/mozilla/pdf.js/wiki/Viewer-options> 

        	WARNING: browsers are unreliable in whether they properly apply 
        	these options; Firefox appears to, but not Chrome, and there can be 
        	iframe issues as well.
         */
        let src = url.pathname.endsWith(".pdf")
                  ? url.href + (url.hash ? "&" : "#") + "view=FitH&pagemode=none"
                  : url.href;
        let cssClass = "loaded-not"
                     + (url.pathname.endsWith(".pdf")
                        ? " pdf"
                        : "");
                     + (options.additionalClasses ?? "")
        return `<iframe
                    src="${src}"
                    frameborder="0"
                    class="${cssClass}"
                    ${(options.additionalAttributes ?? "")}
                        ></iframe>`;
    },

    figcaptionHTMLForMediaLink: (link) => {
        let captionHTML = ``;
        if (Annotations.isAnnotatedLink(link))
            captionHTML = "<figcaption>" + synthesizeIncludeLink(link, {
                "class": "include-annotation include-strict",
                "data-include-selector": ".annotation-abstract > *",
                "data-include-selector-not": ".aux-links-append",
                "data-include-template": "annotation-blockquote-not"
            }).outerHTML + "</figcaption>";
        return captionHTML;
    },

    mediaDimensionsHTMLForMediaLink: (link) => {
        let parts = [ ];
        if (link.dataset.aspectRatio)
            parts.push(`data-aspect-ratio="${(link.dataset.aspectRatio)}"`);
        if (link.dataset.imageWidth)
            parts.push(`width="${(link.dataset.imageWidth)}"`);
        if (link.dataset.imageHeight)
            parts.push(`height="${(link.dataset.imageHeight)}"`);
        return parts.join(" ");
    },

    removeExtraneousClassesFromMediaElement: (media) => {
        //  Remove various link classes.
        media.classList.remove("icon-not", "link-page", "link-live",
            "link-annotated", "link-annotated-partial", "link-annotated-not",
            "has-annotation", "has-annotation-partial", "has-content",
            "has-icon", "has-indicator-hook", "spawns-popup", "spawns-popin");

        //  Remove all `include-` classes.
        media.classList.remove(...(Array.from(media.classList).filter(x => x.startsWith("include-"))));
    },

    /**************************************************************/
    /*  CONTENT TYPES

        Each content type definition has the following REQUIRED members:

            .matches(URL|Element) => boolean

            .isSliceable: boolean

				This property determines whether content documents returned for 
				links of this content type may be “sliced”, via element IDs, 
				selectors, or by other means. If its value is false, then the 
				returned content documents may only be transcluded in their 
				entirety.

        ... plus either these two:

            .sourceURLsForLink(URL|Element) => [ URL ]

            .contentFromResponse(string, URL|Element, URL) => object

        ... or this one:

            .contentFromLink(URL|Element) => object

        A content type definition may also have the following OPTIONAL members:

			.contentCacheKeyForLink(URL|Element) => string

				If this member function is not present, a default content cache
				key (based on the first source URL for the link, or else the URL
				of the link itself) will be used.

            .referenceDataFromContent(object, URL|Element) => object

				NOTE: If this member function is not present, we must ensure 
				that the object returned from .contentFromResponse() or 
				.contentFromLink() has a .document member. (This should be a
				DocumentFragment object which contains the primary content for
				the link.)

			.referenceDataCacheKeyForLink(URL|Element) => string

				NOTE: If this member function is not present, then reference 
				data will not be cached for links of this content type.
     */

    contentTypeForLink: (link) => {
		if (link.dataset.linkContentType) {
			let contentTypeName = link.dataset.linkContentType.replace(/([a-z])-([a-z])/g, (match, p1, p2) => (p1 + p2.toUpperCase()));
			let contentType = Content.contentTypes[contentTypeName];
			if (contentType?.matches(link))
				return contentType;
		}

        for (let [ contentTypeName, contentType ] of Object.entries(Content.contentTypes))
            if (contentType.matches(link))
                return contentType;

        return null;
    },

    contentTypes: {
        dropcapInfo: {
            matches: (link) => {
                return link.classList.contains("link-dropcap");
            },

            isSliceable: false,

            contentFromLink: (link) => {
                let letter = link.dataset.letter;
                let dropcapType = link.dataset.dropcapType;

                let contentDocument = newDocument(
                      `<p>A capital letter <strong>${letter}</strong> dropcap initial, from the `
                    + `<a class="link-page" href="/dropcap#${dropcapType}"><strong>${dropcapType}</strong></a>`
                    + ` dropcap font.</p>`
                );

                //  Fire contentDidLoad event.
                GW.notificationCenter.fireEvent("GW.contentDidLoad", {
                    source: "Content.contentTypes.dropcapInfo.load",
                    container: content,
                    document: content,
                    loadLocation: new URL(link.href)
                });

                return {
                	document: contentDocument
                };
            }
        },

        foreignSite: {
            matches: (link) => {
                //  Some foreign-site links are handled specially.
                if ([ "tweet",
                	  "wikipediaEntry",
                      "remoteVideo",
                      "remoteImage"
                      ].findIndex(x => Content.contentTypes[x].matches(link)) !== -1)
                    return false;

                //  Account for alternate and archive URLs.
                let url = URLFromString(link.dataset.urlArchive ?? link.dataset.urlHtml ?? link.href);

                return (   url.hostname != location.hostname
                        && link.classList.contains("link-live"));
            },

            isSliceable: false,

            contentFromLink: (link) => {
                //  WARNING: EXPERIMENTAL FEATURE!
//              if (localStorage.getItem("enable-embed-proxy") == "true") {
//                  let url = URLFromString(embedSrc);
//                  let proxyURL = URLFromString("https://api.obormot.net/embed.php");
//                  doAjax({
//                      location: proxyURL.href,
//                      params: { url: url.href },
//                      onSuccess: (event) => {
//                          if (Extracts.popFrameProvider.isSpawned(target.popFrame) == false)
//                              return;
//
//                          let doc = newElement("DIV", null, { "innerHTML": event.target.responseText });
//                          doc.querySelectorAll("[href], [src]").forEach(element => {
//                              if (element.href) {
//                                  let elementURL = URLFromString(element.href);
//                                  if (   elementURL.host == location.host
//                                      && !element.getAttribute("href").startsWith("#")) {
//                                      elementURL.host = url.host;
//                                      element.href = elementURL.href;
//                                  }
//                              } else if (element.src) {
//                                  let elementURL = URLFromString(element.src);
//                                  if (elementURL.host == location.host) {
//                                      elementURL.host = url.host;
//                                      element.src = elementURL.href;
//                                  }
//                              }
//                          });
//
//                          if (event.target.getResponseHeader("content-type").startsWith("text/plain"))
//                              doc.innerHTML = `<pre>${doc.innerHTML}</pre>`;
//
//                          target.popFrame.document.querySelector("iframe").srcdoc = doc.innerHTML;
//
//                          Extracts.postRefreshUpdatePopFrameForTarget(target, true);
//                      },
//                      onFailure: (event) => {
//                          if (Extracts.popFrameProvider.isSpawned(target.popFrame) == false)
//                              return;
//
//                          Extracts.postRefreshUpdatePopFrameForTarget(target, false);
//                      }
//                  });
//
//                  return newDocument(`<iframe frameborder="0" sandbox="allow-scripts allow-popups"></iframe>`);
//              }
                //  END EXPERIMENTAL SECTION

                let embedSrc = link.dataset.urlArchive ?? link.dataset.urlHtml ?? link.href;
                let additionalAttributes = [ ];

                //  Determine sandbox settings.
                additionalAttributes.push(Content.contentTypes.foreignSite.shouldEnableScriptsForURL(URLFromString(embedSrc))
                                          ? `sandbox="allow-scripts allow-same-origin"`
                                          : `sandbox`);

				let contentDocument = newDocument(Content.objectHTMLForURL(embedSrc, {
                    additionalAttributes: additionalAttributes.join(" ")
                }));

                return {
                	document: contentDocument
                };
            },

            shouldEnableScriptsForURL: (url) => {
                if (url.hostname == "docs.google.com")
                    return true;

                if (url.hostname == "demos.obormot.net")
                    return true;

                if (   url.hostname == "archive.org"
                    && url.pathname.startsWith("/details/"))
                    return true;

                return false;
            }
        },

		wikipediaEntry: {
			/*	The Wikipedia API only gives usable responses for most, not all,
				Wikipedia URLs.
			 */
			matches: (link) => {
				return (   link.classList.contains("content-transform-not") == false
						&& /(.+?)\.wikipedia\.org/.test(link.hostname)
						&& link.pathname.startsWith("/wiki/")
						&& link.pathname.startsWithAnyOf(_π("/wiki/", [ "File:", "Category:", "Special:", "Wikipedia:Wikipedia_Signpost" ])) == false);
			},

			isSliceable: false,

			sourceURLsForLink: (link) => {
				let apiRequestURL = URLFromString(link.href);

				let wikiPageName = fixedEncodeURIComponent(/\/wiki\/(.+?)$/.exec(decodeURIComponent(apiRequestURL.pathname))[1]);
				apiRequestURL.pathname = `/api/rest_v1/page/html/${wikiPageName}`;
				apiRequestURL.hash = "";

				return [ apiRequestURL ];
			},

            contentFromResponse: (response, link, sourceURL) => {
				let contentDocument = newDocument(response);
				let redirectLink = contentDocument.querySelector("link[rel='mw:PageProp/redirect']");
				if (redirectLink) {
					return {
						loadURLs: Content.contentTypes.wikipediaEntry.sourceURLsForLink(modifiedURL(link, {
							pathname: "/wiki" + redirectLink.getAttribute("href").slice(1)
						}))
					}
				} else {
					return {
						document: contentDocument
					};
				}
            },

			referenceDataCacheKeyForLink: (link) => {
				return link.href;
			},

			referenceDataFromContent: (wikipediaEntryContent, articleLink) => {
				//	Article link.
				let titleLinkHref = articleLink.href;

				//	We use the mobile URL for popping up the live-link.
				let titleLinkHrefForEmbedding = modifiedURL(articleLink, {
					hostname: articleLink.hostname.replace(".wikipedia.org", ".m.wikipedia.org")
				}).href;
				let titleLinkDataAttributes = `data-url-html="${titleLinkHrefForEmbedding}"`;

				//	Do not show the whole page, by default.
				let wholePage = false;

				//	Show full page (sans TOC) if it’s a disambiguation page.
				if (wikipediaEntryContent.document.querySelector("meta[property='mw:PageProp/disambiguation']") != null) {
					wholePage = true;

					//	Send request to record failure in server logs.
					GWServerLogError(Content.contentTypes.wikipediaEntry.sourceURLsForLink(articleLink).first.href + `--disambiguation-error`, "disambiguation page");
				}

				let pageTitleElementHTML = unescapeHTML(wikipediaEntryContent.document.querySelector("title").innerHTML);
				let entryContentHTML, titleHTML, fullTitleHTML, secondaryTitleLinksHTML;
				if (wholePage) {
					entryContentHTML = wikipediaEntryContent.document.innerHTML;
					titleHTML = pageTitleElementHTML;
					fullTitleHTML = pageTitleElementHTML;
				} else if (articleLink.hash > "") {
					let targetElement = wikipediaEntryContent.document.querySelector(selectorFromHash(articleLink.hash));

					/*	Check whether we have tried to load a part of the page which
						does not exist.
					 */
					if (targetElement == null) {
						titleHTML = titleLinkHref;
						fullTitleHTML = titleLinkHref;

						//	No entry content, because the target was not found.
					} else if (/H[0-9]/.test(targetElement.tagName)) {
						//	The target is a section heading.
						let targetHeading = targetElement;
	
						//	The id is on the heading, so the section is its parent.
						let targetSection = targetHeading.parentElement.cloneNode(true);

						//	Excise heading.
						targetHeading = targetSection.firstElementChild;
						targetHeading.remove();

						//	Content sans heading.
						entryContentHTML = targetSection.innerHTML;

						//	Unwrap or delete links, but save them for inclusion in the template.
						secondaryTitleLinksHTML = "";
						//	First link is the section title itself.
						targetHeading.querySelectorAll("a:first-of-type").forEach(link => {
							//  Process link, save HTML, unwrap.
							Content.contentTypes.wikipediaEntry.qualifyWikipediaLink(link, articleLink);
							Content.contentTypes.wikipediaEntry.designateWikiLink(link);
							secondaryTitleLinksHTML += link.outerHTML;
							unwrap(link);
						});
						//	Additional links are other things, who knows what.
						targetHeading.querySelectorAll("a").forEach(link => {
							//  Process link, save HTML, delete.
							Content.contentTypes.wikipediaEntry.qualifyWikipediaLink(link, articleLink);
							Content.contentTypes.wikipediaEntry.designateWikiLink(link);
							secondaryTitleLinksHTML += link.outerHTML;
							link.remove();
						});
						if (secondaryTitleLinksHTML > "")
							secondaryTitleLinksHTML = ` (${secondaryTitleLinksHTML})`;

						//	Cleaned section title.
						titleHTML = targetHeading.innerHTML;
						fullTitleHTML = `${titleHTML} (${pageTitleElementHTML})`;
					} else {
						//	The target is something else.
						entryContentHTML = Transclude.blockContext(targetElement, articleLink).innerHTML;
						titleHTML = articleLink.hash;
					}
				} else {
					entryContentHTML = wikipediaEntryContent.document.querySelector("[data-mw-section-id='0']").innerHTML;
					titleHTML = pageTitleElementHTML;
					fullTitleHTML = pageTitleElementHTML;

					//	Build TOC.
					let sections = Array.from(wikipediaEntryContent.document.querySelectorAll("section")).slice(1);
					if (   sections 
						&& sections.length > 0) {
						entryContentHTML += `<div class="TOC columns">`;
						let headingLevel = 0;
						for (let i = 0; i < sections.length; i++) {
							let section = sections[i];
							let headingElement = section.firstElementChild;
							let newHeadingLevel = parseInt(headingElement.tagName.slice(1));
							if (newHeadingLevel > headingLevel)
								entryContentHTML += `<ul>`;

							if (   i > 0 
								&& newHeadingLevel <= headingLevel)
								entryContentHTML += `</li>`;

							if (newHeadingLevel < headingLevel)
								entryContentHTML += `</ul>`;

							//	We must encode, because the anchor might contain quotes.
							let urlEncodedAnchor = fixedEncodeURIComponent(headingElement.id);

							//	Get heading, parse as HTML, and unwrap links.
							let heading = headingElement.cloneNode(true);
							heading.querySelectorAll("a").forEach(unwrap);

							//	Construct TOC entry.
							entryContentHTML += `<li><a href='${articleLink}#${urlEncodedAnchor}'>${(heading.innerHTML)}</a>`;

							headingLevel = newHeadingLevel;
						}
						entryContentHTML += `</li></ul></div>`;
					}
				}

				let contentDocument = newDocument(entryContentHTML);

				//	Post-process entry content.
				Content.contentTypes.wikipediaEntry.postProcessEntryContent(contentDocument, articleLink);

				//	Request image inversion judgments from invertornot.
				requestImageInversionDataForImagesInContainer(contentDocument);

				//	Pull out initial figure.
				let thumbnailFigureHTML = null;
				if (GW.mediaQueries.mobileWidth.matches == false) {
					let initialFigure = contentDocument.querySelector("figure.float-right:first-child");
					if (initialFigure) {
						thumbnailFigureHTML = initialFigure.outerHTML;
						initialFigure.remove();
					}
				}

				entryContentHTML = contentDocument.innerHTML;

				//	Pop-frame title text. Mark sections with ‘§’ symbol.
				let popFrameTitleHTML = (articleLink.hash > ""
										 ? (fullTitleHTML
											? `${pageTitleElementHTML} &#x00a7; ${titleHTML}`
											: `${titleHTML} (${pageTitleElementHTML})`)
										 : titleHTML);
				let popFrameTitleText = newElement("SPAN", null, { innerHTML: popFrameTitleHTML }).textContent;

				return {
					content: {
						title:                    fullTitleHTML,
						titleLinkHref:            titleLinkHref,
						titleLinkClass:           `title-link link-live content-transform-not`,
						titleLinkIconMetadata:    `data-link-icon-type="svg" data-link-icon="wikipedia"`,
						titleLinkDataAttributes:  titleLinkDataAttributes,
						secondaryTitleLinksHTML:  secondaryTitleLinksHTML,
						entryContent: 		      entryContentHTML,
						thumbnailFigure:          thumbnailFigureHTML
					},
					contentTypeClass:               "wikipedia-entry",
					template:                       "wikipedia-entry-blockquote-inside",
					linkTarget:                     (GW.isMobile() ? "_self" : "_blank"),
					whichTab:                       (GW.isMobile() ? "current" : "new"),
					tabOrWindow:                    (GW.isMobile() ? "tab" : "window"),
					popFrameTemplate:               "wikipedia-entry-blockquote-not",
					popFrameTitleText:              popFrameTitleText,
					popFrameTitleLinkHref:          titleLinkHref,
					annotationFileIncludeTemplate:  "wikipedia-entry-blockquote-title-not"
				};
			},

			additionalAPIRequestHeaders: {
				"Accept": 'text/html; charset=utf-8; profile="https://www.mediawiki.org/wiki/Specs/HTML/2.1.0"'
			},

			/*	Qualify a link in a Wikipedia article.
			 */
			qualifyWikipediaLink: (link, hostArticleLink) => {
				if (link.getAttribute("href") == null)
					return;

				//  Qualify link.
				if (link.matches([
						"a[rel='mw:WikiLink']",
						"a[rel='mw:referencedBy']",
						"span[rel='mw:referencedBy'] a",
						"sup.mw-ref a",
						].join(", ")))
					link.pathname = "/wiki" + link.getAttribute("href").slice(1);
				if (link.getAttribute("href").startsWith("#"))
					link.pathname = hostArticleLink.pathname;
				if (link.hostname == location.hostname)
					link.hostname = hostArticleLink.hostname;
				if (   link.hostname == hostArticleLink.hostname
					&& link.pathname.startsWith("/wiki/") == false
					&& link.pathname.startsWith("/api/") == false)
					link.pathname = "/wiki" + link.pathname;
			},

			/*	Mark a wiki-link appropriately, as annotated, or live, or neither.
			 */
			designateWikiLink: (link) => {
				if (/(.+?)\.wikipedia\.org/.test(link.hostname)) {
					if (Content.contentTypes.wikipediaEntry.matches(link)) {
						link.classList.add("content-transform");
					} else {
						if ((   link.pathname.startsWith("/wiki/Special:")
							 || link.pathname == "/w/index.php"
							 ) == false)
							link.classList.add("link-live");
					}
				}
			},

			/*  Elements to excise from a Wikipedia entry.
			 */
			extraneousElementSelectors: [
				"style",
		// 		".mw-ref",
				".shortdescription",
				"td hr",
				".hatnote",
				".portal",
				".penicon",
		// 		".reference",
				".Template-Fact",
				".error",
				".mwe-math-mathml-inline",
				".mwe-math-mathml-display",
				".sidebar",
				".ambox",
				".unicode.haudio",
		// 		"span[typeof='mw:File']",
			],

			/*  CSS properties to preserve when stripping inline styles.
			 */
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

			/*  Post-process an already-constructed content-transformed
				Wikipedia entry (do HTML cleanup, etc.).
			 */
			postProcessEntryContent: (contentDocument, articleLink) => {
				//  Remove unwanted elements.
				contentDocument.querySelectorAll(Content.contentTypes.wikipediaEntry.extraneousElementSelectors.join(", ")).forEach(element => {
					element.remove();
				});

				//	Clean empty nodes.
				contentDocument.childNodes.forEach(node => {
					if (isNodeEmpty(node))
						node.remove();
				});

				//  Remove location maps (they don’t work right).
				contentDocument.querySelectorAll(".locmap").forEach(locmap => {
					(locmap.closest("tr") ?? locmap).remove();
				});

				//	Remove other maps.
				contentDocument.querySelectorAll("img").forEach(image => {
					let imageSourceURL = URLFromString(image.src);
					if (imageSourceURL.hostname == "maps.wikimedia.org")
						image.remove();
				});

				//  Remove empty paragraphs.
				contentDocument.querySelectorAll("p:empty").forEach(emptyGraf => {
					emptyGraf.remove();
				});

				//	Remove edit-links.
				contentDocument.querySelectorAll("a[title^='Edit this on Wiki'], a[title^='Edit this at Wiki']").forEach(editLink => {
					editLink.remove();
				});

				//  Process links.
				contentDocument.querySelectorAll("a").forEach(link => {
					//	De-linkify non-anchor self-links.
					if (   link.hash     == ""
						&& link.pathname == articleLink.pathname) {
						unwrap(link);
						return;
					}

					//  Qualify links.
					Content.contentTypes.wikipediaEntry.qualifyWikipediaLink(link, articleLink);

					//  Mark other Wikipedia links as also being annotated.
					Content.contentTypes.wikipediaEntry.designateWikiLink(link);

					//  Mark self-links (anchorlinks within the same article).
					if (link.pathname == articleLink.pathname)
						link.classList.add("link-self");
				});

				//	Prevent layout weirdness for footnote links.
				contentDocument.querySelectorAll("a[href*='#cite_note-']").forEach(citationLink => {
					citationLink.classList.add("icon-not");
					citationLink.innerHTML = "&NoBreak;" + citationLink.textContent.trim();
				});

				//	Rectify back-to-citation links in “References” sections.
				contentDocument.querySelectorAll("a[rel='mw:referencedBy']").forEach(backToCitationLink => {
					backToCitationLink.classList.add("icon-not");
					backToCitationLink.classList.add("wp-footnote-back");
					backToCitationLink.innerHTML = backToCitationLink.textContent.trim();
				});

				//	Strip inline styles and some related attributes.
				let tableElementsSelector = "table, thead, tfoot, tbody, tr, th, td";
				contentDocument.querySelectorAll("[style]").forEach(styledElement => {
					//	Skip table elements; we handle those specially.
					if (styledElement.matches(tableElementsSelector))
						return;

					if (styledElement.style.display != "none")
						stripStyles(styledElement, { saveProperties: Content.contentTypes.wikipediaEntry.preservedInlineStyleProperties });
				});
				//	Special handling for table elements.
				contentDocument.querySelectorAll(tableElementsSelector).forEach(tableElement => {
					if (tableElement.style.display != "none") {
						if (tableElement.style.position == "relative")
							stripStyles(tableElement, { saveProperties: [ "text-align", "position", "width", "height" ] });
						else
							stripStyles(tableElement, { saveProperties: [ "text-align" ] });
					}

					[ "width", "height", "align" ].forEach(attribute => {
						tableElement.removeAttribute(attribute);
					});
				});

				//  Rectify table classes.
				contentDocument.querySelectorAll("table.sidebar").forEach(table => {
					table.classList.toggle("infobox", true);
				});

				//  Normalize table cell types.
				contentDocument.querySelectorAll("th:not(:only-child)").forEach(cell => {
					let rowSpan = (cell.rowSpan > 1) ? ` rowspan="${cell.rowSpan}"` : ``;
					let colSpan = (cell.colSpan > 1) ? ` colspan="${cell.colSpan}"` : ``;
					cell.outerHTML = `<td${rowSpan}${colSpan}>${cell.innerHTML}</td>`;
				});

				//  Un-linkify images.
				contentDocument.querySelectorAll("a img").forEach(linkedImage => {
					let enclosingLink = linkedImage.closest("a");
					enclosingLink.parentElement.replaceChild(linkedImage, enclosingLink);
				});

				//	Fix chemical formulas.
				contentDocument.querySelectorAll(".chemf br").forEach(br => {
					br.remove();
				});

				//	Rectify quoteboxes.
				contentDocument.querySelectorAll("div.quotebox").forEach(quotebox => {
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
				let thumbnail = contentDocument.querySelector("img");
				let thumbnailContainer;
				if (thumbnail)
					thumbnailContainer = thumbnail.closest(".infobox-image, .thumb");
				if (   thumbnail
					&& thumbnailContainer
					&& thumbnailContainer.closest(".gallery") == null) {
					while ([ "TR", "TD", "TH" ].includes(thumbnailContainer.tagName))
						thumbnailContainer = thumbnailContainer.parentElement;

					//  Create the figure and move the thumbnail(s) into it.
					let figure = newElement("FIGURE", { "class": "thumbnail float-right" });
					thumbnailContainer.querySelectorAll(".infobox-image img, .thumb img").forEach(image => {
						if (image.closest("figure") == figure)
							return;

						let closestRow = image.closest("tr, .trow, [style*='display: table-row']");
						if (closestRow == null)
							return;

						let allImagesInRow = closestRow.querySelectorAll("img");
						if (allImagesInRow.length > 1) {
							let rowWrapper = newElement("SPAN", { "class": "image-row-wrapper" });
							rowWrapper.append(...allImagesInRow);
							figure.append(rowWrapper);
						} else {
							figure.append(allImagesInRow[0]);
						}

						closestRow.remove();
					});

					//  Create the caption, if need be.
					let caption = contentDocument.querySelector(".mw-default-size + div, .infobox-caption, .thumbcaption");
					if (   caption
						&& caption.textContent > "") {
						figure.appendChild(newElement("FIGCAPTION", null, { "innerHTML": caption.innerHTML }));

						let closestRow = caption.closest("tr, .trow, [style*='display: table-row']");
						if (closestRow)
							closestRow.remove();
					}

					//  Insert the figure as the first child of the entry.
					contentDocument.insertBefore(figure, contentDocument.firstElementChild);

					//  Rectify classes.
					thumbnailContainer.closest("table")?.classList.toggle("infobox", true);
				} else if (   thumbnail
						   && thumbnail.closest("figure")) {
					let figure = thumbnail.closest("figure");

					//  Insert the figure as the first child of the entry.
					contentDocument.insertBefore(figure, contentDocument.firstElementChild);
					figure.classList.add("thumbnail", "float-right");

					let caption = figure.querySelector("figcaption");
					if (caption.textContent == "")
						caption.remove();
				}

				//	Rewrite other figures.
				contentDocument.querySelectorAll("div.thumb").forEach(figureBlock => {
					let figure = newElement("FIGURE");

					let images = figureBlock.querySelectorAll("img");
					if (images.length == 0)
						return;

					images.forEach(image => {
						figure.appendChild(image);
					});

					figure.appendChild(newElement("FIGCAPTION", null, { "innerHTML": figureBlock.querySelector(".thumbcaption")?.innerHTML }));

					figureBlock.parentNode.insertBefore(figure, figureBlock);
					figureBlock.remove();
				});

				//	Float all figures right.
				contentDocument.querySelectorAll("figure").forEach(figure => {
					figure.classList.add("float-right");
				});

				//	Mark certain images as not to be wrapped in figures.
				let noFigureImagesSelector = [
					".mwe-math-element",
					".mw-default-size",
					".sister-logo",
					".side-box-image",
					"p"
				].map(selector => `${selector} img`).join(", ");
				contentDocument.querySelectorAll(noFigureImagesSelector).forEach(image => {
					image.classList.add("figure-not");
				});

				//	Clean up math elements.
				unwrapAll(".mwe-math-element", { root: contentDocument });
				contentDocument.querySelectorAll("dl dd .mwe-math-fallback-image-inline").forEach(inlineButReallyBlockMathElement => {
					//	Unwrap the <dd>.
					unwrap(inlineButReallyBlockMathElement.parentElement);
					//	Unwrap the <dl>.
					unwrap(inlineButReallyBlockMathElement.parentElement);
					//	Rectify class.
					inlineButReallyBlockMathElement.swapClasses([ "mwe-math-fallback-image-inline", "mwe-math-fallback-image-display" ], 1);
				});
				wrapAll(".mwe-math-fallback-image-display", "div.wikipedia-math-block-wrapper", { root: contentDocument });
				wrapAll(".mwe-math-fallback-image-inline", "span.wikipedia-math-inline-wrapper", { root: contentDocument });

				//	Move infoboxes out of the way.
				let childElements = Array.from(contentDocument.children);
				let firstInfoboxIndex = childElements.findIndex(x => x.matches(".infobox"));
				if (firstInfoboxIndex !== -1) {
					let firstInfobox = childElements[firstInfoboxIndex];
					let firstGrafAfterInfobox = childElements.slice(firstInfoboxIndex).find(x => x.matches("p"));
					if (firstGrafAfterInfobox)
						contentDocument.insertBefore(firstGrafAfterInfobox, firstInfobox);
					wrapElement(firstInfobox, ".collapse");
				}

				//	Apply section classes.
				contentDocument.querySelectorAll("section").forEach(section => {
					if (/[Hh][1-9]/.test(section.firstElementChild.tagName))
						section.classList.add("level" + section.firstElementChild.tagName.slice(1));
				});

				//	Paragraphize note-boxes, if any (e.g., disambiguation notes).
				contentDocument.querySelectorAll(".dmbox-body").forEach(noteBox => {
					paragraphizeTextNodesOfElement(noteBox);
					noteBox.parentElement.classList.add("admonition", "tip");
				});

				//	Clean empty nodes, redux.
				contentDocument.childNodes.forEach(node => {
					if (isNodeEmpty(node))
						node.remove();
				});
			}
		},

        tweet: {
            matches: (link) => {
                return (   link.classList.contains("content-transform-not") == false
						&& [ "twitter.com", "x.com" ].includes(link.hostname)
                        && link.pathname.match(/\/.+?\/status\/[0-9]+$/) != null);
            },

            isSliceable: false,

			contentCacheKeyForLink: (link) => {
				return link.href;
			},

            sourceURLsForLink: (link) => {
                let urls = [ ];

                if (link.dataset.urlArchive)
                    urls.push(URLFromString(link.dataset.urlArchive));

                if (link.dataset.urlHtml)
                    urls.push(URLFromString(link.dataset.urlHtml));

                return urls.concat(Content.contentTypes.tweet.liveNitterHosts.map(nitterHost => {
                    let nitterURL = URLFromString(link.href);
                    nitterURL.hostname = nitterHost;
                    return nitterURL;
                }));
            },

            contentFromResponse: (response, link, sourceURL) => {
                return {
                    document: newDocument(response)
                };
            },

			referenceDataCacheKeyForLink: (link) => {
				return link.href;
			},

            referenceDataFromContent: (tweetContent, link) => {
            	//	Nitter host.
                let nitterHost = Content.contentTypes.tweet.getNitterHost();

                //  Class and link icon for link to user’s page.
                let authorLinkClass = "author-link";
                let authorLinkIconMetadata = `data-link-icon-type="svg" data-link-icon="twitter"`;

                //  URL for link to user’s page.
                let authorLinkURL = URLFromString(tweetContent.document.querySelector(".main-tweet a.username").href);
                authorLinkURL.hostname = nitterHost;
                let authorLinkHref = authorLinkURL.href;

                //  Avatar.
                let avatarImgElement = tweetContent.document.querySelector(".main-tweet img.avatar").cloneNode(true);
                let avatarImgSrc = avatarImgElement.getAttribute("src");
                if (avatarImgSrc.startsWith("data:image/svg+xml")) {
                    avatarImgElement.setAttribute("style", avatarImgElement.getAttribute("style")
                                                           + ";"
                                                           + tweetContent.document.querySelector("style").innerHTML.match(/:root\{(.+?)\}/)[1]);
                    let avatarImgSrcVar = avatarImgElement.style.getPropertyValue("background-image").match(/var\((.+?)\)/)[1];
                    avatarImgSrc = avatarImgElement.style.getPropertyValue(avatarImgSrcVar).match(/url\("(.+?)"\)/)[1];
                }
                let avatarImg = newElement("IMG", { src: avatarImgSrc, class: "avatar figure-not" });

                //  Text of link to user’s page.
                let authorLinkParts = tweetContent.document.querySelector("title").textContent.match(/^(.+?) \((@.+?)\):/);
                let authorPlusAvatarHTML = `${avatarImg.outerHTML}“${authorLinkParts[1]}” (<code>${authorLinkParts[2]}</code>)`;

				//	Class and link icon for link to tweet.
                let tweetLinkClass = "tweet-link" + (link.dataset.urlArchive ? " link-live" : "");
                let tweetLinkIconMetadata = authorLinkIconMetadata;

                //  URL for link to tweet.
                let tweetLinkURL = URLFromString(link.href);
                tweetLinkURL.hostname = nitterHost;
                tweetLinkURL.hash = "m";

				//	Data attribute for archived tweet (if available).
                let archivedTweetURLDataAttribute = link.dataset.urlArchive 
                									? `data-url-archive="${(URLFromString(link.dataset.urlArchive).href)}"` 
                									: "";
				//	Text of link to tweet.
                let tweetDate = new Date(Date.parse(tweetContent.document.querySelector(".main-tweet .tweet-date").textContent));
                let tweetDateString = ("" + tweetDate.getFullYear())
                                    + "-"
                                    + ("" + tweetDate.getMonth()).padStart(2, '0')
                                    + "-"
                                    + ("" + tweetDate.getDate()).padStart(2, '0');

                //  Main tweet content.
                let tweetContentHTML = tweetContent.document.querySelector(".main-tweet .tweet-content").innerHTML.split("\n\n").map(graf => `<p>${graf}</p>`).join("\n");

				//	Request image inversion judgments from invertOrNot.
				requestImageInversionDataForImagesInContainer(newDocument(tweetContentHTML));

                //  Attached media (video or images).
                tweetContentHTML += Content.contentTypes.tweet.mediaEmbedHTML(tweetContent.document);

                //  Pop-frame title text.
                let popFrameTitleText = `${authorPlusAvatarHTML} on ${tweetDateString}`;

                return {
                    content: {
                        authorLinkClass:                authorLinkClass,
                        authorLinkHref:                 authorLinkURL.href,
                        authorLinkIconMetadata:         authorLinkIconMetadata,
                        authorPlusAvatar:               authorPlusAvatarHTML,
                        tweetLinkClass:                 tweetLinkClass,
                        tweetLinkHref:                  tweetLinkURL.href,
                        tweetLinkIconMetadata:          tweetLinkIconMetadata,
                        archivedTweetURLDataAttribute:  archivedTweetURLDataAttribute,
                        tweetDate:                      tweetDateString,
                        tweetContent:                   tweetContentHTML
                    },
                    contentTypeClass:       "tweet",
                    template:               "tweet-blockquote-outside",
                    linkTarget:             (GW.isMobile() ? "_self" : "_blank"),
                    whichTab:               (GW.isMobile() ? "current" : "new"),
                    tabOrWindow:            (GW.isMobile() ? "tab" : "window"),
					popFrameTemplate:       "tweet-blockquote-not",
                    popFrameTitleText:      popFrameTitleText,
                    popFrameTitleLinkHref:  tweetLinkURL.href,
                };
            },

            mediaURLFromMetaTag: (mediaMetaTag, nitterHost) => {
                let mediaURL = URLFromString(mediaMetaTag.content);
                mediaURL.hostname = nitterHost;
                return mediaURL;
            },

            mediaEmbedHTML: (tweetDoc) => {
                let attachments = tweetDoc.querySelector(".main-tweet .attachments");
                if (attachments) {
                    let mediaHTML = ``;
                    attachments.querySelectorAll("img, video").forEach(mediaElement => {
                        mediaHTML += `<figure>${mediaElement.outerHTML}</figure>`;
                    });

                    return mediaHTML;
                } else {
                    return "";
                }
            },

            liveNitterHosts: [
                "nitter.poast.org"
            ],

            getNitterHost: () => {
                let hosts = Content.contentTypes.tweet.liveNitterHosts;
                return hosts[rollDie(hosts.length) - 1];
            }
        },

        localCodeFile: {
            matches: (link) => {
                //  Maybe it’s a foreign link?
                if (link.hostname != location.hostname)
                    return false;

                //  Maybe it’s an aux-links link?
                if (link.pathname.startsWith("/metadata/"))
                    return false;

                //  Maybe it’s a local document link?
                if (   link.pathname.startsWith("/doc/www/")
                    || (   link.pathname.startsWith("/doc/")
                        && link.pathname.match(/\.(html|pdf)$/i) != null))
                    return false;

                return link.pathname.endsWithAnyOf(Content.contentTypes.localCodeFile.codeFileExtensions.map(x => `.${x}`));
            },

            isSliceable: false,

            /*  We first try to retrieve a syntax-highlighted version of the
                given code file, stored on the server as an HTML fragment. If
                present, we embed that. If there’s no such fragment, then we
                just embed the contents of the actual code file, in a
                <pre>-wrapped <code> element.
             */
            sourceURLsForLink: (link) => {
                let codeFileURL = URLFromString(link.href);
                codeFileURL.hash = "";
                codeFileURL.search = "";

                let syntaxHighlightedCodeFileURL = URLFromString(codeFileURL.href);
                syntaxHighlightedCodeFileURL.pathname += ".html";

                return [ syntaxHighlightedCodeFileURL, codeFileURL ];
            },

            contentFromResponse: (response, link, sourceURL) => {
                let contentDocument;

                //  Parse (encoding and wrapping first, if need be).
                if (sourceURL.pathname == link.pathname + ".html") {
                    //  Syntax-highlighted code (already HTML-encoded).
                    contentDocument = newDocument(response);

                    //  We want <body> contents only, no metadata and such.
                    let nodes = Array.from(contentDocument.childNodes);
                    let codeWrapper = contentDocument.querySelector("div.sourceCode");
                    contentDocument.replaceChildren(...(nodes.slice(nodes.indexOf(codeWrapper))));

                    //  Handle truncated syntax-highlighted code files.
                    if (codeWrapper.nextElementSibling?.tagName == "P") {
                        codeWrapper.classList.add("truncated");

                        let truncationNotice = codeWrapper.nextElementSibling;
                        truncationNotice.classList.add("truncation-notice");
                        truncationNotice.querySelector("a").classList.add("extract-not");

                        codeWrapper.append(truncationNotice);
                    }

                    //  Set ‘line’ class and fix blank lines.
                    Array.from(contentDocument.querySelector("code").children).forEach(lineSpan => {
                        lineSpan.classList.add("line");
                        if (lineSpan.innerHTML.length == 0)
                            lineSpan.innerHTML = "&nbsp;";
                    });
                } else {
                    //  “Raw” code.
                    let htmlEncodedResponse = response.replace(
                        /[<>]/g,
                        c => ('&#' + c.charCodeAt(0) + ';')
                    ).split("\n").map(
                        line => (`<span class="line">${(line || "&nbsp;")}</span>`)
                    ).join("\n");
                    contentDocument = newDocument(  `<div class="sourceCode">`
                                          + `<pre class="raw-code"><code>`
                                          + htmlEncodedResponse
                                          + `</code></pre>`
                                          + `</div>`);
                }

                return {
                	document: contentDocument
                };
            },

            codeFileExtensions: [
                //  Truncated at 2000 lines for preview.
                "bash", "c", "conf", "css", "diff", "hs", "html", "js",
                "json", "jsonl", "md", "opml", "patch", "php", "py", "R",
                "sh", "xml",
                //  Non-syntax highlighted (due to lack of known format), but truncated:
                "txt"
            ]
        },

        localFragment: {
            matches: (link) => {
                //  Maybe it’s a foreign link?
                if (link.hostname != location.hostname)
                    return false;

                return (   link.pathname.startsWith("/metadata/")
                        && link.pathname.endsWith(".html"));
            },

            isSliceable: true,

            sourceURLsForLink: (link) => {
                let url = URLFromString(link.href);
                url.hash = "";
                url.search = "";

                return [ url ];
            },

            contentFromResponse: (response, link, sourceURL) => {
                let contentDocument = newDocument(response);

                let auxLinksLinkType = AuxLinks.auxLinksLinkType(sourceURL);
                if (auxLinksLinkType) {
                    let auxLinksList = contentDocument.querySelector("ul, ol");
                    if (auxLinksList) {
                        auxLinksList.classList.add("aux-links-list", auxLinksLinkType + "-list");
                        auxLinksList.previousElementSibling.classList.add("aux-links-list-label", auxLinksLinkType + "-list-label");

                        if (auxLinksLinkType == "backlinks") {
                            auxLinksList.querySelectorAll("blockquote").forEach(blockquote => {
                                blockquote.classList.add("backlink-context");
                            });
                            auxLinksList.querySelectorAll("li > p").forEach(p => {
                                p.classList.add("backlink-source");
                            });
                            auxLinksList.querySelectorAll(".backlink-source a:nth-of-type(2), .backlink-context a").forEach(auxLink => {
                                auxLink.dataset.backlinkTargetUrl = AuxLinks.targetOfAuxLinksLink(sourceURL);
                            });
                        }
                    }
                }

                //  Fire contentDidLoad event.
                GW.notificationCenter.fireEvent("GW.contentDidLoad", {
                    source: "Content.contentTypes.localFragment.load",
                    container: contentDocument,
                    document: contentDocument,
                    loadLocation: sourceURL
                });

                return {
                	document: contentDocument
                };
            },

            permittedContentTypes: [ "text/html" ]
        },

        remoteImage: {
            matches: (link) => {
                if (Content.contentTypes.remoteImage.isWikimediaUploadsImageLink(link)) {
                    return true;
                } else {
                    return false;
                }
            },

            isSliceable: true,

            contentFromLink: (link) => {
                if ((Content.contentTypes.remoteImage.isWikimediaUploadsImageLink(link)) == false)
                    return null;

                //  Use annotation abstract (if any) as figure caption.
                let caption = Content.figcaptionHTMLForMediaLink(link);

                /*  Note that we pass in the original link’s classes; this
                    is good for classes like ‘invert’, ‘width-full’, etc.
                 */
                let contentDocument = newDocument(`<figure><img
                											class="${link.classList}"
                											src="${link.href}"
                											loading="eager"
                											decoding="sync"
                											>${caption}</figure>`);

                //  Remove extraneous classes.
                Content.removeExtraneousClassesFromMediaElement(contentDocument.querySelector("img"));

                //  Fire contentDidLoad event.
                GW.notificationCenter.fireEvent("GW.contentDidLoad", {
                    source: "Content.contentTypes.remoteImage.load",
                    container: contentDocument,
                    document: contentDocument,
                    loadLocation: new URL(link.href)
                });

                return {
                	document: contentDocument
                };
            },

            isWikimediaUploadsImageLink: (link) => {
                return (   link.hostname == "upload.wikimedia.org"
                        && link.pathname.endsWithAnyOf(Content.contentTypes.localImage.imageFileExtensions.map(x => `.${x}`)));
            }
        },

        remoteVideo: {
            matches: (link) => {
                if (Content.contentTypes.remoteVideo.isYoutubeLink(link)) {
                    return (Content.contentTypes.remoteVideo.youtubeId(link) != null);
                } else if (Content.contentTypes.remoteVideo.isVimeoLink(link)) {
                    return (Content.contentTypes.remoteVideo.vimeoId(link) != null);
                } else {
                    return false;
                }
            },

            isSliceable: true,

            contentFromLink: (link) => {
                let contentDocument = null;

                if (Content.contentTypes.remoteVideo.isYoutubeLink(link)) {
                    let srcdocStyles =
                          `<style>`
                        + `* { padding: 0; margin: 0; overflow: hidden; } `
                        + `html, body { height: 100%; } `
                        + `img, span { position: absolute; width: 100%; top: 0; bottom: 0; margin: auto; } `
                        + `span { height: 1.5em; text-align: center; font: 48px/1.5 sans-serif; color: white; text-shadow: 0 0 0.5em black; }`
                        + `</style>`;

                    let videoId = Content.contentTypes.remoteVideo.youtubeId(link);
                    let videoEmbedURL = URLFromString(`https://www.youtube.com/embed/${videoId}`);
                    if (link.search > "")
                        videoEmbedURL.search = link.search;
                    let placeholderImgSrc = `https://img.youtube.com/vi/${videoId}/hqdefault.jpg`;
                    let playButtonHTML = `<span class='video-embed-play-button'>&#x25BA;</span>`;
                    let srcdocHTML = `<a href='${videoEmbedURL.href}?autoplay=1'><img src='${placeholderImgSrc}'>${playButtonHTML}</a>`;

                    //  `allow-same-origin` only for EXTERNAL videos, NOT local videos!
                    contentDocument = newDocument(Content.objectHTMLForURL(videoEmbedURL, {
                        additionalClasses: "youtube",
                        additionalAttributes: `srcdoc="${srcdocStyles}${srcdocHTML}" sandbox="allow-scripts allow-same-origin allow-presentation" allowfullscreen`
                    }));
                } else if (Content.contentTypes.remoteVideo.isVimeoLink(link)) {
                    let videoId = Content.contentTypes.remoteVideo.vimeoId(link);
                    let videoEmbedURL = URLFromString(`https://player.vimeo.com/video/${videoId}`);
                    if (link.search > "")
                        videoEmbedURL.search = link.search;

                    contentDocument = newDocument(Content.objectHTMLForURL(videoEmbedURL, {
                        additionalClasses: "vimeo",
                        additionalAttributes: `allow="autoplay; fullscreen; picture-in-picture" allowfullscreen`
                    }));
                }

                return {
                	document: contentDocument
                };
            },

            isYoutubeLink: (link) => {
                return [ "www.youtube.com", "youtube.com", "youtu.be" ].includes(link.hostname);
            },

            youtubeId: (url) => {
                let match = url.href.match(/^.*(youtu.be\/|v\/|u\/\w\/|embed\/|watch\?v=|\&v=)([^#\&\?]*).*/);
                if (   match
                    && match.length == 3
                    && match[2].length == 11) {
                    return match[2];
                } else {
                    return null;
                }
            },

            isVimeoLink: (link) => {
                return [ "vimeo.com" ].includes(link.hostname);
            },

            vimeoId: (url) => {
                let match = url.pathname.match(/^\/([0-9]+)$/);
                if (   match
                    && match.length == 2) {
                    return match[1];
                } else {
                    return null;
                }
            }
        },

        localDocument: {
            matches: (link) => {
                //  Some local-document links are handled specially.
                if ([ "tweet"
                      ].findIndex(x => Content.contentTypes[x].matches(link)) !== -1)
                    return false;

                //  Account for alternate and archive URLs.
                let url = URLFromString(link.dataset.urlArchive ?? link.dataset.urlHtml ?? link.href);

                //  Maybe it’s a foreign link?
                if (url.hostname != location.hostname)
                    return false;

                //  On mobile, we cannot embed PDFs.
                if (   GW.isMobile()
                    && url.pathname.endsWith(".pdf"))
                    return false;

                return (   url.pathname.startsWith("/metadata/") == false
                        && url.pathname.endsWithAnyOf(Content.contentTypes.localDocument.documentFileExtensions.map(x => `.${x}`)));
            },

            isSliceable: false,

            contentFromLink: (link) => {
                let embedSrc = link.dataset.urlArchive ?? link.dataset.urlHtml ?? link.href;
                let additionalAttributes = [ ];

                //  Determine sandbox settings.
                let embedURL = URLFromString(embedSrc);
                if (embedURL.pathname.startsWith("/static/") == true) {
                	additionalAttributes.push(`sandbox="allow-scripts allow-same-origin allow-popups allow-popups-to-escape-sandbox"`);
                } else if (embedURL.pathname.endsWith(".pdf") == false) {
                    additionalAttributes.push(`sandbox="allow-same-origin" referrerpolicy="same-origin"`);
                }

                let contentDocument = newDocument(Content.objectHTMLForURL(embedSrc, {
                    additionalAttributes: additionalAttributes.join(" ")
                }));

				return {
                	document: contentDocument
                };
            },

            documentFileExtensions: [ "html", "pdf", "csv", "doc", "docx", "ods", "xls", "xlsx" ]
        },

        localVideo: {
            matches: (link) => {
                //  Maybe it’s a foreign link?
                if (link.hostname != location.hostname)
                    return false;

                return link.pathname.endsWithAnyOf(Content.contentTypes.localVideo.videoFileExtensions.map(x => `.${x}`));
            },

            isSliceable: true,

            contentFromLink: (link) => {
                //  Import specified dimensions / aspect ratio.
                let dimensions = Content.mediaDimensionsHTMLForMediaLink(link);

                //  Determine video type and poster pathname.
                let videoFileExtension = /\.(\w+?)$/.exec(link.pathname)[1];
                let posterPathname = link.pathname + "-poster.jpg";

                //  Use annotation abstract (if any) as figure caption.
                let caption = Content.figcaptionHTMLForMediaLink(link);

                /*  Note that we pass in the original link’s classes; this
                    is good for classes like ‘invert’, ‘width-full’, etc.
                 */
                let contentDocument = newDocument(`<figure><video
                											${dimensions}
                											class="${link.classList}"
                											controls="controls"
                											preload="none"
                											data-video-poster="${posterPathname}"
                											>`
                										+ `<source
                											src="${link.href}"
                											type="video/${videoFileExtension}"
                											>`
                										+ `</video>${caption}</figure>`);

                //  Remove extraneous classes.
                Content.removeExtraneousClassesFromMediaElement(contentDocument.querySelector("video"));

                //  Fire contentDidLoad event.
                GW.notificationCenter.fireEvent("GW.contentDidLoad", {
                    source: "Content.contentTypes.localVideo.load",
                    container: contentDocument,
                    document: contentDocument,
                    loadLocation: new URL(link.href)
                });

                return {
                	document: contentDocument
                };
            },

            videoFileExtensions: [ "mp4", "webm" ]
        },

        localAudio: {
            matches: (link) => {
                //  Maybe it’s a foreign link?
                if (link.hostname != location.hostname)
                    return false;

                return link.pathname.endsWithAnyOf(Content.contentTypes.localAudio.audioFileExtensions.map(x => `.${x}`));
            },

            isSliceable: true,

            contentFromLink: (link) => {
                //  Use annotation abstract (if any) as figure caption.
                let caption = Content.figcaptionHTMLForMediaLink(link);

                /*  Note that we pass in the original link’s classes; this
                    is good for classes like ‘invert’, ‘width-full’, etc.
                 */
                let contentDocument = newDocument(`<figure><audio
                											class="${link.classList}"
                											controls="controls"
                											preload="none"
                											>`
                										+ `<source src="${link.href}">`
                										+ `</audio>${caption}</figure>`);

                //  Remove extraneous classes.
                Content.removeExtraneousClassesFromMediaElement(contentDocument.querySelector("audio"));

                //  Fire contentDidLoad event.
                GW.notificationCenter.fireEvent("GW.contentDidLoad", {
                    source: "Content.contentTypes.localAudio.load",
                    container: contentDocument,
                    document: contentDocument,
                    loadLocation: new URL(link.href)
                });

                return {
                	document: contentDocument
                };
            },

            audioFileExtensions: [ "mp3" ]
        },

        localImage: {
            matches: (link) => {
                //  Maybe it’s a foreign link?
                if (link.hostname != location.hostname)
                    return false;

                return link.pathname.endsWithAnyOf(Content.contentTypes.localImage.imageFileExtensions.map(x => `.${x}`));
            },

            isSliceable: true,

            contentFromLink: (link) => {
                //  Import specified dimensions / aspect ratio.
                let dimensions = Content.mediaDimensionsHTMLForMediaLink(link);

                //  Use annotation abstract (if any) as figure caption.
                let caption = Content.figcaptionHTMLForMediaLink(link);

                /*  Note that we pass in the original link’s classes; this
                    is good for classes like ‘invert’, ‘width-full’, etc.
                 */
                let contentDocument = newDocument(`<figure><img
                											${dimensions}
                											class="${link.classList}"
                											src="${link.href}"
                											loading="eager"
                											decoding="sync"
                											>${caption}</figure>`);

                //  Remove extraneous classes.
                Content.removeExtraneousClassesFromMediaElement(contentDocument.querySelector("img"));

                //  Fire contentDidLoad event.
                GW.notificationCenter.fireEvent("GW.contentDidLoad", {
                    source: "Content.contentTypes.localImage.load",
                    container: contentDocument,
                    document: contentDocument,
                    loadLocation: new URL(link.href)
                });

                return {
                	document: contentDocument
                };
            },

            imageFileExtensions: [ "bmp", "gif", "ico", "jpeg", "jpg", "png", "svg" ]
        },

        localPage: {
            matches: (link) => {
                //  Maybe it’s a foreign link?
                if (link.hostname != location.hostname)
                    return false;

                /*  If it has a period in it, it’s probably not a page, but is
                    something else, like a file of some sort, or a locally
                    archived document. Still, we allow for explicit overrides.
                 */
                return (   link.pathname.match(/\./) == null
                        || link.pathname.endsWith("/index")
                        || link.classList.contains("link-page"));
            },

            isSliceable: true,

            sourceURLsForLink: (link) => {
                let url = URLFromString(link.href);
                url.hash = "";
                url.search = "";

                return [ url ];
            },

            contentFromResponse: (response, link, sourceURL) => {
                let contentDocument = response
                					  ? newDocument(response)
                					  : document;

                if (response)
                    contentDocument.baseLocation = sourceURL;

                //  Get the body classes.
                let pageBodyClasses = contentDocument.querySelector("meta[name='page-body-classes']").getAttribute("content").trim().split(" ");

                //  Get the page title.
                let pageTitle = contentDocument.querySelector("title").innerHTML.match(Content.contentTypes.localPage.pageTitleRegexp)[1];

                //  Get the page thumbnail URL and metadata.
                let pageThumbnailHTML;
                let pageThumbnailMetaTag = contentDocument.querySelector("meta[property='og:image']");
                if (pageThumbnailMetaTag) {
                    let pageThumbnailURL = URLFromString(pageThumbnailMetaTag.getAttribute("content"));

                    //  Alt text, if provided.
                    let pageThumbnailAltMetaTag = contentDocument.querySelector("meta[property='og:image:alt']");
                    let pageThumbnailAltText = (pageThumbnailAltMetaTag
                                                ? pageThumbnailAltMetaTag.getAttribute("content")
                                                : `Thumbnail image for “${pageTitle}”`
                                                ).replace(/"/g, "&quot;");

                    //  Image dimensions.
                    let pageThumbnailWidth = contentDocument.querySelector("meta[property='og:image:width']").getAttribute("content");
                    let pageThumbnailHeight = contentDocument.querySelector("meta[property='og:image:height']").getAttribute("content");

                    //  Construct and save the <img> tag.
                    if (pageThumbnailURL.pathname.startsWith(Content.contentTypes.localPage.defaultPageThumbnailPathnamePrefix) == false)
                        pageThumbnailHTML = `<img
                            src="${pageThumbnailURL.href}"
                            title="${pageThumbnailAltText}"
                            width="${pageThumbnailWidth}"
                            height="${pageThumbnailHeight}"
                            style="width: ${pageThumbnailWidth}px; height: auto;"
                                >`;

                    //  Request the image, to cache it.
                    doAjax({ location: pageThumbnailURL.href });
                }

                if (response) {
                    //  Fire contentDidLoad event.
                    GW.notificationCenter.fireEvent("GW.contentDidLoad", {
                        source: "Content.contentTypes.localPage.load",
                        container: contentDocument,
                        document: contentDocument,
                        loadLocation: sourceURL
                    });
                }

                return {
                    document:       contentDocument,
                    title:          pageTitle,
                    bodyClasses:    pageBodyClasses,
                    thumbnailHTML:  pageThumbnailHTML
                };
            },

            referenceDataFromContent: (pageContent, link) => {
                //  The page content is the page body plus the metadata block.
                let bodyContentDocument = newDocument();
                //  Add the page metadata block.
                let pageMetadataBlock = pageContent.document.querySelector("#page-metadata");
                if (pageMetadataBlock) {
                    bodyContentDocument.append(newDocument(pageMetadataBlock));

                    pageMetadataBlock = bodyContentDocument.querySelector("#page-metadata");
                    pageMetadataBlock.classList.remove("markdownBody");
                    if (pageMetadataBlock.className == "")
                        pageMetadataBlock.removeAttribute("class");
                }
                //  Add the page main content block.
                bodyContentDocument.append(newDocument(pageContent.document.querySelector("#markdownBody").childNodes));

                //  Find the target element and/or containing block, if any.
                let element = targetElementInDocument(link, bodyContentDocument);

                //  Pop-frame title text.
                let popFrameTitleTextParts = [ ];
                if (link.pathname != location.pathname)
                    popFrameTitleTextParts.push(pageContent.title);

                //  Section title or block id.
                if (element) {
                    let nearestSection = element.closest("section");
                    let nearestFootnote = element.closest("li.footnote");
                    if (nearestFootnote) {
                        popFrameTitleTextParts.push("Footnote", Notes.noteNumber(nearestFootnote));
                        let identifyingSpan = nearestFootnote.querySelector("span[id]:empty");
                        if (identifyingSpan)
                            popFrameTitleTextParts.push(`(#${(identifyingSpan.id)})`);
                    } else if (nearestSection) {
                        //  Section mark (§) for sections.
                        popFrameTitleTextParts.push("&#x00a7;");
                        if (nearestSection.id == "footnotes") {
                            popFrameTitleTextParts.push("Footnotes");
                        } else {
                            popFrameTitleTextParts.push(nearestSection.firstElementChild.textContent);
                        }
                    } else {
                        popFrameTitleTextParts.push(link.hash);
                    }
                }

                return {
                    content:                 bodyContentDocument,
                    pageTitle:               pageContent.title,
                    pageBodyClasses:         pageContent.bodyClasses,
                    pageThumbnailHTML:       pageContent.thumbnailHTML,
                    popFrameTitleLinkHref:   link.href,
                    popFrameTitleText:       popFrameTitleTextParts.join(" "),
                    popFrameTitleTextShort:  popFrameTitleTextParts.first
                }
            },

            permittedContentTypes: [ "text/html" ],
            pageTitleRegexp: /^(.+?) · Gwern\.net( \(reader mode\))?$/,
            defaultPageThumbnailPathnamePrefix: "/static/img/logo/logo-"
        }
    }
};
