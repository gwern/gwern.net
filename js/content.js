Content = {
	/*******************/
	/*	Content caching.
	 */

	cachedContent: { },

	contentCacheKeyForLink: (link) => {
		return (Content.sourceURLsForLink(link)?.first ?? link).href;
	},

	cacheContentForLink: (content, link) => {
		Content.cachedContent[Content.contentCacheKeyForLink(link)] = content;
	},

	cachedContentForLink: (link) => {
		//	Special case for the link being to the current page.
		if (   link.pathname == location.pathname
			&& Content.cachedContent[Content.contentCacheKeyForLink(link)] == null)
			Content.load(link);

		return Content.cachedContent[Content.contentCacheKeyForLink(link)];
	},

	cachedDocumentForLink: (link) => {
		let contentType = Content.contentTypeForLink(link);
		let content = Content.cachedContentForLink(link);
		return (content && content != "LOADING_FAILED"
				? (contentType.referenceDataFromContent ? content.document : content)
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
	/*	Content loading.
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
			if (content) {
				Content.cacheContentForLink(content, link);

				GW.notificationCenter.fireEvent("Content.contentDidLoad", {
					link: link
				});
			} else {
				Content.cacheContentForLink("LOADING_FAILED", link);

				GW.notificationCenter.fireEvent("Content.contentLoadDidFail", {
					link: link
				});

				//	Send request to record failure in server logs.
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
					let contentType = Content.contentTypeForLink(link);
					let httpContentTypeHeader = event.target.getResponseHeader("Content-Type");
					if (   contentType.permittedContentTypes
						&& (   httpContentTypeHeader == null
							|| contentType.permittedContentTypes.includes(httpContentTypeHeader.match(/(.+?)(?:;|$)/)[1]) == false)) {
						//	Send request to record failure in server logs.
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

					//	Send request to record failure in server logs.
					GWServerLogError(link.href + `--missing-content`, "missing content");
				}
			});
		}

		//	Call any provided handlers, if/when appropriate.
		if (loadHandler || loadFailHandler)
			Content.waitForDataLoad(link, loadHandler, loadFailHandler);
	},

	contentFromLink: (link) => {
		return Content.contentTypeForLink(link)?.contentFromLink?.(link);
	},

	contentFromResponse: (response, link, sourceURL) => {
		return Content.contentTypeForLink(link)?.contentFromResponse?.(response, link, sourceURL);
	},

	/****************************/
	/*	Reference data retrieval.
	 */

	referenceDataForLink: (link) => {
		let content = Content.cachedContentForLink(link);
		if (   content == null
			|| content == "LOADING_FAILED") {
			return content;
		} else {
			return Content.referenceDataFromContent(content, link);
		}
	},

	referenceDataFromContent: (content, link) => {
		return (Content.contentTypeForLink(link).referenceDataFromContent?.(content, link) ?? { content: content });
	},

	/***********/
	/*	Helpers.
	 */

    objectHTMLForURL: (url, options = { }) => {
		if (typeof url == "string")
			url = URLFromString(url);

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
		//	Remove various link classes.
		media.classList.remove("no-popup", "icon-not", "link-page", "link-live",
			"link-annotated", "link-annotated-partial", "link-annotated-not",
			"has-annotation", "has-annotation-partial", "has-content",
			"has-icon", "has-indicator-hook", "spawns-popup", "spawns-popin");

		//	Remove all `include-` classes.
		media.classList.remove(...(Array.from(media.classList).filter(x => x.startsWith("include-"))));
	},

	/**************************************************************/
	/*	CONTENT TYPES

		Each has the following necessary members:

			.matches(URL|Element) => boolean
			.isPageContent: boolean

		... plus either these two:

			.sourceURLsForLink(URL|Element) => [ URL ]
			.contentFromResponse(string, URL|Element, URL) => object

		... or this one:

			.contentFromLink(URL|Element) => object

		A content type may also have the following optional members:

			.referenceDataFromContent(object, URL|Element) => object

				NOTE: If this method is not present, then .contentFromResponse
				or .contentFromLink (whichever is present) should return a
				DocumentFragment instead of a dictionary object.
	 */

	contentTypeForLink: (link) => {
		for ([ typeName, contentType ] of Object.entries(Content.contentTypes))
			if (contentType.matches(link))
				return contentType;

		return null;
	},

	contentTypes: {
		foreignSite: {
			matches: (link) => {
				//	Some foreign-site links are handled specially.
				if ([ "tweet",
					  "remoteVideo",
					  "remoteImage"
					  ].findIndex(x => Content.contentTypes[x].matches(link)) !== -1)
					return false;

				//	Account for alternate and archive URLs.
				let url = URLFromString(link.dataset.urlArchive ?? link.dataset.urlHtml ?? link.href);

				return (   url.hostname != location.hostname
        				&& link.classList.contains("link-live"));
			},

			isPageContent: false,

			contentFromLink: (link) => {
				//  WARNING: EXPERIMENTAL FEATURE!
// 				if (localStorage.getItem("enable-embed-proxy") == "true") {
// 					let url = URLFromString(embedSrc);
// 					let proxyURL = URLFromString("https://api.obormot.net/embed.php");
// 					doAjax({
// 						location: proxyURL.href,
// 						params: { url: url.href },
// 						onSuccess: (event) => {
// 							if (Extracts.popFrameProvider.isSpawned(target.popFrame) == false)
// 								return;
//
// 							let doc = newElement("DIV", null, { "innerHTML": event.target.responseText });
// 							doc.querySelectorAll("[href], [src]").forEach(element => {
// 								if (element.href) {
// 									let elementURL = URLFromString(element.href);
// 									if (   elementURL.host == location.host
// 										&& !element.getAttribute("href").startsWith("#")) {
// 										elementURL.host = url.host;
// 										element.href = elementURL.href;
// 									}
// 								} else if (element.src) {
// 									let elementURL = URLFromString(element.src);
// 									if (elementURL.host == location.host) {
// 										elementURL.host = url.host;
// 										element.src = elementURL.href;
// 									}
// 								}
// 							});
//
// 							if (event.target.getResponseHeader("content-type").startsWith("text/plain"))
// 								doc.innerHTML = `<pre>${doc.innerHTML}</pre>`;
//
// 							target.popFrame.document.querySelector("iframe").srcdoc = doc.innerHTML;
//
// 							Extracts.postRefreshUpdatePopFrameForTarget(target, true);
// 						},
// 						onFailure: (event) => {
// 							if (Extracts.popFrameProvider.isSpawned(target.popFrame) == false)
// 								return;
//
// 							Extracts.postRefreshUpdatePopFrameForTarget(target, false);
// 						}
// 					});
//
// 					return newDocument(`<iframe frameborder="0" sandbox="allow-scripts allow-popups"></iframe>`);
// 				}
				//  END EXPERIMENTAL SECTION

				let embedSrc = link.dataset.urlArchive ?? link.dataset.urlHtml ?? link.href;
				let additionalAttributes = [ ];

				//	Determine sandbox settings.
				additionalAttributes.push(Content.contentTypes.foreignSite.shouldEnableScriptsForURL(URLFromString(embedSrc))
										  ? `sandbox="allow-scripts allow-same-origin"`
										  : `sandbox`);

				return newDocument(Content.objectHTMLForURL(embedSrc, {
					additionalAttributes: additionalAttributes.join(" ")
				}));
			},

			shouldEnableScriptsForURL: (url) => {
				if (url.hostname == "docs.google.com")
					return true;

				if (   url.hostname == "archive.org"
					&& url.pathname.startsWith("/details/"))
					return true;

				return false;
			}
		},

		tweet: {
			matches: (link) => {
				return (   [ "twitter.com", "x.com" ].includes(link.hostname)
						&& link.pathname.match(/\/.+?\/status\/[0-9]+$/) != null);
			},

			isPageContent: true,

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

			referenceDataFromContent: (tweetPage, link) => {
				//	Link metadata for title-links.
				let titleLinkClass = "title-link link-live content-transform-not";
				let titleLinkIconMetadata = `data-link-icon-type="svg" data-link-icon="twitter"`;

				let nitterHost = Content.contentTypes.tweet.getNitterHost();

				//	URL for link to user’s page.
				let titleLinkURL = URLFromString(tweetPage.document.querySelector(".main-tweet a.username").href);
				titleLinkURL.hostname = nitterHost;
				let titleLinkHref = titleLinkURL.href;

				//	Avatar.
				let avatarImgElement = tweetPage.document.querySelector(".main-tweet img.avatar").cloneNode(true);
				let avatarImgSrc = avatarImgElement.getAttribute("src");
				if (avatarImgSrc.startsWith("data:image/svg+xml")) {
					avatarImgElement.setAttribute("style", avatarImgElement.getAttribute("style")
														   + ";"
														   + tweetPage.document.querySelector("style").innerHTML.match(/:root\{(.+?)\}/)[1]);
					let avatarImgSrcVar = avatarImgElement.style.getPropertyValue("background-image").match(/var\((.+?)\)/)[1];
					avatarImgSrc = avatarImgElement.style.getPropertyValue(avatarImgSrcVar).match(/url\("(.+?)"\)/)[1];
				}
				let avatarImg = newElement("IMG", { src: avatarImgSrc, class: "avatar figure-not" });

				//	Text of link to user’s page.
				let titleParts = tweetPage.document.querySelector("title").textContent.match(/^(.+?) \((@.+?)\):/);
				let titleText = `“${titleParts[1]}” (${titleParts[2]})`;
				let titleHTML = `${avatarImg.outerHTML}“${titleParts[1]}” (<code>${titleParts[2]}</code>)`;

				//	Link to tweet.
				let tweetDate = new Date(Date.parse(tweetPage.document.querySelector(".main-tweet .tweet-date").textContent));
				let tweetDateString = ("" + tweetDate.getFullYear())
									+ "-"
									+ ("" + tweetDate.getMonth()).padStart(2, '0')
									+ "-"
									+ ("" + tweetDate.getDate()).padStart(2, '0');
				let tweetLinkURL = URLFromString(link.href);
				tweetLinkURL.hostname = nitterHost;
				tweetLinkURL.hash = "m";

				//	Secondary title links.
				let secondaryTitleLinksHTML = ` on <a href="${tweetLinkURL.href}" class="${titleLinkClass}" ${titleLinkIconMetadata}>${tweetDateString}</a>:`;

				//	Tweet content itself.
				let tweetContent = tweetPage.document.querySelector(".main-tweet .tweet-content").innerHTML.split("\n\n").map(graf => `<p>${graf}</p>`).join("\n");

				//	Attached media (video or images).
				tweetContent += Content.contentTypes.tweet.mediaEmbedHTML(tweetPage.document);

				//	Pop-frame title text.
				let popFrameTitleText = `${titleHTML} on ${tweetDateString}`;

				return {
					content: {
						titleHTML:                titleHTML,
						fullTitleHTML:            titleHTML,
						secondaryTitleLinksHTML:  secondaryTitleLinksHTML,
						titleText:                titleText,
						titleLinkHref:            titleLinkHref,
						titleLinkClass:           titleLinkClass,
						titleLinkIconMetadata:    titleLinkIconMetadata,
						abstract: 		          tweetContent,
						dataSourceClass:          "tweet",
					},
					template:                       "annotation-blockquote-outside",
					linkTarget:                     (GW.isMobile() ? "_self" : "_blank"),
					whichTab:                       (GW.isMobile() ? "current" : "new"),
					tabOrWindow:                    (GW.isMobile() ? "tab" : "window"),
					popFrameTitleText:              popFrameTitleText,
					popFrameTitleLinkHref:          tweetLinkURL.href
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
				"nitter.net"
			],

			getNitterHost: () => {
				let hosts = Content.contentTypes.tweet.liveNitterHosts;
				return hosts[rollDie(hosts.length) - 1];
			}
		},

		localCodeFile: {
			matches: (link) => {
				//	Maybe it’s a foreign link?
				if (link.hostname != location.hostname)
					return false;

				//	Maybe it’s an aux-links link?
				if (link.pathname.startsWith("/metadata/"))
					return false;

				//	Maybe it’s a local document link?
				if (   link.pathname.startsWith("/doc/www/")
                	|| (   link.pathname.startsWith("/doc/")
                		&& link.pathname.match(/\.(html|pdf)$/i) != null))
                	return false;

				return link.pathname.endsWithAnyOf(Content.contentTypes.localCodeFile.codeFileExtensions.map(x => `.${x}`));
			},

			isPageContent: false,

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
				let content;

				//	Parse (encoding and wrapping first, if need be).
				if (sourceURL.pathname == link.pathname + ".html") {
					//	Syntax-highlighted code (already HTML-encoded).
					content = newDocument(response);

					//	We want <body> contents only, no metadata and such.
					let nodes = Array.from(content.childNodes);
					let codeWrapper = content.querySelector("div.sourceCode");
					content.replaceChildren(...(nodes.slice(nodes.indexOf(codeWrapper))));

					//	Handle truncated syntax-highlighted code files.
					if (codeWrapper.nextElementSibling?.tagName == "P") {
						codeWrapper.classList.add("truncated");

						let truncationNotice = codeWrapper.nextElementSibling;
						truncationNotice.classList.add("truncation-notice");
						truncationNotice.querySelector("a").classList.add("extract-not");

						codeWrapper.append(truncationNotice);
					}

					//	Set ‘line’ class and fix blank lines.
					Array.from(content.querySelector("code").children).forEach(lineSpan => {
						lineSpan.classList.add("line");
						if (lineSpan.innerHTML.length == 0)
							lineSpan.innerHTML = "&nbsp;";
					});
				} else {
					//	“Raw” code.
					let htmlEncodedResponse = response.replace(
						/[<>]/g,
						c => ('&#' + c.charCodeAt(0) + ';')
					).split("\n").map(
						line => (`<span class="line">${(line || "&nbsp;")}</span>`)
					).join("\n");
					content = newDocument(  `<div class="sourceCode">`
										  + `<pre class="raw-code"><code>`
										  + htmlEncodedResponse
										  + `</code></pre>`
										  + `</div>`);
				}

				return content;
			},

			codeFileExtensions: [
				//	Truncated at 2000 lines for preview.
				"bash", "c", "conf", "css", "diff", "hs", "html", "js",
				"json", "jsonl", "md", "opml", "patch", "php", "py", "R",
				"sh", "xml",
				//	Non-syntax highlighted (due to lack of known format), but truncated:
				"txt"
			]
		},

		localFragment: {
			matches: (link) => {
				//	Maybe it’s a foreign link?
				if (link.hostname != location.hostname)
					return false;

				return (   link.pathname.startsWith("/metadata/")
						&& link.pathname.endsWith(".html"));
			},

			isPageContent: true,

			sourceURLsForLink: (link) => {
				let url = URLFromString(link.href);
				url.hash = "";
				url.search = "";

				return [ url ];
			},

			contentFromResponse: (response, link, sourceURL) => {
				let content = newDocument(response);

				let auxLinksLinkType = AuxLinks.auxLinksLinkType(sourceURL);
				if (auxLinksLinkType) {
					let auxLinksList = content.querySelector("ul, ol");
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
					container: content,
					document: content,
					loadLocation: sourceURL
				});

				return content;
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

			isPageContent: true,

			contentFromLink: (link) => {
				if ((Content.contentTypes.remoteImage.isWikimediaUploadsImageLink(link)) == false)
					return null;

				//	Use annotation abstract (if any) as figure caption.
				let caption = Content.figcaptionHTMLForMediaLink(link);

				/*  Note that we pass in the original link’s classes; this
					is good for classes like ‘invert’, ‘width-full’, etc.
				 */
				let content = newDocument(`<figure><img
											class="${link.classList}"
											src="${link.href}"
											loading="eager"
											decoding="sync"
											>${caption}</figure>`);

				//	Remove extraneous classes.
				Content.removeExtraneousClassesFromMediaElement(content.querySelector("img"));

				//  Fire contentDidLoad event.
				GW.notificationCenter.fireEvent("GW.contentDidLoad", {
					source: "Content.contentTypes.remoteImage.load",
					container: content,
					document: content,
					loadLocation: new URL(link.href)
				});

				return content;
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

			isPageContent: true,

			contentFromLink: (link) => {
				let content = null;

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
					content = newDocument(Content.objectHTMLForURL(videoEmbedURL, {
						additionalClasses: "youtube",
						additionalAttributes: `srcdoc="${srcdocStyles}${srcdocHTML}" sandbox="allow-scripts allow-same-origin allow-presentation" allowfullscreen`
					}));
				} else if (Content.contentTypes.remoteVideo.isVimeoLink(link)) {
					let videoId = Content.contentTypes.remoteVideo.vimeoId(link);
					let videoEmbedURL = URLFromString(`https://player.vimeo.com/video/${videoId}`);
					if (link.search > "")
						videoEmbedURL.search = link.search;

					content = newDocument(Content.objectHTMLForURL(videoEmbedURL, {
						additionalClasses: "vimeo",
						additionalAttributes: `allow="autoplay; fullscreen; picture-in-picture" allowfullscreen`
					}));
				}

				return content;
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
				//	Account for alternate and archive URLs.
				let url = URLFromString(link.dataset.urlArchive ?? link.dataset.urlHtml ?? link.href);

				//	Maybe it’s a foreign link?
				if (url.hostname != location.hostname)
					return false;

				//	On mobile, we cannot embed PDFs.
				if (   GW.isMobile()
					&& url.pathname.endsWith(".pdf"))
					return false;

				return (   url.pathname.startsWith("/metadata/") == false
						&& url.pathname.endsWithAnyOf(Content.contentTypes.localDocument.documentFileExtensions.map(x => `.${x}`)));
			},

			isPageContent: false,

			contentFromLink: (link) => {
				let embedSrc = link.dataset.urlArchive ?? link.dataset.urlHtml ?? link.href;
				let additionalAttributes = [ ];

				//	Determine sandbox settings.
				if (URLFromString(embedSrc).pathname.endsWith(".pdf") == false)
					additionalAttributes.push(`sandbox="allow-same-origin" referrerpolicy="same-origin"`);

				return newDocument(Content.objectHTMLForURL(embedSrc, {
					additionalAttributes: additionalAttributes.join(" ")
				}));
			},

			documentFileExtensions: [ "html", "pdf", "csv", "doc", "docx", "ods", "xls", "xlsx" ]
		},

		localVideo: {
			matches: (link) => {
				//	Maybe it’s a foreign link?
				if (link.hostname != location.hostname)
					return false;

				return link.pathname.endsWithAnyOf(Content.contentTypes.localVideo.videoFileExtensions.map(x => `.${x}`));
			},

			isPageContent: true,

			contentFromLink: (link) => {
				//	Import specified dimensions / aspect ratio.
				let dimensions = Content.mediaDimensionsHTMLForMediaLink(link);

				//	Determine video type and poster pathname.
				let videoFileExtension = /\.(\w+?)$/.exec(link.pathname)[1];
				let posterPathname = link.pathname + "-poster.jpg";

				//	Use annotation abstract (if any) as figure caption.
				let caption = Content.figcaptionHTMLForMediaLink(link);

				/*  Note that we pass in the original link’s classes; this
					is good for classes like ‘invert’, ‘width-full’, etc.
				 */
				let content = newDocument(`<figure><video
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

				//	Remove extraneous classes.
				Content.removeExtraneousClassesFromMediaElement(content.querySelector("video"));

				//  Fire contentDidLoad event.
				GW.notificationCenter.fireEvent("GW.contentDidLoad", {
					source: "Content.contentTypes.localVideo.load",
					container: content,
					document: content,
					loadLocation: new URL(link.href)
				});

				return content;
			},

		    videoFileExtensions: [ "mp4", "webm" ]
		},

		localAudio: {
			matches: (link) => {
				//	Maybe it’s a foreign link?
				if (link.hostname != location.hostname)
					return false;

				return link.pathname.endsWithAnyOf(Content.contentTypes.localAudio.audioFileExtensions.map(x => `.${x}`));
			},

			isPageContent: true,

			contentFromLink: (link) => {
				//	Use annotation abstract (if any) as figure caption.
				let caption = Content.figcaptionHTMLForMediaLink(link);

				/*  Note that we pass in the original link’s classes; this
					is good for classes like ‘invert’, ‘width-full’, etc.
				 */
				let content = newDocument(`<figure><audio
											class="${link.classList}"
											controls="controls"
											preload="none"
											>`
										+ `<source src="${link.href}">`
										+ `</audio>${caption}</figure>`);

				//	Remove extraneous classes.
				Content.removeExtraneousClassesFromMediaElement(content.querySelector("audio"));

				//  Fire contentDidLoad event.
				GW.notificationCenter.fireEvent("GW.contentDidLoad", {
					source: "Content.contentTypes.localAudio.load",
					container: content,
					document: content,
					loadLocation: new URL(link.href)
				});

				return content;
			},

			audioFileExtensions: [ "mp3" ]
		},

		localImage: {
			matches: (link) => {
				//	Maybe it’s a foreign link?
				if (link.hostname != location.hostname)
					return false;

				return link.pathname.endsWithAnyOf(Content.contentTypes.localImage.imageFileExtensions.map(x => `.${x}`));
			},

			isPageContent: true,

			contentFromLink: (link) => {
				//	Import specified dimensions / aspect ratio.
				let dimensions = Content.mediaDimensionsHTMLForMediaLink(link);

				//	Use annotation abstract (if any) as figure caption.
				let caption = Content.figcaptionHTMLForMediaLink(link);

				/*  Note that we pass in the original link’s classes; this
					is good for classes like ‘invert’, ‘width-full’, etc.
				 */
				let content = newDocument(`<figure><img
											${dimensions}
											class="${link.classList}"
											src="${link.href}"
											loading="eager"
											decoding="sync"
											>${caption}</figure>`);

				//	Remove extraneous classes.
				Content.removeExtraneousClassesFromMediaElement(content.querySelector("img"));

				//  Fire contentDidLoad event.
				GW.notificationCenter.fireEvent("GW.contentDidLoad", {
					source: "Content.contentTypes.localImage.load",
					container: content,
					document: content,
					loadLocation: new URL(link.href)
				});

				return content;
			},

			imageFileExtensions: [ "bmp", "gif", "ico", "jpeg", "jpg", "png", "svg" ]
		},

		localPage: {
			matches: (link) => {
				//	Maybe it’s a foreign link?
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

			isPageContent: true,

			sourceURLsForLink: (link) => {
				let url = URLFromString(link.href);
				url.hash = "";
				url.search = "";

				return [ url ];
			},

			contentFromResponse: (response, link, sourceURL) => {
				let page = response
						   ? newDocument(response)
						   : document;

				if (response)
					page.baseLocation = sourceURL;

				//	Get the body classes.
				let pageBodyClasses = page.querySelector("meta[name='page-body-classes']").getAttribute("content").trim().split(" ");

				//  Get the page title.
				let pageTitle = page.querySelector("title").innerHTML.match(Content.contentTypes.localPage.pageTitleRegexp)[1];

				//	Get the page thumbnail URL and metadata.
				let pageThumbnailHTML;
				let pageThumbnailMetaTag = page.querySelector("meta[property='og:image']");
				if (pageThumbnailMetaTag) {
					let pageThumbnailURL = URLFromString(pageThumbnailMetaTag.getAttribute("content"));

					//	Alt text, if provided.
					let pageThumbnailAltMetaTag = page.querySelector("meta[property='og:image:alt']");
					let pageThumbnailAltText = (pageThumbnailAltMetaTag
												? pageThumbnailAltMetaTag.getAttribute("content")
												: `Thumbnail image for “${pageTitle}”`
												).replace(/"/g, "&quot;");

					//	Image dimensions.
					let pageThumbnailWidth = page.querySelector("meta[property='og:image:width']").getAttribute("content");
					let pageThumbnailHeight = page.querySelector("meta[property='og:image:height']").getAttribute("content");

					//	Construct and save the <img> tag.
					if (pageThumbnailURL.pathname.startsWith(Content.contentTypes.localPage.defaultPageThumbnailPathnamePrefix) == false)
						pageThumbnailHTML = `<img
							src="${pageThumbnailURL.href}"
							title="${pageThumbnailAltText}"
							width="${pageThumbnailWidth}"
							height="${pageThumbnailHeight}"
							style="width: ${pageThumbnailWidth}px; height: auto;"
								>`;

					//	Request the image, to cache it.
					doAjax({ location: pageThumbnailURL.href });
				}

				if (response) {
					//  Fire contentDidLoad event.
					GW.notificationCenter.fireEvent("GW.contentDidLoad", {
						source: "Content.contentTypes.localPage.load",
						container: page,
						document: page,
						loadLocation: sourceURL
					});
				}

				return {
					title:          pageTitle,
					bodyClasses:    pageBodyClasses,
					thumbnailHTML:  pageThumbnailHTML,
					document:       page
				};
			},

			referenceDataFromContent: (page, link) => {
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
				let element = targetElementInDocument(link, pageContent);

				//	Pop-frame title text.
				let popFrameTitleTextParts = [ ];
				if (link.pathname != location.pathname)
					popFrameTitleTextParts.push(page.title);

				//	Section title or block id.
				if (element) {
					let nearestSection = element.closest("section");
					let nearestFootnote = element.closest("li.footnote");
					if (nearestFootnote) {
						popFrameTitleTextParts.push("Footnote", Notes.noteNumber(nearestFootnote));
						let identifyingSpan = nearestFootnote.querySelector("span[id]:empty");
						if (identifyingSpan)
							popFrameTitleTextParts.push(`(#${(identifyingSpan.id)})`);
					} else if (nearestSection) {
						//	Section mark (§) for sections.
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
					content:                 pageContent,
					pageTitle:               page.title,
					pageBodyClasses:         page.bodyClasses,
					pageThumbnailHTML:       page.thumbnailHTML,
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
