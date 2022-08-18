/**********************************/
/*	Events fired by annotations.js:

	Annotations.didLoad
		Fired when the Annotations object has loaded.

	Annotations.annotationDidLoad {
			identifier:
				The identifier string for the annotation. (See the
				Extracts.targetIdentifier function in extracts.js for details.)
		}
		Fired after a new annotation has been loaded and cached.

	Annotations.annotationLoadDidFail {
			identifier:
				The identifier string for the annotation. (See the
				Extracts.targetIdentifier function in extracts.js for details.)
		}
		Fired when a new annotation has failed to load, and the load failure
		has been recorded in the annotations cache.
 */

Annotations = {
    /***********/
    /*  General.
        */

    /*  Storage for retrieved and cached annotations.
        */
    cachedReferenceData: { },

    /*  Returns true iff a processed and cached annotation exists for the given
        identifier string.
        */
    //	Called by: Extracts.setUpAnnotationLoadEventWithin (extracts-annotations.js)
    cachedAnnotationExists: (identifier) => {
        let cachedAnnotation = Annotations.cachedReferenceData[identifier];
        return (cachedAnnotation && cachedAnnotation != "LOADING_FAILED");
    },

    /*  Returns cached annotation reference data for a given identifier string,
    	or else either “LOADING_FAILED” (if loading the annotation was attempted
    	but failed) or null (if the annotation has not been loaded).
        */
    //	Called by: Extracts.annotationForTarget (extracts-annotations.js)
    referenceDataForAnnotationIdentifier: (identifier) => {
        return Annotations.cachedReferenceData[identifier];
    },

	//	Called by: Annotations.sourceURLForIdentifier
	//	Called by: Annotations.processedAPIResponseForIdentifier
	//	Called by: Annotations.referenceDataFromParsedAPIResponse
	dataSourceForIdentifier: (identifier) => {
		for ([ sourceName, dataSource ] of Object.entries(Annotations.dataSources))
			if (   sourceName != "local"
				&& dataSource.matches(identifier))
				return dataSource;

		return Annotations.dataSources.local;
	},

	/*	Returns the URL of the annotation resource for the given identifier.
	 */
	//	Called by: Annotations.loadAnnotation
	//	Called by: Annotations.cachedAPIResponseForIdentifier
	//	Called by: Annotations.cacheAPIResponseForIdentifier
	sourceURLForIdentifier: (identifier) => {
		return Annotations.dataSourceForIdentifier(identifier).sourceURLForIdentifier(identifier);
	},

	//	Called by: Annotations.loadAnnotation
	processedAPIResponseForIdentifier: (response, identifier) => {
		return Annotations.dataSourceForIdentifier(identifier).processAPIResponse(response);
	},

	//	Used by: Annotations.cachedAPIResponseForIdentifier
	//	Used by: Annotations.cacheAPIResponseForIdentifier
	cachedAPIResponses: { },

	//	Called by: Annotations.loadAnnotation
	cachedAPIResponseForIdentifier: (identifier) => {
		return Annotations.cachedAPIResponses[Annotations.sourceURLForIdentifier(identifier)];
	},

	//	Called by: Annotations.loadAnnotation
	cacheAPIResponseForIdentifier: (response, identifier) => {
		Annotations.cachedAPIResponses[Annotations.sourceURLForIdentifier(identifier)] = response;
	},

    /*  Load and process the annotation for the given identifier string.
        */
    //	Called by: Extracts.setUpAnnotationLoadEventWithin (extracts-annotations.js)
    loadAnnotation: (identifier) => {
        GWLog("Annotations.loadAnnotation", "annotations.js", 2);

		/*	Get URL of the annotation resource.
		 */
        let sourceURL = Annotations.sourceURLForIdentifier(identifier);

		/*	Depending on the data source, `response` could be HTML,
			JSON, or other. We construct and cache a reference data object,
			then fire the appropriate event.
		 */
		let processResponse = (response) => {
			let referenceData = Annotations.referenceDataFromParsedAPIResponse(response, identifier);

			if (referenceData) {
				Annotations.cachedReferenceData[identifier] = referenceData;

				GW.notificationCenter.fireEvent("Annotations.annotationDidLoad", { identifier: identifier });
			} else {
				Annotations.cachedReferenceData[identifier] = "LOADING_FAILED";

				GW.notificationCenter.fireEvent("Annotations.annotationLoadDidFail", { identifier: identifier });

				//	Send request to record failure in server logs.
				GWServerLogError(sourceURL + `--could-not-process`, "problematic annotation");
			}
		};

		/*	Retrieve, parse, and cache the annotation resource; or use an
			already-cached API response.
		 */
		let response = Annotations.cachedAPIResponseForIdentifier(identifier);
		if (response) {
			processResponse(response);
		} else {
			doAjax({
				location: sourceURL.href,
				onSuccess: (event) => {
					let response = Annotations.processedAPIResponseForIdentifier(event.target.responseText, identifier);

					Annotations.cacheAPIResponseForIdentifier(response, identifier);

					processResponse(response);
				},
				onFailure: (event) => {
					Annotations.cachedReferenceData[identifier] = "LOADING_FAILED";

					GW.notificationCenter.fireEvent("Annotations.annotationLoadDidFail", { identifier: identifier });

					//	Send request to record failure in server logs.
					GWServerLogError(sourceURL, "missing annotation");
				}
			});
		}
    },

	//	Called by: Annotations.loadAnnotation
	referenceDataFromParsedAPIResponse: (response, identifier) => {
		return Annotations.dataSourceForIdentifier(identifier).referenceDataFromParsedAPIResponse(response, identifier);
	},

	/***************************/
	/* ANNOTATION DATA SOURCES */
	/***************************/
	/*	Note on annotation data sources:

		More data sources may be added. Any data source object must have these
		four properties, each a function with the given signature:

		matches: (string) => boolean
		sourceURLForIdentifier: (string) => string
		processAPIResponse: (string) => object
		referenceDataFromParsedAPIResponse: (string|object, string) => object

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
			/*	There could be a local annotation for any identifier. As this
				returns true for all identifiers, it is the fallback data source
				in the event that no other data sources match an identifier.
			 */
			matches: (identifier) => {
				return true;
			},

			//	Called by: Annotations.processedAPIResponseForIdentifier
			//	Called by: Annotations.sourceURLForIdentifier
			sourceURLForIdentifier: (identifier) => {
				return new URL("https://"
							   + location.hostname
							   + Annotations.dataSources.local.basePathname
							   + fixedEncodeURIComponent(fixedEncodeURIComponent(identifier))
							   + ".html");
			},

			//	Called by: Annotations.processedAPIResponseForIdentifier
			processAPIResponse: (response) => {
				return newDocument(response);
			},

			//	Called by: Annotations.referenceDataFromParsedAPIResponse
			referenceDataFromParsedAPIResponse: (referenceEntry, identifier) => {
				let referenceElement = referenceEntry.querySelector(Annotations.dataSources.local.referenceElementSelector);

				//  Author list.
				let authorElement = referenceEntry.querySelector(".author");
				//	Generate comma-separated author list; truncate with “…” abbreviation for 'et al' @ > 3.
				let authorList;
				if (authorElement) {
					authorList = authorElement.textContent.split(", ").slice(0, 3).join(", ");
					if (authorList.length < authorElement.textContent.length)
						authorList += "…";
				}

				//  Date.
				let dateElement = referenceEntry.querySelector(".date");

				// Link Tags
				let tagsElement = referenceEntry.querySelector(".link-tags");

				//	The backlinks link (if exists).
				let backlinksElement = referenceEntry.querySelector(".backlinks");

				//	The similar-links link (if exists).
				let similarElement = referenceEntry.querySelector(".similars");

				//	Abstract (if exists).
				let abstractElement = referenceEntry.querySelector("blockquote");
				//	Unwrap extraneous <div>, if present.
				if (   abstractElement
					&& abstractElement.firstElementChild == abstractElement.lastElementChild
					&& abstractElement.firstElementChild.tagName == "DIV")
					unwrap(abstractElement.firstElementChild);

				return {
					element:        referenceElement,
					titleHTML:      referenceElement.innerHTML.trimQuotes(),
					authorHTML:     (authorElement ? `<span class="data-field author cite-author">${authorList}</span>` : ``),
					dateHTML:       (dateElement ? `<span class="data-field cite-date" title="${dateElement.textContent}">` +
									 dateElement.textContent.replace(/-[0-9][0-9]-[0-9][0-9]$/, "") +
									 `</span>` : ``),
					tagsHTML:       (tagsElement ? `<span class="data-field link-tags">${tagsElement.innerHTML}</span>` : ``),
					backlinksHTML:  (backlinksElement ? `<span class="data-field backlinks">${backlinksElement.innerHTML}</span>` : ``),
					similarHTML:    (similarElement ? `<span class="data-field similars" >${similarElement.innerHTML}</span>` : ``),
					abstract:   	(abstractElement ? newDocument(abstractElement.childNodes) : null)
				};
			},

			basePathname: "/metadata/annotations/",
			referenceElementSelector: "a.link-annotated"
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
	matches: (identifier) => {
		//	The URL() constructor demands a fully qualified URL string.
		if (/^[\/]/.test(identifier))
			return false;

		let url = new URL(identifier);

		return (   url
				&& /(.+?)\.wikipedia\.org/.test(url.hostname)
				&& !(url.pathname.startsWithAnyOf(_π("/wiki/", [ "File:", "Category:", "Special:" ]))));
	},

	//	Called by: Annotations.processedAPIResponseForIdentifier
	//	Called by: Annotations.sourceURLForIdentifier
	sourceURLForIdentifier: (identifier) => {
		annotationURL = new URL(identifier);

		let wikiPageName = fixedEncodeURIComponent(/\/wiki\/(.+?)$/.exec(decodeURIComponent(annotationURL.pathname))[1]);
		annotationURL.pathname = `/api/rest_v1/page/mobile-sections/${wikiPageName}`;
		annotationURL.hash = "";

		return annotationURL;
	},

	//	Called by: Annotations.processedAPIResponseForIdentifier
	processAPIResponse: (response) => {
		return JSON.parse(response);
	},

	//	Called by: Annotations.referenceDataFromParsedAPIResponse
	referenceDataFromParsedAPIResponse: (response, identifier) => {
		let articleURL = new URL(identifier);
		let responseHTML, titleHTML;
		if (articleURL.hash > "") {
			let targetSection = response["remaining"]["sections"].find(section =>
				//	This MUST be decodeURIComponent, and NOT selectorFromHash!!!
				section["anchor"] == decodeURIComponent(articleURL.hash).slice(1)
			);

			/*	Check whether we have tried to load a page section which does
				not exist on the requested wiki page.
			 */
			if (!targetSection)
				return null;

			responseHTML = targetSection["text"];
			titleHTML = targetSection["line"];
		} else {
			responseHTML = response["lead"]["sections"][0]["text"];
			let sections = response["remaining"]["sections"];
			if (sections > []) {
				responseHTML += `<div class="TOC columns"><ul>`;
				let headingLevel = 2;
				for (let i = 0; i < sections.length; i++) {
					let section = sections[i];
					let newHeadingLevel = 1 + parseInt(section["toclevel"]);
					if (newHeadingLevel > headingLevel)
						responseHTML += `<ul>`;

					if (i > 0 && newHeadingLevel <= headingLevel)
						responseHTML += `</li>`;

					if (newHeadingLevel < headingLevel)
						responseHTML += `</ul>`;

					//	We must encode, because the anchor might contain quotes.
					let urlEncodedAnchor = fixedEncodeURIComponent(section["anchor"]);
					responseHTML += `<li><a href='${articleURL}#${urlEncodedAnchor}'>${section["line"]}</a>`;

					headingLevel = newHeadingLevel;
				}
				responseHTML += `</li></ul></div>`;
			}

			titleHTML = response["lead"]["displaytitle"];
		}

		let referenceEntry = newDocument(responseHTML);

		Annotations.dataSources.wikipedia.postProcessReferenceEntry(referenceEntry, identifier);

		return {
			element:        null,
			titleHTML:      titleHTML,
			articleTitle:	response["lead"]["displaytitle"],
			authorHTML:     `<span class="data-field author">Wikipedia</span>`,
			abstract: 		referenceEntry
		};
	},

	/*  Elements to excise from a Wikipedia entry.
		*/
	//	Used in: Annotations.dataSources.wikipedia.postProcessReferenceEntry
	extraneousElementSelectors: [
		"style",
		".mw-ref",
		".shortdescription",
		".plainlinks",
		"td hr",
		".hatnote",
		".portal",
		".penicon",
		".reference",
		".Template-Fact",
		".error",
		".mwe-math-mathml-inline"
	],

	/*  Post-process an already-constructed annotation created from a Wikipedia
		entry (do HTML cleanup, etc.).
		*/
	//	Called by: Annotations.dataSources.wikipedia.referenceDataFromParsedAPIResponse
	postProcessReferenceEntry: (referenceEntry, identifier) => {
		let articleURL = new URL(identifier);

		//  Remove unwanted elements.
		referenceEntry.querySelectorAll(Annotations.dataSources.wikipedia.extraneousElementSelectors.join(", ")).forEach(element => {
			element.remove();
		});

		//  Remove location maps (they don’t work right).
		referenceEntry.querySelectorAll(".locmap").forEach(locmap => {
			locmap.closest("tr").remove();
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

		//  Process links.
		referenceEntry.querySelectorAll("a").forEach(link => {
			//	De-linkify non-anchor self-links.
			if (   link.hash     == ""
				&& link.pathname == articleURL.pathname) {
				unwrap(link);
				return;
			}

			//  Qualify links.
			if (link.getAttribute("href").startsWith("#"))
				link.pathname = articleURL.pathname;
			if (link.hostname == location.hostname)
				link.hostname = articleURL.hostname;

			//  Mark other Wikipedia links as also being annotated.
			if (/(.+?)\.wikipedia\.org/.test(link.hostname)) {
				if (Annotations.dataSources.wikipedia.matches(link)) {
					link.classList.add("link-annotated");
				} else {
					if (!(link.pathname.startsWithAnyOf(_π("/wiki/", [ "Special:" ]))))
						link.classList.add("link-live");
				}
			}

			//  Mark self-links (anchorlinks within the same article).
			if (link.pathname == articleURL.pathname)
				link.classList.add("link-self");
		});

		//  Strip inline styles.
		referenceEntry.querySelectorAll("[style]").forEach(element => {
			element.removeAttribute("style");
		});

		//  Un-linkify images.
		referenceEntry.querySelectorAll("a img").forEach(imageLink => {
			imageLink.parentElement.outerHTML = imageLink.outerHTML;
		});

		//  Normalize table cell types.
		referenceEntry.querySelectorAll("th:not(:only-child)").forEach(cell => {
			cell.outerHTML = `<td>${cell.innerHTML}</td>`;
		});

		//	Fix chemical formulas.
		referenceEntry.querySelectorAll(".chemf br").forEach(br => {
			br.remove();
		});

		//  Rectify table classes.
		referenceEntry.querySelectorAll("table.sidebar").forEach(table => {
			table.classList.toggle("infobox", true);
		});

		//  Separate out the thumbnail and float it.
		let thumbnail = referenceEntry.querySelector("img");
		if (   thumbnail
			&& thumbnail.closest("table")) {
			//  Save reference to the thumbnail’s containing element.
			let thumbnailContainer = thumbnail.parentElement;

			//  Create the figure and move the thumbnail into it.
			let figure = document.createElement("FIGURE");
			figure.classList.add("float-right");
			figure.appendChild(thumbnail);

			//  Create the caption, if need be.
			let caption = referenceEntry.querySelector(".mw-default-size + div");
			if (   caption
				&& caption.textContent > "") {
				let figcaption = document.createElement("FIGCAPTION");
				figcaption.innerHTML = caption.innerHTML;
				figure.appendChild(figcaption);
			}

			//  Insert the figure as the first child of the annotation.
			referenceEntry.insertBefore(figure, referenceEntry.firstElementChild);

			//  Rectify classes.
			thumbnailContainer.closest("table").classList.toggle("infobox", true);

			//  Remove the whole row where the thumbnail was.
			thumbnailContainer.closest("tr").remove();
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
	}
};

/*  Returns true iff the given identifier string is a Wikipedia URL.
	*/
//	Called by: Extracts.annotationForTarget (extracts-annotations.js)
//	Called by: Extracts.titleForPopFrame_ANNOTATION (extracts-annotations.js)
Annotations.isWikipediaArticleLink = (identifier) => {
	return Annotations.dataSources.wikipedia.matches(identifier);
};

GW.notificationCenter.fireEvent("Annotations.didLoad");
