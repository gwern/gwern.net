/**********************************/
/*	Events fired by annotations.js:

	Annotations.didLoad
		Fired when the Annotations object has loaded.

	Annotations.setupDidComplete
		Fired just before the ‘setup’ function returns.

	Annotations.cleanupDidComplete
		Fired just before the ‘cleanup’ function returns.

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
    /*****************/
    /*  Configuration.
        */

    annotationsBasePathname: "/metadata/annotations/",
    annotationReferenceElementSelector: "a.link-annotated",

    /***********/
    /*  General.
        */

	//	Called in: nowhere
    cleanup: () => {
        GWLog("Annotations.cleanup", "annotations.js", 1);

        //  Fire cleanup-complete event.
        GW.notificationCenter.fireEvent("Annotations.cleanupDidComplete");
    },

	//	Called in: this file (doWhenPageLoaded)
    setup: () => {
        GWLog("Annotations.setup", "annotations.js", 1);

        //  Fire setup-complete event.
        GW.notificationCenter.fireEvent("Annotations.setupDidComplete");
    },

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

	/*	Returns the URL of the annotation resource for the given identifier.
	 */
	//	Called by: Annotations.loadAnnotation
	annotationURLForIdentifier: (identifier) => {
		let annotationURL;
        if (Annotations.isWikipediaArticleLink(identifier)) {
            //  Wikipedia entry.
            annotationURL = new URL(identifier);
            let wikiPageName = fixedEncodeURIComponent(/\/wiki\/(.+?)$/.exec(decodeURIComponent(annotationURL.pathname))[1]);
            annotationURL.pathname = `/api/rest_v1/page/mobile-sections/${wikiPageName}`;
            annotationURL.hash = "";
        } else {
            //  Local annotation.
            annotationURL = new URL("https://"
            						+ location.hostname
            						+ Annotations.annotationsBasePathname
            						+ fixedEncodeURIComponent(fixedEncodeURIComponent(identifier))
            						+ ".html");
        }

        return annotationURL;
	},

	//	Called by: Annotations.loadAnnotation
	cachedResponseForIdentifier: (identifier) => {
		if (Annotations.isWikipediaArticleLink(identifier))
			return Annotations.cachedWikipediaAPIResponses[Annotations.annotationURLForIdentifier(identifier)];

		return null;
	},

    /*  Load, stage, and process the annotation for the given identifier string.
        */
    //	Called by: Extracts.setUpAnnotationLoadEventWithin (extracts-annotations.js)
    loadAnnotation: (identifier) => {
        GWLog("Annotations.loadAnnotation", "annotations.js", 2);

		/*	Get URL of the annotation resource.
		 */
        let annotationURL = Annotations.annotationURLForIdentifier(identifier);

		/*	Depending on the annotation source, `response` could be text (HTML),
			JSON (already parsed), or other.
		 */
		let processResponse = (response) => {
			let annotation;
			if (Annotations.isWikipediaArticleLink(identifier)) {
				annotation = Annotations.stagedAnnotationFromWikipediaAPIResponse(response, identifier);
			} else {
				annotation = Extracts.newDocument(response);
			}

			if (annotation) {
				Annotations.cachedReferenceData[identifier] = Annotations.referenceDataForAnnotation(annotation, identifier);

				GW.notificationCenter.fireEvent("Annotations.annotationDidLoad", { identifier: identifier });
			} else {
				Annotations.cachedReferenceData[identifier] = "LOADING_FAILED";

				GW.notificationCenter.fireEvent("Annotations.annotationLoadDidFail", { identifier: identifier });

				//	Send request to record failure in server logs.
				GWServerLogError(annotationURL + `--could-not-process`, "problematic annotation");
			}
		};

		/*	Retrieve the annotation resource and stage the annotation.
		 */
		let response = Annotations.cachedResponseForIdentifier(identifier);
		if (response) {
			processResponse(response);
		} else {
			doAjax({
				location: annotationURL.href,
				onSuccess: (event) => {
					if (Annotations.isWikipediaArticleLink(identifier)) {
						let responseJSON = JSON.parse(event.target.responseText);

						Annotations.cachedWikipediaAPIResponses[annotationURL] = responseJSON;

						processResponse(responseJSON);
					} else {
						processResponse(event.target.responseText);
					}
				},
				onFailure: (event) => {
					Annotations.cachedReferenceData[identifier] = "LOADING_FAILED";

					GW.notificationCenter.fireEvent("Annotations.annotationLoadDidFail", { identifier: identifier });

					//	Send request to record failure in server logs.
					GWServerLogError(annotationURL, "missing annotation");
				}
			});
		}
    },

	//	Called by: Annotations.loadAnnotation
	referenceDataForAnnotation: (annotation, identifier) => {
        if (Annotations.isWikipediaArticleLink(identifier)) {
            return Annotations.referenceDataForWikipediaEntry(annotation);
        } else {
            return Annotations.referenceDataForLocalAnnotation(annotation);
        }
	},

    /*  Annotations generated server-side and hosted locally.
        */
    //	Called by: Annotations.referenceDataForAnnotationIdentifier
    referenceDataForLocalAnnotation: (referenceEntry) => {
        let referenceElement = referenceEntry.querySelector(Annotations.annotationReferenceElementSelector);

        //  Author list.
        let authorElement = referenceEntry.querySelector(".author");
        //	Generate comma-separated author list; truncate with “et al” @ > 3.
        let authorList;
        if (authorElement) {
            authorList = authorElement.textContent.split(", ").slice(0, 3).join(", ");
            if (authorList.length < authorElement.textContent.length)
                authorList += " et al";
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

        return {
            element:        referenceElement,
            titleHTML:      referenceElement.innerHTML.trimQuotes(),
            authorHTML:     (authorElement ? `<span class="data-field author">${authorList}</span>` : ``),
            dateHTML:       (dateElement ? ` (<span class="data-field date" title="${dateElement.textContent}">` +
                             dateElement.textContent.replace(/-[0-9][0-9]-[0-9][0-9]$/, "") +
                             `</span>)` : ``),
            tagsHTML:       (tagsElement ? `<span class="data-field link-tags">${tagsElement.innerHTML}</span>` : ``),
            backlinksHTML:  (backlinksElement ? `<span class="data-field backlinks">${backlinksElement.innerHTML}</span>` : ``),
            similarHTML:    (similarElement ? `<span class="data-field similars" >${similarElement.innerHTML}</span>` : ``),
            abstract:   	(abstractElement ? Extracts.newDocument(abstractElement.children) : null)
        };
    },

	/*******************************/
	/* EXTERNAL ANNOTATION SOURCES */
	/*******************************/

	/*************/
	/*	Wikipedia.
	 */

	cachedWikipediaAPIResponses: { },

    /*  Returns true iff the given identifier string is a Wikipedia URL.
        */
    //	Called by: Annotations.annotationURLForIdentifier
    //	Called by: Annotations.loadAnnotation
    //	Called by: Annotations.referenceDataForAnnotationIdentifier
    //	Called by: Extracts.annotationForTarget (extracts-annotations.js)
    //	Called by: Extracts.titleForPopFrame_ANNOTATION (extracts-annotations.js)
    isWikipediaArticleLink: (identifier) => {
        if (/^[\/]/.test(identifier))
            return false;

        let url = new URL(identifier);

        return (   url 
        		&& /(.+?)\.wikipedia\.org/.test(url.hostname)
        		&& !(url.pathname.startsWithAnyOf(_π("/wiki/", [ "File:", "Category:", "Special:" ]))));
    },

    /*  Wikipedia entries (page summaries or sections).
        */
    //	Called by: Annotations.referenceDataForAnnotationIdentifier
    referenceDataForWikipediaEntry: (referenceEntry) => {
        return {
            element:        null,
            titleHTML:      referenceEntry.titleHTML,
            articleTitle:	referenceEntry.articleTitle,
            authorHTML:     `<span class="data-field author">Wikipedia</span>`,
            abstract: 		Extracts.newDocument(referenceEntry)
        };
    },

	/*	Returns a staged annotation, given the full response JSON of a Wikipedia
		API response.
	 */
	//	Called by: Annotations.loadAnnotation
	stagedAnnotationFromWikipediaAPIResponse: (response, identifier) => {
		let articleURL = new URL(identifier);
		let responseHTML, titleHTML;
		if (articleURL.hash > "") {
			let targetSection = response["remaining"]["sections"].find(section =>
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
				responseHTML += `<div class="TOC"><ul>`;
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

					responseHTML += `<li><a href='${articleURL}#${section["anchor"]}'>${section["line"]}</a>`;

					headingLevel = newHeadingLevel;
				}
				responseHTML += `</li></ul></div>`;
			}

			titleHTML = response["lead"]["displaytitle"];
		}

		let annotation = Extracts.newDocument(responseHTML);
		annotation.titleHTML = titleHTML;
		annotation.articleTitle = response["lead"]["displaytitle"];

		Annotations.postProcessStagedWikipediaAnnotation(annotation, identifier);

		return annotation;
	},

    /*  Elements to excise from a Wikipedia entry.
        */
    //	Used by: Annotations.postProcessStagedWikipediaAnnotation
    wikipediaEntryExtraneousElementSelectors: [
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

    /*  Post-process an already-staged annotation created from a Wikipedia
    	entry (do HTML cleanup, etc.).
        */
    //	Called by: Annotations.loadAnnotation
    postProcessStagedWikipediaAnnotation: (annotation, identifier) => {
		let articleURL = new URL(identifier);

        //  Remove unwanted elements.
        annotation.querySelectorAll(Annotations.wikipediaEntryExtraneousElementSelectors.join(", ")).forEach(element => {
            element.remove();
        });

        //  Remove location maps (they don’t work right).
        annotation.querySelectorAll(".locmap").forEach(locmap => {
            locmap.closest("tr").remove();
        });

        //  Remove empty paragraphs.
        annotation.querySelectorAll("p:empty").forEach(emptyGraf => {
            emptyGraf.remove();
        });

        //  Process links.
        annotation.querySelectorAll("a").forEach(link => {
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
				if (Annotations.isWikipediaArticleLink(link)) {
					link.classList.add("link-annotated");
				} else {
					if (!(link.pathname.startsWithAnyOf(_π("/wiki/", [ "Special:" ]))))
						link.classList.add("link-live");
				}
            }

            //  Mark self-links.
            if (link.pathname == articleURL.pathname)
                link.classList.add("link-self");
        });

        //  Strip inline styles.
        annotation.querySelectorAll("[style]").forEach(element => {
            element.removeAttribute("style");
        });

        //  Un-linkify images.
        annotation.querySelectorAll("a img").forEach(imageLink => {
            imageLink.parentElement.outerHTML = imageLink.outerHTML;
        });

        //  Normalize table cell types.
        annotation.querySelectorAll("th:not(:only-child)").forEach(cell => {
            cell.outerHTML = `<td>${cell.innerHTML}</td>`;
        });

		//	Fix chemical formulas.
		annotation.querySelectorAll(".chemf br").forEach(br => {
			br.remove();
		});

        //  Rectify table classes.
        annotation.querySelectorAll("table.sidebar").forEach(table => {
            table.classList.toggle("infobox", true);
        });

        //  Separate out the thumbnail and float it.
        let thumbnail = annotation.querySelector("img");
        if (   thumbnail 
        	&& thumbnail.closest("table")) {
            //  Save reference to the thumbnail’s containing element.
            let thumbnailContainer = thumbnail.parentElement;

            //  Create the figure and move the thumbnail into it.
            let figure = document.createElement("FIGURE");
            figure.classList.add("float-right");
            figure.appendChild(thumbnail);

            //  Create the caption, if need be.
            let caption = annotation.querySelector(".mw-default-size + div");
            if (   caption 
            	&& caption.textContent > "") {
                let figcaption = document.createElement("FIGCAPTION");
                figcaption.innerHTML = caption.innerHTML;
                figure.appendChild(figcaption);
            }

            //  Insert the figure as the first child of the annotation.
            annotation.insertBefore(figure, annotation.firstElementChild);

            //  Rectify classes.
            thumbnailContainer.closest("table").classList.toggle("infobox", true);

            //  Remove the whole row where the thumbnail was.
            thumbnailContainer.closest("tr").remove();
        } else if (   thumbnail 
        		   && thumbnail.closest("figure")) {
            let figure = thumbnail.closest("figure");

            //  Insert the figure as the first child of the annotation.
            annotation.insertBefore(figure, annotation.firstElementChild);
            figure.classList.add("float-right");

            let caption = figure.querySelector("figcaption");
            if (caption.textContent == "")
                caption.remove();
        }
    }
};

GW.notificationCenter.fireEvent("Annotations.didLoad");

/******************/
/*  Initialization.
    */
doWhenPageLoaded(() => {
    Annotations.setup();
});
