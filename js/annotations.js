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

	GW.contentDidLoad {
			source: "Annotations.loadAnnotation"
			document: Annotations.annotationsWorkspace
			location:
				The URL of the annotation resource.
			identifier:
				The identifier string for the annotation.
				(See the Extracts.targetIdentifier function in extracts.js for
				 details.)
			flags: GW.contentDidLoadEventFlags.needsRewrite

		}
		Fired after a new annotation has been loaded and staged (and, if it is
		a Wikipedia entry, cleaned up / rectified), but not cached yet.

		(See rewrite.js for more information about the keys and values of the
		 GW.contentDidLoad event.)

	GW.contentLoadDidFail {
			source: "Annotations.loadAnnotation"
			document: Annotations.annotationsWorkspace
			location:
				The URL of the annotation resource.
			identifier:
				The identifier string for the annotation.
				(See the Extracts.targetIdentifier function in extracts.js for
				 details.)
		}
		Fired when a new annotation has failed to load (but before the load
		failure has been recorded in the annotations cache).
 */

Annotations = {
    /*****************/
    /*  Configuration.
        */

    annotationsBasePathname: "/metadata/annotations/",
    annotationReferenceElementSelectors: [ "a.docMetadata" ],
    annotationReferenceElementSelectorPrefix: ".annotation > p ",

    /******************/
    /*  Infrastructure.
        */

    annotationsWorkspace: null,

    /***********/
    /*  General.
        */

	//	Called in: nowhere
    cleanup: () => {
        GWLog("Annotations.cleanup", "annotations.js", 1);

        //  Remove staging element for annotations.
        if (Annotations.annotationsWorkspace)
            Annotations.annotationsWorkspace.remove();

        //  Remove content load event handlers.
        GW.notificationCenter.removeHandlerForEvent("GW.contentDidLoad", signalAnnotationLoaded);
        GW.notificationCenter.removeHandlerForEvent("GW.contentLoadDidFail", signalAnnotationLoadFailed);

        //  Fire cleanup-complete event.
        GW.notificationCenter.fireEvent("Annotations.cleanupDidComplete");
    },

	//	Called in: this file (doWhenPageLoaded)
    setup: () => {
        GWLog("Annotations.setup", "annotations.js", 1);

        //  Inject the staging area for annotations.
        document.body.insertAdjacentHTML("beforeend", `<div id="annotations-workspace" style="display:none;"></div>`);
        Annotations.annotationsWorkspace = document.querySelector("#annotations-workspace");

        //  Add handler for if an annotation loads.
        GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", Annotations.signalAnnotationLoaded = (info) => {
            GWLog("Annotations.signalAnnotationLoaded", "annotations.js", 2);

            /*  If this is an annotation that’s loaded, we cache it, remove
                it from the staging element, and fire the annotationDidLoad
                event.
                */
            Annotations.cachedAnnotations[info.identifier] = info.document;
            info.document.remove();

            GW.notificationCenter.fireEvent("Annotations.annotationDidLoad", { identifier: info.identifier });
        }, {
            phase: ">rewrite",
            condition: (info) => (   info.document.parentElement
                                  && info.document.parentElement == Annotations.annotationsWorkspace)
        });

        //  Add handler for if loading an annotation failed.
        GW.notificationCenter.addHandlerForEvent("GW.contentLoadDidFail", Annotations.signalAnnotationLoadFailed = (info) => {
            GWLog("Annotations.signalAnnotationLoadFailed", "annotations.js", 2);

            /*  If this is an annotation that’s failed to load, then we set
                the cache value to indicate this, and fire the
                annotationLoadDidFail event.
                */
            Annotations.cachedAnnotations[info.identifier] = "LOADING_FAILED";

            GW.notificationCenter.fireEvent("Annotations.annotationLoadDidFail", { identifier: info.identifier });
        }, {
        	condition: (info) => info.document == Annotations.annotationsWorkspace
        });

        //  Fire setup-complete event.
        GW.notificationCenter.fireEvent("Annotations.setupDidComplete");
    },

    /*  Storage for retrieved and cached annotations.
        */
    cachedAnnotations: { },

    /*  Returns true iff a processed and cached annotation exists for the given
        identifier string.
        */
    //	Called by: Extracts.setUpAnnotationLoadEventWithin (extracts-annotations.js)
    cachedAnnotationExists: (annotationIdentifier) => {
        let cachedAnnotation = Annotations.cachedAnnotations[annotationIdentifier];
        return (cachedAnnotation && cachedAnnotation != "LOADING_FAILED");
    },

    /*  Returns a cached annotation for a given identifier string, or else
        either “LOADING_FAILED” (if loading the annotation was attempted but
        failed) or null (if the annotation has not been loaded).
        */
    //	Called by: Extracts.annotationForTarget (extracts-annotations.js)
    annotationForIdentifier: (annotationIdentifier) => {
        return Annotations.cachedAnnotations[annotationIdentifier];
    },

	/*	Returns the URL of the annotation resource for the given identifier.
	 */
	//	Called by: Annotations.loadAnnotation
	annotationURLForIdentifier: (annotationIdentifier) => {
		let annotationURL;
        if (Annotations.isWikipediaLink(annotationIdentifier)) {
            //  Wikipedia entry.
            annotationURL = new URL(annotationIdentifier);
            let wikiPageName = /\/([^\/]+?)$/.exec(annotationURL.pathname)[1];
            annotationURL.originalPathname = annotationURL.pathname;
            annotationURL.pathname = `/api/rest_v1/page/mobile-sections/${wikiPageName}`;
        } else {
            //  Local annotation.
            annotationURL = new URL("https://"
            						+ location.hostname
            						+ Annotations.annotationsBasePathname
            						+ fixedEncodeURIComponent(fixedEncodeURIComponent(annotationIdentifier))
            						+ ".html");
        }

        return annotationURL;
	},

    /*  Construct a usable DOM object from the raw HTML of an annotation,
        by inserting it as a child of the annotations workspace element.
        */
    //	Called by: Annotations.loadAnnotation
    stageAnnotation: (annotationRawHTML) => {
        Annotations.annotationsWorkspace.insertAdjacentHTML("beforeend", `<div class="annotation">${annotationRawHTML}</div>`);
        return Annotations.annotationsWorkspace.lastElementChild;
    },

    /*  Load, stage, and process the annotation for the given identifier string.
        */
    //	Called by: Extracts.setUpAnnotationLoadEventWithin (extracts-annotations.js)
    loadAnnotation: (annotationIdentifier) => {
        GWLog("Annotations.loadAnnotation", "annotations.js", 2);

		/*	Get URL of the annotation resource.
		 */
        let annotationURL = Annotations.annotationURLForIdentifier(annotationIdentifier);

		/*	Retrieve the annotation resource and stage the annotation.
		 */
        doAjax({
            location: annotationURL.href,
            onSuccess: (event) => {
                let annotation;
                if (Annotations.isWikipediaLink(annotationIdentifier)) {
                	annotation = Annotations.stagedAnnotationFromWikipediaAPIResponse(event.target.responseText, annotationURL);
                	if (annotation) {
						Annotations.postProcessStagedWikipediaAnnotation(annotation, annotationURL);
                	} else {
						//	Send request to record failure in server logs.
						doAjax({ location: `${location.origin}/error/` + fixedEncodeURIComponent(annotationURL) });
                	}
                } else {
                    annotation = Annotations.stageAnnotation(event.target.responseText);
                }

				if (annotation) {
					/*	Fire GW.contentDidLoad event to trigger a rewrite pass and
						then cause the annotation to be cached (and trigger the
						Annotations.annotationDidLoad event to fire as well).
					 */
					GW.notificationCenter.fireEvent("GW.contentDidLoad", {
						source: "Annotations.loadAnnotation",
						document: annotation,
						location: annotationURL,
						identifier: annotationIdentifier,
						flags: GW.contentDidLoadEventFlags.needsRewrite
					});
				} else {
					GW.notificationCenter.fireEvent("GW.contentLoadDidFail", {
						source: "Annotations.loadAnnotation",
						document: Annotations.annotationsWorkspace,
						identifier: annotationIdentifier,
						location: annotationURL
					});
				}
            },
            onFailure: (event) => {
                GW.notificationCenter.fireEvent("GW.contentLoadDidFail", {
                    source: "Annotations.loadAnnotation",
                    document: Annotations.annotationsWorkspace,
                    location: annotationURL,
                    identifier: annotationIdentifier
                });

				//	Send request to record failure in server logs.
				doAjax({ location: `${location.origin}/error/` + fixedEncodeURIComponent(annotationURL) });
            }
        });
    },

    /*  Used to generate extracts.
        */
    //	Called by: Extracts.annotationForTarget (extracts-annotations.js)
    referenceDataForAnnotationIdentifier: (annotationIdentifier) => {
        let referenceEntry = Annotations.cachedAnnotations[annotationIdentifier];

        if (Annotations.isWikipediaLink(annotationIdentifier)) {
            return Annotations.referenceDataForWikipediaEntry(referenceEntry);
        } else {
            return Annotations.referenceDataForLocalAnnotation(referenceEntry);
        }
    },

    /*  Annotations generated server-side and hosted locally.
        */
    //	Called by: Annotations.referenceDataForAnnotationIdentifier
    referenceDataForLocalAnnotation: (referenceEntry) => {
        let referenceElement = referenceEntry.querySelector(Annotations.annotationReferenceElementSelectors.map(selector =>
            `${Annotations.annotationReferenceElementSelectorPrefix}${selector}`
        ).join(", "));

        //  Author list.
        let authorElement = referenceEntry.querySelector(".author");
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

        // the backlinks link (if exists)
        let backlinksElement = referenceEntry.querySelector(".backlinks");

        // the similar-links link (if exists)
        let similarElement = referenceEntry.querySelector(".similars");

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
            abstractHTML:   referenceEntry.querySelector("blockquote div").innerHTML
        };
    },

	/*******************************/
	/* EXTERNAL ANNOTATION SOURCES */
	/*******************************/

	/*************/
	/*	Wikipedia.
	 */

    /*  Returns true iff the given identifier string is a Wikipedia URL.
        */
    //	Called by: Annotations.loadAnnotation
    //	Called by: Annotations.referenceDataForAnnotationIdentifier
    //	Called by: Extracts.annotationForTarget (extracts-annotations.js)
    //	Called by: Extracts.titleForPopFrame_ANNOTATION (extracts-annotations.js)
    isWikipediaLink: (annotationIdentifier) => {
        if (/^[\?\/]/.test(annotationIdentifier))
            return false;

        let url = new URL(annotationIdentifier);

        return (url && /(.+?)\.wikipedia\.org/.test(url.hostname));
    },

    /*  Wikipedia entries (page summaries or sections).
        */
    //	Called by: Annotations.referenceDataForAnnotationIdentifier
    referenceDataForWikipediaEntry: (referenceEntry) => {
        return {
            element:        referenceEntry,
            titleHTML:      referenceEntry.dataset["titleHTML"],
            authorHTML:     `<span class="data-field author">Wikipedia</span>`,
            dateHTML:       ``,
            tagsHTML:       ``,
            backlinksHTML:  ``,
            abstractHTML:   referenceEntry.innerHTML
        };
    },

	/*	Returns a staged annotation, given the full response text of a Wikipedia
		API response.
	 */
	//	Called by: Annotations.loadAnnotation
	stagedAnnotationFromWikipediaAPIResponse: (responseText, annotationURL) => {
		let response = JSON.parse(responseText);

		let targetSection;
		if (annotationURL.hash > "") {
			targetSection = response["remaining"]["sections"].find(section =>
				section["anchor"] == decodeURIComponent(annotationURL.hash).substr(1)
			);

			/*	Check whether we have tried to load a page section which does
				not exist on the requested wiki page.
			 */
			if (!targetSection)
				return null;
		}

		let responseHTML = targetSection
						   ? targetSection["text"]
						   : response["lead"]["sections"][0]["text"];

		annotation = Annotations.stageAnnotation(responseHTML);
		annotation.dataset["titleHTML"] = annotationURL.hash > ""
										  ? targetSection["line"]
										  : response["lead"]["displaytitle"];

		return annotation;
	},

    /*  Elements to excise from a Wikipedia entry.
        */
    //	Called by: Annotations.postProcessStagedWikipediaAnnotation
    wikipediaEntryExtraneousElementSelectors: [
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
    postProcessStagedWikipediaAnnotation: (annotation, annotationURL) => {
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
            //  Qualify links.
            if (link.getAttribute("href").startsWith("#"))
                link.pathname = annotationURL.originalPathname;
            if (link.hostname == location.hostname)
                link.hostname = annotationURL.hostname;

            //  Mark other Wikipedia links as also being annotated.
            if (/(.+?)\.wikipedia\.org/.test(link.hostname))
                link.classList.add("docMetadata");

            //  Mark self-links.
            if (link.pathname == annotationURL.originalPathname)
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
            let figure = document.createElement("figure");
            figure.classList.add("float-right");
            figure.appendChild(thumbnail);

            //  Create the caption, if need be.
            let caption = annotation.querySelector(".mw-default-size + div");
            if (   caption 
            	&& caption.textContent > "") {
                let figcaption = document.createElement("figcaption");
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
