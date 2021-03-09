Annotations = {
    /*****************/
    /*  Configuration.
        */

	annotationsBasePathname: "/metadata/annotations/",
	annotationReferenceElementSelectors: [ "a.docMetadata", "span.defnMetadata" ],
	annotationReferenceElementSelectorPrefix: ".annotation > p ",

	/******************/
	/*	Infrastructure.
		*/

	annotationsWorkspace: null,

	/***********/
	/*	General.
		*/

	cleanup: () => {
		GWLog("Annotations.cleanup", "annotations.js", 1);

		//  Remove staging element for annotations.
		if (Extracts.annotationsWorkspace)
			Extracts.annotationsWorkspace.remove();

		//  Remove content load event handlers.
		GW.notificationCenter.removeHandlerForEvent("GW.contentDidLoad", signalAnnotationLoaded);
		GW.notificationCenter.removeHandlerForEvent("GW.contentLoadDidFail", signalAnnotationLoadFailed);

		//  Fire cleanup-complete event.
		GW.notificationCenter.fireEvent("Annotations.cleanupDidComplete");	
	},

    setup: () => {
		GWLog("Annotations.setup", "annotations.js", 1);

		//  Inject the staging area for annotations.
		document.body.insertAdjacentHTML("beforeend", `<div id="annotations-workspace" style="display:none;"></div>`);
		Annotations.annotationsWorkspace = document.querySelector("#annotations-workspace");

		//	Add handler for if an annotation loads.
		GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", Annotations.signalAnnotationLoaded = (info) => {
			GWLog("Annotations.signalAnnotationLoaded", "annotations.js", 2);

			/*  If this is an annotation that’s loaded, we cache it, remove 
				it from the staging element, and fire the annotationDidLoad
				event.
				*/
			Annotations.cachedAnnotationReferenceEntries[info.identifier] = info.document;
			info.document.remove();

			GW.notificationCenter.fireEvent("Annotations.annotationDidLoad", { identifier: info.identifier });
		}, {
			phase: ">rewrite",
			condition: (info) => (info.document.parentElement && info.document.parentElement == Annotations.annotationsWorkspace)
		});

		//	Add handler for if loading an annotation failed.
		GW.notificationCenter.addHandlerForEvent("GW.contentLoadDidFail", Annotations.signalAnnotationLoadFailed = (info) => {
			GWLog("Annotations.signalAnnotationLoadFailed", "annotations.js", 2);

			/*	If this is an annotation that’s failed to load, then we set
				the cache value to indicate this, and fire the 
				annotationLoadDidFail event.
				*/
			Annotations.cachedAnnotationReferenceEntries[info.identifier] = "LOADING_FAILED";

			GW.notificationCenter.fireEvent("Annotations.annotationLoadDidFail", { identifier: info.identifier });
		}, { condition: (info) => info.document == Annotations.annotationsWorkspace });

		//  Fire setup-complete event.
		GW.notificationCenter.fireEvent("Annotations.setupDidComplete");
	},

	cachedAnnotationReferenceEntries: { },

    cachedAnnotationExists: (annotationIdentifier) => {
		let cachedAnnotation = Annotations.cachedAnnotationReferenceEntries[annotationIdentifier];
		return (cachedAnnotation && cachedAnnotation != "LOADING_FAILED");
    },

	annotationForIdentifier: (annotationIdentifier) => {
		return Annotations.cachedAnnotationReferenceEntries[annotationIdentifier];
	},

	stageAnnotation: (annotationRawHTML) => {
		Annotations.annotationsWorkspace.insertAdjacentHTML("beforeend", `<div class="annotation">${annotationRawHTML}</div>`);
		return Annotations.annotationsWorkspace.lastElementChild;;
	},

    loadAnnotation: (annotationIdentifier) => {
		GWLog("Annotations.loadAnnotation", "annotations.js", 2);

		let annotationURL;
		if (Annotations.isWikipediaLink(annotationIdentifier)) {
			//  Wikipedia entry.
			annotationURL = new URL(annotationIdentifier);
			let wikiPageName = /\/([^\/]+?)$/.exec(annotationURL.pathname)[1];
			annotationURL.originalPathname = annotationURL.pathname;
			annotationURL.pathname = `/api/rest_v1/page/mobile-sections-${(annotationURL.hash > "" ? "remaining" : "lead")}/${wikiPageName}`;
		} else {
			//  Local annotation.
			annotationURL = new URL("https://" + location.hostname + Annotations.annotationsBasePathname 
							+ fixedEncodeURIComponent(fixedEncodeURIComponent(annotationIdentifier)) + ".html");
		}

		doAjax({
			location: annotationURL.href,
			onSuccess: (event) => {
				let annotation;
				if (Annotations.isWikipediaLink(annotationIdentifier)) {
					let response = JSON.parse(event.target.responseText);

					let targetSection;
					if (annotationURL.hash > "")
						targetSection = response["sections"].find(section => section["anchor"] == decodeURIComponent(annotationURL.hash).substr(1));

					let responseHTML = targetSection ? targetSection["text"] : response["sections"][0]["text"];
					annotation = Annotations.stageAnnotation(responseHTML);

					annotation.dataset["titleText"] = (annotationURL.hash > "") ? targetSection["line"] : response["displaytitle"];
					annotation.dataset["wikiPageName"] = 

					Annotations.processWikipediaEntry(annotation, annotationURL);
				} else {
					annotation = Annotations.stageAnnotation(event.target.responseText);
				}

				GW.notificationCenter.fireEvent("GW.contentDidLoad", { 
					source: "Annotations.loadAnnotation",
					document: annotation, 
					identifier: annotationIdentifier,
					isMainDocument: false,
					needsRewrite: true, 
					clickable: false, 
					collapseAllowed: false, 
					isCollapseBlock: false,
					isFullPage: false,
					location: annotationURL,
					fullWidthPossible: false
				});
			},
			onFailure: (event) => {
				GW.notificationCenter.fireEvent("GW.contentLoadDidFail", {
					source: "Annotations.loadAnnotation",
					document: Annotations.annotationsWorkspace, 
					identifier: annotationIdentifier,
					location: annotationURL
				});
			}
		});
    },

	/*	Used to generate extracts and definitions.
		*/
	referenceDataForAnnotationIdentifier: (annotationIdentifier) => {
		let referenceEntry = Annotations.cachedAnnotationReferenceEntries[annotationIdentifier];

		if (Annotations.isWikipediaLink(annotationIdentifier)) {
			return Annotations.referenceDataForWikipediaEntry(referenceEntry);
		} else {
			return Annotations.referenceDataForLocalAnnotation(referenceEntry);
		}
	},

	/*	Returns true iff the given identifier string is a Wikipedia URL.
		*/
	isWikipediaLink: (annotationIdentifier) => {
		if (/^[\?\/]/.test(annotationIdentifier))
			return false;

		let url = new URL(annotationIdentifier);

		return (url && /(.+?)\.wikipedia\.org/.test(url.hostname));
	},

	/*	Annotations generated server-side and hosted locally.
		*/
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

		return {
			element: 		referenceElement,
			titleText: 		referenceElement.textContent,
			titleHTML: 		referenceElement.innerHTML.trimQuotes(),
			authorHTML:		(authorElement ? `<span class="data-field author">${authorList}</span>` : ``),
			dateHTML:		(dateElement ? ` (<span class="data-field date">${dateElement.textContent}</span>)` : ``),
			abstractHTML:	referenceEntry.querySelector("blockquote div").innerHTML
		};
	},

	/*	Wikipedia entries (page summaries or sections).
		*/
	referenceDataForWikipediaEntry: (referenceEntry) => {
		return {
			element: 		referenceEntry,
			titleText: 		referenceEntry.dataset["titleText"],
			titleHTML: 		referenceEntry.dataset["titleText"],
			authorHTML:		`<span class="data-field author">Wikipedia</span>`,
			dateHTML:		``,
			abstractHTML:	referenceEntry.innerHTML
		};
	},

	processWikipediaEntry: (annotation, annotationURL) => {
		//	Remove unwanted elements.
		annotation.querySelectorAll(".mw-ref, .shortdescription, .plainlinks").forEach(element => {
			element.remove();
		});

		//	Remove empty paragraphs.
		annotation.querySelectorAll("p:empty").forEach(emptyGraf => {
			emptyGraf.remove();
		});

		//	Process links.
		annotation.querySelectorAll("a").forEach(link => {
			//	Qualify links.
			if (link.getAttribute("href").startsWith("#"))
				link.pathname = annotationURL.originalPathname;
			if (link.hostname == location.hostname)
				link.hostname = annotationURL.hostname;

			//	Mark other Wikipedia links as also being annotated.
			if (/(.+?)\.wikipedia\.org/.test(link.hostname))
				link.classList.add("docMetadata");
		});

		//	Strip inline styles.
		annotation.querySelectorAll("*").forEach(element => {
			element.removeAttribute("style");
		});

		//	Un-linkify images.
		annotation.querySelectorAll("a img").forEach(imageLink => {
			imageLink.parentElement.outerHTML = imageLink.outerHTML;
		});

		//	Normalize table cell types.
		annotation.querySelectorAll("th:not(:only-child)").forEach(cell => {
			cell.outerHTML = `<td>${cell.innerHTML}</td>`;
		});
	}
};

GW.notificationCenter.fireEvent("Annotations.didLoad");

/******************/
/*	Initialization.
	*/
doWhenPageLoaded(() => {
	Annotations.setup();
});
