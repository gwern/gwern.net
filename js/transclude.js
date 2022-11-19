/* author: Said Achmiz */
/* license: MIT */

/*********************************/
/*  Events fired by transclude.js:

    GW.contentDidLoad {
            source: "transclude"
            document:
                The temporary wrapper element in which the transcluded content
                is contained. (The transcluded content is unwrapped, and the
                wrapper discarded, immediately after this event and the
                following one [GW.contentDidInject] have fired.)
            loadLocation:
                The URL of the include-link.
            baseLocation:
                The baseLocation of the document into which the transclusion is
                being done.
            flags:
                GW.contentDidLoadEventFlags.needsRewrite
                GW.contentDidLoadEventFlags.fullWidthPossible
                    (This flag is set only if transcluding into the main page.)
                GW.contentDidLoadEventFlags.collapseAllowed
                    (This flag is set only if transcluding into the main page.)
        }
        Fired after a transclusion, but only if the transclusion is done *after*
        the transcluded-into document has already undergone its own rewrite
        pass. (This will be the case often, but not always, due to caching.)

    GW.contentDidInject {
            source: "transclude"
            document:
                The temporary wrapper element in which the transcluded content
                is contained. (The transcluded content is unwrapped, and the
                wrapper discarded, immediately after this event has fired.)
            mainPageContent:
                True if transcluding into the main page, false otherwise.
        }
        Fired immediately after the previous event [GW.contentDidLoad], but only
        if that event’s `needsRewrite` flag is set.

    Rewrite.contentDidChange {
            source: "transclude"
            baseLocation:
                The baseLocation of the document into which the transclusion was
                done.
            document:
                The document into which the transclusion was done (either the
                window’s `document` object, or a DocumentFragment, or a shadow
                root).
        }
        Fired after transclusion has occurred and all unwrapping and cleanup has
        taken place, but only if the GW.contentDidLoad event’s `needsRewrite`
        flag is set (see above).
 */

/****************/
/* TRANSCLUSION */
/****************/

/*  Transclusion is dynamic insertion, into a document, of part or all of
    a different document.


    I. BASICS

    Put an include-link into the page, and at load time, the link will be
    replaced by the content it specifies.

    An include-link is a link (<a> tag) which has the `include` class, e.g.:

        <a class="include" href="/Sidenotes#comparisons"></a>

    At load time, this tag will be replaced with the `#comparisons` section of
    the /Sidenotes page.

    If the include-link’s URL (i.e., the value of its `href` attribute) has no
    hash (a.k.a. fragment identifier), then the entire page content will be
    transcluded. (If the page contains an element with the `markdownBody` ID,
    then only the contents of that element will be transcluded; otherwise, the
    contents of the `body` element will be transcluded; if neither element is
    present, then the complete contents of the page will be transcluded.)

    If the include-link’s URL has a hash, and the page content contains an
    element with an ID matching the hash, then only that element (or that
    element’s contents; see the `include-unwrap` option, below) will be
    transcluded. (If the URL has a hash but the hash does not identify any
    element contained in the page content, then behavior is as if there were
    no hash.)

    (See the ADVANCED section, below, for other ways to use an include-link’s
     URL hash to specify parts of a page to transclude.)


    II. OPTIONS

    Several optional classes modify the behavior of include-links:

    include-annotation
    include-content
        If the include-link is a full-annotated (as opposed to 
        partial-annotated) link, then instead of transcluding the linked
        content, the annotation for the linked content may be transcluded.
        The default behavior is set via the
        Transclude.transcludeAnnotationsByDefault property. If this is set to
        `true`, then annotated links transclude the annotation unless the
        `include-content` class is set (in which case they transclude their
        linked content). If it is set to `false`, then annotated links
        transclude the annotation only if the `include-annotation` class is set
        (otherwise they transclude their linked content).

    include-strict
        By default, include-linked are lazy-loaded. A lazy-loaded include-link
        will not trigger (i.e., transclude its content) immediately at load
        time. Instead, it will wait until the user scrolls down to the part of
        the page where the link is located, or pops up a popup that contains
        that part of the page, or otherwise “looks” at the include-link’s
        surrounding context. Only then will the transclusion take place.
        A strict include-link, on the other hand, triggers immediately at
        load time.

        `include-strict` implies `include-when-collapsed`, because
        otherwise odd behavior can result (eg. a 'strict' transclusion in the
        first line or two of a collapse will be visibly untranscluded; and
        collapses blocking strict transclusion can lead to unpredictable breakage
        when the contents of the transclusion are depended upon by the rest of the
        page, and collapses are added/removed by editors).

    include-when-collapsed
        Normally, an include-link that is inside a collapsed block will not
        trigger at load time; instead, it will trigger only when it is revealed
        by expansion of its containing collapse block(s). The `include-when-collapsed`
        class disables this delay, forcing the include-link to trigger at load time
        (if it is marked as `include-strict`!) even if, when loaded, it is
        within a collapsed block.

        Note that the `include-strict` and `include-when-collapsed` options are
        not mutually exclusive, and do not do the same thing.

    include-unwrap
        Normally, when an include-link’s URL specifies an element ID to
        transclude, the element with that ID is transcluded in its entirety.
        When the `include-unwrap` option is used, the element itself is
        discarded, and only the element’s contents are transcluded.

        (This option has no effect unless the include-link’s URL hash specifies
         a single element ID to transclude.)

    include-replace-container
        Normally, when transclusion occurs, the transcluded content replaces the
        include-link in the page, leaving any surrounding elements untouched.
        When the `include-replace-container` option is used, the include-link’s
        parent element, instead of just the include-link itself, is replaced by
        the transcluded content. (This means that any other contents of the
        include-link’s parent element are also discarded.)

    include-identify-not
        Normally, if the include-link has a nonempty ‘id’ attribute, and that
        ID does not occur in the transcluded content (after any unwrapping; see
        ‘include-unwrap’, above, for details), the content will be wrapped in a
        DIV element, which will be given the ID of the include-link. When the
        `include-identify-not` option is used, this will not be done.

    include-spinner-not
        Hides the “loading spinner” that is normally shown at the site of the
        include-link while content to be transcluded is being retrieved.


    III. ADVANCED

    The transclusion feature supports PmWiki-style transclude range syntax,
    very similar to the one described here:
    https://www.pmwiki.org/wiki/PmWiki/IncludeOtherPages#includeanchor

    To use transclude range syntax, an include-link’s URL should have a “double”
    hash, i.e. a hash consisting of two ‘#’-prefixed parts:

        <a class="include" href="/Sidenotes#tufte-css#tables"></a>

    This will include all parts of the "/Sidenotes" page’s content starting from
    the element with ID `tufte-css`, all the way up to (but *not* including!) the
    element with ID `tables`.

    Either the first or the second identifier (the parts after the ‘#’) may
    instead be empty. The possibilities are:

    #foo#bar
        Include everything starting from element `#foo` up to (but not
        including) element `#bar`.

    ##bar
        Include everything from the start of the page content up to (but not
        including) element `#bar`.

    #foo#
        Include everything starting from element `#foo` to the end of the page.

    ##
        Include the entire page content (same as not having a hash at all).

    In all cases, only the page content is considered, not any “page furniture”
    (i.e., only the contents of `#markdownBody`, if present; or only the
     contents of `<body>`, if present; or the whole page, otherwise).

    If an element of one of the specified IDs is not found in the page, the
    transclusion proceeds as if that identifier were empty.

    If both elements are present, but the end element does not follow the start
    element in the page order (i.e., if the start element comes after the end
    element, or if they are the same), then the transcluded content is empty.
 */

/*****************************************************************************/
/*	Extract template data from an HTML string by looking for:

	1. Elements with the `data-template-field` attribute. The value of the 
	   attribute is the data field name; the .innerHTML of the element is the 
	   field value.

	2. Elements with the `data-template-field-attributes` attribute. The value
	   of the attribute should be a comma-separated list of 
	   `FIELD_NAME=ATTRIBUTE_NAME` pairs. The first (FIELD_NAME) component of 
	   each pair is the data field name; the second (ATTRIBUTE_NAME) component
	   is the name of the attribute of the given element which holds the field
	   value.

	3. Elements with the `data-template-field-properties` attribute. The value
	   of the attribute should be a comma-separated list of 
	   `FIELD_NAME=PROPERTY_NAME` pairs. The first (FIELD_NAME) component of 
	   each pair is the data field name; the second (PROPERTY_NAME) component
	   is the name of the DOM object property of the given element which holds 
	   the field value.
 */
//	(string) => object
function templateDataFromHTML(html) {
	let dataObject = { };

	newDocument(html).querySelectorAll([
		"[data-template-field]", 
		"[data-template-fields-attributes]", 
		"[data-template-fields-properties]" 
	].join(", ")).forEach(element => {
		if (element.dataset.templateField)
			dataObject[element.dataset.templateField] = element.innerHTML;

		if (element.dataset.templateFieldAttributes) {
			element.dataset.templateFieldAttributes.split(",").forEach(templateField => {
				let [ fieldName, attributeName ] = trim(templateField).split("=");
				dataObject[fieldName] = element.getAttribute(attributeName);
			});
		}

		if (element.dataset.templateFieldProperties) {
			element.dataset.templateFieldProperties.split(",").forEach(templateField => {
				let [ fieldName, propertyName ] = trim(templateField).split("=");
				dataObject[fieldName] = element[propertyName];
			});
		}
	});

	return dataObject;
}

/******************************************************************************/
/*	Fill a template with provided reference data (supplemented by an optional 
	context object).

	Reference data may be a data object, or else an HTML string (in which case
	the templateDataFromHTML() function is used to extract data from the HTML).

	If no ‘data’ argument is provided, then the template itself will be parsed 
	to extract reference data (again, using the templateDataFromHTML() 
	function).

	(Context argument must be an object, not a string.)
 */
//	(string, string|object, object) => DocumentFragment
function fillTemplate (template, data = null, context = null) {
	if (template == null)
		return null;

	//	If no data source is provided, use the template itself as data source.
	if (data == null)
		data = template;

	//	If the data source is a string, assume it to be HTML and extract data.
	if (typeof data == "string")
		data = templateDataFromHTML(data);

	/*	Data variables specified in the provided context argument (if any)
		take precedence over the reference data.
	 */
	let value = (fieldName) => {
		return (context && context[fieldName]
				? context[fieldName]
				: (data ? data[fieldName] : null));
	};

	//	Make a copy of the template.
	let filledTemplate = template;

	/*	Conditionals. JavaScript’s regexps do not support recursion, so we
		keep running the replacement until no conditionals remain.
	 */
	let didReplace;
	do {
		didReplace = false;
		filledTemplate = filledTemplate.replace(
			/<\[IF([0-9]*)\s+(.+?)\]>(.+?)(?:<\[ELSE\1\]>(.+?))?<\[IF\1END\]>/gs, 
			(match, nestLevel, fieldName, ifValue, elseValue) => {
				didReplace = true;
				return (value(fieldName) ? (ifValue ?? "") : (elseValue ?? ""));
			});
	} while (didReplace); 

	//	Data variable substitution.
	filledTemplate = filledTemplate.replace(
		/<\{(.+?)\}>/g, 
		(match, fieldName) => (value(fieldName) ?? "")
	);

	return newDocument(filledTemplate);
}

/***********************************************************************/
/*  Replace an include-link with the given content (a DocumentFragment).
 */
//  Called by: Transclude.transclude
function includeContent(includeLink, content) {
    GWLog("includeContent", "transclude.js", 2);

    //  Just in case, do nothing if the link isn’t attached to anything.
    if (includeLink.parentElement == null)
        return;

    //  Prevent race condition, part I.
    includeLink.classList.add("include-in-progress");

    //  Document into which the transclusion is being done.
    let doc = includeLink.getRootNode();

    //  Are we including into the main page, or into a pop-frame or something?
    let includingIntoMainPage = (doc == document);

	//	WITHIN-WRAPPER MODIFICATIONS BEGIN

    //  Wrap (unwrapping first, if need be).
    let wrapper = newElement("SPAN", { "class": "include-wrapper" });
    if (   includeLink.classList.contains("include-unwrap")
        && includeLink.hash > "") {
        let section = content.querySelector(selectorFromHash(includeLink.hash));
        if (section) {
            wrapper.id = section.id;
            wrapper.append(...(section.childNodes));
        }
    } else {
        wrapper.append(content);
    }

    //  Inject.
    let replaceContainer = (   includeLink.parentElement.parentElement != null
                            && includeLink.classList.contains("include-replace-container"));
    let insertWhere = replaceContainer
                      ? includeLink.parentElement
                      : includeLink;
    insertWhere.parentElement.insertBefore(wrapper, insertWhere);

    //  Delete footnotes section, if any.
    let newContentFootnotesSection = wrapper.querySelector("#footnotes");
    if (newContentFootnotesSection)
        newContentFootnotesSection.remove();

    //  Update footnotes, if need be.
    let newFootnotesWrapper = Transclude.isAnnotationTransclude(includeLink)
                              ? null
                              : updateFootnotesAfterInclusion(wrapper, includeLink);

    //  ID transplantation.
    if (   includeLink.id > ""
        && includeLink.classList.contains("include-identify-not") == false
        && wrapper.querySelector("#" + includeLink.id) == null) {
        let idBearerBlock = newElement("DIV", { "id": includeLink.id, "class": "include-wrapper-block" });
        idBearerBlock.append(...(wrapper.childNodes));
        wrapper.append(idBearerBlock);
    }

    //  Special treatment for aux-links blocks.
    if (Extracts.isAuxLinksLink(includeLink)) {
        let auxLinksBlock = null;
        if (wrapper.firstElementChild.classList.contains("include-wrapper-block")) {
            auxLinksBlock = wrapper.firstElementChild;
        } else {
            auxLinksBlock = wrapper.closest("aux-links-append");
        }
        if (auxLinksBlock == null) {
            auxLinksBlock = newElement("DIV");
            auxLinksBlock.append(...(wrapper.childNodes));
            wrapper.append(auxLinksBlock);
        }
        let auxLinksLinkType = Extracts.auxLinksLinkType(includeLink);
        auxLinksBlock.classList.add(`aux-links-block`, `${auxLinksLinkType}-block`);
        if (auxLinksLinkType == "backlinks")
            auxLinksBlock.dataset.targetUrl = Extracts.targetOfAuxLinksLink(includeLink);
    }

    //  Fire events, if need be.
    if (includeLink.needsRewrite) {
        let flags = GW.contentDidLoadEventFlags.needsRewrite;
        if (includingIntoMainPage)
            flags |= (  GW.contentDidLoadEventFlags.fullWidthPossible
                      | GW.contentDidLoadEventFlags.collapseAllowed);

        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "transclude",
            document: wrapper,
            contentType: (Transclude.isAnnotationTransclude(includeLink) ? "annotation" : null),
            loadLocation: new URL(includeLink.href),
            baseLocation: includeLink.baseLocation,
            flags: flags
        });

        GW.notificationCenter.fireEvent("GW.contentDidInject", {
            source: "transclude",
            document: wrapper,
            mainPageContent: includingIntoMainPage
        });

        if (newFootnotesWrapper) {
            GW.notificationCenter.fireEvent("GW.contentDidLoad", {
                source: "transclude.footnotes",
                document: newFootnotesWrapper,
                loadLocation: new URL(includeLink.href),
                baseLocation: includeLink.baseLocation,
                flags: flags
            });

            GW.notificationCenter.fireEvent("GW.contentDidInject", {
                source: "transclude.footnotes",
                document: newFootnotesWrapper,
                mainPageContent: includingIntoMainPage
            });
        }
    }

	//	WITHIN-WRAPPER MODIFICATIONS END; OTHER MODIFICATIONS BEGIN

    //  Save reference for potential removal later.
    let includeLinkParentElement = includeLink.parentElement;

    //  Update TOC, if need be.
    if (includingIntoMainPage)
        updatePageTOC(wrapper, true);

    //  Remove extraneous text node after link, if any.
    if (   replaceContainer == false
        && includeLink.nextSibling
        && includeLink.nextSibling.nodeType == Node.TEXT_NODE
        && isNodeEmpty(includeLink.nextSibling))
        includeLink.parentNode.removeChild(includeLink.nextSibling);

    //  Remove link.
    if (replaceContainer == false)
        includeLink.remove();

    //  Intelligent rectification of surrounding HTML structure.
    if (   Transclude.isAnnotationTransclude(includeLink)
        && replaceContainer == false) {
        let allowedParentTags = [ "SECTION", "DIV" ];

        //  Special handling for annotation transcludes in link bibliographies.
        if (wrapper.parentElement.closest("#link-bibliography, .linkbibliography-append") != null)
            allowedParentTags.push("LI");

        while (   false == allowedParentTags.includes(wrapper.parentElement.tagName)
               && wrapper.parentElement.parentElement != null) {
            let nextNode = wrapper.nextSibling;

            wrapper.parentElement.parentElement.insertBefore(wrapper, wrapper.parentElement.nextSibling);

            if (isNodeEmpty(wrapper.previousSibling)) {
                wrapper.previousSibling.remove();
                continue;
            }

            if (nextNode == null)
                continue;

            let firstPart = wrapper.previousSibling;
            let secondPart = newElement(firstPart.tagName);
            if (firstPart.className > "")
                secondPart.className = firstPart.className;
            while (nextNode) {
                let thisNode = nextNode;
                nextNode = nextNode.nextSibling;
                secondPart.appendChild(thisNode);
            }

            if (isNodeEmpty(firstPart) == true)
                firstPart.remove();

            if (isNodeEmpty(secondPart) == false)
                wrapper.parentElement.insertBefore(secondPart, wrapper.nextSibling);
        }
    }

    //  Unwrap.
    unwrap(wrapper);

    //  Merge in added footnotes, if any.
    if (newFootnotesWrapper) {
        doc.querySelector("#footnotes > ol").append(...(newFootnotesWrapper.children));
        newFootnotesWrapper.remove();
    }

    //  Remove include-link’s container, if specified.
    if (replaceContainer)
        includeLinkParentElement.remove();

	//	OTHER MODIFICATIONS END

    //  Prevent race condition, part II.
    includeLink.classList.add("include-complete");
    includeLink.classList.remove("include-in-progress");

    //  Fire event, if need be.
    if (includeLink.needsRewrite) {
        GW.notificationCenter.fireEvent("Rewrite.contentDidChange", {
            source: "transclude",
            baseLocation: includeLink.baseLocation,
            document: doc
        });
    }
}

/**************************************************************************/
/*  Updates footnotes section after transclusion.

    Returns wrapper element of the added footnotes, if any; null otherwise.
 */
//  Called by: includeContent
function updateFootnotesAfterInclusion(newContent, includeLink) {
    GWLog("updateFootnotesAfterInclusion", "transclude.js", 2);

    let citationsInNewContent = newContent.querySelectorAll(".footnote-ref");
    let footnotesSectionInSourceDocument = Transclude.cachedDocumentForLink(includeLink).querySelector("#footnotes");
    if (   citationsInNewContent.length == 0
        || footnotesSectionInSourceDocument == null)
        return null;

    let newContentDocument = newContent.getRootNode();
    let footnotesSection = newContentDocument.querySelector(".markdownBody > #footnotes");
    if (!footnotesSection) {
        //  Construct footnotes section.
        footnotesSection = newElement("SECTION", { "id": "footnotes", "class": "footnotes", "role": "doc-endnotes" });
        footnotesSection.append(newElement("HR"));
        footnotesSection.append(newElement("OL"));

        //  Wrap.
        let footnotesSectionWrapper = newElement("SPAN", { "class": "include-wrapper" });
        footnotesSectionWrapper.append(footnotesSection);

        //  Inject.
        let markdownBody = (newContentDocument.querySelector("#markdownBody") ?? newContentDocument.querySelector(".markdownBody"));
        markdownBody.append(footnotesSectionWrapper);

        //  Fire event to trigger rewrite pass.
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "transclude.footnotesSection",
            document: footnotesSectionWrapper,
            loadLocation: new URL(includeLink.href),
            baseLocation: includeLink.baseLocation,
            flags: GW.contentDidLoadEventFlags.needsRewrite
        });

        //  Update page TOC to add footnotes section entry.
        updatePageTOC(footnotesSectionWrapper, true);

        //  Unwrap.
        unwrap(footnotesSectionWrapper);
    }

    let newFootnotesWrapper = newElement("OL", { "class": "include-wrapper" });
    let newFootnoteNumber = footnotesSection.querySelector("ol").children.length + 1;

    citationsInNewContent.forEach(citation => {
        //  Footnote.
        let footnote = footnotesSectionInSourceDocument.querySelector(citation.hash);

        //  #fnN
        citation.hash = citation.hash.slice(0, 3) + newFootnoteNumber;

        //  fnrefN
        citation.id = citation.id.slice(0, 5) + newFootnoteNumber;

        //  Link text.
        citation.firstElementChild.textContent = newFootnoteNumber;

        //  Copy the footnote.
        let newFootnote = newFootnotesWrapper.appendChild(document.importNode(footnote, true));

        //  fnN
        newFootnote.id = newFootnote.id.slice(0, 2) + newFootnoteNumber;

        //  #fnrefN
        let newFootnoteBackLink = newFootnote.querySelector("a.footnote-back");
        newFootnoteBackLink.hash = newFootnoteBackLink.hash.slice(0, 6) + newFootnoteNumber;

        //  Increment.
        newFootnoteNumber++;
    });

    footnotesSection.appendChild(newFootnotesWrapper);

    return newFootnotesWrapper;
}

/***********************************************************************/
/*  Handles interactions between include-links and content at locations.
 */
Transclude = {
    /*****************/
    /*  Configuration.
     */

    permittedClassNames: [
        "include",
        "include-annotation",
        "include-content",
        "include-strict",
        "include-when-collapsed",
        "include-unwrap",
        "include-replace-container",
        "include-identify-not",
        "include-spinner-not"
    ],

    transcludeAnnotationsByDefault: true,

    permittedContentTypes: [ "text/html" ],

    lazyLoadViewportMargin: "100%",

    /******************************/
    /*  Detection of include-links.
     */

    isIncludeLink: (link) => {
        return link.classList.containsAnyOf(Transclude.permittedClassNames);
    },

    allIncludeLinksInContainer: (container) => {
        return Array.from(container.querySelectorAll("a[class*='include']")).filter(link => Transclude.isIncludeLink(link));
    },

    isAnnotationTransclude: (includeLink) => {
        if ((Transclude.hasAnnotation(includeLink) || includeLink.classList.contains("include-annotation")) == false)
            return false;

        return ((   Transclude.transcludeAnnotationsByDefault
        		 && Transclude.hasAnnotation(includeLink))
                ? includeLink.classList.contains("include-content") == false
                : includeLink.classList.contains("include-annotation"));
    },

	hasAnnotation: (includeLink) => {
		return (Annotations.isAnnotatedLinkFull(includeLink));
	},

    /***********/
    /*  Caching.
     */

    cachedDocuments: { },

    //  Called by: Transclude.transclude
    cachedDocumentForLink: (includeLink) => {
        if (   includeLink.hostname == location.hostname
            && includeLink.pathname == location.pathname)
            return document;

        return Transclude.cachedDocuments[urlSansHash(includeLink).href];
    },

    //  Called by: Transclude.transclude
    setCachedDocumentForLink: (doc, includeLink) => {
        if (  includeLink.hostname == location.hostname
            && includeLink.pathname == location.pathname)
            return;

        Transclude.cachedDocuments[urlSansHash(includeLink).href] = doc;
    },

    cachedContent: { },

    //  Called by: Transclude.transclude
    cachedContentForLink: (includeLink) => {
        return Transclude.cachedContent[includeLink.href];
    },

    //  Called by: Transclude.transclude
    setCachedContentForLink: (content, includeLink) => {
        Transclude.cachedContent[includeLink.href] = content;
    },

    /**************/
    /*  Templating.
     */

	templateDirectoryPathname: "/static/templates/include/",
	templateListFileName: "templates.json",

	templates: { },

	loadTemplates: () => {
        GWLog("Transclude.loadTemplates", "transclude.js", 1);

		doAjax({
			location: Transclude.templateDirectoryPathname + Transclude.templateListFileName,
			onSuccess: (event) => {
				let templateList = JSON.parse(event.target.responseText);
				for (templateName of templateList)
					Transclude.loadTemplateByName(templateName);
			}
		});
	},

	loadTemplateByName: (templateName) => {
        GWLog("Transclude.loadTemplateByName", "transclude.js", 2);

		doAjax({
			location: Transclude.templateDirectoryPathname + templateName + ".tmpl",
			onSuccess: (event) => {
				Transclude.templates[templateName] = event.target.responseText;
			},
		});
	},

	//	(string, string|object, object) => DocumentFragment
	fillTemplateNamed: (templateName, data, context) => {
		return fillTemplate(Transclude.templates[templateName], data, context);
	},

    /********************************/
    /*  Retrieved content processing.
     */

    //  Called by: Transclude.transclude
    sliceContentFromDocument: (sourceDocument, includeLink) => {
        //  If it’s a full page, extract just the page content.
        let pageContent = sourceDocument.querySelector("#markdownBody") ?? sourceDocument.querySelector("body");
        let content = pageContent ? newDocument(pageContent.childNodes) : sourceDocument;

        //  If the hash specifies part of the page, extract that.
        let anchors = includeLink.hash.match(/#[^#]*/g) ?? [ ];
        if (anchors.length == 1) {
            //  Simple element tranclude.

            let section = content.querySelector(selectorFromHash(includeLink.hash));
            if (section)
                content = newDocument(section);
        } else if (anchors.length == 2) {
            //  PmWiki-like transclude range syntax.

            let startElement = anchors[0].length > 1
                               ? content.querySelector(selectorFromHash(anchors[0]))
                               : null;
            let endElement = anchors[1].length > 1
                             ? content.querySelector(selectorFromHash(anchors[1]))
                             : null;

            /*  If both ends of the range are null, we return the entire
                content.
             */
            if (   startElement == null
                && endElement == null)
                return content;

            /*  If both ends of the range exist, but the end element
                doesn’t follow the start element, we return nothing.
             */
            if (   startElement
                && endElement
                && (   startElement == endElement
                    || startElement.compareDocumentPosition(endElement) == Node.DOCUMENT_POSITION_PRECEDING))
                return newDocument();

            //  Slice.
            let slicedContent = newDocument();

            if (startElement == null) {
                //  From start to id.
                slicedContent.appendChild(content);

                let currentNode = endElement;
                while (currentNode != slicedContent) {
                    while (currentNode.nextSibling) {
                        currentNode.nextSibling.remove();
                    }
                    currentNode = currentNode.parentNode;
                }
                endElement.remove();
            } else if (endElement == null) {
                //  From id to end.
                let nodesToAppend = [ startElement ];

                let currentNode = startElement;
                while (currentNode.parentNode) {
                    while (currentNode.nextSibling) {
                        nodesToAppend.push(currentNode.nextSibling);
                        currentNode = currentNode.nextSibling;
                    }
                    currentNode = currentNode.parentNode;
                }

                nodesToAppend.forEach(node => { slicedContent.appendChild(node); });
            } else {
                //  From id to id.
                let nodesToAppend = [ ];

                /*  Node which contains both start and end elements
                    (which might be the root DocumentFragment).
                 */
                let sharedAncestor = startElement.parentNode;
                while (!sharedAncestor.contains(endElement))
                    sharedAncestor = sharedAncestor.parentNode;

                let currentNode = startElement;

                /*  The branch of the tree containing the start element
                    (if it does not also contain the end element).
                 */
                while (currentNode.parentNode != sharedAncestor) {
                    while (currentNode.nextSibling) {
                        nodesToAppend.push(currentNode.nextSibling);
                        currentNode = currentNode.nextSibling;
                    }
                    currentNode = currentNode.parentNode;
                }

                //  There might be intervening branches.
                if (!currentNode.contains(endElement)) {
                    while (!currentNode.nextSibling.contains(endElement)) {
                        currentNode = currentNode.nextSibling;
                        nodesToAppend.push(currentNode);
                    }
                    currentNode = currentNode.nextSibling;
                }

                //  The branch of the tree containing the end element.
                if (currentNode != endElement) {
                    let endBranchOrigin = currentNode;
                    currentNode = endElement;
                    while (currentNode != endBranchOrigin) {
                        while (currentNode.nextSibling) {
                            currentNode.nextSibling.remove();
                        }
                        currentNode = currentNode.parentNode;
                    }
                    endElement.remove();
                    nodesToAppend.push(endBranchOrigin);
                }

                //  Insert the start element, if not there already.
                if (!nodesToAppend.last.contains(startElement))
                    nodesToAppend.splice(0, 0, startElement);

                //  Assemble.
                nodesToAppend.forEach(node => { slicedContent.appendChild(node); });
            }

            content = slicedContent;
        }

        return content;
    },

    /*************************/
    /*  Include-link handling.
     */

    //  Called by: Transclude.transclude
    //  Called by: Transclude.triggerTranscludesInContainer
    //  Called by: handleTranscludes (rewrite function)
    transclude: (includeLink, now = false) => {
        GWLog("Transclude.transclude", "transclude.js", 2);

        /*  We don’t retry failed loads. We also skip include-links for which a
            transclude operation is already in progress or has completed (which
            might happen if we’re given an include-link to process, but that
            link has already been replaced by its transcluded content and has
            been removed from the document).
         */
        if (includeLink.classList.containsAnyOf([
            "include-loading-failed",
            "include-in-progress",
            "include-complete"
        ])) return;

		/*	If it’s an annotation transclude but doesn’t actually have an
			annotation, do nothing. (Presumably, an annotation will be added to
			this link someday.)
		 */
		if (   Transclude.isAnnotationTransclude(includeLink)
			&& Transclude.hasAnnotation(includeLink) == false)
			return;

		/*  We exclude cross-origin transclusion for security reasons, but from 
			a technical standpoint there’s no reason it shouldn’t work. Simply 
			comment out the block below to enable cross-origin transcludes.
			—SA 2022-08-18
		 */
        if (   includeLink.hostname != location.hostname
            && Transclude.isAnnotationTransclude(includeLink) == false) {
            Transclude.setLinkStateLoadingFailed(includeLink);
            return;
        }

        /*  By default, includes within collapse blocks only get transcluded
            if/when the collapse block is expanded.
         */
        if (   now == false
            && isWithinCollapsedBlock(includeLink)
            && includeLink.classList.contains("include-strict") == false
            && includeLink.classList.contains("include-when-collapsed") == false) {
            includeLink.needsRewrite = true;
            GW.notificationCenter.addHandlerForEvent("Collapse.collapseStateDidChange", (info) => {
                if (isWithinCollapsedBlock(includeLink))
                    return;

                Transclude.transclude(includeLink);
            }, { once: true });

            return;
        }

        //  Transclusion is lazy by default.
        if (   now == false
            && includeLink.classList.contains("include-strict") == false) {
            includeLink.needsRewrite = true;
            requestAnimationFrame(() => {
                lazyLoadObserver(() => {
                    Transclude.transclude(includeLink, true);
                }, includeLink, { rootMargin: Transclude.lazyLoadViewportMargin });
            });

            return;
        }

        //  Set loading state (for visual/interaction purposes).
        Transclude.setLinkStateLoading(includeLink);

        /*  Check whether provider objects for annotation extracts are loaded;
            if not, then wait until they load to attempt transclusion.
         */
        if (   Transclude.isAnnotationTransclude(includeLink)
            && (   window.Annotations == null
                || window.Extracts == null)) {
            let loadHandler = ((info) => {
                if (   window.Annotations == null
                    || window.Extracts == null)
                    return;

                GW.notificationCenter.removeHandlerForEvent("Annotations.didLoad", loadHandler);
                GW.notificationCenter.removeHandlerForEvent("Extracts.didLoad", loadHandler);

                Transclude.transclude(includeLink, true);
            });

            includeLink.needsRewrite = true;

            GW.notificationCenter.addHandlerForEvent("Annotations.didLoad", loadHandler);
            GW.notificationCenter.addHandlerForEvent("Extracts.didLoad", loadHandler);

            return;
        }

        //  Check includable content cache (managed by Transclude in all cases).
        let cachedContent = Transclude.cachedContentForLink(includeLink);
        if (cachedContent) {
            let content = newDocument(cachedContent);
            if (   Transclude.isAnnotationTransclude(includeLink)
                && includeLink.needsRewrite == false) {
                includeLink.needsRewrite = true;
                requestAnimationFrame(() => {
                    includeContent(includeLink, content);
                });
            } else {
                includeContent(includeLink, content);
            }

            return;
        }

        //  Check source document caches (depending on include type).
        if (Transclude.isAnnotationTransclude(includeLink)) {
            //  Get annotation reference data (if it’s been loaded).
            let annotationIdentifier = Extracts.targetIdentifier(includeLink);
            let referenceData = Annotations.referenceDataForAnnotationIdentifier(annotationIdentifier);
            if (referenceData == "LOADING_FAILED") {
                /*  If we’ve already tried and failed to load the annotation, we
                    will not try loading again, and just show a “loading failed”
                    message.
                 */
                Transclude.setLinkStateLoadingFailed(includeLink);

                return;
            } else if (referenceData) {
            	let templateName = (includeLink.dataset.template || referenceData.template);
                Transclude.setCachedContentForLink(Transclude.fillTemplateNamed(templateName, referenceData), 
                								   includeLink);
                Transclude.transclude(includeLink, true);

                return;
            }
        } else {
            let doc = Transclude.cachedDocumentForLink(includeLink);
            if (doc) {
                Transclude.setCachedContentForLink(Transclude.sliceContentFromDocument(doc, includeLink), includeLink);
                Transclude.transclude(includeLink, true);

                return;
            }
        }

        //  No cached documents or content; time for a network load.
        includeLink.needsRewrite = true;
        if (Transclude.isAnnotationTransclude(includeLink)) {
            let annotationIdentifier = Extracts.targetIdentifier(includeLink);

            //  Add load/fail handlers.
            GW.notificationCenter.addHandlerForEvent("Annotations.annotationDidLoad", (info) => {
                Transclude.transclude(includeLink, true);
            }, { once: true, condition: (info) => info.identifier == annotationIdentifier });
            GW.notificationCenter.addHandlerForEvent("Annotations.annotationLoadDidFail", (info) => {
                Transclude.transclude(includeLink, true);

                //  Send request to record failure in server logs.
                GWServerLogError(includeLink.href + `--annotation-transclude-failed`, "failed annotation transclude");
            }, { once: true, condition: (info) => info.identifier == annotationIdentifier });

            //  Request annotation load.
            Annotations.loadAnnotation(annotationIdentifier);
        } else {
            doAjax({
                location: includeLink.href,
                onSuccess: (event) => {
                    let contentType = event.target.getResponseHeader("Content-Type").match(/(.+?)(?:;|$)/)[1];
                    if (Transclude.permittedContentTypes.includes(contentType) == false) {
                        GWServerLogError(includeLink.href + `--transclude-bad-content-type`, "bad transclude content type");
                        Transclude.setLinkStateLoadingFailed(includeLink);

                        return;
                    }

                    Transclude.setCachedDocumentForLink(newDocument(event.target.responseText), includeLink);
                    Transclude.transclude(includeLink, true);
                },
                onFailure: (event) => {
                    Transclude.setLinkStateLoadingFailed(includeLink);
                }
            });
        }
    },

    /*****************/
    /*  Misc. helpers.
     */

    //  Called by: Extracts.localTranscludeForTarget (extracts.js)
    triggerTranscludesInContainer: (container) => {
        Transclude.allIncludeLinksInContainer(container).forEach(includeLink => {
            Transclude.transclude(includeLink, true);
        });
    },

    /********************/
    /*  Loading spinners.
     */

    //  Called by: Transclude.transclude
    setLinkStateLoading: (link) => {
        if (Transclude.isIncludeLink(link) == false)
            return;

        link.classList.add("include-loading");
        if (link.textContent > "")
            link.classList.add("icon-not");
        link.onclick = () => { return false; };
        link.savedTitle = link.title ?? "";
        link.title = "Content is loading. Please wait.";
    },

    //  Called by: Transclude.transclude
    setLinkStateLoadingFailed: (link) => {
        if (Transclude.isIncludeLink(link) == false)
            return;

        link.swapClasses([ "include-loading", "include-loading-failed" ], 1);
        if (link.textContent > "")
            link.classList.remove("icon-not");
        link.onclick = null;
        if (link.savedTitle != null) {
            link.title = link.savedTitle;
            link.savedTitle = null;
        }

        //  Fire event, if need be.
        if (link.needsRewrite) {
            GW.notificationCenter.fireEvent("Rewrite.contentDidChange", {
                source: "transclude.loadingFailed",
                baseLocation: link.baseLocation,
                document: link.getRootNode()
            });
        }
    }
};

/***************************/
/*	Load standard templates.
 */
Transclude.loadTemplates();

/****************************/
/*  Process transclude-links.
 */
addContentLoadHandler(GW.contentLoadHandlers.handleTranscludes = (loadEventInfo) => {
    GWLog("handleTranscludes", "transclude.js", 1);

    Transclude.allIncludeLinksInContainer(loadEventInfo.document).forEach(includeLink => {
        //  Store the location of the included-into document.
        includeLink.baseLocation = loadEventInfo.baseLocation;

        //  Transclude now or maybe later.
        Transclude.transclude(includeLink);
    });
}, "<rewrite");
