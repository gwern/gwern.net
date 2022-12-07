/* author: Said Achmiz */
/* license: MIT */

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
    element contained in the page content, nothing is transcluded.)

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

	include-block-context
		Normally, when an include-link’s URL specifies an element ID to 
		transclude, only (at most; see `include-unwrap`) that element is 
		transcluded. When the `include-block-context` option is used, not only
		the identified element itself, but also its containing block element
		(and everything within) will be included. (What “block element” means
		in this context is not the same as what the HTML spec means by the 
		term. Determination of what counts as a block element is done in a 
		content-aware way.)

		If `include-unwrap` is used as well as `include-block-context`, then the
		identified element’s containing block will be unwrapped, and the
		included content will be all the child nodes of the identified element’s
		containing block.

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

	include-spinner
    include-spinner-not
        Shows or hides the “loading spinner” that is shown at the site of the 
        include-link while content to be transcluded is being retrieved. In the
        absence of either of these classes, the spinner will be shown or not,
        depending on context. Using either class causes the spinner to be shown
        or not shown (respectively), unconditionally.


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
    transclusion fails.

    If both elements are present, but the end element does not follow the start
    element in the page order (i.e., if the start element comes after the end
    element, or if they are the same), then the transcluded content is empty.
 */

/******************************************************************************/
/*	Extract template data from an HTML string by looking for elements with  
	either the `data-template-field` or the `data-template-fields` attribute.

	If the `data-template-fields` attribute is not present but the 
	`data-template-field` attribute is present, then the value of the latter
	attribute is treated as the data field name; the .innerHTML of the 
	element is the field value.

	If the `data-template-fields` attribute is present, then the attribute 
	value is treated as a comma-separated list of 
	`fieldName:fieldValueIdentifier` pairs. For each pair, the part before the
	colon (the fieldName) is the data field name. The part after the colon
	(the fieldValueIdentifier) can be interpreted in one of two ways:

	If the fieldValueIdentifier begins with a dollar sign (the ‘$’ character), 
	then the rest of the identifier (after the dollar sign) is treated as the 
	name of the attribute of the given element which holds the field value.

	If the fieldValueIdentifier is _only_ the ‘$’ character, then the field 
	value will be the value of the data attribute that corresponds to the 
	field name (i.e., if the field is `fooBar`, then the field value will be
	taken from attribute `data-foo-bar`).

	If the fieldValueIdentifier begins with a period (the ‘.’ character), then
	the rest of the identifier (after the period) is treated as the name of the
	DOM object property of the given element which holds the field value.

	If the fieldValueIdentifier is _only_ the ‘.’ character, then the field 
	value will be the value of the element property matching the field name
	(i.e., if the field name is `fooBar`, then the field value will be the 
	value of the element’s .fooBar property).

	Examples:

		<span data-template-field="foo">Bar</span>

	This element defines a data field with name `foo` and value `Bar`.

		<span data-template-field="foo:$title" title="Bar"></span>

	This element defines a data field with name `foo` and value `Bar`.

		<span data-template-field="foo:$title, bar:.tagName" title="Baz"></span>

	This element defines two data fields: one with name `foo` and value `Baz`,
	and one with name `bar` and value `SPAN`.

		<span data-template-field="foo:title" title="Bar"></span>

	This element defines no data fields.
 */
//	(string) => object
function templateDataFromHTML(html) {
	let dataObject = { };

	newDocument(html).querySelectorAll("[data-template-field], [data-template-fields]").forEach(element => {
		if (element.dataset.templateFields) {
			element.dataset.templateFields.split(",").forEach(templateField => {
				let [ beforeColon, afterColon ] = templateField.trim().split(":");
				let fieldName = beforeColon.trim();
				let fieldValueIdentifier = afterColon.trim();

				if (fieldValueIdentifier.startsWith(".")) {
					dataObject[fieldName] = fieldValueIdentifier == "."
											? element[fieldName]
											: element[fieldValueIdentifier.slice(1)];
				} else if (fieldValueIdentifier.startsWith("$")) {
					dataObject[fieldName] = fieldValueIdentifier == "$"
											? element.dataset[fieldName]
											: element.getAttribute(fieldValueIdentifier.slice(1));
				}
			});
		} else {
			dataObject[element.dataset.templateField] = element.innerHTML;
		}
	});

	return dataObject;
}

/***************************************************************************/
/*	Returns true or false, based on the value of defined template expression
	constants. (Returns false for unknown constant name.)
 */
function evaluateTemplateExpressionConstant(constant) {
	if (constant == "_TRUE_")
		return true;

	if (constant == "_FALSE_")
		return false;

	return false;
}

/************************************************************************/
/*	Return either true or false, having evaluated the template expression
	(used in conditionals, e.g. `<[IF !foo & bar]>baz<[IFEND]>`).
 */
function evaluateTemplateExpression(expr, valueFunction = (() => null)) {
	if (expr == "_TRUE_")
		return true;

	if (expr == "_FALSE_")
		return false;

	return evaluateTemplateExpressionConstant(expr.replace(
		//	Brackets.
		/\s*\[\s*(.+?)\s*\]\s*/g,
		(match, bracketedExpr) => 
		(evaluateTemplateExpression(bracketedExpr, valueFunction)
		 ? "_TRUE_"
		 : "_FALSE_")
	).replace(
		//	Boolean AND, OR.
		/^\s*([^&|]+?)\s*([&|])\s*(.+?)\s*$/,
		(match, leftOperand, operator, rightOperand) => {
			let leftOperandTrue = evaluateTemplateExpression(leftOperand, valueFunction);
			let rightOperandTrue = evaluateTemplateExpression(rightOperand, valueFunction);
			let expressionTrue = operator == "&"
								 ? (leftOperandTrue && rightOperandTrue)
								 : (leftOperandTrue || rightOperandTrue);
			return expressionTrue ? "_TRUE_" : "_FALSE_";
		}
	).replace(
		//	Boolean NOT.
		/^\s*!\s*(.+?)\s*$/,
		(match, operand) =>
		(evaluateTemplateExpression(operand, valueFunction)
		 ? "_FALSE_"
		 : "_TRUE_")
	).replace(/^\s*(.+)\s*$/g,
		(match, fieldName) => 
		(/^_(.*)_$/.test(fieldName)
		 ? fieldName
		 : (valueFunction(fieldName) == null
			? "_FALSE_"
			: "_TRUE_"))
	));
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

	Available options (defaults):

		preserveSurroundingWhitespaceInConditionals (false)
			If true, `<[IF foo]> bar <[IFEND]>` becomes ` bar `; 
			if false, `bar`.

		fireContentLoadEvent (false)
			If true, a GW.contentDidLoad event is fired on the filled template.
 */
//	(string, string|object, object) => DocumentFragment
function fillTemplate (template, data = null, context = null, options = { }) {
	if (   template == null
		|| template == "LOADING_FAILED")
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
	let valueFunction = (fieldName) => {
		return (context && context[fieldName]
				? context[fieldName]
				: (data ? data[fieldName] : null));
	};

	//	Make a copy of the template.
	let filledTemplate = template;

	//	Line continuations.
	filledTemplate = filledTemplate.replace(
		/>\\\n\s*</gs,
		(match) => "><"
	);

	//	Comments.
	filledTemplate = filledTemplate.replace(
		/<\(.+?\)>/gs,
		(match) => ""
	);

	/*	Conditionals. JavaScript’s regexps do not support recursion, so we
		keep running the replacement until no conditionals remain.
	 */
	let didReplace;
	do {
		didReplace = false;
		filledTemplate = filledTemplate.replace(
			/<\[IF([0-9]*)\s+(.+?)\]>(.+?)(?:<\[ELSE\1\]>(.+?))?<\[IF\1END\]>/gs,
			(match, nestLevel, expr, ifValue, elseValue) => {
				didReplace = true;
				let returnValue = evaluateTemplateExpression(expr, valueFunction) 
								  ? (ifValue ?? "") 
								  : (elseValue ?? "");
				return options.preserveSurroundingWhitespaceInConditionals
					   ? returnValue
					   : returnValue.trim();
			});
	} while (didReplace);

	//	Data variable substitution.
	filledTemplate = filledTemplate.replace(
		/<\{(.+?)\}>/g,
		(match, fieldName) => (valueFunction(fieldName) ?? "")
	);

	let outputDocument = newDocument(filledTemplate);

	if (options.fireContentLoadEvent) {
		let loadEventInfo = {
            container: outputDocument,
            document: outputDocument
        };

		if (context.loadEventInfo)
			for ([key, value] of Object.entries(context.loadEventInfo))
				if ([ "container", "document" ].includes(key) == false)
					loadEventInfo[key] = value;

		GW.notificationCenter.fireEvent("GW.contentDidLoad", loadEventInfo);
	}

	return outputDocument;
}

/*************************************************************************/
/*	Return appropriate loadLocation for given include-link. (May be null.)
 */
function loadLocationForIncludeLink(includeLink) {
    if (Transclude.isAnnotationTransclude(includeLink) == false) {
    	contentSourceURLs = Content.sourceURLsForTarget(includeLink);
    	return contentSourceURLs
			   ? contentSourceURLs.first
			   : includeLink.eventInfo.loadLocation;
    } else {
    	return null;
    }
}

/***********************************************************************/
/*  Replace an include-link with the given content (a DocumentFragment).
 */
//  Called by: Transclude.transclude
function includeContent(includeLink, content) {
    GWLog("includeContent", "transclude.js", 2);

	//	Where to inject?
    let replaceContainer = (   includeLink.parentElement != null
                            && includeLink.classList.contains("include-replace-container"));
    let insertWhere = replaceContainer
                      ? includeLink.parentElement
                      : includeLink;

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

    /*  Just in case, do nothing if the element-to-be-replaced (either the 
    	include-link itself, or its container, as appropriate) isn’t attached 
    	to anything.
     */
    if (insertWhere.parentNode == null)
        return;

    //  Prevent race condition, part I.
    includeLink.classList.add("include-in-progress");

    //  Document into which the transclusion is being done.
    let containingDocument = includeLink.eventInfo.document;

    //  Save reference for potential removal later.
    let includeLinkParentElement = includeLink.parentElement;

	//	WITHIN-WRAPPER MODIFICATIONS BEGIN

    //  Wrap (unwrapping first, if need be).
    let wrapper = newElement("SPAN", { "class": "include-wrapper" });
    if (   includeLink.classList.contains("include-unwrap")
        && includeLink.hash > ""
        && content.childElementCount == 1) {
		wrapper.id = content.firstElementChild.id;
		wrapper.append(...content.firstElementChild.childNodes);
    } else {
        wrapper.append(content);
    }

    //  Inject wrapper.
    insertWhere.parentNode.insertBefore(wrapper, insertWhere);

    //  Delete footnotes section, if any, when transcluding into a full page.
	let newContentFootnotesSection = wrapper.querySelector("#footnotes");
    if (   newContentFootnotesSection
    	&& containingDocument.querySelector("#page-metadata") != null)
		newContentFootnotesSection.remove();

    //  ID transplantation.
    if (   includeLink.id > ""
        && includeLink.classList.contains("include-identify-not") == false
        && wrapper.querySelector("#" + includeLink.id) == null) {
        let idBearerBlock = newElement("DIV", { "id": includeLink.id, "class": "include-wrapper-block" });
        idBearerBlock.append(...wrapper.childNodes);
        wrapper.append(idBearerBlock);
    }

    //  Fire GW.contentDidInject event.
	let flags = GW.contentDidInjectEventFlags.clickable;
	if (containingDocument == document)
		flags |= GW.contentDidInjectEventFlags.fullWidthPossible;
	GW.notificationCenter.fireEvent("GW.contentDidInject", {
		source: "transclude",
		contentType: (Transclude.isAnnotationTransclude(includeLink) ? "annotation" : null),
		container: wrapper,
		document: containingDocument,
		loadLocation: loadLocationForIncludeLink(includeLink),
		flags: flags,
		includeLink: includeLink
	});

	//	WITHIN-WRAPPER MODIFICATIONS END; OTHER MODIFICATIONS BEGIN

    //  Update footnotes, if need be, when transcluding into a full page.
    if (   containingDocument.querySelector("#page-metadata") != null
    	&& Transclude.isAnnotationTransclude(includeLink) == false)
        updateFootnotesAfterInclusion(includeLink, wrapper, newContentFootnotesSection);

    //  Update TOC, if need be, when transcluding into the base page.
    if (   containingDocument == document
    	&& Transclude.isAnnotationTransclude(includeLink) == false)
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
        if (   wrapper.parentElement != null
        	&& wrapper.parentElement.closest(".link-bibliography-list") != null)
            allowedParentTags.push("LI");

        while (   wrapper.parentElement != null
               && wrapper.parentElement.parentElement != null
        	   && false == allowedParentTags.includes(wrapper.parentElement.tagName)) {
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

    //  Remove include-link’s container, if specified.
    if (replaceContainer)
        includeLinkParentElement.remove();

	//	OTHER MODIFICATIONS END

    //  Prevent race condition, part II.
    includeLink.classList.add("include-complete");
    includeLink.classList.remove("include-in-progress");

    //  Fire event, if need be.
    if (includeLink.delayed) {
        GW.notificationCenter.fireEvent("Rewrite.contentDidChange", {
            source: "transclude",
            document: containingDocument
        });
    }
}

/**************************************************************************/
/*  Updates footnotes section after transclusion.

    Returns wrapper element of the added footnotes, if any; null otherwise.
 */
//  Called by: includeContent
function updateFootnotesAfterInclusion(includeLink, newContent, newContentFootnotesSection) {
    GWLog("updateFootnotesAfterInclusion", "transclude.js", 2);

	/*	If the transcluded content didn’t include the footnotes section of the
		source page, attempt to get the footnotes section from the cached full 
		document that the new content was sliced from.
	 */
    if (   newContentFootnotesSection == null
    	&& Transclude.isAnnotationTransclude(includeLink) == false) {
    	let newContentSourceDocument = Content.cachedDocumentForLink(includeLink);
    	if (newContentSourceDocument)
    		newContentFootnotesSection = newContentSourceDocument.querySelector("#footnotes");
    }

    let citationsInNewContent = newContent.querySelectorAll(".footnote-ref");
    if (   citationsInNewContent.length == 0
        || newContentFootnotesSection == null)
        return null;

    let containingDocument = includeLink.eventInfo.document;

    let footnotesSection = containingDocument.querySelector(".markdownBody > #footnotes");
    if (!footnotesSection) {
        //  Construct footnotes section.
        footnotesSection = newElement("SECTION", { "id": "footnotes", "class": "footnotes", "role": "doc-endnotes" });
        footnotesSection.append(newElement("HR"));
        footnotesSection.append(newElement("OL"));

        //  Wrap.
        let footnotesSectionWrapper = newElement("SPAN", { "class": "include-wrapper" });
        footnotesSectionWrapper.append(footnotesSection);

        //  Inject.
        let markdownBody = (containingDocument.querySelector("#markdownBody") ?? containingDocument.querySelector(".markdownBody"));
        markdownBody.append(footnotesSectionWrapper);

        //  Fire events.
        GW.notificationCenter.fireEvent("GW.contentDidLoad", {
            source: "transclude.footnotesSection",
            container: footnotesSectionWrapper,
            document: containingDocument
        });
		GW.notificationCenter.fireEvent("GW.contentDidInject", {
			source: "transclude.footnotesSection",
			container: footnotesSectionWrapper,
			document: containingDocument,
            flags: 0
		});

        //  Update page TOC to add footnotes section entry.
        updatePageTOC(footnotesSectionWrapper, true);

        //  Unwrap.
        unwrap(footnotesSectionWrapper);
    }

	//	Construct wrapper.
    let newFootnotesWrapper = newElement("OL", { "class": "include-wrapper" });

	//	Add new footnotes to wrapper.
    citationsInNewContent.forEach(citation => {
        //  Original footnote (in source content/document).
        let footnote = newContentFootnotesSection.querySelector(Notes.footnoteSelectorMatching(citation));

        //  Copy the footnote.
        let newFootnote = newFootnotesWrapper.appendChild(document.importNode(footnote, true));
    });

	//	Inject wrapper.
    footnotesSection.appendChild(newFootnotesWrapper);

	//	Fire GW.contentDidLoad event.
	GW.notificationCenter.fireEvent("GW.contentDidLoad", {
		source: "transclude.footnotes",
		container: newFootnotesWrapper,
		document: containingDocument,
		loadLocation: loadLocationForIncludeLink(includeLink)
	});

	//	Parent element of footnotes.
	let footnotesList = footnotesSection.querySelector("ol");

	//	Merge and unwrap.
	footnotesList.append(...(newFootnotesWrapper.children));

	//	Re-number citations/footnotes, and re-order footnotes.
	let footnoteNumber = 1;
	containingDocument.querySelectorAll(".footnote-ref").forEach(citation => {
		let footnote = footnotesSection.querySelector(Notes.footnoteSelectorMatching(citation));

		Notes.setCitationNumber(citation, footnoteNumber);
		Notes.setFootnoteNumber(footnote, footnoteNumber);

		newFootnotesWrapper.appendChild(footnote);

		footnoteNumber++;
	});

	//	Fire inject event.
	let flags = GW.contentDidInjectEventFlags.clickable;
	if (containingDocument == document)
		flags |= GW.contentDidInjectEventFlags.fullWidthPossible;
	GW.notificationCenter.fireEvent("GW.contentDidInject", {
		source: "transclude.footnotes",
		container: newFootnotesWrapper,
		document: containingDocument,
		loadLocation: loadLocationForIncludeLink(includeLink),
		flags: flags
	});

	//	Merge and unwrap (redux).
	footnotesList.append(...(newFootnotesWrapper.children));

	//	Discard wrapper.
	newFootnotesWrapper.remove();
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
        "include-block-context",
        "include-replace-container",
        "include-identify-not"
    ],

    transcludeAnnotationsByDefault: true,

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

    /**************/
    /*  Templating.
     */

	templateDirectoryPathname: "/static/templates/include/",
	templateListFileName: "templates.json",

	templates: { },

	loadTemplates: () => {
        GWLog("Transclude.loadTemplates", "transclude.js", 1);

		doAjax({
			location: versionedAssetURL(Transclude.templateDirectoryPathname + Transclude.templateListFileName).href,
			responseType: "json",
			onSuccess: (event) => {
				let templateList = event.target.response;
				for (templateName of templateList)
					Transclude.loadTemplateByName(templateName);
			}
		});
	},

	loadTemplateByName: (templateName) => {
        GWLog("Transclude.loadTemplateByName", "transclude.js", 2);

		doAjax({
			location: versionedAssetURL(Transclude.templateDirectoryPathname + templateName + ".tmpl").href,
			responseType: "text",
			onSuccess: (event) => {
				Transclude.templates[templateName] = event.target.response;

				GW.notificationCenter.fireEvent("Transclude.templateDidLoad", {
					source: "Transclude.loadTemplateByName",
					templateName: templateName
				});
			},
			onFailure: (event) => {
				Transclude.templates[templateName] = "LOADING_FAILED";

				GW.notificationCenter.fireEvent("Transclude.templateLoadDidFail", {
					source: "Transclude.loadTemplateByName",
					templateName: templateName
				});
			}
		});
	},

	doWhenTemplateLoaded: (templateName, loadHandler, loadFailHandler = null) => {
		let template = Transclude.templates[templateName];
		if (template == "LOADING_FAILED") {
			if (loadFailHandler)
				loadFailHandler();
		} else if (template) {
			loadHandler();
		} else {
			let loadOrFailHandler = (info) => {
				if (info.eventName == "Transclude.templateDidLoad") {
					loadHandler(true);

					GW.notificationCenter.removeHandlerForEvent("Transclude.templateLoadDidFail", loadOrFailHandler);
				} else {
					if (loadFailHandler) 
						loadFailHandler(true);

					GW.notificationCenter.removeHandlerForEvent("Transclude.templateDidLoad", loadOrFailHandler);
				}
			};
			GW.notificationCenter.addHandlerForEvent("Transclude.templateDidLoad", loadOrFailHandler, {
				once: true,
				condition: (info) => info.templateName == templateName
			});
			GW.notificationCenter.addHandlerForEvent("Transclude.templateLoadDidFail", loadOrFailHandler, {
				once: true,
				condition: (info) => info.templateName == templateName
			});
		}
	},

	//	(string, string|object, object) => DocumentFragment
	fillTemplateNamed: (templateName, data, context, options) => {
		return fillTemplate(Transclude.templates[templateName], data, context, options);
	},

    /********************************/
    /*  Retrieved content processing.
     */

    //  Called by: Transclude.transclude
    sliceContentFromDocument: (sourceDocument, includeLink) => {
        //  If it’s a full page, extract just the page content.
        let pageContent = sourceDocument.querySelector("#markdownBody") ?? sourceDocument.querySelector("body");
        let content = pageContent ? newDocument(pageContent.childNodes) : newDocument(sourceDocument);

        //  If the hash specifies part of the page, extract that.
        let anchors = includeLink.hash.match(/#[^#]*/g) ?? [ ];
        if (anchors.length == 1) {
            //  Simple element tranclude.
            let targetElement = targetElementInDocument(includeLink, content);
            if (targetElement == null) {
            	content = newDocument();
            } else {
				//	Optional block context.
				if (includeLink.classList.contains("include-block-context")) {
					let nearestBlock = nearestBlockElement(targetElement);
					if (nearestBlock) {
						content = newDocument(nearestBlock);
					} else {
						content = newDocument(targetElement);
					}
				} else {
					content = newDocument(targetElement);
				}
            }
        } else if (anchors.length == 2) {
            //  PmWiki-like transclude range syntax.

			//	Start element.
			let startElement = null;
			if (anchors[0].length > 1) {
				startElement = content.querySelector(selectorFromHash(anchors[0]));

				//	If specified but missing, transclude nothing.
				if (startElement == null)
					return newDocument();
			}

			//	End element.
			let endElement = null;
			if (anchors[1].length > 1) {
				endElement = content.querySelector(selectorFromHash(anchors[1]));

				//	If specified but missing, transclude nothing.
				if (endElement == null)
					return newDocument();
			}

            /*  If both ends of the range are unspecified, we return the entire
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
            includeLink.delayed = true;
            GW.notificationCenter.addHandlerForEvent("Collapse.collapseStateDidChange", (info) => {
                Transclude.transclude(includeLink);
            }, { 
            	once: true,
            	condition: (info) => (isWithinCollapsedBlock(includeLink) == false)
            });

            return;
        }

        //  Transclusion is lazy by default.
        if (   now == false
            && includeLink.classList.contains("include-strict") == false) {
            includeLink.delayed = true;
            requestAnimationFrame(() => {
                lazyLoadObserver(() => {
                    Transclude.transclude(includeLink, true);
                }, includeLink, { rootMargin: Transclude.lazyLoadViewportMargin });
            });

            return;
        }

        //  Set loading state (for visual/interaction purposes).
        Transclude.setLinkStateLoading(includeLink);

        /*  Check whether provider object is loaded; if not, then wait until it 
        	loads to attempt transclusion.
         */
        let providerObjectName = Transclude.isAnnotationTransclude(includeLink)
        						 ? "Annotations"
        						 : "Content";
        if (window[providerObjectName] == null) {
			includeLink.delayed = true;
			GW.notificationCenter.addHandlerForEvent(`${providerObjectName}.didLoad`, (info) => {
				Transclude.transclude(includeLink, true);
			}, { once: true });

			return;
        }

		//	Get data provider object and data identifier.
		let provider = window[providerObjectName];
		let identifier = provider.targetIdentifier(includeLink);

		//	Request data load, if need be.
		if (provider.cachedDataExists(identifier) == false) {
			provider.load(identifier);
	        includeLink.delayed = true;
		}

		//	When data loads (or if it is already loaded), transclude.
		provider.waitForDataLoad(identifier, 
		   (identifier) => {
		   	//	Load success handler.

			if (Transclude.isAnnotationTransclude(includeLink)) {
				let referenceData = Annotations.referenceDataForTarget(includeLink);

				let templateName = (includeLink.dataset.template || referenceData.template);
				let linkTemplateData = templateDataFromHTML(includeLink.outerHTML);
				let context = {
					loadEventInfo: {
						source: "transclude",
						contentType: "annotation",
						includeLink: includeLink
					}
				};
				for ([key, value] of Object.entries(linkTemplateData)) {
					context[key] = value;
				}
				let options = {
					fireContentLoadEvent: true
				};
				Transclude.doWhenTemplateLoaded(templateName, (delayed) => {
					let content = Transclude.fillTemplateNamed(templateName, referenceData, context, options);

					if (content == null) {
						Transclude.setLinkStateLoadingFailed(includeLink);

						//	Send request to record failure in server logs.
						GWServerLogError(includeLink.href + `--annotation-transclude-template-fill-failed`, 
										 "failed annotation transclude template fill");
					} else {
						includeLink.delayed = true;
						requestAnimationFrame(() => {
							includeContent(includeLink, content);
						});
					}
				}, (delayed) => {
					Transclude.setLinkStateLoadingFailed(includeLink);

					//	Send request to record failure in server logs.
					GWServerLogError(templateName + `--include-template-load-failed`, 
									 "failed include template load");
				});

				return;
			} else {
				let cachedDocument = Content.cachedDocumentForLink(includeLink);
				let content = Transclude.sliceContentFromDocument(cachedDocument, includeLink);
				includeContent(includeLink, content);

				return;
			}
		}, (identifier) => {
		   	//	Load fail handler.

			/*  If we’ve already tried and failed to load the content, we
				will not try loading again, and just show a “loading failed”
				message.
			 */
			Transclude.setLinkStateLoadingFailed(includeLink);

			//  Send request to record failure in server logs.
			GWServerLogError(includeLink.href + `--transclude-failed`, 
							 "failed transclude");

			return;
		});
    },

    /*****************/
    /*  Misc. helpers.
     */

    //  Called by: "beforeprint" listener (rewrite.js)
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

		if (link.classList.containsAnyOf([ "include-spinner", "include-spinner-not" ]) == false) {
			/*	Add loading spinner for whole-page transcludes and anything
				within a link bibliography.
			 */
			if (   link.hash == ""
				|| link.closest(".link-bibliography-list-list") != null)
				link.classList.add("include-spinner");
		}

        link.classList.add("include-loading");
        if (   link.classList.contains("include-spinner")
        	&& link.textContent > "")
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
        if (link.delayed) {
            GW.notificationCenter.fireEvent("Rewrite.contentDidChange", {
                source: "transclude.loadingFailed",
                document: link.eventInfo.document
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
addContentLoadHandler(GW.contentLoadHandlers.handleTranscludes = (eventInfo) => {
    GWLog("handleTranscludes", "transclude.js", 1);

    Transclude.allIncludeLinksInContainer(eventInfo.container).forEach(includeLink => {
		//	Store a reference to the load event info.
		includeLink.eventInfo = eventInfo;

        //  Transclude now or maybe later.
        Transclude.transclude(includeLink);
    });
}, "transclude");

/*************************************************************/
/*	Re-process when injecting. (Necessary for cloned content.)
 */
addContentInjectHandler(GW.contentInjectHandlers.handleTranscludes = GW.contentLoadHandlers.handleTranscludes, "rewrite");
