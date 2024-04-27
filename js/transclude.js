/* author: Said Achmiz */
/* license: MIT */

/****************/
/* TRANSCLUSION */
/****************/

/*  Transclusion is dynamic insertion, into a document, of part or all of
    a different document.


    I. BASICS
    =========

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
    ===========

    Several optional classes modify the behavior of include-links:

    include-annotation
    include-content
        If the include-link is an annotated link, then instead of transcluding 
        the linked content, the annotation for the linked content may be 
        transcluded.

        The default behavior is set via the
        Transclude.transcludeAnnotationsByDefault property. If this is set to
        `true`, then fully (not partially!) annotated links transclude the 
        annotation unless the `include-content` class is set (in which case they 
        transclude their linked content). If it is set to `false`, then fully
        annotated links transclude the annotation only if the 
        `include-annotation` class is set (otherwise they transclude their 
        linked content).

		Note that merely partially annotated links always default to 
		transcluding content, unless the `include-annotation` class is set.
		(See also the `include-annotation-partial` alias class.)

    include-strict
        By default, include-links are lazy-loaded. A lazy-loaded include-link
        will not trigger (i.e., transclude its content) immediately at load
        time. Instead, it will wait until the user scrolls down to the part of
        the page where the link is located, or pops up a popup that contains
        that part of the page, or otherwise “looks” at the include-link’s
        surrounding context. Only then will the transclusion take place.
        A strict include-link, on the other hand, triggers immediately at
        load time.

        Note that `include-strict` implies `include-even-when-collapsed`, 
        because otherwise odd behavior can result (eg. a ‘strict’ transclusion 
        in the first line or two of a collapse will be visibly untranscluded; 
        and collapses blocking strict transclusion can lead to unpredictable 
        breakage when the contents of the transclusion are depended upon by the 
        rest of the page, and collapses are added/removed by editors).

	include-lazy
		By default, include-links are loaded when they are within some scroll
		distance away from the view rect of their scroll container (i.e., the
		viewport, or the frame of a pop-frame, etc.); this is done so that the 
		transcluded content is likely to already be loaded by the time the user
		scrolls to the include-link’s position in the document flow.

		The `include-lazy` option makes the transclusion behavior lazier than 
		usual; an include-link with this class will trigger only when it crosses
		the boundary of the viewport (or the scroll container’s view rect).

		Note that if the `include-strict` option is set, then `include-lazy`
		will have no effect. Similarly, if the `include-even-when-collapsed`
		option is *not* set (assuming that `include-strict` is also not set), 
		then `include-lazy` will have no effect if the include-link is within 
		a collapsed block.

    include-even-when-collapsed
        Normally, an include-link that is inside a collapsed block will not
        trigger at load time; instead, it will trigger only when it is revealed
        by expansion of its containing collapse block(s). The 
        `include-even-when-collapsed` class disables this delay, forcing the 
        include-link to trigger when revealed by scrolling (if it is not marked 
        as `include-strict`; otherwise, `include-strict` will force the 
        include-link to trigger at load time, regardless of anything to do with
        collapses) even if, at such time, it is within a collapsed block.

        Note that the `include-strict` and `include-even-when-collapsed` options 
        do not do the same thing; the former implies the latter, but not the
        other way around.

    include-unwrap
        Normally, when an include-link’s URL specifies an element ID to
        transclude, the element with that ID is transcluded in its entirety.
        When the `include-unwrap` option is used, the element itself is
        discarded, and only the element’s contents are transcluded.

        (This option has no effect unless the include-link’s URL hash specifies
         a single element ID to transclude.)

	include-block-context
	data-block-context-options
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

		The `data-block-context-options` attribute allows various options to be 
		specified for how block context should be determined and handled. The
		value of this attribute is a pipe (`|`) separated list of option fields.
		The following options may be specified:

		expanded
			Expanded block context mode omits paragraphs (the <p> element) from
			consideration as containing blocks.

    include-replace-container
    data-replace-container-selector
    data-replace-container-options
        Normally, when transclusion occurs, the transcluded content replaces the
        include-link in the page, leaving any surrounding elements untouched.
        When the `include-replace-container` option is used, the include-link’s
        parent element, instead of just the include-link itself, is replaced by
        the transcluded content. (This means that any other contents of the
        include-link’s parent element are also discarded.) If the include-link
        has no parent element (if, for example, it is an immediate child node of
        a DocumentFragment), then it will simply behave normally (as if the 
        `include-replace-container option was not set at all).

		The `data-replace-container-selector` attribute allows specification of
		a list of CSS selectors that should be used to locate the ‘container’ 
		element to be replaced by the transcluded content (rather than simply 
		using the include-link’s immediate parent). The value of this attribute 
		is a pipe (`|`) separated list of CSS selectors. Each selector is 
		checked in turn; if no containing element matching the given selector is 
		found, the next selector is checked, and so on. If no containing element 
		matching any of the given selectors is found, then the include-link will 
		default to replacing its immediate parent element (as if the 
		`data-replace-container-selector` attribute were absent). (But see the 
		`strict` option of the `data-replace-container-options` attribute, 
		below).

		The `data-replace-container-options` attribute allows various options to 
		be specified for how container replacement should proceed. The value of 
		this attribute is a pipe (`|`) separated list of option fields. The 
		following options may be specified:

		strict
			If this option is specified, and a value is provided for the 
			`data-replace-container-selector` attribute, and no containing 
			element matching any of the specified selectors is found, then the
			include-link will *not* default to replacing its immediate parent
			(as is the default behavior), but will instead replace nothing 
			(i.e., it will behave as if the `include-replace-selector` option
			were not specified at all).

	include-rectify-not
		Normally, when transclusion occurs, the surrounding HTML structure is
		intelligently rectified, to preserve block containment rules and so on.
		When the `include-rectify-not` option is used, this rectification is
		not done.

		(Not currently used on gwern.net.)

    include-identify-not
        Normally, if the include-link has a nonempty ‘id’ attribute, and that
        ID does not occur in the transcluded content (after any unwrapping; see
        ‘include-unwrap’, above, for details), the content will be wrapped in a
        DIV element, which will be given the ID of the include-link. When the
        `include-identify-not` option is used, this will not be done.

		(Not currently used on gwern.net.)

	include-localize-not
		When content specified by an include-link is transcluded into the base
		page, and the transcluded content has headings, should those headings be 
		added to the page’s table of contents? When transcluded content has 
		footnote references, should those citations be integrated into the host
		page’s footnote numbering, and should the associated footnotes be added
		to the host page’s footnotes section?

		Normally, the answer (and it’s the same answer for both questions, and 
		several related ones such as link qualification) is determined on the 
		basis of the content type of the transcluded content, the context in 
		which it’s being transcluded (e.g., a backlink context block), and some
		other factors. If the `include-localize-not` option is used, however,
		the content will NOT be “localized”, no matter what other conditions
		may obtain.

	include-spinner
    include-spinner-not
        Shows or hides the “loading spinner” that is shown at the site of the
        include-link while content to be transcluded is being retrieved. In the
        absence of either of these classes, the spinner will be shown or not,
        depending on context. Using either class causes the spinner to be shown
        or not shown (respectively), unconditionally.

		(Note that these two classes, unlike the others listed in this section,
		 DO NOT mark a link as an include-link. They must be used in conjunction
		 with the `include` class, or with one or more of the optional include
		 classes listed here.)


    III. ADVANCED
    =============

	1. Transclude range syntax
	--------------------------

    The transclusion feature supports PmWiki-style transclude range syntax,
    very similar to the one described here:
    https://www.pmwiki.org/wiki/PmWiki/IncludeOtherPages#includeanchor

    To use transclude range syntax, an include-link’s URL should have a “double”
    hash, i.e. a hash consisting of two ‘#’-prefixed parts:

        <a class="include" href="/Sidenotes#tufte-css#tables"></a>

    This will include all parts of the "/Sidenotes" page’s content starting from
    the element with ID `tufte-css`, all the way up to (but *not* including!) 
    the element with ID `tables`.

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

	2. Include template
	-------------------

	The `data-include-template` attribute allows selection of include template 
	to use.

	(Note that some include data sources specify a template by default;
	 the `data-include-template` attribute overrides the default in such cases.)

	If a template is specified, the included content is treated as a template
	data source, rather than being included directly. (See comment for the
	templateDataFromHTML() function for information about how template data
	is specified in HTML. Note that some data sources provide template data in
	pre-constructed object form, which bypasses the need to extract it from
	HTML source.)

	If the value of this attribute begins with the ‘$’ character, then the rest
	if the attribute value (after the dollar sign) is treated as a key into the 
	template data object, rather than directly as the name of a template file.
	This allows a template data source to specify different templates for use
	in different contexts. (For example, a template data source may specify a
	default template, to be used when transcluding normally, and a different 
	template to be used when the transcluded content is to be used as the 
	content of a pop-frame. In such a case, the template data object might have
	a field with key `popFrameTemplate` whose value is the name of a template,
	and the include-link’s `data-include-template` attribute would have a value
	of `$popFrameTemplate`.)

	3. Selector-based inclusion/exclusion
	-------------------------------------

	The `data-include-selector` and `data-include-selector-not` attributes allow
	the use of CSS selectors to specify parts of the included DOM subtree to 
	include or omit. (If both attributes are present, 
	`data-include-selector-not` is applied first.)

	The `data-include-selector-options`, `data-include-selector-not-options`,
	and `data-include-selector-general-options` attributes allows various 
	options to be specified for how the selectors should be applied. The values 
	of these attributes are pipe (`|`) separated lists of option fields. The 
	`-options` version of the attribute applies only to `data-include-selector`;
	`-not-options` applies only to `data-include-selector-not`; and 
	`-general-options` applies to both. (The specific options attributes take
	precedence over the general options attribute.)

	The following options may be specified:

	first
		Select only the first element matching the specified selector, instead 
		of selecting all matching elements. (In other words, use querySelector()
		instead of querySelectorAll().)

	(NOTE: `data-include-selector` may be seen as a generalization of the 
	 `include-block-context` option, described above. Note, however, that both
	 `include-block-context` and either or both of `data-include-selector` / 
	 `data-include-selector-not` may be used simultaneously. The effects of the 
	 data attributes are applied last, after all `include-*` options have been
	 applied.)


	IV. ALIASES
	===========

	The following classes, set on include-links, function as aliases for various
	combinations of the above-described functionality. Each entry below lists
	the alias class (or set of multiple specific classes, in some cases), 
	followed by the combination of classes, data attributes, etc. to which the 
	alias is equivalent. Some entries also include usage notes.

	class="include-block-context-expanded"

		class="include-block-context"
		data-block-context-options="expanded"

		“Expanded block context” typically means “broaden the block context 
		beyond a single paragraph”.

	class="include-annotation-partial"

		class="include-annotation"
		data-include-selector-not=".annotation-abstract, .file-includes, figure, .data-field-separator"
		data-template-fields="annotationClassSuffix:$"
		data-annotation-class-suffix="-partial"

		Includes only the metadata of annotations (omitting the annotation 
		abstract, i.e. the body of the annotation, if any). Formats the included
		annotation as a partial.

	class="include-annotation-core"

		class="include-annotation"
		data-include-selector=".annotation-abstract, .file-includes"

		Essentially the opposite of .include-annotation-partial; includes only
		the annotation abstract, omitting metadata. (If there is no abstract - 
		i.e., if the annotation is a partial - the included content will be
		empty.)

	class="include-content-core"

		class="include-content"
		data-include-selector-not="#footnotes, #backlinks-section, 
			#similars-section, #link-bibliography-section, 
			#page-metadata .link-tags, #page-metadata .page-metadata-fields"

		Include a page’s content, omitting “auxiliary” content sections
		(Footnotes, Further Reading, Backlinks, Link Bibliography), as well as
		the page tags and the date/status/confidence/importance/etc. metadata
		fields block.

	class="include-content-no-header"

		class="include-unwrap"
		data-include-selector-not="h1, h2, h3, h4, h5, h6"
		data-include-selector-not-options="first"

		Applied to an include-link that targets a <section>, will include only 
		the content of the section; the <section> will be unwrapped, and the 
		heading discarded. (If applied in some other case, behavior may be
		unpredictable.)

	class="include-caption-not"

		data-include-selector-not=".caption-wrapper"

		Normally, media (image, video, audio) include-links which have 
		annotations will, when transcluded, get a <figcaption> whose contents 
		are the abstract of the annotation. If the `include-caption-not` class
		is set, the caption is omitted. (This class has no effect if applied to
		include-links of non-media content types.)
 */

/******************************************************************************/
/*	Extract template data from an HTML string or DOM object by looking for
	elements with either the `data-template-field` or the
	`data-template-fields` attribute.

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

		<span data-template-fields="foo:$title" title="Bar"></span>

			This element defines one data field, with name `foo` and value `Bar`.

		<span data-template-fields="foo:$title, bar:.tagName" title="Baz"></span>

			This element defines two data fields: one with name `foo` and value 
			`Baz`,and one with name `bar` and value `SPAN`.

		<span data-template-field="foo:title" title="Bar"></span>

			This element defines no data fields. (Likely this is a typo, and 
			the desired attribute name is actually `data-template-fields`; note
			the plural form.)
 */
//	(string|Document|DocumentFragment|Element) => object
function templateDataFromHTML(html) {
	let dataObject = { };

	if ((   html instanceof Document
		 || html instanceof DocumentFragment) == false)
		html = newDocument(html);

	html.querySelectorAll("[data-template-field], [data-template-fields]").forEach(element => {
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

/************************************************************************/
/*	Return either true or false, having evaluated the template expression
	(used in conditionals, e.g. `<[IF !foo & bar]>baz<[IFEND]>`).
 */
function evaluateTemplateExpression(expr, valueFunction = (() => null)) {
	if (expr == "_TRUE_")
		return true;

	if (expr == "_FALSE_")
		return false;

	return evaluateTemplateExpression(expr.replace(
		//	Quotes.
		/(['"])(.*?)(\1)/g,
		(match, leftQuote, quotedExpr, rightQuote) => 
		"<<" + fixedEncodeURIComponent(quotedExpr) + ">>"
	).replace(
		//	Brackets.
		/\s*\[\s*(.+?)\s*\]\s*/g,
		(match, bracketedExpr) =>
		(evaluateTemplateExpression(bracketedExpr, valueFunction)
		 ? "_TRUE_"
		 : "_FALSE_")
	).replace(
		//	Boolean AND, OR.
		/\s*([^&|]+?)\s*([&|])\s*(.+)\s*/g,
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
		/\s*!\s*(\S+)\s*/g,
		(match, operand) =>
		(evaluateTemplateExpression(operand, valueFunction)
		 ? "_FALSE_"
		 : "_TRUE_")
	).replace(
		//	Comparison.
		/\s*(\S+)\s+(\S+)\s*/,
		(match, leftOperand, rightOperand) => {
			let constantRegExp = new RegExp(/^_(\S*)_$/);
			if (   constantRegExp.test(leftOperand)
				|| constantRegExp.test(rightOperand)) {
				return (   evaluateTemplateExpression(leftOperand, valueFunction)
						== evaluateTemplateExpression(rightOperand, valueFunction)
						? "_TRUE_"
						: "_FALSE_");
			} else {
				let literalRegExp = new RegExp(/^<<(.*)>>$/);
				leftOperand = literalRegExp.test(leftOperand)
							  ? decodeURIComponent(leftOperand.slice(2, -2))
							  : valueFunction(leftOperand);
				rightOperand = literalRegExp.test(rightOperand)
							   ? decodeURIComponent(rightOperand.slice(2, -2))
							   : valueFunction(rightOperand);
				return (leftOperand == rightOperand
						? "_TRUE_"
						: "_FALSE_");
			}
		}
	).replace(/\s*(\S+)\s*/g,
		//	Constant or field name.
		(match, constantOrFieldName) =>
		(/^_(\S*)_$/.test(constantOrFieldName)
		 ? constantOrFieldName
		 : (valueFunction(constantOrFieldName)
			? "_TRUE_"
			: "_FALSE_"))
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
//	(string, string|object, object, object) => DocumentFragment
function fillTemplate(template, data = null, context = null, options = { }) {
	if (   template == null
		|| template == "LOADING_FAILED")
		return null;

	//	If no data source is provided, use the template itself as data source.
	if (   data == null
		|| data == "LOADING_FAILED")
		data = template;

	/*	If the data source is a string, assume it to be HTML and extract data;
		likewise, if the data source is a DocumentFragment, extract data.
	 */
	if (   typeof data == "string"
		|| data instanceof DocumentFragment)
		data = templateDataFromHTML(data);

	/*	Data variables specified in the provided context argument (if any)
		take precedence over the reference data.
	 */
	let valueFunction = (fieldName) => {
		return (context && context[fieldName]
				? context[fieldName]
				: (data ? data[fieldName] : null));
	};

	//	Line continuations.
	template = template.replace(
		/>\\\n\s*</gs,
		(match) => "><"
	);

	//	Comments.
	template = template.replace(
		/<\(.+?\)>/gs,
		(match) => ""
	);

	//	Escapes.
	template = template.replace(
		/\\(.)/gs,
		(match, escaped) => "<[:" + escaped.codePointAt(0) + ":]>"
	);

	/*	Conditionals. JavaScript’s regexps do not support recursion, so we
		keep running the replacement until no conditionals remain.
	 */
	let didReplace;
	do {
		didReplace = false;
		template = template.replace(
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
	template = template.replace(
		/<\{(.+?)\}>/g,
		(match, fieldName) => (valueFunction(fieldName) ?? "")
	);

	//	Escapes, redux.
	template = template.replace(
		/<\[:(.+?):\]>/gs,
		(match, codePointSequence) => String.fromCodePoint(...(codePointSequence.split("/").map(x => parseInt(x))))
	);

	//	Construct DOM tree from filled template.
	let outputDocument = newDocument(template);

	//	Fire GW.contentDidLoad event, if need be.
	if (options.fireContentLoadEvent) {
		let loadEventInfo = {
            container: outputDocument,
            document: outputDocument
        };

		if (options.loadEventInfo)
			for (let [key, value] of Object.entries(options.loadEventInfo))
				if ([ "container", "document" ].includes(key) == false)
					loadEventInfo[key] = value;

		GW.notificationCenter.fireEvent("GW.contentDidLoad", loadEventInfo);
	}

	return outputDocument;
}

/*****************************************************************************/
/*	Construct synthetic include-link. The optional ‘link’ argument may be
	a string, a URL object, or an HTMLAnchorElement, in which case it, or its
	.href property, is used as the ‘href’ attribute of the synthesized
	include-link.
 */
function synthesizeIncludeLink(link, attributes, properties) {
	let includeLink = newElement("A", attributes, properties);

	if (link == null)
		return includeLink;

	if (typeof link == "string") {
		includeLink.href = link;
	} else if (link instanceof HTMLAnchorElement) {
		includeLink.href = link.getAttribute("href");
	} else if (link instanceof URL) {
		includeLink.href = link.href;
	} else {
		return null;
	}

	if (link instanceof HTMLAnchorElement) {
		//	Import certain data attributes.
		[ "linkContentType", 
		  "backlinkTargetUrl", 
		  "urlArchive", 
		  "urlHtml" 
		  ].forEach(dataAttributeName => {
			if (link.dataset[dataAttributeName])
				includeLink.dataset[dataAttributeName] = link.dataset[dataAttributeName];
		});
		//  Import certain link classes.
		/*  See corresponding note in annotations.js.
			—SA 2024-02-16
		 */
		[ "link-live", 
		  "link-page", 
		  "link-dropcap", 
		  "link-annotated", 
		  "link-annotated-partial", 
		  "content-transform-not" 
		  ].forEach(targetClass => {
			if (link.classList.contains(targetClass))
				includeLink.classList.add(targetClass);
		});
	}

	//	In case no include classes have been added yet...
	if (Transclude.isIncludeLink(includeLink) == false)
		includeLink.classList.add("include");

	return includeLink;
}

/*************************************************************************/
/*	Return appropriate loadLocation for given include-link. (May be null.)
 */
function loadLocationForIncludeLink(includeLink) {
    if (Transclude.isAnnotationTransclude(includeLink) == false) {
    	return (   Content.sourceURLsForLink(includeLink)?.first 
    			?? includeLink.eventInfo.loadLocation);
    } else {
    	return null;
    }
}

/*******************************************************************************/
/*	Return appropriate contentType string for given include-link. (May be null.)
 */
function contentTypeIdentifierForIncludeLink(includeLink) {
	let contentType = null;

	if (   Transclude.isAnnotationTransclude(includeLink)
		|| (   Content.contentTypes.localFragment.matches(includeLink)
			&& /^\/metadata\/annotation\/[^\/]+$/.test(includeLink.pathname))) {
		contentType = "annotation";
	} else {
		let referenceData = Transclude.dataProviderForLink(includeLink).referenceDataForLink(includeLink);
		if (   referenceData 
			&& referenceData.contentTypeClass != null)
			contentType = referenceData.contentTypeClass.replace(/([a-z])-([a-z])/g, (match, p1, p2) => (p1 + p2.toUpperCase()));
	}

	return contentType;
}

/*****************************************************************/
/*	Standardized parsing for a pipe (`|`) separated options field.
	(Returns null if no non-whitespace options are provided.)
 */
function parsePipedOptions(attributeValue) {
	return attributeValue?.split("|").map(x => x.trim()).filter(x => x > "");
}

/******************************************************************************/
/*	Returns true if content specified by the given include-link should be
	“localized” (i.e., integrated into the page structure - footnotes, table of
	contents, etc. - of the document into which it is being transcluded); false
	otherwise.
 */
function shouldLocalizeContentFromLink(includeLink) {
	if (includeLink.classList.contains("include-localize-not"))
		return false;

	if (includeLink.eventInfo.localize == false)
		return false;

	if (Transclude.dataProviderForLink(includeLink).shouldLocalizeContentFromLink?.(includeLink) == false)
		return false;

	return true;
}

/***********************************************************************/
/*  Replace an include-link with the given content (a DocumentFragment).
 */
//  Called by: Transclude.transclude
function includeContent(includeLink, content) {
    GWLog("includeContent", "transclude.js", 2);

	/*  We skip include-links for which a transclude operation is already in
		progress or has completed (which might happen if we’re given an
		include-link to process, but that link has already been replaced by its
		transcluded content and has been removed from the document).
	 */
	if (includeLink.classList.containsAnyOf([
		"include-in-progress",
		"include-complete"
	])) return;

	//	Where to inject?
	let insertWhere = includeLink;
	if (includeLink.classList.contains("include-replace-container")) {
		/*	This code block implements the `include-replace-container` class
			and the `data-replace-container-selector` and
			`data-replace-container-options` attributes.
			(See documentation, at the start of this file, for details.)
		 */

		//	Parse `data-replace-container-options` attribute.
		let replaceContainerOptions = parsePipedOptions(includeLink.dataset.replaceContainerOptions);

		//	Find the container to replace.
		let replacedContainer = null;
		let replaceContainerSelectors = parsePipedOptions(includeLink.dataset.replaceContainerSelector);
		if (replaceContainerSelectors) {
			/*	If `data-replace-container-selector` is specified, we try each 
				specified selector in turn...
			 */
			while (   replacedContainer == null
				   && replaceContainerSelectors.length > 0)
				replacedContainer = includeLink.closest(replaceContainerSelectors.shift());

			/*	... and if they all fail, we default to the immediate parent, 
				or else no replacement at all (the `strict` option).
			 */
			if (   replacedContainer == null
				&& replaceContainerOptions?.includes("strict") != true)
				replacedContainer = includeLink.parentElement;
		} else {
			/*	If `data-replace-container-selector` is not specified, we simply
				replace the immediate parent.
			 */
			replacedContainer = includeLink.parentElement;
		}

		if (replacedContainer)
			insertWhere = replacedContainer; 
	}

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
    let transcludingIntoFullPage = (containingDocument.querySelector("#page-metadata") != null);

	//	WITHIN-WRAPPER MODIFICATIONS BEGIN

    //  Wrap (unwrapping first, if need be).
    let wrapper = newElement("SPAN", { "class": "include-wrapper" });
    if (   includeLink.classList.contains("include-unwrap")
        && isAnchorLink(includeLink)
        && content.childElementCount == 1) {
		wrapper.id = content.firstElementChild.id;
		wrapper.append(...content.firstElementChild.childNodes);
    } else {
        wrapper.append(content);
    }

    //  Inject wrapper.
    insertWhere.parentNode.insertBefore(wrapper, insertWhere);

    /*  When transcluding into a full page, delete various “metadata” sections
    	such as page-metadata, footnotes, etc. (Save references to some.)
     */
	let newContentFootnotesSection = wrapper.querySelector("#footnotes");
    if (transcludingIntoFullPage) {
    	let metadataSectionsSelector = [
    		"#page-metadata",
    		"#footnotes",
    		"#further-reading",
    		"#similars-section",
    		"#link-bibliography-section"
    	].join(", ");
    	wrapper.querySelectorAll(metadataSectionsSelector).forEach(section => {
    		section.remove();
    	});
    }

    //  ID transplantation.
    if (   includeLink.id > ""
        && includeLink.classList.contains("include-identify-not") == false
        && wrapper.querySelector(`#${(CSS.escape(includeLink.id))}`) == null) {
        let idBearerBlock = newElement("DIV", { "id": includeLink.id, "class": "include-wrapper-block" });
        idBearerBlock.append(...wrapper.childNodes);
        wrapper.append(idBearerBlock);
    }

	//	Clear loading state of all include-links.
	Transclude.allIncludeLinksInContainer(wrapper).forEach(Transclude.clearLinkState);

	//	Determine whether to “localize” content.
	let shouldLocalize = shouldLocalizeContentFromLink(includeLink);

    //  Fire GW.contentDidInject event.
	let flags = GW.contentDidInjectEventFlags.clickable;
	if (containingDocument == document)
		flags |= GW.contentDidInjectEventFlags.fullWidthPossible;
	if (shouldLocalize)
		flags |= GW.contentDidInjectEventFlags.localize;
	GW.notificationCenter.fireEvent("GW.contentDidInject", {
		source: "transclude",
		contentType: contentTypeIdentifierForIncludeLink(includeLink),
		context: includeLink.eventInfo.context,
		container: wrapper,
		document: containingDocument,
		loadLocation: loadLocationForIncludeLink(includeLink),
		flags: flags,
		includeLink: includeLink
	});

	//	WITHIN-WRAPPER MODIFICATIONS END; OTHER MODIFICATIONS BEGIN

    //  Remove extraneous text node after link, if any.
    if (   includeLink.classList.contains("include-replace-container") == false
        && includeLink.nextSibling
        && includeLink.nextSibling.nodeType == Node.TEXT_NODE) {
        let cleanedNodeContents = Typography.processString(includeLink.nextSibling.textContent, Typography.replacementTypes.CLEAN);
        if (   cleanedNodeContents.match(/\S/) == null
        	|| cleanedNodeContents == ".")
	        includeLink.parentNode.removeChild(includeLink.nextSibling);
    }

    //  Remove include-link (along with container, if specified).
    insertWhere.remove();

    //  Intelligent rectification of surrounding HTML structure.
    if (   includeLink.classList.contains("include-rectify-not") == false
    	&& firstBlockOf(wrapper) != null) {
        let allowedParentTags = [ "SECTION", "DIV", "LI", "BLOCKQUOTE", "FIGCAPTION" ];
        while (   wrapper.parentElement != null
               && allowedParentTags.includes(wrapper.parentElement.tagName) == false
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

	//	Distribute backlinks, if need be.
	if (   transcludingIntoFullPage
		&& loadLocationForIncludeLink(includeLink) != null
		&& AuxLinks.auxLinksLinkType(includeLink) == "backlinks"
		&& wrapper.closest("#backlinks-section") != null)
		distributeSectionBacklinks(includeLink, wrapper);

    //  Update footnotes, if need be, when transcluding into a full page.
    if (   transcludingIntoFullPage
		&& shouldLocalize)
        updateFootnotesAfterInclusion(includeLink, wrapper, newContentFootnotesSection);

	//  Update TOC, if need be, when transcluding into the base page.
    if (   containingDocument == document
		&& shouldLocalize)
        updatePageTOCIfNeeded(wrapper);

	//	Aggregate margin notes.
	aggregateMarginNotesIfNeededInDocument(containingDocument);

	//	Import style sheets, if need be.
	if (   containingDocument == document
		|| containingDocument instanceof ShadowRoot)
		importStylesAfterTransclusion(includeLink);

	//	OTHER MODIFICATIONS END

	//	Retain reference to nodes.
	let addedNodes = Array.from(wrapper.childNodes);

    //  Unwrap.
    unwrap(wrapper);

    //  Prevent race condition, part II.
    includeLink.swapClasses([ "include-in-progress", "include-complete" ], 1);

    //  Fire event, if need be.
    if (includeLink.delayed) {
        GW.notificationCenter.fireEvent("Rewrite.contentDidChange", {
            source: "transclude",
            document: containingDocument,
            nodes: addedNodes
        });
    }

	//	Activity ends.
	endActivity();
}

/*****************************************************************************/
/*	Distributes, to each section of the page, all backlinks that point to that
	section specifically.
 */
function distributeSectionBacklinks(includeLink, mainBacklinksBlockWrapper) {
	let containingDocument = includeLink.eventInfo.document;

	let prefix = `gwern-${(loadLocationForIncludeLink(includeLink).pathname.slice(1))}-`;

	mainBacklinksBlockWrapper.querySelectorAll(".backlink-context a[data-target-id]").forEach(backlinkContextLink => {
		let id = backlinkContextLink.dataset.targetId.slice(prefix.length);
		if (id == "")
			return;

		let targetElement = containingDocument.querySelector(`#${(CSS.escape(id))}`);
		if (targetElement == null)
			return;

		let targetBlock = targetElement.closest("section, li.footnote");
		if (targetBlock == null)
			return;

		let backlinksBlock = targetBlock.querySelector(".section-backlinks");
		if (backlinksBlock == null) {
			//	Backlinks block.
			backlinksBlock = newElement("DIV", { "class": "section-backlinks", "id": `${id}-backlinks` });

			//	Label.
			backlinksBlock.append(mainBacklinksBlockWrapper.querySelector("#backlinks").firstElementChild.cloneNode(true));
			let sectionLabelLinkTarget = baseLocationForDocument(containingDocument).pathname + "#" + targetBlock.id;
			let sectionLabelHTML = targetBlock.tagName == "SECTION"
								   ? `“${(targetBlock.firstElementChild.textContent)}”`
								   : `footnote <span class="footnote-number">${(Notes.noteNumberFromHash(targetBlock.id))}</span>`;
			backlinksBlock.querySelector("p strong").innerHTML = `Backlinks for <a href="${sectionLabelLinkTarget}" class="link-page">${sectionLabelHTML}</a>:`;

			//	List.
			backlinksBlock.append(newElement("UL", { "class": "aux-links-list backlinks-list" }));

			//	Collapse wrapper.
			let collapseWrapper = newElement("DIV", { "class": "collapse aux-links-append section-backlinks-container" });
			collapseWrapper.append(backlinksBlock);

			//	Include wrapper.
			let includeWrapper = newElement("DIV", { "class": "include-wrapper section-backlinks-include-wrapper" });
			includeWrapper.append(collapseWrapper);
			let container = targetBlock.classList.contains("collapse")
							? (targetBlock.querySelector(".collapse-content-wrapper") ?? targetBlock)
							: targetBlock;
			container.append(includeWrapper);
		}

		backlinksBlock.querySelector(".backlinks-list").append(backlinkContextLink.closest("li").cloneNode(true));
	});

	containingDocument.querySelectorAll(".section-backlinks-include-wrapper").forEach(includeWrapper => {
		//	Clear loading state of all include-links.
		Transclude.allIncludeLinksInContainer(includeWrapper).forEach(Transclude.clearLinkState);

		//	Fire load event.
		GW.notificationCenter.fireEvent("GW.contentDidLoad", {
			source: "transclude.section-backlinks",
			container: includeWrapper,
			document: containingDocument,
			loadLocation: loadLocationForIncludeLink(includeLink)
		});

		//	Fire inject event.
		let flags = GW.contentDidInjectEventFlags.clickable;
		if (containingDocument == document)
			flags |= GW.contentDidInjectEventFlags.fullWidthPossible;
		GW.notificationCenter.fireEvent("GW.contentDidInject", {
			source: "transclude.section-backlinks",
			container: includeWrapper,
			document: containingDocument,
			loadLocation: loadLocationForIncludeLink(includeLink),
			flags: flags
		});

		unwrap(includeWrapper);
	});
}

/*****************************************************************************/
/*	Returns true iff a given document contains a style sheet identified by the
	given selector.
 */
function documentHasStyleSheet(doc, selector) {
	if (doc == document)
		return (doc.head.querySelector(selector) != null);
	else if (doc instanceof ShadowRoot)
		return (doc.body.querySelector(selector) != null);
	else
		return false;
}

/*****************************************************************************/
/*	Imports needed styles (<style> and/or <link> elements) after transclusion.
 */
function importStylesAfterTransclusion(includeLink) {
	let containingDocument = includeLink.eventInfo.document;
	let newContentSourceDocument = Transclude.dataProviderForLink(includeLink).cachedDocumentForLink(includeLink);

	if (newContentSourceDocument == null)
		return;

	let styleDefs = [
		[ "#mathjax-styles", ".mjpage" ]
	];

	styleDefs.forEach(styleDef => {
		let [ styleSheetSelector, elementSelector ] = styleDef;
		let stylesheet = newContentSourceDocument.querySelector(styleSheetSelector);
		if (   stylesheet
			&& (elementSelector 
				? containingDocument.querySelector(elementSelector) != null
				: true)) {
			/*	Add stylesheet to root document in all cases, if need be.
				(If this is not done, then fonts will not be loaded.)
			 */
			if (documentHasStyleSheet(document, styleSheetSelector) == false)
				document.head.append(stylesheet.cloneNode(true));

			/*	If containing document is a shadow root, give it a copy of the
				style sheet also.
			 */
			if (containingDocument instanceof ShadowRoot)
				containingDocument.insertBefore(stylesheet.cloneNode(true), containingDocument.body);
		}
	});
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
        return;

    let containingDocument = includeLink.eventInfo.document;

	//	If the host page doesn’t have a footnotes section, construct one.
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
        updatePageTOCIfNeeded(footnotesSectionWrapper);

        //  Unwrap.
        unwrap(footnotesSectionWrapper);
    }

	//	Construct wrapper.
    let newFootnotesWrapper = newElement("OL", { "class": "include-wrapper" });

	//	Add new footnotes to wrapper.
    citationsInNewContent.forEach(citation => {
        //  Original footnote (in source content/document).
        let footnote = newContentFootnotesSection.querySelector(Notes.footnoteSelectorMatching(citation));

		//	Determine footnote’s source page, and its note number on that page.
		let sourcePagePathname = (footnote.dataset.sourcePagePathname ?? loadLocationForIncludeLink(includeLink).pathname);
		let originalNoteNumber = (footnote.dataset.originalNoteNumber ?? Notes.noteNumber(citation));

		//	Check for already added copy of this footnote.
		let alreadyAddedFootnote = footnotesSection.querySelector(`li.footnote`
								 + `[data-source-page-pathname='${(CSS.escape(sourcePagePathname))}']`
								 + `[data-original-note-number='${originalNoteNumber}']`);

        //  Copy the footnote, or keep a pointer to it.
        citation.footnote = (alreadyAddedFootnote ?? newFootnotesWrapper.appendChild(document.importNode(footnote, true)));

		if (alreadyAddedFootnote == null) {
			//	Record source page and original number.
			citation.footnote.dataset.sourcePagePathname = sourcePagePathname;
			citation.footnote.dataset.originalNoteNumber = originalNoteNumber;
		}
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
		if (citation.closest(".sidenote"))
			return;

		let footnote = citation.footnote ?? footnotesSection.querySelector(Notes.footnoteSelectorMatching(citation));

		if (footnote.parentElement == newFootnotesWrapper) {
			Notes.setCitationNumber(citation, Notes.noteNumber(footnote));
		} else {
			Notes.setCitationNumber(citation, footnoteNumber);
			Notes.setFootnoteNumber(footnote, footnoteNumber);

			newFootnotesWrapper.appendChild(footnote);

			footnoteNumber++;
		}
	});

	//	Fire inject event.
	let flags = (  GW.contentDidInjectEventFlags.clickable
				 | GW.contentDidInjectEventFlags.localize);
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
        "include-lazy",
        "include-even-when-collapsed",
        "include-unwrap",
        "include-block-context",
        "include-replace-container",
        "include-rectify-not",
        "include-identify-not",
        "include-localize-not"
    ],

    transcludeAnnotationsByDefault: true,

    defaultLoadViewportMargin: "100%",

    /******************************/
    /*  Detection of include-links.
     */

    isIncludeLink: (link) => {
        return link.classList.containsAnyOf(Transclude.permittedClassNames);
    },

    allIncludeLinksInContainer: (container) => {
        return Array.from(container.querySelectorAll("a[class*='include']")).filter(link => Transclude.isIncludeLink(link));
    },

	isContentTransclude: (link) => {
		if (Transclude.isIncludeLink(link) == false)
			return false;

        if ((   Transclude.hasFullAnnotation(link) 
        	 || link.classList.contains("include-annotation")
        	 ) == false)
            return true;

		return ((   Transclude.transcludeAnnotationsByDefault
				 && Transclude.hasFullAnnotation(link))
				? link.classList.contains("include-content") == true
				: link.classList.contains("include-annotation") == false);
	},

    isAnnotationTransclude: (link) => {
		if (Transclude.isIncludeLink(link) == false)
			return false;

        if ((   Transclude.hasFullAnnotation(link) 
        	 || link.classList.contains("include-annotation")
        	 ) == false)
            return false;

        return ((   Transclude.transcludeAnnotationsByDefault
        		 && Transclude.hasFullAnnotation(link))
                ? link.classList.contains("include-content") == false
                : link.classList.contains("include-annotation") == true);
    },

	hasAnnotation: (link) => {
		return (Annotations.isAnnotatedLink(link));
	},

	hasFullAnnotation: (link) => {
		return (Annotations.isAnnotatedLinkFull(link));
	},

    /**************/
    /*  Templating.
     */

	templates: { },

	doWhenTemplateLoaded: (templateName, loadHandler, loadFailHandler = null) => {
		let template = Transclude.templates[templateName];
		if (template == "LOADING_FAILED") {
			if (loadFailHandler)
				loadFailHandler();
		} else if (template) {
			loadHandler(template);
		} else {
			let loadOrFailHandler = (info) => {
				if (info.eventName == "Transclude.templateDidLoad") {
					loadHandler(Transclude.templates[templateName], true);

					GW.notificationCenter.removeHandlerForEvent("Transclude.templateLoadDidFail", loadOrFailHandler);
				} else {
					if (loadFailHandler)
						loadFailHandler(null, true);

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

	//	Used in: Transclude.blockContext
	specificBlockElementSelectors: [
		[	".footnote",
			".sidenote"
			].join(", "),
		".aux-links-append"
	],

	generalBlockElementSelectors: [
		"figure",
		"li",
		"p",
		"blockquote",
		[	"section",
			".markdownBody > *",
// 			".include-wrapper-block",
			].join(", ")
	],

	notBlockElementSelector: [
		".annotation .data-field"
	].join(", "),

	generalBlockContextMinimumLength: 200,

	//	Called by: Transclude.sliceContentFromDocument
	blockContext: (element, includeLink) => {
		let block = null;

		let selectors = [ ...Transclude.specificBlockElementSelectors, ...Transclude.generalBlockElementSelectors ];

		/*	Parse and process block context options (if any) specified by the 
			include-link. (See documentation for the .include-block-context
			class for details.)
		 */
		let options = parsePipedOptions(includeLink.dataset.blockContextOptions);

		//	Expanded mode.
		if (options?.includes("expanded")) {
			//	Remove `p`, to prioritize selectors for enclosing elements.
			selectors.remove("p");

			//	Re-add `p` as a last-resort selector.
			selectors.push("p");
		}

		for (let selector of selectors)
			if (   (block = element.closest(selector) ?? block)
				&& block.matches(Transclude.notBlockElementSelector) == false)
// 				if (   Transclude.specificBlockElementSelectors.includes(selector)
// 					|| block.textContent.length > Transclude.generalBlockContextMinimumLength
// 					|| (   block.parentNode == null
// 						|| block.parentNode instanceof Element == false))
					break;

		let blockContext = newDocument([ "BLOCKQUOTE", "LI" ].includes(block.tagName)
									   ? block.childNodes
									   : block);

		/*	Remove any child sections. (We know the target element is not
			contained within them, because if it were, then *that* section would
			be the block context. So, any child sections are necessarily 
			extraneous.)
			(Do not do this if the section itself is the target element.)
		 */
		if (   block.tagName == "SECTION"
			&& element != block) {
			blockContext.querySelectorAll("section section").forEach(childSection => {
				childSection.remove();
			});
		}

		return blockContext;
	},

    //  Called by: Transclude.sliceContentFromDocument
	isSliceable: (includeLink) => {
		let dataProvider = Transclude.dataProviderForLink(includeLink);
		switch (dataProvider) {
		case Content:
			return Content.contentTypeForLink(includeLink).isSliceable;
		case Annotations:
			return true;
		}
	},

    //  Called by: Transclude.transclude
    sliceContentFromDocument: (sourceDocument, includeLink) => {
		//	Check if slicing is permitted.
		if (Transclude.isSliceable(includeLink) == false)
			return newDocument(sourceDocument);

        //  If it’s a full page, extract just the page content.
        let pageContent = sourceDocument.querySelector("#markdownBody") ?? sourceDocument.querySelector("body");
        let content = pageContent ? newDocument(pageContent.childNodes) : newDocument(sourceDocument);

        //  If the link’s anchor(s) specify part of the page, extract that.
        let anchors = anchorsForLink(includeLink);
        if (anchors.length == 2) {
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
                    || startElement.compareDocumentPosition(endElement) & Node.DOCUMENT_POSITION_PRECEDING))
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
        } else if (isAnchorLink(includeLink)) {
            //  Simple element tranclude.
            let targetElement = targetElementInDocument(includeLink, content);
            if (targetElement) {
				//	Optional block context.
            	/*	Check for whether the target element is *itself* an
            		include-link which will bring in a content block. If so,
            		do not include any *additional* block context, even if
            		the include-link we’re currently processing requests it!
            	 */
				let isBlockTranscludeLink = (   Transclude.isIncludeLink(targetElement)
											 && (   targetElement.classList.contains("include-block-context")
												 || (   targetElement.id > ""
													 && targetElement.classList.contains("include-identify-not") == false)));

				/*	We do not want to transclude annotations within backlink
					context. So, we will transform an annotation include link 
					in such a case into a normal link, and include its block
					context normally.
				 */
				if (   isBlockTranscludeLink
					&& Transclude.isAnnotationTransclude(targetElement)
					&& includeLink.closest(".backlink-context") != null) {
					Transclude.clearLinkState(targetElement);
					Transclude.stripIncludeClassesFromLink(targetElement);
					isBlockTranscludeLink = false;
				}

				if (   includeLink.classList.contains("include-block-context")
					&& isBlockTranscludeLink == false) {
					content = Transclude.blockContext(targetElement, includeLink);
					if (content) {
						//	Mark targeted element, for styling purposes.
						targetElement = targetElementInDocument(includeLink, content);
						if (targetElement)
							targetElement.classList.add("block-context-highlighted");
					} else {
						content = newDocument(targetElement);
					}
				} else {
					content = newDocument(targetElement);
				}
            } else {
            	content = newDocument();

            	reportBrokenAnchorLink(includeLink);
            }
        }

		//	Apply `data-include-selector-not` attribute.
		if (includeLink.dataset.includeSelectorNot) {
			/*	Parse and process selector inclusion options (if any) specified 
				by the include-link. (See documentation for selector-based
				inclusion for details.)
			 */
			let options = parsePipedOptions(   includeLink.dataset.includeSelectorNotOptions
											|| includeLink.dataset.includeSelectorGeneralOptions);
			let elementsToExclude = [ ];
			if (options?.includes("first")) {
				let element = content.querySelector(includeLink.dataset.includeSelectorNot);
				if (element)
					elementsToExclude.push(element);
			} else {
				content.querySelectorAll(includeLink.dataset.includeSelectorNot).forEach(element => {
					if (elementsToExclude.findIndex(x => x.contains(element)) === -1)
						elementsToExclude.push(element);
				});
			}
			elementsToExclude.forEach(element => {
				element.remove();
			});
		}

		//	Apply `data-include-selector` attribute.
		if (includeLink.dataset.includeSelector) {
			/*	Parse and process selector inclusion options (if any) specified 
				by the include-link. (See documentation for selector-based
				inclusion for details.)
			 */
			let options = parsePipedOptions(   includeLink.dataset.includeSelectorOptions
											|| includeLink.dataset.includeSelectorGeneralOptions);
			let elementsToInclude = [ ];
			if (options?.includes("first")) {
				let element = content.querySelector(includeLink.dataset.includeSelector);
				if (element)
					elementsToInclude.push(element);
			} else {
				content.querySelectorAll(includeLink.dataset.includeSelector).forEach(element => {
					if (elementsToInclude.findIndex(x => x.contains(element)) === -1)
						elementsToInclude.push(element);
				});
			}
			content.replaceChildren(...elementsToInclude);
		}

        return content;
    },

    /*************************/
    /*  Include-link handling.
     */

	dataProviderNameForLink: (includeLink) => {
		return (Transclude.isAnnotationTransclude(includeLink)
				? "Annotations"
				: "Content");
	},

	dataProviderForLink: (includeLink) => {
		return window[Transclude.dataProviderNameForLink(includeLink)];
	},

	doWhenDataProviderLoaded: (includeLink, loadHandler) => {
		GW.notificationCenter.addHandlerForEvent(`${(Transclude.dataProviderNameForLink(includeLink))}.didLoad`, 
												 loadHandler, 
												 { once: true });
	},

	//  Enable alias classes for various forms of includes.
	includeLinkAliasTransforms: [ ],

	addIncludeLinkAliasClass: (aliasClass, linkTransform) => {
		Transclude.permittedClassNames.push(aliasClass);
		Transclude.includeLinkAliasTransforms.push([ aliasClass, linkTransform ]);
	},

	resolveIncludeLinkAliasClasses: (includeLink) => {
		Transclude.includeLinkAliasTransforms.forEach(alias => {
			let [ aliasClass, linkTransform ] = alias;
			if (includeLink.classList.contains(aliasClass)) {
				linkTransform(includeLink);
				includeLink.classList.remove(aliasClass);
			}
		});
	},

    //  Called by: Transclude.transclude
    //  Called by: Transclude.triggerTranscludesInContainer
    //  Called by: handleTranscludes (rewrite function)
    transclude: (includeLink, now = false) => {
        GWLog("Transclude.transclude", "transclude.js", 2);

		//	Resolve alias classes.
		Transclude.resolveIncludeLinkAliasClasses(includeLink);

		/*  We don’t retry failed loads, nor do we replicate ongoing loads.
         */
        if (   now == false
        	&& includeLink.classList.containsAnyOf([
        	"include-loading",
            "include-loading-failed"
        ])) return;

		/*	We do not attempt to transclude annotation transclude links which 
			do not (according to their set-by-the-server designation) actually 
			have any annotation.
		 */
		if (   Transclude.isAnnotationTransclude(includeLink)
			&& Transclude.hasAnnotation(includeLink) == false)
			return;

        /*  By default, includes within collapse blocks only get transcluded
            if/when the collapse block is expanded.
         */
        if (   now == false
            && isWithinCollapsedBlock(includeLink)
            && includeLink.classList.contains("include-strict") == false
            && includeLink.classList.contains("include-even-when-collapsed") == false) {
            includeLink.delayed = true;
            GW.notificationCenter.addHandlerForEvent("Collapse.collapseStateDidChange", (info) => {
                Transclude.transclude(includeLink);
            }, {
            	once: true,
            	condition: (info) => (isWithinCollapsedBlock(includeLink) == false)
            });

            return;
        }

        //  Set loading state.
        Transclude.setLinkStateLoading(includeLink);

        //  Transclusion is lazy by default.
        if (   now == false
            && includeLink.classList.contains("include-strict") == false) {
            includeLink.delayed = true;
            requestIdleCallback(() => {
                lazyLoadObserver(() => {
                    Transclude.transclude(includeLink, true);
                }, includeLink, {
                	root: scrollContainerOf(includeLink),
                	rootMargin: (includeLink.classList.contains("include-lazy")
                				 ? "0px"
                				 : Transclude.defaultLoadViewportMargin)
                });
            });

            return;
        }

		//	Get data provider.
		let dataProvider = Transclude.dataProviderForLink(includeLink);
        if (dataProvider == null) {
			/*  If data provider is not loaded, wait until it loads to attempt 
				transclusion.
			 */
			includeLink.delayed = true;
			Transclude.doWhenDataProviderLoaded(includeLink, (info) => {
				Transclude.transclude(includeLink, true);
			});

			return;
        }

		//	Activity begins.
		beginActivity();

		//	Request data load, if need be.
		if (dataProvider.cachedDataExists(includeLink) == false) {
			dataProvider.load(includeLink);
	        includeLink.delayed = true;
		}

		//	When data loads (or if it is already loaded), transclude.
		let processData = (template) => {
			//	Reference data.
			let referenceData = dataProvider.referenceDataForLink(includeLink);

			let content = null;
			if (template) {
				//	Template fill context.
				let context = Object.assign({ }, referenceData, templateDataFromHTML(includeLink));

				//	Template fill options.
				let options = {
					fireContentLoadEvent: true,
					loadEventInfo: {
						source: "transclude",
						contentType: contentTypeIdentifierForIncludeLink(includeLink),
						includeLink: includeLink
					}
				};

				//	Fill template.
				content = fillTemplate(template, referenceData.content, context, options);
			} else if (referenceData.content instanceof DocumentFragment) {
				content = referenceData.content;
			}

			//	Slice and include, or else handle failure.
			if (content) {
				includeContent(includeLink, Transclude.sliceContentFromDocument(content, includeLink));
			} else {
				Transclude.setLinkStateLoadingFailed(includeLink);

				//	Send request to record failure in server logs.
				GWServerLogError(includeLink.href + `--transclude-template-fill-failed`,
								 "failed transclude template fill");
			}
		};
		dataProvider.waitForDataLoad(includeLink, (link) => {
		   	//	Load success handler.

			/*	If a template is specified by name, then we’ll need to make sure
				that it’s loaded before we can fill it with data.
			 */
			let referenceData = dataProvider.referenceDataForLink(includeLink);
			let templateName = includeLink.dataset.includeTemplate || referenceData.template;
			if (templateName) {
				while (templateName.startsWith("$"))
					templateName = referenceData[templateName.slice(1)] || referenceData.template;

				Transclude.doWhenTemplateLoaded(templateName, (template, delayed) => {
					if (delayed)
						includeLink.delayed = true;

					processData(template);
				}, (delayed) => {
					Transclude.setLinkStateLoadingFailed(includeLink);

					//	Send request to record failure in server logs.
					GWServerLogError(templateName + `--include-template-load-failed`,
									 "failed include template load");
				});
			} else {
				processData();
			}
		}, (link) => {
		   	//	Load fail handler.
		   	endActivity();

			/*  If we’ve already tried and failed to load the content, we
				will not try loading again, and just show a “loading failed”
				message.
			 */
			Transclude.setLinkStateLoadingFailed(includeLink);

			//  Send request to record failure in server logs.
			GWServerLogError(includeLink.href + `--transclude-failed`,
							 "failed transclude");
		});
    },

    /*****************/
    /*  Misc. helpers.
     */

    //  Called by: "beforeprint" listener (rewrite.js)
    triggerTranscludesInContainer: (container, eventInfo) => {
        Transclude.allIncludeLinksInContainer(container).forEach(includeLink => {
        	if (eventInfo)
        		includeLink.eventInfo = eventInfo;

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

		//	Designate loading state.
        link.classList.add("include-loading");

		//	Intelligently add loading spinner, unless override class set.
		if (link.classList.containsAnyOf([ "include-spinner", "include-spinner-not" ]) == false) {
			/*	Add loading spinner for link bibliography entries and also any
				include-link not within a collapsed block.
			 */
			if (isWithinCollapsedBlock(link) == false) {
				link.classList.add("include-spinner");
			} else {
				let containingAuxLinksBlock = link.closest(".aux-links-list, .aux-links-append");
				if (   containingAuxLinksBlock
					&& containingAuxLinksBlock.classList.contains("link-bibliography-list")) {
					link.classList.add("include-spinner");
				}
			}
		}

		//	Disable link icon, if loading spinner present.
        if (   link.classList.contains("include-spinner")
        	&& link.textContent > "")
            link.classList.add("icon-not");

		//	Disable normal link functionality.
        link.onclick = () => { return false; };

		//	Set temporary tooltip.
        link.savedTitle = link.title ?? "";
        link.title = "Content is loading. Please wait.";
    },

    //  Called by: Transclude.transclude
    setLinkStateLoadingFailed: (link) => {
        if (Transclude.isIncludeLink(link) == false)
            return;

		//	Record load failure.
        link.swapClasses([ "include-loading", "include-loading-failed" ], 1);

		//	Revert to normal link functionality.
		Transclude.resetLinkBehavior(link);

        //  Fire event, if need be.
        if (link.delayed) {
            GW.notificationCenter.fireEvent("Rewrite.contentDidChange", {
                source: "transclude.loadingFailed",
                document: link.eventInfo.document,
                nodes: [ link ]
            });
        }
    },

    //  Called by: includeContent
	clearLinkState: (link) => {
        if (Transclude.isIncludeLink(link) == false)
            return;

		//	Clear classes.
		link.classList.remove("include-loading", "include-loading-failed");

		//	Revert to normal link functionality.
		Transclude.resetLinkBehavior(link);
	},

	//	Called by: Transclude.setLinkStateLoadingFailed
	//	Called by: Transclude.clearLinkState
	resetLinkBehavior: (link) => {
		//	Re-enable link icon.
        if (link.textContent > "")
            link.classList.remove("icon-not");

		//	Re-enable normal link behavior.
        link.onclick = null;

		//	Replace normal tooltip.
        if (link.savedTitle != null) {
            link.title = link.savedTitle;
            link.savedTitle = null;
        }
	},

	//	Called by: Transclude.sliceContentFromDocument
	stripIncludeClassesFromLink: (link) => {
		link.classList.remove(...Transclude.permittedClassNames, "include-spinner", "include-spinner-not");
	}
};

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

/******************************************/
/*	Add various include-link alias classes.
 */

/*=============================================*/
/*	.include-block-context-expanded
		`class="include-block-context"`
		`data-block-context-options="expanded"`
 */
Transclude.addIncludeLinkAliasClass("include-block-context-expanded", (includeLink) => {
	includeLink.classList.add("include-block-context");
	includeLink.dataset.blockContextOptions = "expanded";
});

/*========================================================*/
/*	.include-annotation-partial
		`class="include-annotation"`
		`data-include-selector-not=".annotation-abstract, .file-includes, figure, .data-field-separator"`
		`data-template-fields="annotationClassSuffix:$"`
		`data-annotation-class-suffix="-partial"`
 */
Transclude.addIncludeLinkAliasClass("include-annotation-partial", (includeLink) => {
	includeLink.classList.add("include-annotation");
	includeLink.dataset.includeSelectorNot = ".annotation-abstract, .file-includes, figure, .data-field-separator";
	includeLink.dataset.templateFields = [
		...((includeLink.dataset.templateFields ?? "").split(",").filter(x => x)),
		"annotationClassSuffix:$"
	].join(",");
	includeLink.dataset.annotationClassSuffix = "-partial";
});

/*====================================================================*/
/*	.include-annotation-core
		`class="include-annotation"`
		`data-include-selector=".annotation-abstract, .file-includes"`
 */
Transclude.addIncludeLinkAliasClass("include-annotation-core", (includeLink) => {
	includeLink.classList.add("include-annotation");
	includeLink.dataset.includeSelector = ".annotation-abstract, .file-includes";
});

/*==========================================================*/
/*	.include-content-core
		`class="include-content"
		`data-include-selector-not="#footnotes, #backlinks-section, 
			#similars-section, #link-bibliography-section, 
			#page-metadata .link-tags, #page-metadata .page-metadata-fields"`
 */
Transclude.addIncludeLinkAliasClass("include-content-core", (includeLink) => {
	includeLink.classList.add("include-content");
	includeLink.dataset.includeSelectorNot = [
		"#footnotes", 
		"#backlinks-section", 
		"#similars-section",
		"#link-bibliography-section", 
		"#page-metadata .link-tags", 
		"#page-metadata .page-metadata-fields"
	].join(", ");
});

/*==========================================================*/
/*	.include-content-no-header
		`class="include-unwrap"`
		`data-include-selector-not="h1, h2, h3, h4, h5, h6"`
		`data-include-selector-not-options="first"`
 */
Transclude.addIncludeLinkAliasClass("include-content-no-header", (includeLink) => {
	includeLink.classList.add("include-unwrap");
	includeLink.dataset.includeSelectorNot = "h1, h2, h3, h4, h5, h6";
	includeLink.dataset.includeSelectorNotOptions = "first";
});

/*==========================================================*/
/*	.include-caption-not
		`data-include-selector-not=".caption-wrapper"`
 */
Transclude.addIncludeLinkAliasClass("include-caption-not", (includeLink) => {
	includeLink.dataset.includeSelectorNot = ".caption-wrapper";
});
