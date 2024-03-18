/* Miscellaneous utility functions. */
/* author: Said Achmiz */
/* license: MIT */

/****************************************************************/
/*	Generates integer from a uniform distribution over [1, size].
 */
function rollDie(size) {
	return Math.floor(Math.random() * size + 1);
}

/**************************************************************************/
/*	Returns array, of given size, of consecutive integers, with given start 
	value.
 */
function range(start, size) {
	return [...Array(size).keys()].map(i => i + start);
}

/*********************************************************/
/*  Returns val, or min if val < min, or max if val > max.
    (In other words, clamps val to [min,max].)
 */
function valMinMax(val, min, max) {
    return Math.max(Math.min(val, max), min);
}

/***********************************************************/
/*  The first item of the array (or null if array is empty).
 */
Object.defineProperty(Array.prototype, "first", {
    get() {
        if (this.length == 0)
            return null;

        return this[0];
    }
});

/**********************************************************/
/*  The last item of the array (or null if array is empty).
 */
Object.defineProperty(Array.prototype, "last", {
    get() {
        if (this.length == 0)
            return null;

        return this[(this.length - 1)];
    }
});

/********************************/
/*  Remove given item from array.
 */
Array.prototype.remove = function (item) {
    let index = this.indexOf(item);
    if (index !== -1)
        this.splice(index, 1);
};

/***************************************************************************/
/*  Remove from array the first item that passes the provided test function.
    The test function should take an array item and return true/false.
    */
Array.prototype.removeIf = function (test) {
    let index = this.findIndex(test);
    if (index !== -1)
        this.splice(index, 1);
};

/******************************************************************************/
/*  Insert the given item into the array just before the first item that passes
    the provided test function. If no item passes the test function, append the
    item to the end of the array.
 */
Array.prototype.insertBefore = function (item, test) {
    let index = this.findIndex(test);
    if (index === -1) {
        this.push(item);
    } else {
        this.splice(index, 0, item);
    }
};

/*********************************************************/
/*	Polyfill for findLastIndex, for older browser versions 
	(Firefox 103 and lower, Chrome 96 and lower).
	https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findLastIndex
	NOTE: Does not support the `thisArg` parameter.
 */
if (Array.prototype.findLastIndex === undefined) {
	Array.prototype.findLastIndex = function (test) {
		for (let i = this.length - 1; i >= 0; i--) {
			if (test(this[i], i, this))
				return i;
		}
		return -1;
	}
}

/************************************************************/
/*	Returns copy of the array, with duplicate values removed.
 */
Array.prototype.unique = function () {
	return this.filter((value, index, array) => array.indexOf(value) == index);
}

/*********************************************/
/*	Returns the string with words capitalized.
 */
String.prototype.capitalizeWords = function () {
	return this.replace(/\b\w/g, l => l.toUpperCase());
};

/********************************************************/
/*  Returns the string trimmed of opening/closing quotes.
 */
String.prototype.trimQuotes = function () {
    return this.replace(/^["'“‘]?(.+?)["'”’]?$/, '$1');
};

/********************************************************************/
/*  Returns true if the string begins with any of the given prefixes.
 */
String.prototype.startsWithAnyOf = function (prefixes) {
    for (prefix of prefixes)
        if (this.startsWith(prefix))
            return true;
    return false;
}

/******************************************************************/
/*  Returns true if the string ends with any of the given suffixes.
 */
String.prototype.endsWithAnyOf = function (suffixes) {
    for (suffix of suffixes)
        if (this.endsWith(suffix))
            return true;
    return false;
}

/*******************************************************************/
/*  Returns true if the string includes any of the given substrings.
 */
String.prototype.includesAnyOf = function (substrings) {
    for (substring of substrings)
        if (this.includes(substring))
            return true
    return false;
}

/***************************************************************************/
/*	Returns the value of the search param with the given key for a the given
	URL object.
 */
URL.prototype.getQueryVariable = function (key) {
	return this.searchParams.get(key);
}

/**************************************************************************/
/*	Set a URL search parameter with the given key to the given value on the
	given URL object.
 */
URL.prototype.setQueryVariable = function (key, value) {
	let query = new URLSearchParams(this.search);
	query.set(key, value);
	this.search = query.toString();
}

/******************************************************************************/
/*	Delete a URL search parameter with the given key from the given URL object.
 */
URL.prototype.deleteQueryVariable = function (key) {
	let query = new URLSearchParams(this.search);
	query.delete(key);
	this.search = query.toString();
}

/*****************************************************************************/
/*	Returns a URL constructed from either a fully qualified URL string,
	or an absolute local URL (pathname starting at root), or a relative URL
	(pathname component replacing part of current URL after last slash), or
	a hash (URL fragment) only.

	(The existing URL() constructor only handles fully qualified URL strings.)

	The optional baseURL argument allows for qualifying non-fully-qualified
	URL strings relative to a base URL other than the current page location.
 */
function URLFromString(urlString, baseURL = location) {
	if (   urlString.startsWith("http://")
		|| urlString.startsWith("https://"))
		return new URL(urlString);

	if (urlString.startsWith("#"))
		return new URL(baseURL.origin + baseURL.pathname + urlString);

	return (urlString.startsWith("/")
			? new URL(baseURL.origin + urlString)
			: new URL(baseURL.href.replace(/[^\/]*$/, urlString)));
}

/****************************************************************************/
/*	Returns a modified URL constructed from the given URL or URL string, with
	the specified modifications in key-value form.
 */
function modifiedURL(url, mods) {
	let modURL = typeof url == "string" 
				 ? URLFromString(url) 
				 : URLFromString(url.href);
	for (let [ key, value ] of Object.entries(mods))
		modURL[key] = value;
	return modURL;
}

/***************************************************************************/
/*	Returns the value of the search param with the given key for a the given
	HTMLAnchorElement object.
 */
HTMLAnchorElement.prototype.getQueryVariable = function (key) {
	let url = URLFromString(this.href);
	return url.searchParams.get(key);
}

/**************************************************************************/
/*	Set a URL search parameter with the given key to the given value on the
	given HTMLAnchorElement.
 */
HTMLAnchorElement.prototype.setQueryVariable = function (key, value) {
	let url = URLFromString(this.href);
	url.setQueryVariable(key, value);
	this.search = url.search;
}

/******************************************************************/
/*	Delete a URL search parameter with the given key from the given 
	HTMLAnchorElement.
 */
HTMLAnchorElement.prototype.deleteQueryVariable = function (key) {
	let url = URLFromString(this.href);
	url.deleteQueryVariable(key);
	this.search = url.search;
}

/****************************************************************************/
/*  Add an event listener to a button (or other clickable element), attaching
    it to both ‘click’ and ‘keyup’ events (for use with keyboard navigation).

	Available option fields:

	includeMouseDown (boolean)
		Also attach the listener to the ‘mousedown’ event, making the element 
		activate on mouse down (rather than only mouse up, as normal).
 */
Element.prototype.addActivateEvent = function(fn, options) {
	options = Object.assign({
		includeMouseDown: false
	}, options);

    let ael = this.activateEventListener = (event) => {
        if (   event.button === 0
            || event.key    === ' ')
            fn(event);
    };
    this.addEventListener("click", ael);
    this.addEventListener("keyup", ael);
    if (options.includeMouseDown)
        this.addEventListener("mousedown", ael);
}

/******************************************************************************/
/*	Removes event listener from a clickable element, automatically detaching it
	from all relevant event types.
 */
Element.prototype.removeActivateEvent = function() {
	let ael = this.activateEventListener;
	this.removeEventListener("mousedown", ael);
	this.removeEventListener("click", ael);
	this.removeEventListener("keyup", ael);
}

/***************************************************************************/
/*  Swap classes on the given element.
    First argument is an array with two string elements (the classes).
    Second argument is 0 or 1 (index of class to add; the other is removed).
 */
Element.prototype.swapClasses = function (classes, whichToAdd) {
    this.classList.remove(classes[1 - whichToAdd]);
    this.classList.add(classes[whichToAdd]);
};

/******************************************************************************/
/*  The first text node of a node or element (or null if an element contains no
    text nodes).
 */
Object.defineProperty(Node.prototype, "firstTextNode", {
    get() {
        if (this.nodeType == Node.TEXT_NODE)
            return this;

        if (this.childNodes.length == 0)
            return null;

        for (let i = 0; i < this.childNodes.length; i++) {
            let firstTextNodeWithinChildNode = this.childNodes[i].firstTextNode;
            if (firstTextNodeWithinChildNode)
                return firstTextNodeWithinChildNode;
        }

        return null;
    }
});

/******************************************************************************/
/*  The last text node of a node or element (or null if an element contains no
    text nodes).
 */
Object.defineProperty(Node.prototype, "lastTextNode", {
    get() {
        if (this.nodeType == Node.TEXT_NODE)
            return this;

        if (this.childNodes.length == 0)
            return null;

        for (let i = this.childNodes.length - 1; i >= 0; i--) {
            let lastTextNodeWithinChildNode = this.childNodes[i].lastTextNode;
            if (lastTextNodeWithinChildNode)
                return lastTextNodeWithinChildNode;
        }

        return null;
    }
});

/**************************************************************************/
/*  Returns true if the list contains any of the tokens in the given array.
 */
DOMTokenList.prototype.containsAnyOf = function (tokens) {
    for (token of tokens)
        if (this.contains(token) == true)
            return true;
    return false;
}

/**************************************************************************/
/*  Returns true if the list contains all of the tokens in the given array.
 */
DOMTokenList.prototype.containsAllOf = function (tokens) {
    for (token of tokens)
        if (this.contains(token) == false)
            return false;
    return true;
}

/**************************************************/
/*	The obvious equivalent of Element’s .innerHTML.
 */
Object.defineProperty(Document.prototype, "innerHTML", {
    get() {
        return Array.from(this.childNodes).map(node => {
        	switch (node.nodeType) {
			case Node.ELEMENT_NODE:
				return node.outerHTML;
			case Node.COMMENT_NODE:
				return `<!--${node.nodeValue}-->`;
			default:
				return node.nodeValue;
        	}
        }).join("");
    }
});

/**************************************************/
/*	The obvious equivalent of Element’s .innerHTML.
 */
Object.defineProperty(DocumentFragment.prototype, "innerHTML", {
    get() {
        return Array.from(this.childNodes).map(node => {
        	switch (node.nodeType) {
			case Node.ELEMENT_NODE:
				return node.outerHTML;
			case Node.COMMENT_NODE:
				return `<!--${node.nodeValue}-->`;
			default:
				return node.nodeValue;
        	}
        }).join("");
    }
});

/**************************/
/*  Selects the given node.
 */
Selection.prototype.selectNode = function (node) {
	let range = new Range();
	range.selectNode(node);
	this.removeAllRanges();
	this.addRange(range);
}

/*************************************************************/
/*	Polyfill for crypto.randomUUID, for older browser versions
	(Mainly Safari < 15.4)
	https://developer.mozilla.org/en-US/docs/Web/API/Crypto/randomUUID
 */
if (crypto.randomUUID === undefined) {
	crypto.randomUUID = function () {
		return "10000000-1000-4000-8000-100000000000".replace(/[018]/g, c =>
			(c ^ crypto.getRandomValues(new Uint8Array(1))[0] & 15 >> c / 4).toString(16)
		);
	}
}

/*******************************************************************************/
/*  Create and return a new element with the specified tag name, attributes, and
    object properties.
 */
function newElement(tagName, attributes, properties) {
	attributes = Object.assign({ }, attributes);
	properties = Object.assign({ }, properties);

    let element = document.createElement(tagName);
    for (const attrName in attributes)
        if (attributes.hasOwnProperty(attrName))
            element.setAttribute(attrName, attributes[attrName]);
    for (const propName in properties)
        if (properties.hasOwnProperty(propName))
            element[propName] = properties[propName];
    return element;
}

/*******************************************************************************/
/*  Create and return a DocumentFragment containing the given content.

    The content can be any of the following (yielding the listed return value):

    null
        an empty DocumentFragment

    a DocumentFragment
        a DocumentFragment containing the given DocumentFragment’s child nodes

    a string
        a DocumentFragment containing the HTML content that results from parsing
        the string

    a Node
        a DocumentFragment containing the node

    a NodeList
        a DocumentFragment containing the nodes
 */
function newDocument(content) {
    let docFrag = new DocumentFragment();

    if (content == null)
        return docFrag;

    if (content instanceof DocumentFragment) {
        content = content.childNodes;
    } else if (typeof content == "string") {
        let wrapper = newElement("DIV");
        wrapper.innerHTML = content;
        content = wrapper.childNodes;
    }

    if (content instanceof Node) {
        docFrag.append(document.importNode(content, true));
    } else if (   content instanceof NodeList
    		   || content instanceof Array) {
        docFrag.append(...(Array.from(content).map(node => document.importNode(node, true))));
    }

    return docFrag;
}

/************************************************************************/
/*	Creates element from HTML string. Returns null if given HTML does not
	define one, and only one, element.
 */
function elementFromHTML(elementHTML) {
	let doc = newDocument(elementHTML);
	if (doc.children.length != 1)
		return null;
	return doc.firstElementChild;
}

/***************************************************************************/
/*	Transfer any of the given CSS classes that the source has to the target.
	(If no classes are specified, then transfer all classes the source has.)
 */
function transferClasses(source, target, classes) {
	if (classes) {
		classes.forEach(cssClass => {
			if (source.classList.contains(cssClass)) {
				source.classList.remove(cssClass);
				target.classList.add(cssClass);
			}
		});
		if (source.className == "")
			source.removeAttribute("class");
	} else {
        target.classList.add(...(source.classList));
        source.removeAttribute("class");
	}
}

/****************************************/
/*  Wrap an element in a wrapper element.

	The value of the `wrapperSpec` argument should be in the form "tagName" or 
	"tagName.class-name-1.class-name-2" (etc.) or ".class-name-1.class-name-2"
	(in which case the tag name will default to "div").

	(Example: "span.foo-bar.baz-quux", which makes the wrapper
	 `<span class="foo-bar baz-quux"></span>`.)

	Available option fields:

	useExistingWrapper (boolean)
		If the value of `useExistingWrapper` is `true`, then, if the given 
		element is already the only element child of an element with the same 
		tag name as the specified wrapper, then do not inject any additional 
		wrapper. If wrapper classes are specified, apply them to this existing 
		wrapper.

	moveClasses (boolean|Array)
		If the value of `moveClasses` is `true`, then all classes are moved
		(not copied!) from the given element to the wrapper. If, instead, the 
		value of `moveClasses` is an array, then all classes which are in the 
		array are moved (not copied!) from the given element to the wrapper.
 */
function wrapElement(element, wrapperSpec = "", options) {
	options = Object.assign({
		useExistingWrapper: false,
		moveClasses: false
	}, options);

	let [ wrapperTagName, ...wrapperClasses ] = wrapperSpec.split(".");

	//	Default wrapper tag to <div>; capitalize tag name.
	wrapperTagName = (wrapperTagName == "" ? "div" : wrapperTagName).toUpperCase();

    if (   options.useExistingWrapper
        && element.parentElement?.tagName == wrapperTagName
        && isOnlyChild(element)) {
        if (wrapperClasses.length > 0)
            element.parentElement.classList.add(...wrapperClasses);
    } else {
        let wrapper = newElement(wrapperTagName);
        if (wrapperClasses.length > 0)
            wrapper.classList.add(...wrapperClasses);
        element.parentNode.insertBefore(wrapper, element);
        wrapper.appendChild(element);
    }

    if (options.moveClasses === true) {
    	transferClasses(element, element.parentElement);
    } else if (options.moveClasses instanceof Array) {
        transferClasses(element, element.parentElement, options.moveClasses);
    }

	return element.parentElement;
}

/****************************************************************************/
/*  Wrap all elements specified by the given selector.

	See the wrapElement() function for details on the `wrapperSpec` and 
	`options` arguments.

	NOTE: The `wrapperSpec` argument may be a wrap function, in which case all 
	option fields that pertain to the wrapElement() function are ignored (as 
	that function is not called in such a case).

	Available option fields:

	root (Element|Document|DocumentFragment)
		Look for the given selector within the subtree of this node.
 */
function wrapAll(selector, wrapperSpec, options) {
	options = Object.assign({
		root: document
	}, options);

    let wrapperFunction = typeof wrapperSpec == "function"
    					  ? wrapperSpec
    					  : (element) => { wrapElement(element, wrapperSpec, options); };

    options.root.querySelectorAll(selector).forEach(wrapperFunction);
}

/**************************************************************************/
/*  Replace an element with its contents. Returns array of unwrapped nodes.

	Available option fields:

	moveID (boolean)
		If the value of this option field is `true`, and the wrapper has only a
		single child element, then that element is assigned the `id` attribute
		of the wrapper (if any).

	moveClasses (boolean|Array)
		If the value of this option field is `true`, then all classes are moved
		from the wrapper to each unwrapped child element. If, instead, the value
		of this option field is an array, then all classes which are in the 
		array are moved from the wrapper to each element child.

	preserveBlockSpacing (boolean)
		If the value of this option field is `true`, then the value of the 
		`--bsm` CSS property of the wrapper (if any) is assigned to the first
		child element of the wrapper.
 */
function unwrap(wrapper, options) {
	options = Object.assign({
		moveID: false,
		moveClasses: false,
		preserveBlockSpacing: false
	}, options);

	if (wrapper == null)
		return;

    if (wrapper.parentNode == null)
        return;

	let nodes = Array.from(wrapper.childNodes);

	//	Move ID, if specified.
	if (   options.moveID
		&& wrapper.id > ""
		&& wrapper.children.length == 1) {
		wrapper.firstElementChild.id = wrapper.id;
	}

	//	Preserve block spacing, if specified.
	if (   options.preserveBlockSpacing
		&& wrapper.children.length > 0) {
		let bsm = wrapper.style.getPropertyValue("--bsm");
		if (bsm > "")
			wrapper.firstElementChild.setProperty("--bsm", bsm);
	}

    while (wrapper.childNodes.length > 0) {
		let child = wrapper.firstChild;

        wrapper.parentNode.insertBefore(child, wrapper);

		if (!(child instanceof Element))
			continue;

		//	Move classes, if specified.
		if (options.moveClasses === true) {
			transferClasses(wrapper, child);
		} else if (options.moveClasses instanceof Array) {
			transferClasses(wrapper, child, options.moveClasses);
		}
    }

    wrapper.remove();

	return nodes;
}

/******************************************************************************/
/*  Wrap element’s contents, then unwrap the element itself and return wrapper.
 */
function rewrapContents(...args) {
	let wrapper = wrapElement(...args);
	unwrap(args[0]);
	return wrapper;
}

/*************************************************************************/
/*  Unwrap all elements specified by the given selector.

	Available option fields (default value):

	root (Element|Document|DocumentFragment)
		Look for the given selector within the subtree of this node.

	(NOTE: All options used by unwrap() are also supported.)
 */
function unwrapAll(selector, options) {
	options = Object.assign({
		root: document
	}, options);

    options.root.querySelectorAll(selector).forEach(element => {
        unwrap(element, options);
    });
}

/*************************************************************************/
/*	Save an element’s inline styles in a .savedStyles DOM object property.

	Available option fields:

	saveProperties (Array)
		Array of property names which should be saved. If this is null, then
		all properties are saves.
 */
function saveStyles(element, options) {
	options = Object.assign({
		saveProperties: null
	}, options);

	let stylesToSave = { };

	for (let i = 0; i < element.style.length; i++) {
		let propertyName = element.style.item(i);
		if (   options.saveProperties == null
			|| options.saveProperties.includes(propertyName)) {
			let propertyValue = element.style.getPropertyValue(propertyName);
			stylesToSave[propertyName] = propertyValue;
		}
	}

	if (Object.entries(stylesToSave).length > 0)
		element.savedStyles = stylesToSave;
}

/******************************************************************************/
/*	Restore an element’s inline styles from a .savedStyles DOM object property.
 */
function restoreStyles(element) {
	if (element.savedStyles == null)
		return;

	for (let [ propertyName, propertyValue ] of Object.entries(element.savedStyles))
		element.style.setProperty(propertyName, propertyValue);

	element.savedStyles = null;
}

/*****************************************************************************/
/*	Strip an element’s inline styles, optionally only removing some styles,
	optionally keeping some styles.

	Available option fields:

	removeProperties (Array)
		Remove only properties whose names are in this array. If this is null,
		then all properties are removed.

	saveProperties (Array)
		Save the value of these properties; after removing some or all 
		properties (depending on the value of the `removeProperties` option), 
		restore these properties to their saved values.
 */
function stripStyles(element, options) {
	options = Object.assign({
		removeProperties: null,
		saveProperties: null
	}, options);

	if (options.saveProperties)
		saveStyles(element, { saveProperties: options.saveProperties });

	if (options.removeProperties) {
		for (let i = 0; i < element.style.length; i++) {
			let propertyName = element.style.item(i);
			if (options.removeProperties.includes(propertyName))
				element.style.removeProperty(propertyName);
		}
	} else {
		element.removeAttribute("style");
	}

	if (options.saveProperties)
		restoreStyles(element);

	if (element.style.length == 0)
		element.removeAttribute("style");
}

/**************************************************************************/
/*  Call the given function when the given element intersects the viewport.

    Available option fields:

	root
		See IntersectionObserver documentation.

	threshold
		See IntersectionObserver documentation.

	rootMargin
		See IntersectionObserver documentation.
 */
function lazyLoadObserver(f, target, options) {
	options = Object.assign({ }, options);

    if (target == null)
        return;

	requestAnimationFrame(() => {
		if (   (options.threshold ?? 0) == 0
			&& (options.rootMargin ?? "0px").includes("-") == false
			&& isWithinRectOf(target, options.root)) {
			f();
			return;
		}

		let observer = new IntersectionObserver((entries) => {
			if (entries.first.isIntersecting == false)
				return;

			f();
			observer.disconnect();
		}, options);

		observer.observe(target);
	});
}

/*******************************************************************************/
/*  Returns true if the node’s parent has just one child (i.e., the given node),
    or if all siblings are empty nodes. (Note that the given node *itself* being
    empty does not prevent this function from returning true!)
 */
function isOnlyChild(node) {
	if (node == null)
		return undefined;

    if (node.parentElement == null)
        return undefined;

    if (node.parentElement.childNodes.length == 1)
        return true;

    let nonemptySiblingsExist = false;
    node.parentElement.childNodes.forEach(child => {
        if (   child != node
            && isNodeEmpty(child) == false)
            nonemptySiblingsExist = true;
    });
    return (nonemptySiblingsExist == false);
}

/******************************************************************************/
/*  Returns true if the node contains only whitespace and/or other empty nodes.

	Available option fields:

	excludeSelector (string)
		If the node is an element node, then if *and only if* matches the given
		selector, always consider it to be non-empty (and thus always return 
		false, no matter what the node may contain). (Note the difference 
		between this option and `alsoExcludeSelector`, below.)

	alsoExcludeSelector (string)
		If the node is an element node, then if the node matches the given 
		selector OR is one of several always-considered-nonempty tag types
		(IMG, SVG, VIDEO, AUDIO, IFRAME, OBJECT), always consider the node to be
		non-empty (and thus always return false, no matter what the node may
		contain). (Note the difference between this option and 
		`excludeSelector`, above.)

	excludeIdentifiedElements (boolean)
		If the node is an element node, and has a non-empty value for the `id`
		attribute, then always consider the node to be non-empty (and thus 
		always return false, no matter what the node may contain).
 */
function isNodeEmpty(node, options) {
	options = Object.assign({
		excludeSelector: null,
		alsoExcludeSelector: null,
		excludeIdentifiedElements: false
	}, options);

	if (node == null)
		return undefined;

    if (node.nodeType == Node.TEXT_NODE)
        return (/^[\s\u2060]*$/.test(node.textContent));

	if (node.nodeType == Node.ELEMENT_NODE) {
		if (   options.excludeIdentifiedElements
			&& node.id > "") {
			return false;
		} else if (   options.excludeSelector != null
				   && node.matches(options.excludeSelector)) {
			return false;
		} else if (   [ "IMG", "SVG", "VIDEO", "AUDIO", "IFRAME", "OBJECT" ].includes(node.tagName.toUpperCase())
				   || (   options.alsoExcludeSelector != null
				       && node.matches(options.alsoExcludeSelector))) {
			return false;
		}
	}

    if (node.childNodes.length == 0)
        return true;

    for (childNode of node.childNodes)
        if (isNodeEmpty(childNode, options) == false)
            return false;

    return true;
}

/************************************************************************/
/*	Wrap text nodes and inline elements in the given element in <p> tags.
 */
function paragraphizeTextNodesOfElement(element) {
	let inlineElementSelector = [
		"a",
		"em",
		"strong",
		"i",
		"b",
		"code",
		"sup",
		"sub",
		"span"
	].join(", ");

	let nodes = Array.from(element.childNodes);
	let nodeSequence = [ ];
	let omitNode = (node) => isNodeEmpty(node, {
		alsoExcludeSelector: "a, br", 
		excludeIdentifiedElements: true
	});
	let node;
	do {
		node = nodes.shift();
		if (   (   node?.nodeType == Node.TEXT_NODE
				|| (   node?.nodeType == Node.ELEMENT_NODE
					&& node.matches(inlineElementSelector)))
			&& omitNode(node) == false) {
			nodeSequence.push(node);
		} else if (omitNode(node)) {
			node?.remove();
		} else {
			if (nodeSequence.length > 0) {
				//	Construct paragraph (<p>) to wrap node sequence.
				//	(This removes the nodes from the element.)
				let graf = newElement("P");
				graf.append(...nodeSequence);

				//	Insert paragraph (with the previously removed nodes).
				element.insertBefore(graf, node)
			}

			nodeSequence = [ ];

			//	Remove <br> elements.
			if (   node?.nodeType == Node.ELEMENT_NODE
				&& node.tagName == "BR")
				node.remove();
		}
	} while (node);
}

/***************************************************/
/*  Causes an element’s contents to become selected.
 */
function selectElementContents(element) {
    let range = document.createRange();
    range.selectNodeContents(element);
    let selection = window.getSelection();
    selection.removeAllRanges();
    selection.addRange(range);
}

/***************************************************************/
/*  Returns a DocumentFragment containing the current selection.
 */
function getSelectionAsDocument(doc = document) {
	let selection = doc.getSelection();
	if (selection.rangeCount == 0)
		return newDocument();

    let docFrag = new DocumentFragment();
    docFrag.append(selection.getRangeAt(0).cloneContents());

	//	Strip whitespace (remove top-level empty nodes).
	let nodesToRemove = [ ];
	docFrag.childNodes.forEach(node => {
		if (isNodeEmpty(node))
			nodesToRemove.push(node);
	});
	nodesToRemove.forEach(node => {
		docFrag.removeChild(node);
	});

    return docFrag;
}

/*********************************************************************/
/*	Workaround for Firefox weirdness, based on more Firefox weirdness.
 */
DocumentFragment.prototype.getSelection = function () {
	return document.getSelection();
}

/***********************************************************************/
/*  Returns true if the point is within the given rect, false otherwise.
 */
function pointWithinRect(point, rect) {
    return (   point.x >= rect.left
            && point.x <= rect.right
            && point.y >= rect.top
            && point.y <= rect.bottom);
}

/**************************************************************/
/*  Returns true if the given rects intersect, false otherwise.
	(If `margin` is nonzero, then the two rects are considered to be 
	 intersecting if the distance between them is less than the margin.
	 Must be given in pixel values only, either as number or as string.)
 */
function doRectsIntersect(rectA, rectB, margin = 0) {
	if (typeof margin == "string")
		margin = parseInt(margin);

    return (   rectA.top    - margin < rectB.bottom
            && rectA.bottom + margin > rectB.top
            && rectA.left   - margin < rectB.right
            && rectA.right  + margin > rectB.left);
}

/******************************************************************************/
/*  Returns true if the given element intersects the given rect,
    false otherwise. (See doRectsIntersect() for meaning of `margin` argument.)
 */
function isWithinRect(element, rect, margin) {
    return doRectsIntersect(element.getBoundingClientRect(), rect, margin);
}

/*******************************************************************************/
/*  Returns true if the first element intersects the bounding rect of the second
	element (or the viewport, if second element is null), false otherwise.
	(See doRectsIntersect() for meaning of `margin` argument.)
 */
function isWithinRectOf(firstElement, secondElement, margin) {
	let secondElementRect = secondElement 
							? secondElement.getBoundingClientRect()
							: new DOMRect(0, 0, window.innerWidth, window.innerHeight);
	return isWithinRect(firstElement, secondElementRect, margin);
}

/******************************************************************************/
/*  Returns true if the given element intersects the viewport, false otherwise.
	(See doRectsIntersect() for meaning of `margin` argument.)
 */
function isOnScreen(element, margin) {
    return isWithinRectOf(element, null, margin);
}

/**************************************/
/*	Returns union of two or more rects.
 */
function rectUnion (aRect, ...args) {
	let union = aRect;
	for (let rect of args) {
		union = new DOMRect(
						Math.min(union.x, rect.x),
						Math.min(union.y, rect.y),
						Math.max(union.right, rect.right) - Math.min(union.x, rect.x),
						Math.max(union.bottom, rect.bottom) - Math.min(union.y, rect.y)
				);
	}
	return union;
}

/*******************************************************************************/
/*  Transforming a URL hash into a CSS selector in a form that’s safe to pass to
    querySelector and similar functions. (All characters that need to be escaped
    according to the CSS spec are escaped.)
 */
function selectorFromHash(hash) {
    if (hash.length <= 1)
        return null;

    //  Chrome’s fancy new “scroll to text fragment”.
    if (hash.startsWith("#:~:"))
        return null;

    return "#" + CSS.escape(decodeURIComponent(hash.slice(1)));
}

/*************************************************************************/
/*  Because encodeURIComponent does not conform to RFC 3986; see MDN docs.
 */
function fixedEncodeURIComponent(str) {
    return encodeURIComponent(str).replace(/[!'()*]/g, function(c) {
        return '%' + c.charCodeAt(0).toString(16);
    });
}

/********************************************/
/*	Returns HTML-unescaped version of string.
 */
function unescapeHTML(str) {
	let parser = new DOMParser();

	return parser.parseFromString(str, "text/html").documentElement.textContent;
}

/***************************************************/
/*	Return the value of a GET (i.e., URL) parameter.
	*/
function getQueryVariable(variable) {
	let query = window.location.search.substring(1);
	let vars = query.split("&");
	for (let i = 0; i < vars.length; i++) {
		let pair = vars[i].split("=");
		if (pair[0] == variable)
			return decodeURIComponent(pair[1]);
	}

	return null;
}

/***********************************************************************/
/*  Helper function for AJAX, by kronusaturn
    https://github.com/kronusaturn/lw2-viewer/blob/master/www/script.js
 */
function urlEncodeQuery(params) {
    return (Object.keys(params)).map(x => (`${x}=${ fixedEncodeURIComponent(params[x]) }`)).join("&");
}

/***********************************************************************/
/*  Helper function for AJAX, by kronusaturn
    https://github.com/kronusaturn/lw2-viewer/blob/master/www/script.js
 */
function doAjax(options) {
	options = Object.assign({
		location: document.location,
		method: "GET",
		params: null,
		serialization: "URL",
		responseType: null,
		headers: null,
		onSuccess: null,
		onFailure: null
	}, options);

    let req = new XMLHttpRequest();

    req.addEventListener("load", (event) => {
        if (event.target.status < 400) {
			options.onSuccess?.(event);
        } else {
			options.onFailure?.(event);
        }
    });
    req.addEventListener("error", (event) => {
		options.onFailure?.(event);
    });

    let location = options.location
    			 + ((   options.params != null
    			 	 && options.method == "GET") 
    			 	? "?" + urlEncodeQuery(options.params)
    			 	: "");
    req.open(options.method, location);

    if (options.responseType)
	    req.responseType = options.responseType;

	if (options.headers)
		for (let [ headerName, headerValue ] of Object.entries(options.headers))
			req.setRequestHeader(headerName, headerValue);

    if (options.method == "POST") {
    	let payload;
    	switch (options.serialization) {
    	case "JSON":
    		payload = JSON.stringify(options.params);
			req.setRequestHeader("Content-Type", "application/json");
    		break;
		case "URL":
		default:
			payload = urlEncodeQuery(options.params);
			req.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
			break;
    	}

        req.send(payload);
    } else {
        req.send();
    }
}

/***********************************************************************/
/*  Changes the page location to the given URL string (or path or hash).
 */
function relocate(s) {
    history.replaceState(null, null, s);
}

/********************************************************/
/*  Return the element targeted by the URL hash, or null.
 */
function getHashTargetedElement() {
	return (location.hash.length > 1
		    ? document.querySelector(selectorFromHash(location.hash))
		    : null);
}

/*************************************************************************/
/*  Simple mutex mechanism.

	Available option fields:

	releaseImmediately (boolean)
		If `true`, release the pass immediately after calling the provided
		function; otherwise, release at the next animation frame.
 */
function doIfAllowed(f, passHolder, passName, options) {
	options = Object.assign({
		releaseImmediately: false
	}, options);

    if (passHolder[passName] == false)
        return;

    passHolder[passName] = false;

    f();

    if (options.releaseImmediately) {
        passHolder[passName] = true;
    } else {
        requestAnimationFrame(() => {
            passHolder[passName] = true;
        });
    }
}

/*******************************************************************************/
/*  When the given event is triggered on the given target, start a timer with
	a duration equal to the given delay (in ms); when the timer fires, it calls
	the given handler function.

    Return value of this function is an anonymous function which removes the
    listeners that this function adds.

	Available option fields:

	cancelOnEvents (Array)
		If any of the events whose names are listed in this array occur after
		the triggering event occurs but prior to the delay elapsing, the timer
		will be canceled and the handler function will not be called.

		NOTE: If `delay` is 0 or less, then `cancelOnEvents` is ignored, and 
		`func` is added as an event handler for `triggerEventName` directly.
 */
function onEventAfterDelayDo(target, triggerEventName, delay, func, options) {
	options = Object.assign({
		cancelOnEvents: [ ]
	}, options);

    if (delay <= 0) {
        target.addEventListener(triggerEventName, func);
        return (() => {
            target.removeEventListener(triggerEventName, func);
        });
    } else {
        let timer = null;
        let triggerEvent = (event) => { timer = setTimeout(func, delay, event); };
        let cancelEvent = (event) => { clearTimeout(timer); };
        target.addEventListener(triggerEventName, triggerEvent);
		options.cancelOnEvents.forEach(cancelEventName => {
			target.addEventListener(cancelEventName, cancelEvent);
		});
        return (() => {
            target.removeEventListener(triggerEventName, triggerEvent);
			options.cancelOnEvents.forEach(cancelEventName => {
				target.removeEventListener(cancelEventName, cancelEvent);
			});
        });
    }
}

/************************************************/
/*	Polyfill for requestIdleCallback() in Safari.
 */
if (window.requestIdleCallback == null) {
	window.requestIdleCallback = (fn) => { setTimeout(fn, 0) };
}
