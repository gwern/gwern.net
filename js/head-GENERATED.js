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

/*************************/
/*	Real modulo operation.
	(See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Remainder )
 */
function modulo(n, d) {
	return (((n % d) + d) % d);
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
};

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
};

/******************************************************************/
/*  Returns true if the string ends with any of the given suffixes.
 */
String.prototype.endsWithAnyOf = function (suffixes) {
    for (suffix of suffixes)
        if (this.endsWith(suffix))
            return true;
    return false;
};

/*******************************************************************/
/*  Returns true if the string includes any of the given substrings.
 */
String.prototype.includesAnyOf = function (substrings) {
    for (substring of substrings)
        if (this.includes(substring))
            return true
    return false;
};

/*****************************************/
/*	Returns numeric hash code of a string.

	https://stackoverflow.com/questions/7616461/generate-a-hash-from-string-in-javascript
 */
String.prototype.hashCode = function () {
	return (this.split('').reduce((prevHash, currVal) => {
		return (((prevHash << 5) - prevHash) + currVal.charCodeAt(0))|0;
	}, 0) + 2147483648);
};

/**************************************************/
/*	Returns camelCase version of kebab-case string.
 */
String.prototype.kebabCaseToCamelCase = function () {
	return this.replace(/([a-z])-([a-z])/g, (match, p1, p2) => (p1 + p2.toUpperCase()));
};

/**************************************************/
/*	Returns kebab-case version of camelCase string.
 */
String.prototype.camelCaseToKebabCase = function () {
	return this.replace(/([a-z])([A-Z])/g, "$1-$2").toLowerCase()
};

/***************************************************************************/
/*	Returns the value of the search param with the given key for a the given
	URL object.
 */
URL.prototype.getQueryVariable = function (key) {
	return this.searchParams.get(key);
};

/**************************************************************************/
/*	Set a URL search parameter with the given key to the given value on the
	given URL object.
 */
URL.prototype.setQueryVariable = function (key, value) {
	let query = new URLSearchParams(this.search);
	query.set(key, value);
	this.search = query.toString();
};

/******************************************************************************/
/*	Delete a URL search parameter with the given key from the given URL object.
 */
URL.prototype.deleteQueryVariable = function (key) {
	let query = new URLSearchParams(this.search);
	query.delete(key);
	this.search = query.toString();
};

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
};

/**************************************************************************/
/*	Set a URL search parameter with the given key to the given value on the
	given HTMLAnchorElement.
 */
HTMLAnchorElement.prototype.setQueryVariable = function (key, value) {
	let url = URLFromString(this.href);
	url.setQueryVariable(key, value);
	this.search = url.search;
};

/******************************************************************/
/*	Delete a URL search parameter with the given key from the given 
	HTMLAnchorElement.
 */
HTMLAnchorElement.prototype.deleteQueryVariable = function (key) {
	let url = URLFromString(this.href);
	url.deleteQueryVariable(key);
	this.search = url.search;
};

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
};

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

/***********************************************************************/
/*  Trims opening/closing quotes from the element’s contents (preserving 
	internal HTML structure).

	Returns the now-modified element.
 */
Element.prototype.trimQuotes = function () {
	let [ first, last ] = [ this.firstTextNode, this.lastTextNode ];
	if (   /^['"‘“]/.test(first.textContent) == true
		&& /['"’”]$/.test(last.textContent)  == true) {
		first.textContent = first.textContent.slice(1);
		last.textContent = last.textContent.slice(0, -1);
	}
	return this;
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
};

/**************************************************************************/
/*  Returns true if the list contains all of the tokens in the given array.
 */
DOMTokenList.prototype.containsAllOf = function (tokens) {
    for (token of tokens)
        if (this.contains(token) == false)
            return false;
    return true;
};

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
};

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
    for (let [ attrName, attrValue ] of Object.entries(attributes))
		element.setAttribute(attrName, attrValue);
    for (let [ propName, propValue ] of Object.entries(properties))
		element[propName] = propValue;
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

	Available option fields:

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

	The `entries` parameter of the IntersectionObserver callback is passed
	to the called function (unless called immediately).

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

			f(entries);
			observer.disconnect();
		}, options);

		observer.observe(target);
	});
}

/***********************************************************************/
/*  Call the given function when the given element is resized.

	The `entries` parameter of the ResizeObserver callback is passed
	to the called function.

	If `false` is returned from the function call, disconnects observer.
	Otherwise, continues observation.
 */
function resizeObserver(f, target) {
	if (target == null)
		return;

	requestAnimationFrame(() => {
		let observer = new ResizeObserver((entries) => {
			if (f(entries) == false)
				observer.disconnect();
		});

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

	excludePredicate (Node => boolean)
		If *and only if* the predicate returns true when called with the node
		as argument, always consider it to be non-empty (and thus always 
		return false, no matter what the node may contain). (NOTE: If this 
		option is set, all other options are ignored.)

	alsoExcludePredicate (Node => boolean)
		If the predicate returns true when called with the node as argument, 
		always consider it to be non-empty (and thus always return false, 
		no matter what the node may contain). (NOTE: If this option is set, 
		the options below will still take effect.)

	excludeSelector (string)
		If the node is an element node, then if *and only if* it matches the 
		given selector, always consider it to be non-empty (and thus always 
		return false, no matter what the node may contain). (Note the difference 
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
		excludePredicate: null,
		alsoExcludePredicate: null,
		excludeSelector: null,
		alsoExcludeSelector: null,
		excludeIdentifiedElements: false
	}, options);

	if (node == null)
		return undefined;

    if (node.nodeType == Node.TEXT_NODE)
        return (/^[\s\u2060]*$/.test(node.textContent));

	if (options.excludePredicate != null) {
		if (options.excludePredicate(node))
			return false;
	} else if (options.alsoExcludePredicate?.(node)) {
		return false;
	} else if (node.nodeType == Node.ELEMENT_NODE) {
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

	Available option fields:

	includeSelector (string)
		Selector for elements which should be considered as content to be
		wrapped in <p> tags. (By default, this selects the most common inline
		elements, such as <a>, <span>, etc.)

	excludeSelector (string)
		Selector for elements which should *not* be considered as content to be
		wrapped in <p> tags. Note that this option takes priority over the 
		previous one (`includeSelector`).

	nodeOmissionOptions (object)
		Options to pass to the isNodeEmpty() call that determines whether a 
		node should be dropped when aggregating nodes into paragraphs.

 */
function paragraphizeTextNodesOfElement(element, options) {
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

	options = Object.assign({
		includeSelector: inlineElementSelector,
		excludeSelector: "",
		nodeOmissionOptions: {
			alsoExcludeSelector: "a, br", 
			excludeIdentifiedElements: true
		}
	}, options);

	let nodes = Array.from(element.childNodes);
	let nodeSequence = [ ];
	let shouldOmitNode = (node) => isNodeEmpty(node, options.nodeOmissionOptions);
	let node;
	do {
		node = nodes.shift();
		let omitNode = shouldOmitNode(node);
		if (   (   node?.nodeType == Node.TEXT_NODE
				|| (   node?.nodeType == Node.ELEMENT_NODE
					&& node.matches(options.includeSelector) == true
					&& node.matches(options.excludeSelector) == false))
			&& omitNode == false) {
			nodeSequence.push(node);
		} else if (omitNode) {
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

	//	Trim whitespace (remove top-level empty nodes at start and end).
	let nodesToRemove = [ ];
	for (let i = 0; i < docFrag.childNodes.length; i++) {
		let node = docFrag.childNodes[i];
		if (isNodeEmpty(node)) {
			nodesToRemove.push(node);
		} else {
			break;
		}
	}
	for (let j = 0; j < docFrag.childNodes.length; j++) {
		let node = docFrag.childNodes[docFrag.childNodes.length - (1 + j)];
		if (isNodeEmpty(node)) {
			nodesToRemove.push(node);
		} else {
			break;
		}
	}
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
};

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
/*  Create global ‘GW’ object, if need be.
 */
if (typeof window.GW == "undefined")
    window.GW = { };


/***********/
/* HELPERS */
/***********/

/*******************************************************************************/
/*  Product of two string arrays. (Argument can be a string, which is equivalent
    to passing an array with a single string member.)
    Returns array whose members are all results of concatenating each left hand
    array string with each right hand array string, e.g.:

        [ "a", "b" ].π([ "x", "y" ])

    will return:

        [ "ax", "ay", "bx", "by" ]

    Any non-string argument must be iterable, else null is returned. Any
    members of a passed array (or other iterable object), whatever their types,
    are stringified and interpolated into the resulting product strings.
 */
Array.prototype.π = function (strings) {
    if (typeof strings == "string")
        strings = [ strings ];

    if (!!strings[Symbol.iterator] == "false")
        return null;

    let product = [ ];
    for (let lhs of this) {
        for (let rhs of strings) {
            product.push(`${lhs}${rhs}`);
        }
    }
    return product;
};

/*****************************************************************************/
/*  As Array.π, but applies sequentially to each argument. (First argument may
    be a string, which is impossible with the Array member version.)
 */
function _π(...args) {
    if (args.length == 0)
        return [ ];

    let product = [ "" ];
    for (let arg of args)
        product = product.π(arg);

    return product;
}


/********************/
/* DEBUGGING OUTPUT */
/********************/

GW.dateTimeFormat = new Intl.DateTimeFormat([], { hour12: false, hour: "numeric", minute: "numeric", second: "numeric" });

function GWTimestamp() {
    let time = Date.now();
    let ms = `${(time % 1000)}`.padStart(3,'0');
    let timestamp = `${GW.dateTimeFormat.format(time)}.${ms}`;

    return timestamp;
}

GW.logLevel = localStorage.getItem("gw-log-level") || 0;
GW.logSourcePadLength = 28;

function GWLog (string, source = "", level = 1) {
    if (GW.logLevel < level)
        return;

    let sourcestamp = (source > "" ? `[${source}]` : `[ ]`).padEnd(GW.logSourcePadLength, ' ');

	let outputString = (`[${GWTimestamp()}]  ` + sourcestamp + string);

	console.log(outputString);
}
GW.setLogLevel = (level, permanently = false) => {
    if (permanently)
        localStorage.setItem("gw-log-level", level);

    GW.logLevel = level;
};

function GWStopWatch(f, ...args) {
    let fname = (f.name || f.toString().slice(0, f.toString().indexOf('{')));
    console.log(`[${GWTimestamp()}]  ${fname} [BEGIN]`);
    let rval = f(...args);
    console.log(`[${GWTimestamp()}]  ${fname} [END]`);
    return rval;
}


/*******************/
/* ERROR REPORTING */
/*******************/

/*  Reports an error by sending an XMLHTTPRequest to the 404 page, suffixed
    with some error string (which gets automatically URL-encoded).

	(Requires utility.js.)
 */
function GWServerLogError(errorString, errorType) {
    doAjax({ location: `${location.origin}/404-error-` + fixedEncodeURIComponent(errorString) });
    GWLog(`Reporting ${(errorType || "error")}:  ${errorString}`, "error reporting", 1);
}


/*****************/
/* MEDIA QUERIES */
/*****************/

GW.mediaQueries = {
    mobileWidth:           matchMedia("(max-width: 649px)"),
    systemDarkModeActive:  matchMedia("(prefers-color-scheme: dark)"),
    hoverAvailable:        matchMedia("only screen and (hover: hover) and (pointer: fine)"),
    portraitOrientation:   matchMedia("(orientation: portrait)"),
    printView:             matchMedia("print")
};

GW.isMobile = () => {
    /*  We consider a client to be mobile if one of two conditions obtain:
        1. JavaScript detects touch capability, AND viewport is narrow; or,
        2. CSS does NOT detect hover capability.
     */
    return (   (   ("ontouchstart" in document.documentElement)
                && GW.mediaQueries.mobileWidth.matches)
            || !GW.mediaQueries.hoverAvailable.matches);
};

GW.isFirefox = () => {
    return (navigator.userAgent.indexOf("Firefox") > 0);
};

GW.isTorBrowser = () => {
	return (("serviceWorker" in navigator) == false);
};

GW.isX11 = () => {
    return (navigator.userAgent.indexOf("X11") > 0);
};


/************************/
/* ACTIVE MEDIA QUERIES */
/************************/

/*  This function provides two slightly different versions of its functionality,
    depending on how many arguments it gets.

    If one function is given (in addition to the media query and its name), it
    is called whenever the media query changes (in either direction).

    If two functions are given (in addition to the media query and its name),
    then the first function is called whenever the media query starts matching,
    and the second function is called whenever the media query stops matching.

    If you want to call a function for a change in one direction only, pass an
    empty closure (NOT null!) as one of the function arguments.

    There is also an optional fifth argument. This should be a function to be
    called when the active media query is canceled.
 */
function doWhenMatchMedia(mediaQuery, name, ifMatchesOrAlwaysDo, otherwiseDo = null, whenCanceledDo = null) {
    if (typeof GW.mediaQueryResponders == "undefined")
        GW.mediaQueryResponders = { };

    let mediaQueryResponder = (event, canceling = false) => {
        if (canceling) {
            GWLog(`Canceling media query “${name}”`, "media queries", 1);

            if (whenCanceledDo != null)
                whenCanceledDo(mediaQuery);
        } else {
            let matches = (typeof event == "undefined") ? mediaQuery.matches : event.matches;

            GWLog(`Media query “${name}” triggered (matches: ${matches ? "YES" : "NO"})`, "media queries", 1);

            if ((otherwiseDo == null) || matches)
                ifMatchesOrAlwaysDo(mediaQuery);
            else
                otherwiseDo(mediaQuery);
        }
    };
    mediaQueryResponder();
    mediaQuery.addListener(mediaQueryResponder);

    GW.mediaQueryResponders[name] = mediaQueryResponder;
}

/*  Deactivates and discards an active media query, after calling the function
    that was passed as the whenCanceledDo parameter when the media query was
    added.
 */
function cancelDoWhenMatchMedia(name) {
	if (GW.mediaQueryResponders[name] == null)
		return;

    GW.mediaQueryResponders[name](null, true);

    for (let [ key, mediaQuery ] of Object.entries(GW.mediaQueries))
        mediaQuery.removeListener(GW.mediaQueryResponders[name]);

    GW.mediaQueryResponders[name] = null;
}


/*****************/
/* NOTIFICATIONS */
/*****************/
/*  The GW.notificationCenter object allows us to register handler functions for
    named events. Any number of handlers may be registered for any given named
    event, and when that event is fired, all of its registered handlers will be
    called. Because event handlers are registered for events by event name
    (which may be any string we like), a handler may be registered for an event
    at any time and at any location in the code. (In other words, an event does
    not need to first be “defined”, nor needs to “exist” in any way, in order
    for a handler to be registered for it.)

    We can also make the calling of any given event handler conditional (with a
    user-defined, handler-specific condition function [closure] that dynamically
    determines whether its associated handler should be called or not, when the
    event the handler was registered for is fired), specify that an event
    handler should be called only once or many times, and group handlers for a
    particular event into named “phases” (to ensure that certain handlers for an
    event are always called before/after others).

    Events themselves are also user-defined. Causing an event to fire is as
    simple as calling GW.notificationCenter.fireEvent() and providing an event
    name (which may be any string), plus an event info dictionary (which may
    contain any keys and values we deem necessary, and which will be passed to
    the handler functions); this will trigger the calling of all the handlers
    that have been registered for that event name.

    See the comments on specific elements of GW.notificationCenter, below, for
    more information.
 */
GW.notificationCenter = {
    /*  Dictionary of registered event handlers for named events.

        KEYS are event names (e.g. ‘GW.contentDidLoad’).

        VALUES are arrays of handler definitions for each event. Each handler
        definition is a dictionary with the following keys/values:

        - ‘f’ (key)
            Handler function to call when the named event fires (passing the
            event info dictionary of the fired event). (See comment on the
            ‘addHandlerForEvent’ function, below, for details.)

        - ‘options’ (key) [optional]
            Event options dictionary, with the following keys/values:

            - ‘condition’ (key) [optional]
                Test function, to which the event info dictionary of the fired
                event is passed; the handler function is called if (and only if)
                the condition returns true.

            - ‘once’ (key) [optional]
                Boolean value; if true, the handler will be removed after the
                handler function is called once (note that if there is a
                condition function provided [see the ‘condition’ key], the
                handler function will not be called - and therefore will not be
                removed - if the named event is fired but the condition
                evaluates to false).

                If not set, defaults to false (ie. by default a handler is
                not removed after an event is fired once, but will continue to
                be invoked each time the named event fires and the condition,
                if any, evaluates as true).

            - ‘phase’ (key) [optional]
                String which specifies when the given handler function should be
                called, relative to other handlers registered for the named
                event.

                The format for this string is as follows:

				- If the entire string is equal to “<”, then the given handler
				  function will be called prior to any handlers that are
				  assigned to any other phase (or to no specific phase). (Within
				  this “before all others” ‘pseudo-phase’, handlers are called
				  in the order in which they were added.)

				- If the entire string is equal to “>”, then the given handler
				  function will be called after any handlers that are assigned
				  to any other phase (or to no specific phase). (Within this
				  “after all others” ‘pseudo-phase’, handlers are called in the
				  order in which they were added.)

				- If the string is empty, then the given handler function will
				  be called after all other handlers, but before any handlers
				  that were assigned to phase “>”. (Within this “no particular
				  phase” ‘pseudo-phase’, handlers are called in the order in
				  which they were added.)

                - If the first character is anything other than ‘<’ or ‘>’, the
                  entire string is treated as the name of a handler phase. The
                  given handler function will be called in the same handler
                  phase as all other handlers assigned to that phase. (Within a
                  phase, handlers are called in the order in which they were
                  added.)

                - If the first character is ‘<’, then the rest of the string
                  is treated as the name of a handler phase. The given handler
                  function will be called prior to any handlers assigned to the
                  specified phase, but after any handlers assigned to an earlier
                  named phase (if any). (Within such a “before phase X”
                  ‘pseudo-phase’, handlers are called in the order in which they
                  were added.)

                - If the first character is ‘>’, then the rest of the string
                  is treated as the name of a handler phase. The given handler
                  function will be called after any handlers assigned to the
                  specified phase, but before any handlers assigned to a later
                  named phase (if any). (Within such an “after phase X”
                  ‘pseudo-phase’, handlers are called in the order in which they
                  were added.)

        When an event is fired, any handlers registered for that event (ie.
        members of the array which is the value for that event’s name in the
        eventHandlers dictionary) are called in array order. (If a condition is
        specified for any given handler, the handler function is only called if
        the condition function - called with the event info dictionary as its
        argument - evaluates true.)

        The order of an event handlers array for a given event is, by default,
        determined by the order in which handlers are registered for that event.
        The value of the ‘phase’ key of an event’s options dictionary can
        override and modify this default order. (See definition of the ‘phase’
        key of an event handler options dictionary, above.)
     */
    eventHandlers: { },

    /*  Defined event handler phases for certain named events.
        (See definition of the ‘phase’ key of an event handler options
         dictionary, above, for more info.)

        Phases are defined in execution order. For example, consider a
        hypothetical GW.exampleDidHappen event, whose handler phases are defined
        as follows: `[ "foo", "bar" ]`. When the GW.exampleDidHappen event
        fires, event handlers are called in the following order:

		1. Handlers assigned to be called before all other phases (ie. those
		   with ‘<’ as the value of their ‘phase’ key in their event handler
		   options dictionary)
        2. Handlers assigned to be called before the ‘foo’ phase (ie.
           those with ‘<foo’ as the value of their ‘phase’ key in their
           event handler options dictionary)
        3. Handlers assigned to be called during the ‘foo’ phase (ie.
           those with ‘foo’ as the value of their ‘phase’ key in their
           event handler options dictionary)
        4. Handlers assigned to be called after the ‘foo’ phase (ie.
           those with ‘>foo’ as the value of their ‘phase’ key in their
           event handler options dictionary)
        5. Handlers assigned to be called before the ‘bar’ phase
           (ie. those with ‘<bar’ as the value of their ‘phase’ key
           in their event handler options dictionary)
        6. Handlers assigned to be called during the ‘bar’ phase
           (ie. those with ‘bar’ as the value of their ‘phase’ key
           in their event handler options dictionary)
        7. Handlers assigned to be called after the ‘bar’ phase
           (ie. those with ‘>bar’ as the value of their ‘phase’ key
           in their event handler options dictionary)
        8. Handlers assigned to be called after all other phases (ie. those
		   with ‘>’ as the value of their ‘phase’ key in their event handler
		   options dictionary)

        (Handlers with no specified phase might be called at any point after
         step 1 and before step 8 in this sequence, depending on when they were
         registered.)
     */
    handlerPhaseOrders: { },

    /*  Register a new event handler for the named event. Arguments are:

        - ‘eventName’
            The name of an event (e.g. ‘GW.contentDidLoad’).

        - ‘f’
            Event handler function. When the event fires, this function will be
            called. Note that if a condition is specified in the event handler
            options (i.e. if a condition function is provided as the value of
            the ‘condition’ key in the event handler options dictionary), then
            the handler function will be called only if the condition function
            evaluates true).

            The event handler function should take one argument: an event info
            dictionary. The keys and values of this dictionary are mostly
            event-specific (but see the ‘fireEvent’ function, below, for more
            info).

        - ‘options’ [optional]
            Event handler options dictionary. See comment on the ‘eventHandlers’
            property (above) for info on possible keys/values.

        Note that if there already exists a registered event handler for the
        given event with the same event handler function as the new handler that
        you are trying to register, then the new handler will not be registered
        (even if it has different handler options than the existing handler).
     */
    addHandlerForEvent: (eventName, f, options) => {
		options = Object.assign({
			condition: null,
			once: false,
			phase: ""
		}, options);

        /*  If this event is currently firing, do not add the handler yet.
            Instead, add it to the waiting list. It will be added once the event
            has finished firing.
         */
        if (GW.notificationCenter.currentEvents.includes(eventName)) {
            if (GW.notificationCenter.waitingHandlers[eventName] == null)
                GW.notificationCenter.waitingHandlers[eventName] = [ ];

            GW.notificationCenter.waitingHandlers[eventName].push({ f: f, options: options });

            return;
        }

        /*  If there’s not already a handlers array for the given event (which
            may be, e.g. because no event handlers have yet been registered
            for this event), create the array.
         */
        if (GW.notificationCenter.eventHandlers[eventName] == null)
            GW.notificationCenter.eventHandlers[eventName] = [ ];

        /*  Array of registered handlers for the named event. Might be empty
            (if no handlers have been registered for this event yet).
         */
        let handlers = GW.notificationCenter.eventHandlers[eventName];

        /*  If there is already a registered handler with the same handler
            function as the one we’re trying to register, do not register this
            new one (even if it has different handler options).
         */
        if (handlers.findIndex(handler => handler.f == f) !== -1)
            return;

        /*  Get the handler phase order for the named event, if any. (Add to it
        	the built-in phases “<” and “>”.)
         */
        let phaseOrder = [ "<", ...(GW.notificationCenter.handlerPhaseOrders[eventName] ?? [ ]), ">" ];

		/*  Get the target phase name, which may be the full value of the
			‘phase’ key of the options dictionary, OR it may be that value
			minus the first character (if the value of the ‘phase’ key
			begins with a ‘<’ or a ‘>’ character).
			*/
		let targetPhase = options.phase.match(/^([<>]?)(.+)?/)[2];

		/*  Get the index of the target phase in the defined handler phase
			order for the named event.
		 */
		let targetPhaseIndex = phaseOrder.indexOf(targetPhase);

		/*  Takes an index into the given event’s handler array. Returns a
			dictionary with these keys/values:

			- ‘phase’ [key]
				The name of the phase to which the handler at the given
				index is assigned (could be an empty string).

			- ‘before’ [key]
				Boolean value; true if the handler at the given index is
				assigned to run before the specified phase, false otherwise
				(ie. if it’s instead assigned to run either during or
				after the specified phase).

			- ‘after’ [key]
				Boolean value; true if the handler at the given index is
				assigned to run after the specified phase, false otherwise
				(ie. if it’s instead assigned to run either before or
				during the specified phase).

			(Note that for an event handler which has not been assigned to
			 any specific phase, ‘phase’ will be the empty string, and both
			 ‘before’ and ‘after’ will be false.)

			Returns null if the given index is out of bounds of the event’s
			handler definitions array.
		 */
		let phaseAt = (index) => {
			if (index >= handlers.length)
				return null;
			let parts = handlers[index].options.phase.match(/^([<>]?)(.*)$/);
			return (parts[2] > ""
					? { phase: parts[2],
						before: (parts[1] == "<"),
						after: (parts[1] == ">") }
					: { phase: parts[1] });
		};

		//	Where in the handlers array to insert the new handler?
        let insertAt;
        if (options.phase == "<") {
			/*	If the handler we’re registering is assigned to phase “<” (i.e.,
				is specified to run before all others), it’s inserted
				immediately after all other handlers already likewise specified.
			 */
        	for (var i = 0; i < handlers.length; i++) {
        		if (phaseAt(i).phase != "<")
        			break;
        	}

			insertAt = i;
        } else if (options.phase == ">") {
			/*	If the handler we’re registering is assigned to phase “>” (i.e.,
				is specified to run after all others), it’s inserted immediately
				after all other handlers already so specified (i.e., at the very
				end of the handlers array).
			 */
        	insertAt = handlers.length;
        } else if (   options.phase == ""
        		   || targetPhaseIndex == -1) {
			/*  If the handler we’re registering isn’t assigned to any
				particular handler phase, or if it’s assigned to a phase that
				does not actually exist in this event’s handler phase order,
				we will add it just before all handlers of phase “>” (i.e.,
				those handlers specified to be called after all others).
			 */
        	for (var j = 0; j < handlers.length; j++) {
        		if (phaseAt(j).phase == ">")
        			break;
        	}

			insertAt = j;
        } else {
			/*	The handler is specified to run before, during, or after a named
				phase (i.e., not “<” or “>”) that (as we’ve confirmed already)
				exists in this event’s defined handler phase order.
			 */

            if (options.phase.startsWith("<")) {
                /*  The handler is assigned to be called before the specified
                    phase.
                 */
                for (var k = 0; k < handlers.length; k++) {
                    /*  We have found the index before which to insert, if the
                        handler at this index is assigned to be called during
                        or after our target phase, OR if it is assigned to be
                        called before, during, or after any later phase.

                        (In other words, we have passed all the handlers which
                         are assigned either to any earlier phase or to before
                         the specified phase.)
                     */
                    let phaseAtThisIndex = phaseAt(k);
                    if (   (   phaseAtThisIndex.phase == targetPhase
                        	&& phaseAtThisIndex.before == false)
                        || phaseOrder.slice(targetPhaseIndex + 1).includes(phaseAtThisIndex.phase))
                        break;
                }

                insertAt = k;
            } else if (options.phase.startsWith(">")) {
                /*  The handler is assigned to be called after the specified
                    phase.
                 */
                for (var m = handlers.length - 1; m > -1; m--) {
                    /*  We have found the index _after_ which to insert (hence
                        the `m++`), if the handler at this index is assigned to
                        be called before, during, or after the target phase, OR
                        if it is assigned to be called before, during, or after
                        any earlier phase.

                        (In other words, we have passed - moving backwards
                         through the handlers array - all the handlers which
                         are assigned to any later phase.)
                     */
                    let phaseAtThisIndex = phaseAt(m);
                    if (   phaseAtThisIndex.phase == targetPhase
                    	|| phaseOrder.slice(0, targetPhaseIndex - 1).includes(phaseAtThisIndex.phase)) {
                        m++;
                        break;
                    }
                }

                insertAt = m;
            } else {
                /*  The handler is assigned to be called during the specified
                    phase.
                 */
                for (var n = 0; n < handlers.length; n++) {
                    /*  We have found the index before which to insert, if the
                        handler at this index is assigned to be called after the
                        target phase, OR if it is assigned to be called before,
                        during, or after any later phase.

                        (In other words, we have passed all the handlers which
                         are assigned either to any earlier phase or to before
                         or during the specified phase.)
                     */
                    let phaseAtThisIndex = phaseAt(n);
                    if (   (   phaseAtThisIndex.phase == targetPhase
                        	&& phaseAtThisIndex.after == true)
                        || phaseOrder.slice(targetPhaseIndex + 1).includes(phaseAtThisIndex.phase))
                        break;
                }

                insertAt = n;
            }
        }

        /*  Add the new event handler to the named event’s handler definitions
            array, at whatever index we have now determined it should go to.
         */
        GW.notificationCenter.eventHandlers[eventName].splice(insertAt, 0, { f: f, options: options });
    },

    /*  Unregister the event handler with the given handler function from the
        specified named event (if such a handler exists).
     */
    removeHandlerForEvent: (eventName, f) => {
        if (GW.notificationCenter.eventHandlers[eventName] == null)
            return;

        GW.notificationCenter.eventHandlers[eventName].removeIf(handler => handler.f === f);
    },

    /*  Unregister all registered event handlers from the specified named event.
     */
    removeAllHandlersForEvent: (eventName) => {
        GW.notificationCenter.eventHandlers[eventName] = null;
    },

    /*  Event-specific pre-fire processing functions. Keys are event names.
        Values are functions that take the event info as an argument, and return
        modified event info.
    */
    prefireProcessors: { },

    /*  Array of events that are currently being fired. Used to avoid adding a
        handler to an event while it’s firing.
     */
    currentEvents: [ ],

    /*  Arrays (keyed to event names) of event handlers waiting to be added to
        events. A handler waits here if its addHandlerForEvent() call happened
        while the target event was firing. The handler will be added once the
        event has finished firing.
     */
    waitingHandlers: { },

    /*  Add all waiting handlers for the event, if any.
     */
    addWaitingHandlersForEvent: (eventName) => {
        if (GW.notificationCenter.waitingHandlers[eventName]) {
            GW.notificationCenter.waitingHandlers[eventName].forEach(handler => {
                if (handler.f) {
                    GW.notificationCenter.addHandlerForEvent(eventName, handler.f, handler.options);
                    handler.f = null;
                }
            });
            GW.notificationCenter.waitingHandlers[eventName] = GW.notificationCenter.waitingHandlers[eventName].filter(handler => handler.f);
        }
    },

    /*  Fire an event with the given name and event info dictionary.

        In addition to printing a console log message (if the log level is set
        to 1 or higher), this will also cause each event handler that has been
        registered for the named event to be called. (Handlers with a condition
        function specified in their event handler options will first have that
        condition function called, and the handler function will only be called
        if the condition evaluates true.)

        The event info dictionary provided to the ‘fireEvent’ function will be
        passed as the argument to each handler function (as well as to any
        condition function that is called to determine whether a handler should
        be called).

        The event info dictionary may contain various, mostly event-specific,
        keys and values. The one common key/value that any event’s info
        dictionary may contain is the ‘source’ key, whose value should be a
        string identifying the function, browser event, or other context which
        caused the given event to be fired (such as ‘DOMContentLoaded’ or
        ‘Annotations.load’). In addition to any ways in which it may be used
        by an event handler, this string (i.e. the value of the ‘source’ key)
        is (if present) included in the console message that is printed when the
        event is fired.
     */
    fireEvent: (eventName, eventInfo = { }) => {
        if (eventName == null)
            return;

        //  Register this event as currently being fired.
        GW.notificationCenter.currentEvents.push(eventName);

        /*  Store event name in info dictionary, so that event handlers can
            access it. (This permits, e.g. the same handler to handle multiple
            events, and conditionally select behavior based on which event is
            calling the handler.)
         */
        eventInfo.eventName = eventName;

        /*  The ‘16’ here is the width of the date field plus spacing.
            The “Source:” text is manually padded to be as wide
            as “[notification]”.
         */
        GWLog(`Event “${eventName}” fired.`
            + `${((eventInfo && eventInfo.source)
                  ? ("\n"
                   + "".padStart(16, ' ')
                   + "       Source:".padEnd(GW.logSourcePadLength, ' ')
                   + eventInfo.source)
                  : ""
                 )}`, "notification");

        /*  If event-specific pre-fire processing is needed, do it.
         */
        if (GW.notificationCenter.prefireProcessors[eventName])
            eventInfo = GW.notificationCenter.prefireProcessors[eventName](eventInfo);

        /*  Call all registered handlers (if any), in order.
         */
        if (GW.notificationCenter.eventHandlers[eventName]) {
            for (let i = 0; i < GW.notificationCenter.eventHandlers[eventName].length; i++) {
                let handler = GW.notificationCenter.eventHandlers[eventName][i];
                /*  If a condition function is provided, call it to determine
                    whether the handler function should be called.
                 */
                if (   handler.options.condition
                    && handler.options.condition(eventInfo) == false)
                    continue;

                /*  If the condition function evaluated true, or if no condition
                    function was provided, we call the handler.
                 */
                handler.f(eventInfo);

                /*  If the handler options specified a true value for the ‘once’
                    key, we unregister this handler after having called it once.

                    (Note that in the case of an once-only handler that’s called
                     conditionally, i.e. one with a specified condition function,
                     regardless of how many times the named event fires, the handler
                     is never automatically removed until its condition evaluates
                     true and the handler actually gets called once.)
                 */
                if (handler.options.once) {
                    GW.notificationCenter.eventHandlers[eventName].splice(i, 1);
                    i--;
                }
            }
        }

        //  Unregister this event from the list of events currently being fired.
        GW.notificationCenter.currentEvents.remove(eventName);

        //  Add any handlers that are waiting to be added.
        GW.notificationCenter.addWaitingHandlersForEvent(eventName);
    }
};


/**************************/
/* LOAD & INJECT HANDLERS */
/**************************/

/*******************************************************************************/
/*  NOTE on the GW.contentDidLoad and GW.contentDidInject events:

    These events are fired whenever any new local page content is loaded and
    injected into the page, respectively. (Here “loaded” may mean “loaded via a
    network request”, “constructed from a template”, or any other process by
    which a new unit of page content is created. This includes the initial page
    load, but also such things as annotations being lazy-loaded, etc. Likewise,
    “injected” may mean “injected into the base page”, “injected into a
    pop-frame shadow-root”, “injected into a DocumentFragment in cache”, etc.)

    Many event handlers are attached to these, because a great deal of
    processing must take place before newly-loaded page content is ready for
    presentation to the user. Typography rectification must take place; the HTML
    structure of certain page elements (such as tables, figures, etc.) must be
    reconfigured; CSS classes must be added; various event listeners attached;
    etc. Most of rewrite.js consists of exactly such “content load handlers” and
    “content inject handlers”, a.k.a. “rewrite functions”. (Additional content
    load and inject handlers are defined elsewhere in the code, as appropriate;
    e.g. the handler that attaches event listeners to annotated links to load
    annotations when the user mouses over such links, which is found in
    extracts-annotations.js.)

    The GW.contentDidLoad event has the following named handler phases (see
    above for details on what this means):

        [ "transclude", "rewrite" ]

    The GW.contentDidInject event has the following named handler phases:

        [ "rewrite", "eventListeners" ]

    The GW.contentDidLoad and GW.contentDidInject events should have the
    following keys and values in their event info dictionary (see above
    for details on event info dictionaries):

        ‘source’ (key) (required)
            String that indicates function (or event name, if fired from a
            browser event listener) from which the event is fired (such as
            ‘Annotation.load’).

        ‘container’ (key) (required)
            DOM object containing the loaded content. (For the GW.contentDidLoad
            event fired on the initial page load, the value of this key is
            `document`, i.e. the root document of the page. For pop-frames, this
            may be the `document` property of the pop-frame, or a
            DocumentFragment containing the embedded page elements.) The
            container will contain nothing but the newly-loaded content.
            (This key can be thought of as “what has been loaded?”.)

        ‘document’ (key) (required)
            Document into which the content was loaded. May or may not be
            identical with the value of the ‘container’ key (in those cases when
            the loaded content is a whole document itself). The value of this
            key is necessarily either a Document (i.e., the root document of the
            page) or a DocumentFragment. (This key can be thought of as “into
            where has the loaded content been loaded?”.)

        ‘contentType’ (key)
            String that indicates content type of the loaded content. Might be
            null (which indicates the default content type: local page content).
            Otherwise may be `annotation` or something else.

        ‘loadLocation’ (key)
            URL object (https://developer.mozilla.org/en-US/docs/Web/API/URL)
            which specifies the URL from which the loaded content was loaded.
            For the main page, the represented URL will be the value of
            `location.href`. For pop-frames, transcludes, etc., the represented
            URL will be that of the page in which the content resides. (If the
            loaded/injected content is not sourced from any page, this key will
            have a null value.)

    The GW.contentDidInject event should additionally have a value for the
    following key:

        ‘flags’ (key) (required)
            Bit field containing various flags (combined via bitwise OR). The
            values of the flags are defined in GW.contentDidInjectEventFlags.

            (Note that event handlers for the ‘GW.contentDidInject’ event can
             access the values of these flags directly via property access on
             the event info, e.g. the following two expressions are equivalent:

               eventInfo.flags & GW.contentDidInjectEventFlags.clickable != 0

               eventInfo.clickable

             It is recommended that the latter form be used.)

            The flags are:

            ‘clickable’
                Currently unused. Reserved for future use.

            ‘stripCollapses’
                Specifies whether the loaded content is permitted to have
                collapsed sections. Generally false. If the value of this key
                is true, then any collapse blocks in the loaded content will be
                automatically expanded and stripped, and all content in
                collapsible sections will be visible at all times.

            ‘fullWidthPossible’
                Specifies whether full-width elements are permitted in the
                loaded content. Generally true only for the main page load. If
                false, elements marked as full-width will be laid out as if for
                a mobile (narrow) viewport, regardless of the actual dimensions
                of the loaded content’s container (i.e. they will not actually
                be “full-width”).

			‘localize’
				Specifies whether content should be “localized” to the context
				into which it is being injected. (Affects things like link
				qualification. See transclude.js for more information.)
				Generally true for page content, false for auxiliary content.
 */

GW.contentLoadHandlers = { };

/*  Add content load handler (i.e., an event handler for the GW.contentDidLoad
    event). (Convenience function.)
 */
function addContentLoadHandler(handler, phase = "", condition = null, once = false) {
    GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", handler, {
    	phase: phase,
    	condition: condition,
    	once: once
    });
}

GW.contentInjectHandlers = { };

/*  Add content inject handler (i.e., an event handler for the
    GW.contentDidInject event). (Convenience function.)
 */
function addContentInjectHandler(handler, phase = "", condition = null, once = false) {
    GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", handler, {
    	phase: phase,
    	condition: condition,
    	once: once
    });
}

/*  Event-specific handler phase order for the ‘GW.contentDidLoad’ and
	‘GW.contentDidInject’ events.
 */
GW.notificationCenter.handlerPhaseOrders["GW.contentDidLoad"] = [ "transclude", "rewrite" ];
GW.notificationCenter.handlerPhaseOrders["GW.contentDidInject"] = [ "rewrite", "eventListeners" ];

/*  Event-specific boolean flags for the ‘GW.contentDidInject’ event.
 */
GW.contentDidInjectEventFlags = {
    clickable:          1 << 0,
    stripCollapses:     1 << 1,
    fullWidthPossible:  1 << 2,
    localize:           1 << 3,
    mergeFootnotes:     1 << 4
};

/*  Event-specific pre-fire processing for the ‘GW.contentDidInject’ event.
 */
GW.notificationCenter.prefireProcessors["GW.contentDidInject"] = (eventInfo) => {
    for (let [flagName, flagValue] of Object.entries(GW.contentDidInjectEventFlags))
        eventInfo[flagName] = (0 != (eventInfo.flags & flagValue));

    return eventInfo;
};


/********************/
/* EVENT LISTENERS */
/********************/

GW.eventListeners = { };

/******************************************************************************/
/*  Adds a named event listener to the page (or other target).

	The purpose of this function is to dramatically improve the performance of
	handler code attached to continuous UI events, such as scrolling (i.e., the 
	“scroll” event, attached to any scroll container) or window resizing (i.e., 
	the “resize” event, attached to the `window` object).

	Because such events are fired at a rate independent of the actual current
	animation frame rate, using the usual event listener system to attach 
	handlers to events of this sort causes performance to suffer, as the 
	handler is called much more often than necessary, leading to lag.

	The solution is to add an event listener that fires only once (the next 
	time the event is triggered); waits for the next animation frame; runs the 
	requisite code; then adds itself as an event listener again. This has the
	effect of running the handler code at most once per animation frame, no 
	matter how much more often the event is fired.

	This behavior is abstracted into an API which easily replaces the built-in 
	addEventListener() API.

	Available option fields:

	name (string)
		A string identifier for the event listener being added. If a name is
		provided, then a reference to the event listener is retained, such that
		the listener may later be removed (see the removeNamedEventListener()
		function).

		Provided names must be unique per event. (A listener “foo” for event 
		“bar” attached to object `baz` will overwrite a listener “foo” for 
		event “bar” attached to object `quux`.)

	defer (boolean)
		If set to true, and the page has not yet finished loading, then the 
		provided handler is not added immediately, but only on the next 
		animation frame after the page has finished loading (using the 
		doWhenPageLoaded() function). (If the page *has* finished loading, then
		the handler is added on the immediately next animation frame.)

		(This is useful for code that implements a UI behavior which should not 
		 operate prior to the page having loaded and appropriate on-load setup 
		 code having run.)

	ifDeferCallWhenAdd (boolean)
		If the `defer` option is enabled (set to true), this option controls 
		whether the provided handler function is called immediately, just prior
		to the event listener being added.

		Essentially, setting this to `true` means that we are assuming that the 
		event in question will have fired at least once between the beginning 
		of the page load process and the deferred moment when we actually add 
		the listener, and thus the handler code should be run.

		NOTE: The invocation of the handler function (fn()) immediately prior 
		to the event listener being added will *not* have any event object 
		passed to it (because it’s not being triggered by a fired event, nor
		called within a listener function that was triggered by a fired event).
		The handler function must be able to handle the case of an undefined
		`event` argument, if this option is used!

		(This option has no effect if the `defer` option is not enabled.)
 */
function addNamedEventListener(target, eventName, fn, options) {
	options = Object.assign({
		name: null,
		defer: false,
		ifDeferCallWhenAdd: false
	}, options);

    if (options.defer) {
        doWhenPageLoaded(() => {
            requestAnimationFrame(() => {
                if (options.ifDeferCallWhenAdd)
                    fn();
                addNamedEventListener(target, eventName, fn, {
                	name: options.name,
                	defer: false
                });
            });
        });

        return;
    }

    let wrapper = (event) => {
        requestAnimationFrame(() => {
        	if (wrapper.removed == true)
        		return;

            fn(event);
            target.addEventListener(eventName, wrapper, { once: true, passive: true });
        });
    }
    target.addEventListener(eventName, wrapper, { once: true, passive: true });

    /*  Retain a reference to the event listener, if a name is provided.
     */
    if (options.name) {
    	if (GW.eventListeners[eventName] == null)
    		GW.eventListeners[eventName] = { };

        GW.eventListeners[eventName][options.name] = {
        	wrapper: wrapper,
        	target: target
        };
    }

    return wrapper;
}

/*  Removes a named event listener from the page (or other target).
 */
function removeNamedEventListener(eventName, name) {
	if (GW.eventListeners[eventName] == null)
		return;

    let listener = GW.eventListeners[eventName][name];
    if (listener) {
        listener.target.removeEventListener(eventName, listener.wrapper);
        listener.wrapper.removed = true;
        GW.eventListeners[eventName][name] = null;
    }
}

/*  Adds a “scroll” event listener to the document (or other target).
 */
function addScrollListener(fn, options) {
	return addNamedEventListener((options.target ?? document), "scroll", fn, options);
}

/*  Removes a named “scroll” event listener from the document (or other 
	target).
 */
function removeScrollListener(name) {
	removeNamedEventListener("scroll", name);
}

/*  Adds a “mousemove” event listener to the window (or other target).
 */
function addMousemoveListener(fn, options) {
	return addNamedEventListener((options.target ?? window), "mousemove", fn, options);
}

/*  Removes a named “mousemove” event listener from the window (or other 
	target).
 */
function removeMousemoveListener(name) {
	removeNamedEventListener("mousemove", name);
}

/*  Adds a “resize” event listener to the window.
 */
function addWindowResizeListener(fn, options) {
	return addNamedEventListener(window, "resize", fn, options);
}

/*  Removes a named “resize” event listener from the window.
 */
function removeWindowResizeListener(name) {
	removeNamedEventListener("resize", name);
}


/****************/
/* SCROLL STATE */
/****************/

GW.scrollState = {
    lastScrollTop:              0,
    unbrokenDownScrollDistance: 0,
    unbrokenUpScrollDistance:   0
};

function updateScrollState(event) {
    GWLog("updateScrollState", "inline.js", 3);

    GW.scrollState.newScrollTop = window.pageYOffset;
    GW.scrollState.unbrokenDownScrollDistance = GW.scrollState.newScrollTop > GW.scrollState.lastScrollTop
        										? (  GW.scrollState.unbrokenDownScrollDistance
        										   + GW.scrollState.newScrollTop
        										   - GW.scrollState.lastScrollTop)
        										: 0;
    GW.scrollState.unbrokenUpScrollDistance = GW.scrollState.newScrollTop < GW.scrollState.lastScrollTop
        									  ? (  GW.scrollState.unbrokenUpScrollDistance
        									     + GW.scrollState.lastScrollTop
        									     - GW.scrollState.newScrollTop)
        									  : 0;
    GW.scrollState.lastScrollTop = GW.scrollState.newScrollTop;
}
addScrollListener(updateScrollState, {
	name: "updateScrollStateScrollListener",
	defer: true,
	ifDeferCallWhenAdd: true
});

/*  Toggles whether the page is scrollable.
 */
function isPageScrollingEnabled() {
    return !(document.documentElement.classList.contains("scroll-enabled-not"));
}
/*  Pass true or false to enable or disable (respectively) page scrolling.
    Calling this function with no arguments toggles the state (enables if
    currently disabled, or vice versa).
 */
function togglePageScrolling(enable) {
    if (typeof enable == "undefined")
        enable = document.documentElement.classList.contains("scroll-enabled-not");

    let preventScroll = (event) => {
    	document.documentElement.scrollTop = GW.scrollState.lastScrollTop;
    };

    /*  The `scroll-enabled-not` CSS class, which is added to the `html` element
        when scrolling is disabled by this function (in order to permit the
        “toggle” behavior, i.e. calling ‘togglePageScrolling’ with no
        arguments), allows the assignment of arbitrary CSS properties to the
        page on the basis of scroll state. This is purely a convenience (which
        may be useful if, for example, some styling needs to change on the basis
        of change in page scroll state, e.g. modifying the appearance of scroll
        bars). No specific CSS properties are needed in order for this function
        to work properly.
     */
    if (   enable
        && isPageScrollingEnabled() == false) {
        document.documentElement.classList.toggle("scroll-enabled-not", false);
        removeScrollListener("preventScroll");
        addScrollListener(updateScrollState, {
        	name: "updateScrollStateScrollListener"
        });
    } else if (  !enable
               && isPageScrollingEnabled() == true) {
        document.documentElement.classList.toggle("scroll-enabled-not", true);
        removeScrollListener("updateScrollStateScrollListener");
        addScrollListener(preventScroll, {
        	name: "preventScroll"
        });
    }
}


/***********/
/* DO-WHEN */
/***********/

/*  Run the given function immediately if the page is already loaded, or add
    a listener to run it as soon as the page loads.
 */
function doWhenPageLoaded(f) {
    if (document.readyState == "complete")
        f();
    else
        window.addEventListener("load", () => { f(); });
}

/*  Run the given function immediately if the page content has already loaded
    (DOMContentLoaded event has fired), or add a listener to run it as soon as
    the event fires.
 */
function doWhenDOMContentLoaded(f) {
    if (GW.DOMContentLoaded == true)
        f();
    else
        window.addEventListener("DOMContentLoaded", () => { f(); });
}

/*	Run the given function immediately if an element specified by a given
	selector exists; otherwise, add a mutation observer to run the given 
	function as soon as such an element is added to the document. The element
	is passed as an argument to the called function.
 */
function doWhenElementExists(f, selector) {
	if (document.querySelector(selector) != null) {
		f();
	} else {
        let observer = new MutationObserver((mutationsList, observer) => {
        	let element = document.querySelector(selector);
            if (element != null) {
                observer.disconnect();
                f(element);
            }
        });

        observer.observe(document.documentElement, { childList: true, subtree: true });
	}
}

/*  Run the given function immediately if the <body> element has already been
    created, or add a mutation observer to run it as soon as the <body> element
    is created.
 */
function doWhenBodyExists(f) {
    doWhenElementExists(f, "body");
}

/*  Run the given function immediately if the <main> element has already been
    created, or add a mutation observer to run it as soon as the <main> element
    is created.
 */
function doWhenMainExists(f) {
    doWhenElementExists(f, "main");
}

/*	Define convenient alias.
 */
doWhenMainExists((main) => {
	document.main = main;
});


/******************/
/* BROWSER EVENTS */
/******************/

/*  We know this is false here, because this script is loaded synchronously 
	from a <script> element in the <head> of the page; so the page body has not 
	yet loaded when this code runs.
 */
GW.DOMContentLoaded = false;

GWLog("document.readyState." + document.readyState, "browser event");
window.addEventListener("DOMContentLoaded", () => {
    GWLog("window.DOMContentLoaded", "browser event");
    GW.DOMContentLoaded = true;
    let pageURL = URLFromString(location.href);
    GW.notificationCenter.fireEvent("GW.contentDidLoad", {
        source: "DOMContentLoaded",
        contentType: "localPage",
        container: document.main,
        document: document,
        loadLocation: pageURL
    });
    GW.notificationCenter.fireEvent("GW.contentDidInject", {
        source: "DOMContentLoaded",
        contentType: "localPage",
        container: document.main,
        document: document,
        loadLocation: pageURL,
        flags: (  GW.contentDidInjectEventFlags.clickable
                | GW.contentDidInjectEventFlags.fullWidthPossible
                | GW.contentDidInjectEventFlags.localize)
    });
});
window.addEventListener("load", () => {
    GWLog("window.load", "browser event");
});
document.addEventListener("readystatechange", () => {
    GWLog("document.readyState." + document.readyState, "browser event");
});
/*********************/
/* SPECIAL OCCASIONS */
/*********************/

/******************************************************************************/
/*	Colorize the given elements (optionally, within the given container).

	The `colorizationSpecs` argument is an array of entries, each of which is a
	an array of three elements: a selector, a CSS variable name, and a color
	value. For example:

	colorizeElements([
		[ "li:nth-of-type(odd)", "--list-bullet", "#f00" ],
		[ "li:nth-of-type(even)", "--list-bullet", "#0f0" ],
		[ "hr", "--icon-image", "#f00" ]
	]);

	This will colorize the list bullets of odd-numbered list items to red, the
	list bullets of even-numbered list items to green, and the icon images of
	horizontal rules to red.

	(An optional fourth element in each entry specifies a dark mode property 
	 which should be disabled - i.e., set to `unset` - when an element is
	 colorized.)

	NOTE: In order for colorization to work properly, the given selectors must 
	have, as values of the given CSS variables, a CSS url() value containing an 
	inline-defined (and *not* base64-encoded) SVG image.
 */
function colorizeElements(colorizationSpecs, container = document.main) {
	for (let [ selector, cssVariableName, referenceColor, undesiredDarkModeProperty ] of colorizationSpecs) {
		let colorTransformFunction = (colorCode) => {
			return Color.processColorValue(colorCode, [ {
				type: Color.ColorTransform.COLORIZE,
				referenceColor: referenceColor
			} ]);
		};

		container.querySelectorAll(selector).forEach(element => {
			colorizeElement(element, cssVariableName, colorTransformFunction, undesiredDarkModeProperty);
		});
	}
}

/*********************************************************/
/*	Helper function. (See colorizeElements() for details.)
 */
function colorizeElement(element, cssVariableName, colorTransformFunction, undesiredDarkModeProperty) {
	if (element.style.getPropertyValue(cssVariableName) > "")
		uncolorizeElement(element, cssVariableName, undesiredDarkModeProperty);

	let svgSrc = getComputedStyle(element).getPropertyValue(cssVariableName).match(/url\(['"]data:image\/svg\+xml;utf8,(.+?)['"]\)/)[1];
	svgSrc = svgSrc.replaceAll("%23", "#").replaceAll("\\", "").replace(/(?<!href=)"(#[0-9A-Fa-f]+)"/g, 
		(match, colorCode) => {
			return `"${(colorTransformFunction(colorCode))}"`;
		});
	let svg = elementFromHTML(svgSrc);
	svg.setAttribute("fill", colorTransformFunction("#000"));
	svgSrc = svg.outerHTML.replaceAll("#", "%23");
	element.style.setProperty(cssVariableName, `url('data:image/svg+xml;utf8,${svgSrc}')`);

	if (undesiredDarkModeProperty)
		element.style.setProperty(undesiredDarkModeProperty, "unset");
}

/***********************************************************/
/*	Helper function. (See uncolorizeElements() for details.)
 */
function uncolorizeElement(element, cssVariableName, darkModeProperty) {
	element.style.removeProperty(cssVariableName);

	if (darkModeProperty)
		element.style.removeProperty(darkModeProperty);
}

/******************************************************************************/
/*	Undo the effects of colorizeElements(), for the given elements (optionally, 
	within the given container).

	(Each entry in the uncolorizationSpecs array need only have the selector 
	 and CSS variable name; no color need be specified.)

	(An optional third element in each entry specifies a dark mode property 
	 which should be un-disabled - i.e., the inline value of `unset` removed - 
	 when an element is un-colorized.)
 */
function uncolorizeElements(uncolorizationSpecs, container = document.main) {
	for (let [ selector, cssVariableName, darkModeProperty ] of uncolorizationSpecs) {
		container.querySelectorAll(selector).forEach(element => {
			uncolorizeElement(element, cssVariableName, darkModeProperty);
		});
	}
}

/**************************************************************************/
/*	Returns the source string for the <svg> container for an SVG page logo.
 */
function svgPageLogoContainerSourceForURL(logoURL) {
	return `<svg
			 class="logo-image visible"
			 viewBox="0 0 64 75"
			 ><use
			   href="${logoURL}#logo"
			   ></use></svg>`;
}

/***************************************************************************/
/*	Calls the provided page logo replacement function, when the page logo is
	available.
 */
function replacePageLogoWhenPossible(replaceLogo) {
    let logoSelector = "#sidebar .logo-image";
    let logoImage;
    if (logoImage = document.querySelector(logoSelector)) {
        replaceLogo(logoImage);
    } else {
        let observer = new MutationObserver((mutationsList, observer) => {
            if (   window.getSavedCount
            	&& window.getAssetPathname
            	&& window.versionedAssetURL
            	&& window.processAssetSequenceOptions
            	&& (logoImage = document.querySelector(logoSelector))) {
                observer.disconnect();
                replaceLogo(logoImage);
            }
        });
        observer.observe(document.documentElement, { childList: true, subtree: true });
    }
}

GW.allowedAssetSequencingModes = [
	"nextAfterSaved",
	"previousBeforeSaved",
	"nextAfterCurrent",
	"previousBeforeCurrent" 
];

/******************************************************************************/
/*  Inject a special page logo image of a specific type (‘halloween’,
    ‘christmas’, etc.). Directory structure and file naming for the
    specified logo type must match existing holiday logos.

    Available option fields:

	mode (string)
		May be “light” or “dark”, or null. Affects the scheme that determines
		the path and file name(s) expected for the logo image file(s). This
		option should be null if there is just the one logo image (or set of
		logo images) that is used in both light and dark mode; otherwise, the
		appropriate mode should be specified.

	identifier (string)
		If there are one or more numbered logo image files (for randomization
		purposes), but we wish to select a specific one, we may provide a
		numeric identifier string ("1", "14", etc.); the logo image file with
		that numeric identifier in the file name will be selected.

		NOTE: If this field is set, then the `sequence` and `randomize` fields 
		are ignored.

    randomize (boolean)
	sequence (string)
		[must be one of “nextAfterSaved”, “previousBeforeSaved”, 
		 “nextAfterCurrent”, or “previousBeforeCurrent”; any other value is 
		 equivalent to a null value]

		These two fields together define the sequencing behavior, over multiple 
		invocations of this function (either within a single page load session 
		or across multiple page loads), of the selection of a logo from multiple 
		available logos of the specified type.

		If neither `randomize` nor `sequence` are set, then only a single,
		specific logo image file will match the generated pattern (it will be
		an un-numbered file, if `identifier` is not set; or it will be a
		specific numbered one, if `identifier` is set).

		If `randomize` is set to true but `sequence` is not set, then a logo
		image file will be selected at random out of the available logo image
		files that match the specified criteria (type, mode, scale).	

		If `sequence` is set to either “nextAfterCurrent” or 
		“previousBeforeCurrent”, then the next logo image file after the 
		currently set logo, or the previous logo image file before the currently
		set logo (respectively) will be selected. If the currently set logo is
		not one of the logo image files that match the specified criteria, then
		the first or the last logo image files (respectively) out of the 
		available matching ones will be selected. (The `randomize` field is
		ignored in this case.)

		If `sequence` is set to either “nextAfterSaved” or 
		“previousBeforeSaved”, then the saved index of the previously selected
		logo is retrieved from localStorage.

		If there is no saved index, and `randomize` is set to true, then a
		starting index is generated randomly, in the range [1,1E6].

		Otherwise, if `sequence` is set to “nextAfterSaved”, the saved index
		is incremented by 1 (or else set to 1, if there is no saved index yet);
		if `sequence` is set to “previousBeforeSaved”, the saved index is 
		decremented by 1 (or else set to 0, if there is no saved index yet).

		(In either case, the new index is saved to localStorage.)

		That logo image will be then be selected which is the i’th in the set
		of logo image files that match the specified criteria, where i is equal
		to the new sequence index modulo the number of matching logo image 
		files.

	link (URL)
		Points the logo link to the specified URL. (If this field is not set,
		the logo link retains its current target, whatever that may be. Note 
		that by default, the logo link starts out pointing to /index.)
 */
function injectSpecialPageLogo(logoType, options) {
	options = Object.assign({
		mode: null,
		identifier: null,
		randomize: false,
		sequence: null,
		link: null
	}, options);

	//	Identifier string (empty, or hyphen plus a number, or “-%R”).
    let logoIdentifierRegexpString = ``;
    if (options.identifier) {
    	logoIdentifierRegexpString = `-${options.identifier}`;
    } else if (   options.randomize == true
    		   || GW.allowedAssetSequencingModes.includes(options.sequence)) {
    	logoIdentifierRegexpString = `-%R`;
    }

	/*	Bitmap files come in several scales (for different pixel densities of
		display); SVGs are singular.
	 */
    let scale = valMinMax(Math.ceil(window.devicePixelRatio), 1, 3);
    let fileFormatRegexpSuffix = `(\\.svg|-small-${scale}x\\.(png|jpg|webp))$`;

	/*	File name pattern further depends on whether we have separate light
		and dark logos of this sort.
	 */
	let logoPathnamePattern = `/static/img/logo/${logoType}/`
							+ (options.mode
							   ? `${options.mode}/logo-${logoType}-${options.mode}`
							   : `logo-${logoType}`)
							+ logoIdentifierRegexpString
							+ fileFormatRegexpSuffix;

    //  Temporarily brighten logo, then fade slowly after set duration.
    let brightenLogoTemporarily = (brightDuration, fadeDuration) => {
        let logoLink = document.querySelector("#sidebar a.logo");

        logoLink.classList.add("bright");
        logoLink.fadeTimer = setTimeout(() => {
            logoLink.swapClasses( [ "bright", "fading" ], 1);
            logoLink.fadeTimer = setTimeout(() => {
                logoLink.classList.remove("fading");
            }, fadeDuration);
        }, brightDuration);

        //  Ensure proper interaction with mouse hover.
        logoLink.addEventListener("mouseenter", (event) => {
            logoLink.classList.remove("fading");
        });
        logoLink.addEventListener("mouseleave", (event) => {
            logoLink.classList.remove("fading");
        });
    };

    /*  Note that getAssetPathname(), versionedAssetURL(), and 
    	processAssetSequenceOptions() are defined in misc.js, and so cannot be 
    	called prior to this.
     */
    replacePageLogoWhenPossible(logoImage => {
		//	Get enclosing link, in case we have to modify it.
		let logoLink = logoImage.closest("a");

        //  Get new logo URL (specified, random, or sequenced).
		let logoPathname = getAssetPathname(logoPathnamePattern, processAssetSequenceOptions(options, {
			currentAssetURL: URLFromString(   logoImage.querySelector("use")?.getAttribute("href") 
										   ?? logoImage.querySelector("img")?.src),
			assetSavedIndexKey: `logo-sequence-index-${logoType}`
		}));

        let versionedLogoURL = versionedAssetURL(logoPathname);

		if (logoPathname.endsWith(".svg")) {
            //  Inject inline SVG.
            logoImage.replaceWith(elementFromHTML(svgPageLogoContainerSourceForURL(versionedLogoURL)));
        } else {
            //  Create new image element and wrapper.
            let imageWrapper = newElement("SPAN", {
                class: "logo-image"
            });
            imageWrapper.appendChild(newElement("IMG", {
                class: "figure-not",
                src: versionedLogoURL.pathname + versionedLogoURL.search
            }));

            //  Inject wrapped image.
            logoImage.replaceWith(imageWrapper);
        }

		//	Point the logo link at the provided URL.
		if (options.link)
			logoLink.href = options.link;

        //  Brighten logo; fade (over 1 second) after 20 seconds.
        brightenLogoTemporarily(20 * 1000, 1000);
    });
}

/*******************************************************************/
/*	Resets the saved sequence index of the specified page logo type.

	(See comment for injectSpecialPageLogo() for more information.)
 */
function resetPageLogoSequenceIndex(logoType) {
	localStorage.removeItem(`logo-sequence-index-${logoType}`);
}

/******************************************/
/*	Reset the page logo to the default one.
 */
function resetPageLogo() {
    /*  Note that versionedAssetURL() is defined in misc.js, and so cannot be 
    	called too early.
     */
    replacePageLogoWhenPossible(logoImage => {
		//	Get enclosing link.
		let logoLink = logoImage.closest("a");

		//	Get versioned logo URL.
        let versionedLogoURL = versionedAssetURL("/static/img/logo/logo-smooth.svg");

		//	Inject inline SVG.
		logoImage.replaceWith(elementFromHTML(svgPageLogoContainerSourceForURL(versionedLogoURL)));

		//	Point the logo link back to the index page.
		logoLink.href = "/index";
    });
}

/*****************/
/*  Configuration.
 */

GW.specialOccasionTestLocalStorageKeyPrefix = "special-occasion-test-";

GW.specialOccasionTestPageNamePrefix = "test-";

/*  If a special function is provided to apply classes, one should also be
    provided to remove those classes. (See the ‘halloween’ entry for example.)
 */
GW.specialOccasions = [
    [ "halloween", isItHalloween, () => {
        //  Default to dark mode during Halloween.
        DarkMode.defaultMode = "dark";

        //  Different special styles for light and dark mode.
        document.body.classList.remove("special-halloween-dark", "special-halloween-light");
        let specialClass = DarkMode.computedMode() == "light"
                           ? "special-halloween-light"
                           : "special-halloween-dark";
        document.body.classList.add(specialClass);

        //  Replace logo.
        let newLogoLink = URLFromString("/dropcap#halloween");
        doWhenMatchMedia(matchMedia("(min-width: 1180px)"), "GW.setHalloweenPageLogoForViewportWidth",
           (mediaQuery) => {
        	injectSpecialPageLogo("halloween", {
        		mode: "dark", 
        		sequence: "previousBeforeSaved",
        		link: newLogoLink
        	});
        }, (mediaQuery) => {
			injectSpecialPageLogo("halloween", {
				mode: "dark",
				identifier: "1",
				link: newLogoLink
			});
        }, (mediaQuery) => {
        	resetPageLogo();
        });
      }, () => {
        document.body.classList.remove("special-halloween-dark", "special-halloween-light");
        cancelDoWhenMatchMedia("GW.setHalloweenPageLogoForViewportWidth");
		resetPageLogoSequenceIndex("halloween");
      } ],
    [ "christmas", isItChristmas, () => {
        //  Different special styles for light and dark mode.
        document.body.classList.remove("special-christmas-dark", "special-christmas-light");
        let specialClass = DarkMode.computedMode() == "light"
                           ? "special-christmas-light"
                           : "special-christmas-dark";
        document.body.classList.add(specialClass);

        //  Replace logo.
        let newLogoLink = URLFromString("/dropcap#christmas");
        doWhenMatchMedia(matchMedia(""), "GW.setChristmasPageLogo",
           (mediaQuery) => {
			injectSpecialPageLogo("christmas", {
				mode: DarkMode.computedMode(),
				sequence: "previousBeforeSaved",
				link: newLogoLink
			});
			doWhenPageLoaded(() => {
				colorizeElements([
					[ "ul > li:nth-of-type(odd)", "--list-bullet", "#f00", "--list-bullet-dark-mode-invert-filter" ],
					[ "ul > li:nth-of-type(even)", "--list-bullet", "#0f0", "--list-bullet-dark-mode-invert-filter" ],
					[ "div[class^='horizontal-rule']:nth-of-type(odd) hr", "--icon-image", "#f00", "--icon-dark-mode-invert-filter" ],
					[ "div[class^='horizontal-rule']:nth-of-type(even) hr", "--icon-image", "#0b0", "--icon-dark-mode-invert-filter" ],
					[ "#x-of-the-day", "--ornament-image-left", "#f00", "--ornament-dark-mode-invert-filter" ],
					[ "#x-of-the-day", "--ornament-image-right", "#f00", "--ornament-dark-mode-invert-filter" ],
					[ "#footer-decoration-container .footer-logo", "--logo-image", "#c00", "--logo-image-dark-mode-invert-filter" ]
				]);
				doWhenElementExists(() => {
					colorizeElements([
						[ "#x-of-the-day .site-of-the-day blockquote", "--background-image", "#126512" ],
						[ "#x-of-the-day .site-of-the-day blockquote", "--background-image-sides", "#126512" ]
					]);
				}, "#x-of-the-day .site-of-the-day");
			});
        }, null, (mediaQuery) => {        	
			resetPageLogo();
			doWhenPageLoaded(() => {
				uncolorizeElements([
					[ "ul > li", "--list-bullet", "--list-bullet-dark-mode-invert-filter" ],
					[ "hr", "--icon-image", "--icon-dark-mode-invert-filter" ],
					[ "#x-of-the-day", "--ornament-image-left", "--ornament-dark-mode-invert-filter" ],
					[ "#x-of-the-day", "--ornament-image-right", "--ornament-dark-mode-invert-filter" ],
					[ "#x-of-the-day .site-of-the-day blockquote", "--background-image" ],
					[ "#x-of-the-day .site-of-the-day blockquote", "--background-image-sides" ]
					[ "#footer-decoration-container .footer-logo", "--logo-image", "--logo-image-dark-mode-invert-filter" ]
				]);
				doWhenElementExists(() => {
					uncolorizeElements([
						[ "#x-of-the-day .site-of-the-day blockquote", "--background-image" ],
						[ "#x-of-the-day .site-of-the-day blockquote", "--background-image-sides" ]
					]);
				}, "#x-of-the-day .site-of-the-day");
			});
        });
      }, () => {
        document.body.classList.remove("special-christmas-dark", "special-christmas-light");
        cancelDoWhenMatchMedia("GW.setChristmasPageLogo");
		resetPageLogoSequenceIndex("christmas");
      } ],
    [ "april-fools", isItAprilFools, () => {
        document.body.classList.add("special-april-fools");
        // TODO: no April Fools logos or dropcaps.. for now. Maybe 2025?

        /*  Turn off the funny after half a minute (the blackletter joke has
            worn old by then).
         */
        let jokeDurationSeconds = 30;
        setTimeout(() => {
            document.body.classList.remove("special-april-fools");
        }, jokeDurationSeconds * 1000);
      }, () => {
        document.body.classList.remove("special-april-fools");
      } ],
    [ "easter", isItEaster, () => {
        document.body.classList.add("special-easter");
        //  Replace logo.
//         injectSpecialPageLogo("easter");
      }, () => {
        document.body.classList.remove("special-easter");
      } ],
];

/******************************************************************************/
/*  Debugging function; pass a special occasion identifier string (e.g.
    “halloween”, “christmas”, “april-fools”, etc.), and either `true` to enable
    testing of that special occasion, or `false` (or nothing) to disable
    testing of that special occasion. (Calling this function with no arguments,
    or null for the first argument, disables all special occasion testing.)

    When testing is enabled for a special occasion, the special occasion code
    will behave as if that special occasion is currently taking place (and any
    special-occasion-specific styling or other modifications will be applied).

    NOTE: Testing for multiple special occasions may be enabled simultaneously,
    but this results in undefined behavior.
 */
function toggleSpecialOccasionTest(specialOccasionName = null, enable = false) {
    if (specialOccasionName == null) {
        let activeSpecialOccasionTestKeys = [ ];
        for (let i = 0; i < localStorage.length; i++) {
            let localStorageKey = localStorage.key(i);
            if (localStorageKey.startsWith(GW.specialOccasionTestLocalStorageKeyPrefix))
                activeSpecialOccasionTestKeys.push(localStorageKey);
        }
        activeSpecialOccasionTestKeys.forEach(key => {
            localStorage.removeItem(key);
        });

        return;
    }

    let specialOccasionTestKey = GW.specialOccasionTestLocalStorageKeyPrefix + specialOccasionName;
    if (enable) {
        localStorage.setItem(specialOccasionTestKey, true)
    } else {
        localStorage.removeItem(specialOccasionTestKey);
    }
}

/********************************************/
/*  Returns true if it’s Halloween right now.

    Test page: </lorem-halloween>
*/
function isItHalloween() {
	//	The test page is Halloween Town.
	if (   document.body.classList.contains(GW.specialOccasionTestPageNamePrefix + "halloween")
		|| localStorage.getItem(GW.specialOccasionTestLocalStorageKeyPrefix + "halloween") == "true")
		return true;

	// 	Match languages from Halloween-celebrating regions (which has gone global <https://en.wikipedia.org/wiki/Halloween#Geography>)
	//	prefix to match language groups like ‘en’, ‘en-US’, ‘en-GB’, ‘en-AU’...
	//	Primary: en,ga,gd (English/Irish/Scots) | Moderate: de,nl,fr,es (Europe) + ja,ko (Asia)
	const halloweenLangs = new Set(['en','ga','gd','de','nl','fr','es','ja','ko']);

	let langCode = (window.navigator.userLanguage ?? window.navigator.language).slice(0, 2).toLowerCase();
	if (halloweenLangs.has(langCode)) {
		let now = new Date();
		let date = now.toString().slice(4,10);
		let hour = now.getHours();

		/*  It is a sin to celebrate Halloween while there is daylight; however,
			calculating local sunset or local ambient light is too hard (where
			would we even get that geolocation or light sensor data from‽), so
			we will simply define 'night' as ≥6PM and <6AM.
		 */
		return (date == "Oct 31" && hour >= 18) || (date == "Nov 01" && hour < 6);
	} else {
	   return false;
	}
}

/********************************************/
/*  Returns true if it’s Christmas right now.

    Test page: </lorem-christmas>
*/
function isItChristmas() {
    //  The test page is Christmas Town.
    if (   document.body.classList.contains(GW.specialOccasionTestPageNamePrefix + "christmas")
        || localStorage.getItem(GW.specialOccasionTestLocalStorageKeyPrefix + "christmas") == "true")
        return true;

    let now = new Date();
    let date = now.toString().slice(4,10);
    let hour = now.getHours();

    /*  Christmas = Christmas Eve + all Christmas Day; Christmas Eve starts in
        the evening, so again ≥6PM. */
    return (date == "Dec 24" && hour >= 18) || (date == "Dec 25");
}

/***************************************************/
/*  Returns true if it’s April Fool’s Day right now.

    Test page: </lorem-april-fools>
*/
function isItAprilFools() {
    //  The test page is blackletterFraktur-town.
    if (   document.body.classList.contains(GW.specialOccasionTestPageNamePrefix + "april-fools")
        || localStorage.getItem(GW.specialOccasionTestLocalStorageKeyPrefix + "april-fools") == "true")
        return true;

    let now = new Date();
    let date = now.toString().slice(4,10);
    let hour = now.getHours();

    /*  We don’t define April Fools as all-day April 1st,
        because too early in the morning no one is awake enough for pranks,
        and after 3PM it’s honestly kinda tiresome. */
    return (date == "Apr 01" && hour >= 8 && hour <= 15);
}

/*****************************************/
/*  Returns true if it’s Easter right now.

    Test page: </lorem-easter>
*/
function isItEaster() {
    if (   document.body.classList.contains(GW.specialOccasionTestPageNamePrefix + "easter")
        || localStorage.getItem(GW.specialOccasionTestLocalStorageKeyPrefix + "easter") == "true")
        return true;

    /*  Easter dates 2024–2050 from <https://www.assa.org.au/edm/#List20>;
        the JS computus (<https://en.wikipedia.org/wiki/Date_of_Easter>)
        algorithms on StackOverflow etc are squirrely enough I’d rather just
        hardwire it. Should I need to update it in 2051, that’s fine...
     */
    let easterDates = [
        "2024-03-31", "2025-04-20", "2026-04-05", "2027-03-28", "2028-04-16",
        "2029-04-01", "2030-04-21", "2031-04-13", "2032-03-28", "2033-04-17",
        "2034-04-09", "2035-03-25", "2036-04-13", "2037-04-05", "2038-04-25",
        "2039-04-10", "2040-04-01", "2041-04-21", "2042-04-06", "2043-03-29",
        "2044-04-17", "2045-04-09", "2046-03-25", "2047-04-14", "2048-04-05",
        "2049-04-18", "2050-04-10"
    ];

    let today = new Date().toISOString().slice(0, 10); // format: `YYYY-MM-DD`

    return easterDates.includes(today);
}

/******************************************************************************/
/*  Applies or removes special-occasion-related CSS classes to the <body>
    element.

    For each special occasion defined in GW.specialOccasions, calls the
    specified testing function (e.g., isItHalloween()).

    If the test returns true, then calls the specified application function if
    one is provided; otherwise just adds to <body> a class
    `special-` + <the name of the special occasion> (e.g., “halloween”).

    If the test returns false, then calls the specified removal function if
    one is provided. If no such function is provided, AND there is no
    application function either (and thus the application consisted merely
    of the default action of adding the default class `special-WHATEVER`), now
    simply removes that default class.

    NOTE: If an application function is provided, but no corresponding removal
    function is provided, then this function will do nothing when an active
    special occasion mode is toggled off! That is why it’s important to provide
    a removal function when providing an application function (see the existing
    entries in GW.specialOccasions for examples).
*/
function applySpecialOccasionClasses() {
    for (let occasion of GW.specialOccasions) {
        let [ name, test, doIfTrue, doIfFalse ] = occasion;
        if (test()) {
            if (doIfTrue)
                doIfTrue();
            else
                document.body.classList.add("special-" + name);
        } else {
            if (doIfFalse)
                doIfFalse();
            else if (!doIfTrue)
                document.body.classList.remove("special-" + name);
        }
    }
}

/***************************************************************************/
/*  Apply special occasion classes (if need be) when the <body> element is
    created; update them (applying or removing, as appropriate) whenever the
    mode changes.
 */
doWhenBodyExists(() => {
    applySpecialOccasionClasses();
    GW.notificationCenter.addHandlerForEvent("DarkMode.computedModeDidChange", (info) => {
        applySpecialOccasionClasses();
    });
    DarkMode.setMode();
});
/**********/
/* LAYOUT */
/**********/

GW.layout = {
	initialPageLayoutComplete: false,

	optionsCache: { },

	blockContainersNeedingLayout: [ ],

	layoutProcessors: [ ],

	//	Block containers
	blockContainers: [
		".markdownBody",
		"section",
		".collapse-block",
		"blockquote",
		".epigraph",
		".admonition",
		".sidenote",
		"#x-of-the-day"
	],

	//	Block elements get layout classes applied to them.
	blockElements: [
		"section",
		".collapse-block",
		"blockquote",
		".epigraph",
		"p",
		".columns",
		".footnote",
		"figure",
		"iframe",
		"hr",
		"div.sourceCode",
		".table-wrapper",
		".math.block",
		".admonition",
		".TOC",
		".interview .exchange",
		".interview .utterance"
	],

	//	Wrappers are transparent at the top and bottom.
	wrapperElements: [
		"div",
		"span",
		".list",
		"li",
		".parsed-raw-block"
	],

	//	Half-wrappers are transparent at the bottom only, not the top.
	halfWrapperElements: [
		"section"
	],

	//	Elements which do not participate in block layout.
	skipElements: [
		".empty",
		".empty-graf",
		".hidden",
		".float",
		"noscript",
		"button",
		"a:empty",
		".heading.collapse"
	],

	//	Elements which always participate in block layout, even when empty.
	nonEmptyElements: [
		"hr"
	],

	//	Do not apply block layout classes to or within these elements.
	blockLayoutExclusionSelector: [
		"#page-metadata",
		".TOC > *",
		".popframe",
		"#hidden-sidenote-storage"
	].join(", "),

	//	Isolate layout within these elements.
	layoutIsolationSelector: [
		".sidenote-column"
	].join(", "),

	emptyNodeExclusionPredicate: (node) => {
		if (node.nodeType != Node.ELEMENT_NODE)
			return false;

		/*	Exclude elements that have any classes (discounting 
			classes added by the layout system).
		 */
		let layoutClasses = [
			"block",
			"first-block",
			"empty-graf",
			"first-graf",
			"intro-graf",
			"list",
			"in-list",
			"float",
			"has-floats",
			"overlap-not",
			"force-dropcap",
			"heading"
		];
		let classes = Array.from(node.classList).filter(cssClass => 
			   layoutClasses.includes(cssClass) == false
			&& /^dropcaps?-/.test(cssClass) == false
		);
		if (classes.length > 0)
			return true;

		//	Exclude elements that have any data attributes.
		if (Object.keys(node.dataset).length > 0)
			return true;

		return false;
	},

	blockSpacing: [
		[ "body.page-index section",					 7, false ],
		[ "body.page-index section li p + p",			 0, false ],

		[ "body.page-404 .display-entry .float + .epigraph",
														 0, false ],
		[ "body.page-404 section:first-of-type",		16, false ],

		[ "section#see-also.first-block", 				 4, false ],

		[ "#link-bibliography-section",                 15, false ],

		[ "body.page-blog-index p.data-field.title:not(.first-block)",	
														10, false ],
		[ "body.blog-page p.data-field + .data-field.annotation-abstract p", 
														 7, false ],
		[ "body.blog-page .aux-links-append + .aux-links-append",
														 4, false ],
		[ "body.blog-page .aux-links-append + .file-include-collapse",
														 4, false ],

		[ ".float.first-block",			 2, false ],
		[ ".first-block",				 0, false ],

		[ ".heading + section",			 5, false ],
		[ ".heading + *",				 4, false ],

		[ ".tweet .tweet-content",		 3, false ],
		[ ".tweet .tweet-content p",	 3, false ],
		[ ".tweet figure",				 8, false ],

		[ "p.data-field.title + p.data-field",
										 1, false ],
		[ "p.data-field.title + .data-field",
										 3, false ],

		[ ".annotation .data-field.file-includes .collapse + annotation .collapse",
										 5, false ],
		[ ".annotation .data-field + .annotation .collapse",
										 5, false ],
		[ ".annotation .collapse.bare-content + annotation .collapse.bare-content",
										 4, false ],
		[ ".annotation * + annotation .collapse.bare-content",
										 6, false ],

		[ ".section-backlinks-container + .section-backlinks-container",
										 4, false ],
		[ ".aux-links-append + .aux-links-append",
										 0, false ],
		[ ".collapse.expanded-not p.aux-links-list-label + *",
										 0, false ],
		[ ".aux-links-append + .file-include-collapse",
										 0, false ],

		[ ".interview .exchange",		 4, false ],
		[ ".interview .utterance",		 2, false ],

		[ ".admonition-title > p + p",   1, false ],

		[ "p.footnote-back-block",		 1, false ],
		[ "p.in-list + p.data-field",	10 ],
		[ "p.first-graf",				10 ],
		[ "p.list-heading",				10 ],
		[ "p",							 0 ],

		[ ".TOC",						10 ],

		[ ".collapse-block",			10 ],

		[ "section.level1",				15 ],
		[ "section.level2",				13 ],
		[ "section.level3",				11 ],
		[ "section.level4",				10 ],
		[ "section.level5",				 9 ],
		[ "section.level6",				 8 ],

		[ "section.footnotes",			14 ],
		[ ".footnote",					 6 ],

		[ "hr",							10 ],

		[ ".aux-links-append .columns",	 4 ],
		[ ".columns",					 6 ],

		[ "figure.outline-not",			 9 ],
		[ "figure",						10 ],

		[ "iframe",						10 ],

		[ "blockquote",					10 ],

		[ ".epigraph",					 6 ],

		[ "div.table-wrapper",			10 ],

		[ "div.sourceCode",				10 ],

		[ ".math.block",				10 ],

		[ ".admonition",				10 ],
	],

	blockSpacingAdjustments: [
		[ "p + p",							(bsm, block) => bsm - 6 ],

		[ [ "p + :not(p)",
			":not(p) + p"
			],								(bsm, block) => bsm - 2 ],

		[ [ "p + blockquote",
			"blockquote + p",
			"blockquote + blockquote"
			],								(bsm, block) => bsm - 2 ],

		[ [	"p + .math.block",
			".math.block + p",
			".math.block + .math.block"
			],								(bsm, block) => bsm - 2 ],

		[ [ ".in-list + .in-list",
			".list-heading + .in-list"
			],								(bsm, block) => bsm - 2 ],

		[ ".aux-links-append p + .aux-links-append p.list-heading",
											(bsm, block) => bsm + 2 ],

		[ "figcaption *",					(bsm, block) => bsm - 2 ],

		[ ".TOC + .collapse-block",		 	(bsm, block) => bsm - 4 ],
	]
};

//	Add support for .desktop-not and .mobile-not classes.
GW.layout.skipElements.push(GW.mediaQueries.mobileWidth.matches ? ".mobile-not" : ".desktop-not");

//	Skip non-layout-containing blocks if they themselves appear in block flow.
GW.layout.skipElements.push(...(GW.layout.blockLayoutExclusionSelector.split(", ")));

//	Default block sequence function options.
GW.layout.defaultOptions = processLayoutOptions({
	blockContainers: GW.layout.blockContainers,
	blockElements: GW.layout.blockElements,
	skipElements: GW.layout.skipElements,
	nonEmptyElements: GW.layout.nonEmptyElements,
	wrapperElements: GW.layout.wrapperElements,
	halfWrapperElements: GW.layout.halfWrapperElements
});

//	Needed so that predicates (like isBlock()) can be called prior to layout.
GW.layout.currentPassBegin = 1;

/**********************************************************************/
/*	Registers a layout processor function, which will be applied to all
	rendered content as part of the dynamic layout process.

	Available option fields:

	blockLayout (boolean)
		If set to true (the default), then this will be treated as a block
		layout processor (and block layout exclusions will be applied to it).
		Otherwise, block layout exclusions will be ignored.
 */
function addLayoutProcessor(name, processor, options) {
	options = Object.assign({
		blockLayout: true
	}, options);

	//	Reference for easy direct calling.
	GW.layout[name] = processor;

	//	Add to layout processor list.
	GW.layout.layoutProcessors.push({
		name: name,
		processor: processor,
		options: options
	});
}

/******************************************************************************/
/*	Applies given layout processor to given block container within the given
	container.

	Fires didComplete event for each time a layout processor fires.
 */
function applyLayoutProcessorToBlockContainer(processorSpec, blockContainer, container) {
	processorSpec.processor(blockContainer);

	GW.notificationCenter.fireEvent("Layout.layoutProcessorDidComplete", {
		document: container.getRootNode(),
		container: container,
		processorName: processorSpec.name,
		blockContainer: blockContainer
	});
}

/****************************************************/
/*	Activates dynamic layout for the given container.
 */
function startDynamicLayoutInContainer(container) {
	let blockContainersSelector = GW.layout.blockContainers.map(
		part => (part == ".markdownBody") ? part : (".markdownBody " + part)
	).join(", ");

	let observer = new MutationObserver((mutationsList, observer) => {
		//	Construct list of all block containers affected by these mutations.
		let affectedBlockContainers = [ ];
		for (let mutationRecord of mutationsList) {
			//	Find nearest ancestor block container.
			let nearestBlockContainer = mutationRecord.target.closest(blockContainersSelector);
			if (nearestBlockContainer == null)
				continue;

			/*	Track whether block layout should be done in this block 
				container, or only non-block rewrites.
			 */
			let blockContainerEntry = {
				blockContainer: nearestBlockContainer,
				doBlockLayout: true
			};

			//	Enforce layout isolation zones.
			let isolationZone = mutationRecord.target.closest(GW.layout.layoutIsolationSelector);
			if (isolationZone?.closest(blockContainersSelector) == nearestBlockContainer) {
				isolationZone.querySelectorAll(blockContainersSelector).forEach(isolatedBlockContainer => {
					//	Avoid adding a block container twice.
					if (affectedBlockContainers.findIndex(x => x.blockContainer == isolatedBlockContainer) == -1) {
						affectedBlockContainers.push({
							blockContainer: isolatedBlockContainer,
							doBlockLayout: true
						});
					}
				});
			} else {
				//	Enforce block layout exclusion zones.
				if (mutationRecord.target.closest(GW.layout.blockLayoutExclusionSelector) != null)
					blockContainerEntry.doBlockLayout = false;

				//	Avoid adding a block container twice.
				if (affectedBlockContainers.findIndex(x => x.blockContainer == blockContainerEntry.blockContainer) == -1)
					affectedBlockContainers.push(blockContainerEntry);
			}			
		}

		/*	Exclude block containers that are contained within other block
			containers in the list, to prevent redundant processing.
		 */
		affectedBlockContainers = affectedBlockContainers.filter(c => affectedBlockContainers.findIndex(x => {
			//	This means “x contains c”.
			return (c.blockContainer.compareDocumentPosition(x.blockContainer) & Node.DOCUMENT_POSITION_CONTAINS);
		}) == -1);

		/*	Add containers to list of containers needing layout processing, if
			they are not there already.
		 */
		affectedBlockContainers.forEach(blockContainerEntry => {
			if (GW.layout.blockContainersNeedingLayout.includes(blockContainerEntry) == false)
				GW.layout.blockContainersNeedingLayout.push(blockContainerEntry);
		});
		requestAnimationFrame(() => {
			GW.layout.currentPassBegin = performance.now();

			//	Do layout in all waiting block containers.
			while (GW.layout.blockContainersNeedingLayout.length > 0) {
				let nextBlockContainerEntry = GW.layout.blockContainersNeedingLayout.shift();
				GW.layout.layoutProcessors.forEach(processorSpec => {
					if (   processorSpec.options.blockLayout == false
						|| nextBlockContainerEntry.doBlockLayout == true)
						applyLayoutProcessorToBlockContainer(processorSpec, nextBlockContainerEntry.blockContainer, container);
				});
			}
		});
	});

	observer.observe(container, { subtree: true, childList: true });
}

/*****************************************************************************/
/*	Process layout options object, so that it contains all the appropriate
	defaults (from GW.layout). (This must be done before the options object is
	read or used in any way except being passed to another function.)
 */
function processLayoutOptions(options) {
	if (options == null)
		return GW.layout.defaultOptions;

	if (options["blockElementsSelector"] != null)
		return options;

	let cacheKey = options.cacheKey;
	if (cacheKey == null) {
		cacheKey = "";
		for (let [ key, value ] of Object.entries(options))
			cacheKey += `| ${key}: ` + value.join(", ");
		options.cacheKey = cacheKey;
	}
	if (GW.layout.optionsCache[cacheKey])
		return GW.layout.optionsCache[cacheKey];

	[	"blockContainers",
		"blockElements",
		"skipElements",
		"nonEmptyElements",
		"wrapperElements",
		"halfWrapperElements"
	].forEach(optionKey => {
		let option = options[optionKey];
		if (option == null) {
			option = GW.layout[optionKey];

			let capitalizedOptionKey = optionKey.slice(0, 1).toUpperCase() + optionKey.slice(1);

			let alsoOption = options["also" + capitalizedOptionKey];
			if (alsoOption != null)
				option = option.concat(alsoOption);

			let notOption = options["not" + capitalizedOptionKey];
			if (notOption != null)
				option = option.filter(x => notOption.includes(x) == false);

			options[optionKey] = option;
		}

		if ([ "wrapperElements", "halfWrapperElements" ].includes(optionKey) == false)
			options[optionKey + "Selector"] = option.join(", ");
	});

	options.wrapperOptions = { };

	let topFilter = (x => [ ...options.wrapperElements, ...options.halfWrapperElements ].includes(x) == false);
	options.wrapperOptions["downOut"] = {
		blockElementsSelector: options.blockElements.filter(topFilter).join(", "),
		blockContainersSelector: options.blockContainers.filter(topFilter).join(", "),
		wrappersSelector: [ ...options.wrapperElements, ...options.halfWrapperElements ].join(", ")
	};
	options.wrapperOptions["upIn"] = options.wrapperOptions["downOut"];

	let bottomFilter = (x => options.wrapperElements.includes(x) == false);
	options.wrapperOptions["downIn"] = {
		blockElementsSelector: options.blockElements.filter(bottomFilter).join(", "),
		blockContainersSelector: options.blockContainers.filter(bottomFilter).join(", "),
		wrappersSelector: options.wrapperElements.join(", ")
	};
	options.wrapperOptions["upOut"] = options.wrapperOptions["downIn"];

	GW.layout.optionsCache[cacheKey] = options;

	return options;
}

/***************************************************************************/
/*	Retrieve desired result from element’s layout cache, or calculate it and
	store in element’s layout cache; and, in any case, return.
 */
function useLayoutCache(element, action, options, f) {
	options = processLayoutOptions(options);

	let cacheKey = `${action} ${options.cacheKey}`;

	if (  (element.layoutCache?.time ?? 0) < GW.layout.currentPassBegin
		|| element.layoutCache[cacheKey] == null) {
		if ((element.layoutCache?.time ?? 0) < GW.layout.currentPassBegin)
			element.layoutCache = { time: GW.layout.currentPassBegin };

		element.layoutCache[cacheKey] = f(element, options);
	}

	return element.layoutCache[cacheKey];
}

/***************************************************************************/
/*	Returns true if element is a wrapper of the given type, false otherwise.

	Types: upOut, downOut, upIn, downIn
 */
function isWrapper(element, wrapperType, options) {
	if (element == null)
		return null;

	return useLayoutCache(element, "isWrapper_" + wrapperType, options, (element, options) => {
		return (   element?.matches(options.wrapperOptions[wrapperType].wrappersSelector) == true
				&& element?.matches(options.wrapperOptions[wrapperType].blockElementsSelector) != true
				&& element?.matches(options.wrapperOptions[wrapperType].blockContainersSelector) != true);
	});
}

/*****************************************************************/
/*	Returns true if element is a skipped element, false otherwise.
 */
function isSkipped(element, options) {
	if (element == null)
		return null;

	return useLayoutCache(element, "isSkipped", options, (element, options) => {
		return (element?.matches(options.skipElementsSelector) == true);
	});
}

/**************************************************************/
/*	Returns true if element is a layout block, false otherwise.
 */
function isBlock(element, options) {
	if (element == null)
		return null;

	return useLayoutCache(element, "isBlock", options, (element, options) => {
		return (element?.matches(options.blockElementsSelector) == true);
	});
}

/***************************************************************************/
/*	Returns true if element is an always-not-empty element, false otherwise.
 */
function isNonEmpty(element, options) {
	if (element == null)
		return null;

	return useLayoutCache(element, "isNonEmpty", options, (element, options) => {
		return (element?.matches(options.nonEmptyElementsSelector) == true);
	});
}

/******************************************************************/
/*	Returns nearest enclosing block container of the given element.
	(Might be null.)
 */
function blockContainerOf(element, options) {
	if (element == null)
		return null;

	return useLayoutCache(element, "blockContainer", options, (element, options) => {
		return element.parentElement?.closest(options.blockContainersSelector);
	});
}

/**************************************************************************/
/*	Returns layout block sequential (next or previous) to the given element
	in the block flow.
 */
function sequentialBlockOf(element, direction, options) {
	if (element == null)
		return null;

	options = processLayoutOptions(options);

	let siblingKey = direction + "ElementSibling";
	let wrapperDirection = (direction == "next" ? "down" : "up");
	let wrapperInType = wrapperDirection + "In";
	let wrapperOutType = wrapperDirection + "Out";
	let terminus = (direction == "next" ? "first" : "last");

	//	Skip elements that don’t participate in block flow.
	if (isSkipped(element[siblingKey], options))
		return sequentialBlockOf(element[siblingKey], direction, options);

	//	Look inside “transparent” wrappers (that don’t affect layout).
	if (isWrapper(element[siblingKey], wrapperInType, options)) {
		let terminalBlock = terminalBlockOf(element[siblingKey], terminus, options);
		if (terminalBlock)
			return terminalBlock;
	}

	//	Skip empty elements.
	if (   isNodeEmpty_metadataAware(element[siblingKey]) == true
		&& isNonEmpty(element[siblingKey], options) == false)
		return sequentialBlockOf(element[siblingKey], direction, options);

	//	An actual block element (the base case).
	if (isBlock(element[siblingKey], options))
		return element[siblingKey];

	/*	If we’re asked for the sequential block of the terminal child of a
		transparent wrapper, we return the sequential block of that wrapper
		(recursively, of course).
	 */
	if (isWrapper(element.parentElement, wrapperOutType, options))
		return sequentialBlockOf(element.parentElement, direction, options);

	return null;
}

/************************************************************************/
/*	Returns layout block previous to the given element in the block flow.
	(Might be null.)
 */
function previousBlockOf(element, options) {
	if (element == null)
		return null;

	return useLayoutCache(element, "previousBlock", options, (element, options) => {
		return sequentialBlockOf(element, "previous", options);
	});
}

/************************************************************************/
/*	Returns layout block next from the given element in the block flow.
	(Might be null.)
 */
function nextBlockOf(element, options) {
	if (element == null)
		return null;

	return useLayoutCache(element, "nextBlock", options, (element, options) => {
		return sequentialBlockOf(element, "next", options);
	});
}

/***************************************************************/
/*	Returns terminal (first or last) layout block of an element.
	(Might be the element itself, or null.)
 */
function terminalBlockOf(element, terminus, options, strictDescent = false) {
	if (element == null)
		return null;

	options = processLayoutOptions(options);

	let wrapperType = (terminus == "first" ? "down" : "up") + "In";

	//	Look inside wrappers (or any block, if strictDescent is specified).
	if (   strictDescent == true
		|| isWrapper(element, wrapperType, options)) {
		let childBlocks = childBlocksOf(element, options);
		for (let i  = (terminus == "first" ? 0                  : childBlocks.length - 1);
				 i != (terminus == "first" ? childBlocks.length : -1);
			     i += (terminus == "first" ? 1                  : -1)) {
			let terminalBlock = terminalBlockOf(childBlocks[i], terminus, options);
			if (   terminalBlock
				&& isSkipped(terminalBlock, options) == false
				&& (   isNodeEmpty_metadataAware(terminalBlock) == false
					|| isNonEmpty(terminalBlock, options) == true))
				return terminalBlock;
		}
	}

	//	The element itself is a layout block (only if no strictDescent).
	if (   strictDescent == false
		&& isBlock(element, options))
		return element;

	return null;
}

/*******************************************/
/*	Returns last layout block of an element.
	(Might be the element itself, or null.)
 */
function lastBlockOf(element, options, strictDescent) {
	if (element == null)
		return null;

	if (strictDescent) {
		return terminalBlockOf(element, "last", options, true);
	} else {
		return useLayoutCache(element, "lastBlock", options, (element, options) => {
			return terminalBlockOf(element, "last", options);
		});
	}
}

/********************************************/
/*	Returns first layout block of an element.
	(Might be the element itself, or null.)
 */
function firstBlockOf(element, options, strictDescent) {
	if (element == null)
		return null;

	if (strictDescent) {
		return terminalBlockOf(element, "first", options, true);
	} else {
		return useLayoutCache(element, "firstBlock", options, (element, options) => {
			return terminalBlockOf(element, "first", options);
		});
	}
}

/***************************************************************************/
/*	Returns all “child” blocks of an element (blocks that are descended from
	the given element with no other blocks in the chain of descent; wrappers
	don’t count).
 */
function childBlocksOf(element, options) {
	if (element == null)
		return null;

	return useLayoutCache(element, "childBlocks", options, (element, options) => {
		options = processLayoutOptions(options);

		let childBlocks = Array.from(element.children);

		for (let i = 0; i < childBlocks.length; i++) {
			if (isWrapper(childBlocks[i], "downIn", options)) {
				childBlocks.splice(i, 1, ...childBlocks[i].children);
				i--;
			} else if (isBlock(childBlocks[i], options) == false) {
				childBlocks.splice(i, 1);
				i--;
			}
		}

		return childBlocks;
	});
}

/****************************************************************************/
/*	Returns true if the element is a “bare wrapper”, i.e. a <div> or <span> 
	with no classes (or, in the <div> case, possibly just the ‘block’ class); 
	false otherwise.
 */
function isBareWrapper(element) {
	return (   (   element.tagName == "DIV"
				&& (   element.className.trim() == ""
					|| element.className.trim() == "block"))
			|| (   element.tagName == "SPAN"
				&& element.className.trim() == ""));
}

/***************************************************************/
/*	Returns tag_name#id.class1.class2.class3 of a given element.
 */
function elementSummaryString(element) {
	return (  element.tagName.toLowerCase()
			+ (element.id ? `#${element.id}` : ``)
			+ (Array.from(element.classList).map(x => `.${x}`).join("")));
}

/********************************************************/
/*	Returns block spacing multiplier for the given block.
 */
function getBlockSpacingMultiplier(block, debug = false) {
	let predicateFromSelector = (selector) => {
		let parts = selector.match(/^(.+) \+ (.+)$/);
		if (parts) {
			/*	Headings do not normally count as layout blocks, but they do
				here (unless it’s a heading of a collapse section, in which case
				it still doesn’t count as a layout block).
			 */
			return (block) => (   previousBlockOf(block, {
									alsoBlockElements: [ "section:not(.collapse) > .heading" ],
									notSkipElements: [ ".float" ],
									cacheKey: "alsoBlocks_nonCollapseSectionHeadings_notSkip_floats"
								  })?.matches(parts[1])
							   && block.matches(parts[2]));
		} else {
			return (block) => block.matches(selector);
		}
	};

	let predicateMatches = (predicate, block) => {
		if (typeof predicate == "string")
			predicate = predicateFromSelector(predicate);

		if (   typeof predicate == "object"
			&& predicate instanceof Array) {
			let predicateArray = predicate;
			predicate = (block) => {
				return (predicateArray.findIndex(x => predicateMatches(x, block)) != -1);
			};
		}

		return (predicate(block) == true);
	};

	if (debug)
		console.log(block);
	for (let [ predicate, result, adjustable = true ] of GW.layout.blockSpacing) {
		if (predicateMatches(predicate, block)) {
			if (debug)
				console.log(predicate);
			let bsm = (typeof result == "function")
					  ? result(block)
					  : result;

			if (adjustable) {
				for (let [ predicate, transform ] of GW.layout.blockSpacingAdjustments)
					if (predicateMatches(predicate, block)) {
						if (debug)
							console.log(predicate);
						bsm = Math.max(0, transform(bsm, block));
					}
			}

			return bsm;
		}
	}

	return undefined;
}

/*****************************************************************************/
/*	Returns a block’s dropcap type (‘goudy’, ‘yinit’, etc.), or null if none.
 */
function dropcapTypeOf(block) {
	if (block == null)
		return null;

	return Array.from(block.classList).find(cssClass => /^dropcaps?-/.test(cssClass))?.replace(/^dropcaps-/, "dropcap-")?.slice("dropcap-".length);
}

/******************************************************************************/
/*	Adds a dropcap class to a block. Dropcaps may be ‘kanzlei’, ‘de-zs’, etc.
	(See default.css for the list.)
 */
function addDropcapClassTo(block, dropcapType) {
	if (block.classList.contains("force-dropcap"))
		return;

	stripDropcapClassesFrom(block);

	block.classList.add(`dropcap-${dropcapType}`);
}

/*************************************/
/*	Strip dropcap classes from block.
 */
function stripDropcapClassesFrom(block) {
	if (block.classList.contains("force-dropcap"))
		return;

	block.classList.remove(...(Array.from(block.classList).filter(className => className.startsWith("dropcap-"))));
}

/**************************************************************************/
/*	Like paragraphizeTextNodesOfElement, but retains elements with metadata
	(an ID, non-layout classes, or any data attributes), as well as links,
	<br> elements, and lists.
 */
function paragraphizeTextNodesOfElementRetainingMetadata(element) {
	paragraphizeTextNodesOfElement(element, {
		excludeSelector: [
			".graf-content-not"		
			].join(", "),
		nodeOmissionOptions: {
			alsoExcludePredicate: GW.layout.emptyNodeExclusionPredicate,
			alsoExcludeSelector: "a, br, ul, ol", 
			excludeIdentifiedElements: true
		}
	});
}

/****************************************************************************/
/*	Given a set of nodes all of which are children of the same parent node, 
	and conditional on the node before the first node of the set being a text
	node that ends in a left parenthesis and the node after the last node of 
	the set being a text node that begins with a right parenthesis, wraps the
	nodes in the set, plus the preceding and following nodes, in a <span>
	with the class `parenthesized-set` plus any other classes given by an 
	optional class string.

	If the conditions are not met, does nothing.
 */
function wrapParenthesizedNodes(className = null, ...args) {
	let parentNode = args.first.parentNode;
	if (parentNode == null)
		return;

	for (let node of args)
		if (node.parentNode != parentNode)
			return;

	if (   args.first.previousSibling?.nodeType == Node.TEXT_NODE
		&& args.first.previousSibling.nodeValue.endsWith("(")
		&& args.last.nextSibling?.nodeType == Node.TEXT_NODE
		&& args.last.nextSibling.nodeValue.startsWith(")")) {
		args.first.previousSibling.nodeValue = args.first.previousSibling.nodeValue.slice(0, -1);
		args.last.nextSibling.nodeValue = args.last.nextSibling.nodeValue.slice(1);
		let nextNode = args.last.nextSibling;
		let wrapper = newElement("SPAN", { class: `parenthesized-set${(className ? (" " + className) : "")}` });
		wrapper.append(document.createTextNode("("), ...args, document.createTextNode(")"));
		parentNode.insertBefore(wrapper, nextNode);
	}
}

/*****************************************************************************/
/*	Like isNodeEmpty, but does not count elements with metadata as being empty
	(i.e., if they have an ID, or non-layout classes, or any data attributes).
 */
function isNodeEmpty_metadataAware(node) {
	return isNodeEmpty(node, {
		alsoExcludePredicate: GW.layout.emptyNodeExclusionPredicate,
		excludeIdentifiedElements: true
	});
}


/*********************/
/* LAYOUT PROCESSORS */
/*********************/

/*******************************************************************************/
/*	Run given callback on given container immediately and also at any later
	time when block layout classes are updated in that container (e.g., <main>).
 */
function processContainerNowAndAfterBlockLayout(container, callback) {
	//	Run immediately...
	callback(container);

	//	... and also add event listener for if block layout classes are updated.
	GW.notificationCenter.addHandlerForEvent("Layout.layoutProcessorDidComplete", (layoutEventInfo) => {
		callback(container);
	}, {
		condition: (layoutEventInfo) => (   layoutEventInfo.container == container
										 && layoutEventInfo.processorName == "applyBlockLayoutClassesInContainer")
	});
}

/*******************************************************************************/
/*	Apply block layout classes to appropriate elements in given block container.
 */
addLayoutProcessor("applyBlockLayoutClassesInContainer", (blockContainer) => {
    GWLog("applyBlockLayoutClassesInContainer", "layout.js", 2);

	let containingDocument = blockContainer.getRootNode();

	//	Designate headings.
	blockContainer.querySelectorAll(range(1, 6).map(x => `h${x}`).join(", ")).forEach(heading => {
		if (heading.closest(GW.layout.blockLayoutExclusionSelector))
			return;

		heading.classList.add("heading");
	});

	//	Designate floats (on non-mobile layouts).
	let floatClasses = [
		".float-left",
		".float-right"
	];
	if (GW.mediaQueries.mobileWidth.matches == false) {
		blockContainer.querySelectorAll(floatClasses.join(", ")).forEach(floatBlock => {
			if (floatBlock.closest(GW.layout.blockLayoutExclusionSelector))
				return;

			floatBlock.classList.add("float");
		});
	} else {
		blockContainer.querySelectorAll(floatClasses.join(", ")).forEach(floatBlock => {
			if (floatBlock.closest(GW.layout.blockLayoutExclusionSelector))
				return;

			floatBlock.classList.remove("float");
		});
	}

	//	Designate lists.
	blockContainer.querySelectorAll([
		"ul",
		"ol"
	].join(", ")).forEach(list => {
		list.classList.add("list");
	});

	//	Designate float-containing lists.
	blockContainer.querySelectorAll(".markdownBody li .float").forEach(floatBlock => {
		if (floatBlock.closest(GW.layout.blockLayoutExclusionSelector))
			return;

		let options = {
			alsoBlockContainers: [ ".list" ],
			cacheKey: "alsoBlockContainers_lists"
		};
		let ancestorBlockContainer = blockContainerOf(floatBlock, options);
		while (ancestorBlockContainer?.matches(".list")) {
			ancestorBlockContainer.classList.add("has-floats");
			ancestorBlockContainer = blockContainerOf(ancestorBlockContainer, options);
		}
	});

	//	Designate “big lists”.
	/*	If any of a list’s list items have multiple non-list children, then
		it is a “big list” (with consequences for block spacing).
	 */
	let listItemChildBlocksOptions = {
		notWrapperElements: [ ".list" ],
		cacheKey: "notWrappers_lists"
	};
	let isBigList = (list) => {
		if (list.matches(".list") != true)
			return false;

		for (let listItem of list.children) {
			if (childBlocksOf(listItem, listItemChildBlocksOptions).filter(x => x.matches(".list") != true).length > 1)
				return true;
		}

		return false;
	};
	blockContainer.querySelectorAll(".list").forEach(list => {
		if (list.closest(GW.layout.blockLayoutExclusionSelector))
			return;

		let bigList = isBigList(list);

		/*	If this is a sub-list, and any other sub-lists on the same level as
			this one are “big lists”, then this is also a “big list” (because
			the designation of “bigness” is applied to *list levels within a
			list tree*, not to individual lists).
		 */
		 let ancestorBlockContainer = blockContainerOf(list, {
		 	alsoBlockContainers: [ "li" ],
		 	cacheKey: "alsoBlockContainers_listItems"
		 });
		 if (ancestorBlockContainer?.matches("li")) {
		 	for (let listItem of ancestorBlockContainer.parentElement.children) {
				if (childBlocksOf(listItem, listItemChildBlocksOptions).findIndex(x => isBigList(x)) != -1) {
					bigList = true;
					break;
				}
		 	}
		 }

		list.classList.toggle("big-list", bigList);
	});

	//	Disable triptychs on mobile layouts.
	blockContainer.querySelectorAll(".triptych").forEach(triptych => {
		if (triptych.closest(GW.layout.blockLayoutExclusionSelector))
			return;

		/*	Why “aptych”? Because on mobile it is laid out in one column
			instead of three, making it “un-folded”:
			https://old.reddit.com/r/AncientGreek/comments/ypts2o/polyptychs_help_with_a_word/
		 */
		triptych.classList.toggle("aptych", GW.mediaQueries.mobileWidth.matches);
	});

	//	Apply special block sequence classes.
	blockContainer.querySelectorAll(GW.layout.blockElements.join(", ")).forEach(block => {
		if (block.closest(GW.layout.blockLayoutExclusionSelector))
			return;

		/*	Designate blocks preceded by nothing (not counting floats and other
			elements that do not participate in block flow) in their block
			container (the .first-block class).

			Headings do not normally count as layout blocks, but they do here
			(unless it’s a heading of a collapse section, in which case it still
			 doesn’t count as a layout block).
		 */
		block.classList.toggle("first-block", previousBlockOf(block, {
			alsoBlockElements: [ "section:not(.collapse) > .heading" ],
			cacheKey: "alsoBlocks_nonCollapseSectionHeadings"
		}) == null);

		//	Designate blocks in lists (the .in-list class).
		block.classList.toggle("in-list", blockContainerOf(block, {
			alsoBlockContainers: [ "li" ],
			cacheKey: "alsoBlockContainers_listItems"
		})?.matches("li") == true);

		//	Apply special paragraph classes.
		if (block.matches("p") == true) {
			//	Empty paragraphs (the .empty-graf class; not displayed).
			let emptyGraf = isNodeEmpty_metadataAware(block);
			block.classList.toggle("empty-graf", emptyGraf);
			if (emptyGraf)
				return;

			/*	Paragraphs not preceded directly by other paragraphs
				(not in lists) (the .first-graf class).
			 */
			let firstGraf = false;
			let previousBlockSelector = [
				":not(p)",
				".text-center",
				".section-metadata",
				".margin-notes-block",
				".page-description-annotation",
				".data-field",
				".admonition-title > p"
			].join(", ");
			let strictPreviousBlock = previousBlockOf(block, {
				alsoBlockElements: [ ".list" ],
				notWrapperElements: [ "li", ".list" ],
				notHalfWrapperElements: [ "section" ],
				cacheKey: "alsoBlocks_lists_notWrappers_listsAndListItems_notHalfWrappers_sections"
			});
			if (   strictPreviousBlock == null
				|| strictPreviousBlock.matches(previousBlockSelector) == true)
				firstGraf = true;
			//	Centered text must be vertically spaced in all cases.
			if (block.matches(".text-center"))
				firstGraf = true;
			block.classList.toggle("first-graf", firstGraf);

			/*	Colon-terminated paragraphs followed by lists
				(the .list-heading class).
			 */
			let listHeading = false;
			let strictNextBlock = nextBlockOf(block, {
				alsoBlockElements: [ ".list" ],
				notWrapperElements: [ ".list" ],
				cacheKey: "alsoBlocks_lists_notWrappers_lists"
			});
			if (   strictNextBlock?.matches(".list, .columns")
				&& block.textContent.trim().endsWith(":")
				&& block.textContent.includes(".") == false)
				listHeading = true;
			block.classList.toggle("list-heading", listHeading);

			/*	Introductory paragraphs of documents or self-contained parts
				of documents (the .intro-graf class).
			 */
			let introGraf = false;
			if (   block.matches(".text-center, .margin-notes-block, .data-field") != true
				&& block.matches(".in-list") != true
				&& block.closest("#footer, figcaption, table") == null
				&& block.firstElementChild?.matches("span.smallcaps") != true) {
				let isFirstWithin = (block, containerSelector, options) => {
					return (   blockContainerOf(block, options)?.matches(containerSelector) == true
							&& previousBlockOf(block, options) == null);
				}
				let options = {
					alsoSkipElements: [ ".epigraph", ".margin-notes-block" ],
					alsoBlockContainers: [ "li" ],
					cacheKey: "alsoSkip_epigraphs_alsoBlockContainers_listItems"
				};

				let previousBlock = previousBlockOf(block, options);
				if (   isFirstWithin(block, "#markdownBody", options)
					|| (   isFirstWithin(block, "section", options)
						&& isFirstWithin(blockContainerOf(block), "#markdownBody"))
					|| previousBlock?.matches(".abstract blockquote, #page-metadata"))
					introGraf = true;

				//	The .intro-graf class also implies .first-graf.
				if (introGraf)
					block.classList.add("first-graf");

				/*  Add dropcap class. This could be set globally, or
					overridden by a .abstract; the latter could be
					`dropcap-not` (which nullifies any page-global dropcap
					class for the given block).
				 */
				let dropcapType = null;
				if (introGraf) {
					dropcapType = (previousBlock?.matches(".abstract blockquote")
								   ? dropcapTypeOf(previousBlock)
								   : null) ?? dropcapTypeOf(containingDocument.body);
				} else {
					let dropcapContainerOptions = {
						alsoBlockContainers: [ "div[class*='dropcap-']" ],
						cacheKey: "alsoBlockContainers_dropcapDivs"
					};
					let dropcapContainer = blockContainerOf(block, dropcapContainerOptions);
					if (   dropcapContainer?.matches("div")
						&& previousBlockOf(block, dropcapContainerOptions) == null) {
						dropcapType = dropcapTypeOf(dropcapContainer);
						block.classList.add("first-graf");
					}
				}
				if (   dropcapType 
					&& dropcapType != "not") {
					addDropcapClassTo(block, dropcapType);
				} else {
					stripDropcapClassesFrom(block);

					/*	If resetDropcapInBlock() has not been defined, then it 
						is also guaranteed to be unnecessary, as that means that
						the functions that *add* dropcaps to blocks have also
						not been defined and thus cannot have been called, so
						nothing needs to be reset.
					 */
					if (window.resetDropcapInBlock)
						resetDropcapInBlock(block);
				}
			}
			block.classList.toggle("intro-graf", introGraf);

			let footnoteBackLink = block.querySelector(".footnote-back");
			if (isOnlyChild(footnoteBackLink))
				block.classList.add("footnote-back-block");
		}
	});
});

/**********************************************/
/*	Apply block spacing in the given container.
 */
addLayoutProcessor("applyBlockSpacingInContainer", (blockContainer) => {
    GWLog("applyBlockSpacingInContainer", "layout.js", 2);

	//	Remove block spacing metadata from what shouldn’t have it.
	blockContainer.querySelectorAll(".block").forEach(block => {
		if (   block.matches(GW.layout.blockElements.join(", ")) == false
			|| block.closest(GW.layout.blockLayoutExclusionSelector) != null) {
			block.classList.remove("block");
			block.style.removeProperty("--bsm");
		}
	});

	//	Apply block spacing.
	blockContainer.querySelectorAll(GW.layout.blockElements.join(", ")).forEach(block => {
		if (block.closest(GW.layout.blockLayoutExclusionSelector))
			return;

		let bsm = getBlockSpacingMultiplier(block);
		if (bsm != undefined) {
			block.classList.add("block");
			block.style.setProperty("--bsm", `${bsm}`);
		} else {
			block.classList.remove("block");
			block.style.removeProperty("--bsm");
		}
	});

	//	Triptychs require special treatment.
	blockContainer.querySelectorAll(".triptych").forEach(triptych => {
		if (triptych.closest(GW.layout.blockLayoutExclusionSelector))
			return;

		if (triptych.classList.contains("aptych")) {
			triptych.style.removeProperty("--bsm");
			triptych.classList.remove("block");
		} else {
			//	Set triptych BSM to that of first panel.
			let triptychBSM = parseInt(triptych.firstElementChild.style.getPropertyValue("--bsm"));;

			//	Set all panels’ BSM to 0.
			for (let panel of triptych.children)
				panel.style.setProperty("--bsm", 0);

			triptych.style.setProperty("--bsm", triptychBSM);
			triptych.classList.add("block");
		}
	});

	//	Lists require special treatment.
	blockContainer.querySelectorAll("li:not(.footnote)").forEach(listItem => {
		if (listItem.closest(GW.layout.blockLayoutExclusionSelector))
			return;

		let firstBlockWithin = firstBlockOf(listItem);
		let bsm = firstBlockWithin?.style.getPropertyValue("--bsm");

		//	Apply list item BSM modifier.
		if (   bsm > ""
			&& listItem.dataset.bsmMod > "")
			bsm = "" + (parseInt(bsm) + parseInt(listItem.dataset.bsmMod));

		//	Apply BSM.
		if (bsm > "") {
			/*	We must propagate the spacing of the first block within the
				list item to the list item itself.
			 */
			listItem.classList.add("block");
			listItem.style.setProperty("--bsm", bsm);

			//	Avoid double-counting.
			if (   firstBlockWithin != null
				&& firstBlockWithin != listItem)
				firstBlockWithin.style.setProperty("--bsm", 0);
		}

		//	Delete now-extraneous data attribute.
		if (listItem.dataset.bsmMod)
			delete listItem.dataset.bsmMod;
	});

	//	Floats require special treatment on non-mobile layouts.
	blockContainer.querySelectorAll(".float").forEach(floatBlock => {
		if (floatBlock.closest(GW.layout.blockLayoutExclusionSelector))
			return;

		//	Floating elements take on the top margin of the next block.
		let nextBlock = nextBlockOf(floatBlock, {
			alsoBlockElements: [ "li" ],
			cacheKey: "alsoBlocks_listItems"
		});
		let nextBlockBSM = nextBlock?.style?.getPropertyValue("--bsm");
		if (nextBlockBSM) {
			/*	If the next block (in the strict sense, i.e. not counting list
				items!) is a paragraph or non-collapse section heading, then 
				adjust margin.
			 */
			let strictNextBlock = nextBlockOf(floatBlock, {
				alsoBlockElements: [ "section:not(.collapse) > .heading" ],
				cacheKey: "alsoBlocks_nonCollapseSectionHeadings"
			});
			if (strictNextBlock?.matches("p, .heading"))
				nextBlockBSM = "" + (parseInt(nextBlockBSM) + 2);

			floatBlock.style.setProperty("--bsm", nextBlockBSM);
		}
	});
});

/****************************************************************************/
/*	Apply block layout classes to the main document, prior to rewrites. (This
	is necessary in browsers that delay MutationObserver firing until after
	DOMContentLoaded.)
 */
addContentLoadHandler(GW.contentLoadHandlers.applyBlockLayoutClassesInMainDocument = (eventInfo) => {
    GWLog("applyBlockLayoutClassesInMainDocument", "layout.js", 1);

	eventInfo.container.querySelectorAll(".markdownBody").forEach(blockContainer => {
		GW.layout.applyBlockLayoutClassesInContainer(blockContainer);
	});
}, "<rewrite", (info) => (info.container == document.main));

/**************************************************************************/
/*	Fire “page layout complete event” on the next animation frame after the 
	end of all rewrites triggered directly by the DOMContentLoaded event.
 */
addContentInjectHandler(GW.contentInjectHandlers.completePageLayout = (eventInfo) => {
    GWLog("completePageLayout", "layout.js", 1);

	requestAnimationFrame(() => {
		GW.layout.initialPageLayoutComplete = true;
		GW.notificationCenter.fireEvent("Layout.initialPageLayoutDidComplete");
	});
}, "<eventListeners", (info) => (info.container == document.main));

/****************************************************************************/
/*	Apply block layout classes to a document fragment, to make them available
	to any other load handlers (rewrite functions).
 */
addContentLoadHandler(GW.contentLoadHandlers.applyBlockLayoutClassesInDocumentFragment = (eventInfo) => {
    GWLog("applyBlockLayoutClassesInDocumentFragment", "layout.js", 1);

	GW.layout.applyBlockLayoutClassesInContainer(eventInfo.container);
}, "<rewrite", (info) => (info.container instanceof DocumentFragment));

/*********************************************************************/
/*	Apply block spacing in collapse block when collapse state changes.
 */
GW.notificationCenter.addHandlerForEvent("Collapse.collapseStateDidChange", (eventInfo) => {
	GWLog("applyBlockSpacingInCollapseBlockOnStateChange", "layout.js", 2);

	GW.layout.applyBlockSpacingInContainer(eventInfo.collapseBlock);
});

/*************************************************/
/*	Activate dynamic layout for the main document.
 */
doWhenMainExists(() => {
	startDynamicLayoutInContainer(document.main);

	//	Add listener to redo layout when orientation changes.
	doWhenMatchMedia(GW.mediaQueries.portraitOrientation, "Layout.updateLayoutWhenOrientationChanges", (mediaQuery) => {
		document.main.querySelectorAll(".markdownBody").forEach(blockContainer => {
			GW.layout.layoutProcessors.forEach(processorSpec => {
				applyLayoutProcessorToBlockContainer(processorSpec, blockContainer, document.main);
			});
		});
	});
});

/******************************************************************************/
/*  Run the given function immediately if initial page layout has completed, or
	add an event handler to run it as soon as initial page layout completes.
 */
function doWhenPageLayoutComplete(f) {
    if (GW.layout.initialPageLayoutComplete == true)
        f();
    else
        GW.notificationCenter.addHandlerForEvent("Layout.initialPageLayoutDidComplete", (info) => {
            f();
        }, { once: true });
}


/**********************/
/* REWRITE PROCESSORS */
/**********************/
/*	“Non-block layout” a.k.a. “rewrite” processors. Like rewrites, but faster.
 */

/*****************************************************************************/
/*	Run rewrite processor in already-loaded main page content, and add rewrite
	processor to process any subsequently loaded content.
 */
function processMainContentAndAddRewriteProcessor(processorName, processor) {
	processor(document.main);
	addLayoutProcessor(processorName, processor, { blockLayout: false });
}

/**********************************************/
/*	Enable inline icons in the given container.
 */
addLayoutProcessor("processInlineIconsInContainer", (blockContainer) => {
    GWLog("processInlineIconsInContainer", "layout.js", 2);

	blockContainer.querySelectorAll("span[class*='icon-']").forEach(inlineIcon => {
		if (inlineIcon.classList.contains("icon-not"))
			return;

		//	Some icons are special.
		let specialIconsSelector = [
			".icon-single-white-star-on-black-circle"
		].join(", ");
		if (inlineIcon.matches(specialIconsSelector))
			inlineIcon.classList.add("icon-special");

		/*	“Special” icons will have their `--icon-url` CSS variable set
			elsewhere (in other runtime code, or in CSS).
		 */
		if (inlineIcon.classList.contains("icon-special") == false) {
			let iconName = Array.from(inlineIcon.classList).find(className => className.startsWith("icon-"))?.slice("icon-".length);
			if (iconName == null)
				return;

			inlineIcon.style.setProperty("--icon-url", `url('/static/img/icon/icons.svg#${iconName}')`);
		}

		inlineIcon.classList.add("inline-icon", "dark-mode-invert");
	});
}, { blockLayout: false });

/**************************************************************************/
/*  Enable special list icons for list items that contain recently modified
    links.
 */
addLayoutProcessor("enableRecentlyModifiedLinkListIcons", (blockContainer) => {
    GWLog("enableRecentlyModifiedLinkIcons", "layout.js", 2);

    blockContainer.querySelectorAll("li a.link-modified-recently").forEach(link => {
        let inList = false;
        let containingGraf = link.closest("p");
        if (containingGraf?.matches("li > p:only-of-type")) {
            inList = true;
        } else if (containingGraf?.matches(".data-field")) {
            /*  This handles cases such as those where we’re transcluding an
                annotation into a list, and each annotation has its own list
                item (thus the .link-modified-recently class would be on the
                title-link of the annotation).
             */
            let ancestor = containingGraf.parentElement;
            while (   ancestor.matches("li") == false
                   && (   ancestor.parentElement.children.length == 1
                       || (   ancestor.parentElement.children.length == 2
                           && ancestor.matches(".include-wrapper"))))
                   ancestor = ancestor.parentElement;
            if (ancestor.matches("li"))
                inList = true;
        }
        if (inList) {
            link.closest("li").classList.add("link-modified-recently-list-item");
            link.classList.add("in-list");

			//	Remove existing icon, if any.
			if (link.classList.contains("has-recently-modified-icon"))
	            removeRecentlyModifiedIconFromLink(link);
        }
    });
}, { blockLayout: false });

/*************************************************************/
/*	Add certain style classes to certain lists and list items.
 */
addLayoutProcessor("designateListStyles", (blockContainer) => {
    GWLog("designateListStyles", "layout.js", 2);

	blockContainer.querySelectorAll("ul > li").forEach(listItem => {
		if (listItem.closest(".TOC") == null)
			listItem.classList.add("dark-mode-invert");
	});
}, { blockLayout: false });

/*************************************************/
/*	Add certain style classes to horizontal rules.
 */
addLayoutProcessor("designateHorizontalRuleStyles", (blockContainer) => {
    GWLog("designateHorizontalRuleStyles", "layout.js", 2);

	blockContainer.querySelectorAll("hr").forEach(hr => {
		hr.classList.add("dark-mode-invert");
	});
}, { blockLayout: false });


/***************************/
/* ADDITIONAL EARLY LAYOUT */
/***************************/

/*************************************************/
/*	Placeholder page for ID-based content loading.
 */
doWhenMainExists(() => {
	if (location.pathname.startsWith("/ref/")) {
		document.querySelectorAll("title, header h1").forEach(element => {
			element.innerHTML = "";
		});
	}
});

/**********************************/
/*	Designate /blog/ pages as such.
 */
doWhenBodyExists(() => {
	if (location.pathname.startsWith("/blog/"))
		document.body.classList.add("blog-page");
});

/**************************************************************************/
/*  Update visibility of a TOC. (Hide if no entries; if main page TOC, also 
	hide if one entry.)
 */
function updateTOCVisibility(TOC) {
	if (TOC == null)
		return;

    let numEntries = TOC.querySelectorAll("li").length;
    if (   (   TOC.id == "TOC"
            && numEntries <= 1)
        || numEntries == 0) {
        TOC.classList.toggle("hidden", true);
    } else {
        TOC.classList.toggle("hidden", false);
    }
}

doWhenElementExists(updateTOCVisibility, "#TOC");
/*	This code is part of dark-mode.js by Said Achmiz.
	See the file `dark-mode.js` for license and more information.
 */

/*	Dark mode: before anything else loads, check browser localStorage for dark 
	mode preference and immediately toggle sets of CSS color variables/classes 
	to avoid any ‘flash of white’ or delayed loading. Note: CSS falls back to 
	the media-query browser/OS variable preference, so still works if JS is 
	blocked! (The JS is only necessary for the theme switcher widget allowing 
	‘force light’/‘force dark’ options. If users block JS, set the dark mode 
	preference, and are unhappy when they get dark mode, well, they made their 
	bed and must lie in it.)
 */

DarkMode = {
	/*	Dark mode elements, switched on or off (or to match system setting) by
		changing the value of their ‘media’ attribute.
	 */
	switchedElementsSelector: [
		"#inlined-styles-colors-dark",
		"#favicon-dark",
		"#favicon-apple-touch-dark"
	].join(", "),

	/*	The ‘media’ attribute values for dark mode elements, for each mode.
	 */
	mediaAttributeValues: {
		"auto":  "all and (prefers-color-scheme: dark)",
		"dark":  "all",
		"light": "not all"
	},

	/*	Overridable default mode.
	 */
	defaultMode: "auto",

    /*  Returns current (saved) mode (light, dark, or auto).
     */
    currentMode: () => {
        return (localStorage.getItem("dark-mode-setting") ?? DarkMode.defaultMode);
    },

	//	Called by: DarkMode.setMode
	saveMode: (newMode = DarkMode.currentMode()) => {
		GWLog("DarkMode.saveMode", "dark-mode-initial.js", 1);

		if (newMode == DarkMode.defaultMode)
			localStorage.removeItem("dark-mode-setting");
		else
			localStorage.setItem("dark-mode-setting", newMode);
	},

	/*  Set specified color mode (auto, light, dark).

		Called by: this file (immediately upon load)
		Called by: DarkMode.modeSelectButtonClicked (dark-mode.js)
	 */
	setMode: (selectedMode = DarkMode.currentMode()) => {
		GWLog("DarkMode.setMode", "dark-mode-initial.js", 1);

		//	Remember previous mode.
		let previousMode = DarkMode.currentMode();

		//	Save the new setting.
		DarkMode.saveMode(selectedMode);

		//	Set ‘media’ attribute of dark mode elements to match requested mode.
		document.querySelectorAll(DarkMode.switchedElementsSelector).forEach(element => {
			element.media = DarkMode.mediaAttributeValues[selectedMode];
		});

		//	Fire event.
		GW.notificationCenter.fireEvent("DarkMode.didSetMode", { previousMode: previousMode });
	},

	/*	Returns currently active color mode (light or dark).
		Based on saved selector mode, plus system setting (if selected mode is
		‘auto’).
	 */
	computedMode: (modeSetting = DarkMode.currentMode(), systemDarkModeActive = GW.mediaQueries.systemDarkModeActive.matches) => {
		return ((   modeSetting == "dark" 
				|| (   modeSetting == "auto" 
					&& systemDarkModeActive == true))
				? "dark"
				: "light");
	}
};

//	Activate saved mode.
DarkMode.setMode();

//	Set up mode change events.
GW.notificationCenter.addHandlerForEvent("DarkMode.didSetMode", (info) => {
	let previousComputedMode = DarkMode.computedMode(info.previousMode, GW.mediaQueries.systemDarkModeActive.matches)
	if (   previousComputedMode != null
		&& previousComputedMode != DarkMode.computedMode())	
		GW.notificationCenter.fireEvent("DarkMode.computedModeDidChange");
});
doWhenMatchMedia(GW.mediaQueries.systemDarkModeActive, "DarkMode.fireComputedModeDidChangeEventForSystemDarkModeChange", (mediaQuery) => {
	let previousComputedMode = DarkMode.computedMode(DarkMode.currentMode(), !(mediaQuery.matches));
	if (previousComputedMode != DarkMode.computedMode())
		GW.notificationCenter.fireEvent("DarkMode.computedModeDidChange");
});
ReaderMode = {
    active: false,

    readerModeTitleNote: " (reader mode)",

	/*	Overridable default mode.
	 */
	defaultMode: "auto",

    /*  Returns current (saved) mode (on, off, or auto).
     */
    currentMode: () => {
        return (localStorage.getItem("reader-mode-setting") ?? ReaderMode.defaultMode);
    },

    /*  Returns true if reader mode is set to be enabled for the current page,
        false otherwise.
     */
    enabled: () => {
        let currentMode = ReaderMode.currentMode();
        return (   currentMode == "on"
                || (   currentMode == "auto"
                    && document.body.classList.contains("reader-mode")))
    },

    /*  Masks links and hide other elements, as appropriate. This will hide
        linkicons and pop-frame indicators, and will thus cause reflow.
     */
    //  Called by: ReaderMode.setMode
    activate: () => {
        GWLog("ReaderMode.activate", "reader-mode-initial.js", 1);

        //  Add body classes.
        document.body.classList.add("reader-mode-active", "masked-links-hidden");

        //  Update document title.
        document.title += ReaderMode.readerModeTitleNote;
    }
};

//  Activate saved mode, once the <body> element is loaded (and classes known).
doWhenBodyExists(() => {
	//  Activate (if needed).
	if (ReaderMode.enabled() == true)
		ReaderMode.activate();
});

//	Once .setMode() is available, set the mode.
GW.notificationCenter.addHandlerForEvent("ReaderMode.didLoad", (eventInfo) => {
	ReaderMode.setMode();
}, { once: true });
GW.assetVersions = {
	"/static/img/icon/icons.svg": "1740442466",
	"/static/img/logo/christmas/dark/logo-christmas-dark-1-small-1x.png": "1734481578",
	"/static/img/logo/christmas/dark/logo-christmas-dark-1-small-2x.png": "1734481578",
	"/static/img/logo/christmas/dark/logo-christmas-dark-1-small-3x.png": "1734481578",
	"/static/img/logo/christmas/dark/logo-christmas-dark-2-small-1x.png": "1734481578",
	"/static/img/logo/christmas/dark/logo-christmas-dark-2-small-2x.png": "1734481578",
	"/static/img/logo/christmas/dark/logo-christmas-dark-2-small-3x.png": "1734481578",
	"/static/img/logo/christmas/dark/logo-christmas-dark-3-small-1x.png": "1734481578",
	"/static/img/logo/christmas/dark/logo-christmas-dark-3-small-2x.png": "1734481578",
	"/static/img/logo/christmas/dark/logo-christmas-dark-3-small-3x.png": "1734481578",
	"/static/img/logo/christmas/dark/logo-christmas-dark-4-small-1x.png": "1734481578",
	"/static/img/logo/christmas/dark/logo-christmas-dark-4-small-2x.png": "1734481578",
	"/static/img/logo/christmas/dark/logo-christmas-dark-4-small-3x.png": "1734481578",
	"/static/img/logo/christmas/dark/logo-christmas-dark-5-small-1x.png": "1734481578",
	"/static/img/logo/christmas/dark/logo-christmas-dark-5-small-2x.png": "1734481578",
	"/static/img/logo/christmas/dark/logo-christmas-dark-5-small-3x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-1-small-1x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-1-small-2x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-1-small-3x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-2-small-1x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-2-small-2x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-2-small-3x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-3-small-1x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-3-small-2x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-3-small-3x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-4-small-1x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-4-small-2x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-4-small-3x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-5-small-1x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-5-small-2x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-5-small-3x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-6-small-1x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-6-small-2x.png": "1734481578",
	"/static/img/logo/christmas/light/logo-christmas-light-6-small-3x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-1-small-1x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-1-small-2x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-1-small-3x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-10-small-1x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-10-small-2x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-10-small-3x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-11-small-1x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-11-small-2x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-11-small-3x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-2-small-1x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-2-small-2x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-2-small-3x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-3-small-1x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-3-small-2x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-3-small-3x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-4-small-1x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-4-small-2x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-4-small-3x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-5-small-1x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-5-small-2x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-5-small-3x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-6-small-1x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-6-small-2x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-6-small-3x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-7-small-1x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-7-small-2x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-7-small-3x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-8-small-1x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-8-small-2x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-8-small-3x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-9-small-1x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-9-small-2x.png": "1734481578",
	"/static/img/logo/halloween/dark/logo-halloween-dark-9-small-3x.png": "1734481578",
	"/static/font/dropcap/dropcat/dark/B-dark-1-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/B-dark-1-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/B-dark-2-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/B-dark-2-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/B-dark-3-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/B-dark-3-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/B-dark-4-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/B-dark-4-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/C-dark-1-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/C-dark-1-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/C-dark-2-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/C-dark-2-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/C-dark-3-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/C-dark-3-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-1-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-1-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-2-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-2-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-3-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-3-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-4-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-4-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-5-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-5-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-6-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-6-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-7-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-7-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/I-dark-1-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/I-dark-1-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-1-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-1-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-10-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-10-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-11-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-11-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-12-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-12-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-13-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-13-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-2-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-2-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-3-small-1x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-3-small-2x.png": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-4-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-4-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-5-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-5-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-6-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-6-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-7-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-7-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-8-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-8-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-9-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-9-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/T-dark-1-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/T-dark-1-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/T-dark-2-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/T-dark-2-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/T-dark-3-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/T-dark-3-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/T-dark-4-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/dark/T-dark-4-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-1-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-1-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-2-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-2-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-3-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-3-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-4-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-4-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-5-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-5-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-6-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-6-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-7-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-7-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/C-light-1-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/C-light-1-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/C-light-2-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/C-light-2-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/C-light-3-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/C-light-3-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/C-light-4-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/C-light-4-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/C-light-5-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/C-light-5-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-1-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-1-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-10-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-10-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-2-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-2-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-3-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-3-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-4-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-4-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-5-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-5-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-6-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-6-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-7-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-7-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-8-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-8-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-9-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-9-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/I-light-1-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/I-light-1-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-1-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-1-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-10-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-10-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-11-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-11-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-12-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-12-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-2-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-2-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-3-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-3-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-4-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-4-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-5-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-5-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-6-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-6-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-7-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-7-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-8-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-8-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-9-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-9-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-1-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-1-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-2-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-2-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-3-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-3-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-4-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-4-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-5-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-5-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-6-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-6-small-2x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-7-small-1x.png": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-7-small-2x.png": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-01-small-1x.png": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-01-small-2x.png": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-02-small-1x.png": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-02-small-2x.png": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-03-small-1x.png": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-03-small-2x.png": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-04-small-1x.png": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-04-small-2x.png": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-05-small-1x.png": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-05-small-2x.png": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-01-small-1x.png": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-01-small-2x.png": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-02-small-1x.png": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-02-small-2x.png": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-03-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-03-small-2x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-04-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-04-small-2x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-05-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-05-small-2x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-01-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-01-small-2x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-02-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-02-small-2x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-03-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-03-small-2x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-04-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-04-small-2x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-05-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-05-small-2x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-01-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-01-small-2x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-02-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-02-small-2x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-03-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-03-small-2x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-04-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-04-small-2x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-05-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-05-small-2x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/T-light-01-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/T-light-01-small-2x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/T-light-02-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/T-light-02-small-2x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/T-light-03-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/T-light-03-small-2x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/T-light-04-small-1x.png": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/T-light-04-small-2x.png": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-05-small-1x.png": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-05-small-2x.png": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-01-small-1x.png": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-01-small-2x.png": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-02-small-1x.png": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-02-small-2x.png": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-03-small-1x.png": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-03-small-2x.png": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-04-small-1x.png": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-04-small-2x.png": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-05-small-1x.png": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-05-small-2x.png": "1734481576",
	"/static/font/dropcap/ninit/A-floral-2.svg": "1734481577",
	"/static/font/dropcap/ninit/A-floral-light-1.svg": "1734481577",
	"/static/font/dropcap/dropcat/dark/B-dark-1.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/B-dark-2.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/B-dark-3.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/B-dark-4.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/C-dark-1.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/C-dark-2.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/C-dark-3.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-1.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-2.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-3.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-4.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-5.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-6.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/G-dark-7.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/I-dark-1.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-1.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-10.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-11.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-12.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-13.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-2.svg": "1734481573",
	"/static/font/dropcap/dropcat/dark/Q-dark-3.svg": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-4.svg": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-5.svg": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-6.svg": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-7.svg": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-8.svg": "1734481574",
	"/static/font/dropcap/dropcat/dark/Q-dark-9.svg": "1734481574",
	"/static/font/dropcap/dropcat/dark/T-dark-1.svg": "1734481574",
	"/static/font/dropcap/dropcat/dark/T-dark-2.svg": "1734481574",
	"/static/font/dropcap/dropcat/dark/T-dark-3.svg": "1734481574",
	"/static/font/dropcap/dropcat/dark/T-dark-4.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-1.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-2.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-3.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-4.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-5.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-6.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/B-light-7.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/C-light-1.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/C-light-2.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/C-light-3.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/C-light-4.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/C-light-5.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-1.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-10.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-2.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-3.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-4.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-5.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-6.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-7.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-8.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/G-light-9.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/I-light-1.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-1.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-10.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-11.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-12.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-2.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-3.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-4.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-5.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-6.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-7.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-8.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/Q-light-9.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-1.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-2.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-3.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-4.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-5.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-6.svg": "1734481574",
	"/static/font/dropcap/dropcat/light/T-light-7.svg": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-01.svg": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-02.svg": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-03.svg": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-04.svg": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-05.svg": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-06.svg": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-07.svg": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-08.svg": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-09.svg": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/F-dark-10.svg": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-01.svg": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-02.svg": "1734481574",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-03.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-04.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-05.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-06.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-07.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-08.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-09.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-10.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-11.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-12.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-13.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-14.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-15.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/T-dark-16.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-01.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-02.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-03.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-04.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-05.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-06.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-07.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-08.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-09.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-10.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-11.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-12.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-13.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-14.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-15.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-16.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-17.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-18.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-19.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-20.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-21.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-22.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-23.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-24.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-25.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-26.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-27.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-28.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-29.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-30.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-31.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-32.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-33.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-34.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-35.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-36.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-37.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-38.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-39.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-40.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-41.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-42.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-43.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-44.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-45.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/dark/V-dark-46.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-01.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-02.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-03.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-04.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-05.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-06.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-07.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-08.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-09.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-10.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-11.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-12.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-13.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-14.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-15.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/F-light-16.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/T-light-01.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/T-light-02.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/T-light-03.svg": "1734481575",
	"/static/font/dropcap/gene-wolfe/light/T-light-04.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-05.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-06.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-07.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-08.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-09.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-10.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-11.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-12.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-13.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-14.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-15.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-16.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-17.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-18.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-19.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-20.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-21.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-22.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-23.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-24.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-25.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/T-light-26.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-01.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-02.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-03.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-04.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-05.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-06.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-07.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-08.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-09.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-10.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-11.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-12.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-13.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-14.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-15.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-16.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-17.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-18.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-19.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-20.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-21.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-22.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-23.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-24.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-25.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-26.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-27.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-28.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-29.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-30.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-31.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-32.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-33.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-34.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-35.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-36.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-37.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-38.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-39.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-40.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-41.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-42.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-43.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-44.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-45.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-46.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-47.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-48.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-49.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-50.svg": "1734481576",
	"/static/font/dropcap/gene-wolfe/light/V-light-51.svg": "1734481576",
	"/static/font/dropcap/ninit/light/A-circuit-light-1.svg": "1734481577",
	"/static/font/dropcap/ninit/light/A-circuit-light-2.svg": "1734481577",
	"/static/font/dropcap/ninit/light/A-mechanical-light-1.svg": "1734481577",
	"/static/font/dropcap/ninit/light/B-mechanical-light-1.svg": "1734481577",
	"/static/font/dropcap/ninit/light/C-abstract-light-1.svg": "1734481577",
	"/static/font/dropcap/ninit/light/C-abstract-light-10.svg": "1734481577",
	"/static/font/dropcap/ninit/light/C-abstract-light-2.svg": "1734481577",
	"/static/font/dropcap/ninit/light/C-abstract-light-3.svg": "1734481577",
	"/static/font/dropcap/ninit/light/C-abstract-light-4.svg": "1734481577",
	"/static/font/dropcap/ninit/light/C-abstract-light-5.svg": "1734481577",
	"/static/font/dropcap/ninit/light/C-abstract-light-6.svg": "1734481577",
	"/static/font/dropcap/ninit/light/C-abstract-light-7.svg": "1734481577",
	"/static/font/dropcap/ninit/light/C-abstract-light-8.svg": "1734481577",
	"/static/font/dropcap/ninit/light/C-abstract-light-9.svg": "1734481577",
	"/static/font/dropcap/ninit/light/C-floral-light-1.svg": "1734481577",
	"/static/font/dropcap/ninit/light/C-floral-light-2.svg": "1734481577",
	"/static/font/dropcap/ninit/light/C-floral-light-3.svg": "1734481577",
	"/static/font/dropcap/ninit/light/C-mechanical-light-1.svg": "1734481577",
	"/static/font/dropcap/ninit/light/C-mechanical-light-2.svg": "1734481577",
	"/static/font/dropcap/ninit/light/C-sciencefiction-light-1.svg": "1734481577"
};
