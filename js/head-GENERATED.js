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
	(pathname component replacing part of current URL after last slash).

	(The existing URL() constructor only handles fully qualified URL strings.)

	The optional baseURL argument allows for qualifying non-fully-qualified
	URL strings relative to a base URL other than the current page location.
 */
function URLFromString(urlString, baseURL = location) {
	if (   urlString.startsWith("http://")
		|| urlString.startsWith("https://"))
		return new URL(urlString);

	return (urlString.startsWith("/")
			? new URL(baseURL.origin + urlString)
			: new URL(baseURL.href.replace(/[^\/]*$/, urlString)));
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

/******************************************************************************/
/*  Adds an event listener to a button (or other clickable element), attaching
    it to both ‘click’ and ‘keyup’ events (for use with keyboard navigation).
    Optionally also attaches the listener to the ‘mousedown’ event, making the
    element activate on mouse down instead of mouse up.
 */
Element.prototype.addActivateEvent = function(fn, includeMouseDown) {
    let ael = this.activateEventListener = (event) => {
        if (   event.button === 0
            || event.key    === ' ')
            fn(event);
    };
    this.addEventListener("click", ael);
    this.addEventListener("keyup", ael);
    if (includeMouseDown)
        this.addEventListener("mousedown", ael);
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
        return Array.from(this.childNodes).map(node => (node.nodeValue || node.outerHTML)).join("");
    }
});

/**************************************************/
/*	The obvious equivalent of Element’s .innerHTML.
 */
Object.defineProperty(DocumentFragment.prototype, "innerHTML", {
    get() {
        return Array.from(this.childNodes).map(node => (node.nodeValue || node.outerHTML)).join("");
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
function newElement(tagName, attributes = { }, properties = { }) {
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
        a DocumentFragment containing the given DocumentFragment’s children

    a string
        a DocumentFragment containing the HTML content that results from parsing
        the string

    a Node
        a DocumentFragment containing the Node

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
 */
function transferClasses(source, target, classes) {
	classes.forEach(cssClass => {
		if (source.classList.contains(cssClass)) {
			source.classList.remove(cssClass);
			target.classList.add(cssClass);
		}
	});
	if (source.className == "")
		source.removeAttribute("class");
}

/****************************************/
/*  Wrap an element in a wrapper element.
 */
function wrapElement(element, wrapClass, wrapTagName = "DIV", useExistingWrapper = false, moveClasses = false) {
    if (   useExistingWrapper
        && element.parentElement
        && element.parentElement.tagName == wrapTagName
        && element.parentElement.children.length == 1) {
        if (wrapClass > "")
            element.parentElement.classList.add(...(wrapClass.split(" ")));
    } else {
        let wrapper = newElement(wrapTagName);
        if (wrapClass > "")
            wrapper.classList.add(...(wrapClass.split(" ")));
        element.parentElement.insertBefore(wrapper, element);
        wrapper.appendChild(element);
    }

    if (moveClasses === false)
        return element.parentElement;

    if (moveClasses === true) {
        element.parentElement.classList.add(...(element.classList));
        element.removeAttribute("class");
        return element.parentElement;
    }

    if (moveClasses instanceof Array)
        transferClasses(element, element.parentElement, moveClasses);

	return element.parentElement;
}

/*****************************************************/
/*  Wrap all elements specified by the given selector.
 */
function wrapAll(selector, 
				 wrapClassOrFunction, 
				 wrapTagName = "DIV", 
				 container = document.body, 
				 useExistingWrappers = false, 
				 moveClasses = false) {
    let wrapperFunction;
    if (typeof wrapClassOrFunction == "function") {
        wrapperFunction = wrapClassOrFunction;
    } else {
        wrapperFunction = (element) => {
            wrapElement(element, wrapClassOrFunction, wrapTagName, useExistingWrappers, moveClasses);
        };
    }

    container.querySelectorAll(selector).forEach(wrapperFunction);
}

/**************************************************************************/
/*  Replace an element with its contents. Returns array of unwrapped nodes.

	Options:

		moveID

		moveClasses
		classesToMove

		preserveBlockSpacing
 */
function unwrap(wrapper, options = {
	moveID: false,
	moveClasses: false,
	classesToMove: null,
	preserveBlockSpacing: false
}) {
	if (wrapper == null)
		return;

    if (wrapper.parentNode == null)
        return;

	let nodes = Array.from(wrapper.childNodes);

	//	Move ID, if specified.
	if (   options.moveID === true
		&& wrapper.id > ""
		&& wrapper.children.length == 1) {
		wrapper.firstElementChild.id = wrapper.id;
	}

	//	Preserve block spacing, if specified.
	if (   options.preserveBlockSpacing === true
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
			if (options.classesToMove == null) {
				child.classList.add(...(wrapper.classList));
			} else {
				options.classesToMove.forEach(cssClass => {
					if (wrapper.classList.contains(cssClass))
						child.classList.add(cssClass);
				});
			}
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

/*******************************************************/
/*  Unwrap all elements specified by the given selector.
 */
function unwrapAll(selector, root = document, options = { }) {
    root.querySelectorAll(selector).forEach(element => {
        unwrap(element, options);
    });
}

/*************************************************************************/
/*	Save an element’s inline styles in a .savedStyles DOM object property.
 */
function saveStyles(element, propertiesToSave) {
	let stylesToSave = { };

	for (let i = 0; i < element.style.length; i++) {
		let propertyName = element.style.item(i);
		if (propertiesToSave.includes(propertyName)) {
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

	for ([ propertyName, propertyValue ] of Object.entries(element.savedStyles))
		element.style.setProperty(propertyName, propertyValue);

	element.savedStyles = null;
}

/*****************************************************************************/
/*	Strip an element’s inline styles, optionally only removing some styles,
	optionally keeping some styles. (The ‘propertiesToSave’ argument overrides
	the ‘propertiesToRemove’ argument, i.e. if a property appears in both
	lists, it is saved.)
 */
function stripStyles(element, propertiesToRemove = null, propertiesToSave = null) {
	if (propertiesToSave)
		saveStyles(element, propertiesToSave);

	if (propertiesToRemove) {
		for (let i = 0; i < element.style.length; i++) {
			let propertyName = element.style.item(i);
			if (propertiesToRemove.includes(propertyName))
				element.style.removeProperty(propertyName);
		}
	} else {
		element.removeAttribute("style");
	}

	if (propertiesToSave)
		restoreStyles(element);

	if (element.style.length == 0)
		element.removeAttribute("style");
}

/******************************************************************************/
/*  Call the given function when the given element (if `target` is an element),
    or the element specified by the given selector (if `target` is a string),
    intersects the viewport.

    Optionally specify the intersection ratio.
 */
function lazyLoadObserver(f, target, options = { }) {
    if (typeof target == "string")
        target = (options.root ?? document).querySelector(target);

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

	Permissible options:

		excludeTags [array]
		alsoExcludeTags [array]
 */
function isNodeEmpty(node, options = { }) {
	if (node == null)
		return undefined;

    if (node.nodeType == Node.TEXT_NODE)
        return (node.textContent.match(/\S/) == null);

	if (node.nodeType == Node.ELEMENT_NODE) {
		if (   options.excludeIdentifiedElements
			&& node.id > "") {
			return false;
		} else if (options.excludeTags != null) {
			if (options.excludeTags.includes(node.tagName.toUpperCase()))
				return false;
		} else {
			if (   [ "IMG", "SVG", "VIDEO", "AUDIO", "IFRAME", "OBJECT" ].includes(node.tagName.toUpperCase())
				|| options.alsoExcludeTags?.includes(node.tagName.toUpperCase()))
				return false;
		}
	}

    if (node.childNodes.length == 0)
        return true;

    for (childNode of node.childNodes)
        if (isNodeEmpty(childNode) == false)
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
		"code",
		"sup",
		"sub",
		"span"
	].join(", ");

	let nodes = Array.from(element.childNodes);
	let nodeSequence = [ ];
	let node;
	let omitNode = (node) => isNodeEmpty(node, { alsoExcludeTags: [ "A" ], excludeIdentifiedElements: true });
	do {
		node = nodes.shift();

		if (   (   node?.nodeType == Node.TEXT_NODE
				|| (   node?.nodeType == Node.ELEMENT_NODE
					&& node.matches(inlineElementSelector)))
			&& omitNode(node) == false) {
			nodeSequence.push(node);
		} else if (omitNode(node)) {
			node.remove();
		} else {
			if (nodeSequence.length > 0) {
				//	Get next non-empty child node of the element (may be null).
				let nextNode = nodeSequence.last.nextSibling;
				while (omitNode(nextNode))
					nextNode = nextNode.nextSibling;

				//	Construct paragraph (<p>) to wrap node sequence.
				//	(This removes the nodes from the element.)
				let graf = newElement("P");
				graf.append(...nodeSequence);

				//	Insert paragraph (with the previously removed nodes).
				element.insertBefore(graf, nextNode)
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

/******************************/
/*	Returns union of two rects.
 */
function rectUnion (aRect, ...args) {
	let union = aRect;
	for (rect of args) {
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
    if (hash.length < 2)
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
    let req = new XMLHttpRequest();

    req.addEventListener("load", (event) => {
        if (event.target.status < 400) {
            if (options["onSuccess"])
                options.onSuccess(event);
        } else {
            if (options["onFailure"])
                options.onFailure(event);
        }
    });
    req.addEventListener("error", (event) => {
        if (options["onFailure"])
            options.onFailure(event);
    });

    let method = (options["method"] || "GET");
    let location = (options.location || document.location)
                   + ((options.params && method == "GET") ? ("?" + urlEncodeQuery(options.params)) : "");
    req.open(method, location);

    if (options["responseType"])
	    req.responseType = options["responseType"];

    if (options["method"] == "POST")
        req.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");

	if (options["headers"] != null)
		for (let [ headerName, headerValue ] of Object.entries(options["headers"]))
			req.setRequestHeader(headerName, headerValue);

    if (options["method"] == "POST") {
        req.send(urlEncodeQuery(options.params));
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

/**************************/
/*  Simple mutex mechanism.
 */
function doIfAllowed(f, passHolder, passName, releaseImmediately = false) {
    if (passHolder[passName] == false)
        return;

    passHolder[passName] = false;

    f();

    if (releaseImmediately) {
        passHolder[passName] = true;
    } else {
        requestAnimationFrame(() => {
            passHolder[passName] = true;
        });
    }
}

/*******************************************************************************/
/*  When the given event is triggered on the given target, after the given delay
    (in ms), call the given handler function. (Optionally, if the given cancel 
    event occurs in the interim - i.e. after the trigger event happens but 
    before the delay elapses - cancel calling the handler.)

    Return value of this function is an anonymous function which removes the
    listeners that this function adds.

    NOTE: If `delay` is 0 or less, then `cancelEventName` is ignored, and `func`
    is added as an event handler for `triggerEventName` directly.

    If `delay` is positive, then `func` will be called by a timer after `delay`
    ms, prior to which time it might be cancelled if `cancelEventName` (if any)
    occurs. (If `cancelEventName` is null, then `func` will be called after
    `delay` unconditionally.)
 */
function onEventAfterDelayDo(target, triggerEventName, delay, func, cancelEventNames = [ ]) {
    if (delay <= 0) {
        target.addEventListener(triggerEventName, func);
        return (() => {
            target.removeEventListener(triggerEventName, func);
        });
    } else {
        let timer = null;
        let events = { };
        target.addEventListener(triggerEventName, events.triggerEvent = (event) => {
            timer = setTimeout(func, delay, event);
        });
        if (typeof cancelEventNames == "string") {
        	cancelEventNames = cancelEventNames > "" 
        					   ? [ cancelEventNames ] 
        					   : [ ];
        }
        if (cancelEventNames.length > 0) {
        	cancelEventNames.forEach(cancelEventName => {
				target.addEventListener(cancelEventName, events.cancelEvent = (event) => {
					clearTimeout(timer);
				});
			});
        }
        return (() => {
            target.removeEventListener(triggerEventName, events.triggerEvent);
            if (cancelEventNames.length > 0) {
	        	cancelEventNames.forEach(cancelEventName => {
	                target.removeEventListener(cancelEventName, events.cancelEvent);
	            });
            }
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

GW.isX11 = () => {
    return (navigator.userAgent.indexOf("X11") > 0);
};


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
    doAjax({ location: `${location.origin}/static/404-error-` + fixedEncodeURIComponent(errorString) });
    GWLog(`Reporting ${(errorType || "error")}:  ${errorString}`, "error reporting", 1);
}


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
    GW.mediaQueryResponders[name](null, true);

    for ([ key, mediaQuery ] of Object.entries(GW.mediaQueries))
        mediaQuery.removeListener(GW.mediaQueryResponders[name]);

    GW.mediaQueryResponders[name] = null;
}


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
    for (lhs of this) {
        for (rhs of strings) {
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
    for (arg of args)
        product = product.π(arg);

    return product;
}


/*************/
/* DOCUMENTS */
/*************/

/*  Return the location (URL) associated with a document.
    (Document|DocumentFragment) => URL
 */
function baseLocationForDocument(doc) {
	if (doc == null) {
		return null;
	} else if (doc == document) {
        return URLFromString(location.href);
    } else if (   doc.body instanceof Element
               && doc.body.classList.contains("popframe-body")) {
        let spawningTarget = (Extracts.popFrameProvider == Popups
                              ? doc.body.popup.spawningTarget
                              : doc.body.popin.spawningTarget);
        return URLFromString(spawningTarget.href);
    } else if (doc.baseLocation) {
        return URLFromString(doc.baseLocation.href);
    } else {
        return null;
    }
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
                the condition returns true

            - ‘once’ (key) [optional]
                Boolean value; if true, the handler will be removed after the
                handler function is called once (note that if there is a
                condition function provided [see the ‘condition’ key], the
                handler function will not be called - and therefore will not be
                removed - if the named event is fired by the condition evaluates
                to false).

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
    addHandlerForEvent: (eventName, f, options = { }) => {
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

		//	Ensure phase option is non-null.
		options.phase = (options.phase ?? "");

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
 */

GW.contentLoadHandlers = { };

/*  Add content load handler (i.e., an event handler for the GW.contentDidLoad
    event). (Convenience function.)
 */
function addContentLoadHandler(handler, phase, condition = null, once = false) {
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
function addContentInjectHandler(handler, phase, condition = null, once = false) {
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
    fullWidthPossible:  1 << 2
};

/*  Event-specific pre-fire processing for the ‘GW.contentDidInject’ event.
 */
GW.notificationCenter.prefireProcessors["GW.contentDidInject"] = (eventInfo) => {
    for ([flagName, flagValue] of Object.entries(GW.contentDidInjectEventFlags))
        eventInfo[flagName] = (0 != (eventInfo.flags & flagValue));

    return eventInfo;
};


/********************/
/* EVENT LISTENERS */
/********************/

GW.eventListeners = { };

/*  Adds a named event listener to the page (or other target).
 */
function addNamedEventListener(eventName, fn, name, options = { }, target = document) {
    if (options?.defer) {
        doWhenPageLoaded(() => {
            requestAnimationFrame(() => {
                if (options?.ifDeferCallWhenAdd)
                    fn();
                addNamedEventListener(eventName, fn, name, { defer: false }, target);
            });
        });
        return;
    }

    let wrapper = (event) => {
        requestAnimationFrame(() => {
            fn(event);
            target.addEventListener(eventName, wrapper, { once: true, passive: true });
        });
    }
    target.addEventListener(eventName, wrapper, { once: true, passive: true });

    /*  Retain a reference to the event listener, if a name is provided.
     */
    if (name) {
    	if (GW.eventListeners[eventName] == null)
    		GW.eventListeners[eventName] = { };

        GW.eventListeners[eventName][name] = { wrapper: wrapper, target: target };
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
        GW.eventListeners[eventName][name] = null;
    }
}

/*  Adds a scroll event listener to the page (or other target).
 */
function addScrollListener(fn, name, options = { }, target = document) {
	return addNamedEventListener("scroll", fn, name, options, target);
}

/*  Removes a named scroll event listener from the page (or other target).
 */
function removeScrollListener(name) {
	removeNamedEventListener("scroll", name);
}

/*  Adds a resize event listener to the window.
 */
function addWindowResizeListener(fn, name, options) {
	return addNamedEventListener("resize", fn, name, options, window);
}

/*  Removes a named resize event listener from the window.
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
addScrollListener(updateScrollState, "updateScrollStateScrollListener", { defer: true, ifDeferCallWhenAdd: true });

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

    let preventScroll = (event) => { document.documentElement.scrollTop = GW.scrollState.lastScrollTop; };

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
        addScrollListener(updateScrollState, "updateScrollStateScrollListener");
    } else if (  !enable
               && isPageScrollingEnabled() == true) {
        document.documentElement.classList.toggle("scroll-enabled-not", true);
        addScrollListener(preventScroll, "preventScroll");
        removeScrollListener("updateScrollStateScrollListener");
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

/*  Run the given function immediately if the <body> element has already been
    created, or add a mutation observer to run it as soon as the <body> element
    is created.
 */
function doWhenBodyExists(f) {
    if (document.body) {
        f();
    } else {
        let observer = new MutationObserver((mutationsList, observer) => {
            if (document.body) {
                observer.disconnect();
                f();
            }
        });

        observer.observe(document.documentElement, { childList: true });
    }
}


/******************/
/* BROWSER EVENTS */
/******************/

/*  We know this is false here, because this script is inlined in the <head>
    of the page; so the page body has not yet loaded when this code runs.
 */
GW.DOMContentLoaded = false;

GWLog("document.readyState." + document.readyState, "browser event");
window.addEventListener("DOMContentLoaded", () => {
    GWLog("window.DOMContentLoaded", "browser event");
    GW.DOMContentLoaded = true;
    let pageURL = URLFromString(location.href);
    GW.notificationCenter.fireEvent("GW.contentDidLoad", {
        source: "DOMContentLoaded",
        container: document.body,
        document: document,
        loadLocation: pageURL
    });
    GW.notificationCenter.fireEvent("GW.contentDidInject", {
        source: "DOMContentLoaded",
        container: document.body,
        document: document,
        loadLocation: pageURL,
        flags: (  GW.contentDidInjectEventFlags.clickable
                | GW.contentDidInjectEventFlags.fullWidthPossible)
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

/********************************************************************/
/*	Inject a special page logo image of a specific type (‘halloween’, 
	‘christmas’, etc.). Directory structure and file naming for the 
	specified logo type must match existing holiday logos.
 */
function injectSpecialPageLogo(logoType, options = { }) {
	let scale = valMinMax(Math.ceil(window.devicePixelRatio), 1, 3);

	let logoPathname;
	if (options.randomize) {
		logoPathname = options.mode
					   ? `/static/img/logo/${logoType}/${options.mode}/logo-${logoType}-${options.mode}-%R-small-${scale}x.png`
					   : `/static/img/logo/${logoType}/logo-${logoType}-%R-small-${scale}x.png`;
	} else {
		logoPathname = options.mode
					   ? `/static/img/logo/${logoType}/${options.mode}/logo-${logoType}-${options.mode}-small-${scale}x.png`
					   : `/static/img/logo/${logoType}/logo-${logoType}-small-${scale}x.png`;
	}

	let logoSelector = "#sidebar .logo-image";
	let logoImage;

	/*	Note that randomAsset() and versionedAssetURL() are defined in misc.js,
		and so cannot be called prior to this.
	 */
	let replaceLogo = (logoImage) => {
		if (options.randomize)
			logoPathname = randomAsset(logoPathname);
		let versionedLogoURL = versionedAssetURL(logoPathname);

		let imageWrapper = newElement("SPAN", {
			class: "logo-image"
		});
		imageWrapper.append(newElement("IMG", {
			class: "figure-not", 
			src: versionedLogoURL.pathname + versionedLogoURL.search
		}));

		logoImage.replaceWith(imageWrapper);
	};

	if (logoImage = document.querySelector(logoSelector)) {
		replaceLogo(logoImage);
	} else {
		let observer = new MutationObserver((mutationsList, observer) => {
			if (logoImage = document.querySelector(logoSelector)) {
				observer.disconnect();
				replaceLogo(logoImage);
			}
		});
		observer.observe(document.documentElement, { childList: true });
	}
}

/*  If a special function is provided to apply classes, one should also be
    provided to remove those classes. (See the ‘halloween’ entry for example.)
 */
GW.specialOccasions = [
    [ "halloween", () => isTodayHalloween(), () => {
		//	Default to dark mode during Halloween.
        DarkMode.defaultMode = "dark";

		//	Different special styles for light and dark mode.
        document.body.classList.remove("special-halloween-dark", "special-halloween-light");
        let specialClass = DarkMode.computedMode() == "light"
                           ? "special-halloween-light"
                           : "special-halloween-dark";
        document.body.classList.add(specialClass);

		//	Replace logo.
		injectSpecialPageLogo("halloween", { mode: "dark", randomize: true });
      }, () => {
        document.body.classList.remove("special-halloween-dark", "special-halloween-light");
      } ],
    [ "christmas", () => isTodayChristmas(), () => {
    	document.body.classList.add("special-christmas");

		//	TODO: this!
		let numLogoVariants = 6;

		//	Replace logo.
		injectSpecialPageLogo("christmas", { mode: DarkMode.computedMode(), randomize: true });
      }, () => {
    	document.body.classList.remove("special-christmas");
      } ],
];

function isTodayHalloween() {
	//	The test page is Halloween Town.
	if (document.body.classList.contains("test-halloween"))
		return true;

    //	Only bother English-speakers with Anglosphere holidays like Halloween:
    let language = window.navigator.userLanguage || window.navigator.language;
    if ("en" == language.slice(0, 2)) { // match ‘en’, ‘en-US’, ‘en-GB’, ‘en-AU’...
        let now = new Date();
        let date = (now.toISOString()).slice(5,10); // `YYYY-MM-DDTHH:mm:ss.sssZ` → MM-DD
        let hour =  now.getHours();
        /*	It is a sin to celebrate Halloween while there is daylight; however, 
        	calculating local sunset or local ambient light is too hard (where 
        	would we even get that geolocation or light sensor data from‽), so 
        	we will simply define ‘night’ as >=6PM and <6AM.
         */
        return (date == "10-31" && hour >= 18) || (date == "11-01" && hour < 6)
    } else {
    	return false;
    }
}
function isTodayChristmas() {
	//	The test page is Christmas Town.
	if (document.body.classList.contains("test-christmas"))
		return true;

    let now = new Date();
    let date = (now.toISOString()).slice(5,10);
    let hour =  now.getHours();
    /*	Christmas = Christmas Eve + all Christmas Day; Christmas Eve starts in 
    	the evening, so again >5PM.
     */
    return (date == "12-24" && hour > 17) || (date == "12-25")
}

function applySpecialOccasionClasses() {
    for (occasion of GW.specialOccasions) {
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
	optionsCache: { },

	containersNeedingLayout: [ ],

	layoutProcessors: [ ],

	//	Block containers 
	blockContainers: [
		".markdownBody",
		"section",
		".collapse",
		"blockquote",
		".epigraph",
		".admonition",
		".sidenote",
		"#x-of-the-day"
	],

	//	Block elements get layout classes applied to them.
	blockElements: [
		"section",
		".collapse",
		"blockquote",
		".epigraph",
		"p",
		".columns",
		".footnote",
		"figure",
		"hr",
		".sourceCode",
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

	//	Do not apply block layout classes within these containers.
	blockLayoutExclusionSelector: [
		"#page-metadata",
		".TOC > *"
	].join(", "),

	blockSpacing: [
		[ "body.page-index .abstract > p.first-block",	 7, false ],
		[ "body.page-index section",					 7, false ],
		[ "body.page-index section li p + p",			 0, false ],

		[ ".float.first-block",			 2, false ],
		[ ".first-block",				 0, false ],

		[ ".heading + section",			 5, false ],
		[ ".heading + *",				 4, false ],

		[ ".annotation p.data-field.title + p.data-field",	
										 1, false ],
		[ ".annotation p.data-field.title + .data-field.annotation-abstract",
										 3, false ],

		[ ".aux-links-append + .aux-links-append",
										 0, false ],
		[ ".collapse.expanded-not p.aux-links-list-label + *",
										 0, false ],

		[ ".interview .exchange",		 4, false ],
		[ ".interview .utterance",		 2, false ],

		[ "p.footnote-back-block",		 1, false ],
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

GW.layout.defaultOptions = processLayoutOptions({
	blockContainers: GW.layout.blockContainers,
	blockElements: GW.layout.blockElements,
	skipElements: GW.layout.skipElements,
	nonEmptyElements: GW.layout.nonEmptyElements,
	wrapperElements: GW.layout.wrapperElements,
	halfWrapperElements: GW.layout.halfWrapperElements
});

/**********************************************************************/
/*	Registers a layout processor function, which will be applied to all 
	rendered content as part of the dynamic layout process.
 */
function addLayoutProcessor(processor, options = { }) {
	GW.layout.layoutProcessors.push([ processor, options ]);
}

/****************************************************/
/*	Activates dynamic layout for the given container.
 */
function startDynamicLayoutInContainer(container) {
	let blockContainersSelector = selectorize(GW.layout.blockContainers);

	let observer = new MutationObserver((mutationsList, observer) => {
		let baseDocumentLocation = baseLocationForDocument(mutationsList.first?.target?.getRootNode());

		//	Construct list of all block containers affected by these mutations.
		let affectedContainers = [ ];

		for (mutationRecord of mutationsList) {
			//	Find block container in which the mutated element is contained.
			let nearestContainer = mutationRecord.target.closest(blockContainersSelector);

			//	Avoid adding a container twice, and apply exclusions.
			if (   nearestContainer
				&& affectedContainers.includes(nearestContainer) == false
				&& nearestContainer.closest(GW.layout.blockLayoutExclusionSelector) == null)
				affectedContainers.push(nearestContainer);
		}

		/*	Exclude containers that are contained within other containers in
			the list, to prevent redundant processing.
		 */
		affectedContainers = affectedContainers.filter(c => affectedContainers.findIndex(x => 
			(c.compareDocumentPosition(x) & Node.DOCUMENT_POSITION_CONTAINS)
		) == -1);

		/*	Add containers to list of containers needing layout processing, if
			they are not there already.
		 */
		affectedContainers.forEach(affectedContainer => {
			if (GW.layout.containersNeedingLayout.includes(affectedContainer) == false)
				GW.layout.containersNeedingLayout.push(affectedContainer);
		});
		requestAnimationFrame(() => {
			GW.layout.currentPassBegin = performance.now();

			//	Do layout in all waiting containers.
			while (GW.layout.containersNeedingLayout.length > 0) {
				let nextContainer = GW.layout.containersNeedingLayout.shift();
				GW.layout.layoutProcessors.forEach(layoutProcessor => {
					let [ processor, options ] = layoutProcessor;

					let info = {
						container: nextContainer,
						baseLocation: baseDocumentLocation
					};
					if (options.condition?.(info) == false)
						return;

					processor(nextContainer);
				});
			}
		});
	});

	observer.observe(container, { subtree: true, childList: true });
}

/*************************************************/
/*	Activate dynamic layout for the main document.
 */
doWhenBodyExists(() => {
	startDynamicLayoutInContainer(document.body);
});

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

/******************************************************************/
/*	Generate element layout cache key for given action and options.
	(Or, just use provided cache key, if any.)
 */
function generateCacheKey(action, options) {
	return `${action} ${options.cacheKey}`;
}

/***************************************************************************/
/*	Retrieve desired result from element’s layout cache, or calculate it and 
	store in element’s layout cache; and, in any case, return.
 */
function useLayoutCache(element, uniqueKey, options, f) {
	options = processLayoutOptions(options);

	let cacheKey = generateCacheKey(uniqueKey, options);

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

	return useLayoutCache(element, "isWrapper", options, (element, options) => {
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

	//	Look inside “transparent” wrappers (that don’t affect layout).
	if (isWrapper(element[siblingKey], wrapperInType, options)) {
		let terminalBlock = terminalBlockOf(element[siblingKey], terminus, options);
		if (terminalBlock)
			return terminalBlock;
	}
	
	//	Skip elements that don’t participate in block flow.
	if (   isSkipped(element[siblingKey], options)
		|| (   isNodeEmpty(element[siblingKey]) == true
			&& isNonEmpty(element[siblingKey], options) == false))
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
		let childBlocks = childBlocksOf(element);
		for (let i  = (terminus == "first" ? 0                  : childBlocks.length - 1); 
				 i != (terminus == "first" ? childBlocks.length : -1); 
			     i += (terminus == "first" ? 1                  : -1)) {
			let terminalBlock = terminalBlockOf(childBlocks[i], terminus, options);
			if (   terminalBlock
				&& isSkipped(terminalBlock, options) == false
				&& (   isNodeEmpty(terminalBlock) == false
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

/**************************************************************************/
/*	Returns assembled and appropriately prefixed selector from given parts.
 */
function selectorize(parts) {
	return parts.map(part => (part == ".markdownBody" ? part : `.markdownBody ${part}`)).join(", ");
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
									cacheKey: "alsoBlocks_nonCollapseSectionHeadings"
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
/*	Returns a block’s drop cap type (‘goudy’, ‘yinit’, etc.), or null if none.
 */
function dropCapTypeOf(block) {
	return Array.from(block.classList).find(cssClass => /^drop-caps?-/.test(cssClass))?.replace("-caps-", "-cap-")?.slice("drop-cap-".length);
}

/******************************************************************************/
/*	Adds a drop cap class to a block. Drop caps may be ‘kanzlei’, ‘de-zs’, etc.
	(See default.css for the list.)
 */
function addDropCapClassTo(block, dropCapType) {
	if (block.classList.contains("force-drop-cap"))
		return;

	stripDropCapClassesFrom(block);

	block.classList.add(`drop-cap-${dropCapType}`);
}

/*************************************/
/*	Strip drop cap classes from block.
 */
function stripDropCapClassesFrom(block) {
	if (block.classList.contains("force-drop-cap"))
		return;

	block.classList.remove(...(Array.from(block.classList).filter(className => className.startsWith("drop-cap-"))));
}


/*********************/
/* LAYOUT PROCESSORS */
/*********************/

/*************************************************************************/
/*	Apply block layout classes to appropriate elements in given container.
 */
addLayoutProcessor(GW.layout.applyBlockLayoutClassesInContainer = (container) => {
	//	Designate headings.
	container.querySelectorAll(selectorize(range(1, 6).map(x => `h${x}`))).forEach(heading => {
		heading.classList.add("heading");
	});

	//	Designate floats.
	container.querySelectorAll(selectorize([
		".float-left",
		".float-right"
	])).forEach(floatBlock => {
		floatBlock.classList.add("float");
	});

	//	Designate lists.
	container.querySelectorAll(selectorize([
		"ul",
		"ol"
	])).forEach(list => {
		list.classList.add("list");
	});

	//	Designate float-containing lists.
	container.querySelectorAll(".markdownBody li .float").forEach(floatBlock => {
		let options = {
			alsoBlockContainers: [ ".list" ],
			cacheKey: "alsoBlockContainers_lists"
		};
		let container = blockContainerOf(floatBlock, options);
		while (container?.matches(".list")) {
			container.classList.add("has-floats");
			container = blockContainerOf(container, options);
		}
	});

	//	Designate “big lists”.
	/*	If any of a list’s list items have multiple non-list children, then 
		it is a “big list” (with consequences for block spacing).
	 */
	let isBigList = (list) => {
		if (list.matches(".list") != true)
			return false;

		for (let listItem of list.children) {
			if (childBlocksOf(listItem, {
					notWrapperElements: [ ".list" ],
					cacheKey: "notWrappers_lists"
				}).filter(x => x.matches(".list") != true).length > 1)
				return true;
		}

		return false;
	};
	container.querySelectorAll(".list").forEach(list => {
		let bigList = isBigList(list);

		/*	If this is a sub-list, and any other sub-lists on the same level as 
			this one are “big lists”, then this is also a “big list” (because
			the designation of “bigness” is applied to *list levels within a
			list tree*, not to individual lists).
		 */
		 let container = blockContainerOf(list, {
		 	alsoBlockContainers: [ "li" ],
		 	cacheKey: "alsoBlockContainers_listItems"
		 });
		 if (container?.matches("li")) {
		 	for (let listItem of container.parentElement.children) {
				if (childBlocksOf(listItem, {
						notWrapperElements: [ ".list" ],
						cacheKey: "notWrappers_lists"
					}).findIndex(x => isBigList(x)) != -1) {
					bigList = true;
					break;
				}
		 	}
		 }

		list.classList.toggle("big-list", bigList);
	});

	//	Apply special block sequence classes.
	container.querySelectorAll(selectorize(GW.layout.blockElements)).forEach(block => {
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
			let emptyGraf = isNodeEmpty(block);
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
				".margin-notes-block",
				".page-description-annotation",
				".annotation .data-field"
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
			if (   strictNextBlock?.matches(".list")
				&& block.textContent.trim().endsWith(":"))
				listHeading = true;
			block.classList.toggle("list-heading", listHeading);

			/*	Introductory paragraphs of documents or self-contained parts
				of documents (the .intro-graf class).
			 */
			let introGraf = false;
			if (   block.matches(".text-center, .margin-notes-block") != true
				&& block.matches(".in-list") != true
				&& block.closest("#footer, figcaption") == null
				&& block.firstElementChild?.matches("span.smallcaps") != true
				&& block.firstChild instanceof HTMLAnchorElement != true) {
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

				/*  Add drop cap class. This could be set globally, or 
					overridden by a .abstract; the latter could be 
					`drop-cap-not` (which nullifies any page-global drop-cap 
					class for the given block).
				 */
				let dropCapType = null;
				if (introGraf) {
					dropCapType = (previousBlock?.matches(".abstract blockquote")
								   ? dropCapTypeOf(previousBlock)
								   : null) ?? dropCapTypeOf(document.body);
				} else if (block.parentElement?.tagName == "DIV") {
					dropCapType = dropCapTypeOf(block.parentElement);
					if (dropCapType && dropCapType != "not")
						block.classList.add("first-graf");
				}
				if (dropCapType && dropCapType != "not") {
					addDropCapClassTo(block, dropCapType);
				} else {
					stripDropCapClassesFrom(block);
				}
			}
			block.classList.toggle("intro-graf", introGraf);
		}
	});
});

/**********************************************/
/*	Apply block spacing in the given container.
 */
addLayoutProcessor(GW.layout.applyBlockSpacingInContainer = (container) => {
	//	Apply block spacing.
	container.querySelectorAll(selectorize(GW.layout.blockElements)).forEach(block => {
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

	//	Lists require special treatment.

	//	Apply list spacing.
	container.querySelectorAll(selectorize([ "li:not(.footnote)" ])).forEach(listItem => {
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

	//	Floats require special treatment.
	container.querySelectorAll(selectorize([ ".float" ])).forEach(floatBlock => {
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
				items!) is a paragraph, then adjust margin.
			 */
			let strictNextBlock = nextBlockOf(floatBlock);
			if (strictNextBlock.matches("p"))
				nextBlockBSM = "" + (parseInt(nextBlockBSM) + 2);

			floatBlock.style.setProperty("--bsm", nextBlockBSM);
		}
	});
});

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
});/*	This code is part of dark-mode.js by Said Achmiz.
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

	/*  Set specified color mode (auto, light, dark).

		Called by: this file (immediately upon load)
		Called by: DarkMode.modeSelectButtonClicked (dark-mode.js)
	 */
	setMode: (selectedMode = DarkMode.currentMode()) => {
		GWLog("DarkMode.setMode", "dark-mode.js", 1);

		//	Set ‘media’ attribute of dark mode elements to match requested mode.
		document.querySelectorAll(DarkMode.switchedElementsSelector).forEach(element => {
			element.media = DarkMode.mediaAttributeValues[selectedMode];
		});

		//	Fire event.
		GW.notificationCenter.fireEvent("DarkMode.didSetMode");
	},

	/*	Returns currently active color mode (light or dark).
		Based on saved selector mode, plus system setting (if selected mode is
		‘auto’).
	 */
	computedMode: () => {
		return ((   DarkMode.currentMode() == "dark" 
				|| (   DarkMode.currentMode() == "auto" 
					&& GW.mediaQueries.systemDarkModeActive.matches))
				? "dark"
				: "light");
	}
};

//	Activate saved mode.
DarkMode.setMode();

//	Set up mode change events.
GW.notificationCenter.addHandlerForEvent("DarkMode.didSetMode", (info) => {
	GW.notificationCenter.fireEvent("DarkMode.computedModeDidChange");
});
doWhenMatchMedia(GW.mediaQueries.systemDarkModeActive, "DarkMode.fireComputedModeDidChangeEventForSystemDarkModeChange", () => {
	GW.notificationCenter.fireEvent("DarkMode.computedModeDidChange");
});
ReaderMode = {
    active: false,

    readerModeTitleNote: " (reader mode)",

    /*  Activate or deactivate reader mode, as determined by the current setting
        and the selected mode.
     */
    //  Called by: this file (doWhenBodyExists)
    //  Called by: ReaderMode.modeSelectButtonClicked (reader-mode.js)
    setMode: (selectedMode = ReaderMode.currentMode()) => {
        GWLog("ReaderMode.setMode", "reader-mode.js", 1);

        //  Activate (if needed).
        if (ReaderMode.enabled() == true)
            ReaderMode.activate();

        //  Fire event.
        GW.notificationCenter.fireEvent("ReaderMode.didSetMode");
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

    /*  Returns current (saved) mode (on, off, or auto).
     */
    currentMode: () => {
        return (localStorage.getItem("reader-mode-setting") || "auto");
    },

    /*  Masks links and hide other elements, as appropriate. This will hide
        linkicons and pop-frame indicators, and will thus cause reflow.
     */
    //  Called by: ReaderMode.setMode
    activate: () => {
        GWLog("ReaderMode.activate", "reader-mode.js", 1);

        ReaderMode.active = true;

        //  Add body classes.
        document.body.classList.add("reader-mode-active", "masked-links-hidden");

        //  Update document title.
        document.title += ReaderMode.readerModeTitleNote;
    },
};

//  Activate saved mode, once the <body> element is loaded (and classes known).
doWhenBodyExists(ReaderMode.setMode);
GW.assetVersions = {
	"/static/img/icon/icons.svg": "1698021605",
	"/static/img/logo/christmas/dark/logo-christmas-dark-1-small-1x.png": "1698080524",
	"/static/img/logo/christmas/dark/logo-christmas-dark-1-small-2x.png": "1698080524",
	"/static/img/logo/christmas/dark/logo-christmas-dark-1-small-3x.png": "1698080524",
	"/static/img/logo/christmas/dark/logo-christmas-dark-2-small-1x.png": "1698080524",
	"/static/img/logo/christmas/dark/logo-christmas-dark-2-small-2x.png": "1698080524",
	"/static/img/logo/christmas/dark/logo-christmas-dark-2-small-3x.png": "1698080524",
	"/static/img/logo/christmas/dark/logo-christmas-dark-3-small-1x.png": "1698080524",
	"/static/img/logo/christmas/dark/logo-christmas-dark-3-small-2x.png": "1698080524",
	"/static/img/logo/christmas/dark/logo-christmas-dark-3-small-3x.png": "1698080524",
	"/static/img/logo/christmas/dark/logo-christmas-dark-4-small-1x.png": "1698080524",
	"/static/img/logo/christmas/dark/logo-christmas-dark-4-small-2x.png": "1698080524",
	"/static/img/logo/christmas/dark/logo-christmas-dark-4-small-3x.png": "1698080524",
	"/static/img/logo/christmas/dark/logo-christmas-dark-5-small-1x.png": "1698080524",
	"/static/img/logo/christmas/dark/logo-christmas-dark-5-small-2x.png": "1698080524",
	"/static/img/logo/christmas/dark/logo-christmas-dark-5-small-3x.png": "1698080524",
	"/static/img/logo/christmas/light/logo-christmas-light-1-small-1x.png": "1698080167",
	"/static/img/logo/christmas/light/logo-christmas-light-1-small-2x.png": "1698080167",
	"/static/img/logo/christmas/light/logo-christmas-light-1-small-3x.png": "1698080167",
	"/static/img/logo/christmas/light/logo-christmas-light-2-small-1x.png": "1698080166",
	"/static/img/logo/christmas/light/logo-christmas-light-2-small-2x.png": "1698080166",
	"/static/img/logo/christmas/light/logo-christmas-light-2-small-3x.png": "1698080167",
	"/static/img/logo/christmas/light/logo-christmas-light-3-small-1x.png": "1698080166",
	"/static/img/logo/christmas/light/logo-christmas-light-3-small-2x.png": "1698080166",
	"/static/img/logo/christmas/light/logo-christmas-light-3-small-3x.png": "1698080166",
	"/static/img/logo/christmas/light/logo-christmas-light-4-small-1x.png": "1698080166",
	"/static/img/logo/christmas/light/logo-christmas-light-4-small-2x.png": "1698080166",
	"/static/img/logo/christmas/light/logo-christmas-light-4-small-3x.png": "1698080166",
	"/static/img/logo/christmas/light/logo-christmas-light-5-small-1x.png": "1698080167",
	"/static/img/logo/christmas/light/logo-christmas-light-5-small-2x.png": "1698080166",
	"/static/img/logo/christmas/light/logo-christmas-light-5-small-3x.png": "1698080166",
	"/static/img/logo/christmas/light/logo-christmas-light-6-small-1x.png": "1698080167",
	"/static/img/logo/christmas/light/logo-christmas-light-6-small-2x.png": "1698080167",
	"/static/img/logo/christmas/light/logo-christmas-light-6-small-3x.png": "1698080167",
	"/static/img/logo/halloween/dark/logo-halloween-dark-1-small-1x.png": "1697576584",
	"/static/img/logo/halloween/dark/logo-halloween-dark-1-small-2x.png": "1697576584",
	"/static/img/logo/halloween/dark/logo-halloween-dark-1-small-3x.png": "1697576584",
	"/static/font/drop-cap/dropcat/dark/B-1-small-1x.png": "1698164495",
	"/static/font/drop-cap/dropcat/dark/B-1-small-2x.png": "1698164496",
	"/static/font/drop-cap/dropcat/dark/B-2-small-1x.png": "1698164496",
	"/static/font/drop-cap/dropcat/dark/B-2-small-2x.png": "1698164496",
	"/static/font/drop-cap/dropcat/dark/B-3-small-1x.png": "1698164495",
	"/static/font/drop-cap/dropcat/dark/B-3-small-2x.png": "1698164495",
	"/static/font/drop-cap/dropcat/dark/B-4-small-1x.png": "1698164495",
	"/static/font/drop-cap/dropcat/dark/B-4-small-2x.png": "1698164497",
	"/static/font/drop-cap/dropcat/light/B-1-small-1x.png": "1698101518",
	"/static/font/drop-cap/dropcat/light/B-1-small-2x.png": "1698101518",
	"/static/font/drop-cap/dropcat/light/B-2-small-1x.png": "1698101518",
	"/static/font/drop-cap/dropcat/light/B-2-small-2x.png": "1698101518",
	"/static/font/drop-cap/dropcat/light/B-3-small-1x.png": "1698101518",
	"/static/font/drop-cap/dropcat/light/B-3-small-2x.png": "1698101519",
	"/static/font/drop-cap/dropcat/light/B-4-small-1x.png": "1698101519",
	"/static/font/drop-cap/dropcat/light/B-4-small-2x.png": "1698101519",
	"/static/font/drop-cap/dropcat/light/B-5-small-1x.png": "1698101519",
	"/static/font/drop-cap/dropcat/light/B-5-small-2x.png": "1698101519",
	"/static/font/drop-cap/dropcat/light/B-6-small-1x.png": "1698101519",
	"/static/font/drop-cap/dropcat/light/B-6-small-2x.png": "1698101519",
	"/static/font/drop-cap/dropcat/light/B-7-small-1x.png": "1698101519",
	"/static/font/drop-cap/dropcat/light/B-7-small-2x.png": "1698101519",
	"/static/font/drop-cap/dropcat/light/C-1-small-1x.png": "1697741351",
	"/static/font/drop-cap/dropcat/light/C-1-small-2x.png": "1697741351",
	"/static/font/drop-cap/dropcat/light/C-2-small-1x.png": "1697741351",
	"/static/font/drop-cap/dropcat/light/C-2-small-2x.png": "1697741350",
	"/static/font/drop-cap/dropcat/light/C-3-small-1x.png": "1697741350",
	"/static/font/drop-cap/dropcat/light/C-3-small-2x.png": "1697741351",
	"/static/font/drop-cap/dropcat/light/C-4-small-1x.png": "1698101189",
	"/static/font/drop-cap/dropcat/light/C-4-small-2x.png": "1698101189",
	"/static/font/drop-cap/dropcat/light/C-5-small-1x.png": "1697741351",
	"/static/font/drop-cap/dropcat/light/C-5-small-2x.png": "1697741351",
	"/static/font/drop-cap/dropcat/light/G-1-small-1x.png": "1698132107",
	"/static/font/drop-cap/dropcat/light/G-1-small-2x.png": "1698132108",
	"/static/font/drop-cap/dropcat/light/G-10-small-1x.png": "1698132111",
	"/static/font/drop-cap/dropcat/light/G-10-small-2x.png": "1698132111",
	"/static/font/drop-cap/dropcat/light/G-2-small-1x.png": "1698132108",
	"/static/font/drop-cap/dropcat/light/G-2-small-2x.png": "1698132107",
	"/static/font/drop-cap/dropcat/light/G-3-small-1x.png": "1698132108",
	"/static/font/drop-cap/dropcat/light/G-3-small-2x.png": "1698132109",
	"/static/font/drop-cap/dropcat/light/G-4-small-1x.png": "1698132109",
	"/static/font/drop-cap/dropcat/light/G-4-small-2x.png": "1698132109",
	"/static/font/drop-cap/dropcat/light/G-5-small-1x.png": "1698132110",
	"/static/font/drop-cap/dropcat/light/G-5-small-2x.png": "1698132110",
	"/static/font/drop-cap/dropcat/light/G-6-small-1x.png": "1698132111",
	"/static/font/drop-cap/dropcat/light/G-6-small-2x.png": "1698132111",
	"/static/font/drop-cap/dropcat/light/G-7-small-1x.png": "1698132111",
	"/static/font/drop-cap/dropcat/light/G-7-small-2x.png": "1698132111",
	"/static/font/drop-cap/dropcat/light/G-8-small-1x.png": "1698132111",
	"/static/font/drop-cap/dropcat/light/G-8-small-2x.png": "1698132111",
	"/static/font/drop-cap/dropcat/light/G-9-small-1x.png": "1698132111",
	"/static/font/drop-cap/dropcat/light/G-9-small-2x.png": "1698132111",
	"/static/font/drop-cap/dropcat/light/I-1-small-1x.png": "1698152552",
	"/static/font/drop-cap/dropcat/light/I-1-small-2x.png": "1698152552",
	"/static/font/drop-cap/dropcat/light/Q-1-small-1x.png": "1698157852",
	"/static/font/drop-cap/dropcat/light/Q-1-small-2x.png": "1698157852",
	"/static/font/drop-cap/dropcat/light/Q-10-small-1x.png": "1698162541",
	"/static/font/drop-cap/dropcat/light/Q-10-small-2x.png": "1698162541",
	"/static/font/drop-cap/dropcat/light/Q-11-small-1x.png": "1698162542",
	"/static/font/drop-cap/dropcat/light/Q-11-small-2x.png": "1698162541",
	"/static/font/drop-cap/dropcat/light/Q-12-small-1x.png": "1698162542",
	"/static/font/drop-cap/dropcat/light/Q-12-small-2x.png": "1698162542",
	"/static/font/drop-cap/dropcat/light/Q-2-small-1x.png": "1698162542",
	"/static/font/drop-cap/dropcat/light/Q-2-small-2x.png": "1698162542",
	"/static/font/drop-cap/dropcat/light/Q-3-small-1x.png": "1698162540",
	"/static/font/drop-cap/dropcat/light/Q-3-small-2x.png": "1698162542",
	"/static/font/drop-cap/dropcat/light/Q-4-small-1x.png": "1698162541",
	"/static/font/drop-cap/dropcat/light/Q-4-small-2x.png": "1698162541",
	"/static/font/drop-cap/dropcat/light/Q-5-small-1x.png": "1698162541",
	"/static/font/drop-cap/dropcat/light/Q-5-small-2x.png": "1698162541",
	"/static/font/drop-cap/dropcat/light/Q-6-small-1x.png": "1698162782",
	"/static/font/drop-cap/dropcat/light/Q-6-small-2x.png": "1698162782",
	"/static/font/drop-cap/dropcat/light/Q-7-small-1x.png": "1698162541",
	"/static/font/drop-cap/dropcat/light/Q-7-small-2x.png": "1698162541",
	"/static/font/drop-cap/dropcat/light/Q-8-small-1x.png": "1698162541",
	"/static/font/drop-cap/dropcat/light/Q-8-small-2x.png": "1698162541",
	"/static/font/drop-cap/dropcat/light/Q-9-small-1x.png": "1698162541",
	"/static/font/drop-cap/dropcat/light/Q-9-small-2x.png": "1698162541",
	"/static/font/drop-cap/dropcat/light/T-1-small-1x.png": "1698156177",
	"/static/font/drop-cap/dropcat/light/T-1-small-2x.png": "1698156177",
	"/static/font/drop-cap/dropcat/light/T-2-small-1x.png": "1698156177",
	"/static/font/drop-cap/dropcat/light/T-2-small-2x.png": "1698156178",
	"/static/font/drop-cap/dropcat/light/T-3-small-1x.png": "1698156178",
	"/static/font/drop-cap/dropcat/light/T-3-small-2x.png": "1698156178",
	"/static/font/drop-cap/dropcat/light/T-4-small-1x.png": "1698156179",
	"/static/font/drop-cap/dropcat/light/T-4-small-2x.png": "1698156179",
	"/static/font/drop-cap/dropcat/light/T-5-small-1x.png": "1698156402",
	"/static/font/drop-cap/dropcat/light/T-5-small-2x.png": "1698156402",
	"/static/font/drop-cap/dropcat/light/T-6-small-1x.png": "1698156181",
	"/static/font/drop-cap/dropcat/light/T-6-small-2x.png": "1698156181",
	"/static/font/drop-cap/dropcat/light/T-7-small-1x.png": "1698156558",
	"/static/font/drop-cap/dropcat/light/T-7-small-2x.png": "1698156558"
};
