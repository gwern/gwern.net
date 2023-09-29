/* Miscellaneous utility functions. */
/* author: Said Achmiz */
/* license: MIT */

/****************************************************************/
/*	Generates integer from a uniform distribution over [1, size].
 */
function rollDie(size) {
	return Math.floor(Math.random() * (size - 1) + 1);
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
		if (options.excludeTags != null) {
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
	do {
		node = nodes.shift();

		if (   (   node?.nodeType == Node.TEXT_NODE
				|| (   node?.nodeType == Node.ELEMENT_NODE
					&& node.matches(inlineElementSelector)))
			&& isNodeEmpty(node, { alsoExcludeTags: [ "A" ] }) == false) {
			nodeSequence.push(node);
		} else if (isNodeEmpty(node, { alsoExcludeTags: [ "A" ] })) {
			node.remove();
		} else {
			if (nodeSequence.length > 0) {
				//	Get next non-empty child node of the element (may be null).
				let nextNode = nodeSequence.last.nextSibling;
				while (isNodeEmpty(nextNode, { alsoExcludeTags: [ "A" ] }))
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
