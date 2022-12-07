/* Miscellaneous utility functions. */
/* author: Said Achmiz */
/* license: MIT */

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
    this.classList.add(classes[whichToAdd]);
    this.classList.remove(classes[1 - whichToAdd]);
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
        if (this.contains(token))
            return true;
    return false;
}

/*********************************************************************/
/*	Workaround for Firefox weirdness, based on more Firefox weirdness.
 */
DocumentFragment.prototype.getSelection = function () {
	return document.getSelection();
}

/**************************************************/
/*	The obvious equivalent of Element’s .innerHTML.
 */
Object.defineProperty(DocumentFragment.prototype, "innerHTML", {
    get() {
        return Array.from(this.childNodes).map(node => (node.nodeValue || node.outerHTML)).join("");
    }
});

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
    } else if (content instanceof NodeList) {
        docFrag.append(...(Array.from(content).map(node => document.importNode(node, true))));
    }

    return docFrag;
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
        return;

    if (moveClasses === true) {
        element.parentElement.classList.add(...(element.classList));
        element.removeAttribute("class");
        return;
    }

    if (!(moveClasses instanceof Array))
        return;

    moveClasses.forEach(cssClass => {
        if (element.classList.contains(cssClass)) {
            element.classList.remove(cssClass);
            element.parentElement.classList.add(cssClass);
        }
    });
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
    if (typeof wrapClassOrFunction == "string") {
        wrapperFunction = (element) => {
            wrapElement(element, wrapClassOrFunction, wrapTagName, useExistingWrappers, moveClasses);
        };
    } else {
        wrapperFunction = wrapClassOrFunction;
    }

    container.querySelectorAll(selector).forEach(wrapperFunction);
}

/****************************************/
/*  Replace an element with its contents.
 */
function unwrap(wrapper, moveClasses = false) {
    if (wrapper.parentNode == null)
        return;

    while (wrapper.childNodes.length > 0) {
		let child = wrapper.firstChild;

        wrapper.parentNode.insertBefore(child, wrapper);

		if (!(child instanceof Element))
			continue;

		if (moveClasses === false)
			continue;

		if (moveClasses === true) {
			child.classList.add(...(wrapper.classList));
			continue;
		}

		if (!(moveClasses instanceof Array))
			continue;

		moveClasses.forEach(cssClass => {
			if (wrapper.classList.contains(cssClass))
				child.classList.add(cssClass);
		});
    }
    wrapper.remove();
}

/*******************************************************/
/*  Unwrap all elements specified by the given selector.
 */
function unwrapAll(selector, root = document, moveClasses = false) {
    root.querySelectorAll(selector).forEach(element => {
        unwrap(element, moveClasses);
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

/*****************************************************************************/
/*  Call the given function when the given element (if `target` is an element),
    or the element specified by the given selector (if `target` is a string),
    intersects the viewport.
    Optionally specify the interaction ratio.
 */
function lazyLoadObserver(f, target, options = { }) {
    if (typeof target == "string")
        target = document.querySelector(target);

    if (target == null)
        return;

    if (   options.root == null
        && (options.threshold ?? 0) == 0
        && (options.rootMargin ?? "0px") == "0px"
        && isOnScreen(target)) {
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
}

/*******************************************************************************/
/*  Returns true if the node’s parent has just one child (i.e., the given node),
    or if all siblings are empty nodes. (Note that the given node *itself* being
    empty does not prevent this function from returning true!)
 */
function isOnlyChild(node) {
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
 */
function isNodeEmpty(node) {
    if (node.nodeType == Node.TEXT_NODE)
        return (node.textContent.match(/\S/) == null);

    if (   node.nodeType == Node.ELEMENT_NODE
        && [ "IMG", "VIDEO", "AUDIO", "IFRAME", "OBJECT" ].includes(node.tagName))
        return false;

    if (node.childNodes.length == 0)
        return true;

    for (childNode of node.childNodes)
        if (isNodeEmpty(childNode) == false)
            return false;

    return true;
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
    let docFrag = new DocumentFragment();
    docFrag.append(doc.getSelection().getRangeAt(0).cloneContents());

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
 */
function doRectsIntersect(rectA, rectB) {
    return (   rectA.top < rectB.bottom
            && rectA.bottom > rectB.top
            && rectA.left < rectB.right
            && rectA.right > rectB.left);
}

/***************************************************************/
/*  Returns true if the given element intersects the given rect,
    false otherwise.
 */
function isWithinRect(element, rect) {
    return doRectsIntersect(element.getBoundingClientRect(), rect);
}

/******************************************************************************/
/*  Returns true if the given element intersects the viewport, false otherwise.
 */
function isOnScreen (element) {
    return isWithinRect(element, new DOMRect(0, 0, window.innerWidth, window.innerHeight));
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

/*****************************************************************************/
/*  Returns a URL object with no hash, and otherwise the `href` equal to that
    represented by the passed object (either a string, or some object that has
    the `href` property, such as an <A> element, or the `location` object).
 */
function urlSansHash(url) {
    let fixedURL = null;
    if (typeof url == "string")
        fixedURL = new URL(url);
    else
        fixedURL = new URL(url.href);
    fixedURL.hash = "";
    return fixedURL;
}

/*************************************************************************/
/*  Because encodeURIComponent does not conform to RFC 3986; see MDN docs.
 */
function fixedEncodeURIComponent(str) {
    return encodeURIComponent(str).replace(/[!'()*]/g, function(c) {
        return '%' + c.charCodeAt(0).toString(16);
    });
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
			return pair[1];
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
    if (options["method"] == "POST") {
        req.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
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
function onEventAfterDelayDo(target, triggerEventName, delay, func, cancelEventName = null) {
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
        if (cancelEventName != null) {
            target.addEventListener(cancelEventName, events.cancelEvent = (event) => {
                clearTimeout(timer);
            });
        }
        return (() => {
            target.removeEventListener(triggerEventName, events.triggerEvent);
            if (cancelEventName != null)
                target.removeEventListener(cancelEventName, events.cancelEvent);
        });
    }
}

