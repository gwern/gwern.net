/*  Create global 'GW' object, if need be.
    */
if (typeof window.GW == "undefined")
    window.GW = { };

/*****************/
/* MEDIA QUERIES */
/*****************/

GW.mediaQueries = {
    mobileWidth:           matchMedia("(max-width: 650px)"),
    systemDarkModeActive:  matchMedia("(prefers-color-scheme: dark)"),
    hoverAvailable:        matchMedia("only screen and (hover: hover) and (pointer: fine)"),
    portraitOrientation:   matchMedia("(orientation: portrait)")
};

GW.isMobile = () => {
	/*  We consider a client to be mobile if one of two conditions obtain:
		1. JavaScript detects touch capability, AND viewport is narrow; or,
		2. CSS does NOT detect hover capability.
		*/
	return (   (   ('ontouchstart' in document.documentElement)
				&& GW.mediaQueries.mobileWidth.matches)
			|| !GW.mediaQueries.hoverAvailable.matches);
};

GW.isFirefox = () => {
	return (navigator.userAgent.indexOf("Firefox") > 0);
};

/********************/
/* DEBUGGING OUTPUT */
/********************/

GW.logLevel = localStorage.getItem("gw-log-level") || 0;
GW.logSourcePadLength = 18;
GW.dateTimeFormat = new Intl.DateTimeFormat([], { hour12: false, hour: "numeric", minute: "numeric", second: "numeric" });

function GWLog (string, source = "", level = 1) {
    if (GW.logLevel < level) return;

    let time = Date.now();
    let ms = `${(time % 1000)}`.padStart(3,'0');
    let timestamp = `[${GW.dateTimeFormat.format(time)}.${ms}]  `;
    let sourcestamp = (source > "" ? `[${source}]` : `[ ]`).padEnd(GW.logSourcePadLength, ' ');

    console.log(timestamp + sourcestamp + string);
}
GW.setLogLevel = (level, permanently = false) => {
    if (permanently)
        localStorage.setItem("gw-log-level", level);

    GW.logLevel = level;
};

/***********/
/* HELPERS */
/***********/

/*	Helper function for AJAX, by kronusaturn
	https://github.com/kronusaturn/lw2-viewer/blob/master/www/script.js
	*/
function doAjax(options) {
	let req = new XMLHttpRequest();
	req.addEventListener("load", (event) => {
		if (event.target.status < 400) {
			if (options["onSuccess"]) options.onSuccess(event);
		} else {
			if (options["onFailure"]) options.onFailure(event);
		}
	});
	req.open((options["method"] || "GET"), (options.location || document.location) + (options.params ? "?" + urlEncodeQuery(options.params) : ""));
	if (options["method"] == "POST") {
		req.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
		req.send(urlEncodeQuery(options.params));
	} else {
		req.send();
	}
}

/*  Adds an event listener to a button (or other clickable element), attaching
    it to both ‘click’ and ‘keyup’ events (for use with keyboard navigation).
    Optionally also attaches the listener to the ‘mousedown’ event, making the
    element activate on mouse down instead of mouse up.
    */
Element.prototype.addActivateEvent = function(fn, includeMouseDown) {
    let ael = this.activateEventListener = (event) => { 
    	if (event.button === 0 || event.key === ' ') 
    		fn(event);
    };
    if (includeMouseDown) this.addEventListener("mousedown", ael);
    this.addEventListener("click", ael);
    this.addEventListener("keyup", ael);
}

/*	Swap classes on the given element.
	First argument is an array with two string elements (the classes).
	Second argument is 0 or 1 (index of class to add; the other is removed).
	*/
Element.prototype.swapClasses = function (classes, whichToAdd) {
	this.classList.add(classes[whichToAdd]);
	this.classList.remove(classes[1 - whichToAdd]);
};

/*  Returns true if the given element intersects the viewport, false otherwise.
    */
function isOnScreen (element) {
    let rect = element.getBoundingClientRect();
    return (rect.top < window.innerHeight &&
            rect.bottom > 0 &&
            rect.left < window.innerWidth &&
            rect.right > 0);
}

/*	Returns the string trimmed of opening/closing quotes.
	*/
String.prototype.trimQuotes = function () {
	return this.replace(/^["'“‘]?(.+?)["'”’]?$/, '$1');
};

/*  Returns true if the string begins with any of the given prefixes.
    */
String.prototype.startsWithAnyOf = function (prefixes) {
    for (prefix of prefixes)
        if (this.startsWith(prefix))
            return true;
    return false;
}

/*  Returns true if the string ends with any of the given suffixes.
    */
String.prototype.endsWithAnyOf = function (suffixes) {
    for (suffix of suffixes)
        if (this.endsWith(suffix))
            return true;
    return false;
}

/*  Returns true if the string includes any of the given substrings.
    */
String.prototype.includesAnyOf = function (substrings) {
    for (substring of substrings)
        if (this.includes(substring))
            return true
    return false;
}

/*  Remove given item from array.
	*/
Array.prototype.remove = function (item) {
    var index = this.indexOf(item);
    if (index !== -1)
        this.splice(index, 1);
};

/*  Remove from array the first item that passes the provided test function.
	The test function should take an array item and return true/false.
	*/
Array.prototype.removeIf = function (test) {
    var index = this.findIndex(test);
    if (index !== -1)
        this.splice(index, 1);
};

/*  Run the given function immediately if the page is already loaded, or add
    a listener to run it as soon as the page loads.
    */
function doWhenPageLoaded(f) {
    if (document.readyState == "complete")
        f();
    else
        window.addEventListener("load", f);
}

/*  Run the given function immediately if the page content has already loaded
	(DOMContentLoaded event has fired), or add a listener to run it as soon as 
	the event fires.
    */
function doWhenDOMContentLoaded(f) {
    if (GW.DOMContentLoaded == true)
        f();
    else
        window.addEventListener("DOMContentLoaded", f);
}

/*  Given an HTML string, creates an element from that HTML, adds it to
    #ui-elements-container (creating the latter if it does not exist), and
    returns the created element.
    */
function addUIElement(element_html) {
    var ui_elements_container = document.querySelector("#ui-elements-container");
    if (!ui_elements_container) {
        ui_elements_container = document.createElement("div");
        ui_elements_container.id = "ui-elements-container";
        document.querySelector("body").appendChild(ui_elements_container);
    }

    ui_elements_container.insertAdjacentHTML("beforeend", element_html);
    return ui_elements_container.lastElementChild;
}

/*  Adds a scroll event listener to the page.
    */
function addScrollListener(fn, name) {
    let wrapper = (event) => {
        requestAnimationFrame(() => {
            fn(event);
            document.addEventListener("scroll", wrapper, { once: true, passive: true });
        });
    }
    document.addEventListener("scroll", wrapper, { once: true, passive: true });

    // Retain a reference to the scroll listener, if a name is provided.
    if (typeof name != "undefined") {
    	if (GW.scrollListeners == null)
    		GW.scrollListeners = { };

        GW.scrollListeners[name] = wrapper;
    }
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

            if (otherwiseDo == null || matches) ifMatchesOrAlwaysDo(mediaQuery);
            else otherwiseDo(mediaQuery);
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

/*****************/
/* NOTIFICATIONS */
/*****************/
/*  Options object may have members:

        `once` (boolean; does the handler get removed after being called?)
    */
GW.notificationCenter = { };
GW.notificationCenter.addHandlerForEvent = function (eventName, f, options = { }) {
    if (GW.notificationCenter[eventName] == null)
        GW.notificationCenter[eventName] = [ ];

    if (GW.notificationCenter[eventName].findIndex(handler => handler.f == f) !== -1)
        return;

    GW.notificationCenter[eventName].push({ f: f, options: options });
};
GW.notificationCenter.removeHandlerForEvent = function (eventName, f, options = { }) {
    if (GW.notificationCenter[eventName] == null)
        return;

    GW.notificationCenter[eventName].removeIf(handler => handler.f == f);
}
GW.notificationCenter.removeAllHandlersForEvent = function (eventName) {
    GW.notificationCenter[eventName] = null;
}
GW.notificationCenter.fireEvent = function (eventName, eventInfo) {
    GWLog(`Event “${eventName}” fired.`, "notification");

    if (GW.notificationCenter[eventName] == null)
        return;

	for (var i = 0; i < GW.notificationCenter[eventName].length; i++) {
		let handler = GW.notificationCenter[eventName][i];
        handler.f(eventInfo);
        if (handler.options.once) {
        	GW.notificationCenter[eventName].splice(i, 1);
        	i--;
        }
	}
}

/******************/
/* BROWSER EVENTS */
/******************/

GW.DOMContentLoaded = false;

GWLog("document.readyState." + document.readyState, "browser event");
window.addEventListener("DOMContentLoaded", () => {
    GWLog("window.DOMContentLoaded", "browser event");
    GW.DOMContentLoaded = true;
});
window.addEventListener("load", () => {
    GWLog("window.load", "browser event");
});
document.addEventListener("readystatechange", () => {
    GWLog("document.readyState." + document.readyState, "browser event");
});
