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
    hoverAvailable:        matchMedia("only screen and (hover:hover) and (pointer:fine)")
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

/*  Returns true if the given element intersects the viewport, false otherwise.
    */
function isOnScreen (element) {
    let rect = element.getBoundingClientRect();
    return (rect.top < window.innerHeight &&
            rect.bottom > 0 &&
            rect.left < window.innerWidth &&
            rect.right > 0);
}

/*  Returns true if the string begins with any of the given prefixes.
    */
String.prototype.startsWithAnyOf = function (prefixes) {
    for (prefix of prefixes)
        if (this.startsWith(prefix))
            return true;
    return false;
}

/*  Returns true if the string includes any of the given substrings.
    */
String.prototype.includesAnyOf = function (substrings) {
    for (const substring of substrings)
        if (this.includes(substring))
            return true
    return false;
}

Array.prototype.remove = function (item) {
    var index = this.indexOf(item);
    if (index !== -1)
        this.splice(index, 1);
};

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
GW.notificationCenter.cancelAllHandlersForEvent = function (eventName) {
    GW.notificationCenter[eventName] = null;
}
GW.notificationCenter.fireEvent = function (eventName, eventInfo) {
    GWLog(`Event “${eventName}” fired.`, "notification");

    if (GW.notificationCenter[eventName] == null)
        return;

    GW.notificationCenter[eventName].forEach(handler => {
        handler.f(eventInfo);
        if (handler.options.once)
            GW.notificationCenter.removeHandlerForEvent(eventName, handler.f);
    });
}

/******************/
/* BROWSER EVENTS */
/******************/

GWLog("document.readyState." + document.readyState, "browser event");
window.addEventListener("DOMContentLoaded", () => {
    GWLog("window.DOMContentLoaded", "browser event");
});
window.addEventListener("load", () => {
    GWLog("window.load", "browser event");
});
document.addEventListener("readystatechange", () => {
    GWLog("document.readyState." + document.readyState, "browser event");
});
