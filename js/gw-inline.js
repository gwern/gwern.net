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

GW.logLevel = localStorage.getItem("gw-log-level");
GW.dateTimeFormat = new Intl.DateTimeFormat([], { hour12: false, hour: "numeric", minute: "numeric", second: "numeric" });

function GWLog (string, level = 1) {
	let time = Date.now();
    if (GW.logLevel >= level)
        console.log("[" + GW.dateTimeFormat.format(time) + "." + `${(time % 1000)}`.padStart(3,'0') + "]  " + string);
}
GW.setLogLevel = (level, permanently = false) => {
	if (permanently)
		localStorage.setItem("gw-log-level", level);

	GW.logLevel = level;
};

/***********/
/* HELPERS */
/***********/

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

/*  Returns true if the string begins with the given prefix.
    */
String.prototype.hasPrefix = function (prefix) {
    return (this.lastIndexOf(prefix, 0) === 0);
}

/*****************/
/* NOTIFICATIONS */
/*****************/
/*	Options object may have members:

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
GW.notificationCenter.cancelHandlerForEvent = function (eventName, f, options = { }) {
	if (GW.notificationCenter[eventName] == null)
		return;

	GW.notificationCenter[eventName].removeIf(handler => handler.f == f);
}
GW.notificationCenter.cancelAllHandlersForEvent = function (eventName) {
	GW.notificationCenter[eventName] = null;
}
GW.notificationCenter.fireEvent = function (eventName) {
	GWLog(`[notification]    Event “${eventName}” fired.`);

	if (GW.notificationCenter[eventName] == null)
		return;

	GW.notificationCenter[eventName].forEach(handler => {
		handler.f();
		if (handler.options.once)
			GW.notificationCenter.cancelHandlerForEvent(eventName, handler.f);
	});
}

/******************/
/* BROWSER EVENTS */
/******************/

GWLog("[browser event]   document.readyState." + document.readyState, 1);
window.addEventListener("DOMContentLoaded", () => {
	GWLog("[browser event]   window.DOMContentLoaded", 1);
});
window.addEventListener("load", () => {
	GWLog("[browser event]   window.load", 1);
});
document.addEventListener("readystatechange", () => {
	GWLog("[browser event]   document.readyState." + document.readyState, 1);
});
