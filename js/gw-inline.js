/*  Create global 'GW' object, if need be.
	*/
if (typeof window.GW == "undefined")
	window.GW = { };

/********************/
/* DEBUGGING OUTPUT */
/********************/

GW.logLevel = localStorage.getItem("gw-log-level");

function GWLog (string, level = 1) {
    if (GW.logLevel >= level)
        console.log(string);
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
	GWLog(`Event “$${eventName}” fired.`);

	if (GW.notificationCenter[eventName] == null)
		return;

	GW.notificationCenter[eventName].forEach(handler => {
		handler.f();
		if (handler.options.once)
			GW.notificationCenter.cancelHandlerForEvent(eventName, handler.f);
	});
}
