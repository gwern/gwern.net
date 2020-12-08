/*  Create global 'GW' object, if need be.
	*/
if (typeof window.GW == "undefined")
	window.GW = { };

/********************/
/* DEBUGGING OUTPUT */
/********************/

function GWLog (string) {
    if (GW.loggingEnabled || localStorage.getItem("logging-enabled") == "true")
        console.log(string);
}
GW.enableLogging = (permanently = false) => {
    if (permanently)
        localStorage.setItem("logging-enabled", "true");

	GW.loggingEnabled = true;
};
GW.disableLogging = (permanently = false) => {
    if (permanently)
        localStorage.removeItem("logging-enabled");

	GW.loggingEnabled = false;
};

/***********/
/* HELPERS */
/***********/

Array.prototype.remove = function (item) {
	var index = this.indexOf(item);
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
/*	Handler object should have members `f` (a function) and `once` (a boolean).
	*/
GW.notificationCenter = { };
GW.notificationCenter.addHandlerForEvent = function (eventName, handler) {
	if (GW.notificationCenter[eventName] == null)
		GW.notificationCenter[eventName] = [ ];

	if (GW.notificationCenter[eventName].includes(handler))
		return;

	GW.notificationCenter[eventName].push(handler);
};
GW.notificationCenter.cancelHandlerForEvent = function (eventName, handler) {
	if (GW.notificationCenter[eventName] == null)
		return;

	GW.notificationCenter[eventName].remove(handler);
}
GW.notificationCenter.cancelAllHandlersForEvent = function (eventName) {
	GW.notificationCenter[eventName] = null;
}
GW.notificationCenter.fireEvent = function (eventName) {
	GWLog(`Event “${eventName}” fired.`);

	if (GW.notificationCenter[eventName] == null)
		return;

	GW.notificationCenter[eventName].forEach(handler => {
		handler.f();
		if (handler.once)
			GW.notificationCenter.cancelHandlerForEvent(eventName, handler);
	});
}
