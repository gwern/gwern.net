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

/*****************/
/* NOTIFICATIONS */
/*****************/
/*	Handler object should have members `f` (a function) and `once` (a boolean).
	*/
GW.notificationCenter = { };

function addHandlerForEvent (eventName, handler) {
	if (GW.notificationCenter[eventName] == null)
		GW.notificationCenter[eventName] = [ ];

	if (GW.notificationCenter[eventName].includes(handler))
		return;

	GW.notificationCenter[eventName].push(handler);
}
function cancelHandlerForEvent (eventName, handler) {
	if (GW.notificationCenter[eventName] == null)
		return;

	GW.notificationCenter[eventName].remove(handler);
}
function cancelAllHandlersForEvent (eventName) {
	GW.notificationCenter[eventName] = null;
}
function fireEvent (eventName) {
	if (GW.notificationCenter[eventName] == null)
		return;

	GW.notificationCenter[eventName].forEach(handler => {
		handler.f();
		if (handler.once)
			cancelDoWhen(eventName, handler);
	});
}
