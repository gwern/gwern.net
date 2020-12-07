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

