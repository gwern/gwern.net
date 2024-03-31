/*********************/
/* SPECIAL OCCASIONS */
/*********************/

/********************************************************************/
/*  Inject a special page logo image of a specific type (‘halloween’,
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

    //  Temporarily brighten logo, then fade slowly after set duration.
    let brightenLogoTemporarily = (brightDuration, fadeDuration) => {
        let logoLink = document.querySelector("#sidebar a.logo");

        logoLink.classList.add("bright");
        logoLink.fadeTimer = setTimeout(() => {
            logoLink.swapClasses( [ "bright", "fading" ], 1);
            logoLink.fadeTimer = setTimeout(() => {
                logoLink.classList.remove("fading");
            }, fadeDuration);
        }, brightDuration);

        //  Ensure proper interaction with mouse hover.
        logoLink.addEventListener("mouseenter", (event) => {
            logoLink.classList.remove("fading");
        });
        logoLink.addEventListener("mouseleave", (event) => {
            logoLink.classList.remove("fading");
        });
    }

    /*  Note that randomAsset() and versionedAssetURL() are defined in misc.js,
        and so cannot be called prior to this.
     */
    let replaceLogo = (logoImage) => {
        //  Get new logo URL (random, if need be).
        if (options.randomize)
            logoPathname = randomAsset(logoPathname);
        let versionedLogoURL = versionedAssetURL(logoPathname);

        //  Create new image element and wrapper.
        let imageWrapper = newElement("SPAN", {
            class: "logo-image"
        });
        imageWrapper.append(newElement("IMG", {
            class: "figure-not",
            src: versionedLogoURL.pathname + versionedLogoURL.search
        }));

        //  Inject wrapped image.
        logoImage.replaceWith(imageWrapper);

        //  Brighten logo; fade (over 1 second) after 20 seconds.
        brightenLogoTemporarily(20 * 1000, 1000);
    };

    let logoSelector = "#sidebar .logo-image";
    let logoImage;
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
    [ "halloween", isTodayHalloween, () => {
        //  Default to dark mode during Halloween.
        DarkMode.defaultMode = "dark";

        //  Different special styles for light and dark mode.
        document.body.classList.remove("special-halloween-dark", "special-halloween-light");
        let specialClass = DarkMode.computedMode() == "light"
                           ? "special-halloween-light"
                           : "special-halloween-dark";
        document.body.classList.add(specialClass);

        //  Replace logo.
        injectSpecialPageLogo("halloween", { mode: "dark", randomize: true });
      }, () => {
        document.body.classList.remove("special-halloween-dark", "special-halloween-light");
      } ],
    [ "christmas", isTodayChristmas, () => {
        //  Different special styles for light and dark mode.
        document.body.classList.remove("special-christmas-dark", "special-christmas-light");
        let specialClass = DarkMode.computedMode() == "light"
                           ? "special-christmas-light"
                           : "special-christmas-dark";
        document.body.classList.add(specialClass);

        //  Replace logo.
        injectSpecialPageLogo("christmas", { mode: DarkMode.computedMode(), randomize: true });
      }, () => {
        document.body.classList.remove("special-christmas-dark", "special-christmas-light");
      } ],
    [ "april-fools", isTodayAprilFools ],
];

function toggleTestHalloween(enable) {
	if (enable)
		localStorage.setItem("test-halloween", true);
	else
		localStorage.removeItem("test-halloween");
}

function isTodayHalloween() {
    //  The test page is Halloween Town.
    if (   document.body.classList.contains("test-halloween")
    	|| localStorage.getItem("test-halloween") == "true")
        return true;

    //  Only bother English-speakers with Anglosphere holidays like Halloween:
    let language = window.navigator.userLanguage || window.navigator.language;
    if ("en" == language.slice(0, 2)) { // match ‘en’, ‘en-US’, ‘en-GB’, ‘en-AU’...
        let now = new Date();
        let date = now.toString().slice(4,10);
        let hour = now.getHours();

        /*  It is a sin to celebrate Halloween while there is daylight; however,
            calculating local sunset or local ambient light is too hard (where
            would we even get that geolocation or light sensor data from‽), so
            we will simply define ‘night’ as >=6PM and <6AM.
        */
        return (date == "Oct 31" && hour >= 18) || (date == "Nov 01" && hour < 6);
    } else {
        return false;
    }
}

function toggleTestChristmas(enable) {
	if (enable)
		localStorage.setItem("test-christmas", true);
	else
		localStorage.removeItem("test-christmas");
}

function isTodayChristmas() {
    //  The test page is Christmas Town.
    if (   document.body.classList.contains("test-christmas")
    	|| localStorage.getItem("test-christmas") == "true")
        return true;

    let now = new Date();
    let date = now.toString().slice(4,10);
    let hour = now.getHours();

    /*  Christmas = Christmas Eve + all Christmas Day; Christmas Eve starts in
        the evening, so again >=6PM.
     */
    return (date == "Dec 24" && hour >= 18) || (date == "Dec 25");
}

function toggleTestAprilFools(enable) {
	if (enable)
		localStorage.setItem("test-april-fools", true);
	else
		localStorage.removeItem("test-april-fools");
}

function isTodayAprilFools() {
    //  The test page is Fraktur-town
    if (   document.body.classList.contains("test-april-fools")
    	|| localStorage.getItem("test-april-fools") == "true")
        return true;

    let now = new Date();
    let date = now.toString().slice(4,10);
    let hour = now.getHours();

    /*  We don’t define April Fools as all-day April 1st,
        because too early in the morning no one is awake enough for pranks, 
        and after 3PM it’s honestly kinda tiresome.
     */
    return (date == "Apr 1" && hour >= 8 && hour <= 15);
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
