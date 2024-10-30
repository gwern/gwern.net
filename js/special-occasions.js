/*********************/
/* SPECIAL OCCASIONS */
/*********************/

/*****************************************************************************/
/*  Inject a special page logo image of a specific type (‘halloween’,
    ‘christmas’, etc.). Directory structure and file naming for the
    specified logo type must match existing holiday logos.

	Available option fields:

	randomize (boolean)
		If set to `true`, selects one of the multiple available logos of the
		specified type, from image files named according to a scheme that 
		includes a number in the name. Otherwise, selects the single, 
		deterministically named image file (which is named according to a 
		scheme determined by the type and mode option).

	mode (string)
		May be “light” or “dark”, or null. Affects the scheme that determines
		the path and file name(s) expected for the logo image file(s). This
		option should be null if there is just the one logo image (or set of
		logo images) that is used in both light and dark mode; otherwise, the
		appropriate mode should be specified.
 */
function injectSpecialPageLogo(logoType, options) {
	options = Object.assign({
		mode: null,
		randomize: false
	}, options);

    let scale = valMinMax(Math.ceil(window.devicePixelRatio), 1, 3);

    let logoPathname;
    let fileFormatRegexpSuffix = `(\\.svg|-small-${scale}x\\.(png|jpg|webp))`;
    if (options.randomize) {
        logoPathname = options.mode
                       ? `/static/img/logo/${logoType}/${options.mode}/logo-${logoType}-${options.mode}-%R${fileFormatRegexpSuffix}$`
                       : `/static/img/logo/${logoType}/logo-${logoType}-%R${fileFormatRegexpSuffix}$`;
    } else {
        logoPathname = options.mode
                       ? `/static/img/logo/${logoType}/${options.mode}/logo-${logoType}-${options.mode}${fileFormatRegexpSuffix}$`
                       : `/static/img/logo/${logoType}/logo-${logoType}${fileFormatRegexpSuffix}$`;
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
    };

    /*  Note that randomAsset() and versionedAssetURL() are defined in misc.js,
        and so cannot be called prior to this.
     */
    let replaceLogo = (logoImage) => {
        //  Get new logo URL (random, if need be).
        if (options.randomize)
            logoPathname = randomAsset(logoPathname);

        let versionedLogoURL = versionedAssetURL(logoPathname);

		if (logoPathname.endsWith(".svg")) {
			//	Create new <svg> element.
			let svgContainer = elementFromHTML(`<svg 
												 class="logo-image visible" 
												 viewBox="0 0 64 75"
												 ><use 
												   href="${versionedLogoURL}#logo"
												   ></use></svg>`);

			//	Inject inline SVG.
			logoImage.replaceWith(svgContainer);
		} else {
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
		}

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

/*****************/
/*  Configuration.
 */

GW.specialOccasionTestLocalStorageKeyPrefix = "special-occasion-test-";

GW.specialOccasionTestPageNamePrefix = "test-";

/*  If a special function is provided to apply classes, one should also be
    provided to remove those classes. (See the ‘halloween’ entry for example.)
 */
GW.specialOccasions = [
    [ "halloween", isItHalloween, () => {
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
    [ "christmas", isItChristmas, () => {
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
    [ "april-fools", isItAprilFools, () => {
        document.body.classList.add("special-april-fools");

        /*  Turn off the funny after half a minute (the blackletter joke has
            worn old by then).
         */
        let jokeDurationSeconds = 30;
        setTimeout(() => {
            document.body.classList.remove("special-april-fools");
        }, jokeDurationSeconds * 1000);
      }, () => {
        document.body.classList.remove("special-april-fools");
      } ],
    [ "easter", isItEaster, () => {
        document.body.classList.add("special-easter");
        //  Replace logo.
//         injectSpecialPageLogo("easter");
      }, () => {
        document.body.classList.remove("special-easter");
      } ],
];

/******************************************************************************/
/*	Debugging function; pass a special occasion identifier string (e.g. 
	“halloween”, “christmas”, “april-fools”, etc.), and either `true` to enable 
	testing of that special occasion, or `false` (or nothing) to disable 
	testing of that special occasion. (Calling this function with no arguments, 
	or null for the first argument, disables all special occasion testing.)

	When testing is enabled for a special occasion, the special occasion code 
	will behave as if that special occasion is currently taking place (and any 
	special-occasion-specific styling or other modifications will be applied).

	NOTE: Testing for multiple special occasions may be enabled simultaneously, 
	but this results in undefined behavior.
 */
function toggleSpecialOccasionTest(specialOccasionName = null, enable = false) {
	if (specialOccasionName == null) {
		let activeSpecialOccasionTestKeys = [ ];
		for (let i = 0; i < localStorage.length; i++) {
			let localStorageKey = localStorage.key(i);
			if (localStorageKey.startsWith(GW.specialOccasionTestLocalStorageKeyPrefix))
				activeSpecialOccasionTestKeys.push(localStorageKey);
		}
		activeSpecialOccasionTestKeys.forEach(key => {
			localStorage.removeItem(key);
		});

		return;
	}

	let specialOccasionTestKey = GW.specialOccasionTestLocalStorageKeyPrefix + specialOccasionName;
	if (enable) {
		localStorage.setItem(specialOccasionTestKey, true)
	} else {
		localStorage.removeItem(specialOccasionTestKey);
	}
}

/********************************************/
/*	Returns true if it’s Halloween right now.

	Test page: </lorem-halloween>
*/
function isItHalloween() {
   // The test page is Halloween Town.
   if (   document.body.classList.contains(GW.specialOccasionTestPageNamePrefix + "halloween")
       || localStorage.getItem(GW.specialOccasionTestLocalStorageKeyPrefix + "halloween") == "true")
       return true;

   // Match languages from Halloween-celebrating regions (which has gone global <https://en.wikipedia.org/wiki/Halloween#Geography>)
   let language = window.navigator.userLanguage || window.navigator.language;
   let langCode = language.slice(0, 2).toLowerCase();  // prefix to match language groups like ‘en’, ‘en-US’, ‘en-GB’, ‘en-AU’...
   // Primary: en,ga,gd (English/Irish/Scots) | Moderate: de,nl,fr,es (Europe) + ja,ko (Asia)
   const halloweenLangs = new Set(['en','ga','gd','de','nl','fr','es','ja','ko']);

   if (halloweenLangs.has(langCode)) {
       let now = new Date();
       let date = now.toString().slice(4,10);
       let hour = now.getHours();
       /*  It is a sin to celebrate Halloween while there is daylight; however,
           calculating local sunset or local ambient light is too hard (where
           would we even get that geolocation or light sensor data from‽), so
           we will simply define 'night' as ≥6PM and <6AM. */
       return (date == "Oct 31" && hour >= 18) || (date == "Nov 01" && hour < 6);
   } else {
       return false;
   }
}

/********************************************/
/*	Returns true if it’s Christmas right now.

	Test page: </lorem-christmas>
*/
function isItChristmas() {
    //  The test page is Christmas Town.
    if (   document.body.classList.contains(GW.specialOccasionTestPageNamePrefix + "christmas")
        || localStorage.getItem(GW.specialOccasionTestLocalStorageKeyPrefix + "christmas") == "true")
        return true;

    let now = new Date();
    let date = now.toString().slice(4,10);
    let hour = now.getHours();

    /*  Christmas = Christmas Eve + all Christmas Day; Christmas Eve starts in
        the evening, so again ≥6PM. */
    return (date == "Dec 24" && hour >= 18) || (date == "Dec 25");
}

/***************************************************/
/*	Returns true if it’s April Fool’s Day right now.

	Test page: </lorem-april-fools>
*/
function isItAprilFools() {
    //  The test page is blackletterFraktur-town.
    if (   document.body.classList.contains(GW.specialOccasionTestPageNamePrefix + "april-fools")
        || localStorage.getItem(GW.specialOccasionTestLocalStorageKeyPrefix + "april-fools") == "true")
        return true;

    let now = new Date();
    let date = now.toString().slice(4,10);
    let hour = now.getHours();

    /*  We don’t define April Fools as all-day April 1st,
        because too early in the morning no one is awake enough for pranks,
        and after 3PM it’s honestly kinda tiresome. */
    return (date == "Apr 01" && hour >= 8 && hour <= 15);
}

/*****************************************/
/*	Returns true if it’s Easter right now.

	Test page: </lorem-easter>
*/
function isItEaster() {
    if (   document.body.classList.contains(GW.specialOccasionTestPageNamePrefix + "easter")
        || localStorage.getItem(GW.specialOccasionTestLocalStorageKeyPrefix + "easter") == "true")
        return true;

    /*  Easter dates 2024–2050 from <https://www.assa.org.au/edm/#List20>;
        the JS computus (<https://en.wikipedia.org/wiki/Date_of_Easter>)
        algorithms on StackOverflow etc are squirrely enough I’d rather just
        hardwire it. Should I need to update it in 2051, that’s fine...
     */
    let easterDates = [
        "2024-03-31", "2025-04-20", "2026-04-05", "2027-03-28", "2028-04-16",
        "2029-04-01", "2030-04-21", "2031-04-13", "2032-03-28", "2033-04-17",
        "2034-04-09", "2035-03-25", "2036-04-13", "2037-04-05", "2038-04-25",
        "2039-04-10", "2040-04-01", "2041-04-21", "2042-04-06", "2043-03-29",
        "2044-04-17", "2045-04-09", "2046-03-25", "2047-04-14", "2048-04-05",
        "2049-04-18", "2050-04-10"
    ];

    let today = new Date().toISOString().slice(0, 10); // format: `YYYY-MM-DD`

    return easterDates.includes(today);
}

/******************************************************************************/
/*	Applies or removes special-occasion-related CSS classes to the <body>
	element.

	For each special occasion defined in GW.specialOccasions, calls the 
	specified testing function (e.g., isItHalloween()).

	If the test returns true, then calls the specified application function if 
	one is provided; otherwise just adds to <body> a class 
	`special-` + <the name of the special occasion> (e.g., “halloween”).

	If the test returns false, then calls the specified removal function if
	one is provided. If no such function is provided, AND there is no 
	application function either (and thus the application consisted merely
	of the default action of adding the default class `special-WHATEVER`), now 
	simply removes that default class.

	NOTE: If an application function is provided, but no corresponding removal
	function is provided, then this function will do nothing when an active
	special occasion mode is toggled off! That is why it’s important to provide
	a removal function when providing an application function (see the existing
	entries in GW.specialOccasions for examples).
*/
function applySpecialOccasionClasses() {
    for (let occasion of GW.specialOccasions) {
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

/***************************************************************************/
/*	Apply special occasion classes (if need be) when the <body> element is 
	created; update them (applying or removing, as appropriate) whenever the 
	mode changes.
 */
doWhenBodyExists(() => {
    applySpecialOccasionClasses();
    GW.notificationCenter.addHandlerForEvent("DarkMode.computedModeDidChange", (info) => {
        applySpecialOccasionClasses();
    });
    DarkMode.setMode();
});
