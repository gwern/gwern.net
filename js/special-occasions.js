/*********************/
/* SPECIAL OCCASIONS */
/*********************/

/******************************************************************************/
/*	Colorize the given elements (optionally, within the given container).

	The `colorizationSpecs` argument is an array of entries, each of which is a
	an array of three elements: a selector, a CSS variable name, and a color
	value. For example:

	colorizeElements([
		[ "li:nth-of-type(odd)", "--list-bullet", "#f00" ],
		[ "li:nth-of-type(even)", "--list-bullet", "#0f0" ],
		[ "hr", "--icon-image", "#f00" ]
	]);

	This will colorize the list bullets of odd-numbered list items to red, the
	list bullets of even-numbered list items to green, and the icon images of
	horizontal rules to red.

	(An optional fourth element in each entry specifies a dark mode property 
	 which should be disabled - i.e., set to `unset` - when an element is
	 colorized.)

	NOTE: In order for colorization to work properly, the given selectors must 
	have, as values of the given CSS variables, a CSS url() value containing an 
	inline-defined (and *not* base64-encoded) SVG image.
 */
function colorizeElements(colorizationSpecs, container = document.main) {
	for (let [ selector, cssVariableName, referenceColor, undesiredDarkModeProperty ] of colorizationSpecs) {
		let colorTransformFunction = (colorCode) => {
			return Color.processColorValue(colorCode, [ {
				type: Color.ColorTransform.COLORIZE,
				referenceColor: referenceColor
			} ]);
		};

		container.querySelectorAll(selector).forEach(element => {
			colorizeElement(element, cssVariableName, colorTransformFunction, undesiredDarkModeProperty);
		});
	}
}

/*********************************************************/
/*	Helper function. (See colorizeElements() for details.)
 */
function colorizeElement(element, cssVariableName, colorTransformFunction, undesiredDarkModeProperty) {
	if (element.style.getPropertyValue(cssVariableName) > "")
		uncolorizeElement(element, cssVariableName, undesiredDarkModeProperty);

	let svgSrc = getComputedStyle(element).getPropertyValue(cssVariableName).match(/url\(['"]data:image\/svg\+xml;utf8,(.+?)['"]\)/)[1];
	svgSrc = svgSrc.replaceAll("%23", "#").replaceAll("\\", "").replace(/(?<!href=)"(#[0-9A-Fa-f]+)"/g, 
		(match, colorCode) => {
			return `"${(colorTransformFunction(colorCode))}"`;
		});
	let svg = elementFromHTML(svgSrc);
	svg.setAttribute("fill", colorTransformFunction("#000"));
	svgSrc = svg.outerHTML.replaceAll("#", "%23");
	element.style.setProperty(cssVariableName, `url('data:image/svg+xml;utf8,${svgSrc}')`);

	if (undesiredDarkModeProperty)
		element.style.setProperty(undesiredDarkModeProperty, "unset");
}

/***********************************************************/
/*	Helper function. (See uncolorizeElements() for details.)
 */
function uncolorizeElement(element, cssVariableName, darkModeProperty) {
	element.style.removeProperty(cssVariableName);

	if (darkModeProperty)
		element.style.removeProperty(darkModeProperty);
}

/******************************************************************************/
/*	Undo the effects of colorizeElements(), for the given elements (optionally, 
	within the given container).

	(Each entry in the uncolorizationSpecs array need only have the selector 
	 and CSS variable name; no color need be specified.)

	(An optional third element in each entry specifies a dark mode property 
	 which should be un-disabled - i.e., the inline value of `unset` removed - 
	 when an element is un-colorized.)
 */
function uncolorizeElements(uncolorizationSpecs, container = document.main) {
	for (let [ selector, cssVariableName, darkModeProperty ] of uncolorizationSpecs) {
		container.querySelectorAll(selector).forEach(element => {
			uncolorizeElement(element, cssVariableName, darkModeProperty);
		});
	}
}

/**************************************************************************/
/*	Returns the source string for the <svg> container for an SVG page logo.
 */
function svgPageLogoContainerSourceForURL(logoURL) {
	return `<svg
			 class="logo-image visible"
			 viewBox="0 0 64 75"
			 ><use
			   href="${logoURL}#logo"
			   ></use></svg>`;
}

/***************************************************************************/
/*	Calls the provided page logo replacement function, when the page logo is
	available.
 */
function replacePageLogoWhenPossible(replaceLogo) {
    let logoSelector = "#sidebar .logo-image";
    let logoImage;
    if (logoImage = document.querySelector(logoSelector)) {
        replaceLogo(logoImage);
    } else {
        let observer = new MutationObserver((mutationsList, observer) => {
            if (   window.getSavedCount
            	&& window.getAssetPathname
            	&& window.versionedAssetURL
            	&& window.processAssetSequenceOptions
            	&& (logoImage = document.querySelector(logoSelector))) {
                observer.disconnect();
                replaceLogo(logoImage);
            }
        });
        observer.observe(document.documentElement, { childList: true, subtree: true });
    }
}

GW.allowedAssetSequencingModes = [
	"nextAfterSaved",
	"previousBeforeSaved",
	"nextAfterCurrent",
	"previousBeforeCurrent" 
];

/******************************************************************************/
/*  Inject a special page logo image of a specific type (‘halloween’,
    ‘christmas’, etc.). Directory structure and file naming for the
    specified logo type must match existing holiday logos.

    Available option fields:

	mode (string)
		May be “light” or “dark”, or null. Affects the scheme that determines
		the path and file name(s) expected for the logo image file(s). This
		option should be null if there is just the one logo image (or set of
		logo images) that is used in both light and dark mode; otherwise, the
		appropriate mode should be specified.

	identifier (string)
		If there are one or more numbered logo image files (for randomization
		purposes), but we wish to select a specific one, we may provide a
		numeric identifier string ("1", "14", etc.); the logo image file with
		that numeric identifier in the file name will be selected.

		NOTE: If this field is set, then the `sequence` and `randomize` fields 
		are ignored.

    randomize (boolean)
	sequence (string)
		[must be one of “nextAfterSaved”, “previousBeforeSaved”, 
		 “nextAfterCurrent”, or “previousBeforeCurrent”; any other value is 
		 equivalent to a null value]

		These two fields together define the sequencing behavior, over multiple 
		invocations of this function (either within a single page load session 
		or across multiple page loads), of the selection of a logo from multiple 
		available logos of the specified type.

		If neither `randomize` nor `sequence` are set, then only a single,
		specific logo image file will match the generated pattern (it will be
		an un-numbered file, if `identifier` is not set; or it will be a
		specific numbered one, if `identifier` is set).

		If `randomize` is set to true but `sequence` is not set, then a logo
		image file will be selected at random out of the available logo image
		files that match the specified criteria (type, mode, scale).	

		If `sequence` is set to either “nextAfterCurrent” or 
		“previousBeforeCurrent”, then the next logo image file after the 
		currently set logo, or the previous logo image file before the currently
		set logo (respectively) will be selected. If the currently set logo is
		not one of the logo image files that match the specified criteria, then
		the first or the last logo image files (respectively) out of the 
		available matching ones will be selected. (The `randomize` field is
		ignored in this case.)

		If `sequence` is set to either “nextAfterSaved” or 
		“previousBeforeSaved”, then the saved index of the previously selected
		logo is retrieved from localStorage.

		If there is no saved index, and `randomize` is set to true, then a
		starting index is generated randomly, in the range [1,1E6].

		Otherwise, if `sequence` is set to “nextAfterSaved”, the saved index
		is incremented by 1 (or else set to 1, if there is no saved index yet);
		if `sequence` is set to “previousBeforeSaved”, the saved index is 
		decremented by 1 (or else set to 0, if there is no saved index yet).

		(In either case, the new index is saved to localStorage.)

		That logo image will be then be selected which is the i’th in the set
		of logo image files that match the specified criteria, where i is equal
		to the new sequence index modulo the number of matching logo image 
		files.

	link (URL)
		Points the logo link to the specified URL. (If this field is not set,
		the logo link retains its current target, whatever that may be. Note 
		that by default, the logo link starts out pointing to /index.)
 */
function injectSpecialPageLogo(logoType, options) {
	options = Object.assign({
		mode: null,
		identifier: null,
		randomize: false,
		sequence: null,
		link: null
	}, options);

	//	Identifier string (empty, or hyphen plus a number, or “-%R”).
    let logoIdentifierRegexpString = ``;
    if (options.identifier) {
    	logoIdentifierRegexpString = `-${options.identifier}`;
    } else if (   options.randomize == true
    		   || GW.allowedAssetSequencingModes.includes(options.sequence)) {
    	logoIdentifierRegexpString = `-%R`;
    }

	/*	Bitmap files come in several scales (for different pixel densities of
		display); SVGs are singular.
	 */
    let scale = valMinMax(Math.ceil(window.devicePixelRatio), 1, 3);
    let fileFormatRegexpSuffix = `(\\.svg|-small-${scale}x\\.(png|jpg|webp))$`;

	/*	File name pattern further depends on whether we have separate light
		and dark logos of this sort.
	 */
	let logoPathnamePattern = `/static/img/logo/${logoType}/`
							+ (options.mode
							   ? `${options.mode}/logo-${logoType}-${options.mode}`
							   : `logo-${logoType}`)
							+ logoIdentifierRegexpString
							+ fileFormatRegexpSuffix;

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

    /*  Note that getAssetPathname(), versionedAssetURL(), and 
    	processAssetSequenceOptions() are defined in misc.js, and so cannot be 
    	called prior to this.
     */
    replacePageLogoWhenPossible(logoImage => {
		//	Get enclosing link, in case we have to modify it.
		let logoLink = logoImage.closest("a");

        //  Get new logo URL (specified, random, or sequenced).
		let logoPathname = getAssetPathname(logoPathnamePattern, processAssetSequenceOptions(options, {
			currentAssetURL: URLFromString(   logoImage.querySelector("use")?.getAttribute("href") 
										   ?? logoImage.querySelector("img")?.src),
			assetSavedIndexKey: `logo-sequence-index-${logoType}`
		}));

        let versionedLogoURL = versionedAssetURL(logoPathname);

		if (logoPathname.endsWith(".svg")) {
            //  Inject inline SVG.
            logoImage.replaceWith(elementFromHTML(svgPageLogoContainerSourceForURL(versionedLogoURL)));
        } else {
            //  Create new image element and wrapper.
            let imageWrapper = newElement("SPAN", {
                class: "logo-image"
            });
            imageWrapper.appendChild(newElement("IMG", {
                class: "figure-not",
                src: versionedLogoURL.pathname + versionedLogoURL.search
            }));

            //  Inject wrapped image.
            logoImage.replaceWith(imageWrapper);
        }

		//	Point the logo link at the provided URL.
		if (options.link)
			logoLink.href = options.link;

        //  Brighten logo; fade (over 1 second) after 20 seconds.
        brightenLogoTemporarily(20 * 1000, 1000);
    });
}

/*******************************************************************/
/*	Resets the saved sequence index of the specified page logo type.

	(See comment for injectSpecialPageLogo() for more information.)
 */
function resetPageLogoSequenceIndex(logoType) {
	localStorage.removeItem(`logo-sequence-index-${logoType}`);
}

/******************************************/
/*	Reset the page logo to the default one.
 */
function resetPageLogo() {
    /*  Note that versionedAssetURL() is defined in misc.js, and so cannot be 
    	called too early.
     */
    replacePageLogoWhenPossible(logoImage => {
		//	Get enclosing link.
		let logoLink = logoImage.closest("a");

		//	Get versioned logo URL.
        let versionedLogoURL = versionedAssetURL("/static/img/logo/logo-smooth.svg");

		//	Inject inline SVG.
		logoImage.replaceWith(elementFromHTML(svgPageLogoContainerSourceForURL(versionedLogoURL)));

		//	Point the logo link back to the index page.
		logoLink.href = "/index";
    });
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
        let newLogoLink = URLFromString("/dropcap#halloween");
        doWhenMatchMedia(matchMedia("(min-width: 1180px)"), "GW.setHalloweenPageLogoForViewportWidth",
           (mediaQuery) => {
        	injectSpecialPageLogo("halloween", {
        		mode: "dark", 
        		sequence: "previousBeforeSaved",
        		link: newLogoLink
        	});
        }, (mediaQuery) => {
			injectSpecialPageLogo("halloween", {
				mode: "dark",
				identifier: "1",
				link: newLogoLink
			});
        }, (mediaQuery) => {
        	resetPageLogo();
        });
      }, () => {
        document.body.classList.remove("special-halloween-dark", "special-halloween-light");
        cancelDoWhenMatchMedia("GW.setHalloweenPageLogoForViewportWidth");
		resetPageLogoSequenceIndex("halloween");
      } ],
    [ "christmas", isItChristmas, () => {
        //  Different special styles for light and dark mode.
        document.body.classList.remove("special-christmas-dark", "special-christmas-light");
        let specialClass = DarkMode.computedMode() == "light"
                           ? "special-christmas-light"
                           : "special-christmas-dark";
        document.body.classList.add(specialClass);

        //  Replace logo.
        let newLogoLink = URLFromString("/dropcap#christmas");
        doWhenMatchMedia(matchMedia(""), "GW.setChristmasPageLogo",
           (mediaQuery) => {
			injectSpecialPageLogo("christmas", {
				mode: DarkMode.computedMode(),
				sequence: "previousBeforeSaved",
				link: newLogoLink
			});
			doWhenPageLoaded(() => {
				colorizeElements([
					[ "ul > li:nth-of-type(odd)", "--list-bullet", "#f00", "--list-bullet-dark-mode-invert-filter" ],
					[ "ul > li:nth-of-type(even)", "--list-bullet", "#0f0", "--list-bullet-dark-mode-invert-filter" ],
					[ "div[class^='horizontal-rule']:nth-of-type(odd) hr", "--icon-image", "#f00", "--icon-dark-mode-invert-filter" ],
					[ "div[class^='horizontal-rule']:nth-of-type(even) hr", "--icon-image", "#0b0", "--icon-dark-mode-invert-filter" ],
					[ "#x-of-the-day", "--ornament-image-left", "#f00", "--ornament-dark-mode-invert-filter" ],
					[ "#x-of-the-day", "--ornament-image-right", "#f00", "--ornament-dark-mode-invert-filter" ],
					[ "#footer-decoration-container .footer-logo", "--logo-image", "#c00", "--logo-image-dark-mode-invert-filter" ]
				]);
				doWhenElementExists(() => {
					colorizeElements([
						[ "#x-of-the-day .site-of-the-day blockquote", "--background-image", "#126512" ],
						[ "#x-of-the-day .site-of-the-day blockquote", "--background-image-sides", "#126512" ]
					]);
				}, "#x-of-the-day .site-of-the-day");
			});
        }, null, (mediaQuery) => {        	
			resetPageLogo();
			doWhenPageLoaded(() => {
				uncolorizeElements([
					[ "ul > li", "--list-bullet", "--list-bullet-dark-mode-invert-filter" ],
					[ "hr", "--icon-image", "--icon-dark-mode-invert-filter" ],
					[ "#x-of-the-day", "--ornament-image-left", "--ornament-dark-mode-invert-filter" ],
					[ "#x-of-the-day", "--ornament-image-right", "--ornament-dark-mode-invert-filter" ],
					[ "#x-of-the-day .site-of-the-day blockquote", "--background-image" ],
					[ "#x-of-the-day .site-of-the-day blockquote", "--background-image-sides" ]
					[ "#footer-decoration-container .footer-logo", "--logo-image", "--logo-image-dark-mode-invert-filter" ]
				]);
				doWhenElementExists(() => {
					uncolorizeElements([
						[ "#x-of-the-day .site-of-the-day blockquote", "--background-image" ],
						[ "#x-of-the-day .site-of-the-day blockquote", "--background-image-sides" ]
					]);
				}, "#x-of-the-day .site-of-the-day");
			});
        });
      }, () => {
        document.body.classList.remove("special-christmas-dark", "special-christmas-light");
        cancelDoWhenMatchMedia("GW.setChristmasPageLogo");
		resetPageLogoSequenceIndex("christmas");
      } ],
    [ "april-fools", isItAprilFools, () => {
        document.body.classList.add("special-april-fools");
        // TODO: no April Fools logos or dropcaps.. for now. Maybe 2025?

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
/*  Debugging function; pass a special occasion identifier string (e.g.
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
/*  Returns true if it’s Halloween right now.

    Test page: </lorem-halloween>
*/
function isItHalloween() {
	//	The test page is Halloween Town.
	if (   document.body.classList.contains(GW.specialOccasionTestPageNamePrefix + "halloween")
		|| localStorage.getItem(GW.specialOccasionTestLocalStorageKeyPrefix + "halloween") == "true")
		return true;

	// 	Match languages from Halloween-celebrating regions (which has gone global <https://en.wikipedia.org/wiki/Halloween#Geography>)
	//	prefix to match language groups like ‘en’, ‘en-US’, ‘en-GB’, ‘en-AU’...
	//	Primary: en,ga,gd (English/Irish/Scots) | Moderate: de,nl,fr,es (Europe) + ja,ko (Asia)
	const halloweenLangs = new Set(['en','ga','gd','de','nl','fr','es','ja','ko']);

	let langCode = (window.navigator.userLanguage ?? window.navigator.language).slice(0, 2).toLowerCase();
	if (halloweenLangs.has(langCode)) {
		let now = new Date();
		let date = now.toString().slice(4,10);
		let hour = now.getHours();

		/*  It is a sin to celebrate Halloween while there is daylight; however,
			calculating local sunset or local ambient light is too hard (where
			would we even get that geolocation or light sensor data from‽), so
			we will simply define 'night' as ≥6PM and <6AM.
		 */
		return (date == "Oct 31" && hour >= 18) || (date == "Nov 01" && hour < 6);
	} else {
	   return false;
	}
}

/********************************************/
/*  Returns true if it’s Christmas right now.

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
/*  Returns true if it’s April Fool’s Day right now.

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
/*  Returns true if it’s Easter right now.

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
/*  Applies or removes special-occasion-related CSS classes to the <body>
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
/*  Apply special occasion classes (if need be) when the <body> element is
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
