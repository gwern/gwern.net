// darkmode.js: Javascript library for controlling page appearance, toggling between regular white and 'dark mode'
// Author: Said Achmiz
// Date: 2020-03-20
// When:  Time-stamp: "2020-10-02 10:29:31 gwern"
// license: PD

/* Experimental 'dark mode': Mac OS (Safari) lets users specify via an OS widget 'dark'/'light' to make everything appear */
/* bright-white or darker (eg for darker at evening to avoid straining eyes & disrupting circadian rhyhms); this then is */
/* exposed by Safari as a CSS variable which can be selected on. This is also currently supported by Firefox weakly as an */
/* about:config variable. Hypothetically, iOS in the future might use its camera or the clock to set 'dark mode' */
/* automatically. https://drafts.csswg.org/mediaqueries-5/#prefers-color-scheme */
/* https://webkit.org/blog/8718/new-webkit-features-in-safari-12-1/ */
/* https://developer.mozilla.org/en-US/docs/Web/CSS/@media/prefers-color-scheme
/* Images are handled specially: images are *not* inverted/negated by default; images with a special class, '.invertible-auto' (set on images by automated tools like ImageMagick scripts counting colors) or '.invertible' (set manually), will be inverted. (This is intended to allow inversion of images which would invert well, like statistical graphs or charts, which are typically black-on-white, and are much more pleasant to read in dark mode when inverted to white-on-black.) Inversion is removed on image hover or image-focus.js click-to-zoom. */

/* Because many users do not have access to a browser/OS which explicitly supports dark mode, cannot modify the browser/OS setting without undesired side-effects, wish to opt in only for specific websites, or simply forget that they turned on dark mode & dislike it, we make dark mode controllable by providing a widget at the top of the page. */

/* For gwern.net, the default white-black scheme is 'light', and it can be flipped to a 'dark' scheme fairly easily by inverting it; the main visual problem is */
/* that blockquotes appear to become much harder to see & image-focus.js doesn't work well without additional tweaks. */

/* Known bugs: 'flash of white' on initial load until darkmode.js runs & reads user settings; browser implementations of invert filters are very slow, leading to 'janky' slow rendering on scrolling */

/****************/
/* MISC HELPERS */
/****************/

/*      Given an HTML string, creates an element from that HTML, adds it to
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

if (typeof window.GW == "undefined")
        window.GW = { };
GW.temp = { };

if (GW.mediaQueries == null)
    GW.mediaQueries = { };
GW.mediaQueries.mobileNarrow = matchMedia("(max-width: 520px)");
GW.mediaQueries.mobileWide = matchMedia("(max-width: 900px)");
GW.mediaQueries.mobileMax = matchMedia("(max-width: 960px)");
GW.mediaQueries.hover = matchMedia("only screen and (hover: hover) and (pointer: fine)");
GW.mediaQueries.systemDarkModeActive = matchMedia("(prefers-color-scheme: dark)");

GW.modeOptions = [
    [ 'auto', 'Auto', 'Set light or dark mode automatically, according to system-wide setting (Win: Start→Personalization→Colors; Mac: Apple→System-Preferences→General→Appearance; iOS: Settings→Display-and-Brightness; Android: Settings→Display' ],
    [ 'light', 'Light', 'Light mode at all times override (black-on-white)' ],
    [ 'dark', 'Dark', 'Dark mode at all times override (inverted: white-on-black)' ]
];
GW.modeStyles = `
    :root {
        --GW-blockquote-background-color: #ddd
    }
    body::before,
    div#popup-container,
    body > * {
        filter: invert(100%) brightness(97%);
    }
    body::before {
        content: '';
        width: 100vw;
        height: 100%;
        position: fixed;
        left: 0;
        top: 0;
        background-color: #fff;
        z-index: -1
    }
    img:not(.invertible-auto):not(.invertible),
    #image-focus-overlay,
    #markdownBody figure img:hover,
    video {
        filter: invert(100%);
    }
    #markdownBody, #mode-selector button {
        text-shadow: 0 0 0 #000
    }
    article > :not(#TOC) a:link {
        text-shadow:
                 0      0 #777,
             .03em      0 #fff,
            -.03em      0 #fff,
                 0  .03em #fff,
                 0 -.03em #fff,
             .06em      0 #fff,
            -.06em      0 #fff,
             .09em      0 #fff,
            -.09em      0 #fff,
             .12em      0 #fff,
            -.12em      0 #fff,
             .15em      0 #fff,
            -.15em      0 #fff
    }
    article > :not(#TOC) blockquote a:link {
        text-shadow:
                 0      0 #777,
             .03em      0 var(--GW-blockquote-background-color),
            -.03em      0 var(--GW-blockquote-background-color),
                 0  .03em var(--GW-blockquote-background-color),
                 0 -.03em var(--GW-blockquote-background-color),
             .06em      0 var(--GW-blockquote-background-color),
            -.06em      0 var(--GW-blockquote-background-color),
             .09em      0 var(--GW-blockquote-background-color),
            -.09em      0 var(--GW-blockquote-background-color),
             .12em      0 var(--GW-blockquote-background-color),
            -.12em      0 var(--GW-blockquote-background-color),
             .15em      0 var(--GW-blockquote-background-color),
            -.15em      0 var(--GW-blockquote-background-color)
    }
    #logo img {
        filter: none;
    }
    #mode-selector {
        opacity: 0.7;
    }
    #mode-selector:hover {
        background-color: #fff;
    }
`;

/****************/
/* DEBUG OUTPUT */
/****************/

function GWLog (string) {
    if (GW.loggingEnabled || localStorage.getItem("logging-enabled") == "true")
        console.log(string);
}

/***********/
/* HELPERS */
/***********/

/*  Run the given function immediately if the page is already loaded, or add
    a listener to run it as soon as the page loads.
    */
function doWhenPageLoaded(f) {
    if (document.readyState == "complete")
        f();
    else
        window.addEventListener("load", f);
}

/*  Adds an event listener to a button (or other clickable element), attaching
    it to both "click" and "keyup" events (for use with keyboard navigation).
    Optionally also attaches the listener to the 'mousedown' event, making the
    element activate on mouse down instead of mouse up.
    */
Element.prototype.addActivateEvent = function(func, includeMouseDown) {
    let ael = this.activateEventListener = (event) => { if (event.button === 0 || event.key === ' ') func(event) };
    if (includeMouseDown) this.addEventListener("mousedown", ael);
    this.addEventListener("click", ael);
    this.addEventListener("keyup", ael);
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
    if (typeof name != "undefined")
        GW[name] = wrapper;
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
            GWLog(`Canceling media query “${name}”`);

            if (whenCanceledDo != null)
                whenCanceledDo(mediaQuery);
        } else {
            let matches = (typeof event == "undefined") ? mediaQuery.matches : event.matches;

            GWLog(`Media query “${name}” triggered (matches: ${matches ? "YES" : "NO"})`);

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

/******************/
/* MODE SELECTION */
/******************/

function injectModeSelector() {
    GWLog("injectModeSelector");

    // Get saved mode setting (or default).
    let currentMode = localStorage.getItem("selected-mode") || 'auto';

    // Inject the mode selector widget and activate buttons.
    let modeSelector = addUIElement(
        "<div id='mode-selector'>" +
        String.prototype.concat.apply("", GW.modeOptions.map(modeOption => {
            let [ name, label, desc ] = modeOption;
            let selected = (name == currentMode ? ' selected' : '');
            let disabled = (name == currentMode ? ' disabled' : '');
            return `<button type='button' class='select-mode-${name}${selected}'${disabled} tabindex='-1' data-name='${name}' title='${desc}'>${label}</button>`})) +
        "</div>");

    modeSelector.querySelectorAll("button").forEach(button => {
        button.addActivateEvent(GW.modeSelectButtonClicked = (event) => {
            GWLog("GW.modeSelectButtonClicked");

            // Determine which setting was chosen (i.e., which button was clicked).
            let selectedMode = event.target.dataset.name;

            // Save the new setting.
            if (selectedMode == "auto") localStorage.removeItem("selected-mode");
            else localStorage.setItem("selected-mode", selectedMode);

            // Actually change the mode.
            setMode(selectedMode);
        });
    });

    document.querySelector("head").insertAdjacentHTML("beforeend", `<style id='mode-selector-styles'>
    #mode-selector {
        position: absolute;
        right: 3px;
        display: flex;
        background-color: #fff;
        padding: 0.125em 0.25em;
        border: 3px solid transparent;
        opacity: 0.3;
        transition:
            opacity 2s ease;
    }
    #mode-selector.hidden {
        opacity: 0;
    }
    #mode-selector:hover {
        transition: none;
        opacity: 1.0;
        border: 3px double #aaa;
    }
    #mode-selector button {
        -moz-appearance: none;
        appearance: none;
        border: none;
        background-color: transparent;
        padding: 0.5em;
        margin: 0;
        line-height: 1;
        font-family: Lucida Sans Unicode, Source Sans Pro, Helvetica, Trebuchet MS, sans-serif;
        font-size: 0.75rem;
        text-align: center;
        color: #777;
        position: relative;
    }
    #mode-selector button:hover,
    #mode-selector button.selected {
        box-shadow:
            0 2px 0 6px #fff inset,
            0 1px 0 6px currentColor inset;
    }
    #mode-selector button:not(:disabled):hover {
        color: #000;
        cursor: pointer;
    }
    #mode-selector button:not(:disabled):active {
        transform: translateY(2px);
        box-shadow:
            0 0px 0 6px #fff inset,
            0 -1px 0 6px currentColor inset;
    }
    #mode-selector button.active:not(:hover)::after {
        content: "";
        position: absolute;
        bottom: 0.25em;
        left: 0;
        right: 0;
        border-bottom: 1px dotted currentColor;
        width: calc(100% - 12px);
        margin: auto;
    }
    </style>`);

    document.querySelector("head").insertAdjacentHTML("beforeend", `<style id='mode-styles'></style>`);

    setMode(currentMode);

    // We pre-query the relevant elements, so we don’t have to run queryAll on
    // every firing of the scroll listener.
    GW.scrollState = {
        "lastScrollTop":                    window.pageYOffset || document.documentElement.scrollTop,
        "unbrokenDownScrollDistance":       0,
        "unbrokenUpScrollDistance":         0,
        "modeSelector":                     document.querySelectorAll("#mode-selector"),
    };
    addScrollListener(updateModeSelectorVisibility, "updateModeSelectorVisibilityScrollListener");
    GW.scrollState.modeSelector[0].addEventListener("mouseover", () => { showModeSelector(); });
    doWhenMatchMedia(GW.mediaQueries.systemDarkModeActive, "updateModeSelectorStateForSystemDarkMode", () => { updateModeSelectorState(); });
}

/*  Show/hide the mode selector in response to scrolling.

    Called by the ‘updateModeSelectorVisibilityScrollListener’ scroll listener.
    */
function updateModeSelectorVisibility(event) {
    GWLog("updateModeSelectorVisibility");

    let newScrollTop = window.pageYOffset || document.documentElement.scrollTop;
    GW.scrollState.unbrokenDownScrollDistance = (newScrollTop > GW.scrollState.lastScrollTop) ?
                                                        (GW.scrollState.unbrokenDownScrollDistance + newScrollTop - GW.scrollState.lastScrollTop) :
                                                        0;
    GW.scrollState.unbrokenUpScrollDistance = (newScrollTop < GW.scrollState.lastScrollTop) ?
                                                     (GW.scrollState.unbrokenUpScrollDistance + GW.scrollState.lastScrollTop - newScrollTop) :
                                                     0;
    GW.scrollState.lastScrollTop = newScrollTop;

    // Hide mode selector when scrolling a full page down.
    if (GW.scrollState.unbrokenDownScrollDistance > window.innerHeight) {
        hideModeSelector();
    }

    // On desktop, show mode selector when scrolling to top of page,
    // or a full page up.
    // On mobile, show mode selector on ANY scroll up.
    if (GW.mediaQueries.mobileNarrow.matches) {
        if (GW.scrollState.unbrokenUpScrollDistance > 0 || GW.scrollState.lastScrollTop <= 0)
            showModeSelector();
    } else if (   GW.scrollState.unbrokenUpScrollDistance > window.innerHeight
               || GW.scrollState.lastScrollTop == 0) {
        showModeSelector();
    }
}

function hideModeSelector() {
    GWLog("hideModeSelector");

    GW.scrollState.modeSelector[0].classList.add("hidden");
}

function showModeSelector() {
    GWLog("showModeSelector");

    GW.scrollState.modeSelector[0].classList.remove("hidden");
}

/*  Update the states of the mode selector buttons.
    */
function updateModeSelectorState() {
    // Get saved mode setting (or default).
    let currentMode = localStorage.getItem("selected-mode") || 'auto';

    // Clear current buttons state.
    let modeSelector = document.querySelector("#mode-selector");
    modeSelector.childNodes.forEach(button => {
        button.classList.remove("active", "selected");
        button.disabled = false;
    });

    // Set the correct button to be selected.
    modeSelector.querySelectorAll(`.select-mode-${currentMode}`).forEach(button => {
        button.classList.add("selected");
        button.disabled = true;
    });

    // Ensure the right button (light or dark) has the “currently active”
    // indicator, if the current mode is ‘auto’.
    if (currentMode == "auto") {
        if (GW.mediaQueries.systemDarkModeActive.matches)
            modeSelector.querySelector(".select-mode-dark").classList.add("active");
        else
            modeSelector.querySelector(".select-mode-light").classList.add("active");
    }
}

/*  Set specified color mode (auto, light, dark).
    */
function setMode(modeOption) {
    GWLog("setMode");

    // Inject the appropriate styles.
    let modeStyles = document.querySelector("#mode-styles");
    if (modeOption == 'auto') {
        modeStyles.innerHTML = `@media (prefers-color-scheme:dark) {${GW.modeStyles}}`;
    } else if (modeOption == 'dark') {
        modeStyles.innerHTML = GW.modeStyles;
    } else {
        modeStyles.innerHTML = "";
    }

    // Update selector state.
    updateModeSelectorState();
}

/******************/
/* INITIALIZATION */
/******************/

doWhenPageLoaded(() => {
    injectModeSelector();
});
