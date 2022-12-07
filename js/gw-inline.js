/*  Create global ‘GW’ object, if need be.
 */
if (typeof window.GW == "undefined")
    window.GW = { };

/*****************/
/* MEDIA QUERIES */
/*****************/

GW.mediaQueries = {
    mobileWidth:           matchMedia("(max-width: 649px)"),
    systemDarkModeActive:  matchMedia("(prefers-color-scheme: dark)"),
    hoverAvailable:        matchMedia("only screen and (hover: hover) and (pointer: fine)"),
    portraitOrientation:   matchMedia("(orientation: portrait)"),
    printView:             matchMedia("print")
};

GW.isMobile = () => {
    /*  We consider a client to be mobile if one of two conditions obtain:
        1. JavaScript detects touch capability, AND viewport is narrow; or,
        2. CSS does NOT detect hover capability.
     */
    return (   (   ("ontouchstart" in document.documentElement)
                && GW.mediaQueries.mobileWidth.matches)
            || !GW.mediaQueries.hoverAvailable.matches);
};

GW.isFirefox = () => {
    return (navigator.userAgent.indexOf("Firefox") > 0);
};

GW.isX11 = () => {
    return (navigator.userAgent.indexOf("X11") > 0);
};


/********************/
/* DEBUGGING OUTPUT */
/********************/

GW.dateTimeFormat = new Intl.DateTimeFormat([], { hour12: false, hour: "numeric", minute: "numeric", second: "numeric" });

function GWTimestamp() {
    let time = Date.now();
    let ms = `${(time % 1000)}`.padStart(3,'0');
    let timestamp = `${GW.dateTimeFormat.format(time)}.${ms}`;

    return timestamp;
}

GW.logLevel = localStorage.getItem("gw-log-level") || 0;
GW.logSourcePadLength = 28;

function GWLog (string, source = "", level = 1) {
    if (GW.logLevel < level)
        return;

    let sourcestamp = (source > "" ? `[${source}]` : `[ ]`).padEnd(GW.logSourcePadLength, ' ');

	let outputString = (`[${GWTimestamp()}]  ` + sourcestamp + string);

    console.log(outputString);

	if (GW.console)
		GW.console.print(outputString);
}
GW.setLogLevel = (level, permanently = false) => {
    if (permanently)
        localStorage.setItem("gw-log-level", level);

    GW.logLevel = level;
};

function GWStopWatch(f, ...args) {
    let fname = (f.name || f.toString().slice(0, f.toString().indexOf('{')));
    console.log(`[${GWTimestamp()}]  ${fname} [BEGIN]`);
    let rval = f(...args);
    console.log(`[${GWTimestamp()}]  ${fname} [END]`);
    return rval;
}


/***********/
/* CONSOLE */
/***********/

GW.consoleTempBuffer = "";
GW.console = {
	print: (string) => {
		GW.consoleTempBuffer += string;
		GW.consoleTempBuffer += "\n";
	},
};


/*******************/
/* ERROR REPORTING */
/*******************/

/*  Reports an error by sending an XMLHTTPRequest to the 404 page, suffixed
    with some error string (which gets automatically URL-encoded).

	(Requires utility.js.)
 */
function GWServerLogError(errorString, errorType) {
    doAjax({ location: `${location.origin}/static/404-error-` + fixedEncodeURIComponent(errorString) });
    GWLog(`Reporting ${(errorType || "error")}:  ${errorString}`, "error reporting", 1);
}


/*************************/
/* JS CLASS MODIFICATION */
/*************************/

/*  The first item of the array (or null if array is empty).
 */
Object.defineProperty(Array.prototype, "first", {
    get() {
        if (this.length == 0)
            return null;

        return this[0];
    }
});

/*  The last item of the array (or null if array is empty).
 */
Object.defineProperty(Array.prototype, "last", {
    get() {
        if (this.length == 0)
            return null;

        return this[(this.length - 1)];
    }
});

/*  Remove given item from array.
 */
Array.prototype.remove = function (item) {
    let index = this.indexOf(item);
    if (index !== -1)
        this.splice(index, 1);
};

/*  Remove from array the first item that passes the provided test function.
    The test function should take an array item and return true/false.
    */
Array.prototype.removeIf = function (test) {
    let index = this.findIndex(test);
    if (index !== -1)
        this.splice(index, 1);
};

/*  Insert the given item into the array just before the first item that passes
    the provided test function. If no item passes the test function, append the
    item to the end of the array.
 */
Array.prototype.insertBefore = function (item, test) {
    let index = this.findIndex(test);
    if (index === -1) {
        this.push(item);
    } else {
        this.splice(index, 0, item);
    }
};

/***********/
/* PAGE UI */
/***********/

/*  Given an HTML string, creates an element from that HTML, adds it to
    #ui-elements-container (creating the latter if it does not exist), and
    returns the created element.
 */
function addUIElement(element_html) {
    let ui_elements_container = document.querySelector("#ui-elements-container");
    if (!ui_elements_container) {
        ui_elements_container = document.createElement("DIV");
        ui_elements_container.id = "ui-elements-container";
        document.querySelector("body").appendChild(ui_elements_container);
    }

    ui_elements_container.insertAdjacentHTML("beforeend", element_html);
    return ui_elements_container.lastElementChild;
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
            GWLog(`Canceling media query “${name}”`, "media queries", 1);

            if (whenCanceledDo != null)
                whenCanceledDo(mediaQuery);
        } else {
            let matches = (typeof event == "undefined") ? mediaQuery.matches : event.matches;

            GWLog(`Media query “${name}” triggered (matches: ${matches ? "YES" : "NO"})`, "media queries", 1);

            if ((otherwiseDo == null) || matches)
                ifMatchesOrAlwaysDo(mediaQuery);
            else
                otherwiseDo(mediaQuery);
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


/***********/
/* HELPERS */
/***********/

/*******************************************************************************/
/*  Product of two string arrays. (Argument can be a string, which is equivalent
    to passing an array with a single string member.)
    Returns array whose members are all results of concatenating each left hand
    array string with each right hand array string, e.g.:

        [ "a", "b" ].π([ "x", "y" ])

    will return:

        [ "ax", "ay", "bx", "by" ]

    Any non-string argument must be iterable, else null is returned. Any
    members of a passed array (or other iterable object), whatever their types,
    are stringified and interpolated into the resulting product strings.
 */
Array.prototype.π = function (strings) {
    if (typeof strings == "string")
        strings = [ strings ];

    if (!!strings[Symbol.iterator] == "false")
        return null;

    let product = [ ];
    for (lhs of this) {
        for (rhs of strings) {
            product.push(`${lhs}${rhs}`);
        }
    }
    return product;
};

/*****************************************************************************/
/*  As Array.π, but applies sequentially to each argument. (First argument may
    be a string, which is impossible with the Array member version.)
 */
function _π(...args) {
    if (args.length == 0)
        return [ ];

    let product = [ "" ];
    for (arg of args)
        product = product.π(arg);

    return product;
}

/*  Returns val, or def if val == defval. (By default, defval is -1.)
    (In other words, `defval(X,Y,Z)` is “return X if Y is Z [else, just Y]”.)
 */
function defval(def, val, defval = -1) {
    return (val == defval) ? def : val;
}

/*  Returns val, or min if val < min, or max if val > max.
    (In other words, clamps val to [min,max].)
 */
function valMinMax(val, min, max) {
    return Math.max(Math.min(val, max), min);
}


/*****************/
/* NOTIFICATIONS */
/*****************/
/*  The GW.notificationCenter object allows us to register handler functions for
    named events. Any number of handlers may be registered for any given named
    event, and when that event is fired, all of its registered handlers will be
    called. Because event handlers are registered for events by event name
    (which may be any string we like), a handler may be registered for an event
    at any time and at any location in the code. (In other words, an event does
    not need to first be “defined”, nor needs to “exist” in any way, in order
    for a handler to be registered for it.)

    We can also make the calling of any given event handler conditional (with a
    user-defined, handler-specific condition function [closure] that dynamically
    determines whether its associated handler should be called or not, when the
    event the handler was registered for is fired), specify that an event
    handler should be called only once or many times, and group handlers for a
    particular event into named “phases” (to ensure that certain handlers for an
    event are always called before/after others).

    Events themselves are also user-defined. Causing an event to fire is as
    simple as calling GW.notificationCenter.fireEvent() and providing an event
    name (which may be any string), plus an event info dictionary (which may
    contain any keys and values we deem necessary, and which will be passed to
    the handler functions); this will trigger the calling of all the handlers
    that have been registered for that event name.

    See the comments on specific elements of GW.notificationCenter, below, for
    more information.
 */
GW.notificationCenter = {
    /*  Dictionary of registered event handlers for named events.

        KEYS are event names (e.g. ‘GW.contentDidLoad’).

        VALUES are arrays of handler definitions for each event. Each handler
        definition is a dictionary with the following keys/values:

        - ‘f’ (key)
            Handler function to call when the named event fires (passing the
            event info dictionary of the fired event). (See comment on the
            ‘addHandlerForEvent’ function, below, for details.)

        - ‘options’ (key) [optional]
            Event options dictionary, with the following keys/values:

            - ‘condition’ (key) [optional]
                Test function, to which the event info dictionary of the fired
                event is passed; the handler function is called if (and only if)
                the condition returns true

            - ‘once’ (key) [optional]
                Boolean value; if true, the handler will be removed after the
                handler function is called once (note that if there is a
                condition function provided [see the ‘condition’ key], the
                handler function will not be called - and therefore will not be
                removed - if the named event is fired by the condition evaluates
                to false).

                If not set, defaults to false (i.e., by default a handler is
                not removed after an event is fired once, but will continue to
                be invoked each time the named event fires and the condition,
                if any, evaluates as true).

            - ‘phase’ (key) [optional]
                String which specifies when the given handler function should be
                called, relative to other handlers registered for the named
                event.

                The format for this string is as follows:

                - If the first character is anything other than ‘<’ or ‘>’, the
                  entire string is treated as the name of a handler phase. The
                  given handler function will be called in the same handler
                  phase as all other handlers assigned to that phase. (Within a
                  phase, handlers are called in the order in which they were
                  added.)

                - If the first character is ‘<’, then the rest of the string
                  is treated as the name of a handler phase. The given handler
                  function will be called prior to any handlers assigned to the
                  specified phase, but after any handlers assigned to an earlier
                  named phase (if any). (Within such a “before phase X”
                  ‘pseudo-phase’, handlers are called in the order in which they
                  were added.)

                - If the first character is ‘>’, then the rest of the string
                  is treated as the name of a handler phase. The given handler
                  function will be called after any handlers assigned to the
                  specified phase, but before any handlers assigned to a later
                  named phase (if any). (Within such an “after phase X”
                  ‘pseudo-phase’, handlers are called in the order in which they
                  were added.)

        When an event is fired, any handlers registered for that event (i.e.,
        members of the array which is the value for that event’s name in the
        eventHandlers dictionary) are called in array order. (If a condition is
        specified for any given handler, the handler function is only called if
        the condition function - called with the event info dictionary as its
        argument - evaluates true.)

        The order of an event handlers array for a given event is, by default,
        determined by the order in which handlers are registered for that event.
        The value of the ‘phase’ key of an event’s options dictionary can
        override and modify this default order. (See definition of the ‘phase’
        key of an event handler options dictionary, above.)
     */
    eventHandlers: { },

    /*  Defined event handler phases for certain named events.
        (See definition of the ‘phase’ key of an event handler options
         dictionary, above, for more info.)

        Phases are defined in execution order. For example, consider a 
        hypothetical GW.exampleDidHappen event, whose handler phases are defined
        as follows: `[ "foo", "bar" ]`. When the GW.exampleDidHappen event 
        fires, event handlers are called in the following order:

		1. Handlers assigned to be called before all other phases (i.e., those 
		   with ‘<’ as the value of their ‘phase’ key in their event handler 
		   options dictionary)
        2. Handlers assigned to be called before the ‘foo’ phase (i.e.,
           those with ‘<foo’ as the value of their ‘phase’ key in their
           event handler options dictionary)
        3. Handlers assigned to be called during the ‘foo’ phase (i.e.,
           those with ‘foo’ as the value of their ‘phase’ key in their
           event handler options dictionary)
        4. Handlers assigned to be called after the ‘foo’ phase (i.e.,
           those with ‘>foo’ as the value of their ‘phase’ key in their
           event handler options dictionary)
        5. Handlers assigned to be called before the ‘bar’ phase
           (i.e., those with ‘<bar’ as the value of their ‘phase’ key
           in their event handler options dictionary)
        6. Handlers assigned to be called during the ‘bar’ phase
           (i.e., those with ‘bar’ as the value of their ‘phase’ key
           in their event handler options dictionary)
        7. Handlers assigned to be called after the ‘bar’ phase
           (i.e., those with ‘>bar’ as the value of their ‘phase’ key
           in their event handler options dictionary)
        8. Handlers assigned to be called after all other phases (i.e., those 
		   with ‘>’ as the value of their ‘phase’ key in their event handler 
		   options dictionary)

        (Handlers with no specified phase might be called at any point after 
         step 1 and before step 8 in this sequence, depending on when they were 
         registered.)
     */
    handlerPhaseOrders: { },

    /*  Register a new event handler for the named event. Arguments are:

        - ‘eventName’
            The name of an event (e.g. ‘GW.contentDidLoad’).

        - ‘f’
            Event handler function. When the event fires, this function will be
            called. Not that if a condition is specified in the event handler
            options (i.e., if a condition function is provided as the value of
            the ‘condition’ key in the event handler options dictionary), then
            the handler function will be called only if the condition function
            evaluates true).

            The event handler function should take one argument: an event info
            dictionary. The keys and values of this dictionary are mostly
            event-specific (but see the ‘fireEvent’ function, below, for more
            info).

        - ‘options’ [optional]
            Event handler options dictionary. See comment on the ‘eventHandlers’
            property (above) for info on possible keys/values.

        Note that if there already exists a registered event handler for the
        given event with the same event handler function as the new handler that
        you are trying to register, then the new handler will not be registered
        (even if it has different handler options than the existing handler).
     */
    addHandlerForEvent: (eventName, f, options = { }) => {
        /*  If this event is currently firing, do not add the handler yet.
            Instead, add it to the waiting list. It will be added once the event
            has finished firing.
         */
        if (GW.notificationCenter.currentEvents.includes(eventName)) {
            if (GW.notificationCenter.waitingHandlers[eventName] == null)
                GW.notificationCenter.waitingHandlers[eventName] = [ ];

            GW.notificationCenter.waitingHandlers[eventName].push({ f: f, options: options });

            return;
        }

        /*  If there’s not already a handlers array for the given event (which
            may be, e.g., because no event handlers have yet been registered
            for this event), create the array.
         */
        if (GW.notificationCenter.eventHandlers[eventName] == null)
            GW.notificationCenter.eventHandlers[eventName] = [ ];

        /*  Array of registered handlers for the named event. Might be empty
            (if no handlers have been registered for this event yet).
         */
        let handlers = GW.notificationCenter.eventHandlers[eventName];

        /*  If there is already a registered handler with the same handler
            function as the one we’re trying to register, do not register this
            new one (even if it has different handler options).
         */
        if (handlers.findIndex(handler => handler.f == f) !== -1)
            return;

        /*  By default, add the new handler to the end of the handlers array
            for this event (so that, when the event is fired, this new handler
            gets called after all the previously registered handlers).

            However, if a defined handler phase order exists for the event
            that we’re registering this handler for (see ‘phaseOrder’, below),
            and a phase has been specified in this handler’s options dictionary,
            then that might result in this handler being inserted into the named
            event’s handler array at a different point.
         */
        let insertAt = handlers.length;

        /*  Get the handler phase order for the named event, if any.

            (If no handler phases have been defined for the given event, then
             we will ignore the value of the ‘phase’ key in the new handler’s
             options dictionary, and simply stick with the default behavior of
             adding the new handler at the end of the event’s handler array.)
         */
        let phaseOrder = GW.notificationCenter.handlerPhaseOrders[eventName];

        /*  If the handler we’re registering isn’t assigned to any particular
            handler phase, we will simply add it to the end of the handler array
            for the given event (and the next large block of code, within the
            conditional below, will not be executed). However, we still want to
            set an empty-string value for the ‘phase’ key of the handler’s
            options dictionary, in order for the ‘phaseAt’ function (below) to
            work properly.
         */
        if (phaseOrder)
            options.phase = (options.phase || "");

        /*  Only if (a) there’s a defined handler phase order for the given
            event, AND (b) the handler we’re registering is being assigned to a
            specific phase, do we have anything to do here...
         */
        if (   options.phase > ""
            && phaseOrder) {
            /*  Get the target phase name, which may be the full value of the
                ‘phase’ key of the options dictionary, OR it may be that value
                minus the first character (if the value of the ‘phase’ key
                begins with a ‘<’ or a ‘>’ character).
                */
            let targetPhase = options.phase.match(/^([<>]?)(.+)/)[2];

            /*  Get the index of the target phase in the defined handler phase
                order for the named event. If the specified phase is not found
                in the defined handler phase order, set targetPhaseIndex to
                the length of the phase order array, thus ensuring that, by
                default, the new handler will be appended to the end of the
                event’s handlers array.
             */
            let targetPhaseIndex = defval(phaseOrder.length, phaseOrder.indexOf(targetPhase), -1);

            /*  Takes an index into the given event’s handler array. Returns a
                dictionary with these keys/values:

                - ‘phase’ [key]
                    The name of the phase to which the handler at the given
                    index is assigned (could be an empty string).

                - ‘before’ [key]
                    Boolean value; true if the handler at the given index is
                    assigned to run before the specified phase, false otherwise
                    (i.e., if it’s instead assigned to run either during or
                    after the specified phase).

                - ‘after’ [key]
                    Boolean value; true if the handler at the given index is
                    assigned to run after the specified phase, false otherwise
                    (i.e., if it’s instead assigned to run either before or
                    during the specified phase).

                (Note that for an event handler which has not been assigned to
                 any specific phase, ‘phase’ will be the empty string, and both
                 ‘before’ and ‘after’ will be false.)

                Returns null if the given index is out of bounds of the event’s
                handler definitions array.
             */
            let phaseAt = (index) => {
                if (index >= handlers.length)
                    return null;
                let parts = handlers[index].options.phase.match(/^([<>]?)(.*)$/);
                return {
                    phase: parts[2],
                    before: (parts[1] == "<"),
                    after: (parts[1] == ">")
                };
            };

            if (options.phase.startsWith("<")) {
                /*  The handler is assigned to be called before the specified
                    phase.
                 */
                for (var i = 0; i < handlers.length; i++) {
                    /*  We have found the index before which to insert, if the
                        handler at this index is assigned to be called during
                        or after our target phase, OR if it is assigned to be
                        called before, during, or after any later phase.

                        (In other words, we have passed all the handlers which
                         are assigned either to any earlier phase or to before
                         the specified phase.)
                     */
                    if (    phaseAt(i).phase == targetPhase
                        && !phaseAt(i).before)
                        break;
                    if (phaseOrder.slice(targetPhaseIndex + 1).includes(phaseAt(i).phase))
                        break;
                }

                /*  If neither of the break conditions in the loop were
                    encountered, i is now equal to the handler array length,
                    and the new handler will be added to the end of the array.
                    Otherwise, it’ll be inserted in the appropriate place.
                 */
                insertAt = i;
            } else if (options.phase.startsWith(">")) {
                /*  The handler is assigned to be called after the specified
                    phase.
                 */
                for (var j = handlers.length - 1; j > -1; j--) {
                    /*  We have found the index _after_ which to insert (hence
                        the `j++`), if the handler at this index is assigned to
                        be called before, during, or after the target phase, OR
                        if it is assigned to be called before, during, or after
                        any earlier phase.

                        (In other words, we have passed - moving backwards
                         through the handlers array - all the handlers which
                         are assigned either to any later phase or to after
                         or during the specified phase.)
                     */
                    if (phaseAt(j).phase == targetPhase) {
                        j++;
                        break;
                    }

                    /*  There are no “earlier phases” if the target phase index
                        is either 0 or out of array bounds of the phase order.
                        (The latter will happen if the target phase is not in
                         the defined phase order for the given event.)
                    */
                    if (   targetPhaseIndex > 0
                        && targetPhaseIndex < phaseOrder.length
                        && phaseOrder.slice(0, targetPhaseIndex - 1).includes(phaseAt(j).phase)) {
                        j++;
                        break;
                    }
                }

                /*  If neither of the break conditions in the loop were
                    encountered, j is now equal to -1; in this case, set j to
                    the handlers array length, such that the new handler will be
                    added to the end of the array. Otherwise, it’ll be inserted
                    in the appropriate place.
                 */
                insertAt = defval(handlers.length, j, -1);
            } else {
                /*  The handler is assigned to be called during the specified
                    phase.
                 */
                for (var k = 0; k < handlers.length; k++) {
                    /*  We have found the index before which to insert, if the
                        handler at this index is assigned to be called after the
                        target phase, OR if it is assigned to be called before,
                        during, or after any later phase.

                        (In other words, we have passed all the handlers which
                         are assigned either to any earlier phase or to before
                         or during the specified phase.)
                     */
                    if (   phaseAt(k).phase == targetPhase
                        && phaseAt(k).after)
                        break;
                    if (phaseOrder.slice(targetPhaseIndex + 1).includes(phaseAt(k).phase))
                        break;
                }

                /*  If neither of the break conditions in the loop were
                    encountered, k is now equal to the handler array length,
                    and the new handler will be added to the end of the array.
                    Otherwise, it’ll be inserted in the appropriate place.
                 */
                insertAt = k;
            }
        }

        /*  Add the new event handler to the named event’s handler definitions
            array, at whatever index we have now determined it should go to.
         */
        GW.notificationCenter.eventHandlers[eventName].splice(insertAt, 0, { f: f, options: options });
    },

    /*  Unregister the event handler with the given handler function from the
        specified named event (if such a handler exists).
     */
    removeHandlerForEvent: (eventName, f) => {
        if (GW.notificationCenter.eventHandlers[eventName] == null)
            return;

        GW.notificationCenter.eventHandlers[eventName].removeIf(handler => handler.f === f);
    },

    /*  Unregister all registered event handlers from the specified named event.
     */
    removeAllHandlersForEvent: (eventName) => {
        GW.notificationCenter.eventHandlers[eventName] = null;
    },

    /*  Event-specific pre-fire processing functions. Keys are event names.
        Values are functions that take the event info as an argument, and return
        modified event info.
    */
    prefireProcessors: { },

    /*  Array of events that are currently being fired. Used to avoid adding a
        handler to an event while it’s firing.
     */
    currentEvents: [ ],

    /*  Arrays (keyed to event names) of event handlers waiting to be added to
        events. A handler waits here if its addHandlerForEvent() call happened
        while the target event was firing. The handler will be added once the
        event has finished firing.
     */
    waitingHandlers: { },

    /*  Add all waiting handlers for the event, if any.
     */
    addWaitingHandlersForEvent: (eventName) => {
        if (GW.notificationCenter.waitingHandlers[eventName]) {
            GW.notificationCenter.waitingHandlers[eventName].forEach(handler => {
                if (handler.f) {
                    GW.notificationCenter.addHandlerForEvent(eventName, handler.f, handler.options);
                    handler.f = null;
                }
            });
            GW.notificationCenter.waitingHandlers[eventName] = GW.notificationCenter.waitingHandlers[eventName].filter(handler => handler.f);
        }
    },

    /*  Fire an event with the given name and event info dictionary.

        In addition to printing a console log message (if the log level is set
        to 1 or higher), this will also cause each event handler that has been
        registered for the named event to be called. (Handlers with a condition
        function specified in their event handler options will first have that
        condition function called, and the handler function will only be called
        if the condition evaluates true.)

        The event info dictionary provided to the ‘fireEvent’ function will be
        passed as the argument to each handler function (as well as to any
        condition function that is called to determine whether a handler should
        be called).

        The event info dictionary may contain various, mostly event-specific,
        keys and values. The one common key/value that any event’s info
        dictionary may contain is the ‘source’ key, whose value should be a
        string identifying the function, browser event, or other context which
        caused the given event to be fired (such as ‘DOMContentLoaded’ or
        ‘Annotations.load’). In addition to any ways in which it may be used 
        by an event handler, this string (i.e., the value of the ‘source’ key) 
        is (if present) included in the console message that is printed when the 
        event is fired.
     */
    fireEvent: (eventName, eventInfo = { }) => {
        if (!eventName)
            return;

        //  Register this event as currently being fired.
        GW.notificationCenter.currentEvents.push(eventName);

        /*  Store event name in info dictionary, so that event handlers can
            access it. (This permits, e.g., the same handler to handle multiple
            events, and conditionally select behavior based on which event is
            calling the handler.)
         */
        eventInfo.eventName = eventName;

        /*  The ‘16’ here is the width of the date field plus spacing.
            The “Source:” text is manually padded to be as wide
            as “[notification]”.
         */
        GWLog(`Event “${eventName}” fired.`
            + `${((eventInfo && eventInfo.source)
                  ? ("\n"
                   + "".padStart(16, ' ')
                   + "       Source:".padEnd(GW.logSourcePadLength, ' ')
                   + eventInfo.source)
                  : ""
                 )}`, "notification");

        /*  If event-specific pre-fire processing is needed, do it.
         */
        if (GW.notificationCenter.prefireProcessors[eventName])
            eventInfo = GW.notificationCenter.prefireProcessors[eventName](eventInfo);

        /*  Call all registered handlers (if any), in order.
         */
        if (GW.notificationCenter.eventHandlers[eventName]) {
            for (let i = 0; i < GW.notificationCenter.eventHandlers[eventName].length; i++) {
                let handler = GW.notificationCenter.eventHandlers[eventName][i];
                /*  If a condition function is provided, call it to determine
                    whether the handler function should be called.
                 */
                if (   handler.options.condition
                    && handler.options.condition(eventInfo) == false)
                    continue;

                /*  If the condition function evaluated true, or if no condition
                    function was provided, we call the handler.
                 */
                handler.f(eventInfo);

                /*  If the handler options specified a true value for the ‘once’
                    key, we unregister this handler after having called it once.

                    (Note that in the case of an once-only handler that’s called
                     conditionally, i.e. one with a specified condition function,
                     regardless of how many times the named event fires, the handler
                     is never automatically removed until its condition evaluates
                     true and the handler actually gets called once.)
                 */
                if (handler.options.once) {
                    GW.notificationCenter.eventHandlers[eventName].splice(i, 1);
                    i--;
                }
            }
        }

        //  Unregister this event from the list of events currently being fired.
        GW.notificationCenter.currentEvents.remove(eventName);

        //  Add any handlers that are waiting to be added.
        GW.notificationCenter.addWaitingHandlersForEvent(eventName);
    }
};

/*  Event-specific handler phase order for the ‘GW.contentDidLoad’ event.
 */
GW.notificationCenter.handlerPhaseOrders["GW.contentDidLoad"] = [ "transclude", "rewrite" ];
GW.notificationCenter.handlerPhaseOrders["GW.contentDidInject"] = [ "rewrite", "eventListeners" ];

/*  Event-specific boolean flags for the ‘GW.contentDidInject’ event.

    See rewrite.js for details on the meaning of these flags.
 */
GW.contentDidInjectEventFlags = {
    clickable:          1 << 0,
    stripCollapses:     1 << 1,
    fullWidthPossible:  1 << 2
};

/*  Event-specific pre-fire processing for the ‘GW.contentDidInject’ event.
 */
GW.notificationCenter.prefireProcessors["GW.contentDidInject"] = (eventInfo) => {
    for ([flagName, flagValue] of Object.entries(GW.contentDidInjectEventFlags))
        eventInfo[flagName] = (0 != (eventInfo.flags & flagValue));

    return eventInfo;
};


/********************/
/* SCROLL LISTENERS */
/********************/

GW.scrollListeners = { };
/*  Adds a scroll event listener to the page.
 */
function addScrollListener(fn, name, options = { }, target = document) {
    if (options.defer) {
        doWhenPageLoaded(() => {
            requestAnimationFrame(() => {
                if (options.ifDeferCallWhenAdd)
                    fn();
                addScrollListener(fn, name, { defer: false }, target);
            });
        });
        return;
    }

    let wrapper = (event) => {
        requestAnimationFrame(() => {
            fn(event);
            target.addEventListener("scroll", wrapper, { once: true, passive: true });
        });
    }
    target.addEventListener("scroll", wrapper, { once: true, passive: true });

    /*  Retain a reference to the scroll listener, if a name is provided.
     */
    if (name)
        GW.scrollListeners[name] = { wrapper: wrapper, target: target };

    return wrapper;
}
/*  Removes a named scroll event listener from the page.
 */
function removeScrollListener(name) {
    let listener = GW.scrollListeners[name];
    if (listener) {
        listener.target.removeEventListener("scroll", listener.wrapper);
        GW.scrollListeners[name] = null;
    }
}


/****************/
/* SCROLL STATE */
/****************/

GW.scrollState = {
    lastScrollTop:              0,
    unbrokenDownScrollDistance: 0,
    unbrokenUpScrollDistance:   0
};

function updateScrollState(event) {
    GWLog("updateScrollState", "gw.js", 3);

    GW.newScrollTop = (window.pageYOffset || document.documentElement.scrollTop);
    GW.scrollState.unbrokenDownScrollDistance = (GW.newScrollTop > GW.scrollState.lastScrollTop)
        ? (GW.scrollState.unbrokenDownScrollDistance + GW.newScrollTop - GW.scrollState.lastScrollTop)
        : 0;
    GW.scrollState.unbrokenUpScrollDistance = (GW.newScrollTop < GW.scrollState.lastScrollTop)
        ? (GW.scrollState.unbrokenUpScrollDistance + GW.scrollState.lastScrollTop - GW.newScrollTop)
        : 0;
    GW.scrollState.lastScrollTop = GW.newScrollTop;
}
addScrollListener(updateScrollState, "updateScrollStateScrollListener", { defer: true });

/*  Toggles whether the page is scrollable.
 */
function isPageScrollingEnabled() {
    return !(document.documentElement.classList.contains("scroll-enabled-not"));
}
/*  Pass true or false to enable or disable (respectively) page scrolling.
    Calling this function with no arguments toggles the state (enables if
    currently disabled, or vice versa).
 */
function togglePageScrolling(enable) {
    if (typeof enable == "undefined")
        enable = document.documentElement.classList.contains("scroll-enabled-not");

    let preventScroll = (event) => { document.documentElement.scrollTop = GW.scrollState.lastScrollTop; };

    /*  The `scroll-enabled-not` CSS class, which is added to the `html` element
        when scrolling is disabled by this function (in order to permit the
        “toggle” behavior, i.e. calling ‘togglePageScrolling’ with no
        arguments), allows the assignment of arbitrary CSS properties to the
        page on the basis of scroll state. This is purely a convenience (which
        may be useful if, for example, some styling needs to change on the basis
        of change in page scroll state, e.g. modifying the appearance of scroll
        bars). No specific CSS properties are needed in order for this function
        to work properly.
     */
    if (   enable
        && isPageScrollingEnabled() == false) {
        document.documentElement.classList.toggle("scroll-enabled-not", false);
        removeScrollListener("preventScroll");
        addScrollListener(updateScrollState, "updateScrollStateScrollListener");
    } else if (  !enable
               && isPageScrollingEnabled() == true) {
        document.documentElement.classList.toggle("scroll-enabled-not", true);
        addScrollListener(preventScroll, "preventScroll");
        removeScrollListener("updateScrollStateScrollListener");
    }
}


/***********/
/* DO-WHEN */
/***********/

/*  Run the given function immediately if the page is already loaded, or add
    a listener to run it as soon as the page loads.
 */
function doWhenPageLoaded(f) {
    if (document.readyState == "complete")
        f();
    else
        window.addEventListener("load", () => { f(); });
}

/*  Run the given function immediately if the page content has already loaded
    (DOMContentLoaded event has fired), or add a listener to run it as soon as
    the event fires.
 */
function doWhenDOMContentLoaded(f) {
    if (GW.DOMContentLoaded == true)
        f();
    else
        window.addEventListener("DOMContentLoaded", () => { f(); });
}

/*  Run the given function immediately if the <body> element has already been
    created, or add a mutation observer to run it as soon as the <body> element
    is created.
 */
function doWhenBodyExists(f) {
    if (document.body) {
        f();
    } else {
        let observer = new MutationObserver((mutationsList, observer) => {
            if (document.body) {
                observer.disconnect();
                f();
            }
        });

        observer.observe(document.documentElement, { childList: true });
    }
}


/******************/
/* BROWSER EVENTS */
/******************/

/*  We know this is false here, because this script is inlined in the <head>
    of the page; so the page body has not yet loaded when this code runs.
 */
GW.DOMContentLoaded = false;

GWLog("document.readyState." + document.readyState, "browser event");
window.addEventListener("DOMContentLoaded", () => {
    GWLog("window.DOMContentLoaded", "browser event");
    GW.DOMContentLoaded = true;
    let pageURL = new URL(location.href);
    GW.notificationCenter.fireEvent("GW.contentDidLoad", {
        source: "DOMContentLoaded",
        container: document.body,
        document: document,
        loadLocation: pageURL
    });
    GW.notificationCenter.fireEvent("GW.contentDidInject", {
        source: "DOMContentLoaded",
        container: document.body,
        document: document,
        loadLocation: pageURL,
        flags: (  GW.contentDidInjectEventFlags.clickable
                | GW.contentDidInjectEventFlags.fullWidthPossible)
    });
});
window.addEventListener("load", () => {
    GWLog("window.load", "browser event");
});
document.addEventListener("readystatechange", () => {
    GWLog("document.readyState." + document.readyState, "browser event");
});


/*********************/
/* SPECIAL OCCASIONS */
/*********************/

/*  If a special function is provided to apply classes, one should also be
    provided to remove those classes. (See the ‘halloween’ entry for example.)
 */
GW.specialOccasions = [
    [ "halloween", () => isTodayHalloween(), () => {
        document.body.classList.remove("special-halloween-dark", "special-halloween-light");
        let specialClass = DarkMode.computedMode() == "dark"
                           ? "special-halloween-dark"
                           : "special-halloween-light";
        document.body.classList.add(specialClass);
      }, () => {
        document.body.classList.remove("special-halloween-dark", "special-halloween-light");
      } ],
    [ "christmas", () => isTodayChristmas() ],
];

function isTodayHalloween() {
    // only bother English-speakers with Anglosphere holidays like Halloween:
    let language = window.navigator.userLanguage || window.navigator.language;
    if ("en" == language.slice(0,2)) { // match 'en', 'en-US', 'en-GB', 'en-AU'...
        let now = new Date();
        let date = (now.toISOString()).slice(5,10); // `YYYY-MM-DDTHH:mm:ss.sssZ` → MM-DD
        let hour =  now.getHours();
        // It is a sin to celebrate Halloween while there is daylight; however, calculating local sunset or local ambient light is too hard
        // (where would we even get that geolocation or light sensor data from‽), so we will simply define 'night' as >5PM and <6AM.
        return (date == "10-31" && hour > 17) || (date == "11-01" && hour < 6)
    }
}
function isTodayChristmas() {
    let now = new Date();
    let date = (now.toISOString()).slice(5,10);
    let hour =  now.getHours();
    // Christmas = Christmas Eve + all Christmas Day; Christmas Eve starts in the evening, so again >5PM.
    return (date == "12-24" && hour > 17) || (date == "12-25")
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
});
