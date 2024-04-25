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

    for (let [ key, mediaQuery ] of Object.entries(GW.mediaQueries))
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


/*************/
/* DOCUMENTS */
/*************/

/*  Return the location (URL) associated with a document.
    (Document|DocumentFragment) => URL
 */
function baseLocationForDocument(doc) {
	if (doc == null) {
		return null;
	} else if (doc == document) {
        return URLFromString(location.href);
    } else if (   doc.body instanceof Element
               && doc.body.classList.contains("popframe-body")) {
        let spawningTarget = (Extracts.popFrameProvider == Popups
                              ? doc.body.popup.spawningTarget
                              : doc.body.popin.spawningTarget);
        return URLFromString(spawningTarget.href);
    } else if (doc.baseLocation) {
        return URLFromString(doc.baseLocation.href);
    } else {
        return null;
    }
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
                the condition returns true.

            - ‘once’ (key) [optional]
                Boolean value; if true, the handler will be removed after the
                handler function is called once (note that if there is a
                condition function provided [see the ‘condition’ key], the
                handler function will not be called - and therefore will not be
                removed - if the named event is fired but the condition 
                evaluates to false).

                If not set, defaults to false (ie. by default a handler is
                not removed after an event is fired once, but will continue to
                be invoked each time the named event fires and the condition,
                if any, evaluates as true).

            - ‘phase’ (key) [optional]
                String which specifies when the given handler function should be
                called, relative to other handlers registered for the named
                event.

                The format for this string is as follows:

				- If the entire string is equal to “<”, then the given handler
				  function will be called prior to any handlers that are
				  assigned to any other phase (or to no specific phase). (Within
				  this “before all others” ‘pseudo-phase’, handlers are called
				  in the order in which they were added.)

				- If the entire string is equal to “>”, then the given handler
				  function will be called after any handlers that are assigned
				  to any other phase (or to no specific phase). (Within this
				  “after all others” ‘pseudo-phase’, handlers are called in the
				  order in which they were added.)

				- If the string is empty, then the given handler function will
				  be called after all other handlers, but before any handlers
				  that were assigned to phase “>”. (Within this “no particular
				  phase” ‘pseudo-phase’, handlers are called in the order in
				  which they were added.)

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

        When an event is fired, any handlers registered for that event (ie.
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

		1. Handlers assigned to be called before all other phases (ie. those
		   with ‘<’ as the value of their ‘phase’ key in their event handler
		   options dictionary)
        2. Handlers assigned to be called before the ‘foo’ phase (ie.
           those with ‘<foo’ as the value of their ‘phase’ key in their
           event handler options dictionary)
        3. Handlers assigned to be called during the ‘foo’ phase (ie.
           those with ‘foo’ as the value of their ‘phase’ key in their
           event handler options dictionary)
        4. Handlers assigned to be called after the ‘foo’ phase (ie.
           those with ‘>foo’ as the value of their ‘phase’ key in their
           event handler options dictionary)
        5. Handlers assigned to be called before the ‘bar’ phase
           (ie. those with ‘<bar’ as the value of their ‘phase’ key
           in their event handler options dictionary)
        6. Handlers assigned to be called during the ‘bar’ phase
           (ie. those with ‘bar’ as the value of their ‘phase’ key
           in their event handler options dictionary)
        7. Handlers assigned to be called after the ‘bar’ phase
           (ie. those with ‘>bar’ as the value of their ‘phase’ key
           in their event handler options dictionary)
        8. Handlers assigned to be called after all other phases (ie. those
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
            called. Note that if a condition is specified in the event handler
            options (i.e. if a condition function is provided as the value of
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
    addHandlerForEvent: (eventName, f, options) => {
		options = Object.assign({
			condition: null,
			once: false,
			phase: ""
		}, options);

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
            may be, e.g. because no event handlers have yet been registered
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

        /*  Get the handler phase order for the named event, if any. (Add to it
        	the built-in phases “<” and “>”.)
         */
        let phaseOrder = [ "<", ...(GW.notificationCenter.handlerPhaseOrders[eventName] ?? [ ]), ">" ];

		/*  Get the target phase name, which may be the full value of the
			‘phase’ key of the options dictionary, OR it may be that value
			minus the first character (if the value of the ‘phase’ key
			begins with a ‘<’ or a ‘>’ character).
			*/
		let targetPhase = options.phase.match(/^([<>]?)(.+)?/)[2];

		/*  Get the index of the target phase in the defined handler phase
			order for the named event.
		 */
		let targetPhaseIndex = phaseOrder.indexOf(targetPhase);

		/*  Takes an index into the given event’s handler array. Returns a
			dictionary with these keys/values:

			- ‘phase’ [key]
				The name of the phase to which the handler at the given
				index is assigned (could be an empty string).

			- ‘before’ [key]
				Boolean value; true if the handler at the given index is
				assigned to run before the specified phase, false otherwise
				(ie. if it’s instead assigned to run either during or
				after the specified phase).

			- ‘after’ [key]
				Boolean value; true if the handler at the given index is
				assigned to run after the specified phase, false otherwise
				(ie. if it’s instead assigned to run either before or
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
			return (parts[2] > ""
					? { phase: parts[2],
						before: (parts[1] == "<"),
						after: (parts[1] == ">") }
					: { phase: parts[1] });
		};

		//	Where in the handlers array to insert the new handler?
        let insertAt;
        if (options.phase == "<") {
			/*	If the handler we’re registering is assigned to phase “<” (i.e.,
				is specified to run before all others), it’s inserted
				immediately after all other handlers already likewise specified.
			 */
        	for (var i = 0; i < handlers.length; i++) {
        		if (phaseAt(i).phase != "<")
        			break;
        	}

			insertAt = i;
        } else if (options.phase == ">") {
			/*	If the handler we’re registering is assigned to phase “>” (i.e.,
				is specified to run after all others), it’s inserted immediately
				after all other handlers already so specified (i.e., at the very
				end of the handlers array).
			 */
        	insertAt = handlers.length;
        } else if (   options.phase == ""
        		   || targetPhaseIndex == -1) {
			/*  If the handler we’re registering isn’t assigned to any
				particular handler phase, or if it’s assigned to a phase that
				does not actually exist in this event’s handler phase order,
				we will add it just before all handlers of phase “>” (i.e.,
				those handlers specified to be called after all others).
			 */
        	for (var j = 0; j < handlers.length; j++) {
        		if (phaseAt(j).phase == ">")
        			break;
        	}

			insertAt = j;
        } else {
			/*	The handler is specified to run before, during, or after a named
				phase (i.e., not “<” or “>”) that (as we’ve confirmed already)
				exists in this event’s defined handler phase order.
			 */

            if (options.phase.startsWith("<")) {
                /*  The handler is assigned to be called before the specified
                    phase.
                 */
                for (var k = 0; k < handlers.length; k++) {
                    /*  We have found the index before which to insert, if the
                        handler at this index is assigned to be called during
                        or after our target phase, OR if it is assigned to be
                        called before, during, or after any later phase.

                        (In other words, we have passed all the handlers which
                         are assigned either to any earlier phase or to before
                         the specified phase.)
                     */
                    let phaseAtThisIndex = phaseAt(k);
                    if (   (   phaseAtThisIndex.phase == targetPhase
                        	&& phaseAtThisIndex.before == false)
                        || phaseOrder.slice(targetPhaseIndex + 1).includes(phaseAtThisIndex.phase))
                        break;
                }

                insertAt = k;
            } else if (options.phase.startsWith(">")) {
                /*  The handler is assigned to be called after the specified
                    phase.
                 */
                for (var m = handlers.length - 1; m > -1; m--) {
                    /*  We have found the index _after_ which to insert (hence
                        the `m++`), if the handler at this index is assigned to
                        be called before, during, or after the target phase, OR
                        if it is assigned to be called before, during, or after
                        any earlier phase.

                        (In other words, we have passed - moving backwards
                         through the handlers array - all the handlers which
                         are assigned to any later phase.)
                     */
                    let phaseAtThisIndex = phaseAt(m);
                    if (   phaseAtThisIndex.phase == targetPhase
                    	|| phaseOrder.slice(0, targetPhaseIndex - 1).includes(phaseAtThisIndex.phase)) {
                        m++;
                        break;
                    }
                }

                insertAt = m;
            } else {
                /*  The handler is assigned to be called during the specified
                    phase.
                 */
                for (var n = 0; n < handlers.length; n++) {
                    /*  We have found the index before which to insert, if the
                        handler at this index is assigned to be called after the
                        target phase, OR if it is assigned to be called before,
                        during, or after any later phase.

                        (In other words, we have passed all the handlers which
                         are assigned either to any earlier phase or to before
                         or during the specified phase.)
                     */
                    let phaseAtThisIndex = phaseAt(n);
                    if (   (   phaseAtThisIndex.phase == targetPhase
                        	&& phaseAtThisIndex.after == true)
                        || phaseOrder.slice(targetPhaseIndex + 1).includes(phaseAtThisIndex.phase))
                        break;
                }

                insertAt = n;
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
        by an event handler, this string (i.e. the value of the ‘source’ key)
        is (if present) included in the console message that is printed when the
        event is fired.
     */
    fireEvent: (eventName, eventInfo = { }) => {
        if (eventName == null)
            return;

        //  Register this event as currently being fired.
        GW.notificationCenter.currentEvents.push(eventName);

        /*  Store event name in info dictionary, so that event handlers can
            access it. (This permits, e.g. the same handler to handle multiple
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


/**************************/
/* LOAD & INJECT HANDLERS */
/**************************/

/*******************************************************************************/
/*  NOTE on the GW.contentDidLoad and GW.contentDidInject events:

    These events are fired whenever any new local page content is loaded and
    injected into the page, respectively. (Here “loaded” may mean “loaded via a
    network request”, “constructed from a template”, or any other process by
    which a new unit of page content is created. This includes the initial page
    load, but also such things as annotations being lazy-loaded, etc. Likewise,
    “injected” may mean “injected into the base page”, “injected into a
    pop-frame shadow-root”, “injected into a DocumentFragment in cache”, etc.)

    Many event handlers are attached to these, because a great deal of
    processing must take place before newly-loaded page content is ready for
    presentation to the user. Typography rectification must take place; the HTML
    structure of certain page elements (such as tables, figures, etc.) must be
    reconfigured; CSS classes must be added; various event listeners attached;
    etc. Most of rewrite.js consists of exactly such “content load handlers” and
    “content inject handlers”, a.k.a. “rewrite functions”. (Additional content
    load and inject handlers are defined elsewhere in the code, as appropriate;
    e.g. the handler that attaches event listeners to annotated links to load
    annotations when the user mouses over such links, which is found in
    extracts-annotations.js.)

    The GW.contentDidLoad event has the following named handler phases (see
    above for details on what this means):

        [ "transclude", "rewrite" ]

    The GW.contentDidInject event has the following named handler phases:

        [ "rewrite", "eventListeners" ]

    The GW.contentDidLoad and GW.contentDidInject events should have the
    following keys and values in their event info dictionary (see above
    for details on event info dictionaries):

        ‘source’ (key) (required)
            String that indicates function (or event name, if fired from a
            browser event listener) from which the event is fired (such as
            ‘Annotation.load’).

        ‘container’ (key) (required)
            DOM object containing the loaded content. (For the GW.contentDidLoad
            event fired on the initial page load, the value of this key is
            `document`, i.e. the root document of the page. For pop-frames, this
            may be the `document` property of the pop-frame, or a
            DocumentFragment containing the embedded page elements.) The
            container will contain nothing but the newly-loaded content.
            (This key can be thought of as “what has been loaded?”.)

        ‘document’ (key) (required)
            Document into which the content was loaded. May or may not be
            identical with the value of the ‘container’ key (in those cases when
            the loaded content is a whole document itself). The value of this
            key is necessarily either a Document (i.e., the root document of the
            page) or a DocumentFragment. (This key can be thought of as “into
            where has the loaded content been loaded?”.)

        ‘contentType’ (key)
            String that indicates content type of the loaded content. Might be
            null (which indicates the default content type: local page content).
            Otherwise may be `annotation` or something else.

        ‘loadLocation’ (key)
            URL object (https://developer.mozilla.org/en-US/docs/Web/API/URL)
            which specifies the URL from which the loaded content was loaded.
            For the main page, the represented URL will be the value of
            `location.href`. For pop-frames, transcludes, etc., the represented
            URL will be that of the page in which the content resides. (If the
            loaded/injected content is not sourced from any page, this key will
            have a null value.)

    The GW.contentDidInject event should additionally have a value for the
    following key:

        ‘flags’ (key) (required)
            Bit field containing various flags (combined via bitwise OR). The
            values of the flags are defined in GW.contentDidInjectEventFlags.

            (Note that event handlers for the ‘GW.contentDidInject’ event can
             access the values of these flags directly via property access on
             the event info, e.g. the following two expressions are equivalent:

               eventInfo.flags & GW.contentDidInjectEventFlags.clickable != 0

               eventInfo.clickable

             It is recommended that the latter form be used.)

            The flags are:

            ‘clickable’
                Currently unused. Reserved for future use.

            ‘stripCollapses’
                Specifies whether the loaded content is permitted to have
                collapsed sections. Generally false. If the value of this key
                is true, then any collapse blocks in the loaded content will be
                automatically expanded and stripped, and all content in
                collapsible sections will be visible at all times.

            ‘fullWidthPossible’
                Specifies whether full-width elements are permitted in the
                loaded content. Generally true only for the main page load. If
                false, elements marked as full-width will be laid out as if for
                a mobile (narrow) viewport, regardless of the actual dimensions
                of the loaded content’s container (i.e. they will not actually
                be “full-width”).

			‘localize’
				Specifies whether content should be “localized” to the context
				into which it is being injected. (Affects things like link
				qualification. See transclude.js for more information.) 
				Generally true for page content, false for auxiliary content.
 */

GW.contentLoadHandlers = { };

/*  Add content load handler (i.e., an event handler for the GW.contentDidLoad
    event). (Convenience function.)
 */
function addContentLoadHandler(handler, phase = "", condition = null, once = false) {
    GW.notificationCenter.addHandlerForEvent("GW.contentDidLoad", handler, {
    	phase: phase,
    	condition: condition,
    	once: once
    });
}

GW.contentInjectHandlers = { };

/*  Add content inject handler (i.e., an event handler for the
    GW.contentDidInject event). (Convenience function.)
 */
function addContentInjectHandler(handler, phase = "", condition = null, once = false) {
    GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", handler, {
    	phase: phase,
    	condition: condition,
    	once: once
    });
}

/*  Event-specific handler phase order for the ‘GW.contentDidLoad’ and
	‘GW.contentDidInject’ events.
 */
GW.notificationCenter.handlerPhaseOrders["GW.contentDidLoad"] = [ "transclude", "rewrite" ];
GW.notificationCenter.handlerPhaseOrders["GW.contentDidInject"] = [ "rewrite", "eventListeners" ];

/*  Event-specific boolean flags for the ‘GW.contentDidInject’ event.
 */
GW.contentDidInjectEventFlags = {
    clickable:          1 << 0,
    stripCollapses:     1 << 1,
    fullWidthPossible:  1 << 2,
    localize:           1 << 3
};

/*  Event-specific pre-fire processing for the ‘GW.contentDidInject’ event.
 */
GW.notificationCenter.prefireProcessors["GW.contentDidInject"] = (eventInfo) => {
    for (let [flagName, flagValue] of Object.entries(GW.contentDidInjectEventFlags))
        eventInfo[flagName] = (0 != (eventInfo.flags & flagValue));

    return eventInfo;
};


/********************/
/* EVENT LISTENERS */
/********************/

GW.eventListeners = { };

/*  Adds a named event listener to the page (or other target).

	Available option fields:

	
 */
function addNamedEventListener(eventName, fn, options) {
	options = Object.assign({
		name: null,
		target: document,
		defer: false,
		ifDeferCallWhenAdd: false
	}, options);

    if (options.defer) {
        doWhenPageLoaded(() => {
            requestAnimationFrame(() => {
                if (options.ifDeferCallWhenAdd)
                    fn();
                addNamedEventListener(eventName, fn, {
                	name: options.name,
                	target: options.target,
                	defer: false
                });
            });
        });

        return;
    }

    let wrapper = (event) => {
        requestAnimationFrame(() => {
            fn(event);
            options.target.addEventListener(eventName, wrapper, { once: true, passive: true });
        });
    }
    options.target.addEventListener(eventName, wrapper, { once: true, passive: true });

    /*  Retain a reference to the event listener, if a name is provided.
     */
    if (options.name) {
    	if (GW.eventListeners[eventName] == null)
    		GW.eventListeners[eventName] = { };

        GW.eventListeners[eventName][options.name] = {
        	wrapper: wrapper,
        	target: options.target
        };
    }

    return wrapper;
}

/*  Removes a named event listener from the page (or other target).
 */
function removeNamedEventListener(eventName, name) {
	if (GW.eventListeners[eventName] == null)
		return;

    let listener = GW.eventListeners[eventName][name];
    if (listener) {
        listener.target.removeEventListener(eventName, listener.wrapper);
        GW.eventListeners[eventName][name] = null;
    }
}

/*  Adds a scroll event listener to the page (or other target).
 */
function addScrollListener(fn, options) {
	return addNamedEventListener("scroll", fn, options);
}

/*  Removes a named scroll event listener from the page (or other target).
 */
function removeScrollListener(name) {
	removeNamedEventListener("scroll", name);
}

/*  Adds a resize event listener to the window.
 */
function addWindowResizeListener(fn, options) {
	options = Object.assign({
		target: window
	}, options);

	return addNamedEventListener("resize", fn, options);
}

/*  Removes a named resize event listener from the window.
 */
function removeWindowResizeListener(name) {
	removeNamedEventListener("resize", name);
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
    GWLog("updateScrollState", "inline.js", 3);

    GW.scrollState.newScrollTop = window.pageYOffset;
    GW.scrollState.unbrokenDownScrollDistance = GW.scrollState.newScrollTop > GW.scrollState.lastScrollTop
        										? (  GW.scrollState.unbrokenDownScrollDistance
        										   + GW.scrollState.newScrollTop
        										   - GW.scrollState.lastScrollTop)
        										: 0;
    GW.scrollState.unbrokenUpScrollDistance = GW.scrollState.newScrollTop < GW.scrollState.lastScrollTop
        									  ? (  GW.scrollState.unbrokenUpScrollDistance
        									     + GW.scrollState.lastScrollTop
        									     - GW.scrollState.newScrollTop)
        									  : 0;
    GW.scrollState.lastScrollTop = GW.scrollState.newScrollTop;
}
addScrollListener(updateScrollState, {
	name: "updateScrollStateScrollListener",
	defer: true, 
	ifDeferCallWhenAdd: true
});

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

    let preventScroll = (event) => {
    	document.documentElement.scrollTop = GW.scrollState.lastScrollTop;
    };

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
        addScrollListener(updateScrollState, {
        	name: "updateScrollStateScrollListener"
        });
    } else if (  !enable
               && isPageScrollingEnabled() == true) {
        document.documentElement.classList.toggle("scroll-enabled-not", true);
        removeScrollListener("updateScrollStateScrollListener");
        addScrollListener(preventScroll, {
        	name: "preventScroll"
        });
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
    let pageURL = URLFromString(location.href);
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
                | GW.contentDidInjectEventFlags.fullWidthPossible
                | GW.contentDidInjectEventFlags.localize)
    });
});
window.addEventListener("load", () => {
    GWLog("window.load", "browser event");
});
document.addEventListener("readystatechange", () => {
    GWLog("document.readyState." + document.readyState, "browser event");
});
