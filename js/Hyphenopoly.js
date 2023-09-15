/**
 * @license Hyphenopoly 5.0.0-beta.4 - client side hyphenation for webbrowsers
 * ©2022  Mathias Nater, Güttingen (mathiasnater at gmail dot com)
 * https://github.com/mnater/Hyphenopoly
 *
 * Released under the MIT license
 * https://github.com/mnater/Hyphenopoly/blob/master/LICENSE
 */

/* globals Hyphenopoly:readonly */
((w, o) => {
    "use strict";
    const SOFTHYPHEN = "\u00AD";

    /**
     * Event
     */
    const event = ((H) => {
        const knownEvents = new Map([
            ["afterElementHyphenation", []],
            ["beforeElementHyphenation", []],
            ["engineReady", []],
            [
                "error", [
                    (e) => {
                        if (e.runDefault) {
                            w.console.warn(e);
                        }
                    }
                ]
            ],
            ["hyphenopolyEnd", []],
            ["hyphenopolyStart", []]
        ]);
        if (H.hev) {
            const userEvents = new Map(o.entries(H.hev));
            knownEvents.forEach((eventFuncs, eventName) => {
                if (userEvents.has(eventName)) {
                    eventFuncs.unshift(userEvents.get(eventName));
                }
            });
        }
        return {
            "fire": ((eventName, eventData) => {
                eventData.runDefault = true;
                eventData.preventDefault = () => {
                    eventData.runDefault = false;
                };
                knownEvents.get(eventName).forEach((eventFn) => {
                    eventFn(eventData);
                });
            })
        };
    })(Hyphenopoly);

    /**
     * Register copy event on element
     * @param {Object} el The element
     * @returns {undefined}
     */
    function registerOnCopy(el) {
        el.addEventListener(
            "copy",
            (e) => {
                e.preventDefault();
                const sel = w.getSelection();
                const div = document.createElement("div");
                div.appendChild(sel.getRangeAt(0).cloneContents());
                e.clipboardData.setData("text/plain", sel.toString().replace(RegExp(SOFTHYPHEN, "g"), ""));
                e.clipboardData.setData("text/html", div.innerHTML.replace(RegExp(SOFTHYPHEN, "g"), ""));
            },
            true
        );
    }

    /**
     * Convert settings from H.setup-Object to Map
     * This is a IIFE to keep complexity low.
     */
    ((H) => {
        /**
         * Create a Map with a default Map behind the scenes. This mimics
         * kind of a prototype chain of an object, but without the object-
         * injection security risk.
         *
         * @param {Map} defaultsMap - A Map with default values
         * @returns {Proxy} - A Proxy for the Map (dot-notation or get/set)
         */
        function createMapWithDefaults(defaultsMap) {
            const userMap = new Map();

            /**
             * The get-trap: get the value from userMap or else from defaults
             * @param {Sring} key - The key to retrieve the value for
             * @returns {*}
             */
            function get(key) {
                return (userMap.has(key))
                    ? userMap.get(key)
                    : defaultsMap.get(key);
            }

            /**
             * The set-trap: set the value to userMap and don't touch defaults
             * @param {Sring} key - The key for the value
             * @param {*} value - The value
             * @returns {*}
             */
            function set(key, value) {
                userMap.set(key, value);
            }
            return new Proxy(defaultsMap, {
                "get": (_target, prop) => {
                    if (prop === "set") {
                        return set;
                    }
                    if (prop === "get") {
                        return get;
                    }
                    return get(prop);
                },
                "ownKeys": () => {
                    return [
                        ...new Set(
                            [...defaultsMap.keys(), ...userMap.keys()]
                        )
                    ];
                }
            });
        }

        const settings = createMapWithDefaults(new Map([
            ["defaultLanguage", "en-us"],
            [
                "dontHyphenate", (() => {
                    const list = "abbr,acronym,audio,br,button,code,img,input,kbd,label,math,option,pre,samp,script,style,sub,sup,svg,textarea,var,video";
                    return createMapWithDefaults(
                        new Map(list.split(",").map((val) => {
                            return [val, true];
                        }))
                    );
                })()
            ],
            ["dontHyphenateClass", "donthyphenate"],
            ["exceptions", new Map()],
            ["keepAlive", true],
            ["normalize", false],
            ["processShadows", false],
            ["safeCopy", true],
            ["substitute", new Map()],
            ["timeout", 1000]
        ]));
        o.entries(H.s).forEach(([key, value]) => {
            switch (key) {
            case "selectors":
                // Set settings.selectors to array of selectors
                settings.set("selectors", o.keys(value));

                /*
                 * For each selector add a property to settings with
                 * selector specific settings
                 */
                o.entries(value).forEach(([sel, selSettings]) => {
                    const selectorSettings = createMapWithDefaults(new Map([
                        ["compound", "hyphen"],
                        ["hyphen", SOFTHYPHEN],
                        ["leftmin", 0],
                        ["leftminPerLang", 0],
                        ["minWordLength", 6],
                        ["mixedCase", true],
                        ["orphanControl", 1],
                        ["rightmin", 0],
                        ["rightminPerLang", 0]
                    ]));
                    o.entries(selSettings).forEach(
                        ([selSetting, setVal]) => {
                            if (typeof setVal === "object") {
                                selectorSettings.set(
                                    selSetting,
                                    new Map(o.entries(setVal))
                                );
                            } else {
                                selectorSettings.set(selSetting, setVal);
                            }
                        }
                    );
                    settings.set(sel, selectorSettings);
                });
                break;
            case "dontHyphenate":
            case "exceptions":
                o.entries(value).forEach(([k, v]) => {
                    settings.get(key).set(k, v);
                });
                break;
            case "substitute":
                o.entries(value).forEach(([lang, subst]) => {
                    settings.substitute.set(
                        lang,
                        new Map(o.entries(subst))
                    );
                });
                break;
            default:
                settings.set(key, value);
            }
        });
        H.c = settings;
    })(Hyphenopoly);

    ((H) => {
        const C = H.c;
        let mainLanguage = null;

        event.fire(
            "hyphenopolyStart",
            {
                "msg": "hyphenopolyStart"
            }
        );

        /**
         * Factory for elements
         * @returns {Object} elements-object
         */
        function makeElementCollection() {
            const list = new Map();

            /*
             * Counter counts the elements to be hyphenated.
             * Needs to be an object (Pass by reference)
             */
            const counter = [0];

            /**
             * Add element to elements
             * @param {object} el The element
             * @param {string} lang The language of the element
             * @param {string} sel The selector of the element
             * @returns {Object} An element-object
             */
            function add(el, lang, sel) {
                const elo = {
                    "element": el,
                    "selector": sel
                };
                if (!list.has(lang)) {
                    list.set(lang, []);
                }
                list.get(lang).push(elo);
                counter[0] += 1;
                return elo;
            }

            /**
             * Removes elements from the list and updates the counter
             * @param {string} lang - The lang of the elements to remove
             */
            function rem(lang) {
                let langCount = 0;
                if (list.has(lang)) {
                    langCount = list.get(lang).length;
                    list.delete(lang);
                    counter[0] -= langCount;
                    if (counter[0] === 0) {
                        event.fire(
                            "hyphenopolyEnd",
                            {
                                "msg": "hyphenopolyEnd"
                            }
                        );
                        if (!C.keepAlive) {
                            window.Hyphenopoly = null;
                        }
                    }
                }
            }

            return {
                add,
                counter,
                list,
                rem
            };
        }

        /**
         * Get language of element by searching its parents or fallback
         * @param {Object} el The element
         * @param {string} parentLang Lang of parent if available
         * @param {boolean} fallback Will falback to mainlanguage
         * @returns {string|null} The language or null
         */
        function getLang(el, parentLang = "", fallback = true) {
            // Find closest el with lang attr not empty
            el = el.closest("[lang]:not([lang=''])");
            if (el && el.lang) {
                return el.lang.toLowerCase();
            }
            if (parentLang) {
                return parentLang;
            }
            return (fallback)
                ? mainLanguage
                : null;
        }

        /**
         * Collect elements that have a selector defined in C.selectors
         * and add them to elements.
         * @param {Object} [parent = null] The start point element
         * @param {string} [selector = null] The selector matching the parent
         * @returns {Object} elements-object
         */
        function collectElements(parent = null, selector = null) {
            const elements = makeElementCollection();

            const dontHyphenateSelector = (() => {
                let s = "." + C.dontHyphenateClass;
                o.getOwnPropertyNames(C.dontHyphenate).forEach((tag) => {
                    if (C.dontHyphenate.get(tag)) {
                        s += "," + tag;
                    }
                });
                return s;
            })();
            const matchingSelectors = C.selectors.join(",") + "," + dontHyphenateSelector;

            /**
             * Recursively walk all elements in el, lending lang and selName
             * add them to elements if necessary.
             * @param {Object} el The element to scan
             * @param {string} pLang The language of the parent element
             * @param {string} sel The selector of the parent element
             * @param {boolean} isChild If el is a child element
             * @returns {undefined}
             */
            function processElements(el, pLang, sel, isChild = false) {
                const eLang = getLang(el, pLang);
                const langDef = H.cf.langs.get(eLang);
                if (langDef === "H9Y") {
                    elements.add(el, eLang, sel);
                    if (!isChild && C.safeCopy) {
                        registerOnCopy(el);
                    }
                } else if (!langDef && eLang !== "zxx") {
                    event.fire(
                        "error",
                        Error(`Element with '${eLang}' found, but '${eLang}.wasm' not loaded. Check language tags!`)
                    );
                }
                el.childNodes.forEach((n) => {
                    if (n.nodeType === 1 && !n.matches(matchingSelectors)) {
                        processElements(n, eLang, sel, true);
                    }
                });
            }

            /**
             * Searches the DOM for each sel
             * @param {object} root The DOM root
             * @returns {undefined}
             */
            function getElems(root) {
                C.selectors.forEach((sel) => {
                    root.querySelectorAll(sel).forEach((n) => {
                        processElements(n, getLang(n), sel, false);
                    });
                });
            }

            if (parent === null) {
                if (C.processShadows) {
                    w.document.querySelectorAll("*").forEach((m) => {
                        if (m.shadowRoot) {
                            getElems(m.shadowRoot);
                        }
                    });
                }
                getElems(w.document);
            } else {
                processElements(parent, getLang(parent), selector);
            }
            return elements;
        }

        const wordHyphenatorPool = new Map();

        /**
         * Factory for hyphenatorFunctions for a specific language and selector
         * @param {Object} lo Language-Object
         * @param {string} lang The language
         * @param {string} sel The selector
         * @returns {function} The hyphenate function
         */
        function createWordHyphenator(lo, lang, sel) {
            const poolKey = lang + "-" + sel;
            if (wordHyphenatorPool.has(poolKey)) {
                return wordHyphenatorPool.get(poolKey);
            }

            const selSettings = C.get(sel);
            lo.cache.set(sel, new Map());

            /**
             * HyphenateFunction for non-compound words
             * @param {string} word The word
             * @returns {string} The hyphenated word
             */
            function hyphenateNormal(word) {
                if (word.length > 61) {
                    event.fire(
                        "error",
                        Error("Found word longer than 61 characters")
                    );
                } else if (!lo.reNotAlphabet.test(word)) {
                    return lo.hyphenate(
                        word,
                        selSettings.hyphen.charCodeAt(0),
                        selSettings.leftminPerLang.get(lang),
                        selSettings.rightminPerLang.get(lang)
                    );
                }
                return word;
            }

            /**
             * HyphenateFunction for compound words
             * @param {string} word The word
             * @returns {string} The hyphenated compound word
             */
            function hyphenateCompound(word) {
                const zeroWidthSpace = "\u200B";
                let parts = null;
                let wordHyphenator = null;
                if (selSettings.compound === "auto" ||
                    selSettings.compound === "all") {
                    wordHyphenator = createWordHyphenator(lo, lang, sel);
                    parts = word.split("-").map((p) => {
                        if (p.length >= selSettings.minWordLength) {
                            return wordHyphenator(p);
                        }
                        return p;
                    });
                    if (selSettings.compound === "auto") {
                        word = parts.join("-");
                    } else {
                        word = parts.join("-" + zeroWidthSpace);
                    }
                } else {
                    word = word.replace("-", "-" + zeroWidthSpace);
                }
                return word;
            }

            /**
             * Checks if a string is mixed case
             * @param {string} s The string
             * @returns {boolean} true if s is mixed case
             */
            function isMixedCase(s) {
                return [...s].map((c) => {
                    return (c === c.toLowerCase());
                }).some((v, i, a) => {
                    return (v !== a[0]);
                });
            }

            /**
             * HyphenateFunction for words (compound or not)
             * @param {string} word The word
             * @returns {string} The hyphenated word
             */
            function hyphenator(word) {
                let hw = lo.cache.get(sel).get(word);
                if (!hw) {
                    if (lo.exc.has(word)) {
                        hw = lo.exc.get(word).replace(
                            /-/g,
                            selSettings.hyphen
                        );
                    } else if (!selSettings.mixedCase && isMixedCase(word)) {
                        hw = word;
                    } else if (word.indexOf("-") === -1) {
                        hw = hyphenateNormal(word);
                    } else {
                        hw = hyphenateCompound(word);
                    }
                    lo.cache.get(sel).set(word, hw);
                }
                return hw;
            }
            wordHyphenatorPool.set(poolKey, hyphenator);
            return hyphenator;
        }

        const orphanControllerPool = new Map();

        /**
         * Factory for function that handles orphans
         * @param {string} sel The selector
         * @returns {function} The function created
         */
        function createOrphanController(sel) {
            if (orphanControllerPool.has(sel)) {
                return orphanControllerPool.get(sel);
            }
            const selSettings = C.get(sel);

            /**
             * Function template
             * @param {string} ignore unused result of replace
             * @param {string} leadingWhiteSpace The leading whiteSpace
             * @param {string} lastWord The last word
             * @param {string} trailingWhiteSpace The trailing whiteSpace
             * @returns {string} Treated end of text
             */
            function controlOrphans(
                ignore,
                leadingWhiteSpace,
                lastWord,
                trailingWhiteSpace
            ) {
                if (selSettings.orphanControl === 3 && leadingWhiteSpace === " ") {
                    // \u00A0 = no-break space (nbsp)
                    leadingWhiteSpace = "\u00A0";
                }
                return leadingWhiteSpace + lastWord.replace(RegExp(selSettings.hyphen, "g"), "") + trailingWhiteSpace;
            }
            orphanControllerPool.set(sel, controlOrphans);
            return controlOrphans;
        }

        const wordRegExpPool = new Map();

        /**
         * Hyphenate an entitiy (text string or Element-Object)
         * @param {string} lang - the language of the string
         * @param {string} sel - the selectorName of settings
         * @param {string} entity - the entity to be hyphenated
         * @returns {string | null} hyphenated str according to setting of sel
         */
        function hyphenate(lang, sel, entity) {
            const lo = H.languages.get(lang);
            const selSettings = C.get(sel);
            const minWordLength = selSettings.minWordLength;


            const regExpWord = (() => {
                const key = lang + minWordLength;
                if (wordRegExpPool.has(key)) {
                    return wordRegExpPool.get(key);
                }

                /*
                 * Transpiled RegExp of
                 * /[${alphabet}\p{Mn}Subset\p{Letter}\00AD-]
                 * {${minwordlength},}/gui
                 */
                const reWord = RegExp(
                    `[${lo.alphabet}a-z\u0300-\u036F\u0483-\u0487\u00DF-\u00F6\u00F8-\u00FE\u0101\u0103\u0105\u0107\u0109\u010D\u010F\u0111\u0113\u0117\u0119\u011B\u011D\u011F\u0123\u0125\u012B\u012F\u0131\u0135\u0137\u013C\u013E\u0142\u0144\u0146\u0148\u014D\u0151\u0153\u0155\u0159\u015B\u015D\u015F\u0161\u0165\u016B\u016D\u016F\u0171\u0173\u017A\u017C\u017E\u017F\u01CE\u01D0\u01D2\u01D4\u01D6\u01D8\u01DA\u01DC\u0219\u021B\u02BC\u0390\u03AC-\u03CE\u03D0\u03E3\u03E5\u03E7\u03E9\u03EB\u03ED\u03EF\u03F2\u0430-\u044F\u0451-\u045C\u045E\u045F\u0491\u04AF\u04E9\u0561-\u0585\u0587\u0905-\u090C\u090F\u0910\u0913-\u0928\u092A-\u0930\u0932\u0933\u0935-\u0939\u093D\u0960\u0961\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A85-\u0A8B\u0A8F\u0A90\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AE0\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B60\u0B61\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB5\u0BB7-\u0BB9\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D28\u0D2A-\u0D39\u0D60\u0D61\u0D7A-\u0D7F\u0E01-\u0E2E\u0E30\u0E32\u0E33\u0E40-\u0E45\u10D0-\u10F0\u1200-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u1E0D\u1E37\u1E41\u1E43\u1E45\u1E47\u1E6D\u1F00-\u1F07\u1F10-\u1F15\u1F20-\u1F27\u1F30-\u1F37\u1F40-\u1F45\u1F50-\u1F57\u1F60-\u1F67\u1F70-\u1F7D\u1F80-\u1F87\u1F90-\u1F97\u1FA0-\u1FA7\u1FB2-\u1FB4\u1FB6\u1FB7\u1FC2-\u1FC4\u1FC6\u1FC7\u1FD2\u1FD3\u1FD6\u1FD7\u1FE2-\u1FE7\u1FF2-\u1FF4\u1FF6\u1FF7\u2C81\u2C83\u2C85\u2C87\u2C89\u2C8D\u2C8F\u2C91\u2C93\u2C95\u2C97\u2C99\u2C9B\u2C9D\u2C9F\u2CA1\u2CA3\u2CA5\u2CA7\u2CA9\u2CAB\u2CAD\u2CAF\u2CB1\u2CC9\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\u00AD\u200B-\u200D-]{${minWordLength},}`, "gui"
                );
                wordRegExpPool.set(key, reWord);
                return reWord;
            })();

            /**
             * Hyphenate text according to setting in sel
             * @param {string} text - the strint to be hyphenated
             * @returns {string} hyphenated string according to setting of sel
             */
            function hyphenateText(text) {
                if (C.normalize) {
                    text = text.normalize("NFC");
                }
                let tn = text.replace(
                    regExpWord,
                    createWordHyphenator(lo, lang, sel)
                );
                if (selSettings.orphanControl !== 1) {
                    tn = tn.replace(
                        /(\u0020*)(\S+)(\s*)$/,
                        createOrphanController(sel)
                    );
                }
                return tn;
            }

            /**
             * Hyphenate element according to setting in sel
             * @param {object} el - the HTMLElement to be hyphenated
             * @returns {undefined}
             */
            function hyphenateElement(el) {
                event.fire(
                    "beforeElementHyphenation",
                    {
                        el,
                        lang
                    }
                );
                el.childNodes.forEach((n) => {
                    if (
                        n.nodeType === 3 &&
                        (/\S/).test(n.data) &&
                        n.data.length >= minWordLength
                    ) {
                        n.data = hyphenateText(n.data);
                    }
                });
                H.res.els.counter[0] -= 1;
                event.fire(
                    "afterElementHyphenation",
                    {
                        el,
                        lang
                    }
                );
            }
            let r = null;
            if (typeof entity === "string") {
                r = hyphenateText(entity);
            } else if (entity instanceof HTMLElement) {
                hyphenateElement(entity);
            }
            return r;
        }

        /**
         * Creates a language-specific string hyphenator
         * @param {String} lang - The language this hyphenator hyphenates
         */
        function createStringHyphenator(lang) {
            return ((entity, sel = ".hyphenate") => {
                if (typeof entity !== "string") {
                    event.fire(
                        "error",
                        Error("This use of hyphenators is deprecated. See https://mnater.github.io/Hyphenopoly/Hyphenators.html")
                    );
                }
                return hyphenate(lang, sel, entity);
            });
        }

        /**
         * Creates a polyglot HTML hyphenator
         */
        function createDOMHyphenator() {
            return ((entity, sel = ".hyphenate") => {
                collectElements(entity, sel).list.forEach((els, l) => {
                    els.forEach((elo) => {
                        hyphenate(l, elo.selector, elo.element);
                    });
                });
                return null;
            });
        }

        H.unhyphenate = () => {
            H.res.els.list.forEach((els) => {
                els.forEach((elo) => {
                    const n = elo.element.firstChild;
                    n.data = n.data.replace(RegExp(C[elo.selector].hyphen, "g"), "");
                });
            });
            return Promise.resolve(H.res.els);
        };

        /**
         * Hyphenate all elements with a given language
         * @param {string} lang The language
         * @param {Array} elArr Array of elements
         * @returns {undefined}
         */
        function hyphenateLangElements(lang, elements) {
            const elArr = elements.list.get(lang);
            if (elArr) {
                elArr.forEach((elo) => {
                    hyphenate(lang, elo.selector, elo.element);
                });
            } else {
                event.fire(
                    "error",
                    Error(`Engine for language '${lang}' loaded, but no elements found.`)
                );
            }
            if (elements.counter[0] === 0) {
                w.clearTimeout(H.timeOutHandler);
                H.hide(0, null);
                event.fire(
                    "hyphenopolyEnd",
                    {
                        "msg": "hyphenopolyEnd"
                    }
                );
                if (!C.keepAlive) {
                    window.Hyphenopoly = null;
                }
            }
        }

        /**
         * Convert the exceptions from user input to Map
         * @param {string} lang - The language for which the Map is created
         * @return {Map}
         */
        function createExceptionMap(lang) {
            let exc = "";
            if (C.exceptions.has(lang)) {
                exc = C.exceptions.get(lang);
            }
            if (C.exceptions.has("global")) {
                if (exc === "") {
                    exc = C.exceptions.get("global");
                } else {
                    exc += ", " + C.exceptions.get("global");
                }
            }
            if (exc === "") {
                return new Map();
            }
            return new Map(exc.split(", ").map((e) => {
                return [e.replace(/-/g, ""), e];
            }));
        }

        /**
         * Setup lo
         * @param {string} lang The language
         * @param {function} hyphenateFunction The hyphenateFunction
         * @param {string} alphabet List of used characters
         * @param {number} leftmin leftmin
         * @param {number} rightmin rightmin
         * @returns {undefined}
         */
        function prepareLanguagesObj(
            lang,
            hyphenateFunction,
            alphabet,
            patternLeftmin,
            patternRightmin
        ) {
            C.selectors.forEach((sel) => {
                const selSettings = C.get(sel);
                if (selSettings.leftminPerLang === 0) {
                    selSettings.set("leftminPerLang", new Map());
                }
                if (selSettings.rightminPerLang === 0) {
                    selSettings.set("rightminPerLang", new Map());
                }
                selSettings.leftminPerLang.set(lang, Math.max(
                    patternLeftmin,
                    selSettings.leftmin,
                    Number(selSettings.leftminPerLang.get(lang)) || 0
                ));

                selSettings.rightminPerLang.set(lang, Math.max(
                    patternRightmin,
                    selSettings.rightmin,
                    Number(selSettings.rightminPerLang.get(lang)) || 0
                ));
            });
            if (!H.languages) {
                H.languages = new Map();
            }
            alphabet = alphabet.replace(/\\*-/g, "\\-");
            H.languages.set(lang, {
                alphabet,
                "cache": new Map(),
                "exc": createExceptionMap(lang),
                "hyphenate": hyphenateFunction,
                "ready": true,
                "reNotAlphabet": RegExp(`[^${alphabet}]`, "i")
            });
            H.hy6ors.get(lang).resolve(createStringHyphenator(lang));
            event.fire(
                "engineReady",
                {
                    lang
                }
            );
            if (H.res.els) {
                hyphenateLangElements(lang, H.res.els);
            }
        }

        const decode = (() => {
            const utf16ledecoder = new TextDecoder("utf-16le");
            return ((ui16) => {
                return utf16ledecoder.decode(ui16);
            });
        })();

        /**
         * Setup env for hyphenateFunction
         * @param {ArrayBuffer} buf Memory buffer
         * @param {function} hyphenateFunc hyphenateFunction
         * @returns {function} hyphenateFunction with closured environment
         */
        function encloseHyphenateFunction(buf, hyphenateFunc) {
            const wordStore = new Uint16Array(buf, 0, 64);
            return ((word, hyphencc, leftmin, rightmin) => {
                wordStore.set([
                    46,
                    ...[...word].map((c) => {
                        return c.charCodeAt(0);
                    }),
                    46,
                    0
                ]);
                const len = hyphenateFunc(leftmin, rightmin, hyphencc);
                if (len > 0) {
                    word = decode(
                        new Uint16Array(buf, 0, len)
                    );
                }
                return word;
            });
        }

        /**
         * Instantiate Wasm Engine
         * @param {string} lang The language
         * @returns {undefined}
         */
        function instantiateWasmEngine(heProm, lang) {
            const wa = window.WebAssembly;

            /**
             * Register character substitutions in the .wasm-hyphenEngine
             * @param {number} alphalen - The length of the alphabet
             * @param {object} exp - Export-object of the hyphenEngine
             */
            function registerSubstitutions(alphalen, exp) {
                if (C.substitute.has(lang)) {
                    const subst = C.substitute.get(lang);
                    subst.forEach((substituer, substituted) => {
                        const substitutedU = substituted.toUpperCase();
                        const substitutedUcc = (substitutedU === substituted)
                            ? 0
                            : substitutedU.charCodeAt(0);
                        alphalen = exp.subst(
                            substituted.charCodeAt(0),
                            substitutedUcc,
                            substituer.charCodeAt(0)
                        );
                    });
                }
                return alphalen;
            }

            /**
             * Instantiate the hyphenEngine
             * @param {object} res - The fetched ressource
             */
            function handleWasm(res) {
                const exp = res.instance.exports;
                // eslint-disable-next-line multiline-ternary
                let alphalen = (wa.Global) ? exp.lct.value : exp.lct;
                alphalen = registerSubstitutions(alphalen, exp);
                heProm.l.forEach((l) => {
                    prepareLanguagesObj(
                        l,
                        encloseHyphenateFunction(
                            exp.mem.buffer,
                            exp.hyphenate
                        ),
                        decode(new Uint16Array(exp.mem.buffer, 1280, alphalen)),
                        /* eslint-disable multiline-ternary */
                        (wa.Global) ? exp.lmi.value : exp.lmi,
                        (wa.Global) ? exp.rmi.value : exp.rmi
                        /* eslint-enable multiline-ternary */
                    );
                });
            }
            heProm.w.then((response) => {
                if (response.ok) {
                    if (
                        wa.instantiateStreaming &&
                        (response.headers.get("Content-Type") === "application/wasm")
                    ) {
                        return wa.instantiateStreaming(response);
                    }
                    return response.arrayBuffer().then((ab) => {
                        return wa.instantiate(ab);
                    });
                }
                return Promise.reject(Error(`File ${lang}.wasm can't be loaded from ${H.paths.patterndir}`));
            }).then(handleWasm, (e) => {
                event.fire("error", e);
                H.res.els.rem(lang);
            });
        }

        H.main = () => {
            H.res.DOM.then(() => {
                mainLanguage = getLang(w.document.documentElement, "", false);
                if (!mainLanguage && C.defaultLanguage !== "") {
                    mainLanguage = C.defaultLanguage;
                }
                const elements = collectElements();
                H.res.els = elements;
                elements.list.forEach((ignore, lang) => {
                    if (H.languages &&
                        H.languages.has(lang) &&
                        H.languages.get(lang).ready
                    ) {
                        hyphenateLangElements(lang, elements);
                    }
                });
            });

            H.res.he.forEach(instantiateWasmEngine);

            Promise.all(
                // Make sure all lang specific hyphenators and DOM are ready
                [...H.hy6ors.entries()].
                    reduce((accumulator, value) => {
                        if (value[0] !== "HTML") {
                            return accumulator.concat(value[1]);
                        }
                        return accumulator;
                    }, []).
                    concat(H.res.DOM)
            ).then(() => {
                H.hy6ors.get("HTML").resolve(createDOMHyphenator());
            }, (e) => {
                event.fire("error", e);
            });
        };
        H.main();
    })(Hyphenopoly);
})(window, Object);
