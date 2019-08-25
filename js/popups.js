// popups.js: standaline Javascript library for creating 'popups' which display link metadata (typically, title/author/date/summary), for extremely convenient reference/abstract reading.
// Author: Said Achmiz
// 2019
// license: MIT (derivative of footnotes.js, which is PD)

// popups.js parses a HTML document and looks for <a> links which have the 'docMetadata' attribute class, and the attributes 'data-popup-title', 'data-popup-author', 'data-popup-date', 'data-popup-doi', 'data-popup-abstract'.
// (These attributes are expected to be populated already by the HTML document's compiler, however, they can also be done dynamically. See 'wikipedia-popups.js' for an example of a library which does Wikipedia-only dynamically on page loads.)

// Whenever any such link is mouse-overed by the user, popups.js will pop up a large tooltip-like square with the contents of the attributes. This is particularly intended for references, where it is extremely convenient to autopopulate links such as to Arxiv.org/Biorxiv.org/Wikipedia with the link's title/author/date/abstract, so the reader can see it instantly.

// For an example of a Hakyll library which generates annotations for Wikipedia/Biorxiv/Arxiv/PDFs/arbitrarily-defined links, see https://www.gwern.net/LinkMetadata.hs ; for a live demonstration, see the links in https://www.gwern.net/newsletter/2019/07

document.querySelector("head").insertAdjacentHTML("beforeend", "<style>" + `
#popupdiv {
    z-index: 10001;
    font-size: 0.8em;
    box-shadow: 0 0 0 2px #fff;
    position: absolute;
    opacity: 1.0;
    transition: none;
}
#popupdiv.fading {
    opacity: 0.0;
    transition:
        opacity 0.75s ease-in 0.1s;
}

#popupdiv > div {
    background-color: #fff;
    padding: 12px 16px 14px 16px;
    max-width: 600px;
    max-height: calc(100vh - 32px);
    border: 3px double #aaa;
    line-height: 1.45;
    overflow: auto;
    overscroll-behavior: none;
}
#popupdiv > div .data-field {
    text-align: left;
    text-indent: 0;
    hyphens: none;
}
#popupdiv > div .data-field + .data-field {
    margin-top: 0.25em;
}
#popupdiv > div .data-field:empty {
    display: none;
}
#popupdiv > div .data-field.title {
    font-weight: bold;
    font-size: 1.125em;
}
#popupdiv > div .data-field.author-plus-date {
    font-style: italic;
}
#popupdiv > div .data-field.abstract {
    text-align: justify;
    text-indent: 2em;
    hyphens: auto;
}

#popupdiv > div .icon {
    background-image: none !important;
    position: relative;
    top: 0.15em;
    font-size: 1.125em;
}
#popupdiv > div .icon::after {
    margin: 0 0.175em 0 0;
    width: 1em;
    height: 1em;
    font-size: 1em;
}
#popupdiv > div .icon:not([href*='.pdf'])::after {
    background-position: center center;
    background-size: 100%;
}
#popupdiv > div .title-link::after {
    content: none;
}

/*  Scroll bar styles (Webkit/Blink only).
    */
#popupdiv > div::-webkit-scrollbar {
    width: 14px;
}
#popupdiv > div::-webkit-scrollbar-thumb {
    background-color: #ccc;
    box-shadow:
        0 0 0 3px #fff inset;
}
#popupdiv > div::-webkit-scrollbar-thumb:hover {
    background-color: #999;
}

@media only screen and (max-width: 64.9ch), not screen and (hover:hover) and (pointer:fine) {
    #popupdiv {
        display: none;
    }
}
` + "</style>");

Extracts = {
    contentContainerSelector: "#markdownBody",
    targetElementsSelector: "#markdownBody a.docMetadata",
    minPopupWidth: 480,
    popuptriggerdelay: 50,
    popupfadeoutdelay: 50,
    popupfadeoutduration: 400,
    popupfadetimeout: false,
    popupkilltimeout: false,
    popuptimeout: false,
    popup: null,
    unbind: function() {
        document.querySelectorAll(Extracts.targetElementsSelector).forEach(target => {
            //  Unbind existing mouseover/mouseout events, if any.
            target.removeEventListener("mouseover", Extracts.targetover);
            target.removeEventListener("mouseout", Extracts.targetout);
        });
    },
    setup: function() {
        // Unbind existing event listeners.
        Extracts.unbind();

        //  Get all targets.
        document.querySelectorAll(Extracts.targetElementsSelector).forEach(target => {
            //  Bind mousemover/mouseout events.
            target.addEventListener("mouseover", Extracts.targetover);
            target.addEventListener("mouseout", Extracts.targetout);

            // Remove the title attribute.
            target.removeAttribute("title");
        });
    },
    //  The mouseover event.
    targetover: (event) => {
        //  Stop the countdown to un-pop the popup.
        clearTimeout(Extracts.popupfadetimeout);
        clearTimeout(Extracts.popupkilltimeout);
        clearTimeout(Extracts.popuptimeout);

        Extracts.popuptimeout = setTimeout(() => {
            //  Get the target.
            let target = event.target.closest("a");
            var targetAbsoluteRect = target.getBoundingClientRect();
            let layoutParentSelector = matchMedia("(max-width: 176ch)").matches ? "main" : "#markdownBody";
            let layoutParent = document.querySelector(layoutParentSelector);
            let layoutParentAbsoluteRect = layoutParent.getBoundingClientRect();
            var targetPosition = {
                left: (targetAbsoluteRect.left - layoutParentAbsoluteRect.left),
                top: (targetAbsoluteRect.top - layoutParentAbsoluteRect.top)
            };

            //  Get, or create, the popup.
            Extracts.popup = document.querySelector("#popupdiv");
            if (Extracts.popup) {
                Extracts.popup.classList.remove("fading");
                Extracts.popup.remove();
            } else {
                Extracts.popup = document.createElement('div');
                Extracts.popup.id = "popupdiv";
                Extracts.popup.className = target.className;
            }

            //  Inject the contents of the popup into the popup div.
            Extracts.popup.innerHTML =
                `<div>
                    <p class='data-field title'><a class='icon' target='_new' href='${target.href}' title='Open this reference in a new window'></a><a class='title-link' href='${target.href}' title='${target.href}'>${target.dataset.popupTitle || ""}</a></p>
                    <p class='data-field author-plus-date'>${target.dataset.popupAuthor || ""}${target.dataset.popupDate ? (" (" + target.dataset.popupDate + ")") : ""}</p>
                    <div class='data-field abstract'>${target.dataset.popupAbstract || ""}</div>
                </div>`;

            //  Inject the popup into the page.
            document.querySelector(Extracts.contentContainerSelector).appendChild(Extracts.popup);

            //  Add event listeners.
            Extracts.popup.addEventListener("mouseover", Extracts.divover);
            Extracts.popup.addEventListener("mouseout", Extracts.targetout);

            /*  How much “breathing room” to give the target (i.e., offset of
                the popup).
                */
            var popupBreathingRoom = {
                x:  (Math.round(targetAbsoluteRect.height) * 1.25),
                y:  (Math.round(targetAbsoluteRect.height) * 1.25)
            };

            /*  Set the horizontal position first; this causes the popup to be
                laid out, and the layout engine calculates the height for us.
                */
            var popupLeft = targetPosition.left;
            if (popupLeft + Extracts.minPopupWidth > layoutParentAbsoluteRect.width)
                popupLeft = layoutParentAbsoluteRect.width - Extracts.minPopupWidth;
            Extracts.popup.style.left = popupLeft + "px";
            //  Correct for various positioning aberrations.
//          if (Extracts.popup.getBoundingClientRect().right > layoutParentAbsoluteRect.width)
//              Extracts.popup.style.maxWidth = (Extracts.popup.clientWidth - (Extracts.popup.getBoundingClientRect().right - layoutParentAbsoluteRect.width) - parseInt(getComputedStyle(Extracts.popup.firstElementChild).paddingRight)) + "px";
//          else if (targetPosition.left + Extracts.popup.clientWidth < layoutParentAbsoluteRect.width)
//              Extracts.popup.style.left = (targetPosition.left) + "px";
//          else if (targetPosition.left - (Extracts.popup.clientWidth) > Extracts.popup.getBoundingClientRect().left)
//              Extracts.popup.style.left = (targetPosition.left - Extracts.popup.clientWidth) + "px";

            //  Now we know how tall the popup is...
            var provisionalExtractPopupHeight = Extracts.popup.clientHeight;

            //  Determining vertical position is full of edge cases.
            var popupTop = targetPosition.top + popupBreathingRoom.y;
            if (popupTop + provisionalExtractPopupHeight > window.innerHeight + window.scrollY - layoutParent.offsetTop) {
                popupTop -= (provisionalExtractPopupHeight + popupBreathingRoom.y);
            }
            if (top + provisionalExtractPopupHeight > window.innerHeight + window.scrollY ||
                provisionalExtractPopupHeight == window.innerHeight ||
                popupTop < window.scrollY - layoutParent.offsetTop) {
                popupTop = window.scrollY - layoutParent.offsetTop;
            }
            if (popupTop + provisionalExtractPopupHeight + 120 < targetPosition.top) {
                popupTop = targetPosition.top - provisionalExtractPopupHeight;
            } else if (top > targetPosition.top) {
                popupTop -= 90;
            }
            if (popupTop < 0) {
                popupTop = 0;
            }
            Extracts.popup.style.top = popupTop + "px";
        }, Extracts.popuptriggerdelay);
    },
    //  The mouseout event.
    targetout: (event) => {
     clearTimeout(Extracts.popupfadetimeout);
     clearTimeout(Extracts.popupkilltimeout);
     clearTimeout(Extracts.popuptimeout);

     if (!Extracts.popup) return;

     Extracts.popupfadetimeout = setTimeout(() => {
         Extracts.popup.classList.add("fading");
         Extracts.popupkilltimeout = setTimeout(() => {
             Extracts.popup.classList.remove("fading");
             Extracts.popup.remove();
         }, Extracts.popupfadeoutduration);
     }, Extracts.popupfadeoutdelay);
    },
    //  The “user moved mouse back into popup” mouseover event.
    divover: (event) => {
     clearTimeout(Extracts.popupfadetimeout);
     clearTimeout(Extracts.popupkilltimeout);
     clearTimeout(Extracts.popuptimeout);
     Extracts.popup.classList.remove("fading");
    }
}

if (document.readyState == "complete") {
    Extracts.setup();
} else {
    window.addEventListener("load", Extracts.setup);
}
