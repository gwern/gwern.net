/*******************/
/*	Cosmetic styles.
	NOTE: These are currently unused on gwern.net (see the injectDefaultStyles
	config variable).
	*/
Extracts.popupDefaultStylesHTML = `<style id='${Extracts.popupStylesID}-default'>
:root {
	--GW-popups-body-background-color: #fff;
	--GW-popups-popup-background-color: #fff;
	--GW-popups-popup-border-color: #aaa;
	--GW-popups-popup-link-hover-color: #888;
	--GW-popups-popup-scrollbar-thumb-color: #ccc;
	--GW-popups-popup-scrollbar-track-color: #fff;
	--GW-popups-popup-scrollbar-thumb-hover-color: #999;
}

#popupdiv {
    font-size: 0.8em;
    box-shadow: 0 0 0 3px var(--GW-popups-body-background-color);
}
#popupdiv > div {
    padding: 0.75em 1em 0.875em 1em;
    border: 3px double var(--GW-popups-popup-border-color);
    background-color: var(--GW-popups-popup-background-color);
    line-height: 1.45;
}

/* TODO: the popups should ideally inherit from the regular CSS once the #markdownBody class is rewritten, and the underlining can be removed */
#popupdiv a {
	text-decoration: underline;
}
#popupdiv a:hover {
	color: var(--GW-popups-popup-link-hover-color);
}

#popupdiv > div .data-field.title {
    font-weight: bold;
    font-size: 1.125em;
}
#popupdiv > div .data-field.author {
    font-style: italic;
}

#popupdiv > div.popup-section-embed,
#popupdiv > div.popup-citation-context {
    padding: 0.75em 1.5em 0.875em 1.5em;
}

#popupdiv > div .icon {
    top: 0.15em;
    font-size: 1.125em;
}

/* TODO: the 4-letter block link icons defined in 'links.css' interact badly with the popup links, so we have to manually filter them out pending a rewrite: */
#popupdiv > div .icon:not([href*='sagepub.com']):not([href*='pnas.org']):not([href*='xkcd.com']):not([href*='rand.org']):not([href*='www.cell.com']):not([href*='publicdomainreview.org']):not([href*='mlp.fandom.com']):not([href*='www.nber.org'])::after {
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
    width: 0.875em;
}
#popupdiv > div::-webkit-scrollbar-thumb {
    background-color: var(--GW-popups-popup-scrollbar-thumb-color);
    box-shadow:
        0 0 0 3px var(--GW-popups-popup-scrollbar-track-color) inset;
}
#popupdiv > div::-webkit-scrollbar-thumb:hover {
    background-color: var(--GW-popups-popup-scrollbar-thumb-hover-color);
}

#popupdiv .originalURL {
  font-size: 0.75em;
}

#popupdiv .iaMirror {
	padding-right: 0.3em;
}

#popupdiv > div .data-field.abstract > p,
#popupdiv > div.popup-section-embed p,
#popupdiv > div.popup-citation-context p {
    text-align: justify;
    text-indent: 1em;
    hyphens: auto;
}
</style>`;

