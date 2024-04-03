/*	This file should be loaded after all other extracts*.js files.
 */

Extracts.config = {
    /*  Selector for containers within which targets may be found.
     */
    contentContainersSelector: [
    	".markdownBody",
    	"#TOC",
    	"#page-metadata",
    	"#sidebar"
    ].join(", "),

	/*	Selector for containers within which targets may not be found.
	 */
    excludedContainerElementsSelector: "h1, h2, h3, h4, h5, h6",

	/*	Selector for targets.
	 */
	targetElementsSelector: "a[href]",

	/*	Elements that shouldn’t be targets.
	 */
	excludedElementsSelector: [
		".section-self-link",
		".footnote-self-link",
		".sidenote-self-link",
		"[aria-hidden='true']",
		"[href$='#top']",
		".extract-not"
	].join(", "),

	/*	Don’t display indicator hooks on links in these containers.
	 */
	hooklessLinksContainersSelector: [
		"body.index #markdownBody",
		"#sidebar",
		".TOC",
		"#floating-header"
	].join(", ")
};

GW.notificationCenter.fireEvent("Extracts.didLoad");

Extracts.setup();
