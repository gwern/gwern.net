/*
 * Title: 404 Error Page URL Suggester
 * Author: Gwern Branwen
 * Date: 2024-06-25
 * When:  Time-stamp: "2025-03-03 12:16:47 gwern"
 * License: CC-0
 *
 * This script enhances the 404 error page on gwern.net by suggesting similar URLs
 * based on the current path that resulted in the 404 error. It fetches the sitemap,
 * compares the current path with all valid URLs, and presents the most similar ones
 * by text edit distance as suggestions to the user.
 *
 * Features:
 * - Fetches and parses the gwern.net sitemap.xml (<https://en.wikipedia.org/wiki/Sitemaps>)
 * - Uses a bounded Levenshtein distance algorithm for efficient URL comparison
 *   (Important as the Gwern.net sitemap.xml has >37k URLs and computing full edit distances
 *    is very slow.)
 * - Presents ≤10 unique, most similar URLs as suggestions
 * - Injects the suggestions into the current page before the "Other Options" section
 *
 * Example:
 * a reader goes to `/rubiiks-cube`, intending to go to `/rubiks-cube`; this will return
 * as suggestions: `/rubiks-cube, /subculture, /rubiks-cube-claude, /sunk-cost, …`
 * of which #1 & #3 are useful to the reader.
 *
 * Usage:
 * Include this script in the 404 error page. It will automatically run when the page loads.
 *
 * Structure of injected HTML:
 * ~~~{.HTML}
 * <section class="level1 block first-block">
 *   <h1 id="guessed-urls">Guessed URLs</h1>
 *   …
 *   <ul class="list-level-1">
 *     <li><a href="[suggested-url]"><code>[suggested-url]</code></a></li>
 *     ...
 *   </ul>
 * </section>
 * ~~~
 *
 * Performance: on Gwern.net, the sitemap.xml has >37k URLs & is >4.7MB.
 * This is a lot to load, parse, & search (although the bounded search takes maybe ~3s?).
 * However, I feel this is worthwhile because: most of the formatting/domain prefix is highly redundant
 * so it compresses well (to ~0.5MB) so it is not that expensive and comparable to an image or two;
 * few legitimate reader will ever hit a 404 during ordinary browsing & load it because we keep
 * all internal URLs up-to-date and have an extremely large manually-curated suite of nginx redirect rules
 * to catch every possible 404 error (I would estimate <10 legitimate 404s per day);
 * the many ill-behaved bots/attackers/scrapers redirecting to or loading the 404 page usually
 * won’t run the JS to download it (and if they do, downloading the sitemap.xml helps waste their bandwidth
 * and functions as a self-DoS); and a legitimate user should be willing to spend 0.5MB
 * and a second or three to get a useful list of URLs to try which will likely be many seconds faster than
 * any alternative way of fixing their 404, like manually running a search in Google (≫0.5MB).
 * One issue is that while sitemap.xml is always up to date server-side, as its cache is
 * specifically flushed on every sync, but a *client* may have cached a stale sitemap.xml.
 * This seems like an unimportant edge-case: precisely because legitimate 404s are so rare,
 * it is unlikely a client will have hit 404 #1, cached, then sometime later hit 404 #2,
 * only where #2 is a brandnew file not in the cached sitemap.xml.
 * Indeed, >95% of Gwern.net 404s are to ‘old’ URLs, created years or decades before.
 * (Which makes some sense: new URLs arrive slowly on Gwern.net, and tend to be harder to typo than
 * the old URLs were, and there is much more traffic to old URLs than new URLs.)
 *
 * Security Note: the sitemap.xml is trusted and assumed to contain clean valid XML with well-behaved
 * alphanumerical/punctuation URLs and maybe a bit of Unicode. We do not attempt to sanitize or
 * validate the XML or the parsed URLs, so if used in settings where attackers may control
 * the sitemap.xml (eg. user-generated content), the XML part should probably be
 * rewritten to be ultra-paranoid & do things like ping the final suggested URLs to
 * make sure they exist.
 *
 * Dependencies: Minimal (vanilla JS + custom utility functions in utility.js)
 *
 * Browser compatibility:
 * This script uses modern JS features like async/await and fetch.
 * It should work in all modern browsers, but may not function in older ones.
 */

const sitemapURL = location.origin + "/sitemap.xml";

/*********************/
/*	Fetch the sitemap.
 */
async function fetchSitemap() {
    try {
        const response = await fetch(sitemapURL);
        const text = await response.text();
        return text;
    } catch (error) {
        console.error("Error fetching sitemap:", error);
        return null;
    }
}

/**************************************/
/*	Parse the sitemap and extract URLs.
 */
function urlPathnamesFromSitemapString(sitemapText) {
    const parser = new DOMParser();
    const xmlDoc = parser.parseFromString(sitemapText, "text/xml");
    const locNodes = xmlDoc.getElementsByTagName("loc");
    //	sitemap XML looks like this:
    //	`<url><loc>https://gwern.net/doc/rotten.com/library/history/espionage/mossad/mossad_logo.gif</loc><changefreq>never</changefreq></url>`
    return Array.from(locNodes).map(locNode => new URL(locNode.textContent).pathname);
}

/*******************************************************/
/*	Calculate a bounded Levenshtein distance.
	<https://en.wikipedia.org/wiki/Levenshtein_distance>
 */
function boundedLevenshteinDistance(a, b, maxDistance) {
    if (Math.abs(a.length - b.length) > maxDistance)
    	return maxDistance + 1;

    const matrix = [];
    for (let i = 0; i <= b.length; i++) {
        matrix[i] = [i];
    }
    for (let j = 1; j <= a.length; j++) {
        matrix[0][j] = j;
    }

    for (let i = 1; i <= b.length; i++) {
        let minDistance = maxDistance + 1;
        for (let j = 1; j <= a.length; j++) {
            if (b.charAt(i - 1) === a.charAt(j - 1)) {
                matrix[i][j] = matrix[i - 1][j - 1];
            } else {
                matrix[i][j] = Math.min(
                    matrix[i - 1][j - 1] + 1,
                    matrix[i][j - 1] + 1,
                    matrix[i - 1][j] + 1
                );
            }
            minDistance = Math.min(minDistance, matrix[i][j]);
        }
        if (minDistance > maxDistance) {
            return maxDistance + 1;
        }
    }

    return matrix[b.length][a.length];
}

/******************************************/
/*	Find similar URLs (full-path distance).
 */
function findSimilarUrlPaths(urlPaths, targetPath, numResults, maxDistance) {
    return urlPaths.filter(
	    //	Quick filter based on length difference
    	urlPath => (Math.abs(urlPath.length - targetPath.length) <= maxDistance)
// 	).unique(
	//	NOTE: Aren’t the URLs in the sitemap unique already? —SA 2025-12-17
    ).map(
    	//	Calculate bounded Levenshtein distance.
    	urlPath => ({
            pathname: urlPath,
            distance: boundedLevenshteinDistance(urlPath, targetPath, maxDistance)
        })
    ).filter(
    	//	Filter by max distance.
    	item => item.distance <= maxDistance
    ).sort(
    	//	Sort by distance.
    	(a, b) => a.distance - b.distance
    ).slice(
    	0, numResults
    ).map(
    	item => item.pathname
    );
}

/******************************************************************************/
/*	Find URLs with similar “basenames” (a.k.a. last segments of the pathname).
 */
function findSimilarByBasename(urlPaths, targetPath, numResults, maxDistance) {
    const targetPathLastSegment = URLFromString(targetPath).pathSegments.last; 
    //	Skip if basename is too short or empty
    if (targetPathLastSegment.length < 3)
    	return [];
 
    return urlPaths.map(
    	//	Get last pathname segments.
    	urlPath => ({
    		pathname: urlPath,
    		lastSegment: URLFromString(urlPath).pathSegments.last
    	})
    ).filter(
    	//	Eliminate directory-only paths.
    	item => item.lastSegment.length > 1
    ).map(
    	//	Calculate bounded Levenshtein distance.
    	item => ({
            ...item,
            distance: boundedLevenshteinDistance(item.lastSegment, targetPathLastSegment, maxDistance)
        })
    ).filter(
    	//	Filter by max distance.
    	item => item.distance <= maxDistance
    ).sort(
    	//	Sort by distance.
		(a, b) => a.distance - b.distance
// 	).unique(
	//	NOTE: Aren’t the URLs in the sitemap unique already? —SA 2025-12-17
	).map(
		item => item.pathname
	);
}

/************************************/
/*	Inject suggestions into the page.
 */
function injectSuggestions(currentPath, suggestedUrlStrings) {
    let suggestionsHtml = suggestedUrlStrings.length > 0
    					  ? suggestedUrlStrings.map(urlString => `<li><p><a class="link-live" href="${urlString}"><code>${urlString}</code></a></p></li>`).join("")
    					  : "<li><p><strong>None found.</strong></p></li>";
	let wrapInColumnsIfMoreEntriesThan = 3;
	let listWrapper = [
		(suggestedUrlStrings.length > wrapInColumnsIfMoreEntriesThan ? `<div class="columns">\n` : ``),
		(suggestedUrlStrings.length > wrapInColumnsIfMoreEntriesThan ? `\n</div>` : ``),
	];
    let suggestionsElement = elementFromHTML(`<section class="level1">
        <h1 id="guessed-urls">Guessed URLs</h1>
        <p>Nearest URLs to your current one (<code>${(location.origin + currentPath)}</code>):</p>
        ${listWrapper[0]}<ul>${suggestionsHtml}</ul>${listWrapper[1]}
    </section>`);

	document.querySelector("#markdownBody").insertBefore(suggestionsElement, document.querySelector("#other-options"));

	Extracts.addTargetsWithin(suggestionsElement);
}

/*****************/
/*	Main function.
 */
async function suggest404Alternatives() {
    const currentPath = window.location.pathname;
    /*	If we redirected to a 404 rather than received it as an error, then the 
    	current URL is useless and can’t be guessed, so we skip the whole 
    	business, saving the bandwidth & injection:
     */
    if (currentPath.endsWith("/404")) {
        console.log("Current page is a 404 page; unable to guess intended URL. Skipping suggestions.");
        return;
    }

    const sitemapText = await fetchSitemap();
    if (sitemapText) {
    	const maxDistance = 8; // max distance chosen heuristically
    	const maxNumResults = 10;

        const urlPaths = urlPathnamesFromSitemapString(sitemapText);
		const similarUrlPaths = findSimilarByBasename(urlPaths, currentPath, maxNumResults, maxDistance * 0.5).concat(
									findSimilarUrlPaths(urlPaths, currentPath, maxNumResults, maxDistance)
								).unique(
								).slice(0, maxNumResults);

        injectSuggestions(currentPath, similarUrlPaths);
    }
}

/**************************************/
/*	Run the script when the page loads.
 */
window.addEventListener("load", suggest404Alternatives);
