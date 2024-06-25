/*
 * Title: 404 Error Page URL Suggester
 * Author: Gwern Branwen
 * Date: 2024-06-25
 * When:  Time-stamp: "2024-06-25 18:57:24 gwern"
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
 * as suggestions: `/rubiks-cube, /subculture, /rubiks-cube-claude, /sunk-cost, ...`
 * of which #1 & #3 are useful to the reader.
 *
 * Usage:
 * Include this script in the 404 error page. It will automatically run when the page loads.
 *
 * Structure of injected HTML:
 * ~~~{.HTML}
 * <section class="level1 block first-block">
 *   <h1 id="guessed-urls">Guessed URLs</h1>
 *   <ul class="list-level-1">
 *     <li><a href="[suggested-url]"><code>[suggested-url]</code></a></li>
 *     ...
 *   </ul>
 * </section>
 * ~~~
 *
 * Dependencies: None (vanilla JS)
 *
 * Browser compatibility:
 * This script uses modern JS features like async/await and fetch.
 * It should work in all modern browsers, but may not function in older ones.
 */

const baseDomain = 'https://gwern.net';
const sitemapURL = baseDomain + '/sitemap.xml';

// Function to fetch the sitemap
async function fetchSitemap() {
    try {
        const response = await fetch(sitemapURL);
        const text = await response.text();
        return text;
    } catch (error) {
        console.error('Error fetching sitemap:', error);
        return null;
    }
}

// Function to parse the sitemap and extract URLs
function parseUrls(sitemapText) {
    const parser = new DOMParser();
    const xmlDoc = parser.parseFromString(sitemapText, "text/xml");
    const urlNodes = xmlDoc.getElementsByTagName("url");
    // sitemap XML looks like this:
    // `<url><loc>https://gwern.net/doc/rotten.com/library/history/espionage/mossad/mossad_logo.gif</loc><changefreq>never</changefreq></url>`
    return Array.from(urlNodes).map(node => new URL(node.getElementsByTagName("loc")[0].textContent).pathname);
}

// Function to calculate a bounded Levenshtein distance <https://en.wikipedia.org/wiki/Levenshtein_distance>
function boundedLevenshteinDistance(a, b, maxDistance) {
    if (Math.abs(a.length - b.length) > maxDistance) return maxDistance + 1;

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

// Function to find similar URLs
function findSimilarUrls(urls, targetUrl, n = 10,
                         maxDistance = 11) { // max distance chosen heuristically
    const targetPath = new URL(targetUrl, baseDomain).pathname;

    // Quick filter based on length difference
    const potentialMatches = urls.filter(url =>
        Math.abs(url.length - targetPath.length) <= maxDistance
    );

    const similarUrls = potentialMatches
        .map(url => ({
            url,
            distance: boundedLevenshteinDistance(url, targetPath, maxDistance)
        }))
        .filter(item => item.distance <= maxDistance)
        .sort((a, b) => a.distance - b.distance);

    // De-duplicate while preserving order
    const seenUrls = new Set();
    const uniqueSimilarUrls = similarUrls.filter(item => {
        if (seenUrls.has(item.url)) return false;
        seenUrls.add(item.url);
        return true;
    }).slice(0, n);

    return uniqueSimilarUrls.map(item => baseDomain + item.url);
}

// Function to inject suggestions into the page
function injectSuggestions(suggestions) {
    const suggestionsHtml = suggestions.map(url => `<li><a class="link-live" href="${url}"><code>${url}</code></a></li>`).join('');
    const suggestionsElement = document.createElement('section');
    suggestionsElement.className = 'level1 block first-block';
    suggestionsElement.innerHTML = `
        <h1 id="guessed-urls">Guessed URLs</h1>
        <ul class="list-level-1">${suggestionsHtml}</ul>
    `;

    const otherOptionsElement = document.getElementById('other-options');
    if (otherOptionsElement) {
        otherOptionsElement.parentNode.insertBefore(suggestionsElement, otherOptionsElement);
    } else {
        console.warn("Couldn't find the 'Other Options' section. Appending to the end of the body.");
        document.body.appendChild(suggestionsElement);
    }
}

// Main function
async function suggest404Alternatives() {
    const currentPath = window.location.pathname;
    const sitemapText = await fetchSitemap();

    if (sitemapText) {
        const urls = parseUrls(sitemapText);
        const similarUrls = findSimilarUrls(urls, currentPath);
        injectSuggestions(similarUrls);
    }
}

// Run the script when the page loads
window.addEventListener('load', suggest404Alternatives);
