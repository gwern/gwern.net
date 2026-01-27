/***********/
/* HELPERS */
/***********/

/***********************************************************/
/*  The first item of the array (or null if array is empty).
 */
Object.defineProperty(Array.prototype, "first", {
    get() {
        if (this.length == 0)
            return null;

        return this[0];
    }
});

/**********************************************************/
/*  The last item of the array (or null if array is empty).
 */
Object.defineProperty(Array.prototype, "last", {
    get() {
        if (this.length == 0)
            return null;

        return this[(this.length - 1)];
    }
});

/*******************************************************/
/*	Returns copy of the array, with null values removed.
 */
Array.prototype.nonnull = function () {
	return this.filter(value => value);
};

/*****************************************************************************/
/*	Returns a URL constructed from either a fully qualified URL string,
	or an absolute local URL (pathname starting at root), or a relative URL
	(pathname component replacing part of current URL after last slash), or
	a hash (URL fragment) only.

	(The existing URL() constructor only handles fully qualified URL strings.)

	The optional baseURL argument allows for qualifying non-fully-qualified
	URL strings relative to a base URL other than the current page location.
 */
function URLFromString(urlString, baseURL = location) {
	if (   urlString.startsWith("http://")
		|| urlString.startsWith("https://"))
		return new URL(urlString);

	if (urlString.startsWith("#"))
		return new URL(baseURL.origin + baseURL.pathname + urlString);

	return (urlString.startsWith("/")
			? new URL(baseURL.origin + urlString)
			: new URL(baseURL.href.replace(/[^\/]*$/, urlString)));
}

/****************************************************************************/
/*	Returns a modified URL constructed from the given URL or URL string, with
	the specified modifications in key-value form.
 */
function modifiedURL(url, mods) {
	let modURL = typeof url == "string"
				 ? URLFromString(url)
				 : URLFromString(url.href);
	for (let [ key, value ] of Object.entries(mods))
		modURL[key] = value;
	return modURL;
}

/***********************************************************************/
/*	Helper function for AJAX, by kronusaturn
		https://github.com/kronusaturn/lw2-viewer/blob/master/www/script.js
 */
function doAjax(options) {
	options = Object.assign({
		location: document.location,
		method: "GET",
		params: null,
		serialization: "URL",
		responseType: null,
		headers: null,
		onLoadStart: null,
		onProgress: null,
		onSuccess: null,
		onFailure: null,
		checkFor404Redirect: true,
		checkFor404RedirectURL: "/404"
	}, options);

	let req = new XMLHttpRequest();

	req.addEventListener("loadstart", (event) => {
		options.onLoadStart?.(event);
	});
	req.addEventListener("progress", (event) => {
		options.onProgress?.(event);
	});
	req.addEventListener("load", (event) => {
		if (event.target.status < 400) {
			if (options.checkFor404Redirect) {
				/*	This feature shouldn’t be necessary, but on some poorly
					configured servers that abuse HTTP status codes in ways
					contrary to the spec (and, indeed, to right thinking and
					moral propriety), it is. Alas, we live in a fallen world.
						—SA 2025-04-25
				 */
				let the404URLString = URLFromString(options.checkFor404RedirectURL).href;
				if (   the404URLString != options.location.href
					&& the404URLString == URLFromString(event.target.responseURL).href) {
					options.onFailure?.(event);
				} else {
					options.onSuccess?.(event);
				}
			} else {
				options.onSuccess?.(event);
			}
		} else {
			options.onFailure?.(event);
		}
	});
	req.addEventListener("error", (event) => {
		options.onFailure?.(event);
	});

	let url = options.location
			+ ((	 options.params != null
				&& options.method == "GET")
			   ? "?" + urlEncodeQuery(options.params)
			   : "");
	req.open(options.method, url);

	if (options.responseType)
		req.responseType = options.responseType;

	if (options.headers)
		for (let [ headerName, headerValue ] of Object.entries(options.headers))
			req.setRequestHeader(headerName, headerValue);

	if (options.method == "POST") {
		let payload;
		switch (options.serialization) {
		case "JSON":
			payload = JSON.stringify(options.params);
			req.setRequestHeader("Content-Type", "application/json");
			break;
		case "URL":
		default:
			payload = urlEncodeQuery(options.params);
			req.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
			break;
		}

		req.send(payload);
	} else {
		req.send();
	}
}

/*******************************************************************************/
/*  Create and return a new element with the specified tag name, attributes, and
    object properties.
 */
function newElement(tagName, attributes, properties) {
	attributes = Object.assign({ }, attributes);
	properties = Object.assign({ }, properties);

    let element = document.createElement(tagName);
    for (let [ attrName, attrValue ] of Object.entries(attributes))
		element.setAttribute(attrName, attrValue);
    for (let [ propName, propValue ] of Object.entries(properties))
		element[propName] = propValue;
    return element;
}

/*******************************************************************************/
/*  Create and return a DocumentFragment containing the given content.

    The content can be any of the following (yielding the listed return value):

    null
        an empty DocumentFragment

    a DocumentFragment
        a DocumentFragment containing the given DocumentFragment’s child nodes

    a string
        a DocumentFragment containing the HTML content that results from parsing
        the string

    a Node
        a DocumentFragment containing the node

    a NodeList
        a DocumentFragment containing the nodes
 */
function newDocument(content) {
    let docFrag = new DocumentFragment();

    if (content == null)
        return docFrag;

    if (content instanceof DocumentFragment) {
        content = content.childNodes;
    } else if (typeof content == "string") {
        let wrapper = newElement("DIV");
        wrapper.innerHTML = content;
        content = wrapper.childNodes;
    }

    if (content instanceof Node) {
        docFrag.append(document.importNode(content, true));
    } else if (   content instanceof NodeList
    		   || content instanceof Array) {
        docFrag.append(...(Array.from(content).map(node => document.importNode(node, true))));
    }

    return docFrag;
}

/************************************************************************/
/*	Creates element from HTML string. Returns null if given HTML does not
	define one, and only one, element.
 */
function elementFromHTML(elementHTML) {
	let doc = newDocument(elementHTML);
	if (doc.children.length != 1)
		return null;
	return doc.firstElementChild;
}

/***************************************************************/
/*	Round an integer up to the next multiple of a given divisor.
 */
function roundUpToMultiple(number, divisor) {
	return (Math.ceil(number / divisor) * divisor);
}

/************************************************************************/
/*	Return a tarball record byte size (with header) for a file, given the 
	file’s size in bytes.
 */
function tarballRecordSize(fileByteSize) {
	return (512 + roundUpToMultiple(fileByteSize, 512)); // tarball record header size, tarball record chunk size
}


/**********/
/* ACTION */
/**********/

/********************************************************/
/*	For both range-based and full-response-based loading.
 */

let resourceBaseName = assets["0"]["basename"];

function assetInfoFromResourceURLString(resourceURLString) {
	let resourceName = resourceURLString.match(/([^\/]+)$/)[1];
	let assetInfo = assets[resourceName];
	if (assetInfo == null)
		return null;

	if (assetInfo["name"] == null)
		assetInfo["name"] = resourceName;
	if (assetInfo["urlString"] == null)
		assetInfo["urlString"] = resourceURLString;

	return assetInfo;
}

//	Compute byte ranges.
let byteOffset = overhead + 512; // tarball record header size
for (let [ assetName, assetInfo ] of Object.entries(assets)) {
	assetInfo["byteRangeStart"] = byteOffset;

	let fileSize = parseInt(assets[assetName]["size"]);
	assetInfo["byteRangeEnd"] = byteOffset + fileSize - 1;
	byteOffset += tarballRecordSize(fileSize);
}

function activateScript(script, scriptContent = null) {
	let replacementScript = newElement("script");
	for (let attrName of script.getAttributeNames())
		replacementScript.setAttribute(attrName, script.getAttribute(attrName));

	//	Inline all scripts.
	if (scriptContent != null) {
		replacementScript.appendChild(document.createTextNode(scriptContent));
		replacementScript.removeAttribute("src");
	} else if (script.src == "") {
		replacementScript.appendChild(document.createTextNode(script.textContent));
	}

	script.replaceWith(replacementScript);
}

function replaceDocumentWithResponse(responseText) {
	document.documentElement.innerHTML = responseText.match(/<html .+?>(.+)(<\/html>|$)/is)[1].replace(
		//	Prevent spurious network requests.
		new RegExp(`${resourceBaseName}/${resourceBaseName}-asset-[0-9]+\.[0-9a-zA-Z]+`, "g"),
		(match) => { return modifiedURL(URLFromString(match), { hostname: "localhost" }).href; }
	);
	//	Activate scripts.
	document.querySelectorAll("script").forEach(script => {
		activateScript(script);
	});
}

function spawnRequestObserver(resourceURLStringsHandler) {
	let perfObserver = new PerformanceObserver((entryList, observer) => {
		resourceURLStringsHandler(entryList.getEntries().map(entry => entry.name));
	});
	perfObserver.observe({ entryTypes: [ "resource" ] });
}

function replaceResourceInDocument(assetInfo) {
	replaceResourceInElement(document.documentElement, assetInfo);
}

/*	The ‘data’ member of the ‘assetInfo’ argument can be anything that the 
	Blob() constructor takes an array of (ArrayBuffer, TypedArray, etc.).
 */
function replaceResourceInElement(element, assetInfo, resourceURLStringRegExp) {
	if (resourceURLStringRegExp == undefined)
		resourceURLStringRegExp = new RegExp(assetInfo["urlString"]);

	if (element.children.length > 0) {
		for (let childElement of element.children)
			replaceResourceInElement(childElement, assetInfo, resourceURLStringRegExp);
	} else if (   element.parentElement != null
			   && resourceURLStringRegExp.test(element.outerHTML)) {
		if (   element.tagName.toLowerCase() == "script"
			&& assetInfo["name"].endsWith(".js")) {
			activateScript(element, (new TextDecoder()).decode(assetInfo["data"]));
		} else {
			let blob = new Blob([ assetInfo["data"] ], { type: assetInfo["content-type"] });
			element.outerHTML = element.outerHTML.replace(
				resourceURLStringRegExp,
				URL.createObjectURL(blob)
			);
		}
	}
}

function handlePageRequestFailure() {
	let noscript = document.querySelector("noscript");
	noscript.outerHTML = noscript.innerHTML;
	let warningsSelector = [
		".js-disabled-warning",
		".local-file-warning",
		".server-fail-warning"
	].join(", ");
	document.querySelectorAll(warningsSelector).forEach(warning => {
		warning.style.display = "none";
	});
	if (location.protocol == "file:") {
		document.querySelector(".local-file-warning").style.display = "";
		document.querySelectorAll(".gwtar-file-base-name").forEach(gwtarFileBaseName => {
			gwtarFileBaseName.textContent = location.pathname.split("/").last.match(/^(.+)\.gwtar\.html$/)[1];
		});
		document.querySelectorAll(".html-file-base-name").forEach(htmlFileBaseName => {
			htmlFileBaseName.textContent = assets["0"]["basename"];
		});
	} else {
		document.querySelector(".server-fail-warning").style.display = "";
	}
}

/********************************/
/*	For range-based loading only.
 */

function getResources(assetInfoRecords) {
	let fullByteRange = assetInfoRecords.map(assetInfo => 
		`${assetInfo["byteRangeStart"]}-${assetInfo["byteRangeEnd"]}`
	).join(",");
	if (fullByteRange == "")
		return;

	doAjax({
		headers: {
			"Range": `bytes=${fullByteRange}`
		},
		responseType: "arraybuffer",
		onSuccess: (event) => {
			let assetInfo = assetInfoRecords.first;
			assetInfo["data"] = event.target.response;
			replaceResourceInDocument(assetInfo);
		}
	});
}

/*	Attempt to get just the HTML of the main page, via a Range request.
 */
function getMainPageHTML() {
	doAjax({
		headers: {
			"Range": `bytes=${assets["0"]["byteRangeStart"]}-${assets["0"]["byteRangeEnd"]}`
		},
		onProgress: (event) => {
			/*	If the response status code is 206, then everything is fine and
				we’re getting our requested byte ranges.

				If the status code is 200, then likely the server has ignored 
				the HTTP Range header, and is sending the whole file. That means
				that we need to handle things differently.

				If the status code is anything else, then some problem is 
				happening, probably. But we don’t need to handle that here.
			 */
			if (event.target.status == 200) {
				event.target.abort();
				getFullPageData();
			}
		},
		onSuccess: (event) => {
			assets["0"]["data"] = event.target.responseText;
			replaceDocumentWithResponse(event.target.responseText);
			spawnRequestObserver((resourceURLStrings) => {
				let assetInfoRecords = resourceURLStrings.map(assetInfoFromResourceURLString).nonnull();

				//	Inject those resources we’ve already retrieved.
				assetInfoRecords.filter(assetInfo => assetInfo["data"] != null).forEach(assetInfo => {
					replaceResourceInDocument(assetInfo);
				});

				//	Retrieve those resources we still need.
		// 		getResources(assetInfoRecords.filter(assetInfo => assetInfo["data"] == null));
				assetInfoRecords.filter(assetInfo => assetInfo["data"] == null).forEach(assetInfo => {
					getResources([ assetInfo ]);
				});
			});
		},
		onFailure: handlePageRequestFailure
	});
}

/****************************************/
/*	For full-response-based loading only.
 */

let loadedResponseData = new Uint8Array(totalArchiveSize);
let loadedResponseDataLength = 0;

function getFullPageData() {
	fetch(location).then(response => {
		let reader = response.body.getReader();
		let callback = ({ done, value }) => {
			if (assets["0"]["status"] == null)
				assets["0"]["status"] = "waiting";

			if (done == false) {
				appendLoadedResponseData(value);

				reader.read().then(callback);
			}

			loadAllWaitingAssets();
		}
		reader.read().then(callback);
	}).catch(error => {
		handlePageRequestFailure();
	});
}

function appendLoadedResponseData(value) {
	loadedResponseData.set(value, loadedResponseDataLength);
	loadedResponseDataLength += value.length;
}

function loadAllWaitingAssets() {
	for (let [ assetName, assetInfo ] of Object.entries(assets)) {
		if (assetInfo["status"] != "waiting")
			continue;

		if (assetInfo["byteRangeEnd"] >= loadedResponseDataLength)
			continue;

		let bytes = loadedResponseData.slice(assetInfo["byteRangeStart"], assetInfo["byteRangeEnd"] + 1);
		if (assetName == "0") {
			let responseText = (new TextDecoder()).decode(bytes);
			assetInfo["data"] = responseText;
			replaceDocumentWithResponse(responseText);
			spawnRequestObserver((resourceURLStrings) => {
				let assetInfoRecords = resourceURLStrings.map(assetInfoFromResourceURLString).nonnull();
				assetInfoRecords.forEach(assetInfo => {
					markResourceWaiting(assetInfo);
				});

				loadAllWaitingAssets();
			});
		} else {
			replaceResourceInDocument(assetInfo);
		}

		assetInfo["status"] = "loaded";
	}
}

function markAssetWaiting(assetInfo) {
	if (assetInfo == null)
		return;

	assetInfo["status"] = "waiting";
}

/***************************************************/
/*	Begin loading (after the prefix document loads).
 */
requestIdleCallback(getMainPageHTML);
