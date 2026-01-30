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

//	Compute byte ranges and save names.
let byteOffset = overhead + 512; // tarball record header size
for (let [ assetName, assetInfo ] of Object.entries(assets)) {
	assetInfo["name"] = assetName;

	assetInfo["byteRangeStart"] = byteOffset;

	let fileSize = parseInt(assets[assetName]["size"]);
	assetInfo["byteRangeEnd"] = byteOffset + fileSize - 1;

	byteOffset += tarballRecordSize(fileSize);
}

function assetInfoFromResourceURLString(resourceURLString) {
	let resourceName = resourceURLString.match(/([^\/]+)$/)[1];
	let assetInfo = assets[resourceName];
	if (assetInfo == null)
		return null;

	if (assetInfo["urlString"] == null)
		assetInfo["urlString"] = resourceURLString;

	return assetInfo;
}

function activateScript(script) {
	let replacementScript = newElement("script");
	for (let attrName of script.getAttributeNames())
		replacementScript.setAttribute(attrName, script.getAttribute(attrName));

	//	Inline all scripts.
	if (script.src > "") {
		let scriptAssetInfo = assetInfoFromResourceURLString(script.src);
		if (scriptAssetInfo["data"] == null)
			return;

		let scriptContent = (new TextDecoder()).decode(scriptAssetInfo["data"])
		replacementScript.appendChild(document.createTextNode(scriptContent));

		replacementScript.removeAttribute("src");
	} else if (script.src == "") {
		replacementScript.appendChild(document.createTextNode(script.textContent));
	}

	script.replaceWith(replacementScript);
}

function rewriteHTMLResponse(responseText) {
	return responseText.match(/<html .+?>(.+)(<\/html>|$)/is)[1].replace(
		//	Prevent spurious network requests.
		new RegExp(`${resourceBaseName}/${resourceBaseName}-asset-[0-9]+\.[0-9a-zA-Z]+`, "g"),
		(match) => { return modifiedURL(URLFromString(match), { hostname: "localhost" }).href; }
	);
}

function renderMainPage() {
	if (assets["0"]["data"] == null)
		return;

	let html = rewriteHTMLResponse((new TextDecoder()).decode(assets["0"]["data"]));
	//	Contents of <head> and <body>, respectively.
	let parts = html.split(/<body.*?>/is).map(part => newDocument(part));
	parts.forEach(part => {
		part.querySelectorAll("script").forEach(script => {
			activateScript(script);
		});
	});
	document.head.replaceChildren(parts[0]);
	document.body.replaceChildren(parts[1]);

	assets["0"]["rendered"] = true;
}

function allScripts() {
	return Object.values(assets).filter(assetInfo => assetInfo["name"].endsWith(".js"));
}

//	Returns true if all .js assets have been loaded, false otherwise.
function allScriptsLoaded() {
	return (Object.values(assets).some(assetInfo => 
		(   assetInfo["name"].endsWith(".js")
		 && assetInfo["data"] == null)
	) == false);
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
	if (assetInfo["data"] == null)
		return;

	if (resourceURLStringRegExp == undefined)
		resourceURLStringRegExp = new RegExp(assetInfo["urlString"]);

	if (element.children.length > 0) {
		for (let childElement of element.children)
			replaceResourceInElement(childElement, assetInfo, resourceURLStringRegExp);
	} else if (   element.parentElement != null
			   && resourceURLStringRegExp.test(element.outerHTML)) {
		//	Save our place...
		let parentElement = element.parentElement;
		let previousElementSibling = element.previousElementSibling;

		//	Wrap data in a Blob and inject the blob URL.
		let blob = new Blob([ assetInfo["data"] ], { type: assetInfo["content-type"] });
		element.outerHTML = element.outerHTML.replace(
			resourceURLStringRegExp,
			URL.createObjectURL(blob)
		);

		//	Previous reference is invalid now, so we get the new element.
		element = previousElementSibling?.nextElementSibling ?? parentElement.firstElementChild;

		//	Special handling for some element types.
		if (element.tagName.toLowerCase() == "source") {
			element.setAttribute("type", assetInfo["content-type"]);

			/*	This weird hack is necessary because audio elements are, for
				some reason, weird about Blob URLs. Somehow, this fixes it.
			 */
			let audio = element.closest("audio");
			if (audio)
				audio.replaceWith(audio.cloneNode(true));
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

function parseMultipartBody(body, boundary) {
	let parts = [ ];

    let bytes = new Uint8Array(body);

	const MultipartParsingSection = Object.freeze({
		START:		Symbol("start"),
		HEADERS:	Symbol("headers"),
		BODY:		Symbol("body")
	});

	let decoder = new TextDecoder();

	let section = MultipartParsingSection.START;
	let boundaryString = `--${boundary}`;
	let i = 0;
	while (i < (bytes.length - boundaryString.length)) {
		if (   section == MultipartParsingSection.START
			|| section == MultipartParsingSection.BODY) {
			for (var offset = 0; offset < boundaryString.length; offset++) {
				let c = String.fromCharCode(bytes[i + offset]);
				if (c != boundaryString[offset])
					break;
			}
			if (offset == boundaryString.length) {
				i += offset;
				if (   String.fromCharCode(bytes[i]) == "-"
					&& String.fromCharCode(bytes[i + 1]) == "-")
					break;

				section = MultipartParsingSection.HEADERS;
				parts.push({ body: [ ], headers: { } });
			} else {
				if (section == MultipartParsingSection.BODY)
					parts.last.body.push(bytes[i]);

				i++;
			}
		} else {
			let headerBytes = [ ];
			while (true) {
				let c = String.fromCharCode(bytes[i]);				
				if (   c == "\r"
					&& decoder.decode(bytes.slice(i, i + 4)) == "\r\n\r\n") {
					i += 4;
					break;
				} else {
					headerBytes.push(bytes[i]);
					i++;
				}
			}
			decoder.decode(Uint8Array.from(headerBytes)).trim().split("\r\n").forEach(header => {
				let [ headerName, headerValue ] = header.split(/:\s+/);
				parts.last.headers[headerName.toLowerCase()] = headerValue;
			});
			section = MultipartParsingSection.BODY;
		}
	}

	parts.forEach(part => {
		part.body = Uint8Array.from(part.body);
	});

	return parts;
}

/*	Callbacks are called once per asset received, NOT once per HTTP response.
 */
function getResources(assetInfoRecords, callbacks) {
	callbacks = Object.assign({ }, callbacks);

	let fullByteRange = assetInfoRecords.sort((a, b) => 
		(a["byteRangeStart"] - b["byteRangeStart"])
	).map(assetInfo => 
		`${assetInfo["byteRangeStart"]}-${assetInfo["byteRangeEnd"]}`
	).join(",");
	if (fullByteRange == "")
		return;

	doAjax({
		headers: {
			"Range": `bytes=${fullByteRange}`
		},
		responseType: "arraybuffer",
		onProgress: (event) => {
			/*	If the response status code is 206, then everything is fine and
				we’re getting our requested byte ranges.

				If the status code is 200, then likely the server has ignored 
				the HTTP Range header, and is sending the whole file. That means
				that we need to handle things differently.

				If the status code is anything else, then some problem is 
				happening, probably. But we don’t need to handle that here.
			 */
			if (event.target.status == 200)
				event.target.abort();

			if (callbacks.onProgress != null)
				callbacks.onProgress(event);
		},
		onSuccess: (event) => {
			let contentTypeParts = event.target.getResponseHeader("content-type").split(";").map(part => part.trim());
			if (contentTypeParts[0] == "multipart/byteranges") {
				let boundaryString = contentTypeParts[1].split("=")[1];
				let parts = parseMultipartBody(event.target.response, boundaryString);
				assetInfoRecords.forEach(assetInfo => {
					let resourceByteRange = `${assetInfo["byteRangeStart"]}-${assetInfo["byteRangeEnd"]}`;
					assetInfo["data"] = parts.find(part => 
						part["headers"]["content-range"].split("/")[0] == `bytes ${resourceByteRange}`
					)["body"];
					if (callbacks.onSuccess != null)
						callbacks.onSuccess(event, assetInfo);
				});
			} else {
				let assetInfo = assetInfoRecords.first;
				assetInfo["data"] = event.target.response;
				if (callbacks.onSuccess != null)
					callbacks.onSuccess(event, assetInfo);
			}
		},
		onFailure: (event) => {
			if (callbacks.onFailure != null)
				callbacks.onFailure(event);
		}
	});
}

/*	Attempt to get just the HTML of the main page, via a Range request.
 */
function getMainPageHTML() {
	getResources([ assets["0"] ], {
		onProgress: (event) => {
			/*	A 200 status code means that we’ve aborted the transfer (because
				the server doesn’t support range requests, it seems), but we
				still need to load the page. So we switch to full-response-based
				mode.
			 */
			if (event.target.status == 200)
				getFullPageData();
		},
		onSuccess: (event, assetInfo) => {
			loadAllScriptsAndThenDo(() => {
				spawnRequestObserver((resourceURLStrings) => {
					let assetInfoRecords = resourceURLStrings.map(assetInfoFromResourceURLString).nonnull();
					if (assetInfoRecords.length == 0)
						return;

					//	Inject those resources we’ve already retrieved.
					assetInfoRecords.filter(assetInfo => assetInfo["data"] != null).forEach(assetInfo => {
						replaceResourceInDocument(assetInfo);
					});

					//	Retrieve those resources we still need.
					let assetInfoRecordsRemaining = assetInfoRecords.filter(
						(assetInfo) => (assetInfo["data"] == null)
					).sort(
						(a, b) => (a["size"] - b["size"])
					);
					let batch = [ ];
					let record = null;
					let assumedBDP = (1 << 20); // 1 MB
					for (let i = 0; i <= assetInfoRecordsRemaining.length; i++) {
						let record = (i == assetInfoRecordsRemaining.length) ? null : assetInfoRecordsRemaining[i];
						let totalBatchSize = batch.reduce((total, recordInBatch) => (total + recordInBatch["size"]), 0);
						if (   record == null
							|| totalBatchSize + record["size"] >= assumedBDP) {
							getResources(batch, {
								onSuccess: (event, assetInfo) => {
									replaceResourceInDocument(assetInfo);
								}
							});
							batch = [ ];
						}
						batch.push(record);
					}
				});

				renderMainPage();
			});
		},
		onFailure: (event) => {
			handlePageRequestFailure();
		}
	});
}

function loadAllScriptsAndThenDo(callback) {
	if (allScriptsLoaded() == true) {
		callback();
		return;
	}

	getResources(allScripts().filter(assetInfo => (assetInfo["data"] == null)), {
		onSuccess: (event, assetInfo) => {
			if (allScriptsLoaded() == true)
				callback();
		}
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

		assetInfo["data"] = loadedResponseData.slice(assetInfo["byteRangeStart"], assetInfo["byteRangeEnd"] + 1);
		assetInfo["status"] = "loaded";

		if (assetName == "0") {
			allScripts().filter(assetInfo => (assetInfo["status"] != "loaded")).forEach(markAssetWaiting);
		} else if (assets["0"]["rendered"] == true) {
			replaceResourceInDocument(assetInfo);
		}

		if (   assets["0"]["rendered"] != true
			&& allScriptsLoaded() == true) {
			spawnRequestObserver((resourceURLStrings) => {
				let assetInfoRecords = resourceURLStrings.map(assetInfoFromResourceURLString).nonnull();
				assetInfoRecords.forEach(assetInfo => {
					markAssetWaiting(assetInfo);
				});

				loadAllWaitingAssets();
			});

			renderMainPage();
		}
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
