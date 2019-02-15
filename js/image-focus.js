window.GW = { };
GW.temp = { };

GW.isMobile = ('ontouchstart' in document.documentElement);

/********************/
/* DEBUGGING OUTPUT */
/********************/

function GWLog (string) {
	if (GW.loggingEnabled || localStorage.getItem("logging-enabled") == "true")
		console.log(string);
}
GW.enableLogging = (permanently = false) => {
	if (permanently)
		localStorage.setItem("logging-enabled", "true");
	else
		GW.loggingEnabled = true;
};
GW.disableLogging = (permanently = false) => {
	if (permanently)
		localStorage.removeItem("logging-enabled");
	else
		GW.loggingEnabled = false;
};

/********************/
/* QUERYING THE DOM */
/********************/

function queryAll(selector, context) {
    context = context || document;
    // Redirect simple selectors to the more performant function
    if (/^(#?[\w-]+|\.[\w-.]+)$/.test(selector)) {
        switch (selector.charAt(0)) {
            case '#':
                // Handle ID-based selectors
                let element = document.getElementById(selector.substr(1));
                return element ? [ element ] : [ ];
            case '.':
                // Handle class-based selectors
                // Query by multiple classes by converting the selector 
                // string into single spaced class names
                var classes = selector.substr(1).replace(/\./g, ' ');
                return [].slice.call(context.getElementsByClassName(classes));
            default:
                // Handle tag-based selectors
                return [].slice.call(context.getElementsByTagName(selector));
        }
    }
    // Default to `querySelectorAll`
    return [].slice.call(context.querySelectorAll(selector));
}
function query(selector, context) {
	let all = queryAll(selector, context);
	return (all.length > 0) ? all[0] : null;
}
Object.prototype.queryAll = function (selector) {
	return queryAll(selector, this);
}
Object.prototype.query = function (selector) {
	return query(selector, this);
}

/****************/
/* MISC HELPERS */
/****************/

/*	Given an HTML string, creates an element from that HTML, adds it to 
	#ui-elements-container (creating the latter if it does not exist), and 
	returns the created element.
	*/
function addUIElement(element_html) {
	var ui_elements_container = query("#ui-elements-container");
	if (!ui_elements_container) {
		ui_elements_container = document.createElement("div");
		ui_elements_container.id = "ui-elements-container";
		query("body").appendChild(ui_elements_container);
	}

	ui_elements_container.insertAdjacentHTML("beforeend", element_html);
	return ui_elements_container.lastElementChild;
}

/*	Toggles whether the page is scrollable.
	*/
function togglePageScrolling(enable) {
	let html = query("html");
	if (!enable) {
		GW.scrollPositionBeforeScrollingDisabled = window.scrollY;
		html.classList.toggle("no-scroll", true);
		html.style.top = `-${GW.scrollPositionBeforeScrollingDisabled}px`;
	} else {
		html.classList.toggle("no-scroll", false);
		html.removeAttribute("style");
		window.scrollTo(0, GW.scrollPositionBeforeScrollingDisabled);
	}
}

/*	Returns true if the array contains the given element.
	*/
Array.prototype.contains = function (element) {
	return (this.indexOf(element) !== -1);
}

/***************/
/* IMAGE FOCUS */
/***************/

function imageFocusSetup() {
	if (typeof GW.imageFocus == "undefined")
		GW.imageFocus = {
			contentImagesSelector:	"#markdownBody img",
			focusedImageSelector:	"#markdownBody img.focused",
			pageContentSelector:	"main",
			shrinkRatio:			0.975,
			hideUITimerDuration:	1500,
			hideUITimerExpired:		() => {
				GWLog("GW.imageFocus.hideUITimerExpired");
				let currentTime = new Date();
				let timeSinceLastMouseMove = (new Date()) - GW.imageFocus.mouseLastMovedAt;
				if (timeSinceLastMouseMove < GW.imageFocus.hideUITimerDuration) {
					GW.imageFocus.hideUITimer = setTimeout(GW.imageFocus.hideUITimerExpired, (GW.imageFocus.hideUITimerDuration - timeSinceLastMouseMove));
				} else {
					hideImageFocusUI();
					cancelImageFocusHideUITimer();
				}
			}
		};

	GWLog("imageFocusSetup");
	// Create event listener for clicking on images to focus them.
	GW.imageClickedToFocus = (event) => {
		GWLog("GW.imageClickedToFocus");
		focusImage(event.target);

		if (!GW.isMobile) {
			// Set timer to hide the image focus UI.
			unhideImageFocusUI();
			GW.imageFocus.hideUITimer = setTimeout(GW.imageFocus.hideUITimerExpired, GW.imageFocus.hideUITimerDuration);
		}
	};
	// Add the listener to all content images.
	queryAll(GW.imageFocus.contentImagesSelector).forEach(image => {
		image.addEventListener("click", GW.imageClickedToFocus);
	});

	// Wrap all images in figures in a span.
	queryAll("figure").forEach(figure => {
		let image = figure.query("img");
		if (!image) return;
		let wrapper = document.createElement("span");
		wrapper.classList.add("image-wrapper");
		wrapper.appendChild(image);
		figure.insertBefore(wrapper, figure.firstChild);
	});

	// Create the image focus overlay.
	let imageFocusOverlay = addUIElement("<div id='image-focus-overlay'>" + 
	`<div class='help-overlay'>
		 <p><strong>Escape</strong> or <strong>click</strong>: Hide zoomed image</p>
		 <p><strong>Space bar:</strong> Reset image size & position</p>
		 <p><strong>Scroll</strong> to zoom in/out</p>
		 <p>(When zoomed in, <strong>drag</strong> to pan; <br/><strong>double-click</strong> to close)</p>
	</div>
	<div class='caption'></div>` + 
	"</div>");
	imageFocusOverlay.dropShadowFilterForImages = " drop-shadow(10px 10px 10px #000) drop-shadow(0 0 10px #444)";

	// On orientation change, reset the size & position.
	if (typeof(window.msMatchMedia || window.MozMatchMedia || window.WebkitMatchMedia || window.matchMedia) !== 'undefined') {
		window.matchMedia('(orientation: portrait)').addListener(() => { setTimeout(resetFocusedImagePosition, 0); });
	}

	// UI starts out hidden.
	hideImageFocusUI();
}

function focusImage(imageToFocus) {
	GWLog("focusImage");

	// Create the focused version of the image.
	imageToFocus.classList.toggle("focused", true);
	let imageFocusOverlay = query("#image-focus-overlay");
	let clonedImage = imageToFocus.cloneNode(true);
	clonedImage.style = "";
	clonedImage.removeAttribute("width");
	clonedImage.removeAttribute("height");
	clonedImage.style.filter = imageToFocus.style.filter + imageFocusOverlay.dropShadowFilterForImages;

	// Add the image to the overlay.
	imageFocusOverlay.appendChild(clonedImage);
	imageFocusOverlay.classList.toggle("engaged", true);

	// Set image to default size and position.
	resetFocusedImagePosition();

	// Blur everything else.
	queryAll(GW.imageFocus.pageContentSelector).forEach(element => {
		element.classList.toggle("blurred", true);
	});

	// Add listener to zoom image with scroll wheel.
	window.addEventListener("wheel", GW.imageFocus.scrollEvent = (event) => {
		GWLog("GW.imageFocus.scrollEvent");
		event.preventDefault();

		let image = query("#image-focus-overlay img");

		// Remove the filter.
		image.savedFilter = image.style.filter;
		image.style.filter = 'none';

		// Locate point under cursor.
		let imageBoundingBox = image.getBoundingClientRect();

		// Calculate resize factor.
		var factor = (image.height > 10 && image.width > 10) || event.deltaY < 0 ?
						1 + Math.sqrt(Math.abs(event.deltaY))/100.0 :
						1;

		// Resize.
		image.style.width = (event.deltaY < 0 ?
							(image.clientWidth * factor) :
							(image.clientWidth / factor))
							+ "px";
		image.style.height = "";

		// Designate zoom origin.
		var zoomOrigin;
		// Zoom from cursor if we're zoomed in to where image exceeds screen, AND
		// the cursor is over the image.
		let imageSizeExceedsWindowBounds = (image.getBoundingClientRect().width > window.innerWidth || image.getBoundingClientRect().height > window.innerHeight);
		let zoomingFromCursor = imageSizeExceedsWindowBounds &&
								(imageBoundingBox.left <= event.clientX &&
								 event.clientX <= imageBoundingBox.right && 
								 imageBoundingBox.top <= event.clientY &&
								 event.clientY <= imageBoundingBox.bottom);
		// Otherwise, if we're zooming OUT, zoom from window center; if we're 
		// zooming IN, zoom from image center.
		let zoomingFromWindowCenter = event.deltaY > 0;
		if (zoomingFromCursor)
			zoomOrigin = { x: event.clientX, 
						   y: event.clientY };
		else if (zoomingFromWindowCenter)
			zoomOrigin = { x: window.innerWidth / 2, 
						   y: window.innerHeight / 2 };
		else
			zoomOrigin = { x: imageBoundingBox.x + imageBoundingBox.width / 2, 
						   y: imageBoundingBox.y + imageBoundingBox.height / 2 };

		// Calculate offset from zoom origin.
		let offsetOfImageFromZoomOrigin = {
			x: imageBoundingBox.x - zoomOrigin.x,
			y: imageBoundingBox.y - zoomOrigin.y
		}
		// Calculate delta from centered zoom.
		let deltaFromCenteredZoom = {
			x: image.getBoundingClientRect().x - (zoomOrigin.x + (event.deltaY < 0 ? offsetOfImageFromZoomOrigin.x * factor : offsetOfImageFromZoomOrigin.x / factor)),
			y: image.getBoundingClientRect().y - (zoomOrigin.y + (event.deltaY < 0 ? offsetOfImageFromZoomOrigin.y * factor : offsetOfImageFromZoomOrigin.y / factor))
		}
		// Adjust image position appropriately.
		image.style.left = parseInt(getComputedStyle(image).left) - deltaFromCenteredZoom.x + "px";
		image.style.top = parseInt(getComputedStyle(image).top) - deltaFromCenteredZoom.y + "px";
		// Gradually re-center image, if it's smaller than the window.
		if (!imageSizeExceedsWindowBounds) {
			let imageCenter = { x: image.getBoundingClientRect().x + image.getBoundingClientRect().width / 2, 
								y: image.getBoundingClientRect().y + image.getBoundingClientRect().height / 2 }
			let windowCenter = { x: window.innerWidth / 2,
								 y: window.innerHeight / 2 }
			let imageOffsetFromCenter = { x: windowCenter.x - imageCenter.x,
										  y: windowCenter.y - imageCenter.y }
			// Divide the offset by 10 because we're nudging the image toward center,
			// not jumping it there.
			image.style.left = parseInt(getComputedStyle(image).left) + imageOffsetFromCenter.x / 10 + "px";
			image.style.top = parseInt(getComputedStyle(image).top) + imageOffsetFromCenter.y / 10 + "px";
		}

		// Put the filter back.
		image.style.filter = image.savedFilter;

		// Set the cursor appropriately.
		setFocusedImageCursor();
	});
	window.addEventListener("MozMousePixelScroll", GW.imageFocus.oldFirefoxCompatibilityScrollEvent = (event) => {
		GWLog("GW.imageFocus.oldFirefoxCompatibilityScrollEvent");
		event.preventDefault();
	});

	// If image is bigger than viewport, it's draggable. Otherwise, click unfocuses.
	window.addEventListener("mouseup", GW.imageFocus.mouseUp = (event) => {
		GWLog("GW.imageFocus.mouseUp");
		window.onmousemove = '';

		// We only want to do anything on left-clicks.
		if (event.button != 0) return;

		// We also don't want to do anything if clicked on the help overlay.
		if (event.target.classList.contains("help-overlay") ||
			event.target.closest(".help-overlay"))
			return;

		let focusedImage = query("#image-focus-overlay img");
		if (event.target == focusedImage && 
			(focusedImage.height >= window.innerHeight || focusedImage.width >= window.innerWidth)) {
			// If the mouseup event was the end of a pan of an overside image,
			// put the filter back; do not unfocus.
			focusedImage.style.filter = focusedImage.savedFilter;
		} else {
			unfocusImageOverlay();
			return;
		}
	});
	window.addEventListener("mousedown", GW.imageFocus.mouseDown = (event) => {
		GWLog("GW.imageFocus.mouseDown");
		event.preventDefault();

		let focusedImage = query("#image-focus-overlay img");
		if (focusedImage.height >= window.innerHeight || focusedImage.width >= window.innerWidth) {
			let mouseCoordX = event.clientX;
			let mouseCoordY = event.clientY;

			let imageCoordX = parseInt(getComputedStyle(focusedImage).left);
			let imageCoordY = parseInt(getComputedStyle(focusedImage).top);

			// Save the filter.
			focusedImage.savedFilter = focusedImage.style.filter;

			window.onmousemove = (event) => {
				// Remove the filter.
				focusedImage.style.filter = "none";
				focusedImage.style.left = imageCoordX + event.clientX - mouseCoordX + 'px';
				focusedImage.style.top = imageCoordY + event.clientY - mouseCoordY + 'px';
			};
			return false;
		}
	});

	// Double-click on the image unfocuses.
	clonedImage.addEventListener('dblclick', GW.imageFocus.doubleClick = (event) => {
		GWLog("GW.imageFocus.doubleClick");
		unfocusImageOverlay();
	});

	// Escape key unfocuses, spacebar resets.
	document.addEventListener("keyup", GW.imageFocus.keyUp = (event) => {
		GWLog("GW.imageFocus.keyUp");
		let allowedKeys = [ " ", "Spacebar", "Escape", "Esc" ];
		if (!allowedKeys.contains(event.key) || 
			getComputedStyle(query("#image-focus-overlay")).display == "none") return;

		event.preventDefault();

		switch (event.key) {
		case "Escape": 
		case "Esc":
			unfocusImageOverlay();
			break;
		case " ":
		case "Spacebar":
			resetFocusedImagePosition();
			break;
		}
	});

	// Prevent spacebar or arrow keys from scrolling page when image focused.
	togglePageScrolling(false);

	// Set the caption.
	setImageFocusCaption();

	// Moving mouse unhides image focus UI.
	window.addEventListener("mousemove", GW.imageFocus.mouseMoved = (event) => {
		GWLog("GW.imageFocus.mouseMoved");
		let currentDateTime = new Date();
		if (!(event.target.tagName == "IMG" || event.target.id == "image-focus-overlay")) {
			cancelImageFocusHideUITimer();
		} else {
			if (!GW.imageFocus.hideUITimer) {
				unhideImageFocusUI();
				GW.imageFocus.hideUITimer = setTimeout(GW.imageFocus.hideUITimerExpired, GW.imageFocus.hideUITimerDuration);
			}
			GW.imageFocus.mouseLastMovedAt = currentDateTime;
		}
	});
}

function resetFocusedImagePosition() {
	GWLog("resetFocusedImagePosition");
	let focusedImage = query("#image-focus-overlay img");
	if (!focusedImage) return;
	
	let sourceImage = query(GW.imageFocus.focusedImageSelector);

	// Make sure that initially, the image fits into the viewport.
	let constrainedWidth = Math.min(sourceImage.naturalWidth, window.innerWidth * GW.imageFocus.shrinkRatio);
	let widthShrinkRatio = constrainedWidth / sourceImage.naturalWidth;
	var constrainedHeight = Math.min(sourceImage.naturalHeight, window.innerHeight * GW.imageFocus.shrinkRatio);
	let heightShrinkRatio = constrainedHeight / sourceImage.naturalHeight;
	let shrinkRatio = Math.min(widthShrinkRatio, heightShrinkRatio);
	GWLog("Setting focused image dimensions...");
	focusedImage.style.width = (sourceImage.naturalWidth * shrinkRatio) + "px";
	focusedImage.style.height = (sourceImage.naturalHeight * shrinkRatio) + "px";

	// Remove modifications to position.
	focusedImage.style.left = "";
	focusedImage.style.top = "";

	// Set the cursor appropriately.
	setFocusedImageCursor();
}
function setFocusedImageCursor() {
	let focusedImage = query("#image-focus-overlay img");
	if (!focusedImage) return;
	focusedImage.style.cursor = (focusedImage.height >= window.innerHeight || focusedImage.width >= window.innerWidth) ? 
						 		'move' : '';
}

function unfocusImageOverlay() {
	GWLog("unfocusImageOverlay");

	// Remove event listeners.
	window.removeEventListener("wheel", GW.imageFocus.scrollEvent);
	window.removeEventListener("MozMousePixelScroll", GW.imageFocus.oldFirefoxCompatibilityScrollEvent);
	// NOTE: The double-click listener does not need to be removed manually,
	// because the focused (cloned) image will be removed anyway.
	document.removeEventListener("keyup", GW.imageFocus.keyUp);
	document.removeEventListener("keydown", GW.imageFocus.keyDown);
	window.removeEventListener("mousemove", GW.imageFocus.mouseMoved);
	window.removeEventListener("mousedown", GW.imageFocus.mouseDown);
	window.removeEventListener("mouseup", GW.imageFocus.mouseUp);

	// Remove focused image and hide overlay.
	let imageFocusOverlay = query("#image-focus-overlay");
	imageFocusOverlay.classList.toggle("engaged", false);
	imageFocusOverlay.query("img").remove();

	// Un-blur content/etc.
	queryAll(GW.imageFocus.pageContentSelector).forEach(element => {
		element.classList.toggle("blurred", false);
	});

	// Unset "focused" class of focused image.
	query(GW.imageFocus.focusedImageSelector).classList.toggle("focused", false);

	// Re-enable page scrolling.
	togglePageScrolling(true);
}

function setImageFocusCaption() {
	GWLog("setImageFocusCaption");
	var T = { }; // Temporary storage.

	// Clear existing caption, if any.
	let captionContainer = query("#image-focus-overlay .caption");
	Array.from(captionContainer.children).forEach(child => { child.remove(); });

	// Determine caption.
	let currentlyFocusedImage = query(GW.imageFocus.focusedImageSelector);
	var captionHTML;
	if ((T.enclosingFigure = currentlyFocusedImage.closest("figure")) && 
		(T.figcaption = T.enclosingFigure.query("figcaption"))) {
		captionHTML = (T.figcaption.query("p")) ? 
					  T.figcaption.innerHTML : 
					  "<p>" + T.figcaption.innerHTML + "</p>"; 
	} else if (currentlyFocusedImage.title != "") {
		captionHTML = `<p>${currentlyFocusedImage.title}</p>`;
	}
	// Insert the caption, if any.
	if (captionHTML) captionContainer.insertAdjacentHTML("beforeend", captionHTML);
}

function hideImageFocusUI() {
	GWLog("hideImageFocusUI");
	let imageFocusOverlay = query("#image-focus-overlay");
	imageFocusOverlay.queryAll(".help-overlay, .caption").forEach(element => {
		element.classList.toggle("hidden", true);
	});
}

function unhideImageFocusUI() {
	GWLog("unhideImageFocusUI");
	let imageFocusOverlay = query("#image-focus-overlay");
	imageFocusOverlay.queryAll(".help-overlay, .caption").forEach(element => {
		element.classList.toggle("hidden", false);
	});
}

function cancelImageFocusHideUITimer() {
	clearTimeout(GW.imageFocus.hideUITimer);
	GW.imageFocus.hideUITimer = null;
}

/******************/
/* INITIALIZATION */
/******************/

imageFocusSetup();