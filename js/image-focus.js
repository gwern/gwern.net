/* Image-focus.js */
/* Written by Obormot, 15 February 2019 */
/* License: GPL (derivative work of https://www.pmwiki.org/wiki/Cookbook/ImgFocus ) */
/* Lightweight dependency-free JavaScript library for "click to focus/zoom" images in HTML web pages. Originally coded for Obormot.net / GreaterWrong.com. */

/***************/
/* IMAGE FOCUS */
/***************/

function imageFocusSetup() {
	GWLog("imageFocusSetup", "image-focus.js", 1);

	if (typeof GW.imageFocus == "undefined")
		GW.imageFocus = {
			contentImagesSelector:	"#markdownBody img",
			focusedImageSelector:	"#markdownBody img.focused",
			shrinkRatio:			0.975,
			hideUITimer:			null,
			hideUITimerDuration:	1500,
			hideUITimerExpired:		() => {
				GWLog("GW.imageFocus.hideUITimerExpired", "image-focus.js", 3);

				let timeSinceLastMouseMove = (new Date()) - GW.imageFocus.mouseLastMovedAt;
				if (timeSinceLastMouseMove < GW.imageFocus.hideUITimerDuration) {
					GW.imageFocus.hideUITimer = setTimeout(GW.imageFocus.hideUITimerExpired, (GW.imageFocus.hideUITimerDuration - timeSinceLastMouseMove));
				} else {
					hideImageFocusUI();
					cancelImageFocusHideUITimer();
				}
			},
			overlay:				null
		};

	//  Create event listener for clicking on images to focus them.
	GW.imageClickedToFocus = (event) => {
		GWLog("GW.imageClickedToFocus", "image-focus.js", 2);

		focusImage(event.target);

		if (!GW.isMobile()) {
			//  Set timer to hide the image focus UI.
			unhideImageFocusUI();
			GW.imageFocus.hideUITimer = setTimeout(GW.imageFocus.hideUITimerExpired, GW.imageFocus.hideUITimerDuration);
		}
	};
	//  Add the listener to all content images.
	document.querySelectorAll(GW.imageFocus.contentImagesSelector).forEach(image => {
		image.addEventListener("click", GW.imageClickedToFocus);
	});

	//  Wrap all images in figures in a span.
	document.querySelectorAll("figure img").forEach(image => {
		let imageContainer = image.parentElement;

		let wrapper = document.createElement("span");
		wrapper.classList.add("image-wrapper");
		wrapper.appendChild(image);
		imageContainer.insertBefore(wrapper, imageContainer.firstChild);

		//  Set ‘focusable’ class, for CSS to apply the hover style and tooltip.
		image.classList.toggle("focusable", true);
		wrapper.classList.toggle("focusable", true);
	});

	//  Create the image focus overlay.
	GW.imageFocus.overlay = addUIElement(`<div id="image-focus-overlay">` +
	`<div class="help-overlay">
		<p class="slideshow-help-text"><strong>Arrow keys:</strong> Next/previous image</p>
		<p><strong>Escape</strong> or <strong>click</strong>: Hide zoomed image</p>
		<p><strong>Space bar:</strong> Reset image size & position</p>
		<p><strong>Scroll</strong> to zoom in/out</p>
		<p>(When zoomed in, <strong>drag</strong> to pan; <br/><strong>double-click</strong> to close)</p>
	</div>
	<div class="image-number"></div>
	<div class="slideshow-buttons">
		<button type="button" class="slideshow-button previous" tabindex="-1" title="Previous image">
			<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 512">
				<path d="M34.52 239.03L228.87 44.69c9.37-9.37 24.57-9.37 33.94 0l22.67 22.67c9.36 9.36 9.37 24.52.04 33.9L131.49 256l154.02 154.75c9.34 9.38 9.32 24.54-.04 33.9l-22.67 22.67c-9.37 9.37-24.57 9.37-33.94 0L34.52 272.97c-9.37-9.37-9.37-24.57 0-33.94z"/>
			</svg>
		</button>
		<button type="button" class="slideshow-button next" tabindex="-1" title="Next image">
			<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 512">
				<path d="M285.476 272.971L91.132 467.314c-9.373 9.373-24.569 9.373-33.941 0l-22.667-22.667c-9.357-9.357-9.375-24.522-.04-33.901L188.505 256 34.484 101.255c-9.335-9.379-9.317-24.544.04-33.901l22.667-22.667c9.373-9.373 24.569-9.373 33.941 0L285.475 239.03c9.373 9.372 9.373 24.568.001 33.941z"/>
			</svg>
		</button>
	</div>
	<div class="caption"></div>` +
	`</div>`);
	GW.imageFocus.dropShadowFilterForImages = " drop-shadow(10px 10px 10px #000) drop-shadow(0 0 10px #444)";

	//  On orientation change, reset the size & position.
	GW.mediaQueries.portraitOrientation.addListener(() => { setTimeout(resetFocusedImagePosition, 0); });

	//  Accesskey-L starts the slideshow.
	(document.querySelector(GW.imageFocus.contentImagesSelector)||{}).accessKey = "l";

	//  Count how many images there are in the post, and set the “… of X” label to that.
	GW.imageFocus.overlay.querySelector(".image-number").dataset.numberOfImages = document.querySelectorAll(GW.imageFocus.contentImagesSelector).length;

	//  Add click listeners to the buttons.
	GW.imageFocus.overlay.querySelectorAll(".slideshow-button").forEach(button => {
		button.addActivateEvent(GW.imageFocus.slideshowButtonClicked = (event) => {
			GWLog("GW.imageFocus.slideshowButtonClicked", "image-focus.js", 2);

			focusNextImage(event.target.classList.contains("next"));
			event.target.blur();
		});
	});

	//  UI starts out hidden.
	hideImageFocusUI();

	GW.notificationCenter.fireEvent("ImageFocus.setupDidComplete");
}

function focusImage(imageToFocus) {
	GWLog("focusImage", "image-focus.js", 1);

	//  Clear ‘last-focused’ class of last focused image.
	let lastFocusedImage = document.querySelector(GW.imageFocus.focusedImageSelector);
	if (lastFocusedImage) {
		lastFocusedImage.classList.remove("last-focused");
		lastFocusedImage.removeAttribute("accesskey");
	}

	//  Create the focused version of the image.
	imageToFocus.classList.toggle("focused", true);
	let clonedImage = imageToFocus.cloneNode(true);
	clonedImage.style = "";
	clonedImage.removeAttribute("width");
	clonedImage.removeAttribute("height");
	clonedImage.style.filter = imageToFocus.style.filter + GW.imageFocus.dropShadowFilterForImages;

	//  Add the image to the overlay.
	GW.imageFocus.overlay.appendChild(clonedImage);
	GW.imageFocus.overlay.classList.toggle("engaged", true);

	//  Save a reference to the image, so we don’t have to querySelector each time.
	GW.currentlyFocusedImage = clonedImage;

	//  Set image to default size and position.
	resetFocusedImagePosition();

	//  Add listener to zoom image with scroll wheel.
	window.addEventListener("wheel", GW.imageFocus.scrollEvent = (event) => {
		GWLog("GW.imageFocus.scrollEvent", "image-focus.js", 3);

		event.preventDefault();

		let image = GW.currentlyFocusedImage;

		//  Remove the filter.
		image.savedFilter = image.style.filter;
		image.style.filter = "none";

		//  Get bounding box of the image within the viewport.
		let imageBoundingBox = image.getBoundingClientRect();

		//  Calculate resize factor.
		var factor = (image.height > 10 && image.width > 10) || event.deltaY < 0
					 ? 1 + Math.sqrt(Math.abs(event.deltaY))/100.0
					 : 1;

		//  Resize.
		image.style.width = (event.deltaY < 0 ?
							(image.clientWidth * factor) :
							(image.clientWidth / factor))
							+ "px";
		image.style.height = "";

		//  Designate zoom origin.
		var zoomOrigin;

		//  Zoom from cursor if we’re zoomed in to where image exceeds screen, AND
		//  the cursor is over the image.
		let imageSizeExceedsWindowBounds = (image.getBoundingClientRect().width > window.innerWidth || image.getBoundingClientRect().height > window.innerHeight);
		let zoomingFromCursor = imageSizeExceedsWindowBounds &&
								(imageBoundingBox.left <= event.clientX &&
								 event.clientX <= imageBoundingBox.right &&
								 imageBoundingBox.top <= event.clientY &&
								 event.clientY <= imageBoundingBox.bottom);

		//  Otherwise, if we’re zooming OUT, zoom from window center; if we’re
		//  zooming IN, zoom from image center.
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

		//  Calculate offset from zoom origin.
		let offsetOfImageFromZoomOrigin = {
			x: imageBoundingBox.x - zoomOrigin.x,
			y: imageBoundingBox.y - zoomOrigin.y
		}

		//  Calculate delta from centered zoom.
		let deltaFromCenteredZoom = {
			x: image.getBoundingClientRect().x - (zoomOrigin.x + (event.deltaY < 0 ? offsetOfImageFromZoomOrigin.x * factor : offsetOfImageFromZoomOrigin.x / factor)),
			y: image.getBoundingClientRect().y - (zoomOrigin.y + (event.deltaY < 0 ? offsetOfImageFromZoomOrigin.y * factor : offsetOfImageFromZoomOrigin.y / factor))
		}

		//  Adjust image position appropriately.
		image.style.left = parseInt(getComputedStyle(image).left) - deltaFromCenteredZoom.x + "px";
		image.style.top = parseInt(getComputedStyle(image).top) - deltaFromCenteredZoom.y + "px";

		//  Gradually re-center image, if it’s smaller than the window.
		if (!imageSizeExceedsWindowBounds) {
			let imageCenter = { x: image.getBoundingClientRect().x + image.getBoundingClientRect().width / 2,
								y: image.getBoundingClientRect().y + image.getBoundingClientRect().height / 2 }
			let windowCenter = { x: window.innerWidth / 2,
								 y: window.innerHeight / 2 }
			let imageOffsetFromCenter = { x: windowCenter.x - imageCenter.x,
										  y: windowCenter.y - imageCenter.y }

			//  Divide the offset by 10 because we’re nudging the image toward center,
			//  not jumping it there.
			image.style.left = parseInt(getComputedStyle(image).left) + imageOffsetFromCenter.x / 10 + "px";
			image.style.top = parseInt(getComputedStyle(image).top) + imageOffsetFromCenter.y / 10 + "px";
		}

		//  Put the filter back.
		image.style.filter = image.savedFilter;

		//  Set the cursor appropriately.
		setFocusedImageCursor();
	}, { passive: false });

	//  If image is bigger than viewport, it’s draggable. Otherwise, click unfocuses.
	window.addEventListener("mouseup", GW.imageFocus.mouseUp = (event) => {
		GWLog("GW.imageFocus.mouseUp", "image-focus.js", 2);

		window.onmousemove = "";

		//  We only want to do anything on left-clicks.
		if (event.button != 0) return;

		//  Don’t unfocus if click was on a slideshow next/prev button!
		if (event.target.closest(".slideshow-button")) return;

		//  We also don’t want to do anything if clicked on the help overlay.
		if (event.target.closest(".help-overlay")) return;

		let focusedImage = GW.currentlyFocusedImage;
		if ((event.target == focusedImage || event.target.tagName == "HTML") &&
			(focusedImage.height >= window.innerHeight || focusedImage.width >= window.innerWidth)) {
			//  If the mouseup event was the end of a pan of an oversized image,
			//  put the filter back; do not unfocus.
			focusedImage.style.filter = focusedImage.savedFilter;
		} else if (event.target.tagName != "HTML") {
			unfocusImageOverlay();
			return;
		}
	});
	window.addEventListener("mousedown", GW.imageFocus.mouseDown = (event) => {
		GWLog("GW.imageFocus.mouseDown", "image-focus.js", 2);

		//  We only want to do anything on left-clicks.
		if (event.button != 0) return;

		event.preventDefault();

		let focusedImage = GW.currentlyFocusedImage;
		if (focusedImage.height >= window.innerHeight || focusedImage.width >= window.innerWidth) {
			let mouseCoordX = event.clientX;
			let mouseCoordY = event.clientY;

			let imageCoordX = parseInt(getComputedStyle(focusedImage).left);
			let imageCoordY = parseInt(getComputedStyle(focusedImage).top);

			//  Save the filter.
			focusedImage.savedFilter = focusedImage.style.filter;

			window.onmousemove = (event) => {
				//  Remove the filter.
				focusedImage.style.filter = "none";
				focusedImage.style.left = imageCoordX + event.clientX - mouseCoordX + "px";
				focusedImage.style.top = imageCoordY + event.clientY - mouseCoordY + "px";
			};
			return false;
		}
	});

	//  Double-click on the image unfocuses.
	clonedImage.addEventListener("dblclick", GW.imageFocus.doubleClick = (event) => {
		GWLog("GW.imageFocus.doubleClick", "image-focus.js", 2);

		//  Don’t unfocus if click was on a slideshow next/prev button!
		if (event.target.closest(".slideshow-button")) return;

		//  We also don’t want to do anything if clicked on the help overlay.
		if (event.target.closest(".help-overlay")) return;

		unfocusImageOverlay();
	});

	//  Escape key unfocuses, spacebar resets.
	document.addEventListener("keyup", GW.imageFocus.keyUp = (event) => {
		GWLog("GW.imageFocus.keyUp", "image-focus.js", 3);

		let allowedKeys = [ " ", "Spacebar", "Escape", "Esc", "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight", "Up", "Down", "Left", "Right" ];
		if (   !allowedKeys.includes(event.key)
			|| getComputedStyle(GW.imageFocus.overlay).display == "none")
			return;

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
		case "ArrowDown":
		case "Down":
		case "ArrowRight":
		case "Right":
			if (document.querySelector(GW.imageFocus.focusedImageSelector))
				focusNextImage(true);
			break;
		case "ArrowUp":
		case "Up":
		case "ArrowLeft":
		case "Left":
			if (document.querySelector(GW.imageFocus.focusedImageSelector))
				focusNextImage(false);
			break;
		}
	});

	setTimeout(() => {
		//  Prevent spacebar or arrow keys from scrolling page when image focused.
		togglePageScrolling(false);
	});

	//  Mark the overlay as being in slide show mode (to show buttons/count).
	GW.imageFocus.overlay.classList.add("slideshow");

	//  Set state of next/previous buttons.
	let images = document.querySelectorAll(GW.imageFocus.contentImagesSelector);
	var indexOfFocusedImage = getIndexOfFocusedImage();
	GW.imageFocus.overlay.querySelector(".slideshow-button.previous").disabled = (indexOfFocusedImage == 0);
	GW.imageFocus.overlay.querySelector(".slideshow-button.next").disabled = (indexOfFocusedImage == images.length - 1);

	//  Set the image number.
	GW.imageFocus.overlay.querySelector(".image-number").textContent = (indexOfFocusedImage + 1);

	//  Replace the hash.
	if (!location.hash.startsWith("#if_slide_"))
	    GW.imageFocus.savedHash = location.hash;
	history.replaceState(null, null, "#if_slide_" + (indexOfFocusedImage + 1));

	//  Set the caption.
	setImageFocusCaption();

	//  Moving mouse unhides image focus UI.
	window.addEventListener("mousemove", GW.imageFocus.mouseMoved = (event) => {
		GWLog("GW.imageFocus.mouseMoved", "image-focus.js", 3);

		let currentDateTime = new Date();
		if (!(event.target == GW.imageFocus.currentlyFocusedImage || event.target == GW.imageFocus.overlay)) {
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
	GWLog("resetFocusedImagePosition", "image-focus.js", 2);

	let focusedImage = GW.currentlyFocusedImage;
	if (!focusedImage) return;

	let sourceImage = document.querySelector(GW.imageFocus.focusedImageSelector);

	//  Make sure that initially, the image fits into the viewport.
	let constrainedWidth = Math.min(sourceImage.naturalWidth, window.innerWidth * GW.imageFocus.shrinkRatio);
	let widthShrinkRatio = constrainedWidth / sourceImage.naturalWidth;
	var constrainedHeight = Math.min(sourceImage.naturalHeight, window.innerHeight * GW.imageFocus.shrinkRatio);
	let heightShrinkRatio = constrainedHeight / sourceImage.naturalHeight;
	let shrinkRatio = Math.min(widthShrinkRatio, heightShrinkRatio);
	focusedImage.style.width = (sourceImage.naturalWidth * shrinkRatio) + "px";
	focusedImage.style.height = (sourceImage.naturalHeight * shrinkRatio) + "px";

	//  Remove modifications to position.
	focusedImage.style.left = "";
	focusedImage.style.top = "";

	//  Set the cursor appropriately.
	setFocusedImageCursor();
}
function setFocusedImageCursor() {
	GWLog("setFocusedImageCursor", "image-focus.js", 2);

	let focusedImage = GW.currentlyFocusedImage;
	if (!focusedImage) return;
	focusedImage.style.cursor = (focusedImage.height >= window.innerHeight || focusedImage.width >= window.innerWidth)
								? "move" : "";
}

function unfocusImageOverlay() {
	GWLog("unfocusImageOverlay", "image-focus.js", 1);

	//  Remove event listeners.
	window.removeEventListener("wheel", GW.imageFocus.scrollEvent);
	//  NOTE: The double-click listener does not need to be removed manually,
	//  because the focused (cloned) image will be removed anyway.
	document.removeEventListener("keyup", GW.imageFocus.keyUp);
	window.removeEventListener("mousemove", GW.imageFocus.mouseMoved);
	window.removeEventListener("mousedown", GW.imageFocus.mouseDown);
	window.removeEventListener("mouseup", GW.imageFocus.mouseUp);

	//  Set accesskey of currently focused image.
	let currentlyFocusedImage = document.querySelector(GW.imageFocus.focusedImageSelector)
	if (currentlyFocusedImage) {
		currentlyFocusedImage.classList.toggle("last-focused", true);
		currentlyFocusedImage.accessKey = "l";
	}

	//  Remove focused image and hide overlay.
	GW.imageFocus.overlay.classList.remove("engaged");
	GW.currentlyFocusedImage.remove();
	GW.currentlyFocusedImage = null;

	//  Unset ‘focused’ class of focused image.
	document.querySelector(GW.imageFocus.focusedImageSelector).classList.remove("focused");

	setTimeout(() => {
		//  Re-enable page scrolling.
		togglePageScrolling(true);
	});

	//  Reset the hash, if needed.
	if (location.hash.startsWith("#if_slide_")) {
		history.replaceState(null, null, GW.imageFocus.savedHash || "#");
		GW.imageFocus.savedHash = null;
	}
}

function getIndexOfFocusedImage() {
	let images = document.querySelectorAll(GW.imageFocus.contentImagesSelector);
	var indexOfFocusedImage = -1;
	for (i = 0; i < images.length; i++) {
		if (images[i].classList.contains("focused")) {
			indexOfFocusedImage = i;
			break;
		}
	}
	return indexOfFocusedImage;
}

function focusNextImage(next = true) {
	GWLog("focusNextImage", "image-focus.js", 1);

	let images = document.querySelectorAll(GW.imageFocus.contentImagesSelector);
	var indexOfFocusedImage = getIndexOfFocusedImage();

	if (next ? (++indexOfFocusedImage == images.length) : (--indexOfFocusedImage == -1)) return;

	//  Remove existing image.
	GW.currentlyFocusedImage.remove();
	//  Unset ‘focused’ class of just-removed image.
	document.querySelector(GW.imageFocus.focusedImageSelector).classList.remove("focused");

	//  Create the focused version of the image.
	images[indexOfFocusedImage].classList.toggle("focused", true);
	let clonedImage = images[indexOfFocusedImage].cloneNode(true);
	clonedImage.style = "";
	clonedImage.removeAttribute("width");
	clonedImage.removeAttribute("height");
	clonedImage.style.filter = images[indexOfFocusedImage].style.filter + GW.imageFocus.dropShadowFilterForImages;
	GW.imageFocus.overlay.appendChild(clonedImage);
	GW.currentlyFocusedImage = clonedImage;
	GW.imageFocus.overlay.classList.toggle("engaged", true);
	//  Set image to default size and position.
	resetFocusedImagePosition();
	//  Set state of next/previous buttons.
	GW.imageFocus.overlay.querySelector(".slideshow-button.previous").disabled = (indexOfFocusedImage == 0);
	GW.imageFocus.overlay.querySelector(".slideshow-button.next").disabled = (indexOfFocusedImage == images.length - 1);
	//  Set the image number display.
	GW.imageFocus.overlay.querySelector(".image-number").textContent = (indexOfFocusedImage + 1);
	//  Set the caption.
	setImageFocusCaption();
	//  Replace the hash.
	history.replaceState(null, null, "#if_slide_" + (indexOfFocusedImage + 1));
}

function setImageFocusCaption() {
	GWLog("setImageFocusCaption", "image-focus.js", 2);

	var T = { }; //  Temporary storage.

	//  Clear existing caption, if any.
	let captionContainer = GW.imageFocus.overlay.querySelector(".caption");
	Array.from(captionContainer.children).forEach(child => { child.remove(); });

	//  Determine caption.
	let currentlyFocusedImage = document.querySelector(GW.imageFocus.focusedImageSelector);
	var captionHTML;
	if ((T.enclosingFigure = currentlyFocusedImage.closest("figure")) &&
		(T.figcaption = T.enclosingFigure.querySelector("figcaption"))) {
		captionHTML = (T.figcaption.querySelector("p")) ?
					  T.figcaption.innerHTML :
					  "<p>" + T.figcaption.innerHTML + "</p>";
	} else if (currentlyFocusedImage.title != "") {
		captionHTML = `<p>${currentlyFocusedImage.title}</p>`;
	}
	//  Insert the caption, if any.
	if (captionHTML) captionContainer.insertAdjacentHTML("beforeend", captionHTML);
}

function hideImageFocusUI() {
	GWLog("hideImageFocusUI", "image-focus.js", 3);

	GW.imageFocus.overlay.querySelectorAll(".slideshow-button, .help-overlay, .image-number, .caption").forEach(element => {
		element.classList.toggle("hidden", true);
	});
}

function unhideImageFocusUI() {
	GWLog("unhideImageFocusUI", "image-focus.js", 3);

	GW.imageFocus.overlay.querySelectorAll(".slideshow-button, .help-overlay, .image-number, .caption").forEach(element => {
		element.classList.remove("hidden");
	});
}

function cancelImageFocusHideUITimer() {
	GWLog("cancelImageFocusHideUITimer", "image-focus.js", 3);

	clearTimeout(GW.imageFocus.hideUITimer);
	GW.imageFocus.hideUITimer = null;
}

function focusImageSpecifiedByURL() {
	GWLog("focusImageSpecifiedByURL", "image-focus.js", 1);

	if (location.hash.startsWith("#if_slide_")) {
		doWhenPageLoaded(() => {
			let images = document.querySelectorAll(GW.imageFocus.contentImagesSelector);
			let imageToFocus = (/#if_slide_([0-9]+)/.exec(location.hash)||{})[1];
			if (imageToFocus > 0 && imageToFocus <= images.length) {
				focusImage(images[imageToFocus - 1]);

				if (!GW.isMobile()) {
					//  Set timer to hide the image focus UI.
					unhideImageFocusUI();
					GW.imageFocus.hideUITimer = setTimeout(GW.imageFocus.hideUITimerExpired, GW.imageFocus.hideUITimerDuration);
				}
			}
		});
	}
}

GW.notificationCenter.fireEvent("ImageFocus.didLoad");

/******************/
/* INITIALIZATION */
/******************/

doWhenDOMContentLoaded(imageFocusSetup);
doWhenDOMContentLoaded(focusImageSpecifiedByURL);

//  TODO: re-enable this once imageFocusSetup() is refactored to be repeatable
//  GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", () => {
// 	imageFocusSetup();
//  }, { once: true });
