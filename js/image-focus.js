/* Image-focus.js */
/* Written by Obormot, 15 February 2019 */
/* License: GPL (derivative work of https://www.pmwiki.org/wiki/Cookbook/ImgFocus ) */
/* Lightweight dependency-free JavaScript library for "click to focus/zoom" images in HTML web pages. Originally coded for Obormot.net / GreaterWrong.com. */

ImageFocus = {
	/****************/
	/* Configuration.
	 ****************/

	contentImagesSelector: ".markdownBody figure img",

	excludedContainerElementsSelector: "a, button, figure.image-focus-not",

	imageGalleryInclusionTest: (image) => {
		return (   image.closest("#markdownBody") != null
				&& image.closest("div.footnotes") == null);
	},

	shrinkRatio: 0.975,

	hideUITimerDuration: 1500,

	dropShadowFilterForImages: " drop-shadow(10px 10px 10px #000) drop-shadow(0 0 10px #444)",

	/*****************/
	/* Infrastructure.
	 *****************/

	focusableImagesSelector: null,
	focusedImageSelector: null,
	galleryImagesSelector: null,

	hideUITimer: null,

	overlay: null,

	mouseLastMovedAt: 0,

	currentlyFocusedImage: null,

	/************/
	/* Functions.
	 ************/

	setup: () => {
		GWLog("ImageFocus.setup", "image-focus.js", 1);

		//  Create the image focus overlay.
		ImageFocus.overlay = addUIElement(`<div id="image-focus-overlay">
			<div class="help-overlay">
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
			<div class="caption"></div>
		</div>`);

		//  On orientation change, reset the size & position.
		GW.mediaQueries.portraitOrientation.addListener((event) => { requestAnimationFrame(ImageFocus.resetFocusedImagePosition); });

		//  Add click listeners to the buttons.
		ImageFocus.overlay.querySelectorAll(".slideshow-button").forEach(button => {
			button.addActivateEvent(ImageFocus.slideshowButtonClicked = (event) => {
				GWLog("ImageFocus.slideshowButtonClicked", "image-focus.js", 2);

				ImageFocus.focusNextImage(event.target.classList.contains("next"));
				event.target.blur();
			});
		});

		//  UI starts out hidden.
		ImageFocus.hideImageFocusUI();

		//	Selector-suffixing function.
		function suffixedSelector(selector, suffix) {
			return selector.split(", ").map(part => part + suffix).join(", ");
		}

		/*	Create auxiliary selectors by suffixing provided content images
			selector with appropriate classes.
		 */
		ImageFocus.focusableImagesSelector = suffixedSelector(ImageFocus.contentImagesSelector, ".focusable");
		ImageFocus.focusedImageSelector = suffixedSelector(ImageFocus.contentImagesSelector, ".focused");
		ImageFocus.galleryImagesSelector = suffixedSelector(ImageFocus.contentImagesSelector, ".gallery-image");

		//	Process images in main document.
		//	TODO: extend
		ImageFocus.processImagesWithin(document);

		GW.notificationCenter.fireEvent("ImageFocus.setupDidComplete");
	},

	processImagesWithin: (container) => {
		GWLog("ImageFocus.processImagesWithin", "image-focus.js", 1);

		/*	Add ‘focusable’ class to all focusable images; add ‘gallery-image’
			class to all focusable images that are to be included in the main
			image gallery.
		 */
		container.querySelectorAll(ImageFocus.contentImagesSelector).forEach(image => {
			if (image.closest(ImageFocus.excludedContainerElementsSelector))
				return;

			image.classList.add("focusable");

			if (ImageFocus.imageGalleryInclusionTest(image))
				image.classList.add("gallery-image");
		});

		//  Add the listener to all focusable images.
		container.querySelectorAll(ImageFocus.focusableImagesSelector).forEach(image => {
			image.addEventListener("click", ImageFocus.imageClickedToFocus);
		});

		//  Wrap all images in figures in a span.
		container.querySelectorAll(ImageFocus.focusableImagesSelector).forEach(image => {
			if (image.closest("figure") == null)
				return;

			let imageContainer = image.parentElement;

			let wrapper = newElement("SPAN", { "class": "image-wrapper" });
			wrapper.appendChild(image);
			imageContainer.insertBefore(wrapper, imageContainer.firstChild);

			/*  Set ‘focusable’ class on the wrapper, for CSS to apply the hover 
				style and tooltip.
			 */
			wrapper.classList.toggle("focusable", true);
		});

		if (container == document) {
			//  Count how many images there are in the post, and set the “… of X” label to that.
			ImageFocus.overlay.querySelector(".image-number").dataset.numberOfImages = container.querySelectorAll(ImageFocus.galleryImagesSelector).length;

			//  Accesskey-L starts the slideshow.
			(container.querySelector(ImageFocus.galleryImagesSelector)||{}).accessKey = "l";
		}
	},

	focusImage: (imageToFocus) => {
		GWLog("ImageFocus.focusImage", "image-focus.js", 1);

		//  Clear ‘last-focused’ class of last focused image.
		let lastFocusedImage = document.querySelector(ImageFocus.focusedImageSelector);
		if (lastFocusedImage) {
			lastFocusedImage.classList.remove("last-focused");
			lastFocusedImage.removeAttribute("accesskey");
		}

		//  Create the focused version of the image.
		imageToFocus.classList.toggle("focused", true);
		let clonedImage = imageToFocus.cloneNode(true);
		clonedImage.removeAttribute("width");
		clonedImage.removeAttribute("height");
		clonedImage.style = "";
		clonedImage.style.filter = imageToFocus.style.filter + ImageFocus.dropShadowFilterForImages;

		//  Add the image to the overlay.
		ImageFocus.overlay.appendChild(clonedImage);
		ImageFocus.overlay.classList.toggle("engaged", true);

		//  Save a reference to the image, so we don’t have to querySelector each time.
		ImageFocus.currentlyFocusedImage = clonedImage;

		//  Set image to default size and position.
		ImageFocus.resetFocusedImagePosition();

		//  Add listener to zoom image with scroll wheel.
		window.addEventListener("wheel", ImageFocus.scrollEvent, { passive: false });

		//  If image is bigger than viewport, it’s draggable. Otherwise, click unfocuses.
		window.addEventListener("mouseup", ImageFocus.mouseUp);
		window.addEventListener("mousedown", ImageFocus.mouseDown);

		//  Double-click on the image unfocuses.
		clonedImage.addEventListener("dblclick", ImageFocus.doubleClick);

		//  Escape key unfocuses, spacebar resets.
		document.addEventListener("keyup", ImageFocus.keyUp);

		requestAnimationFrame(() => {
			//  Prevent spacebar or arrow keys from scrolling page when image focused.
			togglePageScrolling(false);
		});

		//  Mark the overlay as being in slide show mode (to show buttons/count).
		ImageFocus.overlay.classList.add("slideshow");

		//  Set state of next/previous buttons.
		let images = document.querySelectorAll(ImageFocus.galleryImagesSelector);
		let indexOfFocusedImage = ImageFocus.getIndexOfFocusedImage();
		ImageFocus.overlay.querySelector(".slideshow-button.previous").disabled = (indexOfFocusedImage == 0);
		ImageFocus.overlay.querySelector(".slideshow-button.next").disabled = (indexOfFocusedImage == images.length - 1);

		//  Set the image number.
		ImageFocus.overlay.querySelector(".image-number").textContent = (indexOfFocusedImage + 1);

		//  Replace the hash.
		if (!location.hash.startsWith("#if_slide_"))
			ImageFocus.savedHash = location.hash;
		relocate("#if_slide_" + (indexOfFocusedImage + 1));

		//  Set the caption.
		ImageFocus.setImageFocusCaption();

		//  Moving mouse unhides image focus UI.
		window.addEventListener("mousemove", ImageFocus.mouseMoved);
	},

	resetFocusedImagePosition: (useSelf = false) => {
		GWLog("ImageFocus.resetFocusedImagePosition", "image-focus.js", 2);

		let focusedImage = ImageFocus.currentlyFocusedImage;
		if (!focusedImage)
			return;

		let sourceImage = useSelf 
						  ? focusedImage 
						  : document.querySelector(ImageFocus.focusedImageSelector);

		//  Make sure that initially, the image fits into the viewport.
		let recompute = false;
		let imageWidth = sourceImage.naturalWidth;
		let imageHeight = sourceImage.naturalHeight;
		if (imageWidth == 0 || imageHeight == 0) {
			focusedImage.addEventListener("load", (event) => {
				ImageFocus.resetFocusedImagePosition(true);
			}, { once: true });
		}

		//	Constrain dimensions proportionally.
		let constrainedWidth = Math.min(imageWidth, window.innerWidth * ImageFocus.shrinkRatio);
		let widthShrinkRatio = constrainedWidth / imageWidth;
		let constrainedHeight = Math.min(imageHeight, window.innerHeight * ImageFocus.shrinkRatio);
		let heightShrinkRatio = constrainedHeight / imageHeight;
		let shrinkRatio = Math.min(widthShrinkRatio, heightShrinkRatio);

		//	Set dimensions via CSS.
		focusedImage.style.width = (imageWidth * shrinkRatio) + "px";
		focusedImage.style.height = (imageHeight * shrinkRatio) + "px";

		//  Remove modifications to position.
		focusedImage.style.left = "";
		focusedImage.style.top = "";

		//  Set the cursor appropriately.
		ImageFocus.setFocusedImageCursor();
	},

	setFocusedImageCursor: () => {
		GWLog("ImageFocus.setFocusedImageCursor", "image-focus.js", 2);

		let focusedImage = ImageFocus.currentlyFocusedImage;
		if (!focusedImage)
			return;
		focusedImage.style.cursor = (   focusedImage.height >= window.innerHeight
									 || focusedImage.width >= window.innerWidth)
									? "move"
									: "";
	},

	unfocusImageOverlay: () => {
		GWLog("ImageFocus.unfocusImageOverlay", "image-focus.js", 1);

		//  Remove event listeners.
		window.removeEventListener("wheel", ImageFocus.scrollEvent);
		//  NOTE: The double-click listener does not need to be removed manually,
		//  because the focused (cloned) image will be removed anyway.
		document.removeEventListener("keyup", ImageFocus.keyUp);
		window.removeEventListener("mousemove", ImageFocus.mouseMoved);
		window.removeEventListener("mousedown", ImageFocus.mouseDown);
		window.removeEventListener("mouseup", ImageFocus.mouseUp);

		//  Set accesskey of currently focused image.
		let currentlyFocusedImage = document.querySelector(ImageFocus.focusedImageSelector)
		if (currentlyFocusedImage) {
			currentlyFocusedImage.classList.toggle("last-focused", true);
			currentlyFocusedImage.accessKey = "l";
		}

		//  Remove focused image and hide overlay.
		ImageFocus.overlay.classList.remove("engaged");
		ImageFocus.currentlyFocusedImage.remove();
		ImageFocus.currentlyFocusedImage = null;

		//  Unset ‘focused’ class of focused image.
		document.querySelector(ImageFocus.focusedImageSelector).classList.remove("focused");

		requestAnimationFrame(() => {
			//  Re-enable page scrolling.
			togglePageScrolling(true);
		});

		//  Reset the hash, if needed.
		if (location.hash.startsWith("#if_slide_")) {
			relocate(ImageFocus.savedHash || "#");
			ImageFocus.savedHash = null;
		}
	},

	getIndexOfFocusedImage: () => {
		let images = document.querySelectorAll(ImageFocus.galleryImagesSelector);
		let indexOfFocusedImage = -1;
		for (i = 0; i < images.length; i++) {
			if (images[i].classList.contains("focused")) {
				indexOfFocusedImage = i;
				break;
			}
		}
		return indexOfFocusedImage;
	},

	focusNextImage: (next = true) => {
		GWLog("ImageFocus.focusNextImage", "image-focus.js", 1);

		let images = document.querySelectorAll(ImageFocus.galleryImagesSelector);
		let indexOfFocusedImage = ImageFocus.getIndexOfFocusedImage();

		if (next ? (++indexOfFocusedImage == images.length) : (--indexOfFocusedImage == -1))
			return;

		//  Remove existing image.
		ImageFocus.currentlyFocusedImage.remove();
		//  Unset ‘focused’ class of just-removed image.
		document.querySelector(ImageFocus.focusedImageSelector).classList.remove("focused");

		//  Create the focused version of the image.
		images[indexOfFocusedImage].classList.toggle("focused", true);
		let clonedImage = images[indexOfFocusedImage].cloneNode(true);
		clonedImage.style = "";
		clonedImage.removeAttribute("width");
		clonedImage.removeAttribute("height");
		clonedImage.style.filter = images[indexOfFocusedImage].style.filter + ImageFocus.dropShadowFilterForImages;
		ImageFocus.overlay.appendChild(clonedImage);
		ImageFocus.currentlyFocusedImage = clonedImage;
		ImageFocus.overlay.classList.toggle("engaged", true);
		//  Set image to default size and position.
		ImageFocus.resetFocusedImagePosition();
		//  Double-click on the image unfocuses.
		clonedImage.addEventListener("dblclick", ImageFocus.doubleClick);
		//  Set state of next/previous buttons.
		ImageFocus.overlay.querySelector(".slideshow-button.previous").disabled = (indexOfFocusedImage == 0);
		ImageFocus.overlay.querySelector(".slideshow-button.next").disabled = (indexOfFocusedImage == images.length - 1);
		//  Set the image number display.
		ImageFocus.overlay.querySelector(".image-number").textContent = (indexOfFocusedImage + 1);
		//  Set the caption.
		ImageFocus.setImageFocusCaption();
		//  Replace the hash.
		relocate("#if_slide_" + (indexOfFocusedImage + 1));
	},

	setImageFocusCaption: () => {
		GWLog("ImageFocus.setImageFocusCaption", "image-focus.js", 2);

		let T = { }; //  Temporary storage.

		//  Clear existing caption, if any.
		let captionContainer = ImageFocus.overlay.querySelector(".caption");
		captionContainer.replaceChildren();

		//  Determine caption.
		let currentlyFocusedImage = document.querySelector(ImageFocus.focusedImageSelector);
		let captionHTML;
		if (   (T.enclosingFigure = currentlyFocusedImage.closest("figure"))
			&& (T.figcaption = T.enclosingFigure.querySelector("figcaption"))) {
			captionHTML = T.figcaption.querySelector("p") 
						  ? T.figcaption.innerHTML 
						  : "<p>" + T.figcaption.innerHTML + "</p>";
		} else if (currentlyFocusedImage.title != "") {
			captionHTML = `<p>${currentlyFocusedImage.title}</p>`;
		}
		//  Insert the caption, if any.
		if (captionHTML)
			captionContainer.insertAdjacentHTML("beforeend", captionHTML);
	},

	focusImageSpecifiedByURL: () => {
		GWLog("ImageFocus.focusImageSpecifiedByURL", "image-focus.js", 1);

		if (location.hash.startsWith("#if_slide_")) {
			doWhenPageLoaded(() => {
				let images = document.querySelectorAll(ImageFocus.galleryImagesSelector);
				let imageToFocus = (/#if_slide_([0-9]+)/.exec(location.hash)||{})[1];
				if (   imageToFocus > 0
					&& imageToFocus <= images.length) {
					ImageFocus.focusImage(images[imageToFocus - 1]);

					//  Set timer to hide the image focus UI.
					if (GW.isMobile() == false)
						ImageFocus.unhideImageFocusUI();
				}
			});
		}
	},

	/************************************/
	/* Image gallery UI showing / hiding.
	 ************************************/

	hideImageFocusUI: () => {
		GWLog("ImageFocus.hideImageFocusUI", "image-focus.js", 3);

		ImageFocus.overlay.querySelectorAll(".slideshow-button, .help-overlay, .image-number, .caption").forEach(element => {
			element.classList.toggle("hidden", true);
		});
	},

	hideUITimerExpired: () => {
		GWLog("ImageFocus.hideUITimerExpired", "image-focus.js", 3);

		let timeSinceLastMouseMove = (new Date()) - ImageFocus.mouseLastMovedAt;
		if (timeSinceLastMouseMove < ImageFocus.hideUITimerDuration) {
			ImageFocus.hideUITimer = setTimeout(ImageFocus.hideUITimerExpired, (ImageFocus.hideUITimerDuration - timeSinceLastMouseMove));
		} else {
			ImageFocus.hideImageFocusUI();
			ImageFocus.cancelImageFocusHideUITimer();
		}
	},

	unhideImageFocusUI: () => {
		GWLog("ImageFocus.unhideImageFocusUI", "image-focus.js", 3);

		ImageFocus.overlay.querySelectorAll(".slideshow-button, .help-overlay, .image-number, .caption").forEach(element => {
			element.classList.toggle("hidden", false);
		});

		ImageFocus.hideUITimer = setTimeout(ImageFocus.hideUITimerExpired, ImageFocus.hideUITimerDuration);
	},

	cancelImageFocusHideUITimer: () => {
		GWLog("ImageFocus.cancelImageFocusHideUITimer", "image-focus.js", 3);

		clearTimeout(ImageFocus.hideUITimer);
		ImageFocus.hideUITimer = null;
	},

	/*********/
	/* Events.
	 *********/

	//  Event listener for clicking on images to focus them.
	imageClickedToFocus: (event) => {
		GWLog("ImageFocus.imageClickedToFocus", "image-focus.js", 2);

		ImageFocus.focusImage(event.target);

		//  Set timer to hide the image focus UI.
		if (GW.isMobile() == false)
			ImageFocus.unhideImageFocusUI();
	},

	scrollEvent: (event) => {
		GWLog("ImageFocus.scrollEvent", "image-focus.js", 3);

		event.preventDefault();

		let image = ImageFocus.currentlyFocusedImage;

		//  Remove the filter.
		image.savedFilter = image.style.filter;
		image.style.filter = "none";

		//  Get bounding box of the image within the viewport.
		let imageBoundingBox = image.getBoundingClientRect();

		//  Calculate resize factor.
		let factor = ((image.height > 10 && image.width > 10) || event.deltaY < 0)
					 ? 1 + Math.sqrt(Math.abs(event.deltaY))/100.0
					 : 1;

		//  Resize.
		image.style.width = (event.deltaY < 0 ?
							(image.clientWidth * factor) :
							(image.clientWidth / factor))
							+ "px";
		image.style.height = "";

		//  Designate zoom origin.
		let zoomOrigin;

		//  Zoom from cursor if we’re zoomed in to where image exceeds screen, AND
		//  the cursor is over the image.
		let imageSizeExceedsWindowBounds = (   image.getBoundingClientRect().width > window.innerWidth
											|| image.getBoundingClientRect().height > window.innerHeight);
		let zoomingFromCursor =    imageSizeExceedsWindowBounds
								&& (   imageBoundingBox.left <= event.clientX
									&& event.clientX <= imageBoundingBox.right
									&& imageBoundingBox.top <= event.clientY
									&& event.clientY <= imageBoundingBox.bottom);

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
		ImageFocus.setFocusedImageCursor();
	},

	mouseUp: (event) => {
		GWLog("ImageFocus.mouseUp", "image-focus.js", 2);

		window.onmousemove = "";

		//  We only want to do anything on left-clicks.
		if (event.button != 0)
			return;

		//  Don’t unfocus if click was on a slideshow next/prev button!
		if (event.target.closest(".slideshow-button"))
			return;

		//  We also don’t want to do anything if clicked on the help overlay.
		if (event.target.closest(".help-overlay"))
			return;

		let focusedImage = ImageFocus.currentlyFocusedImage;
		if (   (   event.target == focusedImage
				|| event.target.tagName == "HTML")
			&& (   focusedImage.height >= window.innerHeight
				|| focusedImage.width >= window.innerWidth)) {
			//  If the mouseup event was the end of a pan of an oversized image,
			//  put the filter back; do not unfocus.
			focusedImage.style.filter = focusedImage.savedFilter;
		} else if (event.target.tagName != "HTML") {
			ImageFocus.unfocusImageOverlay();
			return;
		}
	},

	mouseDown: (event) => {
		GWLog("ImageFocus.mouseDown", "image-focus.js", 2);

		//  We only want to do anything on left-clicks.
		if (event.button != 0)
			return;

		event.preventDefault();

		let focusedImage = ImageFocus.currentlyFocusedImage;
		if (   focusedImage.height >= window.innerHeight
			|| focusedImage.width >= window.innerWidth) {
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
	},

	doubleClick: (event) => {
		GWLog("ImageFocus.doubleClick", "image-focus.js", 2);

		//  Don’t unfocus if click was on a slideshow next/prev button!
		if (event.target.closest(".slideshow-button"))
			return;

		//  We also don’t want to do anything if clicked on the help overlay.
		if (event.target.closest(".help-overlay"))
			return;

		ImageFocus.unfocusImageOverlay();
	},

	keyUp: (event) => {
		GWLog("ImageFocus.keyUp", "image-focus.js", 3);

		let allowedKeys = [ " ", "Spacebar", "Escape", "Esc", "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight", "Up", "Down", "Left", "Right" ];
		if (   !allowedKeys.includes(event.key)
			|| getComputedStyle(ImageFocus.overlay).display == "none")
			return;

		event.preventDefault();

		switch (event.key) {
		case "Escape":
		case "Esc":
			ImageFocus.unfocusImageOverlay();
			break;
		case " ":
		case "Spacebar":
			ImageFocus.resetFocusedImagePosition();
			break;
		case "ArrowDown":
		case "Down":
		case "ArrowRight":
		case "Right":
			if (document.querySelector(ImageFocus.focusedImageSelector))
				ImageFocus.focusNextImage(true);
			break;
		case "ArrowUp":
		case "Up":
		case "ArrowLeft":
		case "Left":
			if (document.querySelector(ImageFocus.focusedImageSelector))
				ImageFocus.focusNextImage(false);
			break;
		}
	},

	mouseMoved: (event) => {
		GWLog("ImageFocus.mouseMoved", "image-focus.js", 3);

		let currentDateTime = new Date();
		if (!(   event.target == ImageFocus.currentlyFocusedImage
			  || event.target == ImageFocus.overlay)) {
			ImageFocus.cancelImageFocusHideUITimer();
		} else {
			if (ImageFocus.hideUITimer == null)
				ImageFocus.unhideImageFocusUI();

			ImageFocus.mouseLastMovedAt = currentDateTime;
		}
	}
};

GW.notificationCenter.fireEvent("ImageFocus.didLoad");

/******************/
/* INITIALIZATION */
/******************/

doWhenDOMContentLoaded(ImageFocus.setup);
doWhenDOMContentLoaded(ImageFocus.focusImageSpecifiedByURL);

//  TODO: re-enable this once imageFocusSetup() is refactored to be repeatable
//  GW.notificationCenter.addHandlerForEvent("Sidenotes.sidenotesDidConstruct", (info) => {
// 	imageFocusSetup();
//  }, { once: true });
