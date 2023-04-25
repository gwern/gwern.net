/* Image-focus.js */
/* Written by Obormot, 15 February 2019 */
/* License: GPL (derivative work of https://www.pmwiki.org/wiki/Cookbook/ImgFocus ) */
/* Lightweight dependency-free JavaScript library for "click to focus/zoom" images in HTML web pages. Originally coded for Obormot.net / GreaterWrong.com. */

ImageFocus = {
	/****************/
	/* Configuration.
	 ****************/

	contentImagesSelector: [
		".markdownBody figure img"
	].join(", "),

	excludedContainerElementsSelector: [
		"a",
		"button",
		"figure.image-focus-not"
	].join(", "),

	imageGalleryInclusionTest: (image) => {
		return (   image.closest("#markdownBody") != null
				&& image.closest(".footnotes") == null
				&& image.classList.contains("page-thumbnail") == false);
	},

	shrinkRatio: 0.975,

	hideUITimerDuration: 1500,

	dropShadowFilterForImages: " drop-shadow(10px 10px 10px #000) drop-shadow(0 0 10px #444)",

	/*****************/
	/* Infrastructure.
	 *****************/

	imageFocusUIElementsSelector: [
		".slideshow-button",
		".help-overlay",
		".image-number",
		".caption"
	].join(", "),

	focusableImagesSelector: null,
	focusedImageSelector: null,
	galleryImagesSelector: null,

	hideUITimer: null,

	overlay: null,

	mouseLastMovedAt: 0,

	currentlyFocusedImage: null,
	
	imageInFocus: null,

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
				<p>(When zoomed in, <strong>drag</strong> to pan;<br /><strong>double-click</strong> to reset size & position)</p>
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
			<div class="loading-spinner">
				<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path d="M288 24.103v8.169a11.995 11.995 0 0 0 9.698 11.768C396.638 63.425 472 150.461 472 256c0 118.663-96.055 216-216 216-118.663 0-216-96.055-216-216 0-104.534 74.546-192.509 174.297-211.978A11.993 11.993 0 0 0 224 32.253v-8.147c0-7.523-6.845-13.193-14.237-11.798C94.472 34.048 7.364 135.575 8.004 257.332c.72 137.052 111.477 246.956 248.531 246.667C393.255 503.711 504 392.789 504 256c0-121.187-86.924-222.067-201.824-243.704C294.807 10.908 288 16.604 288 24.103z"/></svg>
			</div>
		</div>`);

		//  On orientation change, reset the size & position.
		GW.mediaQueries.portraitOrientation.addListener((event) => { requestAnimationFrame(ImageFocus.resetFocusedImagePosition); });

		//  Add click listeners to the buttons.
		ImageFocus.overlay.querySelectorAll(".slideshow-button").forEach(button => {
			button.addActivateEvent(ImageFocus.slideshowButtonClicked = (event) => {
				GWLog("ImageFocus.slideshowButtonClicked", "image-focus.js", 2);

				ImageFocus.focusNextImage(event.target.classList.contains("next"));
				ImageFocus.cancelImageFocusHideUITimer();
				event.target.blur();
			});
		});

		//	Add listeners to help overlay.
		let helpOverlay = ImageFocus.overlay.querySelector(".help-overlay");
		if (GW.isMobile()) {
			helpOverlay.addEventListener("click", (event) => {
				helpOverlay.classList.toggle("open");
			});
		} else {
			helpOverlay.addEventListener("mouseenter", (event) => {
				helpOverlay.classList.add("open");
			});
			helpOverlay.addEventListener("mouseleave", (event) => {
				helpOverlay.classList.remove("open");
			});
		}

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

        //  Add handler to set up events for images in injected content.
        GW.notificationCenter.addHandlerForEvent("GW.contentDidInject", ImageFocus.processImagesOnContentInject = (info) => {
            GWLog("ImageFocus.processImagesOnContentInject", "image-focus.js", 2);

            ImageFocus.processImagesWithin(info.container);

			//	If this content is (or is being loaded into) the main page...
			if (info.document == document) {
				//  Count how many images there are in the page, and set the “… of X” label to that.
				ImageFocus.overlay.querySelector(".image-number").dataset.numberOfImages = document.querySelectorAll(ImageFocus.galleryImagesSelector).length;

				//  Accesskey-L starts the slideshow.
				(document.querySelector(ImageFocus.galleryImagesSelector)||{}).accessKey = "l";
			}

			//	Fire targets-processed event.
			GW.notificationCenter.fireEvent("ImageFocus.imagesDidProcessOnContentInject", {
				source: "ImageFocus.processImagesOnContentInject",
				container: info.container,
				document: info.document
			});
        }, { phase: "eventListeners" });

		//	Add handler to focus image on hashchange event.
		GW.notificationCenter.addHandlerForEvent("GW.hashDidChange", (info) => {
			ImageFocus.focusImageSpecifiedByURL();
		});

        //  Fire setup-complete event.
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

		//  Wrap all focusable images in a span.
		container.querySelectorAll(ImageFocus.focusableImagesSelector).forEach(image => {
			wrapElement(image, "image-wrapper focusable", "SPAN");
		});
	},

	preloadImage: (image) => {
		if (image.naturalWidth > 0)
			return;

		image.loading = "eager";
		image.decoding = "sync";
	},

	focusImage: (imageToFocus) => {
		GWLog("ImageFocus.focusImage", "image-focus.js", 1);

		//	Show overlay.
		ImageFocus.enterImageFocus();

		//	Show UI.
		ImageFocus.unhideImageFocusUI();

		//	Unfocus currently focused image, if any.
		ImageFocus.unfocusImage();

		//	Focus new image.
		imageToFocus.classList.toggle("focused", true);

		/*	If the new image is part of the main image gallery (i.e., if we are
			in gallery mode, rather than single-image mode)...
		 */
		if (imageToFocus.classList.contains("gallery-image")) {
			//	Update slideshow state.
			let lastFocusedImage = document.querySelector("img.last-focused");
			if (lastFocusedImage) {
				lastFocusedImage.classList.remove("last-focused");
				lastFocusedImage.removeAttribute("accesskey");
			}

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

			//	Also preload the next and previous images.
			if (indexOfFocusedImage > 0)
				ImageFocus.preloadImage(images[indexOfFocusedImage - 1]);
			if (indexOfFocusedImage < images.length - 1)
				ImageFocus.preloadImage(images[indexOfFocusedImage + 1]);
		}

		//	Save reference to newly focused image.
		ImageFocus.currentlyFocusedImage = imageToFocus;

		//	Scroll to focused image.
		revealElement(ImageFocus.currentlyFocusedImage);

		//  Create the focused version of the image.
		ImageFocus.imageInFocus = imageToFocus.cloneNode(true);
		ImageFocus.imageInFocus.loading = "eager";
		ImageFocus.imageInFocus.decoding = "sync";
		ImageFocus.imageInFocus.style = "";
		ImageFocus.imageInFocus.style.filter = imageToFocus.style.filter + ImageFocus.dropShadowFilterForImages;
		ImageFocus.imageInFocus.removeAttribute("title");

		//	Allow for styling based on loading state.
		ImageFocus.imageInFocus.classList.add("loading");
		ImageFocus.imageInFocus.addEventListener("load", (event) => {
			event.target.classList.remove("loading");
		}, { once: true });

		//  Add the image to the overlay.
		ImageFocus.overlay.insertBefore(ImageFocus.imageInFocus, ImageFocus.overlay.querySelector(".loading-spinner"));

		//  Set image to default size and position.
		ImageFocus.resetFocusedImagePosition(true);

		//  If image is bigger than viewport, it’s draggable.
		ImageFocus.imageInFocus.addEventListener("mousedown", ImageFocus.imageMouseDown);

		//  If image is bigger than viewport, double-click resets size/position.
		ImageFocus.imageInFocus.addEventListener("dblclick", ImageFocus.doubleClick);

		/*  If this image is part of the main gallery, then mark the overlay as 
			being in slide show mode (to show buttons/count). Otherwise, the
			overlay should be in single-image mode.
		 */
		ImageFocus.overlay.classList.toggle("slideshow", imageToFocus.classList.contains("gallery-image"));

		//  Set the caption.
		ImageFocus.setImageFocusCaption();

		//	Fire event.
		GW.notificationCenter.fireEvent("ImageFocus.imageDidFocus", { image: imageToFocus });
	},

	resetFocusedImagePosition: (updateOnLoad = false) => {
		GWLog("ImageFocus.resetFocusedImagePosition", "image-focus.js", 2);

		if (ImageFocus.imageInFocus == null)
			return;

		//  Make sure that initially, the image fits into the viewport.
		let imageWidth, imageHeight;
		if ((new URL(ImageFocus.imageInFocus.src)).pathname.endsWith(".svg")) {
			//	Special handling for SVGs, which have no intrinsic size.
			imageWidth = imageHeight = Math.min(window.innerWidth, window.innerHeight);
		} else {
			//	Non-SVGs have intrinsic size.
			imageWidth = ImageFocus.imageInFocus.naturalWidth || ImageFocus.imageInFocus.getAttribute("width");
			imageHeight = ImageFocus.imageInFocus.naturalHeight || ImageFocus.imageInFocus.getAttribute("height");

			if (imageWidth * imageHeight == 0) {
				if (updateOnLoad == true) {
					//	Reset on load.
					ImageFocus.imageInFocus.addEventListener("load", (event) => {
						ImageFocus.resetFocusedImagePosition(false);
					}, { once: true });

					return;
				} else {
					//	This shouldn’t happen. Display an error?
					return;
				}
			}
		}

		//	Constrain dimensions proportionally.
		let constrainedWidth = Math.min(imageWidth, window.innerWidth * ImageFocus.shrinkRatio);
		let widthShrinkRatio = constrainedWidth / imageWidth;
		let constrainedHeight = Math.min(imageHeight, window.innerHeight * ImageFocus.shrinkRatio);
		let heightShrinkRatio = constrainedHeight / imageHeight;
		let shrinkRatio = Math.min(widthShrinkRatio, heightShrinkRatio);

		//	Set dimensions via CSS.
		ImageFocus.imageInFocus.style.width = Math.round(imageWidth * shrinkRatio) + "px";
		ImageFocus.imageInFocus.style.height = Math.round(imageHeight * shrinkRatio) + "px";

		//  Remove modifications to position.
		ImageFocus.imageInFocus.style.left = "";
		ImageFocus.imageInFocus.style.top = "";

		//  Set the cursor appropriately.
		ImageFocus.setFocusedImageCursor();
	},

	setFocusedImageCursor: () => {
		GWLog("ImageFocus.setFocusedImageCursor", "image-focus.js", 2);

		if (ImageFocus.imageInFocus == null)
			return;

		ImageFocus.imageInFocus.style.cursor = (   ImageFocus.imageInFocus.height >= window.innerHeight
												|| ImageFocus.imageInFocus.width >= window.innerWidth)
											   ? "move"
											   : "";
	},

	unfocusImage: () => {
		GWLog("ImageFocus.unfocusImage", "image-focus.js", 1);

		//  Remove image from overlay.
		if (ImageFocus.imageInFocus) {
			ImageFocus.imageInFocus.remove();
			ImageFocus.imageInFocus = null;
		}

		//	Update currently focused image in page.
		if (ImageFocus.currentlyFocusedImage) {
			//	Save reference to image-to-be-unfocused.
			let unfocusedImage = ImageFocus.currentlyFocusedImage;

			ImageFocus.currentlyFocusedImage.classList.remove("focused");
			ImageFocus.currentlyFocusedImage = null;

			//	Fire event.
			GW.notificationCenter.fireEvent("ImageFocus.imageDidUnfocus", { image: unfocusedImage });
		}
	},

	enterImageFocus: () => {
		GWLog("ImageFocus.enterImageFocus", "image-focus.js", 1);

		if (ImageFocus.overlay.classList.contains("engaged"))
			return;

		//	Show overlay.
		ImageFocus.overlay.classList.add("engaged");

		//  Add listener to zoom image with scroll wheel.
		window.addEventListener("wheel", ImageFocus.scrollEvent, { passive: false });

		//  Escape key unfocuses, spacebar resets.
		document.addEventListener("keyup", ImageFocus.keyUp);

		//  Prevent spacebar or arrow keys from scrolling page when image focused.
		requestAnimationFrame(() => {
			togglePageScrolling(false);
		});

		//  Moving mouse unhides image focus UI.
		window.addEventListener("mousemove", ImageFocus.mouseMoved);

		//	Drag-end event; also, click to unfocus.
		window.addEventListener("mouseup", ImageFocus.mouseUp);

		//	Fire event.
		GW.notificationCenter.fireEvent("ImageFocus.imageOverlayDidAppear");
	},

	exitImageFocus: () => {
		GWLog("ImageFocus.exitImageFocus", "image-focus.js", 1);

		/*	If currently focused image is part of the main image gallery, 
			preserve state.
		 */
		if (   ImageFocus.currentlyFocusedImage
			&& ImageFocus.currentlyFocusedImage.classList.contains("gallery-image")) {
			//	Update classes.
			ImageFocus.currentlyFocusedImage.classList.remove("focused");

			if (ImageFocus.currentlyFocusedImage.classList.contains("gallery-image")) {
				ImageFocus.currentlyFocusedImage.classList.add("last-focused");

				//  Set accesskey of currently focused image, to re-focus it.
				ImageFocus.currentlyFocusedImage.accessKey = "l";
			}

			//  Reset the hash, if needed.
			if (location.hash.startsWith("#if_slide_")) {
				relocate(ImageFocus.savedHash || location.pathname);
				ImageFocus.savedHash = null;
			}
		}

		//	Unfocus currently focused image.
		ImageFocus.unfocusImage();

		//  Remove event listeners.
		window.removeEventListener("wheel", ImageFocus.scrollEvent);
		window.removeEventListener("mousemove", ImageFocus.mouseMoved);
		window.removeEventListener("mouseup", ImageFocus.mouseUp);
		document.removeEventListener("keyup", ImageFocus.keyUp);

		//  Hide overlay.
		ImageFocus.overlay.classList.remove("engaged");

		requestAnimationFrame(() => {
			//  Re-enable page scrolling.
			togglePageScrolling(true);
		});

		//	Fire event.
		GW.notificationCenter.fireEvent("ImageFocus.imageOverlayDidDisappear");
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

		//	Find next image to focus.
		let images = document.querySelectorAll(ImageFocus.galleryImagesSelector);
		let indexOfFocusedImage = ImageFocus.getIndexOfFocusedImage();

		//	This shouldn’t happen, but...
		if (next ? (++indexOfFocusedImage == images.length) : (--indexOfFocusedImage == -1))
			return;

		//	Focus new image.
		ImageFocus.focusImage(images[indexOfFocusedImage]);
	},

	setImageFocusCaption: () => {
		GWLog("ImageFocus.setImageFocusCaption", "image-focus.js", 2);

		/*	Get the figure caption, the ‘title’ attribute of the image, and the 
			‘alt’ attribute of the image. Clean each of typographic invisibles
			and educate quotes. Discard duplicate strings. Wrap all remaining 
			(unique) strings in <p> tags, and inject into caption container.
		 */
		let figcaption = ImageFocus.currentlyFocusedImage.closest("figure").querySelector("figcaption");
		ImageFocus.overlay.querySelector(".caption").replaceChildren(newDocument(`<div class="caption-text-wrapper">` 
		  + [ ...[
				(figcaption ? figcaption.cloneNode(true) : null),
				newElement("SPAN", null, { "innerHTML": ImageFocus.currentlyFocusedImage.getAttribute("title") }),
				newElement("SPAN", null, { "innerHTML": ImageFocus.currentlyFocusedImage.getAttribute("alt") }),
			].map(element => {
				if (element)
					Typography.processElement(element, Typography.replacementTypes.CLEAN|Typography.replacementTypes.QUOTES);

				return element;
			}).filter((element, index, array) => (
					element != null
				 && isNodeEmpty(element) == false
				 && array.findIndex(otherElement => (
				 		otherElement != null
					 && otherElement.textContent.trim() == element.textContent.trim())
					) == index)
			).map(element => `<p>${(element.innerHTML.trim())}</p>`)
			].join("") 
		  + `</div>`
		  + `<p class="image-url" title="Click to copy image URL to clipboard">`
		  + `<code class="url">${ImageFocus.currentlyFocusedImage.src}</code>`
		  + `<span class="icon-container">`
		  + `<span class="icon normal"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="M433.941 65.941l-51.882-51.882A48 48 0 0 0 348.118 0H176c-26.51 0-48 21.49-48 48v48H48c-26.51 0-48 21.49-48 48v320c0 26.51 21.49 48 48 48h224c26.51 0 48-21.49 48-48v-48h80c26.51 0 48-21.49 48-48V99.882a48 48 0 0 0-14.059-33.941zM266 464H54a6 6 0 0 1-6-6V150a6 6 0 0 1 6-6h74v224c0 26.51 21.49 48 48 48h96v42a6 6 0 0 1-6 6zm128-96H182a6 6 0 0 1-6-6V54a6 6 0 0 1 6-6h106v88c0 13.255 10.745 24 24 24h88v202a6 6 0 0 1-6 6zm6-256h-64V48h9.632c1.591 0 3.117.632 4.243 1.757l48.368 48.368a6 6 0 0 1 1.757 4.243V112z"/></svg></span>`
		  + `<span class="icon copied"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path d="M256 512A256 256 0 1 0 256 0a256 256 0 1 0 0 512zM369 209L241 337c-9.4 9.4-24.6 9.4-33.9 0l-64-64c-9.4-9.4-9.4-24.6 0-33.9s24.6-9.4 33.9 0l47 47L335 175c9.4-9.4 24.6-9.4 33.9 0s9.4 24.6 0 33.9z"/></svg></span>`
		  + `</span>`
		  + `</p>`));

		//	Activate click-to-copy on image URL.
		let imageURLContainer = ImageFocus.overlay.querySelector(".caption .image-url");
		imageURLContainer.addActivateEvent((event) => {
			copyTextToClipboard(imageURLContainer.querySelector(".url").textContent);

			//	Update icon.
			imageURLContainer.classList.add("copied");

            //  Flash URL, for visual feedback of copy operation.
            imageURLContainer.classList.add("flash");
            setTimeout(() => { imageURLContainer.classList.remove("flash"); }, 150);
		});
		imageURLContainer.addEventListener("mouseleave", (event) => {
			//	Reset icon.
			imageURLContainer.classList.remove("copied");
		});
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
				}
			});
		}
	},

	/************************************/
	/* Image gallery UI showing / hiding.
	 ************************************/

	hideImageFocusUI: () => {
		GWLog("ImageFocus.hideImageFocusUI", "image-focus.js", 3);

		ImageFocus.overlay.querySelectorAll(ImageFocus.imageFocusUIElementsSelector).forEach(element => {
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

		ImageFocus.overlay.querySelectorAll(ImageFocus.imageFocusUIElementsSelector).forEach(element => {
			element.classList.toggle("hidden", false);
		});

		if (GW.isMobile() == false)
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
	},

	scrollEvent: (event) => {
		GWLog("ImageFocus.scrollEvent", "image-focus.js", 3);

		event.preventDefault();

		let image = ImageFocus.imageInFocus;

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
		image.style.height = "auto";

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

		//	Different handling for drag-end events than clicks.
		let imageWasBeingDragged = (window.onmousemove != null);

		//	Do this regardless of where the mouse-up is.
		if (   ImageFocus.imageInFocus.height >= window.innerHeight
			|| ImageFocus.imageInFocus.width >= window.innerWidth) {
			window.onmousemove = "";

			//  Put the filter back.
			ImageFocus.imageInFocus.style.filter = ImageFocus.imageInFocus.savedFilter;
		}

		//	Do nothing more if click is on a UI element.
		if (event.target.closest(ImageFocus.imageFocusUIElementsSelector))
			return;

		//  We only want to do anything on left-clicks.
		if (event.button != 0)
			return;

		if (   (   ImageFocus.imageInFocus.height < window.innerHeight
				&& ImageFocus.imageInFocus.width < window.innerWidth)
			|| (   imageWasBeingDragged == false
				&& event.target != ImageFocus.imageInFocus))
			ImageFocus.exitImageFocus();
	},

	imageMouseDown: (event) => {
		GWLog("ImageFocus.imageMouseDown", "image-focus.js", 2);

		//  We only want to do anything on left-clicks.
		if (event.button != 0)
			return;

		//	Prevent browser/system drag-and-drop initiate.
		event.preventDefault();

		if (   ImageFocus.imageInFocus.height >= window.innerHeight
			|| ImageFocus.imageInFocus.width >= window.innerWidth) {
			let mouseCoordX = event.clientX;
			let mouseCoordY = event.clientY;

			let imageCoordX = parseInt(getComputedStyle(ImageFocus.imageInFocus).left);
			let imageCoordY = parseInt(getComputedStyle(ImageFocus.imageInFocus).top);

			//  Save the filter.
			ImageFocus.imageInFocus.savedFilter = ImageFocus.imageInFocus.style.filter;

			window.onmousemove = (event) => {
				//  Remove the filter.
				ImageFocus.imageInFocus.style.filter = "none";
				ImageFocus.imageInFocus.style.left = imageCoordX + event.clientX - mouseCoordX + "px";
				ImageFocus.imageInFocus.style.top = imageCoordY + event.clientY - mouseCoordY + "px";
			};
			return false;
		}
	},

	doubleClick: (event) => {
		GWLog("ImageFocus.doubleClick", "image-focus.js", 2);

		if (   ImageFocus.imageInFocus.height >= window.innerHeight
			|| ImageFocus.imageInFocus.width >= window.innerWidth)
			ImageFocus.resetFocusedImagePosition();
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
			ImageFocus.exitImageFocus();
			break;
		case " ":
		case "Spacebar":
			ImageFocus.resetFocusedImagePosition();
			break;
		case "ArrowDown":
		case "Down":
		case "ArrowRight":
		case "Right":
			if (   ImageFocus.currentlyFocusedImage
				&& ImageFocus.currentlyFocusedImage.classList.contains("gallery-image"))
				ImageFocus.focusNextImage(true);
			break;
		case "ArrowUp":
		case "Up":
		case "ArrowLeft":
		case "Left":
			if (   ImageFocus.currentlyFocusedImage
				&& ImageFocus.currentlyFocusedImage.classList.contains("gallery-image"))
				ImageFocus.focusNextImage(false);
			break;
		}
	},

	mouseMoved: (event) => {
		GWLog("ImageFocus.mouseMoved", "image-focus.js", 3);

		let currentDateTime = new Date();

		if ([ ImageFocus.imageInFocus, 
			  ImageFocus.overlay, 
			  document.documentElement 
			 ].includes(event.target)) {
			if (ImageFocus.hideUITimer == null)
				ImageFocus.unhideImageFocusUI();

			ImageFocus.mouseLastMovedAt = currentDateTime;
		} else {
			ImageFocus.cancelImageFocusHideUITimer();
		}
	}
};

GW.notificationCenter.fireEvent("ImageFocus.didLoad");

ImageFocus.setup();

//	If the URL specifies an image, focus it after the page has loaded.
ImageFocus.focusImageSpecifiedByURL();
