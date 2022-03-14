GW.readerMode = {
	/*****************/
	/*	Configuration.
	 */
	maskedLinksSelector: "p a, li a",
	maskedLinksParentBlockSelector: "p, li",

	unmaskLinksTriggerElementSelector: "#see-also",

	adjustedPopupTriggerDelay: 2400,

	/******************/
	/*	Infrastructure.
	 */
	markdownBody: document.querySelector("#markdownBody"),

	savedPopupTriggerDelay: null,

	previousPageScrollOffset: null,

	/*************/
	/*	Functions.
	 */
	setup: () => {
		GW.readerMode.maskLinks();
	},

	maskLinks: () => {
		GW.readerMode.maskedLinks = GW.readerMode.markdownBody.querySelectorAll(GW.readerMode.maskedLinksSelector);

		GW.readerMode.maskedLinks.forEach(link => {
			link.classList.add("masked-link");

			let parentGraf = link.closest(GW.readerMode.maskedLinksParentBlockSelector);
			if (parentGraf.hideMaskedLinks == null)
				parentGraf.addEventListener("mouseleave", parentGraf.hideMaskedLinks = (event) => {
					GW.readerMode.hideMaskedLinks();
				});

			link.addEventListener("mouseover", link.showMaskedLinks = (event) => {
				let previousViewportRect = parentGraf.getBoundingClientRect();

				GW.readerMode.showMaskedLinks();

				requestAnimationFrame(() => {
					let newViewportRect = parentGraf.getBoundingClientRect();
					let deltaY = Math.round(newViewportRect.y) - Math.round(previousViewportRect.y);
					document.documentElement.scrollTop += deltaY;
				});
			});
		});

		document.querySelector("head").insertAdjacentHTML("beforeend", `<style id='masked-links-styles'>
			.markdownBody.masked-links-hidden a.masked-link,
			.markdownBody.masked-links-hidden a.masked-link:visited,
			.markdownBody.masked-links-hidden a.masked-link:hover {
				color: inherit;
				background: none !important;
			}
			.markdownBody.masked-links-hidden .masked-link {
				padding-left: 0;
			}
			.markdownBody.masked-links-hidden .masked-link::before,
			.markdownBody.masked-links-hidden .masked-link::after {
				display: none;
			}
		</style>`);

		let observer = new IntersectionObserver((entries, observer) => {
			entries.forEach(entry => {
				if (entry.isIntersecting == false)
					return;

				GW.readerMode.unmaskLinks();
				observer.disconnect();
			});
		}, { threshold: 1.0 });
		observer.observe(document.querySelector(GW.readerMode.unmaskLinksTriggerElementSelector));
	},

	unmaskLinks: () => {
		GW.readerMode.showMaskedLinks();

		document.querySelectorAll("#masked-links-styles").forEach(styles => styles.remove());

		GW.readerMode.maskedLinks.forEach(link => {
			link.removeEventListener("mouseover", link.showMaskedLinks);
			link.showMaskedLinks = null;

			let parentGraf = link.closest(GW.readerMode.maskedLinksParentBlockSelector);
			parentGraf.removeEventListener("mouseleave", parentGraf.hideMaskedLinks);
			parentGraf.hideMaskedLinks = null;
		});

		GW.readerMode.previousPageScrollOffset = null;
	},

	maskedLinksVisible: () => {
		return (GW.readerMode.markdownBody.classList.contains("masked-links-hidden") == false);
	},

	hideMaskedLinks: () => {
		if (GW.readerMode.maskedLinksVisible() == false)
			return;

		GW.readerMode.markdownBody.classList.add("masked-links-hidden");

		if (GW.readerMode.previousPageScrollOffset != null) {
			document.documentElement.scrollTop = GW.readerMode.previousPageScrollOffset;
			GW.readerMode.previousPageScrollOffset = null;
		}

		if (   window.Extracts 
			&& window.Popups 
			&& Extracts.popFrameProvider == Popups) {
			GW.readerMode.savedPopupTriggerDelay = Popups.popupTriggerDelay;
			Popups.popupTriggerDelay = GW.readerMode.adjustedPopupTriggerDelay;
		}
	},

	showMaskedLinks: () => {
		if (GW.readerMode.maskedLinksVisible() == true)
			return;

		GW.readerMode.markdownBody.classList.remove("masked-links-hidden");

		GW.readerMode.previousPageScrollOffset = document.documentElement.scrollTop;

		if (   window.Extracts 
			&& window.Popups 
			&& Extracts.popFrameProvider == Popups
			&& GW.readerMode.savedPopupTriggerDelay != null) {
			requestAnimationFrame(() => {
				Popups.popupTriggerDelay = GW.readerMode.savedPopupTriggerDelay;
				GW.readerMode.savedPopupTriggerDelay = null;
			});
		}
	},
};

GW.readerMode.setup();
GW.readerMode.hideMaskedLinks();
