/*	Popup/floating footnotes to avoid readers needing to scroll to the end of
	the page to see any footnotes; see
	http://ignorethecode.net/blog/2010/04/20/footnotes/ for details;
	also requires jQuery
	*/
var Footnotes = {
	contentContainerSelector: "#markdownBody",
	minFootnoteWidth: 520,
	footnotetimeout: false,
	setup: function() {
		// Get all footnote links.
		var footnotelinks = jQuery('.footnote-ref')

		// Unbind existing mouseover/mouseout events, if any.
		footnotelinks.unbind('mouseover', Footnotes.footnoteover);
		footnotelinks.unbind('mouseout', Footnotes.footnoteoout);

		// Bind mousemover/mouseout events.
		footnotelinks.bind('mouseover', Footnotes.footnoteover);
		footnotelinks.bind('mouseout', Footnotes.footnoteoout);
	},
	// The mouseover event.
	footnoteover: function() {
		// Stop the countdown to un-pop the popup.
		clearTimeout(Footnotes.footnotetimeout);

		jQuery('#footnotediv').stop();
		jQuery('#footnotediv').remove();

		var id = jQuery(this).attr('href').substr(1);
		var position = jQuery(this).offset();

		var div = jQuery(document.createElement('div'));
		div.attr('id','footnotediv');
		div.bind('mouseover', Footnotes.divover);
		div.bind('mouseout', Footnotes.footnoteoout);

		var el = document.getElementById(id);
		div.html('<div>'+jQuery(el).html()+'</div>');

		$(Footnotes.contentContainerSelector).append(div)

		var left = position.left;
		if (left + Footnotes.minFootnoteWidth > jQuery(window).width() + jQuery(window).scrollLeft())
			left = jQuery(window).width() - Footnotes.minFootnoteWidth + jQuery(window).scrollLeft();
		var top = position.top + 20;
		if (top + div.height() > jQuery(window).height() + jQuery(window).scrollTop())
			top = position.top - div.height() - 15;
		if (top < 0) top = 0;
		console.log(top);
		div.css({
			left:left,
			top:top
		});
	},
	// The mouseout event.
	footnoteoout: function() {
		Footnotes.footnotetimeout = setTimeout(function() {
			jQuery('#footnotediv').animate({
				opacity: 0
			}, 600, function() {
				jQuery('#footnotediv').remove();
			});
		}, 100);
	},
	// The "user moved mouse back into popup" mouseover event.
	divover: function() {
		clearTimeout(Footnotes.footnotetimeout);
		jQuery('#footnotediv').stop();
		jQuery('#footnotediv').css({
				opacity: 1
		});
	}
}

jQuery(document).ready(function() {
	Footnotes.setup();
});
