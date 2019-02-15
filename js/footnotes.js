/* popup/floating footnotes to avoid readers needing to scroll to the end of the page to see any footnotes; see http://ignorethecode.net/blog/2010/04/20/footnotes/ for details; also requires jQuery */
var Footnotes = {
    footnotetimeout: false,
    setup: function() {
        var footnotelinks = jQuery('.footnote-ref')

        footnotelinks.unbind('mouseover',Footnotes.footnoteover);
        footnotelinks.unbind('mouseout',Footnotes.footnoteoout);

        footnotelinks.bind('mouseover',Footnotes.footnoteover);
        footnotelinks.bind('mouseout',Footnotes.footnoteoout);
    },
    footnoteover: function() {
        clearTimeout(Footnotes.footnotetimeout);
        jQuery('#footnotediv').stop();
        jQuery('#footnotediv').remove();

        var id = jQuery(this).attr('href').substr(1);
        var position = jQuery(this).offset();

        var div = jQuery(document.createElement('div'));
        div.attr('id','footnotediv');
        div.bind('mouseover',Footnotes.divover);
        div.bind('mouseout',Footnotes.footnoteoout);

        var el = document.getElementById(id);
        div.html('<div>'+jQuery(el).html()+'</div>');

        // WARNING: Original generic code for any HTML page
        // // jQuery(document.body).append(div);
        // This however means that any CSS for sub-body elements, like #markdownBody, does *not* get applied to the floating footnotes when they float, even though they *do* get applied to the footnotes in their original static form as endnotes at the end of the page! This is because they get inserted at the end of 'body', and 'body' is above #markdownBody. This leads to visual inconsistencies. To avoid this, we change the footnotes to get inserted at the end of #markdownBody instead, thereby inheriting all the CSS we've written to customize #markdownBody's appearance. This assumes we have a #markdownBody, of course, which we probably don't on non-Hakyll/Pandoc-generated websites.
        // Specialized to Hakyll-generated websites:
        $("#markdownBody").append(div)

        var left = position.left;
        if(left + 420  > jQuery(window).width() + jQuery(window).scrollLeft())
            left = jQuery(window).width() - 420 + jQuery(window).scrollLeft();
        var top = position.top+20;
        if(top + div.height() > jQuery(window).height() + jQuery(window).scrollTop())
            top = position.top - div.height() - 15;
        div.css({
            left:left,
            top:top,
            opacity:1,
            position: "absolute"
            });
    },
    footnoteoout: function() {
        Footnotes.footnotetimeout = setTimeout(function() {
            jQuery('#footnotediv').animate({
                opacity: 0
            }, 600, function() {
                jQuery('#footnotediv').remove();
            });
        },100);
    },
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
