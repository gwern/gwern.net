//     jquery.sausage.js 1.0.0
//     (c) 2011 Christopher Cliff
//     Freely distributed under the MIT license.
//     For all details and documentation:
//     http://christophercliff.github.com/sausage

(function($, undefined){

    $.widget('cc.sausage', {

        // # Options
        //
        //

        options: {

            // ### page `string`
            //
            // Sets the string to be used to select page elements.
            //
            // Example:
            //
            //      $(window)
            //          .sausage({
            //              page: '.my-page-selector'
            //          })
            //          ;
            //
            page: '.page',

            // ### content `function`
            //
            // Sets the content of the sausage elements. Use `i` and `$page` to render content dynamically.
            //
            // Example:
            //
            //      $(window)
            //          .sausage({
            //              content: function (i, $page) {
            //                  return '<div>' + $page.data('name') + '</div>';
            //          }
            //      })
            //      ;
            //
            content: function (i, $page) {
                return '<span class="sausage-span">' + (i + 1) + '</span>';
            }

        },

        // # Private Methods
        //
        //

        // ## `._create()`
        //
        //
        _create: function () {

            var self = this,
                $el = self.element;

            // Use $el for the outer element.
            self.$outer = $el;
            // Use `body` for the inner element if the outer element is `window`. Otherwise, use the first child of `$el`.
            self.$inner = $.isWindow(self.element.get(0)) ? $('body') : $el.children(':first-child');
            self.$sausages = $('<div class="sausage-set"/>');
            self.sausages = self.$sausages.get(0);
            self.offsets = [];

            self.$sausages
                .appendTo(self.$inner)
                ;

            // Trigger the `create` event.
            self._trigger('create');

            return;
        },

        // ## `._init()`
        //
        //
        _init: function () {

            var self = this;

            // Stop and destroy if scroll bar is not present.
            if (self.$outer.height() >= self.$inner.height())
            {
                self.destroy();

                return;
            }

            self.draw();
            self._update();
            self._events();
            self._delegates();

            // Add a CSS class for styling purposes.
            self.$sausages
                .addClass('sausage-set-init')
                ;

            self.blocked = false;

            // Trigger the `init` event.
            self._trigger('init');

            return;
        },

        // ## `._events()`
        //
        //
        _events: function () {

            var self = this;

            self.hasScrolled = false;

            self.$outer
                .bind('resize.sausage', function(){

                    self.draw();

                })
                .bind('scroll.sausage', function(e){

                    self.hasScrolled = true;

                })
                ;

            // [Prevent crazy amounts of scroll events from being fired](http://ejohn.org/blog/learning-from-twitter/) by setting an interval and listening.
            setInterval(function(){

                if (!self.hasScrolled)
                {
                    return;
                }

                self.hasScrolled = false;
                self._update();

            }, 250);

            return;
        },

        // ## `._getCurrent()`
        //
        //
        _getCurrent: function () {

            var self = this,
                st = self.$outer.scrollTop() + self._getHandleHeight(self.$outer, self.$inner)/4,
                h_win = self.$outer.height(),
                h_doc = self.$inner.height(),
                i = 0;

            for (l = self.offsets.length; i < l; i++)
            {
                if (!self.offsets[i + 1])
                {
                    return i;
                }
                else if (st <= self.offsets[i])
                {
                    return i;
                }
                else if (st > self.offsets[i] && st <= self.offsets[i + 1])
                {
                    return i;
                }
            }

            return i;
        },

        // ## `._delegates()`
        //
        //
        _delegates: function () {

            var self = this;

            self.$sausages
                .delegate('.sausage', 'hover', function(){

                    if (self.blocked)
                    {
                        return;
                    }

                    $(this)
                        .toggleClass('sausage-hover')
                        ;

                })
                .delegate('.sausage', 'click', function(e){
                    e.preventDefault();

                    if (self.blocked)
                    {
                        return;
                    }

                    var $sausage = $(this),
                        val = $sausage.index(),
                        o = self.$inner.find(self.options.page).eq(val).offset().top;

                    self._scrollTo(o);

                    // Trigger the `onClick` event.
                    //
                    // Example:
                    //
                    //      $(window)
                    //          .sausage({
                    //              onClick: function (e, o) {
                    //                  alert('You clicked the sausage at index: ' + o.i);
                    //          }
                    //      })
                    //      ;
                    //
                    self._trigger('onClick', e, {
                        $sausage: $sausage,
                        i: val
                    });

                    if ($sausage.hasClass('current'))
                    {
                        return;
                    }

                    // Trigger the `onUpdate` event.
                    self._trigger('onUpdate', e, {
                        $sausage: $sausage,
                        i: val
                    });
                })
                ;

            return;
        },

        _scrollTo: function (o) {

            var self = this,
                $outer = self.$outer,
                rate = 2/1, // px/ms
                distance = self.offsets[self.current] - o,
                duration = Math.abs(distance/rate);
                // Travel at 2 px per 1 ms but never longer than 1 s.
                duration = (duration < 1000) ? duration : 1000;

            if (self.$outer.get(0) === window)
            {
                $outer = $('body, html, document');
            }

            $outer
                .stop(true)
                .animate({
                    scrollTop: o
                }, duration)
                ;

            return;
        },

        _handleClick: function () {

            var self = this



            return;
        },

        // ## `._update()`
        //
        //
        _update: function () {

            var self = this;
                i = self._getCurrent(),
                c = 'sausage-current';

            if (i === self.current || self.blocked)
            {
                return;
            }

            self.current = i;

            self.$sausages.children().eq(i)
                .addClass(c)
            .siblings()
                .removeClass(c)
                ;

            // Trigger the `update` event.
            self._trigger('update');

            return;
        },

        // ### `._getHandleHeight()`
        //
        //
        _getHandleHeight: function ($outer, $inner) {

            var h_outer = $outer.height(),
                h_inner = $inner.height();

            return h_outer/h_inner*h_outer;
        },

        // # Public Methods
        //
        //

        // ### draw `.sausage("draw")`
        //
        // Creates the sausage UI elements.
        draw: function () {

            var self = this,
                h_win = self.$outer.height(),
                h_doc = self.$inner.height(),
                $items = self.$inner.find(self.options.page),
                $page,
                s = [],
                offset_p,
                offset_s;

            self.offsets = [];
            self.count = $items.length;

            // Detach from DOM while making changes.
            self.$sausages
                .detach()
                .empty()
                ;

            // Calculate the element heights and push to an array.
            for (var i = 0; i < self.count; i++)
            {
                $page = $items.eq(i);
                offset_p = $page.offset();
                offset_s = offset_p.top/h_doc*h_win;

                s.push('<div class="sausage' + ((i === self.current) ? ' sausage-current' : '') + '" style="height:' + ($page.outerHeight()/h_doc*h_win) + 'px;top:' + offset_s + 'px;">' + self.options.content(i, $page) + '</div>');

                // Create `self.offsets` for calculating current sausage.
                self.offsets.push(offset_p.top);
            }

            // Use Array.join() for speed.
            self.sausages.innerHTML = s.join('');

            // And reattach.
            self.$sausages
                .appendTo(self.$inner)
                ;

            return;
        },

        // ### block `.sausage("block")`
        //
        // Blocks the UI to prevent users from interacting with the sausage UI. Useful when loading data and updating the DOM.
        block: function () {

            var self = this,
                c = 'sausage-set-blocked';

            self.blocked = true;

            // Add a CSS class for styling purposes.
            self.$sausages
                .addClass(c)
                ;

            return;
        },

        // ### unblock `.sausage("unblock")`
        //
        // Unblocks the UI once loading and DOM manipulation are complete.
        unblock: function () {

            var self = this,
                c = 'sausage-set-blocked';

            self.$sausages
                .removeClass(c)
                ;

            self.blocked = false;

            return;
        },

        // ### destroy `.sausage("destroy")`
        //
        // Removes the sausage instance from the DOM.
        destroy: function () {

            var self = this;

            self.$outer
                .unbind('.sausage')
                ;

            self.$sausages
                .remove()
                ;

            return;
        }

    });

})(jQuery);
