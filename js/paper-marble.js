// paper-marble.js: ink-marble the current web page (an idle 'screensaver' toy).
// Author: Gwern Branwen, Claude-5-Fable, GPT-5.6 Sol
// Date: 2026-07-07
// When:  Time-stamp: "2026-08-08 14:55:37 gwern"
// License: CC-0 (except the vendored html-to-image library: MIT, notice below)
//
// Turns the current viewport into a sheet of paper being marbled (<https://en.wikipedia.org/wiki/Paper_marbling>).
// Associated with fine typography and old books.
// Examples: <https://gwern.net/doc/cs/js/2026-07-08-gwern-gwernnet-papermarbling-homepagescreenshots-4x2.png>
//
// The page is rasterized into an "ink bath"; its pixels—and pigment drops raining down
// in palettes extracted from scans of historical marbled papers (eg. French curls
// of 1735 & 1880, an English spot paper of 1830, a Victorian spot paper, a
// gilt binding of 1902, and suminagashi)—are advected through a
// divergence-free flow, swept into chevrons by animated tine combs, and raked
// by the mouse.
// The inks are immiscible: a COORDINATE MAP is advected instead of colors, so
// the simulation never blends or averages (display anti-aliasing and the brief
// new-sheet crossfade are the only compositing); boundaries stay sharp, the
// pixel census is (approximately) conserved, and only page + palette colors
// are ever displayed. Palettes rotate every minute as a hard whole-bath
// recolor; every 10 minutes the print is pulled and a fresh sheet begins.
//
// USE: 4 ways (the capture library is vendored, so no library download is
// required; snapshotting may still re-fetch resources referenced by the page):
//  (1) Run now: paste this entire file into the DevTools console of any page,
//      or include it as a plain `<script src=...>`: it marbles on load.
//  (2) PRODUCTION idle screensaver (the Gwern.net deployment): do NOT
//      reference this file from any HTML. The tiny idle loader at the end of
//      `js/misc.js` is included in the site's existing JS bundle;
//      after 24 idle hours it injects
//      `<script src="/static/js/paper-marble.js" data-marble-idle-start>`,
//      which cancels startup if the reader returns during asynchronous capture.
//      Regular readers never download or parse this file.
//  (3) Single-file idle screensaver, for pages that do not mind the weight:
//          <script src="/static/js/paper-marble.js" data-screensaver async></script>
//      arms a 24-hour no-activity timer instead of running (visible top-level
//      tabs only; honors `prefers-reduced-motion`, re-checked at trigger
//      time). NB: `async` only unblocks parsing—this still DOWNLOADS and
//      parses the whole file on every page view, so prefer (b) for any
//      production site. See the DISPATCHER comment at the end of this file.
//  (4) Run anywhere in DevTools console: `document.head.appendChild(Object.assign(document.createElement('script'), {src: 'https://gwern.net/static/js/paper-marble.js'}));`;
//      then `paperMarble.start()` to reset.
//
// CONTROLS: [move/drag mouse] = rake; [right-click] (touch: [long-press]) = drop pigment;
// [left-click] (touch: [tap]) / [ESC] / `window.paperMarble.stop()` = stop & restore.
//
// REPRODUCIBILITY: all stochastic choices flow from one seeded PRNG
// (mulberry32; `CONFIG.seed`, logged at startup). Structured diagnostic events
// are retained on `paperMarble.active.events`. The seed reproduces stochastic
// choices, but wall-clock scheduling, pointer trajectories, viewport state,
// and the captured page make this a trace rather than an exact replay format.
//
// Tunables are in the CONFIG object at the top of the widget function.
//
// References:
// - Lu, Jaffer, Jin, Mao & Tan 2012, "Mathematical Marbling", IEEE Computer
//   Graphics & Applications 32(6)—the closed-form drop and tine-line
//   operators used here. <https://ieeexplore.ieee.org/document/6103983>;
//   see also Aubrey Jaffer's site <https://people.csail.mit.edu/jaffer/Marbling/>.
// - Semi-Lagrangian backward advection of a coordinate map, rendered from a
//   pristine source atlas, is standard fluid-simulation practice; the
//   adaptive-commit / per-frame-display-warp split and the seam-aware
//   toroidal map sampling are documented in the design notes below.
// - html-to-image (W.Y., MIT), vendored below, for page rasterization via
//   SVG `<foreignObject>`: <https://github.com/bubkoo/html-to-image>.
//
//
// Prior art (or: why this had to be written): every ingredient existed
// separately; the combination did not.
// - Marbling simulators on blank canvases are well-trodden in JS: Shiffman's
//   Coding Train challenge #183 (2024)
//   <https://thecodingtrain.com/challenges/183-mathematical-marbling/> and
//   Walker's "Marblizer" (2016)
//   <https://nickwalker.us/assets/projects/marblizer/marblizer-report.pdf>
//   both implement the same Lu-Jaffer drop & tine operators—but as VECTOR
//   graphics: a drop is a polygon whose vertices the closed-form transforms
//   displace. A web page is a raster; it has no polygons to displace, so
//   those architectures cannot consume it. Hence the raster coordinate-map
//   engine here, which transports arbitrary pixels instead of shapes.
// - The closest technical relative is Ghassaei's WebGL "Digital Marbling"
//   (2022) <https://blog.amandaghassaei.com/2022/10/25/digital-marbling/>
//   (the Nervous System marbling puzzles), which *is* raster-based and
//   independently hit the same wall we did: semi-Lagrangian color
//   advection (Stam's Stable Fluids) blurs immiscible inks into soup. She
//   solved crisp boundaries with a GPU bidirectional-mapping fluid solver
//   (BiMocq2: Qu et al 2019,
//   <https://www.seas.upenn.edu/~ziyinq/static/files/bimocq.pdf>); this file
//   solves it with a CPU backward coordinate map, seam-aware sampling, and
//   adaptive commits—cousins in the same mapping-method family. But hers
//   is an art tool: a blank bath, full Navier-Stokes, no page input, no
//   deployment story.
// - Page-as-raster effects (the screenshot-then-distort tradition:
//   liquid-glass demos, displacement-map hover toys, destruction
//   bookmarklets) apply static warps, lenses, or physics gags to the
//   captured page; none run a conservative fluid transport on it, so
//   content smears or shatters rather than marbling.
// - Web "screensaver" idle libraries (`scsaver.js`, giuseppeg/screensaver,
//   idlejs) overlay generic content after a timeout; none consume the page
//   they interrupt.
//
// What appears to be new is the intersection: the page itself as the ink,
// kept immiscible under transport; palettes lifted from scans of historical
// marbled papers, swapped as hard whole-bath recolors; a sheet lifecycle
// (drop → comb → filament → pull the print); and an idle-screensaver
// deployment contract that costs ordinary readers nothing.
//
// Structure of this file: (1) vendored html-to-image v1.11.11 (minified UMD,
// captured into a private lexical binding without touching host globals);
// (2) the marbling widget and `window.paperMarble` API; (3) a dispatcher that
// either runs immediately or arms the idle-screensaver timer.
//
// BUGS:
// - may not work correctly on Safari web browsers?

(function () {
    'use strict';
    const MARBLE_RUNTIME_KEY = Symbol.for('gwern.paper-marble.runtime');
    const MARBLE_IDLE_TICKET_KEY = Symbol.for('gwern.paper-marble.idle-ticket');
    const MARBLE_LOADING_SCRIPT = document.currentScript;
    const existingRuntime = globalThis[MARBLE_RUNTIME_KEY];
    if (existingRuntime && typeof existingRuntime.dispatch === 'function') {
        existingRuntime.dispatch(MARBLE_LOADING_SCRIPT);
        return;
    }

/* ==========================================================================
 * VENDORED LIBRARY: html-to-image v1.11.11
 * <https://github.com/bubkoo/html-to-image>
 *
 * MIT License
 *
 * Copyright (c) 2017-2023 W.Y.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 * ========================================================================== */
const MARBLE_HTML_TO_IMAGE = (function () {
    // Shadow page module systems and redirect the UMD global branch into a
    // private object. No `htmlToImage` property is created or replaced on the
    // host page, including pages with non-configurable globals.
    var define, exports, module;
    var marbleVendorRoot = {};
!function(t,e){"object"==typeof exports&&"undefined"!=typeof module?e(exports):"function"==typeof define&&define.amd?define(["exports"],e):e((t=marbleVendorRoot).htmlToImage={})}(this,(function(t){"use strict";function e(t,e,n,r){return new(n||(n=Promise))((function(i,o){function u(t){try{a(r.next(t))}catch(t){o(t)}}function c(t){try{a(r.throw(t))}catch(t){o(t)}}function a(t){var e;t.done?i(t.value):(e=t.value,e instanceof n?e:new n((function(t){t(e)}))).then(u,c)}a((r=r.apply(t,e||[])).next())}))}function n(t,e){var n,r,i,o,u={label:0,sent:function(){if(1&i[0])throw i[1];return i[1]},trys:[],ops:[]};return o={next:c(0),throw:c(1),return:c(2)},"function"==typeof Symbol&&(o[Symbol.iterator]=function(){return this}),o;function c(c){return function(a){return function(c){if(n)throw new TypeError("Generator is already executing.");for(;o&&(o=0,c[0]&&(u=0)),u;)try{if(n=1,r&&(i=2&c[0]?r.return:c[0]?r.throw||((i=r.return)&&i.call(r),0):r.next)&&!(i=i.call(r,c[1])).done)return i;switch(r=0,i&&(c=[2&c[0],i.value]),c[0]){case 0:case 1:i=c;break;case 4:return u.label++,{value:c[1],done:!1};case 5:u.label++,r=c[1],c=[0];continue;case 7:c=u.ops.pop(),u.trys.pop();continue;default:if(!(i=u.trys,(i=i.length>0&&i[i.length-1])||6!==c[0]&&2!==c[0])){u=0;continue}if(3===c[0]&&(!i||c[1]>i[0]&&c[1]<i[3])){u.label=c[1];break}if(6===c[0]&&u.label<i[1]){u.label=i[1],i=c;break}if(i&&u.label<i[2]){u.label=i[2],u.ops.push(c);break}i[2]&&u.ops.pop(),u.trys.pop();continue}c=e.call(t,u)}catch(t){c=[6,t],r=0}finally{n=i=0}if(5&c[0])throw c[1];return{value:c[0]?c[1]:void 0,done:!0}}([c,a])}}}var r,i=(r=0,function(){return r+=1,"u".concat("0000".concat((Math.random()*Math.pow(36,4)<<0).toString(36)).slice(-4)).concat(r)});function o(t){for(var e=[],n=0,r=t.length;n<r;n++)e.push(t[n]);return e}function u(t,e){var n=(t.ownerDocument.defaultView||window).getComputedStyle(t).getPropertyValue(e);return n?parseFloat(n.replace("px","")):0}function c(t,e){void 0===e&&(e={});var n,r,i,o=e.width||(r=u(n=t,"border-left-width"),i=u(n,"border-right-width"),n.clientWidth+r+i),c=e.height||function(t){var e=u(t,"border-top-width"),n=u(t,"border-bottom-width");return t.clientHeight+e+n}(t);return{width:o,height:c}}var a=16384;function s(t,e){return void 0===e&&(e={}),t.toBlob?new Promise((function(n){t.toBlob(n,e.type?e.type:"image/png",e.quality?e.quality:1)})):new Promise((function(n){for(var r=window.atob(t.toDataURL(e.type?e.type:void 0,e.quality?e.quality:void 0).split(",")[1]),i=r.length,o=new Uint8Array(i),u=0;u<i;u+=1)o[u]=r.charCodeAt(u);n(new Blob([o],{type:e.type?e.type:"image/png"}))}))}function l(t){return new Promise((function(e,n){var r=new Image;r.decode=function(){return e(r)},r.onload=function(){return e(r)},r.onerror=n,r.crossOrigin="anonymous",r.decoding="async",r.src=t}))}function f(t){return e(this,void 0,void 0,(function(){return n(this,(function(e){return[2,Promise.resolve().then((function(){return(new XMLSerializer).serializeToString(t)})).then(encodeURIComponent).then((function(t){return"data:image/svg+xml;charset=utf-8,".concat(t)}))]}))}))}function h(t,r,i){return e(this,void 0,void 0,(function(){var e,o,u;return n(this,(function(n){return e="http://www.w3.org/2000/svg",o=document.createElementNS(e,"svg"),u=document.createElementNS(e,"foreignObject"),o.setAttribute("width","".concat(r)),o.setAttribute("height","".concat(i)),o.setAttribute("viewBox","0 0 ".concat(r," ").concat(i)),u.setAttribute("width","100%"),u.setAttribute("height","100%"),u.setAttribute("x","0"),u.setAttribute("y","0"),u.setAttribute("externalResourcesRequired","true"),o.appendChild(u),u.appendChild(t),[2,f(o)]}))}))}var d=function(t,e){if(t instanceof e)return!0;var n=Object.getPrototypeOf(t);return null!==n&&(n.constructor.name===e.name||d(n,e))};function v(t,e,n){var r=".".concat(t,":").concat(e),i=n.cssText?function(t){var e=t.getPropertyValue("content");return"".concat(t.cssText," content: '").concat(e.replace(/'|"/g,""),"';")}(n):function(t){return o(t).map((function(e){var n=t.getPropertyValue(e),r=t.getPropertyPriority(e);return"".concat(e,": ").concat(n).concat(r?" !important":"",";")})).join(" ")}(n);return document.createTextNode("".concat(r,"{").concat(i,"}"))}function p(t,e,n){var r=window.getComputedStyle(t,n),o=r.getPropertyValue("content");if(""!==o&&"none"!==o){var u=i();try{e.className="".concat(e.className," ").concat(u)}catch(t){return}var c=document.createElement("style");c.appendChild(v(u,n,r)),e.appendChild(c)}}var g="application/font-woff",m="image/jpeg",w={woff:g,woff2:g,ttf:"application/font-truetype",eot:"application/vnd.ms-fontobject",png:"image/png",jpg:m,jpeg:m,gif:"image/gif",tiff:"image/tiff",svg:"image/svg+xml",webp:"image/webp"};function b(t){var e=function(t){var e=/\.([^./]*?)$/g.exec(t);return e?e[1]:""}(t).toLowerCase();return w[e]||""}function y(t){return-1!==t.search(/^(data:)/)}function x(t,e){return"data:".concat(e,";base64,").concat(t)}function S(t,r,i){return e(this,void 0,void 0,(function(){var e,o;return n(this,(function(n){switch(n.label){case 0:return[4,fetch(t,r)];case 1:if(404===(e=n.sent()).status)throw new Error('Resource "'.concat(e.url,'" not found'));return[4,e.blob()];case 2:return o=n.sent(),[2,new Promise((function(t,n){var r=new FileReader;r.onerror=n,r.onloadend=function(){try{t(i({res:e,result:r.result}))}catch(t){n(t)}},r.readAsDataURL(o)}))]}}))}))}var E={};function C(t,r,i){return e(this,void 0,void 0,(function(){var e,o,u,c,a;return n(this,(function(n){switch(n.label){case 0:if(e=function(t,e,n){var r=t.replace(/\?.*/,"");return n&&(r=t),/ttf|otf|eot|woff2?/i.test(r)&&(r=r.replace(/.*\//,"")),e?"[".concat(e,"]").concat(r):r}(t,r,i.includeQueryParams),null!=E[e])return[2,E[e]];i.cacheBust&&(t+=(/\?/.test(t)?"&":"?")+(new Date).getTime()),n.label=1;case 1:return n.trys.push([1,3,,4]),[4,S(t,i.fetchRequestInit,(function(t){var e=t.res,n=t.result;return r||(r=e.headers.get("Content-Type")||""),function(t){return t.split(/,/)[1]}(n)}))];case 2:return u=n.sent(),o=x(u,r),[3,4];case 3:return c=n.sent(),o=i.imagePlaceholder||"",a="Failed to fetch resource: ".concat(t),c&&(a="string"==typeof c?c:c.message),a&&console.warn(a),[3,4];case 4:return E[e]=o,[2,o]}}))}))}function P(t){return e(this,void 0,void 0,(function(){var e;return n(this,(function(n){return"data:,"===(e=t.toDataURL())?[2,t.cloneNode(!1)]:[2,l(e)]}))}))}function R(t,r){return e(this,void 0,void 0,(function(){var e,i,o,u;return n(this,(function(n){switch(n.label){case 0:return t.currentSrc?(e=document.createElement("canvas"),i=e.getContext("2d"),e.width=t.clientWidth,e.height=t.clientHeight,null==i||i.drawImage(t,0,0,e.width,e.height),[2,l(e.toDataURL())]):(o=t.poster,u=b(o),[4,C(o,u,r)]);case 1:return[2,l(n.sent())]}}))}))}function T(t){var r;return e(this,void 0,void 0,(function(){return n(this,(function(e){switch(e.label){case 0:return e.trys.push([0,3,,4]),(null===(r=null==t?void 0:t.contentDocument)||void 0===r?void 0:r.body)?[4,L(t.contentDocument.body,{},!0)]:[3,2];case 1:return[2,e.sent()];case 2:return[3,4];case 3:return e.sent(),[3,4];case 4:return[2,t.cloneNode(!1)]}}))}))}function A(t,e){return d(e,Element)&&(function(t,e){var n=e.style;if(n){var r=window.getComputedStyle(t);r.cssText?(n.cssText=r.cssText,n.transformOrigin=r.transformOrigin):o(r).forEach((function(i){var o=r.getPropertyValue(i);if("font-size"===i&&o.endsWith("px")){var u=Math.floor(parseFloat(o.substring(0,o.length-2)))-.1;o="".concat(u,"px")}d(t,HTMLIFrameElement)&&"display"===i&&"inline"===o&&(o="block"),"d"===i&&e.getAttribute("d")&&(o="path(".concat(e.getAttribute("d"),")")),n.setProperty(i,o,r.getPropertyPriority(i))}))}}(t,e),function(t,e){p(t,e,":before"),p(t,e,":after")}(t,e),function(t,e){d(t,HTMLTextAreaElement)&&(e.innerHTML=t.value),d(t,HTMLInputElement)&&e.setAttribute("value",t.value)}(t,e),function(t,e){if(d(t,HTMLSelectElement)){var n=e,r=Array.from(n.children).find((function(e){return t.value===e.getAttribute("value")}));r&&r.setAttribute("selected","")}}(t,e)),e}function L(t,r,i){return e(this,void 0,void 0,(function(){return n(this,(function(u){return i||!r.filter||r.filter(t)?[2,Promise.resolve(t).then((function(t){return function(t,r){return e(this,void 0,void 0,(function(){return n(this,(function(e){return d(t,HTMLCanvasElement)?[2,P(t)]:d(t,HTMLVideoElement)?[2,R(t,r)]:d(t,HTMLIFrameElement)?[2,T(t)]:[2,t.cloneNode(!1)]}))}))}(t,r)})).then((function(i){return function(t,r,i){var u,c;return e(this,void 0,void 0,(function(){var e;return n(this,(function(n){switch(n.label){case 0:return e=[],0===(e=null!=(a=t).tagName&&"SLOT"===a.tagName.toUpperCase()&&t.assignedNodes?o(t.assignedNodes()):d(t,HTMLIFrameElement)&&(null===(u=t.contentDocument)||void 0===u?void 0:u.body)?o(t.contentDocument.body.childNodes):o((null!==(c=t.shadowRoot)&&void 0!==c?c:t).childNodes)).length||d(t,HTMLVideoElement)?[2,r]:[4,e.reduce((function(t,e){return t.then((function(){return L(e,i)})).then((function(t){t&&r.appendChild(t)}))}),Promise.resolve())];case 1:return n.sent(),[2,r]}var a}))}))}(t,i,r)})).then((function(e){return A(t,e)})).then((function(t){return function(t,r){return e(this,void 0,void 0,(function(){var e,i,o,u,c,a,s,l,f,h,d,v,p;return n(this,(function(n){switch(n.label){case 0:if(0===(e=t.querySelectorAll?t.querySelectorAll("use"):[]).length)return[2,t];i={},p=0,n.label=1;case 1:return p<e.length?(o=e[p],(u=o.getAttribute("xlink:href"))?(c=t.querySelector(u),a=document.querySelector(u),c||!a||i[u]?[3,3]:(s=i,l=u,[4,L(a,r,!0)])):[3,3]):[3,4];case 2:s[l]=n.sent(),n.label=3;case 3:return p++,[3,1];case 4:if((f=Object.values(i)).length){for(h="http://www.w3.org/1999/xhtml",(d=document.createElementNS(h,"svg")).setAttribute("xmlns",h),d.style.position="absolute",d.style.width="0",d.style.height="0",d.style.overflow="hidden",d.style.display="none",v=document.createElementNS(h,"defs"),d.appendChild(v),p=0;p<f.length;p++)v.appendChild(f[p]);t.appendChild(d)}return[2,t]}}))}))}(t,r)}))]:[2,null]}))}))}var N=/url\((['"]?)([^'"]+?)\1\)/g,k=/url\([^)]+\)\s*format\((["']?)([^"']+)\1\)/g,I=/src:\s*(?:url\([^)]+\)\s*format\([^)]+\)[,;]\s*)+/g;function D(t,r,i,o,u){return e(this,void 0,void 0,(function(){var e,c,a,s;return n(this,(function(n){switch(n.label){case 0:return n.trys.push([0,5,,6]),e=i?function(t,e){if(t.match(/^[a-z]+:\/\//i))return t;if(t.match(/^\/\//))return window.location.protocol+t;if(t.match(/^[a-z]+:/i))return t;var n=document.implementation.createHTMLDocument(),r=n.createElement("base"),i=n.createElement("a");return n.head.appendChild(r),n.body.appendChild(i),e&&(r.href=e),i.href=t,i.href}(r,i):r,c=b(r),a=void 0,u?[4,u(e)]:[3,2];case 1:return s=n.sent(),a=x(s,c),[3,4];case 2:return[4,C(e,c,o)];case 3:a=n.sent(),n.label=4;case 4:return[2,t.replace((l=r,f=l.replace(/([.*+?^${}()|\[\]\/\\])/g,"\\$1"),new RegExp("(url\\(['\"]?)(".concat(f,")(['\"]?\\))"),"g")),"$1".concat(a,"$3"))];case 5:return n.sent(),[3,6];case 6:return[2,t]}var l,f}))}))}function M(t){return-1!==t.search(N)}function H(t,r,i){return e(this,void 0,void 0,(function(){var e,o;return n(this,(function(n){return M(t)?(e=function(t,e){var n=e.preferredFontFormat;return n?t.replace(I,(function(t){for(;;){var e=k.exec(t)||[],r=e[0],i=e[2];if(!i)return"";if(i===n)return"src: ".concat(r,";")}})):t}(t,i),o=function(t){var e=[];return t.replace(N,(function(t,n,r){return e.push(r),t})),e.filter((function(t){return!y(t)}))}(e),[2,o.reduce((function(t,e){return t.then((function(t){return D(t,e,r,i)}))}),Promise.resolve(e))]):[2,t]}))}))}function V(t,r,i){var o;return e(this,void 0,void 0,(function(){var e,u;return n(this,(function(n){switch(n.label){case 0:return(e=null===(o=r.style)||void 0===o?void 0:o.getPropertyValue(t))?[4,H(e,null,i)]:[3,2];case 1:return u=n.sent(),r.style.setProperty(t,u,r.style.getPropertyPriority(t)),[2,!0];case 2:return[2,!1]}}))}))}function F(t,r){return e(this,void 0,void 0,(function(){return n(this,(function(e){switch(e.label){case 0:return[4,V("background",t,r)];case 1:return e.sent()?[3,3]:[4,V("background-image",t,r)];case 2:e.sent(),e.label=3;case 3:return[4,V("mask",t,r)];case 4:return e.sent()?[3,6]:[4,V("mask-image",t,r)];case 5:e.sent(),e.label=6;case 6:return[2]}}))}))}function j(t,r){return e(this,void 0,void 0,(function(){var e,i,o;return n(this,(function(n){switch(n.label){case 0:return(e=d(t,HTMLImageElement))&&!y(t.src)||d(t,SVGImageElement)&&!y(t.href.baseVal)?[4,C(i=e?t.src:t.href.baseVal,b(i),r)]:[2];case 1:return o=n.sent(),[4,new Promise((function(n,r){t.onload=n,t.onerror=r;var i=t;i.decode&&(i.decode=n),"lazy"===i.loading&&(i.loading="eager"),e?(t.srcset="",t.src=o):t.href.baseVal=o}))];case 2:return n.sent(),[2]}}))}))}function q(t,r){return e(this,void 0,void 0,(function(){var e,i;return n(this,(function(n){switch(n.label){case 0:return e=o(t.childNodes),i=e.map((function(t){return U(t,r)})),[4,Promise.all(i).then((function(){return t}))];case 1:return n.sent(),[2]}}))}))}function U(t,r){return e(this,void 0,void 0,(function(){return n(this,(function(e){switch(e.label){case 0:return d(t,Element)?[4,F(t,r)]:[3,4];case 1:return e.sent(),[4,j(t,r)];case 2:return e.sent(),[4,q(t,r)];case 3:e.sent(),e.label=4;case 4:return[2]}}))}))}var O={};function B(t){return e(this,void 0,void 0,(function(){var e,r;return n(this,(function(n){switch(n.label){case 0:return null!=(e=O[t])?[2,e]:[4,fetch(t)];case 1:return[4,n.sent().text()];case 2:return r=n.sent(),e={url:t,cssText:r},O[t]=e,[2,e]}}))}))}function z(t,r){return e(this,void 0,void 0,(function(){var i,o,u,c,a=this;return n(this,(function(s){return i=t.cssText,o=/url\(["']?([^"')]+)["']?\)/g,u=i.match(/url\([^)]+\)/g)||[],c=u.map((function(u){return e(a,void 0,void 0,(function(){var e;return n(this,(function(n){return(e=u.replace(o,"$1")).startsWith("https://")||(e=new URL(e,t.url).href),[2,S(e,r.fetchRequestInit,(function(t){var e=t.result;return i=i.replace(u,"url(".concat(e,")")),[u,e]}))]}))}))})),[2,Promise.all(c).then((function(){return i}))]}))}))}function W(t){if(null==t)return[];for(var e=[],n=t.replace(/(\/\*[\s\S]*?\*\/)/gi,""),r=new RegExp("((@.*?keyframes [\\s\\S]*?){([\\s\\S]*?}\\s*?)})","gi");;){if(null===(u=r.exec(n)))break;e.push(u[0])}n=n.replace(r,"");for(var i=/@import[\s\S]*?url\([^)]*\)[\s\S]*?;/gi,o=new RegExp("((\\s*?(?:\\/\\*[\\s\\S]*?\\*\\/)?\\s*?@media[\\s\\S]*?){([\\s\\S]*?)}\\s*?})|(([\\s\\S]*?){([\\s\\S]*?)})","gi");;){var u;if(null===(u=i.exec(n))){if(null===(u=o.exec(n)))break;i.lastIndex=o.lastIndex}else o.lastIndex=i.lastIndex;e.push(u[0])}return e}function $(t,r){return e(this,void 0,void 0,(function(){var e,i;return n(this,(function(n){return e=[],i=[],t.forEach((function(e){if("cssRules"in e)try{o(e.cssRules||[]).forEach((function(t,n){if(t.type===CSSRule.IMPORT_RULE){var o=n+1,u=B(t.href).then((function(t){return z(t,r)})).then((function(t){return W(t).forEach((function(t){try{e.insertRule(t,t.startsWith("@import")?o+=1:e.cssRules.length)}catch(e){console.error("Error inserting rule from remote css",{rule:t,error:e})}}))})).catch((function(t){console.error("Error loading remote css",t.toString())}));i.push(u)}}))}catch(o){var n=t.find((function(t){return null==t.href}))||document.styleSheets[0];null!=e.href&&i.push(B(e.href).then((function(t){return z(t,r)})).then((function(t){return W(t).forEach((function(t){n.insertRule(t,e.cssRules.length)}))})).catch((function(t){console.error("Error loading remote stylesheet",t)}))),console.error("Error inlining remote css file",o)}})),[2,Promise.all(i).then((function(){return t.forEach((function(t){if("cssRules"in t)try{o(t.cssRules||[]).forEach((function(t){e.push(t)}))}catch(e){console.error("Error while reading CSS rules from ".concat(t.href),e)}})),e}))]}))}))}function _(t){return t.filter((function(t){return t.type===CSSRule.FONT_FACE_RULE})).filter((function(t){return M(t.style.getPropertyValue("src"))}))}function G(t,r){return e(this,void 0,void 0,(function(){return n(this,(function(e){switch(e.label){case 0:if(null==t.ownerDocument)throw new Error("Provided element is not within a Document");return[4,$(o(t.ownerDocument.styleSheets),r)];case 1:return[2,_(e.sent())]}}))}))}function J(t,r){return e(this,void 0,void 0,(function(){var e;return n(this,(function(n){switch(n.label){case 0:return[4,G(t,r)];case 1:return e=n.sent(),[4,Promise.all(e.map((function(t){var e=t.parentStyleSheet?t.parentStyleSheet.href:null;return H(t.cssText,e,r)})))];case 2:return[2,n.sent().join("\n")]}}))}))}function Q(t,r){return e(this,void 0,void 0,(function(){var e,i,o,u,c;return n(this,(function(n){switch(n.label){case 0:return null==r.fontEmbedCSS?[3,1]:(i=r.fontEmbedCSS,[3,5]);case 1:return r.skipFonts?(o=null,[3,4]):[3,2];case 2:return[4,J(t,r)];case 3:o=n.sent(),n.label=4;case 4:i=o,n.label=5;case 5:return(e=i)&&(u=document.createElement("style"),c=document.createTextNode(e),u.appendChild(c),t.firstChild?t.insertBefore(u,t.firstChild):t.appendChild(u)),[2]}}))}))}function X(t,r){return void 0===r&&(r={}),e(this,void 0,void 0,(function(){var e,i,o,u;return n(this,(function(n){switch(n.label){case 0:return e=c(t,r),i=e.width,o=e.height,[4,L(t,r,!0)];case 1:return[4,Q(u=n.sent(),r)];case 2:return n.sent(),[4,U(u,r)];case 3:return n.sent(),function(t,e){var n=t.style;e.backgroundColor&&(n.backgroundColor=e.backgroundColor),e.width&&(n.width="".concat(e.width,"px")),e.height&&(n.height="".concat(e.height,"px"));var r=e.style;null!=r&&Object.keys(r).forEach((function(t){n[t]=r[t]}))}(u,r),[4,h(u,i,o)];case 4:return[2,n.sent()]}}))}))}function K(t,r){return void 0===r&&(r={}),e(this,void 0,void 0,(function(){var e,i,o,u,s,f,h,d,v;return n(this,(function(n){switch(n.label){case 0:return e=c(t,r),i=e.width,o=e.height,[4,X(t,r)];case 1:return[4,l(n.sent())];case 2:return u=n.sent(),s=document.createElement("canvas"),f=s.getContext("2d"),h=r.pixelRatio||function(){var t,e;try{e=process}catch(t){}var n=e&&e.env?e.env.devicePixelRatio:null;return n&&(t=parseInt(n,10),Number.isNaN(t)&&(t=1)),t||window.devicePixelRatio||1}(),d=r.canvasWidth||i,v=r.canvasHeight||o,s.width=d*h,s.height=v*h,r.skipAutoScale||function(t){(t.width>a||t.height>a)&&(t.width>a&&t.height>a?t.width>t.height?(t.height*=a/t.width,t.width=a):(t.width*=a/t.height,t.height=a):t.width>a?(t.height*=a/t.width,t.width=a):(t.width*=a/t.height,t.height=a))}(s),s.style.width="".concat(d),s.style.height="".concat(v),r.backgroundColor&&(f.fillStyle=r.backgroundColor,f.fillRect(0,0,s.width,s.height)),f.drawImage(u,0,0,s.width,s.height),[2,s]}}))}))}t.getFontEmbedCSS=function(t,r){return void 0===r&&(r={}),e(this,void 0,void 0,(function(){return n(this,(function(e){return[2,J(t,r)]}))}))},t.toBlob=function(t,r){return void 0===r&&(r={}),e(this,void 0,void 0,(function(){return n(this,(function(e){switch(e.label){case 0:return[4,K(t,r)];case 1:return[4,s(e.sent())];case 2:return[2,e.sent()]}}))}))},t.toCanvas=K,t.toJpeg=function(t,r){return void 0===r&&(r={}),e(this,void 0,void 0,(function(){return n(this,(function(e){switch(e.label){case 0:return[4,K(t,r)];case 1:return[2,e.sent().toDataURL("image/jpeg",r.quality||1)]}}))}))},t.toPixelData=function(t,r){return void 0===r&&(r={}),e(this,void 0,void 0,(function(){var e,i,o,u;return n(this,(function(n){switch(n.label){case 0:return e=c(t,r),i=e.width,o=e.height,[4,K(t,r)];case 1:return u=n.sent(),[2,u.getContext("2d").getImageData(0,0,i,o).data]}}))}))},t.toPng=function(t,r){return void 0===r&&(r={}),e(this,void 0,void 0,(function(){return n(this,(function(e){switch(e.label){case 0:return[4,K(t,r)];case 1:return[2,e.sent().toDataURL()]}}))}))},t.toSvg=X}));
    return marbleVendorRoot.htmlToImage;
}).call(globalThis);

/* ==========================================================================
 * THE MARBLING WIDGET, v11
 * ========================================================================== */
/* DESIGN NOTES & CHANGELOG =================================================
 *
 * Core representation
 * -------------------
 * Colors are never advected directly. Each simulation pixel stores a source
 * coordinate in `mapX`/`mapY`; rendering follows that coordinate into a
 * pristine source atlas containing the captured page plus one solid-color
 * band per pigment slot. This keeps transport from averaging unlike inks.
 * Page edges may be antialiased at final display lookup, and the outgoing
 * sheet is composited during the short reset crossfade, but neither operation
 * feeds blended colors back into the simulation.
 *
 * Page coordinates are stored unwrapped. Winding around a torus cannot be
 * represented by one globally continuous wrapped coordinate field; retaining
 * whole-period offsets makes the winding seam numerically harmless. The map
 * sampler unwraps neighboring page coordinates before interpolation and never
 * interpolates across a page/pigment or pigment-slot discontinuity. Pigment
 * coordinates live in distant atlas bands above `SWATCH_BASE`, so their slot
 * identity survives transport and a palette change can recolor old drops by
 * repainting the bands instead of touching the map.
 *
 * Operators and periodicity
 * -------------------------
 * Ambient motion is semi-Lagrangian backward advection through a velocity
 * field built from a rotating uniform drift plus curl waves. Each wave uses an
 * integer torus harmonic, so opposite bath edges agree exactly. The mouse rake
 * uses nearest-image distances, so its wake also crosses the wrap seam.
 *
 * Drops and tine lines follow the closed-form operators described by Lu et al.
 * A drop maps its core to one pigment band and radially displaces the previous
 * map outside the core. A comb is an integer-lattice family of closed toroidal
 * tine lines; spacing 0 selects one primitive closed line. The analytic fields
 * are continuous across the seam, although their sampled implementation is
 * subject to ordinary floating-point and interpolation error.
 *
 * Rendering and numerical diffusion
 * ---------------------------------
 * Ambient-flow map updates are committed only after accumulated motion reaches
 * roughly `stepPixels`. Between commits, smooth mode recomputes a display-only
 * partial warp from the last committed map. This gives continuous motion
 * without permanently resampling the coordinate map on every animation frame.
 *
 * Animated combs use the same split: the visible comb and its cumulative shear
 * are rendered every frame, but the tine operator is committed once when the
 * stroke ends. Thus a 1.2-second stroke does not permanently resample the map
 * roughly 36 times. Bilinear interpolation of the coordinate map still causes
 * some coordinate-space diffusion whenever a commit occurs; larger, less
 * frequent commits reduce it. `antialias` affects only the final page-atlas
 * lookup. Pigment bands remain exact solid colors.
 *
 * Resolution is limited by both `simMax` and `simPixels`; the latter makes cost
 * track area rather than only the longest viewport dimension. The final canvas
 * is smoothly enlarged by default (`pixelated: false`).
 *
 * Sheet and palette lifecycle
 * ---------------------------
 * Pigment rain, automatic tine strokes, palette changes, and sheet resets have
 * independent clocks. Palette rotation is a hard whole-bath recolor: repainting
 * the atlas bands immediately changes every extant drop assigned to each slot.
 *
 * Each new sheet resets the map to identity, cancels queued and active
 * old-sheet operators, resets rake/tine state, and crossfades from the pulled
 * print. Slot 0 remains rubrication red until the first drop of that sheet has
 * actually landed, even when a palette rotation coincides with the reset.
 * Hidden time is a true pause: deadlines, an active comb, the crossfade, and
 * ambient phase all resume where they stopped instead of firing a return-tab
 * burst.
 *
 * Host-page and startup contract
 * ------------------------------
 * The vendored html-to-image UMD export is captured in a private lexical
 * binding; pre-existing host globals are not replaced. Snapshot/setup is
 * cancellable and transactional. Immediate inclusion waits for `<body>`;
 * idle activity, visibility, or reduced-motion changes can cancel an in-flight
 * launch; partial setup and animation errors clean up the overlay and restore
 * focus, scroll position, and host styles.
 *
 * Interaction runs inside a same-origin iframe hosted by a modal `<dialog>`.
 * This prevents host CSS transforms, top-layer elements, focused controls,
 * keyboard shortcuts, delegated pointer handlers, and document scrolling from
 * operating behind the frozen snapshot. Mouse gestures are tracked per pointer;
 * moving or dragging rakes, right-click drops pigment, click stops. On touch,
 * drag rakes, long-press drops, and tap stops.
 *
 * Reproducibility and limitations
 * -------------------------------
 * One seeded PRNG controls stochastic choices, and structured events are kept
 * on `paperMarble.active.events`. This is a diagnostic trace, not an exact
 * replay: scheduling, pointer trajectories, viewport state, and page capture
 * are external inputs.
 *
 * html-to-image may re-fetch page resources while embedding them. Cross-origin
 * or unsupported resources may be omitted; total capture failure falls back to
 * a blank sheet in the effective page background. CPU cost scales principally
 * with simulation area. Discrete semi-Lagrangian resampling means exact pixel
 * census conservation is not promised, despite the non-diffusive color model.
 *
 * v11 (bounded runtime):
 *  - Pause after `runMinutes` of visible animation; mouse movement resumes it.
 *
 * v10 (documentation repair):
 *  - Restored this section after the v8 structural rewrite accidentally
 *    omitted it. The old block was not copied verbatim because several claims
 *    had become false: tine animation now commits once, tine/flow/rake geometry
 *    is toroidal, rubrication survives coincident palette rotation, the overlay
 *    is isolated, and hidden time pauses the simulation. No runtime behavior
 *    changes in v10.
 *
 * v9:
 *  - Hidden tabs became true pauses for scheduler deadlines, active strokes,
 *    crossfades, and ambient phase; smooth final enlargement became the default.
 *
 * v8 (reliability and torus rewrite):
 *  - Privatized the vendored library and namespaced runtime state; added cancellable,
 *    transactional startup and exact cleanup.
 *  - Replaced the host-page canvas with an isolated modal iframe and complete
 *    input/focus/scroll containment.
 *  - Made waves, rake distance, and tine geometry periodic; changed animated
 *    tines to display-only cumulative warps with one permanent commit.
 *  - Fixed reset/rubrication ordering, cancellation of active strokes, modern
 *    CSS color parsing, per-pointer gestures, configuration validation, and
 *    area-budgeted resolution.
 *
 * v7 ("ink video"):
 *  - Added rotating drift, periodic fresh sheets, smooth per-frame display
 *    warps between adaptive commits, page-atlas antialiasing, and animated combs.
 *
 * v6:
 *  - Added historical scan-derived palettes, hard whole-bath recoloring,
 *    palette-slot pigment storage, and higher default resolution.
 *
 * v5:
 *  - Made the bath toroidal; added automatic tine-line combing, pigment rain,
 *    and adaptive map commits.
 * ========================================================================== */

const MARBLE_OWNER = Symbol('paper-marble-owner');
const MARBLE_STATE = {
    active: null,
    starting: null,
    generation: 0,
    idleArmed: false
};

// Defaults may also be overridden for one run with
// `paperMarble.start({ config: { simPixels: 600000 } })`.
const MARBLE_DEFAULT_CONFIG = {
    seed:        0,         // seeded PRNG; 0 chooses and logs a random seed
    simMax:      1440,      // maximum simulation dimension
    simPixels:   1000000,   // area budget; keeps square viewports from becoming pathological
    fps:         30,
    driftSpeed:  1.6,
    driftPeriod: 480,
    curlAmp:     3,
    waves:       5,
    waveScale:   0.014,
    waveSpeed:   0.025,
    cell:        8,
    maxDt:       0.07,
    stepPixels:  0.75,
    smooth:      true,
    antialias:   true,
    pixelated:   false,    // smooth final upscale; true gives deliberate nearest-neighbor blocks
    rakeRadius:   0.06,
    rakeStrength: 0.9,
    rakeMax:      900,
    rakeDecay:    0.20,
    dropRadius:   0.035,
    rainMean:     12,
    rainFirst:    0.8,
    rainMargin:   0.12,
    tineMean:     25,
    tineFirst:    18,
    tineShift:    0.30,
    tineFalloff:  0.05,
    tineSpacing:  0.20,     // 0 is one closed toroidal tine
    tineStrokeSec: 1.2,     // 0 applies the stroke immediately
    resetMinutes: 10,
    runMinutes:   60,       // visible runtime before pausing; mouse movement resumes; 0 disables
    rubrication: '#cc0000',
    tintPage: true,
    paletteSeconds: 60,
    palettes: [
        // Designed palette; subsequent entries retain their historical sources.
        ['porcelain-rubrication', ['#111d32', '#f4f7fc', '#80b4d6', '#c52c46', '#326b9b', '#c8dce9']],
        ['french-curl-1735',   ['#717574', '#c8a186', '#e2bc9d', '#97403e', '#b77259', '#d69852']],
        ['english-spot-1830',  ['#313f4e', '#8b4739', '#ae937b', '#a76c4f', '#6c5f59', '#55342f']],
        ['french-curl-1880',   ['#520a0d', '#6e2a20', '#844c2d', '#4d4632', '#36627c', '#c16538']],
        ['victorian-spot',     ['#283e33', '#57646c', '#84171d', '#a88622', '#885230', '#3b1f19']],
        ['gilt-binding-1902',  ['#394d43', '#1f3435', '#0e1324', '#5d6552', '#968167', '#df944a']],
        ['suminagashi-ink',    ['#f2f0ea', '#e0d8d0', '#c6beb6', '#847c74', '#3b352d', '#1d1813']],
        ['suminagashi-modern', ['#81bfd6', '#57b5c1', '#160919', '#3d7088', '#1e445c', '#a3d4e5']]
    ]
};

function marbleValidateConfig(c) {
    const finite = (name, test) => {
        if (!Number.isFinite(c[name]) || !test(c[name])) {
            throw new RangeError('marble: invalid CONFIG.' + name + ' = ' + c[name]);
        }
    };
    finite('seed', v => Number.isInteger(v) && v >= 0 && v <= 0xffffffff);
    finite('simMax', v => v >= 2);
    finite('simPixels', v => v >= 4);
    finite('fps', v => v > 0);
    finite('driftSpeed', v => v >= 0);
    finite('driftPeriod', v => v > 0);
    finite('curlAmp', v => v >= 0);
    finite('waves', v => Number.isInteger(v) && v >= 1 && v <= 64);
    finite('waveScale', v => v > 0);
    finite('waveSpeed', v => v >= 0);
    finite('cell', v => v > 0);
    finite('maxDt', v => v > 0);
    finite('stepPixels', v => v > 0);
    finite('rakeRadius', v => v > 0);
    finite('rakeStrength', v => v >= 0);
    finite('rakeMax', v => v > 0);
    finite('rakeDecay', v => v > 0);
    finite('dropRadius', v => v > 0);
    finite('rainMean', v => v >= 0);
    finite('rainFirst', v => v >= 0);
    finite('rainMargin', v => v >= 0 && v < 0.5);
    finite('tineMean', v => v >= 0);
    finite('tineFirst', v => v >= 0);
    finite('tineShift', v => v >= 0);
    finite('tineFalloff', v => v > 0);
    finite('tineSpacing', v => v >= 0);
    finite('tineStrokeSec', v => v >= 0);
    finite('resetMinutes', v => v >= 0);
    finite('runMinutes', v => v >= 0);
    finite('paletteSeconds', v => v > 0);
    for (const name of ['smooth', 'antialias', 'pixelated', 'tintPage']) {
        if (typeof c[name] !== 'boolean') throw new TypeError('marble: CONFIG.' + name + ' must be boolean');
    }
    const isHex = value => typeof value === 'string' && /^#(?:[0-9a-f]{3}|[0-9a-f]{6})$/i.test(value);
    if (!isHex(c.rubrication)) throw new TypeError('marble: CONFIG.rubrication must be #rgb or #rrggbb');
    if (!Array.isArray(c.palettes) || c.palettes.length === 0) {
        throw new TypeError('marble: CONFIG.palettes must be a non-empty array');
    }
    let slots = 0;
    for (const palette of c.palettes) {
        if (!Array.isArray(palette) || typeof palette[0] !== 'string' || !Array.isArray(palette[1]) || palette[1].length === 0) {
            throw new TypeError('marble: each palette must be [name, non-empty color array]');
        }
        if (!slots) slots = palette[1].length;
        if (palette[1].length !== slots || !palette[1].every(isHex)) {
            throw new TypeError('marble: all palettes must have the same number of valid hex colors');
        }
    }
}

async function marbleWaitForBody(alive) {
    while (!document.body) {
        if (!alive()) return false;
        await new Promise(resolve => {
            if (document.readyState === 'loading') {
                document.addEventListener('DOMContentLoaded', resolve, { once: true });
            } else {
                setTimeout(resolve, 0);
            }
        });
    }
    return alive();
}

function marbleInstallPersistentAlias(name, value) {
    const prior = Object.getOwnPropertyDescriptor(window, name);
    if (prior && prior.value !== value && !(prior.value && prior.value[MARBLE_OWNER])) {
        console.warn('marble: leaving pre-existing window.' + name + ' untouched; use window.paperMarble instead.');
        return false;
    }
    try {
        Object.defineProperty(window, name, {
            configurable: true,
            enumerable: false,
            writable: true,
            value
        });
        return true;
    } catch (error) {
        console.warn('marble: could not expose window.' + name + ': ' + error.message);
        return false;
    }
}

function marbleInstallTemporaryAlias(name, value) {
    const prior = Object.getOwnPropertyDescriptor(window, name);
    if (prior && !(prior.value && prior.value[MARBLE_OWNER])) {
        console.warn('marble: leaving pre-existing window.' + name + ' untouched; use window.paperMarble.stop().');
        return () => {};
    }
    try {
        Object.defineProperty(window, name, {
            configurable: true,
            enumerable: false,
            writable: true,
            value
        });
    } catch (error) {
        console.warn('marble: could not expose window.' + name + ': ' + error.message);
        return () => {};
    }
    return () => {
        const current = Object.getOwnPropertyDescriptor(window, name);
        if (!current || current.value !== value) return;
        try {
            if (prior) Object.defineProperty(window, name, prior);
            else delete window[name];
        } catch (error) {
            console.warn('marble: could not restore window.' + name + ': ' + error.message);
        }
    };
}

function marbleCancelPendingStart() {
    if (MARBLE_STATE.starting) MARBLE_STATE.generation++;
}

function marbleStopActive(reason) {
    if (MARBLE_STATE.active) MARBLE_STATE.active.stop(reason || 'api');
    else marbleCancelPendingStart();
}

async function marbleStart(options) {
    options = options && typeof options === 'object' ? options : {};
    if (MARBLE_STATE.active) MARBLE_STATE.active.stop('restart');
    if (MARBLE_STATE.starting) return MARBLE_STATE.starting.promise;

    const token = ++MARBLE_STATE.generation;
    const promise = marbleRun(token, options);
    MARBLE_STATE.starting = { token, promise };
    try {
        return await promise;
    } finally {
        if (MARBLE_STATE.starting && MARBLE_STATE.starting.token === token) {
            MARBLE_STATE.starting = null;
        }
    }
}
Object.defineProperty(marbleStart, MARBLE_OWNER, { value: true });
Object.defineProperty(marbleStopActive, MARBLE_OWNER, { value: true });

async function marbleRun(token, options) {
    const guard = typeof options.guard === 'function' ? options.guard : () => true;
    const alive = () => {
        if (MARBLE_STATE.generation !== token) return false;
        try { return guard() !== false; } catch (error) { return false; }
    };
    if (!await marbleWaitForBody(alive)) return null;

    const CONFIG = Object.assign({}, MARBLE_DEFAULT_CONFIG, options.config || {});
    marbleValidateConfig(CONFIG);
    if (!alive()) return null;

    const vw = Math.max(2, window.innerWidth || document.documentElement.clientWidth || 2);
    const vh = Math.max(2, window.innerHeight || document.documentElement.clientHeight || 2);
    const dimensionScale = CONFIG.simMax / Math.max(vw, vh);
    const pixelScale = Math.sqrt(CONFIG.simPixels / (vw * vh));
    const scale = Math.min(1, dimensionScale, pixelScale);
    const sw = Math.max(2, Math.round(vw * scale));
    const sh = Math.max(2, Math.round(vh * scale));
    const minDim = Math.min(sw, sh);

    // Parse CSS colors through the browser itself. This handles rgb() space/slash
    // syntax, color(), lab(), oklch(), display-p3, and future syntaxes supported
    // by the current engine.
    const colorProbe = document.createElement('canvas');
    colorProbe.width = colorProbe.height = 1;
    const colorContext = colorProbe.getContext('2d', { willReadFrequently: true });
    if (!colorContext) throw new Error('marble: 2D canvas is unavailable');
    function parseCssColor(value) {
        colorContext.clearRect(0, 0, 1, 1);
        colorContext.fillStyle = 'rgba(0, 0, 0, 0)';
        try { colorContext.fillStyle = value || 'transparent'; } catch (error) {}
        colorContext.fillRect(0, 0, 1, 1);
        return Array.from(colorContext.getImageData(0, 0, 1, 1).data);
    }
    function compositeColor(foreground, background) {
        const af = foreground[3] / 255;
        const ab = background[3] / 255;
        const ao = af + ab * (1 - af);
        if (ao <= 0) return [0, 0, 0, 0];
        return [
            Math.round((foreground[0] * af + background[0] * ab * (1 - af)) / ao),
            Math.round((foreground[1] * af + background[1] * ab * (1 - af)) / ao),
            Math.round((foreground[2] * af + background[2] * ab * (1 - af)) / ao),
            Math.round(ao * 255)
        ];
    }
    const rootColor = parseCssColor(getComputedStyle(document.documentElement).backgroundColor);
    const bodyColor = parseCssColor(getComputedStyle(document.body).backgroundColor);
    const white = [255, 255, 255, 255];
    const pageBackdrop = compositeColor(rootColor, white);
    const pageEffective = compositeColor(bodyColor, pageBackdrop);
    const backdropCss = 'rgb(' + pageBackdrop[0] + ' ' + pageBackdrop[1] + ' ' + pageBackdrop[2] + ')';
    const effectiveCss = 'rgb(' + pageEffective[0] + ' ' + pageEffective[1] + ' ' + pageEffective[2] + ')';

    async function snapshotViewport() {
        try {
            if (!alive()) return null;
            const opts = {
                width: vw,
                height: vh,
                pixelRatio: 1,
                backgroundColor: backdropCss,
                cacheBust: false,
                style: {
                    transform: 'translate(' + (-window.scrollX) + 'px,' + (-window.scrollY) + 'px)',
                    transformOrigin: 'top left'
                }
            };
            const isWebKit = /AppleWebKit/.test(navigator.userAgent)
                && !/Chrome|Chromium|Edg\//.test(navigator.userAgent);
            if (isWebKit) {
                try { await MARBLE_HTML_TO_IMAGE.toCanvas(document.body, opts); } catch (error) {}
                if (!alive()) return null;
            }
            const canvas = await MARBLE_HTML_TO_IMAGE.toCanvas(document.body, opts);
            return alive() ? canvas : null;
        } catch (error) {
            if (!alive()) return null;
            console.warn('marble: page capture failed (' + (error && error.message) + '); marbling a blank sheet.');
        }
        const canvas = document.createElement('canvas');
        canvas.width = vw;
        canvas.height = vh;
        const context = canvas.getContext('2d');
        if (!context) throw new Error('marble: 2D canvas is unavailable');
        context.fillStyle = effectiveCss;
        context.fillRect(0, 0, vw, vh);
        return canvas;
    }

    // Bound the wait even if a resource never settles. The timeout cannot
    // interrupt synchronous cloning or abort the vendor's outstanding work;
    // invalidate this launch so a late capture cannot display an overlay.
    let captureTimer;
    const snap = await Promise.race([
        snapshotViewport(),
        new Promise(resolve => {
            captureTimer = setTimeout(() => {
                if (alive()) console.warn('marble: page capture timed out; startup cancelled.');
                if (MARBLE_STATE.generation === token) MARBLE_STATE.generation++;
                resolve(null);
            }, 30 * 1000);
        })
    ]).finally(() => clearTimeout(captureTimer));
    if (!snap || !alive()) return null;

    let sim = document.createElement('canvas');
    sim.width = sw;
    sim.height = sh;
    let sctx = sim.getContext('2d', { willReadFrequently: true });
    if (!sctx) throw new Error('marble: 2D canvas is unavailable');
    sctx.drawImage(snap, 0, 0, sw, sh);

    let srcImg;
    try {
        srcImg = sctx.getImageData(0, 0, sw, sh);
    } catch (error) {
        console.warn('marble: snapshot canvas tainted (' + (error && error.message) + '); marbling a blank sheet.');
        sim = document.createElement('canvas');
        sim.width = sw;
        sim.height = sh;
        sctx = sim.getContext('2d', { willReadFrequently: true });
        if (!sctx) throw new Error('marble: 2D canvas is unavailable');
        sctx.fillStyle = effectiveCss;
        sctx.fillRect(0, 0, sw, sh);
        srcImg = sctx.getImageData(0, 0, sw, sh);
    }
    {
        const d = srcImg.data;
        const br = pageBackdrop[0], bg = pageBackdrop[1], bb = pageBackdrop[2];
        for (let i = 3; i < d.length; i += 4) {
            const a = d[i];
            if (a < 255) {
                const k = a / 255, j = i - 3;
                d[j] = d[j] * k + br * (1 - k);
                d[j + 1] = d[j + 1] * k + bg * (1 - k);
                d[j + 2] = d[j + 2] * k + bb * (1 - k);
                d[i] = 255;
            }
        }
    }
    if (!alive()) return null;

    const SEAM = 8;
    const SWATCH_BASE = 1 << 20;
    const SWATCH_H = 2 * SEAM + 2;
    const HALF_BASE = SWATCH_BASE / 2;
    const palNames = CONFIG.palettes.map(p => p[0]);
    const palColors = CONFIG.palettes.map(p => p[1]);
    const SLOTS = 1 + palColors[0].length;
    const bandHex = new Array(SLOTS);
    const parseHex = hex => {
        const h = hex.slice(1);
        if (h.length === 3) {
            return [parseInt(h[0] + h[0], 16), parseInt(h[1] + h[1], 16), parseInt(h[2] + h[2], 16)];
        }
        return [parseInt(h.slice(0, 2), 16), parseInt(h.slice(2, 4), 16), parseInt(h.slice(4, 6), 16)];
    };
    const atlasH = sh + SLOTS * SWATCH_H;
    const srcPix = new Uint8ClampedArray(sw * atlasH * 4);
    srcPix.set(srcImg.data);
    function paintBand(slot, hex) {
        bandHex[slot] = hex;
        const rgb = parseHex(hex);
        const y0 = sh + slot * SWATCH_H;
        for (let y = y0; y < y0 + SWATCH_H; y++) {
            let i = (y * sw) * 4;
            for (let x = 0; x < sw; x++, i += 4) {
                srcPix[i] = rgb[0];
                srcPix[i + 1] = rgb[1];
                srcPix[i + 2] = rgb[2];
                srcPix[i + 3] = 255;
            }
        }
    }

    let paletteIdx = 0;
    paintBand(0, CONFIG.rubrication);
    palColors[0].forEach((hex, k) => paintBand(1 + k, hex));
    function tintPage() {
        if (!CONFIG.tintPage) {
            srcPix.set(srcImg.data.subarray(0, sw * sh * 4), 0);
            return;
        }
        const cols = palColors[paletteIdx].map(parseHex);
        const lum = cols.map(rgb => (rgb[0] * 77 + rgb[1] * 151 + rgb[2] * 28) >> 8);
        const lut = new Uint8Array(256 * 3);
        for (let v = 0; v < 256; v++) {
            let best = 0, distance = Infinity;
            for (let j = 0; j < cols.length; j++) {
                const d = Math.abs(lum[j] - v);
                if (d < distance) { distance = d; best = j; }
            }
            lut[v * 3] = cols[best][0];
            lut[v * 3 + 1] = cols[best][1];
            lut[v * 3 + 2] = cols[best][2];
        }
        const orig = srcImg.data;
        const n = sw * sh * 4;
        for (let i = 0; i < n; i += 4) {
            const v = (orig[i] * 77 + orig[i + 1] * 151 + orig[i + 2] * 28) >> 8;
            // Keep the source paper and darkest ink as tonal anchors.
            if (v >= 244 || v <= 24) {
                srcPix[i] = orig[i];
                srcPix[i + 1] = orig[i + 1];
                srcPix[i + 2] = orig[i + 2];
                srcPix[i + 3] = 255;
                continue;
            }
            srcPix[i] = lut[v * 3];
            srcPix[i + 1] = lut[v * 3 + 1];
            srcPix[i + 2] = lut[v * 3 + 2];
            srcPix[i + 3] = 255;
        }
    }
    tintPage();

    const swatchY = slot => SWATCH_BASE + slot * SWATCH_H + SWATCH_H / 2;
    const swatchX = sw / 2;
    const out = new ImageData(sw, sh);
    const outPix = out.data;
    const N = sw * sh;
    let mapX = new Float32Array(N), mapY = new Float32Array(N);
    let mapX2 = new Float32Array(N), mapY2 = new Float32Array(N);
    for (let y = 0, i = 0; y < sh; y++) {
        for (let x = 0; x < sw; x++, i++) { mapX[i] = x; mapY[i] = y; }
    }
    const hw = sw / 2, hh = sh / 2;

    function wrap(value, period) {
        value %= period;
        return value < 0 ? value + period : value;
    }

    function sampleMapInto(sx, sy, i) {
        sx = wrap(sx, sw);
        sy = wrap(sy, sh);
        const x0 = Math.floor(sx), y0 = Math.floor(sy);
        const bx = sx - x0, by = sy - y0;
        const x1 = x0 + 1 < sw ? x0 + 1 : 0;
        const y1 = y0 + 1 < sh ? y0 + 1 : 0;
        const r0 = y0 * sw, r1 = y1 * sw;
        const i00 = r0 + x0, i10 = r0 + x1, i01 = r1 + x0, i11 = r1 + x1;
        let x00 = mapX[i00], x10 = mapX[i10], x01 = mapX[i01], x11 = mapX[i11];
        let y00 = mapY[i00], y10 = mapY[i10], y01 = mapY[i01], y11 = mapY[i11];
        const w00 = (1 - bx) * (1 - by), w10 = bx * (1 - by);
        const w01 = (1 - bx) * by, w11 = bx * by;
        const g00 = y00 >= HALF_BASE, g10 = y10 >= HALF_BASE;
        const g01 = y01 >= HALF_BASE, g11 = y11 >= HALF_BASE;
        let d;
        if (!g00 && !g10 && !g01 && !g11) {
            d = x10 - x00; if (d > hw || d < -hw) x10 -= sw * Math.round(d / sw);
            d = x01 - x00; if (d > hw || d < -hw) x01 -= sw * Math.round(d / sw);
            d = x11 - x00; if (d > hw || d < -hw) x11 -= sw * Math.round(d / sw);
            d = y10 - y00; if (d > hh || d < -hh) y10 -= sh * Math.round(d / sh);
            d = y01 - y00; if (d > hh || d < -hh) y01 -= sh * Math.round(d / sh);
            d = y11 - y00; if (d > hh || d < -hh) y11 -= sh * Math.round(d / sh);
            mapX2[i] = x00 * w00 + x10 * w10 + x01 * w01 + x11 * w11;
            mapY2[i] = y00 * w00 + y10 * w10 + y01 * w01 + y11 * w11;
            return;
        }
        if (g00 && g10 && g01 && g11) {
            let mn = Math.min(y00, y10, y01, y11);
            let mx = Math.max(y00, y10, y01, y11);
            if (mx - mn <= SEAM) {
                mapX2[i] = x00 * w00 + x10 * w10 + x01 * w01 + x11 * w11;
                mapY2[i] = y00 * w00 + y10 * w10 + y01 * w01 + y11 * w11;
                return;
            }
        }
        let refW = w00, refX = x00, refY = y00, refG = g00;
        if (w10 > refW) { refW = w10; refX = x10; refY = y10; refG = g10; }
        if (w01 > refW) { refW = w01; refX = x01; refY = y01; refG = g01; }
        if (w11 > refW) { refW = w11; refX = x11; refY = y11; refG = g11; }
        let ex = 0, ey = 0, ew = 0;
        if (refG) {
            if (g00 && Math.abs(y00 - refY) <= SEAM) { ex += x00 * w00; ey += y00 * w00; ew += w00; }
            if (g10 && Math.abs(y10 - refY) <= SEAM) { ex += x10 * w10; ey += y10 * w10; ew += w10; }
            if (g01 && Math.abs(y01 - refY) <= SEAM) { ex += x01 * w01; ey += y01 * w01; ew += w01; }
            if (g11 && Math.abs(y11 - refY) <= SEAM) { ex += x11 * w11; ey += y11 * w11; ew += w11; }
        } else {
            const addPage = (x, y, weight) => {
                let dx = x - refX, dy = y - refY;
                if (dx > hw || dx < -hw) x -= sw * Math.round(dx / sw);
                if (dy > hh || dy < -hh) y -= sh * Math.round(dy / sh);
                ex += x * weight; ey += y * weight; ew += weight;
            };
            if (!g00) addPage(x00, y00, w00);
            if (!g10) addPage(x10, y10, w10);
            if (!g01) addPage(x01, y01, w01);
            if (!g11) addPage(x11, y11, w11);
        }
        mapX2[i] = ew ? ex / ew : refX;
        mapY2[i] = ew ? ey / ew : refY;
    }

    let framesSinceNorm = 0;
    function renormalize() {
        let sx = 0, sy = 0, n = 0;
        for (let i = 0; i < N; i += 97) {
            if (mapY[i] < HALF_BASE) { sx += mapX[i]; sy += mapY[i]; n++; }
        }
        if (!n) return;
        const kx = Math.floor((sx / n) / sw) * sw;
        const ky = Math.floor((sy / n) / sh) * sh;
        if (!kx && !ky) return;
        for (let i = 0; i < N; i++) {
            if (mapY[i] < HALF_BASE) { mapX[i] -= kx; mapY[i] -= ky; }
        }
    }

    const DROP_SOFT = 0.2;
    function applyDrop(cx, cy, radius, pigment) {
        const r2 = radius * radius;
        const h2 = DROP_SOFT * DROP_SOFT * r2;
        const pigmentY = swatchY(pigment);
        let i = 0;
        for (let y = 0; y < sh; y++) {
            let dy = y - cy;
            dy -= sh * Math.round(dy / sh);
            const dy2 = dy * dy;
            for (let x = 0; x < sw; x++, i++) {
                let dx = x - cx;
                dx -= sw * Math.round(dx / sw);
                const d2 = dx * dx + dy2;
                if (d2 <= r2) {
                    mapX2[i] = swatchX;
                    mapY2[i] = pigmentY;
                } else {
                    const s = Math.sqrt((d2 - r2 + h2) / d2);
                    sampleMapInto(cx + dx * s, cy + dy * s, i);
                }
            }
        }
        let tmp = mapX; mapX = mapX2; mapX2 = tmp;
        tmp = mapY; mapY = mapY2; mapY2 = tmp;
    }

    function gcd(a, b) {
        a = Math.abs(a); b = Math.abs(b);
        while (b) { const t = a % b; a = b; b = t; }
        return a || 1;
    }

    // A toroidal comb must be defined by an integer lattice covector. Its phase
    // q = nx*x/sw + ny*y/sh is exactly periodic; displacement is tangent to q,
    // so the shear remains area-preserving and continuous at every wrap seam.
    function makeTineGeometry(ax, ay, requestedAngle, requestedSpacing) {
        const requestedMx = Math.cos(requestedAngle);
        const requestedMy = Math.sin(requestedAngle);
        const requestedNx = -requestedMy;
        const requestedNy = requestedMx;
        const single = requestedSpacing === 0;
        const targetSpacing = single ? minDim : requestedSpacing;
        let nx = Math.round(requestedNx * sw / targetSpacing);
        let ny = Math.round(requestedNy * sh / targetSpacing);
        if (!nx && !ny) {
            if (Math.abs(requestedNx) * sw >= Math.abs(requestedNy) * sh) nx = requestedNx < 0 ? -1 : 1;
            else ny = requestedNy < 0 ? -1 : 1;
        }
        if (single) {
            const divisor = gcd(nx, ny);
            nx /= divisor;
            ny /= divisor;
        }
        const gx = nx / sw, gy = ny / sh;
        const gmag = Math.hypot(gx, gy);
        let mxd = gy / gmag, myd = -gx / gmag;
        if (mxd * requestedMx + myd * requestedMy < 0) { mxd = -mxd; myd = -myd; }
        return {
            x: ax,
            y: ay,
            nx,
            ny,
            gmag,
            mxd,
            myd,
            nxd: gx / gmag,
            nyd: gy / gmag,
            spacing: 1 / gmag,
            single
        };
    }

    function tineShiftAt(x, y, geometry, amount, lambda) {
        const phase = geometry.nx * (x - geometry.x) / sw
            + geometry.ny * (y - geometry.y) / sh;
        const phaseDistance = phase - Math.round(phase);
        const distance = Math.abs(phaseDistance) / geometry.gmag;
        return amount * lambda / (distance + lambda);
    }

    function applyTine(geometry, amount, lambda) {
        let i = 0;
        for (let y = 0; y < sh; y++) {
            for (let x = 0; x < sw; x++, i++) {
                const shift = tineShiftAt(x, y, geometry, amount, lambda);
                sampleMapInto(x - shift * geometry.mxd, y - shift * geometry.myd, i);
            }
        }
        let tmp = mapX; mapX = mapX2; mapX2 = tmp;
        tmp = mapY; mapY = mapY2; mapY2 = tmp;
    }

    let seed = (CONFIG.seed >>> 0)
        || (((Date.now() & 0xffffffff) ^ ((Math.random() * 0x100000000) | 0)) >>> 0)
        || 1;
    const seedUsed = seed;
    const rand = () => {
        seed = (seed + 0x6D2B79F5) >>> 0;
        let z = seed;
        z = Math.imul(z ^ (z >>> 15), z | 1);
        z ^= z + Math.imul(z ^ (z >>> 7), z | 61);
        return ((z ^ (z >>> 14)) >>> 0) / 4294967296;
    };

    const waves = [];
    const TAU = Math.PI * 2;
    for (let i = 0; i < CONFIG.waves; i++) {
        const angle = rand() * TAU;
        const targetK = CONFIG.waveScale * (0.5 + rand());
        let nx = Math.round(Math.cos(angle) * targetK * sw / TAU);
        let ny = Math.round(Math.sin(angle) * targetK * sh / TAU);
        if (!nx && !ny) {
            if (sw >= sh) nx = rand() < 0.5 ? -1 : 1;
            else ny = rand() < 0.5 ? -1 : 1;
        }
        const kx = TAU * nx / sw;
        const ky = TAU * ny / sh;
        const k = Math.hypot(kx, ky);
        waves.push({
            nx,
            ny,
            kx,
            ky,
            w: (rand() - 0.5) * 2 * CONFIG.waveSpeed,
            phi: rand() * TAU,
            amp: (0.6 + 0.8 * rand()) / k
        });
    }

    const cell = CONFIG.cell;
    const gw = Math.max(2, Math.ceil(sw / cell) + 1);
    const gh = Math.max(2, Math.ceil(sh / cell) + 1);
    const gridDx = sw / (gw - 1);
    const gridDy = sh / (gh - 1);
    const invGridDx = 1 / gridDx;
    const invGridDy = 1 / gridDy;
    const vxg = new Float32Array(gw * gh);
    const vyg = new Float32Array(gw * gh);
    const rakeSig2 = (CONFIG.rakeRadius * minDim) ** 2;
    const rake = { x: -1e9, y: -1e9, vx: 0, vy: 0, lastT: 0, active: false };
    let velMax2 = 0;

    function updateVelocity(t) {
        velMax2 = 0;
        const A = CONFIG.curlAmp / CONFIG.waves;
        const driftAngle = TAU * t / CONFIG.driftPeriod;
        const driftX = CONFIG.driftSpeed * Math.cos(driftAngle);
        const driftY = CONFIG.driftSpeed * Math.sin(driftAngle);
        const useRake = rake.active && rake.vx * rake.vx + rake.vy * rake.vy > 1;
        for (let gy = 0; gy < gh; gy++) {
            const y = gy * gridDy;
            for (let gx = 0; gx < gw; gx++) {
                const x = gx * gridDx;
                let u = driftX, v = driftY;
                for (let i = 0; i < waves.length; i++) {
                    const wave = waves[i];
                    const c = Math.cos(wave.kx * x + wave.ky * y + wave.w * t + wave.phi) * wave.amp;
                    u += A * wave.ky * c;
                    v -= A * wave.kx * c;
                }
                if (useRake) {
                    let ddx = x - rake.x, ddy = y - rake.y;
                    ddx -= sw * Math.round(ddx / sw);
                    ddy -= sh * Math.round(ddy / sh);
                    const r2 = ddx * ddx + ddy * ddy;
                    if (r2 < 7 * rakeSig2) {
                        const g = Math.exp(-r2 / rakeSig2) * CONFIG.rakeStrength;
                        const k2 = 2 * (rake.vx * ddy - rake.vy * ddx) / rakeSig2;
                        u += g * (rake.vx - ddy * k2);
                        v += g * (rake.vy + ddx * k2);
                    }
                }
                const index = gy * gw + gx;
                vxg[index] = u;
                vyg[index] = v;
                const m2 = u * u + v * v;
                if (m2 > velMax2) velMax2 = m2;
            }
        }
    }

    let sampledVelocityX = 0, sampledVelocityY = 0;
    function sampleVelocity(x, y) {
        x = wrap(x, sw);
        y = wrap(y, sh);
        const gxF = x * invGridDx;
        const gyF = y * invGridDy;
        const gx0 = Math.min(gw - 2, Math.floor(gxF));
        const gy0 = Math.min(gh - 2, Math.floor(gyF));
        const fx = gxF - gx0, fy = gyF - gy0;
        const gx1 = gx0 + 1, gy1 = gy0 + 1;
        const row0 = gy0 * gw, row1 = gy1 * gw;
        sampledVelocityX = (vxg[row0 + gx0] * (1 - fx) + vxg[row0 + gx1] * fx) * (1 - fy)
            + (vxg[row1 + gx0] * (1 - fx) + vxg[row1 + gx1] * fx) * fy;
        sampledVelocityY = (vyg[row0 + gx0] * (1 - fx) + vyg[row0 + gx1] * fx) * (1 - fy)
            + (vyg[row1 + gx0] * (1 - fx) + vyg[row1 + gx1] * fx) * fy;
    }

    function step(dt, commit, tineWarp) {
        const antialias = CONFIG.antialias;
        let i = 0, di = 0;
        for (let y = 0; y < sh; y++) {
            for (let x = 0; x < sw; x++, i++, di += 4) {
                let qx = x, qy = y;
                if (tineWarp) {
                    const shift = tineShiftAt(x, y, tineWarp.geometry, tineWarp.amount, tineWarp.lambda);
                    qx = x - shift * tineWarp.geometry.mxd;
                    qy = y - shift * tineWarp.geometry.myd;
                }
                sampleVelocity(qx, qy);
                sampleMapInto(qx - sampledVelocityX * dt, qy - sampledVelocityY * dt, i);

                let mx = wrap(mapX2[i], sw);
                let my = mapY2[i];
                if (my >= SWATCH_BASE - 1) {
                    const ry = Math.floor(my - SWATCH_BASE + sh + 0.5);
                    let rx = Math.floor(mx + 0.5);
                    if (rx >= sw) rx = 0;
                    const si = (ry * sw + rx) * 4;
                    outPix[di] = srcPix[si];
                    outPix[di + 1] = srcPix[si + 1];
                    outPix[di + 2] = srcPix[si + 2];
                } else {
                    my = wrap(my, sh);
                    if (antialias) {
                        const ax0 = Math.floor(mx), ay0 = Math.floor(my);
                        const bx = mx - ax0, by = my - ay0;
                        const ax1 = ax0 + 1 < sw ? ax0 + 1 : 0;
                        const ay1 = ay0 + 1 < sh ? ay0 + 1 : 0;
                        const j00 = (ay0 * sw + ax0) * 4, j10 = (ay0 * sw + ax1) * 4;
                        const j01 = (ay1 * sw + ax0) * 4, j11 = (ay1 * sw + ax1) * 4;
                        const q00 = (1 - bx) * (1 - by), q10 = bx * (1 - by);
                        const q01 = (1 - bx) * by, q11 = bx * by;
                        outPix[di] = srcPix[j00] * q00 + srcPix[j10] * q10 + srcPix[j01] * q01 + srcPix[j11] * q11;
                        outPix[di + 1] = srcPix[j00 + 1] * q00 + srcPix[j10 + 1] * q10 + srcPix[j01 + 1] * q01 + srcPix[j11 + 1] * q11;
                        outPix[di + 2] = srcPix[j00 + 2] * q00 + srcPix[j10 + 2] * q10 + srcPix[j01 + 2] * q01 + srcPix[j11 + 2] * q11;
                    } else {
                        let rx = Math.floor(mx + 0.5), ry = Math.floor(my + 0.5);
                        if (rx >= sw) rx = 0;
                        if (ry >= sh) ry = 0;
                        const si = (ry * sw + rx) * 4;
                        outPix[di] = srcPix[si];
                        outPix[di + 1] = srcPix[si + 1];
                        outPix[di + 2] = srcPix[si + 2];
                    }
                }
                outPix[di + 3] = 255;
            }
        }
        if (commit) {
            let tmp = mapX; mapX = mapX2; mapX2 = tmp;
            tmp = mapY; mapY = mapY2; mapY2 = tmp;
        }
    }

    function createOverlayUi() {
        const priorFocus = document.activeElement;
        const scrollX = window.scrollX, scrollY = window.scrollY;
        const disposers = [];
        const restoreStyles = [];
        let dialog = null, iframe = null, frameDocument = null, frameWindow = null;
        let canvas = null, context = null, shown = false, cleaned = false;

        const cleanup = () => {
            if (cleaned) return;
            cleaned = true;
            while (disposers.length) {
                try { disposers.pop()(); } catch (error) {}
            }
            if (dialog && shown && dialog.open && typeof dialog.close === 'function') {
                try { dialog.close(); } catch (error) {}
            }
            if (dialog) dialog.remove();
            while (restoreStyles.length) {
                try { restoreStyles.pop()(); } catch (error) {}
            }
            try { window.scrollTo(scrollX, scrollY); } catch (error) {}
            if (priorFocus && priorFocus.isConnected && typeof priorFocus.focus === 'function') {
                try { priorFocus.focus({ preventScroll: true }); } catch (error) {}
            }
        };

        try {
            const setImportant = (element, property, value) => {
                const oldValue = element.style.getPropertyValue(property);
                const oldPriority = element.style.getPropertyPriority(property);
                element.style.setProperty(property, value, 'important');
                restoreStyles.push(() => {
                    if (element.style.getPropertyValue(property) !== value
                        || element.style.getPropertyPriority(property) !== 'important') return;
                    if (oldValue) element.style.setProperty(property, oldValue, oldPriority);
                    else element.style.removeProperty(property);
                });
            };
            for (const element of [document.documentElement, document.body]) {
                setImportant(element, 'overflow-x', 'hidden');
                setImportant(element, 'overflow-y', 'hidden');
                setImportant(element, 'overscroll-behavior-x', 'none');
                setImportant(element, 'overscroll-behavior-y', 'none');
                setImportant(element, 'touch-action', 'none');
            }

            dialog = document.createElement('dialog');
            dialog.setAttribute('aria-label', 'Paper marbling screensaver');
            for (const [property, value] of Object.entries({
                all: 'initial', display: 'block', position: 'fixed', inset: '0', width: '100vw', height: '100vh',
                maxWidth: 'none', maxHeight: 'none', margin: '0', padding: '0', border: '0',
                overflow: 'hidden', background: 'transparent', boxSizing: 'border-box'
            })) dialog.style.setProperty(property.replace(/[A-Z]/g, m => '-' + m.toLowerCase()), value, 'important');

            iframe = document.createElement('iframe');
            iframe.setAttribute('title', 'Paper marbling interaction surface');
            iframe.src = 'about:blank';
            for (const [property, value] of Object.entries({
                all: 'initial', display: 'block', position: 'absolute', inset: '0', width: '100%', height: '100%',
                margin: '0', padding: '0', border: '0', overflow: 'hidden', background: 'transparent'
            })) iframe.style.setProperty(property.replace(/[A-Z]/g, m => '-' + m.toLowerCase()), value, 'important');
            dialog.appendChild(iframe);
            document.documentElement.appendChild(dialog);

            frameDocument = iframe.contentDocument;
            frameWindow = iframe.contentWindow;
            if (!frameDocument || !frameWindow) throw new Error('marble: could not create isolated overlay frame');
            frameDocument.open();
            frameDocument.write('<!doctype html><html><head><title>Paper marbling</title></head><body></body></html>');
            frameDocument.close();
            Object.assign(frameDocument.documentElement.style, {
                margin: '0', padding: '0', width: '100%', height: '100%', overflow: 'hidden',
                background: 'transparent', overscrollBehavior: 'none'
            });
            Object.assign(frameDocument.body.style, {
                margin: '0', padding: '0', width: '100%', height: '100%', overflow: 'hidden',
                background: 'transparent', overscrollBehavior: 'none', touchAction: 'none'
            });
            canvas = frameDocument.createElement('canvas');
            canvas.width = vw;
            canvas.height = vh;
            canvas.tabIndex = 0;
            canvas.setAttribute('aria-label', 'Animated paper marbling. Escape or click to close.');
            Object.assign(canvas.style, {
                display: 'block', width: '100%', height: '100%', margin: '0', padding: '0',
                cursor: 'crosshair', touchAction: 'none', userSelect: 'none', outline: 'none'
            });
            frameDocument.body.appendChild(canvas);
            context = canvas.getContext('2d');
            if (!context) throw new Error('marble: 2D canvas is unavailable');
            context.imageSmoothingEnabled = !CONFIG.pixelated;
            context.imageSmoothingQuality = 'high';

            let pinningScroll = false;
            const pinScroll = () => {
                if (pinningScroll || (window.scrollX === scrollX && window.scrollY === scrollY)) return;
                pinningScroll = true;
                window.scrollTo(scrollX, scrollY);
                pinningScroll = false;
            };
            window.addEventListener('scroll', pinScroll, { capture: true, passive: true });
            disposers.push(() => window.removeEventListener('scroll', pinScroll, true));

            const show = () => {
                if (typeof dialog.showModal === 'function') dialog.showModal();
                else dialog.setAttribute('open', '');
                shown = true;
                try { frameWindow.focus(); } catch (error) {}
                try { canvas.focus({ preventScroll: true }); } catch (error) { canvas.focus(); }
            };
            return { dialog, iframe, frameWindow, frameDocument, canvas, context, show, cleanup, disposers };
        } catch (error) {
            cleanup();
            throw error;
        }
    }

    let ui = null;
    const localDisposers = [];
    let running = false;
    let stopped = false;
    let raf = 0;
    let stopAliasRestore = () => {};
    let controller = null;
    let stroke = null;
    let dtAcc = 0;
    let overlay = null, octx = null;
    let fade = null, fctx = null;
    const FADE_MS = 1500;
    let fadeUntil = 0;
    const gestures = new Map();
    let rakePointerId = null;
    let mouseReleaseMoved = false;
    let lastTouchAt = -Infinity;

    const startT = performance.now();
    const events = [];
    const simT = now => 'marble[' + ((now - startT) / 1000).toFixed(1) + 's]';
    function record(type, data, now) {
        const event = Object.assign({ type, time: ((now || performance.now()) - startT) / 1000 }, data || {});
        events.push(event);
        if (events.length > 10000) events.shift();
        return event;
    }

    const dropR = minDim * CONFIG.dropRadius;
    const opQueue = [];
    let nextRain = startT + CONFIG.rainFirst * 1000;
    let nextTine = startT + CONFIG.tineFirst * 1000;
    let rainCount = 0;
    let tineSign = 1;
    let paletteAt = startT;
    let sheetAt = startT;
    let cycleNext = 0;
    let paletteChanged = false;
    let rubricationPending = true;

    function exponentialDelay(meanSeconds) {
        const u = Math.max(Number.MIN_VALUE, 1 - rand());
        return -Math.log(u) * meanSeconds * 1000;
    }

    function resetSheet(now) {
        if (fctx) fctx.drawImage(sim, 0, 0);
        fadeUntil = now + FADE_MS;
        for (let y = 0, i = 0; y < sh; y++) {
            for (let x = 0; x < sw; x++, i++) { mapX[i] = x; mapY[i] = y; }
        }
        dtAcc = 0;
        framesSinceNorm = 0;
        rainCount = 0;
        tineSign = 1;
        cycleNext = 0;
        rubricationPending = true;
        opQueue.length = 0;
        stroke = null;
        rake.x = rake.y = -1e9;
        rake.vx = rake.vy = 0;
        rake.active = false;
        nextRain = now + CONFIG.rainFirst * 1000;
        nextTine = now + CONFIG.tineFirst * 1000;
        paintBand(0, CONFIG.rubrication);
        paletteChanged = true;
        record('sheet-reset', {}, now);
        console.log(simT(now) + ': new sheet');
    }

    function rotatePalette(now) {
        const period = CONFIG.paletteSeconds * 1000;
        const turns = Math.max(1, Math.floor((now - paletteAt) / period));
        paletteIdx = (paletteIdx + turns) % palNames.length;
        paletteAt += turns * period;
        cycleNext = 0;
        palColors[paletteIdx].forEach((hex, k) => paintBand(1 + k, hex));
        paintBand(0, rubricationPending ? CONFIG.rubrication : palColors[paletteIdx][0]);
        tintPage();
        paletteChanged = true;
        record('palette', { index: paletteIdx, name: palNames[paletteIdx] }, now);
        console.log(simT(now) + ': palette -> ' + palNames[paletteIdx]);
    }

    function schedule(now) {
        if (CONFIG.resetMinutes > 0 && now - sheetAt >= CONFIG.resetMinutes * 60000) {
            sheetAt = now;
            resetSheet(now);
        }
        if (now - paletteAt >= CONFIG.paletteSeconds * 1000) rotatePalette(now);
        if (CONFIG.rainMean > 0 && now >= nextRain) {
            const margin = CONFIG.rainMargin;
            const u = rand();
            opQueue.push({
                t: now,
                kind: 'drop',
                x: sw * (margin + rand() * (1 - 2 * margin)),
                y: sh * (margin + rand() * (1 - 2 * margin)),
                r: dropR * (0.55 + 1.8 * u * u * u),
                pigment: rainCount === 0 ? 0 : 1 + ((rand() * palColors[paletteIdx].length) | 0)
            });
            rainCount++;
            nextRain = now + exponentialDelay(CONFIG.rainMean);
        }
        if (CONFIG.tineMean > 0 && now >= nextTine) {
            opQueue.push({
                t: now,
                kind: 'tine',
                x: rand() * sw,
                y: rand() * sh,
                angle: rand() * Math.PI,
                m: tineSign * minDim * CONFIG.tineShift * (0.7 + 0.6 * rand()),
                lambda: minDim * CONFIG.tineFalloff,
                spacing: minDim * CONFIG.tineSpacing
            });
            tineSign = -tineSign;
            nextTine = now + exponentialDelay(CONFIG.tineMean);
        }
    }

    let ptrKx = sw / vw, ptrKy = sh / vh;
    function onResize() {
        if (!overlay) return;
        const rect = overlay.getBoundingClientRect();
        if (rect.width > 0) ptrKx = sw / rect.width;
        if (rect.height > 0) ptrKy = sh / rect.height;
    }

    function updateRake(event) {
        const now = performance.now();
        const x = event.clientX * ptrKx, y = event.clientY * ptrKy;
        if (rake.active) {
            const elapsed = Math.max(1, now - rake.lastT) / 1000;
            let vx = (x - rake.x) / elapsed, vy = (y - rake.y) / elapsed;
            const speed = Math.hypot(vx, vy);
            if (speed > CONFIG.rakeMax) { vx *= CONFIG.rakeMax / speed; vy *= CONFIG.rakeMax / speed; }
            rake.vx += (vx - rake.vx) * 0.5;
            rake.vy += (vy - rake.vy) * 0.5;
        }
        rake.x = x;
        rake.y = y;
        rake.lastT = now;
        rake.active = true;
    }

    function queueDropAt(clientX, clientY) {
        opQueue.push({
            t: performance.now(),
            kind: 'drop',
            x: clientX * ptrKx,
            y: clientY * ptrKy,
            r: dropR,
            pigment: 1 + cycleNext
        });
        cycleNext = (cycleNext + 1) % palColors[paletteIdx].length;
    }

    function consume(event) {
        if (event.cancelable) event.preventDefault();
        event.stopPropagation();
        if (typeof event.stopImmediatePropagation === 'function') event.stopImmediatePropagation();
    }

    function clearGesture(pointerId) {
        const gesture = gestures.get(pointerId);
        if (gesture) clearTimeout(gesture.timer);
        gestures.delete(pointerId);
        if (rakePointerId === pointerId) {
            const next = gestures.values().next();
            rakePointerId = next.done ? null : next.value.pointerId;
        }
    }

    function onPointerDown(event) {
        consume(event);
        const now = performance.now();
        const gesture = {
            pointerId: event.pointerId,
            type: event.pointerType,
            button: event.button,
            x: event.clientX,
            y: event.clientY,
            startX: event.clientX,
            startY: event.clientY,
            t: now,
            moved: false,
            consumed: false,
            timer: 0
        };
        gestures.set(event.pointerId, gesture);
        if (rakePointerId === null) rakePointerId = event.pointerId;
        try { overlay.setPointerCapture(event.pointerId); } catch (error) {}
        if (event.pointerType === 'touch') {
            lastTouchAt = now;
            gesture.timer = setTimeout(() => {
                const current = gestures.get(event.pointerId);
                if (current && !current.moved && !current.consumed) {
                    current.consumed = true;
                    queueDropAt(current.x, current.y);
                }
            }, 500);
        }
    }

    function onPointerMove(event) {
        consume(event);
        if (event.pointerType === 'mouse') resumeFromRuntimePause(performance.now());
        const gesture = gestures.get(event.pointerId);
        if (gesture) {
            gesture.x = event.clientX;
            gesture.y = event.clientY;
            if (!gesture.moved
                && Math.hypot(event.clientX - gesture.startX, event.clientY - gesture.startY) > 12) {
                gesture.moved = true;
                clearTimeout(gesture.timer);
            }
        }
        if (event.pointerType === 'mouse' || rakePointerId === event.pointerId) updateRake(event);
    }

    function onPointerUp(event) {
        consume(event);
        const gesture = gestures.get(event.pointerId);
        if (!gesture) return;
        clearTimeout(gesture.timer);
        const elapsed = performance.now() - gesture.t;
        if (event.pointerType === 'touch') {
            lastTouchAt = performance.now();
            if (!gesture.moved && !gesture.consumed) {
                if (elapsed >= 500) queueDropAt(gesture.x, gesture.y);
                else stop('touch-tap');
            }
        } else if (event.pointerType === 'mouse') {
            mouseReleaseMoved = gesture.moved;
        }
        try { overlay.releasePointerCapture(event.pointerId); } catch (error) {}
        clearGesture(event.pointerId);
    }

    function onPointerCancel(event) {
        consume(event);
        clearGesture(event.pointerId);
    }

    function onClick(event) {
        consume(event);
        if (performance.now() - lastTouchAt < 1000) return;
        const moved = mouseReleaseMoved;
        mouseReleaseMoved = false;
        if (!moved && event.button === 0) stop('click');
    }

    function onContextMenu(event) {
        consume(event);
        if (performance.now() - lastTouchAt < 1000) return;
        queueDropAt(event.clientX, event.clientY);
    }

    function onKey(event) {
        consume(event);
        if (event.key === 'Escape') stop('escape');
    }

    function drawComb(now) {
        if (!stroke || !octx) return;
        const progress = Math.min(1, Math.max(0, (now - stroke.t0) / stroke.dur));
        const eased = progress * progress * (3 - 2 * progress);
        const geometry = stroke.geometry;
        const sx = overlay.width / sw, sy = overlay.height / sh;
        const startX = stroke.x * sx, startY = stroke.y * sy;
        const shiftX = eased * stroke.m * geometry.mxd * sx;
        const shiftY = eased * stroke.m * geometry.myd * sy;
        const currentX = startX + shiftX, currentY = startY + shiftY;
        let ndx = geometry.nxd * sx, ndy = geometry.nyd * sy;
        const normalLength = Math.hypot(ndx, ndy) || 1;
        ndx /= normalLength; ndy /= normalLength;
        const length = Math.hypot(overlay.width, overlay.height) * 1.5;
        const alpha = progress < 0.8 ? 1 : (1 - progress) / 0.2;
        octx.save();
        octx.lineCap = 'round';
        for (const style of [
            ['rgba(255,255,255,0.5)', 4, 3],
            ['rgba(0,0,0,0.65)', 2, 1]
        ]) {
            octx.strokeStyle = octx.fillStyle = style[0];
            octx.globalAlpha = alpha;
            octx.lineWidth = style[1];
            octx.beginPath();
            octx.moveTo(currentX - ndx * length, currentY - ndy * length);
            octx.lineTo(currentX + ndx * length, currentY + ndy * length);
            octx.stroke();
            octx.lineWidth = style[2];
            const spacing = geometry.spacing;
            const kMax = geometry.single ? 0 : Math.min(2048, Math.ceil(Math.hypot(sw, sh) / spacing) + 3);
            for (let k = -kMax; k <= kMax; k++) {
                const offset = geometry.single ? 0 : k * spacing;
                const sx0 = stroke.x + geometry.nxd * offset;
                const sy0 = stroke.y + geometry.nyd * offset;
                const sx1 = sx0 + eased * stroke.m * geometry.mxd;
                const sy1 = sy0 + eased * stroke.m * geometry.myd;
                const x0 = sx0 * sx, y0 = sy0 * sy, x1 = sx1 * sx, y1 = sy1 * sy;
                if (x1 < -20 || x1 > overlay.width + 20 || y1 < -20 || y1 > overlay.height + 20) continue;
                octx.globalAlpha = alpha * 0.35;
                octx.beginPath();
                octx.moveTo(x0, y0);
                octx.lineTo(x1, y1);
                octx.stroke();
                octx.globalAlpha = alpha;
                octx.beginPath();
                octx.arc(x1, y1, style[1] + 1, 0, TAU);
                octx.fill();
            }
        }
        octx.restore();
    }

    function paintFrame(now, tineWarp) {
        sctx.putImageData(out, 0, 0);
        octx.drawImage(sim, 0, 0, overlay.width, overlay.height);
        if (now < fadeUntil && fctx) {
            octx.globalAlpha = (fadeUntil - now) / FADE_MS;
            octx.drawImage(fade, 0, 0, overlay.width, overlay.height);
            octx.globalAlpha = 1;
        }
        if (tineWarp) drawComb(now);
    }

    let lastSim = startT;
    let hiddenAt = document.hidden ? startT : null;
    let runtimePaused = false;
    let runUntil = Infinity;
    let pausedMs = 0;

    function resumeFromHidden(now) {
        if (hiddenAt === null) return;
        const pauseStarted = hiddenAt;
        const hiddenDuration = Math.max(0, now - pauseStarted);
        hiddenAt = null;

        // Hidden time is a true pause. Shifting every absolute deadline avoids
        // a palette/reset/rain/tine burst and preserves an in-flight comb or
        // new-sheet crossfade at exactly the phase where the tab disappeared.
        nextRain += hiddenDuration;
        nextTine += hiddenDuration;
        paletteAt += hiddenDuration;
        sheetAt += hiddenDuration;
        pausedMs += hiddenDuration;
        if (Number.isFinite(runUntil)) runUntil += hiddenDuration;
        if (stroke) stroke.t0 += hiddenDuration;
        if (fadeUntil > pauseStarted) fadeUntil += hiddenDuration;
        lastSim = now;
    }

    function resumeFromRuntimePause(now) {
        if (!runtimePaused || stopped || document.hidden) return;
        runtimePaused = false;
        resumeFromHidden(now);
        runUntil = now + CONFIG.runMinutes * 60000;
        record('resume', { reason: 'pointermove' }, now);
        if (running) raf = requestAnimationFrame(frame);
    }

    function onVisibilityChange() {
        const now = performance.now();
        if (document.hidden) {
            if (hiddenAt === null) hiddenAt = now;
        } else if (!runtimePaused) {
            resumeFromHidden(now);
        }
    }

    function runFrame(now) {
        const dt = Math.min(CONFIG.maxDt, Math.max(0, (now - lastSim) / 1000));
        lastSim = now;
        const t = (now - startT - pausedMs) / 1000;
        schedule(now);
        let opsApplied = paletteChanged;
        paletteChanged = false;

        for (let index = 0; index < opQueue.length;) {
            const operation = opQueue[index];
            if (now < operation.t) { index++; continue; }
            if (operation.kind === 'tine' && stroke) { index++; continue; }
            opQueue.splice(index, 1);
            if (operation.kind === 'drop') {
                // Three concentric deposits, with approximately the same total
                // deposited area as the original single drop. Use existing slots.
                const outer = parseHex(bandHex[operation.pigment]);
                let contrast = 1, bestDistance = -1;
                for (let slot = 1; slot < SLOTS; slot++) {
                    const rgb = parseHex(bandHex[slot]);
                    const distance = Math.abs((rgb[0] - outer[0]) * 77
                        + (rgb[1] - outer[1]) * 151 + (rgb[2] - outer[2]) * 28);
                    if (distance > bestDistance) { bestDistance = distance; contrast = slot; }
                }
                const radius = operation.r / Math.sqrt(1 + 0.64 ** 2 + 0.34 ** 2);
                applyDrop(operation.x, operation.y, radius, operation.pigment);
                applyDrop(operation.x, operation.y, radius * 0.64, contrast);
                applyDrop(operation.x, operation.y, radius * 0.34, operation.pigment);
                if (operation.pigment === 0 && rubricationPending) rubricationPending = false;
                record('drop', {
                    pigment: operation.pigment,
                    color: bandHex[operation.pigment],
                    radius: operation.r,
                    x: operation.x,
                    y: operation.y
                }, now);
                console.log(simT(now) + ': drop ' + bandHex[operation.pigment]
                    + ' r=' + operation.r.toFixed(0) + ' @ (' + operation.x.toFixed(0) + ', ' + operation.y.toFixed(0) + ')');
            } else {
                const geometry = makeTineGeometry(operation.x, operation.y, operation.angle, operation.spacing);
                record('tine', {
                    x: operation.x,
                    y: operation.y,
                    requestedAngle: operation.angle,
                    angle: Math.atan2(geometry.myd, geometry.mxd),
                    shift: operation.m,
                    spacing: geometry.single ? 0 : geometry.spacing,
                    nx: geometry.nx,
                    ny: geometry.ny
                }, now);
                console.log(simT(now) + ': tine comb @ '
                    + (Math.atan2(geometry.myd, geometry.mxd) * 180 / Math.PI).toFixed(0)
                    + 'deg, shift ' + operation.m.toFixed(0) + 'px, spacing '
                    + (geometry.single ? 'single' : geometry.spacing.toFixed(0) + 'px'));
                if (CONFIG.tineStrokeSec === 0) {
                    applyTine(geometry, operation.m, operation.lambda);
                } else {
                    stroke = Object.assign({}, operation, {
                        geometry,
                        t0: now,
                        dur: CONFIG.tineStrokeSec * 1000
                    });
                }
            }
            opsApplied = true;
        }

        const decay = Math.exp(-dt / CONFIG.rakeDecay);
        rake.vx *= decay;
        rake.vy *= decay;
        dtAcc += dt;
        updateVelocity(t);

        let tineWarp = null;
        let finishStroke = false;
        if (stroke) {
            const progress = Math.min(1, Math.max(0, (now - stroke.t0) / stroke.dur));
            const eased = progress * progress * (3 - 2 * progress);
            tineWarp = { geometry: stroke.geometry, amount: stroke.m * eased, lambda: stroke.lambda };
            finishStroke = progress >= 1;
        }

        if (finishStroke) {
            if (dtAcc > 0) {
                step(dtAcc, true, null);
                dtAcc = 0;
                if (++framesSinceNorm >= 300) { renormalize(); framesSinceNorm = 0; }
            }
            applyTine(stroke.geometry, stroke.m, stroke.lambda);
            stroke = null;
            step(0, false, null);
            paintFrame(now, null);
            return;
        }

        const maxVelocity = Math.sqrt(velMax2);
        const commit = maxVelocity * dtAcc >= CONFIG.stepPixels;
        const forceRender = !!tineWarp || opsApplied;
        if (commit) {
            const dtEffective = Math.min(dtAcc, maxVelocity > 0 ? 2 * CONFIG.stepPixels / maxVelocity : dtAcc);
            step(dtEffective, true, null);
            dtAcc = 0;
            if (++framesSinceNorm >= 300) { renormalize(); framesSinceNorm = 0; }
            if (tineWarp) step(0, false, tineWarp);
            paintFrame(now, tineWarp);
        } else if (CONFIG.smooth || forceRender) {
            step(dtAcc, false, tineWarp);
            paintFrame(now, tineWarp);
        }
    }

    function frame(now) {
        if (!running) return;
        if (document.hidden) {
            if (hiddenAt === null) hiddenAt = now;
            if (running) raf = requestAnimationFrame(frame);
            return;
        }
        resumeFromHidden(now);
        if (CONFIG.runMinutes > 0 && now >= runUntil) {
            runtimePaused = true;
            hiddenAt = now;
            raf = 0;
            record('pause', { reason: 'runtime-limit' }, now);
            return;
        }
        try {
            if (now - lastSim >= 1000 / CONFIG.fps - 2) runFrame(now);
        } catch (error) {
            console.error('marble: animation stopped after an error.', error);
            stop('error');
            return;
        }
        if (running) raf = requestAnimationFrame(frame);
    }

    function stop(reason) {
        if (stopped) return;
        stopped = true;
        running = false;
        if (raf) cancelAnimationFrame(raf);
        for (const gesture of gestures.values()) clearTimeout(gesture.timer);
        gestures.clear();
        while (localDisposers.length) {
            try { localDisposers.pop()(); } catch (error) {}
        }
        stopAliasRestore();
        if (ui) ui.cleanup();
        if (MARBLE_STATE.active === controller) MARBLE_STATE.active = null;
        record('stop', { reason: reason || 'user' }, performance.now());
        console.log('marble: stopped.');
    }
    Object.defineProperty(stop, MARBLE_OWNER, { value: true });

    try {
        // Finish the expensive first render before touching the host page, then
        // yield once so activity queued during capture/setup can cancel an idle
        // launch before the modal surface appears.
        updateVelocity(0);
        step(0, false, null);
        sctx.putImageData(out, 0, 0);
        await new Promise(resolve => setTimeout(resolve, 0));
        if (!alive()) return null;

        ui = createOverlayUi();
        overlay = ui.canvas;
        octx = ui.context;
        fade = document.createElement('canvas');
        fade.width = sw;
        fade.height = sh;
        fctx = fade.getContext('2d');
        if (!fctx) throw new Error('marble: 2D canvas is unavailable');

        const listen = (target, type, handler, optionsValue) => {
            target.addEventListener(type, handler, optionsValue);
            localDisposers.push(() => target.removeEventListener(type, handler, optionsValue));
        };
        listen(overlay, 'pointerdown', onPointerDown, true);
        listen(overlay, 'pointermove', onPointerMove, true);
        listen(overlay, 'pointerup', onPointerUp, true);
        listen(overlay, 'pointercancel', onPointerCancel, true);
        listen(overlay, 'lostpointercapture', onPointerCancel, true);
        listen(overlay, 'click', onClick, true);
        listen(overlay, 'contextmenu', onContextMenu, true);
        listen(overlay, 'auxclick', consume, true);
        listen(overlay, 'dblclick', consume, true);
        listen(ui.frameWindow, 'wheel', consume, { capture: true, passive: false });
        listen(ui.frameWindow, 'touchmove', consume, { capture: true, passive: false });
        listen(ui.frameWindow, 'dragstart', consume, true);
        listen(ui.frameWindow, 'selectstart', consume, true);
        listen(ui.frameWindow, 'keydown', onKey, true);
        listen(ui.frameWindow, 'keypress', consume, true);
        listen(ui.frameWindow, 'keyup', consume, true);
        listen(window, 'resize', onResize, true);
        listen(document, 'visibilitychange', onVisibilityChange, true);
        listen(ui.dialog, 'cancel', event => { consume(event); stop('escape'); }, true);
        listen(ui.dialog, 'close', () => { if (running) stop('dialog-close'); }, true);

        if (!alive()) {
            while (localDisposers.length) localDisposers.pop()();
            ui.cleanup();
            return null;
        }

        octx.drawImage(sim, 0, 0, overlay.width, overlay.height);
        ui.show();
        onResize();

        controller = {
            stop,
            canvas: overlay,
            dialog: ui.dialog,
            iframe: ui.iframe,
            config: CONFIG,
            dimensions: { viewportWidth: vw, viewportHeight: vh, simulationWidth: sw, simulationHeight: sh, scale },
            seed: seedUsed,
            events,
            get running() { return running; },
            get paused() { return runtimePaused; }
        };
        MARBLE_STATE.active = controller;
        stopAliasRestore = marbleInstallTemporaryAlias('__marbleStop', stop);
        running = true;
        runUntil = CONFIG.runMinutes > 0 ? performance.now() + CONFIG.runMinutes * 60000 : Infinity;
        record('start', { seed: seedUsed, sw, sh, scale }, performance.now());
        console.log('marble: running at ' + sw + 'x' + sh + ' (scale ' + scale.toFixed(2)
            + ', ' + CONFIG.fps + 'fps cap), seed ' + seedUsed + '. Move/drag = rake; '
            + 'right-click (touch: long-press) = drop pigment; click (touch: tap) or ESC = stop.');
        raf = requestAnimationFrame(frame);
        return controller;
    } catch (error) {
        running = false;
        if (raf) cancelAnimationFrame(raf);
        while (localDisposers.length) {
            try { localDisposers.pop()(); } catch (cleanupError) {}
        }
        try { stopAliasRestore(); } catch (cleanupError) {}
        if (ui) ui.cleanup();
        if (MARBLE_STATE.active === controller) MARBLE_STATE.active = null;
        throw error;
    }
}

function marbleStartWithActivityCancellation(ticket) {
    let valid = !ticket || ticket.valid !== false;
    const listeners = [];
    const cancel = () => {
        valid = false;
        if (MARBLE_STATE.active) MARBLE_STATE.active.stop('activity-during-start');
        else marbleCancelPendingStart();
    };
    const add = (target, type) => {
        target.addEventListener(type, cancel, { capture: true, passive: true });
        listeners.push(() => target.removeEventListener(type, cancel, true));
    };
    for (const type of ['pointermove', 'pointerdown', 'keydown', 'wheel', 'scroll', 'touchstart']) add(window, type);
    add(document, 'visibilitychange');
    const motion = window.matchMedia && matchMedia('(prefers-reduced-motion: reduce)');
    if (motion) {
        if (typeof motion.addEventListener === 'function') {
            motion.addEventListener('change', cancel);
            listeners.push(() => motion.removeEventListener('change', cancel));
        } else if (typeof motion.addListener === 'function') {
            motion.addListener(cancel);
            listeners.push(() => motion.removeListener(cancel));
        }
    }
    const guard = () => valid
        && (!ticket || ticket.valid !== false)
        && !document.hidden
        && !(motion && motion.matches);
    return marbleStart({ guard })
        // Keep the activity sentries for one more task. This catches input that
        // arrived during a long synchronous setup pass but could not dispatch
        // until immediately after the controller was published.
        .then(result => new Promise(resolve => setTimeout(() => resolve(result), 0)))
        .finally(() => {
            while (listeners.length) listeners.pop()();
            if (ticket && globalThis[MARBLE_IDLE_TICKET_KEY] === ticket) {
                try { delete globalThis[MARBLE_IDLE_TICKET_KEY]; } catch (error) {}
            }
        });
}

function marbleDispatch(script) {
    const dataset = script && script.dataset ? script.dataset : {};
    const screensaver = 'screensaver' in dataset;
    const idleTriggered = 'marbleIdleStart' in dataset;
    if (idleTriggered) {
        marbleStartWithActivityCancellation(globalThis[MARBLE_IDLE_TICKET_KEY]).catch(error => console.error(error));
        return;
    }
    if (!screensaver) {
        marbleStart().catch(error => console.error(error));
        return;
    }
    if (window !== window.top || MARBLE_STATE.idleArmed) return;
    MARBLE_STATE.idleArmed = true;

    const IDLE_MS = 24 * 60 * 60 * 1000;
    const CHECK_MS = 60 * 1000;
    let lastActivity = Date.now();
    let activityEpoch = 0;
    function activity() {
        lastActivity = Date.now();
        activityEpoch++;
        marbleCancelPendingStart();
    }
    for (const type of ['pointermove', 'pointerdown', 'keydown', 'wheel', 'scroll', 'touchstart']) {
        window.addEventListener(type, activity, { passive: true, capture: true });
    }
    document.addEventListener('visibilitychange', activity);

    setInterval(() => {
        const motion = window.matchMedia && matchMedia('(prefers-reduced-motion: reduce)').matches;
        if (document.hidden || MARBLE_STATE.active || MARBLE_STATE.starting || motion
            || Date.now() - lastActivity < IDLE_MS) return;
        const epoch = activityEpoch;
        lastActivity = Date.now();
        marbleStart({
            guard: () => activityEpoch === epoch
                && !document.hidden
                && !(window.matchMedia && matchMedia('(prefers-reduced-motion: reduce)').matches)
        }).catch(error => console.error(error));
    }, CHECK_MS);
}

const MARBLE_API = {
    start: marbleStart,
    stop: marbleStopActive,
    cancelStart: marbleCancelPendingStart,
    dispatch: marbleDispatch,
    defaults: MARBLE_DEFAULT_CONFIG,
    get active() { return MARBLE_STATE.active; },
    get starting() { return !!MARBLE_STATE.starting; }
};
Object.defineProperty(MARBLE_API, MARBLE_OWNER, { value: true });
Object.defineProperty(globalThis, MARBLE_RUNTIME_KEY, {
    configurable: true,
    enumerable: false,
    writable: false,
    value: MARBLE_API
});

if (!Object.prototype.hasOwnProperty.call(window, 'paperMarble')) {
    try {
        Object.defineProperty(window, 'paperMarble', {
            configurable: true,
            enumerable: false,
            writable: false,
            value: MARBLE_API
        });
    } catch (error) {
        console.warn('marble: could not expose window.paperMarble: ' + error.message);
    }
} else if (window.paperMarble !== MARBLE_API) {
    console.warn('marble: leaving pre-existing window.paperMarble untouched; the API remains at Symbol.for("gwern.paper-marble.runtime").');
}
marbleInstallPersistentAlias('__marbleStart', marbleStart);
if (!Object.prototype.hasOwnProperty.call(window, '__marbleStarting')) {
    try {
        Object.defineProperty(window, '__marbleStarting', {
            configurable: true,
            enumerable: false,
            get: () => !!MARBLE_STATE.starting
        });
    } catch (error) {}
}

marbleDispatch(MARBLE_LOADING_SCRIPT);


})();
