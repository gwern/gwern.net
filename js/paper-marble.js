// paper-marble.js: ink-marble the current web page (an idle 'screensaver' toy).
// Author: Gwern Branwen, Claude-5-Fable
// Date: 2026-07-07
// When:  Time-stamp: "2026-07-08 11:19:53 gwern"
// License: CC-0 (except the vendored html-to-image library: MIT, notice below)
//
// Turns the current viewport into a sheet of paper being marbled (<https://en.wikipedia.org/wiki/Paper_marbling>).
// Associated with fine typography and old books.
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
// USE: 3 ways (fully self-contained: no network access required in any mode):
//  (1) Run now: paste this entire file into the DevTools console of any page,
//      or include it as a plain `<script src=...>`: it marbles on load.
//  (2) PRODUCTION idle screensaver (the Gwern.net deployment): do NOT
//      reference this file from any HTML. Instead, inline the tiny companion
//      stub (`marble-screensaver.js`, ~40 lines) into the site's existing JS
//      bundle; after 1 idle hour it injects a plain
//      `<script src="/static/js/paper-marble.js">`, which runs on load per
//      (1). Regular readers thus pay a few hundred gzipped bytes of stub and
//      never download or parse this file at all.
//  (3) Single-file idle screensaver, for pages that do not mind the weight:
//          <script src="/static/js/paper-marble.js" data-screensaver async></script>
//      arms a 1-hour no-activity timer instead of running (visible top-level
//      tabs only; honors `prefers-reduced-motion`, re-checked at trigger
//      time). NB: `async` only unblocks parsing—this still DOWNLOADS and
//      parses the whole file on every page view, so prefer (b) for any
//      production site. See the DISPATCHER comment at the end of this file.
//  (4) Run anywhere in DevTools console: `document.head.appendChild(Object.assign(document.createElement('script'), {src: 'https://gwern.net/static/js/paper-marble.js'}));`;
//      then `__marbleStart()` to reset.
//
// CONTROLS: [drag mouse] = rake; [right-click] (touch: [long-press]) = drop pigment;
// [left-click] (touch: [tap]) / [ESC] / `window.__marbleStop()` = stop & restore.
//
// REPRODUCIBILITY: all stochastic choices flow from one seeded PRNG
// (mulberry32; `CONFIG.seed`, logged at startup), and every event (drop, tine
// stroke, palette rotation, sheet reset) is logged with its simulation
// timestamp. A session can therefore be replayed from its log; the seed alone
// reproduces a statistically identical session (event *timing* still follows
// wall-clock frame timing, so it is not bit-identical).
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
// shadow-wrapped so it always attaches to `globalThis`, even on AMD pages);
// (2) the marbling widget, defined as `window.__marbleStart()`—defining it
// does NOT run it; (3) a dispatcher that either runs it immediately or arms
// the idle-screensaver timer, depending on how this file was included.

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
(function () {
    // shadow any page module system so the UMD banner below always takes the
    // global branch and attaches globalThis.htmlToImage
    var define, exports, module;
!function(t,e){"object"==typeof exports&&"undefined"!=typeof module?e(exports):"function"==typeof define&&define.amd?define(["exports"],e):e((t="undefined"!=typeof globalThis?globalThis:t||self).htmlToImage={})}(this,(function(t){"use strict";function e(t,e,n,r){return new(n||(n=Promise))((function(i,o){function u(t){try{a(r.next(t))}catch(t){o(t)}}function c(t){try{a(r.throw(t))}catch(t){o(t)}}function a(t){var e;t.done?i(t.value):(e=t.value,e instanceof n?e:new n((function(t){t(e)}))).then(u,c)}a((r=r.apply(t,e||[])).next())}))}function n(t,e){var n,r,i,o,u={label:0,sent:function(){if(1&i[0])throw i[1];return i[1]},trys:[],ops:[]};return o={next:c(0),throw:c(1),return:c(2)},"function"==typeof Symbol&&(o[Symbol.iterator]=function(){return this}),o;function c(c){return function(a){return function(c){if(n)throw new TypeError("Generator is already executing.");for(;o&&(o=0,c[0]&&(u=0)),u;)try{if(n=1,r&&(i=2&c[0]?r.return:c[0]?r.throw||((i=r.return)&&i.call(r),0):r.next)&&!(i=i.call(r,c[1])).done)return i;switch(r=0,i&&(c=[2&c[0],i.value]),c[0]){case 0:case 1:i=c;break;case 4:return u.label++,{value:c[1],done:!1};case 5:u.label++,r=c[1],c=[0];continue;case 7:c=u.ops.pop(),u.trys.pop();continue;default:if(!(i=u.trys,(i=i.length>0&&i[i.length-1])||6!==c[0]&&2!==c[0])){u=0;continue}if(3===c[0]&&(!i||c[1]>i[0]&&c[1]<i[3])){u.label=c[1];break}if(6===c[0]&&u.label<i[1]){u.label=i[1],i=c;break}if(i&&u.label<i[2]){u.label=i[2],u.ops.push(c);break}i[2]&&u.ops.pop(),u.trys.pop();continue}c=e.call(t,u)}catch(t){c=[6,t],r=0}finally{n=i=0}if(5&c[0])throw c[1];return{value:c[0]?c[1]:void 0,done:!0}}([c,a])}}}var r,i=(r=0,function(){return r+=1,"u".concat("0000".concat((Math.random()*Math.pow(36,4)<<0).toString(36)).slice(-4)).concat(r)});function o(t){for(var e=[],n=0,r=t.length;n<r;n++)e.push(t[n]);return e}function u(t,e){var n=(t.ownerDocument.defaultView||window).getComputedStyle(t).getPropertyValue(e);return n?parseFloat(n.replace("px","")):0}function c(t,e){void 0===e&&(e={});var n,r,i,o=e.width||(r=u(n=t,"border-left-width"),i=u(n,"border-right-width"),n.clientWidth+r+i),c=e.height||function(t){var e=u(t,"border-top-width"),n=u(t,"border-bottom-width");return t.clientHeight+e+n}(t);return{width:o,height:c}}var a=16384;function s(t,e){return void 0===e&&(e={}),t.toBlob?new Promise((function(n){t.toBlob(n,e.type?e.type:"image/png",e.quality?e.quality:1)})):new Promise((function(n){for(var r=window.atob(t.toDataURL(e.type?e.type:void 0,e.quality?e.quality:void 0).split(",")[1]),i=r.length,o=new Uint8Array(i),u=0;u<i;u+=1)o[u]=r.charCodeAt(u);n(new Blob([o],{type:e.type?e.type:"image/png"}))}))}function l(t){return new Promise((function(e,n){var r=new Image;r.decode=function(){return e(r)},r.onload=function(){return e(r)},r.onerror=n,r.crossOrigin="anonymous",r.decoding="async",r.src=t}))}function f(t){return e(this,void 0,void 0,(function(){return n(this,(function(e){return[2,Promise.resolve().then((function(){return(new XMLSerializer).serializeToString(t)})).then(encodeURIComponent).then((function(t){return"data:image/svg+xml;charset=utf-8,".concat(t)}))]}))}))}function h(t,r,i){return e(this,void 0,void 0,(function(){var e,o,u;return n(this,(function(n){return e="http://www.w3.org/2000/svg",o=document.createElementNS(e,"svg"),u=document.createElementNS(e,"foreignObject"),o.setAttribute("width","".concat(r)),o.setAttribute("height","".concat(i)),o.setAttribute("viewBox","0 0 ".concat(r," ").concat(i)),u.setAttribute("width","100%"),u.setAttribute("height","100%"),u.setAttribute("x","0"),u.setAttribute("y","0"),u.setAttribute("externalResourcesRequired","true"),o.appendChild(u),u.appendChild(t),[2,f(o)]}))}))}var d=function(t,e){if(t instanceof e)return!0;var n=Object.getPrototypeOf(t);return null!==n&&(n.constructor.name===e.name||d(n,e))};function v(t,e,n){var r=".".concat(t,":").concat(e),i=n.cssText?function(t){var e=t.getPropertyValue("content");return"".concat(t.cssText," content: '").concat(e.replace(/'|"/g,""),"';")}(n):function(t){return o(t).map((function(e){var n=t.getPropertyValue(e),r=t.getPropertyPriority(e);return"".concat(e,": ").concat(n).concat(r?" !important":"",";")})).join(" ")}(n);return document.createTextNode("".concat(r,"{").concat(i,"}"))}function p(t,e,n){var r=window.getComputedStyle(t,n),o=r.getPropertyValue("content");if(""!==o&&"none"!==o){var u=i();try{e.className="".concat(e.className," ").concat(u)}catch(t){return}var c=document.createElement("style");c.appendChild(v(u,n,r)),e.appendChild(c)}}var g="application/font-woff",m="image/jpeg",w={woff:g,woff2:g,ttf:"application/font-truetype",eot:"application/vnd.ms-fontobject",png:"image/png",jpg:m,jpeg:m,gif:"image/gif",tiff:"image/tiff",svg:"image/svg+xml",webp:"image/webp"};function b(t){var e=function(t){var e=/\.([^./]*?)$/g.exec(t);return e?e[1]:""}(t).toLowerCase();return w[e]||""}function y(t){return-1!==t.search(/^(data:)/)}function x(t,e){return"data:".concat(e,";base64,").concat(t)}function S(t,r,i){return e(this,void 0,void 0,(function(){var e,o;return n(this,(function(n){switch(n.label){case 0:return[4,fetch(t,r)];case 1:if(404===(e=n.sent()).status)throw new Error('Resource "'.concat(e.url,'" not found'));return[4,e.blob()];case 2:return o=n.sent(),[2,new Promise((function(t,n){var r=new FileReader;r.onerror=n,r.onloadend=function(){try{t(i({res:e,result:r.result}))}catch(t){n(t)}},r.readAsDataURL(o)}))]}}))}))}var E={};function C(t,r,i){return e(this,void 0,void 0,(function(){var e,o,u,c,a;return n(this,(function(n){switch(n.label){case 0:if(e=function(t,e,n){var r=t.replace(/\?.*/,"");return n&&(r=t),/ttf|otf|eot|woff2?/i.test(r)&&(r=r.replace(/.*\//,"")),e?"[".concat(e,"]").concat(r):r}(t,r,i.includeQueryParams),null!=E[e])return[2,E[e]];i.cacheBust&&(t+=(/\?/.test(t)?"&":"?")+(new Date).getTime()),n.label=1;case 1:return n.trys.push([1,3,,4]),[4,S(t,i.fetchRequestInit,(function(t){var e=t.res,n=t.result;return r||(r=e.headers.get("Content-Type")||""),function(t){return t.split(/,/)[1]}(n)}))];case 2:return u=n.sent(),o=x(u,r),[3,4];case 3:return c=n.sent(),o=i.imagePlaceholder||"",a="Failed to fetch resource: ".concat(t),c&&(a="string"==typeof c?c:c.message),a&&console.warn(a),[3,4];case 4:return E[e]=o,[2,o]}}))}))}function P(t){return e(this,void 0,void 0,(function(){var e;return n(this,(function(n){return"data:,"===(e=t.toDataURL())?[2,t.cloneNode(!1)]:[2,l(e)]}))}))}function R(t,r){return e(this,void 0,void 0,(function(){var e,i,o,u;return n(this,(function(n){switch(n.label){case 0:return t.currentSrc?(e=document.createElement("canvas"),i=e.getContext("2d"),e.width=t.clientWidth,e.height=t.clientHeight,null==i||i.drawImage(t,0,0,e.width,e.height),[2,l(e.toDataURL())]):(o=t.poster,u=b(o),[4,C(o,u,r)]);case 1:return[2,l(n.sent())]}}))}))}function T(t){var r;return e(this,void 0,void 0,(function(){return n(this,(function(e){switch(e.label){case 0:return e.trys.push([0,3,,4]),(null===(r=null==t?void 0:t.contentDocument)||void 0===r?void 0:r.body)?[4,L(t.contentDocument.body,{},!0)]:[3,2];case 1:return[2,e.sent()];case 2:return[3,4];case 3:return e.sent(),[3,4];case 4:return[2,t.cloneNode(!1)]}}))}))}function A(t,e){return d(e,Element)&&(function(t,e){var n=e.style;if(n){var r=window.getComputedStyle(t);r.cssText?(n.cssText=r.cssText,n.transformOrigin=r.transformOrigin):o(r).forEach((function(i){var o=r.getPropertyValue(i);if("font-size"===i&&o.endsWith("px")){var u=Math.floor(parseFloat(o.substring(0,o.length-2)))-.1;o="".concat(u,"px")}d(t,HTMLIFrameElement)&&"display"===i&&"inline"===o&&(o="block"),"d"===i&&e.getAttribute("d")&&(o="path(".concat(e.getAttribute("d"),")")),n.setProperty(i,o,r.getPropertyPriority(i))}))}}(t,e),function(t,e){p(t,e,":before"),p(t,e,":after")}(t,e),function(t,e){d(t,HTMLTextAreaElement)&&(e.innerHTML=t.value),d(t,HTMLInputElement)&&e.setAttribute("value",t.value)}(t,e),function(t,e){if(d(t,HTMLSelectElement)){var n=e,r=Array.from(n.children).find((function(e){return t.value===e.getAttribute("value")}));r&&r.setAttribute("selected","")}}(t,e)),e}function L(t,r,i){return e(this,void 0,void 0,(function(){return n(this,(function(u){return i||!r.filter||r.filter(t)?[2,Promise.resolve(t).then((function(t){return function(t,r){return e(this,void 0,void 0,(function(){return n(this,(function(e){return d(t,HTMLCanvasElement)?[2,P(t)]:d(t,HTMLVideoElement)?[2,R(t,r)]:d(t,HTMLIFrameElement)?[2,T(t)]:[2,t.cloneNode(!1)]}))}))}(t,r)})).then((function(i){return function(t,r,i){var u,c;return e(this,void 0,void 0,(function(){var e;return n(this,(function(n){switch(n.label){case 0:return e=[],0===(e=null!=(a=t).tagName&&"SLOT"===a.tagName.toUpperCase()&&t.assignedNodes?o(t.assignedNodes()):d(t,HTMLIFrameElement)&&(null===(u=t.contentDocument)||void 0===u?void 0:u.body)?o(t.contentDocument.body.childNodes):o((null!==(c=t.shadowRoot)&&void 0!==c?c:t).childNodes)).length||d(t,HTMLVideoElement)?[2,r]:[4,e.reduce((function(t,e){return t.then((function(){return L(e,i)})).then((function(t){t&&r.appendChild(t)}))}),Promise.resolve())];case 1:return n.sent(),[2,r]}var a}))}))}(t,i,r)})).then((function(e){return A(t,e)})).then((function(t){return function(t,r){return e(this,void 0,void 0,(function(){var e,i,o,u,c,a,s,l,f,h,d,v,p;return n(this,(function(n){switch(n.label){case 0:if(0===(e=t.querySelectorAll?t.querySelectorAll("use"):[]).length)return[2,t];i={},p=0,n.label=1;case 1:return p<e.length?(o=e[p],(u=o.getAttribute("xlink:href"))?(c=t.querySelector(u),a=document.querySelector(u),c||!a||i[u]?[3,3]:(s=i,l=u,[4,L(a,r,!0)])):[3,3]):[3,4];case 2:s[l]=n.sent(),n.label=3;case 3:return p++,[3,1];case 4:if((f=Object.values(i)).length){for(h="http://www.w3.org/1999/xhtml",(d=document.createElementNS(h,"svg")).setAttribute("xmlns",h),d.style.position="absolute",d.style.width="0",d.style.height="0",d.style.overflow="hidden",d.style.display="none",v=document.createElementNS(h,"defs"),d.appendChild(v),p=0;p<f.length;p++)v.appendChild(f[p]);t.appendChild(d)}return[2,t]}}))}))}(t,r)}))]:[2,null]}))}))}var N=/url\((['"]?)([^'"]+?)\1\)/g,k=/url\([^)]+\)\s*format\((["']?)([^"']+)\1\)/g,I=/src:\s*(?:url\([^)]+\)\s*format\([^)]+\)[,;]\s*)+/g;function D(t,r,i,o,u){return e(this,void 0,void 0,(function(){var e,c,a,s;return n(this,(function(n){switch(n.label){case 0:return n.trys.push([0,5,,6]),e=i?function(t,e){if(t.match(/^[a-z]+:\/\//i))return t;if(t.match(/^\/\//))return window.location.protocol+t;if(t.match(/^[a-z]+:/i))return t;var n=document.implementation.createHTMLDocument(),r=n.createElement("base"),i=n.createElement("a");return n.head.appendChild(r),n.body.appendChild(i),e&&(r.href=e),i.href=t,i.href}(r,i):r,c=b(r),a=void 0,u?[4,u(e)]:[3,2];case 1:return s=n.sent(),a=x(s,c),[3,4];case 2:return[4,C(e,c,o)];case 3:a=n.sent(),n.label=4;case 4:return[2,t.replace((l=r,f=l.replace(/([.*+?^${}()|\[\]\/\\])/g,"\\$1"),new RegExp("(url\\(['\"]?)(".concat(f,")(['\"]?\\))"),"g")),"$1".concat(a,"$3"))];case 5:return n.sent(),[3,6];case 6:return[2,t]}var l,f}))}))}function M(t){return-1!==t.search(N)}function H(t,r,i){return e(this,void 0,void 0,(function(){var e,o;return n(this,(function(n){return M(t)?(e=function(t,e){var n=e.preferredFontFormat;return n?t.replace(I,(function(t){for(;;){var e=k.exec(t)||[],r=e[0],i=e[2];if(!i)return"";if(i===n)return"src: ".concat(r,";")}})):t}(t,i),o=function(t){var e=[];return t.replace(N,(function(t,n,r){return e.push(r),t})),e.filter((function(t){return!y(t)}))}(e),[2,o.reduce((function(t,e){return t.then((function(t){return D(t,e,r,i)}))}),Promise.resolve(e))]):[2,t]}))}))}function V(t,r,i){var o;return e(this,void 0,void 0,(function(){var e,u;return n(this,(function(n){switch(n.label){case 0:return(e=null===(o=r.style)||void 0===o?void 0:o.getPropertyValue(t))?[4,H(e,null,i)]:[3,2];case 1:return u=n.sent(),r.style.setProperty(t,u,r.style.getPropertyPriority(t)),[2,!0];case 2:return[2,!1]}}))}))}function F(t,r){return e(this,void 0,void 0,(function(){return n(this,(function(e){switch(e.label){case 0:return[4,V("background",t,r)];case 1:return e.sent()?[3,3]:[4,V("background-image",t,r)];case 2:e.sent(),e.label=3;case 3:return[4,V("mask",t,r)];case 4:return e.sent()?[3,6]:[4,V("mask-image",t,r)];case 5:e.sent(),e.label=6;case 6:return[2]}}))}))}function j(t,r){return e(this,void 0,void 0,(function(){var e,i,o;return n(this,(function(n){switch(n.label){case 0:return(e=d(t,HTMLImageElement))&&!y(t.src)||d(t,SVGImageElement)&&!y(t.href.baseVal)?[4,C(i=e?t.src:t.href.baseVal,b(i),r)]:[2];case 1:return o=n.sent(),[4,new Promise((function(n,r){t.onload=n,t.onerror=r;var i=t;i.decode&&(i.decode=n),"lazy"===i.loading&&(i.loading="eager"),e?(t.srcset="",t.src=o):t.href.baseVal=o}))];case 2:return n.sent(),[2]}}))}))}function q(t,r){return e(this,void 0,void 0,(function(){var e,i;return n(this,(function(n){switch(n.label){case 0:return e=o(t.childNodes),i=e.map((function(t){return U(t,r)})),[4,Promise.all(i).then((function(){return t}))];case 1:return n.sent(),[2]}}))}))}function U(t,r){return e(this,void 0,void 0,(function(){return n(this,(function(e){switch(e.label){case 0:return d(t,Element)?[4,F(t,r)]:[3,4];case 1:return e.sent(),[4,j(t,r)];case 2:return e.sent(),[4,q(t,r)];case 3:e.sent(),e.label=4;case 4:return[2]}}))}))}var O={};function B(t){return e(this,void 0,void 0,(function(){var e,r;return n(this,(function(n){switch(n.label){case 0:return null!=(e=O[t])?[2,e]:[4,fetch(t)];case 1:return[4,n.sent().text()];case 2:return r=n.sent(),e={url:t,cssText:r},O[t]=e,[2,e]}}))}))}function z(t,r){return e(this,void 0,void 0,(function(){var i,o,u,c,a=this;return n(this,(function(s){return i=t.cssText,o=/url\(["']?([^"')]+)["']?\)/g,u=i.match(/url\([^)]+\)/g)||[],c=u.map((function(u){return e(a,void 0,void 0,(function(){var e;return n(this,(function(n){return(e=u.replace(o,"$1")).startsWith("https://")||(e=new URL(e,t.url).href),[2,S(e,r.fetchRequestInit,(function(t){var e=t.result;return i=i.replace(u,"url(".concat(e,")")),[u,e]}))]}))}))})),[2,Promise.all(c).then((function(){return i}))]}))}))}function W(t){if(null==t)return[];for(var e=[],n=t.replace(/(\/\*[\s\S]*?\*\/)/gi,""),r=new RegExp("((@.*?keyframes [\\s\\S]*?){([\\s\\S]*?}\\s*?)})","gi");;){if(null===(u=r.exec(n)))break;e.push(u[0])}n=n.replace(r,"");for(var i=/@import[\s\S]*?url\([^)]*\)[\s\S]*?;/gi,o=new RegExp("((\\s*?(?:\\/\\*[\\s\\S]*?\\*\\/)?\\s*?@media[\\s\\S]*?){([\\s\\S]*?)}\\s*?})|(([\\s\\S]*?){([\\s\\S]*?)})","gi");;){var u;if(null===(u=i.exec(n))){if(null===(u=o.exec(n)))break;i.lastIndex=o.lastIndex}else o.lastIndex=i.lastIndex;e.push(u[0])}return e}function $(t,r){return e(this,void 0,void 0,(function(){var e,i;return n(this,(function(n){return e=[],i=[],t.forEach((function(e){if("cssRules"in e)try{o(e.cssRules||[]).forEach((function(t,n){if(t.type===CSSRule.IMPORT_RULE){var o=n+1,u=B(t.href).then((function(t){return z(t,r)})).then((function(t){return W(t).forEach((function(t){try{e.insertRule(t,t.startsWith("@import")?o+=1:e.cssRules.length)}catch(e){console.error("Error inserting rule from remote css",{rule:t,error:e})}}))})).catch((function(t){console.error("Error loading remote css",t.toString())}));i.push(u)}}))}catch(o){var n=t.find((function(t){return null==t.href}))||document.styleSheets[0];null!=e.href&&i.push(B(e.href).then((function(t){return z(t,r)})).then((function(t){return W(t).forEach((function(t){n.insertRule(t,e.cssRules.length)}))})).catch((function(t){console.error("Error loading remote stylesheet",t)}))),console.error("Error inlining remote css file",o)}})),[2,Promise.all(i).then((function(){return t.forEach((function(t){if("cssRules"in t)try{o(t.cssRules||[]).forEach((function(t){e.push(t)}))}catch(e){console.error("Error while reading CSS rules from ".concat(t.href),e)}})),e}))]}))}))}function _(t){return t.filter((function(t){return t.type===CSSRule.FONT_FACE_RULE})).filter((function(t){return M(t.style.getPropertyValue("src"))}))}function G(t,r){return e(this,void 0,void 0,(function(){return n(this,(function(e){switch(e.label){case 0:if(null==t.ownerDocument)throw new Error("Provided element is not within a Document");return[4,$(o(t.ownerDocument.styleSheets),r)];case 1:return[2,_(e.sent())]}}))}))}function J(t,r){return e(this,void 0,void 0,(function(){var e;return n(this,(function(n){switch(n.label){case 0:return[4,G(t,r)];case 1:return e=n.sent(),[4,Promise.all(e.map((function(t){var e=t.parentStyleSheet?t.parentStyleSheet.href:null;return H(t.cssText,e,r)})))];case 2:return[2,n.sent().join("\n")]}}))}))}function Q(t,r){return e(this,void 0,void 0,(function(){var e,i,o,u,c;return n(this,(function(n){switch(n.label){case 0:return null==r.fontEmbedCSS?[3,1]:(i=r.fontEmbedCSS,[3,5]);case 1:return r.skipFonts?(o=null,[3,4]):[3,2];case 2:return[4,J(t,r)];case 3:o=n.sent(),n.label=4;case 4:i=o,n.label=5;case 5:return(e=i)&&(u=document.createElement("style"),c=document.createTextNode(e),u.appendChild(c),t.firstChild?t.insertBefore(u,t.firstChild):t.appendChild(u)),[2]}}))}))}function X(t,r){return void 0===r&&(r={}),e(this,void 0,void 0,(function(){var e,i,o,u;return n(this,(function(n){switch(n.label){case 0:return e=c(t,r),i=e.width,o=e.height,[4,L(t,r,!0)];case 1:return[4,Q(u=n.sent(),r)];case 2:return n.sent(),[4,U(u,r)];case 3:return n.sent(),function(t,e){var n=t.style;e.backgroundColor&&(n.backgroundColor=e.backgroundColor),e.width&&(n.width="".concat(e.width,"px")),e.height&&(n.height="".concat(e.height,"px"));var r=e.style;null!=r&&Object.keys(r).forEach((function(t){n[t]=r[t]}))}(u,r),[4,h(u,i,o)];case 4:return[2,n.sent()]}}))}))}function K(t,r){return void 0===r&&(r={}),e(this,void 0,void 0,(function(){var e,i,o,u,s,f,h,d,v;return n(this,(function(n){switch(n.label){case 0:return e=c(t,r),i=e.width,o=e.height,[4,X(t,r)];case 1:return[4,l(n.sent())];case 2:return u=n.sent(),s=document.createElement("canvas"),f=s.getContext("2d"),h=r.pixelRatio||function(){var t,e;try{e=process}catch(t){}var n=e&&e.env?e.env.devicePixelRatio:null;return n&&(t=parseInt(n,10),Number.isNaN(t)&&(t=1)),t||window.devicePixelRatio||1}(),d=r.canvasWidth||i,v=r.canvasHeight||o,s.width=d*h,s.height=v*h,r.skipAutoScale||function(t){(t.width>a||t.height>a)&&(t.width>a&&t.height>a?t.width>t.height?(t.height*=a/t.width,t.width=a):(t.width*=a/t.height,t.height=a):t.width>a?(t.height*=a/t.width,t.width=a):(t.width*=a/t.height,t.height=a))}(s),s.style.width="".concat(d),s.style.height="".concat(v),r.backgroundColor&&(f.fillStyle=r.backgroundColor,f.fillRect(0,0,s.width,s.height)),f.drawImage(u,0,0,s.width,s.height),[2,s]}}))}))}t.getFontEmbedCSS=function(t,r){return void 0===r&&(r={}),e(this,void 0,void 0,(function(){return n(this,(function(e){return[2,J(t,r)]}))}))},t.toBlob=function(t,r){return void 0===r&&(r={}),e(this,void 0,void 0,(function(){return n(this,(function(e){switch(e.label){case 0:return[4,K(t,r)];case 1:return[4,s(e.sent())];case 2:return[2,e.sent()]}}))}))},t.toCanvas=K,t.toJpeg=function(t,r){return void 0===r&&(r={}),e(this,void 0,void 0,(function(){return n(this,(function(e){switch(e.label){case 0:return[4,K(t,r)];case 1:return[2,e.sent().toDataURL("image/jpeg",r.quality||1)]}}))}))},t.toPixelData=function(t,r){return void 0===r&&(r={}),e(this,void 0,void 0,(function(){var e,i,o,u;return n(this,(function(n){switch(n.label){case 0:return e=c(t,r),i=e.width,o=e.height,[4,K(t,r)];case 1:return u=n.sent(),[2,u.getContext("2d").getImageData(0,0,i,o).data]}}))}))},t.toPng=function(t,r){return void 0===r&&(r={}),e(this,void 0,void 0,(function(){return n(this,(function(e){switch(e.label){case 0:return[4,K(t,r)];case 1:return[2,e.sent().toDataURL()]}}))}))},t.toSvg=X}));
}).call(globalThis);

/* ==========================================================================
 * THE MARBLING WIDGET
 * ========================================================================== */
/* DESIGN NOTES & CHANGELOG =================================================
 *
 * Non-diffusive: instead of advecting colors (which blends them), we advect
 * a COORDINATE MAP—each screen pixel remembers which point of a pristine
 * source atlas its ink came from—and render by nearest-neighbor lookup.
 * The inks are immiscible: no color outside the original page + the pigment
 * palette is ever displayed, and boundaries stay razor-sharp forever.
 *
 *  - ANIMATED COMB STROKES: a tine stroke no longer twitches the whole sheet
 *    at once. The comb is drawn on screen—a bar with pins at the tine
 *    spacing, halo'd so it reads on any palette—and dragged along the
 *    stroke over tineStrokeSec, applying its shear incrementally (eased;
 *    the sub-shears compose exactly, since displacement parallel to the
 *    tine lines leaves the falloff distance invariant). The cause of the
 *    deformation is now visible as it happens. Drops are ~30% smaller.
 * v7 ("ink video"):
 *  - ROTATING DRIFT: the ambient current is a slow gyre (full rotation every
 *    driftPeriod seconds) rather than a fixed conveyor, so no stretch axis
 *    persists long enough to smear stretched filaments into directional
 *    banding; the time-integrated stretching is isotropic.
 *  - NEW SHEET every resetMinutes: the map snaps back to identity—the
 *    marbler pulls the print and re-inks the tray with the pristine page —
 *    and the operator lifecycle restarts (rubrication drop first), with the
 *    outgoing sheet crossfading briefly over the fresh one. Palettes keep
 *    their own 60s clock and rotate straight through resets, so each sheet
 *    lives through ~15 palettes from crisp drops to fully developed chaos.
 *  - CONTINUOUS DISPLAY: rendering is decoupled from map commitment. Every
 *    frame draws a partial warp of the committed map by the analytically
 *    known displacement accrued since the last commit; the map itself still
 *    advances only in ~stepPixels increments. Partial warps are recomputed
 *    fresh from the committed base each frame, so they add ZERO coordinate
 *    diffusion—smooth 30fps motion with the conservation of large steps.
 *    (CONFIG.smooth: false restores draw-on-commit, near-free but ticky.)
 *  - ANTIALIASED INK: page ink is fetched bilinearly from the atlas, so
 *    filaments stretched below one pixel render as partial coverage that
 *    glides sub-pixel, instead of dropout dashes that pop. Display-only:
 *    the simulation still never blends; edge pixels do show intermediate
 *    shades (that is what antialiasing is). Pigment interiors stay exact
 *    and pigment/page contours stay hard. CONFIG.antialias: false reverts.
 *  - Cost: smooth mode pays a full render pass per frame (~30fps at a
 *    ~1000px viewport on midrange hardware; scales with viewport area).
 *
 * v6:
 *  - HISTORICAL PALETTES: pigments are k-means-extracted from scans of real
 *    marbled papers (French curls of 1735 & 1880, an English spot paper of
 *    1830, a Victorian spot paper, a gilt binding of 1902, and suminagashi
 *    both classic and modern)—cluster medians of the most-saturated
 *    members, i.e. the pigments themselves rather than the scan average.
 *    Rain and right-click draw from the current palette; palettes rotate
 *    every paletteSeconds (default 60). Rotation is a HARD RECOLOR of the
 *    whole bath: drops store a palette SLOT, and rotation repaints the slot
 *    swatches in place, so every drop already floating re-renders in the
 *    new palette's corresponding color on the minute—never a blend, and
 *    never an accumulating jumble of past palettes. The rubrication accent
 *    persists only until the first rotation. Drops, tine strokes, and
 *    palette changes are logged to the console.
 *  - Resolution up to simMax 1440 (~native) with the ambient flow halved
 *    again; adaptive stepping keeps average cost low, though each map step
 *    is proportionally heavier—expect the rake to feel ~20fps on midrange
 *    hardware. Lower simMax if that grates.
 *
 * v5:
 *  - TOROIDAL domain: the bath wraps; drifting ink recirculates instead of
 *    leaking out the edges, so the pixel census is now conserved globally,
 *    not just interiorly. (Caveat: a drop's far-field push overlaps its own
 *    wrap images slightly, so conservation per drop is approximate.)
 *  - TINE LINES: the second classic marbling operator—an area-preserving
 *    shear along a line (a whole comb of parallel lines, via `spacing`),
 *    displacement decaying as lambda/(d+lambda) with distance d from the
 *    nearest tine. Applied automatically at Poisson intervals with random
 *    angle and alternating stroke direction (gel-git), it shreds drop rings
 *    into the chevrons/nonpareil structure of real marbled paper without
 *    waiting for the user's rake.
 *  - Pigment rain retuned: smaller, rarer drops with a size distribution
 *    skewed small (occasional large one), so the page reads as veined stone
 *    rather than poster blobs.
 *  - Higher resolution (simMax 1024, ~2x the pixels of before), paid for by
 *    a 30fps cap, a slower ambient flow, and ADAPTIVE STEPPING: the map
 *    advances in ~0.75px steps rather than every frame. Bilinear map
 *    resampling acts as a slight coordinate-space diffusion (fine patterns
 *    slowly relax); diffusion per distance traveled scales as (1 - step
 *    size), so fewer, larger steps preserve structure ~5x better and skip
 *    the per-frame advection work in between. The rake forces every-frame
 *    steps while it is moving.
 *
 * Conservation, measured under stress (20 simulated seconds of continuous
 * drift): exact for pure flow; ~0.5% census drift with tine strokes; a few
 * percent around drops (residual coordinate diffusion at their compressed
 * rims); pigment areas conserved exactly. No off-atlas color is ever
 * displayed under any combination.
 *
 * Controls: mouse = rake, always down (drag to pull the ink);
 *           right-click = drop the next pigment at the pointer;
 *           LEFT-CLICK / ESC / window.__marbleStop() = stop, restore page.
 * Touch:    drag = rake; TAP = stop; LONG-PRESS (500ms) = drop pigment.
 *           The overlay sets touch-action: none so drags rake instead of
 *           scrolling; sizing uses % of the viewport so mobile URL-bar
 *           collapse and rotation stay sane (the sim keeps its capture
 *           size and stretches; input rescales).
 * The overlay intercepts all pointer events while active, so no clicks
 * reach the page underneath; left-click-to-wake stays instinctive.
 *
 * Known limitations:
 *  - Cross-origin fonts/images may be skipped in the snapshot (harmless:
 *    it is posterized and stirred beyond recognition within a minute).
 *  - Tine combs are not periodic across the wrap seam, so a comb pattern is
 *    discontinuous where it crosses it (rendered correctly, just not
 *    continued).
 */
// The payload. Exposed as a named global so the dispatcher below (and any
// external loader) can invoke it; it does NOT run just by being defined.
window.__marbleStart = async function () {
    'use strict';
    // Early double-run lock: __marbleStop is not installed until setup
    // completes (after an async page capture), so two quick invocations
    // could otherwise both proceed. __marbleStarting closes that window.
    if (window.__marbleStarting) return;
    if (typeof window.__marbleStop === 'function') { window.__marbleStop(); }
    window.__marbleStarting = true;
    try {

        'use strict';


        const CONFIG = {
            seed:      0,        // PRNG seed for ALL stochastic choices; 0 = random.
                                 // The seed used is logged at startup: replay a
                                 // session's choices by setting it here.
            simMax:    1440,     // max simulation dimension (px); lower = faster, blockier
            fps:       30,       // simulation frame-rate cap (rAF is skipped between)
            driftSpeed:  1.6,    // ambient current speed (sim-px/sec); wraps toroidally
            driftPeriod: 480,    // seconds per full rotation of the drift direction: a
                                 // slow gyre, so no stretch axis persists long enough to
                                 // smear filaments into directional banding
            curlAmp:   4,        // swirl strength (sim-px/sec)
            waves:     5,        // sinusoid components in the flow potential
            waveScale: 0.045,    // spatial frequency of swirls (~1/wavelength)
            waveSpeed: 0.05,     // how fast the swirl pattern itself evolves (rad/sec)
            cell:      8,        // velocity-field grid resolution (sim-px per cell)
            maxDt:     0.07,     // clamp on frame dt (sec), avoids jumps after tab-switch
            stepPixels: 0.75,    // commit the map in steps of ~this many px (see frame())
            smooth:    true,     // render a partial display warp EVERY frame (continuous
                                 // motion; costs a full render pass per frame—set false
                                 // to draw only on commits, ~free but ticky)
            antialias: true,     // bilinear page-ink fetch: sub-pixel filaments render as
                                 // partial coverage instead of dropout dashes; edge pixels
                                 // gain intermediate shades (display-only, sim unaffected)
            pixelated: true,     // nearest-neighbor upscale to screen (moot at scale 1)
            rakeRadius:   0.06,  // rake tine width: Gaussian falloff radius (fraction of min dim)
            rakeStrength: 0.9,   // fraction of pointer velocity imparted to the ink
            rakeMax:     900,    // cap on rake speed (sim-px/sec), keeps fast flicks tasteful
            rakeDecay:   0.20,   // seconds for the rake's wake to die off once the mouse stops
            dropRadius:  0.035,  // base drop radius (fraction of min dim)
            rainMean:    12,     // mean seconds between rain drops (Poisson); 0 disables rain
            rainFirst:   0.8,    // seconds until the first (rubrication) drop
            rainMargin:  0.12,   // rain avoids this fraction of each edge
            tineMean:    25,     // mean seconds between automatic tine strokes; 0 disables
            tineFirst:   18,     // seconds until the first tine stroke
            tineShift:   0.30,   // stroke displacement at the tine (fraction of min dim)
            tineFalloff: 0.05,   // displacement falloff length lambda (fraction of min dim)
            tineSpacing: 0.20,   // comb spacing between parallel tines (fraction of min dim)
            tineStrokeSec: 1.2,  // a tine stroke is ANIMATED over this long: the comb is
                                 // drawn on screen and dragged, applying its shear
                                 // incrementally, instead of the sheet twitching at once
            resetMinutes: 10,    // "new sheet": restore the pristine page and start the
                                 // marbling lifecycle over (0 disables); palettes keep
                                 // their own clock and rotate straight through
            rubrication: '#cc0000',   // the first drop of each sheet: gwern.net rubrication red
            tintPage: true,           // posterize the page ground through the palette
            paletteSeconds: 60,       // rotate to the next palette this often
            // Pigment palettes k-means-extracted from scans of real marbled
            // papers (cluster medians of the most-saturated members, so they are
            // the pigments, not the scan average). Rain and right-click both draw
            // from the current palette; palettes rotate every paletteSeconds.
            palettes: [
                ['french-curl-1735',   ['#717574', '#c8a186', '#e2bc9d', '#97403e', '#b77259', '#d69852']],
                ['english-spot-1830',  ['#313f4e', '#8b4739', '#ae937b', '#a76c4f', '#6c5f59', '#55342f']],
                ['french-curl-1880',   ['#520a0d', '#6e2a20', '#844c2d', '#4d4632', '#36627c', '#c16538']],
                ['victorian-spot',     ['#283e33', '#57646c', '#84171d', '#a88622', '#885230', '#3b1f19']],
                ['gilt-binding-1902',  ['#394d43', '#1f3435', '#0e1324', '#5d6552', '#968167', '#df944a']],
                ['suminagashi-ink',    ['#f2f0ea', '#e0d8d0', '#c6beb6', '#847c74', '#3b352d', '#1d1813']],
                ['suminagashi-modern', ['#81bfd6', '#57b5c1', '#160919', '#3d7088', '#1e445c', '#a3d4e5']]
            ]
        };
        const SEAM = 8;   // map-coordinate discontinuity threshold (px); see sampleMapInto
        // Map coordinates are stored UNWRAPPED: on a torus, a map that has wound
        // around cannot be globally continuous in wrapped coordinates, and the
        // resulting artificial seam erodes under advection (corner-picking there
        // duplicates content and biases the census). Unwrapped, values that
        // differ by a full period render identically—the winding seam becomes
        // invisible and harmless—and wrapping happens only at the final atlas
        // fetch. Pigment map targets sit at SWATCH_BASE, far above any Y
        // excursion page ink can plausibly accumulate, so the two never mix
        // (and their gap always trips the seam detector).
        const SWATCH_BASE = 1 << 20;

        /* ---- 1+2. snapshot the viewport --------------------------------------- */
        // Rasterization: html-to-image inlines the page's computed styles into
        // an SVG <foreignObject> and lets the BROWSER rasterize it. Because it
        // never parses CSS itself, it is immune to the growing class of
        // modern-CSS failures (color(), oklch(), lab()) that killed the
        // unmaintained html2canvas, and it tracks future CSS for free. Fidelity
        // quirks (cross-origin fonts/images may be skipped) do not matter here:
        // the snapshot is posterized to six palette colors at t=0 and stirred
        // beyond recognition within a minute. If the capture fails outright
        // (e.g. cross-origin stylesheet security errors), we marble a blank
        // sheet of the page's background color—pure-ebru mode, the toy runs.
        const vw = window.innerWidth, vh = window.innerHeight;
        const scale = Math.min(1, CONFIG.simMax / Math.max(vw, vh));
        const sw = Math.max(2, Math.round(vw * scale));
        const sh = Math.max(2, Math.round(vh * scale));
        const minDim = Math.min(sw, sh);
        // Resolve the page's EFFECTIVE background: many pages leave <body>
        // transparent and paint <html> (or nothing: browser-default white).
        // Without this, transparent snapshot pixels would read as black ink.
        const cssAlpha = c => {
            if (!c || c === 'transparent') return 0;
            const m = c.match(/rgba?\(([^)]+)\)/);
            if (!m) return 1;
            const p = m[1].split(',');
            return p.length >= 4 ? parseFloat(p[3]) : 1;
        };
        const cssRGB = c => {
            const m = c.match(/rgba?\(([^)]+)\)/);
            if (m) { const p = m[1].split(',').map(parseFloat); return [p[0], p[1], p[2]]; }
            if (c[0] === '#') {
                const h = c.slice(1);
                return h.length === 3
                    ? [parseInt(h[0] + h[0], 16), parseInt(h[1] + h[1], 16), parseInt(h[2] + h[2], 16)]
                    : [parseInt(h.slice(0, 2), 16), parseInt(h.slice(2, 4), 16), parseInt(h.slice(4, 6), 16)];
            }
            return [255, 255, 255];
        };
        let pageBg = getComputedStyle(document.body).backgroundColor;
        if (cssAlpha(pageBg) === 0) pageBg = getComputedStyle(document.documentElement).backgroundColor;
        if (cssAlpha(pageBg) === 0) pageBg = '#ffffff';

        async function snapshotViewport() {
            try {
                const htmlToImage = globalThis.htmlToImage;   // vendored above
                const opts = {
                    width: vw, height: vh,
                    pixelRatio: 1,
                    backgroundColor: pageBg,
                    style: {
                        transform: 'translate(' + (-window.scrollX) + 'px,' + (-window.scrollY) + 'px)',
                        transformOrigin: 'top left'
                    }
                };
                // WebKit's foreignObject rasterizer often returns an incomplete
                // first render (missing fonts/images); the standard workaround
                // is to render twice and keep the second.
                const isWebKit = /AppleWebKit/.test(navigator.userAgent)
                              && !/Chrome|Chromium|Edg\//.test(navigator.userAgent);
                if (isWebKit) { try { await htmlToImage.toCanvas(document.body, opts); } catch (e2) {} }
                return await htmlToImage.toCanvas(document.body, opts);
            } catch (e) {
                console.warn('marble: page capture failed (' + (e && e.message) + '); marbling a blank sheet.');
            }
            const c = document.createElement('canvas');
            c.width = vw; c.height = vh;
            const cctx = c.getContext('2d');
            cctx.fillStyle = pageBg;
            cctx.fillRect(0, 0, vw, vh);
            return c;
        }

        const snap = await snapshotViewport();

        let sim  = document.createElement('canvas');
        sim.width = sw; sim.height = sh;
        let sctx = sim.getContext('2d', { willReadFrequently: true });
        sctx.drawImage(snap, 0, 0, sw, sh);

        let srcImg;
        try {
            srcImg = sctx.getImageData(0, 0, sw, sh);
        } catch (e) {
            // A tainted canvas stays tainted forever: rebuild a clean one and
            // marble a blank sheet (same degradation as a failed capture).
            console.warn('marble: snapshot canvas tainted (' + (e && e.message) + '); marbling a blank sheet.');
            sim = document.createElement('canvas');
            sim.width = sw; sim.height = sh;
            sctx = sim.getContext('2d', { willReadFrequently: true });
            sctx.fillStyle = pageBg;
            sctx.fillRect(0, 0, sw, sh);
            srcImg = sctx.getImageData(0, 0, sw, sh);
        }
        {   // composite any residual snapshot transparency over the resolved
            // background, so alpha can be ignored (as opaque) everywhere after
            const [br, bgr, bb] = cssRGB(pageBg);
            const d = srcImg.data;
            for (let i = 3; i < d.length; i += 4) {
                const a = d[i];
                if (a < 255) {
                    const k = a / 255, j = i - 3;
                    d[j]     = d[j]     * k + br  * (1 - k);
                    d[j + 1] = d[j + 1] * k + bgr * (1 - k);
                    d[j + 2] = d[j + 2] * k + bb  * (1 - k);
                    d[i] = 255;
                }
            }
        }

        /* ---- 3. source atlas: page snapshot + pigment swatches --------------- */
        // The atlas is the pristine, never-modified source of all ink. Rows
        // 0..sh-1 are the page; below them, each pigment gets a solid band of
        // SWATCH_H (> 2*SEAM) rows, so the seam detector always fires between
        // different pigments and between pigment and page—coordinates are
        // never blended across a color boundary.
        const SWATCH_H = 2 * SEAM + 2;
        // Pigment SLOTS, not colors: band 0 is the accent (rubrication until the
        // first rotation), bands 1..6 are the current palette's positions. Drops
        // store a slot index; palette rotation REPAINTS the bands in place, so
        // every drop already in the bath hard-swaps to the new palette's color
        // in the same position—a strict recolor, never a blend, and the page
        // ink is untouched. (All palettes must have the same length.)
        const palNames  = CONFIG.palettes.map(p => p[0]);
        const palColors = CONFIG.palettes.map(p => p[1]);
        const SLOTS = 1 + palColors[0].length;
        const bandHex = new Array(SLOTS);
        const parseHex = hex => {
            const h = hex.replace('#', '');
            const f = c => parseInt(c + c, 16);
            return h.length === 3
                ? [f(h[0]), f(h[1]), f(h[2])]
                : [parseInt(h.slice(0, 2), 16), parseInt(h.slice(2, 4), 16), parseInt(h.slice(4, 6), 16)];
        };
        const atlasH = sh + SLOTS * SWATCH_H;
        const srcPix = new Uint8ClampedArray(sw * atlasH * 4);
        srcPix.set(srcImg.data);
        function paintBand(slot, hex) {
            bandHex[slot] = hex;
            const [r, g, b] = parseHex(hex);
            const y0 = sh + slot * SWATCH_H;
            for (let y = y0; y < y0 + SWATCH_H; y++) {
                for (let x = 0; x < sw; x++) {
                    const i = (y * sw + x) * 4;
                    srcPix[i] = r; srcPix[i + 1] = g; srcPix[i + 2] = b; srcPix[i + 3] = 255;
                }
            }
        }
        paintBand(0, CONFIG.rubrication);
        palColors[0].forEach((hex, k) => paintBand(1 + k, hex));

        // Page tinting: posterize the page ground through the current palette by
        // nearest luminance—each source pixel maps to exactly ONE palette
        // color (a strict recolor, never a blend; no color outside the palette
        // is created). This is what makes the sheet read as a marbled paper of
        // the palette's era: the ground of the reference papers is itself
        // colored size, not neutral. Baked into the atlas page rows on each
        // rotation, re-derived from the pristine snapshot kept in srcImg (no
        // compounding), so the render loop pays nothing. Set tintPage: false
        // for the neutral page-as-ground look.
        function tintPage() {
            if (!CONFIG.tintPage) return;
            const cols = palColors[paletteIdx].map(parseHex);
            const lum = cols.map(([r, g, b]) => (r * 77 + g * 151 + b * 28) >> 8);
            const lut = new Uint8Array(256 * 3);
            for (let v = 0; v < 256; v++) {
                let bi = 0, bd = 1e9;
                for (let j = 0; j < cols.length; j++) {
                    const d = Math.abs(lum[j] - v);
                    if (d < bd) { bd = d; bi = j; }
                }
                lut[v * 3] = cols[bi][0]; lut[v * 3 + 1] = cols[bi][1]; lut[v * 3 + 2] = cols[bi][2];
            }
            const orig = srcImg.data;   // pristine page pixels, never modified
            const n = sw * sh * 4;
            for (let i = 0; i < n; i += 4) {
                const v = (orig[i] * 77 + orig[i + 1] * 151 + orig[i + 2] * 28) >> 8;
                srcPix[i]     = lut[v * 3];
                srcPix[i + 1] = lut[v * 3 + 1];
                srcPix[i + 2] = lut[v * 3 + 2];
            }
        }
        const swatchY = p => SWATCH_BASE + p * SWATCH_H + SWATCH_H / 2;   // pigment p's map target
        const swatchX = sw / 2;

        const out    = new ImageData(sw, sh);
        const outPix = out.data;

        // The coordinate map: for each screen pixel, the (float) atlas position
        // whose ink now sits there.
        const N = sw * sh;
        let mapX  = new Float32Array(N), mapY  = new Float32Array(N);
        let mapX2 = new Float32Array(N), mapY2 = new Float32Array(N);
        for (let y = 0, i = 0; y < sh; y++) {
            for (let x = 0; x < sw; x++, i++) { mapX[i] = x; mapY[i] = y; }
        }

        const HALF_BASE = SWATCH_BASE / 2;
        const hw = sw / 2, hh = sh / 2;

        // Seam-aware bilinear sample of (mapX, mapY) at position (sx, sy), written
        // into (mapX2, mapY2) at index i. The domain is a TORUS: sx, sy wrap
        // (callers guarantee excursions of less than one period, so a single
        // conditional suffices).
        //
        // Key fact: the PAGE-ink map is continuous everywhere—every marbling
        // operator (advection, drop, tine) is a homeomorphism—so an apparent
        // jump between page values is either a torus winding (a period multiple:
        // unwrap it and interpolate exactly) or a steep-but-real gradient from
        // compression (interpolate it as it is). Snapping either one quantizes
        // sub-pixel motion; the snapped column then forms a new jump against its
        // neighbors and accretes a growing plateau of duplicated content as it
        // advects—a severe census bias. The ONLY genuine discontinuities are
        // pigment boundaries, identified by CLASS (mapY >= SWATCH_BASE/2), never
        // by value spread; only those get majority-side blending (coordinates
        // must not blend across them, or ink would be fetched from the
        // meaningless straight line between two atlas regions).
        function sampleMapInto(sx, sy, i) {
            if (sx < 0) sx += sw; else if (sx >= sw) sx -= sw;
            if (sy < 0) sy += sh; else if (sy >= sh) sy -= sh;
            const x0 = sx | 0, y0 = sy | 0;
            const bx = sx - x0, by = sy - y0;
            const x1 = x0 + 1 < sw ? x0 + 1 : 0;
            const y1 = y0 + 1 < sh ? y0 + 1 : 0;
            const r0 = y0 * sw, r1 = y1 * sw;
            const i00 = r0 + x0, i10 = r0 + x1, i01 = r1 + x0, i11 = r1 + x1;
            let x00 = mapX[i00], x10 = mapX[i10], x01 = mapX[i01], x11 = mapX[i11];
            let y00 = mapY[i00], y10 = mapY[i10], y01 = mapY[i01], y11 = mapY[i11];
            const w00 = (1 - bx) * (1 - by), w10 = bx * (1 - by);
            const w01 = (1 - bx) * by,       w11 = bx * by;
            const g00 = y00 >= HALF_BASE, g10 = y10 >= HALF_BASE;
            const g01 = y01 >= HALF_BASE, g11 = y11 >= HALF_BASE;
            let d;
            if (!g00 && !g10 && !g01 && !g11) {
                // all page ink: unwrap torus windings toward corner 00, interpolate
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
                // all pigment: same band (constant values) -> interpolation is exact
                let mn = y00 < y10 ? y00 : y10, mx2 = y00 > y10 ? y00 : y10;
                if (y01 < mn) mn = y01; if (y01 > mx2) mx2 = y01;
                if (y11 < mn) mn = y11; if (y11 > mx2) mx2 = y11;
                if (mx2 - mn <= SEAM) {
                    mapX2[i] = x00 * w00 + x10 * w10 + x01 * w01 + x11 * w11;
                    mapY2[i] = y00 * w00 + y10 * w10 + y01 * w01 + y11 * w11;
                    return;
                }
            }
            // pigment boundary (mixed classes, or mixed pigment bands):
            // blend only the max-weight corner's group
            let refW = w00, refX = x00, refY = y00, refG = g00;
            if (w10 > refW) { refW = w10; refX = x10; refY = y10; refG = g10; }
            if (w01 > refW) { refW = w01; refX = x01; refY = y01; refG = g01; }
            if (w11 > refW) { refW = w11; refX = x11; refY = y11; refG = g11; }
            let ex = 0, ey = 0, ew = 0;
            if (refG) {   // pigment group: same band only
                if (g00) { d = y00 - refY; if (d <= SEAM && d >= -SEAM) { ex += x00 * w00; ey += y00 * w00; ew += w00; } }
                if (g10) { d = y10 - refY; if (d <= SEAM && d >= -SEAM) { ex += x10 * w10; ey += y10 * w10; ew += w10; } }
                if (g01) { d = y01 - refY; if (d <= SEAM && d >= -SEAM) { ex += x01 * w01; ey += y01 * w01; ew += w01; } }
                if (g11) { d = y11 - refY; if (d <= SEAM && d >= -SEAM) { ex += x11 * w11; ey += y11 * w11; ew += w11; } }
            } else {      // page group: unwrap each member toward the reference
                if (!g00) {
                    d = x00 - refX; if (d > hw || d < -hw) x00 -= sw * Math.round(d / sw);
                    d = y00 - refY; if (d > hh || d < -hh) y00 -= sh * Math.round(d / sh);
                    ex += x00 * w00; ey += y00 * w00; ew += w00;
                }
                if (!g10) {
                    d = x10 - refX; if (d > hw || d < -hw) x10 -= sw * Math.round(d / sw);
                    d = y10 - refY; if (d > hh || d < -hh) y10 -= sh * Math.round(d / sh);
                    ex += x10 * w10; ey += y10 * w10; ew += w10;
                }
                if (!g01) {
                    d = x01 - refX; if (d > hw || d < -hw) x01 -= sw * Math.round(d / sw);
                    d = y01 - refY; if (d > hh || d < -hh) y01 -= sh * Math.round(d / sh);
                    ex += x01 * w01; ey += y01 * w01; ew += w01;
                }
                if (!g11) {
                    d = x11 - refX; if (d > hw || d < -hw) x11 -= sw * Math.round(d / sw);
                    d = y11 - refY; if (d > hh || d < -hh) y11 -= sh * Math.round(d / sh);
                    ex += x11 * w11; ey += y11 * w11; ew += w11;
                }
            }
            mapX2[i] = ex / ew;
            mapY2[i] = ey / ew;
        }

        // Periodically shift ALL page map values by a global period multiple to
        // keep them near [0,sw)x[0,sh): a uniform shift renders identically
        // (mod period), creates no new seams, keeps the fetch fast path hot, and
        // protects Float32 sub-pixel precision on long runs. Pigment Y values
        // (>= SWATCH_BASE) are left alone; pigment X shifts harmlessly (swatch
        // bands are uniform in x).
        function renormalize() {
            let sx = 0, sy = 0, n = 0;
            for (let i = 0; i < N; i += 97) {
                const y = mapY[i];
                if (y < SWATCH_BASE / 2) { sx += mapX[i]; sy += y; n++; }
            }
            if (n === 0) return;
            const kx = Math.floor((sx / n) / sw) * sw;
            const ky = Math.floor((sy / n) / sh) * sh;
            if (kx === 0 && ky === 0) return;
            for (let i = 0; i < N; i++) {
                if (mapY[i] < SWATCH_BASE / 2) { mapX[i] -= kx; mapY[i] -= ky; }
            }
        }
        let framesSinceNorm = 0;

        /* ---- 4. marbling operators: drop & tine ------------------------------- */
        // Drop: an area-preserving bijection. A new pixel at distance d > r from
        // the center got its ink from distance sqrt(d^2 - r^2) along the same
        // ray; d <= r is fresh pigment. Distances wrap to the nearest torus image.
        // The exact transform has INFINITE compression at the rim, which a
        // pixel-resolution map cannot represent—per-frame resampling then
        // diffuses the near-singular ring and biases the census. So the rim is
        // mollified: sqrt(d^2 - r^2 + h^2) with h = DROP_SOFT*r caps compression
        // at ~1/DROP_SOFT x, at the cost of overwriting a small central disk of
        // displaced content (~DROP_SOFT^2 of the drop's area). This is the
        // surface-tension floor of real marbling, moved into the operator.
        const DROP_SOFT = 0.2;
        function applyDrop(cx, cy, r, pigment) {
            const r2 = r * r;
            const h2 = DROP_SOFT * DROP_SOFT * r2;
            const sy2 = swatchY(pigment);
            let i = 0;
            for (let y = 0; y < sh; y++) {
                let dy = y - cy;
                if (dy > hh) dy -= sh; else if (dy < -hh) dy += sh;
                const dy2 = dy * dy;
                for (let x = 0; x < sw; x++, i++) {
                    let dx = x - cx;
                    if (dx > hw) dx -= sw; else if (dx < -hw) dx += sw;
                    const d2 = dx * dx + dy2;
                    if (d2 <= r2) {
                        mapX2[i] = swatchX;
                        mapY2[i] = sy2;
                    } else {
                        const s = Math.sqrt((d2 - r2 + h2) / d2);
                        sampleMapInto(cx + dx * s, cy + dy * s, i);
                    }
                }
            }
            let tmp = mapX; mapX = mapX2; mapX2 = tmp;
            tmp = mapY; mapY = mapY2; mapY2 = tmp;
        }

        // Tine comb: an area-preserving shear. Points displace by m*lambda/(d+lambda)
        // along the line direction, where d is the distance to the nearest tine of
        // a comb of parallel lines `spacing` apart (spacing 0 = a single tine).
        // Any shear u' = u + f(v) has unit Jacobian, so the census is untouched.
        function applyTine(ax, ay, angle, m, lambda, spacing) {
            const mxd = Math.cos(angle), myd = Math.sin(angle);   // line direction M
            const nxd = -myd, nyd = mxd;                          // normal N
            let i = 0;
            for (let y = 0; y < sh; y++) {
                for (let x = 0; x < sw; x++, i++) {
                    const c = (x - ax) * nxd + (y - ay) * nyd;
                    const d = spacing > 0
                        ? Math.abs(c - spacing * Math.round(c / spacing))
                        : Math.abs(c);
                    const s = m * lambda / (d + lambda);
                    sampleMapInto(x - s * mxd, y - s * myd, i);
                }
            }
            let tmp = mapX; mapX = mapX2; mapX2 = tmp;
            tmp = mapY; mapY = mapY2; mapY2 = tmp;
        }

        /* ---- 5. schedulers: pigment rain & automatic tine strokes -------------- */
        const startT = performance.now();   // simulation epoch, for log timestamps
        const dropR = minDim * CONFIG.dropRadius;
        const opQueue = [];   // {t, kind:'drop'|'tine', ...}, processed in frame()
        let nextRain = performance.now() + CONFIG.rainFirst * 1000;
        let nextTine = performance.now() + CONFIG.tineFirst * 1000;
        let rainCount = 0;
        let tineSign  = 1;    // alternate stroke direction: gel-git ("come-and-go")
        let paletteIdx = 0;
        let paletteAt  = performance.now();
        let cycleNext  = 0;   // right-click cycles within the current palette
        let paletteChanged = false;   // forces a render step so the recolor shows
        let sheetAt = performance.now();
        const simT = now => 'marble[' + ((now - startT) / 1000).toFixed(1) + 's]';
        tintPage();                   // bake the initial palette into the ground

        // "New sheet": the marbler pulls the print and re-inks the tray. The map
        // snaps back to identity (the pristine page—kept unmodified in srcImg —
        // reappears through the current palette's tint) and the operator
        // lifecycle restarts, rubrication drop first. The outgoing sheet
        // crossfades over the fresh one for a moment (display-only blend).
        function resetSheet(now) {
            for (let y = 0, i = 0; y < sh; y++) {
                for (let x = 0; x < sw; x++, i++) { mapX[i] = x; mapY[i] = y; }
            }
            dtAcc = 0;
            rainCount = 0;
            cycleNext = 0;
            opQueue.length = 0;
            nextRain = now + CONFIG.rainFirst * 1000;
            nextTine = now + CONFIG.tineFirst * 1000;
            paintBand(0, CONFIG.rubrication);   // each sheet opens with rubrication
                                                // (the next palette rotation may
                                                // recolor it, as with any pigment)
            fctx.drawImage(sim, 0, 0);          // capture the outgoing sheet
            fadeUntil = now + FADE_MS;
            paletteChanged = true;              // force an immediate render
            console.log(simT(now) + ': new sheet');
        }

        function schedule(now) {
            if (CONFIG.resetMinutes > 0 && now - sheetAt >= CONFIG.resetMinutes * 60000) {
                sheetAt = now;
                resetSheet(now);
            }
            if (now - paletteAt >= CONFIG.paletteSeconds * 1000) {
                paletteIdx = (paletteIdx + 1) % palNames.length;
                paletteAt = now;
                cycleNext = 0;
                palColors[paletteIdx].forEach((hex, k) => paintBand(1 + k, hex));
                paintBand(0, palColors[paletteIdx][0]);   // the accent band follows too
                tintPage();
                paletteChanged = true;
                console.log(simT(now) + ': palette -> ' + palNames[paletteIdx]);
            }
            if (CONFIG.rainMean > 0 && now >= nextRain) {
                const mg = CONFIG.rainMargin;
                const u = rand();
                opQueue.push({
                    t: now, kind: 'drop',
                    x: sw * (mg + rand() * (1 - 2 * mg)),
                    y: sh * (mg + rand() * (1 - 2 * mg)),
                    r: dropR * (0.55 + 1.8 * u * u * u),   // mostly small, occasional large
                    pigment: rainCount === 0
                        ? 0   // the very first drop: rubrication (accent slot)
                        : 1 + ((rand() * palColors[paletteIdx].length) | 0)
                });
                rainCount++;
                nextRain = now - Math.log(1 - rand()) * CONFIG.rainMean * 1000;
            }
            if (CONFIG.tineMean > 0 && now >= nextTine) {
                opQueue.push({
                    t: now, kind: 'tine',
                    x: rand() * sw, y: rand() * sh,
                    angle: rand() * Math.PI,
                    m: tineSign * minDim * CONFIG.tineShift * (0.7 + 0.6 * rand()),
                    lambda: minDim * CONFIG.tineFalloff,
                    spacing: minDim * CONFIG.tineSpacing
                });
                tineSign = -tineSign;
                nextTine = now - Math.log(1 - rand()) * CONFIG.tineMean * 1000;
            }
        }

        // Pointer -> sim scaling, kept correct across resize / rotation. The sim
        // itself stays at its capture size (the display just stretches; a fresh
        // paste gives a fresh sheet at the new size) but input must keep mapping
        // to the right sim coordinates.
        let ptrKx = scale, ptrKy = scale;
        function onResize() {
            const r = overlay.getBoundingClientRect();
            if (r.width > 0)  ptrKx = sw / r.width;
            if (r.height > 0) ptrKy = sh / r.height;
        }

        /* ---- 6. the rake ------------------------------------------------------ */
        const rake = { x: -1e9, y: -1e9, vx: 0, vy: 0, lastT: 0, active: false };

        function onMove(e) {
            gestureMove(e);
            const now = performance.now();
            const x = e.clientX * ptrKx, y = e.clientY * ptrKy;   // CSS px -> sim px
            if (rake.active) {
                const edt = Math.max(1, now - rake.lastT) / 1000;
                let ivx = (x - rake.x) / edt, ivy = (y - rake.y) / edt;
                const sp = Math.hypot(ivx, ivy);
                if (sp > CONFIG.rakeMax) { ivx *= CONFIG.rakeMax / sp; ivy *= CONFIG.rakeMax / sp; }
                rake.vx += (ivx - rake.vx) * 0.5;   // exponential smoothing
                rake.vy += (ivy - rake.vy) * 0.5;
            }
            rake.x = x; rake.y = y; rake.lastT = now; rake.active = true;
        }

        /* ---- 7. flow field ---------------------------------------------------- */
        // Ambient velocity is the curl of a scalar potential (sum of traveling
        // sine waves), so it is divergence-free. The rake is ALSO a stream
        // function, psi = G(r)*(v x delta), Gaussian G: a localized translation
        // at the tine plus the incompressible return flow around it.
        // One seeded PRNG (mulberry32: Math.imul keeps every step in exact
        // int32, unlike a naive LCG multiply) drives ALL stochastic choices —
        // the flow field, the rain, and the tine strokes.
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
        for (let i = 0; i < CONFIG.waves; i++) {
            const ang = rand() * Math.PI * 2;
            const k   = CONFIG.waveScale * (0.5 + rand());
            waves.push({
                kx:  Math.cos(ang) * k,
                ky:  Math.sin(ang) * k,
                w:   (rand() - 0.5) * 2 * CONFIG.waveSpeed,
                phi: rand() * Math.PI * 2,
                amp: (0.6 + 0.8 * rand()) / k          // normalize so |v| ~ O(1) per wave
            });
        }

        const cell = CONFIG.cell;
        const gw = Math.ceil(sw / cell) + 1;
        const gh = Math.ceil(sh / cell) + 1;
        const vxg = new Float32Array(gw * gh);
        const vyg = new Float32Array(gw * gh);
        const rakeSig2 = (CONFIG.rakeRadius * minDim) ** 2;
        let velMax2 = 0;   // max squared speed over the grid, updated per call

        function updateVelocity(t) {
            velMax2 = 0;
            const A = CONFIG.curlAmp / CONFIG.waves;
            // the drift direction rotates slowly (a gyre, not a conveyor): the
            // time-integrated stretch tensor stays isotropic, so maximally
            // stretched filaments cannot accumulate into axis-aligned banding
            const da = 2 * Math.PI * t / CONFIG.driftPeriod;
            const dx = CONFIG.driftSpeed * Math.cos(da);
            const dy = CONFIG.driftSpeed * Math.sin(da);
            const useRake = rake.active
                && (rake.vx * rake.vx + rake.vy * rake.vy) > 1;
            const rs = CONFIG.rakeStrength;
            for (let gy = 0; gy < gh; gy++) {
                const y = gy * cell;
                for (let gx = 0; gx < gw; gx++) {
                    const x = gx * cell;
                    let u = dx, v = dy;
                    for (let i = 0; i < waves.length; i++) {
                        const wv = waves[i];
                        const c = Math.cos(wv.kx * x + wv.ky * y + wv.w * t + wv.phi) * wv.amp;
                        u += A * ( wv.ky * c);   //  d(psi)/dy
                        v += A * (-wv.kx * c);   // -d(psi)/dx
                    }
                    if (useRake) {
                        const ddx = x - rake.x, ddy = y - rake.y;
                        const r2 = ddx * ddx + ddy * ddy;
                        if (r2 < 7 * rakeSig2) {
                            const g = Math.exp(-r2 / rakeSig2) * rs;
                            const k2 = 2 * (rake.vx * ddy - rake.vy * ddx) / rakeSig2;
                            u += g * (rake.vx - ddy * k2);
                            v += g * (rake.vy + ddx * k2);
                        }
                    }
                    const idx = gy * gw + gx;
                    vxg[idx] = u;
                    vyg[idx] = v;
                    const m2 = u * u + v * v;
                    if (m2 > velMax2) velMax2 = m2;
                }
            }
        }

        /* ---- 8. advection step / render ----------------------------------------- */
        // Semi-Lagrangian backward advection of the coordinate map, fused with
        // the render. When `commit` is false this is a DISPLAY WARP only: the
        // advected map is used to draw this frame but is not swapped in, so
        // repeated partial warps are always recomputed fresh from the committed
        // base and accumulate no coordinate diffusion. The map itself only
        // advances when `commit` is true (~every stepPixels of displacement).
        //
        // Fetch: with CONFIG.antialias, page ink is sampled bilinearly from the
        // atlas (4 local source taps)—display-only antialiasing that renders
        // sub-pixel filaments as partial coverage instead of dropout dashes, at
        // the cost of intermediate shades along ink edges. Pigment bands are
        // uniform, so their fetch stays exact either way; pigment/page contours
        // remain hard.
        function step(dt, commit) {
            const invCell = 1 / cell;
            const aa = CONFIG.antialias;
            let i = 0, di = 0;
            for (let y = 0; y < sh; y++) {
                const gyF = y * invCell;
                const gy0 = gyF | 0;
                const fy  = gyF - gy0;
                const row0 = gy0 * gw;
                const row1 = (gy0 + 1 < gh ? gy0 + 1 : gh - 1) * gw;
                for (let x = 0; x < sw; x++, i++, di += 4) {
                    const gxF = x * invCell;
                    const gx0 = gxF | 0;
                    const fx  = gxF - gx0;
                    const gx1 = gx0 + 1 < gw ? gx0 + 1 : gw - 1;
                    const u = (vxg[row0 + gx0] * (1 - fx) + vxg[row0 + gx1] * fx) * (1 - fy)
                            + (vxg[row1 + gx0] * (1 - fx) + vxg[row1 + gx1] * fx) * fy;
                    const v = (vyg[row0 + gx0] * (1 - fx) + vyg[row0 + gx1] * fx) * (1 - fy)
                            + (vyg[row1 + gx0] * (1 - fx) + vyg[row1 + gx1] * fx) * fy;

                    sampleMapInto(x - u * dt, y - v * dt, i);

                    // wrap unwrapped map coords into the atlas (fast path: in range)
                    let mx = mapX2[i];
                    if (mx < 0 || mx >= sw) { mx %= sw; if (mx < 0) mx += sw; }
                    let my = mapY2[i];
                    if (my >= SWATCH_BASE - 1) {
                        // pigment: bands are uniform fills, nearest is exact
                        const ry = (my - SWATCH_BASE + sh + 0.5) | 0;
                        const rx = (mx + 0.5) | 0;
                        const si = (ry * sw + (rx >= sw ? 0 : rx)) * 4;
                        outPix[di]     = srcPix[si];
                        outPix[di + 1] = srcPix[si + 1];
                        outPix[di + 2] = srcPix[si + 2];
                    } else {
                        if (my < 0 || my >= sh) { my %= sh; if (my < 0) my += sh; }
                        if (aa) {
                            // bilinear over the (toroidal) page rows of the atlas
                            const ax0 = mx | 0, bx2 = mx - ax0;
                            const ay0 = my | 0, by2 = my - ay0;
                            const ax1 = ax0 + 1 < sw ? ax0 + 1 : 0;
                            const ay1 = ay0 + 1 < sh ? ay0 + 1 : 0;
                            const j00 = (ay0 * sw + ax0) * 4, j10 = (ay0 * sw + ax1) * 4;
                            const j01 = (ay1 * sw + ax0) * 4, j11 = (ay1 * sw + ax1) * 4;
                            const q00 = (1 - bx2) * (1 - by2), q10 = bx2 * (1 - by2);
                            const q01 = (1 - bx2) * by2,       q11 = bx2 * by2;
                            outPix[di]     = srcPix[j00]     * q00 + srcPix[j10]     * q10 + srcPix[j01]     * q01 + srcPix[j11]     * q11;
                            outPix[di + 1] = srcPix[j00 + 1] * q00 + srcPix[j10 + 1] * q10 + srcPix[j01 + 1] * q01 + srcPix[j11 + 1] * q11;
                            outPix[di + 2] = srcPix[j00 + 2] * q00 + srcPix[j10 + 2] * q10 + srcPix[j01 + 2] * q01 + srcPix[j11 + 2] * q11;
                        } else {
                            let rx = (mx + 0.5) | 0; if (rx >= sw) rx = 0;
                            let ry = (my + 0.5) | 0; if (ry >= sh) ry = 0;
                            const si = (ry * sw + rx) * 4;
                            outPix[di]     = srcPix[si];
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

        /* ---- 9. overlay & loop -------------------------------------------------- */
        const overlay = document.createElement('canvas');
        overlay.width = vw; overlay.height = vh;
        Object.assign(overlay.style, {
            position: 'fixed', top: '0', left: '0',
            width: '100%', height: '100%',   // % of the viewport tracks mobile URL-bar
                                             // collapse where 100vh does not
            zIndex: '2147483647', pointerEvents: 'auto', cursor: 'crosshair',
            touchAction: 'none',             // receive pointermove during touch drags
                                             // instead of the browser claiming a scroll
            overscrollBehavior: 'none',
            userSelect: 'none', webkitUserSelect: 'none'
        });
        document.body.appendChild(overlay);
        const fade = document.createElement('canvas');   // outgoing sheet, for the reset crossfade
        fade.width = sw; fade.height = sh;
        const fctx = fade.getContext('2d');
        const FADE_MS = 1500;
        let fadeUntil = 0;
        const octx = overlay.getContext('2d');
        octx.imageSmoothingEnabled = !CONFIG.pixelated;
        octx.imageSmoothingQuality = 'high';

        let stroke = null;   // the active (animated) tine stroke, or null

        // Draw the comb being dragged: a bar with pins at the tine spacing,
        // moving along the stroke direction, with faint trails behind the pins.
        // Drawn twice (light halo under dark line) so it reads on any palette.
        function drawComb(now) {
            if (!stroke) return;
            const prog = Math.min(1, (now - stroke.t0) / stroke.dur);
            const e = prog * prog * (3 - 2 * prog);
            const sc = vw / sw;
            const mxd = Math.cos(stroke.angle), myd = Math.sin(stroke.angle);
            const nxd = -myd, nyd = mxd;
            const ax = stroke.x * sc, ay = stroke.y * sc;          // stroke origin
            const cx = ax + e * stroke.m * mxd * sc;               // comb bar now
            const cy = ay + e * stroke.m * myd * sc;
            const L = Math.hypot(vw, vh);
            const sp = stroke.spacing * sc;
            const alpha = prog < 0.8 ? 1 : (1 - prog) / 0.2;       // fade out at the end
            octx.save();
            octx.lineCap = 'round';
            for (const [color, wBar, wTrail, aMul] of
                 [['rgba(255,255,255,0.5)', 4, 3, alpha], ['rgba(0,0,0,0.65)', 2, 1, alpha]]) {
                octx.strokeStyle = octx.fillStyle = color;
                octx.globalAlpha = aMul;
                octx.lineWidth = wBar;
                octx.beginPath();                                  // the comb bar
                octx.moveTo(cx - nxd * L, cy - nyd * L);
                octx.lineTo(cx + nxd * L, cy + nyd * L);
                octx.stroke();
                octx.lineWidth = wTrail;
                const kMax = Math.ceil(L / sp);
                for (let k = -kMax; k <= kMax; k++) {              // pins + trails
                    const px = cx + nxd * k * sp, py = cy + nyd * k * sp;
                    if (px < -20 || px > vw + 20 || py < -20 || py > vh + 20) continue;
                    octx.globalAlpha = aMul * 0.35;                // trail from stroke start
                    octx.beginPath();
                    octx.moveTo(ax + nxd * k * sp, ay + nyd * k * sp);
                    octx.lineTo(px, py);
                    octx.stroke();
                    octx.globalAlpha = aMul;
                    octx.beginPath();                              // the pin
                    octx.arc(px, py, wBar + 1, 0, Math.PI * 2);
                    octx.fill();
                }
            }
            octx.restore();
        }

        let raf = 0;
        let lastSim = startT;
        const t0 = startT;
        let running = true;
        let dtAcc = 0;

        function frame(now) {
            if (!running) return;
            raf = requestAnimationFrame(frame);
            if (now - lastSim < 1000 / CONFIG.fps - 2) return;   // fps cap
            const dt = Math.min(CONFIG.maxDt, (now - lastSim) / 1000);
            lastSim = now;
            const t = (now - t0) / 1000;
            schedule(now);
            let opsApplied = paletteChanged;
            paletteChanged = false;
            for (let qi = 0; qi < opQueue.length; ) {
                const d = opQueue[qi];
                if (now < d.t) { qi++; continue; }
                if (d.kind === 'tine' && stroke) { qi++; continue; }   // combs wait their
                                                    // turn, but drops jump ahead of them
                opQueue.splice(qi, 1);
                if (d.kind === 'drop') {
                    applyDrop(d.x, d.y, d.r, d.pigment);
                    console.log(`${simT(now)}: drop ${bandHex[d.pigment]} r=${d.r.toFixed(0)} @ (${d.x.toFixed(0)}, ${d.y.toFixed(0)})`);
                } else {
                    stroke = { ...d, applied: 0, t0: now, dur: CONFIG.tineStrokeSec * 1000 };
                    console.log(`${simT(now)}: tine comb @ ${(d.angle * 180 / Math.PI).toFixed(0)}deg, shift ${d.m.toFixed(0)}px, spacing ${d.spacing.toFixed(0)}px`);
                }
                opsApplied = true;
            }
            // Active comb stroke: apply its shear incrementally, eased. Shears
            // along the same tine lines compose exactly additively (displacement
            // is parallel to the lines, so perpendicular distance—hence the
            // falloff—is invariant), so the sub-steps sum to exactly m.
            if (stroke) {
                const prog = Math.min(1, (now - stroke.t0) / stroke.dur);
                const e = prog * prog * (3 - 2 * prog);   // smoothstep: the comb eases in and out
                const target = stroke.m * e;
                const dm = target - stroke.applied;
                if (dm !== 0) {
                    applyTine(stroke.x, stroke.y, stroke.angle, dm, stroke.lambda, stroke.spacing);
                    stroke.applied = target;
                    opsApplied = true;
                }
                if (prog >= 1) stroke = null;
            }
            const decay = Math.exp(-dt / CONFIG.rakeDecay);   // rake wake dies off
            rake.vx *= decay;
            rake.vy *= decay;
            dtAcc += dt;
            updateVelocity(t);
            const maxV = Math.sqrt(velMax2);
            // ADAPTIVE COMMITS, CONTINUOUS DISPLAY: the map only advances (is
            // committed) every ~stepPixels of displacement—each bilinear
            // resampling diffuses the coordinate map by ~f(1-f) px^2 (f = step
            // size in px), so fewer, larger steps preserve fine structure
            // several-fold better. But with CONFIG.smooth, every frame renders a
            // partial warp of the committed map by the displacement accrued so
            // far: recomputed fresh each frame, it accumulates no diffusion, and
            // motion is continuous instead of ticking at the commit rate.
            const commit = maxV * dtAcc >= CONFIG.stepPixels;
            if (commit || CONFIG.smooth || opsApplied) {
                const dtEff = commit
                    ? Math.min(dtAcc, maxV > 0 ? (2 * CONFIG.stepPixels) / maxV : dtAcc)
                    : dtAcc;
                step(dtEff, commit);
                if (commit) {
                    dtAcc = 0;
                    if (++framesSinceNorm >= 300) { renormalize(); framesSinceNorm = 0; }
                }
                sctx.putImageData(out, 0, 0);
                octx.drawImage(sim, 0, 0, vw, vh);
                if (now < fadeUntil) {
                    octx.globalAlpha = (fadeUntil - now) / FADE_MS;
                    octx.drawImage(fade, 0, 0, vw, vh);
                    octx.globalAlpha = 1;
                }
                drawComb(now);
            }
        }

        function stop() {
            running = false;
            cancelAnimationFrame(raf);
            clearTimeout(gesture.lpTimer);
            overlay.remove();   // also removes the overlay's listeners
            window.removeEventListener('resize', onResize);
            document.removeEventListener('keydown', onKey, true);
            delete window.__marbleStop;
            console.log('marble: stopped.');
        }
        function onKey(e) { if (e.key === 'Escape') stop(); }

        function queueDropAt(clientX, clientY) {
            opQueue.push({ t: performance.now(), kind: 'drop',
                           x: clientX * ptrKx, y: clientY * ptrKy,
                           r: dropR,
                           pigment: 1 + cycleNext });
            cycleNext = (cycleNext + 1) % palColors[paletteIdx].length;
        }

        // Gesture grammar. Mouse: move = rake, left-click = stop, right-click =
        // drop (unchanged). Touch: drag = rake, TAP = stop (the wake instinct),
        // LONG-PRESS = drop. Touch taps are detected explicitly from pointer
        // down/up displacement and timing, because a crisply-ended rake drag can
        // otherwise emit a synthetic click and dismiss the toy by accident.
        const gesture = { x: 0, y: 0, t: 0, moved: false, consumed: false, type: 'mouse', lpTimer: 0 };

        function gestureMove(e) {
            if (!gesture.moved
                && Math.hypot(e.clientX - gesture.x, e.clientY - gesture.y) > 12) {
                gesture.moved = true;
                clearTimeout(gesture.lpTimer);
            }
        }
        function onPointerDown(e) {
            gesture.type = e.pointerType;
            gesture.x = e.clientX; gesture.y = e.clientY;
            gesture.t = performance.now();
            gesture.moved = false;
            gesture.consumed = false;
            if (e.pointerType === 'touch') {
                try { overlay.setPointerCapture(e.pointerId); } catch (err) {}
                clearTimeout(gesture.lpTimer);
                gesture.lpTimer = setTimeout(() => {
                    if (!gesture.moved) {          // long-press: drop pigment here
                        gesture.consumed = true;
                        queueDropAt(gesture.x, gesture.y);
                    }
                }, 500);
            }
        }
        function onPointerUp(e) {
            clearTimeout(gesture.lpTimer);
            if (e.pointerType === 'touch' && !gesture.moved && !gesture.consumed
                && performance.now() - gesture.t < 350) {
                stop();                            // tap = wake
            }
        }
        function onClick(e) {
            e.preventDefault();
            e.stopPropagation();
            if (gesture.type === 'touch') return;  // touch handled in onPointerUp
            stop();
        }
        function onContextMenu(e) {
            e.preventDefault();
            e.stopPropagation();
            if (gesture.type === 'touch') return;  // Android long-press: our timer drops
            queueDropAt(e.clientX, e.clientY);
        }

        overlay.addEventListener('pointerdown', onPointerDown);
        overlay.addEventListener('pointerup', onPointerUp);
        overlay.addEventListener('pointercancel', () => clearTimeout(gesture.lpTimer));
        overlay.addEventListener('pointermove', onMove);
        overlay.addEventListener('click', onClick);   // 'click' (not pointerdown), so the
                                                      // release can't fall through to a link
        overlay.addEventListener('contextmenu', onContextMenu);
        window.addEventListener('resize', onResize);
        document.addEventListener('keydown', onKey, true);
        window.__marbleStop = stop;

        console.log(`marble: running at ${sw}x${sh} (scale ${scale.toFixed(2)}, ${CONFIG.fps}fps cap), seed ${seedUsed}. ` +
                    'Drag = rake; right-click (touch: long-press) = drop pigment; ' +
                    'left-click (touch: tap) or ESC to stop.');
        raf = requestAnimationFrame(frame);
    } finally {
        delete window.__marbleStarting;   // lock released; __marbleStop (if
                                          // installed) now owns the lifecycle
    }
};

/* ==========================================================================
 * DISPATCHER: run-now vs. idle-screensaver mode
 *
 * Plain inclusion—console paste, or <script src="paper-marble.js"> with no
 * attributes (which is how the optional external 2KB stub loader injects
 * this file)—marbles IMMEDIATELY.
 *
 * Inclusion with a data-screensaver attribute:
 *     <script src="/static/js/paper-marble.js" data-screensaver async></script>
 * instead ARMS a 1-hour no-activity timer and marbles when the reader has
 * wandered off. The guards below (top-level tab, prefers-reduced-motion,
 * visibility) gate the PAYLOAD itself in this mode, not merely a loader.
 * Activity handlers are passive single-assignments; idleness is polled once
 * a minute; nothing else runs.
 *
 * NB: this single-file mode still DOWNLOADS AND PARSES the whole file on
 * every page view (`async` affects execution order, not fetching). It is for
 * pages that do not mind ~88KB for a rare easter egg. A production site
 * should instead inline the external stub loader (marble-screensaver.js)
 * into its JS bundle and never reference this file from HTML: the stub
 * injects this file, un-attributed, only after the idle hour.
 * ========================================================================== */
(function () {
    'use strict';
    var script = document.currentScript;
    var screensaver = !!(script && script.dataset && ('screensaver' in script.dataset));
    if (!screensaver) {
        window.__marbleStart().catch(function (e) { console.error(e); });
        return;
    }
    if (window !== window.top) return;   // not inside iframes/popups

    var IDLE_MS  = 60 * 60 * 1000;       // 1 hour
    var CHECK_MS = 60 * 1000;
    var lastActivity = Date.now();
    function activity() { lastActivity = Date.now(); }
    ['pointermove', 'pointerdown', 'keydown', 'wheel', 'scroll', 'touchstart']
        .forEach(function (ev) {
            window.addEventListener(ev, activity, { passive: true, capture: true });
        });
    document.addEventListener('visibilitychange', activity);

    setInterval(function () {
        if (document.hidden                       // background tab
            || window.__marbleStop                // already marbling
            || window.__marbleStarting            // already starting
            || (window.matchMedia                 // re-checked at trigger time
                && matchMedia('(prefers-reduced-motion: reduce)').matches)
            || Date.now() - lastActivity < IDLE_MS) return;
        lastActivity = Date.now();                // re-arm for the next idle hour
        window.__marbleStart().catch(function (e) { console.error(e); });
    }, CHECK_MS);
})();
