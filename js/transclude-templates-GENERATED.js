Transclude.templates = {
	"annotation-blockquote-inside": `<div class="annotation<{annotationClassSuffix}>">
	<p class="data-field title <[IF authorDateAux]>author-date-aux<[IFEND]>">
		<a 
		   class="<{titleLinkClass}>"
		   title="Open <{titleLinkHref}> in <{whichTab}> <{tabOrWindow}>"
		   href="<{titleLinkHref}>"
		   target="<{linkTarget}>"
		   <{titleLinkDataAttributes}>
			   ><{title}></a>\\
		<[IF [ abstract | fileIncludes ] & !authorDateAux ]><span class="data-field-separator">:</span><[IFEND]>\\
		<[IF authorDateAux]><[IF2 author]>,\\ <[IF2END]><{authorDateAux}><[IF2 [ abstract | fileIncludes ] ]><span class="data-field-separator">:</span><[IF2END]><[IFEND]>
	</p>
	<[IF abstract]>
	<blockquote class="data-field annotation-abstract">
		<[IF2 thumbnailFigure]>
		<{thumbnailFigure}>
		<[IF2END]>
		<{abstract}>
		<[IF2 fileIncludes]>
		<div class="data-field file-includes"><{fileIncludes}></div>
		<[IF2END]>
	</blockquote>
	<[ELSE]>
		<[IF2 fileIncludes]>
		<div class="data-field file-includes"><{fileIncludes}></div>
		<[IF2END]>
	<[IFEND]>
</div>`,
	"annotation-blockquote-not": `<div class="annotation<{annotationClassSuffix}>">
	<[IF thumbnailFigure]>
	<{thumbnailFigure}>
	<[IFEND]>
	<p class="data-field title">
		<a 
		   class="<{titleLinkClass}>"
		   title="Open <{titleLinkHref}> in <{whichTab}> <{tabOrWindow}>"
		   href="<{titleLinkHref}>"
		   target="<{linkTarget}>"
		   <{titleLinkDataAttributes}>
			   ><{title}></a>
	</p>
	<[IF authorDateAux]>
	<p class="data-field author-date-aux"><{authorDateAux}></p>
	<[IFEND]>
	<[IF abstract]>
	<div class="data-field annotation-abstract"><{abstract}></div>
	<[IFEND]>
	<[IF fileIncludes]>
	<div class="data-field file-includes"><{fileIncludes}></div>
	<[IFEND]>
</div>`,
	"annotation-blockquote-outside": `<blockquote class="annotation<{annotationClassSuffix}>">
	<[IF thumbnailFigure]>
	<{thumbnailFigure}>
	<[IFEND]>
	<p class="data-field title">
		<a 
		   class="<{titleLinkClass}>"
		   title="Open <{titleLinkHref}> in <{whichTab}> <{tabOrWindow}>"
		   href="<{titleLinkHref}>"
		   target="<{linkTarget}>"
		   <{titleLinkDataAttributes}>
			   ><{title}></a>
	</p>
	<[IF authorDateAux]>
	<p class="data-field author-date-aux"><{authorDateAux}></p>
	<[IFEND]>
	<[IF abstract]>
	<div class="data-field annotation-abstract"><{abstract}></div>
	<[IFEND]>
	<[IF fileIncludes]>
	<div class="data-field file-includes"><{fileIncludes}></div>
	<[IFEND]>
</blockquote>`,
	"pop-frame-title-annotation": `<[IF popFrameTitleArchiveLinkHref]>
<a
    class="popframe-title-link"
    title="Open <{popFrameTitleArchiveLinkHref}> in <{whichTab}> <{tabOrWindow}>."
    href="<{popFrameTitleArchiveLinkHref}>"
    target="<{linkTarget}>"
        ><{popFrameTitleText}></a>
<[ELSE]>
<a
    class="popframe-title-link"
    href="<{popFrameTitleLinkHref}>"
    title="Open <{popFrameTitleLinkHref}> in <{whichTab}> <{tabOrWindow}>."
    target="<{linkTarget}>"
        ><{popFrameTitleText}></a>
<[IFEND]>
`,
	"pop-frame-title-standard": `<a
	class="popframe-title-link"
	href="<{popFrameTitleLinkHref}>"
	title="Open <{popFrameTitleLinkHref}> in <{whichTab}> <{tabOrWindow}>."
	target="<{linkTarget}>"
		><{popFrameTitleText}></a>`,
	"tweet-blockquote-not": `<div class="content-transform <{contentTypeClass}>">
	<p class="data-field tweet-links">
		<a 
		   class="<{authorLinkClass}>"
		   title="Open <{authorLinkHref}> in <{whichTab}> <{tabOrWindow}>"
		   href="<{titleLinkHref}>"
		   target="<{linkTarget}>"
		   <{authorLinkIconMetadata}>
			   ><{authorPlusAvatar}></a>\\
		on \\
		<a
		   class="<{tweetLinkClass}>" 
		   title="Open <{tweetLinkHref> in <{whichTab}> <{tabOrWindow}>"
		   href="<{tweetLinkHref}>" 
		   <{archivedTweetURLDataAttribute}> 
		   <{tweetLinkIconMetadata}>
		   	   ><{tweetDate}></a>
	</p>
	<div class="data-field tweet-content"><{tweetContent}></div>
</div>`,
	"tweet-blockquote-outside": `<blockquote class="content-transform <{contentTypeClass}>">
	<p class="data-field tweet-links">
		<a 
		   class="<{authorLinkClass}>"
		   title="Open <{authorLinkHref}> in <{whichTab}> <{tabOrWindow}>"
		   href="<{authorLinkHref}>"
		   target="<{linkTarget}>"
		   <{authorLinkIconMetadata}>
			   ><{authorPlusAvatar}></a>\\
		on \\
		<a
		   class="<{tweetLinkClass}>" 
		   title="Open <{tweetLinkHref> in <{whichTab}> <{tabOrWindow}>"
		   href="<{tweetLinkHref}>" 
		   <{archivedTweetURLDataAttribute}> 
		   <{tweetLinkIconMetadata}>
		   	   ><{tweetDate}></a>
	</p>
	<div class="data-field tweet-content"><{tweetContent}></div>
</blockquote>`,
	"wikipedia-entry-blockquote-inside": `<div class="content-transform <{contentTypeClass}>">
	<p class="data-field title">
		<a 
		   class="<{titleLinkClass}>"
		   title="Open <{titleLinkHref}> in <{whichTab}> <{tabOrWindow}>"
		   href="<{titleLinkHref}>"
		   target="<{linkTarget}>"
		   <{titleLinkDataAttributes}>
		   <{titleLinkIconMetadata}>
			   ><{title}></a>:
	</p>
	<blockquote class="data-field entry-content">
		<[IF thumbnailFigure]>
		<{thumbnailFigure}>
		<[IFEND]>
		<{entryContent}>
	</blockquote>
</div>`,
	"wikipedia-entry-blockquote-not": `<div class="content-transform <{contentTypeClass}>">
	<[IF thumbnailFigure]>
	<{thumbnailFigure}>
	<[IFEND]>
	<p class="data-field title">
		<a 
		   class="<{titleLinkClass}>"
		   title="Open <{titleLinkHref}> in <{whichTab}> <{tabOrWindow}>"
		   href="<{titleLinkHref}>"
		   target="<{linkTarget}>"
		   <{titleLinkDataAttributes}>
		   <{titleLinkIconMetadata}>
			   ><{title}></a>
	</p>
	<div class="data-field entry-content"><{entryContent}></div>
</div>`,
	"wikipedia-entry-blockquote-title-not": `<div class="content-transform <{contentTypeClass}>">
	<blockquote class="data-field entry-content">
		<[IF thumbnailFigure]>
		<{thumbnailFigure}>
		<[IFEND]>
		<{entryContent}>
	</blockquote>
</div>`,
};
