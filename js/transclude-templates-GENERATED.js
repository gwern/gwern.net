Transclude.templates = {
	"annotation-blockquote-inside": `<div class="annotation<{annotationClassSuffix}> <{dataSourceClass}>">
	<[IF fullTitleHTML]>
	<p class="data-field title <[IF2 authorDateAux]>author-date-aux<[IF2END]>">
		<a 
		   class="<{titleLinkClass}>"
		   title="Open <{titleLinkHref}> in <{whichTab}> <{tabOrWindow}>"
		   href="<{titleLinkHref}>"
		   <[IF2 linkTarget]>target="<{linkTarget}>"<[IF2END]>
		   <[IF2 titleLinkDataAttributes]><{titleLinkDataAttributes}><[IF2END]>
		   <{titleLinkIconMetadata}>
			   ><{fullTitleHTML}></a>\\
		<[IF2 secondaryTitleLinksHTML]><span class="secondary-title-links"><{secondaryTitleLinksHTML}></span><[IF2END]>\\
		<[IF2 [ abstract | fileIncludes ] & !authorDateAux & ! [ annotationClassSuffix "-partial" ] ]>:<[IF2END]>\\
		<[IF2 authorDateAux]><[IF3 author]>,\\ <[IF3END]><{authorDateAux}><[IF3 [ abstract | fileIncludes ] & ! [ annotationClassSuffix "-partial" ] ]>:<[IF3END]><[IF2END]>
	</p>
	<[IFEND]>
	<[IF abstract]>
	<blockquote class="data-field annotation-abstract">
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
	"annotation-blockquote-not": `<div class="annotation<{annotationClassSuffix}> <{dataSourceClass}>">
	<[IF fullTitleHTML]>
	<p class="data-field title">
		<a 
		   class="<{titleLinkClass}>"
		   title="Open <{titleLinkHref}> in <{whichTab}> <{tabOrWindow}>"
		   href="<{titleLinkHref}>"
		   <[IF2 linkTarget]>target="<{linkTarget}>"<[IF2END]>
		   <[IF2 titleLinkDataAttributes]><{titleLinkDataAttributes}><[IF2END]>
		   <{titleLinkIconMetadata}>
			   ><{fullTitleHTML}></a>\\
		<[IF2 secondaryTitleLinksHTML]><span class="secondary-title-links"><{secondaryTitleLinksHTML}></span><[IF2END]>
	</p>
	<[IFEND]>
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
	"annotation-blockquote-outside": `<blockquote class="annotation<{annotationClassSuffix}> <{dataSourceClass}>">
	<[IF fullTitleHTML]>
	<p class="data-field title">
		<a 
		   class="<{titleLinkClass}>"
		   title="Open <{titleLinkHref}> in <{whichTab}> <{tabOrWindow}>"
		   href="<{titleLinkHref}>"
		   <[IF2 linkTarget]>target="<{linkTarget}>"<[IF2END]>
		   <[IF2 titleLinkDataAttributes]><{titleLinkDataAttributes}><[IF2END]>
		   <{titleLinkIconMetadata}>
			   ><{fullTitleHTML}></a>\\
		<[IF2 secondaryTitleLinksHTML]><span class="secondary-title-links"><{secondaryTitleLinksHTML}></span><[IF2END]>
	</p>
	<[IFEND]>
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
};
