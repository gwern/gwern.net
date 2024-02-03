Transclude.templates = {
	"annotation-blockquote-inside": `<div class="annotation<{annotationClassSuffix}> <{dataSourceClass}>">
	<p class="data-field title <[IF authorDateAux]>author-date-aux<[IFEND]>">
		<a 
		   class="<{titleLinkClass}>"
		   title="Open <{titleLinkHref}> in <{whichTab}> <{tabOrWindow}>"
		   href="<{titleLinkHref}>"
		   <[IF linkTarget]>target="<{linkTarget}>"<[IFEND]>
		   <[IF titleLinkDataAttributes]><{titleLinkDataAttributes}><[IFEND]>
		   <{titleLinkIconMetadata}>
			   ><{fullTitleHTML}></a>\\
		<[IF secondaryTitleLinksHTML]><span class="secondary-title-links"><{secondaryTitleLinksHTML}></span><[IFEND]>\\
		<[IF abstract & !authorDateAux ]>:<[IFEND]>\\
		<[IF authorDateAux]><[IF2 author]>,\\ <[IF2END]><{authorDateAux}><[IF2 abstract]>:<[IF2END]><[IFEND]>
	</p>
	<[IF abstract]>
	<blockquote class="data-field annotation-abstract"><{abstract}></blockquote>
	<[IFEND]>
</div>`,
	"annotation-blockquote-not": `<div class="annotation<{annotationClassSuffix}> <{dataSourceClass}>">
	<p class="data-field title">
		<a 
		   class="<{titleLinkClass}>"
		   title="Open <{titleLinkHref}> in <{whichTab}> <{tabOrWindow}>"
		   href="<{titleLinkHref}>"
		   <[IF linkTarget]>target="<{linkTarget}>"<[IFEND]>
		   <[IF titleLinkDataAttributes]><{titleLinkDataAttributes}><[IFEND]>
		   <{titleLinkIconMetadata}>
			   ><{titleHTML}></a>\\
		<[IF secondaryTitleLinksHTML]><span class="secondary-title-links"><{secondaryTitleLinksHTML}></span><[IFEND]>
	</p>
	<[IF authorDateAux]>
	<p class="data-field author-date-aux"><{authorDateAux}></p>
	<[IFEND]>
	<[IF abstract]>
	<div class="data-field annotation-abstract"><{abstract}></div>
	<[IFEND]>
</div>`,
	"annotation-blockquote-outside": `<blockquote class="annotation<{annotationClassSuffix}> <{dataSourceClass}>">
	<p class="data-field title">
		<a 
		   class="<{titleLinkClass}>"
		   title="Open <{titleLinkHref}> in <{whichTab}> <{tabOrWindow}>"
		   href="<{titleLinkHref}>"
		   <[IF linkTarget]>target="<{linkTarget}>"<[IFEND]>
		   <[IF titleLinkDataAttributes]><{titleLinkDataAttributes}><[IFEND]>
		   <{titleLinkIconMetadata}>
			   ><{titleHTML}></a>\\
		<[IF secondaryTitleLinksHTML]><span class="secondary-title-links"><{secondaryTitleLinksHTML}></span><[IFEND]>
	</p>
	<[IF authorDateAux]>
	<p class="data-field author-date-aux"><{authorDateAux}></p>
	<[IFEND]>
	<[IF abstract]>
	<div class="data-field annotation-abstract"><{abstract}></div>
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
