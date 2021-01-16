/*	Typography.js
	(Copyright 2020 Said Achmiz)
	MIT License
	
	is based on 

	https://github.com/kellym/smartquotes.js
	(Copyright 2013 Kelly Martin)
	MIT License

	and

	https://github.com/kronusaturn/lw2-viewer
	(Copyright 2018 kronusaturn)
	MIT License
	*/

Typography = {
	replacements: (types) => {
		let allReplacements = [ ];
		let replacementTypeDefinitions = [
			[ Typography.replacementTypes.QUOTES,		Typography.replacementDefinitionGroups.quotes		],
			[ Typography.replacementTypes.HYPHENS,		Typography.replacementDefinitionGroups.hyphens		],
			[ Typography.replacementTypes.ELLIPSES,		Typography.replacementDefinitionGroups.ellipses		],
			[ Typography.replacementTypes.ARROWS,		Typography.replacementDefinitionGroups.arrows		],
			[ Typography.replacementTypes.WORDBREAKS,	Typography.replacementDefinitionGroups.wordbreaks	],
			[ Typography.replacementTypes.MISC,			Typography.replacementDefinitionGroups.misc			]
		];
		for ([ replacementTypeCode, replacementGroup ] of replacementTypeDefinitions) {
			if (types & replacementTypeCode)
				for (replacement of replacementGroup)
					allReplacements.push(replacement);
		}
		return allReplacements;
	},
	replacementTypes: {
		QUOTES:		0x0001,
		HYPHENS:	0x0002,
		ELLIPSES:	0x0004,
		ARROWS:		0x0008,
		WORDBREAKS:	0x0010,
		MISC:		0x0020,
		ALL: 		(0x0001 + 0x0002 + 0x0004 + 0x0008 + 0x0010 + 0x0020)
	},
	replacementDefinitionGroups: {
		quotes: [
			// triple prime
			[/'''/g, '\u2034'],
			// beginning "
			[/([\s([]|^)"([^\s?!.,;\/)])/g, '$1\u201c$2'],
			// ending "
			[/(\u201c[^"]*)"([^"]*$|[^\u201c"]*\u201c)/g, '$1\u201d$2'],
			// remaining " at end of word
			[/([^0-9])"/g, '$1\u201d'],
			// double quotes
			// NOTE: This rule cannot be used until the code is de-localized.
// 			[/"(.+?)"/g, '\u201c$1\u201d'],
			// double prime as two single quotes
			[/''/g, '\u2033'],
			// beginning '
			[/(\W|^)'(\S)/g, '$1\u2018$2'],
			// conjunction's possession
			[/([a-z])'([a-z])/ig, '$1\u2019$2'],
			// abbrev. years like '93
			[/(\u2018)([0-9]{2}[^\u2019]*)(\u2018([^0-9]|$)|$|\u2019[a-z])/ig, '\u2019$2$3'],
			// ending '
			[/((\u2018[^']*)|[a-z])'([^0-9]|$)/ig, '$1\u2019$3'],
			// backwards apostrophe
			[/(\B|^)\u2018(?=([^\u2018\u2019]*\u2019\b)*([^\u2018\u2019]*\B\W[\u2018\u2019]\b|[^\u2018\u2019]*$))/ig, '$1\u2019'],
			// double prime
			[/"/g, '\u2033'],
			// prime
			[/'/g, '\u2032']
		],
		hyphens: [
			// turn a hyphen surrounded by spaces, between words, into an em-dash
			[/([a-z\u201d]) - ([a-z\u201c])/ig, '$1\u2014$2'],
			// turn a hyphen between a space and a quote, into an em-dash
			[/([a-z]) -(\u201d)/ig, '$1\u2014$2'],
			[/(\u201c)- ([a-z])/ig, '$1\u2014$2'],
			// turn a double or triple hyphen, optionally surrounded by spaces, between words, or at the start of a line, into an em-dash
			[/([a-z"'“”‘’]|\n) ?---? ?([a-z"'“”‘’])/ig, '$1\u2014$2'],
			// turn a hyphen surrounded by spaces, between decimal digits, into an en-dash
			[/([0-9]) - ([0-9])/g, '$1\u2013$2' ]
		],
		ellipses: [
			// ellipsis rectification
			[/(^|\s)\.\.\./g, '$1…'],
			[/\.\.\.(\s|$)/g, '…$1']
		],
		arrows: [
			// Arrows
			[/(\s)->(\s)/g, '$1\u2192$2'],
			[/(\s)<-(\s)/g, '$1\u2190$2'],
			[/(\s)=>(\s)/g, '$1\u21d2$2'],
			[/(\s)<=(\s)/g, '$1\u21d0$2'],
			[/(\s)<=>(\s)/g, '$1\u21d4$2']
		],
		wordbreaks: [
			// word-breaks after slashes (for long URLs etc.)
			[/\/+/g, '$&\u200b'],
		],
		misc: [
			// convert nbsp to regular space
			[/\xa0/g, ' '],
			// Two spaces after a period is INCORRECT.
			[ /(\w[\.\?\!])[ \u00a0]{2}(\w)/g, '$1 $2'],
			// Hyphen followed by a numeral (with an optional space first), becomes an actual minus sign
			[/(\s)-( ?)([0-9])/g, '$1\u2212$2$3']
		]
	},
	processString: (str, replacementTypes = Typography.replacementTypes.ALL) => {
		Typography.replacements(replacementTypes).forEach(replace => {
			GWLog("---", "typography.js", 3);
			GWLog(replace[0], "typography.js", 3);
			GWLog(str, "typography.js", 3);
			str = str.replace(replace[0], replace[1]);
			GWLog(str, "typography.js", 3);
		});
		return str;
	},
	replaceZeroWidthSpaces: (element) => {
		var mustReplace = false;
		for (let i = 0; i < element.childNodes.length; i++) {
			let node = element.childNodes[i];
			if (node.nodeType === Node.ELEMENT_NODE) {
				Typography.replaceZeroWidthSpaces(node);
			} else if (   node.nodeType === Node.TEXT_NODE
					   && node.nodeValue.match(/\u200b/) != null) {
				mustReplace = true;
			}
		}
		if (mustReplace) {
			// Replace U+200B ZERO-WIDTH SPACE with <wbr> tag.
			element.innerHTML = element.innerHTML.replace(/\u200b/g, "<wbr>");

			// Remove all but one of each set of consecutive <wbr> tags.
			var prevNodeIsWBR = false;
			for (let i = 0; i < element.childNodes.length; i++) {
				let node = element.childNodes[i];
				if (node.nodeType === Node.ELEMENT_NODE && node.tagName == "WBR") {
					if (prevNodeIsWBR) {
						node.remove();
						i--;
					} else {
						prevNodeIsWBR = true;
					}
				} else {
					prevNodeIsWBR = false;
				}
			}
		}
	},
	processElement: (element, replacementTypes = Typography.replacementTypes.ALL, replaceZeroWidthSpaces = true) => {
		if ([ 'CODE', 'PRE', 'SCRIPT', 'STYLE', 'NOSCRIPT' ].includes(element.nodeName))
			return;

		for (let i = 0; i < element.childNodes.length; i++) {
			let node = element.childNodes[i];

			if (node.nodeType === Node.TEXT_NODE) {
				node.nodeValue = Typography.processString(node.nodeValue, replacementTypes);
			} else if (node.childNodes.length > 0) {
				Typography.processElement(node, replacementTypes, false);
			}
		}

		//  Transform zero-width spaces into <wbr> tags.
		if (replaceZeroWidthSpaces)
			Typography.replaceZeroWidthSpaces(element);
	}
};
