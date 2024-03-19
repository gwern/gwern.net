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
		let specifiedReplacements = [ ];
		let replacementTypeDefinitions = [
			[ Typography.replacementTypes.QUOTES,		Typography.replacementDefinitionGroups.quotes		],
			[ Typography.replacementTypes.HYPHENS,		Typography.replacementDefinitionGroups.hyphens		],
			[ Typography.replacementTypes.ELLIPSES,		Typography.replacementDefinitionGroups.ellipses		],
			[ Typography.replacementTypes.ARROWS,		Typography.replacementDefinitionGroups.arrows		],
			[ Typography.replacementTypes.WORDBREAKS,	Typography.replacementDefinitionGroups.wordbreaks	],
			[ Typography.replacementTypes.MISC,			Typography.replacementDefinitionGroups.misc			],
			[ Typography.replacementTypes.SOFTHYPHENS,	Typography.replacementDefinitionGroups.softHyphens	],
			[ Typography.replacementTypes.JOINERS,		Typography.replacementDefinitionGroups.joiners		],
			[ Typography.replacementTypes.SEPARATORS,	Typography.replacementDefinitionGroups.separators	]
		];
		for (let [ replacementTypeCode, replacementGroup ] of replacementTypeDefinitions) {
			if (types & replacementTypeCode)
				for (replacement of replacementGroup)
					specifiedReplacements.push(replacement);
		}
		return specifiedReplacements;
	},
	replacementTypes: {
		NONE:			0x0000,
		QUOTES:			0x0001,
		HYPHENS:		0x0002,
		ELLIPSES:		0x0004,
		ARROWS:			0x0008,
		WORDBREAKS:		0x0010,
		MISC:			0x0020,
		SOFTHYPHENS:	0x0040,
		JOINERS:		0x0080,
		SEPARATORS:		0x0100,
		CLEAN: 			(0x0040 + 0x0080 + 0x0100)
	},
	replacementDefinitionGroups: {
		quotes: [
			// triple prime
			[ /'''/, '\u2034' ],
			// beginning "
			[ /(?<=[\s([]|^)"(?=[^\s?!.,;\/)])/, '\u201c' ],
			// ending "
			[ /(?<=\u201c[^"]*)"(?=[^"]*$|[^\u201c"]*(?=\u201c))/, '\u201d' ],
			// remaining " at end of word
			[ /(?<=[^0-9])"/, '\u201d' ],
			// double quotes
			[ /"(.+?)"/, '\u201c$1\u201d' ],
			// double prime as two single quotes
			[ /''/, '\u2033' ],
			// beginning '
			[ /(?<=\W|^)'(?=\S)/, '\u2018' ],
			// conjunction's possession
			[ /(?<=[a-z0-9])'(?=[a-z])/i, '\u2019' ],
			// abbrev. years like '93
			[ /\u2018(?=(?:[0-9]{2}[^\u2019]*)(?:\u2018(?:[^0-9]|$)|$|\u2019[a-z]))/i, '\u2019' ],
			// ending '
			[ /(?<=(\u2018[^']*)|[a-z])'(?=[^0-9]|$)/i, '\u2019' ],
			// backwards apostrophe
			[ /(?<=\B|^)\u2018(?=([^\u2018\u2019]*\u2019\b)*([^\u2018\u2019]*\B\W[\u2018\u2019]\b|[^\u2018\u2019]*$))/i, '\u2019' ],
			// double prime
			[ /"/, '\u2033' ],
			// prime
			[ /'/, '\u2032' ]
		],
		hyphens: [
			// turn a hyphen surrounded by spaces, between words, into an em-dash
			[ /(?<=[a-z\u201d]) (-) (?=[a-z\u201c])/i, '\u2014' ],
			// turn a hyphen between a space and a quote, into an em-dash
			[ /(?<=[a-z]) (-)(?=\u201d)/i, '\u2014' ],
			[ /(?<=\u201c)(-) (?=[a-z])/i, '\u2014' ],
			// turn a double or triple hyphen, optionally surrounded by spaces, between words, or at the start of a line, into an em-dash
			[ /(?<=[a-z"'“”‘’]|\n) ?(---?) ?(?=[a-z"'“”‘’])/i, '\u2014' ],
			// turn a hyphen surrounded by spaces, between decimal digits, into an en-dash
			[ /(?<=[0-9]) (-) (?=[0-9])/, '\u2013' ]
		],
		ellipses: [
			// Ellipsis rectification.
			[ /(?<=^|\s)\.\.\./, '…' ],
			[ /\.\.\.(?=\s|$)/, '…' ]
		],
		arrows: [
			// Arrows
			[ /(?<=\s)->(?=\s)/, '\u2192' ],
			[ /(?<=\s)<-(?=\s)/, '\u2190' ],
			[ /(?<=\s)=>(?=\s)/, '\u21d2' ],
			[ /(?<=\s)<=(?=\s)/, '\u21d0' ],
			[ /(?<=\s)<=>(?=\s)/, '\u21d4' ]
		],
		wordbreaks: [
			// Word-breaks after slashes (for long URLs etc.).
			[ /(?<=.)\/+(?=[^\u200b\/])/, '$&\u200b' ],
		],
		misc: [
			// Convert nbsp to regular space.
			[ /\xa0/, ' ' ],
			// Two spaces after a period is INCORRECT.
			[ /(?<=\w[\.\?\!])[ \u00a0]{2}(?=\w)/, ' ' ],
			// Hyphen followed by a numeral (with an optional space first), becomes an actual minus sign.
			[ /(?<=\s)-( ?)(?=[0-9])/, '\u2212$1' ]
		],
		softHyphens: [
			// Strip soft hyphens.
			[ /\u00ad/, '' ]
		],
		joiners: [
			// Strip joiners.
			[ /\u2060/, '' ]
		],
		separators: [
			// Strip zero-width spaces.
			[ /\u200b|&ZeroWidthSpace;/, '' ],
			// Strip hair spaces.
			[ /\u200a|&hairsp;/, '' ],
		]
	},
	processString: (str, replacementTypes = Typography.replacementTypes.NONE, segments = null) => {
		if (segments == null)
			segments = [ str.length ];

		function segmentIndexAtOffset(segments, offset) {
			let currentSegmentStart = 0;
			for (let i = 0; i < segments.length; i++) {
				if (   offset >= currentSegmentStart
					&& offset < currentSegmentStart + segments[i])
					return i;

				currentSegmentStart += segments[i];
			}
			return -1;
		}

		Typography.replacements(replacementTypes).forEach(replacement => {
			let [ pattern, template ] = replacement;

			let globalPattern = new RegExp(pattern.source, pattern.flags + "g");
			let match = null;
			while (match = globalPattern.exec(str)) {
				let oldLength = str.length;
				str = str.replace(pattern, template);
				let lengthChange = str.length - oldLength;

				if (lengthChange == 0)
					continue;

				let segmentAtMatchStart = segmentIndexAtOffset(segments, match.index);
				let segmentAtMatchEnd = segmentIndexAtOffset(segments, match.index + match[0].length - 1);
				if (segmentAtMatchStart == segmentAtMatchEnd) {
					segments[segmentAtMatchStart] += lengthChange;
				} else {
					//	TODO: THIS!
				}
			}
		});

		return str;
	},
	excludedTags: [ 'PRE', 'SCRIPT', 'STYLE', 'NOSCRIPT' ],
	unmodifiedTags: [ 'CODE '],
	processElement: (element, replacementTypes = Typography.replacementTypes.NONE, rectifyWordBreaks = true) => {
		if (Typography.excludedTags.includes(element.nodeName))
			return null;

		function decomposeElement(element) {
			let text = "";
			let textNodes = [ ];

			if (Typography.excludedTags.includes(element.nodeName))
				return [ text, textNodes ];

			for (node of element.childNodes) {
				if (node.nodeType === Node.TEXT_NODE) {
					textNodes.push(node);
					text += node.nodeValue;
				} else if (node.childNodes.length > 0) {
					let [ subtext, subnodes ] = decomposeElement(node);
					text += subtext;
					textNodes.splice(textNodes.length, 0, ...subnodes);
				}
			}

			return [ text, textNodes ];
		}

		let [ text, textNodes ] = decomposeElement(element);
		let segments = textNodes.map(node => node.nodeValue.length);
		if (Typography.unmodifiedTags.includes(element.nodeName) == false)
			text = Typography.processString(text, replacementTypes, segments);
		let currentSegmentStart = 0;
		for (let i = 0; i < textNodes.length; i++) {
			textNodes[i].nodeValue = text.slice(currentSegmentStart, currentSegmentStart + segments[i]);
			currentSegmentStart += segments[i];
		}

		//  Transform separators into <wbr> tags.
		if (rectifyWordBreaks)
			Typography.rectifyWordBreaks(element);

		return text;
	},
	rectifyWordBreaks: (element) => {
		let replacements = [ ];
		for (node of element.childNodes) {
			if (node.nodeType === Node.ELEMENT_NODE) {
				Typography.rectifyWordBreaks(node);
			} else if (node.nodeType === Node.TEXT_NODE) {
				let sepRegExp = new RegExp(Typography.replacementDefinitionGroups.separators.map(x => x[0].source).join("|"), "g");
				let parts = [ ];
				let start = 0;
				let match = null;
				while (match = sepRegExp.exec(node.textContent)) {
					parts.push([ start, match.index ]);
					start = match.index + match[0].length;
				}
				if (parts.length > 0) {
					let replacementNodes = [ ];
					parts.forEach(part => {
						if (part[1] > part[0])
							replacementNodes.push(document.createTextNode(node.textContent.slice(...part)));
						replacementNodes.push(newElement("WBR"));
					});
					replacementNodes.push(document.createTextNode(node.textContent.slice(start)));
					replacements.push([ node, replacementNodes ]);
				}
			}
		}
		if (replacements.length > 0) {
			//	Replace.
			replacements.forEach(replacement => {
				let [ replacedNode, replacementNodes ] = replacement;
				replacedNode.parentNode.replaceChild(newDocument(replacementNodes), replacedNode);
			});

			//	Remove all but one of each set of consecutive <wbr> tags.
			function isWBR(node) {
				return (   node.nodeType === Node.ELEMENT_NODE
						&& node.tagName == "WBR");
			}

			function isEmptyTextNode(node) {
				return (   node.nodeType === Node.TEXT_NODE
						&& isNodeEmpty(node) == true);
			}

			let prevNodeIsWBR = false;
			for (let i = 0; i < element.childNodes.length; i++) {
				let node = element.childNodes[i];
				if (isWBR(node) && prevNodeIsWBR == false) {
					prevNodeIsWBR = true;
				} else if (prevNodeIsWBR) {
					if (   isWBR(node) 
						|| isEmptyTextNode(node)) {
						node.remove();
						i--;
					} else {
						prevNodeIsWBR = false;
					}
				}
			}
		}
	}
};
