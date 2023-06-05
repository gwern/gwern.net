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
			[ Typography.replacementTypes.MISC,			Typography.replacementDefinitionGroups.misc			],
			[ Typography.replacementTypes.SOFTHYPHENS,	Typography.replacementDefinitionGroups.softHyphens	],
			[ Typography.replacementTypes.JOINERS,		Typography.replacementDefinitionGroups.joiners		],
			[ Typography.replacementTypes.SEPARATORS,	Typography.replacementDefinitionGroups.separators	]
		];
		for ([ replacementTypeCode, replacementGroup ] of replacementTypeDefinitions) {
			if (types & replacementTypeCode)
				for (replacement of replacementGroup)
					allReplacements.push(replacement);
		}
		return allReplacements;
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
			[ /'''/g, retainLength => '\u2034' + (retainLength ? '\u2063\u2063' : '') ],
			// beginning "
			[ /([\s([]|^)"([^\s?!.,;\/)])/g, '$1\u201c$2' ],
			// ending "
			[ /(\u201c[^"]*)"([^"]*$|[^\u201c"]*(?=\u201c))/g, '$1\u201d$2' ],
			// remaining " at end of word
			[ /([^0-9])"/g, '$1\u201d' ],
			// double quotes
			[ /"(.+?)"/g, '\u201c$1\u201d' ],
			// double prime as two single quotes
			[ /''/g, retainLength => '\u2033' + (retainLength ? '\u2063' : '') ],
			// beginning '
			[ /(\W|^)'(\S)/g, '$1\u2018$2' ],
			// conjunction's possession
			[ /([a-z0-9])'([a-z])/ig, '$1\u2019$2' ],
			// abbrev. years like '93
			[ /(\u2018)([0-9]{2}[^\u2019]*)(\u2018([^0-9]|$)|$|\u2019[a-z])/ig, '\u2019$2$3' ],
			// ending '
			[ /((\u2018[^']*)|[a-z])'([^0-9]|$)/ig, '$1\u2019$3' ],
			// backwards apostrophe
			[ /(\B|^)\u2018(?=([^\u2018\u2019]*\u2019\b)*([^\u2018\u2019]*\B\W[\u2018\u2019]\b|[^\u2018\u2019]*$))/ig, '$1\u2019' ],
			// double prime
			[ /"/g, '\u2033' ],
			// prime
			[ /'/g, '\u2032' ]
		],
		hyphens: [
			// turn a hyphen surrounded by spaces, between words, into an em-dash
			[ /([a-z\u201d]) - ([a-z\u201c])/ig, retainLength => (retainLength ? '$1\u2063\u2014\u2063$2' : '$1\u2014$2') ],
			// turn a hyphen between a space and a quote, into an em-dash
			[ /([a-z]) -(\u201d)/ig, retainLength => (retainLength ? '$1\u2063\u2014$2' : '$1\u2014$2') ],
			[ /(\u201c)- ([a-z])/ig, retainLength => (retainLength ? '$1\u2014\u2063$2' : '$1\u2014$2') ],
			// turn a double or triple hyphen, optionally surrounded by spaces, between words, or at the start of a line, into an em-dash
			[ /([a-z"'“”‘’]|\n)( ?---? ?)([a-z"'“”‘’])/ig, retainLength => (retainLength
				? (m0, m1, m2, m3) => (m1 + '\u2014'.padStart(m2.length - 1, '\u2063') + m3)
				: (m0, m1, m2, m3) => (m1 + '\u2014' + m3)
			  ) ],
			// turn a hyphen surrounded by spaces, between decimal digits, into an en-dash
			[ /([0-9]) - ([0-9])/g, retainLength => (retainLength ? '$1\u2063\u2013\u2063$2' : '$1\u2013$2') ]
		],
		ellipses: [
			// Ellipsis rectification.
			[ /(^|\s)\.\.\./g, retainLength => (retainLength ? '$1…\u2063\u2063' : '$1…') ],
			[ /\.\.\.(\s|$)/g, retainLength => (retainLength ? '\u2063\u2063…$1' : '…$1') ]
		],
		arrows: [
			// Arrows
			[ /(\s)->(\s)/g, retainLength => (retainLength ? '$1\u2063\u2192$2' : '$1\u2192$2') ],
			[ /(\s)<-(\s)/g, retainLength => (retainLength ? '$1\u2192\u2063$2' : '$1\u2190$2') ],
			[ /(\s)=>(\s)/g, retainLength => (retainLength ? '$1\u2063\u2192$2' : '$1\u21d2$2') ],
			[ /(\s)<=(\s)/g, retainLength => (retainLength ? '$1\u2192\u2063$2' : '$1\u21d0$2') ],
			[ /(\s)<=>(\s)/g, retainLength => (retainLength ? '$1\u2063\u2192\u2063$2' : '$1\u21d4$2') ]
		],
		/*	This replacement type adds length, so the ‘retainLength’ trick does
			not work. See the ‘processElement’ method for how we deal with this.
		 */
		wordbreaks: [
			// Word-breaks after slashes (for long URLs etc.).
			[ /.\/+/g, '$&\u200b' ],
		],
		misc: [
			// Convert nbsp to regular space.
			[ /\xa0/g, ' ' ],
			// Two spaces after a period is INCORRECT.
			[ /(\w[\.\?\!])[ \u00a0]{2}(\w)/g, retainLength => (retainLength ? '$1 \u2063$2' : '$1 $2') ],
			// Hyphen followed by a numeral (with an optional space first), becomes an actual minus sign.
			[ /(\s)-( ?)([0-9])/g, '$1\u2212$2$3' ]
		],
		softHyphens: [
			// Strip soft hyphens.
			[ /\u00ad/g, retainLength => (retainLength ? '\u2063' : '') ]
		],
		joiners: [
			// Strip joiners.
			[ /\u2060/g, retainLength => (retainLength ? '\u2063' : '') ]
		],
		separators: [
			// Strip zero-width spaces.
			[ /\u200b|&ZeroWidthSpace;/g, retainLength => (retainLength ? '\u2063' : '') ],
			// Strip hair spaces.
			[ /\u200a|&hairsp;/g, retainLength => (retainLength ? '\u2063' : '') ],
		]
	},
	processString: (str, replacementTypes = Typography.replacementTypes.NONE, options = { }) => {
		Typography.replacements(replacementTypes).forEach(replace => {
			replacement = typeof replace[1] === "function"
						  ? replace[1](options.retainLength)
						  : replace[1];
			str = str.replace(replace[0], replacement);
		});
		return str;
	},
	substringSansSeparators: (text, value, position) => {
		return text.substr(position, value.length).replace(/\u2063/g, '');
	},
	replaceZeroWidthSpaces: (element) => {
		let replacements = [ ];
		for (node of element.childNodes) {
			if (node.nodeType === Node.ELEMENT_NODE) {
				Typography.replaceZeroWidthSpaces(node);
			} else if (node.nodeType === Node.TEXT_NODE) {
				let zwsRegExp = new RegExp(Typography.replacementDefinitionGroups.separators.map(x => x[0].source).join("|"), "g");
				let parts = [ ];
				let start = 0;
				let match = null;
				while (match = zwsRegExp.exec(node.textContent)) {
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
			let prevNodeIsWBR = false;
			for (let i = 0; i < element.childNodes.length; i++) {
				let node = element.childNodes[i];
				if (   node.nodeType === Node.ELEMENT_NODE
					&& node.tagName == "WBR") {
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
	processElement: (element, replacementTypes = Typography.replacementTypes.NONE, replaceZeroWidthSpaces = true) => {
		if ([ 'CODE', 'PRE', 'SCRIPT', 'STYLE', 'NOSCRIPT' ].includes(element.nodeName))
			return;

		//	Word breaks require special treatment, because they add length.
		let doWordbreaks = (replacementTypes & Typography.replacementTypes.WORDBREAKS);
		replacementTypes &= ~(Typography.replacementTypes.WORDBREAKS);

		let text = "";
		let textNodes = [ ];

		//	We process everything *but* the wordbreaks rule in a non-local way.
		for (node of element.childNodes) {
			if (node.nodeType === Node.TEXT_NODE) {
				textNodes.push([ node, text.length ]);
				text += node.nodeValue;
			} else if (node.childNodes.length > 0) {
				text += Typography.processElement(node, replacementTypes, false);
			}
		}
		text = Typography.processString(text, replacementTypes, { retainLength: true });
		for (nodeInfo of textNodes)
			nodeInfo[0].nodeValue = Typography.substringSansSeparators(text, nodeInfo[0].nodeValue, nodeInfo[1]);

		/*	Word breaks cannot be processed non-locally; each text node must be
			handled individually, due to length increase from added word breaks.
		 */
		if (doWordbreaks) {
			for (node of element.childNodes) {
				if (node.nodeType === Node.TEXT_NODE) {
					node.nodeValue = Typography.processString(node.nodeValue, Typography.replacementTypes.WORDBREAKS);
				} else if (node.childNodes.length > 0) {
					Typography.processElement(node, Typography.replacementTypes.WORDBREAKS, false);
				}
			}
		}

		//  Transform zero-width spaces into <wbr> tags.
		if (replaceZeroWidthSpaces)
			Typography.replaceZeroWidthSpaces(element);

		return text;
	}
};
