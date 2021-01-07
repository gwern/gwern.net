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
	replacements: [
		// convert nbsp to regular space
		[/\xa0/, ' '],
		// line-breaks after slashes (for long URLs etc.)
		[/\/+/, '\\&\u200b'],

		// triple prime
		[/'''/g, '\u2063\u2063\u2034'],
		// beginning "
		[/([\s([]|^)"([^\s?!.,;\/)])/g, '$1\u201c$2'],
		// ending "
		[/(\u201c[^"]*)"([^"]*$|[^\u201c"]*\u201c)/g, '$1\u201d$2'],
		// remaining " at end of word
		[/([^0-9])"/g, '$1\u201d'],
		// double quotes
		[/"(.+?)"/g, '\u201c$1\u201d'],
		// double prime as two single quotes
		[/''/g, '\u2063\u2033'],
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
		[/'/g, '\u2032'],
	
		// turn a hyphen surrounded by spaces, between words, into an em-dash
		[/([a-z\u201d]) - ([a-z\u201c])/ig, '$1\u2014$2'],
		// turn a hyphen between a space and a quote, into an em-dash
		[/([a-z]) -(\u201d)/ig, '$1\u2014$2'],
		[/(\u201c)- ([a-z])/ig, '$1\u2014$2'],
		// turn a double or triple hyphen, optionally surrounded by spaces, between words, or at the start of a line, into an em-dash
		[/([a-z"'“”‘’]|\n) ?---? ?([a-z"'“”‘’])/ig, '$1\u2014$2'],
	
		// Two spaces after a period is INCORRECT.
		[ /(\w[\.\?\!])[ \u00a0]{2}(\w)/g, '$1 $2'],
	
		// ellipsis rectification
		[/(\s)\.\.\./g, '$1…'],
		[/\.\.\.(\s)/g, '…$1'],
	
		// Hyphen followed by a numeral (with an optional space first), becomes an actual minus sign
		[/(\s)-( ?)([0-9])/g, '$1\u2212$2$3'],
	
		// Arrows
		[/(\s)->(\s)/g, '$1\u2192$2'],
		[/(\s)<-(\s)/g, '$1\u2190$2'],
		[/(\s)=>(\s)/g, '$1\u21d2$2'],
		[/(\s)<=(\s)/g, '$1\u21d0$2'],
		[/(\s)<=>(\s)/g, '$1\u21d4$2']
	],
	processString: (str, clean = true) => {
		Typography.replacements.forEach(replace => {
			str = str.replace(replace[0], replace[1]);
		});
		return clean ? Typography.cleanString(str) : str;
	},
	cleanString: (str) => {
		return str.replace('\u2063', '');
	},
	processElement: (element) => {
		if ([ 'CODE', 'PRE', 'SCRIPT', 'STYLE', 'NOSCRIPT' ].includes(element.nodeName))
			return;

		let text = '';
		let textNodes = [ ];

		//  Compile all text first so we handle working around child nodes.
		for (let i = 0; i < element.childNodes.length; i++) {
			let node = element.childNodes[i];

			if (node.nodeType === Node.TEXT_NODE) {
				textNodes.push({ node: node, startpos: text.length });
				text += node.nodeValue;
			} else if (node.childNodes.length > 0) {
				text += Typography.processElement(node);
			}
		}

		//  Do the replacements (do not clean invisible separators yet).
		text = Typography.processString(text, false);

		//  Clean invisible separators.
		for (nodeInfo of textNodes)
			nodeInfo.node.nodeValue = Typography.cleanString(text.substr(nodeInfo.startpos, nodeInfo.node.nodeValue.length));

		return text;
	}
};

