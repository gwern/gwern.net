<?php
/*	See /static/build/build_font_css.php for details on the format of this file.

	Font overview:

	- Source Serif Pro
		workhorse font for body text
	- Source Sans Pro
		for ToC and UI elements; primarily intended for Mac/iOS users
	- IBM Plex Mono
		for code blocks
	- Initial-capital fonts 
		for 5 kinds of dropcaps; enabled in /static/css/default.css
		- Deutsche Zierschrift
			for general pages
		- Yinit
			for technical/scientific pages
			https://www.tug.org/TUGboat/tb12-1/tb31hara.pdf#page=8
		- Goudy
			for humanities/literature/history
			https://wiki.obormot.net/Main/BonusFontsDemo?demo_font_one=Goudy+Initialen
		- Cheshire
		- Kanzlei
	- Quivira and Noto Emoji
		for specific symbols; e.g. Unicode linkicons

	NOTE on ‘font-display: swap’:
	We display text as soon as possible (e.g., for body text, system font such 
	as Baskerville; see font stacks defined in /static/css/initial.css)
	and re-render with webfont (e.g. Source Serif Pro) whenever it downloads:
	https://developers.google.com/web/updates/2016/02/font-display
	https://developer.mozilla.org/en-US/docs/Web/CSS/@font-face/font-display
	https://caniuse.com/#search=font-display
 */
?>

!inline
Source Serif Pro
italic	-
`400	*
`600	*
`700	*

!inline
Source Serif Pro
italic	+
`400	*

!inline
Source Sans Pro
italic	-
`400	*
`700	*

!inline
Source Sans Pro
italic	+
`400	*

Source Serif Pro
/static/font/ssfp/SourceSerifPro-BASIC-
ttf
font-display	swap
italic			/$/		Italic
unicode-range	U+0020-007E, U+00A0-00FF, U+2010, U+2013-2014, U+2018-2019, U+201C-201D, U+2212
`200	ExtraLight
`300	Light
`400	Regular
`600	Semibold
`700	Bold
`900	Black

Source Sans Pro
/static/font/ssp/SourceSansPro-BASIC-
ttf
font-display	swap
italic			/$/		Italic
unicode-range	U+0020-007E, U+00A0-00FF, U+2010, U+2013-2014, U+2018-2019, U+201C-201D, U+2212
`200	ExtraLight
`300	Light
`400	Regular
`600	Semibold
`700	Bold
`900	Black

IBM Plex Mono
/static/font/ibm-plex-mono/IBMPlexMono-
otf
font-display	swap
italic			/$/		Italic
`normal			Regular
`bold			Bold

Quivira
/static/font/quivira/Quivira-
ttf
font-display	swap
`normal			SUBSETTED

Noto Emoji
/static/font/noto-emoji/NotoEmoji-
ttf
font-display	swap
`normal			Bold-SUBSETTED

<?php
/*	Each dropcap font is subsetted into A-Z, so only *one* letter needs to be 
	loaded (at a cost of 8-16KB), rather than 200-700KB or worse.
 */
function all_the_letters() {
	for ($c = 0x41; $c <= 0x5A; $c++) {
		/*	Example (sans leading tabs; backtick is at line start):
			`	A
				unicode-range	U+0041
		 */
		echo "`	". chr($c) . "\n";
		echo "	unicode-range	U+00" . strtoupper(dechex($c)) . "\n";
	}
}
?>

Deutsche Zierschrift
/static/font/dropcap/de-zs/DeutscheZierschrift-
ttf
font-display	swap
<?php all_the_letters(); ?>

Yinit
/static/font/dropcap/yinit/Yinit-
ttf
font-display	swap
<?php all_the_letters(); ?>

Goudy Initialen
/static/font/dropcap/goudy/GoudyInitialen-
ttf
font-display	swap
<?php all_the_letters(); ?>

Cheshire Initials
/static/font/dropcap/cheshire/Cheshire-Initials-
ttf
font-display	swap
<?php all_the_letters(); ?>

Kanzlei Initialen
/static/font/dropcap/kanzlei/Kanzlei-Initialen-
ttf
font-display	swap
<?php all_the_letters(); ?>

Blackmoor Plain
/static/font/blackletter/BlackmoorPlain
otf
font-display	swap
`normal			`

Cloister Black
/static/font/blackletter/CloisterBlack
ttf
font-display	swap
`normal			`

Deutsche Schrift
/static/font/blackletter/DeutscheSchrift
ttf
font-display	swap
`normal			`

Engravers Old English
/static/font/blackletter/EngraversOldEnglish-
ttf
font-display	swap
`normal			Regular
`bold			Bold

Great Primer Uncials
/static/font/blackletter/GreatPrimerUncials
otf
font-display	swap
`normal			`

Gutenberg Gothic
/static/font/blackletter/GutenbergGothic
ttf
font-display	swap
`normal			`

Hansa Gothic
/static/font/blackletter/HansaGothic
ttf
font-display	swap
`normal			`
