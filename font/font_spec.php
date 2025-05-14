<?php
/*	See /static/build/build_font_css.php for details on the format of this file.

	Font overview:

	- Source Serif 4
		workhorse font for body text
	- Source Sans 3
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
	and re-render with webfont (e.g. Source Serif 4) whenever it downloads:
	https://developers.google.com/web/updates/2016/02/font-display
	https://developer.mozilla.org/en-US/docs/Web/CSS/@font-face/font-display
	https://caniuse.com/#search=font-display
 */
?>

!inline
Source Serif 4
italic	-
`400	*
`600	*
`700	*

!inline
Source Serif 4
italic	+
`400	*

!inline
Source Sans 3
italic	-
`400	*
`700	*

!inline
Source Sans 3
italic	+
`400	*

Source Serif 4
/static/font/ssf4/SourceSerif4-Text-BASIC-
otf
font-display	swap
italic			/$/		Italic
unicode-range	U+0020-007E, U+00A0-00FF, U+0152, U+0153, U+017F, U+018F, U+0192, U+0259, U+02C6-02CC, U+02D8-02DD, U+0300-0304, U+0306-030C, U+0327, U+0391-03A1, U+03A3-03A9, U+03B1-03C9, U+2002-2007, U+2009-200B, U+2011-2015, U+2018-201A, U+201C-201E, U+2020-2022, U+2025-2026, U+202F, U+2030, U+2032-2033, U+2039-203A, U+203C, U+2044, U+2047-2049, U+2190-2193, U+2196-2199, U+2202, U+2206, U+2212, U+2215, U+221A, U+221E, U+2248, U+2260, U+2264-2265, U+25A0, U+25B2-25B3, U+25B6-25B7, U+25BC-25BD, U+25C0-25C1, U+25C6, U+25C9, U+2752, U+2E3A-2E3B
`200	ExtraLight
`300	Light
`400	Regular
`600	Semibold
`700	Bold
`900	Black

Source Sans 3
/static/font/ss3/SourceSans3-BASIC-
otf
font-display	swap
italic			/$/		Italic
unicode-range	U+0020-00BF, U+00C6, U+00D7, U+00D8, U+00DE, U+00DF, U+00E6, U+00F0, U+00F7, U+00F8, U+00FE, U+0152, U+0153, U+017F, U+018F, U+0192, U+0259, U+02C6-02CC, U+02D8-02DD, U+0300-030C, U+2002-2049, U+2190-2199, U+2212, U+2215, U+221E, U+2248, U+2260, U+2264-2265, U+2318, U+2325, U+2326, U+232B, U+25A0-25E6, U+2610, U+263C, U+263F-2642, U+2660, U+2663, U+2665, U+2666, U+2752, U+275B-2760, U+2E3A, U+2E3B
`200	ExtraLight
`300	Light
`400	Regular
`500	Medium
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

Great Primer Uncials
/static/font/blackletter/GreatPrimerUncials
otf
font-display	swap
`normal			`
