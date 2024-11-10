/*********/
/* COLOR */
/*********/

Color = {
	ColorSpace: {
		RGB: "RGB",
		HSV: "HSV",
		XYZ: "XYZ",
		Lab: "Lab",
		YCC: "YCC"
	},

	ColorTransform: {
		COLORIZE: "colorize"
	},

	ColorTransformSettings: {
		"colorize": {
			defaultColorSpace: "Lab",
			"Lab": {
				//	L (lightness)
				maxBaseValue: 70
			},
			"YCC": {
				//	Y (luma)
				maxBaseValue: 0.7
			}
		}
	},

	processColorValue: (colorString, transforms, options) => {
		options = Object.assign({
			//	By default, we output in the same format as the input.
			output: colorString.startsWith("#") ? "hex" : "rgba"
		}, options);

		//	Save original value (for alpha restoration later).
		let originalColorRGBA = Color.rgbaFromString(colorString);
		let transformedValueRGBA = originalColorRGBA;

		//	Apply transforms.
		transforms.forEach(transform => {
			let transformType = transform[0];
			if (transformType == Color.ColorTransform.COLORIZE) {
				let colorSpace = transform[2] ?? Color.ColorTransformSettings[transformType].defaultColorSpace;
				let referenceColorRGBA = Color.rgbaFromString(transform[1]);

				if (colorSpace == Color.ColorSpace.Lab) {
					let referenceColorLab = Color.labFromXYZ(Color.xyzFromRGB(referenceColorRGBA));
					let subjectColorLab = Color.labFromXYZ(Color.xyzFromRGB(transformedValueRGBA));
					let transformedSubjectColorLab = Color.colorValueTransform_colorize(subjectColorLab, referenceColorLab, Color.ColorSpace.Lab);

					transformedValueRGBA = Color.rgbFromXYZ(Color.xyzFromLab(transformedSubjectColorLab));
				} else if (colorSpace == Color.ColorSpace.YCC) {
					let referenceColorYCC = Color.yccFromRGB(referenceColorRGBA);
					let subjectColorYCC = Color.yccFromRGB(transformedValueRGBA);
					let transformedSubjectColorYCC = Color.colorValueTransform_colorize(subjectColorYCC, referenceColorYCC, Color.ColorSpace.YCC);

					transformedValueRGBA = Color.rgbFromYCC(transformedSubjectColorYCC);
				}
			}
		});

		//	Restore alpha value (discarded during processing).
		transformedValueRGBA.alpha = originalColorRGBA.alpha;

		return (options.output == "hex"
				? Color.hexStringFromRGB(transformedValueRGBA)
				: Color.rgbaStringFromRGBA(transformedValueRGBA));
	},

	colorizeTransformLabMaxBaseLightness: 70,

	colorizeTransformYCCMaxBaseLuma: 0.5,

	/*	In L*a*b*, retain lightness (L*) but set color (a* and b*) from the
		specified reference color.

		In YCoCg, retain luma (Y) but set chroma (Co and Cg) from the specified
		reference color.

		In any other color space, has no effect.
	 */
	colorValueTransform_colorize: (color, referenceColor, colorSpace) => {
		if ([ Color.ColorSpace.Lab, Color.ColorSpace.YCC ].includes(colorSpace) == false)
			return color;

		let maxBaseValue = Color.ColorTransformSettings[Color.ColorTransform.COLORIZE][colorSpace].maxBaseValue;

		if (colorSpace == Color.ColorSpace.Lab) {
			let baseLightness = Math.min(referenceColor.lightness, maxBaseValue);

			color.lightness = baseLightness + (100 - baseLightness) * (color.lightness / 100);
			color.a = referenceColor.a;
			color.b = referenceColor.b;
		} else if (colorSpace == Color.ColorSpace.YCC) {
			let baseLuma = Math.min(referenceColor.Y, maxBaseValue);

			color.Y  = baseLuma + (1 - baseLuma) * color.Y;
			color.Co = referenceColor.Co;
			color.Cg = referenceColor.Cg;
		}

		return color;
	},

	rgbaFromString: (colorString) => {
		let rgba = { };
		if (colorString.startsWith("#")) {
			let bareHexValues = colorString.slice(1);
			if (bareHexValues.length == "3")
				bareHexValues = bareHexValues.replace(/./g, "$&$&");
			let values = bareHexValues.match(/../g).map(hexString => parseInt(hexString, 16));
			rgba = Object.fromEntries([
				[ "red",   values[0] ],
				[ "green", values[1] ],
				[ "blue",  values[2] ],
				[ "alpha", 1.0       ]
			]);
		} else if (colorString.toLowerCase().startsWith("rgb")) {
			let values = colorString.match(/rgba?\((.+?)\)/)[1].replace(/\s/, "").split(",").map(decString => parseInt(decString));
			rgba = Object.fromEntries([
				[ "red",   values[0] ],
				[ "green", values[1] ],
				[ "blue",  values[2] ],
				[ "alpha", values[3] ?? 1.0 ]
			]);
		}
		return rgba;
	},

	hexStringFromRGB: (rgb) => {
		return ("#" + [ rgb.red, rgb.green, rgb.blue ].map(hexValue => Math.round(hexValue).toString(16).padStart(2, "0")).join(""));
	},

	rgbaStringFromRGBA: (rgba) => {
		return (  "rgba(" 
				+ [ rgba.red, rgba.green, rgba.blue ].map(value => Math.round(value).toString().padStart(3, " ")).join(", ") 
				+ ", " + Math.round(rgba.alpha).toString()
				+ ")");
	},

	//	https://en.wikipedia.org/wiki/YCoCg
	yccFromRGB: (rgb) => {
		let red   = rgb.red   / 255.0;
		let green = rgb.green / 255.0;
		let blue  = rgb.blue  / 255.0;

		return {
			Y:  red *  0.25 + green * 0.5 + blue *  0.25,
			Co: red *  0.5                + blue * -0.5,
			Cg: red * -0.25 + green * 0.5 + blue * -0.25
		}
	},

	rgbFromYCC: (ycc) => {
		return {
			red:   Math.max(0, Math.min(1, ycc.Y + ycc.Co - ycc.Cg)) * 255.0,
			green: Math.max(0, Math.min(1, ycc.Y          + ycc.Cg)) * 255.0,
			blue:  Math.max(0, Math.min(1, ycc.Y - ycc.Co - ycc.Cg)) * 255.0
		}
	},

	rgbFromXYZ: (xyz) => {
		let x = xyz.x / 100.0;
		let y = xyz.y / 100.0;
		let z = xyz.z / 100.0;

		let r = x *  3.2406 + y * -1.5372 + z * -0.4986;
		let g = x * -0.9689 + y *  1.8758 + z *  0.0415;
		let b = x *  0.0557 + y * -0.2040 + z *  1.0570;

		let rgbValues = [ r, g, b ];

		for (let [ i, value ] of Object.entries(rgbValues)) {
			value = value > 0.0031308
					? (1.055 * Math.pow(value, (1.0/2.4)) - 0.055) 
					: (12.92 * value);
			rgbValues[i] = Math.min(Math.max(value, 0.0), 1.0) * 255.0;
		}

		return {
			red:   rgbValues[0],
			green: rgbValues[1],
			blue:  rgbValues[2],
		};
	},

	xyzFromLab: (lab) => {
		let y = (lab.lightness + 16.0) / 116.0;
		let x = lab.a / 500.0 + y;
		let z = y - lab.b / 200.0;

		let xyzValues = [ x, y, z ];

		for (let [ i, value ] of Object.entries(xyzValues)) {
			xyzValues[i] = Math.pow(value, 3) > 0.008856 
						   ? (Math.pow(value, 3)) 
						   : ((value - 16.0/116.0) / 7.787);
		}

		return {
			x: xyzValues[0] *  95.047,
			y: xyzValues[1] * 100.000,
			z: xyzValues[2] * 108.883,
		};
	},

	labFromXYZ: (xyz) => {
		let xyzValues = [ xyz.x, xyz.y, xyz.z ];

		xyzValues[0] /= 95.047;
		xyzValues[1] /= 100.000;
		xyzValues[2] /= 108.883;

		for (let [ i, value ] of Object.entries(xyzValues)) {
			xyzValues[i] = value > 0.008856
						   ? (Math.pow(value, (1.0/3.0))) 
						   : ((7.787 * value) + (16.0/116.0));
		}

		let [ x, y, z ] = xyzValues;

		return {
			lightness: (116.0 * y) - 16.0,
			a: 500.0 * (x - y),
			b: 200.0 * (y - z)
		};
	},

	xyzFromRGB: (rgb) => {
		let rgbValues = [ rgb.red, rgb.green, rgb.blue ];

		for (let [ i, value ] of Object.entries(rgbValues)) {
			value /= 255.0;
			rgbValues[i] = value > 0.04045 
						   ? (Math.pow(((value + 0.055) / 1.055), 2.4)) 
						   : (value / 12.92);
		}

		let [ red, green, blue ] = rgbValues;

		return {
			x: (red * 0.4124 + green * 0.3576 + blue * 0.1805) * 100.0,
			y: (red * 0.2126 + green * 0.7152 + blue * 0.0722) * 100.0,
			z: (red * 0.0193 + green * 0.1192 + blue * 0.9505) * 100.0
		}
	},

	rgbFromHSV: (hsv) => {
		let red, greed, blue;
		if (hsv.saturation != 0) {
			let h = hsv.hue * 6.0;
			if (h == 6.0)
				h = 0;
			let value1 = hsv.value * (1 - hsv.saturation);
			let value2 = hsv.value * (1 - hsv.saturation * (h - floor(h)));
			let value3 = hsv.value * (1 - hsv.saturation * (1 - (h - floor(h))));
		
			red = green = blue = 0.0;

			     if (i == 0) { red = hsv.value; green = value3;    blue = value1;    }
			else if (i == 1) { red = value2;    green = hsv.value; blue = value1;    }
			else if (i == 2) { red = value1;    green = hsv.value; blue = value3;    }
			else if (i == 3) { red = value1;    green = value2;    blue = hsv.value; }
			else if (i == 4) { red = value3;    green = value1;    blue = hsv.value; }
			else             { red = hsv.value; green = value1;    blue = value2;    }
		} else {
			red = green = blue = hsv.value;
		}
	
		return {
			red:   red   * 255.0,
			green: green * 255.0,
			blue:  blue  * 255.0
		};
	},

	hsvFromRGB: (rgb) => {
		let red   = rgb.red   / 255.0;
		let green = rgb.green / 255.0;
		let blue  = rgb.blue  / 255.0;

		let minValue = Math.min(red, green, blue);
		let maxValue = Math.max(red, green, blue);
		let valueDelta = maxValue - minValue;

		let value = maxValue;
		let hue = 0;
		let saturation = 0;

		if (valueDelta != 0) {
			saturation = valueDelta / maxValue;

			let deltaRed   = (((maxValue - red)   / 6) + (valueDelta / 2)) / valueDelta;
			let deltaGreen = (((maxValue - green) / 6) + (valueDelta / 2)) / valueDelta;
			let deltaBlue  = (((maxValue - blue)  / 6) + (valueDelta / 2)) / valueDelta;

			     if (red   == maxValue) hue =             deltaBlue  - deltaGreen;
			else if (green == maxValue) hue = (1.0/3.0) + deltaRed   - deltaBlue;
			else if (blue  == maxValue) hue = (2.0/3.0) + deltaGreen - deltaRed;

			     if (hue < 0) hue += 1;
			else if (hue > 1) hue -= 1;
		}
	
		return {
			hue:        hue,
			saturation: saturation,
			lightness:  lightness
		};
	}
};
