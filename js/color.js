/*********/
/* COLOR */
/*********/

Color = {
	ColorSpace: {
		RGB:   "RGB",
		HSV:   "HSV",
		HSL:   "HSL",
		XYZ:   "XYZ",
		Lab:   "Lab",
		YCC:   "YCC",
		Oklab: "Oklab",
		Oklch: "Oklch"
	},

	ColorTransform: {
		COLORIZE: "colorize"
	},

	ColorTransformSettings: {
		"colorize": {
			defaultColorSpace: "Oklch",
			"Lab": {
				//	L (lightness)
				minBaseValue: 0.00,
				maxBaseValue: 0.70
			},
			"YCC": {
				//	Y (luma)
				minBaseValue: 0.48,
				maxBaseValue: 0.50
			},
			"Oklab": {
				//	L (lightness)
				minBaseValue: 0.62,
				maxBaseValue: 0.77
			},
			"Oklch": {
				//	L (lightness)
				minBaseValue: 0.62,
				maxBaseValue: 0.77,
				chromaBoostFactor: 0.75, // values above 1.0 possible... but not recommended
				antiClusteringFactor: 0.75 // [ 0.0, 1.0 ]
			},
			"HSL": {
				//	L (lightness)
				minBaseValue: 0.65,
				maxBaseValue: 0.77,
				saturationBoostFactor: 0.50, // [ 0.0, 1.0 ]
				antiClusteringFactor: 0.25 // [ 0.0, 1.0 ]
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
			if (transform.type == Color.ColorTransform.COLORIZE) {
				let workingColorSpace = (   transform.colorSpace 
										 ?? Color.ColorTransformSettings[transform.type].defaultColorSpace);
				let referenceColorRGBA = Color.rgbaFromString(transform.referenceColor);
				let subjectColorInWorkingColorSpace = Color.fromRGB(transformedValueRGBA, workingColorSpace);
				let referenceColorInWorkingColorSpace = Color.fromRGB(referenceColorRGBA, workingColorSpace);
				let transformedSubjectColorInWorkingColorSpace = Color.colorValueTransform_colorize(subjectColorInWorkingColorSpace,
																									referenceColorInWorkingColorSpace,
																									workingColorSpace);
				transformedValueRGBA = Color.rgbFrom(transformedSubjectColorInWorkingColorSpace, workingColorSpace);
			}
		});

		//	Restore alpha value (discarded during processing).
		transformedValueRGBA.alpha = originalColorRGBA.alpha;

		return (options.output == "hex"
				? Color.hexStringFromRGB(transformedValueRGBA)
				: Color.rgbaStringFromRGBA(transformedValueRGBA));
	},

	/*	In L*a*b* or Oklab, retain lightness (L*) but set color (a* and b*) 
		from the specified reference color.

		In Oklch, retain lightness (L) but set chroma (C) and hue (h°) from the
		specified reference color.

		In YCoCg, retain luma (Y) but set chroma (Co and Cg) from the specified
		reference color.

		In HSL, retain lightness (L) but set saturation (S) and hue (H) from
		the specified reference color.

		In any other color space, has no effect.
	 */
	colorValueTransform_colorize: (color, referenceColor, colorSpace) => {
		if ([ Color.ColorSpace.Lab,
			  Color.ColorSpace.YCC,
			  Color.ColorSpace.Oklab,
			  Color.ColorSpace.Oklch,
			  Color.ColorSpace.HSL
			  ].includes(colorSpace) == false)
			return color;

		let minBaseValue = Color.ColorTransformSettings[Color.ColorTransform.COLORIZE][colorSpace].minBaseValue;
		let maxBaseValue = Color.ColorTransformSettings[Color.ColorTransform.COLORIZE][colorSpace].maxBaseValue;

		if (colorSpace == Color.ColorSpace.Lab) {
			color.a = referenceColor.a;
			color.b = referenceColor.b;

			let baseLightness = Math.max(Math.min(referenceColor.L, maxBaseValue), minBaseValue);
			color.L = baseLightness + (1.0 - baseLightness) * color.L;
		} else if (colorSpace == Color.ColorSpace.YCC) {
			color.Co = referenceColor.Co;
			color.Cg = referenceColor.Cg;

			let baseLuma = Math.max(Math.min(referenceColor.Y, maxBaseValue), minBaseValue);
			color.Y  = baseLuma + (1.0 - baseLuma) * color.Y;
		} else if (colorSpace == Color.ColorSpace.Oklab) {
			color.a = referenceColor.a;
			color.b = referenceColor.b;

			let baseLightness = Math.max(Math.min(referenceColor.L, maxBaseValue), minBaseValue);
			color.L = baseLightness + (1.0 - baseLightness) * color.L;
		} else if (colorSpace == Color.ColorSpace.Oklch) {
			color.C = referenceColor.C;
			color.h = referenceColor.h;

			let baseLightness = Math.max(Math.min(referenceColor.L, maxBaseValue), minBaseValue);
			color.L = baseLightness + (1.0 - baseLightness) * color.L;

			//	Saturate.
			let maxChroma = Color.oklchFromRGB(Color.rgbFromOklch({ L: color.L, C: 1.0, h: color.h })).C;
			let chromaBoostFactor = Color.ColorTransformSettings[Color.ColorTransform.COLORIZE]["Oklch"].chromaBoostFactor;
			let antiClusteringFactor = Color.ColorTransformSettings[Color.ColorTransform.COLORIZE]["Oklch"].antiClusteringFactor;
			color.C += chromaBoostFactor * (maxChroma - color.C) * (1.0 - color.L * antiClusteringFactor);

			//	Gamut correction.
			let maxLightness = Color.oklchFromRGB(Color.rgbFromOklch({ L: 1.0, C: color.C, h: color.h })).L;
			if (color.L > maxLightness)
				color.C *= (1.0 - color.L) / (1.0 - maxLightness);
		} else if (colorSpace == Color.ColorSpace.HSL) {
			color.hue = referenceColor.hue;
			color.saturation = referenceColor.saturation;

			//	Gamma correction.
// 			color.lightness = Math.pow(color.lightness, 0.5);

			let baseLightness = Math.max(Math.min(referenceColor.lightness, maxBaseValue), minBaseValue);
			color.lightness = baseLightness + (1.0 - baseLightness) * color.lightness;

			//	Saturate.
			let saturationBoostFactor = Color.ColorTransformSettings[Color.ColorTransform.COLORIZE]["HSL"].saturationBoostFactor;
			let antiClusteringFactor = Color.ColorTransformSettings[Color.ColorTransform.COLORIZE]["HSL"].antiClusteringFactor;
			color.saturation += saturationBoostFactor * (1.0 - color.saturation) * (1.0 - color.lightness * antiClusteringFactor);
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
				+ ", " + Math.round(rgba.alpha ?? 1.0).toString()
				+ ")");
	},

	//	Main convenience method (from RGB).
	fromRGB: (rgb, targetColorSpace) => {
		switch (targetColorSpace) {
		case Color.ColorSpace.HSV:
			return Color.hsvFromRGB(rgb);
		case Color.ColorSpace.HSL:
			return Color.hslFromRGB(rgb);
		case Color.ColorSpace.Lab:
			return Color.labFromRGB(rgb);
		case Color.ColorSpace.YCC:
			return Color.yccFromRGB(rgb);
		case Color.ColorSpace.Oklab:
			return Color.oklabFromRGB(rgb);
		case Color.ColorSpace.Oklch:
			return Color.oklchFromRGB(rgb);
		case Color.ColorSpace.RGB:
			return rgb;
		default:
			return null;
		}
	},

	//	Main convenience method (to RGB).
	rgbFrom: (color, sourceColorSpace) => {
		switch (sourceColorSpace) {
		case Color.ColorSpace.HSV:
			return Color.rgbFromHSV(color);
		case Color.ColorSpace.HSL:
			return Color.rgbFromHSL(color);
		case Color.ColorSpace.Lab:
			return Color.rgbFromLab(color);
		case Color.ColorSpace.YCC:
			return Color.rgbFromYCC(color);
		case Color.ColorSpace.Oklab:
			return Color.rgbFromOklab(color);
		case Color.ColorSpace.Oklch:
			return Color.rgbFromOklch(color);
		case Color.ColorSpace.RGB:
			return color;
		default:
			return null;
		}
	},

	//	Convenience method.
	labFromRGB: (rgb) => {
		return Color.labFromXYZ(Color.xyzFromRGB(rgb));
	},

	//	Convenience method.
	rgbFromLab: (lab) => {
		return Color.rgbFromXYZ(Color.xyzFromLab(lab));
	},

	//	Convenience method.
	oklabFromRGB: (rgb) => {
		return Color.oklabFromXYZ(Color.xyzFromRGB(rgb));
	},

	//	Convenience method.
	rgbFromOklab: (oklab) => {
		return Color.rgbFromXYZ(Color.xyzFromOklab(oklab));
	},

	//	Convenience method.
	oklchFromRGB: (rgb) => {
		return Color.oklchFromOklab(Color.oklabFromXYZ(Color.xyzFromRGB(rgb)));
	},

	//	Convenience method.
	rgbFromOklch: (oklch) => {
		return Color.rgbFromXYZ(Color.xyzFromOklab(Color.oklabFromOklch(oklch)));
	},

	oklchFromOklab: (oklab) => {
		return {
			L: oklab.L,
			C: Math.sqrt(Math.pow(oklab.a, 2) + Math.pow(oklab.b, 2)),
			h: Math.atan2(oklab.b, oklab.a)
		};
	},

	oklabFromOklch: (oklch) => {
		return {
			L: oklch.L,
			a: oklch.C * Math.cos(oklch.h),
			b: oklch.C * Math.sin(oklch.h)
		};
	},

	oklabFromXYZ: (xyz) => {
		let l = Math.pow(xyz.x *  0.8189330101 + xyz.y *  0.3618667424 + xyz.z * -0.1288597137, 1.0/3.0);
		let m = Math.pow(xyz.x *  0.0329845436 + xyz.y *  0.9293118715 + xyz.z *  0.0361456387, 1.0/3.0);
		let s = Math.pow(xyz.x *  0.0482003018 + xyz.y *  0.2643662691 + xyz.z *  0.6338517070, 1.0/3.0);

		return {
			L: l *  0.2104542553 + m *  0.7936177850 + s * -0.0040720468,
			a: l *  1.9779984951 + m * -2.4285922050 + s *  0.4505937099,
			b: l *  0.0259040371 + m *  0.7827717662 + s * -0.8086757660
		};
	},

	xyzFromOklab: (oklab) => {
		let l = Math.pow(oklab.L *  0.9999999985 + oklab.a *  0.3963377922 + oklab.b *  0.2158037581, 3);
		let m = Math.pow(oklab.L *  1.0000000089 + oklab.a * -0.1055613423 + oklab.b * -0.0638541748, 3);
		let s = Math.pow(oklab.L *  1.0000000547 + oklab.a * -0.0894841821 + oklab.b * -1.2914855379, 3);

		return {
			x: l *  1.2270138511 + m * -0.5577999807 + s *  0.2812561490,
			y: l * -0.0405801784 + m *  1.1122568696 + s * -0.0716766787,
			z: l * -0.0763812845 + m * -0.4214819784 + s *  1.5861632204
		};
	},

	//	https://en.wikipedia.org/wiki/YCoCg
	yccFromRGB: (rgb) => {
		let red   = rgb.red   / 255.0;
		let green = rgb.green / 255.0;
		let blue  = rgb.blue  / 255.0;

		return {
			Y:  red *  0.25 + green * 0.50 + blue *  0.25,
			Co: red *  0.50                + blue * -0.50,
			Cg: red * -0.25 + green * 0.50 + blue * -0.25
		}
	},

	rgbFromYCC: (ycc) => {
		return {
			red:   Math.max(0.0, Math.min(1.0, ycc.Y + ycc.Co - ycc.Cg)) * 255.0,
			green: Math.max(0.0, Math.min(1.0, ycc.Y          + ycc.Cg)) * 255.0,
			blue:  Math.max(0.0, Math.min(1.0, ycc.Y - ycc.Co - ycc.Cg)) * 255.0
		}
	},

	hslFromRGB: (rgb) => {
		let red   = rgb.red   / 255.0;
		let green = rgb.green / 255.0;
		let blue  = rgb.blue  / 255.0;

		let minValue = Math.min(red, green, blue);
		let maxValue = Math.max(red, green, blue);
		let maxDelta = maxValue - minValue;

		let hue = 0.0;
		let saturation = 0.0;
		let lightness = (maxValue + minValue) / 2.0;

		if (maxDelta != 0.0) {
			saturation = lightness > 0.5
						 ? maxDelta / (2.0 - (maxValue + minValue))
						 : maxDelta / (maxValue + minValue);

			     if (red   == maxValue) hue = (green - blue)  / maxDelta + (green < blue ? 6.0 : 0.0);
			else if (green == maxValue) hue = (blue  - red)   / maxDelta + 2.0;
			else if (blue  == maxValue) hue = (red   - green) / maxDelta + 4.0;

			hue /= 6.0;
		}
	
		return {
			hue:        hue,
			saturation: saturation,
			lightness:  lightness
		};
	},

	rgbFromHSL: (hsl) => {
		let red, green, blue;

		if (hsl.saturation != 0.0) {
			function colorChannelFromHue(p, q, t) {
				if (t < 0.0) t += 1.0;
				if (t > 1.0) t -= 1.0;

				if (t < 1.0/6.0) return p + (q - p) * 6.0 * t;
				if (t < 1.0/2.0) return q;
				if (t < 2.0/3.0) return p + (q - p) * 6.0 * (2.0/3.0 - t);

				return p;
			}

			let q = hsl.lightness < 0.5
					? hsl.lightness * (1.0 + hsl.saturation)
					: hsl.lightness + hsl.saturation - (hsl.lightness * hsl.saturation);
			let p = 2.0 * hsl.lightness - q;

			red   = colorChannelFromHue(p, q, hsl.hue + 1.0/3.0);
			green = colorChannelFromHue(p, q, hsl.hue);
			blue  = colorChannelFromHue(p, q, hsl.hue - 1.0/3.0);
		} else {
			red = green = blue = hsl.lightness;
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
		let maxDelta = maxValue - minValue;

		let hue = 0.0;
		let saturation = 0.0;
		let value = maxValue;

		if (maxDelta != 0.0) {
			saturation = maxDelta / maxValue;

			let deltaRed   = (((maxValue - red)   / 6.0) + (maxDelta / 2.0)) / maxDelta;
			let deltaGreen = (((maxValue - green) / 6.0) + (maxDelta / 2.0)) / maxDelta;
			let deltaBlue  = (((maxValue - blue)  / 6.0) + (maxDelta / 2.0)) / maxDelta;

			     if (red   == maxValue) hue =             deltaBlue  - deltaGreen;
			else if (green == maxValue) hue = (1.0/3.0) + deltaRed   - deltaBlue;
			else if (blue  == maxValue) hue = (2.0/3.0) + deltaGreen - deltaRed;

			     if (hue < 0.0) hue += 1.0;
			else if (hue > 1.0) hue -= 1.0;
		}
	
		return {
			hue:        hue,
			saturation: saturation,
			value:      value
		};
	},

	rgbFromHSV: (hsv) => {
		let red, greed, blue;

		if (hsv.saturation != 0.0) {
			let h = hsv.hue * 6.0;
			if (h == 6.0)
				h = 0.0;
			let i = Math.floor(h);
			let value1 = hsv.value * (1.0 - hsv.saturation);
			let value2 = hsv.value * (1.0 - hsv.saturation * (h - i));
			let value3 = hsv.value * (1.0 - hsv.saturation * (1.0 - (h - i)));
		
			red = green = blue = 0.0;

			     if (i == 0.0) { red = hsv.value; green = value3;    blue = value1;    }
			else if (i == 1.0) { red = value2;    green = hsv.value; blue = value1;    }
			else if (i == 2.0) { red = value1;    green = hsv.value; blue = value3;    }
			else if (i == 3.0) { red = value1;    green = value2;    blue = hsv.value; }
			else if (i == 4.0) { red = value3;    green = value1;    blue = hsv.value; }
			else               { red = hsv.value; green = value1;    blue = value2;    }
		} else {
			red = green = blue = hsv.value;
		}
	
		return {
			red:   red   * 255.0,
			green: green * 255.0,
			blue:  blue  * 255.0
		};
	},

	xyzFromRGB: (rgb) => {
		let rgbValues = [ rgb.red, rgb.green, rgb.blue ];

		for (let [ i, value ] of Object.entries(rgbValues)) {
			value /= 255.0;
			rgbValues[i] = value > 0.04045 
						   ? Math.pow(((value + 0.055) / 1.055), 2.4)
						   : value / 12.92;
		}

		let [ red, green, blue ] = rgbValues;

		return {
			x: red * 0.4124 + green * 0.3576 + blue * 0.1805,
			y: red * 0.2126 + green * 0.7152 + blue * 0.0722,
			z: red * 0.0193 + green * 0.1192 + blue * 0.9505
		}
	},

	rgbFromXYZ: (xyz) => {
		let x = xyz.x;
		let y = xyz.y;
		let z = xyz.z;

		let r = x *  3.2406 + y * -1.5372 + z * -0.4986;
		let g = x * -0.9689 + y *  1.8758 + z *  0.0415;
		let b = x *  0.0557 + y * -0.2040 + z *  1.0570;

		let rgbValues = [ r, g, b ];

		for (let [ i, value ] of Object.entries(rgbValues)) {
			value = value > 0.0031308
					? 1.055 * Math.pow(value, (1.0/2.4)) - 0.055 
					: 12.92 * value;
			rgbValues[i] = Math.min(Math.max(value, 0.0), 1.0) * 255.0;
		}

		return {
			red:   rgbValues[0],
			green: rgbValues[1],
			blue:  rgbValues[2],
		};
	},

	labFromXYZ: (xyz) => {
		let xyzValues = [ xyz.x, xyz.y, xyz.z ];

		xyzValues[0] /= 0.95047;
		xyzValues[1] /= 1.00000;
		xyzValues[2] /= 1.08883;

		for (let [ i, value ] of Object.entries(xyzValues)) {
			xyzValues[i] = value > 0.008856
						   ? Math.pow(value, (1.0/3.0))
						   : (7.787 * value) + (0.16/1.16);
		}

		let [ x, y, z ] = xyzValues;

		return {
			L: (1.16 * y) - 0.16,
			a: 5.0 * (x - y),
			b: 2.0 * (y - z)
		};
	},

	xyzFromLab: (lab) => {
		let y = (lab.L + 0.16) / 1.16;
		let x = lab.a / 5.0 + y;
		let z = y - lab.b / 2.0;

		let xyzValues = [ x, y, z ];

		for (let [ i, value ] of Object.entries(xyzValues)) {
			xyzValues[i] = Math.pow(value, 3) > 0.008856 
						   ? Math.pow(value, 3) 
						   : (value - 0.16/1.16) / 7.787;
		}

		return {
			x: xyzValues[0] * 0.95047,
			y: xyzValues[1] * 1.00000,
			z: xyzValues[2] * 1.08883,
		};
	}
};
