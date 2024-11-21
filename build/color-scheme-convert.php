<?php

/*
color-scheme-convert.php

Copyright (c) 2018 Said Achmiz

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
 */

if (!isset($argv[1]))
	die;
	
$debug_enabled = false;

## Get command line arguments.
$stylesheet = file_get_contents($argv[1]);
$mode = @$argv[2] ?: 1;
$working_color_space = @$argv[3] ?: "Lab";
$gamma = @$argv[4] ?: 0.55;

## Process and print.
$stylesheet = preg_replace_callback('/(#[0-9abcdef]+)([,; ])/i', 'ProcessColorValue', $stylesheet);
$stylesheet = preg_replace_callback('/rgba\(\s*([0-9]+),\s*([0-9]+),\s*([0-9]+),\s*(.+)\s*\)/i', 'ProcessColorValue_RGBA', $stylesheet);
echo $stylesheet;

/******************/
/* CSS PROCESSING */
/******************/

function ProcessColorValue($m) {
	global $working_color_space;

	debug_log($m[1]);
	$m[1] = HexFromRGB(RGBFrom(CVT(fromRGB(RGBFromHex($m[1]), $working_color_space), $working_color_space), $working_color_space));

	return implode(array_slice($m,1));
}
function ProcessColorValue_RGBA($m) {
	global $working_color_space;

	$rgb = array_slice($m, 1, 3);
	debug_log(PCC($rgb));
	$rgb = RGBFrom(CVT(fromRGB($rgb, $working_color_space), $working_color_space), $working_color_space);

	//	Round.
	foreach ($rgb as $k => $v)
		$rgb[$k] = round($v);

	//	Reattach alpha.
	$rgb[] = $m[4];
	
	return "rgba(" . implode(", ", $rgb) . ")";
}

/***********/
/* HELPERS */
/***********/

function debug_log($string) {
	global $debug_enabled;
	if ($debug_enabled)
		error_log($string);
}

/******************/
/* TRANSFORMATION */
/******************/

## CVT = "Color Value Transform"
function CVT($value, $color_space) {
	global $mode, $gamma;
	## The mode is a bit field; set binary flags indicate specific transformations.
	## Flags are checked, and applied, in order from lowest bit position to highest.
	##
	## 0x0001: lightness inversion (in Lab, YCC, Oklab, or Oklch).
	## 0x0002: hue inversion (in Lab or YCC).
	##
	## The following six flags are mutually exclusive:
	##
	## 0x0004: maps whites to reds (in HSV; keeps V constant, sets H to 0°, S to maximum)
	## 0x0008: maps whites to yellows (in HSV; keeps V constant, sets H to 60°, S to maximum)
	## 0x0010: maps whites to greens (in HSV; keeps V constant, sets H to 120°, S to maximum)
	## 0x0020: maps whites to teal/turquoise (in HSV; keeps V constant, sets H to 180°, S to maximum)
	## 0x0040: maps whites to blue (in HSV; keeps V constant, sets H to 240°, S to maximum)
	## 0x0080: maps whites to magenta (in HSV; keeps V constant, sets H to 300°, S to maximum)

	// Lightness inversion.
	if ($mode & 0x0001) {
		if (in_array($color_space, [ "Lab", "YCC" ])) {
			$value[0] = 1.0 - $value[0];
			$value[0] = pow(max(0.0, $value[0]), $gamma);

			debug_log("  →  {$color_space} ".PCC($value));
		} else if (in_array($color_space, [ "Oklab", "Oklch" ])) {
			$temp_value = $value;
			if ($color_space == "Oklch")
				$temp_value = OklabFromOklch($temp_value);
			$temp_value = LabFromXYZ(XYZFromOklab($temp_value));

			// Invert in Lab.
			$temp_value[0] = 1.0 - $temp_value[0];
			$temp_value[0] = pow(max(0.0, $temp_value[0]), $gamma);

			$temp_value = OklabFromXYZ(XYZFromLab($temp_value));
			if ($color_space == "Oklch")
				$temp_value = OklchFromOklab($temp_value);

			// Use Ok-space color values, to prevent hue distortion.
			$value[0] = $temp_value[0];

			debug_log("  →  {$color_space} ".PCC($value));
		}
	}

	// Hue inversion.
	if ($mode & 0x0002) {
		switch ($color_space) {
			case "Lab":
				$value[1] *= -1;
				$value[2] *= -1;
				break;
			case "YCC":
				break;
			default:
				break;
		}
	}

	if ($mode & 0x00FC) {
		$hsv_value = [ ];
		switch ($color_space) {
			case "Lab":
				$hsv_value = HSVFromRGB(RGBFromLab($value));
				break;
			case "YCC":
				$hsv_value = HSVFromRGB(RGBFromYCC($value));
				break;
			default:
				break;
		}
		if ($mode & 0x0004) {
			$hsv_value[0] = 0.0 / 360.0;
			$hsv_value[1] = 1.0;
		} else if ($mode & 0x0008) {
			$hsv_value[0] = 60.0 / 360.0;
			$hsv_value[1] = 1.0;
		} else if ($mode & 0x0010) {
			$hsv_value[0] = 120.0 / 360.0;
			$hsv_value[1] = 1.0;
		} else if ($mode & 0x0020) {
			$hsv_value[0] = 180.0 / 360.0;
			$hsv_value[1] = 1.0;
		} else if ($mode & 0x0040) {
			$hsv_value[0] = 240.0 / 360.0;
			$hsv_value[1] = 1.0;
		} else if ($mode & 0x0080) {
			$hsv_value[0] = 300.0 / 360.0;
			$hsv_value[1] = 1.0;	
		}
		switch ($color_space) {
			case "Lab":
				$value = LabFromRGB(RGBFromHSV($hsv_value));
				break;
			case "YCC":
				$value = YCCFromRGB(RGBFromHSV($hsv_value));
				break;
			default:
				break;
		}
	}

	debug_log("  →  {$color_space} ".PCC($value));

	return $value;
}

/*********************/
/* FORMAT CONVERSION */
/*********************/

function RGBFromHex($hexColorString) {
	if ($hexColorString[0] == '#')
		$hexColorString = substr($hexColorString,1);
	if (strlen($hexColorString) == 3)
		$hexColorString = preg_replace("/./","$0$0",$hexColorString);
	$components = str_split($hexColorString, 2);
	foreach ($components as $i => $hexColor)
		$components[$i] = hexdec($hexColor);
	debug_log("  →  RGB ".PCC($components));
	return $components;
}

function HexFromRGB($rgb_components) {
	foreach ($rgb_components as $i => $component) {
		$hex_value = dechex(round($component));
		if (strlen($hex_value) == 1)
			$hex_value = "0".$hex_value;
		$rgb_components[$i] = $hex_value;
	}
	$hexColorString = "#" . implode($rgb_components);
	$hexColorString = preg_replace("/([0-9abcdef])\\1([0-9abcdef])\\2([0-9abcdef])\\3/", "$1$2$3", $hexColorString);
	debug_log("  →  ".$hexColorString);
	return $hexColorString;
}

## PCC = "Print Color Components"
function PCC($components) {
	$rounded_components = [ ];
	foreach ($components as $k => $v)
		$rounded_components[$k] = round($v, 4);
	return "( " . implode(", ", $rounded_components) . " )";
}

/**************************/
/* COLOR SPACE CONVERSION */
/**************************/

//	Main convenience method (from RGB).
function fromRGB($rgb_components, $target_color_space) {
	switch ($target_color_space) {
	case "HSV":
		return HSVFromRGB($rgb_components);
	case "HSL":
		return HSLFromRGB($rgb_components);
	case "Lab":
		return LabFromRGB($rgb_components);
	case "YCC":
		return YCCFromRGB($rgb_components);
	case "Oklab":
		return OklabFromRGB($rgb_components);
	case "Oklch":
		return OklchFromRGB($rgb_components);
	case "RGB":
		return $rgb_components;
	default:
		return null;
	}
}

//	Main convenience method (to RGB).
function RGBFrom($color_components, $source_color_space) {
	switch ($source_color_space) {
	case "HSV":
		return RGBFromHSV($color_components);
	case "HSL":
		return RGBFromHSL($color_components);
	case "Lab":
		return RGBFromLab($color_components);
	case "YCC":
		return RGBFromYCC($color_components);
	case "Oklab":
		return RGBFromOklab($color_components);
	case "Oklch":
		return RGBFromOklch($color_components);
	case "RGB":
		return $color_components;
	default:
		return null;
	}
}

//	Convenience function.
function LabFromRGB($rgb_components) {
	return LabFromXYZ(XYZFromRGB($rgb_components));
}

//	Convenience function.
function RGBFromLab($lab_components) {
	return RGBFromXYZ(XYZFromLab($lab_components));
}

//	Convenience function.
function OklabFromRGB($rgb_components) {
	return OklabFromXYZ(XYZFromRGB($rgb_components));
}

//	Convenience function.
function RGBFromOklab($oklab_components) {
	return RGBFromXYZ(XYZFromOklab($oklab_components));
}

//	Convenience function.
function OklchFromRGB($rgb_components) {
	return OklchFromOklab(OklabFromXYZ(XYZFromRGB($rgb_components)));
}

//	Convenience function.
function RGBFromOklch($oklch_components) {
	return RGBFromXYZ(XYZFromOklab(OklabFromOklch($oklch_components)));
}

function OklchFromOklab($oklab_components) {
	list($val_L, $val_a, $val_b) = $oklab_components;

	$var_L = $val_L;
	$var_C = sqrt(pow($val_a, 2) + pow($val_b, 2));
	$var_h = atan2($val_b, $val_a);

	debug_log("  →  Oklch ".PCC([ $var_L, $var_C, $var_h ]));
	return [ $var_L, $var_C, $var_h ];
}

function OklabFromOklch($oklch_components) {
	list($val_L, $val_C, $val_h) = $oklch_components;

	$var_L = $val_L;
	$var_a = $val_C * cos($val_h);
	$var_b = $val_C * sin($val_h);

	debug_log("  →  Oklab ".PCC([ $var_L, $var_a, $var_b ]));
	return [ $var_L, $var_a, $var_b ];
}

function OklabFromXYZ($xyz_components) {
	list($val_X, $val_Y, $val_Z) = $xyz_components;

	$var_l = pow($val_X *  0.8189330101 + $val_Y *  0.3618667424 + $val_Z * -0.1288597137, 1.0/3.0);
	$var_m = pow($val_X *  0.0329845436 + $val_Y *  0.9293118715 + $val_Z *  0.0361456387, 1.0/3.0);
	$var_s = pow($val_X *  0.0482003018 + $val_Y *  0.2643662691 + $val_Z *  0.6338517070, 1.0/3.0);

	$var_L = $var_l *  0.2104542553 + $var_m *  0.7936177850 + $var_s * -0.0040720468;
	$var_a = $var_l *  1.9779984951 + $var_m * -2.4285922050 + $var_s *  0.4505937099;
	$var_b = $var_l *  0.0259040371 + $var_m *  0.7827717662 + $var_s * -0.8086757660;

	debug_log("  →  Oklab ".PCC([ $var_L, $var_a, $var_b ]));
	return [ $var_L, $var_a, $var_b ];
}

function XYZFromOklab($oklab_components) {
	list($val_L, $val_a, $val_b) = $oklab_components;

	$var_l = pow($val_L *  0.9999999985 + $val_a *  0.3963377922 + $val_b *  0.2158037581, 3);
	$var_m = pow($val_L *  1.0000000089 + $val_a * -0.1055613423 + $val_b * -0.0638541748, 3);
	$var_s = pow($val_L *  1.0000000547 + $val_a * -0.0894841821 + $val_b * -1.2914855379, 3);

	$var_X = $var_l *  1.2270138511 + $var_m * -0.5577999807 + $var_s *  0.2812561490;
	$var_Y = $var_l * -0.0405801784 + $var_m *  1.1122568696 + $var_s * -0.0716766787;
	$var_Z = $var_l * -0.0763812845 + $var_m * -0.4214819784 + $var_s *  1.5861632204;

	debug_log("  →  XYZ ".PCC([ $var_X, $var_Y, $var_Z ]));
	return [ $var_X, $var_Y, $var_Z ];
}

## https://en.wikipedia.org/wiki/YCoCg
function YCCFromRGB($rgb_components) {
	$val_R = $rgb_components[0] / 255.0;
	$val_G = $rgb_components[1] / 255.0;
	$val_B = $rgb_components[2] / 255.0;

	$var_Y  = $val_R *  0.25 + $val_G * 0.50 + $val_B *  0.25;
	$var_Co = $val_R *  0.50                 + $val_B * -0.50;
	$var_Cg = $val_R * -0.25 + $val_G * 0.50 + $val_B * -0.25;

	debug_log("  →  YCC ".PCC([ $var_Y, $var_Co, $var_Cg ]));
	return [ $var_Y, $var_Co, $var_Cg ];
}

function RGBFromYCC($ycc_components) {
	list ($val_Y, $val_Co, $val_Cg) = $ycc_components;

	$var_R = max(0.0, min(1.0, $val_Y + $val_Co - $val_Cg)) * 255.0;
	$var_G = max(0.0, min(1.0, $val_Y           + $val_Cg)) * 255.0;
	$var_B = max(0.0, min(1.0, $val_Y - $val_Co - $val_Cg)) * 255.0;

	debug_log("  →  RGB ".PCC([ $var_R, $var_G, $var_B ]));
	return [ $var_R, $var_G, $var_B ];
}

function HSLFromRGB($rgb_components) {
	$val_R = $rgb_components[0] / 255.0;
	$val_G = $rgb_components[1] / 255.0;
	$val_B = $rgb_components[2] / 255.0;

	$val_Min = min($val_R, $val_G, $val_B);
	$val_Max = max($val_R, $val_G, $val_B);
	$del_Max = $val_Max - $val_Min;

	$var_H = 0.0;
	$var_S = 0.0;
	$var_L = ($val_Max + $val_Min) / 2.0;

	if ($del_Max != 0) {
		$var_S = $var_L > 0.5
				 ? $del_Max / (2.0 - ($val_Max + $val_Min))
				 : $del_Max / ($val_Max + $val_Min);

		     if ($val_R == $val_Max) $var_H = ($val_G - $val_B) / $del_Max + ($val_G < $val_B ? 6.0 : 0.0);
		else if ($val_G == $val_Max) $var_H = ($val_B - $val_R) / $del_Max + 2.0;
		else if ($val_B == $val_Max) $var_H = ($val_R - $val_G) / $del_Max + 4.0;

		$var_H /= 6.0;
	}

	debug_log("  →  HSL ".PCC([ $var_H, $var_S, $var_L ]));
	return [ $var_H, $var_S, $var_L ];
}

function RGBFromHSL($hsl_components) {
	$val_H = $hsv_components[0];
	$val_S = $hsv_components[1];
	$val_V = $hsv_components[2];

	$var_R = $var_G = $var_B = 0.0;

	if ($val_S != 0.0) {
		function color_channel_from_hue($p, $q, $t) {
			if ($t < 0.0) $t += 1.0;
			if ($t > 1.0) $t -= 1.0;

			if ($t < 1.0/6.0) return $p + ($q - $p) * 6.0 * $t;
			if ($t < 1.0/2.0) return $q;
			if ($t < 2.0/3.0) return $p + ($q - $p) * 6.0 * (2.0/3.0 - $t);

			return $p;
		}

		$var_q = $val_L < 0.5
				 ? $val_L * (1.0 * $val_S)
				 : $val_L + $val_S - ($val_L * $val_S);
		$var_p = 2.0 * $val_L - $var_q;

		$var_R = color_channel_from_hue($var_p, $var_q, $val_H + 1.0/3.0);
		$var_G = color_channel_from_hue($var_p, $var_q, $val_H);
		$var_B = color_channel_from_hue($var_p, $var_q, $val_H - 1.0/3.0);
	} else {
		$var_R = $var_G = $var_B = $val_L;
	}

	$var_R *= 255.0;
	$var_G *= 255.0;
	$var_B *= 255.0;
	
	debug_log("  →  RGB ".PCC([ $var_R, $var_G, $var_B ]));
	return [ $var_R, $var_G, $var_B ];
}

function HSVFromRGB($rgb_components) {
	$val_R = $rgb_components[0] / 255.0;
	$val_G = $rgb_components[1] / 255.0;
	$val_B = $rgb_components[2] / 255.0;

	$val_Min = min($val_R, $val_G, $val_B);
	$val_Max = max($val_R, $val_G, $val_B);
	$del_Max = $val_Max - $val_Min;

	$var_H = 0.0;
	$var_S = 0.0;
	$var_V = $val_Max;

	if ($del_Max != 0.0) {
		$var_S = $del_Max / $val_Max;

		$del_R = ((($val_Max - $val_R) / 6.0) + ($del_Max / 2)) / $del_Max;
		$del_G = ((($val_Max - $val_G) / 6.0) + ($del_Max / 2)) / $del_Max;
		$del_B = ((($val_Max - $val_B) / 6.0) + ($del_Max / 2)) / $del_Max;

		     if ($val_R == $val_Max) $var_H = $del_B - $del_G;
		else if ($val_G == $val_Max) $var_H = (1.0/3.0) + $del_R - $del_B;
		else if ($val_B == $val_Max) $var_H = (2.0/3.0) + $del_G - $del_R;

		     if ($var_H < 0.0) $var_H += 1.0;
    	else if ($var_H > 1.0) $var_H -= 1.0;
	}
	
	debug_log("  →  HSV ".PCC([ $var_H, $var_S, $var_V ]));
	return [ $var_H, $var_S, $var_V ];
}

function RGBFromHSV($hsv_components) {
	$val_H = $hsv_components[0];
	$val_S = $hsv_components[1];
	$val_V = $hsv_components[2];

	$var_R = $var_G = $var_B = 0.0;

	if ($val_S != 0.0) {
		$var_h = $val_H * 6.0;
		if ($var_h == 6.0)
			$var_h = 0.0;
		$var_i = floor($var_h);
		$var_1 = $val_V * (1.0 - $val_S);
		$var_2 = $val_V * (1.0 - $val_S * ($var_h - $var_i));
		$var_3 = $val_V * (1.0 - $val_S * (1.0 - ($var_h - $var_i)));
		
		     if ($var_i == 0.0) { $var_R = $val_V; $var_G = $var_3; $var_B = $var_1; }
		else if ($var_i == 1.0) { $var_R = $var_2; $var_G = $val_V; $var_B = $var_1; }
		else if ($var_i == 2.0) { $var_R = $var_1; $var_G = $val_V; $var_B = $var_3; }
		else if ($var_i == 3.0) { $var_R = $var_1; $var_G = $var_2; $var_B = $val_V; }
		else if ($var_i == 4.0) { $var_R = $var_3; $var_G = $var_1; $var_B = $val_V; }
		else                    { $var_R = $val_V; $var_G = $var_1; $var_B = $var_2; }
	} else {
		$var_R = $var_G = $var_B = $val_V;
	}

	$var_R *= 255.0;
	$var_G *= 255.0;
	$var_B *= 255.0;
	
	debug_log("  →  RGB ".PCC([ $var_R, $var_G, $var_B ]));
	return [ $var_R, $var_G, $var_B ];
}

function XYZFromRGB($rgb_components) {
	$rgb_values = [
		$rgb_components[0],
		$rgb_components[1],
		$rgb_components[2]
	];

	foreach ($rgb_values as $i => $value) {
		$value /= 255.0;
		$rgb_values[$i] = $value > 0.04045
						  ? pow((($value + 0.055) / 1.055), 2.4) 
						  : $value / 12.92;
	}

	list($val_R, $val_G, $val_B) = $rgb_values;

	$var_X = $val_R * 0.4124 + $val_G * 0.3576 + $val_B * 0.1805;
	$var_Y = $val_R * 0.2126 + $val_G * 0.7152 + $val_B * 0.0722;
	$var_Z = $val_R * 0.0193 + $val_G * 0.1192 + $val_B * 0.9505;
	
	debug_log("  →  XYZ ".PCC([ $var_X, $var_Y, $var_Z ]));
	return [ $var_X, $var_Y, $var_Z ];
}

function RGBFromXYZ($xyz_components) {
	list($val_X, $val_Y, $val_Z) = $xyz_components;

	$var_R = $val_X *  3.2406 + $val_Y * -1.5372 + $val_Z * -0.4986;
	$var_G = $val_X * -0.9689 + $val_Y *  1.8758 + $val_Z *  0.0415;
	$var_B = $val_X *  0.0557 + $val_Y * -0.2040 + $val_Z *  1.0570;
	
	$rgb_values = [ $var_R, $var_G, $var_B ];
	foreach ($rgb_values as $i => $value) {
		$value = $value > 0.0031308 
				 ? 1.055 * pow($value, (1.0/2.4)) - 0.055 
				 : 12.92 * $value;
		$rgb_values[$i] = min(max($value, 0.0), 1.0) * 255.0;
	}
	
	debug_log("  →  RGB ".PCC($rgb_values));
	return $rgb_values;
}

function LabFromXYZ($xyz_components) {
	$xyz_values = [
		$xyz_components[0],
		$xyz_components[1],
		$xyz_components[2]
	];

	$xyz_values[0] /= 0.95047;
	$xyz_values[1] /= 1.00000;
	$xyz_values[2] /= 1.08883;

	foreach ($xyz_values as $i => $value) {
		$xyz_values[$i] = $value > 0.008856 
						  ? pow($value, (1.0/3.0)) 
						  : (7.787 * $value) + (0.16/1.16);
	}

	list($val_X, $val_Y, $val_Z) = $xyz_values;

	$var_L = (1.16 * $val_Y) - 0.16;
	$var_a = 5.0 * ($val_X - $val_Y);
	$var_b = 2.0 * ($val_Y - $val_Z);
	
	debug_log("  →  Lab ".PCC([ $var_L, $var_a, $var_b ]));
	return [ $var_L, $var_a, $var_b ];
}

function XYZFromLab($lab_components) {
	list($val_L, $val_a, $val_b) = $lab_components;

	$var_Y = ($val_L + 0.16) / 1.16;
	$var_X = $val_a / 5.0 + $var_Y;
	$var_Z = $var_Y - $val_b / 2.0;

	$xyz_values = [ $var_X, $var_Y, $var_Z ];
	
	foreach ($xyz_values as $i => $value) {
		$xyz_values[$i] = pow($value, 3) > 0.008856 
						  ? pow($value, 3) 
						  : ($value - 0.16/1.16) / 7.787;
	}

	$xyz_values[0] *= 0.95047;
	$xyz_values[1] *= 1.00000;
	$xyz_values[2] *= 1.08883;

	debug_log("  →  XYZ ".PCC($xyz_values));
	return $xyz_values;
}

?>