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
$gamma = @$argv[4] ?: 0.5;

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
	switch ($working_color_space) {
		case "YCC":
			$m[1] = HexFromRGB(RGBFromYCC(CVT(YCCFromRGB(RGBFromHex($m[1])), "YCC")));
			break;
		case "Lab":
		default:
			$m[1] = HexFromRGB(RGBFromXYZ(XYZFromLab(CVT(LabFromXYZ(XYZFromRGB(RGBFromHex($m[1]))), "Lab"))));
			break;
	}

	return implode(array_slice($m,1));
}
function ProcessColorValue_RGBA($m) {
	global $working_color_space;
	debug_log(PCC(array_slice($m, 1, 3)));
	$rgba = [ ];
	switch ($working_color_space) {
		case "YCC":
			$rgba = RGBFromYCC(CVT(YCCFromRGB(array_slice($m, 1, 3)), "YCC"));
			break;
		case "Lab":
		default:
			$rgba = RGBFromXYZ(XYZFromLab(CVT(LabFromXYZ(XYZFromRGB(array_slice($m, 1, 3))), "Lab")));
			break;
	}
	foreach ($rgba as $k => $v) {
		$rgba[$k] = round($v);
	}
	$rgba[] = $m[4];
	
	return "rgba(" . implode(", ", $rgba) . ")";
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
	## 0x0001: lightness inversion (in Lab or YCC).
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
		switch ($color_space) {
			case "Lab":
				$value[0] = 100 - $value[0];
				// Cut-rate gamma correction.
				$value[0] = 100.0 * pow($value[0] / 100.0, $gamma);
				break;
			case "YCC":
				$value[0] = 255 - $value[0];
				// Cut-rate gamma correction.
				$value[0] = 255.0 * pow($value[0] / 255.0, $gamma);
				break;
			default:
				break;
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
				$hsv_value = HSVFromRGB(RGBFromXYZ(XYZFromLab($value)));
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
				$value = LabFromXYZ(XYZFromRGB(RGBFromHSV($hsv_value)));
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
	$components = str_split($hexColorString,2);
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
	foreach ($components as $k => $v) {
		$components[$k] = round($v, 2);
	}
	return "( " . implode(", ", $components) . " )";
}

/**************************/
/* COLOR SPACE CONVERSION */
/**************************/

function YCCFromRGB($rgb_components) {
	$R = $rgb_components[0];
	$G = $rgb_components[1];
	$B = $rgb_components[2];

	$Y  = ( 0.25 * $R) + ( 0.50 * $G) + ( 0.25 * $B);
	$Co = ( 0.50 * $R) + ( 0.00 * $G) + (-0.50 * $B);
	$Cg = (-0.25 * $R) + ( 0.50 * $G) + (-0.25 * $B);

	debug_log("  →  YCC ".PCC([ $Y, $Co, $Cg ]));
	return [ $Y, $Co, $Cg ];
}

function RGBFromYCC($ycc_components) {
	$Y =  $ycc_components[0];
	$Co = $ycc_components[1];
	$Cg = $ycc_components[2];

	$R = ( 1 * $Y) + ( 1 * $Co) + (-1 * $Cg);
	$G = ( 1 * $Y) + ( 0 * $Co) + ( 1 * $Cg);
	$B = ( 1 * $Y) + (-1 * $Co) + (-1 * $Cg);
	
	$R = min($R, 255.0);
	$G = min($G, 255.0);
	$B = min($B, 255.0);

	debug_log("  →  RGB ".PCC([ $R, $G, $B ]));
	return [ $R, $G, $B ];
}

function HSVFromRGB($rgb_components) {
	$var_R = $rgb_components[0] / 255.0;
	$var_G = $rgb_components[1] / 255.0;
	$var_B = $rgb_components[2] / 255.0;

	$var_Min = min($var_R, $var_G, $var_B);
	$var_Max = max($var_R, $var_G, $var_B);
	$del_Max = $var_Max - $var_Min;

	$V = $var_Max;
	$H = 0;
	$S = 0;

	if ($del_Max != 0) {
		$S = $del_Max / $var_Max;

		$del_R = ((($var_Max - $var_R) / 6) + ($del_Max / 2)) / $del_Max;
		$del_G = ((($var_Max - $var_G) / 6) + ($del_Max / 2)) / $del_Max;
		$del_B = ((($var_Max - $var_B) / 6) + ($del_Max / 2)) / $del_Max;

		if ($var_R == $var_Max) $H = $del_B - $del_G;
		else if ($var_G == $var_Max) $H = (1.0/3.0) + $del_R - $del_B;
		else if ($var_B == $var_Max) $H = (2.0/3.0) + $del_G - $del_R;

		if ($H < 0) $H += 1;
    	else if ($H > 1) $H -= 1;
	}
	
	debug_log("  →  HSV ".PCC([ $H, $S, $V ]));
	return [ $H, $S, $V ];
}

function RGBFromHSV($hsv_components) {
	$H = $hsv_components[0];
	$S = $hsv_components[1];
	$V = $hsv_components[2];

	$R = $G = $B = $V * 255.0;
	
	if ($S != 0) {
		$var_h = $H * 6.0;
		if ($var_h == 6.0)
			$var_h = 0;
		$var_i = floor($var_h);
		$var_1 = $V * (1 - $S);
		$var_2 = $V * (1 - $S * ($var_h - $var_i));
		$var_3 = $V * (1 - $S * (1 - ($var_h - $var_i)));
		
		$var_r = $var_g = $var_b = 0.0;

		if ($var_i == 0) { $var_r = $V; $var_g = $var_3; $var_b = $var_1; }
		else if ($var_i == 1) { $var_r = $var_2; $var_g = $V; $var_b = $var_1; }
		else if ($var_i == 2) { $var_r = $var_1; $var_g = $V; $var_b = $var_3; }
		else if ($var_i == 3) { $var_r = $var_1; $var_g = $var_2; $var_b = $V; }
		else if ($var_i == 4) { $var_r = $var_3; $var_g = $var_1; $var_b = $V ; }
		else { $var_r = $V; $var_g = $var_1 ; $var_b = $var_2; }

		$R = $var_r * 255.0;
		$G = $var_g * 255.0;
		$B = $var_b * 255.0;
	}
	
	debug_log("  →  RGB ".PCC([ $R, $G, $B ]));
	return [ $R, $G, $B ];
}

function XYZFromRGB($rgb_components) {
	foreach ($rgb_components as $i => $component) {
		$component /= 255.0;
		$rgb_components[$i] = ($component > 0.04045) ?
							  (pow((($component + 0.055) / 1.055), 2.4)) :
							  ($component / 12.92);
	}

	$var_R = $rgb_components[0] * 100.0;
	$var_G = $rgb_components[1] * 100.0;
	$var_B = $rgb_components[2] * 100.0;

	$X = $var_R * 0.4124 + $var_G * 0.3576 + $var_B * 0.1805;
	$Y = $var_R * 0.2126 + $var_G * 0.7152 + $var_B * 0.0722;
	$Z = $var_R * 0.0193 + $var_G * 0.1192 + $var_B * 0.9505;
	
	debug_log("  →  XYZ ".PCC([ $X, $Y, $Z ]));
	return [ $X, $Y, $Z ];
}

function LabFromXYZ($xyz_components) {
	$xyz_components[0] /= 95.047;
	$xyz_components[1] /= 100.000;
	$xyz_components[2] /= 108.883;

	foreach ($xyz_components as $i => $component) {
		$xyz_components[$i] = ($component > 0.008856) ?
							  (pow($component, (1.0/3.0))) :
							  ((7.787 * $component) + (16.0/116.0));
	}

	$var_X = $xyz_components[0];
	$var_Y = $xyz_components[1];
	$var_Z = $xyz_components[2];

	$L = (116.0 * $var_Y) - 16.0;
	$a = 500.0 * ($var_X - $var_Y);
	$b = 200.0 * ($var_Y - $var_Z);
	
	debug_log("  →  Lab ".PCC([ $L, $a, $b ]));
	return [ $L, $a, $b ];
}

function XYZFromLab($lab_components) {
	
	$var_Y = ($lab_components[0] + 16.0) / 116.0;
	$var_X = $lab_components[1] / 500.0 + $var_Y;
	$var_Z = $var_Y - $lab_components[2] / 200.0;
	$xyz_components = [ $var_X, $var_Y, $var_Z ];
	
	foreach ($xyz_components as $i => $component) {
		$xyz_components[$i] = (pow($component, 3) > 0.008856) ?
							  (pow($component, 3)) :
							  (($component - 16.0/116.0) / 7.787);
	}

	$xyz_components[0] *= 95.047;
	$xyz_components[1] *= 100.000;
	$xyz_components[2] *= 108.883;

	debug_log("  →  XYZ ".PCC($xyz_components));
	return $xyz_components;
}

function RGBFromXYZ($xyz_components) {
	$var_X = $xyz_components[0] / 100.0;
	$var_Y = $xyz_components[1] / 100.0;
	$var_Z = $xyz_components[2] / 100.0;

	$var_R = $var_X *  3.2406 + $var_Y * -1.5372 + $var_Z * -0.4986;
	$var_G = $var_X * -0.9689 + $var_Y *  1.8758 + $var_Z *  0.0415;
	$var_B = $var_X *  0.0557 + $var_Y * -0.2040 + $var_Z *  1.0570;
	
	$rgb_components = [ $var_R, $var_G, $var_B ];
	foreach ($rgb_components as $i => $component) {
		$component = ($component > 0.0031308) ?
					 (1.055 * pow($component, (1.0/2.4)) - 0.055) : 
					 (12.92 * $component);
		$rgb_components[$i] = min(max($component, 0.0), 1.0) * 255.0;
	}
	
	debug_log("  →  RGB ".PCC($rgb_components));
	return $rgb_components;
}

?>