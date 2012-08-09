;;; tty-colors.el --- color support for character terminals

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: Eli Zaretskii
;; Maintainer: FSF
;; Keywords: terminals, faces

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs support for colors evolved from the X Window System; color
;; support for character-based terminals came later.  Many Lisp
;; packages use color names defined by X and assume the availability
;; of certain functions that look up colors, convert them to pixel
;; values, etc.

;; This file provides a more or less useful emulation of the X color
;; functionality for character-based terminals, and thus relieves the
;; rest of Emacs from including special code for this case.

;; Here's how it works.  The support for terminal and MSDOS frames
;; maintains an alist, called `tty-defined-color-alist', which
;; associates colors supported by the terminal driver with small
;; integers.  (These small integers are passed to the library
;; functions which set the color, and are effectively indices of the
;; colors in the supported color palette.)  When Emacs needs to send a
;; color command to the terminal, the color name is first looked up in
;; `tty-defined-color-alist'.  If not found, functions from this file
;; can be used to map the color to one of the supported colors.
;; Specifically, the X RGB values of the requested color are extracted
;; from `color-name-rgb-alist' and then the supported color is found
;; with the minimal distance in the RGB space from the requested
;; color.

;; `tty-defined-color-alist' is created at startup by calling the
;; function `tty-register-default-colors', defined below, which in
;; turn calls `tty-color-define', passing it each supported color, its
;; index, and its RGB values.  The standard list of colors supported
;; by many Unix color terminals, including xterm, FreeBSD, and
;; GNU/Linux, is supplied below in `tty-standard-colors'.  Some
;; terminal-specific files in lisp/term define their own standard
;; colors.  If your terminal supports different or additional colors,
;; call `tty-color-define' from your `.emacs' or `site-start.el'.  For
;; more-or-less standard definitions of VGA text-mode colors, see
;; lisp/term/pc-win.el.

;;; Code:

;; The following list is taken from rgb.txt distributed with X.
;;
;; WARNING: Some colors, such as "lightred", do not appear in this
;; list.  If you think it's a good idea to add them, don't!  The
;; problem is that the X-standard definition of "red" actually
;; corresponds to "lightred" on VGA (that's why pc-win.el and
;; w32-fns.el define "lightred" with the same RGB values as "red"
;; below).  Adding "lightred" here would therefore create confusing
;; and counter-intuitive results, like "red" and "lightred" being the
;; same color.  A similar situation exists with other "light*" colors.
;;
;; Nevertheless, "lightred" and other similar color names *are*
;; defined for the MS-DOS and MS-Windows consoles, because the users
;; on those systems expect these colors to be available.
;;
;; For these reasons, package maintainers are advised NOT to use color
;; names such as "lightred" or "lightblue", because they will have
;; different effect on different displays.  Instead, use "red1" and
;; "blue1", respectively.
;;
;; Note: the RGB values below are in the range 0-65535, but are derived
;; from the standard 8-bit X definitions (so the upper and lower bytes
;; of each value are actually identical).
;;
(defconst color-name-rgb-alist
  '(("snow"		65535 64250 64250)
    ("ghostwhite"	63736 63736 65535)
    ("whitesmoke"	62965 62965 62965)
    ("gainsboro"	56540 56540 56540)
    ("floralwhite"	65535 64250 61680)
    ("oldlace"		65021 62965 59110)
    ("linen"		64250 61680 59110)
    ("antiquewhite"	64250 60395 55255)
    ("papayawhip"	65535 61423 54741)
    ("blanchedalmond"	65535 60395 52685)
    ("bisque"		65535 58596 50372)
    ("peachpuff"	65535 56026 47545)
    ("navajowhite"	65535 57054 44461)
    ("moccasin"		65535 58596 46517)
    ("cornsilk"		65535 63736 56540)
    ("ivory"		65535 65535 61680)
    ("lemonchiffon"	65535 64250 52685)
    ("seashell"		65535 62965 61166)
    ("honeydew"		61680 65535 61680)
    ("mintcream"	62965 65535 64250)
    ("azure"		61680 65535 65535)
    ("aliceblue"	61680 63736 65535)
    ("lavender"		59110 59110 64250)
    ("lavenderblush"	65535 61680 62965)
    ("mistyrose"	65535 58596 57825)
    ("white"		65535 65535 65535)
    ("black"		    0     0     0)
    ("darkslategray"	12079 20303 20303)
    ("darkslategrey"	12079 20303 20303)
    ("dimgray"		26985 26985 26985)
    ("dimgrey"		26985 26985 26985)
    ("slategray"	28784 32896 37008)
    ("slategrey"	28784 32896 37008)
    ("lightslategray"	30583 34952 39321)
    ("lightslategrey"	30583 34952 39321)
    ("gray"		48830 48830 48830)
    ("grey"		48830 48830 48830)
    ("lightgrey"	54227 54227 54227)
    ("lightgray"	54227 54227 54227)
    ("midnightblue"	 6425  6425 28784)
    ("navy"		    0     0 32896)
    ("navyblue"		    0     0 32896)
    ("cornflowerblue"	25700 38293 60909)
    ("darkslateblue"	18504 15677 35723)
    ("slateblue"	27242 23130 52685)
    ("mediumslateblue"	31611 26728 61166)
    ("lightslateblue"	33924 28784 65535)
    ("mediumblue"	    0     0 52685)
    ("royalblue"	16705 26985 57825)
    ("blue"		    0     0 65535)
    ("dodgerblue"	 7710 37008 65535)
    ("deepskyblue"	    0 49087 65535)
    ("skyblue"		34695 52942 60395)
    ("lightskyblue"	34695 52942 64250)
    ("steelblue"	17990 33410 46260)
    ("lightsteelblue"	45232 50372 57054)
    ("lightblue"	44461 55512 59110)
    ("powderblue"	45232 57568 59110)
    ("paleturquoise"	44975 61166 61166)
    ("darkturquoise"	    0 52942 53713)
    ("mediumturquoise"	18504 53713 52428)
    ("turquoise"	16448 57568 53456)
    ("cyan"		    0 65535 65535)
    ("lightcyan"	57568 65535 65535)
    ("cadetblue"	24415 40606 41120)
    ("mediumaquamarine"	26214 52685 43690)
    ("aquamarine"	32639 65535 54484)
    ("darkgreen"	    0 25700     0)
    ("darkolivegreen"	21845 27499 12079)
    ("darkseagreen"	36751 48316 36751)
    ("seagreen"		11822 35723 22359)
    ("mediumseagreen"	15420 46003 29041)
    ("lightseagreen"	 8224 45746 43690)
    ("palegreen"	39064 64507 39064)
    ("springgreen"	    0 65535 32639)
    ("lawngreen"	31868 64764     0)
    ("green"		    0 65535     0)
    ("chartreuse"	32639 65535     0)
    ("mediumspringgreen"    0 64250 39578)
    ("greenyellow"	44461 65535 12079)
    ("limegreen"	12850 52685 12850)
    ("yellowgreen"	39578 52685 12850)
    ("forestgreen"	 8738 35723  8738)
    ("olivedrab"	27499 36494  8995)
    ("darkkhaki"	48573 47031 27499)
    ("khaki"		61680 59110 35980)
    ("palegoldenrod"	61166 59624 43690)
    ("lightgoldenrodyellow" 64250 64250 53970)
    ("lightyellow"	65535 65535 57568)
    ("yellow"		65535 65535     0)
    ("gold"		65535 55255     0)
    ("lightgoldenrod"	61166 56797 33410)
    ("goldenrod"	56026 42405  8224)
    ("darkgoldenrod"	47288 34438  2827)
    ("rosybrown"	48316 36751 36751)
    ("indianred"	52685 23644 23644)
    ("saddlebrown"	35723 17733  4883)
    ("sienna"		41120 21074 11565)
    ("peru"		52685 34181 16191)
    ("burlywood"	57054 47288 34695)
    ("beige"		62965 62965 56540)
    ("wheat"		62965 57054 46003)
    ("sandybrown"	62708 42148 24672)
    ("tan"		53970 46260 35980)
    ("chocolate"	53970 26985  7710)
    ("firebrick"	45746  8738  8738)
    ("brown"		42405 10794 10794)
    ("darksalmon"	59881 38550 31354)
    ("salmon"		64250 32896 29298)
    ("lightsalmon"	65535 41120 31354)
    ("orange"		65535 42405     0)
    ("darkorange"	65535 35980     0)
    ("coral"		65535 32639 20560)
    ("lightcoral"	61680 32896 32896)
    ("tomato"		65535 25443 18247)
    ("orangered"	65535 17733     0)
    ("red"		65535     0     0)
    ("hotpink"		65535 26985 46260)
    ("deeppink"		65535  5140 37779)
    ("pink"		65535 49344 52171)
    ("lightpink"	65535 46774 49601)
    ("palevioletred"	56283 28784 37779)
    ("maroon"		45232 12336 24672)
    ("mediumvioletred"	51143  5397 34181)
    ("violetred"	53456  8224 37008)
    ("magenta"		65535     0 65535)
    ("violet"		61166 33410 61166)
    ("plum"		56797 41120 56797)
    ("orchid"		56026 28784 54998)
    ("mediumorchid"	47802 21845 54227)
    ("darkorchid"	39321 12850 52428)
    ("darkviolet"	38036     0 54227)
    ("blueviolet"	35466 11051 58082)
    ("purple"		41120  8224 61680)
    ("mediumpurple"	37779 28784 56283)
    ("thistle"		55512 49087 55512)
    ("snow1"		65535 64250 64250)
    ("snow2"		61166 59881 59881)
    ("snow3"		52685 51657 51657)
    ("snow4"		35723 35209 35209)
    ("seashell1"	65535 62965 61166)
    ("seashell2"	61166 58853 57054)
    ("seashell3"	52685 50629 49087)
    ("seashell4"	35723 34438 33410)
    ("antiquewhite1"	65535 61423 56283)
    ("antiquewhite2"	61166 57311 52428)
    ("antiquewhite3"	52685 49344 45232)
    ("antiquewhite4"	35723 33667 30840)
    ("bisque1"		65535 58596 50372)
    ("bisque2"		61166 54741 47031)
    ("bisque3"		52685 47031 40606)
    ("bisque4"		35723 32125 27499)
    ("peachpuff1"	65535 56026 47545)
    ("peachpuff2"	61166 52171 44461)
    ("peachpuff3"	52685 44975 38293)
    ("peachpuff4"	35723 30583 25957)
    ("navajowhite1"	65535 57054 44461)
    ("navajowhite2"	61166 53199 41377)
    ("navajowhite3"	52685 46003 35723)
    ("navajowhite4"	35723 31097 24158)
    ("lemonchiffon1"	65535 64250 52685)
    ("lemonchiffon2"	61166 59881 49087)
    ("lemonchiffon3"	52685 51657 42405)
    ("lemonchiffon4"	35723 35209 28784)
    ("cornsilk1"	65535 63736 56540)
    ("cornsilk2"	61166 59624 52685)
    ("cornsilk3"	52685 51400 45489)
    ("cornsilk4"	35723 34952 30840)
    ("ivory1"		65535 65535 61680)
    ("ivory2"		61166 61166 57568)
    ("ivory3"		52685 52685 49601)
    ("ivory4"		35723 35723 33667)
    ("honeydew1"	61680 65535 61680)
    ("honeydew2"	57568 61166 57568)
    ("honeydew3"	49601 52685 49601)
    ("honeydew4"	33667 35723 33667)
    ("lavenderblush1"	65535 61680 62965)
    ("lavenderblush2"	61166 57568 58853)
    ("lavenderblush3"	52685 49601 50629)
    ("lavenderblush4"	35723 33667 34438)
    ("mistyrose1"	65535 58596 57825)
    ("mistyrose2"	61166 54741 53970)
    ("mistyrose3"	52685 47031 46517)
    ("mistyrose4"	35723 32125 31611)
    ("azure1"		61680 65535 65535)
    ("azure2"		57568 61166 61166)
    ("azure3"		49601 52685 52685)
    ("azure4"		33667 35723 35723)
    ("slateblue1"	33667 28527 65535)
    ("slateblue2"	31354 26471 61166)
    ("slateblue3"	26985 22873 52685)
    ("slateblue4"	18247 15420 35723)
    ("royalblue1"	18504 30326 65535)
    ("royalblue2"	17219 28270 61166)
    ("royalblue3"	14906 24415 52685)
    ("royalblue4"	10023 16448 35723)
    ("blue1"		    0     0 65535)
    ("blue2"		    0     0 61166)
    ("blue3"		    0     0 52685)
    ("blue4"		    0     0 35723)
    ("dodgerblue1"	 7710 37008 65535)
    ("dodgerblue2"	 7196 34438 61166)
    ("dodgerblue3"	 6168 29812 52685)
    ("dodgerblue4"	 4112 20046 35723)
    ("steelblue1"	25443 47288 65535)
    ("steelblue2"	23644 44204 61166)
    ("steelblue3"	20303 38036 52685)
    ("steelblue4"	13878 25700 35723)
    ("deepskyblue1"	    0 49087 65535)
    ("deepskyblue2"	    0 45746 61166)
    ("deepskyblue3"	    0 39578 52685)
    ("deepskyblue4"	    0 26728 35723)
    ("skyblue1"		34695 52942 65535)
    ("skyblue2"		32382 49344 61166)
    ("skyblue3"		27756 42662 52685)
    ("skyblue4"		19018 28784 35723)
    ("lightskyblue1"	45232 58082 65535)
    ("lightskyblue2"	42148 54227 61166)
    ("lightskyblue3"	36237 46774 52685)
    ("lightskyblue4"	24672 31611 35723)
    ("slategray1"	50886 58082 65535)
    ("slategray2"	47545 54227 61166)
    ("slategray3"	40863 46774 52685)
    ("slategray4"	27756 31611 35723)
    ("lightsteelblue1"	51914 57825 65535)
    ("lightsteelblue2"	48316 53970 61166)
    ("lightsteelblue3"	41634 46517 52685)
    ("lightsteelblue4"	28270 31611 35723)
    ("lightblue1"	49087 61423 65535)
    ("lightblue2"	45746 57311 61166)
    ("lightblue3"	39578 49344 52685)
    ("lightblue4"	26728 33667 35723)
    ("lightcyan1"	57568 65535 65535)
    ("lightcyan2"	53713 61166 61166)
    ("lightcyan3"	46260 52685 52685)
    ("lightcyan4"	31354 35723 35723)
    ("paleturquoise1"	48059 65535 65535)
    ("paleturquoise2"	44718 61166 61166)
    ("paleturquoise3"	38550 52685 52685)
    ("paleturquoise4"	26214 35723 35723)
    ("cadetblue1"	39064 62965 65535)
    ("cadetblue2"	36494 58853 61166)
    ("cadetblue3"	31354 50629 52685)
    ("cadetblue4"	21331 34438 35723)
    ("turquoise1"	    0 62965 65535)
    ("turquoise2"	    0 58853 61166)
    ("turquoise3"	    0 50629 52685)
    ("turquoise4"	    0 34438 35723)
    ("cyan1"		    0 65535 65535)
    ("cyan2"		    0 61166 61166)
    ("cyan3"		    0 52685 52685)
    ("cyan4"		    0 35723 35723)
    ("darkslategray1"	38807 65535 65535)
    ("darkslategray2"	36237 61166 61166)
    ("darkslategray3"	31097 52685 52685)
    ("darkslategray4"	21074 35723 35723)
    ("aquamarine1"	32639 65535 54484)
    ("aquamarine2"	30326 61166 50886)
    ("aquamarine3"	26214 52685 43690)
    ("aquamarine4"	17733 35723 29812)
    ("darkseagreen1"	49601 65535 49601)
    ("darkseagreen2"	46260 61166 46260)
    ("darkseagreen3"	39835 52685 39835)
    ("darkseagreen4"	26985 35723 26985)
    ("seagreen1"	21588 65535 40863)
    ("seagreen2"	20046 61166 38036)
    ("seagreen3"	17219 52685 32896)
    ("seagreen4"	11822 35723 22359)
    ("palegreen1"	39578 65535 39578)
    ("palegreen2"	37008 61166 37008)
    ("palegreen3"	31868 52685 31868)
    ("palegreen4"	21588 35723 21588)
    ("springgreen1"	    0 65535 32639)
    ("springgreen2"	    0 61166 30326)
    ("springgreen3"	    0 52685 26214)
    ("springgreen4"	    0 35723 17733)
    ("green1"		    0 65535     0)
    ("green2"		    0 61166     0)
    ("green3"		    0 52685     0)
    ("green4"		    0 35723     0)
    ("chartreuse1"	32639 65535     0)
    ("chartreuse2"	30326 61166     0)
    ("chartreuse3"	26214 52685     0)
    ("chartreuse4"	17733 35723     0)
    ("olivedrab1"	49344 65535 15934)
    ("olivedrab2"	46003 61166 14906)
    ("olivedrab3"	39578 52685 12850)
    ("olivedrab4"	26985 35723  8738)
    ("darkolivegreen1"	51914 65535 28784)
    ("darkolivegreen2"	48316 61166 26728)
    ("darkolivegreen3"	41634 52685 23130)
    ("darkolivegreen4"	28270 35723 15677)
    ("khaki1"		65535 63222 36751)
    ("khaki2"		61166 59110 34181)
    ("khaki3"		52685 50886 29555)
    ("khaki4"		35723 34438 20046)
    ("lightgoldenrod1"	65535 60652 35723)
    ("lightgoldenrod2"	61166 56540 33410)
    ("lightgoldenrod3"	52685 48830 28784)
    ("lightgoldenrod4"	35723 33153 19532)
    ("lightyellow1"	65535 65535 57568)
    ("lightyellow2"	61166 61166 53713)
    ("lightyellow3"	52685 52685 46260)
    ("lightyellow4"	35723 35723 31354)
    ("yellow1"		65535 65535     0)
    ("yellow2"		61166 61166     0)
    ("yellow3"		52685 52685     0)
    ("yellow4"		35723 35723     0)
    ("gold1"		65535 55255     0)
    ("gold2"		61166 51657     0)
    ("gold3"		52685 44461     0)
    ("gold4"		35723 30069     0)
    ("goldenrod1"	65535 49601  9509)
    ("goldenrod2"	61166 46260  8738)
    ("goldenrod3"	52685 39835  7453)
    ("goldenrod4"	35723 26985  5140)
    ("darkgoldenrod1"	65535 47545  3855)
    ("darkgoldenrod2"	61166 44461  3598)
    ("darkgoldenrod3"	52685 38293  3084)
    ("darkgoldenrod4"	35723 25957  2056)
    ("rosybrown1"	65535 49601 49601)
    ("rosybrown2"	61166 46260 46260)
    ("rosybrown3"	52685 39835 39835)
    ("rosybrown4"	35723 26985 26985)
    ("indianred1"	65535 27242 27242)
    ("indianred2"	61166 25443 25443)
    ("indianred3"	52685 21845 21845)
    ("indianred4"	35723 14906 14906)
    ("sienna1"		65535 33410 18247)
    ("sienna2"		61166 31097 16962)
    ("sienna3"		52685 26728 14649)
    ("sienna4"		35723 18247  9766)
    ("burlywood1"	65535 54227 39835)
    ("burlywood2"	61166 50629 37265)
    ("burlywood3"	52685 43690 32125)
    ("burlywood4"	35723 29555 21845)
    ("wheat1"		65535 59367 47802)
    ("wheat2"		61166 55512 44718)
    ("wheat3"		52685 47802 38550)
    ("wheat4"		35723 32382 26214)
    ("tan1"		65535 42405 20303)
    ("tan2"		61166 39578 18761)
    ("tan3"		52685 34181 16191)
    ("tan4"		35723 23130 11051)
    ("chocolate1"	65535 32639  9252)
    ("chocolate2"	61166 30326  8481)
    ("chocolate3"	52685 26214  7453)
    ("chocolate4"	35723 17733  4883)
    ("firebrick1"	65535 12336 12336)
    ("firebrick2"	61166 11308 11308)
    ("firebrick3"	52685  9766  9766)
    ("firebrick4"	35723  6682  6682)
    ("brown1"		65535 16448 16448)
    ("brown2"		61166 15163 15163)
    ("brown3"		52685 13107 13107)
    ("brown4"		35723  8995  8995)
    ("salmon1"		65535 35980 26985)
    ("salmon2"		61166 33410 25186)
    ("salmon3"		52685 28784 21588)
    ("salmon4"		35723 19532 14649)
    ("lightsalmon1"	65535 41120 31354)
    ("lightsalmon2"	61166 38293 29298)
    ("lightsalmon3"	52685 33153 25186)
    ("lightsalmon4"	35723 22359 16962)
    ("orange1"		65535 42405     0)
    ("orange2"		61166 39578     0)
    ("orange3"		52685 34181     0)
    ("orange4"		35723 23130     0)
    ("darkorange1"	65535 32639     0)
    ("darkorange2"	61166 30326     0)
    ("darkorange3"	52685 26214     0)
    ("darkorange4"	35723 17733     0)
    ("coral1"		65535 29298 22102)
    ("coral2"		61166 27242 20560)
    ("coral3"		52685 23387 17733)
    ("coral4"		35723 15934 12079)
    ("tomato1"		65535 25443 18247)
    ("tomato2"		61166 23644 16962)
    ("tomato3"		52685 20303 14649)
    ("tomato4"		35723 13878  9766)
    ("orangered1"	65535 17733     0)
    ("orangered2"	61166 16448     0)
    ("orangered3"	52685 14135     0)
    ("orangered4"	35723  9509     0)
    ("red1"		65535     0     0)
    ("red2"		61166     0     0)
    ("red3"		52685     0     0)
    ("red4"		35723     0     0)
    ("deeppink1"	65535  5140 37779)
    ("deeppink2"	61166  4626 35209)
    ("deeppink3"	52685  4112 30326)
    ("deeppink4"	35723  2570 20560)
    ("hotpink1"		65535 28270 46260)
    ("hotpink2"		61166 27242 42919)
    ("hotpink3"		52685 24672 37008)
    ("hotpink4"		35723 14906 25186)
    ("pink1"		65535 46517 50629)
    ("pink2"		61166 43433 47288)
    ("pink3"		52685 37265 40606)
    ("pink4"		35723 25443 27756)
    ("lightpink1"	65535 44718 47545)
    ("lightpink2"	61166 41634 44461)
    ("lightpink3"	52685 35980 38293)
    ("lightpink4"	35723 24415 25957)
    ("palevioletred1"	65535 33410 43947)
    ("palevioletred2"	61166 31097 40863)
    ("palevioletred3"	52685 26728 35209)
    ("palevioletred4"	35723 18247 23901)
    ("maroon1"		65535 13364 46003)
    ("maroon2"		61166 12336 42919)
    ("maroon3"		52685 10537 37008)
    ("maroon4"		35723  7196 25186)
    ("violetred1"	65535 15934 38550)
    ("violetred2"	61166 14906 35980)
    ("violetred3"	52685 12850 30840)
    ("violetred4"	35723  8738 21074)
    ("magenta1"		65535     0 65535)
    ("magenta2"		61166     0 61166)
    ("magenta3"		52685     0 52685)
    ("magenta4"		35723     0 35723)
    ("orchid1"		65535 33667 64250)
    ("orchid2"		61166 31354 59881)
    ("orchid3"		52685 26985 51657)
    ("orchid4"		35723 18247 35209)
    ("plum1"		65535 48059 65535)
    ("plum2"		61166 44718 61166)
    ("plum3"		52685 38550 52685)
    ("plum4"		35723 26214 35723)
    ("mediumorchid1"	57568 26214 65535)
    ("mediumorchid2"	53713 24415 61166)
    ("mediumorchid3"	46260 21074 52685)
    ("mediumorchid4"	31354 14135 35723)
    ("darkorchid1"	49087 15934 65535)
    ("darkorchid2"	45746 14906 61166)
    ("darkorchid3"	39578 12850 52685)
    ("darkorchid4"	26728  8738 35723)
    ("purple1"		39835 12336 65535)
    ("purple2"		37265 11308 61166)
    ("purple3"		32125  9766 52685)
    ("purple4"		21845  6682 35723)
    ("mediumpurple1"	43947 33410 65535)
    ("mediumpurple2"	40863 31097 61166)
    ("mediumpurple3"	35209 26728 52685)
    ("mediumpurple4"	23901 18247 35723)
    ("thistle1"		65535 57825 65535)
    ("thistle2"		61166 53970 61166)
    ("thistle3"		52685 46517 52685)
    ("thistle4"		35723 31611 35723)
    ("gray0"		    0     0     0)
    ("grey0"		    0     0     0)
    ("gray1"		  771   771   771)
    ("grey1"		  771   771   771)
    ("gray2"		 1285  1285  1285)
    ("grey2"		 1285  1285  1285)
    ("gray3"		 2056  2056  2056)
    ("grey3"		 2056  2056  2056)
    ("gray4"		 2570  2570  2570)
    ("grey4"		 2570  2570  2570)
    ("gray5"		 3341  3341  3341)
    ("grey5"		 3341  3341  3341)
    ("gray6"		 3855  3855  3855)
    ("grey6"		 3855  3855  3855)
    ("gray7"		 4626  4626  4626)
    ("grey7"		 4626  4626  4626)
    ("gray8"		 5140  5140  5140)
    ("grey8"		 5140  5140  5140)
    ("gray9"		 5911  5911  5911)
    ("grey9"		 5911  5911  5911)
    ("gray10"		 6682  6682  6682)
    ("grey10"		 6682  6682  6682)
    ("gray11"		 7196  7196  7196)
    ("grey11"		 7196  7196  7196)
    ("gray12"		 7967  7967  7967)
    ("grey12"		 7967  7967  7967)
    ("gray13"		 8481  8481  8481)
    ("grey13"		 8481  8481  8481)
    ("gray14"		 9252  9252  9252)
    ("grey14"		 9252  9252  9252)
    ("gray15"		 9766  9766  9766)
    ("grey15"		 9766  9766  9766)
    ("gray16"		10537 10537 10537)
    ("grey16"		10537 10537 10537)
    ("gray17"		11051 11051 11051)
    ("grey17"		11051 11051 11051)
    ("gray18"		11822 11822 11822)
    ("grey18"		11822 11822 11822)
    ("gray19"		12336 12336 12336)
    ("grey19"		12336 12336 12336)
    ("gray20"		13107 13107 13107)
    ("grey20"		13107 13107 13107)
    ("gray21"		13878 13878 13878)
    ("grey21"		13878 13878 13878)
    ("gray22"		14392 14392 14392)
    ("grey22"		14392 14392 14392)
    ("gray23"		15163 15163 15163)
    ("grey23"		15163 15163 15163)
    ("gray24"		15677 15677 15677)
    ("grey24"		15677 15677 15677)
    ("gray25"		16448 16448 16448)
    ("grey25"		16448 16448 16448)
    ("gray26"		16962 16962 16962)
    ("grey26"		16962 16962 16962)
    ("gray27"		17733 17733 17733)
    ("grey27"		17733 17733 17733)
    ("gray28"		18247 18247 18247)
    ("grey28"		18247 18247 18247)
    ("gray29"		19018 19018 19018)
    ("grey29"		19018 19018 19018)
    ("gray30"		19789 19789 19789)
    ("grey30"		19789 19789 19789)
    ("gray31"		20303 20303 20303)
    ("grey31"		20303 20303 20303)
    ("gray32"		21074 21074 21074)
    ("grey32"		21074 21074 21074)
    ("gray33"		21588 21588 21588)
    ("grey33"		21588 21588 21588)
    ("gray34"		22359 22359 22359)
    ("grey34"		22359 22359 22359)
    ("gray35"		22873 22873 22873)
    ("grey35"		22873 22873 22873)
    ("gray36"		23644 23644 23644)
    ("grey36"		23644 23644 23644)
    ("gray37"		24158 24158 24158)
    ("grey37"		24158 24158 24158)
    ("gray38"		24929 24929 24929)
    ("grey38"		24929 24929 24929)
    ("gray39"		25443 25443 25443)
    ("grey39"		25443 25443 25443)
    ("gray40"		26214 26214 26214)
    ("grey40"		26214 26214 26214)
    ("gray41"		26985 26985 26985)
    ("grey41"		26985 26985 26985)
    ("gray42"		27499 27499 27499)
    ("grey42"		27499 27499 27499)
    ("gray43"		28270 28270 28270)
    ("grey43"		28270 28270 28270)
    ("gray44"		28784 28784 28784)
    ("grey44"		28784 28784 28784)
    ("gray45"		29555 29555 29555)
    ("grey45"		29555 29555 29555)
    ("gray46"		30069 30069 30069)
    ("grey46"		30069 30069 30069)
    ("gray47"		30840 30840 30840)
    ("grey47"		30840 30840 30840)
    ("gray48"		31354 31354 31354)
    ("grey48"		31354 31354 31354)
    ("gray49"		32125 32125 32125)
    ("grey49"		32125 32125 32125)
    ("gray50"		32639 32639 32639)
    ("grey50"		32639 32639 32639)
    ("gray51"		33410 33410 33410)
    ("grey51"		33410 33410 33410)
    ("gray52"		34181 34181 34181)
    ("grey52"		34181 34181 34181)
    ("gray53"		34695 34695 34695)
    ("grey53"		34695 34695 34695)
    ("gray54"		35466 35466 35466)
    ("grey54"		35466 35466 35466)
    ("gray55"		35980 35980 35980)
    ("grey55"		35980 35980 35980)
    ("gray56"		36751 36751 36751)
    ("grey56"		36751 36751 36751)
    ("gray57"		37265 37265 37265)
    ("grey57"		37265 37265 37265)
    ("gray58"		38036 38036 38036)
    ("grey58"		38036 38036 38036)
    ("gray59"		38550 38550 38550)
    ("grey59"		38550 38550 38550)
    ("gray60"		39321 39321 39321)
    ("grey60"		39321 39321 39321)
    ("gray61"		40092 40092 40092)
    ("grey61"		40092 40092 40092)
    ("gray62"		40606 40606 40606)
    ("grey62"		40606 40606 40606)
    ("gray63"		41377 41377 41377)
    ("grey63"		41377 41377 41377)
    ("gray64"		41891 41891 41891)
    ("grey64"		41891 41891 41891)
    ("gray65"		42662 42662 42662)
    ("grey65"		42662 42662 42662)
    ("gray66"		43176 43176 43176)
    ("grey66"		43176 43176 43176)
    ("gray67"		43947 43947 43947)
    ("grey67"		43947 43947 43947)
    ("gray68"		44461 44461 44461)
    ("grey68"		44461 44461 44461)
    ("gray69"		45232 45232 45232)
    ("grey69"		45232 45232 45232)
    ("gray70"		46003 46003 46003)
    ("grey70"		46003 46003 46003)
    ("gray71"		46517 46517 46517)
    ("grey71"		46517 46517 46517)
    ("gray72"		47288 47288 47288)
    ("grey72"		47288 47288 47288)
    ("gray73"		47802 47802 47802)
    ("grey73"		47802 47802 47802)
    ("gray74"		48573 48573 48573)
    ("grey74"		48573 48573 48573)
    ("gray75"		49087 49087 49087)
    ("grey75"		49087 49087 49087)
    ("gray76"		49858 49858 49858)
    ("grey76"		49858 49858 49858)
    ("gray77"		50372 50372 50372)
    ("grey77"		50372 50372 50372)
    ("gray78"		51143 51143 51143)
    ("grey78"		51143 51143 51143)
    ("gray79"		51657 51657 51657)
    ("grey79"		51657 51657 51657)
    ("gray80"		52428 52428 52428)
    ("grey80"		52428 52428 52428)
    ("gray81"		53199 53199 53199)
    ("grey81"		53199 53199 53199)
    ("gray82"		53713 53713 53713)
    ("grey82"		53713 53713 53713)
    ("gray83"		54484 54484 54484)
    ("grey83"		54484 54484 54484)
    ("gray84"		54998 54998 54998)
    ("grey84"		54998 54998 54998)
    ("gray85"		55769 55769 55769)
    ("grey85"		55769 55769 55769)
    ("gray86"		56283 56283 56283)
    ("grey86"		56283 56283 56283)
    ("gray87"		57054 57054 57054)
    ("grey87"		57054 57054 57054)
    ("gray88"		57568 57568 57568)
    ("grey88"		57568 57568 57568)
    ("gray89"		58339 58339 58339)
    ("grey89"		58339 58339 58339)
    ("gray90"		58853 58853 58853)
    ("grey90"		58853 58853 58853)
    ("gray91"		59624 59624 59624)
    ("grey91"		59624 59624 59624)
    ("gray92"		60395 60395 60395)
    ("grey92"		60395 60395 60395)
    ("gray93"		60909 60909 60909)
    ("grey93"		60909 60909 60909)
    ("gray94"		61680 61680 61680)
    ("grey94"		61680 61680 61680)
    ("gray95"		62194 62194 62194)
    ("grey95"		62194 62194 62194)
    ("gray96"		62965 62965 62965)
    ("grey96"		62965 62965 62965)
    ("gray97"		63479 63479 63479)
    ("grey97"		63479 63479 63479)
    ("gray98"		64250 64250 64250)
    ("grey98"		64250 64250 64250)
    ("gray99"		64764 64764 64764)
    ("grey99"		64764 64764 64764)
    ("gray100"		65535 65535 65535)
    ("grey100"		65535 65535 65535)
    ("darkgrey"		43433 43433 43433)
    ("darkgray"		43433 43433 43433)
    ("darkblue"		    0     0 35723)
    ("darkcyan"		    0 35723 35723) ; no "lightmagenta", see comment above
    ("darkmagenta"	35723     0 35723)
    ("darkred"		35723     0     0)  ; but no "lightred", see comment above
    ("lightgreen"	37008 61166 37008))
  "An alist of X color names and associated 16-bit RGB values.")

(defconst tty-standard-colors
  '(("black"	0     0     0     0)
    ("red"	1 65535     0     0)
    ("green"	2     0 65535     0)
    ("yellow"	3 65535 65535     0)
    ("blue"	4     0     0 65535)
    ("magenta"	5 65535     0 65535)
    ("cyan"	6     0 65535 65535)
    ("white"	7 65535 65535 65535))
  "An alist of 8 standard tty colors, their indices and RGB values.")

;; This is used by term.c
(defconst tty-color-mode-alist
  '((never . -1)
    (no . -1)
    (default . 0)
    (auto . 0)
    (ansi8 . 8)
    (always . 8)
    (yes . 8))
  "An alist of supported standard tty color modes and their aliases.")

(defun tty-color-alist (&optional frame)
  "Return an alist of colors supported by FRAME's terminal.
FRAME defaults to the selected frame.
Each element of the returned alist is of the form:
 \(NAME INDEX R G B\)
where NAME is the name of the color, a string;
INDEX is the index of this color to be sent to the terminal driver
when the color should be displayed; it is typically a small integer;
R, G, and B are the intensities of, accordingly, red, green, and blue
components of the color, represented as numbers between 0 and 65535.
The file `etc/rgb.txt' in the Emacs distribution lists the standard
RGB values of the X colors.  If RGB is nil, this color will not be
considered by `tty-color-translate' as an approximation to another
color."
  tty-defined-color-alist)

(defun tty-modify-color-alist (elt &optional frame)
  "Put the association ELT into the alist of terminal colors for FRAME.
ELT should be of the form  \(NAME INDEX R G B\) (see `tty-color-alist'
for details).
If the association for NAME already exists in the color alist, it is
modified to specify \(INDEX R G B\) as its cdr.  Otherwise, ELT is
appended to the end of the color alist.
If FRAME is unspecified or nil, it defaults to the selected frame.
Value is the modified color alist for FRAME."
  (let* ((entry (assoc (car elt) (tty-color-alist frame))))
    (if entry
	(setcdr entry (cdr elt))
      ;; Keep the colors in the order they are registered.
      (setq entry
	    (list (append (list (car elt)
				(cadr elt))
			  (copy-sequence (cddr elt)))))
      (setq tty-defined-color-alist (nconc tty-defined-color-alist entry)))
    tty-defined-color-alist))

(defun tty-register-default-colors ()
  "Register the default set of colors for a character terminal."
  (let* ((colors tty-standard-colors)
	 (color (car colors)))
    (while colors
      (tty-color-define (car color) (cadr color) (cddr color))
      (setq colors (cdr colors) color (car colors)))
    ;; Modifying color mappings means realized faces don't
    ;; use the right colors, so clear them.
    (clear-face-cache)))

(defun tty-color-canonicalize (color)
  "Return COLOR in canonical form.
A canonicalized color name is all-lower case, with any blanks removed."
  (let ((case-fold-search nil))
    (if (string-match "[A-Z ]" color)
	(replace-regexp-in-string " +" "" (downcase color))
      color)))

(defun tty-color-define (name index &optional rgb frame)
  "Specify a tty color by its NAME, terminal INDEX and RGB values.
NAME is a string, INDEX is typically a small integer used to send to
the terminal driver a command to switch this color on, and RGB is a
list of 3 numbers that specify the intensity of red, green, and blue
components of the color.
If specified, each one of the RGB components must be a number between
0 and 65535.  If RGB is omitted, the specified color will never be used
by `tty-color-translate' as an approximation to another color.
FRAME is the frame where the defined color should be used.
If FRAME is not specified or is nil, it defaults to the selected frame."
  (if (or (not (stringp name))
	  (not (integerp index))
	  (and rgb (or (not (listp rgb)) (/= (length rgb) 3))))
      (error "Invalid specification for tty color \"%s\"" name))
  (tty-modify-color-alist
   (append (list (tty-color-canonicalize name) index) rgb) frame))

(defun tty-color-clear (&optional frame)
  "Clear the list of supported tty colors for frame FRAME.
If FRAME is unspecified or nil, it defaults to the selected frame."
  (setq tty-defined-color-alist nil))

(defun tty-color-off-gray-diag (r g b)
  "Compute the angle between the color given by R,G,B and the gray diagonal.
The gray diagonal is the diagonal of the 3D cube in RGB space which
connects the points corresponding to the black and white colors.  All the
colors whose RGB coordinates belong to this diagonal are various shades
of gray, thus the name."
  (let ((mag (sqrt (* 3 (+ (* r r) (* g g) (* b b))))))
    (if (< mag 1) 0 (acos (/ (+ r g b) mag)))))

(defun tty-color-approximate (rgb &optional frame)
  "Find the color in `tty-color-alist' that best approximates RGB.
Value is a list of the form \(NAME INDEX R G B\).
The argument RGB should be an rgb value, that is, a list of three
integers in the 0..65535 range.
FRAME defaults to the selected frame."
  (let* ((color-list (tty-color-alist frame))
	 (candidate (car color-list))
	 (best-distance 195076)	;; 3 * 255^2 + 15
	 (r (ash (car rgb) -8))
	 (g (ash (cadr rgb) -8))
	 (b (ash (nth 2 rgb) -8))
	 best-color)
    (while candidate
      (let ((try-rgb (cddr candidate))
	    ;; If the approximated color is not close enough to the
	    ;; gray diagonal of the RGB cube, favor non-gray colors.
	    ;; (The number 0.065 is an empirical ad-hoc'ery.)
	    (favor-non-gray (>= (tty-color-off-gray-diag r g b) 0.065))
	    try-r try-g try-b
	    dif-r dif-g dif-b dist)
	;; If the RGB values of the candidate color are unknown, we
	;; never consider it for approximating another color.
	(if try-rgb
	    (progn
	      (setq try-r (lsh (car try-rgb) -8)
		    try-g (lsh (cadr try-rgb) -8)
		    try-b (lsh (nth 2 try-rgb) -8))
	      (setq dif-r (- r try-r)
		    dif-g (- g try-g)
		    dif-b (- b try-b))
	      (setq dist (+ (* dif-r dif-r) (* dif-g dif-g) (* dif-b dif-b)))
	      (if (and (< dist best-distance)
		       ;; The candidate color is on the gray diagonal
		       ;; if its RGB components are all equal.
		       (or (/= try-r try-g) (/= try-g try-b)
			   (not favor-non-gray)))
		  (setq best-distance dist
			best-color candidate)))))
      (setq color-list (cdr color-list))
      (setq candidate (car color-list)))
    best-color))

(defun tty-color-standard-values (color)
"Return standard RGB values of the color COLOR.

The result is a list of integer RGB values--(RED GREEN BLUE).
These values range from 0 to 65535; white is (65535 65535 65535).

The returned value reflects the standard X definition of COLOR,
regardless of whether the terminal can display it, so the return value
should be the same regardless of what display is being used."
  (let ((len (length color)))
    (cond ((and (>= len 4) ;; X-style "#XXYYZZ" color spec
		(eq (aref color 0) ?#)
		(member (aref color 1)
			'(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
			     ?a ?b ?c ?d ?e ?f)))
	   ;; Translate the string "#XXYYZZ" into a list
	   ;; of numbers (XX YY ZZ).  If the primary colors
	   ;; are specified with less than 4 hex digits,
	   ;; the used digits represent the most significant
	   ;; bits of the value (e.g. #XYZ = #X000Y000Z000).
	   (let* ((ndig (/ (- len 1) 3))
		  (i1 1)
		  (i2 (+ i1 ndig))
		  (i3 (+ i2 ndig)))
	     (list
	      (lsh
	       (string-to-number (substring color i1 i2) 16)
	       (* 4 (- 4 ndig)))
	      (lsh
	       (string-to-number (substring color i2 i3) 16)
	       (* 4 (- 4 ndig)))
	      (lsh
	       (string-to-number (substring color i3) 16)
	       (* 4 (- 4 ndig))))))
	  ((and (>= len 9) ;; X-style RGB:xx/yy/zz color spec
		(string= (substring color 0 4) "rgb:"))
	   ;; Translate the string "RGB:XX/YY/ZZ" into a list
	   ;; of numbers (XX YY ZZ).  If fewer than 4 hex
	   ;; digits are used, they represent the fraction
	   ;; of the maximum value (RGB:X/Y/Z = #XXXXYYYYZZZZ).
	   (let* ((ndig (/ (- len 3) 3))
		  (maxval (1- (ash 1 (* 4 (- ndig 1)))))
		  (i1 4)
		  (i2 (+ i1 ndig))
		  (i3 (+ i2 ndig)))
	     (list
	      (/ (* (string-to-number
		     (substring color i1 (- i2 1)) 16)
		    255)
		 maxval)
	      (/ (* (string-to-number
		     (substring color i2 (- i3 1)) 16)
		    255)
		 maxval)
	      (/ (* (string-to-number
		     (substring color i3) 16)
		    255)
		 maxval))))
	  (t
	   (cdr (assoc color color-name-rgb-alist))))))

(defun tty-color-translate (color &optional frame)
  "Given a color COLOR, return the index of the corresponding TTY color.

COLOR must be a string that is either the color's name, or its X-style
specification like \"#RRGGBB\" or \"RGB:rr/gg/bb\", where each primary.
color can be given with 1 to 4 hex digits.

If COLOR is a color name that is found among supported colors in
`tty-color-alist', the associated index is returned.  Otherwise, the
RGB values of the color, either as given by the argument or from
looking up the name in `color-name-rgb-alist', are used to find the
supported color that is the best approximation for COLOR in the RGB
space.
If COLOR is neither a valid X RGB specification of the color, nor a
name of a color in `color-name-rgb-alist', the returned value is nil.

If FRAME is unspecified or nil, it defaults to the selected frame."
  (cadr (tty-color-desc color frame)))

(defun tty-color-by-index (idx &optional frame)
  "Given a numeric index of a tty color, return its description.

FRAME, if unspecified or nil, defaults to the selected frame.
Value is a list of the form \(NAME INDEX R G B\)."
  (and idx
       (let ((colors (tty-color-alist frame))
	     desc found)
	 (while colors
	   (setq desc (car colors))
	   (if (eq idx (car (cdr desc)))
	       (setq found desc))
	   (setq colors (cdr colors)))
	 found)))

(defun tty-color-values (color &optional frame)
  "Return RGB values of the color COLOR on a termcap frame FRAME.

If COLOR is not directly supported by the display, return the RGB
values for a supported color that is its best approximation.
The value is a list of integer RGB values--\(RED GREEN BLUE\).
These values range from 0 to 65535; white is (65535 65535 65535).
If FRAME is omitted or nil, use the selected frame."
  (cddr (tty-color-desc color frame)))

(defun tty-color-desc (color &optional frame)
  "Return the description of the color COLOR for a character terminal.
Value is a list of the form \(NAME INDEX R G B\).  The returned NAME or
RGB value may not be the same as the argument COLOR, because the latter
might need to be approximated if it is not supported directly."
  (and (stringp color)
       (let ((color (tty-color-canonicalize color)))
	  (or (assoc color (tty-color-alist frame))
	      (let ((rgb (tty-color-standard-values color)))
		(and rgb (tty-color-approximate rgb frame)))))))

(defun tty-color-gray-shades (&optional display)
  "Return the number of gray colors supported by DISPLAY's terminal.
A color is considered gray if the 3 components of its RGB value are equal."
  (let* ((frame (if (framep display) display
		  ;; FIXME: this uses an arbitrary frame from DISPLAY!
		  (car (frames-on-display-list display))))
	 (colors (tty-color-alist frame))
	 (count 0)
	 desc r g b)
    (while colors
      (setq desc (cddr (car colors))
	    r (car desc)
	    g (cadr desc)
	    b (car (cddr desc)))
      (and (numberp r)
	   (eq r g) (eq g b)
	   (setq count (1+ count)))
      (setq colors (cdr colors)))
    count))

;;; tty-colors.el ends here
