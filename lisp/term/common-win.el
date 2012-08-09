;;; common-win.el --- common part of handling window systems

;; Copyright (C) 1993-1994, 2001-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: terminals

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

;;; Code:

(defcustom x-select-enable-clipboard t
  "Non-nil means cutting and pasting uses the clipboard.
This is in addition to, but in preference to, the primary selection.

Note that MS-Windows does not support selection types other than the
clipboard.  (The primary selection that is set by Emacs is not
accessible to other programs on MS-Windows.)

This variable is not used by the Nextstep port."
  :type 'boolean
  :group 'killing
  ;; The GNU/Linux version changed in 24.1, the MS-Windows version did not.
  :version "24.1")

(defvar x-last-selected-text)		; w32-fns.el
(declare-function w32-set-clipboard-data "w32select.c"
		  (string &optional ignored))
(defvar ns-last-selected-text)		; ns-win.el
(declare-function ns-set-pasteboard "ns-win" (string))

(defun x-select-text (text)
  "Select TEXT, a string, according to the window system.

On X, if `x-select-enable-clipboard' is non-nil, copy TEXT to the
clipboard.  If `x-select-enable-primary' is non-nil, put TEXT in
the primary selection.

On MS-Windows, make TEXT the current selection.  If
`x-select-enable-clipboard' is non-nil, copy the text to the
clipboard as well.

On Nextstep, put TEXT in the pasteboard (`x-select-enable-clipboard'
is not used)."
  (cond ((eq system-type 'windows-nt)
	 (if x-select-enable-clipboard
	     (w32-set-clipboard-data text))
	 (setq x-last-selected-text text))
	((featurep 'ns)
	 ;; Don't send the pasteboard too much text.
	 ;; It becomes slow, and if really big it causes errors.
	 (ns-set-pasteboard text)
	 (setq ns-last-selected-text text))
	(t
	 ;; With multi-tty, this function may be called from a tty frame.
	 (when (eq (framep (selected-frame)) 'x)
	   (when x-select-enable-primary
	     (x-set-selection 'PRIMARY text)
	     (setq x-last-selected-text-primary text))
	   (when x-select-enable-clipboard
	     (x-set-selection 'CLIPBOARD text)
	     (setq x-last-selected-text-clipboard text))))))

;;;; Function keys

(defvar x-alternatives-map
  (let ((map (make-sparse-keymap)))
    ;; Map certain keypad keys into ASCII characters that people usually expect.
    (define-key map [M-backspace] [?\M-\d])
    (define-key map [M-delete] [?\M-\d])
    (define-key map [M-tab] [?\M-\t])
    (define-key map [M-linefeed] [?\M-\n])
    (define-key map [M-clear] [?\M-\C-l])
    (define-key map [M-return] [?\M-\C-m])
    (define-key map [M-escape] [?\M-\e])
    (unless (featurep 'ns)
      (define-key map [iso-lefttab] [backtab])
      (define-key map [S-iso-lefttab] [backtab]))
    (and (or (eq system-type 'windows-nt)
	     (featurep 'ns))
	 (define-key map [S-tab] [backtab]))
    map)
  "Keymap of possible alternative meanings for some keys.")

(defun x-setup-function-keys (frame)
  "Set up `function-key-map' on the graphical frame FRAME."
  ;; Don't do this twice on the same display, or it would break
  ;; normal-erase-is-backspace-mode.
  (unless (terminal-parameter frame 'x-setup-function-keys)
    ;; Map certain keypad keys into ASCII characters that people usually expect.
    (with-selected-frame frame
      (let ((map (copy-keymap x-alternatives-map)))
        (set-keymap-parent map (keymap-parent local-function-key-map))
        (set-keymap-parent local-function-key-map map))
      (when (featurep 'ns)
	(setq interprogram-cut-function 'x-select-text
	      interprogram-paste-function 'x-selection-value
	      system-key-alist
	      (list
	       ;; These are special "keys" used to pass events from C to lisp.
	       (cons (logior (lsh 0 16)   1) 'ns-power-off)
	       (cons (logior (lsh 0 16)   2) 'ns-open-file)
	       (cons (logior (lsh 0 16)   3) 'ns-open-temp-file)
	       (cons (logior (lsh 0 16)   4) 'ns-drag-file)
	       (cons (logior (lsh 0 16)   5) 'ns-drag-color)
	       (cons (logior (lsh 0 16)   6) 'ns-drag-text)
	       (cons (logior (lsh 0 16)   7) 'ns-change-font)
	       (cons (logior (lsh 0 16)   8) 'ns-open-file-line)
;;;	       (cons (logior (lsh 0 16)   9) 'ns-insert-working-text)
;;;	       (cons (logior (lsh 0 16)  10) 'ns-delete-working-text)
	       (cons (logior (lsh 0 16)  11) 'ns-spi-service-call)
	       (cons (logior (lsh 0 16)  12) 'ns-new-frame)
	       (cons (logior (lsh 0 16)  13) 'ns-toggle-toolbar)
	       (cons (logior (lsh 0 16)  14) 'ns-show-prefs)
	       ))))
    (set-terminal-parameter frame 'x-setup-function-keys t)))

(defvar x-invocation-args)

(defvar x-command-line-resources nil)

;; Handler for switches of the form "-switch value" or "-switch".
(defun x-handle-switch (switch &optional numeric)
  (let ((aelt (assoc switch command-line-x-option-alist)))
    (if aelt
	(setq default-frame-alist
	      (cons (cons (nth 3 aelt)
			  (if numeric
			      (string-to-number (pop x-invocation-args))
			    (or (nth 4 aelt) (pop x-invocation-args))))
		    default-frame-alist)))))

;; Handler for switches of the form "-switch n"
(defun x-handle-numeric-switch (switch)
  (x-handle-switch switch t))

;; Handle options that apply to initial frame only
(defun x-handle-initial-switch (switch)
  (let ((aelt (assoc switch command-line-x-option-alist)))
    (if aelt
	(setq initial-frame-alist
	      (cons (cons (nth 3 aelt)
			  (or (nth 4 aelt) (pop x-invocation-args)))
		    initial-frame-alist)))))

;; Make -iconic apply only to the initial frame!
(defun x-handle-iconic (switch)
  (setq initial-frame-alist
	(cons '(visibility . icon) initial-frame-alist)))

;; Handle the -xrm option.
(defun x-handle-xrm-switch (switch)
  (unless (consp x-invocation-args)
    (error "%s: missing argument to `%s' option" (invocation-name) switch))
  (setq x-command-line-resources
	(if (null x-command-line-resources)
	    (pop x-invocation-args)
	  (concat x-command-line-resources "\n" (pop x-invocation-args)))))

(declare-function x-parse-geometry "frame.c" (string))

;; Handle the geometry option
(defun x-handle-geometry (switch)
  (let* ((geo (x-parse-geometry (pop x-invocation-args)))
	 (left (assq 'left geo))
	 (top (assq 'top geo))
	 (height (assq 'height geo))
	 (width (assq 'width geo)))
    (if (or height width)
	(setq default-frame-alist
	      (append default-frame-alist
		      '((user-size . t))
		      (if height (list height))
		      (if width (list width)))
	      initial-frame-alist
	      (append initial-frame-alist
		      '((user-size . t))
		      (if height (list height))
		      (if width (list width)))))
    (if (or left top)
	(setq initial-frame-alist
	      (append initial-frame-alist
		      '((user-position . t))
		      (if left (list left))
		      (if top (list top)))))))

(defvar x-resource-name)

;; Handle the -name option.  Set the variable x-resource-name
;; to the option's operand; set the name of
;; the initial frame, too.
(defun x-handle-name-switch (switch)
  (or (consp x-invocation-args)
      (error "%s: missing argument to `%s' option" (invocation-name) switch))
  (setq x-resource-name (pop x-invocation-args)
	initial-frame-alist (cons (cons 'name x-resource-name)
				  initial-frame-alist)))

(defvar x-display-name nil
  "The name of the window display on which Emacs was started.
On X, the display name of individual X frames is recorded in the
`display' frame parameter.")

(defun x-handle-display (switch)
  "Handle -display DISPLAY option."
  (setq x-display-name (pop x-invocation-args))
  ;; Make subshell programs see the same DISPLAY value Emacs really uses.
  ;; Note that this isn't completely correct, since Emacs can use
  ;; multiple displays.  However, there is no way to tell an already
  ;; running subshell which display the user is currently typing on.
  (setenv "DISPLAY" x-display-name))

(defun x-handle-args (args)
  "Process the X (or Nextstep) related command line options in ARGS.
This is done before the user's startup file is loaded.
Copies the options in ARGS to `x-invocation-args'.  It then extracts
the X (or Nextstep) options according to the handlers defined in
`command-line-x-option-alist' (or `command-line-ns-option-alist').
For example, `x-handle-switch' handles a switch like \"-fg\" and its
value \"black\".  This function returns ARGS minus the arguments that
have been processed."
  ;; We use ARGS to accumulate the args that we don't handle here, to return.
  (setq x-invocation-args args		; FIXME let-bind?
	args nil)
  (while (and x-invocation-args
	      (not (equal (car x-invocation-args) "--")))
    (let* ((this-switch (pop x-invocation-args))
	   (orig-this-switch this-switch)
	   (option-alist (if (featurep 'ns)
			     command-line-ns-option-alist
			   command-line-x-option-alist))
	   completion argval aelt handler)
      ;; Check for long options with attached arguments
      ;; and separate out the attached option argument into argval.
      (if (string-match "^--[^=]*=" this-switch)
	  (setq argval (substring this-switch (match-end 0))
		this-switch (substring this-switch 0 (1- (match-end 0)))))
      ;; Complete names of long options.
      (if (string-match "^--" this-switch)
	  (progn
	    (setq completion (try-completion this-switch option-alist))
	    (if (eq completion t)
		;; Exact match for long option.
		nil
	      (if (stringp completion)
		  (let ((elt (assoc completion option-alist)))
		    ;; Check for abbreviated long option.
		    (or elt
			(error "Option `%s' is ambiguous" this-switch))
		    (setq this-switch completion))))))
      (setq aelt (assoc this-switch option-alist))
      (if aelt (setq handler (nth 2 aelt)))
      (if handler
	  (if argval
	      (let ((x-invocation-args
		     (cons argval x-invocation-args)))
		(funcall handler this-switch))
	    (funcall handler this-switch))
	(setq args (cons orig-this-switch args)))))
  (nconc (nreverse args) x-invocation-args))


;;
;; Available colors
;;
;; The ordering of the colors is chosen for the user's convenience in
;; `list-colors-display', which displays the reverse of this list.
;; Roughly speaking, `list-colors-display' orders by (i) named shades
;; of gray with hue 0.0, sorted by value (ii) named colors with
;; saturation 1.0, sorted by hue, (iii) named non-white colors with
;; saturation less than 1.0, sorted by hue, (iv) other named shades of
;; white, (v) numbered colors sorted by hue, and (vi) numbered shades
;; of gray.

(declare-function ns-list-colors "nsfns.m" (&optional frame))

(defvar x-colors
  (if (featurep 'ns) (ns-list-colors)
    (purecopy
     '("gray100" "grey100" "gray99" "grey99" "gray98" "grey98" "gray97"
       "grey97" "gray96" "grey96" "gray95" "grey95" "gray94" "grey94"
       "gray93" "grey93" "gray92" "grey92" "gray91" "grey91" "gray90"
       "grey90" "gray89" "grey89" "gray88" "grey88" "gray87" "grey87"
       "gray86" "grey86" "gray85" "grey85" "gray84" "grey84" "gray83"
       "grey83" "gray82" "grey82" "gray81" "grey81" "gray80" "grey80"
       "gray79" "grey79" "gray78" "grey78" "gray77" "grey77" "gray76"
       "grey76" "gray75" "grey75" "gray74" "grey74" "gray73" "grey73"
       "gray72" "grey72" "gray71" "grey71" "gray70" "grey70" "gray69"
       "grey69" "gray68" "grey68" "gray67" "grey67" "gray66" "grey66"
       "gray65" "grey65" "gray64" "grey64" "gray63" "grey63" "gray62"
       "grey62" "gray61" "grey61" "gray60" "grey60" "gray59" "grey59"
       "gray58" "grey58" "gray57" "grey57" "gray56" "grey56" "gray55"
       "grey55" "gray54" "grey54" "gray53" "grey53" "gray52" "grey52"
       "gray51" "grey51" "gray50" "grey50" "gray49" "grey49" "gray48"
       "grey48" "gray47" "grey47" "gray46" "grey46" "gray45" "grey45"
       "gray44" "grey44" "gray43" "grey43" "gray42" "grey42" "gray41"
       "grey41" "gray40" "grey40" "gray39" "grey39" "gray38" "grey38"
       "gray37" "grey37" "gray36" "grey36" "gray35" "grey35" "gray34"
       "grey34" "gray33" "grey33" "gray32" "grey32" "gray31" "grey31"
       "gray30" "grey30" "gray29" "grey29" "gray28" "grey28" "gray27"
       "grey27" "gray26" "grey26" "gray25" "grey25" "gray24" "grey24"
       "gray23" "grey23" "gray22" "grey22" "gray21" "grey21" "gray20"
       "grey20" "gray19" "grey19" "gray18" "grey18" "gray17" "grey17"
       "gray16" "grey16" "gray15" "grey15" "gray14" "grey14" "gray13"
       "grey13" "gray12" "grey12" "gray11" "grey11" "gray10" "grey10"
       "gray9" "grey9" "gray8" "grey8" "gray7" "grey7" "gray6" "grey6"
       "gray5" "grey5" "gray4" "grey4" "gray3" "grey3" "gray2" "grey2"
       "gray1" "grey1" "gray0" "grey0"
       "LightPink1" "LightPink2" "LightPink3" "LightPink4"
       "pink1" "pink2" "pink3" "pink4"
       "PaleVioletRed1" "PaleVioletRed2" "PaleVioletRed3" "PaleVioletRed4"
       "LavenderBlush1" "LavenderBlush2" "LavenderBlush3" "LavenderBlush4"
       "VioletRed1" "VioletRed2" "VioletRed3" "VioletRed4"
       "HotPink1" "HotPink2" "HotPink3" "HotPink4"
       "DeepPink1" "DeepPink2" "DeepPink3" "DeepPink4"
       "maroon1" "maroon2" "maroon3" "maroon4"
       "orchid1" "orchid2" "orchid3" "orchid4"
       "plum1" "plum2" "plum3" "plum4"
       "thistle1" "thistle2" "thistle3" "thistle4"
       "MediumOrchid1" "MediumOrchid2" "MediumOrchid3" "MediumOrchid4"
       "DarkOrchid1" "DarkOrchid2" "DarkOrchid3" "DarkOrchid4"
       "purple1" "purple2" "purple3" "purple4"
       "MediumPurple1" "MediumPurple2" "MediumPurple3" "MediumPurple4"
       "SlateBlue1" "SlateBlue2" "SlateBlue3" "SlateBlue4"
       "RoyalBlue1" "RoyalBlue2" "RoyalBlue3" "RoyalBlue4"
       "LightSteelBlue1" "LightSteelBlue2" "LightSteelBlue3" "LightSteelBlue4"
       "SlateGray1" "SlateGray2" "SlateGray3" "SlateGray4"
       "DodgerBlue1" "DodgerBlue2" "DodgerBlue3" "DodgerBlue4"
       "SteelBlue1" "SteelBlue2" "SteelBlue3" "SteelBlue4"
       "SkyBlue1" "SkyBlue2" "SkyBlue3" "SkyBlue4"
       "LightSkyBlue1" "LightSkyBlue2" "LightSkyBlue3" "LightSkyBlue4"
       "LightBlue1" "LightBlue2" "LightBlue3" "LightBlue4"
       "CadetBlue1" "CadetBlue2" "CadetBlue3" "CadetBlue4"
       "azure1" "azure2" "azure3" "azure4"
       "LightCyan1" "LightCyan2" "LightCyan3" "LightCyan4"
       "PaleTurquoise1" "PaleTurquoise2" "PaleTurquoise3" "PaleTurquoise4"
       "DarkSlateGray1" "DarkSlateGray2" "DarkSlateGray3" "DarkSlateGray4"
       "aquamarine1" "aquamarine2" "aquamarine3" "aquamarine4"
       "SeaGreen1" "SeaGreen2" "SeaGreen3" "SeaGreen4"
       "honeydew1" "honeydew2" "honeydew3" "honeydew4"
       "DarkSeaGreen1" "DarkSeaGreen2" "DarkSeaGreen3" "DarkSeaGreen4"
       "PaleGreen1" "PaleGreen2" "PaleGreen3" "PaleGreen4"
       "DarkOliveGreen1" "DarkOliveGreen2" "DarkOliveGreen3" "DarkOliveGreen4"
       "OliveDrab1" "OliveDrab2" "OliveDrab3" "OliveDrab4"
       "ivory1" "ivory2" "ivory3" "ivory4"
       "LightYellow1" "LightYellow2" "LightYellow3" "LightYellow4"
       "khaki1" "khaki2" "khaki3" "khaki4"
       "LemonChiffon1" "LemonChiffon2" "LemonChiffon3" "LemonChiffon4"
       "LightGoldenrod1" "LightGoldenrod2" "LightGoldenrod3" "LightGoldenrod4"
       "cornsilk1" "cornsilk2" "cornsilk3" "cornsilk4"
       "goldenrod1" "goldenrod2" "goldenrod3" "goldenrod4"
       "DarkGoldenrod1" "DarkGoldenrod2" "DarkGoldenrod3" "DarkGoldenrod4"
       "wheat1" "wheat2" "wheat3" "wheat4"
       "NavajoWhite1" "NavajoWhite2" "NavajoWhite3" "NavajoWhite4"
       "burlywood1" "burlywood2" "burlywood3" "burlywood4"
       "AntiqueWhite1" "AntiqueWhite2" "AntiqueWhite3" "AntiqueWhite4"
       "bisque1" "bisque2" "bisque3" "bisque4"
       "tan1" "tan2" "tan3" "tan4"
       "PeachPuff1" "PeachPuff2" "PeachPuff3" "PeachPuff4"
       "seashell1" "seashell2" "seashell3" "seashell4"
       "chocolate1" "chocolate2" "chocolate3" "chocolate4"
       "sienna1" "sienna2" "sienna3" "sienna4"
       "LightSalmon1" "LightSalmon2" "LightSalmon3" "LightSalmon4"
       "salmon1" "salmon2" "salmon3" "salmon4"
       "coral1" "coral2" "coral3" "coral4"
       "tomato1" "tomato2" "tomato3" "tomato4"
       "MistyRose1" "MistyRose2" "MistyRose3" "MistyRose4"
       "snow1" "snow2" "snow3" "snow4"
       "RosyBrown1" "RosyBrown2" "RosyBrown3" "RosyBrown4"
       "IndianRed1" "IndianRed2" "IndianRed3" "IndianRed4"
       "firebrick1" "firebrick2" "firebrick3" "firebrick4"
       "brown1" "brown2" "brown3" "brown4"
       "magenta1" "magenta2" "magenta3" "magenta4"
       "blue1" "blue2" "blue3" "blue4"
       "DeepSkyBlue1" "DeepSkyBlue2" "DeepSkyBlue3" "DeepSkyBlue4"
       "turquoise1" "turquoise2" "turquoise3" "turquoise4"
       "cyan1" "cyan2" "cyan3" "cyan4"
       "SpringGreen1" "SpringGreen2" "SpringGreen3" "SpringGreen4"
       "green1" "green2" "green3" "green4"
       "chartreuse1" "chartreuse2" "chartreuse3" "chartreuse4"
       "yellow1" "yellow2" "yellow3" "yellow4"
       "gold1" "gold2" "gold3" "gold4"
       "orange1" "orange2" "orange3" "orange4"
       "DarkOrange1" "DarkOrange2" "DarkOrange3" "DarkOrange4"
       "OrangeRed1" "OrangeRed2" "OrangeRed3" "OrangeRed4"
       "red1" "red2" "red3" "red4"
       "lavender blush" "LavenderBlush" "ghost white" "GhostWhite"
       "lavender" "alice blue" "AliceBlue" "azure" "light cyan"
       "LightCyan" "mint cream" "MintCream" "honeydew" "ivory"
       "light goldenrod yellow" "LightGoldenrodYellow" "light yellow"
       "LightYellow" "beige" "floral white" "FloralWhite" "old lace"
       "OldLace" "blanched almond" "BlanchedAlmond" "moccasin"
       "papaya whip" "PapayaWhip" "bisque" "antique white"
       "AntiqueWhite" "linen" "peach puff" "PeachPuff" "seashell"
       "misty rose" "MistyRose" "snow" "light pink" "LightPink" "pink"
       "hot pink" "HotPink" "deep pink" "DeepPink" "maroon"
       "pale violet red" "PaleVioletRed" "violet red" "VioletRed"
       "medium violet red" "MediumVioletRed" "violet" "plum" "thistle"
       "orchid" "medium orchid" "MediumOrchid" "dark orchid"
       "DarkOrchid" "purple" "blue violet" "BlueViolet" "medium purple"
       "MediumPurple" "light slate blue" "LightSlateBlue"
       "medium slate blue" "MediumSlateBlue" "slate blue" "SlateBlue"
       "dark slate blue" "DarkSlateBlue" "midnight blue" "MidnightBlue"
       "navy" "navy blue" "NavyBlue" "dark blue" "DarkBlue"
       "light steel blue" "LightSteelBlue" "cornflower blue"
       "CornflowerBlue" "dodger blue" "DodgerBlue" "royal blue"
       "RoyalBlue" "light slate gray" "light slate grey"
       "LightSlateGray" "LightSlateGrey" "slate gray" "slate grey"
       "SlateGray" "SlateGrey" "dark slate gray" "dark slate grey"
       "DarkSlateGray" "DarkSlateGrey" "steel blue" "SteelBlue"
       "cadet blue" "CadetBlue" "light sky blue" "LightSkyBlue"
       "sky blue" "SkyBlue" "light blue" "LightBlue" "powder blue"
       "PowderBlue" "pale turquoise" "PaleTurquoise" "turquoise"
       "medium turquoise" "MediumTurquoise" "dark turquoise"
       "DarkTurquoise"  "dark cyan" "DarkCyan" "aquamarine"
       "medium aquamarine" "MediumAquamarine" "light sea green"
       "LightSeaGreen" "medium sea green" "MediumSeaGreen" "sea green"
       "SeaGreen" "dark sea green" "DarkSeaGreen" "pale green"
       "PaleGreen" "lime green" "LimeGreen" "dark green" "DarkGreen"
       "forest green" "ForestGreen" "light green" "LightGreen"
       "green yellow" "GreenYellow" "yellow green" "YellowGreen"
       "olive drab" "OliveDrab" "dark olive green" "DarkOliveGreen"
       "lemon chiffon" "LemonChiffon" "khaki" "dark khaki" "DarkKhaki"
       "cornsilk" "pale goldenrod" "PaleGoldenrod" "light goldenrod"
       "LightGoldenrod" "goldenrod" "dark goldenrod" "DarkGoldenrod"
       "wheat" "navajo white" "NavajoWhite" "tan" "burlywood"
       "sandy brown" "SandyBrown" "peru" "chocolate" "saddle brown"
       "SaddleBrown" "sienna" "rosy brown" "RosyBrown" "dark salmon"
       "DarkSalmon" "coral" "tomato" "light salmon" "LightSalmon"
       "salmon" "light coral" "LightCoral" "indian red" "IndianRed"
       "firebrick" "brown" "dark red" "DarkRed" "magenta"
       "dark magenta" "DarkMagenta" "dark violet" "DarkViolet"
       "medium blue" "MediumBlue" "blue" "deep sky blue" "DeepSkyBlue"
       "cyan" "medium spring green" "MediumSpringGreen" "spring green"
       "SpringGreen" "green" "lawn green" "LawnGreen" "chartreuse"
       "yellow" "gold" "orange" "dark orange" "DarkOrange" "orange red"
       "OrangeRed" "red" "white" "white smoke" "WhiteSmoke" "gainsboro"
       "light gray" "light grey" "LightGray" "LightGrey" "gray" "grey"
       "dark gray" "dark grey" "DarkGray" "DarkGrey" "dim gray"
       "dim grey" "DimGray" "DimGrey" "black")))
  "List of basic colors available on color displays.
For X, the list comes from the `rgb.txt' file,v 10.41 94/02/20.
For Nextstep, this is a list of non-PANTONE colors returned by
the operating system.")

(defvar w32-color-map)

(defun xw-defined-colors (&optional frame)
  "Internal function called by `defined-colors', which see."
  (if (featurep 'ns)
      x-colors
    (or frame (setq frame (selected-frame)))
    (let (defined-colors)
      (dolist (this-color (if (eq system-type 'windows-nt)
			      (or (mapcar 'car w32-color-map) x-colors)
			    x-colors))
	(and (color-supported-p this-color frame t)
	     (setq defined-colors (cons this-color defined-colors))))
      defined-colors)))

;;; common-win.el ends here
