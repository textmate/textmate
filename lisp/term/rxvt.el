;;; rxvt.el --- define function key sequences and standard colors for rxvt

;; Copyright (C) 2002-2012  Free Software Foundation, Inc.

;; Author: Eli Zaretskii
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

(defvar rxvt-function-map
  (let ((map (make-sparse-keymap)))

    ;; Set up input-decode-map entries that termcap and terminfo don't know.
    (define-key map "\e[A" [up])
    (define-key map "\e[B" [down])
    (define-key map "\e[C" [right])
    (define-key map "\e[D" [left])
    (define-key map "\e[2~" [insert])
    (define-key map "\e[3~" [delete])
    (define-key map "\e[4~" [select])
    (define-key map "\e[5~" [prior])
    (define-key map "\e[6~" [next])
    (define-key map "\e[7~" [home])
    (define-key map "\e[8~" [end])
    (define-key map "\e[11~" [f1])
    (define-key map "\e[12~" [f2])
    (define-key map "\e[13~" [f3])
    (define-key map "\e[14~" [f4])
    (define-key map "\e[15~" [f5])
    (define-key map "\e[17~" [f6])
    (define-key map "\e[18~" [f7])
    (define-key map "\e[19~" [f8])
    (define-key map "\e[20~" [f9])
    (define-key map "\e[21~" [f10])
    ;; The strings emitted by f11 and f12 are the same as the strings
    ;; emitted by S-f1 and S-f2, so don't define f11 and f12.
    ;; (define-key rxvt-function-map "\e[23~" [f11])
    ;; (define-key rxvt-function-map "\e[24~" [f12])
    (define-key map "\e[29~" [print])

    (define-key map "\e[11^" [C-f1])
    (define-key map "\e[12^" [C-f2])
    (define-key map "\e[13^" [C-f3])
    (define-key map "\e[14^" [C-f4])
    (define-key map "\e[15^" [C-f5])
    (define-key map "\e[17^" [C-f6])
    (define-key map "\e[18^" [C-f7])
    (define-key map "\e[19^" [C-f8])
    (define-key map "\e[20^" [C-f9])
    (define-key map "\e[21^" [C-f10])

    (define-key map "\e[23~" [S-f1])
    (define-key map "\e[24~" [S-f2])
    (define-key map "\e[25~" [S-f3])
    (define-key map "\e[26~" [S-f4])
    (define-key map "\e[28~" [S-f5])
    (define-key map "\e[29~" [S-f6])
    (define-key map "\e[31~" [S-f7])
    (define-key map "\e[32~" [S-f8])
    (define-key map "\e[33~" [S-f9])
    (define-key map "\e[34~" [S-f10])

    (define-key map "\e[23^" [C-S-f1])
    (define-key map "\e[24^" [C-S-f2])
    (define-key map "\e[25^" [C-S-f3])
    (define-key map "\e[26^" [C-S-f4])
    (define-key map "\e[28^" [C-S-f5])
    (define-key map "\e[29^" [C-S-f6])
    (define-key map "\e[31^" [C-S-f7])
    (define-key map "\e[32^" [C-S-f8])
    (define-key map "\e[33^" [C-S-f9])
    (define-key map "\e[34^" [C-S-f10])

    (define-key map "\e[2^" [C-insert])
    (define-key map "\e[3^" [C-delete])
    (define-key map "\e[5^" [C-prior])
    (define-key map "\e[6^" [C-next])
    (define-key map "\e[7^" [C-home])
    (define-key map "\e[8^" [C-end])
    (define-key map "\eOd" [C-left])
    (define-key map "\eOc" [C-right])
    (define-key map "\eOa" [C-up])
    (define-key map "\eOb" [C-down])

    (define-key map "\e[2;2~" [S-insert])
    (define-key map "\e[3$" [S-delete])
    (define-key map "\e[5$" [S-prior])
    (define-key map "\e[6$" [S-next])
    (define-key map "\e[7$" [S-home])
    (define-key map "\e[8$" [S-end])
    (define-key map "\e[d" [S-left])
    (define-key map "\e[c" [S-right])
    (define-key map "\e[a" [S-up])
    (define-key map "\e[b" [S-down])
    map)
  "Function key overrides for rxvt.")

(defvar rxvt-alternatives-map
  (let ((map (make-sparse-keymap)))
    ;; The terminal initialization C code file might have initialized
    ;; function keys F11->F42 from the termcap/terminfo information.  On
    ;; a PC-style keyboard these keys correspond to
    ;; MODIFIER-FUNCTION_KEY, where modifier is S-, C-, C-S-.  The
    ;; code here substitutes the corresponding definitions in
    ;; function-key-map.  This substitution is needed because if a key
    ;; definition if found in function-key-map, there are no further
    ;; lookups in other keymaps.
    (define-key map [f11] [S-f1])
    (define-key map [f12] [S-f2])
    (define-key map [f13] [S-f3])
    (define-key map [f14] [S-f4])
    (define-key map [f15] [S-f5])
    (define-key map [f16] [S-f6])
    (define-key map [f17] [S-f7])
    (define-key map [f18] [S-f8])
    (define-key map [f19] [S-f9])
    (define-key map [f20] [S-f10])

    (define-key map [f23] [C-f1])
    (define-key map [f24] [C-f2])
    (define-key map [f25] [C-f3])
    (define-key map [f26] [C-f4])
    (define-key map [f27] [C-f5])
    (define-key map [f28] [C-f6])
    (define-key map [f29] [C-f7])
    (define-key map [f30] [C-f8])
    (define-key map [f31] [C-f9])
    (define-key map [f32] [C-f10])

    (define-key map [f33] [C-S-f1])
    (define-key map [f34] [C-S-f2])
    (define-key map [f35] [C-S-f3])
    (define-key map [f36] [C-S-f4])
    (define-key map [f37] [C-S-f5])
    (define-key map [f38] [C-S-f6])
    (define-key map [f39] [C-S-f7])
    (define-key map [f40] [C-S-f8])
    (define-key map [f41] [C-S-f9])
    (define-key map [f42] [C-S-f10])
    map)
  "Keymap of possible alternative meanings for some keys.")

(defun terminal-init-rxvt ()
  "Terminal initialization function for rxvt."

  (let ((map (copy-keymap rxvt-alternatives-map)))
    (set-keymap-parent map (keymap-parent local-function-key-map))
    (set-keymap-parent local-function-key-map map))

  ;; Use inheritance to let the main keymap override those defaults.
  ;; This way we don't override terminfo-derived settings or settings
  ;; made in the .emacs file.
  (let ((m (copy-keymap rxvt-function-map)))
    (set-keymap-parent m (keymap-parent input-decode-map))
    (set-keymap-parent input-decode-map m))

  ;; Initialize colors and background mode.
  (rxvt-register-default-colors)
  (rxvt-set-background-mode)
  ;; This recomputes all the default faces given the colors we've just set up.
  (tty-set-up-initial-frame-faces))

;; Set up colors, for those versions of rxvt that support it.
(defvar rxvt-standard-colors
  ;; The names of the colors in the comments taken from the rxvt.1 man
  ;; page; the corresponding RGB values--from rgb.txt.
  '(("black"          0 (  0   0   0))	; black
    ("red"            1 (205   0   0))	; red3
    ("green"          2 (  0 205   0))	; green3
    ("yellow"         3 (205 205   0))	; yellow3
    ("blue"           4 (  0   0 205))	; blue3
    ("magenta"        5 (205   0 205))	; magenta3
    ("cyan"           6 (  0 205 205))	; cyan3
    ("white"          7 (229 229 229))	; gray90
    ("brightblack"    8 ( 77  77  77))	; gray30
    ("brightred"      9 (255   0   0))	; red
    ("brightgreen"   10 (  0 255   0))	; green
    ("brightyellow"  11 (255 255   0))	; yellow
    ("brightblue"    12 (  0   0 255))	; blue
    ("brightmagenta" 13 (255   0 255))	; magenta
    ("brightcyan"    14 (  0 255 255))	; cyan
    ("brightwhite"   15 (255 255 255)))	; white
  "Names of 16 standard rxvt colors, their numbers, and RGB values.")

(defun rxvt-rgb-convert-to-16bit (prim)
  "Convert an 8-bit primary color value PRIM to a corresponding 16-bit value."
  (min 65535 (round (* (/ prim 255.0) 65535.0))))

(defun rxvt-register-default-colors ()
  "Register the default set of colors for rxvt or compatible emulator.

This function registers the number of colors returned by `display-color-cells'
for the currently selected frame."
  (let* ((ncolors (display-color-cells))
	 (colors rxvt-standard-colors)
	 (color (car colors)))
    (if (> ncolors 0)
	;; Clear the 8 default tty colors registered by startup.el
	(tty-color-clear))
    ;; Only register as many colors as are supported by the display.
    (while (and (> ncolors 0) colors)
      (tty-color-define (car color) (cadr color)
			(mapcar 'rxvt-rgb-convert-to-16bit
				(car (cddr color))))
      (setq colors (cdr colors)
	    color (car colors)
	    ncolors (1- ncolors)))
    (when (> ncolors 0)
      (cond
       ((= ncolors 240)			; 256-color rxvt
	;; 216 non-gray colors first
	(let ((r 0) (g 0) (b 0))
	  (while (> ncolors 24)
	    ;; This and other formulas taken from 256colres.pl and
	    ;; 88colres.pl in the xterm distribution.
	    (tty-color-define (format "color-%d" (- 256 ncolors))
			      (- 256 ncolors)
			      (mapcar 'rxvt-rgb-convert-to-16bit
				      (list (round (* r 42.5))
					    (round (* g 42.5))
					    (round (* b 42.5)))))
	    (setq b (1+ b))
	    (if (> b 5)
		(setq g (1+ g)
		      b 0))
	    (if (> g 5)
		(setq r (1+ r)
		      g 0))
	    (setq ncolors (1- ncolors))))
	;; Now the 24 gray colors
	(while (> ncolors 0)
	  (setq color (rxvt-rgb-convert-to-16bit (+ 8 (* (- 24 ncolors) 10))))
	  (tty-color-define (format "color-%d" (- 256 ncolors))
			    (- 256 ncolors)
			    (list color color color))
	  (setq ncolors (1- ncolors))))

       ((= ncolors 72) ; rxvt-unicode
	;; 64 non-gray colors
	(let ((levels '(0 139 205 255))
	      (r 0) (g 0) (b 0))
	  (while (> ncolors 8)
	    (tty-color-define (format "color-%d" (- 88 ncolors))
			      (- 88 ncolors)
			      (mapcar 'rxvt-rgb-convert-to-16bit
				      (list (nth r levels)
					    (nth g levels)
					    (nth b levels))))
	    (setq b (1+ b))
	    (if (> b 3)
		(setq g (1+ g)
		      b 0))
	    (if (> g 3)
		(setq r (1+ r)
		      g 0))
	    (setq ncolors (1- ncolors))))
	;; Now the 8 gray colors
	(while (> ncolors 0)
	  (setq color (rxvt-rgb-convert-to-16bit
		       (floor
			(if (= ncolors 8)
			    46.36363636
			  (+ (* (- 8 ncolors) 23.18181818) 69.54545454)))))
	  (tty-color-define (format "color-%d" (- 88 ncolors))
			    (- 88 ncolors)
			    (list color color color))
	  (setq ncolors (1- ncolors))))
       (t (error "Unsupported number of rxvt colors (%d)" (+ 16 ncolors)))))
    ;; Modifying color mappings means realized faces don't use the
    ;; right colors, so clear them.
    (clear-face-cache)))

;; rxvt puts the default colors into an environment variable
;; COLORFGBG.  We use this to set the background mode in a more
;; intelligent way than the default guesswork in startup.el.
(defun rxvt-set-background-mode ()
  "Set background mode as appropriate for the default rxvt colors."
  (let ((fgbg (getenv "COLORFGBG"))
	bg rgb)
    (set-terminal-parameter nil 'background-mode 'light)
    (when (and fgbg
	       (string-match ".*;\\([0-9][0-9]?\\)\\'" fgbg))
      (setq bg (string-to-number (substring fgbg (match-beginning 1))))
      ;; The next line assumes that rxvt-standard-colors are ordered
      ;; by the color index in the ascending order!
      (setq rgb (car (cddr (nth bg rxvt-standard-colors))))
      ;; See the commentary in frame-set-background-mode about the
      ;; computation below.
      (if (< (apply '+ rgb)
	     ;; The following line assumes that white is the 15th
	     ;; color in rxvt-standard-colors.
	     (* (apply '+ (car (cddr (nth 15 rxvt-standard-colors)))) 0.6))
	  (set-terminal-parameter nil 'background-mode 'dark)))))

;;; rxvt.el ends here
