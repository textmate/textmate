;;; color.el --- Color manipulation library -*- coding: utf-8; -*-

;; Copyright (C) 2010-2012 Free Software Foundation, Inc.

;; Authors: Julien Danjou <julien@danjou.info>
;;          Drew Adams <drew.adams@oracle.com>
;; Keywords: lisp, faces, color, hex, rgb, hsv, hsl, cie-lab, background

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

;; This package provides functions for manipulating colors, including
;; converting between color representations, computing color
;; complements, and computing CIEDE2000 color distances.
;;
;; Supported color representations include RGB (red, green, blue), HSV
;; (hue, saturation, value), HSL (hue, saturation, luminance), sRGB,
;; CIE XYZ, and CIE L*a*b* color components.

;;; Code:

(eval-when-compile
  (require 'cl))

;; Emacs < 23.3
(eval-and-compile
  (unless (boundp 'float-pi)
    (defconst float-pi (* 4 (atan 1)) "The value of Pi (3.1415926...).")))

;;;###autoload
(defun color-name-to-rgb (color &optional frame)
  "Convert COLOR string to a list of normalized RGB components.
COLOR should be a color name (e.g. \"white\") or an RGB triplet
string (e.g. \"#ff12ec\").

Normally the return value is a list of three floating-point
numbers, (RED GREEN BLUE), each between 0.0 and 1.0 inclusive.

Optional arg FRAME specifies the frame where the color is to be
displayed.  If FRAME is omitted or nil, use the selected frame.
If FRAME cannot display COLOR, return nil."
  ;; `colors-values' maximum value is either 65535 or 65280 depending on the
  ;; display system. So we use a white conversion to get the max value.
  (let ((valmax (float (car (color-values "#ffffff")))))
    (mapcar (lambda (x) (/ x valmax)) (color-values color frame))))

(defun color-rgb-to-hex  (red green blue)
  "Return hexadecimal notation for the color RED GREEN BLUE.
RED GREEN BLUE must be numbers between 0.0 and 1.0 inclusive."
  (format "#%02x%02x%02x"
          (* red 255) (* green 255) (* blue 255)))

(defun color-complement (color-name)
  "Return the color that is the complement of COLOR-NAME.
COLOR-NAME should be a string naming a color (e.g. \"white\"), or
a string specifying a color's RGB components (e.g. \"#ff12ec\")."
  (let ((color (color-name-to-rgb color-name)))
    (list (- 1.0 (car color))
          (- 1.0 (cadr color))
          (- 1.0 (caddr color)))))

(defun color-gradient (start stop step-number)
  "Return a list with STEP-NUMBER colors from START to STOP.
The color list builds a color gradient starting at color START to
color STOP. It does not include the START and STOP color in the
resulting list."
  (let* ((r (nth 0 start))
	 (g (nth 1 start))
	 (b (nth 2 start))
	 (r-step (/ (- (nth 0 stop) r) (1+ step-number)))
	 (g-step (/ (- (nth 1 stop) g) (1+ step-number)))
	 (b-step (/ (- (nth 2 stop) b) (1+ step-number)))
	 result)
    (dotimes (n step-number)
      (push (list (setq r (+ r r-step))
		  (setq g (+ g g-step))
		  (setq b (+ b b-step)))
	    result))
    (nreverse result)))

(defun color-hue-to-rgb (v1 v2 h)
  "Compute hue from V1 and V2 H. Internally used by
`color-hsl-to-rgb'."
  (cond
   ((< h (/ 1.0 6))   (+ v1 (* (- v2 v1) h 6.0)))
   ((< h 0.5)         v2)
   ((< h (/ 2.0 3))   (+ v1 (* (- v2 v1) (- (/ 2.0 3) h) 6.0)))
   (t                 v1)))

(defun color-hsl-to-rgb (H S L)
    "Convert H S L (HUE, SATURATION, LUMINANCE) , where HUE is in
radians and both SATURATION and LUMINANCE are between 0.0 and
1.0, inclusive to their RGB representation.

Return a list (RED, GREEN, BLUE) which each be numbers between
0.0 and 1.0, inclusive."

  (if (= S 0.0)
      (list L L L)
    (let* ((m2 (if (<= L 0.5)
		   (* L (+ 1.0 S))
		 (- (+ L S) (* L S))))
	   (m1 (- (* 2.0 L) m2)))
      (list
       (color-hue-to-rgb m1 m2 (+ H (/ 1.0 3)))
       (color-hue-to-rgb m1 m2 H)
       (color-hue-to-rgb m1 m2 (- H (/ 1.0 3)))))))

(defun color-complement-hex (color)
  "Return the color that is the complement of COLOR, in hexadecimal format."
  (apply 'color-rgb-to-hex (color-complement color)))

(defun color-rgb-to-hsv (red green blue)
  "Convert RED, GREEN, and BLUE color components to HSV.
RED, GREEN, and BLUE should each be numbers between 0.0 and 1.0,
inclusive.  Return a list (HUE, SATURATION, VALUE), where HUE is
in radians and both SATURATION and VALUE are between 0.0 and 1.0,
inclusive."
  (let* ((r (float red))
	 (g (float green))
	 (b (float blue))
	 (max (max r g b))
	 (min (min r g b)))
    (if (< (- max min) 1e-8)
	(list 0.0 0.0 0.0)
      (list
       (/ (* 2 float-pi
	     (cond ((and (= r g) (= g b)) 0)
		   ((and (= r max)
			 (>= g b))
		    (* 60 (/ (- g b) (- max min))))
		   ((and (= r max)
			 (< g b))
		    (+ 360 (* 60 (/ (- g b) (- max min)))))
		   ((= max g)
		    (+ 120 (* 60 (/ (- b r) (- max min)))))
		   ((= max b)
		    (+ 240 (* 60 (/ (- r g) (- max min)))))))
	  360)
       (if (= max 0) 0 (- 1 (/ min max)))
       (/ max 255.0)))))

(defun color-rgb-to-hsl (red green blue)
  "Convert RED GREEN BLUE colors to their HSL representation.
RED, GREEN, and BLUE should each be numbers between 0.0 and 1.0,
inclusive.

Return a list (HUE, SATURATION, LUMINANCE), where HUE is in radians
and both SATURATION and LUMINANCE are between 0.0 and 1.0,
inclusive."
  (let* ((r red)
         (g green)
         (b blue)
         (max (max r g b))
         (min (min r g b))
         (delta (- max min))
         (l (/ (+ max min) 2.0)))
    (if (= delta 0)
	(list 0.0 0.0 l)
      (let* ((s (if (<= l 0.5) (/ delta (+ max min))
		  (/ delta (- 2.0 max min))))
	     (rc (/ (- max r) delta))
	     (gc (/ (- max g) delta))
	     (bc (/ (- max b) delta))
	     (h  (mod
		  (/
		   (cond
		    ((= r max)      (- bc gc))
		    ((= g max)      (+ 2.0 rc (- bc)))
		    (t              (+ 4.0 gc (- rc))))
		   6.0) 1.0)))
	(list h s l)))))

(defun color-srgb-to-xyz (red green blue)
  "Convert RED GREEN BLUE colors from the sRGB color space to CIE XYZ.
RED, BLUE and GREEN must be between 0 and 1, inclusive."
  (let ((r (if (<= red 0.04045)
               (/ red 12.95)
             (expt (/ (+ red 0.055) 1.055) 2.4)))
        (g (if (<= green 0.04045)
               (/ green 12.95)
             (expt (/ (+ green 0.055) 1.055) 2.4)))
        (b (if (<= blue 0.04045)
               (/ blue 12.95)
             (expt (/ (+ blue 0.055) 1.055) 2.4))))
    (list (+ (* 0.4124564 r) (* 0.3575761 g) (* 0.1804375 b))
          (+ (* 0.21266729 r) (* 0.7151522 g) (* 0.0721750 b))
          (+ (* 0.0193339 r) (* 0.1191920 g) (* 0.9503041 b)))))

(defun color-xyz-to-srgb (X Y Z)
  "Convert CIE X Y Z colors to sRGB color space."
  (let ((r (+ (* 3.2404542 X) (* -1.5371385 Y) (* -0.4985314 Z)))
        (g (+ (* -0.9692660 X) (* 1.8760108 Y) (* 0.0415560 Z)))
        (b (+ (* 0.0556434 X) (* -0.2040259 Y) (* 1.0572252 Z))))
    (list (if (<= r 0.0031308)
              (* 12.92 r)
            (- (* 1.055 (expt r (/ 1 2.4))) 0.055))
          (if (<= g 0.0031308)
              (* 12.92 g)
            (- (* 1.055 (expt g (/ 1 2.4))) 0.055))
          (if (<= b 0.0031308)
              (* 12.92 b)
            (- (* 1.055 (expt b (/ 1 2.4))) 0.055)))))

(defconst color-d65-xyz '(0.950455 1.0 1.088753)
  "D65 white point in CIE XYZ.")

(defconst color-cie-ε (/ 216 24389.0))
(defconst color-cie-κ (/ 24389 27.0))

(defun color-xyz-to-lab (X Y Z &optional white-point)
  "Convert CIE XYZ to CIE L*a*b*.
WHITE-POINT specifies the (X Y Z) white point for the
conversion. If omitted or nil, use `color-d65-xyz'."
  (destructuring-bind (Xr Yr Zr) (or white-point color-d65-xyz)
      (let* ((xr (/ X Xr))
             (yr (/ Y Yr))
             (zr (/ Z Zr))
             (fx (if (> xr color-cie-ε)
                     (expt xr (/ 1 3.0))
                   (/ (+ (* color-cie-κ xr) 16) 116.0)))
             (fy (if (> yr color-cie-ε)
                     (expt yr (/ 1 3.0))
                   (/ (+ (* color-cie-κ yr) 16) 116.0)))
             (fz (if (> zr color-cie-ε)
                     (expt zr (/ 1 3.0))
                   (/ (+ (* color-cie-κ zr) 16) 116.0))))
        (list
         (- (* 116 fy) 16)                  ; L
         (* 500 (- fx fy))                  ; a
         (* 200 (- fy fz))))))              ; b

(defun color-lab-to-xyz (L a b &optional white-point)
  "Convert CIE L*a*b* to CIE XYZ.
WHITE-POINT specifies the (X Y Z) white point for the
conversion. If omitted or nil, use `color-d65-xyz'."
  (destructuring-bind (Xr Yr Zr) (or white-point color-d65-xyz)
      (let* ((fy (/ (+ L 16) 116.0))
             (fz (- fy (/ b 200.0)))
             (fx (+ (/ a 500.0) fy))
             (xr (if (> (expt fx 3.0) color-cie-ε)
                     (expt fx 3.0)
               (/ (- (* fx 116) 16) color-cie-κ)))
             (yr (if (> L (* color-cie-κ color-cie-ε))
                     (expt (/ (+ L 16) 116.0) 3.0)
                   (/ L color-cie-κ)))
             (zr (if (> (expt fz 3) color-cie-ε)
                     (expt fz 3.0)
                   (/ (- (* 116 fz) 16) color-cie-κ))))
        (list (* xr Xr)                 ; X
              (* yr Yr)                 ; Y
              (* zr Zr)))))             ; Z

(defun color-srgb-to-lab (red green blue)
  "Convert RGB to CIE L*a*b*."
  (apply 'color-xyz-to-lab (color-srgb-to-xyz red green blue)))

(defun color-lab-to-srgb (L a b)
  "Convert CIE L*a*b* to RGB."
  (apply 'color-xyz-to-srgb (color-lab-to-xyz L a b)))

(defun color-cie-de2000 (color1 color2 &optional kL kC kH)
  "Return the CIEDE2000 color distance between COLOR1 and COLOR2.
Both COLOR1 and COLOR2 should be in CIE L*a*b* format, as
returned by `color-srgb-to-lab' or `color-xyz-to-lab'."
  (destructuring-bind (L₁ a₁ b₁) color1
    (destructuring-bind (L₂ a₂ b₂) color2
      (let* ((kL (or kL 1))
             (kC (or kC 1))
             (kH (or kH 1))
             (C₁ (sqrt (+ (expt a₁ 2.0) (expt b₁ 2.0))))
             (C₂ (sqrt (+ (expt a₂ 2.0) (expt b₂ 2.0))))
             (C̄ (/ (+ C₁ C₂) 2.0))
             (G (* 0.5 (- 1 (sqrt (/ (expt C̄ 7.0) (+ (expt C̄ 7.0) (expt 25 7.0)))))))
             (a′₁ (* (+ 1 G) a₁))
             (a′₂ (* (+ 1 G) a₂))
             (C′₁ (sqrt (+ (expt a′₁ 2.0) (expt b₁ 2.0))))
             (C′₂ (sqrt (+ (expt a′₂ 2.0) (expt b₂ 2.0))))
             (h′₁ (if (and (= b₁ 0) (= a′₁ 0))
                      0
                    (let ((v (atan b₁ a′₁)))
                      (if (< v 0)
                          (+ v (* 2 float-pi))
                        v))))
             (h′₂ (if (and (= b₂ 0) (= a′₂ 0))
                      0
                    (let ((v (atan b₂ a′₂)))
                      (if (< v 0)
                          (+ v (* 2 float-pi))
                        v))))
             (ΔL′ (- L₂ L₁))
             (ΔC′ (- C′₂ C′₁))
             (Δh′ (cond ((= (* C′₁ C′₂) 0)
                         0)
                        ((<= (abs (- h′₂ h′₁)) float-pi)
                         (- h′₂ h′₁))
                        ((> (- h′₂ h′₁) float-pi)
                         (- (- h′₂ h′₁) (* 2 float-pi)))
                        ((< (- h′₂ h′₁) (- float-pi))
                         (+ (- h′₂ h′₁) (* 2 float-pi)))))
             (ΔH′ (* 2 (sqrt (* C′₁ C′₂)) (sin (/ Δh′ 2.0))))
             (L̄′ (/ (+ L₁ L₂) 2.0))
             (C̄′ (/ (+ C′₁ C′₂) 2.0))
             (h̄′ (cond ((= (* C′₁ C′₂) 0)
                        (+ h′₁ h′₂))
                       ((<= (abs (- h′₁ h′₂)) float-pi)
                        (/ (+ h′₁ h′₂) 2.0))
                       ((< (+ h′₁ h′₂) (* 2 float-pi))
                        (/ (+ h′₁ h′₂ (* 2 float-pi)) 2.0))
                       ((>= (+ h′₁ h′₂) (* 2 float-pi))
                        (/ (+ h′₁ h′₂ (* -2 float-pi)) 2.0))))
             (T (+ 1
                   (- (* 0.17 (cos (- h̄′ (degrees-to-radians 30)))))
                   (* 0.24 (cos (* h̄′ 2)))
                   (* 0.32 (cos (+ (* h̄′ 3) (degrees-to-radians 6))))
                   (- (* 0.20 (cos (- (* h̄′ 4) (degrees-to-radians 63)))))))
             (Δθ (* (degrees-to-radians 30) (exp (- (expt (/ (- h̄′ (degrees-to-radians 275)) (degrees-to-radians 25)) 2.0)))))
             (Rc (* 2 (sqrt (/ (expt C̄′ 7.0) (+ (expt C̄′ 7.0) (expt 25.0 7.0))))))
             (Sl (+ 1 (/ (* 0.015 (expt (- L̄′ 50) 2.0)) (sqrt (+ 20 (expt (- L̄′ 50) 2.0))))))
             (Sc (+ 1 (* C̄′ 0.045)))
             (Sh (+ 1 (* 0.015 C̄′ T)))
             (Rt (- (* (sin (* Δθ 2)) Rc))))
        (sqrt (+ (expt (/ ΔL′ (* Sl kL)) 2.0)
                 (expt (/ ΔC′ (* Sc kC)) 2.0)
                 (expt (/ ΔH′ (* Sh kH)) 2.0)
                 (* Rt (/ ΔC′ (* Sc kC)) (/ ΔH′ (* Sh kH)))))))))

(defun color-clamp (value)
  "Make sure VALUE is a number between 0.0 and 1.0 inclusive."
  (min 1.0 (max 0.0 value)))

(defun color-saturate-hsl (H S L percent)
  "Return a color PERCENT more saturated than the one defined in
H S L color-space.

Return a list (HUE, SATURATION, LUMINANCE), where HUE is in radians
and both SATURATION and LUMINANCE are between 0.0 and 1.0,
inclusive."
  (list H (color-clamp (+ S (/ percent 100.0))) L))

(defun color-saturate-name (name percent)
  "Short hand to saturate COLOR by PERCENT.

See `color-saturate-hsl'."
  (apply 'color-rgb-to-hex
	 (apply 'color-hsl-to-rgb
		(apply 'color-saturate-hsl
		       (append
			(apply 'color-rgb-to-hsl
			       (color-name-to-rgb name))
			(list percent))))))

(defun color-desaturate-hsl (H S L percent)
  "Return a color PERCENT less saturated than the one defined in
H S L color-space.

Return a list (HUE, SATURATION, LUMINANCE), where HUE is in radians
and both SATURATION and LUMINANCE are between 0.0 and 1.0,
inclusive."
  (color-saturate-hsl H S L (- percent)))

(defun color-desaturate-name (name percent)
  "Short hand to desaturate COLOR by PERCENT.

See `color-desaturate-hsl'."
  (color-saturate-name name (- percent)))

(defun color-lighten-hsl (H S L percent)
  "Return a color PERCENT lighter than the one defined in
H S L color-space.

Return a list (HUE, SATURATION, LUMINANCE), where HUE is in radians
and both SATURATION and LUMINANCE are between 0.0 and 1.0,
inclusive."
  (list H S (color-clamp (+ L (/ percent 100.0)))))

(defun color-lighten-name (name percent)
  "Short hand to saturate COLOR by PERCENT.

See `color-lighten-hsl'."
  (apply 'color-rgb-to-hex
	 (apply 'color-hsl-to-rgb
		(apply 'color-lighten-hsl
		       (append
			(apply 'color-rgb-to-hsl
			       (color-name-to-rgb name))
			(list percent))))))

(defun color-darken-hsl (H S L percent)
  "Return a color PERCENT darker than the one defined in
H S L color-space.

Return a list (HUE, SATURATION, LUMINANCE), where HUE is in radians
and both SATURATION and LUMINANCE are between 0.0 and 1.0,
inclusive."
  (color-lighten-hsl H S L (- percent)))

(defun color-darken-name (name percent)
  "Short hand to saturate COLOR by PERCENT.

See `color-darken-hsl'."
  (color-lighten-name name (- percent)))

(provide 'color)

;;; color.el ends here
