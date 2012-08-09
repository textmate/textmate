;;; pulse.el --- Pulsing Overlays

;;; Copyright (C) 2007-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; Version: 1.0

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
;;
;; Manage temporary pulsing of faces and overlays.
;;
;; This is a temporal decoration technique where something is to be
;; highlighted briefly.  This adds a gentle pulsing style to the text
;; decorated this way.
;;
;; The following are useful entry points:
;;
;; `pulse' - Cause `pulse-highlight-face' to shift toward background color.
;;      Assumes you are using a version of Emacs that supports pulsing.
;;
;;
;; `pulse-momentary-highlight-one-line' - Pulse a single line at POINT.
;; `pulse-momentary-highlight-region' - Pulse a region.
;; `pulse-momentary-highlight-overlay' - Pulse an overlay.
;;      These three functions will just blink the specified area if
;;      the version of Emacs you are using doesn't support pulsing.
;;
;; `pulse-line-hook-function' - A simple function that can be used in a
;;      hook that will pulse whatever line the cursor is on.
;;
;;; History:
;;
;; The original pulse code was written for semantic tag highlighting.
;; It has been extracted, and adapted for general purpose pulsing.
;;
;; Pulse is a part of CEDET.  http://cedet.sf.net

(defun  pulse-available-p ()
  "Return non-nil if pulsing is available on the current frame."
  (condition-case nil
      (let ((v (color-values (face-background 'default))))
	(numberp (car-safe v)))
    (error nil)))

(defcustom pulse-flag (pulse-available-p)
  "Whether to use pulsing for momentary highlighting.
Pulsing involves a bright highlight that slowly shifts to the
background color.

If the value is nil, highlight with an unchanging color until a
key is pressed.
If the value is `never', do no coloring at all.
Any other value means to do the default pulsing behavior.

If `pulse-flag' is non-nil, but `pulse-available-p' is nil, then
this flag is ignored."
  :group 'pulse
  :type 'boolean)

(defface pulse-highlight-start-face
  '((((class color) (background dark))
     (:background "#AAAA33"))
    (((class color) (background light))
     (:background "#FFFFAA")))
  "*Face used at beginning of a highlight."
  :group 'pulse)

(defface pulse-highlight-face
  '((((class color) (background dark))
     (:background "#AAAA33"))
    (((class color) (background light))
     (:background "#FFFFAA")))
  "*Face used during a pulse for display.  *DO NOT CUSTOMIZE*
Face used for temporary highlighting of tags for effect."
  :group 'pulse)

;;; Code:
;;
(defun pulse-int-to-hex (int &optional nb-digits)
  "Convert integer argument INT to a #XXXXXXXXXXXX format hex string.
Each X in the output string is a hexadecimal digit.
NB-DIGITS is the number of hex digits.  If INT is too large to be
represented with NB-DIGITS, then the result is truncated from the
left.  So, for example, INT=256 and NB-DIGITS=2 returns \"00\", since
the hex equivalent of 256 decimal is 100, which is more than 2 digits.

This function was blindly copied from hexrgb.el by Drew Adams.
http://www.emacswiki.org/cgi-bin/wiki/hexrgb.el"
  (setq nb-digits (or nb-digits 4))
  (substring (format (concat "%0" (int-to-string nb-digits) "X") int) (- nb-digits)))

(defun pulse-color-values-to-hex (values)
  "Convert list of rgb color VALUES to a hex string, #XXXXXXXXXXXX.
Each X in the string is a hexadecimal digit.
Input VALUES is as for the output of `x-color-values'.

This function was blindly copied from hexrgb.el by Drew Adams.
http://www.emacswiki.org/cgi-bin/wiki/hexrgb.el"
  (concat "#"
          (pulse-int-to-hex (nth 0 values) 4) ; red
          (pulse-int-to-hex (nth 1 values) 4) ; green
          (pulse-int-to-hex (nth 2 values) 4))) ; blue

(defcustom pulse-iterations 10
  "Number of iterations in a pulse operation."
  :group 'pulse
  :type 'number)
(defcustom pulse-delay .03
  "Delay between face lightening iterations, as used by `sit-for'."
  :group 'pulse
  :type 'number)

(defun pulse-lighten-highlight ()
  "Lighten the face by 1/`pulse-iterations' toward the background color.
Return t if there is more drift to do, nil if completed."
  (if (>= (get 'pulse-highlight-face :iteration) pulse-iterations)
      nil
    (let* ((frame (color-values (face-background 'default)))
	   (start (color-values (face-background
				 (get 'pulse-highlight-face
				      :startface))))
	   (frac  (list (/ (- (nth 0 frame) (nth 0 start)) pulse-iterations)
			(/ (- (nth 1 frame) (nth 1 start)) pulse-iterations)
			(/ (- (nth 2 frame) (nth 2 start)) pulse-iterations)))
	   (it (get 'pulse-highlight-face :iteration))
	   )
      (set-face-background 'pulse-highlight-face
			   (pulse-color-values-to-hex
			    (list
			     (+ (nth 0 start) (* (nth 0 frac) it))
			     (+ (nth 1 start) (* (nth 1 frac) it))
			     (+ (nth 2 start) (* (nth 2 frac) it)))))
      (put 'pulse-highlight-face :iteration (1+ it))
      (if (>= (1+ it) pulse-iterations)
	  nil
	t))))

(defun pulse-reset-face (&optional face)
  "Reset the pulse highlighting FACE."
  (set-face-background 'pulse-highlight-face
		       (if face
			   (face-background face)
			 (face-background 'pulse-highlight-start-face)
			 ))
  (put 'pulse-highlight-face :startface (or face
					    'pulse-highlight-start-face))
  (put 'pulse-highlight-face :iteration 0))

(defun pulse (&optional face)
  "Pulse the colors on our highlight face.
If optional FACE is provided, reset the face to FACE color,
instead of `pulse-highlight-start-face'.
Be sure to call `pulse-reset-face' after calling pulse."
  (unwind-protect
      (progn
	(pulse-reset-face face)
	(while (and (pulse-lighten-highlight)
		    (sit-for pulse-delay))
	  nil))))

;;; Convenience Functions
;;
(defvar pulse-momentary-overlay nil
  "The current pulsing overlay.")

(defun pulse-momentary-highlight-overlay (o &optional face)
  "Pulse the overlay O, unhighlighting before next command.
Optional argument FACE specifies the fact to do the highlighting."
  (overlay-put o 'original-face (overlay-get o 'face))
  (add-to-list 'pulse-momentary-overlay o)
  (if (eq pulse-flag 'never)
      nil
    (if (or (not pulse-flag) (not (pulse-available-p)))
	;; Provide a face... clear on next command
	(progn
	  (overlay-put o 'face (or face 'pulse-highlight-start-face))
	  (add-hook 'pre-command-hook
		    'pulse-momentary-unhighlight))
      ;; pulse it.
      (unwind-protect
	  (progn
	    (overlay-put o 'face 'pulse-highlight-face)
	    ;; The pulse function puts FACE onto 'pulse-highlight-face.
	    ;; Thus above we put our face on the overlay, but pulse
	    ;; with a reference face needed for the color.
	    (pulse face))
	(pulse-momentary-unhighlight)))))

(defun pulse-momentary-unhighlight ()
  "Unhighlight a line recently highlighted."
  ;; If someone passes in an overlay, then pulse-momentary-overlay
  ;; will still be nil, and won't need modifying.
  (when pulse-momentary-overlay
    ;; clear the starting face
    (mapc
     (lambda (ol)
       (overlay-put ol 'face (overlay-get ol 'original-face))
       (overlay-put ol 'original-face nil)
       ;; Clear the overlay if it needs deleting.
       (when (overlay-get ol 'pulse-delete) (delete-overlay ol)))
     pulse-momentary-overlay)

    ;; Clear the variable.
    (setq pulse-momentary-overlay nil))

  ;; Reset the pulsing face.
  (pulse-reset-face)

  ;; Remove this hook.
  (remove-hook 'pre-command-hook 'pulse-momentary-unhighlight))

(defun pulse-momentary-highlight-one-line (point &optional face)
  "Highlight the line around POINT, unhighlighting before next command.
Optional argument FACE specifies the face to do the highlighting."
  (let ((start (point-at-bol))
	(end (save-excursion
	       (end-of-line)
	       (when (not (eobp))
		 (forward-char 1))
	       (point))))
    (pulse-momentary-highlight-region start end face)))

(defun pulse-momentary-highlight-region (start end &optional face)
  "Highlight between START and END, unhighlighting before next command.
Optional argument FACE specifies the fact to do the highlighting."
  (let ((o (make-overlay start end)))
    ;; Mark it for deletion
    (overlay-put o 'pulse-delete t)
    (pulse-momentary-highlight-overlay o face)))

;;; Random integration with other tools

(defvar pulse-command-advice-flag nil)

(defun pulse-line-hook-function ()
  "Function used in hooks to pulse the current line.
Only pulses the line if `pulse-command-advice-flag' is non-nil."
  (when pulse-command-advice-flag
    (pulse-momentary-highlight-one-line (point))))

(provide 'pulse)

;;; pulse.el ends here
