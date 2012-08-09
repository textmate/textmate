;;; disp-table.el --- functions for dealing with char tables

;; Copyright (C) 1987, 1994-1995, 1999, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: Erik Naggum <erik@naggum.no>
;; Based on a previous version by Howard Gayle
;; Maintainer: FSF
;; Keywords: i18n
;; Package: emacs

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

(put 'display-table 'char-table-extra-slots 6)

;;;###autoload
(defun make-display-table ()
  "Return a new, empty display table."
  (make-char-table 'display-table nil))

(or standard-display-table
    (setq standard-display-table (make-display-table)))

;;; Display-table slot names.  The property value says which slot.

(put 'truncation 'display-table-slot 0)
(put 'wrap 'display-table-slot 1)
(put 'escape 'display-table-slot 2)
(put 'control 'display-table-slot 3)
(put 'selective-display 'display-table-slot 4)
(put 'vertical-border 'display-table-slot 5)

;;;###autoload
(defun display-table-slot (display-table slot)
  "Return the value of the extra slot in DISPLAY-TABLE named SLOT.
SLOT may be a number from 0 to 5 inclusive, or a slot name (symbol).
Valid symbols are `truncation', `wrap', `escape', `control',
`selective-display', and `vertical-border'."
  (let ((slot-number
	 (if (numberp slot) slot
	   (or (get slot 'display-table-slot)
	       (error "Invalid display-table slot name: %s" slot)))))
    (char-table-extra-slot display-table slot-number)))

;;;###autoload
(defun set-display-table-slot (display-table slot value)
  "Set the value of the extra slot in DISPLAY-TABLE named SLOT to VALUE.
SLOT may be a number from 0 to 5 inclusive, or a name (symbol).
Valid symbols are `truncation', `wrap', `escape', `control',
`selective-display', and `vertical-border'."
  (let ((slot-number
	 (if (numberp slot) slot
	   (or (get slot 'display-table-slot)
	       (error "Invalid display-table slot name: %s" slot)))))
    (set-char-table-extra-slot display-table slot-number value)))

;;;###autoload
(defun describe-display-table (dt)
  "Describe the display table DT in a help buffer."
  (with-help-window "*Help*"
    (princ "\nTruncation glyph: ")
    (prin1 (display-table-slot dt 'truncation))
    (princ "\nWrap glyph: ")
    (prin1 (display-table-slot dt 'wrap))
    (princ "\nEscape glyph: ")
    (prin1 (display-table-slot dt 'escape))
    (princ "\nCtrl glyph: ")
    (prin1 (display-table-slot dt 'control))
    (princ "\nSelective display glyph sequence: ")
    (prin1 (display-table-slot dt 'selective-display))
    (princ "\nVertical window border glyph: ")
    (prin1 (display-table-slot dt 'vertical-border))
    (princ "\nCharacter display glyph sequences:\n")
    (with-current-buffer standard-output
      (let ((vector (make-vector 256 nil))
	    (i 0))
	(while (< i 256)
	  (aset vector i (aref dt i))
	  (setq i (1+ i)))
	(describe-vector
	 vector 'display-table-print-array))
      (help-mode))))

(defun display-table-print-array (desc)
  (insert "[")
  (let ((column (current-column))
	(width (window-width))
	string)
    (dotimes (i (length desc))
      (setq string (format "%s" (aref desc i)))
      (cond
       ((>= (+ (current-column) (length string) 1)
	    width)
	(insert "\n")
	(insert (make-string column ? )))
       ((> i 0)
	(insert " ")))
      (insert string)))
  (insert "]\n"))

;;;###autoload
(defun describe-current-display-table ()
  "Describe the display table in use in the selected window and buffer."
  (interactive)
  (let ((disptab (or (window-display-table (selected-window))
		     buffer-display-table
		     standard-display-table)))
    (if disptab
	(describe-display-table disptab)
      (message "No display table"))))

;;;###autoload
(defun standard-display-8bit (l h)
  "Display characters representing raw bytes in the range L to H literally.

On a terminal display, each character in the range is displayed
by sending the corresponding byte directly to the terminal.

On a graphic display, each character in the range is displayed
using the default font by a glyph whose code is the corresponding
byte.

Note that ASCII printable characters (SPC to TILDA) are displayed
in the default way after this call."
  (or standard-display-table
      (setq standard-display-table (make-display-table)))
  (if (> h 255)
      (setq h 255))
  (while (<= l h)
    (if (< l 128)
	(aset standard-display-table l
	      (if (or (< l ?\s) (= l 127)) (vector l)))
      (let ((c (unibyte-char-to-multibyte l)))
	(aset standard-display-table c (vector c))))
    (setq l (1+ l))))

;;;###autoload
(defun standard-display-default (l h)
  "Display characters in the range L to H using the default notation."
  (or standard-display-table
      (setq standard-display-table (make-display-table)))
  (while (<= l h)
    (if (and (>= l ?\s) (characterp l))
	(aset standard-display-table l nil))
    (setq l (1+ l))))

;; This function does NOT take terminal-dependent escape sequences.
;; For that, you need to go through create-glyph.  Use one of the
;; other functions below, or roll your own.
;;;###autoload
(defun standard-display-ascii (c s)
  "Display character C using printable string S."
  (or standard-display-table
      (setq standard-display-table (make-display-table)))
  (aset standard-display-table c (vconcat s)))

;;;###autoload
(defun standard-display-g1 (c sc)
  "Display character C as character SC in the g1 character set.
This function assumes that your terminal uses the SO/SI characters;
it is meaningless for an X frame."
  (if (memq window-system '(x w32 ns))
      (error "Cannot use string glyphs in a windowing system"))
  (or standard-display-table
      (setq standard-display-table (make-display-table)))
  (aset standard-display-table c
	(vector (create-glyph (concat "\016" (char-to-string sc) "\017")))))

;;;###autoload
(defun standard-display-graphic (c gc)
  "Display character C as character GC in graphics character set.
This function assumes VT100-compatible escapes; it is meaningless for an
X frame."
  (if (memq window-system '(x w32 ns))
      (error "Cannot use string glyphs in a windowing system"))
  (or standard-display-table
      (setq standard-display-table (make-display-table)))
  (aset standard-display-table c
	(vector (create-glyph (concat "\e(0" (char-to-string gc) "\e(B")))))

;;;###autoload
(defun standard-display-underline (c uc)
  "Display character C as character UC plus underlining."
  (or standard-display-table
      (setq standard-display-table (make-display-table)))
  (aset standard-display-table c
	(vector
	 (if window-system
	     (make-glyph-code uc 'underline)
	   (create-glyph (concat "\e[4m" (char-to-string uc) "\e[m"))))))

;;;###autoload
(defun create-glyph (string)
  "Allocate a glyph code to display by sending STRING to the terminal."
  (if (= (length glyph-table) 65536)
      (error "No free glyph codes remain"))
  ;; Don't use slots that correspond to ASCII characters.
  (if (= (length glyph-table) 32)
      (setq glyph-table (vconcat glyph-table (make-vector 224 nil))))
  (setq glyph-table (vconcat glyph-table (list string)))
  (1- (length glyph-table)))

;;;###autoload
(defun make-glyph-code (char &optional face)
  "Return a glyph code representing char CHAR with face FACE."
  ;; Due to limitations on Emacs integer values, faces with
  ;; face id greater that 512 are silently ignored.
  (if (not face)
      char
    (let ((fid (face-id face)))
      (if (< fid 64) ; we have 32 - 3(LSB) - 1(SIGN) - 22(CHAR) = 6 bits for face id
	  (logior char (lsh fid 22))
	(cons char fid)))))

;;;###autoload
(defun glyph-char (glyph)
  "Return the character of glyph code GLYPH."
  (if (consp glyph)
      (car glyph)
    (logand glyph #x3fffff)))

;;;###autoload
(defun glyph-face (glyph)
  "Return the face of glyph code GLYPH, or nil if glyph has default face."
  (let ((face-id (if (consp glyph) (cdr glyph) (lsh glyph -22))))
    (and (> face-id 0)
	 (catch 'face
	   (dolist (face (face-list))
	     (when (eq (face-id face) face-id)
	       (throw 'face face)))))))

;;;###autoload
(defun standard-display-european (arg)
  "Semi-obsolete way to toggle display of ISO 8859 European characters.

This function is semi-obsolete; you probably don't need it, or else you
probably should use `set-language-environment' or `set-locale-environment'.

This function enables European character display if ARG is positive,
disables it if negative.  Otherwise, it toggles European character display.

When this mode is enabled, characters in the range of 160 to 255
display not as octal escapes, but as accented characters.  Codes 146
and 160 display as apostrophe and space, even though they are not the
ASCII codes for apostrophe and space.

Enabling European character display with this command noninteractively
from Lisp code also selects Latin-1 as the language environment.
This provides increased compatibility for users who call this function
in `.emacs'."

  (if (or (<= (prefix-numeric-value arg) 0)
	  (and (null arg)
	       (char-table-p standard-display-table)
	       ;; Test 161, because 160 displays as a space.
	       (equal (aref standard-display-table
			    (unibyte-char-to-multibyte 161))
		      (vector (unibyte-char-to-multibyte 161)))))
      (progn
	(standard-display-default
	 (unibyte-char-to-multibyte 160) (unibyte-char-to-multibyte 255))
	(unless (or (memq window-system '(x w32 ns)))
	  (and (terminal-coding-system)
	       (set-terminal-coding-system nil))))

    (display-warning 'i18n
		     "`standard-display-european' is semi-obsolete; see its doc string for details"
		     :warning)

    ;; Switch to Latin-1 language environment
    ;; unless some other has been specified.
    (if (equal current-language-environment "English")
	(set-language-environment "latin-1"))
    (unless (or noninteractive (memq window-system '(x w32 ns)))
      ;; Send those codes literally to a character-based terminal.
      ;; If we are using single-byte characters,
      ;; it doesn't matter which coding system we use.
      (set-terminal-coding-system
       (let ((c (intern (downcase current-language-environment))))
	 (if (coding-system-p c) c 'latin-1))))
    (standard-display-european-internal)))

(provide 'disp-table)

;;; disp-table.el ends here
