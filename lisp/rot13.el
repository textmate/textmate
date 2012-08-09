;;; rot13.el --- display a buffer in ROT13

;; Copyright (C) 1988, 2001-2012 Free Software Foundation, Inc.

;; Author: Howard Gayle
;; Maintainer: FSF

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

;; The entry point, `rot13-other-window', performs a Caesar cipher
;; encrypt/decrypt on the current buffer and displays the result in another
;; window.  ROT13 encryption is sometimes used on USENET as a read-at-your-
;; own-risk wrapper for material some might consider offensive, such as
;; ethnic humor.
;;
;; Written by Howard Gayle.
;; This hack is mainly to show off the char table stuff.
;;
;; New entry points, `rot13', `rot13-string', and `rot13-region' that
;; performs Caesar cipher encrypt/decrypt on buffers and strings, was
;; added by Simon Josefsson.

;;; Code:

(defvar rot13-display-table
  (let ((table (make-display-table))
	(i 0))
    (while (< i 26)
      (aset table (+ i ?a) (vector (+ (% (+ i 13) 26) ?a)))
      (aset table (+ i ?A) (vector (+ (% (+ i 13) 26) ?A)))
      (setq i (1+ i)))
    table)
  "Char table for ROT13 display.")

(defvar rot13-translate-table
  (let ((str (make-string 127 0))
	(i 0))
    (while (< i 127)
      (aset str i i)
      (setq i (1+ i)))
    (setq i 0)
    (while (< i 26)
      (aset str (+ i ?a) (+ (% (+ i 13) 26) ?a))
      (aset str (+ i ?A) (+ (% (+ i 13) 26) ?A))
      (setq i (1+ i)))
    str)
  "String table for ROT13 translation.")

;;;###autoload
(defun rot13 (object &optional start end)
  "Return ROT13 encryption of OBJECT, a buffer or string."
  (if (bufferp object)
      (with-current-buffer object
	(rot13-region start end))
    (rot13-string object)))

;;;###autoload
(defun rot13-string (string)
  "Return ROT13 encryption of STRING."
  (with-temp-buffer
    (insert string)
    (rot13-region (point-min) (point-max))
    (buffer-string)))

;;;###autoload
(defun rot13-region (start end)
  "ROT13 encrypt the region between START and END in current buffer."
  (interactive "r")
  (translate-region start end rot13-translate-table))

;;;###autoload
(defun rot13-other-window ()
  "Display current buffer in ROT13 in another window.
The text itself is not modified, only the way it is displayed is affected.

To terminate the ROT13 display, delete that window.  As long as that window
is not deleted, any buffer displayed in it will become instantly encoded
in ROT13.

See also `toggle-rot13-mode'."
  (interactive)
  (let ((w (display-buffer (current-buffer) t)))
    (set-window-display-table w rot13-display-table)))

;;;###autoload
(defun toggle-rot13-mode ()
  "Toggle the use of ROT13 encoding for the current window."
  (interactive)
  (if (eq (window-display-table (selected-window)) rot13-display-table)
      (set-window-display-table (selected-window) nil)
    (if (null (window-display-table (selected-window)))
	(set-window-display-table (selected-window) rot13-display-table))))

(provide 'rot13)

;;; rot13.el ends here
