;;; tramp-uu.el --- uuencode in Lisp

;; Copyright (C) 2002-2012 Free Software Foundation, Inc.

;; Author: Kai Großjohann <kai.grossjohann@gmx.net>
;; Keywords: comm, terminals
;; Package: tramp

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

;; An implementation of "uuencode" in Lisp.  Uses the function
;; base64-encode-region which is built-in to modern Emacsen.

;;; Code:

(defvar tramp-uu-b64-alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "Mapping from base64-encoded character to the byte it represents.")

(defvar tramp-uu-b64-char-to-byte
  (let ((i 0))
    (mapcar (lambda (c)
	      (prog1
		  (cons c i)
		(setq i (1+ i))))
	    tramp-uu-b64-alphabet))
  "Alist of mapping from base64 character to its byte.")

(defun tramp-uu-byte-to-uu-char (byte)
  "Return the character encoding BYTE."
  (if (zerop byte) ?` (+ byte 32)))

(defun tramp-uu-b64-char-to-byte (char)
  "Return the byte that is encoded as CHAR."
  (cdr (assq char tramp-uu-b64-char-to-byte)))

;;;###tramp-autoload
(defun tramp-uuencode-region (beg end)
  "UU-encode the region between BEG and END."
  ;; First we base64 encode the region, then we transmogrify that into
  ;; uu encoding.
  (let ((len (base64-encode-region beg end t))
	(padding 0)
	i c)
    (save-excursion
      (goto-char beg)
      (setq i 0)
      (while (< i len)
	(setq c (char-after (point)))
	(delete-char 1)
	(if (equal c ?=)
	    ;; "=" means padding.  Insert "`" instead.  Not counted for length.
	    (progn (insert "`") (setq len (1- len)))
	  (insert (tramp-uu-byte-to-uu-char (tramp-uu-b64-char-to-byte c)))
	  (setq i (1+ i)))
	;; Every 60 characters, add "M" at beginning of line (as
	;; length byte) and insert a newline.
	(when (zerop (% i 60))
	  (save-excursion
	    (beginning-of-line)
	    (insert (char-to-string (+ 32 (/ (* 3 60) 4)))))
	  (insert "\n")))
      ;; If there is something leftover, we compute the length byte
      ;; for that stuff and insert it and a trailing newline.
      (unless (zerop (% i 60))
	(save-excursion
	  (beginning-of-line)
	  (insert (char-to-string (+ 32 (% (- end beg) 45)))))
	(insert "\n"))
      ;; Why is there always a "`" line at the end?
      (insert "`\nend\n")
      (goto-char beg)
      (insert "begin 600 xxx\n"))))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-uu 'force)))

(provide 'tramp-uu)

;;; tramp-uu.el ends here

;; Local Variables:
;; mode: Emacs-Lisp
;; coding: utf-8
;; End:
