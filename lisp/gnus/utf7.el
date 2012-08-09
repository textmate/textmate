;;; utf7.el --- UTF-7 encoding/decoding for Emacs   -*-coding: iso-8859-1;-*-

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: Jon K Hellan <hellan@acm.org>
;; Maintainer: bugs@gnus.org
;; Keywords: mail

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

;; UTF-7 - A Mail-Safe Transformation Format of Unicode - RFC 2152
;; This is a transformation format of Unicode that contains only 7-bit
;; ASCII octets and is intended to be readable by humans in the limiting
;; case that the document consists of characters from the US-ASCII
;; repertoire.
;; In short, runs of characters outside US-ASCII are encoded as base64
;; inside delimiters.
;; A variation of UTF-7 is specified in IMAP 4rev1 (RFC 2060) as the way
;; to represent characters outside US-ASCII in mailbox names in IMAP.
;; This library supports both variants, but the IMAP variation was the
;; reason I wrote it.
;; The routines convert UTF-7 -> UTF-16 (16 bit encoding of Unicode)
;; -> current character set, and vice versa.
;; However, until Emacs supports Unicode, the only Emacs character set
;; supported here is ISO-8859.1, which can trivially be converted to/from
;; Unicode.
;; When decoding results in a character outside the Emacs character set,
;; an error is thrown.  It is up to the application to recover.

;; UTF-7 should be done by providing a coding system.  Mule-UCS does
;; already, but I don't know if it does the IMAP version and it's not
;; clear whether that should really be a coding system.  The UTF-16
;; part of the conversion can be done with coding systems available
;; with Mule-UCS or some versions of Emacs.  Unfortunately these were
;; done wrongly (regarding handling of byte-order marks and how the
;; variants were named), so we don't have a consistent name for the
;; necessary coding system.  The code below doesn't seem to DTRT
;; generally.  E.g.:
;;
;; (utf7-encode "a+£")
;;   => "a+ACsAow-"
;;
;; $ echo "a+£"|iconv -f iso-8859-1 -t utf-7
;; a+-+AKM
;;
;;  -- fx


;;; Code:

(require 'base64)
(eval-when-compile (require 'cl))
(require 'mm-util)

(defconst utf7-direct-encoding-chars " -%'-*,-[]-}"
  "Character ranges which do not need escaping in UTF-7.")

(defconst utf7-imap-direct-encoding-chars
  (concat utf7-direct-encoding-chars "+\\~")
  "Character ranges which do not need escaping in the IMAP variant of UTF-7.")

(defconst utf7-utf-16-coding-system
  (cond ((mm-coding-system-p 'utf-16-be-no-signature) ; Mule-UCS
	 'utf-16-be-no-signature)
	((and (mm-coding-system-p 'utf-16-be) ; Emacs
	      ;; Avoid versions with BOM.
	      (= 2 (length (encode-coding-string "a" 'utf-16-be))))
	 'utf-16-be)
	((mm-coding-system-p 'utf-16-be-nosig) ; ?
	 'utf-16-be-nosig))
  "Coding system which encodes big endian UTF-16 without a BOM signature.")

(defsubst utf7-imap-get-pad-length (len modulus)
  "Return required length of padding for IMAP modified base64 fragment."
  (mod (- len) modulus))

(defun utf7-encode-internal (&optional for-imap)
  "Encode text in (temporary) buffer as UTF-7.
Use IMAP modification if FOR-IMAP is non-nil."
  (let ((start (point-min))
	(end (point-max)))
    (narrow-to-region start end)
    (goto-char start)
    (let* ((esc-char (if for-imap ?& ?+))
	   (direct-encoding-chars
	    (if for-imap utf7-imap-direct-encoding-chars
	      utf7-direct-encoding-chars))
	   (not-direct-encoding-chars (concat "^" direct-encoding-chars)))
      (while (not (eobp))
	(skip-chars-forward direct-encoding-chars)
	(unless (eobp)
	  (insert esc-char)
	  (let ((p (point))
		(fc (following-char))
		(run-length
		 (skip-chars-forward not-direct-encoding-chars)))
	    (if (and (= fc esc-char)
		     (= run-length 1))	; Lone esc-char?
		(delete-char -1)        ; Now there's one too many
	      (utf7-fragment-encode p (point) for-imap))
	    (insert "-")))))))

(defun utf7-fragment-encode (start end &optional for-imap)
  "Encode text from START to END in buffer as UTF-7 escape fragment.
Use IMAP modification if FOR-IMAP is non-nil."
  (save-restriction
    (narrow-to-region start end)
    (funcall (utf7-get-u16char-converter 'to-utf-16))
    (mm-with-unibyte-current-buffer
      (base64-encode-region start (point-max)))
    (goto-char start)
    (let ((pm (point-max)))
      (when for-imap
	(while (search-forward "/" nil t)
	  (replace-match ",")))
      (skip-chars-forward "^= \t\n" pm)
      (delete-region (point) pm))))

(defun utf7-decode-internal (&optional for-imap)
  "Decode UTF-7 text in (temporary) buffer.
Use IMAP modification if FOR-IMAP is non-nil."
  (let ((start (point-min))
	(end (point-max)))
    (goto-char start)
    (let* ((esc-pattern (concat "^" (char-to-string (if for-imap ?& ?+))))
	   (base64-chars (concat "A-Za-z0-9+"
				 (char-to-string (if for-imap ?, ?/)))))
      (while (not (eobp))
	(skip-chars-forward esc-pattern)
	(unless (eobp)
	  (forward-char)
	  (let ((p (point))
		(run-length (skip-chars-forward base64-chars)))
	    (when (and (not (eobp)) (= (following-char) ?-))
	      (delete-char 1))
	    (unless (= run-length 0)	; Encoded lone esc-char?
	      (save-excursion
		(utf7-fragment-decode p (point) for-imap)
		(goto-char p)
		(delete-char -1)))))))))

(defun utf7-fragment-decode (start end &optional for-imap)
  "Decode base64 encoded fragment from START to END of UTF-7 text in buffer.
Use IMAP modification if FOR-IMAP is non-nil."
  (save-restriction
    (narrow-to-region start end)
    (when for-imap
      (goto-char start)
      (while (search-forward "," nil 'move-to-end) (replace-match "/")))
    (let ((pl (utf7-imap-get-pad-length (- end start) 4)))
      (insert (make-string pl ?=))
      (base64-decode-region start (+ end pl)))
    (funcall (utf7-get-u16char-converter 'from-utf-16))))

(defun utf7-get-u16char-converter (which-way)
  "Return a function to convert between UTF-16 and current character set."
  (if utf7-utf-16-coding-system
      (if (eq which-way 'to-utf-16)
	  (lambda ()
	    (encode-coding-region (point-min) (point-max)
				  utf7-utf-16-coding-system))
	(lambda ()
	  (decode-coding-region (point-min) (point-max)
				utf7-utf-16-coding-system)))
    ;; Add test to check if we are really Latin-1.
    (if (eq which-way 'to-utf-16)
	'utf7-latin1-u16-char-converter
      'utf7-u16-latin1-char-converter)))

(defun utf7-latin1-u16-char-converter ()
  "Convert latin 1 (ISO-8859.1) characters to 16 bit Unicode.
Characters are converted to raw byte pairs in narrowed buffer."
  (mm-encode-coding-region (point-min) (point-max) 'iso-8859-1)
  (mm-disable-multibyte)
  (goto-char (point-min))
  (while (not (eobp))
    (insert 0)
    (forward-char)))

(defun utf7-u16-latin1-char-converter ()
  "Convert 16 bit Unicode characters to latin 1 (ISO-8859.1).
Characters are in raw byte pairs in narrowed buffer."
  (goto-char (point-min))
  (while (not (eobp))
    (if (= 0 (following-char))
	(delete-char 1)
	(error "Unable to convert from Unicode"))
    (forward-char))
  (mm-decode-coding-region (point-min) (point-max) 'iso-8859-1)
  (mm-enable-multibyte))

;;;###autoload
(defun utf7-encode (string &optional for-imap)
  "Encode UTF-7 STRING.  Use IMAP modification if FOR-IMAP is non-nil."
  (if (and (coding-system-p 'utf-7) (coding-system-p 'utf-7-imap))
      ;; Emacs 23 with proper support for IMAP
      (encode-coding-string string (if for-imap 'utf-7-imap 'utf-7))
    (mm-with-multibyte-buffer
     (insert string)
     (utf7-encode-internal for-imap)
     (buffer-string))))

(defun utf7-decode (string &optional for-imap)
  "Decode UTF-7 STRING.  Use IMAP modification if FOR-IMAP is non-nil."
  (if (and (coding-system-p 'utf-7) (coding-system-p 'utf-7-imap))
      ;; Emacs 23 with proper support for IMAP
      (decode-coding-string string (if for-imap 'utf-7-imap 'utf-7))
    (mm-with-unibyte-buffer
     (insert string)
     (utf7-decode-internal for-imap)
     (mm-enable-multibyte)
     (buffer-string))))

(provide 'utf7)

;;; utf7.el ends here
