;;; utf-7.el --- utf-7 coding system

;; Copyright (C) 2003-2012  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: i18n, mail

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

;; Defines a coding system for UTF-7, defined in RFC 2152.  Non-ASCII
;; segments are encoded as base64-encoded big endian UTF-16.  Also
;; defines a variation required for IMAP (RFC 2060).

;; The encoding and decoding was originally taken from Jon K Hellan's
;; implementation in Gnus, but has been substantially re-done.

;; This probably needs more attention.  In particular, it's not
;; completely consistent with iconv's behavior.  It's arguable
;; whether the IMAP version should be a coding system since it's
;; apparently only used for IMAP mailbox names, so it's commented out.

;;; Code:

(defun utf-7-decode (len imap)
  "Decode LEN bytes of UTF-7 at point.
IMAP non-nil means use the IMAP version."
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (+ (point) len))
      (let ((not-esc (if imap "^&" "^+"))
	    (skip-chars (if imap "A-Za-z0-9+," "A-Za-z0-9+/")))
	(while (not (eobp))
	  (skip-chars-forward not-esc)
	  (unless (eobp)
	    (forward-char)
	    (let ((p (point))
		  (run-length (skip-chars-forward skip-chars)))
	      (if (eq ?- (char-after))
		  (delete-char 1))
	      (unless (= run-length 0)	; encoded lone esc-char
		(let ((pl (mod (- run-length) 4)))
		  (insert-char ?= pl)
		  (if imap
		      (subst-char-in-region p (point) ?, ?/))
		  (base64-decode-region p (point)))
		(decode-coding-region p (point) 'utf-16be)
		(save-excursion
		  (goto-char p)
		  (delete-char -1)))))))
      (- (point-max) (point-min)))))

;;;###autoload
(defun utf-7-post-read-conversion (len)
  (utf-7-decode len nil))

;;;###autoload
(defun utf-7-imap-post-read-conversion (len)
  (utf-7-decode len t))

(defun utf-7-encode (from to imap)
  "Encode bytes between FROM and TO to UTF-7.
ESC and SKIP-CHARS are adjusted for the normal and IMAP versions."
  (let* ((old-buf (current-buffer))
	 (esc (if imap ?& ?+))
	 ;; These are characters which can be encoded asis.
	 (skip-chars (if imap
			 "\t\n\r\x20-\x25\x27-\x7e" ; rfc2060 
		       ;; This includes the rfc2152 optional set.
		       ;; Perhaps it shouldn't (like iconv).
		       "\t\n\r -*,-[]-}"))
	 (not-skip-chars (format "^%s%c" skip-chars esc)))
    (set-buffer (generate-new-buffer " *temp*"))
    (if (stringp from)
	(insert from)
      (insert-buffer-substring old-buf from to))
    (goto-char (point-min))
    (while (not (eobp))
      (skip-chars-forward skip-chars)
      (if (eq esc (char-after))
	  (progn (forward-char)
		 (insert ?-))
	(unless (eobp)
	  (insert esc)
	  (let ((p (point)))
	    (skip-chars-forward not-skip-chars)
	    (save-restriction
	      ;; encode-coding-region doesn't preserve point
	      (narrow-to-region p (point))
	      (encode-coding-region p (point-max) 'utf-16be)
	      (base64-encode-region p (point-max))
	      (if imap
		  (subst-char-in-region p (point-max) ?/ ?,))
	      (goto-char p)
	      ;; As I read the RFC, this isn't correct, but it's
	      ;; consistent with iconv, at least regarding `='.
	      (skip-chars-forward "^= \t\n")
	      (delete-region (point) (point-max))))
          ;; RFC2060 stipulates that all names MUST end in US-ASCII (i.e.
          ;; a name that ends with a Unicode octet MUST end with a "-").
	  (if (or imap (not (eobp)))
	    (insert ?-)))))
    nil))

;;;###autoload
(defun utf-7-pre-write-conversion (from to)
  (utf-7-encode from to nil))

;;;###autoload
(defun utf-7-imap-pre-write-conversion (from to)
  (utf-7-encode from to t))

(provide 'utf-7)

;;; utf-7.el ends here
