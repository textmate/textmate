;;; mm-bodies.el --- Functions for decoding MIME things

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	MORIOKA Tomohiko <morioka@jaist.ac.jp>
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

;; For Emacs <22.2 and XEmacs.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(require 'mm-util)
(require 'rfc2047)
(require 'mm-encode)

(defvar mm-uu-yenc-decode-function)
(defvar mm-uu-decode-function)
(defvar mm-uu-binhex-decode-function)

;; 8bit treatment gets any char except: 0x32 - 0x7f, LF, TAB, BEL,
;; BS, vertical TAB, form feed, and ^_
;;
;; Note that CR is *not* included, as that would allow a non-paired CR
;; in the body contrary to RFC 2822:
;;
;;   - CR and LF MUST only occur together as CRLF; they MUST NOT
;;     appear independently in the body.

(defvar mm-7bit-chars "\x20-\x7f\n\t\x7\x8\xb\xc\x1f")

(defcustom mm-body-charset-encoding-alist
  '((iso-2022-jp . 7bit)
    (iso-2022-jp-2 . 7bit)
    ;; We MUST encode UTF-16 because it can contain \0's which is
    ;; known to break servers.
    ;; Note: UTF-16 variants are invalid for text parts [RFC 2781],
    ;; so this can't happen :-/.
    ;; PPS: Yes, it can happen if the user specifies UTF-16 in the MML
    ;; markup. - jh.
    (utf-16 . base64)
    (utf-16be . base64)
    (utf-16le . base64))
  "Alist of MIME charsets to encodings.
Valid encodings are `7bit', `8bit', `quoted-printable' and `base64'."
  :type '(repeat (cons (symbol :tag "charset")
		       (choice :tag "encoding"
			       (const 7bit)
			       (const 8bit)
			       (const quoted-printable)
			       (const base64))))
  :group 'mime)

(autoload 'message-options-get "message")
(declare-function message-options-set "message" (symbol value))

(defun mm-encode-body (&optional charset)
  "Encode a body.
Should be called narrowed to the body that is to be encoded.
If there is more than one non-ASCII MULE charset in the body, then the
list of MULE charsets found is returned.
If CHARSET is non-nil, it is used as the MIME charset to encode the body.
If successful, the MIME charset is returned.
If no encoding was done, nil is returned."
  (if (not (mm-multibyte-p))
      ;; In the non-Mule case, we search for non-ASCII chars and
      ;; return the value of `mail-parse-charset' if any are found.
      (or charset
	  (save-excursion
	    (goto-char (point-min))
	    (if (re-search-forward "[^\x0-\x7f]" nil t)
		(or mail-parse-charset
		    (message-options-get 'mm-body-charset-encoding-alist)
		    (message-options-set
		     'mm-body-charset-encoding-alist
		     (mm-read-coding-system "Charset used in the article: ")))
	      ;; The logic in `mml-generate-mime-1' confirms that it's OK
	      ;; to return nil here.
	      nil)))
    (save-excursion
      (if charset
	  (progn
	    (mm-encode-coding-region (point-min) (point-max)
				     (mm-charset-to-coding-system charset))
	    charset)
	(goto-char (point-min))
	(let ((charsets (mm-find-mime-charset-region (point-min) (point-max)
						     mm-hack-charsets)))
	  (cond
	   ;; No encoding.
	   ((null charsets)
	    nil)
	   ;; Too many charsets.
	   ((> (length charsets) 1)
	    charsets)
	   ;; We encode.
	   (t
	    (prog1
		(setq charset (car charsets))
	      (mm-encode-coding-region (point-min) (point-max)
				       (mm-charset-to-coding-system charset))))
	   ))))))

(defun mm-long-lines-p (length)
  "Say whether any of the lines in the buffer is longer than LENGTH."
  (save-excursion
    (goto-char (point-min))
    (end-of-line)
    (while (and (not (eobp))
		(not (> (current-column) length)))
      (forward-line 1)
      (end-of-line))
    (and (> (current-column) length)
	 (current-column))))

(defvar message-posting-charset)

(defun mm-body-encoding (charset &optional encoding)
  "Do Content-Transfer-Encoding and return the encoding of the current buffer."
  (when (stringp encoding)
    (setq encoding (intern (downcase encoding))))
  (let ((bits (mm-body-7-or-8))
	(longp (mm-long-lines-p 1000)))
    (require 'message)
    (cond
     ((and (not longp)
	   (not (and mm-use-ultra-safe-encoding
		     (or (save-excursion (re-search-forward " $" nil t))
			 (save-excursion (re-search-forward "^From " nil t)))))
	   (eq bits '7bit))
      bits)
     ((and (not mm-use-ultra-safe-encoding)
	   (not longp)
	   (not (cdr (assq charset mm-body-charset-encoding-alist)))
	   (or (eq t (cdr message-posting-charset))
	       (memq charset (cdr message-posting-charset))
	       (eq charset mail-parse-charset)))
      bits)
     (t
      (let ((encoding (or encoding
			  (cdr (assq charset mm-body-charset-encoding-alist))
			  (mm-qp-or-base64))))
	(when mm-use-ultra-safe-encoding
	  (setq encoding (mm-safer-encoding encoding)))
	(mm-encode-content-transfer-encoding encoding "text/plain")
	encoding)))))

(defun mm-body-7-or-8 ()
  "Say whether the body is 7bit or 8bit."
  (if (save-excursion
	(goto-char (point-min))
	(skip-chars-forward mm-7bit-chars)
	(eobp))
      '7bit
    '8bit))

;;;
;;; Functions for decoding
;;;

(defun mm-decode-content-transfer-encoding (encoding &optional type)
  "Decodes buffer encoded with ENCODING, returning success status.
If TYPE is `text/plain' CRLF->LF translation may occur."
  (prog1
      (condition-case error
	  (cond
	   ((eq encoding 'quoted-printable)
	    (quoted-printable-decode-region (point-min) (point-max))
	    t)
	   ((eq encoding 'base64)
	    (base64-decode-region
	     (point-min)
	     ;; Some mailers insert whitespace
	     ;; junk at the end which
	     ;; base64-decode-region dislikes.
	     ;; Also remove possible junk which could
	     ;; have been added by mailing list software.
	     (save-excursion
	       (goto-char (point-min))
	       (while (re-search-forward "^[\t ]*\r?\n" nil t)
		 (delete-region (match-beginning 0) (match-end 0)))
	       (goto-char (point-max))
	       (when (re-search-backward "^[\t ]*[A-Za-z0-9+/]+=*[\t ]*$"
					 nil t)
		 (forward-line))
	       (point))))
	   ((memq encoding '(nil 7bit 8bit binary))
	    ;; Do nothing.
	    t)
	   ((memq encoding '(x-uuencode x-uue))
	    (require 'mm-uu)
	    (funcall mm-uu-decode-function (point-min) (point-max))
	    t)
	   ((eq encoding 'x-binhex)
	    (require 'mm-uu)
	    (funcall mm-uu-binhex-decode-function (point-min) (point-max))
	    t)
	   ((eq encoding 'x-yenc)
	    (require 'mm-uu)
	    (funcall mm-uu-yenc-decode-function (point-min) (point-max))
	    )
	   ((functionp encoding)
	    (funcall encoding (point-min) (point-max))
	    t)
	   (t
	    (message "Unknown encoding %s; defaulting to 8bit" encoding)))
	(error
	 (message "Error while decoding: %s" error)
	 nil))
    (when (and
	   type
	   (memq encoding '(base64 x-uuencode x-uue x-binhex x-yenc))
	   (string-match "\\`text/" type))
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t)
	(replace-match "\n" t t)))))

(defun mm-decode-body (charset &optional encoding type)
  "Decode the current article that has been encoded with ENCODING to CHARSET.
ENCODING is a MIME content transfer encoding.
CHARSET is the MIME charset with which to decode the data after transfer
decoding.  If it is nil, default to `mail-parse-charset'."
  (when (stringp charset)
    (setq charset (intern (downcase charset))))
  (when (or (not charset)
	    (eq 'gnus-all mail-parse-ignored-charsets)
	    (memq 'gnus-all mail-parse-ignored-charsets)
	    (memq charset mail-parse-ignored-charsets))
    (setq charset mail-parse-charset))
  (save-excursion
    (when encoding
      (mm-decode-content-transfer-encoding encoding type))
    (when (and (featurep 'mule) ;; Fixme: Wrong test for unibyte session.
	       (not (eq charset 'gnus-decoded)))
      (let ((coding-system (mm-charset-to-coding-system
			    ;; Allow overwrite using
			    ;; `mm-charset-override-alist'.
			    charset nil t)))
	(if (and (not coding-system)
		 (listp mail-parse-ignored-charsets)
		 (memq 'gnus-unknown mail-parse-ignored-charsets))
	    (setq coding-system
		  (mm-charset-to-coding-system mail-parse-charset)))
	(when (and charset coding-system
		   ;; buffer-file-coding-system
		   ;;Article buffer is nil coding system
		   ;;in XEmacs
		   (mm-multibyte-p)
		   (or (not (eq coding-system 'ascii))
		       (setq coding-system mail-parse-charset)))
	  (mm-decode-coding-region (point-min) (point-max)
				   coding-system))
	(setq buffer-file-coding-system
	      (if (boundp 'last-coding-system-used)
		  (symbol-value 'last-coding-system-used)
		coding-system))))))

(defun mm-decode-string (string charset)
  "Decode STRING with CHARSET."
  (when (stringp charset)
    (setq charset (intern (downcase charset))))
  (when (or (not charset)
	    (eq 'gnus-all mail-parse-ignored-charsets)
	    (memq 'gnus-all mail-parse-ignored-charsets)
	    (memq charset mail-parse-ignored-charsets))
    (setq charset mail-parse-charset))
  (or
   (when (featurep 'mule)
     (let ((coding-system (mm-charset-to-coding-system
			   charset
			   ;; Allow overwrite using
			   ;; `mm-charset-override-alist'.
			   nil t)))
       (if (and (not coding-system)
		(listp mail-parse-ignored-charsets)
		(memq 'gnus-unknown mail-parse-ignored-charsets))
	   (setq coding-system
		 (mm-charset-to-coding-system mail-parse-charset)))
       (when (and charset coding-system
		  (mm-multibyte-p)
		  (or (not (eq coding-system 'ascii))
		      (setq coding-system mail-parse-charset)))
	 (mm-decode-coding-string string coding-system))))
   string))

(provide 'mm-bodies)

;;; mm-bodies.el ends here
