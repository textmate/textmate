;;; mailclient.el --- mail sending via system's mail client.

;; Copyright (C) 2005-2012 Free Software Foundation, Inc.

;; Author: David Reitter <david.reitter@gmail.com>
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

;; This package allows to hand over a buffer to be sent off
;; via the system's designated e-mail client.
;; Note that the e-mail client will display the contents of the buffer
;; again for editing.
;; The e-mail client is taken to be whoever handles a mailto: URL
;; via `browse-url'.
;; Mailto: URLs are composed according to RFC2368.

;; MIME bodies are not supported - we rather expect the mail client
;; to encode the body and add, for example, a digital signature.
;; The mailto URL RFC calls for "short text messages that are
;; actually the content of automatic processing."
;; So mailclient.el is ideal for situations where an e-mail is
;; generated automatically, and the user can edit it in the
;; mail client (e.g. bug-reports).

;; To activate:
;; (setq send-mail-function 'mailclient-send-it) ; if you use `mail'

;;; Code:


(require 'sendmail)   ;; for mail-sendmail-undelimit-header
(require 'mail-utils) ;; for mail-fetch-field
(require 'browse-url)

(defcustom mailclient-place-body-on-clipboard-flag
  (fboundp 'w32-set-clipboard-data)
  "If non-nil, put the e-mail body on the clipboard in mailclient.
This is useful on systems where only short mailto:// URLs are
supported.  Defaults to non-nil on Windows, nil otherwise."
  :type 'boolean
  :group 'mail)

(defun mailclient-encode-string-as-url (string)
  "Convert STRING to a URL, using utf-8 as encoding."
  (apply (function concat)
	 (mapcar
	  (lambda (char)
	    (cond
	     ((eq char ?\x20) "%20")   ;; space
	     ((eq char ?\n) "%0D%0A")  ;; newline
	     ((string-match "[-a-zA-Z0-9_:/.@]" (char-to-string char))
	      (char-to-string char))   ;; printable
	     (t                        ;; everything else
	      (format "%%%02x" char))))	;; escape
	  ;; Convert string to list of chars
	  (append (encode-coding-string string 'utf-8)))))

(defvar mailclient-delim-static "?")
(defun mailclient-url-delim ()
  (let ((current mailclient-delim-static))
    (setq mailclient-delim-static "&")
    current))

(defun mailclient-gather-addresses (str &optional drop-first-name)
  (let ((field (mail-fetch-field str nil t)))
    (if field
	(save-excursion
	  (let ((first t)
		(result ""))
	    (mapc
	     (lambda (recp)
	       (setq result
		     (concat
		      result
		      (if (and drop-first-name
			       first)
			  ""
			(concat (mailclient-url-delim) str "="))
		      (mailclient-encode-string-as-url
		       recp)))
	       (setq first nil))
	     (split-string
	      (mail-strip-quoted-names field) "\, *"))
	    result)))))

(declare-function clipboard-kill-ring-save "menu-bar.el" (beg end))

;;;###autoload
(defun mailclient-send-it ()
  "Pass current buffer on to the system's mail client.
Suitable value for `send-mail-function'.
The mail client is taken to be the handler of mailto URLs."
  (require 'mail-utils)
  (let ((case-fold-search nil)
	delimline
	(mailbuf (current-buffer)))
    (unwind-protect
	(with-temp-buffer
	  (insert-buffer-substring mailbuf)
	  ;; Move to header delimiter
	  (mail-sendmail-undelimit-header)
	  (setq delimline (point-marker))
	  (if mail-aliases
	      (expand-mail-aliases (point-min) delimline))
	  (goto-char (point-min))
	  ;; ignore any blank lines in the header
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  (let ((case-fold-search t)
		;; Use the external browser function to send the
		;; message.
		(browse-url-mailto-function nil))
	    ;; initialize limiter
	    (setq mailclient-delim-static "?")
	    ;; construct and call up mailto URL
	    (browse-url
	     (concat
	      (save-excursion
		(narrow-to-region (point-min) delimline)
		(concat
		 "mailto:"
		 ;; some of the headers according to RFC822
		 (mailclient-gather-addresses "To"
					      'drop-first-name)
		 (mailclient-gather-addresses "cc"  )
		 (mailclient-gather-addresses "bcc"  )
		 (mailclient-gather-addresses "Resent-To"  )
		 (mailclient-gather-addresses "Resent-cc"  )
		 (mailclient-gather-addresses "Resent-bcc"  )
		 (mailclient-gather-addresses "Reply-To"  )
		 ;; The From field is not honored for now: it's
		 ;; not necessarily configured. The mail client
		 ;; knows the user's address(es)
		 ;; (mailclient-gather-addresses "From"  )
		 ;; subject line
		 (let ((subj (mail-fetch-field "Subject" nil t)))
		   (widen) ;; so we can read the body later on
		   (if subj ;; if non-blank
		       ;; the mail client will deal with
		       ;; warning the user etc.
		       (concat (mailclient-url-delim) "subject="
			       (mailclient-encode-string-as-url subj))
		     ""))))
	      ;; body
	      (concat
	       (mailclient-url-delim) "body="
	       (mailclient-encode-string-as-url
		(if mailclient-place-body-on-clipboard-flag
		    (progn
		      (clipboard-kill-ring-save
		       (+ 1 delimline) (point-max))
		      (concat
		       "*** E-Mail body has been placed on clipboard, "
		       "please paste it here! ***"))
		  ;; else
		  (buffer-substring (+ 1 delimline) (point-max))))))))))))

(provide 'mailclient)

;;; mailclient.el ends here
