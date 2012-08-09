;;; canlock.el --- functions for Cancel-Lock feature

;; Copyright (C) 1998-1999, 2001-2012 Free Software Foundation, Inc.

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: news, cancel-lock, hmac, sha1, rfc2104

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

;; Canlock is a library for generating and verifying Cancel-Lock and/or
;; Cancel-Key header in news articles.  This is used to protect articles
;; from rogue cancel, supersede or replace attacks.  The method is based
;; on draft-ietf-usefor-cancel-lock-01.txt which was released on November
;; 3rd 1998.  For instance, you can add Cancel-Lock (and possibly Cancel-
;; Key) header in a news article by using a hook which will be evaluated
;; just before sending an article as follows:
;;
;; (add-hook '*e**a*e-header-hook 'canlock-insert-header t)
;;
;; Verifying Cancel-Lock is mainly a function of news servers, however,
;; you can verify your own article using the command `canlock-verify' in
;; the (raw) article buffer.  You will be prompted for the password for
;; each time if the option `canlock-password' or `canlock-password-for-
;; verify' is nil.  Note that setting these options is a bit unsafe.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'sha1)

(defvar mail-header-separator)

(defgroup canlock nil
  "The Cancel-Lock feature."
  :group 'news)

(defcustom canlock-password nil
  "Password to use when signing a Cancel-Lock or a Cancel-Key header."
  :type '(radio (const :format "Not specified " nil)
		(string :tag "Password"))
  :group 'canlock)

(defcustom canlock-password-for-verify canlock-password
  "Password to use when verifying a Cancel-Lock or a Cancel-Key header."
  :type '(radio (const :format "Not specified " nil)
		(string :tag "Password"))
  :group 'canlock)

(defcustom canlock-force-insert-header nil
  "If non-nil, insert a Cancel-Lock or a Cancel-Key header even if the
buffer does not look like a news message."
  :type 'boolean
  :group 'canlock)

(eval-when-compile
  (defmacro canlock-string-as-unibyte (string)
    "Return a unibyte string with the same individual bytes as STRING."
    (if (fboundp 'string-as-unibyte)
	(list 'string-as-unibyte string)
      string)))

(defun canlock-sha1 (message)
  "Make a SHA-1 digest of MESSAGE as a unibyte string of length 20 bytes."
  (let (sha1-maximum-internal-length)
    (sha1 message nil nil 'binary)))

(defun canlock-make-cancel-key (message-id password)
  "Make a Cancel-Key header."
  (when (> (length password) 20)
    (setq password (canlock-sha1 password)))
  (setq password (concat password (make-string (- 64 (length password)) 0)))
  (let ((ipad (mapconcat (lambda (byte)
			   (char-to-string (logxor 54 byte)))
			 password ""))
	(opad (mapconcat (lambda (byte)
			   (char-to-string (logxor 92 byte)))
			 password "")))
    (base64-encode-string
     (canlock-sha1
      (concat opad
	      (canlock-sha1
	       (concat ipad (canlock-string-as-unibyte message-id))))))))

(defun canlock-narrow-to-header ()
  "Narrow the buffer to the head of the message."
  (let (case-fold-search)
    (narrow-to-region
     (goto-char (point-min))
     (goto-char (if (re-search-forward
		     (format "^$\\|^%s$"
			     (regexp-quote mail-header-separator))
		     nil t)
		    (match-beginning 0)
		  (point-max))))))

(defun canlock-delete-headers ()
  "Delete Cancel-Key or Cancel-Lock headers in the narrowed buffer."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (while (re-search-forward "^Cancel-\\(Key\\|Lock\\):" nil t)
      (delete-region (match-beginning 0)
		     (if (re-search-forward "^[^\t ]" nil t)
			 (goto-char (match-beginning 0))
		       (point-max))))))

(defun canlock-fetch-fields (&optional key)
  "Return a list of the values of Cancel-Lock header.
If KEY is non-nil, look for a Cancel-Key header instead.  The buffer
is expected to be narrowed to just the headers of the message."
  (let ((field (mail-fetch-field (if key "Cancel-Key" "Cancel-Lock")))
	fields rest
	(case-fold-search t))
    (when field
      (setq fields (split-string field "[\t\n\r ,]+"))
      (while fields
	(when (string-match "^sha1:" (setq field (pop fields)))
	  (push (substring field 5) rest)))
      (nreverse rest))))

(defun canlock-fetch-id-for-key ()
  "Return a Message-ID in Cancel, Supersedes or Replaces header.
The buffer is expected to be narrowed to just the headers of the
message."
  (or (let ((cancel (mail-fetch-field "Control")))
	(and cancel
	     (string-match "^cancel[\t ]+\\(<[^\t\n @<>]+@[^\t\n @<>]+>\\)"
			   cancel)
	     (match-string 1 cancel)))
      (mail-fetch-field "Supersedes")
      (mail-fetch-field "Replaces")))

;;;###autoload
(defun canlock-insert-header (&optional id-for-key id-for-lock password)
  "Insert a Cancel-Key and/or a Cancel-Lock header if possible."
  (let (news control key-for-key key-for-lock)
    (save-excursion
      (save-restriction
	(canlock-narrow-to-header)
	(when (setq news (or canlock-force-insert-header
			     (mail-fetch-field "Newsgroups")))
	  (unless id-for-key
	    (setq id-for-key (canlock-fetch-id-for-key)))
	  (if (and (setq control (mail-fetch-field "Control"))
		   (string-match "^cancel[\t ]+<[^\t\n @<>]+@[^\t\n @<>]+>"
				 control))
	      (setq id-for-lock nil)
	    (unless id-for-lock
	      (setq id-for-lock (mail-fetch-field "Message-ID"))))
	  (canlock-delete-headers)
	  (goto-char (point-max))))
      (when news
	(if (not (or id-for-key id-for-lock))
	    (message "There are no Message-ID(s)")
	  (unless password
	    (setq password (or canlock-password
			       (read-passwd
				"Password for Canlock: "))))
	  (if (or (not (stringp password)) (zerop (length password)))
	      (message "Password for Canlock is bad")
	    (setq key-for-key (when id-for-key
				(canlock-make-cancel-key
				 id-for-key password))
		  key-for-lock (when id-for-lock
				 (canlock-make-cancel-key
				  id-for-lock password)))
	    (if (not (or key-for-key key-for-lock))
		(message "Couldn't insert Canlock header")
	      (when key-for-key
		(insert "Cancel-Key: sha1:" key-for-key "\n"))
	      (when key-for-lock
		(insert "Cancel-Lock: sha1:"
			(base64-encode-string (canlock-sha1 key-for-lock))
			"\n")))))))))

;;;###autoload
(defun canlock-verify (&optional buffer)
  "Verify Cancel-Lock or Cancel-Key in BUFFER.
If BUFFER is nil, the current buffer is assumed.  Signal an error if
it fails."
  (interactive)
  (let (keys locks errmsg id-for-key id-for-lock password
	     key-for-key key-for-lock match)
    (save-excursion
      (when buffer
	(set-buffer buffer))
      (save-restriction
	(widen)
	(canlock-narrow-to-header)
	(setq keys (canlock-fetch-fields 'key)
	      locks (canlock-fetch-fields))
	(if (not (or keys locks))
	    (setq errmsg
		  "There are neither Cancel-Lock nor Cancel-Key headers")
	  (setq id-for-key (canlock-fetch-id-for-key)
		id-for-lock (mail-fetch-field "Message-ID"))
	  (or id-for-key id-for-lock
	      (setq errmsg "There are no Message-ID(s)")))))
    (if errmsg
	(error "%s" errmsg)
      (setq password (or canlock-password-for-verify
			 (read-passwd "Password for Canlock: ")))
      (if (or (not (stringp password)) (zerop (length password)))
	  (error "Password for Canlock is bad")
	(when keys
	  (when id-for-key
	    (setq key-for-key (canlock-make-cancel-key id-for-key password))
	    (while (and keys (not match))
	      (setq match (string-equal key-for-key (pop keys)))))
	  (setq keys (if match "good" "bad")))
	(setq match nil)
	(when locks
	  (when id-for-lock
	    (setq key-for-lock
		  (base64-encode-string
		   (canlock-sha1 (canlock-make-cancel-key id-for-lock
							  password))))
	    (when (and locks (not match))
	      (setq match (string-equal key-for-lock (pop locks)))))
	  (setq locks (if match "good" "bad")))
	(prog1
	    (when (member "bad" (list keys locks))
	      "bad")
	  (cond ((and keys locks)
		 (message "Cancel-Key is %s, Cancel-Lock is %s" keys locks))
		(locks
		 (message "Cancel-Lock is %s" locks))
		(keys
		 (message "Cancel-Key is %s" keys))))))))

(provide 'canlock)

;;; canlock.el ends here
