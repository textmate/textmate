;;; epa-mail.el --- the EasyPG Assistant, minor-mode for mail composer -*- lexical-binding: t -*-
;; Copyright (C) 2006-2012 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: PGP, GnuPG, mail, message
;; Package: epa

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

;;; Code:

(require 'epa)
(require 'mail-utils)

(defvar epa-mail-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-ed" 'epa-mail-decrypt)
    (define-key keymap "\C-c\C-ev" 'epa-mail-verify)
    (define-key keymap "\C-c\C-es" 'epa-mail-sign)
    (define-key keymap "\C-c\C-ee" 'epa-mail-encrypt)
    (define-key keymap "\C-c\C-ei" 'epa-mail-import-keys)
    (define-key keymap "\C-c\C-eo" 'epa-insert-keys)
    (define-key keymap "\C-c\C-e\C-d" 'epa-mail-decrypt)
    (define-key keymap "\C-c\C-e\C-v" 'epa-mail-verify)
    (define-key keymap "\C-c\C-e\C-s" 'epa-mail-sign)
    (define-key keymap "\C-c\C-e\C-e" 'epa-mail-encrypt)
    (define-key keymap "\C-c\C-e\C-i" 'epa-mail-import-keys)
    (define-key keymap "\C-c\C-e\C-o" 'epa-insert-keys)
    keymap))

(defvar epa-mail-mode-hook nil)
(defvar epa-mail-mode-on-hook nil)
(defvar epa-mail-mode-off-hook nil)

;;;###autoload
(define-minor-mode epa-mail-mode
  "A minor-mode for composing encrypted/clearsigned mails.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  nil " epa-mail" epa-mail-mode-map)

(defun epa-mail--find-usable-key (keys usage)
  "Find a usable key from KEYS for USAGE.
USAGE would be `sign' or `encrypt'."
  (catch 'found
    (while keys
      (let ((pointer (epg-key-sub-key-list (car keys))))
	(while pointer
	  (if (and (memq usage (epg-sub-key-capability (car pointer)))
		   (not (memq (epg-sub-key-validity (car pointer))
			      '(revoked expired))))
	      (throw 'found (car keys)))
	  (setq pointer (cdr pointer))))
      (setq keys (cdr keys)))))

;;;###autoload
(defun epa-mail-decrypt ()
  "Decrypt OpenPGP armors in the current buffer.
The buffer is expected to contain a mail message.

Don't use this command in Lisp programs!"
  (interactive)
  (epa-decrypt-armor-in-region (point-min) (point-max)))

;;;###autoload
(defun epa-mail-verify ()
  "Verify OpenPGP cleartext signed messages in the current buffer.
The buffer is expected to contain a mail message.

Don't use this command in Lisp programs!"
  (interactive)
  (epa-verify-cleartext-in-region (point-min) (point-max)))

;;;###autoload
(defun epa-mail-sign (start end signers mode)
  "Sign the current buffer.
The buffer is expected to contain a mail message.

Don't use this command in Lisp programs!"
  (interactive
   (save-excursion
     (goto-char (point-min))
     (if (search-forward mail-header-separator nil t)
	 (forward-line))
     (setq epa-last-coding-system-specified
	   (or coding-system-for-write
	       (epa--select-safe-coding-system (point) (point-max))))
     (let ((verbose current-prefix-arg))
       (list (point) (point-max)
	     (if verbose
		 (epa-select-keys (epg-make-context epa-protocol)
				  "Select keys for signing.
If no one is selected, default secret key is used.  "
				  nil t))
	     (if verbose
		 (epa--read-signature-type)
	       'clear)))))
  (epa-sign-region start end signers mode))

;;;###autoload
(defun epa-mail-encrypt (start end recipients sign signers)
  "Encrypt the current buffer.
The buffer is expected to contain a mail message.

Don't use this command in Lisp programs!"
  (interactive
   (save-excursion
     (let ((verbose current-prefix-arg)
	   (config (epg-configuration))
	   (context (epg-make-context epa-protocol))
	   recipients-string recipients recipient-key sign)
       (goto-char (point-min))
       (save-restriction
	 (narrow-to-region (point)
			   (if (search-forward mail-header-separator nil 0)
			       (match-beginning 0)
			     (point)))
	 (setq recipients-string
	       (mapconcat #'identity
			  (nconc (mail-fetch-field "to" nil nil t)
				 (mail-fetch-field "cc" nil nil t)
				 (mail-fetch-field "bcc" nil nil t))
			  ","))
	 (setq recipients
	       (mail-strip-quoted-names
		(with-temp-buffer
		  (insert "to: " recipients-string "\n")
		  (expand-mail-aliases (point-min) (point-max))
		  (car (mail-fetch-field "to" nil nil t))))))
       (if recipients
	   (setq recipients (delete ""
				    (split-string recipients
						  "[ \t\n]*,[ \t\n]*"))))

       ;; Process all the recipients thru the list of GnuPG groups.
       ;; Expand GnuPG group names to what they stand for.
       (setq recipients
	     (apply #'nconc
		    (mapcar
		     (lambda (recipient)
		       (or (epg-expand-group config recipient)
			   (list recipient)))
		     recipients)))

       (goto-char (point-min))
       (if (search-forward mail-header-separator nil t)
	   (forward-line))
       (setq epa-last-coding-system-specified
	     (or coding-system-for-write
		 (epa--select-safe-coding-system (point) (point-max))))
       (list (point) (point-max)
	     (if verbose
		 (epa-select-keys
		  context
		  "Select recipients for encryption.
If no one is selected, symmetric encryption will be performed.  "
		  recipients)
	       (if recipients
		   (mapcar
		    (lambda (recipient)
		      (setq recipient-key
			    (epa-mail--find-usable-key
			     (epg-list-keys
			      (epg-make-context epa-protocol)
			      (if (string-match "@" recipient)
				  (concat "<" recipient ">")
				recipient))
			     'encrypt))
		      (unless (or recipient-key
				  (y-or-n-p
				   (format
				    "No public key for %s; skip it? "
				    recipient)))
			(error "No public key for %s" recipient))
		      recipient-key)
		    recipients)))
	     (setq sign (if verbose (y-or-n-p "Sign? ")))
	     (if sign
		 (epa-select-keys context
				  "Select keys for signing.  "))))))
  ;; Don't let some read-only text stop us from encrypting.
  (let ((inhibit-read-only t))
    (epa-encrypt-region start end recipients sign signers)))

;;;###autoload
(defun epa-mail-import-keys ()
  "Import keys in the OpenPGP armor format in the current buffer.
The buffer is expected to contain a mail message.

Don't use this command in Lisp programs!"
  (interactive)
  (epa-import-armor-in-region (point-min) (point-max)))

;;;###autoload
(define-minor-mode epa-global-mail-mode
  "Minor mode to hook EasyPG into Mail mode.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  :global t :init-value nil :group 'epa-mail :version "23.1"
  (remove-hook 'mail-mode-hook 'epa-mail-mode)
  (if epa-global-mail-mode
      (add-hook 'mail-mode-hook 'epa-mail-mode)))

(provide 'epa-mail)

;;; epa-mail.el ends here
